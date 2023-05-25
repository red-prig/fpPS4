unit md_vnops;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 windows,
 ntapi,
 mqueue,
 time,
 vfile,
 vmount,
 vdirent,
 vuio,
 vnode,
 vnamei,
 vnode_if,
 vfs_default,
 ufs,
 ufs_vnops,
 kern_mtx,
 kern_sx;

function md_mount(mp:p_ufs_mount):Integer;
function md_unmount(mp:p_ufs_mount):Integer;
function md_statfs(mp:p_mount;sbp:p_statfs):Integer;

function md_free_dirent(de:p_ufs_dirent):Integer;

function md_vmkdir(dmp:p_ufs_mount;name:PChar;namelen:Integer;dotdot:p_ufs_dirent):p_ufs_dirent;

function md_lookup(ap:p_vop_lookup_args):Integer;
function md_readdir(ap:p_vop_readdir_args):Integer;
function md_inactive(ap:p_vop_inactive_args):Integer;
function md_reclaim(ap:p_vop_reclaim_args):Integer;
function md_getattr(ap:p_vop_getattr_args):Integer;

function md_readlink(ap:p_vop_readlink_args):Integer;
function md_symlink(ap:p_vop_symlink_args):Integer;
function md_link(ap:p_vop_link_args):Integer;

function md_mkdir(ap:p_vop_mkdir_args):Integer;
function md_remove(ap:p_vop_remove_args):Integer;
function md_rmdir(ap:p_vop_rmdir_args):Integer;
function md_rename(ap:p_vop_rename_args):Integer;

function md_create(ap:p_vop_create_args):Integer;
function md_open(ap:p_vop_open_args):Integer;
function md_close(ap:p_vop_close_args):Integer;
function md_fsync(ap:p_vop_fsync_args):Integer;
function md_setattr(ap:p_vop_setattr_args):Integer;

function md_read(ap:p_vop_read_args):Integer;
function md_write(ap:p_vop_write_args):Integer;

function md_advlock(ap:p_vop_advlock_args):Integer;
function md_advlockpurge(ap:p_vop_advlockpurge_args):Integer;

const
 md_vnodeops_host:vop_vector=(
  vop_default       :@ufs_vnodeops_root;
  vop_bypass        :nil;

  vop_islocked      :nil;
  vop_lookup        :@md_lookup;
  vop_create        :@md_create;
  vop_whiteout      :nil;
  vop_mknod         :nil;
  vop_open          :@md_open;
  vop_close         :@md_close;
  vop_access        :nil; //parent
  vop_accessx       :nil;
  vop_getattr       :@md_getattr;
  vop_setattr       :@md_setattr;
  vop_markatime     :nil;
  vop_read          :@md_read;
  vop_write         :@md_write;
  vop_ioctl         :nil;
  vop_poll          :nil;
  vop_kqfilter      :nil;
  vop_revoke        :nil;
  vop_fsync         :@md_fsync;
  vop_remove        :@md_remove;
  vop_link          :@md_link;
  vop_rename        :@md_rename;
  vop_mkdir         :@md_mkdir;
  vop_rmdir         :@md_rmdir;
  vop_symlink       :@md_symlink;
  vop_readdir       :@md_readdir;
  vop_readlink      :@md_readlink;
  vop_inactive      :@md_inactive;
  vop_reclaim       :@md_reclaim;
  vop_lock1         :nil;
  vop_unlock        :nil;
  vop_bmap          :nil;
  vop_strategy      :nil;
  vop_getwritemount :nil;
  vop_print         :nil;
  vop_pathconf      :@vop_stdpathconf;
  vop_advlock       :@md_advlock;
  vop_advlockasync  :nil;
  vop_advlockpurge  :@md_advlockpurge;
  vop_reallocblks   :nil;
  vop_getpages      :nil;
  vop_putpages      :nil;
  vop_vptofh        :nil;
  vop_vptocnp       :nil;
  vop_allocate      :nil;
  vop_unp_bind      :nil;
  vop_unp_connect   :nil;
  vop_unp_detach    :nil;
 );

implementation

uses
 sysutils,
 errno,
 vfcntl,
 vstat,
 vfs_subr,
 subr_uio,
 kern_time;

const
 UFS_SET_READONLY=(not &0222);

 FILE_DIR_ACCESS=SYNCHRONIZE or
                 FILE_LIST_DIRECTORY or
                 FILE_WRITE_DATA or
                 FILE_ADD_FILE or
                 FILE_ADD_SUBDIRECTORY or
                 FILE_TRAVERSE or
                 FILE_READ_ATTRIBUTES or
                 FILE_WRITE_ATTRIBUTES;

 FILE_SHARE_ALL=FILE_SHARE_READ or
                FILE_SHARE_WRITE or
                FILE_SHARE_DELETE;

type
 TOBJ_ATTR=packed record
  OATTR:OBJECT_ATTRIBUTES;
  UPATH:UNICODE_STRING;
 end;

 TNT_DIRENT=packed record
  Info:FILE_ID_FULL_DIR_INFORMATION;
  Name:array[0..260] of WCHAR;
 end;

 T_NT_SYMLINK=packed record
  info:REPARSE_DATA_BUFFER;
  Name:array[0..MAX_PATH*2] of WCHAR;
 end;

 T_NT_LINK=packed record
  info:FILE_LINK_INFORMATION;
  Name:array[0..MAX_PATH] of WCHAR;
 end;

 T_NT_RENAME=T_NT_LINK;

function VFSTOUFS(mp:p_mount):p_ufs_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

function INIT_UNICODE(FileName:PWideChar):UNICODE_STRING;
begin
 Result.Length       :=strlen(FileName)*SizeOf(WideChar);
 Result.MaximumLength:=Result.Length+SizeOf(WideChar);
 Result.Buffer       :=FileName;
end;

procedure INIT_OBJ(var OBJ:TOBJ_ATTR;fd:THandle;attr:ULONG;FileName:PWideChar);
begin
 OBJ.OATTR.Length:=SizeOf(OBJECT_ATTRIBUTES);

 OBJ.OATTR.RootDirectory:=fd;
 OBJ.OATTR.ObjectName   :=@OBJ.UPATH;
 OBJ.OATTR.Attributes   :=attr;

 OBJ.UPATH:=INIT_UNICODE(FileName);
end;

function ntf2px(n:Integer):Integer; inline;
begin
 Case DWORD(n) of
  STATUS_SUCCESS               :Result:=0;
  STATUS_PENDING               :Result:=EWOULDBLOCK;
  STATUS_NO_MORE_FILES         :Result:=0;
  STATUS_ACCESS_VIOLATION      :Result:=EFAULT;
  STATUS_INVALID_HANDLE        :Result:=EBADF;
  STATUS_NO_SUCH_FILE          :Result:=ENOENT;
  STATUS_END_OF_FILE           :Result:=0;
  STATUS_NO_MEMORY             :Result:=ENOMEM;
  STATUS_ACCESS_DENIED         :Result:=EACCES;
  STATUS_DISK_CORRUPT_ERROR    :Result:=EIO;
  STATUS_OBJECT_NAME_NOT_FOUND :Result:=ENOENT;
  STATUS_OBJECT_NAME_COLLISION :Result:=EEXIST;
  STATUS_OBJECT_PATH_NOT_FOUND :Result:=ENOENT;
  STATUS_SHARING_VIOLATION     :Result:=EACCES;
  STATUS_FILE_LOCK_CONFLICT    :Result:=EWOULDBLOCK;
  STATUS_LOCK_NOT_GRANTED      :Result:=EWOULDBLOCK;
  STATUS_RANGE_NOT_LOCKED      :Result:=ENOLCK;
  STATUS_DISK_FULL             :Result:=ENOSPC;
  STATUS_FILE_IS_A_DIRECTORY   :Result:=EISDIR;
  STATUS_NOT_SAME_DEVICE       :Result:=EXDEV;
  STATUS_INSUFFICIENT_RESOURCES:Result:=ENOMEM;
  STATUS_DIRECTORY_NOT_EMPTY   :Result:=ENOTEMPTY;
  STATUS_FILE_CORRUPT_ERROR    :Result:=EIO;
  STATUS_NOT_A_DIRECTORY       :Result:=ENOTDIR;
  STATUS_NAME_TOO_LONG         :Result:=ENAMETOOLONG;
  STATUS_IO_DEVICE_ERROR       :Result:=EIO;
  STATUS_TOO_MANY_LINKS        :Result:=EMLINK;
  STATUS_CANT_CROSS_RM_BOUNDARY:Result:=EXDEV;
  else
                                Result:=EINVAL;
 end;
end;

function get_unix_file_time(time:LARGE_INTEGER):timespec; inline;
begin
 Int64(time):=Int64(time)-DELTA_EPOCH_IN_UNIT;
 Result.tv_sec :=(Int64(time) div UNIT_PER_SEC);
 Result.tv_nsec:=(Int64(time) mod UNIT_PER_SEC)*100;
end;

function get_win_file_time(time:timespec):LARGE_INTEGER; inline;
begin
 Int64(Result):=(time.tv_sec*UNIT_PER_SEC)+(time.tv_nsec div 100);
 Int64(Result):=Int64(Result)+DELTA_EPOCH_IN_UNIT;
end;

function NT_FA_TO_DT(a,e:ULONG):Byte;
begin
 if ((a and FILE_ATTRIBUTE_REPARSE_POINT)<>0) and
    (e=IO_REPARSE_TAG_SYMLINK) then
 begin
  Result:=DT_LNK;
 end else
 if ((a and FILE_ATTRIBUTE_DEVICE)<>0) then
 begin
  Result:=DT_CHR; //DT_FIFO ???
 end else
 if ((a and FILE_ATTRIBUTE_DIRECTORY)<>0) then
 begin
  Result:=DT_DIR;
 end else
 begin
  Result:=DT_REG;
 end;
end;

function get_inode(i:LARGE_INTEGER):Integer; inline;
begin
 Result:=(i.LowPart+i.HighPart); //simple hash
end;

procedure fix_win_path(name:PWideChar;namelen:Integer);
begin
 if (name=nil) then Exit;
 while (namelen>0) do
 begin
  if (name[0]='/') then
  begin
   name[0]:='\';
  end;
  Inc(name);
  Dec(namelen);
 end;
end;

procedure fix_unix_path(name:PAnsiChar;namelen:Integer);
begin
 if (name=nil) then Exit;
 while (namelen>0) do
 begin
  if (name[0]='\') then
  begin
   name[0]:='/';
  end;
  Inc(name);
  Dec(namelen);
 end;
end;

function _UTF8Encode(P:PWideChar;len:SizeInt):RawByteString;
var
 i:SizeInt;
 hs:RawByteString;
begin
 Result:='';
 if (p=nil) or (len<=0) then exit;
 hs:='';
 SetLength(hs,len*3);
 i:=UnicodeToUtf8(PAnsiChar(hs),length(hs)+1,P,len);
 if (i>0) then
 begin
  SetLength(hs,i-1);
  Result:=hs;
 end;
end;

function _UTF8Decode(P:PAnsiChar;len:SizeInt):WideString;
var
 i:SizeInt;
 hs:WideString;
begin
 Result:='';
 if (p=nil) or (len<=0) then exit;
 hs:='';
 SetLength(hs,len);
 i:=Utf8ToUnicode(PWideChar(hs),length(hs)+1,P,len,True);
 if (i>0) then
 begin
  SetLength(hs,i-1);
  Result:=hs;
 end;
end;

function md_mount_is_valid(vp:p_vnode):Integer;
var
 mp:p_mount;
begin
 Result:=EXDEV;
 mp:=vp^.v_mount;
 if (mp=nil) then Exit;
 if (mp^.mnt_vfc<>@ufs_vfsconf) then Exit;
 if ((mp^.mnt_flag and MNT_ROOTFS)<>0) then Exit;
 Result:=0;
end;

function md_mount(mp:p_ufs_mount):Integer;
var
 w:WideString;
 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;
 F:Thandle;
 R:DWORD;
begin
 w:=UTF8Decode(ExpandFileName(mp^.ufs_path));
 w:='\??\'+w;

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,0,OBJ_CASE_INSENSITIVE,PWideChar(w));
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtOpenFile(@F,
               FILE_DIR_ACCESS,
               @OBJ,
               @BLK,
               FILE_SHARE_ALL,
               FILE_OPEN_FOR_BACKUP_INTENT or
               FILE_SYNCHRONOUS_IO_NONALERT or
               FILE_DIRECTORY_FILE
 );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 mp^.ufs_md_fp:=Pointer(F);
end;

function md_unmount(mp:p_ufs_mount):Integer;
var
 fd:THandle;
begin
 fd:=THandle(System.InterlockedExchange(mp^.ufs_md_fp,nil));
 if (fd<>0) then
 begin
  NtClose(fd);
 end;
 Result:=0;
end;

function mul_div_u64(m,d,v:QWORD):QWORD; sysv_abi_default; assembler; nostackframe;
asm
 movq v,%rax
 mulq m
 divq d
end;

function md_statfs(mp:p_mount;sbp:p_statfs):Integer;
var
 dmp:p_ufs_mount;
 FFF:FILE_FS_FULL_SIZE_INFORMATION;
 BLK:IO_STATUS_BLOCK;
 Sector:Int64;
 R:DWORD;
begin
 dmp:=VFSTOUFS(mp);
 if (dmp^.ufs_md_fp=nil) then Exit(0);

 FFF:=Default(FILE_FS_FULL_SIZE_INFORMATION);
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtQueryVolumeInformationFile(
           THandle(dmp^.ufs_md_fp),
           @BLK,
           @FFF,
           SizeOf(FFF),
           FileFsFullSizeInformation
          );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 Sector:=(FFF.SectorsPerAllocationUnit*FFF.BytesPerSector);

 sbp^.f_bavail:=mul_div_u64(Sector,DEV_BSIZE,QWORD(FFF.CallerAvailableAllocationUnits));
 sbp^.f_blocks:=mul_div_u64(Sector,DEV_BSIZE,QWORD(FFF.TotalAllocationUnits          ));
 sbp^.f_bfree :=mul_div_u64(Sector,DEV_BSIZE,QWORD(FFF.ActualAvailableAllocationUnits));
end;

function md_free_dirent(de:p_ufs_dirent):Integer;
var
 fd:THandle;
begin
 if ((de^.ufs_flags and UFS_DROOT)=0) then //if not root dir
 begin
  fd:=THandle(System.InterlockedExchange(de^.ufs_md_fp,nil));
  if (fd<>0) then
  begin
   NtClose(fd);
  end;
 end;
 Result:=0;
end;

function md_open_dirent(de:p_ufs_dirent):Integer;
var
 w:WideString;
 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;
 F:Thandle;
 R:DWORD;
begin
 w:=_UTF8Decode(@de^.ufs_dirent^.d_name,de^.ufs_dirent^.d_namlen);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,THandle(de^.ufs_dir^.ufs_md_fp),0,PWideChar(w));
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtOpenFile(@F,
               FILE_DIR_ACCESS,
               @OBJ,
               @BLK,
               FILE_SHARE_ALL,
               FILE_OPEN_FOR_BACKUP_INTENT or
               FILE_SYNCHRONOUS_IO_NONALERT or
               FILE_DIRECTORY_FILE
 );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 de^.ufs_md_fp:=Pointer(F);
end;

function md_open_dirent_file(de:p_ufs_dirent;symlink:Boolean;fdr:PHandle):Integer;
var
 w:WideString;
 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;
 F:Thandle;
 opt:DWORD;
 R:DWORD;
begin
 w:=_UTF8Decode(@de^.ufs_dirent^.d_name,de^.ufs_dirent^.d_namlen);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,THandle(de^.ufs_dir^.ufs_md_fp),0,PWideChar(w));
 BLK:=Default(IO_STATUS_BLOCK);

 opt:=FILE_OPEN_FOR_BACKUP_INTENT or FILE_SYNCHRONOUS_IO_NONALERT;

 if (de^.ufs_dirent^.d_type=DT_DIR) then
 begin
  opt:=opt or FILE_DIRECTORY_FILE;
 end;

 if symlink then
 begin
  opt:=opt or FILE_OPEN_REPARSE_POINT;
 end;

 R:=NtOpenFile(@F,
               SYNCHRONIZE or
               FILE_CAN_DELETE or
               FILE_READ_DATA or
               //FILE_WRITE_DATA or
               FILE_READ_ATTRIBUTES or
               FILE_WRITE_ATTRIBUTES,
               @OBJ,
               @BLK,
               FILE_SHARE_ALL,
               opt
 );


 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 fdr^:=F;
end;

function md_find_rel_mount_path(var src:PWideChar;var len:SizeInt):Integer;
var
 mp:p_mount;
 W:WideString;
begin
 Result:=ERESTART;
 mtx_lock(mountlist_mtx);

 mp:=TAILQ_FIRST(@mountlist);
 while (mp<>nil) do
 begin

  if (mp^.mnt_vfc=@ufs_vfsconf) then //current fstype
  if ((mp^.mnt_flag and MNT_ROOTFS)=0) then //host mount
  begin
   W:=UTF8Decode(ExpandFileName(mp^.mnt_stat.f_mntfromname));

   if (len>=Length(W)) then
   begin

    //compare case insensetive
    if (CompareStringW(
         LOCALE_SYSTEM_DEFAULT,
         NORM_IGNORECASE,
         PWideChar(W),
         Length(W),
         PWideChar(src),
         Length(W)
        )=2) then
    begin
     Inc(src,Length(W));
     Dec(len,Length(W));

     Result:=0;
     Break;
    end; //cmp
   end; //len
  end;

  mp:=TAILQ_NEXT(mp,@mp^.mnt_list);
 end;

 mtx_unlock(mountlist_mtx);
end;

function md_update_symlink(de:p_ufs_dirent;fd:THandle):Integer;
var
 NT_SYMLINK:T_NT_SYMLINK;
 BLK:IO_STATUS_BLOCK;

 P:PWideChar;
 len:SizeInt;

 U:RawByteString;
 nt_abs:Boolean;

 R:DWORD;
begin
 if (de^.ufs_dirent^.d_type<>DT_LNK) then Exit(EINVAL);

 if (de^.ufs_symlink<>nil) then Exit(0); //cached

 NT_SYMLINK:=Default(T_NT_SYMLINK);
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtFsControlFile(
           fd,
           0,
           nil,
           nil,
           @BLK,
           FSCTL_GET_REPARSE_POINT,
           nil,
           0,
           @NT_SYMLINK,
           SizeOf(NT_SYMLINK)
          );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 //only symlink
 if (NT_SYMLINK.info.ReparseTag<>IO_REPARSE_TAG_SYMLINK) then
 begin
  Exit(ERESTART);
 end;

 P:=@NT_SYMLINK.info.SymbolicLinkReparseBuffer.PathBuffer;
 P:=Pointer(P)+NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameOffset;
 len:=NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameLength div 2;

 if (PQWORD(P)^=$005C003F003F005C) then // "\??\"
 begin
  Inc(P,4);
  Dec(len,4);

  nt_abs:=True;
 end else
 if (PWORD(P)^=$005C) then // "\"
 begin
  //broke struct
  Dec(P,2);
  Inc(len,2);

  P[0]:=GetCurrentDir[1];
  P[1]:=':';

  nt_abs:=True;
 end else
 if (PWORD(P)[1]=$003A) then // "C:"
 begin
  nt_abs:=True;
 end else
 begin
  nt_abs:=False;
 end;

 if nt_abs then
 begin
  //find by mount points
  Result:=md_find_rel_mount_path(P,len);
  if (Result<>0) then Exit;
 end;

 U:=_UTF8Encode(P,len);

 fix_unix_path(PAnsiChar(U),Length(U));

 //save to cache
 de^.ufs_symlink:=AllocMem(Length(U)+1);
 Move(PAnsiChar(U)^, de^.ufs_symlink^, Length(U));
end;

function md_update_dirent(FD:THandle;de:p_ufs_dirent;prev:PFILE_BASIC_INFORMATION):Integer;
label
 _retry,
 _exit;
var
 RL:THandle;

 FBI:FILE_BASIC_INFORMATION;
 FSI:FILE_STANDARD_INFORMATION;
 FII:FILE_INTERNAL_INFORMATION;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;
begin
 Result:=0;
 if (de=nil) then Exit;

 if (FD<>0) then
 begin
  RL:=0;
 end else
 if (de^.ufs_md_fp=nil) then
 begin
  Result:=md_open_dirent_file(de,(de^.ufs_dirent^.d_type=DT_LNK),@FD);
  if (Result<>0) then Exit;
  RL:=FD;
 end else
 begin
  FD:=THandle(de^.ufs_md_fp);
  RL:=0;
 end;

 if (prev<>nil) then
 begin
  //just copy
  FBI:=prev^;
 end else
 begin
  //load time and file type
  FBI:=Default(FILE_BASIC_INFORMATION);
  BLK:=Default(IO_STATUS_BLOCK);

  R:=NtQueryInformationFile(
      FD,
      @BLK,
      @FBI,
      SizeOf(FBI),
      FileBasicInformation
     );

  Result:=ntf2px(R);
  if (Result<>0) then goto _exit;
 end;

 de^.ufs_atime:=get_unix_file_time(FBI.LastAccessTime);
 de^.ufs_mtime:=get_unix_file_time(FBI.LastWriteTime);
 de^.ufs_ctime:=get_unix_file_time(FBI.ChangeTime);
 de^.ufs_btime:=get_unix_file_time(FBI.CreationTime);

 ////

 _retry:

 if (de^.ufs_dirent^.d_type=DT_LNK) then
 begin
  //load symlink info
  if (de^.ufs_symlink=nil) then
  begin
   Result:=md_update_symlink(de,FD);

   if (Result=ERESTART) then
   begin
    Result:=0;
    //update type
    de^.ufs_dirent^.d_type:=NT_FA_TO_DT(FBI.FileAttributes,0);
    goto _retry;
   end;
   if (Result<>0) then goto _exit;
  end;
 end else
 if (de^.ufs_dirent^.d_type<>DT_DIR) then
 begin
  //load size
  FSI:=Default(FILE_STANDARD_INFORMATION);
  BLK:=Default(IO_STATUS_BLOCK);

  R:=NtQueryInformationFile(
      FD,
      @BLK,
      @FSI,
      SizeOf(FSI),
      FileStandardInformation
     );

  Result:=ntf2px(R);
  if (Result<>0) then goto _exit;

  de^.ufs_links:=FSI.NumberOfLinks;
  de^.ufs_size :=Int64(FSI.EndOfFile);
  de^.ufs_bytes:=Int64(FSI.AllocationSize);
 end;

 ////

 if (de^.ufs_inode=0) then
 begin
  //load inode
  FII:=Default(FILE_INTERNAL_INFORMATION);
  BLK:=Default(IO_STATUS_BLOCK);

  R:=NtQueryInformationFile(
      FD,
      @BLK,
      @FII,
      SizeOf(FII),
      FileInternalInformation
     );

  Result:=ntf2px(R);
  if (Result<>0) then goto _exit;

  de^.ufs_inode:=get_inode(FII.IndexNumber);
  de^.ufs_dirent^.d_fileno:=de^.ufs_inode;
 end;

_exit:
 if (RL<>0) then
 begin
  NtClose(RL);
 end;
end;

function md_newdirent(name:PChar;namelen:Integer):p_ufs_dirent;
var
 i:Integer;
 de:p_ufs_dirent;
 d:t_dirent;
begin
 d.d_namlen:=namelen;
 i:=sizeof(t_ufs_dirent) + GENERIC_DIRSIZ(@d);
 de:=AllocMem(i);
 de^.ufs_dirent:=p_dirent(de + 1);
 de^.ufs_dirent^.d_namlen:=namelen;
 de^.ufs_dirent^.d_reclen:=GENERIC_DIRSIZ(@d);

 Move(name^, de^.ufs_dirent^.d_name, namelen);

 de^.ufs_dirent^.d_name[namelen]:=#0;

 de^.ufs_links  :=1;
 de^.ufs_ref    :=1;

 sx_init(@de^.ufs_md_lock, 'md_lock');
 TAILQ_INIT(@de^.ufs_dlist);

 Exit(de);
end;

function md_vmkdir(dmp:p_ufs_mount;name:PChar;namelen:Integer;dotdot:p_ufs_dirent):p_ufs_dirent;
var
 nd:p_ufs_dirent;
 error:Integer;
begin
 { Create the new directory }
 nd:=md_newdirent(name, namelen);

 nd^.ufs_dirent^.d_type:=DT_DIR;
 nd^.ufs_mode :=UFS_DEFAULT_MODE;
 nd^.ufs_links:=2;
 nd^.ufs_dir  :=dotdot;

 if (dotdot=nil) then
 begin
  //move root handle
  nd^.ufs_flags:=UFS_DROOT;
  nd^.ufs_md_fp:=dmp^.ufs_md_fp;
 end else
 begin
  error:=md_open_dirent(nd);
  if (error<>0) then
  begin
   ufs_de_drop(nd);
   Exit(nil);
  end;

  sx_assert(@dotdot^.ufs_md_lock);
  TAILQ_INSERT_TAIL(@dotdot^.ufs_dlist,nd,@nd^.ufs_list);
  Inc(dotdot^.ufs_links);
  ufs_de_hold(dotdot);
 end;

 error:=md_update_dirent(0,nd,nil);
 if (error<>0) then
 begin
  ufs_de_drop(nd);
  Exit(nil);
 end;

 Exit(nd);
end;

function md_find_cache(dd:p_ufs_dirent;name:PChar;namelen:Integer;_type:Integer):p_ufs_dirent;
var
 de:p_ufs_dirent;
begin
 sx_assert(@dd^.ufs_md_lock);

 de:=TAILQ_FIRST(@dd^.ufs_dlist);
 while (de<>nil) do
 begin
  if (namelen<>de^.ufs_dirent^.d_namlen) then
  begin
   de:=TAILQ_NEXT(de,@de^.ufs_list);
   continue;
  end;
  if (_type<>0) and (_type<>de^.ufs_dirent^.d_type) then
  begin
   de:=TAILQ_NEXT(de,@de^.ufs_list);
   continue;
  end;

  if (CompareByte(name^, de^.ufs_dirent^.d_name, namelen)<>0) then
  begin
   de:=TAILQ_NEXT(de,@de^.ufs_list);
   continue;
  end;
  break;
 end;

 Exit(de);
end;

function md_new_cache(dd:p_ufs_dirent;name:PChar;namelen:Integer;prev:PFILE_BASIC_INFORMATION;var nd:p_ufs_dirent):Integer;
var
 de:p_dirent;
begin
 sx_assert(@dd^.ufs_md_lock);

 Result:=0;

 nd:=md_newdirent(name, namelen);

 nd^.ufs_mode :=UFS_DEFAULT_MODE;
 nd^.ufs_dir  :=dd;

 if ((prev^.FileAttributes and FILE_ATTRIBUTE_READONLY)<>0) then
 begin
  nd^.ufs_mode:=nd^.ufs_mode and UFS_SET_READONLY;
 end;

 de:=nd^.ufs_dirent;

 de^.d_fileno:=nd^.ufs_inode;
 de^.d_type  :=NT_FA_TO_DT(prev^.FileAttributes,IO_REPARSE_TAG_SYMLINK);

 if (de^.d_type=DT_DIR) then
 begin
  nd^.ufs_links:=2;
  Result:=md_open_dirent(nd);
  if (Result<>0) then
  begin
   ufs_de_drop(nd);
   nd:=nil;
   Exit;
  end;
 end;

 Result:=md_update_dirent(0,nd,prev);
 if (Result<>0) then
 begin
  ufs_de_drop(nd);
  nd:=nil;
  Exit;
 end;

 //link->dir
 if (nd^.ufs_md_fp=nil) and (de^.d_type=DT_DIR) then
 begin
  nd^.ufs_links:=2;
  Result:=md_open_dirent(nd);
  if (Result<>0) then
  begin
   ufs_de_drop(nd);
   nd:=nil;
   Exit;
  end;
 end;

 TAILQ_INSERT_TAIL(@dd^.ufs_dlist,nd,@nd^.ufs_list);
 Inc(dd^.ufs_links);
 ufs_de_hold(dd);
end;

function md_lookup_dirent(dd:p_ufs_dirent;name:PChar;namelen:Integer;var nd:p_ufs_dirent):Integer;
var
 w:WideString;

 FBI:FILE_BASIC_INFORMATION;
 OBJ:TOBJ_ATTR;
 R:DWORD;
begin
 sx_assert(@dd^.ufs_md_lock);

 Result:=0;
 nd:=nil;

 w:=_UTF8Decode(name,namelen);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,THandle(dd^.ufs_md_fp),0,PWideChar(w));

 R:=NtQueryAttributesFile(@OBJ,@FBI);

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 Result:=md_new_cache(dd,name,namelen,@FBI,nd);
end;

procedure md_unlink_cache(de:p_ufs_dirent);
var
 dd:p_ufs_dirent;
 notlocked:Boolean;
begin
 if (de=nil) then Exit;

 //clear fd
 md_free_dirent(de);

 dd:=System.InterlockedExchange(de^.ufs_dir,nil); //parent

 if ((de^.ufs_flags and UFS_CREATE)<>0) then
 begin
  //unlink soft
  de^.ufs_dir:=nil;
  ufs_de_drop(dd);
  Exit;
 end;

 if (dd<>nil) then
 begin
  ufs_de_hold(dd);
  notlocked:=not sx_xlocked(@dd^.ufs_md_lock);
  if notlocked then
  begin
   sx_xlock(@dd^.ufs_md_lock);
  end;
  TAILQ_REMOVE(@dd^.ufs_dlist,de,@de^.ufs_list);
  if notlocked then
  begin
   sx_unlock(@dd^.ufs_md_lock);
  end;
  ufs_de_drop(dd); //prev hold
  ufs_de_drop(dd); //list hold
 end;
end;

procedure md_delete_cache(de:p_ufs_dirent);
var
 s:Pointer;
begin
 if (de=nil) then Exit;

 Assert(de^.ufs_ref>0);

 Assert((de^.ufs_flags and UFS_DOOMED)=0,'ufs_delete doomed dirent');
 de^.ufs_flags:=de^.ufs_flags or UFS_DOOMED;

 md_unlink_cache(de);

 s:=System.InterlockedExchange(de^.ufs_symlink,nil);
 if (s<>nil) then
 begin
  FreeMem(s);
 end;

 ufs_de_drop(de);
end;

function md_inactive(ap:p_vop_inactive_args):Integer;
var
 vp:p_vnode;
 mp:p_mount;
 de:p_ufs_dirent;
begin
 vp:=ap^.a_vp;

 mp:=vp^.v_mount;
 if ((mp^.mnt_flag and MNT_RDONLY)=0) then
 begin
  de:=ufs_relv(vp);
  if (de<>nil) then
  begin
   md_delete_cache(de);
  end;
 end;

 Exit(0);
end;

function md_reclaim(ap:p_vop_reclaim_args):Integer;
var
 vp:p_vnode;
 de:p_ufs_dirent;
begin
 vp:=ap^.a_vp;

 de:=ufs_relv(vp);
 if (de<>nil) then
 begin
  md_delete_cache(de);
 end;

 //vnode_destroy_vobject(vp);

 Exit(0);
end;

function md_lookupx(ap:p_vop_lookup_args):Integer;
var
 cnp:p_componentname;
 dvp:p_vnode;
 vpp:pp_vnode;
 de,dd:p_ufs_dirent;
 error,flags,nameiop:Integer;
 pname:PChar;
 dmp:p_ufs_mount;
begin
 cnp:=ap^.a_cnp;
 vpp:=ap^.a_vpp;
 dvp:=ap^.a_dvp;
 pname:=cnp^.cn_nameptr;
 flags:=cnp^.cn_flags;
 nameiop:=cnp^.cn_nameiop;
 dd:=dvp^.v_data;
 vpp^:=nil;

 if (dvp^.v_type<>VDIR) then
  Exit(ENOTDIR);

 if (((flags and ISDOTDOT)<>0) and ((dvp^.v_vflag and VV_ROOT)<>0)) then
  Exit(EIO);

 error:=VOP_ACCESS(dvp, VEXEC);
 if (error<>0) then
  Exit(error);

 if (cnp^.cn_namelen=1) and (pname^='.') then
 begin
  if ((flags and ISLASTCN) and nameiop<>LOOKUP) then
   Exit(EINVAL);

  vpp^:=dvp;
  VREF(dvp);

  Exit(0);
 end;

 Result:=0;
 de:=md_find_cache(dd, cnp^.cn_nameptr, cnp^.cn_namelen, 0);

 if (de=nil) then
 begin
  Result:=md_lookup_dirent(dd,cnp^.cn_nameptr,cnp^.cn_namelen,de);
 end;

 if (de=nil) then
 begin
  Case nameiop of
   CREATE,
   RENAME:
    begin
     if (Result=ENOENT) and
        ((flags and (LOCKPARENT or WANTPARENT))<>0) and
        ((flags and ISLASTCN)<>0) then
     begin
      cnp^.cn_flags:=cnp^.cn_flags or SAVENAME;
      Exit(EJUSTRETURN);
     end;
    end;
   else;
  end;
  Exit;
 end;

 if (cnp^.cn_nameiop=DELETE) and ((flags and ISLASTCN)<>0) then
 begin
  error:=VOP_ACCESS(dvp, VWRITE);
  if (error<>0) then
   Exit(error);

  if (vpp^=dvp) then
  begin
   VREF(dvp);
   vpp^:=dvp;
   Exit(0);
  end;
 end;

 dmp:=VFSTOUFS(ap^.a_dvp^.v_mount);
 sx_xlock(@dmp^.ufs_lock);
 error:=ufs_allocv(de, dvp^.v_mount, cnp^.cn_lkflags and LK_TYPE_MASK, vpp);

 Exit(error);
end;

function md_lookup(ap:p_vop_lookup_args):Integer;
var
 dd:p_ufs_dirent;
begin
 dd:=ap^.a_dvp^.v_data;
 sx_xlock(@dd^.ufs_md_lock);

 Result:=md_lookupx(ap);

 sx_xunlock(@dd^.ufs_md_lock);
end;

function md_readdir(ap:p_vop_readdir_args):Integer;
var
 uio:p_uio;
 dd:p_ufs_dirent;
 dt:t_dirent;
 off:Int64;
 i:Integer;

 NT_DIRENT:TNT_DIRENT;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;
 restart:Boolean;
begin
 if (ap^.a_vp^.v_type<>VDIR) then
  Exit(ENOTDIR);

 uio:=ap^.a_uio;
 if (uio^.uio_offset < 0) then
  Exit(EINVAL);

 dd:=ap^.a_vp^.v_data;
 off:=0;
 restart:=True;

 sx_xlock(@dd^.ufs_md_lock);

 repeat
  NT_DIRENT:=Default(TNT_DIRENT);
  BLK:=Default(IO_STATUS_BLOCK);

  R:=NtQueryDirectoryFile(
            THandle(dd^.ufs_md_fp),
            0,
            nil,
            nil,
            @BLK,
            @NT_DIRENT,
            SizeOf(NT_DIRENT),
            FileIdFullDirectoryInformation,
            True,
            nil,
            restart
           );
  restart:=false;

  if (R=STATUS_NO_MORE_FILES) then Break;

  Result:=ntf2px(R);
  if (Result<>0) then Break;

  dt:=Default(t_dirent);

  i:=UnicodeToUtf8(@dt.d_name,
                   t_dirent.MAXNAMLEN+1,
                   @NT_DIRENT.Name,
                   NT_DIRENT.Info.FileNameLength div 2);

  if (i<=0) then
  begin
   //skip error
   Continue;
  end;

  dt.d_reclen:=SizeOf(t_dirent)-(t_dirent.MAXNAMLEN+1)+((i + 3) and (not 3)); //zero include

  if (dt.d_reclen > uio^.uio_resid) then break;

  if (off >= uio^.uio_offset) then
  begin
   dt.d_fileno:=get_inode(NT_DIRENT.Info.FileId);
   dt.d_type  :=NT_FA_TO_DT(NT_DIRENT.Info.FileAttributes,NT_DIRENT.Info.EaSize);
   dt.d_namlen:=i-1; //zero exclude

   Result:=vfs_read_dirent(ap, @dt, off);
   if (Result<>0) then break;
  end;

  Inc(off,dt.d_reclen);
 until false;

 sx_xunlock(@dd^.ufs_md_lock);
 uio^.uio_offset:=off;

 Exit(0);
end;

function md_getattr(ap:p_vop_getattr_args):Integer;
var
 vp:p_vnode;
 de:p_ufs_dirent;

begin
 vp:=ap^.a_vp;
 de:=vp^.v_data;

 sx_xlock(@de^.ufs_md_lock);

 Result:=md_update_dirent(THandle(vp^.v_un),de,nil);

 sx_xunlock(@de^.ufs_md_lock);

 if (Result<>0) then Exit;

 Result:=ufs_getattr(ap);
end;

function md_readlink(ap:p_vop_readlink_args):Integer;
var
 de:p_ufs_dirent;
begin
 de:=ap^.a_vp^.v_data;
 if (de^.ufs_dirent^.d_type<>DT_LNK) then Exit(EINVAL);

 sx_xlock(@de^.ufs_md_lock);

 Result:=0;
 if (de^.ufs_symlink=nil) then //not cached
 begin
  Result:=md_update_dirent(0,de,nil);
 end;

 sx_xunlock(@de^.ufs_md_lock);

 if (Result<>0) then Exit;

 Exit(uiomove(de^.ufs_symlink, strlen(de^.ufs_symlink), ap^.a_uio));
end;

function md_symlink(ap:p_vop_symlink_args):Integer;
const
 REPARSE_DATA_OFFSET=ptrint(@REPARSE_DATA_BUFFER(nil^).GenericReparseBuffer);
label
 _del,
 _err;
var
 len,error:Integer;
 dd:p_ufs_dirent;
 de:p_ufs_dirent;
 dmp:p_ufs_mount;

 Privilege:DWORD;
 PrivState:Pointer;

 w:WideString;

 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;
 NT_SYMLINK:T_NT_SYMLINK;
 FBI:FILE_BASIC_INFORMATION;

 FD:THandle;
 R:DWORD;
 del_on_close:Boolean;
begin
 error:=0;
 //error:=priv_check(curkthread, PRIV_DEVFS_SYMLINK);
 if (error<>0) then Exit(error);

 len:=strlen(ap^.a_target);
 if (len<=0) then Exit(EINVAL);
 if (len>MAX_PATH) then Exit(ENAMETOOLONG);

 //Get Privilege
 Privilege:=SE_CREATE_SYMBOLIC_LINK_PRIVILEGE;
 R:=RtlAcquirePrivilege(@Privilege,1,0,@PrivState);

 if (R<>0) then Exit(EPERM);

 dd:=ap^.a_dvp^.v_data;

 w:=_UTF8Decode(ap^.a_cnp^.cn_nameptr, ap^.a_cnp^.cn_namelen);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,THandle(dd^.ufs_md_fp),0,PWideChar(w));
 BLK:=Default(IO_STATUS_BLOCK);

 sx_xlock(@dd^.ufs_md_lock);

 R:=NtCreateFile(@FD,
                 FILE_READ_ATTRIBUTES or
                 FILE_WRITE_ATTRIBUTES or
                 FILE_CAN_DELETE or
                 SYNCHRONIZE,
                 @OBJ,
                 @BLK,
                 nil,
                 FILE_ATTRIBUTE_NORMAL,
                 0,
                 FILE_CREATE,
                 FILE_SYNCHRONOUS_IO_NONALERT or
                 FILE_OPEN_REPARSE_POINT or
                 FILE_NON_DIRECTORY_FILE {FILE_DIRECTORY_FILE}, //target dir or file ???
                 nil,
                 0);

 Result:=ntf2px(R);
 if (Result<>0) then goto _err;

 w:=_UTF8Decode(ap^.a_target,len);
 len:=Length(w);

 NT_SYMLINK:=Default(T_NT_SYMLINK);

 NT_SYMLINK.info.ReparseTag       :=IO_REPARSE_TAG_SYMLINK;
 NT_SYMLINK.info.ReparseDataLength:=(SizeOf(REPARSE_DATA_BUFFER)-REPARSE_DATA_OFFSET)+(len*4);

 NT_SYMLINK.info.SymbolicLinkReparseBuffer.PrintNameOffset     :=0;
 NT_SYMLINK.info.SymbolicLinkReparseBuffer.PrintNameLength     :=(len*SizeOf(WideChar));
 NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameOffset:=(len*SizeOf(WideChar));
 NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameLength:=(len*SizeOf(WideChar));

 if (ap^.a_target[0]<>'/') then //is relative
 begin
  NT_SYMLINK.info.SymbolicLinkReparseBuffer.Flags              :=SYMLINK_FLAG_RELATIVE;
  //only from relative
  fix_win_path(PWideChar(w),len);
 end;

 Move(PWideChar(w)^,NT_SYMLINK.Name[0]  ,len*SizeOf(WideChar));
 Move(PWideChar(w)^,NT_SYMLINK.Name[len],len*SizeOf(WideChar));

 BLK:=Default(IO_STATUS_BLOCK);

 //Need SE_CREATE_SYMBOLIC_LINK_PRIVILEGE
 R:=NtFsControlFile(FD,
                    0,
                    nil,
                    nil,
                    @BLK,
                    FSCTL_SET_REPARSE_POINT,
                    @NT_SYMLINK,
                    NT_SYMLINK.info.ReparseDataLength+REPARSE_DATA_OFFSET,
                    nil,
                    0);

 Result:=ntf2px(R);
 if (Result<>0) then
 begin
  //mark delete on close handle
  _del:
  del_on_close:=true;
  NtSetInformationFile(FD,@BLK,@del_on_close,1,FileDispositionInformation);
  NtClose(FD);
  _err:
  sx_xunlock(@dd^.ufs_md_lock);
  RtlReleasePrivilege(PrivState);
  Exit;
 end;

 FBI:=Default(FILE_BASIC_INFORMATION);
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtQueryInformationFile(
     FD,
     @BLK,
     @FBI,
     SizeOf(FBI),
     FileBasicInformation
    );

 Result:=ntf2px(R);
 if (Result<>0) then goto _del;

 NtClose(FD);

 //clear cache
 de:=md_find_cache(dd,ap^.a_cnp^.cn_nameptr,ap^.a_cnp^.cn_namelen,0);
 md_unlink_cache(de);

 //new dirent
 Result:=md_new_cache(dd,ap^.a_cnp^.cn_nameptr,ap^.a_cnp^.cn_namelen,@FBI,de);

 sx_xunlock(@dd^.ufs_md_lock);
 RtlReleasePrivilege(PrivState);

 if (de=nil) then Exit; //if new fail

 dmp:=VFSTOUFS(ap^.a_dvp^.v_mount);
 sx_xlock(@dmp^.ufs_lock);
 Exit(ufs_allocv(de, ap^.a_dvp^.v_mount, LK_EXCLUSIVE, ap^.a_vpp)); //sx_xunlock
end;

function md_link(ap:p_vop_link_args):Integer;
var
 cnp:p_componentname;
 dd:p_ufs_dirent;
 de:p_ufs_dirent;

 i:Integer;
 FD:THandle;
 NT_LINK:T_NT_LINK;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;
begin
 Result:=md_mount_is_valid(ap^.a_tdvp);
 if (Result<>0) then Exit;

 cnp:=ap^.a_cnp;
 dd:=ap^.a_tdvp^.v_data;
 de:=ap^.a_vp^.v_data;

 sx_xlock(@dd^.ufs_md_lock);

 Result:=md_open_dirent_file(de,True,@FD);
 if (Result<>0) then
 begin
  sx_xunlock(@dd^.ufs_md_lock);
  Exit;
 end;

 NT_LINK:=Default(T_NT_LINK);
 NT_LINK.info.ReplaceIfExists:=false;
 NT_LINK.info.RootDirectory  :=THandle(dd^.ufs_md_fp);

 i:=Utf8ToUnicode(@NT_LINK.Name,
                  MAX_PATH,
                  cnp^.cn_nameptr,
                  cnp^.cn_namelen,
                  True);

 if (i<=0) then
 begin
  sx_xunlock(@dd^.ufs_md_lock);
  NtClose(FD);
  Exit(ENAMETOOLONG);
 end;

 NT_LINK.info.FileNameLength:=(i-1)*SizeOf(WideChar); //zero exclude

 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtSetInformationFile(FD,
                         @BLK,
                         @NT_LINK,
                         NT_LINK.info.FileNameLength+24,
                         FileLinkInformation);

 Result:=ntf2px(R);

 if (Result=0) then
 begin
  //clear cache
  de:=md_find_cache(dd,cnp^.cn_nameptr,cnp^.cn_namelen,0);
  md_unlink_cache(de);
 end;

 sx_xunlock(@dd^.ufs_md_lock);
 NtClose(FD);
end;

function md_mkdir(ap:p_vop_mkdir_args):Integer;
var
 dvp:p_vnode;
 cnp:p_componentname;
 vap:p_vattr;
 dmp:p_ufs_mount;
 dd:p_ufs_dirent;
 de:p_ufs_dirent;

 w:WideString;
 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;
 FD:Thandle;
 R:DWORD;
begin
 dvp:=ap^.a_dvp;
 cnp:=ap^.a_cnp;
 vap:=ap^.a_vap;
 dmp:=VFSTOUFS(dvp^.v_mount);

 dd:=dvp^.v_data;

 sx_xlock(@dd^.ufs_md_lock);

 de:=md_find_cache(dd, cnp^.cn_nameptr, cnp^.cn_namelen, 0);

 if (de<>nil) then
 begin
  sx_xunlock(@dd^.ufs_md_lock);
  Exit(EEXIST);
 end;

 w:=_UTF8Decode(cnp^.cn_nameptr,cnp^.cn_namelen);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,THandle(dd^.ufs_md_fp),0,PWideChar(w));
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtCreateFile(@FD,
                 SYNCHRONIZE or
                 FILE_LIST_DIRECTORY or
                 FILE_READ_ATTRIBUTES or
                 FILE_WRITE_ATTRIBUTES,
                 @OBJ,
                 @BLK,
                 nil,
                 FILE_ATTRIBUTE_NORMAL,
                 FILE_SHARE_READ or
                 FILE_SHARE_WRITE,
                 FILE_CREATE,
                 FILE_DIRECTORY_FILE or
                 FILE_SYNCHRONOUS_IO_NONALERT or
                 FILE_OPEN_FOR_BACKUP_INTENT or
                 FILE_OPEN_REPARSE_POINT,
                 nil,
                 0);

 Result:=ntf2px(R);
 if (Result<>0) then
 begin
  sx_xunlock(@dd^.ufs_md_lock);
  Exit;
 end;

 NtClose(FD);

 //clear cache
 de:=md_find_cache(dd,ap^.a_cnp^.cn_nameptr,ap^.a_cnp^.cn_namelen,0);
 md_unlink_cache(de);

 de:=md_vmkdir(dmp,cnp^.cn_nameptr,cnp^.cn_namelen,dd);

 if (de=nil) then
 begin
  sx_xunlock(@dd^.ufs_md_lock);
  Exit(ENOMEM);
 end;

 de^.ufs_mode:=vap^.va_mode;

 sx_xunlock(@dd^.ufs_md_lock);

 sx_xlock(@dmp^.ufs_lock);
 Exit(ufs_allocv(de, ap^.a_dvp^.v_mount, LK_EXCLUSIVE, ap^.a_vpp)); //sx_xunlock
end;

function md_remove(ap:p_vop_remove_args):Integer;
var
 dvp,vp:p_vnode;
 dd,de:p_ufs_dirent;

 FD:THandle;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;

 del_on_close:Boolean;
begin
 dvp:=ap^.a_dvp;
 vp:=ap^.a_vp;

 dd:=dvp^.v_data;
 de:=vp^.v_data;

 sx_xlock(@dd^.ufs_md_lock);

 Result:=md_open_dirent_file(de,True,@FD);
 if (Result<>0) then Exit;

 BLK:=Default(IO_STATUS_BLOCK);
 del_on_close:=true;

 R:=NtSetInformationFile(FD,@BLK,@del_on_close,1,FileDispositionInformation);

 Result:=ntf2px(R);
 if (Result<>0) then
 begin
  sx_xunlock(@dd^.ufs_md_lock);
  NtClose(FD);
  Exit;
 end;

 //clear cache
 md_unlink_cache(de);

 NtClose(FD); //<-deleted

 sx_xunlock(@dd^.ufs_md_lock);
end;

function md_rmdir(ap:p_vop_rmdir_args):Integer;
var
 dvp,vp:p_vnode;
 dd,de:p_ufs_dirent;

 FD:THandle;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;

 del_on_close:Boolean;
begin
 dvp:=ap^.a_dvp;
 vp:=ap^.a_vp;

 dd:=dvp^.v_data;
 de:=vp^.v_data;

 if (de^.ufs_dirent^.d_type<>DT_DIR) then Exit(ENOTDIR);

 sx_xlock(@dd^.ufs_md_lock);

 Result:=md_open_dirent_file(de,True,@FD);
 if (Result<>0) then Exit;

 BLK:=Default(IO_STATUS_BLOCK);
 del_on_close:=true;

 R:=NtSetInformationFile(FD,@BLK,@del_on_close,1,FileDispositionInformation);

 Result:=ntf2px(R);
 if (Result<>0) then
 begin
  sx_xunlock(@dd^.ufs_md_lock);
  NtClose(FD);
  Exit;
 end;

 //clear cache
 md_unlink_cache(de);

 NtClose(FD); //<-deleted

 sx_xunlock(@dd^.ufs_md_lock);
end;

function md_rename(ap:p_vop_rename_args):Integer;
label
 _exit;
var
 dd_f,dd_t:p_ufs_dirent;
 de_f,de_t:p_ufs_dirent;
 cnp_t:p_componentname;

 FD:THandle;
 NT_RENAME:T_NT_RENAME;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;
 i:Integer;

begin
 Result:=md_mount_is_valid(ap^.a_tdvp);
 if (Result<>0) then Exit;

 dd_f:=ap^.a_fdvp^.v_data;
 dd_t:=ap^.a_tdvp^.v_data;
 de_f:=ap^.a_fvp^.v_data;
 cnp_t:=ap^.a_tcnp;

 de_t:=nil;
 if (ap^.a_tvp<>nil) then
 begin
  de_t:=ap^.a_tvp^.v_data;
 end;

 sx_xlock(@dd_f^.ufs_md_lock);
 if (dd_f<>dd_t) then
 begin
  sx_xlock(@dd_t^.ufs_md_lock);
 end;

 Result:=md_open_dirent_file(de_f,True,@FD);
 if (Result<>0) then Exit;

 NT_RENAME:=Default(T_NT_RENAME);
 NT_RENAME.info.ReplaceIfExists:=True;
 NT_RENAME.info.RootDirectory  :=THandle(dd_t^.ufs_md_fp);

 i:=Utf8ToUnicode(@NT_RENAME.Name,
                  MAX_PATH,
                  cnp_t^.cn_nameptr,
                  cnp_t^.cn_namelen,
                  True);

 if (i<=0) then
 begin
  Result:=ENAMETOOLONG;
  goto _exit;
 end;

 NT_RENAME.info.FileNameLength:=(i-1)*SizeOf(WideChar); //zero exclude

 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtSetInformationFile(FD,
                         @BLK,
                         @NT_RENAME,
                         NT_RENAME.info.FileNameLength+24,
                         FileRenameInformation);

 Result:=ntf2px(R);

 if (Result<>0) then goto _exit;

 //clear cache
 md_unlink_cache(de_f);
 md_unlink_cache(de_t);

 _exit:
 sx_xunlock(@dd_f^.ufs_md_lock);
 if (dd_f<>dd_t) then
 begin
  sx_xunlock(@dd_t^.ufs_md_lock);
 end;

 NtClose(FD);
end;

Function GetDesiredAccess(flags:Integer):DWORD; inline;
begin
 Result:=SYNCHRONIZE or
         FILE_READ_ATTRIBUTES or
         FILE_WRITE_ATTRIBUTES;

 if ((flags and FREAD)<>0) then
 begin
  Result:=Result or FILE_READ_DATA;
 end;

 if ((flags and FWRITE)<>0) then
 begin
  Result:=Result or FILE_WRITE_DATA or FILE_APPEND_DATA;
 end;
end;

Function GetCreationDisposition(flags:Integer):DWORD; inline;
begin
 Result:=0;
 if ((flags and O_CREAT)<>0) then
 begin
  if ((flags and O_EXCL)<>0) then
  begin
   Result:=FILE_CREATE;
  end else
  if ((flags and O_TRUNC)<>0) then
  begin
   Result:=FILE_OVERWRITE_IF;
  end else
  begin
   Result:=FILE_OPEN_IF;
  end;
 end else
 if ((flags and O_TRUNC)<>0) then
 begin
  Result:=FILE_OVERWRITE;
 end else
 begin
  Result:=FILE_OPEN;
 end;
end;

Function GetFileAttrtibute(flags,mode:Integer):DWORD; inline;
begin
 Result:=FILE_ATTRIBUTE_NORMAL;
 if ((flags and O_CREAT)<>0) and
    ((mode and S_IWUSR)=0) then
 begin
  Result:=Result or FILE_ATTRIBUTE_READONLY;
 end;
end;

Function GetCreateOptions(flags:Integer):DWORD; inline;
begin
 Result:=FILE_SYNCHRONOUS_IO_NONALERT or
         FILE_NON_DIRECTORY_FILE;
 if ((flags and (O_FSYNC or O_DSYNC))<>0) then
 begin
  Result:=Result or FILE_WRITE_THROUGH;
 end;
end;

function md_create(ap:p_vop_create_args):Integer;
var
 dvp:p_vnode;
 cnp:p_componentname;
 vap:p_vattr;
 dmp:p_ufs_mount;

 dd:p_ufs_dirent;
 nd:p_ufs_dirent;
begin
 dvp:=ap^.a_dvp;
 cnp:=ap^.a_cnp;
 vap:=ap^.a_vap;

 dd:=dvp^.v_data;
 if (dd=nil) then Exit(EPERM);

 nd:=md_newdirent(cnp^.cn_nameptr,cnp^.cn_namelen);
 if (nd=nil) then Exit(ENOMEM);

 nd^.ufs_flags:=UFS_CREATE;
 nd^.ufs_mode :=vap^.va_mode;
 nd^.ufs_dir  :=dd;

 nd^.ufs_dirent^.d_type:=DT_REG;

 //link soft
 ufs_de_hold(dd);

 dmp:=VFSTOUFS(dvp^.v_mount);
 sx_xlock(@dmp^.ufs_lock);
 Exit(ufs_allocv(nd, ap^.a_dvp^.v_mount, LK_EXCLUSIVE, ap^.a_vpp)); //sx_xunlock
end;

function md_open(ap:p_vop_open_args):Integer;
var
 vp:p_vnode;
 fp:p_file;
 flags:Integer;

 DA,FA,CD,CO:DWORD;

 dd:p_ufs_dirent;
 de:p_ufs_dirent;
 dc:p_ufs_dirent;

 w:WideString;
 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;

 FD:THandle;
 R:DWORD;
begin
 vp:=ap^.a_vp;
 fp:=ap^.a_fp;
 flags:=ap^.a_mode;

 if (fp=nil) or (vp=nil) then Exit(EPERM);

 vp^.v_un:=nil;

 case vp^.v_type of
  VREG:;
  VLNK:Exit(0);
  VDIR:Exit(0);
  else
   Exit(EPERM);
 end;

 de:=vp^.v_data;
 if (de=nil) then Exit(EPERM);

 dd:=de^.ufs_dir;
 if (dd=nil) then Exit(EPERM);

 sx_xlock(@dd^.ufs_md_lock);

 w:=_UTF8Decode(@de^.ufs_dirent^.d_name,de^.ufs_dirent^.d_namlen);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,THandle(dd^.ufs_md_fp),0,PWideChar(w));
 BLK:=Default(IO_STATUS_BLOCK);

 DA:=GetDesiredAccess(flags);
 FA:=GetFileAttrtibute(flags,de^.ufs_mode);
 CD:=GetCreationDisposition(flags);
 CO:=GetCreateOptions(flags);

 R:=NtCreateFile(@FD,
                 DA,
                 @OBJ,
                 @BLK,
                 nil,
                 FA,
                 FILE_SHARE_ALL,
                 CD,
                 CO,
                 nil,
                 0);

 Result:=ntf2px(R);
 if (Result<>0) then
 begin
  md_unlink_cache(de);
  sx_xunlock(@dd^.ufs_md_lock);
  Exit;
 end;

 Result:=md_update_dirent(FD,de,nil);
 if (Result<>0) then
 begin
  md_unlink_cache(de);
  sx_xunlock(@dd^.ufs_md_lock);
  NtClose(FD);
  Exit;
 end;

 vp^.v_un:=Pointer(FD);

 if ((de^.ufs_flags and UFS_CREATE)<>0) then
 begin
  if ((flags and O_EXCL)<>0) then
  begin
   //clear cache
   dc:=md_find_cache(dd,@de^.ufs_dirent^.d_name,de^.ufs_dirent^.d_namlen,0);
   md_unlink_cache(dc);
  end;
  //
  de^.ufs_flags:=de^.ufs_flags and (not UFS_CREATE);
  //link cache
  TAILQ_INSERT_TAIL(@dd^.ufs_dlist,de,@de^.ufs_list);
  Inc(dd^.ufs_links);
 end;

 sx_xunlock(@dd^.ufs_md_lock);
end;

function md_close(ap:p_vop_close_args):Integer;
var
 vp:p_vnode;
 FD:THandle;
begin
 vp:=ap^.a_vp;

 FD:=THandle(System.InterlockedExchange(vp^.v_un,nil));
 if (FD<>0) then
 begin
  NtClose(FD);
 end;

 Result:=0;
end;

function md_fsync(ap:p_vop_fsync_args):Integer;
var
 vp:p_vnode;
 FD:THandle;
 fullsync:Boolean;

 BLK:IO_STATUS_BLOCK;
begin
 vp:=ap^.a_vp;
 FD:=THandle(vp^.v_un);
 fullsync:=((ap^.a_waitfor and 2)<>0);

 if (FD=0) then Exit(EINVAL);

 BLK:=Default(IO_STATUS_BLOCK);

 //result doesn't matter
 NtFlushBuffersFile(FD,@BLK);

 if fullsync then
 begin
  //atime
  //mtime
 end;

 Result:=0;
end;

function md_setattr(ap:p_vop_setattr_args):Integer;
label
 _err;
var
 de:p_ufs_dirent;
 vap:p_vattr;
 vp:p_vnode;
 error:Integer;
 uid:uid_t;
 gid:gid_t;

 change_time,change_size:Boolean;

 FD,RL:THandle;

 FBI:FILE_BASIC_INFORMATION;
 BLK:IO_STATUS_BLOCK;
 SIZE:Int64;
 R:DWORD;

 procedure _settime(var dst:LARGE_INTEGER;var src:timespec); inline;
 begin
  if (src.tv_sec<>-1) and (src.tv_nsec<>-1) then
  begin
   dst:=get_win_file_time(src);
  end;
 end;

begin
 Result:=0;

 vap:=ap^.a_vap;
 vp:=ap^.a_vp;

 if (vap^.va_type     <>VNON) or
    (vap^.va_nlink    <>VNOVAL) or
    (vap^.va_fsid     <>VNOVAL) or
    (vap^.va_fileid   <>VNOVAL) or
    (vap^.va_blocksize<>VNOVAL) or
    ((vap^.va_flags   <>VNOVAL) and (vap^.va_flags<>0)) or
    (vap^.va_rdev     <>VNOVAL) or
    (vap^.va_bytes    <>VNOVAL) or
    (vap^.va_gen      <>VNOVAL) then
 begin
  Exit(EINVAL);
 end;

 de:=vp^.v_data;

 error:=0;
 change_time:=False;
 change_size:=False;

 if (vap^.va_uid=VNOVAL) then
  uid:=de^.ufs_uid
 else
  uid:=vap^.va_uid;

 if (vap^.va_gid=VNOVAL) then
  gid:=de^.ufs_gid
 else
  gid:=vap^.va_gid;

 if (uid<>de^.ufs_uid) or (gid<>de^.ufs_gid) then
 begin
  //if ((ap^.a_cred^.cr_uid<>de^.de_uid) or uid<>de^.de_uid or
  //    (gid<>de^.de_gid and !groupmember(gid, ap^.a_cred))) then
  //begin
  // error:=priv_check(td, PRIV_VFS_CHOWN);
  // if (error<>) then
  //  Exit(error);
  //end;
  de^.ufs_uid:=uid;
  de^.ufs_gid:=gid;
 end;

 if (vap^.va_mode<>VNOVAL) then
 begin
  //if (ap^.a_cred^.cr_uid<>de^.de_uid) then
  //begin
  // error:=priv_check(td, PRIV_VFS_ADMIN);
  // if (error<>0) then
  //  Exit(error);
  //end;
  de^.ufs_mode:=vap^.va_mode;
 end;

 if (vap^.va_atime.tv_sec<>VNOVAL) or
    (vap^.va_mtime.tv_sec<>VNOVAL) or
    (vap^.va_birthtime.tv_sec<>VNOVAL) then
 begin
  { See the comment in ufs_vnops::ufs_setattr(). }

  error:=VOP_ACCESS(vp, VADMIN);
  if (error<>0) then
  begin
   if ((vap^.va_vaflags and VA_UTIMES_NULL)=0) then Exit(error);
   error:=VOP_ACCESS(vp, VWRITE);
   if (error<>0) then Exit(error);
  end;

  if (vap^.va_atime.tv_sec<>VNOVAL) then
  begin
   de^.ufs_atime:=vap^.va_atime;
  end;
  if (vap^.va_mtime.tv_sec<>VNOVAL) then
  begin
   de^.ufs_mtime:=vap^.va_mtime;
  end;
  if (vap^.va_birthtime.tv_sec<>VNOVAL) then
  begin
   de^.ufs_btime:=vap^.va_birthtime;
  end;

  change_time:=True;
 end;

 if (vap^.va_size<>VNOVAL) then
 begin
  change_size:=True;
 end;

 if change_time or change_size then
 begin

  if (vp^.v_un<>nil) then
  begin
   FD:=THandle(vp^.v_un);
   RL:=0;
  end else
  if (de^.ufs_md_fp<>nil) then
  begin
   FD:=THandle(de^.ufs_md_fp);
   RL:=0;
  end else
  begin
   Result:=md_open_dirent_file(de,True,@FD);
   if (Result<>0) then Exit;
   RL:=FD;
  end;

  if change_time then
  begin
   BLK:=Default(IO_STATUS_BLOCK);

   R:=NtQueryInformationFile(
       FD,
       @BLK,
       @FBI,
       SizeOf(FBI),
       FileBasicInformation);

   Result:=ntf2px(R);
   if (Result<>0) then goto _err;

   //update
   de^.ufs_ctime:=get_unix_file_time(FBI.ChangeTime);

   _settime(FBI.LastAccessTime,de^.ufs_atime);
   _settime(FBI.LastWriteTime ,de^.ufs_mtime);
   _settime(FBI.CreationTime  ,de^.ufs_btime);

   BLK:=Default(IO_STATUS_BLOCK);

   R:=NtSetInformationFile(
       FD,
       @BLK,
       @FBI,
       SizeOf(FBI),
       FileBasicInformation);

   Result:=ntf2px(R);
   if (Result<>0) then goto _err;
  end;

  if change_size then
  begin
   SIZE:=vap^.va_size;

   R:=NtSetInformationFile(
       FD,
       @BLK,
       @SIZE,
       SizeOf(Int64),
       FileEndOfFileInformation);

   if (R<>0) then
   begin
    R:=NtSetInformationFile(
        FD,
        @BLK,
        @SIZE,
        SizeOf(Int64),
        FileAllocationInformation);
   end;

   Result:=ntf2px(R);
   if (Result<>0) then goto _err;

   de^.ufs_size:=SIZE;
  end;

  _err:
  if (RL<>0) then
  begin
   NtClose(RL);
  end;
 end;

end;

type
 t_uio_cb=function(
  FileHandle   :THandle;
  Event        :THandle;
  ApcRoutine   :Pointer;
  ApcContext   :Pointer;
  IoStatusBlock:PIO_STATUS_BLOCK;
  Buffer       :Pointer;
  Length       :ULONG;
  ByteOffset   :PLARGE_INTEGER;
  Key          :PULONG
 ):DWORD; stdcall;

function md_io(vp:p_vnode;uio:p_uio;ioflag:Integer):Integer;
var
 de:p_ufs_dirent;
 F:Thandle;
 iocb:t_uio_cb;

 append:Boolean;
 locked:Boolean;

 iov:p_iovec;
 cnt:Integer;

 OFFSET:Int64;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;
begin
 Result:=0;
 de:=vp^.v_data;
 F:=THandle(vp^.v_un);

 case uio^.uio_rw of
  UIO_READ :iocb:=@NtReadFile;
  UIO_WRITE:iocb:=@NtWriteFile;
  else
            Exit(EINVAL);
 end;

 append:=(uio^.uio_rw=UIO_WRITE) and ((ioflag and IO_APPEND)<>0);

 locked:=((ioflag and IO_UNIT)<>0) and (not append);

 if locked then
 begin
  sx_xlock(@de^.ufs_md_lock);
 end;

 while (uio^.uio_iovcnt<>0) or (uio^.uio_resid<>0) do
 begin
  iov:=uio^.uio_iov;
  cnt:=iov^.iov_len;

  if (cnt=0) then
  begin
   Inc(uio^.uio_iov);
   Dec(uio^.uio_iovcnt);
   continue;
  end;

  if append then
  begin
   OFFSET:=Int64(FILE_WRITE_TO_END_OF_FILE_L);
  end else
  begin
   OFFSET:=uio^.uio_offset;
  end;

  BLK:=Default(IO_STATUS_BLOCK);

  R:=iocb(F,0,nil,nil,@BLK,iov^.iov_base,cnt,@OFFSET,nil);

  if (R=STATUS_PENDING) then
  begin
   R:=NtWaitForSingleObject(F,False,nil);
  end;

  Result:=ntf2px(R);

  if (Int64(BLK.Information)<cnt) then
  begin
   //partial
   cnt:=BLK.Information;
   if (cnt<>0) then
   begin
    Inc(iov^.iov_base  ,cnt);
    Dec(iov^.iov_len   ,cnt);
    Dec(uio^.uio_resid ,cnt);

    if not append then
    begin
     Inc(uio^.uio_offset,cnt);
    end;
   end;
   Break;
  end;

  if (Result<>0) then Break;

  Inc(iov^.iov_base  ,cnt);
  Dec(iov^.iov_len   ,cnt);
  Dec(uio^.uio_resid ,cnt);

  if not append then
  begin
   Inc(uio^.uio_offset,cnt);
  end;

  Inc(uio^.uio_iov);
  Dec(uio^.uio_iovcnt);
 end;

 if locked then
 begin
  sx_xunlock(@de^.ufs_md_lock);
 end;
end;

function md_read(ap:p_vop_read_args):Integer;
begin
 case ap^.a_vp^.v_type of
  VDIR:Exit(VOP_READDIR(ap^.a_vp, ap^.a_uio, nil, nil, nil));
  VREG:Exit(md_io(ap^.a_vp,ap^.a_uio,ap^.a_ioflag));
  else
       Exit(EBADF);
 end;
end;

function md_write(ap:p_vop_write_args):Integer;
begin
 case ap^.a_vp^.v_type of
  VREG:Exit(md_io(ap^.a_vp,ap^.a_uio,ap^.a_ioflag));
  else
       Exit(EBADF);
 end;
end;

function md_advlock(ap:p_vop_advlock_args):Integer;
var
 vp:p_vnode;
 de:p_ufs_dirent;
 fp:p_file;
 fl:p_flock;
 op:Integer;
 wf:Integer;

 offset,size:Int64;

 F:THandle;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;
begin
 vp:=ap^.a_vp;
 if (vp^.v_type<>VREG) then Exit(EBADF);

 de:=vp^.v_data;
 fp:=ap^.a_id;
 fl:=ap^.a_fl;
 op:=ap^.a_op;
 wf:=ap^.a_flags;

 F:=THandle(vp^.v_un);

 case op of
  F_SETLK:;
  else
   Exit(ENOTSUP);
 end;

 case fl^.l_type of
  F_RDLCK:;
  F_UNLCK:;
  F_WRLCK:;
  else
   Exit(ENOTSUP);
 end;

 case fl^.l_whence of
  SEEK_SET:offset:=fl^.l_start;
  SEEK_CUR:offset:=fp^.f_offset+fl^.l_start;
  SEEK_END:
   begin
    sx_xlock(@de^.ufs_md_lock);

    Result:=md_update_dirent(F,de,nil);

    offset:=de^.ufs_size+fl^.l_start;

    sx_xunlock(@de^.ufs_md_lock);

    if (Result<>0) then Exit;
   end;
  else
       Exit(ENOTSUP);
 end;

 size:=fl^.l_len;
 if (size=0) then
 begin
  size:=-1;
 end;

 case fl^.l_type of
  F_RDLCK:
   begin
    R:=NtLockFile(
        F,
        0,
        nil,
        nil,
        @BLK,
        @offset,
        @size,
        0,
        ((wf and F_WAIT)=0),
        False
       );
   end;
  F_WRLCK:
   begin
    R:=NtLockFile(
        F,
        0,
        nil,
        nil,
        @BLK,
        @offset,
        @size,
        0,
        ((wf and F_WAIT)=0),
        True
       );
   end;
  F_UNLCK:
   begin
    R:=NtUnlockFile(
        F,
        @BLK,
        @offset,
        @size,
        0
       );
   end
  else;
 end;

 Result:=ntf2px(R);
end;

function md_advlockpurge(ap:p_vop_advlockpurge_args):Integer;
begin
 //Locks are automatically released on NtClose
 Result:=0;
end;


end.

