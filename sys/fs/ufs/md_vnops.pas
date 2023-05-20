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
 vfs_vnops,
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

const
 md_vnodeops_host:vop_vector=(
  vop_default       :@ufs_vnodeops_root;
  vop_bypass        :nil;

  vop_islocked      :nil;
  vop_lookup        :@md_lookup;
  vop_create        :nil;
  vop_whiteout      :nil;
  vop_mknod         :nil;
  vop_open          :nil; //TODO
  vop_close         :nil; //TODO
  vop_access        :nil; //parent
  vop_accessx       :nil;
  vop_getattr       :@md_getattr;
  vop_setattr       :@ufs_setattr;
  vop_markatime     :nil;
  vop_read          :nil; //parent
  vop_write         :nil;
  vop_ioctl         :nil;
  vop_poll          :nil;
  vop_kqfilter      :nil;
  vop_revoke        :nil;
  vop_fsync         :nil; //TODO
  vop_remove        :nil; //TODO
  vop_link          :nil; //TODO
  vop_rename        :nil; //TODO
  vop_mkdir         :nil; //TODO
  vop_rmdir         :nil; //TODO
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
  vop_advlock       :nil;
  vop_advlockasync  :nil;
  vop_advlockpurge  :nil;
  vop_reallocblks   :nil;
  vop_getpages      :nil;
  vop_putpages      :nil;
  vop_getacl        :nil;
  vop_setacl        :nil;
  vop_aclcheck      :nil;
  vop_closeextattr  :nil;
  vop_getextattr    :nil;
  vop_listextattr   :nil;
  vop_openextattr   :nil;
  vop_deleteextattr :nil;
  vop_setextattr    :nil;
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
 kern_thr,
 vfs_subr,
 subr_uio,
 kern_time;

const
 UFS_SET_READONLY=(not &0222);

type
 TOBJ_ATTR=packed record
  OATTR:OBJECT_ATTRIBUTES;
  UPATH:UNICODE_STRING;
 end;

 TNT_DIRENT=packed record
  Info:FILE_ID_FULL_DIR_INFORMATION;
  Name:array[0..260] of WCHAR;
 end;

 T_NT_SYMLINK=record
  info:REPARSE_DATA_BUFFER;
  Name:array[0..MAX_PATH*2] of WCHAR;
 end;

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
  STATUS_SUCCESS              :Result:=0;
  STATUS_PENDING              :Result:=EWOULDBLOCK;
  STATUS_ACCESS_VIOLATION     :Result:=EFAULT;
  STATUS_INVALID_HANDLE       :Result:=EBADF;
  STATUS_NO_SUCH_FILE         :Result:=ENOENT;
  STATUS_NO_MEMORY            :Result:=ENOMEM;
  STATUS_ACCESS_DENIED        :Result:=EPERM;
  STATUS_DISK_CORRUPT_ERROR   :Result:=EIO;
  STATUS_OBJECT_NAME_NOT_FOUND:Result:=ENOENT;
  STATUS_OBJECT_NAME_COLLISION:Result:=EEXIST;
  STATUS_DISK_FULL            :Result:=ENOSPC;
  STATUS_FILE_IS_A_DIRECTORY  :Result:=EISDIR;
  STATUS_DIRECTORY_NOT_EMPTY  :Result:=ENOTEMPTY;
  STATUS_FILE_CORRUPT_ERROR   :Result:=EIO;
  STATUS_NOT_A_DIRECTORY      :Result:=ENOTDIR;
  STATUS_NAME_TOO_LONG        :Result:=ENAMETOOLONG;
  STATUS_IO_DEVICE_ERROR      :Result:=EIO;
  STATUS_TOO_MANY_LINKS       :Result:=EMLINK;
  else
                               Result:=EINVAL;
 end;
end;

function get_unix_file_time(time:LARGE_INTEGER):timespec;
begin
 Int64(time):=Int64(time)-DELTA_EPOCH_IN_UNIT;
 Result.tv_sec :=(Int64(time) div UNIT_PER_SEC);
 Result.tv_nsec:=(Int64(time) mod UNIT_PER_SEC)*100;
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
               SYNCHRONIZE or
               FILE_LIST_DIRECTORY or
               FILE_READ_ATTRIBUTES or
               FILE_WRITE_ATTRIBUTES,
               @OBJ,
               @BLK,
               FILE_SHARE_READ or
               FILE_SHARE_WRITE or
               FILE_SHARE_DELETE,
               FILE_OPEN_FOR_BACKUP_INTENT or
               FILE_SYNCHRONOUS_IO_NONALERT or
               FILE_DIRECTORY_FILE
 );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 mp^.ufs_md_fp:=Pointer(F);
end;

function md_unmount(mp:p_ufs_mount):Integer;
begin
 if (mp^.ufs_md_fp<>nil) then
 begin
  NtClose(THandle(mp^.ufs_md_fp));
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
begin
 if (de^.ufs_md_fp<>nil) then
 begin
  NtClose(THandle(de^.ufs_md_fp));
 end;
 Result:=0;
end;

function md_open_dirent(de:p_ufs_dirent):Integer;
var
 u:RawByteString;
 w:WideString;
 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;
 F:Thandle;
 R:DWORD;
begin
 SetString(u,@de^.ufs_dirent^.d_name,de^.ufs_dirent^.d_namlen);
 w:=UTF8Decode(u);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,THandle(de^.ufs_dir^.ufs_md_fp),0,PWideChar(w));
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtOpenFile(@F,
               SYNCHRONIZE or
               FILE_LIST_DIRECTORY or
               FILE_READ_ATTRIBUTES or
               FILE_WRITE_ATTRIBUTES,
               @OBJ,
               @BLK,
               FILE_SHARE_READ or
               FILE_SHARE_WRITE or
               FILE_SHARE_DELETE,
               FILE_OPEN_FOR_BACKUP_INTENT or
               FILE_SYNCHRONOUS_IO_NONALERT or
               FILE_DIRECTORY_FILE
 );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 de^.ufs_md_fp:=Pointer(F);
end;

function md_open_dirent_file(de:p_ufs_dirent;fdr:PHandle):Integer;
var
 u:RawByteString;
 w:WideString;
 OBJ:TOBJ_ATTR;
 BLK:IO_STATUS_BLOCK;
 F:Thandle;
 opt:DWORD;
 R:DWORD;
begin
 SetString(u,@de^.ufs_dirent^.d_name,de^.ufs_dirent^.d_namlen);
 w:=UTF8Decode(u);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,THandle(de^.ufs_dir^.ufs_md_fp),0,PWideChar(w));
 BLK:=Default(IO_STATUS_BLOCK);

 opt:=FILE_OPEN_FOR_BACKUP_INTENT or FILE_SYNCHRONOUS_IO_NONALERT;
 case de^.ufs_dirent^.d_type of
  DT_DIR:opt:=opt or FILE_DIRECTORY_FILE;
  DT_LNK:opt:=opt or FILE_OPEN_REPARSE_POINT;
  else;
 end;

 R:=NtOpenFile(@F,
               SYNCHRONIZE or
               FILE_READ_DATA or
               FILE_READ_ATTRIBUTES or
               FILE_WRITE_ATTRIBUTES,
               @OBJ,
               @BLK,
               FILE_SHARE_READ or
               FILE_SHARE_WRITE or
               FILE_SHARE_DELETE,
               opt
 );


 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 fdr^:=F;
end;

function md_find_rel_mount_path(const src:WideString;var dst:RawByteString):Integer;
var
 mp:p_mount;
 W,R:WideString;
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

   if (Length(src)>=Length(W)) then
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
     R:=src;
     System.Delete(R,1,Length(W));
     dst:=UTF8Encode(R);

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
 W:WideString;
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

 if (PQWORD(P)^=$005C003F003F005C) then // "\??\"
 begin
  P:=P+4;
  Dec(NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameLength,8);
  SetString(W,P,NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameLength div 2);
  nt_abs:=True;
 end else
 if (PWORD(P)^=$005C) then // "\"
 begin
  SetString(W,P,NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameLength div 2);
  U:=UTF8Encode(W);
  U:=ExpandFileName(U);
  W:=UTF8Decode(U);
  nt_abs:=True;
 end else
 if (PWORD(P)[1]=$003A) then // "C:"
 begin
  SetString(W,P,NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameLength div 2);
  nt_abs:=True;
 end else
 begin
  SetString(W,P,NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameLength div 2);
  nt_abs:=False;
 end;

 if nt_abs then
 begin
  //find by mount points
  Result:=md_find_rel_mount_path(W,U);
  if (Result<>0) then Exit;
 end else
 begin
  U:=UTF8Encode(W);
 end;

 //save to cache
 de^.ufs_symlink:=AllocMem(Length(U)+1);
 Move(PAnsiChar(U)^, de^.ufs_symlink^, Length(U));
end;

function md_update_dirent(de:p_ufs_dirent;prev:PFILE_BASIC_INFORMATION):Integer;
label
 _retry,
 _exit;
var
 FP,RL:THandle;

 FBI:FILE_BASIC_INFORMATION;
 FSI:FILE_STANDARD_INFORMATION;
 FII:FILE_INTERNAL_INFORMATION;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;
begin
 Result:=0;
 if (de=nil) then Exit;

 if (de^.ufs_md_fp=nil) then
 begin
  Result:=md_open_dirent_file(de,@FP);
  if (Result<>0) then Exit;
  RL:=FP;
 end else
 begin
  FP:=THandle(de^.ufs_md_fp);
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
      FP,
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
   Result:=md_update_symlink(de,FP);

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
      FP,
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
      FP,
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

 error:=md_update_dirent(nd,nil);
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

function md_new_cache(dd:p_ufs_dirent;name:PChar;namelen:Integer;prev:PFILE_BASIC_INFORMATION):p_ufs_dirent;
var
 nd:p_ufs_dirent;
 de:p_dirent;
 R:Integer;
begin
 sx_assert(@dd^.ufs_md_lock);

 Result:=nil;

 nd:=md_newdirent(name, namelen);

 nd^.ufs_mode :=UFS_DEFAULT_MODE;
 nd^.ufs_links:=1; //read link???
 nd^.ufs_dir  :=dd;

 if ((prev^.FileAttributes and FILE_ATTRIBUTE_READONLY)=0) then
 begin
  nd^.ufs_mode:=nd^.ufs_mode and UFS_SET_READONLY;
 end;

 de:=nd^.ufs_dirent;

 de^.d_fileno:=nd^.ufs_inode;
 de^.d_type  :=NT_FA_TO_DT(prev^.FileAttributes,IO_REPARSE_TAG_SYMLINK);

 if (de^.d_type=DT_DIR) then
 begin
  R:=md_open_dirent(nd);
  if (R<>0) then
  begin
   ufs_de_drop(nd);
   Exit(nil);
  end;
 end;

 R:=md_update_dirent(nd,prev);
 if (R<>0) then
 begin
  ufs_de_drop(nd);
  Exit(nil);
 end;

 //link->dir
 if (de^.d_type=DT_DIR) then
 begin
  R:=md_open_dirent(nd);
  if (R<>0) then
  begin
   ufs_de_drop(nd);
   Exit(nil);
  end;
 end;

 TAILQ_INSERT_TAIL(@dd^.ufs_dlist,nd,@nd^.ufs_list);
 Inc(dd^.ufs_links);
 ufs_de_hold(dd);

 Exit(nd);
end;

function md_lookup_dirent(dd:p_ufs_dirent;name:PChar;namelen:Integer):p_ufs_dirent;
var
 u:RawByteString;
 w:WideString;

 FBI:FILE_BASIC_INFORMATION;
 OBJ:TOBJ_ATTR;
 R:DWORD;
begin
 sx_assert(@dd^.ufs_md_lock);

 Result:=nil;
 SetString(u,name,namelen);
 w:=UTF8Decode(u);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,THandle(dd^.ufs_md_fp),0,PWideChar(w));

 R:=NtQueryAttributesFile(@OBJ,@FBI);

 if (R<>0) then Exit;

 Result:=md_new_cache(dd,name,namelen,@FBI);
end;

procedure md_delete_cache(de:p_ufs_dirent);
var
 dd:p_ufs_dirent;
 vp:p_vnode;
begin
 if (de=nil) then Exit;

 Assert((de^.ufs_flags and UFS_DOOMED)=0,'ufs_delete doomed dirent');
 de^.ufs_flags:=de^.ufs_flags or UFS_DOOMED;

 dd:=de^.ufs_dir; //parent
 if (dd<>nil) then
 begin
  ufs_de_hold(dd);
  sx_xlock(@dd^.ufs_md_lock);
 end;

 mtx_lock(ufs_interlock);
 vp:=de^.ufs_vnode;
 if (vp<>nil) then
 begin
  VI_LOCK(vp);
  mtx_unlock(ufs_interlock);
  vholdl(vp);

  if (dd<>nil) then
  begin
   sx_unlock(@dd^.ufs_md_lock);
  end;

  vn_lock(vp, LK_EXCLUSIVE or LK_INTERLOCK or LK_RETRY);

  vgone(vp);

  VOP_UNLOCK(vp, 0);

  vdrop(vp);

  if (dd<>nil) then
  begin
   sx_xlock(@dd^.ufs_md_lock);
  end;
 end else
  mtx_unlock(ufs_interlock);

 if (de^.ufs_symlink<>nil) then
 begin
  FreeMem(de^.ufs_symlink);
  de^.ufs_symlink:=nil;
 end;

 if (dd<>nil) then
 begin
  TAILQ_REMOVE(@dd^.ufs_dlist,de,@de^.ufs_list);
  sx_unlock(@dd^.ufs_md_lock);
  ufs_de_drop(dd); //prev hold
  ufs_de_drop(dd); //list hold
 end;

 ufs_de_drop(de);
end;

procedure md_unlink_cache(de:p_ufs_dirent);
var
 dd:p_ufs_dirent;
 notlocked:Boolean;
begin
 if (de=nil) then Exit;

 dd:=de^.ufs_dir; //parent
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
  mtx_lock(ufs_interlock);
  de:=vp^.v_data;
  if (de<>nil) then
  begin
   de^.ufs_vnode:=nil;
   vp^.v_data:=nil;
  end;
  mtx_unlock(ufs_interlock);

  md_delete_cache(de);
 end;

 Exit(0);
end;

function md_reclaim(ap:p_vop_reclaim_args):Integer;
var
 vp:p_vnode;
 de:p_ufs_dirent;
begin
 vp:=ap^.a_vp;

 mtx_lock(ufs_interlock);
 de:=vp^.v_data;
 if (de<>nil) then
 begin
  de^.ufs_vnode:=nil;
  vp^.v_data:=nil;
 end;
 mtx_unlock(ufs_interlock);

 md_delete_cache(de);

 //vnode_destroy_vobject(vp);

 Exit(0);
end;

function md_lookupx(ap:p_vop_lookup_args):Integer;
label
 _error;
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

 de:=md_find_cache(dd, cnp^.cn_nameptr, cnp^.cn_namelen, 0);

 if (de=nil) then
 begin
  de:=md_lookup_dirent(dd, cnp^.cn_nameptr, cnp^.cn_namelen);
 end;

 if (de=nil) then
 begin
  Case nameiop of
   CREATE:
    begin
     //if not last
     if ((flags and ISLASTCN)=0) then Exit(ENOENT);
    end;

   LOOKUP,
   DELETE,
   RENAME:Exit(ENOENT);
   else;
  end;
  goto _error;
 end;

 if ((de^.ufs_flags and UFS_WHITEOUT)<>0) then
 begin
  _error:
  if ((nameiop=CREATE) or (nameiop=RENAME)) and
     ((flags and (LOCKPARENT or WANTPARENT))<>0) and
     ((flags and ISLASTCN)<>0) then
  begin
   cnp^.cn_flags:=cnp^.cn_flags or SAVENAME;
   Exit(EJUSTRETURN);
  end;
  Exit(ENOENT);
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
  dt.d_reclen:=SizeOf(t_dirent)-(t_dirent.MAXNAMLEN+1)+NT_DIRENT.Info.FileNameLength;

  if (dt.d_reclen > uio^.uio_resid) then break;

  if (off >= uio^.uio_offset) then
  begin
   dt.d_fileno:=get_inode(NT_DIRENT.Info.FileId);
   //dt.d_reclen
   dt.d_type  :=NT_FA_TO_DT(NT_DIRENT.Info.FileAttributes,NT_DIRENT.Info.EaSize);
   dt.d_namlen:=NT_DIRENT.Info.FileNameLength-1;
   dt.d_name  :=UTF8Encode(WideString(NT_DIRENT.Name));

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

 Result:=md_update_dirent(de,nil);
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

 if (de^.ufs_symlink=nil) then //not cached
 begin
  Result:=md_update_dirent(de,nil);
  if (Result<>0) then Exit;
 end;

 sx_xunlock(@de^.ufs_md_lock);

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
 u:RawByteString;

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

 //get Privilege
 Privilege:=SE_CREATE_SYMBOLIC_LINK_PRIVILEGE;
 R:=RtlAcquirePrivilege(@Privilege,1,0,@PrivState);

 if (R<>0) then Exit(EPERM);

 dd:=ap^.a_dvp^.v_data;

 SetString(u,ap^.a_cnp^.cn_nameptr, ap^.a_cnp^.cn_namelen);
 w:=UTF8Decode(u);

 OBJ:=Default(TOBJ_ATTR);
 INIT_OBJ(OBJ,THandle(dd^.ufs_md_fp),0,PWideChar(w));
 BLK:=Default(IO_STATUS_BLOCK);

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
                 nil,0);

 Result:=ntf2px(R);
 if (Result<>0) then goto _err;

 NT_SYMLINK:=Default(T_NT_SYMLINK);

 NT_SYMLINK.info.ReparseTag       :=IO_REPARSE_TAG_SYMLINK;
 NT_SYMLINK.info.ReparseDataLength:=(SizeOf(REPARSE_DATA_BUFFER)-REPARSE_DATA_OFFSET)+(len*4);

 NT_SYMLINK.info.SymbolicLinkReparseBuffer.PrintNameOffset     :=0;
 NT_SYMLINK.info.SymbolicLinkReparseBuffer.PrintNameLength     :=(len*2);
 NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameOffset:=(len*2);
 NT_SYMLINK.info.SymbolicLinkReparseBuffer.SubstituteNameLength:=(len*2);

 if (ap^.a_target[0]<>'/') then //is relative
 begin
  NT_SYMLINK.info.SymbolicLinkReparseBuffer.Flags              :=SYMLINK_FLAG_RELATIVE;
 end;

 SetString(u,ap^.a_target,len);
 w:=UTF8Decode(u);

 Move(PWideChar(w)^,NT_SYMLINK.Name[0]  ,len*2);
 Move(PWideChar(w)^,NT_SYMLINK.Name[len],len*2);

 BLK:=Default(IO_STATUS_BLOCK);

 //need SE_CREATE_SYMBOLIC_LINK_PRIVILEGE
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
 RtlReleasePrivilege(PrivState);

 //

 sx_xlock(@dd^.ufs_md_lock);

 //clear cache
 de:=md_find_cache(dd,ap^.a_cnp^.cn_nameptr,ap^.a_cnp^.cn_namelen,0);
 md_unlink_cache(de);

 //new dirent
 de:=md_new_cache(dd,ap^.a_cnp^.cn_nameptr,ap^.a_cnp^.cn_namelen,@FBI);

 sx_xunlock(@dd^.ufs_md_lock);

 if (de=nil) then //if new fail
 begin
  Exit(EEXIST);
 end;

 dmp:=VFSTOUFS(ap^.a_dvp^.v_mount);
 sx_xlock(@dmp^.ufs_lock);
 Exit(ufs_allocv(de, ap^.a_dvp^.v_mount, LK_EXCLUSIVE, ap^.a_vpp)); //sx_xunlock
end;


end.

