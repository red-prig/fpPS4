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
function md_free_dirent(de:p_ufs_dirent):Integer;

function md_vmkdir(dmp:p_ufs_mount;name:PChar;namelen:Integer;dotdot:p_ufs_dirent;inode:DWORD):p_ufs_dirent;

function md_lookup(ap:p_vop_lookup_args):Integer;
function md_readdir(ap:p_vop_readdir_args):Integer;

const
 md_vnodeops_host:vop_vector=(
  vop_default       :@ufs_vnodeops_root;
  vop_bypass        :nil;

  vop_islocked      :nil;
  vop_lookup        :@md_lookup;
  vop_create        :nil;
  vop_whiteout      :nil;
  vop_mknod         :nil;
  vop_open          :nil;
  vop_close         :nil;
  vop_access        :nil; //parent
  vop_accessx       :nil;
  vop_getattr       :@ufs_getattr;
  vop_setattr       :@ufs_setattr;
  vop_markatime     :nil;
  vop_read          :nil; //parent
  vop_write         :nil;
  vop_ioctl         :nil;
  vop_poll          :nil;
  vop_kqfilter      :nil;
  vop_revoke        :nil;
  vop_fsync         :nil;
  vop_remove        :@ufs_remove;
  vop_link          :nil;
  vop_rename        :nil;
  vop_mkdir         :@ufs_mkdir;
  vop_rmdir         :@ufs_rmdir;
  vop_symlink       :@ufs_symlink;
  vop_readdir       :@md_readdir;
  vop_readlink      :@ufs_readlink;
  vop_inactive      :nil;
  vop_reclaim       :@ufs_reclaim;
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

type
 TOBJ_ATTR=packed record
  OATTR:OBJECT_ATTRIBUTES;
  UPATH:UNICODE_STRING;
 end;

 TNT_DIRENT=packed record
  Info:FILE_ID_FULL_DIR_INFORMATION;
  Name:array[0..260] of WCHAR;
 end;

function VFSTOUFS(mp:p_mount):p_ufs_mount; inline;
begin
 Result:=mp^.mnt_data;
end;

procedure INIT_OBJ(var OBJ:TOBJ_ATTR;fd:THandle;FileName:PWideChar);
begin
 OBJ.OATTR.Length:=SizeOf(OBJECT_ATTRIBUTES);

 OBJ.OATTR.RootDirectory:=fd;
 OBJ.OATTR.ObjectName   :=@OBJ.UPATH;
 OBJ.OATTR.Attributes   :=OBJ_CASE_INSENSITIVE;

 OBJ.UPATH.Length       :=strlen(FileName)*SizeOf(WideChar);
 OBJ.UPATH.MaximumLength:=OBJ.UPATH.Length+1;
 OBJ.UPATH.Buffer       :=FileName;
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
 INIT_OBJ(OBJ,0,PWideChar(w));
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtOpenFile(@F,
               SYNCHRONIZE or FILE_LIST_DIRECTORY or FILE_READ_ATTRIBUTES or FILE_WRITE_ATTRIBUTES,
               @OBJ,
               @BLK,
               FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
               FILE_OPEN_FOR_BACKUP_INTENT or FILE_SYNCHRONOUS_IO_NONALERT or FILE_DIRECTORY_FILE
 );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 mp^.ufs_md_fp:=Pointer(F);
end;

function md_unmount(mp:p_ufs_mount):Integer;
begin
 NtClose(THandle(mp^.ufs_md_fp));
 Result:=0;
end;

function md_free_dirent(de:p_ufs_dirent):Integer;
begin
 NtClose(THandle(de^.ufs_md_fp));
 Result:=0;
end;

function md_update_dirent(de:p_ufs_dirent):Integer;
var
 FBI:FILE_BASIC_INFORMATION;
 //FSI:FILE_STANDARD_INFORMATION;
 BLK:IO_STATUS_BLOCK;
 R:DWORD;
begin
 //ufs_inode (4) = FileInternalInformation (8) ??? hash it?

 FBI:=Default(FILE_BASIC_INFORMATION);
 //FSI:=Default(FILE_STANDARD_INFORMATION);
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtQueryInformationFile(
     THandle(de^.ufs_md_fp),
     @BLK,
     @FBI,
     SizeOf(FBI),
     FileBasicInformation
    );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 de^.ufs_atime:=get_unix_file_time(FBI.LastAccessTime);
 de^.ufs_mtime:=get_unix_file_time(FBI.LastWriteTime);
 de^.ufs_ctime:=get_unix_file_time(FBI.ChangeTime);
 de^.ufs_btime:=get_unix_file_time(FBI.CreationTime);

 {
 BLK:=Default(IO_STATUS_BLOCK);

 R:=NtQueryInformationFile(
     THandle(de^.ufs_md_fp),
     @BLK,
     @FSI,
     SizeOf(FSI),
     FileStandardInformation
    );

 Result:=ntf2px(R);
 if (Result<>0) then Exit;

 de^.ufs_links:=FSI.NumberOfLinks;
 }
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

 Exit(de);
end;

function md_vmkdir(dmp:p_ufs_mount;name:PChar;namelen:Integer;dotdot:p_ufs_dirent;inode:DWORD):p_ufs_dirent;
var
 nd:p_ufs_dirent;
 error:Integer;
begin
 { Create the new directory }
 nd:=md_newdirent(name, namelen);

 TAILQ_INIT(@nd^.ufs_dlist);

 nd^.ufs_dirent^.d_type:=DT_DIR;
 nd^.ufs_mode :=&0555;
 nd^.ufs_links:=2;
 nd^.ufs_dir  :=nd;

 if (inode<>0) then
  nd^.ufs_inode:=inode
 else
  nd^.ufs_inode:=ufs_alloc_cdp_inode;

 if (dotdot=nil) then
 begin
  nd^.ufs_md_fp:=dmp^.ufs_md_fp;
 end else
 begin
  //nd^.ufs_md_fp:=???
  sx_assert(@dmp^.ufs_lock);
  TAILQ_INSERT_TAIL(@dotdot^.ufs_dlist,nd,@nd^.ufs_list);
  Inc(dotdot^.ufs_links);
 end;

 error:=md_update_dirent(nd);
 Assert(error=0);

 Exit(nd);
end;


function md_lookupx(dmp:p_ufs_mount;ap:p_vop_lookup_args):Integer;
var
 cnp:p_componentname;
 dvp:p_vnode;
 vpp:pp_vnode;
 de,dd:p_ufs_dirent;
 dde:p_ufs_dirent;
 error,flags,nameiop,dvplocked:Integer;
 pname:PChar;
begin
 cnp:=ap^.a_cnp;
 vpp:=ap^.a_vpp;
 dvp:=ap^.a_dvp;
 pname:=cnp^.cn_nameptr;
 flags:=cnp^.cn_flags;
 nameiop:=cnp^.cn_nameiop;
 dd:=dvp^.v_data;
 vpp^:=nil;

 ////////

 Exit(ENOENT);
end;

function md_lookup(ap:p_vop_lookup_args):Integer;
var
 dmp:p_ufs_mount;
begin
 dmp:=VFSTOUFS(ap^.a_dvp^.v_mount);
 sx_xlock(@dmp^.ufs_lock);
 Result:=md_lookupx(dmp,ap);
 sx_xunlock(@dmp^.ufs_lock);
end;

function md_readdir(ap:p_vop_readdir_args):Integer;
var
 uio:p_uio;

 //mp:p_mount;
 //dmp:p_ufs_mount;

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

 //mp:=ap^.a_vp^.v_mount;
 //dmp:=VFSTOUFS(mp);

 dd:=ap^.a_vp^.v_data;

 off:=0;

 restart:=True;
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
  if (Result<>0) then Exit;

  dt:=Default(t_dirent);
  dt.d_reclen:=SizeOf(t_dirent)-(t_dirent.MAXNAMLEN+1)+NT_DIRENT.Info.FileNameLength;

  if (dt.d_reclen > uio^.uio_resid) then break;

  if (off >= uio^.uio_offset) then
  begin
   dt.d_fileno:=NT_DIRENT.Info.FileIndex; // FileId ????
   //dt.d_reclen
   dt.d_type  :=NT_FA_TO_DT(NT_DIRENT.Info.FileAttributes,NT_DIRENT.Info.EaSize);
   dt.d_namlen:=NT_DIRENT.Info.FileNameLength-1;
   dt.d_name  :=UTF8Encode(WideString(NT_DIRENT.Name));

   Result:=vfs_read_dirent(ap, @dt, off);
   if (Result<>0) then break;
  end;

  Inc(off,dt.d_reclen);
 until false;

 uio^.uio_offset:=off;

 Exit(0);
end;

end.

