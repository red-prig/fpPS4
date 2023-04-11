unit vstat;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 time;

type
 p_stat=^t_stat;
 t_stat=packed object
  type
   dev_t    =DWORD;
   ino_t    =DWORD;
   mode_t   =Word;
   nlink_t  =Word;
   uid_t    =DWORD;
   gid_t    =DWORD;
   off_t    =Int64;
   blkcnt_t =Int64;
   blksize_t=DWORD;
   fflags_t =DWORD;
  var
   st_dev     :dev_t     ;      // inode's device
   st_ino     :ino_t     ;      // inode's number
   st_mode    :mode_t    ;      // inode protection mode         S_IFMT.....
   st_nlink   :nlink_t   ;      // number of hard links
   st_uid     :uid_t     ;      // user ID of the file's owner   S_IRWXU....
   st_gid     :gid_t     ;      // group ID of the file's group  S_IRWXG....
   st_rdev    :dev_t     ;      // device type
   st_atim    :timespec  ;      // time of last access
   st_mtim    :timespec  ;      // time of last data modification
   st_ctim    :timespec  ;      // time of last file status change
   st_size    :off_t     ;      // file size, in bytes
   st_blocks  :blkcnt_t  ;      // blocks allocated for file
   st_blksize :blksize_t ;      // optimal blocksize for I/O
   st_flags   :fflags_t  ;      // user defined flags for file
   st_gen     :DWORD     ;      // file generation number
   st_lspare  :DWORD     ;
   st_birthtim:timespec  ;      // time of file creation
 end;

const
 S_IRWXU  =&0000700; // RWX mask for owner
 S_IRUSR  =&0000400; // R for owner
 S_IWUSR  =&0000200; // W for owner
 S_IXUSR  =&0000100; // X for owner

 S_IRWXG  =&0000070; // RWX mask for group
 S_IRGRP  =&0000040; // R for group
 S_IWGRP  =&0000020; // W for group
 S_IXGRP  =&0000010; // X for group

 S_IRWXO  =&0000007; // RWX mask for other
 S_IROTH  =&0000004; // R for other
 S_IWOTH  =&0000002; // W for other
 S_IXOTH  =&0000001; // X for other

 S_IFMT   =&0170000; // type of file mask
 S_IFIFO  =&0010000; // named pipe (fifo)
 S_IFCHR  =&0020000; // character special
 S_IFDIR  =&0040000; // directory
 S_IFBLK  =&0060000; // block special
 S_IFREG  =&0100000; // regular
 S_IFLNK  =&0120000; // symbolic link
 S_IFSOCK =&0140000; // socket
 S_ISVTX  =&0001000; // save swapped text even after use

 S_BLKSIZE=512;      // block size used in the stat struct

implementation

end.

