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

 S_ISUID  =&0004000; // set user id on execution
 S_ISGID  =&0002000; // set group id on execution
 S_ISTXT  =&0001000; // sticky bit

 S_IFWHT  =&0160000; // whiteout

 ACCESSPERMS=(S_IRWXU or S_IRWXG or S_IRWXO); // 0777
 ALLPERMS   =(S_ISUID or S_ISGID or S_ISTXT or S_IRWXU or S_IRWXG or S_IRWXO); // 7777
 DEFFILEMODE=(S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH); // 0666

 S_BLKSIZE=512;      // block size used in the stat struct

 NODEV=-1; // non-existent device

{
 * Definitions of flags stored in file flags word.
 *
 * Super-user and owner changeable flags.
}
 UF_SETTABLE =$0000ffff; // mask of owner changeable flags
 UF_NODUMP   =$00000001; // do not dump file
 UF_IMMUTABLE=$00000002; // file may not be changed
 UF_APPEND   =$00000004; // writes to file may only append
 UF_OPAQUE   =$00000008; // directory is opaque wrt. union
 UF_NOUNLINK =$00000010; // file may not be removed or renamed

//Super-user changeable flags.
 SF_SETTABLE =$ffff0000; // mask of superuser changeable flags
 SF_ARCHIVED =$00010000; // file is archived
 SF_IMMUTABLE=$00020000; // file may not be changed
 SF_APPEND   =$00040000; // writes to file may only append
 SF_NOUNLINK =$00100000; // file may not be removed or renamed
 SF_SNAPSHOT =$00200000; // snapshot inode

//Shorthand abbreviations of above.
 OPAQUE   =(UF_OPAQUE);
 APPEND   =(UF_APPEND or SF_APPEND);
 IMMUTABLE=(UF_IMMUTABLE or SF_IMMUTABLE);
 NOUNLINK =(UF_NOUNLINK or SF_NOUNLINK);

 vttoif_tab:array[0..9] of Integer=(
  0, S_IFREG, S_IFDIR, S_IFBLK, S_IFCHR, S_IFLNK,
  S_IFSOCK, S_IFIFO, S_IFMT, S_IFMT
 );

implementation

end.

