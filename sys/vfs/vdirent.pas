unit vdirent;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 // File types
 DT_UNKNOWN= 0; //The type is unknown.
 DT_FIFO   = 1; //A named pipe, or FIFO.
 DT_CHR    = 2; //A character device.
 DT_DIR    = 4; //A directory.
 DT_BLK    = 6; //A block device.
 DT_REG    = 8; //A regular file.
 DT_LNK    =10; //A symbolic link.
 DT_SOCK   =12; //A local-domain socket.
 DT_WHT    =14; //

type
 p_dirent=^dirent;
 dirent=packed object
  Const
   MAXNAMLEN=255;
  Var
   d_fileno:DWORD; // file number of entry
   d_reclen:WORD;  // length of this record
   d_type  :BYTE;  // file type, see below
   d_namlen:BYTE;  // length of string in d_name
   d_name  :array[0..MAXNAMLEN] of AnsiChar; //name must be no longer than this
 end;

function IFTODT(mode:DWORD):DWORD; inline;
function DTTOIF(dirtype:DWORD):DWORD; inline;

implementation

function IFTODT(mode:DWORD):DWORD; inline;
begin
 Result:=(mode and &0170000) shr 12;
end;

function DTTOIF(dirtype:DWORD):DWORD; inline;
begin
 Result:=dirtype shl 12;
end;

end.

