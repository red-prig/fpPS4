unit vfs_aio;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

function sys_aio_cancel (fd:Integer;aiocbp:Pointer):Integer;
function sys_aio_error  (aiocbp:Pointer):Integer;
function sys_aio_fsync  (op:Integer;aiocbp:Pointer):Integer;
function sys_aio_read   (aiocbp:Pointer):Integer;
function sys_aio_write  (aiocbp:Pointer):Integer;
function sys_aio_return (aiocbp:Pointer):Integer;
function sys_aio_suspend(aiocbp:Pointer;nent:Integer;timeout:Pointer):Integer;
function sys_aio_waitcomplete(aiocbp:Pointer;timeout:Pointer):Integer;

implementation

uses
 errno;

function sys_aio_cancel(fd:Integer;aiocbp:Pointer):Integer;
begin
 //priv_check(td,PRIV_VFS_SYSTEM)
 Exit(EPERM);
end;

function sys_aio_error(aiocbp:Pointer):Integer;
begin
 //priv_check(td,PRIV_VFS_SYSTEM)
 Exit(EPERM);
end;

function sys_aio_fsync(op:Integer;aiocbp:Pointer):Integer;
begin
 //priv_check(td,PRIV_VFS_SYSTEM)
 Exit(EPERM);
end;

function sys_aio_read(aiocbp:Pointer):Integer;
begin
 //priv_check(td,PRIV_VFS_SYSTEM)
 Exit(EPERM);
end;

function sys_aio_write(aiocbp:Pointer):Integer;
begin
 //priv_check(td,PRIV_VFS_SYSTEM)
 Exit(EPERM);
end;

function sys_aio_return(aiocbp:Pointer):Integer;
begin
 //priv_check(td,PRIV_VFS_SYSTEM)
 Exit(EPERM);
end;

function sys_aio_suspend(aiocbp:Pointer;nent:Integer;timeout:Pointer):Integer;
begin
 //priv_check(td,PRIV_VFS_SYSTEM)
 Exit(EPERM);
end;

function sys_aio_waitcomplete(aiocbp:Pointer;timeout:Pointer):Integer;
begin
 //priv_check(td,PRIV_VFS_SYSTEM)
 Exit(EPERM);
end;


end.






