unit sys_umtx;

{$mode ObjFPC}{$H+}

interface

uses
 _umtx,
 kern_umtx;

function URWLOCK_READER_COUNT(c:DWORD):DWORD; inline;

function sys_umtx_op(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;

implementation

function URWLOCK_READER_COUNT(c:DWORD):DWORD; inline;
begin
 Result:=(c and URWLOCK_MAX_READERS);
end;

function sys_umtx_op(obj:Pointer;op:Integer;val:QWORD;uaddr1,uaddr2:Pointer):Integer;
var
 ua:_umtx_op_args;
begin
 ua.obj   :=obj;
 ua.op    :=op;
 ua._a    :=0;
 ua.val   :=val;
 ua.uaddr1:=uaddr1;
 ua.uaddr2:=uaddr2;
 Result:=_sys_umtx_op(@ua);
end;

end.

