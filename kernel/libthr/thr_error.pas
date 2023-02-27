unit thr_error;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 thr_private,
 thr_init,
 pthread_md;

var
 g_errno:Integer=0;

procedure hmqw8GlN_tI(base:Pointer;size:QWORD); //hmqw8GlN+tI
function  __error:PInteger;
procedure cerror;

implementation

var
 g_exclude_stack_base:Pointer=nil;
 g_exclude_stack_size:QWORD  =0;

procedure hmqw8GlN_tI(base:Pointer;size:QWORD); //hmqw8GlN+tI
begin
 g_exclude_stack_base:=base;
 g_exclude_stack_size:=size;
end;

function __error:PInteger;
var
 sptr:Pointer;
 td:p_pthread;
begin
 sptr:=nil;
 if (g_exclude_stack_base<>nil) then
 begin
  sptr:=System.SPtr;
 end;
 if (_thr_initial<>nil) and
    (
     (sptr<g_exclude_stack_base) or
     (g_exclude_stack_base=nil) or
     ((g_exclude_stack_base+g_exclude_stack_size)<sptr)
    ) then
 begin
  td:=_get_curthread;
  if (td=nil) or (td=_thr_initial) then
  begin
   Result:=@g_errno;
  end else
  begin
   Result:=@td^.error;
  end;
  Exit;
 end;
 Result:=@g_errno;
end;

procedure cerror; assembler; nostackframe;
label
 _err;
asm
 jc _err
 ret
 _err:
 push %rax
 call __error
 pop  %rcx
 mov  %ecx,(%rax)
 mov  $-1,%rax
 mov  $-1,%rdx
end;

end.

