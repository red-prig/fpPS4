unit kern_sx;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_rwlock;

type
 p_sx=^t_sx;
 t_sx=packed record
  n:PChar;
  c:Pointer;
  m:QWORD;
 end;

procedure sx_init(p:p_sx;name:PChar);
function  sx_xlocked(p:p_sx):Boolean;
procedure sx_assert(p:p_sx);
procedure sx_slock(p:p_sx);
procedure sx_xlock(p:p_sx);
procedure sx_sunlock(p:p_sx);
procedure sx_xunlock(p:p_sx);
procedure sx_unlock(p:p_sx);
procedure sx_destroy(p:p_sx);

implementation

procedure sx_init(p:p_sx;name:PChar);
begin
 p^.n:=name;
 p^.c:=nil;
 p^.m:=0;
end;

function sx_xlocked(p:p_sx):Boolean;
begin
 Result:=(PDWORD(@p^.m)[0]=System.GetCurrentThreadId) and
         (PDWORD(@p^.m)[1]=2);
end;

procedure sx_assert(p:p_sx);
begin
 Assert(sx_xlocked(p),'sx_assert');
end;

procedure sx_slock(p:p_sx);
begin
 //Writeln('    sx_slock:',HexStr(p),':',p^.n);

 rw_rlock(p^.c);
 PDWORD(@p^.m)[0]:=0;
 PDWORD(@p^.m)[1]:=1;
end;

procedure sx_xlock(p:p_sx);
begin
 //Writeln('    sx_xlock:',HexStr(p),':',p^.n);

 rw_wlock(p^.c);
 PDWORD(@p^.m)[0]:=System.GetCurrentThreadId;
 PDWORD(@p^.m)[1]:=2;
end;

procedure sx_sunlock(p:p_sx);
begin
 //Writeln('  sx_sunlock:',HexStr(p),':',p^.n);

 rw_runlock(p^.c);
end;

procedure sx_xunlock(p:p_sx);
begin
 //Writeln('  sx_xunlock:',HexStr(p),':',p^.n);

 Assert(PDWORD(@p^.m)[0]=System.GetCurrentThreadId,'sx_unlock');

 PDWORD(@p^.m)[0]:=0;
 PDWORD(@p^.m)[1]:=0;

 rw_wunlock(p^.c);
end;

procedure sx_unlock(p:p_sx);
begin
 //Writeln(' sx_unlock:',HexStr(p),':',p^.n);

 case PDWORD(@p^.m)[1] of
  1:rw_runlock(p^.c);
  2:
    begin
     Assert(PDWORD(@p^.m)[0]=System.GetCurrentThreadId,'sx_unlock');

     PDWORD(@p^.m)[0]:=0;
     PDWORD(@p^.m)[1]:=0;

     rw_wunlock(p^.c);
    end;
  else
    Assert(false,'sx_unlock');
 end;
end;

procedure sx_destroy(p:p_sx);
begin
 //
end;

end.

