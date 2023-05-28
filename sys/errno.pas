unit errno;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

{$I sce_errno.inc}
{$I errno.inc}

function px2sce(e:Integer):Integer; inline;
function sce2px(e:Integer):Integer; inline;

function _get_errno:Integer; inline;
function _set_errno(r:Integer):Integer;
function _set_sce_errno(r:Integer):Integer;

implementation

uses
 thr_error;

function px2sce(e:Integer):Integer; inline;
begin
 if (e=0) then
  Result:=0
 else
  Result:=e-$7ffe0000;
end;

function sce2px(e:Integer):Integer; inline;
begin
 if (e=0) then
  Result:=0
 else
  Result:=e+$7ffe0000;
end;

function _get_errno:Integer; inline;
begin
 Result:=__error^;
end;

function _set_errno(r:Integer):Integer;
begin
 Result:=0;
 __error^:=r;
 if (r<>0) then
 begin
  Result:=-1;
 end;
end;

function _set_sce_errno(r:Integer):Integer;
begin
 __error^:=sce2px(r);
 Result:=r;
end;

end.

