unit thr_init;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 thr_private;

var
 _usrstack   :Pointer;
 _thr_initial:p_pthread;

implementation

end.

