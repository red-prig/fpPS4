unit kern_uuid;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 SysUtils;

type
 p_uuid=^t_uuid;
 t_uuid=TGuid;
 {$IF sizeof(t_uuid)<>16}{$STOP sizeof(t_uuid)<>16}{$ENDIF}

function sys_uuidgen(store:Pointer;count:Integer):Integer;

implementation

uses
 errno,
 systm;

function kern_uuidgen(store:p_uuid;count:ptrint):p_uuid;
var
 n:ptrint;
begin
 For n:=0 to count-1 do
 begin
  CreateGUID(store[n]);
 end;

 Exit(store);
end;

function sys_uuidgen(store:Pointer;count:Integer):Integer;
var
 kstore:p_uuid;
begin
 {
  * Limit the number of UUIDs that can be created at the same time
  * to some arbitrary number. This isn't really necessary, but I
  * like to have some sort of upper-bound that's less than 2G :-)
  * XXX probably needs to be tunable.
  }
 if (count < 1) or (count > 2048) then
  Exit(EINVAL);

 kstore:=AllocMem(count * sizeof(t_uuid));

 kern_uuidgen(store, count);

 Result:=copyout(kstore, store, count * sizeof(t_uuid));

 FreeMem(kstore);
end;

end.

