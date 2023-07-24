unit kern_patcher;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm;

procedure patcher_process_section(_obj:Pointer;data,vaddr:Pointer;filesz:QWORD);

implementation

uses
 vm_object;

procedure patcher_process_section(_obj:Pointer;data,vaddr:Pointer;filesz:QWORD);
var
 obj:vm_object_t;
begin
 Assert(_obj<>nil,'patcher_process_section');
 obj:=_obj;

end;



end.

