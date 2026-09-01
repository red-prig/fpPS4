unit vm_internal_object;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 uma;

const
 INT_MOBJ_FREE=1;
 INT_UNION_OBJ=2;

type
 pp_vm_int_obj=^p_vm_int_obj;
 p_vm_int_obj=^vm_int_obj;

 t_nt_obj_free_cb =procedure(obj:p_vm_int_obj);
 t_nt_obj_mmmap_cb=procedure(obj:p_vm_int_obj;start,offset,size:QWORD);

 p_vm_int_obj_vtable=^vm_int_obj_vtable;
 vm_int_obj_vtable=object
  free :t_nt_obj_free_cb;
  mmmap:t_nt_obj_mmmap_cb;
  unmap:t_nt_obj_mmmap_cb;
 end;

 vm_int_obj=packed record
  vtable:p_vm_int_obj_vtable;
  hfile :THandle;
  refs  :DWORD;
  flags :Byte;
  maxp  :Byte;
 end;

const
 dummy_vtable:vm_int_obj_vtable=();

function  vm_int_obj_allocate  (vtable:p_vm_int_obj_vtable;hfile:THandle;maxp:Byte):p_vm_int_obj;
procedure vm_int_obj_init      (obj:p_vm_int_obj;vtable:p_vm_int_obj_vtable;hfile:THandle;maxp,flags:Byte);
procedure vm_int_obj_destroy   (obj:p_vm_int_obj);
procedure vm_int_obj_reference (obj:p_vm_int_obj);
procedure vm_int_obj_deallocate(obj:p_vm_int_obj);

implementation

var
 vm_nt_obj_zone:uma_zone_t=nil;

procedure lazy_init;
var
 new,old:uma_zone_t;
begin
 if (vm_nt_obj_zone=nil) then
 begin
  new:=uma_zcreate('vm_int_obj',sizeof(vm_int_obj) , nil, nil, nil, nil, UMA_ALIGN_PTR, 0);
  old:=System.InterlockedCompareExchange(Pointer(vm_nt_obj_zone),Pointer(new),nil);
  if (old<>nil) then
  begin
   uma_zdestroy(new);
  end;
 end;
end;

function vm_int_obj_allocate(vtable:p_vm_int_obj_vtable;hfile:THandle;maxp:Byte):p_vm_int_obj;
begin
 Assert(maxp<>0);

 lazy_init;

 Result:=uma_zalloc(vm_nt_obj_zone, M_WAITOK or M_ZERO);

 if (vtable=nil) then
 begin
  vtable:=@dummy_vtable;
 end;

 Result^.vtable:=vtable;
 Result^.hfile :=hfile;
 Result^.refs  :=1;
 Result^.flags :=INT_MOBJ_FREE or INT_UNION_OBJ;
 Result^.maxp  :=maxp;
end;

procedure vm_int_obj_init(obj:p_vm_int_obj;vtable:p_vm_int_obj_vtable;hfile:THandle;maxp,flags:Byte);
begin
 Assert(obj<>nil);
 Assert(maxp<>0);

 if (vtable=nil) then
 begin
  vtable:=@dummy_vtable;
 end;

 obj^.vtable:=vtable;
 obj^.hfile :=hfile;
 obj^.flags :=flags;
 obj^.maxp  :=maxp;
end;

procedure vm_int_obj_destroy(obj:p_vm_int_obj);
var
 free:t_nt_obj_free_cb;
begin
 free:=obj^.vtable^.free;

 if (free<>nil) then
 begin
  free(obj);
 end;

 if ((obj^.flags and INT_MOBJ_FREE)<>0) then
 begin
  uma_zfree(vm_nt_obj_zone, obj);
 end;
end;

procedure vm_int_obj_reference(obj:p_vm_int_obj);
begin
 if (obj=nil) then Exit;

 System.InterlockedIncrement(obj^.refs);
end;

procedure vm_int_obj_deallocate(obj:p_vm_int_obj);
begin
 if (obj=nil) then Exit;

 if (System.InterlockedDecrement(obj^.refs)=0) then
 begin
  vm_int_obj_destroy(obj);
 end;
end;


end.

