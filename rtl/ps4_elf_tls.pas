unit ps4_elf_tls;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Hamt;

type
 Pdtv=^Tdtv;
 Tdtv=packed record
  value:Pointer;
  is_static:QWORD;
  gen:QWORD;
 end;
 Ptls_tcb=^Ttls_tcb;
 Ttls_tcb=packed record
  seg_adr:Pointer;
  dtv:Pdtv;
  _dtv:Tdtv;
  base:Pointer;
 end;

function  _init_tls_tcb(Size,_align,is_static,gen:QWORD):Ptls_tcb;
function  _get_tls_tcb(gen:QWORD):Ptls_tcb;
procedure _free_tls_tcb_all;

implementation

threadvar
 tls_local:THAMT;

function _init_tls_tcb(Size,_align,is_static,gen:QWORD):Ptls_tcb;
var
 full_size:QWORD;
 base:Pointer;
 dtv :Pointer;
 tcb :Ptls_tcb;
 PP  :PPointer;
begin
 if (_align=0) then _align:=1;

 full_size:=Size+(_align-1)+SizeOf(Ttls_tcb);
 base:=AllocMem(full_size);
 dtv :=Align(base,_align);

 tcb:=Pointer(dtv+Size);
 tcb^.base:=base;

 tcb^.seg_adr:=tcb;
 tcb^.dtv:=@tcb^._dtv;
 tcb^._dtv.value:=dtv;
 tcb^._dtv.is_static:=is_static;
 tcb^._dtv.gen:=gen;

 if (tls_local=nil) then tls_local:=HAMT_create64;
 PP:=HAMT_insert64(tls_local,gen,tcb);
 Assert(PP<>nil);
 Assert(PP^=tcb);
 Result:=tcb;
end;

function _get_tls_tcb(gen:QWORD):Ptls_tcb;
var
 PP:PPointer;
begin
 Result:=nil;
 PP:=HAMT_search64(tls_local,gen);
 if (PP<>nil) then Result:=PP^;
end;

procedure _free_tls_tcb(data,userdata:Pointer);
Var
 tcb:Ptls_tcb;
 base:Pointer;
begin
 tcb:=data;
 if (tcb=nil) then Exit;
 base:=tcb^.base;
 FreeMem(base);
end;

procedure _free_tls_tcb_all;
begin
 HAMT_destroy64(tls_local,@_free_tls_tcb,nil);
 tls_local:=nil;
end;

end.

