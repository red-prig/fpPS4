unit vm_blockpool;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vm;

type
 p_dmem_block=^t_dmem_block;
 t_dmem_block=bitpacked record
  offset   :0..8388607;  //23
  valid    :0..1;        //1
  prot     :0..63;       //6
  onion    :0..1;        //1
  writeback:0..1;        //1
 end;
 {$IF sizeof(t_dmem_block)<>4}{$STOP t_dmem_block<>4}{$ENDIF}

const
 MT_WRITEBACK:t_dmem_block=(
  offset   :0;
  valid    :0;
  prot     :0;
  onion    :0;
  writeback:1;
 );
 MT_ONION_MT_WRITEBACK:t_dmem_block=(
  offset   :0;
  valid    :0;
  prot     :0;
  onion    :1;
  writeback:1;
 );

const
 M_1GB=(1024*1024*1024);
 M_64K=(64*1024);

function  blockpool_obj_get_info(map  :Pointer;
                                 obj  :Pointer;
                                 addr :QWORD;
                                 qinfo:pSceKernelVirtualQueryInfo;
                                 has_sdk_version_5:Boolean):Integer; external;

procedure blockpool_type_protect(map        :Pointer;
                                 obj        :Pointer;
                                 vm_start   :QWORD;
                                 block_start:DWORD;
                                 block___end:DWORD;
                                 mtype      :DWORD;
                                 prot       :DWORD); external;

procedure blockpool_obj_unmap(map        :Pointer;
                              obj        :Pointer;
                              vm_start   :QWORD;
                              block_start:DWORD;
                              block___end:DWORD); external;

implementation

end.

