unit srConfig;

{$mode ObjFPC}{$H+}

interface

type
 TcbGetDmem=function(P:Pointer):Pointer; register;

 PsrConfig=^TsrConfig;
 TsrConfig=packed object
  PrintAsm:Boolean;
  PrintCfg:Boolean;
  UseVertexInput:Boolean; //True
  UseTexelBuffer:Boolean;
  UseOutput16:Boolean;
  UseOnlyUserdataPushConst:Boolean;
  //
  DescriptorSet:DWORD; //0
  //
  SpvVersion:PtrUint;                      // $10100
  maxUniformBufferRange:PtrUint;           // $FFFF
  PushConstantsOffset:PtrUint;             // 0
  maxPushConstantsSize:PtrUint;            // 128
  minStorageBufferOffsetAlignment:PtrUint; // $10
  minUniformBufferOffsetAlignment:PtrUint; // $100
  //
  BitcastPointer:packed record
   Workgroup:Boolean;
   Storage  :Boolean;
  end;
  //
  OnGetDmem:TcbGetDmem;
  //
  Procedure Init;
  Function  CanUseStorageBufferClass:Boolean;
  Function  IsSpv14:Boolean;
 end;

implementation

Procedure TsrConfig.Init;
begin
 UseVertexInput:=True;
 //
 SpvVersion:=$10100;
 maxUniformBufferRange:=$FFFF;
 maxPushConstantsSize:=128;
 minStorageBufferOffsetAlignment:=0;
 minUniformBufferOffsetAlignment:=0;
end;

Function TsrConfig.CanUseStorageBufferClass:Boolean;
begin
 Result:=(SpvVersion>=$10300);
end;

Function TsrConfig.IsSpv14:Boolean;
begin
 Result:=(SpvVersion>=$10400);
end;

end.

