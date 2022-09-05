unit srConfig;

{$mode ObjFPC}{$H+}

interface

type
 PsrConfig=^TsrConfig;
 TsrConfig=packed object
  PrintAsm:Boolean;
  UseVertexInput:Boolean; //True
  UseTexelBuffer:Boolean;
  UseOutput16:Boolean;
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
  Procedure Init;
  Function  CanUseStorageBufferClass:Boolean;
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

end.

