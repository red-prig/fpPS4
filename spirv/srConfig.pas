unit srConfig;

{$mode ObjFPC}{$H+}

interface

type
 TcbGetDmem=function(P:Pointer):Pointer; register;

 PsrConfig=^TsrConfig;
 TsrConfig=packed object
  PrintAsm                :Boolean;
  PrintCfg                :Boolean;
  UseVertexInput          :Boolean; //True
  UseTexelBuffer          :Boolean;
  UseOutput16             :Boolean;
  UseAtomicFloatMinMax    :Boolean;
  UseOnlyUserdataPushConst:Boolean;
  UseExtendedEXECMask     :Boolean;
  //
  DescriptorSet:DWORD; //0
  //
  SpvVersion                     :PtrUint; // $10100
  maxUniformBufferRange          :PtrUint; // $FFFF
  PushConstantsOffset            :PtrUint; // 0
  maxPushConstantsSize           :PtrUint; // 128
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
  procedure UpgradeVersion(NewVersion:PtrUint);
  procedure UpgradeVersion13;
  procedure UpgradeVersion14;
  procedure UpgradeVersion15;
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

procedure TsrConfig.UpgradeVersion(NewVersion:PtrUint);
begin
 if (SpvVersion<NewVersion) then
 begin
  SpvVersion:=NewVersion;
 end;
end;

procedure TsrConfig.UpgradeVersion13;
begin
 UpgradeVersion($10300);
end;

procedure TsrConfig.UpgradeVersion14;
begin
 UpgradeVersion($10400);
end;

procedure TsrConfig.UpgradeVersion15;
begin
 UpgradeVersion($10500);
end;


end.

