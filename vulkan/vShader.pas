unit vShader;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  shaders,
  Vulkan,
  vDevice;

type
 TvShaderStage=(
  vShaderStageLs,
  vShaderStageHs,
  vShaderStageEs,
  vShaderStageGs,
  vShaderStageVs,
  vShaderStagePs,
  vShaderStageCs
 );

 TvSupportDescriptorType=array[0..1] of TVkDescriptorType;

 TvShader=class;

 TvShaderParser=class
  FOwner:TvShader;
  procedure Parse(data:Pointer;size:Ptruint);     virtual;
  procedure OnEntryPoint(Stage:DWORD;P:PChar);    virtual;
  procedure OnSourceExtension(P:PChar);           virtual;
  procedure OnLocalSize(var x,y,z:DWORD);         virtual;
  procedure OnBinding(var Target,id:DWORD);       virtual;
  procedure OnDescriptorSet(var Target,id:DWORD); virtual;
 end;

 CvShaderParser=class of TvShaderParser;

 TvShader=class
  FHandle:TVkShaderModule;
  FStage:TVkShaderStageFlagBits;
  FEntry:RawByteString;
  Destructor  Destroy; override;
  procedure   ClearInfo; virtual;
  function    parser:CvShaderParser; virtual;
  procedure   LoadFromMemory(data:Pointer;size:Ptruint);
  procedure   LoadFromStream(Stream:TStream);
  procedure   LoadFromFile(const FileName:RawByteString);
  procedure   LoadFromResource(const FileName:RawByteString);
 end;

 ///

 TvShaderParserCompute=class(TvShaderParser)
  procedure OnLocalSize(var x,y,z:DWORD); override;
 end;

 TvShaderCompute=class(TvShader)
  FLocalSize:TVkOffset3D;
  function parser:CvShaderParser; override;
 end;

implementation

Destructor TvShader.Destroy;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyShaderModule(Device.FHandle,FHandle,nil);
 end;
end;

procedure TvShader.ClearInfo;
begin
 ord(FStage):=0;
 FEntry:='';
 //SetLength(FBinds,0);
end;

function TvShader.parser:CvShaderParser;
begin
 Result:=TvShaderParser;
end;

procedure TvShader.LoadFromMemory(data:Pointer;size:Ptruint);
var
 parser_instance:TvShaderParser;
 cinfo:TVkShaderModuleCreateInfo;
 r:TVkResult;
begin
 if (FHandle<>VK_NULL_HANDLE) then
 begin
  vkDestroyShaderModule(Device.FHandle,FHandle,nil);
 end;
 FHandle:=VK_NULL_HANDLE;
 ClearInfo;

 parser_instance:=parser.Create;
 parser_instance.FOwner:=Self;
 parser_instance.Parse(data,size);
 parser_instance.Free;

 cinfo:=Default(TVkShaderModuleCreateInfo);
 cinfo.sType   :=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
 cinfo.codeSize:=size;
 cinfo.pCode   :=data;
 r:=vkCreateShaderModule(Device.FHandle,@cinfo,nil,@FHandle);
 if (r<>VK_SUCCESS) then
 begin
  Writeln('vkCreateShaderModule:',r);
  Exit;
 end;
end;

procedure TvShader.LoadFromStream(Stream:TStream);
var
 M:TCustomMemoryStream;
begin
 if Stream.InheritsFrom(TCustomMemoryStream) then
 begin
  M:=TCustomMemoryStream(Stream);
 end else
 begin
  M:=TMemoryStream.Create;
  TMemoryStream(M).LoadFromStream(Stream);
 end;
 LoadFromMemory(M.Memory,M.Size);
 if (M<>Stream) then
 begin
  M.Free;
 end;
end;

procedure TvShader.LoadFromFile(const FileName:RawByteString);
Var
 F:THandle;
 data:Pointer;
 size:Int64;
begin
 F:=FileOpen(FileName,fmOpenRead or fmShareDenyWrite);
 if (F=feInvalidHandle) then Exit;
 size:=FileSeek(F,0,fsFromEnd);
 if (size<0) then
 begin
  FileClose(F);
  Exit;
 end;
 FileSeek(F,0,fsFromBeginning);
 data:=AllocMem(size);
 size:=FileRead(F,data^,size);
 if (size<0) then
 begin
  FreeMem(data);
  FileClose(F);
  Exit;
 end;
 LoadFromMemory(data,size);
 FreeMem(data);
 FileClose(F);
end;

procedure TvShader.LoadFromResource(const FileName:RawByteString);
var
 Stream:TStream;
begin
 Stream:=GetResourceStream(FileName,'SPV');
 LoadFromStream(Stream);
 Stream.Free;
end;

type
 PSPIRVHeader=^TSPIRVHeader;
 TSPIRVHeader=packed record
  MAGIC:DWORD;
  VERSION_MINOR:WORD;
  VERSION_MAJOR:WORD;
  TOOL_VERSION:WORD;
  TOOL_ID:WORD;
  BOUND:DWORD;
  RESERVED:DWORD;
 end;

 PSPIRVInstruction=^TSPIRVInstruction;
 TSPIRVInstruction=packed record
  OP:WORD;
  COUNT:WORD;
 end;

Const
 MagicNumber = 119734787;
 //Operation
 OpSourceExtension = 4;
 OpEntryPoint = 15;
 OpExecutionMode = 16;
 OpTypeVoid = 19;
 OpTypeBool = 20;
 OpTypeInt = 21;
 OpTypeFloat = 22;
 OpTypeVector = 23;
 OpTypeMatrix = 24;
 OpTypeImage = 25;
 OpTypeSampler = 26;
 OpTypeSampledImage = 27;
 OpTypeArray = 28;
 OpTypeRuntimeArray = 29;
 OpTypeStruct = 30;
 OpTypeOpaque = 31;
 OpTypePointer = 32;
 OpTypeFunction = 33;
 OpTypeEvent = 34;
 OpTypeDeviceEvent = 35;
 OpTypeReserveId = 36;
 OpTypeQueue = 37;
 OpTypePipe = 38;
 OpTypeForwardPointer = 39;
 OpDecorate = 71;
 OpVariable = 59;
 //ExecutionMode
 LocalSize = 17;
 //Decoration
 Sample = 17;
 Binding = 33;
 DescriptorSet = 34;
 //StorageClass
 UniformConstant = 0;
 Uniform = 2;
 Image = 11;
 StorageBuffer = 12;
 //ExecutionModel
 Vertex = 0;
 TessellationControl = 1;
 TessellationEvaluation = 2;
 Geometry = 3;
 Fragment = 4;
 GLCompute = 5;
 Kernel = 6;
 TaskNV = 5267;
 MeshNV = 5268;
 RayGenerationKHR = 5313;
 IntersectionKHR = 5314;
 AnyHitKHR = 5315;
 ClosestHitKHR = 5316;
 MissKHR = 5317;
 CallableKHR = 5318;

function GetStageFlag(FStage:DWORD):TVkShaderStageFlagBits;
begin
 case FStage of
  Vertex                :Result:=VK_SHADER_STAGE_VERTEX_BIT;
  TessellationControl   :Result:=VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT;
  TessellationEvaluation:Result:=VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT;
  Geometry              :Result:=VK_SHADER_STAGE_GEOMETRY_BIT;
  Fragment              :Result:=VK_SHADER_STAGE_FRAGMENT_BIT;
  GLCompute             :Result:=VK_SHADER_STAGE_COMPUTE_BIT;
  Kernel                :Result:=VK_SHADER_STAGE_COMPUTE_BIT;
  TaskNV                :Result:=VK_SHADER_STAGE_TASK_BIT_NV;
  MeshNV                :Result:=VK_SHADER_STAGE_MESH_BIT_NV;
  RayGenerationKHR      :Result:=VK_SHADER_STAGE_RAYGEN_BIT_KHR;
  IntersectionKHR       :Result:=VK_SHADER_STAGE_INTERSECTION_BIT_KHR;
  AnyHitKHR             :Result:=VK_SHADER_STAGE_ANY_HIT_BIT_KHR;
  ClosestHitKHR         :Result:=VK_SHADER_STAGE_CLOSEST_HIT_BIT_KHR;
  MissKHR               :Result:=VK_SHADER_STAGE_MISS_BIT_KHR;
  CallableKHR           :Result:=VK_SHADER_STAGE_CALLABLE_BIT_KHR;
  else
   ord(Result):=0;
 end;
end;

procedure TvShaderParser.Parse(data:Pointer;size:Ptruint);
var
 //orig_data:Pointer;
 //orig_size:Ptruint;
 I:TSPIRVInstruction;
 f:Ptruint;
 //r:PvShaderBind;
 d:dword;

 {
 function Fetch(ID:DWORD):PvShaderBind;
 var
  i:Integer;
 begin
  if Length(FBinds)<>0 then
  For i:=0 to High(FBinds) do
   if (ID=FBinds[i].FDVID) then
   begin
    Exit(@FBinds[i]);
   end;
  i:=Length(FBinds);
  SetLength(FBinds,i+1);
  FBinds[i]:=Default(TvShaderBind);
  FBinds[i].FDVID:=ID;
  Result:=@FBinds[i];
 end;

 function find_pointer_type(data:Pointer;size:Ptruint;var id:DWORD):boolean;
 var
  I:TSPIRVInstruction;
  f:Ptruint;
 begin
  Result:=false;
  repeat
   I:=PSPIRVInstruction(data)^;
   if (I.OP=OpTypePointer) and (I.COUNT=4) then
   if (PDWORD(data)[1]=id) then
   begin
    id:=PDWORD(data)[3];
    Exit(true);
   end;
   if (I.COUNT=0) then I.COUNT:=1;
   f:=I.COUNT*SizeOf(DWORD);
   if (size<f) then Break;
   data:=data+f;
   size:=size-f;
  until false;
 end;

 function find_type(data:Pointer;size:Ptruint;var id:DWORD):boolean;
 var
  I:TSPIRVInstruction;
  f:Ptruint;
 begin
  Result:=false;
  repeat
   I:=PSPIRVInstruction(data)^;
   Case I.OP of
    OpTypeVoid          ,
    OpTypeBool          ,
    OpTypeInt           ,
    OpTypeFloat         ,
    OpTypeVector        ,
    OpTypeMatrix        ,
    OpTypeImage         ,
    OpTypeSampler       ,
    OpTypeSampledImage  ,
    OpTypeArray         ,
    OpTypeRuntimeArray  ,
    OpTypeStruct        ,
    OpTypeOpaque        ,
    OpTypePointer       ,
    OpTypeFunction      ,
    OpTypeEvent         ,
    OpTypeDeviceEvent   ,
    OpTypeReserveId     ,
    OpTypeQueue         ,
    OpTypePipe          ,
    OpTypeForwardPointer:
     if (PDWORD(data)[1]=id) then
     begin
      id:=PDWORD(data)[3];
      Exit(true);
     end;
   end;
   if (I.COUNT=0) then I.COUNT:=1;
   f:=I.COUNT*SizeOf(DWORD);
   if (size<f) then Break;
   data:=data+f;
   size:=size-f;
  until false;
 end;
 }



begin
 if (size<=SizeOf(TSPIRVHeader)) then Exit;
 if (PSPIRVHeader(data)^.MAGIC<>MagicNumber) then Exit;
 data:=data+SizeOf(TSPIRVHeader);
 size:=size-SizeOf(TSPIRVHeader);

 //orig_data:=data;
 //orig_size:=size;

 repeat
  I:=PSPIRVInstruction(data)^;
  Case I.OP of
   OpSourceExtension:
    if (I.COUNT>=2) then
    begin
     OnSourceExtension(PChar(@PDWORD(data)[1]));
    end;
   OpEntryPoint:
    if (I.COUNT>=4) then
    begin
     OnEntryPoint(PDWORD(data)[1],PChar(@PDWORD(data)[3]));
    end;
   OpExecutionMode:
    if (I.COUNT>=3) then
    begin
     d:=PDWORD(data)[2];
     case d of
      LocalSize:
       if (I.COUNT>=6) then
       begin
        OnLocalSize(PDWORD(data)[3],PDWORD(data)[4],PDWORD(data)[5]);
       end;
     end;
    end;
   OpDecorate:
    if (I.COUNT>=4) then
    begin
     d:=PDWORD(data)[2];
     case d of
      {Sample:
       begin
        r:=Fetch(PDWORD(data)[1]);
        r^.FSCLS:=Sample shl 16;
       end;}
      Binding:
       begin
        OnBinding(PDWORD(data)[1],PDWORD(data)[3]);
        //r:=Fetch(PDWORD(data)[1]);
        //r^.FBIND:=PDWORD(data)[3];
       end;
      DescriptorSet:
       begin
        OnDescriptorSet(PDWORD(data)[1],PDWORD(data)[3]);
        //r:=Fetch(PDWORD(data)[1]);
        //r^.FDSET:=PDWORD(data)[3];
       end;
     end;
    end;
   {OpVariable:
    if (I.COUNT>=4) then
    begin
     d:=PDWORD(data)[3];
     case d of
      UniformConstant,
      Uniform,
      Image,
      StorageBuffer:
       begin
        r:=Fetch(PDWORD(data)[2]);
        r^.FSCLS:=d;
        d:=PDWORD(data)[1];
        if find_pointer_type(orig_data,orig_size,d) then
        if find_type(orig_data,orig_size,d) then
        begin
         r^.FTYPE:=d;
        end;
       end;
     end;
    end;}
  end;
  if (I.COUNT=0) then I.COUNT:=1;
  f:=I.COUNT*SizeOf(DWORD);
  if (size<f) then Break;
  data:=data+f;
  size:=size-f;
 until false;
end;

procedure TvShaderParser.OnEntryPoint(Stage:DWORD;P:PChar);
begin
 FOwner.FStage:=GetStageFlag(Stage);
 FOwner.FEntry:=P;
end;

procedure TvShaderParser.OnSourceExtension(P:PChar);
begin
 //
end;

procedure TvShaderParser.OnLocalSize(var x,y,z:DWORD);
begin
 //
end;

procedure TvShaderParser.OnBinding(var Target,id:DWORD);
begin
 //
end;

procedure TvShaderParser.OnDescriptorSet(var Target,id:DWORD);
begin
 //
end;

//

procedure TvShaderParserCompute.OnLocalSize(var x,y,z:DWORD);
begin
 with TvShaderCompute(FOwner) do
 begin
  if (FLocalSize.x>0) then x:=FLocalSize.x else FLocalSize.x:=x;
  if (FLocalSize.y>0) then y:=FLocalSize.y else FLocalSize.y:=y;
  if (FLocalSize.z>0) then z:=FLocalSize.z else FLocalSize.z:=z;
 end;
end;

function TvShaderCompute.parser:CvShaderParser;
begin
 Result:=TvShaderParserCompute;
end;

//  =0,
//  VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER=1,
//  VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE=2,
//  VK_DESCRIPTOR_TYPE_STORAGE_IMAGE=3,
//  VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER=4,
//  VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER=5,
//  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER=6,
//  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER=7,
//  VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC=8,
//  VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC=9,
//  VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT=10,
//  VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT=1000138000,
//  VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR=1000150000,
//  VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV=1000165000,
//  VK_DESCRIPTOR_TYPE_MUTABLE_VALVE=1000351000

{
function TvShaderBind.GetSupportTypes:TvSupportDescriptorType;
begin
 Result:=Default(TvSupportDescriptorType);
 case FType of
  Sample shl 16   :begin Result[0]:=VK_DESCRIPTOR_TYPE_SAMPLER; end;;
  UniformConstant :begin Result[0]:=; end;;
  Uniform         :begin Result[0]:=; end;;
  Workgroup       :begin Result[0]:=; end;;
  CrossWorkgroup  :begin Result[0]:=; end;;
  Image           :begin Result[0]:=; end;;
  StorageBuffer   :begin Result[0]:=; end;;
 end;
end;}

end.

