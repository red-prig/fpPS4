unit vShaderExt;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ps4_shader,
  vm_mmap,
  vRegs2Vulkan,
  Vulkan,
  vDevice,
  vPipeline,
  vShader,
  vImage,
  vSetLayoutManager,
  vPipelineLayoutManager,
  si_ci_vi_merged_enum,
  si_ci_vi_merged_registers,
  si_ci_vi_merged_groups;


type
 TvResourceType=(
  vtRoot,
  vtImmData,
  vtBufPtr2,
  vtFunPtr2,
  vtVSharp2,
  vtVSharp4,
  vtSSharp4,
  vtTSharp4,
  vtTSharp8,
  vtLDS,
  vtGDS
 );

 TvDescriptorType=(
  dtDATA   ,
  dtVTX_ATR, //VERTEX ATTRIBUTE
  dtSAMPLER, //VK_DESCRIPTOR_TYPE_SAMPLER
  dtSAM_IMG, //VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
  dtSTR_IMG, //VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
  dtRNT_IMG, //
  dtUTX_BUF, //VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  dtSTX_BUF, //VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  dtRTX_BUF, //
  dtUNF_BUF, //VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
  dtSTR_BUF, //VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
  dtPSH_CST  //PUSH CONSTANT
 );

 TrDstSel=bitpacked record
  x,y,z,w:0..15; //(0..7)
 end;

 TvResInfo=bitpacked record
  roffset:Boolean;  // 0.. 1 :1
  rstride:Boolean;  // 1.. 2 :1
  rdsel  :Boolean;  // 2.. 3 :1
  rfmt   :Boolean;  // 3.. 4 :1
  invalid:Boolean;  // 4.. 5 :1
  degam  :Boolean;  // 5.. 6 :1
  _align1:0..3;     // 6.. 8 :2

  dfmt   :0..63;    // 8..14 :6
  _align2:0..3;     //14..16 :2

  nfmt   :0..15;    //16..20 :4
  rtype  :0..15;    //20..24 :4

  _align3:Byte;     //24..32 :8

  dstsel :TrDstSel; //32..48 :16

  stride :Word;     //48..64 :16
 end;

 {$IF sizeof(TvResInfo)<>8}{$STOP sizeof(TvResInfo)<>8}{$ENDIF}

 TvDataLayout=packed record
  rtype :TvResourceType;
  offset:DWORD;
  rinfo :TvResInfo;
 end;

 ADataLayout=array of TvDataLayout;

 TvFuncCb=procedure(addr:ADataLayout) of object;

 TvLayoutFlags=Set of (vMemoryRead,vMemoryWrite,vMipArray,vForceDegamma);

 PvCustomLayout=^TvCustomLayout;
 TvCustomLayout=packed object
  dtype :TvDescriptorType;
  bind  :DWORD;
  size  :DWORD;
  offset:DWORD;
  mask  :DWORD; //vtVSharp2
  flags :TvLayoutFlags;
  addr  :ADataLayout;
  function GetVulkanDescType:TVkDescriptorType;
 end;

 ACustomLayout=array of TvCustomLayout;

 TvCustomLayoutCb=procedure(const L:TvCustomLayout;Fset:TVkUInt32;pUserData,pImmData:PDWORD) of object;

 TvShaderParserExt=class(TvShaderParser)
  type
   t_context_state=(cNone,cData,cDesc,cCache);

   PvDataLayout2=^TvDataLayout2;
   TvDataLayout2=packed record
    state :t_context_state;
    //
    rtype :TvResourceType;
    dtype :TvDescriptorType;
    //
    bind  :DWORD;           //Desc Layout
    size  :DWORD;           //Desc/Data Layout
    offset:DWORD;           //Desc/Data Layout
    mask  :DWORD;           //vtVSharp2
    flags :TvLayoutFlags;   //Desc Layout
    rinfo :TvResInfo;       //Data Layout
    imm   :array of DWORD;  //Data Layout
    immofs:Boolean;         //Data Layout
   end;

   ADataLayout2=array of TvDataLayout2;

  var
   FDataStack:ADataLayout2;

   FINPUT_CNTL_ID :Integer;
   FEXPORT_INFO_ID:Integer;

  procedure Parse(data:Pointer;size:Ptruint);     override;
  procedure OnDescriptorSet(var Target,id:DWORD); override;
  procedure OnSourceExtension(P:PChar);           override;
  procedure CloseData(c_deep:Integer;c_header:Boolean);
  procedure PushData (const N:RawByteString);
  procedure PushDesc (const N:RawByteString);
  procedure PushCache(const N:RawByteString);
  procedure OnValue  (const N,V:RawByteString);
  function  GetLayoutAddr:ADataLayout;
  procedure PopData  (L:PvDataLayout2);
  procedure PopDesc  (L:PvDataLayout2);
 end;

 A_INPUT_CNTL=array[0..31] of TSPI_PS_INPUT_CNTL_0;

 PRENDER_TARGET=^TRENDER_TARGET;

 TEXPORT_COLOR=packed record
  FORMAT     :Byte;
  NUMBER_TYPE:Byte;
  COMP_SWAP  :Byte;
 end;
 AEXPORT_COLOR=array[0..7] of TEXPORT_COLOR;

 TImmData=array of DWORD;

 TvShaderExt=class(TvShader)

  FDescSetId:Integer;

  FHash_gcn:QWORD;
  FHash_spv:QWORD;

  FSetLayout:TvSetLayout;

  FDataLayouts:ACustomLayout;

  FVertLayouts:ACustomLayout;
  FUnifLayouts:ACustomLayout;
  FFuncLayouts:ACustomLayout;

  FPushConst:TvCustomLayout;

  FImmData:TImmData;

  FParams:record
   VGPR_COMP_CNT:Byte;
   //
   NUM_INTERP   :Byte;
   EXPORT_COUNT :Byte;

   STEP_RATE_0:DWORD;
   STEP_RATE_1:DWORD;
   //
   SHADER_CONTROL:TDB_SHADER_CONTROL;
   INPUT_CNTL    :A_INPUT_CNTL;
   EXPORT_COLOR  :AEXPORT_COLOR;
   //
   NUM_THREAD_X:TCOMPUTE_NUM_THREAD_X;
   NUM_THREAD_Y:TCOMPUTE_NUM_THREAD_Y;
   NUM_THREAD_Z:TCOMPUTE_NUM_THREAD_Z;
  end;

  FGeomRectList:TvShaderExt;

  procedure  ClearInfo; override;
  Destructor Destroy;   override;
  function   parser:CvShaderParser; override;
  procedure  InitSetLayout;
  procedure  AddToPipeline(p:TvPipelineLayout);
  Procedure  EnumFuncLayout(cb:TvFuncCb);
  Procedure  AddVertLayout2(addr:ADataLayout;bind:DWORD);
  Procedure  EnumVertLayout(cb:TvCustomLayoutCb;Fset:TVkUInt32;pUserData,pImmData:PDWORD);
  Procedure  AddDataLayout2(addr:ADataLayout;offset:DWORD);
  Procedure  AddBuffLayout2(dtype:TvDescriptorType;
                            addr:ADataLayout;
                            bind,size,offset,mask:DWORD;
                            flags:TvLayoutFlags);
  Procedure  SetPushConst2(addr:ADataLayout;size:DWORD);
  Function   GetPushConstData(pUserData:Pointer):Pointer;
  Procedure  AddUnifLayout2(dtype:TvDescriptorType;
                            addr:ADataLayout;
                            bind:DWORD;
                            flags:TvLayoutFlags);
  Procedure  EnumUnifLayout(cb:TvCustomLayoutCb;Fset:TVkUInt32;pUserData,pImmData:PDWORD);
  Procedure  AddFuncLayout2(addr:ADataLayout;size:DWORD;imm:TImmData);
  function   InsertImm(imm:TImmData):DWORD;
  Procedure  EnumFuncLayout(cb:TvCustomLayoutCb;Fset:TVkUInt32;pUserData,pImmData:PDWORD);
  Procedure  AddImmData(D:DWORD);
  function   GetImmData:PDWORD;
  function   IsPSSimpleShader:Boolean;
  function   IsVSSimpleShader:Boolean;
  function   IsCSClearShader:Boolean;
  function   IsVSRectListShader:Boolean;
 end;

 TBufBindExt=packed record
  fset  :TVkUInt32;
  bind  :TVkUInt32;
  offset:TVkUInt32;
  memuse:TVkUInt32;

  addr  :Pointer;
  size  :TVkUInt32;

  cformat:TVkFormat;
 end;

 TvBindImageType=(vbSampled,vbStorage,vbMipStorage);

 TImageBindExt=packed record
  btype :TvBindImageType;
  fset  :TVkUInt32;
  bind  :TVkUInt32;
  memuse:TVkUInt32;

  FImage:TvImageKey;
  FView :TvImageViewKey;
 end;

 TSamplerBindExt=packed record
  fset:TVkUInt32;
  bind:TVkUInt32;

  PS:PSSharpResource4;
 end;

 TvUniformBuilder=object
  FBuffers :array of TBufBindExt;
  FImages  :array of TImageBindExt;
  FSamplers:array of TSamplerBindExt;

  Procedure InsertBuffer(var b:TBufBindExt);

  Procedure AddVSharp2(PV:PVSharpResource2;fset,bind,size,offset,mask:DWord;flags:TvLayoutFlags);
  Procedure AddVSharp4(PV:PVSharpResource4;fset,bind,size,offset:DWord;flags:TvLayoutFlags);
  Procedure AddBufPtr (P:Pointer          ;fset,bind,size,offset:DWord;flags:TvLayoutFlags);

  Procedure AddTSharp4(PT:PTSharpResource4;btype:TvBindImageType;fset,bind:DWord;flags:TvLayoutFlags);
  Procedure AddTSharp8(PT:PTSharpResource8;btype:TvBindImageType;fset,bind:DWord;flags:TvLayoutFlags);
  Procedure AddSSharp4(PS:PSSharpResource4;fset,bind:DWord);
  procedure AddAttr   (const b:TvCustomLayout;Fset:TVkUInt32;pUserData,pImmData:PDWORD);
 end;

 AvShaderStage=array[TvShaderStage] of TvShaderExt;

 PvShadersKey=^TvShadersKey;
 TvShadersKey=object
  FShaders:AvShaderStage;
  FPrimtype:Integer;
  Procedure SetLSShader(Shader:TvShaderExt);
  Procedure SetHSShader(Shader:TvShaderExt);
  Procedure SetESShader(Shader:TvShaderExt);
  Procedure SetGSShader(Shader:TvShaderExt);
  Procedure SetVSShader(Shader:TvShaderExt);
  Procedure SetPSShader(Shader:TvShaderExt);
  Procedure SetCSShader(Shader:TvShaderExt);
  procedure ExportLayout(var A:AvSetLayout;var B:AvPushConstantRange);
  Procedure ExportStages(Stages:PVkPipelineShaderStageCreateInfo;stageCount:PVkUInt32);
 end;

 TvBindVertexBuffer=packed object
  min_addr:Pointer;
  binding :Word;
  stride  :Word;
  count   :TVkUInt32;
  Function GetSize:TVkUInt32;
 end;

 AvVertexInputBindingDescription   =array[0..31] of TVkVertexInputBindingDescription;
 AvBindVertexBuffer                =array[0..31] of TvBindVertexBuffer;
 AvVertexInputAttributeDescription =array[0..31] of TVkVertexInputAttributeDescription;

 AvVertexInputBindingDescription2  =array[0..31] of TVkVertexInputBindingDescription2EXT;
 AvVertexInputAttributeDescription2=array[0..31] of TVkVertexInputAttributeDescription2EXT;

 TvVertexInputEXT=record
  vertexBindingDescriptionCount  :TVkUInt32;
  vertexAttributeDescriptionCount:TVkUInt32;
  VertexBindingDescriptions      :AvVertexInputBindingDescription2;
  VertexAttributeDescriptions    :AvVertexInputAttributeDescription2;
 end;

 TvAttrBuilder=object
  const
   maxVertexInputBindingStride=16383;
   maxVertexInputBindings     =32;
   maxVertexInputAttributes   =32;
  var
   FBindDescsCount:Byte;
   FAttrDescsCount:Byte;
   FBindDescs:AvVertexInputBindingDescription;
   FBindVBufs:AvBindVertexBuffer;
   FAttrDescs:AvVertexInputAttributeDescription;
  //
  function  NewBindDesc(binding,stride,count:TVkUInt32;base:Pointer):TVkUInt32;
  procedure NewAttrDesc(location,binding,offset:TVkUInt32;format:TVkFormat);
  procedure PatchAttr(binding,offset:TVkUInt32);
  Procedure AddVSharp(PV:PVSharpResource4;location:DWord);
  procedure AddAttr(const v:TvCustomLayout;Fset:TVkUInt32;pUserData,pImmData:PDWORD);
  Procedure Export2(var input:TvVertexInputEXT);
 end;

 TvShaderGroup=class
  FKey   :TvShadersKey;
  FLayout:TvPipelineLayout;
  Procedure Clear;
  Function  Compile:Boolean;
  Procedure ExportAttrBuilder(var AttrBuilder   :TvAttrBuilder   ;GPU_USERDATA:PGPU_USERDATA);
  Procedure ExportUnifBuilder(var UniformBuilder:TvUniformBuilder;GPU_USERDATA:PGPU_USERDATA);
 end;

function GetSharpByPatch(pUserData,pImmData:Pointer;const addr:ADataLayout):Pointer;

function IsClearDepthShaders(const FShaders:AvShaderStage):Boolean; inline;

function test_desc(const b:TvCustomLayout;pUserData,pImmData:PDWORD):Boolean;

implementation

uses
 kern_dmem;

function TvCustomLayout.GetVulkanDescType:TVkDescriptorType;
begin
 case dtype of
  dtSAMPLER:Result:=VK_DESCRIPTOR_TYPE_SAMPLER;
  dtSAM_IMG:Result:=VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE;
  dtSTR_IMG:Result:=VK_DESCRIPTOR_TYPE_STORAGE_IMAGE;
  dtUTX_BUF:Result:=VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER;
  dtSTX_BUF:Result:=VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER;
  dtUNF_BUF:Result:=VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
  dtSTR_BUF:Result:=VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
  else
   Assert(false);
 end;
end;

{
function TShaderFuncKey.c(var a,b:TShaderFuncKey):Integer;
begin
 //1 FLen
 Result:=Integer((a.FLen>b.FLen) and (b.FLen<>0))-Integer((a.FLen<b.FLen) and (a.FLen<>0));
 if (Result<>0) then Exit;

 //2 pData
 Result:=CompareDWord(a.pData^,b.pData^,Max(a.FLen,b.FLen) div 4);
end;
}

Function TvBindVertexBuffer.GetSize:TVkUInt32;
begin
 if (stride=0) then
 begin
  Result:=count;
 end else
 begin
  Result:=stride*count;
 end;
end;

procedure TvShaderExt.ClearInfo;
begin
 inherited;

 //dont clear FDescSetId

 FSetLayout:=nil;

 FVertLayouts:=Default(ACustomLayout);
 FUnifLayouts:=Default(ACustomLayout);
 FFuncLayouts:=Default(ACustomLayout);

 FPushConst:=Default(TvCustomLayout);

 SetLength(FImmData,0);
end;

Destructor TvShaderExt.Destroy;
begin
 inherited;
end;

function TvShaderExt.parser:CvShaderParser;
begin
 Result:=TvShaderParserExt;
end;

procedure TvShaderExt.InitSetLayout;
var
 i,p:Integer;
 descriptorCount:TVkUInt32;
 A:AVkDescriptorSetLayoutBinding;
begin
 if (FSetLayout<>nil) then Exit;
 A:=Default(AVkDescriptorSetLayoutBinding);

  //++ other todo
 SetLength(A,
           Length(FUnifLayouts)
          );

 p:=0;
 if (Length(FUnifLayouts)<>0) then
 begin
  For i:=0 to High(FUnifLayouts) do
  begin
   if (vMipArray in FUnifLayouts[i].flags) then
   begin
    descriptorCount:=16;
   end else
   begin
    descriptorCount:=1;
   end;
   //
   A[p]:=Default(TVkDescriptorSetLayoutBinding);
   A[p].binding        :=FUnifLayouts[i].bind;
   A[p].descriptorType :=FUnifLayouts[i].GetVulkanDescType;
   A[p].descriptorCount:=descriptorCount;
   A[p].stageFlags     :=ord(FStage);
   //
   Inc(p);
  end;
 end;

 FSetLayout:=FetchSetLayout(ord(FStage),0,A);
end;

procedure TvShaderExt.AddToPipeline(p:TvPipelineLayout);
begin
 InitSetLayout;

 p.AddLayout(FSetLayout);
 if (FPushConst.size<>0) then
 begin
  p.AddPushConst(0,FPushConst.size,ord(FStage));
 end;
end;

procedure TvShaderParserExt.Parse(data:Pointer;size:Ptruint);
begin
 FEXPORT_INFO_ID:=-1;
 inherited;
 CloseData(0,True);
end;

procedure TvShaderParserExt.OnDescriptorSet(var Target,id:DWORD);
begin
 with TvShaderExt(FOwner) do
 begin
  if (FDescSetId>=0) then id:=FDescSetId;
 end;
end;

function _get_hex_dword(P:PChar):DWord;
var
 Error:word;
 s:string[9];
begin
 s[0]:=#9;
 s[1]:='$';
 PQWORD(@s[2])^:=PQWORD(P)^;
 Result:=0;
 Val(s,Result,Error);
end;

function _get_hex_char(P:PChar):DWord;
begin
 case P^ of
  '0'..'9':Result:=ord(P^)-ord('0');
  'A'..'F':Result:=ord(P^)-ord('A')+$A;
 end;
end;

Procedure AddToCustomLayout(var A:ACustomLayout;const v:TvCustomLayout);
begin
 Insert(v,A,Length(A));
end;

Procedure AddToDataLayout(var A:ADataLayout;const v:TvDataLayout);
begin
 Insert(v,A,Length(A));
end;

procedure TvShaderParserExt.OnSourceExtension(P:PChar);
label
 _state2;
var
 curr   :PChar;
 c_state:Integer;
 c_deep :Integer;
 c_hdtp :AnsiChar;
 c_name :RawByteString;
begin
 c_state:=0;
 c_deep :=0;
 c_hdtp :=#0;
 c_name :='';

 //Writeln(P);

 curr:=P;
 while (curr^<>#0) do
 begin
  case c_state of
   0:
     begin
      case curr^ of
       #9,' ':
         begin
          Inc(c_deep);
         end;
       '#','+','-','*','%':
         begin
          //header
          Inc(c_deep);
          c_hdtp :=curr^;
          c_state:=1;
         end;
       else
         begin
          c_state:=2;
          goto _state2;
         end;
      end;
     end;
   1: //[ ][ ][#][HEADER]
     begin
      CloseData(c_deep,True);
      case c_hdtp of
       '#':PushData(Trim(curr));
       '+':PushDesc(Trim(curr));
       '-':PushCache(Trim(curr));
       '*':PushDesc(Trim(curr));
       '%':PushDesc(Trim(curr));
       else;
      end;
      Exit;
     end;
   2: //[ ][ ][NAME]
     begin
      _state2:

      case curr^ of
       ':':
        begin
         c_state:=3;
        end;
       else
        begin
         c_name:=c_name+curr^;
        end;
      end;
     end;
   3: //[ ][ ][NAME][:][VALUE]
     begin
      CloseData(c_deep,False);
      OnValue(Trim(c_name),Trim(curr));
      Exit;
     end;
   else
    Assert(false);
  end;

  Inc(curr);
 end;

end;

{
+R
 +VTX
 +VTX
 +F
  +S
   V
  G
 +B
}

procedure TvShaderParserExt.CloseData(c_deep:Integer;c_header:Boolean);

 Procedure Pop;
 var
  L:PvDataLayout2;
 begin
  L:=@FDataStack[High(FDataStack)];
  //
  case L^.state of
   cData:PopData(L);
   cDesc:PopDesc(L);
   else;
  end;
  //
  L^:=Default(TvDataLayout2);
  //
  SetLength(FDataStack,High(FDataStack));
 end;

begin
 if c_header then
 begin
  while (Length(FDataStack)<>0) and
        (c_deep<=Length(FDataStack)) do
  begin
   Pop;
  end;
 end else
 begin
  while (Length(FDataStack)<>0) and
        (c_deep<Length(FDataStack)) do
  begin
   Pop;
  end;
 end;
end;

procedure TvShaderParserExt.PushData(const N:RawByteString);
Var
 L:TvDataLayout2;
begin
 L:=Default(TvDataLayout2);
 L.state:=cData;

 Case N of
  'R':L.rtype:=vtRoot   ;
  'D':L.rtype:=vtImmData;
  'B':L.rtype:=vtBufPtr2;
  'F':L.rtype:=vtFunPtr2;
  'v':L.rtype:=vtVSharp2;
  'V':L.rtype:=vtVSharp4;
  'S':L.rtype:=vtSSharp4;
  't':L.rtype:=vtTSharp4;
  'T':L.rtype:=vtTSharp8;
  'L':L.rtype:=vtLDS    ;
  'G':L.rtype:=vtGDS    ;
  else
   Assert(false,'TODO: Unknow data layout:"'+N+'"');
 end;

 Insert(L,FDataStack,Length(FDataStack));
end;

procedure TvShaderParserExt.PushDesc(const N:RawByteString);
Var
 L:TvDataLayout2;
begin
 L:=Default(TvDataLayout2);
 L.state:=cDesc;

 Case N of
  'VTX_ATR':L.dtype:=dtVTX_ATR;
  'SAMPLER':L.dtype:=dtSAMPLER;
  'SAM_IMG':L.dtype:=dtSAM_IMG;
  'STR_IMG':L.dtype:=dtSTR_IMG;
  'RNT_IMG':L.dtype:=dtRNT_IMG;
  'UTX_BUF':L.dtype:=dtUTX_BUF;
  'STX_BUF':L.dtype:=dtSTX_BUF;
  'RTX_BUF':L.dtype:=dtRTX_BUF;
  'UNF_BUF':L.dtype:=dtUNF_BUF;
  'STR_BUF':L.dtype:=dtSTR_BUF;
  'PSH_CST':L.dtype:=dtPSH_CST;
  else
   Assert(false,'TODO: Unknow desc type:"'+N+'"');
 end;

 if (L.dtype=dtSTR_BUF) then
 begin
  L.size:=High(DWORD);
 end;

 Insert(L,FDataStack,Length(FDataStack));
end;

procedure TvShaderParserExt.PushCache(const N:RawByteString);
Var
 L:TvDataLayout2;
begin
 L:=Default(TvDataLayout2);
 L.state:=cCache;

 case N of
  'EXPORT_COLOR':Inc(FEXPORT_INFO_ID);
  else;
 end;

 Insert(L,FDataStack,Length(FDataStack));
end;

function StrToDWord2(const s:RawByteString):DWord;
begin
 Result:=0;
 if Copy(S,1,2)='0x' then
 begin
  TryStrToDWord('$'+Copy(S,3),Result);
 end else
 begin
  TryStrToDWord(S,Result);
 end;
end;

function StrToFlags(const s:RawByteString):TvLayoutFlags;
var
 i:Integer;
begin
 Result:=[];
 For i:=1 to Length(s) do
 begin
  case UpCase(s[i]) of
   'R':Result:=Result+[vMemoryRead];
   'W':Result:=Result+[vMemoryWrite];
   'M':Result:=Result+[vMipArray];
   'D':Result:=Result+[vForceDegamma];
   else;
  end;
 end;
end;

function StrToDstSel(const s:RawByteString):TrDstSel;
var
 i:Integer;
 v:Byte;
begin
 Result:=Default(TrDstSel);
 For i:=1 to Length(s) do
 begin
  case UpCase(s[i]) of
   '0':v:=0;
   '1':v:=1;
   'R':v:=4;
   'G':v:=5;
   'B':v:=6;
   'A':v:=7;
   else
       v:=0; //incorrect value is interpreted as 0
  end;
  case i of
   1:Result.x:=v;
   2:Result.y:=v;
   3:Result.z:=v;
   4:Result.w:=v;
  end;
 end;
end;

procedure StrToRinfo(const s:RawByteString;var rinfo:TvResInfo);
var
 i:Integer;
begin
 For i:=1 to Length(s) do
 begin
  case UpCase(s[i]) of
   'O':rinfo.roffset:=True;
   'S':rinfo.rstride:=True;
   'D':rinfo.rdsel  :=True;
   'F':rinfo.rfmt   :=True;
   else;
  end;
 end;
end;

procedure TvShaderParserExt.OnValue(const N,V:RawByteString);
var
 L:PvDataLayout2;
begin
 if (Length(FDataStack)=0) then Exit;
 L:=@FDataStack[High(FDataStack)];

 case N of
  'BND':L^.bind  :=StrToDWord2(V);
  'LEN':L^.size  :=StrToDWord2(V);
  'OFS':L^.offset:=StrToDWord2(V);

  'FLG':L^.flags:=StrToFlags(V);
  'MSK':L^.mask :=StrToDWord2(V);

  'RINF':StrToRinfo(V,L^.rinfo);
  'INVL':L^.rinfo.invalid:=(StrToDWord2(V)<>0);
  'DFMT':L^.rinfo.dfmt   :=StrToDWord2(V);
  'NFMT':L^.rinfo.nfmt   :=StrToDWord2(V);
  'STRD':L^.rinfo.stride :=StrToDWord2(V);
  'TYPE':L^.rinfo.rtype  :=StrToDWord2(V);
  'DSEL':L^.rinfo.dstsel :=StrToDstSel(V);
  'FDGM':L^.rinfo.degam  :=(StrToDWord2(V)<>0);

  'IMM':Insert(StrToDWord2(V),L^.imm,Length(L^.imm));

  'VGPR_COMP_CNT':
    with TvShaderExt(FOwner) do
    begin
     FParams.VGPR_COMP_CNT:=StrToDWord2(V);
    end;
  'VGT_STEP_RATE_0':
    with TvShaderExt(FOwner) do
    begin
     FParams.STEP_RATE_0:=StrToDWord2(V);
    end;
  'VGT_STEP_RATE_1':
    with TvShaderExt(FOwner) do
    begin
     FParams.STEP_RATE_1:=StrToDWord2(V);
    end;

  'DB_SHADER_CONTROL':
    with TvShaderExt(FOwner) do
    begin
     DWORD(FParams.SHADER_CONTROL):=StrToDWord2(V);
    end;

  'PS_NUM_INTERP':
    with TvShaderExt(FOwner) do
    begin
     FParams.NUM_INTERP:=StrToDWord2(V);
    end;

  'PS_INPUT_CNTL':
    with TvShaderExt(FOwner) do
    begin
     if (FINPUT_CNTL_ID<Length(A_INPUT_CNTL)) then
     begin
      DWORD(FParams.INPUT_CNTL[FINPUT_CNTL_ID]):=StrToDWord2(V);
      Inc(FINPUT_CNTL_ID);
     end;
    end;

  'EXPORT_COUNT':
    with TvShaderExt(FOwner) do
    begin
     FParams.EXPORT_COUNT:=StrToDWord2(V);
    end;

  'FORMAT':
    with TvShaderExt(FOwner) do
    begin
     if (FEXPORT_INFO_ID>=0) then
     if (FEXPORT_INFO_ID<Length(AEXPORT_COLOR)) then
     begin
      FParams.EXPORT_COLOR[FEXPORT_INFO_ID].FORMAT:=StrToDWord2(V);
     end;
    end;

  'NUMBER_TYPE':
    with TvShaderExt(FOwner) do
    begin
     if (FEXPORT_INFO_ID>=0) then
     if (FEXPORT_INFO_ID<Length(AEXPORT_COLOR)) then
     begin
      FParams.EXPORT_COLOR[FEXPORT_INFO_ID].NUMBER_TYPE:=StrToDWord2(V);
     end;
    end;

  'COMP_SWAP':
    with TvShaderExt(FOwner) do
    begin
     if (FEXPORT_INFO_ID>=0) then
     if (FEXPORT_INFO_ID<Length(AEXPORT_COLOR)) then
     begin
      FParams.EXPORT_COLOR[FEXPORT_INFO_ID].COMP_SWAP:=StrToDWord2(V);
     end;
    end;

  'CS_NUM_THREAD_X':
    with TvShaderExt(FOwner) do
    begin
     DWORD(FParams.NUM_THREAD_X):=StrToDWord2(V);
    end;
  'CS_NUM_THREAD_Y':
    with TvShaderExt(FOwner) do
    begin
     DWORD(FParams.NUM_THREAD_Y):=StrToDWord2(V);
    end;
  'CS_NUM_THREAD_Z':
    with TvShaderExt(FOwner) do
    begin
     DWORD(FParams.NUM_THREAD_Z):=StrToDWord2(V);
    end;

  else
   Assert(false,'Unknow Value '+N);
 end;

 //Writeln(N,' ',V);
end;

function TvShaderParserExt.GetLayoutAddr:ADataLayout;
var
 i:Integer;
 D:TvDataLayout;
 L:PvDataLayout2;
begin
 Result:=Default(ADataLayout);
 D:=Default(TvDataLayout);

 if Length(FDataStack)<>0 then
 For i:=High(FDataStack) downto 0 do
 begin
  L:=@FDataStack[i];

  if (L^.state=cData) then
  begin

   //alloc offset
   if (L^.rtype=vtImmData) then
   if (not L^.immofs) then
   begin
    with TvShaderExt(FOwner) do
    begin
     L^.offset:=InsertImm(L^.imm);
    end;
    L^.immofs:=True;
   end;

   D.rtype :=L^.rtype;
   D.offset:=L^.offset;
   D.rinfo :=L^.rinfo;

   Insert(D,Result,Length(Result));
  end;
 end;
end;

procedure TvShaderParserExt.PopData(L:PvDataLayout2);
begin

 case L^.rtype of
  vtFunPtr2:
     with TvShaderExt(FOwner) do
     begin
      AddFuncLayout2(Self.GetLayoutAddr,L^.size,L^.imm);
     end;
  vtImmData:; //skip
  vtLDS    :; //skip
  vtGDS    :; //skip
  else
     with TvShaderExt(FOwner) do
     begin
      AddDataLayout2(Self.GetLayoutAddr,0);
     end;
 end;

end;

procedure TvShaderParserExt.PopDesc(L:PvDataLayout2);
begin
 with TvShaderExt(FOwner) do
  case L^.dtype of
   dtVTX_ATR:AddVertLayout2(Self.GetLayoutAddr,L^.bind);
   dtSAMPLER:AddUnifLayout2(L^.dtype,Self.GetLayoutAddr,L^.bind,L^.flags);
   dtSAM_IMG:AddUnifLayout2(L^.dtype,Self.GetLayoutAddr,L^.bind,L^.flags);
   dtSTR_IMG:AddUnifLayout2(L^.dtype,Self.GetLayoutAddr,L^.bind,L^.flags);
   dtRNT_IMG:AddUnifLayout2(L^.dtype,Self.GetLayoutAddr,L^.bind,L^.flags);
   dtUTX_BUF:Assert(False,'TODO:UTX_BUF');
   dtSTX_BUF:Assert(False,'TODO:STX_BUF');
   dtRTX_BUF:Assert(False,'TODO:RTX_BUF');
   dtUNF_BUF:AddBuffLayout2(L^.dtype,Self.GetLayoutAddr,L^.bind,L^.size,L^.offset,L^.mask,L^.flags);
   dtSTR_BUF:AddBuffLayout2(L^.dtype,Self.GetLayoutAddr,L^.bind,L^.size,L^.offset,L^.mask,L^.flags);
   dtPSH_CST:SetPushConst2(Self.GetLayoutAddr,L^.size);
   else;
  end;
end;

Procedure TvShaderExt.EnumFuncLayout(cb:TvFuncCb);
var
 i:Integer;
begin
 if (cb=nil) then Exit;
 if (Length(FFuncLayouts)=0) then Exit;
 For i:=0 to High(FFuncLayouts) do
 begin
  cb(FFuncLayouts[i].addr);
 end;
end;

Procedure TvShaderExt.AddVertLayout2(addr:ADataLayout;bind:DWORD);
var
 v:TvCustomLayout;
begin
 v:=Default(TvCustomLayout);
 v.bind :=bind;
 v.addr :=addr;
 v.flags:=[vMemoryRead];

 AddToCustomLayout(FVertLayouts,v);
end;

Procedure TvShaderExt.EnumVertLayout(cb:TvCustomLayoutCb;Fset:TVkUInt32;pUserData,pImmData:PDWORD);
var
 i:Integer;
begin
 if (cb=nil) then Exit;
 if (Length(FVertLayouts)=0) then Exit;
 For i:=0 to High(FVertLayouts) do
 begin
  cb(FVertLayouts[i],Fset,pUserData,pImmData);
 end;
end;

function CmpADataLayout(const a,b:ADataLayout):Boolean;
begin
 Result:=False;
 if (Length(a)=Length(b)) then
 begin
  Result:=(CompareByte(a[0],b[0],SizeOf(TvDataLayout)*Length(a))=0);
 end;
end;

Procedure TvShaderExt.AddDataLayout2(addr:ADataLayout;offset:DWORD);

var
 v:TvCustomLayout;
begin
 if (Length(FDataLayouts)<>0) then
 if CmpADataLayout(FDataLayouts[High(FDataLayouts)].addr,addr) then
 begin
  Exit;
 end;

 v:=Default(TvCustomLayout);
 v.dtype :=dtDATA;
 v.addr  :=addr;
 v.offset:=offset;

 AddToCustomLayout(FDataLayouts,v);
end;

Procedure TvShaderExt.AddBuffLayout2(dtype:TvDescriptorType;
                                     addr:ADataLayout;
                                     bind,size,offset,mask:DWORD;
                                     flags:TvLayoutFlags);

var
 v:TvCustomLayout;
begin
 AddDataLayout2(addr,offset); //moving the offset from the desc to the data

 v:=Default(TvCustomLayout);
 v.dtype :=dtype;
 v.bind  :=bind;
 v.size  :=size;
 v.offset:=offset;
 v.mask  :=mask;
 v.flags :=flags;
 v.addr  :=addr;

 AddToCustomLayout(FUnifLayouts,v);
end;

Procedure TvShaderExt.SetPushConst2(addr:ADataLayout;size:DWORD);
begin
 FPushConst:=Default(TvCustomLayout);
 FPushConst.dtype:=dtPSH_CST;
 FPushConst.size :=size;
 FPushConst.addr :=addr;
end;

Function TvShaderExt.GetPushConstData(pUserData:Pointer):Pointer;
begin
 Result:=nil;
 if (pUserData=nil) then Exit;
 if (FPushConst.size=0) then Exit;

 Result:=GetSharpByPatch(pUserData,GetImmData,FPushConst.addr);

 if (Result=nil) then Exit;

 Case FPushConst.addr[0].rtype of
  vtVSharp2,
  vtVSharp4:Result:=Pointer(PVSharpResource4(Result)^.base and (not 3));
  vtTSharp4,
  vtTSharp8:Result:=Pointer(QWORD(PTSharpResource4(Result)^.base) shl 8);
  else;
 end;

end;

Procedure TvShaderExt.AddUnifLayout2(dtype:TvDescriptorType;
                                     addr:ADataLayout;
                                     bind:DWORD;
                                     flags:TvLayoutFlags);
var
 v:TvCustomLayout;
begin
 v:=Default(TvCustomLayout);
 v.dtype:=dtype;
 v.bind :=bind;
 v.flags:=flags;
 v.addr :=addr;

 AddToCustomLayout(FUnifLayouts,v);
end;

Procedure TvShaderExt.EnumUnifLayout(cb:TvCustomLayoutCb;Fset:TVkUInt32;pUserData,pImmData:PDWORD);
var
 i:Integer;
begin
 if (cb=nil) then Exit;
 if (Length(FUnifLayouts)=0) then Exit;
 For i:=0 to High(FUnifLayouts) do
 begin
  cb(FUnifLayouts[i],Fset,pUserData,pImmData);
 end;
end;

Procedure TvShaderExt.AddFuncLayout2(addr:ADataLayout;size:DWORD;imm:TImmData);
var
 v:TvCustomLayout;
begin
 v:=Default(TvCustomLayout);
 v.size  :=size;
 v.addr  :=addr;
 v.offset:=InsertImm(imm);

 AddToCustomLayout(FFuncLayouts,v);
end;

function TvShaderExt.InsertImm(imm:TImmData):DWORD;
begin
 Result:=Length(FImmData)*SizeOf(DWORD);
 //
 Insert(imm,FImmData,Length(FImmData));
end;

Procedure TvShaderExt.EnumFuncLayout(cb:TvCustomLayoutCb;Fset:TVkUInt32;pUserData,pImmData:PDWORD);
var
 i:Integer;
begin
 if (cb=nil) then Exit;
 if (Length(FFuncLayouts)=0) then Exit;
 For i:=0 to High(FFuncLayouts) do
 begin
  cb(FFuncLayouts[i],Fset,pUserData,pImmData);
 end;
end;

Procedure TvShaderExt.AddImmData(D:DWORD);
begin
 Insert(D,FImmData,Length(FImmData));
end;

function TvShaderExt.GetImmData:PDWORD;
begin
 Result:=@FImmData[0];
end;

//

function TvShaderExt.IsPSSimpleShader:Boolean;
begin
 if (self=nil) then Exit(False);

 Result:=(FHash_gcn=QWORD($E9FF5D4699E5B9AD));
end;

function TvShaderExt.IsVSSimpleShader:Boolean;
begin
 if (self=nil) then Exit(False);

 Result:=(FHash_gcn=QWORD($00DF6E6331449451));
end;

function TvShaderExt.IsCSClearShader:Boolean;
begin
 if (self=nil) then Exit(False);

 case FHash_gcn of
  QWORD($7DCE68F83F66B337):Result:=True;
  QWORD($B3628C7542451F40):Result:=True;
  QWORD($1248CAD0608E843B):Result:=True;
  QWORD($19FC060ACE401D0D):Result:=True;
  else
   Result:=False;
 end;
end;

function TvShaderExt.IsVSRectListShader:Boolean;
begin
 if (self=nil) then Exit(False);

 Result:=(FHash_gcn=QWORD($00DF6E6331449451));
end;

function IsClearDepthShaders(const FShaders:AvShaderStage):Boolean; inline;
begin
 Result:=False;

 if (FShaders[vShaderStageLs]=nil) and
    (FShaders[vShaderStageHs]=nil) and
    (FShaders[vShaderStageEs]=nil) and
    (FShaders[vShaderStageGs]=nil) and
    (FShaders[vShaderStageCs]=nil) then

 if (FShaders[vShaderStageVs].IsVSSimpleShader) and
    (
     FShaders[vShaderStagePs].IsPSSimpleShader or
     (FShaders[vShaderStagePs]=nil)
    ) then
 begin
  Result:=True;

 end;
end;

///

function GetSharpByPatch(pUserData,pImmData:Pointer;const addr:ADataLayout):Pointer;
var
 i:Integer;
 pData :Pointer;
 pSharp:Pointer;
 pDmem :Pointer;
begin
 Result:=nil;
 if (Length(addr)=0) then Exit;

 pData :=pUserData;
 pSharp:=pUserData;
 pDmem :=pUserData;

 For i:=High(addr) downto 0 do
 begin
  pData:=pData+addr[i].offset;
  pDmem:=pDmem+addr[i].offset;

  Case addr[i].rtype of
   vtRoot:
     begin
      pSharp:=pData;
      pDmem :=pData;
     end;
   vtImmData:
     begin
      pData :=pImmData+addr[i].offset;
      pSharp:=pData;
      pDmem :=pData;
     end;
   vtBufPtr2:
     begin
      pData:=Pointer(PPtrUint(pDmem)^ and (not 3));

      pDmem:=get_dmem_ptr(pData);

      pSharp:=pData;
     end;
   vtFunPtr2:
     begin
      pData:=PPointer(pDmem)^;

      pDmem:=get_dmem_ptr(pData);

      pSharp:=pData;
     end;
   vtVSharp2,
   vtVSharp4:
     begin
      pSharp:=pData;

      if (i<>0) then
      begin
       pData:=Pointer(PVSharpResource4(pDmem)^.base and (not 3));

       pDmem:=get_dmem_ptr(pData);
      end;

     end;
   vtSSharp4:
     begin
      pSharp:=pData;
      Break;
     end;
   vtTSharp4,
   vtTSharp8:
     begin
      pSharp:=pData;

      if (i<>0) then
      begin
       pData:=Pointer(QWORD(PTSharpResource4(pDmem)^.base) shl 8);

       pDmem:=get_dmem_ptr(pData);
      end;

     end;
   else
    Assert(false,'GetSharpByPatch:'+IntToStr(ord(addr[i].rtype)));
  end;

 end;

 Result:=pSharp;
end;

//

function TvAttrBuilder.NewBindDesc(binding,stride,count:TVkUInt32;base:Pointer):TVkUInt32;
var
 i:Byte;
begin
 if (FBindDescsCount>=maxVertexInputBindings) then
 begin
  Assert(false,'maxVertexInputBindings');
 end;

 i:=FBindDescsCount;
 FBindDescsCount:=FBindDescsCount+1;

 FBindVBufs[i].min_addr:=base;
 FBindVBufs[i].binding :=binding;
 FBindVBufs[i].stride  :=stride;
 FBindVBufs[i].count   :=count;

 FBindDescs[i].binding  :=binding;
 FBindDescs[i].stride   :=stride;
 FBindDescs[i].inputRate:=VK_VERTEX_INPUT_RATE_VERTEX;

 Result:=i;
end;

procedure TvAttrBuilder.NewAttrDesc(location,binding,offset:TVkUInt32;format:TVkFormat);
var
 i:Integer;
begin
 if (FAttrDescsCount>=maxVertexInputAttributes) then
 begin
  Assert(false,'maxVertexInputAttributes');
 end;

 i:=FAttrDescsCount;
 FAttrDescsCount:=FAttrDescsCount+1;

 FAttrDescs[i].location:=location;
 FAttrDescs[i].binding :=binding ;
 FAttrDescs[i].format  :=format  ;
 FAttrDescs[i].offset  :=offset  ;
end;

procedure TvAttrBuilder.PatchAttr(binding,offset:TVkUInt32);
var
 i:Integer;
begin
 if (FAttrDescsCount<>0) then
  For i:=0 to FAttrDescsCount-1 do
  if (FAttrDescs[i].binding=binding) then
  begin
   FAttrDescs[i].offset:=FAttrDescs[i].offset+offset;
  end;
end;

function _ptr_diff(p1,p2:Pointer):TVkUInt32; inline;
begin
 if (p1>p2) then
  Result:=p1-p2
 else
  Result:=p2-p1;
end;

Procedure TvAttrBuilder.AddVSharp(PV:PVSharpResource4;location:DWord);
var
 pv_base  :Pointer;
 pv_stride:TVkUInt32;
 pv_count :TVkUInt32;
 offset   :TVkUInt32;
 i:Integer;
begin
 if (PV=nil) then Exit;

 Assert(PV^.dfmt<>0,'AddVSharp:invalid dfmt!');

 pv_base  :=Pointer(PV^.base and (not 3));
 pv_stride:=PV^.stride;
 pv_count :=PV^.num_records;

 if (FBindDescsCount<>0) then
  For i:=0 to FBindDescsCount-1 do
  begin
   With FBindVBufs[i] do
   begin
    //If the element's stride is the same,
    // add the attribute to the binding
    if (stride=pv_stride) then
    begin
     //Let's calculate the difference in addresses
     // between the binding and the new buffer
     offset:=_ptr_diff(min_addr,pv_base);
     //If the difference is greater than the stride,
     // then we skip
     if (offset<=stride-1) then
     begin
      //If the offset is negative relative
      // to the base, then
      if (min_addr>pv_base) then
      begin
       //We patch the previous attributes,
       // adjust the offset
       PatchAttr(binding,offset);
       //This is now the new base address,
       // so reset the offset.
       min_addr:=pv_base;
       offset  :=0;
      end;

      //update count
      if (count<pv_count) then
      begin
       count:=pv_count;
      end;

      NewAttrDesc(location,binding,offset,_get_vsharp_cformat(PV));
      Exit;
     end;
    end;
   end;
  end;

 //Binding not found, adding new binding and attribute

 i:=FBindDescsCount;

 NewBindDesc(i,pv_stride,pv_count,pv_base);

 NewAttrDesc(location,i,0,_get_vsharp_cformat(PV));

end;

procedure TvAttrBuilder.AddAttr(const v:TvCustomLayout;Fset:TVkUInt32;pUserData,pImmData:PDWORD);
var
 PV:PVSharpResource4;
begin
 PV:=GetSharpByPatch(pUserData,pImmData,v.addr);
 //print_vsharp(PV);
 AddVSharp(PV,v.bind);
end;

Procedure TvAttrBuilder.Export2(var input:TvVertexInputEXT);
var
 i:Byte;
begin
 input:=Default(TvVertexInputEXT);

 input.vertexBindingDescriptionCount  :=FBindDescsCount;
 input.vertexAttributeDescriptionCount:=FAttrDescsCount;

 if (FBindDescsCount<>0) then
 For i:=0 to FBindDescsCount-1 do
 begin
  input.VertexBindingDescriptions[i].sType    :=VK_STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT;
  input.VertexBindingDescriptions[i].binding  :=FBindDescs[i].binding;
  input.VertexBindingDescriptions[i].stride   :=FBindDescs[i].stride;
  input.VertexBindingDescriptions[i].inputRate:=FBindDescs[i].inputRate;
  input.VertexBindingDescriptions[i].divisor  :=1;
 end;

 if (FAttrDescsCount<>0) then
 For i:=0 to FAttrDescsCount-1 do
 begin
  input.VertexAttributeDescriptions[i].sType   :=VK_STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT;
  input.VertexAttributeDescriptions[i].location:=FAttrDescs[i].location;
  input.VertexAttributeDescriptions[i].binding :=FAttrDescs[i].binding;
  input.VertexAttributeDescriptions[i].format  :=FAttrDescs[i].format;
  input.VertexAttributeDescriptions[i].offset  :=FAttrDescs[i].offset;
 end;

end;

//

function _get_buf_mem_usage(flags:TvLayoutFlags):Byte; inline;
begin
 Result:=(ord(vMemoryRead   in flags)*TM_READ) or
         (ord(vMemoryWrite  in flags)*TM_WRITE);
end;

Procedure TvUniformBuilder.InsertBuffer(var b:TBufBindExt);
begin
 Insert(b,FBuffers,Length(FBuffers));
end;

Procedure TvUniformBuilder.AddVSharp2(PV:PVSharpResource2;fset,bind,size,offset,mask:DWord;flags:TvLayoutFlags);
var
 b:TBufBindExt;
 stride:Integer;

 base,start,__end,_size:QWORD;

 V:TVSharpResource2;
begin
 Assert(PV<>nil);
 if (PV=nil) then Exit;

 //print_vsharp(PVSharpResource4(PV));

 b:=Default(TBufBindExt);
 b.fset  :=fset;
 b.bind  :=bind;
 b.offset:=offset;
 b.memuse:=_get_buf_mem_usage(flags);

 V:=PV^;
 PDWORD(@V)[1]:=PDWORD(@V)[1] or mask;

 b.addr:=Pointer(V.base and (not 3));

 stride:=V.stride;
 if (stride=0) then stride:=1;

 //size is unknow, try 4KB
 base :=QWORD(get_dmem_ptr(b.addr));
 start:=base;
 __end:=base+4*1024;

 gpu_get_bound(start,__end);

 if (start=0) then
 begin
  Assert(false,'vtVSharp2');
 end;

 _size:=(__end-base);

 _size:=_size+offset; //take into account the offset inside the shader

 if (_size>4*1024) then
 begin
  _size:=4*1024;
 end;

 //
 b.size:=_size;

 b.cformat:=VK_FORMAT_UNDEFINED;

 InsertBuffer(b);
end;

function IsInvalidVSharp(nfmt,dfmt:Byte;num_records:DWORD):Boolean; inline;
begin
 Result:=(dfmt=0) or (dfmt=15) or (num_records=0);
end;

function IsInvalidTSharp(nfmt,dfmt,_type:Byte):Boolean; inline;
begin
 Result:=(nfmt in [IMG_NUM_FORMAT_RESERVED_6 ,
                   IMG_NUM_FORMAT_RESERVED_8 ,
                   IMG_NUM_FORMAT_RESERVED_10,
                   IMG_NUM_FORMAT_RESERVED_11,
                   IMG_NUM_FORMAT_RESERVED_12,
                   IMG_NUM_FORMAT_RESERVED_13,
                   IMG_NUM_FORMAT_RESERVED_14,
                   IMG_NUM_FORMAT_RESERVED_15]
         ) or
         (dfmt in [IMG_DATA_FORMAT_INVALID    ,
                   IMG_DATA_FORMAT_RESERVED_15,
                   IMG_DATA_FORMAT_RESERVED_23,
                   IMG_DATA_FORMAT_RESERVED_24,
                   IMG_DATA_FORMAT_RESERVED_25,
                   IMG_DATA_FORMAT_RESERVED_26,
                   IMG_DATA_FORMAT_RESERVED_27,
                   IMG_DATA_FORMAT_RESERVED_28,
                   IMG_DATA_FORMAT_RESERVED_29,
                   IMG_DATA_FORMAT_RESERVED_30,
                   IMG_DATA_FORMAT_RESERVED_31]
         ) or
         (_type in [SQ_RSRC_IMG_RSVD_0,
                    SQ_RSRC_IMG_RSVD_1,
                    SQ_RSRC_IMG_RSVD_2,
                    SQ_RSRC_IMG_RSVD_3,
                    SQ_RSRC_IMG_RSVD_4,
                    SQ_RSRC_IMG_RSVD_5,
                    SQ_RSRC_IMG_RSVD_6,
                    SQ_RSRC_IMG_RSVD_7]
         );
end;

Procedure TvUniformBuilder.AddVSharp4(PV:PVSharpResource4;fset,bind,size,offset:DWord;flags:TvLayoutFlags);
var
 b:TBufBindExt;
 stride,num_records:Integer;

 invalid:Integer;
begin
 Assert(PV<>nil);
 if (PV=nil) then Exit;

 //print_vsharp(PV);

 invalid:=ord(IsInvalidVSharp(PV^.nfmt,PV^.dfmt,PV^.num_records))*TM_INVAL;

 b:=Default(TBufBindExt);
 b.fset  :=fset;
 b.bind  :=bind;
 b.offset:=offset;
 b.memuse:=_get_buf_mem_usage(flags) or invalid;

 b.addr:=Pointer(PV^.base and (not 3));

 stride     :=PV^.stride;
 num_records:=PV^.num_records;
 //
 if (stride=0)      then stride:=1;
 if (num_records=0) then num_records:=1;
 //
 b.size:=(stride*num_records)+offset; //take into account the offset inside the shader
 //
 if (b.size>size) then b.size:=size;  //input size already taking into account offset

 b.cformat:=_get_vsharp_cformat(PV);

 InsertBuffer(b);
end;

Procedure TvUniformBuilder.AddBufPtr(P:Pointer;fset,bind,size,offset:DWord;flags:TvLayoutFlags);
var
 b:TBufBindExt;

 base,start,__end,_size:QWORD;
begin
 Assert(P<>nil);
 if (P=nil) or (size=0) then Exit;

 b:=Default(TBufBindExt);
 b.fset  :=fset;
 b.bind  :=bind;
 b.offset:=offset;
 b.memuse:=_get_buf_mem_usage(flags);

 b.addr:=P;
 b.size:=size; //input size already taking into account offset

 if (size=$FFFFFFFF) then
 begin
  //size is unknow, try 4KB
  base :=QWORD(get_dmem_ptr(b.addr));
  start:=base;
  __end:=base+4*1024;

  gpu_get_bound(start,__end);

  _size:=(__end-base);

  if (_size>4*1024) then
  begin
   _size:=4*1024;
  end;

  b.size:=_size;
 end;

 b.cformat:=VK_FORMAT_UNDEFINED;

 InsertBuffer(b);
end;

Procedure TvUniformBuilder.AddTSharp4(PT:PTSharpResource4;btype:TvBindImageType;fset,bind:DWord;flags:TvLayoutFlags);
var
 b:TImageBindExt;
 hint:s_image_usage;

 //start,__end:QWORD;
begin
 Assert(PT<>nil);
 if (PT=nil) then Exit;

 //print_tsharp4(PT);

 b:=Default(TImageBindExt);
 b.btype :=btype;
 b.fset  :=fset;
 b.bind  :=bind;
 b.memuse:=_get_buf_mem_usage(flags);

 hint:=[];

 if (vForceDegamma in flags) then
 begin
  hint:=hint+[iu_degamma];
 end;

 if (btype in [vbStorage,vbMipStorage]) then
 begin
  hint:=hint+[iu_storage];
 end;

 b.FImage:=_get_tsharp4_image_info(PT,hint);
 b.FView :=_get_tsharp4_image_view(PT,hint);

 {
 //Marking textures that contain incorrect
 // virtual memory as invalid,
 // I don't know how correct this approach is
 start:=QWORD(get_dmem_ptr(b.FImage.Addr));
 __end:=start+1;
 gpu_get_bound(start,__end);
 if (start=0) then
 begin
  Writeln(HexStr(QWORD(b.FImage.Addr),10),'->INVALID');
  b.FImage.params.invalid:=1;
 end;
 }

 b.memuse:=b.memuse or (b.FImage.params.invalid)*TM_INVAL;

 Insert(b,FImages,Length(FImages));
end;

Procedure TvUniformBuilder.AddTSharp8(PT:PTSharpResource8;btype:TvBindImageType;fset,bind:DWord;flags:TvLayoutFlags);
var
 b:TImageBindExt;
 hint:s_image_usage;

 //start,__end:QWORD;
begin
 Assert(PT<>nil);
 if (PT=nil) then Exit;

 //print_tsharp8(PT);

 b:=Default(TImageBindExt);
 b.btype :=btype;
 b.fset  :=fset;
 b.bind  :=bind;
 b.memuse:=_get_buf_mem_usage(flags);

 hint:=[];

 if (vForceDegamma in flags) then
 begin
  hint:=hint+[iu_degamma];
 end;

 if (btype in [vbStorage,vbMipStorage]) then
 begin
  hint:=hint+[iu_storage];
 end;

 b.FImage:=_get_tsharp8_image_info(PT,hint);
 b.FView :=_get_tsharp8_image_view(PT,hint);

 {
 //Marking textures that contain incorrect
 // virtual memory as invalid,
 // I don't know how correct this approach is
 start:=QWORD(get_dmem_ptr(b.FImage.Addr));
 __end:=start+1;
 gpu_get_bound(start,__end);
 if (start=0) then
 begin
  Writeln(HexStr(QWORD(b.FImage.Addr),10),'->INVALID');
  b.FImage.params.invalid:=1;
 end;
 }

 b.memuse:=b.memuse or (b.FImage.params.invalid)*TM_INVAL;

 Insert(b,FImages,Length(FImages));
end;

procedure TvUniformBuilder.AddAttr(const b:TvCustomLayout;Fset:TVkUInt32;pUserData,pImmData:PDWORD);
var
 P:Pointer;
begin
 P:=GetSharpByPatch(pUserData,pImmData,b.addr);
 Assert(P<>nil);
 if (P=nil) then Exit;

 Case b.dtype of
  dtSAMPLER:
    Case b.addr[0].rtype of
     vtSSharp4:AddSSharp4(P,fset,b.bind);
     else
      Assert(false,'AddAttr');
    end;
  //
  dtSAM_IMG:
    Case b.addr[0].rtype of
     vtTSharp4:AddTSharp4(P,vbSampled,fset,b.bind,b.flags);
     vtTSharp8:AddTSharp8(P,vbSampled,fset,b.bind,b.flags);
     else
      Assert(false,'AddAttr');
    end;
  //
  dtSTR_IMG:
    if (vMipArray in b.flags) then
    begin
     Case b.addr[0].rtype of
      vtTSharp4:AddTSharp4(P,vbMipStorage,fset,b.bind,b.flags);
      vtTSharp8:AddTSharp8(P,vbMipStorage,fset,b.bind,b.flags);
      else
       Assert(false,'AddAttr');
     end;
    end else
    begin
     Case b.addr[0].rtype of
      vtTSharp4:AddTSharp4(P,vbStorage,fset,b.bind,b.flags);
      vtTSharp8:AddTSharp8(P,vbStorage,fset,b.bind,b.flags);
      else
       Assert(false,'AddAttr');
     end;
    end;
  //
  dtUNF_BUF,
  dtSTR_BUF:
    Case b.addr[0].rtype of
     vtRoot,
     vtBufPtr2:AddBufPtr (P,Fset,b.bind,b.size,b.offset,b.flags);
     vtVSharp2:AddVSharp2(P,Fset,b.bind,b.size,b.offset,b.mask,b.flags);
     vtVSharp4:AddVSharp4(P,Fset,b.bind,b.size,b.offset,b.flags);
     else
      Assert(false,'AddAttr');
    end;

  else
   Assert(false,'AddAttr');
 end;

 //Writeln('----');
end;

function AlignShift(addr:Pointer;alignment:PtrUInt):PtrUInt; inline;
begin
 if (alignment>1) then
 begin
  Result:=(PtrUInt(addr) mod alignment);
 end else
 begin
  Result:=0;
 end;
end;

Procedure TvUniformBuilder.AddSSharp4(PS:PSSharpResource4;fset,bind:DWord);
var
 b:TSamplerBindExt;
begin
 Assert(PS<>nil);
 if (PS=nil) then Exit;

 //print_ssharp4(PS);

 b:=Default(TSamplerBindExt);
 b.fset:=fset;
 b.bind:=bind;
 b.PS:=PS;

 Insert(b,FSamplers,Length(FSamplers));
end;

//

Procedure TvShadersKey.SetLSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_VERTEX_BIT) then
  FShaders[vShaderStageLs]:=Shader;
end;

Procedure TvShadersKey.SetHSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT) then
  FShaders[vShaderStageHs]:=Shader;
end;

Procedure TvShadersKey.SetESShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT) then
  FShaders[vShaderStageEs]:=Shader;
end;

Procedure TvShadersKey.SetGSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_GEOMETRY_BIT) then
  FShaders[vShaderStageGs]:=Shader;
end;

Procedure TvShadersKey.SetVSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_VERTEX_BIT) then
  FShaders[vShaderStageVs]:=Shader;
end;

Procedure TvShadersKey.SetPSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_FRAGMENT_BIT) then
  FShaders[vShaderStagePs]:=Shader;
end;

Procedure TvShadersKey.SetCSShader(Shader:TvShaderExt);
begin
 if (Shader=nil) then Exit;
 if (Shader.FStage=VK_SHADER_STAGE_COMPUTE_BIT) then
 begin
  FShaders[vShaderStageCs]:=Shader;
 end;
end;

procedure TvShadersKey.ExportLayout(var A:AvSetLayout;
                                    var B:AvPushConstantRange);
var
 i:TvShaderStage;
 Shader:TvShaderExt;
 ia,p:Integer;
 CacheLayout:TvSetLayout;
begin
 p:=0;

 //need sorted by FDescSetId

 For i:=Low(TvShaderStage) to High(TvShaderStage) do
 begin
  Shader:=FShaders[i];
  if (Shader<>nil) then
  begin
   Shader.InitSetLayout;

   p:=Shader.FDescSetId;

   ia:=Length(A);
   if ((p+1)>ia) then
   begin
    SetLength(A,p+1);
    For ia:=ia to High(A) do
    begin
     A[ia]:=nil;
    end;
   end;

   A[p]:=Shader.FSetLayout;

   if (Shader.FPushConst.size<>0) then
   begin
    p:=Length(B);
    SetLength(B,p+1);

    B[p]:=Default(TVkPushConstantRange);
    B[p].stageFlags:=ord(Shader.FStage);
    B[p].offset    :=Shader.FPushConst.offset;
    B[p].size      :=Shader.FPushConst.size;
   end;

  end;
 end;

 //fill zeros
 if (Length(A)<>0) then
 begin
  CacheLayout:=nil;
  For ia:=0 to High(A) do
   if (A[ia]=nil) then
   begin
    if (CacheLayout=nil) then
    begin
     CacheLayout:=FetchSetLayout(0,0,[]);
    end;

    A[ia]:=CacheLayout;
   end;
 end;
end;

Procedure TvShadersKey.ExportStages(Stages:PVkPipelineShaderStageCreateInfo;stageCount:PVkUInt32);
var
 i:TvShaderStage;
 c:Integer;
begin
 c:=0;
 For i:=Low(TvShaderStage) to High(TvShaderStage) do
  if (FShaders[i]<>nil) then
  begin
   Assert(FShaders[i].FHandle<>VK_NULL_HANDLE);
   Stages[c].sType :=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
   Stages[c].stage :=FShaders[i].FStage;
   Stages[c].module:=FShaders[i].FHandle;
   Stages[c].pName :=PChar(FShaders[i].FEntry);
   Inc(c);
  end;
 stageCount^:=c;
end;

Procedure TvShaderGroup.Clear;
begin
 FKey:=Default(TvShadersKey);
 FLayout:=nil;
end;

Function TvShaderGroup.Compile:Boolean;
var
 A:AvSetLayout;
 B:AvPushConstantRange;
begin
 Result:=True;
 if (FLayout<>nil) then Exit;

 A:=Default(AvSetLayout);
 B:=Default(AvPushConstantRange);

 FKey.ExportLayout(A,B);

 FLayout:=FetchPipelineLayout(A,B);
 Result:=(FLayout<>nil);
end;

Procedure TvShaderGroup.ExportAttrBuilder(var AttrBuilder:TvAttrBuilder;GPU_USERDATA:PGPU_USERDATA);
var
 Shader:TvShaderExt;
 pUserData,pImmData:PDWORD;
begin
 Shader:=FKey.FShaders[vShaderStageVs];
 if (Shader<>nil) then
 begin
  pUserData:=GPU_USERDATA^.get_user_data(vShaderStageVs);
  pImmData :=Shader.GetImmData;
  Shader.EnumVertLayout(@AttrBuilder.AddAttr,Shader.FDescSetId,pUserData,pImmData);
 end;
end;

Procedure TvShaderGroup.ExportUnifBuilder(var UniformBuilder:TvUniformBuilder;GPU_USERDATA:PGPU_USERDATA);
var
 Shader:TvShaderExt;
 i:TvShaderStage;
 pUserData,pImmData:PDWORD;
begin
 For i:=Low(TvShaderStage) to High(TvShaderStage) do
 begin
  Shader:=FKey.FShaders[i];
  if (Shader<>nil) then
  begin
   pUserData:=GPU_USERDATA^.get_user_data(i);
   pImmData :=Shader.GetImmData;
   Shader.EnumUnifLayout(@UniformBuilder.AddAttr,Shader.FDescSetId,pUserData,pImmData);
  end;
 end;
end;

function GetNumType(nfmt:Byte):Byte; inline;
begin
 Case nfmt of
  IMG_NUM_FORMAT_UNORM  :Result:=IMG_NUM_FORMAT_UNORM;
  IMG_NUM_FORMAT_SRGB   :Result:=IMG_NUM_FORMAT_UNORM;

  IMG_NUM_FORMAT_SNORM  :Result:=IMG_NUM_FORMAT_SNORM;

  IMG_NUM_FORMAT_UINT   :Result:=IMG_NUM_FORMAT_UINT;
  IMG_NUM_FORMAT_SINT   :Result:=IMG_NUM_FORMAT_SINT;

  IMG_NUM_FORMAT_USCALED:Result:=IMG_NUM_FORMAT_FLOAT;
  IMG_NUM_FORMAT_SSCALED:Result:=IMG_NUM_FORMAT_FLOAT;
  IMG_NUM_FORMAT_FLOAT  :Result:=IMG_NUM_FORMAT_FLOAT;
  else
                         Result:=IMG_NUM_FORMAT_RESERVED_15;
 end;
end;

function test_desc(const b:TvCustomLayout;pUserData,pImmData:PDWORD):Boolean;
var
 P:Pointer;
 PV:PVSharpResource4 absolute P;
 PT:PTSharpResource4 absolute P;
 PS:PSSharpResource4 absolute P;
 a:QWORD;
 rinfo:TvResInfo;
begin
 Result:=True;

 P:=GetSharpByPatch(pUserData,pImmData,b.addr);
 if (P=nil) then Exit;

 rinfo:=b.addr[0].rinfo;

 Case b.addr[0].rtype of
  vtVSharp2:
   with PVSharpResource2(P)^ do
   begin

    if rinfo.rstride then
    if (stride<>rinfo.stride) then
    begin
     Exit(False);
    end;

   end;
  vtVSharp4:
   with PV^ do
   begin

    if (IsInvalidVSharp(nfmt,dfmt,num_records)<>rinfo.invalid) then
    begin
     Exit(False);
    end else
    if rinfo.invalid then
    begin
     //do not check further
     Exit(True);
    end;

    if rinfo.rstride then
    if (stride<>rinfo.stride) then
    begin
     Exit(False);
    end;

    if rinfo.rdsel then
    if (dst_sel_x<>rinfo.dstsel.x) or
       (dst_sel_y<>rinfo.dstsel.y) or
       (dst_sel_z<>rinfo.dstsel.z) or
       (dst_sel_w<>rinfo.dstsel.w) then
    begin
     Exit(False);
    end;

    if rinfo.rfmt then
    if (dfmt<>rinfo.dfmt) or
       (nfmt<>rinfo.nfmt) then
    begin
     Exit(False);
    end;

   end;

  vtSSharp4:
   begin
    with PS^ do
    begin

     if (force_degamma<>ord(rinfo.degam)) then
     begin
      Exit(False);
     end;

    end;
   end;

  vtTSharp4,
  vtTSharp8:
   with PT^ do
   begin

    if (IsInvalidTSharp(nfmt,dfmt,_type)<>rinfo.invalid) then
    begin
     Exit(False);
    end else
    if rinfo.invalid then
    begin
     //do not check further
     Exit(True);
    end;

    if (rinfo.rtype<>_type) then
    begin
     Exit(False);
    end;

    if rinfo.rdsel then
    if (dst_sel_x<>rinfo.dstsel.x) or
       (dst_sel_y<>rinfo.dstsel.y) or
       (dst_sel_z<>rinfo.dstsel.z) or
       (dst_sel_w<>rinfo.dstsel.w) then
    begin
     Exit(False);
    end;

    if rinfo.rfmt then
    if (dfmt            <>rinfo.dfmt) or
       (GetNumType(nfmt)<>GetNumType(rinfo.nfmt)) then
    begin
     Exit(False);
    end;

   end;
  else;
 end;

 if rinfo.roffset then
 Case b.addr[0].rtype of
  vtRoot,
  vtBufPtr2:
    begin
     a:=AlignShift(P,limits.minStorageBufferOffsetAlignment);
     if (a<>b.offset) then
     begin
      Exit(False);
     end;
    end;
  vtVSharp2,
  vtVSharp4:
    begin
     a:=AlignShift(Pointer(PV^.base and (not 3)),limits.minStorageBufferOffsetAlignment);
     if (a<>b.offset) then
     begin
      Exit(False);
     end;
    end;
  else
   Assert(false,'AddAttr');
 end;

end;

end.




