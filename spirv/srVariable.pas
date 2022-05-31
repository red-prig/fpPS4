unit srVariable;

{$mode ObjFPC}{$H+}

interface

uses
  spirv,
  srNodes,
  srTypes,
  srRefId;

type
 PsrVariable=^TsrVariable;

 TsrVariable=object
  pPrev,pNext:PsrVariable;
  //----
  read_count:DWORD;
  write_count:DWORD;
  ID:TsrRefId; //post id
  pType:PsrType;
  dtype:TsrDataType;
  pSource:TOpParamSingle; //ntInput,ntVertLayout,ntFragLayout,ntOutput,ntUniform,ntBuffer
  Procedure mark_read;
  Procedure mark_unread;
  Procedure mark_write;
  Function  GetStorageClass:DWORD;
  Procedure Clear;
  function  GetName:RawByteString;
 end;

 TsrVariableList=specialize TNodeList<PsrVariable>;

implementation

uses
 srLayout,
 srVertLayout,
 srFragLayout,
 srInput,
 srOutput;

Procedure TsrVariable.mark_read;
begin
 Inc(read_count);
end;

Procedure TsrVariable.mark_unread;
begin
 if (read_count<>0) then Dec(read_count);
end;

Procedure TsrVariable.mark_write;
begin
 Inc(write_count);
end;

Function TsrVariable.GetStorageClass:DWORD;
begin
 Result:=StorageClass.Private_;
 if (pSource.pData<>nil) then
 Case pSource.ntype of
  ntInput,
  ntVertLayout,
  ntFragLayout,
  ntOutput    ,
  ntUniform   ,
  ntBuffer    :Result:=PsrDescriptor(pSource.pData)^.FStorage;
  else
   Assert(false,'GetStorageClass');
 end;
end;

Procedure TsrVariable.Clear;
begin
 if (pType<>nil) then
 begin
  pType^.mark_unread;
  pType:=nil;
 end;
 if (pSource.pData<>nil) then
 begin
  Case pSource.ntype of
   ntInput     ,
   ntVertLayout,
   ntFragLayout,
   ntOutput    ,
   ntUniform   ,
   ntBuffer    :PsrDescriptor(pSource.pData)^.pVar:=nil;
   else
    Assert(false,'Clear');
  end;
  pSource.pData:=nil;
 end;
end;

function TsrVariable.GetName:RawByteString;
begin
 Result:='';
 if (pSource.pData<>nil) then
 begin
  Case pSource.ntype of
   ntInput :
     Result:=PsrInput(pSource.pData)^.GetName;
   ntVertLayout:
     Result:=PsrVertLayout(pSource.pData)^.GetName;
   ntFragLayout:
     Result:=PsrFragLayout(pSource.pData)^.GetName;
   ntOutput:
     Result:=PsrOutput(pSource.pData)^.GetName;
   else;
  end;
 end;
end;

end.

