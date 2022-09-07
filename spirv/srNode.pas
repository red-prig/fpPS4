unit srNode;

{$mode objfpc}{$H+}

interface

type
 PsrNode=^TsrNode;
 PPsrNode=^PsrNode;

 PPrepTypeNode=^TPrepTypeNode;
 TPrepTypeNode=record
  dnode:PsrNode;
  rtype:Integer;
 end;

 TsrNodeVmt=class
   class Procedure add_read    (node,src:PsrNode);             virtual;
   class Procedure rem_read    (node,src:PsrNode);             virtual;
   //
   class Procedure add_write   (node,src:PsrNode);             virtual;
   class Procedure rem_write   (node,src:PsrNode);             virtual;
   //
   class Procedure mark_read   (node,src:PsrNode);             virtual;
   class Procedure mark_unread (node,src:PsrNode);             virtual;
   //
   class Procedure zero_read   (node:PsrNode);                 virtual;
   class Procedure zero_unread (node:PsrNode);                 virtual;
   //
   class Procedure mark_write  (node,src:PsrNode);             virtual;
   class Procedure mark_unwrite(node,src:PsrNode);             virtual;
   //
   class Function  pwrite_count(node:PsrNode):PDWORD           virtual;
   class Procedure SetWriter(node,w,line:PsrNode);             virtual;
   class Procedure ResetWriter(node,w:PsrNode);                virtual;
   class Function  is_used(node:PsrNode):Boolean               virtual;
   class Function  GetPtype(node:PsrNode):PsrNode;             virtual;
   class Function  GetStorageClass(node:PsrNode):DWORD;        virtual;
   class function  GetStorageName(node:PsrNode):RawByteString; virtual;
   class function  GetPrintName(node:PsrNode):RawByteString;   virtual;
   class function  GetPrintData(node:PsrNode):RawByteString;   virtual;
   class function  Down(node:PsrNode):Pointer;                 virtual;
   class function  Next(node:PsrNode):Pointer;                 virtual;
   class function  Prev(node:PsrNode):Pointer;                 virtual;
   class function  Parent(node:PsrNode):Pointer;               virtual;
   class function  First(node:PsrNode):Pointer;                virtual;
   class function  Last(node:PsrNode):Pointer;                 virtual;
   class Procedure PrepType(node:PPrepTypeNode);               virtual;
   //
   class function  GetIndexCount(node:PsrNode):DWORD;          virtual;
   //
   class function  GetRef(node:PsrNode):Pointer;               virtual;
   //
   class function  GetData(node:PsrNode;data:Pointer):Ptruint; virtual;
 end;

 TsrNodeType=class of TsrNodeVmt;

 TsrNode=packed object
  protected
   fntype:TsrNodeType;
  private
   fread_count:DWORD;
  public
   property    ntype:TsrNodeType read fntype;
   property    read_count:DWORD  read fread_count;
   function    IsType(t:TsrNodeType):Boolean;
   function    AsType(t:TsrNodeType):Pointer;
   Procedure   mark_read   (src:PsrNode);
   Procedure   mark_unread (src:PsrNode);
   Procedure   mark_write  (src:PsrNode);
   Procedure   mark_unwrite(src:PsrNode);
   function    write_count:DWORD;
   Procedure   SetWriter(w,line:PsrNode);
   Procedure   ResetWriter(w:PsrNode);
   function    IsUsed:Boolean;
   Function    pType:Pointer;
   Function    GetStorageClass:DWORD;
   function    GetStorageName:RawByteString;
   function    GetPrintName:RawByteString;
   function    GetPrintData:RawByteString;
   function    Next:Pointer;
   function    Prev:Pointer;
   function    Parent:Pointer;
   function    First:Pointer;
   function    Last:Pointer;
   procedure   PrepType(rtype:Integer);
   function    GetIndexCount:DWORD;
   function    GetRef:Pointer;
   function    GetData(data:Pointer):Ptruint;
 end;

 TCustomEmit=class
  Function  Alloc(Size:ptruint):Pointer;                   virtual abstract;
  Function  GetConfig          :Pointer;                   virtual abstract;
  Function  GetCodeHeap        :Pointer;                   virtual abstract;
  Function  GetLiteralList     :Pointer;                   virtual abstract;
  Function  GetTypeList        :Pointer;                   virtual abstract;
  Function  GetConstList       :Pointer;                   virtual abstract;
  Function  GetRegsStory       :Pointer;                   virtual abstract;
  Function  GetCapabilityList  :Pointer;                   virtual abstract;
  Function  GetHeaderList      :Pointer;                   virtual abstract;
  Function  GetDecorateList    :Pointer;                   virtual abstract;
  Function  GetDebugInfoList   :Pointer;                   virtual abstract;
  Function  GetVariableList    :Pointer;                   virtual abstract;
  Function  GetInputList       :Pointer;                   virtual abstract;
  Function  GetOutputList      :Pointer;                   virtual abstract;
  Function  GetDataLayoutList  :Pointer;                   virtual abstract;
  Function  GetVertLayoutList  :Pointer;                   virtual abstract;
  Function  GetFragLayoutList  :Pointer;                   virtual abstract;
  Function  GetBufferList      :Pointer;                   virtual abstract;
  Function  GetUniformList     :Pointer;                   virtual abstract;
  Function  GetBitcastList     :Pointer;                   virtual abstract;
  Function  GetCacheOpList     :Pointer;                   virtual abstract;
  Function  GetFuncList        :Pointer;                   virtual abstract;
  Function  GetCursor          :Pointer;                   virtual abstract;
  function  curr_line          :Pointer;                   virtual abstract;
  function  init_line          :Pointer;                   virtual abstract;
  function  NewRefNode         :PsrNode;                   virtual abstract;
  function  OpCast  (pLine,      dst,src:PsrNode):PsrNode; virtual abstract;
  function  OpLoad  (pLine,dtype,dst,src:PsrNode):PsrNode; virtual abstract;
  function  OpStore (pLine,      dst,src:PsrNode):PsrNode; virtual abstract;
  procedure PostLink(pLine,      dst:PsrNode);             virtual abstract;
  procedure emit_spi;                                      virtual abstract;
 end;

implementation

class Procedure TsrNodeVmt.add_read(node,src:PsrNode);
begin
 Inc(node^.fread_count);
end;

class Procedure TsrNodeVmt.rem_read(node,src:PsrNode);
begin
 Assert(node^.fread_count<>0,node^.ntype.ClassName);
 if (node^.fread_count=0) then Exit;
 Dec(node^.fread_count);
end;

class Procedure TsrNodeVmt.add_write(node,src:PsrNode);
var
 p:PDWORD;
begin
 p:=node^.fntype.pwrite_count(node);
 if (p<>nil) then
 begin
  Inc(p^);
 end;
end;

class Procedure TsrNodeVmt.rem_write(node,src:PsrNode);
var
 p:PDWORD;
begin
 p:=node^.fntype.pwrite_count(node);
 if (p<>nil) then
 begin
  Assert(p^<>0);
  if (p^=0) then Exit;
  Dec(p^);
 end;
end;

class Procedure TsrNodeVmt.mark_read(node,src:PsrNode);
var
 prv:Boolean;
begin
 repeat
  Assert(node^.fntype<>nil);

  prv:=node^.IsUsed;

  node^.fntype.add_read(node,src);

  if (prv=node^.IsUsed) then Exit;

  node^.fntype.zero_read(node);

  src :=node;
  node:=node^.fntype.Down(node);
 until (node=nil);
end;

class Procedure TsrNodeVmt.mark_unread(node,src:PsrNode);
var
 prv:Boolean;
begin
 repeat
  Assert(node^.fntype<>nil);

  prv:=node^.IsUsed;

  node^.fntype.rem_read(node,src);

  if (prv=node^.IsUsed) then Exit;

  node^.fntype.zero_unread(node);

  src :=node;
  node:=node^.fntype.Down(node);
 until (node=nil);
end;

class Procedure TsrNodeVmt.zero_read(node:PsrNode);
begin
 //
end;

class Procedure TsrNodeVmt.zero_unread(node:PsrNode);
begin
 //
end;

class Procedure TsrNodeVmt.mark_write(node,src:PsrNode);
var
 prv:Boolean;
begin
 repeat
  Assert(node^.fntype<>nil);

  prv:=node^.IsUsed;

  node^.fntype.add_write(node,src);

  if (prv=node^.IsUsed) then Exit;

  node^.fntype.zero_read(node);

  src :=node;
  node:=node^.fntype.Down(node);
 until (node=nil);
end;

class Procedure TsrNodeVmt.mark_unwrite(node,src:PsrNode);
var
 prv:Boolean;
begin
 repeat
  Assert(node^.fntype<>nil);

  prv:=node^.IsUsed;

  node^.fntype.rem_write(node,src);

  if (prv=node^.IsUsed) then Exit;

  node^.fntype.zero_unread(node);

  src :=node;
  node:=node^.fntype.Down(node);
 until (node=nil);
end;

class Function TsrNodeVmt.pwrite_count(node:PsrNode):PDWORD;
begin
 Result:=nil;
end;

class Procedure TsrNodeVmt.SetWriter(node,w,line:PsrNode);
begin
 w^.mark_read(node);
end;

class Procedure TsrNodeVmt.ResetWriter(node,w:PsrNode);
begin
 w^.mark_unread(node);
end;

class Function TsrNodeVmt.is_used(node:PsrNode):Boolean;
begin
 Result:=(node^.read_count<>0) or (node^.write_count<>0);
end;

class Function TsrNodeVmt.GetPtype(node:PsrNode):PsrNode;
begin
 Result:=nil;
end;

class Function TsrNodeVmt.GetStorageClass(node:PsrNode):DWORD;
begin
 Result:=6;
end;

class function TsrNodeVmt.GetStorageName(node:PsrNode):RawByteString;
begin
 Result:='';
end;

class function TsrNodeVmt.GetPrintName(node:PsrNode):RawByteString;
begin
 Result:='';
end;

class function TsrNodeVmt.GetPrintData(node:PsrNode):RawByteString;
begin
 Result:='';
end;

class function TsrNodeVmt.Down(node:PsrNode):Pointer;
begin
 Result:=nil;
end;

class function TsrNodeVmt.Next(node:PsrNode):Pointer;
begin
 Result:=nil;
end;

class function TsrNodeVmt.Prev(node:PsrNode):Pointer;
begin
 Result:=nil;
end;

class function TsrNodeVmt.Parent(node:PsrNode):Pointer;
begin
 Result:=nil;
end;

class function TsrNodeVmt.First(node:PsrNode):Pointer;
begin
 Result:=nil;
end;

class function TsrNodeVmt.Last(node:PsrNode):Pointer;
begin
 Result:=nil;
end;

class Procedure TsrNodeVmt.PrepType(node:PPrepTypeNode);
begin
 node^.dnode:=nil;
end;

class function TsrNodeVmt.GetIndexCount(node:PsrNode):DWORD;
begin
 Result:=0;
end;

class function TsrNodeVmt.GetRef(node:PsrNode):Pointer;
begin
 Result:=nil;
end;

class function TsrNodeVmt.GetData(node:PsrNode;data:Pointer):Ptruint;
begin
 Result:=0;
end;

//

function TsrNode.IsType(t:TsrNodeType):Boolean;
begin
 Result:=False;
 if (@Self=nil) then Exit;
 Result:=fntype.InheritsFrom(t);
end;

function TsrNode.AsType(t:TsrNodeType):Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 if not fntype.InheritsFrom(t) then Exit;
 Result:=@Self;
end;

Procedure TsrNode.mark_read(src:PsrNode);
begin
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 fntype.mark_read(@Self,src);
end;

Procedure TsrNode.mark_unread(src:PsrNode);
begin
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 fntype.mark_unread(@Self,src);
end;

Procedure TsrNode.mark_write(src:PsrNode);
begin
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 fntype.mark_write(@Self,src);
end;

Procedure TsrNode.mark_unwrite(src:PsrNode);
begin
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 fntype.mark_unwrite(@Self,src);
end;

function TsrNode.write_count:DWORD;
var
 p:PDWORD;
begin
 Result:=0;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 p:=fntype.pwrite_count(@Self);
 if (p=nil) then Exit;
 Result:=p^;
end;

Procedure TsrNode.SetWriter(w,line:PsrNode);
begin
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 fntype.SetWriter(@Self,w,line);
end;

Procedure TsrNode.ResetWriter(w:PsrNode);
begin
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 fntype.ResetWriter(@Self,w);
end;

function TsrNode.IsUsed:Boolean;
begin
 Result:=False;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.is_used(@Self);
end;

Function TsrNode.pType:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.GetPtype(@Self);
end;

Function TsrNode.GetStorageClass:DWORD;
begin
 Result:=6;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.GetStorageClass(@Self);
end;

function TsrNode.GetStorageName:RawByteString;
begin
 Result:='';
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.GetStorageName(@Self);
end;

function TsrNode.GetPrintName:RawByteString;
begin
 Result:='';
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.GetPrintName(@Self);
end;

function TsrNode.GetPrintData:RawByteString;
begin
 Result:='';
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.GetPrintData(@Self);
end;

function TsrNode.Next:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.Next(@Self);
end;

function TsrNode.Prev:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.Prev(@Self);
end;

function TsrNode.Parent:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.Parent(@Self);
end;

function TsrNode.First:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.First(@Self);
end;

function TsrNode.Last:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.Last(@Self);
end;

procedure TsrNode.PrepType(rtype:Integer);
var
 node:TPrepTypeNode;
begin
 node:=Default(TPrepTypeNode);
 node.dnode:=@Self;
 node.rtype:=rtype;
 While (node.dnode<>nil) do
 begin
  Assert(node.dnode^.fntype<>nil);
  node.dnode^.fntype.PrepType(@node);
 end;
end;

function TsrNode.GetIndexCount:DWORD;
begin
 Result:=0;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.GetIndexCount(@Self);
end;

function TsrNode.GetRef:Pointer;
begin
 Result:=nil;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.GetRef(@Self);
end;

function TsrNode.GetData(data:Pointer):Ptruint;
begin
 Result:=0;
 if (@Self=nil) then Exit;
 Assert(fntype<>nil);
 Result:=fntype.GetData(@Self,data);
end;

end.

