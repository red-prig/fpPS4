unit SynEditLineStream;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SynEditTextBuffer,
  SynEditTextBase,
  LazSynEditText,
  SynLog,
  LineStream;

type
 TSynEditLineStream=class(TSynEditStringList)
  public
   var
    FSynLog:TCustomSynLog;
    FStream:TLineStream;
    FPCache:String;
   procedure Clear;                                                            override;
   procedure AddStrings(AStrings: TStrings);                                   override;
   procedure Delete(Index: integer);                                           override;
   procedure DeleteLines(Index, NumLines: integer);                            override;
   function  Get(Index: integer): string;                                      override;
   function  GetCapacity: integer;                                             override;
   procedure SetCapacity(NewCapacity: integer);                                override;
   function  GetCount: integer;                                                override;
   function  GetExpandedString(Index: integer): string;                        override;
   function  GetLengthOfLongestLine: integer;                                  override;
   function  GetObject(Index: integer): TObject;                               override;
   function  GetRange(Index: Pointer): TSynManagedStorageMem;                  override;
   function  Add(const S: string): integer;                                    override;
   procedure Insert(Index: integer; const S: string);                          override;
   procedure InsertLines(Index, NumLines: integer);                            override;
   function  GetPChar(ALineIndex: Integer; out ALen: Integer): PChar;          override;
   procedure Put(Index: integer; const S: string);                             override;
   procedure PutObject(Index: integer; AObject: TObject);                      override;
   procedure PutRange(Index: Pointer; const ARange: TSynManagedStorageMem);    override;
   procedure EditInsert(LogX, LogY: Integer; AText: String);                   override;
   function  EditDelete(LogX, LogY, ByteLen: Integer): String;                 override;
   function  EditReplace(LogX, LogY, ByteLen: Integer; AText: String): String; override;
   procedure EditLineBreak(LogX, LogY: Integer);                               override;
   procedure EditLineJoin(LogY: Integer; FillText: String = '');               override;
   procedure EditLinesInsert(LogY, ACount: Integer; AText: String = '');       override;
   procedure EditLinesDelete(LogY, ACount: Integer);                           override;
   procedure EditUndo(Item: TSynEditUndoItem);                                 override;
   procedure EditRedo(Item: TSynEditUndoItem);                                 override;
   procedure Update;
   procedure Reset;
 end;

implementation

procedure TSynEditLineStream.Clear;
begin
 //
end;

procedure TSynEditLineStream.AddStrings(AStrings: TStrings);
begin
 //
end;

procedure TSynEditLineStream.Delete(Index: integer);
begin
 //
end;

procedure TSynEditLineStream.DeleteLines(Index, NumLines: integer);
begin
 //
end;

function  TSynEditLineStream.Get(Index: integer): string;
begin
 Result:=FStream.Get(Index);
end;

function  TSynEditLineStream.GetCapacity: integer;
begin
 Result:=FStream.GetCount;
end;

procedure TSynEditLineStream.SetCapacity(NewCapacity: integer);
begin
 //
end;

function  TSynEditLineStream.GetCount: integer;
begin
 Result:=FStream.GetCount;
end;

function  TSynEditLineStream.GetExpandedString(Index: integer): string;
begin
 Result:=FStream.Get(Index);
end;

function  TSynEditLineStream.GetLengthOfLongestLine: integer;
begin
 Result:=100;
end;

function  TSynEditLineStream.GetObject(Index: integer): TObject;
begin
 Result:=nil;
end;

function  TSynEditLineStream.GetRange(Index: Pointer): TSynManagedStorageMem;
begin
 Result:=inherited;
 //Result:=nil;
end;

function  TSynEditLineStream.Add(const S: string): integer;
begin
//
end;

procedure TSynEditLineStream.Insert(Index: integer; const S: string);
begin
//
end;

procedure TSynEditLineStream.InsertLines(Index, NumLines: integer);
begin
 //
end;

function  TSynEditLineStream.GetPChar(ALineIndex: Integer; out ALen: Integer): PChar;
begin
 FPCache:=FStream.Get(ALineIndex);

 Result:=PChar(FPCache);
 ALen  :=Length(FPCache);
end;

procedure TSynEditLineStream.Put(Index: integer; const S: string);
begin
//
end;


procedure TSynEditLineStream.PutObject(Index: integer; AObject: TObject);
begin
 //
end;

procedure TSynEditLineStream.PutRange(Index: Pointer; const ARange: TSynManagedStorageMem);
begin
 //
end;

procedure TSynEditLineStream.EditInsert(LogX, LogY: Integer; AText: String);
begin
 //
end;

function  TSynEditLineStream.EditDelete(LogX, LogY, ByteLen: Integer): String;
begin
 //
end;

function  TSynEditLineStream.EditReplace(LogX, LogY, ByteLen: Integer; AText: String): String;
begin
 //
end;

procedure TSynEditLineStream.EditLineBreak(LogX, LogY: Integer);
begin
 //
end;

procedure TSynEditLineStream.EditLineJoin(LogY: Integer; FillText: String = '');
begin
 //
end;

procedure TSynEditLineStream.EditLinesInsert(LogY, ACount: Integer; AText: String = '');
begin
 //
end;

procedure TSynEditLineStream.EditLinesDelete(LogY, ACount: Integer);
begin
 //
end;

procedure TSynEditLineStream.EditUndo(Item: TSynEditUndoItem);
begin
 //
end;

procedure TSynEditLineStream.EditRedo(Item: TSynEditUndoItem);
begin
 //
end;

procedure TSynEditLineStream.Update;
var
 i,c,ffcnt,delta:Integer;
 tend:Boolean;
begin
 BeginUpdate;
 //
 i:=FSynLog.TopLine+FSynLog.LinesInWindow+3;
 //
 ffcnt:=FStream.GetCount;
 delta:=FStream.Update;
 //
 tend:=(i>=ffcnt);
 //
 if tend then
 begin
  i:=FSynLog.LinesInWindow;
  c:=FStream.GetCount;
  //
  if (c>i) then
  begin
   i:=c-i+1;
  end else
  begin
   i:=1;
  end;
  //
  FSynLog.TopLine:=i;
 end;
 //
 IncreaseTextChangeStamp;
 //
 SendNotification(senrLineChange, self, ffcnt-1, 1);
 SendNotification(senrLineCount , self, ffcnt-1, delta);
 //
 EndUpdate;
end;

procedure TSynEditLineStream.Reset;
var
 i,c,ffcnt,delta:Integer;
 tend:Boolean;
begin
 BeginUpdate;
 //
 i:=FSynLog.TopLine+FSynLog.LinesInWindow+3;
 //
 FStream.Reset;
 ffcnt:=FStream.GetCount;
 delta:=FStream.Update;
 //
 tend:=(i>=ffcnt);
 //
 if tend then
 begin
  i:=FSynLog.LinesInWindow;
  c:=FStream.GetCount;
  //
  if (c>i) then
  begin
   i:=c-i+1;
  end else
  begin
   i:=1;
  end;
  //
  FSynLog.TopLine:=i;
 end;
 //
 IncreaseTextChangeStamp;
 //
 SendNotification(senrLineCount , self, ffcnt-1, delta);
 //
 EndUpdate;
end;

end.

