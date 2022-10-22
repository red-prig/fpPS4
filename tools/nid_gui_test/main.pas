unit main;

{$mode objfpc}{$H+}

interface

uses
  sha1,
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    NidBase64: TLabeledEdit;
    NidHex: TLabeledEdit;
    NidName: TLabeledEdit;
    procedure TextKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
    procedure NidBase64Click(Sender: TObject);
    procedure NidHexClick(Sender: TObject);
    procedure NidNameClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

uses
 ps4libdoc;

{$R *.lfm}

function ps4_nid_hash(NidName:PChar):QWORD;
const
 salt:array[0..15] of Byte=($51,$8D,$64,$A6,$35,$DE,$D8,$C1,$E6,$B0,$39,$B1,$C3,$E5,$52,$30);
var
 Context:TSHA1Context;
 Digest:TSHA1Digest;
begin
 SHA1Init(Context);
 SHA1Update(Context,NidName^,StrLen(NidName));
 SHA1Update(Context,salt,Length(salt));
 SHA1Final(Context,Digest);
 Result:=PQWORD(@Digest)^;
end;

function EncodeValue64(nVal:QWORD):RawByteString;
const
 nEncLenMax=11;
var
 i,nIndex:Integer;
begin
 SetLength(Result,nEncLenMax);
 For i:=nEncLenMax downto 1 do
 begin
  if (i<>nEncLenMax) then
  begin
   nIndex:=nVal and 63;
   nVal:=nVal shr 6;
  end else
  begin
   nIndex:=(nVal and 15) shl 2;
   nVal:=nVal shr 4;
  end;
  case nIndex of
    0..25:Result[i]:=Char(nIndex+Byte('A')-0);
   26..51:Result[i]:=Char(nIndex+Byte('a')-26);
   52..61:Result[i]:=Char(nIndex+Byte('0')-52);
       62:Result[i]:='+';
       63:Result[i]:='-';
  end;
 end;
end;

function DecodeValue64(strEnc:PAnsiChar;len:SizeUint;var nVal:QWORD):Boolean;
const
 nEncLenMax=11;
var
 i,nIndex:Integer;
begin
 Result:=False;
 nVal:=0;
 if (len>nEncLenMax) or (len=0) then Exit;
 For i:=0 to len-1 do
 begin
  case strEnc[i] of
   'A'..'Z':nIndex:=Byte(strEnc[i])-Byte('A')+0;
   'a'..'z':nIndex:=Byte(strEnc[i])-Byte('a')+26;
   '0'..'9':nIndex:=Byte(strEnc[i])-Byte('0')+52;
   '+':nIndex:=62;
   '-':nIndex:=63;
   else Exit;
  end;
  if (i<(nEncLenMax-1)) then
  begin
   nVal:=nVal shl 6;
   nVal:=nVal or nIndex;
  end else
  begin
   nVal:=nVal shl 4;
   nVal:=nVal or (nIndex shr 2);
  end;
 end;
 Result:=True;
end;

{ TfrmMain }

procedure TfrmMain.TextKeyDown(Sender:TObject;var Key:Word;Shift:TShiftState);
begin
 if (Key=13) then
 if Sender.InheritsFrom(TLabeledEdit) then
 if (TLabeledEdit(Sender).OnExit<>nil) then
 begin
  TLabeledEdit(Sender).OnExit(Sender);
 end;
end;

procedure TfrmMain.NidBase64Click(Sender: TObject);
var
 S:RawByteString;
 nid:QWORD;
begin
 S:=NidBase64.Text;
 nid:=0;
 if DecodeValue64(PAnsiChar(S),Length(S),nid) then
 begin
  NidHex.Text:=HexStr(nid,16);
  NidName.Text:=ps4libdoc.GetFunctName(nid);
 end else
 begin
  NidHex.Text:='';
  NidName.Text:='';
 end;
end;

procedure TfrmMain.NidHexClick(Sender: TObject);
var
 S:RawByteString;
 nid:QWORD;
begin
 S:='$'+NidHex.Text;
 nid:=0;
 if TryStrToQWord(S,nid) then
 begin
  NidBase64.Text:=EncodeValue64(nid);
  NidName.Text:=ps4libdoc.GetFunctName(nid);
 end else
 begin
  NidBase64.Text:='';
  NidName.Text:='';
 end;
end;

procedure TfrmMain.NidNameClick(Sender: TObject);
var
 S:RawByteString;
 nid:QWORD;
begin
 S:=NidName.Text;
 nid:=ps4_nid_hash(PChar(S));
 NidBase64.Text:=EncodeValue64(nid);
 NidHex.Text:=HexStr(nid,16);
end;

{ TfrmMain }

end.

