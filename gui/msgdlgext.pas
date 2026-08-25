unit MsgDlgExt;

{$mode ObjFPC}{$H+}

interface

uses
 Classes,
 SysUtils,
 Dialogs,
 Controls,
 StdCtrls,
 Forms,
 Graphics;

const
 MsgDlgBtnToStr: array[TMsgDlgBtn] of PChar = (
  '&Yes',
  '&No',
  '&OK',
  '&Cancel',
  '&Abort',
  '&Retry',
  '&Ignore',
  '&All',
  '&NoToAll',
  '&YesToAll',
  '&Help',
  '&Close'
 );

 mrNone    =Controls.mrNone    ;
 mrOK      =Controls.mrOK      ;
 mrCancel  =Controls.mrCancel  ;
 mrAbort   =Controls.mrAbort   ;
 mrRetry   =Controls.mrRetry   ;
 mrIgnore  =Controls.mrIgnore  ;
 mrYes     =Controls.mrYes     ;
 mrNo      =Controls.mrNo      ;
 mrAll     =Controls.mrAll     ;
 mrNoToAll =Controls.mrNoToAll ;
 mrYesToAll=Controls.mrYesToAll;
 mrClose   =Controls.mrClose   ;

 MsgDlgBtnToResult: array[TMsgDlgBtn] of Byte = (
  mrYes,
  mrNo,
  mrOK,
  mrCancel,
  mrAbort,
  mrRetry,
  mrIgnore,
  mrAll,
  mrNoToAll,
  mrYesToAll,
  mrNone, //Help
  mrClose
 );

type
 TMsgDlgAButtons=array of TMsgDlgBtn;

function GetRealFontSize(Font:TFont):Integer;

function MessageDlgEx(const AMsg:RawByteString;
                      const ACaption:RawByteString;
                      AButtons:TMsgDlgAButtons;
                      AParent:TForm):TModalResult;

implementation

function GetRealFontSize(Font:TFont):Integer;
var
 fd: TFontData;
begin
 fd := Graphics.GetFontData(Font.Handle);
 Result := ((-fd.Height) * 72) div Font.PixelsPerInch;
end;

function MessageDlgEx(const AMsg:RawByteString;
                      const ACaption:RawByteString;
                      AButtons:TMsgDlgAButtons;
                      AParent:TForm):TModalResult;
var
 MsgForm:TForm;
 MsgMemo:TMemo;
 MsgBtnz:TButton;

 //(asrTop, asrBottom, asrCenter);
 Procedure NewBtn(DlgType:TMsgDlgBtn;DlgPos:TAnchorSideReference);
 begin
  MsgBtnz:=TButton.Create(MsgForm);

  case DlgPos of
   asrTop:
     begin
      MsgBtnz.Anchors:=[akLeft,akBottom];
      MsgBtnz.AnchorSide[akLeft  ].Control:=MsgForm;
      MsgBtnz.AnchorSide[akLeft  ].Side   :=asrTop;
      MsgBtnz.AnchorSide[akBottom].Control:=MsgForm;
      MsgBtnz.AnchorSide[akBottom].Side   :=asrBottom;
     end;
   asrBottom:
     begin
      MsgBtnz.Anchors:=[akRight,akBottom];
      MsgBtnz.AnchorSide[akRight ].Control:=MsgForm;
      MsgBtnz.AnchorSide[akRight ].Side   :=asrBottom;
      MsgBtnz.AnchorSide[akBottom].Control:=MsgForm;
      MsgBtnz.AnchorSide[akBottom].Side   :=asrBottom;
     end;
   asrCenter:
     begin
      MsgBtnz.Anchors:=[akLeft,akBottom];
      MsgBtnz.AnchorSide[akLeft  ].Control:=MsgForm;
      MsgBtnz.AnchorSide[akLeft  ].Side   :=asrCenter;
      MsgBtnz.AnchorSide[akBottom].Control:=MsgForm;
      MsgBtnz.AnchorSide[akBottom].Side   :=asrBottom;
     end;
  end;

  MsgBtnz.BorderSpacing.Around :=10;
  MsgBtnz.Constraints.MinHeight:=25;
  MsgBtnz.Constraints.MinWidth :=75;
  MsgBtnz.AutoSize   :=True;
  MsgBtnz.Caption    :=MsgDlgBtnToStr[DlgType];
  MsgBtnz.Parent     :=MsgForm;
  MsgBtnz.ModalResult:=MsgDlgBtnToResult[DlgType];
 end;

begin
 MsgBtnz:=nil;

 MsgForm:=TForm.Create(nil);
 try
  MsgForm.Caption    :=ACaption;
  MsgForm.Position   :=poDesigned;
  MsgForm.BorderIcons:=[biSystemMenu];
  MsgForm.FormStyle  :=fsSystemStayOnTop;
  MsgForm.Left:= AParent.Left + (AParent.Width  - MsgForm.Width ) div 2;
  MsgForm.Top := AParent.Top  + (AParent.Height - MsgForm.Height) div 2;
  MsgForm.Width :=400;
  MsgForm.Height:=200;
  //
  Case Length(AButtons) of
   0:;
   1:
     begin
      NewBtn(AButtons[0],asrTop);
     end;
   2:
     begin
      NewBtn(AButtons[0],asrTop);
      NewBtn(AButtons[1],asrBottom);
     end;
   3:
     begin
      NewBtn(AButtons[0],asrTop);
      NewBtn(AButtons[1],asrCenter);
      NewBtn(AButtons[2],asrBottom);
     end;
   else;
  end;
  //
  MsgMemo:=TMemo.Create(MsgForm);
  MsgMemo.ReadOnly:=True;
  MsgMemo.Font.Name:='Courier New';
  MsgMemo.Font.Size:=GetRealFontSize(AParent.Font) + 2;
  //
  MsgMemo.Anchors:=[akTop,akLeft,akRight,akBottom];
  MsgMemo.AnchorSide[akTop   ].Control:=MsgForm;
  MsgMemo.AnchorSide[akTop   ].Side   :=asrTop;
  MsgMemo.AnchorSide[akLeft  ].Control:=MsgForm;
  MsgMemo.AnchorSide[akLeft  ].Side   :=asrTop;
  MsgMemo.AnchorSide[akRight ].Control:=MsgForm;
  MsgMemo.AnchorSide[akRight ].Side   :=asrBottom;
  MsgMemo.AnchorSide[akBottom].Control:=MsgForm;
  MsgMemo.AnchorSide[akBottom].Side   :=asrBottom;
  if (MsgBtnz<>nil) then
  begin
   MsgMemo.AnchorSide[akBottom].Control:=MsgBtnz;
   MsgMemo.AnchorSide[akBottom].Side   :=asrTop;
  end;
  MsgMemo.BorderSpacing.Bottom:=10;
  //
  MsgMemo.Text  :=AMsg;
  MsgMemo.Parent:=MsgForm;
  //
  Result:=MsgForm.ShowModal;
 finally
  MsgForm.Free;
 end;

 {
 MsgFrm:=CreateMessageDialog(AMsg, ADlgType, AButtons);
 try
  MsgFrm.Position :=poDefaultSizeOnly;
  MsgFrm.FormStyle:=fsSystemStayOnTop;
  MsgFrm.Left:= AParent.Left + (AParent.Width  - MsgFrm.Width ) div 2;
  MsgFrm.Top := AParent.Top  + (AParent.Height - MsgFrm.Height) div 2;
  Result:=MsgFrm.ShowModal;
 finally
  MsgFrm.Free
 end;
 }
end;

end.

