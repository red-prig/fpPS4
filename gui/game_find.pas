unit game_find;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type

  { TFrmFind }

  TFrmFind = class(TForm)
    BtnPrev: TButton;
    BtnNext: TButton;
    EdtFind: TEdit;
    Label1: TLabel;
    procedure BtnNextClick(Sender: TObject);
    procedure BtnPrevClick(Sender: TObject);
    procedure EdtFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public
   ListGrid: TStringGrid;
   procedure FindStr(const str:RawByteString;is_next:Boolean);
  end;

var
  FrmFind: TFrmFind;

implementation

uses
 LCLType;

{$R *.lfm}

{ TFrmFind }

procedure TFrmFind.FindStr(const str:RawByteString;is_next:Boolean);
var
 aRow,aCol,ColCount,RowCount:Integer;
 UStr:UnicodeString;
 UVal:UnicodeString;
begin
 if (str='') then Exit;

 aRow    :=ListGrid.Row;
 ColCount:=ListGrid.ColCount;
 RowCount:=ListGrid.RowCount;

 if (RowCount<=1) then Exit;
 if (ColCount =0) then Exit;

 if (aRow<=0) then aRow:=1;

 UStr:=UnicodeLowerCase(UnicodeString(str));

 while (True) do
 begin
  //
  case is_next of
   True :Inc(aRow);
   False:Dec(aRow);
  end;
  //
  case is_next of
   True :if (aRow>=RowCount) then Break;
   False:if (aRow<=0)        then Break;
  end;
  //

  For aCol:=0 to ColCount-1 do
  begin
   UVal:=UnicodeLowerCase(UnicodeString(ListGrid.Cells[aCol,aRow]));

   if (Pos(UStr,UVal)<>0) then
   begin

    if (aRow)>(ListGrid.TopRow + ListGrid.VisibleRowCount) then
    begin
     ListGrid.TopRow:=ListGrid.VisibleRowCount - aRow;
    end else
    if (aRow)<(ListGrid.TopRow) then
    begin
     ListGrid.TopRow:=aRow;
    end;

    ListGrid.Row:=aRow;

    Exit;
   end;
  end;

 end;

 ShowMessage('Not found!');
end;

procedure TFrmFind.BtnNextClick(Sender: TObject);
begin
 FindStr(Trim(EdtFind.Text),True);
end;

procedure TFrmFind.BtnPrevClick(Sender: TObject);
begin
 FindStr(Trim(EdtFind.Text),False);
end;

procedure TFrmFind.EdtFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if (Key=VK_RETURN) then
 begin
  BtnNextClick(Sender);
 end;
end;

end.

