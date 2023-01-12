{
 Copyright 2022 Korchazhkin Sergey (KumurTash@gmail.com)
 This file is part of "Castle Lines 2D".

 "Castle Lines 2D" is a set of components for the "Castle Game Engine" - https://castle-engine.io/
 "Castle Lines 2D" is free software; see the file COPYING.txt,
 included in this distribution, for details about the copyright.

 "Castle Lines 2D" is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 ----------------------------------------------------------------------------
 }
{* TPointsEditor }
unit UPointsEditor;

interface

uses
  Classes,LCLType, SysUtils, Forms, Grids, Controls, StdCtrls, ExtCtrls,Dialogs,Menus, // Lazarus units
  CastleVectors,                                                                       // CGE units
  CastleLine2DBase,CastleLine2DGizmos,CastleLine2DMath;                                // CastleLine2D units

type
  {*
   TStringGrid дополненный событием которое возникает после вставки из буфера обмена.
   Используется в @link(TPointsEditor)
  }
  TStringGridWithOnPasteFromClipboard = class(TStringGrid)
    private
      FOnPasteFromClipboard:TNotifyEvent;
    protected
      procedure DoPasteFromClipboard; override;
    public
      {*Событие возникает после вставки из буфера обмена}
      property OnPasteFromClipboard: TNotifyEvent read FOnPasteFromClipboard write FOnPasteFromClipboard;
  end;

  {*
   Класс содержит методы для создания и вызова окна редактирования
   набора точек (вершин) @link(TCastleLine2DBase.Points) в инспекторе объектов.
  }
 TPointsEditor=class(TObject)
  private
   Form:TForm;
   PanelTop:TPanel;
   BtnAdd:TButton;
   PanelBottom:TPanel;
   BtnClose:TButton;
   BtnSave:TButton;
   StringGrid: TStringGridWithOnPasteFromClipboard;
   SaveDialog:TSaveDialog;
   OpenDialog:TOpenDialog;
   BtnSaveToFile:TButton;
   BtnLoadFromFile:TButton;
   PopupMenu: TPopupMenu;
   MenuItemCopy: TMenuItem;
   MenuItemPaste: TMenuItem;


   IsSave:boolean;
   {для реализации кнопки Отмена}
   OldPoints:TVector2List;
   {Обьект свойство которго редактируются. Передается в конструкторе}
   FLine:TCastleLine2DBase;
   {Происходит после вствки из буфера обменя}
   procedure StringGridPaste(Sender: TObject);
   procedure StringGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: AnsiString);
   procedure StringGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

   procedure BtnDelClick(Sender: TObject; aCol, aRow: Integer);
   procedure BtnAddClick(Sender: TObject);
   procedure BtnCloseClick(Sender: TObject);
   procedure BtnSaveClick(Sender: TObject);
   procedure BtnSaveToFileClick(Sender: TObject);
   procedure BtnLoadFromFileClick(Sender: TObject);
   procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
   procedure MenuItemCopyClick(Sender: TObject);
   procedure MenuItemPasteClick(Sender: TObject);
  public
   constructor Create(ALine:TCastleLine2DBase);
   destructor Destroy; override;
   {*Вызвать окно редактирования}
   procedure ShowModal;
   {*Перезагрузить таблицу}
   procedure ReLoad;

end;

implementation

{---------------TStringGridWithOnPasteFromClipboard-----------------------------}
procedure TStringGridWithOnPasteFromClipboard.DoPasteFromClipboard;
begin
  inherited;
  if FOnPasteFromClipboard<>nil then FOnPasteFromClipboard(Self);
end;

{---------------------------TPointsEditor---------------------------------------}

destructor TPointsEditor.Destroy;
begin
  OldPoints.Free;

  MenuItemCopy.Free;
  MenuItemPaste.Free;
  PopupMenu.Free;
  BtnSave.Free;
  BtnLoadFromFile.Free;
  BtnSaveToFile.Free;
  OpenDialog.Free;
  SaveDialog.Free;
  StringGrid.Free;
  BtnAdd.Free;
  BtnClose.Free;
  PanelTop.Free;
  PanelBottom.Free;
  Form.Free;
  inherited;
end;

constructor TPointsEditor.Create(ALine:TCastleLine2DBase);
begin
  inherited Create;
  FLine:=ALine;
  OldPoints:=TVector2List.Create;
  IsSave:=false;
  Form:=TForm.Create(nil);
  with Form do begin
       Width:=600;
       Height:=500;
       Position:=poScreenCenter;
       Caption:='Points editor';
       BorderStyle:=bsDialog;
       OnClose:={$ifdef FPC}@{$endif}FormClose;
  end;
  PanelTop:=TPanel.Create(Form);
  with PanelTop do begin
       Parent:=Form;
       Align:=alTop;
       BevelOuter:=bvNone;
       AutoSize:=true;
  end;
  BtnAdd:=TButton.Create(PanelTop);
  with BtnAdd do begin
       Parent:=PanelTop;
       Caption:='Add';
       Width:=Width*2;
       Align:=alLeft;
       BorderSpacing.Around:=10;
       OnClick:={$ifdef FPC}@{$endif}BtnAddClick;
  end;
  PanelBottom:=TPanel.Create(Form);
  with PanelBottom do begin
       Parent:=Form;
       Align:=alBottom;
       BevelOuter:=bvNone;
       AutoSize:=true;
  end;
  BtnClose:=TButton.Create(PanelBottom);
  with BtnClose do begin
       Parent:=PanelBottom;
       Caption:='Cancel';
       Width:=Width*2;
       Align:=alRight;
       BorderSpacing.Around:=10;
       OnClick:={$ifdef FPC}@{$endif}BtnCloseClick;
  end;
  BtnSave:=TButton.Create(PanelBottom);
  with BtnSave do begin
       Parent:=PanelBottom;
       Caption:='Ok';
       Width:=Width*2;
       Align:=alRight;
       BorderSpacing.Around:=10;
       OnClick:={$ifdef FPC}@{$endif}BtnSaveClick;
  end;
  StringGrid:=TStringGridWithOnPasteFromClipboard.Create(Form);
  with StringGrid do begin
       Parent:=Form;
       Align:=alClient;
       Options:=Options+[goEditing];
       StringGrid.Columns.Add;
       StringGrid.Columns.Add;
       StringGrid.Columns.Add;
       StringGrid.Columns.Items[2].ButtonStyle:=cbsButtonColumn;
       StringGrid.OnSetEditText:={$ifdef FPC}@{$endif}StringGridSetEditText;
       StringGrid.OnPasteFromClipboard:={$ifdef FPC}@{$endif}StringGridPaste;
       StringGrid.OnButtonClick:={$ifdef FPC}@{$endif}BtnDelClick;
       StringGrid.OnKeyDown:={$ifdef FPC}@{$endif}StringGridKeyDown;
  end;
  SaveDialog:=TSaveDialog.Create(Form);
  with SaveDialog do begin
       Filter:='csv files|*.csv;*.CSV|All files|*';
       FileName:='CastleLine.csv';
  end;
  BtnSaveToFile:=TButton.Create(PanelTop);
  with BtnSaveToFile do begin
    Parent:=PanelTop;
    Caption:='Save file';
    Width:=Width*2;
    Align:=alRight;
    BorderSpacing.Around:=10;
    OnClick:={$ifdef FPC}@{$endif}BtnSaveToFileClick;
  end;
  OpenDialog:=TOpenDialog.Create(Form);
  with OpenDialog do begin
      Filter:='csv files|*.csv;*.CSV|All files|*';
  end;

  BtnLoadFromFile:=TButton.Create(PanelTop);
  with BtnLoadFromFile do begin
    Parent:=PanelTop;
    Caption:='Load file';
    Width:=Width*2;
    Align:=alRight;
    BorderSpacing.Around:=10;
    OnClick:={$ifdef FPC}@{$endif}BtnLoadFromFileClick;
  end;

  PopupMenu:= TPopupMenu.Create(StringGrid);
  StringGrid.PopupMenu:=PopupMenu;
  MenuItemCopy:=TMenuItem.Create(PopupMenu);
  with MenuItemCopy do begin
    Caption:='Copy (Ctrl-C)';
    OnClick:={$ifdef FPC}@{$endif}MenuItemCopyClick;
  end;
  MenuItemPaste:=TMenuItem.Create(PopupMenu);
  with MenuItemPaste do begin
    Caption:='Paste (Ctrl-V)';
    OnClick:={$ifdef FPC}@{$endif}MenuItemPasteClick;
  end;
  PopupMenu.Items.Add(MenuItemCopy);
  PopupMenu.Items.Add(MenuItemPaste);




end;

procedure TPointsEditor.ReLoad;
var i,temp:integer;
    //для подгонки ширины столбцов
    Width0,Width1,Width2,Width3:integer;
begin
    // отчистка формы
    StringGrid.Clear;
    // заполняем заново
    Width0:=14;
    Width1:=160;
    Width2:=160;
    Width3:=40;
    StringGrid.RowCount:=FLine.Points.Count+1;
    StringGrid.Columns.Items[0].Title.Caption:='X';
    StringGrid.Columns.Items[1].Title.Caption:='Y';
    StringGrid.Columns.Items[2].Title.Caption:='';

    for i:=0 to FLine.Points.Count-1 do begin
      StringGrid.Cells[0,i+1]:=i.ToString;
      temp := StringGrid.Canvas.TextWidth(StringGrid.Cells[0,i+1]);
      if temp>Width0 then Width0:=temp;
      StringGrid.Cells[1,i+1]:=FLine.Points[i].X.ToString;
      temp := StringGrid.Canvas.TextWidth(StringGrid.Cells[1,i+1]);
      if temp>Width1 then Width1:=temp;
      StringGrid.Cells[2,i+1]:=FLine.Points[i].Y.ToString;
      temp := StringGrid.Canvas.TextWidth(StringGrid.Cells[2,i+1]);
      if temp>Width2 then Width2:=temp;
      StringGrid.Cells[3,i+1]:='Delete';
      temp := StringGrid.Canvas.TextWidth(StringGrid.Cells[3,i+1]);
      if temp>Width3 then Width3:=temp;
    end;

    StringGrid.ColWidths[0] := Width0 + StringGrid.GridLineWidth + 10;
    StringGrid.ColWidths[1] := Width1 + StringGrid.GridLineWidth + 10;
    StringGrid.ColWidths[2] := Width2 + StringGrid.GridLineWidth + 10;
    StringGrid.ColWidths[3] := Width3 + StringGrid.GridLineWidth + 20;

end;

procedure TPointsEditor.ShowModal;
begin
  OldPoints.Clear;
  OldPoints.AddRange(FLine.Points);
  ReLoad;
  form.ShowModal;
end;

procedure TPointsEditor.StringGridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: AnsiString);
var s:string;
    i:integer;
begin
  if (ACol<>1)and(ACol<>2) then Exit;
  if (ARow=1) and (FLine.LineType=ltClose)and(FLine.Points.Count>3) then i:=FLine.Points.Count-1 else i:=ARow-1;
  s:=StringGrid.Cells[ACol,ARow];
  if ACol=1 then FLine.Points[i]:=Vector2(DoFloat(s,'-'),FLine.Points[ARow-1].Y);
  if ACol=2 then FLine.Points[i]:=Vector2(FLine.Points[i].X,DoFloat(s,'-'));
  StringGrid.Cells[ACol,ARow]:=s;

  if (FLine.LineType=ltClose)and(FLine.Points.Count>3)  then begin
   if ARow=1 then StringGrid.Cells[ACol,StringGrid.RowCount-1]:=s;
   if ARow=StringGrid.RowCount-1 then StringGrid.Cells[ACol,1]:=s;
  end;

  if (FLine.LineType=ltClose)and(FLine.Points.Count=3) and (TVector2.Equals(FLine.Points[0],FLine.Points[2])) then begin  // что бы избежать ошибок
   if ACol=1 then StringGrid.Cells[ACol,ARow]:=(FLine.Points[2].X+1).ToString;
   if ACol=2 then StringGrid.Cells[ACol,ARow]:=(FLine.Points[2].Y+1).ToString;
  end;
  FLine.ReLoad;
  Line2DGizmos.ReLoad;

end;

procedure TPointsEditor.StringGridPaste(Sender: TObject);
var i:integer;
    s1,s2:string;
    v:TVector2;
begin
  if (FLine.LineType=ltClose)and(FLine.Points.Count>3)  then begin
    StringGrid.Cells[1,1]:=StringGrid.Cells[1,StringGrid.RowCount-1];
    StringGrid.Cells[2,1]:=StringGrid.Cells[2,StringGrid.RowCount-1];
  end;
  for i:=0 to FLine.Points.Count-1 do begin
     s1:=StringGrid.Cells[1,i+1];
     s2:=StringGrid.Cells[2,i+1];
     v:=Vector2(DoFloat(s1),DoFloat(s2));
     StringGrid.Cells[1,i+1]:=s1;
     StringGrid.Cells[2,i+1]:=s2;
     FLine.Points[i]:=v;
     StringGrid.Cells[3,i+1]:='Delete';
  end;
  FLine.ReLoad;
  Line2DGizmos.ReLoad;

end;

procedure TPointsEditor.StringGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if StringGrid.EditorMode=true then Exit;
  if Key=VK_ESCAPE then Form.Close;
end;



procedure TPointsEditor.BtnDelClick(Sender: TObject; aCol, aRow: Integer);
begin
 if FLine.Points.Count<=2 then Exit;
 FLine.Points.Delete(ARow-1);
 FLine.ReLoad;
 Line2DGizmos.ReLoad;
 Reload;
 StringGrid.Row:=ARow;
 StringGrid.Col:=1;
end;


procedure TPointsEditor.BtnAddClick(Sender: TObject);
begin
    if (FLine.LineType=ltClose)and(FLine.Points.Count=2) and (TVector2.Equals(FLine.Points[0],Vector2(0,0)))
    then FLine.Points.Add(Vector2(1,1))
    else FLine.Points.Add(Vector2(0,0));
    FLine.ReLoad;
    Line2DGizmos.ReLoad;
    Reload;
    StringGrid.Row:=StringGrid.RowCount-1;
    StringGrid.Col:=1;
end;


procedure TPointsEditor.BtnCloseClick(Sender: TObject);
begin
  Form.Close;
end;

procedure TPointsEditor.BtnSaveClick(Sender: TObject);
begin
 isSave:=true;
 Form.Close;
end;

procedure TPointsEditor.BtnSaveToFileClick(Sender: TObject);
begin
 if SaveDialog.Execute then begin
  if FileExists(SaveDialog.FileName) then begin
   if Application.MessageBox('Would you like to replace the existing file?', 'File already exists...', MB_ICONQUESTION + MB_YESNO)= IDYES then
    FLine.SavePointsToFile(SaveDialog.FileName);
  end else
    FLine.SavePointsToFile(SaveDialog.FileName);
 end;
end;

procedure TPointsEditor.BtnLoadFromFileClick(Sender: TObject);
begin
 if OpenDialog.Execute then begin
  FLine.LoadPointsFromFile(OpenDialog.FileName);
  FLine.ReLoad;
  Line2DGizmos.ReLoad;
  Reload;
 end;
end;

procedure TPointsEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
 if not IsSave then begin
  FLine.Clear;
  FLine.Points.AddRange(OldPoints);
  FLine.ReLoad;
  Line2DGizmos.ReLoad;
 end;
end;

procedure TPointsEditor.MenuItemCopyClick(Sender: TObject);
begin
  StringGrid.CopyToClipboard(true);
end;

procedure TPointsEditor.MenuItemPasteClick(Sender: TObject);
begin
 StringGrid.DoPasteFromClipboard;
end;

end.

