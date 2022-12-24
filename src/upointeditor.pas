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

{* TPointEditor }
unit UPointEditor;

interface

uses
 Classes,LCLType, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,ExtCtrls,Spin, // Lazarus units
 CastleVectors;                                                                                     // CGE units
type
{*Результат вызова окна редактирования}
TPointEditorResult=(peCancel, //*< нажата кнопка "Cancel"
                    peDelete, //*< нажата кнопка "Delete"
                    peSave);  //*< нажата кнопка "Save"
{*Класс для окна редактирования конкретной точки (вершины)
@link(TCastleLine2DBase.Points TCastleLine2DBase.Points[i]) из редактора.
Окно редактирования вызывается путем нажатия средней кнопкой мыши по
выбранной точке в редакторе CGE.
}
TPointEditor=class(TObject)
  private
   Form:TForm;
   PanelTop:TPanel;
   PanelX:TPanel;
   PanelY:TPanel;
   PanelBottom:TPanel;
   BtnDel:TButton;
   BtnClose:TButton;
   BtnSave:TButton;
   FloatSpinEditX:TFloatSpinEdit;
   FloatSpinEditY:TFloatSpinEdit;
   LabelPoint:TLabel;
   LabelX:TLabel;
   LabelY:TLabel;

   FExecute:TPointEditorResult;
   FResultVector:TVector2;
   procedure BtnDelClick(Sender: TObject);
   procedure BtnSaveClick(Sender: TObject);
   procedure BtnCloseClick(Sender: TObject);
   procedure FloatSpinEditXKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
   procedure FloatSpinEditYKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
   constructor Create;
   destructor Destroy; override;
   {*
    Вызвать окно редактирования.
    index - для отображения номера точки.
    vector - точка.
   }
   function ShowModal(const index:integer;const vector:TVector2):TPointEditorResult;
   {* Точка, полученная в результате редактирования}
   property ResultVector:TVector2 read FResultVector;
   {* Результат вызова}
   property Execute:TPointEditorResult read FExecute;

end;
implementation

constructor TPointEditor.Create;
begin
  Form:=TForm.Create(nil);
   with Form do begin
        Position:=poScreenCenter;
        Caption:='Point editor';
        BorderStyle:=bsDialog;
        AutoSize:=true;
   end;
   PanelTop:=TPanel.Create(Form);
   with PanelTop do begin
        Parent:=Form;
        top:=0;
        Align:=alTop;
        BevelOuter:=bvNone;
        AutoSize:=true;
   end;
   LabelPoint:=TLabel.Create(PanelTop);
   with LabelPoint do begin
         Parent:=PanelTop;
         Caption:='Point';
         Align:=alClient;
         BorderSpacing.Around:=10;
         Alignment:=taCenter;
         AutoSize:=true;
   end;
   PanelX:=TPanel.Create(Form);
   with PanelX do begin
        Parent:=Form;
        top:=1;
        Align:=alTop;
        BevelOuter:=bvNone;
        AutoSize:=true;
   end;
   LabelX:=TLabel.Create(PanelX);
   with LabelX do begin
         Parent:=PanelX;
         Caption:='X:';
         Align:=alLeft;
         BorderSpacing.Around:=10;
         Alignment:=taCenter;
         AutoSize:=true;
         Layout:=tlCenter;
   end;
   FloatSpinEditX:=TFloatSpinEdit.Create(PanelX);
   with FloatSpinEditX do begin
         Parent:=PanelX;
         Align:=alClient;
         BorderSpacing.Around:=10;
         OnKeyDown:={$ifdef FPC}@{$endif}FloatSpinEditXKeyDown;
   end;
   PanelY:=TPanel.Create(Form);
   with PanelY do begin
        Parent:=Form;
        top:=2;
        Align:=alTop;
        BevelOuter:=bvNone;
        AutoSize:=true;
   end;
   LabelY:=TLabel.Create(PanelY);
   with LabelY do begin
         Parent:=PanelY;
         Caption:='Y:';
         Align:=alLeft;
         BorderSpacing.Around:=10;
         Alignment:=taCenter;
         AutoSize:=true;
         Layout:=tlCenter;
   end;
   FloatSpinEditY:=TFloatSpinEdit.Create(PanelY);
   with FloatSpinEditY do begin
         Parent:=PanelY;
         Align:=alClient;
         BorderSpacing.Around:=10;
         OnKeyDown:={$ifdef FPC}@{$endif}FloatSpinEditYKeyDown;
   end;
   PanelBottom:=TPanel.Create(Form);
   with PanelBottom do begin
        Parent:=Form;
        top:=3;
        Align:=alTop;
        BevelOuter:=bvNone;
        AutoSize:=true;
   end;
   BtnClose:=TButton.Create(PanelBottom);
   with BtnClose do begin
        Parent:=PanelBottom;
        left:=2;
        Caption:=' Close ';
        AutoSize:=true;
        Align:=alRight;
        BorderSpacing.Around:=10;
        OnClick:={$ifdef FPC}@{$endif}BtnCloseClick;
   end;
   BtnSave:=TButton.Create(PanelBottom);
   with BtnSave do begin
        Parent:=PanelBottom;
        left:=1;
        Caption:=' Save ';
        AutoSize:=true;
        Align:=alRight;
        BorderSpacing.Around:=10;
        OnClick:={$ifdef FPC}@{$endif}BtnSaveClick;
   end;
   BtnDel:=TButton.Create(PanelBottom);
   with BtnDel do begin
        Parent:=PanelBottom;
        left:=0;
        Caption:=' Delete ';
        AutoSize:=true;
        Align:=alRight;
        BorderSpacing.Around:=10;
        BorderSpacing.Right:=100;
        OnClick:={$ifdef FPC}@{$endif}BtnDelClick;

   end;

   FExecute:=peCancel;
   FResultVector:=Vector2(0,0);

end;

destructor TPointEditor.Destroy;
begin
  BtnDel.Destroy;
  BtnClose.Destroy;
  BtnSave.Destroy;
  FloatSpinEditX.Destroy;
  FloatSpinEditY.Destroy;
  LabelPoint.Destroy;
  LabelX.Destroy;
  LabelY.Destroy;
  PanelTop.Destroy;
  PanelX.Destroy;
  PanelY.Destroy;
  PanelBottom.Destroy;
  Form.Destroy;
  inherited;
end;

function TPointEditor.ShowModal(const index:integer;const vector:TVector2):TPointEditorResult;
begin
  FResultVector:=vector;
  FExecute:=peCancel;
  FloatSpinEditX.Value:=vector.X;
  FloatSpinEditY.Value:=vector.Y;
  LabelPoint.Caption:='Point '+inttostr(index);
  Form.ShowModal;
  FloatSpinEditX.SetFocus;
  Result:=FExecute;
end;

procedure TPointEditor.BtnDelClick(Sender: TObject);
begin
 FExecute:=peDelete;
 Form.Close;
end;

procedure TPointEditor.BtnSaveClick(Sender: TObject);
begin
  FExecute:=peSave;
  FResultVector:=Vector2(FloatSpinEditX.Value,FloatSpinEditY.Value);
  Form.Close;
end;

procedure TPointEditor.BtnCloseClick(Sender: TObject);
begin
  FExecute:=peCancel;
  Form.Close;
end;

procedure TPointEditor.FloatSpinEditXKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then BtnClose.Click;
  if (Key=VK_RETURN) or (key=VK_DOWN) then FloatSpinEditY.SetFocus;

end;

procedure TPointEditor.FloatSpinEditYKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then BtnClose.Click;
  if key=VK_UP then FloatSpinEditX.SetFocus;
  if Key=VK_RETURN then BtnSave.Click;

end;



end.

