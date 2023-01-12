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
{*TCastleLine2DBase}
unit CastleLine2DBase;


interface

uses
   Classes,csvreadwrite,                        // Lazarus units
   CastleScene,CastleVectors,CastleClassUtils,  // CGE units
   CastleLine2DMath;                            // CastleLine2D units
type
  {*Тип линии}
  TCastleLineType=(ltOpen,  //*< обычная линия
                   ltClose);//*< замкнутая линия
  {*
  Базовый класс для фигур, которые определяются набором вершин @link(TCastleLine2DBase.Points Points).
  @bold(Потомки класса должны переопределить метод ReLoad.)
  Для набора вершин Points предусмотрен редактор свойств
  Также возможно редактировать набор вершин непосредственно из окна редактора CGE.
  Для входа в режим редактирования используйте "verb" (Edit mode ON) -
  правой кнопкой мыши по объекту в окне иерархии объектов.
  }
  TCastleLine2DBase = class (TCastleAbstractPrimitive)
  private
    {точки - вершины линии}
    FPoints:TVector2List;
    {Тип линии}
    FLineType:TCastleLineType;
    {Запущен ли процесс сериализации}
    IsSerialization:boolean;
    {Включен ли режим редактирования}
    FEditMode:boolean;
    {привести линиую к соответствие типу}
    procedure CheckLineType;
  protected
    {*Установить тип линии}
    procedure SetLineType(NewLineType:TCastleLineType); virtual;
  public
    {*Удалить все точки}
    procedure Clear; virtual;
    {Для сохранения в редакторе}
    procedure CustomSerialization(const SerializationProcess: TSerializationProcess); override;
    {*Построить геометрию заново @bold(Потомки класса должны переопределить этот метод)}
    procedure ReLoad; virtual;
    {*Сохранить точки @link(TCastleLine2DBase.Points Points) в файл}
    procedure SavePointsToFile(const FileName:string);
    {*Загрузить точки @link(TCastleLine2DBase.Points Points) из файла}
    procedure LoadPointsFromFile(const FileName:string);
    {*
       Проверка на самопересечение.
    }
    function SelfIntersections:boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {Разделы свойств}
    function PropertySections(const PropertyName: String): TPropertySections; override;
    {*Включен ли режим редактирования}
    property EditMode:boolean read FEditMode write FEditMode;
  published
    {*Тип линии (обычная линия, замкнутая линия)}
    property LineType:TCastleLineType read FLineType write SetLineType default ltOpen;
    {*Точки (вершины) на основе которых потомки выстраивают геометрию}
    property Points:TVector2List read FPoints write FPoints;

  end;


implementation

uses
   {$ifdef CASTLE_DESIGN_MODE}
    Dialogs,PropEdits, ComponentEditors,  // Lazarus units
    CastlePropEdits, CastleLog,           // CGE units
    CastleLine2DGizmos,UPointsEditor,     // CastleLine2D units
   {$endif}
   SysUtils;

{constructor}
constructor TCastleLine2DBase.Create(AOwner: TComponent);
begin
  inherited;
  Self.Material:=pmUnlit;
  EditMode:=false;
  IsSerialization:=false;
  FPoints:=TVector2List.Create;
  FLineType:=ltOpen;
end;

{destructor}
destructor TCastleLine2DBase.Destroy;
begin
 {$ifdef CASTLE_DESIGN_MODE}
 Line2DGizmos.Hide;
 {$endif}
 FPoints.Free;
 inherited;
end;

{нарисовать заново}
procedure TCastleLine2DBase.ReLoad;
begin
 if IsSerialization then Exit;
 CheckLineType;
end;

{удалить все точки}
procedure TCastleLine2DBase.Clear;
begin
  FPoints.Clear;
end;

{Установить тип линии}
procedure TCastleLine2DBase.SetLineType(NewLineType:TCastleLineType);
begin
 if FLineType=NewLineType then Exit;
 FLineType:=NewLineType;
end;


{привести линиую к соответствие типу}
procedure TCastleLine2DBase.CheckLineType;
begin
 if FPoints.Count<3 then Exit;
 if FLineType=ltOpen then Exit;
 if (FPoints.Count=3)and TVector2.Equals(FPoints[0],FPoints[2]) then begin
  FPoints.Delete(2);
  {$ifdef CASTLE_DESIGN_MODE}
   Line2DGizmos.ReLoad;
  {$endif}
  Exit;
 end;
 if (FPoints.Count>3)and(TVector2.Equals(FPoints[0],FPoints[FPoints.Count-1])=false) then begin
  FPoints[0]:=FPoints[FPoints.Count-1];
  {$ifdef CASTLE_DESIGN_MODE}
   Line2DGizmos.ReLoad;
  {$endif}
  Exit;
 end;
end;

{сохранить точки в файл}
procedure TCastleLine2DBase.SavePointsToFile(const FileName:string);
var F:Text;
    i:integer;
begin
 try
  AssignFile(F,FileName);
  Rewrite(F);
  for i:=0 to FPoints.Count-1 do begin
    Writeln(F,FPoints[i].X.ToString+';'+FPoints[i].Y.ToString);
  end;
 finally
   Close(F);
 end;
end;

{загрузить точки из файла}
procedure TCastleLine2DBase.LoadPointsFromFile(const FileName:string);
var
  Parser: TCSVParser;
  Stream: TFileStream;
  s:string;
  row:integer;
  v:TVector2;
begin
  Parser := TCSVParser.Create;
  try
   stream := TFileStream.Create(FileName, fmOpenRead);
   Parser.Delimiter := ';';
   Parser.DetectBOM := true;
   parser.SetSource(stream);
   row:=0;
   Clear;
   while parser.ParseNextCell do begin
     if parser.CurrentRow>row then begin
       FPoints.Add(v);
       row:=parser.CurrentRow;
     end;
     s := parser.CurrentCellText;
     case parser.CurrentCol of
       0: v.X:=DoFloat(s);
       1: v.Y:=DoFloat(s);
     end;
   end;
   // добавить последнюю строку
   FPoints.Add(v);
   CheckLineType;
  finally
    stream.Free;
    parser.Free;
  end;

end;

function TCastleLine2DBase.SelfIntersections:boolean;
var i,j:integer;
    Cross:TVector2;
begin
  // проверка на самопересечение
  // на пересечение надо надо проверять не каждый отрезок, а через один
  Result:=false;
  for i:=0 to Points.Count-3 do begin
   for j:=i+2 to Points.Count-3 do begin
     if CrossingSegments(Points[i],Points[i+1],Points[j],Points[j+1],Cross)=1 then Exit(true);
   end;
  end;
  // проверка последнего отрезка
  for j:=1 to Points.Count-4 do begin
    if CrossingSegments(Points[Points.Count-2],Points[Points.Count-1],Points[j],Points[j+1],Cross)=1 then Exit(true);
  end;
end;



{для сохранения в редакторе}
procedure TCastleLine2DBase.CustomSerialization(const SerializationProcess: TSerializationProcess);
const eps = 0.000001;
var CountPoints,i:integer;
    Point:TVector2;
begin
  IsSerialization:=true;
  inherited;
  CountPoints:=FPoints.Count;
  SerializationProcess.ReadWriteInteger('CountPoints',CountPoints,true);
  FPoints.Count:=CountPoints;
  for i:=0 to CountPoints-1 do begin
    Point:= FPoints[i];
    SerializationProcess.ReadWriteVector('Points'+inttostr(i), Point,vector2(eps,eps), true);
    FPoints[i]:=Point;
  end;
  IsSerialization:=false;
  ReLoad;
end;

{разделы свойств}
function TCastleLine2DBase.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'LineType') or
     (PropertyName = 'Points')
  then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;



{-------------------------TCastleLine2DBaseEditor-------------------------------}
{$ifdef CASTLE_DESIGN_MODE}

type
  { Editor for TCastleLine2DBase }
  TCastleLine2DBaseEditor = class(TComponentEditor)
  public

    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  {Editor for Points property}
  TCastleLine2DBaseEditorPointsEditor = class(TClassPropertyEditor)
  public
   procedure Edit; Override;
   function  GetAttributes: TPropertyAttributes; Override;
  end;

{------------------------TCastleLine2DBaseEditor-------------------------------}

function TCastleLine2DBaseEditor.GetVerbCount: Integer;
begin
  Result := (inherited GetVerbCount) + 1;
end;

function TCastleLine2DBaseEditor.GetVerb(Index: Integer): string;
var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    Result := inherited GetVerb(Index)
  else
  if Index = InheritedCount then
  begin
   if (Component as TCastleLine2DBase).EditMode then Result := 'Edit mode OFF' else Result := 'Edit mode ON';
  end else
    Result := '';
end;

procedure TCastleLine2DBaseEditor.ExecuteVerb(Index: Integer);
var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    inherited ExecuteVerb(Index)
  else
  if Index = InheritedCount then
  begin
     if (Component as TCastleLine2DBase).EditMode then begin
      Line2DGizmos.Hide;
     end else begin
       Line2DGizmos.Show((Component as TCastleLine2DBase));
     end;
  end else
    WritelnWarning('TCastleLine2DBaseEditor.ExecuteVerb invalid verb index: %d', [Index]);
end;

{---------------TCastleLine2DBaseEditorPointsEditor-----------------------------}

procedure TCastleLine2DBaseEditorPointsEditor.Edit;
var PE:TPointsEditor;
    SaveEditMode:boolean;
begin
  PE:=TPointsEditor.Create((GetComponent(0) as TCastleLine2DBase));
  SaveEditMode:=(GetComponent(0) as TCastleLine2DBase).EditMode;
  Line2DGizmos.Show((GetComponent(0) as TCastleLine2DBase));
  PE.ShowModal;
  PE.Free;
  if not SaveEditMode then Line2DGizmos.Hide;
end;

function TCastleLine2DBaseEditorPointsEditor.GetAttributes: TPropertyAttributes;
begin
 Result := [paDialog];
end;

{$endif}


initialization
  {$ifdef CASTLE_DESIGN_MODE}
  RegisterComponentEditor(TCastleLine2DBase, TCastleLine2DBaseEditor);
  RegisterPropertyEditor(TypeInfo(TVector2List),TCastleLine2DBase,'Points',TCastleLine2DBaseEditorPointsEditor);
  {$endif}

end.

