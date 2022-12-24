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
{*TCastlePolygon2D}
unit CastlePolygon2D;
interface

uses
  Classes,                                                   // Lazarus units
  X3DNodes,CastleTriangulate,CastleVectors,CastleClassUtils, // CGE units
  CastleLine2DBase,CastleLine2DMath;                         // CastleLine2D units

type
  {*
   Потенциально невыпуклый многоугольник.
   Строится по набору вершин методом триангуляции. @br

   Для входа в режим редактирования используйте "verb" (Edit mode ON) -
   правой кнопкой мыши по объекту в окне иерархии объектов.
  }
  TCastlePolygon2D = class (TCastleLine2DBase)
   private
     {Треугольники для рисования}
     TriangleSet2DNode:TTriangleSet2DNode;
     {для триангуляции}
     Triangles:TVector2List;
     {Для определения нужно ли перестраивать сцену при изменении Scale}
     SignScale:Single;
     {для триангуляции многоугольника для заполнения}
     procedure  AddFill(const Tri: TVector3Integer);
     {для триангуляции многоугольника для заполнения}
     function GetPoint(Index: integer): TVector3;
   protected
     {*Для определения нужно ли перестраивать сцену при изменении Scale}
     procedure ChangedTransform; override;
     {*Тип линии может быть только ltClose (замкнутая линия)}
     procedure SetLineType(NewLineType:TCastleLineType); override;
   public
     procedure ReLoad override;
     {*
      При создании в редакторе CGE добавляются точки, образующие звезду.
      При создании непосредственно в коде (RunTime) точки не добавляются.
     }
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure Clear; override;
     {*
        Проверка на самопересечение.
        При самопересечении многоугольник не отображается.
     }
     function SelfIntersections:boolean;
     {Разделы свойств}
     function PropertySections(const PropertyName: String): TPropertySections; override;
   published
     property LineType;


  end;

implementation

uses
  {$ifdef CASTLE_DESIGN_MODE}
   PropEdits,
  {$endif}
  SysUtils,CastleComponentSerialize;

constructor TCastlePolygon2D.Create(AOwner: TComponent);
begin
  inherited;
  SignScale:=1;
  LineType:=ltClose;
  Triangles:=TVector2List.Create;
  TriangleSet2DNode:=TTriangleSet2DNode.Create;

  {$ifdef CASTLE_DESIGN_MODE}
  Points.Add(Vector2(0,80));
  Points.Add(Vector2(25,35));
  Points.Add(Vector2(75,25));
  Points.Add(Vector2(40,-10));
  Points.Add(Vector2(50,-60));
  Points.Add(Vector2(0,-40));
  Points.Add(Vector2(-50,-60));
  Points.Add(Vector2(-40,-10));
  Points.Add(Vector2(-75,25));
  Points.Add(Vector2(-25,35));
  Points.Add(Vector2(0,80));
  ReLoad;
  {$endif}
end;

destructor TCastlePolygon2D.Destroy;
begin
 Triangles.Free;
 inherited;
end;

procedure TCastlePolygon2D.Clear;
begin
  inherited;
  Triangles.Clear;
end;

function TCastlePolygon2D.SelfIntersections:boolean;
var i,j:integer;
    Cross:TVector2;
begin
  // проверка на самопересечение
  // на пересечение надо надо проверять не каждый отрезок, а через один
  Result:=false;
  for i:=0 to Points.Count-3 do begin
   for j:=i+2 to Points.Count-3 do begin
     if CrossingSegments(Points[i],Points[i+1],Points[j],Points[j+1],Cross)=1 then Result:=true;
     if Result then break;
   end;
    if Result then break;
  end;
  // проверка последнего отрезка
  for j:=1 to Points.Count-4 do begin
    if CrossingSegments(Points[Points.Count-2],Points[Points.Count-1],Points[j],Points[j+1],Cross)=1 then Result:=true;
    if Result then break;
  end;
end;

procedure TCastlePolygon2D.ReLoad;
var i:integer;
begin
  inherited;
  if Points.Count<4 then begin
   TriangleSet2DNode.SetVertices([]);
   ShapeNode.Geometry:=TriangleSet2DNode;
   Exit;
  end;
  Triangles.Clear;
  // триангуляция
  if not SelfIntersections then begin
   TriangulateFace(nil,Points.Count,{$ifdef FPC}@{$endif}GetPoint,{$ifdef FPC}@{$endif}AddFill,0);
   // вершины треугольников должны быть в порядке обхода по часавой стрелке
   i:=0;
   while i<Triangles.Count do begin
    DoGoodTriangle(Triangles,i+0,i+1,i+2,Scale.XY);
    i:=i+3;
   end;
  end;
  TriangleSet2DNode.SetVertices(Triangles);
  ShapeNode.Geometry:=TriangleSet2DNode;

end;

procedure  TCastlePolygon2D.AddFill(const Tri: TVector3Integer);
begin
   Triangles.Add(Points[Tri.X]);
   Triangles.Add(Points[Tri.Y]);
   Triangles.Add(Points[Tri.Z]);
end;
function TCastlePolygon2D.GetPoint(Index: integer): TVector3;
begin
   Result:=Vector3(Points[index],0);
end;


procedure TCastlePolygon2D.ChangedTransform;
var Sc:single;
begin
  inherited;
  Sc:=Scale.X*Scale.Y;
  if ((Sc<0)and(SignScale>0))or((Sc>0)and(SignScale<0)) then begin
   SignScale:=Sc;
   ReLoad;
  end;
end;

{разделы свойств}
function TCastlePolygon2D.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'LineType')
  then
    Result := []
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastlePolygon2D.SetLineType(NewLineType:TCastleLineType);
begin
 inherited SetLineType(ltClose);
end;

initialization

RegisterSerializableComponent(TCastlePolygon2D, 'Polygon2D Transform');
{$ifdef CASTLE_DESIGN_MODE}
RegisterPropertyEditor(TypeInfo(TCastleLineType), TCastlePolygon2D, 'LineType', THiddenPropertyEditor);
{$endif}


end.

