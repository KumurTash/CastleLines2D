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
  X3DNodes,CastleTriangulate,CastleVectors,CastleClassUtils,CastleUtils, // CGE units
  CastleLine2DBase,CastleLine2DMath;                         // CastleLine2D units

type
  {* Тип для определения способа вычисления текстурных координат}
  TPolygonTexCoordMode=(cmDefault,  //*< Текстурные координаты считаются методами CGE.
                        cmRepeat);  //*< Повторяющаяся текстура.

  {*
   Потенциально невыпуклый многоугольник.
   Строится по набору вершин методом триангуляции. @br

   Для входа в режим редактирования используйте "verb" (Edit mode ON) -
   правой кнопкой мыши по объекту в окне иерархии объектов.
  }
  TCastlePolygon2D = class (TCastleLine2DBase)
   private
     {Треугольники для рисования}
     FGeometry: TIndexedTriangleSetNode;
     FCoordinate: TCoordinateNode;
     FTextureCoordinate: TTextureCoordinateNode;
     {для триангуляции}
     Triangles:TVector2List;
     {Для определения нужно ли перестраивать сцену при изменении Scale}
     SignScale:Single;
     {Площадь}
     FArea:Single;
     {способ наложения текстурных координат}
     FTexCoordMode:TPolygonTexCoordMode;
     FTextureScale:TVector2;
     FTextureScalePersistent:TCastleVector2Persistent;
     {для триангуляции многоугольника для заполнения}
     procedure  AddFill(const Tri: TVector3Integer);
     {для триангуляции многоугольника для заполнения}
     function GetPoint(Index: integer): TVector3;
     procedure SetTexCoordMode(cm:TPolygonTexCoordMode);
     procedure SetTextureScale(const AValue: TVector2);
     function  GetTextureScalePersistent:TVector2;
     procedure SetTextureScalePersistent(const AValue: TVector2);
   protected
     {*Для определения нужно ли перестраивать сцену при изменении Scale}
     procedure ChangedTransform; override;
     {*Тип линии может быть только ltClose (замкнутая линия)}
     procedure SetLineType(NewLineType:TCastleLineType); override;
   public
     {*
      Вычислить площадь без отображения полигона
     }
     function  CalculateAreaWithoutDisplay:single;
     procedure ReLoad override;
     {*
      При создании в редакторе CGE добавляются точки, образующие звезду.
      При создании непосредственно в коде (RunTime) точки не добавляются.
     }
     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;
     procedure Clear; override;
     {Разделы свойств}
     function PropertySections(const PropertyName: String): TPropertySections; override;
     {*
      Площадь многоугольника вычисляется в @Link(CastlePolygon2D.Reload).
      Если многоугольник отображать не требуется, то можно площадь можно вычеслить
      используя функцию @Link(CastlePolygon2D.CalculateAreaWithoutDisplay).
      Если обнаружено самопересечение, то площадь равна 0.
     }
     property Area:single read FArea;
     {* Параметр влияет на увеличение текстуры при  TexCoordMode=cmRepeat.
      Чем он меньше, тем текстура больше.}
     property TextureScale:TVector2 read FTextureScale write SetTextureScale;
   published
     {* @Link(TCastlePolygon2D.TextureScale)}
     property TextureScalePersistent:TCastleVector2Persistent read FTextureScalePersistent;
     {* Метод вычисления текстурных координат}
     property TexCoordMode:TPolygonTexCoordMode read FTexCoordMode write SetTexCoordMode default cmDefault;
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
  TexCoordMode:=cmDefault;
  LineType:=ltClose;
  Triangles:=TVector2List.Create;
  FGeometry:=TIndexedTriangleSetNode.Create;
  FCoordinate:=TCoordinateNode.Create;
  FTextureCoordinate:=TTextureCoordinateNode.Create;
  FGeometry.Coord:=FCoordinate;
  ShapeNode.Geometry:=FGeometry;
  FArea:=0;

  FTextureScale := Vector2(1, 1);
  FTextureScalePersistent := TCastleVector2Persistent.Create;
  FTextureScalePersistent.InternalGetValue := {$ifdef FPC}@{$endif} GetTextureScalePersistent;
  FTextureScalePersistent.InternalSetValue := {$ifdef FPC}@{$endif} SetTextureScalePersistent;
  FTextureScalePersistent.InternalDefaultValue := FTextureScale;

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

procedure TCastlePolygon2D.ReLoad;
var
  Indexes:TLongIntList;    // для добавления в FGeometry
  List3:TVector3List;      // для добавления в FGeometry
  TexCoord:TVector2List;   // текстурные координаты
  i:integer;
begin
  inherited;
  if (Points.Count<4) or SelfIntersections then begin
   FCoordinate.SetPoint([]);
   Exit;
  end;
  List3:=TVector3List.Create;
  Indexes:=TLongIntList.Create;
  if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
    TexCoord:=TVector2List.Create;
  end;
  Triangles.Clear;
  FArea:=0;
  // триангуляция
   TriangulateFace(nil,Points.Count,{$ifdef FPC}@{$endif}GetPoint,{$ifdef FPC}@{$endif}AddFill,0);
   // вершины треугольников должны быть в порядке обхода по часавой стрелке
   i:=0;
   while i<Triangles.Count do begin
    DoGoodTriangle(Triangles,i+0,i+1,i+2,Scale.XY);
    List3.AddRange([Vector3(Triangles[i+0],0),Vector3(Triangles[i+1],0),Vector3(Triangles[i+2],0)]);
    Indexes.AddRange([i+0,i+1,i+2]);
    if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
      TexCoord.AddRange([Triangles[i+0]*FTextureScale*0.01,Triangles[i+1]*FTextureScale*0.01,Triangles[i+2]*FTextureScale*0.01]);
    end;
    i:=i+3;
   end;

   // Геометрия
   if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
    FTextureCoordinate.SetPoint(TexCoord);
    FGeometry.TexCoord:= FTextureCoordinate;
   end else begin
    // надо вернуть координаты текстуры в исходное состояние если поменяли тип текстуры на cmDefault
    if (Self.Texture<>'') and (FTextureCoordinate.ParentFieldsCount>0) then begin
     FTextureCoordinate.FreeRemovingFromAllParents;
     FTextureCoordinate:=TTextureCoordinateNode.Create;
    end;
   end;

   FCoordinate.SetPoint(List3);
   FGeometry.SetIndex(Indexes);

  List3.Free;
  Indexes.Free;

  if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
    TexCoord.Free;
  end;
end;

procedure  TCastlePolygon2D.AddFill(const Tri: TVector3Integer);
begin
   Triangles.AddRange([Points[Tri.X],
                       Points[Tri.Y],
                       Points[Tri.Z]]);
   FArea:=FArea+Abs(0.5*(Points[Tri.X].X-Points[Tri.Z].X)*(Points[Tri.Y].Y-Points[Tri.Z].Y)-
                        (Points[Tri.Y].X-Points[Tri.Z].X)*(Points[Tri.X].Y-Points[Tri.Z].Y));

end;
function TCastlePolygon2D.GetPoint(Index: integer): TVector3;
begin
   Result:=Vector3(Points[index],0);
end;

function  TCastlePolygon2D.CalculateAreaWithoutDisplay:single;
begin
 Triangles.Clear;
 FArea:=0;
 // триангуляция
 if not SelfIntersections then
   TriangulateFace(nil,Points.Count,{$ifdef FPC}@{$endif}GetPoint,{$ifdef FPC}@{$endif}AddFill,0);
 Result:=FArea;
end;

procedure TCastlePolygon2D.SetTexCoordMode(cm:TPolygonTexCoordMode);
begin
  FTexCoordMode:=cm;
end;


procedure TCastlePolygon2D.SetTextureScale(const AValue: TVector2);
var x,y:single;
begin
  x:=AValue.X;
  y:=AValue.Y;
  if x<=0 then x:=0.01;
  if y<=0 then y:=0.01;
  FTextureScale:=Vector2(x,y);
end;
function  TCastlePolygon2D.GetTextureScalePersistent:TVector2;
begin
 Result:=FTextureScale;
end;
procedure TCastlePolygon2D.SetTextureScalePersistent(const AValue: TVector2);
begin
 TextureScale := AValue;
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
    if (PropertyName = 'TexCoordMode') or
       (PropertyName = 'TextureScalePersistent')
    then
      Result := [psBasic]
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

