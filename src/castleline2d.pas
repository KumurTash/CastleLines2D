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
{*TCastleLine2D}
unit CastleLine2D;

interface

uses
  Classes,                                   // Lazarus units
  CastleVectors,X3DNodes,CastleClassUtils,   // CGE units
  CastleLine2DBase,CastleLine2DMath;         // CastleLine2D units


type
  {* Тип соединения прямоугольников в линии}
  TJoinMode=(jmBevel, //*< Скошенные края
             jmRound, //*< Закругленные края
             jmSharp);//*< Острые края
  {* Тип начала и конца линии}
  TBeginEndMode=(bmNone,
                 bmRound,   //*< Полукруг
                 bmTriangle,//*< Треугольник
                 bmBox);    //*< Половина квадрата
  {*
   Полилиния строится из прямоугольников по набору вершин.
   Имеет различные методы соединения прямоугольников,
   различные варианты начала и конца линии.
   Может быть как замкнутой, так и не замкнутой @br

   Для входа в режим редактирования используйте "verb" (Edit mode ON) -
   правой кнопкой мыши по объекту в окне иерархии объектов.
  }
  TCastleLine2D = class (TCastleLine2DBase)
    private
      {Треугольники для рисования}
      TriangleSet2DNode:TTriangleSet2DNode;
      {Тип соеденения}
      FJoinMode:TJoinMode;
      {Количество сегментов при JoinMode=jmRound}
      FJoinRoundPrecision:integer;
      {Если угол (в родианах) между линиями < FJoinSharpLimitRad то применяется jmRound иначе jmSharp   (используется при JoinMode=jmSharp}
      FJoinSharpLimitRad:single;
      {чем начинается линия}
      FBeginMode:TBeginEndMode;
      {чем заканчивается линия}
      FEndMode:TBeginEndMode;
      {Ширина линии}
      FLineWidth:Single;
      {Для определения нужно ли перестраивать сцену при изменении Scale}
      SignScale:Single;
      procedure SetJoinMode(jm:TJoinMode);
      procedure SetJoinRoundPrecision(RoundPrecision:integer);
      procedure SetJoinSharpLimitRad(Limit:single);
      procedure SetBeginMode(mode:TBeginEndMode);
      procedure SetEndMode(mode:TBeginEndMode);
      procedure SetLineWidth(wl:Single);
    protected
      {Для определения нужно ли перестраивать сцену при изменении Scale}
      procedure ChangedTransform; override;
    public
      {*
       При создании в редакторе CGE создает линию из двух точек.
       При создании непосредственно в коде (RunTime) точки не добавляются.
      }
      constructor Create(AOwner: TComponent); override;
      procedure ReLoad; override;
      {Разделы свойств}
      function PropertySections(const PropertyName: String): TPropertySections; override;
    published
      {* Тип соединения прямоугольников в линии}
      property JoinMode:TJoinMode read FJoinMode write SetJoinMode default jmBevel;
      {* Количество сегментов в секторе при JoinMode=jmRound}
      property JoinRoundPrecision:integer read FJoinRoundPrecision write SetJoinRoundPrecision default 4;
      {* При этом или меньшем угле между линиями острый угол не строится. Используется при JoinMode=jmSharp}
      property JoinSharpLimitRad:single read FJoinSharpLimitRad write SetJoinSharpLimitRad default 0;
      {* Тип начала линии}
      property JoinBeginMode:TBeginEndMode read FBeginMode write SetBeginMode default bmNone;
      {* Тип конца линии}
      property JoinEndMode:TBeginEndMode read FEndMode write SetEndMode default bmNone;
      {* Ширина линии}
      property LineWidth:Single read FLineWidth write SetLineWidth default 10;
  end;

implementation

uses SysUtils,CastleComponentSerialize;

constructor TCastleLine2D.Create(AOwner: TComponent);
begin
  inherited;
  SignScale:=1;
  TriangleSet2DNode:=TTriangleSet2DNode.Create;
  FJoinMode:=jmBevel;
  FBeginMode:=bmNone;
  FEndMode:=bmNone;
  FLineWidth:=10;
  FJoinRoundPrecision:=4;
  FJoinSharpLimitRad:=0;
  {$ifdef CASTLE_DESIGN_MODE}
   Points.Add(Vector2(-50,-50));
   Points.Add(Vector2(50,50));
   ReLoad;
  {$endif}

end;

procedure TCastleLine2D.Reload;
var
    cross,A,B,C,A1,A2,B1,B2:TVector2;
    i,pi1,pi2:integer;
    Rects,BeginEnds:TVector2List;
    isCross:boolean;
    Triangles:TVector2List;
begin
   if (LineWidth<=0) or (Points.Count<=0) then begin
     TriangleSet2DNode.SetVertices([]);
     ShapeNode.Geometry:=TriangleSet2DNode;
     Exit;
   end;
   inherited;
   // заполняем вершины прямоугольников
   Triangles:=TVector2List.Create;
   for pi1:=0 to Points.Count-2 do begin
    pi2:=pi1+1;
    AddRect(Triangles,Points[pi1],Points[pi2],LineWidth);
   end;
   // --------------------------Прямоугольники на стыках не должны пересекаться
   Rects:=TVector2List.Create;
   Rects.AddRange(Triangles);
   for i:=1 to Points.Count-1 do begin
      pi1:=i-1;
      pi2:=i;
      if i=Points.Count-1 then begin        // необходимо обработать последний и первый прямоугольник
        if (LineType<>ltOpen)and(Points.Count>3) then begin     // если это не открытая линия
         pi1:=i-1;
         pi2:=0;
        end else Continue;
      end;
      isCross:=false;
      if CrossingSegments(Rects[pi1*6+1],Rects[pi1*6+2],Rects[pi2*6+1],Rects[pi2*6+2],cross) = 1 then begin
         Triangles[pi1*6+2]:=cross;
         Triangles[pi2*6+1]:=cross;
         Triangles[pi1*6+5]:=Triangles[pi1*6+2];
         // для заполнения промежутков
         isCross:=true;
         A:=cross;
         B:=Rects[pi1*6+3];
         C:=Rects[pi2*6+0];
         if FJoinMode=jmSharp then begin
          A1:=Rects[pi1*6+0]; A2:=Rects[pi1*6+3];
          B1:=Rects[pi2*6+3]; B2:=Rects[pi2*6+0];
         end;
       end else
       if CrossingSegments(Rects[pi1*6+0],Rects[pi1*6+3],Rects[pi2*6+0],Rects[pi2*6+3],cross) = 1 then begin
         Triangles[pi1*6+3]:=cross;
         Triangles[pi2*6+0]:=cross;
         Triangles[pi2*6+4]:=Triangles[pi2*6+0];
         // для заполнения промежутков
         isCross:=true;
         A:=cross;
         B:=Rects[pi1*6+2];
         C:=Rects[pi2*6+1];
         if FJoinMode=jmSharp then begin
          A1:=Rects[pi1*6+1]; A2:=Rects[pi1*6+2];
          B1:=Rects[pi2*6+2]; B2:=Rects[pi2*6+1];
         end;
       end else
       if CrossingSegments(Rects[pi1*6+1],Rects[pi1*6+2],Rects[pi2*6+0],Rects[pi2*6+3],cross) = 1 then begin
         Triangles[pi1*6+2]:=cross;
         Triangles[pi2*6+0]:=cross;
         Triangles[pi1*6+5]:=Triangles[pi1*6+2];
         Triangles[pi2*6+4]:=Triangles[pi2*6+0];
         // для заполнения промежутков
         isCross:=true;
         A:=cross;
         B:=Rects[pi1*6+3];
         C:=Rects[pi2*6+1];
         if FJoinMode=jmSharp then begin
           A1:=Rects[pi1*6+0]; A2:=Rects[pi1*6+3];
           B1:=Rects[pi2*6+2]; B2:=Rects[pi2*6+1];
         end;
       end else
       if CrossingSegments(Rects[pi1*6+0],Rects[pi1*6+3],Rects[pi2*6+1],Rects[pi2*6+2],cross) = 1 then begin
         Triangles[pi1*6+3]:=cross;
         Triangles[pi2*6+1]:=cross;
         // для заполнения промежутков
         isCross:=true;
         A:=cross;
         B:=Rects[pi1*6+2];
         C:=Rects[pi2*6+0];
         if FJoinMode=jmSharp then begin
           A1:=Rects[pi1*6+1]; A2:=Rects[pi1*6+2];
           B1:=Rects[pi2*6+3]; B2:=Rects[pi2*6+0];
         end;
       end;
       // заполняем промежутки между прямоугольниками
       if isCross then begin
        if FJoinMode=jmRound then AddArcBetweenPoints(Triangles,Points[pi2],A,B,C,FJoinRoundPrecision);
        if FJoinMode=jmBevel then AddTriangle(Triangles,A,B,C);
        if FJoinMode=jmSharp then begin
        if AngleRadBetweenVectors(Vector3(A2-A1,0),Vector3(B2-B1,0))>FJoinSharpLimitRad then
           AddCrossSegments(Triangles,A,A1,A2,B1,B2)
        else
          AddTriangle(Triangles,A,B,C);
        end;
       end else begin
        if FJoinMode=jmRound then AddHalfCircle(Triangles,Rects[pi1*6+2],Rects[pi1*6+3],FJoinRoundPrecision,Rects[pi1*6+0]);
       end;
   end;

   //----- Начало и конец
   if  (LineType=ltOpen)or((Points.Count<=3)and(LineType<>ltOpen))  then begin
    BeginEnds:=TVector2List.Create;
    if FBeginMode=bmTriangle then AddTriangleOpposite(BeginEnds,Rects[0],Rects[1],Rects[2]);
    if FBeginMode=bmBox then AddHalfBox(BeginEnds,Rects[0],Rects[1],Rects[2]);
    if FBeginMode=bmRound then AddHalfCircle(BeginEnds,Rects[0],Rects[1],FJoinRoundPrecision,Rects[2]);
    Triangles.InsertRange(0,BeginEnds);
    BeginEnds.Clear;
    pi1:= Points.Count-2;
    if FEndMode=bmTriangle then AddTriangleOpposite(BeginEnds,Rects[pi1*6+2],Rects[pi1*6+3],Rects[pi1*6+0]);
    if FEndMode=bmBox then AddHalfBox(BeginEnds,Rects[pi1*6+2],Rects[pi1*6+3],Rects[pi1*6+0]);
    if FEndMode=bmRound then AddHalfCircle(BeginEnds,Rects[pi1*6+2],Rects[pi1*6+3],FJoinRoundPrecision,Rects[pi1*6+0]);
    Triangles.AddRange(BeginEnds);
    BeginEnds.Free;
   end;
   //----- вершины треугольников должны быть в порядке обхода по часавой стрелке
   i:=0;
   while i<Triangles.Count-2 do begin
    DoGoodTriangle(Triangles,i+0,i+1,i+2,Scale.XY);
    i:=i+3;
   end;
   Rects.Free;
   TriangleSet2DNode.SetVertices(Triangles);
   ShapeNode.Geometry:=TriangleSet2DNode;
   Triangles.Free;
end;

procedure TCastleLine2D.ChangedTransform;
var Sc:single;
begin
  inherited;
  Sc:=Scale.X*Scale.Y;
  if ((Sc<0)and(SignScale>0))or((Sc>0)and(SignScale<0)) then begin
   SignScale:=Sc;
   ReLoad;
  end;
end;


procedure TCastleLine2D.SetJoinMode(jm:TJoinMode);
begin
  if FJoinMode=jm then Exit;
  FJoinMode:=jm;
end;

procedure TCastleLine2D.SetJoinRoundPrecision(RoundPrecision:integer);
begin
  if FJoinRoundPrecision=RoundPrecision then Exit;
  if RoundPrecision<1 then FJoinRoundPrecision:=1 else FJoinRoundPrecision:=RoundPrecision;
end;

procedure TCastleLine2D.SetBeginMode(mode:TBeginEndMode);
begin
  if mode=FBeginMode then Exit;
  FBeginMode:=mode;
end;

procedure TCastleLine2D.SetEndMode(mode:TBeginEndMode);
begin
  if mode=FEndMode then Exit;
  FEndMode:=mode;
end;

procedure TCastleLine2D.SetJoinSharpLimitRad(Limit:single);
begin
  if Limit=FJoinSharpLimitRad then Exit;
  if Limit<0 then FJoinSharpLimitRad:=0 else
  if Limit>pi then FJoinSharpLimitRad:=pi else
  FJoinSharpLimitRad:=Limit;
end;

procedure TCastleLine2D.SetLineWidth(wl:Single);
begin
  if FLineWidth=wl then Exit;
  if wl<0 then FLineWidth:=0 else FLineWidth:=wl;
end;

{разделы свойств}
function TCastleLine2D.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'JoinMode') or
     (PropertyName = 'JoinRoundPrecision') or
     (PropertyName = 'JoinSharpLimitRad') or
     (PropertyName = 'JoinBeginMode') or
     (PropertyName = 'JoinEndMode') or
     (PropertyName = 'LineWidth')
  then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;


initialization

RegisterSerializableComponent(TCastleLine2D, 'Line2D Transform');

end.
