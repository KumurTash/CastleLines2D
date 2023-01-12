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

{*
 Модуль содержит некоторые математические методы, а также методы для построения
 некоторых примитивов.
}
unit CastleLine2DMath;

interface

uses
   SysUtils,Math,  // Lazarus units
   CastleVectors,CastleUtils;  // CGE units

  {*
   Преобразует строку StringValue в число путем удаления лишнего.
   Remain это неудаляемый остаток в строке
   (можно использовать для того, чтобы '-' в начале строки не удалялся)
  }
  function  DoFloat(var StringValue:string;const Remain:string=''):single;

  {*
    Минимальное расстояние от точки (P) до отрезка (SP1:SP2) с расчетом точки проекции (PP) на отрезок. @br
    0 - точка проецируется на отрезок: Distance = расст.от P до отрезка SP1,SP2. PP - проекция точки P на отрезок SP1,SP2 @br
    1 - точка P лежит "левее"  SP1: Distance = расст.от P до SP1. PP=SP1 @br
    2 - точка P лежит "правее" SP2: Distance = расст.от P до SP2. PP=SP2
  }
  function PSDistanceEx(const P,SP1,SP2:TVector2;var Distance:Single; var PP:TVector2):integer;

  {*
   Пересечение отрезков A1:A2, B1:B2. @br
    1 Если есть точка пересечения "CrossPoint" @br
    0 если это параллельные линии @br
   -1 если нет точек пересечения
  }
  function CrossingSegments(const a1,a2,b1,b2:TVector2; var CrossPoint:TVector2):shortint;

  {*
   Если порядок обхода треугольника A,B,C по часовой стрелке, то вернет @true
  }
  function IsClockwiseTriangle(A,B,C:TVector2):boolean;

  {*
   Строит дугу между точками A,B c центром в точке Center @br
   O - точка соединения сегментов (может совпадать с Center) @br
   EdgesCount - количество сегментов @br
   Полученные треугольники добавляются в Arc @br
   Result = длинна дуги
  }
  function AddArcBetweenPoints(var Arc:TVector2List; const Center,O,A,B:TVector2; const EdgesCount:integer):single;

  {*
  Строит квадрат у которого скруглен один угол на отрезке А:B в направлении противоположном Opposite.
  EdgesCount количество сегментов дуги.
  Result = длинна дуги.
  }
  procedure AddRoundBoxTop(var Arc:TVector2List; const A,B:TVector2; const Opposite:Tvector2; const EdgesCount:integer);

  {*
   Строит полукруг на отрезке А:B в направлении противоположном Opposite @br
   EdgesCount - количество сегментов @br
   Полученные треугольники добавляются в Arc
  }
  procedure AddHalfCircle(var Arc:TVector2List; const A,B:TVector2; const EdgesCount:integer; const Opposite:Tvector2);

  {*
   Добавит треугольник в Arc
  }
  procedure AddTriangle(var Arc:TVector2List; const A,B,C:TVector2);

  {*
   Строит прямоугольный равнобедренный треугольник на отрезке А:B в направлении противоположном Opposite.
   Гипотенуза - A:B
  }
  procedure AddTriangleOpposite(var Arc:TVector2List; const A,B:TVector2; const Opposite:Tvector2);

  {*
   Строит прямоугольный равнобедренный треугольник на отрезке А:B в направлении противоположном Opposite.
   Гипотенуза A:NewPoint
  }
  procedure AddTriangleTop(var Arc:TVector2List; const A,B:TVector2; const Opposite:Tvector2);


  {*
   Строит половину квадрат на отрезке А:B в направлении противоположном Opposite
  }
  procedure AddHalfBox(var Arc:TVector2List; const A,B:TVector2; const Opposite:Tvector2);

  {*
   Строит квадрат на отрезке А:B в направлении противоположном Opposite
  }
  procedure AddBox(var Arc:TVector2List; const A,B:TVector2; const Opposite:Tvector2);


  {*
  Строит прямоугольник осью симметрии которого является отрезок A:B. @br
  Создает 0..5 точек 4 точка равна 0 точке. 5 точка равна 2 точке @br
  При A.X<B.X нулевая точка это левая нижняя @br
  При A.X>B.X нулевая точка это правая верхняя @br
  Если A.X=B.X и A.Y<B.Y нулевая точка это правая нижняя @br
  Если A.X=B.X и A.Y>B.Y нулевая точка это левая верхняя @br
  Обход прямоугольника по часовой стрелке  @br
  width - высота прямоугольника
  }
  procedure AddRect(var Arc:TVector2List; const A,B:TVector2; width:single);

  {*
    Добавит треугольники для соединения двух отрезков в точке пересечения вне этих отрезков.
    A1:A2 первый отрезок.
    B1:B2 второй отрезок.
    O - точка соединения треугольников. @br
    В результате построит треугольники (O,A2,cross) и (O,B2,cross). @br
    Result = длина построенных отрезков.
  }
  function AddCrossSegments(var Arc:TVector2List; const O,A1,A2,B1,B2:TVector2):single;

  {*
  Сортирует вершины треугольника в порядок обхода по часовой стрелке
  в зависимости от Scale (во 2 и 4 четвертях порядок обхода меняется). @br
  Если произведены изменения (поменялись местами B и С), то вернет true.
  }
  function DoGoodTriangle(var Lt:TVector2List; A,B,C:integer; Scale:TVector2):boolean;

implementation

function DoFloat(var StringValue:string;const Remain:string=''):single;
var i:integer;
    OldSeparator: char;
begin
  OldSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  i:=pos(',',StringValue);
  if i>0 then StringValue[i]:='.';
  while (StringValue<>Remain)and(Length(StringValue)>0)and(TryStrToFloat(StringValue,Result)=false) do begin
    StringValue:=copy(StringValue,0,Length(StringValue)-1);
  end;
  if (StringValue=Remain)or(Length(StringValue)=0) then Result:=0;

  DefaultFormatSettings.DecimalSeparator := OldSeparator;
end;


function PSDistanceEx(const P,SP1,SP2:TVector2;var Distance:Single; var PP:TVector2):integer;
var
  dX21,dY21,dX1,dY1,dX2,dY2,aSide,d,L2:single;
begin
  dX21:=SP2.X-SP1.X;
  dY21:=SP2.Y-SP1.Y;
  dX1:=P.X-SP1.X;
  dY1:=P.Y-SP1.Y;
  //если угол между векторами (SP1->SP2) и (SP1->P)  > +-90 градусов
  //=> P лежит "левее" точки SP1 => мин.Dist = расст.до точки SP1
  aSide:=dX21*dX1+dY21*dY1;
  if aSide <= 0 then
  begin
    if aSide = 0 then
      Result:=0
    else
      Result:=1;
    Distance:=sqrt(dX1*dX1+dY1*dY1);
    PP:=SP1;
  end
  else
  begin
    dX2:=P.X-SP2.X;
    dY2:=P.Y-SP2.Y;
    //если угол между векторами (SP2->SP1) и (SP2->P)  > +-90 градусов
    //=> P лежит "правее" точки SP2 => мин.Dist = расст.до точки SP2
    aSide:=dX21*dX2+dY21*dY2;
    if aSide >= 0 then  //= ((-dX21)*dX2+(-dY21)*dY2 < 0)
    begin
      if aSide = 0 then
        Result:=0
      else
        Result:=2;
      Distance:=sqrt(dX2*dX2+dY2*dY2);
      PP:=SP2;
    end
    else
    //оба угла <= 90гр. => точка проецируется на отрезок (SP1-SP2)
    //=> мин.расст = расстоянию до прямой SP1-SP2
    begin
      d:=dY21*dX1-dX21*dY1;       //ненормированное расстояние до отрезка со знаком
      L2:=dX21*dX21+dY21*dY21;    //квадрат длины отрезка
      Distance:=abs(d)/sqrt(L2);  //расстояние
      d:=d/L2;
      PP.X:=P.X-dY21*d;
      PP.Y:=P.Y+dX21*d;
      Result:=0;
    end;
  end;
end;

function CrossingSegments(const a1,a2,b1,b2:TVector2; var CrossPoint:TVector2):shortint;
var d,da,db,ta,tb: single;
begin
  CrossPoint:=Vector2(0,0);
  if VectorsParallel(Vector3(a2-a1,0),Vector3(b2-b1,0)) then Exit(0);
  d :=(a1.x-a2.x)*(b2.y-b1.y) - (a1.y-a2.y)*(b2.x-b1.x);
  da:=(a1.x-b1.x)*(b2.y-b1.y) - (a1.y-b1.y)*(b2.x-b1.x);
  db:=(a1.x-a2.x)*(a1.y-b1.y) - (a1.y-a2.y)*(a1.x-b1.x);

  ta:=da/d;
  tb:=db/d;
  if (0<=ta)and(ta<=1)and(0<=tb)and(tb<=1)then begin
   CrossPoint:=Vector2(a1.x+ta*(a2.x-a1.x),a1.y+ta*(a2.y-a1.y));
   Result := 1
  end else Result := -1;

end;

function IsClockwiseTriangle(A,B,C:TVector2):boolean;
var ABx,ABy,ACx,ACy:Single;
begin
 ABx:=B.x-A.x;
 ABy:=B.y-A.y;
 ACx:=A.x-C.x;
 ACy:=A.y-C.y;
 Result:=(ABx * ACy - ACx * ABy)<0;
end;


function AddArcBetweenPoints(var Arc:TVector2List; const Center,O,A,B:TVector2; const EdgesCount:integer):single;
var angle:single; //угол между сегментами
    i:integer;
    Point1,Point2,v1,v2:TVector3;
    NewArc:TVector2List;
  begin
  Result:=0;
  if TVector2.Equals(B,A) then Exit;
  v1:=Vector3(A-Center,0);
  v2:=Vector3(B-Center,0);
  if v1.IsZero or v2.IsZero then angle:=pi/EdgesCount else angle:=AngleRadBetweenVectors(v1,v2)/(EdgesCount);

  if not IsClockwiseTriangle(Center,A,B) then angle:=-angle;
  NewArc:=TVector2List.Create;
  for i:=0 to EdgesCount-1 do begin
   NewArc.Add(O);
   v1:=Vector3(A-Center,0);
   v2:=Vector3(Center,0);
   Point1:=v2+RotatePointAroundAxisRad(angle*i,v1,Vector3(0,0,1));
   NewArc.Add(Point1.XY);
   Point2:=v2+RotatePointAroundAxisRad(angle*(i+1),v1,Vector3(0,0,1));
   NewArc.Add(Point2.XY);
   Result:=Result+PointsDistance(Point1,Point2);
  end;
  if Result>0 then Arc.AddRange(NewArc);
  NewArc.Free;

end;


procedure AddHalfCircle(var Arc:TVector2List; const A,B:TVector2; const EdgesCount:integer;const Opposite:Tvector2);
var angle:single;
    i:integer;
    point,v1,v2:TVector3;
    Center:TVector2;
  begin
  angle:=pi/EdgesCount;
  Center:=(A+B)*Vector2(0.5,0.5);
  if not IsClockwiseTriangle(Opposite,A,B) then angle:=-angle;
  for i:=0 to EdgesCount-1 do begin
   Arc.Add(Center);
   v1:=Vector3(A-Center,0);
   v2:=Vector3(Center,0);
   Point:=v2+RotatePointAroundAxisRad(angle*i,v1,Vector3(0,0,1));
   Arc.Add(Point.XY);
   Point:=v2+RotatePointAroundAxisRad(angle*(i+1),v1,Vector3(0,0,1));
   Arc.Add(Point.XY);
  end;
end;


procedure AddTriangle(var Arc:TVector2List; const A,B,C:TVector2);
begin
  Arc.Add(A);
  Arc.Add(B);
  Arc.Add(C);
end;


procedure AddTriangleOpposite(var Arc:TVector2List; const A,B:TVector2; const Opposite:Tvector2);
var point,v1,v2:TVector3;
    Center:TVector2;
  begin
  Arc.Add(A);
  Arc.Add(B);
  Center:=(A+B)*Vector2(0.5,0.5);
  v1:=Vector3(A-Center,0);
  v2:=Vector3(Center,0);

  if not IsClockwiseTriangle(Opposite,A,B) then begin
   Point:=v2+RotatePointAroundAxisMinus90(v1,Vector3(0,0,1));
  end else begin
   Point:=v2+RotatePointAroundAxis90(v1,Vector3(0,0,1));
  end;
   Arc.Add(Point.XY);
end;

procedure AddTriangleTop(var Arc:TVector2List; const A,B:TVector2; const Opposite:Tvector2);
var point:TVector3;
begin
  Arc.Add(A);
  Arc.Add(B);
  if not IsClockwiseTriangle(Opposite,A,B) then begin
   Point:=Vector3(A,0)+RotatePointAroundAxisMinus90(Vector3(A-B,0),Vector3(0,0,1));
  end else begin
   Point:=Vector3(A,0)+RotatePointAroundAxis90(Vector3(A-B,0),Vector3(0,0,1));
  end;
  Arc.Add(Point.XY);
end;

procedure AddRoundBoxTop(var Arc:TVector2List; const A,B:TVector2; const Opposite:Tvector2;const EdgesCount:integer);
var point:TVector3;
begin
  if not IsClockwiseTriangle(Opposite,A,B) then begin
   Point:=Vector3(A,0)+RotatePointAroundAxisMinus90(Vector3(A-B,0),Vector3(0,0,1));
  end else begin
   Point:=Vector3(A,0)+RotatePointAroundAxis90(Vector3(A-B,0),Vector3(0,0,1));
  end;
  AddArcBetweenPoints(Arc,A,A,Point.XY,B,EdgesCount);
end;

procedure AddHalfBox(var Arc:TVector2List; const A,B:TVector2; const Opposite:Tvector2);
var Point1,Point2,Center:TVector3;
begin
  Center:=Vector3((A+B)*Vector2(0.5,0.5),0);

  if not IsClockwiseTriangle(Opposite,A,B) then begin
   Point1:=Vector3(A,0)+RotatePointAroundAxis90(Center-Vector3(A,0),Vector3(0,0,1));
   Point2:=Vector3(B,0)+RotatePointAroundAxisMinus90(Center-Vector3(B,0),Vector3(0,0,1));
  end else begin
    Point1:=Vector3(A,0)+RotatePointAroundAxisMinus90(Center-Vector3(A,0),Vector3(0,0,1));
    Point2:=Vector3(B,0)+RotatePointAroundAxis90(Center-Vector3(B,0),Vector3(0,0,1));
  end;
  Arc.AddRange([A,
               B,
               Point1.XY,
               Point1.XY,
               Point2.XY,
               B]);
end;

procedure AddBox(var Arc:TVector2List; const A,B:TVector2; const Opposite:Tvector2);
var Point1,Point2:TVector3;
begin
  if not IsClockwiseTriangle(Opposite,A,B) then begin
   Point1:=Vector3(A,0)+RotatePointAroundAxis90(Vector3(B-A,0),Vector3(0,0,1));
   Point2:=Vector3(B,0)+RotatePointAroundAxisMinus90(Vector3(A-B,0),Vector3(0,0,1));
  end else begin
    Point1:=Vector3(A,0)+RotatePointAroundAxisMinus90(Vector3(B-A,0),Vector3(0,0,1));
    Point2:=Vector3(B,0)+RotatePointAroundAxis90(Vector3(A-B,0),Vector3(0,0,1));
  end;
  Arc.AddRange([A,
               B,
               Point1.XY,
               Point1.XY,
               Point2.XY,
               B]);

end;

procedure AddRect(var Arc:TVector2List; const A,B:TVector2; width:single);
var d,dx,dy,angle:single;
    NewRect:array[0..5] of TVector2;
begin
    d:=width/2;
    if Math.IsZero(A.x-B.x) then begin // если вертикальная линия
     if B.Y>A.Y then d:=0-d;
     NewRect[0]:=(Vector2(A.x-d,A.y));
     NewRect[1]:=(Vector2(B.x-d,B.y));
     NewRect[2]:=(Vector2(B.x+d,B.y));
     NewRect[3]:=(Vector2(A.x+d,A.y));
    end else begin
     angle:=2*Pi-AngleRadPointToPoint(A.X,A.Y,B.X,B.Y);
     dx:=(d*sin(angle)); //проекции d на оси ОХ и ОY
     dy:=(d*cos(angle));
     NewRect[0]:=(Vector2((A.x-dx),(A.y-dy)));
     NewRect[1]:=(Vector2((B.x-dx),(B.y-dy)));
     NewRect[2]:=(Vector2((B.x+dx),(B.y+dy)));
     NewRect[3]:=(Vector2((A.x+dx),(A.y+dy)));
    end;
    NewRect[4]:=NewRect[0];
    NewRect[5]:=NewRect[2];

    Arc.AddRange(NewRect);
end;



function AddCrossSegments(var Arc:TVector2List; const O,A1,A2,B1,B2:TVector2):single;
var cross:TVector2;
    Line1,Line2:TVector3;
    NewArc:TVector2List;
begin
 Result:=0;
 if TVector2.Equals(A1,B1) and TVector2.Equals(A2,B2)  then Exit;
 if VectorsParallel(Vector3(A2-A1,0),Vector3(B2-B1,0)) then Exit;
 Line1:=Line2DFrom2Points(A1, A2);
 Line2:=Line2DFrom2Points(B1, B2);
 cross:=O;
 try
   cross:=Lines2DIntersection(Line1, Line2);
 finally
 end;
 NewArc:=TVector2List.Create;
 NewArc.AddRange([O,
                  A2,
                  cross,
                  O,
                  B2,
                  cross]);
 Result:=PointsDistance(A2,cross)+PointsDistance(B2,cross);
 if Result>0 then Arc.AddRange(NewArc);
 NewArc.Free;
end;




function DoGoodTriangle(var Lt:TVector2List; A,B,C:integer; Scale:TVector2):boolean;
var Sc:Single;
    good:Boolean;
begin
 Result:=false;
 good:=IsClockwiseTriangle(Lt[A],Lt[B],Lt[C]);
 Sc:=Scale.X*Scale.Y;
 // во 2 и 4 четвертях порядок обхода меняется
 if ((good=false)and(Sc>0))or((good=true)and(Sc<0))  then begin
  Result:=true;
  Lt.Exchange(B,C);
 end;
end;


end.

