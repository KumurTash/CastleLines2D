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
unit Water;
{
 Алгоритм расчета волн написан по статье:
 https://gamedevelopment.tutsplus.com/tutorials/make-a-splash-with-dynamic-2d-water-effects--gamedev-236
}
interface

uses
  Classes, SysUtils,CastleLine2DBase,CastleVectors;

type

TWaterColumn = record
  Index:integer;       // для связи с объектом линии
  TargetHeight:single; // начальное положение по Y
  Height:single;       // текущее положение по Y
  Speed:single;
  lDeltas:single;      //разницу в высоте между точкой и ее левым соседом
  rDeltas:single;      //разницу в высоте между точкой и ее правым соседом
end;


  TWater = class
    private
      FLine:TCastleLine2DBase;
      WaterColumns:array of TWaterColumn;
      FTension:Single;
      FDampening:Single;
      FSpread:Single;
      FCalcCount:integer;
      function GetCount:integer;
    public
      constructor Create (ALine:TCastleLine2DBase;ABeginIndex:integer;AEndIndex:Integer);
      procedure Update;
      procedure Splash(const index:integer;const speed:single);

      {
       Низкое значение вызовет большие волны, которые медленно колеблются.
       Высокое значение создаст небольшие волны, которые быстро колеблются.
       Высокая постоянная пружины сделает воду больше похожей на дрожащее желе.
       По умолчанию 0.025
      }
      property Tension:Single read FTension write FTension;
      {
       Высокий коэффициент увлажнения сделает воду густой, как патока, в то время
       как низкое значение позволит волнам колебаться в течение длительного времени.
       По умолчанию 0.025
      }
      property Dampening:Single read FDampening write FDampening;
      {
       Переменная контролирует, с какой скоростью распространяются волны.
       Она может принимать значения от 0 до 0,5,
       при этом большие значения ускоряют распространение волн.
       По умолчанию 0.25
      }
      property Spread:Single read FSpread write  FSpread;
      {
       Количество проходов, где столбцы тянут за собой своих соседей
       Значение > 0
       Влияет на плавность волн. Чем меньше тем волны плавнее
       По умолчанию 8
      }
      property CalcCount:integer read FCalcCount write FCalcCount;
      {
       количество точек
      }
      property Count:integer read GetCount;

  end;

implementation

constructor TWater.Create (ALine:TCastleLine2DBase;ABeginIndex:integer;AEndIndex:Integer);
var i:integer;
begin
  FTension:=0.025;
  FDampening:=0.025;
  FSpread:=0.25;
  FCalcCount:=8;
  FLine:=ALine;
  SetLength(WaterColumns,AEndIndex-ABeginIndex+1);
  for i:=0 to Length(WaterColumns)-1 do begin
    with WaterColumns[i] do begin
      Index:=ABeginIndex+i;
      TargetHeight:=FLine.Points[index].Y;
      Height:=FLine.Points[index].Y;
      Speed:=0;
      lDeltas:=0;
      rDeltas:=0;
    end;
  end;
end;

procedure TWater.Update;
  procedure UpdateWaterColumn(var Column:TWaterColumn; const ADampening, ATension:single);
  var
    x:single;
  begin
    x:= Column.Height-Column.TargetHeight;
    Column.Speed:=Column.Speed+ATension*x-Column.Speed*ADampening;
      Column.Height:=Column.Height-Column.Speed;
    if x<0 then
    x:=x;
  end;
var
  i,j,n:integer;
begin
  n:=Length(WaterColumns);
  for i:=0 to n-1 do begin
    UpdateWaterColumn(WaterColumns[i],Dampening, Tension)
  end;
  for j:=0 to FCalcCount-1 do begin
    for i:=0 to n-1 do begin
       if i>0 then begin
   	 WaterColumns[i].lDeltas:=Spread*(WaterColumns[i-1].Height-WaterColumns[i].Height);
   	 WaterColumns[i-1].Speed:=WaterColumns[i-1].Speed+WaterColumns[i].lDeltas;
       end;
       if i<n-1 then begin
 	WaterColumns[i].rDeltas:=Spread*(WaterColumns[i+1].Height-WaterColumns[i].Height);
   	WaterColumns[i+1].Speed:=WaterColumns[i+1].Speed+WaterColumns[i].rDeltas;
       end;
    end;
    for i:=0 to n-1 do begin
 	if i>0 then WaterColumns[i-1].Height:=WaterColumns[i-1].Height-WaterColumns[i].lDeltas;
 	if i<n-1 then WaterColumns[i+1].Height:=WaterColumns[i+1].Height-WaterColumns[i].rDeltas;
    end;
  end;

  for i:=0 to n-1 do FLine.Points[WaterColumns[i].Index]:=Vector2(FLine.Points[WaterColumns[i].Index].X,WaterColumns[i].Height);
  FLine.ReLoad;



end;



procedure TWater.Splash(const index:integer;const speed:single);
begin
  if (index>=0) and (index<Length(WaterColumns)) then WaterColumns[index].Speed:=speed;

end;

function TWater.GetCount:integer;
begin
  Result:=Length(WaterColumns);
end;


end.

