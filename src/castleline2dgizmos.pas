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
{*TCastleLine2DGizmos, Line2DGizmos}
unit CastleLine2DGizmos;


interface

uses Classes,Math,                                                        // Lazarus units
     CastleLine2DBase,CastleLine2DMath,                                   // CastleLine2D units
     CastleTransform,CastleTriangles,CastleRenderOptions,CastleColors,    // CGE units
     CastleVectors,CastleScene,X3DNodes,CastleKeysMouse,CastleProjection;

const
  {*Расстояние до точки при котором точка выделяется в частях от текущей высоты камеры}
  DISTANCE_SELECTED_POINT=0.01;
  {*Расстояние до точки при котором точка вставляется на линию в частях от текущей высоты камеры}
  DISTANCE_INSERT_POINT=0.005;
  {*размер Scale текста в частях от текущей высоты камеры}
  TEXT_SCALE=0.002;
  {*Размер шрифта}
  FONT_SIZE=11;
  {*Ширина линии для редактирования}
  LINE_WIDTH_EDITOR=2;
  {*Размер точки вершин}
  POINT_SIZE=8;
  {*Расстояние до границы в частях от текущей высоты камеры}
  BORDER_DISTANCE=1;

type
  {*
  Геометрия для редактирования потомков @link(TCastleLine2DBase) в редакторе CGE @br
  Левая кнопка мыши - добавить точку, переместить точку @br
  Правая кнопка мыши - удалить точку @br
  Средняя кнопка мыши - вызвать окно для редактирования точки @br

  Для входа в режим редактирования используйте "verb" (Edit mode ON)-
  правой кнопкой мыши по объекту в окне иерархии объектов. @br
  }
  TCastleLine2DGizmos = class (TCastleScene)
    private
      {выбранная вершина (если ничего не выбрано, то =-1)}
      FSelectPoint:integer;
      {для вставки новой точки в редакторе}
      FInsertPoint:TVector2;
      {индекс для вставки новой точки в редакторе}
      FInsertPointIndex:integer;
      {включен механизм перетаскивания}
      isMovePoint:boolean;
      {Линия которую редактирует}
      Line:TCastleLine2DBase;
      {Нажат ли Шифт}
      isShift:boolean;
      ShiftPoint:TVector2;

      const
       DistanceToExistGizmo = 1;
      {Берет процент от текущей высоты камеры. Работает только с ортогональной камерой}
      { TODO: Do you need a perspective camera?  Look TVisualizeTransformSelected.TGizmoScene.UpdateSize in DesignVisualizeTransform}
      function CameraPercentage(percent:single):Single;

      function BuildScene: TX3DRootNode;
    protected
      function LocalRayCollision(const RayOrigin, RayDirection: TVector3;const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision; override;
    public
      function PointingDeviceMove(const Pick: TRayCollisionNode; const Distance: Single): Boolean;override;
      function PointingDevicePress(const Pick: TRayCollisionNode; const Distance: Single): Boolean; override;
      function PointingDeviceRelease(const Pick: TRayCollisionNode; const Distance: Single; const CancelAction: Boolean): Boolean; override;
      function Press(const E: TInputPressRelease): boolean; override;
      function Release(const E: TInputPressRelease): boolean; override;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      {* Построить геометрию заново}
      procedure ReLoad;
      {* Спрятать геометрию для редактирования}
      Procedure Hide;
      {* Отобразить геометрию для редактирования}
      Procedure Show(ALine:TCastleLine2DBase);


    published


  end;


  var
  {*
  Этот объект используется для редактирования всех потомков @link(TCastleLine2DBase)
  в редакторе CGE. При входе в режим редактирования этому объекту назначается новый родитель
  методом Show(ALine:TCastleLine2DBase).
  }
  Line2DGizmos:TCastleLine2DGizmos;

implementation


uses {$ifdef CASTLE_DESIGN_MODE} UPointEditor, {$endif}
     SysUtils;


constructor TCastleLine2DGizmos.Create(AOwner: TComponent);
begin
  inherited;
  Line:=nil;
  Setup2D;
  Self.SetTransient;
  Hide;

  ListenPressRelease:=true;   // что бы работали события мыши
  RenderOptions.PointSize:=POINT_SIZE;
  RenderOptions.LineWidth:=LINE_WIDTH_EDITOR;



  RenderLayer := rlFront;
  InternalExistsOnlyInMeaningfulParents := true;
  Collides := false;
  Pickable := true;
  CastShadows := false;
  ExcludeFromStatistics := true;
  InternalExcludeFromParentBoundingVolume := true;


end;

destructor TCastleLine2DGizmos.Destroy;
begin
  inherited;
end;

procedure TCastleLine2DGizmos.ReLoad;
begin
  if Line=nil then Exit;
  Load(BuildScene,true);
end;

Procedure TCastleLine2DGizmos.Hide;
begin
  if Self.Exists=false then Exit;
  FSelectPoint:=-1;
  FInsertPointIndex:=-1;
  FInsertPoint:=TVector2.Zero;
  isMovePoint:=false;
  isShift:=false;
  Exists:=false;

  if Line<>nil then begin
   Line.EditMode:=false;
   Line.Remove(Self);
  end;
  Line:=nil;
end;

Procedure TCastleLine2DGizmos.Show(ALine:TCastleLine2DBase);
begin
  if ALine=Line then Exit;
  Hide;
  Line:=ALine;
  if Line<>nil then begin
   Line.EditMode:=true;
   Line.Insert(0,Self);
  end;
  Exists:=true;
  ReLoad;
end;


function TCastleLine2DGizmos.LocalRayCollision(const RayOrigin, RayDirection: TVector3;
  const TrianglesToIgnoreFunc: TTriangleIgnoreFunc): TRayCollision;
var
  DistanceToCameraSqr: Single;
  GizmoShouldExist: Boolean;
begin
  { Similar to TInternalCastleEditorGizmo.LocalRayCollision, do not collide
     when gizmo is also hidden. }
   DistanceToCameraSqr := PointsDistanceSqr(
     TVector3.Zero,
     RayOrigin
   );
   GizmoShouldExist := DistanceToCameraSqr > Sqr(DistanceToExistGizmo);
   if not GizmoShouldExist then
     Exit(nil); // do not pick with gizmo with raycast

   Result := inherited;

   { Hack to make picking of the gizmo work even when gizmo is obscured
     by other TCastleTransform (including bbox of Parent, which is what
     we actually want to transform).
     Hacking Distance to be smallest possible means that it "wins"
     when TCastleTransform.LocalRayCollision desides which collision
     is first along the ray. }
   if Result <> nil then
     Result.Distance := 0;
end;

function TCastleLine2DGizmos.PointingDevicePress(const Pick: TRayCollisionNode; const Distance: Single): Boolean;
var Point:TVector3;

begin
  Result:=inherited;
  if Result then Exit;
   ShiftPoint:=Vector2(MinSingle,MinSingle);
   if FSelectPoint>=0 then begin
    isMovePoint:=True;
    Result:=True;
   end else
   if FInsertPointIndex<0 then begin // добавление точки
      Point:=OutsideToLocal(Pick.Point);
      Line.Points.Add(Point.XY);
      Line.ReLoad;
      Reload;
      Result:=True;
   end;

end;

function TCastleLine2DGizmos.Release(const E: TInputPressRelease): boolean;
begin
    if E.IsKey(keyShift) then isShift:=false;
end;

function TCastleLine2DGizmos.Press(const E: TInputPressRelease): boolean;
{$ifdef CASTLE_DESIGN_MODE}
var PE:TPointEditor;
{$endif}
begin
  Result:=inherited;
  if Result then Exit;
  if E.IsKey(keyEscape) then  begin
   Hide;
   Exit(true);
  end;
  if E.IsKey(keyShift) then begin
   isShift:=true;
  end;
  if E.IsMouseButton(buttonRight)and(FSelectPoint>=0)and(isMovePoint=false)and(Line.Points.Count>2) then begin  // удаление точки
   Line.Points.Delete(FSelectPoint);
   FSelectPoint:=-1;
   Line.ReLoad;
   Reload;
   Exit(true);
  end;
  {$ifdef CASTLE_DESIGN_MODE}
  if (E.IsMouseButton(buttonMiddle)and(FSelectPoint>=0)and(isMovePoint=false)) then begin
    PE:=TPointEditor.Create;
    PE.ShowModal(FSelectPoint,Line.Points[FSelectPoint]);
    if PE.Execute=peSave then Line.Points[FSelectPoint]:=PE.ResultVector;
    if (PE.Execute=peDelete)and(Line.Points.Count>2) then begin
     Line.Points.Delete(FSelectPoint);
    FSelectPoint:=-1;
    end;
    FreeAndNil(PE);
    ReLoad;
    Line.ReLoad;
    Exit(true);
  end;
  {$endif}


end;

function TCastleLine2DGizmos.PointingDeviceMove(const Pick: TRayCollisionNode; const Distance: Single): boolean;
var i:integer;
    point:TVector3;
    OldSelectPoint:integer;
    OldInsertPoint:TVector2;
    PP:TVector2;
    dist:Single;
    camDistSelect,camDistInsert:Single;
begin
   Result:=inherited;
   if Result then Exit;
   camDistSelect := CameraPercentage(DISTANCE_SELECTED_POINT); // расстояние до точки с учетом камеры
   camDistInsert := CameraPercentage(DISTANCE_INSERT_POINT); // расстояние до точки с учетом камеры
   Point:=OutsideToLocal(Pick.Point);
   if isMovePoint=False then begin
     OldSelectPoint:=FSelectPoint;
     // выделение точки
     FSelectPoint:=-1;
     for i:=Line.Points.Count-1 downto 0  do begin
      if PointsDistance(Point.XY,Line.Points[i])<camDistSelect then begin
       FSelectPoint:=i;
       Result:=true;
       break;
      end;
     end;

     // вставка точки на линию
         FInsertPointIndex:=-1;
         OldInsertPoint:=FInsertPoint;
         FInsertPoint:=TVector2.Zero;
         PP:=TVector2.Zero;
         dist:=MaxSingle;
         if FSelectPoint=-1 then begin
          for i:=Line.Points.Count-1 downto 1 do begin
           if PSDistanceEx(Point.XY,Line.Points[i],Line.Points[i-1],dist,PP)=0 then begin
            if dist<camDistInsert then begin
             FInsertPointIndex:=i;
             FInsertPoint:=PP;
             break;
            end; {if dist<camDist}
           end; {if PSDistanceEx....}
          end; {for}
         end; {if fselectPoint=-1}
         // end вставка точки на линию

    if (OldSelectPoint<>FSelectPoint)or(FInsertPoint.X<>OldInsertPoint.X)or(FInsertPoint.Y<>OldInsertPoint.Y) then begin
       Load(BuildScene,true);
       Result:=true;
     end;

   end else begin // Механизм перемещения
    Result:=true;
    if isShift then begin
     Writeln(ShiftPoint.ToString);
     if TVector2.Equals(ShiftPoint,Vector2(MinSingle,MinSingle)) then ShiftPoint:=Line.Points[FSelectPoint] else
       if Abs(Point.XY.X-ShiftPoint.X)>Abs(Point.XY.Y-ShiftPoint.Y) then Line.Points[FSelectPoint]:=Vector2(Point.XY.X,ShiftPoint.Y)
       else Line.Points[FSelectPoint]:=Vector2(ShiftPoint.X,Point.XY.Y);
    end else  Line.Points[FSelectPoint]:=Point.XY;
    Line.ReLoad;
    Reload;
   end;

end;


function TCastleLine2DGizmos.PointingDeviceRelease(const Pick: TRayCollisionNode; const Distance: Single; const CancelAction: Boolean): Boolean;
begin
 Result:=inherited;
 if Result then Exit;

 if isMovePoint then begin  // конец перемещения точки
  isMovePoint:=False;
  Result:=True
 end else
 if FInsertPointIndex>=0 then begin // вставить точку
   if FSelectPoint=-1 then begin
     Line.Points.Insert(FInsertPointIndex,FInsertPoint);
     FSelectPoint:=FInsertPointIndex;
     FInsertPointIndex:=-1;
     FInsertPoint:=TVector2.Zero;
     Line.ReLoad;
     Reload;
     Result:=true;
   end;
 end;

end;

function TCastleLine2DGizmos.BuildScene: TX3DRootNode;
var
  {}
  TransformNode:TTransformNode;
  {Точки вершин}
  GPoints:TPolypoint2DNode;
  ShapePoints:TShapeNode;
  MaterialPoints:TMaterialNode;
  {Точка для вставки}
  GInsertPoint:TPolypoint2DNode;
  ShapeInsertPoint:TShapeNode;
  MaterialInsertPoint:TMaterialNode;
  {Выделенная точка}
  GSelectPoint:TPolypoint2DNode;
  ShapeSelectPoint:TShapeNode;
  MaterialSelectPoint:TMaterialNode;
  {Линии }
  GLines:TPolyline2DNode;
  ShapeLines:TShapeNode;
  MaterialLines:TMaterialNode;
  {Граница, в которой можно редактировать}
  GBorder:TPolyline2DNode;
  ShapeBorder:TShapeNode;
  MaterialBorder:TMaterialNode;
  {Текст для обозначения номеров вершин}
  TextTransform:TTransformNode;
  GText:TTextNode;
  ShapeText:TShapeNode;
  Font:TFontStyleNode;
  MaterialText:TMaterialNode;

  i:integer;
  MaxXY,MinXY:TVector2;
  ScaleUniform: Single;
begin
  Result := TX3DRootNode.Create;
  if Line.Points.Count<=0 then Exit;
  MaxXY:=Line.Points[0];
  MinXY:=Line.Points[0];
  for i:=0 to Line.Points.Count-1 do begin
   if Line.Points.Count>1 then begin
     if MaxXY.X<Line.Points[i].X then MaxXY.X:=Line.Points[i].X;
     if MaxXY.Y<Line.Points[i].Y then MaxXY.Y:=Line.Points[i].Y;
     if MinXY.X>Line.Points[i].X then MinXY.X:=Line.Points[i].X;
     if MinXY.Y>Line.Points[i].Y then MinXY.Y:=Line.Points[i].Y;
   end;
  end;
  ScaleUniform:=CameraPercentage(BORDER_DISTANCE);
  MaxXY:=MaxXY+Vector2(ScaleUniform,ScaleUniform);
  MinXY:=MinXY-Vector2(ScaleUniform,ScaleUniform);


  TransformNode:=TTransformNode.Create;
  TextTransform:=nil;

  //граница
  GBorder:=TPolyline2DNode.CreateWithShape(ShapeBorder);
  GBorder.SetLineSegments([MinXY,Vector2(MinXY.X,MaxXY.Y),MaxXY,Vector2(MaxXY.X,MinXY.Y),MinXY]);
  MaterialBorder:=TMaterialNode.Create;
  MaterialBorder.EmissiveColor := RedRGB;
  ShapeBorder.Material := MaterialBorder;


  //линии
  GLines:=TPolyline2DNode.CreateWithShape(ShapeLines);
  GLines.SetLineSegments(Line.Points);
  MaterialLines := TMaterialNode.Create;
  MaterialLines.EmissiveColor := RedRGB;
  ShapeLines.Material := MaterialLines;
  // точки вершин
  GPoints:=TPolypoint2DNode.CreateWithShape(ShapePoints);
  GPoints.SetPoint(Line.Points);
  MaterialPoints := TMaterialNode.Create;
  MaterialPoints.EmissiveColor := RedRGB;
  ShapePoints.Material := MaterialPoints;
  // выделенная точка
  if FSelectPoint>=0 then begin;
   GSelectPoint:=TPolypoint2DNode.CreateWithShape(ShapeSelectPoint);
   GSelectPoint.SetPoint(Line.Points[FSelectPoint]);
   MaterialSelectPoint:=TMaterialNode.Create;
   MaterialSelectPoint.EmissiveColor := GrayRGB;
   ShapeSelectPoint.Material:=MaterialSelectPoint;
   TransformNode.AddChildren(ShapeSelectPoint);
   // текст
   if World.MainCamera.ProjectionType=ptOrthographic then begin
    GText:=TTextNode.CreateWithTransform(ShapeText,TextTransform);
    MaterialText:=TMaterialNode.Create;
    MaterialText.EmissiveColor := RedRGB;
    ShapeText.Material:=MaterialText;
    GText.SetText([FSelectPoint.ToString]);
    Font:=TFontStyleNode.Create;
    Font.Size:=FONT_SIZE;
    GText.FontStyle:=Font;
    // подгон масштаба под камеру
    ScaleUniform := CameraPercentage(TEXT_SCALE);
    TextTransform.Translation:=Vector3(Line.Points[FSelectPoint].X-Font.Font.TextWidth(FSelectPoint.ToString)/4*ScaleUniform,Line.Points[FSelectPoint].Y+ScaleUniform*POINT_SIZE/1.5,1);
    TextTransform.Scale:=Vector3(ScaleUniform, ScaleUniform, ScaleUniform);
   end;
  end;
  // точка для вставки на линию
  if FInsertPointIndex>=0 then begin
   GInsertPoint:=TPolypoint2DNode.CreateWithShape(ShapeInsertPoint);
   GInsertPoint.SetPoint(FInsertPoint);
   MaterialInsertPoint:=TMaterialNode.Create;
   MaterialInsertPoint.EmissiveColor := BlueRGB;
   ShapeInsertPoint.Material:=MaterialInsertPoint;
   TransformNode.AddChildren(ShapeInsertPoint);
  end;

  TransformNode.AddChildren(ShapeBorder);
  TransformNode.AddChildren(ShapePoints);
  TransformNode.AddChildren(ShapeLines);

  if TextTransform<>nil then Result.AddChildren(TextTransform);
  Result.AddChildren(TransformNode);

end;


function TCastleLine2DGizmos.CameraPercentage(percent:single):Single;
begin
  // TODO: Do you need a perspective camera?  Look TVisualizeTransformSelected.TGizmoScene.UpdateSize in DesignVisualizeTransform
  Result :=  percent*World.MainCamera.Orthographic.EffectiveRect.Height;
  Result := Parent.WorldToLocalDistance(Result);

end;

initialization
  Line2DGizmos:=TCastleLine2DGizmos.Create(nil);

finalization
   Line2DGizmos.Free;






end.

