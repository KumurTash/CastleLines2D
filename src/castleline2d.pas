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
  Classes,                                               // Lazarus units
  CastleVectors,X3DNodes,CastleClassUtils,CastleUtils,CastleTransform,   // CGE units
  CastleLine2DBase,CastleLine2DMath;                     // CastleLine2D units


type
  {* Тип для определения способа вычесления текстурных координат}
  TLineTexCoordMode=(cmDefault,       //*< Тексурными координаты считаются методами CGE.
                     cmLine,          //*< Используется вся высота текстуры для наложения на линию.
                     cmBeginEndLine); //*< Позволяет использовать разные области текстуры для начала и конца. Подробнее @link(TCastleLine2D.TexCoordMode)
  {* Тип соединения прямоугольников в линии}
  TJoinMode=(jmBevel, //*< Скошенные края
             jmRound, //*< Закругленные края
             jmSharp);//*< Острые края
  {* Тип начала и конца линии}
  TBeginEndMode=(bmNone,
                 bmRound,         //*< Полукруг
                 bmTopRound,      //*< Скругленный квадрат сверху
                 bmBottomRound,   //*< Скругленный квадрат снизу
                 bmTriangle,      //*< Треугольник
                 bmTopTriangle,   //*< Треугольник с гипотенузой вверху
                 bmBottomTriangle,//*< Треугольник с гипотенузой внизу
                 bmHalfBox,       //*< Половина квадрата
                 bmBox);          //*< Квадрат
  PBeginEndMode=^TBeginEndMode;
  {* Способ наложения текстуры на изгибы линии}
  TJoinTexCoordMode=(jcmStretch,  //*< растянуть текстуру
                     jcmCrop);    //*< обрезать текстуру


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
      {Треугольники для рисования с текстурными координатами}
      FGeometry: TIndexedTriangleSetNode;
      FCoordinate: TCoordinateNode;
      FTextureCoordinate: TTextureCoordinateNode;
      {Для получения размера текстуры}
      FTextureNodeOnlyToGetSize: TImageTextureNode;
      {Для определения была ли изменина текстура}
      FOldTextureURL:String;
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
      {Режим наложения текстурных координат}
      FTexCoordMode:TLineTexCoordMode;
      {Режим наложения текстурных координат на изгибах линии}
      FJoinTexCoordMode:TJoinTexCoordMode;
      {При изменения текстуры устанавливает FTextureNodeOnlyToGetSize}
      function AfterChangeTexture:boolean;
      {универсальная процедура для добавления текстурных координат для любой геометрии прикрепляемой к прямоугольнику линии
       В Arc должны лежать только вершины начала (конца) @br
       TexCoord - сюда добавятся текстурные координаты @br
       BeginX,BeginY - начало текстуры по осям. @br
       EndY - конец текстуры по оси Y @br
       TX - коофициент оси X для правильного наложения текстуры. Обычно равен  @link(TCastleLine2D.FTexX) @br
       Minus - отрицательное направление текстурных координат}
      procedure AddElementTexCoord(const Arc:TVector2List; const A,B:TVector2; var TexCoord:TVector2List; BeginX,BeginY,EndY:single; TX:single; Minus:boolean=false);
      procedure SetJoinMode(jm:TJoinMode);
      procedure SetJoinRoundPrecision(RoundPrecision:integer);
      procedure SetJoinSharpLimitRad(Limit:single);
      procedure SetBeginMode(mode:TBeginEndMode);
      procedure SetEndMode(mode:TBeginEndMode);
      procedure SetLineWidth(wl:Single);
      class procedure CreateInitialPoints(Sender: TObject);
    protected
      {*
        Коэффициент для правильного наложения текстуры.
        Определяет какую часть текстуры по оси Y необходимо использовать.
        При TexCoordMode=cmLine равен 1, при TexCoordMode=cmBeginEndLine равен 0.5
      }
      FTexY:single;
      {*
       Коэффициент оси X для правильного наложения текстуры.
       Обычно равен (Высота текстуры)/(Ширина линии*Ширина текстуры)
      }
      FTexX:single;
      { Для определения нужно ли перестраивать сцену при изменении Scale}
      procedure ChangedTransform; override;
      {*
       Добавить геометрию начала и конца линии. A,B - отрезок, куда прикрепляется геометрия. @br
       Opposite - в противоположном направлении от этой точки будет строиться геометрия. @br
       GList - сюда необходимо добавлять геометрию. @br
       BeginOrEnd - указатель на @link(TCastleLine2D.JoinBeginMode) или на @link(TCastleLine2D.JoinEndMode). @br
       Потомки должны переопределить метод, если требуется добавить новую геометрию.
      }
      procedure AddGeometryBeginEnd(const BeginOrEnd:PBeginEndMode; var GList:TVector2List; const A,B,Opposite:TVector2); virtual;

      {*
       Устанавливает @link(TCastleLine2D.TexCoordMode) и @link(TCastleLine2D.FTexY) и @link(TCastleLine2D.FTexX).
      }
      procedure SetTexCoordMode(cm:TLineTexCoordMode);

    public
      {*
       При создании в редакторе CGE создает линию из двух точек.
       При создании непосредственно в коде (RunTime) точки не добавляются.
      }
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
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
      {*
       Метод вычисления текстурных координат. @br
       При @bold(cmDefault) текстурные координаты устанавливаются методами CGE. @br
       При @bold(cmLine) линия повторяет текcтуру по мере удлинения. Текстура может быть любая. @br
       При @bold(cmBeginEndLine) используются разные области текстуры для начала и конца.
       Текстура должна быть специално подготовлена. @br
       Примеры текстур для @bold(cmBeginEndLine):
          @image(../img/BeginEndLine1.png) или @image(../img/BeginEndLine2.png) или @image(../img/BeginEndLine3.png)
      }
      property TexCoordMode:TLineTexCoordMode read FTexCoordMode write SetTexCoordMode default cmDefault;
      {* Способ вычисления текстурных координат на изгибы линии. При JoinMode=jmSharp не используется}
      property JoinTexCoordMode:TJoinTexCoordMode read FJoinTexCoordMode write FJoinTexCoordMode default jcmCrop;

  end;

implementation

uses SysUtils,CastleComponentSerialize;

class procedure TCastleLine2D.CreateInitialPoints(Sender: TObject);
var
   P:TCastleLine2D;
begin
 P := Sender as TCastleLine2D;
 P.Points.Add(Vector2(-50,-50));
 P.Points.Add(Vector2(50,50));
 P.ReLoad;

end;

constructor TCastleLine2D.Create(AOwner: TComponent);
begin
  inherited;
  SignScale:=1;
  FGeometry:=TIndexedTriangleSetNode.Create;
  FCoordinate:=TCoordinateNode.Create;
  FTextureCoordinate:=TTextureCoordinateNode.Create;
  FTexCoordMode:=cmDefault;
  FTextureNodeOnlyToGetSize:=TImageTextureNode.Create;
  FOldTextureURL:='';
  FJoinMode:=jmBevel;
  FBeginMode:=bmNone;
  FEndMode:=bmNone;
  FLineWidth:=10;
  FJoinRoundPrecision:=4;
  FJoinSharpLimitRad:=0;
  FJoinTexCoordMode:=jcmCrop;

  FGeometry.Coord:=FCoordinate;
  ShapeNode.Geometry:=FGeometry;
end;

destructor TCastleLine2D.Destroy;
begin
    FTextureNodeOnlyToGetSize.Free;
   inherited;
end;

procedure TCastleLine2D.Reload;
var
    cr,cross,A,B,C,A1,A2,B1,B2:TVector2;
    i,j,k,pi1,pi2:integer;
    Rects:TVector2List;      // хранит неизмененные прямоугольники
    Triangles:TVector2List;  // хранит трапеции и промежутки между ними
    BeginEnds:TVector2List;  // хранит начало или конец (не одновременно), еще используется для Join пр jmRound
    Joins:TVector2List;      // используется для построения Join
    ResultCoord:TVector2List;// Для сборки всех элементов линии попорядку
    List3:TVector3List;      // для добавления в FGeometry
    isCross:boolean;         // для построени я трапеций. есть ли пересечения прямоугольников
    Indexes:TLongIntList;    // для добавления в FGeometry
    // для текстурных координат
    CurTex:single; // текущее положение текстурных координат по оси X
    len:single;
    iA1,iA2,iB,iC,icr:integer;
    TexCoordBeginEnds:TVector2List;  // текстурные координаты начала или конца (не одновременно)
    TexCoord:TVector2List;           // хранит текстурные координаты трапеций и промежутки между ними
    ResultTexCoord:TVector2List;     // здесь собирается все текстурные координаты попорядку
    JoinA1,JoinA2,JoinB,JoinC:TVector2;



    procedure AddTriangleToResult(pi:integer);
    begin
     ResultCoord.AddRange([Triangles[pi*6+0],Triangles[pi*6+1],Triangles[pi*6+2],
                           Triangles[pi*6+3],Triangles[pi*6+4],Triangles[pi*6+5]]);
     if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then
         ResultTexCoord.AddRange([TexCoord[pi*6+0],TexCoord[pi*6+1],TexCoord[pi*6+2],
                                  TexCoord[pi*6+3],TexCoord[pi*6+4],TexCoord[pi*6+5]]);
    end;

begin

   if (LineWidth<=0) or (Points.Count<=0) then begin
     FCoordinate.SetPoint([]);
     Exit;
   end;
   inherited;
   Rects:=TVector2List.Create;
   ResultCoord:=TVector2List.Create;
   BeginEnds:=TVector2List.Create;
   Triangles:=TVector2List.Create;
   List3:=TVector3List.Create;
   Indexes:=TLongIntList.Create;
   Joins:=TVector2List.Create;
   // для текстурных координат
   if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
    TexCoord:=TVector2List.Create;
    ResultTexCoord:=TVector2List.Create;
    TexCoordBeginEnds:=TVector2List.Create;
    CurTex:=0;
    AfterChangeTexture;
   end;
   // заполняем вершины прямоугольников
   for pi1:=0 to Points.Count-2 do begin
    pi2:=pi1+1;
    AddRect(Triangles,Points[pi1],Points[pi2],LineWidth);
    // текстурные координаты для получившихся прямоугольников
    if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
     len:=PointsDistance(Points[pi1],Points[pi2])*FTexX;
     TexCoord.AddRange([
       Vector2(CurTex,0),
       Vector2(CurTex+len,0),
       Vector2(CurTex+len,FTexY),
       Vector2(CurTex,FTexY),
       Vector2(CurTex,0),
       Vector2(CurTex+len,FTexY)
      ]);
     CurTex:=CurTex+len;
    end;
   end;

   // --------------------------Прямоугольники на стыках не должны пересекаться
   Rects.AddRange(Triangles);
   CurTex:=0;
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
      if CrossingSegments(Rects[pi1*6+3],Rects[pi1*6+2],Rects[pi2*6+3],Rects[pi2*6+2],cross) = 1 then begin
         isCross:=true;
         iA1:=pi1*6+2; iA2:=pi2*6+3; iB:=pi1*6+1; iC:=pi2*6+0;
         if FJoinMode=jmSharp then begin
          A1:=Rects[pi1*6+0]; A2:=Rects[pi1*6+1];
          B1:=Rects[pi2*6+1]; B2:=Rects[pi2*6+0];
         end;
       end else
       if CrossingSegments(Rects[pi1*6+0],Rects[pi1*6+1],Rects[pi2*6+0],Rects[pi2*6+1],cross) = 1 then begin
         isCross:=true;
         iA1:=pi1*6+1; iA2:=pi2*6+0; iB:=pi1*6+2; iC:=pi2*6+3;
         if FJoinMode=jmSharp then begin
          A1:=Rects[pi1*6+3]; A2:=Rects[pi1*6+2];
          B1:=Rects[pi2*6+2]; B2:=Rects[pi2*6+3];
         end;
       end else
       if CrossingSegments(Rects[pi1*6+3],Rects[pi1*6+2],Rects[pi2*6+0],Rects[pi2*6+1],cross) = 1 then begin
         isCross:=true;
         iA1:=pi1*6+2; iA2:=pi2*6+0; iB:=pi1*6+1; iC:=pi2*6+3;
         if FJoinMode=jmSharp then begin
           A1:=Rects[pi1*6+0]; A2:=Rects[pi1*6+1];
           B1:=Rects[pi2*6+2]; B2:=Rects[pi2*6+3];
         end;
       end else
       if CrossingSegments(Rects[pi1*6+0],Rects[pi1*6+1],Rects[pi2*6+3],Rects[pi2*6+2],cross) = 1 then begin
         isCross:=true;
         iA1:=pi1*6+1; iA2:=pi2*6+3; iB:=pi1*6+2; iC:=pi2*6+0;
         if FJoinMode=jmSharp then begin
           A1:=Rects[pi1*6+3]; A2:=Rects[pi1*6+2];
           B1:=Rects[pi2*6+1]; B2:=Rects[pi2*6+0];
         end;
       end;

       //Если найдено пересечение
       if isCross then begin
         // поправить прямоугольники, чтобы они не пересекались
         Triangles[iA1]:=cross;
         Triangles[iA2]:=cross;
         Triangles[pi1*6+4]:=Triangles[pi1*6+0];
         Triangles[pi1*6+5]:=Triangles[pi1*6+2];
         Triangles[pi2*6+4]:=Triangles[pi2*6+0];
         Triangles[pi2*6+5]:=Triangles[pi2*6+2];
         // текстурные координаты
         if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
            len:=PointsDistance(Rects[iA1],Triangles[iA1])*FTexX;
            TexCoord[iA1]:=TexCoord[iA1]-Vector2(len,0);
            len:=PointsDistance(Rects[iA2],Triangles[iA2])*FTexX;
            TexCoord[iA2]:=TexCoord[iA2]+Vector2(len,0);
            TexCoord[pi1*6+4]:=TexCoord[pi1*6+0];
            TexCoord[pi1*6+5]:=TexCoord[pi1*6+2];
            TexCoord[pi2*6+4]:=TexCoord[pi2*6+0];
            TexCoord[pi2*6+5]:=TexCoord[pi2*6+2];
         end;
         A:=Triangles[iA1]; B:=Triangles[iB]; C:=Triangles[iC];

         //для текстурных координат соеденений
         if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
          if TexCoord[iB].Y>TexCoord[iA1].Y then begin
           JoinA1:= Rects[iA1];
           JoinB:= Rects[iB];
           JoinA2:= Rects[iA2];
           JoinC:= Rects[iC];
          end else begin
           JoinA1:= Rects[iB];
           JoinB:= Rects[iA1];
           JoinA2:= Rects[iC];
           JoinC:= Rects[iA2];
          end;
         end;
         // Получившиеся трапецию надо добавить в результат построения
         if not( (LineType<>ltOpen)and(Points.Count>3)and(pi1=0) ) then AddTriangleToResult(pi1);

        // заполнить промежутки между прямоугольниками
        // скошенный угол
         if (FJoinMode=jmBevel)or
            ((FJoinMode=jmRound)and(FJoinRoundPrecision<=1))or
            ((FJoinMode=jmSharp)and(AngleRadBetweenVectors(Vector3(A2-A1,0),Vector3(B2-B1,0))<=FJoinSharpLimitRad)) then begin
          cr:=(B+C)*Vector2(0.5,0.5);
          len:=PointsDistance(B,C);
          if len>0 then begin
           AddTriangle(ResultCoord,A,B,cr);
           AddTriangle(ResultCoord,A,C,cr);
           // текстурные координаты
           if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
            len:=len*FTexX;
            CurTex:=CurTex+Len;
            for j:=0 to 5 do TexCoord[pi2*6+j]:=TexCoord[pi2*6+j]+Vector2(CurTex,0);
            if JoinTexCoordMode=jcmStretch then begin
             ResultTexCoord.AddRange([
              TexCoord[iA1],
              TexCoord[iB],
              TexCoord[iB]+Vector2(len/2,0),
              TexCoord[iA2],
              TexCoord[iC],
              TexCoord[iC]-Vector2(len/2,0)
             ]);
            end else begin
             Joins.Clear;
             Joins.Add(cr);
             ResultTexCoord.AddRange([TexCoord[iA1],TexCoord[iB]]);
             AddElementTexCoord(Joins,JoinA1,JoinB,ResultTexCoord,TexCoord[iB].X,0,FTexY,FTexX,false);
             ResultTexCoord.AddRange([TexCoord[iA2],TexCoord[iC]]);
             AddElementTexCoord(Joins,JoinA2,JoinC,ResultTexCoord,TexCoord[iC].X,0,FTexY,FTexX,true);
            end;
           end;
          end;
         end;
        // закругленный угол
        if (FJoinMode=jmRound)and(FJoinRoundPrecision>1) then begin
         Joins.Clear;
         len:=AddArcBetweenPoints(Joins,Points[pi2],A,B,C,FJoinRoundPrecision);
         ResultCoord.AddRange(Joins);
         // текстурные координаты
         if (Self.Texture<>'')and(FTexCoordMode<>cmDefault)and(len>0) then begin
          len:=len*FTexX;
          CurTex:=CurTex+Len;
          for j:=0 to 5 do TexCoord[pi2*6+j]:=TexCoord[pi2*6+j]+Vector2(CurTex,0);
          icr:=FJoinRoundPrecision div 2;
          for j:=0 to icr-1 do begin
            if JoinTexCoordMode=jcmStretch then begin
             ResultTexCoord.AddRange([
                TexCoord[iA1],
                TexCoord[iB]+Vector2(j*len/FJoinRoundPrecision,0),
                TexCoord[iB]+Vector2((j+1)*len/FJoinRoundPrecision,0)
               ]);
            end else begin
             BeginEnds.Clear;
             ResultTexCoord.Add(TexCoord[iA1]);
             BeginEnds.AddRange([Joins[j*3+1],Joins[j*3+2]]);
             AddElementTexCoord(BeginEnds,JoinA1,JoinB,ResultTexCoord,TexCoord[iB].X,0,FTexY,FTexX,false);
            end;
          end;
          for j:=icr to FJoinRoundPrecision-1 do begin
            if JoinTexCoordMode=jcmStretch then begin
             k:=icr-j+1;
             ResultTexCoord.AddRange([
               TexCoord[iA2],
               TexCoord[iC]-Vector2((k+1)*len/FJoinRoundPrecision,0),
               TexCoord[iC]-Vector2(k*len/FJoinRoundPrecision,0)
              ]);
            end else begin
             BeginEnds.Clear;
             ResultTexCoord.Add(TexCoord[iA2]);
             BeginEnds.AddRange([Joins[j*3+1],Joins[j*3+2]]);
             AddElementTexCoord(BeginEnds,JoinA2,JoinC,ResultTexCoord,TexCoord[iC].X,0,FTexY,FTexX,true);
            end;
          end;
         end;

        end;
        // острый угол
        if (FJoinMode=jmSharp)and(AngleRadBetweenVectors(Vector3(A2-A1,0),Vector3(B2-B1,0))>FJoinSharpLimitRad) then begin
           len:=AddCrossSegments(ResultCoord,A,A1,A2,B1,B2);
           if (Self.Texture<>'')and(FTexCoordMode<>cmDefault)and(len>0) then begin
               len:=len*FTexX;
               CurTex:=CurTex+Len;
               for j:=0 to 5 do TexCoord[pi2*6+j]:=TexCoord[pi2*6+j]+Vector2(CurTex,0);
               ResultTexCoord.AddRange([
                 TexCoord[iA1],
                 TexCoord[iB],
                 TexCoord[iB]+Vector2(len/2,0),
                 TexCoord[iA2],
                 TexCoord[iC],
                 TexCoord[iC]-Vector2(len/2,0)
               ]);
           end;
        end;

       end else begin  // if isCross
       //если пересечений не было, в результат добавить надо исходный прямоугольник
        if not( (LineType<>ltOpen)and(Points.Count>3)and(pi1=0) ) then AddTriangleToResult(pi1);
        if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
            for j:=0 to 5 do TexCoord[pi2*6+j]:=TexCoord[pi2*6+j]+Vector2(CurTex,0);
        end;
        // при jmRound надо добавить полукруг если пересечения не найдены, но линии расположены под углом
        if (FJoinMode=jmRound)and(VectorsParallel(Vector3(Points[pi2]-Points[pi1],0),Vector3(Points[pi2+1]-Points[pi2],0))=false) then begin
          BeginEnds.Clear;
          AddHalfCircle(BeginEnds,Rects[pi1*6+1],Rects[pi1*6+2],FJoinRoundPrecision,Rects[pi1*6+0]);
          ResultCoord.AddRange(BeginEnds);
          if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then
             AddElementTexCoord(BeginEnds,Rects[pi1*6+1],Rects[pi1*6+2],ResultTexCoord,TexCoord[pi1*6+1].X,0,FTexY,FTexX);
        end;
       end;
   end;// for

   // надо добавить в результат последнюю трапецию
     if pi2<Points.Count-1 then AddTriangleToResult(pi2) // при закрытой линии
     else AddTriangleToResult(pi1);// при открытой

   //----- Начало и конец
   if  (LineType=ltOpen)or((Points.Count<=3)and(LineType<>ltOpen))  then begin
    // начало
    if JoinBeginMode<>bmNone then begin
     BeginEnds.Clear;
     AddGeometryBeginEnd(@JoinBeginMode,BeginEnds,Rects[0],Rects[3],Rects[2]);
     ResultCoord.InsertRange(0,BeginEnds);
     // текстурные координаты для начала
     if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
      if FTexCoordMode=cmBeginEndLine then begin
       AddElementTexCoord(BeginEnds,Rects[0],Rects[3],TexCoordBeginEnds,0,FTexY,2*FTexY,FTexX,true);
      end else begin
       AddElementTexCoord(BeginEnds,Rects[0],Rects[3],TexCoordBeginEnds,0,0,FTexY,FTexX,true);
      end;
      ResultTexCoord.InsertRange(0,TexCoordBeginEnds);
     end;
    end;
    // конец
    if JoinEndMode<>bmNone then begin
     BeginEnds.Clear;
     pi1:= Points.Count-2;
     AddGeometryBeginEnd(@JoinEndMode,BeginEnds,Rects[pi1*6+2],Rects[pi1*6+1],Rects[pi1*6+0]);
     ResultCoord.AddRange(BeginEnds);
     // текстурные координаты для конца
     if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
      TexCoordBeginEnds.Clear;
      if FTexCoordMode=cmBeginEndLine then begin
       AddElementTexCoord(BeginEnds,Rects[pi1*6+1],Rects[pi1*6+2],TexCoordBeginEnds,0,FTexY,2*FTexY,FTexX);
      end else begin
       AddElementTexCoord(BeginEnds,Rects[pi1*6+1],Rects[pi1*6+2],TexCoordBeginEnds,TexCoord[pi1*6+1].X,0,FTexY,FTexX);
      end;
      ResultTexCoord.AddRange(TexCoordBeginEnds);
     end;
    end;
   end;
   //----- вершины треугольников должны быть в порядке обхода по часавой стрелке
   i:=0;
   while i<ResultCoord.Count-2 do begin
    if DoGoodTriangle(ResultCoord,i+0,i+1,i+2,Scale.XY) then begin
      if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then ResultTexCoord.Exchange(i+1,i+2);
    end;
    List3.AddRange([Vector3(ResultCoord[i+0],0),Vector3(ResultCoord[i+1],0),Vector3(ResultCoord[i+2],0)]);
    Indexes.AddRange([i+0,i+1,i+2]);
    i:=i+3;
   end;

   // Геометрия
   FCoordinate.SetPoint(List3);
   if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
    FTextureCoordinate.SetPoint(ResultTexCoord);
    FGeometry.TexCoord:= FTextureCoordinate;
   end else begin
    // надо вернуть координаты текстуры в исходное состояние если поменяли тип текстуры на cmDefault
    if (Self.Texture<>'') and (FTextureCoordinate.ParentFieldsCount>0) then begin
     FTextureCoordinate.FreeRemovingFromAllParents;
     FTextureCoordinate:=TTextureCoordinateNode.Create;
    end;
   end;
   FGeometry.SetIndex(Indexes);

   Rects.Free;
   ResultCoord.Free;
   BeginEnds.Free;
   Triangles.Free;
   List3.Free;
   Indexes.Free;
   Joins.Free;
   if (Self.Texture<>'')and(FTexCoordMode<>cmDefault) then begin
    TexCoord.Free;
    ResultTexCoord.Free;
    TexCoordBeginEnds.Free;
   end;

end;

procedure TCastleLine2D.AddGeometryBeginEnd(const BeginOrEnd:PBeginEndMode; var GList:TVector2List; const A,B,Opposite:TVector2);
begin
   if BeginOrEnd^=bmTriangle then AddTriangleOpposite(GList,A,B,Opposite);
   if BeginOrEnd^=bmTopTriangle then AddTriangleTop(GList,A,B,Opposite);
   if BeginOrEnd^=bmBottomTriangle then AddTriangleTop(GList,B,A,Opposite);
   if BeginOrEnd^=bmHalfBox then AddHalfBox(GList,B,A,Opposite);
   if BeginOrEnd^=bmBox then AddBox(GList,B,A,Opposite);
   if BeginOrEnd^=bmRound then AddHalfCircle(GList,A,B,FJoinRoundPrecision,Opposite);
   if BeginOrEnd^=bmTopRound then AddRoundBoxTop(GList,A,B,Opposite,FJoinRoundPrecision);
   if BeginOrEnd^=bmBottomRound then AddRoundBoxTop(GList,B,A,Opposite,FJoinRoundPrecision);
end;

procedure TCastleLine2D.AddElementTexCoord(const Arc:TVector2List; const A,B:TVector2; var TexCoord:TVector2List; BeginX,BeginY,EndY:single; TX:single; Minus:boolean=false);
var i:integer;
    TY:single;
    Tex:TVector2;
    PP:TVector2;
    len:single;
begin
 TY:=(EndY-BeginY)/PointsDistance(A,B);
 for i:=0 to Arc.Count-1 do begin
   PSDistanceEx(Arc[i],A,B,len,PP);
   if Minus then Tex.X:=-len*TX else Tex.X:=len*TX;
   Tex.X:=BeginX+Tex.X;
   Tex.Y:=BeginY+PointsDistance(A,PP)*TY;
   TexCoord.Add(Tex);
 end;
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
  SetTexCoordMode(FTexCoordMode);// для пересчета коэфициентов текстуры
end;

procedure TCastleLine2D.SetTexCoordMode(cm:TLineTexCoordMode);
begin
  FTexCoordMode:=cm;
  if FTextureNodeOnlyToGetSize.TextureImage<>nil then begin
   if FTexCoordMode=cmBeginEndLine then FTexY:=0.5 else FTexY:=1;
   FTexX:=FTexY*FTextureNodeOnlyToGetSize.TextureImage.Height/(FLineWidth*FTextureNodeOnlyToGetSize.TextureImage.Width);
  end;
end;

function TCastleLine2D.AfterChangeTexture:boolean;
begin
  Result:=false;
  if FOldTextureURL<>Self.Texture then begin
   FOldTextureURL:=Self.Texture;
   FTextureNodeOnlyToGetSize.Free;
   FTextureNodeOnlyToGetSize:=TImageTextureNode.Create;
   FTextureNodeOnlyToGetSize.SetUrl(Self.Texture);
   SetTexCoordMode(FTexCoordMode);// для пересчета коэфициентов
   Result:=true;
  end;

end;

{разделы свойств}
function TCastleLine2D.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'JoinMode') or
     (PropertyName = 'JoinRoundPrecision') or
     (PropertyName = 'JoinSharpLimitRad') or
     (PropertyName = 'JoinBeginMode') or
     (PropertyName = 'JoinEndMode') or
     (PropertyName = 'LineWidth')  or
     (PropertyName = 'TexCoordMode') or
     (PropertyName = 'JoinTexCoordMode')
  then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;


var
  R: TRegisteredComponent;

initialization

R := TRegisteredComponent.Create;
R.ComponentClass := TCastleLine2D;
R.Caption := ['Line2D Transform'];
R.OnCreate := {$ifdef FPC}@{$endif}TCastleLine2D.CreateInitialPoints;
RegisterSerializableComponent(R);

end.
