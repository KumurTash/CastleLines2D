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

{ Main state, where most of the application logic takes place.}
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,CastleColors,
  CastleViewport,CastleLine2D;

const
  DISTANCE=10;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
   Lines:array of TCastleLine2D;
   CurrentPosition:TVector3;
   CurrentColor:TCastleColor;
   IsMotion:boolean;
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    Viewport1: TCastleViewport;
    BtnClear :TCastleButton;
    BtnChangeColor :TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean;override;
    function Motion(const Event: TInputMotion): Boolean;  override;

    procedure BtnClearClick(Sender: TObject);
    procedure BtnChangeColorClick(Sender: TObject);
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

destructor TStateMain.Destroy;
var i,n:integer;
begin
  n:=Length(Lines);
  for i:=0 to n-1 do Lines[i].Free;
  SetLength(Lines,0);
  inherited;
end;

procedure TStateMain.Start;
begin
  inherited;
  IsMotion:=false;
  CurrentColor:=White;
  BtnChangeColor.OnClick:={$ifdef FPC}@{$endif}BtnChangeColorClick;
  BtnClear.OnClick:={$ifdef FPC}@{$endif}BtnClearClick;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
var n:integer;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonLeft) then
  begin
    CurrentPosition:=Vector3(Viewport1.PositionTo2DWorld(Event.Position, true), 0);
    n:=Length(Lines);
    SetLength(Lines,n+1);
    Lines[n]:=TCastleLine2D.Create(Viewport1);
    Lines[n].Color:=CurrentColor;
    Lines[n].JoinBeginMode:=bmRound;
    Lines[n].JoinEndMode:=bmRound;
    Viewport1.Items.Insert(0,Lines[n]);
    Lines[n].Translation :=CurrentPosition;
    Lines[n].Points.Add(Vector2(0,0));
    IsMotion:=true;
    Exit(true);
  end;

end;

function TStateMain.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    IsMotion := false;
    Exit(ExclusiveEvents);
  end;
end;

function TStateMain.Motion(const Event: TInputMotion): Boolean;
var
  NewPosition:TVector3;
  n:integer;
begin
  Result := inherited;
  if Result then Exit;

  if isMotion then begin
    NewPosition:=Vector3(Viewport1.PositionTo2DWorld(Event.Position, true), 0);
    if PointsDistance(CurrentPosition,NewPosition)>DISTANCE then begin
      CurrentPosition:=NewPosition;
      n:=Length(Lines)-1;
      Lines[n].Points.Add((NewPosition-Lines[n].Translation).XY);
      Lines[n].ReLoad;
    end;
    Exit(ExclusiveEvents)
  end;
end;

procedure TStateMain.BtnClearClick(Sender: TObject);
var i,n:integer;
begin
  n:=Length(Lines);
  for i:=0 to n-1 do Lines[i].Free;
  SetLength(Lines,0);
end;

procedure TStateMain.BtnChangeColorClick(Sender: TObject);
begin
  CurrentColor:=Vector4(Random,Random,Random,1);
end;




end.
