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

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,Math,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleLine2D,CastlePolygon2D,
  Water;
type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    Water1:TWater;
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    Button1:TCastleButton;
    PolygonWater:TCastlePolygon2D;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure Button1Click(Sender: TObject);
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

procedure TStateMain.Start;
begin
  inherited;
  Button1.OnClick:={$ifdef FPC}@{$endif}Button1Click;
  Water1:=TWater.Create(PolygonWater,3,31);
  Water1.Tension:=0.05;
  Water1.Dampening:=0.05;
  Water1.Spread:=0.1;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  Water1.Update;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;
end;

procedure TStateMain.Button1Click(Sender: TObject);
begin
  Water1.Splash(14,300);
end;

end.
