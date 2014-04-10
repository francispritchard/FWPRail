UNIT FWPShowMessageUnit;
{ A non-modal replacement for ShowMessage }

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, RailDraw, GetTime, Lenz, InitVars, CreateRoute,
     StdCtrls;

TYPE
  TFWPShowMessageWindow = CLASS(TForm)
    FWPShowMessageLabel: TLabel;
    OKButton: TButton;
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE FWPShowMessage(Msg : String); Overload;
{ Replacement for Delphi ShowMessage - allows for box to be closed by the user or by program }

VAR
  FWPShowMessageWindow : TFWPShowMessageWindow;

IMPLEMENTATION

{$R *.dfm}

USES Options;

CONST
  UnitName = 'FWPShowMessage';

VAR
  SaveRouteSettingArrayPos : Integer = 0;
  SaveSubRouteSettingArrayPos : Integer = 0;

PROCEDURE FWPShowMessage(Msg : String); Overload;
{ Replacement for Delphi ShowMessage - allows for box to be closed by the user or by program. [Not sure user bit is working - button is
  not mentioned in this code! FWP 13/9/11]
}
VAR
  MsgWidth : Integer;

BEGIN
  WITH FWPShowMessageWindow DO BEGIN
    { Default window width was 200, height 110 }
    MsgWidth := Canvas.TextWidth(Msg);

    { Don't know why MsgWidth on its own isn't enough, but it ain't }
    FWPShowMessageWindow.Width := MsgWidth + MulDiv(FWPRailWindow.ClientWidth, 45, 1000);
    FWPShowMessageLabel.Left := MulDiv(FWPRailWindow.ClientWidth, 5, 1000);

    FWPShowMessageLabel.Caption := Msg;
    IF NOT Visible THEN
      Show;
  END; {WITH}
END; { FWPShowMessage }

INITIALIZATION

END { FWPShowMessageUnit }.