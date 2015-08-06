UNIT Splash;

INTERFACE

USES
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

TYPE
  TSplashForm = CLASS(TForm)
    SplashCopyrightLabel: TLabel;
    SplashImage: TImage;
    SplashPanel: TPanel;
    SplashRailwayProgramLabel: TLabel;
    SplashRightsReservedLabel: TLabel;
    PROCEDURE SplashPanelCreate(Sender: TObject);
  END;

VAR
  SplashForm: TSplashForm;

PROCEDURE HideSplashForm;
{ Allows the main program to hide the splash form once it's no longer needed }

FUNCTION IsSplashFormVisible : Boolean;
{ Returns whether or not the splash form is visible }

IMPLEMENTATION

{$R *.dfm}

PROCEDURE TSplashForm.SplashPanelCreate(Sender: TObject);
BEGIN
  SplashCopyrightLabel.Caption := 'Copyright © 1988-2015 F.W. Pritchard && A.M. Stokes';
END;

PROCEDURE HideSplashForm;
{ Allows the main program to hide the splash form once it's no longer needed }
BEGIN
  SplashForm.Hide;
END;

FUNCTION IsSplashFormVisible : Boolean;
{ Returns whether or not the splash form is visible }
BEGIN
  IF (SplashForm <> NIL) AND (SplashForm.Visible) THEN
    Result := True
  ELSE
    Result := False;
END;

END { Splash }.
