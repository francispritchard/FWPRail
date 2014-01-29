UNIT Splash;

INTERFACE

USES
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

TYPE
  TSplashForm = CLASS(TForm)
    SplashPanel: TPanel;
    SplashImage: TImage;
    SplashRailwayProgramLabel: TLabel;
    SplashCopyrightLabel: TLabel;
    SplashRightsReservedLabel: TLabel;
    PROCEDURE SplashPanelCreate(Sender: TObject);
  END;

VAR
  SplashForm: TSplashForm;

IMPLEMENTATION

{$R *.dfm}

PROCEDURE TSplashForm.SplashPanelCreate(Sender: TObject);
BEGIN
  SplashCopyrightLabel.Caption := 'Copyright © 1998-2014 F.W. Pritchard && A.M. Stokes';
END;

END { Splash }.
