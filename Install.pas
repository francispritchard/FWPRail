UNIT Install;
{ Used only when installing or re-installing FWPRail

  Copyright © F.W. Pritchard 2014. All Rights Reserved.

  v0.1  09/12/14 First written
}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

TYPE
  TInstallForm = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  InstallForm: TInstallForm;

IMPLEMENTATION

{$R *.dfm}

USES Initvars, MiscUtils, Options;

PROCEDURE GetInitialisationData;
VAR
  InstallOK : Boolean;
  PermissibleStrings : StringArrayType;

BEGIN
  InstallOk := True;
  SetLength(PermissibleStrings, 0);

  IF NOT GetStringDataFromUser('Enter Path To Rail Data Files', '', PermissibleStrings, PathToRailDataFiles) THEN
    ShowMessage('No data file path supplied - install cancelled');

  IF NOT GetStringDataFromUser('Enter Path To Rail Source Files', '', PermissibleStrings, PathToRailSourceFiles) THEN
    ShowMessage('No source file path supplied - install cancelled');

  IF NOT GetStringDataFromUser('Enter Path To Log Files', '', PermissibleStrings, PathToLogFiles) THEN
    ShowMessage('No log file supplied - install cancelled');
END; { GetInitialisationData; }

END { Install }.
