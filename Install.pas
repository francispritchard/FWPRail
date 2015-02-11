UNIT Install;
{ Used only when installing or re-installing FWPRail

  Copyright © F.W. Pritchard 2014-15. All Rights Reserved.

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

FUNCTION GetStringDataFromUser(Prompt, DefaultStr : String; PermissibleStrings : StringArrayType; OUT NewString : String) : Boolean;
{ Makes sure a non-empty string is returned by the user, and optionally that it is matches one of the strings supplied }
CONST
  Caption = '';

VAR
  I : Integer;
  OK : Boolean;
  PermissibleStringFound : Boolean;

BEGIN
  REPEAT
    IF NOT InputQuery(Caption, Prompt, NewString) THEN BEGIN
      OK := True;
      Result := False;
    END ELSE BEGIN
      Result := True;
      OK := False;

      IF NewString = '' THEN
        ShowMessage('Empty string - please try again')
      ELSE
        IF Length(PermissibleStrings) = 0 THEN
          OK := True
        ELSE BEGIN
          I := 0;
          PermissibleStringFound := False;

          WHILE (I <= High(PermissibleStrings)) AND NOT PermissibleStringFound DO BEGIN
            IF PermissibleStrings[I] = NewString THEN
              PermissibleStringFound := True
            ELSE
              Inc(I);
          END;

          IF NOT PermissibleStringFound THEN
            ShowMessage('"' + NewString + '" does not match the list of permissible strings')
          ELSE
            OK := True;
        END;
    END;
  UNTIL OK;
END; { GetStringDataFromUser }

PROCEDURE GetInitialisationData;
VAR
  InstallOK : Boolean;
  PermissibleStrings : StringArrayType;

BEGIN
  InstallOk := True;
  SetLength(PermissibleStrings, 0);

  IF NOT GetStringDataFromUser('Enter Path To Rail Data Files', '', PermissibleStrings, PathToRailDataFiles) THEN BEGIN
    ShowMessage('No data file path supplied - install cancelled');
    InstallOK := False;
  END;

  IF InstallOK THEN BEGIN
    IF NOT GetStringDataFromUser('Enter Path To Rail Source Files', '', PermissibleStrings, PathToRailSourceFiles) THEN BEGIN
      ShowMessage('No source file path supplied - install cancelled');
      InstallOK := False;
    END;
  END;

  IF InstallOk THEN BEGIN
    IF NOT GetStringDataFromUser('Enter Path To Log Files', '', PermissibleStrings, PathToLogFiles) THEN BEGIN
      ShowMessage('No log file supplied - install cancelled');
      InstallOK := False;
    END;
  END;

  IF InstallOK THEN BEGIN

  END;
END; { GetInitialisationData; }

END { Install }.
