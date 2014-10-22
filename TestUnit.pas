UNIT TestUnit;
{ Used for testing stuff }

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, IdTCPClient, Web.Win.Sockets, Vcl.Grids, Data.DB, Data.Win.ADODB,
     Vcl.ExtCtrls;

TYPE
  TTestUnitForm = CLASS(TForm)
    TestUnitTimer: TTimer;
    procedure TestUnitTimerTick(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  TestUnitForm: TTestUnitForm;

PROCEDURE TestProc(OUT KeyOK : Boolean);
{ Used to call whatever routine we are testing. Set KeyOK to false if not in use. }

IMPLEMENTATION

{$R *.dfm}

USES InitVars, RailDraw, MiscUtils, Feedback, DateUtils, StrUtils, Diagrams, Lenz, StationMonitors, CreateRoute, GetTime, MMSystem, Registry, LocationData, Types, Locks,
     Options, FWPShowMessageUnit, Math {sic}, Cuneo, Main, Edit;

CONST
  UnitRef = 'TestUnit';

VAR
  RepeatCount : integer = 0;
  SaveEditedLine : Integer = UnknownLine;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE WriteStuff;
{ Test procedure that writes stuff out }
CONST
  AppendToFile = True;

VAR
  ErrorMsg : String;
  Stuff : String;
  TempFile : Text;
  TempFilename : String;

BEGIN
  TempFilename := 'c:\railtempfile.csv'; { add to .ini **** }
  OpenOutputFileOK(TempFile, TempFilename, ErrorMsg, NOT AppendToFile);
  WriteLn(TempFile, Stuff);
  CloseFile(TempFile);
END; { WriteStuff }

(* ******************************************************************** *)

procedure TTestUnitForm.TestUnitTimerTick(Sender: TObject);
begin
  IF EditedLine <> SaveEditedLine THEN BEGIN
    WriteToStatusBarPanel(StatusBarPanel3, 'EditedLine=' + LineToStr(EditedLine));
    SaveEditedLine := SaveEditedLine;
  END;
end;

PROCEDURE TestProc4();
BEGIN
END; { TestProc4 }

PROCEDURE TestProc3;
// VAR

BEGIN
END; { TestProc3 }

PROCEDURE TestProc2;
BEGIN
END; { TestProc2 }

PROCEDURE TestProc(OUT KeyOK : Boolean);
{ Used to call whatever routine we are testing. Set KeyOK to false if not in use. }
BEGIN
  KeyOK := False;

  TestUnitForm.TestUnitTimer.Interval := 100;
  TestUnitForm.TestUnitTimer.Enabled := True;

//  TestProc2;

//  KeyOK := True;
END; { TestProc }



INITIALIZATION

FINALIZATION

END { TestUnit }.

