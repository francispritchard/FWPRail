UNIT TestUnit;
{ Used for testing stuff }

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, IdTCPClient, Web.Win.Sockets, Vcl.Grids;

TYPE
  TTestUnitForm = CLASS(TForm)
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
     Options, FWPShowMessageUnit, Math {sic}, Cuneo, Main;

CONST
  UnitRef = 'TestUnit';

VAR
  RepeatCount : integer = 0;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }


(* ******************************************************************** *)

{ Syntax Check stuff: }
VAR
  InputFile, OutputFile : Text;

PROCEDURE OpenInputFile(InputFileName : String);
VAR
  ErrorMsg : String;

BEGIN
  OpenInputFileOK(InputFile, InputFileName, ErrorMsg);
  Reset(InputFile);
END; { OpenFiles }

PROCEDURE OpenOutputFile(OutputFileName : String);
BEGIN
  AssignFile(OutputFile, OutputFileName);
  IF FileExists(OutputFileName) THEN
    Erase(OutputFile);
  { create it so we can write to it when we exit }
  Rewrite(OutputFile);
END; { OpenFiles }

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

PROCEDURE Test3;
//VAR
BEGIN
  //
END; { Test3 }

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
//  TestUnitForm.Show;
//  TestProc3;
END; { TestProc }

INITIALIZATION

FINALIZATION

END { TestUnit }.

