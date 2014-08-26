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
  SaveProcedureOrFunctionNameStr : String = '';

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

PROCEDURE LookForComments(Filename : String);
VAR
  Buf : String;
  FunctionLine : Boolean;
  I : Integer;
  InitialisationLine : Boolean;
  LineCount : Integer;
  OverloadBracketPos : Integer;
  ProcedureLine : Boolean;
  TempStr : String;

BEGIN
  LineCount := 0;;
  FunctionLine := False;
  InitialisationLine := False;
  ProcedureLine := False;

  WHILE NOT EoF(InputFile) DO BEGIN
    ReadLn(InputFile, Buf);
    Inc(LineCount);

    { Look for procedure/function ends without matching procedure/function names as comments }
    IF Pos('PROCEDURE', Buf) = 1 THEN BEGIN
      ProcedureLine := True;
      SaveProcedureOrFunctionNameStr := GetFollowingChars(Buf, 'PROCEDURE ', '(');
      IF Copy(SaveProcedureOrFunctionNameStr, Length(SaveProcedureOrFunctionNameStr)) = ';' THEN
        SaveProcedureOrFunctionNameStr := Copy(SaveProcedureOrFunctionNameStr, 1, Length(SaveProcedureOrFunctionNameStr) - 1);
    END ELSE
      IF Pos('FUNCTION', Buf) = 1 THEN BEGIN
        FunctionLine := True;
        SaveProcedureOrFunctionNameStr := GetFollowingChars(Buf, 'FUNCTION', '(');
        IF Pos(':', SaveProcedureOrFunctionNameStr) > 0 THEN
          SaveProcedureOrFunctionNameStr := GetFollowingChars(Buf, 'FUNCTION', ':');
      END ELSE
        IF Pos('INITIALIZATION', Buf) = 1 THEN BEGIN
          SaveProcedureOrFunctionNameStr := 'Initialization';
          InitialisationLine := True;
        END;

    IF Pos('END;', Buf) = 1 THEN BEGIN
      (* Change {1} etc to -1 as that is how we do overloading *)
      FOR I := 1 TO 9 DO BEGIN
        OverloadBracketPos := Pos('{' + IntToStr(I) + '}', SaveProcedureOrFunctionNameStr);
        IF OverloadBracketPos > 0 THEN
          SaveProcedureOrFunctionNameStr := Copy(SaveProcedureOrFunctionNameStr, 1, OverloadBracketPos - 1) + '-' + IntToStr(I);
      END; {FOR}

      IF Pos('{', Buf) <> 6 THEN
        WriteLn(OutputFile, Filename  + ' (' + IntToStr(LineCount) + ') No name follows "END;" at ' + Buf)
      ELSE BEGIN
        TempStr := GetFollowingChars(Buf, 'END; { ', '}');
        IF TempStr = '' THEN
          WriteLn(OutputFile, Filename  + ' (' + IntToStr(LineCount) + '): No name follows "END;" at ' + Buf)
        ELSE BEGIN
          { if there's a dot in it, it's probably preceded by the Class name so omit it }
          IF Pos('.', SaveProcedureOrFunctionNameStr) > 0 THEN
            SaveProcedureOrFunctionNameStr := GetFollowingChars(SaveProcedureOrFunctionNameStr, '.', '');

          IF Trim(TempStr) <> Trim(SaveProcedureOrFunctionNameStr) THEN
              WriteLn(OutputFile, Filename + ' (' + IntToStr(LineCount) + ') '
                                           + IfThen(ProcedureLine, 'Procedure')
                                           + IfThen(FunctionLine, 'Function')
                                           + IfThen(InitialisationLine, 'Initialization')
                                           + ' "' + Trim(SaveProcedureOrFunctionNameStr) + '" does not match end of routine name "' + Trim(TempStr) + '"');
        END;
      END;
      FunctionLine := False;
      InitialisationLine := False;
      ProcedureLine := False;
    END;

    IF Length(Buf) > 172 THEN
      WriteLn(OutputFile, Filename + ' (' + IntToStr(LineCount) + ') > 172: "' + Buf + '"');
  END;
END;

PROCEDURE CheckAllFiles;
VAR
  SearchRec: TSearchRec;

BEGIN
  Debug('Beginning check...');
  OpenOutputFile('C:\Doc\Google Drive\RAD Studio\Projects\Rail\0Rail.syntaxcheck');
  IF FindFirst('C:\Doc\Google Drive\RAD Studio\Projects\Rail\*.pas', FaAnyFile, SearchRec) = 0 THEN BEGIN
    REPEAT
      { If SearchRec = . OR .. then skip to next iteration }
      IF (SearchRec.Name =  '.') OR (SearchRec.Name =  '..') OR DirectoryExists(SearchRec.Name) THEN
        Continue;

      OpenInputFile('C:\Doc\Google Drive\RAD Studio\Projects\Rail\' + SearchRec.Name);
      LookForComments(SearchRec.Name);
      CloseFile(InputFile);

    { Loop until no more files are found }
    UNTIL FindNext(SearchRec) <> 0;
  END;

  CloseFile(OutputFile);
  Debug('Check completed');
END; { CheckAllFiles; }

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
  CheckAllFiles;
END; { TestProc }

INITIALIZATION

FINALIZATION

END { TestUnit }.

