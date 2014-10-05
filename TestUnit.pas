UNIT TestUnit;
{ Used for testing stuff }

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, IdTCPClient, Web.Win.Sockets, Vcl.Grids,
  Data.DB, Data.Win.ADODB;

TYPE
  TTestUnitForm = CLASS(TForm)
    TrackCircuitsADOTable: TADOTable;
    TrackCircuitsADOConnection: TADOConnection;
    TrackCircuitDataSource: TDataSource;
    procedure TrackCircuitsADOTableBeforeDelete(DataSet: TDataSet);
    procedure TrackCircuitsADOTableAfterDelete(DataSet: TDataSet);
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
  NewTrackCircuits : ARRAY OF TrackCircuitRec;
  RepeatCount : integer = 0;

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

PROCEDURE NewReadInTrackCircuitDataFromDatabase;
{ Initialise the track circuit data which depends on lines being initialised first.

  Interesting notes from the past here:

  Unit 77 inputs 5-8 free
  Unit 78 (tunnel) for UMC
  Unit 86 input 5 does not have a corresponding LB101 as 6-8 are photocell inputs
  Unit 94 input 6 reserved for new siding point (1 to 5 are points already)
  Unit 112 input 8 (FY) is free [TC input]

  Unit 93 inputs 2-3 reserved for new siding A
  Unit 95 inputs 5-6 reserved for new siding B
  Unit 99 inputs 3-4 reseved for cross over points from platforms 5 to 6
  Unit 116 input 2 free [point input]
}
CONST
  StopTimer = True;

VAR
  ErrorMsg : String;
  TC : Integer;
  TempStr : String;

BEGIN
  TRY
    Log('A INITIALISING TRACK CIRCUITS {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track Circuit database file "' + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInTrackCircuitDataFromDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;
      Log('T Track circuit data table and connection opened to initialise the trackCircuits');

      TrackCircuitsADOTable.Sort := '[' + TC_NumberFieldName + '] ASC';
      TrackCircuitsADOTable.First;
      SetLength(NewTrackCircuits, 0);

      WHILE NOT TrackCircuitsADOTable.EOF DO BEGIN
        WITH TrackCircuitsADOTable DO BEGIN
          SetLength(NewTrackCircuits, Length(NewTrackCircuits) + 1);
          TC := High(NewTrackCircuits);
          WITH NewTrackCircuits[TC] DO BEGIN
            ErrorMsg := '';

            TC_Number := TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsInteger;
            IF TC_Number <> TC THEN
              ErrorMsg := 'it does not match the line number in the database (' + IntToStr(TC_Number) + ')';

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(TC_LengthFieldName).AsString;
              IF TempStr = '' THEN
                TC_LengthInInches := 0
              ELSE
                IF NOT TryStrToFloat(TempStr, TC_LengthInInches) THEN
                  Debug('TC=' + IntToStr(TC_Number) + ': invalid length String "' + TempStr + '"');
            END;

            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating TC=' + IntToStr(High(NewTrackCircuits)) + ' (' + IntToStr(TC_Number) + '): '
                                            + '[' + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInTrackCircuitDataFromDatabase');
            END;
          END; {WITH}
        END; {WITH}
        TrackCircuitsADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('T Track circuit data table and connection closed');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInTrackCircuitDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { NewReadInTrackCircuitDataFromDatabase }

PROCEDURE NewWriteOutTrackCircuitDataToDatabase;
{ Write out some track circuit data to the track circuit data file }
VAR
  TC : Integer;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track circuit database file "' + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                      + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'WriteOutTrackCircuitDataFromDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;
      Log('T Track circuit data table and connection opened to write out track circuit data');

      TrackCircuitsADOTable.First;

      FOR TC := 0 TO High(NewTrackCircuits) DO BEGIN
        WITH NewTrackCircuits[TC] DO BEGIN
          IF TC_DataChanged THEN BEGIN
            TC_DataChanged := False;

            IF NOT TrackCircuitsADOTable.Locate(TC_NumberFieldName, IntToStr(TC), []) THEN BEGIN
              Log('T Track circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TC) + ' but it cannot be found');
            END ELSE BEGIN
              Log('T Track circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TC));

              Log('T Recording in track circuit database that TC ' + IntToStr(TC) + ' ' + TC_NumberFieldName + ' is ''' + IntToStr(TC_Number) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsString := IntToStr(TC_Number);
              TrackCircuitsADOTable.Post;

              Log('T Recording in track circuit database that TC ' + IntToStr(TC) + ' ' + TC_LengthFieldName + ' is ''' + FloatToStr(TC_LengthInInches) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_LengthFieldName).AsString := '12345';
              TrackCircuitsADOTable.Post;
            END;
          END;
        END; {WITH}
      END; {FOR}

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('L Track circuit data table and connection closed after writing track circuit data');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutTrackCircuitData: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { NewWriteOutTrackCircuitDataToDatabase }

FUNCTION NewDeleteRecordFromTrackCircuitDatabase(TrackCircuitToDeleteNum : Integer) : Boolean;
{ Remove a record from the trackCircuit database }
BEGIN
  Result := False;
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track circuit database file "' + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                      + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'DeleteRecordFromTrackCircuitDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;

      IF NOT TrackCircuitsADOTable.Locate(TC_NumberFieldName, IntToStr(High(NewTrackCircuits)), []) THEN BEGIN
        Log('T Track circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TrackCircuitToDeleteNum) + ' but it cannot be found');
      END ELSE BEGIN
        Log('T Track circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TrackCircuitToDeleteNum));

        { Now delete the track circuit - we have already checked, in the Edit unit, whether deleting it will cause knock-on problems with other track circuits }

//      TrackCircuitsADOTable.Sort := '[' + TC_NumberFieldName + '] ASC';

//        TrackCircuitsADOTable.FindLast;
  debug(TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsString + ' - ' + TrackCircuitsADOTable.FieldByName(TC_LengthFieldName).AsString);
        TrackCircuitsADOTable.Delete;
        Log('TG TrackCircuit ' + IntToStr(TrackCircuitToDeleteNum) + ' has been deleted');
        Result := True;
      END;

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('T Track circuit Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToTrackCircuitDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { NewDeleteRecordFromTrackCircuitDatabase }

PROCEDURE TestProc(OUT KeyOK : Boolean);
{ Used to call whatever routine we are testing. Set KeyOK to false if not in use. }
BEGIN
  KeyOK := False;

//  NewReadInTrackCircuitDataFromDatabase;
//
//  NewNewTrackCircuits[5].TC_LengthInInches := 12345;
//  NewNewTrackCircuits[5].TC_DataChanged := True;
//
//  NewWriteOutTrackCircuitDataToDatabase;
//
//  NewDeleteRecordFromTrackCircuitDatabase(2);

  NewReadInTrackCircuitDataFromDatabase;
  NewTrackCircuits[5] := NewTrackCircuits[High(NewTrackCircuits)];
  NewTrackCircuits[5].TC_Number := 5;
  NewTrackCircuits[5].TC_DataChanged := True;

  NewWriteOutTrackCircuitDataToDatabase;

  NewDeleteRecordFromTrackCircuitDatabase(High(NewTrackCircuits));
  SetLength(NewTrackCircuits, High(NewTrackCircuits) - 1);
//  TestProc2;

//  KeyOK := True;
END; { TestProc }

procedure TTestUnitForm.TrackCircuitsADOTableAfterDelete(DataSet: TDataSet);
begin
  debug('after deleting');
  debug(TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsString + ' - ' + TrackCircuitsADOTable.FieldByName(TC_LengthFieldName).AsString);
end;

procedure TTestUnitForm.TrackCircuitsADOTableBeforeDelete(DataSet: TDataSet);
begin
  debug('before deleting');
  debug(TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsString + ' - ' + TrackCircuitsADOTable.FieldByName(TC_LengthFieldName).AsString);
end;

INITIALIZATION

FINALIZATION

END { TestUnit }.

