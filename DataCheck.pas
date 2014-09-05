UNIT DataCheck;
{ Formatting-check and data-validation stuff

  v.0.1 28/08/14 First written
}

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

TYPE
  TDataCheckForm = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE CompareTwoLineDatabases(Line1DataFilename, Line1DataFilenameSuffix, Line2DataFilename, Line2DataFilenameSuffix : String);
{ Compare two line databases - used for testing }

PROCEDURE CompareTwoPointDatabases(Point1DataFilename, Point1DataFilenameSuffix, Point2DataFilename, Point2DataFilenameSuffix : String);
{ Compare two point databases - used for testing }

PROCEDURE CompareTwoSignalDatabases(Signal1DataFilename, Signal1DataFilenameSuffix, Signal2DataFilename, Signal2DataFilenameSuffix : String);
{ Compare two signal databases - used for testing }

PROCEDURE DoGeneralCheck;
{ Debug check for various things including two different feedback units serving the same track circuit }

PROCEDURE FormatCheckAllFiles;
{ Checks all .pas files for formatting errors }

VAR
  DataCheckForm: TDataCheckForm;

IMPLEMENTATION

{$R *.dfm}

USES MiscUtils, StrUtils, InitVars, ADODB, Options, Raildraw, Feedback, CreateRoute;

CONST
  UnitRef = 'DataCheck';

VAR
  InputFile, OutputFile : Text;
  SaveProcedureOrFunctionNameStr : String = '';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE DoGeneralCheck;
{ Debug check for various things including two different feedback units serving the same track circuit }
CONST
  IndicatorToBeSet = True;
  SuppressMessage = True;
  UpLine = True;

VAR
  ErrorCount : Integer;
  FeedbackData : FeedbackRec;
  FeedbackNum1 : Integer;
  FeedbackNumFound : Boolean;
  FeedbackPoint : Integer;
  FeedbackType : TypeOfFeedBackType;
  I, J : Integer;
  L : Integer;
  LenzPointNum : Integer;
  LenzPointNumFound : Boolean;
  LenzPointNumOutOfUse : Boolean;
  P : Integer;
  PointCount : Integer;
  S : Integer;
  SaveLine : Integer;
  SaveTypeOfLine : TypeOfLine;
  TC : Integer;
  TCAboveFeedbackUnit : Integer;
  TCHasFeedback : Boolean;
  TCInUse : Boolean;
  TempDraftRouteArray : StringArrayType;
  TempLinesNotAvailableStr : String;

BEGIN
  DrawLineInLogFile(UnknownLocoChipAsZeroesStr, 'X', '=', UnitRef);
  Log('X G E N E R A L_____C H E C K  {NOUNITREF}');
  DrawLineInLogFile(UnknownLocoChipAsZeroesStr, 'X', '-', UnitRef);

  WriteToStatusBarPanel(StatusBarPanel2, 'Feedback check');
  ErrorCount := 0;
  Debug('Commencing feedback unit check - please wait...');
  FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
    FeedbackData.Feedback_Unit := I;
    ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum1);
    IF FeedbackType = FeedbackDetectorOutOfUse THEN
      Log('XG Feedback unit ' + IntToStr(FeedbackData.Feedback_Unit) + ' is marked as out of use');
  END;

  IF ErrorCount > 0 THEN
    Debug('Feedback check completed - report in LogFile with header ''FFFF'' includes ' + IntToStr(ErrorCount) + ' errors found')
  ELSE
    Log('XG Feedback check completed - no feedback units serving more than one track circuit');

  { Also check for line sections track circuit numbers unused }
  ErrorCount := 0;
  Debug('Commencing track circuit check - please wait...');
  FOR TC := 0 TO High(TrackCircuits) DO BEGIN
    TCInUse := False;
    TCHasFeedback := False;

    { see if the track circuit number is in use }
    FOR L := 0 TO High(Lines) DO BEGIN
      IF Lines[L].Line_TC = TC THEN
        TCInUse := True;
    END;

    { now see if the track circuit has feedback }
    FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
      FOR J := 1 TO 8 DO BEGIN
        FeedbackData.Feedback_Unit := I;
        FeedbackData.Feedback_Input := J;
        ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum1);
        IF FeedbackType = TrackCircuitFeedbackDetector THEN BEGIN
          IF FeedbackNum1 = TC THEN
            TCHasFeedback := True;
        END;
      END;
    END;

      IF NOT TCInUse AND NOT TCHasFeedback THEN BEGIN
        Inc(ErrorCount);
        Log('X TC=' + IntToStr(TC) + ' not in use (and has no feedback)');
      END ELSE
        IF NOT TCInUse THEN BEGIN
          Inc(ErrorCount);
          Log('X TC=' + IntToStr(TC) + ' not in use');
        END ELSE
          IF NOT TCHasFeedback THEN BEGIN
            Inc(ErrorCount);
            Log('X TC=' + IntToStr(TC) + ' has no feedback');
          END;
  END;

  IF ErrorCount > 0 THEN
    Debug('Trackcircuit check completed - ' + IntToStr(ErrorCount) + ' errors noted in LogFile')
  ELSE
    Log('XG Trackcircuit check completed - all feedback inputs accounted for');

  { Also check for track circuit feedback inputs unused }
  ErrorCount := 0;
  Debug('Commencing track circuit feedback input check - please wait...');
  FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
    FOR J := 1 TO 8 DO BEGIN
      FeedbackData.Feedback_Unit := I;
      FeedbackData.Feedback_Input := J;
      ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum1);
      IF FeedbackType = TrackCircuitFeedbackDetector THEN BEGIN
        FeedbackNumFound := False;
        FOR TC := 0 TO High(TrackCircuits) DO BEGIN
          IF TC = FeedbackNum1 THEN BEGIN
            FeedbackNumFound := True;

            IF TrackCircuits[TC].TC_OccupationState = TCOutOfUseSetByUser THEN
              Log('X Trackcircuit feedback unit ' + IntToStr(I) + ' input no. ' + IntToStr(J) + ' not in use as TC=' + IntToStr(TC) + ' is marked as being set out of use');
          END;
        END;

        IF NOT FeedbackNumFound THEN BEGIN
          Log('X Trackcircuit feedback unit ' + IntToStr(I) + ' input no. ' + IntToStr(J) + ' not in use ');
          Inc(ErrorCount);
        END;
      END;
    END;
  END;

  IF ErrorCount > 0 THEN
    Debug('Trackcircuit feedback check completed - ' + IntToStr(ErrorCount) + ' errors noted in LogFile')
  ELSE BEGIN
    Log('XG Trackcircuit feedback check completed - all feedback inputs accounted for');
  END;

  { Debug point data }
  ErrorCount := 0;
  Debug('Commencing point feedback check - please wait...');
  { now see if the point has feedback }
  FOR P := 0 TO High(Points) DO BEGIN
    IF NOT Points[P].Point_HasFeedback THEN BEGIN
      Inc(ErrorCount);
      Log('X Point ' + IntToStr(P) + ' has no feedback');
    END;
  END; {FOR}

  IF ErrorCount > 0 THEN
    Debug('Point check completed - ' + IntToStr(ErrorCount) + ' errors noted in LogFile')
  ELSE BEGIN
    Debug('Point check completed - all feedback inputs accounted for');
    Log('X Point check completed - all feedback inputs accounted for');
  END;

  { Also check for point feedback inputs unused }
  ErrorCount := 0;
  Debug('Commencing point feedback input check - please wait...');
  FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
    FOR J := 1 TO 8 DO BEGIN
      FeedbackData.Feedback_Unit := I;
      FeedbackData.Feedback_Input := J;
      ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum1);
      IF FeedbackType = PointFeedbackDetector THEN BEGIN
        FeedbackNumFound := False;
        FeedbackPoint := UnknownPoint;
        P := 0;
        WHILE (P <= High(Points)) AND NOT FeedbackNumFound DO BEGIN
          IF (Points[P].Point_FeedbackUnit = I) AND (Points[P].Point_FeedbackInput = J) THEN BEGIN
            FeedbackNumFound := True;
            FeedbackPoint := P;
          END;

          Inc(P);
        END; {FOR}

        IF NOT FeedbackNumFound THEN BEGIN
          Log('X Point feedback unit ' + IntToStr(I) + ' input no. ' + IntToStr(J) + ' not used');
          Inc(ErrorCount);
        END ELSE
          IF (FeedbackPoint <> UnknownPoint) AND Points[FeedbackPoint].Point_OutOfUse THEN
            Log('X Point feedback unit ' + IntToStr(I) + ' input no. ' + IntToStr(J) + ' not in use as point is marked out of use');
      END;
    END; {FOR}
  END; {FOR}

  IF ErrorCount > 0 THEN
    Debug('Point feedback unit check completed - ' + IntToStr(ErrorCount) + ' errors noted in LogFile')
  ELSE
    Log('X Point feedback unit check completed - all feedback inputs in use');

  ErrorCount := 0;
  { See if the same feedback unit is attached to two different points }
  FOR I := 0 TO High(Points) DO BEGIN
    FOR J := 0 TO High(Points) DO BEGIN
      IF (I <> J)
      AND (Points[I].Point_FeedbackUnit <> 0)
      AND (Points[I].Point_FeedbackInput <> 0)
      AND (Points[I].Point_FeedbackUnit = Points[J].Point_FeedbackUnit)
      AND (Points[I].Point_FeedbackInput = Points[J].Point_FeedbackInput)
      THEN BEGIN
        Log('X Point ' + IntToStr(I) + ' and point ' + IntToStr(J) + ' have the same feedback input');
        Inc(ErrorCount);
      END;
    END; {FOR}
  END; {FOR}

  IF ErrorCount > 0 THEN
    Debug('Point feedback check completed - ' + IntToStr(ErrorCount) + ' errors noted in LogFile')
  ELSE
    Log('X Point feedback check completed - all feedback inputs accounted for');

  { And show which points are not installed (by looking at the Lenz point numbers) }

  { Need to count down to find the last Lenz point num }
  ErrorCount := 0;
  I := 1000;
  PointCount := 0;
  WHILE (I > 0) AND (PointCount = 0) DO BEGIN
    J := 0;
    WHILE J <= High(Points) DO BEGIN
      IF Points[J].Point_LenzNum = I THEN
        PointCount := I;
      Inc(J);
    END; {WHILE}
    Dec(I);
  END; {WHILE}

  { Now record missing ones }
  FOR I := 1 TO PointCount DO BEGIN
    LenzPointNumFound := False;
    LenzPointNumOutOfUse := False;
    J := 0;
    LenzPointNum := UnknownPoint;
    WHILE J <= High(Points) DO BEGIN
      IF Points[J].Point_LenzNum = I THEN BEGIN
        LenzPointNum := J;
        LenzPointNumFound := True;
        IF Points[J].Point_OutOfUse THEN
          LenzPointNumOutOfUse := True;
      END;
      Inc(J);
    END; {WHILE}

    IF NOT LenzPointNumFound THEN BEGIN
      Log('X No Lenz point num ' + IntToStr(I));
      Inc(ErrorCount);
    END ELSE
      IF LenzPointNumOutOfUse THEN
        Log('X Lenz point num ' + IntToStr(I) + ' marked as out of use'
               + ' [Unit ' + IntToStr(Points[LenzPointNum].Point_LenzUnit) + ' (' + Points[LenzPointNum].Point_LenzUnitType + ')]');
  END;

  IF ErrorCount > 0 THEN
    Debug('Missing Lenz point numbers found - ' + IntToStr(ErrorCount) + ' errors noted in LogFile')
  ELSE
    Log('XG All Lenz point numbers accounted for');

  { Show any signals which do not have resetting track circuits }
  Debug('Checking for signal resetting track circuits...');
  FOR S := 0 TO High(Signals) DO BEGIN
    IF Signals[S].Signal_Type = SemaphoreDistant THEN
      { semaphore distants don't have resetting track circuits }
      Log('S [Note: Semaphore Distant S=' + IntToStr(S) + ' cannot have a resetting track circuit]')
    ELSE BEGIN
      IF NOT FindNextSignalOrBufferStop(S, UnknownSignal, UnknownBufferStop, NOT IndicatorToBeSet, TempLinesNotAvailableStr, TempDraftRouteArray) THEN BEGIN
        IF NOT Signals[S].Signal_OutOfUse THEN
          Log('S No resetting TC for S=' + IntToStr(S) + ' [' + LineToStr(Signals[S].Signal_AdjacentLine) + '] found as no next signal or bufferstop found')
        ELSE
          Log('S No resetting TC for S=' + IntToStr(S) + ' [' + LineToStr(Signals[S].Signal_AdjacentLine) + '] found as no next signal or bufferstop found but signal is '
                 + 'marked as out of use');
      END ELSE BEGIN
        CreateLockingArrayFromDraftRouteArray(UnknownLocoChipStr, TempDraftRouteArray, Signals[S].Signal_RouteLockingNeededArray);
        TC := GetResettingTrackCircuit(UnknownLocoChipStr, S, SuppressMessage);
        IF TC = UnknownTrackCircuit THEN
          Log('S No resetting TC for S=' + IntToStr(S) + ' [' + LineToStr(Signals[S].Signal_AdjacentLine) + '] found');
      END;
    END;
  END; {FOR}

  { See if any track circuits have different line characteristics, i.e. TC=126 occuping both UFYU5 (classified as a main/goods line) and FYX5 (classified as a fiddleyard).
    This causes a problem in terms of speeds for track circuits.
  }
  SaveLine := UnknownLine;
  Debug('Checking track circuit line types...');
  FOR TC := 0 TO High(TrackCircuits) DO BEGIN
    SaveTypeOfLine := UnknownTypeOfLine;
    FOR L := 0 TO High(Lines) DO BEGIN
      IF Lines[L].Line_TC = TC THEN
        IF SaveTypeOfLine = UnknownTypeOfLine THEN BEGIN
          SaveLine := L;
          SaveTypeOfLine := Lines[L].Line_TypeOfLine;
        END ELSE
          IF Lines[L].Line_TypeOfLine <> SaveTypeOfLine THEN
            Log('X TC=' + IntToStr(TC) + ' has line type of ' + TypeOfLineToStr(Lines[L].Line_TypeOfLine) + ' at ' + LineToStr(L)
                   + ' but line type of ' + TypeOfLineToStr(SaveTypeOfLine) + ' at ' + LineToStr(SaveLine))
          ELSE
            SaveTypeOfLine := Lines[L].Line_TypeOfLine;
    END; {FOR}
  END; {FOR}

  Debug('End of check');

  DrawLineInLogFile(UnknownLocoChipAsZeroesStr, 'X', '-', UnitRef);
  Log('X E N D_____OF_____G E N E R A L_____C H E C K  {NOUNITREF}');
  DrawLineInLogFile(UnknownLocoChipAsZeroesStr, 'X', '=', UnitRef);
END; { DoGeneralCheck }

PROCEDURE CompareTwoLineDatabases(Line1DataFilename, Line1DataFilenameSuffix, Line2DataFilename, Line2DataFilenameSuffix : String);
{ Compare two line databases - used for testing }
CONST
  StopTimer = True;

VAR
  ErrorFound : Boolean;
  Line : Integer;

  PROCEDURE CheckString(FieldName : String; Table2, Table1 : TADOTable; VAR ErrorFound : Boolean);
  BEGIN
    IF UpperCase(Table1.FieldByName(FieldName).AsString) <> UpperCase(Table2.FieldByName(FieldName).AsString) THEN BEGIN
      IF NOT ErrorFound THEN BEGIN
        Log('XG Differences found in line databases '
                + '"' + Line2DataFilename + '.' + Line2DataFilenameSuffix + '" and "' + Line1DataFilename + '.' + Line1DataFilenameSuffix + '"');
        ErrorFound := True;
      END;
      Log('XG P=' + IntToStr(Line) + ' ' + FieldName + ': "' + Table1.FieldByName(FieldName).AsString + '" to "' + Table2.FieldByName(FieldName).AsString + '"');
    END;
  END; { CheckString }

  PROCEDURE CheckBoolean(FieldName : String; Table2, Table1 : TADOTable; VAR ErrorFound : Boolean);
  BEGIN
    IF Table1.FieldByName(FieldName).AsBoolean <> Table2.FieldByName(FieldName).AsBoolean THEN BEGIN
      IF NOT ErrorFound THEN BEGIN
        Log('XG Differences found in line databases '
                + '"' + Line2DataFilename + '.' + Line2DataFilenameSuffix + '" and "' + Line1DataFilename + '.' + Line1DataFilenameSuffix + '"');
        ErrorFound := True;
      END;
      Log('XG P=' + IntToStr(Line) + ' ' + FieldName
              + ': "' + BoolToStr(Table1.FieldByName(FieldName).AsBoolean, True) + '" to "' + BoolToStr(Table2.FieldByName(FieldName).AsBoolean, True) + '"');
    END;
  END; { CheckBoolean }

BEGIN
  TRY
    Log('A LINE DATABASES COMPARISON {BlankLineBefore}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + Line1DataFilename + '.' + Line1DataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Line database file "' + PathToRailDataFiles + Line1DataFilename + '.' + Line1DataFilenameSuffix + '"'
                                      + ' cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'CompareTwoLineDatabases')
        ELSE
          Exit;
      END;

      LinesADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                             + PathToRailDataFiles + Line1DataFilename + '.' + Line1DataFilenameSuffix
                                             + ';Persist Security Info=False';
      TRY
        LinesADOConnection.Connected := True;
      EXCEPT
        ON E:Exception DO
          Log('EG CompareTwoLineDatabases 1: ' + E.ClassName + ' error raised, with message: '+ E.Message);
      END; {TRY}

      LinesADOTable.Open;

      LinesADOConnection2.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                              + PathToRailDataFiles + Line2DataFilename + '.' + Line2DataFilenameSuffix
                                              + ';Persist Security Info=False';
      TRY
        LinesADOConnection2.Connected := True;
      EXCEPT
        ON E:Exception DO
          Log('EG CompareTwoLineDatabases 2: ' + E.ClassName + ' error raised, with message: '+ E.Message);
      END; {TRY}

      LinesADOTable2.Open;

      Log('S Line data table and connection opened to compare Line 1 data with Line 2 data');

      ErrorFound := False;
      Line := -1;
      LinesADOTable.Sort := '[' + Line_NumberFieldName + '] ASC';
      LinesADOTable.First;
      LinesADOTable2.Sort := '[' + Line_NumberFieldName + '] ASC';
      LinesADOTable2.First;

      REPEAT
        Inc(Line);

        IF Line > High(Lines) THEN BEGIN
          IF NOT LinesADOTable.EOF THEN
            Log('XG Last declared line (Line=' + IntToStr(Line - 1) + ') processed but Line database ' + '"' + Line1DataFilename + '.' + Line1DataFilenameSuffix
                    + ' has not yet reached end of file')
          ELSE
          IF NOT LinesADOTable2.EOF THEN
            Log('XG Last declared line (Line=' + IntToStr(Line - 1) + ') processed but Line database ' + '"' + Line2DataFilename + '.' + Line2DataFilenameSuffix
                    + ' has not yet reached end of file');
        END ELSE BEGIN
          WITH Lines[Line] DO BEGIN
            CheckString(Line_BufferStopNumberFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_BufferStopTheatreDestinationStrFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_DirectionFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_DownConnectionChFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_DownRowFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_EndOfLineMarkerFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_GradientFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_InUseFeedbackUnitFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_LengthFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_LocationStrFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_NumberFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckBoolean(Line_OutOfUseFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_NameStrFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_TCFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_TypeOfLineFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_UpConnectionChFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_UpXAbsoluteFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_UpXLineStrFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
            CheckString(Line_UpRowFieldName, LinesADOTable, LinesADOTable2, ErrorFound);
          END; {WITH}
        END;

        IF LinesADOTable.EOF AND NOT LinesADOTable2.EOF THEN BEGIN
          Log('XG Line database ' + '"' + Line1DataFilename + '.' + Line1DataFilenameSuffix
                  + '" is shorter than "' + Line2DataFilename + '.' + Line2DataFilenameSuffix + '"');
          Log('XG A later entry in line database ' + '"' + Line1DataFilename + '.' + Line1DataFilenameSuffix + '" is Line=' + IntToStr(Line));
        END ELSE
          IF NOT LinesADOTable.EOF AND LinesADOTable2.EOF THEN BEGIN
            Log('XG Line database ' + '"' + Line2DataFilename + '.' + Line2DataFilenameSuffix
                    + '" is shorter than "' + Line1DataFilename + '.' + Line1DataFilenameSuffix + '"');
            Log('XG A later entry in line database ' + '"' + Line2DataFilename + '.' + Line2DataFilenameSuffix + '" is Line=' + IntToStr(Line));
          END;

        LinesADOTable2.Next;
        LinesADOTable.Next;
      UNTIL LinesADOTable.EOF AND LinesADOTable2.EOF;

      IF NOT ErrorFound THEN
        Log('XG No differences found in line databases '
                + '"' + Line1DataFilename + '.' + Line1DataFilenameSuffix + '" and "' + Line2DataFilename + '.' + Line2DataFilenameSuffix + '"');

      { Tidy up the database }
      LinesADOTable.Close;
      LinesADOConnection.Connected := False;
      Log('S Line Data 1 table and connection closed');
      LinesADOTable2.Close;
      LinesADOConnection.Connected := False;
      Log('S Line Data 2 table and connection closed');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG InitialiseLines: ' + E.ClassName + 'error raised, with message: '+ E.Message);
  END; {TRY}
END; { CompareTwoLineDatabases }

PROCEDURE CompareTwoPointDatabases(Point1DataFilename, Point1DataFilenameSuffix, Point2DataFilename, Point2DataFilenameSuffix : String);
{ Compare two point databases - used for testing }
CONST
  StopTimer = True;

VAR
  ErrorFound : Boolean;
  P : Integer;

  PROCEDURE CheckString(FieldName : String; Table2, Table1 : TADOTable; VAR ErrorFound : Boolean);
  BEGIN
    IF UpperCase(Table1.FieldByName(FieldName).AsString) <> UpperCase(Table2.FieldByName(FieldName).AsString) THEN BEGIN
      IF NOT ErrorFound THEN BEGIN
        Log('XG Differences found in point databases '
                + '"' + Point2DataFilename + '.' + Point2DataFilenameSuffix + '" and "' + Point1DataFilename + '.' + Point1DataFilenameSuffix + '"');
        ErrorFound := True;
      END;
      Log('XG P=' + IntToStr(P) + ' ' + FieldName + ': "' + Table1.FieldByName(FieldName).AsString + '" to "' + Table2.FieldByName(FieldName).AsString + '"');
    END;
  END; { CheckString }

  PROCEDURE CheckBoolean(FieldName : String; Table2, Table1 : TADOTable; VAR ErrorFound : Boolean);
  BEGIN
    IF Table1.FieldByName(FieldName).AsBoolean <> Table2.FieldByName(FieldName).AsBoolean THEN BEGIN
      IF NOT ErrorFound THEN BEGIN
        Log('XG Differences found in point databases '
                + '"' + Point2DataFilename + '.' + Point2DataFilenameSuffix + '" and "' + Point1DataFilename + '.' + Point1DataFilenameSuffix + '"');
        ErrorFound := True;
      END;
      Log('XG P=' + IntToStr(P) + ' ' + FieldName
              + ': "' + BoolToStr(Table1.FieldByName(FieldName).AsBoolean, True) + '" to "' + BoolToStr(Table2.FieldByName(FieldName).AsBoolean, True) + '"');
    END;
  END; { CheckBoolean }

BEGIN
  TRY
    Log('A POINT DATABASES COMPARISON {BlankLineBefore}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + Point1DataFilename + '.' + Point1DataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Point database file "' + PathToRailDataFiles + Point1DataFilename + '.' + Point1DataFilenameSuffix + '"'
                                      + ' cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'CompareTwoPointDatabases')
        ELSE
          Exit;
      END;

      PointsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                              + PathToRailDataFiles + Point1DataFilename + '.' + Point1DataFilenameSuffix
                                              + ';Persist Security Info=False';
      TRY
        PointsADOConnection.Connected := True;
      EXCEPT
        ON E:Exception DO
          Log('EG CompareTwoPointDatabases 1: ' + E.ClassName + ' error raised, with message: '+ E.Message);
      END; {TRY}

      PointsADOTable.Open;

      PointsADOConnection2.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                               + PathToRailDataFiles + Point2DataFilename + '.' + Point2DataFilenameSuffix
                                               + ';Persist Security Info=False';
      TRY
        PointsADOConnection2.Connected := True;
      EXCEPT
        ON E:Exception DO
          Log('EG CompareTwoPointDatabases 2: ' + E.ClassName + ' error raised, with message: '+ E.Message);
      END; {TRY}

      PointsADOTable2.Open;

      Log('S Point data table and connection opened to compare Point 1 data with Point 2 data');

      ErrorFound := False;
      P := -1;
      PointsADOTable.Sort := '[' + Point_NumberFieldName + '] ASC';
      PointsADOTable.First;
      PointsADOTable2.Sort := '[' + Point_NumberFieldName + '] ASC';
      PointsADOTable2.First;

      REPEAT
        Inc(P);

        IF P > High(Points) THEN BEGIN
          IF NOT PointsADOTable.EOF THEN
            Log('XG Last declared point (P=' + IntToStr(P - 1) + ') processed but point database ' + '"' + Point1DataFilename + '.' + Point1DataFilenameSuffix
                    + ' has not yet reached end of file')
          ELSE
          IF NOT PointsADOTable2.EOF THEN
            Log('XG Last declared point (P=' + IntToStr(P - 1) + ') processed but point database ' + '"' + Point2DataFilename + '.' + Point2DataFilenameSuffix
                    + ' has not yet reached end of file');
        END ELSE BEGIN
          WITH Points[P] DO BEGIN
            CheckString(Point_DivergingLineFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_HeelLineFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_StraightLineFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_LenzNumFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_LenzUnitFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_LenzUnitTypeFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckBoolean(Point_ManualOperationFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_FeedbackUnitFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_FeedbackInputFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckBoolean(Point_FeedbackOnIsStraightFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckBoolean(Point_WiringReversedFlagFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_TypeFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_RelatedPointFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_DefaultStateFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckBoolean(Point_LockedIfHeelTCOccupiedFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckBoolean(Point_OutOfUseFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckBoolean(Point_LockedIfNonHeelTCsOccupiedFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_NotesFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            CheckString(Point_LastManualStateAsReadInFieldName, PointsADOTable, PointsADOTable2, ErrorFound);

            { This is unlikely to be useful
            CheckString(Point_LastFeedbackStateAsReadInFieldName, PointsADOTable, PointsADOTable2, ErrorFound);
            }
          END; {WITH}
        END;

        IF PointsADOTable.EOF AND NOT PointsADOTable2.EOF THEN BEGIN
          Log('XG Point database ' + '"' + Point1DataFilename + '.' + Point1DataFilenameSuffix
                  + '" is shorter than "' + Point2DataFilename + '.' + Point2DataFilenameSuffix + '"');
          Log('XG A later entry in point database ' + '"' + Point1DataFilename + '.' + Point1DataFilenameSuffix + '" is P=' + IntToStr(P));
        END ELSE
          IF NOT PointsADOTable.EOF AND PointsADOTable2.EOF THEN BEGIN
            Log('XG Point database ' + '"' + Point2DataFilename + '.' + Point2DataFilenameSuffix
                    + '" is shorter than "' + Point1DataFilename + '.' + Point1DataFilenameSuffix + '"');
            Log('XG A later entry in point database ' + '"' + Point2DataFilename + '.' + Point2DataFilenameSuffix + '" is P=' + IntToStr(P));
          END;

        PointsADOTable2.Next;
        PointsADOTable.Next;
      UNTIL PointsADOTable.EOF AND PointsADOTable2.EOF;

      IF NOT ErrorFound THEN
        Log('XG No differences found in point databases '
                + '"' + Point1DataFilename + '.' + Point1DataFilenameSuffix + '" and "' + Point2DataFilename + '.' + Point2DataFilenameSuffix + '"');

      { Tidy up the database }
      PointsADOTable.Close;
      PointsADOConnection.Connected := False;
      Log('S Point Data 1 table and connection closed');
      PointsADOTable2.Close;
      PointsADOConnection.Connected := False;
      Log('S Point Data 2 table and connection closed');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG InitialisePoints: ' + E.ClassName + 'error raised, with message: '+ E.Message);
  END; {TRY}
END; { CompareTwoSignalDatabases }

PROCEDURE CompareTwoSignalDatabases(Signal1DataFilename, Signal1DataFilenameSuffix, Signal2DataFilename, Signal2DataFilenameSuffix : String);
{ Compare two signal databases - used for testing }
CONST
  StopTimer = True;

VAR
  ErrorFound : Boolean;
  S : Integer;

  PROCEDURE CheckString(FieldName : String; Table2, Table1 : TADOTable; VAR ErrorFound : Boolean);
  BEGIN
    IF UpperCase(Table1.FieldByName(FieldName).AsString) <> UpperCase(Table2.FieldByName(FieldName).AsString) THEN BEGIN
      IF NOT ErrorFound THEN BEGIN
        Log('XG Differences found in signal databases '
                + '"' + Signal2DataFilename + '.' + Signal2DataFilenameSuffix + '" and "' + Signal1DataFilename + '.' + Signal1DataFilenameSuffix + '"');
        ErrorFound := True;
      END;
      Log('XG S=' + IntToStr(S) + ' ' + FieldName + ': "' + Table1.FieldByName(FieldName).AsString + '" to "' + Table2.FieldByName(FieldName).AsString + '"');
    END;
  END; { CheckString }

  PROCEDURE CheckBoolean(FieldName : String; Table2, Table1 : TADOTable; VAR ErrorFound : Boolean);
  BEGIN
    IF Table1.FieldByName(FieldName).AsBoolean <> Table2.FieldByName(FieldName).AsBoolean THEN BEGIN
      IF NOT ErrorFound THEN BEGIN
        Log('XG Differences found in signal databases '
                + '"' + Signal2DataFilename + '.' + Signal2DataFilenameSuffix + '" and "' + Signal1DataFilename + '.' + Signal1DataFilenameSuffix + '"');
        ErrorFound := True;
      END;
      Log('XG S=' + IntToStr(S) + ' ' + FieldName
              + ': "' + BoolToStr(Table1.FieldByName(FieldName).AsBoolean, True) + '" to "' + BoolToStr(Table2.FieldByName(FieldName).AsBoolean, True) + '"');
    END;
  END; { CheckBoolean }

BEGIN
  TRY
    Log('A SIGNAL DATABASES COMPARISON {BlankLineBefore}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + Signal1DataFilename + '.' + Signal1DataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Signal database file "' + PathToRailDataFiles + Signal1DataFilename + '.' + Signal1DataFilenameSuffix + '"'
                                      + ' cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'CompareTwoSignalDatabases')
        ELSE
          Exit;
      END;

      SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                               + PathToRailDataFiles + Signal1DataFilename + '.' + Signal1DataFilenameSuffix
                                               + ';Persist Security Info=False';
      TRY
        SignalsADOConnection.Connected := True;
      EXCEPT
        ON E:Exception DO
          Log('EG CompareTwoSignalDatabases 1: ' + E.ClassName + ' error raised, with message: '+ E.Message);
      END; {TRY}

      SignalsADOTable.Open;

      SignalsADOConnection2.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                               + PathToRailDataFiles + Signal2DataFilename + '.' + Signal2DataFilenameSuffix
                                               + ';Persist Security Info=False';
      TRY
        SignalsADOConnection2.Connected := True;
      EXCEPT
        ON E:Exception DO
          Log('EG CompareTwoSignalDatabases 2: ' + E.ClassName + ' error raised, with message: '+ E.Message);
      END; {TRY}

      SignalsADOTable2.Open;

      Log('S Signal data table and connection opened to compare signal 1 data with signal 2 data');

      ErrorFound := False;
      S := -1;
      SignalsADOTable.Sort := '[' + Signal_NumberFieldName + '] ASC';
      SignalsADOTable.First;
      SignalsADOTable2.Sort := '[' + Signal_NumberFieldName + '] ASC';
      SignalsADOTable2.First;

      REPEAT
        Inc(S);

        IF S > High(Signals) THEN BEGIN
          IF NOT SignalsADOTable.EOF THEN
            Log('XG Last declared signal (S=' + IntToStr(S - 1) + ') processed but signal database ' + '"' + Signal1DataFilename + '.' + Signal1DataFilenameSuffix
                    + ' has not yet reached end of file')
          ELSE
          IF NOT SignalsADOTable2.EOF THEN
            Log('XG Last declared signal (S=' + IntToStr(S - 1) + ') processed but signal database ' + '"' + Signal2DataFilename + '.' + Signal2DataFilenameSuffix
                    + ' has not yet reached end of file');
        END ELSE BEGIN
          WITH Signals[S] DO BEGIN
            CheckString(Signal_AdjacentLineFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_AdjacentLineXOffsetFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_ApproachControlAspectFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_AsTheatreDestinationFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_IndicatorFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_UpperLeftIndicatorTargetFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_MiddleLeftIndicatorTargetFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_LowerLeftIndicatorTargetFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_UpperRightIndicatorTargetFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_MiddleRightIndicatorTargetFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_LowerRightIndicatorTargetFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_IndicatorDecoderFunctionNumFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_IndicatorDecoderNumFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_IndicatorSpeedRestrictionFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_LocationsToMonitorFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_NextSignalIfNoIndicatorFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_NotUsedForRouteingFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_NotesFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_OppositePassingLoopSignalFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckBoolean(Signal_OutOfUseFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckBoolean(Signal_PossibleRouteHoldFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckBoolean(Signal_PossibleStationStartRouteHoldFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_AccessoryAddressFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_DecoderNumFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_TypeFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
            CheckString(Signal_DirectionFieldName, SignalsADOTable, SignalsADOTable2, ErrorFound);
          END; {WITH}
        END;

        IF SignalsADOTable.EOF AND NOT SignalsADOTable2.EOF THEN BEGIN
          Log('XG Signal database ' + '"' + Signal1DataFilename + '.' + Signal1DataFilenameSuffix
                  + '" is shorter than "' + Signal2DataFilename + '.' + Signal2DataFilenameSuffix + '"');
          Log('XG A later entry in signal database ' + '"' + Signal1DataFilename + '.' + Signal1DataFilenameSuffix + '" is S=' + IntToStr(S));
        END ELSE
          IF NOT SignalsADOTable.EOF AND SignalsADOTable2.EOF THEN BEGIN
            Log('XG Signal database ' + '"' + Signal2DataFilename + '.' + Signal2DataFilenameSuffix
                    + '" is shorter than "' + Signal1DataFilename + '.' + Signal1DataFilenameSuffix + '"');
            Log('XG A later entry in signal database ' + '"' + Signal2DataFilename + '.' + Signal2DataFilenameSuffix + '" is S=' + IntToStr(S));
          END;

        SignalsADOTable2.Next;
        SignalsADOTable.Next;
      UNTIL SignalsADOTable.EOF AND SignalsADOTable2.EOF;

      IF NOT ErrorFound THEN
        Log('XG No differences found in signal databases '
                + '"' + Signal1DataFilename + '.' + Signal1DataFilenameSuffix + '" and "' + Signal2DataFilename + '.' + Signal2DataFilenameSuffix + '"');

      { Tidy up the database }
      SignalsADOTable.Close;
      SignalsADOConnection.Connected := False;
      Log('S Signal Data 1 table and connection closed');
      SignalsADOTable2.Close;
      PointsADOConnection.Connected := False;
      Log('S Signal Data 2 table and connection closed');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG InitialiseSignals: ' + E.ClassName + 'error raised, with message: '+ E.Message);
  END; {TRY}
END; { CompareTwoSignalDatabases }

PROCEDURE LookForEndRoutineComments(Filename : String; VAR LinesAddedCount : Integer);
{ Look for procedure/function ends without matching procedure/function names as comments }
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
  LineCount := 0;
  FunctionLine := False;
  InitialisationLine := False;
  ProcedureLine := False;

  WHILE NOT EoF(InputFile) DO BEGIN
    ReadLn(InputFile, Buf);
    Inc(LineCount);

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

      IF Pos('{', Buf) <> 6 THEN BEGIN
        WriteLn(OutputFile, Filename  + ' (' + IntToStr(LineCount) + ') No name follows "END;" at ' + Buf);
        Inc(LinesAddedCount);
      END ELSE BEGIN
        TempStr := GetFollowingChars(Buf, 'END; { ', '}');
        IF TempStr = '' THEN BEGIN
          WriteLn(OutputFile, Filename  + ' (' + IntToStr(LineCount) + '): No name follows "END;" at ' + Buf);
          Inc(LinesAddedCount);
        END ELSE BEGIN
          { if there's a dot in it, it's probably preceded by the Class name so omit it }
          IF Pos('.', SaveProcedureOrFunctionNameStr) > 0 THEN
            SaveProcedureOrFunctionNameStr := GetFollowingChars(SaveProcedureOrFunctionNameStr, '.', '');

          IF Trim(TempStr) <> Trim(SaveProcedureOrFunctionNameStr) THEN BEGIN
            WriteLn(OutputFile, Filename + ' (' + IntToStr(LineCount) + ') '
                                         + IfThen(ProcedureLine, 'Procedure')
                                         + IfThen(FunctionLine, 'Function')
                                         + IfThen(InitialisationLine, 'Initialization')
                                         + ' "' + Trim(SaveProcedureOrFunctionNameStr) + '" does not match end of routine name "' + Trim(TempStr) + '"');
            Inc(LinesAddedCount);
          END;
        END;
      END;
      FunctionLine := False;
      InitialisationLine := False;
      ProcedureLine := False;
    END;

    IF Length(Buf) > 172 THEN BEGIN
      WriteLn(OutputFile, Filename + ' (' + IntToStr(LineCount) + ') > 172: "' + Buf + '"');
      Inc(LinesAddedCount);
    END;
  END;
END; { LookForEndRoutineComments }

PROCEDURE FormatCheckAllFiles;
{ Checks all .pas files for formatting errors }
CONST
  AppendToFile = True;

VAR
  ErrorMsg : String;
  LinesAddedCount : Integer;
  SearchRec: TSearchRec;

BEGIN
  LinesAddedCount := 0;
  Debug('Beginning formatting check...');
  IF NOT OpenOutputFileOK(OutputFile, PathToRailDataFiles + DataCheckFileName, ErrorMsg, NOT AppendToFile) THEN
    Debug(ErrorMsg)
  ELSE BEGIN
    IF FindFirst(PathToRailSourceFiles + '*.pas', FaAnyFile, SearchRec) = 0 THEN BEGIN
      REPEAT
        { If SearchRec = . OR .. then skip to next iteration }
        IF (SearchRec.Name =  '.') OR (SearchRec.Name =  '..') OR DirectoryExists(SearchRec.Name) THEN
          Continue;

        IF NOT OpenInputFileOK(InputFile, PathToRailSourceFiles + SearchRec.Name, ErrorMsg) THEN
          Debug(ErrorMsg)
        ELSE BEGIN
          LookForEndRoutineComments(SearchRec.Name, LinesAddedCount);
          CloseInputOrOutputFile(InputFile, PathToRailSourceFiles + SearchRec.Name);
        END;

      { Loop until no more files are found }
      UNTIL FindNext(SearchRec) <> 0;
    END;
  END;

  CloseInputOrOutputFile(OutputFile, DataCheckFileName);
  IF LinesAddedCount = 0 THEN
    Debug('Formatting check completed - no lines written to ' + DataCheckFileName)
  ELSE
    Debug('Formatting check completed - ' + IntToStr(LinesAddedCount) + ' lines written to ' + DataCheckFileName);
END; { FormatCheckAllFiles }

END { DataCheck }.
