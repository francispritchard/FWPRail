UNIT CreateRoute;
{ Creates the routeing

  FWP 26/12/00 Extracted from the then Path unit
  FWP 07/02/01 Doubled length of Route string to 20
  FWP 06/12/04 Development of automated routeing begun
  FWP 06/04/05 Renamed CreateRoute from Route
  FWP 01/05/09 Routeing exceptions now read in from an MSAccess database
}

INTERFACE

USES InitVars, RailDraw, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Grids, StdCtrls, ComCtrls, ExtCtrls, DB, ADODB;

TYPE
  TCreateRouteDisplayColoursWindow = CLASS(TForm)
    CreateRouteDisplayColoursWindowRichEdit: TRichEdit;
    RouteingExceptionDataADOConnection: TADOConnection;
    RouteingExceptionDataADOTable: TADOTable;
    RouteingExceptionDataSource: TDataSource;
    PROCEDURE CreateRouteDisplayColoursWindowCreate(Sender: TObject);
    PROCEDURE CreateRouteDisplayColoursWindowRichEditKeyDown(Sender: TObject; VAR Key: Word; Shift: TShiftState);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  CreateRouteDisplayColoursWindow : TCreateRouteDisplayColoursWindow;

PROCEDURE CreateClearingSubRouteArray(Route, SubRoute : Integer);
{ Takes the subroute setting up array and converts it to clear the subroute }

PROCEDURE CreateInitialRouteRelatedArrays(T : Train; LocoChip : Integer; RouteArray : StringArrayType; AutoRouteSetting : Boolean;
                                          StartSignal, EndSignal, EndBufferStop, StartLine, EndLine : Integer);
{ Set up the various route-related arrays }

PROCEDURE CreateLockingArrayFromDraftRouteArray(LocoChip : Integer; DraftRouteArray : StringArrayType; OUT LockingArray : StringArrayType);
{ Creates locking based on the route previously found - adds the original linename data as it's used in drawing the subroute }

PROCEDURE CreateRouteArrayFromLockingArray(Route : Integer; LockingArray : StringArrayType; OUT RouteArray : StringArrayType);
{ The route is divided into subroutes, each controlled by a signal, which enables partial routes to be set. Now add the signal found at the start of each subroute to the
  end of the section of the array it controls. Start off by adding the signal at the very start of the route (it effectively throws away the signal at the end of the route,
  as that should always be on).
}
PROCEDURE CreateRouteArraysForTrain(T : Train);
{ See what routeing needs doing for the given train }

PROCEDURE FindAndHighlightAllRoutes(RouteStartLine : Integer; RouteDirection : DirectionType; TrainType : TypeOfTrainType; TrainLength : Integer;
                                    OUT DraftRouteArray : StringArrayType; OUT OK : Boolean);
{ Looks for all possible routes, highlights them and returns a list of them }

PROCEDURE FindAndHighlightNearestSignalsToTheatreIndicator(RouteStartLine : Integer; RouteDirection : DirectionType; OUT DraftRouteArray : StringArrayType;
                                                           OUT OK : Boolean);
{ Looks for the nearest signals to a given theatre indicator, highlights them and returns a list of them if the route is available }

PROCEDURE FindNearestSignalsToGivenSignal(RouteStartLine : Integer; RouteDirection : DirectionType; OUT DraftRouteArray : StringArrayType; OUT OK : Boolean);
{ Looks for nearest signals only and returns a list of them - used in setting up routes in advance }

FUNCTION FindNextSignalOrBufferStop(CurrentSignal, NextSignal, NextBufferStop : Integer; IndicatorToBeSet : Boolean; OUT LinesNotAvailableStr : String;
                                    OUT RouteArray : StringArrayType) : Boolean;
{ Looks for the next signal using the rules in the LineAvailable procedure }

PROCEDURE FindPreviousSignals(S : Integer; OUT PreviousSignal1, PreviousSignal2 : Integer);
{ Looks for the signals before ours, so we can change their aspects too. We just go in reverse, following how the points are set - if a
7  signal is off, it must be previous to us, as the points it controls would be locked
}
PROCEDURE FindRouteFromLineAToLineB(LocoChip, Journey, S, StartLine, EndLine : Integer; Direction : DirectionType; TrainType : TypeOfTrainType; TrainLength : Integer;
                                    EmergencyRouteing, AreOutOfUseLinesIncluded : Boolean; OUT DraftRouteArray : StringArrayType;
                                    OUT LinesNotAvailableStr, ErrorMsg : String; OUT RouteFound : Boolean);
{ Uses line names to find a route from A to B }

FUNCTION GetResettingTrackCircuit(LocoChip, S : Integer; SuppressMessage : Boolean) : Integer;
{ Extract the resetting trackcircuit (if any) from the Locking Array }

PROCEDURE ReadInRouteingExceptionsFromDatabase;
{ Read in from file parameters that indicate routes that are explicitly not allowed }

IMPLEMENTATION

{$R *.dfm}

USES GetTime, Diagrams, MiscUtils, Locks, Startup, Input, Cuneo, DateUtils, Movement, LocoUtils, Lenz, StrUtils, ProgressBar, Help, ShellAPI, LocationData, Options;

CONST
  UnitRef = 'CreateRoute';

VAR
  LineCounter : Integer = 0;
  LinesNotAvailableNum : Integer = 0;
  NonThroughLine : Integer = UnknownLine;
  SaveSignal : Integer = UnknownSignal;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' <Unit=' + UnitRef + '>');
END; { Log }

PROCEDURE ReadInRouteingExceptionsFromDatabase;
{ Read in from file parameters that indicate routes that are explicitly not allowed }
VAR
  ErrorMsg : String;
  FieldName : String;
  I : Integer;
  NotLine : Boolean;
  TempArea : Integer;
  TempLine : Integer;
  TempLocation : Integer;
  TempStr : String;
  TempStrArray : StringArrayType;
  TempTrainTypeNum : Integer;

BEGIN
  TRY
    Log('G INITIALISING ROUTEING EXCEPTIONS <BlankLineBefore>');
    
    WITH CreateRouteDisplayColoursWindow DO BEGIN
      RouteingExceptionDataADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                             + PathToRailDataFiles + RouteingExceptionDataFilename + '.' + RouteingExceptionDataFilenameSuffix
                                                             + ';Persist Security Info=False';
      RouteingExceptionDataADOConnection.Connected := True;
      RouteingExceptionDataADOTable.Open;
      Log('T RouteingExceptionData data table and connection opened to initialise the RouteingExceptionData data');

      RouteingExceptionDataADOTable.Sort := 'Rule ASC';
      RouteingExceptionDataADOTable.First;
      WHILE NOT RouteingExceptionDataADOTable.EOF DO BEGIN
        ErrorMsg := '';

        SetLength(RouteingExceptions, Length(RouteingExceptions) + 1);
        WITH RouteingExceptions[High(RouteingExceptions)] DO BEGIN
          RouteingException_AllowedInEmergency := False;
          SetLength(RouteingException_CurrentLines, 0);
          SetLength(RouteingException_CurrentLinesExcepted, 0);
          SetLength(RouteingException_EndAreas, 0);
          SetLength(RouteingException_EndAreasExcepted, 0);
          SetLength(RouteingException_EndLines, 0);
          SetLength(RouteingException_EndLinesExcepted, 0);
          SetLength(RouteingException_EndLocations, 0);
          SetLength(RouteingException_EndLocationsExcepted, 0);
          SetLength(RouteingException_LinesRoutedOver, 0);
          SetLength(RouteingException_PreviousLines, 0);
          SetLength(RouteingException_StartAreas, 0);
          SetLength(RouteingException_StartAreasExcepted, 0);
          SetLength(RouteingException_StartLines, 0);
          SetLength(RouteingException_StartLinesExcepted, 0);
          SetLength(RouteingException_StartLocations, 0);
          SetLength(RouteingException_StartLocationsExcepted, 0);

          FieldName := 'Rule';
          IF RouteingExceptionDataADOTable.FieldByName(FieldName).AsString = '' THEN
            ErrorMsg := 'No rule number supplied'
          ELSE
            RouteingException_Rule := StrToInt(RouteingExceptionDataADOTable.FieldByName(FieldName).AsString);

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Allow In Emergency';
            RouteingException_AllowedInEmergency := RouteingExceptionDataADOTable.FieldByName(FieldName).AsBoolean;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Start Lines';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr <> '' THEN BEGIN
              ExtractSubStringsFromString(TempStr, ',', TempStrArray);
              FOR I := 0 TO High(TempStrArray) DO BEGIN
                NotLine := False;
                IF Copy(TempStrArray[I], 1, 4) = 'NOT ' THEN BEGIN
                  TempStrArray[I] := Copy(TempStrArray[I], 5);
                  NotLine := True;
                END;
                TempLine := StrToLine(TempStrArray[I]);
                IF TempLine = UnknownLine THEN
                  ErrorMsg := 'invalid start line "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                ELSE
                  IF NotLine THEN
                    AppendToIntegerArray(RouteingException_StartLinesExcepted, TempLine)
                  ELSE
                    AppendToIntegerArray(RouteingException_StartLines, TempLine);
              END; {FOR}
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Start Locations';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr <> '' THEN BEGIN
              ExtractSubStringsFromString(TempStr, ',', TempStrArray);
              FOR I := 0 TO High(TempStrArray) DO BEGIN
                NotLine := False;
                IF Copy(TempStrArray[I], 1, 4) = 'NOT ' THEN BEGIN
                  TempStrArray[I] := Copy(TempStrArray[I], 5);
                  NotLine := True;
                END;
                TempLocation := StrToLocation(TempStrArray[I]);
                IF TempLocation = UnknownLine THEN
                  ErrorMsg := 'invalid start location "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                ELSE
                  IF NotLine THEN
                    AppendToIntegerArray(RouteingException_StartLocationsExcepted, TempLocation)
                  ELSE
                    AppendToIntegerArray(RouteingException_StartLocations, TempLocation);
              END; {FOR}
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Start Areas';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr <> '' THEN BEGIN
              ExtractSubStringsFromString(TempStr, ',', TempStrArray);
              FOR I := 0 TO High(TempStrArray) DO BEGIN
                NotLine := False;
                IF Copy(TempStrArray[I], 1, 4) = 'NOT ' THEN BEGIN
                  TempStrArray[I] := Copy(TempStrArray[I], 5);
                  NotLine := True;
                END;
                TempArea := StrToArea(TempStrArray[I]);
                IF TempArea = UnknownArea THEN
                  ErrorMsg := 'invalid start area "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                ELSE
                  IF NotLine THEN
                    AppendToIntegerArray(RouteingException_StartAreasExcepted, TempArea)
                  ELSE
                    AppendToIntegerArray(RouteingException_StartAreas, TempArea);
              END; {FOR}
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'End Lines';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr <> '' THEN BEGIN
              ExtractSubStringsFromString(TempStr, ',', TempStrArray);
              FOR I := 0 TO High(TempStrArray) DO BEGIN
                NotLine := False;
                IF Copy(TempStrArray[I], 1, 4) = 'NOT ' THEN BEGIN
                  TempStrArray[I] := Copy(TempStrArray[I], 5);
                  NotLine := True;
                END;
                TempStrArray[I] := TrimRemoveSpacesAndMakeUpperCase(TempStrArray[I]);
                IF Pos('UNKNOWNLINE', TempStrArray[I]) > 0 THEN
                  AppendToIntegerArray(RouteingException_EndLinesExcepted, UnknownLine)
                ELSE BEGIN
                  TempLine := StrToLine(TempStrArray[I]);

                  IF TempLine = UnknownLine THEN
                    ErrorMsg := 'invalid end line "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                  ELSE
                    IF NotLine THEN
                      AppendToIntegerArray(RouteingException_EndLinesExcepted, TempLine)
                    ELSE
                      AppendToIntegerArray(RouteingException_EndLines, TempLine);
                 END;
              END; {FOR}
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'End Locations';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr <> '' THEN BEGIN
              ExtractSubStringsFromString(TempStr, ',', TempStrArray);
              FOR I := 0 TO High(TempStrArray) DO BEGIN
                NotLine := False;
                IF Copy(TempStrArray[I], 1, 4) = 'NOT ' THEN BEGIN
                  TempStrArray[I] := Copy(TempStrArray[I], 5);
                  NotLine := True;
                END;
                TempLocation := StrToLocation(TempStrArray[I]);
                IF TempLocation = UnknownLine THEN
                  ErrorMsg := 'invalid end location "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                ELSE
                  IF NotLine THEN
                    AppendToIntegerArray(RouteingException_EndLocationsExcepted, TempLocation)
                  ELSE
                    AppendToIntegerArray(RouteingException_EndLocations, TempLocation);
              END; {FOR}
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'End Areas';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr <> '' THEN BEGIN
              ExtractSubStringsFromString(TempStr, ',', TempStrArray);
              FOR I := 0 TO High(TempStrArray) DO BEGIN
                NotLine := False;
                IF Copy(TempStrArray[I], 1, 4) = 'NOT ' THEN BEGIN
                  TempStrArray[I] := Copy(TempStrArray[I], 5);
                  NotLine := True;
                END;
                TempArea := StrToArea(TempStrArray[I]);
                IF TempArea = UnknownArea THEN
                  ErrorMsg := 'invalid end area "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                ELSE
                  IF NotLine THEN
                    AppendToIntegerArray(RouteingException_EndAreasExcepted, TempArea)
                  ELSE
                    AppendToIntegerArray(RouteingException_EndAreas, TempArea);
              END; {FOR}
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Current Lines';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr <> '' THEN BEGIN
              ExtractSubStringsFromString(TempStr, ',', TempStrArray);
              FOR I := 0 TO High(TempStrArray) DO BEGIN
                NotLine := False;
                IF Copy(TempStrArray[I], 1, 4) = 'NOT ' THEN BEGIN
                  TempStrArray[I] := Copy(TempStrArray[I], 5);
                  NotLine := True;
                END;
                TempLine := StrToLine(TempStrArray[I]);
                IF TempLine = UnknownLine THEN
                  ErrorMsg := 'invalid current line "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                ELSE
                  IF NotLine THEN
                    AppendToIntegerArray(RouteingException_CurrentLinesExcepted, TempLine)
                  ELSE
                    AppendToIntegerArray(RouteingException_CurrentLines, TempLine);
              END; {FOR}
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Previous Lines';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr <> '' THEN BEGIN
              ExtractSubStringsFromString(TempStr, ',', TempStrArray);
              FOR I := 0 TO High(TempStrArray) DO BEGIN
                NotLine := False;
                IF Copy(TempStrArray[I], 1, 4) = 'NOT ' THEN BEGIN
                  TempStrArray[I] := Copy(TempStrArray[I], 5);
                  NotLine := True;
                END;
                TempLine := StrToLine(TempStrArray[I]);
                IF TempLine = UnknownLine THEN
                  ErrorMsg := 'invalid previous line "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                ELSE
                  IF NotLine THEN
                    AppendToIntegerArray(RouteingException_PreviousLinesExcepted, TempLine)
                  ELSE
                    AppendToIntegerArray(RouteingException_PreviousLines, TempLine);
              END; {FOR}
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Lines Routed Over';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr <> '' THEN BEGIN
              ExtractSubStringsFromString(TempStr, ',', TempStrArray);
              FOR I := 0 TO High(TempStrArray) DO BEGIN
                NotLine := False;
                IF Copy(TempStrArray[I], 1, 4) = 'NOT ' THEN BEGIN
                  TempStrArray[I] := Copy(TempStrArray[I], 5);
                  NotLine := True;
                END;
                TempLine := StrToLine(TempStrArray[I]);
                IF TempLine = UnknownLine THEN
                  ErrorMsg := 'invalid line routed over "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                ELSE
                  IF NotLine THEN
                    AppendToIntegerArray(RouteingException_LinesRoutedOverexcepted, TempLine)
                  ELSE
                    AppendToIntegerArray(RouteingException_LinesRoutedOver, TempLine);
              END; {FOR}
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Route Direction';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr = '' THEN
              RouteingException_RouteDirection := UnknownDirection
            ELSE BEGIN
              RouteingException_RouteDirection := StrToDirectionType(TempStr);
              IF RouteingException_RouteDirection = UnknownDirection THEN
                ErrorMsg := 'Invalid route direction "' + TempStr + '" in rule ' + IntToStr(RouteingException_Rule);
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Train Type Numbers';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr <> '' THEN BEGIN
              ExtractSubStringsFromString(TempStr, ',', TempStrArray);
              FOR I := 0 TO High(TempStrArray) DO BEGIN
                IF NOT TryStrToInt(TempStrArray[I], TempTrainTypeNum) THEN
                  ErrorMsg := 'invalid train type number "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                ELSE
                  IF (TempTrainTypeNum < 0) OR (TempTrainTypeNum > 9) THEN
                    ErrorMsg := 'invalid train type number "' + TempStrArray[I] + '" in rule ' + IntToStr(RouteingException_Rule)
                  ELSE
                    AppendToTrainTypeArray(RouteingException_TrainTypes, TrainTypeNumToTrainType(TempTrainTypeNum));
              END; {FOR}
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Max Train Length';
            TempStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF TempStr = '' THEN
              RouteingException_MaxTrainLength := 0
            ELSE
              IF NOT TryStrToInt(TempStr, RouteingException_MaxTrainLength) THEN
                ErrorMsg := 'Invalid Max Train Length "' + TempStr + '" in rule ' + IntToStr(RouteingException_Rule);
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'Stop String';
            RouteingException_StopStr := RouteingExceptionDataADOTable.FieldByName(FieldName).AsString;
            IF RouteingException_StopStr = '' THEN
              ErrorMsg := 'Missing stop string in rule ' + IntToStr(RouteingException_Rule)
          END;

          IF ErrorMsg <> '' THEN BEGIN
            IF MessageDialogueWithDefault('Error in creating routeing exception : ' + ErrorMsg
                                          + CRLF
                                          + 'Do you wish to continue?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
            THEN
              ShutDownProgram(UnitRef, 'ReadInRouteingExceptionsFromDatabase');
          END;

          RouteingExceptionDataADOTable.Next;
        END; {WITH}
      END; {WHILE}

      { Tidy up the database }
      RouteingExceptionDataADOTable.Close;
      RouteingExceptionDataADOConnection.Connected := False;
      Log('T RouteingExceptionData data table and connection closed');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInRouteingExceptionsFromDatabase: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ReadInRouteingExceptionsFromDatabase }

PROCEDURE CreateDraftRouteArray(LocoChip : Integer; OUT DraftRouteArray : StringArrayType; StartLine, EndLine : Integer; RouteDirection : DirectionType;
                                IndicatorState : IndicatorStateType; TrainType: TypeOfTrainType; TrainLength : Integer; VAR NextSignal : Integer;
                                OUT NextBufferStop : Integer; ReturnTheSpecifiedRoute, ReturnAllRoutes, LookForNextSignalOrBufferStopOnly,
                                LookForASpecificSignalOrBufferStop, IncludeOutOfUseLines, LookForAllSignalsAndBufferStops : Boolean; OUT LinesNotAvailableStr : String;
                                EmergencyRouteing : Boolean; OUT RouteFound : Boolean);
CONST
  Facing = True;
  NumberElements = True;

VAR
  PossibleRoutesArray : StringArrayType;

  PROCEDURE RecordLine(CurrentLine : Integer; LineColour : TColour);
  { Note which line it is, and colour it if required, but not if it's the line we're starting from }
  CONST
    ActiveTrain = True;
    ProcessMessages = True;

  BEGIN
    IF RouteDebuggingMode THEN
      { slow down the drawing process }
      Pause(50, ProcessMessages);

    AppendToStringArray(DraftRouteArray, 'L=' + LineToStr(CurrentLine) + '+');
    IF RouteDebuggingMode THEN
      DrawLine(CurrentLine, LineColour, NOT ActiveTrain);
    Lines[CurrentLine].Line_RoutedOver := True;
  END; { RecordLine }

  PROCEDURE RecordPoint(P : Integer; Facing : Boolean; PointState : PointStateType; PointColour : TColour);
  { Note which point it is, and colour it if required }
  BEGIN
    IF Facing THEN BEGIN
      IF PointState = Straight THEN
        AppendToStringArray(DraftRouteArray, 'FP=' + IntToStr(P) + '-')
      ELSE
        AppendToStringArray(DraftRouteArray, 'FP=' + IntToStr(P) + '/');
    END ELSE BEGIN
      IF PointState = Straight THEN
        AppendToStringArray(DraftRouteArray, 'TP=' + IntToStr(P) + '-')
      ELSE
        AppendToStringArray(DraftRouteArray, 'TP=' + IntToStr(P) + '/');
    END;
    IF PointDebuggingMode THEN
      DrawPoint(P, PointColour);
  END; { RecordPoint }

  FUNCTION ChangePointStateStringToStraight(Str : String) : String;
  { Change the state the point has in the array ( not its real state) }
  BEGIN
    IF Copy(Str, Length(Str), 1) = '/' THEN
      Result := Copy(Str, 1, Length(Str) - 1) + '-'
    ELSE
      Debug('**** Cannot change point state for string "' + Str + '"');
  END; { ChangePointStateStringToStraight }

  FUNCTION ChangePointStateStringToDiverging(Str : String) : String;
  { Change the state the point has in the array ( not its real state) }
  BEGIN
    IF Copy(Str, Length(Str), 1) = '-' THEN
      Result := Copy(Str, 1, Length(Str) - 1) + '/'
    ELSE
      Debug('**** Cannot change point state for string "' + Str + '"');
  END; { ChangePointStateStringToDiverging }

  FUNCTION GetPointDummyState(P : Integer) : PointStateType;
  { Return the state the point has in the array (not its real state) unless it's a catch point }
  VAR
    DraftRouteArrayPos : Integer;
    PointFound : Boolean;

  BEGIN
    IF PointIsCatchPoint(P) THEN
      { catch points are always straight for the purpose of tracing a route }
      Result := Straight
    ELSE BEGIN
      DraftRouteArrayPos := 0;
      PointFound := False;
      Result := PointStateUnknown;
      IF Length(DraftRouteArray) > 0 THEN BEGIN
        WHILE (DraftRouteArrayPos < Length(DraftRouteArray))
        AND NOT PointFound
        DO BEGIN
          IF (Pos('FP=', DraftRouteArray[DraftRouteArrayPos]) > 0)
          AND (ExtractPointFromString(DraftRouteArray[DraftRouteArrayPos]) = P)
          THEN BEGIN
            PointFound := True;
            Result := ExtractPointStateFromString(DraftRouteArray[DraftRouteArrayPos]);
          END ELSE
            Inc(DraftRouteArrayPos);
        END; {WHILE}
      END;

      IF Result = PointStateUnknown THEN BEGIN
        IF (IndicatorState = LeftIndicatorLit) OR (IndicatorState = RightIndicatorLit) THEN
          Result := Diverging
        ELSE
          Result := Straight;
      END;
    END;
  END; { GetPointDummyState }

  FUNCTION Backtrack(VAR CurrentLine, PreviousLine, PreviousLineButOne : Integer; Str : String) : Boolean;
  { Work backwards until we reach a point }
  CONST
    ActiveTrain = True;

  VAR
    DebugStr : String;
    I : Integer;
    NextPointDummyState : PointStateType; { not the real state of point NextPoint }
    PreviousPointFound : Boolean;
    DraftRouteArrayPos : Integer;

  BEGIN
    { Look through the route array looking for the line we're on. Then backwards until we find the previous point - if it's marked as straight, we need to mark it as
      diverging, and if it's diverging, we need to go back until we find one that's straight; then we extract the previous line name, and head off in the new direction.
    }
    DraftRouteArrayPos := High(DraftRouteArray);
    PreviousPointFound := False;
    Result := True;
    IF DraftRouteArrayPos = 0 THEN
      Result := False
    ELSE BEGIN
      WHILE (DraftRouteArrayPos <= Length(DraftRouteArray))
            AND (DraftRouteArrayPos > 0)
            AND NOT PreviousPointFound
            AND (Result = True)
      DO BEGIN
        Dec(DraftRouteArrayPos);
        IF DraftRouteArrayPos = 0 THEN BEGIN
          Result := False;
        END ELSE BEGIN
          IF Copy(DraftRouteArray[DraftRouteArrayPos], 1, 3) = 'FP=' THEN BEGIN
            PreviousPointFound := True;

            { Go back one element to get the line - or back two if there's a signal or another point in the way }
            IF (Pos('FS=', DraftRouteArray[DraftRouteArrayPos - 1]) > 0)
            OR (Pos('TS=', DraftRouteArray[DraftRouteArrayPos - 1]) > 0)
            OR (Pos('TP=', DraftRouteArray[DraftRouteArrayPos - 1]) > 0)
            OR (Pos('FP=', DraftRouteArray[DraftRouteArrayPos - 1]) > 0)
            THEN BEGIN
              IF DraftRouteArrayPos > 1 THEN BEGIN
                IF PreviousLineButOne <> PreviousLine THEN
                  PreviousLineButOne := PreviousLine;
                IF PreviousLine <> CurrentLine THEN
                  PreviousLine := CurrentLine;
                CurrentLine := ExtractLineFromString(DraftRouteArray[DraftRouteArrayPos - 2]);
              END;
            END ELSE BEGIN
              IF PreviousLineButOne <> PreviousLine THEN
                PreviousLineButOne := PreviousLine;
              IF PreviousLine <> CurrentLine THEN
                PreviousLine := CurrentLine;
              CurrentLine := ExtractLineFromString(DraftRouteArray[DraftRouteArrayPos - 1]);
            END;

            IF CurrentLine = UnknownLine THEN
              Debug('Unknown line in Backtrack');

            NextPointDummyState := ExtractPointStateFromString(DraftRouteArray[DraftRouteArrayPos]);
            IF ((NextPointDummyState = Straight)
               AND (IndicatorState = NoIndicatorLit))
            OR ((NextPointDummyState = Diverging)
               AND (IndicatorState <> NoIndicatorLit))
            THEN BEGIN
              { Write out the un-backtracked state }
              IF RouteBackTrackDebuggingMode THEN BEGIN
                DebugStr := 'BT: ' + DraftRouteArray[High(DraftRouteArray)];

              END;

              IF IndicatorState = NoIndicatorLit THEN
                { the point has started off as straight, so we mark it as diverging and return }
                DraftRouteArray[DraftRouteArrayPos] := ChangePointStateStringToDiverging(DraftRouteArray[DraftRouteArrayPos])
              ELSE
                { we do the opposite to seeking a straight route - the point starts off diverging so we change it to straight for the second pass }
                DraftRouteArray[DraftRouteArrayPos] := ChangePointStateStringToStraight(DraftRouteArray[DraftRouteArrayPos]);

              { unmark through line markers }
              FOR I := DraftRouteArrayPos TO High(DraftRouteArray) DO BEGIN
                IF (Pos('L=', DraftRouteArray[I]) > 0) THEN
                  IF NonThroughLine = ExtractLineFromString(DraftRouteArray[I]) THEN BEGIN
                    Lines[NonThroughLine].Line_RoutedOver := False;
                    NonThroughLine := UnknownLine;
                  END;
              END;

              { and truncate the dynamic array by changing its length }
              SetLength(DraftRouteArray, DraftRouteArrayPos + 1);

              { Now write out the backtracked state }
              IF RouteBackTrackDebuggingMode THEN BEGIN
                IF Str <> '' THEN
                  DebugStr := DebugStr + ' (' + Str + ' at line ' + LineToStr(CurrentLine) +')';
                DebugStr := DebugStr + ' to ' + DraftRouteArray[High(DraftRouteArray)];
                Log('R ' + DebugStr);
              END;

            END ELSE
              { it's been changed to straight (or diverging) already - ignore it and continue reversing }
              PreviousPointFound := False;
          END;
        END;
      END; {WHILE}
    END;
  END; { Backtrack }

  PROCEDURE LookOutForSignalsOrBufferStops(CurrentLine : Integer; OUT FoundASignalOrBufferStop : String);
  { Look out for signals in passing }

    FUNCTION SignalAlreadyRecordedOnRoute(Signal : Integer) : Boolean;
    { Checks if the given signal is in the signal array }
    VAR
      I : Integer;

    BEGIN
      Result := False;
      I := 0;
      WHILE (I < High(PossibleRoutesArray))
      AND (Result = False)
      DO BEGIN
        IF ExtractSignalFromString(PossibleRoutesArray[I]) = Signal THEN
          Result := True;
        Inc(I);
      END; {WHILE}
    END; { SignalAlreadyRecordedOnRoute }

    PROCEDURE RecordAndDrawFacingSignalOnRoute(S : Integer);
    { Record and draw the signals found on the route }
    BEGIN
      IF NOT SignalAlreadyRecordedOnRoute(S) THEN BEGIN
        { route setting }
        SetLength(PossibleRoutesArray, Length(PossibleRoutesArray) + 1);
        PossibleRoutesArray[High(PossibleRoutesArray)] := 'FS=' + IntToStr(S);
        IF Routes_RouteSettingByHand THEN
          Signals[S].Signal_PostColour := SignalPostRouteSettingColour
        ELSE
          IF Routes_TheatreIndicatorSettingInitiated THEN
            Signals[S].Signal_PostColour := SignalPostTheatreSettingColour
          ELSE
            IF Routes_NearestSignalTestingInitiated THEN
              Signals[S].Signal_PostColour := clLime;

        DrawSignalPost(S);
      END;
    END; { RecordAndDrawFacingSignalOnRoute }

    PROCEDURE RecordAndDrawBufferStopOnRoute(BS : Integer);
    { Record and draw the buffer stops found on the route }
    VAR
      I : Integer;
      BufferStopFound : Boolean;

    BEGIN
      BufferStopFound := False;
      { only record it if it's not recorded already }
      FOR I := 0 TO High(PossibleRoutesArray) DO BEGIN
        IF ExtractBufferStopFromString(PossibleRoutesArray[I]) = BS THEN
          BufferStopFound := True;
      END;

      IF NOT BufferStopFound THEN BEGIN
        { route setting }
        SetLength(PossibleRoutesArray, Length(PossibleRoutesArray) + 1);
        PossibleRoutesArray[High(PossibleRoutesArray)] := 'BS=' + IntToStr(BS);
        IF Routes_RouteSettingByHand THEN
          DrawBufferStop(BS, SignalPostRouteSettingColour)
        ELSE
          IF Routes_TheatreIndicatorSettingInitiated THEN
            { theatre indicator setting }
            DrawBufferStop(BS, SignalPostTheatreSettingColour)
          ELSE
            IF Routes_NearestSignalTestingInitiated THEN
              DrawBufferStop(BS, clLime);
      END;
    END; { RecordBufferStopOnRoute }

  BEGIN
    IF CurrentLine <> StartLine THEN BEGIN
      IF (GetLineAdjacentSignal(CurrentLine) <> UnknownSignal)

      AND NOT Signals[GetLineAdjacentSignal(CurrentLine)].Signal_NotUsedForRouteing
//      AND NOT Signals[Lines[CurrentLine].Line_AdjacentSignal].Signal_OutOfUse
      AND (Signals[GetLineAdjacentSignal(CurrentLine)].Signal_Direction = RouteDirection)
      THEN BEGIN
        FoundASignalOrBufferStop := 'FS=' + IntToStr(GetLineAdjacentSignal(CurrentLine));
        { note where we are }
        RecordAndDrawFacingSignalOnRoute(GetLineAdjacentSignal(CurrentLine));
      END ELSE
        IF Lines[CurrentLine].Line_AdjacentBufferStop <> UnknownBufferStop THEN BEGIN
          FoundASignalOrBufferStop := 'BS=' + IntToStr(Lines[CurrentLine].Line_AdjacentBufferStop);
          { note where we are }
          RecordAndDrawBufferStopOnRoute(Lines[CurrentLine].Line_AdjacentBufferStop);
        END ELSE
          FoundASignalOrBufferStop := '';
    END;
  END; { LookOutForSignalsOrBufferStops }

  FUNCTION LineAvailable(CurrentLine, PreviousLine, PreviousLineButOne : Integer; OUT StopStr : String) : Boolean;
  { Check it is permissible to traverse the line }
  CONST
    ActiveTrain = True;

  VAR
    Count : Integer;
    CurrentLineCheck : Boolean;
    CurrentLineStop : Boolean;
    EndLineCheck : Boolean;
    EndLineStop : Boolean;
    EndLineExceptedCheck : Boolean;
    EndLineExceptedStop : Boolean;
    EndLocationCheck : Boolean;
    EndLocationStop : Boolean;
    EndLocationExceptedCheck : Boolean;
    EndLocationExceptedStop : Boolean;
    EndAreaCheck : Boolean;
    EndAreaStop : Boolean;
    EndAreaExceptedCheck : Boolean;
    EndAreaExceptedStop : Boolean;
    FoundASignalOrBufferStop : String;
    LineRoutedOverCheck : Boolean;
    LineRoutedOverStop : Boolean;
    PreviousLineCheck : Boolean;
    PreviousLineStop : Boolean;
    Rule : Integer;
    StartAreaCheck : Boolean;
    StartAreaStop : Boolean;
    StartAreaExceptedCheck : Boolean;
    StartAreaExceptedStop : Boolean;
    StartLineCheck : Boolean;
    StartLineStop : Boolean;
    StartLineExceptedCheck : Boolean;
    StartLineExceptedStop : Boolean;
    StartLocationCheck : Boolean;
    StartLocationStop : Boolean;
    StartLocationExceptedCheck : Boolean;
    StartLocationExceptedStop : Boolean;
    TrainTypeCheck : Boolean;
    TrainTypeStop : Boolean;

  BEGIN
    StopStr := '';
    Result := False;
    FoundASignalOrBufferStop := '';

    IF LookForAllSignalsAndBufferStops THEN BEGIN
      LookOutForSignalsOrBufferStops(CurrentLine, FoundASignalOrBufferStop);
      IF FoundASignalOrBufferStop <> '' THEN BEGIN
        RecordLine(CurrentLine, clAqua);
        StopStr := 'Found ' + FoundASignalOrBufferStop;
      END;
    END;

    IF Lines[CurrentLine].Line_Location <> UnknownLocation THEN BEGIN
      IF Locations[Lines[CurrentLine].Line_Location].Location_ThroughLocationState = NonThroughLocation THEN BEGIN
        IF (CurrentLine <> StartLine)
        AND (CurrentLine <> EndLine)
        THEN BEGIN
          IF Lines[CurrentLine].Line_Location <> Lines[StartLine].Line_Location THEN BEGIN
            IF (EndLine <> UnknownLine)
            AND (Lines[CurrentLine].Line_Location <> Lines[EndLine].Line_Location)
            THEN
              StopStr := 'line is not a through line (embedded rule)';
          END;
        END;
      END;
    END;

    IF RouteDirection = Up THEN BEGIN
      IF CurrentLine = Lines[StartLine].Line_NextDownLine THEN
        StopStr := 'Cannot return to start line (embedded rule)';
    END ELSE
      IF RouteDirection = Down THEN BEGIN
        IF CurrentLine = Lines[StartLine].Line_NextUpLine THEN
          StopStr := 'Cannot return to start line (embedded rule)';
      END;

    { Can't go up a projected line }
    IF (Lines[CurrentLine].Line_TypeOfLine = ProjectedLine) THEN
      StopStr := 'cannot route on projected line (embedded rule)';

    IF Lines[CurrentLine].Line_OutOfUseState = OutOfUse THEN
      StopStr := 'line out of use (embedded rule)';

    { See if the line is one way the other way }
    IF (RouteDirection = Up)
    AND (Lines[CurrentLine].Line_Direction = Down)
    THEN
      StopStr := 'Dir = Up and CurrentLine = Down (embedded rule)'
    ELSE
      IF (RouteDirection = Down)
      AND (Lines[CurrentLine].Line_Direction = Up)
      THEN
        StopStr := 'Dir = Down and CurrentLine = Up (embedded rule)';

    { If a line is permanently occupied by something which is out-of-use, do not route over it (unless we're allowed to do so)}
    IF (Lines[CurrentLine].Line_TC <> UnknownTC)
    AND TrackCircuitStateIsPermanentlyOccupied(Trackcircuits[Lines[CurrentLine].Line_TC].TC_OccupationState)
    AND (CurrentLine <> StartLine)
    AND (CurrentLine <> Lines[StartLine].Line_NextUpLine)
    AND (CurrentLine <> Lines[StartLine].Line_NextDownLine)
    THEN
      IF NOT IncludeOutOfUseLines THEN
        StopStr := 'TC=' + IntToStr(Lines[CurrentLine].Line_TC) + ' not in use (embedded rule)';

    { See if any points are marked as out of use }
    IF Lines[CurrentLine].Line_NextUpPoint <> UnknownPoint THEN
      IF Points[Lines[CurrentLine].Line_NextUpPoint].Point_OutOfUse THEN
        StopStr := 'P=' + IntToStr(Lines[CurrentLine].Line_NextUpPoint) + ' is out of use (embedded rule)';

    IF Lines[CurrentLine].Line_NextDownPoint <> UnknownPoint THEN
      IF Points[Lines[CurrentLine].Line_NextDownPoint].Point_OutOfUse THEN
        StopStr := 'P=' + IntToStr(Lines[CurrentLine].Line_NextDownPoint) + ' is out of use (embedded rule)';

    { Now the various ways we can only go in an emergency }
    IF NOT EmergencyRouteing THEN BEGIN
      { see if it's a goods train, to route up the goods line }
      { this needs to be more sophisticated, to see if anything is due on the main, and to route passenger traffic there if not **** }
      { or an express, to route up the main line }
      IF TrainType = ExpressPassenger THEN
        IF Lines[CurrentLine].Line_TypeOfLine = GoodsLine THEN
          StopStr := 'express should not go on goods (embedded rule)';

      { see we're not using sidings as through routes, unless a siding is the starting point or the destination }
      IF (Lines[CurrentLine].Line_TypeOfLine = SidingLine)
      AND (Lines[StartLine].Line_TypeOfLine <> SidingLine)
      AND ((EndLine <> UnknownLine)
           AND (Lines[EndLine].Line_TypeOfLine <> SidingLine))
      THEN
        StopStr := 'sidings as through routes (embedded rule)';
    END; { End of non-emergency Routeing }

    Rule := 0;
    WHILE (Rule <= High(RouteingExceptions))
    AND (StopStr = '')
    DO BEGIN
      WITH RouteingExceptions[Rule] DO BEGIN
        CurrentLineCheck := False;
        EndLineCheck := False;
        EndLineExceptedCheck := False;
        EndLocationCheck := False;
        EndLocationExceptedCheck := False;
        EndAreaCheck := False;
        EndAreaExceptedCheck := False;
        LineRoutedOverCheck := False;
        PreviousLineCheck := False;
        StartAreaCheck := False;
        StartAreaExceptedCheck := False;
        StartLineCheck := False;
        StartLineExceptedCheck := False;
        StartLocationCheck := False;
        StartLocationExceptedCheck := False;
        TrainTypeCheck := False;

        CurrentLineStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_CurrentLines) > 0 THEN BEGIN
          CurrentLineCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_CurrentLines))
          AND NOT CurrentLineStop
          DO BEGIN
            IF (RouteingException_CurrentLines[Count] <> UnknownLine)
            AND (RouteingException_CurrentLines[Count] = CurrentLine)
            THEN
              CurrentLineStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        PreviousLineStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_PreviousLines) > 0 THEN BEGIN
          PreviousLineCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_PreviousLines))
          AND NOT PreviousLineStop
          DO BEGIN
            IF (RouteingException_PreviousLines[Count] <> UnknownLine)
            AND (RouteingException_PreviousLines[Count] = PreviousLine)
            THEN
              PreviousLineStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        StartLineStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_StartLines) > 0 THEN BEGIN
          StartLineCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_StartLines))
          AND NOT StartLineStop
          DO BEGIN
            IF (RouteingException_StartLines[Count] <> UnknownLine)
            AND (RouteingException_StartLines[Count] = StartLine)
            THEN
              StartLineStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        StartLineExceptedStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_StartLinesExcepted) > 0 THEN BEGIN
          { here we want to allow any exceptions through }
          StartLineExceptedStop := True;
          StartLineExceptedCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_StartLinesExcepted))
          AND StartLineExceptedStop
          DO BEGIN
            IF (RouteingException_StartLinesExcepted[Count] <> UnknownArea)
            AND (StartLine <> UnknownLine)
            AND (Lines[StartLine].Line_Location <> UnknownLocation)
            AND (RouteingException_StartLinesExcepted[Count] = Locations[Lines[StartLine].Line_Location].Location_Area)
            THEN
              StartLineExceptedStop := False;
            Inc(Count);
          END; {WHILE}
        END;

        StartLocationStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_StartLocations) > 0 THEN BEGIN
          StartLocationCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_StartLocations))
          AND NOT StartLocationStop
          DO BEGIN
            IF (RouteingException_StartLocations[Count] <> UnknownLocation)
            AND (StartLine <> UnknownLine)
            AND (Lines[StartLine].Line_Location <> UnknownLocation)
            AND (RouteingException_StartLocations[Count] = Lines[StartLine].Line_Location)
            THEN
              StartLocationStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        StartLocationExceptedStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_StartLocationsExcepted) > 0 THEN BEGIN
          { here we want to allow any exceptions through }
          StartLocationExceptedStop := True;
          StartLocationExceptedCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_StartLocationsExcepted))
          AND StartLocationExceptedStop
          DO BEGIN
            IF (RouteingException_StartLocationsExcepted[Count] <> UnknownArea)
            AND (StartLine <> UnknownLine)
            AND (Lines[StartLine].Line_Location <> UnknownLocation)
            AND (RouteingException_StartLocationsExcepted[Count] = Locations[Lines[StartLine].Line_Location].Location_Area)
            THEN
              StartLocationExceptedStop := False;
            Inc(Count);
          END; {WHILE}
        END;

        StartAreaStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_StartAreas) > 0 THEN BEGIN
          StartAreaCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_StartAreas))
          AND NOT StartAreaStop
          DO BEGIN
            IF (RouteingException_StartAreas[Count] <> UnknownArea)
            AND (StartLine <> UnknownLine)
            AND (Lines[StartLine].Line_Location <> UnknownLocation)
            AND (RouteingException_StartAreas[Count] = Locations[Lines[StartLine].Line_Location].Location_Area)
            THEN
              StartAreaStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        StartAreaExceptedStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_StartAreasExcepted) > 0 THEN BEGIN
          { here we want to allow any exceptions through }
          StartAreaExceptedStop := True;
          StartAreaExceptedCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_StartAreasExcepted))
          AND StartAreaExceptedStop
          DO BEGIN
            IF (RouteingException_StartAreasExcepted[Count] <> UnknownArea)
            AND (StartLine <> UnknownLine)
            AND (Lines[StartLine].Line_Location <> UnknownLocation)
            AND (RouteingException_StartAreasExcepted[Count] = Locations[Lines[StartLine].Line_Location].Location_Area)
            THEN
              StartAreaExceptedStop := False;
            Inc(Count);
          END; {WHILE}
        END;

        EndLineStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_EndLines) > 0 THEN BEGIN
          EndLineCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_EndLines))
          AND NOT EndLineStop
          DO BEGIN
            IF (RouteingException_EndLines[Count] <> UnknownLine)
            AND (RouteingException_EndLines[Count] = EndLine)
            THEN
              EndLineStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        EndLineExceptedStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_EndLinesExcepted) > 0 THEN BEGIN
          { here we want to allow any exceptions through }
          EndLineExceptedStop := True;
          EndLineExceptedCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_EndLinesExcepted))
          AND EndLineExceptedStop
          DO BEGIN
            IF (RouteingException_EndLinesExcepted[Count] <> UnknownArea)
            AND (EndLine <> UnknownLine)
            AND (Lines[EndLine].Line_Location <> UnknownLocation)
            AND (RouteingException_EndLinesExcepted[Count] = Locations[Lines[EndLine].Line_Location].Location_Area)
            THEN
              EndLineExceptedStop := False;
            Inc(Count);
          END; {WHILE}
        END;

        EndLocationStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_EndLocations) > 0 THEN BEGIN
          EndLocationCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_EndLocations))
          AND NOT EndLocationStop
          DO BEGIN
            IF (RouteingException_EndLocations[Count] <> UnknownLocation)
            AND (EndLine <> UnknownLine)
            AND (Lines[EndLine].Line_Location <> UnknownLocation)
            AND (RouteingException_EndLocations[Count] = Lines[EndLine].Line_Location)
            THEN
              EndLocationStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        EndLocationExceptedStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_EndLocationsExcepted) > 0 THEN BEGIN
          EndLocationExceptedCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_EndLocationsExcepted))
          AND NOT EndLocationExceptedStop
          DO BEGIN
            IF (RouteingException_EndLocationsExcepted[Count] <> UnknownLocation)
            AND (EndLine <> UnknownLine)
            AND (Lines[EndLine].Line_Location <> UnknownLocation)
            AND (RouteingException_EndLocationsExcepted[Count] <> Lines[EndLine].Line_Location)
            THEN
              EndLocationExceptedStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        EndLocationExceptedStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_EndLocationsExcepted) > 0 THEN BEGIN
          { here we want to allow any exceptions through }
          EndLocationExceptedStop := True;
          EndLocationExceptedCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_EndLocationsExcepted))
          AND EndLocationExceptedStop
          DO BEGIN
            IF (RouteingException_EndLocationsExcepted[Count] <> UnknownArea)
            AND (EndLine <> UnknownLine)
            AND (Lines[EndLine].Line_Location <> UnknownLocation)
            AND (RouteingException_EndLocationsExcepted[Count] = Locations[Lines[EndLine].Line_Location].Location_Area)
            THEN
              EndLocationExceptedStop := False;
            Inc(Count);
          END; {WHILE}
        END;

        EndAreaStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_EndAreas) > 0 THEN BEGIN
          EndAreaCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_EndAreas))
          AND NOT EndAreaStop
          DO BEGIN
            IF (RouteingException_EndAreas[Count] <> UnknownArea)
            AND (EndLine <> UnknownLine)
            AND (Lines[EndLine].Line_Location <> UnknownLocation)
            AND (RouteingException_EndAreas[Count] = Locations[Lines[EndLine].Line_Location].Location_Area)
            THEN
              EndAreaStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        EndAreaExceptedStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_EndAreasExcepted) > 0 THEN BEGIN
          { here we want to allow any exceptions through }
          EndAreaExceptedStop := True;
          EndAreaExceptedCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_EndAreasExcepted))
          AND EndAreaExceptedStop
          DO BEGIN
            IF (RouteingException_EndAreasExcepted[Count] <> UnknownArea)
            AND (EndLine <> UnknownLine)
            AND (Lines[EndLine].Line_Location <> UnknownLocation)
            AND (RouteingException_EndAreasExcepted[Count] = Locations[Lines[EndLine].Line_Location].Location_Area)
            THEN
              EndAreaExceptedStop := False;
            Inc(Count);
          END; {WHILE}
        END;

        LineRoutedOverStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_LinesRoutedOver) > 0 THEN BEGIN
          LineRoutedOverCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_LinesRoutedOver))
          AND NOT LineRoutedOverStop
          DO BEGIN
            IF (RouteingException_LinesRoutedOver[Count] <> UnknownLine)
            AND Lines[RouteingException_LinesRoutedOver[Count]].Line_RoutedOver
            THEN
              LineRoutedOverStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        TrainTypeStop := False;
        IF Length(RouteingExceptions[Rule].RouteingException_TrainTypes) > 0 THEN BEGIN
          TrainTypeCheck := True;
          Count := 0;
          WHILE (Count <= High(RouteingExceptions[Rule].RouteingException_TrainTypes))
          AND NOT TrainTypeStop
          DO BEGIN
            IF (RouteingException_TrainTypes[Count] <> UnknownTrainType)
            AND (RouteingException_TrainTypes[Count] = TrainType)
            THEN
              TrainTypeStop := True;
            Inc(Count);
          END; {WHILE}
        END;

        IF NOT RouteingExceptions[Rule].RouteingException_AllowedInEmergency THEN
          IF NOT CurrentLineCheck
          OR (CurrentLineCheck
                 AND CurrentLineStop)
          THEN
            IF NOT PreviousLineCheck
            OR (PreviousLineCheck
                AND PreviousLineStop)
            THEN
              IF NOT StartLineCheck
              OR (StartLineCheck
                  AND StartLineStop)
              THEN
                IF NOT StartLineExceptedCheck
                OR (StartLineExceptedCheck
                    AND StartLineExceptedStop)
                THEN
                  IF NOT StartLocationCheck
                  OR (StartLocationCheck
                      AND StartLocationStop)
                  THEN
                    IF NOT StartLocationExceptedCheck
                    OR (StartLocationExceptedCheck
                        AND StartLocationExceptedStop)
                    THEN
                      IF NOT StartAreaCheck
                      OR (StartAreaCheck
                          AND StartAreaStop)
                      THEN
                        IF NOT StartAreaExceptedCheck
                        OR (StartAreaExceptedCheck
                            AND StartAreaExceptedStop)
                        THEN
                          IF NOT EndLineCheck
                          OR (EndLineCheck
                              AND EndLineStop)
                          THEN
                            IF NOT EndLineExceptedCheck
                            OR (EndLineExceptedCheck
                                AND EndLineExceptedStop)
                              THEN
                                IF NOT EndLocationCheck
                                OR (EndLocationCheck
                                    AND EndLocationStop)
                                THEN
                                  IF NOT EndLocationExceptedCheck
                                  OR (EndLocationExceptedCheck
                                      AND EndLocationExceptedStop)
                                    THEN
                                      IF NOT EndAreaCheck
                                      OR (EndAreaCheck
                                          AND EndAreaStop)
                                      THEN
                                        IF NOT EndAreaExceptedCheck
                                        OR (EndAreaExceptedCheck
                                            AND EndAreaExceptedStop)
                                          THEN
                                            IF NOT LineRoutedOverCheck
                                            OR (LineRoutedOverCheck
                                                AND LineRoutedOverStop)
                                            THEN
                                              IF (RouteingExceptions[Rule].RouteingException_RouteDirection = UnknownDirection)
                                              OR ((RouteingException_RouteDirection <> UnknownDirection)
                                                   AND (RouteingException_RouteDirection = RouteDirection))
                                              THEN
                                                IF (RouteingExceptions[Rule].RouteingException_MaxTrainLength = 0)
                                                OR ((RouteingException_MaxTrainLength > 0)
                                                    AND (TrainLength <> UnknownTrainLength)
                                                    AND (RouteingException_MaxTrainLength < TrainLength))
                                                THEN
                                                  IF NOT TrainTypeCheck
                                                  OR (TrainTypeCheck
                                                      AND TrainTypeStop)
                                                  THEN
                                                    StopStr := RouteingException_StopStr + ' (Rule ' + IntToStr(Rule) + ')';
      END; {WITH}
      Inc(Rule);
    END; { WHILE}

    { otherwise it's ok }
    IF StopStr = '' THEN
      Result := True
    ELSE BEGIN
      Inc(LinesNotAvailableNum);
      RecordLine(CurrentLine, LineNotAvailableColour);

      IF RouteDebuggingMode THEN BEGIN
        Log('XG ' + IntToStr(LinesNotAvailableNum) + ' ' + LineToStr(CurrentLine) + ': ' + StopStr);
        DrawLine(CurrentLine, LineNotAvailableColour, NOT ActiveTrain, IntToStr(LinesNotAvailableNum));
      END;

      IF LinesNotAvailableStr = '' THEN
        LinesNotAvailableStr := IntToStr(LinesNotAvailableNum) + ' ' + LineToStr(CurrentLine) + ': ' + StopStr
      ELSE
        LinesNotAvailableStr := LinesNotAvailableStr + '; ' + IntToStr(LinesNotAvailableNum) + ' ' + LineToStr(CurrentLine) + ': ' + StopStr
    END;

    IF AllRouteDebuggingMode THEN
      Log('R ' + LineToStr(CurrentLine) + ': ' + LineToStr(CurrentLine) + ': ' + StopStr);
  END; { LineAvailable }

  PROCEDURE DoRouteSearch(VAR CurrentLine, PreviousLine, PreviousLineButOne : Integer; Next : NextLineRouteingType; OUT ExitFunctionNum : Integer);
  { Tracing the route line to line, point to point, etc. }
  VAR
    FoundASignalOrBufferStop : String;
    NextPoint : Integer;
    NextPointDummyState : PointStateType; { not the real state of point NextPoint }

  BEGIN
    FoundASignalOrBufferStop := '';
    NextPoint := UnknownPoint;

    CASE Next OF
      PointIsNext:
        BEGIN
          IF RouteDirection = Up THEN
            NextPoint := Lines[CurrentLine].Line_NextUpPoint
          ELSE
            IF RouteDirection = Down THEN
              NextPoint := Lines[CurrentLine].Line_NextDownPoint;

          { Is it in the RouteArray already? If so, extract its state. If it's unknown or a catch point, it's returned as being straight }
          NextPointDummyState := GetPointDummyState(NextPoint);

          { where to go next }
          IF RouteDirection = Points[NextPoint].Point_FacingDirection THEN BEGIN
            { It is a facing point }
            IF NextPointDummyState = Straight THEN BEGIN
              { point is straight }
              IF StartLine <> CurrentLine THEN
                RecordLine(CurrentLine, clYellow);
              IF (NOT LookForASpecificSignalOrBufferStop
                  AND LookForNextSignalOrBufferStopOnly)
              OR ReturnAllRoutes
              THEN
                LookOutForSignalsOrBufferStops(CurrentLine, FoundASignalOrBufferStop);

              { now seen which line is next }
              IF PreviousLineButOne <> PreviousLine THEN
                PreviousLineButOne := PreviousLine;
              IF PreviousLine <> CurrentLine THEN
                PreviousLine := CurrentLine;
              CurrentLine := Points[NextPoint].Point_StraightLine;
              RecordPoint(NextPoint, Facing, Straight, clLime);
            END ELSE BEGIN
              { point is diverging }
              RecordLine(CurrentLine, clYellow);
              IF ReturnAllRoutes THEN
                LookOutForSignalsOrBufferStops(CurrentLine, FoundASignalOrBufferStop);

              { now seen which line is next }
              IF PreviousLineButOne <> PreviousLine THEN
                PreviousLineButOne := PreviousLine;
              IF PreviousLine <> CurrentLine THEN
                PreviousLine := CurrentLine;
              CurrentLine := Points[NextPoint].Point_DivergingLine;
              RecordPoint(NextPoint, Facing, Diverging, clLime);
            END;
          END ELSE BEGIN
            { a trailing point }
            IF StartLine <> CurrentLine THEN
              RecordLine(CurrentLine, clCream);
            IF ReturnAllRoutes THEN
              LookOutForSignalsOrBufferStops(CurrentLine, FoundASignalOrBufferStop);
              
            { work out which direction the point is trailing in }
            IF CurrentLine = Points[NextPoint].Point_StraightLine THEN
              RecordPoint(NextPoint, NOT Facing, Straight, clRed)
            ELSE
              RecordPoint(NextPoint, NOT Facing, Diverging, clRed);

            { Next point is trailing so now seen which line is next }
            IF PreviousLineButOne <> PreviousLine THEN
              PreviousLineButOne := PreviousLine;
            IF PreviousLine <> CurrentLine THEN
              PreviousLine := CurrentLine;
            CurrentLine := Points[NextPoint].Point_HeelLine;
          END;

          IF CurrentLine = EndLine THEN BEGIN
            { note where we are }
            RecordLine(CurrentLine, clLime);
            ExitFunctionNum := 8;
            RouteFound := True;
          END;
        END;
      LineIsNext:
        BEGIN
          { and where to go next }
          IF StartLine <> CurrentLine THEN
            RecordLine(CurrentLine, clWhite);

          IF ReturnAllRoutes THEN
            LookOutForSignalsOrBufferStops(CurrentLine, FoundASignalOrBufferStop);

          { now seen which line is next }
          IF PreviousLineButOne <> PreviousLine THEN
            PreviousLineButOne := PreviousLine;
          IF PreviousLine <> CurrentLine THEN
            PreviousLine := CurrentLine;

          IF RouteDirection = Up THEN
            CurrentLine := Lines[CurrentLine].Line_NextUpLine
          ELSE
            IF RouteDirection = Down THEN
              CurrentLine := Lines[CurrentLine].Line_NextDownLine;

          IF CurrentLine = EndLine THEN BEGIN
            { note where we are }
            RecordLine(CurrentLine, clLime);
            ExitFunctionNum := 7;
            RouteFound := True;
          END;
        END;
      EndOfLineIsNext:
        BEGIN
          { note where we are }
          RecordLine(CurrentLine, clRed);

          IF ReturnAllRoutes THEN
            LookOutForSignalsOrBufferStops(CurrentLine, FoundASignalOrBufferStop);

          { If we're looking for the next buffer stop only, we may have found it }
          IF NOT LookForASpecificSignalOrBufferStop
             AND LookForNextSignalOrBufferStopOnly
             AND (Lines[CurrentLine].Line_AdjacentBufferStop <> UnknownBufferStop)
             AND (GetBufferStopDirection(Lines[CurrentLine].Line_AdjacentBufferStop) = RouteDirection)
          THEN BEGIN
            RouteFound := True;
            { note where we are }
            NextBufferStop := Lines[CurrentLine].Line_AdjacentBufferStop;
            ExitFunctionNum := 9;
          END ELSE
            IF NOT Backtrack(CurrentLine, PreviousLine, PreviousLineButOne, 'End of Line') THEN BEGIN
              FoundASignalOrBufferStop := '';
              ExitFunctionNum := 10;
              RouteFound := False;
            END;
        END;
      UnknownNextLineRouteingType:
        BEGIN
          RecordLine(CurrentLine, clBlue);
          ShowMessage('UnknownNextLineRouteingType at ' + LineToStr(CurrentLine));
        END;
    END; {CASE}
  END; { DoRouteSearch }

{ Start of CreateDraftRouteArray }
VAR
  CurrentLine : Integer;
  DebugStr : String;
  DraftRouteArrayPos : Integer;
  ExitFunctionNum : Integer;
  LineFound : Boolean;
  Next : NextLineRouteingType;
  PossibleRoutesArrayPos : Integer;
  PreviousLine : Integer;
  PreviousLineButOne : Integer;
  StopStr : String;

BEGIN
  TRY
    ExitFunctionNum := 0;
    NonThroughLine := UnknownLine;
    StopStr := '';
    RouteFound := False;
    LinesNotAvailableNum := 0;
    LinesNotAvailableStr := '';

    { First clear the routed flag (whereby we know where we've been) }
    FOR CurrentLine := 0 TO High(Lines) DO
      Lines[CurrentLine].Line_RoutedOver := False;

    { and empty the route array }
    SetLength(DraftRouteArray, 0);

    { and add the signal at the start of the route }
    AppendToStringArray(DraftRouteArray, 'FS=' + IntToStr(GetLineAdjacentSignal(StartLine)));

    { also add the line we're starting from, so we can backtrack to it if we need to - need to remove it at the end, though }
    AppendToStringArray(DraftRouteArray, 'L=' + LineToStr(StartLine) + '+');

    CurrentLine := StartLine;
    PreviousLine := UnknownLine;
    PreviousLineButOne := UnknownLine;

    IF RouteDrawingMode THEN
      DebugStr := '';

    { Main loop }
    WHILE ExitFunctionNum = 0 DO BEGIN
      IF RouteDrawingMode THEN
        DebugStr:= DebugStr + LineToStr(CurrentLine) + ' ';

      IF CurrentLine = UnknownLine THEN BEGIN
        Debug('Unknown line in main loop');
        ExitFunctionNum := 9999;
      END ELSE BEGIN
        IF (StartLine <> EndLine)
        AND (CurrentLine = EndLine)
        THEN BEGIN
          { allow us to return whence we started, and not just sit there - used for FY to FY trips }
          ExitFunctionNum := 1;
          RouteFound := True;
        END ELSE BEGIN
          { If the line isn't the one just before the point we want to look at it (which we do need to visit twice), see if the line has already been routed over - if so,
            we need to backtrack
          }
          IF (Length(DraftRouteArray) > 1)
          AND (((Pos('FP=', DraftRouteArray[High(DraftRouteArray) - 1]) = 0)
               AND (Pos('TP=', DraftRouteArray[High(DraftRouteArray) - 1]) = 0)))
          THEN BEGIN
            IF CurrentLine <> ExtractLineFromString(DraftRouteArray[High(DraftRouteArray) - 1]) THEN BEGIN
              IF Lines[CurrentLine].Line_RoutedOver THEN BEGIN
                IF NOT Backtrack(CurrentLine, PreviousLine, PreviousLineButOne, LineToStr(CurrentLine) + ' routed over') THEN BEGIN
                  ExitFunctionNum := 2;
                  RouteFound := False;
                END;
              END;
            END;
          END;

          IF NOT LineAvailable(CurrentLine, PreviousLine, PreviousLineButOne, StopStr) THEN BEGIN
            IF NOT Backtrack(CurrentLine, PreviousLine, PreviousLineButOne, 'Stop: ' + StopStr) THEN BEGIN
              ExitFunctionNum := 3;
              RouteFound := False;
            END;
          END ELSE BEGIN
            IF RouteDirection = Up THEN
              Next := Lines[CurrentLine].Line_NextUpType
            ELSE
              IF RouteDirection = Down THEN
                Next := Lines[CurrentLine].Line_NextDownType
              ELSE BEGIN
                ShowMessage('RouteDirection = UnknownDirection - Line= ' + LineToStr(CurrentLine));
                { shouldn't ever get here, but it stops the compiler giving a warning }
                Next := UnknownNextLineRouteingType;
              END;

            { Looking for a specific next signal or buffer stop }
            IF LookForASpecificSignalOrBufferStop THEN BEGIN
              IF (CurrentLine <> UnknownLine)
              AND ((GetLineAdjacentSignal(CurrentLine) <> UnknownSignal)
                   AND (GetLineAdjacentSignal(CurrentLine) = NextSignal))
              OR ((Lines[CurrentLine].Line_AdjacentBufferStop <> UnknownBufferStop)
                  AND (Lines[CurrentLine].Line_AdjacentBufferStop = NextBufferStop))
              THEN BEGIN
                RouteFound := True;
                { note where we are }
                RecordLine(CurrentLine, clYellow);
                ExitFunctionNum := 4;
              END ELSE
                DoRouteSearch(CurrentLine, PreviousLine, PreviousLineButOne, Next, ExitFunctionNum);
            END ELSE
              { Looking for any next signal }
              IF LookForNextSignalOrBufferStopOnly THEN BEGIN
                IF (GetLineAdjacentSignal(CurrentLine) <> UnknownSignal)
                AND (GetLineAdjacentSignal(CurrentLine) <> GetLineAdjacentSignal(StartLine))
                AND (Signals[GetLineAdjacentSignal(CurrentLine)].Signal_Direction = RouteDirection)
                THEN BEGIN
                  RouteFound := True;
                  { note where we are }
                  RecordLine(CurrentLine, clAqua);
                  NextSignal := GetLineAdjacentSignal(CurrentLine);
                  ExitFunctionNum := 6;
                END ELSE
                  DoRouteSearch(CurrentLine, PreviousLine, PreviousLineButOne, Next, ExitFunctionNum);
              END ELSE
                { the default position - not looking for any type of signal }
                DoRouteSearch(CurrentLine, PreviousLine, PreviousLineButOne, Next, ExitFunctionNum);
          END;
        END;
      END; {WHILE}

      IF RouteDrawingMode THEN
        Log('R ' + DebugStr);
    END;
    IF ShowCreateRouteExitFunctionNum THEN
      Log('XG ExitFunctionNum = ' + IntToStr(ExitFunctionNum));

    { Finally remove the first "L=", inserted at the beginning of the procedure so we might backtrack to it }
    LineFound := False;
    DraftRouteArrayPos := 0;
    WHILE (DraftRouteArrayPos < High(DraftRouteArray))
    AND NOT LineFound
    DO BEGIN
      IF Pos('L=', DraftRouteArray[DraftRouteArrayPos]) > 0 THEN BEGIN
        LineFound := True;
        DeleteElementFromStringArray(DraftRouteArray, DraftRouteArrayPos);
      END;
      Inc(DraftRouteArrayPos);
    END; {WHILE}

    IF ReturnAllRoutes THEN BEGIN
      { return the routes found in the DraftRouteArray array }
      SetLength(DraftRouteArray, 0);
      FOR PossibleRoutesArrayPos := 0 TO High(PossibleRoutesArray) DO
        AppendToStringArray(DraftRouteArray, PossibleRoutesArray[PossibleRoutesArrayPos]);
    END ELSE
      IF NOT RouteFound THEN
        SetLength(DraftRouteArray, 0);

    { and record what's happening }
  //  WriteToLogFileAndTestFile := True;
  //  WriteStringArrayToLog(NoLocoChip, 'X', 'Draft Route Array:', DraftRouteArray, NumberElements);
  //  WriteToLogFileAndTestFile := False;
  //  Debug;
  EXCEPT
    ON E : Exception DO
      Log('EG CreateDraftRouteArray: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CreateDraftRouteArray }

PROCEDURE CreateLockingArrayFromDraftRouteArray(LocoChip : Integer; DraftRouteArray : StringArrayType; OUT LockingArray : StringArrayType);
{ Creates locking based on the route previously found - adds the original line-name data as it's used in drawing the subroute }
CONST
  NumberElements = True;

VAR
  BufferStopFound : Boolean;
  CrossOverPointStr : String;
  FirstLineTC : Integer;
  FirstSignalFound : Integer;
  FirstTrackCircuitFound : Boolean;
  HoldMarkerFound : Boolean;
  I : Integer;
  JourneyStr : String;
  RouteDirection : DirectionType;
  S, TC : Integer;
  SignalFound : Boolean;
  TempDraftRouteArray : StringArrayType;
  TempDraftRouteArrayPos, TempArrayPos : Integer;
  TempPoint : Integer;
  TempSignal, TempBufferStop : Integer;
  TheatreIndicatorPos : Integer;

BEGIN
  TRY
    BufferStopFound := False;
    FirstTrackCircuitFound := False;
    SetLength(LockingArray, 0);
    RouteDirection := UnknownDirection;
    HoldMarkerFound := False;
    JourneyStr := '';
    TempBufferStop := UnknownBufferStop;
    TempSignal := UnknownSignal;
    TheatreIndicatorPos := 0;
    SetLength(TempDraftRouteArray, 0);

    { If there's a hold marker in the draft route array, we delete it - this causes a weird error in the length of the draft route array, in that its length outside this
      subroutine remains the same, so work with a copy instead.
    }
    FOR I := 0 TO High(DraftRouteArray) DO
      AppendToStringArray(TempDraftRouteArray, DraftRouteArray[I]);

    FirstSignalFound := UnknownSignal;
    FirstLineTC := UnknownTC;

    TempDraftRouteArrayPos := 0;
    WHILE (TempDraftRouteArrayPos < Length(TempDraftRouteArray)) DO BEGIN
      IF (Pos('FS=', TempDraftRouteArray[TempDraftRouteArrayPos]) > 0)
      OR (Pos('BS=', TempDraftRouteArray[TempDraftRouteArrayPos]) > 0)
      THEN BEGIN
        IF (Pos('FS=', TempDraftRouteArray[TempDraftRouteArrayPos]) > 0) THEN BEGIN
          { found a signal }
          TempSignal := ExtractSignalFromString(TempDraftRouteArray[TempDraftRouteArrayPos]);
          RouteDirection := Signals[TempSignal].Signal_Direction;
        END ELSE BEGIN
          { found a buffer stop }
          BufferStopFound := True;
          TempBufferStop := ExtractBufferStopFromString(TempDraftRouteArray[TempDraftRouteArrayPos]);
          RouteDirection := BufferStops[TempBufferStop].BufferStop_Direction;
        END;

        { If it's the signal at the end of the route... }
        IF (TempDraftRouteArrayPos = High(TempDraftRouteArray))
        { ...or the start of the next subroute, ... }
        OR (Pos('SR=', TempDraftRouteArray[TempDraftRouteArrayPos + 1]) > 0)
        { or a hold marker preceded by a signal }
        OR (TempDraftRouteArray[TempDraftRouteArrayPos + 1] = HoldMarker)
        THEN BEGIN
           { ...the preceding signal should be on (as it concludes the subroute) unless we're pulling off a semaphore distant }
          IF NOT BufferStopFound THEN BEGIN
//            IF (FirstSignalFound <> unknownSignal)
//            AND (Signals[FirstSignalFound].Signal_Type = SemaphoreDistant)
//            THEN
//              AppendToStringArray(LockingArray, TempDraftRouteArray[TempDraftRouteArrayPos] + '\')
//            ELSE
              AppendToStringArray(LockingArray, TempDraftRouteArray[TempDraftRouteArrayPos] + '=');
          END;

          { Now add the theatre destination }
          IF TheatreIndicatorPos <> 0 THEN BEGIN
            IF NOT BufferStopFound THEN
              LockingArray[TheatreIndicatorPos] := LockingArray[TheatreIndicatorPos] + 'FS=' + IntToStr(TempSignal)
            ELSE
              LockingArray[TheatreIndicatorPos] := LockingArray[TheatreIndicatorPos] + 'BS=' + IntToStr(TempBufferStop);
            TheatreIndicatorPos := 0;
          END;
        END ELSE BEGIN
          { though add theatre/route indicator info if there is one }
          IF (Signals[TempSignal].Signal_Indicator = TheatreIndicator) THEN BEGIN
            { Add the theatre instruction - we will add the destination data later }
            AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + '>');
            { and store its position }
            TheatreIndicatorPos := High(LockingArray);
          END ELSE BEGIN
            SignalFound := False;
            IF Signals[TempSignal].Signal_Indicator = JunctionIndicator THEN BEGIN
              { there is a supplied next signal }
              I := TempDraftRouteArrayPos;

              WHILE (I < Length(TempDraftRouteArray))
              AND NOT SignalFound
              DO BEGIN
                { compare it against the next signal in the array }
                IF (Pos('FS=', TempDraftRouteArray[I]) > 0)
                AND (ExtractSignalFromString(TempDraftRouteArray[I]) <> TempSignal)
                THEN BEGIN
                  SignalFound := True;
                  IF Signals[TempSignal].Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetSignal = ExtractSignalFromString(TempDraftRouteArray[I])
                  THEN
                    AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'UL|')
                  ELSE
                    IF Signals[TempSignal].Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal = ExtractSignalFromString(TempDraftRouteArray[I])
                    THEN
                      AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'ML|')
                    ELSE
                      IF Signals[TempSignal].Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal = ExtractSignalFromString(TempDraftRouteArray[I])
                      THEN
                        AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'LL|')
                      ELSE
                        IF Signals[TempSignal].Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal =
                                                                                                                             ExtractSignalFromString(TempDraftRouteArray[I])
                        THEN
                          AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'UR|')
                        ELSE
                          IF Signals[TempSignal].Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal =
                                                                                                                             ExtractSignalFromString(TempDraftRouteArray[I])
                          THEN
                            AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'MR|')
                          ELSE
                            IF Signals[TempSignal].Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal =
                                                                                                                             ExtractSignalFromString(TempDraftRouteArray[I])
                            THEN
                              AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'LR|')
                            ELSE
                              AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + '.');
                END;
                Inc(I);
              END; {WHILE}

              IF NOT SignalFound
              AND (Pos('BS=', TempDraftRouteArray[High(TempDraftRouteArray)]) > 0)
              THEN BEGIN
                { there's a buffer stop instead }
                TempBufferStop := ExtractBufferStopFromString(TempDraftRouteArray[High(TempDraftRouteArray)]);
                IF Signals[TempSignal].Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetBufferStop = TempBufferStop
                THEN
                  AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'UL|')
                ELSE
                  IF Signals[TempSignal].Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetBufferStop = TempBufferStop
                  THEN
                    AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'ML|')
                  ELSE
                    IF Signals[TempSignal].Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetBufferStop = TempBufferStop
                    THEN
                      AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'LL|')
                    ELSE
                      IF Signals[TempSignal].Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetBufferStop = TempBufferStop
                      THEN
                        AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'UR|')
                      ELSE
                        IF Signals[TempSignal].Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetBufferStop = TempBufferStop
                        THEN
                          AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'MR|')
                        ELSE
                          IF Signals[TempSignal].Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetBufferStop = TempBufferStop
                          THEN
                            AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + 'LR|')
                          ELSE
                            AppendToStringArray(LockingArray, 'FR=' + IntToStr(TempSignal) + '.');
              END;
            END;
          END;

          IF (TempDraftRouteArrayPos = High(TempDraftRouteArray)) THEN
            { the signal at the end of the route must be on (unless it's automatic ****) }
            AppendToStringArray(LockingArray, 'FS=' + IntToStr(TempSignal) + '=')
          ELSE
            AppendToStringArray(LockingArray, 'FS=' + IntToStr(TempSignal) + '\');
        END;
      END;

      { Copy point data too, and check for crossover ponts, three-way points and points potentially locked by catch points }
      IF Pos('FP=', TempDraftRouteArray[TempDraftRouteArrayPos]) > 0 THEN BEGIN
        TempPoint := ExtractPointFromString(TempDraftRouteArray[TempDraftRouteArrayPos]);

        { If it's a point potentially locked by a catch point, add the catch point first }
        IF Points[TempPoint].Point_Type = CatchPointUp THEN BEGIN
          IF Points[Points[TempPoint].Point_OtherPoint].Point_FacingDirection = Down THEN
            AppendToStringArray(LockingArray, 'FP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '/');
        END ELSE
          IF Points[TempPoint].Point_Type = CatchPointDown THEN BEGIN
            IF Points[Points[TempPoint].Point_OtherPoint].Point_FacingDirection = Up THEN
              AppendToStringArray(LockingArray, 'FP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '/');
          END;

        { or, if it's a protected point, switch the catch point first if it doesn't appear later in the draft routeing array }
        IF Points[TempPoint].Point_Type = ProtectedPoint THEN
          IF NOT IsPointInStringArray(TempDraftRouteArray, Points[TempPoint].Point_OtherPoint) THEN
            AppendToStringArray(LockingArray, 'FP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '/');

        { If we want a three-way point A to diverge }
        IF (Points[TempPoint].Point_Type = ThreeWayPointA)
        AND (RightStr(TempDraftRouteArray[TempDraftRouteArrayPos], 1) = '/')
        THEN BEGIN
          { we need to make sure that the B point is straight first }
          Log(LocoChipToStr(LocoChip) + ' P P=' + IntToStr(TempPoint) + ' is a three way A set to diverge, so three way B P=' + IntToStr(Points[TempPoint].Point_OtherPoint)
                                      + ' must be set straight first');
          AppendToStringArray(LockingArray, 'FP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '-');
        END;

        AppendToStringArray(LockingArray, TempDraftRouteArray[TempDraftRouteArrayPos]);
        { and check on any crossover points that would cause a collision if there were an overrun }
        IF (Points[TempPoint].Point_Type = CrossOverPoint)
        AND (Points[TempPoint].Point_OtherPoint <> UnknownPoint)
        THEN BEGIN
          { Otherwise set it to its opposing point's state }
          IF ExtractPointStateFromString(TempDraftRouteArray[TempDraftRouteArrayPos]) = Straight THEN
            CrossOverPointStr := 'XP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '-'
          ELSE
            CrossOverPointStr := 'XP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '/'
        END;

        IF CrossOverPointStr <> '' THEN
          AppendToStringArray(LockingArray, CrossOverPointStr);
      END ELSE
        IF Pos('TP=', TempDraftRouteArray[TempDraftRouteArrayPos]) > 0 THEN BEGIN
          TempPoint := ExtractPointFromString(TempDraftRouteArray[TempDraftRouteArrayPos]);

          { If it's a point potentially locked by a catch point, add the catch point first }
          IF Points[TempPoint].Point_Type = CatchPointUp THEN BEGIN
            IF Points[Points[TempPoint].Point_OtherPoint].Point_FacingDirection = Down THEN
              AppendToStringArray(LockingArray, 'FP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '/');
          END ELSE
            IF Points[TempPoint].Point_Type = CatchPointDown THEN BEGIN
              IF Points[Points[TempPoint].Point_OtherPoint].Point_FacingDirection = Up THEN
                AppendToStringArray(LockingArray, 'FP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '/');
            END;

          { or, if it's a protected point, switch the catch point first if it doesn't appear later in the draft routeing array }
          IF Points[TempPoint].Point_Type = ProtectedPoint THEN
            IF NOT IsPointInStringArray(TempDraftRouteArray, Points[TempPoint].Point_OtherPoint) THEN
              AppendToStringArray(LockingArray, 'FP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '/');

          IF RouteDirection <> UnknownDirection THEN BEGIN
            { If we want a three-way point A to diverge }
            IF (Points[TempPoint].Point_Type = ThreeWayPointA)
            AND (RightStr(TempDraftRouteArray[TempDraftRouteArrayPos], 1) = '/')
            THEN BEGIN
              { we need to make sure that the B point is straight first }
              { Log(LocoChipStr + ' P P=' + IntToStr(TempPoint) + ' is a three way A set to diverge, so three way B P=' + IntToStr(Points[TempPoint].Point_OtherPoint)
                                + ' must be set straight first');
              }
              AppendToStringArray(LockingArray, 'FP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '-');
            END ELSE
              { If we want a three-way point B to diverge }
              IF (Points[TempPoint].Point_Type = ThreeWayPointB)
              AND (RightStr(TempDraftRouteArray[TempDraftRouteArrayPos], 1) = '/')
              THEN BEGIN
                { we need to make sure that the A point is straight first }
                { Log(LocoChipStr + ' P P=' + IntToStr(TempPoint) + ' is a three way B set to diverge, so three way A P=' + IntToStr(Points[TempPoint].Point_OtherPoint)
                                  + ' must be set straight first');
                }
                AppendToStringArray(LockingArray, 'FP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '-');
              END;

            AppendToStringArray(LockingArray, TempDraftRouteArray[TempDraftRouteArrayPos]);
            { and check on any crossover points that would cause a collision if there were an overrun }
            IF (Points[TempPoint].Point_Type = CrossOverPoint)
            AND (Points[TempPoint].Point_OtherPoint <> UnknownPoint)
            THEN BEGIN
              { Otherwise set it to its opposing point's state }
              IF ExtractPointStateFromString(TempDraftRouteArray[TempDraftRouteArrayPos]) = Straight THEN
                CrossOverPointStr := 'XP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '-'
              ELSE
                CrossOverPointStr := 'XP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '/'
            END;
            
            IF CrossOverPointStr <> '' THEN
              AppendToStringArray(LockingArray, CrossOverPointStr);
          END;
        END;

      { add trackcircuit data from lines }
      IF Pos('L=', TempDraftRouteArray[TempDraftRouteArrayPos]) > 0 THEN BEGIN
        { see if there's any TC data - don't do this for the trackcircuit adjacent to the signal, or trains stopped at signals at the start of routes would not be allowed
          to proceed, as that line section would always be marked as occupied. Look out for signals that both start and end a route, however (check for them by seeing if
          the signal is off or not).
        }

        { find the first signal in the array }
        TempArrayPos := -1;
        WHILE (TempArrayPos < High(TempDraftRouteArray))
        AND (FirstLineTC = UnknownTC)
        DO BEGIN
          Inc(TempArrayPos);
          IF ((Pos('FS=', TempDraftRouteArray[TempArrayPos]) > 0)
          AND (Pos('\', TempDraftRouteArray[TempArrayPos]) = 0))
          THEN BEGIN
            FirstSignalFound := ExtractSignalFromString(TempDraftRouteArray[TempArrayPos]);
            FirstLineTC := Lines[Signals[ExtractSignalFromString(TempDraftRouteArray[TempArrayPos])].Signal_AdjacentLine].Line_TC;
          END;
        END; {WHILE}

        IF FirstLineTC = UnknownTC THEN BEGIN
          Log('X Routeing Error: No signal found in TempDraftRouteArray: ');
          WriteStringArrayToLog(LocoChip, 'R', TempDraftRouteArray);
          ShowMessage('Fatal error - see log file');
          Exit;
        END;

        TC := Lines[ExtractLineFromString(TempDraftRouteArray[TempDraftRouteArrayPos])].Line_TC;
        IF TC <> UnknownTC THEN BEGIN
          { the finding of the trackcircuit next to the first signal is recorded lest it also exists at the end of the route! }
          IF FirstTrackCircuitFound OR (FirstLineTC <> TC) THEN BEGIN
            AppendToStringArray(LockingArray, 'TC=' + IntToStr(TC) + '*');
            { add the linename data too as it's used for drawing subroutes }
            AppendToStringArray(LockingArray, TempDraftRouteArray[TempDraftRouteArrayPos]);
          END ELSE
            IF RouteDebuggingMode
            AND NOT (Signals[FirstSignalFound].Signal_FailMsgWritten)
            THEN
              Log(LocoChipToStr(LocoChip) + ' S Line ' + TempDraftRouteArray[TempDraftRouteArrayPos]
                                           + ' is adjacent to S=' + IntToStr(FirstSignalFound)
                                           + ' so first TC=' + IntToStr(FirstLineTC) + ' is ignored');
          FirstTrackCircuitFound := True;
        END;
      END;

      { add signal data for signals not going the same way we are - to check they're not off }
      IF Pos('L=', TempDraftRouteArray[TempDraftRouteArrayPos]) > 0 THEN BEGIN
        FOR S := 0 TO High(Signals) DO BEGIN
//          IF NOT Signals[S].Signal_OutOfUse THEN
            IF Signals[S].Signal_AdjacentLine = ExtractLineFromString(TempDraftRouteArray[TempDraftRouteArrayPos]) THEN
              IF Signals[S].Signal_Direction <> RouteDirection THEN
                AppendToStringArray(LockingArray, 'TS=' + IntToStr(S) + '=');
        END;
      END;
      { copy across journey data }
      IF Pos('J=', TempDraftRouteArray[TempDraftRouteArrayPos]) > 0 THEN
        AppendToStringArray(LockingArray, TempDraftRouteArray[TempDraftRouteArrayPos]);

      { and the hold marker }
      IF Pos(HoldMarker, TempDraftRouteArray[TempDraftRouteArrayPos]) > 0 THEN
        AppendToStringArray(LockingArray, TempDraftRouteArray[TempDraftRouteArrayPos]);

      { subroute number data }
      IF Pos('SR=', TempDraftRouteArray[TempDraftRouteArrayPos]) > 0 THEN BEGIN
        AppendToStringArray(LockingArray, TempDraftRouteArray[TempDraftRouteArrayPos]);
        IF JourneyStr <> '' THEN BEGIN
          AppendToStringArray(LockingArray, JourneyStr);
          JourneyStr := '';
        END;
        IF HoldMarkerFound THEN BEGIN
          AppendToStringArray(LockingArray, HoldMarker);
          HoldMarkerFound := False;
        END;
      END;

      { and finally buffer stop data if any }
      IF (Pos('BS=', TempDraftRouteArray[TempDraftRouteArrayPos]) > 0)
      AND (LockingArray[High(LockingArray)] <> TempDraftRouteArray[TempDraftRouteArrayPos])
      THEN
        AppendToStringArray(LockingArray, TempDraftRouteArray[TempDraftRouteArrayPos]);

      Inc(TempDraftRouteArrayPos)
    END; {WHILE}

    { and, if there are subroutes, remove unnecessary duplicate elements from each subroute rather than from the whole array }
    IF CountSubRoutes(LockingArray) = 0 THEN
      RemoveDuplicateElementsFromStringArray(LockingArray)
    ELSE
      RemoveDuplicateElementsFromStringArray(LockingArray, CountSubRoutes(LockingArray));

    { and record what's happening }
  //  WriteToLogFileAndTestFile := True;
  //  WriteStringArrayToLog(NoLocoChip, 'X', 'new Locking Array:', LockingArray, NumberElements);
  //  WriteToLogFileAndTestFile := False;
  EXCEPT
    ON E : Exception DO
      Log('EG CreateLockingArrayFromDraftRouteArray: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CreateLockingArrayFromDraftRouteArray }

PROCEDURE CreateRouteArrayFromLockingArray(Route : Integer; LockingArray : StringArrayType; OUT RouteArray : StringArrayType);
{ The route is divided into subroutes, each controlled by a signal, which enables partial routes to be set. Now add the signal found at the start of each subroute to the
  end of the section of the array it controls. Start off by adding the signal at the very start of the route (it effectively throws away the signal at the end of the route,
  as that should always be on).
}
VAR
  Done : Boolean;
  LockingArrayPos : Integer;
  SaveIndicatorStringAtStartOfSubRoute : String;
  SaveSignalAtStartOfSubRoute : Integer;
  HoldMarkerFound : Boolean;
  Journey : Integer;
  RouteArrayPos : Integer;
  RouteLen : Real;
  SaveSubRouteMarker : String;
  TC : Integer;
  TrackCircuitPos : Integer;

BEGIN
  TRY
    SaveIndicatorStringAtStartOfSubRoute := '';
    SaveSignalAtStartOfSubRoute := UnknownRoute;
    HoldMarkerFound := False;
    SaveSubRouteMarker := '';
    SetLength(RouteArray, 0);
    TrackCircuitPos := 0;

    LockingArrayPos := 0;
    Done := False;
    { Work through the locking array }
    WHILE (LockingArrayPos <= High(LockingArray))
    AND NOT Done
    DO BEGIN
      IF IsElementSubRouteMarker(LockingArray, LockingArrayPos) THEN BEGIN
        { save it }
        SaveSubRouteMarker := LockingArray[LockingArrayPos];

        { Is there any saved signal data to add? }
        IF SaveIndicatorStringAtStartOfSubRoute <> '' THEN BEGIN
          AppendToStringArray(RouteArray, SaveIndicatorStringAtStartOfSubRoute);
          SaveIndicatorStringAtStartOfSubRoute := '';
        END;

        IF SaveSignalAtStartOfSubRoute <> UnknownRoute THEN
          AppendToStringArray(RouteArray, 'FS=' + IntToStr(SaveSignalAtStartOfSubRoute) + '\');

        IF LockingArrayPos = High(LockingArray) THEN
          { we've finished }
          Done := True
        ELSE BEGIN
          { advance beyond it, saving route indicators and signals for later }
          Inc(LockingArrayPos);

          { see if the second element (the subroute marker is the first) is a journey marker }
          Journey := ExtractJourneyFromString(LockingArray[LockingArrayPos]);
          IF Journey <> UnknownJourney THEN
            { advance beyond it }
            Inc(LockingArrayPos);

          { see if the next element (the subroute marker is the first) is a HOLD marker }
          IF LockingArray[LockingArrayPos] = HoldMarker THEN BEGIN
            HoldMarkerFound := True;
            { and advance beyond it }
            Inc(LockingArrayPos);
          END;
          { or maybe the second element (after the subroute marker) is a route indicator }
          IF IsElementIndicator(LockingArray, LockingArrayPos) THEN BEGIN
            SaveIndicatorStringAtStartOfSubRoute := LockingArray[LockingArrayPos];
            { and advance beyond it }
            Inc(LockingArrayPos);
          END;
          SaveSignalAtStartOfSubRoute := ExtractSignalFromString(LockingArray[LockingArrayPos]);

          { Finally add the subroute number, journey number and hold marker if any to the route array }
          AppendToStringArray(RouteArray, SaveSubRouteMarker);
          SaveSubRouteMarker := '';
          IF Journey <> UnknownJourney THEN
            AppendToStringArray(RouteArray, 'J=' + IntToStr(Journey));

          IF HoldMarkerFound THEN BEGIN
            AppendToStringArray(RouteArray, HoldMarker);
            HoldMarkerFound := False;
          END;

          { Note where it is so we can move the trackcircuit data to the front }
          TrackCircuitPos := High(RouteArray);
        END;
      END ELSE BEGIN
        { Now see if there's a trackcircuit to be moved to the front of the subroute }
        IF ExtractTrackCircuitFromString(LockingArray[LockingArrayPos]) <> UnknownTC THEN BEGIN
          Inc(TrackCircuitPos);
          InsertElementInStringArray(RouteArray, TrackCircuitPos, LockingArray[LockingArrayPos])
        END ELSE
          AppendToStringArray(RouteArray, LockingArray[LockingArrayPos]);
      END;

      Inc(LockingArrayPos);
    END; {WHILE}

    { Calculate how long the route is, if required }
    IF ShowRouteLengths THEN BEGIN
      RouteLen := 0;
      FOR RouteArrayPos := 0 TO High(RouteArray) DO BEGIN
        TC := ExtractTrackCircuitFromString(RouteArray[RouteArrayPos]);
        IF TC <> UnknownTC THEN
          RouteLen := RouteLen + TrackCircuits[TC].TC_LengthInInches;
      END; {FOR}
      Debug('R=' + IntToStr(Route) + ': length=' + FloatToStr(RouteLen));
    END;

    { Finally, add any signal data left over }
    IF SaveIndicatorStringAtStartOfSubRoute <> '' THEN
      AppendToStringArray(RouteArray, SaveIndicatorStringAtStartOfSubRoute);
    IF SaveSignalAtStartOfSubRoute <> UnknownRoute THEN
      AppendToStringArray(RouteArray, 'FS=' + IntToStr(SaveSignalAtStartOfSubRoute) + '\');

  //WriteStringArrayToLog(NoLocoChip, 'X', 'new ra:', routearray);
  //Debug;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CreateRouteArrayFromLockingArray: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CreateRouteArrayFromLockingArray }

PROCEDURE FindRouteFromLineAToLineB(LocoChip, Journey, S, StartLine, EndLine : Integer; Direction : DirectionType; TrainType : TypeOfTrainType; TrainLength : Integer;
                                    EmergencyRouteing, AreOutOfUseLinesIncluded : Boolean; OUT DraftRouteArray : StringArrayType;
                                    OUT LinesNotAvailableStr, ErrorMsg : String; OUT RouteFound : Boolean);
{ Uses line names to find a route from A to B }
CONST
  ActiveTrain = True;
  LookForASpecificSignalOrBufferStop = True;
  LookForAllSignalsAndBufferStopsOnly = True;
  LookForNextSignalOrBufferStopOnly = True;
  NumberElements = True;
  ReturnAllRoutes = True;
  ReturnTheSpecifiedRoute = True;

VAR
  CurrentLine : Integer;
  DebugStr : String;
  I : Integer;
  IndicatorState : IndicatorStateType;
  LocoChipStr : String;
  NextBufferStop : Integer;
  NextSignal : Integer;
  SubRoute : Integer;
  TempS : Integer;

BEGIN
  TRY
    { Is it a route we know about? }
    SetLength(DraftRouteArray, 0);
    RouteFound:= False;
    NextSignal := UnknownSignal;
    RouteFound := False;
    SubRoute := UnknownRoute;
    IndicatorState := NoIndicatorLit;
    LocoChipStr := LocoChipToStr(LocoChip);

    { Now start the search }
    IF RouteDebuggingMode THEN
      DrawLine(StartLine, clYellow, NOT ActiveTrain);

    IF StartLine = EndLine THEN BEGIN
      ErrorMsg := 'Cannot create a route if the startline and endline are the same';
      Log(LocoChipStr + ' R ' + ErrorMsg);
    END ELSE BEGIN
      DebugStr := 'Creating R=' + DirectionToStr(Direction, ShortStringType) + ' from ' + LineToStr(StartLine) + ' to ' + LineToStr(EndLine);
      log('X ' + debugstr);

      IF StartLine = UnknownLine THEN BEGIN
        ErrorMsg := 'cannot create a route with only the endline (' + LineToStr(EndLine) + ') specified';
        Log(LocoChipStr + ' RG ' + IfThen(Journey <> UnknownJourney,
                                         DisplayJourneyNumber(Journey))
                        + ErrorMsg);
        RouteFound := False;
      END ELSE
        CreateDraftRouteArray(LocoChip, DraftRouteArray, StartLine, EndLine, Direction, IndicatorState, TrainType, TrainLength, NextSignal, NextBufferStop,
                              ReturnTheSpecifiedRoute, NOT ReturnAllRoutes, NOT LookForNextSignalOrBufferStopOnly, NOT LookForASpecificSignalOrBufferStop,
                              AreOutOfUseLinesIncluded, NOT LookForAllSignalsAndBufferStopsOnly, LinesNotAvailableStr, EmergencyRouteing, RouteFound);

      IF RouteFound THEN BEGIN
         { First save the route to avoid searching for it again unless we are already using it }
        DebugStr := IfThen(Journey <> UnknownJourney,
                           DisplayJourneyNumber(Journey))
                    + IfThen(S <> UnknownSignal,
                             'S=' + IntToStr(S));
        IF DebugStr = '' THEN
          DebugStr := 'Found'
        ELSE
          DebugStr := DebugStr + ': found';

        IF NOT EmergencyRouteing THEN
          Log(LocoChipStr + ' R ' + DebugStr
                          + ' route by searching'
                          + ' from ' + LocationToStr(Lines[StartLine].Line_Location, ShortStringType) + ' ' + DirectionToStr(Direction)
                          +  ' to ' + LocationToStr(Lines[EndLine].Line_Location, ShortStringType) + ' (' + LineToStr(StartLine) + ' to ' + LineToStr(EndLine) + ')')
        ELSE
          Log(LocoChipStr + ' R ' + DebugStr
                          + ' (emergency) route by searching'
                          + ' from ' + LocationToStr(Lines[StartLine].Line_Location, ShortStringType) + ' ' + DirectionToStr(Direction)
                          +  ' to ' + LocationToStr(Lines[EndLine].Line_Location, ShortStringType) + ' (' + LineToStr(StartLine) + ' to ' + LineToStr(EndLine) + ')');

        { Show what we've found }
        IF RouteDrawingMode OR RouteDebuggingMode THEN
          DrawAllPoints;
        { and record any adjacent signal if it's going the same way we are - to allow routeing from signal to signal }
        I := 0;
        WHILE I < High(DraftRouteArray) DO BEGIN
          CurrentLine := ExtractLineFromString(DraftRouteArray[I]);
          IF CurrentLine = UnknownLine THEN
            Inc(I)
          ELSE
            { now a signal to insert in RouteArray }
            IF (GetLineAdjacentSignal(CurrentLine) <> UnknownSignal)
    //        AND NOT Signals[Lines[CurrentLine].Line_AdjacentSignal].Signal_OutOfUse
            AND (Direction = Signals[GetLineAdjacentSignal(CurrentLine)].Signal_Direction)
            THEN BEGIN
              Inc(I);
              InsertElementInStringArray(DraftRouteArray, I, 'FS=' + IntToStr(GetLineAdjacentSignal(CurrentLine)));
            END ELSE
              { now buffer stops }
              IF (Lines[CurrentLine].Line_AdjacentBufferStop <> UnknownBufferStop) THEN BEGIN
                Inc(I);
                InsertElementInStringArray(DraftRouteArray, I, 'BS=' + IntToStr(Lines[CurrentLine].Line_AdjacentBufferStop));
              END ELSE
                { found a line }
                Inc(I);
        END; {WHILE}

        RemoveDuplicateElementsFromStringArray(DraftRouteArray);

        { Add subroute numbers, and copy the signal number across to start the new subroute }
        I := 0;
        WHILE I < Length(DraftRouteArray) DO BEGIN
          IF Pos('FS=', DraftRouteArray[I]) > 0 THEN BEGIN
            IF SubRoute = UnknownSubroute THEN
              SubRoute := 0
            ELSE
              Inc(SubRoute);
            IF SubRoute <> 0 THEN BEGIN
              { now insert the signal number again }
              InsertElementInStringArray(DraftRouteArray, I + 1, DraftRouteArray[I]);
              Inc(I);
            END;
            { insert the subroute number }
            InsertElementInStringArray(DraftRouteArray, I, 'SR=' + IntToStr(SubRoute));
            Inc(I);

            { if it's a possible route hold signal, add the hold marker before inserting the signal number }
            TempS := ExtractSignalFromString(DraftRouteArray[I]);
            IF TempS = UnknownSignal THEN BEGIN
              ErrorMsg := 'Error in FindRouteFromLineAToLineB: DraftRouteArray[' + IntToStr(I) + '] does not contain a signal';
              Log('XG ' + ErrorMsg);
            END ELSE BEGIN
              IF (Signals[TempS].Signal_PossibleRouteHold) OR (Signals[TempS].Signal_PossibleStationStartRouteHold) THEN BEGIN
                InsertElementInStringArray(DraftRouteArray, I, HoldMarker);
                Inc(I);
              END;
            END;
          END;
          Inc(I);
        END; {WHILE}

        { and the final signal or buffer stop (needed for theatre indicators) }
        IF GetLineAdjacentSignal(EndLine) <> UnknownSignal THEN
          AppendToStringArray(DraftRouteArray, 'FS=' + IntToStr(GetLineAdjacentSignal(EndLine)))
        ELSE
          IF Lines[EndLine].Line_AdjacentBufferStop <> UnknownBufferStop THEN
            AppendToStringArray(DraftRouteArray, 'BS=' + IntToStr(Lines[EndLine].Line_AdjacentBufferStop))
          ELSE BEGIN
            ErrorMsg := 'Routeing Error: ' + LineToStr(EndLine) + ' does not have an adjacent signal or buffer stop';
            Log('XG ' + ErrorMsg);
          END;
      END;

    //DrawLineInLogFile(nolocochip, 'X', '"');
    //          WriteStringArrayToLog(LocoChip, 'R', 'J=' + IntToStr(Journey) + ': '
    //                                                     + 'XXXXX'
    //                                                     + ': ', DraftRouteArray, 2, 190, 'SR=');
    //DrawLineInLogFile(nolocochip, 'X', '"');
    //debug;
    //      IF DebugStr <> '' THEN BEGIN
    //        Debug(DebugStr);
    //        Log(LocoChip, 'R', DebugStr);
    //        SaveDebugStr := DebugStr;
    //      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG FindRouteFromLineAToLineB: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FindRouteFromLineAToLineB }

PROCEDURE FindAndHighlightAllRoutes(RouteStartLine : Integer; RouteDirection : DirectionType; TrainType : TypeOfTrainType; TrainLength : Integer;
                                    OUT DraftRouteArray : StringArrayType; OUT OK : Boolean);
{ Looks for all possible routes, highlights them and returns a list of them }
CONST
  EmergencyRouteing = True;
  LookForASpecificSignalOrBufferStop = True;
  LookForAllSignalsAndBufferStopsOnly = True;
  IncludeOutOfUseLines = True;
  LookForNextSignalOrBufferStopOnly = True;
  ReturnAllRoutes = True;
  ReturnTheSpecifiedRoute = True;

VAR
  LinesNotAvailableStr : String;
  NextBufferStop : Integer;
  NextSignal : Integer;
  RouteFound : Boolean;

BEGIN
  NextSignal := UnknownSignal;

  { The main finding procedure }
  CreateDraftRouteArray(NoLocoChip, DraftRouteArray, RouteStartLine, UnknownLine, RouteDirection, NoIndicatorLit, TrainType, TrainLength, NextSignal, NextBufferStop,
                        NOT ReturnTheSpecifiedRoute, ReturnAllRoutes, NOT LookForNextSignalOrBufferStopOnly, NOT LookForASpecificSignalOrBufferStop,
                        NOT IncludeOutOfUseLines, NOT LookForAllSignalsAndBufferStopsOnly, LinesNotAvailableStr, NOT EmergencyRouteing, RouteFound);
END; { FindAndHighlightAllRoutes }

PROCEDURE FindAndHighlightNearestSignalsToTheatreIndicator(RouteStartLine : Integer; RouteDirection : DirectionType; OUT DraftRouteArray : StringArrayType;
                                                           OUT OK : Boolean);
{ Looks for the nearest signals to a given theatre indicator, highlights them and returns a list of them if the route is available }
CONST
  EmergencyRouteing = True;
  LookForASpecificSignalOrBufferStop = True;
  LookForAllSignalsAndBufferStopsOnly = True;
  IncludeOutOfUseLines = True;
  LookForNextSignalOrBufferStopOnly = True;
  ReturnAllRoutes = True;
  ReturnTheSpecifiedRoute = True;

VAR
  LinesNotAvailableStr : String;
  NextBufferStop : Integer;
  NextSignal : Integer;
  RouteFound : Boolean;

BEGIN
  NextSignal := UnknownSignal;

  { The main finding procedure }
  CreateDraftRouteArray(NoLocoChip, DraftRouteArray, RouteStartLine, UnknownLine, RouteDirection, NoIndicatorLit, UnknownTrainType, UnknownTrainLength, NextSignal,
                        NextBufferStop, NOT ReturnTheSpecifiedRoute, ReturnAllRoutes, NOT LookForNextSignalOrBufferStopOnly, NOT LookForASpecificSignalOrBufferStop,
                        NOT IncludeOutOfUseLines, LookForAllSignalsAndBufferStopsOnly, LinesNotAvailableStr, NOT EmergencyRouteing, RouteFound);
END; { FindAndHighlightNearestSignals }

PROCEDURE FindNearestSignalsToGivenSignal(RouteStartLine : Integer; RouteDirection : DirectionType; OUT DraftRouteArray : StringArrayType; OUT OK : Boolean);
{ Looks for nearest signals only and returns a list of them - used in setting up routes in advance }
CONST
  EmergencyRouteing = True;
  LookForASpecificSignalOrBufferStop = True;
  LookForAllSignalsAndBufferStopsOnly = True;
  IncludeOutOfUseLines = True;
  LookForNextSignalOrBufferStopOnly = True;
  ReturnAllRoutes = True;
  ReturnTheSpecifiedRoute = True;

VAR
  LinesNotAvailableStr : String;
  NextBufferStop : Integer;
  NextSignal : Integer;
  RouteFound : Boolean;

BEGIN
  NextSignal := UnknownSignal;

  { The main finding procedure }
  CreateDraftRouteArray(NoLocoChip, DraftRouteArray, RouteStartLine, UnknownLine, RouteDirection, NoIndicatorLit, UnknownTrainType, UnknownTrainLength, NextSignal,
                        NextBufferStop, NOT ReturnTheSpecifiedRoute, ReturnAllRoutes, NOT LookForNextSignalOrBufferStopOnly, NOT LookForASpecificSignalOrBufferStop,
                        IncludeOutOfUseLines, LookForAllSignalsAndBufferStopsOnly, LinesNotAvailableStr, NOT EmergencyRouteing, RouteFound);
END; { FindNearestSignalsToGivenSignal }

FUNCTION GetResettingTrackCircuit(LocoChip, S : Integer; SuppressMessage : Boolean) : Integer;
{ Extract the resetting trackcircuit (if any) from the locking array }
VAR
  LockingArrayPos : Integer;
  TrackCircuitFound : Boolean;

BEGIN
  Result := 0;
  TRY
    WITH Signals[S] DO BEGIN
      LockingArrayPos := -1;
      TrackCircuitFound := False;

      REPEAT
        Inc(LockingArrayPos);
        IF Length(Signal_RouteLockingNeededArray) = 0 THEN
          Log('XG GetResettingSignal Routine Error: Locking array for S=' + IntToStr(S) + ' is empty')
        ELSE BEGIN
          { work through the lock list seeking the first trackcircuit - but choose the second if the first is where we are currently }
          IF Pos('TC=', Signal_RouteLockingNeededArray[LockingArrayPos]) > 0 THEN BEGIN
            Result := ExtractTrackCircuitFromString(Signal_RouteLockingNeededArray[LockingArrayPos]);
            IF Result <> Signals[S].Signal_AdjacentTC THEN BEGIN
              TrackCircuitFound := True;
              TrackCircuits[Result].TC_ResettingSignal := S;
            END;
          END;
        END;
      UNTIL (LockingArrayPos = High(Signal_RouteLockingNeededArray)) OR TrackCircuitFound OR (Length(Signal_RouteLockingNeededArray) = 0);

      IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
        IF TrackCircuitFound THEN BEGIN
          IF NOT SuppressMessage THEN
            Log(LocoChipToStr(LocoChip) + ' S Resetting trackcircuit for S=' + IntToStr(S) + ' is TC=' + IntToStr(Result))
        END ELSE
          Log('XG GetResettingSignal Routine Error: No resetting trackcircuit found for S=' + IntToStr(S));
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG GetResettingTrackCircuit: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { GetResettingTrackCircuit }

PROCEDURE FindPreviousSignals(S : Integer; OUT PreviousSignal1, PreviousSignal2 : Integer);
{ Looks for the signals before ours, so we can change their aspects too. We just go in reverse, following how the points are set - if a signal is off, it must be previous
  to us, as the points it controls would be locked.
}
VAR
  CurrentLine : Integer;
  DebugStr : String;
  ExitFunction : Boolean;
  Next : NextLineRouteingType;
  NextPoint : Integer;
  SearchDirection : DirectionType;
  SignalCount : Integer;

BEGIN
  TRY
    DebugStr := 'PS:';
    SignalCount := 0;
    ExitFunction := False;

    IF Signals[S].Signal_Direction = Up THEN
      SearchDirection := Down
    ELSE
      SearchDirection := Up;

    CurrentLine := Signals[S].Signal_AdjacentLine;

    WHILE NOT ExitFunction DO BEGIN
      { see if we've found a signal - but not the one we started with! }
      IF (CurrentLine <> Signals[S].Signal_AdjacentLine)
      AND (GetLineAdjacentSignal(CurrentLine) <> UnknownSignal)
      AND (Signals[GetLineAdjacentSignal(CurrentLine)].Signal_Direction <> SearchDirection)
      THEN BEGIN
        IF ((Signals[GetLineAdjacentSignal(CurrentLine)].Signal_Indicator = TheatreIndicator)
           AND (Signals[GetLineAdjacentSignal(CurrentLine)].Signal_IndicatorState = TheatreIndicatorLit))
        OR (Signals[GetLineAdjacentSignal(CurrentLine)].Signal_Aspect <> RedAspect)
        { include sempahore distants as they obey different rules }
        OR (Signals[GetLineAdjacentSignal(CurrentLine)].Signal_Type = SemaphoreDistant)
        THEN BEGIN
          Inc(SignalCount);
          CASE SignalCount OF
            1:
              PreviousSignal1 := GetLineAdjacentSignal(CurrentLine);
            2:
              BEGIN
                PreviousSignal2 := GetLineAdjacentSignal(CurrentLine);
                ExitFunction := True;
              END;
          END; {CASE}
        END ELSE
          ExitFunction := True;
      END;

      IF NOT ExitFunction THEN BEGIN
        IF SearchDirection = Up THEN
          Next := Lines[CurrentLine].Line_NextUpType
        ELSE
          Next := Lines[CurrentLine].Line_NextDownType;

        CASE Next OF
          PointIsNext:
            BEGIN
              IF SearchDirection = Up THEN
                NextPoint := Lines[CurrentLine].Line_NextUpPoint
              ELSE
                NextPoint := Lines[CurrentLine].Line_NextDownPoint;

              { where to go next }
              IF SearchDirection = Points[NextPoint].Point_FacingDirection THEN BEGIN
                { a facing point - see which way it's set }
                IF Points[NextPoint].Point_PresentState = Straight THEN BEGIN
                  IF LockDebuggingMode THEN
                    DrawPoint(NextPoint, clLime);
                  DebugStr := DebugStr + ' FP=' + IntToStr(NextPoint) + '-';
                  CurrentLine := Points[NextPoint].Point_StraightLine
                END ELSE
                  IF (Points[NextPoint].Point_Type = CatchPointUp)
                  OR (Points[NextPoint].Point_Type = CatchPointDown)
                  OR (Points[NextPoint].Point_PresentState = Straight)
                  THEN BEGIN
                    DebugStr := DebugStr + ' FP=' + IntToStr(NextPoint) + '?';
                    IF LockDebuggingMode THEN
                      DrawPoint(NextPoint, clRed);
                    ExitFunction := True;
                  END ELSE BEGIN
                    IF LockDebuggingMode THEN
                      DrawPoint(NextPoint, clLime);
                    DebugStr := DebugStr + ' FP=' + IntToStr(NextPoint) + '/';
                    CurrentLine := Points[NextPoint].Point_DivergingLine;
                  END;
              END ELSE BEGIN
                { a trailing point - if it's not set in our direction, stop searching here }
                IF (CurrentLine = Points[NextPoint].Point_StraightLine)
                AND (Points[NextPoint].Point_PresentState = Straight)
                THEN BEGIN
                  CurrentLine := Points[NextPoint].Point_HeelLine;
                  IF LockDebuggingMode THEN
                    DrawPoint(NextPoint, clLime)
                  ELSE
                    DrawPoint(NextPoint, ForegroundColour);
                  DebugStr := DebugStr + ' TP=' + IntToStr(NextPoint) + '-';
                END ELSE
                  IF (CurrentLine = Points[NextPoint].Point_DivergingLine)
                  AND (Points[NextPoint].Point_PresentState = Diverging)
                  THEN BEGIN
                    CurrentLine := Points[NextPoint].Point_HeelLine;
                    IF LockDebuggingMode THEN
                      DrawPoint(NextPoint, clLime)
                    ELSE
                      DrawPoint(NextPoint, ForegroundColour);
                    DebugStr := DebugStr + ' TP=' + IntToStr(NextPoint) + '/';
                  END ELSE BEGIN
                    { if it's not set in our direction, stop searching here }
                    IF LockDebuggingMode THEN
                      DrawPoint(NextPoint, clRed)
                    ELSE
                      DrawPoint(NextPoint, ForegroundColour);
                    DebugStr := DebugStr + ' TP=' + IntToStr(NextPoint) + '#';
                    ExitFunction := True;
                  END;
              END;
            END;
          LineIsNext:
            BEGIN
              { where to go next }
              IF SearchDirection = Up THEN
                CurrentLine := Lines[CurrentLine].Line_NextUpLine
              ELSE
                CurrentLine := Lines[CurrentLine].Line_NextDownLine;
            END;
          EndOfLineIsNext:
            ExitFunction := True;
          UnknownNextLineRouteingType:
            ShowMessage('UnknownNextLineRouteingType at ' + LineToStr(CurrentLine));
        END; {CASE}
      END;
    END; {WHILE}
  //  Debug(DebugStr);
  EXCEPT
    ON E : Exception DO
      Log('EG FindPreviousSignals: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { FindPreviousSignals }

FUNCTION FindNextSignalOrBufferStop(CurrentSignal, NextSignal, NextBufferStop : Integer; IndicatorToBeSet : Boolean; OUT LinesNotAvailableStr : String;
                                    OUT RouteArray : StringArrayType) : Boolean;
{ Looks for the next signal or buffer stop using the rules in the LineAvailable procedure }
CONST
  EmergencyRouteing = True;
  LookForASpecificSignalOrBufferStop = True;
  LookForAllSignalsAndBufferStopsOnly = True;
  IncludeOutOfUseLines = True;
  LookForNextSignalOrBufferStopOnly = True;
  ReturnAllRoutes = True;
  ReturnTheSpecifiedRoute = True;

VAR
  IndicatorState : IndicatorStateType;
  RouteDirection : DirectionType;
  RouteFound : Boolean;
  StartLine, EndLine : Integer;

BEGIN
  Result := FAlse;

  TRY
    LinesNotAvailableStr := '';
    RouteFound := False;
    IndicatorState := NoIndicatorLit;

    IF (GetSignalAspect(CurrentSignal) <> RedAspect) THEN
      Debug('S=' + IntToStr(CurrentSignal) + ' is not on in routine FindNextSignalOrBufferStop')
    ELSE BEGIN
      WITH Signals[CurrentSignal] DO BEGIN
        StartLine := Signal_AdjacentLine;
        RouteDirection := Signal_Direction;

        IF NOT IndicatorToBeSet THEN BEGIN
          IndicatorState := Signal_IndicatorState;
          IF Signal_NextSignalIfNoIndicator <> UnknownSignal THEN
            NextSignal := Signal_NextSignalIfNoIndicator;
        END ELSE BEGIN
          { indicator is set }
          IF Signal_Indicator = JunctionIndicator THEN
            IndicatorState := Signals[CurrentSignal].Signal_IndicatorState;
        END;
      END; {WITH}

      EndLine := UnknownLine;

      IF (NextSignal <> UnknownSignal) OR (NextBufferStop <> UnknownBufferStop) THEN
        { Look for a specific signal and resetting trackcircuit }
        CreateDraftRouteArray(NoLocoChip, RouteArray, StartLine, EndLine, RouteDirection, IndicatorState, UnknownTrainType, UnknownTrainLength, NextSignal, NextBufferStop,
                              NOT ReturnTheSpecifiedRoute, NOT ReturnAllRoutes, LookForNextSignalOrBufferStopOnly, LookForASpecificSignalOrBufferStop,
                              NOT IncludeOutOfUseLines, NOT LookForAllSignalsAndBufferStopsOnly, LinesNotAvailableStr, NOT EmergencyRouteing, RouteFound)
      ELSE
        { Look for the next signal and resetting trackcircuit }
        CreateDraftRouteArray(NoLocoChip, RouteArray, StartLine, EndLine, RouteDirection, IndicatorState, UnknownTrainType, UnknownTrainLength, NextSignal, NextBufferStop,
                              NOT ReturnTheSpecifiedRoute, NOT ReturnAllRoutes, LookForNextSignalOrBufferStopOnly, NOT LookForASpecificSignalOrBufferStop,
                              NOT IncludeOutOfUseLines, NOT LookForAllSignalsAndBufferStopsOnly, LinesNotAvailableStr, NOT EmergencyRouteing, RouteFound);

      IF RouteFound THEN
        { convert the route array traced into a locking array for the current signal }
        IF NextSignal <> UnknownSignal THEN
          AppendToStringArray(RouteArray, 'FS=' + IntToStr(NextSignal));
    END;
    Result := RouteFound;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG FindNextSignalOrBufferStop: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FindNextSignalOrBufferStop }

PROCEDURE CreateInitialRouteRelatedArrays(T : Train; LocoChip : Integer; RouteArray : StringArrayType; AutoRouteSetting : Boolean;
                                          StartSignal, EndSignal, EndBufferStop, StartLine, EndLine : Integer);
{ Set up the various route-related arrays }
VAR
  I : Integer;
  SubRouteCount : Integer;
  SubRouteCountStart : Integer;
  TotalSubRoutes : Integer;

  PROCEDURE CreateSubRouteArrays(SubRouteCountStart : Integer);
  { Create subroute arrays by copying the route array into the array of array of arrays! }
  VAR
    LookForStartLine : Boolean;
    RouteArrayPos : Integer;
    SubRouteArrayPos : Integer;
    SubRoute : Integer;

  BEGIN
    LookForStartLine := False;
    RouteArrayPos := 0;
    SubRouteArrayPos := 0;
    SubRouteCount := SubRouteCountStart;

    WHILE RouteArrayPos < Length(RouteArray) DO BEGIN
      IF (Pos('SR=', RouteArray[RouteArrayPos]) > 0) THEN BEGIN
        { see if the subroute needs internally renumbering (e.g. if it's a second routeing) }
        SubRoute := ExtractSubRouteFromString(RouteArray[RouteArrayPos]);
        Inc(SubRouteCount);
        IF SubRoute <> SubRouteCount THEN
          RouteArray[RouteArrayPos] := 'SR=' + IntToStr(SubRouteCount);
        SubRouteArrayPos := 0;
        
        { Set up the arrays }
        SetLength(Routes_SubRouteStartSignals[Routes_RouteCounter], SubRouteCount + 1);
        SetLength(Routes_SubRouteStartLines[Routes_RouteCounter], SubRouteCount + 1);
        SetLength(Routes_SubRouteEndLines[Routes_RouteCounter], SubRouteCount + 1);

        { Look for a subroute starting signal, which is actually the previous subroute's ending signal - that's why they precede "SR="s. Doesn't of course work for the
          first subroute.
        }
        IF SubRouteCount > (SubRouteCountStart + 1) THEN
          Routes_SubRouteStartSignals[Routes_RouteCounter, SubRouteCount - 1] := ExtractSignalFromString(RouteArray[RouteArrayPos - 1]);

        { and look out for the starting line name }
        LookForStartLine := True;
      END;

      IF RouteArrayPos = High(RouteArray) THEN
        { if we're at the end of the array, there is no subroute to follow, so the last item in the route array must be the start signal for the final subroute }
        Routes_SubRouteStartSignals[Routes_RouteCounter, SubRouteCount] := ExtractSignalFromString(RouteArray[RouteArrayPos]);

      { Add line names to the subroute linename array }
      IF LookForStartLine THEN BEGIN
        { look for the first linename in the subroute }
        IF Pos('L=', RouteArray[RouteArrayPos]) > 0 THEN BEGIN
          Routes_SubRouteStartLines[Routes_RouteCounter, SubRouteCount] := ExtractLineFromString(RouteArray[RouteArrayPos]);
          LookForStartLine := False;
        END;
      END;

      { Store every line in the subroute - by the end of the array it will be the last one! }
      IF Pos('L=', RouteArray[RouteArrayPos]) > 0 THEN
        Routes_SubRouteEndLines[Routes_RouteCounter, SubRouteCount] := ExtractLineFromString(RouteArray[RouteArrayPos]);

      { Finally, copy the data in, looking out for the last line }
      SetLength(Routes_SubRouteSettingStrings[Routes_RouteCounter, SubRouteCount], SubRouteArrayPos + 1);
      Routes_SubRouteSettingStrings[Routes_RouteCounter, SubRouteCount, SubRouteArrayPos] := RouteArray[RouteArrayPos];
      Inc(RouteArrayPos);
      Inc(SubRouteArrayPos);
    END; {WHILE}
  END; { CreateSubRouteArrays }

BEGIN
  TRY
    { Make sure there are no duplicates in the array, as it will cause problems later }
    RemoveDuplicateElementsFromStringArray(RouteArray, CountSubRoutes(RouteArray));

    { Note which loco it's for }
    AppendToIntegerArray(Routes_LocoChips, LocoChip);

    { and which train, if any }
    AppendToTrainArray(Routes_Trains, T);

    { Set up the route recording arrays }
    AppendToIntegerArray(Routes_StartSignals, StartSignal);
    IF EndBufferStop = UnknownBufferStop THEN BEGIN
      AppendToIntegerArray(Routes_EndSignals, EndSignal);
      AppendToIntegerArray(Routes_EndBufferStops, UnknownBufferStop);
    END ELSE BEGIN
      AppendToIntegerArray(Routes_EndSignals, UnknownSignal);
      AppendToIntegerArray(Routes_EndBufferStops, EndBufferStop);
    END;

    AppendToIntegerArray(Routes_Journeys, UnknownJourney);
    AppendToLineArray(Routes_StartLines, StartLine);
    AppendToLineArray(Routes_EndLines, EndLine);

    { Need to keep a note of how far we've got in route setting in auto mode }
    AppendToIntegerArray(Routes_CurrentSettingSubRoute, 0);
    AppendToIntegerArray(Routes_CurrentClearingSubRoute, 0);
    AppendToIntegerArray(Routes_CurrentSubRouteSettingPos, 0);
    AppendToIntegerArray(Routes_CurrentSubRouteClearingPos, 0);

    { Set up some Boolean variables }
    AppendToBooleanArray(Routes_Cleared, False);
    IF NOT AutoRouteSetting THEN
      AppendToBooleanArray(Routes_AutoSettingMode, False)
    ELSE
      AppendToBooleanArray(Routes_AutoSettingMode, True);
    AppendToBooleanArray(Routes_SettingUpFailuresMsgWrittenArray, False);
    AppendToBooleanArray(Routes_ClearingFailuresMsg1WrittenArray, False);
    AppendToBooleanArray(Routes_ClearingFailuresMsg2WrittenArray, False);
    AppendToBooleanArray(Routes_ClearingFailuresMsg3WrittenArray, False);
    AppendToIntegerArray(Routes_PointResultPendingPoint, UnknownPoint);
    AppendToBooleanArray(Routes_PointResultPendingPointMsgWrittenArray, False);
    AppendToDateTimeArray(Routes_PossibleRerouteTime, 0);
    AppendToBooleanArray(Routes_RouteClearingsInProgress, False);
    AppendToBooleanArray(Routes_RouteClearingsWithoutPointResetting, False);
    AppendToBooleanArray(Routes_RouteingsCancelled, False);
    AppendToBooleanArray(Routes_RoutesSettingUpHeldMsgWrittenArray, False);
    AppendToStringArray(Routes_RoutesSettingUpStalledMsgArray, '');
    AppendToBooleanArray(Routes_RoutesSettingUpStalledMsgWrittenArray, False);
    AppendToDateTimeArray(Routes_RoutesSettingUpStalledTimeArray, 0);
    AppendToBooleanArray(Routes_SubRoutesAheadNotClearMsgWrittenArray, False);

    { Note which new route number is active }
    Inc(Routes_RouteCounter);
    AppendToIntegerArray(Routes_Routes, Routes_RouteCounter);
    { and tell the system to start }
    AppendToBooleanArray(Routes_RouteSettingsInProgress, True);
    { and how many subroutes it has... }
    TotalSubRoutes := CountSubRoutes(RouteArray);
    AppendToIntegerArray(Routes_TotalSubRoutes, TotalSubRoutes);
    { or whether we've finished }
    AppendToBooleanArray(Routes_RouteSettingsCompleted, False);

    { ...and their initial setting }
    { Add one row to the subroute multidimensional array, then add its columns [there seems to no way to combine the two operations - if one tries, one gets all the rows the
      same length!]
    }
    SetLength(Routes_SubRouteStates, Routes_RouteCounter + 1);
    SetLength(Routes_SubRouteStates[Routes_RouteCounter], TotalSubRoutes);
    FOR I := 0 TO (TotalSubRoutes - 1) DO
      Routes_SubRouteStates[Routes_RouteCounter, I] := SubRouteNotYetSetUp;

    { Add one row to the strings multidimensional array, then add its columns }
    SetLength(Routes_SubRouteSettingStrings, Routes_RouteCounter + 1);
    SetLength(Routes_SubRouteSettingStrings[Routes_RouteCounter], TotalSubRoutes);
    SetLength(Routes_SubRouteClearingStrings, Routes_RouteCounter + 1);
    SetLength(Routes_SubRouteClearingStrings[Routes_RouteCounter], TotalSubRoutes);

    { Other subroute arrays }
    SetLength(Routes_SubRouteStartSignals, Routes_RouteCounter + 1);
    SetLength(Routes_SubRouteStartLines, Routes_RouteCounter + 1);
    SetLength(Routes_SubRouteEndLines, Routes_RouteCounter + 1);

    { and the approach control }
    AppendToBooleanArray(Routes_ApproachControlsSet, False);
    SetLength(Routes_ApproachControlledSignals, Routes_RouteCounter + 1);
    SetLength(Routes_ApproachControlSignalsWaitingToBeSet, Routes_RouteCounter + 1);
    AppendToBooleanArray(Routes_ApproachControlSignalsMsgWrittenArray, False);

    SubRouteCountStart := -1;
    CreateSubRouteArrays(SubRouteCountStart);
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CreateRouteArrays: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CreateRouteArrays }

PROCEDURE CreateClearingSubRouteArray(Route, SubRoute : Integer);
{ Takes the subroute setting up array and converts it to clear the subroute }
VAR
  I : Integer;
  RouteItemToCheck, NewRouteItem : String;
  TempPoint : Integer;
  PointsSeenArray : IntegerArrayType;

BEGIN
  TRY
    SetLength(PointsSeenArray, 0);
    SetLength(Routes_SubRouteClearingStrings[Route, SubRoute], 0);

    I := 0;
    WHILE I <= High(Routes_SubRouteSettingStrings[Route, SubRoute]) DO BEGIN
      RouteItemToCheck := Routes_SubRouteSettingStrings[Route, SubRoute, I];
      IF (Pos('FS=', RouteItemToCheck) > 0) THEN BEGIN
        { ignore any route indicator items, and move signal on commands to the start of each subroute because the points cannot be unlocked until the signal is on }
        IF Copy(RouteItemToCheck, Length(RouteItemToCheck), 1) = '\' THEN BEGIN
          NewRouteItem := Copy(RouteItemToCheck, 1, Length(RouteItemToCheck) - 1) + '=';
          InsertElementInStringArray(Routes_SubRouteClearingStrings[Route, SubRoute], 1, NewRouteItem);
        END;
      END ELSE
        { and also copy the line data, to undraw the lines, and the point data - to unlock them, and restore them to their default state if required - but ignore trailing
          signals ("TS=").
        }
        IF (Pos('L=', RouteItemToCheck) > 0)
        OR (Pos('XP=', RouteItemToCheck) > 0)
        OR (Pos('TC=', RouteItemToCheck) > 0)
        OR (Pos('SR=', RouteItemToCheck) > 0)
        THEN
          AppendToStringArray(Routes_SubRouteClearingStrings[Route, SubRoute], RouteItemToCheck)
        ELSE
          IF (Pos('FP=', RouteItemToCheck) > 0) OR (Pos('TP=', RouteItemToCheck) > 0) THEN BEGIN
            TempPoint := ExtractPointFromString(RouteItemToCheck);

            { Watch out for catch points }
            IF Points[TempPoint].Point_Type = ProtectedPoint THEN BEGIN
              IF NOT IsElementInIntegerArray(PointsSeenArray, Points[TempPoint].Point_OtherPoint) THEN BEGIN
                { if we haven't processed a protected point's catch point, process that first as it will subsequently be locked by the protected point }
                AppendToStringArray(Routes_SubRouteClearingStrings[Route, SubRoute], 'TP=' + IntToStr(Points[TempPoint].Point_OtherPoint) + '-');
                { and record the fact we've added it }
                AppendToIntegerArray(PointsSeenArray, Points[TempPoint].Point_OtherPoint);
              END;
            END;

            { Add the point if we haven't just added it above }
            IF NOT IsElementInIntegerArray(PointsSeenArray, TempPoint) THEN
              AppendToStringArray(Routes_SubRouteClearingStrings[Route, SubRoute], 'TP=' + IntToStr(TempPoint) + '-');
          END;

      Inc(I);
    END; {WHILE}

    { Include start of subroute details in the diagnostics from the original setting-up subroute }
    WriteStringArrayToLog(Routes_LocoChips[Route], 'R', 'Final Route Array to clear R=' + IntToStr(Route)
                                                        + ', SR=' + IntToStr(SubRoute)
                                                        + ' ' + DescribeSubRoute(Route, SubRoute)
                                                        + ' :', Routes_SubRouteClearingStrings[Route, SubRoute], 2, 190, 'SR=');
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CreateClearingSubRouteArray: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CreateClearingSubRouteArray }

FUNCTION JourneyAheadIsClear(T : Train; S, Route, Journey : Integer; StartLine, EndLine : Integer; ArrivalTime : TDateTime; VAR DraftRouteArray : StringArrayType) : Boolean;
{ Tests the journey given - if clear returns true; else tries alternative routes - if one of those if clear, substitute it for the original journey; if none are clear,
  return true anyway, as otherwise the routeing will never happen (??? ****).
}
CONST
  CurrentlyRouteing = True;
  EmergencyRouteing = True;
  FindNextAvailableLocation = True;
  MayReselectOldLocation = True;
  NewJourney = True;
  OmitLocoTypeRestriction = True;
  PreRouteing = True;
  RebuildRouteArray = True;
  TimetableLoading = True;

VAR
  Area : Integer;
  ErrorMsg : String;
  LinesNotAvailableStr : String;
  NewLocation : Integer;
  OK : Boolean;
  OldLocation : Integer;
  RouteCurrentlyLocked : Boolean;
  RoutePermanentlyLocked : Boolean;
  SaveTrainType : TypeOfTrainType;
  SuccessMsg : String;
  TempTime : TDateTime;

BEGIN
  Result := False;

  TRY
    WITH T^ DO BEGIN
      DrawLineInLogFile(Train_LocoChip, 'R', '_', UnitRef);
      Log(Train_LocoChipStr + ' R J=' + IntToStr(Journey)
                            + ': is journey ahead from ' + LineToStr(StartLine) + ' to ' + LineToStr(EndLine) + ' clear?');
      NewLocation := UnknownLocation;

      { First needs to check that the location occupation array allows this journey to end where specified. If there's nothing in the array, perhaps because the location
        array has been marked 'out of use', which deletes any entries, and then restored to use, (which doesn't reinstate any entries as the program may by then have
        reallocated them), then add this train again.
      }
      WITH Train_JourneysArray[Journey] DO BEGIN
        Log(Train_LocoChipStr + ' R J=' + IntToStr(Journey) + ': checking location occupation');
        IF Journey < Train_TotalJourneys THEN
          CheckLocationOccupation(T, Journey, Journey + 1,
                                  Lines[EndLine].Line_Location,
                                  IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                             TrainJourney_ActualArrivalTime,
                                             IncMinute(TrainJourney_CurrentArrivalTime, -1)),
                                  Train_JourneysArray[Journey + 1].TrainJourney_CurrentDepartureTime,
                                  OK)
        ELSE
          CheckLocationOccupation(T, Journey, Journey + 1,
                                  Lines[EndLine].Line_Location,
                                  IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                             TrainJourney_ActualArrivalTime,
                                             IncMinute(TrainJourney_CurrentArrivalTime, -1)),
                                  StrToTime('23:59'),
                                  OK);
      END; {WITH}

      { Now checks if the route there is clear of obstructions - we don't care if the route is permanently locked or not }
      IF OK THEN BEGIN
        CheckRouteAheadLocking(T, DraftRouteArray, RouteCurrentlyLocked, RoutePermanentlyLocked, ErrorMsg);
        IF NOT RouteCurrentlyLocked
        AND NOT RoutePermanentlyLocked
        THEN BEGIN
          Log(Train_LocoChipStr + ' R J=' + IntToStr(Journey)
                                + ': route ahead at S=' + IntToStr(S)
                                + ' (' + LineToStr(StartLine) + ' to ' + LineToStr(EndLine) + ')' + ' is not locked');
          Result := True;
        END ELSE BEGIN
          OldLocation := Train_JourneysArray[Journey].TrainJourney_EndLocation;

          Log(Train_LocoChipStr + ' R J=' + IntToStr(Journey)
                                + ': route ahead at S=' + IntToStr(S)
                                + ' (' + LineToStr(StartLine) + ' to ' + LineToStr(EndLine) + ')'
                                + ' is locked; checking alternative subroute availability');
          Area := Locations[OldLocation].Location_Area;
          IF Area = UnknownArea THEN
            Result := False
          ELSE BEGIN
            IF AlternativeAreaOrLocationAvailable(T, Journey, Area, OldLocation, NewLocation, TempTime, NOT PreRouteing, CurrentlyRouteing, NOT OmitLocoTypeRestriction,
                                                  NOT FindNextAvailableLocation, NOT MayReselectOldLocation, ErrorMsg, SuccessMsg)
            THEN
              Result := True
            ELSE BEGIN
              { Try again after setting the loco type to unknown - allows express trains to use the goods line and vice versa }
              SaveTrainType := T^.Train_Type;
              T^.Train_Type := UnknownTrainType;
              Log(Train_LocoChipStr + ' R J=' + IntToStr(Journey)
                                    + ': rechecking alternative subroute availability with train type set to unknown:');
              IF AlternativeAreaOrLocationAvailable(T, Journey, Area, OldLocation, NewLocation, TempTime, NOT PreRouteing, CurrentlyRouteing, NOT OmitLocoTypeRestriction,
                                                    NOT FindNextAvailableLocation, NOT MayReselectOldLocation, ErrorMsg, SuccessMsg)
              THEN
                Result := True;

              T^.Train_Type := SaveTrainType;
            END;

            IF Result = True THEN BEGIN
              { an alternative area or location is available }
              Result := True;
              Log(Train_LocoChipStr + ' R J=' + IntToStr(Journey)
                                    + ': alternative location ' + LocationToStr(NewLocation, ShortStringType) + ' found');

              { We now have to amend the end parameters for the journey into the area concerned - we use CreateJourney as that also rebuilds the RouteArray }
              WITH Train_JourneysArray[Journey] DO
                CreateJourney(T, Journey, NOT NewJourney,
                              Locations[Train_JourneysArray[Journey].TrainJourney_StartLocation].Location_Area,
                              Locations[NewLocation].Location_Area,
                              TrainJourney_StartLocation, NewLocation,
                              TrainJourney_DiagrammedStartLocation, TrainJourney_DiagrammedEndLocation,
                              TrainJourney_StartLine, UnknownLine,
                              TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime, TrainJourney_CurrentArrivalTime,
                              TrainJourney_Direction,
                              TrainJourney_RouteArray,
                              RebuildRouteArray,
                              TrainJourney_StoppingOnArrival,
                              TrainJourney_NotForPublicUse,
                              NOT EmergencyRouteing,
                              TrainJourney_StartOfRepeatJourney,
                              NOT TimetableLoading,
                              ErrorMsg, LinesNotAvailableStr, OK);

              DrawLineInLogFile(Train_LocoChip, 'R', '-', UnitRef);

              { and amend the start parameters for the onward journey (if any) too }
              IF Journey < High(Train_JourneysArray) THEN BEGIN
                WITH Train_JourneysArray[Journey + 1] DO
                  CreateJourney(T, Journey + 1, NOT NewJourney,
                                Locations[NewLocation].Location_Area, TrainJourney_EndArea,
                                NewLocation, TrainJourney_EndLocation,
                                TrainJourney_DiagrammedStartLocation, TrainJourney_DiagrammedEndLocation,
                                UnknownLine, TrainJourney_EndLine,
                                TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime, TrainJourney_CurrentArrivalTime,
                                TrainJourney_Direction,
                                TrainJourney_RouteArray,
                                RebuildRouteArray,
                                TrainJourney_StoppingOnArrival,
                                TrainJourney_NotForPublicUse,
                                NOT EmergencyRouteing,
                                TrainJourney_StartOfRepeatJourney,
                                NOT TimetableLoading,
                                ErrorMsg, LinesNotAvailableStr, OK);

                DrawLineInLogFile(Train_LocoChip, 'R', '-', UnitRef);
                DrawDiagrams(UnitRef, 'JourneyAheadIsClear');
              END;
              SetUpTrainLocationOccupationsAbInitio(T, OK);
            END;
          END;
        END;
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG JourneyAheadIsClear: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { JourneyAheadIsClear }

PROCEDURE CreateRouteArraysForTrain(T : Train);
{ See what routeing needs doing for the given train }
CONST
  AutoRouteSetting = True;
  Cancel = True;
  Check = True;
  Delayed = True;
  Indent = True;
  Undo = True;

VAR
  DraftRouteArray : StringArrayType;
  DraftRouteArrayPos : Integer;
  ElementPos : Integer;
  EndBufferStop : Integer;
  EndSignal : Integer;
  FinalRouteArray : StringArrayType;
  I : Integer;
  InitialTrackCircuitsRequired : Boolean;
  JourneyCount : Integer;
  JourneyFound : Boolean;
//  L : Integer;
  LocationsToMonitorCount : Integer;
  LockingArray : StringArrayType;
  NewRoute : Integer;
  OtherJourneyCount : Integer;
  OtherT : Train;
  OtherTrainActualDepartureTime : TDateTime;
  OtherTrainArrivalTime : TDateTime;
  OtherTrainDepartureTime : TDateTime;
  OtherTrainDirection : DirectionType;
  OtherTrainStartArea : Integer;
  OtherTrainTypeNum : Integer;
//  PlatformTrackCircuitsSetToOtherRoutes : Boolean;
  RouteCreationHeld : Boolean;
  RouteEndLine : Integer;
//  RouteHoldStr : String;
  RouteStartLine : Integer;
  S : Integer;
  SignalFound : Boolean;
  StartSignal : Integer;
  SubRouteCount : Integer;
  SubRoute : Integer;
  TCCount : Integer;
  TCFound : Boolean;
//  TCJourneyFound : Boolean;
  TempLocation : Integer;
  TempRouteCounter : Integer;
  TestList : DiagramsEntryType;
  ThisTrainArrivalTime : TDateTime;
  ThisTrainDepartureTime : TDateTime;
  ThisTrainTypeNum : Integer;
//  TrainFound : Boolean;
  TrainJourneyRouteArrayPos : Integer;

BEGIN
  TRY
    IF InAutoMode THEN BEGIN
      InitialTrackCircuitsRequired := True;
      SetLength(DraftRouteArray, 0);

      WITH T^ DO BEGIN
        { Do not do this test every tick as it is time consuming }
        IF CurrentRailwayTime > IncSecond(Train_RouteCheckedTime, 15) THEN BEGIN { put 15 in as a .ini parameter? **** }
          Train_RouteCheckedTime := CurrentRailwayTime;

          IF ((Train_CurrentStatus <> Suspended)
          AND (Train_CurrentStatus <> MissingAndSuspended)
          AND (Train_CurrentStatus <> Cancelled))
          AND ((Train_CurrentStatus = ReadyForRouteing)
               OR (Train_CurrentStatus = ReadyToDepart)
               OR (Train_CurrentStatus = Departed)
               OR (Train_CurrentStatus = RouteingWhileDeparted))
          THEN BEGIN
            { Set up a new route with a new number - Routes_RouteCounter will be incremented if the route setting up is successful. The new route may encompass a number of
              separate journeys.
            }
            NewRoute := Routes_RouteCounter + 1;

            JourneyCount := -1;
            RouteCreationHeld := False;
            WHILE (JourneyCount < High(Train_JourneysArray))
            AND NOT RouteCreationHeld
            DO BEGIN
              Inc(JourneyCount);
              WITH Train_JourneysArray[JourneyCount] DO BEGIN
                IF NOT TrainJourney_Created THEN BEGIN
                  IF (Train_PossibleRerouteTime > 0)
                  AND (CompareTime(CurrentRailwayTime, Train_PossibleRerouteTime) > 0)
                  THEN BEGIN
                    makesound(5);
                    Train_PossibleRerouteTime := 0;
                  END;

                  { See if another train has priority }
                  RouteCreationHeld := False;
                  Train_RouteCreationHoldNum := 0;
                  OtherT := TrainList;
                  WHILE (OtherT <> NIL)
                  AND NOT RouteCreationHeld
                  DO BEGIN
                    IF (OtherT <> T)
                    AND OtherT^.Train_DiagramFound
                    AND (OtherT^.Train_CurrentStatus <> Cancelled)
                    AND (OtherT^.Train_CurrentStatus <> Suspended)
                    AND (OtherT^.Train_CurrentStatus <> MissingAndSuspended)
                    THEN BEGIN
                      IF Length(OtherT^.Train_JourneysArray) > 0 THEN BEGIN
                        OtherTrainActualDepartureTime := OtherT^.Train_JourneysArray[OtherT^.Train_CurrentJourney].TrainJourney_ActualDepartureTime;
                        OtherTrainDepartureTime := OtherT^.Train_JourneysArray[OtherT^.Train_CurrentJourney].TrainJourney_CurrentDepartureTime;
                        OtherTrainStartArea := OtherT^.Train_JourneysArray[OtherT^.Train_CurrentJourney].TrainJourney_StartArea;
                        OtherTrainDirection := OtherT^.Train_JourneysArray[OtherT^.Train_CurrentJourney].TrainJourney_Direction;

                        IF OtherTrainActualDepartureTime = 0 THEN BEGIN
                          IF TrainJourney_StartArea = OtherTrainStartArea THEN BEGIN
                            IF TrainJourney_Direction = OtherTrainDirection THEN BEGIN
                              IF CompareTime(TrainJourney_CurrentDepartureTime, OtherTrainDepartureTime) = 0 THEN BEGIN
                                { give expresses priority }
                                IF (T^.Train_Type <> ExpressPassenger)
                                AND (OtherT^.Train_Type = ExpressPassenger)
                                THEN BEGIN
                                  RouteCreationHeld := True;
                                  Train_RouteCreationHoldNum := 1;
                                  Train_RouteCreationReleasedMsg := '';
                                  Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                                + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                                + ': held as an express train (' + LocoChipToStr(OtherT^.Train_LocoChip) + ')'
                                                                + ' is due to leave at the same time'
                                                                + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                                  IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                                    Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                                    Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                                  END;
                                END ELSE
                                  { this covers all other non-passenger types that we might be }
                                  IF (T^.Train_Type <> ExpressPassenger)
                                  AND (T^.Train_Type <> OrdinaryPassenger)
                                  AND (OtherT^.Train_Type = OrdinaryPassenger)
                                  THEN BEGIN
                                    RouteCreationHeld := True;
                                    Train_RouteCreationHoldNum := 2;
                                    Train_RouteCreationReleasedMsg := '';
                                    Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                                  + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                                  + ': held as a passenger train (' + LocoChipToStr(OtherT^.Train_LocoChip) + ')'
                                                                  + ' is due to leave at the same time'
                                                                  + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                                    IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                                      Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                                      Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                                    END;
                                  END;
                              END;
                            END;
                          END;
                        END;
                      END;
                    END;
                    OtherT := OtherT^.Train_NextRecord;
                  END; {WHILE}

                  IF NOT RouteCreationHeld THEN BEGIN
                    IF Train_RouteCreationHeldMsgWrittenArray[1] THEN BEGIN
                      Train_RouteCreationHoldMsg := ' ';
                      Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                        + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                        + ': express-train-leaving-at-the-same-time hold released at ' + TimeToHMSStr(CurrentRailwayTime);
                      Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);
                      Train_RouteCreationHeldMsgWrittenArray[1] := False;
                      Train_RouteCreationHoldNum := 0;
                    END ELSE
                      IF Train_RouteCreationHeldMsgWrittenArray[2] THEN BEGIN
                        Train_RouteCreationHoldMsg := ' ';
                        Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                          + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                          + ': passenger-train-leaving-at-the-same-time hold released at ' + TimeToHMSStr(CurrentRailwayTime);
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);
                        Train_RouteCreationHeldMsgWrittenArray[2] := False;
                        Train_RouteCreationHoldNum := 0;
                      END;
                  END;

//                  { Note when we arrive from the previous journey }
//                  IF (JourneyCount > 0)
//                  AND Train_JourneysArray[JourneyCount - 1].TrainJourney_StoppingOnArrival
//                  AND (Train_JourneysArray[JourneyCount - 1].TrainJourney_CurrentArrivalTime = 0)
//                  AND (Train_JourneysArray[JourneyCount - 1].TrainJourney_Cleared)
//                  THEN
//                    Train_JourneysArray[JourneyCount - 1].TrainJourney_CurrentArrivalTime := CurrentRailwayTime;

                  { See whether this part of the routeing should happen yet - check if any previous routes for the same loco have not yet been fully set up. Seem to be
                    two versions of this.
                  }
                  IF NOT RouteCreationHeld THEN BEGIN
                    TempRouteCounter := 0;
                    Train_RouteCreationHoldNum := 3;
                    WHILE (TempRouteCounter <= High(Routes_LocoChips))
                    AND NOT RouteCreationHeld
                    DO BEGIN
                      IF Routes_LocoChips[TempRouteCounter] = Train_LocoChip THEN BEGIN
                        IF NewRoute <> TempRouteCounter THEN BEGIN
                          IF NOT Routes_RouteSettingsCompleted[TempRouteCounter] THEN BEGIN
                            RouteCreationHeld := True;
                            Train_RouteCreationReleasedMsg := '';
                            Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                          + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                          + ': held as a previous route' + ' (R=' + IntToStr(TempRouteCounter) + ')' + ' has not yet been set up'
                                                          + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                            IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                              Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                              Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                            END;
                          END;
                        END;
                      END;
                      Inc(TempRouteCounter);
                    END; {WHILE}

                    IF NOT RouteCreationHeld
                    AND Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum]
                    THEN BEGIN
                      Train_RouteCreationHoldMsg := ' ';
                      Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                        + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                        + ': previous-route-not-set-up hold released at ' + TimeToHMSStr(CurrentRailwayTime);
                      Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);
                      Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := False;
                      Train_RouteCreationHoldNum := 0;
                    END;
                  END;

                  { See if any timings need revising - if we are running late, for example }
                  IF NOT RouteCreationHeld THEN BEGIN
                    Train_RouteCreationHoldNum := 4;
                    IF (JourneyCount > 0)
                    AND Train_JourneysArray[JourneyCount - 1].TrainJourney_StoppingOnArrival
                    AND Train_JourneysArray[JourneyCount - 1].TrainJourney_Cleared
                    AND (Train_JourneysArray[JourneyCount - 1].TrainJourney_ActualArrivalTime > IncSecond(CurrentRailwayTime, -30))
                    THEN BEGIN
                      RouteCreationHeld := True;
                      Train_RouteCreationReleasedMsg := '';
                      Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                    + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                    + ': held until ' + TimeToHMSStr(IncSecond(CurrentRailwayTime, 60))
                                                    + ' as there must be 30 seconds between arrival and commencing departure routeing'
                                                    + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                      IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                      END;
                    END ELSE
                      IF Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Train_RouteCreationHoldMsg := ' ';
                        Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                          + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                          + ': 30-seconds-between-arrival-and-commencing-departure-routeing'
                                                          + ' hold released at ' + TimeToHMSStr(CurrentRailwayTime)
                                                          + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := False;
                        Train_RouteCreationHoldNum := 0;
                      END;
                  END;

                  { See if the routeing of the next journey needs to be held until we arrive }
                  IF NOT RouteCreationHeld THEN BEGIN
                    Train_RouteCreationHoldNum := 5;
                    IF (JourneyCount > 0)
                    AND T^.Train_JourneysArray[JourneyCount - 1].TrainJourney_StoppingOnArrival
                    AND NOT T^.Train_JourneysArray[JourneyCount - 1].TrainJourney_Cleared
    // AND False { route-ahead checked - nothing else due to occupy it for some time **** }
                    THEN BEGIN
                      RouteCreationHeld := True;
                      Train_RouteCreationReleasedMsg := '';
                      Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                    + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                    + ': held until the train has arrived and stopped after its previous journey'
                                                    + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                      IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                      END;
                    END ELSE
                      IF Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Train_RouteCreationHoldMsg := ' ';
                        Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                          + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                          + ': train-not-arrived-and-stopped hold released at ' + TimeToHMSStr(CurrentRailwayTime)
                                                          + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := False;
                        Train_RouteCreationHoldNum := 0;
                      END;
                  END;

                  IF NOT RouteCreationHeld THEN BEGIN
                    { Immediately or soon afterwards, are there other trains entering the station from the main line? }
                    Train_RouteCreationHoldNum := 6;
                    IF TrainJourney_RouteArray[1] = HoldMarker THEN BEGIN
                      { Consider trains that are held at an approaching signal first }
                      S := ExtractSignalFromString(TrainJourney_RouteArray[2]);
                      IF S <> UnknownSignal THEN BEGIN
                        IF Length(Signals[S].Signal_LocationsToMonitorArray) > 0 THEN BEGIN
                          { we have to see what is happening in the supplied list of platforms }
                          TestList := DiagramsList;
                          WHILE (TestList <> NIL)
                          AND NOT RouteCreationHeld
                          DO BEGIN
                            OtherT := TestList^.TrainPtr;
                            IF Train_LocoChip <> TestList^.TrainPtr^.Train_LocoChip THEN BEGIN
                              IF OtherT^.Train_DiagramFound
                              AND (OtherT^.Train_CurrentStatus <> Suspended)
                              AND (OtherT^.Train_CurrentStatus <> Cancelled)
                              AND (OtherT^.Train_CurrentStatus <> Departed)
                              AND (OtherT^.Train_CurrentStatus <> RouteingWhileDeparted)
                              THEN BEGIN
                                OtherJourneyCount := OtherT^.Train_CurrentJourney;
                                WHILE OtherJourneyCount <= OtherT^.Train_TotalJourneys DO BEGIN
                                  LocationsToMonitorCount := 0;
                                  WHILE LocationsToMonitorCount <= High(Signals[S].Signal_LocationsToMonitorArray) DO BEGIN
                                    TempLocation := Signals[S].Signal_LocationsToMonitorArray[LocationsToMonitorCount];
                                    IF TempLocation = OtherT^.Train_JourneysArray[OtherJourneyCount].TrainJourney_StartLocation
                                    THEN BEGIN
                                      { allow trains to leave platforms or fiddleyards before routeing things in }
                                      ThisTrainArrivalTime := Train_JourneysArray[Train_CurrentJourney].TrainJourney_CurrentArrivalTime;
                                      OtherTrainDepartureTime := OtherT^.Train_JourneysArray[OtherJourneyCount].TrainJourney_CurrentDepartureTime;
                                      IF CompareTime(IncMinute(ThisTrainArrivalTime, 1), OtherTrainDepartureTime) >= 0 THEN BEGIN
                                        IF TrainJourney_Direction <> OtherT^.Train_JourneysArray[OtherJourneyCount].TrainJourney_Direction
                                        THEN BEGIN
                                          RouteCreationHeld := True;
                                          Train_RouteCreationReleasedMsg := '';
                                          Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                                        + LineToStr(TrainJourney_StartLine)
                                                                        + ' to ' + LineToStr(TrainJourney_EndLine)
                                                                        + ': held as another train ('
                                                                        + LocoChipToStr(OtherT^.Train_LocoChip)
                                                                        + ') has an earlier departure time from '
                                                                        + AreaToStr(OtherT^.Train_JourneysArray[OtherJourneyCount].TrainJourney_StartArea, LongStringType);
                                          IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                                            Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                                            Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                                          END;
                                        END;
                                      END;
                                    END;

                                    Inc(LocationsToMonitorCount);
                                  END; {WHILE}
                                  Inc(OtherJourneyCount);
                                END; {WHILE}
                              END;
                            END;
                            TestList := TestList^.NextDiagramsRecord;
                          END; {WHILE}
                        END;
                      END;
                    END;
                  END;

                  IF NOT RouteCreationHeld THEN BEGIN
                    { Immediately or soon afterwards, are there other trains entering the station from the main line? }
                    Train_RouteCreationHoldNum := 7;
                    IF TrainJourney_RouteArray[1] = HoldMarker THEN BEGIN
                      { Consider trains that are held at an approaching signal first }
                      S := ExtractSignalFromString(TrainJourney_RouteArray[2]);
                      IF S <> UnknownSignal THEN BEGIN
                        IF Length(Signals[S].Signal_LocationsToMonitorArray) > 0 THEN BEGIN
                          { we have to see what is happening in the supplied list of platforms }
                          TestList := DiagramsList;
                          WHILE (TestList <> NIL)
                          AND NOT RouteCreationHeld
                          DO BEGIN
                            OtherT := TestList^.TrainPtr;
                            IF Train_LocoChip <> TestList^.TrainPtr^.Train_LocoChip THEN BEGIN
                              IF OtherT^.Train_DiagramFound
                              AND (OtherT^.Train_CurrentStatus <> Suspended)
                              AND (OtherT^.Train_CurrentStatus <> Cancelled)
                              AND (OtherT^.Train_CurrentStatus <> Departed)
                              AND (OtherT^.Train_CurrentStatus <> RouteingWhileDeparted)
                              THEN BEGIN
                                OtherJourneyCount := OtherT^.Train_CurrentJourney;
                                WHILE OtherJourneyCount <= OtherT^.Train_TotalJourneys DO BEGIN
                                  LocationsToMonitorCount := 0;
                                  WHILE LocationsToMonitorCount <= High(Signals[S].Signal_LocationsToMonitorArray) DO BEGIN
                                    TempLocation := Signals[S].Signal_LocationsToMonitorArray[LocationsToMonitorCount];
                                    IF TempLocation = OtherT^.Train_JourneysArray[OtherJourneyCount].TrainJourney_StartLocation
                                    THEN BEGIN
                                      { allow trains that arrived in platforms or fiddleyards first to leave them first }
                                      ThisTrainArrivalTime := Train_JourneysArray[Train_CurrentJourney].TrainJourney_CurrentArrivalTime;
                                      OtherTrainArrivalTime := OtherT^.Train_JourneysArray[OtherJourneyCount].TrainJourney_CurrentArrivalTime;
                                      IF CompareTime(IncMinute(ThisTrainArrivalTime, 1), OtherTrainArrivalTime) >= 0 THEN BEGIN
                                        IF TrainJourney_Direction = OtherT^.Train_JourneysArray[OtherJourneyCount].TrainJourney_Direction
                                        THEN BEGIN
                                          RouteCreationHeld := True;
                                          Train_RouteCreationReleasedMsg := '';
                                          Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                                        + LineToStr(TrainJourney_StartLine)
                                                                        + ' to ' + LineToStr(TrainJourney_EndLine)
                                                                        + ': held as another train ('
                                                                        + LocoChipToStr(OtherT^.Train_LocoChip)
                                                                        + ') arrived first (at ' + TimeToHMSStr(OtherTrainArrivalTime)
                                                                        + ') so should depart first';
                                          IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                                            Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                                            Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                                          END;
                                        END;
                                      END;
                                    END;

                                    Inc(LocationsToMonitorCount);
                                  END; {WHILE}
                                  Inc(OtherJourneyCount);
                                END; {WHILE}
                              END;
                            END;
                            TestList := TestList^.NextDiagramsRecord;
                          END; {WHILE}
                        END;
                      END;
                    END;

                    IF NOT RouteCreationHeld
                    AND Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum]
                    THEN BEGIN
                      Train_RouteCreationHoldMsg := ' ';
                      Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                        + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                        + ': route creation hold released at '
                                                        + TimeToHMSStr(CurrentRailwayTime)
                                                        + ' as there is no longer a higher priority train';
                      Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);
                      Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := False;
                      Train_RouteCreationHoldNum := 0;
                    END;
                  END;

                  { See whether this part of the routeing should happen yet - if we're not due to stop, route it straight away; otherwise a minute before the departure
                    time is time enough, although in some cases we will want to continue routeing anyway, e.g. if nothing is in the way, and we're not changing direction
                    *****
                  }
                  IF NOT RouteCreationHeld THEN BEGIN
                    Train_RouteCreationHoldNum := 8;
                    IF (JourneyCount > 0)
                    AND (Train_JourneysArray[JourneyCount - 1].TrainJourney_StoppingOnArrival
                    AND (CurrentRailwayTime < IncSecond(TrainJourney_CurrentDepartureTime, -60)))
    //AND False { route-ahead to be checked - nothing else due to occupy it for some time **** }
                    THEN BEGIN
                      RouteCreationHeld := True;
                      Train_RouteCreationReleasedMsg := '';
                      Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                    + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                    + ': held as it is not a minute before departure time of '
                                                    + TimeToHMSStr(TrainJourney_CurrentDepartureTime)
                                                    + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                      IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                      END;
                    END ELSE
                      IF Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Train_RouteCreationHoldMsg := ' ';
                        Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                          + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                          + ': one-minute-before-departure hold released'
                                                          + ' at ' + TimeToHMSStr(CurrentRailwayTime)
                                                          + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := False;
                        Train_RouteCreationHoldNum := 0;
                      END;
                  END;

                  { Look at the previous journey to see if we change direction }
                  IF NOT RouteCreationHeld THEN BEGIN
                    Train_RouteCreationHoldNum := 9;
                    IF (JourneyCount > 0)
                    AND (Train_JourneysArray[JourneyCount - 1].TrainJourney_StoppingOnArrival
                    AND (Train_JourneysArray[JourneyCount - 1].TrainJourney_Direction <> Train_JourneysArray[JourneyCount].TrainJourney_Direction)
                    AND NOT (Train_JourneysArray[JourneyCount - 1].TrainJourney_Cleared))
                    THEN BEGIN
                      RouteCreationHeld := True;
                      Train_RouteCreationReleasedMsg := '';
                      Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                    + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                    + ': held as the train will change direction when it arrives'
                                                    + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                      IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                      END;
                    END ELSE
                      IF Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Train_RouteCreationHoldMsg := ' ';
                        Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                          + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                          + ': change-of-direction-and-non-arrival hold released at ' + TimeToHMSStr(CurrentRailwayTime)
                                                          + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := False;
                        Train_RouteCreationHoldNum := 0;
                      END;
                  END;

                  { See if the earlier part of the route is set up - if so, we can proceed with this part }
                  IF NOT RouteCreationHeld THEN BEGIN
                    Train_RouteCreationHoldNum := 10;
                    IF (JourneyCount > 0)
                    AND (Length(DraftRouteArray) > 0)
                    THEN BEGIN
                      RouteCreationHeld := True;
                      Train_RouteCreationReleasedMsg := '';
                      Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                    + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                    + ': held as previous part of the route is not yet set up'
                                                    + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                      IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                      END;
                    END ELSE
                      IF Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Train_RouteCreationHoldMsg := ' ';
                        Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                          + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                          + ': previous-part-of-route-not-set-up hold released at ' + TimeToHMSStr(CurrentRailwayTime)
                                                          + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := False;
                        Train_RouteCreationHoldNum := 0;
                      END;
                  END;

                  { If there's a hold marker, look at the route immediately ahead and see if it's clear }
                  IF NOT RouteCreationHeld THEN BEGIN
                    Train_RouteCreationHoldNum := 11;
                    IF (Length(TrainJourney_RouteArray) = 0)
                    AND NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum]
                    THEN BEGIN
                      Log(Train_LocoChipStr + ' RG J=' + IntToStr(JourneyCount) + ' TrainJourney_RouteArray is empty');
                      Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                    END ELSE BEGIN
                      Train_SubRouteAheadCheckedTime := CurrentRailwayTime;
                      IF TrainJourney_RouteArray[1] = HoldMarker THEN
                        S := ExtractSignalFromString(TrainJourney_RouteArray[2])
                      ELSE
                        S := ExtractSignalFromString(TrainJourney_RouteArray[1]);
                      IF NOT JourneyAheadIsClear(T, S, NewRoute, JourneyCount, TrainJourney_StartLine, TrainJourney_EndLine, TrainJourney_CurrentArrivalTime,
                                                 TrainJourney_RouteArray)
                      THEN BEGIN
                        RouteCreationHeld := True;
                        Train_RouteingHeldAtSignal := S;
                        Train_RouteCreationReleasedMsg := '';
                        Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                      + 'R=' + IntToStr(NewRoute) + ' '
                                                      + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                      + ': held at S=' + IntToStr(Train_RouteingHeldAtSignal) + ' as route ahead is not clear'
                                                      + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';

                        IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                          Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);
                          Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;

                          { Set the time after which we can consider rerouting this train to avoid the route ahead that isn't clear }
                          Train_PossibleRerouteTime := IncMinute(CurrentRailwayTime, RouteAheadNotClearWaitTimeInMinutes);
                        END;
                      END ELSE BEGIN
                        IF Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                          Train_RouteCreationHoldMsg := ' ';
                          Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                            + 'R=' + IntToStr(NewRoute) + ' '
                                                            + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                            + ': route-ahead-not-clear hold released at ' + TimeToHMSStr(CurrentRailwayTime)
                                                            + ' (RCH=' + IntToStr(Train_RouteCreationHoldNum) + ')';
                          Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);

                          Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := False;
                          Train_RouteingHeldAtSignal := UnknownSignal;
                          Train_SubRouteAheadCheckedTime := 0;
                          { because we're starting the route from a signal, we already know where we are }
                          InitialTrackCircuitsRequired := False;
                          Train_RouteCreationHoldNum := 0;
                        END;
                      END;
                    END;
                  END;

                  { See if the last part of the journey has to be done with the user driving }
                  IF NOT RouteCreationHeld THEN BEGIN
                    Train_RouteCreationHoldNum := 12;
                    { just a test to see we haven't gone beyond the array boundaries - saves waiting for the range check error }
                    IF Train_RouteCreationHoldNum > LastRouteCreationHeldMsgNumber THEN BEGIN
                      Log('E Range Check Error: Train_RouteCreationHoldNum ' + IntToStr(Train_RouteCreationHoldNum)
                                                + ' > LastRouteCreationHeldMsgNumber' + IntToStr(LastRouteCreationHeldMsgNumber));
                      ShowMessage('E Range Check Error: Train_RouteCreationHoldNum ' + IntToStr(Train_RouteCreationHoldNum)
                                                + ' > LastRouteCreationHeldMsgNumber' + IntToStr(LastRouteCreationHeldMsgNumber) + ':'
                                  + CRLF
                                  + ' please inform the program''s author');
                    END;

                    IF Train_JourneysArray[JourneyCount].TrainJourney_UserToDrive
                    AND NOT Train_UserDriving
                    THEN BEGIN
                      RouteCreationHeld := True;
                      Train_RouteCreationReleasedMsg := '';
                      Train_RouteCreationHoldMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                    + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                    + ': held until operator indicates he has taken over';
                      IF NOT Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationHoldMsg);

                        StartSignal := UnknownSignal;
                        SignalFound := False;
                        TrainJourneyRouteArrayPos := 0;
                        WHILE (TrainJourneyRouteArrayPos <= High(TrainJourney_RouteArray))
                        AND NOT SignalFound
                        DO BEGIN
                          IF ExtractSignalFromString(TrainJourney_RouteArray[TrainJourneyRouteArrayPos]) <> UnknownSignal THEN BEGIN
                            SignalFound := True;
                            StartSignal := ExtractSignalFromString(TrainJourney_RouteArray[TrainJourneyRouteArrayPos]);
                          END;
                          Inc(TrainJourneyRouteArrayPos);
                        END; {WHILE}

                        IF StartSignal <> UnknownSignal THEN
                          Debug('=Operator - please take over loco ' + LocoChipToStr(Train_LocoChip)
                                + ' when it is at signal ' + IntToStr(StartSignal))
                        ELSE BEGIN
                          Debug('Curious error: no signal found in TrainJourney_RouteArray');
                          WriteStringArrayToLog(Train_LocoChip, 'R', 'Curious error: no signal found in TrainJourney_RouteArray', TrainJourney_RouteArray);
                          ASM
                            Int 3;
                          END; {ASM}
                        END;
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := True;
                      END;
                    END ELSE
                      IF Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] THEN BEGIN
                        Train_RouteCreationHoldMsg := ' ';
                        Train_RouteCreationReleasedMsg := 'J=' + IntToStr(JourneyCount) + ': '
                                                          + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                          + ': operator-taking-control hold released at ' + TimeToHMSStr(CurrentRailwayTime);
                        Log(Train_LocoChipStr + ' R ' + Train_RouteCreationReleasedMsg);
                        Train_RouteCreationHeldMsgWrittenArray[Train_RouteCreationHoldNum] := False;
                        Train_RouteCreationHoldNum := 0;
                      END;
                  END;

                  IF RouteCreationHeld THEN
                    Train_RouteCreationHeldJourney := JourneyCount
                  ELSE BEGIN
                    { Success - we have a created a route }
                    Log(Train_LocoChipStr + ' R J=' + IntToStr(JourneyCount)
                                          + ': last Train_RouteCreationHoldNum was ' + IntToStr(Train_RouteCreationHoldNum));
                    Train_RouteCreationHoldNum := 0;
                    Train_RouteCreationHeldJourney := UnknownJourney;
                    Train_RouteCreationHoldMsg := '';
                    TrainJourney_Created := True;

                    IF TestingMode THEN
                      WriteStringArrayToLog(Train_LocoChip, 'R', 'J=' + IntToStr(JourneyCount)
                                                                 + ': Temp Draft Route Array to set up'
                                                                 + ' R=' + IntToStr(NewRoute) + ' '
                                                                 + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                                 + ' released at ' + TimeToHMSStr(CurrentRailwayTime)
                                                                 + ': ', TrainJourney_RouteArray, 2, 190, 'SR=');
                    { Insert the journey number after the subroute numbers }
                    TrainJourneyRouteArrayPos := 0;
                    WHILE TrainJourneyRouteArrayPos < Length(TrainJourney_RouteArray) DO BEGIN
                      IF (Pos('SR=', TrainJourney_RouteArray[TrainJourneyRouteArrayPos]) > 0) THEN
                        InsertElementInStringArray
                                              (TrainJourney_RouteArray, TrainJourneyRouteArrayPos + 1, 'J=' + IntToStr(JourneyCount));
                      Inc(TrainJourneyRouteArrayPos);
                    END; {WHILE}

                    { and append the new journey to the previous ones }
                    AppendStringArray2ToStringArray1(DraftRouteArray, TrainJourney_RouteArray);

                    IF Train_CurrentSourceLocation = UnknownLocation THEN
                      Train_CurrentSourceLocation := TrainJourney_StartLocation;
                    TrainJourney_Route := NewRoute;

                    { See if we need to work out which are the initial trackcircuits - if we're starting ab initio, or from a change of direction }
                    IF (JourneyCount = 0)
                    OR ((Train_JourneysArray[JourneyCount - 1].TrainJourney_StoppingOnArrival)
                       AND (Train_JourneysArray[JourneyCount - 1].TrainJourney_Direction <> Train_JourneysArray[JourneyCount].TrainJourney_Direction))
                    THEN BEGIN
                      InitialTrackCircuitsRequired := True;
                      IF JourneyCount = 0 THEN
                        Log(Train_LocoChipStr + ' R J=' + IntToStr(JourneyCount) + ' R=' + IntToStr(NewRoute)
                                              + ' JourneyCount=0 - initial trackcircuits required')
                      ELSE
                        Log(Train_LocoChipStr + ' R J=' + IntToStr(JourneyCount) + ' R=' + IntToStr(NewRoute)
                                              + ' change of direction to ' + DirectionToStr(Train_JourneysArray[JourneyCount].TrainJourney_Direction)
                                              + ' - initial trackcircuits required');
                    END ELSE BEGIN
                      InitialTrackCircuitsRequired := False;
                      Log(Train_LocoChipStr + ' R J=' + IntToStr(JourneyCount) + ' R=' + IntToStr(NewRoute)
                                            + ' Initial trackcircuits not required');
                    END;
                  END;
                END;
              END; {WITH}
            END; {WHILE}

            { Whether or not the routeing is being held, if DraftRouteArray has something in it, it means that some of the routeing has been successful, so that part can
              go ahead.
            }
            IF Length(DraftRouteArray) > 0 THEN BEGIN
              IF RouteCreationHeld THEN
                JourneyCount := JourneyCount - 1;
              Train_PossibleRerouteTime := 0;

              { Update the train's status - we need two different statuses since trains are allowed to move if we're routeing while departed, but not otherwise, or the
                trains start off before the due time.
              }
              IF Train_CurrentStatus <> RouteingWhileDeparted THEN BEGIN
                IF Train_CurrentStatus = Departed THEN BEGIN
                  ChangeTrainStatus(T, RouteingWhileDeparted);
                  Log(Train_LocoChipStr + ' R J=' + IntToStr(JourneyCount) + ': routeing R=' + IntToStr(NewRoute) + ' while departed');
                END ELSE BEGIN
                  ChangeTrainStatus(T, CommencedRouteing);
                  Log(Train_LocoChipStr + ' R J=' + IntToStr(JourneyCount) + ': commencing routeing R=' + IntToStr(NewRoute));
                END;
              END;

              IF Train_CurrentRoute = UnknownRoute THEN BEGIN
                { if this is the very first route we're setting up for this train. (To test if it's the first route, if CurrentRoute is at -1 it means the first route
                  hasn't been set up and CurrentRoute incremented yet).
                }
                Train_CurrentDirection := Train_JourneysArray[0].TrainJourney_Direction;
                Train_CurrentSignal := UnknownSignal;
                Log(Train_LocoChipStr + ' L Resetting Train_CurrentSignal');
                Train_CurrentRoute := Train_JourneysArray[0].TrainJourney_Route;
              END;

              Log(Train_LocoChipStr + ' R Train_CurrentRoute is ' + IntToStr(Train_CurrentRoute));
              Train_CurrentDirection := Train_JourneysArray[JourneyCount].TrainJourney_Direction;
              Log(Train_LocoChipStr + ' R Train_CurrentDirection is ' + DirectionToStr(Train_CurrentDirection));
              IF Train_CurrentSignal <> UnknownSignal THEN
                Log(Train_LocoChipStr + ' R Train_CurrentSignal is S=' + IntToStr(Train_CurrentSignal));

              { Renumber the subroutes as we've combined the journey subroute arrays into the draft route array }
              DraftRouteArrayPos := 0;
              SubRouteCount := -1;
              WHILE DraftRouteArrayPos < Length(DraftRouteArray) DO BEGIN
                IF (Pos('SR=', DraftRouteArray[DraftRouteArrayPos]) > 0) THEN BEGIN
                  SubRoute := ExtractSubRouteFromString(DraftRouteArray[DraftRouteArrayPos]);
                  Inc(SubRouteCount);
                  IF SubRoute <> SubRouteCount THEN
                    DraftRouteArray[DraftRouteArrayPos] := 'SR=' + IntToStr(SubRouteCount);
                END;
                Inc(DraftRouteArrayPos);
              END; {WHILE}

              { Obtain the route start signals and end signals or bufferstops - may need to check they're ok? **** }
              IF ExtractSignalFromString(DraftRouteArray[1]) <> UnknownSignal THEN
                StartSignal := ExtractSignalFromString(DraftRouteArray[1])
              ELSE
                IF ExtractSignalFromString(DraftRouteArray[2]) <> UnknownSignal THEN
                  StartSignal := ExtractSignalFromString(DraftRouteArray[2])
                ELSE
                  StartSignal := ExtractSignalFromString(DraftRouteArray[3]);

              RouteStartLine := Signals[StartSignal].Signal_AdjacentLine;
              EndSignal := ExtractSignalFromString(DraftRouteArray[High(DraftRouteArray)]);
              IF EndSignal <> UnknownSignal THEN BEGIN
                EndBufferStop := UnknownBufferStop;
                RouteEndLine := Signals[EndSignal].Signal_AdjacentLine;
              END ELSE BEGIN
                EndBufferStop := ExtractBufferStopFromString(DraftRouteArray[High(DraftRouteArray)]);
                RouteEndLine := BufferStops[EndBufferStop].BufferStop_AdjacentLine;
              END;

              { Now set up the locking }
              CreateLockingArrayFromDraftRouteArray(Train_LocoChip, DraftRouteArray, LockingArray);

              { Now create the route... }
              CreateRouteArrayFromLockingArray(Routes_RouteCounter, LockingArray, FinalRouteArray);

              { ...and the route-related arrays }
              CreateInitialRouteRelatedArrays(T, Train_LocoChip, FinalRouteArray, AutoRouteSetting, StartSignal, EndSignal, EndBufferStop, RouteStartLine, RouteEndLine);

              Routes_Journeys[Routes_RouteCounter] := JourneyCount;

              { ...and which way it's heading }
              AppendToDirectionArray(Routes_Directions, Train_CurrentDirection);

              IF TestingMode THEN BEGIN
                WriteStringArrayToLog(Train_LocoChip, 'R', 'Draft Route Array to set up R=' + IntToStr(Routes_RouteCounter)
                                                           + ' ' + DescribeStartAndEndOfRoute(Routes_RouteCounter),
                                      DraftRouteArray,
                                      2, 190, 'SR=');
                WriteStringArrayToLog(Train_LocoChip, 'R', 'Locking Array to set up R=' + IntToStr(Routes_RouteCounter)
                                                           + ' ' + DescribeStartAndEndOfRoute(Routes_RouteCounter),
                                      LockingArray,
                                      2, 190, 'SR=');

                FOR I := 0 TO (Routes_TotalSubRoutes[Routes_RouteCounter] - 1) DO
                  WriteStringArrayToLog(Train_LocoChip, 'R', 'Final Route Array to set up R='
                                                             + IntToStr(Routes_RouteCounter) + '/' + IntToStr(I)
                                                             + ' ' + DescribeSubRoute(Routes_RouteCounter, I),
                                        Routes_SubRouteSettingStrings[Routes_RouteCounter, I],
                                        2, 190, 'SR=');
              END;

              { See which is the first TC - store it in the journey record to use to work out which journey we are in. NB: some routes span more than one journey, if the
                journeys cover intermediate stations at which we do not stop, but that does not matter as we only need the data to work out when to start a journey.
              }
              JourneyCount := 0;
              JourneyFound := False;
              { See on which route the first journey is }
              WHILE (JourneyCount <= High(Train_JourneysArray))
              AND NOT JourneyFound
              DO BEGIN
                IF Routes_RouteCounter = NewRoute THEN BEGIN
                  JourneyFound := True;

                  TCCount := 0;
                  TCFound := False;
                  { See which TC is the first on it }
                  WHILE (TCCount <= High(FinalRouteArray))
                  AND NOT TCFound
                  DO BEGIN
                    IF Pos('TC=', FinalRouteArray[TCCount]) > 0 THEN BEGIN
                      Log(Train_LocoChipStr + ' R Journey ' + IntToStr(JourneyCount)
                                            + ' 1st TC=' + IntToStr(ExtractTrackCircuitFromString(FinalRouteArray[TCCount])));
                      TCFound := True;
                    END;
                    Inc(TCCount);
                  END; {WHILE}
                END;
                Inc(JourneyCount);
              END; {WHILE}

              { Store the appropriate trackcircuits in the record that controls where we are }
              FOR I := 0 TO High(FinalRouteArray) DO BEGIN
                IF Pos('TC=', FinalRouteArray[I]) > 0 THEN BEGIN
                  { but check for duplicates }
                  IF (Length(Train_TCsNotClearedArray) = 0)
                  OR (Train_TCsNotClearedArray[High(Train_TCsNotClearedArray)] <> FinalRouteArray[I])
                  THEN
                    AppendToStringArray(Train_TCsNotClearedArray, FinalRouteArray[I]);
                END;
              END; {FOR}

              { We use LockingArray as that has the starting signals first }
              FOR I := 0 TO High(LockingArray) DO BEGIN
                IF (Pos('TC=', LockingArray[I]) > 0) THEN BEGIN
                  { but check for duplicates }
                  IF NOT IsElementInStringArray(Train_TCsAndSignalsNotClearedArray, LockingArray[I], ElementPos) THEN
                    AppendToStringArray(Train_TCsAndSignalsNotClearedArray, LockingArray[I]);
                END ELSE
                  IF ((Pos('FS=', LockingArray[I]) > 0)
                       AND (Pos('FR', LockingArray[I]) = 0)
                       AND (ExtractSignalFromString(LockingArray[I]) <> SaveSignal))
                  THEN BEGIN
                    SaveSignal := ExtractSignalFromString(LockingArray[I]);
                    { record the first signal to be found, if it's not there already }
                    IF NOT IsSignalInStringArray(Train_TCsAndSignalsNotClearedArray, SaveSignal) THEN
                      AppendToStringArray(Train_TCsAndSignalsNotClearedArray, LockingArray[I]);
                  END;
              END;

              IF EndBufferStop <> UnknownBufferStop THEN
                AppendToStringArray(Train_TCsAndSignalsNotClearedArray, 'BS=' + IntToStr(EndBufferStop));

              Log(Train_LocoChipStr + ' L TC&SGnc:');
              WriteStringArrayToLog(Train_LocoChip, 'L', Train_TCsAndSignalsNotClearedArray, 2, 190);

              IF InitialTrackCircuitsRequired THEN BEGIN
                SetInitialTrackCircuits(T);
                IF Train_InitialTrackCircuits[1] = UnknownTC THEN
                  Log(Train_LocoChipStr + ' EG Initial trackcircuits not ok');
              END;
            END;
          END;
        END;
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG CreateRouteArraysForTrain: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CreateRouteArraysForTrain }

PROCEDURE TCreateRouteDisplayColoursWindow.CreateRouteDisplayColoursWindowCreate(Sender: TObject);
BEGIN
  CreateRouteDisplayColoursWindow.Height := CreateRouteDisplayColoursWindowHeight;
  CreateRouteDisplayColoursWindow.Width := CreateRouteDisplayColoursWindowWidth;
  CreateRouteDisplayColoursWindow.Top := CreateRouteDisplayColoursWindowTop;
  CreateRouteDisplayColoursWindow.Left := CreateRouteDisplayColoursWindowLeft;
END; { CreateRouteDisplayColoursWindowCreate }

PROCEDURE TCreateRouteDisplayColoursWindow.CreateRouteDisplayColoursWindowRichEditKeyDown(Sender: TObject; VAR Key: Word; Shift: TShiftState);
BEGIN
  CASE Key OF
    vk_Escape, vk_Return:
      CreateRouteDisplayColoursWindow.Hide;
  END; {CASE}
END; { CreateRouteDisplayColoursWindowRicheditKeyDown }

INITIALIZATION

END { CreateRoute }.
