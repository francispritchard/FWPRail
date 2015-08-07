UNIT LinesUnit;
{ Controls the various lines and line-related things

  Copyright © F.W. Pritchard 2015. All Rights Reserved.

  v0.1  02/02/15 Code mainly extracted mainly from InitVars
}
INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, InitVars;

TYPE
  TLinesUnitForm = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

  EndOfLineType = (BufferStopAtUp, BufferStopAtDown, ProjectedLineAtUp, ProjectedLineAtDown, NotEndOfLine, UnknownEndOfLine);
  GradientType = (Level, RisingIfUp, RisingIfDown, UnknownGradientType);
  TypeOfLine = (MainOrGoods, MainLine, GoodsLine, BranchLineDouble, BranchLineSingle, IslandStationLine, MainStationLine, BranchStationLine, WindowStationLine, SidingLine,
                FiddleyardLine, SidingsApproach, StationAvoiding, ProjectedLine, NewlyCreatedLine, UnknownTypeOfLine);
  { adjust FirstTypeOfLine or LastTypeOfLine if alteration made to above declaration }

CONST
  FirstTypeOfLine = MainOrGoods;
  LastTypeOfLine = ProjectedLine;

PROCEDURE AddNewRecordToLineDatabase;
{ Append a record to the line database }

PROCEDURE CalculateLinePolygons(Line : Integer);
{ Work out the position for the various line polygons }

PROCEDURE CalculateLinePositions;
{ Work out where the lines are on the screen }

PROCEDURE CheckLineConnectionsAreOK;
{ To check each line has the requisite connection data, as locking depends on this }

FUNCTION DeleteRecordFromLineDatabase(LineToDeleteNum : Integer) : Boolean;
{ Remove a record from the line database }

PROCEDURE FollowThatLine(CurrentLine, TC : Integer; SearchDirection : DirectionType; FindPoint : Boolean; OUT AdjoiningUpTC, AdjoiningDownTC, NextPoint : Integer);
{ Follow lines and points until we find a different track circuit }

FUNCTION GetBufferStopDirection(BufferStopNum : Integer) : DirectionType;
{ Return a buffer stop's direction }

FUNCTION GetLinesForTrackCircuit(TC : Integer) : IntegerArrayType;
{ Return all the lines on a given track circuit }

PROCEDURE InitialiseLineVariables(Line : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }

FUNCTION LinesAreAdjacent(L1, L2 : Integer; ErrorMsg : String) : Boolean;
{ Returns true if the two given lines are adjacent }

PROCEDURE ReadInLineDataFromDatabase;
{ Initialise the data for each of the line segments }

FUNCTION ValidateLineConnectionCh(LineConnectionCh : String; OUT ErrorMsg : String) : String;
{ See if the connection char exceeds one character }

FUNCTION ValidateLineDirection(LineDirectionStr : String; OUT ErrorMsg : String) : DirectionType;
{ Check the direction is correct }

FUNCTION ValidateLineEndOfLineMarker(EndOfLineMarkerStr : String; OUT ErrorMsg : String) : EndOfLineType;
{ See if the supplied end of line string is correct }

FUNCTION ValidateLineGradient(LineGradientStr : String; OUT ErrorMsg : String) : GradientType;
{ Checks the line's gradient is valid }

FUNCTION ValidateLineInUseFeedbackUnit(FeedbackUnitStr : String; OUT ErrorMsg : String) : Integer;
{ Only checks that the supplied data is numeric }

FUNCTION ValidateLineInUseFeedbackInput(FeedbackInputStr : String; OUT ErrorMsg : String) : Integer;
{ Check whether the line-in-use feedback input number is valid }

FUNCTION ValidateLineLocation(LineLocationStr : String; OUT ErrorMsg : String) : Integer;
{ Checks whether the line location is valid }

FUNCTION ValidateLineName(Str : String; Line : Integer; OUT ErrorMsg : String) : String;
{ Validates whether the new line name matches an existing one }

FUNCTION ValidateLineTrackCircuit(LineTCStr : String; OUT ErrorMsg : String) : Integer;
{ Checks the validity of the supplied line track circuit }

FUNCTION ValidateLineType(LineTypeStr : String; OUT ErrorMsg : String) : TypeOfLine;
{ See if the type of line is correct }

PROCEDURE WriteOutLineDataToDatabase;
{ Write out some line data to the line data file }

TYPE
  BufferStopRec = RECORD
    BufferStop_AdjacentLine : Integer;
    BufferStop_AdjacentTrackCircuit : Integer;
    BufferStop_AsTheatreDestination : String;
    BufferStop_CurrentColour : TColour;
    BufferStop_Direction : DirectionType;
    BufferStop_MouseRect : TRect; { mouse access rectangle }
    BufferStop_Number : Integer;
    BufferStop_X : Integer;
    BufferStop_Y1 : Integer;
    BufferStop_Y2 : Integer;
  END;

  LineRec = RECORD
    Line_AdjacentBufferStop : Integer;
    Line_BufferStopTheatreDestinationStr : String;
    Line_CurrentColour : TColour;
    Line_DataChanged : Boolean;
    Line_Direction : DirectionType;
    Line_DownConnectionCh : String;
    Line_DownConnectionChRect : TRect;
    Line_DownConnectionChBold : Boolean;
    Line_DownRow : Extended;
    Line_EndOfLineMarker : EndOfLineType;
    Line_RouteDrawnOver : Boolean; { use for route debugging }
    Line_Gradient : GradientType;
    Line_GridDownX : Integer;
    Line_GridDownY : Integer;
    Line_GridUpX : Integer;
    Line_GridUpY : Integer;
    Line_InitialOutOfUseState : OutOfUseState;
    Line_InUseFeedbackUnit : Integer;
    Line_InUseFeedbackInput : Integer;
    Line_Location : Integer;
    Line_LockFailureNotedInSubRouteUnit : Boolean;
    Line_MousePolygon : ARRAY [0..4] OF TPoint; { mouse access for indicators }
    Line_NameStr : String;
    Line_NextDownIsEndOfLine : EndOfLineType;
    Line_NextDownLine : Integer;
    Line_NextDownPoint : Integer;
    Line_NextDownType : NextLineRouteingType;
    Line_NextUpIsEndofLine : EndOfLineType;
    Line_NextUpLine : Integer;
    Line_NextUpPoint : Integer;
    Line_NextUpType : NextLineRouteingType;
    Line_NoLongerOutOfUse : Boolean;
    Line_Number : Integer;
    Line_OldColour : TColour;
    Line_OutOfUseState : OutOfUseState;
    Line_RoutedOver : Boolean;
    Line_RouteLockingForDrawing : Integer; { used for drawing those bits of Line that are routed over }
    Line_RouteSet : Integer;
    Line_SaveOutOfUseState : OutOfUseState;
    Line_TC : Integer;
    Line_TypeOfLine : TypeOfLine;
    Line_UpConnectionCh : String;
    Line_UpConnectionChRect : TRect;
    Line_UpConnectionChBold : Boolean;
    Line_UpRow : Extended;

    { For line editing }
    Line_DownHandlePolygon : ARRAY [0..4] OF TPoint;
    Line_IsBeingMovedByHandle : HandleType;
    Line_IsTempNewLine : Boolean;
    Line_MidHandlePolygon : ARRAY [0..4] OF TPoint;
    Line_ShowHandles : Boolean;
    Line_UpHandlePolygon : ARRAY [0..4] OF TPoint;
  END;

CONST
  Line_BufferStopTheatreDestinationStrFieldName : String = 'Buffer Stop Theatre Destination';
  Line_DirectionFieldName : String = 'Direction';
  Line_DownConnectionChFieldName : String = 'Down Connection Ch';
  Line_DownRowFieldName : String = 'Down Row';
  Line_GridDownXFieldName : String = 'Grid Down X';
  Line_GridDownYFieldName : String = 'Grid Down Y';
  Line_EndOfLineMarkerFieldName : String = 'End Of Line Marker';
  Line_GradientFieldName : String = 'Gradient';
  Line_InUseFeedbackUnitFieldName : String = 'In Use Feedback Unit';
  Line_InUseFeedbackInputFieldName : String = 'In Use Feedback Input';
  Line_LocationStrFieldName : String = 'Location';
  Line_NameStrFieldName : String = 'Line Name';
  Line_NumberFieldName : String = 'Line Number';
  Line_OutOfUseFieldName : String = 'Out Of Use';
  Line_TCFieldName : String = 'Line TC';
  Line_TypeOfLineFieldName : String = 'Type Of Line';
  Line_UpConnectionChFieldName : String = 'Up Connection Ch';
  Line_UpRowFieldName : String = 'Up Row';
  Line_GridUpXFieldName : String = 'Grid Up X';
  Line_GridUpYFieldName : String = 'Grid Up Y';

VAR
  LinesUnitForm: TLinesUnitForm;

  BufferStops : ARRAY OF BufferStopRec;
  Lines : ARRAY OF LineRec;
  LinesInitialised : Boolean = False;

IMPLEMENTATION

{$R *.dfm}

USES MiscUtils, RailDraw, Options, PointsUnit, TrackCircuitsUnit, System.Types, Feedback, LocationsUnit, Main, Logging;

CONST
  UnitRef = 'LinesUnit';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION GetBufferStopDirection(BufferStopNum : Integer) : DirectionType;
{ Return a buffer stop's direction }
BEGIN
  Result := BufferStops[BufferStopNum].BufferStop_Direction;
END; { GetBufferStopDirection }

FUNCTION LinesAreAdjacent(L1, L2 : Integer; ErrorMsg : String) : Boolean;
{ Returns true if the two given lines are adjacent }
BEGIN
  Result := False;
  ErrorMsg := '';

  IF Lines[L1].Line_NextUpLine = L2 THEN BEGIN
    Result := True;
    ErrorMsg := 'Next line up to ' + LineToStr(L1) + ' is ' + LineToStr(L2);
  END ELSE
    IF Lines[L1].Line_NextDownLine = L2 THEN BEGIN
      Result := True;
      ErrorMsg := 'Next line down to ' + LineToStr(L1) + ' is ' + LineToStr(L2);
    END ELSE
      IF Lines[L2].Line_NextUpLine = L1 THEN BEGIN
        Result := True;
        ErrorMsg := 'Next line up to ' + LineToStr(L2) + ' is ' + LineToStr(L1);
      END ELSE
        IF Lines[L2].Line_NextDownLine = L1 THEN BEGIN
          Result := True;
          ErrorMsg := 'Next line down to ' + LineToStr(L2) + ' is ' + LineToStr(L1);
        END;
END; { LinesAreAdjacent }

FUNCTION GetLinesForTrackCircuit(TC : Integer) : IntegerArrayType;
{ Return all the lines on a given track circuit }
VAR
  L : Integer;

BEGIN
  SetLength(Result, 0);

  FOR L := 0 TO High(Lines) DO
    IF (TC <> UnknownTrackCircuit) AND (TC = Lines[L].Line_TC) THEN
      AppendToLineArray(Result, L);
END; { GetLinesForTrackCircuit }

PROCEDURE CheckLineConnectionsAreOK;
{ To check each line has the requisite connection data, as locking depends on this }
VAR
  L : Integer;

BEGIN
  TRY
    Log('A CHECKING LINE CONNECTIONS ARE OK');

    FOR L := 0 TO High(Lines) DO BEGIN
      WITH Lines[L] DO BEGIN
        IF (Line_NextUpLine = UnknownLine)
        AND (Line_NextUpPoint = UnknownPoint)
        AND (Line_NextUpIsEndOfLine = NotEndOfLine)
        THEN BEGIN
//          Log('X No line continuation data for up of line ' + LineToStr(L));
//          IF MessageDialogueWithDefault('No line continuation data for up of line ' + LineToStr(L),
//                                        StopTimer, mtWarning, [mbOK, mbAbort], ['&Continue', '&Exit'], mbOK) = mrAbort
//          THEN
//            ShutDownProgram(UnitRef, 'CheckLinesAreOK');
        END;
        IF (Line_NextDownLine = UnknownLine)
        AND (Line_NextDownPoint = UnknownPoint)
        AND (Line_NextDownIsEndOfLine = NotEndOfLine)
        THEN BEGIN
//          Log('X No line continuation data for down of line ' + LineToStr(L));
//          IF MessageDialogueWithDefault('No line continuation data for down of line ' + LineToStr(L),
//                                        StopTimer, mtWarning, [mbOK, mbAbort], ['&Continue', '&Exit'], mbOK) = mrAbort
//          THEN
//            ShutDownProgram(UnitRef, 'CheckLinesAreOK');
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG CheckLinesAreOK:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CheckLinesAreOK }

PROCEDURE FollowThatLine(CurrentLine, TC : Integer; SearchDirection : DirectionType; FindPoint : Boolean; OUT AdjoiningUpTC, AdjoiningDownTC, NextPoint : Integer);
{ Follow lines and points until we find a different track circuit }

  FUNCTION FindTrackCircuitOrPoint(CurrentLine, TC : Integer; OUT AdjoiningUpTC, AdjoiningDownTC : Integer) : Boolean;
  BEGIN
    Result := False;
    TRY
      IF (Lines[CurrentLine].Line_TC <> UnknownTrackCircuit) AND (Lines[CurrentLine].Line_TC <> TC) THEN BEGIN
        Result := True;
        CASE SearchDirection OF
          Up:
            AdjoiningUpTC := Lines[CurrentLine].Line_TC;
          Down:
            AdjoiningDownTC := Lines[CurrentLine].Line_TC;
        END; {CASE}
      END;
    EXCEPT
      ON E : Exception DO
        Log('EG FoundAdjacentTrackCircuit:' + E.ClassName + ' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { FindTrackCircuitOrPoint }

VAR
  ExitFunction : Boolean;
  Next : NextLineRouteingType;
  SaveCurrentLine : Integer;

BEGIN
  TRY
    ExitFunction := False;
    WHILE NOT ExitFunction DO BEGIN
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

            IF FindPoint THEN
              Exit;

            { where to go next }
            IF SearchDirection = Points[NextPoint].Point_FacingDirection THEN BEGIN
              { a facing point - see which way it's set }
              IF Points[NextPoint].Point_PresentState = Straight THEN BEGIN
                IF InLockDebuggingMode THEN
                  DrawPoint(NextPoint, clLime);

                CurrentLine := Points[NextPoint].Point_StraightLine;
                IF FindTrackCircuitOrPoint(CurrentLine, TC, AdjoiningUpTC, AdjoiningDownTC) THEN
                  ExitFunction := True;
              END ELSE
                IF Points[NextPoint].Point_PresentState = Diverging THEN BEGIN
                  IF InLockDebuggingMode THEN
                    DrawPoint(NextPoint, clLime);

                  CurrentLine := Points[NextPoint].Point_DivergingLine;
                  IF (CurrentLine = UnknownLine) OR FindTrackCircuitOrPoint(CurrentLine, TC, AdjoiningUpTC, AdjoiningDownTC) THEN
                    ExitFunction := True;
                END ELSE BEGIN
                  { Points[NextPoint].Point_PresentState = PointStateUnknown }
                  IF InLockDebuggingMode THEN
                    DrawPoint(NextPoint, clRed);
                  ExitFunction := True;
                END;
            END ELSE BEGIN
              { a trailing point - if it's not set in our direction, stop searching here }
              IF ((CurrentLine = Points[NextPoint].Point_StraightLine) AND (Points[NextPoint].Point_PresentState = Straight)) THEN BEGIN
                CurrentLine := Points[NextPoint].Point_HeelLine;
                IF (CurrentLine = UnknownLine) OR FindTrackCircuitOrPoint(CurrentLine, TC, AdjoiningUpTC, AdjoiningDownTC) THEN
                    ExitFunction := True;

                IF InLockDebuggingMode THEN
                  DrawPoint(NextPoint, clLime)
                ELSE
                  DrawPoint(NextPoint, ForegroundColour);
              END ELSE
                IF ((CurrentLine = Points[NextPoint].Point_DivergingLine) AND (Points[NextPoint].Point_PresentState = Diverging)) THEN BEGIN
                  CurrentLine := Points[NextPoint].Point_HeelLine;
                  IF (CurrentLine = UnknownLine) OR FindTrackCircuitOrPoint(CurrentLine, TC, AdjoiningUpTC, AdjoiningDownTC) THEN
                      ExitFunction := True;

                  IF InLockDebuggingMode THEN
                    DrawPoint(NextPoint, clLime)
                  ELSE
                    DrawPoint(NextPoint, ForegroundColour);
                END ELSE BEGIN
                  { if it's not set in our direction, stop searching here }
                  IF InLockDebuggingMode THEN
                    DrawPoint(NextPoint, clRed)
                  ELSE
                    DrawPoint(NextPoint, ForegroundColour);
                  ExitFunction := True;
                END;
            END;
          END;
        LineIsNext:
          BEGIN
            { where to go next }
            SaveCurrentLine := CurrentLine;
            IF SearchDirection = Up THEN BEGIN
              CurrentLine := Lines[CurrentLine].Line_NextUpLine;
              IF CurrentLine = UnknownLine THEN BEGIN
                Log('XG Severe error: unknown line next up of ' + Lines[SaveCurrentLine].Line_NameStr);
                Exit;
              END;
            END ELSE BEGIN
              CurrentLine := Lines[CurrentLine].Line_NextDownLine;
              IF CurrentLine = UnknownLine THEN BEGIN
                Log('XG Severe error: unknown line next down of ' + Lines[SaveCurrentLine].Line_NameStr);
                Exit;
              END;
            END;

            IF NOT FindPoint THEN
              IF FindTrackCircuitOrPoint(CurrentLine, TC, AdjoiningUpTC, AdjoiningDownTC) THEN
                ExitFunction := True;
          END;
        EndOfLineIsNext:
          ExitFunction := True;
        UnknownNextLineRouteingType:
          ShowMessage('UnknownNextLineRouteingType at ' + LineToStr(CurrentLine));
      END; {CASE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FollowThatLine:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FollowThatLine }

PROCEDURE CalculateLineEndCharacters(Line : Integer);
{ Add the line-end characters which indicate where a line goes next }
VAR
  ScreenUpX : Integer;
  ScreenUpY : Integer;
  ScreenDownX : Integer;
  ScreenDownY : Integer;

BEGIN
  WITH RailWindowBitmap.Canvas DO BEGIN
    WITH Lines[Line] DO BEGIN
      ScreenUpX := MapGridXToScreenX(Line_GridUpX);
      ScreenUpY := MapGridYToScreenY(Line_GridUpY);
      ScreenDownX := MapGridXToScreenX(Line_GridDownX);
      ScreenDownY := MapGridYToScreenY(Line_GridDownY);

      { a straight line }
      Font.Height := -MulDiv(FWPRailWindow.ClientHeight, FWPRailWindowFontHeight, ZoomScaleFactor);
      IF Line_UpConnectionCh <> '' THEN BEGIN
        WITH Line_UpConnectionChRect DO BEGIN
          Left := ScreenUpX - (TextWidth(Line_UpConnectionCh) DIV 2);
          Top := ScreenUpY - (TextHeight(Line_UpConnectionCh) DIV 2);
          Right := ScreenUpX + (TextWidth(Line_UpConnectionCh) DIV 2);
          Bottom := ScreenUpY + (TextHeight(Line_UpConnectionCh) DIV 2);
        END; {WITH}
      END;

      IF Line_DownConnectionCh <> '' THEN BEGIN
        WITH Line_DownConnectionChRect DO BEGIN
          Left := ScreenDownX - (TextWidth(Line_DownConnectionCh) DIV 2);
          Top := ScreenDownY - (TextHeight(Line_DownConnectionCh) DIV 2);
          Right := ScreenDownX + (TextWidth(Line_DownConnectionCh) DIV 2);
          Bottom := ScreenDownY + (TextHeight(Line_DownConnectionCh) DIV 2);
        END; {WITH}
      END;
    END; {WITH}
  END; {WITH}
END; { CalculateLineEndCharacters }

PROCEDURE CalculateLinePositions;
{ Work out where the lines are on the screen }
VAR
  Line : Integer;
  OtherLine : Integer;

  PROCEDURE CreateBufferStop(L : Integer; BSDirection : DirectionType; BSTheatreDestination : String);
  { Create a bufferstop record }
  BEGIN
    SetLength(BufferStops, Length(BufferStops) + 1);

    WITH BufferStops[High(BufferStops)] DO BEGIN
      BufferStop_AdjacentLine := L;
      BufferStop_AdjacentTrackCircuit := Lines[BufferStop_AdjacentLine].Line_TC;
      IF (BufferStop_AdjacentTrackCircuit = UnknownTrackCircuit) AND ProgramStarting THEN
        Log('E Buffer stop ' + IntToStr(High(BufferStops)) + ' (at line' + ' ' + Lines[BufferStop_AdjacentLine].Line_NameStr + ') has no adjacent track circuit');

      BufferStop_AsTheatreDestination := BSTheatreDestination;
      BufferStop_CurrentColour := BufferStopColour;
      BufferStop_Direction := BSDirection;
      BufferStop_Number := High(BufferStops);

      Lines[L].Line_AdjacentBufferStop := BufferStop_Number;
    END; {WITH}
  END; { CreateBufferStop }

  PROCEDURE CalculateBufferStopPositions;
  { Work out where the buffer stops are on the screen }
  VAR
    BufferStop : Integer;

  BEGIN
    BufferStop := 0;
    WHILE BufferStop <= High(BufferStops) DO BEGIN
      WITH BufferStops[BufferStop] DO BEGIN
        IF BufferStop_Direction = Up THEN BEGIN
          BufferStop_X := MapGridXToScreenX(Lines[BufferStop_AdjacentLine].Line_GridUpX);
          BufferStop_Y1 := MapGridYToScreenY(Lines[BufferStop_AdjacentLine].Line_GridUpY) - BufferStopVerticalSpacingScaled;
          BufferStop_Y2 := MapGridYToScreenY(Lines[BufferStop_AdjacentLine].Line_GridUpY) + BufferStopVerticalSpacingScaled;
        END ELSE BEGIN
          BufferStop_X := MapGridXToScreenX(Lines[BufferStop_AdjacentLine].Line_GridDownX);
          BufferStop_Y1 := MapGridYToScreenY(Lines[BufferStop_AdjacentLine].Line_GridDownY) - BufferStopVerticalSpacingScaled;
          BufferStop_Y2 := MapGridYToScreenY(Lines[BufferStop_AdjacentLine].Line_GridDownY) + BufferStopVerticalSpacingScaled;
        END;

        { The mouse rectangle }
        WITH BufferStop_MouseRect DO BEGIN
          Left := BufferStop_X - MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScaleFactor);
          Top := BufferStop_Y1;
          Right := BufferStop_X + MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScaleFactor);
          Bottom := BufferStop_Y2;
        END; {WITH}
      END; {WITH}
      Inc(BufferStop);
    END; {WHILE}
  END; { CalculateBufferStopPositions }

BEGIN
  TRY
    Line := 0;
    WHILE Line <= High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        Line_GridUpY := Round(Line_UpRow * GridInterLineSpacing);
        Line_GridDownY := Round(Line_DownRow * GridInterLineSpacing);
      END; {WITH}
      Inc(Line);
    END; {WHILE}

    Line := 0;
    WHILE Line <= High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        CalculateLinePolygons(Line);
        CalculateLineEndCharacters(Line);
      END; {WITH}
      Inc(Line);
    END; {WHILE}

    { Sort out buffer stop positions }
    FOR Line := 0 TO High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        Line_CurrentColour := TCUnoccupiedColour;
        Line_OldColour := TCUnoccupiedColour;
        Line_RoutedOver := False;
        Line_NoLongerOutOfUse := False;

        CASE Line_EndOfLineMarker OF
          NotEndOfLine:
            BEGIN
              Line_NextUpType := LineIsNext;
              Line_NextUpIsEndofLine := NotEndOfLine;
              Line_NextDownType := LineIsNext;
              Line_NextDownIsEndOfLine := NotEndOfLine;
            END;
          BufferStopAtUp:
            BEGIN
              Line_NextUpType := EndOfLineIsNext;
              Line_NextUpIsEndofLine := BufferStopAtUp;
              Line_NextDownType := LineIsNext;
              { and create a bufferstop record }
              CreateBufferStop(Line, Up, Line_BufferStopTheatreDestinationStr);
            END;
          BufferStopAtDown:
            BEGIN
              Line_NextDownType := EndOfLineIsNext;
              Line_NextDownIsEndOfLine := BufferStopAtDown;
              Line_NextUpType := LineIsNext;
              { and create a bufferstop record }
              CreateBufferStop(Line, Down, Line_BufferStopTheatreDestinationStr);
            END;
          ProjectedLineAtUp:
            BEGIN
              Line_NextUpType := EndOfLineIsNext;
              Line_NextUpIsEndofLine := ProjectedLineAtUp;
              Line_NextDownType := LineIsNext;
            END;
          ProjectedLineAtDown:
            BEGIN
              Line_NextDownType := EndOfLineIsNext;
              Line_NextDownIsEndOfLine := ProjectedLineAtDown;
              Line_NextUpType := LineIsNext;
            END;
        END; { CASE }

        IF Line_NextUpType = UnknownNextLineRouteingType THEN BEGIN
          IF MessageDialogueWithDefault('Line_NextUpType = UnknownNextLineRouteingType at ' + LineToStr(Line),
                                        StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
          THEN
            ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
        END ELSE
          IF Line_NextDownType = UnknownNextLineRouteingType THEN BEGIN
            IF MessageDialogueWithDefault('Line_NextDownType = UnknownNextLineRouteingType at ' + LineToStr(Line),
                                          StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
            THEN
              ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
          END;

      END; {WITH}
    END; {FOR}

    { Now work out which lines are next to which }
    FOR Line := 0 TO High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        FOR OtherLine := 0 TO High(Lines) DO BEGIN
          IF Line <> OtherLine THEN BEGIN
            { First, see if a line is disconnected from its neighbour - look for an alphabetic character at its start or end }
            IF (Line_UpConnectionCh <> '') AND (Line_UpConnectionCh = Lines[OtherLine].Line_DownConnectionCh) THEN BEGIN
              Line_NextUpType := LineIsNext;
              Line_NextUpLine := OtherLine
            END ELSE
              IF (Line_DownConnectionCh <> '') AND (Line_DownConnectionCh = Lines[OtherLine].Line_UpConnectionCh) THEN BEGIN
                Line_NextDownType := LineIsNext;
                Line_NextDownLine := OtherLine
              END ELSE BEGIN
                { Now look for normal horizontal connections }
                IF (Line_GridDownX = Lines[OtherLine].Line_GridUpX) AND (Line_GridDownY = Lines[OtherLine].Line_GridUpY) AND
                  (Line_GridUpY = Lines[OtherLine].Line_GridDownY) THEN BEGIN
                  Line_NextDownType := LineIsNext;
                  Line_NextDownLine := OtherLine;
                END;

                IF (Line_GridUpX = Lines[OtherLine].Line_GridDownX) AND (Line_GridUpY = Lines[OtherLine].Line_GridDownY) AND
                  (Line_GridDownY = Lines[OtherLine].Line_GridUpY) THEN BEGIN
                  Line_NextUpType := LineIsNext;
                  Line_NextUpLine := OtherLine;
                END;
              END;

            { If there are no normal horizontal connections, see if there's a non-horizontal one }
            IF (Line_NextUpLine = UnknownLine) OR (Line_NextDownLine = UnknownLine) THEN BEGIN
              IF (Line_GridDownX = Lines[OtherLine].Line_GridUpX) AND (Line_GridDownY = Lines[OtherLine].Line_GridUpY) THEN BEGIN
                Line_NextDownType := LineIsNext;
                Line_NextDownLine := OtherLine;
              END;

              IF (Line_GridUpX = Lines[OtherLine].Line_GridDownX) AND (Line_GridUpY = Lines[OtherLine].Line_GridDownY) THEN BEGIN
                Line_NextUpType := LineIsNext;
                Line_NextUpLine := OtherLine;
              END;
            END;
          END;
        END;
      END; {WITH}
    END; {FOR}

    CalculateBufferStopPositions;

  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CalculateLinePositions: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CalculateLinePositions }

FUNCTION ValidateLineInUseFeedbackUnit(FeedbackUnitStr : String; OUT ErrorMsg : String) : Integer;
{ Only checks that the supplied data is numeric }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF (FeedbackUnitStr <> '') AND NOT TryStrToInt(FeedbackUnitStr, Result) THEN
    ErrorMsg := 'ValidateLineInUseFeedbackUnit: invalid line-in-use feedback unit "' + FeedbackUnitStr + '"';
END; { ValidateLineInUseFeedbackUnit }

FUNCTION ValidateLineInUseFeedbackInput(FeedbackInputStr : String; OUT ErrorMsg : String) : Integer;
{ Check whether the line-in-use feedback input number is valid }
BEGIN
  ErrorMsg := '';

  IF FeedbackInputStr = '' THEN
    Result := 0
  ELSE
    IF NOT TryStrToInt(FeedbackInputStr, Result) THEN
      ErrorMsg := 'ValidateLineInUseFeedbackInput: invalid integer "' + FeedbackInputStr + '"'
    ELSE
      IF (Result < 1) OR (Result > 8) THEN
        ErrorMsg := 'ValidateLineInUseFeedbackInput: feedback input number ' + IntToStr(Result) + ' is out of range';
END; { ValidateLineInUseFeedbackInput }

FUNCTION ValidateLineGradient(LineGradientStr : String; OUT ErrorMsg : String) : GradientType;
{ Checks the line's gradient is valid }
BEGIN
  ErrorMsg := '';

  Result := StrToGradient(LineGradientStr);
  IF (LineGradientStr = '') OR (Result = UnknownGradientType) THEN
    ErrorMsg := 'ValidateLineGradient: unknown gradient type "' + LineGradientStr + '"';
END; { ValidateLineGradient }

FUNCTION ValidateLineConnectionCh(LineConnectionCh : String; OUT ErrorMsg : String) : String;
{ See if the connection char exceeds one character }
BEGIN
  ErrorMsg := '';
  Result := '';

  IF Length(LineConnectionCh) > 1 THEN
    ErrorMsg := 'ValidateLineConnectionCh: line connection character "' + LineConnectionCh + '" is too long'
  ELSE
    Result := LineConnectionCh;
END; { ValidateLineConnectionCh }

FUNCTION ValidateLineType(LineTypeStr : String; OUT ErrorMsg : String) : TypeOfLine;
{ See if the type of line is correct }
BEGIN
  ErrorMsg := '';

  Result := StrToTypeOfLine(LineTypeStr);
  IF (LineTypeStr = '') OR (Result = UnknownTypeOfLine) THEN
    ErrorMsg := 'ValidateLineType: unknown type of line "' + LineTypeStr + '"';
END; { ValidateLineType }

FUNCTION ValidateLineDirection(LineDirectionStr : String; OUT ErrorMsg : String) : DirectionType;
{ Check the direction is correct }
BEGIN
  ErrorMsg := '';

  Result := StrToDirectionType(LineDirectionStr);
  IF (LineDirectionStr = '') OR (Result = UnknownDirection) THEN
    ErrorMsg := ':ValidateLineDirection unknown line direction "' + LineDirectionStr + '"';
END; { ValidateLineDirection }

FUNCTION ValidateLineEndOfLineMarker(EndOfLineMarkerStr : String; OUT ErrorMsg : String) : EndOfLineType;
{ See if the supplied end of line string is correct }
BEGIN
  ErrorMsg := '';

  Result := StrToEndOfLine(EndOfLineMarkerStr);
  IF (EndOfLineMarkerStr = '') OR (Result = UnknownEndOfLine) THEN
    ErrorMsg := 'ValidateLineEndOfLineMarker: unknown end of line marker "' + EndOfLineMarkerStr + '"';
END; { ValidateLineEndOfLineMarker }

FUNCTION ValidateLineTrackCircuit(LineTCStr : String; OUT ErrorMsg : String) : Integer;
{ Checks the validity of the supplied line track circuit }
BEGIN
  ErrorMsg := '';

  IF LineTCStr = '' THEN
    Result := UnknownTrackCircuit
  ELSE
    IF NOT TryStrToInt(LineTCStr, Result) THEN
      ErrorMsg := 'ValidateLineTrackCircuit: invalid line TC integer "' + LineTCStr + '"'
    ELSE
      IF ((Result < 0) AND (Result > High(TrackCircuits))) OR (Result = UnknownTrackCircuit) THEN
        ErrorMsg := 'ValidateLineTrackCircuit: track-circuit number ' + IntToStr(Result) + ' outside bounds';
END; { ValidateLineTrackCircuit }

FUNCTION ValidateLineLocation(LineLocationStr : String; OUT ErrorMsg : String) : Integer;
{ Checks whether the line location is valid }
BEGIN
  ErrorMsg := '';

  IF LineLocationStr = '' THEN
    Result := UnknownLocation
  ELSE BEGIN
    Result := StrToLocation(LineLocationStr);
    IF Result = UnknownLocation THEN
      ErrorMsg := 'ValidateLineLocation: unknown line location "' + LineLocationStr + '"';
  END;
END; { ValidateLineLocation }

FUNCTION ValidateLineName(Str : String; Line : Integer; OUT ErrorMsg : String) : String;
{ Validates whether the new line name matches an existing one }
VAR
  TempLine : Integer;

BEGIN
  ErrorMsg := '';
  Result := Str;

  TempLine := 0;
  WHILE (TempLine <= High(Lines)) AND (TempLine <> Line) AND (ErrorMsg = '') DO BEGIN
    { check all lines apart from the one we've just created }
    IF Str = Lines[TempLine].Line_NameStr THEN
      ErrorMsg := 'duplicate line name "' + Str + '" (' + IntToStr(TempLine) + ') found'
    ELSE
      Inc(TempLine);
  END; {WHILE}
END; { ValidateLineName }

PROCEDURE InitialiseLineVariables(Line : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }
BEGIN
  WITH Lines[Line] DO BEGIN
    Line_AdjacentBufferStop := UnknownBufferStop;
    Line_DownConnectionCh := '';
    Line_DownConnectionChBold := False;
    Line_EndOfLineMarker := NotEndOfLine;
    Line_LockFailureNotedInSubRouteUnit := False;
    Line_NameStr := '*' + IntToStr(Line) + '*';
    Line_NextDownIsEndOfLine := NotEndOfLine;
    Line_NextDownPoint := UnknownPoint;
    Line_NextDownLine := UnknownLine;
    Line_NextDownType := UnknownNextLineRouteingType;
    Line_NextUpIsEndofLine := NotEndOfLine;
    Line_NextUpLine := UnknownLine;
    Line_NextUpPoint := UnknownPoint;
    Line_NextUpType := UnknownNextLineRouteingType;
    Line_RouteDrawnOver := False;
    Line_RouteLockingForDrawing := UnknownRoute;
    Line_RouteSet := UnknownRoute;
    Line_TC := UnknownTrackCircuit;
    Line_TypeOfLine := NewlyCreatedLine;
    Line_UpConnectionCh := '';
    Line_UpConnectionChBold := False;
  END; {WITH}
END; { InitialiseLineVariables }

PROCEDURE ReadInLineDataFromDatabase;
{ Initialise the data for each of the line segments }
CONST
  Horizontal = True;
  StopTimer = True;

VAR
  ErrorMsg : String;
  HasFeedback : Boolean;
  Line : Integer;
  OtherLine : Integer;

BEGIN
  TRY
    Log('A INITIALISING LINES {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Line database file "' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase')
        ELSE
          Exit;
      END;

      LinesADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix
                                             + ';Persist Security Info=False';
      LinesADOConnection.Connected := True;
      LinesADOTable.Open;
      Log('T Line data table and connection opened to initialise the lines');

      LinesADOTable.Sort := '[' + Line_NumberFieldName + '] ASC';
      LinesADOTable.First;

      { and make sure the arrays are empty, otherwise the next time the screen is resized we will merely add more lines/bufferstops }
      SetLength(Lines, 0);
      SetLength(BufferStops, 0);

      WHILE NOT LinesADOTable.EOF DO BEGIN
        WITH LinesADOTable DO BEGIN
          SetLength(Lines, Length(Lines) + 1);
          Line := High(Lines);
          WITH Lines[Line] DO BEGIN
            ErrorMsg := '';
            InitialiseLineVariables(Line);
            Line_DataChanged := False;
            Line_GridUpX := 0;
            Line_GridDownX := 0;
            Line_GridUpY := 0;
            Line_GridDownY := 0;
            Line_IsTempNewLine := False;
            Line_IsBeingMovedByHandle := NoHandle;
            Line_ShowHandles := False;

            Line_Number := FieldByName(Line_NumberFieldName).AsInteger;
            IF Line_Number <> Line THEN
              ErrorMsg := 'it does not match the line number in the database (' + IntToStr(Line_Number) + ')';

            IF ErrorMsg = '' THEN
              Line_NameStr := ValidateLineName(FieldByName(Line_NameStrFieldName).AsString, Line, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_UpRow := ValidateRow(FieldByName('Up Row').AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_DownRow := ValidateRow(FieldByName('Down Row').AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_GridUpX := ValidateGridX(FieldByName(Line_GridUpXFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_GridDownX := ValidateGridX(FieldByName(Line_GridDownXFieldName).AsString, ErrorMsg);

//            IF ErrorMsg = '' THEN
//              Line_GridUpY := ValidateGridX(FieldByName(Line_GridUpYFieldName).AsString, ErrorMsg);
//
//            IF ErrorMsg = '' THEN
//              Line_GridDownY := ValidateGridX(FieldByName(Line_GridDownYFieldName).AsString, ErrorMsg);
//
            IF ErrorMsg = '' THEN
              Line_Location := ValidateLineLocation(FieldByName(Line_LocationStrFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_TC := ValidateLineTrackCircuit(FieldByName(Line_TCFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_EndOfLineMarker := ValidateLineEndOfLineMarker(FieldByName(Line_EndOfLineMarkerFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_BufferStopTheatreDestinationStr := FieldByName(Line_BufferStopTheatreDestinationStrFieldName).AsString;

            IF ErrorMsg = '' THEN
              Line_Direction := ValidateLineDirection(FieldByName(Line_DirectionFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_TypeOfLine := ValidateLineType(FieldByName(Line_TypeOfLineFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_UpConnectionCh := ValidateLineConnectionCh(FieldByName(Line_UpConnectionChFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_DownConnectionCh := ValidateLineConnectionCh(FieldByName(Line_DownConnectionChFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_Gradient := ValidateLineGradient(FieldByName(Line_GradientFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN BEGIN
              IF FieldByName(Line_OutOfUseFieldName).AsBoolean THEN BEGIN
                Line_InitialOutOfUseState := OutOfUse; { this is used only in deciding whether to save a changed state on exit }
                Line_OutOfUseState := OutOfUse;
                Line_SaveOutOfUseState := OutOfUse;
              END ELSE BEGIN
                Line_InitialOutOfUseState := InUse;
                Line_OutOfUseState := InUse;
                Line_SaveOutOfUseState := InUse;
              END;

              { Even if the line itself is in use, that can be overridden by the location being out of use }
              IF Line_Location <> UnknownLocation THEN
                IF Locations[Line_Location].Location_OutOfUse THEN
                  Line_OutOfUseState := OutOfUse;
            END;

            IF ErrorMsg = '' THEN
              Line_InUseFeedbackUnit := ValidateFeedbackUnit(FieldByName(Line_InUseFeedbackUnitFieldName).AsString, HasFeedback, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_InUseFeedbackInput := ValidateFeedbackInput(FieldByName(Line_InUseFeedbackInputFieldName).AsString, HasFeedback, Line_InUseFeedbackUnit, LineFeedback,
                                                               Line_Number, ErrorMsg);
            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating Line=' + IntToStr(High(Lines)) + ' (' + Line_NameStr + '): '
                                            + '[' + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
            END;
          END;
        END; {WITH}
        LinesADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      LinesADOTable.Close;
      LinesADOConnection.Connected := False;
      Log('T Line Data table and connection closed');
    END; {WITH}

    { Now we have to do certain tests as some of the data was not be available while we read it in. First see if there are any duplicate connection characters }
    FOR Line := 0 TO High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        FOR OtherLine := 0 TO High(Lines) DO BEGIN
          IF Line <> OtherLine THEN BEGIN
            IF (Line_UpConnectionCh <> '') AND (Line_UpConnectionCh = Lines[OtherLine].Line_UpConnectionCh) THEN BEGIN
              IF MessageDialogueWithDefault('Duplicate up connection character ''' + Line_UpConnectionCh + ''''
                                            + ' found at line ' + LineToStr(Line) + ' and at ' + LineToStr(OtherLine),
                                            StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
              THEN
                ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
            END ELSE
              IF (Line_DownConnectionCh <> '') AND (Line_DownConnectionCh = Lines[OtherLine].Line_DownConnectionCh) THEN BEGIN
                IF MessageDialogueWithDefault('Duplicate down connection character ''' + Line_DownConnectionCh + '''' +
                                              ' found at line ' + LineToStr(Line) + ' and at ' + LineToStr(OtherLine),
                                              StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
                THEN
                  ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
              END;
          END;
        END; {FOR}
      END; {WITH}
    END; {FOR}

    { Now work out the X and Y values }
    CalculateLinePositions;

    FOR Line := 0 TO High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        { In all cases, UpX should be less than DownX }
        IF Line_GridUpX > Line_GridDownX THEN BEGIN
          IF MessageDialogueWithDefault('Line ' + Line_NameStr + ': UpX > DownX',
                                        StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
          THEN
            ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
        END;
      END; {WITH}
    END; {FOR}

    { And check for lines which are declared but not initialised }
    FOR Line := 0 TO High(Lines) DO
      IF LineToStr(Line) = '' THEN
        Log('X! Line ' + IntToStr(Line) + ' does not have a name');

  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInLineDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}

(* TP { This is left over from the Turbo Pascal version }
    DX := (X2 - X1) / AspectRatio;
    { AMS incomprehensible comment follows: }
    { / not * cos here we are moving from screen coordinates to normalised coordinates not vice versa }
    DY := Y2 - Y1;

    { Scale to give accurate speeds }
    LineLength := Round(20 * Sqrt(DX * DX + DY * DY));
TP *)
END; { ReadInLineDataFromDatabase }

PROCEDURE AddNewRecordToLineDatabase;
{ Append a record to the line database }
BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Line database file "' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'AddNewRecordToLineDatabase')
        ELSE
          Exit;
      END;

      LinesADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix
                                             + ';Persist Security Info=False';
      LinesADOConnection.Connected := True;
      LinesADOTable.Open;
      LinesADOTable.Append;
      LinesADOTable.FieldByName(Line_NumberFieldName).AsInteger := High(Lines);
      LinesADOTable.Post;

      Log('S Line data table and connection opened to create record for Line ' + IntToStr(High(Lines)));
      { Tidy up the database }
      LinesADOTable.Close;
      LinesADOConnection.Connected := False;
      Log('S Line Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToLineDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { AddNewRecordToLineDatabase }

PROCEDURE WriteOutLineDataToDatabase;
{ Write out some line data to the line data file }
VAR
  Line : Integer;
  TempInt : Integer;
  TempExtended : Extended;
  TempStr : String;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Line database file "' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'WriteOutLineDataFromDatabase')
        ELSE
          Exit;
      END;

      LinesADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix
                                             + ';Persist Security Info=False';
      LinesADOConnection.Connected := True;
      LinesADOTable.Open;
      Log('L Loco Data table and connection opened to write out line data');

      Line := 0;
      WHILE Line <= High(Lines) DO BEGIN
        WITH Lines[Line] DO BEGIN
          { Deal with out-of-use changes }
          IF ((Line_OutOfUseState = OutOfUse)
               OR ((Line_Location <> UnknownLocation) AND Locations[Line_Location].Location_OutOfUse))
          AND (Line_InitialOutOfUseState = InUse)
          THEN BEGIN
            LinesADOTable.First;
            WHILE NOT LinesADOTable.EOF DO BEGIN
              WITH LinesADOTable DO BEGIN
                IF FieldByName(Line_NameStrFieldName).AsString = Line_NameStr THEN BEGIN
                  Edit;
                  FieldByName(Line_OutOfUseFieldName).AsBoolean := True;
                  Post;

                  Log('S Recording in line database that Line ' + IntToStr(Line) + ' (' + Line_NameStr + ') is out of use');
                END;
              END; {WITH}
              LinesADOTable.Next;
            END; {WHILE}
          END ELSE BEGIN
            IF (Line_OutOfUseState = InUse) AND (Line_InitialOutOfUseState = OutOfUse) THEN BEGIN
              LinesADOTable.First;
              WHILE NOT LinesADOTable.EOF DO BEGIN
                WITH LinesADOTable DO BEGIN
                  IF FieldByName(Line_NameStrFieldName).AsString = Line_NameStr THEN BEGIN
                    Edit;
                    FieldByName(Line_OutOfUseFieldName).AsBoolean := False;
                    Post;

                    Log('S Recording in line database that Line ' + IntToStr(Line) + ' (' + Line_NameStr + ') is in use');
                  END;
                END; {WITH}
                LinesADOTable.Next;
              END; {WHILE}
            END;
          END;
        END; {WITH}
        Inc(Line);
      END; {WHILE}

      { Now deal with any general editing changes }
      LinesADOTable.First;
      Line := 0;
      WHILE NOT LinesADOTable.EOF DO BEGIN
        WITH Lines[Line] DO BEGIN
          IF Line_DataChanged THEN BEGIN
            Line_DataChanged := False;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_NameStrFieldName + ' is ''' + Line_NameStr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_NameStrFieldName).AsString := Line_NameStr;
            LinesADOTable.Post;

            TempInt := Line_GridUpX;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_GridUpXFieldName + ' is ''' + IntToStr(TempInt) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_GridUpXFieldName).AsString := IntToStr(TempInt);
            LinesADOTable.Post;

            TempInt := Line_GridDownX;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_GridDownXFieldName + ' is ''' + IntToStr(TempInt) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_GridDownXFieldName).AsString := IntToStr(TempInt);
            LinesADOTable.Post;

            TempInt := Line_GridUpY;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_GridUpYFieldName + ' is ''' + IntToStr(TempInt) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_GridUpYFieldName).AsString := IntToStr(TempInt);
            LinesADOTable.Post;

            TempInt := Line_GridDownY;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_GridDownYFieldName + ' is ''' + IntToStr(TempInt) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_GridDownYFieldName).AsString := IntToStr(TempInt);
            LinesADOTable.Post;

            TempExtended := Line_UpRow;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_UpRowFieldName + ' is ''' + FloatToStr(TempExtended) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_UpRowFieldName).AsString := FloatToStr(TempExtended);
            LinesADOTable.Post;

            TempExtended := Line_DownRow;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_DownRowFieldName + ' is ''' + FloatToStr(TempExtended) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_DownRowFieldName).AsString := FloatToStr(TempExtended);
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_BufferStopTheatreDestinationStrFieldName
                   + ' is ''' + Line_BufferStopTheatreDestinationStr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_BufferStopTheatreDestinationStrFieldName).AsString := Line_BufferStopTheatreDestinationStr;
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_DirectionFieldName + ' is ''' + DirectionToStr(Line_Direction) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_DirectionFieldName).AsString := DirectionToStr(Line_Direction);
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_DownConnectionChFieldName + ' is ''' + Line_DownConnectionCh + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_DownConnectionChFieldName).AsString := Line_DownConnectionCh;
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_UpConnectionChFieldName + ' is ''' + Line_UpConnectionCh + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_UpConnectionChFieldName).AsString := Line_UpConnectionCh;
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_EndOfLineMarkerFieldName + ' is ''' + EndOfLineToStr(Line_EndOfLineMarker) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_EndOfLineMarkerFieldName).AsString := EndOfLineToStr(Line_EndOfLineMarker);
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_GradientFieldName + ' is ''' + GradientToStr(Line_Gradient) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_GradientFieldName).AsString := GradientToStr(Line_Gradient);
            LinesADOTable.Post;

            IF Line_InUseFeedbackUnit <> 0 THEN
              TempStr := IntToStr(Line_InUseFeedbackUnit)
            ELSE
              TempStr := '';
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_InUseFeedbackUnitFieldName + ' is ''' + TempStr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_InUseFeedbackUnitFieldName).AsString := TempStr;
            LinesADOTable.Post;

            IF Line_InUseFeedbackInput <> 0 THEN
              TempStr := IntToStr(Line_InUseFeedbackInput)
            ELSE
              TempStr := '';
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_InUseFeedbackInputFieldName + ' is ''' + TempStr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_InUseFeedbackInputFieldName).AsString := TempStr;
            LinesADOTable.Post;

            IF Line_Location <> UnknownLocation THEN
              Tempstr := LocationToStr(Line_Location)
            ELSE
              Tempstr := '';
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_LocationStrFieldName + ' is ''' + Tempstr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_LocationStrFieldName).AsString := Tempstr;
            LinesADOTable.Post;

            IF Line_TC <> UnknownTrackCircuit THEN
              TempStr := IntToStr(Line_TC)
            ELSE
              TempStr := '';
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_TCFieldName + ' is ''' + TempStr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_TCFieldName).AsString := TempStr;
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_TypeOfLineFieldName + ' is ''' + TypeOfLineToStr(Line_TypeOfLine) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_TypeOfLineFieldName).AsString := TypeOfLineToStr(Line_TypeOfLine);
            LinesADOTable.Post;
          END;
        END; {WITH}

        Inc(Line);
        LinesADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      LinesADOTable.Close;
      LinesADOConnection.Connected := False;
      Log('L Line Data table and connection closed after writing line data');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutLineDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutLineDataToDatabase }

FUNCTION DeleteRecordFromLineDatabase(LineToDeleteNum : Integer) : Boolean;
{ Remove a record from the line database }
BEGIN
  Result := False;
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Line database file "' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'DeleteRecordFromLineDatabase')
        ELSE
          Exit;
      END;

      LinesADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix
                                             + ';Persist Security Info=False';
      LinesADOConnection.Connected := True;
      LinesADOTable.Open;
      IF NOT LinesADOTable.Locate(Line_NumberFieldName, IntToStr(LineToDeleteNum), []) THEN BEGIN
        Log('L Line data table and connection opened to delete Line ' + LineToStr(LineToDeleteNum) + ' but it cannot be found');
      END ELSE BEGIN
        Log('L Line data table and connection opened to delete Line ' + LineToStr(LineToDeleteNum));

        { Delete the line }
        LinesADOTable.Delete;
        Log('LG Line ' + IntToStr(LineToDeleteNum) + ' has been deleted');
        Result := True;
      END;

      { Tidy up the database }
      LinesADOTable.Close;
      LinesADOConnection.Connected := False;
      Log('L Line Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DeleteRecordFromLineDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DeleteRecordFromLineDatabase }

PROCEDURE CalculateLinePolygons(Line : Integer);
{ Work out the position for the various line polygons }
VAR
  MidScreenX : Integer;
  MidScreenY : Integer;
  ScreenUpX : Integer;
  ScreenUpY : Integer;
  ScreenDownX : Integer;
  ScreenDownY : Integer;

BEGIN
  WITH Lines[Line] DO BEGIN
    ScreenUpX := MapGridXToScreenX(Line_GridUpX);
    ScreenUpY := MapGridYToScreenY(Line_GridUpY);
    ScreenDownX := MapGridXToScreenX(Line_GridDownX);
    ScreenDownY := MapGridYToScreenY(Line_GridDownY);

    { The mouse polygon }
    Line_MousePolygon[0] := Point(ScreenUpX, ScreenUpY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_MousePolygon[1] := Point(ScreenUpX, ScreenUpY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_MousePolygon[2] := Point(ScreenDownX, ScreenDownY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_MousePolygon[3] := Point(ScreenDownX, ScreenDownY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_MousePolygon[4] := Line_MousePolygon[0];

    { The handles }
    Line_UpHandlePolygon[0] := Point(ScreenUpX - SignalRadiusScaled, ScreenUpY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_UpHandlePolygon[1] := Point(ScreenUpX - SignalRadiusScaled, ScreenUpY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_UpHandlePolygon[2] := Point(ScreenUpX + SignalRadiusScaled, ScreenUpY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_UpHandlePolygon[3] := Point(ScreenUpX + SignalRadiusScaled, ScreenUpY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_UpHandlePolygon[4] := Line_UpHandlePolygon[0];

    Line_DownHandlePolygon[0] := Point(ScreenDownX + SignalRadiusScaled, ScreenDownY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_DownHandlePolygon[1] := Point(ScreenDownX + SignalRadiusScaled, ScreenDownY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_DownHandlePolygon[2] := Point(ScreenDownX - SignalRadiusScaled, ScreenDownY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_DownHandlePolygon[3] := Point(ScreenDownX - SignalRadiusScaled, ScreenDownY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_DownHandlePolygon[4] := Line_DownHandlePolygon[0];

    MidScreenX := ScreenUpX + ((ScreenDownX - ScreenUpX) DIV 2);
    MidScreenY := ScreenUpY + ((ScreenDownY - ScreenUpY) DIV 2);
    Line_MidHandlePolygon[0] := Point(MidScreenX - SignalRadiusScaled, MidScreenY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_MidHandlePolygon[1] := Point(MidScreenX - SignalRadiusScaled, MidScreenY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_MidHandlePolygon[2] := Point(MidScreenX + SignalRadiusScaled, MidScreenY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_MidHandlePolygon[3] := Point(MidScreenX + SignalRadiusScaled, MidScreenY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_MidHandlePolygon[4] := Line_MidHandlePolygon[0];
  END; {WITH}
END; { CalculateLinePolygons }

END { LinesUnit }.
