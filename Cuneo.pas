UNIT Cuneo;

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, InitVars, ExtCtrls, StdCtrls, Edit;

TYPE
  TCuneoWindow = CLASS(TForm)
    MouseButtonDownTimer: TTimer;
    SignalPostFlashingTimer: TTimer;
    PROCEDURE MouseButtonDownTimerTick(Sender: TObject);
    PROCEDURE SignalPostFlashingTimerTick(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE ChangeStateOfWhatIsUnderMouse(ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState; HelpRequired : Boolean);
{ See what the mouse is currently pointing at something, and change its state if appropriate }

FUNCTION GetLineFoundNum : Integer;
{ Returns which line we're on }

FUNCTION GetZoomRect : TRect;
{ Return the current zoom rectangle }

PROCEDURE MouseButtonPressed(Button : TMouseButton; ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState);
{ The mouse button has been pressed }

PROCEDURE MouseButtonReleased(Button : TMouseButton; ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState);
{ Button released }

PROCEDURE SetZoomRect(TempZoomRect : TRect);
{ Set the current zoom rectangle }

PROCEDURE WhatIsUnderMouse(ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState); Overload;
{ Returns the current mouse position and whether a specific item has been found at that position without a keypress being required }

PROCEDURE WhatIsUnderMouse(ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState; OUT BufferStopFoundNum : Integer; OUT IndicatorFoundNum : Integer;
                           OUT IndicatorFoundType : JunctionIndicatorType; OUT PointFoundNum : Integer; OUT SignalFoundNum : Integer; OUT SignalPostFoundNum : Integer;
                           OUT TheatreIndicatorFoundNum : Integer; OUT TRSPlungerFoundLocation : Integer); Overload;
{ Returns the current mouse position and whether a specific item has been found at that position without a keypress being required }

VAR
  CuneoWindow: TCuneoWindow;
  SaveGridClickPosX : Integer = -1;
  SaveGridClickPosY : Integer = -1;

IMPLEMENTATION

{$R *.dfm}

USES RailDraw, Locks, Startup, Route, GetTime, Diagrams, MiscUtils, Movement, Lenz, Input, LocoUtils, Types, CreateRoute, LocoDialogue, LocationData, Help, StrUtils,
     Options, Main, DateUtils;

CONST
  UnitRef = 'Cuneo';
  UndrawRequired = True;
  UndrawToBeAutomatic = True;

TYPE
  MouseButton = TMouseButton;

VAR
  ButtonPress : MouseButton;
  DownLineEndCharacterLine : Integer = UnknownLine;
  EmergencyRouteingStored : Boolean = False;
  LineFoundNum : Integer = UnknownLine;
  LineFoundArray : IntegerArrayType;
  MouseMovingX : Integer;
  MouseMovingY : Integer;
  MouseX : Integer;
  MouseY : Integer;
  MoveZoomWindowMode : Boolean = False;
  NewLineFoundNum : Integer = UnknownLine;
  SaveDivergingLine : Integer = UnknownLine;
  SaveDivergingLineColour : TColour;
  SaveDownBufferStop : Integer = UnknownBufferStop;
  SaveHeelLine : Integer = UnknownLine;
  SaveHeelLineColour : TColour;
  SaveIndicatorString : String;
  SaveLine : Integer = UnknownLine;
  SaveLineColour : TColour;
  SaveNextUpLineColour : TColour;
  SaveNextDownLine : Integer = UnknownLine;
  SaveNextDownLineColour : TColour;
  SaveNextDownPoint : Integer = UnknownPoint;
  SaveNextUpLine : Integer = UnknownLine;
  SaveNextUpPoint : Integer = UnknownPoint;
  SavePoint : Integer = UnknownPoint;
  SavePossibleRoutesArray : StringArrayType;
  SaveScreenClickPosX : Integer = -1;
  SaveScreenClickPosY : Integer = -1;
  SaveStraightLine : Integer = UnknownLine;
  SaveStraightLineColour : TColour;
  SaveUpBufferStop : Integer = UnknownBufferStop;
  SignalPostDrawn : Boolean = False;
  SignalPostToBeFlashed : Integer = UnknownSignal;
//  TCAdjoiningTCsDrawnNum : Integer = UnknownTrackCircuit;
//  TRSPlungerFoundLocation : Integer = UnknownLocation;
  UpLineEndCharacterLine : Integer = UnknownLine;
  ZoomRect : TRect;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION GetLineFoundNum : Integer;
{ Returns which line we're on }
BEGIN
  Result := LineFoundNum;
END; { GetLineFoundNum }

FUNCTION GetZoomRect : TRect;
{ Return the current zoom rectangle }
BEGIN
  Result := ZoomRect;
END; { GetZoomRect }

PROCEDURE SetZoomRect(TempZoomRect : TRect);
{ Set the current zoom rectangle }
BEGIN
  ZoomRect := TempZoomRect;
END; { SetZoomRect }

PROCEDURE WhatIsUnderMouseMainProc(ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState; OUT BufferStopFoundNum : Integer; OUT IndicatorFoundNum : Integer;
                                   OUT IndicatorFoundType : JunctionIndicatorType; OUT PointFoundNum : Integer; OUT SignalFoundNum : Integer;
                                   OUT SignalPostFoundNum : Integer; OUT TheatreIndicatorFoundNum : Integer; OUT TRSPlungerFoundLocation : Integer);
{ Returns the current mouse position and whether a specific item has been found at that position without a keypress being required }
CONST
  IndicatorToBeSet = True;
  SuppressMessage = True;
  UpLine = True;

VAR
  B, P, S : Integer;
  DebugStr : String;
  DuplicateLineFound : Boolean;
  GridX : Integer;
  GridY : Integer;
  I : Integer;
  J : Integer;
  Line : Integer;
  LockingFailureString : String;
  NearestLine : Integer;
  NewLine : Integer;
  ObjectFound : Boolean;
  SaveRecordLineDrawingMode : Boolean;
  StatusBarPanel1Str : String;
  StatusBarPanel2Str : String;
  T : TrainIndex;
  TC : Integer;
  TempDraftRouteArray : StringArrayType;
  TempLinesNotAvailableStr : String;
  DummyRow : Integer;
  TempStatusBarPanel1Str : String;
  TooNearSignal : Integer;
//  TRSPlungerFound : Boolean;

BEGIN
  TRY
    GridX := MapScreenXToGridX(ScreenClickPosX);
    GridY := MapScreenYToGridY(ScreenClickPosY);

    ObjectFound := False;
    IndicatorFoundNum := UnknownSignal;
    IndicatorFoundType := UnknownJunctionIndicator;
    LineFoundNum := UnknownLine;
    SetLength(LineFoundArray, 0);
    NewLineFoundNum := UnknownLine;
    BufferStopFoundNum := UnknownPoint;
    PointFoundNum := UnknownPoint;
    SignalFoundNum := UnknownSignal;
    SignalPostFoundNum := UnknownSignal;
    TheatreIndicatorFoundNum := UnknownSignal;
    TRSPlungerFoundLocation := UnknownLocation;
    UpLineEndCharacterLine := UnknownLine;
    DownLineEndCharacterLine := UnknownLine;

    { Only proceed if the mouse has moved }
    IF (ScreenClickPosX <> SaveScreenClickPosX) OR (ScreenClickPosY <> SaveScreenClickPosY) THEN BEGIN
      SaveScreenClickPosX := ScreenClickPosX;
      SaveScreenClickPosY := ScreenClickPosY;

      StatusBarPanel1Str := '';

      IF DebuggingMode THEN
        { Display mouse co-ordinates }
        WriteToStatusBarPanel(StatusBarPanel2, IntToStr(ScreenClickPosX) + ',' + IntToStr(ScreenClickPosY) + '; '
                              + IntToStr(GridX)  + '/1000,'
                              + IntToStr(GridY)  + '/1000');

      IF EditMode THEN BEGIN
        IF SignalDragging THEN
          DragSignal(EditedSignal, ScreenClickPosX, ScreenClickPosY, NearestLine, TooNearSignal)
        ELSE
          IF MoveZoomWindowMode THEN BEGIN
            HideStatusBarAndUpDownIndications;

            FWPRailWindow.HorzScrollBar.Position := FWPRailWindow.HorzScrollBar.Position + MouseMovingX - ScreenClickPosX;
            FWPRailWindow.VertScrollBar.Position := FWPRailWindow.VertScrollBar.Position + MouseMovingY - ScreenClickPosY;
            MouseMovingX := ScreenClickPosX;
            MouseMovingY := ScreenClickPosY;
          END ELSE
            IF EndOfLineDragging AND (EditedLine <> UnknownLine) THEN
              DragEndOfLine(GridX, GridY, ShiftState)
            ELSE
              IF EditMode AND WholeLineDragging AND (EditedLine <> UnknownLine) THEN
                DragWholeLine(GridX, GridY);
       END ELSE
        IF MoveZoomWindowMode THEN BEGIN
          HideStatusBarAndUpDownIndications;

          FWPRailWindow.HorzScrollBar.Position := FWPRailWindow.HorzScrollBar.Position + MouseMovingX - ScreenClickPosX;
          FWPRailWindow.VertScrollBar.Position := FWPRailWindow.VertScrollBar.Position + MouseMovingY - ScreenClickPosY;
          MouseMovingX := ScreenClickPosX;
          MouseMovingY := ScreenClickPosY;
        END;

      { Sort out the cursor }
      IF (CreateLineMode AND (GetNearestLine(ScreenClickPosX, ScreenClickPosY) <> UnknownLine))
      OR (CreateLineMode AND (ssCtrl IN ShiftState))
      THEN
        ChangeCursor(crDrag)
      ELSE
        IF SignalDragging AND (GetNearestLine(ScreenClickPosX, ScreenClickPosY) = UnknownLine) THEN
          ChangeCursor(crNoDrop)
        ELSE
          IF SignalDragging THEN
            ChangeCursor(crDrag)
          ELSE
            ChangeCursor(crDefault);

      MouseX := ScreenClickPosX + ScrollBarXAdjustment;
      MouseY := ScreenClickPosY + ScrollBarYAdjustment;

      { Signals }
      FOR S := 0 TO High(Signals) DO BEGIN
        WITH Signals[S] DO BEGIN
          IF NOT (EditMode AND SignalDragging AND (S = EditedSignal)) THEN BEGIN
            IF PtInRect(Signal_MouseRect, Point(MouseX, MouseY)) THEN BEGIN
              ObjectFound := True;
              TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'S' + IntToStr(S) + ' ';
              SignalFoundNum := S;

              IF SignalIsLocked(S, LockingFailureString) THEN
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + '[' + LockingFailureString + '] ';
              IF Signals[S].Signal_OutOfUse THEN
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + '(X) ';
              IF Signal_HiddenStationSignalAspect <> NoAspect THEN
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'HA=' + AspectToStr(Signal_HiddenStationSignalAspect, ShortStringType);

              { Show the track circuit which resets the signal }
              IF ShowSignalResettingTrackCircuitsInStatusBar THEN BEGIN
                IF NOT FindNextSignalOrBufferStop(S, UnknownSignal, UnknownBufferStop, NOT IndicatorToBeSet, TempLinesNotAvailableStr, TempDraftRouteArray) THEN
                  TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'No RTC'
                ELSE BEGIN
                  CreateLockingArrayFromDraftRouteArray(UnknownLocoChipStr, TempDraftRouteArray, Signals[S].Signal_RouteLockingNeededArray);
                  TC := GetResettingTrackCircuit(UnknownLocoChipStr, S, SuppressMessage);
                  TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'RTC=' + IntToStr(TC);
                END;
              END;
            END ELSE
              IF PtInRect(Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_MouseRect, Point(MouseX, MouseY))
              AND ((Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_MouseRect.Left <> 0)
                    AND (Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_MouseRect.Bottom <> 0))
              THEN BEGIN
                ObjectFound := True;
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'ULI for S' + IntToStr(S) + ' ';
                IndicatorFoundNum := S;
                IndicatorFoundType := UpperLeftIndicator;
              END ELSE
                IF PtInRect(Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_MouseRect, Point(MouseX, MouseY))
                AND ((Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_MouseRect.Left <> 0)
                      AND (Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_MouseRect.Bottom <> 0))
                THEN BEGIN
                  ObjectFound := True;
                  TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'MLI for S' + IntToStr(S) + ' ';
                  IndicatorFoundNum := S;
                  IndicatorFoundType := MiddleLeftIndicator;
                END ELSE
                  IF PtInRect(Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_MouseRect, Point(MouseX, MouseY))
                  AND ((Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_MouseRect.Left <> 0)
                        AND (Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_MouseRect.Bottom <> 0))
                  THEN BEGIN
                    ObjectFound := True;
                    TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'LLI for S' + IntToStr(S) + ' ';
                    IndicatorFoundNum := S;
                    IndicatorFoundType := LowerLeftIndicator;
                  END ELSE
                    IF PtInRect(Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_MouseRect, Point(MouseX, MouseY))
                    AND ((Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_MouseRect.Left <> 0)
                          AND (Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_MouseRect.Bottom <> 0))
                    THEN BEGIN
                      ObjectFound := True;
                      TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'URI for S' + IntToStr(S) + ' ';
                      IndicatorFoundNum := S;
                      IndicatorFoundType := UpperRightIndicator;
                    END ELSE
                      IF PtInRect(Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_MouseRect, Point(MouseX, MouseY))
                      AND ((Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_MouseRect.Left <> 0)
                            AND (Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_MouseRect.Bottom <> 0))
                      THEN BEGIN
                        ObjectFound := True;
                        TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'MRI for S' + IntToStr(S) + ' ';
                        IndicatorFoundNum := S;
                        IndicatorFoundType := MiddleRightIndicator;
                      END ELSE
                        IF PtInRect(Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_MouseRect, Point(MouseX, MouseY))
                        AND ((Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_MouseRect.Left <> 0)
                              AND (Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_MouseRect.Bottom <> 0))
                        THEN BEGIN
                          ObjectFound := True;
                          TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'LRI for S' + IntToStr(S) + ' ';
                          IndicatorFoundNum := S;
                          IndicatorFoundType := LowerRightIndicator;
                        END ELSE
                          IF PtInRect(Signal_PostMouseRect, Point(MouseX, MouseY))
                          AND ((Signal_PostMouseRect.Left <> 0)
                               AND (Signal_PostMouseRect.Bottom <> 0))
                          THEN BEGIN
                            ObjectFound := True;
                            TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'SP for S' + IntToStr(S) + ' ';
                            SignalPostFoundNum := S;
                        END ELSE
                          IF PtInRect(Signal_IndicatorMouseRect, Point(MouseX, MouseY))
                          AND ((Signal_IndicatorMouseRect.Left <> 0)
                               AND (Signal_IndicatorMouseRect.Bottom <> 0))
                          THEN BEGIN
                            ObjectFound := True;
                            TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'T for S' + IntToStr(S) + ' ';
                            TheatreIndicatorFoundNum := S;

                            { Show the track circuit which resets the signal if the indicator is on }
                            IF ShowSignalResettingTrackCircuitsInStatusBar THEN BEGIN
                              IF Signals[S].Signal_Indicator <> NoIndicator THEN BEGIN
                                IF NOT FindNextSignalOrBufferStop(S, UnknownSignal, UnknownBufferStop, IndicatorToBeSet, TempLinesNotAvailableStr, TempDraftRouteArray)
                                THEN
                                  TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'No RTC'
                                ELSE BEGIN
                                  CreateLockingArrayFromDraftRouteArray(UnknownLocoChipStr, TempDraftRouteArray, Signals[S].Signal_RouteLockingNeededArray);
                                  TC := GetResettingTrackCircuit(UnknownLocoChipStr, S, SuppressMessage);
                                  TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'RTC=' + IntToStr(TC);
                                END;
                              END;
                            END;
                          END;
          END;
        END; {WITH}
      END;

      StatusBarPanel1Str := TempStatusBarPanel1Str;
      TempStatusBarPanel1Str := '';

      { Points }
      IF PointDebuggingMode THEN BEGIN
        SaveRecordLineDrawingMode := RecordLineDrawingMode;
        RecordLineDrawingMode := False;
        IF SaveHeelLine <> UnknownLine THEN
          DrawLine(SaveHeelLine, SaveHeelLineColour, NOT ActiveTrain);
        IF SaveStraightLine <> UnknownLine THEN
          DrawLine(SaveStraightLine, SaveStraightLineColour, NOT ActiveTrain);
        IF SaveDivergingLine <> UnknownLine THEN
          DrawLine(SaveDivergingLine, SaveDivergingLineColour, NOT ActiveTrain);
        IF SavePoint <> UnknownPoint THEN
          DrawPoint(SavePoint, BackgroundColour);
        RecordLineDrawingMode := SaveRecordLineDrawingMode;
      END;

      PointFoundNum := UnknownPoint;
      P := 0;
      WHILE (P <= High(Points)) AND (PointFoundNum = Unknownpoint) DO BEGIN
        WITH Points[P] DO BEGIN
          IF PtInRect(Point_MouseRect, Point(MouseX, MouseY)) THEN BEGIN
            ObjectFound := True;

            { Change the cursor as often points are difficult to focus on }
            IF NOT SignalDragging AND NOT CreateLineMode AND (Screen.Cursor <> crCross) THEN
              ChangeCursor(crCross);

            TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'P' + IntToStr(P) + ' ';
            IF PointIsLocked(P, LockingFailureString) THEN
              TempStatusBarPanel1Str := TempStatusBarPanel1Str + '[' + LockingFailureString + '] ';
            IF Point_ManualOperation THEN
              TempStatusBarPanel1Str := TempStatusBarPanel1Str + '[manual] ';
            IF Point_OutOfUse THEN
              TempStatusBarPanel1Str := TempStatusBarPanel1Str + '[out of use] ';

            IF ShowLinesWhichLockPoints THEN BEGIN
              IF Points[P].Point_LockedIfHeelTCOccupied THEN
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'Heel line = ' + LineToStr(Points[P].Point_HeelLine) + ' ';

              IF Points[P].Point_LockedIfNonHeelTCsOccupied THEN BEGIN
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'Straight line = ' + LineToStr(Points[P].Point_StraightLine) + ' ';
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'Diverging line = ' + LineToStr(Points[P].Point_DivergingLine) + ' ';
              END;
            END;

            PointFoundNum := P;
            IF PointDebuggingMode THEN BEGIN
              SaveRecordLineDrawingMode := RecordLineDrawingMode;
              RecordLineDrawingMode := SaveRecordLineDrawingMode;
              DrawPoint(P, clAqua);
              SavePoint := P;
              SaveHeelLine := Point_HeelLine;
              SaveHeelLineColour := Lines[Point_HeelLine].Line_CurrentColour;
              DrawLine(Point_HeelLine, PointHeelLineColour, NOT ActiveTrain);
              DebugStr := 'H=' + LineToStr(Point_HeelLine);

              SaveStraightLine := Point_StraightLine;
              SaveStraightLineColour := Lines[Point_StraightLine].Line_CurrentColour;
              DrawLine(Point_StraightLine, PointStraightLineColour, NOT ActiveTrain);
              DebugStr := DebugStr + ' S=' + LineToStr(Point_StraightLine);

              SaveDivergingLine := Point_DivergingLine;
              SaveDivergingLineColour := Lines[Point_DivergingLine].Line_CurrentColour;
              DrawLine(Point_DivergingLine, PointDivergingLineColour, NOT ActiveTrain);
              RecordLineDrawingMode := SaveRecordLineDrawingMode;
              DebugStr := DebugStr + ' D=' + LineToStr(Point_DivergingLine);

              Debug(DebugStr);
            END;
          END;
        END; {WITH}
        Inc(P);
      END; {WHILE}

      IF TempStatusBarPanel1Str <> '' THEN BEGIN
        IF StatusBarPanel1Str <> '' THEN
          { add a separator }
          StatusBarPanel1Str := StatusBarPanel1Str + ';' + TempStatusBarPanel1Str
        ELSE
          StatusBarPanel1Str := TempStatusBarPanel1Str;
        TempStatusBarPanel1Str := '';
      END;

      { Look for line related things }
      IF LockDebuggingMode THEN BEGIN
        IF SaveNextUpPoint <> UnknownPoint THEN
          DrawPoint(SaveNextUpPoint, BackgroundColour);
        IF SaveNextDownPoint <> UnknownPoint THEN
          DrawPoint(SaveNextDownPoint, BackgroundColour);
      END;

      IF LockDebuggingMode OR LineDebuggingMode THEN BEGIN
        SaveRecordLineDrawingMode := RecordLineDrawingMode;
        RecordLineDrawingMode := False;
        IF SaveLine <> UnknownLine THEN
          DrawLine(SaveLine, SaveLineColour, NOT ActiveTrain);
        IF SaveUpBufferStop <> UnknownBufferStop THEN
          DrawBufferStop(SaveUpBufferStop, BufferStopColour);
        IF SaveDownBufferStop <> UnknownBufferStop THEN
          DrawBufferStop(SaveDownBufferStop, BufferStopColour);
        IF SaveNextUpLine <> UnknownLine THEN
          DrawLine(SaveNextUpLine, TCUnoccupiedColour, NOT ActiveTrain);
        IF SaveNextDownLine <> UnknownLine THEN
          DrawLine(SaveNextDownLine, TCUnoccupiedColour, NOT ActiveTrain);
        RecordLineDrawingMode := SaveRecordLineDrawingMode;
      END;

      { Buffer stops }
      B := 0;
      BufferStopFoundNum := UnknownBufferStop;
      WHILE (B <= High(BufferStops)) AND (BufferStopFoundNum = UnknownBufferStop) DO BEGIN
        WITH BufferStops[B] DO BEGIN
          IF PtInRect(BufferStop_MouseRect, Point(MouseX, MouseY)) THEN BEGIN
            ObjectFound := True;
            BufferStopFoundNum := BufferStops[B].BufferStop_Number;
            TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'B' + IntToStr(BufferStopFoundNum) + ' ';
          END;
        END; {WITH}
        Inc(B);
      END; {WHILE}

      IF TempStatusBarPanel1Str <> '' THEN BEGIN
        IF StatusBarPanel1Str <> '' THEN
          { add a separator }
          StatusBarPanel1Str := StatusBarPanel1Str + ';' + TempStatusBarPanel1Str
        ELSE
          StatusBarPanel1Str := TempStatusBarPanel1Str;
        TempStatusBarPanel1Str := '';
      END;

      { Lines: see if the mouse pointer is within lines rectangle - note: no guarantee that MX1 < MX2 }
      FOR Line := 0 TO High(Lines) DO BEGIN
        WITH Lines[Line] DO BEGIN
          IF PointInPolygon(Line_MousePolygon, Point(MouseX, MouseY)) THEN BEGIN
            ObjectFound := True;

            { Write out the line name }
            TempStatusBarPanel1Str := TempStatusBarPanel1Str + ' (' + LineToStr(Line) + ' [' + LocationToStr(Lines[Line].Line_Location, ShortStringType) + ']';
            IF Lines[Line].Line_RouteLockingForDrawing <> UnknownRoute THEN
              TempStatusBarPanel1Str := TempStatusBarPanel1Str + '[R=' + IntToStr(Lines[Line].Line_RouteLockingForDrawing) + ']';
            TempStatusBarPanel1Str := TempStatusBarPanel1Str + ')';
            LineFoundNum := Line;
            AppendToIntegerArray(LineFoundArray, Line);

            IF Line_OutOfUseState = OutOfUse THEN BEGIN
              IF (Lines[Line].Line_Location <> UnknownLocation) AND (Locations[Lines[Line].Line_Location].Location_OutOfUse) THEN
                { it's not necessarily the line itself that's out of use }
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + '[location ' + LocationToStr(Lines[Line].Line_Location, ShortStringType) + ' out of use]'
              ELSE
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + '[line out of use]';
            END;

            { Debug('Up=' +  LineToStr(Lines[L].Line_NextUpLine) + ' Down=' + LineToStr(Lines[L].Line_NextDownLine)); }

            { Look for track circuits }
            IF (Lines[Line].Line_TC <> UnknownTrackCircuit) THEN BEGIN
              IF TrackCircuits[Lines[Line].Line_TC].TC_LocoChip <> UnknownLocoChip THEN BEGIN
                T := GetTrainIndexFromLocoChip(TrackCircuits[Lines[Line].Line_TC].TC_LocoChip);
                IF T <> UnknownTrainIndex THEN BEGIN
                  WITH Trains[T] DO BEGIN
                    ObjectFound := True;
                    StatusBarPanel2Str := LocoChipToStr(Train_LocoChip) + ': ' + TrainStatusToStr(Train_CurrentStatus);
                    IF Train_RouteCreationHoldMsg <> '' THEN
                      StatusBarPanel2Str := StatusBarPanel2Str + IfThen(Train_RouteCreationHeldJourney = Train_CurrentJourney,
                                                                        { only display route creation hold messages if the journey is the current one }
                                                                        IfThen(Train_RouteCreationHoldMsg <> '',
                                                                               ' - ' + Train_RouteCreationHoldMsg)
                                                               + IfThen(Train_RouteCreationReleasedMsg <> '',
                                                                        ' - ' + Train_RouteCreationReleasedMsg));

                    IF Train_CurrentRoute < High(Train_JourneysArray) THEN
                      IF Train_CurrentRoute < Length(Routes_RoutesSettingUpStalledMsgArray) THEN
                        IF Routes_RoutesSettingUpStalledMsgArray[Train_CurrentRoute] <> '' THEN
                          StatusBarPanel2Str := StatusBarPanel2Str + ' - R=' + IntToStr(Train_CurrentRoute) + ' stalled '
                                                                   + Routes_RoutesSettingUpStalledMsgArray[Train_CurrentRoute];
                  END;
                END;
              END;

              TempStatusBarPanel1Str := TempStatusBarPanel1Str + ' TC=' + IntToStr(Lines[Line].Line_TC);
              IF TrackCircuits[Lines[Line].Line_TC].TC_Headcode <> '' THEN
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + ' ' + TrackCircuits[Lines[Line].Line_TC].TC_Headcode;
              IF TrackCircuits[Lines[Line].Line_TC].TC_SpeedRestrictionInMPH <> NoSpecifiedSpeed THEN BEGIN
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + ' ' + ' max:' + MPHToStr(TrackCircuits[Lines[Line].Line_TC].TC_SpeedRestrictionInMPH);
                IF TrackCircuits[Lines[Line].Line_TC].TC_SpeedRestrictionDirection <> Bidirectional THEN
                  IF TrackCircuits[Lines[Line].Line_TC].TC_SpeedRestrictionDirection = Up THEN
                    TempStatusBarPanel1Str := TempStatusBarPanel1Str + '<'
                  ELSE
                    { TrackCircuits[Lines[L].Line_TC].TC_SpeedRestrictionDirection = Down }
                    TempStatusBarPanel1Str := TempStatusBarPanel1Str + '>';
              END;
              IF TrackCircuits[Lines[Line].Line_TC].TC_UserMustDrive THEN
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + ' {U}';

              TempStatusBarPanel1Str := TempStatusBarPanel1Str + ' [' + TrackCircuitStateToStr(TrackCircuits[Lines[Line].Line_TC].TC_OccupationState);
              IF (TrackCircuits[Lines[Line].Line_TC].TC_LocoChip <> UnknownLocoChip) AND (TrackCircuits[Lines[Line].Line_TC].TC_OccupationState <> TCUnoccupied) THEN
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + ' by ' + IntToStr(TrackCircuits[Lines[Line].Line_TC].TC_LocoChip);

              TempStatusBarPanel1Str := TempStatusBarPanel1Str + ']';

              IF TrackCircuits[Lines[Line].Line_TC].TC_LockedForRoute <> UnknownRoute THEN BEGIN
                TempStatusBarPanel1Str := TempStatusBarPanel1Str + ' [R=' + IntToStr(TrackCircuits[Lines[Line].Line_TC].TC_LockedForRoute)
                                                                 + ' (' + LocoChipToStr(Routes_LocoChips[TrackCircuits[Lines[Line].Line_TC].TC_LockedForRoute]) + ')';
                IF TrackCircuits[Lines[Line].Line_TC].TC_Journey = UnknownJourney THEN
                  TempStatusBarPanel1Str := TempStatusBarPanel1Str + ']'
                ELSE
                  TempStatusBarPanel1Str := TempStatusBarPanel1Str + ' (J=' + IntToStr(TrackCircuits[Lines[Line].Line_TC].TC_Journey) + ')]';
              END;

            END;

            IF LineDebuggingMode THEN BEGIN
              SaveRecordLineDrawingMode := RecordLineDrawingMode;
              RecordLineDrawingMode := False;
              WITH Lines[Line] DO BEGIN
                CASE Lines[Line].Line_NextUpType OF
                  EndOfLineIsNext:
                    IF Lines[Line].Line_NextUpIsEndOfLine = BufferStopAtUp THEN BEGIN
                      SaveUpBufferStop := Line_AdjacentBufferStop;
                      DrawBufferStop(Line_AdjacentBufferStop, clLime);
                    END;
                  PointIsNext:
                    BEGIN
                      SaveNextUpPoint := Lines[Line].Line_NextUpPoint;
                      DrawPoint(Lines[Line].Line_NextUpPoint, clLime);
                    END;
                  LineIsNext:
                    BEGIN
                      SaveNextUpLine := Line_NextUpLine;
                      SaveNextUpLineColour := Lines[Line_NextUpLine].Line_CurrentColour;
                      DrawLine(Lines[Line].Line_NextUpLine, clLime, NOT ActiveTrain);
                    END;
                END; {CASE}

                SaveLine := Line;
                SaveLineColour := Lines[Line].Line_CurrentColour;
                DrawLine(Line, clYellow, NOT ActiveTrain);

                IF Lines[Line].Line_NextDownIsEndOfLine = BufferStopAtDown THEN BEGIN
                  SaveDownBufferStop := Lines[Line].Line_AdjacentBufferStop;
                  DrawBufferStop(Lines[Line].Line_AdjacentBufferStop, clRed)
                END ELSE
                  IF Lines[Line].Line_NextDownPoint <> UnknownPoint THEN BEGIN
                    SaveNextDownPoint := Lines[Line].Line_NextDownPoint;
                    DrawPoint(Lines[Line].Line_NextDownPoint, clRed);
                  END ELSE BEGIN
                    SaveNextDownLine := Line_NextDownLine;
                    SaveNextDownLineColour := Lines[Line_NextDownLine].Line_CurrentColour;
                    DrawLine(Lines[Line].Line_NextDownLine, clRed, NOT ActiveTrain);
                  END;
              END; {WITH}
              RecordLineDrawingMode := SaveRecordLineDrawingMode;
            END;

  //          { If the mouse has moved away from a line, this will clear the panel }
  //          IF FWPRailWindow.FWPRailWindowStatusBar.Panels[StatusBarPanel2].Text <> '' THEN
  //            WriteToStatusBarPanel(StatusBarPanel2, '');
          END;
        END; {WITH}
      END;

      IF TempStatusBarPanel1Str <> '' THEN BEGIN
        IF StatusBarPanel1Str <> '' THEN
          { add a separator }
          StatusBarPanel1Str := StatusBarPanel1Str + ';' + TempStatusBarPanel1Str
        ELSE
          StatusBarPanel1Str := TempStatusBarPanel1Str;
        TempStatusBarPanel1Str := '';
      END;

      { Is it a line-end character? - if so, highlight the corresponding line-end } { move this to Raildraw? ********* }
      FOR Line := 0 TO High(Lines) DO BEGIN
        WITH Lines[Line] DO BEGIN
          WITH RailWindowBitmap.Canvas DO BEGIN
            IF PtInRect(Line_UpConnectionChRect, Point(MouseX, MouseY)) THEN
              UpLineEndCharacterLine := Line;
            IF PtInRect(Line_DownConnectionChRect, Point(MouseX, MouseY)) THEN
              DownLineEndCharacterLine := Line;
          END; {WITH}
        END; {WITH}
      END; {FOR}

    //  { Is it a TRS Plunger? }
    //  TRSPlungerFound := False;
    //  TRSPlungerLocation := FirstMainPlatformLocation;
    //  WHILE (TRSPlungerLocation <= LastMainPlatformLocation) AND NOT TRSPlungerFound DO BEGIN
    //    IF PtInRect(MainPlatformPlungers[TRSPlungerLocation].TRSPlunger_MouseRect, Point(MouseX, MouseY)) THEN BEGIN
    //      ObjectFound := True;
    //      TRSPlungerFound := True;
    //      TempStatusBarPanel1Str := TempStatusBarPanel1Str + 'TRSPlunger for ' + MainPlatformPlungers[TRSPlungerLocation].TRSPlunger_PlatformNumStr + ' ';
    //      TRSPlungerFoundLocation := TRSPlungerLocation;
    //    END ELSE
    //      Inc(TRSPlungerLocation);
    //  END; {WHILE}

      { Write out the description if we've found something to describe }
      WriteToStatusBarPanel(StatusBarPanel1, StatusBarPanel1Str);
      StatusBarPanel1Str := '';

      IF StatusBarPanel2Str <> '' THEN
        WriteToStatusBarPanel(StatusBarPanel2, StatusBarPanel2Str);

      IF NOT ObjectFound THEN
        IF NOT SignalDragging AND NOT EndOfLineDragging AND NOT WholeLineDragging AND NOT EditMode AND NOT CreateLineMode AND (Screen.Cursor <> crDefault) THEN
          ChangeCursor(crDefault);

      { Remove any duplicates from LinesFoundArray - testing shows that they occasionally appear if the cursor is just in a particular place }
      DuplicateLineFound := False;
      I := 0;
      J := 1;
      WHILE (I <= High(LineFoundArray)) AND NOT DuplicateLineFound DO BEGIN
        WHILE (J <= High(LineFoundArray)) AND NOT DuplicateLineFound DO BEGIN
          IF I <> J THEN BEGIN
            IF LineFoundArray[I] = LineFoundArray[J] THEN
              DeleteElementFromIntegerArray(LineFoundArray, I);
          END;
          Inc(J);
        END; {WHILE}
        Inc(I);
      END; {WHILE}
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WhatIsUnderMouse: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WhatIsUnderMouseMainProc }

PROCEDURE WhatIsUnderMouse(ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState); Overload;
{ Returns the current mouse position }
VAR
  BufferStopFoundNum : Integer;
  IndicatorFoundNum : Integer;
  IndicatorFoundType : JunctionIndicatorType;
  PointFoundNum : Integer;
  SignalFoundNum : Integer;
  SignalPostFoundNum : Integer;
  TheatreIndicatorFoundNum : Integer;
  TRSPlungerFoundLocation : Integer;

BEGIN
  WhatIsUnderMouseMainProc(ScreenClickPosX, ScreenClickPosY, ShiftState, BufferStopFoundNum, IndicatorFoundNum, IndicatorFoundType, PointFoundNum, SignalFoundNum,
                           SignalPostFoundNum, TheatreIndicatorFoundNum, TRSPlungerFoundLocation);
END; { WhatIsUnderMouse }

PROCEDURE WhatIsUnderMouse(ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState; OUT BufferStopFoundNum : Integer; OUT IndicatorFoundNum : Integer;
                           OUT IndicatorFoundType : JunctionIndicatorType; OUT PointFoundNum : Integer; OUT SignalFoundNum : Integer; OUT SignalPostFoundNum : Integer;
                           OUT TheatreIndicatorFoundNum : Integer; OUT TRSPlungerFoundLocation : Integer); Overload;
{ Returns the current mouse position and whether a specific item has been found at that position without a keypress being required }
BEGIN
  WhatIsUnderMouseMainProc(ScreenClickPosX, ScreenClickPosY, ShiftState, BufferStopFoundNum, IndicatorFoundNum, IndicatorFoundType, PointFoundNum, SignalFoundNum,
                           SignalPostFoundNum, TheatreIndicatorFoundNum, TRSPlungerFoundLocation);
END; { WhatIsUnderMouse }

PROCEDURE TCuneoWindow.SignalPostFlashingTimerTick(Sender: TObject);
{ This timer is used to flash the route starting signal post while route setting is being done by the user }
BEGIN
  IF Routes_RouteSettingByHand OR Routes_TheatreIndicatorSettingInitiated OR Routes_NearestSignalTestingInitiated THEN BEGIN
    IF SignalPostDrawn THEN BEGIN
      Signals[SignalPostToBeFlashed].Signal_PostColour := SignalPostColour;
      DrawSignalPost(SignalPostToBeFlashed);
      SignalPostDrawn := False;
      InvalidateScreen(UnitRef, 'CuneoTimerTick');
    END ELSE BEGIN
      IF Routes_RouteSettingByHand THEN BEGIN
        IF Routes_RouteSettingByEmergencyRoute THEN
          Signals[SignalPostToBeFlashed].Signal_PostColour := SignalPostEmergencyRouteSettingColour
        ELSE
          Signals[SignalPostToBeFlashed].Signal_PostColour := SignalPostRouteSettingColour;
      END ELSE
        IF Routes_TheatreIndicatorSettingInitiated THEN
          Signals[SignalPostToBeFlashed].Signal_PostColour := SignalPostTheatreSettingColour
        ELSE
          IF Routes_NearestSignalTestingInitiated THEN
            Signals[SignalPostToBeFlashed].Signal_PostColour := clRed;

      DrawSignalPost(SignalPostToBeFlashed);
      SignalPostDrawn := True;
      InvalidateScreen(UnitRef, 'SignalPostFlashingTimerTick');
    END;
  END;
END; { SignalPostFlashingTimerTick }

PROCEDURE ChangeStateOfWhatIsUnderMouse(ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState; HelpRequired : Boolean);
{ See what the mouse is currently pointing at something, and change its state if appropriate }
CONST
  Amendment = True;
  IrrelevantLine = UnknownLine;
  IrrelevantLocation = UnknownLocation;
  IrrelevantNum : Integer = 0;
  StartButton = True;
  Undo = True;

VAR
  BufferStopFoundNum : Integer;
  CursorXY : TPoint;
  GridX : Integer;
  GridY : Integer;
  IndicatorFoundNum : Integer;
  IndicatorFoundType : JunctionIndicatorType;
  IrrelevantShiftState : TShiftState;
  PointFoundNum : Integer;
  PointChangedOK : Boolean;
  SignalFoundNum : Integer;
  SignalPostFoundNum : Integer;
  TheatreIndicatorFoundNum : Integer;
  TRSPlungerFoundLocation : Integer;

  PROCEDURE UpLineEndCharacterSelected(Line : Integer; HelpRequired : Boolean);
  { Find the corresponding line end }
  VAR
    ConnectionChFound : Boolean;
    Line2 : Integer;

  BEGIN
    IF HelpRequired THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '');
      AddRichLine(HelpWindow.HelpRichEdit, '<B>Up End Line Characters</B>');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Left Mouse</B> - indicate where the other end of the line is');
    END ELSE BEGIN
      ConnectionChFound := False;
      Line2 := 0;
      WHILE (Line2 <= High(Lines)) AND NOT ConnectionChFound DO BEGIN
        IF Lines[Line2].Line_DownConnectionCh = Lines[Line].Line_UpConnectionCh THEN BEGIN
          ConnectionChFound := True;
          Lines[Line2].Line_DownConnectionChBold := True;
          DrawConnectionCh(Line2, Down);
        END;
        Inc(Line2);
      END; {WHILE}
    END;
  END; { UpLineEndCharacterSelected }

  PROCEDURE DownLineEndCharacterSelected(Line : Integer; HelpRequired : Boolean);
  { Find the corresponding line end }
  VAR
    ConnectionChFound : Boolean;
    Line2 : Integer;

  BEGIN
    IF HelpRequired THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '');
      AddRichLine(HelpWindow.HelpRichEdit, '<B>Down End Line Characters</B>');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Left Mouse</B> - indicate where the other end of the line is');
    END ELSE BEGIN
      { Find the corresponding line end }
      ConnectionChFound := False;
      Line2 := 0;
      WHILE (Line2 <= High(Lines)) AND NOT ConnectionChFound DO BEGIN
        IF Lines[Line2].Line_UpConnectionCh = Lines[Line].Line_DownConnectionCh THEN BEGIN
          ConnectionChFound := True;
          Lines[Line2].Line_UpConnectionChBold := True;
          DrawConnectionCh(Line2, Up);
        END;
        Inc(Line2);
      END; {WHILE}
    END;
  END; { DownLineEndCharacterSelected }

  PROCEDURE ChangeSignal(S : Integer; ShiftState : TShiftState; HelpRequired : Boolean);
  { Change signal aspects }
  CONST
    NoActionCh = '';
    NoTrainRecord = 0;

  VAR
    I : Integer;
    OK : Boolean;
    SaveLockingMode : Boolean;
    RouteLockingArray : IntegerArrayType;

  BEGIN
    SaveLockingMode := LockingMode;

    IF HelpRequired THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '');
      AddRichLine(HelpWindow.HelpRichEdit, '<B>Signals</B>');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Left Mouse</B> - change signal');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Alt + Left Mouse</B> - remove route locking from signal');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Shift + Left Mouse</B> - force signal to change even if locked');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Ctrl + Left Mouse</B> - change a signal''s hidden station signal aspect');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Ctrl + Left Mouse</B> - set hidden station signal aspect of signal to red');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Ctrl + Alt + Left Mouse</B> - reset signal failure message');
    END ELSE
      IF Routes_RouteSettingByHand THEN
        Debug('!Cannot change S=' + IntToStr(S) + ' while route setting')
      ELSE
        IF Signals[S].Signal_OutOfUse THEN BEGIN
          { Can't route if the signal that is out of use }
          Log('R! User cannot change signal S=' + IntToStr(S) + ' as it is out of use');
          Exit;
        END ELSE
          IF (ssCtrl IN ShiftState) AND (ssAlt IN ShiftState) THEN BEGIN
            { allow the signal failure message to be written even if it's been previously turned off }
            Debug('S=' + IntToStr(S) + ': allowing fail message');
            IF Signals[S].Signal_FailMsgWritten THEN BEGIN
              Log('S S=' + IntToStr(S) + ': allowing fail message');
              Signals[S].Signal_FailMsgWritten := False;
            END;
          END ELSE
            IF ssShift IN ShiftState THEN BEGIN
              { Force the signal to move even if it's locked }
              SetMode(Locking, TurnOff);
              Log('SG Locking mode suspended by user when changing S=' + IntToStr(S) + ' {BLANKLINEBEFORE}');
              PullSignal(UnknownLocoChipStr, S, NoIndicatorLit, NoRoute, NoSubRoute, UnknownLine, UnknownTrainType, ByUser, OK);
            END ELSE
              IF ssAlt IN ShiftState THEN BEGIN
                { Remove route locking from a signal }
                SetMode(Locking, TurnOff);
                Debug('S=' + IntToStr(S) + ': removing locking');
                IF SignalIsLockedByAnyRoute(S, RouteLockingArray) THEN BEGIN
                  FOR I := 0 TO High(RouteLockingArray) DO BEGIN
                    Log('SG S=' + IntToStr(S) + ': R=' + IntToStr(RouteLockingArray[I]) + ' locking removed by user');
                    UnlockSignalLockedBySpecificRoute(S, RouteLockingArray[I]);
                  END;
                END;
              END ELSE
                IF ssCtrl IN ShiftState THEN BEGIN
                  { Change the hidden station signal aspect }
                  IF Signals[S].Signal_HiddenStationSignalAspect = NoAspect THEN BEGIN
                    SetHiddenStationSignalAspectSignals(NoTrainRecord, S, UnknownJourney, UnknownRoute);
                    Log('SG User setting S=' + IntToStr(S) + ' hidden station aspect to Red');
                    Signals[S].Signal_PostColour := clRed;
                  END ELSE BEGIN
                    ClearHiddenStationSignalAspectSignals(NoTrainRecord, S);
                    Log('SG User clearing hidden station signal aspect of S=' + IntToStr(S));
                    Signals[S].Signal_PostColour := SignalPostColour;
                  END;
                  DrawSignal(S);
                END ELSE
                  BEGIN
                    { no shift keys pressed }
                    PullSignal(UnknownLocoChipStr, S, NoIndicatorLit, NoRoute, NoSubRoute, UnknownLine, UnknownTrainType, ByUser, OK);
//                    IF TheatreSignalSetting THEN
//                      TheatreSignalSetting := False;
                  END;

    LockingMode := SaveLockingMode;
  END; { ChangeSignal }

  PROCEDURE ChangeSignalIndicator(S : Integer; J : JunctionIndicatorType; ShiftState : TShiftState; HelpRequired : Boolean);
  { Change signal route indicators }
  CONST
    NoActionCh = '';

  VAR
    IndicatorLit : IndicatorStateType;
    OK : Boolean;
    SaveLockingMode : Boolean;

  BEGIN
    IF HelpRequired THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '');
      AddRichLine(HelpWindow.HelpRichEdit, '<B>Signal Indicators</B>');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Left Mouse</B> - change signal indicator');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Shift + Left Mouse</B> - force signal indicator to change even if locked');
    END ELSE
      IF ShowSignalJunctionDestinations THEN BEGIN
        IF Signals[S].Signal_JunctionIndicators[J].JunctionIndicator_TargetSignal <> UnknownSignal THEN
          DrawSignalData(Signals[S].Signal_JunctionIndicators[J].JunctionIndicator_TargetSignal,
                         IntToStr(Signals[S].Signal_JunctionIndicators[J].JunctionIndicator_TargetSignal), clLime)
        ELSE
          IF Signals[S].Signal_JunctionIndicators[J].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
            DrawBufferStopData(Signals[S].Signal_JunctionIndicators[J].JunctionIndicator_TargetBufferStop,
                               IntToStr(Signals[S].Signal_JunctionIndicators[J].JunctionIndicator_TargetBufferStop), clLime);
      END ELSE BEGIN
        WITH Signals[S] DO BEGIN
          SaveLockingMode := LockingMode;
          IF ssShift IN ShiftState THEN BEGIN
            SetMode(Locking, TurnOff);
            Log('S Locking mode suspended when changing signal indicator ' + IntToStr(S) + ' {BLANKLINEBEFORE}');
          END;

          IF Routes_RouteSettingByHand THEN
            Debug('!Cannot change indicator while route setting')
          ELSE BEGIN
            IF (GetSignalAspect(S) <> RedAspect) AND (Signals[S].Signal_IndicatorState <> NoIndicatorLit) THEN
              { signal is off and indicator is illuminated }
              PullSignal(UnknownLocoChipStr, S, NoIndicatorLit, NoRoute, NoSubRoute, UnknownLine, UnknownTrainType, ByUser, OK)
            ELSE
              IF (Signals[S].Signal_IndicatorState <> NoIndicatorLit) THEN BEGIN
                { indicator illumination has been cancelled }
                Log('S User cancelling S=' + IntToStr(S) + ' indicator');
                SetIndicator(UnknownLocoChipStr, S, NoIndicatorLit, '', NoRoute, ByUser);
                UnlockPointsLockedBySignal(S);
                UnlockSignalLockedByUser(S);
              END ELSE
                IF Signals[S].Signal_OutOfUse THEN BEGIN
                  { Can't route if the signal that is out of use }
                  Log('R! User cannot change indicator if signal is out of use (S=' + IntToStr(S) + ')');
                  Signals[S].Signal_PostColour := SignalPostColour;
                  Exit;
                END ELSE
                  IF GetSignalAspect(S) <> RedAspect THEN BEGIN
                    Log('S! User cannot change indicator when a signal is off (S=' + IntToStr(S) +')');
                  END ELSE BEGIN
                    Signals[S].Signal_FailMsgWritten := False;
                    IndicatorLit := NoIndicatorLit;
                  
                    CASE J OF
                      UpperLeftIndicator:
                        IndicatorLit := UpperLeftIndicatorLit;
                      MiddleLeftIndicator:
                        IndicatorLit := MiddleLeftIndicatorLit;
                      LowerLeftIndicator:
                        IndicatorLit := LowerLeftIndicatorLit;
                      UpperRightIndicator:
                        IndicatorLit := UpperRightIndicatorLit;
                      MiddleRightIndicator:
                        IndicatorLit := MiddleRightIndicatorLit;
                      LowerRightIndicator:
                        IndicatorLit := LowerRightIndicatorLit;
                    END; {CASE}

                    PullSignal(UnknownLocoChipStr, S, IndicatorLit, NoRoute, NoSubRoute, UnknownLine, UnknownTrainType, ByUser, OK);
                  END;

        END;
      END; {WITH}
      LockingMode := SaveLockingMode;
    END;
  END; { ChangeSignalIndicator }

  FUNCTION ChangePoint(P : Integer; ShiftState : TShiftState; HelpRequired : Boolean) : Boolean;
  { Switch points, returning the result }
  CONST
    ErrorMessageRequired = True;

  VAR
    DebugStr : String;
    PointResultPending : Boolean;
    SaveLockingMode : Boolean;

  BEGIN
    IF HelpRequired THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '');
      AddRichLine(HelpWindow.HelpRichEdit, '<B>Points');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Left Mouse</B> - change point');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Shift + Left Mouse</B> - force point to move (if, for example, it has stuck)');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Ctrl + Left Mouse</B> - force point to move even if locked');
    END ELSE BEGIN
      IF Routes_RouteSettingByHand THEN
        Debug('!Cannot change point while route setting')
      ELSE
        IF Routes_TheatreIndicatorSettingInitiated THEN
          Debug('!Cannot change point while theatre route indicator setting')
        ELSE BEGIN
          WITH Points[P] DO BEGIN
            IF Point_PresentState = Straight THEN
              Point_RequiredState := Diverging
            ELSE
              { PresentState = Diverging or PointStateUnknown }
              Point_RequiredState := Straight;

            IF ssShift IN ShiftState THEN
              { Force the point to move }
              PullPoint(UnknownLocoChipStr, P, NoRoute, NoSubRoute, ForcePoint, ByUser, ErrorMessageRequired, PointResultPending, DebugStr, Result)
            ELSE
              IF ssCtrl IN ShiftState THEN BEGIN
                { Force the point to move even if it's locked }
                SaveLockingMode := LockingMode;
                SetMode(Locking, TurnOff);
                Log('P Locking mode suspended when changing point ' + IntToStr(P) + ' {BLANKLINEBEFORE}');
                PullPoint(UnknownLocoChipStr, P, NoRoute, NoSubRoute, ForcePoint, ByUser, ErrorMessageRequired, PointResultPending,
                          DebugStr, Result);
                LockingMode := SaveLockingMode;
              END ELSE
                { move it normally }
                PullPoint(UnknownLocoChipStr, P, NoRoute, NoSubRoute, NOT ForcePoint, ByUser, ErrorMessageRequired, PointResultPending,
                          DebugStr, Result);
          END; {WITH}
        END;
    END;
  END; { ChangePoint }

  PROCEDURE ChangeTRSPlunger(TRSPlungerFoundLocation : Integer;  HelpRequired : Boolean);
  { Deal with mouse presses on the train-ready-to-start plunger }
  CONST
    Pressed = True;

  BEGIN
    IF HelpRequired THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '');
      AddRichLine(HelpWindow.HelpRichEdit, '<B>TRS Plungers</B>');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Left Mouse</B> - press a plunger');
    END ELSE BEGIN
      IF NOT StationStartMode THEN
        Debug('!Not in Station Start Mode')
      ELSE BEGIN
//        IF NOT MainPlatformPlungers[TRSPlungerFoundLocation].TRSPlunger_Locked THEN BEGIN
//          MainPlatformPlungers[TRSPlungerFoundLocation].TRSPlunger_Pressed := True;
//          Log('A Received plunger data: plunger for ' + LocationToStr(TRSPlungerFoundLocation) + ' has been pressed');
//          DrawTRSPlunger(TRSPlungerFoundLocation, Pressed);
//        END;
      END;
    END;
  END; { ChangeTRSPlunger }

//  FUNCTION ActivateTrackCircuit(L : Integer; HelpRequired : Boolean) : Boolean;
//  { This activates or de-activates a track circuit. More fancy things are done via a pop menu produced by a shift key. }
//  VAR
//    AdjacentTrackCircuitUp, AdjacentTrackCircuitDown : Integer;
//    T : TrainIndex;
//
//  BEGIN
//    Result := False;
//    WITH Lines[Line] DO BEGIN
//      IF Line_TC <> UnknownTrackCircuit THEN BEGIN
//        IF ShowAdjacentTrackCircuitMode THEN BEGIN
//          { Draw a track circuit and its adjoining track circuits }
//          FindAdjoiningTrackCircuits(Lines[Line].Line_TC, AdjacentTrackCircuitUp, AdjacentTrackCircuitDown);
//          IF TCAdjoiningTCsDrawnNum = UnknownTrackCircuit THEN BEGIN
//            { one hasn't been drawn before }
//            DrawTrackCircuitsWithAdjoiningTrackCircuits(Lines[Line].Line_TC, clRed, clYellow);
//            TCAdjoiningTCsDrawnNum := Lines[Line].Line_TC;
//          END ELSE BEGIN
//            { We've previously drawn one, so undraw that one before drawing the new one }
//            DrawTrackCircuitsWithAdjoiningTrackCircuits(TCAdjoiningTCsDrawnNum, ForegroundColour, ForegroundColour);
//            IF TCAdjoiningTCsDrawnNum = Lines[Line].Line_TC THEN
//              TCAdjoiningTCsDrawnNum := UnknownTrackCircuit
//            ELSE BEGIN
//              DrawTrackCircuitsWithAdjoiningTrackCircuits(Lines[Line].Line_TC, clRed, clYellow);
//              TCAdjoiningTCsDrawnNum := Lines[Line].Line_TC;
//            END;
//          END;
//        END ELSE
//          { now consider its current state - if in user-set state or mystery state }
//          IF (TrackCircuitStateIsPermanentlyOccupied(TrackCircuits[Line_TC].TC_OccupationState))
//          AND NOT (TrackCircuitStateIsPermanentlyOccupied(TrackCircuits[Line_TC].TC_PreviousOccupationState))
//          THEN BEGIN
//            { set it to its previous state, and previous loco chip number if any }
//            IF (TrackCircuits[Line_TC].TC_PreviousOccupationState <> TCUnoccupied) THEN
//              SetTrackCircuitState(TrackCircuits[Line_TC].TC_PreviousLocoChip, Line_TC, TrackCircuits[Line_TC].TC_PreviousOccupationState)
//            ELSE BEGIN
//              IF TrackCircuits[Line_TC].TC_LocoChip <> UnknownLocoChip THEN BEGIN
//                Debug('Locochip ' + LocoChipToStr(TrackCircuits[Line_TC].TC_LocoChip) + ' cleared from TC=' + IntToStr(Line_TC));
//                T := GetTrainIndex(TrackCircuits[Line_TC].TC_LocoChip);
//                IF T <= High(Trains) THEN
//                  T^.Train_SavedLocation := UnknownLocation;
//              END;
//              SetTrackCircuitState(Line_TC, TrackCircuits[Line_TC].TC_PreviousOccupationState);
//            END;
//          END ELSE
//            { if it's marked as occupied }
//            IF TrackCircuitStateIsTemporarilyOccupied(TrackCircuits[Line_TC].TC_OccupationState) THEN BEGIN
//              Result := True;
//              { then set it to system occupied or unoccupied }
//              Log('T User setting TC=' + IntToStr(Line_TC) + ' = off');
//              SetTrackCircuitState(Line_TC, TCUnoccupied);
//              TrackCircuits[Line_TC].TC_LocoChip := UnknownLocoChip;
//            END ELSE BEGIN
//              { mark it occupied }
//              Log('T User setting TC=' + IntToStr(Line_TC) + ' = on');
//              SetTrackCircuitState(Line_TC, TCFeedbackOccupation);
//            END;
//        { Debug('TC=' + IntToStr(Line_TC) + ', Ln=' + LineNameToString(LineNameFound)); }
//      END;
//    END; {WITH}
//  END; { ActivateTrackCircuit }
//
  FUNCTION NotInArray(StringArray : StringArrayType; Str : String) : Boolean;
  { Returns true if the given element is not in the array }
  VAR
    I : Integer;
  BEGIN
    Result := True;
    I := 0;
    WHILE I <= High(StringArray) DO BEGIN
      IF Str = StringArray[I] THEN
        Result := False;
      Inc(I);
    END; {WHILE}
  END; { NotInArray }

  FUNCTION SignalPostSelected(S, BS : Integer; ShiftState : TShiftState; HelpRequired : Boolean) : Boolean;
  { Select the start or end of a route, or set a theatre indicator, or press a station start plunger }
  CONST
    IncludeOutOfUseLinesOn = True;

  VAR
    DebugStr : String;
    DraftRouteArray, LockingArray, RouteArray : StringArrayType;
    ErrorMsg : String;
    I, SubRoute : Integer;
    LinesNotAvailableStr : String;
    OK : Boolean;
    PutativelyRouteSetting, PutativelyTheatreIndicatorSetting : Boolean;
    RouteCount : Integer;
    RouteFindingCancelled : Boolean;
    RouteFoundOK : Boolean;
    Route : Integer;
    StartLine, EndLine : Integer;
    StartSignal, EndSignal : Integer;
    TempBufferStop: Integer;
    TempSignal : Integer;
    T : TrainIndex;
    TrainLengthInInchesForRouteing : Integer;
    TrainTypeForRouteing : TypeOfTrainType;

  BEGIN
    Result := True;

    TRY
      EndLine := UnknownLine;
      EndSignal := UnknownSignal;
      PutativelyRouteSetting := False;
      PutativelyTheatreIndicatorSetting := False;
      RouteFindingCancelled := False;
      Route := UnknownRoute;
      Routes_RouteSettingByEmergencyRoute := False;
      SaveIndicatorString := '';
      StartLine := UnknownLine;
      T := 0;
      TrainLengthInInchesForRouteing := UnknownTrainLength;
      TrainTypeForRouteing := UnknownTrainType;

      IF HelpRequired THEN BEGIN
        AddRichLine(HelpWindow.HelpRichEdit, '');
        AddRichLine(HelpWindow.HelpRichEdit, '<B>Signal Posts</B>');
        AddRichLine(HelpWindow.HelpRichEdit, '  <B>Routeing</B>');
        AddRichLine(HelpWindow.HelpRichEdit, '    <B>Left Mouse</B> - start or end manual routeing');
        AddRichLine(HelpWindow.HelpRichEdit, '    <B>Shift + Left Mouse</B> - routeing for express passenger trains');
        AddRichLine(HelpWindow.HelpRichEdit, '    <B>Shift + Alt + Left Mouse</B> - emergency routeing for express passenger trains');
        AddRichLine(HelpWindow.HelpRichEdit, '    <B>Ctrl + Left Mouse</B> - routeing for passenger trains');
        AddRichLine(HelpWindow.HelpRichEdit, '    <B>Ctrl + Alt + Left Mouse</B> - routeing for express passenger trains');
        AddRichLine(HelpWindow.HelpRichEdit, '    <B>Alt + Left Mouse</B> - routeing for express freight trains');

        AddRichLine(HelpWindow.HelpRichEdit, '  <B>Route Clearing</B>');
        AddRichLine(HelpWindow.HelpRichEdit, '    <B>Left Mouse</B> - clear a route');
        AddRichLine(HelpWindow.HelpRichEdit, '    <B>Shift + Left Mouse</B> - clear a route without resetting point directions');

        AddRichLine(HelpWindow.HelpRichEdit, '  <B>Theatre indicator selection</B>');
        AddRichLine(HelpWindow.HelpRichEdit, '    <B>Right Mouse</B> - start or end signal theatre set up');
      END ELSE BEGIN
        IF (S <> UnknownSignal)
        OR (BS <> UnknownBufferStop)
        THEN BEGIN
          IF (S > 0) AND (S <> UnknownSignal) AND (Signals[S].Signal_Type = SemaphoreDistant) THEN
            { routeing cannot be done starting from a semaphore distant as trains never stop at them }
            Debug('Cannot use post as cannot route from a Semaphore Distant')
          ELSE BEGIN
            IF (S > 0) AND (S <> UnknownSignal) AND Signals[S].Signal_NotUsedForRouteing THEN
              { routeing cannot be done starting from a semaphore distant as trains never stop at them }
              Debug('Cannot use post as signal noted as not used for routeing')
            ELSE BEGIN
              IF StationStartMode AND (S > 0) AND (S <> UnknownSignal) AND Signals[S].Signal_TRSHeld THEN BEGIN
                { we can't do any route-setting or theatre setting if there's a train waiting to be released }
                Signals[S].Signal_PostColour := SignalPostStationStartModeColour;
                DrawSignalPost(S);
                Log('A S=' + IntToStr(S) + ' route-holding released');
                Signals[S].Signal_TRSReleased := True;
              END ELSE BEGIN
                { If we're in the middle of processing a route, don't start another one }
                IF NOT Routes_RouteSettingByHand AND NOT Routes_TheatreIndicatorSettingInitiated AND NOT Routes_NearestSignalTestingInitiated THEN BEGIN
                  EmergencyRouteingStored := False;

                  IF BS = UnknownBufferStop THEN BEGIN
                    IF (ssShift IN ShiftState) AND (ssAlt IN ShiftState) AND (S < 0) THEN BEGIN
                      Routes_NearestSignalTestingInitiated := True;
                      Signals[S].Signal_PostColour := clLime;
                    END ELSE BEGIN
                      RouteingByUser := True;
                      IF S > -1 THEN BEGIN
                        PutativelyRouteSetting := True;
                        Signals[Abs(S)].Signal_PostColour := SignalPostRouteSettingColour;
                      END ELSE
                        { If the signal post number is negative, it's a theatre indicator setting }
                        IF S < 0 THEN BEGIN
                          PutativelyTheatreIndicatorSetting := True;
                          S := Abs(S);
                          IF Signals[S].Signal_Indicator <> TheatreIndicator THEN BEGIN
                            Debug('Cannot set theatre indicator if signal (S=' + IntToStr(S) + ') does not have one');
                            DrawFailure(S, 'T');
                            Log('S User cannot set theatre indicator if signal does not have one (S=' + IntToStr(S) + ')');
                            Signals[S].Signal_PostColour := SignalPostColour;
                            Exit;
                          END ELSE
                            IF Signals[S].Signal_OutOfUse THEN BEGIN
                              { Can't route if the signal is out of use }
                              Log('R! User cannot set theatre indicator if signal is out of use (S=' + IntToStr(S) + ')');
                              Signals[S].Signal_PostColour := SignalPostColour;
                              Exit;
                            END ELSE
                              Signals[S].Signal_PostColour := SignalPostTheatreSettingColour;
                        END ELSE
                          IF (S = UnknownSignal) AND (BS = UnknownBufferStop) THEN BEGIN
                            ShowMessage('Signal/Buffer Stop has no number');
                            Exit;
                          END;

                      IF PutativelyRouteSetting THEN BEGIN
                        { See if we're clearing an existing route }
                        FOR RouteCount := 0 TO High(Routes_Routes) DO BEGIN
                          Route := Routes_Routes[RouteCount];
                          IF (Length(Routes_SubRouteSettingStrings[Route]) > 0)
                          AND (Routes_StartSignals[Route] <> UnknownSignal)
                          AND (Routes_SubRouteStates[Route, 0] = SubRouteSetUp)
                          THEN BEGIN
                            IF S = Routes_StartSignals[Route] THEN BEGIN
                              { If route is set up, then we're clearing it }
                              Log(LocoChipToStr(Routes_LocoChips[Route]) + ' R R=' + IntToStr(Route) + ': route clearing request from user'
                                                                         + ' received - signal post ' + IntToStr(S) + ' pressed a second time');
                              { Substitute the clearing route for the original setting up route }
                              FOR SubRoute := 0 TO (Routes_TotalSubRoutes[Route] - 1) DO BEGIN
                                IF Routes_SubRouteStates[Route, SubRoute] = SubRouteSetUp THEN BEGIN
                                  CreateClearingSubRouteArray(Route, SubRoute);
                                  Routes_SubRouteStates[Route, SubRoute] := SubRouteToBeCleared;
                                END;
                              END;

                              Signals[S].Signal_PostColour := SignalPostColour;
                              { and initiate the route clearing process }
                              Routes_RouteClearingsInProgress[Route] := True;
                              IF ssShift IN ShiftState THEN
                                Routes_RouteClearingsWithoutPointResetting[Route] := True;
                              { telling the system to start with the first subroute }
                              Routes_CurrentClearingSubRoute[Route] := 0;
                              { and leave this subroutine }
                              Exit;
                            END;
                          END;
                        END;
                      END;
                    END;
                  END;

                  { Can't start a route with a buffer stop - (where would one go?) }
                  IF BS <> UnknownBufferStop THEN BEGIN
                    Log('R! User cannot start a route with a buffer stop (BS=' + IntToStr(BS) + ')');
                    { need to exit here from the route setting up now }
                    Exit;
                  END ELSE
                    IF PutativelyRouteSetting THEN BEGIN
                      IF Signals[S].Signal_OutOfUse THEN BEGIN
                        { Can't route if the signal that is out of use }
                        Log('R! User cannot start a route with a signal that is out of use (S=' + IntToStr(S) + ')');
                        Signals[S].Signal_PostColour := SignalPostColour;
                        Exit;
                      END ELSE
                        IF GetSignalAspect(S) <> RedAspect THEN BEGIN
                          { Can't route if the signal is off }
                          Log('R! User cannot start a route with a signal that is off (S=' + IntToStr(S) + ')');
                          Signals[S].Signal_PostColour := SignalPostColour;
                          Exit;
                        END ELSE
                          IF IndicatorStateToStr(Signals[S].Signal_IndicatorState) <> 'N' THEN BEGIN
                            { Can't route if the signal is off }
                            Log('R! User cannot start a route with a signal whose indicator is off (S=' + IntToStr(S) + ')');
                            Signals[S].Signal_PostColour := SignalPostColour;
                            Exit;
                          END ELSE BEGIN
                            Routes_RouteSettingByHand := True;
                            Log('R Route setting request initiated by user - signal post ' + IntToStr(S) + ' pressed');
                          END;
                    END ELSE
                      IF PutativelyTheatreIndicatorSetting THEN BEGIN
                        IF (GetSignalAspect(S) <> RedAspect)
                        AND (Signals[S].Signal_IndicatorState = TheatreIndicatorLit)
                        THEN BEGIN
                          { pressing a theatre indicator to put the signal on }
                          PullSignal(UnknownLocoChipSTr, S, NoIndicatorLit, Route, NoSubRoute, UnknownLine, UnknownTrainType, ByUser, OK);
                          Exit;
                        END ELSE
                          IF GetSignalAspect(S) <> RedAspect THEN BEGIN
                            Log('S! User cannot set a theatre indicator if the signal is already off (S=' + IntToStr(S) + ')');
                            Exit;
                          END ELSE BEGIN
                            Log('S Theatre indicator setting request initiated by user - signal post ' + IntToStr(S) + ' pressed');
                            Routes_TheatreIndicatorSettingInitiated := True;
                            SetIndicator(UnknownLocoChipStr, S, QueryIndicatorLit, '', NoRoute, ByUser);
                            Log('S User setting theatre indicator for S=' + IntToStr(S) + ' to Query');
                          END;
                      END;

                  { Start setting up routes - see if a loco has been selected from the timetable, as that will give us data about train type, etc. }
                  IF GetTrainClickedOn = UnknownTrainIndex THEN BEGIN
                    TrainTypeForRouteing := UnknownTrainType;
                    TrainLengthInInchesForRouteing := UnknownTrainLength;
                  END ELSE BEGIN
                    TrainTypeForRouteing := Trains[GetTrainClickedOn].Train_Type;
                    TrainLengthInInchesForRouteing := Trains[GetTrainClickedOn].Train_CurrentLengthInInches;
                  END;

                  { If Shift or Ctrl are pressed, we want to amend the train type }
                  IF (ssShift IN ShiftState)
                  AND (ssAlt IN ShiftState)
                  THEN BEGIN
                    TrainTypeForRouteing := ExpressPassengerType;
                    EmergencyRouteingStored := True;
                    Debug('Emergency routeing for express passenger trains');
                  END ELSE
                    IF ssShift IN ShiftState THEN BEGIN
                      TrainTypeForRouteing := ExpressPassengerType;
                      Debug('Routeing for express passenger trains');
                    END ELSE
                      IF (ssCtrl IN ShiftState) AND (ssAlt IN ShiftState) THEN BEGIN
                        TrainTypeForRouteing := OrdinaryPassengerType;
                        EmergencyRouteingStored := True;
                        Debug('Emergency routeing for ordinary passenger trains');
                      END ELSE
                        IF ssCtrl IN ShiftState THEN BEGIN
                          TrainTypeForRouteing := OrdinaryPassengerType;
                          Debug('Routeing for ordinary passenger trains');
                        END ELSE
                          IF ssAlt IN ShiftState THEN BEGIN
                            TrainTypeForRouteing := ExpressFreightType;
                            Debug('Routeing for express freight trains');
                          END;

                  StartSignal := S;
                  IF Routes_RouteSettingByHand THEN BEGIN
                    IF TrainTypeForRouteing <> UnknownTrainType THEN
                      FindAndHighlightAllRoutes(Signals[S].Signal_AdjacentLine, Signals[S].Signal_Direction, TrainTypeForRouteing, TrainLengthInInchesForRouteing,
                                                SavePossibleRoutesArray, OK)
                    ELSE
                      FindAndHighlightAllRoutes(Signals[S].Signal_AdjacentLine, Signals[S].Signal_Direction, UnknownTrainType, TrainLengthInInchesForRouteing,
                                                SavePossibleRoutesArray, OK);
                    { and add the first signal }
                    InsertElementInStringArray(SavePossibleRoutesArray, 0, 'FS=' + IntToStr(StartSignal));
                  END ELSE
                    IF Routes_TheatreIndicatorSettingInitiated THEN BEGIN
                      FindAndHighlightNearestSignalsToTheatreIndicator(Signals[S].Signal_AdjacentLine, Signals[S].Signal_Direction, SavePossibleRoutesArray, OK);
                      InsertElementInStringArray(SavePossibleRoutesArray, 0, 'FS=' + IntToStr(StartSignal));
                    END ELSE
                      IF Routes_NearestSignalTestingInitiated THEN
                        FindNearestSignalsToGivenSignal(Signals[S].Signal_AdjacentLine, Signals[S].Signal_Direction, SavePossibleRoutesArray, OK);

                  { Enable the flashing signal post timer }
                  CuneoWindow.SignalPostFlashingTimer.Enabled := True;

                  SignalPostToBeFlashed := S;
                  DrawSignalPost(S);
                END ELSE BEGIN
                  { Either RouteSettingByHandInitiated OR TheatreIndicatorSettingInitiated OR NextJunctionSignalTestingInitiated }
                  IF Length(SavePossibleRoutesArray) > 0 THEN BEGIN
                    { S may be negative if we're theatre setting }
                    S := Abs(S);

                    { It could be that we're cancelling the route selection or theatre indication }
                    IF (SavePossibleRoutesArray[0] = 'FS=' + IntToStr(S))
                    OR ((BS = UnknownBufferStop) AND NotInArray(SavePossibleRoutesArray, 'FS=' + IntToStr(S)))
                    THEN BEGIN
                      { Cancel the route setting }
                      StartSignal := ExtractSignalFromString(SavePossibleRoutesArray[0]);
                      SignalPostToBeFlashed := UnknownSignal;

                      CuneoWindow.SignalPostFlashingTimer.Enabled := False;
                      { De-highlight the start signal post }
                      Signals[S].Signal_PostColour := SignalPostColour;
                      DrawSignalPost(S);

                      IF Routes_RouteSettingByHand THEN BEGIN
                        Log('R Route setting request cancelled by user - signal post ' + IntToStr(S) + ' pressed a second time');
                        Routes_RouteSettingByHand := False;
                        RouteFindingCancelled := True;
                      END ELSE
                        IF Routes_TheatreIndicatorSettingInitiated THEN BEGIN
                          Log('S Theatre route indicator setting request cancelled by user - signal post ' + IntToStr(S) + ' pressed a second time');
                          Routes_TheatreIndicatorSettingInitiated := False;
                          RouteFindingCancelled := True;
                          { and remove the query indication }
                          SetIndicator(UnknownLocoChipStr, S, NoIndicatorLit, '', NoRoute, ByUser);
                        END ELSE
                          IF Routes_NearestSignalTestingInitiated THEN BEGIN
                            Log('S NextJunctionSignalTest cancelled by user - signal post ' + IntToStr(S) + ' pressed a second time');
                            Routes_NearestSignalTestingInitiated := False;
                            RouteFindingCancelled := True;
                          END;

                      { and the possible end of route signal posts/buffer stops }
                      FOR I := 0 TO High(SavePossibleRoutesArray) DO BEGIN
                        IF ExtractSignalFromString(SavePossibleRoutesArray[I]) <> UnknownSignal THEN BEGIN
                          Signals[ExtractSignalFromString(SavePossibleRoutesArray[I])].Signal_PostColour := SignalPostColour;
                          DrawSignalPost(ExtractSignalFromString(SavePossibleRoutesArray[I]));
                        END ELSE
                          IF ExtractBufferStopFromString(SavePossibleRoutesArray[I]) <> UnknownBufferStop THEN
                            DrawBufferStop(ExtractBufferStopFromString(SavePossibleRoutesArray[I]), BufferStopColour);
                      END;

                      { and clear the array of possible routes }
                      SetLength(SavePossibleRoutesArray, 0);
                    END ELSE BEGIN
                      { Now we are setting up a route - selecting end of route - see which signal (or buffer stop) us and the station or fiddleyard already highlighted as a
                        possibility we've clicked on.
                      }
                      OK := False;
                      StartSignal := ExtractSignalFromString(SavePossibleRoutesArray[0]);
                      StartLine := Signals[StartSignal].Signal_AdjacentLine;
                      EndSignal := S;
                      RouteingByUser := False;

                      IF BS <> UnknownBufferStop THEN BEGIN
                        { we've chosen a buffer stop - check it's a proper selection }
                        I := 0;
                        WHILE (I < Length(SavePossibleRoutesArray)) AND NOT OK DO BEGIN
                          IF ExtractBufferStopFromString(SavePossibleRoutesArray[I]) = BS THEN
                            OK := True;
                          Inc(I);
                        END; {WHILE}
                        IF OK THEN BEGIN
                          IF Routes_RouteSettingByHand THEN BEGIN
                            Log('R Route setting request by user concluded - BS=' + IntToStr(BS) + ' pressed');
                            FindRouteFromLineAToLineB(UnknownLocoChipStr, UnknownJourney, S, Signals[StartSignal].Signal_AdjacentLine,
                                                      BufferStops[BS].BufferStop_AdjacentLine, Signals[StartSignal].Signal_Direction, TrainTypeForRouteing,
                                                      TrainLengthInInchesForRouteing, EmergencyRouteingStored, NOT IncludeOutOfUseLinesOn, DraftRouteArray,
                                                      LinesNotAvailableStr, ErrorMsg, RouteFoundOK);
                            EndLine := BufferStops[BS].BufferStop_AdjacentLine;
                          END ELSE BEGIN
                            Log('S Theatre indicator route setting request by user concluded - BS=' + IntToStr(BS) + ' pressed');
                            FindRouteFromLineAToLineB(UnknownLocoChipStr, UnknownJourney, S, Signals[StartSignal].Signal_AdjacentLine,
                                                      BufferStops[BS].BufferStop_AdjacentLine, Signals[StartSignal].Signal_Direction, TrainTypeForRouteing,
                                                      TrainLengthInInchesForRouteing, EmergencyRouteingStored, NOT IncludeOutOfUseLinesOn, DraftRouteArray,
                                                      LinesNotAvailableStr, ErrorMsg, RouteFoundOK);
                            AppendToStringArray(DraftRouteArray, 'BS=' + IntToStr(BS));
                            EndLine := BufferStops[BS].BufferStop_AdjacentLine;
                          END;
                        END;
                      END ELSE BEGIN
                        { we've chosen a signal - check it's a proper selection }
                        I := 0;
                        WHILE (I < Length(SavePossibleRoutesArray)) AND NOT OK DO BEGIN
                          IF ExtractSignalFromString(SavePossibleRoutesArray[I]) = EndSignal THEN
                            OK := True;
                          Inc(I);
                        END; {WHILE}
                        IF OK THEN BEGIN
                          IF Routes_RouteSettingByHand THEN BEGIN
                            Log('R User Route setting request concluded - signal post ' + IntToStr(S) + ' pressed');
                            FindRouteFromLineAToLineB(UnknownLocoChipStr, UnknownJourney, S, Signals[StartSignal].Signal_AdjacentLine,
                                                      Signals[EndSignal].Signal_AdjacentLine, Signals[StartSignal].Signal_Direction, TrainTypeForRouteing,
                                                      TrainLengthInInchesForRouteing, EmergencyRouteingStored, NOT IncludeOutOfUseLinesOn, DraftRouteArray,
                                                      LinesNotAvailableStr, ErrorMsg, RouteFoundOK);
                            EndLine := Signals[EndSignal].Signal_AdjacentLine;
                          END ELSE BEGIN
                            Log('S User Theatre indicator route setting request concluded'
                                   + ' - signal post ' + IntToStr(StartSignal) + ' pressed');
                            FindRouteFromLineAToLineB(UnknownLocoChipStr, UnknownJourney, S, Signals[StartSignal].Signal_AdjacentLine,
                                                      Signals[EndSignal].Signal_AdjacentLine, Signals[StartSignal].Signal_Direction, TrainTypeForRouteing,
                                                      TrainLengthInInchesForRouteing, EmergencyRouteingStored, NOT IncludeOutOfUseLinesOn, DraftRouteArray,
                                                      LinesNotAvailableStr, ErrorMsg, RouteFoundOK);
                            AppendToStringArray(DraftRouteArray, 'FS=' + IntToStr(EndSignal));
                            EndLine := Signals[EndSignal].Signal_AdjacentLine;
                          END;
                        END;
                      END;
                    END;

                    IF NOT RouteFoundOK AND NOT RouteFindingCancelled THEN BEGIN
                      DebugStr := 'Route from S=' + IntToStr(StartSignal) + ' to ';
                      IF BS = UnknownBufferStop THEN
                        DebugStr := DebugStr + 'S=' + IntToStr(EndSignal)
                      ELSE
                        DebugStr := DebugStr + 'BS=' + IntToStr(BS);
                      DebugStr := DebugStr + ' not found';

                      Log('R! ' + DebugStr + ' (' + ErrorMsg + ')');
                    END ELSE BEGIN
                      { No more route finding, so reset the train type etc. ... }
                      TrainTypeForRouteing := UnknownTrainType;

                      { ....turn off the flashing signal post... }
                      SignalPostToBeFlashed := UnknownSignal;
                      CuneoWindow.SignalPostFlashingTimer.Enabled := False;

                      { ...and de-highlight all the possible end of route buttons }
                      FOR I := 0 TO High(SavePossibleRoutesArray) DO BEGIN
                        TempSignal := ExtractSignalFromString(SavePossibleRoutesArray[I]);
                        IF TempSignal <> UnknownSignal THEN BEGIN
                          Signals[TempSignal].Signal_PostColour := SignalPostColour;
                          DrawSignalPost(TempSignal);
                        END;
                        TempBufferStop := ExtractBufferStopFromString(SavePossibleRoutesArray[I]);
                        IF TempBufferStop <> UnknownBufferStop THEN
                          DrawBufferStop(ExtractBufferStopFromString(SavePossibleRoutesArray[I]), BufferStopColour);
                      END;

                      IF Routes_TheatreIndicatorSettingInitiated THEN BEGIN
                        Routes_TheatreIndicatorSettingInitiated := False;
                        PullSignal(UnknownLocoChipStr, StartSignal, TheatreIndicatorLit, Route, NoSubRoute, EndLine, TrainTypeForRouteing, ByUser, OK);
                      END ELSE
                        IF Routes_RouteSettingByHand THEN BEGIN

                          { Now set up the locking }
                          CreateLockingArrayFromDraftRouteArray(UnknownLocoChipStr, DraftRouteArray, LockingArray);

                          { Now create the route array }
                          CreateRouteArrayFromLockingArray(Routes_RouteCounter, LockingArray, RouteArray);

                          { and the other route-related arrays }
                          CreateInitialRouteRelatedArrays(T, UnknownLocoChip, RouteArray, InAutoMode, StartSignal, EndSignal, BS, StartLine, EndLine);

                          WriteStringArrayToLog(UnknownLocoChipStr, 'R', 'Draft Route Array to set up R=' + IntToStr(Routes_RouteCounter)
                                                                         + ' ' + DescribeStartAndEndOfRoute(Routes_RouteCounter) + ':',
                                                                         DraftRouteArray,
                                                                         2, 190, 'SR=');
                          WriteStringArrayToLog(UnknownLocoChipStr, 'R', 'Locking Array to set up R=' + IntToStr(Routes_RouteCounter)
                                                                         + ' ' + DescribeStartAndEndOfRoute(Routes_RouteCounter) + ':',
                                                                         LockingArray,
                                                                         2, 190, 'SR=');
                          FOR I := 0 TO (Routes_TotalSubRoutes[Routes_RouteCounter] - 1) DO
                            WriteStringArrayToLog(UnknownLocoChipStr, 'R', 'Final Route Array to set up'
                                                                           +' R=' + IntToStr(Routes_RouteCounter)
                                                                           + '/' + IntToStr(I) + ' '
                                                                           + DescribeSubRoute(Routes_RouteCounter, I) + ':',
                                                                           Routes_SubRouteSettingStrings[Routes_RouteCounter, I],
                                                                           2, 190, 'SR=');
                          { and empty the route array }
                          SetLength(SavePossibleRoutesArray, 0);
                        END;
                      END;
                    END;
                END;
              END;
            END;
          END;
        END;
      END;
    EXCEPT
      ON E : Exception DO
        Log('EG SignalPostSelected: ' + E.ClassName + ' error raised, with message: ' + E.Message);
    END; {TRY}
  END; { SignalPostSelected }

  PROCEDURE TheatreIndicatorSelected(S, BS : Integer; ShiftState : TShiftState; HelpRequired : Boolean);
  { Select a theatre indicator }
  BEGIN
    IF HelpRequired THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '');
      AddRichLine(HelpWindow.HelpRichEdit, '<B>Theatre Indicators</B>');
      AddRichLine(HelpWindow.HelpRichEdit, '  <B>Left Mouse</B> - begin setting a theatre indicator');
    END ELSE BEGIN
      { But don't allow it to be selected if it's already set }
      IF Signals[S].Signal_TheatreIndicatorString <> '' THEN BEGIN
        Debug('!Cannot set a theatre indicator on if it''s already on');
        Forbid;
      END ELSE
        { the signal number is made negative to indicate to the subroutine that it's a theatre indicator setting }
        SignalPostSelected(-S, UnknownBufferStop, ShiftState, HelpRequired);
    END;
  END; { TheatreIndicatorSelected }

VAR
  LineHandleFound : Boolean;

{ Main Procedure for ChangeStateOfWhatIsUnderMouse }
BEGIN
  TRY
    IF HelpRequired THEN BEGIN
      ChangePoint(IrrelevantNum, IrrelevantShiftState, HelpRequired);
      ChangeSignal(IrrelevantNum, IrrelevantShiftState, HelpRequired);
      ChangeSignalIndicator(IrrelevantNum, UnknownJunctionIndicator, IrrelevantShiftState, HelpRequired);
      ChangeTRSPlunger(IrrelevantLocation, HelpRequired);
      DownLineEndCharacterSelected(IrrelevantLine, HelpRequired);
      SignalPostSelected(IrrelevantNum, IrrelevantNum, IrrelevantShiftState, HelpRequired);
      TheatreIndicatorSelected(IrrelevantNum, IrrelevantNum, IrrelevantShiftState, HelpRequired);
      UpLineEndCharacterSelected(IrrelevantLine, HelpRequired);
      WriteNextLineDetailToDebugWindow(LineFoundNum, HelpRequired);
    END ELSE BEGIN
      GridX := MapScreenXToGridX(ScreenClickPosX);
      GridY := MapScreenYToGridY(ScreenClickPosY);

      WhatIsUnderMouse(ScreenClickPosX, ScreenClickPosY, ShiftState, BufferStopFoundNum, IndicatorFoundNum, IndicatorFoundType, PointFoundNum, SignalFoundNum,
                       SignalPostFoundNum, TheatreIndicatorFoundNum, TRSPlungerFoundLocation);

      { We get the FoundNum data from the WhatIsUnderMouse routine }
      IF EditMode THEN BEGIN
        IF ButtonPress = mbLeft THEN BEGIN
          IF SignalFoundNum <> UnknownSignal THEN BEGIN
            IF (EditedSignal <> UnknownSignal) AND (EditedSignal <> SignalFoundNum) THEN BEGIN
              { reset the timer here, as potentially opening the message dialogue in CheckIfEditedSignalDataHasChanged doesn't call the mouse-up event }
              CuneoWindow.MouseButtonDownTimer.Enabled := False;
              GetCursorPos(CursorXY);
              CheckIfAnyEditedDataHasChanged;
              SetCursorPos(CursorXY.X, CursorXY.Y);
            END;

            IF NOT CreateLineMode THEN
              StartSignalEdit(SignalFoundNum);
          END ELSE
            IF PointFoundNum <> UnknownPoint THEN BEGIN
              IF (EditedPoint <> UnknownPoint) AND (EditedPoint <> PointFoundNum) THEN BEGIN
                CheckIfAnyEditedDataHasChanged;
              END;

              IF NOT CreateLineMode THEN
                StartPointEdit(PointFoundNum);
            END ELSE
              IF CreateLineMode THEN BEGIN
                LineHandleFound := False;

                IF EditedLine <> UnknownLine THEN BEGIN
                  WITH Lines[EditedLine] DO BEGIN
                    Assert(Line_ShowHandles = True);

                    IF PointInPolygon(Line_UpHandlePolygon, Point(ScreenClickPosX, ScreenClickPosY)) AND NOT EndOfLineDragging THEN BEGIN
                      LineHandleFound := True;
                      EndOfLineDragging := True;
                      Line_IsBeingMovedByHandle := UpHandle;
                      DragEndOfLine(GridX, GridY, ShiftState)
                    END ELSE
                      IF PointInPolygon(Line_DownHandlePolygon, Point(ScreenClickPosX, ScreenClickPosY)) AND NOT EndOfLineDragging THEN BEGIN
                        LineHandleFound := True;
                        EndOfLineDragging := True;
                        Line_IsBeingMovedByHandle := DownHandle;
                        DragEndOfLine(GridX, GridY, ShiftState)
                      END ELSE
                        IF PointInPolygon(Line_MidHandlePolygon, Point(ScreenClickPosX, ScreenClickPosY)) AND NOT WholeLineDragging THEN BEGIN
                          LineHandleFound := True;
                          Line_IsBeingMovedByHandle := MidHandle;
                          WholeLineDragging := True;

                          { store where we are so that we can work out the offset to move it }
                          SaveGridClickPosX := GridX;
                          SaveGridClickPosY := GridY;
                          DragWholeLine(GridX, GridY);
                        END;
                  END; {WITH}
                END;

                IF NOT LineHandleFound THEN BEGIN
                  IF LineFoundNum <> UnknownLine THEN BEGIN
                    { we're not yet editing a line, so see if we're already on one }
                    WITH Lines[LineFoundNum] DO BEGIN
                      { first reset any previously-set variables }
                      DeselectLine;

                      { now set them }
                      EditedLine := LineFoundNum;
                      EditingExistingLine := True;
                      Line_ShowHandles := True;

                      IF ssShift IN ShiftState THEN
                        { split the line }
                        SplitLine(LineFoundNum, GridX, GridY);
                    END; {WITH}
                  END ELSE BEGIN
                    IF NOT EndOfLineDragging THEN BEGIN
                      IF EditedLine <> UnknownLine THEN
                        DeselectLine;
                      EndOfLineDragging := True;
                      StartLineEdit(LineFoundNum);

                      { and create a new line record }
                      SetLength(Lines, Length(Lines) + 1);
                      WITH Lines[High(Lines)] DO BEGIN
                        Line_IsTempNewLine := True;
                        Line_GridUpX := GridX;
                        Line_GridUpY := GridY;
                        Line_GridDownX := GridX;
                        Line_GridDownY := GridY;

                        Line_ShowHandles := True;
                      END; {WITH}

                      EditedLine := High(Lines);
                    END;
                  END;
                END;
              END ELSE BEGIN
                { reset the timer here, as if we haven't clicked on a signal, point, etc., we don't want dragging to be turned on }
                CuneoWindow.MouseButtonDownTimer.Enabled := False;

                IF NOT EndOfLineDragging AND NOT WholeLineDragging THEN BEGIN
                  CheckIfAnyEditedDataHasChanged;
                  ClearEditValueList;
                END;

                IF Zooming THEN
                  MoveZoomWindowMode := True;
              END;
        END ELSE
          IF ButtonPress = mbRight THEN BEGIN
            SetSignalPopupNum(UnknownLine);
            SetPointPopupNum(UnknownPoint);
            ClearLinePopupNumArray;
            SetBufferStopPopupNum(UnknownLine);

            IF SignalFoundNum <> UnknownSignal THEN BEGIN
              SetSignalPopupNum(SignalFoundNum);
              FWPRailWindow.SignalPopupMenu.Popup(MouseX, MouseY);
              SignalFoundNum := UnknownSignal;
            END ELSE
              IF PointFoundNum <> UnknownPoint THEN BEGIN
                SetPointPopupNum(PointFoundNum);
                FWPRailWindow.PointPopupMenu.Popup(MouseX, MouseY);
                PointFoundNum := UnknownPoint;
              END ELSE
                IF BufferStopFoundNum <> UnknownBufferStop THEN BEGIN
                  SetBufferStopPopupNum(BufferStopFoundNum);
                  FWPRailWindow.BufferStopPopupMenu.Popup(MouseX, MouseY);
                  BufferStopFoundNum := UnknownBufferStop;
                END ELSE BEGIN
                  { we probably want to create a line near here - we don't need to be on one to create it }
                  IF LineFoundNum <> UnknownLine THEN
                    SetLinePopupNumArray(LineFoundArray);
                  FWPRailWindow.LinePopupMenu.Popup(MouseX, MouseY);
                  LineFoundNum := UnknownLine;
                END;
          END;
      END ELSE BEGIN
        { Not Edit Mode }
        IF ButtonPress = mbLeft THEN BEGIN
          IF SignalFoundNum <> UnknownSignal THEN
            ChangeSignal(SignalFoundNum, ShiftState, HelpRequired)
          ELSE
            IF IndicatorFoundNum <> UnknownSignal THEN
              ChangeSignalIndicator(IndicatorFoundNum, IndicatorFoundType, ShiftState, HelpRequired)
            ELSE
              IF SignalPostFoundNum <> UnknownSignal THEN
                SignalPostSelected(SignalPostFoundNum, UnknownBufferStop, ShiftState, HelpRequired)
              ELSE
                IF BufferStopFoundNum <> UnknownBufferStop THEN
                  SignalPostSelected(UnknownSignal, BufferStopFoundNum, ShiftState, HelpRequired)
                ELSE
                  IF PointFoundNum <> UnknownPoint THEN BEGIN
                    PointChangedOK := ChangePoint(PointFoundNum, ShiftState, HelpRequired);

                    { also switch the opposite cross-over point (unless Alt is pressed) }
                    IF (Points[PointFoundNum].Point_Type = CrossOverPoint) THEN BEGIN
                      IF NOT PointChangedOK THEN
                        Log('A Cannot change crossover point P=' + IntToStr(Points[PointFoundNum].Point_RelatedPoint)
                               + ' as corresponding point P=' + IntToStr(PointFoundNum) + ' failed to change')
                      ELSE BEGIN
                        IF NOT (ssShift IN ShiftState) AND NOT (ssAlt IN ShiftState) THEN
                          { this second test may seem superfluous but is needed if we're in a pause between switching LS150 points, when other mouse clicks might get
                            through and cause problems
                          }
                          IF PointFoundNum <> UnknownPoint THEN
                            ChangePoint(Points[PointFoundNum].Point_RelatedPoint, ShiftState, HelpRequired);
                      END;
                    END;
                  END ELSE
                    IF TheatreIndicatorFoundNum <> UnknownSignal THEN
                      TheatreIndicatorSelected(TheatreIndicatorFoundNum, UnknownBufferStop, ShiftState, HelpRequired)
                    ELSE
                      IF UpLineEndCharacterLine <> UnknownLine THEN
                        UpLineEndCharacterSelected(UpLineEndCharacterLine, HelpRequired)
                      ELSE
                        IF DownLineEndCharacterLine <> UnknownLine THEN
                          DownLineEndCharacterSelected(DownLineEndCharacterLine, HelpRequired)
                        ELSE
                          IF TRSPlungerFoundLocation <> UnknownLocation THEN
                            ChangeTRSPlunger(TRSPlungerFoundLocation, HelpRequired)
                          ELSE
                            IF Zooming THEN
                              MoveZoomWindowMode := True;
        END ELSE
          IF ButtonPress = mbRight THEN BEGIN
            IF SignalFoundNum <> UnknownSignal THEN BEGIN
              SetSignalPopupNum(SignalFoundNum);
              FWPRailWindow.SignalPopupMenu.Popup(MouseX, MouseY);
              SignalFoundNum := UnknownSignal;
            END ELSE
              IF SignalPostFoundNum <> UnknownSignal THEN
                { a theatre indicator request - pass the negative of the signal number }
                SignalPostSelected(-SignalPostFoundNum, UnknownBufferStop, ShiftState, HelpRequired)
              ELSE
                IF PointFoundNum <> UnknownPoint THEN BEGIN
                  SetPointPopupNum(PointFoundNum);
                  FWPRailWindow.PointPopupMenu.Popup(MouseX, MouseY);
                  PointFoundNum := UnknownPoint;
                END ELSE
                  IF BufferStopFoundNum <> UnknownBufferStop THEN BEGIN
                    SetBufferStopPopupNum(BufferStopFoundNum);
                    FWPRailWindow.BufferStopPopupMenu.Popup(MouseX, MouseY);
                    BufferStopFoundNum := UnknownBufferStop;
                  END ELSE
                    IF TheatreIndicatorFoundNum <> UnknownSignal THEN
                      SignalPostSelected(-TheatreIndicatorFoundNum, UnknownBufferStop, ShiftState, HelpRequired)
                    ELSE
                      IF LineFoundNum <> UnknownLine THEN BEGIN
                        IF ssShift IN ShiftState THEN
                          WriteNextLineDetailToDebugWindow(LineFoundNum, HelpRequired)
                        ELSE BEGIN
                          SetLinePopupNumArray(LineFoundArray);
                          FWPRailWindow.LinePopupMenu.Popup(MouseX, MouseY);
                          LineFoundNum := UnknownLine;
                        END;
                      END ELSE
                        FWPRailWindow.PopupMenu.Popup(MouseX, MouseY);
          END;
      END;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG ChangeStateOfWhatIsUnderMouse: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ChangeStateOfWhatIsUnderMouse }

PROCEDURE TCuneoWindow.MouseButtonDownTimerTick(Sender: TObject);
BEGIN
  IF EditMode AND (EditedSignal <> UnknownSignal) THEN BEGIN
    { only start dragging after a short delay with the mouse held down }
    IF NOT SignalDragging THEN BEGIN
      ChangeCursor(crDrag);
      SignalDragging := True;

      WITH Signals[EditedSignal] DO BEGIN
        SaveDragX := Signal_LineX;
        SaveDragY := Signal_LineWithVerticalSpacingY;
      END; {WITH}
    END;

    CuneoWindow.MouseButtonDownTimer.Enabled := False;
  END;
END; { MouseButtonDownTimerTick }

PROCEDURE MouseButtonReleased(Button : TMouseButton; ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState);
{ Button released - only used for a few processes }
VAR
  Line : Integer;

BEGIN
  TRY
    { Reset the saved mouse position }
    SaveGridClickPosX := -1;
    SaveGridClickPosY := -1;

    IF EditMode THEN BEGIN
      { Reset the timer }
      CuneoWindow.MouseButtonDownTimer.Enabled := False;

      { Ending an actual or a potential drag and drop }
      IF SignalDragging THEN
        DropSignal;

      IF EndOfLineDragging THEN BEGIN
        EndOfLineDragging := False;
        LineDraggingComplete(ShiftState);
      END;

      IF WholeLineDragging THEN BEGIN
        WholeLineDragging := False;
        LineDraggingComplete(ShiftState);
      END;
    END;

    Line := 0;
    WHILE Line <= High(Lines) DO BEGIN
      IF Lines[Line].Line_UpConnectionChBold THEN BEGIN
        Lines[Line].Line_UpConnectionChBold := False;
        DrawConnectionCh(Line, Up);
        InvalidateScreen(UnitRef, 'MouseButtonReleased');
      END;
      Inc(Line);
    END;

    Line := 0;
    WHILE Line <= High(Lines) DO BEGIN
      IF Lines[Line].Line_DownConnectionChBold THEN BEGIN
        Lines[Line].Line_DownConnectionChBold := False;
        DrawConnectionCh(Line, Down);
        InvalidateScreen(UnitRef, 'MouseButtonReleased');
      END;
      Inc(Line);
    END;

    { Ending a zoomed screen mouse move }
    IF MoveZoomWindowMode THEN BEGIN
      MoveZoomWindowMode := False;
      ShowStatusBarAndUpDownIndications;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG MouseButtonReleased: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { MouseButtonReleased }

PROCEDURE MouseButtonPressed(Button : TMouseButton; ScreenClickPosX, ScreenClickPosY : Integer; ShiftState : TShiftState);
{ Button pressed }
CONST
  HelpRequired = True;

BEGIN
  { See if we're in the middle of an exit sequence, and, if so, abort it }
  IF EscKeyStored THEN BEGIN
    EscKeyStored := False;
    Debug('Mouse button press detected rather than Escape pressed a second time - shutdown cancelled');
  END;

  { Set the signal-dragging timer }
  CuneoWindow.MouseButtonDownTimer.Enabled := True;

  { See if the keyboard is locked }
  IF KeyBoardandMouseLocked THEN
    Debug('Mouse locked - press Shift + ''K'' to unlock ')
  ELSE BEGIN
    MouseX := ScreenClickPosX + ScrollBarXAdjustment;
    MouseY := ScreenClickPosY + ScrollBarYAdjustment;

    { Store the position of X and Y in case we decide the pan the screen by using the mouse }
    MouseMovingX := ScreenClickPosX;
    MouseMovingY := ScreenClickPosY;

    ButtonPress := Button;
    WriteToStatusBarPanel(StatusBarPanel2, '');

    { See if the loco dialogue is visible, and if a train is being controlled }
    IF LocoDialogueWindow.Visible AND (GetLocoDialogueLocoSpeed > 0) AND (Button = mbRight)
    THEN
      CheckEmergencyStop(Button, ShiftState)
    ELSE
      ChangeStateOfWhatIsUnderMouse(ScreenClickPosX, ScreenClickPosY, ShiftState, NOT HelpRequired);

    InvalidateScreen(UnitRef, 'MouseButtonPressed');
  END;
END; { MouseButtonPressed }

INITIALIZATION

END { Cuneo }.
