UNIT Main;
{ Creates the various windows and runs the timers

  Copyright © F.W. Pritchard 2015. All Rights Reserved.

  v0.1  10/04/14 Unit extracted from RailDraw
}

INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, InitVars, Vcl.ExtCtrls,
     TrackCircuitsUnit;

TYPE
  TMainWindow = CLASS(TForm)
    MainTimer: TTimer;
    WatchdogOneSecondTimer: TTimer;
    PROCEDURE MainTimerTick(Sender: TObject);
    PROCEDURE MainWindowCreate(Sender: TObject);
    PROCEDURE WatchdogOneSecondTimerTick(Sender: TObject);
  PRIVATE
    { Private declarations }
    PROCEDURE SendStringToWatchdogProgram(S : String);

  PUBLIC
    { Public declarations }
  END;

PROCEDURE InitialiseLocationLines;
{ Now we have set up the lines and signals, we can work out how they relate to locations. Some long locations (UFY for instance) will have multiple lines attached *** }

PROCEDURE InitialiseMainUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

PROCEDURE StartSystemTimer;
{ Starts the system timer only }

PROCEDURE StopSystemTimer;
{ Stops the system timer only }

PROCEDURE TurnAutoModeOff(User : Boolean);
{ Turns auto mode off }

PROCEDURE TurnAutoModeOn;
{ Turns auto mode on }

VAR
  MainWindow: TMainWindow;

IMPLEMENTATION

{$R *.dfm}

USES GetTime, RailDraw, MiscUtils, Locks, LocationData, Feedback, Options, System.StrUtils, Lenz, System.DateUtils, TestUnit, Movement, FWPShowMessageUnit, CreateRoute,
     Diagrams, Route, Replay, Startup, Cuneo, LocoUtils, StationMonitors, ProgressBar, LocoDialogue, Help, WorkingTimetable, Edit, RDCUnit, Input, Train, SyncObjs,
     Logging, SignalsUnit, PointsUnit, LinesUnit;

CONST
  ConnectedViaUSBStr = 'via USB';
  ConnectedViaEthernetStr = 'via Ethernet';
  NotConnectedStr = 'but not connected to Lenz';
  UndrawRequired = True;
  UndrawToBeAutomatic = True;
  UnitRef = 'Main';

VAR
  InMainLoop : Boolean = False;
  NumbersArrayCounter : Integer = -1;
  SaveSystemStatusEmergencyOff : Boolean;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE TMainWindow.MainWindowCreate(Sender: TObject);
CONST
  ReadWriteRegistry = True;

BEGIN
  TRY
    IF OptionsWindow = NIL THEN BEGIN
      OptionsWindow := TOptionsWindow.Create(Application);
      OptionsWindow.Update;
    END;
    IF RailDriverWindow = NIL THEN BEGIN
      RailDriverWindow := TRailDriverWindow.Create(Application);
      RailDriverWindow.Update;
    END;
    IF DebugWindow = NIL THEN BEGIN
      DebugWindow := TDebugWindow.Create(Application);
      DebugWindow.Update;
    END;
    IF DiagramsWindow = NIL THEN BEGIN
      DiagramsWindow := TDiagramsWindow.Create(Application);
      DiagramsWindow.Update;
    END;
    IF FeedbackWindow = NIL THEN BEGIN
      FeedbackWindow := TFeedbackWindow.Create(Application);
      FeedbackWindow.Update;
    END;
    IF DebuggingOptionsWindow = NIL THEN BEGIN
      DebuggingOptionsWindow := TDebuggingOptionsWindow.Create(Application);
      DebuggingOptionsWindow.Update;
    END;
    IF CuneoWindow = NIL THEN BEGIN
      CuneoWindow := TCuneoWindow.Create(Application);
      CuneoWindow.Update;
    END;
    IF LockListWindow = NIL THEN BEGIN
      LockListWindow := TLockListWindow.Create(Application);
      LockListWindow.Update;
    END;
    IF LenzWindow = NIL THEN BEGIN
      LenzWindow := TLenzWindow.Create(Application);
      LenzWindow.Update;
    END;
    IF LocoUtilsWindow = NIL THEN BEGIN
      LocoUtilsWindow := TLocoUtilsWindow.Create(Application);
      LocoUtilsWindow.Update;
    END;
    IF InitVarsWindow = NIL THEN BEGIN
      InitVarsWindow := TInitVarsWindow.Create(Application);
      InitVarsWindow.Update;
    END;
    IF ClockWindow = NIL THEN BEGIN
      ClockWindow := TClockWindow.Create(Application);
      ClockWindow.Update;
    END;
    IF MovementWindow = NIL THEN BEGIN
      MovementWindow := TMovementWindow.Create(Application);
      MovementWindow.Update;
    END;
    IF RouteWindow = NIL THEN BEGIN
      RouteWindow := TRouteWindow.Create(Application);
      RouteWindow.Update;
    END;
    IF DisplayLineColoursWindow = NIL THEN BEGIN
      DisplayLineColoursWindow := TDisplayLineColoursWindow.Create(Application);
      DisplayLineColoursWindow.Update;
      DisplayLineColoursWindow.Visible := False;
    END;
    IF TestUnitForm = NIL THEN BEGIN
      TestUnitForm := TTestUnitForm.Create(Application);
      TestUnitForm.Update;
    END;
    IF StationMonitorsWindow = NIL THEN BEGIN
      StationMonitorsWindow := TStationMonitorsWindow.Create(Application);
      StationMonitorsWindow.Update;
    END;
    IF RoutesWritingProgressBarWindow = NIL THEN BEGIN
      RoutesWritingProgressBarWindow := TRoutesWritingProgressBarWindow.Create(Application);
      RoutesWritingProgressBarWindow.Update;
    END;
    IF LocoDialogueWindow = NIL THEN BEGIN
      LocoDialogueWindow := TLocoDialogueWindow.Create(Application);
      LocoDialogueWindow.Update;
    END;
    IF HelpWindow = NIL THEN BEGIN
      HelpWindow := THelpWindow.Create(Application);
      HelpWindow.Update;
    END;
    IF LocationDataWindow = NIL THEN BEGIN
      LocationDataWindow := TLocationDataWindow.Create(Application);
      LocationDataWindow.Update;
    END;
    IF FWPShowMessageWindow = NIL THEN BEGIN
      FWPShowMessageWindow := TFWPShowMessageWindow.Create(Application);
      FWPShowMessageWindow.Update;
    END;
    IF ReplayForm = NIL THEN BEGIN
      ReplayForm := TReplayForm.Create(Application);
      ReplayForm.Update;
    END;
    IF WorkingTimetableWindow = NIL THEN BEGIN
      WorkingTimetableWindow := TWorkingTimetableWindow.Create(Application);
      WorkingTimetableWindow.Update;
    END;
    IF EditWindow = NIL THEN BEGIN
      EditWindow := TEditWindow.Create(Application);
      EditWindow.Update;
    END;
    IF InputDialogueBox = NIL THEN BEGIN
      InputDialogueBox := TInputDialogueBox.Create(Application);
      InputDialogueBox.Update;
    END;
    IF FWPRailWindow = NIL THEN BEGIN
      FWPRailWindow := TFWPRailWindow.Create(Application);
      FWPRailWindow.Visible := False;
    END;

    InitialiseStartupUnit;
    ReadIniFile;

    { Now initialise the log files }
    IF InLogsCurrentlyKeptMode THEN
      InitialiseLogFiles;

    InitialiseMainUnit;
    InitialiseInitVarsUnit;
    InitialiseOptionsUnit;
    InitialiseLocoDialogueUnit;
    InitialiseLocksUnit;
    InitialiseLocoUtilsUnit;
    InitialiseMiscUtilsUnit;
    InitialiseMovementUnit;
    InitialiseRailDrawUnit;
    InitialiseGetTimeUnit;
    InitialiseWorkingTimetableUnit;
    InitialiseEditUnit;
    InitialiseDisplayLineColoursWindow;
    InitialiseLoggingWindow;
    InitialiseLenzUnit;
    IF InRDCMode THEN
      StartRailDriver;
  EXCEPT
    ON E : Exception DO
      Log('EG MainWindowCreate:' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { MainWindowCreate }

PROCEDURE InitialiseLocationLines;
{ Now we have set up the lines and signals, we can work out how they relate to locations. Some long locations (UFY for instance) will have multiple lines attached *** }
VAR
  L : Integer;
  LineAtDownStr : String;
  LineAtUpStr : String;
  Location : Integer;
  SaveScreenDownX : Integer;
  SaveScreenUpX : Integer;

BEGIN
  TRY
    FOR Location := 0 TO High(Locations) DO BEGIN
      WITH Locations[Location] DO BEGIN
        Location_LineAtUp := UnknownLine;
        Location_LineAtDown := UnknownLine;
        SaveScreenUpX := FWPRailWindow.ClientWidth;
        SaveScreenDownX := 0;
        { Set up L to contain the lines at the ends of locations - if there is more than one line related to a location, use the one nearest to up or down }
        FOR L := 0 TO High(Lines) DO BEGIN
          WITH Lines[L] DO BEGIN
            IF Lines[L].Line_Location = Location THEN BEGIN
              IF ((GetLineAdjacentSignal(L) <> UnknownSignal)
                  AND (Signals[GetLineAdjacentSignal(L)].Signal_Direction = Up)
                  AND NOT Signals[GetLineAdjacentSignal(L)].Signal_OutOfUse)
              AND (MapGridXToScreenX(Lines[L].Line_GridUpX) < SaveScreenUpX)
              THEN BEGIN
                Location_LineAtUp := L;
                SaveScreenUpX := MapGridXToScreenX(Lines[L].Line_GridUpX);
              END ELSE
                IF ((Lines[L].Line_AdjacentBufferStop <> UnknownBufferStop) AND (GetBufferStopDirection(Lines[L].Line_AdjacentBufferStop) = Up))
                AND (BufferStops[Lines[L].Line_AdjacentBufferStop].BufferStop_X <= SaveScreenUpX)
                THEN
                  Location_LineAtUp := L
                ELSE
                  IF ((GetLineAdjacentSignal(L) <> UnknownSignal)
                      AND (Signals[GetLineAdjacentSignal(L)].Signal_Direction = Down)
                      AND NOT Signals[GetLineAdjacentSignal(L)].Signal_OutOfUse)
                  AND (MapGridXToScreenX(Lines[L].Line_GridDownX) > SaveScreenDownX)
                  THEN BEGIN
                    Location_LineAtDown := L;
                    SaveScreenDownX := MapGridXToScreenX(Lines[L].Line_GridDownX);
                  END ELSE
                    IF ((Lines[L].Line_AdjacentBufferStop <> UnknownBufferStop) AND (GetBufferStopDirection(Lines[L].Line_AdjacentBufferStop) = Down))
                    { remember that BufferStops array is dynamic and starts at zero }
                    AND (BufferStops[Lines[L].Line_AdjacentBufferStop].BufferStop_X >= SaveScreenDownX)
                    THEN
                      Location_LineAtDown := L;

            END; {WITH}
          END;
        END;

        { Now record whether platforms have other platforms up or down from them - this is needed as otherwise, for example, the routeing will attempt to route up trains
          from the down lines into the furthest up platform (PlatformA) even if the adjacent and nearer platform (PlatformB) is already occupied.
        }
        Location_PlatformOrFiddleyardAtUp := UnknownLocation;
        Location_PlatformOrFiddleyardAtDown := UnknownLocation;

        IF Location_IsPlatform THEN BEGIN
          IF Location_LineAtUp = UnknownLine THEN BEGIN
            LineAtUpStr := UnknownLineStr;
            Location_PlatformOrFiddleyardAtUp := UnknownLocation;
            IF Location_DirectionPriority <> DownOnly THEN
              Log('E Line up of ' + LocationToStr(Location, ShortStringType) + ' is unknown (is there an adjacent signal missing?)');
          END ELSE BEGIN
            LineAtUpStr := LineToStr(Location_LineAtUp);
            IF Lines[Location_LineAtUp].Line_NextUpLine = UnknownLine THEN
              Location_PlatformOrFiddleyardAtUp := UnknownLocation
            ELSE
              IF (Lines[Location_LineAtUp].Line_NextUpLine <> UnknownLine)
              AND (Lines[Lines[Location_LineAtUp].Line_NextUpLine].Line_Location <> UnknownLocation)
              AND (Lines[Lines[Location_LineAtUp].Line_NextUpLine].Line_Location <> Location)
              AND (Locations[Lines[Lines[Location_LineAtUp].Line_NextUpLine].Line_Location].Location_IsPlatform)
              THEN
                Location_PlatformOrFiddleyardAtUp := Lines[Lines[Location_LineAtUp].Line_NextUpLine].Line_Location;
          END;
        END;

        IF Location_IsPlatform THEN BEGIN
          IF Location_LineAtDown = UnknownLine THEN BEGIN
            LineAtDownStr := UnknownLineStr;
            Location_PlatformOrFiddleyardAtDown := UnknownLocation;
            IF Location_DirectionPriority <> UpOnly THEN
              Log('E Line down of ' + LocationToStr(Location, ShortStringType) + ' is unknown (is there an adjacent signal missing?)');
          END ELSE BEGIN
            LineAtDownStr := LineToStr(Location_LineAtDown);
            IF Lines[Location_LineAtDown].Line_NextDownLine = UnknownLine THEN
              Location_PlatformOrFiddleyardAtDown := UnknownLocation
            ELSE
              IF (Lines[Location_LineAtDown].Line_NextDownLine <> UnknownLine)
              AND (Lines[Lines[Location_LineAtDown].Line_NextDownLine].Line_Location <> UnknownLocation)
              AND (Lines[Lines[Location_LineAtDown].Line_NextDownLine].Line_Location <> Location)
              AND (Locations[Lines[Lines[Location_LineAtDown].Line_NextDownLine].Line_Location].Location_IsPlatform)
              THEN
                Location_PlatformOrFiddleyardAtDown := Lines[Lines[Location_LineAtDown].Line_NextDownLine].Line_Location;
          END;
        END;

        { Now record if locations are cul-de-sacs }
        IF (Locations[Location].Location_LineAtUp <> UnknownLine) AND (Lines[Locations[Location].Location_LineAtUp].Line_NextUpIsEndOfLine <> NotEndOfLine) THEN
          Locations[Location].Location_LineAtUpIsEndOfLine := True
        ELSE
          Locations[Location].Location_LineAtUpIsEndOfLine := False;

        IF (Locations[Location].Location_LineAtDown <> UnknownLine) AND (Lines[Locations[Location].Location_LineAtDown].Line_NextDownIsEndOfLine <> NotEndOfLine) THEN
          Locations[Location].Location_LineAtDownIsEndOfLine := True
        ELSE
          Locations[Location].Location_LineAtDownIsEndOfLine := False;

        IF InDebuggingMode THEN
          Log('* Location: ' + LocationToStr(Location, ShortStringType)
                 + IfThen(Location_LineAtUp <> UnknownLine,
                          IfThen(Locations[Location].Location_LineAtUpIsEndOfLine,
                                 ' - End of line at Up',
                                 ' - Line at Up is ' + Lines[Location_LineAtUp].Line_NameStr))
                 + IfThen(Location_LineAtUp <> UnknownLine,
                          IfThen(Locations[Location].Location_LineAtDownIsEndOfLine,
                                 ' - End of line at Down',
                                 ' - Line at Down is ' + Lines[Location_LineAtDown].Line_NameStr))
                 + IfThen(Location_PlatformOrFiddleyardAtUp <> UnknownLocation,
                          ' - Location at Up is ' + LocationToStr(Location_PlatformOrFiddleyardAtUp))
                 + IfThen(Location_PlatformOrFiddleyardAtDown <> UnknownLocation,
                          ' - Location at Down is ' + LocationToStr(Location_PlatformOrFiddleyardAtDown)));
      END; {WITH}
    END; {FOR}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG InitialiseLocationLines: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { InitialiseLocationLines }

PROCEDURE StartSystemTimer;
{ Starts the system timer only }
BEGIN
  MainWindow.MainTimer.Enabled := True;
END; { StartSystemTimer }

PROCEDURE StopSystemTimer;
{ Stops the system timer only }
BEGIN
  MainWindow.MainTimer.Enabled := False;
END; { StopSystemTimer }

PROCEDURE TurnAutoModeOff(User : Boolean);
{ Turns auto mode off }
CONST
  TrainListOnly = True;

VAR
  DebugStr : String;
  R : Integer;

BEGIN
  TRY
    InAutoMode := False;

    { Stop the timers }
    IF ClockWindow <> NIL THEN
      ClockWindow.GetTimeTimer.Enabled := False;

    { If we press the Stop key, stop current routeing too }
    R := 0;
    IF Routes_RouteCounter > -1 THEN BEGIN
      WHILE R <= Routes_RouteCounter DO BEGIN
        IF Routes_RouteSettingsInProgress[R] THEN
          RouteingSuspendedWhenStopPressed := True;
        Inc(R);
      END; {WHILE}
    END;

    SaveSignalsCurrentState;

    IF NOT SystemOnline THEN
      Log('AG Auto mode turned off - clock stopped')
    ELSE BEGIN
      StopLocos('auto mode turn off');
      IF User THEN
        DebugStr := 'Auto mode turned off by user'
      ELSE
        DebugStr := 'Auto mode turned off by system';
      DebugStr := DebugStr + ' - clock stopped, and all locos stopped';
      Log('AG ' + DebugStr);
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG TurnAutoModeOff:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TurnAutoModeOff }

PROCEDURE TurnAutoModeOn;
{ Turns auto mode on }
BEGIN
  TRY
    AutoModeInitiated := True;

    { Allow routeing to continue }
    RouteingSuspendedWhenStopPressed := False;
    InAutoMode := True;

    IF NOT Restart AND InLogsCurrentlyKeptMode THEN BEGIN
      { First a replay command - this doesn't use Log, as we don;t want the time etc. }
      WriteLn(LargeLogFile, '{Replay Write}');

      Log('AG AutoOn Button pressed - clock started')
    END ELSE BEGIN
      IF SystemOnline THEN BEGIN
        Log('AG AutoOn Button pressed - clock restarted')
      END ELSE BEGIN
        StartLocos(Restart);
        Log('AG AutoOn Button pressed - clock restarted, and all locos restarted');
      END;

      RestoreAllSignalsToPreviousState;
      Log('A All signals now set to previous state');
    END;
    LocosStopped := False;
    IF Restart THEN
      IF SystemOnline THEN
        StartLocos(Restart);
    Restart := True;
    RouteClearingOnlyMode := False;
    { and restart the auto clock }
    ClockWindow.GetTimeTimer.Enabled := True;
    { and the system timer }
    StartSystemTimer;
  EXCEPT
    ON E : Exception DO
      Log('EG TurnAutoModeOn:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TurnAutoModeOn }

PROCEDURE CheckPointsAwaitingFeedback;
{ See if any point changes are pending - i.e. we're waiting for feedback that confirms the change }
CONST
  ErrorMessageRequired = True;

VAR
  DebugStr : String;
  LocoChipStr : String;
  OK : Boolean;
  P : Integer;
  PointFeedbackWaitInSeconds : Double;
  PointResultPending : Boolean;
  RouteFound : Boolean;
  Route : Integer;
  SubRoute : Integer;
  T : TrainIndex;
  TempDivergingLineStr : String;

BEGIN
  TRY
    IF NOT ProgramStarting THEN BEGIN
      FOR P := 0 TO High(Points) DO BEGIN
        WITH Points[P] DO BEGIN
          LocoChipStr := LocoChipToStr(Point_LocoChipLockingTheRoute);
          IF Point_FeedbackPending THEN BEGIN
            IF Point_PresentState = Point_RequiredState THEN BEGIN
              IF Point_ManualOperation THEN BEGIN
                Point_FeedbackPending := False;
                Point_AwaitingManualChange := False;
                Log(LocoChipStr + ' P P=' + IntToStr(P) + ' pending change to ' + PointStateToStr(Point_RequiredState) + ' was successful');
                IF FWPShowMessageWindow.Visible THEN
                  FWPShowMessageWindow.Visible := False;
              END ELSE BEGIN
                { No need to draw it, as the change will have made the system draw it }
                DebugStr := 'P=' + IntToStr(P) + ' pending change to ';
                DebugStr := DebugStr + PointStateToStr(Point_RequiredState) + ' was successful after a '
                                     + FloatToStr(MilliSecondsBetween(Time, Point_FeedbackStartTime) / 1000) + ' second wait';
                Log(LocoChipStr + ' P ' + DebugStr);
                Point_FeedbackPending := False;
                Point_FeedbackPendingMsgWritten := False;
              END;

              IF Point_RequiredState = Straight THEN
                Log(LocoChipStr + ' P P=' + IntToStr(P) + ' D=S')
              ELSE
                Log(LocoChipStr + ' P P=' + IntToStr(P) + ' D=D');

            END ELSE BEGIN
              IF NOT Point_ManualOperation THEN BEGIN
                { PresentState <> RequiredState: a five second wait should be sufficient }
                PointFeedbackWaitInSeconds := Round(SecondSpan(Time, Point_FeedbackStartTime));
                IF PointFeedbackWaitInSeconds >= PointFeedbackMaximumWaitInSeconds THEN BEGIN
                  DebugStr := DebugStr + 'P=' + IntToStr(P) + ' pending change to ' + PointStateToStr(Point_RequiredState)
                                                              + ' failed after a ' + FloatToStr(PointFeedbackWaitInSeconds) + ' second wait';
                  Point_FeedbackPending := False;
                  Point_FeedbackPendingMsgWritten := False;
                  Log(LocoChipStr + ' P ' + DebugStr);

                  IF NOT Point_SecondAttempt THEN BEGIN
                    { have one more attempt at making it switch }
                    Point_SecondAttempt := True;
                    Log(LocoChipStr + ' P Second attempt to switch P=' + IntToStr(P));
                    PullPoint(LocoChipToStr(Point_LocoChipLockingTheRoute), P, NoRoute, NoSubRoute, NOT ForcePoint, ByUser, NOT ErrorMessageRequired, PointResultPending,
                              DebugStr, OK);
                    Exit;
                  END;

                  { Illustrate where the offending turnout is; note: blue is the new yellow here (!) - as clBlue comes out as pmNotXored clYellow }
                  DrawOutline(Point_MouseRect, clBlue, UndrawRequired, NOT UndrawToBeAutomatic);
                  Point_MaybeBeingSetToManual := True;

                  IF (Points[P].Point_Type <> CatchPointUp) AND (Points[P].Point_Type <> CatchPointDown) THEN
                    { we need to do this as an IfThen clause seems to evaluate the whole expression and we therefore get a range check error when a catch point does not
                      have a diverging line
                    }
                    TempDivergingLineStr := ' ' + Lines[Points[P].Point_DivergingLine].Line_NameStr
                  ELSE
                    TempDivergingLineStr := '';

                  { and produce a pop up message for the user, as this is important }
                  MakeSound(1);
                  CASE MessageDialogueWithDefault('P=' + IntToStr(P) + ' [Lenz ' + IntToStr(Points[P].Point_LenzNum)
                                                  + '] ('
                                                  + Lines[Points[P].Point_HeelLine].Line_NameStr
                                                  + ' ' + Lines[Points[P].Point_StraightLine].Line_NameStr
                                                  + TempDivergingLineStr
                                                  + ') has failed to change to ' + PointStateToStr(Point_RequiredState) + ':' + CRLF
                                                  + 'Retry, or set this point manually to ' + PointStateToStr(Point_RequiredState)
                                                  + ' and press ''Now Set'' when completed, or Retry or Ignore the problem',
                                                  NOT StopTimer, mtWarning, [mbAbort, mbOK, mbRetry],
                                                  ['&Now Set', '&Ignore', '&Retry'], mbAbort)
                  OF
                    mrOK: { Point Set }
                      BEGIN
                        Log(LocoChipStr + ' P P=' + IntToStr(P) + ' manually set to ' + PointStateToStr(Point_RequiredState));
                        Point_PreviousState := Point_PresentState;
                        Point_PresentState := Point_RequiredState;
                        InvalidateScreen(UnitRef, 'CheckPointsAwaitingFeedback');
                        IF MessageDialogueWithDefault('Set this point to manual for the rest of this session?',
                                                      NOT StopTimer, mtWarning, [mbYes, mbNo], mbYes) = mrYes
                        THEN BEGIN
                          Point_ManualOperation := True;
                          Log('P P=' + IntToStr(P) + ' set to manual operation');
                        END;
                      END;
                    mrRetry:
                      BEGIN
                        { Try forcing the point to switch the other way first - sometimes unsticks a point }
                        IF Point_RequiredState = Diverging THEN
                          Point_RequiredState := Straight
                        ELSE
                          Point_RequiredState := Diverging;
                        PullPoint(LocoChipToStr(Point_LocoChipLockingTheRoute), P, NoRoute, NoSubRoute, ForcePoint, ByUser, NOT ErrorMessageRequired,
                                                PointResultPending, DebugStr, OK);

                        IF Point_RequiredState = Diverging THEN
                          Point_RequiredState := Straight
                        ELSE
                          Point_RequiredState := Diverging;
                        PullPoint(LocoChipToStr(Point_LocoChipLockingTheRoute), P, NoRoute, NoSubRoute, {NOT} ForcePoint, ByUser, NOT ErrorMessageRequired,
                                                PointResultPending, DebugStr, OK);
                      END;
                    mrAbort: { Ignore }
                      { a major problem: cancel the route, and the train }
                      BEGIN
                        Route := 0;
                        RouteFound := False;
                        WHILE (Route <= High(Routes_Routes)) AND NOT RouteFound DO BEGIN
                          IF Routes_PointResultPendingPoint[Route] <> UnknownPoint THEN BEGIN
                            Log(LocoChipToStr(Routes_LocoChips[Route]) + ' R P=' + IntToStr(Routes_PointResultPendingPoint[Route])
                                                                       + ' pending point change and R=' + IntToStr(Route)
                                                                       + ' setting up cancelled by user');
                            Routes_RouteSettingsInProgress[Route] := False;
                            Routes_RouteClearingsInProgress[Route] := True;
                            FOR SubRoute := 0 TO High(Routes_SubRouteStates[Route]) DO BEGIN
                              CreateClearingSubRouteArray(Route, SubRoute);
                              Routes_SubRouteStates[Route, SubRoute] := SubRouteToBeCleared;
                            END;
                            IF MessageDialogueWithDefault('R=' + IntToStr(Route) + ' has been cancelled as a result of the failure'
                                                          + ' of point ' + IntToStr(Routes_PointResultPendingPoint[Route]) + ':'
                                                          + CRLF
                                                          + 'Do you want to cancel the train as well?',
                                                          NOT StopTimer, mtError, [mbYes, mbNo], ['&Cancel', '&Don''t Cancel'], mbYes) = mrYes
                            THEN BEGIN
                              IF Routes_LocoChips[Route] <> UnknownLocoChip THEN BEGIN
                                Log(LocoChipToStr(Routes_LocoChips[Route]) + ' DG System occupation and diagram entry cancelled by user');
                                { look for our train }
                                T := GetTrainIndexFromLocoChip(Routes_LocoChips[Route]);
                                IF T <> UnknownTrainIndex THEN
                                  CancelTrain(T, ByUser, TrainExists);
                              END;
                            END ELSE BEGIN

                              { may want to set the route by hand *** }

                            END;
                            Routes_PointResultPendingPoint[Route] := UnknownPoint;
                            RouteFound := True;
                          END;
                          Inc(Route);
                        END; {WHILE}
                      END;
                  END; {CASE}
                  Point_MaybeBeingSetToManual := False;
                END;
              END;
            END;
          END;
        END; {WITH}
      END; {FOR}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG CheckPointsAwaitingFeedback:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CheckPointsAwaitingFeedback }

PROCEDURE TMainWindow.MainTimerTick(Sender: TObject);
{ Runs the main loop }
CONST
  ErrorMsgRequired = True;
  ForceDisplay = True;
  LightsOn = True;
  TrackCircuitOccupation = True;

VAR
  ErrorMsg : String;
  I : Integer;
  KeyOut : Boolean;
  LockingMsg : String;
  OK : Boolean;
  PointResultPending : Boolean;
  T : TrainIndex;
  TempRoute : Integer;

  PROCEDURE CheckSystemStatus;
  { Ask for current system status and deal with abnormal states }
  VAR
    SystemStatus : LenzSystemRec;
    TC : Integer;

  BEGIN
    TRY
      SystemStatus := ReturnSystemStatus;
      IF (SystemStatus.EmergencyOff AND NOT SaveSystemStatusEmergencyOff) THEN BEGIN
        Log('E Emergency - saving track-circuit settings');
        FOR TC := 0 TO High(TrackCircuits) DO BEGIN
          TrackCircuits[TC].TC_EmergencyState := TrackCircuits[TC].TC_OccupationState;
          TrackCircuits[TC].TC_EmergencyLocoChip := TrackCircuits[TC].TC_LocoChip;
        END;
        IF SystemStatus.EmergencyOff THEN
          SaveSystemStatusEmergencyOff := True;
      END;

      IF (SystemStatus.EmergencyOff OR SystemStatus.EmergencyStop)
      AND NOT LocosStopped
      THEN BEGIN
        { And now stop all active trains }
        LocosStopped := True;
        TurnAutoModeOff(NOT ByUser);
        IF NOT EmergencyStopMsgDisplayed THEN BEGIN
          MessageDialogueWithDefault('All locos emergency stopped', StopTimer, mtInformation, [mbOK], ['&OK'], mbOK);
          EmergencyStopMsgDisplayed := True;
        END;
        Log('EG ALL LOCOS EMERGENCY STOPPED');
      END;

      IF LocosStopped
      AND NOT SystemStatus.EmergencyOff
      AND NOT SystemStatus.EmergencyStop
      THEN BEGIN
        { Emergency over - can now reload track-circuit data after a short wait, then restart trains slowly }
        IF SaveSystemStatusEmergencyOff THEN BEGIN
          SaveSystemStatusEmergencyOff := False;
          EmergencyStopMsgDisplayed := False;
          StartSystemTimer;

          { The problem is that, after a restart, the track circuit info comes in automatically - we want to check it only once it has arrived - as there is no way to tell
            when it has finished arriving, the answer seems to be to allow a five second delay.
          }
          PostEmergencyTime := IncSecond(Time, 5);
          PostEmergencyTimeSet := True;
          DrawMap;
        END;
      END;
      IF PostEmergencyTimeSet AND (Time >= PostEmergencyTime) THEN BEGIN
        { Restore the track-circuit data after an emergency (but wait a short time before doing so, as the feedback system will read in the feedback data again after a system
          reset, and we need to restore our data after that). Do we need to save and restore other TC data (like PreviousTCstate)? ****
        }
        PostEmergencyTimeSet := False;
        Log('EG Emergency over - restoring track-circuit data');
        FOR TC := 0 TO High(TrackCircuits) DO BEGIN
          IF (TrackCircuits[TC].TC_OccupationState <> TrackCircuits[TC].TC_EmergencyState)
          THEN BEGIN
            IF TrackCircuits[TC].TC_OccupationState = TCFeedbackOccupation THEN BEGIN
              IF TrackCircuits[TC].TC_EmergencyState = TCPermanentFeedbackOccupation THEN
                TrackCircuits[TC].TC_OccupationState := TCPermanentFeedbackOccupation;
            END ELSE BEGIN
              IF TrackCircuits[TC].TC_EmergencyState = TCFeedbackOccupation THEN
                { don't want to restore feedback occupation when there's no current feedback }
                Log('E Not restoring TC=' + IntToStr(TC)
                       + ' from ' + TrackCircuitStateToStr(TrackCircuits[TC].TC_OccupationState)
                       + ' to ' + TrackCircuitStateToStr(TrackCircuits[TC].TC_EmergencyState)
                       + ' as there is no current feedback there')
              ELSE BEGIN
                Log('E Restoring TC=' + IntToStr(TC)
                       + ' from ' + TrackCircuitStateToStr(TrackCircuits[TC].TC_OccupationState)
                       + ' to ' + TrackCircuitStateToStr(TrackCircuits[TC].TC_EmergencyState));
                TrackCircuits[TC].TC_OccupationState := TrackCircuits[TC].TC_EmergencyState;
              END;
            END;
          END;
          IF TrackCircuits[TC].TC_LocoChip <> TrackCircuits[TC].TC_EmergencyLocoChip THEN BEGIN
            Log('E Restoring TC=' + IntToStr(TC) + ' locochip was ' + IntToStr(TrackCircuits[TC].TC_LocoChip)
                   + '; is now ' + IntToStr(TrackCircuits[TC].TC_EmergencyLocoChip));
            TrackCircuits[TC].TC_LocoChip := TrackCircuits[TC].TC_EmergencyLocoChip;
          END;
        END;
        DrawMap;
      END;
    EXCEPT
      ON E : Exception DO
        Log('EG CheckSystemStatus:' + E.ClassName + ' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { CheckSystemStatus }

  PROCEDURE CheckStationStartMode;
  { See if station start mode is being turned on or off - press button held down for five seconds does it }
//  VAR
//    Location : Integer;

  BEGIN
    IF (StationStartModeSetUpTime <> 0) AND (Time > IncSecond(StationStartModeSetUpTime, 5)) THEN BEGIN
      StationStartModeSetUpTime := 0;
      IF InStationStartMode THEN
        SetMode(StationStart, TurnOff)
      ELSE BEGIN
        SetMode(StationStart, TurnOn);
        { and set all button presses to false }
//        FOR Location := FirstMainPlatformLocation TO LastMainPlatformLocation DO
//          MainPlatformPlungers[Location].TRSPlunger_Pressed := False;
      END;
    END;
  END; { CheckStationStartMode }

BEGIN
  TRY
    IF NOT InMainLoop THEN BEGIN
      InMainLoop := True;

//      StopSystemTimer;

      IF RunTestUnitOnStartup THEN BEGIN
        Debug('Running test unit on startup');
        TestProc(KeyOut);
        RunTestUnitOnStartup := False;
      END;

  //    { See if any rectangles need to be undrawn } { seems not to be in use (as TimeRectangleDrawn is not initialised 8/4/14 ********* }
  //    IF TimeRectangleDrawn > 0 THEN BEGIN
  //      IF ((GetTickCount - TimeRectangleDrawn) > MaxRectangleUndrawTime) THEN BEGIN
  //        DrawOutline(UndrawRect, SaveUndrawRectColour, UndrawRequired, NOT UndrawToBeAutomatic);
  //        TimeRectangleDrawn := 0;
  //      END;
  //    END;

      CheckSystemStatus;

      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 1');

      IF InAutoMode THEN
        MoveAllTrains;

      { See if any point changes are pending - i.e. we're waiting for feedback that confirms the change }
      CheckPointsAwaitingFeedback;

      { See if any trains have strayed }
      IF InAutoMode THEN
        LookOutForStrayingTrains;

      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 2');

      IF InAutoMode THEN BEGIN
        DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 3');
        MoveAllTrains;

        DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 4');
        CheckTrainsHaveArrived;

        DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 5');
        CheckTrainsHaveDeparted;

        DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 6');
        MoveAllTrains;

        DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 7');
        CheckTrainsReadyToDepart;
      END;

      { On each tick go through one of the routes currently active, and continue route setting if required }
      IF NOT RouteingSuspendedWhenStopPressed AND NOT RouteingSuspendedForModalDialogue THEN BEGIN
        IF Length(Routes_Routes) > 0 THEN BEGIN
          { Work out which route we're doing on this iteration }
          Inc(NumbersArrayCounter);
          IF NumbersArrayCounter > High(Routes_Routes) THEN
            { Reset the counter if it's too high }
            NumbersArrayCounter := 0;
          TempRoute := Routes_Routes[NumbersArrayCounter];

          { Set up routes }
          IF Routes_RouteSettingsInProgress[TempRoute] THEN
            { there is some route setting to be done }
            SetUpASubRoute(TempRoute);
  // %%%%%
          IF InAutoMode THEN BEGIN
            DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 8');
            MoveAllTrains;
          END;

          { Clear routes }
          IF InAutoMode OR RouteingByUser OR True THEN
            IF Routes_RouteClearingsInProgress[TempRoute] THEN
              { there is some route clearing to be done }
              ClearARoute(TempRoute);

    //      IF InAutoMode THEN
    //        TestClearARoute(TempRoute);

          IF InAutoMode THEN BEGIN
            DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 9');
            MoveAllTrains;
          END;

          { and set any signals needed because of approach control being in operation }
          IF Routes_ApproachControlsSet[TempRoute] THEN
            ProcessApproachLockedSignals(TempRoute);
        END;
      END;

      { And see if any points need to be reset (this is done here as sometimes points do not otherwise reset, because although the route can be reset, the point may still be
        locked at that stage).
      }
      IF InAutoMode OR ResetAllSwitchedPoints THEN BEGIN
        IF NOT PointResettingMode THEN
          SetLength(PointResettingToDefaultStateArray, 0)
        ELSE BEGIN
          I := 0;
          WHILE I <= High(PointResettingToDefaultStateArray) DO BEGIN
            IF NOT PointIsLocked(PointResettingToDefaultStateArray[I], LockingMsg) THEN BEGIN
              WITH Points[PointResettingToDefaultStateArray[I]] DO BEGIN
                IF Point_PresentState = Point_RequiredState THEN
                  DeleteElementFromIntegerArray(PointResettingToDefaultStateArray, I)
                ELSE BEGIN
                  IF Point_ResettingTime = 0 THEN
                    Point_ResettingTime := Time
                  ELSE
                    IF (Point_ResettingTime <> 0) AND (CompareTime(IncSecond(Point_ResettingTime, 5), Time) < 0) THEN BEGIN
                      Log('P Resetting P=' + IntToStr(PointResettingToDefaultStateArray[I]) + ' one minute after unlocking');
                      PullPoint(UnknownLocoChipStr, PointResettingToDefaultStateArray[I], NoRoute, NoSubRoute, NOT ForcePoint, NOT ByUser,
                                NOT ErrorMsgRequired, PointResultPending, ErrorMsg, OK);
  //                      IF OK THEN
  //                        LastPointResetTime := Time;

                      Point_ResettingTime := 0;
                      DeleteElementFromIntegerArray(PointResettingToDefaultStateArray, I);
                    END;
                END;
              END; {WITH}
            END;
            Inc(I);
          END; {WHILE}
        END;
      END;

      IF InAutoMode THEN BEGIN
        { See if any train lights are due to be switched on }
        I := 0;
        WHILE I <= High(LightsToBeSwitchedOnArray) DO BEGIN
          WITH LightsToBeSwitchedOnArray[I] DO BEGIN
            WITH Trains[LightsToBeSwitchedOn_Train] DO BEGIN
              IF CurrentRailwayTime >= LightsToBeSwitchedOn_SwitchOnTime THEN BEGIN
                Log(Train_LocoChipStr + ' L Now switching on ' + LightsToBeSwitchedOn_ColourStr1 + ' lights at up'
                                      + ' and ' + LightsToBeSwitchedOn_ColourStr2 + ' lights at down');
                SetTrainDirection(LightsToBeSwitchedOn_Train, LightsToBeSwitchedOn_Direction1, ForceAWrite, OK);
                IF LightsToBeSwitchedOn_Direction2 <> UnknownDirection THEN
                  SetTwoLightingChips(Train_LocoIndex, LightsToBeSwitchedOn_Direction1, LightsToBeSwitchedOn_Direction2, LightsOn);
                TurnTrainLightsOn(LightsToBeSwitchedOn_Train, OK);

                IF TrainHasCabLights(LightsToBeSwitchedOn_Train) AND Train_CabLightsAreOn THEN
                  TurnTrainCablightsOff(LightsToBeSwitchedOn_Train, OK);

                DeleteElementFromLightsToBeSwitchedOnArray(I);
              END;
            END; {WITH}
          END; {WITH}
          Inc(I);
        END; {WHILE}
      END;

      IF InAutoMode THEN BEGIN
        IF NOT RouteClearingOnlyMode AND NOT RouteingSuspendedForModalDialogue THEN BEGIN
          { Create routes that need creating, but don't do so if RouteClearingOnly is on (mode whereby routes are automatically cleared when train pass, but no routes are
            created - problem with the mode though - doesn't clear the system-occupied TCs when the train passes **** FWP 16/10/06)
          }
          FOR T := 0 TO High(Trains) DO
            CreateRouteArraysForTrain(T);
        END;

        DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 10');
        MoveAllTrains;

        { If any train has passed each track circuit, the subroute can be released }
        ReleaseSubRoutes;

        DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 11');
        MoveAllTrains;

        DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 12');
        MoveAllTrains;

        DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 13');
        MoveAllTrains;

        IF ReplayMode THEN BEGIN
          IF ReplayScrollDown THEN
            AdvanceLogFileByOneLine
          ELSE
            IF ReplayScrollUp THEN
              ReverseLogFileByOneLine
            ELSE
              IF ReplaySignalChangeSearch THEN
                AdvanceLogFileByOneLine;
        END;

        CheckStationStartMode;
      END;

      InMainLoop := False;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG MainTimerTick: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { MainTimerTick }

PROCEDURE TMainWindow.WatchdogOneSecondTimerTick(Sender: TObject);
{ This sends a message to the watchdog program once a second to show we're still running. It also tells the watchdog what kind of connection we're using. }
BEGIN
  IF LenzConnection = USBConnection THEN
    SendStringToWatchdogProgram('FWPRail is running ' + ConnectedViaUSBStr)
  ELSE
    IF LenzConnection = EthernetConnection THEN
      SendStringToWatchdogProgram('FWPRail is running ' + ConnectedViaEthernetStr)
    ELSE
      IF LenzConnection = NoConnection THEN
        SendStringToWatchdogProgram('FWPRail is running ' + NotConnectedStr);
END; { WatchdogOneSecondTimerTick }

PROCEDURE TMainWindow.SendStringToWatchdogProgram(S : String);
VAR
  copyData: TCopyDataStruct;
  ReceiverHandle : THandle;
  ReceiverTypeString : PWideChar;
  Res : Integer;

BEGIN
  ReceiverTypeString := 'TWatchdogWindow';
  ReceiverHandle := FindWindow(ReceiverTypeString, NIL);
  IF ReceiverHandle = 0 THEN BEGIN
    IF NOT WatchdogErrorMsgFlag THEN BEGIN
      Debug('FWPRail watchdog not found - proceeding without it');
      WatchdogActiveMsgFlag := False;
      WatchdogErrorMsgFlag := True;
    END;

    Exit;
  END;

  { We have found the watchdog program }
  IF InDebuggingMode THEN
    Log('X Sending "' + S + '" message to Watchdog program');

  CopyData.lpData := PChar(S);
  CopyData.cbdata := Bytelength(S);
  CopyData.dwData := ReceiverHandle;

  Res := SendMessage(ReceiverHandle, WM_COPYDATA, Application.Handle, LPARAM(@CopyData));
  IF (Res = 0) AND NOT WatchdogErrorMsgFlag THEN BEGIN
    Log('X FWPRail Watchdog has not responded correctly to "FWPRail is running" message');
    WatchdogErrorMsgFlag := True;
    WatchdogActiveMsgFlag := False;
  END ELSE
    IF Res = 1 THEN BEGIN
      IF InDebuggingMode THEN
        Log('X FWPRail Watchdog has acknowledged the "FWPRail is running" message');
      WatchdogErrorMsgFlag := False;
      IF NOT WatchdogActiveMsgFlag THEN
        WatchdogActiveMsgFlag := True;
    END ELSE
      IF InDebuggingMode THEN
        Log('XG FWPRail Watchdog has incorrectly responded to "FWPRail is running" message with the response number: ' + IntToStr(Res));
END; { SendStringToWatchdogProgram }

PROCEDURE InitialiseMainUnit;
{ Such routines as this allow us to initialises the units in the order we wish }
BEGIN
  SaveSystemStatusEmergencyOff := False;

  Log('A Main Unit initialised');
END; { InitialiseGetTimeUnit }

INITIALIZATION

END { Main }.
