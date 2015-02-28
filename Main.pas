UNIT Main;
{ Creates the various windows and runs the timers

  Copyright © F.W. Pritchard 2015. All Rights Reserved.

  v0.1  10/04/14 Unit extracted from RailDraw
}

INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, InitVars, Vcl.ExtCtrls,
     TrackCircuitsUnit;

TYPE
  TMainUnitWindow = CLASS(TForm)
    MainUnitTimer: TTimer;
    MainUnitWatchdogOneSecondTimer: TTimer;
    PROCEDURE MainUnitTimerTick(Sender: TObject);
    PROCEDURE MainUnitWatchdogOneSecondTimerTick(Sender: TObject);
    PROCEDURE MainUnitWindowCreate(Sender: TObject);
  PRIVATE
    { Private declarations }
    PROCEDURE SendStringToWatchdogProgram(S : String);

  PUBLIC
    { Public declarations }
  END;

PROCEDURE InitialiseMainUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

PROCEDURE ShutDownProgram(UnitRef : String; SubroutineStr : String);
{ Shut down the program neatly }

PROCEDURE StartSystemTimer;
{ Starts the system timer only }

PROCEDURE StopOrResumeAllOperations(Str : String);
{ Deal with emergency situations by stopping operations or restarting them }

PROCEDURE StopSystemTimer;
{ Stops the system timer only }

PROCEDURE TurnAutoModeOff(User : Boolean);
{ Turns auto mode off }

PROCEDURE TurnAutoModeOn;
{ Turns auto mode on }

VAR
  MainUnitWindow: TMainUnitWindow;

IMPLEMENTATION

{$R *.dfm}

USES GetTime, RailDraw, MiscUtils, Locks, LocationsUnit, Feedback, Options, System.StrUtils, Lenz, System.DateUtils, TestUnit, Movement, FWPShowMessageUnit, CreateRoute,
     Diagrams, Route, Replay, Startup, Cuneo, LocoUtils, StationMonitors, ProgressBar, LocoDialogue, Help, WorkingTimetable, Edit, RDCUnit, Input, Train, SyncObjs,
     Logging, SignalsUnit, PointsUnit, LinesUnit, TCPIP;

CONST
  ConnectedViaUSBStr = 'via USB';
  ConnectedViaEthernetStr = 'via Ethernet';
  NotConnectedStr = 'but not connected to Lenz';
  UnitRef = 'Main';

VAR
  InMainLoop : Boolean = False;
  NumbersArrayCounter : Integer = -1;
  OperationsStopped : Boolean = False;
  SaveSystemStatusEmergencyOff : Boolean;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE StopOrResumeAllOperations(Str : String);
{ Deal with emergency situations by stopping operations or restarting them }
VAR
  OK : Boolean;
  P : Integer;
  S : Integer;

BEGIN
  IF NOT SystemOnline THEN
    Debug('Cannot stop or resume operations - system offline')
  ELSE BEGIN
    IF OperationsStopped THEN BEGIN
      IF MessageDialogueWithDefault('Resume operations?', NOT StopTimer, mtConfirmation, [mbOK, mbAbort], mbAbort) = mrOK THEN BEGIN
        Log('A ' + Str + ' pressed : requesting resume all operations');
        ResumeOperations(OK);
        IF OK THEN BEGIN
          Log('AG Operations resumed');
          OperationsStopped := False;
        END ELSE
          Log('A! Operations not resumed');

        InvalidateScreen(UnitRef, 'StopOrResumeAllOperations');
      END;
    END ELSE BEGIN
      Log('A ' + Str + ' pressed : requesting stop all operations');
      StopOperations(OK);
      IF OK THEN BEGIN
        Log('A! All operations stopped');
        OperationsStopped := True;
      END;

      FOR P := 0 TO High(Points) DO
        EmergencyDeselectPoint(P, OK);
      Log('P! User has switched all points off');

      FOR S := 0 TO High(Signals) DO
        EmergencyDeselectSignal(S, OK);
      Log('S! User has switched all signals off');
    END;
  END;
END; { StopOrResumeAllOperations }

PROCEDURE TMainUnitWindow.MainUnitWindowCreate(Sender: TObject);
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
    IF LocationsUnitWindow = NIL THEN BEGIN
      LocationsUnitWindow := TLocationsUnitWindow.Create(Application);
      LocationsUnitWindow.Update;
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
      Log('EG MainUnitWindowCreate:' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { MainUnitWindowCreate }

PROCEDURE StartSystemTimer;
{ Starts the system timer only }
BEGIN
  MainUnitWindow.MainUnitTimer.Enabled := True;
END; { StartSystemTimer }

PROCEDURE StopSystemTimer;
{ Stops the system timer only }
BEGIN
  MainUnitWindow.MainUnitTimer.Enabled := False;
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

PROCEDURE TMainUnitWindow.MainUnitTimerTick(Sender: TObject);
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

PROCEDURE TMainUnitWindow.MainUnitWatchdogOneSecondTimerTick(Sender: TObject);
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

PROCEDURE TMainUnitWindow.SendStringToWatchdogProgram(S : String);
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

PROCEDURE ShutDownProgram(UnitRef : String; SubroutineStr : String);
{ Shut down the program neatly }

CONST
  Init = True;
  TrainListOnly = True;
  ReadWriteRegistry = True;

VAR
  OK : Boolean;
  T : TrainIndex;
  WindowsTaskBar : HWND;

BEGIN { ShutDownProgram }
  TRY
    { Write out the locations of the locos so we know where they are when we start up next time (locations are the last known location, added when a loco moves, or is
      purged)
    }
    Log('A! Shut down initiated');
    ProgramShuttingDown := True;

    IF ScreenColoursSetForPrinting THEN
      { we need to do this or the wrong colours are saved in the registry }
      ResetScreenColoursAfterPrinting;

    { Close the station monitor web page if it exists }
    CloseStationMonitorsWebPage(OK);
    IF OK THEN
      Log('A Station Monitors web page closed');

    { Restore the Windows taskbar if we're in full screen mode and it's been disabled }
    IF WindowsTaskbarDisabled THEN BEGIN
      { Find handle of TASKBAR }
      WindowsTaskBar := FindWindow('Shell_TrayWnd', NIL);
      { Enable the taskbar }
      EnableWindow(WindowsTaskBar, True);
      { Show the taskbar }
      ShowWindow(WindowsTaskbar, SW_SHOW);

      WindowsTaskbarDisabled := False;
    END;

    IF NOT ProgramStarting THEN BEGIN
      WriteOutLineDataToDatabase;
      WriteOutLocationDataToDatabase;
      WriteOutPointDataToDatabase;
      WriteOutSignalDataToDatabase;

      IF SystemOnline THEN
        WriteOutLocoDataToDatabase;

      { Stop any trains that are currently moving - better than leaving them running }
      IF StopAllLocosAtShutDown THEN
        StopLocos('shutdown');

      IF NOT AllSignalsSwitchedOff THEN
        SetAllSignalsToDanger;

      IF SwitchActiveLocoLightsOffAtShutDown THEN BEGIN
        T := 0;
        WHILE T <= High(Trains) DO BEGIN
          WITH Trains[T] DO BEGIN
            IF (Train_LocoIndex <> UnknownLocoIndex) AND (TrainFoundInDiagrams(Train_LocoIndex) <> 0) THEN BEGIN
              IF Train_HasLights THEN BEGIN
                TurnTrainLightsOff(T, OK);
                IF TrainHasCabLights(T) AND Train_CabLightsAreOn THEN
                  TurnTrainCabLightsOff(T, OK);
              END;
            END;
          END; {WITH}
          Inc(T);
        END; {WHILE}
      END;

      { Write then close the log file }
      WriteToLogFileAndTestFile := True;
      IF InRDCMode AND RailDriverInitialised THEN BEGIN
        WriteToRailDriverLEDs('');
        CloseRailDriver;
      END;

      StopSystemTimer;

      { Write things back to the .ini file }
      IF NOT ReplayMode AND NOT ProgramStarting THEN BEGIN
        Log('A Writing .ini file');
        WriteIniFile;
      END;
    END;

    IF LenzConnection = USBConnection THEN
      StopLANUSBServer;

    Log('A Shut down initiated in ' + UnitRef + ' unit, ' + SubroutineStr + ' subroutine, is now complete (' + DescribeActualDateAndTime + ')');
    IF InLogsCurrentlyKeptMode THEN BEGIN
      CloseFile(TestLogFile);
      CloseFile(LargeLogFile);
      IF MultipleLogFilesRequired THEN BEGIN
        CloseFile(ErrorLogFile);
        CloseFile(LocoLogFile);
        CloseFile(RouteLogFile);
        CloseFile(SignalPointAndTCLogFile);
        CloseFile(DiagramsLogFile);
        CloseFile(WorkingTimetableLogFile);
      END;

      { Now reset the mode so that we don't try to write to log files after they've been closed }
      SetMode(LogsCurrentlyKept, False);
    END;

    IF RestoreLogsToPreviousState THEN BEGIN
      { Erase the newly created log file (if we're only doing a replay, not running a proper sequence, or else logging is off and rename the previous ones if they exist }
      RenameLaterFiles(LargeLogFile, PathToLogFiles + LogFileName, LogFileNameSuffix);
      RenameLaterFiles(TestLogFile, PathToLogFiles + LogFileName + '-Test', LogFileNameSuffix);
      IF MultipleLogFilesRequired THEN BEGIN
        RenameLaterFiles(ErrorLogFile, PathToLogFiles + LogFileName + '-Error', LogFileNameSuffix);
        RenameLaterFiles(LocoLogFile, PathToLogFiles + LogFileName + '-Loco', LogFileNameSuffix);
        RenameLaterFiles(RouteLogFile, PathToLogFiles + LogFileName + '-Route', LogFileNameSuffix);
        RenameLaterFiles(SignalPointAndTCLogFile, PathToLogFiles + LogFileName + '-SignalPointAndTC', LogFileNameSuffix);
        RenameLaterFiles(DiagramsLogFile, PathToLogFiles + LogFileName + '-Diagrams', LogFileNameSuffix);
        RenameLaterFiles(WorkingTimetableLogFile, PathToLogFiles + LogFileName + '-WorkingTimetable', LogFileNameSuffix);
      END;
    END;

    { and stop }
    IF NOT ReplayMode THEN BEGIN
      Application.Terminate;
      IF NOT Application.Terminated THEN
        Halt(12);
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG ShutDownProgram: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ShutDownProgram }

PROCEDURE InitialiseMainUnit;
{ Such routines as this allow us to initialises the units in the order we wish }
BEGIN
  SaveSystemStatusEmergencyOff := False;

  Log('A Main Unit initialised');
END; { InitialiseMainUnit }

INITIALIZATION

END { Main }.
