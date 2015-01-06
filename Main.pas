UNIT Main;
{ Creates the various windows and runs the timers

  Copyright © F.W. Pritchard 2015. All Rights Reserved.

  v0.1  10/04/14 Unit extracted from Raildraw
}

INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, InitVars, Vcl.ExtCtrls;

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

PROCEDURE CheckLineConnectionsAreOK;
{ To check each line has the requisite connection data, as locking depends on this }

PROCEDURE FindAdjoiningTrackCircuits(TC : Integer; OUT AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit : Integer);
{ Work out which are the adjacent track circuits. Does not trace along all lines, just the way points are set. }

PROCEDURE FindNextPoint(TC : Integer; SearchDirection : DirectionType; OUT NextPoint : Integer);
{ Work out which is the next point }

FUNCTION GetBufferStopDirection(BufferStopNum : Integer) : DirectionType;
{ Return a buffer stop's direction }

PROCEDURE InitialiseLocationLines;
{ Now we have set up the lines and signals, we can work out how they relate to locations. Some long locations (UFY for instance) will have multiple lines attached *** }

PROCEDURE InitialiseMainUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

PROCEDURE RestoreAllSignalsToPreviousState;
{ Sets all off signals to how they were before short circuit }

PROCEDURE SaveSignalsCurrentState;
{ Save all the previous state of all signals }

PROCEDURE SetAllSignalsToDanger;
{ Sets all off signals to on }

PROCEDURE SetSignal(LocoChipStr : String; S : Integer; NewAspect : AspectType; LogSignalData, ForceWriting : Boolean);
{ Set the state of a particular signal and draws it }

PROCEDURE SetTrackCircuitState{1}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the track circuit is occupied, and mark it with a loco chip number }

PROCEDURE SetTrackCircuitState{2}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the track circuit is occupied, mark it with a loco chip number, and give an explanation }

PROCEDURE SetTrackCircuitState{3}(TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the track circuit is occupied }

PROCEDURE SetTrackCircuitState{4}(TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the track circuit is occupied and give an explanation. Also see whether we want it recorded in the Location occupation array - this is to avoid
  duplicate recordings at startup.
}
PROCEDURE StartSystemTimer;
{ Starts the system timer only }

PROCEDURE StopSystemTimer;
{ Stops the system timer only }

PROCEDURE TurnAllSignalsOff;
{ Turn off the LEDs in the signals }

PROCEDURE TurnAutoModeOff(User : Boolean);
{ Turns auto mode off }

PROCEDURE TurnAutoModeOn;
{ Turns auto mode on }

VAR
  MainWindow: TMainWindow;

IMPLEMENTATION

{$R *.dfm}

USES GetTime, Raildraw, MiscUtils, Locks, LocationData, Feedback, Options, System.StrUtils, Lenz, System.DateUtils, TestUnit, Movement, FWPShowMessageUnit, CreateRoute,
     Diagrams, Route, Replay, Startup, Cuneo, LocoUtils, StationMonitors, ProgressBar, LocoDialogue, Help, WorkingTimetable, Edit, RDCUnit, Input, Train, SyncObjs,
     Logging;

CONST
  ConnectedViaUSBStr = 'via USB';
  ConnectedViaEthernetStr = 'via Ethernet';
  NotConnectedStr = 'but not connected to Lenz';
  UndrawRequired = True;
  UndrawToBeAutomatic = True;
  UnitRef = 'Main';

VAR
  MainCriticalSection : TCriticalSection;
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
    InitialiseRaildrawUnit;
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

PROCEDURE SetSignal(LocoChipStr : String; S : Integer; NewAspect : AspectType; LogSignalData, ForceWriting : Boolean);
{ Set the state of a particular signal and draws it }
BEGIN
  TRY
    WITH Signals[S] DO BEGIN
      IF (Signal_Aspect <> NewAspect) OR ForceWriting THEN BEGIN
        Signal_Aspect := NewAspect;
        IF NOT ProgramStartup AND LogSignalData THEN
          Log(LocoChipStr + ' S S=' + IntToStr(S) + ' successfully set to ' + AspectToStr(Signals[S].Signal_Aspect));

        IF SystemOnline AND NOT ResizeMap THEN BEGIN
          IF Signal_DecoderNum <> 0 THEN
            { uses LF100 decoders - bits usually set as follows:
              green is bit 1, red 2, single yellow 3, double yellow 3 + 4; the indicator is bit 4 (not necessarily on same decoder though)
            }
            SetSignalFunction(LocoChipStr, S)
          ELSE
            IF Signal_AccessoryAddress <> 0 THEN
              { uses TrainTech SC3 units for controlling Dapol semaphores }
              IF NewAspect = RedAspect THEN
                MakeSemaphoreSignalChange(LocoChipStr, S, Signal_AccessoryAddress, SignalOn)
              ELSE
                MakeSemaphoreSignalChange(LocoChipStr, S, Signal_AccessoryAddress, SignalOff);
        END;

        IF NOT ProgramStartup THEN
          { calling invalidate here didn't redraw the signal in time - repaint causes the screen to be redrawn instantly and not via the message queue }
          FWPRailWindow.Repaint;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG SetSignal:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetSignal }

PROCEDURE SetAllSignalsToDanger;
{ Sets all off signals to on }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      IF NOT Signals[S].Signal_OutOfUse THEN BEGIN
        IF (GetSignalAspect(S) <> RedAspect) THEN
          SetSignal(UnknownLocoChipStr, S, RedAspect, LogSignalData, NOT ForceAWrite);
        IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
          SetIndicator(UnknownLocoChipStr, S, NoIndicatorLit, '', NoRoute, NOT ByUser);
      END;
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG SetAllSignalsToDanger:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetAllSignalsToDanger }

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

PROCEDURE SaveSignalsCurrentState;
{ Save all the previous state of all signals }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF NOT Signal_OutOfUse THEN BEGIN
          Signal_PreviousAspect := Signals[S].Signal_Aspect;
          Signal_PreviousIndicatorState := Signals[S].Signal_IndicatorState;
          Signal_PreviousTheatreIndicatorString := Signal_TheatreIndicatorString;
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG SaveSignalsCurrentState:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SaveSignalsCurrentState }

PROCEDURE TurnAllSignalsOff;
{ Turn off the LEDs in the signals }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      SetSignal(UnknownLocoChipStr, S, NoAspect, LogSignalData, NOT ForceAWrite);
      IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
        SetIndicator(UnknownLocoChipStr, S, NoIndicatorLit, '', NoRoute, NOT ByUser);
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG TurnAllSignalsOff:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TurnAllSignalsOff }

PROCEDURE RestoreAllSignalsToPreviousState;
{ Sets all off signals to how they were before short circuit }
VAR
  S : Integer;

BEGIN
  TRY
    Log('S Restoring all signals to their previous aspects');
    FOR S := 0 TO High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF NOT Signal_OutOfUse THEN BEGIN
          { have to set state to NoAspect, or SetSignal won't redraw the SignalAspect/indicator }
          Signal_Aspect := NoAspect;
          Signal_IndicatorState := NoIndicatorLit;
          SetSignal(UnknownLocoChipStr, S, Signal_PreviousAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousIndicatorState <> NoIndicatorLit THEN
            SetIndicator(UnknownLocoChipStr, S, Signal_PreviousIndicatorState, Signal_PreviousTheatreIndicatorString, NoRoute, NOT ByUser);
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG RestoreAllSignalsToPreviousState:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { RestoreAllSignalsToPreviousState }

FUNCTION GetBufferStopDirection(BufferStopNum : Integer) : DirectionType;
{ Return a buffer stop's direction }
BEGIN
  Result := BufferStops[BufferStopNum].BufferStop_Direction;
END; { GetBufferStopDirection }

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
            IF SearchDirection = Up THEN
              CurrentLine := Lines[CurrentLine].Line_NextUpLine
            ELSE
              CurrentLine := Lines[CurrentLine].Line_NextDownLine;

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

PROCEDURE FindAdjoiningTrackCircuits(TC : Integer; OUT AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit : Integer);
{ Work out which are the adjacent track circuits. Does not trace along all lines, just the way points are set. }
CONST
  FindPoint = True;

VAR
  Line : Integer;
  NextPoint : Integer;
  TCFound : Boolean;

BEGIN
  TRY
    AdjoiningUpTrackCircuit := UnknownTrackCircuit;
    AdjoiningDownTrackCircuit := UnknownTrackCircuit;

    IF TC <> UnknownTrackCircuit THEN BEGIN
      Line := 0;
      TCFound := False;
      WHILE (Line <= High(Lines)) AND NOT TCFound DO BEGIN
        IF Lines[Line].Line_TC = TC THEN BEGIN
          TCFound := True;

          FollowThatLine(Line, TC, Up, NOT FindPoint, AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit, NextPoint);
          FollowThatLine(Line, TC, Down, NOT FindPoint, AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit, NextPoint);
        END;
        Inc(Line);
      END; {WHILE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FindAdjoiningTrackCircuits:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FindAdjoiningTrackCircuits }

PROCEDURE FindNextPoint(TC : Integer; SearchDirection : DirectionType; OUT NextPoint : Integer);
{ Work out which is the next point }
CONST
  FindPoint = True;

VAR
  AdjoiningUpTrackCircuit : Integer;
  AdjoiningDownTrackCircuit : Integer;
  Line : Integer;
  TCFound : Boolean;

BEGIN
  TRY
    NextPoint := UnknownPoint;

    IF TC <> UnknownTrackCircuit THEN BEGIN
      Line := 0;
      TCFound := False;
      WHILE (Line <= High(Lines)) AND NOT TCFound DO BEGIN
        IF Lines[Line].Line_TC = TC THEN BEGIN
          TCFound := True;

          IF SearchDirection = Up THEN
            FollowThatLine(Line, TC, Up, FindPoint, AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit, NextPoint)
          ELSE
            FollowThatLine(Line, TC, Down, FindPoint, AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit, NextPoint);
        END;
        Inc(Line);
      END; {WHILE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FindAdjoiningTrackCircuits:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FindNextPoint }

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

PROCEDURE SetTrackCircuitStateMainProcedure(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType; Explanation : String);
{ Set whether and how the track circuit is occupied and give an explanation if any }
CONST
  DoNotWriteMessage = True;
  DiagramsLoading = True;

VAR
  AdjacentTrackCircuits : IntegerArrayType;
  AdjacentUpTC, AdjacentDownTC : Integer;
  ErrorMsg : String;
  I, J : Integer;
  LineArray : IntegerArrayType;
  Location : Integer;
  LocationOccupationCount : Integer;
  TempLocationLocoChips : IntegerArrayType;
  LocoFound : Boolean;
  OK : Boolean;
  P : Integer;
  T : TrainIndex;

BEGIN
  TRY
    IF (TC < 0) OR (TC > High(TrackCircuits)) THEN BEGIN
      ShowMessage('Non-existent TC=' + IntToStr(TC) + ' is trying to be set to ' + TrackCircuitStateToStr(NewState));
      Log('X Non-existent TC=' + IntToStr(TC) + ' is trying to be set to ' + TrackCircuitStateToStr(NewState));
      InvalidateScreen(UnitRef, 'SetTrackCircuitStateMainProcedure');
    END ELSE BEGIN
      WITH TrackCircuits[TC] DO BEGIN
        IF (NewState <> TrackCircuits[TC].TC_OccupationState) OR (LocoChip <> TrackCircuits[TC].TC_LocoChip) THEN BEGIN
          { Store the state in case we have to revert, but don't do it on both occupation and end of occupation or we can't revert }
          TC_PreviousOccupationState := TC_OccupationState;
          IF TC_LocoChip <> UnknownLocoChip THEN BEGIN
            Log('T TC=' + IntToStr(TC) + '''s TC_PreviousLocoChip set to ' + LocoChipToStr(TC_LocoChip));
            TC_PreviousLocoChip := TC_LocoChip;
          END;

          { If track circuit is now unoccupied, having been system occupied, see if part of a route needs clearing in advance of the full route tidying up in ClearARoute }
          IF NOT RedrawScreen THEN BEGIN
            T := GetTrainIndexFromLocoChip(TC_LocoChip);
            IF (TC_PreviousOccupationState = TCSystemOccupation)
            OR ((T <> UnknownTrainIndex)
                AND NOT Trains[T].Train_UseTrailingTrackCircuits
                AND (TC_PreviousOccupationState = TCFeedbackOccupation))
            THEN BEGIN
              TC_OccupationState := NewState;
              IF NewState = TCUnoccupied THEN BEGIN
                IF TrackCircuits[TC].TC_LockedForRoute <> UnknownRoute THEN BEGIN
                  FOR P := 0 TO High(Points) DO BEGIN
                    { using a FOR loop here, as there may be more than one point locked by the same TC route }
                    IF Points[P].Point_TCAtHeel = TC THEN BEGIN
                      IF Points[P].Point_TCAtHeel = TC THEN BEGIN
                        IF PointIsLockedByASpecificRoute(P, TrackCircuits[TC].TC_LockedForRoute) THEN BEGIN
                          Log(LocoChipToStr(TC_LocoChip) + ' R Point ' + IntToStr(P) + ' was locked by'
                                                         + ' R=' + IntToStr(TrackCircuits[TC].TC_LockedForRoute) + ' - now unlocked');
                          UnlockPointLockedBySpecificRoute(P, TrackCircuits[TC].TC_LockedForRoute, DoNotWriteMessage);

                          { and any related crossover or three-way points too }
                          IF Points[P].Point_RelatedPoint <> UnknownPoint THEN
                            IF PointIsLockedByASpecificRoute(Points[P].Point_RelatedPoint, TrackCircuits[TC].TC_LockedForRoute) THEN
                              UnlockPointLockedBySpecificRoute(Points[P].Point_RelatedPoint, TrackCircuits[TC].TC_LockedForRoute, DoNotWriteMessage);
                        END;
                      END;
                    END;
                  END; {FOR}

                  { and see if any route-locking drawing needs to be removed too }
                  LineArray := GetLinesForTrackCircuit(TC);
                  FOR I := 0 TO High(LineArray) DO BEGIN
                    IF Lines[LineArray[I]].Line_RouteLockingForDrawing = TrackCircuits[TC].TC_LockedForRoute THEN BEGIN
                      Log(LocoChipToStr(TC_LocoChip) + ' R L=' + LineToStr(LineArray[I]) + ' RouteLockingForDrawing was '
                                                     + IntToStr(Lines[LineArray[I]].Line_RouteLockingForDrawing) + ' now set to unknown route');
                      Lines[LineArray[I]].Line_RouteLockingForDrawing := UnknownRoute;
                    END;
                    IF Lines[LineArray[I]].Line_RouteSet <> - 1 THEN BEGIN
                      Log(LocoChipToStr(TC_LocoChip) + ' R L=' + LineToStr(LineArray[I]) + ' SubRouteSet set to False');
                      Lines[LineArray[I]].Line_RouteSet := UnknownRoute;
                    END;
                  END;

                  { or if a signal needs unlocking too }
                  FOR I := 0 TO High(TC_AdjacentSignals) DO BEGIN
                    IF SignalIsLockedBySpecificRoute(TC_AdjacentSignals[I], TrackCircuits[TC].TC_LockedForRoute) THEN BEGIN
                      Log(LocoChipToStr(TC_LocoChip) + ' R S=' + IntToStr(TC_AdjacentSignals[I]) + ' unlocked by R=' + IntToStr(TrackCircuits[TC].TC_LockedForRoute));
                      UnlockSignalLockedBySpecificRoute(TC_AdjacentSignals[I], TrackCircuits[TC].TC_LockedForRoute);
                    END;
                  END;

                  UnlockTrackCircuitRouteLocking(TC);
                END;
              END;
            END;

            TC_OccupationState := NewState;

            IF (NewState = TCOutOfUseSetByUser) OR (NewState = TCOutOfUseAsNoFeedbackReceived) THEN BEGIN
              Location := GetLocationFromTrackCircuit(TC);
              { reallocate any train expected to occupy the location }
              IF Location <> UnknownLocation THEN BEGIN
                FOR LocationOccupationCount := 0 TO High(LocationOccupations[Location]) DO BEGIN
                  { Store any locochip numbers now as we have to clear the array before we can reallocate them. (If we don't, it is possible that they will be reallocated
                    to the same, defunct, location.
                  }
                  IF LocationOccupations[Location, LocationOccupationCount].LocationOccupation_LocoChip <> UnknownLocoChip THEN
                    AppendToIntegerArray(TempLocationLocoChips, LocationOccupations[Location, LocationOccupationCount].LocationOccupation_LocoChip);
                END;

                { Now mark the location out of use }
                ClearLocationOccupationsForSpecificLocation(Location);
                SetLocationOccupationAllDayState(UnknownLocoIndex, Location, LocationOutOfUseOccupation, ErrorMsg, OK);
              END;

              IF Length(TempLocationLocoChips) > 0 THEN BEGIN
                FOR I := 0 TO High(TempLocationLocoChips) DO BEGIN
                  T := GetTrainIndexFromLocoChip(TempLocationLocoChips[I]);
                  IF T <> UnknownTrainIndex THEN
                    SetUpTrainLocationOccupationsAbInitio(T, OK);
                END;
              END;
            END;

            IF NewState <> TCUnoccupied THEN BEGIN
              TC_LocoChip := LocoChip;
              TC_Headcode := '????'; { **** }
            END ELSE BEGIN
              { TC is now unoccupied - reset its loco occupation - but not if it's a system occupation }
              IF (TC_LocoChip <> UnknownLocoChip) AND (NewState = TCUnoccupied) THEN BEGIN
                Log('T TC=' + IntToStr(TC) + ' TC_LocoChip=' + LocoChipToStr(TC_LocoChip) + ' set to unknown loco Chip');
                TC_LocoChip := UnknownLocoChip;
              END;

              TC_Headcode := '----';

              IF ((TrackCircuits[TC].TC_PreviousOccupationState = TCOutOfUseSetByUser)
              OR (TrackCircuits[TC].TC_PreviousOccupationState = TCOutOfUseAsNoFeedbackReceived))
              THEN BEGIN
                Location := GetLocationFromTrackCircuit(TC);
                ClearLocationOccupationsForSpecificLocation(Location);
              END;
            END;

            { Is this a signal-resetting track circuit? If so, reset the signal. And put in a timing update here - for delays **** }
            IF (NewState = TCFeedbackOccupation) AND (TC_ResettingSignal <> UnknownSignal) THEN BEGIN
              WITH Signals[TC_ResettingSignal] DO BEGIN
                IF Signal_Aspect <> RedAspect THEN BEGIN
                  { check first for any semaphore homes locked by semaphore distants, which would block the homes being reset }
                  CheckSemaphoreDistantBeforeSemaphoreHomeCleared(TC_ResettingSignal);
                  PullSignal(UnknownLocoChipStr, TC_ResettingSignal, NoIndicatorLit, NoRoute, NoSubRoute, UnknownLine, TC, NOT ByUser, OK);
                  IF OK THEN BEGIN
                    Log('S S=' + IntToStr(TC_ResettingSignal) + ' reset by TC=' + IntToStr(TC));
                    { also reset any hidden station signal aspects }
                    IF Signals[TC_ResettingSignal].Signal_HiddenStationSignalAspect <> RedAspect THEN BEGIN
                      Signals[TC_ResettingSignal].Signal_HiddenStationSignalAspect := NoAspect;
                      Signals[TC_ResettingSignal].Signal_PostColour := SignalPostColour;
                    END;
                  END ELSE BEGIN
                    IF NOT Signal_FailedToResetFlag THEN BEGIN
                      Log('SG S=' + IntToStr(TC_ResettingSignal) + ' failed to reset');
                      Signal_FailedToResetFlag := True;
                    END;
                  END;
                END; {WITH}
              END;
              TC_ResettingSignal := UnknownSignal;
            END;

            IF NOT InAutoMode
            AND ((NewState = TCFeedbackOccupation) OR (NewState = TCLocoOutOfPlaceOccupation))
            AND (TC_LocoChip = UnknownLocoChip)
            AND NOT ProgramStartup
            AND NOT InLocoSpeedTimingMode
            THEN BEGIN
              { we've not been given a loco, so see if we can work out which loco it is }
              Log('E TC=' + IntToStr(TC) + ' feedback occupied but no loco number supplied - searching adjacent TCs...');
              FindAdjoiningTrackCircuits(TC, AdjacentUpTC, AdjacentDownTC);

              LocoFound := False;
              I := 0;
              J := 0;
              { see if any adjacent track circuits are marked as occupied }
              SetLength(AdjacentTrackCircuits, 0);
              IF AdjacentUptc <> UnknownTrackCircuit THEN
                AppendToIntegerArray(AdjacentTrackCircuits, AdjacentUpTC);
              IF AdjacentDowntc <> UnknownTrackCircuit THEN
                AppendToIntegerArray(AdjacentTrackCircuits, AdjacentDownTC);
              WHILE (I <= High(AdjacentTrackCircuits)) AND NOT LocoFound DO BEGIN
                IF ((TrackCircuits[AdjacentTrackCircuits[I]].TC_OccupationState = TCFeedbackOccupation)
                    OR (TrackCircuits[AdjacentTrackCircuits[I]].TC_OccupationState = TCLocoOutOfPlaceOccupation))
                AND (TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip <> UnknownLocoChip)
                THEN BEGIN
                  Log('E ...adjacent TC=' + IntToStr(AdjacentTrackCircuits[I])
                         + ' is marked as feedback occupied by ' + LocoChipToStr(TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip)
                         + ', so maybe this is the loco we want');
                  { we've putatively found it }
                  LocoFound := True;
                  { but make sure the other adjacent TCs are unoccupied, or we won't know which loco it is that's moving }
                  WHILE J <= High(AdjacentTrackCircuits) DO BEGIN
                    IF I <> J THEN BEGIN
                      IF ((TrackCircuits[AdjacentTrackCircuits[J]].TC_OccupationState = TCFeedbackOccupation)
                         OR (TrackCircuits[AdjacentTrackCircuits[J]].TC_OccupationState = TCLocoOutOfPlaceOccupation))
                      AND (TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip <> UnknownLocoChip)
                      AND (TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip <> TrackCircuits[AdjacentTrackCircuits[J]].TC_LocoChip)
                      THEN BEGIN
                        LocoFound := False;
                        Log('E ...adjacent TC=' + IntToStr(AdjacentTrackCircuits[J])
                               + ' is marked as feedback occupied by ' + LocoChipToStr(TrackCircuits[AdjacentTrackCircuits[J]].TC_LocoChip)
                               + ', so do not know which loco to select');
                      END;
                    END;
                    Inc(J);
                  END; {WHILE}
                  IF NOT LocoFound THEN
                    Log('E ... but no loco found in adjacent TCs')
                  ELSE BEGIN
                    TC_LocoChip := TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip;
                    T := GetTrainIndexFromLocoChip(TC_LocoChip);
                    IF T <> UnknownTrainIndex THEN
                      Trains[T].Train_CurrentTC := TC;
                    Log(LocoChipToStr(TC_LocoChip) + ' E ...found loco in adjacent TC=' + IntToStr(AdjacentTrackCircuits[I])
                                                   + ' so assuming it has moved to TC=' + IntToStr(TC));
                  END;
                END;
                Inc(I);
              END; {WHILE}
            END;
          END;

          IF Explanation <> '' THEN
            Log(LocoChipToStr(TC_LocoChip) + ' T Setting TC=' + IntToStr(TC) + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
                                           + ' ' + TrackCircuitStateToStr(NewState) + ' (' + Explanation + ')')
          ELSE
            Log(LocoChipToStr(TC_LocoChip) + ' T Setting TC=' + IntToStr(TC) + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
                                           + ' ' + TrackCircuitStateToStr(NewState));

          IF NOT ProgramStartup THEN
            InvalidateScreen(UnitRef, 'SetTrackCircuitStateMainProcedure');
        END;
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG SetTrackCircuitStateMainProcedure: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SetTrackCircuitStateMainProcedure }

PROCEDURE SetTrackCircuitState{1}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the track circuit is occupied, and mark it with a loco chip number }
CONST
  Explanation = '';

BEGIN
  SetTrackCircuitStateMainProcedure(LocoChip, TC, NewState, Explanation);
END; { SetTrackCircuitState-1 }

PROCEDURE SetTrackCircuitState{2}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the track circuit is occupied, mark it with a loco chip number, and give an explanation }
BEGIN
  SetTrackCircuitStateMainProcedure(LocoChip, TC, NewState, Explanation);
END; { SetTrackCircuitState-2 }

PROCEDURE SetTrackCircuitState{3}(TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the track circuit is occupied }
CONST
  Explanation = '';

VAR
  LocoChip : Integer;
  T : TrainIndex;
  TCFound : Boolean;

BEGIN
  TRY
    { See if it belongs to a loco }
    LocoChip := UnknownLocoChip;
    TCFound := False;
    T := 0;
    WHILE (T <= High(Trains)) AND NOT TCFound DO BEGIN
      WITH Trains[T] DO BEGIN
        IF Train_DiagramFound THEN BEGIN
          IF Train_CurrentTC = TC THEN BEGIN
            TCFound := True;
            LocoChip := Train_LocoChip;
            Log('T TC=' + IntToStr(TC) + ' allocated to loco ' + LocoChipToStr(LocoChip) + ' as it is that loco''s Train_CurrentTC');
          END;
        END;
        Inc(T);
      END; {WITH}
    END; {WHILE}

    SetTrackCircuitStateMainProcedure(LocoChip, TC, NewState, Explanation);
  EXCEPT
    ON E : Exception DO
      Log('EG SetTrackCircuitState-3:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetTrackCircuitState-3 }

PROCEDURE SetTrackCircuitState{4}(TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the track circuit is occupied and give an explanation. Also see whether we want it recorded in the Location occupation array - this is to avoid
  duplicate recordings at startup.
}
BEGIN
  SetTrackCircuitStateMainProcedure(UnknownLocoChip, TC, NewState, Explanation);
END; { SetTrackCircuitState-4 }

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
    IF NOT ProgramStartup THEN BEGIN
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
    { TCriticalSection allows a thread in a multithreaded application to temporarily block other threads from accessing a block of code }
    MainCriticalSection.Enter;
    StopSystemTimer;

    TRY
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
    FINALLY
      StartSystemTimer;
      MainCriticalSection.Leave;
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

BEGIN
  MainCriticalSection := TCriticalSection.Create;
END;

END { Main }.
