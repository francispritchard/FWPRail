UNIT Startup;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, GetTime, Lenz, Initvars, StdCtrls, Types, Registry;

TYPE
  TDebuggingOptionsWindow = CLASS(TForm)
    Startup_AllRouteDebuggingCheckBox: TCheckBox;
    Startup_DebuggingCheckBox: TCheckBox;
    Startup_FeedbackDebuggingCheckBox: TCheckBox;
    Startup_LineDebuggingCheckBox: TCheckBox;
    Startup_LockDebuggingCheckBox: TCheckBox;
    Startup_LockingCheckBox: TCheckBox;
    Startup_LogsKeptCheckBox: TCheckBox;
    Startup_PointDebuggingCheckBox: TCheckBox;
    Startup_RecordLineDrawingCheckBox: TCheckBox;
    Startup_RouteBacktrackDebuggingCheckBox: TCheckBox;
    Startup_RouteDebuggingCheckBox: TCheckBox;
    Startup_RouteDrawingCheckBox: TCheckBox;
    Startup_TestingCheckBox: TCheckBox;
    PROCEDURE DebuggingOptionsWindowHide(Sender: TObject);
    PROCEDURE DebuggingOptionsWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE DebuggingOptionsWindowShow(Sender: TObject);
    PROCEDURE Startup_AllRouteDebuggingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_DebuggingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_FeedbackDebuggingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_LineDebuggingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_LockDebuggingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_LockingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_LogsKeptCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_PointDebuggingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_RecordLineDrawingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_RouteBacktrackDebuggingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_RouteDebuggingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_RouteDrawingCheckBoxClick(Sender: TObject);
    PROCEDURE Startup_TestingCheckBoxClick(Sender: TObject);

  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  DebuggingOptionsWindow: TDebuggingOptionsWindow;
  ShiftKeyHeldDownOnStartup : Boolean;

PROCEDURE InitialiseStartupUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

IMPLEMENTATION

{$R *.dfm}

USES LocoUtils, MiscUtils, Input, RailDraw, RDCUnit, DateUtils, Feedback, CreateRoute, Diagrams, StrUtils, LocationsUnit, Options;

CONST
  UnitRef = 'Startup';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE GetWord(VAR Str : String; VAR Word : String);
{ Finds the first word in S (delimited by spaces) and returns it in Word having removed it from Str }
VAR
  Start, Finish : Integer;

BEGIN
  Start := 1;
  WHILE (Start <= Length(Str)) AND (Str[Start] = ' ') DO
    Inc(Start);
  Finish := Start;
  WHILE (Finish <= Length(Str)) AND (Str[Finish] <> ' ') DO
    Inc(Finish);

  Word := Copy(Str, Start, Finish - Start);
  Str := Copy(Str, Finish);
END; { GetWord }

PROCEDURE SetParameter(IndividualParameterString : String; VAR OK : Boolean);
{ Set the parameters }
CONST
  WarnUser = True;

VAR
  Ch : Char;
  ErrorCode : Integer;
  HelpString : String;

BEGIN
  OK := True;
  Ch := IndividualParameterString[1];
  HelpString := 'RAIL [options]';

  IF (Ch = '/') OR (Ch = '-') AND (Length(IndividualParameterString) > 1) THEN BEGIN
    { advance beyond the '/' or '-' }
    IndividualParameterString := Copy(UpperCase(IndividualParameterString), 2, 255);
    Ch := IndividualParameterString[1];
    CASE Ch OF
      '?':
        { List all the options here **** }
        WriteLn('Test');
      'A':
        IF IndividualParameterString = 'A-' THEN
          SetMode(AnonymousOccupation, TurnOff)
        ELSE
          IF IndividualParameterString = 'ARD' THEN
          { used for development }
            SetMode(AllRouteDebugging, TurnOn)
          ELSE
            IF IndividualParameterString = 'ATCM' THEN BEGIN
              SetMode(ShowAdjacentTrackCircuit, TurnOn);
              Debug('Displaying adjacent track circuit mode = ON');
            END ELSE
              OK := False;
      'C':
        IF IndividualParameterString = 'CONNECTION=ETHERNET' THEN
          DesiredLenzConnection := EthernetConnection
        ELSE
          IF IndividualParameterString = 'CONNECTION=USB' THEN
            DesiredLenzConnection := USBConnection
          ELSE
            IF Copy(IndividualParameterString, 1, Length('CONNECTION')) = 'CONNECTION' THEN BEGIN
              DesiredLenzConnection := NoConnection;
              Log('XG Invalid connection type: ' + IndividualParameterString);
            END;
      'D':
        IF IndividualParameterString = 'D-' THEN
          StartWithDiagrams := False
        ELSE
          IF IndividualParameterString = 'D+' THEN BEGIN
            IF WorkingTimetableMode THEN BEGIN
              Log('XG WorkingTimetable Mode (W+) and DiagramMode (D+) cannot both be set - defaulting to DiagramMode');
              OK := False;
              StartWithDiagrams := True;
              WorkingTimetableMode := False;
            END;
          END ELSE
            IF IndividualParameterString = 'Debug' THEN
              { used for development }
              SetMode(GeneralDebugging, TurnOn)
            ELSE
              OK := False;
      'F':
        IF IndividualParameterString = 'FD' THEN
          SetFeedbackDebuggingModeOn('FeedbackDebuggingMode=ON', NOT ReadOutAdjacentSignalNumber, NOT ReadOutTCInFull, NOT ReadOutTCOnce, NOT ReadOutDecoderNumber)
        ELSE
          IF IndividualParameterString = 'FDS' THEN BEGIN
            SetFeedbackDebuggingModeOn('FeedbackDebuggingMode=ON', NOT ReadOutAdjacentSignalNumber, ReadOutTCInFull, NOT ReadOutTCOnce, NOT ReadOutDecoderNumber)
          END ELSE
            IF IndividualParameterString = 'FSS' THEN
              ScreenMode := FullScreenWithStatusBarMode
            ELSE
              IF IndividualParameterString = 'FS' THEN
                ScreenMode := FullScreenMode
              ELSE
                IF IndividualParameterString = 'FTC=OFF' THEN
                  DisplayFlashingTrackCircuits := False
                ELSE
                  IF IndividualParameterString = 'FTC=ON' THEN
                    DisplayFlashingTrackCircuits := True
                  ELSE
                    OK := False;
      'I':
        IF IndividualParameterString = 'I' THEN
          { used for development }
          SetMode(LineDebugging, TurnOn)
        ELSE
          IF IndividualParameterString = 'IL=OFF' THEN
            CheckForIdenticalLinesInLog := False
          ELSE
            IF IndividualParameterString = 'IL=ON' THEN
              CheckForIdenticalLinesInLog := True
            ELSE
              IF IndividualParameterString = 'INI' THEN
                { read from .ini file rather than the registry }
                ReadFromRegistry := False
              ELSE
                OK := False;
      'K':
        IF IndividualParameterString = 'K' THEN
          { used for development }
          SetMode(LockDebugging, TurnOn);
      'L':
        IF IndividualParameterString = 'L' THEN
          SetMode(Locking, TurnOff)
        ELSE
          { need to alter below to cope with suffixes **** }
          IF Pos('LOGFILE', IndividualParameterString) > 0 THEN BEGIN
            IF Length(IndividualParameterString) > Length('LOGFILE') THEN
              LogFileName := GetFollowingChars(IndividualParameterString, 'LOGFILE', '')
          END ELSE
            OK := False;
      'M':
        IF IndividualParameterString = 'M+' THEN
          { Make menus visible }
          ShowMenus
        ELSE
          IF IndividualParameterString = 'M-' THEN
            { Make menus invisible }
            HideMenus
          ELSE
            IF IndividualParameterString = 'ML' THEN
              MultipleLogFilesRequired := True
            ELSE
              OK := False;
      'N':
        IF IndividualParameterString = 'NOSPLASH' THEN
          { do nothing - it's been dealt with at system initialisation (in rail.pas) }
        ELSE
          IF IndividualParameterString = 'NOLOG' THEN
            { Turn of logging }
            SetMode(LogsCurrentlyKept, False)
          ELSE
            IF IndividualParameterString = 'NOBEEP' THEN
              { Turn of the sound made when bold text appears in the debug window }
              MakeSoundWhenDebugWindowBoldTextAppears := False
            ELSE
              OK := False;
      'O':
        IF NOT ShiftKeyHeldDownOnStartup THEN BEGIN
          { holding the shift key down on startup overrides the offline parameter }
          IF IndividualParameterString = 'OFFLINE' THEN BEGIN
            SystemSetOfflineByCommandLineParameter := True;
            SetSystemOffline('by command line parameter', NOT SoundWarning);
          END ELSE
            IF IndividualParameterString = 'OFFLINEWITHPREVIOUSPOINTSSET' THEN BEGIN
              SystemSetOfflineByCommandLineParameter := True;
              SetSystemOffline('by command line parameter', NOT SoundWarning);
              SetMode(PreviousPointSettings, TurnOn);
            END;
        END;
      'P':
        IF IndividualParameterString = 'PFW:' THEN
          Val(GetFollowingChars(IndividualParameterString, 'PFW:', ''), PointFeedbackMaximumWaitInSeconds, ErrorCode)
        ELSE
          IF IndividualParameterString = 'PD' THEN
            { used for development }
            SetMode(PointDebugging, TurnOn)
          ELSE
            OK := False;
      'R':
        IF IndividualParameterString = 'RDC=ON' THEN BEGIN
          SetMode(RDC, TurnOn)
          // RailDriverWindow.RailDriverWindowTimer.Enabled := True;
        END ELSE
          IF IndividualParameterString = 'RDC=OFF' THEN
            { this is not necessary, but it's easier to turn it on or off in the parameter string this way }
            SetMode(RDC, TurnOff)
          ELSE
            IF IndividualParameterString = 'RD' THEN
              SetMode(RouteDebugging, TurnOn)
            ELSE
              IF IndividualParameterString = 'RS' THEN
                SetMode(RouteDrawing, TurnOn)
              ELSE
                IF IndividualParameterString = 'RB' THEN
                  SetMode(RouteBacktrackDebugging, TurnOn)
                ELSE
                  IF Pos('REPLAYFILE', IndividualParameterString) > 0 THEN BEGIN
                    IF Length(IndividualParameterString) > Length('REPLAYFILE') THEN
                      ReplayFileName := GetFollowingChars(IndividualParameterString, 'REPLAYFILE', '');
                  END ELSE
                    IF IndividualParameterString = 'RUNTESTUNIT' THEN BEGIN
                      RunTestUnitOnStartup := True;
                    END ELSE
                      OK := False;
      'S':
        IF IndividualParameterString = 'S' THEN
          SetMode(StationStart, TurnOn)
        ELSE
          OK := False;
      'T':
        IF IndividualParameterString = 'T=' THEN BEGIN
          IF TimeIsValid(Copy(IndividualParameterString, 3, 255)) THEN BEGIN
            ProgramStartTime := StrToTime(Copy(IndividualParameterString, 3, 255));
            SetCurrentRailwayTimeAndDayOfTheWeek(ProgramStartTime);
          END ELSE
            OK := False;
        END ELSE
          IF IndividualParameterString = 'TR' THEN
            SetMode(RecordingMonitorScreens, TurnOn)
          ELSE
            IF IndividualParameterString = 'TEST' THEN
              SetMode(Testing, TurnOn)
            ELSE
              OK := False;
      'W':
        IF IndividualParameterString = 'W-' THEN
          WorkingTimetableMode := False
        ELSE
          IF IndividualParameterString = 'W+' THEN BEGIN
            IF StartWithDiagrams THEN BEGIN
              Log('XG WorkingTimetable Mode (W+) and DiagramMode (D+) cannot both be set - defaulting to DiagramMode');
              OK := False;
            END ELSE BEGIN
              WorkingTimetableMode := True;
              StartWithDiagrams := False;
              IF Copy(IndividualParameterString, 1, 2) = 'W:' THEN BEGIN
                CurrentRailwayDayOfTheWeek := StrToDayOfTheWeek(Copy(IndividualParameterString, 3));
                IF CurrentRailwayDayOfTheWeek = UnknownDayOfTheWeek THEN
                  Log('XG Unknown day of the week in parameter ' + IndividualParameterString)
                ELSE
                  { Write out the current time now to include the day of the week }
                  SetCurrentRailwayTimeAndDayOfTheWeek(CurrentRailwayTime);
              END;
            END;
          END;
      'X':
        IF IndividualParameterString = 'X' THEN
          { used for development }
          ShowCreateRouteExitFunctionNum := True;
      'Y':
        IF IndividualParameterString = 'Y:' THEN BEGIN
          IF Length(IndividualParameterString) > Length('Y:') THEN BEGIN
            ShowByteParam := GetFollowingChars(IndividualParameterString, 'Y:', '');
            IF (ShowByteParam = 'ALL')
            OR ((ShowByteParam >= '0') AND (ShowByteParam <= '9'))
            THEN
              OK := False;
          END;
        END ELSE
          OK := False;
      ';', '!':
        { anything preceded by ";" or "!" is ok - it allows commenting out of parameters }
        OK := True;
    ELSE {CASE}
      OK := False;
    END; {CASE}
  END ELSE
    OK := False;
END; { SetParameter }

PROCEDURE HandleParameters;
{ Process user parameters, from environment and command line }
VAR
  I : Integer;
  OK, Done : Boolean;
  Parameter : String;
  RailEnvString : String;
  TempStr : String;

BEGIN
  OK := True;

  { Look at the environment string "Set Rail=..." }
  RailEnvString := GetEnvironmentVariable('RAIL');
  Done := False;
  REPEAT
    GetWord(RailEnvString, Parameter);
    IF Parameter = '' THEN
      Done := True
    ELSE BEGIN
      SetParameter(Parameter, OK);
      IF NOT OK THEN
        Log('XG Invalid parameter in environment string: ' + Parameter);
    END;
  UNTIL Done;

  IF ParamCount = 0 THEN BEGIN
    { Read the parameters in from the .ini file - it may well be that we are doing a programmed restart, which doesn't by its nature have parameters }
    IF CurrentParametersFromIniFile <> '' THEN BEGIN
      CurrentParametersFromParamStr := CurrentParametersFromIniFile;
      TempStr := CurrentParametersFromIniFile;
      Done := False;
      REPEAT
        GetWord(TempStr, Parameter);
        IF Parameter = '' THEN
          Done := True
        ELSE BEGIN
          SetParameter(Parameter, OK);
          IF NOT OK THEN
            Log('XG Invalid parameter in environment string: ' + Parameter);
        END;
      UNTIL Done;
    END;
  END ELSE BEGIN
    { otherwise look at the parameters given on the run command }
    FOR I := 1 TO ParamCount DO BEGIN
      SetParameter(ParamStr(I), OK);
      IF NOT OK THEN
        Log('XG Invalid command-line parameter: ' + ParamStr(I))
      ELSE
        CurrentParametersFromParamStr := CurrentParametersFromParamStr + ParamStr(I) + ' ';
    END;
  END;

  Log('A Startup Detail:');

  IF InAllRouteDebuggingMode THEN
    Log('AG All Route Debugging Mode ON {INDENT=2}');

  IF NOT InAnonymousOccupationMode THEN
    Log('AG Anonymous Occupation Mode OFF {INDENT=2}');

  IF InDebuggingMode THEN
    Log('AG Debugging Mode ON {INDENT=2}');

  IF InFeedbackDebuggingMode THEN
    Log('AG Feedback Debugging Mode ON {INDENT=2}');

  IF InLineDebuggingMode THEN
    Log('AG Line Debugging Mode ON {INDENT=2}');

  IF InLockDebuggingMode THEN
    Log('AG Lock Debugging Mode ON {INDENT=2}');

  IF NOT InLockingMode THEN
    Log('AG Locking State OFF {INDENT=2}');

  IF InLocoSpeedTimingMode THEN
    Log('AG Loco Speed Timing ON {INDENT=2}');

  IF NOT InLogsCurrentlyKeptMode THEN
    Log('AG LogsCurrentlyKept is OFF {INDENT=2}');

  IF LogCurrentTimeMode THEN
    Log('AG LogCurrentTimeMode is ON {INDENT=2}');

  IF NOT MakeSoundWhenDebugWindowBoldTextAppears THEN
    Log('AG Make Sound When Debug Window Bold Text Appears OFF {INDENT=2}');

  IF InRDCMode THEN
    Log('AG RDC Mode ON {INDENT=2}');

  IF ReadOutTCInFull THEN
    Log('AG Read Out TC In Full ON {INDENT=2}')
  ELSE
    IF ReadOutTCOnce THEN
      Log('AG Read Out TC Once ON {INDENT=2}')
    ELSE
      IF ReadOutDecoderNumber THEN
        Log('AG Read Out Decoder Number OFF {INDENT=2}')
      ELSE
        IF ReadOutAdjacentSignalNumber THEN
          Log('AG Read Out Adjacent Signal Number OFF {INDENT=2}');

  IF InRecordingMonitorScreensMode THEN
    Log('AG RecordingMonitorScreensMode is ON {INDENT=2}');

  IF NOT InRecordLineDrawingMode THEN
    Log('AG Record Line Drawing Mode OFF {INDENT=2}');

  IF InRouteBacktrackDebuggingMode THEN
    Log('AG Route Backtrack Debugging Mode ON {INDENT=2}');

  IF InRouteDebuggingMode THEN
    Log('AG Route Debugging Mode ON {INDENT=2}');

  IF InRouteDrawingMode THEN
    Log('AG Route Drawing Mode ON {INDENT=2}');

  IF InStationStartMode THEN
    Log('AG Station Start Mode ON {INDENT=2}');

  IF InTestingMode THEN
    Log('AG Testing Mode ON {INDENT=2}');

  { Let the user know if the system is offline or in some other odd state at startup }
  IF SystemStatusStr <> '' THEN BEGIN
    Debug(UpperCase(SystemStatusStr));
    SystemStatusStr := '';
  END;
END; { HandleParameters }

PROCEDURE TDebuggingOptionsWindow.Startup_DebuggingCheckBoxClick(Sender: TObject);
BEGIN
  IF Startup_DebuggingCheckBox.Checked THEN
    SetMode(GeneralDebugging, TurnOn)
  ELSE
    SetMode(GeneralDebugging, TurnOff);
END; { Startup_DebuggingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_FeedbackDebuggingCheckBoxClick(Sender: TObject);
BEGIN
  IF Startup_FeedbackDebuggingCheckBox.Checked THEN
    SetFeedbackDebuggingModeOn('Feedback debugging = ON',
                               NOT ReadOutAdjacentSignalNumber, NOT ReadOutTCInFull, ReadOutTCOnce, NOT ReadOutDecoderNumber)
  ELSE
    SetFeedbackDebuggingModeOff('Feedback debugging = OFF')
END; { Startup_FeedbackDebuggingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_LineDebuggingCheckBoxClick(Sender: TObject);
BEGIN
  IF NOT Startup_LineDebuggingCheckBox.Checked THEN
    SetMode(LineDebugging, TurnOff)
  ELSE BEGIN
    SetMode(LineDebugging, TurnOn);

    { mutually exclusive, as it gets v. confusing! }
    SetMode(LockDebugging, TurnOff);
    Startup_LockDebuggingCheckBox.State := cbUnchecked;

    SetMode(PointDebugging, TurnOff);
    Startup_PointDebuggingCheckBox.State := cbUnchecked;
  END;
END; { Startup_LineDebuggingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_LockDebuggingCheckBoxClick(Sender: TObject);
BEGIN
  IF NOT Startup_LockDebuggingCheckBox.Checked THEN
    SetMode(LockDebugging, TurnOff)
  ELSE BEGIN
    SetMode(LockDebugging, TurnOn);

    { mutually exclusive, as it gets v. confusing! }
    SetMode(LineDebugging, TurnOff);
    Startup_LineDebuggingCheckBox.State := cbUnchecked;

    SetMode(PointDebugging, TurnOff);
    Startup_PointDebuggingCheckBox.State := cbUnchecked;
  END;
END; { Startup_LockDebuggingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_PointDebuggingCheckBoxClick(Sender: TObject);
BEGIN
  IF NOT Startup_PointDebuggingCheckBox.Checked THEN
    SetMode(PointDebugging, TurnOff)
  ELSE BEGIN
    SetMode(PointDebugging, TurnOn);

    { mutually exclusive, as it gets v. confusing! }
    SetMode(LineDebugging, TurnOff);
    Startup_LineDebuggingCheckBox.State := cbUnchecked;

    SetMode(LockDebugging, TurnOff);
    Startup_LockDebuggingCheckBox.State := cbUnchecked;
  END;
END; { Startup_PointDebuggingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_RouteDebuggingCheckBoxClick(Sender: TObject);
BEGIN
  IF Startup_RouteDebuggingCheckBox.Checked THEN
    SetMode(RouteDebugging, TurnOn)
  ELSE
    SetMode(RouteDebugging, TurnOff);
END; { Startup_RouteDebuggingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_RouteBacktrackDebuggingCheckBoxClick(Sender: TObject);
BEGIN
  IF Startup_RouteBacktrackDebuggingCheckBox.Checked THEN
    SetMode(RouteBacktrackDebugging, TurnOn)
  ELSE
    SetMode(RouteBacktrackDebugging, TurnOff);
END; { Startup_RouteBacktrackDebuggingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_AllRouteDebuggingCheckBoxClick(Sender: TObject);
BEGIN
  IF Startup_AllRouteDebuggingCheckBox.Checked THEN
    SetMode(AllRouteDebugging, TurnOn)
  ELSE
    SetMode(AllRouteDebugging, TurnOff);
END; { Startup_AllRouteDebuggingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_RouteDrawingCheckBoxClick(Sender: TObject);
BEGIN
  IF Startup_RouteDrawingCheckBox.Checked THEN
    SetMode(RouteDrawing, TurnOn)
  ELSE
    SetMode(RouteDrawing, TurnOff);
END; { Startup_RouteDrawingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_TestingCheckBoxClick(Sender: TObject);
BEGIN
  IF Startup_TestingCheckBox.Checked THEN
    SetMode(Testing, TurnOn)
  ELSE
    SetMode(Testing, TurnOff);
END; { Startup_TestingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_RecordLineDrawingCheckBoxClick(Sender: TObject);
BEGIN
  IF Startup_RecordLineDrawingCheckBox.Checked THEN
    SetMode(RecordLineDrawing, TurnOn)
  ELSE
    SetMode(RecordLineDrawing, TurnOff);
END; { Startup_RecordLineDrawingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_LockingCheckBoxClick(Sender: TObject);
BEGIN
  IF Startup_LockingCheckBox.Checked THEN
    SetMode(Locking, TurnOff)
  ELSE
    SetMode(Locking, TurnOn);
END; { Startup_LockingCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.Startup_LogsKeptCheckBoxClick(Sender: TObject);
BEGIN
  WITH Startup_LogsKeptCheckBox DO BEGIN
    IF Checked THEN BEGIN
      SetMode(LogsCurrentlyKept, True);
      Log('A Logs Kept= ON');
    END ELSE BEGIN
      SetMode(LogsCurrentlyKept, False);
      Log('A Logs Kept = OFF');
    END;
  END; {WITH}
END; { LogsKeptCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.DebuggingOptionsWindowShow(Sender: TObject);
BEGIN
  Left := DebuggingOptionsWindowLeft;
  Top := DebuggingOptionsWindowTop;

  IF InAllRouteDebuggingMode THEN
    Startup_AllRouteDebuggingCheckBox.State := cbChecked;
  IF InDebuggingMode THEN
    Startup_DebuggingCheckBox.State := cbChecked;
  IF InFeedbackDebuggingMode THEN
    Startup_FeedbackDebuggingCheckBox.State := cbChecked;
  IF InLineDebuggingMode THEN
    Startup_LineDebuggingCheckBox.State := cbChecked;
  IF InLockDebuggingMode THEN
    Startup_LockDebuggingCheckBox.State := cbChecked;
  IF NOT InLockingMode THEN
    Startup_LockingCheckBox.State := cbChecked;
  IF InLogsCurrentlyKeptMode THEN
    Startup_LogsKeptCheckBox.State := cbChecked;
  IF InPointDebuggingMode THEN
    Startup_PointDebuggingCheckBox.State := cbChecked;
  IF InRecordLineDrawingMode THEN
    Startup_RecordLineDrawingCheckBox.State := cbChecked;
  IF InRouteDebuggingMode THEN
    Startup_RouteDebuggingCheckBox.State := cbChecked;
  IF InRouteBacktrackDebuggingMode THEN
    Startup_RouteBacktrackDebuggingCheckBox.State := cbChecked;
  IF InRouteDrawingMode THEN
    Startup_RouteDrawingCheckBox.State := cbChecked;
  IF InTestingMode THEN
    Startup_TestingCheckBox.State := cbChecked;

  { Save where the mouse cursor is }
  SaveMouseCursorPos := Mouse.CursorPos;
  { And move it to the debug window }
  Mouse.CursorPos := Point(DebuggingOptionsWindow.Left + (DebuggingOptionsWindow.Width DIV 2),
                           DebuggingOptionsWindow.Top + (DebuggingOptionsWindow.Height DIV 2));
END; { DebuggingOptionsWindowShow }

PROCEDURE TDebuggingOptionsWindow.DebuggingOptionsWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    vk_Escape, vk_Return:
      DebuggingOptionsWindow.Hide;
  END; {CASE}
END; { DebuggingOptionsWindowKeyDown }

PROCEDURE TDebuggingOptionsWindow.DebuggingOptionsWindowHide(Sender: TObject);
BEGIN
  DebuggingOptionsWindowLeft := Left;
  DebuggingOptionsWindowTop := Top;
  { Restore the mouse cursor to its original position only if it's still within the debug window when we hide the window }
  IF PtInRect(Rect(DebuggingOptionsWindow.Left,
                   DebuggingOptionsWindow.Top,
                   DebuggingOptionsWindow.Left + Width,
                   DebuggingOptionsWindow.Top + Height),
              Mouse.CursorPos)
  THEN
    Mouse.CursorPos := SaveMouseCursorPos;
END; { DebuggingOptionsWindowHide }

PROCEDURE LoadIcons;
{ The icons are held in the system resource file, itself compiled by using "brcc32 -v rail.rc" from the command prompt. The file "rail.rc" is the resource script file. }
BEGIN
  EditIcon := TIcon.Create;
  OnlineIcon := TIcon.Create;
  OfflineIcon := TIcon.Create;

  EditIcon.Handle := LoadIcon(hInstance, 'EditIcon');
  OnlineIcon.Handle := LoadIcon(hInstance, 'OnlineIcon');
  OfflineIcon.Handle := LoadIcon(hInstance, 'OfflineIcon');
END; { LoadIcons }

PROCEDURE InitialiseStartupUnit;
{ Such routines as this allow us to initialises the units in the order we wish }
BEGIN
  { Deal with any user-provided parameters }
  IF ShiftKeyHeldDownOnStartup THEN
    Log('AG ShiftKeyHeldDownOnStartup = TRUE');

  HandleParameters;
  IF FWPRailWindow.FWPRailWindowStatusBar.Panels[StatusBarPanel0].Text = '' THEN
    WriteToStatusBarPanel(StatusBarPanel0, TimeToHMSStr(CurrentRailwayTime));

  Log('A Startup unit initialised');
END; { InitialiseStartupUnit }

INITIALIZATION

LoadIcons;

END { StartUp }.
