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

FUNCTION GetAllRouteDebuggingMode : Boolean;
{ Return the status of AllRouteDebuggingMode }

PROCEDURE InitialiseStartupUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

IMPLEMENTATION

{$R *.dfm}

USES LocoUtils, MiscUtils, Input, RailDraw, RDCUnit, DateUtils, Feedback, CreateRoute, Diagrams, StrUtils, LocationData, Options;

CONST
  UnitRef = 'Startup';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION GetAllRouteDebuggingMode : Boolean;
{ Return the status of AllRouteDebuggingMode }
BEGIN
  Result := AllRouteDebuggingMode;
END; { GetAllRouteDebuggingMode }

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

PROCEDURE SetParameter(ParamString : String; VAR OK : Boolean);
{ Set the parameters }
CONST
  WarnUser = True;

VAR
  Ch : Char;
  ErrorCode : Integer;
  HelpString : String;

BEGIN
  OK := True;
  Ch := ParamString[1];
  HelpString := 'RAIL [options]';

  IF (Ch = '/') OR (Ch = '-') AND (Length(ParamString) > 1) THEN BEGIN
    { advance beyond the '/' or '-' }
    ParamString := Copy(UpperCase(ParamString), 2, 255);
    Ch := ParamString[1];
    CASE Ch OF
      '?':
        { List all the options here **** }
        WriteLn('Test');
      'A':
        IF ParamString = 'A-' THEN
          SetMode(AnonymousOccupation, TurnOff)
        ELSE
          IF ParamString = 'ARD' THEN
          { used for development }
            SetMode(AllRouteDebugging, TurnOn)
          ELSE
            IF ParamString = 'ATCM' THEN BEGIN
              SetMode(ShowAdjacentTrackCircuit, TurnOn);
              Debug('Displaying adjacent track circuit mode = ON');
            END ELSE
              OK := False;
      'C':
        IF ParamString = 'CONNECTION=ETHERNET' THEN
          DesiredLenzConnection := EthernetConnection
        ELSE
          IF ParamString = 'CONNECTION=USB' THEN
            DesiredLenzConnection := USBConnection
          ELSE
            IF Copy(ParamString, 1, Length('CONNECTION')) = 'CONNECTION' THEN BEGIN
              DesiredLenzConnection := NoConnection;
              Log('XG Invalid connection type: ' + ParamString);
            END;
      'D':
        IF Copy(ParamString, 1, 2) = 'D-' THEN
          StartWithDiagrams := False
        ELSE
          IF Copy(ParamString, 1, 2) = 'D+' THEN BEGIN
            IF WorkingTimetableMode THEN BEGIN
              Log('XG WorkingTimetable Mode (W+) and DiagramMode (D+) cannot both be set - defaulting to DiagramMode');
              OK := False;
              StartWithDiagrams := True;
              WorkingTimetableMode := False;
            END;
          END ELSE
            IF ParamString = 'Debug' THEN
              { used for development }
              SetMode(GeneralDebugging, TurnOn)
            ELSE
              OK := False;
      'F':
        IF ParamString = 'FD' THEN
          SetFeedbackDebuggingModeOn('FeedbackDebuggingMode=ON', NOT ReadOutAdjacentSignalNumber, NOT ReadOutTCInFull, NOT ReadOutTCOnce, NOT ReadOutDecoderNumber)
        ELSE
          IF ParamString = 'FDS' THEN BEGIN
            SetFeedbackDebuggingModeOn('FeedbackDebuggingMode=ON', NOT ReadOutAdjacentSignalNumber, ReadOutTCInFull, NOT ReadOutTCOnce, NOT ReadOutDecoderNumber)
          END ELSE
            IF ParamString = 'FSS' THEN
              ScreenMode := FullScreenWithStatusBarMode
            ELSE
              IF ParamString = 'FS' THEN
                ScreenMode := FullScreenMode
              ELSE
                IF ParamString = 'FTC=OFF' THEN
                  DisplayFlashingTrackCircuits := False
                ELSE
                  IF ParamString = 'FTC=ON' THEN
                    DisplayFlashingTrackCircuits := True
                  ELSE
                    OK := False;
      'I':
        IF ParamString = 'I' THEN
          { used for development }
          SetMode(LineDebugging, TurnOn)
        ELSE
          IF ParamString = 'IL=OFF' THEN
            CheckForIdenticalLinesInLog := False
          ELSE
            IF ParamString = 'IL=ON' THEN
              CheckForIdenticalLinesInLog := True
            ELSE
              OK := False;
      'K':
        IF ParamString = 'O' THEN
          { used for development }
          SetMode(LockDebugging, TurnOn);
      'L':
        IF ParamString = 'L' THEN
          SetMode(Locking, TurnOff)
        ELSE
          { need to alter below to cope with suffixes **** }
          IF Copy(ParamString, 1, 7) = 'LOGFILE' THEN BEGIN
            IF Length(ParamString) > 8 THEN
              LogFileName := Copy(ParamString, 9, 255)
          END ELSE
            OK := False;
      'M':
        IF ParamString = 'M+' THEN
          { Make menus visible }
          ShowMenus
        ELSE
          IF ParamString = 'M-' THEN
            { Make menus invisible }
            HideMenus
          ELSE
            IF ParamString = 'ML' THEN
              MultipleLogFilesRequired := True
            ELSE
              OK := False;
      'N':
         IF Copy(ParamString, 1, 8) = 'NOSPLASH' THEN
           { do nothing - it's been dealt with at system initialisation (in rail.pas) }
         ELSE
           IF Copy(ParamString, 1, 5) = 'NOLOG' THEN
             { Turn of logging }
             LogsCurrentlyKept := False
           ELSE
             IF Copy(ParamString, 1, 6) = 'NOBEEP' THEN
               { Turn of the sound made when bold text appears in the debug window }
               MakeSoundWhenDebugWindowBoldTextAppears := False
             ELSE
               OK := False;
      'O':
         IF Copy(ParamString, 1, 7) = 'OFFLINE' THEN BEGIN
           SystemSetOfflineByCommandLineParameter := True;
           SetSystemOffline('by command line parameter', NOT SoundWarning);
         END;
      'P':
        IF Copy(ParamString, 1, 4) = 'PFW:' THEN
          Val(Copy(ParamString, 5, 1), PointFeedbackMaximumWaitInSeconds, ErrorCode)
        ELSE
          IF ParamString = 'PD' THEN
            { used for development }
            SetMode(PointDebugging, TurnOn)
          ELSE
            OK := False;
      'R':
        IF ParamString = 'RDC=ON' THEN BEGIN
          SetMode(RDC, TurnOn)
          // RailDriverWindow.RailDriverWindowTimer.Enabled := True;
        END ELSE
          IF ParamString = 'RDC=OFF' THEN
            { this is not necessary, but it's easier to turn it on or off in the parameter string this way }
            SetMode(RDC, TurnOff)
          ELSE
            IF ParamString = 'RD' THEN
              SetMode(RouteDebugging, TurnOn)
            ELSE
              IF ParamString = 'RS' THEN
                SetMode(RouteDrawing, TurnOn)
              ELSE
                IF ParamString = 'RB' THEN
                  SetMode(RouteBacktrackDebugging, TurnOn)
                ELSE
                  IF Copy(ParamString, 1, 10) = 'REPLAYFILE' THEN BEGIN
                    IF Length(ParamString) > 11 THEN
                      ReplayFileName := Copy(ParamString, 12, 255)
                  END ELSE
                    IF Copy(ParamString, 1, 11) = 'RUNTESTUNIT' THEN BEGIN
                      RunTestUnitOnStartup := True;
                    END ELSE
                      OK := False;
      'S':
        IF ParamString = 'S' THEN
          SetMode(StationStart, TurnOn)
        ELSE
          OK := False;
      'T':
        IF Copy(ParamString, 1, 2) = 'T=' THEN BEGIN
          IF TimeIsValid(Copy(ParamString, 3, 255)) THEN BEGIN
            ProgramStartTime := StrToTime(Copy(ParamString, 3, 255));
            SetCurrentRailwayTimeAndDayOfTheWeek(ProgramStartTime);
          END ELSE
            OK := False;
        END ELSE
          IF ParamString = 'TR' THEN
            SetMode(RecordingMonitorScreens, TurnOn)
          ELSE
            IF ParamString = 'TEST' THEN
              SetMode(Testing, TurnOn)
            ELSE
              OK := False;
      'W':
        IF Copy(ParamString, 1, 2) = 'W-' THEN
          WorkingTimetableMode := False
        ELSE
          IF Copy(ParamString, 1, 2) = 'W+' THEN BEGIN
            IF StartWithDiagrams THEN BEGIN
              Log('XG WorkingTimetable Mode (W+) and DiagramMode (D+) cannot both be set - defaulting to DiagramMode');
              OK := False;
            END ELSE BEGIN
              WorkingTimetableMode := True;
              StartWithDiagrams := False;
              IF Copy(ParamString, 1, 2) = 'W:' THEN BEGIN
                CurrentRailwayDayOfTheWeek := StrToDayOfTheWeek(Copy(ParamString, 3));
                IF CurrentRailwayDayOfTheWeek = UnknownDayOfTheWeek THEN
                  Log('XG Unknown day of the week in parameter ' + ParamString)
                ELSE
                  { Write out the current time now to include the day of the week }
                  SetCurrentRailwayTimeAndDayOfTheWeek(CurrentRailwayTime);
              END;
            END;
          END;
      'X':
        IF ParamString = 'X' THEN
          { used for development }
          ShowCreateRouteExitFunctionNum := True;
      'Y':
        IF Copy(ParamString, 1, 2) = 'Y:' THEN BEGIN
          ShowByteParam := Copy(ParamString, 3, 255);
          IF (ShowByteParam = 'ALL')
          OR ((ShowByteParam >= '0') AND (ShowByteParam <= '9'))
          THEN
            OK := False;
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

  { Now initialise the log files }
  IF LogsCurrentlyKept THEN
    InitialiseLogFiles;

  Log('A Startup Detail:');

  IF AllRouteDebuggingMode THEN
    Log('AG All Route Debugging Mode ON {INDENT=2}');

  IF NOT AnonymousOccupationMode THEN
    Log('AG Anonymous Occupation Mode OFF {INDENT=2}');

  IF DebuggingMode THEN
    Log('AG Debugging Mode ON {INDENT=2}');

  IF FeedbackDebuggingMode THEN
    Log('AG Feedback Debugging Mode ON {INDENT=2}');

  IF LineDebuggingMode THEN
    Log('AG Line Debugging Mode ON {INDENT=2}');

  IF LockDebuggingMode THEN
    Log('AG Lock Debugging Mode ON {INDENT=2}');

  IF NOT LockingMode THEN
    Log('AG Locking State OFF {INDENT=2}');

  IF LocoSpeedTimingMode THEN
    Log('AG Loco Speed Timing ON {INDENT=2}');

  IF NOT LogsCurrentlyKept THEN
    Log('AG LogsCurrentlyKept is OFF {INDENT=2}');

  IF LogCurrentTimeMode THEN
    Log('AG LogCurrentTimeMode is ON {INDENT=2}');

  IF NOT MakeSoundWhenDebugWindowBoldTextAppears THEN
    Log('AG Make Sound When Debug Window Bold Text Appears OFF {INDENT=2}');

  IF RDCMode THEN
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

  IF RecordingMonitorScreensMode THEN
    Log('AG RecordingMonitorScreensMode is ON {INDENT=2}');

  IF NOT RecordLineDrawingMode THEN
    Log('AG Record Line Drawing Mode OFF {INDENT=2}');

  IF RouteBacktrackDebuggingMode THEN
    Log('AG Route Backtrack Debugging Mode ON {INDENT=2}');

  IF RouteDebuggingMode THEN
    Log('AG Route Debugging Mode ON {INDENT=2}');

  IF RouteDrawingMode THEN
    Log('AG Route Drawing Mode ON {INDENT=2}');

  IF StationStartMode THEN
    Log('AG Station Start Mode ON {INDENT=2}');

  IF SystemSetOfflineByCommandLineParameter THEN
    Log('AG System Set Offline By Command Line Parameter ON {INDENT=2}');

  IF TestingMode THEN
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
      LogsCurrentlyKept := True;
      Log('A Logs Kept= ON');
    END ELSE BEGIN
      LogsCurrentlyKept := True;
      Log('A Logs Kept = OFF');
    END;
  END; {WITH}
END; { LogsKeptCheckBoxClick }

PROCEDURE TDebuggingOptionsWindow.DebuggingOptionsWindowShow(Sender: TObject);
BEGIN
  Left := DebuggingOptionsWindowLeft;
  Top := DebuggingOptionsWindowTop;

  IF AllRouteDebuggingMode THEN
    Startup_AllRouteDebuggingCheckBox.State := cbChecked;
  IF DebuggingMode THEN
    Startup_DebuggingCheckBox.State := cbChecked;
  IF FeedbackDebuggingMode THEN
    Startup_FeedbackDebuggingCheckBox.State := cbChecked;
  IF LineDebuggingMode THEN
    Startup_LineDebuggingCheckBox.State := cbChecked;
  IF LockDebuggingMode THEN
    Startup_LockDebuggingCheckBox.State := cbChecked;
  IF NOT LockingMode THEN
    Startup_LockingCheckBox.State := cbChecked;
  IF LogsCurrentlyKept THEN
    Startup_LogsKeptCheckBox.State := cbChecked;
  IF PointDebuggingMode THEN
    Startup_PointDebuggingCheckBox.State := cbChecked;
  IF RecordLineDrawingMode THEN
    Startup_RecordLineDrawingCheckBox.State := cbChecked;
  IF RouteDebuggingMode THEN
    Startup_RouteDebuggingCheckBox.State := cbChecked;
  IF RouteBacktrackDebuggingMode THEN
    Startup_RouteBacktrackDebuggingCheckBox.State := cbChecked;
  IF RouteDrawingMode THEN
    Startup_RouteDrawingCheckBox.State := cbChecked;
  IF TestingMode THEN
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

PROCEDURE InitialiseStartupUnit;
{ Such routines as this allow us to initialises the units in the order we wish }
BEGIN
  { Deal with any user-provided parameters }
  HandleParameters;
  IF FWPRailWindow.FWPRailWindowStatusBar.Panels[StatusBarPanel0].Text = '' THEN
    WriteToStatusBarPanel(StatusBarPanel0, TimeToHMSStr(CurrentRailwayTime));

  Log('A Startup unit initialised');
END; { InitialiseStartupUnit }

INITIALIZATION

END { StartUp }.
