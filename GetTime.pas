UNIT GetTime;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, InitVars;

TYPE
  TClockWindow = CLASS(TForm)
    CancelButton: TButton;
    Clock: TDateTimePicker;
    GetTimeTimer: TTimer;
    OKButton: TButton;
    PROCEDURE ClockWindowMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
    PROCEDURE ClockWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE CancelButtonClick(Sender: TObject);
    PROCEDURE GetTimeTimerTick(Sender: TObject);
    PROCEDURE OKButtonClick(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  CurrentRailwayTime : TDateTime;
  CurrentRailwayTimeStr : String;
  RailwayTimeInterval : RailwayTimeIntervalType;

FUNCTION DayTime : Boolean;
{ Returns True if it's daytime, arbitrarily set between 8 am and 6 pm }

PROCEDURE InitialiseGetTimeUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

PROCEDURE RunTime(TimeSpeed : Integer);
{ Alter the number of ticks per second }

PROCEDURE SetRailwayTimeInterval(Interval : RailwayTimeIntervalType);
{ Sets the number of milliseconds required for a second }

PROCEDURE SetCurrentRailwayTimeAndDayOfTheWeek(TimeToSet : TDateTime);
{ Initialise the time and day of the week }

VAR
  ClockWindow: TClockWindow;
  SaveWheelTime : TDateTime = 0;

IMPLEMENTATION

{$R *.dfm}

USES RailDraw, IDGlobal, MiscUtils, Input, DateUtils, LocoUtils, StationMonitors, LocationData, Options;

CONST
  UnitRef = 'Time';
  ForceDisplay = True;

VAR
  ActualMilliSecondsPerSecond : Integer;
  SaveCurrentRailwayTimeStr : String;
  SaveMin : Word = 0;
  SaveRailwayTime : TDateTime = 0;
  WasDayTime : Boolean;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE WriteTimeToStatusBar(OUT TimeStr : String);
{ Write out the time to the first panel on the status bar }
VAR
  CurrentRailwayDayOfTheWeekStr : String;

BEGIN
  IF NOT WorkingTimetableMode OR (CurrentRailwayDayOfTheWeek = UnknownDayOfTheWeek) THEN
    TimeStr := CurrentRailwayTimeStr
  ELSE BEGIN
    IF CurrentRailwayDayOfTheWeek = Monday THEN
      CurrentRailwayDayOfTheWeekStr := 'Mon'
    ELSE
      IF CurrentRailwayDayOfTheWeek = Tuesday THEN
        CurrentRailwayDayOfTheWeekStr := 'Tue'
      ELSE
        IF CurrentRailwayDayOfTheWeek = Wednesday THEN
          CurrentRailwayDayOfTheWeekStr := 'Wed'
        ELSE
          IF CurrentRailwayDayOfTheWeek = Thursday THEN
            CurrentRailwayDayOfTheWeekStr := 'Thu'
          ELSE
            IF CurrentRailwayDayOfTheWeek = Friday THEN
              CurrentRailwayDayOfTheWeekStr := 'Fri'
            ELSE
              IF CurrentRailwayDayOfTheWeek = Saturday THEN
                CurrentRailwayDayOfTheWeekStr := 'Sat'
              ELSE
                IF CurrentRailwayDayOfTheWeek = Sunday THEN
                  CurrentRailwayDayOfTheWeekStr := 'Sun';

    IF WorkingTimetableMode
    AND (CurrentRailwayDayOfTheWeekStr <> '')
    THEN
      TimeStr := CurrentRailwayDayOfTheWeekStr + ' ' + CurrentRailwayTimeStr;
  END;
  WriteToStatusBarPanel(StatusBarPanel0, TimeStr);
END; { WriteTimeToStatusBar }

PROCEDURE SetCurrentRailwayTimeAndDayOfTheWeek(TimeToSet : TDateTime);
{ Initialise the time }
VAR
  TimeStr : String;

BEGIN
  CurrentRailwayTime := TimeToSet;
  CurrentRailwayTimeStr := TimeToHMSStr(CurrentRailwayTime);
  WasDayTime := DayTime;

  WriteTimeToStatusBar(TimeStr);
  Log('AG Time set to ' + TimeStr);
END; { SetCurrentRailwayTime }

FUNCTION DayTime : Boolean;
{ Returns true if it's daytime, arbitrarily set between 8 am and 6 pm but can be changed by user }
BEGIN
  IF DayTimeSetByUser THEN
    Result := True
  ELSE
    IF (DayLightStartTime > CurrentRailwayTime)
    OR (DayLightEndTime < CurrentRailwayTime)
    OR NightTimeSetByUser
    THEN
      Result := False
    ELSE
      Result := True;
END; { DayTime }

PROCEDURE SetRailwayTimeInterval(Interval: RailwayTimeIntervalType);
{ Sets the number of milliseconds required for a second }
BEGIN
  CASE Interval OF
    Normal:
      BEGIN
        RailwayTimeInterval := Normal;
        ClockWindow.GetTimeTimer.Interval := 220;
        IF NOT ClockWindow.GetTimeTimer.Enabled THEN
          Log('AG Time now running normally')
        ELSE
          Log('AG Time set to run normally');
      END;
    Slower:
      BEGIN
        { one railway second = one actual second }
        RailwayTimeInterval := Slower;
        ClockWindow.GetTimeTimer.Interval := 1000;
        IF NOT ClockWindow.GetTimeTimer.Enabled THEN
          Log('AG Time now running slower')
        ELSE
          Log('AG Time set to run slower');
      END;
    Faster:
      BEGIN
        RailwayTimeInterval := Faster;
        ClockWindow.GetTimeTimer.Interval := 80;
        IF NOT ClockWindow.GetTimeTimer.Enabled THEN
          Log('AG Time now running faster')
        ELSE
          Log('AG Time set to run faster');
      END;
    Fastest:
      BEGIN
        RailwayTimeInterval := Fastest;
        ClockWindow.GetTimeTimer.Interval := 1;
        IF NOT ClockWindow.GetTimeTimer.Enabled THEN
          Log('AG Time now running at its fastest')
        ELSE
          Log('AG Time set to run at its fastest');
      END;
  END; {CASE}
END; { SetRailwayTimeInterval }

PROCEDURE RunTime(TimeSpeed : Integer);
{ Alter the number of ticks per second }
BEGIN
  Log('X Time now running at ' + IntToStr(TimeSpeed) + ' tps');
  ActualMilliSecondsPerSecond := TimeSpeed;
END; { RunTime }

PROCEDURE TClockWindow.ClockWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      ClockWindow.Visible := False;
  END; {CASE}
END; { ClockWindowKeyDown }

PROCEDURE TClockWindow.CancelButtonClick(Sender: TObject);
BEGIN
  ClockWindow.Visible := False;
END; { CancelButtonClick }

PROCEDURE TClockWindow.OKButtonClick(Sender: TObject);
{ Set various times. Note that there is a bug in the TDateTime Picker in that input of 00:00:00 causes an exception }
BEGIN
  TRY
    IF Caption = SetCurrentRailwayTimeCaption THEN BEGIN
      SetCurrentRailwayTimeAndDayOfTheWeek(ClockWindow.Clock.Time);
      Log('A Current railway time set in ClockWindow to ' + TimeToHMStr(CurrentRailwayTime));
      ClockWindow.Visible := False;
    END ELSE
      IF Caption = SetProgramStartTimeCaption THEN BEGIN
        ProgramStartTime := ClockWindow.Clock.Time;
        Log('A Program Start Time set in ClockWindow to ' + TimeToHMStr(ProgramStartTime));
        { If the clock hasn't started running yet, adjust the current program start time }
        IF NOT AutoModeInitiated THEN BEGIN
          SetCurrentRailwayTimeAndDayOfTheWeek(ProgramStartTime);
          Log('AG New Program Start Time adopted as Auto Mode has not yet been initiated');
        END;
        ClockWindow.Visible := False;
      END ELSE
        IF Caption = SetDaylightStartTimeCaption THEN BEGIN
          DaylightStartTimeStr := TimeToHMSStr(ClockWindow.Clock.Time);
          Log('A Daylight Start Time set in ClockWindow to ' + DaylightStartTimeStr);
          ClockWindow.Visible := False;
        END ELSE
          IF Caption = SetDaylightEndTimeCaption THEN BEGIN
            { test to see if this is earlier then the daylight start time }
            IF StrToTime(DaylightStartTimeStr) > ClockWindow.Clock.Time THEN BEGIN
              ShowMessage('Daylight End Time "' + TimeToHMSStr(ClockWindow.Clock.Time) + '" cannot be earlier than Daylight Start Time "' + DaylightStartTimeStr + '"');
              Log('A Daylight Start Time "' + TimeToHMSStr(ClockWindow.Clock.Time) + ' cannot be earlier than Program Start Time "' + TimeToHMSStr(ProgramStartTime));
            END ELSE BEGIN
              DaylightEndTimeStr := TimeToHMSStr(ClockWindow.Clock.Time);
              Log('A Daylight End Time set in ClockWindow to ' + DaylightEndTimeStr);
              ClockWindow.Visible := False;
            END;
          END;
    SetUpStationMonitors([], StationMonitorsCurrentDisplayOrderNum);
    DrawStationMonitorsWindow(StationMonitorsCurrentArea);
  EXCEPT
    ON E : Exception DO
      Log('EG OKButtonClick: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { OKButtonClick }

PROCEDURE TClockWindow.GetTimeTimerTick(Sender: TObject);
VAR
  OK : Boolean;
  T : Train;
  TimeStr : String;

BEGIN
  CurrentRailwayTime := IncSecond(CurrentRailwayTime);
  IF SaveRailwayTime = 0 THEN
    SaveRailwayTime := CurrentRailwayTime;
  CurrentRailwayTimeStr := TimeToHMSStr(CurrentRailwayTime);

  IF SaveCurrentRailwayTimeStr <> CurrentRailwayTimeStr THEN BEGIN
    { Need to see if night turns to day (or vice versa) - may affect loco lights }
    IF (DayTime <> WasDayTime) OR DayTimeSetByUser OR NightTimeSetByUser THEN BEGIN
      T := TrainList;
      WHILE T <> NIL DO BEGIN
        IF T^.Train_DiagramFound THEN BEGIN
          IF T^.Train_LightsType = ExpressModelsSeparateHeadlights THEN BEGIN
            IF T^.Train_LightsOn THEN BEGIN
              { turning lights back on adjusts them to the new time }
              TurnLightsOff(T^.Train_LocoChip);
              TurnLightsOn(T^.Train_LocoChip, OK);
            END;
          END ELSE
            IF T^.Train_LightsType = CustomLightingKit THEN BEGIN
              IF T^.Train_LightsOn THEN BEGIN
                { turning lights back on adjusts them to the new time }

              END;
            END;
        END;
        T := T^.Train_NextRecord;
      END; {WHILE}
      IF DayTimeSetByUser THEN BEGIN
        DayTimeSetByUser := False;
        Log('AG DayTimeSetByUser now off');
      END ELSE
        IF NightTimeSetByUser THEN BEGIN
          NightTimeSetByUser := False;
          Log('AG NightTimeSetByUser now off');
        END;
    END;

    { Write the new time to the screen }
    IF (StationMonitorDisplay = StationClockDisplay)
    OR (Copy(SaveCurrentRailwayTimeStr, 1, 5) <> Copy(CurrentRailwayTimeStr, 1, 5))
    THEN BEGIN
      DrawStationMonitorsWindow(StationMonitorsCurrentArea);
      IF LocationDataWindow.Visible THEN
        LocationDataWindow.Caption := 'LocationDataWindow (' + CurrentRailwayTimeStr + ')';
    END;

    WriteTimeToStatusBar(TimeStr);

    SaveCurrentRailwayTimeStr := CurrentRailwayTimeStr;
    WasDayTime := DayTime;
  END;
END; { GetTimeTimerTick }

PROCEDURE TClockWindow.ClockWindowMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
{ Adjust speed by means of the mouse wheel - slow down the reading, though }
VAR
  Hour, Min, Sec, MSec: Word;

BEGIN
  TRY
    IF ClockWindow.Active THEN BEGIN
      DecodeTime(ClockWindow.Clock.Time, Hour, Min, Sec, MSec);

      { If there's a small movement of the wheel (usually WheelDelta = 120 or -120), reduce the amount of speed increase/decrease }
      IF (WheelDelta > -240)
      AND (WheelDelta < 240)
      THEN
        IF MilliSecondsBetween(Time, SaveWheelTime) < 200 THEN
          Exit;

      SaveWheelTime := Time;

      IF WheelDelta > 0 THEN
        Inc(Min)
      ELSE
        IF WheelDelta < 0 THEN
          Dec(Min);

      ClockWindow.Clock.Time := EncodeTime(Hour, Min, Sec, MSec);
      DrawStationMonitorsWindow(StationMonitorsCurrentArea);
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG ControlTimeByMouseWheel: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ControlTimeByMouseWheel }

PROCEDURE InitialiseGetTimeUnit;
{ Such routines as this allow us to initialises the units in the order we wish }
BEGIN
  SaveCurrentRailwayTimeStr := StringOfChar(' ', 11);
  WasDayTime := DayTime;

  Log('A GetTime unit initialised');
END; { InitialiseGetTimeUnit }

INITIALIZATION

END { GetTime }.
