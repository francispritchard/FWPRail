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

FUNCTION IfThenTime{1}(Test : Boolean; Time1, Time2 : TDateTime) : TDateTime; Overload;
{ Returns the first supplied time if the test is true, the second time if not. (The system IfThen routine doesn't work with TDateTime values) }

FUNCTION IfThenTime{2}(Test : Boolean; Time : TDateTime) : TDateTime; Overload;
{ Returns the supplied time if the test is true. (The system IfThen routine doesn't work with TDateTime values) }

PROCEDURE InitialiseGetTimeUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

FUNCTION NewMinutesBetween(CONST AThen, ANow: TDateTime): Int64;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
FUNCTION NewSecondsBetween(CONST AThen, ANow: TDateTime): Integer;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
FUNCTION NewMilliSecondsBetween(CONST AThen, ANow: TDateTime): Int64;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
PROCEDURE NewSetCurrentRailwayTime;
{ Sets the current railway time }

PROCEDURE NewSetDaylightEndTime;
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }

PROCEDURE NewSetDaylightStartTime;
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }

PROCEDURE NewSetProgramStartTime;
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }

FUNCTION RoundTimeToNextWholeMinute(Time : TDateTime) : TDateTime;
{ Round the time to the next whole minute, i.e. 06:30:00 becomes 0G:31:00, but 06:30:20 becomes 06:32:00 }

PROCEDURE RunTime(TimeSpeed : Integer);
{ Alter the number of ticks per second }

FUNCTION SameTimeInHoursAndMinutesOnly(Time1, Time2 : TDateTime) : Boolean;
{ Compares two given times and returns true if they are the same in terms of hours and minutes (the system routine SameTime compares times down to the millisecond level,
  and comparing two times by means of Time1 = Time2 also fails if there is a franction of a difference between them).
}
PROCEDURE SetCurrentRailwayDayOfTheWeek;
{ Set a new day of the week }

PROCEDURE SetCurrentRailwayTimeAndDayOfTheWeek(TimeToSet : TDateTime);
{ Initialise the time and day of the week }

PROCEDURE SetRailwayTimeInterval(Interval : RailwayTimeIntervalType);
{ Sets the number of milliseconds required for a second }

FUNCTION TimeIsValid(TimeStr : String) : Boolean;
{ Checking a given time string - an hour/min separator is permissible. Also ignore a trailing asterisk }

VAR
  ClockWindow: TClockWindow;
  SaveWheelTime : TDateTime = 0;

IMPLEMENTATION

{$R *.dfm}

USES RailDraw, IDGlobal, MiscUtils, Input, DateUtils, LocoUtils, StationMonitors, LocationsUnit, Options, Train, Logging, StrUtils;

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

FUNCTION DateTimeToMilliSeconds(DateTime: TDateTime) : Int64;
{ Converts a TDateTime variable to Int64 milliseconds from 0001-01-01 }
VAR
  TimeStamp : SysUtils.TTimeStamp;

BEGIN
  { Call DateTimeToTimeStamp to convert DateTime to TimeStamp }
  TimeStamp := SysUtils.DateTimeToTimeStamp(DateTime);

  { Multiply and add to complete the conversion }
  Result := Int64(TimeStamp.Date) * MSecsPerDay + TimeStamp.Time;
END; { DateTimeToMilliSeconds }

FUNCTION MilliSecondsToDateTime(MilliSeconds: Int64) : TDateTime;
{ Converts an Int64 milliseconds from 0001-01-01 to TDateTime variable }
VAR
  TimeStamp : SysUtils.TTimeStamp;

BEGIN
  { Divide and mod the milliseconds into the TimeStamp record }
  TimeStamp.Date := MilliSeconds DIV MSecsPerDay;
  TimeStamp.Time := MilliSeconds MOD MSecsPerDay;

  { Call TimeStampToDateTime to complete the conversion }
  Result := SysUtils.TimeStampToDateTime(TimeStamp);
END; { MilliSecondsToDateTime }

FUNCTION NewMinutesBetween(CONST AThen, ANow: TDateTime) : Int64;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
VAR
  nnNow, nnThen: Int64;

  FUNCTION TruncDateTimeToMinutes(DateTime: TDateTime) : LongInt;
  { Truncates a DateTime to minutes from 0001-01-01 }
  VAR
    ms: Int64;

  BEGIN
    ms := DateTimeToMilliSeconds(DateTime);
    Result := ms div (60*1000);
  END; { TruncDateTimeToMinutes }

BEGIN
  { Substitutes DateTimeToTimeStamp and TimeStampToMilliSeconds, TruncDateTimeToMinutes for use of Trunc function. }
  nnNow := TruncDateTimeToMinutes(ANow);
  nnThen := TruncDateTimeToMinutes(AThen);
  Result := nnNow - nnThen;
END; { NewMinutesBetween }

FUNCTION NewSecondsBetween(CONST AThen, ANow: TDateTime): Integer;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
VAR
  nnNow, nnThen: Int64;

  FUNCTION TruncDateTimeToSeconds(DateTime: TDateTime): Int64;
  { Truncates a DateTime to seconds from 0001-01-01 }
  VAR
    ms: Int64;

  BEGIN
    ms := DateTimeToMilliSeconds(DateTime);
    Result := ms div 1000;
  END; { TruncDateTimeToSeconds }

BEGIN
  { Substitutes DateTimeToTimeStamp and TimeStampToMilliSeconds, TruncDateTimeToMinutes for use of Trunc function. }
  nnNow := TruncDateTimeToSeconds(ANow);
  nnThen := TruncDateTimeToSeconds(AThen);
  Result := nnNow - nnThen;
END; { NewSecondsBetween }

FUNCTION NewMilliSecondsBetween(CONST AThen, ANow : TDateTime) : Int64;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
VAR
  nnNow, nnThen: Int64;

  FUNCTION TruncDateTimeToMilliSeconds(DateTime: TDateTime) : Int64;
  { Truncates a DateTime to seconds from 0001-01-01 }
  BEGIN
    Result := DateTimeToMilliSeconds(DateTime);
  END; { TruncDateTimeToSeconds }

BEGIN
  { Substitutes DateTimeToTimeStamp and TimeStampToMilliSeconds, TruncDateTimeToMinutes for use of Trunc function }
  nnNow := TruncDateTimeToMilliSeconds(ANow);
  nnThen := TruncDateTimeToMilliSeconds(AThen);
  Result := nnNow - nnThen;
END; { NewMilliSecondsBetween }

FUNCTION IfThenTime{1}(Test : Boolean; Time1, Time2 : TDateTime) : TDateTime; Overload;
{ Returns the first supplied time if the test is true, the second time if not. (The system IfThen routine doesn't work with TDateTime values) }
BEGIN
  IF Test THEN
    Result := Time1
  ELSE
    Result := Time2;
END; { IfThenTime-1 }

FUNCTION IfThenTime{2}(Test : Boolean; Time : TDateTime) : TDateTime; Overload;
{ Returns the supplied time if the test is true. (The system IfThen routine doesn't work with TDateTime values) }
BEGIN
  IF Test THEN
    Result := Time
  ELSE
    Result := 0;
END; { IfThenTime-2 }

FUNCTION RoundTimeToNextWholeMinute(Time : TDateTime) : TDateTime;
{ Round the time to the next whole minute, i.e. 06:30:00 becomes 06:31:00, but 06:30:20 becomes 06:32:00 }
VAR
  Hour, Min, Sec, mSec: Word;

BEGIN
  DecodeTime(Time, Hour, Min, Sec, mSec);
  IF Sec = 0 THEN BEGIN
    Time := IncMinute(Time, 1);
    DecodeTime(Time, Hour, Min, Sec, mSec);
    mSec := 0;
  END ELSE BEGIN
    Time := IncMinute(Time, 2);
    DecodeTime(Time, Hour, Min, Sec, mSec);
    Sec := 0;
    mSec := 0;
  END;

  Result := EncodeTime(Hour, Min, Sec, mSec);
END; { RoundTimeToNextWholeMinute }

FUNCTION SameTimeInHoursAndMinutesOnly(Time1, Time2 : TDateTime) : Boolean;
{ Compares two given times and returns true if they are the same in terms of hours and minutes (the system routine SameTime compares times down to the millisecond level,
  and comparing two times by means of Time1 = Time2 also fails if there is a fraction of a difference between them).
}
VAR
  Hour1, Min1, Sec1, MSec1: Word;
  Hour2, Min2, Sec2, MSec2: Word;

BEGIN
  Result := False;

  DecodeTime(Time1, Hour1, Min1, Sec1, MSec1);
  DecodeTime(Time2, Hour2, Min2, Sec2, MSec2);

  IF Hour1 = Hour2 THEN
    IF Min1 = Min2 THEN
      Result := True;
END; { SameTimeInHoursAndMinutesOnly }

FUNCTION TimeIsValid(TimeStr : String) : Boolean;
{ Checking a given time string - an hour/min separator is permissible. Also ignore a trailing asterisk }
VAR
  ErrorCode : Integer;
  Hour, Min, Sec, MSec : Word;
  OK : Boolean;

BEGIN
  OK := True;

  Hour := 0;
  Min := 0;
  Sec := 0;
  MSec := 0;

  { Remove any trailing asterisks - there to advise that time string is system created }
  IF RightStr(TimeStr, 1) = '*' THEN
    TimeStr := Copy(TimeStr, 1, Length(TimeStr) - 1);

  IF (Length(TimeStr) <> 5) AND (Length(TimeStr) <> 8) THEN
    OK := False;

  IF OK THEN BEGIN
    Val(Copy(TimeStr, 1, 2), Hour, ErrorCode);
    IF ErrorCode <> 0 THEN
      OK := False;
  END;

  IF OK THEN BEGIN
    Val(Copy(TimeStr, 4, 2), Min, ErrorCode);
    IF ErrorCode <> 0 THEN
      OK := False;
  END;

  IF OK THEN BEGIN
    IF Length(TimeStr) = 8 THEN BEGIN
      Val(Copy(TimeStr, 7, 2), Sec, ErrorCode);
      IF ErrorCode <> 0 THEN
        OK := False;
    END;
  END;

  IF OK THEN
    OK := IsValidTime(Hour, Min, Sec, MSec);

  IF NOT OK THEN
    Debug('Time string "' + TimeStr + '" invalid');

  Result := OK;
END; { TimeIsValid }

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

    IF WorkingTimetableMode AND (CurrentRailwayDayOfTheWeekStr <> '') THEN
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
END; { SetCurrentRailwayTimeAndDayOfTheWeek }

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
  T : TrainIndex;
  TimeStr : String;
  UserMsg : String;

BEGIN
  TRY
    CurrentRailwayTime := IncSecond(CurrentRailwayTime);
    IF SaveRailwayTime = 0 THEN
      SaveRailwayTime := CurrentRailwayTime;
    CurrentRailwayTimeStr := TimeToHMSStr(CurrentRailwayTime);

    IF SaveCurrentRailwayTimeStr <> CurrentRailwayTimeStr THEN BEGIN
      { Need to see if night turns to day (or vice versa) - may affect loco lights }
      IF (DayTime <> WasDayTime) OR DayTimeSetByUser OR NightTimeSetByUser THEN BEGIN
        T := 0;
        WHILE T <= High(Trains) DO BEGIN
          WITH Trains[T] DO BEGIN
            IF Train_DiagramFound THEN BEGIN
              WITH Locos[Train_LocoIndex] DO BEGIN
                IF Locos[Train_LocoIndex].Loco_LightsType = ExpressModelsSeparateHeadlights THEN BEGIN
                  IF Loco_LightsOn THEN BEGIN
                    { turning lights back on adjusts them to the new time }
                    TurnLocoLightsOff(Train_LocoIndex, NOT UserMsgRequired, UserMsg, OK);
                    TurnLocoLightsOn(Train_LocoIndex, NOT NonMovingLoco, NOT LightLoco, NOT UserMsgRequired, UserMsg, OK);
                  END;
                END ELSE
                  IF Loco_LightsType = CustomLightingKit THEN BEGIN
                    IF Loco_LightsOn THEN BEGIN
                      { turning lights back on adjusts them to the new time }

                    END;
                  END;
              END; {WITH}
            END;
            Inc(T);
          END; {WITH}
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
        IF (LocationsUnitWindow <> NIL) AND (LocationsUnitWindow.Visible) THEN
          LocationsUnitWindow.Caption := 'LocationsUnitWindow (' + CurrentRailwayTimeStr + ')';
      END;

      WriteTimeToStatusBar(TimeStr);

      SaveCurrentRailwayTimeStr := CurrentRailwayTimeStr;
      WasDayTime := DayTime;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG GetTimeTimerTick: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
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
      IF (WheelDelta > -240) AND (WheelDelta < 240) THEN
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
END; { ClockWindowMouseWheel }

PROCEDURE SetCurrentRailwayDayOfTheWeek;
{ Set a new day of the week }
VAR
  DefaultStr : String;
  NextDayOfTheWeek : DayOfTheWeekType;
  Str : String;
  UpperCaseStr : String;

BEGIN
  NextDayOfTheWeek := GetNextDayOfTheWeek(CurrentRailwayDayOfTheWeek);
  DefaultStr := DayOfTheWeekToStr(NextDayOfTheWeek);

  Str := InputBox('New Day of the Week', 'Enter new day of the week', DefaultStr);
  UpperCaseStr := UpperCase(Str);

  CurrentRailwayDayOfTheWeek := StrToDayOfTheWeek(UpperCaseStr);
  IF CurrentRailwayDayOfTheWeek <> UnknownDayOfTheWeek THEN
    SetCurrentRailwayTimeAndDayOfTheWeek(CurrentRailwayTime)
  ELSE
    Debug('!"' + Str + '" is an invalid day of the week');
END; { SetCurrentRailwayDayOfTheWeekClick }

PROCEDURE NewSetCurrentRailwayTime;
{ Sets the current railway time }
BEGIN
  ClockWindow.Clock.Time := StrToTime(CurrentRailwayTimeStr);
  ClockWindow.Caption := SetCurrentRailwayTimeCaption;
  ClockWindow.Visible := True;
  ClockWindow.OKButton.SetFocus;
END; { SetCurrentRailwayTime }

PROCEDURE NewSetProgramStartTime;
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  ClockWindow.Clock.Time := ProgramStartTime;
  ClockWindow.Caption := SetProgramStartTimeCaption;
  ClockWindow.Visible := True;
  ClockWindow.OKButton.SetFocus;
END; { SetProgramStartTime }

PROCEDURE NewSetDaylightStartTime;
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  ClockWindow.Clock.Time := StrToTime(DaylightStartTimeStr);
  ClockWindow.Caption := SetDaylightStartTimeCaption;
  ClockWindow.Visible := True;
  ClockWindow.OKButton.SetFocus;
END; { SetDaylightStartTime }

PROCEDURE NewSetDaylightEndTime;
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  ClockWindow.Clock.Time := StrToTime(DaylightEndTimeStr);
  ClockWindow.Caption := SetDaylightEndTimeCaption;
  ClockWindow.Visible := True;
  ClockWindow.OKButton.SetFocus;
END; { SetDaylightEndTime }

PROCEDURE InitialiseGetTimeUnit;
{ Such routines as this allow us to initialises the units in the order we wish }
BEGIN
  SaveCurrentRailwayTimeStr := StringOfChar(' ', 11);
  WasDayTime := DayTime;

  Log('A GetTime unit initialised');
END; { InitialiseGetTimeUnit }

INITIALIZATION

END { GetTime }.
