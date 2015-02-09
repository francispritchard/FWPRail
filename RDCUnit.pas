UNIT RDCUnit;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Initvars, ComCtrls;

  { These function names (relating to the PIE interace) are case sensitive }
  FUNCTION CleanupInterface(Handle : LongInt) : Integer; STDCALL;
  FUNCTION ClearBuffer(Handle : LongInt) : Integer; STDCALL;
  FUNCTION CloseInterface(Handle : LongInt) : Integer; STDCALL;
  FUNCTION EnumeratePIE(VID : LongInt; VAR Data : LongInt; VAR Count : LongInt) : Integer; STDCALL;
  FUNCTION ReadLast(Handle : LongInt; VAR Data : Byte) : Integer; STDCALL;
  FUNCTION ReadData (Handle : LongInt; VAR Data : Byte) : Integer; STDCALL;
  FUNCTION SetupInterface(Handle : LongInt) : Integer; STDCALL;
  FUNCTION WriteData(Handle : LongInt; VAR Data : Byte) : Integer; STDCALL;

TYPE
  TRailDriverWindow = CLASS(TForm)
    BailOffLabel: TLabel;
    Button0: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    Button27: TButton;
    Button28: TButton;
    Button29: TButton;
    Button30: TButton;
    Button31: TButton;
    Button32: TButton;
    Button33: TButton;
    Button34: TButton;
    Button35: TButton;
    Button36: TButton;
    Button37: TButton;
    Button38: TButton;
    Button39: TButton;
    Button40: TButton;
    Button41: TButton;
    Button42: TButton;
    Button43: TButton;
    LocoBrakeLabel: TLabel;
    RailDriverTimer: TTimer;
    RegulatorLabel: TLabel;
    ReverserLabel: TLabel;
    ThreeWayA: TLabel;
    ThreeWayB: TLabel;
    TrainBrakeLabel: TLabel;
    WriteButton: TButton;
    ReverserForwardProgressBar: TProgressBar;
    ReverserValue: TLabel;
    RegulatorValue: TLabel;
    TrainBrakeValue: TLabel;
    LocoBrakeValue: TLabel;
    BailOffValue: TLabel;
    ThreeWaySwitchAValue: TLabel;
    ThreeWaySwitchBValue: TLabel;
    RegulatorForwardProgressBar: TProgressBar;
    ReverserReverseProgressBar: TProgressBar;
    RegulatorReverseProgressBar: TProgressBar;
    RegulatorPosProgressBar: TProgressBar;
    LEDTimer: TTimer;
    PROCEDURE LEDTimerTick(Sender: TObject);
    PROCEDURE RailDriverTimerTick(Sender: TObject);
    PROCEDURE RailDriverWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE RailDriverWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE WriteButtonClick(Sender: TObject);

  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

CONST
  UnitRef = 'RDC';

VAR
  EnumerateResult : LongInt;
  RailDriverDeviceHandle : LongInt;
  RailDriverWindow: TRailDriverWindow;
  SaveRailDriverLEDStr : String = '';
  TestWriteCount : Integer = 0;

  BailOffData : Byte;
  DirectionWritten : Boolean = False;
  LocoBrakeData : Byte;
  LocoExists : Boolean = False;
  MessageDialogueVisible : Boolean = False;
  LocoSelected : Boolean = False;
  LocoSelectionInProgress : Boolean = False;
  RailDriverNotReadyMsg : Boolean = False;
  RailDriverReady : Boolean = False;
  RegulatorPos : Cardinal = 0;
  ReverserData, RegulatorData : Byte;
  ThreeWaySwitchAData, ThreeWaySwitchBData : Byte;
  TrainBrakeData : Byte;
  TrainBrakePos : Cardinal = 0;

PROCEDURE CloseRailDriver;
{ Close the RailDriver unit down }

PROCEDURE SetSpeedByRailDriverConsole(L : LocoIndex);
{ Set the speed by using the console }

PROCEDURE StartRailDriver;
{ Start up the Rail Driver, calibrating it if necessary }

PROCEDURE WriteToRailDriverLEDs(LEDStr : String);
{ Writes the three letter string to the RailDriver red LED }

IMPLEMENTATION

USES MiscUtils, GetTime, Startup, Lenz, StrUtils, LocoUtils, Diagrams, Input, DateUtils, Movement, FWPShowMessageUnit, Options, Train, Logging;

TYPE
  ThreeWaySwitchPosType = (Left, Centre, Right);

VAR
  DeviceData : ARRAY [0..99] OF LongInt;
  DataReadIn : ARRAY [0..14] OF Byte;
  DataToBeWritten : ARRAY [0..7] OF Byte;

  CalibratingAButton : Boolean = False;
  CalibrationFileOpen : Boolean = False;
  DoneButtonDown : Boolean = False;

  Accelerating : Boolean = False;
  BailOffMinCalibrated : Boolean = False;
  BailOffMaxCalibrated : Boolean = False;
  ButtonDown, ButtonPressed, ButtonStateChanged : ARRAY [0..43] OF Boolean;
  Decelerating : Boolean = False;
  EmergencyBrakeApplied : Boolean = False;
  EmergencyBrakeCalibrated : Boolean = False;
  RegulatorInNeutral : Boolean = True;
  LastCalibrateStartTime : TDateTime;
  LEDAppendString : String = '';
  LEDLongStr : String = '';
  LEDLongStrInitialised : Boolean = False;
  LocoBrakeMinCalibrated : Boolean = False;
  LocoBrakeMaxCalibrated : Boolean = False;
  LocoChipByButton : String = '';
  NewSpeed : Integer = 0;
  NumberOfSecondsToAccelerate : Real = 0.5;
  NumberOfSecondsToDecelerate : Real = 1;
  RegulatorForwardMinCalibrated : Boolean = False;
  RegulatorForwardMaxCalibrated : Boolean = False;
  RegulatorReverseMinCalibrated : Boolean = False;
  RegulatorReverseMaxCalibrated : Boolean = False;
  ReverserForwardMinCalibrated : Boolean = False;
  ReverserForwardMaxCalibrated : Boolean = False;
  ReverserReverseMinCalibrated : Boolean = False;
  ReverserReverseMaxCalibrated : Boolean = False;
  SaveSpeedNum : Integer = 0;
  TempMin : Integer = 0;
  TempMax : Integer = 0;
  ThreeWaySwitchALeftNumCalibrated : Boolean = False;
  ThreeWaySwitchAMidNumCalibrated : Boolean = False;
  ThreeWaySwitchARightNumCalibrated : Boolean = False;
  ThreeWaySwitchBLeftNumCalibrated : Boolean = False;
  ThreeWaySwitchBMidNumCalibrated : Boolean = False;
  ThreeWaySwitchBRightNumCalibrated : Boolean = False;
  TrainBrakeMinCalibrated : Boolean = False;
  TrainBrakeMaxCalibrated : Boolean = False;

  { these function names are case sensitive }
  FUNCTION EnumeratePIE; EXTERNAL 'PIEHid.dll';
  FUNCTION SetupInterface; EXTERNAL 'PIEHid.dll';
  FUNCTION CloseInterface; EXTERNAL 'PIEHid.dll';
  FUNCTION ReadData; EXTERNAL 'PIEHid.dll';
  FUNCTION WriteData; EXTERNAL 'PIEHid.dll';
  FUNCTION ReadLast; EXTERNAL 'PIEHid.dll';
  FUNCTION ClearBuffer; EXTERNAL 'PIEHid.dll';
  FUNCTION CleanupInterface; EXTERNAL 'PIEHid.dll';

{$R *.dfm}

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION WriteRailDriverError(ErrorCode : Integer) : String;
BEGIN
  CASE ErrorCode OF
    { Enumerate() errors }
    101: Result := 'Bad HID device information set handle';
    102: Result := 'No devices found in device information set';
    103: Result := 'Error getting device interface detail (symbolic link name)';
    104: Result := 'Error getting device interface detail (symbolic link name)';
    105: Result := 'Unable to open a handle.';

    { SetUpInterface() errors }
    201: Result := 'Bad interface handle';
    202: Result := 'Cannot allocate memory for ring buffer';
    203: Result := 'Cannot create mutex';
    204: Result := 'Cannot create read thread';
    205: Result := 'Cannot open read handle';
    206: Result := 'Cannot open read handle - Access Denied';
    207: Result := 'Cannot open read handle - bad DevicePath';
    208: Result := 'Cannot open write handle';
    209: Result := 'Cannot open write handle - Access Denied';
    210: Result := 'Cannot open write handle - bad DevicePath';

    { ReadData() errors }
    301: Result := 'Bad interface handle';
    302: Result := 'Read length is zero';
    303: Result := 'Could not acquire data mutex';
    304: Result := 'Insufficient data (< readSize bytes)';
    305: Result := 'Could not release data mutex';
    306: Result := 'Could not release data mutex';
    307: Result := 'Handle Invalid or Device_Not_Found (probably device unplugged)';

    { Write() errors }
    401: Result := 'Bad interface handle';
    402: Result := 'Write length is zero';
    403: Result := 'Write failed';
    404: Result := 'Write incomplete';
    405: Result := 'unable to acquire write mutex';
    406: Result := 'unable to release write mutex';
    407: Result := 'Handle Invalid or Device_Not_Found (probably device unplugged) (previous buffered write)';
    408: Result := 'Buffer full';
    409: Result := 'Previous buffered write failed.';
    410: Result := 'Previous buffered write sent wrong number of bytes';
    411: Result := 'timer failed';
    412: Result := 'previous buffered write count not release mutex';

    { ReadLast errors }
    501: Result := 'Bad interface handle';
    502: Result := 'Read length is zero';
    503: Result := 'Could not acquire data mutex';
    504: Result := 'Insufficient data (< readSize bytes)';
    505: Result := 'Could not release data mutex';
    506: Result := 'Could not release data mutex';
    507: Result := 'Handle Invalid or Device_Not_Found (probably device unplugged)';

    { ClearBuffer() errors }
    601: Result := 'Bad interface handle';
    602: Result := 'Could not release data mutex';
    603: Result := 'Could not acquire data mutex';
  ELSE
    Result := 'No error text available for error ' + IntToStr(ErrorCode);
  END; {CASE}
END; { WriteRailDriverError }

{$O-}
PROCEDURE WriteToRailDriverLEDs(LEDStr : String);
{ Writes the three letter string to the RailDriver red LED }
VAR
  Ch : Char;
  I : Integer;

  FUNCTION GetLEDNumber(Ch : Char) : Integer;
  { Returns the number to drive the LED }
  BEGIN
    CASE Ch Of
    {   0
       5 1
        6
       4 2
        3  .=7
    }
      ' ':
        Result := 0;
      '1':
        Result := 6;    {0000 0110}
      '2':
        Result := 91;   {0101 1011}
      '3':
        Result := 79;   {0100 1111}
      '4':
        Result := 102;  {0110 0110}
      '5':
        Result := 109;  {0110 1101}
      '6':
        Result := 124;  {0111 1100}
      '7':
        Result := 7;    {0000 0111}
      '8':
        Result := 127;  {0111 1111}
      '9':
        Result := 103;  {0110 0111}
      '0', 'O', 'o':
        Result := 63;   {0011 1111}
      'A', 'a':
        Result := 119;  {0111 0111}
      'B', 'b':
        Result := 124;  {0111 1100}
      'C', 'c':
        Result := 57;   {0011 1001}
      'D', 'd':
        Result := 94;   {0101 1110}
      'E', 'e':
        Result := $79;  {0111 1001}
      'F', 'f':
        Result := 113;  {0111 0001}
      'G', 'g':
        Result := 61;   {0111 1001}
      'H', 'h':
        Result := 118;  {0111 0110}
      'I', 'i':
        Result := 48;   {0011 0000}
      'J', 'j':
        Result := 14;   {0000 1110}
      'L', 'l':
        Result := 56;   {0011 1000}
      'N', 'n':
        Result := 84;   {0101 0100}
      'P', 'p':
        Result := 115;  {0111 0011}
      'R', 'r':
        Result := 80;   {0101 0000}
      'S', 's':
        Result := 109;  {0110 1101}
      'T', 't':
        Result := 120;  {0111 1000}
      'U', 'u', 'V', 'v':
        Result := 62;   {0011 1110}
      '-':
        Result := 64;   {0100 0000}
      '.':
        Result := 128;  {1000 0000}
      '!':
        Result := 130;  {1000 0010}
    ELSE
      Result := 0;
    END; {CASE}
  END; { GetLEDNumber }

BEGIN
  IF LEDStr <> SaveRailDriverLEDStr THEN BEGIN
    SaveRailDriverLEDStr := LEDStr;
    DataToBeWritten[2] := 0;
    DataToBeWritten[3] := 0;
    DataToBeWritten[4] := 0;

    IF Length(LEDStr) = 0 THEN
      LEDStr := StringOfChar(' ', 3)
    ELSE
      IF Length(LEDStr) = 1 THEN
        LEDStr := StringOfChar(' ', 2) + LEDStr
      ELSE
        IF Length(LEDStr) = 2 THEN
          LEDStr := ' ' + LEDStr;

    IF Length(LEDStr) > 3 THEN
      LEDStr := Copy(LEDStr, Length(LEDStr) - 2, 3);

    FOR I := 1 TO 3 DO BEGIN
      Ch := LEDStr[I];
      CASE I OF
        1: { left digit }
          DataToBeWritten[4]:= GetLEDNumber(Ch);
        2: { middle digit }
          DataToBeWritten[3]:= GetLEDNumber(Ch);
        3: { right digit }
          DataToBeWritten[2]:= GetLEDNumber(Ch);
      END; {CASE}
    END;

    DataToBeWritten[0] := 0; { ReportID }
    DataToBeWritten[1] := 134; { Write Command (to LED $86) }

    DataToBeWritten[5] := 0; { 0 for both }
    DataToBeWritten[6] := 0; { 0 for both }
    DataToBeWritten[7] := 0; { 1 for speaker, 0 for display }

    WriteData(RailDriverDeviceHandle, DataToBeWritten[0]);
    IF LEDLongStr = '' THEN
      Log('A LED: ' + LEDStr);
  END;
END; { WriteToRailDriverLEDs }

PROCEDURE AppendToRailDriverLEDs(NewDigit : String);
BEGIN
{ Add a character to the LEDs, scrolling the previous character(s) left by one }
  LEDAppendString := LEDAppendString + NewDigit;
  IF Length(LEDAppendString) > 3 THEN
    LEDAppendString := Copy(LEDAppendString, 2, 3);
   WriteToRailDriverLEDs(LEDAppendString);
END; { AppendToRailDriverLEDs }

FUNCTION GetThreeWaySwitchAPos : ThreeWaySwitchPosType;
{ Return the current position of the given three position switch }
VAR
  LeftToCentreMidPoint, CentreToRightMidPoint : Integer;

BEGIN
  LeftToCentreMidPoint := RDCThreeWaySwitchALeftNum + (RDCThreeWaySwitchAMidNum - RDCThreeWaySwitchALeftNum) DIV 2;
  CentreToRightMidPoint := RDCThreeWaySwitchAMidNum + (RDCThreeWaySwitchARightNum - RDCThreeWaySwitchAMidNum) DIV 2;

  IF (ThreeWaySwitchAData > 0) AND (ThreeWaySwitchAData < LeftToCentreMidPoint) THEN
    Result := Left
  ELSE
    IF (ThreeWaySwitchAData > LeftToCentreMidPoint) AND (ThreeWaySwitchAData < CentreToRightMidPoint) THEN
      Result := Centre
    ELSE
      Result := Right;
END; { GetThreeWaySwitchAPos }

FUNCTION GetThreeWaySwitchBPos : ThreeWaySwitchPosType;
{ Return the current position of the given three position switch }
VAR
  LeftToCentreMidPoint, CentreToRightMidPoint : Integer;

BEGIN
  LeftToCentreMidPoint := RDCThreeWaySwitchBLeftNum + (RDCThreeWaySwitchBMidNum - RDCThreeWaySwitchBLeftNum) DIV 2;
  CentreToRightMidPoint := RDCThreeWaySwitchBMidNum + (RDCThreeWaySwitchBRightNum - RDCThreeWaySwitchBMidNum) DIV 2;

  IF (ThreeWaySwitchBData > 0) AND (ThreeWaySwitchBData < LeftToCentreMidPoint) THEN
    Result := Left
  ELSE
    IF (ThreeWaySwitchBData > LeftToCentreMidPoint) AND (ThreeWaySwitchBData < CentreToRightMidPoint) THEN
      Result := Centre
    ELSE
      Result := Right;
END; { GetThreeWaySwitchBPos }

FUNCTION GetReverserDirection : DirectionType;
{ Return whether the reverser is forwards or reverse (up or down) }
BEGIN
  IF ReverserData <= RDCReverserForwardMax THEN
    Result := Up
  ELSE
    IF ReverserData >= RDCReverserReverseMin THEN
      Result := Down
    ELSE
      { neutral }
      Result := UnknownDirection;
END; { GetReverserDirection }

FUNCTION GetRegulatorDirection : DirectionType;
{ Return whether the regulator is forwards or reverse (up or down) }
BEGIN
  IF RegulatorData <= RDCRegulatorForwardMax THEN
    Result := Up
  ELSE
    IF RegulatorData >= RDCRegulatorReverseMin THEN
      Result := Down
    ELSE
      { neutral }
      Result := UnknownDirection;
END; { GetRegulatorDirection }

FUNCTION GetRegulatorPos : Integer;
{ Return the current position of the regulator in the range 0 to 9 }
VAR
  Range : Integer;
  WhereInRange : Integer;
  Direction : DirectionType;
  Step : Integer;

BEGIN
  { First see if the regulator is "forward" or "reverse" }
  WhereInRange := 0;
  Range := 0;
  Direction := GetRegulatorDirection;
  CASE Direction OF
    Up:
      BEGIN
        Range := RDCRegulatorForwardMax - (RDCRegulatorForwardMin + 1);
        WhereInRange := Range - (RegulatorData - (RDCRegulatorForwardMin + 4));
      END;
    Down:
      BEGIN
        Range := RDCRegulatorReverseMax - (RDCRegulatorReverseMin + 1);
        WhereInRange := RegulatorData - (RDCRegulatorReverseMin - 4);
      END;
    UnknownDirection:
      BEGIN
        Range := 0;
        WhereInRange := 0;
      END;
  END; {CASE}

  IF WhereInRange < 0 THEN
    WhereInRange := 0
  ELSE
    IF WhereInRange > 255 THEN
      WhereInRange := 255;

  { There are ten program speed settings (which translate into the Lenz 28 speed steps) }
  IF WhereInRange = 0 THEN
    Result := 0
  ELSE BEGIN
    Step := Range DIV 11;
    Result := (WhereInRange DIV Step) - 1;
    IF Result > 10 THEN
      Result := 10
    ELSE
      IF Result < 0 THEN
        Result := 0;
  END;
END; { GetRegulatorPos }

FUNCTION GetTrainBrakePos : Integer;
{ Return the current position of the train brake in the range 0 to 30 }
VAR
  Range : Real;
  WhereInRange : Integer;
  Step : Real;

BEGIN
  { First see if the TrainBrake is "forward" or "reverse" }
  Range := RDCTrainBrakeMax - (RDCTrainBrakeMin + 1);
  WhereInRange := TrainBrakeData - RDCTrainBrakeMin - 2;

  IF WhereInRange < 0 THEN
    WhereInRange := 0
  ELSE
    IF WhereInRange > 255 THEN
      WhereInRange := 255;

  { There are thirty settings (which translate into seconds) }
  IF WhereInRange = 0 THEN
    Result := 0
  ELSE BEGIN
    Step := Range / 46;
    Result := Trunc((WhereInRange / Step) - 1);
    IF Result > 46 THEN
      Result := 45
    ELSE
      IF Result < 0 THEN
        Result := 0;
  END;
END; { GetTrainBrakePos }

PROCEDURE SetSpeedByRailDriverConsole(L : LocoIndex);
{ Set the speed by using the console }
VAR
  OK : Boolean;
  PresentSpeedNum : Integer;
  UserMsg : String;

BEGIN
  IF L <> UnknownLocoIndex THEN BEGIN
    WITH Locos[L] DO BEGIN
      { Get current speed }
      IF SystemOnline THEN
        PresentSpeedNum := GetLenzSpeed(Locos[L], NOT ForceARead)
      ELSE
  //      PresentSpeedNum := SpeedInMPHToLocoLenzSpeed(T, Loco_CurrentSpeedInMPH)
  ;

      IF NOT RegulatorInNeutral AND (SaveSpeedNum <> PresentSpeedNum) THEN BEGIN
        IF LEDLongStr = '' THEN
          { do not write anything else out if we're outputting a long string }
          WriteToRailDriverLEDs(IntToStr(PresentSpeedNum));
        SaveSpeedNum := PresentSpeedNum;
      END;

  //  LightsType = (NoLights, HeadlightsAndTailLightsConnected, HeadlightsAndTailLightsSeparatelySwitched,
  //                ExpressModelsSeparateHeadlights, LightsOperatedByTwoChips, LightsShouldBeDimmed, CustomLightingKit);

      IF Loco_LightsType <> NoLights THEN BEGIN
        CASE Loco_LightsType OF
          HeadlightsAndTailLightsConnected:
            BEGIN
              IF GetThreeWaySwitchAPos = Left THEN BEGIN
                IF Loco_LightsOn THEN
                  TurnLocoLightsOff(L, NOT UserMsgRequired, UserMsg, OK);
              END ELSE
                IF GetThreeWaySwitchAPos = Centre THEN BEGIN
                  IF NOT Loco_LightsOn THEN
                    TurnLocoLightsOn(L, NonMovingLoco, LightLoco, NOT UserMsgRequired, UserMsg, OK);
                END;
            END;
          LightsOperatedByTwoChips:
            BEGIN
              IF GetThreeWaySwitchAPos = Left THEN BEGIN
                IF Loco_LightsOn THEN
                  IF Loco_CurrentDirection = Up THEN
                    TurnLocoHeadLightsOff(L, NOT UserMsgRequired, UserMsg, OK);
              END ELSE
                IF GetThreeWaySwitchAPos = Centre THEN BEGIN
                  IF NOT Loco_LightsOn THEN
                    TurnLocoLightsOn(L, NonMovingLoco, LightLoco, NOT UserMsgRequired, UserMsg, OK);
                END;

            END;
        END; {CASE}
      END;

      { First check position of Reverser }
      RegulatorInNeutral := False;
      IF GetReverserDirection = Up THEN BEGIN
        { Write out the direction to the LEDs }
        IF (PresentSpeedNum = 0) AND NOT DirectionWritten THEN BEGIN
          IF LEDLongStr = '' THEN
            { do not write anything else out if we're outputting a long string }
            WriteToRailDriverLEDs('Up');
          DirectionWritten := True;
        END;
        { change direction if necessary }
        IF (Loco_CurrentDirection = Down) AND SystemOnline THEN
          SetLocoDirection(Locos[L], Up, OK);
      END ELSE
        IF GetReverserDirection = Down THEN BEGIN
          { Write out the direction to the LEDs }
          IF (PresentSpeedNum = 0) AND NOT DirectionWritten THEN BEGIN
            IF LEDLongStr = '' THEN
              { do not write anything else out if we're outputting a long string }
              WriteToRailDriverLEDs('Dn');
            DirectionWritten := True;
          END;
          { change direction if necessary }
          IF (Loco_CurrentDirection = Up) AND SystemOnline THEN
            SetLocoDirection(Locos[L], Down, OK);
        END ELSE
          { regulator in neutral }
          IF RegulatorPos = 0 THEN BEGIN
            RegulatorInNeutral := True;
            IF LEDLongStr = '' THEN
              { do not write anything else out if we're outputting a long string }
              WriteToRailDriverLEDs('---');
            DirectionWritten := False;
          END;

      IF RegulatorInNeutral THEN BEGIN
  //      Loco_DesiredSpeedInMPH := Stop;
  //      Train_AccelerationTimeInSeconds := GetTrainBrakePos; &&&&&
      END ELSE BEGIN
  //      Train_AccelerationTimeInSeconds := GetTrainBrakePos;
  //      { So that we don't accelerate at one speed notch per 60 seconds! }
  //      IF Train_Accelerating AND (Train_AccelerationTimeInSeconds > 5.0) THEN
  //        Train_AccelerationTimeInSeconds := 5.0; &&&&&

  //      CASE GetRegulatorPos OF
  //        0:
  //          Train_DesiredSpeedInMPH := Stop;
  //        1:
  //          Train_DesiredSpeedInMPH := MPH20;
  //        2:
  //          Train_DesiredSpeedInMPH := MPH30;
  //        3:
  //          Train_DesiredSpeedInMPH := MPH40;
  //        4:
  //          Train_DesiredSpeedInMPH := MPH50;
  //        5:
  //          Train_DesiredSpeedInMPH := MPH60;
  //        6:
  //          Train_DesiredSpeedInMPH := MPH70;
  //        7:
  //          Train_DesiredSpeedInMPH := MPH80; { *** }
  //        8:
  //          Train_DesiredSpeedInMPH := MPH90; { *** }
  //        9:
  //          Train_DesiredSpeedInMPH := MPH100; { *** }
  //        10:
  //          Train_DesiredSpeedInMPH := MPH120; { *** }
  //      ELSE
  //        Log('AG Funny RegulatorPos from RD: ' + IntToStr(RegulatorPos));
  //      END; {CASE}
      END;

      IF (TrainBrakeData >= RDCEmergencyBrakeMin) AND (TrainBrakeData <= RDCEmergencyBrakeMax) THEN BEGIN
        { emergency brake }
        IF PresentSpeedNum <> 0 THEN BEGIN
          SetLenzSpeedAndDirection(Locos[L], QuickStop, Loco_CurrentDirection, OK);
  //        IF Train_DoubleHeaderLocoIndex <> UnknownLocoIndex THEN
  //          SetLenzSpeedAndDirection(Train_DoubleHeaderLocoIndex, QuickStop, Train_CurrentDirection, OK); &&&&&
          Log('A Emergency brake applied');
          EmergencyBrakeApplied := True;
        END;
      END ELSE BEGIN
  //      IF PresentSpeedNum <> TrainSpeedInMPHToLenzSpeed(T, Train_DesiredSpeedInMPH) THEN
  //        SetDesiredLocoLenzSpeed(Train_LocoIndex); &&&&&
      END;
  (*
      ELSE BEGIN
        EmergencyBrakeApplied := False;
        IF (Train_BrakeData >= RDCTrain_BrakeMin) AND (Train_BrakeData <= RDCTrain_BrakeMax - 5) THEN BEGIN
          { We're braking }
          Range := (RDCTrain_BrakeMax - 3) - (RDCTrain_BrakeMin + 2);
          Num := Train_BrakeData - RDCTrain_BrakeMin - 2;
          IF Num < 0 THEN
            Num := 0;
          Train_BrakePos := MulDiv(Num, 6, Range);
          IF NumberOfSecondsToDecelerate <> (Train_BrakePos + 1) THEN BEGIN
            NumberOfSecondsToDecelerate := Train_BrakePos + 1;
            Log('AG Braking: NumberOfSecondsToDecelerate = ' + FloatToStr(NumberOfSecondsToDecelerate)]);
          END;
        END;
      END;
  *)
    END; {WITH}
  END;
END; { SetSpeedByRailDriver }

FUNCTION CheckRailDriverConsoleIsAttached : Integer;
{ See if the RailDriver console is attached }
VAR
  Count : LongInt;
  I : Integer;
  DevicePID : LongInt;
  DeviceUP : LongInt;
BEGIN
  FOR I := 0 TO 99 DO
    DeviceData[I] := 0;

  EnumerateResult := EnumeratePIE($5F3, DeviceData[0], Count);
  IF EnumerateResult = 0 THEN BEGIN
    EnumerateResult := 307; { no device plugged in }
    For I := 0 TO Count - 1 DO BEGIN
      DevicePID := DeviceData[I * 4];
      DeviceUP := DeviceData[I * 4 + 2];
      RailDriverDeviceHandle := deviceData[I * 4 + 3];
      IF (DeviceUp = 12) AND (DevicePID = 210) THEN
        { get the handle }
        EnumerateResult := SetupInterface(RailDriverDeviceHandle);
    END;
  END;
  Result := EnumerateResult;
END; { CheckRailDriverConsoleIsAttached }

PROCEDURE CloseRailDriver;
{ Close the RailDriver unit down }
BEGIN
  CloseInterface(RailDriverDeviceHandle);
END; { CloseRailDriver }

{$O-}
PROCEDURE CloseRailDriverWindow;
{ Closes the calibration window and, if calibrating, exits RDC Mode }
BEGIN
  RailDriverWindow.Hide;
  IF NOT RailDriverInitialised THEN BEGIN
    RailDriverWindow.RailDriverTimer.Enabled := False;
    SetMode(RDC, False);
    Log('AG RDC Mode turned off');
    WriteToRailDriverLEDs('');
  END;
END; { CloseCalibrationWindow }

PROCEDURE CheckCloseRailDriverWindow(OUT OK : Boolean);
{ Checks whether we really want to close the RailDriver window }
BEGIN
  OK := True;
  IF RailDriverInitialised OR NOT RailDriverCalibrationStarted THEN
    CloseRailDriverWindow
  ELSE BEGIN
    RailDriverWindow.RailDriverTimer.Enabled := False;
    IF MessageDialogueWithDefault('Cancel calibration - this will cancel RDC Mode and any new calibration data will not be saved'
                                  + ' - are you sure?',
                                  StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
    THEN
      OK := False
    ELSE BEGIN
      RailDriverWindow.RailDriverTimer.Enabled := True;
      RDCBailOffMin := 0;
      RDCBailOffMax := 0;
      RDCEmergencyBrakeMin := 0;
      RDCEmergencyBrakeMax := 0;
      RDCLocoBrakeMin := 0;
      RDCLocoBrakeMax := 0;
      RDCRegulatorForwardMin := 0;
      RDCRegulatorForwardMax := 0;
      RDCRegulatorReverseMin := 0;
      RDCRegulatorReverseMax := 0;
      RDCReverserForwardMin := 0;
      RDCReverserForwardMax := 0;
      RDCReverserReverseMin := 0;
      RDCReverserReverseMax := 0;
      RDCTrainBrakeMin := 0;
      RDCTrainBrakeMax := 0;
      RDCThreeWaySwitchALeftNum := 0;
      RDCThreeWaySwitchAMidNum := 0;
      RDCThreeWaySwitchARightNum := 0;
      RDCThreeWaySwitchBLeftNum := 0;
      RDCThreeWaySwitchBMidNum := 0;
      RDCThreeWaySwitchBRightNum := 0;

      CloseRailDriverWindow;
    END;
  END;
END; { CheckCloseRailDriverWindow }

PROCEDURE DoCalibration;
{ Start the calibration process }

  PROCEDURE CalibrateMin(DataReadIn : Integer; VAR Min : Integer; VAR Done : Boolean; Instructions : String);
  { Find the minimum }
  VAR
    OK : Boolean;

  BEGIN
    WITH RailDriverWindow DO BEGIN
      Done := False;
      IF NOT MessageDialogueVisible THEN BEGIN
        MessageDialogueVisible := True;
        CASE MessageDialogueWithDefault(Instructions + ' and press OK', StopTimer, mtConfirmation, [mbOK, mbAbort], mbOK) OF
          mrOK:
            BEGIN
              CalibratingAButton := True;
              LastCalibrateStartTime := Time;
              Min := 255;
            END;
          mrAbort:
            BEGIN
              CheckCloseRailDriverWindow(OK);
              IF OK THEN
                Exit;
            END;
        END; {CASE}
      END;
      IF CalibratingAButton THEN BEGIN
        IF NOT FWPShowMessageWindow.Visible THEN
          FWPShowMessage('Calibrating...');
        IF DataReadIn < Min THEN
          Min := DataReadIn;
        Debug('Min=' + IntToStr(Min));

        IF Round(SecondSpan(Time, LastCalibrateStartTime)) > 3 THEN BEGIN
          Debug('Min=' + IntToStr(Min));
          Done := True;
          CalibratingAButton := False;
          FWPShowMessageWindow.Hide;
          MessageDialogueVisible := False;
        END;
      END;
    END; {WITH}
  END; { CalibrateMin }

  PROCEDURE CalibrateMid(DataReadIn : Integer; VAR Mid : Integer; VAR Done : Boolean; Instructions : String);
  { Find the mid position }
  VAR
    OK : Boolean;

  BEGIN
    WITH RailDriverWindow DO BEGIN
      Done := False;
      IF NOT MessageDialogueVisible THEN BEGIN
        MessageDialogueVisible := True;
        CASE MessageDialogueWithDefault(Instructions + ' and press OK', StopTimer, mtConfirmation, [mbOK, mbAbort], mbOK) OF
          mrOK:
            BEGIN
              CalibratingAButton := True;
              LastCalibrateStartTime := Time;
              TempMin := 255;
              TempMax := 0;
            END;
          mrAbort:
            BEGIN
              CheckCloseRailDriverWindow(OK);
              IF OK THEN
                Exit;
            END;
        END; {CASE}
      END;
      IF CalibratingAButton THEN BEGIN
        IF NOT FWPShowMessageWindow.Visible THEN
          FWPShowMessage('Calibrating...');
        IF DataReadIn < TempMin THEN
          TempMin := DataReadIn;
        IF DataReadIn > TempMax THEN
          TempMax := DataReadIn;
        Debug('Min=' + IntToStr(TempMin) + ', Max=' + IntToStr(TempMax));

        IF Round(SecondSpan(Time, LastCalibrateStartTime)) > 3 THEN BEGIN
          Mid := TempMin + ((TempMax - TempMin) DIV 2);
          Debug('Min=' + IntToStr(TempMin) + ', Max=' + IntToStr(TempMax) + ', Mid=' + IntToStr(Mid));
          Mid := TempMin + (TempMax - TempMin);
          Done := True;
          CalibratingAButton := False;
          FWPShowMessageWindow.Hide;
          MessageDialogueVisible := False;
        END;
      END;
    END; {WITH}
  END; { CalibrateMid }

  PROCEDURE CalibrateMax(DataReadIn : Integer; VAR Max : Integer; VAR Done : Boolean; Instructions : String);
  { Find the maximum }
  VAR
    OK : Boolean;

  BEGIN
    WITH RailDriverWindow DO BEGIN
      Done := False;
      IF NOT MessageDialogueVisible THEN BEGIN
        MessageDialogueVisible := True;
        CASE MessageDialogueWithDefault(Instructions + ' and press OK', StopTimer, mtConfirmation, [mbOK, mbAbort], mbOK) OF
          mrOK:
            BEGIN
              CalibratingAButton := True;
              LastCalibrateStartTime := Time;
              Max := 0;
            END;
          mrAbort:
            BEGIN
              CheckCloseRailDriverWindow(OK);
              IF OK THEN
                Exit;
            END;
        END; {CASE}
      END;
      IF CalibratingAButton THEN BEGIN
        IF NOT FWPShowMessageWindow.Visible THEN
          FWPShowMessage('Calibrating...');
        IF DataReadIn > Max THEN
          Max := DataReadIn;
        Debug('Max=' + IntToStr(Max));

        IF Round(SecondSpan(Time, LastCalibrateStartTime)) > 3 THEN BEGIN
          Debug('Max=' + IntToStr(Max));
          Done := True;
          CalibratingAButton := False;
          FWPShowMessageWindow.Hide;
          MessageDialogueVisible := False;
        END;
      END;
    END; {WITH}
  END; { CalibrateMax }

BEGIN
  WITH RailDriverWindow DO BEGIN
    Visible := True;
  //  BringToFront;

    { Calibrate reverser - forward }
    IF NOT ReverserForwardMinCalibrated THEN
      CalibrateMin(ReverserData, RDCReverserForwardMin, ReverserForwardMinCalibrated, 'Push the reverser to full forward');
    IF ReverserForwardMinCalibrated AND NOT ReverserForwardMaxCalibrated THEN
      CalibrateMax(ReverserData, RDCReverserForwardMax, ReverserForwardMaxCalibrated,'Pull the reverser back almost to the neutral stop');

    { Calibrate reverser - reverse }
    IF ReverserForwardMaxCalibrated AND NOT ReverserReverseMaxCalibrated THEN
      CalibrateMax(ReverserData, RDCReverserReverseMax, ReverserReverseMaxCalibrated, 'Pull the reverser back to full reverse');
    IF ReverserReverseMaxCalibrated AND NOT ReverserReverseMinCalibrated THEN
        CalibrateMin(ReverserData, RDCReverserReverseMin, ReverserReverseMinCalibrated, 'Push the reverser almost to the neutral stop');

    { Calibrate regulator - forward }
    IF ReverserReverseMinCalibrated AND NOT RegulatorForwardMinCalibrated THEN
      CalibrateMin(RegulatorData, RDCRegulatorForwardMin, RegulatorForwardMinCalibrated, 'Push the regulator to full forward');
    IF RegulatorForwardMinCalibrated AND NOT RegulatorForwardMaxCalibrated THEN
      CalibrateMax(RegulatorData, RDCRegulatorForwardMax, RegulatorForwardMaxCalibrated, 'Pull the regulator back to the neutral stop');

    { Calibrate regulator - reverse }
    IF RegulatorForwardMaxCalibrated AND NOT RegulatorReverseMaxCalibrated THEN
      CalibrateMax(RegulatorData, RDCRegulatorReverseMax, RegulatorReverseMaxCalibrated, 'Pull the regulator back to full reverse');
    IF RegulatorReverseMaxCalibrated AND NOT RegulatorReverseMinCalibrated THEN
      CalibrateMin(RegulatorData, RDCRegulatorReverseMin, RegulatorReverseMinCalibrated, 'Push the regulator back to the neutral stop');

    { Calibrate train brake - full off }
    IF RegulatorReverseMinCalibrated AND NOT TrainBrakeMaxCalibrated THEN
      CalibrateMax(TrainBrakeData, RDCTrainBrakeMax, TrainBrakeMaxCalibrated, 'Pull the train brake back to full off');
    { Calibrate train brake - full on }
    IF TrainBrakeMaxCalibrated AND NOT TrainBrakeMinCalibrated THEN
      CalibrateMin(TrainBrakeData, RDCTrainBrakeMin, TrainBrakeMinCalibrated, 'Push the train brake up to full on (not emergency)');
    { Calibrate train brake - emergency on }
    RDCEmergencyBrakeMax := RDCTrainBrakeMin - 1;
    IF TrainBrakeMinCalibrated AND NOT EmergencyBrakeCalibrated THEN
      CalibrateMin(TrainBrakeData, RDCEmergencyBrakeMin, EmergencyBrakeCalibrated, 'Push the train brake forward to emergency stop');

    { Calibrate loco brake }
    IF EmergencyBrakeCalibrated AND NOT LocoBrakeMaxCalibrated THEN
      CalibrateMax(LocoBrakeData, RDCLocoBrakeMax, LocoBrakeMaxCalibrated, 'Pull the loco brake back to full off');
    IF LocoBrakeMaxCalibrated AND NOT LocoBrakeMinCalibrated THEN
      CalibrateMin(LocoBrakeData, RDCLocoBrakeMin, LocoBrakeMinCalibrated, 'Push the loco brake up to full on');

    { Calibrate bail off }
    IF LocoBrakeMinCalibrated AND NOT BailOffMinCalibrated THEN
      CalibrateMin(BailoffData, RDCBailOffMin, BailOffMinCalibrated, 'Leave the loco brake in the bail on position');
    IF BailOffMinCalibrated AND NOT BailOffMaxCalibrated THEN
      CalibrateMax(BailOffData, RDCBailOffMax, BailOffMaxCalibrated, 'Push the loco brake across to bail off');

    { Calibrate 3 way switch A - left }
    IF BailOffMaxCalibrated AND NOT ThreeWaySwitchALeftNumCalibrated THEN
      CalibrateMin(ThreeWaySwitchAData, RDCThreeWaySwitchALeftNum, ThreeWaySwitchALeftNumCalibrated, 'Set the upper 3 way switch to the left position');

    { Calibrate 3 way switch A - centre }
    IF ThreeWaySwitchALeftNumCalibrated AND NOT ThreeWaySwitchAMidNumCalibrated THEN
      CalibrateMid(ThreeWaySwitchAData, RDCThreeWaySwitchAMidNum, ThreeWaySwitchAMidNumCalibrated, 'Set the upper 3 way switch to the centre position');

    { Calibrate 3 way switch A - right }
    IF ThreeWaySwitchAMidNumCalibrated AND NOT ThreeWaySwitchARightNumCalibrated THEN
      CalibrateMax(ThreeWaySwitchAData, RDCThreeWaySwitchARightNum, ThreeWaySwitchARightNumCalibrated, 'Set the upper 3 way switch to the right position');

    { Calibrate 3 way switch B - left }
    IF ThreeWaySwitchARightNumCalibrated AND NOT ThreeWaySwitchBLeftNumCalibrated THEN
      CalibrateMin(ThreeWaySwitchBData, RDCThreeWaySwitchBLeftNum, ThreeWaySwitchBLeftNumCalibrated, 'Set the lower 3 way switch to the left position');

    { Calibrate 3 way switch B - centre }
    IF ThreeWaySwitchBLeftNumCalibrated AND NOT ThreeWaySwitchBMidNumCalibrated THEN
      CalibrateMid(ThreeWaySwitchBData, RDCThreeWaySwitchBMidNum, ThreeWaySwitchBMidNumCalibrated, 'Set the lower 3 way switch to the centre position');

    { Calibrate 3 way switch B - right }
    IF ThreeWaySwitchBMidNumCalibrated AND NOT ThreeWaySwitchBRightNumCalibrated THEN
      CalibrateMax(ThreeWaySwitchBData, RDCThreeWaySwitchBRightNum, ThreeWaySwitchBRightNumCalibrated, 'Set the lower 3 way switch to the right position');

    { and tidy up }
    IF BailOffMinCalibrated AND BailOffMaxCalibrated
    AND EmergencyBrakeCalibrated
    AND LocoBrakeMinCalibrated AND LocoBrakeMaxCalibrated
    AND RegulatorForwardMinCalibrated AND RegulatorForwardMaxCalibrated
    AND RegulatorReverseMinCalibrated AND RegulatorReverseMaxCalibrated
    AND ReverserForwardMinCalibrated AND ReverserForwardMaxCalibrated
    AND ReverserReverseMinCalibrated AND ReverserReverseMaxCalibrated
    AND ThreeWaySwitchALeftNumCalibrated AND ThreeWaySwitchAMidNumCalibrated AND ThreeWaySwitchARightNumCalibrated
    AND ThreeWaySwitchBLeftNumCalibrated AND ThreeWaySwitchBMidNumCalibrated AND ThreeWaySwitchBRightNumCalibrated
    AND TrainBrakeMinCalibrated AND TrainBrakeMaxCalibrated
    THEN BEGIN
      IF NOT MessageDialogueVisible THEN BEGIN
        MessageDialogueVisible := True;
        ShowMessage('Calibration complete');
        RailDriverInitialised := True;
        WriteToRailDriverLEDs('  .');
        Visible := False;
        SendToBack;
        Log('AG RailDriver console initialised');
      END;
    END;
  END; {WITH}
END; { DoCalibration }

FUNCTION CheckButtons(Data : ARRAY OF Byte) : Boolean;
{ Return true if any buttons have been pressed - also stores their state }
VAR
  ButtonNo : Integer;

BEGIN
  Result := False;
  WITH RailDriverWindow DO BEGIN
    IF (Data[8] AND 1) = 1 THEN BEGIN
      IF NOT ButtonDown[0] THEN BEGIN
        ButtonDown[0] := True;
        ButtonStateChanged[0] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[0] THEN BEGIN
        ButtonDown[0] := False;
        ButtonStateChanged[0] := True;
      END;
    END;
    IF ButtonDown[0] AND ButtonStateChanged[0] THEN
      ButtonPressed[0] := True;

    IF (Data[8] AND 2) = 2 THEN BEGIN
      IF NOT ButtonDown[1] THEN BEGIN
        ButtonDown[1] := True;
        ButtonStateChanged[1] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[1] THEN BEGIN
        ButtonDown[1] := False;
        ButtonStateChanged[1] := True;
      END;
    END;
    IF ButtonDown[1] AND ButtonStateChanged[1] THEN
      ButtonPressed[1] := True;
   
    IF (Data[8] AND 4) = 4 THEN BEGIN
      IF NOT ButtonDown[2] THEN BEGIN
        ButtonDown[2] := True;
        ButtonStateChanged[2] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[2] THEN BEGIN
        ButtonDown[2] := False;
        ButtonStateChanged[2] := True;
      END;
    END;
    IF ButtonDown[2] AND ButtonStateChanged[2] THEN
      ButtonPressed[2] := True;

    IF (Data[8] AND 8) = 8 THEN BEGIN
      IF NOT ButtonDown[3] THEN BEGIN
        ButtonDown[3] := True;
        ButtonStateChanged[3] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[3] THEN BEGIN
        ButtonDown[3] := False;
        ButtonStateChanged[3] := True;
      END;
    END;
    IF ButtonDown[3] AND ButtonStateChanged[3] THEN
      ButtonPressed[3] := True;

    IF (Data[8] AND 16) = 16 THEN BEGIN
      IF NOT ButtonDown[4] THEN BEGIN
        ButtonDown[4] := True;
        ButtonStateChanged[4] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[4] THEN BEGIN
        ButtonDown[4] := False;
        ButtonStateChanged[4] := True;
      END;
    END;
    IF ButtonDown[4] AND ButtonStateChanged[4] THEN
      ButtonPressed[4] := True;

    IF (Data[8] AND 32) = 32 THEN BEGIN
      IF NOT ButtonDown[5] THEN BEGIN
        ButtonDown[5] := True;
        ButtonStateChanged[5] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[5] THEN BEGIN
        ButtonDown[5] := False;
        ButtonStateChanged[5] := True;
      END;
    END;
    IF ButtonDown[5] AND ButtonStateChanged[5] THEN
      ButtonPressed[5] := True;

    IF (Data[8] AND 64) = 64 THEN BEGIN
      IF NOT ButtonDown[6] THEN BEGIN
        ButtonDown[6] := True;
        ButtonStateChanged[6] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[6] THEN BEGIN
        ButtonDown[6] := False;
        ButtonStateChanged[6] := True;
      END;
    END;
    IF ButtonDown[6] AND ButtonStateChanged[6] THEN
      ButtonPressed[6] := True;

    IF (Data[8] AND 128) = 128 THEN BEGIN
      IF NOT ButtonDown[7] THEN BEGIN
        ButtonDown[7] := True;
        ButtonStateChanged[7] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[7] THEN BEGIN
        ButtonDown[7] := False;
        ButtonStateChanged[7] := True;
      END;
    END;
    IF ButtonDown[7] AND ButtonStateChanged[7] THEN
      ButtonPressed[7] := True;

    IF (Data[9] AND 1) = 1 THEN BEGIN
      IF NOT ButtonDown[8] THEN BEGIN
        ButtonDown[8] := True;
        ButtonStateChanged[8] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[8] THEN BEGIN
        ButtonDown[8] := False;
        ButtonStateChanged[8] := True;
      END;
    END;
    IF ButtonDown[8] AND ButtonStateChanged[8] THEN
      ButtonPressed[8] := True;

    IF (Data[9] AND 2) = 2 THEN BEGIN
      IF NOT ButtonDown[9] THEN BEGIN
        ButtonDown[9] := True;
        ButtonStateChanged[9] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[9] THEN BEGIN
        ButtonDown[9] := False;
        ButtonStateChanged[9] := True;
      END;
    END;
    IF ButtonDown[9] AND ButtonStateChanged[9] THEN
      ButtonPressed[9] := True;

    IF (Data[9] AND 4) = 4 THEN BEGIN
      IF NOT ButtonDown[10] THEN BEGIN
        ButtonDown[10] := True;
        ButtonStateChanged[10] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[10] THEN BEGIN
        ButtonDown[10] := False;
        ButtonStateChanged[10] := True;
      END;
    END;
    IF ButtonDown[10] AND ButtonStateChanged[10] THEN
      ButtonPressed[10] := True;

    IF (Data[9] AND 8) = 8 THEN BEGIN
      IF NOT ButtonDown[11] THEN BEGIN
        ButtonDown[11] := True;
        ButtonStateChanged[11] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[11] THEN BEGIN
        ButtonDown[11] := False;
        ButtonStateChanged[11] := True;
      END;
    END;
    IF ButtonDown[11] AND ButtonStateChanged[11] THEN
      ButtonPressed[11] := True;

    IF (Data[9] AND 16) = 16 THEN BEGIN
      IF NOT ButtonDown[12] THEN BEGIN
        ButtonDown[12] := True;
        ButtonStateChanged[12] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[12] THEN BEGIN
        ButtonDown[12] := False;
        ButtonStateChanged[12] := True;
      END;
    END;
    IF ButtonDown[12] AND ButtonStateChanged[12] THEN
      ButtonPressed[12] := True;

    IF (Data[9] AND 32) = 32 THEN BEGIN
      IF NOT ButtonDown[13] THEN BEGIN
        ButtonDown[13] := True;
        ButtonStateChanged[13] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[13] THEN BEGIN
        ButtonDown[13] := False;
        ButtonStateChanged[13] := True;
      END;
    END;
    IF ButtonDown[13] AND ButtonStateChanged[13] THEN
      ButtonPressed[13] := True;

    IF (Data[9] AND 64) = 64 THEN BEGIN
      IF NOT ButtonDown[14] THEN BEGIN
        ButtonDown[14] := True;
        ButtonStateChanged[14] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[14] THEN BEGIN
        ButtonDown[14] := False;
        ButtonStateChanged[14] := True;
      END;
    END;
    IF ButtonDown[14] AND ButtonStateChanged[14] THEN
      ButtonPressed[14] := True;

    IF (Data[9] AND 128) = 128 THEN BEGIN
      IF NOT ButtonDown[15] THEN BEGIN
        ButtonDown[15] := True;
        ButtonStateChanged[15] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[15] THEN BEGIN
        ButtonDown[15] := False;
        ButtonStateChanged[15] := True;
      END;
    END;
    IF ButtonDown[15] AND ButtonStateChanged[15] THEN
      ButtonPressed[15] := True;

    IF (Data[10] AND 1) = 1 THEN BEGIN
      IF NOT ButtonDown[16] THEN BEGIN
        ButtonDown[16] := True;
        ButtonStateChanged[16] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[16] THEN BEGIN
        ButtonDown[16] := False;
        ButtonStateChanged[16] := True;
      END;
    END;
    IF ButtonDown[16] AND ButtonStateChanged[16] THEN
      ButtonPressed[16] := True;

    IF (Data[10] AND 2) = 2 THEN BEGIN
      IF NOT ButtonDown[17] THEN BEGIN
        ButtonDown[17] := True;
        ButtonStateChanged[17] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[17] THEN BEGIN
        ButtonDown[17] := False;
        ButtonStateChanged[17] := True;
      END;
    END;
    IF ButtonDown[17] AND ButtonStateChanged[17] THEN
      ButtonPressed[17] := True;

    IF (Data[10] AND 4) = 4 THEN BEGIN
      IF NOT ButtonDown[18] THEN BEGIN
        ButtonDown[18] := True;
        ButtonStateChanged[18] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[18] THEN BEGIN
        ButtonDown[18] := False;
        ButtonStateChanged[18] := True;
      END;
    END;
    IF ButtonDown[18] AND ButtonStateChanged[18] THEN
      ButtonPressed[18] := True;

    IF (Data[10] AND 8) = 8 THEN BEGIN
      IF NOT ButtonDown[19] THEN BEGIN
        ButtonDown[19] := True;
        ButtonStateChanged[19] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[19] THEN BEGIN
        ButtonDown[19] := False;
        ButtonStateChanged[19] := True;
      END;
    END;
    IF ButtonDown[19] AND ButtonStateChanged[19] THEN
      ButtonPressed[19] := True;

    IF (Data[10] AND 16) = 16 THEN BEGIN
      IF NOT ButtonDown[20] THEN BEGIN
        ButtonDown[20] := True;
        ButtonStateChanged[20] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[20] THEN BEGIN
        ButtonDown[20] := False;
        ButtonStateChanged[20] := True;
      END;
    END;
    IF ButtonDown[20] AND ButtonStateChanged[20] THEN
      ButtonPressed[20] := True;

    IF (Data[10] AND 32) = 32 THEN BEGIN
      IF NOT ButtonDown[21] THEN BEGIN
        ButtonDown[21] := True;
        ButtonStateChanged[21] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[21] THEN BEGIN
        ButtonDown[21] := False;
        ButtonStateChanged[21] := True;
      END;
    END;
    IF ButtonDown[21] AND ButtonStateChanged[21] THEN
      ButtonPressed[21] := True;

    IF (Data[10] AND 64) = 64 THEN BEGIN
      IF NOT ButtonDown[22] THEN BEGIN
        ButtonDown[22] := True;
        ButtonStateChanged[22] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[22] THEN BEGIN
        ButtonDown[22] := False;
        ButtonStateChanged[22] := True;
      END;
    END;
    IF ButtonDown[22] AND ButtonStateChanged[22] THEN
      ButtonPressed[22] := True;

    IF (Data[10] AND 128) = 128 THEN BEGIN
      IF NOT ButtonDown[23] THEN BEGIN
        ButtonDown[23] := True;
        ButtonStateChanged[23] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[23] THEN BEGIN
        ButtonDown[23] := False;
        ButtonStateChanged[23] := True;
      END;
    END;
    IF ButtonDown[23] AND ButtonStateChanged[23] THEN
      ButtonPressed[23] := True;

    IF (Data[11] AND 1) = 1 THEN BEGIN
      IF NOT ButtonDown[24] THEN BEGIN
        ButtonDown[24] := True;
        ButtonStateChanged[24] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[24] THEN BEGIN
        ButtonDown[24] := False;
        ButtonStateChanged[24] := True;
      END;
    END;
    IF ButtonDown[24] AND ButtonStateChanged[24] THEN
      ButtonPressed[24] := True;

    IF (Data[11] AND 2) = 2 THEN BEGIN
      IF NOT ButtonDown[25] THEN BEGIN
        ButtonDown[25] := True;
        ButtonStateChanged[25] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[25] THEN BEGIN
        ButtonDown[25] := False;
        ButtonStateChanged[25] := True;
      END;
    END;
    IF ButtonDown[25] AND ButtonStateChanged[25] THEN
      ButtonPressed[25] := True;

    IF (Data[11] AND 4) = 4 THEN BEGIN
      IF NOT ButtonDown[26] THEN BEGIN
        ButtonDown[26] := True;
        ButtonStateChanged[26] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[26] THEN BEGIN
        ButtonDown[26] := False;
        ButtonStateChanged[26] := True;
      END;
    END;
    IF ButtonDown[26] AND ButtonStateChanged[26] THEN
      ButtonPressed[26] := True;

    IF (Data[11] AND 8) = 8 THEN BEGIN
      IF NOT ButtonDown[27] THEN BEGIN
        ButtonDown[27] := True;
        ButtonStateChanged[27] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[27] THEN BEGIN
        ButtonDown[27] := False;
        ButtonStateChanged[27] := True;
      END;
    END;
    IF ButtonDown[27] AND ButtonStateChanged[27] THEN
      ButtonPressed[27] := True;

    IF (Data[11] AND 16) = 16 THEN BEGIN
      IF NOT ButtonDown[28] THEN BEGIN
        ButtonDown[28] := True;
        ButtonStateChanged[28] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[28] THEN BEGIN
        ButtonDown[28] := False;
        ButtonStateChanged[28] := True;
      END;
    END;
    IF ButtonDown[28] AND ButtonStateChanged[28] THEN
      ButtonPressed[28] := True;

    IF (Data[11] AND 32) = 32 THEN BEGIN
      IF NOT ButtonDown[29] THEN BEGIN
        ButtonDown[29] := True;
        ButtonStateChanged[29] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[29] THEN BEGIN
        ButtonDown[29] := False;
        ButtonStateChanged[29] := True;
      END;
    END;
    IF ButtonDown[29] AND ButtonStateChanged[29] THEN
      ButtonPressed[29] := True;

    IF (Data[11] AND 64) = 64 THEN BEGIN
      IF NOT ButtonDown[30] THEN BEGIN
        ButtonDown[30] := True;
        ButtonStateChanged[30] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[30] THEN BEGIN
        ButtonDown[30] := False;
        ButtonStateChanged[30] := True;
      END;
    END;
    IF ButtonDown[30] AND ButtonStateChanged[30] THEN
      ButtonPressed[30] := True;

    IF (Data[11] AND 128) = 128 THEN BEGIN
      IF NOT ButtonDown[31] THEN BEGIN
        ButtonDown[31] := True;
        ButtonStateChanged[31] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[31] THEN BEGIN
        ButtonDown[31] := False;
        ButtonStateChanged[31] := True;
      END;
    END;
    IF ButtonDown[31] AND ButtonStateChanged[31] THEN
      ButtonPressed[31] := True;

    IF (Data[12] AND 1) = 1 THEN BEGIN
      IF NOT ButtonDown[32] THEN BEGIN
        ButtonDown[32] := True;
        ButtonStateChanged[32] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[32] THEN BEGIN
        ButtonDown[32] := False;
        ButtonStateChanged[32] := True;
      END;
    END;
    IF ButtonDown[32] AND ButtonStateChanged[32] THEN
      ButtonPressed[32] := True;

    IF (Data[12] AND 2) = 2 THEN BEGIN
      IF NOT ButtonDown[33] THEN BEGIN
        ButtonDown[33] := True;
        ButtonStateChanged[33] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[33] THEN BEGIN
        ButtonDown[33] := False;
        ButtonStateChanged[33] := True;
      END;
    END;
    IF ButtonDown[33] AND ButtonStateChanged[33] THEN
      ButtonPressed[33] := True;

    IF (Data[12] AND 4) = 4 THEN BEGIN
      IF NOT ButtonDown[34] THEN BEGIN
        ButtonDown[34] := True;
        ButtonStateChanged[34] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[34] THEN BEGIN
        ButtonDown[34] := False;
        ButtonStateChanged[34] := True;
      END;
    END;
    IF ButtonDown[34] AND ButtonStateChanged[34] THEN
      ButtonPressed[34] := True;

    IF (Data[12] AND 8) = 8 THEN BEGIN
      IF NOT ButtonDown[35] THEN BEGIN
        ButtonDown[35] := True;
        ButtonStateChanged[35] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[35] THEN BEGIN
        ButtonDown[35] := False;
        ButtonStateChanged[35] := True;
      END;
    END;
    IF ButtonDown[35] AND ButtonStateChanged[35] THEN
      ButtonPressed[35] := True;

    IF (Data[12] AND 16) = 16 THEN BEGIN
      IF NOT ButtonDown[36] THEN BEGIN
        ButtonDown[36] := True;
        ButtonStateChanged[36] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[36] THEN BEGIN
        ButtonDown[36] := False;
        ButtonStateChanged[36] := True;
      END;
    END;
    IF ButtonDown[36] AND ButtonStateChanged[36] THEN
      ButtonPressed[36] := True;

    IF (Data[12] AND 32) = 32 THEN BEGIN
      IF NOT ButtonDown[37] THEN BEGIN
        ButtonDown[37] := True;
        ButtonStateChanged[37] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[37] THEN BEGIN
        ButtonDown[37] := False;
        ButtonStateChanged[37] := True;
      END;
    END;
    IF ButtonDown[37] AND ButtonStateChanged[37] THEN
      ButtonPressed[37] := True;

    IF (Data[12] AND 64) = 64 THEN BEGIN
      IF NOT ButtonDown[38] THEN BEGIN
        ButtonDown[38] := True;
        ButtonStateChanged[38] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[38] THEN BEGIN
        ButtonDown[38] := False;
        ButtonStateChanged[38] := True;
      END;
    END;
    IF ButtonDown[38] AND ButtonStateChanged[38] THEN
      ButtonPressed[38] := True;

    IF (Data[12] AND 128) = 128 THEN BEGIN
      IF NOT ButtonDown[39] THEN BEGIN
        ButtonDown[39] := True;
        ButtonStateChanged[39] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[39] THEN BEGIN
        ButtonDown[39] := False;
        ButtonStateChanged[39] := True;
      END;
    END;
    IF ButtonDown[39] AND ButtonStateChanged[39] THEN
      ButtonPressed[39] := True;

    IF (Data[13] AND 1) = 1 THEN BEGIN
      IF NOT ButtonDown[40] THEN BEGIN
        ButtonDown[40] := True;
        ButtonStateChanged[40] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[40] THEN BEGIN
        ButtonDown[40] := False;
        ButtonStateChanged[40] := True;
      END;
    END;
    IF ButtonDown[40] AND ButtonStateChanged[40] THEN
      ButtonPressed[40] := True;

    IF (Data[13] AND 2) = 2 THEN BEGIN
      IF NOT ButtonDown[41] THEN BEGIN
        ButtonDown[41] := True;
        ButtonStateChanged[41] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[41] THEN BEGIN
        ButtonDown[41] := False;
        ButtonStateChanged[41] := True;
      END;
    END;
    IF ButtonDown[41] AND ButtonStateChanged[41] THEN
      ButtonPressed[41] := True;

    IF (Data[13] AND 4) = 4 THEN BEGIN
      IF NOT ButtonDown[42] THEN BEGIN
        ButtonDown[42] := True;
        ButtonStateChanged[42] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[42] THEN BEGIN
        ButtonDown[42] := False;
        ButtonStateChanged[42] := True;
      END;
    END;
    IF ButtonDown[42] AND ButtonStateChanged[42] THEN
      ButtonPressed[42] := True;

    IF (Data[13] AND 8) = 8 THEN BEGIN
      IF NOT ButtonDown[43] THEN BEGIN
        ButtonDown[43] := True;
        ButtonStateChanged[43] := True;
      END;
    END ELSE BEGIN
      IF ButtonDown[43] THEN BEGIN
        ButtonDown[43] := False;
        ButtonStateChanged[43] := True;
      END;
    END;
    IF ButtonDown[43] AND ButtonStateChanged[43] THEN
      ButtonPressed[43] := True;
  END; {WITH}

  { Return true if any button has been pressed }
  FOR ButtonNo := 0 TO 43 DO
    IF ButtonPressed[ButtonNo] THEN
      Result := True;
END; { CheckButtons }

PROCEDURE ResetButton(ButtonNum : Integer);
{ Reset button state to unpressed }
BEGIN
  ButtonStateChanged[ButtonNum] := False;
  ButtonPressed[ButtonNum] := False;
END; { ResetButton }

PROCEDURE ResetAllButtons;
{ Reset all button states to unpressed }
VAR
  ButtonNo : Integer;

BEGIN
  FOR ButtonNo := 0 TO 43 DO BEGIN
    ButtonDown[ButtonNo] := False;
    ButtonStateChanged[ButtonNo] := False;
    ButtonPressed[ButtonNo] := False;
  END;
END; { ResetAllButtons }

PROCEDURE TRailDriverWindow.LEDTimerTick(Sender: TObject);
BEGIN
  { First write out any characters that need writing to the LED }
  IF Length(LEDLongStr) > 0 THEN BEGIN
    { Add three spaces at the end, to clear the display when the string has scrolled }
    IF NOT LEDLongStrInitialised THEN BEGIN
      Log('A LED scroll: ' + LEDLongStr);
      LEDLongStr := StringOfChar(' ', 3) + LEDLongStr;
      LEDLongStr := LEDLongStr + StringOfChar(' ', 3);
      LEDLongStrInitialised := True;
    END;

    WriteToRailDriverLEDs(Copy(LEDLongStr, 1, 3));
    LEDLongStr := Copy(LEDLongStr, 2, Length(LEDLongStr) - 1);
    IF LEDLongStr = '' THEN
      LEDLongStrInitialised := False;
  END;
END; { LEDTimerTick }

PROCEDURE TRailDriverWindow.RailDriverTimerTick(Sender: TObject);
VAR
  DataReadResult : Integer;
  I : Integer;
  NewDigit : String;
  LocoChipByButtonAsInteger : Integer;
  T : TrainIndex;

BEGIN
  T := 0;

  { See if the controls are set to neural positions }
  IF ButtonPressed[27] THEN BEGIN
    LEDLongStr := 'hello daniel!';
    ResetButton(27);
  END;

  IF ButtonPressed[26] THEN BEGIN
    LEDLongStr := 'hello francis!';
    ResetButton(26);
  END;

  DataReadResult := ReadData(RailDriverDeviceHandle, DataReadIn[0]);
  IF DataReadResult = 0 THEN BEGIN
    ReverserData := DataReadIn[1];
    RegulatorData := DataReadIn[2];
    { Check the controls are in neutral so we're ready to start }
    IF NOT RailDriverReady THEN BEGIN
      IF (GetReverserDirection = UnknownDirection) AND (GetRegulatorDirection = UnknownDirection) THEN BEGIN
        RailDriverReady := True;
        { And reset any buttons that have been pressed before now }
        ResetAllButtons;
        { And clear the LED display }
        WriteToRailDriverLEDs('  .');
      END ELSE
        { the reverser is not in neutral }
        IF NOT RailDriverNotReadyMsg THEN BEGIN
          IF GetReverserDirection <> UnknownDirection THEN
            Debug('=Put RDC Reverser into neutral');
          IF GetRegulatorDirection <> UnknownDirection THEN
            Debug('=Put RDC Regulator into neutral');
          RailDriverNotReadyMsg := True;
          WriteToRailDriverLEDs('Err');
        END;
    END;

    TrainBrakeData := DataReadIn[3];
    LocoBrakeData := DataReadIn[4];
    BailOffData := DataReadIn[5];
    ThreeWaySwitchAData := DataReadIn[6];
    ThreeWaySwitchBData := DataReadIn[7];

    ReverserValue.Caption := 'R=' + IntToStr(ReverserData);
    RegulatorValue.Caption := 'T=' + IntToStr(RegulatorData) + ' ';
    TrainBrakeValue.Caption := 'A=' + IntToStr(TrainBrakeData) + ' ';
    LocoBrakeValue.Caption := 'L=' + IntToStr(LocoBrakeData) + ' ';
    BailOffValue.Caption := 'B=' + IntToStr(BailOffData) + ' ';
    ThreeWaySwitchAValue.Caption := '3a=' + IntToStr(ThreeWaySwitchAData) + ' ';
    ThreeWaySwitchBValue.Caption := '3b=' + IntToStr(ThreeWaySwitchBData) + ' ';

    IF RailDriverInitialised THEN BEGIN
      { with the calibration done, we can work out where the progress bars should be }
      CASE GetReverserDirection OF
        Up:
          ReverserForwardProgressBar.Position := 100;
        Down:
          ReverserReverseProgressBar.Position := 100;
      ELSE
        BEGIN
          ReverserForwardProgressBar.Position := 0;
          ReverserReverseProgressBar.Position := 0;
        END;
      END; {CASE}

      CASE GetRegulatorDirection OF
        Up:
          RegulatorForwardProgressBar.Position := 100;
        Down:
          RegulatorReverseProgressBar.Position := 100;
      ELSE
        BEGIN
          RegulatorForwardProgressBar.Position := 0;
          RegulatorReverseProgressBar.Position := 0;
        END;
      END; {CASE}
      RegulatorPosProgressBar.Position := GetRegulatorPos;
    END;

    CheckButtons(DataReadIn);
  END;

  IF NOT RailDriverInitialised THEN BEGIN
//    IF MessageDialogueWithDefault('Calibrate RailDriver?', StopTimer, mtConfirmation, [mbYes, mbNo], mbYes) = mrNo THEN
//      CheckCloseRailDriverWindow(OK)
//    ELSE
      DoCalibration
  END ELSE
    IF RailDriverReady THEN BEGIN
      { otherwise process any data }
      { See if loco selection is cancelled }
      IF ButtonPressed[39] AND ButtonPressed[41]
      AND (LocoSelectionInProgress OR LocoSelected)
      THEN BEGIN
        ResetButton(39);
        ResetButton(41);
        IF LocoSelected THEN
          Log('AG Loco control by RDC cancelled')
        ELSE
          Log('AG Loco selection by RDC cancelled');
        WriteToRailDriverLEDs('  .');
        IF LocoSelectionInProgress THEN BEGIN
          LocoChipByButton := '';
          LocoSelectionInProgress := False;
        END ELSE
          IF LocoSelected THEN BEGIN
            { delete the train record (if any) }
            LocoSelected := False;
//            { where is T coming from? **** }
//            IF T <> NIL THEN BEGIN
//              RemoveTrainFromDiagrams(T);
//              T := NIL;
//            END;
          END;
      END ELSE BEGIN
        IF ButtonPressed[40] { OR (Length(Train_LocoChipByButton) = 4) } THEN BEGIN
          ResetButton(40);
          IF NOT LocoSelectionInProgress AND NOT LocoSelected THEN BEGIN
            LocoSelectionInProgress := True;
            WriteToRailDriverLEDs('LS ');
            Debug('Selecting a loco by RDC:');
            LocoChipByButton := '';
          END ELSE BEGIN
            IF NOT LocoSelected THEN BEGIN
              { see if it exists }
              LEDAppendString := '';
              IF LocoChipByButton = '' THEN BEGIN
                { no number entered }
                Log('AG Loco selection by RDC cancelled');
                WriteToRailDriverLEDs('  .');
                LocoSelectionInProgress := False;
              END ELSE BEGIN
                LocoChipByButtonAsInteger := StrToInt(LocoChipByButton);
                T := 0;
                LocoExists := False;
                WHILE (T <= High(Trains)) AND NOT LocoExists DO BEGIN
                  IF Trains[T].Train_LocoChip = LocoChipByButtonAsInteger THEN
                    LocoExists := True
                  ELSE
                    Inc(T);
                END; {WHILE}

                IF NOT LocoExists THEN BEGIN
                  Log('AG Loco ' + LocoChipByButton + ' does not exist' + ' - loco selection by RDC cancelled');
                  WriteToRailDriverLEDs('err');
                  LocoSelectionInProgress := False;
                END ELSE BEGIN
                  Log('AG Loco ' + LocoChipByButton + ' selected');

                  { see if the loco is already }

                  WriteToRailDriverLEDs(' ok');
                  LocoSelected := True;
                  LocoSelectionInProgress := False;
                END;
             END;
            END;
          END;
        END;

        IF LocoSelectionInProgress THEN BEGIN
          NewDigit := '';
          { Now read in a number }
          FOR I := 0 TO 4 DO BEGIN
            { buttons 1 - 5 }
            IF ButtonPressed[I] THEN BEGIN
              NewDigit := IntToStr(I + 1);
              ResetButton(I);
            END;
          END;
          FOR I := 14 TO 17 DO BEGIN
            { buttons 6 - 9 }
            IF ButtonPressed[I] THEN BEGIN
              NewDigit := IntToStr(I - 8);
              ResetButton(I);
            END;
          END;
          IF ButtonPressed[18] THEN BEGIN
            { and deal with Button 18 specially }
            NewDigit := '0';
            ResetButton(18);
          END;

          LocoChipByButton := LocoChipByButton + NewDigit;
          IF NewDigit <> '' THEN BEGIN
            AppendToRailDriverLEDs(NewDigit);
            Debug(NewDigit);
          END;
        END;

        IF T <> 0 THEN BEGIN
//          T^.Train_ControlledByRDC := True; &&&&&
          SetSpeedByRailDriverConsole(T);
        END;
      END;
    END;
END; { TRailDriverForm.RailDriverTimerTick }

PROCEDURE TRailDriverWindow.WriteButtonClick(Sender: TObject);
BEGIN
  IF RailDriverInitialised THEN BEGIN
    Inc(TestWriteCount);
    IF TestWriteCount > 999 THEN
      TestWriteCount := 0;
    WriteToRailDriverLEDs(IntToStr(TestWriteCount));
  END;
END; { WriteButtonClick }

PROCEDURE InitialiseRailDriverConsole;
{ Set up the RailDriver console so the reads from the analogue devices can be interpreted }
BEGIN
  EnumerateResult := CheckRailDriverConsoleIsAttached;
  IF EnumerateResult <> 0 THEN
    Log('XG RailDriver console initialisation failed - ' + WriteRailDriverError(EnumerateResult))
  ELSE BEGIN
    RailDriverInitialised := False;
    RailDriverWindow.RailDriverTimer.Enabled := True;
    { If the RailDriver hasn't previously been set up, do the calibration - look at one variable }
    IF RDCBailOffMin <> 0 THEN
      BailOffMinCalibrated := True;
    IF RDCBailOffMin <> 0 THEN
      BailOffMaxCalibrated := True;
    IF RDCEmergencyBrakeMin <> 0 THEN
      EmergencyBrakeCalibrated := True;
    IF RDCEmergencyBrakeMax <> 0 THEN
      EmergencyBrakeCalibrated := True;
    IF RDCLocoBrakeMin <> 0 THEN
      LocoBrakeMinCalibrated := True;
    IF RDCLocoBrakeMax <> 0 THEN
      LocoBrakeMaxCalibrated := True;
    IF RDCRegulatorForwardMin <> 0 THEN
      RegulatorForwardMinCalibrated := True;
    IF RDCRegulatorForwardMax <> 0 THEN
      RegulatorForwardMaxCalibrated := True;
    IF RDCRegulatorReverseMin <> 0 THEN
      RegulatorReverseMinCalibrated := True;
    IF RDCRegulatorReverseMax <> 0 THEN
      RegulatorReverseMaxCalibrated := True;
    IF RDCReverserForwardMin <> 0 THEN
      ReverserForwardMinCalibrated := True;
    IF RDCReverserForwardMax <> 0 THEN
      ReverserForwardMaxCalibrated := True;
    IF RDCReverserReverseMin <> 0 THEN
      ReverserReverseMinCalibrated := True;
    IF RDCReverserReverseMax <> 0 THEN
      ReverserReverseMaxCalibrated := True;
    IF RDCThreeWaySwitchALeftNum <> 0 THEN
      ThreeWaySwitchALeftNumCalibrated := True;
    IF RDCThreeWaySwitchAMidNum <> 0 THEN
      ThreeWaySwitchAMidNumCalibrated := True;
    IF RDCThreeWaySwitchARightNum <> 0 THEN
      ThreeWaySwitchARightNumCalibrated := True;
    IF RDCThreeWaySwitchBLeftNum <> 0 THEN
      ThreeWaySwitchBLeftNumCalibrated := True;
    IF RDCThreeWaySwitchBMidNum <> 0 THEN
      ThreeWaySwitchBMidNumCalibrated := True;
    IF RDCThreeWaySwitchBRightNum <> 0 THEN
      ThreeWaySwitchBRightNumCalibrated := True;
    IF RDCTrainBrakeMin <> 0 THEN
      TrainBrakeMinCalibrated := True;
    IF RDCTrainBrakeMax <> 0 THEN
      TrainBrakeMaxCalibrated := True;

    IF BailOffMinCalibrated AND BailOffMaxCalibrated
    AND EmergencyBrakeCalibrated
    AND LocoBrakeMinCalibrated AND LocoBrakeMaxCalibrated
    AND RegulatorForwardMinCalibrated AND RegulatorForwardMaxCalibrated
    AND RegulatorReverseMinCalibrated AND RegulatorReverseMaxCalibrated
    AND ReverserForwardMinCalibrated AND ReverserForwardMaxCalibrated
    AND ReverserReverseMinCalibrated AND ReverserReverseMaxCalibrated
    AND ThreeWaySwitchALeftNumCalibrated AND ThreeWaySwitchAMidNumCalibrated
    AND ThreeWaySwitchARightNumCalibrated AND ThreeWaySwitchBLeftNumCalibrated
    AND ThreeWaySwitchBMidNumCalibrated AND ThreeWaySwitchBRightNumCalibrated
    AND TrainBrakeMinCalibrated AND TrainBrakeMaxCalibrated
    THEN BEGIN
      WriteToRailDriverLEDs('  .');
      Log('AG RailDriver console initialised');
      RailDriverInitialised := True;
      RailDriverReady := False;
    END ELSE
      { If the RailDriver hasn't previously been set up, do the calibration }
      WriteToRailDriverLEDs('CAL');
  END;
END; { InitialiseRailDriverConsole }

PROCEDURE StartRailDriver;
{ Start up the Rail Driver, calibrating it if necessary }
BEGIN
  InitialiseRailDriverConsole;
  ResetAllButtons;
END; { StartRailDriver }

PROCEDURE TRailDriverWindow.RailDriverWindowClose(Sender: TObject; VAR Action: TCloseAction);
VAR
  OK : Boolean;

BEGIN
  CheckCloseRailDriverWindow(OK);
  IF NOT OK THEN
    Action := caNone
  ELSE BEGIN
    Log('AG RailDriver window closed - RDC Mode off');
    SetMode(RDC, False);
  END;
END; { RailDriverWindowClose }

PROCEDURE TRailDriverWindow.RailDriverWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    { Exclude most non-alphanumeric keys }
    vk_Shift, vk_Control, vk_Menu { Alt }, vk_Pause, vk_SnapShot { PrtSc }, vk_LWin, vk_RWin, vk_Apps { Windows Applications },
    vk_Numlock, vk_Scroll, vk_Cancel { Ctrl-Break}, vk_Capital { Caps Lock }:
      { do nothing };
  ELSE {CASE}
    KeyPressedDown(Key, ShiftState);
  END; {CASE}
END; { RailDriverWindowKeyDown }

END { RDCUnit }.
