UNIT InitVars;
{ Initialises variables StrPas

  v2.0  08/09/06 Signal data now read in from MSAccess database
}
// {$DEFINE RDC}

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Types, StdCtrls, ComCtrls, ExtCtrls, DB, ADODB, System.UITypes;

TYPE
  TInitVarsWindow = CLASS(TForm)
    AreasDataSource : TDataSource;
    AreasADOConnection : TADOConnection;
    AreasADOTable : TADOTable;
    FeedbackUnitsADOConnection: TADOConnection;
    FeedbackUnitsADOTable: TADOTable;
    FeedbackUnitDataSource : TDataSource;
    LinesDataSource: TDataSource;
    LinesDataSource2: TDataSource;
    LinesADOConnection: TADOConnection;
    LinesADOConnection2: TADOConnection;
    LinesADOTable: TADOTable;
    LinesADOTable2: TADOTable;
    LocationsADOConnection : TADOConnection;
    LocationsADOConnection2: TADOConnection;
    LocationsADOTable : TADOTable;
    LocationsADOTable2: TADOTable;
    LocationsDataSource : TDataSource;
    LocationsDataSource2: TDataSource;
    PlatformsADOConnection: TADOConnection;
    PlatformsADOTable: TADOTable;
    PlatformDataSource : TDataSource;
    PointsADOConnection : TADOConnection;
    PointsADOConnection2 : TADOConnection;
    PointsADOTable : TADOTable;
    PointsADOTable2 : TADOTable;
    PointsDataSource : TDataSource;
    PointsDataSource2 : TDataSource;
    SaveDialogue : TSaveDialog;
    SignalsADOConnection : TADOConnection;
    SignalsADOConnection2 : TADOConnection;
    SignalsADOTable : TADOTable;
    SignalsADOTable2 : TADOTable;
    SignalsDataSource : TDataSource;
    SignalsDataSource2 : TDataSource;
    StationMonitorsWebDiagnosticsMemo: TMemo;
    TrackCircuitsADOConnection: TADOConnection;
    TrackCircuitsADOConnection2: TADOConnection;
    TrackCircuitsADOTable: TADOTable;
    TrackCircuitsADOTable2: TADOTable;
    TrackCircuitDataSource : TDataSource;
    TrackCircuitDataSource2: TDataSource;
    UnderwayOrCompleted1Label : TLabel;
    UnderwayOrCompleted2Label : TLabel;
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

CONST
  AppendToFile = True;

VAR
  InitVarsWindow : TInitVarsWindow;

CONST
  ProgramTitle = 'FWP''s Railway Program for Windows';
  CopyrightStatementForDisplaying = 'Copyright © F.W. Pritchard && A.M. Stokes 1988-2015';
  CopyrightStatementForWritingToFile = 'Copyright © F.W. Pritchard & A.M. Stokes 1988-2015';

  CRLF : ARRAY [1..2] OF Char = #13#10;

  { what the predefined colours mean (BGR not RGB):
  ClAqua = 16776960     $00 FF FF 00
  clBlack = 0           $00 00 00 00
  clBlue = 16711680     $00 FF 00 00
  clCream = 15793151    $00 F0 FB FF
  clLtGray = 12632256   $00 C0 C0 C0
  clDkGray = 8421504    $00 80 80 80
  clFuchsia = 16711935  $00 FF 00 FF
  clGreen = 32768       $00 00 80 00
  clLime = 65280        $00 00 FF 00
  ClMaroon = 128        $00 00 00 80
  clNavy = 8388608      $00 80 00 00
  clPurple = 8388736    $00 80 00 80
  ClRed = 255           $00 00 00 FF
  clTeal = 8421376      $00 80 80 00
  clWhite = 16777215    $00 FF FF FF
  ClYellow = 65535      $00 00 FF FF
  }
  { Don't know why Delphi doesn't predefine the following (change ColourToStr and StrToColour in MiscUtils if change any of these) }
  clFWPDkBlue = $00802040;         { 0 128  32  64 }
  clFWPOrange = $000080FF;         { 0   0 128 255 }
  clFWPLtBrown = $004080FF;        { 0  64 128 255 }
  clFWPDkBrown = $00004080;        { 0   0  64 128 }
  clFWPPink = $008080FF;           { 0 128 128 255 }
  clFWPPlatformColour = $0038ABB1; { 0  56 171 177 }
  clFWPDkGrey = $00242424;         { 0  36  36  36 }
  clFWPVeryDkGrey = $00212121;     { 0  33  33  33 }

  clFWPDkBlueStr = 'clFWPDkBlue';
  clFWPOrangeStr = 'clFWPOrange';
  clFWPLtBrownStr = 'clFWPLtBrown';
  clFWPDkBrownStr = 'clFWPDkBrown';
  clFWPPinkStr = 'clFWPPink';
  clFWPPlatformColourStr = 'clFWPPlatformColour';
  clFWPDkGreyStr = 'clFWPDkGrey';
  clFWPVeryDkGreyStr = 'clFWPVeryDkGrey';

  FirstRouteCreationHeldMsgNumber = 1;
  LastRouteCreationHeldMsgNumber = 12;

  ActiveTrain = True;
  Bold = True;
  ByUser = True;
  Checked = True;
  ForceARead = True;
  ForceAWrite = True;
  ForcePoint = True;
  HelpRequired = True;
  Highlight = True;
  LightLoco = True;
  NewData = True;
  NonMovingLoco = True;
  QuickStop = -1;
  SoundWarning = True;
  StopTimer = True;
  TrainExists = True;
  TurnOn = True;
  TurnOff = False;
  UndrawRequired = True;
  UndrawToBeAutomatic = True;
  UnknownArea = 99999;
  UnknownBufferStop = 99999;
  UnknownJourney = 99999;
  UnknownLine = 99999;
  UnknownLocation = 99999;
  UnknownLocoChip = 99999;
  UnknownLocoIndex = 99999;
  UnknownLocoClass = 99999;
  UnknownSubRoute = 99999;
  UnknownPoint = 99999;
  UnknownRoute = 99999;
  UnknownRow = 99999;
  UnknownSignal = 99999;
  UnknownTrackCircuit = 99999;
  UnknownTrainIndex = 99999;
  UnknownTrainLength = 99999;
  UnknownTrainTypeNum = 99999;
  UnknownTRSPlunger = 99999;
  UserMsgRequired = True;
  NoTC = UnknownTrackCircuit;

  NoRoute = UnknownRoute;
  NoSubRoute = UnknownSubRoute;

  { The status bar panels }
  StatusBarPanel0 = 0;
  StatusBarPanel1 = 1;
  StatusBarPanel2 = 2;
  StatusBarPanel3 = 3;

  Function0 = 0;
  Function1 = 1;
  Function2 = 2;
  Function3 = 3;
  Function4 = 4;
  Function5 = 5;
  Function6 = 6;
  Function7 = 7;
  Function8 = 8;
  Function9 = 9;
  Function10 = 10;
  Function11 = 11;
  Function12 = 12;

TYPE
  TColour = TColor;
  TColourDialogue = TColorDialog;

  BooleanArrayType = ARRAY OF Boolean;
  DateTimeArrayType = ARRAY OF TDateTime;
  IntegerArrayType = ARRAY OF Integer;
  StringArrayType = ARRAY OF String;

  LocoControlStateType = (ControlledByProgram, ControlledByRDC, ControlledByUser, ControlledByUnknownDevice);

  { Various modes }
  ModeType = (AllRouteDebuggingModeType, AnonymousOccupationModeType, FeedbackDebuggingModeType, GeneralDebuggingModeType, LineDebuggingModeType, LockDebuggingModeType,
              LockingModeType, LocoSpeedTimingModeType, LogsCurrentlyKeptModeType, PointDebuggingModeType, PreviousPointSettingsModeType, RDCModeType,
              RecordingMonitorScreensModeType, RecordLineDrawingModeType, RouteDebuggingModeType, RouteBacktrackDebuggingModeType, RouteDrawingModeType,
              ShowAdjacentTrackCircuitModeType, StationStartModeType, TestingModeType);

  DirectionType = (Up, Down, Bidirectional, UnknownDirection);
  DirectionArrayType = ARRAY OF DirectionType;

  AreaType = (Station, Siding, Fiddleyard, HoldingSignal, OtherArea, UnknownAreaType);

  AreaRec = RECORD
    Area_AccessibleAreasUp : IntegerArrayType;
    Area_AccessibleAreasUpStrArray : StringArrayType;
    Area_AccessibleAreasDown : IntegerArrayType;
    Area_AccessibleAreasDownStrArray : StringArrayType;
    Area_IsHoldingArea : Boolean;
    Area_IsReversingArea : Boolean;
    Area_LongStr : String;
    Area_ShortStr : String;
    Area_SortByLocationLength : Boolean;
    Area_StationMonitorsDisplayOrderNum : Integer;
    Area_Type : AreaType;
  END;

TYPE
  HandleType = (UpHandle, MidHandle, DownHandle, NoHandle);
  MPHType = (MPH0, MPH10, MPH20, MPH30, MPH40, MPH50, MPH60, MPH70, MPH80, MPH90, MPH100, MPH110, MPH120, UnknownMPH, NoSpecifiedSpeed);
  NextLineRouteingType = (EndOfLineIsNext, LineIsNext, PointIsNext, UnknownNextLineRouteingType);
  OutOfUseState = (InUse, OutOfUse);

  LightsType = (NoLights, HeadlightsAndTailLightsConnected, HeadlightsAndTailLightsSeparatelySwitched, ExpressModelsSeparateHeadlights, LightsOperatedByTwoChips,
                LightsShouldBeDimmed, CustomLightingKit);
  LightsColourType = (Red, White);

  LocoIndex = Integer;

  LocoRec = RECORD
    Loco_LocoChip : Integer;

    Loco_Accelerating : Boolean;
    Loco_AccelerationAdjustRange : Integer;
    Loco_AccelerationStartTime : TDateTime;
    Loco_AccelerationStr : String;
    Loco_AccelerationTimeInterval : Real;
    Loco_Active : Boolean;
    Loco_ActualNumStr : String;
    Loco_ControlState : LocoControlStateType;
    Loco_CurrentDirection : DirectionType;
    Loco_CurrentLenzSpeed : Integer;
    Loco_Decelerating : Boolean;
    Loco_DesiredLenzSpeed : Integer;
    Loco_Description : String;
    Loco_DiagramFound : Boolean;
    Loco_FixedDirection : DirectionType;
    Loco_FixedLengthInInches : Integer;
    Loco_Functions : ARRAY [0..12] OF Boolean;
    Loco_Functions0To4Byte : Byte;
    Loco_Functions5To12Byte : Byte;
    Loco_LastTC : Integer;
    Loco_HasCabLights : Boolean;
    Loco_HomeArea : Integer;
    Loco_LastLengthInInches : Integer;
    Loco_LastLocation : Integer;
    Loco_LightingChipDown : Integer;
    Loco_LightingChipDownIndex : LocoIndex;
    Loco_LightingChipUp : Integer;
    Loco_LightingChipUpIndex : LocoIndex;
    Loco_LightingChipRecordForChip : Integer;
    Loco_LightsMsg : String;
    Loco_LightsOn : Boolean;
    Loco_LightsType : LightsType;
    Loco_LocoChipStr : String;
    Loco_LocoClassStr : String;
    Loco_LocoName : String;
    Loco_LocoTypeStr : String;
    Loco_MaximumSpeedInMPH : MPHType;
    Loco_NumberOfCarriages : Integer;
    Loco_PreviousControlState : LocoControlStateType;
    Loco_SavedDirection : DirectionType;
    Loco_SaveDesiredLenzSpeed : Integer;
    Loco_Speed10, Loco_Speed20, Loco_Speed30, Loco_Speed40, Loco_Speed50, Loco_Speed60 : Integer;
    Loco_Speed70, Loco_Speed80, Loco_Speed90, Loco_Speed100, Loco_Speed110, Loco_Speed120 : Integer;
    Loco_SpeedArray : ARRAY [1..12] OF Integer;
    Loco_SpeedByte : Byte;
    Loco_SpeedByteReadIn : Boolean;
    Loco_SpeedSettingsMissing : Boolean;
    Loco_SpeedStepMode : Integer;
    Loco_SpeedString : String;
    Loco_TrainIndex : Integer;
    Loco_UseTrailingTrackCircuits : Boolean;
  END;

TYPE
  DirectionPriorityType = (PreferablyUp, UpOnly, TerminatingAtUp, PreferablyDown, DownOnly, TerminatingAtDown, NoDirectionPriority);
  ThroughLocationStateType = (ThroughLocation, NonThroughLocation, UnknownThroughLocationState);
  ThroughOrStoppingPriorityType = (ThroughPriority, StoppingPriority, NoStoppingPriority);
  TrainPriorityType = (ExpressOnly, ExpressPreferred, PassengerOnly, PassengerPreferred, NoTrainPriority);

  PlatformNumberPositionType = (LeftTop, RightTop, CentreTop, LeftBottom, RightBottom, CentreBottom, UnknownPlatformNumberPosition);

  { This relates to how a platform is drawn }
  PlatformRec = RECORD
    Platform_RunningNumber : Integer;
    Platform_Description : String;
    Platform_Height : Integer;
    Platform_LeftLine : Integer;
    Platform_LeftLineAdjustment : Integer;
    Platform_NumberAStr : String;
    Platform_NumberBStr : String;
    Platform_NumberPositionA : PlatformNumberPositionType;
    Platform_NumberPositionB : PlatformNumberPositionType;
    Platform_Rect : TRect;
    Platform_RightLine : Integer;
    Platform_RightLineAdjustment : Integer;
    Platform_RowAbovePlatform : Extended;
    Platform_RowBelowPlatform : Extended;
  END;

CONST
    Platform_RunningNumberFieldName = 'RunningNumber';
    Platform_DescriptionFieldName = 'Description';
    Platform_HeightFieldName = 'Height';
    Platform_LeftLineFieldName = 'LeftLine';
    Platform_LeftLineAdjustmentFieldName = 'LeftLineAdjustment';
    Platform_NumberAStrFieldName = 'PlatformNumberA';
    Platform_NumberBStrFieldName = 'PlatformNumberB';
    Platform_NumberAPositionFieldName = 'PlatformNumberAPosition';
    Platform_NumberBPositionFieldName = 'PlatformNumberBPosition';
    Platform_RightLineFieldName = 'RightLine';
    Platform_RightLineAdjustmentFieldName = 'RightLineAdjustment';
    Platform_RowAbovePlatformFieldName = 'RowAbovePlatform';
    Platform_RowBelowPlatformFieldName = 'RowBelowPlatform';

TYPE
  ReversingAreasRecType = RECORD
    ReversingAreas_AccessibleToFirstStationArea : Boolean;
    ReversingAreas_Area : Integer;
    ReversingAreas_ToFirstStationAreaDirection : DirectionType;
    ReversingAreas_NearestToFirstStationArea : Boolean;
    ReversingAreas_TimeToFirstStationAreaInMinutes : Integer;
  END;

  TRSPlungerTriangleType = RECORD
    PlungerX : Integer;
    PlungerY : Integer;
    PlungerXScaled : Integer;
    PlungerYScaled : Integer;
  END;

  TRSPlungerRec = RECORD
    TRSPlunger_AwaitingPressingMsgWritten : Boolean;
    TRSPlunger_HasBeenPressedMsgWritten : Boolean;
    TRSPlunger_Direction : DirectionType;
    TRSPlunger_FeedbackInput : Integer;
    TRSPlunger_FeedbackUnit : Integer;
    TRSPlunger_Locked : Boolean;
    TRSPlunger_MouseRect : TRect;
    TRSPlunger_Num : Integer;
    TRSPlunger_PlatformNumStr : String;
    TRSPlunger_Present : Boolean;
    TRSPlunger_Pressed : Boolean;
    TRSPlunger_Triangle : TRSPlungerTriangleType;
    TRSPlunger_WaitAfterPlungerPressedInSeconds : Cardinal;
  END;

TYPE
  ProgramOnTheMainType = (ChangeDirectionToUp, ChangeDirectionToDown, ChangeAcceleration, ChangeDeceleration);

TYPE
  SuitableAdditionalTrainsRec = RECORD
    SuitableAdditionalTrains_DepartureTime1 : TDateTime;
    SuitableAdditionalTrains_DepartureTime2 : TDateTime;
    SuitableAdditionalTrains_DepartureTime3 : TDateTime;
    SuitableAdditionalTrains_Direction1 : DirectionType;
    SuitableAdditionalTrains_Direction2 : DirectionType;
    SuitableAdditionalTrains_Direction3 : DirectionType;
    SuitableAdditionalTrains_EndArea1 : Integer;
    SuitableAdditionalTrains_EndArea2 : Integer;
    SuitableAdditionalTrains_EndArea3 : Integer;
    SuitableAdditionalTrains_InUse : Boolean;
    SuitableAdditionalTrains_LengthInInches : Integer;
    SuitableAdditionalTrains_LocoChip : Integer;
    SuitableAdditionalTrains_NumberOfCarriages : Integer;
    SuitableAdditionalTrains_StartArea : Integer;
    SuitableAdditionalTrains_TravelTimeInMinutes1 : Integer;
    SuitableAdditionalTrains_TravelTimeInMinutes2 : Integer;
    SuitableAdditionalTrains_TravelTimeInMinutes3 : Integer;
    SuitableAdditionalTrains_Weighting : Integer;
    SuitableAdditionalTrains_WeightingStr : String;
  END;

  SuitableAdditionalTrainsArrayType = ARRAY OF SuitableAdditionalTrainsRec;

  SubRouteStateType = (SubRouteNotYetSetUp, SubRouteSettingUpInProgress, SubRouteSettingUpStalled, SubRouteSettingUpFailed, SubRouteSettingUpCancelled,
                       SubRouteSettingUpHeld, SubRouteSettingUpHeldByStationSupervisor, SubRouteSettingUpReleased, SubRouteSetUp, SubRouteToBeCleared, SubRouteCleared);
  SubRouteStateArrayType = ARRAY OF SubRouteStateType;

  { Rectangle timing-related type declarations }
  TimedRectangleRec = RECORD
    MaxTimedRectangleUndrawTime : Cardinal;
    SaveTimedRectColour : TColour;
    TimedRect : TRect;
    TimedRectangleDrawnTime : Cardinal;
    TimedRectNum : Integer;
  END;

  TimedRectangleArrayType = ARRAY OF TimedRectangleRec;

TYPE
  WorkingTimetableStatusType = (EntryCreatedFromWorkingTimetable, AdditionalEntryCreated, IncorrectDayOfTheWeekForEntry, EntryCancelled, UnknownEntryStatus);
  DayOfTheWeekType = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, UnknownDayOfTheWeek);
  DaysOfTheWeekSetType = SET OF DayOfTheWeekType;

  WorkingTimetableRecType = RECORD
    WorkingTimetable_DaysOfTheWeekSet : DaysOfTheWeekSetType;
    WorkingTimetable_Direction : DirectionType;
    WorkingTimetable_EntryNumStr : String;
    WorkingTimetable_FinalStationArea : Integer;
    WorkingTimetable_FirstStationArea : Integer;
    WorkingTimetable_FirstStationDepartureTime : TDateTime;
    WorkingTimetable_LastStationArea : Integer;
    WorkingTimetable_LocoChip : Integer;
    WorkingTimetable_PossibleLocoClasses : StringArrayType;
    WorkingTimetable_PreferredNumberOfCarriages : Integer;
    WorkingTimetable_Status : WorkingTimetableStatusType;
    WorkingTimetable_StoppingAtStationAreas : IntegerArrayType;
    WorkingTimetable_TrainTypeNum : Integer;
    WorkingTimetable_TravelTimeInMinutes : Integer;
  END;

  WorkingTimetableRecArrayType = ARRAY OF WorkingTimetableRecType;

  InfoRec = RECORD
    LocoChip : Integer;
    InfoString : String;
  END;

  { Miscellaneous type declarations }
  ArrivalOrDepartureType = (Arrival, Departure);
  LenzConnectionType = (EthernetConnection, USBConnection, NoConnection);
  PointerArrayType = ARRAY OF Pointer;
  RailwayTimeIntervalType = (Normal, Faster, Fastest, Slower);
  ScreenModeType = (DefaultWindowedScreenMode, CustomWindowedScreenMode, FullScreenMode, FullScreenWithStatusBarMode);
  SortOrder = (Unsorted, LocoChipSort, ReverseLocoChipSort, LocoChipAndDepartureTimeSort, TrainTypeSort, DepartureTimeAndTrainTypeSort);
  StationMonitorsType = (StationArrivalsDisplay, StationArrivalsAndDeparturesDisplay, StationDeparturesDisplay, StationClockDisplay);
  StatusBarStateType = (Visible, Hidden);
  StringType = (LongStringType, ShortStringType, VeryShortStringType);

  LenzSystemRec = RECORD
                    EmergencyStop : Boolean;
                    EmergencyOff : Boolean;
                    ProgrammingMode : Boolean;
                    StartMode : Boolean;
                  END;

CONST
  NoTime = True;
  NoUnitRef = True;
  NumberElements = True;

  { Various strings - these have to match those in database fields }
  { make these changeable in due course *** }
  BufferStopAtDownStr = 'Buffer Stop At Down';
  BufferStopAtUpStr = 'Buffer Stop At Up';
  ClearLineString = '[clear line string]';
  NotEndOfLineStr = 'Not End Of Line';
  ProjectedLineAtUpStr = 'Projected Line At Up';
  ProjectedLineAtDownStr = 'Projected Line At Down';

  MainOrGoodsLineStr = 'Main or Goods Line';
  MainLineStr = 'Main Line';
  MainStationLineStr = 'Main Station Line';
  BranchLineDoubleStr = 'Branch Line Double';
  BranchLineSingleStr = 'Branch Line Single';
  BranchStationLineStr = 'Branch Station Line';
  FiddleyardLineStr = 'Fiddleyard Line';
  GoodsLineStr = 'Goods Line';
  IslandStationLineStr = 'Island Station Line';
  NewlyCreatedLineStr = 'Newly Created Line';
  ProjectedLineStr = 'Projected Line';
  SidingLineStr = 'Siding Line';
  SidingsApproachLineStr = 'Sidings Approach Line';
  StationAvoidingLineStr = 'Station Avoiding Line';
  UnknownLineTypeStr = 'Unknown Line Type';
  WindowStationLineStr = 'Window Station Line';

  CallingOnStr = 'Calling On';
  TwoAspectStr = '2 Aspect';
  ThreeAspectStr = '3 Aspect';
  FourAspectStr = '4 Aspect';
  SemaphoreHomeStr = 'Semaphore Home';
  SemaphoreDistantStr = 'Semaphore Distant';

  NoIndicatorStr = 'No Indicator';
  JunctionIndicatorStr = 'Junction Indicator';
  TheatreIndicatorStr = 'Theatre Indicator';
  QueryIndicatorStr = 'Query Indicator';

  UnknownAreaStr = '[unknown area]';
  UnknownBufferStopStr = '[unknown buffer stop]';
  UnknownColourStr = 'Unknown Colour';
  UnknownLineStr = 'Unknown Line';
  UnknownLocoChipStr = '[unknown loco chip]';
  UnknownLocoChipAsZeroesStr = '0000';
  UnknownJourneyStr = '[unknown journey]';
  UnknownLocationStr = 'Unknown Location';
  UnknownPointStr = '[unknown point]';
  UnknownRouteStr = '[unknown route]';
  UnknownSignalStr = '[unknown signal]';
  UnknownTrackCircuitStr = '[unknown track circuit]';
  UnknownTrainStatusStr = '[unknown train status]';
  UnknownTrainTypeStr = '[unknown train type]';
  UnknownTypeStr = '[unknown type]';

  HoldMarker = 'HOLD';

  SetCurrentRailwayTimeCaption = 'Set Current Railway Time';
  SetProgramStartTimeCaption = 'Set Program Start Time';
  SetDaylightStartTimeCaption = 'Set Daylight Start Time';
  SetDaylightEndTimeCaption = 'Set Daylight End Time';

  LevelStr = 'Level';
  RisingIfDownStr = 'Rising If Down';
  RisingIfUpStr = 'Rising If Up';

  DivergingStr = 'Diverging';
  OutOfActionStr = 'Out Of Action';
  StraightStr = 'Straight';
  UnknownStr = 'Unknown';

  NonThroughLocationStr = 'Non Through Location';
  ThroughLocationStr = 'Through Location';

  FeedbackDetectorOutOfUseStr = 'Out Of Use';
  LineFeedbackDetectorStr = 'Line Detector';
  MixedFeedbackDetectorStr = 'Mixed';
  PointFeedbackDetectorStr = 'Point Detector';
  TrackCircuitFeedbackDetectorStr = 'Track Circuit Detector';
  TRSPlungerFeedbackDetectorStr = 'TRS Plunger Detector';
  UnknownFeedbackDetectorStr = 'Unknown';

  LineFeedbackStr = 'Line Feedback';
  PointFeedbackStr = 'Point Feedback';
  TrackCircuitFeedbackStr = 'Track Circuit Feedback';
  TRSPlungerFeedbackStr = 'TRS Plunger Feedback';
  UnknownFeedbackStr = 'Unknown Feedback Type';

  LightLocoStr = 'Light Loco';
  ExpressPassengerStr = 'Express Passenger';
  OrdinaryPassengerStr = 'Ordinary Passenger';
  ExpressFreightStr = 'Express Freight';
  Freight75mphStr = '75mph Freight';
  EmptyCoachingStockStr = 'Empty Coaching Stock';
  Freight60mphStr = '60mph Freight';
  Freight45mphStr = '45mph Freight';
  Freight35mphStr = '35mph Freight';
  InternationalStr = 'International';

  OrdinaryPointStr = 'Ordinary Point';
  CrossOverPointStr = 'Cross Over Point';
  ThreeWayPointAStr = 'Three Way Point A';
  ThreeWayPointBStr = 'Three Way Point B';
  SingleSlipStr = 'Single Slip';
  DoubleSlipStr = 'Double Slip';
  ProtectedPointStr = 'Protected Point';
  CatchPointUpStr = 'Catch Point Up';
  CatchPointDownStr = 'Catch Point Down';
  UnknownPointTypeStr = 'Unknown Point Type';

  FlashingDoubleYellowAspectStr = 'flashing double yellow';
  FlashingSingleYellowAspectStr = 'flashing single yellow';
  DoubleYellowAspectStr = 'double yellow';
  SingleYellowAspectStr = 'single yellow';
  GreenAspectStr = 'green';
  RedAspectStr = 'red';
  NoAspectStr = 'no aspect';

  FlashingDoubleYellowAspectShortStr = 'YY*';
  FlashingSingleYellowAspectShortStr = 'Y*';
  DoubleYellowAspectShortStr = 'YY';
  SingleYellowAspectShortStr = 'Y';
  GreenAspectShortStr = 'G';
  RedAspectShortStr = 'R';
  NoAspectShortStr = 'N';

  SemaphoreAspectOff = 'off';
  SemaphoreAspectOn = 'on';
  UnknownSemaphoreAspect = 'Unknown Semaphore Aspect';

  LeftTopStr = 'Left Top';
  RightTopStr = 'Right Top';
  CentreTopStr = 'Centre Top';
  LeftBottomStr = 'Left Bottom';
  RightBottomStr = 'Right Bottom';
  CentreBottomStr = 'Centre Bottom';
  UnknownPlatformNumberPositionStr = 'Unknown Platform Number Position';

  SolidStr = 'Solid';
  DashStr = 'Dash';
  DotStr = 'Dot';
  DashDotStr = 'DashDot';
  DashDotDotStr = 'DashDotDot';
  ClearStr = 'Clear';
  InsideFrameStr = 'InsideFrame';

  MondayStr = 'Monday';
  TuesdayStr = 'Tuesday';
  WednesdayStr = 'Wednesday';
  ThursdayStr = 'Thursday';
  FridayStr = 'Friday';
  SaturdayStr = 'Saturday';
  SundayStr = 'Sunday';
  UnknownDayofTheWeekStr = 'Unknown Day of the Week';

  MondayShortStr = 'Mon';
  TuesdayShortStr = 'Tue';
  WednesdayShortStr = 'Wed';
  ThursdayShortStr = 'Thu';
  FridayShortStr = 'Fri';
  SaturdayShortStr = 'Sat';
  SundayShortStr = 'Sun';

  BlankLineBeforeLogStr = '{BLANKLINEBEFORE}';
  LineLogStr = '{LINE}';
  LineEqualsLogStr = '{LINE=';
  LineBeforeLogStr = '{LINE=BEFORE}';
  LineAfterLogStr = '{LINE=AFTER}';
  LineBeforAndAfterLogStr = '{LINE=BEFOREANDAFTER}';

  LogSignalData = True;

VAR
  AllSignalsSwitchedOff : Boolean = False;
  Areas : ARRAY OF AreaRec;
  AutoModeInitiated : Boolean = False;
  BreakPointRequiredInMakeSoundRoutine : Boolean = False;
  CreateLineMode : Boolean = False;
  CrossHairCursor : TCursor;
  DapolCleaningWagonLocoChip : Integer = UnknownLocoChip;
  DapolCleaningWagonLocoChipRunning : Boolean = False;
  DayTimeSetByUser : Boolean = False;
  DesiredLenzConnection : LenzConnectionType = NoConnection;
  DiagramsArray : IntegerArrayType;
  DisplayFeedbackStringsInDebugWindow : Boolean = False;
  EditIcon : TIcon;
  EditMode : Boolean = False;
  EmergencyStopMsgDisplayed : Boolean = False;
  EscKeyStored : Boolean = False;
  FullScreenPenWidth : Integer = 5;
  GridInterLineSpacing : Integer = 0;
  InAutoMode : Boolean = False;
  InterLineSpacing : Integer = 0;
  KeyBoardandMouseLocked : Boolean = False;
  LastPointChanged : Integer = UnknownPoint;
  LastTimeAnyPointChanged : TDateTime = 0;
  LenzConnection : LenzConnectionType = NoConnection;
  LineHighlighted : Integer = UnknownLine;
  Locos : ARRAY OF LocoRec;
  LocosStopped : Boolean = True;
  MainPlatformPlungers : ARRAY OF TRSPlungerRec;
  MissingTrainArray : ARRAY [1..9] OF Boolean = (False, False, False, False, False, False, False, False, False);
  MissingTrainCounter : Integer = 0;
  NightTimeSetByUser : Boolean = False;
  NoFeedBackList : StringArrayType;
  NumberLines : Boolean = False;
  OfflineIcon : TIcon;
  OnlineIcon : TIcon;
  Platforms : ARRAY OF PlatformRec;
  PointHighlighted : Integer = UnknownPoint;
  PointResettingMode : Boolean = True;
  PointResettingToDefaultStateArray : IntegerArrayType;
  PostEmergencyTime : TDateTime = 0;
  PostEmergencyTimeSet : Boolean = False;
  PreviousPointSettingsMode : Boolean = False;
  ProgramShuttingDown : Boolean = False;
  ProgramStarting : Boolean = True;
  RailWindowBitmapCanvasPenWidth : Integer;
  RailDriverCalibrated : Boolean = False;
  RailDriverCalibrationStarted : Boolean = False;
  RailDriverInitialised : Boolean = False;
  ReadOutAdjacentSignalNumber : Boolean = False;
  ReadOutDecoderNumber : Boolean = False;
  ReadOutTCInFull : Boolean = False;
  ReadOutTCOnce : Boolean = False;
  RedrawScreen : Boolean = False;
  ReinitialiseFWPRailWindowVariables : Boolean = False;
  ReplayMode : Boolean = False;
  ReplayScrollDown : Boolean = False;
  ReplayScrollUp : Boolean = False;
  ReplaySignalChangeSearch : Boolean = False;
  ResetAllSwitchedPoints : Boolean = False;
  ResizeMap : Boolean = False;
  Restart : Boolean = False;
  RestoreLogsToPreviousState : Boolean = False;
  ReversingAreas : ARRAY OF ReversingAreasRecType;
  RouteClearingOnlyMode : Boolean = False;
  RouteWritingCancel : Boolean = False;
  SaveStationMonitorsCurrentArea : Integer = UnknownArea;
  SaveLocoDialogueMaskEditText : String = '';
  SaveMouseCursorPos : TPoint;
  SaveRouteInfoStr : String = '';
  ScreenColoursSetForPrinting : Boolean = False;
  ScreenMode : ScreenModeType = DefaultWindowedScreenMode;
  ShowCreateRouteExitFunctionNum : Boolean = False;
  ShowRouteLengths : Boolean = False;
  SourcePos, Destination1Pos, Destination2Pos, Destination3Pos : Integer;
  StationStartModeSetUpTime : TDateTime = 0;
  StationMonitorDisplay : StationMonitorsType;
  StationMonitorsAlreadySetUp : Boolean = False;
  StationMonitorsCurrentArea : Integer = UnknownArea;
  StationMonitorsCurrentDisplayOrderNum : Integer = 0;
  Stop : MPHType = MPH0;
  StopAllLocomotivesWhenDebugCalled : Boolean = False;
  SystemOnline : Boolean = False;
  SystemSetOfflineByCommandLineParameter : Boolean = False;
  SystemStatusStr : String = '';
  TerminatingSpeedReductionMode : Boolean = False;
  TextWindowHeight : Word;
  ThinLineMode : Boolean = True;
  TimeLastDataReceived : Cardinal = 0;
  TimeOutMsgWritten : Boolean = False;
  UpLineY, DownLineY : Word;
  VerboseFlag : Boolean = False;
  WatchdogTimerCount : Integer = 0;
  WindowPenWidth : Integer = 1;
  WorkingTimetableRecArray : WorkingTimetableRecArrayType;
  Zooming : Boolean = False;

  { Various file related variables }
  ErrorLogFile, LargeLogFile, LocoLogFile, ReplayFile, RouteLogFile, SignalPointAndTCLogFile, DiagramsLogFile, WorkingTimetableLogFile, TestLogFile : TextFile;

  DebugWindowLines : ARRAY OF String;
  DebugCounter : Integer = 0; { may be used for debugging number of times things written for instance }

  { and various log file data }
  LogFileOpen : Boolean = False;
  MultipleLogFilesRequired : Boolean = False;
  WriteToLogFileAndTestFile : Boolean = False;

  { Other variables }
  ShowAreas : Boolean = False;
  ShowByteParam : String;
  ShowOneFeedbackUnitOnly : Boolean = False;
  ShowFeedbackUnitNum : Integer = 0;
  ShowLenzPointNumbers : Boolean = False;
  ShowLenzPointUnitGroups : Boolean = False;
  ShowLineDetail : Boolean = False;
  ShowLineOccupationDetail : Boolean = True;
  ShowLineDirectionDetail : Boolean = False;
  ShowLineGradients : Boolean = False;
  ShowLineHandles : Boolean = False;
  ShowLineNumbers : Boolean = False;
  ShowLinesUpXAbsoluteValue : Boolean = False;
  ShowLinesWhichLockPoints : Boolean = False;
  ShowLinesWithoutTrackCircuits : Boolean = False;
  ShowLocationLengthDetail : Boolean = False;
  ShowLocations : Boolean = False;
  ShowMouseRectangles : Boolean = False;
  ShowPointDefaultState : Boolean = False;
  ShowPointDetail : Boolean = False;
  ShowPointFeedbackDataInSeparateColours : Boolean = False;
  ShowPointFeedbackDataInUse : Boolean = False;
  ShowPointsStraightAndDiverging : Boolean = False;
  ShowPointsThatAreLocked : Boolean = False;
  ShowPointType : Boolean = False;
  ShowSignalAndBufferStopNums : Boolean = False;
  ShowSignalHiddenStationSignalAspects : Boolean = False;
  ShowSignalJunctionDestinations : Boolean = False;
  ShowSignalResettingTrackCircuitsInStatusBar : Boolean = False;
  ShowSignalsAndBufferStopsWithTheatreDestinations : Boolean = False;
  ShowSignalsWhereRouteingCanBeHeldAndThoseNotUsedForRouteing : Boolean = False;
  ShowSignalsWithAdjacentTrackCircuits : Boolean = False;
  ShowTrackCircuitFeedbackDataInSeparateColours : Boolean = False;
  ShowTrackCircuitFeedbackDataInUse : Boolean = False;
  ShowTrackCircuitLengths : Boolean = False;
  ShowTrackCircuits : Boolean = False;
  ShowTrackCircuitsRoutedOver : Boolean = False;

  { Various diagrams window popup related variables }
  DiagramsChanged : Boolean = False;
  DiagramsLineToAmend : String;
  DiagramsLoaded : Boolean = False;

  { Time related variables }
  SaveActualTime : TDateTime;

  SaveBrushColour : TColour;
  SaveBrushStyle : TBrushStyle;
  SaveFontColour : TColour;
  SaveFontHeight : Integer;
  SaveFontName : String;
  SaveFontSize : Integer;
  SaveFontStyle : TFontStyles;
  SavePenColour : TColour;
  SavePenMode : TPenMode;
  SavePenStyle : TPenStyle;
  SavePenWidth : Integer;

CONST
  MaxColourNum = 13;

TYPE
  ColoursArrayType = ARRAY [1..MaxColourNum] OF TColour;

CONST
  ColoursArray : ColoursArrayType = (clYellow, clLime, clRed, clFWPPink, clFWPOrange, clFuchsia, clAqua, clGreen, clCream, clFWPLtBrown, clTeal, clSkyBlue, clWhite);

VAR
  { Effectively these are constants, set up by initialisation }
//  FunctionDecoderBytes : ARRAY [FirstFunctionDecoder..LastFunctionDecoder] OF Byte;
  FunctionDecoderBytes : ARRAY OF Byte;
  FunctionDecoderArrayOffset : Integer;

  HoursUpX, HoursStartY, HoursDownX, HoursEndY : Word;
  MinutesUpX, MinutesStartY, MinutesDownX, MinutesEndY : Word;
  SecondsUpX, SecondsStartY, SecondsDownX, SecondsEndY : Word;

PROCEDURE AddLineToStationMonitorsWebDiagnosticsMemo(S : String);
{ Adds a line of text to the station monitor unit's web diagnostics memo }

PROCEDURE CalculatePlatformPositions;
{ Create the platform rectangle }

FUNCTION DescribeActualDateAndTime : String;
{ Return the current real date and time as a String }

PROCEDURE InitialiseInitVarsUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

PROCEDURE InitialiseLogFiles;
{ Open a new file for test output - rename two previous ones if they exist }

PROCEDURE ReadInAreasDataFromDatabase;
{ Initialise the area data }

PROCEDURE ReadInPlatformDataFromDatabase;
{ Initialise the platform data }

FUNCTION ValidateGridX(Str : String; OUT ErrorMsg : String) : Integer;
{ Converts grid string to number }

FUNCTION ValidateRow(RowStr : String; OUT ErrorMsg : String) : Extended;
{ See whether the row provided is valid }

IMPLEMENTATION

{$R *.dfm}

USES LocoUtils, MiscUtils, Lenz, RailDraw, IniFiles, Startup, DateUtils, GetTime, Diagrams, StrUtils, Grids, Movement, LocationsUnit, Feedback, Options, PointsUnit,
     LinesUnit, Train, Main, Logging;

CONST
  UnitRef = 'InitVars';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION DescribeActualDateAndTime : String;
{ Return the current real date and time as a String }
BEGIN
  { The system format for the date }
  FormatSettings.ShortDateFormat := 'dd/mm/yyyy';

  Result := DateToStr(Now) + ' ' + TimeToHMSStr(Now);
END; { DescribeActualDateAndTime }

PROCEDURE AddLineToStationMonitorsWebDiagnosticsMemo(S : String);
{ Adds a line of text to the station monitor unit's web diagnostics memo }
BEGIN
  TRY
    IF (InitVarsWindow.StationMonitorsWebDiagnosticsMemo <> NIL) AND NOT ProgramShuttingDown THEN
      InitVarsWindow.StationMonitorsWebDiagnosticsMemo.Lines.Add(S);
  EXCEPT
    ON E : Exception DO
      Log('EG AddLineToStationMonitorsWebDiagnosticsMemo:' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { AddLineToStationMonitorsWebDiagnosticsMemo }

PROCEDURE ReadInAreasDataFromDatabase;
{ Initialise the area data }
CONST
  StopTimer = True;

VAR
  A_Num : Integer;
  A_StationMonitorsDisplayOrderNumStr : String;
  A_TypeStr : String;
//  DotFound : Boolean;
  ErrorMsg : String;
  FieldName : String;
  I : Integer;
  TempArea : Integer;
  TempStr : String;

BEGIN
  TRY
    Log('A INITIALISING AREAS {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + AreaDataFilename + '.' + AreaDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Areas database file "' + PathToRailDataFiles + AreaDataFilename + '.' + AreaDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInAreasDataFromDatabase')
        ELSE BEGIN
//          { ask where the files are }
//          IF PathToRailDataFiles = '' THEN
//            IF NOT InputQuery('Area Database', 'Enter the Path to the Rail Data Files directory or press Cancel to exit the program', PathToRailDataFiles) THEN
//              ShutDownProgram(UnitRef, 'ReadInAreasDataFromDatabase');
//          IF AreaDataFileName = '' THEN BEGIN
//            IF NOT InputQuery('Area Database', 'Enter the database filename or press Cancel to exit the program', TempStr) THEN
//              ShutDownProgram(UnitRef, 'ReadInAreasDataFromDatabase')
//            ELSE BEGIN
//              DotFound := False;
//              I := Length(TempStr);
//              WHILE (I >= 0) AND NOT DotFound DO BEGIN
//                IF TempStr[I] = '.' THEN BEGIN
//                  DotFound := True;
//                  AreaDataFileName := Copy(TempStr, 1, I - 1);
//                  AreaDataFileNameSuffix := Copy(TempStr, I + 1);
//                END;
//                Dec(I);
//              END; {WHILE}
//            END;
//          END;
        END;
      END;

      AreasADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + AreaDataFilename + '.' + AreaDataFilenameSuffix
                                             + ';Persist Security Info=False';
      AreasADOConnection.Connected := True;
      AreasADOTable.Open;
      Log('T Area data table and connection opened to initialise the area data');

      { First see if the area numbers in the MSAccess file are sequential and, if not, renumber it - we need this or deletions from the MSAccess file will cause problems }
      A_Num := -1;
      AreasADOTable.First;
      FieldName := 'AreaNum';
      WHILE NOT AreasADOTable.EOF DO BEGIN
        Inc(A_Num);
        IF AreasADOTable.FieldByName(FieldName).AsInteger <> A_Num THEN BEGIN
          { we need to renumber from here on }
          AreasADOTable.Edit;
          AreasADOTable.FieldByName(FieldName).AsInteger := A_Num;
          AreasADOTable.Post;
        END;
        AreasADOTable.Next;
      END; {WHILE}

      AreasADOTable.First;
      SetLength(Areas, 0);
      SetLength(ReversingAreas, 0);

      WITH AreasADOTable DO BEGIN
        AreasADOTable.Sort := '[AreaNum] ASC';
        WHILE NOT AreasADOTable.EOF DO BEGIN
          SetLength(Areas, Length(Areas) + 1);
          WITH Areas[High(Areas)] DO BEGIN
            { Some defaults first }
            Area_IsHoldingArea := False;
            Area_IsReversingArea := False;
            Area_SortByLocationLength := False;
            Area_StationMonitorsDisplayOrderNum := 0;
            Area_Type := UnknownAreaType;

            FieldName := 'AreaNum';
            A_Num := AreasADOTable.FieldByName(FieldName).AsInteger;
            IF A_Num <> High(Areas) THEN
              ErrorMsg := 'it does not match the area number in the database (' + IntToStr(A_Num) + ')';

            FieldName := 'LongName';
            Area_LongStr := AreasADOTable.FieldByName(FieldName).AsString;
            IF Area_LongStr = '' THEN
              ErrorMsg := 'missing long name';

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'ShortName';
              Area_ShortStr := AreasADOTable.FieldByName(FieldName).AsString;
              IF Area_ShortStr = '' THEN
                ErrorMsg := 'missing short name';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'StationMonitorsDisplayOrderNumber';
              A_StationMonitorsDisplayOrderNumStr := AreasADOTable.FieldByName(FieldName).AsString;
              IF A_StationMonitorsDisplayOrderNumStr = '' THEN
                Area_StationMonitorsDisplayOrderNum := 0
              ELSE
                IF NOT TryStrToInt(A_StationMonitorsDisplayOrderNumStr, Area_StationMonitorsDisplayOrderNum) THEN
                  ErrorMsg := 'Invalid integer "' + A_StationMonitorsDisplayOrderNumStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'SortTimetableByTrainLength';
              Area_SortByLocationLength := AreasADOTable.FieldByName(FieldName).AsBoolean;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'HoldingAreaOnly';
              Area_IsHoldingArea := AreasADOTable.FieldByName(FieldName).AsBoolean;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'ReversingArea';
              Area_IsReversingArea := AreasADOTable.FieldByName(FieldName).AsBoolean;

              IF Area_IsReversingArea THEN BEGIN
                { Also create the reversing areas database }
                SetLength(ReversingAreas, Length(ReversingAreas) + 1);
                WITH ReversingAreas[High(ReversingAreas)] DO BEGIN
                  ReversingAreas_Area := A_Num;
                  ReversingAreas_AccessibleToFirstStationArea := False;
                  ReversingAreas_ToFirstStationAreaDirection := UnknownDirection;
                  ReversingAreas_NearestToFirstStationArea := False;
                  ReversingAreas_TimeToFirstStationAreaInMinutes := 0;
                END; {WITH}
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Type';
              A_TypeStr := AreasADOTable.FieldByName(FieldName).AsString;
              IF A_TypeStr = 'Station' THEN
                Area_Type := Station
              ELSE
                IF A_TypeStr = 'Sidings' THEN
                  Area_Type := Siding
                ELSE
                  IF A_TypeStr = 'Fiddleyard' THEN
                    Area_Type := Fiddleyard
                  ELSE
                    IF A_TypeStr = 'Holding Signal' THEN
                      Area_Type := HoldingSignal
                    ELSE
                      IF A_TypeStr = 'Other' THEN
                        Area_Type := OtherArea
                      ELSE
                        Area_Type := UnknownAreaType;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'AccessibleAreasUp';
              SetLength(Area_AccessibleAreasUp, 0);
              TempStr := AreasADOTable.FieldByName(FieldName).AsString;

              { Extract the data into a String array for now - we expect a comma as a delimiter }
              ExtractSubStringsFromString(TempStr, ',', Area_AccessibleAreasUpStrArray);
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'AccessibleAreasDown';
              SetLength(Area_AccessibleAreasDown, 0);
              TempStr := AreasADOTable.FieldByName(FieldName).AsString;

              { Extract the data into a String array for now - we expect a comma as a delimiter }
              ExtractSubStringsFromString(TempStr, ',', Area_AccessibleAreasDownStrArray);
            END;

            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating Area=' + IntToStr(A_Num) + ' (' + Areas[High(Areas)].Area_LongStr + '): '
                                            + ErrorMsg
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInAreasDataFromDatabase');
            END;
          END; {WITH}
          AreasADOTable.Next;
        END; {WHILE}
      END; {WITH}

      { Tidy up the database }
      AreasADOTable.Close;
      AreasADOConnection.Connected := False;
      Log('T Area Data table and connection closed');

      { Now process the data we just read in - we couldn't do this until all the area data was read in }
      A_Num := 0;
      WHILE (A_Num <= High(Areas)) AND (ErrorMsg = '') DO BEGIN
        WITH Areas[A_Num] DO BEGIN
          SetLength(Area_AccessibleAreasUp, 0);
          I := 0;
          WHILE (I <= High(Area_AccessibleAreasUpStrArray)) AND (ErrorMsg = '') DO BEGIN
            TempArea := StrToArea(Area_AccessibleAreasUpStrArray[I]);
            IF TempArea = UnknownArea THEN
              ErrorMsg := 'unknown area "' + Area_AccessibleAreasUpStrArray[I] + ''' in Area_AccessibleAreasUp field for Area=' + AreaToStr(A_Num)
            ELSE
              AppendToAreaArray(Area_AccessibleAreasUp, TempArea);

            Inc(I);
          END; {WHILE}

          SetLength(Area_AccessibleAreasDown, 0);
          I := 0;
          WHILE (I <= High(Area_AccessibleAreasDownStrArray)) AND (ErrorMsg = '') DO BEGIN
            TempArea := StrToArea(Area_AccessibleAreasDownStrArray[I]);
            IF TempArea = UnknownArea THEN
              ErrorMsg := 'unknown area "' + Area_AccessibleAreasDownStrArray[I] + ''' in Area_AccessibleAreasDown field for Area=' + AreaToStr(A_Num)
            ELSE
              AppendToAreaArray(Area_AccessibleAreasDown, TempArea);

            Inc(I);
          END; {WHILE}

          IF NOT Area_IsHoldingArea AND (Length(Area_AccessibleAreasUp) = 0) AND (Length(Area_AccessibleAreasDown) = 0) THEN
            ErrorMsg := 'it has neither Possible Up Destinations or Possible Down Destinations';
        END; {WITH}

        IF ErrorMsg = '' THEN
          Inc(A_Num);
      END; {WHILE}

      IF ErrorMsg <> '' THEN BEGIN
        IF MessageDialogueWithDefault('Error in creating Area=' + IntToStr(A_Num) + ' (' + Areas[A_Num].Area_LongStr + '): '
                                      + ErrorMsg
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInAreasDataFromDatabase');
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInAreasDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadInAreasDataFromDatabase }

FUNCTION ValidateRow(RowStr : String; OUT ErrorMsg : String) : Extended;
{ See whether the row provided is valid }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF RowStr = '' THEN
    Result := 0
  ELSE
    IF NOT TryStrToFloat(RowStr, Result) THEN
      ErrorMsg := 'ValidateRow: invalid number';

  IF Result < 0 THEN
    ErrorMsg := 'ValidateRow: row number cannot be less than zero'
  ELSE
    IF Result > WindowRows THEN
      ErrorMsg := 'ValidateRow: row number cannot exceed the specified number of screeen rows (' + IntToStr(WindowRows) + ')';
END; { ValidateRow }

FUNCTION ValidateGridX(Str : String; OUT ErrorMsg : String) : Integer;
{ Converts grid string to number }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF Str <> '' THEN
    IF NOT TryStrToInt(Str, Result) THEN
      ErrorMsg := 'ValidateGridX: invalid integer "' + Str + '"';
END; { ValidateGridX }

PROCEDURE CalculatePlatformPositions;
{ Create the platform rectangle }
VAR
  P : Integer;

BEGIN
  FOR P := 0 TO High(Platforms) DO BEGIN
    WITH Platforms[P] DO BEGIN
      WITH Platform_Rect DO BEGIN
        Left := MapGridXToScreenX(Lines[Platform_LeftLine].Line_GridUpX);
        IF Platform_LeftLineAdjustment > 0 THEN
          Left := Left + MulDiv(FWPRailWindow.ClientWidth, Platform_LeftLineAdjustment, ZoomScaleFactor);

        Top := -1;
        IF Platforms[P].Platform_RowAbovePlatform <> 0 THEN
          Top := Round(Platforms[P].Platform_RowAbovePlatform * InterLineSpacing);

        Right := MapGridXToScreenX(Lines[Platform_RightLine].Line_GridDownX);
        IF Platform_RightLineAdjustment > 0 THEN
          Right := Right + MulDiv(FWPRailWindow.ClientWidth, Platform_RightLineAdjustment, ZoomScaleFactor);

        Bottom := -1;
        IF Platforms[P].Platform_RowBelowPlatform <> 0 THEN
          Bottom := Round(Platforms[P].Platform_RowBelowPlatform * InterLineSpacing);

        IF Top = -1 THEN
          Top := Platforms[P].Platform_Rect.Bottom - MulDiv(FWPRailWindow.ClientHeight, Platforms[P].Platform_Height, ZoomScaleFactor)
        ELSE
          IF Bottom = -1 THEN
            Bottom := Platforms[P].Platform_Rect.Top + MulDiv(FWPRailWindow.ClientHeight, Platforms[P].Platform_Height, ZoomScaleFactor);

        IF Top = Bottom THEN
          Debug('Platform ' + IntToStr(P) + ': platform top value (' + IntToStr(Top) + ') = platform bottom value (' + IntToStr(Bottom) + ')')
        ELSE
          IF Top > Bottom THEN
            Debug('Platform ' + IntToStr(P) + ': platform top value (' + IntToStr(Top) + ') > platform bottom value (' + IntToStr(Bottom) + ')');

        Top := Top + PlatformEdgeVerticalSpacingScaled;
        Bottom := Bottom - PlatformEdgeVerticalSpacingScaled;
      END; {WITH}
    END; {WITH}
  END; {FOR}
END; { CalculatePlatformPositions }

PROCEDURE ReadInPlatformDataFromDatabase;
{ Initialise the platform data }
CONST
  StopTimer = True;

VAR
  ErrorMsg : String;
  P : Integer;
  TempStr : String;

BEGIN
  TRY
    Log('A INITIALISING PLATFORMS {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + PlatformDataFilename + '.' + PlatformDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Platform database file "' + PathToRailDataFiles + PlatformDataFilename + '.' + PlatformDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInPlatformDataFromDatabase')
        ELSE
          Exit;
      END;

      PlatformsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                    + PathToRailDataFiles + PlatformDataFilename + '.' + PlatformDataFilenameSuffix
                                                    + ';Persist Security Info=False';
      PlatformsADOConnection.Connected := True;
      PlatformsADOTable.Open;
      Log('T Platform data table and connection opened to initialise the platform data');

      { First see if the platform numbers in the MSAccess file are sequential and, if not, renumber it; We need this or deletions from the MSAccess file cause problems }
      P := -1;
      PlatformsADOTable.First;
      WHILE NOT PlatformsADOTable.EOF DO BEGIN
        Inc(P);
        IF PlatformsADOTable.FieldByName(Platform_RunningNumberFieldName).AsInteger <> P THEN BEGIN
          { we need to renumber from here on }
          PlatformsADOTable.Edit;
          PlatformsADOTable.FieldByName(Platform_RunningNumberFieldName).AsInteger := P;
          PlatformsADOTable.Post;
        END;
        PlatformsADOTable.Next;
      END; {WHILE}

      PlatformsADOTable.Sort := '[' + Platform_RunningNumberFieldName + '] ASC';
      PlatformsADOTable.First;
      SetLength(Platforms, 0);

      WHILE NOT PlatformsADOTable.EOF DO BEGIN
        WITH PlatformsADOTable DO BEGIN
          SetLength(Platforms, Length(Platforms) + 1);
          P := High(Platforms);
          WITH Platforms[P] DO BEGIN
            ErrorMsg := '';

            Platform_RunningNumber := FieldByName(Platform_RunningNumberFieldName).AsInteger;
            IF Platform_RunningNumber <> P THEN
              ErrorMsg := 'it does not match the platform number in the database (' + IntToStr(Platform_RunningNumber) + ')';

            IF ErrorMsg = '' THEN BEGIN
              Platform_Description := FieldByName(Platform_DescriptionFieldName).AsString;

              Platform_NumberAStr := FieldByName(Platform_NumberAStrFieldName).AsString;

              TempStr := FieldByName(Platform_NumberAPositionFieldName).AsString;
              IF TempStr = '' THEN
                ErrorMsg := 'no platform A position supplied'
              ELSE BEGIN
                Platform_NumberPositionA := StrToPlatformNumberPosition(TempStr);
                IF Platform_NumberPositionA = UnknownPlatformNumberPosition THEN
                  ErrorMsg := 'platform number A position supplied is unknown "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              Platform_NumberBStr := FieldByName(Platform_NumberBStrFieldName).AsString;

              TempStr := FieldByName(Platform_NumberBPositionFieldName).AsString;
              IF TempStr = '' THEN BEGIN
                IF Platform_NumberBStr <> '' THEN
                  ErrorMsg := 'no platform B position supplied';
              END ELSE BEGIN
                IF Platform_NumberBStr = '' THEN
                  ErrorMsg := 'platform B position supplied (' + TempStr + ') but platform B does not have a value'
                ELSE BEGIN
                  Platform_NumberPositionB := StrToPlatformNumberPosition(TempStr);
                  IF Platform_NumberPositionB = UnknownPlatformNumberPosition THEN
                    ErrorMsg := 'platform number B position supplied is unknown "' + TempStr + '"';
                END;
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Platform_LeftLineFieldName).AsString;
              IF TempStr = '' THEN
                ErrorMsg := 'missing left line value'
              ELSE BEGIN
                Platform_LeftLine := StrToLine(TempStr);
                IF Platform_LeftLine = UnknownLine THEN
                  ErrorMsg := 'invalid left line "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Platform_LeftLineAdjustmentFieldName).AsString;
              IF TempStr = '' THEN
                Platform_LeftLineAdjustment := 0
              ELSE BEGIN
                IF Platform_LeftLine = UnknownLine THEN
                  ErrorMsg := 'cannot have a left line adjustment (' + TempStr + ') with a left line value'
                ELSE
                  IF NOT TryStrToInt(TempStr, Platform_LeftLineAdjustment) THEN
                    ErrorMsg := 'invalid left line "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Platform_RightLineFieldName).AsString;
              IF TempStr = '' THEN
                ErrorMsg := 'missing right line value'
              ELSE BEGIN
                Platform_RightLine := StrToLine(TempStr);
                IF Platform_RightLine = UnknownLine THEN
                  ErrorMsg := 'invalid right line "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Platform_RightLineAdjustmentFieldName).AsString;
              IF TempStr = '' THEN
                Platform_RightLineAdjustment := 0
              ELSE BEGIN
                IF Platform_RightLine = UnknownLine THEN
                  ErrorMsg := 'cannot have a right line adjustment (' + TempStr + ') with a right line value'
                ELSE
                  IF NOT TryStrToInt(TempStr, Platform_RightLineAdjustment) THEN
                    ErrorMsg := 'invalid right line "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN
              Platform_RowAbovePlatform := ValidateRow(FieldByName(Platform_RowAbovePlatformFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Platform_RowBelowPlatform := ValidateRow(FieldByName(Platform_RowBelowPlatformFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              IF (Platform_RowAbovePlatform = 0) AND (Platform_RowBelowPlatform = 0) THEN
                ErrorMsg := 'must have either a location above or a location below';

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Platform_HeightFieldName).AsString;
              IF TempStr = '' THEN
                Platform_Height := 0
              ELSE
                IF NOT TryStrToInt(TempStr, Platform_Height) THEN
                  ErrorMsg := 'invalid height "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN
              IF Platform_Height > 0 THEN
                IF (Platform_RowAbovePlatform <> 0) AND (Platform_RowBelowPlatform <> 0) THEN
                  ErrorMsg := 'cannot have both a location above and a location below if a height is specified';

            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating Platform=' + IntToStr(High(Platforms))
                                            + ' (' + Platform_Description + '): ' + '['
                                            + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?', StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInPlatformDataFromDatabase');
            END;
          END; {WITH}
        END; {WITH}
        PlatformsADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      PlatformsADOTable.Close;
      PlatformsADOConnection.Connected := False;
      Log('T Platform data table and connection closed');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG InitialisePlatformData: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}

  CalculatePlatformPositions;

//
// { Now the TRS plunger mouse rectangle }
// WITH TRSPlunger_MouseRect DO BEGIN
// WITH TRSPlunger_Triangle DO BEGIN
// IF TRSPlunger_Direction = Up THEN BEGIN
// Left := PlungerX - TRSPlungerLengthScaled;
// Top := PlungerY;
// Right := PlungerX;
// Bottom := PlungerY + TRSPlungerLengthScaled;
// END ELSE BEGIN
// Left := PlungerX;
// Top := PlungerY;
// Right := PlungerX + TRSPlungerLengthScaled;
// Bottom := PlungerY + TRSPlungerLengthScaled;
// END;
// END; {WITH}
// END; {WITH}
//
// TRSPlunger_Pressed := False;
// TRSPlunger_Locked := True;
// TRSPlunger_AwaitingPressingMsgWritten := False;
// TRSPlunger_HasBeenPressedMsgWritten := False;
// END; {WITH}
// END; {FOR}

    { Now do the branch platforms }
  // WITH BranchPlatformOrSidingOrFiddleyardPlungers[MainPlatformLocation] DO BEGIN
  // CASE MainPlatformLocation OF
  // BranchPlatformOrSidingOrFiddleyard1:
  // TRSPlunger_PlatformNumStr := 'BT1';
  // BranchPlatformOrSidingOrFiddleyard2:
  // TRSPlunger_PlatformNumStr := 'BT2';
  // BranchPlatformOrSidingOrFiddleyard3:
  // TRSPlunger_PlatformNumStr := 'BT3';
  // END; {CASE}
  // END; {WITH}
  // END; {FOR}
// END;
END; { ReadInPlatformDataFromDatabase }

PROCEDURE InitialiseLogFiles;
{ Open a new file for test output - rename two previous ones if they exist }
CONST
  AppendToFile = True;
  OverwriteFile = True;

VAR
  ErrorMsg : String;

BEGIN
  { Check that the path to the log files exists - if not, create it }
  IF PathToLogFiles = '' THEN BEGIN
    Log('X! Error - PathToLogFiles is empty - log files directory cannot be opened or created');
    SetMode(LogsCurrentlyKeptModeType, False);
  END ELSE BEGIN
    IF NOT DirectoryExists(PathToLogFiles) THEN
      ForceDirectories(PathToLogFiles);

    { First the standard debug (and replay) files }
    RenameEarlierFiles(LargeLogFile, PathToLogFiles + LogFileName, LogFileNameSuffix);
    IF NOT OpenOutputFileOK(LargeLogFile, PathToLogFiles + LogFileName + '.' + LogFileNameSuffix, ErrorMsg, NOT AppendToFile) THEN
      ShowMessage(ErrorMsg)
    ELSE BEGIN
      WriteLn(LargeLogFile, GetProgramTitle);
      WriteLn(LargeLogFile, GetProgramVersion('Debugging (and Replay) Log File'));
      WriteLn(LargeLogFile, '{Replay NoWrite}');
      LogFileOpen := True;
    END;

    { Now the test file }
    RenameEarlierFiles(TestLogFile, PathToLogFiles + LogFileName + '-Test', LogFileNameSuffix);
    IF NOT OpenOutputFileOK(TestLogFile, PathToLogFiles + LogFileName + '-Test.' + LogFileNameSuffix, ErrorMsg, NOT AppendToFile) THEN
      ShowMessage(ErrorMsg)
    ELSE BEGIN
      WriteLn(TestLogFile, GetProgramTitle);
      WriteLn(TestLogFile, GetProgramVersion('Test Log File'));
      IF InTestingMode THEN
        Flush(TestLogFile);
    END;

    { Now the individual log files }
    IF MultipleLogFilesRequired THEN BEGIN
      RenameEarlierFiles(ErrorLogFile, PathToLogFiles + LogFileName + '-Error', LogFileNameSuffix);
      Rewrite(ErrorLogFile);
      WriteLn(ErrorLogFile, GetProgramTitle);
      WriteLn(ErrorLogFile, GetProgramVersion('Error Log File'));

      RenameEarlierFiles(LocoLogFile, PathToLogFiles + LogFileName + '-Loco', LogFileNameSuffix);
      Rewrite(LocoLogFile);
      WriteLn(LocoLogFile, GetProgramTitle);
      WriteLn(LocoLogFile, GetProgramVersion('Loco Log File'));

      RenameEarlierFiles(RouteLogFile, PathToLogFiles + LogFileName + '-Route', LogFileNameSuffix);
      Rewrite(RouteLogFile);
      WriteLn(RouteLogFile, GetProgramTitle);
      WriteLn(RouteLogFile, GetProgramVersion('Route Log File'));

      RenameEarlierFiles(SignalPointAndTCLogFile, PathToLogFiles + LogFileName + '-SignalPointAndTC', LogFileNameSuffix);
      Rewrite(SignalPointAndTCLogFile);
      WriteLn(SignalPointAndTCLogFile, GetProgramTitle);
      WriteLn(SignalPointAndTCLogFile, GetProgramVersion('Signal Point And TC Log File'));

      RenameEarlierFiles(DiagramsLogFile, PathToLogFiles + LogFileName + '-Diagrams', LogFileNameSuffix);
      Rewrite(DiagramsLogFile);
      WriteLn(DiagramsLogFile, GetProgramTitle);
      WriteLn(DiagramsLogFile, GetProgramVersion('Diagrams Log File'));

      RenameEarlierFiles(WorkingTimetableLogFile, PathToLogFiles + LogFileName + '-WorkingTimetable', LogFileNameSuffix);
      Rewrite(WorkingTimetableLogFile);
      WriteLn(WorkingTimetableLogFile, GetProgramTitle);
      WriteLn(WorkingTimetableLogFile, GetProgramVersion('WorkingTimetable Log File'));
    END;
  END;
END; { InitialiseLogFiles }

PROCEDURE InitialiseInitVarsUnit;
{ Such routines as this allow us to initialises the units in the order we wish }
BEGIN
  DefaultFWPRailWindowTop := 0;
  DefaultFWPRailWindowLeft := 0;
  DefaultFWPRailWindowHeight := MulDiv(Screen.WorkAreaHeight, DefaultFWPRailWindowHeight, 100);
  DefaultFWPRailWindowWidth := Screen.DeskTopWidth;

  DefaultDebugWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultDebugWindowTop := MulDiv(Screen.WorkAreaHeight, 800, 1000);
  DefaultDebugWindowLeft := MulDiv(Screen.WorkAreaWidth, 500, 1000);
  DefaultDebugWindowWidth := MulDiv(Screen.WorkAreaWidth, 500, 1000);

  DefaultDiagramsWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultDiagramsWindowTop := MulDiv(Screen.WorkAreaHeight, 800, 1000);
  DefaultDiagramsWindowLeft := 0;
  DefaultDiagramsSmallWindowWidth := MulDiv(Screen.WorkAreaWidth, 500, 1000);
  DefaultDiagramsLargeWindowWidth := Screen.WorkAreaWidth;

  DefaultDisplayLineColoursWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultDisplayLineColoursWindowTop := MulDiv(Screen.WorkAreaHeight, 500, 1000);
  DefaultDisplayLineColoursWindowLeft := 0;
  DefaultDisplayLineColoursWindowWidth := MulDiv(Screen.WorkAreaHeight, 200, 1000);

  DefaultEditWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultEditWindowTop := MulDiv(Screen.WorkAreaHeight, 800, 1000);
  DefaultEditWindowLeft := 0;
  DefaultEditWindowWidth := MulDiv(Screen.WorkAreaWidth, 500, 1000);

  DefaultLockListWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultLockListWindowTop := MulDiv(Screen.WorkAreaHeight, 500, 1000);
  DefaultLockListWindowLeft := 0;
  DefaultLockListWindowWidth := MulDiv(Screen.WorkAreaWidth, 200, 1000);

  DefaultLocoUtilsWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultLocoUtilsWindowTop := MulDiv(Screen.WorkAreaHeight, 500, 1000);
  DefaultLocoUtilsWindowLeft := 0;
  DefaultLocoUtilsWindowWidth := MulDiv(Screen.WorkAreaWidth, 200, 1000);

  DefaultMovementWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultMovementWindowTop := MulDiv(Screen.WorkAreaHeight, 500, 1000);
  DefaultMovementWindowLeft := 0;
  DefaultMovementWindowWidth := MulDiv(Screen.WorkAreaWidth, 200, 1000);

  DefaultOptionsWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultOptionsWindowTop := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultOptionsWindowLeft := MulDiv(Screen.WorkAreaWidth, 200, 1000);
  DefaultOptionsWindowWidth := MulDiv(Screen.WorkAreaWidth, 200, 1000);

  DefaultWorkingTimetableWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultWorkingTimetableWindowTop := MulDiv(Screen.WorkAreaHeight, 800, 1000);
  DefaultWorkingTimetableWindowLeft := 0;
  DefaultWorkingTimetableSmallWindowWidth := MulDiv(Screen.WorkAreaWidth, 500, 1000);
  DefaultWorkingTimetableLargeWindowWidth := Screen.WorkAreaWidth;

  Log('A InitVars unit initialised');
END; { InitialiseInitVarsUnit }

INITIALIZATION

BEGIN
  SaveActualTime := Time;
END; { Initialization }

END { InitVars }.
