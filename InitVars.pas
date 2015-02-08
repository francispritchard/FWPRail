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
  ModeType = (AllRouteDebugging, AnonymousOccupation, FeedbackDebugging, GeneralDebugging, LineDebugging, LockDebugging, Locking, LocoSpeedTiming, LogsCurrentlyKept,
              PointDebugging, PreviousPointSettings, RDC, RecordingMonitorScreens, RecordLineDrawing, RouteDebugging, RouteBacktrackDebugging, RouteDrawing,
              ShowAdjacentTrackCircuit, StationStart, Testing);

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
  BufferStopRec = RECORD
    BufferStop_AdjacentLine : Integer;
    BufferStop_AdjacentTrackCircuit : Integer;
    BufferStop_AsTheatreDestination : String;
    BufferStop_CurrentColour : TColour;
    BufferStop_Direction : DirectionType;
    BufferStop_MouseRect : TRect; { mouse access rectangle }
    BufferStop_Number : Integer;
    BufferStop_X : Integer;
    BufferStop_Y1 : Integer;
    BufferStop_Y2 : Integer;
  END;

  { Feedback-related type declarations }
  TypeOfFeedback = (TrackCircuitFeedback, TRSPlungerFeedback, PointFeedback, LineFeedback, UnknownFeedbackType);

  TypeOfFeedbackDetector = (TrackCircuitFeedbackDetector, TRSPlungerFeedbackDetector, PointFeedbackDetector, LineFeedbackDetector, MixedFeedbackDetectors,
                            FeedbackDetectorOutOfUse, UnknownFeedbackDetectorType);

  FeedbackRec = RECORD
    Feedback_DetectorOutOfUse : Boolean;
    Feedback_InputTypeArray : ARRAY [1..8] OF TypeOfFeedback;
    Feedback_InputOnArray : ARRAY [1..8] OF Boolean;
    Feedback_InputLine : ARRAY [1..8] OF Integer;
    Feedback_InputPoint : ARRAY [1..8] OF Integer;
    Feedback_InputTrackCircuit : ARRAY [1..8] OF Integer;
    Feedback_InputTRSPlunger : ARRAY [1..8] OF Integer;
    Feedback_TCAboveUnit : Integer;
  END;

  { Line-related type declarations }
  EndOfLineType = (BufferStopAtUp, BufferStopAtDown, ProjectedLineAtUp, ProjectedLineAtDown, NotEndOfLine, UnknownEndOfLine);

  TypeOfLine = (MainOrGoods, MainLine, GoodsLine, BranchLineDouble, BranchLineSingle, IslandStationLine, MainStationLine, BranchStationLine, WindowStationLine, SidingLine,
                FiddleyardLine, SidingsApproach, StationAvoiding, ProjectedLine, NewlyCreatedLine, UnknownTypeOfLine);
  { adjust FirstTypeOfLine or LastTypeOfLine if alteration made to above declaration }

  GradientType = (Level, RisingIfUp, RisingIfDown, UnknownGradientType);
  HandleType = (UpHandle, MidHandle, DownHandle, NoHandle);
  MPHType = (MPH0, MPH10, MPH20, MPH30, MPH40, MPH50, MPH60, MPH70, MPH80, MPH90, MPH100, MPH110, MPH120, UnknownMPH, NoSpecifiedSpeed);
  NextLineRouteingType = (EndOfLineIsNext, LineIsNext, PointIsNext, UnknownNextLineRouteingType);
  OutOfUseState = (InUse, OutOfUse);
  PointStateType = (Diverging, Straight, PointOutOfAction, PointStateUnknown);

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

  LineRec = RECORD
    Line_AdjacentBufferStop : Integer;
    Line_BufferStopTheatreDestinationStr : String;
    Line_CurrentColour : TColour;
    Line_DataChanged : Boolean;
    Line_Direction : DirectionType;
    Line_DownConnectionCh : String;
    Line_DownConnectionChRect : TRect;
    Line_DownConnectionChBold : Boolean;
    Line_DownRow : Extended;
    Line_EndOfLineMarker : EndOfLineType;
    Line_Gradient : GradientType;
    Line_GridDownX : Integer;
    Line_GridDownY : Integer;
    Line_GridUpX : Integer;
    Line_GridUpY : Integer;
    Line_InitialOutOfUseState : OutOfUseState;
    Line_InUseFeedbackUnit : Integer;
    Line_InUseFeedbackInput : Integer;
    Line_Location : Integer;
    Line_LockFailureNotedInSubRouteUnit : Boolean;
    Line_MousePolygon : ARRAY [0..4] OF TPoint; { mouse access for indicators }
    Line_NameStr : String;
    Line_NextDownIsEndOfLine : EndOfLineType;
    Line_NextDownLine : Integer;
    Line_NextDownPoint : Integer;
    Line_NextDownType : NextLineRouteingType;
    Line_NextUpIsEndofLine : EndOfLineType;
    Line_NextUpLine : Integer;
    Line_NextUpPoint : Integer;
    Line_NextUpType : NextLineRouteingType;
    Line_NoLongerOutOfUse : Boolean;
    Line_Number : Integer;
    Line_OldColour : TColour;
    Line_OutOfUseState : OutOfUseState;
    Line_RoutedOver : Boolean;
    Line_RouteLockingForDrawing : Integer; { used for drawing those bits of Line that are routed over }
    Line_RouteSet : Integer;
    Line_SaveOutOfUseState : OutOfUseState;
    Line_TC : Integer;
    Line_TypeOfLine : TypeOfLine;
    Line_UpConnectionCh : String;
    Line_UpConnectionChRect : TRect;
    Line_UpConnectionChBold : Boolean;
    Line_UpRow : Extended;

    { For line editing }
    Line_DownHandlePolygon : ARRAY [0..4] OF TPoint;
    Line_IsBeingMovedByHandle : HandleType;
    Line_IsTempNewLine : Boolean;
    Line_MidHandlePolygon : ARRAY [0..4] OF TPoint;
    Line_ShowHandles : Boolean;
    Line_UpHandlePolygon : ARRAY [0..4] OF TPoint;
  END;

CONST
  Line_BufferStopTheatreDestinationStrFieldName : String = 'Buffer Stop Theatre Destination';
  Line_DirectionFieldName : String = 'Direction';
  Line_DownConnectionChFieldName : String = 'Down Connection Ch';
  Line_DownRowFieldName : String = 'Down Row';
  Line_GridDownXFieldName : String = 'Grid Down X';
  Line_GridDownYFieldName : String = 'Grid Down Y';
  Line_EndOfLineMarkerFieldName : String = 'End Of Line Marker';
  Line_GradientFieldName : String = 'Gradient';
  Line_InUseFeedbackUnitFieldName : String = 'In Use Feedback Unit';
  Line_InUseFeedbackInputFieldName : String = 'In Use Feedback Input';
  Line_LocationStrFieldName : String = 'Location';
  Line_NameStrFieldName : String = 'Line Name';
  Line_NumberFieldName : String = 'Line Number';
  Line_OutOfUseFieldName : String = 'Out Of Use';
  Line_TCFieldName : String = 'Line TC';
  Line_TypeOfLineFieldName : String = 'Type Of Line';
  Line_UpConnectionChFieldName : String = 'Up Connection Ch';
  Line_UpRowFieldName : String = 'Up Row';
  Line_GridUpXFieldName : String = 'Grid Up X';
  Line_GridUpYFieldName : String = 'Grid Up Y';

TYPE
  DirectionPriorityType = (PreferablyUp, UpOnly, TerminatingAtUp, PreferablyDown, DownOnly, TerminatingAtDown, NoDirectionPriority);
  ThroughLocationStateType = (ThroughLocation, NonThroughLocation, UnknownThroughLocationState);
  ThroughOrStoppingPriorityType = (ThroughPriority, StoppingPriority, NoStoppingPriority);
  TrainPriorityType = (ExpressOnly, ExpressPreferred, PassengerOnly, PassengerPreferred, NoTrainPriority);

  { Location-related type declarations }
  LocationRec = RECORD
    Location_AccessibleLocationsUp : IntegerArrayType;
    Location_AccessibleLocationsOrAreasUpStrArray : StringArrayType; { to store the data for later test }
    Location_AccessibleLocationsDown : IntegerArrayType;
    Location_AccessibleLocationsOrAreasDownStrArray : StringArrayType; { to store the data for later test }
    Location_AdjoiningPlatform : Integer;
    Location_AdjoiningPlatformStr : String;
    { We need this as well as the integer variable above as we can't test the validity of the adjacent location until we have read all the location data in. (The same
      applies to the parallel access data below).
    }
    Location_Area : Integer;
    Location_DestinationPriorityAreas : IntegerArrayType;
    Location_DirectionPriority : DirectionPriorityType;
    Location_IsPlatform : Boolean;
    Location_IsSiding : Boolean;
    Location_IsFiddleyard : Boolean;
    Location_LengthInInches : Extended; { in inches because the only tape measure FWP has is ISGP's Dean cloth tape one }
    Location_LineAtUpIsEndOfLine : Boolean;
    Location_LineAtDownIsEndOfLine : Boolean;
    Location_LineAtUp : Integer;
    Location_LineAtDown : Integer;
    Location_LocosNotAbleToUse : IntegerArrayType;
    Location_LocoClassesReservedFor : StringArrayType; { needs to be a String as could include 57XX etc. }
    Location_LongNameStr : String;
    Location_OutOfUse : Boolean;
    Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyard : Integer;
    Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr : String; { to store the data for later test }
    Location_PlatformOrFiddleyardAtUp : Integer;
    Location_PlatformOrFiddleyardAtDown : Integer;
    Location_PlatformOrFiddleyardDirection : DirectionType;
    Location_PlatformOrFiddleyardNumStr : String;
    Location_PlatformPriority : Integer;
    Location_RecordInLocationOccupationArray : Boolean;
    Location_ShortNameStr : String;
    Location_ThroughLocationState : ThroughLocationStateType;
    Location_ThroughOrStoppingPriority : ThroughOrStoppingPriorityType;
    Location_TrainPriority : TrainPriorityType;
    Location_TRSPlungerX : Integer;
    Location_TRSPlungerY : Integer;
  END;

CONST
  Location_AccessibleLocationsOrAreasDownFieldName = 'Accessible Locations Or Areas Down';
  Location_AccessibleLocationsOrAreasUpFieldName = 'Accessible Locations Or Areas Up';
  Location_AdjoiningPlatformFieldName = 'Adjoining Platform';
  Location_AreaFieldName = 'Area';
  Location_DestinationPriorityAreasFieldName = 'Destination Priority Areas';
  Location_DirectionPriorityFieldName = 'Direction Priority';
  Location_FiddleyardFieldName = 'Fiddleyard';
  Location_LengthInInchesFieldName = 'Length In Inches';
  Location_LocoClassesReservedForFieldName = 'Loco Classes Reserved For';
  Location_LocosNotAbleToUseFieldName = 'Locos Not Able To Use';
  Location_NameStrFieldName = 'Location Name';
  Location_NotesFieldName = 'Notes';
  Location_NumberFieldName = 'Location Number';
  Location_OutOfUseFieldName = 'Out Of Use';
  Location_PlatformDirectionFieldName = 'Platform Direction';
  Location_PlatformFieldName = 'Platform';
  Location_PlatformNumberStringFieldName = 'Platform Number String';
  Location_PlatformParallelAccessFieldName = 'Platform Parallel Access';
  Location_PlatformPriorityFieldName = 'Platform Priority';
  Location_RecordInLocationOccupationArrayFieldName = 'Record In Location Occupation Array';
  Location_ShortStringFieldName = 'Short String';
  Location_SidingFieldName = 'Siding';
  Location_ThroughLocationFieldName = 'Through Location';
  Location_ThroughOrStoppingPriorityFieldName = 'Through Or Stopping Priority';
  Location_TrainPriorityFieldName = 'Train Priority';
  Location_TRSPlungerXFieldName = 'TRS Plunger X';
  Location_TRSPlungerYFieldName = 'TRS Plunger Y';

TYPE
  LocationOccupationStateType = (LocationUnoccupied, LocationStartOfDayOccupation, LocationTemporaryOccupation, LocationUnknownOccupation,
                                 LocationPermanentOccupationWithFeedback, LocationPermanentOccupationSetByUser, LocationPermanentOccupationSetBySystem,
                                 LocationEndOfDayOccupation, LocationOutOfUseOccupation);
  LocationOccupationRec = RECORD
    LocationOccupation_EndTime : TDateTime;
    LocationOccupation_JourneyA : Integer;
    LocationOccupation_JourneyB : Integer;
    LocationOccupation_LocoChip : Integer;
    LocationOccupation_StartTime : TDateTime;
    LocationOccupation_State : LocationOccupationStateType;
  END;

  LocationOccupationArrayType = ARRAY OF LocationOccupationRec;

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

  { Signal-related type declarations }
  TypeOfSignal = (CallingOn, TwoAspect, ThreeAspect, FourAspect, SemaphoreHome, SemaphoreDistant, UnknownSignalType);
  AspectType = (DoubleYellowAspect, FlashingDoubleYellowAspect, GreenAspect, SingleYellowAspect, FlashingSingleYellowAspect, RedAspect, NoAspect, UnknownAspectType);

  IndicatorType = (NoIndicator, JunctionIndicator, TheatreIndicator, QueryIndicator, UnknownIndicator);
  IndicatorStateType = (NoIndicatorLit, LeftIndicatorLit, RightIndicatorLit, UpperLeftIndicatorLit, MiddleLeftIndicatorLit, LowerLeftIndicatorLit, UpperRightIndicatorLit,
                        MiddleRightIndicatorLit, LowerRightIndicatorLit, TheatreIndicatorLit, QueryIndicatorLit);
  SignalStateType = (SignalOff, SignalOn, UnknownSignalState);

  JunctionIndicatorType = (UpperLeftIndicator, MiddleLeftIndicator, LowerLeftIndicator, UpperRightIndicator, MiddleRightIndicator, LowerRightIndicator,
                           UnknownJunctionIndicator);
  JunctionIndicatorRec = RECORD
    JunctionIndicator_Exists : Boolean;
    JunctionIndicator_TargetSignal : Integer;
    JunctionIndicator_TargetBufferStop : Integer;
    JunctionIndicator_MouseRect : TRect;
  END;

  QuadrantType = (UpperQuadrant, LowerQuadrant, NoQuadrant);

  SignalRec = RECORD
    Signal_AccessoryAddress : Integer;
    Signal_Aspect : AspectType;
    Signal_AdjacentLine : Integer;
    Signal_AdjacentLineXOffset : Integer;
    Signal_AdjacentTC : Integer;
    Signal_ApproachControlAspect : AspectType;
    Signal_ApproachLocked : Boolean;
    Signal_AsTheatreDestination : String; { what a signal pointing at this signal might display }
    Signal_Automatic : Boolean; { not yet implemented }
    Signal_DataChanged : Boolean;
    Signal_DecoderNum : Integer;
    Signal_Direction : DirectionType;
    Signal_Energised : Boolean;
    Signal_EnergisedTime : TDateTime;
    Signal_FailedToResetFlag : Boolean;
    Signal_FailMsgWritten : Boolean;
    Signal_FindNextSignalBufferStopMsgWritten : Boolean;
    Signal_FromWhichUserMustDrive : Boolean;
    Signal_HiddenStationSignalAspect : AspectType; { used to force stopping at stations where signal is potentially off }
    Signal_Indicator : IndicatorType;
    Signal_IndicatorDecoderNum : Integer;
    Signal_IndicatorDecoderFunctionNum : Integer;
    Signal_IndicatorMouseRect : TRect; { mouse access rectangle for indicators }
    Signal_IndicatorSpeedRestriction : MPHType; { applicable only if the route indicator is set }
    Signal_IndicatorState : IndicatorStateType;
    Signal_JunctionIndicators : ARRAY [JunctionIndicatorType] OF JunctionIndicatorRec;
    Signal_LampIsOn : Boolean; { used for flashing aspects }
    Signal_LineX : Integer;
    Signal_LineY : Integer;
    Signal_LineWithVerticalSpacingY : Integer;
    Signal_LocationsToMonitorArray : IntegerArrayType;

    { Signal_LockedArray and Signal_RouteLockingNeededArray sound similar but serve different purposes - RouteLockingNeededArray covers the lines, track circuits, points,
      etc. ahead that must be locked before a signal can be pulled off; Signal_LockedArray shows whether a signal is locked either by a specific route or by a user.
    }
    Signal_LockedArray : StringArrayType;
    Signal_LockedBySemaphoreDistant : Boolean;

    Signal_LockFailureNotedInRouteUnit : Boolean;
    Signal_MouseRect : TRect; { mouse access rectangle for signal }
    Signal_NextSignalIfNoIndicator : Integer;
    Signal_NotUsedForRouteing : Boolean;
    Signal_Notes : String;
    Signal_Number : Integer;
    Signal_OppositePassingLoopSignal : Integer;
    Signal_OutOfUse : Boolean;
    Signal_OutOfUseMsgWritten : Boolean;
    Signal_PossibleRouteHold : Boolean;
    Signal_PossibleStationStartRouteHold : Boolean;
    Signal_PostColour : TColour;
    Signal_PostMouseRect : TRect; { mouse access rectangle for signal posts }
    Signal_PreviousAspect : AspectType;
    Signal_PreviousIndicatorState : IndicatorStateType;
    Signal_PreviousTheatreIndicatorString : String;
    Signal_PreviousSignal1 : Integer;
    Signal_PreviousSignal2 : Integer;
    Signal_PreviousHiddenStationSignalAspectSignal1 : Integer;
    Signal_PreviousHiddenStationSignalAspectSignal2 : Integer;
    Signal_PreviousLineX : Integer;
    Signal_PreviousLineY : Integer;
    Signal_PreviousLineWithVerticalSpacingY : Integer;
    Signal_Quadrant : QuadrantType;
    Signal_ResettingTC : Integer;

    { see note above for Signal_LockedArray }
    Signal_RouteLockingNeededArray : StringArrayType;

    Signal_SemaphoreDistantHomesArray : IntegerArrayType; { needed to tell a semaphore distant which semaphore homes lock it }
    Signal_SemaphoreDistantLocking : Integer;
    Signal_StateChanged : Boolean;
    Signal_TheatreIndicatorString : String; { what this signal might display }
    Signal_TRSHeld : Boolean;
    Signal_TRSHeldMsgWritten : Boolean;
    Signal_TRSReleased : Boolean;
    Signal_TRSReleasedMsgWritten : Boolean;
    Signal_Type : TypeOfSignal;
  END;

  WriteReadType = (ReadOnly, WriteOnly, WriteThenRead);

CONST
  Signal_AccessoryAddressFieldName : String = 'Signal Accessory Address';
  Signal_AdjacentLineFieldName : String = 'Signal Adjacent Line';
  Signal_AdjacentLineXOffsetFieldName : String = 'Signal AdjacentLine XOffset';
  Signal_ApproachControlAspectFieldName : String = 'Signal Approach Control Aspect';
  Signal_AsTheatreDestinationFieldName : String = 'Signal As Theatre Destination';
  Signal_AutomaticFieldName : String = 'Signal Automatic'; { not in use }
  Signal_DirectionFieldName : String = 'Signal Direction';
  Signal_DecoderNumFieldName : String = 'Signal Decoder Num';
  Signal_FromWhichUserMustDriveFieldName : String = 'Signal From Which User Must Drive';
  Signal_IndicatorDecoderFunctionNumFieldName : String = 'Signal Indicator Decoder Function Num';
  Signal_IndicatorDecoderNumFieldName : String = 'Signal Indicator Decoder Num';
  Signal_IndicatorSpeedRestrictionFieldName : String = 'Signal Indicator Speed Restriction';
  Signal_IndicatorFieldName : String = 'Signal Indicator';
  Signal_JunctionIndicatorsFieldName : String = 'Signal Junction Indicators';
  Signal_LocationsToMonitorFieldName : String = 'Signal Locations To Monitor';
  Signal_LowerLeftIndicatorTargetFieldName : String = 'Signal Lower Left Indicator Target';
  Signal_LowerRightIndicatorTargetFieldName : String = 'Signal Lower Right Indicator Target';
  Signal_MiddleLeftIndicatorTargetFieldName : String = 'Signal Middle Left Indicator Target';
  Signal_MiddleRightIndicatorTargetFieldName : String = 'Signal Middle Right Indicator Target';
  Signal_NextSignalIfNoIndicatorFieldName : String = 'Signal Next Signal If No Indicator';
  Signal_NotesFieldName : String = 'Signal Notes';
  Signal_NotUsedForRouteingFieldName : String = 'Signal Not Used For Routeing';
  Signal_NumberFieldName : String = 'Signal Number';
  Signal_OppositePassingLoopSignalFieldName : String = 'Signal Opposite Passing Loop Signal';
  Signal_OutOfUseFieldName : String = 'Signal Out Of Use';
  Signal_PossibleRouteHoldFieldName : String = 'Signal Possible Route Hold';
  Signal_PossibleStationStartRouteHoldFieldName : String = 'Signal Possible Station Start Route Hold';
  Signal_QuadrantFieldName : String = 'Signal Quadrant';
  Signal_SemaphoreDistantHomesArrayFieldName : String = 'Signal Distant Homes';
  Signal_TypeFieldName : String = 'Signal Type';
  Signal_UpDownFieldName : String = 'Signal Direction';
  Signal_UpperLeftIndicatorTargetFieldName : String = 'Signal Upper Left Indicator Target';
  Signal_UpperRightIndicatorTargetFieldName : String = 'Signal Upper Right Indicator Target';
  Signal_VerticalSpacingFieldName : String = 'Signal Vertical Spacing';

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

  { Track-circuit-related type declarations }
  TrackCircuitStateType = (TCFeedbackOccupation, TCFeedbackOccupationButOutOfUse, TCPermanentFeedbackOccupation, TCPermanentOccupationSetByUser, TCSystemOccupation,
                           TCPermanentSystemOccupation, TCMissingOccupation, TCOutOfUseSetByUser, TCOutOfUseAsNoFeedbackReceived, TCLocoOutOfPlaceOccupation, TCUnoccupied);

  TrackCircuitRec = RECORD
    TC_AdjacentBufferStop : Integer;
    TC_AdjacentSignals : IntegerArrayType;
    TC_DataChanged : Boolean;
    TC_EmergencyLocoChip : Integer;
    TC_EmergencyState : TrackCircuitStateType;
    TC_FeedbackInput : Integer;
    TC_FeedbackUnit : Integer;
    TC_FeedbackOccupation : Boolean;
    TC_Flashing : Boolean;
    TC_Gradient : GradientType;
    TC_HeadCode : String;
    TC_Journey : Integer;
    TC_LengthInInches : Extended;
    TC_LineArray : IntegerArrayType;
    TC_LitUp : Boolean; { used for flashing }
    TC_Location : Integer;
    TC_LockedForRoute : Integer;
    TC_LockFailureNotedInSubRouteUnit : Boolean;
    TC_LocoChip : Integer;
    TC_LocoStalled : Boolean;
    TC_MissingTrainNoted : Boolean;
    TC_MysteriouslyOccupied : Boolean;
    TC_Number : Integer;
    TC_OccupationStartTime : TDateTime;
    TC_OccupationState : TrackCircuitStateType;
    TC_PreviousLocoChip : Integer;
    TC_PreviousOccupationState : TrackCircuitStateType;
    TC_ResettingSignal : Integer;
    TC_SaveRouteLocking : Integer;
    TC_SaveTrackCircuitHeadCode : String;
    TC_SpeedRestrictionInMPH : MPHType;
    TC_SpeedRestrictionDirection : DirectionType;
  END;

CONST
  TC_NumberFieldName : String = 'TCNum';
  TC_LengthInInchesFieldName : String = 'Length';
  TC_FeedbackUnitFieldName : String = 'FeedbackUnit';
  TC_FeedbackInputFieldName : String = 'FeedbackInput';

TYPE
  { Train-related type declarations }
  TypeOfTrainType = (LightLocoType, ExpressPassengerType, OrdinaryPassengerType, ExpressFreightType, Freight75mphType, EmptyCoachingStockType, Freight60mphType,
                     Freight45mphType, Freight35mphType, InternationalType, UnknownTrainType);
  TrainTypeArray = ARRAY OF TypeOfTrainType;

  TrainJourneyRec = RECORD
    TrainJourney_ActualArrivalTime : TDateTime;
    TrainJourney_ActualDepartureTime : TDateTime;
    TrainJourney_AdditionalRequiredStationWaitInMinutes : Integer;
    TrainJourney_Cleared : Boolean;
    { TrainJourney_Commenced : Boolean; - use TrainJourney_ActualDepartureTime <> 0 instead }
    TrainJourney_Created : Boolean;
    TrainJourney_CurrentArrivalTime : TDateTime;
    TrainJourney_CurrentDepartureTime : TDateTime;
    TrainJourney_DiagrammedArrivalTime : TDateTime;
    TrainJourney_DiagrammedDepartureTime : TDateTime;
    TrainJourney_DiagrammedStartLocation : Integer;
    TrainJourney_DiagrammedEndLocation : Integer;
    TrainJourney_Direction : DirectionType;
    TrainJourney_DurationInMinutes : Integer;
    TrainJourney_EndArea : Integer;
    TrainJourney_EndBufferStop : Integer;
    TrainJourney_EndLine : Integer;
    TrainJourney_EndLocation : Integer;
    TrainJourney_EndStationName : String;
    TrainJourney_EndSignal : Integer;
    TrainJourney_FirstTC : Integer;
    TrainJourney_LengthInInches : Real;
    TrainJourney_LocationsPending : Boolean;
    TrainJourney_LockingArray : StringArrayType;
    TrainJourney_NotForPublicUse : Boolean;
    TrainJourney_Route : Integer;
    TrainJourney_RouteArray : StringArrayType;
    TrainJourney_SetUp : Boolean;
    TrainJourney_StartArea : Integer;
    TrainJourney_StartLine : Integer;
    TrainJourney_StartLocation : Integer;
    TrainJourney_StartStationName : String;
    TrainJourney_StartOfRepeatJourney : Boolean;
    TrainJourney_StartSignal : Integer;
    TrainJourney_StoppingOnArrival : Boolean;
    TrainJourney_UserToDrive : Boolean;
  END;

  TrainJourneyRecArrayType = ARRAY OF TrainJourneyRec;

  { Note: we need Missing, Suspended, and MissingAndSuspended as otherwise the status can oscillate between Missing and Suspended if a train is suspended while missing - in
    that case, unsuspending renders it missing even though it may no longer be missing, and it's then a status we can't get out of.
  }
  TrainStatusType = (ReadyForCreation, WaitingForLightsOn, WaitingForHiddenStationSignalAspectToClear, WaitingForRouteing, InLightsOnTime, ReadyForRouteing,
                     CommencedRouteing, ReadyToDepart, Departed, RouteingWhileDeparted, RouteCompleted, WaitingForRemovalFromDiagrams, ToBeRemovedFromDiagrams,
                     RemovedFromDiagrams, Missing, MissingAndSuspended, Suspended, NonMoving, Cancelled, UnknownTrainStatus);
  TrainIndex = Integer;

  TrainRec = RECORD
    Train_LocoChip : Integer;
    Train_DoubleHeaderLocoChip : Integer;
    Train_LocoChipStr : String; { from loco record }
    Train_DoubleHeaderLocoChipStr : String; { from loco record }
    Train_LocoIndex : LocoIndex;
    Train_DoubleHeaderLocoIndex : LocoIndex;

//    Train_Accelerating : Boolean;
//    Train_AccelerationAdjustRange : Integer;
//    Train_AccelerationStartTime : TDateTime;
//    Train_AccelerationStr : String;
    Train_AccelerationTimeInSeconds : Real;
//    Train_AccelerationTimeInterval : Real;
    Train_ActualNumStr : String; { from loco record }
    Train_AtCurrentBufferStop : Integer;
    Train_AtCurrentSignal : Integer;
    Train_AtHiddenStationSignalAspectSignal : Integer; { to stop trains at signals that otherwise would be off }
    Train_BeingAdvanced : Boolean;
    Train_BeingAdvancedTC : Integer;
    Train_CabLightsAreOn : Boolean;
    Train_CabLightsHaveBeenOn : Boolean;
//    Train_ControlState : LocoControlStateType;
    Train_CurrentArrivalTime : TDateTime;
    Train_CurrentBufferStop : Integer;
    Train_CurrentDirection : DirectionType;
    Train_CurrentJourney : Integer;
    Train_CurrentLengthInInches : Integer;
    Train_CurrentRoute : Integer;
    Train_CurrentSignal : Integer;
    Train_CurrentSourceLocation : Integer;
    Train_CurrentSpeedInMPH : MPHType;
    Train_CurrentStatus : TrainStatusType;
    Train_CurrentTC : Integer;
//    Train_Decelerating : Boolean;
    Train_Description : String;
    Train_DesiredSpeedInMPH : MPHType;
    Train_DiagramFound : Boolean;
    Train_DiagramsGridRowNums : IntegerArrayType;
    Train_DistanceToCurrentSignalOrBufferStop : Real;
    Train_DistanceToNextSignalButOneOrBufferStop : Real;
    Train_DistanceToNextSignalOrBufferStop : Real;
    Train_EmergencyRouteing : Boolean;
    Train_ExtraPowerAdjustment : Integer; { used temporarily to increase the train speed where necessary }
    Train_FirstStationSpecifiedStartTime : TDateTime;
    Train_FixedDirection : DirectionType;
    Train_FixedLengthInInches : Integer; { from loco record }
//    Train_Functions : ARRAY [0..12] OF Boolean;
//    Train_Functions0To4Byte : Byte;
//    Train_Functions5To12Byte : Byte;
    Train_GradientSpeedAdjustment : Integer;
    Train_GradientSpeedAdjustmentMsgWritten : Boolean;
    Train_HasLights : Boolean;
    Train_Headcode : String;
    Train_InitialTrackCircuits : ARRAY [1..5] OF Integer;
    Train_InLightsOnTime : Boolean; { train inactive but for lights being on }
    Train_JourneysArray : TrainJourneyRecArrayType;
    Train_LastLengthInInches : Integer; { from loco record }
    Train_LastLocation : Integer;
    Train_LastMissingTC : Integer;
    Train_LastRouteLockedMsgStr : String;
    Train_LastSignal : Integer;
    Train_LastTC : Integer; { from loco record }
    Train_LightsOn : Boolean;
    Train_LightsOnTime : TDateTime;
    Train_LightsRemainOnWhenJourneysComplete : Boolean;
    Train_LocatedAtStartup : Boolean;
    Train_Locations : IntegerArrayType;
    Train_LocoClassStr : String; { from loco record }
    Train_LocoName : String; { from loco record }
    Train_LocoTypeStr : String; { from loco record }
    Train_MaximumSpeedInMPH : MPHType;
    Train_MinimumAccelerationTimeInSeconds : Integer; { needed as we only calculate it once when we enter a track circuit }
    Train_MissingMessage : Boolean;
    Train_MissingNum : Integer;
    Train_NextTC : Integer;
    Train_NextButOneTC : Integer;
    Train_NotInPlaceMsgWritten : Boolean;
    Train_NotLocatedAtStartupMsgWritten : Boolean;
    Train_NumberOfCarriages : Integer;
    Train_PossibleRerouteTime : TDateTime;
//    Train_PreviousControlState : LocoControlStateType;
    Train_PreviousStatus : TrainStatusType;
    Train_PreviousTC : Integer;
    Train_Reversing : Boolean;
    Train_ReversingDelayInSeconds : Cardinal;
    Train_ReversingStartTime : TDateTime;
    Train_ReversingWaitStarted : Boolean;
    Train_RouteCheckedTime : TDateTime;
    Train_RouteCreationHeldJourney : Integer;
    Train_RouteCreationHeldMsgWrittenArray : ARRAY [FirstRouteCreationHeldMsgNumber..LastRouteCreationHeldMsgNumber] OF Boolean;
    Train_RouteCreationHoldNum : Integer;
    Train_RouteCreationHoldMsg : String;
    Train_RouteCreationPlatformHeldStr : String;
    Train_RouteCreationReleasedMsg : String;
    Train_RouteingHeldAtSignal : Integer;
    Train_SaveCurrentTC : Integer;
    Train_SavedLocation : Integer;
    Train_SavedRoute : Integer;
    Train_SaveSpeedInFiddleyardMsg : String;
    Train_SaveTCsClearedStr : String;
    Train_SaveTCsForReleaseStr : String;
    Train_SaveTCsOccupiedStr : String;
    Train_SectionStartTime : TDateTime;
    Train_SpeedString : String;
    Train_StalledMsgWritten : Boolean;
    Train_SubRouteAheadCheckedTime : TDateTime;
    Train_TakenOverByUserMsgWritten : Boolean;
    Train_TCsAndSignalsNotClearedArray : StringArrayType;
    Train_TCsAndSignalsNotClearedStr : String;
    Train_TCsNotClearedArray : StringArrayType;
    Train_TCsNotClearedStr : String;
    Train_TCsOccupiedOrClearedArray : StringArrayType;
    Train_TCsOccupiedOrClearedStr : String;
    Train_TCsReleasedArray : StringArrayType;
    Train_TCsReleasedStr : String;
    Train_TempDraftRouteArray : StringArrayType;
    Train_TempLockingArray : StringArrayType;
    Train_TerminatingSpeedReductionMsgWritten : Boolean;
    Train_TotalJourneys : Integer; { starts at 0 }
    Train_Type : TypeOfTrainType;
    Train_TypeNum : Integer;
    Train_UserDriving : Boolean;
    Train_UserPowerAdjustment : Integer; { used by the user to increase or decrease the train speed where necessary }
    Train_UserRequiresInstructions : Boolean;
    Train_UserSpeedInstructionMsg : String;
    Train_UseTrailingTrackCircuits : Boolean;
    { where a train doesn't have lights at both ends, it may need artificial track-circuit activation }
    Train_WaitingForHiddenStationSignalAspectStartTime : TDateTime;
    Train_WorkingTimetableLastArrivalArea : Integer;
    Train_WorkingTimetableLastArrivalTime : TDateTime;
    Train_WorkingTimetableLastEntryNumStr : String;
  END;

  RouteingExceptionRec = RECORD
    RouteingException_AllowedInEmergency : Boolean;
    RouteingException_CurrentLines : IntegerArrayType;
    RouteingException_CurrentLinesExcepted : IntegerArrayType;
    RouteingException_EndAreas : IntegerArrayType;
    RouteingException_EndAreasExcepted : IntegerArrayType;
    RouteingException_EndLines : IntegerArrayType;
    RouteingException_EndLinesExcepted : IntegerArrayType;
    RouteingException_EndLocations : IntegerArrayType;
    RouteingException_EndLocationsExcepted : IntegerArrayType;
    RouteingException_LinesRoutedOver : IntegerArrayType;
    RouteingException_LinesRoutedOverExcepted : IntegerArrayType;
    RouteingException_MaxTrainLength : Integer;
    RouteingException_PreviousLines : IntegerArrayType;
    RouteingException_PreviousLinesExcepted : IntegerArrayType;
    RouteingException_RouteDirection : DirectionType;
    RouteingException_Rule : Integer;
    RouteingException_StartAreas : IntegerArrayType;
    RouteingException_StartAreasExcepted : IntegerArrayType;
    RouteingException_StartLines : IntegerArrayType;
    RouteingException_StartLinesExcepted : IntegerArrayType;
    RouteingException_StartLocations : IntegerArrayType;
    RouteingException_StartLocationsExcepted : IntegerArrayType;
    RouteingException_StopStr : String;
    RouteingException_TrainTypes : TrainTypeArray;
  END;

  LightsToBeSwitchedOnRec = RECORD
    LightsToBeSwitchedOn_Train: TrainIndex;
    LightsToBeSwitchedOn_ColourStr1 : String;
    LightsToBeSwitchedOn_ColourStr2 : String;
    LightsToBeSwitchedOn_Direction1 : DirectionType;
    LightsToBeSwitchedOn_Direction2 : DirectionType;
    LightsToBeSwitchedOn_SwitchOnTime : TDateTime;
  END;

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
  TrainArrayType = ARRAY OF TrainIndex;

  LenzSystemRec = RECORD
                EmergencyStop : Boolean;
                EmergencyOff : Boolean;
                ProgrammingMode : Boolean;
                StartMode : Boolean;
              END;

  { not yet in use - failed in testing 1/09 }
  SavedRouteRec = RECORD
    SavedRoute_Array : StringArrayType;
    SavedRoute_StartLine : Integer;
    SavedRoute_EndLine : Integer;
    SavedRoute_Direction : DirectionType;
    SavedRoute_TrainType : TypeOfTrainType;
    SavedRoute_EmergencyRouteing : Boolean;
  END;

CONST
  FirstTypeOfLine = MainOrGoods;
  LastTypeOfLine = ProjectedLine;

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
  BufferStops : ARRAY OF BufferStopRec;
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
  FeedbackUnitRecords : ARRAY OF FeedbackRec;
  FirstFeedbackUnit : Integer = -1;
  LastFeedbackUnit : Integer = 99999;
  FullScreenPenWidth : Integer = 5;
  GridInterLineSpacing : Integer = 0;
  InAutoMode : Boolean = False;
  InterLineSpacing : Integer = 0;
  KeyBoardandMouseLocked : Boolean = False;
  LastPointChanged : Integer = UnknownPoint;
  LastTimeAnyPointChanged : TDateTime = 0;
  LenzConnection : LenzConnectionType = NoConnection;
  LightsToBeSwitchedOnArray : ARRAY OF LightsToBeSwitchedOnRec;
  LineHighlighted : Integer = UnknownLine;
  Lines : ARRAY OF LineRec;
  LinesInitialised : Boolean = False;
  Locations : ARRAY OF LocationRec;
  LocationOccupations : ARRAY OF ARRAY OF LocationOccupationRec;
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
  RouteingByUser : Boolean = False;
  RouteingExceptions : ARRAY OF RouteingExceptionRec;
  RouteingSuspendedForModalDialogue : Boolean = False;
  RouteingSuspendedWhenStopPressed : Boolean = False;
  RouteWritingCancel : Boolean = False;
  SaveStationMonitorsCurrentArea : Integer = UnknownArea;
  SaveLocoDialogueMaskEditText : String = '';
  SaveMouseCursorPos : TPoint;
  SaveRouteInfoStr : String = '';
  ScreenColoursSetForPrinting : Boolean = False;
  ScreenMode : ScreenModeType = DefaultWindowedScreenMode;
  ShowCreateRouteExitFunctionNum : Boolean = False;
  ShowRouteLengths : Boolean = False;
  SignalHighlighted : Integer = UnknownSignal;
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
  TempTrainArray : ARRAY OF TrainIndex;
  TerminatingSpeedReductionMode : Boolean = False;
  TextWindowHeight : Word;
  ThinLineMode : Boolean = True;
  TimeLastDataReceived : Cardinal = 0;
  TimeOutMsgWritten : Boolean = False;
  TrackCircuitHighlighted : Integer = UnknownTrackCircuit;
  TrackCircuits : ARRAY OF TrackCircuitRec;
  TrackCircuitsInitialised : Boolean = False;
  Trains : ARRAY OF TrainRec;
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
  Signals : ARRAY OF SignalRec;

  Routes_ApproachControlledSignals : ARRAY OF IntegerArrayType;
  Routes_ApproachControlSignalsMsgWrittenArray : BooleanArrayType;
  Routes_ApproachControlSignalsWaitingToBeSet : ARRAY OF StringArrayType;
  Routes_ApproachControlsSet : BooleanArrayType; { once this on, all subsequent route setting is done using it }
  Routes_AutoSettingMode : BooleanArrayType;
  Routes_Cleared : BooleanArrayType;
  Routes_ClearingFailuresMsg1WrittenArray : BooleanArrayType;
  Routes_ClearingFailuresMsg2WrittenArray : BooleanArrayType;
  Routes_ClearingFailuresMsg3WrittenArray : BooleanArrayType;
  Routes_CurrentClearingSubRoute : IntegerArrayType;
  Routes_CurrentSettingSubRoute : IntegerArrayType;
  Routes_CurrentSubRouteClearingPos : IntegerArrayType;
  Routes_CurrentSubRouteSettingPos : IntegerArrayType;
  Routes_Directions : DirectionArrayType;
  Routes_EndBufferStops : IntegerArrayType;
  Routes_EndLines : IntegerArrayType;
  Routes_EndSignals : IntegerArrayType;
  Routes_Journeys : IntegerArrayType;
  Routes_LocoChips : IntegerArrayType;
  Routes_NearestSignalTestingInitiated : Boolean = False; { This is for testing route saving }
  Routes_PointResultPendingMsgWrittenArray : BooleanArrayType;
  Routes_PointResultPendingPoint : IntegerArrayType;
  Routes_PointResultPendingPointMsgWrittenArray : BooleanArrayType;
  Routes_PossibleRerouteTime : DateTimeArrayType;
  Routes_RouteClearingsInProgress : BooleanArrayType;
  Routes_RouteClearingsWithoutPointResetting : BooleanArrayType;
  Routes_RouteCounter : Integer = -1;
  Routes_RouteingsCancelled : BooleanArrayType;
  Routes_Routes : IntegerArrayType;
  Routes_RouteSettingByEmergencyRoute : Boolean = False;
  Routes_RouteSettingByHand : Boolean = False;
  Routes_RouteSettingsCompleted : BooleanArrayType;
  Routes_RouteSettingsInProgress : BooleanArrayType;
  Routes_RoutesSettingUpHeldMsgWrittenArray : BooleanArrayType;
  Routes_RoutesSettingUpStalledMsgArray : StringArrayType;
  Routes_RoutesSettingUpStalledMsgWrittenArray : BooleanArrayType;
  Routes_RoutesSettingUpStalledTimeArray : DateTimeArrayType;
  Routes_SettingUpFailuresMsgWrittenArray : BooleanArrayType;
  Routes_StartLines : IntegerArrayType;
  Routes_StartSignals : IntegerArrayType;
  Routes_SubRouteClearingStrings : ARRAY OF ARRAY OF StringArrayType; { ie a 3-dimensional ARRAY OF String }
  Routes_SubRouteEndLines : ARRAY OF IntegerArrayType;
  Routes_SubRoutesAheadNotClearMsgWrittenArray : BooleanArrayType;
  Routes_SubRouteSettingStrings : ARRAY OF ARRAY OF StringArrayType; { ie a 3-dimensional ARRAY OF String }
  Routes_SubRouteStartLines : ARRAY OF IntegerArrayType;
  Routes_SubRouteStartSignals : ARRAY OF IntegerArrayType;
  Routes_SubRouteStates : ARRAY OF SubRouteStateArrayType; { 2D as there are 3 states per subroute }
  Routes_TheatreIndicatorSettingInitiated : Boolean = False;
  Routes_TotalSubRoutes : IntegerArrayType;
  Routes_Trains : TrainArrayType;
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

PROCEDURE AddNewRecordToTrackCircuitDatabase;
{ Append a record to the track-circuit database }

PROCEDURE CalculatePlatformPositions;
{ Create the platform rectangle }

PROCEDURE CalculateTCAdjacentSignals;
{ Work out which track circuits are next the signal }

PROCEDURE CalculateTCAdjacentBufferStops;
{ Work out which track circuits are next a buffer stop }

FUNCTION DeleteRecordFromTrackCircuitDatabase(TrackCircuitToDeleteNum : Integer) : Boolean;
{ Remove a record from the track-circuit database }

FUNCTION DescribeActualDateAndTime : String;
{ Return the current real date and time as a String }

PROCEDURE InitialiseInitVarsUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

PROCEDURE InitialiseLogFiles;
{ Open a new file for test output - rename two previous ones if they exist }

PROCEDURE InitialiseScreenDrawingVariables;
{ Set up the default screen drawing variables }

PROCEDURE InitialiseTrackCircuitVariables(TC : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }

PROCEDURE ReadInAreasDataFromDatabase;
{ Initialise the area data }

PROCEDURE ReadInFeedbackDataFromDatabase;
{ Initialise the feedback unit data }

PROCEDURE ReadInLocationDataFromDatabase;
{ Initialise the location data }

PROCEDURE ReadInPlatformDataFromDatabase;
{ Initialise the platform data }

PROCEDURE ReadInTrackCircuitDataFromDatabase;
{ Initialise the track circuit data which depends on lines being initialised first }

PROCEDURE RestoreScreenDrawingVariables;
{ Restore the default screen drawing variables }

PROCEDURE SaveScreenDrawingVariables;
{ Save the screen drawing variables }

FUNCTION ValidateGridX(Str : String; OUT ErrorMsg : String) : Integer;
{ Converts grid string to number }

FUNCTION ValidateRow(RowStr : String; OUT ErrorMsg : String) : Extended;
{ See whether the row provided is valid }

PROCEDURE WriteOutLocationDataToDatabase;
{ Write out some location data to the location data file }

PROCEDURE WriteOutTrackCircuitDataToDatabase;
{ Write out some track-circuit data to the track-circuit data file }

IMPLEMENTATION

{$R *.dfm}

USES
  LocoUtils, MiscUtils, Lenz, RailDraw, IniFiles, Startup, DateUtils, GetTime, Diagrams, StrUtils, Grids, Movement, LocationData, Feedback, Options, PointsUnit;

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

PROCEDURE InitialiseTrackCircuitVariables(TC : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }
BEGIN
  WITH TrackCircuits[TC] DO BEGIN
    SetLength(TC_AdjacentSignals, 0);
    TC_AdjacentBufferStop := UnknownBufferStop;
    TC_DataChanged := False;
    TC_EmergencyLocoChip := UnknownLocoChip;
    TC_EmergencyState := TCUnoccupied;
    TC_FeedbackOccupation := False;
    TC_Flashing := False;
    TC_Gradient := Level;
    TC_HeadCode := '';
    TC_Journey := UnknownJourney;
    TC_LitUp := True;
    TC_Location := UnknownLocation;
    TC_LockedForRoute := UnknownRoute;
    TC_LockFailureNotedInSubRouteUnit := False;
    TC_LocoChip := UnknownLocoChip;
    TC_LocoStalled := False;
    TC_MissingTrainNoted := False;
    TC_MysteriouslyOccupied := False;
    TC_OccupationStartTime := 0;
    TC_OccupationState := TCUnoccupied;
    TC_PreviousLocoChip := UnknownLocoChip;
    TC_PreviousOccupationState := TCUnoccupied;
    TC_ResettingSignal := UnknownSignal;
    TC_SaveRouteLocking := UnknownRoute;
    TC_SaveTrackCircuitHeadCode := '';
    TC_SpeedRestrictionInMPH := NoSpecifiedSpeed;
    TC_SpeedRestrictionDirection := Bidirectional;

    SetLength(TC_LineArray, 0);
  END; {WITH}
END; { InitialiseTrackCircuitVariables(TC); }

PROCEDURE ReadInTrackCircuitDataFromDatabase;
{ Initialise the track-circuit data which depends on lines being initialised first.

  Interesting notes from the past here:

  Unit 77 inputs 5-8 free
  Unit 78 (tunnel) for UMC
  Unit 86 input 5 does not have a corresponding LB101 as 6-8 are photocell inputs
  Unit 94 input 6 reserved for new siding point (1 to 5 are points already)
  Unit 112 input 8 (FY) is free [TC input]

  Unit 93 inputs 2-3 reserved for new siding A
  Unit 95 inputs 5-6 reserved for new siding B
  Unit 99 inputs 3-4 reserved for cross over points from platforms 5 to 6
  Unit 116 input 2 free [point input]
}
CONST
  StopTimer = True;

VAR
  ErrorMsg : String;
  HasFeedback : Boolean;
  Location : Integer;
  TC : Integer;
  TempStr : String;

BEGIN
  TRY
    Log('A INITIALISING TRACK CIRCUITS {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track-circuit database file "' + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInTrackCircuitDataFromDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;
      Log('T Track-circuit data table and connection opened to initialise the track circuits');

      TrackCircuitsADOTable.Sort := '[' + TC_NumberFieldName + '] ASC';
      TrackCircuitsADOTable.First;
      SetLength(TrackCircuits, 0);

      WHILE NOT TrackCircuitsADOTable.EOF DO BEGIN
        WITH TrackCircuitsADOTable DO BEGIN
          SetLength(TrackCircuits, Length(TrackCircuits) + 1);
          TC := High(TrackCircuits);
          WITH TrackCircuits[TC] DO BEGIN
            ErrorMsg := '';

            TC_Number := TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsInteger;
            IF TC_Number <> TC THEN
              ErrorMsg := 'it does not match the line number in the database (' + IntToStr(TC_Number) + ')';

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(TC_LengthInInchesFieldName).AsString;
              IF TempStr = '' THEN
                TC_LengthInInches := 0
              ELSE
                IF NOT TryStrToFloat(TempStr, TC_LengthInInches) THEN
                  Debug('TC=' + IntToStr(TC_Number) + ': invalid length String "' + TempStr + '"');
            END;

            IF ErrorMsg = '' THEN
              TC_FeedbackUnit := ValidateFeedbackUnit(FieldByName(TC_FeedbackUnitFieldName).AsString, HasFeedback, ErrorMsg);

            IF ErrorMsg = '' THEN
              TC_FeedbackInput := ValidateFeedbackInput(FieldByName(TC_FeedbackInputFieldName).AsString, HasFeedback, TC_FeedbackUnit, TrackCircuitFeedback,
                                                        TC_Number, ErrorMsg);
            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating TC=' + IntToStr(High(TrackCircuits)) + ' (' + IntToStr(TC_Number) + '): '
                                            + '[' + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInTrackCircuitDataFromDatabase');
            END;
          END; {WITH}
        END; {WITH}
        TrackCircuitsADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('T Track-circuit data table and connection closed');
    END; {WITH}

    FOR TC := 0 TO High(TrackCircuits) DO
      InitialiseTrackCircuitVariables(TC);

    { Initialise track-circuit lines and locations }
    CalculateTCAdjacentBufferStops;

    { Check that all track circuits have a TC_LineArray value }
    TC := 0;
    WHILE TC <= High(TrackCircuits) DO BEGIN
      IF Length(TrackCircuits[TC].TC_LineArray) = 0 THEN
        Debug('!TC=' + IntToStr(TC) + ': has no entry in the LineData file');
      Inc(TC);
    END; {WHILE}

    { Work out how long individual locations are based on track-circuit length. This can be overriden by a specific length given below, however. }
    FOR Location := 0 TO High(Locations) DO BEGIN
      { Check that the location doesn't already have a designated preset length - this is necessary in platforms, for instance, as the total track-circuit length may well
        include the line beyond signals.
      }
      IF Locations[Location].Location_LengthInInches = 0 THEN
        FOR TC := 0 TO High(TrackCircuits) DO BEGIN
          IF TrackCircuits[TC].TC_Location = Location THEN BEGIN
            Locations[Location].Location_LengthInInches := Locations[Location].Location_LengthInInches + TrackCircuits[TC].TC_LengthInInches;
          END;
        END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInTrackCircuitDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadInTrackCircuitDataFromDatabase }

PROCEDURE AddNewRecordToTrackCircuitDatabase;
{ Append a record to the track-circuit database }
BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track-circuit database file "' + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                      + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'AddNewRecordToTrackCircuitDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;
      TrackCircuitsADOTable.Append;
      TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsInteger := High(TrackCircuits);
      TrackCircuitsADOTable.Post;

      Log('S Track-circuit data table and connection opened to create record for TrackCircuit ' + IntToStr(High(TrackCircuits)));
      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('S Track-circuit data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToTrackCircuitDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { AddNewRecordToTrackCircuitDatabase }

PROCEDURE WriteOutTrackCircuitDataToDatabase;
{ Write out some track-circuit data to the track-circuit data file }
VAR
  TC : Integer;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track-circuit database file "' + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                      + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'WriteOutTrackCircuitDataFromDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;
      Log('T Track-circuit data table and connection opened to write out track-circuit data');

      TrackCircuitsADOTable.First;
      TC := 0;

      WHILE TC <= High(TrackCircuits) DO BEGIN
        WITH TrackCircuits[TC] DO BEGIN
          IF TC_DataChanged THEN BEGIN
            TC_DataChanged := False;

            IF NOT TrackCircuitsADOTable.Locate(TC_NumberFieldName, IntToStr(TC), []) THEN BEGIN
              Log('T Track-circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TC) + ' but it cannot be found');
            END ELSE BEGIN
              Log('T Track-circuit data table and connection opened to record that ' + TC_NumberFieldName + ' is ''' + IntToStr(TC_Number) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsString := IntToStr(TC_Number);
              TrackCircuitsADOTable.Post;

              Log('T Track-circuit data table and connection opened to record that ' + TC_LengthInInchesFieldName + ' is ''' + FloatToStr(TC_LengthInInches) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_LengthInInchesFieldName).AsString := FloatToStr(TC_LengthInInches);
              TrackCircuitsADOTable.Post;

              Log('T Track-circuit data table and connection opened to record that ' + TC_FeedbackUnitFieldName + ' is ''' + IntToStr(TC_FeedbackUnit) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_FeedbackUnitFieldName).AsString := IntToStr(TC_FeedbackUnit);
              TrackCircuitsADOTable.Post;

              Log('T Track-circuit data table and connection opened to record that ' + TC_FeedbackInputFieldName + ' is ''' + IntToStr(TC_FeedbackInput) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_FeedbackInputFieldName).AsString := IntToStr(TC_FeedbackInput);
              TrackCircuitsADOTable.Post;
            END;
          END;
        END; {WITH}

        Inc(TC);
      END; {WHILE}

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('L Track-circuit data table and connection closed after writing track-circuit data');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutTrackCircuitData: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutTrackCircuitDataToDatabase }

FUNCTION DeleteRecordFromTrackCircuitDatabase(TrackCircuitToDeleteNum : Integer) : Boolean;
{ Remove a record from the track-circuit database }
BEGIN
  Result := False;
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track-circuit database file "' + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                      + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'DeleteRecordFromTrackCircuitDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;

      IF NOT TrackCircuitsADOTable.Locate(TC_NumberFieldName, IntToStr(TrackCircuitToDeleteNum), []) THEN BEGIN
        Log('T Track-circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TrackCircuitToDeleteNum) + ' but it cannot be found');
      END ELSE BEGIN
        Log('T Track-circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TrackCircuitToDeleteNum));

        { Now delete the track circuit - we have already checked, in the Edit unit, whether deleting it will cause knock-on problems with other track circuits }
        TrackCircuitsADOTable.Delete;
        Log('TG TrackCircuit ' + IntToStr(TrackCircuitToDeleteNum) + ' has been deleted');
        Result := True;
      END;

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('T Track-circuit data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DeleteRecordFromTrackCircuitDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DeleteRecordFromTrackCircuitDatabase }

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

PROCEDURE InitialiseLocationVariables(Location : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }
BEGIN
  WITH Locations[Location] DO BEGIN
    Location_Area := UnknownArea;
    SetLength(Location_LocosNotAbleToUse, 0);
    SetLength(Location_LocoClassesReservedFor, 0);
    Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyard := UnknownLocation;
    Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr := '';
    Location_PlatformOrFiddleyardDirection := UnknownDirection;
    Location_PlatformOrFiddleyardNumStr := '';
    Location_RecordInLocationOccupationArray := False;
  END; {WITH}
END; { InitialiseLocationVariables }

PROCEDURE ReadInLocationDataFromDatabase;
{ Initialise the location data }
CONST
  StopTimer = True;

VAR
  A : Integer;
  AccessibleLocationsCount : Integer;
  ErrorMsg : String;
  I : Integer;
  Loc_Num : Integer;
  Location : Integer;
  LocationExceptions : IntegerArrayType;
  LocationExceptionsStrArray : StringArrayType;
  LocoChip : Integer;
  NewLocation : Integer;
  OK : Boolean;
  TempArea : Integer;
  TempLocation : Integer;
  TempStrArray : StringArrayType;
  TempStr : String;
  TestStr1, TestStr2 : String;

BEGIN
  TRY
    Log('A INITIALISING LOCATIONS {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + LocationDataFilename + '.' + LocationDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Location database file "' + PathToRailDataFiles + LocationDataFilename + '.' + LocationDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInLocationDataFromDatabase')
        ELSE
          Exit;
      END;

      LocationsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                 + PathToRailDataFiles + LocationDataFilename + '.' + LocationDataFilenameSuffix
                                                 + ';Persist Security Info=False';
      LocationsADOConnection.Connected := True;
      LocationsADOTable.Open;
      Log('T Location data table and connection opened to initialise the location data');

      { First see if the location numbers in the MSAccess file are sequential and, if not, renumber it - we need this or deletions from the MSAccess file will cause
        problems
      }
      Loc_Num := -1;
      LocationsADOTable.First;
      WHILE NOT LocationsADOTable.EOF DO BEGIN
        Inc(Loc_Num);
        IF LocationsADOTable.FieldByName(Location_NumberFieldName).AsInteger <> Loc_Num THEN BEGIN
          { we need to renumber from here on }
          LocationsADOTable.Edit;
          LocationsADOTable.FieldByName(Location_NumberFieldName).AsInteger := Loc_Num;
          LocationsADOTable.Post;
        END;
        LocationsADOTable.Next;
      END; {WHILE}

      LocationsADOTable.First;
      SetLength(Locations, 0);

      WITH LocationsADOTable DO BEGIN
        LocationsADOTable.Sort := '[' + Location_NumberFieldName + '] ASC';
        WHILE NOT LocationsADOTable.EOF DO BEGIN
          SetLength(Locations, Length(Locations) + 1);
          WITH Locations[High(Locations)] DO BEGIN
            InitialiseLocationVariables(High(Locations));
            ErrorMsg := '';

            Loc_Num := LocationsADOTable.FieldByName(Location_NumberFieldName).AsInteger;
            IF Loc_Num <> High(Locations) THEN
              ErrorMsg := 'it does not match the location number in the database (' + IntToStr(Loc_Num) + ')';

            IF ErrorMsg = '' THEN BEGIN
              Location_LongNameStr := LocationsADOTable.FieldByName(Location_NameStrFieldName).AsString;
              IF Location_LongNameStr = '' THEN
                ErrorMsg := 'missing long name';
            END;

            IF ErrorMsg = '' THEN BEGIN
              Location_ShortNameStr := LocationsADOTable.FieldByName(Location_ShortStringFieldName).AsString;
              IF Location_ShortNameStr = '' THEN
                ErrorMsg := 'missing short name';
            END;

            IF ErrorMsg = '' THEN
              Location_OutOfUse := LocationsADOTable.FieldByName(Location_OutOfUseFieldName).AsBoolean;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := LocationsADOTable.FieldByName(Location_AreaFieldName).AsString;
              IF TempStr = '' THEN
                Location_Area := UnknownArea
              ELSE BEGIN
                Location_Area := StrToArea(TempStr);
                IF Location_Area = UnknownArea THEN
                  ErrorMsg := 'unknown area "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := LocationsADOTable.FieldByName(Location_ThroughLocationFieldName).AsString;
              IF TempStr = '' THEN
                Location_ThroughLocationState := ThroughLocation
              ELSE
                IF TempStr = 'Non-Through Location' THEN
                  Location_ThroughLocationState := NonThroughLocation
                ELSE
                  ErrorMsg := 'unknown through location state "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := LocationsADOTable.FieldByName(Location_LocoClassesReservedForFieldName).AsString;
              IF TempStr <> '' THEN BEGIN
                ExtractSubStringsFromString(TempStr, ',', TempStrArray);
                FOR I := 0 TO High(TempStrArray) DO
                  AppendToStringArray(Location_LocoClassesReservedFor, TempStrArray[I]);
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := LocationsADOTable.FieldByName(Location_LocosNotAbleToUseFieldName).AsString;
              IF TempStr <> '' THEN BEGIN
                ExtractSubStringsFromString(TempStr, ',', TempStrArray);
                FOR I := 0 TO High(TempStrArray) DO BEGIN
                  IF NOT TryStrToInt(TempStrArray[I], LocoChip) THEN
                    ErrorMsg := 'invalid loco chip number "' + TempStrArray[I] + '"'
                  ELSE
                    AppendToIntegerArray(Location_LocosNotAbleToUse, LocoChip);
                END; {FOR}
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              Location_IsPlatform := LocationsADOTable.FieldByName(Location_PlatformFieldName).AsBoolean;
            END;

            IF ErrorMsg = '' THEN BEGIN
              Location_IsSiding := LocationsADOTable.FieldByName(Location_SidingFieldName).AsBoolean;
            END;

            IF ErrorMsg = '' THEN
              Location_IsFiddleyard := LocationsADOTable.FieldByName(Location_FiddleyardFieldName).AsBoolean;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := LocationsADOTable.FieldByName(Location_LengthInInchesFieldName).AsString;
              IF TempStr <> '' THEN
                IF NOT TryStrToFloat(TempStr, Location_LengthInInches) THEN
                  ErrorMsg := 'invalid length "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := LocationsADOTable.FieldByName(Location_TRSPlungerXFieldName).AsString;
              IF TempStr <> '' THEN
                IF NOT TryStrToInt(TempStr, Location_TRSPlungerX) THEN
                  ErrorMsg := 'invalid TRS Plunger X value "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := LocationsADOTable.FieldByName(Location_TRSPlungerYFieldName).AsString;
              IF TempStr <> '' THEN
                IF NOT TryStrToInt(TempStr, Location_TRSPlungerY) THEN
                  ErrorMsg := 'invalid TRS Plunger Y value "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr := LocationsADOTable.FieldByName(Location_PlatformParallelAccessFieldName).AsString;
              { Note: we can't test that this field contains a valid location until all the locations are read in }
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := LocationsADOTable.FieldByName(Location_PlatformDirectionFieldName).AsString;
              IF TempStr <> '' THEN BEGIN
                Location_PlatformOrFiddleyardDirection := StrToDirectionType(TempStr);
                IF Location_PlatformOrFiddleyardDirection = UnknownDirection THEN
                  ErrorMsg := 'invalid direction "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              Location_PlatformOrFiddleyardNumStr := LocationsADOTable.FieldByName(Location_PlatformNumberStringFieldName).AsString;
            END;

            IF ErrorMsg = '' THEN BEGIN
              Location_RecordInLocationOccupationArray := LocationsADOTable.FieldByName(Location_RecordInLocationOccupationArrayFieldName).AsBoolean;
            END;

            IF ErrorMsg = '' THEN BEGIN
              Location_AdjoiningPlatformStr := FieldByName(Location_AdjoiningPlatformFieldName).AsString;
              { Note: we can only check this data after all the locations have been read in }
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Location_DestinationPriorityAreasFieldName).AsString;
              SetLength(TempStrArray, 0);
              IF TempStr <> '' THEN BEGIN
                { We expect a comma as a delimiter }
                ExtractSubStringsFromString(TempStr, ',', TempStrArray);

                I := 0;
                WHILE (I <= High(TempStrArray)) AND (ErrorMsg = '') DO BEGIN
                  IF TempStrArray[I] = 'AllSidings' THEN BEGIN
                    FOR A := 0 TO High(Areas) DO
                      IF Areas[A].Area_Type = Siding THEN
                        AppendToAreaArray(Location_DestinationPriorityAreas, A);
                  END ELSE BEGIN
                    TempArea := StrToArea(TempStrArray[I]);
                    IF TempArea = UnknownArea THEN
                      ErrorMsg := 'unknown area "' + TempStrArray[I] + '" for ' + Location_LongNameStr
                    ELSE
                      AppendToAreaArray(Location_DestinationPriorityAreas, TempArea);
                  END;
                  Inc(I);
                END; {WHILE}
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Location_PlatformPriorityFieldName).AsString;
              IF TempStr = '' THEN
                Location_PlatformPriority := 0
              ELSE
                IF NOT TryStrToInt(TempStr, Location_PlatformPriority) THEN
                  ErrorMsg := 'invalid integer "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Location_TrainPriorityFieldName).AsString;
              IF TempStr = 'Express Only' THEN
                Location_TrainPriority := ExpressOnly
              ELSE
                IF TempStr = 'Express Preferred' THEN
                  Location_TrainPriority := ExpressPreferred
                ELSE
                  IF TempStr = 'Passenger Only' THEN
                    Location_TrainPriority := PassengerOnly
                  ELSE
                    IF TempStr = 'Passenger Preferred' THEN
                      Location_TrainPriority := PassengerPreferred
                    ELSE
                      IF TempStr = '' THEN
                        Location_TrainPriority := NoTrainPriority
                      ELSE
                        ErrorMsg := 'unknown train priority "' + TempStr + '" for ' + Location_LongNameStr;
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Location_DirectionPriorityFieldName).AsString;
              IF TempStr = 'Preferably Up' THEN
                Location_DirectionPriority := PreferablyUp
              ELSE
                IF TempStr = 'Up Only' THEN
                  Location_DirectionPriority := UpOnly
                ELSE
                  IF TempStr = 'Terminating at Up' THEN
                    Location_DirectionPriority := TerminatingAtUp
                  ELSE
                    IF TempStr = 'Preferably Down' THEN
                      Location_DirectionPriority := PreferablyDown
                    ELSE
                      IF TempStr = 'Down Only' THEN
                        Location_DirectionPriority := DownOnly
                      ELSE
                        IF TempStr = 'Terminating at Down' THEN
                          Location_DirectionPriority := TerminatingAtDown
                        ELSE
                          IF TempStr = '' THEN
                            Location_DirectionPriority := NoDirectionPriority
                          ELSE
                            ErrorMsg := 'unknown direction priority "' + TempStr + '" for ' + Location_LongNameStr;
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Location_ThroughOrStoppingPriorityFieldName).AsString;
              IF TempStr = 'Through' THEN
                Location_ThroughOrStoppingPriority := ThroughPriority
              ELSE
                IF TempStr = 'Stopping' THEN
                  Location_ThroughOrStoppingPriority := StoppingPriority
                ELSE
                  IF TempStr = '' THEN
                    Location_ThroughOrStoppingPriority := NoStoppingPriority
                  ELSE
                    ErrorMsg := 'unknown through or stopping priority "' + TempStr + '" for ' + Location_LongNameStr;
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Location_AccessibleLocationsOrAreasUpFieldName).AsString;
              SetLength(Location_AccessibleLocationsOrAreasUpStrArray, 0);
              IF TempStr <> '' THEN
                { Extract the data into a String array for now - we expect a comma as a delimiter }
                ExtractSubStringsFromString(TempStr, ',', Location_AccessibleLocationsOrAreasUpStrArray);
                { Note: we can only check this data after all the locations have been read in }
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(Location_AccessibleLocationsOrAreasDownFieldName).AsString;
              SetLength(Location_AccessibleLocationsOrAreasDownStrArray, 0);
              IF TempStr <> '' THEN
                { Extract the data into a String array for now - we expect a comma as a delimiter }
                ExtractSubStringsFromString(TempStr, ',', Location_AccessibleLocationsOrAreasDownStrArray);
                { Note: we can only check this data after all the locations have been read in }
            END;

            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating Location ' + IntToStr(High(Locations)) + ' (' + Location_LongNameStr + ' ): '
                                            + '[' + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInLocationDataFromDatabase 1');
            END;
          END; {WITH}

          LocationsADOTable.Next;
        END; {WHILE}
      END; {WITH}

      { Tidy up the database }
      LocationsADOTable.Close;
      LocationsADOConnection.Connected := False;
      Log('T Location Data table and connection closed');
    END; {WITH}

    { And now test whether the locations referenced in the entries just read in are valid - this test cannot be carried out until all the locations are read in }
    Location := 0;
    ErrorMsg := '';
    WHILE Location <= High(Locations) DO BEGIN
      WITH Locations[Location] DO BEGIN
        OK := False;
        IF Location_AdjoiningPlatformStr <> '' THEN BEGIN
          I := 0;
          WHILE (I <= High(Locations)) AND NOT OK DO BEGIN
            TestStr1 := UpperCase(RemoveAllSpacesFromAString(Location_AdjoiningPlatformStr));
            TestStr2 := UpperCase(RemoveAllSpacesFromAString(Locations[I].Location_LongNameStr));
            IF TestStr1 = TestStr2 THEN BEGIN
              Location_AdjoiningPlatform := StrToLocation(Location_AdjoiningPlatformStr);
              IF Location_AdjoiningPlatform <> UnknownLocation THEN
                OK := True
              ELSE
                ErrorMsg := 'unknown adjoining platform "' + Location_AdjoiningPlatformStr;
            END;
            Inc(I);
          END; {WHILE}
        END;

        IF ErrorMsg = '' THEN BEGIN
          IF Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr <> '' THEN BEGIN
            I := 0;
            WHILE (I <= High(Locations)) AND NOT OK DO BEGIN
              TestStr1 := UpperCase(RemoveAllSpacesFromAString(Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr));
              TestStr2 := UpperCase(RemoveAllSpacesFromAString(Locations[I].Location_LongNameStr));
              IF TestStr1 = TestStr2 THEN BEGIN
                Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyard := StrToLocation(Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr);
                IF Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyard <> UnknownLocation THEN
                  OK := True
                ELSE
                  ErrorMsg := 'unknown access platform "' + Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr;
              END;
              Inc(I);
            END; {WHILE}
          END;
        END;

        SetLength(LocationExceptions, 0);
        SetLength(LocationExceptionsStrArray, 0);
        IF ErrorMsg = '' THEN BEGIN
          IF Length(Location_AccessibleLocationsOrAreasUpStrArray) > 0 THEN BEGIN
            { Run through the comma delimited list of locations or areas }
            FOR AccessibleLocationsCount := 0 TO High(Location_AccessibleLocationsOrAreasUpStrArray) DO BEGIN
              { Use TempStr to reduce the size of the variable name! }
              TempStr := Location_AccessibleLocationsOrAreasUpStrArray[AccessibleLocationsCount];
              { Is it a location? }
              TempLocation := StrToLocation(TempStr);
              IF TempLocation <> UnknownLocation THEN
                AppendToIntegerArray(Location_AccessibleLocationsUp, TempLocation)
              ELSE BEGIN
                { Or is it an area, perhaps with exceptions? }
                I := Pos('except', TempStr);
                IF I = 0 THEN
                  TempArea := StrToArea(TempStr)
                ELSE BEGIN
                  { extract the exceptions }
                  ExtractSubStringsFromString(TempStr, ' ', LocationExceptionsStrArray);
                  TempArea := StrToArea(LocationExceptionsStrArray[0]);
                  FOR I := 2 TO High(LocationExceptionsStrArray) DO BEGIN
                    TempLocation := StrToLocation(LocationExceptionsStrArray[I]);
                    IF TempLocation = UnknownLocation THEN
                      ErrorMsg := 'unknown excepted location ' + LocationExceptionsStrArray[I] + ' in Accessible Locations Up'
                    ELSE
                      AppendToIntegerArray(LocationExceptions, TempLocation);
                  END;
                END;

                { Locate the locations }
                IF TempArea <> UnknownArea THEN BEGIN
                  FOR NewLocation := 0 TO High(Locations) DO
                    IF (Locations[NewLocation].Location_Area = TempArea) AND NOT IsElementInIntegerArray(LocationExceptions, NewLocation) THEN
                      AppendToIntegerArray(Location_AccessibleLocationsUp, NewLocation);
                END ELSE
                  ErrorMsg := 'unknown accessible Up location ' + Location_AccessibleLocationsOrAreasUpStrArray
                    [AccessibleLocationsCount] + ' in Accessible Locations Or Areas Up';
              END;
            END; {FOR}
          END;
        END;

        SetLength(LocationExceptions, 0);
        SetLength(LocationExceptionsStrArray, 0);
        IF ErrorMsg = '' THEN BEGIN
          IF Length(Location_AccessibleLocationsOrAreasDownStrArray) > 0 THEN BEGIN
            { Run through the comma delimited list of locations or areas }
            FOR AccessibleLocationsCount := 0 TO High(Location_AccessibleLocationsOrAreasDownStrArray) DO BEGIN
              { Use TempStr to reduce the size of the variable name! }
              TempStr := Location_AccessibleLocationsOrAreasDownStrArray[AccessibleLocationsCount];
              { Is it a location? }
              TempLocation := StrToLocation(TempStr);
              IF TempLocation <> UnknownLocation THEN
                AppendToIntegerArray(Location_AccessibleLocationsDown, TempLocation)
              ELSE BEGIN
                { Or is it an area, perhaps with exceptions? }
                I := Pos('except', TempStr);
                IF I = 0 THEN
                  TempArea := StrToArea(TempStr)
                ELSE BEGIN
                  { extract the exceptions }
                  ExtractSubStringsFromString(TempStr, ' ', LocationExceptionsStrArray);
                  TempArea := StrToArea(LocationExceptionsStrArray[0]);
                  FOR I := 2 TO High(LocationExceptionsStrArray) DO BEGIN
                    TempLocation := StrToLocation(LocationExceptionsStrArray[I]);
                    IF TempLocation = UnknownLocation THEN
                      ErrorMsg := 'unknown excepted location ' + LocationExceptionsStrArray[I] + ' in Accessible Locations Down'
                    ELSE
                      AppendToIntegerArray(LocationExceptions, TempLocation);
                  END;
                END;

                { Locate the locations }
                IF TempArea <> UnknownArea THEN BEGIN
                  FOR NewLocation := 0 TO High(Locations) DO
                    IF (Locations[NewLocation].Location_Area = TempArea) AND NOT IsElementInIntegerArray(LocationExceptions, NewLocation) THEN
                      AppendToIntegerArray(Location_AccessibleLocationsDown, NewLocation);
                END ELSE
                  ErrorMsg := 'unknown accessible down location ' + Location_AccessibleLocationsOrAreasDownStrArray
                    [AccessibleLocationsCount] + ' in Accessible Locations Or Areas Down';
              END;
            END; {FOR}
          END;
        END;
      END; {WITH}
      Inc(Location);
    END; {WHILE}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInLocationDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadInLocationDataFromDatabase }

PROCEDURE WriteOutLocationDataToDatabase;
{ Write out some location data to the location data file }
VAR
  Location : Integer;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
// IF SystemOffline THEN BEGIN
// ShowMessage('System offline so not writing out location data');
// Log('X System offline so not writing out location data');
// END ELSE
      BEGIN
        IF NOT FileExists(PathToRailDataFiles + LocationDataFilename + '.' + LocationDataFilenameSuffix) THEN BEGIN
          IF MessageDialogueWithDefault('Location database file "' + PathToRailDataFiles + LocationDataFilename + '.' + LocationDataFilenameSuffix + '" cannot be located'
                                        + CRLF
                                        + 'Do you wish to continue?',
                                        StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
          THEN
            ShutDownProgram(UnitRef, 'WriteOutLocationDataFromDatabase')
          ELSE
            Exit;
        END;

        LocationsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                   + PathToRailDataFiles + LocationDataFilename + '.' + LocationDataFilenameSuffix
                                                   + ';Persist Security Info=False';
        LocationsADOConnection.Connected := True;
        LocationsADOTable.Open;
        Log('T Location data table and connection opened to write out some location data');

        LocationsADOTable.First;
        WHILE NOT LocationsADOTable.EOF DO BEGIN
          WITH LocationsADOTable DO BEGIN
            Location := 0;
            WHILE Location <= High(Locations) DO BEGIN
              WITH Locations[Location] DO BEGIN
                IF FieldByName(Location_NameStrFieldName).AsString = Location_LongNameStr THEN BEGIN
                  IF Location_OutOfUse <> FieldByName(Line_OutOfUseFieldName).AsBoolean THEN BEGIN
                    Edit;
                    FieldByName(Line_OutOfUseFieldName).AsBoolean := Location_OutOfUse;
                    Post;
                  END;
                END; {WITH}
              END;
              Inc(Location);
            END;
          END; {WITH}
          LocationsADOTable.Next;
        END; {WHILE}

        { Tidy up the database }
        LocationsADOTable.Close;
        LocationsADOConnection.Connected := False;
        Log('L Location Data table and connection closed after writing locations');
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutLocationDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutLocationDataToDatabase }

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

PROCEDURE CalculateTCAdjacentBufferStops;
{ Work out which track circuits are next a buffer stop }
VAR
  L : Integer;
  TC : Integer;
  TCLocationFound : Boolean;

BEGIN
  FOR TC := 0 TO High(TrackCircuits) DO BEGIN
    TCLocationFound := False;
    L := 0;
    WHILE (L <= High(Lines)) AND NOT TCLocationFound DO BEGIN
      IF Lines[L].Line_TC = TC THEN BEGIN
        TCLocationFound := True;
        AppendToLineArray(TrackCircuits[TC].TC_LineArray, L);
        TrackCircuits[TC].TC_Location := Lines[L].Line_Location;

        { also initialise the buffer stop data }
        TrackCircuits[TC].TC_AdjacentBufferStop := Lines[L].Line_AdjacentBufferStop;

        { and the gradient - give the track circuit the benefit of the doubt so that if any line comprising it is not level, that gradient is the one recorded as being at
          the track circuit
        }
        IF Lines[L].Line_Gradient <> Level THEN
          TrackCircuits[TC].TC_Gradient := Lines[L].Line_Gradient;
      END;
      Inc(L);
    END; {WHILE}
  END; {FOR}
END; { CalculateTCAdjacentBufferStops }

PROCEDURE CalculateTCAdjacentSignals;
{ Work out which track circuits are next the signal and/or a bufferstop }
VAR
  S : Integer;
  TC : Integer;

BEGIN
  TRY
    FOR TC := 0 TO High(TrackCircuits) DO
      SetLength(TrackCircuits[TC].TC_AdjacentSignals, 0);

    FOR S := 0 TO High(Signals) DO
      IF Signals[S].Signal_AdjacentLine <> UnknownLine THEN
        AppendToIntegerArray(TrackCircuits[Lines[Signals[S].Signal_AdjacentLine].Line_TC].TC_AdjacentSignals, S);
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CalculateTCAdjacentSignals: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CalculateTCAdjacentSignals }

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

PROCEDURE ReadInFeedbackDataFromDatabase;
{ Initialise the feedback unit data }
CONST
  StopTimer = True;

VAR
  ErrorMsg : String;
  FieldName : String;
  FeedbackUnit : Integer;
  FirstFeedbackUnitFound : Boolean;
  Input : Integer;
  TempStr : String;

BEGIN
  TRY
    Log('A INITIALISING FEEDBACK UNIT DATA {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + FeedbackDataFilename + '.' + FeedbackDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Feedback database file "' + PathToRailDataFiles + FeedbackDataFilename + '.' + FeedbackDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInFeedbackDataFromDatabase')
        ELSE
          Exit;
      END;

      FeedbackUnitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                        + PathToRailDataFiles + FeedbackDataFilename + '.' + FeedbackDataFilenameSuffix
                                                        + ';Persist Security Info=False';
      FeedbackUnitsADOConnection.Connected := True;
      FeedbackUnitsADOTable.Open;
      Log('T Feedback data table and connection opened to initialise the feedback unit data');

      FeedbackUnitsADOTable.Sort := '[Unit] ASC';
      FeedbackUnitsADOTable.First;
      SetLength(FeedbackUnitRecords, 0);

      FirstFeedbackUnit := 99999;
      LastFeedbackUnit := -1;

      FirstFeedbackUnitFound := False;

      WHILE NOT FeedbackUnitsADOTable.EOF DO BEGIN
        WITH FeedbackUnitsADOTable DO BEGIN
          ErrorMsg := '';

          FieldName := 'Unit';
          FeedbackUnit := FieldByName(FieldName).AsInteger;
          IF NOT FirstFeedbackUnitFound THEN BEGIN
            { work out where the real start of the dynamic array is }
            FirstFeedBackUnit := FeedbackUnit;
            FirstFeedbackUnitFound := True;
          END;
          SetLength(FeedbackUnitRecords, FeedbackUnit + 1);
          LastFeedBackUnit := High(FeedbackUnitRecords);

          WITH FeedbackUnitRecords[High(FeedbackUnitRecords)] DO BEGIN
            Feedback_DetectorOutOfUse := False;

            FOR Input := 1 TO 8 DO BEGIN
              Feedback_InputLine[Input] := UnknownLine;
              Feedback_InputPoint[Input] := UnknownPoint;
              Feedback_InputTrackCircuit[Input] := UnknownTrackCircuit;
              Feedback_InputTRSPlunger[Input] := UnknownTRSPlunger;
            END; {FOR}
          END; {WHILE}

          FieldName := 'Input1Type';
          IF FieldByName(FieldName).AsString = '' THEN
            ErrorMsg := 'missing feedback type'
          ELSE BEGIN
            WITH Feedbackunitrecords[high(Feedbackunitrecords)] DO BEGIN
              Feedback_InputTypeArray[1] := StrToFeedbackType(FieldByName(FieldName).AsString);
              IF Feedback_InputTypeArray[1] = UnknownFeedbackType THEN
                ErrorMsg := 'unknown feedback type';
            END; {with}
          END;

          IF ErrorMsg = '' THEN BEGIN
            { propagate the other input types if only the first is occupied }
            Input := 2;
            WHILE (Input <= 8) AND (ErrorMsg = '') DO BEGIN
              FieldName := 'Input' + IntToStr(Input) + 'Type';
              WITH Feedbackunitrecords[high(Feedbackunitrecords)] DO
                IF FieldByName(FieldName).AsString = '' THEN
                  { propagate the other input types if only the first is occupied }
                  Feedback_InputTypeArray[Input] := Feedback_InputTypeArray[1]
                ELSE BEGIN
                  Feedback_InputTypeArray[Input] := StrToFeedbackType(FieldByName(FieldName).AsString);
                  IF Feedback_InputTypeArray[Input] = UnknownFeedbackType THEN
                    ErrorMsg := 'unknown feedback type "' + FieldByName(FieldName).AsString + '" for Input' + IntToStr(Input) + 'Type';
                END;
              Inc(Input);
            END; {WHILE}
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'TCAbove';
            TempStr := FieldByName(FieldName).AsString;
            IF TempStr = '' THEN
              Feedbackunitrecords[high(Feedbackunitrecords)].Feedback_TCAboveUnit := UnknownTrackCircuit
            ELSE
              IF NOT TryStrToInt(TempStr, Feedbackunitrecords[high(Feedbackunitrecords)].Feedback_TCAboveUnit) THEN
                ErrorMsg := 'invalid integer ''' + TempStr + ''' in TCAbove field';
          END;

          IF ErrorMsg <> '' THEN BEGIN
            IF MessageDialogueWithDefault('Error in creating Feedback Detector=' + IntToStr(high(Feedbackunitrecords)) + ': '
                                          + '[' + ErrorMsg + ']:'
                                          + CRLF
                                          + 'Do you wish to continue?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
            THEN
              ShutDownProgram(UnitRef, 'InitialiseFeedback');
          END;
        END; {WITH}
        FeedbackUnitsADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      FeedbackUnitsADOTable.Close;
      FeedbackUnitsADOConnection.Connected := False;
      Log('T Feedback unit data table and connection closed');

      Log('T Reading in feedback data from unit ' + IntToStr(FirstFeedbackUnit) + ' to unit ' + IntToStr(LastFeedbackUnit) + ' from database');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInFeedbackDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadInFeedbackDataFromDatabase }

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
    SetMode(LogsCurrentlyKept, False);
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

PROCEDURE SaveScreenDrawingVariables;
{ Save the screen drawing variables }
BEGIN
  WITH RailWindowBitmap.Canvas DO BEGIN
    SaveBrushColour := Brush.Color;
    SaveBrushStyle := Brush.Style;
    SaveFontColour := Font.Color;
    SaveFontHeight := Font.Height;
    SaveFontName := Font.Name;
    SaveFontStyle := Font.Style;
    SavePenColour := Pen.Color;
    SavePenMode := Pen.Mode;
    SavePenStyle := Pen.Style;
    SavePenWidth := Pen.Width;
  END; {WITH}
END; { SaveScreenDrawingVariables }

PROCEDURE RestoreScreenDrawingVariables;
{ Restore the default screen drawing variables }
BEGIN
  WITH RailWindowBitmap.Canvas DO BEGIN
    Brush.Color := SaveBrushColour;
    Brush.Style := SaveBrushStyle;
    Font.Color := SaveFontColour;
    Font.Height := SaveFontHeight;
    Font.Name := SaveFontName;
    Font.Style := SaveFontStyle;
    Pen.Color := SavePenColour;
    Pen.Mode := SavePenMode;
    Pen.Style := SavePenStyle;
    Pen.Width := SavePenWidth;
  END; {WITH}
END; { RestoreScreenDrawingVariables }

PROCEDURE InitialiseScreenDrawingVariables;
{ Set up the default screen drawing variables }
BEGIN
  WITH RailWindowBitmap.Canvas DO BEGIN
    Brush.Color := BackgroundColour;
    Brush.Style := bsSolid;
    Font.Color := ForegroundColour;
    Font.Height := -MulDiv(FWPRailWindow.ClientHeight, 11, 1000);
    Font.Name := RailFontName;
    Font.Style := [];
    Pen.Color := ForegroundColour;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Pen.Width := 1;
  END; {WITH}
END; { InitialiseScreenDrawingVariables }

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
