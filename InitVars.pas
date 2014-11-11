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
  CopyrightStatementForDisplaying = 'Copyright © F.W. Pritchard && A.M. Stokes 1988-2014';
  CopyrightStatementForWritingToFile = 'Copyright © F.W. Pritchard & A.M. Stokes 1988-2014';

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
  clFWPDkBlue = $0802040; { 0 128  32  64 }
  clFWPOrange = $000080FF; { 0   0 128 255 }
  clFWPLtBrown = $004080FF; { 0  64 128 255 }
  clFWPDkBrown = $00004080; { 0   0  64 128 }
  clFWPPink = $008080FF; { 0 128 128 255 }
  clFWPPlatformColour = $0038ABB1;
  clFWPDkGrey = $00242424; { 0 36 36 36 }
  clFWPVeryDkGrey = $00212121; { 0  33  33  33 }

  clFWPDkBlueStr = 'clFWPDkBlue';
  clFWPOrangeStr = 'clFWPOrange';
  clFWPLtBrownStr = 'clFWPLtBrown';
  clFWPDkBrownStr = 'clFWPDkBrown';
  clFWPPinkStr = 'clFWPPink';
  clFWPPlatformColourStr = 'clFWPPlatformColour';
  clFWPDkGreyStr = 'clFWPDkGrey';
  clFWPVeryDkGreyStr = 'clFWPVeryDkGrey';

  FirstFunctionDecoder = 9001;
  LastFunctionDecoder = 9916;

  FirstFeedbackUnit = 66; { could be 65, but if a feedback unit resets itself, it reverts to 65, and thus causes an error }
  LastFeedbackUnit = 126; { ************** replace now feedback data is read in from the database }

  FirstRouteCreationHeldMsgNumber = 1;
  LastRouteCreationHeldMsgNumber = 12;

  ActiveTrain = True;
  Bold = True;
  ByUser = True;
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
  ModeType = (AllRouteDebugging, AnonymousOccupation, GeneralDebugging, LineDebugging, LockDebugging, Locking, PointDebugging, PreviousPointSettings, RDC,
              RecordingMonitorScreens, RecordLineDrawing, RouteDebugging, RouteBacktrackDebugging, RouteDrawing, ShowAdjacentTrackCircuit, StationStart, Testing);

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
  TypeOfFeedbackType = (TrackCircuitFeedbackDetector, TRSPlungerFeedbackDetector, PointFeedbackDetector, LineFeedbackDetector, MixedFeedbackDetectors,
                        FeedbackDetectorOutOfUse, UnknownFeedbackDetectorType);

  FeedbackRec = RECORD
    Feedback_Input : Integer;
    Feedback_Input1Type : TypeOfFeedbackType;
    Feedback_InputOn : Boolean;
    Feedback_InputTypeArray : ARRAY [1..8] OF TypeOfFeedbackType;
    Feedback_TCAboveUnit : Integer;
    Feedback_Type : TypeOfFeedbackType;
    Feedback_Unit : Byte;
  END;

  MenuPopupTypes = (NoClickPopupType,
                    SignalChangeDirectionPopupType, SignalDeletePopupType, SignalEditPopupType, SignalOutOfUsePopupType, SignalUndoChangesPopupType,
                    PointDeletePopupType, PointEditPopupType, PointOutOfUsePopupType, PointToManualPopupType, PointUnlockPopupType,
                    BufferStopEditPopupType,
                    LineAllocateLocoToTrackCircuitPopupType, LineChangeInternalLocoDirectionToDownPopupType, LineChangeInternalLocoDirectionToUpPopupType,
                    LineCreateDownSignalPopupType, LineCreateUpSignalPopupType, LineDeleteLinePopupType, LineCreatePointPopupType, LineEnterCreateLinePopupType,
                    LineExitCreateLinePopupType, LineEditPopupType, LineLocationOutOfUsePopupType, LineOutOfUsePopupType, LineShowLocoLastErrorMessagePopupType,
                    LineTCFeedbackOccupationPopupType, LineTCOutOfUsePopupType, LineTCPermanentOccupationPopupType, LineTCSpeedRestrictionPopupType,
                    LineTCSystemOccupationPopupType, LineTCUnoccupiedPopupType, LineTCUserMustDrivePopupType, LineAllocateTrackCircuitPopupType,
                    LineRemoveTrackCircuitPopupType);

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
    TRSPlunger_Locked : Boolean;
    TRSPlunger_MouseRect : TRect;
    TRSPlunger_Num : Integer;
    TRSPlunger_PlatformNumStr : String;
    TRSPlunger_Present : Boolean;
    TRSPlunger_Pressed : Boolean;
    TRSPlunger_Triangle : TRSPlungerTriangleType;
    TRSPlunger_WaitAfterPlungerPressedInSeconds : Cardinal;
  END;

  { Point-related type declarations }
  TypeOfPoint = (OrdinaryPoint, CrossOverPoint, ThreeWayPointA, ThreeWayPointB, SingleSlip, DoubleSlip, ProtectedPoint, CatchPointUp, CatchPointDown, PointTypeUnknown);

  PointRec = RECORD
    Point_AwaitingManualChange : Boolean;
    Point_DataChanged : Boolean;
    Point_DefaultState : PointStateType;
    Point_DivergingLine : Integer;
    Point_Energised : Boolean;
    Point_EnergisedTime : TDateTime;
    Point_FacingDirection : DirectionType;
    Point_FarX : Word; { Position of other ends }
    Point_FarY : Word; { Position of other ends }
    Point_FeedbackOnIsStraight : Boolean;
    Point_FeedbackPending : Boolean;
    Point_FeedbackPendingMsgWritten : Boolean;
    Point_FeedbackStartTime : TDateTime;
    Point_FeedbackUnit : Integer;
    Point_FeedbackInput : Integer;
    Point_ForcedDelayMsg1Written : Boolean;
    Point_ForcedDelayMsg2Written : Boolean;
    Point_HasFeedback : Boolean;
    Point_HeelLine : Integer;
    Point_LastChangedTime : TDateTime;
    Point_LastFeedbackStateAsReadIn : PointStateType;
    Point_LastManualStateAsReadIn : PointStateType;
    Point_LenzNum : Integer;
    Point_LenzUnit : Integer;
    Point_LenzUnitType : String;
    Point_LockedByUser : Boolean;
    Point_LockedIfHeelTCOccupied : Boolean;
    Point_LockedIfNonHeelTCsOccupied : Boolean;
    Point_LockFailureNotedInLocksUnit : Boolean;
    Point_LockFailureNotedInSubRouteUnit : Boolean;
    Point_LockingArray : StringArrayType;
    Point_LockingState : PointStateType; { used to detect points that have moved while locked }
    Point_LocoChipLockingTheRoute : Integer;
    Point_ManualOperation : Boolean;
    Point_MaybeBeingSetToManual : Boolean;
    Point_MouseRect : TRect; { mouse access rectangle }
    Point_MovedWhenLocked : Boolean;
    Point_Notes : String;
    Point_Number : Integer;
    Point_OutOfUse : Boolean;
    Point_PresentState : PointStateType;
    Point_PreviousState : PointStateType;
    Point_RelatedPoint : Integer;
    Point_RequiredState : PointStateType;
    Point_ResettingTime : TDateTime;
    Point_RouteLockedByLocoChip : Integer;
    Point_StraightLine : Integer;
    Point_TCAtHeel : Integer;
    Point_Type : TypeOfPoint;
    Point_SecondAttempt : Boolean;
    Point_SetASecondTime : Boolean;
    Point_WaitTime : TDateTime;
    Point_WiringReversedFlag : Boolean;
    Point_X : Word; { position of common point }
    Point_Y : Word; { position of common point }
  END;

CONST
  Point_DefaultStateFieldName : String = 'Default State';
  Point_DivergingLineFieldName : String = 'Diverging Line';
  Point_FeedbackInputFieldName : String = 'Feedback Input';
  Point_FeedbackOnIsStraightFieldName : String = 'On Is Straight';
  Point_FeedbackUnitFieldName : String = 'Feedback Unit';
  Point_HeelLineFieldName : String = 'Heel Line';
  Point_LastFeedbackStateAsReadInFieldName : String = 'Last Feedback State';
  Point_LastManualStateAsReadInFieldName : String = 'Last Manual State';
  Point_LenzNumFieldName : String = 'Lenz Point Number';
  Point_LenzUnitFieldName : String = 'Lenz Point Unit';
  Point_LenzUnitTypeFieldName : String = 'Lenz Point Unit Type';
  Point_LockedByUserFieldName : String = 'Locked By User';
  Point_LockedIfHeelTCOccupiedFieldName : String = 'Locked If Heel TC Occupied';
  Point_LockedIfNonHeelTCsOccupiedFieldName : String = 'Locked If Non-Heel TCs Occupied';
  Point_ManualOperationFieldName : String = 'Manual Operation';
  Point_NotesFieldName : String = 'Notes';
  Point_NumberFieldName : String = 'Point Number';
  Point_OutOfUseFieldName : String = 'Out Of Use';
  Point_RelatedPointFieldName : String = 'Related Point';
  Point_StraightLineFieldName : String = 'Straight Line';
  Point_TypeFieldName : String = 'Point Type';
  Point_WiringReversedFlagFieldName : String = 'Wiring Reversed';

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

  { Trackcircuit-related type declarations }
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
    TC_UserMustDrive : Boolean;
  END;

CONST
  TC_NumberFieldName : String = 'TCNum';
  TC_LengthFieldName : String = 'Length';
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
  TRSPlungerFeedbackDetectorStr = 'TRS Plunger';
  UnknownFeedbackDetectorStr = 'Unknown';

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
  AllRouteDebuggingMode : Boolean = False;
  AllSignalsSwitchedOff : Boolean = False;
  AnonymousOccupationMode : Boolean = True;
  Areas : ARRAY OF AreaRec;
  AutoModeInitiated : Boolean = False;
  BreakPointRequiredInMakeSoundRoutine : Boolean = False;
  BufferStops : ARRAY OF BufferStopRec;
  CreateLineMode : Boolean = False;
  CrossHairCursor : TCursor;
  DapolCleaningWagonLocoChip : Integer = UnknownLocoChip;
  DapolCleaningWagonLocoChipRunning : Boolean = False;
  DayTimeSetByUser : Boolean = False;
  DebuggingMode : Boolean = False;
  DesiredLenzConnection : LenzConnectionType = NoConnection;
  DiagramsArray : IntegerArrayType;
  DisplayFeedbackStringsInDebugWindow : Boolean = False;
  EditIcon : TIcon;
  EditMode : Boolean = False;
  EmergencyStopMsgDisplayed : Boolean = False;
  EscKeyStored : Boolean = False;
  FeedbackDebuggingMode : Boolean = False;
  FeedbackUnitData : ARRAY OF FeedbackRec;
  FeedbackUnitInUseArray : ARRAY [FirstFeedbackUnit..LastFeedbackUnit] OF Boolean;
  FullScreenPenWidth : Integer = 5;
  FWPRailWindowInitialised : Boolean = False;
  FWPRailWindowPartInitialised : Boolean = False;
  GridInterLineSpacing : Integer = 0;
  InAutoMode : Boolean = False;
  InterLineSpacing : Integer = 0;
  KeyBoardandMouseLocked : Boolean = False;
  LastPointChanged : Integer = UnknownPoint;
  LastTimeAnyPointChanged : TDateTime = 0;
  LenzConnection : LenzConnectionType = NoConnection;
  LightsToBeSwitchedOnArray : ARRAY OF LightsToBeSwitchedOnRec;
  LineDebuggingMode : Boolean = False;
  LineHighlighted : Integer = UnknownLine;
  Lines : ARRAY OF LineRec;
  LinesInitialised : Boolean = False;
  Locations : ARRAY OF LocationRec;
  LockDebuggingMode : Boolean = False;
  LockingMode : Boolean = True;
  LocationOccupations : ARRAY OF ARRAY OF LocationOccupationRec;
  Locos : ARRAY OF LocoRec;
  LocoSpeedTimingMode : Boolean = False;
  LocosStopped : Boolean = True;
  LogsCurrentlyKept : Boolean; { initialised by LogsKeptMode - allows LogsKeptMode to be changed without affecting the current logging }
  MainPlatformPlungers : ARRAY OF TRSPlungerRec;
  MissingTrainArray : ARRAY [1..9] OF Boolean = (False, False, False, False, False, False, False, False, False);
  MissingTrainCounter : Integer = 0;
  NightTimeSetByUser : Boolean = False;
  NoFeedBackList : StringArrayType;
  NumberLines : Boolean = False;
  OfflineIcon : TIcon;
  OnlineIcon : TIcon;
  Platforms : ARRAY OF PlatformRec;
  PointDebuggingMode : Boolean = False;
  PointHighlighted : Integer = UnknownPoint;
  PointResettingMode : Boolean = True;
  PointResettingToDefaultStateArray : IntegerArrayType;
  Points : ARRAY OF PointRec;
  PostEmergencyTime : TDateTime = 0;
  PostEmergencyTimeSet : Boolean = False;
  PreviousPointSettingsMode : Boolean = False;
  ProgramShuttingDown : Boolean = False;
  ProgramStartup : Boolean = True;
  RailWindowBitmapCanvasPenWidth : Integer;
  RailDriverCalibrated : Boolean = False;
  RailDriverCalibrationStarted : Boolean = False;
  RailDriverInitialised : Boolean = False;
  RDCMode : Boolean = False;
  ReadOutAdjacentSignalNumber : Boolean = False;
  ReadOutDecoderNumber : Boolean = False;
  ReadOutTCInFull : Boolean = False;
  ReadOutTCOnce : Boolean = False;
  RecordLineDrawingMode : Boolean = False;
  RecordingMonitorScreensMode : Boolean = False;
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
  RouteBacktrackDebuggingMode : Boolean = False;
  RouteClearingOnlyMode : Boolean = False;
  RouteDebuggingMode : Boolean = False;
  RouteDrawingMode : Boolean = False;
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
  TestingMode : Boolean = False;
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

  ShowAdjacentTrackCircuitMode : Boolean = False;
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
  FunctionDecoderBytes : ARRAY [FirstFunctionDecoder..LastFunctionDecoder] OF Byte;

  AspectRatio : Real; { Y/X ratio for screen }

  HoursUpX, HoursStartY, HoursDownX, HoursEndY : Word;
  MinutesUpX, MinutesStartY, MinutesDownX, MinutesEndY : Word;
  SecondsUpX, SecondsStartY, SecondsDownX, SecondsEndY : Word;

PROCEDURE AddLineToStationMonitorsWebDiagnosticsMemo(S : String);
{ Adds a line of text to the station monitor unit's web diagnostics memo }

PROCEDURE AddNewRecordToLineDatabase;
{ Append a record to the line database }

PROCEDURE AddNewRecordToPointDatabase;
{ Append a record to the point database }

PROCEDURE AddNewRecordToSignalDatabase;
{ Append a record to the signal database }

PROCEDURE CalculateLinePolygons(Line : Integer);
{ Work out the position for the various line polygons }

PROCEDURE CalculateLinePositions;
{ Work out where the lines are on the screen }

PROCEDURE CalculatePlatformPositions;
{ Create the platform rectangle }

PROCEDURE CalculatePointPositions;
{ Create where the points are on the screen }

PROCEDURE CalculateTCAdjacentSignals;
{ Work out which track circuits are next the signal }

PROCEDURE CalculateTCAdjacentBufferStops;
{ Work out which track circuits are next a buffer stop }

PROCEDURE CalculateSignalPosition(S : Integer);
{ Work out where a signal is on the screen }

PROCEDURE CalculateAllSignalPositions;
{ Work out where all the signals are on the screen }

FUNCTION DeleteRecordFromLineDatabase(LineToDeleteNum : Integer) : Boolean;
{ Remove a record from the line database }

FUNCTION DeleteRecordFromPointDatabase(PointToDeleteNum : Integer) : Boolean;
{ Remove a record from the point database }

FUNCTION DeleteRecordFromSignalDatabase(SignalToDeleteNum : Integer) : Boolean;
{ Remove a record from the signal database }

FUNCTION DeleteRecordFromTrackCircuitDatabase(TrackCircuitToDeleteNum : Integer) : Boolean;
{ Remove a record from the trackCircuit database }

FUNCTION DescribeActualDateAndTime : String;
{ Return the current real date and time as a String }

FUNCTION GetLineAdjacentSignal(Line : Integer) : Integer;
{ Return the signal nearest the line }

PROCEDURE InitialiseInitVarsUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

PROCEDURE InitialiseLogFiles;
{ Open a new file for test output - rename two previous ones if they exist }

PROCEDURE InitialiseScreenDrawingVariables;
{ Set up the default screen drawing variables }

PROCEDURE ReadInAreasDataFromDatabase;
{ Initialise the area data }

PROCEDURE ReadInFeedbackDataFromDatabase;
{ Initialise the feedback unit data }

PROCEDURE ReadInLineDataFromDatabase;
{ Initialise the data for each of the line segments }

PROCEDURE ReadInLocationDataFromDatabase;
{ Initialise the location data }

PROCEDURE ReadInPlatformDataFromDatabase;
{ Initialise the platform data }

PROCEDURE ReadInPointDataFromDatabase;
{ Create all the points }

PROCEDURE ReadInSignalDataFromDatabase(NewSignalData : Boolean);
{ Create entries for the signals }

PROCEDURE ReadInTrackCircuitDataFromDatabase;
{ Initialise the track circuit data which depends on lines being initialised first }

PROCEDURE RestoreScreenDrawingVariables;
{ Restore the default screen drawing variables }

PROCEDURE SaveScreenDrawingVariables;
{ Save the screen drawing variables }

PROCEDURE SetUpLineDrawingVars;
{ Set up the positions of the lines and platforms }

FUNCTION ValidateBufferStopNumber(BufferStopStr : String; OUT ErrorMsg : String) : Integer;
{ See if there's a valid number (or nothing) }

FUNCTION ValidateLineConnectionCh(LineConnectionCh : String; OUT ErrorMsg : String) : String;
{ See if the connection char exceeds one character }

FUNCTION ValidateLineDirection(LineDirectionStr : String; OUT ErrorMsg : String) : DirectionType;
{ Check the direction is correct }

FUNCTION ValidateLineEndOfLineMarker(EndOfLineMarkerStr : String; OUT ErrorMsg : String) : EndOfLineType;
{ See if the supplied end of line string is correct }

FUNCTION ValidateLineGradient(LineGradientStr : String; OUT ErrorMsg : String) : GradientType;
{ Checks the line's gradient is valid }

FUNCTION ValidateLineInUseFeedbackUnit(FeedbackUnitStr : String; OUT ErrorMsg : String) : Integer;
{ Only checks that the supplied data is numeric }

FUNCTION ValidateLineLocation(LineLocationStr : String; OUT ErrorMsg : String) : Integer;
{ Checks whether the line location is valid }

FUNCTION ValidateLineName(Str : String; Line : Integer; OUT ErrorMsg : String) : String;
{ Validates whether the new line name matches an existing one }

FUNCTION ValidateLineTrackCircuit(LineTCStr : String; OUT ErrorMsg : String) : Integer;
{ Checks the validity of the supplied line track circuit }

FUNCTION ValidateLineType(LineTypeStr : String; OUT ErrorMsg : String) : TypeOfLine;
{ See if the type of line is correct }

FUNCTION ValidateGridX(Str : String; OUT ErrorMsg : String) : Integer;
{ Converts grid string to number }

FUNCTION ValidateNextSignalIfNoIndicator(Str : String; Init : Boolean; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns what the other signal is if no indicator is lit }

FUNCTION ValidatePointDefaultState(NewStateStr : String; HeelLine, StraightLine, DivergingLine : Integer; OUT PresentState : PointStateType;
                                   OUT ErrorMsg : String) : PointStateType;
{ Check whether the point state supplied is valid }

FUNCTION ValidatePointDivergingLineName(LineName : String; PointType : TypeOfPoint; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Diverging Line name is valid }

FUNCTION ValidatePointFeedbackInput(FeedbackInputStr : String; PointHasFeedback : Boolean; OUT ErrorMsg : String) : Integer;
{ Check whether the point feedback input number is valid }

FUNCTION ValidatePointFeedbackUnit(FeedbackUnitStr : String; OUT PointHasFeedback : Boolean; OUT ErrorMsg : String) : Integer;
{ Check whether the feedback unit exists and is valid }

FUNCTION ValidatePointHeelLineName(LineName : String; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Heel Line name is valid }

FUNCTION ValidatePointLastFeedbackStateAsReadIn(PointStateStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : PointStateType;
{ Check whether the last point state read in is valid }

FUNCTION ValidatePointLastManualStateAsReadIn(PointStateStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : PointStateType;
{ Check whether the last point state read in is valid }

FUNCTION ValidatePointLenzNum(LenzNumStr : String; PointLastManualStateAsReadIn : PointStateType; OUT PointManualOperation : Boolean;
                              OUT PointPresentState : PointStateType; OUT ErrorMsg : String) : Integer;
{ Check whether the Lenz point number is valid }

FUNCTION ValidatePointLenzUnit(LenzUnitStr : String; OUT ErrorMsg : String) : Integer;
{ Check whether a Lenz point unit is valid }

FUNCTION ValidatePointLenzUnitType(LenzUnitTypeStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : String;
{ Check whether a Lenz point unit type is valid }

FUNCTION ValidatePointRelatedPoint(P : Integer; RelatedPointStr : String; PointType : TypeOfPoint; OUT ErrorMsg : String) : Integer;
{ Check whether the value of the connected point (if any) is valid }

FUNCTION ValidatePointStraightLineName(LineName : String; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Straight Line name is valid }

FUNCTION ValidatePointType(PointTypeStr : String; OUT ErrorMsg : String) : TypeOfPoint;
{ Check that the supplied point type is valid }

FUNCTION ValidateRow(RowStr : String; OUT ErrorMsg : String) : Extended;
{ See whether the row provided is valid }

FUNCTION ValidateSignalAccessoryAddress(Str : String; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder address for a TrainTech S3 accessory decoder. This test must be done after Signal Type is validated. }

FUNCTION ValidateSignalAdjacentLine(SignalBeingValidated : Integer; Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the line next to a signal }

FUNCTION ValidateSignalAdjacentLineXOffset(Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns whether a signal's on screen adjustment if any }

FUNCTION ValidateSignalApproachControlAspect(Str : String; OUT ErrorMsg : String) : AspectType;
{ Validates and if ok returns the approach control signal aspect }

FUNCTION ValidateSignalAsTheatreDestination(Str : String; OUT ErrorMsg : String) : String;
{ Validates and if ok returns the one or two character display used in a theatre indicator }

FUNCTION ValidateSignalDecoderNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for a signal decoder. This check must be done after Accessory Address and Signal Type are validated. }

FUNCTION ValidateSignalDirection(Str : String; OUT ErrorMsg : String) : DirectionType;
{ Validates and if ok returns the signal direction }

FUNCTION ValidateSignalIndicator(Str : String; OUT ErrorMsg : String) : IndicatorType;
{ Validates and if ok returns the signal indicator }

FUNCTION ValidateSignalIndicatorDecoderFunctionNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for an indicator function decoder. This check must be done after Accessory Address and Signal Type are validated. }

FUNCTION ValidateSignalIndicatorDecoderNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for an indicator decoder. This check must be done after Accessory Address and Signal Type are validated. }

FUNCTION ValidateSignalDistantHomesArray(S : Integer; Signal_Type : TypeOfSignal; Str : String; OUT ErrorMsg : String) : IntegerArrayType;
{ Validates the home signal numbers supplied }

FUNCTION ValidateSignalIndicatorSpeedRestriction(Str : String; Indicator : IndicatorType; OUT ErrorMsg : String) : MPHType;
{ Validates and if ok returns what the tempoarary speed restriction is. This test must be carried outr after Indicator is validated. }

FUNCTION ValidateSignalJunctionIndicators1(Str, FieldName : String; Signal_Indicator : IndicatorType; OUT ErrorMsg : String) : JunctionIndicatorRec;
{ The first part of verifying whether junction indicators are correctly set up; this part also returns the values for each junction indicator. This test requires that
  the indicator has been validated first
}
PROCEDURE ValidateSignalJunctionIndicators2(StrArray : ARRAY OF String; SignalIndicator : IndicatorType; SignalJunctionIndicators : ARRAY OF JunctionIndicatorRec;
                                            OUT ErrorMsg : String);
{ The second part of verifying whether junction indictaors are correctly set up }

FUNCTION ValidateSignalLocationsToMonitorArray(Str : String; PossibleRouteHold : Boolean; OUT ErrorMsg : String) : IntegerArrayType;
{ Validates and if ok returns the signal locations monitored when a route is held. This test must be done after Possible Route Hold is validated. }

FUNCTION ValidateSignalNum(SignalToTest : Integer) : String;
{ Validates a signal number. This has to be carried out separately from other validation, as when creating signals a reference may be made to a signal not yet created }

FUNCTION ValidateSignalOppositePassingLoopSignal(Str : String; Init : Boolean; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the other signal involved in a passing loop }

FUNCTION ValidateSignalOutOfUseAndAddAdjacentTC(Flag : Boolean; AdjacentLine : Integer; OUT AdjacentTC : Integer; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns true if a signal is marked as not in use. This test must be done after Adjacent Signal is validated. }

FUNCTION ValidateSignalPossibleStationStartRouteHold(Flag : Boolean; PossibleRouteHold : Boolean; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns whether a station start route hold is in operation. This test must be done after Possible Route Hold is validated. }

FUNCTION ValidateSignalQuadrant(Str : String; OUT ErrorMsg : String) : QuadrantType;
{ Validates and if ok returns the quadrant type }

FUNCTION ValidateSignalType(Str : String; Quadrant : QuadrantType; DistantHomesArray : IntegerArrayType; OUT ErrorMsg : String) : TypeOfSignal;
{ Validates and if ok returns the signal type }

PROCEDURE WriteOutLineDataToDatabase;
{ Write out some line data to the line data file }

PROCEDURE WriteOutLocationDataToDatabase;
{ Write out some location data to the location data file }

PROCEDURE WriteOutPointDataToDatabase;
{ If a point's data has been changed, record it in the database }

PROCEDURE WriteOutSignalDataToDatabase;
{ If a Signal's data has been changed, record it in the database }

PROCEDURE WriteOutTrackCircuitDataToDatabase;
{ Write out some track circuit data to the track circuit data file }

IMPLEMENTATION

{$R *.dfm}

USES
  LocoUtils, MiscUtils, Lenz, RailDraw, IniFiles, Startup, DateUtils, GetTime, Diagrams, StrUtils, Grids, Movement, LocationData, Feedback, Options;

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

PROCEDURE SetUpLineDrawingVars;
{ Set up the positions of the lines and plaforms }
BEGIN
  { Interval spacing : the following data has been read in from the registry }
  BufferStopVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, BufferStopVerticalSpacing, ZoomScaleFactor * 10);
  GridInterLineSpacing := 1000 DIV (WindowRows + 1);
  IndicatorHorizontalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, IndicatorHorizontalSpacing, ZoomScaleFactor * 10);
  IndicatorVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, IndicatorVerticalSpacing, ZoomScaleFactor * 10);
  InterLineSpacing := (FWPRailWindow.ClientHeight * 1000) DIV ((WindowRows + 1) * ZoomScalefactor);
  MouseRectangleEdgeVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, MouseRectangleEdgeVerticalSpacing, ZoomScaleFactor * 10);
  PlatformEdgeVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, PlatformEdgeVerticalSpacing, ZoomScaleFactor * 10);
  PlatformNumberEdgeHorizontalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, PlatformNumberEdgeHorizontalSpacing, ZoomScaleFactor * 10);
  PlatformNumberEdgeVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, PlatformNumberEdgeVerticalSpacing, ZoomScaleFactor * 10);
  SignalHorizontalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, SignalHorizontalSpacing, ZoomScaleFactor * 10);
  SignalRadiusScaled := MulDiv(FWPRailWindow.ClientWidth, SignalRadius, ZoomScaleFactor * 10);
  SignalSemaphoreHeightScaled := MulDiv(FWPRailWindow.ClientWidth, SignalSemaphoreHeight, ZoomScaleFactor * 10);
  SignalSemaphoreWidthScaled := MulDiv(FWPRailWindow.ClientHeight, SignalSemaphoreWidth, ZoomScaleFactor * 10);
  SignalVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, SignalVerticalSpacing, ZoomScaleFactor * 10);
  SpeedRestrictionHorizontalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, SpeedRestrictionHorizontalSpacing, ZoomScaleFactor * 10);
  SpeedRestrictionVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, SpeedRestrictionVerticalSpacing, ZoomScaleFactor * 10);
  TheatreIndicatorHorizontalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, TheatreIndicatorHorizontalSpacing, ZoomScaleFactor * 10);
  TheatreIndicatorVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, TheatreIndicatorVerticalSpacing, ZoomScaleFactor * 10);
  TRSPlungerLengthScaled := MulDiv(FWPRailWindow.ClientWidth, TRSPlungerLength, ZoomScaleFactor * 10);
END; { SetUpLineDrawingVars }

PROCEDURE ReadInTrackCircuitDataFromDatabase;
{ Initialise the track circuit data which depends on lines being initialised first.

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
  Location : Integer;
  TC : Integer;
  TempStr : String;

BEGIN
  TRY
    Log('A INITIALISING TRACK CIRCUITS {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track Circuit database file "' + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix + '" cannot be located'
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
      Log('T Track circuit data table and connection opened to initialise the trackCircuits');

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
              TempStr := FieldByName(TC_LengthFieldName).AsString;
              IF TempStr = '' THEN
                TC_LengthInInches := 0
              ELSE
                IF NOT TryStrToFloat(TempStr, TC_LengthInInches) THEN
                  Debug('TC=' + IntToStr(TC_Number) + ': invalid length String "' + TempStr + '"');
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(TC_FeedbackUnitFieldName).AsString;
              IF TempStr = '' THEN
                TC_FeedbackUnit := 0
              ELSE
                IF NOT TryStrToInt(TempStr, TC_FeedbackUnit) THEN
                  ErrorMsg := 'invalid feedback unit "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(TC_FeedbackInputFieldName).AsString;
              IF TempStr = '' THEN
                TC_FeedbackInput := 0
              ELSE
                IF NOT TryStrToInt(TempStr, TC_FeedbackInput) THEN
                  ErrorMsg := 'invalid feedback input "' + TempStr + '"';
            END;

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
      Log('T Track circuit data table and connection closed');
    END; {WITH}

    FOR TC := 0 TO High(TrackCircuits) DO BEGIN
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
        TC_UserMustDrive := False;

        SetLength(TC_LineArray, 0);
      END; {WITH}
    END; {FOR}

    { Initialise track circuit lines and locations }
    CalculateTCAdjacentBufferStops;

    { Check that all track circuits have a TC_LineArray value }
    TC := 0;
    WHILE TC <= High(TrackCircuits) DO BEGIN
      IF Length(TrackCircuits[TC].TC_LineArray) = 0 THEN
        Debug('!TC=' + IntToStr(TC) + ': has no entry in the LineData file');
      Inc(TC);
    END; {WHILE}

    { Work out how long individual locations are based on track circuit length. This can be overriden by a specific length given below, however. }
    FOR Location := 0 TO High(Locations) DO BEGIN
      { Check that the location doesn't already have a designated preset length - this is necessary in platforms, for instance, as the total track circuit length may well
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

PROCEDURE WriteOutTrackCircuitDataToDatabase;
{ Write out some track circuit data to the track circuit data file }
VAR
  TC : Integer;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track circuit database file "' + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
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
      Log('T Track circuit data table and connection opened to write out track circuit data');

      TrackCircuitsADOTable.First;
      TC := 0;
//      WHILE NOT TrackCircuitsADOTable.EOF DO BEGIN
//        WITH TrackCircuits[TC] DO BEGIN
//          IF TC_DataChanged THEN BEGIN
//            TC_DataChanged := False;
//
//            Log('T Recording in track circuit database that TC ' + IntToStr(TC) + ' ' + TC_NumberFieldName + ' is ''' + IntToStr(TC_Number) + '''');
//            TrackCircuitsADOTable.Edit;
//            TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsString := IntToStr(TC_Number);
//            TrackCircuitsADOTable.Post;
//          END;
//        END; {WITH}
//
//        Inc(TC);
//        TrackCircuitsADOTable.Next;
//      END; {WHILE}

      WHILE TC <= High(TrackCircuits) DO BEGIN
        WITH TrackCircuits[TC] DO BEGIN
          IF TC_DataChanged THEN BEGIN
            TC_DataChanged := False;

            IF NOT TrackCircuitsADOTable.Locate(TC_NumberFieldName, IntToStr(TC), []) THEN BEGIN
              Log('T Track circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TC) + ' but it cannot be found');
            END ELSE BEGIN
              Log('T Track circuit data table and connection opened to record that ' + TC_NumberFieldName + ' is ''' + IntToStr(TC_Number) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsString := IntToStr(TC_Number);
              TrackCircuitsADOTable.Post;
            END;
          END;
        END; {WITH}

        Inc(TC);
      END; {WHILE}

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('L Track circuit data table and connection closed after writing track circuit data');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutTrackCircuitData: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutTrackCircuitDataToDatabase }

FUNCTION DeleteRecordFromTrackCircuitDatabase(TrackCircuitToDeleteNum : Integer) : Boolean;
{ Remove a record from the trackCircuit database }
BEGIN
  Result := False;
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track circuit database file "' + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
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
        Log('T Track circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TrackCircuitToDeleteNum) + ' but it cannot be found');
      END ELSE BEGIN
        Log('T Track circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TrackCircuitToDeleteNum));

        { Now delete the track circuit - we have already checked, in the Edit unit, whether deleting it will cause knock-on problems with other track circuits }
        TrackCircuitsADOTable.Delete;
        Log('TG TrackCircuit ' + IntToStr(TrackCircuitToDeleteNum) + ' has been deleted');
        Result := True;
      END;

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('T Track circuit Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToTrackCircuitDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
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
        ELSE
          Exit;
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
            { Some defaults first }
            Location_Area := UnknownArea;
            SetLength(Location_LocosNotAbleToUse, 0);
            SetLength(Location_LocoClassesReservedFor, 0);
            Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyard := UnknownLocation;
            Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr := '';
            Location_PlatformOrFiddleyardDirection := UnknownDirection;
            Location_PlatformOrFiddleyardNumStr := '';
            Location_RecordInLocationOccupationArray := False;

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

PROCEDURE CalculateLinePolygons(Line : Integer);
{ Work out the position for the various line polygons }
VAR
  MidScreenX : Integer;
  MidScreenY : Integer;
  ScreenUpX : Integer;
  ScreenUpY : Integer;
  ScreenDownX : Integer;
  ScreenDownY : Integer;

BEGIN
  WITH Lines[Line] DO BEGIN
    ScreenUpX := MapGridXToScreenX(Line_GridUpX);
    ScreenUpY := MapGridYToScreenY(Line_GridUpY);
    ScreenDownX := MapGridXToScreenX(Line_GridDownX);
    ScreenDownY := MapGridYToScreenY(Line_GridDownY);

    { The mouse polygon }
    Line_MousePolygon[0] := Point(ScreenUpX, ScreenUpY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_MousePolygon[1] := Point(ScreenUpX, ScreenUpY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_MousePolygon[2] := Point(ScreenDownX, ScreenDownY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_MousePolygon[3] := Point(ScreenDownX, ScreenDownY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_MousePolygon[4] := Line_MousePolygon[0];

    { The handles }
    Line_UpHandlePolygon[0] := Point(ScreenUpX - SignalRadiusScaled, ScreenUpY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_UpHandlePolygon[1] := Point(ScreenUpX - SignalRadiusScaled, ScreenUpY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_UpHandlePolygon[2] := Point(ScreenUpX + SignalRadiusScaled, ScreenUpY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_UpHandlePolygon[3] := Point(ScreenUpX + SignalRadiusScaled, ScreenUpY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_UpHandlePolygon[4] := Line_UpHandlePolygon[0];

    Line_DownHandlePolygon[0] := Point(ScreenDownX + SignalRadiusScaled, ScreenDownY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_DownHandlePolygon[1] := Point(ScreenDownX + SignalRadiusScaled, ScreenDownY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_DownHandlePolygon[2] := Point(ScreenDownX - SignalRadiusScaled, ScreenDownY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_DownHandlePolygon[3] := Point(ScreenDownX - SignalRadiusScaled, ScreenDownY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_DownHandlePolygon[4] := Line_DownHandlePolygon[0];

    MidScreenX := ScreenUpX + ((ScreenDownX - ScreenUpX) DIV 2);
    MidScreenY := ScreenUpY + ((ScreenDownY - ScreenUpY) DIV 2);
    Line_MidHandlePolygon[0] := Point(MidScreenX - SignalRadiusScaled, MidScreenY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_MidHandlePolygon[1] := Point(MidScreenX - SignalRadiusScaled, MidScreenY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_MidHandlePolygon[2] := Point(MidScreenX + SignalRadiusScaled, MidScreenY - MouseRectangleEdgeVerticalSpacingScaled);
    Line_MidHandlePolygon[3] := Point(MidScreenX + SignalRadiusScaled, MidScreenY + MouseRectangleEdgeVerticalSpacingScaled);
    Line_MidHandlePolygon[4] := Line_MidHandlePolygon[0];
  END; {WITH}
END; { CalculateLinePolygons }

PROCEDURE CalculateLineEndCharacters(Line : Integer);
{ Add the line-end characters which indicate where a line goes next }
VAR
  ScreenUpX : Integer;
  ScreenUpY : Integer;
  ScreenDownX : Integer;
  ScreenDownY : Integer;

BEGIN
  WITH RailWindowBitmap.Canvas DO BEGIN
    WITH Lines[Line] DO BEGIN
      ScreenUpX := MapGridXToScreenX(Line_GridUpX);
      ScreenUpY := MapGridYToScreenY(Line_GridUpY);
      ScreenDownX := MapGridXToScreenX(Line_GridDownX);
      ScreenDownY := MapGridYToScreenY(Line_GridDownY);

      { a straight line }
      Font.Height := -MulDiv(FWPRailWindow.ClientHeight, FWPRailWindowFontHeight, ZoomScaleFactor);
      IF Line_UpConnectionCh <> '' THEN BEGIN
        WITH Line_UpConnectionChRect DO BEGIN
          Left := ScreenUpX - (TextWidth(Line_UpConnectionCh) DIV 2);
          Top := ScreenUpY - (TextHeight(Line_UpConnectionCh) DIV 2);
          Right := ScreenUpX + (TextWidth(Line_UpConnectionCh) DIV 2);
          Bottom := ScreenUpY + (TextHeight(Line_UpConnectionCh) DIV 2);
        END; {WITH}
      END;

      IF Line_DownConnectionCh <> '' THEN BEGIN
        WITH Line_DownConnectionChRect DO BEGIN
          Left := ScreenDownX - (TextWidth(Line_DownConnectionCh) DIV 2);
          Top := ScreenDownY - (TextHeight(Line_DownConnectionCh) DIV 2);
          Right := ScreenDownX + (TextWidth(Line_DownConnectionCh) DIV 2);
          Bottom := ScreenDownY + (TextHeight(Line_DownConnectionCh) DIV 2);
        END; {WITH}
      END;
    END; {WITH}
  END; {WITH}
END; { CalculateLineEndCharacters }

PROCEDURE CalculateLinePositions;
{ Work out where the lines are on the screen }
VAR
  Line : Integer;
  OtherLine : Integer;

  PROCEDURE CreateBufferStop(L : Integer; BSDirection : DirectionType; BSTheatreDestination : String);
  { Create a bufferstop record }
  BEGIN
    SetLength(BufferStops, Length(BufferStops) + 1);

    WITH BufferStops[High(BufferStops)] DO BEGIN
      BufferStop_AdjacentLine := L;
      BufferStop_AdjacentTrackCircuit := Lines[BufferStop_AdjacentLine].Line_TC;
      IF (BufferStop_AdjacentTrackCircuit = UnknownTrackCircuit) AND ProgramStartup THEN
        Log('E Buffer stop ' + IntToStr(High(BufferStops)) + ' (at line' + ' ' + Lines[BufferStop_AdjacentLine].Line_NameStr + ') has no adjacent track circuit');

      BufferStop_AsTheatreDestination := BSTheatreDestination;
      BufferStop_CurrentColour := BufferStopColour;
      BufferStop_Direction := BSDirection;
      BufferStop_Number := High(BufferStops);

      Lines[L].Line_AdjacentBufferStop := BufferStop_Number;
    END; {WITH}
  END; { CreateBufferStop }

  PROCEDURE CalculateBufferStopPositions;
  { Work out where the buffer stops are on the screen }
  VAR
    BufferStop : Integer;

  BEGIN
    BufferStop := 0;
    WHILE BufferStop <= High(BufferStops) DO BEGIN
      WITH BufferStops[BufferStop] DO BEGIN
        IF BufferStop_Direction = Up THEN BEGIN
          BufferStop_X := MapGridXToScreenX(Lines[BufferStop_AdjacentLine].Line_GridUpX);
          BufferStop_Y1 := MapGridYToScreenY(Lines[BufferStop_AdjacentLine].Line_GridUpY) - BufferStopVerticalSpacingScaled;
          BufferStop_Y2 := MapGridYToScreenY(Lines[BufferStop_AdjacentLine].Line_GridUpY) + BufferStopVerticalSpacingScaled;
        END ELSE BEGIN
          BufferStop_X := MapGridXToScreenX(Lines[BufferStop_AdjacentLine].Line_GridDownX);
          BufferStop_Y1 := MapGridYToScreenY(Lines[BufferStop_AdjacentLine].Line_GridDownY) - BufferStopVerticalSpacingScaled;
          BufferStop_Y2 := MapGridYToScreenY(Lines[BufferStop_AdjacentLine].Line_GridDownY) + BufferStopVerticalSpacingScaled;
        END;

        { The mouse rectangle }
        WITH BufferStop_MouseRect DO BEGIN
          Left := BufferStop_X - MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScaleFactor);
          Top := BufferStop_Y1;
          Right := BufferStop_X + MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScaleFactor);
          Bottom := BufferStop_Y2;
        END; {WITH}
      END; {WITH}
      Inc(BufferStop);
    END; {WHILE}
  END; { CalculateBufferStopPositions }

BEGIN
  TRY
    Line := 0;
    WHILE Line <= High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        Line_GridUpY := Round(Line_UpRow * GridInterLineSpacing);
        Line_GridDownY := Round(Line_DownRow * GridInterLineSpacing);
      END; {WITH}
      Inc(Line);
    END; {WHILE}

    Line := 0;
    WHILE Line <= High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        CalculateLinePolygons(Line);
        CalculateLineEndCharacters(Line);
      END; {WITH}
      Inc(Line);
    END; {WHILE}

    { Sort out buffer stop positions }
    FOR Line := 0 TO High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        Line_CurrentColour := TCUnoccupiedColour;
        Line_OldColour := TCUnoccupiedColour;
        Line_RoutedOver := False;
        Line_NoLongerOutOfUse := False;

        CASE Line_EndOfLineMarker OF
          NotEndOfLine:
            BEGIN
              Line_NextUpType := LineIsNext;
              Line_NextUpIsEndofLine := NotEndOfLine;
              Line_NextDownType := LineIsNext;
              Line_NextDownIsEndOfLine := NotEndOfLine;
            END;
          BufferStopAtUp:
            BEGIN
              Line_NextUpType := EndOfLineIsNext;
              Line_NextUpIsEndofLine := BufferStopAtUp;
              Line_NextDownType := LineIsNext;
              { and create a bufferstop record }
              CreateBufferStop(Line, Up, Line_BufferStopTheatreDestinationStr);
            END;
          BufferStopAtDown:
            BEGIN
              Line_NextDownType := EndOfLineIsNext;
              Line_NextDownIsEndOfLine := BufferStopAtDown;
              Line_NextUpType := LineIsNext;
              { and create a bufferstop record }
              CreateBufferStop(Line, Down, Line_BufferStopTheatreDestinationStr);
            END;
          ProjectedLineAtUp:
            BEGIN
              Line_NextUpType := EndOfLineIsNext;
              Line_NextUpIsEndofLine := ProjectedLineAtUp;
              Line_NextDownType := LineIsNext;
            END;
          ProjectedLineAtDown:
            BEGIN
              Line_NextDownType := EndOfLineIsNext;
              Line_NextDownIsEndOfLine := ProjectedLineAtDown;
              Line_NextUpType := LineIsNext;
            END;
        END; { CASE }

        IF Line_NextUpType = UnknownNextLineRouteingType THEN BEGIN
          IF MessageDialogueWithDefault('Line_NextUpType = UnknownNextLineRouteingType at ' + LineToStr(Line),
                                        StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
          THEN
            ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
        END ELSE
          IF Line_NextDownType = UnknownNextLineRouteingType THEN BEGIN
            IF MessageDialogueWithDefault('Line_NextDownType = UnknownNextLineRouteingType at ' + LineToStr(Line),
                                          StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
            THEN
              ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
          END;

      END; {WITH}
    END; {FOR}

    { Now work out which lines are next to which }
    FOR Line := 0 TO High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        FOR OtherLine := 0 TO High(Lines) DO BEGIN
          IF Line <> OtherLine THEN BEGIN
            { First, see if a line is disconnected from its neighbour - look for an alphabetic character at its start or end }
            IF (Line_UpConnectionCh <> '') AND (Line_UpConnectionCh = Lines[OtherLine].Line_DownConnectionCh) THEN BEGIN
              Line_NextUpType := LineIsNext;
              Line_NextUpLine := OtherLine
            END ELSE
              IF (Line_DownConnectionCh <> '') AND (Line_DownConnectionCh = Lines[OtherLine].Line_UpConnectionCh) THEN BEGIN
                Line_NextDownType := LineIsNext;
                Line_NextDownLine := OtherLine
              END ELSE BEGIN
                { Now look for normal horizontal connections }
                IF (Line_GridDownX = Lines[OtherLine].Line_GridUpX) AND (Line_GridDownY = Lines[OtherLine].Line_GridUpY) AND
                  (Line_GridUpY = Lines[OtherLine].Line_GridDownY) THEN BEGIN
                  Line_NextDownType := LineIsNext;
                  Line_NextDownLine := OtherLine;
                END;

                IF (Line_GridUpX = Lines[OtherLine].Line_GridDownX) AND (Line_GridUpY = Lines[OtherLine].Line_GridDownY) AND
                  (Line_GridDownY = Lines[OtherLine].Line_GridUpY) THEN BEGIN
                  Line_NextUpType := LineIsNext;
                  Line_NextUpLine := OtherLine;
                END;
              END;

            { If there are no normal horizontal connections, see if there's a non-horizontal one }
            IF (Line_NextUpLine = UnknownLine) OR (Line_NextDownLine = UnknownLine) THEN BEGIN
              IF (Line_GridDownX = Lines[OtherLine].Line_GridUpX) AND (Line_GridDownY = Lines[OtherLine].Line_GridUpY) THEN BEGIN
                Line_NextDownType := LineIsNext;
                Line_NextDownLine := OtherLine;
              END;

              IF (Line_GridUpX = Lines[OtherLine].Line_GridDownX) AND (Line_GridUpY = Lines[OtherLine].Line_GridDownY) THEN BEGIN
                Line_NextUpType := LineIsNext;
                Line_NextUpLine := OtherLine;
              END;
            END;
          END;
        END;
      END; {WITH}
    END; {FOR}

    CalculateBufferStopPositions;

  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CalculateLinePositions: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CalculateLinePositions }

FUNCTION ValidateLineInUseFeedbackUnit(FeedbackUnitStr : String; OUT ErrorMsg : String) : Integer;
{ Only checks that the supplied data is numeric }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF (FeedbackUnitStr <> '') AND NOT TryStrToInt(FeedbackUnitStr, Result) THEN
    ErrorMsg := 'ValidateLineInUseFeedbackUnit: invalid line-in-use feedback unit "' + FeedbackUnitStr + '"';
END; { ValidateLineInUseFeedbackUnit }

FUNCTION ValidateLineGradient(LineGradientStr : String; OUT ErrorMsg : String) : GradientType;
{ Checks the line's gradient is valid }
BEGIN
  ErrorMsg := '';

  Result := StrToGradient(LineGradientStr);
  IF (LineGradientStr = '') OR (Result = UnknownGradientType) THEN
    ErrorMsg := 'ValidateLineGradient: unknown gradient type "' + LineGradientStr + '"';
END; { ValidateLineGradient }

FUNCTION ValidateLineConnectionCh(LineConnectionCh : String; OUT ErrorMsg : String) : String;
{ See if the connection char exceeds one character }
BEGIN
  ErrorMsg := '';
  Result := '';

  IF Length(LineConnectionCh) > 1 THEN
    ErrorMsg := 'ValidateLineConnectionCh: line connection character "' + LineConnectionCh + '" is too long'
  ELSE
    Result := LineConnectionCh;
END; { ValidateLineConnectionCh }

FUNCTION ValidateLineType(LineTypeStr : String; OUT ErrorMsg : String) : TypeOfLine;
{ See if the type of line is correct }
BEGIN
  ErrorMsg := '';

  Result := StrToTypeOfLine(LineTypeStr);
  IF (LineTypeStr = '') OR (Result = UnknownTypeOfLine) THEN
    ErrorMsg := 'ValidateLineType: unknown type of line "' + LineTypeStr + '"';
END; { ValidateLineType }

FUNCTION ValidateLineDirection(LineDirectionStr : String; OUT ErrorMsg : String) : DirectionType;
{ Check the direction is correct }
BEGIN
  ErrorMsg := '';

  Result := StrToDirectionType(LineDirectionStr);
  IF (LineDirectionStr = '') OR (Result = UnknownDirection) THEN
    ErrorMsg := ':ValidateLineDirection unknown line direction "' + LineDirectionStr + '"';
END; { ValidateLineDirection }

FUNCTION ValidateBufferStopNumber(BufferStopStr : String; OUT ErrorMsg : String) : Integer;
{ See if there's a valid number (or nothing) }
BEGIN
  ErrorMsg := '';

  IF BufferStopStr= '' THEN
    Result := UnknownBufferStop
  ELSE
    IF NOT TryStrToInt(BufferStopStr, Result) THEN
      ErrorMsg := 'ValidateBufferStopNumber: invalid buffer stop number "' + BufferStopStr + '"';
END; { ValidateBufferStopNumber }

FUNCTION ValidateLineEndOfLineMarker(EndOfLineMarkerStr : String; OUT ErrorMsg : String) : EndOfLineType;
{ See if the supplied end of line string is correct }
BEGIN
  ErrorMsg := '';

  Result := StrToEndOfLine(EndOfLineMarkerStr);
  IF (EndOfLineMarkerStr = '') OR (Result = UnknownEndOfLine) THEN
    ErrorMsg := 'ValidateLineEndOfLineMarker: unknown end of line marker "' + EndOfLineMarkerStr + '"';
END; { ValidateLineEndOfLineMarker }

FUNCTION ValidateLineTrackCircuit(LineTCStr : String; OUT ErrorMsg : String) : Integer;
{ Checks the validity of the supplied line track circuit }
BEGIN
  ErrorMsg := '';

  IF LineTCStr = '' THEN
    Result := UnknownTrackCircuit
  ELSE
    IF NOT TryStrToInt(LineTCStr, Result) THEN
      ErrorMsg := 'ValidateLineTrackCircuit: invalid line TC integer "' + LineTCStr + '"'
    ELSE
      IF ((Result < 0) AND (Result > High(TrackCircuits))) OR (Result = UnknownTrackCircuit) THEN
        ErrorMsg := 'ValidateLineTrackCircuit: track circuit number ' + IntToStr(Result) + ' outside bounds';
END; { ValidateLineTrackCircuit }

FUNCTION ValidateLineLocation(LineLocationStr : String; OUT ErrorMsg : String) : Integer;
{ Checks whether the line location is valid }
BEGIN
  ErrorMsg := '';

  IF LineLocationStr = '' THEN
    Result := UnknownLocation
  ELSE BEGIN
    Result := StrToLocation(LineLocationStr);
    IF Result = UnknownLocation THEN
      ErrorMsg := 'ValidateLineLocation: unknown line location "' + LineLocationStr + '"';
  END;
END; { ValidateLineLocation }

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

FUNCTION ValidateLineName(Str : String; Line : Integer; OUT ErrorMsg : String) : String;
{ Validates whether the new line name matches an existing one }
VAR
  TempLine : Integer;

BEGIN
  ErrorMsg := '';
  Result := Str;

  TempLine := 0;
  WHILE (TempLine <= High(Lines)) AND (TempLine <> Line) AND (ErrorMsg = '') DO BEGIN
    { check all lines apart from the one we've just created }
    IF Str = Lines[TempLine].Line_NameStr THEN
      ErrorMsg := 'duplicate line name "' + Str + '" (' + IntToStr(TempLine) + ') found'
    ELSE
      Inc(TempLine);
  END; {WHILE}
END; { ValidateLineName }

PROCEDURE ReadInLineDataFromDatabase;
{ Initialise the data for each of the line segments }
CONST
  Horizontal = True;
  StopTimer = True;

VAR
  ErrorMsg : String;
  Line : Integer;
  OtherLine : Integer;

BEGIN
  TRY
    Log('A INITIALISING LINES {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Line database file "' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase')
        ELSE
          Exit;
      END;

      LinesADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix
                                             + ';Persist Security Info=False';
      LinesADOConnection.Connected := True;
      LinesADOTable.Open;
      Log('T Line data table and connection opened to initialise the lines');

      LinesADOTable.Sort := '[' + Line_NumberFieldName + '] ASC';
      LinesADOTable.First;

      { and make sure the arrays are empty, otherwise the next time the screen is resized we will merely add more lines/bufferstops }
      SetLength(Lines, 0);
      SetLength(BufferStops, 0);

      WHILE NOT LinesADOTable.EOF DO BEGIN
        WITH LinesADOTable DO BEGIN
          SetLength(Lines, Length(Lines) + 1);
          Line := High(Lines);
          WITH Lines[Line] DO BEGIN
            ErrorMsg := '';

            Line_GridUpX := 0;
            Line_GridDownX := 0;
            Line_GridUpY := 0;
            Line_GridDownY := 0;

            Line_DataChanged := False;
            Line_AdjacentBufferStop := UnknownBufferStop;
            Line_LockFailureNotedInSubRouteUnit := False;
            Line_RouteLockingForDrawing := UnknownRoute;
            Line_RouteSet := UnknownRoute;

            Line_NextUpIsEndofLine := NotEndOfLine;
            Line_NextUpLine := UnknownLine;
            Line_NextUpPoint := UnknownPoint;
            Line_NextUpType := UnknownNextLineRouteingType;

            Line_NextDownIsEndOfLine := NotEndOfLine;
            Line_NextDownLine := UnknownLine;
            Line_NextDownPoint := UnknownPoint;
            Line_NextDownType := UnknownNextLineRouteingType;

            Line_UpConnectionCh := '';
            Line_UpConnectionChBold := False;
            Line_DownConnectionCh := '';
            Line_DownConnectionChBold := False;

            Line_IsBeingMovedByHandle := NoHandle;
            Line_IsTempNewLine := False;
            Line_ShowHandles := False;

            Line_Number := FieldByName(Line_NumberFieldName).AsInteger;
            IF Line_Number <> Line THEN
              ErrorMsg := 'it does not match the line number in the database (' + IntToStr(Line_Number) + ')';

            IF ErrorMsg = '' THEN
              Line_NameStr := ValidateLineName(FieldByName(Line_NameStrFieldName).AsString, Line, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_UpRow := ValidateRow(FieldByName('Up Row').AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_DownRow := ValidateRow(FieldByName('Down Row').AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_GridUpX := ValidateGridX(FieldByName(Line_GridUpXFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_GridDownX := ValidateGridX(FieldByName(Line_GridDownXFieldName).AsString, ErrorMsg);

//            IF ErrorMsg = '' THEN
//              Line_GridUpY := ValidateGridX(FieldByName(Line_GridUpYFieldName).AsString, ErrorMsg);
//
//            IF ErrorMsg = '' THEN
//              Line_GridDownY := ValidateGridX(FieldByName(Line_GridDownYFieldName).AsString, ErrorMsg);
//
            IF ErrorMsg = '' THEN
              Line_Location := ValidateLineLocation(FieldByName(Line_LocationStrFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_TC := ValidateLineTrackCircuit(FieldByName(Line_TCFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_EndOfLineMarker := ValidateLineEndOfLineMarker(FieldByName(Line_EndOfLineMarkerFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_BufferStopTheatreDestinationStr := FieldByName(Line_BufferStopTheatreDestinationStrFieldName).AsString;

            IF ErrorMsg = '' THEN
              Line_Direction := ValidateLineDirection(FieldByName(Line_DirectionFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_TypeOfLine := ValidateLineType(FieldByName(Line_TypeOfLineFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_UpConnectionCh := ValidateLineConnectionCh(FieldByName(Line_UpConnectionChFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_DownConnectionCh := ValidateLineConnectionCh(FieldByName(Line_DownConnectionChFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN
              Line_Gradient := ValidateLineGradient(FieldByName(Line_GradientFieldName).AsString, ErrorMsg);

            IF ErrorMsg = '' THEN BEGIN
              IF FieldByName(Line_OutOfUseFieldName).AsBoolean THEN BEGIN
                Line_InitialOutOfUseState := OutOfUse; { this is used only in deciding whether to save a changed state on exit }
                Line_OutOfUseState := OutOfUse;
                Line_SaveOutOfUseState := OutOfUse;
              END ELSE BEGIN
                Line_InitialOutOfUseState := InUse;
                Line_OutOfUseState := InUse;
                Line_SaveOutOfUseState := InUse;
              END;

              { Even if the line itself is in use, that can be overridden by the location being out of use }
              IF Line_Location <> UnknownLocation THEN
                IF Locations[Line_Location].Location_OutOfUse THEN
                  Line_OutOfUseState := OutOfUse;
            END;

            IF ErrorMsg = '' THEN
              Line_InUseFeedbackUnit := ValidateLineInUseFeedbackUnit(FieldByName(Line_InUseFeedbackUnitFieldName).AsString, ErrorMsg);

            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating Line=' + IntToStr(High(Lines)) + ' (' + Line_NameStr + '): '
                                            + '[' + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
            END;
          END;
        END; {WITH}
        LinesADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      LinesADOTable.Close;
      LinesADOConnection.Connected := False;
      Log('T Line Data table and connection closed');
    END; {WITH}

    { Now we have to do certain tests as some of the data was not be available while we read it in. First see if there are any duplicate connection characters }
    FOR Line := 0 TO High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        FOR OtherLine := 0 TO High(Lines) DO BEGIN
          IF Line <> OtherLine THEN BEGIN
            IF (Line_UpConnectionCh <> '') AND (Line_UpConnectionCh = Lines[OtherLine].Line_UpConnectionCh) THEN BEGIN
              IF MessageDialogueWithDefault('Duplicate up connection character ''' + Line_UpConnectionCh + ''''
                                            + ' found at line ' + LineToStr(Line) + ' and at ' + LineToStr(OtherLine),
                                            StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
              THEN
                ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
            END ELSE
              IF (Line_DownConnectionCh <> '') AND (Line_DownConnectionCh = Lines[OtherLine].Line_DownConnectionCh) THEN BEGIN
                IF MessageDialogueWithDefault('Duplicate down connection character ''' + Line_DownConnectionCh + '''' +
                                              ' found at line ' + LineToStr(Line) + ' and at ' + LineToStr(OtherLine),
                                              StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
                THEN
                  ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
              END;
          END;
        END; {FOR}
      END; {WITH}
    END; {FOR}

    { Now work out the X and Y values }
    CalculateLinePositions;

    FOR Line := 0 TO High(Lines) DO BEGIN
      WITH Lines[Line] DO BEGIN
        { In all cases, UpX should be less than DownX }
        IF Line_GridUpX > Line_GridDownX THEN BEGIN
          IF MessageDialogueWithDefault('Line ' + Line_NameStr + ': UpX > DownX',
                                        StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
          THEN
            ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
        END;
      END; {WITH}
    END; {FOR}

    { And check for lines which are declared but not initialised }
    FOR Line := 0 TO High(Lines) DO
      IF LineToStr(Line) = '' THEN
        Log('X! Line ' + IntToStr(Line) + ' does not have a name');

  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInLineDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}

(* TP { This is left over from the Turbo Pascal version }
    DX := (X2 - X1) / AspectRatio;
    { AMS incomprehensible comment follows: }
    { / not * cos here we are moving from screen coordinates to normalised coordinates not vice versa }
    DY := Y2 - Y1;

    { Scale to give accurate speeds }
    LineLength := Round(20 * Sqrt(DX * DX + DY * DY));
TP *)
END; { ReadInLineDataFromDatabase }

PROCEDURE AddNewRecordToLineDatabase;
{ Append a record to the line database }
BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Line database file "' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'AddNewRecordToLineDatabase')
        ELSE
          Exit;
      END;

      LinesADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix
                                             + ';Persist Security Info=False';
      LinesADOConnection.Connected := True;
      LinesADOTable.Open;
      LinesADOTable.Append;
      LinesADOTable.FieldByName(Line_NumberFieldName).AsInteger := High(Lines);
      LinesADOTable.Post;

      Log('S Line data table and connection opened to create record for Line ' + IntToStr(High(Lines)));
      { Tidy up the database }
      LinesADOTable.Close;
      LinesADOConnection.Connected := False;
      Log('S Line Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToLineDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { AddNewRecordToLineDatabase }

PROCEDURE WriteOutLineDataToDatabase;
{ Write out some line data to the line data file }
VAR
  Line : Integer;
  TempInt : Integer;
  TempExtended : Extended;
  TempStr : String;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Line database file "' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'WriteOutLineDataFromDatabase')
        ELSE
          Exit;
      END;

      LinesADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix
                                             + ';Persist Security Info=False';
      LinesADOConnection.Connected := True;
      LinesADOTable.Open;
      Log('L Loco Data table and connection opened to write out line data');

      Line := 0;
      WHILE Line <= High(Lines) DO BEGIN
        WITH Lines[Line] DO BEGIN
          { Deal with out of use changes }
          IF ((Line_OutOfUseState = OutOfUse)
               OR ((Line_Location <> UnknownLocation) AND Locations[Line_Location].Location_OutOfUse))
          AND (Line_InitialOutOfUseState = InUse)
          THEN BEGIN
            LinesADOTable.First;
            WHILE NOT LinesADOTable.EOF DO BEGIN
              WITH LinesADOTable DO BEGIN
                IF FieldByName(Line_NameStrFieldName).AsString = Line_NameStr THEN BEGIN
                  Edit;
                  FieldByName(Line_OutOfUseFieldName).AsBoolean := True;
                  Post;

                  Log('S Recording in line database that Line ' + IntToStr(Line) + ' (' + Line_NameStr + ') is out of use');
                END;
              END; {WITH}
              LinesADOTable.Next;
            END; {WHILE}
          END ELSE BEGIN
            IF (Line_OutOfUseState = InUse) AND (Line_InitialOutOfUseState = OutOfUse) THEN BEGIN
              LinesADOTable.First;
              WHILE NOT LinesADOTable.EOF DO BEGIN
                WITH LinesADOTable DO BEGIN
                  IF FieldByName(Line_NameStrFieldName).AsString = Line_NameStr THEN BEGIN
                    Edit;
                    FieldByName(Line_OutOfUseFieldName).AsBoolean := False;
                    Post;

                    Log('S Recording in line database that Line ' + IntToStr(Line) + ' (' + Line_NameStr + ') is in use');
                  END;
                END; {WITH}
                LinesADOTable.Next;
              END; {WHILE}
            END;
          END;
        END; {WITH}
        Inc(Line);
      END; {WHILE}

      { Now deal with any general editing changes }
      LinesADOTable.First;
      Line := 0;
      WHILE NOT LinesADOTable.EOF DO BEGIN
        WITH Lines[Line] DO BEGIN
          IF Line_DataChanged THEN BEGIN
            Line_DataChanged := False;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_NameStrFieldName + ' is ''' + Line_NameStr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_NameStrFieldName).AsString := Line_NameStr;
            LinesADOTable.Post;

            TempInt := Line_GridUpX;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_GridUpXFieldName + ' is ''' + IntToStr(TempInt) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_GridUpXFieldName).AsString := IntToStr(TempInt);
            LinesADOTable.Post;

            TempInt := Line_GridDownX;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_GridDownXFieldName + ' is ''' + IntToStr(TempInt) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_GridDownXFieldName).AsString := IntToStr(TempInt);
            LinesADOTable.Post;

            TempInt := Line_GridUpY;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_GridUpYFieldName + ' is ''' + IntToStr(TempInt) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_GridUpYFieldName).AsString := IntToStr(TempInt);
            LinesADOTable.Post;

            TempInt := Line_GridDownY;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_GridDownYFieldName + ' is ''' + IntToStr(TempInt) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_GridDownYFieldName).AsString := IntToStr(TempInt);
            LinesADOTable.Post;

            TempExtended := Line_UpRow;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_UpRowFieldName + ' is ''' + FloatToStr(TempExtended) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_UpRowFieldName).AsString := FloatToStr(TempExtended);
            LinesADOTable.Post;

            TempExtended := Line_DownRow;
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_DownRowFieldName + ' is ''' + FloatToStr(TempExtended) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_DownRowFieldName).AsString := FloatToStr(TempExtended);
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_BufferStopTheatreDestinationStrFieldName
                   + ' is ''' + Line_BufferStopTheatreDestinationStr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_BufferStopTheatreDestinationStrFieldName).AsString := Line_BufferStopTheatreDestinationStr;
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_DirectionFieldName + ' is ''' + DirectionToStr(Line_Direction) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_DirectionFieldName).AsString := DirectionToStr(Line_Direction);
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_DownConnectionChFieldName + ' is ''' + Line_DownConnectionCh + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_DownConnectionChFieldName).AsString := Line_DownConnectionCh;
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_UpConnectionChFieldName + ' is ''' + Line_UpConnectionCh + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_UpConnectionChFieldName).AsString := Line_UpConnectionCh;
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_EndOfLineMarkerFieldName + ' is ''' + EndOfLineToStr(Line_EndOfLineMarker) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_EndOfLineMarkerFieldName).AsString := EndOfLineToStr(Line_EndOfLineMarker);
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_GradientFieldName + ' is ''' + GradientToStr(Line_Gradient) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_GradientFieldName).AsString := GradientToStr(Line_Gradient);
            LinesADOTable.Post;

            IF Line_InUseFeedbackUnit <> 0 THEN
              TempStr := IntToStr(Line_InUseFeedbackUnit)
            ELSE
              TempStr := '';
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_InUseFeedbackUnitFieldName + ' is ''' + TempStr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_InUseFeedbackUnitFieldName).AsString := TempStr;
            LinesADOTable.Post;

            IF Line_Location <> UnknownLocation THEN
              Tempstr := LocationToStr(Line_Location)
            ELSE
              Tempstr := '';
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_LocationStrFieldName + ' is ''' + Tempstr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_LocationStrFieldName).AsString := Tempstr;
            LinesADOTable.Post;

            IF Line_TC <> UnknownTrackCircuit THEN
              TempStr := IntToStr(Line_TC)
            ELSE
              TempStr := '';
            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_TCFieldName + ' is ''' + TempStr + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_TCFieldName).AsString := TempStr;
            LinesADOTable.Post;

            Log('S Recording in Line database that Line ' + IntToStr(Line) + ' ' + Line_TypeOfLineFieldName + ' is ''' + TypeOfLineToStr(Line_TypeOfLine) + '''');
            LinesADOTable.Edit;
            LinesADOTable.FieldByName(Line_TypeOfLineFieldName).AsString := TypeOfLineToStr(Line_TypeOfLine);
            LinesADOTable.Post;
          END;
        END; {WITH}

        Inc(Line);
        LinesADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      LinesADOTable.Close;
      LinesADOConnection.Connected := False;
      Log('L Line Data table and connection closed after writing line data');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutLineDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutLineDataToDatabase }

FUNCTION DeleteRecordFromLineDatabase(LineToDeleteNum : Integer) : Boolean;
{ Remove a record from the line database }
BEGIN
  Result := False;
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Line database file "' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'DeleteRecordFromLineDatabase')
        ELSE
          Exit;
      END;

      LinesADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix
                                             + ';Persist Security Info=False';
      LinesADOConnection.Connected := True;
      LinesADOTable.Open;
      IF NOT LinesADOTable.Locate(Line_NumberFieldName, IntToStr(LineToDeleteNum), []) THEN BEGIN
        Log('L Line data table and connection opened to delete Line ' + LineToStr(LineToDeleteNum) + ' but it cannot be found');
      END ELSE BEGIN
        Log('L Line data table and connection opened to delete Line ' + LineToStr(LineToDeleteNum));

        { Delete the line }
        LinesADOTable.Delete;
        Log('LG Line ' + IntToStr(LineToDeleteNum) + ' has been deleted');
        Result := True;
      END;

      { Tidy up the database }
      LinesADOTable.Close;
      LinesADOConnection.Connected := False;
      Log('L Line Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DeleteRecordFromLineDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DeleteRecordFromLineDatabase }

PROCEDURE CalculateSignalPosition(S : Integer);
{ Work out where a signal is on the screen }
BEGIN
  TRY
    WITH Signals[S] DO BEGIN
      IF Signal_AdjacentLine <> UnknownLine THEN BEGIN
        IF Signal_Direction = Up THEN BEGIN
          Signal_LineX := MapGridXToScreenX(Lines[Signal_AdjacentLine].Line_GridUpX);
          Signal_LineX := Signal_LineX + SignalRadiusScaled;
          IF Signal_Indicator <> NoIndicator THEN
            Signal_LineX := Signal_LineX + SignalHorizontalSpacingScaled;
          IF Signal_Type = FourAspect THEN
            Signal_LineX := Signal_LineX + SignalHorizontalSpacingScaled;
        END ELSE BEGIN
          { Down }
          Signal_LineX := MapGridXToScreenX(Lines[Signal_AdjacentLine].Line_GridDownX);
          Signal_LineX := Signal_LineX - SignalRadiusScaled;
          IF Signal_Indicator <> NoIndicator THEN
            Signal_LineX := Signal_LineX - SignalHorizontalSpacingScaled;
          IF Signal_Type = FourAspect THEN
            Signal_LineX := Signal_LineX - SignalHorizontalSpacingScaled;
        END;

        Signal_LineY := MapGridYToScreenY(Lines[Signal_AdjacentLine].Line_GridUpY);

        { Adjust left or right if AdjacentLineXOffset greater than or less than zero respectively }
        IF Signal_AdjacentLineXOffset > 0 THEN
          Signal_LineX := Signal_LineX + MulDiv(FWPRailWindow.ClientWidth, Signal_AdjacentLineXOffset, ZoomScaleFactor)
        ELSE
          IF Signal_AdjacentLineXOffset < 0 THEN
            Signal_LineX := Signal_LineX - MulDiv(FWPRailWindow.ClientWidth, Abs(Signal_AdjacentLineXOffset), ZoomScaleFactor);

        SignalVerticalSpacingScaled := SignalVerticalSpacingScaled;

        IF Signal_Direction = Up THEN
          Signal_LineWithVerticalSpacingY := Signal_LineY + SignalVerticalSpacingScaled
        ELSE
          IF Signal_Direction = Down THEN
            Signal_LineWithVerticalSpacingY := Signal_LineY - SignalVerticalSpacingScaled;
      END; {WITH}

      { Set up mouse access rectangles }
      WITH Signal_MouseRect DO BEGIN
        RailWindowBitmap.Canvas.Pen.Width := WindowPenWidth;

        IF (Signal_Type <> SemaphoreHome) AND (Signal_Type <> SemaphoreDistant) THEN BEGIN
          { it covers the colour-light signal circles }
          Left := Signal_LineX - SignalRadiusScaled;
          Top := Signal_LineWithVerticalSpacingY - SignalRadiusScaled;
          Right := Signal_LineX + SignalRadiusScaled;
          Bottom := Signal_LineWithVerticalSpacingY + SignalRadiusScaled;
        END ELSE BEGIN
          { it covers the semaphore signal arms }
          IF Signal_Direction = Up THEN BEGIN
            Left := Signal_LineX - SignalSemaphoreWidthScaled;
            Top := Signal_LineWithVerticalSpacingY + RailWindowBitmap.Canvas.Pen.Width;
            Right := Signal_LineX + (SignalSemaphoreWidthScaled * 2);
            Bottom := Signal_LineWithVerticalSpacingY + (SignalSemaphoreWidthScaled * 2);
          END ELSE
            IF Signal_Direction = Down THEN BEGIN
              Left := Signal_LineX - (SignalSemaphoreWidthScaled * 2);
              Top := Signal_LineWithVerticalSpacingY - (SignalSemaphoreWidthScaled * 2);
              Right := Signal_LineX + SignalSemaphoreWidthScaled;
              Bottom := Signal_LineWithVerticalSpacingY - RailWindowBitmap.Canvas.Pen.Width;
            END;
        END;
      END; {WITH}

      { Initialise the route indicator mouse access rectangles }
      WITH Signal_IndicatorMouseRect DO BEGIN
        Left := 0;
        Top := 0;
        Right := 0;
        Bottom := 0;
      END;

      IF Signal_Direction = Up THEN BEGIN
        IF Signal_Type = FourAspect THEN
          Signal_MouseRect.Left := Signal_MouseRect.Left - SignalHorizontalSpacingScaled;
        IF Signal_Indicator <> NoIndicator THEN BEGIN
          WITH Signal_IndicatorMouseRect DO BEGIN
            Left := Signal_MouseRect.Left - (MulDiv(IndicatorHorizontalSpacingScaled, 150, 100));
            Top := Signal_MouseRect.Top;
            Right := Signal_MouseRect.Left;
            Bottom := Signal_MouseRect.Bottom;
          END; {WITH}
        END;
      END ELSE
        IF Signal_Direction = Down THEN BEGIN
          { Signal_Direction = Down }
          IF Signal_Type = FourAspect THEN
            Signal_MouseRect.Right := Signal_MouseRect.Right + SignalHorizontalSpacingScaled;
          IF Signal_Indicator <> NoIndicator THEN BEGIN
            IF Signal_Indicator <> NoIndicator THEN BEGIN
              WITH Signal_IndicatorMouseRect DO BEGIN
                Left := Signal_MouseRect.Right;
                Top := Signal_MouseRect.Top;
                Right := Signal_MouseRect.Right + (MulDiv(IndicatorHorizontalSpacingScaled, 150, 100));
                Bottom := Signal_MouseRect.Bottom;
              END; {WITH}
            END;
          END;
        END;

      { Now the signal posts (used for routeing) }
      WITH Signal_PostMouseRect DO BEGIN
        IF Signal_Direction = Up THEN BEGIN
          { pen.width is the width of the line outlining the signal }
          Left := Signal_LineX + SignalRadiusScaled;
          Top := Signal_LineWithVerticalSpacingY - SignalRadiusScaled;
          Right := Signal_LineX + SignalRadiusScaled + MulDiv(FWPRailWindow.ClientWidth, 10, ZoomScalefactor);
          Bottom := Signal_LineWithVerticalSpacingY + SignalRadiusScaled;
        END ELSE
          IF Signal_Direction = Down THEN BEGIN
            Left := Signal_LineX - SignalRadiusScaled - MulDiv(FWPRailWindow.ClientWidth, 10, ZoomScalefactor);
            Top := Signal_LineWithVerticalSpacingY - SignalRadiusScaled;
            Right := Signal_LineX - SignalRadiusScaled;
            Bottom := Signal_LineWithVerticalSpacingY + SignalVerticalSpacingScaled - RailWindowBitmapCanvasPenWidth;
          END;
      END; {WITH}
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CalculateSignalPosition: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CalculateSignalPosition }

PROCEDURE CalculateAllSignalPositions;
{ Work out where all the signals are on the screen }
VAR
  S : Integer;

BEGIN
  TRY
    S := 0;
    WHILE S <= High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF Signal_AdjacentLine <> UnknownLine THEN
          CalculateSignalPosition(S);
      END; {WITH}
      Inc(S);
    END; {WHILE}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CalculateAllSignalPositions: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CalculateAllSignalPositions }

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

FUNCTION ValidateSignalAccessoryAddress(Str : String; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder address for a TrainTech S3 accessory decoder. This test must be done after Signal Type is validated. }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF Str <> '' THEN
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalAccessoryAddress: invalid signal accessory address integer String "' + Str + '"'
    ELSE
      IF (SignalType <> SemaphoreHome) AND (SignalType <> SemaphoreDistant) THEN
        ErrorMsg := 'ValidateSignalAccessoryAddress: cannot use TrainTech SC3 accessory addresses with non-semaphore signals';
END; { ValidateSignalAccessoryAddress }

FUNCTION ValidateSignalAdjacentLine(SignalBeingValidated : Integer; Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the line next to a signal }
VAR
  S : Integer;

BEGIN
  ErrorMsg := '';

  Result := StrToLine(Str);
  IF Result = UnknownLine THEN
    ErrorMsg := 'ValidateSignalAdjacentLine: unknown line';

  IF ErrorMsg = '' THEN BEGIN
    { Check that there isn't more than one signal on the same bit of line - it causes problems }
    S := 0;
    WHILE S < - High(Signals) DO BEGIN
      IF S <> SignalBeingValidated THEN BEGIN
        IF Signals[S].Signal_AdjacentLine = Result THEN
          ErrorMsg := 'ValidateSignalAdjacentLine: S=' + IntToStr(SignalBeingValidated) + ':'
                      + ' adjacent line ' +  LineToStr(Signals[S].Signal_AdjacentLine) + ' is already in use by another signal (S=' + IntToStr(S) + ')';
      END;
      Inc(S);
    END; {WHILE}
  END;
END; { ValidateSignalAdjacentLine }

FUNCTION ValidateSignalApproachControlAspect(Str : String; OUT ErrorMsg : String) : AspectType;
{ Validates and if ok returns the approach control signal aspect }
BEGIN
  ErrorMsg := '';
  Result := NoAspect;

  Str := UpperCase(Str);
  IF Str <> '' THEN BEGIN
    IF Str = 'NONE' THEN
      Result := NoAspect
    ELSE
      IF Str = 'RED' THEN
        Result := RedAspect
      ELSE
        IF Str = 'SINGLE YELLOW' THEN
          Result := SingleYellowAspect
        ELSE
          ErrorMsg := 'ValidateSignalApproachControlAspect: invalid approach control aspect';
  END;
END; { ValidateSignalApproachControlAspect }

FUNCTION ValidateSignalAsTheatreDestination(Str : String; OUT ErrorMsg : String) : String;
{ Validates and if ok returns the one or two character display used in a theatre indicator }
BEGIN
  ErrorMsg := '';
  Result := '';

  IF Length(Str) > 2 THEN
    ErrorMsg := 'ValidateSignalAsTheatreDestination: Theatre Destination "' + Str + '" is more than two characters long'
  ELSE
    Result := Str;
END; { ValidateSignalAsTheatreDestination }

FUNCTION ValidateSignalDecoderNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for a signal decoder. This check must be done after Accessory Address and Signal Type are validated. }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF Str <> '' THEN
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalDecoderNum: invalid signal decoder integer String "' + Str + '"'
    ELSE
      IF AccessoryAddress <> 0 THEN
        ErrorMsg := 'ValidateSignalDecoderNum: cannot combine signal decoders and TrainTech SC3 accessory addresses'
      ELSE
        IF (SignalType = SemaphoreHome) OR (SignalType = SemaphoreDistant) THEN
          ErrorMsg := 'ValidateSignalDecoderNum: cannot use function decoder addresses with semaphore signals';
END; { ValidateSignalDecoderNum }

FUNCTION ValidateSignalDirection(Str : String; OUT ErrorMsg : String) : DirectionType;
{ Validates and if ok returns the signal direction }
BEGIN
  ErrorMsg := '';

  Result := StrToDirectionType(Str);
  IF Result = UnknownDirection THEN
    ErrorMsg := 'ValidateSignalDirection: unknown signal direction';
END; { ValidateSignalDirection }

FUNCTION ValidateSignalIndicatorSpeedRestriction(Str : String; Indicator : IndicatorType; OUT ErrorMsg : String) : MPHType;
{ Validates and if ok returns what the tempoarary speed restriction is. This test must be carried outr after Indicator is validated. }
VAR
  TempInt : Integer;

BEGIN
  ErrorMsg := '';
  Result := NoSpecifiedSpeed;

  IF Str <> '' THEN BEGIN
    IF TryStrToInt(Trim(Str), TempInt) THEN
      Result := IntToMPH(TempInt)
    ELSE
      ErrorMsg := 'ValidateSignalIndicatorSpeedRestriction: invalid integer String "' + Str + '"';

    IF ErrorMsg = '' THEN
      IF (Result <> NoSpecifiedSpeed) AND (Indicator = NoIndicator) THEN
        ErrorMsg := 'ValidateSignalIndicatorSpeedRestriction: cannot have an indicator speed restriction if there is no indicator';
  END;
END; { ValidateSignalIndicatorSpeedRestriction }

FUNCTION ValidateSignalJunctionIndicators1(Str, FieldName : String; Signal_Indicator : IndicatorType; OUT ErrorMsg : String) : JunctionIndicatorRec;
{ The first part of verifying whether junction indicators are correctly set up; this part also returns the values for each junction indicator. This test requires that
  Indicator has been validated first
}
BEGIN
  ErrorMsg := '';
  Result.JunctionIndicator_Exists := False;
  Result.JunctionIndicator_TargetSignal := UnknownSignal;
  Result.JunctionIndicator_TargetBufferStop := UnknownBufferStop;

  Str := UpperCase(Str);
  IF Str <> '' THEN BEGIN
    IF (Copy(Str, 1, 1) <> 'S') AND (Copy(Str, 1, 2) <> 'BS') THEN
      ErrorMsg := 'ValidateSignalJunctionIndicators1: invalid target signal (S) or bufferstop (BS) for ' + FieldName + ': "' + Str + '"'
    ELSE
      IF Copy(Str, 1, 1) = 'S' THEN BEGIN
        Str := Copy(Str, 2);
        IF (Str = '') OR NOT TryStrToInt(Trim(Str), Result.JunctionIndicator_TargetSignal) THEN
          ErrorMsg := 'ValidateSignalJunctionIndicators1: invalid target signal (S) or bufferstop (BS) for ' + FieldName + ': "' + Str + '"';
      END
      ELSE
        IF Copy(Str, 1, 2) = 'BS' THEN BEGIN
          Str := Copy(Str, 3);
          IF (Str = '') OR NOT TryStrToInt(Trim(Str), Result.JunctionIndicator_TargetBufferStop) THEN
            ErrorMsg := 'ValidateSignalJunctionIndicators1: invalid target signal (S) or bufferstop (BS) for ' + FieldName + ': "' + Str + '"';
        END;

    IF ErrorMsg = '' THEN
      Result.JunctionIndicator_Exists := True;
  END;
END; { ValidateSignalJunctionIndicators1 }

PROCEDURE ValidateSignalJunctionIndicators2(StrArray : ARRAY OF String; SignalIndicator : IndicatorType; SignalJunctionIndicators : ARRAY OF JunctionIndicatorRec;
                                            OUT ErrorMsg : String);
{ The second part of verifying whether junction indicators are correctly set up }
VAR
  I, J : Integer;
  IndicatorsFound : Boolean;

BEGIN
  ErrorMsg := '';

  { See if any of the signal indicator data is duplicated }
  I := 0;
  WHILE (I <= High(StrArray)) AND (ErrorMsg = '') DO BEGIN
    J := 0;
    WHILE (J <= High(StrArray)) AND (ErrorMsg = '') DO BEGIN
      IF I <> J THEN BEGIN
        IF (StrArray[I] <> '') AND (StrArray[I] = StrArray[J]) THEN
          ErrorMsg := 'ValidateSignalJunctionIndicators2: identical signal/bufferstopdata (' + StrArray[I] + ') in indicator fields';
      END;
      Inc(J);
    END; {WHILE}
    Inc(I);
  END; {WHILE}

  IF ErrorMsg = '' THEN BEGIN
    IndicatorsFound := False;
    FOR I := 0 TO 5 DO BEGIN
      IF SignalJunctionIndicators[I].JunctionIndicator_Exists THEN
        IndicatorsFound := True;
    END; {FOR}

    IF (SignalIndicator = JunctionIndicator) AND NOT IndicatorsFound THEN
      ErrorMsg := 'ValidateSignalJunctionIndicators2: signal indicator type is set to Junction Indicator even though there is no indicator data'
    ELSE
      IF (SignalIndicator <> JunctionIndicator) AND IndicatorsFound THEN
        ErrorMsg := 'ValidateSignalJunctionIndicators2: signal indicator type is not set to Junction Indicator even though there is indicator data';
  END;
END; { ValidateSignalJunctionIndicators2 }

FUNCTION ValidateNextSignalIfNoIndicator(Str : String; Init : Boolean; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns what the other signal is if no indicator is lit }
BEGIN
  ErrorMsg := '';
  Result := UnknownSignal;

  Str := UpperCase(Str);
  IF Str <> '' THEN BEGIN
    IF Copy(Str, 1, 1) <> 'S' THEN
      ErrorMsg := 'ValidateNextSignalIfNoIndicator: next signal if no indicator: "' + Str + '" not preceded by ''S'''
    ELSE BEGIN
      Str := Copy(Str, 2);
      IF NOT TryStrToInt(Trim(Str), Result) THEN
        ErrorMsg := 'ValidateNextSignalIfNoIndicator: invalid signal integer string "' + Str + '"';
    END;
  END;
END; { ValidateNextSignalIfNoIndicator }

FUNCTION ValidateSignalNum(SignalToTest : Integer) : String;
{ Validates a signal number. This has to be carried out separately from other validation, as when creating signals a reference may be made to a signal not yet created }
BEGIN
  Result := '';

  IF (SignalToTest <> UnknownSignal) AND (SignalToTest > High(Signals)) THEN
    Result := 'ValidateSignalNum: cross-reference to invalid signal number "' + IntToStr(SignalToTest) + '"';
END; { ValidateSignalNum }

FUNCTION ValidateSignalIndicator(Str : String; OUT ErrorMsg : String) : IndicatorType;
{ Validates and if ok returns the signal indicator }
BEGIN
  ErrorMsg := '';
  Result := NoIndicator;

  Str := UpperCase(Str);
  IF Str <> '' THEN BEGIN
    CASE Str[1] OF
      'J':
        Result := JunctionIndicator;
      'T':
        Result := TheatreIndicator;
      ELSE { CASE }
        ErrorMsg := 'ValidateSignalIndicator: invalid indicator';
    END; { CASE }
  END;
END; { ValidateSignalIndicator }

FUNCTION ValidateSignalIndicatorDecoderNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for an indicator decoder. This check must be done after Accessory Address and Signal Type are validated. }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF Str <> '' THEN
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalIndicatorDecoderNum: invalid signal indicator decoder integer String "' + Str + '"'
    ELSE
      IF AccessoryAddress <> 0 THEN
        ErrorMsg := 'ValidateSignalIndicatorDecoderNum: cannot combine signal decoders and TrainTech SC3 accessory addresses'
      ELSE
        IF (SignalType = SemaphoreHome) OR (SignalType = SemaphoreDistant) THEN
          ErrorMsg := 'ValidateSignalIndicatorDecoderNum: cannot use function decoder addresses with semaphore signals';
END; { ValidateSignalIndicatorDecoderNum }

FUNCTION ValidateSignalIndicatorDecoderFunctionNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for an indicator function decoder. This check must be done after Accessory Address and Signal Type are validated. }
BEGIN
  ErrorMsg := '';
  Result := 0;

  IF Str <> '' THEN
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalIndicatorDecoderFunctionNum: invalid signal indicator decoder function integer String "' + Str + '"'
    ELSE
      IF AccessoryAddress <> 0 THEN
        ErrorMsg := 'ValidateSignalIndicatorDecoderFunctionNum: cannot combine signal decoders and TrainTech SC3 accessory addresses'
      ELSE
        IF (SignalType = SemaphoreHome) OR (SignalType = SemaphoreDistant) THEN
          ErrorMsg := 'ValidateSignalIndicatorDecoderFunctionNum: cannot use function decoder addresses with semaphore signals';
END; { ValidateSignalIndicatorDecoderFunctionNum }

FUNCTION ValidateSignalLocationsToMonitorArray(Str : String; PossibleRouteHold : Boolean; OUT ErrorMsg : String) : IntegerArrayType;
{ Validates and if ok returns the signal locations monitored when a route is held. This test must be done after Possible Route Hold is validated. }
VAR
  I : Integer;
  PlatformDataOK : Boolean;
  TempLocation : Integer;
  TempLocationsToMonitorStrArray : StringArrayType;

BEGIN
  ErrorMsg := '';
  SetLength(Result, 0);

  IF Str <> '' THEN BEGIN
    IF NOT PossibleRouteHold THEN
      ErrorMsg := 'ValidateSignalLocationsToMonitorArray: cannot monitor locations without a route hold'
    ELSE BEGIN
      ExtractSubStringsFromString(Str, ',', TempLocationsToMonitorStrArray);
      I := 0;
      PlatformDataOK := True;
      WHILE (I <= High(TempLocationsToMonitorStrArray)) AND PlatformDataOK DO BEGIN
        TempLocation := StrToLocation(TempLocationsToMonitorStrArray[I]);
        IF TempLocation = UnknownLocation THEN BEGIN
          PlatformDataOK := False;
          ErrorMsg := 'ValidateSignalLocationsToMonitorArray: unknown location "' + TempLocationsToMonitorStrArray[I] + '" in Signal Locations To Monitor';
        END ELSE
          AppendToLocationArray(Result, TempLocation);
        Inc(I);
      END; {WHILE}
    END;
  END;
END; { ValidateSignalLocationsToMonitorArray }

FUNCTION ValidateSignalOppositePassingLoopSignal(Str : String; Init : Boolean; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the other signal involved in a passing loop }
BEGIN
  ErrorMsg := '';
  Result := UnknownSignal;

  Str := UpperCase(Str);
  IF Str <> '' THEN BEGIN
    IF Copy(Str, 1, 1) <> 'S' THEN
      ErrorMsg := 'ValidateSignalOppositePassingLoopSignal: next signal if no indicator: "' + Str + '" not preceded by ''S'''
    ELSE BEGIN
      Str := Copy(Str, 2);
      IF NOT TryStrToInt(Trim(Str), Result) THEN
        ErrorMsg := 'ValidateSignalOppositePassingLoopSignal: invalid signal integer string "' + Str + '"';
    END;
  END;
END; { ValidateSignalOppositePassingLoopSignal }

FUNCTION ValidateSignalOutOfUseAndAddAdjacentTC(Flag : Boolean; AdjacentLine : Integer; OUT AdjacentTC : Integer; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns true if a signal is marked as not in use. This test must be done after Adjacent Signal is validated. }
BEGIN

  ErrorMsg := '';
  Result := Flag;

  { Note which lines and track circuits are next the signal }
  IF NOT Flag THEN BEGIN
    AdjacentTC := Lines[AdjacentLine].Line_TC;
    IF AdjacentTC = UnknownTrackCircuit THEN
      ErrorMsg := 'ValidateSignalOutOfUse: adjacent to line' + ' ' + LineToStr(AdjacentLine) + ') has no adjacent track circuit';
  END;
END; { ValidateSignalOutOfUseAndAddAdjacentTC }

FUNCTION ValidateSignalPossibleStationStartRouteHold(Flag : Boolean; PossibleRouteHold : Boolean; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns whether a station start route hold is in operation. This test must be done after Possible Route Hold is validated. }
BEGIN
  ErrorMsg := '';
  Result := Flag;

  IF PossibleRouteHold AND Flag THEN
    { both shouldn't be ticked }
    ErrorMsg := 'ValidateSignalPossibleStationStartRouteHold: route hold and station start route hold are both ticked';
END; { ValidateSignalPossibleStationStartRouteHold }

FUNCTION ValidateSignalQuadrant(Str : String; OUT ErrorMsg : String) : QuadrantType;
{ Validates and if ok returns the quadrant type }
BEGIN
  ErrorMsg := '';
  Result := NoQuadrant;

  Str := UpperCase(Str);

  IF Str = 'UPPER' THEN
    Result := UpperQuadrant
  ELSE
    IF Str = 'LOWER' THEN
      Result := LowerQuadrant
    ELSE
      IF Str <> '' THEN
        ErrorMsg := 'Invalid signal quadrant type';
END; { ValidateSignalQuadrant }

FUNCTION ValidateSignalType(Str : String; Quadrant : QuadrantType; DistantHomesArray : IntegerArrayType; OUT ErrorMsg : String) : TypeOfSignal;
{ Validates and if ok returns the signal type }
BEGIN
  ErrorMsg := '';
  Result := StrToSignalType(Str);

  IF Result = UnknownSignalType THEN
    ErrorMsg := 'ValidateSignalType: invalid signal type'
  ELSE
    IF ((Result = SemaphoreHome) OR (Result = SemaphoreDistant))
    AND (Quadrant = NoQuadrant)
    THEN
      ErrorMsg := 'ValidateSignalType: missing quadrant';

  IF (Result = CallingOn) OR (Result = TwoAspect) OR (Result = ThreeAspect) OR (Result = FourAspect) THEN
    IF Quadrant <> NoQuadrant THEN
      ErrorMsg := 'ValidateSignalType: non-semaphore signals cannot have upper or lower quadrants';

  IF Length(DistantHomesArray) <> 0 THEN
    IF Result <> SemaphoreDistant THEN
      ErrorMsg := 'ValidateSignalType: only semaphore distants can have "Distant Homes"';
END; { ValidateSignalType }

FUNCTION ValidateSignalAdjacentLineXOffset(Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns a signal's on-screen adjustment if any }
BEGIN
  ErrorMsg := '';

  IF Str = '' THEN
    Result := -1
  ELSE
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalAdjacentLineXOffset: Invalid integer String "' + Str + '"';
END; { ValidateSignalAdjacentLineXOffset }

FUNCTION ValidateIndicatorDestinations(S : Integer) : String;
{ Validates whether the indicator destinations are valid. This test can only be done once all the signal data has been read in. }
VAR
  TempJunctionIndicator : JunctionIndicatorType;

BEGIN
  Result := '';

  WITH Signals[S] DO BEGIN
    FOR TempJunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
      IF Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_Exists THEN
        IF (Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetSignal <> UnknownSignal)
        AND ((Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetSignal < 0)
              OR
             (Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetSignal > High(Signals)))
        THEN
          Result := ' target for its ' + JunctionIndicatorTypeToStr(TempJunctionIndicator) + ' indicator'
                    + ' (S' + IntToStr(Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetSignal) + ')' + ' is not a valid signal number'
        ELSE
          IF (Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop)
          AND ((Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetBufferStop < 0)
               OR
               (Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetBufferStop > High(BufferStops)))
          THEN
            Result := ' target for its ' + JunctionIndicatorTypeToStr(TempJunctionIndicator) + ' indicator'
                      + ' (BS' + IntToStr(Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetBufferStop) + ')' + ' is not a valid buffer stop number';

      IF (Signal_NextSignalIfNoIndicator <> UnknownSignal) AND ((Signal_NextSignalIfNoIndicator < 0) OR (Signal_NextSignalIfNoIndicator > High(Signals))) THEN
        Result := ' target for next signal if no indicator (S' + IntToStr(Signal_NextSignalIfNoIndicator) + ') is not a valid signal number';
    END; {FOR}
  END; {WITH}
END; { ValidateIndicatorDestinations }

FUNCTION ValidateSignalDistantHomesArray(S : Integer; Signal_Type : TypeOfSignal; Str : String; OUT ErrorMsg : String) : IntegerArrayType;
{ Validates the home signal numbers supplied }
VAR
  I, J : Integer;
  SignalDataOK : Boolean;
  TempSignal : Integer;
  TempSignalsStrArray : StringArrayType;

BEGIN
  ErrorMsg := '';
  SetLength(Result, 0);

  IF (Signal_Type <> SemaphoreDistant) AND (Str <> '') THEN
    ErrorMsg := 'ValidateSignalDistantHomesArray: non-distant semaphore signal ' + IntToStr(S) + ' has home signals connected to it'
  ELSE BEGIN
    IF Signal_Type = SemaphoreDistant THEN BEGIN
      IF Str = '' THEN
        ErrorMsg := 'ValidateSignalDistantHomesArray: semaphore distant signal ' + IntToStr(S) + ' does not have any home signals connected to it'
      ELSE BEGIN
        IF Str <> '' THEN BEGIN
          ExtractSubStringsFromString(Str, ',', TempSignalsStrArray);
          I := 0;
          SignalDataOK := True;
          WHILE (I <= High(TempSignalsStrArray)) AND SignalDataOK DO BEGIN
            IF Copy(TempSignalsStrArray[I], 1, 1) <> 'S' THEN
              ErrorMsg := 'ValidateSignalDistantHomesArray: signal "' + TempSignalsStrArray[I] + '" not preceded by ''S'''
            ELSE BEGIN
              TempSignalsStrArray[I] := Copy(TempSignalsStrArray[I], 2);
              IF NOT TryStrToInt(Trim(TempSignalsStrArray[I]), TempSignal) THEN
                ErrorMsg := 'ValidateSignalDistantHomesArray: invalid signal integer string "' + TempSignalsStrArray[I] + '"'
              ELSE
                { we need to validate the signal number too, but can't do that until all the signals are read in }
                AppendToLocationArray(Result, TempSignal);
            END;
            Inc(I);
          END; {WHILE}

          { Now check that the supplied homes don't appear in any other DistantHomesArray as a home can't be attached to more than one distant }
          FOR TempSignal := 0 TO High(Signals) DO BEGIN
            IF TempSignal <> S THEN BEGIN
              IF Signals[TempSignal].Signal_Type = SemaphoreDistant THEN BEGIN
                I := 0;
                WHILE (I <= High(Signals[TempSignal].Signal_SemaphoreDistantHomesArray)) AND (ErrorMsg = '') DO BEGIN
                  J := 0;
                  WHILE (J <= High(Result)) AND (ErrorMsg = '') DO BEGIN
                    IF Signals[TempSignal].Signal_SemaphoreDistantHomesArray[I] = Result[J] THEN
                      ErrorMsg := 'ValidateSignalDistantHomesArray: signal ' + IntToStr(Result[J])
                                  + ' appears in both DistantHomesArray for S' + IntToStr(S) + ' and for S' +  IntToStr(TempSignal);
                    Inc(J);
                  END; {WHILE}
                  Inc(I);
                END; {WHILE}
              END;
            END;
          END; {FOR}
        END;
      END;
    END;
  END;
END; { ValidateSignalDistantHomesArray }

PROCEDURE ReadInSignalDataFromDatabase(NewSignalData : Boolean);
{ Create entries for the signals }
CONST
  ApproachControlled = True;
  InUse = True;
  Init = True;
  PermitRouteTo = True;
  StopTimer = True;

VAR
  DistantFound : Boolean;
  ErrorMsg : String;
  I : Integer;
  S : Integer;
  TempJunctionIndicator : JunctionIndicatorType;
  TempLineArray : IntegerArrayType;
  TempS : Integer;
  TempSignalNumber : Integer;
  TempStrArray : ARRAY [0..5] OF String;

BEGIN
  TRY
    Log('A READ IN SIGNAL DATA FROM DATABASE {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      SetLength(Signals, 0);
      SetLength(TempLineArray, 0);

      IF NOT FileExists(PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Signal database file "' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInSignalDataFromDatabase')
        ELSE
          Exit;
      END;

      SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix
                                               + ';Persist Security Info=False';
      TRY
        SignalsADOConnection.Connected := True;
      EXCEPT
        ON E : Exception DO
          Log('EG ReadInSignalDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
      END; {TRY}

      SignalsADOTable.Open;

      { First see if the signal numbers in the MSAccess file are sequential and, if not, renumber it - we need this or deletions from the MSAccess file will causes
        problems
      }
      S := -1;
      SignalsADOTable.Sort := '[' + Signal_NumberFieldName + '] ASC';
      SignalsADOTable.First;
      WHILE NOT SignalsADOTable.EOF DO BEGIN
        Inc(S);
        IF SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger <> S THEN BEGIN
          { we need to renumber from here on }
          SignalsADOTable.Edit;
          SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger := S;
          SignalsADOTable.Post;
        END;
        SignalsADOTable.Next;
      END; {WHILE}

      Log('S Signal data table and connection opened to initialise the signal data');

      S := -1;
      SignalsADOTable.Sort := '[' + Signal_NumberFieldName + '] ASC';
      SignalsADOTable.First;
      WHILE NOT SignalsADOTable.EOF DO BEGIN
        ErrorMsg := '';
        Inc(S);

        TempSignalNumber := SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger;
        IF TempSignalNumber <> S THEN
          ErrorMsg := 'it does not match the signal number in the database (' + IntToStr(TempSignalNumber) + ')'
        ELSE
          SetLength(Signals, Length(Signals) + 1);

        WITH Signals[S] DO BEGIN
          IF ErrorMsg = '' THEN BEGIN
            Signal_AccessoryAddress := 0;
            Signal_AdjacentLine := UnknownLine;
            Signal_AdjacentLineXOffset := 0;
            Signal_AdjacentTC := UnknownTrackCircuit;
            Signal_ApproachControlAspect := NoAspect;
            Signal_ApproachLocked := False;

            IF NewSignalData THEN
              { colour-light signals are set to RedAspect just before they're switched on, unless we're reloading the data; semaphore signals are always set to red aspect }
              Signal_Aspect := RedAspect
            ELSE
              Signal_Aspect := NoAspect;

            Signal_AsTheatreDestination := ''; { what a signal pointing at this signal might display }
            Signal_Automatic := False; { not yet implemented }
            Signal_DataChanged := False;
            Signal_DecoderNum := 0;
            Signal_Direction := UnknownDirection;
            Signal_Energised := False;
            Signal_EnergisedTime := 0;
            Signal_FailedToResetFlag := False;
            Signal_FailMsgWritten := False;
            Signal_FindNextSignalBufferStopMsgWritten := False;
            Signal_HiddenStationSignalAspect := NoAspect;
            Signal_Indicator := NoIndicator;
            Signal_IndicatorDecoderFunctionNum := 0;
            Signal_IndicatorDecoderNum := 0;
            Signal_IndicatorSpeedRestriction := MPH0; { applicable only if the route indicator is set }
            Signal_IndicatorState := NoIndicatorLit;

            FOR TempJunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
              WITH Signal_JunctionIndicators[TempJunctionIndicator] DO BEGIN
                JunctionIndicator_Exists := False;
                JunctionIndicator_TargetSignal := UnknownSignal;
                JunctionIndicator_TargetBufferStop := UnknownBufferStop;
              END; {WITH}
            END; {FOR}

            Signal_LampIsOn := True;
            Signal_LineWithVerticalSpacingY := UnknownLine;
            Signal_LineX := 0;
            Signal_LineY := 0;
            SetLength(Signal_LocationsToMonitorArray, 0);

            { Signal_LockedArray and Signal_RouteLockingNeededArray sound similar but serve different purposes - RouteLockingNeededArray covers the lines, track circuits,
              points, etc. ahead that must be locked before a signal can be pulled off; Signal_LockedArray shows whether a signal is locked either by a specific route or
              by a user.
            }
            SetLength(Signal_LockedArray, 0);
            Signal_LockedBySemaphoreDistant := False;
            Signal_LockFailureNotedInRouteUnit := False;
            Signal_NextSignalIfNoIndicator := UnknownSignal;
            Signal_Notes := '';
            Signal_NotUsedForRouteing := True;
            Signal_Number := TempSignalNumber;
            Signal_OppositePassingLoopSignal := UnknownSignal;
            Signal_OutOfUse := False;
            Signal_OutOfUseMsgWritten := False;
            Signal_PossibleRouteHold := False;
            Signal_PossibleStationStartRouteHold := False;
            Signal_PostColour := ForegroundColour;
            Signal_PreviousAspect := NoAspect;
            Signal_PreviousHiddenStationSignalAspectSignal1 := UnknownSignal;
            Signal_PreviousHiddenStationSignalAspectSignal2 := UnknownSignal;
            Signal_PreviousIndicatorState := NoIndicatorLit;
            Signal_PreviousLineWithVerticalSpacingY := 0;
            Signal_PreviousLineX := 0;
            Signal_PreviousLineY := 0;
            Signal_PreviousSignal1 := UnknownSignal;
            Signal_PreviousSignal2 := UnknownSignal;
            Signal_PreviousTheatreIndicatorString := '';
            Signal_Quadrant := NoQuadrant;
            Signal_ResettingTC := UnknownTrackCircuit;

            { see note above for Signal_LockedArray }
            SetLength(Signal_RouteLockingNeededArray, 0);
            SetLength(Signal_SemaphoreDistantHomesArray, 0); { needed to tell a semaphore distant which semaphore homes lock it }
            Signal_SemaphoreDistantLocking := UnknownSignal;
            Signal_StateChanged := False;
            Signal_TheatreIndicatorString := '';
            Signal_TRSHeld := False;
            Signal_TRSHeldMsgWritten := False;
            Signal_TRSReleased := False;
            Signal_TRSReleasedMsgWritten := False;
            Signal_Type := TwoAspect;
          END;

          IF ErrorMsg = '' THEN
            Signal_Quadrant := ValidateSignalQuadrant(SignalsADOTable.FieldByName(Signal_QuadrantFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            IF SignalsADOTable.FieldByName(Signal_TypeFieldName).AsString = '' THEN
              ErrorMsg := 'no signal type given'
            ELSE
              Signal_Type := ValidateSignalType(SignalsADOTable.FieldByName(Signal_TypeFieldName).AsString, Signal_Quadrant, Signal_SemaphoreDistantHomesArray, ErrorMsg);
          END;

          { NB the signal fields are in the following order for validation purposes }
          Signal_Indicator := NoIndicator;
          IF ErrorMsg = '' THEN
            Signal_Indicator := ValidateSignalIndicator(SignalsADOTable.FieldByName(Signal_IndicatorFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[0] := SignalsADOTable.FieldByName(Signal_UpperLeftIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[UpperLeftIndicator] :=
                                                   ValidateSignalJunctionIndicators1(TempStrArray[0], Signal_UpperLeftIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[1] := SignalsADOTable.FieldByName(Signal_MiddleLeftIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[MiddleLeftIndicator] :=
                                                  ValidateSignalJunctionIndicators1(TempStrArray[1], Signal_MiddleLeftIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[2] := SignalsADOTable.FieldByName(Signal_LowerLeftIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[LowerLeftIndicator] :=
                                                   ValidateSignalJunctionIndicators1(TempStrArray[2], Signal_LowerLeftIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[3] := SignalsADOTable.FieldByName(Signal_UpperRightIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[UpperRightIndicator] :=
                                                  ValidateSignalJunctionIndicators1(TempStrArray[3], Signal_UpperRightIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[4] := SignalsADOTable.FieldByName(Signal_MiddleRightIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[MiddleRightIndicator] :=
                                                 ValidateSignalJunctionIndicators1(TempStrArray[4], Signal_MiddleRightIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[5] := SignalsADOTable.FieldByName(Signal_LowerRightIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[LowerRightIndicator] :=
                                                  ValidateSignalJunctionIndicators1(TempStrArray[5], Signal_LowerRightIndicatorTargetFieldName, Signal_Indicator, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN
            ValidateSignalJunctionIndicators2(TempStrArray, Signal_Indicator, Signal_JunctionIndicators, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_AdjacentLine := ValidateSignalAdjacentLine(S, SignalsADOTable.FieldByName(Signal_AdjacentLineFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_AdjacentLineXOffset := ValidateSignalAdjacentLineXOffset(SignalsADOTable.FieldByName(Signal_AdjacentLineXOffsetFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_Direction := ValidateSignalDirection(SignalsADOTable.FieldByName(Signal_DirectionFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_IndicatorSpeedRestriction :=
                       ValidateSignalIndicatorSpeedRestriction(SignalsADOTable.FieldByName(Signal_IndicatorSpeedRestrictionFieldName).AsString, Signal_Indicator, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_NextSignalIfNoIndicator :=
                                             ValidateNextSignalIfNoIndicator(SignalsADOTable.FieldByName(Signal_NextSignalIfNoIndicatorFieldName).AsString, Init, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_OppositePassingLoopSignal := ValidateSignalOppositePassingLoopSignal(SignalsADOTable.FieldByName(Signal_OppositePassingLoopSignalFieldName).AsString,
                                                                                        Init, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_AsTheatreDestination := ValidateSignalAsTheatreDestination(SignalsADOTable.FieldByName(Signal_AsTheatreDestinationFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_OutOfUse :=
                  ValidateSignalOutOfUseAndAddAdjacentTC(SignalsADOTable.FieldByName(Signal_OutOfUseFieldName).AsBoolean, Signal_AdjacentLine, Signal_AdjacentTC, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_Notes := SignalsADOTable.FieldByName(Signal_NotesFieldName).AsString;

          IF ErrorMsg = '' THEN
            Signal_NotUsedForRouteing := SignalsADOTable.FieldByName(Signal_NotUsedForRouteingFieldName).AsBoolean;

          { This is the accessory address used by the TrainTech SC3s switching Dapol semaphores }
          IF ErrorMsg = '' THEN
            Signal_AccessoryAddress := ValidateSignalAccessoryAddress(SignalsADOTable.FieldByName(Signal_AccessoryAddressFieldName).AsString, Signal_Type, ErrorMsg);

          { This is for signals using function decoder addresses }
          IF ErrorMsg = '' THEN
            Signal_DecoderNum := ValidateSignalDecoderNum(SignalsADOTable.FieldByName(Signal_DecoderNumFieldName).AsString, Signal_AccessoryAddress, Signal_Type, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_IndicatorDecoderNum := ValidateSignalIndicatorDecoderNum
                                                (SignalsADOTable.FieldByName(Signal_IndicatorDecoderNumFieldName).AsString, Signal_AccessoryAddress, Signal_Type, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_IndicatorDecoderFunctionNum := ValidateSignalIndicatorDecoderFunctionNum
                                        (SignalsADOTable.FieldByName(Signal_IndicatorDecoderFunctionNumFieldName).AsString, Signal_AccessoryAddress, Signal_Type, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_ApproachControlAspect := ValidateSignalApproachControlAspect(SignalsADOTable.FieldByName(Signal_ApproachControlAspectFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_PossibleRouteHold := SignalsADOTable.FieldByName(Signal_PossibleRouteHoldFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Signal_PossibleStationStartRouteHold := ValidateSignalPossibleStationStartRouteHold
                                                 (SignalsADOTable.FieldByName(Signal_PossibleStationStartRouteHoldFieldName).AsBoolean, Signal_PossibleRouteHold, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_LocationsToMonitorArray :=
                        ValidateSignalLocationsToMonitorArray(SignalsADOTable.FieldByName(Signal_LocationsToMonitorFieldName).AsString, Signal_PossibleRouteHold, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_Automatic := SignalsADOTable.FieldByName(Signal_AutomaticFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Signal_SemaphoreDistantHomesArray := ValidateSignalDistantHomesArray(S, Signal_Type,
                                                                                 SignalsADOTable.FieldByName(Signal_SemaphoreDistantHomesArrayFieldName).AsString,
                                                                                 ErrorMsg);

          IF Signal_PossibleRouteHold AND Signal_PossibleStationStartRouteHold THEN
            { both shouldn't be ticked }
            ErrorMsg := 'route hold and station start route hold are both ticked';

          IF ErrorMsg = '' THEN BEGIN
            { The two following arrays sound similar but serve different purposes - RouteLockingNeededArray covers the lines, track circuits, points, etc. ahead that must
              be locked before a signal can be pulled off; Signal_LockedArray shows whether a signal is locked either by a specific route or by a user.
            }
            SetLength(Signal_RouteLockingNeededArray, 0);
            SetLength(Signal_LockedArray, 0);
          END ELSE BEGIN
            IF MessageDialogueWithDefault('Error in creating S=' + IntToStr(S) + ': '
                                          + ErrorMsg
                                          + CRLF
                                          + 'Do you wish to continue?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
            THEN
              ShutDownProgram(UnitRef, 'ReadInSignalDataFromDatabase 1')
            ELSE
              Signal_OutOfUse := True;
          END;
          SignalsADOTable.Next;
        END; {WITH}
      END; {WHILE}

      { Tidy up the database }
      SignalsADOTable.Close;
      SignalsADOConnection.Connected := False;
      Log('S Signal Data table and connection closed');
    END; {WITH}

    { Check that signal numbers are valid. (We can only do this once all the signal details are read in). }
    S := 0;
    WHILE (S <= High(Signals)) AND (ErrorMsg = '') DO BEGIN
      WITH Signals[S] DO BEGIN
        ErrorMsg := ValidateSignalNum(Signal_NextSignalIfNoIndicator);

        IF ErrorMsg = '' THEN
          ErrorMsg := ValidateSignalNum(Signal_OppositePassingLoopSignal);

        IF ErrorMsg = '' THEN BEGIN
          { we also need to validate the signals held in the DistantHomesArrays }
          I := 0;
          WHILE (I <= High(Signals[S].Signal_SemaphoreDistantHomesArray)) AND (ErrorMsg = '') DO BEGIN
            ErrorMsg := ValidateSignalNum(Signals[S].Signal_SemaphoreDistantHomesArray[I]);
            Inc(I);
          END; {WHILE}
        END;

        IF ErrorMsg = '' THEN
          Inc(S);
      END; {WITH}
    END; {WHILE}

    IF ErrorMsg = '' THEN BEGIN
      { And add locking semaphore distants to records for semaphore homes }
      S := 0;
      WHILE S <= High(Signals) DO BEGIN
        IF Signals[S].Signal_Type = SemaphoreHome THEN BEGIN
          TempS := 0;
          DistantFound := False;
          WHILE (TempS <= High(Signals)) AND NOT DistantFound DO BEGIN
            IF Signals[TempS].Signal_Type = SemaphoreDistant THEN BEGIN
              IF IsElementInIntegerArray(Signals[TempS].Signal_SemaphoreDistantHomesArray, S) THEN BEGIN
                Signals[S].Signal_SemaphoreDistantLocking := TempS;
                DistantFound := True;
              END;
            END;
            Inc(TempS);
          END; {WHILE}
        END;
        Inc(S);
      END; {WHILE}
    END;

    { Check that indicator destinations are valid. (We can only do this once all the signal details are read in, too). }
    IF ErrorMsg = '' THEN BEGIN
      S := 0;
      WHILE (S <= High(Signals)) AND (ErrorMsg = '') DO BEGIN
        ErrorMsg := ValidateIndicatorDestinations(S);
        IF ErrorMsg = '' THEN
          Inc(S);
      END; {WHILE}
    END;

    IF ErrorMsg <> '' THEN
      IF MessageDialogueWithDefault('Error in creating S=' + IntToStr(S) + ': '
                                    + ErrorMsg
                                    + CRLF
                                    + 'Do you wish to continue?',
                                    StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
      THEN
        ShutDownProgram(UnitRef, 'ReadInSignalDataFromDatabase 2');

    CalculateTCAdjacentSignals;
    CalculateAllSignalPositions;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInSignalDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadInSignalDataFromDatabase }

PROCEDURE AddNewRecordToSignalDatabase;
{ Append a record to the signal database }
BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Signal database file "' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'AddNewRecordToSignalDatabase')
        ELSE
          Exit;
      END;

      SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix
                                               + ';Persist Security Info=False';
      SignalsADOConnection.Connected := True;
      SignalsADOTable.Open;
      SignalsADOTable.Append;
      SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger := High(Signals);
      SignalsADOTable.Post;

      Log('S Signal data table and connection opened to write out signal data that has changed');
      { Tidy up the database }
      SignalsADOTable.Close;
      SignalsADOConnection.Connected := False;
      Log('S Signal Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToSignalDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { AddNewRecordToSignalDatabase }

FUNCTION DeleteRecordFromSignalDatabase(SignalToDeleteNum : Integer) : Boolean;
{ Remove a record from the signal database }
BEGIN
  Result := False;
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Signal database file "' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'DeleteRecordFromSignalDatabase')
        ELSE
          Exit;
      END;

      SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix
                                               + ';Persist Security Info=False';
      SignalsADOConnection.Connected := True;
      SignalsADOTable.Open;
      IF NOT SignalsADOTable.Locate(Signal_NumberFieldName, IntToStr(SignalToDeleteNum), []) THEN BEGIN
        Log('S Signal data table and connection opened to delete S' + IntToStr(SignalToDeleteNum) + ' but it cannot be found');
      END ELSE BEGIN
        Log('S Signal data table and connection opened to delete S' + IntToStr(SignalToDeleteNum));

        { Now delete the signal - we have already checked, in the Edit unit, whether deleting it will cause knock-on problems with other signals }
        SignalsADOTable.Delete;
        Log('SG S' + IntToStr(SignalToDeleteNum) + ' has been deleted');
        Result := True;
      END;

      { Tidy up the database }
      SignalsADOTable.Close;
      SignalsADOConnection.Connected := False;
      Log('S Signal Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToSignalDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DeleteRecordFromSignalDatabase }

PROCEDURE WriteOutSignalDataToDatabase;
{ If a Signal's data has been changed, record it in the database }
VAR
  DebugStr : String;
  I : Integer;
  JunctionIndicatorStr : String;
  S : Integer;
  SignalDatabaseNeedsUpdating : Boolean;
  TempStr : String;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      SignalDatabaseNeedsUpdating := False;

      { First see if we need to open the database }
      S := 0;
      WHILE (S <= High(Signals)) AND NOT SignalDatabaseNeedsUpdating DO BEGIN
        IF Signals[S].Signal_DataChanged THEN
          SignalDatabaseNeedsUpdating := True;
        Inc(S);
      END; {WHILE}

      IF SignalDatabaseNeedsUpdating THEN BEGIN
        IF NOT FileExists(PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix) THEN BEGIN
          IF MessageDialogueWithDefault('Signal database file "' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix + '" cannot be located'
                                        + CRLF
                                        + 'Do you wish to continue?',
                                        StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
          THEN
            ShutDownProgram(UnitRef, 'DeleteRecordFromSignalDatabase')
          ELSE
            Exit;
        END;

        SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                 + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix
                                                 + ';Persist Security Info=False';
        SignalsADOConnection.Connected := True;
        SignalsADOTable.Open;
        SignalsADOTable.Edit;
        Log('S Signal data table and connection opened to write out signal data that has changed');

        SignalsADOTable.First;
        WHILE NOT SignalsADOTable.EOF DO BEGIN
          S := SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger;
          WITH Signals[S] DO BEGIN
            IF Signal_DataChanged THEN BEGIN
              Signal_DataChanged := False;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_NumberFieldName + ' is ''' + IntToStr(Signal_Number) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NumberFieldName).AsString := IntToStr(Signal_Number);
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_AccessoryAddressFieldName
                     + ' is ''' + IntToStr(Signals[S].Signal_AccessoryAddress) + '''');
              TempStr := IntToStr(Signal_AccessoryAddress);
              IF TempStr = '0' THEN
                { the database records a zero as a space in this field }
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AccessoryAddressFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_AdjacentLineFieldName + ' is ''' + LineToStr(Signal_AdjacentLine) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AdjacentLineFieldName).AsString := LineToStr(Signal_AdjacentLine);
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_AdjacentLineXOffsetFieldName
                     + ' is ''' + IntToStr(Signal_AdjacentLineXOffset) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AdjacentLineXOffsetFieldName).AsString := IntToStr(Signal_AdjacentLineXOffset);
              SignalsADOTable.Post;

              { The database records "no aspect" as a space in this field }
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_ApproachControlAspectFieldName
                     + ' is ''' + AspectToStr(Signal_ApproachControlAspect) + '''');
              TempStr := AspectToStr(Signal_ApproachControlAspect);
              IF UpperCase(TempStr) = 'NO ASPECT' THEN
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_ApproachControlAspectFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              DebugStr := '';
              IF Length(Signals[S].Signal_SemaphoreDistantHomesArray) > 0 THEN BEGIN
                DebugStr := 'S' + IntToStr(Signals[S].Signal_SemaphoreDistantHomesArray[0]);
                IF Length(Signals[S].Signal_SemaphoreDistantHomesArray) > 1 THEN BEGIN
                  FOR I := 1 TO High(Signals[S].Signal_SemaphoreDistantHomesArray) DO
                    DebugStr := DebugStr + ', S' + IntToStr(Signals[S].Signal_SemaphoreDistantHomesArray[I]);
                END;
              END;
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_SemaphoreDistantHomesArrayFieldName + ' contains ''' + DebugStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_SemaphoreDistantHomesArrayFieldName).AsString := DebugStr;
              SignalsADOTable.Post;


              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_AsTheatreDestinationFieldName + ' is ''' + Signal_AsTheatreDestination + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AsTheatreDestinationFieldName).AsString := Signal_AsTheatreDestination;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_AutomaticFieldName
                     + '  is ''' + IfThen(Signals[S].Signal_Automatic, 'automatic', 'not automatic') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AutomaticFieldName).AsBoolean := Signal_Automatic;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_DecoderNumFieldName + ' is ''' + IntToStr(Signal_DecoderNum) + '''');
              TempStr := IntToStr(Signal_DecoderNum);
              IF TempStr = '0' THEN
                { the database records a zero as a space in this field }
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_DecoderNumFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_DirectionFieldName + ' is ''' + DirectionToStr(Signal_Direction) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_DirectionFieldName).AsString := DirectionToStr(Signal_Direction, VeryShortStringType);
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorFieldName
                     + ' is ''' + IndicatorToStr(Signal_Indicator, LongStringType) + '''');
              TempStr := IndicatorToStr(Signal_Indicator, ShortStringType);
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorDecoderFunctionNumFieldName
                     + ' is ''' + IntToStr(Signal_IndicatorDecoderFunctionNum) + '''');
              TempStr := IntToStr(Signal_IndicatorDecoderFunctionNum);
              IF TempStr = '0' THEN
                { the database records a zero as a space in this field }
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorDecoderFunctionNumFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorDecoderNumFieldName
                     + ' is ''' + IntToStr(Signal_IndicatorDecoderNum) + '''');
              TempStr := IntToStr(Signal_IndicatorDecoderNum);
              IF TempStr = '0' THEN
                { the database records a zero as a space in this field }
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorDecoderNumFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorSpeedRestrictionFieldName
                     + ' is ''' + MPHToStr(Signal_IndicatorSpeedRestriction) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorSpeedRestrictionFieldName).AsString := MPHToStr(Signal_IndicatorSpeedRestriction);
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_UpperLeftIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_UpperLeftIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_MiddleLeftIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_MiddleLeftIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_LowerLeftIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_LowerLeftIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_UpperRightIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_UpperRightIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_MiddleRightIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_MiddleRightIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_LowerRightIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_LowerRightIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              TempStr := '';
              { insert a comma between entries }
              IF Length(Signal_LocationsToMonitorArray) > 0 THEN
                FOR I := 0 TO High(Signal_LocationsToMonitorArray) DO
                  TempStr := TempStr + Locations[Signal_LocationsToMonitorArray[I]].Location_LongNameStr + ',';

              { and remove the comma at the end of the String, if any }
              IF Copy(TempStr, Length(TempStr), 1) = ',' THEN
                TempStr := Copy(TempStr, 1, Length(TempStr) - 1);

              { also remove any spaces in the text - this will help to get around the 250 character length restriction in MSAccess fields }
              TempStr := ReplaceText(TempStr, ' ', '');
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_LocationsToMonitorFieldName + ' is ''' + TempStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_LocationsToMonitorFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              TempStr := IntToStr(Signal_NextSignalIfNoIndicator);
              IF TempStr = IntToStr(UnknownSignal) THEN
                { the database records unknown signal as a space in this field }
                TempStr := ''
              ELSE
                TempStr := 'S' + TempStr;
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_NextSignalIfNoIndicatorFieldName + ' is ''' + TempStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NextSignalIfNoIndicatorFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_NotUsedForRouteingFieldName
                     + ' is ''' + IfThen(Signals[S].Signal_NotUsedForRouteing, 'not used for routeing', 'used for routeing') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NotUsedForRouteingFieldName).AsBoolean := Signal_NotUsedForRouteing;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_NotesFieldName + ' is ''' + Signal_Notes + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NotesFieldName).AsString := Signal_Notes;
              SignalsADOTable.Post;

              TempStr := IntToStr(Signal_OppositePassingLoopSignal);
              IF TempStr = IntToStr(UnknownSignal) THEN
                { the database records unknown signal as a space in this field }
                TempStr := ''
              ELSE
                TempStr := 'S' + TempStr;
              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_OppositePassingLoopSignalFieldName + ' is ''' + TempStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_OppositePassingLoopSignalFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_OutOfUseFieldName
                     + ' is ''' + IfThen(Signals[S].Signal_OutOfUse, 'out of use', 'back in use') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_OutOfUseFieldName).AsBoolean := Signal_OutOfUse;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_PossibleRouteHoldFieldName
                     + ' is ''' + IfThen(Signals[S].Signal_PossibleRouteHold, 'a possible route hold', 'not a possible route hold') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_PossibleRouteHoldFieldName).AsBoolean := Signal_PossibleRouteHold;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_PossibleStationStartRouteHoldFieldName +
                    ' is ''' + IfThen(Signals[S].Signal_PossibleStationStartRouteHold,
                                      'a possible station start route hold',
                                      'not a possible station start route hold') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_PossibleStationStartRouteHoldFieldName).AsBoolean := Signal_PossibleStationStartRouteHold;
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_QuadrantFieldName + ' is ''' + SignalQuadrantToStr(Signal_Quadrant) + '''');
              SignalsADOTable.Edit;
              IF Signal_Quadrant = NoQuadrant THEN
                SignalsADOTable.FieldByName(Signal_QuadrantFieldName).AsString := ''
              ELSE
                SignalsADOTable.FieldByName(Signal_QuadrantFieldName).AsString := SignalQuadrantToStr(Signal_Quadrant);
              SignalsADOTable.Post;

              Log('S Recording in signal database that S=' + IntToStr(S) + ' ' + Signal_TypeFieldName + ' is ''' + SignalTypeToStr(Signal_Type, LongStringType) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_TypeFieldName).AsString := SignalTypeToStr(Signal_Type, ShortStringType);
              SignalsADOTable.Post;
            END; {WITH}
          END;
          SignalsADOTable.Next;
        END; {WHILE}

        { Tidy up the database }
        SignalsADOTable.Close;
        SignalsADOConnection.Connected := False;
        Log('S Signal Data table and connection closed');
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutSignalDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutSignalDataToDatabase }

FUNCTION GetLineAdjacentSignal(Line : Integer) : Integer;
{ Return the signal nearest the line, except for semaphore distants as they are treated differently }
VAR
  Found : Boolean;
  S : Integer;

BEGIN
  Result := UnknownSignal;
  Found := False;

  S := 0;
  WHILE (S <= High(Signals)) AND NOT Found DO BEGIN
    IF (Signals[S].Signal_Type <> SemaphoreDistant) AND (Signals[S].Signal_AdjacentLine = Line) THEN BEGIN
      Found := True;
      Result := S;
    END ELSE
      Inc(S);
  END; {WHILE}
END; { GetLineAdjacentSignal }

PROCEDURE CalculatePointPositions;
{ Create where the points are on the screen. Note that three-way points must have the first, 'a' point, set to straight before the 'b' point can be set. }
VAR
  DebugStr : String;
  DeltaX, XLength, YLength : LongInt;
  P : Integer;
  UpX, UpY, OtherX, OtherY : Integer;

BEGIN
  TRY
    FOR P := 0 TO High(Points) DO BEGIN
      WITH Points[P] DO BEGIN
        { Mark the position of the heel of the point as UpX and UpY and add the appropriate data to the data for each line to show if there's a point at its start or end }
        UpX := 0;
        IF (Lines[Point_HeelLine].Line_GridUpX = Lines[Point_StraightLine].Line_GridDownX) THEN BEGIN
          UpX := Lines[Point_HeelLine].Line_GridUpX;

          Lines[Point_HeelLine].Line_NextUpType := PointIsNext;
          Lines[Point_HeelLine].Line_NextUpPoint := P;

          Lines[Point_StraightLine].Line_NextDownType := PointIsNext;
          Lines[Point_StraightLine].Line_NextDownPoint := P;

          IF NOT PointIsCatchPoint(P) THEN BEGIN
            Lines[Point_DivergingLine].Line_NextDownType := PointIsNext;
            Lines[Point_DivergingLine].Line_NextDownPoint := P;

            Point_FacingDirection := Up;

            IF Lines[Point_StraightLine].Line_GridDownX <> Lines[Point_DivergingLine].Line_GridDownX THEN BEGIN
              DebugStr := 'Lines do not properly intersect at P=' + IntToStr(P) + ': '
                          + 'DownX for straight line ' + LineToStr(Point_StraightLine) + ' is ' + IntToStr(Lines[Point_StraightLine].Line_GridDownX)
                          + ' and DownX for diverging line ' + LineToStr(Point_DivergingLine) + ' is ' + IntToStr(Lines[Point_DivergingLine].Line_GridDownX)
                          + CRLF
                          + 'Do you wish to continue?';
              IF MessageDialogueWithDefault(DebugStr, StopTimer, mtWarning, [mbOK, mbAbort], mbOK) = mrAbort THEN
                ShutDownProgram(UnitRef, 'CreatePoint');
            END;
          END;
        END ELSE
          IF (Lines[Point_HeelLine].Line_GridDownX = Lines[Point_StraightLine].Line_GridUpX) THEN BEGIN
            UpX := Lines[Point_StraightLine].Line_GridUpX;

            Lines[Point_HeelLine].Line_NextDownType := PointIsNext;
            Lines[Point_HeelLine].Line_NextDownPoint := P;

            Lines[Point_StraightLine].Line_NextUpType := PointIsNext;
            Lines[Point_StraightLine].Line_NextUpPoint := P;

            IF NOT PointIsCatchPoint(P) THEN BEGIN
              Lines[Point_DivergingLine].Line_NextUpType := PointIsNext;
              Lines[Point_DivergingLine].Line_NextUpPoint := P;

              Point_FacingDirection := Down;

              IF Lines[Point_StraightLine].Line_GridUpX <> Lines[Point_DivergingLine].Line_GridUpX THEN BEGIN
                DebugStr := 'Lines do not properly intersect at P=' + IntToStr(P) + ': ' + 'UpX for straight line ' + LineToStr(Point_StraightLine)
                            + ' is ' + IntToStr(Lines[Point_StraightLine].Line_GridUpX) + ' and UpX for diverging line ' + LineToStr(Point_DivergingLine)
                            + ' is ' + IntToStr(Lines[Point_DivergingLine].Line_GridUpX)
                            + CRLF
                            + 'Do you wish to continue?';
                IF MessageDialogueWithDefault(DebugStr, StopTimer, mtWarning, [mbOK, mbAbort], mbOK) = mrAbort THEN
                  ShutDownProgram(UnitRef, 'CreatePoint');
              END;
            END;
          END
          ELSE
            { shouldn't get here }
            Log('X! Failure in creating P=' + IntToStr(P)
                    + ' (' + LineToStr(Point_HeelLine) + '/' + LineToStr(Point_StraightLine) + '/' + LineToStr(Point_DivergingLine) + ')');

        UpY := Lines[Point_StraightLine].Line_GridUpY;

        { Now see if whether the diverging line shares that common point - if it does, swap UpX and DownX }
        IF NOT PointIsCatchPoint(P) THEN BEGIN
          IF UpX = Lines[Point_DivergingLine].Line_GridUpX THEN BEGIN
            OtherX := Lines[Point_DivergingLine].Line_GridDownX;
            OtherY := Lines[Point_DivergingLine].Line_GridDownY;
          END ELSE BEGIN
            OtherX := Lines[Point_DivergingLine].Line_GridUpX;
            OtherY := Lines[Point_DivergingLine].Line_GridUpY;
          END;
        END ELSE BEGIN
          IF Points[P].Point_Type = CatchPointUp THEN BEGIN
            OtherX := Lines[Point_StraightLine].Line_GridUpX - 100;
            OtherY := Lines[Point_StraightLine].Line_GridUpY - 100;
          END ELSE BEGIN
            OtherX := Lines[Point_StraightLine].Line_GridDownX + 100;
            OtherY := Lines[Point_StraightLine].Line_GridDownY + 100;
          END;
        END;

        Point_X := UpX;
        Point_Y := UpY;

        IF OtherX < UpX THEN
          DeltaX := -DeltaPointX
        ELSE
          DeltaX := +DeltaPointX;

        XLength := LongInt(OtherX) - LongInt(UpX);
        YLength := LongInt(OtherY) - LongInt(UpY);
        IF XLength = 0 THEN
          Debug('!Failure in creating P=' + IntToStr(P) + ' - UpX and OtherX are the same');

        Point_FarX := UpX + DeltaX;
        Point_FarY := UpY + (DeltaX * YLength + XLength DIV 2) DIV XLength;

        WITH Point_MouseRect DO BEGIN
          { Set up rectangles for mouse access }
          IF Point_FarY < Point_Y THEN BEGIN
            IF Point_FarX < Point_X THEN BEGIN
              Left := MapGridXToScreenX(Point_FarX);
              Top := MapGridYToScreenY(Point_FarY);
              Right := MapGridXToScreenX(Point_X);
              Bottom := MapGridYToScreenY(Point_Y);
            END ELSE BEGIN
              Left := MapGridXToScreenX(Point_X);
              Top := MapGridYToScreenY(Point_FarY);
              Right := MapGridXToScreenX(Point_FarX);
              Bottom := MapGridYToScreenY(Point_Y);
            END;
          END ELSE BEGIN
            IF Point_FarX < Point_X THEN BEGIN
              Left := MapGridXToScreenX(Point_FarX);
              Top := MapGridYToScreenY(Point_Y);
              Right := MapGridXToScreenX(Point_X);
              Bottom := MapGridYToScreenY(Point_FarY);
            END ELSE BEGIN
              Left := MapGridXToScreenX(Point_X);
              Top := MapGridYToScreenY(Point_Y);
              Right := MapGridXToScreenX(Point_FarX);
              Bottom := MapGridYToScreenY(Point_FarY);
            END;
          END;
        END; {WITH}
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('E : ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CalculatePointPositions }

FUNCTION ValidatePointStraightLineName(LineName : String; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Straight Line name is valid }
BEGIN
  ErrorMsg := '';
  Result := UnknownLine;

  IF LineName = '' THEN
    ErrorMsg := 'ValidatePointStraightLineName: no Straight Line name given'
  ELSE BEGIN
    Result := StrToLine(LineName);
    IF Result = UnknownLine THEN
      ErrorMsg := 'ValidatePointStraightLineName: unknown Straight Line name "' + LineName + '"';
  END;
END; { ValidatePointStraightLineName }

FUNCTION ValidatePointHeelLineName(LineName : String; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Heel Line name is valid }
BEGIN
  ErrorMsg := '';
  Result := UnknownLine;

  IF LineName = '' THEN
    ErrorMsg := 'ValidatePointHeelLineName: no Heel Line name given'
  ELSE BEGIN
    Result := StrToLine(LineName);
    IF Result = UnknownLine THEN
      ErrorMsg := 'ValidatePointHeelLineName: unknown Heel Line name "' + LineName + '"';
  END;
END; { ValidatePointHeelLineName }

FUNCTION ValidatePointDivergingLineName(LineName : String; PointType : TypeOfPoint; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Diverging Line name is valid }
BEGIN
  ErrorMsg := '';
  Result := UnknownLine;

  IF LineName = '' THEN BEGIN
    IF (PointType <> CatchPointUp) AND (PointType <> CatchPointDown) THEN
      ErrorMsg := 'ValidatePointDivergingLineName: no Diverging Line name given'
  END ELSE BEGIN
    Result := StrToLine(LineName);
    IF Result = UnknownLine THEN
      ErrorMsg := 'ValidatePointDivergingLineName: unknown Diverging Line name "' + LineName + '"'
    ELSE
      IF (PointType = CatchPointUp) OR (PointType = CatchPointDown) THEN
        ErrorMsg := 'ValidatePointDivergingLineName: cannot have a Diverging Line if the point type is catch point up or down';
  END;
END; { ValidatePointDivergingLineName }

FUNCTION ValidatePointType(PointTypeStr : String; OUT ErrorMsg : String) : TypeOfPoint;
{ Check that the supplied point type is valid }
BEGIN
  ErrorMsg := '';
  Result := StrToPointType(PointTypeStr);

  IF Result = PointTypeUnknown THEN
    ErrorMsg := 'ValidatePointType: unknown point type: ''' + PointTypeStr;
END; { ValidatePointType }

FUNCTION ValidatePointLastFeedbackStateAsReadIn(PointStateStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : PointStateType;
{ Check whether the last feedback point state read in is valid }
BEGIN
  ErrorMsg := '';
  Result := PointStateUnknown;

  IF PointStateStr <> '' THEN BEGIN
    IF PointManualOperation THEN
      ErrorMsg := 'ValidatePointLastFeedbackStateAsReadIn : last feedback state recorded but point marked as being manually operated'
    ELSE
      Result := StrToPointState(PointStateStr);
  END;
END; { ValidatePointLastFeedbackStateAsReadIn }

FUNCTION ValidatePointLastManualStateAsReadIn(PointStateStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : PointStateType;
{ Check whether the last manual point state read in is valid }
BEGIN
  ErrorMsg := '';
  Result := PointStateUnknown;

  IF PointStateStr <> '' THEN BEGIN
    IF NOT PointManualOperation THEN
      ErrorMsg := 'ValidatePointLastManualStateAsReadIn : last manual state recorded but point not marked as being manually operated'
    ELSE
      Result := StrToPointState(PointStateStr);
  END;
END; { ValidatePointLastManualStateAsReadIn }

FUNCTION ValidatePointLenzNum(LenzNumStr : String; PointLastManualStateAsReadIn : PointStateType; OUT PointManualOperation : Boolean;
                              OUT PointPresentState : PointStateType; OUT ErrorMsg : String) : Integer;
{ Check whether the Lenz point number is valid }
BEGIN
  ErrorMsg := '';

  IF LenzNumStr = '' THEN
    Result := 0
  ELSE
    IF NOT TryStrToInt(LenzNumStr, Result) THEN
      ErrorMsg := 'ValidatePointLenzNum: invalid integer "' + LenzNumStr + '"';

  IF ErrorMsg = '' THEN BEGIN
    { If manual operation is ticked, it overrides the given Lenz data: this is useful if a Lenz point decoder fails (e.g. at 6/3/08) }
    IF NOT PointManualOperation THEN
      PointManualOperation := (Result = 0)
    ELSE BEGIN
      PointManualOperation := True;
      PointPresentState := PointLastManualStateAsReadIn;
    END;
  END;
END; { ValidatePointLenzNum }

FUNCTION ValidatePointLenzUnit(LenzUnitStr : String; OUT ErrorMsg : String) : Integer;
{ Check whether a Lenz point unit is valid }
BEGIN
  ErrorMsg := '';

  IF LenzUnitStr = '' THEN
    Result := 0
  ELSE
    IF NOT TryStrToInt(LenzUnitStr, Result) THEN
      ErrorMsg := 'ValidatePointLenzUnit: invalid integer "' + LenzUnitStr + '"';
END; { ValidatePointLenzUnit }

FUNCTION ValidatePointLenzUnitType(LenzUnitTypeStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : String;
{ Check whether a Lenz point unit type is valid }
BEGIN
  ErrorMsg := '';
  Result := '';

  IF PointManualOperation THEN BEGIN
    IF LenzUnitTypeStr <> '' THEN
      ErrorMsg := 'cannot have a Lenz point unit type if the point is set to manual operation'
  END ELSE
    IF (LenzUnitTypeStr <> 'LS101') AND (LenzUnitTypeStr <> 'LS150') THEN
      ErrorMsg := 'Invalid Lenz point unit type ''' + LenzUnitTypeStr + ''''
    ELSE
      Result := LenzUnitTypeStr;
END; { ValidatePointLenzUnitType }

FUNCTION ValidatePointFeedbackUnit(FeedbackUnitStr : String; OUT PointHasFeedback : Boolean; OUT ErrorMsg : String) : Integer;
{ Check whether the feedback unit exists and is valid }
BEGIN
  ErrorMsg := '';
  Result := 0;
  PointHasFeedback := False;

  IF (FeedbackUnitStr <> '') AND (FeedbackUnitStr <> '0') THEN BEGIN
    IF NOT TryStrToInt(FeedbackUnitStr, Result) THEN
      ErrorMsg := 'ValidatePointFeedbackUnit: invalid integer "' + FeedbackUnitStr + '"'
    ELSE
      PointHasFeedback := True;
  END;
END; { ValidatePointFeedbackUnit }

FUNCTION ValidatePointFeedbackInput(FeedbackInputStr : String; PointHasFeedback : Boolean; OUT ErrorMsg : String) : Integer;
{ Check whether the point feedback input number is valid }
BEGIN
  ErrorMsg := '';

  IF NOT PointHasFeedback THEN
    Result := 0
  ELSE BEGIN
    IF FeedbackInputStr = '' THEN
      Result := 0
    ELSE
      IF NOT TryStrToInt(FeedbackInputStr, Result) THEN
        ErrorMsg := 'ValidatePointFeedbackInput: invalid integer "' + FeedbackInputStr + '"'
      ELSE
        IF (Result < 1) OR (Result > 8) THEN
          ErrorMsg := 'ValidatePointFeedbackInput:  feedback input number ' + IntToStr(Result) + ' is out of range';
  END;
END; { ValidatePointFeedbackInput }

FUNCTION ValidatePointRelatedPoint(P : Integer; RelatedPointStr : String; PointType : TypeOfPoint; OUT ErrorMsg : String) : Integer;
{ Check whether the value of the connected point (if any) is valid }
BEGIN
  ErrorMsg := '';

  IF RelatedPointStr = '' THEN
    Result := UnknownPoint
  ELSE
    IF NOT TryStrToInt(RelatedPointStr, Result) THEN
      ErrorMsg := 'ValidatePointRelatedPoint: invalid integer "' + RelatedPointStr + '"';

  IF ErrorMsg = '' THEN
    IF P = Result THEN
      ErrorMsg := 'ValidatePointRelatedPoint: value of "other point" cannot be the same as the point''s own number';

  IF ErrorMsg = '' THEN BEGIN
    IF (PointType <> OrdinaryPoint) AND (PointType <> CatchPointUp) AND (PointType <> CatchPointDown) AND (Result = UnknownPoint) THEN
      ErrorMsg := 'ValidatePointRelatedPoint: value of "other point" is missing'
    ELSE
      IF (PointType = ProtectedPoint) AND (Result = UnknownPoint) THEN
        ErrorMsg := 'ValidatePointRelatedPoint: value of "other point" is missing'
      ELSE
        IF (PointType = OrdinaryPoint) AND (Result <> UnknownPoint) THEN
          ErrorMsg := 'ValidatePointRelatedPoint: value of "other point" is invalid';
  END;
END; { ValidatePointRelatedPoint }

FUNCTION ValidatePointDefaultState(NewStateStr : String; HeelLine, StraightLine, DivergingLine : Integer; OUT PresentState : PointStateType; OUT ErrorMsg : String)
                                   : PointStateType;
{ Check whether the point state supplied is valid }
BEGIN
  ErrorMsg := '';

  Result := StrToPointState(NewStateStr);
  IF (NewStateStr <> '') AND (Result = PointStateUnknown) THEN
    ErrorMsg := 'ValidatePointDefaultState: invalid state "' + NewStateStr + '"'
  ELSE
    IF Result = PointOutOfAction THEN BEGIN
      { the point doesn't go anywhere }
      Lines[HeelLine].Line_NextUpType := EndOfLineIsNext;
      Lines[HeelLine].Line_NextDownType := EndOfLineIsNext;
      Lines[StraightLine].Line_NextUpType := EndOfLineIsNext;
      Lines[StraightLine].Line_NextDownType := EndOfLineIsNext;
      Lines[DivergingLine].Line_NextUpType := EndOfLineIsNext;
      Lines[DivergingLine].Line_NextDownType := EndOfLineIsNext;
      PresentState := PointOutOfAction;
    END;
END; { ValidatePointDefaultState }

PROCEDURE ReadInPointDataFromDatabase;
{ Create all the points; three numbers in each call are Lenz point number, feedback unit and input number on feedback unit.
  FeedbackOnIsStraight indicates whether feedback is on when point is straight; WiredCorrectly shows if point when on = straight or diverging.

  Next Lenz Point Num:
  Next LS150 is 167; (they go in sixes so need two together or LS110s cannot follow the numbering: 113 & 120 are a pair, 155 and 161, 167 and 173, 179 and 185 are).

  Next LS110 (if any) is 167 or 179; (they go in fours). (32, 83 not used).

  Points currently not responding: 37-41 [??], 127-129 (route to new station not yet installed),
  133-136 [not yet installed - temporarily in use - see note of 12/4/09 below], 138 [failed], 141-142 [failed], 146 [failed]

  temporarily rearranging feedback below FY 11-12/3/08:
    pt 001 - L=17 F=107-2 On WR
    pt 112 - L=9 F=107-6 - WR
    pt 134 - L=137 F=109-5 On -
    pt 135 - L=148 F=109-6 - -
    pt 129 - L=12 F=107-3 On WR

  another temporary fix after failure of an LS110  12/4/09 (no idea what last year's fix was for but it would appear to be permanent!)
    pt 47  - was 53 - 14 - 85 - 5 - on - on
    pt 53  - was 55 - 14 - 85 - 8 - off - off
    pt 55  - was 56 - 14 - 85 - 6 - off - off
    pt 109 - was 54 - 14 - 85 - 7 - off - off

  To set up points to appear properly on screen, first set Wiring Reversed and OnIsStraight to False. Then adjust each physical point to be straight. Any points that do
  not appear straight on screen set OnIsStraight to True. Then test each point via the screen, and any that do not switch as planned set WiringReversed to True. All should
  then work properly! (FWP 17/6/09)
}
CONST
  StopTimer = True;

VAR
  Bookmark : TBookmark;
  ErrorMsg : String;
  LastManualStateStr : String;
  NextInDatabaseP : Integer;
  P : Integer;
  TempPointNumber : Integer;

BEGIN
  TRY
    Log('A INITIALISING POINTS {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Point database file "' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInPointDataFromDatabase')
        ELSE
          Exit;
      END;

      PointsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix
                                              + ';Persist Security Info=False';
      PointsADOConnection.Connected := True;
      PointsADOTable.Open;
      Log('P Point data table and connection opened to initialise the point data');

      { First see if the point numbers in the MSAccess file are sequential and, if not, renumber it - we need this or deletions from the MSAccess file will cause problems }
      P := -1;
      PointsADOTable.Sort := '[' + Point_NumberFieldName + '] ASC';
      PointsADOTable.First;
      WHILE NOT PointsADOTable.EOF DO BEGIN
        Inc(P);
        NextInDatabaseP := PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger;
        IF NextInDatabaseP <> P THEN BEGIN
          TRY
            Bookmark := PointsADOTable.GetBookmark;
            { firstly see if any points cross-refer to the missing point at P, and note the fact }
            PointsADOTable.First;
            { at the start of the database }
            WHILE NOT PointsADOTable.EOF DO BEGIN
              IF PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsInteger = P THEN BEGIN
                Log('A! P=' + IntToStr(PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger) + '''s Related Point refers to '
                        + ' P=' + IntToStr(PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsInteger)
                        + ' which did not exist prior to point renumbering - Related Point has been set to unknown');
                PointsADOTable.Edit;
                PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsInteger := UnknownPoint;
                PointsADOTable.Post;
              END;
              PointsADOTable.Next;
            END; {WHILE}

            { secondly we have to renumber any points that cross-refer to this point - allocate memory and assign where we are in the database to the bookmark }
            PointsADOTable.First;
            { at the start of the database }
            WHILE NOT PointsADOTable.EOF DO BEGIN
              IF InitVarsWindow.PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsInteger = NextInDatabaseP THEN BEGIN
                IF NextInDatabaseP = P THEN
                  { this shouldn't happen }
                  Log('A! Related Point is ' + IntToStr(NextInDatabaseP) + ' is the same as P' + IntToStr(P));
                PointsADOTable.Edit;
                PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsInteger := P;
                PointsADOTable.Post;
              END;
              PointsADOTable.Next;
            END; {WHILE}
          FINALLY
            { and return whence we came }
            PointsADOTable.GotoBookmark(Bookmark);
          END;

          { and now we need to renumber this (and in due course all subsequent) entries }
          PointsADOTable.Edit;
          PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger := P;
          PointsADOTable.Post;
        END;
        PointsADOTable.Next;
      END; {WHILE}

      P := -1;
      PointsADOTable.Sort := '[' + Point_NumberFieldName + '] ASC';
      PointsADOTable.First;

      SetLength(Points, 0);
      WHILE NOT PointsADOTable.EOF DO BEGIN
        ErrorMsg := '';
        LastManualStateStr := '';
        Inc(P);

        TempPointNumber := PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger;
        IF PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger <> TempPointNumber THEN
          ErrorMsg := 'it does not match the point number in the database (' + IntToStr(TempPointNumber) + ')'
        ELSE
          SetLength(Points, Length(Points) + 1);

        WITH Points[P] DO BEGIN
          Point_AwaitingManualChange := False;
          Point_DataChanged := False;
          Point_Energised := False;
          Point_EnergisedTime := 0;
          Point_FeedbackPending := False;
          Point_FeedbackPendingMsgWritten := False;
          Point_ForcedDelayMsg1Written := False;
          Point_ForcedDelayMsg2Written := False;
          Point_LastChangedTime := 0;
          Point_LocoChipLockingTheRoute := UnknownLocoChip;
          Point_LockedByUser := False;
          Point_LockFailureNotedInLocksUnit := False;
          Point_LockFailureNotedInSubRouteUnit := False;
          Point_LockingState := PointStateUnknown;
          Point_MaybeBeingSetToManual := False;
          Point_MovedWhenLocked := False;
          Point_Number := TempPointNumber;
          Point_PresentState := PointStateUnknown;
          Point_RequiredState := PointStateUnknown;
          Point_ResettingTime := 0;
          Point_RouteLockedByLocoChip := UnknownRoute;
          Point_SecondAttempt := False;
          Point_SetASecondTime := False;
          Point_WaitTime := 0;

          SetLength(Point_LockingArray, 0);

          IF ErrorMsg = '' THEN
            Point_HeelLine := ValidatePointHeelLineName(PointsADOTable.FieldByName(Point_HeelLineFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_TCAtHeel := Lines[Point_HeelLine].Line_TC;

          IF ErrorMsg = '' THEN
            Point_StraightLine := ValidatePointStraightLineName(PointsADOTable.FieldByName(Point_StraightLineFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_Type := ValidatePointType(PointsADOTable.FieldByName(Point_TypeFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_DivergingLine := ValidatePointDivergingLineName(PointsADOTable.FieldByName(Point_DivergingLineFieldName).AsString, Point_Type, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_LockedByUser := PointsADOTable.FieldByName(Point_LockedByUserFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_OutOfUse := PointsADOTable.FieldByName(Point_OutOfUseFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_ManualOperation := PointsADOTable.FieldByName(Point_ManualOperationFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_LastFeedbackStateAsReadIn := ValidatePointLastFeedbackStateAsReadIn(PointsADOTable.FieldByName(Point_LastFeedbackStateAsReadInFieldName).AsString,
                                                                                      Point_ManualOperation, ErrorMsg);
          IF ErrorMsg = '' THEN
            Point_LastManualStateAsReadIn := ValidatePointLastManualStateAsReadIn(PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString,
                                                                                  Point_ManualOperation, ErrorMsg);
          IF ErrorMsg = '' THEN
            Point_LenzNum := ValidatePointLenzNum(PointsADOTable.FieldByName(Point_LenzNumFieldName).AsString, Point_LastManualStateAsReadIn, Point_ManualOperation,
                                                  Point_PresentState, ErrorMsg);
          IF ErrorMsg = '' THEN
            Point_LenzUnit := ValidatePointLenzUnit(PointsADOTable.FieldByName(Point_LenzUnitFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_LenzUnitType := ValidatePointLenzUnitType(PointsADOTable.FieldByName(Point_LenzUnitTypeFieldName).AsString, Point_ManualOperation, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_FeedbackUnit := ValidatePointFeedbackUnit(PointsADOTable.FieldByName(Point_FeedbackUnitFieldName).AsString, Point_HasFeedback, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_FeedbackInput := ValidatePointFeedbackInput(PointsADOTable.FieldByName(Point_FeedbackInputFieldName).AsString, Point_HasFeedback, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_FeedbackOnIsStraight := PointsADOTable.FieldByName(Point_FeedbackOnIsStraightFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_WiringReversedFlag := PointsADOTable.FieldByName(Point_WiringReversedFlagFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_RelatedPoint := ValidatePointRelatedPoint(P, PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsString, Point_Type, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_Notes := PointsADOTable.FieldByName(Point_NotesFieldName).AsString;

          IF ErrorMsg = '' THEN
            Point_DefaultState := ValidatePointDefaultState(PointsADOTable.FieldByName(Point_DefaultStateFieldName).AsString, Point_HeelLine, Point_StraightLine,
                                                            Point_DivergingLine, Point_PresentState, ErrorMsg);
          IF ErrorMsg = '' THEN
            Point_LockedIfHeelTCOccupied := PointsADOTable.FieldByName(Point_LockedIfHeelTCOccupiedFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_LockedIfNonHeelTCsOccupied := PointsADOTable.FieldByName(Point_LockedIfNonHeelTCsOccupiedFieldName).AsBoolean;

          IF ErrorMsg = '' THEN BEGIN
            IF Point_ManualOperation THEN BEGIN
              IF Point_LastManualStateAsReadIn <> PointStateUnknown THEN
                Point_RequiredState := Point_LastManualStateAsReadIn
              ELSE
                Point_RequiredState := Point_DefaultState;
            END ELSE
              Point_RequiredState := Point_DefaultState;
          END;

          IF ErrorMsg <> '' THEN BEGIN
            IF MessageDialogueWithDefault('Error in creating P=' + IntToStr(P) + ': '
                                          + ErrorMsg
                                          + CRLF
                                          + 'Do you wish to continue?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
            THEN
              ShutDownProgram(UnitRef, 'ReadInPointDataFromDatabase');
          END;

          PointsADOTable.Next;
        END; {WITH}
      END; {WHILE}

      { Tidy up the database }
      PointsADOTable.Close;
      PointsADOConnection.Connected := False;
      Log('P Point data table and connection closed');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInPointDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}

  CalculatePointPositions;
END; { ReadInPointDataFromDatabase }

FUNCTION DeleteRecordFromPointDatabase(PointToDeleteNum : Integer) : Boolean;
{ Remove a record from the point database }
BEGIN
  Result := False;
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Point database file "' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'DeleteRecordFromPointDatabase')
        ELSE
          Exit;
      END;

      PointsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix
                                               + ';Persist Security Info=False';
      PointsADOConnection.Connected := True;
      PointsADOTable.Open;
      IF NOT PointsADOTable.Locate(Point_NumberFieldName, IntToStr(PointToDeleteNum), []) THEN BEGIN
        Log('P Point data table and connection opened to delete P' + IntToStr(PointToDeleteNum) + ' but it cannot be found');
      END ELSE BEGIN
        Log('P Point data table and connection opened to delete P' + IntToStr(PointToDeleteNum));

        { Now delete the point - we have already checked, in the Edit unit, whether deleting it will cause knock-on problems with other Points }
        PointsADOTable.Delete;
        Log('PG P' + IntToStr(PointToDeleteNum) + ' has been deleted');
        Result := True;
      END;

      { Tidy up the database }
      PointsADOTable.Close;
      PointsADOConnection.Connected := False;
      Log('S Point Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToPointDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DeleteRecordFromPointDatabase }

PROCEDURE AddNewRecordToPointDatabase;
{ Append a record to the point database }
BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Point database file "' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'AddNewRecordToPointDatabase')
        ELSE
          Exit;
      END;

      PointsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix
                                               + ';Persist Security Info=False';
      PointsADOConnection.Connected := True;
      PointsADOTable.Open;
      PointsADOTable.Append;
      PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger := High(Points);
      PointsADOTable.Post;

      Log('S Point data table and connection opened to write out point data that has changed');
      { Tidy up the database }
      PointsADOTable.Close;
      PointsADOConnection.Connected := False;
      Log('S Point Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToPointDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { AddNewRecordToPointDatabase }

PROCEDURE WriteOutPointDataToDatabase;
{ If a point's data has been changed, record it in the database }
VAR
  P : Integer;
  PointDatabaseNeedsUpdating : Boolean;
  TempStr : String;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      PointDatabaseNeedsUpdating := False;

      { First see if we need to open the database }
      P := 0;
      WHILE (P <= High(Points)) AND NOT PointDatabaseNeedsUpdating DO BEGIN
        WITH Points[P] DO BEGIN
          IF Point_DataChanged THEN
            PointDatabaseNeedsUpdating := True;

          IF Point_ManualOperation AND (Point_LastManualStateAsReadIn <> Point_PresentState) THEN
            PointDatabaseNeedsUpdating := True;
          IF NOT Point_ManualOperation AND (Point_LastManualStateAsReadIn <> PointStateUnknown) THEN
            PointDatabaseNeedsUpdating := True;

          IF NOT Point_ManualOperation AND (Point_LastFeedbackStateAsReadIn <> Point_PresentState) THEN
            PointDatabaseNeedsUpdating := True;
        END; {WITH}
        Inc(P);
      END; {WHILE}

      IF PointDatabaseNeedsUpdating THEN BEGIN
        IF NOT FileExists(PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix) THEN BEGIN
          IF MessageDialogueWithDefault('Point database file "' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix + '" cannot be located'
                                        + CRLF
                                        + 'Do you wish to continue?',
                                        StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
          THEN
            ShutDownProgram(UnitRef, 'WriteOutPointDataToDatabase')
          ELSE
            Exit;
        END;

        PointsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix
                                                + ';Persist Security Info=False';
        PointsADOConnection.Connected := True;
        PointsADOTable.Open;
        PointsADOTable.Edit;
        Log('P Point data table and connection opened to write out point data that has changed');

        PointsADOTable.First;
        WHILE NOT PointsADOTable.EOF DO BEGIN
          P := PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger;
          WITH Points[P] DO BEGIN
            IF Point_DataChanged THEN BEGIN
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_NumberFieldName).AsString := IntToStr(Point_Number);
              PointsADOTable.Post;
              IF PointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + ' is now ' + IntToStr(Point_Number));

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_OutOfUseFieldName).AsBoolean := Point_OutOfUse;
              PointsADOTable.Post;
              IF PointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + ' is now ' + IfThen(Points[P].Point_OutOfUse, 'out of use', 'back in use'));

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_LockedByUserFieldName).AsBoolean := Point_LockedByUser;
              PointsADOTable.Post;
              IF PointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + ' is now ' + IfThen(Points[P].Point_LockedByUser, 'locked by user', 'back in use'));

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsString := IntToStr(Point_RelatedPoint);
              PointsADOTable.Post;
              IF PointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Related Point is now ' + IntToStr(Points[P].Point_RelatedPoint));

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_TypeFieldName).AsString := PointTypeToStr(Point_Type);
              PointsADOTable.Post;
              IF PointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Point Type is now ' + PointTypeToStr(Points[P].Point_Type));
            END;

            IF NOT Point_ManualOperation AND (Point_LastManualStateAsReadIn <> PointStateUnknown) THEN BEGIN
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString := '';
              PointsADOTable.Post;
            END
              ELSE
                IF (Point_ManualOperation) AND (Point_LastManualStateAsReadIn <> Point_PresentState) THEN BEGIN
                  PointsADOTable.Edit;
                  IF Points[P].Point_PresentState = PointStateUnknown THEN
                    PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString := ''
                  ELSE
                    IF Points[P].Point_PresentState = Straight THEN
                      PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString := 'Straight'
                    ELSE
                      PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString := 'Diverging';
                  PointsADOTable.Post;
                  IF PointDebuggingMode THEN
                    Log('P Recording in point database that manual P=' + IntToStr(P) + '''s state is now ' + PointStateToStr(Points[P].Point_PresentState));
                END;

            { And of points from which we've had feedback }
            IF NOT Point_ManualOperation THEN BEGIN
              PointsADOTable.Edit;
              IF Points[P].Point_PresentState = PointStateUnknown THEN
                PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString := ''
              ELSE
                IF Points[P].Point_PresentState = Straight THEN
                  PointsADOTable.FieldByName(Point_LastFeedbackStateAsReadInFieldName).AsString := 'Straight'
                ELSE
                  PointsADOTable.FieldByName(Point_LastFeedbackStateAsReadInFieldName).AsString := 'Diverging';
              PointsADOTable.Post;
              IF PointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s feedback state is now ' + PointStateToStr(Points[P].Point_PresentState));
            END;
          END; {WITH}
          PointsADOTable.Next;
        END; {WHILE}

        { Tidy up the database }
        PointsADOTable.Close;
        PointsADOConnection.Connected := False;
        Log('P Point data table and connection closed');
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutPointDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutPointDataToDatabase }

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
  F : Integer;
  FirstFeedbackUnit : Integer;
  Input : Integer;
  LastFeedbackUnit : Integer;
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
      SetLength(FeedbackUnitData, 0);
      FirstFeedbackUnit := 99999;
      LastFeedbackUnit := 0;

      WHILE NOT FeedbackUnitsADOTable.EOF DO BEGIN
        WITH FeedbackUnitsADOTable DO BEGIN
          SetLength(FeedbackUnitData, Length(FeedbackUnitData) + 1);
          F := High(FeedbackUnitData);
          WITH FeedbackUnitData[F] DO BEGIN
            ErrorMsg := '';

            FieldName := 'Unit';
            Feedback_Unit := FieldByName(FieldName).AsInteger;
            IF Feedback_Unit < FirstFeedbackUnit THEN
              FirstFeedbackUnit := Feedback_Unit;
            IF Feedback_Unit > LastFeedbackUnit THEN
              LastFeedbackUnit := Feedback_Unit;

            FieldName := 'Type';
            IF FieldByName(FieldName).AsString = '' THEN
              ErrorMsg := 'missing feedback type'
            ELSE BEGIN
              Feedback_Type := StrToFeedbackUnitType(FieldByName(FieldName).AsString);
              IF Feedback_Type = UnknownFeedbackDetectorType THEN
                ErrorMsg := 'unknown feedback type';
            END;

            IF ErrorMsg = '' THEN BEGIN
              IF Feedback_Type = MixedFeedbackDetectors THEN BEGIN
                { the inputs on the unit are mixed, i.e. they come from different detector types }
                Input := 1;
                WHILE (Input <= 8) AND (ErrorMsg = '') DO BEGIN
                  FieldName := 'Input' + IntToStr(Input) + 'Type';
                  IF FieldByName(FieldName).AsString = '' THEN
                    ErrorMsg := 'missing feedback type for Input' + IntToStr(Input) + 'Type'
                  ELSE BEGIN
                    Feedback_InputTypeArray[Input] := StrToFeedbackUnitType(FieldByName(FieldName).AsString);
                    IF Feedback_InputTypeArray[Input] = UnknownFeedbackDetectorType THEN
                      ErrorMsg := 'unknown feedback type for Innput' + IntToStr(Input) + 'Type';
                  END;
                  Inc(Input);
                END; {WHILE}
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'TCAbove';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Feedback_TCAboveUnit := UnknownTrackCircuit
              ELSE
                IF NOT TryStrToInt(TempStr, Feedback_TCAboveUnit) THEN
                  ErrorMsg := 'invalid integer ''' + TempStr + ''' in TCAbove field';
            END;

            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating Feedback Detector=' + IntToStr(Feedback_Unit) + ': '
                                            + '[' + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'InitialiseFeedback');
            END;

          END; {WITH}
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
    IF TestingMode THEN
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
VAR
  I : Integer;

BEGIN
  FOR I := FirstFunctionDecoder TO LastFunctionDecoder DO
    FunctionDecoderBytes[I] := 0;
  FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO
    FeedbackUnitInUseArray[I] := False;

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

  DefaultDisplayColoursWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultDisplayColoursWindowTop := MulDiv(Screen.WorkAreaHeight, 500, 1000);
  DefaultDisplayColoursWindowLeft := 0;
  DefaultDisplayColoursWindowWidth := MulDiv(Screen.WorkAreaHeight, 200, 1000);

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
