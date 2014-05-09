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
    FeedbackUnitDataADOConnection : TADOConnection;
    FeedbackUnitDataADOTable : TADOTable;
    FeedbackUnitDataSource : TDataSource;
    LineDataSource : TDataSource;
    LineDataADOConnection : TADOConnection;
    LineDataADOTable : TADOTable;
    LocationsADOConnection : TADOConnection;
    LocationsADOTable : TADOTable;
    LocationsDataSource : TDataSource;
    PlatformDataADOConnection : TADOConnection;
    PlatformDataADOTable : TADOTable;
    PlatformDataSource : TDataSource;
    PointsADOConnection : TADOConnection;
    PointsADOTable : TADOTable;
    PointsDataSource : TDataSource;
    PointsADOTable2 : TADOTable;
    PointsADOConnection2 : TADOConnection;
    PointsDataSource2 : TDataSource;
    SaveDialogue : TSaveDialog;
    SignalsADOConnection : TADOConnection;
    SignalsADOTable : TADOTable;
    SignalsDataSource : TDataSource;
    SignalsDataSource2 : TDataSource;
    SignalsADOTable2 : TADOTable;
    SignalsADOConnection2 : TADOConnection;
    TrackCircuitDataADOConnection : TADOConnection;
    TrackCircuitDataADOTable : TADOTable;
    TrackCircuitDataSource : TDataSource;
    UnderwayOrCompleted1Label : TLabel;
    UnderwayOrCompleted2Label : TLabel;

    StationMonitorsWebDiagnosticsMemo: TMemo;
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
  clFWPDkGrey = 2368548; { 0 36 36 36 }
  clFWPVeryDkGrey = 3355443; { 0  33  33  33 }

  FirstFunctionDecoder = 9001;
  LastFunctionDecoder = 9916;

  FirstFeedbackUnit = 66; { could be 65, but if a feedback unit resets itself, it reverts to 65, and thus causes an error }
  LastFeedbackUnit = 126; { ************** replace now feedback data is read in from the database }

  FirstRouteCreationHeldMsgNumber = 1;
  LastRouteCreationHeldMsgNumber = 12;

  ByUser = True;
  ForceARead = True;
  ForceAWrite = True;
  ForcePoint = True;
  HelpRequired = True;
  Highlight = True;
  QuickStop = True;
  StopTimer = True;
  TrainExists = True;
  UnknownArea = 99999;
  UnknownBufferStop = 99999;
  UnknownJourney = 99999;
  UnknownLine = 99999;
  UnknownLocation = 99999;
  UnknownLocoChip = 0;
  UnknownLocoClass = 99999;
  UnknownSubRoute = 99999;
  UnknownPoint = 99999;
  UnknownRoute = 99999;
  UnknownSignal = 99999;
  UnknownTrackCircuit = 99999;
  UnknownTrainLength = 99999;
  UnknownTrainTypeNum = 99999;
  NoTC = UnknownTrackCircuit;

  NoLocoChip = UnknownLocoChip;
  NoRoute = UnknownRoute;
  NoSubRoute = UnknownSubRoute;

  { The status bar panels }
  StatusBarPanel0 = 0;
  StatusBarPanel1 = 1;
  StatusBarPanel2 = 2;

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

  { make these changeable in due course *** }
  ClearLineString = '[clear line string]';
  UnknownAreaStr = '[unknown area]';
  UnknownColourStr = 'Unknown Colour';
  UnknownLineStr = '[unknown line]';
  UnknownLocationStr = '[unknown location]';
  UnknownTrainStatusStr = '[unknown train status]';
  UnknownTrainTypeStr = '[unknown train type]';
  UnknownTypeStr = '[unknown type]';

  HoldMarker = 'HOLD';

  SetCurrentRailwayTimeCaption = 'Set Current Railway Time';
  SetProgramStartTimeCaption = 'Set Program Start Time';
  SetDaylightStartTimeCaption = 'Set Daylight Start Time';
  SetDaylightEndTimeCaption = 'Set Daylight End Time';

TYPE
  TColour = TColor;
  TColourDialogue = TColorDialog;

  BooleanArrayType = ARRAY OF Boolean;
  DateTimeArrayType = ARRAY OF TDateTime;
  IntegerArrayType = ARRAY OF Integer;
  StringArrayType = ARRAY OF String;

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

  InfoRec = RECORD
    LocoChip : Integer;
    InfoString : String;
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

  { Line-related type declarations }
  EndOfLineType = (BufferStopAtUp, BufferStopAtDown, ProjectedLineAtUp, ProjectedLineAtDown, NotEndOfLine, UnknownEndOfLine);

  TypeOfLine = (MainOrGoods, MainLine, GoodsLine, BranchLineDouble, BranchLineSingle, IslandStationLine, MainStationLine, BranchStationLine, WindowStationLine, SidingLine,
                FiddleyardLine, SidingsApproach, StationAvoiding, ProjectedLine, UnknownTypeOfLine);
  { adjust FirstTypeOfLine or LastTypeOfLine if alteration made to above declaration }

  GradientType = (Level, RisingIfUp, RisingIfDown, UnknownGradientType);
  MPHType = (MPH0, MPH10, MPH20, MPH30, MPH40, MPH50, MPH60, MPH70, MPH80, MPH90, MPH100, MPH110, MPH120, UnknownMPH, NoSpecifiedSpeed);
  NextLineRouteingType = (EndOfLineIsNext, LineIsNext, PointIsNext, UnknownNextLineRouteingType);
  OutOfUseState = (InUse, OutOfUse);
  PointStateType = (Diverging, Straight, PointOutOfAction, PointStateUnknown);

  LineRec = RECORD
    Line_AdjacentBufferStop : Integer;
    Line_BufferStopNum : Integer;
    Line_BufferStopTheatreDestinationStr : String;
    Line_CurrentColour : TColour;
    Line_Direction : DirectionType;
    Line_DownConnectionCh : String;
    Line_DownConnectionChRect : TRect;
    Line_DownConnectionChBold : Boolean;
    Line_DownXAbsolute : Integer;
    Line_DownX : Integer;
    Line_DownYAbsolute : Integer;
    Line_DownY : Integer;
    Line_DownYLocation : Integer;
    Line_DownYLocationStr : String;
    Line_EndOfLineMarker : EndOfLineType;
    Line_Gradient : GradientType;
    Line_InitialOutOfUseState : OutOfUseState;
    Line_InUseFeedbackUnit : Integer;
    Line_InUseFeedbackInput : Integer;
    Line_Length : Integer;
    Line_Location : Integer;
    Line_LockFailureNotedInSubRouteUnit : Boolean;
    Line_MousePolygon : ARRAY [0..4] OF TPoint; { mouse access for indicators }
    Line_NextDownIsEndOfLine : EndOfLineType;
    Line_NextDownLine : Integer;
    Line_NextDownPoint : Integer;
    Line_NextDownType : NextLineRouteingType;
    Line_NextUpIsEndofLine : EndOfLineType;
    Line_NextUpLine : Integer;
    Line_NextUpPoint : Integer;
    Line_NextUpType : NextLineRouteingType;
    Line_NoLongerOutOfUse : Boolean;
    Line_OldColour : TColour;
    Line_OutOfUseState : OutOfUseState;
    Line_RoutedOver : Boolean;
    Line_RouteLockingForDrawing : Integer; { used for drawing those bits of Line that are routed over }
    Line_RouteSet : Integer;
    Line_SaveOutOfUseState : OutOfUseState;
    Line_Str : String;
    Line_TC : Integer;
    Line_TempNum : Integer;
    Line_TypeOfLine : TypeOfLine;
    Line_UpConnectionCh : String;
    Line_UpConnectionChRect : TRect;
    Line_UpConnectionChBold : Boolean;
    Line_UpXAbsolute : Integer;
    Line_UpX : Integer;
    Line_UpXLineStr : String;
    Line_UpXValueSpecified : Boolean;
    Line_UpYAbsolute : Integer;
    Line_UpY : Integer;
    Line_UpYLocation : Integer;
    Line_UpYLocationStr : String;
  END;

  LightsType = (NoLights, HeadlightsAndTailLightsConnected, HeadlightsAndTailLightsSeparatelySwitched, ExpressModelsSeparateHeadlights, LightsOperatedByTwoChips,
                LightsShouldBeDimmed, CustomLightingKit);
  LightsColourType = (Red, White);

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
    Location_LongStr : String;
    Location_OutOfUse : Boolean;
    Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyard : Integer;
    Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr : String; { to store the data for later test }
    Location_PlatformOrFiddleyardAtUp : Integer;
    Location_PlatformOrFiddleyardAtDown : Integer;
    Location_PlatformOrFiddleyardDirection : DirectionType;
    Location_PlatformOrFiddleyardNumStr : String;
    Location_PlatformPriority : Integer;
    Location_RecordInLocationOccupationArray : Boolean;
    Location_ShortStr : String;
    Location_ThroughLocationState : ThroughLocationStateType;
    Location_ThroughOrStoppingPriority : ThroughOrStoppingPriorityType;
    Location_TrainPriority : TrainPriorityType;
    Location_TRSPlungerX : Integer;
    Location_TRSPlungerY : Integer;
    Location_Y : Integer;
    Location_YLocationStr : String;
    Location_YLocation : Integer;
    Location_YLocationAdjustment : Integer;
    Location_YScaled : Integer;
  END;

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
    Platform_LocationAbovePlatform : Integer; { do we need these as part of the record? **** }
    Platform_LocationBelowPlatform : Integer;
    Platform_NumberAStr : String;
    Platform_NumberBStr : String;
    Platform_NumberPositionA : PlatformNumberPositionType;
    Platform_NumberPositionB : PlatformNumberPositionType;
    Platform_Rect : TRect;
    Platform_RightLine : Integer;
    Platform_RightLineAdjustment : Integer;
  END;

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
    Point_DivergingLine : Integer;
    Point_HeelLine : Integer;
    Point_StraightLine : Integer;

    Point_AwaitingManualChange : Boolean;
    Point_DataChanged : Boolean;
    Point_DefaultState : PointStateType;
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
    Point_LastChangedTime : TDateTime;
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
    Point_ManualOperation : Boolean;
    Point_ManualStateAsReadIn : PointStateType;
    Point_MaybeBeingSetToManual : Boolean;
    Point_MouseRect : TRect; { mouse access rectangle }
    Point_MovedWhenLocked : Boolean;
    Point_Notes : String;
    Point_OtherPoint : Integer;
    Point_OutOfUse : Boolean;
    Point_PresentState : PointStateType;
    Point_PreviousState : PointStateType;
    Point_RequiredState : PointStateType;
    Point_ResettingTime : TDateTime;
    Point_RouteLockedByLocoChip : Integer;
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
  Point_NumberFieldName : String = 'PointNum';

  Point_DefaultStateFieldName : String = 'DefaultState';
  Point_DivergingLineFieldName : String = 'DivergingLine';
  Point_FeedbackInputFieldName : String = 'FeedbackInput';
  Point_FeedbackOnIsStraightFieldName : String = 'OnIsStraight';
  Point_FeedbackUnitFieldName : String = 'FeedbackUnit';
  Point_HeelLineFieldName : String = 'HeelLine';
  Point_LenzNumFieldName : String = 'LenzPointNum';
  Point_LenzUnitFieldName : String = 'LenzPointUnit';
  Point_LenzUnitTypeFieldName : String = 'LenzPointUnitType';
  Point_LockedByUserFieldName : String = 'LockedByUser';
  Point_LockedIfHeelTCOccupiedFieldName : String = 'LockedIfHeelTCOccupied';
  Point_LockedIfNonHeelTCsOccupiedFieldName : String = 'LockedIfNonHeelTCsOccupied';
  Point_ManualOperationFieldName : String = 'ManualOperation';
  Point_ManualStateAsReadInFieldName : String = 'LastManualState';
  Point_NotesFieldName : String = 'Notes';
  Point_OtherPointFieldName : String = 'OtherPoint';
  Point_OutOfUseFieldName : String = 'OutOfUse';
  Point_StraightLineFieldName : String = 'StraightLine';
  Point_TypeFieldName : String = 'PointType';
  Point_WiringReversedFlagFieldName : String = 'WiringReversed';

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
    Signal_AdjacentTC : Integer;
    Signal_ApproachControlAspect : AspectType;
    Signal_ApproachLocked : Boolean;
    Signal_AsTheatreDestination : String; { what a signal pointing at this signal might display }
    Signal_Automatic : Boolean; { not yet implemented }
    Signal_DataChanged : Boolean;
    Signal_DecoderNum : Integer;
    Signal_Direction : DirectionType;
    Signal_DistantHomesArray : IntegerArrayType; { needed to tell a semaphore distant which semaphore homes lock it }
    Signal_Energised : Boolean;
    Signal_EnergisedTime : TDateTime;
    Signal_FailedToResetFlag : Boolean;
    Signal_FailMsgWritten : Boolean;
    Signal_FindNextSignalBufferStopMsgWritten : Boolean;
    Signal_HiddenAspect : AspectType; { used to force stopping at stations where signal is potentially off }
    Signal_Indicator : IndicatorType;
    Signal_IndicatorDecoderNum : Integer;
    Signal_IndicatorDecoderFunctionNum : Integer;
    Signal_IndicatorMouseRect : TRect; { mouse access rectangle for indicators }
    Signal_IndicatorSpeedRestriction : MPHType; { applicable only if the route indicator is set }
    Signal_IndicatorState : IndicatorStateType;
    Signal_JunctionIndicators : ARRAY [JunctionIndicatorType] OF JunctionIndicatorRec;
    Signal_LampIsOn : Boolean; { used for flashing aspects }
    Signal_LocationX : Integer;
    Signal_LocationY : Integer;
    Signal_LocationsToMonitorArray : IntegerArrayType;

    { Signal_LockedArray and Signal_RouteLockingNeededArray sound similar but serve different purposes - RouteLockingNeededArray covers the lines, track circuits, points,
      etc. ahead that must be locked before a signal can be pulled off; Signal_LockedArray shows whether a signal is locked either by a specific route or by a user.
    }
    Signal_LockedArray : StringArrayType;

    Signal_LockFailureNotedInRouteUnit : Boolean;
    Signal_MouseRect : TRect; { mouse access rectangle for signal }
    Signal_NextSignalIfNoIndicator : Integer;
    Signal_NotUsedForRouteing : Boolean;
    Signal_Notes : String;
    Signal_OppositePassingLoopSignal : Integer;
    Signal_OutOfUse : Boolean;
    Signal_OutOfUseMsgWritten : Boolean;
    Signal_PossibleRouteHold : Boolean;
    Signal_PossibleStationStartRouteHold : Boolean;
    Signal_PreviousAspect : AspectType;
    Signal_PreviousIndicatorState : IndicatorStateType;
    Signal_PreviousTheatreIndicatorString : String;
    Signal_PreviousSignal1 : Integer;
    Signal_PreviousSignal2 : Integer;
    Signal_PreviousHiddenAspectSignal1 : Integer;
    Signal_PreviousHiddenAspectSignal2 : Integer;
    Signal_PostColour : TColour;
    Signal_PostMouseRect : TRect; { mouse access rectangle for signal posts }
    Signal_Quadrant : QuadrantType;
    Signal_ResettingTC : Integer;

    { see note above for Signal_LockedArray }
    Signal_RouteLockingNeededArray : StringArrayType;

    Signal_StateChanged : Boolean;
    Signal_TheatreIndicatorString : String; { what this signal might display }
    Signal_TRSHeld : Boolean;
    Signal_TRSHeldMsgWritten : Boolean;
    Signal_TRSReleased : Boolean;
    Signal_TRSReleasedMsgWritten : Boolean;
    Signal_Type : TypeOfSignal;
    Signal_VerticalSpacing : Integer;
    Signal_XAdjustment : Integer;
  END;

  WriteReadType = (ReadOnly, WriteOnly, WriteThenRead);

CONST
  Signal_NumberFieldName : String = 'Signal Number';

  Signal_AccessoryAddressFieldName : String = 'Signal Accessory Address';
  Signal_AdjacentLineFieldName : String = 'Signal Adjacent Line';
  Signal_ApproachControlAspectFieldName : String = 'Signal Approach Control Aspect';
  Signal_AsTheatreDestinationFieldName : String = 'Signal As Theatre Destination';
  Signal_AutomaticFieldName : String = 'Signal Automatic'; { not in use }
  Signal_DirectionFieldName : String = 'Signal Direction';
  Signal_DecoderNumFieldName : String = 'Signal Decoder Num';
  Signal_DistantHomesArrayFieldName : String = 'Signal Distant Homes';
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
  Signal_OppositePassingLoopSignalFieldName : String = 'Signal Opposite Passing Loop Signal';
  Signal_OutOfUseFieldName : String = 'Signal Out Of Use';
  Signal_PossibleRouteHoldFieldName : String = 'Signal Possible Route Hold';
  Signal_PossibleStationStartRouteHoldFieldName : String = 'Signal Possible Station Start Route Hold';
  Signal_QuadrantFieldName : String = 'Signal Quadrant';
  Signal_TypeFieldName : String = 'Signal Type';
  Signal_UpDownFieldName : String = 'Signal Direction';
  Signal_UpperLeftIndicatorTargetFieldName : String = 'Signal Upper Left Indicator Target';
  Signal_UpperRightIndicatorTargetFieldName : String = 'Signal Upper Right Indicator Target';
  Signal_VerticalSpacingFieldName : String = 'Signal Vertical Spacing';
  Signal_XAdjustmentFieldName : String = 'Signal X Adjustment';

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

  LineArrayType = ARRAY OF Integer;

  TrackCircuitRec = RECORD
    TC_AdjacentBufferStop : Integer;
    TC_AdjacentSignals : IntegerArrayType;
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
    TC_LineArray : LineArrayType;
    TC_LitUp : Boolean; { used for flashing }
    TC_Location : Integer;
    TC_LockedForRoute : Integer;
    TC_LockFailureNotedInSubRouteUnit : Boolean;
    TC_LocoChip : Integer;
    TC_LocoStalled : Boolean;
    TC_MissingTrainNoted : Boolean;
    TC_MysteriouslyOccupied : Boolean;
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

  { Train-related type declarations }
  TypeOfTrainType = (LightLoco, ExpressPassenger, OrdinaryPassenger, ExpressFreight, Freight75mph, EmptyCoachingStock, Freight60mph, Freight45mph, Freight35mph,
                     International, UnknownTrainType);
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
  TrainStatusType = (ReadyForCreation, WaitingForLightsOn, WaitingForSignalHiddenAspectToClear, WaitingForRouteing, InLightsOnTime, ReadyForRouteing, CommencedRouteing,
                     ReadyToDepart, Departed, RouteingWhileDeparted, RouteCompleted, WaitingForRemovalFromDiagrams, ReadyForRemovalFromDiagrams, RemovedFromDiagrams,
                     Missing, MissingAndSuspended, Suspended, NonMoving, Cancelled, UnknownTrainStatus);
  Train = ^TrainRec;

  TrainRec = RECORD
    Train_LocoChip : Integer;
    Train_DoubleHeaderLocoChip : Integer;

    Train_Accelerating : Boolean;
    Train_AccelerationAdjustRange : Integer;
    Train_AccelerationStartTime : TDateTime;
    Train_AccelerationStr : String;
    Train_AccelerationTimeInSeconds : Real;
    Train_AccelerationTimeInterval : Real;
    Train_Active : Boolean;
    Train_ActualNumStr : String;
    Train_AtCurrentBufferStop : Integer;
    Train_AtCurrentSignal : Integer;
    Train_AtHiddenAspectSignal : Integer;
    Train_BeingAdvanced : Boolean;
    Train_BeingAdvancedTC : Integer;
    Train_CabLightsHaveBeenOn : Boolean;
    Train_ControlledByProgram : Boolean;
    Train_ControlledByRDC : Boolean;
    Train_CurrentArrivalTime : TDateTime;
    Train_CurrentBufferStop : Integer;
    Train_CurrentDirection : DirectionType;
    Train_CurrentJourney : Integer;
    Train_CurrentLengthInInches : Integer;
    Train_CurrentLenzSpeed : Integer;
    Train_CurrentRoute : Integer;
    Train_CurrentSignal : Integer;
    Train_CurrentSourceLocation : Integer;
    Train_CurrentSpeedInMPH : MPHType;
    Train_CurrentStatus : TrainStatusType;
    Train_CurrentTC : Integer;
    Train_Decelerating : Boolean;
    Train_Description : String;
    Train_DesiredLenzSpeed : Integer;
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
    Train_FixedLengthInInches : Integer;
    Train_Functions : ARRAY [0..12] OF Boolean;
    Train_Functions0To4Byte : Byte;
    Train_Functions5To12Byte : Byte;
    Train_GradientSpeedAdjustment : Integer;
    Train_GradientSpeedAdjustmentMsgWritten : Boolean;
    Train_HasCabLights : Boolean;
    Train_Headcode : String;
    Train_HomeArea : Integer;
    Train_InitialTrackCircuits : ARRAY [1..5] OF Integer;
    Train_InLightsOnTime : Boolean; { train inactive but for lights being on }
    Train_JourneysArray : TrainJourneyRecArrayType;
    Train_LastLengthInInches : Integer;
    Train_LastLocation : Integer;
    Train_LastMissingTC : Integer;
    Train_LastRouteLockedMsgStr : String;
    Train_LastSignal : Integer;
    Train_LastTC : Integer;
    Train_LightingChipDown : Integer;
    Train_LightingChipDownAddress : Train;
    Train_LightingChipUp : Integer;
    Train_LightingChipUpAddress : Train;
    Train_LightingChipRecordForChip : Integer;
    Train_LightsMsg : String;
    Train_LightsOn : Boolean;
    Train_LightsOnTime : TDateTime;
    Train_LightsRemainOnWhenJourneysComplete : Boolean;
    Train_LightsType : LightsType;
    Train_LocatedAtStartup : Boolean;
    Train_Locations : IntegerArrayType;
    Train_LocoChipStr : String;
    Train_LocoClassStr : String;
    Train_LocoName : String;
    Train_LocoTypeStr : String;
    Train_MaximumSpeedInMPH : MPHType;
    Train_MinimumAccelerationTimeInSeconds : Integer; { needed as we only calculate it once when we enter a trackcircuit }
    Train_MissingMessage : Boolean;
    Train_MissingNum : Integer;
    Train_NextTC : Integer;
    Train_NextButOneTC : Integer;
    Train_NotInPlaceMsgWritten : Boolean;
    Train_NumberOfCarriages : Integer;
    Train_NotLocatedAtStartupMsgWritten : Boolean;
    Train_PossibleRerouteTime : TDateTime;
    Train_PreviouslyControlledByProgram : Boolean;
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
    Train_SaveDesiredLenzSpeed : Integer;
    Train_SavedLocation : Integer;
    Train_SavedRoute : Integer;
    Train_SaveSpeedInFiddleyardMsg : String;
    Train_SaveTCsClearedStr : String;
    Train_SaveTCsForReleaseStr : String;
    Train_SaveTCsOccupiedStr : String;
    Train_SectionStartTime : TDateTime;
    Train_Speed10, Train_Speed20, Train_Speed30, Train_Speed40, Train_Speed50, Train_Speed60 : Integer;
    Train_Speed70, Train_Speed80, Train_Speed90, Train_Speed100, Train_Speed110, Train_Speed120 : Integer;
    Train_SpeedArray : ARRAY [1..12] OF Integer;
    Train_SpeedByte : Byte;
    Train_SpeedByteReadIn : Boolean;
    Train_SpeedSettingsMissing : Boolean;
    Train_SpeedStepMode : Integer;
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
    Train_UserRequiresInstructionMsg : String;
    Train_UseTrailingTrackCircuits : Boolean;
    { where a train doesn't have lights at both ends, it may need artificial track-circuit activation }
    Train_WaitingForHiddenAspectStartTime : TDateTime;
    Train_WorkingTimetableLastArrivalArea : Integer;
    Train_WorkingTimetableLastArrivalTime : TDateTime;
    Train_WorkingTimetableLastEntryNumStr : String;

    { linked-list bit: }
    Train_NextRecord : Train;
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
    LightsToBeSwitchedOn_Train : Train;
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

  { Diagram-related type declarations }
  DiagramsEntryType = ^DiagramsRec;

  DiagramsRec = RECORD
    TrainPtr : Train;
    { linked-list bit: }
    NextDiagramsRecord : DiagramsEntryType;
  END;

  DiagramsArrayType = ARRAY OF DiagramsRec;

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
  TrainArrayType = ARRAY OF Train;

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
  LevelStr = 'Level';
  RisingIfDownStr = 'RisingIfDown';
  RisingIfUpStr = 'RisingIfUp';

  DivergingStr = 'Diverging';
  OutOfActionStr = 'OutOfAction';
  StraightStr = 'Straight';
  UnknownStr = 'Unknown';

  NonThroughLocationStr = 'NonThroughLocation';
  ThroughLocationStr = 'ThroughLocation';

  FeedbackDetectorOutOfUseStr = 'OutOfUse';
  LineFeedbackDetectorStr = 'LineDetector';
  MixedFeedbackDetectorStr = 'Mixed';
  PointFeedbackDetectorStr = 'PointDetector';
  TrackCircuitFeedbackDetectorStr = 'TrackCircuitDetector';
  TRSPlungerFeedbackDetectorStr = 'TRSPlunger';
  UnknownFeedbackDetectorStr = 'Unknown';

  LogSignalData = True;

VAR
  AllSignalsSwitchedOff : Boolean = False;
  AnonymousOccupationMode : Boolean = True;
  Areas : ARRAY OF AreaRec;
  AutoModeInitiated : Boolean = False;
  BreakPointRequiredInMakeSoundRoutine : Boolean = False;
  BufferStops : ARRAY OF BufferStopRec;
  CrossHairCursor : TCursor;
  DayTimeSetByUser : Boolean = False;
  DebuggingMode : Boolean = False;
  DesiredLenzConnection : LenzConnectionType = NoConnection;
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
  InAutoMode : Boolean = False;
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
  PreparingZoom : Boolean = False;
  ProgramShuttingDown : Boolean = False;
  ProgramStartup : Boolean = True;
  RailWindowBitmapCanvasPenWidth : Integer;
  RailDriverCalibrated : Boolean = False;
  RailDriverCalibrationStarted : Boolean = False;
  RailDriverInitialised : Boolean = False;
  RDCMode : Boolean = False;
  ReadOutDecoderNumber : Boolean = False;
  ReadOutAdjacentSignalNumber : Boolean = False;
  ReadOutTCInFull : Boolean = False;
  ReadOutTCOnce : Boolean = False;
  RecordLineDrawingMode : Boolean = False;
  RecordingMonitorScreens : Boolean = False;
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
  TempTrainArray : ARRAY OF Train;
  TerminatingSpeedReductionMode : Boolean = False;
  TestingMode : Boolean = False;
  TextWindowHeight : Word;
  ThinLineMode : Boolean = True;
  TimeLastDataReceived : Cardinal = 0;
  TimeOutMsgWritten : Boolean = False;
  TrackCircuitHighlighted : Integer = UnknownTrackCircuit;
  TrackCircuits : ARRAY OF TrackCircuitRec;
  TrackCircuitsInitialised : Boolean = False;
  UpLineY, DownLineY : Word;
  VerboseFlag : Boolean = False;
  WatchdogTimerCount : Integer = 0;
  WindowPenWidth : Integer = 1;
  WorkingTimetableRecArray : WorkingTimetableRecArrayType;
  Zooming : Boolean = False;

  { Lists }
  TrainList : Train = NIL;
  TTrainList : TTrain = NIL;
  DiagramsList : DiagramsEntryType;

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
  Routes_EndLines : LineArrayType;
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
  Routes_StartLines : LineArrayType;
  Routes_StartSignals : IntegerArrayType;
  Routes_SubRouteClearingStrings : ARRAY OF ARRAY OF StringArrayType; { ie a 3-dimensional ARRAY OF String }
  Routes_SubRouteEndLines : ARRAY OF LineArrayType;
  Routes_SubRoutesAheadNotClearMsgWrittenArray : BooleanArrayType;
  Routes_SubRouteSettingStrings : ARRAY OF ARRAY OF StringArrayType; { ie a 3-dimensional ARRAY OF String }
  Routes_SubRouteStartLines : ARRAY OF LineArrayType;
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
  ShowLineNumbers : Boolean = False;
  ShowLinesWhereUpXValueSpecified : Boolean = False;
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
  ShowSignalHiddenAspects : Boolean = False;
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

PROCEDURE AddNewRecordToSignalDatabase;
{ Append a record to the signal database }

PROCEDURE CalculateBufferStopPositions(ScaleFactor : Integer);
{ Work out where the buffer stops are on the screen }

PROCEDURE CalculateLinePositions(ScaleFactor : Integer);
{ Work out where the lines are on the screen }

PROCEDURE CalculateLocationPositions(ScaleFactor : Integer);
{ Work out where the locations are on the screen }

PROCEDURE CalculatePlatformPositions(ScaleFactor : Integer);
{ Create the platform rectangle }

PROCEDURE CalculatePointPositions;
{ Create where the points are on the screen }

PROCEDURE CalculateTCAdjacentSignals;
{ Work out which trackcircuits are next the signal }

PROCEDURE CalculateSignalPositions(ScaleFactor : Integer);
{ Work out where the signals are on the screen }

FUNCTION DeleteRecordFromSignalDatabaseAndRenumberSignals(SignalNumToDelete : Integer) : Boolean;
{ Remove a record from the signal database }

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
{ Initialise the trackcircuit data which depends on lines being initialised first }

PROCEDURE RestoreScreenDrawingVariables;
{ Restore the default screen drawing variables }

PROCEDURE SaveScreenDrawingVariables;
{ Save the screen drawing variables }

PROCEDURE SetUpLineDrawingVars(ScaleFactor : Integer);
{ Set up the positions of the lines and platforms }

FUNCTION ValidateDirection(Str : String; OUT ErrorMsg : String) : DirectionType;
{ Validates and if ok returns the signal direction }

FUNCTION ValidateIndicatorSpeedRestriction(Str : String; Indicator : IndicatorType; OUT ErrorMsg : String) : MPHType;
{ Validates and if ok returns what the tempoarary speed restriction is. This test must be carried outr after Indicator is validated. }

FUNCTION ValidateJunctionIndicators1(Str, FieldName : String; Signal_Indicator : IndicatorType; OUT ErrorMsg : String) : JunctionIndicatorRec;
{ The first part of verifying whether junction indicators are correctly set up; this part also returns the values for each junction indicator. This test requires that
  the indicator has been validated first
}
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

FUNCTION ValidatePointLenzNum(LenzNumStr : String; PointManualStateAsReadIn : PointStateType; OUT PointManualOperation : Boolean; OUT PointPresentState : PointStateType;
                              OUT ErrorMsg : String) : Integer;
{ Check whether the Lenz point number is valid }

FUNCTION ValidatePointLenzUnit(LenzUnitStr : String; OUT ErrorMsg : String) : Integer;
{ Check whether a Lenz point unit is valid }

FUNCTION ValidatePointLenzUnitType(LenzUnitTypeStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : String;
{ Check whether a Lenz point unit type is valid }

FUNCTION ValidatePointManualStateAsReadIn(PointStateStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : PointStateType;
{ Check whether the last point state read in is valid }

FUNCTION ValidatePointOtherPoint(OtherPointStr : String; PointType : TypeOfPoint; OUT ErrorMsg : String) : Integer;
{ Check whether the value of the connected point (if any) is valid }

FUNCTION ValidatePointStraightLineName(LineName : String; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Straight Line name is valid }

FUNCTION ValidatePointType(PointTypeStr : String; OUT ErrorMsg : String) : TypeOfPoint;
{ Check that the supplied point type is valid }

FUNCTION ValidateSignalAccessoryAddress(Str : String; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder address for a TrainTech S3 accessory decoder. This test must be done after Signal Type is validated. }

FUNCTION ValidateSignalAdjacentLine(SignalBeingValidated : Integer; Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the line next to a signal }

FUNCTION ValidateSignalApproachControlAspect(Str : String; OUT ErrorMsg : String) : AspectType;
{ Validates and if ok returns the approach control signal aspect }

FUNCTION ValidateSignalAsTheatreDestination(Str : String; OUT ErrorMsg : String) : String;
{ Validates and if ok returns the one or two character display used in a theatre indicator }

FUNCTION ValidateSignalDecoderNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for a signal decoder. This check must be done after Accessory Address and Signal Type are validated. }

FUNCTION ValidateSignalIndicator(Str : String; OUT ErrorMsg : String) : IndicatorType;
{ Validates and if ok returns the signal indicator }

FUNCTION ValidateSignalIndicatorDecoderFunctionNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for an indicator function decoder. This check must be done after Accessory Address and Signal Type are validated. }

FUNCTION ValidateSignalIndicatorDecoderNum(Str : String; AccessoryAddress : Integer; SignalType : TypeOfSignal; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the decoder number for an indicator decoder. This check must be done after Accessory Address and Signal Type are validated. }

FUNCTION ValidateSignalLocationsToMonitorArray(Str : String; PossibleRouteHold : Boolean; OUT ErrorMsg : String) : IntegerArrayType;
{ Validates and if ok returns the signal locations monitored when a route is held. This test must be done after Possible Route Hold is validated. }

FUNCTION ValidateSignalNum(SignalToTest : Integer) : String;
{ Validates a signal number. This has to be carried out separately from other validation, as when creating signals a reference may be made to a signal not yet created }

FUNCTION ValidateSignalOppositePassingLoopSignal(Str : String; Init : Boolean; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns the other signal involved in a passing loop }

FUNCTION ValidateSignalOutOfUseAndAddAdjacentTC(S : Integer; Flag : Boolean; AdjacentLine : Integer; OUT AdjacentTC : Integer; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns true if a signal is marked as not in use. This test must be done after Adjacent Signal is validated. }

FUNCTION ValidateSignalPossibleStationStartRouteHold(Flag : Boolean; PossibleRouteHold : Boolean; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns whether a station start route hold is in operation. This test must be done after Possible Route Hold is validated. }

FUNCTION ValidateSignalQuadrant(Str : String; OUT ErrorMsg : String) : QuadrantType;
{ Validates and if ok returns the quadrant type }

FUNCTION ValidateSignalType(Str : String; Quadrant : QuadrantType; OUT ErrorMsg : String) : TypeOfSignal;
{ Validates and if ok returns the signal type }

FUNCTION ValidateSignalXAdjustment(Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns whether a signal's on screen adjustment if any }

PROCEDURE ValidateJunctionIndicators2(StrArray : ARRAY OF String; SignalIndicator : IndicatorType; SignalJunctionIndicators : ARRAY OF JunctionIndicatorRec;
                                      OUT ErrorMsg : String);
{ The second part of verifying whether junction indictaors are correctly set up }

PROCEDURE WriteOutLineDataToDatabase;
{ Write out some line data to the line data file }

PROCEDURE WriteOutLocationDataToDatabase;
{ Write out some location data to the location data file }

PROCEDURE WriteOutPointDataToDatabase;
{ If a point's data has been changed, record it in the database }

PROCEDURE WriteOutSignalDataToDatabase;
{ If a Signal's data has been changed, record it in the database }

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

PROCEDURE SetUpLineDrawingVars(ScaleFactor : Integer);
{ Set up the positions of the lines and plaforms }
BEGIN
  { Interval spacing : the following data has been read in from the .ini file }
  DeltaPointXSpaced := MulDiv(FWPRailWindow.ClientHeight, DeltaPointX, ScaleFactor * 10);
  BufferStopVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, BufferStopVerticalSpacing, ScaleFactor * 10);
  IndicatorHorizontalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, IndicatorHorizontalSpacing, ScaleFactor * 10);
  IndicatorVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, IndicatorVerticalSpacing, ScaleFactor * 10);
  MouseRectangleEdgeVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, MouseRectangleEdgeVerticalSpacing, ScaleFactor * 10);
  PlatformEdgeVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, PlatformEdgeVerticalSpacing, ScaleFactor * 10);
  PlatformNumberEdgeHorizontalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, PlatformNumberEdgeHorizontalSpacing, ScaleFactor * 10);
  PlatformNumberEdgeVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, PlatformNumberEdgeVerticalSpacing, ScaleFactor * 10);
  SignalHorizontalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, SignalHorizontalSpacing, ScaleFactor * 10);
  SignalRadiusScaled := MulDiv(FWPRailWindow.ClientWidth, SignalRadius, ScaleFactor * 10);
  SignalSemaphoreHeightScaled := MulDiv(FWPRailWindow.ClientWidth, SignalSemaphoreHeight, ScaleFactor * 10);
  SignalSemaphoreWidthScaled := MulDiv(FWPRailWindow.ClientHeight, SignalSemaphoreWidth, ScaleFactor * 10);
  SignalVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, SignalVerticalSpacing, ScaleFactor * 10);
  SpeedRestrictionHorizontalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, SpeedRestrictionHorizontalSpacing, ScaleFactor * 10);
  SpeedRestrictionVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, SpeedRestrictionVerticalSpacing, ScaleFactor * 10);
  TheatreIndicatorHorizontalSpacingScaled := MulDiv(FWPRailWindow.ClientWidth, TheatreIndicatorHorizontalSpacing, ScaleFactor * 10);
  TheatreIndicatorVerticalSpacingScaled := MulDiv(FWPRailWindow.ClientHeight, TheatreIndicatorVerticalSpacing, ScaleFactor * 10);
  TRSPlungerLengthScaled := MulDiv(FWPRailWindow.ClientWidth, TRSPlungerLength, ScaleFactor * 10);
END; { SetUpLineDrawingVars }

PROCEDURE ReadInTrackCircuitDataFromDatabase;
{ Initialise the trackcircuit data which depends on lines being initialised first.

  Interesting notes from the past here:

  Unit 77 inputs 5-8 free
  Unit 78 (tunnel) for UMC
  Unit 86 input 5 does not have a corresponding LB101 as 6-8 are photocell inputs
  Unit 94 input 6 reserved for new siding point (1 to 5 are points already)
  Unit 112 input 8 (FY) is free [TC input]

  Unit 93 inputs 2-3 reserved for new siding A
  Unit 95 inputs 5-6 reserved for new siding B
  Unit 99 inputs 3-4 reseved for cross over points from platforms 5 to 6
  Unit 116 input 2 free [point input]

}
CONST
  StopTimer = True;

VAR
  ErrorMsg : String;
  FieldName : String;
  L : Integer;
  Location : Integer;
  TC : Integer;
  TCNum : Integer;
  TCLocationFound : Boolean;
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

      TrackCircuitDataADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                        + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                        + ';Persist Security Info=False';
      TrackCircuitDataADOConnection.Connected := True;
      TrackCircuitDataADOTable.Open;
      Log('T Track circuit data table and connection opened to initialise the trackCircuits');

      { First see if the track circuit numbers in the MSAccess file are sequential and, if not, renumber it - we need this or deletions from the MSAccess file will cause
        problems
      }
      FieldName := 'TCNum';
      TrackCircuitDataADOTable.Sort := 'TCNum ASC';
      TrackCircuitDataADOTable.First;
      SetLength(TrackCircuits, 0);

      WHILE NOT TrackCircuitDataADOTable.EOF DO BEGIN
        WITH TrackCircuitDataADOTable DO BEGIN
          SetLength(TrackCircuits, Length(TrackCircuits) + 1);
          TC := High(TrackCircuits);
          WITH TrackCircuits[TC] DO BEGIN
            ErrorMsg := '';

            FieldName := 'TCNum';
            TCNum := TrackCircuitDataADOTable.FieldByName(FieldName).AsInteger;
            IF TCNum <> TC THEN
              ErrorMsg := 'it does not match the line number in the database (' + IntToStr(TCNum) + ')';

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Length';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                TC_LengthInInches := 0
              ELSE
                IF NOT TryStrToFloat(TempStr, TC_LengthInInches) THEN
                  Debug('TC=' + IntToStr(TCNum) + ': invalid length String "' + TempStr + '"');
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'FeedbackUnit';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                TC_FeedbackUnit := 0
              ELSE
                IF NOT TryStrToInt(TempStr, TC_FeedbackUnit) THEN
                  ErrorMsg := 'invalid feedback unit "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'FeedbackInput';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                TC_FeedbackInput := 0
              ELSE
                IF NOT TryStrToInt(TempStr, TC_FeedbackInput) THEN
                  ErrorMsg := 'invalid feedback input "' + TempStr + '"';
            END;

            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating TC=' + IntToStr( High(TrackCircuits)) + ' (' + IntToStr(TCNum) + '): '
                                            + '[' + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInTrackCircuitDataFromDatabase');
            END;
          END; { WITH }
        END; { WITH }
        TrackCircuitDataADOTable.Next;
      END; { WHILE }

      { Tidy up the database }
      TrackCircuitDataADOTable.Close;
      TrackCircuitDataADOConnection.Connected := False;
      Log('T Track circuit data table and connection closed');
    END; { WITH }

    FOR TC := 0 TO High(TrackCircuits) DO BEGIN
      WITH TrackCircuits[TC] DO BEGIN
        SetLength(TC_AdjacentSignals, 0);
        TC_AdjacentBufferStop := UnknownBufferStop;
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
      END; { WITH }

      { Initialise trackcircuit lines and locations }
      TCLocationFound := False;
      L := 0;
      WHILE (L < High(Lines)) AND NOT TCLocationFound DO BEGIN
        IF Lines[L].Line_TC = TC THEN BEGIN
          TCLocationFound := True;
          AppendToLineArray(TrackCircuits[TC].TC_LineArray, L);
          TrackCircuits[TC].TC_Location := Lines[L].Line_Location;

          { also initialise the buffer stop data }
          TrackCircuits[TC].TC_AdjacentBufferStop := Lines[L].Line_AdjacentBufferStop;

          { and the gradient - give the trackcircuit the benefit of the doubt so that if any line comprising it is not level, that gradient is the one recorded as being at
            the trackcircuit
          }
          IF Lines[L].Line_Gradient <> Level THEN
            TrackCircuits[TC].TC_Gradient := Lines[L].Line_Gradient;
        END;
        Inc(L);
      END; { WHILE }
    END; { FOR }

    { Check that all track circuits have a TC_LineArray value }
    TC := 0;
    WHILE TC <= High(TrackCircuits) DO BEGIN
      IF Length(TrackCircuits[TC].TC_LineArray) = 0 THEN
        Debug('!TC=' + IntToStr(TC) + ': has no entry in the LineData file');
      Inc(TC);
    END; { WHILE }

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
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG ReadInTrackCircuitDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
END; { ReadInTrackCircuitDataFromDatabase }

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
      END; { WHILE }

      AreasADOTable.First;
      SetLength(Areas, 0);
      SetLength(ReversingAreas, 0);

      WITH AreasADOTable DO BEGIN
        AreasADOTable.Sort := 'AreaNum ASC';
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
                END; { WITH }
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
          END; { WITH }
          AreasADOTable.Next;
        END; { WHILE }
      END; { WITH }

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
          END; { WHILE }

          SetLength(Area_AccessibleAreasDown, 0);
          I := 0;
          WHILE (I <= High(Area_AccessibleAreasDownStrArray)) AND (ErrorMsg = '') DO BEGIN
            TempArea := StrToArea(Area_AccessibleAreasDownStrArray[I]);
            IF TempArea = UnknownArea THEN
              ErrorMsg := 'unknown area "' + Area_AccessibleAreasDownStrArray[I] + ''' in Area_AccessibleAreasDown field for Area=' + AreaToStr(A_Num)
            ELSE
              AppendToAreaArray(Area_AccessibleAreasDown, TempArea);

            Inc(I);
          END; { WHILE }

          IF NOT Area_IsHoldingArea AND (Length(Area_AccessibleAreasUp) = 0) AND (Length(Area_AccessibleAreasDown) = 0) THEN
            ErrorMsg := 'it has neither Possible Up Destinations or Possible Down Destinations';
        END; { WITH }

        IF ErrorMsg = '' THEN
          Inc(A_Num);
      END; { WHILE }

      IF ErrorMsg <> '' THEN BEGIN
        IF MessageDialogueWithDefault('Error in creating Area=' + IntToStr(A_Num) + ' (' + Areas[A_Num].Area_LongStr + '): '
                                      + ErrorMsg
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInAreasDataFromDatabase');
      END;
    END; { WITH }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG ReadInAreasDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
END; { ReadInAreasDataFromDatabase }

PROCEDURE CalculateLocationPositions(ScaleFactor : Integer);
{ Work out where the locations are on the screen }
VAR
  Location : Integer;

BEGIN
  Location := 0;
  WHILE Location <= High(Locations) DO BEGIN
    WITH Locations[Location] DO
      Location_YScaled := MulDiv(FWPRailWindow.ClientHeight, Location_Y, ScaleFactor);
    Inc(Location);
  END; { WHILE }
END; { CalculateLocationPositions }

PROCEDURE ReadInLocationDataFromDatabase;
{ Initialise the location data }
CONST
  StopTimer = True;

VAR
  A : Integer;
  AccessibleLocationsCount : Integer;
  ErrorMsg : String;
  FieldName : String;
  I : Integer;
  Iterations : Integer;
  Loc_Num : Integer;
  Location : Integer;
  LocationExceptions : IntegerArrayType;
  LocationExceptionsStrArray : StringArrayType;
  LocoChip : Integer;
  MissingYValue : Boolean;
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
      FieldName := 'LocationNum';
      WHILE NOT LocationsADOTable.EOF DO BEGIN
        Inc(Loc_Num);
        IF LocationsADOTable.FieldByName(FieldName).AsInteger <> Loc_Num THEN BEGIN
          { we need to renumber from here on }
          LocationsADOTable.Edit;
          LocationsADOTable.FieldByName(FieldName).AsInteger := Loc_Num;
          LocationsADOTable.Post;
        END;
        LocationsADOTable.Next;
      END; { WHILE }

      LocationsADOTable.First;
      SetLength(Locations, 0);

      WITH LocationsADOTable DO BEGIN
        LocationsADOTable.Sort := 'LocationNum ASC';
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

            FieldName := 'LocationNum';
            Loc_Num := LocationsADOTable.FieldByName(FieldName).AsInteger;
            IF Loc_Num <> High(Locations) THEN
              ErrorMsg := 'it does not match the location number in the database (' + IntToStr(Loc_Num) + ')';

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'LocationName';
              Location_LongStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF Location_LongStr = '' THEN
                ErrorMsg := 'missing long name';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'ShortString';
              Location_ShortStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF Location_ShortStr = '' THEN
                ErrorMsg := 'missing short name';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'OutOfUse';
              Location_OutOfUse := LocationsADOTable.FieldByName(FieldName).AsBoolean;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Area';
              TempStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Location_Area := UnknownArea
              ELSE BEGIN
                Location_Area := StrToArea(TempStr);
                IF Location_Area = UnknownArea THEN
                  ErrorMsg := 'unknown area "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'ThroughLocation';
              TempStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Location_ThroughLocationState := ThroughLocation
              ELSE
                IF TempStr = 'Non-Through Location' THEN
                  Location_ThroughLocationState := NonThroughLocation
                ELSE
                  ErrorMsg := 'unknown through location state "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'LocoClassesReservedFor';
              TempStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF TempStr <> '' THEN BEGIN
                ExtractSubStringsFromString(TempStr, ',', TempStrArray);
                FOR I := 0 TO High(TempStrArray) DO
                  AppendToStringArray(Location_LocoClassesReservedFor, TempStrArray[I]);
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'LocosNotAbleToUse';
              TempStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF TempStr <> '' THEN BEGIN
                ExtractSubStringsFromString(TempStr, ',', TempStrArray);
                FOR I := 0 TO High(TempStrArray) DO BEGIN
                  IF NOT TryStrToInt(TempStrArray[I], LocoChip) THEN
                    ErrorMsg := 'invalid loco chip number "' + TempStrArray[I] + '"'
                  ELSE
                    AppendToIntegerArray(Location_LocosNotAbleToUse, LocoChip);
                END; { FOR }
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Platform';
              Location_IsPlatform := LocationsADOTable.FieldByName(FieldName).AsBoolean;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Siding';
              Location_IsSiding := LocationsADOTable.FieldByName(FieldName).AsBoolean;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Fiddleyard';
              Location_IsFiddleyard := LocationsADOTable.FieldByName(FieldName).AsBoolean;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'LengthInInches';
              TempStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF TempStr <> '' THEN
                IF NOT TryStrToFloat(TempStr, Location_LengthInInches) THEN
                  ErrorMsg := 'invalid length "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Y';
              TempStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF TempStr <> '' THEN
                IF NOT TryStrToInt(TempStr, Location_Y) THEN
                  ErrorMsg := 'invalid YPos value "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'YLocation';
              Location_YLocationStr := LocationsADOTable.FieldByName(FieldName).AsString;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'YLocationAdjustment';
              TempStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF TempStr <> '' THEN
                IF NOT TryStrToInt(TempStr, Location_YLocationAdjustment) THEN
                  ErrorMsg := 'invalid YPos location adjustment value "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'TRSPlungerX';
              TempStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF TempStr <> '' THEN
                IF NOT TryStrToInt(TempStr, Location_TRSPlungerX) THEN
                  ErrorMsg := 'invalid TRS Plunger X value "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'TRSPlungerY';
              TempStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF TempStr <> '' THEN
                IF NOT TryStrToInt(TempStr, Location_TRSPlungerY) THEN
                  ErrorMsg := 'invalid TRS Plunger Y value "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'PlatformParallelAccess';
              Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr := LocationsADOTable.FieldByName(FieldName).AsString;
              { Note: we can't test that this field contains a valid location until all the locations are read in }
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'PlatformDirection';
              TempStr := LocationsADOTable.FieldByName(FieldName).AsString;
              IF TempStr <> '' THEN BEGIN
                Location_PlatformOrFiddleyardDirection := StrToDirectionType(TempStr);
                IF Location_PlatformOrFiddleyardDirection = UnknownDirection THEN
                  ErrorMsg := 'invalid direction "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'PlatformNumStr';
              Location_PlatformOrFiddleyardNumStr := LocationsADOTable.FieldByName(FieldName).AsString;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'RecordInLocationOccupationArray';
              Location_RecordInLocationOccupationArray := LocationsADOTable.FieldByName(FieldName).AsBoolean;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'AdjoiningPlatform';
              Location_AdjoiningPlatformStr := FieldByName(FieldName).AsString;
              { Note: we can only check this data after all the locations have been read in }
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'DestinationPriorityAreas';
              TempStr := FieldByName(FieldName).AsString;
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
                      ErrorMsg := 'unknown area "' + TempStrArray[I] + '" for ' + Location_LongStr
                    ELSE
                      AppendToAreaArray(Location_DestinationPriorityAreas, TempArea);
                  END;
                  Inc(I);
                END; { WHILE }
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'PlatformPriority';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Location_PlatformPriority := 0
              ELSE
                IF NOT TryStrToInt(TempStr, Location_PlatformPriority) THEN
                  ErrorMsg := 'invalid integer "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'TrainPriority';
              TempStr := FieldByName(FieldName).AsString;
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
                        ErrorMsg := 'unknown train priority "' + TempStr + '" for ' + Location_LongStr;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'DirectionPriority';
              TempStr := FieldByName(FieldName).AsString;
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
                            ErrorMsg := 'unknown direction priority "' + TempStr + '" for ' + Location_LongStr;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'ThroughOrStoppingPriority';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = 'Through' THEN
                Location_ThroughOrStoppingPriority := ThroughPriority
              ELSE
                IF TempStr = 'Stopping' THEN
                  Location_ThroughOrStoppingPriority := StoppingPriority
                ELSE
                  IF TempStr = '' THEN
                    Location_ThroughOrStoppingPriority := NoStoppingPriority
                  ELSE
                    ErrorMsg := 'unknown through or stopping priority "' + TempStr + '" for ' + Location_LongStr;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'AccessibleLocationsOrAreasUp';
              TempStr := FieldByName(FieldName).AsString;
              SetLength(Location_AccessibleLocationsOrAreasUpStrArray, 0);
              IF TempStr <> '' THEN
                { Extract the data into a String array for now - we expect a comma as a delimiter }
                ExtractSubStringsFromString(TempStr, ',', Location_AccessibleLocationsOrAreasUpStrArray);
                { Note: we can only check this data after all the locations have been read in }
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'AccessibleLocationsOrAreasDown';
              TempStr := FieldByName(FieldName).AsString;
              SetLength(Location_AccessibleLocationsOrAreasDownStrArray, 0);
              IF TempStr <> '' THEN
                { Extract the data into a String array for now - we expect a comma as a delimiter }
                ExtractSubStringsFromString(TempStr, ',', Location_AccessibleLocationsOrAreasDownStrArray);
                { Note: we can only check this data after all the locations have been read in }
            END;

            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating Location ' + IntToStr( High(Locations)) + ' (' + Location_LongStr + ' ): '
                                            + '[' + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInLocationDataFromDatabase 1');
            END;
          END; { WITH }

          LocationsADOTable.Next;
        END; { WHILE }
      END; { WITH }

      { Tidy up the database }
      LocationsADOTable.Close;
      LocationsADOConnection.Connected := False;
      Log('T Location Data table and connection closed');
    END; { WITH }

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
            TestStr2 := UpperCase(RemoveAllSpacesFromAString(Locations[I].Location_LongStr));
            IF TestStr1 = TestStr2 THEN BEGIN
              Location_AdjoiningPlatform := StrToLocation(Location_AdjoiningPlatformStr);
              IF Location_AdjoiningPlatform <> UnknownLocation THEN
                OK := True
              ELSE
                ErrorMsg := 'unknown adjoining platform "' + Location_AdjoiningPlatformStr;
            END;
            Inc(I);
          END; { WHILE }
        END;

        IF ErrorMsg = '' THEN BEGIN
          IF Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr <> '' THEN BEGIN
            I := 0;
            WHILE (I <= High(Locations)) AND NOT OK DO BEGIN
              TestStr1 := UpperCase(RemoveAllSpacesFromAString(Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr));
              TestStr2 := UpperCase(RemoveAllSpacesFromAString(Locations[I].Location_LongStr));
              IF TestStr1 = TestStr2 THEN BEGIN
                Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyard := StrToLocation(Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr);
                IF Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyard <> UnknownLocation THEN
                  OK := True
                ELSE
                  ErrorMsg := 'unknown access platform "' + Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyardStr;
              END;
              Inc(I);
            END; { WHILE }
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
            END; { FOR }
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
                    IF (Locations[NewLocation].Location_Area = TempArea)
                    AND NOT IsElementInIntegerArray(LocationExceptions, NewLocation)
                    THEN
                      AppendToIntegerArray(Location_AccessibleLocationsDown, NewLocation);
                END
                ELSE
                  ErrorMsg := 'unknown accessible down location ' + Location_AccessibleLocationsOrAreasDownStrArray
                    [AccessibleLocationsCount] + ' in Accessible Locations Or Areas Down';
              END;
            END; { FOR }
          END;
        END;
      END; { WITH }
      Inc(Location);
    END; { WHILE }

    { And work out Y positions of locations - again, can't do this until all the locations are read in, and then we have to run through the list of locations until we build
      up all the missing Y values, as the list may not be in the correct order and locations may reference other locations which themselves haven't been initialised yet
    }
    TempLocation := UnknownLocation;
    Iterations := 0;
    IF ErrorMsg = '' THEN BEGIN
      REPEAT
        MissingYValue := False;
        Location := 0;
        WHILE (Location <= High(Locations)) AND (Iterations <= 100000) DO BEGIN
          WITH Locations[Location] DO BEGIN
            IF Location_Y = 0 THEN BEGIN
              IF Location_YLocationStr <> '' THEN BEGIN
                TempLocation := StrToLocation(Location_YLocationStr);
                IF TempLocation = UnknownLocation THEN
                  ErrorMsg := 'unknown Y location "' + Location_YLocationStr + '" supplied for location ' + LocationToStr(Location)
                ELSE BEGIN
                  Location_Y := Locations[TempLocation].Location_Y;
                  IF Location_Y = 0 THEN
                    MissingYValue := True
                  ELSE
                    Location_Y := Location_Y + Location_YLocationAdjustment;
                END;
              END;
            END;
          END; { WITH }
          Inc(Location);
          Inc(Iterations);
        END; { WHILE }
      UNTIL NOT MissingYValue OR (ErrorMsg <> '') OR (Iterations = 100000);
    END;

    IF Iterations >= 100000 THEN
      ErrorMsg := 'Too many iterations in calculating Y positions of locations - error probably with location=' + LocationToStr(TempLocation)
    ELSE
      CalculateLocationPositions(1000);

    IF ErrorMsg <> '' THEN BEGIN
      IF MessageDialogueWithDefault('Error when checking Loc=' + IntToStr(Location - 1) + ' (' + Locations[Location - 1] .Location_LongStr + '): '
                                    + ErrorMsg
                                    + CRLF
                                    + 'Do you wish to continue?',
                                    StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
      THEN
        ShutDownProgram(UnitRef, 'ReadInLocationDataFromDatabase 2');
    END;

  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG ReadInLocationDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
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
                IF FieldByName('LocationName').AsString = Location_LongStr THEN BEGIN
                  IF Location_OutOfUse <> FieldByName('OutOfUse').AsBoolean THEN BEGIN
                    Edit;
                    FieldByName('OutOfUse').AsBoolean := Location_OutOfUse;
                    Post;
                  END;
                END; { WITH }
              END;
              Inc(Location);
            END;
          END; { WITH }
          LocationsADOTable.Next;
        END; { WHILE }

        { Tidy up the database }
        LocationsADOTable.Close;
        LocationsADOConnection.Connected := False;
        Log('L Location Data table and connection closed after writing locations');
      END;
    END; { WITH }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG WriteOutLocationDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
END; { WriteOutLocationDataToDatabase }

PROCEDURE CalculateLinePositions(ScaleFactor : Integer);
{ Work out where the lines are on the screen }
VAR
  ErrorMsg : String;
  Iterations : Integer;
  L, L2 : Integer;
  MissingXValue : Boolean;
  ObliquePos : Integer;
  TempLocation1 : Integer;
  TempLocation2 : Integer;
  TempLocationStr1 : String;
  TempLocationStr2 : String;

BEGIN
  TRY
    L := 0;
    Iterations := 0;
    WHILE (L <= High(Lines)) AND (Iterations <= 10000) DO BEGIN
      WITH Lines[L] DO BEGIN
        MissingXValue := False;
        REPEAT
          { read in the values until one is found }
          FOR L2 := 0 TO High(Lines) DO BEGIN
            WITH Lines[L2] DO BEGIN
              IF Line_UpXAbsolute = 0 THEN BEGIN
                IF Lines[StrToLine(Line_UpXLineStr)].Line_DownXAbsolute = 0 THEN
                  MissingXValue := True
                ELSE BEGIN
                  IF Line_Length < 0 THEN BEGIN
                    Line_DownXAbsolute := Lines[StrToLine(Line_UpXLineStr)].Line_UpXAbsolute;
                    Line_UpXAbsolute := Line_DownXAbsolute + Line_Length;
                    MissingXValue := False;
                  END ELSE BEGIN
                    Line_UpXAbsolute := Lines[StrToLine(Line_UpXLineStr)].Line_DownXAbsolute;
                    Line_DownXAbsolute := Line_UpXAbsolute + Line_Length;
                    MissingXValue := False;
                  END;
                END;
              END;
            END; { WITH }
          END; { FOR }
          Inc(Iterations);
        UNTIL NOT MissingXValue OR (Iterations = 10000);

        IF Iterations >= 10000 THEN
          Debug('Too many iterations in CalculateLinePositions');
      END;
      Inc(L);
      Inc(Iterations);
    END; { WHILE }

    L := 0;
    WHILE L <= High(Lines) DO BEGIN
      WITH Lines[L] DO BEGIN
        Line_UpX := MulDiv(FWPRailWindow.ClientWidth, Line_UpXAbsolute, ScaleFactor);
        Line_DownX := MulDiv(FWPRailWindow.ClientWidth, Line_DownXAbsolute, ScaleFactor);
      END; { WITH }
      Inc(L);
    END; { WHILE }

    L := 0;
    WHILE L <= High(Lines) DO BEGIN
      WITH Lines[L] DO BEGIN
        IF Line_UpYAbsolute <> 0 THEN
          Line_UpY := MulDiv(FWPRailWindow.ClientHeight, Line_UpYAbsolute, ScaleFactor)
        ELSE BEGIN
          { see if it's two locations (from which we obtain the average Y value) }
          ObliquePos := Pos('/', Line_UpYLocationStr);
          IF ObliquePos = 0 THEN BEGIN
            Line_UpYLocation := StrToLocation(Line_UpYLocationStr);
            IF Line_UpYLocation = UnknownLocation THEN
              ErrorMsg := 'unknown UpY location "' + Line_UpYLocationStr + '"'
            ELSE BEGIN
              Line_UpYAbsolute := Locations[Line_UpYLocation].Location_Y;
              Line_UpY := Locations[Line_UpYLocation].Location_YScaled;
            END;
          END ELSE BEGIN
            TempLocationStr1 := Copy(Line_UpYLocationStr, 1, ObliquePos - 1);
            TempLocation1 := StrToLocation(TempLocationStr1);
            IF TempLocation1 = UnknownLocation THEN
              ErrorMsg := 'unknown UpY location "' + TempLocationStr1 + '"'
            ELSE BEGIN
              TempLocationStr2 := Copy(Line_UpYLocationStr, ObliquePos + 1);
              TempLocation2 := StrToLocation(TempLocationStr2);
              IF TempLocation2 = UnknownLocation THEN
                ErrorMsg := 'unknown UpY location "' + TempLocationStr2 + '"'
              ELSE BEGIN
                Line_UpYAbsolute := GetMidPos(Locations[TempLocation1].Location_Y, Locations[TempLocation2].Location_Y);
                Line_UpY := MulDiv(FWPRailWindow.ClientHeight, Line_UpYAbsolute, ScaleFactor)
              END;
            END;
          END;
        END;

        IF Line_DownYAbsolute <> 0 THEN
          Line_DownY := MulDiv(FWPRailWindow.ClientHeight, Line_DownYAbsolute, ScaleFactor)
        ELSE BEGIN
          { see if it's two locations (from which we obtain the average Y value) }
          ObliquePos := Pos('/', Line_DownYLocationStr);
          IF ObliquePos = 0 THEN BEGIN
            Line_DownYLocation := StrToLocation(Line_DownYLocationStr);
            IF Line_DownYLocation = UnknownLocation THEN
              ErrorMsg := 'unknown DownY location "' + Line_DownYLocationStr + '"'
            ELSE BEGIN
              Line_DownYAbsolute := Locations[Line_DownYLocation].Location_Y;
              Line_DownY := Locations[Line_DownYLocation].Location_YScaled;
            END;
          END ELSE BEGIN
            TempLocationStr1 := Copy(Line_DownYLocationStr, 1, ObliquePos - 1);
            TempLocation1 := StrToLocation(TempLocationStr1);
            IF TempLocation1 = UnknownLocation THEN
              ErrorMsg := 'unknown DownY location "' + TempLocationStr1 + '"'
            ELSE BEGIN
              TempLocationStr2 := Copy(Line_DownYLocationStr, ObliquePos + 1);
              TempLocation2 := StrToLocation(TempLocationStr2);
              IF TempLocation2 = UnknownLocation THEN
                ErrorMsg := 'unknown DownY location "' + TempLocationStr2 + '"'
              ELSE BEGIN
                Line_DownYAbsolute := GetMidPos(Locations[TempLocation1].Location_Y, Locations[TempLocation2].Location_Y);
                Line_DownY := MulDiv(FWPRailWindow.ClientHeight, Line_DownYAbsolute, ScaleFactor)
              END;
            END;
          END;
        END;
      END; { WITH }
      Inc(L);
    END; { WHILE }

    L := 0;
    WHILE L <= High(Lines) DO BEGIN
      WITH Lines[L] DO BEGIN
        Line_MousePolygon[0] := Point(Line_UpX, Line_UpY + MouseRectangleEdgeVerticalSpacingScaled);
        Line_MousePolygon[1] := Point(Line_UpX, Line_UpY - MouseRectangleEdgeVerticalSpacingScaled);
        Line_MousePolygon[2] := Point(Line_DownX, Line_DownY - MouseRectangleEdgeVerticalSpacingScaled);
        Line_MousePolygon[3] := Point(Line_DownX, Line_DownY + MouseRectangleEdgeVerticalSpacingScaled);
        Line_MousePolygon[4] := Line_MousePolygon[0];

        { Add the line-end characters which indicate where a line goes next }
        WITH RailWindowBitmap.Canvas DO BEGIN
          { a straight line }
          Font.Height := -MulDiv(FWPRailWindow.ClientHeight, FWPRailWindowFontHeight, ScaleFactor);
          IF Line_UpConnectionCh <> '' THEN BEGIN
            WITH Line_UpConnectionChRect DO BEGIN
              Left := Line_UpX - (TextWidth(Line_UpConnectionCh) DIV 2);
              Top := Line_UpY - (TextHeight(Line_UpConnectionCh) DIV 2);
              Right := Line_UpX + (TextWidth(Line_UpConnectionCh) DIV 2);
              Bottom := Line_UpY + (TextHeight(Line_UpConnectionCh) DIV 2);
            END; { WITH }
          END;
          IF Line_DownConnectionCh <> '' THEN BEGIN
            WITH Line_DownConnectionChRect DO BEGIN
              Left := Line_DownX - (TextWidth(Line_DownConnectionCh) DIV 2);
              Top := Line_DownY - (TextHeight(Line_DownConnectionCh) DIV 2);
              Right := Line_DownX + (TextWidth(Line_DownConnectionCh) DIV 2);
              Bottom := Line_DownY + (TextHeight(Line_DownConnectionCh) DIV 2);
            END; { WITH }
          END;
        END; { WITH }
      END; { WITH }
      Inc(L);
    END; { WHILE }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG CalculateLinePositions: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }


  IF VerboseFlag THEN
    FOR L := 0 TO High(Lines) DO
      WITH Lines[L] DO
        Log('X ' + Line_Str +  ' UpX=' + IntToStr(Line_UpX) + ' UpY=' + IntToStr(Line_UpY) + ' DownX=' + IntToStr(Line_DownX) + ' DownY=' + IntToStr(Line_DownY));
END; { CalculateLinePositions }

PROCEDURE CalculateBufferStopPositions(ScaleFactor : Integer);
{ Work out where the buffer stops are on the screen }
VAR
  BufferStop : Integer;

BEGIN
  BufferStop := 0;
  WHILE BufferStop <= High(BufferStops) DO BEGIN
    WITH BufferStops[BufferStop] DO BEGIN
      IF BufferStop_Direction = Up THEN BEGIN
        BufferStop_X := Lines[BufferStop_AdjacentLine].Line_UpX;
        BufferStop_Y1 := Lines[BufferStop_AdjacentLine].Line_UpY - BufferStopVerticalSpacingScaled;
        BufferStop_Y2 := Lines[BufferStop_AdjacentLine].Line_UpY + BufferStopVerticalSpacingScaled;
      END ELSE BEGIN
        BufferStop_X := Lines[BufferStop_AdjacentLine].Line_DownX;
        BufferStop_Y1 := Lines[BufferStop_AdjacentLine].Line_DownY - BufferStopVerticalSpacingScaled;
        BufferStop_Y2 := Lines[BufferStop_AdjacentLine].Line_DownY + BufferStopVerticalSpacingScaled;
      END;

      { The mouse rectangle }
      WITH BufferStop_MouseRect DO BEGIN
        Left := BufferStop_X - MulDiv(FWPRailWindow.ClientWidth, 5, ScaleFactor);
        Top := BufferStop_Y1;
        Right := BufferStop_X + MulDiv(FWPRailWindow.ClientWidth, 5, ScaleFactor);
        Bottom := BufferStop_Y2;
      END; { WITH }
    END; { WITH }
    Inc(BufferStop);
  END; { WHILE }
END; { CalculateBufferStopPositions }

PROCEDURE ReadInLineDataFromDatabase;
{ Initialise the data for each of the line segments }
CONST
  Horizontal = True;
  StopTimer = True;

VAR
  ErrorMsg : String;
  FieldName : String;
  L : Integer;
  OtherL : Integer;
  SaveLine : Integer;
  TempStr : String;
  TempLine : Integer;

  PROCEDURE CreateBufferStop(L, BSNumber : Integer; BSDirection : DirectionType; BSTheatreDestination : String);
  { Create a bufferstop record }
  BEGIN
    SetLength(BufferStops, Length(BufferStops) + 1);

    Lines[L].Line_AdjacentBufferStop := High(BufferStops);

    WITH BufferStops[High(BufferStops)] DO BEGIN
      BufferStop_AdjacentLine := L;
      BufferStop_AdjacentTrackCircuit := Lines[BufferStop_AdjacentLine].Line_TC;
      IF (BufferStop_AdjacentTrackCircuit = UnknownTrackCircuit) AND ProgramStartup THEN
        Log('E Buffer stop ' + IntToStr( High(BufferStops)) + ' (at line' + ' ' + Lines[BufferStop_AdjacentLine].Line_Str + ') has no adjacent trackcircuit');

      BufferStop_AsTheatreDestination := BSTheatreDestination;
      BufferStop_CurrentColour := BufferStopColour;
      BufferStop_Direction := BSDirection;
      BufferStop_Number := BSNumber;
    END; { WITH }
  END; { CreateBufferStop }

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

      LineDataADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix
                                                + ';Persist Security Info=False';
      LineDataADOConnection.Connected := True;
      LineDataADOTable.Open;
      Log('T Line data table and connection opened to initialise the lines');

      LineDataADOTable.First;
      L := -1;
      FieldName := 'LineNum';
      WHILE NOT LineDataADOTable.EOF DO BEGIN
        Inc(L);
        IF LineDataADOTable.FieldByName(FieldName).AsInteger <> L THEN BEGIN
          { we need to renumber from here on }
          LineDataADOTable.Edit;
          LineDataADOTable.FieldByName('LineNum').AsInteger := L;
          LineDataADOTable.Post;
        END;
        LineDataADOTable.Next;
      END; { WHILE }

      LineDataADOTable.Sort := 'LineNum ASC';
      LineDataADOTable.First;

      { and make sure the arrays are empty, otherwise the next time the screen is resized we will merely add more lines/bufferstops }
      SetLength(Lines, 0);
      SetLength(BufferStops, 0);

      WHILE NOT LineDataADOTable.EOF DO BEGIN
        WITH LineDataADOTable DO BEGIN
          SetLength(Lines, Length(Lines) + 1);
          L := High(Lines);
          WITH Lines[L] DO BEGIN
            ErrorMsg := '';

            Line_UpXAbsolute := 0;
            Line_UpX := 0;
            Line_DownXAbsolute := 0;
            Line_DownX := 0;

            Line_UpYAbsolute := 0;
            Line_UpY := 0;
            Line_DownYAbsolute := 0;
            Line_DownY := 0;

            Line_DownConnectionCh := '';
            Line_AdjacentBufferStop := UnknownBufferStop;
            Line_LockFailureNotedInSubRouteUnit := False;
            Line_NextUpIsEndofLine := NotEndOfLine;
            Line_NextDownIsEndOfLine := NotEndOfLine;
            Line_NextDownPoint := UnknownPoint;
            Line_NextDownType := UnknownNextLineRouteingType;
            Line_NextDownLine := UnknownLine;
            Line_NextUpLine := UnknownLine;
            Line_NextUpPoint := UnknownPoint;
            Line_RouteLockingForDrawing := UnknownRoute;
            Line_RouteSet := UnknownRoute;
            Line_UpConnectionCh := '';
            Line_UpXValueSpecified := False;

            FieldName := 'LineNum';
            Line_TempNum := FieldByName(FieldName).AsInteger;
            IF Line_TempNum <> L THEN
              ErrorMsg := 'it does not match the line number in the database (' + IntToStr(Line_TempNum) + ')';

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Line';
              Line_Str := FieldByName(FieldName).AsString;

              TempLine := 0;
              WHILE TempLine <= High(Lines) - 1 DO BEGIN
                { check all lines apart from the one we've just created }
                IF Line_Str = Lines[TempLine].Line_Str THEN
                  ErrorMsg := 'duplicate line name found';
                Inc(TempLine);
              END; { WHILE }

              FieldName := 'Up X Line';
              Line_UpXLineStr := FieldByName(FieldName).AsString;
              IF Line_Str = Line_UpXLineStr THEN
                ErrorMsg := 'UpXLine cannot be the same value as Line';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Up X';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr <> '' THEN BEGIN
                IF NOT TryStrToInt(TempStr, Line_UpXAbsolute) THEN
                  ErrorMsg := 'invalid UpX integer "' + TempStr + '"'
                ELSE
                  Line_UpXValueSpecified := True;
              END
              ELSE
                IF Line_UpXLineStr = '' THEN
                  ErrorMsg := 'must have a Line_UpXLine value if UpX is zero (Line_UpXLineStr=' + Line_UpXLineStr + ')';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Length';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr <> '' THEN BEGIN
                IF NOT TryStrToInt(TempStr, Line_Length) THEN
                  ErrorMsg := 'invalid length integer';
                IF Line_UpXAbsolute <> 0 THEN
                  Line_DownXAbsolute := Line_UpXAbsolute + Line_Length;
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Up Y Location';
              Line_UpYLocationStr := FieldByName(FieldName).AsString;
              IF Line_UpYLocationStr = '' THEN
                Line_UpYLocation := UnknownLocation;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Up Y';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Line_UpYAbsolute := 0
              ELSE BEGIN
                IF NOT TryStrToInt(TempStr, Line_UpYAbsolute) THEN
                  ErrorMsg := 'invalid UpY integer "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Down Y Location';
              Line_DownYLocationStr := FieldByName(FieldName).AsString;
              IF Line_DownYLocationStr = '' THEN
                Line_DownYLocation := UnknownLocation;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Down Y';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Line_DownYAbsolute := 0
              ELSE BEGIN
                IF NOT TryStrToInt(TempStr, Line_DownYAbsolute) THEN
                  ErrorMsg := 'invalid DownY integer "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Location';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Line_Location := UnknownLocation
              ELSE BEGIN
                Line_Location := StrToLocation(TempStr);
                IF Line_Location = UnknownLocation THEN
                  ErrorMsg := 'unknown line location "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Line TC';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Line_TC := UnknownTrackCircuit
              ELSE
                IF NOT TryStrToInt(TempStr, Line_TC) THEN
                  ErrorMsg := 'invalid line TC integer "' + TempStr + '"'
                ELSE
                  IF ((Line_TC < 0) AND (Line_TC > High(TrackCircuits))) OR (Line_TC = UnknownTrackCircuit) THEN
                    ErrorMsg := 'trackcircuit number ' + IntToStr(Line_TC) + ' outside bounds';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'End Of Line Marker';
              TempStr := FieldByName(FieldName).AsString;
              Line_EndOfLineMarker := StrToEndOfLine(TempStr);
              IF (TempStr = '') OR (Line_EndOfLineMarker = UnknownEndOfLine) THEN
                ErrorMsg := 'unknown end of line marker "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Buffer Stop Number';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Line_BufferStopNum := UnknownBufferStop
              ELSE
                IF NOT TryStrToInt(TempStr, Line_BufferStopNum) THEN
                  ErrorMsg := 'invalid buffer stop number "' + TempStr + '"';

              TempLine := 0;
              WHILE TempLine <= High(Lines) - 1 DO BEGIN
                { check all lines apart from the one we've just created }
                IF (Line_BufferStopNum <> UnknownBufferStop) AND (Line_BufferStopNum = Lines[TempLine].Line_BufferStopNum) THEN
                  ErrorMsg := 'duplicate buffer stop number ' + IntToStr(Line_BufferStopNum) + ' found';
                Inc(TempLine);
              END; { WHILE }
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Buffer Stop Theatre Destination';
              Line_BufferStopTheatreDestinationStr := FieldByName(FieldName).AsString;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Direction';
              TempStr := FieldByName(FieldName).AsString;
              Line_Direction := StrToDirectionType(TempStr);
              IF (TempStr = '') OR (Line_Direction = UnknownDirection) THEN
                ErrorMsg := 'unknown line direction "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Type Of Line';
              TempStr := FieldByName(FieldName).AsString;
              Line_TypeOfLine := StrToTypeOfLine(TempStr);
              IF (TempStr = '') OR (Line_TypeOfLine = UnknownTypeOfLine) THEN
                ErrorMsg := 'unknown type of line "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Up Connection Ch';
              Line_UpConnectionCh := FieldByName(FieldName).AsString;
              IF Length(Line_UpConnectionCh) > 1 THEN
                ErrorMsg := 'line up connection ch is too long "' + TempStr + '"';
              Line_UpConnectionChBold := False;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Down Connection Ch';
              Line_DownConnectionCh := FieldByName(FieldName).AsString;
              IF Length(Line_DownConnectionCh) > 1 THEN
                ErrorMsg := 'line down connection ch is too long "' + TempStr + '"';
              Line_DownConnectionChBold := False;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Gradient';
              TempStr := FieldByName(FieldName).AsString;
              Line_Gradient := StrToGradient(TempStr);
              IF (TempStr = '') OR (Line_Gradient = UnknownGradientType) THEN
                ErrorMsg := 'unknown gradient type "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Out Of Use';
              IF FieldByName(FieldName).AsBoolean THEN BEGIN
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

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'In Use Feedback Unit';
              TempStr := FieldByName(FieldName).AsString;
              IF (TempStr <> '') AND NOT TryStrToInt(TempStr, Line_InUseFeedbackUnit) THEN
                ErrorMsg := 'invalid line in use feedback unit "' + TempStr + '"';
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'In Use Feedback Input';
              TempStr := FieldByName(FieldName).AsString;
              IF (TempStr <> '') AND NOT TryStrToInt(TempStr, Line_InUseFeedbackInput) THEN
                ErrorMsg := 'invalid line in use feedback unit "' + TempStr + '"';
            END;

            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating Line=' + IntToStr(High(Lines)) + ' (' + Line_Str + '): '
                                            + '[' + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
            END;
          END;
        END; { WITH }
        LineDataADOTable.Next;
      END; { WHILE }

      { Tidy up the database }
      LineDataADOTable.Close;
      LineDataADOConnection.Connected := False;
      Log('T Line Data table and connection closed');
    END; { WITH }

    { Now we have to do certain tests as some of the data was not be available while we read it in). }
    FOR L := 0 TO High(Lines) DO BEGIN
      WITH Lines[L] DO BEGIN
        IF Line_UpXLineStr <> '' THEN BEGIN
          TempLine := StrToLine(Line_UpXLineStr);
          IF TempLine = UnknownLine THEN
            IF MessageDialogueWithDefault('Line ' + LineToStr(L) + ': invalid Line_UpXLine value "' + Line_UpXLineStr + '"',
                                          StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
            THEN
              ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
        END;
      END; { WITH }
    END; { FOR }

    { see if there are any duplicate connection characters }
    FOR L := 0 TO High(Lines) DO BEGIN
      WITH Lines[L] DO BEGIN
        FOR OtherL := 0 TO High(Lines) DO BEGIN
          IF L <> OtherL THEN BEGIN
            IF (Line_UpConnectionCh <> '') AND (Line_UpConnectionCh = Lines[OtherL].Line_UpConnectionCh) THEN BEGIN
              IF MessageDialogueWithDefault('Duplicate up connection character ''' + Line_UpConnectionCh + ''''
                                            + ' found at line ' + LineToStr(L) + ' and at ' + LineToStr(OtherL),
                                            StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
              THEN
                ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
            END
            ELSE
              IF (Line_DownConnectionCh <> '') AND (Line_DownConnectionCh = Lines[OtherL].Line_DownConnectionCh) THEN BEGIN
                IF MessageDialogueWithDefault('Duplicate down connection character ''' + Line_DownConnectionCh + '''' +
                                              ' found at line ' + LineToStr(L) + ' and at ' + LineToStr(OtherL),
                                              StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
                THEN
                  ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
              END;
          END;
        END; { FOR }
      END; { WITH }
    END; { FOR }

    { Now work out the X and Y values }
    CalculateLinePositions(1000);

    FOR L := 0 TO High(Lines) DO BEGIN
      WITH Lines[L] DO BEGIN
        { In all cases, UpX should be less than DownX }
        IF Line_UpX > Line_DownX THEN BEGIN
          IF MessageDialogueWithDefault('Line ' + Line_Str + ': UpX > DownX',
                                        StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
          THEN
            ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
        END;
      END; { WITH }
    END; { FOR }

    FOR L := 0 TO High(Lines) DO BEGIN
      WITH Lines[L] DO BEGIN
        Line_CurrentColour := TCUnoccupiedColour;
        Line_OldColour := TCUnoccupiedColour;
        Line_RoutedOver := False;
        Line_NoLongerOutOfUse := False;

        CASE Line_EndOfLineMarker OF
          NotEndOfLine : BEGIN
              Line_NextUpType := LineIsNext;
              Line_NextUpIsEndofLine := NotEndOfLine;
              Line_NextDownType := LineIsNext;
              Line_NextDownIsEndOfLine := NotEndOfLine;
            END;
          BufferStopAtUp : BEGIN
              Line_NextUpType := EndOfLineIsNext;
              Line_NextUpIsEndofLine := BufferStopAtUp;
              Line_NextDownType := LineIsNext;
              { and create a bufferstop record }
              CreateBufferStop(L, Line_BufferStopNum, Up, Line_BufferStopTheatreDestinationStr);
            END;
          BufferStopAtDown : BEGIN
              Line_NextDownType := EndOfLineIsNext;
              Line_NextDownIsEndOfLine := BufferStopAtDown;
              Line_NextUpType := LineIsNext;
              { and create a bufferstop record }
              CreateBufferStop(L, Line_BufferStopNum, Down, Line_BufferStopTheatreDestinationStr);
            END;
          ProjectedLineAtUp : BEGIN
              Line_NextUpType := EndOfLineIsNext;
              Line_NextUpIsEndofLine := ProjectedLineAtUp;
              Line_NextDownType := LineIsNext;
            END;
          ProjectedLineAtDown : BEGIN
              Line_NextDownType := EndOfLineIsNext;
              Line_NextDownIsEndOfLine := ProjectedLineAtDown;
              Line_NextUpType := LineIsNext;
            END;
        END; { CASE }

        IF Line_NextUpType = UnknownNextLineRouteingType THEN BEGIN
          IF MessageDialogueWithDefault('Line_NextUpType = UnknownNextLineRouteingType at ' + LineToStr(L),
                                        StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
          THEN
            ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
        END
        ELSE
          IF Line_NextDownType = UnknownNextLineRouteingType THEN BEGIN
            IF MessageDialogueWithDefault('Line_NextDownType = UnknownNextLineRouteingType at ' + LineToStr(L),
                                          StopTimer, mtError, [mbOK, mbAbort], mbAbort) = mrAbort
            THEN
              ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
          END;

      END; { WITH }
    END; { FOR }

    CalculateBufferStopPositions(1000);

    { Now they all exist, we can work out which are next to which }
    FOR L := 0 TO High(Lines) DO BEGIN
      WITH Lines[L] DO BEGIN
        FOR OtherL := 0 TO High(Lines) DO BEGIN
          IF L <> OtherL THEN BEGIN
            { First, see if a line is disconnected from its neighbour - look for an alphabetic character at its start or end }
            IF (Line_UpConnectionCh <> '') AND (Line_UpConnectionCh = Lines[OtherL].Line_DownConnectionCh) THEN BEGIN
              Line_NextUpType := LineIsNext;
              Line_NextUpLine := OtherL
            END
            ELSE
              IF (Line_DownConnectionCh <> '') AND (Line_DownConnectionCh = Lines[OtherL].Line_UpConnectionCh) THEN BEGIN
                Line_NextDownType := LineIsNext;
                Line_NextDownLine := OtherL
              END ELSE BEGIN
                { Now look for normal horizontal connections }
                IF (Line_DownX = Lines[OtherL].Line_UpX) AND (Line_DownY = Lines[OtherL].Line_UpY) AND
                  (Line_UpY = Lines[OtherL].Line_DownY) THEN BEGIN
                  Line_NextDownType := LineIsNext;
                  Line_NextDownLine := OtherL;
                END;

                IF (Line_UpX = Lines[OtherL].Line_DownX) AND (Line_UpY = Lines[OtherL].Line_DownY) AND
                  (Line_DownY = Lines[OtherL].Line_UpY) THEN BEGIN
                  Line_NextUpType := LineIsNext;
                  Line_NextUpLine := OtherL;
                END;
              END;

            { If there are no normal horizontal connections, see if there's a non-horizontal one }
            IF (Line_NextUpLine = UnknownLine) OR (Line_NextDownLine = UnknownLine) THEN BEGIN
              IF (Line_DownX = Lines[OtherL].Line_UpX) AND (Line_DownY = Lines[OtherL].Line_UpY) THEN BEGIN
                Line_NextDownType := LineIsNext;
                Line_NextDownLine := OtherL;
              END;

              IF (Line_UpX = Lines[OtherL].Line_DownX) AND (Line_UpY = Lines[OtherL].Line_DownY) THEN BEGIN
                Line_NextUpType := LineIsNext;
                Line_NextUpLine := OtherL;
              END;
            END;
          END;
        END;
      END; { WITH }
    END; { FOR }

    { And check for lines which are declared but not initialised }
    SaveLine := UnknownLine;
    FOR L := 0 TO High(Lines) DO BEGIN
      IF LineToStr(L) = '' THEN
        IF L = 0 THEN BEGIN
          IF MessageDialogueWithDefault('First line declared in Initvars but not initialised in RailDraw - do you wish to continue?',
                                        StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
          THEN
            ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
        END ELSE BEGIN
          IF MessageDialogueWithDefault('Line declared in Initvars but not initialised in RailDraw (after line "' + Lines[SaveLine].Line_Str + '")'
                                        + ' - do you wish to continue?',
                                        StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
          THEN
            ShutDownProgram(UnitRef, 'ReadInLineDataFromDatabase');
        END;
      SaveLine := L;
    END; { FOR }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG ReadInLineDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }

(* TP { This is left over from the Turbo Pascal version }
    DX := (X2 - X1) / AspectRatio;
    { AMS incomprehensible comment follows: }
    { / not * cos here we are moving from screen coordinates to normalised coordinates not vice versa }
    DY := Y2 - Y1;

    { Scale to give accurate speeds }
    LineLength := Round(20 * Sqrt(DX * DX + DY * DY));
TP *)
END; { ReadInLineDataFromDatabase }

PROCEDURE WriteOutLineDataToDatabase;
{ Write out some line data to the line data file }
VAR
  L : Integer;

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

      LineDataADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + LineDataFilename + '.' + LineDataFilenameSuffix
                                                + ';Persist Security Info=False';
      LineDataADOConnection.Connected := True;
      LineDataADOTable.Open;
      Log('L Loco Data table and connection opened to write out line data');

      L := 0;
      WHILE L <= High(Lines) DO BEGIN
        WITH Lines[L] DO BEGIN
          IF ((Line_OutOfUseState = OutOfUse)
              OR ((Line_Location <> UnknownLocation) AND Locations[Line_Location].Location_OutOfUse))
          AND (Line_InitialOutOfUseState = InUse)
          THEN BEGIN
            LineDataADOTable.First;
            WHILE NOT LineDataADOTable.EOF DO BEGIN
              WITH LineDataADOTable DO BEGIN
                IF FieldByName('Line').AsString = Line_Str THEN BEGIN
                  Edit;
                  FieldByName('Out Of Use').AsBoolean := True;
                  Post;
                END;
              END; { WITH }
              LineDataADOTable.Next;
            END; { WHILE }
            LineDataADOTable.Close;
          END ELSE
            IF (Line_OutOfUseState = InUse)
            AND (Line_InitialOutOfUseState = OutOfUse)
            THEN BEGIN
              LineDataADOTable.First;
              WHILE NOT LineDataADOTable.EOF DO BEGIN
                WITH LineDataADOTable DO BEGIN
                  IF FieldByName('Line').AsString = Line_Str THEN BEGIN
                    Edit;
                    FieldByName('Out Of Use').AsBoolean := False;
                    Post;
                  END;
                END; { WITH }
                LineDataADOTable.Next;
              END; { WHILE }
              LineDataADOTable.Close;
            END;
        END; { WITH }
        Inc(L);
      END; { WHILE }

// LineDataADOTable.First;
// WHILE NOT LineDataADOTable.EOF DO BEGIN
// WITH LineDataADOTable DO BEGIN
// L := 0;
// WHILE L <= High(Lines) DO BEGIN
// WITH Lines[L] DO BEGIN
// debug(inttostr(l));
// IF FieldByName('Line').AsString = Line_Str THEN BEGIN
// IF (Line_Location <> UnknownLocation)
// AND Locations[Line_Location].Location_OutOfUse
// THEN
// TempOutOfUse := Line_SaveOutOfUse
// ELSE
// TempOutOfUse := Line_OutOfUse;
//
// IF TempOutOfUse <> FieldByName('Out Of Use').AsBoolean THEN BEGIN
// Edit;
// FieldByname('Out Of Use').AsBoolean := Line_outOfUse;
// Post;
// END;
// END; {WITH}
// END;
// Inc(L);
// END;
// END; {WITH}
// LineDataADOTable.Next;
// END; {WHILE}
//
      { Tidy up the database }
      LineDataADOTable.Close;
      LineDataADOConnection.Connected := False;
      Log('L Line Data table and connection closed after writing line data');
    END; { WITH }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG WriteOutLineData: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
END; { WriteOutLineDataToDatabase }

PROCEDURE CalculateSignalPositions(ScaleFactor : Integer);
{ Work out where the signals are on the screen }
VAR
  S : Integer;

BEGIN
  TRY
    S := 0;
    WHILE S <= High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF Signal_AdjacentLine <> UnknownLine THEN BEGIN
          IF Signal_Direction = Up THEN BEGIN
            Signal_LocationX := Lines[Signal_AdjacentLine].Line_UpX + SignalRadiusScaled;
            IF Signal_Indicator <> NoIndicator THEN
              Signal_LocationX := Signal_LocationX + SignalHorizontalSpacingScaled;
            IF Signal_Type = FourAspect THEN
              Signal_LocationX := Signal_LocationX + SignalHorizontalSpacingScaled;
          END ELSE BEGIN
            { Down }
            Signal_LocationX := Lines[Signal_AdjacentLine].Line_DownX - SignalRadiusScaled;
            IF Signal_Indicator <> NoIndicator THEN
              Signal_LocationX := Signal_LocationX - SignalHorizontalSpacingScaled;
            IF Signal_Type = FourAspect THEN
              Signal_LocationX := Signal_LocationX - SignalHorizontalSpacingScaled;
          END;

          { Adjust left or right if XAdjustment greater than or less than zero respectively }
          IF Signal_XAdjustment > 0 THEN
            Signal_LocationX := Signal_LocationX + MulDiv(FWPRailWindow.ClientWidth, Signal_XAdjustment, ScaleFactor)
          ELSE
            IF Signal_XAdjustment < 0 THEN
              Signal_LocationX := Signal_LocationX - MulDiv(FWPRailWindow.ClientWidth, Abs(Signal_XAdjustment), ScaleFactor);

          Signal_LocationY := Lines[Signal_AdjacentLine].Line_UpY;

          Signal_VerticalSpacing := SignalVerticalSpacingScaled;

          IF Signal_Direction = Up THEN
            Signal_LocationY := Signal_LocationY + Signal_VerticalSpacing
          ELSE
            IF Signal_Direction = Down THEN
              Signal_LocationY := Signal_LocationY - Signal_VerticalSpacing;

          { Set up mouse access rectangles }
          WITH Signal_MouseRect DO BEGIN
            RailWindowBitmap.Canvas.Pen.Width := WindowPenWidth;

            IF (Signal_Type <> SemaphoreHome) AND (Signal_Type <> SemaphoreDistant) THEN BEGIN
              { it covers the signal circles }
              Left := Signal_LocationX - SignalRadiusScaled;
              Top := Signal_LocationY - SignalRadiusScaled;
              Right := Signal_LocationX + SignalRadiusScaled;
              Bottom := Signal_LocationY + SignalRadiusScaled;
            END ELSE BEGIN
              { it covers the signal arms }
              IF (Signal_Direction = Up) AND (Signal_Quadrant = UpperQuadrant) THEN BEGIN
                Left := Signal_LocationX - SignalSemaphoreWidthScaled;
                Top := Signal_LocationY + RailWindowBitmap.Canvas.Pen.Width;
                Right := Signal_LocationX + (SignalSemaphoreHeightScaled * 2);
                Bottom := Signal_LocationY + (SignalSemaphoreWidthScaled * 2);
              END ELSE
                IF (Signal_Direction = Up) AND (Signal_Quadrant = LowerQuadrant) THEN BEGIN
                  Left := Signal_LocationX;
                  Top := Signal_LocationY + RailWindowBitmap.Canvas.Pen.Width;
                  Right := Signal_LocationX + (SignalSemaphoreHeightScaled * 2) + SignalSemaphoreWidthScaled;
                  Bottom := Signal_LocationY + (SignalSemaphoreWidthScaled * 2);
                END ELSE
                  IF (Signal_Direction = Down) AND (Signal_Quadrant = UpperQuadrant) THEN BEGIN
                    Left := Signal_LocationX - SignalSemaphoreWidthScaled;
                    Top := Signal_LocationY - (SignalSemaphoreWidthScaled * 2);
                    Right := Signal_LocationX + (SignalSemaphoreHeightScaled * 2);
                    Bottom := Signal_LocationY - RailWindowBitmap.Canvas.Pen.Width;
                  END ELSE
                    IF (Signal_Direction = Down) AND (Signal_Quadrant = LowerQuadrant) THEN BEGIN
                      Left := Signal_LocationX - (SignalSemaphoreHeightScaled * 2) - SignalSemaphoreWidthScaled;
                      Top := Signal_LocationY - (SignalSemaphoreWidthScaled * 2);
                      Right := Signal_LocationX;
                      Bottom := Signal_LocationY - RailWindowBitmap.Canvas.Pen.Width;
                    END;
            END;
          END; { WITH }

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
              END; { WITH }
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
                  END; { WITH }
                END;
              END;
            END;

          { Now the signal posts (used for routeing) }
          WITH Signal_PostMouseRect DO BEGIN
            IF Signal_Direction = Up THEN BEGIN
              { pen.width is the width of the line outlining the signal }
              Left := Signal_LocationX + SignalRadiusScaled;
              Top := Signal_LocationY - SignalRadiusScaled;
              Right := Signal_LocationX + SignalRadiusScaled + MulDiv(FWPRailWindow.ClientWidth, 10, ZoomScalefactor);
              Bottom := Signal_LocationY + SignalRadiusScaled;
            END ELSE
              IF Signal_Direction = Down THEN BEGIN
                Left := Signal_LocationX - SignalRadiusScaled - MulDiv(FWPRailWindow.ClientWidth, 10, ZoomScalefactor);
                Top := Signal_LocationY - SignalRadiusScaled;
                Right := Signal_LocationX - SignalRadiusScaled;
                Bottom := Signal_LocationY + Signal_VerticalSpacing - RailWindowBitmapCanvasPenWidth;
              END;
          END; { WITH }
        END;
      END; { WITH }
      Inc(S);
    END; { WHILE }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG CalculateSignalPositions: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
END; { CalculateSignalPositions }

PROCEDURE CalculateTCAdjacentSignals;
{ Work out which trackcircuits are next the signal }
VAR
  S : Integer;
  TC : Integer;

BEGIN
  FOR TC := 0 TO High(TrackCircuits) DO
    SetLength(TrackCircuits[TC].TC_AdjacentSignals, 0);

  FOR S := 0 TO High(Signals) DO
    AppendToIntegerArray(TrackCircuits[Lines[Signals[S].Signal_AdjacentLine].Line_TC].TC_AdjacentSignals, S);
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
    END; { WHILE }
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

FUNCTION ValidateDirection(Str : String; OUT ErrorMsg : String) : DirectionType;
{ Validates and if ok returns the signal direction }
BEGIN
  ErrorMsg := '';

  Result := StrToDirectionType(Str);
  IF Result = UnknownDirection THEN
    ErrorMsg := 'ValidateDirection: unknown signal direction';
END; { ValidateDirection }

FUNCTION ValidateIndicatorSpeedRestriction(Str : String; Indicator : IndicatorType; OUT ErrorMsg : String) : MPHType;
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
      ErrorMsg := 'ValidateIndicatorSpeedRestriction: invalid integer String "' + Str + '"';

    IF ErrorMsg = '' THEN
      IF (Result <> NoSpecifiedSpeed) AND (Indicator = NoIndicator) THEN
        ErrorMsg := 'ValidateIndicatorSpeedRestriction: cannot have an indicator speed restriction if there is no indicator';
  END;
END; { ValidateIndicatorSpeedRestriction }

FUNCTION ValidateJunctionIndicators1(Str, FieldName : String; Signal_Indicator : IndicatorType; OUT ErrorMsg : String) : JunctionIndicatorRec;
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
      ErrorMsg := 'ValidateJunctionIndicators1: invalid target signal (S) or bufferstop (BS) for ' + FieldName + ': "' + Str + '"'
    ELSE
      IF Copy(Str, 1, 1) = 'S' THEN BEGIN
        Str := Copy(Str, 2);
        IF (Str = '') OR NOT TryStrToInt(Trim(Str), Result.JunctionIndicator_TargetSignal) THEN
          ErrorMsg := 'ValidateJunctionIndicators1: invalid target signal (S) or bufferstop (BS) for ' + FieldName + ': "' + Str + '"';
      END
      ELSE
        IF Copy(Str, 1, 2) = 'BS' THEN BEGIN
          Str := Copy(Str, 3);
          IF (Str = '') OR NOT TryStrToInt(Trim(Str), Result.JunctionIndicator_TargetBufferStop) THEN
            ErrorMsg := 'ValidateJunctionIndicators1: invalid target signal (S) or bufferstop (BS) for ' + FieldName + ': "' + Str + '"';
        END;

    IF ErrorMsg = '' THEN
      Result.JunctionIndicator_Exists := True;
  END;
END; { ValidateJunctionIndicators1 }

PROCEDURE ValidateJunctionIndicators2(StrArray : ARRAY OF String; SignalIndicator : IndicatorType; SignalJunctionIndicators : ARRAY OF JunctionIndicatorRec;
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
          ErrorMsg := 'ValidateJunctionIndicators2: identical signal/bufferstopdata (' + StrArray[I] + ') in indicator fields';
      END;
      Inc(J);
    END; { WHILE }
    Inc(I);
  END; { WHILE }

  IF ErrorMsg = '' THEN BEGIN
    IndicatorsFound := False;
    FOR I := 0 TO 5 DO BEGIN
      IF SignalJunctionIndicators[I].JunctionIndicator_Exists THEN
        IndicatorsFound := True;
    END; { FOR }

    IF (SignalIndicator = JunctionIndicator) AND NOT IndicatorsFound THEN
      ErrorMsg := 'ValidateJunctionIndicators2: signal indicator type is set to Junction Indicator even though there is no indicator data'
    ELSE
      IF (SignalIndicator <> JunctionIndicator) AND IndicatorsFound THEN
        ErrorMsg := 'ValidateJunctionIndicators2: signal indicator type is not set to Junction Indicator even though there is indicator data';
  END;
END; { ValidateJunctionIndicators2 }

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
END; { ValidateNextSignalIfNoIndicator }

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
      END; { WHILE }
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

FUNCTION ValidateSignalOutOfUseAndAddAdjacentTC(S : Integer; Flag : Boolean; AdjacentLine : Integer; OUT AdjacentTC : Integer; OUT ErrorMsg : String) : Boolean;
{ Validates and if ok returns true if a signal is marked as not in use. This test must be done after Adjacent Signal is validated. }
BEGIN

  ErrorMsg := '';
  Result := Flag;

  { Note which lines and trackcircuits are next the signal }
  IF NOT Flag THEN BEGIN
    AdjacentTC := Lines[AdjacentLine].Line_TC;
    IF AdjacentTC = UnknownTrackCircuit THEN
      ErrorMsg := 'ValidateSignalOutOfUse: S=' + IntToStr(S) + ' (adjacent to line' + ' ' + LineToStr(AdjacentLine) + ') has no adjacent trackcircuit';
// ELSE
// AppendToIntegerArray(TrackCircuits[Lines[AdjacentLine].Line_TC].TC_AdjacentSignals, S);
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

  Str := UpperCase(Str);

  IF STR = 'UPPER' THEN
    Result := UpperQuadrant
  ELSE
    IF STR = 'LOWER' THEN
      Result := LowerQuadrant
    ELSE
      Result := NoQuadrant;
END; { ValidateQuadrant }

FUNCTION ValidateSignalType(Str : String; Quadrant : QuadrantType; OUT ErrorMsg : String) : TypeOfSignal;
{ Validates and if ok returns the signal type }
BEGIN
  ErrorMsg := '';
  Result := UnknownSignalType;

  Str := UpperCase(Str);
  CASE Str[1] OF
    'C':
      Result := CallingOn;
    '2':
      Result := TwoAspect;
    '3':
      Result := ThreeAspect;
    '4':
      Result := FourAspect;
    'H':
      IF Quadrant = NoQuadrant THEN
        ErrorMsg := 'ValidateSignalType: missing quadrant'
      ELSE
        Result := SemaphoreHome;
    'D':
      IF Quadrant = NoQuadrant THEN
        ErrorMsg := 'ValidateSignalType: missing quadrant'
      ELSE
        Result := SemaphoreDistant;
    ELSE { CASE }
      ErrorMsg := 'ValidateSignalType: invalid signal type';
  END; { CASE }
END; { ValidateSignalType }

FUNCTION ValidateSignalXAdjustment(Str : String; OUT ErrorMsg : String) : Integer;
{ Validates and if ok returns a signal's on-screen adjustment if any }
BEGIN
  ErrorMsg := '';

  IF Str = '' THEN
    Result := -1
  ELSE
    IF NOT TryStrToInt(Trim(Str), Result) THEN
      ErrorMsg := 'ValidateSignalXAdjustment: Invalid integer String "' + Str + '"';
END; { ValidateSignalXAdjustment }

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

      IF (Signal_NextSignalIfNoIndicator <> UnknownSignal)
      AND ((Signal_NextSignalIfNoIndicator < 0)
           OR
           (Signal_NextSignalIfNoIndicator > High(Signals)))
      THEN
        Result := ' target for next signal if no indicator (S' + IntToStr(Signal_NextSignalIfNoIndicator) + ') is not a valid signal number';
    END; { FOR }
  END; { WITH }
END; { ValidateIndicatorDestinations }

FUNCTION ValidateSignalDistantHomesArray(Str : String; OUT ErrorMsg : String) : IntegerArrayType;
{ Validates the signal numbers supplied }
VAR
  I : Integer;
  SignalDataOK : Boolean;
  TempSignal : Integer;
  TempSignalsStrArray : StringArrayType;

BEGIN
  ErrorMsg := '';
  SetLength(Result, 0);

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
        ELSE BEGIN
          IF ValidateSignalNum(TempSignal) = '' THEN BEGIN
            SignalDataOK := False;
            ErrorMsg := 'ValidateSignalDistantHomesArray: unknown signal "' + TempSignalsStrArray[I];
          END ELSE
            AppendToLocationArray(Result, TempSignal);
        END;
      END;
      Inc(I);
    END; { WHILE }
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
  ErrorMsg : String;
  S : Integer;
  TempLineArray : LineArrayType;
  TempStrArray : ARRAY [0..5] OF String;

BEGIN
  TRY
    Log('A READ IN SIGNAL DATA FROM DATABASE  {BLANKLINEBEFORE}');

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
      END; { TRY }

      SignalsADOTable.Open;

      { First see if the signal numbers in the MSAccess file are sequential and, if not, renumber it - we need this or deletions from the MSAccess file will causes problems }
      S := -1;
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
      END; { WHILE }

      Log('S Signal data table and connection opened to initialise the signal data');

      S := -1;
      SignalsADOTable.Sort := '[Signal Number] ASC';
      SignalsADOTable.First;
      WHILE NOT SignalsADOTable.EOF DO BEGIN
        ErrorMsg := '';
        Inc(S);

        IF SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger <> S THEN
          ErrorMsg := 'it does not match the signal number in the database (' + IntToStr(SignalsADOTable.FieldByName(Signal_NumberFieldName).AsInteger) + ')'
        ELSE
          SetLength(Signals, Length(Signals) + 1);

        WITH Signals[S] DO BEGIN
          IF ErrorMsg = '' THEN BEGIN
            Signal_AdjacentTC := UnknownTrackCircuit;
            Signal_ApproachLocked := False;

            IF NewSignalData THEN
              { colour-light signals are set to RedAspect just before they're switched on, unless we're reloading the data; semaphore signals are always set to red aspect }
              Signal_Aspect := RedAspect
            ELSE
              Signal_Aspect := NoAspect;

            Signal_Automatic := False; { not yet implemented }
            Signal_DataChanged := False;
            SetLength(Signal_DistantHomesArray, 0);
            Signal_Energised := False;
            Signal_EnergisedTime := 0;
            Signal_FailedToResetFlag := False;
            Signal_FailMsgWritten := False;
            Signal_FindNextSignalBufferStopMsgWritten := False;
            Signal_HiddenAspect := NoAspect;
            Signal_IndicatorState := NoIndicatorLit;
            Signal_LampIsOn := True;
            Signal_LockFailureNotedInRouteUnit := False;
            Signal_OutOfUseMsgWritten := False;
            SetLength(Signal_LocationsToMonitorArray, 0);
            Signal_PostColour := ForegroundColour;
            Signal_PreviousAspect := NoAspect;
            Signal_PreviousIndicatorState := NoIndicatorLit;
            Signal_PreviousSignal1 := UnknownSignal;
            Signal_PreviousSignal2 := UnknownSignal;
            Signal_PreviousHiddenAspectSignal1 := UnknownSignal;
            Signal_PreviousHiddenAspectSignal2 := UnknownSignal;
            Signal_PreviousTheatreIndicatorString := '';
            Signal_ResettingTC := UnknownTrackCircuit;
            Signal_StateChanged := False;
            Signal_TheatreIndicatorString := '';
            Signal_TRSHeld := False;
            Signal_TRSHeldMsgWritten := False;
            Signal_TRSReleased := False;
            Signal_Type := TwoAspect;
            Signal_VerticalSpacing := 0;
          END;

          IF ErrorMsg = '' THEN
            Signal_Quadrant := ValidateSignalQuadrant(SignalsADOTable.FieldByName(Signal_QuadrantFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            { this is to indicate which semaphore homes control the distant }
            Signal_DistantHomesArray := ValidateSignalDistantHomesArray(SignalsADOTable.FieldByName(Signal_DistantHomesArrayFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            IF SignalsADOTable.FieldByName(Signal_TypeFieldName).AsString = '' THEN
              ErrorMsg := 'no signal type given'
            ELSE
              Signal_Type := ValidateSignalType(SignalsADOTable.FieldByName(Signal_TypeFieldName).AsString, Signal_Quadrant, ErrorMsg);
          END;

          { NB the signal fields are in the following order for validation purposes }
          Signal_Indicator := NoIndicator;
          IF ErrorMsg = '' THEN
            Signal_Indicator := ValidateSignalIndicator(SignalsADOTable.FieldByName(Signal_IndicatorFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[0] := SignalsADOTable.FieldByName(Signal_UpperLeftIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[UpperLeftIndicator] := ValidateJunctionIndicators1(TempStrArray[0], Signal_UpperLeftIndicatorTargetFieldName, Signal_Indicator,
                                                                                         ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[1] := SignalsADOTable.FieldByName(Signal_MiddleLeftIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[MiddleLeftIndicator] := ValidateJunctionIndicators1(TempStrArray[1], Signal_MiddleLeftIndicatorTargetFieldName, Signal_Indicator,
                                                                                          ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[2] := SignalsADOTable.FieldByName(Signal_LowerLeftIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[LowerLeftIndicator] := ValidateJunctionIndicators1(TempStrArray[2], Signal_LowerLeftIndicatorTargetFieldName, Signal_Indicator,
                                                                                         ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[3] := SignalsADOTable.FieldByName(Signal_UpperRightIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[UpperRightIndicator] := ValidateJunctionIndicators1(TempStrArray[3], Signal_UpperRightIndicatorTargetFieldName, Signal_Indicator,
                                                                                          ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[4] := SignalsADOTable.FieldByName(Signal_MiddleRightIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[MiddleRightIndicator] := ValidateJunctionIndicators1(TempStrArray[4], Signal_MiddleRightIndicatorTargetFieldName, Signal_Indicator,
                                                                                           ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            TempStrArray[5] := SignalsADOTable.FieldByName(Signal_LowerRightIndicatorTargetFieldName).AsString;
            Signal_JunctionIndicators[LowerRightIndicator] := ValidateJunctionIndicators1(TempStrArray[5], Signal_LowerRightIndicatorTargetFieldName, Signal_Indicator,
                                                                                          ErrorMsg);
          END;

          IF ErrorMsg = '' THEN
            ValidateJunctionIndicators2(TempStrArray, Signal_Indicator, Signal_JunctionIndicators, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_AdjacentLine := ValidateSignalAdjacentLine(S, SignalsADOTable.FieldByName(Signal_AdjacentLineFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_XAdjustment := ValidateSignalXAdjustment(SignalsADOTable.FieldByName(Signal_XAdjustmentFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_Direction := ValidateDirection(SignalsADOTable.FieldByName(Signal_DirectionFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_IndicatorSpeedRestriction := ValidateIndicatorSpeedRestriction(SignalsADOTable.FieldByName(Signal_IndicatorSpeedRestrictionFieldName).AsString,
                                                                                  Signal_Indicator, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_NextSignalIfNoIndicator := ValidateNextSignalIfNoIndicator(SignalsADOTable.FieldByName(Signal_NextSignalIfNoIndicatorFieldName).AsString, Init, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_OppositePassingLoopSignal := ValidateSignalOppositePassingLoopSignal(SignalsADOTable.FieldByName(Signal_OppositePassingLoopSignalFieldName).AsString,
                                                                                        Init, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_AsTheatreDestination := ValidateSignalAsTheatreDestination(SignalsADOTable.FieldByName(Signal_AsTheatreDestinationFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Signal_OutOfUse := ValidateSignalOutOfUseAndAddAdjacentTC(S, SignalsADOTable.FieldByName(Signal_OutOfUseFieldName).AsBoolean, Signal_AdjacentLine,
                                                                      Signal_AdjacentTC, ErrorMsg);
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
            Signal_LocationsToMonitorArray := ValidateSignalLocationsToMonitorArray
                                                             (SignalsADOTable.FieldByName(Signal_LocationsToMonitorFieldName).AsString, Signal_PossibleRouteHold, ErrorMsg);
          IF ErrorMsg = '' THEN
            Signal_Automatic := SignalsADOTable.FieldByName(Signal_AutomaticFieldName).AsBoolean;

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
        END; { WITH }
      END; { WHILE }

      { Tidy up the database }
      SignalsADOTable.Close;
      SignalsADOConnection.Connected := False;
      Log('S Signal Data table and connection closed');
    END; { WITH }

    { Check that signal numbers are valid. (We can only do this once all the signal details are read in). }
    S := 0;
    WHILE (S <= High(Signals)) AND (ErrorMsg = '') DO BEGIN
      WITH Signals[S] DO BEGIN
        ErrorMsg := ValidateSignalNum(Signal_NextSignalIfNoIndicator);
        ErrorMsg := ValidateSignalNum(Signal_OppositePassingLoopSignal);
        IF ErrorMsg = '' THEN
          Inc(S);
      END; { WITH }
    END; { WHILE }

    { Check that indicator destinations are valid. (We can only do this once all the signal details are read in, too). }
    IF ErrorMsg = '' THEN BEGIN
      S := 0;
      WHILE (S <= High(Signals)) AND (ErrorMsg = '') DO BEGIN
        ErrorMsg := ValidateIndicatorDestinations(S);
        IF ErrorMsg = '' THEN
          Inc(S);
      END; { WHILE }
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
    CalculateSignalPositions(ZoomScalefactor);
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG ReadInSignalDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
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
      SignalsADOTable.FieldByName('Signal Number').AsInteger := High(Signals);
      SignalsADOTable.Post;

      Log('S Signal data table and connection opened to write out signal data that has changed');
      { Tidy up the database }
      SignalsADOTable.Close;
      SignalsADOConnection.Connected := False;
      Log('S Signal Data table and connection closed');
    END;
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG AddNewRecordToSignalDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
END; { AddNewRecordToSignalDatabase }

FUNCTION DeleteRecordFromSignalDatabaseAndRenumberSignals(SignalNumToDelete : Integer) : Boolean;
{ Remove a record from the signal database }
VAR
  S : Integer;

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
          ShutDownProgram(UnitRef, 'DeleteRecordFromSignalDatabaseAndRenumberSignals')
        ELSE
          Exit;
      END;

      SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix
                                               + ';Persist Security Info=False';
      SignalsADOConnection.Connected := True;
      SignalsADOTable.Open;
      IF NOT SignalsADOTable.Locate(Signal_NumberFieldName, IntToStr(SignalNumToDelete), []) THEN BEGIN
        Log('S Signal data table and connection opened to delete S=' + IntToStr(SignalNumToDelete) + ' but it cannot be found');
      END ELSE BEGIN
        Log('S Signal data table and connection opened to delete S=' + IntToStr(SignalNumToDelete));
        SignalsADOTable.Delete;
        Log('SG S=' + IntToStr(SignalNumToDelete) + ' has been deleted');

        { Now renumber the rest }
        SignalsADOTable.Edit;

        S := SignalNumToDelete + 1;
        Log('S Signal data table and connection opened to write out signal data that has changed');
        IF NOT SignalsADOTable.Locate(Signal_NumberFieldName, IntToStr(S), []) THEN
          Log('SG Cannot renumber subsequent signals as there aren''t any')
        ELSE BEGIN
          S := SignalNumToDelete - 1;
          WHILE NOT SignalsADOTable.EOF DO BEGIN
            Inc(S);

            SignalsADOTable.Edit;
            SignalsADOTable.FieldByName(Signal_NumberFieldName).AsString := IntToStr(S);
            SignalsADOTable.Post;

            SignalsADOTable.Next;
          END; { WHILE }
        END;
        Log('S Recording in Signal database that signals S=' + IntToStr(SignalNumToDelete) + ' to S=' + IntToStr(S) + ' have been renumbered');
        Result := True;
      END;

      { Tidy up the database }
      SignalsADOTable.Close;
      SignalsADOConnection.Connected := False;
      Log('S Signal Data table and connection closed');
    END;
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG AddNewRecordToSignalDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
END; { DeleteRecordFromSignalDatabaseAndRenumberSignals }

PROCEDURE WriteOutSignalDataToDatabase;
{ If a Signal's data has been changed, record it in the database }
VAR
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
      END; { WHILE }

      IF SignalDatabaseNeedsUpdating THEN BEGIN
        IF NOT FileExists(PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix) THEN BEGIN
          IF MessageDialogueWithDefault('Signal database file "' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix + '" cannot be located'
                                        + CRLF
                                        + 'Do you wish to continue?',
                                        StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
          THEN
            ShutDownProgram(UnitRef, 'DeleteRecordFromSignalDatabaseAndRenumberSignals')
          ELSE
            Exit;
        END;

        SignalsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + SignalDataFilename + '.' + SignalDataFilenameSuffix
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

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_AccessoryAddressFieldName
                     + ' is ''' + IntToStr(Signals[S].Signal_AccessoryAddress) + '''');
              TempStr := IntToStr(Signal_AccessoryAddress);
              IF TempStr = '0' THEN
                TempStr := '';
              { The database records a zero as a space in this field }
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AccessoryAddressFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_AdjacentLineFieldName + ' is ''' + LineToStr(Signal_AdjacentLine) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AdjacentLineFieldName).AsString := LineToStr(Signal_AdjacentLine);
              SignalsADOTable.Post;

              { The database records "no aspect" as a space in this field }
              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_ApproachControlAspectFieldName
                     + ' is ''' + AspectToStr(Signal_ApproachControlAspect) + '''');
              TempStr := AspectToStr(Signal_ApproachControlAspect);
              IF UpperCase(TempStr) = 'NO ASPECT' THEN
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_ApproachControlAspectFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_AsTheatreDestinationFieldName + ' is ''' + Signal_AsTheatreDestination + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AsTheatreDestinationFieldName).AsString := Signal_AsTheatreDestination;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_AutomaticFieldName
                     + '  is ''' + IfThen(Signals[S].Signal_Automatic, 'automatic', 'not automatic') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_AutomaticFieldName).AsBoolean := Signal_Automatic;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_DecoderNumFieldName + ' is ''' + IntToStr(Signal_DecoderNum) + '''');
              TempStr := IntToStr(Signal_DecoderNum);
              IF TempStr = '0' THEN
                TempStr := '';
              { The database records a zero as a space in this field }
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_DecoderNumFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_DirectionFieldName + ' is ''' + DirectionToStr(Signal_Direction) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_DirectionFieldName).AsString := DirectionToStr(Signal_Direction, VeryShortStringType);
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorFieldName
                     + ' is ''' + IndicatorToStr(Signal_Indicator, LongStringType) + '''');
              TempStr := IndicatorToStr(Signal_Indicator, ShortStringType);
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorDecoderFunctionNumFieldName
                     + ' is ''' + IntToStr(Signal_IndicatorDecoderFunctionNum) + '''');
              TempStr := IntToStr(Signal_IndicatorDecoderFunctionNum);
              IF TempStr = '0' THEN
                TempStr := '';
              { The database records a zero as a space in this field }
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorDecoderFunctionNumFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorDecoderNumFieldName
                     + ' is ''' + IntToStr(Signal_IndicatorDecoderNum) + '''');
              TempStr := IntToStr(Signal_IndicatorDecoderNum);
              IF TempStr = '0' THEN
                TempStr := '';
              { The database records a zero as a space in this field }
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_IndicatorDecoderNumFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_IndicatorSpeedRestrictionFieldName
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
              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_UpperLeftIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_UpperLeftIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_MiddleLeftIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_MiddleLeftIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_LowerLeftIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_LowerLeftIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_UpperRightIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_UpperRightIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_MiddleRightIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_MiddleRightIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              JunctionIndicatorStr := '';
              IF Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                JunctionIndicatorStr := 'S' + IntToStr(Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal)
              ELSE
                IF Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
                  JunctionIndicatorStr := 'BS' + IntToStr(Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetBufferStop);
              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_LowerRightIndicatorTargetFieldName + ' is ''' + JunctionIndicatorStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_LowerRightIndicatorTargetFieldName).AsString := JunctionIndicatorStr;
              SignalsADOTable.Post;

              TempStr := '';
              { insert a comma between entries }
              IF Length(Signal_LocationsToMonitorArray) > 0 THEN
                FOR I := 0 TO High(Signal_LocationsToMonitorArray) DO
                  TempStr := TempStr + Locations[Signal_LocationsToMonitorArray[I]].Location_LongStr + ',';
              { and remove the comma at the end of the String, if any }
              IF Copy(TempStr, Length(TempStr), 1) = ',' THEN
                TempStr := Copy(TempStr, 1, Length(TempStr) - 1);
              { also remove any spaces in the text - this will help to get around the 250 character length restriction in MSAccess fields }
              TempStr := ReplaceText(TempStr, ' ', '');
              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_LocationsToMonitorFieldName + ' is ''' + TempStr + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_LocationsToMonitorFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_NextSignalIfNoIndicatorFieldName
                     + ' is ''' + IntToStr(Signal_NextSignalIfNoIndicator) + '''');
              TempStr := IntToStr(Signal_NextSignalIfNoIndicator);
              IF TempStr = IntToStr(UnknownSignal) THEN
                TempStr := '';
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NextSignalIfNoIndicatorFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_NotUsedForRouteingFieldName
                     + ' is ''' + IfThen(Signals[S].Signal_NotUsedForRouteing, 'not used for routeing', 'used for routeing') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NotUsedForRouteingFieldName).AsBoolean := Signal_NotUsedForRouteing;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_NotesFieldName + ' is ''' + Signal_Notes + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_NotesFieldName).AsString := Signal_Notes;
              SignalsADOTable.Post;
              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_OppositePassingLoopSignalFieldName
                     + ' is ''' + IntToStr(Signal_OppositePassingLoopSignal) + '''');
              TempStr := IntToStr(Signal_OppositePassingLoopSignal);
              IF TempStr = IntToStr(UnknownSignal) THEN
                { the database records unknown signal as a space in this field }
                TempStr := ''
              ELSE
                TempStr := 'S' + TempStr;
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_OppositePassingLoopSignalFieldName).AsString := TempStr;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_OutOfUseFieldName
                     + ' is ''' + IfThen(Signals[S].Signal_OutOfUse, 'out of use', 'back in use') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_OutOfUseFieldName).AsBoolean := Signal_OutOfUse;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_PossibleRouteHoldFieldName
                     + ' is ''' + IfThen(Signals[S].Signal_PossibleRouteHold, 'a possible route hold', 'not a possible route hold') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_PossibleRouteHoldFieldName).AsBoolean := Signal_PossibleRouteHold;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_PossibleStationStartRouteHoldFieldName +
                    ' is ''' + IfThen(Signals[S].Signal_PossibleStationStartRouteHold,
                                      'a possible station start route hold',
                                      'not a possible station start route hold') + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_PossibleStationStartRouteHoldFieldName).AsBoolean := Signal_PossibleStationStartRouteHold;
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_QuadrantFieldName + ' is ''' + SignalQuadrantToStr(Signal_Quadrant) + '''');
              SignalsADOTable.Edit;
              IF Signal_Quadrant = NoQuadrant THEN
                SignalsADOTable.FieldByName(Signal_QuadrantFieldName).AsString := ''
              ELSE
                SignalsADOTable.FieldByName(Signal_QuadrantFieldName).AsString := SignalQuadrantToStr(Signal_Quadrant);
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_TypeFieldName + ' is ''' + SignalTypeToStr(Signal_Type, LongStringType) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_TypeFieldName).AsString := SignalTypeToStr(Signal_Type, ShortStringType);
              SignalsADOTable.Post;

              Log('S Recording in Signal database that S=' + IntToStr(S) + ' ' + Signal_XAdjustmentFieldName + ' is ''' + IntToStr(Signal_XAdjustment) + '''');
              SignalsADOTable.Edit;
              SignalsADOTable.FieldByName(Signal_XAdjustmentFieldName).AsString := IntToStr(Signal_XAdjustment);
              SignalsADOTable.Post;
            END; { WITH }
          END;
          SignalsADOTable.Next;
        END; { WHILE }

        { Tidy up the database }
        SignalsADOTable.Close;
        SignalsADOConnection.Connected := False;
        Log('S Signal Data table and connection closed');
      END;
    END; { WITH }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG WriteOutSignalDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
END; { WriteOutSignalDataToDatabase }

FUNCTION GetLineAdjacentSignal(Line : Integer) : Integer;
{ Return the signal nearest the line }
VAR
  Found : Boolean;
  S : Integer;

BEGIN
  Result := UnknownSignal;
  Found := False;

  S := 0;
  WHILE (S <= High(Signals)) AND NOT Found DO BEGIN
    IF Signals[S].Signal_AdjacentLine = Line THEN BEGIN
      Found := True;
      Result := S;
    END
    ELSE
      Inc(S);
  END; { WHILE }
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
        IF (Lines[Point_HeelLine].Line_UpX = Lines[Point_StraightLine].Line_DownX) THEN BEGIN
          UpX := Lines[Point_HeelLine].Line_UpX;

          Lines[Point_HeelLine].Line_NextUpType := PointIsNext;
          Lines[Point_HeelLine].Line_NextUpPoint := P;

          Lines[Point_StraightLine].Line_NextDownType := PointIsNext;
          Lines[Point_StraightLine].Line_NextDownPoint := P;

          IF NOT PointIsCatchPoint(P) THEN BEGIN
            Lines[Point_DivergingLine].Line_NextDownType := PointIsNext;
            Lines[Point_DivergingLine].Line_NextDownPoint := P;

            Point_FacingDirection := Up;

            IF Lines[Point_StraightLine].Line_DownX <> Lines[Point_DivergingLine].Line_DownX THEN BEGIN
              DebugStr := 'Lines do not properly intersect at P=' + IntToStr(P) + ': '
                          + 'DownX for straight line ' + LineToStr(Point_StraightLine) + ' is ' + IntToStr(Lines[Point_StraightLine].Line_DownX)
                          + ' and DownX for diverging line ' + LineToStr(Point_DivergingLine) + ' is ' + IntToStr(Lines[Point_DivergingLine].Line_DownX)
                          + CRLF
                          + 'Do you wish to continue?';
              IF MessageDialogueWithDefault(DebugStr, StopTimer, mtWarning, [mbOK, mbAbort], mbOK) = mrAbort THEN
                ShutDownProgram(UnitRef, 'CreatePoint');
            END;
          END;
        END
        ELSE
          IF (Lines[Point_HeelLine].Line_DownX = Lines[Point_StraightLine].Line_UpX) THEN BEGIN
            UpX := Lines[Point_StraightLine].Line_UpX;

            Lines[Point_HeelLine].Line_NextDownType := PointIsNext;
            Lines[Point_HeelLine].Line_NextDownPoint := P;

            Lines[Point_StraightLine].Line_NextUpType := PointIsNext;
            Lines[Point_StraightLine].Line_NextUpPoint := P;

            IF NOT PointIsCatchPoint(P) THEN BEGIN
              Lines[Point_DivergingLine].Line_NextUpType := PointIsNext;
              Lines[Point_DivergingLine].Line_NextUpPoint := P;

              Point_FacingDirection := Down;

              IF Lines[Point_StraightLine].Line_UpX <> Lines[Point_DivergingLine].Line_UpX THEN BEGIN
                DebugStr := 'Lines do not properly intersect at P=' + IntToStr(P) + ': ' + 'UpX for straight line ' + LineToStr(Point_StraightLine)
                            + ' is ' + IntToStr(Lines[Point_StraightLine].Line_UpX) + ' and UpX for diverging line ' + LineToStr(Point_DivergingLine)
                            + ' is ' + IntToStr(Lines[Point_DivergingLine].Line_UpX)
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

        UpY := Lines[Point_StraightLine].Line_UpY;

        { Now see if whether the diverging line shares that common point - if it does, swap UpX and DownX }
        IF NOT PointIsCatchPoint(P) THEN BEGIN
          IF UpX = Lines[Point_DivergingLine].Line_UpX THEN BEGIN
            OtherX := Lines[Point_DivergingLine].Line_DownX;
            OtherY := Lines[Point_DivergingLine].Line_DownY;
          END ELSE BEGIN
            OtherX := Lines[Point_DivergingLine].Line_UpX;
            OtherY := Lines[Point_DivergingLine].Line_UpY;
          END;
        END ELSE BEGIN
          IF Points[P].Point_Type = CatchPointUp THEN BEGIN
            OtherX := Lines[Point_StraightLine].Line_UpX - 100;
            OtherY := Lines[Point_StraightLine].Line_UpY - 100;
          END ELSE BEGIN
            OtherX := Lines[Point_StraightLine].Line_DownX + 100;
            OtherY := Lines[Point_StraightLine].Line_DownY + 100;
          END;
        END;

        Point_X := UpX;
        Point_Y := UpY;

        IF OtherX < UpX THEN
          DeltaX := -DeltaPointXSpaced
        ELSE
          DeltaX := +DeltaPointXSpaced;

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
              Left := Point_FarX;
              Top := Point_FarY;
              Right := Point_X;
              Bottom := Point_Y;
            END ELSE BEGIN
              Left := Point_X;
              Top := Point_FarY;
              Right := Point_FarX;
              Bottom := Point_Y;
            END;
          END ELSE BEGIN
            IF Point_FarX < Point_X THEN BEGIN
              Left := Point_FarX;
              Top := Point_Y;
              Right := Point_X;
              Bottom := Point_FarY;
            END ELSE BEGIN
              Left := Point_X;
              Top := Point_Y;
              Right := Point_FarX;
              Bottom := Point_FarY;
            END;
          END;
        END; { WITH }
      END; { WITH }
    END; { FOR }
  EXCEPT
    ON E : Exception DO
      Log('E : ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
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

FUNCTION ValidatePointManualStateAsReadIn(PointStateStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : PointStateType;
{ Check whether the last point state read in is valid }
BEGIN
  ErrorMsg := '';
  Result := StrToPointState(PointStateStr);

  IF Result <> PointStateUnknown THEN
    IF NOT PointManualOperation THEN
      ErrorMsg := 'ValidatePointManualStateAsReadIn : last manual state recorded but point not marked as being manually operated';
END; { ValidatePointManualStateAsReadIn }

FUNCTION ValidatePointLenzNum(LenzNumStr : String; PointManualStateAsReadIn : PointStateType; OUT PointManualOperation : Boolean; OUT PointPresentState : PointStateType;
                              OUT ErrorMsg : String) : Integer;
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
      PointPresentState := PointManualStateAsReadIn;
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

FUNCTION ValidatePointOtherPoint(OtherPointStr : String; PointType : TypeOfPoint; OUT ErrorMsg : String) : Integer;
{ Check whether the value of the connected point (if any) is valid }
BEGIN
  ErrorMsg := '';

  IF OtherPointStr = '' THEN
    Result := UnknownPoint
  ELSE
    IF NOT TryStrToInt(OtherPointStr, Result) THEN
      ErrorMsg := 'ValidatePointOtherPoint: invalid integer "' + OtherPointStr + '"';

  IF ErrorMsg = '' THEN BEGIN
    IF (PointType <> OrdinaryPoint) AND (PointType <> CatchPointUp) AND (PointType <> CatchPointDown) AND (Result = UnknownPoint) THEN
      ErrorMsg := 'ValidatePointOtherPoint: value of "other point" is missing'
    ELSE
      IF (PointType = ProtectedPoint) AND (Result = UnknownPoint) THEN
        ErrorMsg := 'ValidatePointOtherPoint: value of "other point" is missing'
      ELSE
        IF (PointType = OrdinaryPoint) AND (Result <> UnknownPoint) THEN
          ErrorMsg := 'ValidatePointOtherPoint: value of "other point" is invalid';
  END;
END; { ValidatePointOtherPoint }

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
      PointsADOTable.Sort := 'PointNum ASC';
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
              IF PointsADOTable.FieldByName('OtherPoint').AsInteger = P THEN BEGIN
                Log('A! P=' + IntToStr(PointsADOTable.FieldByName('PointNUm').AsInteger) + '''s OtherPoint refers to '
                        + ' P=' + IntToStr(PointsADOTable.FieldByName('OtherPoint').AsInteger)
                        + ' which did not exist prior to point renumbering - OtherPoint has been set to 9999');
                PointsADOTable.Edit;
                PointsADOTable.FieldByName('OtherPoint').AsInteger := 9999;
                PointsADOTable.Post;
              END;
              PointsADOTable.Next;
            END; { WHILE }

            { secondly we have to renumber any points that cross-refer to this point - allocate memory and assign where we are in the database to the bookmark }
            PointsADOTable.First;
            { at the start of the database }
            WHILE NOT PointsADOTable.EOF DO BEGIN
              IF InitVarsWindow.PointsADOTable.FieldByName('OtherPoint').AsInteger = NextInDatabaseP THEN BEGIN
                IF NextInDatabaseP = P THEN
                  { this shouldn't happen }
                  Log('A! OtherPoint=' + IntToStr(NextInDatabaseP) + ' is the same as P=' + IntToStr(P));
                PointsADOTable.Edit;
                PointsADOTable.FieldByName('OtherPoint').AsInteger := P;
                PointsADOTable.Post;
              END;
              PointsADOTable.Next;
            END; { WHILE }
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
      END; { WHILE }

      P := -1;
      PointsADOTable.Sort := 'PointNum ASC';
      PointsADOTable.First;

      SetLength(Points, 0);
      WHILE NOT PointsADOTable.EOF DO BEGIN
        ErrorMsg := '';
        LastManualStateStr := '';
        Inc(P);

        IF PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger <> P THEN
          ErrorMsg := 'it does not match the point number in the database (' + IntToStr(PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger) + ')'
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
          Point_LockedByUser := False;
          Point_LockFailureNotedInLocksUnit := False;
          Point_LockFailureNotedInSubRouteUnit := False;
          Point_LockingState := PointStateUnknown;
          Point_MaybeBeingSetToManual := False;
          Point_MovedWhenLocked := False;
          Point_PresentState := PointStateUnknown;
          Point_RequiredState := PointStateUnknown;
          Point_ResettingTime := 0;
          Point_RouteLockedByLocoChip := UnknownLocoChip;
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
            Point_ManualStateAsReadIn := ValidatePointManualStateAsReadIn(PointsADOTable.FieldByName(Point_ManualStateAsReadInFieldName).AsString, Point_ManualOperation,
                                                                          ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_LenzNum := ValidatePointLenzNum(PointsADOTable.FieldByName(Point_LenzNumFieldName).AsString, Point_ManualStateAsReadIn, Point_ManualOperation,
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
            Point_OtherPoint := ValidatePointOtherPoint(PointsADOTable.FieldByName(Point_OtherPointFieldName).AsString, Point_Type, ErrorMsg);

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
              IF Point_ManualStateAsReadIn <> PointStateUnknown THEN
                Point_RequiredState := Point_ManualStateAsReadIn
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
        END; { WITH }
      END; { WHILE }

      { Tidy up the database }
      PointsADOTable.Close;
      PointsADOConnection.Connected := False;
      Log('P Point data table and connection closed');
    END; { WITH }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG ReadInPointDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }

  CalculatePointPositions;
END; { ReadInPointDataFromDatabase }

PROCEDURE WriteOutPointDataToDatabase;
{ If a point's data has been changed, record it in the database }
VAR
  P : Integer;
  PointDatabaseNeedsUpdating : Boolean;

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
          IF Point_ManualOperation AND (Point_ManualStateAsReadIn <> Point_PresentState) THEN
            PointDatabaseNeedsUpdating := True;
          IF NOT Point_ManualOperation AND (Point_ManualStateAsReadIn <> PointStateUnknown) THEN
            PointDatabaseNeedsUpdating := True;
        END; { WITH }
        Inc(P);
      END; { WHILE }

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
          P := PointsADOTable.FieldByName('PointNum').AsInteger;
          WITH Points[P] DO BEGIN
            { First changes to out of use status }
            IF Point_DataChanged THEN BEGIN
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_OutOfUseFieldName).AsBoolean := Point_OutOfUse;
              PointsADOTable.Post;
              Log('P Recording in point database that P=' + IntToStr(P) + ' is now ' + IfThen(Points[P].Point_OutOfUse, 'out of use', 'back in use'));
            END;

            { And if the user has locked a particular point }
            IF Point_DataChanged THEN BEGIN
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_LockedByUserFieldName).AsBoolean := Point_LockedByUser;
              PointsADOTable.Post;
              Log('P Recording in point database that P=' + IntToStr(P) + ' is now ' + IfThen(Points[P].Point_LockedByUser, 'locked by user', 'back in use'));
            END;

            { And the last known state of manual points }
            IF NOT Point_ManualOperation AND (Point_ManualStateAsReadIn <> PointStateUnknown) THEN BEGIN
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_ManualStateAsReadInFieldName).AsString := '';
              PointsADOTable.Post;
            END
              ELSE
                IF (Point_ManualOperation) AND (Point_ManualStateAsReadIn <> Point_PresentState) THEN BEGIN
                  PointsADOTable.Edit;
                  IF Points[P].Point_PresentState = Straight THEN
                    PointsADOTable.FieldByName('LastManualState').AsString := 'straight'
                  ELSE
                    PointsADOTable.FieldByName('LastManualState').AsString := 'diverging';
                  PointsADOTable.Post;
                  Log('P Recording in point database that manual P=' + IntToStr(P) + '''s state is now ' + PointStateToStr(Points[P].Point_PresentState));
                END;
          END; { WITH }
          PointsADOTable.Next;
        END; { WHILE }

        { Tidy up the database }
        PointsADOTable.Close;
        PointsADOConnection.Connected := False;
        Log('P Point data table and connection closed');
      END;
    END; { WITH }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG WriteOutPointDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
END; { WriteOutPointDataToDatabase }

PROCEDURE CalculatePlatformPositions(ScaleFactor : Integer);
{ Create the platform rectangle }
VAR
  P : Integer;

BEGIN
  FOR P := 0 TO High(Platforms) DO BEGIN
    WITH Platforms[P] DO BEGIN
      WITH Platform_Rect DO BEGIN
        Left := Lines[Platform_LeftLine].Line_UpX;
        IF Platform_LeftLineAdjustment > 0 THEN
          Left := Left + MulDiv(FWPRailWindow.ClientWidth, Platform_LeftLineAdjustment, ScaleFactor);

        Top := -1;
        IF Platforms[P].Platform_LocationAbovePlatform <> UnknownLocation THEN
          Top := MulDiv(FWPRailWindow.ClientHeight, Locations[Platform_LocationAbovePlatform].Location_Y, ScaleFactor);

        Right := Lines[Platform_RightLine].Line_DownX;
        IF Platform_RightLineAdjustment > 0 THEN
          Right := Right + MulDiv(FWPRailWindow.ClientWidth, Platform_RightLineAdjustment, ScaleFactor);

        Bottom := -1;
        IF Platforms[P].Platform_LocationBelowPlatform <> UnknownLocation THEN
          Bottom := MulDiv(FWPRailWindow.ClientHeight, Locations[Platform_LocationBelowPlatform].Location_Y, ScaleFactor);

        IF Top = -1 THEN
          Top := Platforms[P].Platform_Rect.Bottom - MulDiv(FWPRailWindow.ClientHeight, Platforms[P].Platform_Height, ScaleFactor)
        ELSE
          IF Bottom = -1 THEN
            Bottom := Platforms[P].Platform_Rect.Top + MulDiv(FWPRailWindow.ClientHeight, Platforms[P].Platform_Height, ScaleFactor);

        IF Top = Bottom THEN
          Debug('Platform ' + IntToStr(P) + ': platform top value (' + IntToStr(Top) + ') = platform bottom value (' + IntToStr(Bottom) + ')')
        ELSE
          IF Top > Bottom THEN
            Debug('Platform ' + IntToStr(P) + ': platform top value (' + IntToStr(Top) + ') > platform bottom value (' + IntToStr(Bottom) + ')');
        Top := Top + PlatformEdgeVerticalSpacingScaled;
        Bottom := Bottom - PlatformEdgeVerticalSpacingScaled;
      END; { WITH }
    END; { WITH }
  END; { FOR }
END; { CalculatePlatformPositions }

PROCEDURE ReadInPlatformDataFromDatabase;
{ Initialise the platform data }
CONST
  StopTimer = True;

VAR
  ErrorMsg : String;
  FieldName : String;
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

      PlatformDataADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                    + PathToRailDataFiles + PlatformDataFilename + '.' + PlatformDataFilenameSuffix
                                                    + ';Persist Security Info=False';
      PlatformDataADOConnection.Connected := True;
      PlatformDataADOTable.Open;
      Log('T Platform data table and connection opened to initialise the platform data');

      { First see if the platform numbers in the MSAccess file are sequential and, if not, renumber it; We need this or deletions from the MSAccess file cause problems }
      P := -1;
      PlatformDataADOTable.First;
      FieldName := 'RunningNumber';
      WHILE NOT PlatformDataADOTable.EOF DO BEGIN
        Inc(P);
        IF PlatformDataADOTable.FieldByName(FieldName).AsInteger <> P THEN BEGIN
          { we need to renumber from here on }
          PlatformDataADOTable.Edit;
          PlatformDataADOTable.FieldByName(FieldName).AsInteger := P;
          PlatformDataADOTable.Post;
        END;
        PlatformDataADOTable.Next;
      END; { WHILE }

      PlatformDataADOTable.Sort := 'RunningNumber ASC';
      PlatformDataADOTable.First;
      SetLength(Platforms, 0);

      WHILE NOT PlatformDataADOTable.EOF DO BEGIN
        WITH PlatformDataADOTable DO BEGIN
          SetLength(Platforms, Length(Platforms) + 1);
          P := High(Platforms);
          WITH Platforms[P] DO BEGIN
            ErrorMsg := '';

            FieldName := 'RunningNumber';
            Platform_RunningNumber := FieldByName(FieldName).AsInteger;
            IF Platform_RunningNumber <> P THEN
              ErrorMsg := 'it does not match the platform number in the database (' + IntToStr(Platform_RunningNumber) + ')';

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Description';
              Platform_Description := FieldByName(FieldName).AsString;

              FieldName := 'PlatformNumberA';
              Platform_NumberAStr := FieldByName(FieldName).AsString;

              FieldName := 'PlatformNumberAPosition';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                ErrorMsg := 'no platform A position supplied'
              ELSE BEGIN
                Platform_NumberPositionA := StrToPlatformNumberPosition(TempStr);
                IF Platform_NumberPositionA = UnknownPlatformNumberPosition THEN
                  ErrorMsg := 'platform number A position supplied is unknown "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'PlatformNumberB';
              Platform_NumberBStr := FieldByName(FieldName).AsString;

              FieldName := 'PlatformNumberBPosition';
              TempStr := FieldByName(FieldName).AsString;
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
              FieldName := 'LeftLine';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                ErrorMsg := 'missing left line value'
              ELSE BEGIN
                Platform_LeftLine := StrToLine(TempStr);
                IF Platform_LeftLine = UnknownLine THEN
                  ErrorMsg := 'invalid left line "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'LeftLineAdjustment';
              TempStr := FieldByName(FieldName).AsString;
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
              FieldName := 'RightLine';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                ErrorMsg := 'missing right line value'
              ELSE BEGIN
                Platform_RightLine := StrToLine(TempStr);
                IF Platform_RightLine = UnknownLine THEN
                  ErrorMsg := 'invalid right line "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'RightLineAdjustment';
              TempStr := FieldByName(FieldName).AsString;
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

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'LocationAbovePlatform';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Platform_LocationAbovePlatform := UnknownLocation
              ELSE BEGIN
                Platform_LocationAbovePlatform := StrToLocation(TempStr);
                IF Platform_LocationAbovePlatform = UnknownLocation THEN
                  ErrorMsg := 'invalid location above platform "' + TempStr + '"';
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'LocationBelowPlatform';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Platform_LocationBelowPlatform := UnknownLocation
              ELSE BEGIN
                Platform_LocationBelowPlatform := StrToLocation(TempStr);
                IF Platform_LocationBelowPlatform = UnknownLocation THEN
                  ErrorMsg := 'invalid location below platform "' + TempStr + '"';
              END;
            END;

            IF (Platform_LocationAbovePlatform = UnknownLocation) AND (Platform_LocationBelowPlatform = UnknownLocation) THEN
              ErrorMsg := 'must have either a location above or a location below';

            IF ErrorMsg = '' THEN BEGIN
              FieldName := 'Height';
              TempStr := FieldByName(FieldName).AsString;
              IF TempStr = '' THEN
                Platform_Height := 0
              ELSE
                IF NOT TryStrToInt(TempStr, Platform_Height) THEN
                  ErrorMsg := 'invalid height "' + TempStr + '"';
            END;

            IF Platform_Height > 0 THEN
              IF (Platform_LocationAbovePlatform <> UnknownLocation) AND (Platform_LocationBelowPlatform <> UnknownLocation) THEN
                ErrorMsg := 'cannot have both a location above and a location below if a height is specified';

            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating Platform=' + IntToStr( High(Platforms))
                                            + ' (' + Platform_Description + '): ' + '['
                                            + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?', StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInPlatformDataFromDatabase');
            END;
          END; { WITH }
        END; { WITH }
        PlatformDataADOTable.Next;
      END; { WHILE }

      { Tidy up the database }
      PlatformDataADOTable.Close;
      PlatformDataADOConnection.Connected := False;
      Log('T Platform data table and connection closed');
    END; { WITH }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG InitialisePlatformData: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }

  CalculatePlatformPositions(1000);

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

      FeedbackUnitDataADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                        + PathToRailDataFiles + FeedbackDataFilename + '.' + FeedbackDataFilenameSuffix
                                                        + ';Persist Security Info=False';
      FeedbackUnitDataADOConnection.Connected := True;
      FeedbackUnitDataADOTable.Open;
      Log('T Feedback data table and connection opened to initialise the feedback unit data');

      FeedbackUnitDataADOTable.Sort := 'Unit ASC';
      FeedbackUnitDataADOTable.First;
      SetLength(FeedbackUnitData, 0);
      FirstFeedbackUnit := 99999;
      LastFeedbackUnit := 0;

      WHILE NOT FeedbackUnitDataADOTable.EOF DO BEGIN
        WITH FeedbackUnitDataADOTable DO BEGIN
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
                END; { WHILE }
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

          END; { WITH }
        END; { WITH }
        FeedbackUnitDataADOTable.Next;
      END; { WHILE }

      { Tidy up the database }
      FeedbackUnitDataADOTable.Close;
      FeedbackUnitDataADOConnection.Connected := False;
      Log('T Feedback unit data table and connection closed');

      Log('T Reading in feedback data from unit ' + IntToStr(FirstFeedbackUnit) + ' to unit ' + IntToStr(LastFeedbackUnit) + ' from database');
    END; { WITH }
  EXCEPT { TRY }
    ON E : Exception DO
      Log('EG ReadInFeedbackDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; { TRY }
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
  END; { WITH }
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
  END; { WITH }
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
  END; { WITH }
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

  DefaultCreateRouteDisplayColoursWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultCreateRouteDisplayColoursWindowTop := MulDiv(Screen.WorkAreaHeight, 500, 1000);
  DefaultCreateRouteDisplayColoursWindowLeft := 0;
  DefaultCreateRouteDisplayColoursWindowWidth := MulDiv(Screen.WorkAreaHeight, 200, 1000);

  DefaultDebugWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultDebugWindowTop := MulDiv(Screen.WorkAreaHeight, 800, 1000);
  DefaultDebugWindowLeft := MulDiv(Screen.WorkAreaWidth, 500, 1000);
  DefaultDebugWindowWidth := MulDiv(Screen.WorkAreaWidth, 500, 1000);

  DefaultDiagramsWindowHeight := MulDiv(Screen.WorkAreaHeight, 200, 1000);
  DefaultDiagramsWindowTop := MulDiv(Screen.WorkAreaHeight, 800, 1000);
  DefaultDiagramsWindowLeft := 0;
  DefaultDiagramsSmallWindowWidth := MulDiv(Screen.WorkAreaWidth, 500, 1000);
  DefaultDiagramsLargeWindowWidth := Screen.WorkAreaWidth;

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
END;

END { InitVars }.
