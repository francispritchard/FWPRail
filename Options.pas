UNIT Options;
{ Allows options to be read in and written to the Registry and to be changed by means of a Value List Editor.

  v.0.1 31/03/09 First written
  v.0.2 03/04/09 TValueListEditor found after TTreeView tried and did not do what was required
  v.0.3 04/04/09 LoadIniFile unit merged into this unit
}

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, Grids, ValEdit, Initvars, Registry, Movement, System.Types;

TYPE
  TOptionsWindow = CLASS(TForm)
    OptionsWindowFindDialog: TFindDialog;
    OptionsValueListEditor: TValueListEditor;
    PROCEDURE OptionsValueListEditorDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    PROCEDURE OptionsValueListEditorKeyDown(Sender: TObject; VAR Key: Word; Shift: TShiftState);
    PROCEDURE OptionsValueListEditorValidate(Sender: TObject; ACol, ARow: Integer; CONST KeyName, KeyValue: String);
    PROCEDURE OptionsWindowFindDialogShow(Sender: TObject);
    PROCEDURE OptionsWindowFindDialogClose(Sender: TObject);
    PROCEDURE OptionsWindowFindDialogFind(Sender: TObject);
    PROCEDURE OptionsWindowShow(Sender: TObject);
    procedure OptionsWindowCreate(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE InitialiseOptionsUnit;
{ Initialises the unit }

PROCEDURE ReadIniFile;
{ Read in data from the .ini file or from the Registry, except for the track-circuit data }

PROCEDURE ReadIniFileForTrackCircuitData;
{ Read in track-circuit data from the .ini file or from the Registry }

PROCEDURE RestoreScreenDefaults;
{ Restore screen colours, fonts, etc. }

PROCEDURE WriteIniFile;
{ Write out data to the registry and to the .ini file. (Do both so that the .ini file can be archived but not normally used when options are read in). }

VAR
  OptionsWindow: TOptionsWindow;

  { Windows variables first - these are all initialised in InitialiseInitVarsUnit }
  DefaultDebuggingOptionsWindowLeft : Integer = 785; { these shouldn't be magic numbers *** }
  DebuggingOptionsWindowLeft : Integer = 785; { these shouldn't be magic numbers *** }

  DefaultDebuggingOptionsWindowTop : Integer = 415; { these shouldn't be magic numbers *** }
  DebuggingOptionsWindowTop : Integer = 415; { these shouldn't be magic numbers *** }

  DefaultDebugWindowHeight : Integer;
  DebugWindowHeight : Integer;

  DefaultDebugWindowLeft : Integer;
  DebugWindowLeft : Integer;

  DefaultDebugWindowTop : Integer;
  DebugWindowTop : Integer;

  DefaultDebugWindowWidth : Integer;
  DebugWindowWidth : Integer;

  DefaultDiagramsWindowHeight : Integer;
  DiagramsWindowHeight : Integer;

  DefaultDiagramsWindowLeft : Integer;
  DiagramsWindowLeft : Integer;

  DefaultDiagramsWindowTop : Integer;
  DiagramsWindowTop : Integer;

  DefaultDiagramsSmallWindowWidth : Integer;
  DiagramsSmallWindowWidth : Integer;

  DefaultDiagramsLargeWindowWidth : Integer;
  DiagramsLargeWindowWidth : Integer;

  DefaultDisplayLineColoursWindowHeight : Integer;
  DisplayLineColoursWindowHeight : Integer;

  DefaultDisplayLineColoursWindowTop : Integer;
  DisplayLineColoursWindowTop : Integer;

  DefaultDisplayLineColoursWindowLeft : Integer;
  DisplayLineColoursWindowLeft : Integer;

  DefaultDisplayLineColoursWindowWidth : Integer;
  DisplayLineColoursWindowWidth : Integer;

  DefaultEditWindowHeight : Integer;
  EditWindowHeight : Integer;

  DefaultEditWindowLeft : Integer;
  EditWindowLeft : Integer;

  DefaultEditWindowTop : Integer;
  EditWindowTop : Integer;

  DefaultEditWindowWidth : Integer;
  EditWindowWidth : Integer;

  DefaultFWPRailWindowHeight : Integer;
  FWPRailWindowHeight : Integer;

  DefaultFWPRailWindowLeft : Integer = 0; { these shouldn't be magic numbers *** }
  FWPRailWindowLeft : Integer = 0;

  DefaultFWPRailWindowFontHeight : Integer = 10;
  FWPRailWindowFontHeight : Integer;

  DefaultFWPRailWindowTop : Integer = 0;
  FWPRailWindowTop : Integer = 0;

  DefaultFWPRailWindowWidth : Integer;
  FWPRailWindowWidth : Integer;

  DefaultLockListWindowHeight : Integer;
  LockListWindowHeight : Integer;

  DefaultLockListWindowTop : Integer;
  LockListWindowTop : Integer;

  DefaultLockListWindowLeft : Integer;
  LockListWindowLeft : Integer;

  DefaultLockListWindowWidth : Integer;
  LockListWindowWidth : Integer;

  DefaultLocoUtilsWindowHeight : Integer;
  LocoUtilsWindowHeight : Integer;

  DefaultLocoUtilsWindowTop : Integer;
  LocoUtilsWindowTop : Integer;

  DefaultLocoUtilsWindowLeft : Integer;
  LocoUtilsWindowLeft : Integer;

  DefaultLocoUtilsWindowWidth : Integer;
  LocoUtilsWindowWidth : Integer;

  DefaultLoggingWindowHeight : Integer = 0; { these shouldn't be magic numbers *** }
  LoggingWindowHeight : Integer;

  DefaultLoggingWindowTop : Integer = 0;
  LoggingWindowTop : Integer;

  DefaultLoggingWindowLeft : Integer = 0;
  LoggingWindowLeft : Integer;

  DefaultLoggingWindowWidth : Integer = 0;
  LoggingWindowWidth : Integer;

  DefaultMovementWindowHeight : Integer;
  MovementWindowHeight : Integer;

  DefaultMovementWindowTop : Integer;
  MovementWindowTop : Integer;

  DefaultMovementWindowLeft : Integer;
  MovementWindowLeft : Integer;

  DefaultMovementWindowWidth : Integer;
  MovementWindowWidth : Integer;

  DefaultOptionsWindowHeight : Integer;
  OptionsWindowHeight : Integer;

  DefaultOptionsWindowTop : Integer;
  OptionsWindowTop : Integer;

  DefaultOptionsWindowLeft : Integer;
  OptionsWindowLeft : Integer;

  DefaultOptionsWindowWidth : Integer;
  OptionsWindowWidth : Integer;

  DefaultWorkingTimetableWindowHeight : Integer;
  WorkingTimetableWindowHeight : Integer;

  DefaultWorkingTimetableWindowTop : Integer;
  WorkingTimetableWindowTop : Integer;

  DefaultWorkingTimetableWindowLeft : Integer;
  WorkingTimetableWindowLeft : Integer;

  DefaultWorkingTimetableSmallWindowWidth : Integer;
  WorkingTimetableSmallWindowWidth : Integer;

  DefaultWorkingTimetableLargeWindowWidth : Integer;
  WorkingTimetableLargeWindowWidth : Integer;

  { Dialogue Boxes }
  DefaultLineDialogueBoxLeft : Integer = 5; { these shouldn't be magic numbers *** }
  LineDialogueBoxLeft : Integer = 5; { these shouldn't be magic numbers *** }

  DefaultLineDialogueBoxTop : Integer = 473; { these shouldn't be magic numbers *** }
  LineDialogueBoxTop : Integer = 473; { these shouldn't be magic numbers *** }

  DefaultLocoDialogueWindowLeft : Integer = 739; { these shouldn't be magic numbers *** }
  LocoDialogueWindowLeft : Integer = 739; { these shouldn't be magic numbers *** }

  DefaultLocoDialogueWindowTop : Integer = 250; { these shouldn't be magic numbers *** }
  LocoDialogueWindowTop : Integer = 250; { these shouldn't be magic numbers *** }

  DefaultPointDialogueBoxLeft : Integer = 5; { these shouldn't be magic numbers *** }
  PointDialogueBoxLeft : Integer = 5; { these shouldn't be magic numbers *** }

  DefaultPointDialogueBoxTop : Integer = 473; { these shouldn't be magic numbers *** }
  PointDialogueBoxTop : Integer = 473; { these shouldn't be magic numbers *** }

  DefaultSignalDialogueBoxLeft : Integer = 5; { these shouldn't be magic numbers *** }
  SignalDialogueBoxLeft : Integer = 5; { these shouldn't be magic numbers *** }

  DefaultSignalDialogueBoxTop : Integer = 473; { these shouldn't be magic numbers *** }
  SignalDialogueBoxTop : Integer = 473; { these shouldn't be magic numbers *** }

  DefaultTrackCircuitDialogueBoxLeft : Integer = 5; { these shouldn't be magic numbers *** }
  TrackCircuitDialogueBoxLeft : Integer = 5; { these shouldn't be magic numbers *** }

  DefaultTrackCircuitDialogueBoxTop : Integer = 473; { these shouldn't be magic numbers *** }
  TrackCircuitDialogueBoxTop : Integer = 473; { these shouldn't be magic numbers *** }

  { Other variables }
  CurrentParametersFromIniFile : String = '';
  CurrentParametersFromParamStr : String = '';

  DefaultAcceptAllPermanentOccupationsWithoutFeedback : Boolean = True;
  AcceptAllPermanentOccupationsWithoutFeedback : Boolean;

  DefaultAreaDataFilename : String = 'AreaData';
  AreaDataFilename : String;

  DefaultAreaDataFilenameSuffix : String = 'mdb';
  AreaDataFilenameSuffix : String;

  DefaultAutomaticallySetFocusWhenInDebugWindow : Boolean = False;
  AutomaticallySetFocusWhenInDebugWindow : Boolean;

  DefaultBackgroundColour : TColour = clBlack;
  BackgroundColour : TColour = clBlack;

  DefaultBufferStopNumberColour : TColour = clLime;
  BufferStopNumberColour : TColour = clLime;

  DefaultBufferStopColour : TColour = clDkGray;
  BufferStopColour : TColour = clDkGray;

  DefaultBufferStopRed : TColour = clRed;
  BufferStopRed : TColour = clRed;

  DefaultBufferStopVerticalSpacing : Integer = 100;
  BufferStopVerticalSpacing : Integer;
  BufferStopVerticalSpacingScaled : Integer;

  DefaultCancelAllTrainsWithNoFeedbackOccupation : Boolean = False;
  CancelAllTrainsWithNoFeedbackOccupation : Boolean;

  DefaultCarriageLengthInInches : Integer = 12;
  CarriageLengthInInches : Integer;

  DefaultCheckForIdenticalLinesInLog : Boolean = True;
  CheckForIdenticalLinesInLog : Boolean;

  DefaultCurrentRailwayDayOfTheWeek : String = 'UnknownDayOfTheWeek';
  CurrentRailwayDayOfTheWeek : DayOfTheWeekType;

  DefaultDataCheckFileName : String = '0Rail.syntaxcheck';
  DataCheckFileName : String;

  DefaultDayLightStartTimeStr : String = '08:00:00';
  DayLightStartTimeStr : String = '08:00:00';
  DayLightStartTime : TDateTime;

  DefaultDayLightEndTimeStr : String = '17:59:59';
  DayLightEndTimeStr : String = '17:59:59';
  DayLightEndTime : TDateTime;

  DefaultDeltaPointX : Integer = 10;
  DeltaPointX : Integer;

  DefaultDiagramsFilename : String = 'Diagrams';
  DiagramsFilename : String;

  DefaultDiagramsFilenameSuffix : String = 'mdb';
  DiagramsFilenameSuffix : String;

  DefaultDiagramsWindowGridBackgroundColour : TColor = clMenu;
  DiagramsWindowGridBackgroundColour : TColour = clMenu;

  DefaultDisplayDiagrams : Boolean = True;
  DisplayDiagrams : Boolean;

  DefaultDisplayFlashingTrackCircuits : Boolean = False;
  DisplayFlashingTrackCircuits : Boolean;

  DefaultDisplayLocoChipNums : Boolean = True;
  DisplayLocoChipNums : Boolean;

  DefaultDisplayLocoHeadcodes : Boolean = False;
  DisplayLocoHeadcodes : Boolean;

  DefaultDisplayNotForPublicUseTrainsInStationMonitors : Boolean = False;
  DisplayNotForPublicUseTrainsInStationMonitors : Boolean;

  DefaultDisplayRoutesAndJourneys : Boolean = False;
  DisplayRoutesAndJourneys : Boolean;

  DefaultDisplayWorkingTimetable : Boolean = True;
  DisplayWorkingTimetable : Boolean;

  DefaultDoNotCancelTrainsWithNoFeedbackOccupation : Boolean = False;
  DoNotCancelTrainsWithNoFeedbackOccupation : Boolean;

  DefaultFeedbackDataFilename : String = 'FeedbackUnitData';
  FeedbackDataFilename : String;

  DefaultFeedbackDataFilenameSuffix : String = 'mdb';
  FeedbackDataFilenameSuffix : String;

  DefaultFiddleyardLinePenStyle : TPenStyle = psDot;
  FiddleyardLinePenStyle : TPenStyle = psDot;

  DefaultForegroundColour : TColour = clDkGray;
  ForegroundColour : TColour = clDkGray;

  DefaultHighlightTrackCircuitSpeedRestrictions : Boolean = False;
  HighlightTrackCircuitSpeedRestrictions : Boolean;

  DefaultIndicatorHorizontalSpacing : Integer = 100;
  IndicatorHorizontalSpacing : Integer;
  IndicatorHorizontalSpacingScaled : Integer;

  DefaultIndicatorVerticalSpacing : Integer = 100;
  IndicatorVerticalSpacing : Integer;
  IndicatorVerticalSpacingScaled : Integer;

  DefaultLargeDiagramsWindowSelected : Boolean = False;
  LargeDiagramsWindowSelected : Boolean;

  DefaultLargeWorkingTimetableWindowSelected : Boolean = False;
  LargeWorkingTimetableWindowSelected : Boolean;

  DefaultLineDataFilename : String = 'LineData';
  LineDataFilename : String;

  DefaultLineDataFilenameSuffix : String = 'mdb';
  LineDataFilenameSuffix : String;

  DefaultLineNotAvailableColour : TColour = clLime;
  LineNotAvailableColour : TColour = clLime;

  DefaultLineRoutedOverColour : TColour = clWhite;
  LineRoutedOverColour : TColour = clWhite;

  DefaultLinesWithoutTrackCircuitsColour : TColour = clYellow;
  LinesWithoutTrackCircuitsColour : TColour = clYellow;

  DefaultLineFontHeight : Integer = 10;
  LineFontHeight : Integer;

  DefaultLineThicknessInFullScreenMode : String = 'Thick';
  LineThicknessInFullScreenMode : String;

  DefaultLocationDataFilename : String = 'LocationData';
  LocationDataFilename : String;

  DefaultLocationDataFilenameSuffix : String = 'mdb';
  LocationDataFilenameSuffix : String;

  DefaultLocoDataFilename : String = 'LocoData';
  LocoDataFilename : String;

  DefaultLocoDataFilenameSuffix : String = 'mdb';
  LocoDataFilenameSuffix : String;

  DefaultLocoDialogueSpeedInMPH : Boolean = False;
  LocoDialogueSpeedInMPH : Boolean;

  DefaultLocoStalledColour : TColour = clGreen;
  LocoStalledColour : TColour = clGreen;

  DefaultLocoTimingTimeBeforeAutoStopInSeconds : Integer = 90;
  LocoTimingTimeBeforeAutoStopInSeconds : Integer;

  DefaultLogCurrentTimeMode : Boolean = False;
  LogCurrentTimeMode : Boolean;

  DefaultLogFileMaxWidthInChars : Integer = 80;
  LogFileMaxWidthInChars : Integer;

  DefaultLogFilename : String = 'Log';
  LogFilename : String;

  DefaultLogFilenameSuffix : String = '';
  LogFilenameSuffix : String;

  DefaultLoggingWindowFontName : String = 'Lucida Console';
  LoggingWindowFontName : String;

  DefaultLoggingWindowFontSize : Integer = 8;
  LoggingWindowFontSize : Integer;

  DefaultLogsKeptMode : Boolean = True;
  LogsKeptMode : Boolean;

  DefaultMakeSoundWhenDebugWindowBoldTextAppears : Boolean = True;
  MakeSoundWhenDebugWindowBoldTextAppears : Boolean;

  DefaultMaxRectangleUndrawTime : Cardinal = 1;
  MaxRectangleUndrawTime : Cardinal;

  DefaultMenusVisible : Boolean = False;
  MenusVisible : Boolean;

  DefaultMonitorStrayingTrains : Boolean = True;
  MonitorStrayingTrains : Boolean;

  DefaultMouseRectangleEdgeVerticalSpacing : Integer = 100;
  MouseRectangleEdgeVerticalSpacing : Integer;
  MouseRectangleEdgeVerticalSpacingScaled : Integer;

  DefaultOptionsWindowValueListEditorCol0Width : Integer = 100;
  OptionsWindowValueListEditorCol0Width : Integer = 100;

  PathToRailDataFiles : String;
  PathToRailSourceFiles : String;
  PathToLogFiles : String;

  DefaultPlatformColour : TColour = clFWPPlatformColour;
  PlatformColour : TColour = clFWPPlatformColour;

  DefaultPlatformDataFilename : String = 'PlatformData';
  PlatformDataFilename : String;

  DefaultPlatformDataFilenameSuffix : String = 'mdb';
  PlatformDataFilenameSuffix : String;

  DefaultPlatformEdgeVerticalSpacing : Integer = 100;
  PlatformEdgeVerticalSpacing : Integer;
  PlatformEdgeVerticalSpacingScaled : Integer;

  DefaultPlatformNumberColour : TColour = clBlack;
  PlatformNumberColour : TColour = clBlack;

  DefaultPlatformNumberFontHeight : Integer = 10;
  PlatformNumberFontHeight : Integer;

  DefaultPlatformNumberEdgeHorizontalSpacing : Integer = 100;
  PlatformNumberEdgeHorizontalSpacing : Integer;
  PlatformNumberEdgeHorizontalSpacingScaled : Integer;

  DefaultPlatformNumberEdgeVerticalSpacing : Integer = 10;
  PlatformNumberEdgeVerticalSpacing : Integer;
  PlatformNumberEdgeVerticalSpacingScaled : Integer;

  DefaultPointColour : TColor = clDkGray;
  PointColour : TColour = clDkGray;

  DefaultPointDataFilename : String = 'PointData';
  PointDataFilename : String;

  DefaultPointDataFilenameSuffix : String = 'mdb';
  PointDataFilenameSuffix : String;

  DefaultPointDivergingLineColour : TColour = clRed;
  PointDivergingLineColour : TColour = clRed;

  DefaultPointDownFacingColour : TColour = clAqua;
  PointDownFacingColour : TColour = clAqua;

  DefaultPointFeedbackDataInUseColour : TColour = clLime;
  PointFeedbackDataInUseColour : TColour = clLime;

  DefaultPointFeedbackDataOutOfUseColour : TColour = clFWPOrange;
  PointFeedbackDataOutOfUseColour : TColour = clFWPOrange;

  DefaultPointFeedbackMaximumWaitInSeconds : Integer = 3;
  PointFeedbackMaximumWaitInSeconds : Integer = 3;

  DefaultPointHeelLineColour : TColour = clYellow;
  PointHeelLineColour : TColour = clYellow;

  DefaultPointLenzNumberColour : TColour = clLime;
  PointLenzNumberColour : TColour = clLime;

  DefaultPointLockedByUserColour : TColour = clYellow;
  PointLockedByUserColour : TColour;

  DefaultPointManualOperationColour : TColour = clYellow;
  PointManualOperationColour : TColour = clYellow;

  DefaultPointOutOfUseColour : TColour = clFWPPink;
  PointOutOfUseColour : TColour = clFWPPink;

  DefaultPointStraightLineColour : TColour = clLime;
  PointStraightLineColour : TColour = clLime;

  DefaultPointsWithoutFeedbackColour : TColour = clAqua;
  PointsWithoutFeedbackColour : TColour = clAqua;

  DefaultPointUndrawColour : TColour = clFWPVeryDkGrey;
  PointUndrawColour : TColour = clFWPVeryDkGrey;

  DefaultPointUpFacingColour : TColour = clYellow;
  PointUpFacingColour : TColour = clYellow;

  DefaultProgramStartTimeStr : String = '06:30:00';
  ProgramStartTimeStr : String = '06:30:00';
  ProgramStartTime : TDateTime;

  DefaultProjectedLinePenStyle : TPenStyle = psDashDot;
  ProjectedLinePenStyle : TPenStyle = psDashDot;

  DefaultRailFontName : String = 'Arial';
  RailFontName : String;

  { For obvious reasons the following parameter is only used on the command-line }
  ReadFromRegistry : Boolean = True;

  DefaultReplayFilename : String = 'Log';
  ReplayFilename : String;

  DefaultReplayFilenameSuffix : String = 'r';
  ReplayFilenameSuffix : String;

  DefaultRouteAheadNotClearWaitTimeInMinutes : Integer = 5;
  RouteAheadNotClearWaitTimeInMinutes : Integer = 5;

  DefaultRouteingExceptionDataFilename : String = 'RouteingExceptionData';
  RouteingExceptionDataFilename : String;

  DefaultRouteingExceptionDataFilenameSuffix : String = 'mdb';
  RouteingExceptionDataFilenameSuffix : String;

  DefaultRunTestUnitOnStartup : Boolean = False;
  RunTestUnitOnStartup : Boolean;

  DefaultScreenComponentEditedColour1 : TColor = clAqua;
  ScreenComponentEditedColour1 : TColor = clAqua;

  DefaultScreenComponentEditedColour2 : TColor = clPurple;
  ScreenComponentEditedColour2 : TColor = clPurple;

  DefaultShowCancelledTrainsInDiagrams : Boolean = True;
  ShowCancelledTrainsInDiagrams : Boolean;

  DefaultShowIncorrectDayOfTheWeekEntriesInWorkingTimetable : Boolean = False;
  ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable : Boolean;

  DefaultShowNonMovingTrainsInDiagrams : Boolean = False;
  ShowNonMovingTrainsInDiagrams : Boolean;

  DefaultShowNonStopsInDiagrams : Boolean = True;
  ShowNonStopsInDiagrams : Boolean;

  DefaultShowPointDefaultStateColour : TColour = clWhite;
  ShowPointDefaultStateColour : TColour = clWhite;

  DefaultShowPointLockedColour : TColour = clYellow;
  ShowPointLockedColour : TColour = clYellow;

  DefaultShowTrackCircuitsWhereUserMustDrive : Boolean = True;
  ShowTrackCircuitsWhereUserMustDrive : Boolean;

  DefaultSidingPenStyle : TPenStyle = psDot;
  SidingPenStyle : TPenStyle = psDot;

  { Colours for signals }
  DefaultSignalAspectRed : TColour = clRed;
  SignalAspectRed : TColour = clRed;

  DefaultSignalAspectGreen : TColour = clLime;
  SignalAspectGreen : TColour = clLime;

  DefaultSignalAspectUnlit : TColor = clDkGray;
  SignalAspectUnlit : TColour = clDkGray;

  DefaultSignalAspectYellow : TColour = clYellow;
  SignalAspectYellow : TColour = clYellow;

  DefaultSignalDataFilename : String = 'SignalData';
  SignalDataFilename : String;

  DefaultSignalDataFilenameSuffix : String = 'mdb';
  SignalDataFilenameSuffix : String;

  DefaultSignalHorizontalSpacing : Integer = 100;
  SignalHorizontalSpacing : Integer;
  SignalHorizontalSpacingScaled : Integer;

  DefaultSignalNumberColour : TColour = clYellow;
  SignalNumberColour : TColour = clYellow;

  { Colours for signal posts }
  DefaultSignalPostColour : TColour = clDkGray;
  SignalPostColour : TColour = clDkGray;

  DefaultSignalPostEmergencyRouteSettingColour : TColour = clRed;
  SignalPostEmergencyRouteSettingColour : TColour = clRed;

  DefaultSignalPostRouteSettingColour : TColour = clLime;
  SignalPostRouteSettingColour : TColour = clLime;

  DefaultSignalPostStationStartModeColour : TColour = clFuchsia;
  SignalPostStationStartModeColour : TColour = clFuchsia;

  DefaultSignalPostTheatreSettingColour : TColour = clAqua;
  SignalPostTheatreSettingColour : TColour = clAqua;

  DefaultSignalRadius : Integer = 50;
  SignalRadius : Integer;
  SignalRadiusScaled : Integer;

  DefaultSignalSemaphoreHeight : Integer = 25;
  SignalSemaphoreHeight : Integer;
  SignalSemaphoreHeightScaled : Integer;

  DefaultSignalSemaphoreWidth : Integer = 70;
  SignalSemaphoreWidth : Integer;
  SignalSemaphoreWidthScaled : Integer;

  DefaultSignalVerticalSpacing : Integer = 150;
  SignalVerticalSpacing : Integer;
  SignalVerticalSpacingScaled : Integer;

  DefaultSpeedRestrictionHorizontalSpacing : Integer = 100;
  SpeedRestrictionHorizontalSpacing : Integer;
  SpeedRestrictionHorizontalSpacingScaled : Integer;

  DefaultSpeedRestrictionVerticalSpacing : Integer = 30;
  SpeedRestrictionVerticalSpacing : Integer;
  SpeedRestrictionVerticalSpacingScaled : Integer;

  DefaultStartRepeatJourneysOnNewLineInDiagrams : Boolean = True;
  StartRepeatJourneysOnNewLineInDiagrams : Boolean;

  DefaultStartWithDiagrams : Boolean = True;
  StartWithDiagrams : Boolean;

  DefaultStationEndOfDayPassengerLeavingTimeInMinutes : Integer = 4;
  StationEndOfDayPassengerLeavingTimeInMinutes : Integer = 4;

  DefaultStationOppositeDirectionExitMinimumWaitTimeInMinutes : Integer = 2;
  StationOppositeDirectionExitMinimumWaitTimeInMinutes : Integer = 2;

  DefaultStationSameDirectionExitMinimumWaitTimeInMinutes : Integer = 1;
  StationSameDirectionExitMinimumWaitTimeInMinutes : Integer = 1;

  DefaultStationStartMode : Boolean = False;

  DefaultStationStartOfDayPassengerBoardingTimeInMinutes : Integer = 4;
  StationStartOfDayPassengerBoardingTimeInMinutes : Integer = 4;

  DefaultStationMonitorsFontName : String = 'Arial';
  StationMonitorsFontName : String;

  DefaultStationMonitorsLargeFontHeight : Integer = 48;
  StationMonitorsLargeFontHeight : Integer;

  DefaultStationMonitorsSmallFontHeight : Integer = 36;
  StationMonitorsSmallFontHeight : Integer;

  DefaultStationMonitorsWebPageRequired : Boolean = False;
  StationMonitorsWebPageRequired : Boolean;

  DefaultStopAllLocosAtShutDown : Boolean = True;
  StopAllLocosAtShutDown : Boolean;

  DefaultSwitchActiveLocoLightsOffAtShutDown : Boolean = True;
  SwitchActiveLocoLightsOffAtShutDown : Boolean;

  DefaultTCFeedbackDataOutOfUseColour : TColour = clFWPOrange;
  TCFeedbackDataOutOfUseColour : TColour = clFWPOrange;

  DefaultTCFeedbackDataInUseColour : TColour = clLime;
  TCFeedbackDataInUseColour : TColour = clLime;

  DefaultTCFeedbackOccupationButOutOfUseColour : TColour = clFuchsia;
  TCFeedbackOccupationButOutOfUseColour : TColour = clFuchsia;

  DefaultTCFeedbackOccupationColour : TColour = clRed;
  TCFeedbackOccupationColour : TColour = clRed;

  DefaultTCLocoOutOfPlaceOccupationColour : TColour = clLime;
  TCLocoOutOfPlaceOccupationColour : TColour = clLime;

  DefaultTCLocoOutOfPlaceOccupationPenStyle : TPenStyle = psDashDot;
  TCLocoOutOfPlaceOccupationPenStyle : TPenStyle = psDashDot;

  DefaultTCMissingOccupationColour : TColour = clAqua;
  TCMissingOccupationColour : TColour = clAqua;

  DefaultTCOutOfUseAsNoFeedbackReceivedColour : TColour = clDkGray;
  TCOutOfUseAsNoFeedbackReceivedColour : TColour = clDkGray;

  DefaultTCOutOfUseAsNoFeedbackReceivedPenStyle : TPenStyle = psDashDotDot;
  TCOutOfUseAsNoFeedbackReceivedPenStyle : TPenStyle = psDashDotDot;

  DefaultTCOutOfUseSetByUserColour : TColour = clGray;
  TCOutOfUseSetByUserColour : TColour = clGray;

  DefaultTCOutOfUseSetByUserPenStyle : TPenStyle = psDashDot;
  TCOutOfUseSetByUserPenStyle : TPenStyle = psDashDot;

  DefaultTCPermanentFeedbackOccupationColour : TColour = clRed;
  TCPermanentFeedbackOccupationColour : TColour = clRed;

  DefaultTCPermanentFeedbackOccupationPenStyle : TPenStyle = psDashDot;
  TCPermanentFeedbackOccupationPenStyle : TPenStyle = psDashDot;

  DefaultTCPermanentOccupationSetByUserColour : TColour = clFWPOrange;
  TCPermanentOccupationSetByUserColour : TColour = clFWPOrange;

  DefaultTCPermanentOccupationSetByUserPenStyle: TPenStyle = psDot;
  TCPermanentOccupationSetByUserPenStyle : TPenStyle = psDot;

  DefaultTCPermanentSystemOccupationColour : TColour = clFWPPink;
  TCPermanentSystemOccupationColour : TColour = clFWPPink;

  DefaultTCPermanentSystemOccupationPenStyle : TPenStyle = psDashDot;
  TCPermanentSystemOccupationPenStyle : TPenStyle = psDashDot;

  DefaultTCSpeedRestrictionColour : TColour = clDkGray;
  TCSpeedRestrictionColour : TColour = clDkGray;

  DefaultTCSystemOccupationColour : TColour = clFWPPink;
  TCSystemOccupationColour : TColour = clFWPPink;

  DefaultTheatreIndicatorHorizontalSpacing : Integer = 100;
  TheatreIndicatorHorizontalSpacing : Integer;
  TheatreIndicatorHorizontalSpacingScaled : Integer;

  DefaultTheatreIndicatorVerticalSpacing : Integer = 10;
  TheatreIndicatorVerticalSpacing : Integer;
  TheatreIndicatorVerticalSpacingScaled : Integer;

  DefaultTCUnoccupiedColour : TColour = clDkGray; { the default line colour }
  TCUnoccupiedColour : TColour = clDkGray; { the default line colour }

  DefaultTCUserMustDriveColour : TColour = clGreen;
  TCUserMustDriveColour : TColour = clGreen;

  DefaultTheatreBoxHeight : Integer = 10;
  TheatreBoxHeight : Integer;

  DefaultTheatreBoxWidth : Integer = 5;
  TheatreBoxWidth : Integer;

  DefaultTheatreFontHeight : Integer = 15;
  TheatreFontHeight : Integer;

  DefaultTrainActiveColour : TColor = clRed;
  TrainActiveColour : TColour = clRed;

  DefaultTrainInactiveColour : TColour = clWhite;
  TrainInactiveColour : TColour = clWhite;

  DefaultTrackCircuitDataFilename : String = 'TrackCircuitData';
  TrackCircuitDataFilename : String;

  DefaultTrackCircuitDataFilenameSuffix : String = 'mdb';
  TrackCircuitDataFilenameSuffix : String;

  DefaultTRSPlungerColour : TColour = $0038ABB1; { 0-B-G-R not RGB! }
  TRSPlungerColour : TColour = $0038ABB1; { 0-B-G-R not RGB! }

  DefaultTRSPlungerLength : Integer = 60;
  TRSPlungerLength : Integer;
  TRSPlungerLengthScaled : Integer;

  DefaultTRSPlungerOutlineColour : TColour = clBlack;
  TRSPlungerOutlineColour : TColour = clBlack;

  DefaultTRSPlungerPressedColour : TColour = clYellow;
  TRSPlungerPressedColour : TColour = clYellow;

  DefaultUseDisplayLineColoursWindowSettingsOnStartup : Boolean = False;
  UseDisplayLineColoursWindowSettingsOnStartup :  Boolean = False;

  DefaultWaitBeforeRerouteInMinutes : Integer = 5;
  WaitBeforeRerouteInMinutes : Integer;

  DefaultWindowRows : Integer = 25;
  WindowRows : Integer;

  DefaultWorkingTimetableFilename : String = 'WorkingTimetable';
  WorkingTimetableFilename : String;

  DefaultWorkingTimetableFilenameSuffix : String = 'mdb';
  WorkingTimetableFilenameSuffix : String;

  DefaultWorkingTimetableMode : Boolean = True;
  WorkingTimetableMode : Boolean;

  DefaultWorkingTimetableWindowGridBackgroundColour : TColor = clMenu;
  WorkingTimetableWindowGridBackgroundColour : TColour = clMenu;

  RDCBailOffMin : Integer = 0;
  RDCBailOffMax : Integer = 0;
  RDCEmergencyBrakeMin : Integer = 0;
  RDCEmergencyBrakeMax : Integer = 0;
  RDCLocoBrakeMin : Integer = 0;
  RDCLocoBrakeMax : Integer = 0;
  RDCRegulatorForwardMin : Integer = 0;
  RDCRegulatorForwardMax : Integer = 0;
  RDCRegulatorReverseMin : Integer = 0;
  RDCRegulatorReverseMax : Integer = 0;
  RDCReverserForwardMin : Integer = 0;
  RDCReverserForwardMax : Integer = 0;
  RDCReverserReverseMin : Integer = 0;
  RDCReverserReverseMax : Integer = 0;
  RDCTrainBrakeMin : Integer = 0;
  RDCTrainBrakeMax : Integer = 0;
  RDCThreeWaySwitchALeftNum : Integer = 0;
  RDCThreeWaySwitchAMidNum : Integer = 0;
  RDCThreeWaySwitchARightNum : Integer = 0;
  RDCThreeWaySwitchBLeftNum : Integer = 0;
  RDCThreeWaySwitchBMidNum : Integer = 0;
  RDCThreeWaySwitchBRightNum : Integer = 0;

IMPLEMENTATION

{$R *.dfm}

USES MiscUtils, Raildraw, Locks, LocoUtils, CreateRoute, Diagrams, GetTime, Help, LocationData, Edit, WorkingTimetable, LocoDialogue, Logging, IniFiles;

CONST
  UnitRef = 'Options';

VAR
  CellStringCount : Integer = 0;
  CellStringStartArray : IntegerArrayType;
  OptionsWindowFindDialogActive : Boolean = False;
  PreviousFoundPos : Integer;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

CONST
  ColoursSectionStr = 'Colours';
    { Colours for buffer stops }
    BufferStopColourStr = 'Buffer Stop Colour';
    BufferStopNumberColourStr = 'Buffer Stop Number Colour';
    BufferStopRedStr = 'Buffer Stop Red';

    { Colours for points }
    PointColourStr = 'Point Colour';
    PointDivergingLineColourStr = 'Point Diverging Line Colour';
    PointDownFacingColourStr = 'Point Down Facing Colour';
    PointFeedbackDataInUseColourStr = 'Point Feedback Data In Use Colour';
    PointFeedbackDataOutOfUseColourStr = 'Point Feedback Data Out Of Use Colour';
    PointHeelLineColourStr = 'Point Heel Line Colour';
    PointLenzNumberColourStr = 'Point Lenz Number Colour';
    PointLockedByUserColourStr = 'Point Locked By User Colour';
    PointManualOperationColourStr = 'Point Manual Operation Colour';
    PointOutOfUseColourStr = 'Point Out Of Use Colour';
    PointStraightLineColourStr = 'Point Straight Line Colour';
    PointsWithoutFeedbackColourStr = 'Points Without Feedback Colour';
    PointUndrawColourStr = 'Point Undraw Colour';
    PointUpFacingColourStr = 'Up Facing Point Colour';
    ShowPointDefaultStateColourStr = 'Show Point Default State Colour';
    ShowPointLockedColourStr = 'Show Point Locked Colour';

    { Colours for signal posts }
    SignalPostColourStr = 'Signal Post Colour';
    SignalPostEmergencyRouteSettingColourStr = 'Signal Post Emergency Route Setting Colour';
    SignalPostRouteSettingColourStr = 'Signal Post Route Setting Colour';
    SignalPostStationStartModeColourStr = 'Signal Post Station Start Mode Colour';
    SignalPostTheatreSettingColourStr = 'Signal Post Theatre Setting Colour';

    { Colours for signals }
    SignalAspectUnlitStr = 'Signal Aspect Unlit';
    SignalAspectRedStr = 'Signal Aspect Red';
    SignalAspectYellowStr = 'Signal Aspect Yellow';
    SignalAspectGreenStr = 'Signal Aspect Green';
    SignalNumberColourStr = 'Signal Number Colour';

    { Colours for lines }
    LineNotAvailableColourStr = 'Line Not Available Colour';
    LineRoutedOverColourStr = 'Line Routed Over Colour';

    { Colours for platforms }
    PlatformColourStr = 'Platform Colour';
    PlatformNumberColourStr = 'Platform Number Colour';

    { Colours for track circuits }
    TCFeedbackDataInUseColourStr = 'TC Feedback Data In Use Colour';
    TCFeedbackDataOutOfUseColourStr = 'TC Feedback Data Out Of Use Colour';
    TCFeedbackOccupationColourStr = 'TC Feedback Occupation Colour';
    TCFeedbackOccupationButOutOfUseColourStr = 'TC Feedback Occupation But Out Of Use Colour';
    TCLocoOutOfPlaceOccupationColourStr = 'TC Loco Out of Place Occupation Colour';
    TCMissingOccupationColourStr = 'TC Missing Occupation Colour';
    TCOutOfUseAsNoFeedbackReceivedColourStr = 'TC Out Of Use As No Feedback Received Colour';
    TCOutOfUseSetByUserColourStr = 'TC Out Of Use Set By User Colour';
    TCPermanentFeedbackOccupationColourStr = 'TC Permanent Feedback Occupation';
    TCPermanentOccupationSetByUserColourStr = 'TC Permanent Occupation Set By User';
    TCPermanentSystemOccupationColourStr = 'TC Permanent System Occupation';
    TCSpeedRestrictionColourStr = 'TC Speed Restriction Colour';
    TCSystemOccupationColourStr = 'TC System Occupation Colour';
    TCUnknownOccupationColourStr = 'TC Unknown Occupation Colour';
    TCUnoccupiedColourStr = 'TC Unoccupied Colour';
    TCUserMustDriveColourStr = 'TC User Must Drive Colour';

    TrainActiveColourStr = 'Train Active Colour';
    TrainInactiveColourStr = 'Train Inactive Colour';

    { Colours for TRS plungers }
    TRSPlungerColourStr = 'TRS Plunger Colour';
    TRSPlungerOutlineColourStr = 'Plunger Outline Colour';
    TRSPlungerPressedColourStr = 'Plunger Pressed Colour';

    { Miscellaneous colours }
    BackgroundColourStr = 'Background Colour';
    ForegroundColourStr = 'Foreground Colour';
    DiagramsWindowGridBackgroundColourStr = 'Diagrams Window Grid Background Colour';
    LinesWithoutTrackCircuitsColourStr = 'Lines Without Track Circuits Colour';
    LocoStalledColourStr = 'Loco Stalled Colour';
    ScreenComponentEditedColour1Str = 'Screen Component Edited Colour1';
    ScreenComponentEditedColour2Str = 'Screen Component Edited Colour2';
    WorkingTimetableWindowGridBackgroundColourStr = 'Working Timetable Window Grid Background Colour';

  PenStylesSectionStr = 'Pen Styles';
    FiddleyardLinePenStyleStr = 'Fiddleyard Line Pen Style';
    ProjectedLinePenStyleStr = 'Projected Line Pen Style';
    SidingPenStyleStr = 'Siding Pen Style';
    TCOutOfUseAsNoFeedbackReceivedPenStyleStr = 'TC Out Of Use As No Feedback Received Pen Style';
    TCOutOfUseSetByUserPenStyleStr = 'TC Out Of Use Set By User Pen Style';
    TCPermanentFeedbackOccupationPenStyleStr = 'TC Permanent Feedback Occupation Pen Style';
    TCPermanentOccupationSetByUserPenStyleStr = 'TC Permanent Occupation Set By User Pen Style';
    TCPermanentSystemOccupationPenStyleStr = 'TC Permanent System Occupation Pen Style';
    TCLocoOutOfPlaceOccupationPenStyleStr = 'TC Loco Out Of Place Occupation Pen Style';

  FontsSectionStr = 'Fonts';
    RailFontNameStr = 'Rail Font Name';
    LineFontHeightStr = 'Line Font Height';
    LoggingWindowFontNameStr = 'Logging Window Font Name';
    LoggingWindowFontSizeStr = 'Logging Window Font Size';
    FWPRailWindowFontHeightStr = 'Main Window Font Height';
    PlatformNumberFontHeightStr = 'Platform Number Font Height';
    StationMonitorsFontNameStr = 'Station Monitors Font Name';
    StationMonitorsLargeFontHeightStr = 'Station Monitors Large Font Height';
    StationMonitorsSmallFontHeightStr = 'Station Monitors Small Font Height';
    TheatreFontHeightStr = 'Theatre Font Height';

  DialogueBoxSectionStr = 'Dialogue Boxes';
    DebuggingOptionsWindowLeftStr = 'Debugging Options Window Left';
    DebuggingOptionsWindowTopStr = 'Debugging Options Window Top';
    LineDialogueBoxLeftStr = 'Line Dialogue Window Left';
    LineDialogueBoxTopStr = 'Line Dialogue Window Top';
    LocoDialogueWindowLeftStr = 'Loco Dialogue Window Left';
    LocoDialogueWindowTopStr = 'Loco Dialogue Window Top';
    LocoDialogueSpeedInMPHStr = 'Loco Dialogue Speed In MPH';
    PointDialogueBoxLeftStr = 'Point Dialogue Box Left';
    PointDialogueBoxTopStr = 'Point Dialogue Box Top';
    SignalDialogueBoxLeftStr = 'Signal Dialogue Box Left';
    SignalDialogueBoxTopStr = 'Signal Dialogue Box Top';
    TrackCircuitDialogueBoxLeftStr = 'Track-circuit Dialogue Box Left';
    TrackCircuitDialogueBoxTopStr = 'Track-circuit Dialogue Box Top';

  FilesSectionStr = 'Files';
    AreaDataFilenameStr = 'Area Data Filename';
    AreaDataFilenameSuffixStr = 'Area Data Filename Suffix';
    DataCheckFileNameStr = 'Data Check FileName';
    DiagramsFilenameStr = 'Diagrams Filename';
    DiagramsFilenameSuffixStr = 'Diagrams Filename Suffix';
    FeedbackDataFilenameStr = 'Feedback Unit Data Filename';
    FeedbackDataFilenameSuffixStr = 'Feedback Unit Data Filename Suffix';
    LineDataFilenameStr = 'Line Data Filename';
    LineDataFilenameSuffixStr = 'Line Data Filename Suffix';
    LocoDataFilenameStr = 'Loco Data Filename';
    LocationDataFilenameSuffixStr = 'Location Data Filename Suffix';
    LocationDataFilenameStr = 'Location Data Filename';
    LocoDataFilenameSuffixStr = 'Loco Data Filename Suffix';
    LocoLogDirectoryNameStr = 'Loco Log Directory Name';
    LogFilenameStr = 'Log Filename Prefix';
    LogFilenameSuffixStr = 'Log Filename Suffix';
    PathToLogFilesStr = 'Path for Log Files';
    PathToRailDataFilesStr = 'Path For Rail Data Files';
    PathToRailSourceFilesStr = 'Path For Rail Source Files';
    PlatformDataFilenameStr = 'Platform Data Filename';
    PlatformDataFilenameSuffixStr = 'Platform Data Filename Suffix';
    PointDataFilenameStr = 'Point Data Filename';
    PointDataFilenameSuffixStr = 'Point Data Filename Suffix';
    ReplayFilenameStr = 'Replay Filename';
    ReplayFilenameSuffixStr = 'Replay Filename Suffix';
    RouteingExceptionDataFilenameStr = 'Routeing Exception Data Filename';
    RouteingExceptionDataFilenameSuffixStr = 'Routeing Exception Data Filename Suffix';
    SignalDataFilenameStr = 'Signal Data Filename';
    SignalDataFilenameSuffixStr = 'Signal Data Filename Suffix';
    TrackCircuitDataFilenameStr = 'TrackCircuit Data Filename';
    TrackCircuitDataFilenameSuffixStr = 'TrackCircuit Data Filename Suffix';
    WorkingTimetableFilenameStr = 'Working Timetable Filename';
    WorkingTimetableFilenameSuffixStr = 'Working Timetable Filename Suffix';

  MiscellaneousDataSectionStr = 'Miscellaneous Data';
    CurrentParametersStr = 'Current Parameters';

  OtherOptionsSectionStr = 'Other Options';
    AcceptAllPermanentOccupationsWithoutFeedbackStr = 'Accept All Permanent Occupations Without Feedback';
    AutomaticallySetFocusWhenInDebugWindowStr = 'Automatically Set Focus When In Debug Window';
    CancelAllTrainsWithNoFeedbackOccupationStr = 'Cancel All Trains With No Feedback Occupation';
    CarriageLengthInInchesStr = 'Carriage Length In Inches';
    CheckForIdenticalLinesInLogStr = 'Check For Identical Lines In Log';
    DisplayDiagramsStr = 'Display Diagrams';
    DisplayFlashingTrackCircuitsStr = 'Display Flashing Track Circuits';
    DisplayLocoChipNumsStr = 'Display Loco Chip Numbers';
    DisplayLocoHeadcodesStr = 'Display Loco Headcodes';
    DisplayNotForPublicUseTrainsInStationMonitorsStr = 'Display Not For Public Use Trains In Station Monitors';
    DisplayRoutesAndJourneysStr = 'Display Journeys & Routes';
    DisplayWorkingTimetableStr = 'Display Working Timetable';
    DoNotCancelTrainsWithNoFeedbackOccupationStr = 'Do Not Cancel Trains With No Feedback Occupation';
    HighlightTrackCircuitSpeedRestrictionsStr = 'Highlight Track Circuit Speed Restrictions';
    LargeDiagramsWindowSelectedStr = 'Large Diagrams Window Selected';
    LargeWorkingTimetableWindowSelectedStr = 'Large Working Timetable Window Selected';
    LineThicknessInFullScreenModeStr = 'Line Thickness In Full Screen Mode';
    LocoTimingTimeBeforeAutoStopInSecondsStr = 'Loco Timing Time Before Auto Stop In Seconds';
    LogCurrentTimeModeStr = 'Log Current Time Mode';
    LogsKeptModeStr = 'Logs Kept Mode';
    MakeSoundWhenDebugWindowBoldTextAppearsStr = 'Make Sound When Debug Window Bold Text Appears';
    MaxRectangleUndrawTimeStr = 'Max Rectangle Undraw Time';
    MenusVisibleStr = 'Menus Visible';
    MonitorStrayingTrainsStr = 'Monitor Straying Trains';
    PointFeedbackMaximumWaitInSecondsStr = 'Point Feedback Maximum Wait In Seconds';
    RouteAheadNotClearWaitTimeInMinutesStr = 'Route Ahead Not Clear Wait Time In Minutes';
    RunTestUnitOnStartupStr = 'Run Test Unit On Startup';
    ShowCancelledTrainsInDiagramsStr = 'Show Cancelled Trains In Diagrams';
    ShowIncorrectDayOfTheWeekEntriesInWorkingTimetableStr = 'Show Incorrect Day Of The Week Entries In Working Timetable';
    ShowNonMovingTrainsInDiagramsStr = 'Show Non-Moving Trains In Diagrams';
    ShowNonStopsInDiagramsStr = 'Show Non Stops In Diagrams';
    ShowTrackCircuitsWhereUserMustDriveStr = 'Show Track Circuits Where User Must Drive';
    StartRepeatJourneysOnNewLineInDiagramsStr = 'Start Repeat Journeys On New Line In Diagrams';
    StartWithDiagramsStr = 'Start With Diagrams';
    StationEndOfDayPassengerLeavingTimeInMinutesStr = 'Station End Of Day Passenger Leaving Time In Minutes';
    StationMonitorsWebPageRequiredStr = 'Station Monitors Web Page Required';
    StationOppositeDirectionExitMinimumWaitTimeInMinutesStr = 'Station Opposite Direction Exit Minimum Wait Time In Minutes';
    StationSameDirectionExitMinimumWaitTimeInMinutesStr = 'Station Same Direction Exit Minimum Wait Time In Minutes';
    StationStartModeStr = 'Station Start Mode';
    StationStartOfDayPassengerBoardingTimeInMinutesStr = 'Station Start Of Day Passenger Boarding Time In Minutes';
    StopAllLocosAtShutDownStr = 'Stop All Locos At Shut Down';
    SwitchActiveLocoLightsOffAtShutDownStr = 'Switch Active Loco Lights Off At Shut Down';
    TheatreBoxHeightStr = 'Theatre Box Height';
    TheatreBoxWidthStr = 'Theatre Box Width';
    UseDisplayLineColoursWindowSettingsOnStartupStr = 'Use Display Line Colours Window Settings On Startup';
    WaitBeforeRerouteInMinutesStr = 'Wait Before Reroute In Minutes';
    WorkingTimetableModeStr = 'WorkingTimetable Mode';

  RDCSectionStr = 'RDC';
    RDCBailOffMinStr = 'RDC BailOff Min';
    RDCBailOffMaxStr = 'RDC BailOff Max';
    RDCEmergencyBrakeMinStr = 'RDC Emergency Brake Min';
    RDCEmergencyBrakeMaxStr = 'RDC Emergency Brake Max';
    RDCLocoBrakeMinStr = 'RDC Loco Brake Min';
    RDCLocoBrakeMaxStr = 'RDC Loco Brake Max';
    RDCRegulatorForwardMinStr = 'RDC Regulator Forward Min';
    RDCRegulatorForwardMaxStr = 'RDC Regulator Forward Max';
    RDCRegulatorReverseMinStr = 'RDC Regulator Reverse Min';
    RDCRegulatorReverseMaxStr = 'RDC Regulator Reverse Max';
    RDCReverserForwardMinStr = 'RDC Reverser Forward Min';
    RDCReverserForwardMaxStr = 'RDC Reverser Forward Max';
    RDCReverserReverseMinStr = 'RDC Reverser Reverse Min';
    RDCReverserReverseMaxStr = 'RDC Reverser Reverse Max';
    RDCThreeWaySwitchALeftNumStr = 'RDC ThreeWay Switch A Left';
    RDCThreeWaySwitchAMidNumStr = 'RDC ThreeWay Switch A Mid';
    RDCThreeWaySwitchARightNumStr = 'RDC ThreeWay Switch A Right';
    RDCThreeWaySwitchBLeftNumStr = 'RDC ThreeWay Switch B Left';
    RDCThreeWaySwitchBMidNumStr = 'RDC ThreeWay Switch B Mid';
    RDCThreeWaySwitchBRightNumStr = 'RDC ThreeWay Switch B Right';
    RDCTrainBrakeMinStr = 'RDC Train Brake Min';
    RDCTrainBrakeMaxStr = 'RDC Train Brake Max';

  ScreenOptionsStr = 'Screen Options';
    CustomWindowedScreenStr = 'Custom Windowed Screen';
    DefaultWindowedScreenStr = 'Default Windowed Screen';
    FullScreenStr = 'Full Screen Without Status Bar';
    FullScreenWithStatusBarStr = 'Full Screen With Status Bar';
    LogFileMaxWidthInCharsStr = 'Log File Max Width In Chars';
    ScreenModeStr = 'ScreenMode';

    BufferStopVerticalSpacingStr = 'Buffer Stop Vertical Spacing';
    DeltaPointXStr = 'DeltaPointX';
    IndicatorHorizontalSpacingStr = 'Indicator Horizontal Spacing';
    IndicatorVerticalSpacingStr = 'Indicator Vertical Spacing';
    MouseRectangleEdgeVerticalSpacingStr = 'Mouse Rectangle Edge Vertical Spacing';
    PlatformEdgeVerticalSpacingStr = 'Platform Edge Vertical Spacing';
    PlatformNumberEdgeHorizontalSpacingStr = 'Platform Number Edge Horizontal Spacing';
    PlatformNumberEdgeVerticalSpacingStr = 'Platform Number Edge Vertical Spacing';
    SignalHorizontalSpacingStr = 'Signal Horizontal Spacing';
    SignalRadiusStr = 'Signal Radius';
    SignalSemaphoreHeightStr = 'Signal Semaphore Height';
    SignalSemaphoreWidthStr = 'Signal Semaphore Width';
    SignalVerticalSpacingStr = 'Signal Vertical Spacing';
    SpeedRestrictionHorizontalSpacingStr = 'Speed Restriction Horizontal Spacing';
    SpeedRestrictionVerticalSpacingStr = 'Speed Restriction Vertical Spacing';
    TheatreIndicatorHorizontalSpacingStr = 'Theatre Indicator Horizontal Spacing';
    TheatreIndicatorVerticalSpacingStr = 'Theatre Indicator Vertical Spacing';
    TRSPlungerLengthStr = 'TRS Plunger Length';
    WindowRowsStr = 'Window Rows';

  TimesSectionStr = 'Times';
    { These start time constants have to have different names from the variables containing the program start time itself }
    ProgramStartTimeOptionStr = 'Program Start Time';
    DayLightEndTimeOptionStr = 'Daylight End Time';
    DayLightStartTimeOptionStr = 'Daylight Start Time';

    CurrentRailwayDayOfTheWeekStr = 'Current Railway Day Of The Week';

  TrackCircuitsSectionStr = 'TrackCircuits';
    TrackCircuitOutOfUseSetByUserStr = 'Track Circuit Out Of Use Set By User';
    TrackCircuitOutOfUseAsNoFeedbackReceivedStr = 'Track Circuit Out Of Use As No Feedback Received';
    TrackCircuitPermanentOccupationSetByUserStr = 'Track Circuit Permanent Occupation Set By User';
    TrackCircuitSpeedRestrictionStr = 'Track Circuit Speed Restriction';
    TrackCircuitUserMustDriveStr = 'Track Circuit User Must Drive';

  UserSectionStr = 'User';

  WindowsSectionStr = 'Windows';
    FWPRailWindowTopStr = 'Main Window Top';
    FWPRailWindowLeftStr = 'Main Window Left';
    FWPRailWindowWidthStr = 'Main Window Width';
    FWPRailWindowHeightStr = 'Main Window Height';
    FWPRailWindowClientWidthStr = 'Main Window Client Width';
    FWPRailWindowClientHeightStr = 'Main Window Client Height';

    DisplayLineColoursWindowHeightStr = 'CreateRoute Display Line Colours Window Height';
    DisplayLineColoursWindowLeftStr = 'CreateRoute Display Line Colours Window Left';
    DisplayLineColoursWindowTopStr = 'CreateRoute Display Line Colours Window Top';
    DisplayLineColoursWindowWidthStr = 'CreateRoute Display Line Colours Window Width';

    EditWindowHeightStr = 'Edit Window Height';
    EditWindowLeftStr = 'Edit Window Left';
    EditWindowTopStr = 'Edit Window Top';
    EditWindowWidthStr = 'Edit Window Width';

    DebugWindowHeightStr = 'MiscUtils Debug Window Height';
    DebugWindowLeftStr = 'MiscUtils Debug Window Left';
    DebugWindowTopStr = 'MiscUtils Debug Window Top';
    DebugWindowWidthStr = 'MiscUtils Debug Window Width';

    DiagramsWindowHeightStr = 'Diagrams Window Height';
    DiagramsWindowLeftStr = 'Diagrams Window Left';
    DiagramsWindowTopStr = 'Diagrams Window Top';
    DiagramsSmallWindowWidthStr = 'Diagrams Small Window Width';
    DiagramsLargeWindowWidthStr = 'Diagrams Large Window Width';

    LocationDataWindowHeightStr = 'LocationData Window Height';
    LocationDataWindowLeftStr = 'LocationData Window Left';
    LocationDataWindowTopStr = 'LocationData Window Top';
    LocationDataWindowWidthStr = 'LocationData Window Width';

    LockListWindowHeightStr = 'LockList Window Height';
    LockListWindowLeftStr = 'LockList Window Left';
    LockListWindowTopStr = 'LockList Window Top';
    LockListWindowWidthStr = 'LockList Window Width';

    LocoUtilsWindowHeightStr = 'LocoUtils Window Height';
    LocoUtilsWindowLeftStr = 'LocoUtils Window Left';
    LocoUtilsWindowTopStr = 'LocoUtils Window Top';
    LocoUtilsWindowWidthStr = 'LocoUtils Window Width';

    LoggingWindowHeightStr = 'Logging Window Height';
    LoggingWindowLeftStr = 'Logging Window Left';
    LoggingWindowTopStr = 'Logging Window Top';
    LoggingWindowWidthStr = 'Logging Window Width';

    MovementWindowHeightStr = 'Movement Window Height';
    MovementWindowLeftStr = 'Movement Window Left';
    MovementWindowTopStr = 'Movement Window Top';
    MovementWindowWidthStr = 'Movement Window Width';

    OptionsWindowHeightStr = 'Options Window Height';
    OptionsWindowLeftStr = 'Options Window Left';
    OptionsWindowTopStr = 'Options Window Top';
    OptionsWindowWidthStr = 'Options Window Width';
    OptionsWindowValueListEditorCol0WidthStr = 'Options Window ValueListEditor Col0 Width';

    WorkingTimetableWindowHeightStr = 'Working Timetable Window Height';
    WorkingTimetableWindowLeftStr = 'Working Timetable Window Left';
    WorkingTimetableWindowTopStr = 'Working Timetable Window Top';
    WorkingTimetableSmallWindowWidthStr = 'Working Timetable Small Window Width';
    WorkingTimetableLargeWindowWidthStr = 'Working Timetable Large Window Width';

PROCEDURE RestoreScreenDefaults;
{ Restore screen colours, fonts, etc. }
BEGIN
  { Colours for buffer stops }
  BufferStopColour := DefaultBufferStopColour;
  BufferStopNumberColour := DefaultBufferStopNumberColour;
  BufferStopRed := DefaultBufferStopRed;

  { Colours for TRS plungers }
  TRSPlungerColour := DefaultTRSPlungerColour;
  TRSPlungerOutlineColour := DefaultTRSPlungerOutlineColour;
  TRSPlungerPressedColour := DefaultTRSPlungerPressedColour;

  { Colours for platforms }
  PlatformColour := DefaultPlatformColour;
  PlatformNumberColour := DefaultPlatformNumberColour;

  { Colours for points }
  PointColour := DefaultPointColour;
  PointDivergingLineColour := DefaultPointDivergingLineColour;
  PointDownFacingColour := DefaultPointDownFacingColour;
  PointFeedbackDataInUseColour := DefaultPointFeedbackDataInUseColour;
  PointFeedbackDataOutOfUseColour := DefaultPointFeedbackDataOutOfUseColour;
  PointHeelLineColour := DefaultPointHeelLineColour;
  PointLenzNumberColour := DefaultPointLenzNumberColour;
  PointLockedByUserColour := DefaultPointLockedByUserColour;
  PointManualOperationColour := DefaultPointManualOperationColour;
  PointOutOfUseColour := DefaultPointOutOfUseColour;
  PointStraightLineColour := DefaultPointStraightLineColour;
  PointsWithoutFeedbackColour := DefaultPointsWithoutFeedbackColour;
  PointUndrawColour := DefaultPointUndrawColour;
  PointUpFacingColour := DefaultPointUpFacingColour;
  ShowPointDefaultStateColour := DefaultShowPointDefaultStateColour;
  ShowPointLockedColour := DefaultShowPointLockedColour;

  { Colours for signal posts }
  SignalPostColour := DefaultSignalPostColour;
  SignalPostEmergencyRouteSettingColour := DefaultSignalPostEmergencyRouteSettingColour;
  SignalPostRouteSettingColour := DefaultSignalPostRouteSettingColour;
  SignalPostStationStartModeColour := DefaultSignalPostStationStartModeColour;
  SignalPostTheatreSettingColour := DefaultSignalPostTheatreSettingColour;

  { Colours for signals }
  SignalAspectGreen := DefaultSignalAspectGreen;
  SignalAspectRed := DefaultSignalAspectRed;
  SignalAspectUnlit := DefaultSignalAspectUnlit;
  SignalAspectYellow := DefaultSignalAspectYellow;
  SignalNumberColour := DefaultSignalNumberColour;

  { Colours for lines }
  LineNotAvailableColour := DefaultLineNotAvailableColour;
  LineRoutedOverColour := DefaultLineRoutedOverColour;

  { Colours for track circuits }
  TCFeedbackDataInUseColour := DefaultTCFeedbackDataInUseColour;
  TCFeedbackDataOutOfUseColour := DefaultTCFeedbackDataOutOfUseColour;
  TCFeedbackOccupationColour := DefaultTCFeedbackOccupationColour;
  TCFeedbackOccupationButOutOfUseColour := DefaultTCFeedbackOccupationButOutOfUseColour;
  TCLocoOutOfPlaceOccupationColour := DefaultTCLocoOutOfPlaceOccupationColour;
  TCMissingOccupationColour := DefaultTCMissingOccupationColour;
  TCOutOfUseSetByUserColour := DefaultTCOutOfUseSetByUserColour;
  TCOutOfUseAsNoFeedbackReceivedColour := DefaultTCOutOfUseAsNoFeedbackReceivedColour;

  TCPermanentFeedbackOccupationColour := DefaultTCPermanentFeedbackOccupationColour;
  TCPermanentOccupationSetByUserColour := DefaultTCPermanentOccupationSetByUserColour;
  TCPermanentSystemOccupationColour := DefaultTCPermanentSystemOccupationColour;
  TCSpeedRestrictionColour := DefaultTCSpeedRestrictionColour;
  TCSystemOccupationColour := DefaultTCSystemOccupationColour;
  TCUnoccupiedColour := DefaultTCUnoccupiedColour;
  TCUserMustDriveColour := DefaultTCUserMustDriveColour;
  TrainActiveColour := DefaultTrainActiveColour;
  TrainInactiveColour := DefaultTrainInactiveColour;

  { Miscellaneous colours }
  BackgroundColour := DefaultBackgroundColour;
  ForegroundColour := DefaultForegroundColour;
  DiagramsWindowGridBackgroundColour := DefaultDiagramsWindowGridBackgroundColour;
  LinesWithoutTrackCircuitsColour := DefaultLinesWithoutTrackCircuitsColour;
  LocoStalledColour := DefaultLocoStalledColour;
  ScreenComponentEditedColour1 := DefaultScreenComponentEditedColour1;
  ScreenComponentEditedColour2 := DefaultScreenComponentEditedColour2;
  WorkingTimetableWindowGridBackgroundColour := DefaultWorkingTimetableWindowGridBackgroundColour;

  { Pen styles }
  FiddleyardLinePenStyle := DefaultFiddleyardLinePenStyle;
  ProjectedLinePenStyle := DefaultProjectedLinePenStyle;
  SidingPenStyle := DefaultSidingPenStyle;
  TCLocoOutOfPlaceOccupationPenStyle := DefaultTCLocoOutOfPlaceOccupationPenStyle;
  TCOutOfUseAsNoFeedbackReceivedPenStyle := DefaultTCOutOfUseAsNoFeedbackReceivedPenStyle;
  TCOutOfUseSetByUserPenStyle := DefaultTCOutOfUseSetByUserPenStyle;
  TCPermanentFeedbackOccupationPenStyle := DefaultTCPermanentFeedbackOccupationPenStyle;
  TCPermanentOccupationSetByUserPenStyle := DefaultTCPermanentOccupationSetByUserPenStyle;
  TCPermanentSystemOccupationPenStyle := DefaultTCPermanentSystemOccupationPenStyle;

  { Fonts }
  RailFontName := DefaultRailFontName;
  LineFontHeight := DefaultLineFontHeight;
  LoggingWindowFontName := DefaultLoggingWindowFontName;
  LoggingWindowFontSize := DefaultLoggingWindowFontSize;
  FWPRailWindowFontHeight := DefaultFWPRailWindowFontHeight;
  PlatformNumberFontHeight := DefaultPlatformNumberFontHeight;
  StationMonitorsFontName := DefaultStationMonitorsFontName;
  StationMonitorsLargeFontHeight := DefaultStationMonitorsLargeFontHeight;
  StationMonitorsSmallFontHeight := DefaultStationMonitorsSmallFontHeight;
  TheatreFontHeight := DefaultTheatreFontHeight;
END; { RestoreScreenDefaults }

PROCEDURE ReadIniFile;
{ Read in data from the .ini file or from the Registry, except for the track-circuit data }
VAR
  IniFile : TIniFile;
  RegistryIniFile : TRegistryIniFile;
  TempStr : String;

  FUNCTION FWPReadString(SectionStr, DataStr, DefaultDataStr : String) : String;
  BEGIN
    IF ReadFromRegistry THEN
      Result := RegistryIniFile.ReadString(SectionStr, DataStr, DefaultDataStr)
    ELSE
      Result := IniFile.ReadString(SectionStr, DataStr, DefaultDataStr);
  END; { FWPReadString }

  FUNCTION FWPReadBool(SectionStr, DataStr: String; DefaultDataBool : Boolean) : Boolean;
  BEGIN
    IF ReadFromRegistry THEN
      Result := RegistryIniFile.ReadBool(SectionStr, DataStr, DefaultDataBool)
    ELSE
      Result := IniFile.ReadBool(SectionStr, DataStr, DefaultDataBool);
  END; { FWPReadBool }

  FUNCTION FWPReadInteger(SectionStr, DataStr : String; DefaultDataInt : Integer) : Integer;
  BEGIN
    IF ReadFromRegistry THEN
      Result := RegistryIniFile.ReadInteger(SectionStr, DataStr, DefaultDataInt)
    ELSE
      Result := IniFile.ReadInteger(SectionStr, DataStr, DefaultDataInt);
  END; { FWPReadInteger }

BEGIN
  TRY
    IF ReadFromRegistry THEN
      RegistryIniFile := TRegistryIniFile.Create('FWPRail')
    ELSE
      IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Rail.ini');

    { Check for various user supplied file data.
      NB Track-circuit data is read in separately in the ReadIniFileForTrackCircuitData routine.
    }
    PathToLogFiles := FWPReadString(FilesSectionStr, PathToLogFilesStr, '');
    PathToRailDataFiles := FWPReadString(FilesSectionStr, PathToRailDataFilesStr, '');
    PathToRailSourceFiles := FWPReadString(FilesSectionStr, PathToRailSourceFilesStr, '');
    AreaDataFilename := FWPReadString(FilesSectionStr, AreaDataFilenameStr, DefaultAreaDataFilename);
    AreaDataFilenameSuffix := FWPReadString(FilesSectionStr, AreaDataFilenameSuffixStr, DefaultAreaDataFilenameSuffix);
    DataCheckFileName := FWPReadString(FilesSectionStr, DataCheckFileNameStr, DefaultDataCheckFileName);
    DiagramsFilename := FWPReadString(FilesSectionStr, DiagramsFilenameStr, DefaultDiagramsFilename);
    DiagramsFilenameSuffix := FWPReadString(FilesSectionStr, DiagramsFilenameSuffixStr, DefaultDiagramsFilenameSuffix);
    FeedbackDataFilename := FWPReadString(FilesSectionStr, FeedbackDataFilenameStr, DefaultFeedbackDataFilename);
    FeedbackDataFilenameSuffix := FWPReadString(FilesSectionStr, FeedbackDataFilenameSuffixStr, DefaultFeedbackDataFilenameSuffix);
    LineDataFilename := FWPReadString(FilesSectionStr, LineDataFilenameStr, DefaultLineDataFilename);
    LineDataFilenameSuffix := FWPReadString(FilesSectionStr, LineDataFilenameSuffixStr, DefaultLineDataFilenameSuffix);

    LocationDataFilename := FWPReadString(FilesSectionStr, LocationDataFilenameStr, DefaultLocationDataFilename);
    LocationDataFilenameSuffix := FWPReadString(FilesSectionStr, LocationDataFilenameSuffixStr, DefaultLocationDataFilenameSuffix);
    LocoDataFilename := FWPReadString(FilesSectionStr, LocoDataFilenameStr, DefaultLocoDataFilename);
    LocoDataFilenameSuffix := FWPReadString(FilesSectionStr, LocoDataFilenameSuffixStr, DefaultLocoDataFilenameSuffix);
    LogFilename := FWPReadString(FilesSectionStr, LogFilenameStr, DefaultLogFilename);
    LogFilenameSuffix := FWPReadString(FilesSectionStr, LogFilenameSuffixStr, DefaultLogFilenameSuffix);
    PlatformDataFilename := FWPReadString(FilesSectionStr, PlatformDataFilenameStr, DefaultPlatformDataFilename);
    PlatformDataFilenameSuffix := FWPReadString(FilesSectionStr, PlatformDataFilenameSuffixStr, DefaultPlatformDataFilenameSuffix);
    PointDataFilename := FWPReadString(FilesSectionStr, PointDataFilenameStr, DefaultPointDataFilename);
    PointDataFilenameSuffix := FWPReadString(FilesSectionStr, PointDataFilenameSuffixStr, DefaultPointDataFilenameSuffix);
    ReplayFilename := FWPReadString(FilesSectionStr, ReplayFilenameStr, DefaultReplayFilename);
    ReplayFilenameSuffix := FWPReadString(FilesSectionStr, ReplayFilenameSuffixStr, DefaultReplayFilenameSuffix);
    RouteingExceptionDataFilename := FWPReadString(FilesSectionStr, RouteingExceptionDataFilenameStr, DefaultRouteingExceptionDataFilename);
    RouteingExceptionDataFilenameSuffix := FWPReadString(FilesSectionStr, RouteingExceptionDataFilenameSuffixStr, DefaultRouteingExceptionDataFilenameSuffix);
    SignalDataFilename := FWPReadString(FilesSectionStr, SignalDataFilenameStr, DefaultSignalDataFilename);
    SignalDataFilenameSuffix := FWPReadString(FilesSectionStr, SignalDataFilenameSuffixStr, DefaultSignalDataFilenameSuffix);
    TrackCircuitDataFilename := FWPReadString(FilesSectionStr, TrackCircuitDataFilename, DefaultTrackCircuitDataFilename);
    TrackCircuitDataFilenameSuffix := FWPReadString(FilesSectionStr, TrackCircuitDataFilenameSuffixStr, DefaultTrackCircuitDataFilenameSuffix);
    WorkingTimetableFilename := FWPReadString(FilesSectionStr, WorkingTimetableFilename, DefaultWorkingTimetableFilename);
    WorkingTimetableFilenameSuffix := FWPReadString(FilesSectionStr, WorkingTimetableFilenameSuffixStr, DefaultWorkingTimetableFilenameSuffix);

    { Colours for buffer stops }
    BufferStopColour := StrToColour(FWPReadString(ColoursSectionStr, BufferStopColourStr, ColourToStr(DefaultBufferStopColour)));
    BufferStopNumberColour := StrToColour(FWPReadString(ColoursSectionStr, BufferStopNumberColourStr, ColourToStr(DefaultBufferStopNumberColour)));
    BufferStopRed := StrToColour(FWPReadString(ColoursSectionStr, BufferStopRedStr, ColourToStr(DefaultBufferStopRed)));

    { Colours for lines }
    LineNotAvailableColour := StrToColour(FWPReadString(ColoursSectionStr, LineNotAvailableColourStr, ColourToStr(DefaultLineNotAvailableColour)));
    LineRoutedOverColour := StrToColour(FWPReadString(ColoursSectionStr, LineRoutedOverColourStr, ColourToStr(DefaultLineRoutedOverColour)));

    { Colours for TRS plungers }
    TRSPlungerColour := StrToColour(FWPReadString(ColoursSectionStr, TRSPlungerColourStr, ColourToStr(DefaultTRSPlungerColour)));
    TRSPlungerOutlineColour := StrToColour(FWPReadString(ColoursSectionStr, TRSPlungerOutlineColourStr, ColourToStr(DefaultTRSPlungerOutlineColour)));
    TRSPlungerPressedColour := StrToColour(FWPReadString(ColoursSectionStr, TRSPlungerPressedColourStr, ColourToStr(DefaultTRSPlungerPressedColour)));

    { Colours for platforms }
    PlatformColour := StrToColour(FWPReadString(ColoursSectionStr, PlatformColourStr, ColourToStr(DefaultPlatformColour)));
    PlatformNumberColour := StrToColour(FWPReadString(ColoursSectionStr, PlatformNumberColourStr, ColourToStr(DefaultPlatformNumberColour)));

    { Colours for points }
    PointColour := StrToColour(FWPReadString(ColoursSectionStr, PointColourStr, ColourToStr(DefaultPointColour)));
    PointDivergingLineColour := StrToColour(FWPReadString(ColoursSectionStr, PointDivergingLineColourStr, ColourToStr(DefaultPointDivergingLineColour)));
    PointDownFacingColour := StrToColour(FWPReadString(ColoursSectionStr, PointDownFacingColourStr, ColourToStr(DefaultPointDownFacingColour)));
    PointFeedbackDataInUseColour := StrToColour(FWPReadString(ColoursSectionStr, PointFeedbackDataInUseColourStr, ColourToStr(DefaultPointFeedbackDataInUseColour)));
    PointFeedbackDataOutOfUseColour := StrToColour(FWPReadString(ColoursSectionStr, PointFeedbackDataoutOfUseColourStr,
                                                                                                                    ColourToStr(DefaultPointFeedbackDataOutOfUseColour)));
    PointHeelLineColour := StrToColour(FWPReadString(ColoursSectionStr, PointHeelLineColourStr, ColourToStr(DefaultPointHeelLineColour)));
    PointLenzNumberColour := StrToColour(FWPReadString(ColoursSectionStr, PointLenzNumberColourStr, ColourToStr(DefaultPointLenzNumberColour)));
    PointLockedByUserColour := StrToColour(FWPReadString(ColoursSectionStr, PointLockedByUserColourStr, ColourToStr(DefaultPointLockedByUserColour)));
    PointManualOperationColour := StrToColour(FWPReadString(ColoursSectionStr, PointManualOperationColourStr, ColourToStr(DefaultPointManualOperationColour)));
    PointOutOfUseColour := StrToColour(FWPReadString(ColoursSectionStr, PointOutOfUseColourStr, ColourToStr(DefaultPointOutOfUseColour)));
    PointStraightLineColour := StrToColour(FWPReadString(ColoursSectionStr, PointStraightLineColourStr, ColourToStr(DefaultPointStraightLineColour)));
    PointsWithoutFeedbackColour := StrToColour(FWPReadString(ColoursSectionStr, PointsWithoutFeedbackColourStr, ColourToStr(DefaultPointsWithoutFeedbackColour)));
    PointUndrawColour := StrToColour(FWPReadString(ColoursSectionStr, PointUndrawColourStr, ColourToStr(DefaultPointUndrawColour)));
    PointUpFacingColour := StrToColour(FWPReadString(ColoursSectionStr, PointUpFacingColourStr, ColourToStr(DefaultPointUpFacingColour)));
    ShowPointDefaultStateColour := StrToColour(FWPReadString(ColoursSectionStr, ShowPointDefaultStateColourStr, ColourToStr(DefaultShowPointDefaultStateColour)));
    ShowPointLockedColour := StrToColour(FWPReadString(ColoursSectionStr, ShowPointLockedColourStr, ColourToStr(DefaultShowPointLockedColour)));

    { Colours for signal posts }
    SignalPostColour := StrToColour(FWPReadString(ColoursSectionStr, SignalPostColourStr, ColourToStr(DefaultSignalPostColour)));
    SignalPostEmergencyRouteSettingColour := StrToColour(FWPReadString(ColoursSectionStr, SignalPostEmergencyRouteSettingColourStr,
                                                                                                              ColourToStr(DefaultSignalPostEmergencyRouteSettingColour)));
    SignalPostRouteSettingColour := StrToColour(FWPReadString(ColoursSectionStr, SignalPostRouteSettingColourStr, ColourToStr(DefaultSignalPostRouteSettingColour)));
    SignalPostStationStartModeColour := StrToColour(FWPReadString(ColoursSectionStr, SignalPostStationStartModeColourStr,
                                                                                                                   ColourToStr(DefaultSignalPostStationStartModeColour)));
    SignalPostTheatreSettingColour := StrToColour(FWPReadString(ColoursSectionStr, SignalPostTheatreSettingColourStr, ColourToStr(DefaultSignalPostTheatreSettingColour)));

    { Colours for signals }
    SignalAspectGreen := StrToColour(FWPReadString(ColoursSectionStr, SignalAspectGreenStr, ColourToStr(DefaultSignalAspectGreen)));
    SignalAspectRed := StrToColour(FWPReadString(ColoursSectionStr, SignalAspectRedStr, ColourToStr(DefaultSignalAspectRed)));
    SignalAspectUnlit := StrToColour(FWPReadString(ColoursSectionStr, SignalAspectUnlitStr, ColourToStr(DefaultSignalAspectUnlit)));
    SignalAspectYellow := StrToColour(FWPReadString(ColoursSectionStr, SignalAspectYellowStr, ColourToStr(DefaultSignalAspectYellow)));
    SignalNumberColour := StrToColour(FWPReadString(ColoursSectionStr, SignalNumberColourStr, ColourToStr(DefaultSignalNumberColour)));


    { Colours for track circuits }
    TCFeedbackDataInUseColour := StrToColour(FWPReadString(ColoursSectionStr, TCFeedbackDataInUseColourStr, ColourToStr(DefaultTCFeedbackDataInUseColour)));
    TCFeedbackDataOutOfUseColour := StrToColour(FWPReadString(ColoursSectionStr, TCFeedbackDataOutOfUseColourStr, ColourToStr(DefaultTCFeedbackDataOutOfUseColour)));
    TCFeedbackOccupationColour := StrToColour(FWPReadString(ColoursSectionStr, TCFeedbackOccupationColourStr, ColourToStr(DefaultTCFeedbackOccupationColour)));
    TCFeedbackOccupationButOutOfUseColour := StrToColour(FWPReadString(ColoursSectionStr, TCFeedbackOccupationButOutOfUseColourStr,
                                                                                                              ColourToStr(DefaultTCFeedbackOccupationButOutOfUseColour)));
    TCLocoOutOfPlaceOccupationColour := StrToColour(FWPReadString(ColoursSectionStr, TCLocoOutOfPlaceOccupationColourStr,
                                                                                                                   ColourToStr(DefaultTCLocoOutOfPlaceOccupationColour)));
    TCMissingOccupationColour := StrToColour(FWPReadString(ColoursSectionStr, TCMissingOccupationColourStr, ColourToStr(DefaultTCMissingOccupationColour)));
    TCOutOfUseSetByUserColour := StrToColour(FWPReadString(ColoursSectionStr, TCOutOfUseSetByUserColourStr, ColourToStr(DefaultTCOutOfUseSetByUserColour)));
    TCOutOfUseAsNoFeedbackReceivedColour := StrToColour(FWPReadString(ColoursSectionStr, TCOutOfUseAsNoFeedbackReceivedColourStr,
                                                                                                               ColourToStr(DefaultTCOutOfUseAsNoFeedbackReceivedColour)));

    TCPermanentFeedbackOccupationColour := StrToColour(FWPReadString(ColoursSectionStr, TCPermanentFeedbackOccupationColourStr,
                                                                                                                ColourToStr(DefaultTCPermanentFeedbackOccupationColour)));
    TCPermanentOccupationSetByUserColour := StrToColour(FWPReadString(ColoursSectionStr, TCPermanentOccupationSetByUserColourStr,
                                                                                                               ColourToStr(DefaultTCPermanentOccupationSetByUserColour)));
    TCPermanentSystemOccupationColour := StrToColour(FWPReadString(ColoursSectionStr, TCPermanentSystemOccupationColourStr,
                                                                                                                  ColourToStr(DefaultTCPermanentSystemOccupationColour)));
    TCSpeedRestrictionColour := StrToColour(FWPReadString(ColoursSectionStr, TCSpeedRestrictionColourStr, ColourToStr(DefaultTCSpeedRestrictionColour)));
    TCSystemOccupationColour := StrToColour(FWPReadString(ColoursSectionStr, TCSystemOccupationColourStr, ColourToStr(DefaultTCSystemOccupationColour)));
    TCUnoccupiedColour := StrToColour(FWPReadString(ColoursSectionStr, TCUnoccupiedColourStr, ColourToStr(DefaultTCUnoccupiedColour)));
    TCUserMustDriveColour := StrToColour(FWPReadString(ColoursSectionStr, TCUserMustDriveColourStr, ColourToStr(DefaultTCUserMustDriveColour)));
    TrainActiveColour := StrToColour(FWPReadString(ColoursSectionStr, TrainActiveColourStr, ColourToStr(DefaultTrainActiveColour)));
    TrainInactiveColour := StrToColour(FWPReadString(ColoursSectionStr, TrainInactiveColourStr, ColourToStr(DefaultTrainInactiveColour)));

    { Miscellaneous colours }
    BackgroundColour := StrToColour(FWPReadString(ColoursSectionStr, BackgroundColourStr, ColourToStr(DefaultBackgroundColour)));
    ForegroundColour := StrToColour(FWPReadString(ColoursSectionStr, ForegroundColourStr, ColourToStr(DefaultForegroundColour)));
    DiagramsWindowGridBackgroundColour := StrToColour(FWPReadString(ColoursSectionStr, DiagramsWindowGridBackgroundColourStr,
                                                                                                                 ColourToStr(DefaultDiagramsWindowGridBackgroundColour)));
    LinesWithoutTrackCircuitsColour := StrToColour(FWPReadString(ColoursSectionStr, LinesWithoutTrackCircuitsColourStr,
                                                                                                                    ColourToStr(DefaultLinesWithoutTrackCircuitsColour)));
    LocoStalledColour := StrToColour(FWPReadString(ColoursSectionStr, LocoStalledColourStr, ColourToStr(DefaultLocoStalledColour)));
    ScreenComponentEditedColour1 := StrToColour(FWPReadString(ColoursSectionStr, ScreenComponentEditedColour1Str, ColourToStr(DefaultScreenComponentEditedColour1)));
    ScreenComponentEditedColour2 := StrToColour(FWPReadString(ColoursSectionStr, ScreenComponentEditedColour2Str, ColourToStr(DefaultScreenComponentEditedColour2)));
    WorkingTimetableWindowGridBackgroundColour := StrToColour(FWPReadString(ColoursSectionStr, WorkingTimetableWindowGridBackgroundColourStr,
                                                                                                         ColourToStr(DefaultWorkingTimetableWindowGridBackgroundColour)));
    { Pen styles }
    FiddleyardLinePenStyle := StrToPenStyle(FWPReadString(PenStylesSectionStr, FiddleyardLinePenStyleStr, PenStyleToStr(DefaultFiddleyardLinePenStyle)));
    ProjectedLinePenStyle := StrToPenStyle(FWPReadString(PenStylesSectionStr, ProjectedLinePenStyleStr, PenStyleToStr(DefaultProjectedLinePenStyle)));
    SidingPenStyle := StrToPenStyle(FWPReadString(PenStylesSectionStr, SidingPenStyleStr, PenStyleToStr(DefaultSidingPenStyle)));
    TCLocoOutOfPlaceOccupationPenStyle := StrToPenStyle(FWPReadString(PenStylesSectionStr, TCLocoOutOfPlaceOccupationPenStyleStr,
                                                                                                               PenStyleToStr(DefaultTCLocoOutOfPlaceOccupationPenStyle)));
    TCOutOfUseAsNoFeedbackReceivedPenStyle := StrToPenStyle(FWPReadString(PenStylesSectionStr, TCOutOfUseAsNoFeedbackReceivedPenStyleStr,
                                                                                                           PenStyleToStr(DefaultTCOutOfUseAsNoFeedbackReceivedPenStyle)));
    TCOutOfUseSetByUserPenStyle := StrToPenStyle(FWPReadString(PenStylesSectionStr, TCOutOfUseSetByUserPenStyleStr, PenStyleToStr(DefaultTCOutOfUseSetByUserPenStyle)));
    TCPermanentFeedbackOccupationPenStyle := StrToPenStyle(FWPReadString(PenStylesSectionStr, TCPermanentFeedbackOccupationPenStyleStr,
                                                                                                            PenStyleToStr(DefaultTCPermanentFeedbackOccupationPenStyle)));
    TCPermanentOccupationSetByUserPenStyle := StrToPenStyle(FWPReadString(PenStylesSectionStr, TCPermanentOccupationSetByUserPenStyleStr,
                                                                                                           PenStyleToStr(DefaultTCPermanentOccupationSetByUserPenStyle)));
    TCPermanentSystemOccupationPenStyle := StrToPenStyle(FWPReadString(PenStylesSectionStr, TCPermanentSystemOccupationPenStyleStr,
                                                                                                              PenStyleToStr(DefaultTCPermanentSystemOccupationPenStyle)));
    { Fonts }
    RailFontName := FWPReadString(FontsSectionStr, RailFontNameStr, DefaultRailFontName);
    LineFontHeight := FWPReadInteger(FontsSectionStr, LineFontHeightStr, DefaultLineFontHeight);
    LoggingWindowFontName := FWPReadString(FontsSectionStr, LoggingWindowFontNameStr, DefaultLoggingWindowFontName);
    LoggingWindowFontSize := FWPReadInteger(FontsSectionStr, LoggingWindowFontSizeStr, DefaultLoggingWindowFontSize);
    FWPRailWindowFontHeight := FWPReadInteger(FontsSectionStr, FWPRailWindowFontHeightStr, DefaultFWPRailWindowFontHeight);
    PlatformNumberFontHeight := FWPReadInteger(FontsSectionStr, PlatformNumberFontHeightStr, DefaultPlatformNumberFontHeight);
    StationMonitorsFontName := FWPReadString(FontsSectionStr, StationMonitorsFontNameStr, DefaultStationMonitorsFontName);
    StationMonitorsLargeFontHeight := FWPReadInteger(FontsSectionStr, StationMonitorsLargeFontHeightStr, DefaultStationMonitorsLargeFontHeight);
    StationMonitorsSmallFontHeight := FWPReadInteger(FontsSectionStr, StationMonitorsSmallFontHeightStr, DefaultStationMonitorsSmallFontHeight);
    TheatreFontHeight := FWPReadInteger(FontsSectionStr, TheatreFontHeightStr, DefaultTheatreFontHeight);

    { Dialogue box variables }
    DebuggingOptionsWindowLeft := FWPReadInteger(DialogueBoxSectionStr, DebuggingOptionsWindowLeftStr, DefaultDebuggingOptionsWindowLeft);
    DebuggingOptionsWindowTop := FWPReadInteger(DialogueBoxSectionStr, DebuggingOptionsWindowTopStr, DefaultDebuggingOptionsWindowTop);
    LineDialogueBoxLeft := FWPReadInteger(DialogueBoxSectionStr, LineDialogueBoxLeftStr, DefaultLineDialogueBoxLeft);
    LineDialogueBoxTop := FWPReadInteger(DialogueBoxSectionStr, LineDialogueBoxTopStr, DefaultLineDialogueBoxTop);
    LocoDialogueWindowLeft := FWPReadInteger(DialogueBoxSectionStr, LocoDialogueWindowLeftStr, DefaultLocoDialogueWindowLeft);
    LocoDialogueWindowTop := FWPReadInteger(DialogueBoxSectionStr, LocoDialogueWindowTopStr, DefaultLocoDialogueWindowTop);
    PointDialogueBoxLeft := FWPReadInteger(DialogueBoxSectionStr, PointDialogueBoxLeftStr, DefaultPointDialogueBoxLeft);
    PointDialogueBoxTop := FWPReadInteger(DialogueBoxSectionStr, PointDialogueBoxTopStr, DefaultPointDialogueBoxTop);
    SignalDialogueBoxLeft := FWPReadInteger(DialogueBoxSectionStr, SignalDialogueBoxLeftStr, DefaultSignalDialogueBoxLeft);
    SignalDialogueBoxTop := FWPReadInteger(DialogueBoxSectionStr, SignalDialogueBoxTopStr, DefaultSignalDialogueBoxTop);
    TrackCircuitDialogueBoxLeft := FWPReadInteger(DialogueBoxSectionStr, TrackCircuitDialogueBoxLeftStr, DefaultTrackCircuitDialogueBoxLeft);
    TrackCircuitDialogueBoxTop := FWPReadInteger(DialogueBoxSectionStr, TrackCircuitDialogueBoxTopStr, DefaultTrackCircuitDialogueBoxTop);

    LocoDialogueSpeedInMPH := FWPReadBool(DialogueBoxSectionStr, LocoDialogueSpeedInMPHStr, DefaultLocoDialogueSpeedInMPH);

    { Screen settings }
    LogFileMaxWidthInChars := FWPReadInteger(ScreenOptionsStr, LogFileMaxWidthInCharsStr, DefaultLogFileMaxWidthInChars);

    { Windows }
    TempStr := FWPReadString(ScreenOptionsStr, ScreenModeStr, '');
    IF TempStr <> '' THEN BEGIN
      IF TempStr = FullScreenStr THEN
        ScreenMode := FullScreenMode
      ELSE
        IF TempStr = FullScreenWithStatusBarStr THEN
          ScreenMode := FullScreenWithStatusBarMode
        ELSE
          IF TempStr = CustomWindowedScreenStr THEN BEGIN
            ScreenMode := CustomWindowedScreenMode;
          END;
    END;

    FWPRailWindowTop := FWPReadInteger(WindowsSectionStr, FWPRailWindowTopStr, DefaultFWPRailWindowTop);
    FWPRailWindowLeft := FWPReadInteger(WindowsSectionStr, FWPRailWindowLeftStr, DefaultFWPRailWindowLeft);
    FWPRailWindowWidth := FWPReadInteger(WindowsSectionStr, FWPRailWindowWidthStr, DefaultFWPRailWindowWidth);
    FWPRailWindowHeight := FWPReadInteger(WindowsSectionStr, FWPRailWindowHeightStr, DefaultFWPRailWindowHeight);

    DebugWindowTop := FWPReadInteger(WindowsSectionStr, DebugWindowTopStr, DefaultDebugWindowTop);
    DebugWindowLeft := FWPReadInteger(WindowsSectionStr, DebugWindowLeftStr, DefaultDebugWindowLeft);
    DebugWindowWidth := FWPReadInteger(WindowsSectionStr, DebugWindowWidthStr, DefaultDebugWindowWidth);
    DebugWindowHeight := FWPReadInteger(WindowsSectionStr, DebugWindowHeightStr, DefaultDebugWindowHeight);

    DiagramsWindowTop := FWPReadInteger(WindowsSectionStr, DiagramsWindowTopStr, DefaultDiagramsWindowTop);
    DiagramsWindowLeft := FWPReadInteger(WindowsSectionStr, DiagramsWindowLeftStr, DefaultDiagramsWindowLeft);
    DiagramsSmallWindowWidth := FWPReadInteger(WindowsSectionStr, DiagramsSmallWindowWidthStr, DefaultDiagramsSmallWindowWidth);
    DiagramsLargeWindowWidth := FWPReadInteger(WindowsSectionStr, DiagramsLargeWindowWidthStr, DefaultDiagramsLargeWindowWidth);
    DiagramsWindowHeight := FWPReadInteger(WindowsSectionStr, DiagramsWindowHeightStr, DefaultDiagramsWindowHeight);

    EditWindowTop := FWPReadInteger(WindowsSectionStr, EditWindowTopStr, DefaultEditWindowTop);
    EditWindowLeft := FWPReadInteger(WindowsSectionStr, EditWindowLeftStr, DefaultEditWindowLeft);
    EditWindowWidth := FWPReadInteger(WindowsSectionStr, EditWindowWidthStr, DefaultEditWindowWidth);
    EditWindowHeight := FWPReadInteger(WindowsSectionStr, EditWindowHeightStr, DefaultEditWindowHeight);

    LockListWindowTop := FWPReadInteger(WindowsSectionStr, LockListWindowTopStr, DefaultLockListWindowTop);
    LockListWindowLeft := FWPReadInteger(WindowsSectionStr, LockListWindowLeftStr, DefaultLockListWindowLeft);
    LockListWindowWidth := FWPReadInteger(WindowsSectionStr, LockListWindowWidthStr, DefaultLockListWindowWidth);
    LockListWindowHeight := FWPReadInteger(WindowsSectionStr, LockListWindowHeightStr, DefaultLockListWindowHeight);

    LocoUtilsWindowTop := FWPReadInteger(WindowsSectionStr, LocoUtilsWindowTopStr, DefaultLocoUtilsWindowTop);
    LocoUtilsWindowLeft := FWPReadInteger(WindowsSectionStr, LocoUtilsWindowLeftStr, DefaultLocoUtilsWindowLeft);
    LocoUtilsWindowWidth := FWPReadInteger(WindowsSectionStr, LocoUtilsWindowWidthStr, DefaultLocoUtilsWindowWidth);
    LocoUtilsWindowHeight := FWPReadInteger(WindowsSectionStr, LocoUtilsWindowHeightStr, DefaultLocoUtilsWindowHeight);

    LoggingWindowTop := FWPReadInteger(WindowsSectionStr, LoggingWindowTopStr, DefaultLoggingWindowTop);
    LoggingWindowLeft := FWPReadInteger(WindowsSectionStr, LoggingWindowLeftStr, DefaultLoggingWindowLeft);
    LoggingWindowWidth := FWPReadInteger(WindowsSectionStr, LoggingWindowWidthStr, DefaultLoggingWindowWidth);
    LoggingWindowHeight := FWPReadInteger(WindowsSectionStr, LoggingWindowHeightStr, DefaultLoggingWindowHeight);

    MovementWindowTop := FWPReadInteger(WindowsSectionStr, MovementWindowTopStr, DefaultMovementWindowTop);
    MovementWindowLeft := FWPReadInteger(WindowsSectionStr, MovementWindowLeftStr, DefaultMovementWindowLeft);
    MovementWindowWidth := FWPReadInteger(WindowsSectionStr, MovementWindowWidthStr, DefaultMovementWindowWidth);
    MovementWindowHeight := FWPReadInteger(WindowsSectionStr, MovementWindowHeightStr, DefaultMovementWindowHeight);

    OptionsWindowTop := FWPReadInteger(WindowsSectionStr, OptionsWindowTopStr, DefaultOptionsWindowTop);
    OptionsWindowLeft := FWPReadInteger(WindowsSectionStr, OptionsWindowLeftStr, DefaultOptionsWindowLeft);
    OptionsWindowWidth := FWPReadInteger(WindowsSectionStr, OptionsWindowWidthStr, DefaultOptionsWindowWidth);
    OptionsWindowHeight := FWPReadInteger(WindowsSectionStr, OptionsWindowHeightStr, DefaultOptionsWindowHeight);
    OptionsWindowValueListEditorCol0Width := FWPReadInteger(WindowsSectionStr, OptionsWindowValueListEditorCol0WidthStr, DefaultOptionsWindowValueListEditorCol0Width);

    WorkingTimetableWindowTop := FWPReadInteger(WindowsSectionStr, WorkingTimetableWindowTopStr, DefaultWorkingTimetableWindowTop);
    WorkingTimetableWindowLeft := FWPReadInteger(WindowsSectionStr, WorkingTimetableWindowLeftStr, DefaultWorkingTimetableWindowLeft);
    WorkingTimetableWindowHeight := FWPReadInteger(WindowsSectionStr, WorkingTimetableWindowHeightStr, DefaultWorkingTimetableWindowHeight);
    WorkingTimetableSmallWindowWidth := FWPReadInteger(WindowsSectionStr, WorkingTimetableSmallWindowWidthStr, DefaultWorkingTimetableSmallWindowWidth);
    WorkingTimetableLargeWindowWidth := FWPReadInteger(WindowsSectionStr, WorkingTimetableLargeWindowWidthStr, DefaultWorkingTimetableLargeWindowWidth);
    WorkingTimetableWindowHeight := FWPReadInteger(WindowsSectionStr, WorkingTimetableWindowHeightStr, DefaultWorkingTimetableWindowHeight);

    { Spacing Options }
    BufferStopVerticalSpacing := FWPReadInteger(ScreenOptionsStr, BufferStopVerticalSpacingStr, DefaultBufferStopVerticalSpacing);
    DeltaPointX := FWPReadInteger(ScreenOptionsStr, DeltaPointXStr, DefaultDeltaPointX);
    IndicatorHorizontalSpacing := FWPReadInteger(ScreenOptionsStr, IndicatorHorizontalSpacingStr, DefaultIndicatorHorizontalSpacing);
    IndicatorVerticalSpacing := FWPReadInteger(ScreenOptionsStr, IndicatorVerticalSpacingStr, DefaultIndicatorVerticalSpacing);
    MouseRectangleEdgeVerticalSpacing := FWPReadInteger(ScreenOptionsStr, MouseRectangleEdgeVerticalSpacingStr, DefaultMouseRectangleEdgeVerticalSpacing);
    PlatformEdgeVerticalSpacing := FWPReadInteger(ScreenOptionsStr, PlatformEdgeVerticalSpacingStr, DefaultPlatformEdgeVerticalSpacing);
    PlatformNumberEdgeHorizontalSpacing := FWPReadInteger(ScreenOptionsStr, PlatformNumberEdgeHorizontalSpacingStr, DefaultPlatformNumberEdgeHorizontalSpacing);
    PlatformNumberEdgeVerticalSpacing := FWPReadInteger(ScreenOptionsStr, PlatformNumberEdgeVerticalSpacingStr, DefaultPlatformNumberEdgeVerticalSpacing);
    SignalHorizontalSpacing := FWPReadInteger(ScreenOptionsStr, SignalHorizontalSpacingStr, DefaultSignalHorizontalSpacing);
    SignalRadius := FWPReadInteger(ScreenOptionsStr, SignalRadiusStr, DefaultSignalRadius);
    SignalSemaphoreHeight := FWPReadInteger(ScreenOptionsStr, SignalSemaphoreHeightStr, DefaultSignalSemaphoreHeight);
    SignalSemaphoreWidth := FWPReadInteger(ScreenOptionsStr, SignalSemaphoreWidthStr, DefaultSignalSemaphoreWidth);
    SignalVerticalSpacing := FWPReadInteger(ScreenOptionsStr, SignalVerticalSpacingStr, DefaultSignalVerticalSpacing);
    SpeedRestrictionHorizontalSpacing := FWPReadInteger(ScreenOptionsStr, SpeedRestrictionHorizontalSpacingStr, DefaultSpeedRestrictionHorizontalSpacing);
    SpeedRestrictionVerticalSpacing := FWPReadInteger(ScreenOptionsStr, SpeedRestrictionVerticalSpacingStr, DefaultSpeedRestrictionVerticalSpacing);
    TheatreIndicatorHorizontalSpacing := FWPReadInteger(ScreenOptionsStr, TheatreIndicatorHorizontalSpacingStr, DefaultTheatreIndicatorHorizontalSpacing);
    TheatreIndicatorVerticalSpacing := FWPReadInteger(ScreenOptionsStr, TheatreIndicatorVerticalSpacingStr, DefaultTheatreIndicatorVerticalSpacing);
    TRSPlungerLength := FWPReadInteger(ScreenOptionsStr, TRSPlungerLengthStr, DefaultTRSPlungerLength);
    WindowRows := FWPReadInteger(ScreenOptionsStr, WindowRowsStr, DefaultWindowRows);

    { Other Options }
    AcceptAllPermanentOccupationsWithoutFeedback := FWPReadBool(OtherOptionsSectionStr, AcceptAllPermanentOccupationsWithoutFeedbackStr,
                                                                                                                     DefaultAcceptAllPermanentOccupationsWithoutFeedback);
    AutomaticallySetFocusWhenInDebugWindow := FWPReadBool(OtherOptionsSectionStr, AutomaticallySetFocusWhenInDebugWindowStr, DefaultAutomaticallySetFocusWhenInDebugWindow);
    CancelAllTrainsWithNoFeedbackOccupation := FWPReadBool(OtherOptionsSectionStr, CancelAllTrainsWithNoFeedbackOccupationStr,
                                                                                                                          DefaultCancelAllTrainsWithNoFeedbackOccupation);
    CarriageLengthInInches := FWPReadInteger(OtherOptionsSectionStr, CarriageLengthInInchesStr, DefaultCarriageLengthInInches);
    CheckForIdenticalLinesInLog := FWPReadBool(OtherOptionsSectionStr, CheckForIdenticalLinesInLogStr, DefaultCheckForIdenticalLinesInLog);
    DisplayDiagrams := FWPReadBool(OtherOptionsSectionStr, DisplayDiagramsStr, DefaultDisplayDiagrams);
    DisplayFlashingTrackCircuits := FWPReadBool(OtherOptionsSectionStr, DisplayFlashingTrackCircuitsStr, DefaultDisplayFlashingTrackCircuits);
    DisplayLocoChipNums := FWPReadBool(OtherOptionsSectionStr, DisplayLocoChipNumsStr, DefaultDisplayLocoChipNums);
    DisplayLocoHeadcodes := FWPReadBool(OtherOptionsSectionStr, DisplayLocoHeadcodesStr, DefaultDisplayLocoHeadcodes);
    DisplayNotForPublicUseTrainsInStationMonitors := FWPReadBool(OtherOptionsSectionStr, DisplayNotForPublicUseTrainsInStationMonitorsStr,
                                                                                                                    DefaultDisplayNotForPublicUseTrainsInStationMonitors);
    DisplayRoutesAndJourneys := FWPReadBool(OtherOptionsSectionStr, DisplayRoutesAndJourneysStr, DefaultDisplayRoutesAndJourneys);
    DoNotCancelTrainsWithNoFeedbackOccupation := FWPReadBool(OtherOptionsSectionStr, DoNotCancelTrainsWithNoFeedbackOccupationStr,
                                                                                                                        DefaultDoNotCancelTrainsWithNoFeedbackOccupation);
    HighlightTrackCircuitSpeedRestrictions := FWPReadBool(OtherOptionsSectionStr, HighlightTrackCircuitSpeedRestrictionsStr, DefaultHighlightTrackCircuitSpeedRestrictions);
    LargeDiagramsWindowSelected := FWPReadBool(OtherOptionsSectionStr, LargeDiagramsWindowSelectedStr, DefaultLargeDiagramsWindowSelected);
    LargeWorkingTimetableWindowSelected := FWPReadBool(OtherOptionsSectionStr, LargeWorkingTimetableWindowSelectedStr, DefaultLargeWorkingTimetableWindowSelected);
    LineThicknessInFullScreenMode := FWPReadString(OtherOptionsSectionStr, LineThicknessInFullScreenModeStr, DefaultLineThicknessInFullScreenMode);
    LocoTimingTimeBeforeAutoStopInSeconds := FWPReadInteger(OtherOptionsSectionStr, LocoTimingTimeBeforeAutoStopInSecondsStr, DefaultLocoTimingTimeBeforeAutoStopInSeconds);
    LogCurrentTimeMode := FWPReadBool(OtherOptionsSectionStr, LogCurrentTimeModeStr, DefaultLogCurrentTimeMode);
    LogsKeptMode := FWPReadBool(OtherOptionsSectionStr, LogsKeptModeStr, DefaultLogsKeptMode);
//    LogsCurrentlyKept := LogsKeptMode; { this allows LogsKeptMode to be changed without affecting the current logging }
    MakeSoundWhenDebugWindowBoldTextAppears := FWPReadBool(OtherOptionsSectionStr, MakeSoundWhenDebugWindowBoldTextAppearsStr,
                                                                                                                          DefaultMakeSoundWhenDebugWindowBoldTextAppears);
    MaxRectangleUndrawTime := FWPReadInteger(OtherOptionsSectionStr, MaxRectangleUndrawTimeStr, DefaultMaxRectangleUndrawTime);
    MenusVisible := FWPReadBool(OtherOptionsSectionStr, MenusVisibleStr, DefaultMenusVisible);
    MonitorStrayingTrains := FWPReadBool(OtherOptionsSectionStr, MonitorStrayingTrainsStr, DefaultMonitorStrayingTrains);
    PointFeedbackMaximumWaitInSeconds := FWPReadInteger(OtherOptionsSectionStr, PointFeedbackMaximumWaitInSecondsStr, DefaultPointFeedbackMaximumWaitInSeconds);
    RouteAheadNotClearWaitTimeInMinutes := FWPReadInteger(OtherOptionssectionStr, RouteAheadNotClearWaitTimeInMinutesStr, DefaultRouteAheadNotClearWaitTimeInMinutes);
    RunTestUnitOnStartup := FWPReadBool(OtherOptionsSectionStr, RunTestUnitOnStartupStr, DefaultRunTestUnitOnStartup);
    ShowCancelledTrainsInDiagrams := FWPReadBool(OtherOptionsSectionStr, ShowCancelledTrainsInDiagramsStr, DefaultShowCancelledTrainsInDiagrams);
    ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable := FWPReadBool(OtherOptionsSectionStr, ShowIncorrectDayOfTheWeekEntriesInWorkingTimetableStr,
                                                                                                               DefaultShowIncorrectDayOfTheWeekEntriesInWorkingTimetable);
    ShowNonMovingTrainsInDiagrams := FWPReadBool(OtherOptionsSectionStr, ShowNonMovingTrainsInDiagramsStr, DefaultShowNonMovingTrainsInDiagrams);
    ShowNonStopsInDiagrams := FWPReadBool(OtherOptionsSectionStr, ShowNonStopsInDiagramsStr, DefaultShowNonStopsInDiagrams);
    ShowTrackCircuitsWhereUserMustDrive := FWPReadBool(OtherOptionsSectionStr, ShowTrackCircuitsWhereUserMustDriveStr, DefaultShowTrackCircuitsWhereUserMustDrive);
    StartRepeatJourneysOnNewLineInDiagrams := FWPReadBool(OtherOptionsSectionStr, StartRepeatJourneysOnNewLineInDiagramsStr, DefaultStartRepeatJourneysOnNewLineInDiagrams);
    StartWithDiagrams := FWPReadBool(OtherOptionsSectionStr, StartWithDiagramsStr, DefaultStartWithDiagrams);
    StationEndOfDayPassengerLeavingTimeInMinutes := FWPReadInteger(OtherOptionsSectionStr, StationEndOfDayPassengerLeavingTimeInMinutesStr,
                                                                                                                     DefaultStationEndOfDayPassengerLeavingTimeInMinutes);
    StationMonitorsWebPageRequired := FWPReadBool(OtherOptionsSectionStr, StationMonitorsWebPageRequiredStr, DefaultStationMonitorsWebPageRequired);
    StationOppositeDirectionExitMinimumWaitTimeInMinutes := FWPReadInteger(OtherOptionssectionStr, StationOppositeDirectionExitMinimumWaitTimeInMinutesStr,
                                                                                                             DefaultStationOppositeDirectionExitMinimumWaitTimeInMinutes);
    StationSameDirectionExitMinimumWaitTimeInMinutes := FWPReadInteger(OtherOptionsSectionStr, StationSameDirectionExitMinimumWaitTimeInMinutesStr,
                                                                                                                 DefaultStationSameDirectionExitMinimumWaitTimeInMinutes);
    SetMode(StationStart, FWPReadBool(OtherOptionsSectionStr, StationStartModeStr, DefaultStationStartMode));
    StationStartOfDayPassengerBoardingTimeInMinutes := FWPReadInteger(OtherOptionssectionStr, StationStartOfDayPassengerBoardingTimeInMinutesStr,
                                                                                                                  DefaultStationStartOfDayPassengerBoardingTimeInMinutes);
    StopAllLocosAtShutDown := FWPReadBool(OtherOptionsSectionStr, StopAllLocosAtShutDownStr, DefaultStopAllLocosAtShutDown);
    SwitchActiveLocoLightsOffAtShutDown := FWPReadBool(OtherOptionsSectionStr, SwitchActiveLocoLightsOffAtShutDownStr, DefaultSwitchActiveLocoLightsOffAtShutDown);
    TheatreBoxHeight := FWPReadInteger(OtherOptionsSectionStr, TheatreBoxHeightStr, DefaultTheatreBoxHeight);
    TheatreBoxWidth := FWPReadInteger(OtherOptionsSectionStr, TheatreBoxWidthStr, DefaultTheatreBoxWidth);
    UseDisplayLineColoursWindowSettingsOnStartup := FWPReadBool(OtherOptionsSectionStr, UseDisplayLineColoursWindowSettingsOnStartupStr,
                                                                                                                     DefaultUseDisplayLineColoursWindowSettingsOnStartup);
    IF UseDisplayLineColoursWindowSettingsOnStartup THEN BEGIN
      DisplayLineColoursWindowTop := FWPReadInteger(WindowsSectionStr, DisplayLineColoursWindowTopStr, DefaultDisplayLineColoursWindowTop);
      DisplayLineColoursWindowLeft := FWPReadInteger(WindowsSectionStr, DisplayLineColoursWindowLeftStr, DefaultDisplayLineColoursWindowLeft);
      DisplayLineColoursWindowWidth := FWPReadInteger(WindowsSectionStr, DisplayLineColoursWindowWidthStr, DefaultDisplayLineColoursWindowWidth);
      DisplayLineColoursWindowHeight := FWPReadInteger(WindowsSectionStr, DisplayLineColoursWindowHeightStr, DefaultDisplayLineColoursWindowHeight);
    END;

    WaitBeforeRerouteInMinutes := FWPReadInteger(OtherOptionsSectionStr, WaitBeforeRerouteInMinutesStr, DefaultWaitBeforeRerouteInMinutes);
    WorkingTimetableMode := FWPReadBool(OtherOptionsSectionStr, WorkingTimetableModeStr, DefaultWorkingTimetableMode);

    { Time options }
    TempStr := FWPReadString(TimesSectionStr, ProgramStartTimeOptionStr, DefaultProgramStartTimeStr);
    IF (TempStr <> '') AND (TimeIsValid(TempStr)) THEN BEGIN
      ProgramStartTime := StrToTime(TempStr);
      Log('A Program Start Time set to ' + TempStr);
    END;

    TempStr := FWPReadString(TimesSectionStr, DayLightStartTimeOptionStr, DefaultDayLightStartTimeStr);
    IF (TempStr <> '') AND (TimeIsValid(TempStr)) THEN
      DayLightStartTime := StrToTime(TempStr);

    TempStr := FWPReadString(TimesSectionStr, DayLightEndTimeOptionStr, DefaultDayLightEndTimeStr);
    IF (TempStr <> '') AND (TimeIsValid(TempStr)) THEN
      DayLightEndTime := StrToTime(TempStr);

    TempStr := FWPReadString(TimesSectionStr, CurrentRailwayDayOfTheWeekStr, DefaultCurrentRailwayDayOfTheWeek);
    CurrentRailwayDayOfTheWeek := StrToDayOfTheWeek(TempStr);
    SetCurrentRailwayTimeAndDayOfTheWeek(ProgramStartTime);

    { RailDriver Data }
    RDCBailOffMin := FWPReadInteger(RDCSectionStr, RDCBailOffMinStr, 0);
    RDCBailOffMax := FWPReadInteger(RDCSectionStr, RDCBailOffMaxStr, 0);
    RDCEmergencyBrakeMin := FWPReadInteger(RDCSectionStr, RDCEmergencyBrakeMinStr, 0);
    RDCEmergencyBrakeMax := FWPReadInteger(RDCSectionStr, RDCEmergencyBrakeMaxStr, 0);
    RDCLocoBrakeMin := FWPReadInteger(RDCSectionStr, RDCLocoBrakeMinStr, 0);
    RDCLocoBrakeMax := FWPReadInteger(RDCSectionStr, RDCLocoBrakeMaxStr, 0);
    RDCRegulatorForwardMin := FWPReadInteger(RDCSectionStr, RDCRegulatorForwardMinStr, 0);
    RDCRegulatorForwardMax := FWPReadInteger(RDCSectionStr, RDCRegulatorForwardMaxStr, 0);
    RDCRegulatorReverseMin := FWPReadInteger(RDCSectionStr, RDCRegulatorReverseMinStr, 0);
    RDCRegulatorReverseMax := FWPReadInteger(RDCSectionStr, RDCRegulatorReverseMaxStr, 0);
    RDCReverserForwardMin := FWPReadInteger(RDCSectionStr, RDCReverserForwardMinStr, 0);
    RDCReverserForwardMax := FWPReadInteger(RDCSectionStr, RDCReverserForwardMaxStr, 0);
    RDCReverserReverseMin := FWPReadInteger(RDCSectionStr, RDCReverserReverseMinStr, 0);
    RDCReverserReverseMax := FWPReadInteger(RDCSectionStr, RDCReverserReverseMaxStr, 0);
    RDCThreeWaySwitchALeftNum := FWPReadInteger(RDCSectionStr, RDCThreeWaySwitchALeftNumStr, 0);
    RDCThreeWaySwitchAMidNum := FWPReadInteger(RDCSectionStr, RDCThreeWaySwitchAMidNumStr, 0);
    RDCThreeWaySwitchARightNum := FWPReadInteger(RDCSectionStr, RDCThreeWaySwitchARightNumStr, 0);
    RDCThreeWaySwitchBLeftNum := FWPReadInteger(RDCSectionStr, RDCThreeWaySwitchBLeftNumStr, 0);
    RDCThreeWaySwitchBMidNum := FWPReadInteger(RDCSectionStr, RDCThreeWaySwitchBMidNumStr, 0);
    RDCThreeWaySwitchBRightNum := FWPReadInteger(RDCSectionStr, RDCThreeWaySwitchBRightNumStr, 0);
    RDCTrainBrakeMin := FWPReadInteger(RDCSectionStr, RDCTrainBrakeMinStr, 0);
    RDCTrainBrakeMax := FWPReadInteger(RDCSectionStr, RDCTrainBrakeMaxStr, 0);

    { Other Miscellaneous Data }
    CurrentParametersFromIniFile := FWPReadString(MiscellaneousDataSectionStr, CurrentParametersStr, '');

    { The inifile is freed not here but after the track circuits are read in }
  EXCEPT
    ON E : Exception DO
      Log('EG ReadIniFile: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadIniFileMainProcedure }

PROCEDURE ReadIniFileForTrackCircuitData;
{ Read in track-circuit data from the .ini file or from the Registry }
CONST
  Delimiter = ';';

VAR
  DelimiterPos : Integer;
  IniFile : TIniFile;
  RegistryIniFile : TRegistryIniFile;
  TempStr : String;
  TempStr2 : String;
  TC : Integer;

  FUNCTION FWPReadString(SectionStr, DataStr, DefaultDataStr : String) : String;
  BEGIN
    IF ReadFromRegistry THEN
      Result := RegistryIniFile.ReadString(SectionStr, DataStr, DefaultDataStr)
    ELSE
      Result := IniFile.ReadString(SectionStr, DataStr, DefaultDataStr);
  END; { FWPReadString }

BEGIN
  TRY
    IF ReadFromRegistry THEN
      RegistryIniFile := TRegistryIniFile.Create('FWPRail')
    ELSE
      IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Rail.ini');

    { Now track-circuit info. (Can't use SetTrackCircuitState here as it writes to the log file which is not yet open, as the log file name is held in the .ini file). }
    FOR TC := 0 TO High(TrackCircuits) DO BEGIN
      TempStr := FWPReadString(TrackCircuitsSectionStr, IntToStr(TC), '');
      REPEAT
        DelimiterPos := Pos(';', TempStr);
        TempStr2 := Copy(TempStr, 1, DelimiterPos - 1);
        IF TempStr2 = TrackCircuitOutOfUseSetByUserStr THEN
          TrackCircuits[TC].TC_OccupationState := TCOutOfUseSetByUser
        ELSE
          IF TempStr2 = TrackCircuitOutOfUseAsNoFeedbackReceivedStr THEN
            TrackCircuits[TC].TC_OccupationState := TCOutOfUseAsNoFeedbackReceived
          ELSE
            IF TempStr2 = TrackCircuitPermanentOccupationSetByUserStr THEN
              TrackCircuits[TC].TC_OccupationState := TCPermanentOccupationSetByUser
            ELSE
              IF Pos(TrackCircuitSpeedRestrictionStr, TempStr2) > 0 THEN BEGIN
                TempStr2 := Copy(TempStr, Pos('MPH=', TempStr));
                TrackCircuits[TC].TC_SpeedRestrictionInMPH := IntToMPH(StrToInt(Copy(TempStr2, 5, Pos(' ', TempStr2) - 5)));
                { and its direction }
                TrackCircuits[TC].TC_SpeedRestrictionDirection := StrToDirectionType(Copy(TempStr2, Pos('Dir=', TempStr2) + 4, 1));
              END ELSE
                IF TempStr2 = TrackCircuitUserMustDriveStr THEN
                  TrackCircuits[TC].TC_UserMustDrive := True;

        TempStr := Copy(TempStr, DelimiterPos + 1);
      UNTIL (TempStr = '') OR (DelimiterPos = 0);
    END; {FOR}

    IF ReadFromRegistry THEN
      RegistryIniFile.Free
    ELSE
      IniFile.Free;
  EXCEPT
    ON E : Exception DO
      Log('EG ReadIniFileForTrackCircuitData: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadIniFileForTrackCircuitDataMainProcedure }

PROCEDURE WriteIniFile;
{ Write out data to the registry and to the .ini file. (Do both so that the .ini file can be archived but not normally used when options are read in). Special routines are
  needed to do the writing as the TIniFile and TRegistryIniFile are different types.
}
CONST
  StopTimer = True;

VAR
  IniFile : TIniFile;
  RegistryIniFile : TRegistryIniFile;
  TC : Integer;
  TCString : String;

  PROCEDURE WriteStringTwice(SectionStr, Data1Str, Data2Str : String);
  BEGIN
    IniFile.WriteString(SectionStr, Data1Str, Data2Str);
    RegistryIniFile.WriteString(SectionStr, Data1Str, Data2Str);
  END; { WriteStringTwice }

  PROCEDURE WriteIntegerTwice(SectionStr, DataStr : String; DataInt : Integer);
  BEGIN
    IniFile.WriteInteger(SectionStr, DataStr, DataInt);
    RegistryIniFile.WriteInteger(SectionStr, DataStr, DataInt);
  END; { WriteIntegerTwice }

  PROCEDURE WriteBoolTwice(SectionStr, DataStr : String; DataBool : Boolean);
  BEGIN
    IniFile.WriteBool(SectionStr, DataStr, DataBool);
    RegistryIniFile.WriteBool(SectionStr, DataStr, DataBool);
  END; { WriteBoolTwice }

BEGIN
  TRY
    { The .ini file was stored where the executable is }
    RegistryIniFile := TRegistryIniFile.Create('FWPRail');
    IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Rail.ini');

    { Now track circuits that are user designated as having special properties - delete when no longer special }
    IF Length(TrackCircuits) > 0 THEN BEGIN
//        IF (TrackCircuits[183].TC_OccupationState <> TCOutOfUseSetByUser) AND (TrackCircuits[183].TC_OccupationState <> TCOutOfUseAsNoFeedbackReceived) THEN BEGIN
//          Log('X! .ini error!!! TC=183 not out of use');
//          ASM
//            Int 3
//          END; {ASM}
//        END ELSE BEGIN
        FOR TC := 0 TO High(TrackCircuits) DO BEGIN
          TCString := '';
          IF TrackCircuits[TC].TC_OccupationState = TCOutOfUseSetByUser THEN
            TCString := TCString + TrackCircuitOutOfUseSetByUserStr + ';';

          IF TrackCircuits[TC].TC_OccupationState = TCPermanentOccupationSetByUser THEN
            TCString := TCString + TrackCircuitPermanentOccupationSetByUserStr + ';';

          IF (TrackCircuits[TC].TC_SpeedRestrictionInMPH <> NoSpecifiedSpeed) AND (TrackCircuits[TC].TC_SpeedRestrictionInMPH <> MPH0) THEN
            TCString := TCString + TrackCircuitSpeedRestrictionStr
                                   + ' MPH=' + MPHToStr(TrackCircuits[TC].TC_SpeedRestrictionInMPH)
                                   + ' Dir=' + DirectionToStr(TrackCircuits[TC].TC_SpeedRestrictionDirection)
                                   + ';';

          IF TrackCircuits[TC].TC_UserMustDrive THEN
            TCString := TCString + TrackCircuitUserMustDriveStr + ';';

          IF TCString <> '' THEN
            WriteStringTwice(TrackCircuitsSectionStr, IntToStr(TC), TCString)
          ELSE BEGIN
            IF (TrackCircuits[TC].TC_OccupationState <> TCOutOfUseAsNoFeedbackReceived) AND RegistryIniFile.ValueExists(TrackCircuitsSectionStr, IntToStr(TC)) THEN
              RegistryIniFile.DeleteKey(TrackCircuitsSectionStr, IntToStr(TC));
            IF (TrackCircuits[TC].TC_OccupationState <> TCOutOfUseAsNoFeedbackReceived) AND IniFile.ValueExists(TrackCircuitsSectionStr, IntToStr(TC)) THEN
              IniFile.DeleteKey(TrackCircuitsSectionStr, IntToStr(TC));
            END;
        END; {FOR }
//        END;
    END;

    WriteStringTwice('User', 'User', GetUserFromWindows);
    WriteStringTwice('User', 'Date', DateTimeToStr(Now));
    WriteStringTwice('User', 'Computer', GetComputerNetName);
    WriteStringTwice('User', 'Version', GetVersionInfoAsString);
    WriteStringTwice('User', 'Build', GetBuildInfoAsString);

    { various file data }
    WriteStringTwice(FilesSectionStr, PathToRailDataFilesStr, PathToRailDataFiles);
    WriteStringTwice(FilesSectionStr, PathToRailSourceFilesStr, PathToRailSourceFiles);
    WriteStringTwice(FilesSectionStr, PathToLogFilesStr, PathToLogFiles);

    WriteStringTwice(FilesSectionStr, AreaDataFilenameStr, AreaDataFilename);
    WriteStringTwice(FilesSectionStr, AreaDataFilenameSuffixStr, AreaDataFilenameSuffix);
    WriteStringTwice(FilesSectionStr, DataCheckFileNameStr, DataCheckFileName);
    IF DiagramsFilename = '' THEN BEGIN
      { check that we want no diagrams the next time we start }
      IF MessageDialogueWithDefault('There are no diagrams loaded: do you wish to start the next session with diagrams or without?',
                                    StopTimer, mtError, [mbYes, mbNo], ['&With', 'With&out'], mbYes) = mrYes
      THEN
        DiagramsFilename := DefaultDiagramsFilename;
    END;
    WriteStringTwice(FilesSectionStr, DiagramsFilenameStr, DiagramsFilename);
    WriteStringTwice(FilesSectionStr, DiagramsFilenameSuffixStr, DiagramsFilenameSuffix);
    WriteStringTwice(FilesSectionStr, FeedbackDataFilenameStr, FeedbackDataFilename);
    WriteStringTwice(FilesSectionStr, FeedbackDataFilenameSuffixStr, FeedbackDataFilenameSuffix);

    WriteStringTwice(FilesSectionStr, LineDataFilenameStr, LineDataFilename);
    WriteStringTwice(FilesSectionStr, LineDataFilenameSuffixStr, LineDataFilenameSuffix);
    WriteStringTwice(FilesSectionStr, LocationDataFilenameStr, LocationDataFilename);
    WriteStringTwice(FilesSectionStr, LocationDataFilenameSuffixStr, LocationDataFilenameSuffix);
    WriteStringTwice(FilesSectionStr, LocoDataFilenameStr, LocoDataFilename);
    WriteStringTwice(FilesSectionStr, LocoDataFilenameSuffixStr, LocoDataFilenameSuffix);
    WriteStringTwice(FilesSectionStr, LogFilenameStr, LogFilename);
    WriteStringTwice(FilesSectionStr, LogFilenameSuffixStr, LogFilenameSuffix);
    WriteStringTwice(FilesSectionStr, PlatformDataFilenameStr, PlatformDataFilename);
    WriteStringTwice(FilesSectionStr, PlatformDataFilenameSuffixStr, PlatformDataFilenameSuffix);
    WriteStringTwice(FilesSectionStr, PointDataFilenameStr, PointDataFilename);
    WriteStringTwice(FilesSectionStr, PointDataFilenameSuffixStr, PointDataFilenameSuffix);
    WriteStringTwice(FilesSectionStr, ReplayFilenameStr, ReplayFilename);
    WriteStringTwice(FilesSectionStr, ReplayFilenameSuffixStr, ReplayFilenameSuffix);
    WriteStringTwice(FilesSectionStr, RouteingExceptionDataFilenameStr, RouteingExceptionDataFilename);
    WriteStringTwice(FilesSectionStr, RouteingExceptionDataFilenameSuffixStr, RouteingExceptionDataFilenameSuffix);
    WriteStringTwice(FilesSectionStr, SignalDataFilenameStr, SignalDataFilename);
    WriteStringTwice(FilesSectionStr, SignalDataFilenameSuffixStr, SignalDataFilenameSuffix);
    WriteStringTwice(FilesSectionStr, TrackCircuitDataFilenameStr, TrackCircuitDataFilename);
    WriteStringTwice(FilesSectionStr, TrackCircuitDataFilenameSuffixStr, TrackCircuitDataFilenameSuffix);
    WriteStringTwice(FilesSectionStr, WorkingTimetableFilenameStr, WorkingTimetableFilename);
    WriteStringTwice(FilesSectionStr, WorkingTimetableFilenameSuffixStr, WorkingTimetableFilenameSuffix);

    { Colours for buffer stops }
    WriteStringTwice(ColoursSectionStr, BufferStopColourStr, ColourToStr(BufferStopColour));
    WriteStringTwice(ColoursSectionStr, BufferStopNumberColourStr, ColourToStr(BufferStopNumberColour));
    WriteStringTwice(ColoursSectionStr, BufferStopRedStr, ColourToStr(BufferStopRed));

    { Colours for TRS plungers }
    WriteStringTwice(ColoursSectionStr, TRSPlungerColourStr, ColourToStr(TRSPlungerColour));
    WriteStringTwice(ColoursSectionStr, TRSPlungerOutlineColourStr, ColourToStr(TRSPlungerOutlineColour));
    WriteStringTwice(ColoursSectionStr, TRSPlungerPressedColourStr, ColourToStr(TRSPlungerPressedColour));

    { Colours for points }
    WriteStringTwice(ColoursSectionStr, PointColourStr, ColourToStr(PointColour));
    WriteStringTwice(ColoursSectionStr, PointDivergingLineColourStr, ColourToStr(PointDivergingLineColour));
    WriteStringTwice(ColoursSectionStr, PointDownFacingColourStr, ColourToStr(PointDownFacingColour));
    WriteStringTwice(ColoursSectionStr, PointFeedbackDataInUseColourStr, ColourToStr(PointFeedbackDataInUseColour));
    WriteStringTwice(ColoursSectionStr, PointFeedbackDataOutOfUseColourStr, ColourToStr(PointFeedbackDataOutOfUseColour));
    WriteStringTwice(ColoursSectionStr, PointHeelLineColourStr, ColourToStr(PointHeelLineColour));
    WriteStringTwice(ColoursSectionStr, PointLenzNumberColourStr, ColourToStr(PointLenzNumberColour));
    WriteStringTwice(ColoursSectionStr, PointLockedByUserColourStr, ColourToStr(PointLockedByUserColour));
    WriteStringTwice(ColoursSectionStr, PointManualOperationColourStr, ColourToStr(PointManualOperationColour));
    WriteStringTwice(ColoursSectionStr, PointOutOfUseColourStr, ColourToStr(PointOutOfUseColour));
    WriteStringTwice(ColoursSectionStr, PointStraightLineColourStr, ColourToStr(PointStraightLineColour));
    WriteStringTwice(ColoursSectionStr, PointsWithoutFeedbackColourStr, ColourToStr(PointsWithoutFeedbackColour));
    WriteStringTwice(ColoursSectionStr, PointUndrawColourStr, ColourToStr(PointUndrawColour));
    WriteStringTwice(ColoursSectionStr, PointUpFacingColourStr, ColourToStr(PointUpFacingColour));
    WriteStringTwice(ColoursSectionStr, ShowPointDefaultStateColourStr, ColourToStr(ShowPointDefaultStateColour));
    WriteStringTwice(ColoursSectionStr, ShowPointLockedColourStr, ColourToStr(ShowPointLockedColour));

    { Colours for signal posts }
    WriteStringTwice(ColoursSectionStr, SignalPostColourStr, ColourToStr(SignalPostColour));
    WriteStringTwice(ColoursSectionStr, SignalPostEmergencyRouteSettingColourStr, ColourToStr(SignalPostEmergencyRouteSettingColour));
    WriteStringTwice(ColoursSectionStr, SignalPostRouteSettingColourStr, ColourToStr(SignalPostRouteSettingColour));
    WriteStringTwice(ColoursSectionStr, SignalPostStationStartModeColourStr, ColourToStr(SignalPostStationStartModeColour));
    WriteStringTwice(ColoursSectionStr, SignalPostTheatreSettingColourStr, ColourToStr(SignalPostTheatreSettingColour));

    { Colours for signals }
    WriteStringTwice(ColoursSectionStr, SignalAspectGreenStr, ColourToStr(SignalAspectGreen));
    WriteStringTwice(ColoursSectionStr, SignalAspectRedStr, ColourToStr(SignalAspectRed));
    WriteStringTwice(ColoursSectionStr, SignalAspectUnlitStr, ColourToStr(SignalAspectUnlit));
    WriteStringTwice(ColoursSectionStr, SignalAspectYellowStr, ColourToStr(SignalAspectYellow));
    WriteStringTwice(ColoursSectionStr, SignalNumberColourStr, ColourToStr(SignalNumberColour));

    { Colours for lines }
    WriteStringTwice(ColoursSectionStr, LineNotAvailableColourStr, ColourToStr(LineNotAvailableColour));
    WriteStringTwice(ColoursSectionStr, LineRoutedOverColourStr, ColourToStr(LineRoutedOverColour));

    { Colours for platforms }
    WriteStringTwice(ColoursSectionStr, PlatformColourStr, ColourToStr(PlatformColour));
    WriteStringTwice(ColoursSectionStr, PlatformNumberColourStr, ColourToStr(PlatformNumberColour));

    { Colours for track circuits }
    WriteStringTwice(ColoursSectionStr, TCFeedbackDataInUseColourStr, ColourToStr(TCFeedbackDataInUseColour));
    WriteStringTwice(ColoursSectionStr, TCFeedbackDataOutOfUseColourStr, ColourToStr(TCFeedbackDataOutOfUseColour));
    WriteStringTwice(ColoursSectionStr, TCFeedbackOccupationColourStr, ColourToStr(TCFeedbackOccupationColour));
    WriteStringTwice(ColoursSectionStr, TCFeedbackOccupationButOutOfUseColourStr, ColourToStr(TCFeedbackOccupationButOutOfUseColour));
    WriteStringTwice(ColoursSectionStr, TCLocoOutOfPlaceOccupationColourStr, ColourToStr(TCLocoOutOfPlaceOccupationColour));
    WriteStringTwice(ColoursSectionStr, TCMissingOccupationColourStr, ColourToStr(TCMissingOccupationColour));
    WriteStringTwice(ColoursSectionStr, TCOutOfUseSetByUserColourStr, ColourToStr(TCOutOfUseSetByUserColour));
    WriteStringTwice(ColoursSectionStr, TCOutOfUseAsNoFeedbackReceivedColourStr, ColourToStr(TCOutOfUseAsNoFeedbackReceivedColour));
    WriteStringTwice(ColoursSectionStr, TCPermanentFeedbackOccupationColourStr, ColourToStr(TCPermanentFeedbackOccupationColour));
    WriteStringTwice(ColoursSectionStr, TCPermanentOccupationSetByUserColourStr, ColourToStr(TCPermanentOccupationSetByUserColour));
    WriteStringTwice(ColoursSectionStr, TCPermanentSystemOccupationColourStr, ColourToStr(TCPermanentSystemOccupationColour));
    WriteStringTwice(ColoursSectionStr, TCSpeedRestrictionColourStr, ColourToStr(TCSpeedRestrictionColour));
    WriteStringTwice(ColoursSectionStr, TCSystemOccupationColourStr, ColourToStr(TCSystemOccupationColour));
    WriteStringTwice(ColoursSectionStr, TCUnoccupiedColourStr, ColourToStr(TCUnoccupiedColour));
    WriteStringTwice(ColoursSectionStr, TCUserMustDriveColourStr, ColourToStr(TCUserMustDriveColour));

    { Miscellaneous colours }
    WriteStringTwice(ColoursSectionStr, BackgroundColourStr, ColourToStr(BackgroundColour));
    WriteStringTwice(ColoursSectionStr, ForegroundColourStr, ColourToStr(ForegroundColour));
    WriteStringTwice(ColoursSectionStr, DiagramsWindowGridBackgroundColourStr, ColourToStr(DiagramsWindowGridBackgroundColour));
    WriteStringTwice(ColoursSectionStr, LinesWithoutTrackCircuitsColourStr, ColourToStr(LinesWithoutTrackCircuitsColour));
    WriteStringTwice(ColoursSectionStr, LocoStalledColourStr, ColourToStr(LocoStalledColour));
    WriteStringTwice(ColoursSectionStr, ScreenComponentEditedColour1Str, ColourToStr(ScreenComponentEditedColour1));
    WriteStringTwice(ColoursSectionStr, ScreenComponentEditedColour2Str, ColourToStr(ScreenComponentEditedColour2));
    WriteStringTwice(ColoursSectionStr, TrainActiveColourStr, ColourToStr(TrainActiveColour));
    WriteStringTwice(ColoursSectionStr, TrainInactiveColourStr, ColourToStr(TrainInactiveColour));
    WriteStringTwice(ColoursSectionStr, WorkingTimetableWindowGridBackgroundColourStr, ColourToStr(WorkingTimetableWindowGridBackgroundColour));

    { Pen styles }
    WriteStringTwice(PenStylesSectionStr, FiddleyardLinePenStyleStr, PenStyleToStr(FiddleyardLinePenStyle));
    WriteStringTwice(PenStylesSectionStr, ProjectedLinePenStyleStr, PenStyleToStr(ProjectedLinePenStyle));
    WriteStringTwice(PenStylesSectionStr, SidingPenStyleStr, PenStyleToStr(SidingPenStyle));
    WriteStringTwice(PenStylesSectionStr, TCLocoOutOfPlaceOccupationPenStyleStr, PenStyleToStr(TCLocoOutOfPlaceOccupationPenStyle));
    WriteStringTwice(PenStylesSectionStr, TCOutOfUseAsNoFeedbackReceivedPenStyleStr, PenStyleToStr(TCOutOfUseAsNoFeedbackReceivedPenStyle));
    WriteStringTwice(PenStylesSectionStr, TCOutOfUseSetByUserPenStyleStr, PenStyleToStr(TCOutOfUseSetByUserPenStyle));
    WriteStringTwice(PenStylesSectionStr, TCPermanentFeedbackOccupationPenStyleStr, PenStyleToStr(TCPermanentFeedbackOccupationPenStyle));
    WriteStringTwice(PenStylesSectionStr, TCPermanentOccupationSetByUserPenStyleStr, PenStyleToStr(TCPermanentOccupationSetByUserPenStyle));
    WriteStringTwice(PenStylesSectionStr, TCPermanentSystemOccupationPenStyleStr, PenStyleToStr(TCPermanentSystemOccupationPenStyle));

    { Fonts }
    WriteStringTwice(FontsSectionStr, RailFontNameStr, RailFontName);
    WriteIntegerTwice(FontsSectionStr, LineFontHeightStr, LineFontHeight);
    WriteStringTwice(FontsSectionStr, LoggingWindowFontNameStr, LoggingWindowFontName);
    WriteIntegerTwice(FontsSectionStr, LoggingWindowFontSizeStr, LoggingWindowFontSize);
    WriteIntegerTwice(FontsSectionStr, FWPRailWindowFontHeightStr, FWPRailWindowFontHeight);
    WriteIntegerTwice(FontsSectionStr, PlatformNumberFontHeightStr, PlatformNumberFontHeight);
    WriteStringTwice(FontsSectionStr, StationMonitorsFontNameStr, StationMonitorsFontName);
    WriteIntegerTwice(FontsSectionStr, StationMonitorsLargeFontHeightStr, StationMonitorsLargeFontHeight);
    WriteIntegerTwice(FontsSectionStr, StationMonitorsSmallFontHeightStr, StationMonitorsSmallFontHeight);
    WriteIntegerTwice(FontsSectionStr, TheatreFontHeightStr, TheatreFontHeight);

    { Dialogue Boxes variables }
    WriteIntegerTwice(DialogueBoxSectionStr, DebuggingOptionsWindowLeftStr, DebuggingOptionsWindowLeft);
    WriteIntegerTwice(DialogueBoxSectionStr, DebuggingOptionsWindowTopStr, DebuggingOptionsWindowTop);
    WriteIntegerTwice(DialogueBoxSectionStr, LineDialogueBoxLeftStr, LineDialogueBoxLeft);
    WriteIntegerTwice(DialogueBoxSectionStr, LineDialogueBoxTopStr, LineDialogueBoxTop);
    WriteIntegerTwice(DialogueBoxSectionStr, LocoDialogueWindowLeftStr, LocoDialogueWindowLeft);
    WriteIntegerTwice(DialogueBoxSectionStr, LocoDialogueWindowTopStr, LocoDialogueWindowTop);
    WriteBoolTwice(DialogueBoxSectionStr, LocoDialogueSpeedInMPHStr, LocoDialogueSpeedInMPH);
    WriteIntegerTwice(DialogueBoxSectionStr, PointDialogueBoxLeftStr, PointDialogueBoxLeft);
    WriteIntegerTwice(DialogueBoxSectionStr, PointDialogueBoxTopStr, PointDialogueBoxTop);
    WriteIntegerTwice(DialogueBoxSectionStr, SignalDialogueBoxLeftStr, SignalDialogueBoxLeft);
    WriteIntegerTwice(DialogueBoxSectionStr, SignalDialogueBoxTopStr, SignalDialogueBoxTop);
    WriteIntegerTwice(DialogueBoxSectionStr, TrackCircuitDialogueBoxLeftStr, TrackCircuitDialogueBoxLeft);
    WriteIntegerTwice(DialogueBoxSectionStr, TrackCircuitDialogueBoxTopStr, TrackCircuitDialogueBoxTop);

    { Process RailDriver data }
    WriteIntegerTwice(RDCSectionStr, RDCBailOffMinStr, RDCBailOffMin);
    WriteIntegerTwice(RDCSectionStr, RDCBailOffMaxStr, RDCBailOffMax);
    WriteIntegerTwice(RDCSectionStr, RDCEmergencyBrakeMinStr, RDCEmergencyBrakeMin);
    WriteIntegerTwice(RDCSectionStr, RDCEmergencyBrakeMaxStr, RDCEmergencyBrakeMax);
    WriteIntegerTwice(RDCSectionStr, RDCLocoBrakeMinStr, RDCLocoBrakeMin);
    WriteIntegerTwice(RDCSectionStr, RDCLocoBrakeMaxStr, RDCLocoBrakeMax);
    WriteIntegerTwice(RDCSectionStr, RDCRegulatorForwardMinStr, RDCRegulatorForwardMin);
    WriteIntegerTwice(RDCSectionStr, RDCRegulatorForwardMaxStr, RDCRegulatorForwardMax);
    WriteIntegerTwice(RDCSectionStr, RDCRegulatorReverseMinStr, RDCRegulatorReverseMin);
    WriteIntegerTwice(RDCSectionStr, RDCRegulatorReverseMaxStr, RDCRegulatorReverseMax);
    WriteIntegerTwice(RDCSectionStr, RDCReverserForwardMinStr, RDCReverserForwardMin);
    WriteIntegerTwice(RDCSectionStr, RDCReverserForwardMaxStr, RDCReverserForwardMax);
    WriteIntegerTwice(RDCSectionStr, RDCReverserReverseMinStr, RDCReverserReverseMin);
    WriteIntegerTwice(RDCSectionStr, RDCReverserReverseMaxStr, RDCReverserReverseMax);
    WriteIntegerTwice(RDCSectionStr, RDCTrainBrakeMinStr, RDCTrainBrakeMin);
    WriteIntegerTwice(RDCSectionStr, RDCTrainBrakeMaxStr, RDCTrainBrakeMax);
    WriteIntegerTwice(RDCSectionStr, RDCThreeWaySwitchALeftNumStr, RDCThreeWaySwitchALeftNum);
    WriteIntegerTwice(RDCSectionStr, RDCThreeWaySwitchAMidNumStr, RDCThreeWaySwitchAMidNum);
    WriteIntegerTwice(RDCSectionStr, RDCThreeWaySwitchARightNumStr, RDCThreeWaySwitchARightNum);
    WriteIntegerTwice(RDCSectionStr, RDCThreeWaySwitchBLeftNumStr, RDCThreeWaySwitchBLeftNum);
    WriteIntegerTwice(RDCSectionStr, RDCThreeWaySwitchBMidNumStr, RDCThreeWaySwitchBMidNum);
    WriteIntegerTwice(RDCSectionStr, RDCThreeWaySwitchBRightNumStr, RDCThreeWaySwitchBRightNum);

    { Windows }
    WriteIntegerTwice(WindowsSectionStr, FWPRailWindowWidthStr, FWPRailWindow.Width);
    WriteIntegerTwice(WindowsSectionStr, FWPRailWindowHeightStr, FWPRailWindow.Height);
    WriteIntegerTwice(WindowsSectionStr, FWPRailWindowTopStr, FWPRailWindow.Top);
    WriteIntegerTwice(WindowsSectionStr, FWPRailWindowLeftStr, FWPRailWindow.Left);
    WriteIntegerTwice(WindowsSectionStr, FWPRailWindowClientWidthStr, FWPRailWindow.ClientWidth);
    WriteIntegerTwice(WindowsSectionStr, FWPRailWindowClientHeightStr, FWPRailWindow.ClientHeight);

    IF (DisplayLineColoursWindow <> NIL) AND (UseDisplayLineColoursWindowSettingsOnStartup) THEN BEGIN
      WriteIntegerTwice(WindowsSectionStr, DisplayLineColoursWindowHeightStr, DisplayLineColoursWindow.Height);
      WriteIntegerTwice(WindowsSectionStr, DisplayLineColoursWindowLeftStr, DisplayLineColoursWindow.Left);
      WriteIntegerTwice(WindowsSectionStr, DisplayLineColoursWindowTopStr, DisplayLineColoursWindow.Top);
      WriteIntegerTwice(WindowsSectionStr, DisplayLineColoursWindowWidthStr, DisplayLineColoursWindow.Width);
    END;

    IF DebugWindow <> NIL THEN BEGIN
      WriteIntegerTwice(WindowsSectionStr, DebugWindowHeightStr, DebugWindow.Height);
      WriteIntegerTwice(WindowsSectionStr, DebugWindowLeftStr, DebugWindow.Left);
      WriteIntegerTwice(WindowsSectionStr, DebugWindowTopStr, DebugWindow.Top);
      WriteIntegerTwice(WindowsSectionStr, DebugWindowWidthStr, DebugWindow.Width);
    END;

    IF DiagramsWindow <> NIL THEN BEGIN
      WriteIntegerTwice(WindowsSectionStr, DiagramsWindowHeightStr, DiagramsWindow.Height);
      WriteIntegerTwice(WindowsSectionStr, DiagramsWindowLeftStr, DiagramsWindow.Left);
      WriteIntegerTwice(WindowsSectionStr, DiagramsWindowTopStr, DiagramsWindow.Top);
      WriteIntegerTwice(WindowsSectionStr, DiagramsLargeWindowWidthStr, DiagramsLargeWindowWidth);
      WriteIntegerTwice(WindowsSectionStr, DiagramsSmallWindowWidthStr, DiagramsSmallWindowWidth);
    END;

    IF EditWindow <> NIL THEN BEGIN
      WriteIntegerTwice(WindowsSectionStr, EditWindowHeightStr, EditWindow.Height);
      WriteIntegerTwice(WindowsSectionStr, EditWindowLeftStr, EditWindow.Left);
      WriteIntegerTwice(WindowsSectionStr, EditWindowTopStr, EditWindow.Top);
      WriteIntegerTwice(WindowsSectionStr, EditWindowWidthStr, EditWindowWidth);
    END;

    IF LockListWindow <> NIL THEN BEGIN
      WriteIntegerTwice(WindowsSectionStr, LockListWindowHeightStr, LockListWindow.Height);
      WriteIntegerTwice(WindowsSectionStr, LockListWindowLeftStr, LockListWindow.Left);
      WriteIntegerTwice(WindowsSectionStr, LockListWindowTopStr, LockListWindow.Top);
      WriteIntegerTwice(WindowsSectionStr, LockListWindowWidthStr, LockListWindow.Width);
    END;

    IF LocoUtilsWindow <> NIL THEN BEGIN
      WriteIntegerTwice(WindowsSectionStr, LocoUtilsWindowHeightStr, LocoUtilsWindow.Height);
      WriteIntegerTwice(WindowsSectionStr, LocoUtilsWindowLeftStr, LocoUtilsWindow.Left);
      WriteIntegerTwice(WindowsSectionStr, LocoUtilsWindowTopStr, LocoUtilsWindow.Top);
      WriteIntegerTwice(WindowsSectionStr, LocoUtilsWindowWidthStr, LocoUtilsWindow.Width);
    END;

    IF LoggingWindow <> NIL THEN BEGIN
      WriteIntegerTwice(WindowsSectionStr, LoggingWindowHeightStr, LoggingWindow.Height);
      WriteIntegerTwice(WindowsSectionStr, LoggingWindowLeftStr, LoggingWindow.Left);
      WriteIntegerTwice(WindowsSectionStr, LoggingWindowTopStr, LoggingWindow.Top);
      WriteIntegerTwice(WindowsSectionStr, LoggingWindowWidthStr, LoggingWindow.Width);
    END;

    IF MovementWindow <> NIL THEN BEGIN
      WriteIntegerTwice(WindowsSectionStr, MovementWindowHeightStr, MovementWindow.Height);
      WriteIntegerTwice(WindowsSectionStr, MovementWindowLeftStr, MovementWindow.Left);
      WriteIntegerTwice(WindowsSectionStr, MovementWindowTopStr, MovementWindow.Top);
      WriteIntegerTwice(WindowsSectionStr, MovementWindowWidthStr, MovementWindow.Width);
    END;

    IF OptionsWindow <> NIL THEN BEGIN
      WriteIntegerTwice(WindowsSectionStr, OptionsWindowHeightStr, OptionsWindow.Height);
      WriteIntegerTwice(WindowsSectionStr, OptionsWindowLeftStr, OptionsWindow.Left);
      WriteIntegerTwice(WindowsSectionStr, OptionsWindowTopStr, OptionsWindow.Top);
      WriteIntegerTwice(WindowsSectionStr, OptionsWindowWidthStr, OptionsWindow.Width);

      WriteIntegerTwice(WindowsSectionStr, OptionsWindowValueListEditorCol0WidthStr, OptionsWindow.OptionsValueListEditor.ColWidths[0]);
    END;

    IF WorkingTimetableWindow <> NIL THEN BEGIN
      WriteIntegerTwice(WindowsSectionStr, WorkingTimetableWindowHeightStr, WorkingTimetableWindow.Height);
      WriteIntegerTwice(WindowsSectionStr, WorkingTimetableWindowLeftStr, WorkingTimetableWindow.Left);
      WriteIntegerTwice(WindowsSectionStr, WorkingTimetableWindowTopStr, WorkingTimetableWindow.Top);
      WriteIntegerTwice(WindowsSectionStr, WorkingTimetableLargeWindowWidthStr, WorkingTimetableLargeWindowWidth);
      WriteIntegerTwice(WindowsSectionStr, WorkingTimetableSmallWindowWidthStr, WorkingTimetableSmallWindowWidth);
    END;

    { Other options }
    WriteBoolTwice(OtherOptionsSectionStr, AcceptAllPermanentOccupationsWithoutFeedbackStr, AcceptAllPermanentOccupationsWithoutFeedback);
    WriteBoolTwice(OtherOptionsSectionStr, AutomaticallySetFocusWhenInDebugWindowStr, AutomaticallySetFocusWhenInDebugWindow);
    WriteBoolTwice(OtherOptionsSectionStr, CancelAllTrainsWithNoFeedbackOccupationStr, CancelAllTrainsWithNoFeedbackOccupation);
    WriteIntegerTwice(OtherOptionsSectionStr, CarriageLengthInInchesStr, CarriageLengthInInches);
    WriteBoolTwice(OtherOptionsSectionStr, CheckForIdenticalLinesInLogStr, CheckForIdenticalLinesInLog);
    WriteBoolTwice(OtherOptionsSectionStr, DisplayDiagramsStr, DisplayDiagrams);
    WriteBoolTwice(OtherOptionsSectionStr, DisplayFlashingTrackCircuitsStr, DisplayFlashingTrackCircuits);
    WriteBoolTwice(OtherOptionsSectionStr, DisplayLocoChipNumsStr, DisplayLocoChipNums);
    WriteBoolTwice(OtherOptionsSectionStr, DisplayLocoHeadcodesStr, DisplayLocoHeadcodes);
    WriteBoolTwice(OtherOptionsSectionStr, DisplayNotForPublicUseTrainsInStationMonitorsStr, DisplayNotForPublicUseTrainsInStationMonitors);
    WriteBoolTwice(OtherOptionsSectionStr, DisplayRoutesAndJourneysStr, DisplayRoutesAndJourneys);
    WriteBoolTwice(OtherOptionsSectionStr, DisplayWorkingTimetableStr, DisplayWorkingTimetable);
    WriteBoolTwice(OtherOptionsSectionStr, DoNotCancelTrainsWithNoFeedbackOccupationStr, DoNotCancelTrainsWithNoFeedbackOccupation);
    WriteBoolTwice(OtherOptionsSectionStr, HighlightTrackCircuitSpeedRestrictionsStr, HighlightTrackCircuitSpeedRestrictions);
    WriteBoolTwice(OtherOptionsSectionStr, LargeDiagramsWindowSelectedStr, LargeDiagramsWindowSelected);
    WriteBoolTwice(OtherOptionsSectionStr, LargeWorkingTimetableWindowSelectedStr, LargeWorkingTimetableWindowSelected);
    WriteStringTwice(OtherOptionsSectionStr, LineThicknessInFullScreenModeStr, LineThicknessInFullScreenMode);
    WriteIntegerTwice(OtherOptionsSectionStr, LocoTimingTimeBeforeAutoStopInSecondsStr, LocoTimingTimeBeforeAutoStopInSeconds);
    WriteBoolTwice(OtherOptionsSectionStr, LogCurrentTimeModeStr, LogCurrentTimeMode);
    WriteBoolTwice(OtherOptionsSectionStr, LogsKeptModeStr, LogsKeptMode);
    WriteBoolTwice(OtherOptionsSectionStr, MakeSoundWhenDebugWindowBoldTextAppearsStr, MakeSoundWhenDebugWindowBoldTextAppears);
    WriteIntegerTwice(OtherOptionsSectionStr, MaxRectangleUndrawTimeStr, MaxRectangleUndrawTime);
    WriteBoolTwice(OtherOptionsSectionStr, MenusVisibleStr, MenusVisible);
    WriteBoolTwice(OtherOptionsSectionStr, MonitorStrayingTrainsStr, MonitorStrayingTrains);
    WriteIntegerTwice(OtherOptionsSectionStr, PointFeedbackMaximumWaitInSecondsStr, PointFeedbackMaximumWaitInSeconds);
    WriteIntegerTwice(OtherOptionsSectionStr, RouteAheadNotClearWaitTimeInMinutesStr, RouteAheadNotClearWaitTimeInMinutes);
    WriteBoolTwice(OtherOptionsSectionStr, RunTestUnitOnStartupStr, RunTestUnitOnStartup);
    WriteBoolTwice(OtherOptionsSectionStr, ShowCancelledTrainsInDiagramsStr, ShowCancelledTrainsInDiagrams);
      
    WriteBoolTwice(OtherOptionsSectionStr, ShowIncorrectDayOfTheWeekEntriesInWorkingTimetableStr, ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable);
    WriteBoolTwice(OtherOptionsSectionStr, ShowNonMovingTrainsInDiagramsStr, ShowNonMovingTrainsInDiagrams);
    WriteBoolTwice(OtherOptionsSectionStr, ShowNonStopsInDiagramsStr, ShowNonStopsInDiagrams);
    WriteBoolTwice(OtherOptionsSectionStr, ShowTrackCircuitsWhereUserMustDriveStr, ShowTrackCircuitsWhereUserMustDrive);
    WriteBoolTwice(OtherOptionsSectionStr, StartRepeatJourneysOnNewLineInDiagramsStr, StartRepeatJourneysOnNewLineInDiagrams);
    WriteBoolTwice(OtherOptionsSectionStr, StartWithDiagramsStr, StartWithDiagrams);
    WriteIntegerTwice(OtherOptionsSectionStr, StationEndOfDayPassengerLeavingTimeInMinutesStr, StationEndOfDayPassengerLeavingTimeInMinutes);
    WriteBoolTwice(OtherOptionsSectionStr, StationMonitorsWebPageRequiredStr, StationMonitorsWebPageRequired);
    WriteIntegerTwice(OtherOptionsSectionStr, StationOppositeDirectionExitMinimumWaitTimeInMinutesStr, StationOppositeDirectionExitMinimumWaitTimeInMinutes);
    WriteIntegerTwice(OtherOptionsSectionStr, StationSameDirectionExitMinimumWaitTimeInMinutesStr, StationSameDirectionExitMinimumWaitTimeInMinutes);
    WriteBoolTwice(OtherOptionsSectionStr, StationStartModeStr, InStationStartMode);
    WriteIntegerTwice(OtherOptionsSectionStr, StationStartOfDayPassengerBoardingTimeInMinutesStr, StationStartOfDayPassengerBoardingTimeInMinutes);
    WriteBoolTwice(OtherOptionsSectionStr, StopAllLocosAtShutDownStr, StopAllLocosAtShutDown);
    WriteBoolTwice(OtherOptionsSectionStr, SwitchActiveLocoLightsOffAtShutDownStr, SwitchActiveLocoLightsOffAtShutDown);
    WriteIntegerTwice(OtherOptionsSectionStr, TheatreBoxHeightStr, TheatreBoxHeight);
    WriteIntegerTwice(OtherOptionsSectionStr, TheatreBoxWidthStr, TheatreBoxWidth);
    WriteBoolTwice(OtherOptionsSectionStr, UseDisplayLineColoursWindowSettingsOnStartupStr, UseDisplayLineColoursWindowSettingsOnStartup);
    WriteIntegerTwice(OtherOptionsSectionStr, WaitBeforeRerouteInMinutesStr, WaitBeforeRerouteInMinutes);
    WriteBoolTwice(OtherOptionsSectionStr, WorkingTimetableModeStr, WorkingTimetableMode);

    { Various times }
    WriteStringTwice(TimesSectionStr, ProgramStartTimeOptionStr, TimeToHMSStr(ProgramStartTime));
    WriteStringTwice(TimesSectionStr, DayLightStartTimeOptionStr, TimeToHMSStr(DayLightStartTime));
    WriteStringTwice(TimesSectionStr, DayLightEndTimeOptionStr, TimeToHMSStr(DayLightEndTime));
    WriteStringTwice(TimesSectionStr, CurrentRailwayDayOfTheWeekStr, DayOfTheWeekToStr(CurrentRailwayDayOfTheWeek));

    { Screen settings }
    WriteIntegerTwice(ScreenOptionsStr, LogFileMaxWidthInCharsStr, LogFileMaxWidthInChars);

    CASE ScreenMode OF
      DefaultWindowedScreenMode:
        WriteStringTwice(ScreenOptionsStr, ScreenModeStr, DefaultWindowedScreenStr);
      FullScreenMode:
        WriteStringTwice(ScreenOptionsStr, ScreenModeStr, FullScreenStr);
      FullScreenWithStatusBarMode:
        WriteStringTwice(ScreenOptionsStr, ScreenModeStr, FullScreenWithStatusBarStr);
      CustomWindowedScreenMode:
        WriteStringTwice(ScreenOptionsStr, ScreenModeStr, CustomWindowedScreenStr);
    END; {CASE}

    WriteIntegerTwice(ScreenOptionsStr, BufferStopVerticalSpacingStr, BufferStopVerticalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, DeltaPointXStr, DeltaPointX);
    WriteIntegerTwice(ScreenOptionsStr, IndicatorHorizontalSpacingStr, IndicatorHorizontalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, IndicatorVerticalSpacingStr, IndicatorVerticalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, MouseRectangleEdgeVerticalSpacingStr, MouseRectangleEdgeVerticalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, PlatformEdgeVerticalSpacingStr, PlatformEdgeVerticalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, PlatformNumberEdgeHorizontalSpacingStr, PlatformNumberEdgeHorizontalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, PlatformNumberEdgeVerticalSpacingStr, PlatformNumberEdgeVerticalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, SignalHorizontalSpacingStr, SignalHorizontalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, SignalRadiusStr, SignalRadius);
    WriteIntegerTwice(ScreenOptionsStr, SignalSemaphoreHeightStr, SignalSemaphoreHeight);
    WriteIntegerTwice(ScreenOptionsStr, SignalSemaphoreWidthStr, SignalSemaphoreWidth);
    WriteIntegerTwice(ScreenOptionsStr, SignalVerticalSpacingStr, SignalVerticalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, SpeedRestrictionHorizontalSpacingStr, SpeedRestrictionHorizontalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, SpeedRestrictionVerticalSpacingStr, SpeedRestrictionVerticalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, TheatreIndicatorHorizontalSpacingStr, TheatreIndicatorHorizontalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, TheatreIndicatorVerticalSpacingStr, TheatreIndicatorVerticalSpacing);
    WriteIntegerTwice(ScreenOptionsStr, TRSPlungerLengthStr, TRSPlungerLength);
    WriteIntegerTwice(ScreenOptionsStr, WindowRowsStr, WindowRows);

    { Other Miscellaneous Data }
    WriteStringTwice(MiscellaneousDataSectionStr, CurrentParametersStr, CurrentParametersFromParamStr);

    IniFile.Free;
    RegistryIniFile.Free;
  EXCEPT
    ON E : Exception DO
      Log('EG WriteIniFile: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteIniFile }

PROCEDURE WriteOptionsToValueList;
{ Fill the options value list with values that can be changed by the user }
CONST
  FilenamesStr = '<B>Filenames<\B>';
  FullScreenOptionsStr = '<B>Full Screen Options<\B>';
  OtherSettingsStr = '<B>Other Settings<\B>';
  PathsAndDirectoriesStr = '<B>Paths and Directories<\B>';
  ScreenSpacingValuesStr = '<B>Screen Spacing Values<\B>';
  FontsStr = '<B>Fonts<\B>';
  TimeOfDayValuesStr ='<B>Time of Day Values<\B>';
  RDCStr = '<B>Rail Driver Values<\B>';

BEGIN
  TRY
    WITH OptionsWindow.OptionsValueListEditor DO BEGIN
      { Paths and Directories }
      Values[PathsAndDirectoriesStr] := ' ';
      { note: there appears to be a bug in the ValueList that adds a PickList to the first item even if we set its type to read-only and simple: }
      ItemProps[PathsAndDirectoriesStr].ReadOnly := True;
      ItemProps[PathsAndDirectoriesStr].EditStyle := esSimple;

      Values[PathToLogFilesStr] := PathToLogFiles;
      Values[PathToRailDataFilesStr] := PathToRailDataFiles;
      Values[PathToRailSourceFilesStr] := PathToRailSourceFiles;

      { Filenames }
      Values[FilenamesStr] := '';
      ItemProps[FilenamesStr].ReadOnly := True;

      Values[AreaDataFilenameStr] := AreaDataFilename;
      Values[AreaDataFilenameSuffixStr] := AreaDataFilenameSuffix;
      Values[DataCheckFileNameStr] := DataCheckFileName;
      Values[DiagramsFilenameStr] := DiagramsFilename;
      Values[DiagramsFilenameSuffixStr] := DiagramsFilenameSuffix;
      Values[FeedbackDataFilenameStr] := FeedbackDataFilename;
      Values[FeedbackDataFilenameSuffixStr] := FeedbackDataFilenameSuffix;
      Values[LineDataFilenameStr] := LineDataFilename;
      Values[LineDataFilenameSuffixStr] := LineDataFilenameSuffix;
      Values[LocoDataFilenameStr] := LocoDataFilename;
      Values[LocationDataFilenameSuffixStr] := LocationDataFilenameSuffix;
      Values[LocationDataFilenameStr] := LocationDataFilename;
      Values[LocoDataFilenameSuffixStr] := LocoDataFilenameSuffix;
      Values[LogFilenameStr] := LogFilename;
      Values[LogFilenameSuffixStr] := LogFilenameSuffix;
      Values[PlatformDataFilenameStr] := PlatformDataFilename;
      Values[PlatformDataFilenameSuffixStr] := PlatformDataFilenameSuffix;
      Values[PointDataFilenameStr] := PointDataFilename;
      Values[PointDataFilenameSuffixStr] := PointDataFilenameSuffix;
      Values[RouteingExceptionDataFilenameStr] := RouteingExceptionDataFilename;
      Values[RouteingExceptionDataFilenameSuffixStr] := RouteingExceptionDataFilenameSuffix;
      Values[ReplayFilenameStr] := ReplayFilename;
      Values[ReplayFilenameSuffixStr] := ReplayFilenameSuffix;
      Values[SignalDataFilenameStr] := SignalDataFilename;
      Values[SignalDataFilenameSuffixStr] := SignalDataFilenameSuffix;
      Values[TrackCircuitDataFilenameStr] := TrackCircuitDataFilename;
      Values[TrackCircuitDataFilenameSuffixStr] := TrackCircuitDataFilenameSuffix;
      Values[WorkingTimetableFilenameStr] := WorkingTimetableFilename;
      Values[WorkingTimetableFilenameSuffixStr] := WorkingTimetableFilenameSuffix;

      { Full Screen }
      Values[FullScreenOptionsStr] := '';
      ItemProps[FullScreenOptionsStr].ReadOnly := True;

      CASE ScreenMode OF
        DefaultWindowedScreenMode:
          Values[ScreenModeStr] := DefaultWindowedScreenStr;
        FullScreenMode:
          Values[ScreenModeStr] := FullScreenStr;
        FullScreenWithStatusBarMode:
          Values[ScreenModeStr] := FullScreenWithStatusBarStr;
        CustomWindowedScreenMode:
          Values[ScreenModeStr] := CustomWindowedScreenStr;
      END; {CASE}

      ItemProps[ScreenModeStr].PickList.Add(CustomWindowedScreenStr);
      ItemProps[ScreenModeStr].PickList.Add(DefaultWindowedScreenStr);
      ItemProps[ScreenModeStr].PickList.Add(FullScreenStr);
      ItemProps[ScreenModeStr].PickList.Add(FullScreenWithStatusBarStr);
      ItemProps[ScreenModeStr].EditStyle := esPickList;

      { Fonts }
      Values[FontsStr] := '';
      Values[RailFontNameStr] := RailFontName;
      Values[LineFontHeightStr] := IntToStr(LineFontHeight);
      Values[LoggingWindowFontNameStr] := LoggingWindowFontName;
      Values[LoggingWindowFontSizeStr] := IntToStr(LoggingWindowFontSize);
      Values[FWPRailWindowFontHeightStr] := IntToStr(FWPRailWindowFontHeight);
      Values[PlatformNumberFontHeightStr] := IntToStr(PlatformNumberFontHeight);
      Values[StationMonitorsFontNameStr] := StationMonitorsFontName;
      Values[StationMonitorsLargeFontHeightStr] := IntToStr(StationMonitorsLargeFontHeight);
      Values[StationMonitorsSmallFontHeightStr] := IntToStr(StationMonitorsSmallFontHeight);
      Values[TheatreFontHeightStr] := IntToStr(TheatreFontHeight);

      { Screen Spacing }
      Values[ScreenSpacingValuesStr] := '';
      ItemProps[ScreenSpacingValuesStr].ReadOnly := True;

      Values[LogFileMaxWidthInCharsStr] := IntToStr(LogFileMaxWidthInChars);
      Values[BufferStopVerticalSpacingStr] := IntToStr(BufferStopVerticalSpacing);
      Values[DeltaPointXStr] := IntToStr(DeltaPointX);
      Values[IndicatorHorizontalSpacingStr] := IntToStr(IndicatorHorizontalSpacing);
      Values[IndicatorVerticalSpacingStr] := IntToStr(IndicatorVerticalSpacing);
      Values[MouseRectangleEdgeVerticalSpacingStr] := IntToStr(MouseRectangleEdgeVerticalSpacing);
      Values[PlatformEdgeVerticalSpacingStr] := IntToStr(PlatformEdgeVerticalSpacing);
      Values[PlatformNumberEdgeHorizontalSpacingStr] := IntToStr(PlatformNumberEdgeHorizontalSpacing);
      Values[PlatformNumberEdgeVerticalSpacingStr] := IntToStr(PlatformNumberEdgeVerticalSpacing);
      Values[SignalHorizontalSpacingStr] := IntToStr(SignalHorizontalSpacing);
      Values[SignalRadiusStr] := IntToStr(SignalRadius);
      Values[SignalSemaphoreHeightStr] := IntToStr(SignalSemaphoreHeight);
      Values[SignalSemaphoreWidthStr] := IntToStr(SignalSemaphoreWidth);
      Values[SignalVerticalSpacingStr] := IntToStr(SignalVerticalSpacing);
      Values[SpeedRestrictionHorizontalSpacingStr] := IntToStr(SpeedRestrictionHorizontalSpacing);
      Values[SpeedRestrictionVerticalSpacingStr] := IntToStr(SpeedRestrictionVerticalSpacing);
      Values[TheatreIndicatorHorizontalSpacingStr] := IntToStr(TheatreIndicatorHorizontalSpacing);
      Values[TheatreIndicatorVerticalSpacingStr] := IntToStr(TheatreIndicatorVerticalSpacing);
      Values[TRSPlungerLengthStr] := IntToStr(TRSPlungerLength);
      Values[WindowRowsStr] := IntToStr(WindowRows);

      { Times }
      Values[TimeOfDayValuesStr] := '';
      ItemProps[TimeOfDayValuesStr].ReadOnly := True;

      Values[ProgramStartTimeOptionStr] := TimeToHMSStr(ProgramStartTime);
      Values[DayLightEndTimeOptionStr] := TimeToHMSStr(DayLightEndTime);
      Values[DayLightStartTimeOptionStr] := TimeToHMSStr(DayLightStartTime);
      Values[CurrentRailwayDayOfTheWeekStr] := DayOfTheWeekToStr(CurrentRailwayDayOfTheWeek);

      { Other Settings }
      Values[OtherSettingsStr] := '';
      ItemProps[OtherSettingsStr].ReadOnly := True;

      Values[AcceptAllPermanentOccupationsWithoutFeedbackStr] := BoolToStr(AcceptAllPermanentOccupationsWithoutFeedback, True);
      ItemProps[AcceptAllPermanentOccupationsWithoutFeedbackStr].PickList.Add('True');
      ItemProps[AcceptAllPermanentOccupationsWithoutFeedbackStr].PickList.Add('False');
      ItemProps[AcceptAllPermanentOccupationsWithoutFeedbackStr].EditStyle := esPickList;

      Values[AutomaticallySetFocusWhenInDebugWindowStr] := BoolToStr(AutomaticallySetFocusWhenInDebugWindow, True);
      ItemProps[AutomaticallySetFocusWhenInDebugWindowStr].PickList.Add('True');
      ItemProps[AutomaticallySetFocusWhenInDebugWindowStr].PickList.Add('False');
      ItemProps[AutomaticallySetFocusWhenInDebugWindowStr].EditStyle := esPickList;

      Values[CancelAllTrainsWithNoFeedbackOccupationStr] := BoolToStr(CancelAllTrainsWithNoFeedbackOccupation, True);
      ItemProps[CancelAllTrainsWithNoFeedbackOccupationStr].PickList.Add('True');
      ItemProps[CancelAllTrainsWithNoFeedbackOccupationStr].PickList.Add('False');
      ItemProps[CancelAllTrainsWithNoFeedbackOccupationStr].EditStyle := esPickList;

      Values[CarriageLengthInInchesStr] := IntToStr(CarriageLengthInInches);

      Values[CheckForIdenticalLinesInLogStr] := BoolToStr(CheckForIdenticalLinesInLog, True);
      ItemProps[CheckForIdenticalLinesInLogStr].PickList.Add('True');
      ItemProps[CheckForIdenticalLinesInLogStr].PickList.Add('False');
      ItemProps[CheckForIdenticalLinesInLogStr].EditStyle := esPickList;

      Values[DisplayDiagramsStr] := BoolToStr(DisplayDiagrams, True);
      ItemProps[DisplayDiagramsStr].PickList.Add('True');
      ItemProps[DisplayDiagramsStr].PickList.Add('False');
      ItemProps[DisplayDiagramsStr].EditStyle := esPickList;

      Values[DisplayFlashingTrackCircuitsStr] := BoolToStr(DisplayFlashingTrackCircuits, True);
      ItemProps[DisplayFlashingTrackCircuitsStr].PickList.Add('True');
      ItemProps[DisplayFlashingTrackCircuitsStr].PickList.Add('False');
      ItemProps[DisplayFlashingTrackCircuitsStr].EditStyle := esPickList;

      Values[DisplayLocoChipNumsStr] := BoolToStr(DisplayLocoChipNums, True);
      ItemProps[DisplayLocoChipNumsStr].PickList.Add('True');
      ItemProps[DisplayLocoChipNumsStr].PickList.Add('False');
      ItemProps[DisplayLocoChipNumsStr].EditStyle := esPickList;

      Values[DisplayLocoHeadcodesStr] := BoolToStr(DisplayLocoHeadcodes, True);
      ItemProps[DisplayLocoHeadcodesStr].PickList.Add('True');
      ItemProps[DisplayLocoHeadcodesStr].PickList.Add('False');
      ItemProps[DisplayLocoHeadcodesStr].EditStyle := esPickList;

      Values[DisplayNotForPublicUseTrainsInStationMonitorsStr] := BoolToStr(DisplayNotForPublicUseTrainsInStationMonitors, True);
      ItemProps[DisplayNotForPublicUseTrainsInStationMonitorsStr].PickList.Add('True');
      ItemProps[DisplayNotForPublicUseTrainsInStationMonitorsStr].PickList.Add('False');
      ItemProps[DisplayNotForPublicUseTrainsInStationMonitorsStr].EditStyle := esPickList;

      Values[DisplayRoutesAndJourneysStr] := BoolToStr(DisplayRoutesAndJourneys, True);
      ItemProps[DisplayRoutesAndJourneysStr].PickList.Add('True');
      ItemProps[DisplayRoutesAndJourneysStr].PickList.Add('False');
      ItemProps[DisplayRoutesAndJourneysStr].EditStyle := esPickList;

      Values[DisplayWorkingTimetableStr] := BoolToStr(DisplayWorkingTimetable, True);
      ItemProps[DisplayWorkingTimetableStr].PickList.Add('True');
      ItemProps[DisplayWorkingTimetableStr].PickList.Add('False');
      ItemProps[DisplayWorkingTimetableStr].EditStyle := esPickList;

      Values[DoNotCancelTrainsWithNoFeedbackOccupationStr] := BoolToStr(DoNotCancelTrainsWithNoFeedbackOccupation, True);
      ItemProps[DoNotCancelTrainsWithNoFeedbackOccupationStr].PickList.Add('True');
      ItemProps[DoNotCancelTrainsWithNoFeedbackOccupationStr].PickList.Add('False');
      ItemProps[DoNotCancelTrainsWithNoFeedbackOccupationStr].EditStyle := esPickList;

      Values[HighlightTrackCircuitSpeedRestrictionsStr] := BoolToStr(HighlightTrackCircuitSpeedRestrictions, True);
      ItemProps[HighlightTrackCircuitSpeedRestrictionsStr].PickList.Add('True');
      ItemProps[HighlightTrackCircuitSpeedRestrictionsStr].PickList.Add('False');
      ItemProps[HighlightTrackCircuitSpeedRestrictionsStr].EditStyle := esPickList;

      Values[LargeDiagramsWindowSelectedStr] := BoolToStr(LargeDiagramsWindowSelected, True);
      ItemProps[LargeDiagramsWindowSelectedStr].PickList.Add('True');
      ItemProps[LargeDiagramsWindowSelectedStr].PickList.Add('False');
      ItemProps[LargeDiagramsWindowSelectedStr].EditStyle := esPickList;

      Values[LargeWorkingTimetableWindowSelectedStr] := BoolToStr(LargeWorkingTimetableWindowSelected, True);
      ItemProps[LargeWorkingTimetableWindowSelectedStr].PickList.Add('True');
      ItemProps[LargeWorkingTimetableWindowSelectedStr].PickList.Add('False');
      ItemProps[LargeWorkingTimetableWindowSelectedStr].EditStyle := esPickList;

      Values[LineThicknessInFullScreenModeStr] := LineThicknessInFullScreenMode;
      ItemProps[LineThicknessInFullScreenModeStr].PickList.Add('Thick');
      ItemProps[LineThicknessInFullScreenModeStr].PickList.Add('Thin');
      ItemProps[LineThicknessInFullScreenModeStr].EditStyle := esPickList;

      Values[LocoTimingTimeBeforeAutoStopInSecondsStr] := IntToStr(LocoTimingTimeBeforeAutoStopInSeconds);

      Values[LogCurrentTimeModeStr] := BoolToStr(LogCurrentTimeMode, True);
      ItemProps[LogCurrentTimeModeStr].PickList.Add('True');
      ItemProps[LogCurrentTimeModeStr].PickList.Add('False');
      ItemProps[LogCurrentTimeModeStr].EditStyle := esPickList;

      Values[LogsKeptModeStr] := BoolToStr(LogsKeptMode, True);
      ItemProps[LogsKeptModeStr].PickList.Add('True');
      ItemProps[LogsKeptModeStr].PickList.Add('False');
      ItemProps[LogsKeptModeStr].EditStyle := esPickList;

      Values[MakeSoundWhenDebugWindowBoldTextAppearsStr] := BoolToStr(MakeSoundWhenDebugWindowBoldTextAppears, True);
      ItemProps[MakeSoundWhenDebugWindowBoldTextAppearsStr].PickList.Add('True');
      ItemProps[MakeSoundWhenDebugWindowBoldTextAppearsStr].PickList.Add('False');
      ItemProps[MakeSoundWhenDebugWindowBoldTextAppearsStr].EditStyle := esPickList;

      Values[MaxRectangleUndrawTimeStr] := IntToStr(MaxRectangleUndrawTime);

      Values[MenusVisibleStr] := BoolToStr(MenusVisible, True);
      ItemProps[MenusVisibleStr].PickList.Add('True');
      ItemProps[MenusVisibleStr].PickList.Add('False');
      ItemProps[MenusVisibleStr].EditStyle := esPickList;

      Values[MonitorStrayingTrainsStr] := BoolToStr(MonitorStrayingTrains, True);
      ItemProps[MonitorStrayingTrainsStr].PickList.Add('True');
      ItemProps[MonitorStrayingTrainsStr].PickList.Add('False');
      ItemProps[MonitorStrayingTrainsStr].EditStyle := esPickList;

      Values[PointFeedbackMaximumWaitInSecondsStr] := IntToStr(PointFeedbackMaximumWaitInSeconds);

      Values[RouteAheadNotClearWaitTimeInMinutesStr] := IntToStr(RouteAheadNotClearWaitTimeInMinutes);

      Values[RunTestUnitOnStartupStr] := BoolToStr(RunTestUnitOnStartup, True);
      ItemProps[RunTestUnitOnStartup].PickList.Add('True');
      ItemProps[RunTestUnitOnStartup].PickList.Add('False');
      ItemProps[RunTestUnitOnStartup].EditStyle := esPickList;

      Values[ShowCancelledTrainsInDiagramsStr] := BoolToStr(ShowCancelledTrainsInDiagrams, True);
      ItemProps[ShowCancelledTrainsInDiagramsStr].PickList.Add('True');
      ItemProps[ShowCancelledTrainsInDiagramsStr].PickList.Add('False');
      ItemProps[ShowCancelledTrainsInDiagramsStr].EditStyle := esPickList;

      Values[ShowIncorrectDayOfTheWeekEntriesInWorkingTimetableStr] := BoolToStr(ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable, True);
      ItemProps[ShowIncorrectDayOfTheWeekEntriesInWorkingTimetableStr].PickList.Add('True');
      ItemProps[ShowIncorrectDayOfTheWeekEntriesInWorkingTimetableStr].PickList.Add('False');
      ItemProps[ShowIncorrectDayOfTheWeekEntriesInWorkingTimetableStr].EditStyle := esPickList;

      Values[ShowNonMovingTrainsInDiagramsStr] := BoolToStr(ShowNonMovingTrainsInDiagrams, True);
      ItemProps[ShowNonMovingTrainsInDiagramsStr].PickList.Add('True');
      ItemProps[ShowNonMovingTrainsInDiagramsStr].PickList.Add('False');
      ItemProps[ShowNonMovingTrainsInDiagramsStr].EditStyle := esPickList;

      Values[ShowNonStopsInDiagramsStr] := BoolToStr(ShowNonStopsInDiagrams, True);
      ItemProps[ShowNonStopsInDiagramsStr].PickList.Add('True');
      ItemProps[ShowNonStopsInDiagramsStr].PickList.Add('False');
      ItemProps[ShowNonStopsInDiagramsStr].EditStyle := esPickList;

      Values[ShowTrackCircuitsWhereUserMustDriveStr] := BoolToStr(ShowTrackCircuitsWhereUserMustDrive, True);
      ItemProps[ShowTrackCircuitsWhereUserMustDriveStr].PickList.Add('True');
      ItemProps[ShowTrackCircuitsWhereUserMustDriveStr].PickList.Add('False');
      ItemProps[ShowTrackCircuitsWhereUserMustDriveStr].EditStyle := esPickList;

      Values[StartRepeatJourneysOnNewLineInDiagramsStr] := BoolToStr(StartRepeatJourneysOnNewLineInDiagrams, True);
      ItemProps[StartRepeatJourneysOnNewLineInDiagramsStr].PickList.Add('True');
      ItemProps[StartRepeatJourneysOnNewLineInDiagramsStr].PickList.Add('False');
      ItemProps[StartRepeatJourneysOnNewLineInDiagramsStr].EditStyle := esPickList;

      Values[StartWithDiagramsStr] := BoolToStr(StartWithDiagrams, True);
      ItemProps[StartWithDiagramsStr].PickList.Add('True');
      ItemProps[StartWithDiagramsStr].PickList.Add('False');
      ItemProps[StartWithDiagramsStr].EditStyle := esPickList;

      Values[StationEndOfDayPassengerLeavingTimeInMinutesStr] := IntToStr(StationEndOfDayPassengerLeavingTimeInMinutes);

      Values[StationMonitorsWebPageRequiredStr] := BoolToStr(StationMonitorsWebPageRequired, True);
      ItemProps[StationMonitorsWebPageRequiredStr].PickList.Add('True');
      ItemProps[StationMonitorsWebPageRequiredStr].PickList.Add('False');
      ItemProps[StationMonitorsWebPageRequiredStr].EditStyle := esPickList;

      Values[StationOppositeDirectionExitMinimumWaitTimeInMinutesStr] := IntToStr(StationOppositeDirectionExitMinimumWaitTimeInMinutes);

      Values[StationSameDirectionExitMinimumWaitTimeInMinutesStr] := IntToStr(StationSameDirectionExitMinimumWaitTimeInMinutes);

      Values[StationStartModeStr] := BoolToStr(InStationStartMode, True);
      ItemProps[StationStartModeStr].PickList.Add('True');
      ItemProps[StationStartModeStr].PickList.Add('False');
      ItemProps[StationStartModeStr].EditStyle := esPickList;

      Values[StationStartOfDayPassengerBoardingTimeInMinutesStr] := IntToStr(StationStartOfDayPassengerBoardingTimeInMinutes);

      Values[StopAllLocosAtShutDownStr] := BoolToStr(StopAllLocosAtShutDown, True);
      ItemProps[StopAllLocosAtShutDownStr].PickList.Add('True');
      ItemProps[StopAllLocosAtShutDownStr].PickList.Add('False');
      ItemProps[StopAllLocosAtShutDownStr].EditStyle := esPickList;

      Values[SwitchActiveLocoLightsOffAtShutDownStr] := BoolToStr(SwitchActiveLocoLightsOffAtShutDown, True);
      ItemProps[SwitchActiveLocoLightsOffAtShutDownStr].PickList.Add('True');
      ItemProps[SwitchActiveLocoLightsOffAtShutDownStr].PickList.Add('False');
      ItemProps[SwitchActiveLocoLightsOffAtShutDownStr].EditStyle := esPickList;

      Values[TheatreBoxHeightStr] := IntToStr(TheatreBoxHeight);
      Values[TheatreBoxWidthStr] := IntToStr(TheatreBoxWidth);

      Values[UseDisplayLineColoursWindowSettingsOnStartupStr] := BoolToStr(UseDisplayLineColoursWindowSettingsOnStartup, True);

      Values[WaitBeforeRerouteInMinutesStr] := IntToStr(WaitBeforeRerouteInMinutes);

      Values[WorkingTimetableModeStr] := BoolToStr(WorkingTimetableMode, True);
      ItemProps[WorkingTimetableModeStr].PickList.Add('True');
      ItemProps[WorkingTimetableModeStr].PickList.Add('False');
      ItemProps[WorkingTimetableModeStr].EditStyle := esPickList;

      { Add the RailDriver data - one wouldn't expect it to be changed this way, but it's a way of zeroing it if the wrong data has been accidentally acquired }
      Values[RDCStr] := '';
      ItemProps[RDCStr].ReadOnly := True;

      Values[RDCBailOffMinStr] := IntToStr(RDCBailOffMin);
      Values[RDCBailOffMaxStr] := IntToStr(RDCBailOffMax);
      Values[RDCEmergencyBrakeMinStr] := IntToStr(RDCEmergencyBrakeMin);
      Values[RDCEmergencyBrakeMaxStr] := IntToStr(RDCEmergencyBrakeMax);
      Values[RDCLocoBrakeMinStr] := IntToStr(RDCLocoBrakeMin);
      Values[RDCLocoBrakeMaxStr] := IntToStr(RDCLocoBrakeMax);
      Values[RDCRegulatorForwardMinStr] := IntToStr(RDCRegulatorForwardMin);
      Values[RDCRegulatorForwardMaxStr] := IntToStr(RDCRegulatorForwardMax);
      Values[RDCRegulatorReverseMinStr] := IntToStr(RDCRegulatorReverseMin);
      Values[RDCRegulatorReverseMaxStr] := IntToStr(RDCRegulatorReverseMax);
      Values[RDCReverserForwardMinStr] := IntToStr(RDCReverserForwardMin);
      Values[RDCReverserForwardMaxStr] := IntToStr(RDCReverserForwardMax);
      Values[RDCReverserReverseMinStr] := IntToStr(RDCReverserReverseMin);
      Values[RDCReverserReverseMaxStr] := IntToStr(RDCReverserReverseMax);
      Values[RDCTrainBrakeMinStr] := IntToStr(RDCTrainBrakeMin);
      Values[RDCTrainBrakeMaxStr] := IntToStr(RDCTrainBrakeMax);
      Values[RDCThreeWaySwitchALeftNumStr] := IntToStr(RDCThreeWaySwitchALeftNum);
      Values[RDCThreeWaySwitchAMidNumStr] := IntToStr(RDCThreeWaySwitchAMidNum);
      Values[RDCThreeWaySwitchARightNumStr] := IntToStr(RDCThreeWaySwitchARightNum);
      Values[RDCThreeWaySwitchBLeftNumStr] := IntToStr(RDCThreeWaySwitchBLeftNum);
      Values[RDCThreeWaySwitchBMidNumStr] := IntToStr(RDCThreeWaySwitchBMidNum);
      Values[RDCThreeWaySwitchBRightNumStr] := IntToStr(RDCThreeWaySwitchBRightNum);
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG WriteOptionsToValueList: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOptionsToValueList }

PROCEDURE SaveOptionFromValueList(KeyName, NewKeyValue : String);
{ Save values from the options value list }

  FUNCTION ConfirmValueListChange(KeyName, NewValue, OldValue : String) : Boolean;
  { Check that some of the value list changes are really required }
  CONST
    StopTimer = True;

  BEGIN
    Result := False;
    IF (OldValue <> '') AND (NewValue = '') THEN BEGIN
      IF MessageDialogueWithDefault(KeyName + ': do you want to delete ''' + OldValue + '''?',
                                    StopTimer, mtError, [mbYes, mbNo], mbNo) = mrYes
      THEN
        Result := True;
    END ELSE
      IF (OldValue = '') AND (NewValue <> '') THEN BEGIN
        IF MessageDialogueWithDefault(KeyName + ': do you want to insert ''' + NewValue + '''?',
                                      StopTimer, mtError, [mbYes, mbNo], mbNo) = mrYes
        THEN
          Result := True;
      END ELSE
        IF MessageDialogueWithDefault(KeyName + ': do you want to change ''' + OldValue + ''' to ''' + NewValue + '''?',
                                      StopTimer, mtError, [mbYes, mbNo], mbNo) = mrYes
        THEN
          Result := True;
  END; { ConfirmValueListChange }

  PROCEDURE CheckStringValueWithSetValues(KeyNameToTest, OriginalKeyName, NewKeyValue : String; VAR OriginalKeyValue : String; SetValues : String);
  { Enquire if the replacement value in a value list is correct having first checked that it is permitted }
  VAR
    I : Integer;
    ValuesArray : StringArrayType;
    ValueFound : Boolean;

  BEGIN
    WITH OptionsWindow.OptionsValueListEditor DO BEGIN
      IF (KeyNameToTest = OriginalKeyName) AND (NewKeyValue <> OriginalKeyValue) THEN BEGIN
        { Check the confirmed values are allowed }
        ExtractSubStringsFromString(SetValues, ',', ValuesArray);
        I := 0;
        ValueFound := False;
        WHILE (I <= High(ValuesArray)) AND NOT ValueFound DO BEGIN
          IF UpperCase(NewKeyValue) = UpperCase(ValuesArray[I]) THEN BEGIN
            ValueFound := True;
            { ValuesArray[I] is used here as a replacement and not NewKeyValue, as ValuesArray[I] will have the correct case }
            IF NOT ConfirmValueListChange(OriginalKeyName, ValuesArray[I], OriginalKeyValue) THEN
              Values[OriginalKeyName] := OriginalKeyValue
            ELSE BEGIN
              OriginalKeyValue := ValuesArray[I];
              Values[OriginalKeyName] := ValuesArray[I];
            END;
          END;
          Inc(I);
        END; {WHILE}

        IF NOT ValueFound THEN BEGIN
          ShowMessage('' + NewKeyValue + ''' is not allowed (permitted values are: ' + SetValues + ')');
          Values[OriginalKeyName] := OriginalKeyValue;
        END;
      END;
    END; {WITH}
  END; { CheckStringValueWithSetValues }

  PROCEDURE CheckStringValueListValue(KeyNameToTest, OriginalKeyName, NewKeyValue : String; VAR OriginalKeyValue : String);
  { Verify and replace a string value in a value list }
  BEGIN
    WITH OptionsWindow.OptionsValueListEditor DO BEGIN
      IF (KeyNameToTest = OriginalKeyName) AND (NewKeyValue <> OriginalKeyValue) THEN BEGIN
        IF ConfirmValueListChange(OriginalKeyName, NewKeyValue, OriginalKeyValue) THEN
          OriginalKeyValue := NewKeyValue
        ELSE
          Values[OriginalKeyName] := OriginalKeyValue;
      END;
    END; {WITH}
  END; { CheckStringValueListValue }

  PROCEDURE CheckIntegerValueListValue(KeyNameToTest, OriginalKeyName, NewKeyValue : String; VAR OriginalKeyValue : Integer);
  { Verify and replace an integer value in a value list }
  BEGIN
    WITH OptionsWindow.OptionsValueListEditor DO BEGIN
      IF (KeyNameToTest = OriginalKeyName) AND (NewKeyValue <> IntToStr(OriginalKeyValue)) THEN BEGIN
        IF NOT TryStrToInt(NewKeyValue, OriginalKeyValue) THEN BEGIN
          ShowMessage('Invalid integer value ''' + NewKeyValue + '');
          Values[OriginalKeyName] := IntToStr(OriginalKeyValue);
        END;
      END;
    END; {WITH}
  END; { CheckIntegerValueListValue }

  PROCEDURE CheckCardinalValueListValue(KeyNameToTest, OriginalKeyName, NewKeyValue : String; VAR OriginalKeyValue : Cardinal);
  { Verify and replace an cardinal value in a value list }
  VAR
    TempInt : Integer;

  BEGIN
    WITH OptionsWindow.OptionsValueListEditor DO BEGIN
      IF (KeyNameToTest = OriginalKeyName) AND (NewKeyValue <> IntToStr(OriginalKeyValue)) THEN BEGIN
        TempInt := OriginalKeyValue;
        IF NOT TryStrToInt(NewKeyValue, TempInt) THEN BEGIN
          ShowMessage('Invalid cardinal value ''' + NewKeyValue + '');
          Values[OriginalKeyName] := IntToStr(OriginalKeyValue);
        END;
      END;
    END; {WITH}
  END; { CheckCardinalValueListValue }

  PROCEDURE CheckBooleanValueListValue(KeyNameToTest, OriginalKeyName, NewKeyValue : String; VAR OriginalKeyValue : Boolean);
  { Verify and replace a boolean value in a value list }
  BEGIN
    WITH OptionsWindow.OptionsValueListEditor DO BEGIN
      IF (KeyNameToTest = OriginalKeyName) AND (NewKeyValue <> BoolToStr(OriginalKeyValue)) THEN BEGIN
        IF NOT TryStrToBool(NewKeyValue, OriginalKeyValue) THEN BEGIN
          ShowMessage('Invalid boolean value ''' + NewKeyValue + '');
          Values[OriginalKeyName] := BoolToStr(OriginalKeyValue, True);
        END;
      END;
    END; {WITH}
  END; { CheckBooleanValueListValue }

VAR
  TempStationStartMode : Boolean;

BEGIN
  TRY
    WITH OptionsWindow.OptionsValueListEditor DO BEGIN
      { Paths and Directories }
      CheckStringValueListValue(KeyName, PathToLogFilesStr, NewKeyValue, PathToLogFiles);
      CheckStringValueListValue(KeyName, PathToRailDataFilesStr, NewKeyValue, PathToRailDataFiles);

      { Filenames - we make sure that changes to these are confirmed }
      CheckStringValueListValue(KeyName, AreaDataFilenameStr, NewKeyValue, AreaDataFilename);
      CheckStringValueListValue(KeyName, AreaDataFilenameSuffixStr, NewKeyValue, AreaDataFilenameSuffix);
      CheckStringValueListValue(KeyName, DataCheckFileNameStr, NewKeyValue, DataCheckFileName);
      CheckStringValueListValue(KeyName, DiagramsFilenameStr, NewKeyValue, DiagramsFilename);
      CheckStringValueListValue(KeyName, DiagramsFilenameSuffixStr, NewKeyValue, DiagramsFilenameSuffix);
      CheckStringValueListValue(KeyName, FeedbackDataFilenameStr, NewKeyValue, FeedbackDataFilename);
      CheckStringValueListValue(KeyName, FeedbackDataFilenameSuffixStr, NewKeyValue, FeedbackDataFilenameSuffix);
      CheckStringValueListValue(KeyName, LineDataFilenameStr, NewKeyValue, LineDataFilename);
      CheckStringValueListValue(KeyName, LineDataFilenameSuffixStr, NewKeyValue, LineDataFilenameSuffix);
      CheckStringValueListValue(KeyName, LocoDataFilenameStr, NewKeyValue, LocoDataFilename);
      CheckStringValueListValue(KeyName, LocationDataFilenameSuffixStr, NewKeyValue, LocationDataFilenameSuffix);
      CheckStringValueListValue(KeyName, LocationDataFilenameStr, NewKeyValue, LocationDataFilename);
      CheckStringValueListValue(KeyName, LocoDataFilenameSuffixStr, NewKeyValue, LocoDataFilenameSuffix);
      CheckStringValueListValue(KeyName, LogFilenameStr, NewKeyValue, LogFilename);
      CheckStringValueListValue(KeyName, LogFilenameSuffixStr, NewKeyValue, LogFilenameSuffix);
      CheckStringValueListValue(KeyName, PlatformDataFilenameStr, NewKeyValue, PlatformDataFilename);
      CheckStringValueListValue(KeyName, PlatformDataFilenameSuffixStr, NewKeyValue, PlatformDataFilenameSuffix);
      CheckStringValueListValue(KeyName, PointDataFilenameStr, NewKeyValue, PointDataFilename);
      CheckStringValueListValue(KeyName, PointDataFilenameSuffixStr, NewKeyValue, PointDataFilenameSuffix);
      CheckStringValueListValue(KeyName, ReplayFilenameStr, NewKeyValue, ReplayFilename);
      CheckStringValueListValue(KeyName, ReplayFilenameSuffixStr, NewKeyValue, ReplayFilenameSuffix);
      CheckStringValueListValue(KeyName, RouteingExceptionDataFilenameStr, NewKeyValue, RouteingExceptionDataFilename);
      CheckStringValueListValue(KeyName, RouteingExceptionDataFilenameSuffixStr, NewKeyValue, RouteingExceptionDataFilenameSuffix);
      CheckStringValueListValue(KeyName, SignalDataFilenameStr, NewKeyValue, SignalDataFilename);
      CheckStringValueListValue(KeyName, SignalDataFilenameSuffixStr, NewKeyValue, SignalDataFilenameSuffix);
      CheckStringValueListValue(KeyName, TrackCircuitDataFilenameStr, NewKeyValue, TrackCircuitDataFilename);
      CheckStringValueListValue(KeyName, TrackCircuitDataFilenameSuffixStr, NewKeyValue, TrackCircuitDataFilenameSuffix);
      CheckStringValueListValue(KeyName, WorkingTimetableFilenameStr, NewKeyValue, WorkingTimetableFilename);
      CheckStringValueListValue(KeyName, WorkingTimetableFilenameSuffixStr, NewKeyValue, WorkingTimetableFilenameSuffix);

      { Full Screen }
      IF (KeyName = ScreenModeStr) AND (NewKeyValue <> ScreenModeStr) THEN BEGIN
        IF ScreenModeStr = DefaultWindowedScreenStr THEN
          ScreenMode := DefaultWindowedScreenMode
        ELSE
          IF ScreenModeStr = FullScreenStr THEN
            ScreenMode := FullScreenMode
          ELSE
            IF ScreenModeStr = FullScreenWithStatusBarStr THEN
              ScreenMode := FullScreenWithStatusBarMode
            ELSE
              IF ScreenModeStr = CustomWindowedScreenStr THEN
                ScreenMode := CustomWindowedScreenMode;
      END; {CASE}

      { Fonts }
      CheckStringValueListValue(KeyName, RailFontNameStr, NewKeyValue, RailFontName);
      CheckIntegerValueListValue(KeyName, LineFontHeightStr, NewKeyValue, LineFontHeight);
      CheckStringValueListValue(KeyName, LoggingWindowFontNameStr, NewKeyValue, LoggingWindowFontName);
      CheckIntegerValueListValue(KeyName, LoggingWindowFontSizeStr, NewKeyValue, LoggingWindowFontSize);
      CheckIntegerValueListValue(KeyName, FWPRailWindowFontHeightStr, NewKeyValue, FWPRailWindowFontHeight);
      CheckIntegerValueListValue(KeyName, PlatformNumberFontHeightStr, NewKeyValue, PlatformNumberFontHeight);
      CheckStringValueListValue(KeyName, StationMonitorsFontNameStr, NewKeyValue, StationMonitorsFontName);
      CheckIntegerValueListValue(KeyName, StationMonitorsLargeFontHeightStr, NewKeyValue, StationMonitorsLargeFontHeight);
      CheckIntegerValueListValue(KeyName, StationMonitorsSmallFontHeightStr, NewKeyValue, StationMonitorsSmallFontHeight);
      CheckIntegerValueListValue(KeyName, TheatreFontHeightStr, NewKeyValue, TheatreFontHeight);

      { Screen Spacing }
      CheckIntegerValueListValue(KeyName, LogFileMaxWidthInCharsStr, NewKeyValue, LogFileMaxWidthInChars);
      CheckIntegerValueListValue(KeyName, BufferStopVerticalSpacingStr, NewKeyValue, BufferStopVerticalSpacing);
      CheckIntegerValueListValue(KeyName, DeltaPointXStr, NewKeyValue, DeltaPointX);
      CheckIntegerValueListValue(KeyName, IndicatorHorizontalSpacingStr, NewKeyValue, IndicatorHorizontalSpacing);
      CheckIntegerValueListValue(KeyName, IndicatorVerticalSpacingStr, NewKeyValue, IndicatorVerticalSpacing);
      CheckIntegerValueListValue(KeyName, MouseRectangleEdgeVerticalSpacingStr, NewKeyValue, MouseRectangleEdgeVerticalSpacing);
      CheckIntegerValueListValue(KeyName, PlatformEdgeVerticalSpacingStr, NewKeyValue, PlatformEdgeVerticalSpacing);
      CheckIntegerValueListValue(KeyName, PlatformNumberEdgeHorizontalSpacingStr, NewKeyValue, PlatformNumberEdgeHorizontalSpacing);
      CheckIntegerValueListValue(KeyName, PlatformNumberEdgeVerticalSpacingStr, NewKeyValue, PlatformNumberEdgeVerticalSpacing);
      CheckIntegerValueListValue(KeyName, SignalHorizontalSpacingStr, NewKeyValue, SignalHorizontalSpacing);
      CheckIntegerValueListValue(KeyName, SignalRadiusStr, NewKeyValue, SignalRadius);
      CheckIntegerValueListValue(KeyName, SignalSemaphoreHeightStr, NewKeyValue, SignalSemaphoreHeight);
      CheckIntegerValueListValue(KeyName, SignalSemaphoreWidthStr, NewKeyValue, SignalSemaphoreWidth);
      CheckIntegerValueListValue(KeyName, SignalVerticalSpacingStr, NewKeyValue, SignalVerticalSpacing);
      CheckIntegerValueListValue(KeyName, SpeedRestrictionHorizontalSpacingStr, NewKeyValue, SpeedRestrictionHorizontalSpacing);
      CheckIntegerValueListValue(KeyName, SpeedRestrictionVerticalSpacingStr, NewKeyValue, SpeedRestrictionVerticalSpacing);
      CheckIntegerValueListValue(KeyName, TheatreIndicatorHorizontalSpacingStr, NewKeyValue, TheatreIndicatorHorizontalSpacing);
      CheckIntegerValueListValue(KeyName, TheatreIndicatorVerticalSpacingStr, NewKeyValue, TheatreIndicatorVerticalSpacing);
      CheckIntegerValueListValue(KeyName, TRSPlungerLengthStr, NewKeyValue, TRSPlungerLength);
      CheckIntegerValueListValue(KeyName, WindowRowsStr, NewKeyValue, WindowRows);

      { Times }
      IF (KeyName = ProgramStartTimeOptionStr) AND (NewKeyValue <> ProgramStartTimeStr) THEN BEGIN
        IF NOT TimeIsValid(NewKeyValue) THEN BEGIN
          ShowMessage('Invalid program start time: ' + NewKeyValue);
          Values[ProgramStartTimeOptionStr] := ProgramStartTimeStr;
        END ELSE BEGIN
          ProgramStartTime := StrToTime(NewKeyValue);
          IF NOT AutoModeInitiated THEN BEGIN
            SetCurrentRailwayTimeAndDayOfTheWeek(ProgramStartTime);
            Log('AG New Program Start Time of ' + TimeToHMSStr(ProgramStartTime) + ' adopted as Auto Mode has not yet been initiated');
          END;
        END;
      END;

      IF (KeyName = DayLightEndTimeOptionStr) AND (NewKeyValue <> DayLightEndTimeStr) THEN BEGIN
        IF TimeIsValid(NewKeyValue) THEN
          DayLightEndTime := StrToTime(NewKeyValue)
        ELSE BEGIN
          ShowMessage('Invalid day light end time : ' + NewKeyValue);
          Values[DayLightEndTimeOptionStr] := DayLightEndTimeStr;
        END;
      END;

      IF (KeyName = DayLightStartTimeOptionStr) AND (NewKeyValue <> DayLightStartTimeStr) THEN BEGIN
        IF TimeIsValid(NewKeyValue) THEN
          DayLightStartTime := StrToTime(NewKeyValue)
        ELSE BEGIN
          ShowMessage('Invalid day light start time: ' + NewKeyValue);
          Values[DayLightStartTimeOptionStr] := DayLightStartTimeStr;
        END;
      END;

      IF (KeyName = CurrentRailwayDayOfTheWeekStr) AND (NewKeyValue <> DayOfTheWeekToStr(CurrentRailwayDayOfTheWeek)) THEN BEGIN
        IF StrToDayOfTheWeek(NewKeyValue) = UnknownDayOfTheWeek THEN BEGIN
          ShowMessage('Invalid day of the week: ' + NewKeyValue);
          Values[CurrentRailwayDayOfTheWeekStr] := DayOfTheWeekToStr(CurrentRailwayDayOfTheWeek);
        END ELSE
          Values[CurrentRailwayDayOfTheWeekStr] := NewKeyValue;
      END;

      { Other Settings }
      CheckBooleanValueListValue(KeyName, AcceptAllPermanentOccupationsWithoutFeedbackStr, NewKeyValue, AcceptAllPermanentOccupationsWithoutFeedback);
      CheckBooleanValueListValue(KeyName, AutomaticallySetFocusWhenInDebugWindowStr, NewKeyValue, AutomaticallySetFocusWhenInDebugWindow);
      CheckBooleanValueListValue(KeyName, CancelAllTrainsWithNoFeedbackOccupationStr, NewKeyValue, CancelAllTrainsWithNoFeedbackOccupation);
      CheckIntegerValueListValue(KeyName, CarriageLengthInInchesStr, NewKeyValue, CarriageLengthInInches);
      CheckBooleanValueListValue(KeyName, CheckForIdenticalLinesInLogStr, NewKeyValue, CheckForIdenticalLinesInLog);
      CheckBooleanValueListValue(KeyName, DisplayDiagramsStr, NewKeyValue, DisplayDiagrams);
      CheckBooleanValueListValue(KeyName, DisplayFlashingTrackCircuitsStr, NewKeyValue, DisplayFlashingTrackCircuits);
      CheckBooleanValueListValue(KeyName, DisplayLocoChipNumsStr, NewKeyValue, DisplayLocoChipNums);
      CheckBooleanValueListValue(KeyName, DisplayLocoHeadcodesStr, NewKeyValue, DisplayLocoHeadcodes);
      CheckBooleanValueListValue(KeyName, DisplayNotForPublicUseTrainsInStationMonitorsStr, NewKeyValue, DisplayNotForPublicUseTrainsInStationMonitors);
      CheckBooleanValueListValue(KeyName, DisplayRoutesAndJourneysStr, NewKeyValue, DisplayRoutesAndJourneys);
      CheckBooleanValueListValue(KeyName, DisplayWorkingTimetableStr, NewKeyValue, DisplayWorkingTimetable);
      CheckBooleanValueListValue(KeyName, DoNotCancelTrainsWithNoFeedbackOccupationStr, NewKeyValue, DoNotCancelTrainsWithNoFeedbackOccupation);
      CheckBooleanValueListValue(KeyName, HighlightTrackCircuitSpeedRestrictionsStr, NewKeyValue, HighlightTrackCircuitSpeedRestrictions);
      CheckBooleanValueListValue(KeyName, LargeDiagramsWindowSelectedStr, NewKeyValue, LargeDiagramsWindowSelected);
      CheckBooleanValueListValue(KeyName, LargeWorkingTimetableWindowSelectedStr, NewKeyValue, LargeWorkingTimetableWindowSelected);
      CheckStringValueWithSetValues(KeyName, LineThicknessInFullScreenModeStr, NewKeyValue, LineThicknessInFullScreenMode, 'Thin, Thick');
      CheckIntegerValueListValue(KeyName, LocoTimingTimeBeforeAutoStopInSecondsStr, NewKeyValue, LocoTimingTimeBeforeAutoStopInSeconds);
      CheckBooleanValueListValue(KeyName, LogCurrentTimeModeStr, NewKeyValue, LogCurrentTimeMode);
      CheckBooleanValueListValue(KeyName, LogsKeptModeStr, NewKeyValue, LogsKeptMode);
      CheckBooleanValueListValue(KeyName, MakeSoundWhenDebugWindowBoldTextAppearsStr, NewKeyValue, MakeSoundWhenDebugWindowBoldTextAppears);
      CheckCardinalValueListValue(KeyName, MaxRectangleUndrawTimeStr, NewKeyValue, MaxRectangleUndrawTime);
      CheckBooleanValueListValue(KeyName, MenusVisibleStr, NewKeyValue, MenusVisible);
      CheckBooleanValueListValue(KeyName, MonitorStrayingTrainsStr, NewKeyValue, MonitorStrayingTrains);
      CheckIntegerValueListValue(KeyName, PointFeedbackMaximumWaitInSecondsStr, NewKeyValue, PointFeedbackMaximumWaitInSeconds);
      CheckIntegerValueListValue(KeyName, RouteAheadNotClearWaitTimeInMinutesStr, NewKeyValue, RouteAheadNotClearWaitTimeInMinutes);
      CheckBooleanValueListValue(KeyName, RunTestUnitOnStartupStr, NewKeyValue, RunTestUnitOnStartup);
      CheckBooleanValueListValue(KeyName, ShowCancelledTrainsInDiagramsStr, NewKeyValue, ShowCancelledTrainsInDiagrams);
     
      CheckBooleanValueListValue(KeyName, ShowIncorrectDayOfTheWeekEntriesInWorkingTimetableStr, NewKeyValue, ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable);
      CheckBooleanValueListValue(KeyName, ShowNonMovingTrainsInDiagramsStr, NewKeyValue, ShowNonMovingTrainsInDiagrams);
      CheckBooleanValueListValue(KeyName, ShowNonStopsInDiagramsStr, NewKeyValue, ShowNonStopsInDiagrams);
      CheckBooleanValueListValue(KeyName, ShowTrackCircuitsWhereUserMustDriveStr, NewKeyValue, ShowTrackCircuitsWhereUserMustDrive);
      CheckBooleanValueListValue(KeyName, StartRepeatJourneysOnNewLineInDiagramsStr, NewKeyValue, StartRepeatJourneysOnNewLineInDiagrams);
      CheckBooleanValueListValue(KeyName, StartWithDiagramsStr, NewKeyValue, StartWithDiagrams);
      CheckIntegerValueListValue(KeyName, StationEndOfDayPassengerLeavingTimeInMinutesStr, NewKeyValue, StationEndOfDayPassengerLeavingTimeInMinutes);
      CheckBooleanValueListValue(KeyName, StationMonitorsWebPageRequiredStr, NewKeyValue, StationMonitorsWebPageRequired);
      CheckIntegerValueListValue(KeyName, StationOppositeDirectionExitMinimumWaitTimeInMinutesStr, NewKeyValue, StationOppositeDirectionExitMinimumWaitTimeInMinutes);
      CheckIntegerValueListValue(KeyName, StationSameDirectionExitMinimumWaitTimeInMinutesStr, NewKeyValue, StationSameDirectionExitMinimumWaitTimeInMinutes);
      TempStationStartMode := InStationStartMode;
        CheckBooleanValueListValue(KeyName, StationStartModeStr, NewKeyValue, TempStationStartMode);
      CheckIntegerValueListValue(KeyName, StationStartOfDayPassengerBoardingTimeInMinutesStr, NewKeyValue, StationStartOfDayPassengerBoardingTimeInMinutes);
      CheckBooleanValueListValue(KeyName, StopAllLocosAtShutDownStr, NewKeyValue, StopAllLocosAtShutDown);
      CheckBooleanValueListValue(KeyName, SwitchActiveLocoLightsOffAtShutDownStr, NewKeyValue, SwitchActiveLocoLightsOffAtShutDown);
      CheckIntegerValueListValue(KeyName, TheatreBoxHeightStr, NewKeyValue, TheatreBoxHeight);
      CheckIntegerValueListValue(KeyName, TheatreBoxWidthStr, NewKeyValue, TheatreBoxWidth);
      CheckBooleanValueListValue(KeyName, UseDisplayLineColoursWindowSettingsOnStartupStr, NewKeyValue, UseDisplayLineColoursWindowSettingsOnStartup);
      CheckIntegerValueListValue(KeyName, WaitBeforeRerouteInMinutesStr, NewKeyValue, WaitBeforeRerouteInMinutes);
      CheckBooleanValueListValue(KeyName, WorkingTimetableModeStr, NewKeyValue, WorkingTimetableMode);

      { RDC }
      Values[RDCSectionStr] := '';
      ItemProps[RDCSectionStr].ReadOnly := True;

      CheckIntegerValueListValue(KeyName, RDCBailOffMinStr, NewKeyValue, RDCBailOffMin);
      CheckIntegerValueListValue(KeyName, RDCBailOffMaxStr, NewKeyValue, RDCBailOffMax);
      CheckIntegerValueListValue(KeyName, RDCEmergencyBrakeMinStr, NewKeyValue, RDCEmergencyBrakeMin);
      CheckIntegerValueListValue(KeyName, RDCEmergencyBrakeMaxStr, NewKeyValue, RDCEmergencyBrakeMax);
      CheckIntegerValueListValue(KeyName, RDCLocoBrakeMinStr, NewKeyValue, RDCLocoBrakeMin);
      CheckIntegerValueListValue(KeyName, RDCLocoBrakeMaxStr, NewKeyValue, RDCLocoBrakeMax);
      CheckIntegerValueListValue(KeyName, RDCRegulatorForwardMinStr, NewKeyValue, RDCRegulatorForwardMin);
      CheckIntegerValueListValue(KeyName, RDCRegulatorForwardMaxStr, NewKeyValue, RDCRegulatorForwardMax);
      CheckIntegerValueListValue(KeyName, RDCRegulatorReverseMinStr, NewKeyValue, RDCRegulatorReverseMin);
      CheckIntegerValueListValue(KeyName, RDCRegulatorReverseMaxStr, NewKeyValue, RDCRegulatorReverseMax);
      CheckIntegerValueListValue(KeyName, RDCReverserForwardMinStr, NewKeyValue, RDCReverserForwardMin);
      CheckIntegerValueListValue(KeyName, RDCReverserForwardMaxStr, NewKeyValue, RDCReverserForwardMax);
      CheckIntegerValueListValue(KeyName, RDCReverserReverseMinStr, NewKeyValue, RDCReverserReverseMin);
      CheckIntegerValueListValue(KeyName, RDCReverserReverseMaxStr, NewKeyValue, RDCReverserReverseMax);
      CheckIntegerValueListValue(KeyName, RDCTrainBrakeMinStr, NewKeyValue, RDCTrainBrakeMin);
      CheckIntegerValueListValue(KeyName, RDCTrainBrakeMaxStr, NewKeyValue, RDCTrainBrakeMax);
      CheckIntegerValueListValue(KeyName, RDCThreeWaySwitchALeftNumStr, NewKeyValue, RDCThreeWaySwitchALeftNum);
      CheckIntegerValueListValue(KeyName, RDCThreeWaySwitchAMidNumStr, NewKeyValue, RDCThreeWaySwitchAMidNum);
      CheckIntegerValueListValue(KeyName, RDCThreeWaySwitchARightNumStr, NewKeyValue, RDCThreeWaySwitchARightNum);
      CheckIntegerValueListValue(KeyName, RDCThreeWaySwitchBLeftNumStr, NewKeyValue, RDCThreeWaySwitchBLeftNum);
      CheckIntegerValueListValue(KeyName, RDCThreeWaySwitchBMidNumStr, NewKeyValue, RDCThreeWaySwitchBMidNum);
      CheckIntegerValueListValue(KeyName, RDCThreeWaySwitchBRightNumStr, NewKeyValue, RDCThreeWaySwitchBRightNum);
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG SaveOptionFromValueList: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SaveOptionFromValueList }

PROCEDURE TOptionsWindow.OptionsValueListEditorValidate(Sender: TObject; ACol, ARow: Integer; CONST KeyName, KeyValue: String);
BEGIN
  SaveOptionFromValueList(KeyName, KeyValue);
  ReinitialiseFWPRailWindowVariables := True;
  InvalidateScreen(UnitRef, 'OptionsValueListEditorValidate');
END; { OptionsValueListEditorValidate }

PROCEDURE TOptionsWindow.OptionsValueListEditorDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
VAR
  CellText : String;

BEGIN
  AppendToIntegerArray(CellStringStartArray, CellStringCount);
  WITH OptionsWindow.OptionsValueListEditor DO BEGIN
    WITH OptionsWindow.OptionsValueListEditor.Canvas DO BEGIN
      CellText := Cells[ACol, ARow];
      IF Copy(CellText, 1, 3) = '<B>' THEN BEGIN
        Font.Style := [fsBold];
        CellText := Copy(Celltext, 4);
      END;

      { the end bold command is only there for consistency, as the cell can only be written in one style }
      IF Copy(CellText, Length(CellText) - 3, 4) = '<\B>' THEN
        CellText := Copy(CellText, 1, Length(CellText) - 4);

      TextRect(Rect, Rect.Left, Rect.Top, CellText);
    END; {WITH}
  END; {WITH}
  CellStringCount := CellStringCount + Length(CellText);
END; { OptionsWindowValueListEditorDrawCell }

PROCEDURE TOptionsWindow.OptionsWindowShow(Sender: TObject);
BEGIN
  WriteOptionsToValueList;
END; { OptionsWindowShow }

PROCEDURE InitialiseOptionsUnit;
{ Initialises the unit }
BEGIN
  LocoUtilsWindow.Height := LocoUtilsWindowHeight;
  LocoUtilsWindow.Width := LocoUtilsWindowWidth;
  LocoUtilsWindow.Top := LocoUtilsWindowTop;
  LocoUtilsWindow.Left := LocoUtilsWindowLeft;

  OptionsWindow.Height := OptionsWindowHeight;
  OptionsWindow.Width := OptionsWindowWidth;
  OptionsWindow.Top := OptionsWindowTop;
  OptionsWindow.Left := OptionsWindowLeft;

  OptionsWindow.OptionsValueListEditor.ColWidths[0] := OptionsWindowValueListEditorCol0Width;
END; { InitialiseOptionsUnit }

//procedure TForm1.FormClick(Sender: TObject);
//begin
//  FindDialog1.Execute(Handle)
//end;

procedure TOptionsWindow.OptionsWindowCreate(Sender: TObject);
begin
  OptionsWindowFindDialog.Options := [frDown, frHideWholeWord, frHideUpDown];
end;

PROCEDURE TOptionsWindow.OptionsWindowFindDialogFind(Sender: TObject);
var
  CurX, CurY, GridWidth, GridHeight: integer;
  X, Y: integer;
  TargetText: string;
  CellText: string;
  i: integer;
  GridRect: TGridRect;
label
  TheEnd;
begin
  CurX := OptionsValueListEditor.Selection.Left + 1;
  CurY := OptionsValueListEditor.Selection.Top;
  GridWidth := OptionsValueListEditor.ColCount;
  GridHeight := OptionsValueListEditor.RowCount;
  Y := CurY;
  X := CurX;
  if frMatchCase in OptionsWindowFindDialog.Options then
    TargetText := OptionsWindowFindDialog.FindText
  else
    TargetText := AnsiLowerCase(OptionsWindowFindDialog.FindText);
  while Y < GridHeight do
  begin
    while X < GridWidth do
    begin
      if frMatchCase in OptionsWindowFindDialog.Options then
        CellText := OptionsValueListEditor.Cells[X, Y]
      else
        CellText := AnsiLowerCase(OptionsValueListEditor.Cells[X, Y]);
      i := Pos(TargetText, CellText) ;
      if i > 0 then
      begin
        GridRect.Left := X;
        GridRect.Right := X;
        GridRect.Top := Y;
        GridRect.Bottom := Y;
        OptionsValueListEditor.Selection := GridRect;
        goto TheEnd;
      end;
      inc(X);
    end;
    inc(Y);
    X := OptionsValueListEditor.FixedCols;
  end;
TheEnd:
END; { OptionsWindowFindDialogFind }

PROCEDURE SearchOptionsText(S : String);
{ Search the Options Window text }
BEGIN
  PreviousFoundPos := 0;
  WITH OptionsWindow DO BEGIN
//    OptionsWindowFindDialog.Position := Point(OptionsWindow.Left + (OptionsWindow.Width DIV 2), OptionsWindow.Top);
    OptionsWindowFindDialog.Execute;
  END; {WITH}
END; { SearchOptionsText }

PROCEDURE TOptionsWindow.OptionsValueListEditorKeyDown(Sender: TObject; VAR Key: Word; Shift: TShiftState);
VAR
  S : String;
BEGIN
  IF Key = vk_Escape THEN BEGIN
    IF OptionsWindowFindDialogActive THEN
      OptionsWindowFindDialog.CloseDialog
    ELSE
      OptionsWindow.Hide;
  END ELSE BEGIN
    IF ((Key = Ord('F')) OR (Key = Ord('f'))) AND (ssCtrl IN Shift) THEN BEGIN
      S := Chr(Key);
      SearchOptionsText(S);
    END;
  END;
END; { OptionsValueListEditorKeyDown }

PROCEDURE TOptionsWindow.OptionsWindowFindDialogShow(Sender: TObject);
BEGIN
  OptionsWindowFindDialogActive := True;
END; { OptionsWindowFindDialogShow }

PROCEDURE TOptionsWindow.OptionsWindowFindDialogClose(Sender: TObject);
BEGIN
  OptionsWindowFindDialogActive := False;
END; { OptionsWindowFindDialogClose }

END { Options }.
