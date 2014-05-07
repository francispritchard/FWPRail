UNIT Options;
{ Allows options to be read in and writen to the Registry and to be changed bny means of a Value List Editor.

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
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE InitialiseOptionsUnit;
{ Initialises the unit }

PROCEDURE ReadIniFile;
{ Read in data from the .ini file or from the Registry, except for the trackcircuit data }

PROCEDURE ReadIniFileForTrackCircuitData;
{ Read in trackcircuit data from the .ini file or from the Registry }

PROCEDURE WriteIniFile;
{ Write out data to the .ini file or to the Registry }

VAR
  OptionsWindow: TOptionsWindow;

  { Windows variables first - these are all initialised in InitialiseInitVarsUnit }
  DefaultCreateRouteDisplayColoursWindowHeight : Integer;
  CreateRouteDisplayColoursWindowHeight : Integer;

  DefaultCreateRouteDisplayColoursWindowTop : Integer;
  CreateRouteDisplayColoursWindowTop : Integer;

  DefaultCreateRouteDisplayColoursWindowLeft : Integer;
  CreateRouteDisplayColoursWindowLeft : Integer;

  DefaultCreateRouteDisplayColoursWindowWidth : Integer;
  CreateRouteDisplayColoursWindowWidth : Integer;

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

  DefaultDayLightStartTimeStr : String = '08:00:00';
  DayLightStartTimeStr : String = '08:00:00';
  DayLightStartTime : TDateTime;

  DefaultDayLightEndTimeStr : String = '17:59:59';
  DayLightEndTimeStr : String = '17:59:59';
  DayLightEndTime : TDateTime;

  DefaultDeltaPointX : Integer = 150;
  DeltaPointX : Integer;
  DeltaPointXSpaced : Integer;

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

  DefaultIncludeLocoChipInStationMonitors : Boolean = False;
  IncludeLocoChipInStationMonitors : Boolean;

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

  DefaultLenzPointNumberColour : TColour = clLime;
  LenzPointNumberColour : TColour = clLime;

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

  DefaultRailFontName : String = 'Arial';
  RailFontName : String;

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

  DefaultPathToRailDataFiles : String = 'C:\Doc\Google Drive\RAD Studio\Projects\Rail Data Files\';
  PathToRailDataFiles : String;

  DefaultPathToLogFiles : String = 'C:\Doc\Google Drive\RAD Studio\Projects\Rail Data Files\Logs\';
  PathToLogFiles : String;

  DefaultPathToLocoLogFiles : String = 'C:\Doc\Google Drive\RAD Studio\Projects\Rail Data Files\LocoLogs\';
  PathToLocoLogFiles : String;

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

  DefaultSignalSemaphoreWidth : Integer = 50;
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
  StationStartMode : Boolean;

  DefaultStationStartOfDayPassengerBoardingTimeInMinutes : Integer = 4;
  StationStartOfDayPassengerBoardingTimeInMinutes : Integer = 4;

  DefaultStationMonitorsDataFilename : String = 'StationMonitorsData';
  StationMonitorsDataFilename : String;

  DefaultStationMonitorsDataFilenameSuffix : String = 'csv';
  StationMonitorsDataFilenameSuffix : String;

  DefaultStationMonitorsFontName : String = 'Arial';
  StationMonitorsFontName : String;

  DefaultStationMonitorsLargeFontHeight : Integer = 48;
  StationMonitorsLargeFontHeight : Integer;

  DefaultStationMonitorsSmallFontHeight : Integer = 36;
  StationMonitorsSmallFontHeight : Integer;

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

  DefaultWaitBeforeRerouteInMinutes : Integer = 5;
  WaitBeforeRerouteInMinutes : Integer;

  DefaultWorkingTimetableFilename : String = 'WorkingTimetable';
  WorkingTimetableFilename : String;

  DefaultWorkingTimetableFilenameSuffix : String = 'mdb';
  WorkingTimetableFilenameSuffix : String;

  DefaultWorkingTimetableMode : Boolean = True;
  WorkingTimetableMode : Boolean;

  DefaultWorkingTimetableWindowGridBackgroundColour : TColor = clMenu;
  WorkingTimetableWindowGridBackgroundColour : TColour = clMenu;

  DefaultWritingStationMonitorsDisplayToFile : Boolean = False;
  WritingStationMonitorsDisplayToFile : Boolean;

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

USES MiscUtils, Raildraw, Locks, LocoUtils, CreateRoute, Diagrams, GetTime, Help, LocationData, Edit, WorkingTimetable, LocoDialogue, Logging;

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
    LenzPointNumberColourStr = 'Lenz Point Number Colour';
    PointColourStr = 'Point Colour';
    PointDivergingLineColourStr = 'Point Diverging Line Colour';
    PointDownFacingColourStr = 'Point Down Facing Colour';
    PointFeedbackDataInUseColourStr = 'Point Feedback Data In Use Colour';
    PointFeedbackDataOutOfUseColourStr = 'Point Feedback Data Out Of Use Colour';
    PointHeelLineColourStr = 'Point Heel Line Colour';
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

    LineRoutedOverColourStr = 'Line Routed Over Colour';

    { Colours for platforms }
    PlatformColourStr = 'Platform Colour';
    PlatformNumberColourStr = 'Platform Number Colour';

    { Colours for trackcircuits }
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
    LineNotAvailableColourStr = 'Line Not Available Colour';
    LinesWithoutTrackCircuitsColourStr = 'Lines Without Trackcircuits Colour';
    LocoStalledColourStr = 'Loco Stalled Colour';
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
    TrackCircuitDialogueBoxLeftStr = 'Track Circuit Dialogue Box Left';
    TrackCircuitDialogueBoxTopStr = 'Track Circuit Dialogue Box Top';

  FilesSectionStr = 'Files';
    AreaDataFilenameStr = 'Area Data Filename';
    AreaDataFilenameSuffixStr = 'Area Data Filename Suffix';
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
    StationMonitorsDataFilenameStr = 'Station Monitors Data Filename';
    StationMonitorsDataFilenameSuffixStr = 'Station Monitors Data Filename Suffix';
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
    DisplayFlashingTrackCircuitsStr = 'Display Flashing Trackcircuits';
    DisplayLocoChipNumsStr = 'Display Loco Chip Numbers';
    DisplayLocoHeadcodesStr = 'Display Loco Headcodes';
    DisplayNotForPublicUseTrainsInStationMonitorsStr = 'Display Not For Public Use Trains In Station Monitors';
    DisplayRoutesAndJourneysStr = 'Display Journeys & Routes';
    DisplayWorkingTimetableStr = 'Display Working Timetable';
    DoNotCancelTrainsWithNoFeedbackOccupationStr = 'Do Not Cancel Trains With No Feedback Occupation';
    HighlightTrackCircuitSpeedRestrictionsStr = 'Highlight Track Circuit Speed Restrictions';
    IncludeLocoChipInStationMonitorsStr = 'Include LocoChip In Station Monitors';
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
    StationOppositeDirectionExitMinimumWaitTimeInMinutesStr = 'Station Opposite Direction Exit Minimum Wait Time In Minutes';
    StationSameDirectionExitMinimumWaitTimeInMinutesStr = 'Station Same Direction Exit Minimum Wait Time In Minutes';
    StationStartModeStr = 'Station Start Mode';
    StationStartOfDayPassengerBoardingTimeInMinutesStr = 'Station Start Of Day Passenger Boarding Time In Minutes';
    StopAllLocosAtShutDownStr = 'Stop All Locos At Shut Down';
    SwitchActiveLocoLightsOffAtShutDownStr = 'Switch Active Loco Lights Off At Shut Down';
    TheatreBoxHeightStr = 'Theatre Box Height';
    TheatreBoxWidthStr = 'Theatre Box Width';
    WaitBeforeRerouteInMinutesStr = 'Wait Before Reroute In Minutes';
    WorkingTimetableModeStr = 'WorkingTimetable Mode';
    WritingStationMonitorsDisplayToFileStr = 'Writing Station Monitors Display To File';

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

    CreateRouteDisplayColoursWindowHeightStr = 'CreateRoute DisplayColours Window Height';
    CreateRouteDisplayColoursWindowLeftStr = 'CreateRoute DisplayColours Window Left';
    CreateRouteDisplayColoursWindowTopStr = 'CreateRoute DisplayColours Window Top';
    CreateRouteDisplayColoursWindowWidthStr = 'CreateRoute DisplayColours Window Width';

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

PROCEDURE ReadIniFile; //MainProcedure(IniFile : TRegistryIniFile);
{ Read in data from the .ini file or from the Registry, except for the trackcircuit data }
VAR
  IniFile : TRegistryIniFile;
  TempStr : String;

BEGIN
  TRY
//    IniFile := TRegistryIniFile.Create(ExtractFilePath(Application.ExeName) + 'Rail.ini');
    IniFile := TRegistryIniFile.Create('FWPRail');

    WITH IniFile DO BEGIN
      { Check for various user supplied file data.
        NB TrackCircuit data is read in separately in the ReadIniFileForTrackCircuitData routine.
      }
      PathToLogFiles := ReadString(FilesSectionStr, PathToLogFilesStr, DefaultPathToLogFiles);
      PathToRailDataFiles := ReadString(FilesSectionStr, PathToRailDataFilesStr, DefaultPathToRailDataFiles);
      AreaDataFilename := ReadString(FilesSectionStr, AreaDataFilenameStr, DefaultAreaDataFilename);
      AreaDataFilenameSuffix := ReadString(FilesSectionStr, AreaDataFilenameSuffixStr, DefaultAreaDataFilenameSuffix);
      DiagramsFilename := ReadString(FilesSectionStr, DiagramsFilenameStr, DefaultDiagramsFilename);
      DiagramsFilenameSuffix := ReadString(FilesSectionStr, DiagramsFilenameSuffixStr, DefaultDiagramsFilenameSuffix);
      FeedbackDataFilename := ReadString(FilesSectionStr, FeedbackDataFilenameStr, DefaultFeedbackDataFilename);
      FeedbackDataFilenameSuffix := ReadString(FilesSectionStr, FeedbackDataFilenameSuffixStr, DefaultFeedbackDataFilenameSuffix);
      LocationDataFilename := ReadString(FilesSectionStr, LocationDataFilenameStr, DefaultLocationDataFilename);
      LocationDataFilenameSuffix := ReadString(FilesSectionStr, LocationDataFilenameSuffixStr, DefaultLocationDataFilenameSuffix);
      LocoDataFilename := ReadString(FilesSectionStr, LocoDataFilenameStr, DefaultLocoDataFilename);
      LocoDataFilenameSuffix := ReadString(FilesSectionStr, LocoDataFilenameSuffixStr, DefaultLocoDataFilenameSuffix);
      LineDataFilename := ReadString(FilesSectionStr, LineDataFilenameStr, DefaultLineDataFilename);
      LineDataFilenameSuffix := ReadString(FilesSectionStr, LineDataFilenameSuffixStr, DefaultLineDataFilenameSuffix);
      LogFilename := ReadString(FilesSectionStr, LogFilenameStr, DefaultLogFilename);
      LogFilenameSuffix := ReadString(FilesSectionStr, LogFilenameSuffixStr, DefaultLogFilenameSuffix);
      PlatformDataFilename := ReadString(FilesSectionStr, PlatformDataFilenameStr, DefaultPlatformDataFilename);
      PlatformDataFilenameSuffix := ReadString(FilesSectionStr, PlatformDataFilenameSuffixStr, DefaultPlatformDataFilenameSuffix);
      PointDataFilename := ReadString(FilesSectionStr, PointDataFilenameStr, DefaultPointDataFilename);
      PointDataFilenameSuffix := ReadString(FilesSectionStr, PointDataFilenameSuffixStr, DefaultPointDataFilenameSuffix);
      ReplayFilename := ReadString(FilesSectionStr, ReplayFilenameStr, DefaultReplayFilename);
      ReplayFilenameSuffix := ReadString(FilesSectionStr, ReplayFilenameSuffixStr, DefaultReplayFilenameSuffix);
      RouteingExceptionDataFilename := ReadString(FilesSectionStr, RouteingExceptionDataFilenameStr, DefaultRouteingExceptionDataFilename);
      RouteingExceptionDataFilenameSuffix := ReadString(FilesSectionStr, RouteingExceptionDataFilenameSuffixStr, DefaultRouteingExceptionDataFilenameSuffix);
      SignalDataFilename := ReadString(FilesSectionStr, SignalDataFilenameStr, DefaultSignalDataFilename);
      StationMonitorsDataFilename := ReadString(FilesSectionStr, StationMonitorsDataFilenameStr, DefaultStationMonitorsDataFilename);
      SignalDataFilenameSuffix := ReadString(FilesSectionStr, SignalDataFilenameSuffixStr, DefaultSignalDataFilenameSuffix);
      StationMonitorsDataFilenameSuffix := ReadString(FilesSectionStr, StationMonitorsDataFilenameSuffixStr, DefaultStationMonitorsDataFilenameSuffix);
      TrackCircuitDataFilename := ReadString(FilesSectionStr, TrackCircuitDataFilename, DefaultTrackCircuitDataFilename);
      TrackCircuitDataFilenameSuffix := ReadString(FilesSectionStr, TrackCircuitDataFilenameSuffixStr, DefaultTrackCircuitDataFilenameSuffix);
      WorkingTimetableFilename := ReadString(FilesSectionStr, WorkingTimetableFilename, DefaultWorkingTimetableFilename);
      WorkingTimetableFilenameSuffix := ReadString(FilesSectionStr, WorkingTimetableFilenameSuffixStr, DefaultWorkingTimetableFilenameSuffix);
      { Colours for buffer stops }
      BufferStopColour := StrToColour(ReadString(ColoursSectionStr, BufferStopColourStr, ColourToStr(DefaultBufferStopColour)));
      BufferStopNumberColour := StrToColour(ReadString(ColoursSectionStr, BufferStopNumberColourStr, ColourToStr(DefaultBufferStopNumberColour)));
      BufferStopRed := StrToColour(ReadString(ColoursSectionStr, BufferStopRedStr, ColourToStr(DefaultBufferStopRed)));

      { Colours for TRS plungers }
      TRSPlungerColour := StrToColour(ReadString(ColoursSectionStr, TRSPlungerColourStr, ColourToStr(DefaultTRSPlungerColour)));
      TRSPlungerOutlineColour := StrToColour(ReadString(ColoursSectionStr, TRSPlungerOutlineColourStr, ColourToStr(DefaultTRSPlungerOutlineColour)));
      TRSPlungerPressedColour := StrToColour(ReadString(ColoursSectionStr, TRSPlungerPressedColourStr, ColourToStr(DefaultTRSPlungerPressedColour)));
      { Colours for platforms }
      PlatformColour := StrToColour(ReadString(ColoursSectionStr, PlatformColourStr, ColourToStr(DefaultPlatformColour)));
      PlatformNumberColour := StrToColour(ReadString(ColoursSectionStr, PlatformNumberColourStr, ColourToStr(DefaultPlatformNumberColour)));
      { Colours for points }
      LenzPointNumberColour := StrToColour(ReadString(ColoursSectionStr, LenzPointNumberColourStr, ColourToStr(DefaultLenzPointNumberColour)));
      PointColour := StrToColour(ReadString(ColoursSectionStr, PointColourStr, ColourToStr(DefaultPointColour)));
      PointDivergingLineColour := StrToColour(ReadString(ColoursSectionStr, PointDivergingLineColourStr, ColourToStr(DefaultPointDivergingLineColour)));
      PointDownFacingColour := StrToColour(ReadString(ColoursSectionStr, PointDownFacingColourStr, ColourToStr(DefaultPointDownFacingColour)));
      PointFeedbackDataInUseColour := StrToColour(ReadString(ColoursSectionStr, PointFeedbackDataInUseColourStr, ColourToStr(DefaultPointFeedbackDataInUseColour)));
      PointFeedbackDataOutOfUseColour := StrToColour(ReadString(ColoursSectionStr, PointFeedbackDataoutOfUseColourStr,
                                                                                                                      ColourToStr(DefaultPointFeedbackDataOutOfUseColour)));
      PointHeelLineColour := StrToColour(ReadString(ColoursSectionStr, PointHeelLineColourStr, ColourToStr(DefaultPointHeelLineColour)));
      PointLockedByUserColour := StrToColour(ReadString(ColoursSectionStr, PointLockedByUserColourStr, ColourToStr(DefaultPointLockedByUserColour)));
      PointManualOperationColour := StrToColour(ReadString(ColoursSectionStr, PointManualOperationColourStr, ColourToStr(DefaultPointManualOperationColour)));
      PointOutOfUseColour := StrToColour(ReadString(ColoursSectionStr, PointOutOfUseColourStr, ColourToStr(DefaultPointOutOfUseColour)));
      PointStraightLineColour := StrToColour(ReadString(ColoursSectionStr, PointStraightLineColourStr, ColourToStr(DefaultPointStraightLineColour)));
      PointsWithoutFeedbackColour := StrToColour(ReadString(ColoursSectionStr, PointsWithoutFeedbackColourStr, ColourToStr(DefaultPointsWithoutFeedbackColour)));
      PointUndrawColour := StrToColour(ReadString(ColoursSectionStr, PointUndrawColourStr, ColourToStr(DefaultPointUndrawColour)));
      PointUpFacingColour := StrToColour(ReadString(ColoursSectionStr, PointUpFacingColourStr, ColourToStr(DefaultPointUpFacingColour)));
      ShowPointDefaultStateColour := StrToColour(ReadString(ColoursSectionStr, ShowPointDefaultStateColourStr, ColourToStr(DefaultShowPointDefaultStateColour)));
      ShowPointLockedColour := StrToColour(ReadString(ColoursSectionStr, ShowPointLockedColourStr, ColourToStr(DefaultShowPointLockedColour)));
      { Colours for signal posts }
      SignalPostColour := StrToColour(ReadString(ColoursSectionStr, SignalPostColourStr, ColourToStr(DefaultSignalPostColour)));
      SignalPostEmergencyRouteSettingColour := StrToColour(ReadString(ColoursSectionStr, SignalPostEmergencyRouteSettingColourStr,
                                                                                                                ColourToStr(DefaultSignalPostEmergencyRouteSettingColour)));
      SignalPostRouteSettingColour := StrToColour(ReadString(ColoursSectionStr, SignalPostRouteSettingColourStr, ColourToStr(DefaultSignalPostRouteSettingColour)));
      SignalPostStationStartModeColour := StrToColour(ReadString(ColoursSectionStr, SignalPostStationStartModeColourStr,
                                                                                                                     ColourToStr(DefaultSignalPostStationStartModeColour)));
      SignalPostTheatreSettingColour := StrToColour(ReadString(ColoursSectionStr, SignalPostTheatreSettingColourStr, ColourToStr(DefaultSignalPostTheatreSettingColour)));
      { Colours for signals }
      SignalAspectGreen := StrToColour(ReadString(ColoursSectionStr, SignalAspectGreenStr, ColourToStr(DefaultSignalAspectGreen)));
      SignalAspectRed := StrToColour(ReadString(ColoursSectionStr, SignalAspectRedStr, ColourToStr(DefaultSignalAspectRed)));
      SignalAspectUnlit := StrToColour(ReadString(ColoursSectionStr, SignalAspectUnlitStr, ColourToStr(DefaultSignalAspectUnlit)));
      SignalAspectYellow := StrToColour(ReadString(ColoursSectionStr, SignalAspectYellowStr, ColourToStr(DefaultSignalAspectYellow)));
      SignalNumberColour := StrToColour(ReadString(ColoursSectionStr, SignalNumberColourStr, ColourToStr(DefaultSignalNumberColour)));

      { Colours for lines }
      LineRoutedOverColour := StrToColour(ReadString(ColoursSectionStr, LineRoutedOverColourStr, ColourToStr(DefaultLineRoutedOverColour)));
      { Colours for trackcircuits }
      TCFeedbackDataInUseColour := StrToColour(ReadString(ColoursSectionStr, TCFeedbackDataInUseColourStr, ColourToStr(DefaultTCFeedbackDataInUseColour)));
      TCFeedbackDataOutOfUseColour := StrToColour(ReadString(ColoursSectionStr, TCFeedbackDataOutOfUseColourStr, ColourToStr(DefaultTCFeedbackDataOutOfUseColour)));
      TCFeedbackOccupationColour := StrToColour(ReadString(ColoursSectionStr, TCFeedbackOccupationColourStr, ColourToStr(DefaultTCFeedbackOccupationColour)));
      TCFeedbackOccupationButOutOfUseColour := StrToColour(ReadString(ColoursSectionStr, TCFeedbackOccupationButOutOfUseColourStr,
                                                                                                                ColourToStr(DefaultTCFeedbackOccupationButOutOfUseColour)));
      TCLocoOutOfPlaceOccupationColour := StrToColour(ReadString(ColoursSectionStr, TCLocoOutOfPlaceOccupationColourStr,
                                                                                                                     ColourToStr(DefaultTCLocoOutOfPlaceOccupationColour)));
      TCMissingOccupationColour := StrToColour(ReadString(ColoursSectionStr, TCMissingOccupationColourStr, ColourToStr(DefaultTCMissingOccupationColour)));
      TCOutOfUseSetByUserColour := StrToColour(ReadString(ColoursSectionStr, TCOutOfUseSetByUserColourStr, ColourToStr(DefaultTCOutOfUseSetByUserColour)));
      TCOutOfUseAsNoFeedbackReceivedColour := StrToColour(ReadString(ColoursSectionStr, TCOutOfUseAsNoFeedbackReceivedColourStr,
                                                                                                                 ColourToStr(DefaultTCOutOfUseAsNoFeedbackReceivedColour)));

      TCPermanentFeedbackOccupationColour := StrToColour(ReadString(ColoursSectionStr, TCPermanentFeedbackOccupationColourStr,
                                                                                                                  ColourToStr(DefaultTCPermanentFeedbackOccupationColour)));
      TCPermanentOccupationSetByUserColour := StrToColour(ReadString(ColoursSectionStr, TCPermanentOccupationSetByUserColourStr,
                                                                                                                 ColourToStr(DefaultTCPermanentOccupationSetByUserColour)));
      TCPermanentSystemOccupationColour := StrToColour(ReadString(ColoursSectionStr, TCPermanentSystemOccupationColourStr,
                                                                                                                    ColourToStr(DefaultTCPermanentSystemOccupationColour)));
      TCSpeedRestrictionColour := StrToColour(ReadString(ColoursSectionStr, TCSpeedRestrictionColourStr, ColourToStr(DefaultTCSpeedRestrictionColour)));
      TCSystemOccupationColour := StrToColour(ReadString(ColoursSectionStr, TCSystemOccupationColourStr, ColourToStr(DefaultTCSystemOccupationColour)));
      TCUnoccupiedColour := StrToColour(ReadString(ColoursSectionStr, TCUnoccupiedColourStr, ColourToStr(DefaultTCUnoccupiedColour)));
      TrainActiveColour := StrToColour(ReadString(ColoursSectionStr, TrainActiveColourStr, ColourToStr(DefaultTrainActiveColour)));
      TrainInactiveColour := StrToColour(ReadString(ColoursSectionStr, TrainInactiveColourStr, ColourToStr(DefaultTrainInactiveColour)));
      { Miscellaneous colours }
      BackgroundColour := StrToColour(ReadString(ColoursSectionStr, BackgroundColourStr, ColourToStr(DefaultBackgroundColour)));
      ForegroundColour := StrToColour(ReadString(ColoursSectionStr, ForegroundColourStr, ColourToStr(DefaultForegroundColour)));
      DiagramsWindowGridBackgroundColour := StrToColour(ReadString(ColoursSectionStr, DiagramsWindowGridBackgroundColourStr,
                                                                                                                   ColourToStr(DefaultDiagramsWindowGridBackgroundColour)));
      LineNotAvailableColour := StrToColour(ReadString(ColoursSectionStr, LineNotAvailableColourStr, ColourToStr(DefaultLineNotAvailableColour)));
      LinesWithoutTrackCircuitsColour := StrToColour(ReadString(ColoursSectionStr, LinesWithoutTrackCircuitsColourStr,
                                                                                                                      ColourToStr(DefaultLinesWithoutTrackCircuitsColour)));
      LocoStalledColour := StrToColour(ReadString(ColoursSectionStr, LocoStalledColourStr, ColourToStr(DefaultLocoStalledColour)));
      WorkingTimetableWindowGridBackgroundColour := StrToColour(ReadString(ColoursSectionStr, WorkingTimetableWindowGridBackgroundColourStr,
                                                                                                           ColourToStr(DefaultWorkingTimetableWindowGridBackgroundColour)));
      { Pen styles }
      FiddleyardLinePenStyle := StrToPenStyle(ReadString(PenStylesSectionStr, FiddleyardLinePenStyleStr, PenStyleToStr(DefaultFiddleyardLinePenStyle)));
      ProjectedLinePenStyle := StrToPenStyle(ReadString(PenStylesSectionStr, ProjectedLinePenStyleStr, PenStyleToStr(DefaultProjectedLinePenStyle)));
      SidingPenStyle := StrToPenStyle(ReadString(PenStylesSectionStr, SidingPenStyleStr, PenStyleToStr(DefaultSidingPenStyle)));
      TCLocoOutOfPlaceOccupationPenStyle := StrToPenStyle(ReadString(PenStylesSectionStr, TCLocoOutOfPlaceOccupationPenStyleStr,
                                                                                                                 PenStyleToStr(DefaultTCLocoOutOfPlaceOccupationPenStyle)));
      TCOutOfUseAsNoFeedbackReceivedPenStyle := StrToPenStyle(ReadString(PenStylesSectionStr, TCOutOfUseAsNoFeedbackReceivedPenStyleStr,
                                                                                                             PenStyleToStr(DefaultTCOutOfUseAsNoFeedbackReceivedPenStyle)));
      TCOutOfUseSetByUserPenStyle := StrToPenStyle(ReadString(PenStylesSectionStr, TCOutOfUseSetByUserPenStyleStr, PenStyleToStr(DefaultTCOutOfUseSetByUserPenStyle)));
      TCPermanentFeedbackOccupationPenStyle := StrToPenStyle(ReadString(PenStylesSectionStr, TCPermanentFeedbackOccupationPenStyleStr,
                                                                                                              PenStyleToStr(DefaultTCPermanentFeedbackOccupationPenStyle)));
      TCPermanentOccupationSetByUserPenStyle := StrToPenStyle(ReadString(PenStylesSectionStr, TCPermanentOccupationSetByUserPenStyleStr,
                                                                                                             PenStyleToStr(DefaultTCPermanentOccupationSetByUserPenStyle)));
      TCPermanentSystemOccupationPenStyle := StrToPenStyle(ReadString(PenStylesSectionStr, TCPermanentSystemOccupationPenStyleStr,
                                                                                                                PenStyleToStr(DefaultTCPermanentSystemOccupationPenStyle)));
      { Fonts }
      RailFontName := ReadString(FontsSectionStr, RailFontNameStr, DefaultRailFontName);
      LineFontHeight := ReadInteger(FontsSectionStr, LineFontHeightStr, DefaultLineFontHeight);
      LoggingWindowFontName := ReadString(FontsSectionStr, LoggingWindowFontNameStr, DefaultLoggingWindowFontName);
      LoggingWindowFontSize := ReadInteger(FontsSectionStr, LoggingWindowFontSizeStr, DefaultLoggingWindowFontSize);
      FWPRailWindowFontHeight := ReadInteger(FontsSectionStr, FWPRailWindowFontHeightStr, DefaultFWPRailWindowFontHeight);
      PlatformNumberFontHeight := ReadInteger(FontsSectionStr, PlatformNumberFontHeightStr, DefaultPlatformNumberFontHeight);
      StationMonitorsFontName := ReadString(FontsSectionStr, StationMonitorsFontNameStr, DefaultStationMonitorsFontName);
      StationMonitorsLargeFontHeight := ReadInteger(FontsSectionStr, StationMonitorsLargeFontHeightStr, DefaultStationMonitorsLargeFontHeight);
      StationMonitorsSmallFontHeight := ReadInteger(FontsSectionStr, StationMonitorsSmallFontHeightStr, DefaultStationMonitorsSmallFontHeight);
      TheatreFontHeight := ReadInteger(FontsSectionStr, TheatreFontHeightStr, DefaultTheatreFontHeight);

      { Dialogue box variables }
      DebuggingOptionsWindowLeft := ReadInteger(DialogueBoxSectionStr, DebuggingOptionsWindowLeftStr, DefaultDebuggingOptionsWindowLeft);
      DebuggingOptionsWindowTop := ReadInteger(DialogueBoxSectionStr, DebuggingOptionsWindowTopStr, DefaultDebuggingOptionsWindowTop);
      LineDialogueBoxLeft := ReadInteger(DialogueBoxSectionStr, LineDialogueBoxLeftStr, DefaultLineDialogueBoxLeft);
      LineDialogueBoxTop := ReadInteger(DialogueBoxSectionStr, LineDialogueBoxTopStr, DefaultLineDialogueBoxTop);
      LocoDialogueWindowLeft := ReadInteger(DialogueBoxSectionStr, LocoDialogueWindowLeftStr, DefaultLocoDialogueWindowLeft);
      LocoDialogueWindowTop := ReadInteger(DialogueBoxSectionStr, LocoDialogueWindowTopStr, DefaultLocoDialogueWindowTop);
      PointDialogueBoxLeft := ReadInteger(DialogueBoxSectionStr, PointDialogueBoxLeftStr, DefaultPointDialogueBoxLeft);
      PointDialogueBoxTop := ReadInteger(DialogueBoxSectionStr, PointDialogueBoxTopStr, DefaultPointDialogueBoxTop);
      SignalDialogueBoxLeft := ReadInteger(DialogueBoxSectionStr, SignalDialogueBoxLeftStr, DefaultSignalDialogueBoxLeft);
      SignalDialogueBoxTop := ReadInteger(DialogueBoxSectionStr, SignalDialogueBoxTopStr, DefaultSignalDialogueBoxTop);
      TrackCircuitDialogueBoxLeft := ReadInteger(DialogueBoxSectionStr, TrackCircuitDialogueBoxLeftStr, DefaultTrackCircuitDialogueBoxLeft);
      TrackCircuitDialogueBoxTop := ReadInteger(DialogueBoxSectionStr, TrackCircuitDialogueBoxTopStr, DefaultTrackCircuitDialogueBoxTop);

      LocoDialogueSpeedInMPH := ReadBool(DialogueBoxSectionStr, LocoDialogueSpeedInMPHStr, DefaultLocoDialogueSpeedInMPH);

      { Screen settings }
      LogFileMaxWidthInChars := ReadInteger(ScreenOptionsStr, LogFileMaxWidthInCharsStr, DefaultLogFileMaxWidthInChars);

      { Windows }
      TempStr := ReadString(ScreenOptionsStr, ScreenModeStr, '');
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

      FWPRailWindowTop := ReadInteger(WindowsSectionStr, FWPRailWindowTopStr, DefaultFWPRailWindowTop);
      FWPRailWindowLeft := ReadInteger(WindowsSectionStr, FWPRailWindowLeftStr, DefaultFWPRailWindowLeft);
      FWPRailWindowWidth := ReadInteger(WindowsSectionStr, FWPRailWindowWidthStr, DefaultFWPRailWindowWidth);
      FWPRailWindowHeight := ReadInteger(WindowsSectionStr, FWPRailWindowHeightStr, DefaultFWPRailWindowHeight);

      CreateRouteDisplayColoursWindowTop := ReadInteger(WindowsSectionStr, CreateRouteDisplayColoursWindowTopStr, DefaultCreateRouteDisplayColoursWindowTop);
      CreateRouteDisplayColoursWindowLeft := ReadInteger(WindowsSectionStr, CreateRouteDisplayColoursWindowLeftStr, DefaultCreateRouteDisplayColoursWindowLeft);
      CreateRouteDisplayColoursWindowWidth := ReadInteger(WindowsSectionStr, CreateRouteDisplayColoursWindowWidthStr, DefaultCreateRouteDisplayColoursWindowWidth);
      CreateRouteDisplayColoursWindowHeight := ReadInteger(WindowsSectionStr, CreateRouteDisplayColoursWindowHeightStr, DefaultCreateRouteDisplayColoursWindowHeight);

      DebugWindowTop := ReadInteger(WindowsSectionStr, DebugWindowTopStr, DefaultDebugWindowTop);
      DebugWindowLeft := ReadInteger(WindowsSectionStr, DebugWindowLeftStr, DefaultDebugWindowLeft);
      DebugWindowWidth := ReadInteger(WindowsSectionStr, DebugWindowWidthStr, DefaultDebugWindowWidth);
      DebugWindowHeight := ReadInteger(WindowsSectionStr, DebugWindowHeightStr, DefaultDebugWindowHeight);

      DiagramsWindowTop := ReadInteger(WindowsSectionStr, DiagramsWindowTopStr, DefaultDiagramsWindowTop);
      DiagramsWindowLeft := ReadInteger(WindowsSectionStr, DiagramsWindowLeftStr, DefaultDiagramsWindowLeft);
      DiagramsSmallWindowWidth := ReadInteger(WindowsSectionStr, DiagramsSmallWindowWidthStr, DefaultDiagramsSmallWindowWidth);
      DiagramsLargeWindowWidth := ReadInteger(WindowsSectionStr, DiagramsLargeWindowWidthStr, DefaultDiagramsLargeWindowWidth);
      DiagramsWindowHeight := ReadInteger(WindowsSectionStr, DiagramsWindowHeightStr, DefaultDiagramsWindowHeight);

      EditWindowTop := ReadInteger(WindowsSectionStr, EditWindowTopStr, DefaultEditWindowTop);
      EditWindowLeft := ReadInteger(WindowsSectionStr, EditWindowLeftStr, DefaultEditWindowLeft);
      EditWindowWidth := ReadInteger(WindowsSectionStr, EditWindowWidthStr, DefaultEditWindowWidth);
      EditWindowHeight := ReadInteger(WindowsSectionStr, EditWindowHeightStr, DefaultEditWindowHeight);

      LockListWindowTop := ReadInteger(WindowsSectionStr, LockListWindowTopStr, DefaultLockListWindowTop);
      LockListWindowLeft := ReadInteger(WindowsSectionStr, LockListWindowLeftStr, DefaultLockListWindowLeft);
      LockListWindowWidth := ReadInteger(WindowsSectionStr, LockListWindowWidthStr, DefaultLockListWindowWidth);
      LockListWindowHeight := ReadInteger(WindowsSectionStr, LockListWindowHeightStr, DefaultLockListWindowHeight);

      LocoUtilsWindowTop := ReadInteger(WindowsSectionStr, LocoUtilsWindowTopStr, DefaultLocoUtilsWindowTop);
      LocoUtilsWindowLeft := ReadInteger(WindowsSectionStr, LocoUtilsWindowLeftStr, DefaultLocoUtilsWindowLeft);
      LocoUtilsWindowWidth := ReadInteger(WindowsSectionStr, LocoUtilsWindowWidthStr, DefaultLocoUtilsWindowWidth);
      LocoUtilsWindowHeight := ReadInteger(WindowsSectionStr, LocoUtilsWindowHeightStr, DefaultLocoUtilsWindowHeight);

      LoggingWindowTop := ReadInteger(WindowsSectionStr, LoggingWindowTopStr, DefaultLoggingWindowTop);
      LoggingWindowLeft := ReadInteger(WindowsSectionStr, LoggingWindowLeftStr, DefaultLoggingWindowLeft);
      LoggingWindowWidth := ReadInteger(WindowsSectionStr, LoggingWindowWidthStr, DefaultLoggingWindowWidth);
      LoggingWindowHeight := ReadInteger(WindowsSectionStr, LoggingWindowHeightStr, DefaultLoggingWindowHeight);

      MovementWindowTop := ReadInteger(WindowsSectionStr, MovementWindowTopStr, DefaultMovementWindowTop);
      MovementWindowLeft := ReadInteger(WindowsSectionStr, MovementWindowLeftStr, DefaultMovementWindowLeft);
      MovementWindowWidth := ReadInteger(WindowsSectionStr, MovementWindowWidthStr, DefaultMovementWindowWidth);
      MovementWindowHeight := ReadInteger(WindowsSectionStr, MovementWindowHeightStr, DefaultMovementWindowHeight);

      OptionsWindowTop := ReadInteger(WindowsSectionStr, OptionsWindowTopStr, DefaultOptionsWindowTop);
      OptionsWindowLeft := ReadInteger(WindowsSectionStr, OptionsWindowLeftStr, DefaultOptionsWindowLeft);
      OptionsWindowWidth := ReadInteger(WindowsSectionStr, OptionsWindowWidthStr, DefaultOptionsWindowWidth);
      OptionsWindowHeight := ReadInteger(WindowsSectionStr, OptionsWindowHeightStr, DefaultOptionsWindowHeight);
      OptionsWindowValueListEditorCol0Width := ReadInteger(WindowsSectionStr, OptionsWindowValueListEditorCol0WidthStr, DefaultOptionsWindowValueListEditorCol0Width);

      WorkingTimetableWindowTop := ReadInteger(WindowsSectionStr, WorkingTimetableWindowTopStr, DefaultWorkingTimetableWindowTop);
      WorkingTimetableWindowLeft := ReadInteger(WindowsSectionStr, WorkingTimetableWindowLeftStr, DefaultWorkingTimetableWindowLeft);
      WorkingTimetableWindowHeight := ReadInteger(WindowsSectionStr, WorkingTimetableWindowHeightStr, DefaultWorkingTimetableWindowHeight);
      WorkingTimetableSmallWindowWidth := ReadInteger(WindowsSectionStr, WorkingTimetableSmallWindowWidthStr, DefaultWorkingTimetableSmallWindowWidth);
      WorkingTimetableLargeWindowWidth := ReadInteger(WindowsSectionStr, WorkingTimetableLargeWindowWidthStr, DefaultWorkingTimetableLargeWindowWidth);
      WorkingTimetableWindowHeight := ReadInteger(WindowsSectionStr, WorkingTimetableWindowHeightStr, DefaultWorkingTimetableWindowHeight);

      { Spacing Options }
      BufferStopVerticalSpacing := ReadInteger(ScreenOptionsStr, BufferStopVerticalSpacingStr, DefaultBufferStopVerticalSpacing);
      DeltaPointX := ReadInteger(ScreenOptionsStr, DeltaPointXStr, DefaultDeltaPointX);
      IndicatorHorizontalSpacing := ReadInteger(ScreenOptionsStr, IndicatorHorizontalSpacingStr, DefaultIndicatorHorizontalSpacing);
      IndicatorVerticalSpacing := ReadInteger(ScreenOptionsStr, IndicatorVerticalSpacingStr, DefaultIndicatorVerticalSpacing);
      MouseRectangleEdgeVerticalSpacing := ReadInteger(ScreenOptionsStr, MouseRectangleEdgeVerticalSpacingStr, DefaultMouseRectangleEdgeVerticalSpacing);
      PlatformEdgeVerticalSpacing := ReadInteger(ScreenOptionsStr, PlatformEdgeVerticalSpacingStr, DefaultPlatformEdgeVerticalSpacing);
      PlatformNumberEdgeHorizontalSpacing := ReadInteger(ScreenOptionsStr, PlatformNumberEdgeHorizontalSpacingStr, DefaultPlatformNumberEdgeHorizontalSpacing);
      PlatformNumberEdgeVerticalSpacing := ReadInteger(ScreenOptionsStr, PlatformNumberEdgeVerticalSpacingStr, DefaultPlatformNumberEdgeVerticalSpacing);
      SignalHorizontalSpacing := ReadInteger(ScreenOptionsStr, SignalHorizontalSpacingStr, DefaultSignalHorizontalSpacing);
      SignalRadius := ReadInteger(ScreenOptionsStr, SignalRadiusStr, DefaultSignalRadius);
      SignalSemaphoreHeight := ReadInteger(ScreenOptionsStr, SignalSemaphoreHeightStr, DefaultSignalSemaphoreHeight);
      SignalSemaphoreWidth := ReadInteger(ScreenOptionsStr, SignalSemaphoreWidthStr, DefaultSignalSemaphoreWidth);
      SignalVerticalSpacing := ReadInteger(ScreenOptionsStr, SignalVerticalSpacingStr, DefaultSignalVerticalSpacing);
      SpeedRestrictionHorizontalSpacing := ReadInteger(ScreenOptionsStr, SpeedRestrictionHorizontalSpacingStr, DefaultSpeedRestrictionHorizontalSpacing);
      SpeedRestrictionVerticalSpacing := ReadInteger(ScreenOptionsStr, SpeedRestrictionVerticalSpacingStr, DefaultSpeedRestrictionVerticalSpacing);
      TheatreIndicatorHorizontalSpacing := ReadInteger(ScreenOptionsStr, TheatreIndicatorHorizontalSpacingStr, DefaultTheatreIndicatorHorizontalSpacing);
      TheatreIndicatorVerticalSpacing := ReadInteger(ScreenOptionsStr, TheatreIndicatorVerticalSpacingStr, DefaultTheatreIndicatorVerticalSpacing);
      TRSPlungerLength := ReadInteger(ScreenOptionsStr, TRSPlungerLengthStr, DefaultTRSPlungerLength);

      { Other Options }
      AcceptAllPermanentOccupationsWithoutFeedback := ReadBool(OtherOptionsSectionStr, AcceptAllPermanentOccupationsWithoutFeedbackStr,
                                                                                                                       DefaultAcceptAllPermanentOccupationsWithoutFeedback);
      AutomaticallySetFocusWhenInDebugWindow := ReadBool(OtherOptionsSectionStr, AutomaticallySetFocusWhenInDebugWindowStr, DefaultAutomaticallySetFocusWhenInDebugWindow);
      CancelAllTrainsWithNoFeedbackOccupation := ReadBool(OtherOptionsSectionStr, CancelAllTrainsWithNoFeedbackOccupationStr,
                                                                                                                            DefaultCancelAllTrainsWithNoFeedbackOccupation);
      CarriageLengthInInches := ReadInteger(OtherOptionsSectionStr, CarriageLengthInInchesStr, DefaultCarriageLengthInInches);
      CheckForIdenticalLinesInLog := ReadBool(OtherOptionsSectionStr, CheckForIdenticalLinesInLogStr, DefaultCheckForIdenticalLinesInLog);
      DisplayDiagrams := ReadBool(OtherOptionsSectionStr, DisplayDiagramsStr, DefaultDisplayDiagrams);
      DisplayFlashingTrackCircuits := ReadBool(OtherOptionsSectionStr, DisplayFlashingTrackCircuitsStr, DefaultDisplayFlashingTrackCircuits);
      DisplayLocoChipNums := ReadBool(OtherOptionsSectionStr, DisplayLocoChipNumsStr, DefaultDisplayLocoChipNums);
      DisplayLocoHeadcodes := ReadBool(OtherOptionsSectionStr, DisplayLocoHeadcodesStr, DefaultDisplayLocoHeadcodes);
      DisplayNotForPublicUseTrainsInStationMonitors := ReadBool(OtherOptionsSectionStr, DisplayNotForPublicUseTrainsInStationMonitorsStr,
                                                                                                                      DefaultDisplayNotForPublicUseTrainsInStationMonitors);
      DisplayRoutesAndJourneys := ReadBool(OtherOptionsSectionStr, DisplayRoutesAndJourneysStr, DefaultDisplayRoutesAndJourneys);
      DoNotCancelTrainsWithNoFeedbackOccupation := ReadBool(OtherOptionsSectionStr, DoNotCancelTrainsWithNoFeedbackOccupationStr,
                                                                                                                          DefaultDoNotCancelTrainsWithNoFeedbackOccupation);
      HighlightTrackCircuitSpeedRestrictions := ReadBool(OtherOptionsSectionStr, HighlightTrackCircuitSpeedRestrictionsStr, DefaultHighlightTrackCircuitSpeedRestrictions);
      IncludeLocoChipInStationMonitors := ReadBool(OtherOptionsSectionStr, IncludeLocoChipInStationMonitorsStr, DefaultIncludeLocoChipInStationMonitors);
      LargeDiagramsWindowSelected := ReadBool(OtherOptionsSectionStr, LargeDiagramsWindowSelectedStr, DefaultLargeDiagramsWindowSelected);
      LargeWorkingTimetableWindowSelected := ReadBool(OtherOptionsSectionStr, LargeWorkingTimetableWindowSelectedStr, DefaultLargeWorkingTimetableWindowSelected);
      LineThicknessInFullScreenMode := ReadString(OtherOptionsSectionStr, LineThicknessInFullScreenModeStr, DefaultLineThicknessInFullScreenMode);
      LocoTimingTimeBeforeAutoStopInSeconds := ReadInteger(OtherOptionsSectionStr, LocoTimingTimeBeforeAutoStopInSecondsStr, DefaultLocoTimingTimeBeforeAutoStopInSeconds);
      LogCurrentTimeMode := ReadBool(OtherOptionsSectionStr, LogCurrentTimeModeStr, DefaultLogCurrentTimeMode);
      LogsKeptMode := ReadBool(OtherOptionsSectionStr, LogsKeptModeStr, DefaultLogsKeptMode);
      LogsCurrentlyKept := LogsKeptMode; { this allows LogsKeptMode to be changed without affecting the current logging } 
      MakeSoundWhenDebugWindowBoldTextAppears := ReadBool(OtherOptionsSectionStr, MakeSoundWhenDebugWindowBoldTextAppearsStr,
                                                                                                                            DefaultMakeSoundWhenDebugWindowBoldTextAppears);
      MaxRectangleUndrawTime := ReadInteger(OtherOptionsSectionStr, MaxRectangleUndrawTimeStr, DefaultMaxRectangleUndrawTime);
      MenusVisible := ReadBool(OtherOptionsSectionStr, MenusVisibleStr, DefaultMenusVisible);
      MonitorStrayingTrains := ReadBool(OtherOptionsSectionStr, MonitorStrayingTrainsStr, DefaultMonitorStrayingTrains);
      PointFeedbackMaximumWaitInSeconds := ReadInteger(OtherOptionsSectionStr, PointFeedbackMaximumWaitInSecondsStr, DefaultPointFeedbackMaximumWaitInSeconds);
      RouteAheadNotClearWaitTimeInMinutes := ReadInteger(OtherOptionssectionStr, RouteAheadNotClearWaitTimeInMinutesStr, DefaultRouteAheadNotClearWaitTimeInMinutes);
      RunTestUnitOnStartup := ReadBool(OtherOptionsSectionStr, RunTestUnitOnStartupStr, DefaultRunTestUnitOnStartup);
      ShowCancelledTrainsInDiagrams := ReadBool(OtherOptionsSectionStr, ShowCancelledTrainsInDiagramsStr, DefaultShowCancelledTrainsInDiagrams);
      ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable := ReadBool(OtherOptionsSectionStr, ShowIncorrectDayOfTheWeekEntriesInWorkingTimetableStr,
                                                                                                                 DefaultShowIncorrectDayOfTheWeekEntriesInWorkingTimetable);
      ShowNonMovingTrainsInDiagrams := ReadBool(OtherOptionsSectionStr, ShowNonMovingTrainsInDiagramsStr, DefaultShowNonMovingTrainsInDiagrams);
      ShowNonStopsInDiagrams := ReadBool(OtherOptionsSectionStr, ShowNonStopsInDiagramsStr, DefaultShowNonStopsInDiagrams);
      ShowTrackCircuitsWhereUserMustDrive := ReadBool(OtherOptionsSectionStr, ShowTrackCircuitsWhereUserMustDriveStr, DefaultShowTrackCircuitsWhereUserMustDrive);
      StartRepeatJourneysOnNewLineInDiagrams := ReadBool(OtherOptionsSectionStr, StartRepeatJourneysOnNewLineInDiagramsStr, DefaultStartRepeatJourneysOnNewLineInDiagrams);
      StartWithDiagrams := ReadBool(OtherOptionsSectionStr, StartWithDiagramsStr, DefaultStartWithDiagrams);
      StationEndOfDayPassengerLeavingTimeInMinutes := ReadInteger(OtherOptionsSectionStr, StationEndOfDayPassengerLeavingTimeInMinutesStr,
                                                                                                                       DefaultStationEndOfDayPassengerLeavingTimeInMinutes);
      StationOppositeDirectionExitMinimumWaitTimeInMinutes := ReadInteger(OtherOptionssectionStr, StationOppositeDirectionExitMinimumWaitTimeInMinutesStr,
                                                                                                               DefaultStationOppositeDirectionExitMinimumWaitTimeInMinutes);
      StationSameDirectionExitMinimumWaitTimeInMinutes := ReadInteger(OtherOptionsSectionStr, StationSameDirectionExitMinimumWaitTimeInMinutesStr,
                                                                                                                   DefaultStationSameDirectionExitMinimumWaitTimeInMinutes);
      StationStartMode := ReadBool(OtherOptionsSectionStr, StationStartModeStr, DefaultStationStartMode);
      StationStartOfDayPassengerBoardingTimeInMinutes := ReadInteger(OtherOptionssectionStr, StationStartOfDayPassengerBoardingTimeInMinutesStr,
                                                                                                                    DefaultStationStartOfDayPassengerBoardingTimeInMinutes);
      StopAllLocosAtShutDown := ReadBool(OtherOptionsSectionStr, StopAllLocosAtShutDownStr, DefaultStopAllLocosAtShutDown);
      SwitchActiveLocoLightsOffAtShutDown := ReadBool(OtherOptionsSectionStr, SwitchActiveLocoLightsOffAtShutDownStr, DefaultSwitchActiveLocoLightsOffAtShutDown);
      TheatreBoxHeight := ReadInteger(OtherOptionsSectionStr, TheatreBoxHeightStr, DefaultTheatreBoxHeight);
      TheatreBoxWidth := ReadInteger(OtherOptionsSectionStr, TheatreBoxWidthStr, DefaultTheatreBoxWidth);
      WaitBeforeRerouteInMinutes := ReadInteger(OtherOptionsSectionStr, WaitBeforeRerouteInMinutesStr, DefaultWaitBeforeRerouteInMinutes);
      WorkingTimetableMode := ReadBool(OtherOptionsSectionStr, WorkingTimetableModeStr, DefaultWorkingTimetableMode);
      WritingStationMonitorsDisplayToFile := ReadBool(OtherOptionsSectionStr, WritingStationMonitorsDisplayToFileStr, DefaultWritingStationMonitorsDisplayToFile);
      { Time options }
      TempStr := ReadString(TimesSectionStr, ProgramStartTimeOptionStr, DefaultProgramStartTimeStr);
      IF (TempStr <> '')
      AND (TimeIsValid(TempStr))
      THEN BEGIN
        ProgramStartTime := StrToTime(TempStr);
        Log('A Program Start Time set to ' + TempStr);
      END;

      TempStr := ReadString(TimesSectionStr, DayLightStartTimeOptionStr, DefaultDayLightStartTimeStr);
      IF (TempStr <> '')
      AND (TimeIsValid(TempStr))
      THEN
        DayLightStartTime := StrToTime(TempStr);

      TempStr := ReadString(TimesSectionStr, DayLightEndTimeOptionStr, DefaultDayLightEndTimeStr);
      IF (TempStr <> '')
      AND (TimeIsValid(TempStr))
      THEN
        DayLightEndTime := StrToTime(TempStr);

      TempStr := ReadString(TimesSectionStr, CurrentRailwayDayOfTheWeekStr, DefaultCurrentRailwayDayOfTheWeek);
      CurrentRailwayDayOfTheWeek := StrToDayOfTheWeek(TempStr);
      SetCurrentRailwayTimeAndDayOfTheWeek(ProgramStartTime);

      { RailDriver Data }
      RDCBailOffMin := ReadInteger(RDCSectionStr, RDCBailOffMinStr, 0);
      RDCBailOffMax := ReadInteger(RDCSectionStr, RDCBailOffMaxStr, 0);
      RDCEmergencyBrakeMin := ReadInteger(RDCSectionStr, RDCEmergencyBrakeMinStr, 0);
      RDCEmergencyBrakeMax := ReadInteger(RDCSectionStr, RDCEmergencyBrakeMaxStr, 0);
      RDCLocoBrakeMin := ReadInteger(RDCSectionStr, RDCLocoBrakeMinStr, 0);
      RDCLocoBrakeMax := ReadInteger(RDCSectionStr, RDCLocoBrakeMaxStr, 0);
      RDCRegulatorForwardMin := ReadInteger(RDCSectionStr, RDCRegulatorForwardMinStr, 0);
      RDCRegulatorForwardMax := ReadInteger(RDCSectionStr, RDCRegulatorForwardMaxStr, 0);
      RDCRegulatorReverseMin := ReadInteger(RDCSectionStr, RDCRegulatorReverseMinStr, 0);
      RDCRegulatorReverseMax := ReadInteger(RDCSectionStr, RDCRegulatorReverseMaxStr, 0);
      RDCReverserForwardMin := ReadInteger(RDCSectionStr, RDCReverserForwardMinStr, 0);
      RDCReverserForwardMax := ReadInteger(RDCSectionStr, RDCReverserForwardMaxStr, 0);
      RDCReverserReverseMin := ReadInteger(RDCSectionStr, RDCReverserReverseMinStr, 0);
      RDCReverserReverseMax := ReadInteger(RDCSectionStr, RDCReverserReverseMaxStr, 0);
      RDCThreeWaySwitchALeftNum := ReadInteger(RDCSectionStr, RDCThreeWaySwitchALeftNumStr, 0);
      RDCThreeWaySwitchAMidNum := ReadInteger(RDCSectionStr, RDCThreeWaySwitchAMidNumStr, 0);
      RDCThreeWaySwitchARightNum := ReadInteger(RDCSectionStr, RDCThreeWaySwitchARightNumStr, 0);
      RDCThreeWaySwitchBLeftNum := ReadInteger(RDCSectionStr, RDCThreeWaySwitchBLeftNumStr, 0);
      RDCThreeWaySwitchBMidNum := ReadInteger(RDCSectionStr, RDCThreeWaySwitchBMidNumStr, 0);
      RDCThreeWaySwitchBRightNum := ReadInteger(RDCSectionStr, RDCThreeWaySwitchBRightNumStr, 0);
      RDCTrainBrakeMin := ReadInteger(RDCSectionStr, RDCTrainBrakeMinStr, 0);
      RDCTrainBrakeMax := ReadInteger(RDCSectionStr, RDCTrainBrakeMaxStr, 0);

      { Other Miscellaneous Data }
      CurrentParametersFromIniFile := ReadString(MiscellaneousDataSectionStr, CurrentParametersStr, '');
    END; {WITH}

    { The inifile is freed not here but after the track circuits are read in }
  EXCEPT
    ON E : Exception DO
      Log('EG ReadIniFile: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadIniFileMainProcedure }

PROCEDURE ReadIniFileForTrackCircuitData; //MainProcedure(IniFile : TRegistryIniFile);
{ Read in trackcircuit data from the .ini file or from the Registry }
CONST
  Delimiter = ';';

VAR
  DelimiterPos : Integer;
  IniFile : TRegistryIniFile;
  TempStr : String;
  TempStr2 : String;
  TC : Integer;

BEGIN
  TRY
    IniFile := TRegistryIniFile.Create('FWPRail');

    WITH IniFile DO BEGIN
      { Now trackcircuit info. (Can't use SetTrackCircuitState here as it writes to the log file which is not yet open, as the log file name is held in the .ini file). }
      FOR TC := 0 TO High(TrackCircuits) DO BEGIN
        TempStr := ReadString(TrackCircuitsSectionStr, IntToStr(TC), '');
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
    END; {WITH}

    IniFile.Free;
  EXCEPT
    ON E : Exception DO
      Log('EG ReadIniFileForTrackCircuitData: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadIniFileForTrackCircuitDataMainProcedure }

PROCEDURE WriteIniFile;
{ Write out data to the .ini file or to the Registry }
CONST
  StopTimer = True;

VAR
  IniFile : TRegistryIniFile;
  TC : Integer;
  TCString : String;

BEGIN
  TRY
    { The .ini file was stored where the executable is }
//    IniFile := TRegistryIniFile.Create(ExtractFilePath(Application.ExeName) + 'Rail.ini');
    IniFile := TRegistryIniFile.Create('FWPRail');

    WITH IniFile DO BEGIN
      { Now trackcircuits that are user designated as having special properties - delete when no longer special }
      IF Length(TrackCircuits) > 0 THEN BEGIN
//        IF (TrackCircuits[183].TC_OccupationState <> TCOutOfUseSetByUser)
//        AND (TrackCircuits[183].TC_OccupationState <> TCOutOfUseAsNoFeedbackReceived)
//        THEN BEGIN
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

            IF (TrackCircuits[TC].TC_SpeedRestrictionInMPH <> NoSpecifiedSpeed)
            AND (TrackCircuits[TC].TC_SpeedRestrictionInMPH <> MPH0)
            THEN
              TCString := TCString + TrackCircuitSpeedRestrictionStr
                                     + ' MPH=' + MPHToStr(TrackCircuits[TC].TC_SpeedRestrictionInMPH)
                                     + ' Dir=' + DirectionToStr(TrackCircuits[TC].TC_SpeedRestrictionDirection)
                                     + ';';

            IF TrackCircuits[TC].TC_UserMustDrive THEN
              TCString := TCString + TrackCircuitUserMustDriveStr + ';';

            IF TCString <> '' THEN
              WriteString(TrackCircuitsSectionStr, IntToStr(TC), TCString)
            ELSE
              IF (TrackCircuits[TC].TC_OccupationState <> TCOutOfUseAsNoFeedbackReceived)
              AND ValueExists(TrackCircuitsSectionStr, IntToStr(TC))
              THEN
                DeleteKey(TrackCircuitsSectionStr, IntToStr(TC));
          END; {FOR }
//        END;
      END;

      WriteString('User', 'User', GetUserFromWindows);
      WriteString('User', 'Date', DateTimeToStr(Now));
      WriteString('User', 'Computer', GetComputerNetName);
      WriteString('User', 'Version', GetVersionInfoAsString);
      WriteString('User', 'Build', GetBuildInfoAsString);

      { various file data }
      WriteString(FilesSectionStr, PathToRailDataFilesStr, PathToRailDataFiles);
      WriteString(FilesSectionStr, PathToLogFilesStr, PathToLogFiles);

      WriteString(FilesSectionStr, AreaDataFilenameStr, AreaDataFilename);
      WriteString(FilesSectionStr, AreaDataFilenameSuffixStr, AreaDataFilenameSuffix);

      IF DiagramsFilename = '' THEN BEGIN
        { check that we want no diagrams the next time we start }
        IF MessageDialogueWithDefault('There are no diagrams loaded: do you wish to start the next session with diagrams or without?',
                                      StopTimer, mtError, [mbYes, mbNo], ['&With', 'With&out'], mbYes) = mrYes
        THEN
          DiagramsFilename := DefaultDiagramsFilename;
      END;
      WriteString(FilesSectionStr, DiagramsFilenameStr, DiagramsFilename);
      WriteString(FilesSectionStr, DiagramsFilenameSuffixStr, DiagramsFilenameSuffix);

      WriteString(FilesSectionStr, FeedbackDataFilenameStr, FeedbackDataFilename);
      WriteString(FilesSectionStr, FeedbackDataFilenameSuffixStr, FeedbackDataFilenameSuffix);
      WriteString(FilesSectionStr, LineDataFilenameStr, LineDataFilename);
      WriteString(FilesSectionStr, LineDataFilenameSuffixStr, LineDataFilenameSuffix);
      WriteString(FilesSectionStr, LocationDataFilenameStr, LocationDataFilename);
      WriteString(FilesSectionStr, LocationDataFilenameSuffixStr, LocationDataFilenameSuffix);
      WriteString(FilesSectionStr, LocoDataFilenameStr, LocoDataFilename);
      WriteString(FilesSectionStr, LocoDataFilenameSuffixStr, LocoDataFilenameSuffix);
      WriteString(FilesSectionStr, LogFilenameStr, LogFilename);
      WriteString(FilesSectionStr, LogFilenameSuffixStr, LogFilenameSuffix);
      WriteString(FilesSectionStr, PlatformDataFilenameStr, PlatformDataFilename);
      WriteString(FilesSectionStr, PlatformDataFilenameSuffixStr, PlatformDataFilenameSuffix);
      WriteString(FilesSectionStr, PointDataFilenameStr, PointDataFilename);
      WriteString(FilesSectionStr, PointDataFilenameSuffixStr, PointDataFilenameSuffix);
      WriteString(FilesSectionStr, ReplayFilenameStr, ReplayFilename);
      WriteString(FilesSectionStr, ReplayFilenameSuffixStr, ReplayFilenameSuffix);
      WriteString(FilesSectionStr, RouteingExceptionDataFilenameStr, RouteingExceptionDataFilename);
      WriteString(FilesSectionStr, RouteingExceptionDataFilenameSuffixStr, RouteingExceptionDataFilenameSuffix);
      WriteString(FilesSectionStr, SignalDataFilenameStr, SignalDataFilename);
      WriteString(FilesSectionStr, SignalDataFilenameSuffixStr, SignalDataFilenameSuffix);
      WriteString(FilesSectionStr, StationMonitorsDataFilenameStr, StationMonitorsDataFilename);
      WriteString(FilesSectionStr, StationMonitorsDataFilenameSuffixStr, StationMonitorsDataFilenameSuffix);
      WriteString(FilesSectionStr, TrackCircuitDataFilenameStr, TrackCircuitDataFilename);
      WriteString(FilesSectionStr, TrackCircuitDataFilenameSuffixStr, TrackCircuitDataFilenameSuffix);
      WriteString(FilesSectionStr, WorkingTimetableFilenameStr, WorkingTimetableFilename);
      WriteString(FilesSectionStr, WorkingTimetableFilenameSuffixStr, WorkingTimetableFilenameSuffix);

      { Colours for buffer stops }
      WriteString(ColoursSectionStr, BufferStopColourStr, ColourToStr(BufferStopColour));
      WriteString(ColoursSectionStr, BufferStopNumberColourStr, ColourToStr(BufferStopNumberColour));
      WriteString(ColoursSectionStr, BufferStopRedStr, ColourToStr(BufferStopRed));

      { Colours for TRS plungers }
      WriteString(ColoursSectionStr, TRSPlungerColourStr, ColourToStr(TRSPlungerColour));
      WriteString(ColoursSectionStr, TRSPlungerOutlineColourStr, ColourToStr(TRSPlungerOutlineColour));
      WriteString(ColoursSectionStr, TRSPlungerPressedColourStr, ColourToStr(TRSPlungerPressedColour));

      { Colours for points }
      WriteString(ColoursSectionStr, LenzPointNumberColourStr, ColourToStr(LenzPointNumberColour));
      WriteString(ColoursSectionStr, PointColourStr, ColourToStr(PointColour));
      WriteString(ColoursSectionStr, PointDivergingLineColourStr, ColourToStr(PointDivergingLineColour));
      WriteString(ColoursSectionStr, PointDownFacingColourStr, ColourToStr(PointDownFacingColour));
      WriteString(ColoursSectionStr, PointFeedbackDataInUseColourStr, ColourToStr(PointFeedbackDataInUseColour));
      WriteString(ColoursSectionStr, PointFeedbackDataOutOfUseColourStr, ColourToStr(PointFeedbackDataOutOfUseColour));
      WriteString(ColoursSectionStr, PointHeelLineColourStr, ColourToStr(PointHeelLineColour));
      WriteString(ColoursSectionStr, PointLockedByUserColourStr, ColourToStr(PointLockedByUserColour));
      WriteString(ColoursSectionStr, PointManualOperationColourStr, ColourToStr(PointManualOperationColour));
      WriteString(ColoursSectionStr, PointOutOfUseColourStr, ColourToStr(PointOutOfUseColour));
      WriteString(ColoursSectionStr, PointStraightLineColourStr, ColourToStr(PointStraightLineColour));
      WriteString(ColoursSectionStr, PointsWithoutFeedbackColourStr, ColourToStr(PointsWithoutFeedbackColour));
      WriteString(ColoursSectionStr, PointUndrawColourStr, ColourToStr(PointUndrawColour));
      WriteString(ColoursSectionStr, PointUpFacingColourStr, ColourToStr(PointUpFacingColour));
      WriteString(ColoursSectionStr, ShowPointDefaultStateColourStr, ColourToStr(ShowPointDefaultStateColour));
      WriteString(ColoursSectionStr, ShowPointLockedColourStr, ColourToStr(ShowPointLockedColour));

      { Colours for signal posts }
      WriteString(ColoursSectionStr, SignalPostColourStr, ColourToStr(SignalPostColour));
      WriteString(ColoursSectionStr, SignalPostEmergencyRouteSettingColourStr, ColourToStr(SignalPostEmergencyRouteSettingColour));
      WriteString(ColoursSectionStr, SignalPostRouteSettingColourStr, ColourToStr(SignalPostRouteSettingColour));
      WriteString(ColoursSectionStr, SignalPostStationStartModeColourStr, ColourToStr(SignalPostStationStartModeColour));
      WriteString(ColoursSectionStr, SignalPostTheatreSettingColourStr, ColourToStr(SignalPostTheatreSettingColour));

      { Colours for signals }
      WriteString(ColoursSectionStr, SignalAspectGreenStr, ColourToStr(SignalAspectGreen));
      WriteString(ColoursSectionStr, SignalAspectRedStr, ColourToStr(SignalAspectRed));
      WriteString(ColoursSectionStr, SignalAspectUnlitStr, ColourToStr(SignalAspectUnlit));
      WriteString(ColoursSectionStr, SignalAspectYellowStr, ColourToStr(SignalAspectYellow));
      WriteString(ColoursSectionStr, SignalNumberColourStr, ColourToStr(SignalNumberColour));

      { Colours for lines }
      WriteString(ColoursSectionStr, LineRoutedOverColourStr, ColourToStr(LineRoutedOverColour));

      { Colours for platforms }
      WriteString(ColoursSectionStr, PlatformColourStr, ColourToStr(PlatformColour));
      WriteString(ColoursSectionStr, PlatformNumberColourStr, ColourToStr(PlatformNumberColour));

      { Colours for trackcircuits }
      WriteString(ColoursSectionStr, TCFeedbackDataInUseColourStr, ColourToStr(TCFeedbackDataInUseColour));
      WriteString(ColoursSectionStr, TCFeedbackDataOutOfUseColourStr, ColourToStr(TCFeedbackDataOutOfUseColour));
      WriteString(ColoursSectionStr, TCFeedbackOccupationColourStr, ColourToStr(TCFeedbackOccupationColour));
      WriteString(ColoursSectionStr, TCFeedbackOccupationButOutOfUseColourStr, ColourToStr(TCFeedbackOccupationButOutOfUseColour));
      WriteString(ColoursSectionStr, TCLocoOutOfPlaceOccupationColourStr, ColourToStr(TCLocoOutOfPlaceOccupationColour));
      WriteString(ColoursSectionStr, TCMissingOccupationColourStr, ColourToStr(TCMissingOccupationColour));
      WriteString(ColoursSectionStr, TCOutOfUseSetByUserColourStr, ColourToStr(TCOutOfUseSetByUserColour));
      WriteString(ColoursSectionStr, TCOutOfUseAsNoFeedbackReceivedColourStr, ColourToStr(TCOutOfUseAsNoFeedbackReceivedColour));
      WriteString(ColoursSectionStr, TCPermanentFeedbackOccupationColourStr, ColourToStr(TCPermanentFeedbackOccupationColour));
      WriteString(ColoursSectionStr, TCPermanentOccupationSetByUserColourStr, ColourToStr(TCPermanentOccupationSetByUserColour));
      WriteString(ColoursSectionStr, TCPermanentSystemOccupationColourStr, ColourToStr(TCPermanentSystemOccupationColour));
      WriteString(ColoursSectionStr, TCSpeedRestrictionColourStr, ColourToStr(TCSpeedRestrictionColour));
      WriteString(ColoursSectionStr, TCSystemOccupationColourStr, ColourToStr(TCSystemOccupationColour));
      WriteString(ColoursSectionStr, TCUnoccupiedColourStr, ColourToStr(TCUnoccupiedColour));

      { Miscellaneous colours }
      WriteString(ColoursSectionStr, BackgroundColourStr, ColourToStr(BackgroundColour));
      WriteString(ColoursSectionStr, ForegroundColourStr, ColourToStr(ForegroundColour));
      WriteString(ColoursSectionStr, DiagramsWindowGridBackgroundColourStr, ColourToStr(DiagramsWindowGridBackgroundColour));
      WriteString(ColoursSectionStr, LineNotAvailableColourStr, ColourToStr(LineNotAvailableColour));
      WriteString(ColoursSectionStr, LinesWithoutTrackCircuitsColourStr, ColourToStr(LinesWithoutTrackCircuitsColour));
      WriteString(ColoursSectionStr, LocoStalledColourStr, ColourToStr(LocoStalledColour));
      WriteString(ColoursSectionStr, TrainActiveColourStr, ColourToStr(TrainActiveColour));
      WriteString(ColoursSectionStr, TrainInactiveColourStr, ColourToStr(TrainInactiveColour));
      WriteString(ColoursSectionStr, WorkingTimetableWindowGridBackgroundColourStr, ColourToStr(WorkingTimetableWindowGridBackgroundColour));

      { Pen styles }
      WriteString(PenStylesSectionStr, FiddleyardLinePenStyleStr, PenStyleToStr(FiddleyardLinePenStyle));
      WriteString(PenStylesSectionStr, ProjectedLinePenStyleStr, PenStyleToStr(ProjectedLinePenStyle));
      WriteString(PenStylesSectionStr, SidingPenStyleStr, PenStyleToStr(SidingPenStyle));
      WriteString(PenStylesSectionStr, TCLocoOutOfPlaceOccupationPenStyleStr, PenStyleToStr(TCLocoOutOfPlaceOccupationPenStyle));
      WriteString(PenStylesSectionStr, TCOutOfUseAsNoFeedbackReceivedPenStyleStr, PenStyleToStr(TCOutOfUseAsNoFeedbackReceivedPenStyle));
      WriteString(PenStylesSectionStr, TCOutOfUseSetByUserPenStyleStr, PenStyleToStr(TCOutOfUseSetByUserPenStyle));
      WriteString(PenStylesSectionStr, TCPermanentFeedbackOccupationPenStyleStr, PenStyleToStr(TCPermanentFeedbackOccupationPenStyle));
      WriteString(PenStylesSectionStr, TCPermanentOccupationSetByUserPenStyleStr, PenStyleToStr(TCPermanentOccupationSetByUserPenStyle));
      WriteString(PenStylesSectionStr, TCPermanentSystemOccupationPenStyleStr, PenStyleToStr(TCPermanentSystemOccupationPenStyle));

      { Fonts }
      WriteString(FontsSectionStr, RailFontNameStr, RailFontName);
      WriteInteger(FontsSectionStr, LineFontHeightStr, LineFontHeight);
      WriteString(FontsSectionStr, LoggingWindowFontNameStr, LoggingWindowFontName);
      WriteInteger(FontsSectionStr, LoggingWindowFontSizeStr, LoggingWindowFontSize);
      WriteInteger(FontsSectionStr, FWPRailWindowFontHeightStr, FWPRailWindowFontHeight);
      WriteInteger(FontsSectionStr, PlatformNumberFontHeightStr, PlatformNumberFontHeight);
      WriteString(FontsSectionStr, StationMonitorsFontNameStr, StationMonitorsFontName);
      WriteInteger(FontsSectionStr, StationMonitorsLargeFontHeightStr, StationMonitorsLargeFontHeight);
      WriteInteger(FontsSectionStr, StationMonitorsSmallFontHeightStr, StationMonitorsSmallFontHeight);
      WriteInteger(FontsSectionStr, TheatreFontHeightStr, TheatreFontHeight);

      { Dialogue Boxes variables }
      WriteInteger(DialogueBoxSectionStr, DebuggingOptionsWindowLeftStr, DebuggingOptionsWindowLeft);
      WriteInteger(DialogueBoxSectionStr, DebuggingOptionsWindowTopStr, DebuggingOptionsWindowTop);
      WriteInteger(DialogueBoxSectionStr, LineDialogueBoxLeftStr, LineDialogueBoxLeft);
      WriteInteger(DialogueBoxSectionStr, LineDialogueBoxTopStr, LineDialogueBoxTop);
      WriteInteger(DialogueBoxSectionStr, LocoDialogueWindowLeftStr, LocoDialogueWindowLeft);
      WriteInteger(DialogueBoxSectionStr, LocoDialogueWindowTopStr, LocoDialogueWindowTop);
      WriteBool(DialogueBoxSectionStr, LocoDialogueSpeedInMPHStr, LocoDialogueSpeedInMPH);
      WriteInteger(DialogueBoxSectionStr, PointDialogueBoxLeftStr, PointDialogueBoxLeft);
      WriteInteger(DialogueBoxSectionStr, PointDialogueBoxTopStr, PointDialogueBoxTop);
      WriteInteger(DialogueBoxSectionStr, SignalDialogueBoxLeftStr, SignalDialogueBoxLeft);
      WriteInteger(DialogueBoxSectionStr, SignalDialogueBoxTopStr, SignalDialogueBoxTop);
      WriteInteger(DialogueBoxSectionStr, TrackCircuitDialogueBoxLeftStr, TrackCircuitDialogueBoxLeft);
      WriteInteger(DialogueBoxSectionStr, TrackCircuitDialogueBoxTopStr, TrackCircuitDialogueBoxTop);

      { Process RailDriver data }
      WriteInteger(RDCSectionStr, RDCBailOffMinStr, RDCBailOffMin);
      WriteInteger(RDCSectionStr, RDCBailOffMaxStr, RDCBailOffMax);
      WriteInteger(RDCSectionStr, RDCEmergencyBrakeMinStr, RDCEmergencyBrakeMin);
      WriteInteger(RDCSectionStr, RDCEmergencyBrakeMaxStr, RDCEmergencyBrakeMax);
      WriteInteger(RDCSectionStr, RDCLocoBrakeMinStr, RDCLocoBrakeMin);
      WriteInteger(RDCSectionStr, RDCLocoBrakeMaxStr, RDCLocoBrakeMax);
      WriteInteger(RDCSectionStr, RDCRegulatorForwardMinStr, RDCRegulatorForwardMin);
      WriteInteger(RDCSectionStr, RDCRegulatorForwardMaxStr, RDCRegulatorForwardMax);
      WriteInteger(RDCSectionStr, RDCRegulatorReverseMinStr, RDCRegulatorReverseMin);
      WriteInteger(RDCSectionStr, RDCRegulatorReverseMaxStr, RDCRegulatorReverseMax);
      WriteInteger(RDCSectionStr, RDCReverserForwardMinStr, RDCReverserForwardMin);
      WriteInteger(RDCSectionStr, RDCReverserForwardMaxStr, RDCReverserForwardMax);
      WriteInteger(RDCSectionStr, RDCReverserReverseMinStr, RDCReverserReverseMin);
      WriteInteger(RDCSectionStr, RDCReverserReverseMaxStr, RDCReverserReverseMax);
      WriteInteger(RDCSectionStr, RDCTrainBrakeMinStr, RDCTrainBrakeMin);
      WriteInteger(RDCSectionStr, RDCTrainBrakeMaxStr, RDCTrainBrakeMax);
      WriteInteger(RDCSectionStr, RDCThreeWaySwitchALeftNumStr, RDCThreeWaySwitchALeftNum);
      WriteInteger(RDCSectionStr, RDCThreeWaySwitchAMidNumStr, RDCThreeWaySwitchAMidNum);
      WriteInteger(RDCSectionStr, RDCThreeWaySwitchARightNumStr, RDCThreeWaySwitchARightNum);
      WriteInteger(RDCSectionStr, RDCThreeWaySwitchBLeftNumStr, RDCThreeWaySwitchBLeftNum);
      WriteInteger(RDCSectionStr, RDCThreeWaySwitchBMidNumStr, RDCThreeWaySwitchBMidNum);
      WriteInteger(RDCSectionStr, RDCThreeWaySwitchBRightNumStr, RDCThreeWaySwitchBRightNum);

      { Windows }
      WriteInteger(WindowsSectionStr, FWPRailWindowWidthStr, FWPRailWindow.Width);
      WriteInteger(WindowsSectionStr, FWPRailWindowHeightStr, FWPRailWindow.Height);
      WriteInteger(WindowsSectionStr, FWPRailWindowTopStr, FWPRailWindow.Top);
      WriteInteger(WindowsSectionStr, FWPRailWindowLeftStr, FWPRailWindow.Left);
      WriteInteger(WindowsSectionStr, FWPRailWindowClientWidthStr, FWPRailWindow.ClientWidth);
      WriteInteger(WindowsSectionStr, FWPRailWindowClientHeightStr, FWPRailWindow.ClientHeight);

      IF CreateRouteDisplayColoursWindow <> NIL THEN BEGIN
        WriteInteger(WindowsSectionStr, CreateRouteDisplayColoursWindowHeightStr, CreateRouteDisplayColoursWindow.Height);
        WriteInteger(WindowsSectionStr, CreateRouteDisplayColoursWindowLeftStr, CreateRouteDisplayColoursWindow.Left);
        WriteInteger(WindowsSectionStr, CreateRouteDisplayColoursWindowTopStr, CreateRouteDisplayColoursWindow.Top);
        WriteInteger(WindowsSectionStr, CreateRouteDisplayColoursWindowWidthStr, CreateRouteDisplayColoursWindow.Width);
      END;

      IF DebugWindow <> NIL THEN BEGIN
        WriteInteger(WindowsSectionStr, DebugWindowHeightStr, DebugWindow.Height);
        WriteInteger(WindowsSectionStr, DebugWindowLeftStr, DebugWindow.Left);
        WriteInteger(WindowsSectionStr, DebugWindowTopStr, DebugWindow.Top);
        WriteInteger(WindowsSectionStr, DebugWindowWidthStr, DebugWindow.Width);
      END;

      IF DiagramsWindow <> NIL THEN BEGIN
        WriteInteger(WindowsSectionStr, DiagramsWindowHeightStr, DiagramsWindow.Height);
        WriteInteger(WindowsSectionStr, DiagramsWindowLeftStr, DiagramsWindow.Left);
        WriteInteger(WindowsSectionStr, DiagramsWindowTopStr, DiagramsWindow.Top);
        WriteInteger(WindowsSectionStr, DiagramsLargeWindowWidthStr, DiagramsLargeWindowWidth);
        WriteInteger(WindowsSectionStr, DiagramsSmallWindowWidthStr, DiagramsSmallWindowWidth);
      END;

      IF EditWindow <> NIL THEN BEGIN
        WriteInteger(WindowsSectionStr, EditWindowHeightStr, EditWindow.Height);
        WriteInteger(WindowsSectionStr, EditWindowLeftStr, EditWindow.Left);
        WriteInteger(WindowsSectionStr, EditWindowTopStr, EditWindow.Top);
        WriteInteger(WindowsSectionStr, EditWindowWidthStr, EditWindowWidth);
      END;

      IF LockListWindow <> NIL THEN BEGIN
        WriteInteger(WindowsSectionStr, LockListWindowHeightStr, LockListWindow.Height);
        WriteInteger(WindowsSectionStr, LockListWindowLeftStr, LockListWindow.Left);
        WriteInteger(WindowsSectionStr, LockListWindowTopStr, LockListWindow.Top);
        WriteInteger(WindowsSectionStr, LockListWindowWidthStr, LockListWindow.Width);
      END;

      IF LocoUtilsWindow <> NIL THEN BEGIN
        WriteInteger(WindowsSectionStr, LocoUtilsWindowHeightStr, LocoUtilsWindow.Height);
        WriteInteger(WindowsSectionStr, LocoUtilsWindowLeftStr, LocoUtilsWindow.Left);
        WriteInteger(WindowsSectionStr, LocoUtilsWindowTopStr, LocoUtilsWindow.Top);
        WriteInteger(WindowsSectionStr, LocoUtilsWindowWidthStr, LocoUtilsWindow.Width);
      END;

      IF LoggingWindow <> NIL THEN BEGIN
        WriteInteger(WindowsSectionStr, LoggingWindowHeightStr, LoggingWindow.Height);
        WriteInteger(WindowsSectionStr, LoggingWindowLeftStr, LoggingWindow.Left);
        WriteInteger(WindowsSectionStr, LoggingWindowTopStr, LoggingWindow.Top);
        WriteInteger(WindowsSectionStr, LoggingWindowWidthStr, LoggingWindow.Width);
      END;

      IF MovementWindow <> NIL THEN BEGIN
        WriteInteger(WindowsSectionStr, MovementWindowHeightStr, MovementWindow.Height);
        WriteInteger(WindowsSectionStr, MovementWindowLeftStr, MovementWindow.Left);
        WriteInteger(WindowsSectionStr, MovementWindowTopStr, MovementWindow.Top);
        WriteInteger(WindowsSectionStr, MovementWindowWidthStr, MovementWindow.Width);
      END;

      IF OptionsWindow <> NIL THEN BEGIN
        WriteInteger(WindowsSectionStr, OptionsWindowHeightStr, OptionsWindow.Height);
        WriteInteger(WindowsSectionStr, OptionsWindowLeftStr, OptionsWindow.Left);
        WriteInteger(WindowsSectionStr, OptionsWindowTopStr, OptionsWindow.Top);
        WriteInteger(WindowsSectionStr, OptionsWindowWidthStr, OptionsWindow.Width);

        WriteInteger(WindowsSectionStr, OptionsWindowValueListEditorCol0WidthStr, OptionsWindow.OptionsValueListEditor.ColWidths[0]);
      END;

      IF WorkingTimetableWindow <> NIL THEN BEGIN
        WriteInteger(WindowsSectionStr, WorkingTimetableWindowHeightStr, WorkingTimetableWindow.Height);
        WriteInteger(WindowsSectionStr, WorkingTimetableWindowLeftStr, WorkingTimetableWindow.Left);
        WriteInteger(WindowsSectionStr, WorkingTimetableWindowTopStr, WorkingTimetableWindow.Top);
        WriteInteger(WindowsSectionStr, WorkingTimetableLargeWindowWidthStr, WorkingTimetableLargeWindowWidth);
        WriteInteger(WindowsSectionStr, WorkingTimetableSmallWindowWidthStr, WorkingTimetableSmallWindowWidth);
      END;

      { Other options }
      WriteBool(OtherOptionsSectionStr, AcceptAllPermanentOccupationsWithoutFeedbackStr, AcceptAllPermanentOccupationsWithoutFeedback);
      WriteBool(OtherOptionsSectionStr, AutomaticallySetFocusWhenInDebugWindowStr, AutomaticallySetFocusWhenInDebugWindow);
      WriteBool(OtherOptionsSectionStr, CancelAllTrainsWithNoFeedbackOccupationStr, CancelAllTrainsWithNoFeedbackOccupation);
      WriteInteger(OtherOptionsSectionStr, CarriageLengthInInchesStr, CarriageLengthInInches);
      WriteBool(OtherOptionsSectionStr, CheckForIdenticalLinesInLogStr, CheckForIdenticalLinesInLog);
      WriteBool(OtherOptionsSectionStr, DisplayDiagramsStr, DisplayDiagrams);
      WriteBool(OtherOptionsSectionStr, DisplayFlashingTrackCircuitsStr, DisplayFlashingTrackCircuits);
      WriteBool(OtherOptionsSectionStr, DisplayLocoChipNumsStr, DisplayLocoChipNums);
      WriteBool(OtherOptionsSectionStr, DisplayLocoHeadcodesStr, DisplayLocoHeadcodes);
      WriteBool(OtherOptionsSectionStr, DisplayNotForPublicUseTrainsInStationMonitorsStr, DisplayNotForPublicUseTrainsInStationMonitors);
      WriteBool(OtherOptionsSectionStr, DisplayRoutesAndJourneysStr, DisplayRoutesAndJourneys);
      WriteBool(OtherOptionsSectionStr, DisplayWorkingTimetableStr, DisplayWorkingTimetable);
      WriteBool(OtherOptionsSectionStr, DoNotCancelTrainsWithNoFeedbackOccupationStr, DoNotCancelTrainsWithNoFeedbackOccupation);
      WriteBool(OtherOptionsSectionStr, HighlightTrackCircuitSpeedRestrictionsStr, HighlightTrackCircuitSpeedRestrictions);
      WriteBool(OtherOptionsSectionStr, IncludeLocoChipInStationMonitorsStr, IncludeLocoChipInStationMonitors);
      WriteBool(OtherOptionsSectionStr, LargeDiagramsWindowSelectedStr, LargeDiagramsWindowSelected);
      WriteBool(OtherOptionsSectionStr, LargeWorkingTimetableWindowSelectedStr, LargeWorkingTimetableWindowSelected);
      WriteString(OtherOptionsSectionStr, LineThicknessInFullScreenModeStr, LineThicknessInFullScreenMode);
      WriteInteger(OtherOptionsSectionStr, LocoTimingTimeBeforeAutoStopInSecondsStr, LocoTimingTimeBeforeAutoStopInSeconds);
      WriteBool(OtherOptionsSectionStr, LogCurrentTimeModeStr, LogCurrentTimeMode);
      WriteBool(OtherOptionsSectionStr, LogsKeptModeStr, LogsKeptMode);
      WriteBool(OtherOptionsSectionStr, MakeSoundWhenDebugWindowBoldTextAppearsStr, MakeSoundWhenDebugWindowBoldTextAppears);
      WriteInteger(OtherOptionsSectionStr, MaxRectangleUndrawTimeStr, MaxRectangleUndrawTime);
      WriteBool(OtherOptionsSectionStr, MenusVisibleStr, MenusVisible);
      WriteBool(OtherOptionsSectionStr, MonitorStrayingTrainsStr, MonitorStrayingTrains);
      WriteInteger(OtherOptionsSectionStr, PointFeedbackMaximumWaitInSecondsStr, PointFeedbackMaximumWaitInSeconds);
      WriteInteger(OtherOptionsSectionStr, RouteAheadNotClearWaitTimeInMinutesStr, RouteAheadNotClearWaitTimeInMinutes);
      WriteBool(OtherOptionsSectionStr, RunTestUnitOnStartupStr, RunTestUnitOnStartup);
      WriteBool(OtherOptionsSectionStr, ShowCancelledTrainsInDiagramsStr, ShowCancelledTrainsInDiagrams);
      
      WriteBool(OtherOptionsSectionStr, ShowIncorrectDayOfTheWeekEntriesInWorkingTimetableStr, ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable);
      WriteBool(OtherOptionsSectionStr, ShowNonMovingTrainsInDiagramsStr, ShowNonMovingTrainsInDiagrams);
      WriteBool(OtherOptionsSectionStr, ShowNonStopsInDiagramsStr, ShowNonStopsInDiagrams);
      WriteBool(OtherOptionsSectionStr, ShowTrackCircuitsWhereUserMustDriveStr, ShowTrackCircuitsWhereUserMustDrive);
      WriteBool(OtherOptionsSectionStr, StartRepeatJourneysOnNewLineInDiagramsStr, StartRepeatJourneysOnNewLineInDiagrams);
      WriteBool(OtherOptionsSectionStr, StartWithDiagramsStr, StartWithDiagrams);
      WriteInteger(OtherOptionsSectionStr, StationEndOfDayPassengerLeavingTimeInMinutesStr, StationEndOfDayPassengerLeavingTimeInMinutes);
      WriteInteger(OtherOptionsSectionStr, StationOppositeDirectionExitMinimumWaitTimeInMinutesStr, StationOppositeDirectionExitMinimumWaitTimeInMinutes);
      WriteInteger(OtherOptionsSectionStr, StationSameDirectionExitMinimumWaitTimeInMinutesStr, StationSameDirectionExitMinimumWaitTimeInMinutes);
      WriteBool(OtherOptionsSectionStr, StationStartModeStr, StationStartMode);
      WriteInteger(OtherOptionsSectionStr, StationStartOfDayPassengerBoardingTimeInMinutesStr, StationStartOfDayPassengerBoardingTimeInMinutes);
      WriteBool(OtherOptionsSectionStr, StopAllLocosAtShutDownStr, StopAllLocosAtShutDown);
      WriteBool(OtherOptionsSectionStr, SwitchActiveLocoLightsOffAtShutDownStr, SwitchActiveLocoLightsOffAtShutDown);
      WriteInteger(OtherOptionsSectionStr, TheatreBoxHeightStr, TheatreBoxHeight);
      WriteInteger(OtherOptionsSectionStr, TheatreBoxWidthStr, TheatreBoxWidth);
      WriteInteger(OtherOptionsSectionStr, WaitBeforeRerouteInMinutesStr, WaitBeforeRerouteInMinutes);
      WriteBool(OtherOptionsSectionStr, WorkingTimetableModeStr, WorkingTimetableMode);
      WriteBool(OtherOptionsSectionStr, WritingStationMonitorsDisplayToFileStr, WritingStationMonitorsDisplayToFile);

      { Various times }
      WriteString(TimesSectionStr, ProgramStartTimeOptionStr, TimeToHMSStr(ProgramStartTime));
      WriteString(TimesSectionStr, DayLightStartTimeOptionStr, TimeToHMSStr(DayLightStartTime));
      WriteString(TimesSectionStr, DayLightEndTimeOptionStr, TimeToHMSStr(DayLightEndTime));
      WriteString(TimesSectionStr, CurrentRailwayDayOfTheWeekStr, DayOfTheWeekToStr(CurrentRailwayDayOfTheWeek));

      { Screen settings }
      WriteInteger(ScreenOptionsStr, LogFileMaxWidthInCharsStr, LogFileMaxWidthInChars);

      CASE ScreenMode OF
        DefaultWindowedScreenMode:
          WriteString(ScreenOptionsStr, ScreenModeStr, DefaultWindowedScreenStr);
        FullScreenMode:
          WriteString(ScreenOptionsStr, ScreenModeStr, FullScreenStr);
        FullScreenWithStatusBarMode:
          WriteString(ScreenOptionsStr, ScreenModeStr, FullScreenWithStatusBarStr);
        CustomWindowedScreenMode:
          WriteString(ScreenOptionsStr, ScreenModeStr, CustomWindowedScreenStr);
      END; {CASE}

      WriteInteger(ScreenOptionsStr, BufferStopVerticalSpacingStr, BufferStopVerticalSpacing);
      WriteInteger(ScreenOptionsStr, DeltaPointXStr, DeltaPointX);
      WriteInteger(ScreenOptionsStr, IndicatorHorizontalSpacingStr, IndicatorHorizontalSpacing);
      WriteInteger(ScreenOptionsStr, IndicatorVerticalSpacingStr, IndicatorVerticalSpacing);
      WriteInteger(ScreenOptionsStr, MouseRectangleEdgeVerticalSpacingStr, MouseRectangleEdgeVerticalSpacing);
      WriteInteger(ScreenOptionsStr, PlatformEdgeVerticalSpacingStr, PlatformEdgeVerticalSpacing);
      WriteInteger(ScreenOptionsStr, PlatformNumberEdgeHorizontalSpacingStr, PlatformNumberEdgeHorizontalSpacing);
      WriteInteger(ScreenOptionsStr, PlatformNumberEdgeVerticalSpacingStr, PlatformNumberEdgeVerticalSpacing);
      WriteInteger(ScreenOptionsStr, SignalHorizontalSpacingStr, SignalHorizontalSpacing);
      WriteInteger(ScreenOptionsStr, SignalRadiusStr, SignalRadius);
      WriteInteger(ScreenOptionsStr, SignalSemaphoreHeightStr, SignalSemaphoreHeight);
      WriteInteger(ScreenOptionsStr, SignalSemaphoreWidthStr, SignalSemaphoreWidth);
      WriteInteger(ScreenOptionsStr, SignalVerticalSpacingStr, SignalVerticalSpacing);
      WriteInteger(ScreenOptionsStr, SpeedRestrictionHorizontalSpacingStr, SpeedRestrictionHorizontalSpacing);
      WriteInteger(ScreenOptionsStr, SpeedRestrictionVerticalSpacingStr, SpeedRestrictionVerticalSpacing);
      WriteInteger(ScreenOptionsStr, TheatreIndicatorHorizontalSpacingStr, TheatreIndicatorHorizontalSpacing);
      WriteInteger(ScreenOptionsStr, TheatreIndicatorVerticalSpacingStr, TheatreIndicatorVerticalSpacing);
      WriteInteger(ScreenOptionsStr, TRSPlungerLengthStr, TRSPlungerLength);

      { Other Miscellaneous Data }
      WriteString(MiscellaneousDataSectionStr, CurrentParametersStr, CurrentParametersFromParamStr);
    END; {WITH}
    IniFile.Free;
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

      { Filenames }
      Values[FilenamesStr] := '';
      ItemProps[FilenamesStr].ReadOnly := True;

      Values[AreaDataFilenameStr] := AreaDataFilename;
      Values[AreaDataFilenameSuffixStr] := AreaDataFilenameSuffix;
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
      Values[StationMonitorsDataFilenameStr] := StationMonitorsDataFilename;
      Values[StationMonitorsDataFilenameSuffixStr] := StationMonitorsDataFilenameSuffix;
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

      Values[IncludeLocoChipInStationMonitorsStr] := BoolToStr(IncludeLocoChipInStationMonitors, True);
      ItemProps[IncludeLocoChipInStationMonitorsStr].PickList.Add('True');
      ItemProps[IncludeLocoChipInStationMonitorsStr].PickList.Add('False');
      ItemProps[IncludeLocoChipInStationMonitorsStr].EditStyle := esPickList;

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

      Values[StationOppositeDirectionExitMinimumWaitTimeInMinutesStr] := IntToStr(StationOppositeDirectionExitMinimumWaitTimeInMinutes);

      Values[StationSameDirectionExitMinimumWaitTimeInMinutesStr] := IntToStr(StationSameDirectionExitMinimumWaitTimeInMinutes);

      Values[StationStartModeStr] := BoolToStr(StationStartMode, True);
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

      Values[WaitBeforeRerouteInMinutesStr] := IntToStr(WaitBeforeRerouteInMinutes);

      Values[WorkingTimetableModeStr] := BoolToStr(WorkingTimetableMode, True);
      ItemProps[WorkingTimetableModeStr].PickList.Add('True');
      ItemProps[WorkingTimetableModeStr].PickList.Add('False');
      ItemProps[WorkingTimetableModeStr].EditStyle := esPickList;

      Values[WritingStationMonitorsDisplayToFileStr] := BoolToStr(WritingStationMonitorsDisplayToFile, True);
      ItemProps[WritingStationMonitorsDisplayToFileStr].PickList.Add('True');
      ItemProps[WritingStationMonitorsDisplayToFileStr].PickList.Add('False');
      ItemProps[WritingStationMonitorsDisplayToFileStr].EditStyle := esPickList;

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
    IF (OldValue <> '')
    AND (NewValue = '')
    THEN BEGIN
      IF MessageDialogueWithDefault(KeyName + ': do you want to delete ''' + OldValue + '''?',
                                    StopTimer, mtError, [mbYes, mbNo], mbNo) = mrYes
      THEN
        Result := True;
    END ELSE
      IF (OldValue = '')
      AND (NewValue <> '')
      THEN BEGIN
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
      IF (KeyNameToTest = OriginalKeyName)
      AND (NewKeyValue <> OriginalKeyValue)
      THEN BEGIN
        { Check the confirmed values are allowed }
        ExtractSubStringsFromString(SetValues, ',', ValuesArray);
        I := 0;
        ValueFound := False;
        WHILE (I <= High(ValuesArray))
        AND NOT ValueFound
        DO BEGIN
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
      IF (KeyNameToTest = OriginalKeyName)
      AND (NewKeyValue <> OriginalKeyValue)
      THEN BEGIN
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
      IF (KeyNameToTest = OriginalKeyName)
      AND (NewKeyValue <> IntToStr(OriginalKeyValue))
      THEN BEGIN
        IF NOT TryStrToInt(NewKeyValue, OriginalKeyValue) THEN BEGIN
          ShowMessage('Invalid integer value ''' + NewKeyValue + '');
          Values[OriginalKeyName] := IntToStr(OriginalKeyValue);
        END;
      END;
    END; {WITH}
  END; { CheckIntegerValueListValue }

  PROCEDURE CheckCardinalValueListValue(KeyNameToTest, OriginalKeyName, NewKeyValue : String; VAR OriginalKeyValue : Cardinal);
  { Verify and replace an cardinal value in a value list  }
  VAR
    TempInt : Integer;

  BEGIN
    WITH OptionsWindow.OptionsValueListEditor DO BEGIN
      IF (KeyNameToTest = OriginalKeyName)
      AND (NewKeyValue <> IntToStr(OriginalKeyValue))
      THEN BEGIN
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
      IF (KeyNameToTest = OriginalKeyName)
      AND (NewKeyValue <> BoolToStr(OriginalKeyValue))
      THEN BEGIN
        IF NOT TryStrToBool(NewKeyValue, OriginalKeyValue) THEN BEGIN
          ShowMessage('Invalid boolean value ''' + NewKeyValue + '');
          Values[OriginalKeyName] := BoolToStr(OriginalKeyValue, True);
        END;
      END;
    END; {WITH}
  END; { CheckBooleanValueListValue }

BEGIN
  TRY
    WITH OptionsWindow.OptionsValueListEditor DO BEGIN
      { Paths and Directories }
      CheckStringValueListValue(KeyName, PathToLogFilesStr, NewKeyValue, PathToLogFiles);
      CheckStringValueListValue(KeyName, PathToRailDataFilesStr, NewKeyValue, PathToRailDataFiles);

      { Filenames - we make sure that changes to these are confirmed }
      CheckStringValueListValue(KeyName, AreaDataFilenameStr, NewKeyValue, AreaDataFilename);
      CheckStringValueListValue(KeyName, AreaDataFilenameSuffixStr, NewKeyValue, AreaDataFilenameSuffix);
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
      CheckStringValueListValue(KeyName, StationMonitorsDataFilenameStr, NewKeyValue, StationMonitorsDataFilename);
      CheckStringValueListValue(KeyName, StationMonitorsDataFilenameSuffixStr, NewKeyValue, StationMonitorsDataFilenameSuffix);
      CheckStringValueListValue(KeyName, TrackCircuitDataFilenameStr, NewKeyValue, TrackCircuitDataFilename);
      CheckStringValueListValue(KeyName, TrackCircuitDataFilenameSuffixStr, NewKeyValue, TrackCircuitDataFilenameSuffix);
      CheckStringValueListValue(KeyName, WorkingTimetableFilenameStr, NewKeyValue, WorkingTimetableFilename);
      CheckStringValueListValue(KeyName, WorkingTimetableFilenameSuffixStr, NewKeyValue, WorkingTimetableFilenameSuffix);

      { Full Screen }
      IF (KeyName = ScreenModeStr)
      AND (NewKeyValue <> ScreenModeStr)
      THEN BEGIN
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

      { Times }
      IF (KeyName = ProgramStartTimeOptionStr)
      AND (NewKeyValue <> ProgramStartTimeStr)
      THEN BEGIN
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

      IF (KeyName = DayLightEndTimeOptionStr)
      AND (NewKeyValue <> DayLightEndTimeStr)
      THEN BEGIN
        IF TimeIsValid(NewKeyValue) THEN
          DayLightEndTime := StrToTime(NewKeyValue)
        ELSE BEGIN
          ShowMessage('Invalid day light end time : ' + NewKeyValue);
          Values[DayLightEndTimeOptionStr] := DayLightEndTimeStr;
        END;
      END;

      IF (KeyName = DayLightStartTimeOptionStr)
      AND (NewKeyValue <> DayLightStartTimeStr)
      THEN BEGIN
        IF TimeIsValid(NewKeyValue) THEN
          DayLightStartTime := StrToTime(NewKeyValue)
        ELSE BEGIN
          ShowMessage('Invalid day light start time: ' + NewKeyValue);
          Values[DayLightStartTimeOptionStr] := DayLightStartTimeStr;
        END;
      END;

      IF (KeyName = CurrentRailwayDayOfTheWeekStr)
      AND (NewKeyValue <> DayOfTheWeekToStr(CurrentRailwayDayOfTheWeek))
      THEN BEGIN
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
      CheckBooleanValueListValue(KeyName, IncludeLocoChipInStationMonitorsStr, NewKeyValue, IncludeLocoChipInStationMonitors);
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
      CheckIntegerValueListValue(KeyName, StationOppositeDirectionExitMinimumWaitTimeInMinutesStr, NewKeyValue, StationOppositeDirectionExitMinimumWaitTimeInMinutes);
      CheckIntegerValueListValue(KeyName, StationSameDirectionExitMinimumWaitTimeInMinutesStr, NewKeyValue, StationSameDirectionExitMinimumWaitTimeInMinutes);
      CheckBooleanValueListValue(KeyName, StationStartModeStr, NewKeyValue, StationStartMode);
      CheckIntegerValueListValue(KeyName, StationStartOfDayPassengerBoardingTimeInMinutesStr, NewKeyValue, StationStartOfDayPassengerBoardingTimeInMinutes);
      CheckBooleanValueListValue(KeyName, StopAllLocosAtShutDownStr, NewKeyValue, StopAllLocosAtShutDown);
      CheckBooleanValueListValue(KeyName, SwitchActiveLocoLightsOffAtShutDownStr, NewKeyValue, SwitchActiveLocoLightsOffAtShutDown);
      CheckIntegerValueListValue(KeyName, TheatreBoxHeightStr, NewKeyValue, TheatreBoxHeight);
      CheckIntegerValueListValue(KeyName, TheatreBoxWidthStr, NewKeyValue, TheatreBoxWidth);
      CheckIntegerValueListValue(KeyName, WaitBeforeRerouteInMinutesStr, NewKeyValue, WaitBeforeRerouteInMinutes);
      CheckBooleanValueListValue(KeyName, WorkingTimetableModeStr, NewKeyValue, WorkingTimetableMode);
      CheckBooleanValueListValue(KeyName, WritingStationMonitorsDisplayToFileStr, NewKeyValue, WritingStationMonitorsDisplayToFile);

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
      Log('EG FillOptionsValueList: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SaveOptionFromValueList }

PROCEDURE TOptionsWindow.OptionsValueListEditorValidate(Sender: TObject; ACol, ARow: Integer; CONST KeyName, KeyValue: String);
BEGIN
  SaveOptionFromValueList(KeyName, KeyValue);
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
END; { InitialiseOptionsUnit }

PROCEDURE TOptionsWindow.OptionsWindowFindDialogFind(Sender: TObject);
//VAR
//  FoundPos : Integer;

BEGIN
//  WITH OptionsWindow DO BEGIN
//    FindDialogFind(OptionsWindowFindDialog, OptionsValueListEditor.Strings.Text, FoundPos);
//    IF FoundPos > 0 THEN BEGIN
//      debug;
//        HelpRichEdit.SelStart := PreviousFoundPos - 1;
//        HelpRichEdit.SelLength := Length(HelpWindowFindDialog.FindText);
//        HelpRichEdit.SetFocus;
//        HelpRichEdit.Perform(EM_SCROLLCARET, 0, 0);
//    END ELSE
//      ShowMessage('Could not find "' + HelpWindowFindDialog.FindText + '"');
//  END; {WITH}
END; { OptionsWindowFindDialogFind }

PROCEDURE SearchOptionsText(S : String);
{ Search the Options Window text }
BEGIN
  PreviousFoundPos := 0;
  WITH OptionsWindow DO BEGIN
    OptionsWindowFindDialog.Position := Point(OptionsWindow.Left + (OptionsWindow.Width DIV 2), OptionsWindow.Top);
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
    IF (Key = Ord('F'))
    AND (ssCtrl IN Shift)
    THEN BEGIN
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
