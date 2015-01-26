UNIT RailDraw;
{ Handles graphics display for railway program

  Copyright © F.W. Pritchard 1988-2015. All Rights Reserved.

  v0.1  00/00/88 Unit created
  v0.2  24/04/13 Header added; semaphore signal routine rewritten by James D King and FWP
  v1.0  08/04/14 Separated out screen drawing
}

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Forms, Dialogs, Menus, Initvars, StdCtrls, ComCtrls, ExtCtrls, Controls, ActnList, DB, ADODB, Buttons,
     System.UITypes, Vcl.AppEvnts;

TYPE
  TFWPRailWindow = CLASS(TForm)
    FlashTimer: TTimer;
    BufferStopPopupMenu: TPopupMenu;
    FWPRailApplicationEvents: TApplicationEvents;
    FWPRailWindowColourDialogue: TColorDialog;
    FWPRailWindowMenu: TMainMenu;
    FWPRailWindowPopupOpenDialogue: TOpenDialog;
    FWPRailWindowStatusBar: TStatusBar;
    GeneralPopupMenu: TPopupMenu;
    LinePopupMenu: TPopupMenu;
    MainDropDownMenuDisplay: TMenuItem;
    MainDropdownMenuDisplayShowDebugOutputWindow: TMenuItem;
    MainDropdownMenuDisplayDiagramsWindow: TMenuItem;
    MainDropdownMenuDisplayShowMainMenu: TMenuItem;
    MainDropdownMenuDisplayShowStatusBar: TMenuItem;
    MainDropdownMenuDisplayWorkingTimetableWindow: TMenuItem;
    MainDropdownMenuDisplayZoomScreen: TMenuItem;
    MainDropDownMenuFile: TMenuItem;
    MainDropDownMenuFileExit: TMenuItem;
    MainDropdownMenuHelp: TMenuItem;
    MainDropDownMenuHelpAboutRail: TMenuItem;
    MainDropdownMenuHelpRailHelp: TMenuItem;
    PointPopupMenu: TPopupMenu;
    PopupTimer: TTimer;
    SignalPopupMenu: TPopupMenu;

    PROCEDURE BufferStopPopupItemClick(Sender: TObject);
    PROCEDURE BufferStopMenuOnPopup(Sender: TObject);
    PROCEDURE FlashTimerTick(Sender: TObject);
    PROCEDURE FWPRailApplicationEventsShortCut(VAR Msg: TWMKey; VAR Handled: Boolean);
    PROCEDURE FWPRailWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE FWPRailWindowDestroy(Sender: TObject);
    PROCEDURE FWPRailWindowDragDrop(Sender, Source: TObject; X, Y: Integer);
    PROCEDURE FWPRailWindowDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    PROCEDURE FWPRailWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE FWPRailWindowMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE FWPRailWindowMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE FWPRailWindowMouseUp(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE FWPRailWindowMouseWheel(Sender: TObject; ShiftState: TShiftState; WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
    PROCEDURE FWPRailWindowPaint(Sender: TObject);
    PROCEDURE FWPRailWindowResize(Sender: TObject);
    PROCEDURE FWPRailWindowShortCut(VAR Msg: TWMKey; VAR Handled: Boolean);
    PROCEDURE FWPRailWindowStatusBarClick(Sender: TObject);
    PROCEDURE FWPRailWindowStatusBarMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE GeneralPopupItemClick(Sender: TObject);
    PROCEDURE GeneralPopupMenuOnPopup(Sender: TObject);
    PROCEDURE HelpMenuAboutClick(Sender: TObject);
    PROCEDURE LinePopupItemClick(Sender: TObject);
    PROCEDURE LinePopupMenuOnPopup(Sender: TObject);
    PROCEDURE LocoInfoMenuItemClick(Sender: TObject);
    PROCEDURE MainDropdownMenuDisplayShowDebugOutputWindowClick(Sender: TObject);
    PROCEDURE MainDropdownMenuDisplayDiagramsWindowClick(Sender: TObject);
    PROCEDURE MainDropdownMenuDisplayShowMainMenuClick(Sender: TObject);
    PROCEDURE MainDropdownMenuDisplayWorkingTimetableWindowClick(Sender: TObject);
    PROCEDURE MainDropdownMenuDisplayZoomScreenClick(Sender: TObject);
    PROCEDURE MainDropDownMenuFileExitClick(Sender: TObject);
    PROCEDURE MainDropdownMenuHelpRailHelpClick(Sender: TObject);
    PROCEDURE MainRunMenuResumeOperationsClick(Sender: TObject);
    PROCEDURE PointPopupMenuOnPopup(Sender: TObject);
    PROCEDURE PointPopupItemClick(Sender: TObject);
    PROCEDURE PopupTimerTick(Sender: TObject);
    PROCEDURE ResetSizeAndPositionOfAllWindowsClick(Sender: TObject);
    PROCEDURE ShowStatusBarClick(Sender: TObject);
    PROCEDURE SignalPopupItemClick(Sender: TObject);
    PROCEDURE SignalPopupMenuOnPopup(Sender: TObject);

  PRIVATE
    { Private declarations }
//    PROCEDURE ApplicationMessage(VAR Msg: TMsg; VAR Handled: Boolean);
//    { Intercept messages - only way of getting at the tab key! Now replaced by ShortCut above Sept 2009 }

    PROCEDURE wmSize( var msg: TWMSize ); message WM_SIZE;
    { Called when a window has been resized }

    PROCEDURE WMCopyData(VAR Msg : TWMCopyData); Message WM_COPYDATA;
    { Receives data from the Watchdog program }
  PUBLIC
    { Public declarations }
    PROCEDURE WMHScroll(VAR ScrollData: TMessage); MESSAGE wm_HScroll;
    { Added to allow interception of scroll bar events }
    PROCEDURE WMVScroll(VAR ScrollData: TMessage); MESSAGE wm_VScroll;
    { Added to allow interception of scroll bar events }
  END;

  MenuPopupTypes = (NoClickPopupType,
                    { General Menu }
                    ShowMainMenusPopupType, ClockSubMenuPopupType, StartClockPopupType, StopClockPopupType,
                    RunClockNormallyPopupType, RunClockSlowerPopupType, RunClockFasterPopupType, RunClockFastestPopupType,
                    SetCurrentRailwayTimePopupType, SetCurrentRailwayDayOfTheWeekPopupType, SetProgramStartTimePopupType,
                    SetDaylightStartTimePopupType, SetDaylightEndTimePopupType,

                    ChangeBackgroundColourPopupType, RestoreBackgroundColourPopupType,
                    ChangeForegroundColourPopupType, RestoreForegroundColourPopupType,
                    ChangeBufferStopColourPopupType, RestoreBufferStopColourPopupType,
                    ChangeBufferStopNumberColourPopupType, RestoreBufferStopNumberColourPopupType,
                    ChangeBufferStopRedPopupType, RestoreBufferStopRedPopupType,
                    ChangeLineRoutedOverColourPopupType, RestoreLineRoutedOverColourPopupType,
                    ChangeLineNotAvailableColourPopupType, RestoreLineNotAvailableColourPopupType,
                    ChangeLocoStalledColourPopupType, RestoreLocoStalledColourPopupType,
                    ChangeTCFeedbackOccupationColourPopupType, RestoreTCFeedbackOccupationColourPopupType,
                    ChangeTCFeedbackOccupationButOutOfUseColourPopupType, RestoreTCFeedbackOccupationButOutOfUseColourPopupType,
                    ChangeTCFeedbackDataInUseColourPopupType, RestoreTCFeedbackDataInUseColourPopupType,
                    ChangeTCFeedbackDataOutOfUseColourPopupType, RestoreTCFeedbackDataOutOfUseColourPopupType,
                    ChangeTCMissingOccupationColourPopupType, RestoreTCMissingOccupationColourPopupType,
                    ChangeTCPermanentFeedbackOccupationColourPopupType, RestoreTCPermanentFeedbackOccupationColourPopupType,
                    ChangeTCPermanentOccupationSetByUserColourPopupType, RestoreTCPermanentOccupationSetByUserColourPopupType,
                    ChangeTCPermanentSystemOccupationColourPopupType, RestoreTCPermanentSystemOccupationColourPopupType,
                    ChangeTCSpeedRestrictionColourPopupType, RestoreTCSpeedRestrictionColourPopupType,
                    ChangeTCSystemOccupationColourPopupType, RestoreTCSystemOccupationColourPopupType,
                    ChangeTCOutOfUseSetByUserColourPopupType, RestoreTCOutOfUseSetByUserColourPopupType,
                    ChangeTCOutOfUseAsNoFeedbackReceivedColourPopupType, RestoreTCOutOfUseAsNoFeedbackReceivedColourPopupType,
                    ChangeTCUnoccupiedColourPopupType, RestoreTCUnoccupiedColourPopupType,

                    ChangePlatformColourPopupType, RestorePlatformColourPopupType,
                    ChangeTRSPlungerColourPopupType, RestoreTRSPlungerColourPopupType,
                    ChangeTRSPlungerPressedColourPopupType, RestoreTRSPlungerPressedColourPopupType,
                    ChangeTRSPlungerOutlineColourPopupType, RestoreTRSPlungerOutlineColourPopupType,

                    ChangeDefaultPointColourPopupType, RestoreDefaultPointColourPopupType,
                    ChangePointDivergingLineColourPopupType, RestorePointDivergingLineColourPopupType,
                    ChangePointDownFacingColourPopupType, RestorePointDownFacingColourPopupType,
                    ChangePointFeedbackDataInUseColourPopupType, RestorePointFeedbackDataInUseColourPopupType,
                    ChangePointFeedbackDataOutOfUseColourPopupType, RestorePointFeedbackDataOutOfUseColourPopupType,
                    ChangePointHeelLineColourPopupType, RestorePointHeelLineColourPopupType,
                    ChangePointLenzNumberColourPopupType, RestorePointLenzNumberColourPopupType,
                    ChangePointLockedBySystemColourPopupType, RestorePointLockedBySystemColourPopupType,
                    ChangePointLockedByUserColourPopupType, RestorePointLockedByUserColourPopupType,
                    ChangePointManualOperationColourPopupType, RestorePointManualOperationColourPopupType,
                    ChangePointOutOfUseColourPopupType, RestorePointOutOfUseColourPopupType,
                    ChangePointStraightLineColourPopupType, RestorePointStraightLineColourPopupType,
                    ChangePointUndrawColourPopupType, RestorePointUndrawColourPopupType,
                    ChangePointUpFacingColourPopupType, RestorePointUpFacingColourPopupType,
                    ChangePointsWithoutFeedbackColourPopupType, RestorePointsWithoutFeedbackColourPopupType,
                    ChangePointDefaultStateColourPopupType, RestorePointDefaultStateColourPopupType,

                    ChangeSignalAspectRedPopupType, RestoreSignalAspectRedPopupType,
                    ChangeSignalAspectGreenPopupType, RestoreSignalAspectGreenPopupType,
                    ChangeSignalAspectYellowPopupType, RestoreSignalAspectYellowPopupType,
                    ChangeSignalAspectUnlitPopupType, RestoreSignalAspectUnlitPopupType,
                    ChangeSignalNumberColourPopupType, RestoreSignalNumberColourPopupType,
                    ChangeSignalPostBaseColourPopupType, RestoreSignalPostBaseColourPopupType,
                    ChangeSignalPostRouteSettingColourPopupType, RestoreSignalPostRouteSettingColourPopupType,
                    ChangeSignalPostEmergencyRouteSettingColourPopupType, RestoreSignalPostEmergencyRouteSettingColourPopupType,
                    ChangeSignalPostTheatreSettingColourPopupType, RestoreSignalPostTheatreSettingColourPopupType,
                    ChangeSignalsFromWhichUserMustDriveSignalPostColourPopupType, RestoreSignalsFromWhichUserMustDriveSignalPostColourPopupType,

                    ChangeTrainActiveColourPopupType, RestoreTrainActiveColourPopupType,
                    ChangeTrainInactiveColourPopupType, RestoreTrainInactiveColourPopupType,
                    ChangeScreenComponentEditedColour1PopupType, RestoreScreenComponentEditedColour1PopupType,
                    ChangeScreenComponentEditedColour2PopupType, RestoreScreenComponentEditedColour2PopupType,

                    RestoreAllDefaultColoursPopupType,

                    SolidPenStylePopupType, DashPenStylePopupType, DotPenStylePopupType, DashDotPenStylePopupType, DashDotDotPenStylePopupType, ClearPenStylePopupType,
                    InsideFramePenStylePopupType,

                    ChangePointPopupType, ChangeSignalPopupType, ListLocomotivesPopupType, ShowTrackCircuitPopupType, DebugOptionsPopupType,

                    ResetMainWindowSizeAndPositionPopupType, ResetSizeAndPositionOfAllWindowsPopupType, RestoreAllScreenDrawingDefaultSettingsPopupType,
                    RestoreAllProgramDefaultSettingsPopupType,

                    { Signals Menu }
                    SignalChangeDirectionPopupType, SignalDeletePopupType, SignalEditPopupType, SignalOutOfUsePopupType, SignalUndoChangesPopupType,
                    SignalUserMustDriveFromPopupType,

                    { Points Menu}
                    PointDeletePopupType, PointEditPopupType, PointOutOfUsePopupType, PointToManualPopupType, PointUnlockPopupType,

                    { BufferStops Menu }
                    BufferStopEditPopupType,
                    { Lines with locos }
                    LineAllocateLocoToTrackCircuitPopupType, LineChangeInternalLocoDirectionToDownPopupType, LineChangeInternalLocoDirectionToUpPopupType,
                    { Lines with points }
                    LineCreateCatchPointUpPopupType, LineCreateCatchPointDownPopupType, LineCreateOrdinaryPointPopupType, LineCreateCrossOverPointPopupType,
                    LineCreateThreeWayPointAPopupType, LineCreateThreeWayPointBPopupType,
                    { Lines with signals }
                    LineCreateDownSignalPopupType, LineCreateUpSignalPopupType,
                    { Lines out of use }
                    LineLocationOutOfUsePopupType, LineOutOfUsePopupType,
                    { Lines miscellaneous }
                    LineDeletePopupType,
                    LineEnterCreateLineModePopupType, LineExitCreateLineModePopupType, LineEnterEditModePopupType, LineExitEditModePopupType,
                    LineEditPopupType,
                    LineShowLocoLastErrorMessagePopupType,
                    LineSplitPopupType, LineJoinPopupType,
                    { Line track circuits }
                    LineTCFeedbackOccupationPopupType, LineTCOutOfUsePopupType, LineTCPermanentOccupationPopupType, LineTCSpeedRestrictionPopupType,
                    LineTCSystemOccupationPopupType, LineTCUnoccupiedPopupType, LineAllocateExistingTrackCircuitPopupType,
                    LineAllocateNewTrackCircuitPopupType, LineRemoveTrackCircuitPopupType);

  PenStylePopupTypes = (SidingPenStylePopupType, FiddleyardLinePenStylePopupType, ProjectedLinePenStylePopupType, SignalsFromWhichUserMustDriveSignalPostPenStylePopupType,
                        TCOutOfUseAsNoFeedbackReceivedPenStylePopupType, TCOutOfUseSetByUserPenStylePopupType, TCPermanentFeedbackOccupationPenStylePopupType,
                        TCPermanentOccupationSetByUserPenStylePopupType, TCPermanentSystemOccupationPenStylePopupType, TCLocoOutOfPlacePenStylePopupType,
                        NoPenStylePopupType);

  TMenuItemExtended = CLASS(TMenuItem)
  PRIVATE
    fValue: String;
    fMenuPopupType : MenuPopupTypes;
    fPenStylePopupType : PenStylePopupTypes;

  PUBLISHED
    PROPERTY Value : String Read fValue Write fValue;
    PROPERTY MenuPopupType : MenuPopupTypes Read fMenuPopupType Write fMenuPopupType;
    PROPERTY PenStylePopupType : PenStylePopupTypes Read fPenStylePopupType Write fPenStylePopupType;
  END;

PROCEDURE CanvasTextOutAngle(X, Y : Integer; D : Word; S : String);
{ D is in tenths if a degree - i.e. 450 - 45 degrees. This is not used, but might come in useful }

PROCEDURE ChangeCursor(NewCursor : TCursor);
{ Change the shape of the cursor (from the Delphi Help system) }

PROCEDURE ClearLinePopupNumArray;
{ Empties the line popup num array }

PROCEDURE DrawAllPoints;
{ Draw all the points }

PROCEDURE DrawAllSignals(ShowSignalAndBufferStopNums, ShowTheatreDestinations : Boolean);
{ Draw all the signals }

PROCEDURE DrawBufferStop(BufferStopNum : Integer; Colour : TColour);
{ Draw a buffer stop }

PROCEDURE DrawBufferStopData(B : Integer; BufferStopText : String; Colour : TColor);
{ Put the bufferstop name or other supplied data on the diagram }

PROCEDURE DrawConnectionCh(Line : Integer; Direction : DirectionType);
{ Draw character at line starts/ends to indicate where lines are going when they disappear off the screen }

PROCEDURE DrawFailure(Device : Integer; ActionCh : String);
{ Describes the offending items in the status bar }

PROCEDURE DrawLine{1}(Line : Integer; NewLineColour : Integer; ActiveTrain : Boolean); Overload;
{ Draw an individual line, with headcode if required, and store the line colour }

PROCEDURE DrawLine{2}(Line : Integer; NewLineColour : Integer; ActiveTrain : Boolean; TempLineText : String); Overload;
{ Draw an individual line, with headcode if required, and store the line colour }

PROCEDURE DrawMap;
{ Draws the track layout }

PROCEDURE DrawOutline(NewRect : TRect; Colour : TColour; UndrawRequired, UndrawToBeAutomatic : Boolean); Overload;
{ We need this as the default Delphi Rectangle is filled in }

PROCEDURE DrawOutline(FWPPolygon : ARRAY OF TPoint; Colour : TColour; UndrawRequired, UndrawToBeAutomatic : Boolean); Overload;
{ We need this as the default Delphi Rectangle is filled in }

PROCEDURE DrawPoint(P : Integer; Colour : TColour);
{ Draw a point }

PROCEDURE DrawPointNum(P : Integer; Colour : TColour);
{ Put the number of the point on the diagram }

PROCEDURE DrawSignal(S : Integer);
{ Draw a signal at the current position. We need to know if it is for up or down traffic, a home or distant or calling on, and what aspect it is. Signal_Line.X is the
  of the main aspect
}
PROCEDURE DrawSignalData(S : Integer; Str : String; Colour : Integer);
{ Put the signal name or other supplied data on the diagram }

PROCEDURE DrawSignalPost(S : Integer);
{ Draws the signal post using Signal_PostColour }

PROCEDURE DrawSpeedRestrictions;
{ Draw speed restrictions next to lines - but only draw one sign per track circuit }

PROCEDURE DrawTrackCircuit{1}(TC : Integer; TCColour : TColour); Overload;
{ Draws a given track circuit }

PROCEDURE DrawTrackCircuit{2}(TC : Integer; TCColour : TColour; TempLineText : String); Overload;
{ Draws a given track circuit - this version is used by Replay to add train descriptions }

PROCEDURE DrawTrackCircuitsWithAdjoiningTrackCircuits(TC : Integer; TCColour1, TCColour2 : TColour);
{ Draw a track circuit and show which track circuits adjoin it }

PROCEDURE DrawTRSPlunger(Location : Integer; Pressed : Boolean);
{ Indicate on a platform that a train-ready-to-start plunger has been pressed }

FUNCTION GetSaveCursor : TCursor;
{ Return the SaveCursor variable state }

PROCEDURE HideStatusBarAndUpDownIndications;
{ Before a zoomed screen move, hide the status bar and the "up" and "down" markers }

PROCEDURE InitialiseRailDrawUnit;
{ Initialises the unit }

PROCEDURE InvalidateScreen(UnitRefParam, CallingStr : String);
{ Draw the screen by invalidating it }

PROCEDURE ResetAllWindowsSizeAndPosition;
{ Reset all the windows to their default state }

PROCEDURE ResetFWPRailWindowSizeAndPosition;
{ Reset the window's size and position }

PROCEDURE ResetScreenColoursAfterPrinting;
{ Restore the colours to those saved before printing the screen in printer-friendly colours }

PROCEDURE SetBufferStopPopupNum(Num : Integer);
{ Assign to the buffer stop popup number }

PROCEDURE SetCaption(Window : TForm; Caption : String);
{ Sets a window caption }

PROCEDURE SetLinePopupNumArray(NumArray : IntegerArrayType);
{ Assign to the LinePopupNumArray }

PROCEDURE SetPointPopupNum(Num : Integer);
{ assign to the PointPopupNum variable state }

PROCEDURE SetSignalPopupNum(Num : Integer);
{ Assign to the SignalPopupNum variable state }

PROCEDURE SetScreenColoursBeforePrinting;
{ Save the screen colours before printing the screen in printer-friendly colours }

PROCEDURE ShowStatusBarAndUpDownIndications;
{ After a zoomed screen move, restore the status bar and the "up" and "down" markers }

PROCEDURE WriteToStatusBarPanel(PanelNum : Integer; Str : String);
{ Write the text in the chosen panel }

CONST
  crCrossHair = 5;
  crCrossHairForUpSignal = 6;
  crCrossHairForDownSignal = 7;
  crPointLever = 8;
  crArrowRed = 9;

VAR
  FWPRailWindow : TFWPRailWindow;
  PopupTimerCount : Integer = 0;
  RailWindowBitmap : TBitmap;
  SaveBackgroundColourForPrinting : TColor;
  SaveBufferStopColourForPrinting : TColor;
  SaveBufferStopNumberColourForPrinting : TColor;
  SaveBufferStopRedForPrinting : TColor;
  SaveForegroundColourForPrinting : TColor;
  SaveLineNotAvailableColourForPrinting : TColor;
  SaveLineRoutedOverColourForPrinting : TColor;
  SaveLocoStalledColourForPrinting : TColor;
  SavePlatformColourForPrinting : TColor;
  SavePointColourForPrinting : TColor;
  SavePointDefaultStateColourForPrinting : TColor;
  SavePointDivergingLineColourForPrinting : TColor;
  SavePointDownFacingColourForPrinting : TColor;
  SavePointFeedbackDataInUseColourForPrinting : TColor;
  SavePointFeedbackDataOutOfUseColourForPrinting : TColor;
  SavePointHeelLineColourForPrinting : TColor;
  SavePointLenzNumberColourForPrinting : TColor;
  SavePointLockedBySystemColourForPrinting : TColor;
  SavePointManualOperationColourForPrinting : TColor;
  SavePointStraightLineColourForPrinting :TColor;
  SavePointsWithoutFeedbackColourForPrinting : TColor;
  SavePointUpFacingColourForPrinting : TColor;
  SaveShowSignalJunctionDestinations : Boolean = False;
  SaveScreenComponentEditedColour1ForPrinting : TColor;
  SaveScreenComponentEditedColour2ForPrinting : TColor;
  SaveSignalNumberColourForPrinting : TColor;
  SaveSignalPostColourForPrinting : TColor;
  SaveSignalPostRouteSettingColourForPrinting : TColor;
  SaveSignalPostTheatreSettingColourForPrinting : TColor;
  SaveSignalsFromWhichUserMustDriveSignalPostColour : TColor;
  SaveTCMissingOccupationColourForPrinting : TColor;
  SaveTCFeedbackOccupationColourForPrinting : TColor;
  SaveTCOutOfUseSetByUserColourForPrinting : TColor;
  SaveTCOutOfUseAsNoFeedbackReceivedColourForPrinting : TColor;
  SaveTCPermanentFeedbackOccupationColourForPrinting : TColor;
  SaveTCPermanentOccupationSetByUserColourForPrinting : TColor;
  SaveTCPermanentSystemOccupationColourForPrinting : TColor;
  SaveTCSystemOccupationColourForPrinting : TColor;
  SaveTCUnoccupiedColourForPrinting : TColor;
  SaveTrainActiveColourForPrinting : TColor;
  SaveTrainInactiveColourForPrinting : TColor;
  SaveTRSPlungerColourForPrinting : TColor;
  SaveTRSPlungerOutlineColourForPrinting : TColor;
  SaveTRSPlungerPressedColourForPrinting : TColor;
  SaveStatusPanel0Str : String = '';
  SaveStatusPanel1Str : String = '';
  SaveStatusPanel2Str : String = '';
  SaveStatusPanel3Str : String = '';
  ScrollBarXAdjustment : Integer = 0;
  ScrollBarYAdjustment : Integer = 0;
  SignalDragging : Boolean = False;
  WatchdogActiveMsgFlag : Boolean = False;
  WatchdogErrorMsgFlag : Boolean = False;
  WindowsTaskbarDisabled : Boolean = False;
  ZoomScaleFactor : Integer = 1000;
Region : HRGN;
testregion : boolean = false;

IMPLEMENTATION

{$R *.dfm}

USES MiscUtils, Startup, Lenz, Input, Locks, Cuneo, Movement, GetTime, CreateRoute, Diagrams, RDCUnit, Types, Feedback, Route, LocoUtils, IniFiles, LocoDialogue,
     StrUtils, Help, Math {sic}, LocationData, FWPShowMessageUnit, Replay, TestUnit, WorkingTimetable, Options, Registry, Edit, Logging, Main, Splash;

CONST
  UnitRef = 'RailDraw';
  UndrawRequired = True;
  UndrawToBeAutomatic = True;

VAR
  BufferStopPopupNum : Integer;
  DiagramsCheckingInProgress : Boolean = False;
  LinePopupNumArray : IntegerArrayType;
  LocationLinesInitialised : Boolean = False;
  PointPopupNum : Integer;
  SaveCursor : TCursor = crDefault;
  SaveScreenMode : ScreenModeType = DefaultWindowedScreenMode;
  SaveZoomScaleFactor : Integer = 1000;
  SignalPopupNum : Integer;
  StatusBarX : Integer = 0;
  StatusBarY : Integer = 0;
  TimeRectangleDrawn : Cardinal = 0;
  TrackCircuitPopupLine : Integer;
  UpDownMarkersVisible : Boolean = True;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION GetSaveCursor : TCursor;
{ Return the SaveCursor variable state }
BEGIN
  Result := SaveCursor;
END; { SaveCursor }

PROCEDURE ClearLinePopupNumArray;
{ Empties the line popup num array }
BEGIN
  SetLength(LinePopupNumArray, 0);
END; { ClearLinePopupNumArray }

PROCEDURE SetLinePopupNumArray(NumArray : IntegerArrayType);
{ Assign to the LinePopupNumArray }
VAR
  I : Integer;

BEGIN
  ClearLinePopupNumArray;
  FOR I := 0 TO High(NumArray) DO
    AppendToIntegerArray(LinePopupNumArray, NumArray[I]);
END; { SetLinePopupNum }

PROCEDURE SetPointPopupNum(Num : Integer);
{ Assign to the GetPointPopupNum variable state }
BEGIN
  PointPopupNum := Num;
END; { SetPointPopupNum }

PROCEDURE SetSignalPopupNum(Num : Integer);
{ Assign to the GetSignalPopupNum variable state }
BEGIN
  SignalPopupNum := Num;
END; { SetSignalPopupNum }

PROCEDURE SetBufferStopPopupNum(Num : Integer);
{ Assign to the buffer stop popup number }
BEGIN
  BufferStopPopupNum := Num;
END; { SetBufferStopPopupNum }

PROCEDURE DrawRedLampAndVerticalLine(X, Y1, Y2 : Integer; Colour : TCOlour);
{ Draw a red lamp and vertical line where there is a buffer stop or line obstruction }
BEGIN
  WITH RailWindowBitmap.Canvas DO BEGIN
    { Draw the line }
    Pen.Color := Colour;
    MoveTo(X - ScrollBarXAdjustment, Y1 - ScrollBarYAdjustment);
    LineTo(X - ScrollBarXAdjustment, Y2 - ScrollBarYAdjustment);

    { and the red lamp }
    Pen.Color := BufferStopRed;
    Brush.Color := BufferStopRed;
    Ellipse(X - (BufferStopVerticalSpacingScaled DIV 5) - ScrollBarXAdjustment,
            ((Y2 -Y1) DIV 2) + Y1 - (BufferStopVerticalSpacingScaled DIV 5) - ScrollBarYAdjustment,
            X + (BufferStopVerticalSpacingScaled DIV 2) - ScrollBarXAdjustment,
            ((Y2 -Y1) DIV 2) + Y1 + (BufferStopVerticalSpacingScaled DIV 5) - ScrollBarYAdjustment);
  END; {WITH}
END; { DrawRedLampAndVerticalLine }

PROCEDURE DrawBufferStop(BufferStopNum : Integer; Colour : TColour);
{ Draw a buffer stop }
BEGIN
  InitialiseScreenDrawingVariables;
  WITH RailWindowBitmap.Canvas DO BEGIN
    WITH BufferStops[BufferStopNum] DO BEGIN
      { record the current colour }
      BufferStop_CurrentColour := Colour;
      IF InRecordLineDrawingMode THEN
        Log('X BS=' + IntToStr(BufferStopNum) + ' ' + ColourToStr(Colour));

      { Draw the line and red lamp }
      DrawRedLampAndVerticalLine(BufferStop_X, BufferStop_Y1, BufferStop_Y2, Colour);

      IF ShowMouseRectangles THEN
        DrawOutline(BufferStop_MouseRect, clFWPOrange, NOT UndrawRequired, NOT UndrawToBeAutomatic);
    END; {WITH}
  END; {WITH}
END; { DrawBufferStop }

PROCEDURE DrawBufferStopData(B : Integer; BufferStopText : String; Colour : TColor);
{ Put the bufferstop name or other supplied data on the diagram }
BEGIN
  InitialiseScreenDrawingVariables;
  WITH RailWindowBitmap.Canvas DO BEGIN
    Font.Style := [fsBold];
    Font.Color := Colour;
    Brush.Color := BackgroundColour;
    Font.Height := -MulDiv(FWPRailWindow.ClientHeight, LineFontHeight, ZoomScaleFactor);

    WITH BufferStops[B] DO BEGIN
      IF BufferStop_Direction = Down THEN
        TextOut(BufferStop_X - TextWidth(BufferStopText) - MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScaleFactor) - ScrollBarXAdjustment,
                ((BufferStop_Y2 - BufferStop_Y1) DIV 2) + BufferStop_Y1 - (TextHeight(BufferStopText) DIV 2) - ScrollBarYAdjustment,
                BufferStopText)
      ELSE
        TextOut(BufferStop_X + MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScaleFactor) - ScrollBarXAdjustment,
                ((BufferStop_Y2 - BufferStop_Y1) DIV 2) + BufferStop_Y1 - (TextHeight(BufferStopText) DIV 2) - ScrollBarYAdjustment,
                BufferStopText);
    END; {WITH}
  END; {WITH}
END; { DrawBufferStopData }

PROCEDURE SetCaption(Window : TForm; Caption : String);
{ Sets a window caption }
BEGIN
  IF Window <> NIL THEN BEGIN
    IF Window <> FWPRailWindow THEN
      Window.Caption := Caption
    ELSE BEGIN
      IF NOT InTestingMode THEN
        FWPRailWindow.Caption := 'FWP''s Railway Program ' + Caption
      ELSE
        FWPRailWindow.Caption := 'FWP''s Railway Program (Version ' + GetVersionInfoAsString + ' Build ' + GetBuildInfoAsString + ') ' + Caption;
    END;
  END;
END; { SetCaption }

PROCEDURE ShowLinePos;
{ Showing how line segments are sub-divided - for development }
VAR
  FeedbackUnitFound : Boolean;
  LineNameWidth : Word;
  ScreenDownX : Integer;
  ScreenDownY : Integer;
  ScreenUpX : Integer;
  ScreenUpY : Integer;
  SegmentText : String;
  TrackCircuitNumbered : Boolean;
  X, Y : Integer;

  PROCEDURE DrawSegmentText(SegmentText : String; UpX, UpY, DownX, DownY : Integer);
  { Draws the text in the right place on screen, using the correct font for certain symbols }
  VAR
    SaveLineFontName : String;

  BEGIN
    WITH RailWindowBitmap.Canvas DO BEGIN
      SaveLineFontName := Font.Name;
      { arrows need to be in the Symbol typeface }
      IF (SegmentText = '¨') OR (SegmentText = 'Æ') OR (SegmentText = '≠') OR (SegmentText = 'Ø') OR (SegmentText = '≠Ø') THEN
        Font.Name := 'Symbol';

      TextOut(UpX + (DownX - UpX) DIV 2 - TextWidth(SegmentText) DIV 2 - ScrollBarXAdjustment,
             (UpY + (DownY - UpY) DIV 2) - TextHeight(SegmentText) DIV 2 - ScrollBarYAdjustment,
              SegmentText);
      Font.Name := SaveLineFontName;
    END; {WITH}
  END; { DrawSegmentText }

  PROCEDURE ShowTrackCircuitData;
  { Display data relating to track circuits }
  VAR
    ColourNum : Integer;
    F : Integer;
    FirstUnit : Integer;
    FeedbackType : TypeOfFeedbackDetector;
    I, J : Integer;
    LastUnit : Integer;
    Line : Integer;
    LineDrawn : Boolean;
    P : Integer;
    Pos : Word;
    TC : Integer;
    TempNum : Integer;

  BEGIN
    TRY
      WITH RailWindowBitmap.Canvas DO BEGIN
        { First clear existing line detail, as it may obscure the data we're writing out }
        ShowLineOccupationDetail := False;
        FOR Line := 0 TO High(Lines) DO
          DrawLine(Line, Lines[Line].Line_CurrentColour, False);

        IF ShowOneFeedbackUnitOnly OR ShowTrackCircuitFeedbackDataInSeparateColours THEN BEGIN
            { show which Lenz feedback unit is being used }
          IF ShowOneFeedbackUnitOnly THEN BEGIN
            FirstUnit := ShowFeedbackUnitNum;
            LastUnit := ShowFeedbackUnitNum;
          END ELSE BEGIN
            FirstUnit := FirstFeedbackUnit;
            LastUnit := LastFeedbackUnit;
          END;

          { Go through all the colours in sequence, but start randomly - this is better than choosing colours at random, as one can end up with too many colours the same
            that way. Note: the From in RandomRange is included in the results, but the To is not.
          }
          ColourNum := RandomRange(1, MaxColourNum + 1);

          FOR F := FirstUnit TO LastUnit DO BEGIN
            WITH FeedbackUnitRecords[F] DO BEGIN
              IF NOT ShowOneFeedbackUnitOnly THEN BEGIN
                Inc(ColourNum);
                { Add another bit of randomness to the process, as otherwise the adjoining units might always come out the same colour. Note: the From in RandomRange is
                  included in the results, but the To is not.
                }
                TempNum := RandomRange(1, 3);
                IF TempNum = 1 THEN
                  Inc(ColourNum);
                IF ColourNum > MaxColourNum THEN
                  ColourNum := 1;
                Font.Color := ColoursArray[ColourNum];
              END;

              SegmentText := IntToStr(FirstUnit);
              FOR J := 1 TO 8 DO BEGIN
                IF Feedback_InputPoint[J] <> UnknownPoint THEN BEGIN
                  IF ShowOneFeedbackUnitOnly THEN BEGIN
                    Font.Color := clAqua;
                    WITH Points[Feedback_InputPoint[J]] DO BEGIN
                      SegmentText := IntToStr(F) + IntToStr(J);
                      WITH Point_MouseRect DO BEGIN
                        IF Point_FarY < Point_Y THEN
                          TextOut(Right - ScrollBarXAdjustment,
                                  Top + ((Bottom - Top - TextHeight(SegmentText)) DIV 2) - ScrollBarYAdjustment,
                                  SegmentText)
                        ELSE
                          TextOut(Left - TextWidth(SegmentText) - ScrollBarXAdjustment,
                                  Top + ((Bottom - Top - TextHeight(SegmentText)) DIV 2) - ScrollBarYAdjustment,
                                  SegmentText);
                      END; {WITH}
                    END; {WITH}
                  END;
                END ELSE
                  IF Feedback_InputTrackCircuit[J] <> UnknownTrackCircuit THEN BEGIN
                    IF ShowOneFeedbackUnitOnly THEN
                      Font.Color := clYellow;
                    IF (Feedback_InputTrackCircuit[J] >= 0) AND (Feedback_InputTrackCircuit[J] <= High(TrackCircuits)) THEN BEGIN
                      SegmentText := IntToStr(FirstUnit) + IntToStr(J);
                      IF Length(TrackCircuits[Feedback_InputTrackCircuit[J]].TC_LineArray) > 0 THEN BEGIN
                        Line := TrackCircuits[Feedback_InputTrackCircuit[J]].TC_LineArray[0];
                        WITH Lines[Line] DO BEGIN
                          ScreenUpX := MapGridXToScreenX(Line_GridUpX);
                          ScreenUpY := MapGridYToScreenY(Line_GridUpY);
                          ScreenDownX := MapGridXToScreenX(Line_GridDownX);
                          ScreenDownY := MapGridYToScreenY(Line_GridDownY);
                          TextOut(ScreenUpX + (ScreenDownX - ScreenUpX) DIV 2 - TextWidth(SegmentText) DIV 2 - ScrollBarXAdjustment,
                                  (ScreenUpY + (ScreenDownY - ScreenUpY) DIV 2) - TextHeight(SegmentText) DIV 2 - ScrollBarYAdjustment,
                                  SegmentText);
                        END; {WITH}
                        SegmentText := '';
                      END;
                    END;
                  END;
              END; {FOR}
            END; {WITH}
          END; {FOR}

          IF ShowOneFeedbackUnitOnly THEN
            ShowOneFeedbackUnitOnly := False;
        END ELSE BEGIN
          FOR TC := 0 TO High(TrackCircuits) DO BEGIN
            { Write out the track-circuit number once only }
            Line := 0;
            TrackCircuitNumbered := False;
            WHILE Line <= High(Lines) DO BEGIN
              IF Lines[Line].Line_TC = TC THEN BEGIN
                { Draw vertical lines as separators }
                IF (Lines[Line].Line_NextUpLine <> UnknownLine) AND (Lines[Lines[Line].Line_NextUpLine].Line_TC <> TC) THEN BEGIN
                  Pen.Color := clWhite;
                  Pen.Style := psSolid;
                  FOR Pos := 0 TO 10 DO BEGIN
                    WITH Lines[Line] DO BEGIN
                      ScreenUpX := MapGridXToScreenX(Line_GridUpX);
                      ScreenUpY := MapGridYToScreenY(Line_GridUpY);
                      ScreenDownX := MapGridXToScreenX(Line_GridDownX);
                      ScreenDownY := MapGridYToScreenY(Line_GridDownY);
                      X := ScreenUpX + MulDiv(ScreenDownX - ScreenUpX, Pos, 10);
                      Y := ScreenUpY + MulDiv(ScreenDownY - ScreenUpY, Pos, 10);
                      IF Pos = 0 THEN BEGIN
                        MoveTo(X - ScrollBarXAdjustment, Y - 6 - ScrollBarYAdjustment);
                        LineTo(X - ScrollBarXAdjustment, Y + 6 - ScrollBarYAdjustment);
                      END;
                    END; {WITH}
                  END; {FOR}
                END;

                { Colour TCs differently to distinguish them }
                LineDrawn := False;
                IF Lines[Line].Line_NextUpLine <> UnknownLine THEN BEGIN
                  IF Lines[Lines[Line].Line_NextUpLine].Line_TC <> TC THEN BEGIN
                    IF Lines[Lines[Line].Line_NextUpLine].Line_CurrentColour <> clLime THEN BEGIN
                      DrawLine(Line, clLime, NOT ActiveTrain);
                      LineDrawn := True;
                      Font.Color := clLime;
                    END ELSE
                      IF Lines[Lines[Line].Line_NextUpLine].Line_CurrentColour <> clRed THEN BEGIN
                        DrawLine(Line, clRed, NOT ActiveTrain);
                        LineDrawn := True;
                        Font.Color := clRed;
                      END ELSE BEGIN
                        DrawLine(Line, clYellow, NOT ActiveTrain);
                        LineDrawn := True;
                        Font.Color := clYellow;
                      END;
                  END;

                  IF Lines[Line].Line_NextDownLine <> UnknownLine THEN BEGIN
                    IF Lines[Lines[Line].Line_NextDownLine].Line_TC <> TC THEN BEGIN
                      IF Lines[Lines[Line].Line_NextDownLine].Line_CurrentColour <> clLime THEN BEGIN
                        DrawLine(Line, clLime, NOT ActiveTrain);
                        LineDrawn := True;
                        Font.Color := clLime;
                      END ELSE
                        IF Lines[Lines[Line].Line_NextDownLine].Line_CurrentColour <> clRed THEN BEGIN
                          DrawLine(Line, clRed, NOT ActiveTrain);
                          LineDrawn := True;
                          Font.Color := clRed;
                        END ELSE BEGIN
                          DrawLine(Line, clYellow, NOT ActiveTrain);
                          LineDrawn := True;
                          Font.Color := clYellow;
                        END;
                    END;
                  END;

                  IF NOT LineDrawn THEN BEGIN
                    DrawLine(Line, clYellow, NOT ActiveTrain);
                    Font.Color := clYellow;
                  END;
                END;

                IF NOT TrackCircuitNumbered OR (ScreenMode = FullScreenMode) OR (ScreenMode = FullScreenWithStatusBarMode) THEN BEGIN
                  WITH Lines[Line] DO BEGIN
                    Font.Style := [fsBold];
                    IF ShowTrackCircuits THEN
                      SegmentText := IntToStr(TC);

                    { note: the following options are mutually exclusive, so it doesn't need to be IF THEN ELSE }
                    IF ShowTrackCircuitLengths THEN BEGIN
                      IF Line_TC <> UnknownTrackCircuit THEN BEGIN
                        { how long (in inches) each track circuit is }
                        IF TrackCircuits[Line_TC].TC_LengthInInches = 0.0 THEN
                          { a missing length }
                          Font.Color := clYellow
                        ELSE
                          Font.Color := clLime;

                        SegmentText := FloatToStr(TrackCircuits[Line_TC].TC_LengthInInches);
                        { remove leading 0.s or trailing .0s }
                        IF Copy(SegmentText, 1, 2) = '0.' THEN
                          SegmentText := Copy(SegmentText, 2, 255)
                        ELSE
                          IF Copy(SegmentText, Length(SegmentText) - 1, 2) = '.0' THEN
                            SegmentText := Copy(SegmentText, 1, Length(SegmentText) - 2);
                      END;
                    END;

                    IF ShowTrackCircuitFeedbackDataInUse THEN BEGIN
                      { show which Lenz feedback unit is being used }
                      IF Line_TC <> UnknownTrackCircuit THEN BEGIN
                        SegmentText := '';
                        FeedbackUnitFound := False;
                        I := FirstFeedbackUnit;
                        WHILE (I <= LastFeedbackUnit) AND NOT FeedbackUnitFound DO BEGIN
                          FOR J := 1 TO 8 DO BEGIN
                            IF FeedbackUnitRecords[I].Feedback_InputTypeArray[J] = TrackCircuitFeedback THEN BEGIN
                              IF FeedbackUnitRecords[I].Feedback_InputTrackCircuit[J] = Line_TC THEN BEGIN
                                FeedbackUnitFound := True;
                                SegmentText := IntToStr(I) + IntToStr(J);
//                              IF FeedbackUnitInUseArray[I] THEN { &&&&&& }
//                                Font.Color := TCFeedbackDataInUseColour
//                              ELSE
//                                Font.Color := TCFeedbackDataOutOfUseColour;
                              END;
                            END;
                          END;
                          Inc(I);
                        END; {WHILE}
                        IF SegmentText = '' THEN BEGIN
                          Font.Color := clAqua; { TrackCircuitsWithoutFeedbackColour }
                          SegmentText := '0000';
                        END;
                      END;
                    END;

                    IF ScreenColoursSetForPrinting THEN
                      Font.Color := clBlack;

                    ScreenUpX := MapGridXToScreenX(Line_GridUpX);
                    ScreenUpY := MapGridYToScreenY(Line_GridUpY);
                    ScreenDownX := MapGridXToScreenX(Line_GridDownX);
                    ScreenDownY := MapGridYToScreenY(Line_GridDownY);
                    TextOut(ScreenUpX + (ScreenDownX - ScreenUpX) DIV 2 - TextWidth(SegmentText) DIV 2 - ScrollBarXAdjustment,
                           (ScreenUpY + (ScreenDownY - ScreenUpY) DIV 2) - TextHeight(SegmentText) DIV 2 - ScrollBarYAdjustment,
                            SegmentText);
                    SegmentText := '';
                  END; {WITH}
                  TrackCircuitNumbered := True;
                END;
              END;
              Inc(Line);
            END; {WHILE}
          END; {FOR}
        END;
        ShowLineOccupationDetail := True;
      END; {WITH}
    EXCEPT
      ON E : Exception DO
        Log('EG ShowTrackCircuitData:' + E.ClassName + ' error raised, with message: ' + E.Message);
    END; {TRY}
  END; { ShowTrackCircuitData }

  PROCEDURE ShowLineData;
  { show various line display options }
  VAR
    Line : Integer;
    Pos : Integer;

  BEGIN
    WITH RailWindowBitmap.Canvas DO BEGIN
      ShowLineOccupationDetail := False;

      { First clear existing line detail, as it may obscure the data we're writing out }
      FOR Line := 0 TO High(Lines) DO
        DrawLine(Line, Lines[Line].Line_CurrentColour, False);

      FOR Line := 0 TO High(Lines) DO BEGIN
        SegmentText := '';
        WITH Lines[Line] DO BEGIN
          ScreenUpX := MapGridXToScreenX(Line_GridUpX);
          ScreenUpY := MapGridYToScreenY(Line_GridUpY);
          ScreenDownX := MapGridXToScreenX(Line_GridDownX);
          ScreenDownY := MapGridYToScreenY(Line_GridDownY);

          { How which track circuits have routes set over them }
          IF ShowTrackCircuitsRoutedOver THEN BEGIN
            IF Line_TC <> UnknownTrackCircuit THEN BEGIN
              Font.Color := clAqua;
              IF TrackCircuits[Line_TC].TC_LockedForRoute <> UnknownRoute THEN
                SegmentText := IntToStr(TrackCircuits[Line_TC].TC_LockedForRoute);
            END;
          END;

          IF ShowLineDetail THEN BEGIN
            { show the internal name for each track segment }
            Font.Color := GetLineTypeColour(Line_TypeOfLine);
            LineNameWidth := TextWidth(LineToStr(Line));
            IF ScreenDownX > ScreenUpX THEN BEGIN
              IF (ScreenDownX - ScreenUpX) > LineNameWidth THEN
                SegmentText := LineToStr(Line)
              ELSE
                SegmentText := Copy(LineToStr(Line), Length(LineToStr(Line)), 1);
            END ELSE BEGIN
              IF (ScreenUpX - ScreenDownX) > LineNameWidth THEN
                SegmentText := LineToStr(Line)
              ELSE
                SegmentText := Copy(LineToStr(Line), Length(LineToStr(Line)), 1);
            END;
          END;

          IF ShowLineNumbers THEN BEGIN
            { show the internal numbers for each track segment }
            Font.Color := GetLineTypeColour(Line_TypeOfLine);
            LineNameWidth := TextWidth(IntToStr(Line));
            IF ScreenDownX > ScreenUpX THEN BEGIN
              IF (ScreenDownX - ScreenUpX) > LineNameWidth THEN
                SegmentText := IntToStr(Line)
              ELSE
                SegmentText := Copy(IntToStr(Line), Length(IntToStr(Line)), 1);
            END ELSE BEGIN
              IF (ScreenUpX - ScreenDownX) > LineNameWidth THEN
                SegmentText := LineToStr(Line)
              ELSE
                SegmentText := Copy(IntToStr(Line), Length(IntToStr(Line)), 1);
            END;
          END;

          IF ShowLinesUpXAbsoluteValue THEN BEGIN
            Font.Color := GetLineTypeColour(Line_TypeOfLine);
            LineNameWidth := TextWidth(LineToStr(Line));
            SegmentText := IntToStr(Lines[Line].Line_GridUpX);
          END;

          IF ShowLineDirectionDetail THEN BEGIN
            { Indicate lines that are not designated as through lines }
            IF Lines[Line].Line_Location <> UnknownLocation THEN BEGIN
              CASE Locations[Lines[Line].Line_Location].Location_ThroughLocationState OF
                NonThroughLocation:
                  BEGIN
                    Font.Color := clRed;
                    SegmentText := 'X';
                  END;
                ThroughLocation:
                  BEGIN
                    { if the line direction is one way, which way it is }
                    Font.Color := clAqua;
                    IF Lines[Line].Line_Direction = Up THEN BEGIN
                      IF ScreenUpY = ScreenDownY THEN
                        SegmentText := Char(172) { left arrow }
                      ELSE
                        IF ScreenUpY > ScreenDownY THEN
                          SegmentText := Char(175) { down arrow }
                        ELSE
                          SegmentText := Char(173); { up arrow }
                    END ELSE
                      IF Lines[Line].Line_Direction = Down THEN BEGIN
                        IF ScreenUpY = ScreenDownY THEN
                          SegmentText := Char(174) { right arrow }
                        ELSE
                          IF ScreenUpY > ScreenDownY THEN
                            SegmentText := Char(173) { up arrow }
                          ELSE
                            SegmentText := Char(175); { down arrow }
                      END ELSE BEGIN
                        Font.Color := clLime;
                        SegmentText := '=';
                      END;
                  END;
                UnknownThroughLocationState:
                  BEGIN
                    Font.Color := clYellow;
                    SegmentText := '?';
                  END;
              END; {CASE}
            END;
          END;

          IF ShowLineGradients THEN BEGIN
            { Indicate lines which are marked as being on a gradient }
            Font.Color := clLime;
            IF Lines[Line].Line_Gradient = RisingIfDown THEN BEGIN
              IF Line_Direction = Down THEN
                SegmentText := '≠' { up arrow }
              ELSE
                IF Line_Direction = Up THEN
                  SegmentText := 'Ø' { down arrow }
                ELSE
                  IF Line_Direction = Bidirectional THEN
                    SegmentText := '≠Ø' { Up and down arrows }
            END ELSE BEGIN
              IF Lines[Line].Line_Gradient = RisingIfUp THEN BEGIN
                IF Line_Direction = Up THEN
                  SegmentText := '≠' { up arrow }
                ELSE
                  IF Line_Direction = Down THEN
                    SegmentText := 'Ø' { down arrow }
                  ELSE
                    IF Line_Direction = Bidirectional THEN
                      SegmentText := '≠Ø' { Up and down arrows }
              END;
            END;
          END;

          IF ShowAreas THEN BEGIN
            IF Lines[Line].Line_Location <> UnknownLocation THEN BEGIN
              IF Locations[Line_Location].Location_Area <> UnknownArea THEN BEGIN
                IF (Areas[Locations[Line_Location].Location_Area].Area_IsHoldingArea) AND (Areas[Locations[Line_Location].Location_Area].Area_IsReversingArea) THEN
                  Font.Color := clAqua
                ELSE
                  IF Areas[Locations[Line_Location].Location_Area].Area_IsHoldingArea THEN
                    Font.Color := clYellow
                  ELSE
                    IF Areas[Locations[Line_Location].Location_Area].Area_IsReversingArea THEN
                      Font.Color := clRed
                    ELSE
                      Font.Color := clLime;

                SegmentText := AreaToStr(Locations[Line_Location].Location_Area, ShortStringType);
              END;
            END ELSE
              SegmentText := '?';
          END;

          IF SegmentText <> '' THEN BEGIN
            IF ScreenColoursSetForPrinting THEN
              Font.Color := clBlack;

            IF Zooming THEN
              Font.Size := 14; { should depend on the zoom factor? ************ }

            DrawSegmentText(SegmentText, ScreenUpX, ScreenUpY, ScreenDownX, ScreenDownY);
          END;

          { Draw vertical lines to show the line segments }
          FOR Pos := 0 TO 10 DO BEGIN
            X := ScreenUpX + MulDiv(ScreenDownX - ScreenUpX, Pos, 10);
            Y := ScreenUpY + MulDiv(ScreenDownY - ScreenUpY, Pos, 10);
            IF Pos = 0 THEN BEGIN
              MoveTo(X - ScrollBarXAdjustment, Y - 6 - ScrollBarYAdjustment);
              LineTo(X - ScrollBarXAdjustment, Y + 6 - ScrollBarYAdjustment);
            END;
          END; {FOR}
        END; {WITH}
      END; {FOR}
      ShowLineOccupationDetail := True;
    END; {WITH}
  END; { ShowLineData }

  PROCEDURE ShowLocationData;
  { show various location display options }
  VAR
    ElementPos : Integer;
    Line : Integer;
    LineFound : Boolean;
    Location : Integer;
    TempLocationArray : IntegerArrayType;

  BEGIN
    TRY
      { First clear existing line detail, as it may obscure the data we're writing out }
      ShowLineOccupationDetail := False;
      FOR Line := 0 TO High(Lines) DO
        DrawLine(Line, Lines[Line].Line_CurrentColour, False);

      SetLength(TempLocationArray, 0);

      WITH RailWindowBitmap.Canvas DO BEGIN
        FOR Location := 0 TO High(Locations) DO BEGIN
          IF Location = UnknownLocation THEN
            SegmentText := '?'
          ELSE BEGIN
            SegmentText := '';
            WITH Locations[Location] DO BEGIN
              IF ShowLocations THEN BEGIN
                Font.Color := clLime;
                SegmentText := LocationToStr(Location, ShortStringType);
              END ELSE
                IF ShowLocationLengthDetail THEN BEGIN
                  IF NOT IsElementInLocationArray(TempLocationArray, Location, ElementPos) THEN BEGIN
                    { only display the length once for each location }
                    AppendToLocationArray(TempLocationArray, Location);
                    Font.Color := clAqua;
                    SegmentText := LocationToStr(Location, ShortStringType) + ' ' + FloatToStr(Location_LengthInInches);
                  END;
                END;
            END; {WITH}

            { Find a line so we can get an X and Y position for the location }
            LineFound := False;
            Line := 0;
            WHILE (Line <= High(Lines)) AND NOT LineFound DO BEGIN
              IF Lines[Line].Line_Location = Location THEN
                LineFound := True
              ELSE
                Inc(Line);
            END; {WHILE}

            IF LineFound THEN
              WITH Lines[Line] DO BEGIN
                ScreenUpX := MapGridXToScreenX(Line_GridUpX);
                ScreenUpY := MapGridYToScreenY(Line_GridUpY);
                ScreenDownX := MapGridXToScreenX(Line_GridDownX);
                ScreenDownY := MapGridYToScreenY(Line_GridDownY);
                DrawSegmentText(SegmentText, ScreenUpX, ScreenUpY, ScreenDownX, ScreenDownY);
              END; {WITH}
          END;
        END; {FOR}
        ShowLineOccupationDetail := True;
      END; {WITH}
    EXCEPT
      ON E : Exception DO
        Log('EG ShowLocationData:' + E.ClassName + ' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { ShowLocationData }

VAR
  Line : Integer;

BEGIN
  TRY
    InitialiseScreenDrawingVariables;

    WITH RailWindowBitmap.Canvas DO BEGIN
      Font.Color := clYellow;
      Font.Style := [fsBold];
      Font.Height := -MulDiv(FWPRailWindow.ClientHeight, LineFontHeight, ZoomScalefactor);

      IF ShowTrackCircuits
      OR ShowTrackCircuitLengths
      OR ShowTrackCircuitFeedbackDataInUse
      OR ShowTrackCircuitFeedbackDataInSeparateColours
      OR ShowOneFeedbackUnitOnly
      THEN
        ShowTrackCircuitData;

      IF ShowLinesWithoutTrackCircuits THEN BEGIN
        Line := 0;
        WHILE Line <= High(Lines) DO BEGIN
          IF Lines[Line].Line_TC = UnknownTrackCircuit THEN BEGIN
            Font.Color := LinesWithoutTrackCircuitsColour;
            SegmentText := 'X';
            WITH Lines[Line] DO BEGIN
              ScreenUpX := MapGridXToScreenX(Line_GridUpX);
              ScreenUpY := MapGridYToScreenY(Line_GridUpY);
              ScreenDownX := MapGridXToScreenX(Line_GridDownX);
              ScreenDownY := MapGridYToScreenY(Line_GridDownY);
              DrawSegmentText(SegmentText, ScreenUpX, ScreenUpY, ScreenDownX, ScreenDownY);
            END; {WITH}
           END;
          Inc(Line);
        END; {WHILE}
      END;

      { The following options cycle through the various lines }
      IF ShowAreas
      OR ShowLineDetail
      OR ShowLineNumbers
      OR ShowLinesUpXAbsoluteValue
      OR ShowLinesWhichLockPoints
      OR ShowLineDirectionDetail
      OR ShowLineGradients
      OR ShowTrackCircuitsRoutedOver
      THEN
        ShowLineData
      ELSE
        IF ShowLocationLengthDetail
        OR ShowLocations
        THEN
          ShowLocationData;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG ShowLinePos:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ShowLinePos }

PROCEDURE DrawOutline(FWPPolygon : ARRAY OF TPoint; Colour : TColour; UndrawRequired, UndrawToBeAutomatic : Boolean); Overload;
{ We need this as the default Delphi Rectangle is filled in }
BEGIN
  TRY
    InitialiseScreenDrawingVariables;

    WITH RailWindowBitmap.Canvas DO BEGIN
      IF UndrawRequired THEN
        Pen.Mode := pmNotXor;
      Pen.Color := Colour;
      Brush.Color := BackgroundColour;

      FWPPolygon[0].X := FWPPolygon[0].X - ScrollBarXAdjustment;
      FWPPolygon[0].Y := FWPPolygon[0].Y - ScrollBarYAdjustment;
      FWPPolygon[1].X := FWPPolygon[1].X - ScrollBarXAdjustment;
      FWPPolygon[1].Y := FWPPolygon[1].Y - ScrollBarYAdjustment;
      FWPPolygon[2].X := FWPPolygon[2].X - ScrollBarXAdjustment;
      FWPPolygon[2].Y := FWPPolygon[2].Y - ScrollBarYAdjustment;
      FWPPolygon[3].X := FWPPolygon[3].X - ScrollBarXAdjustment;
      FWPPolygon[3].Y := FWPPolygon[3].Y - ScrollBarYAdjustment;
      FWPPolygon[4] := FWPPolygon[0];
      Polyline(FWPPolygon);
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawOutline:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawOutline }

PROCEDURE DrawOutline(NewRect : TRect; Colour : TColour; UndrawRequired, UndrawToBeAutomatic : Boolean); Overload;
{ We need this as the default Delphi Rectangle is filled in }
VAR
  FWPPolygon : ARRAY [0..4] OF TPoint;

BEGIN
  FWPPolygon[0] := Point(NewRect.Left, NewRect.Top);
  FWPPolygon[1] := Point(NewRect.Right, NewRect.Top);
  FWPPolygon[2] := Point(NewRect.Right, NewRect.Bottom);
  FWPPolygon[3] := Point(NewRect.Left, NewRect.Bottom);
  FWPPolygon[4] := FWPPolygon[0];
  DrawOutline(FWPPolygon, Colour, UndrawRequired, UndrawToBeAutomatic);
END; { DrawOutline }

PROCEDURE WriteToStatusBarPanel(PanelNum : Integer; Str : String);
{ Write the text in the chosen panel if the text has changed. Note: Debug can only be used in this routine with a second Boolean parameter turning DoNoWriteToStatusPanel
  on, as otherwise both routines call each other and the stack overflows.
}
CONST
  DoNotWriteToStatusBarPanel = True;

BEGIN
  TRY
    IF FWPRailWindow <> NIL THEN BEGIN
      WITH FWPRailWindow.FWPRailWindowStatusBar DO BEGIN
        CASE PanelNum OF
          0:
            IF SaveStatusPanel0Str <> Str THEN BEGIN
              Panels[StatusBarPanel0].Text := Str;
              SaveStatusPanel0Str := Str;
            END;
          1:
            IF SaveStatusPanel1Str <> Str THEN BEGIN
              Panels[StatusBarPanel1].Text := Str;
              SaveStatusPanel1Str := Str;
            END;
          2:
            IF SaveStatusPanel2Str <> Str THEN BEGIN
              Panels[StatusBarPanel2].Text := Str;
              SaveStatusPanel2Str := Str;
            END;
          3:
            IF SaveStatusPanel3Str <> Str THEN BEGIN
              Panels[StatusBarPanel3].Text := Str;
              SaveStatusPanel3Str := Str;
            END;
        END; {CASE}
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG WriteToStatusBarPanel:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WriteToStatusBarPanel }

PROCEDURE DrawFailure(Device : Integer; ActionCh : String);
{ Describes the offending items in the status bar }
BEGIN
  TRY
    IF NOT InAutoMode THEN BEGIN
      MakeSound(1);
      WriteToStatusBarPanel(StatusBarPanel2, '*** Failure: ' + IntToStr(Device) + ActionCh);
      CASE ActionCh[1] OF
        '/', '-':
          { a point }
          WITH Points[Device] DO
            { note: black really is the new white here - as clBlack comes out as pmNotXored clWhite }
            DrawOutline(Point_MouseRect, clBlack, UndrawRequired, UndrawToBeAutomatic);
      END; {CASE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawFailure:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawFailure }

PROCEDURE DrawSpeedRestrictions;
{ Draw speed restrictions next to lines - but only draw one sign per track circuit }
CONST
  LeftArrowCh = '¨';
  RightArrowCh = 'Æ';
  NorthArrowCh = '≠';
  SouthArrowCh = 'Ø';

VAR
  I : Integer;
  Indicator : JunctionIndicatorType;
  Line : Integer;
  HorizontalArrowAdjustment : Integer;
  LeftArrowNeeded : Boolean;
  NorthArrowNeededOnLeft : Boolean; { we need to use North/South to avoid confusion with the railway's normal Up/Down directions }
  NorthArrowNeededOnRight : Boolean;
  RightArrowNeeded : Boolean;
  SaveLineFontName : String;
  ScreenDownX : Integer;
  ScreenUpX : Integer;
  ScreenUpY : Integer;
  SouthArrowNeededOnLeft : Boolean;
  SouthArrowNeededOnRight : Boolean;
  SpeedStr : String;
  TCArray : IntegerArrayType;

BEGIN
  TRY
    IF FWPRailWindow <> NIL THEN BEGIN
      InitialiseScreenDrawingVariables;
      SetLength(TCArray, 0);

      WITH RailWindowBitmap.Canvas DO BEGIN
        SaveLineFontName := Font.Name;
        Font.Name := 'Symbol';
        Font.Height := -MulDiv(FWPRailWindow.ClientHeight, LineFontHeight, ZoomScalefactor);

        FOR Line := 0 TO High(Lines) DO BEGIN
          WITH Lines[Line] DO BEGIN
            IF (Line_TC <> UnknownTrackCircuit) AND NOT IsElementInIntegerArray(TCArray, Line_TC) THEN BEGIN
              { store the TC so that we don't draw the restriction on each part of the TC }
              AppendToIntegerArray(TCArray, Line_TC);
            
              LeftArrowNeeded := False;
              RightArrowNeeded := False;
              NorthArrowNeededOnLeft := False;
              NorthArrowNeededOnRight := False;
              SouthArrowNeededOnLeft := False;
              SouthArrowNeededOnRight := False;
              SpeedStr := '';

              WITH TrackCircuits[Line_TC] DO BEGIN
                { Deal with possible speed restrictions }
                IF TC_SpeedRestrictionInMPH <> NoSpecifiedSpeed THEN BEGIN
                  Font.Color := TCSpeedRestrictionColour;
                  SpeedStr := MPHtoStr(TrackCircuits[Line_TC].TC_SpeedRestrictionInMPH);
                  IF TrackCircuits[Line_TC].TC_SpeedRestrictionDirection = Up THEN
                    LeftArrowNeeded := True
                  ELSE
                    IF TrackCircuits[Line_TC].TC_SpeedRestrictionDirection = Down THEN
                      RightArrowNeeded := True
                    ELSE
                      IF TrackCircuits[Line_TC].TC_SpeedRestrictionDirection = Bidirectional THEN BEGIN
                        LeftArrowNeeded := True;
                        RightArrowNeeded := True;
                      END;
                END; {WITH}

                { Also see if there are any signal indicator speed restrictions }
                I := 0;
                WHILE I <= High(TrackCircuits[Line_TC].TC_AdjacentSignals) DO BEGIN
                  IF Signals[TrackCircuits[Line_TC].TC_AdjacentSignals[I]].Signal_IndicatorSpeedRestriction <> NoSpecifiedSpeed THEN BEGIN
                    Font.Color := TCSpeedRestrictionColour;
                    SpeedStr := MPHToStr(Signals[TrackCircuits[Line_TC].TC_AdjacentSignals[I]].Signal_IndicatorSpeedRestriction);
                    IF SignalHasLeftJunctionIndicator(TrackCircuits[Line_TC].TC_AdjacentSignals[I], Indicator) THEN BEGIN
                      IF Signals[TrackCircuits[Line_TC].TC_AdjacentSignals[I]].Signal_Direction = Up THEN
                        SouthArrowNeededOnRight := True
                      ELSE
                        NorthArrowNeededOnLeft := True;
                    END ELSE
                      IF SignalHasRightJunctionIndicator(TrackCircuits[Line_TC].TC_AdjacentSignals[I], Indicator) THEN BEGIN
                        IF Signals[TrackCircuits[Line_TC].TC_AdjacentSignals[I]].Signal_Direction = Up THEN
                          NorthArrowNeededOnRight := True
                        ELSE
                          SouthArrowNeededOnLeft := True;
                      END;
                  END;
                  Inc(I);
                END; {WHILE}

                IF SpeedStr <> '' THEN BEGIN
                  { Write out the text - we need special code for adding arrows, as they use the Symbol typeface. HorizontalArrowAdjustment is needed to position the arrow
                    in line with the text.
                  }
                  HorizontalArrowAdjustment := MulDiv(FWPRailWindow.ClientWidth, 1, ZoomScalefactor);
                  ScreenUpX := MapGridXToScreenX(Line_GridUpX);
                  ScreenUpY := MapGridYToScreenY(Line_GridUpY);
                  ScreenDownX := MapGridXToScreenX(Line_GridDownX);

                  IF LeftArrowNeeded THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextOut(ScreenUpX + SpeedRestrictionHorizontalSpacingScaled - ScrollBarXAdjustment,
                            ScreenUpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                            LeftArrowCh);
                    TextRect(Rect(ScreenUpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(LeftArrowCh) - ScrollBarXAdjustment,
                                  ScreenUpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  ScreenUpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(LeftArrowCh) + TextWidth(SpeedStr) - ScrollBarXAdjustment,
                                  ScreenUpY - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             ScreenUpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(LeftArrowCh) - ScrollBarXAdjustment,
                             ScreenUpY - TextHeight(SpeedStr) - (SpeedRestrictionVerticalSpacingScaled) + HorizontalArrowAdjustment - ScrollBarYAdjustment,
                             SpeedStr);
                  END;
                  IF NorthArrowNeededOnRight THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextOut(ScreenUpX + SpeedRestrictionHorizontalSpacingScaled - ScrollBarXAdjustment,
                            ScreenUpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                            NorthArrowCh);
                    TextRect(Rect(ScreenUpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(NorthArrowCh) - ScrollBarXAdjustment,
                                  ScreenUpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  ScreenUpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(NorthArrowCh) + TextWidth(SpeedStr) - ScrollBarXAdjustment,
                                  ScreenUpY - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             ScreenUpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(NorthArrowCh) - ScrollBarXAdjustment,
                             ScreenUpY - TextHeight(SpeedStr) - (SpeedRestrictionVerticalSpacingScaled) + HorizontalArrowAdjustment - ScrollBarYAdjustment,
                             SpeedStr);
                  END;
                  IF SouthArrowNeededOnRight THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextOut(ScreenUpX + SpeedRestrictionHorizontalSpacingScaled - ScrollBarXAdjustment,
                            ScreenUpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                            SouthArrowCh);
                    TextRect(Rect(ScreenUpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                                  ScreenUpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  ScreenUpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(SouthArrowCh) + TextWidth(SpeedStr) - ScrollBarXAdjustment,
                                  ScreenUpY - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             ScreenUpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                             ScreenUpY - TextHeight(SpeedStr) - (SpeedRestrictionVerticalSpacingScaled) + HorizontalArrowAdjustment - ScrollBarYAdjustment,
                             SpeedStr);
                  END;
                  IF RightArrowNeeded THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextRect(Rect(ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(SpeedStr) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                                  ScreenUpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                                  ScreenUpY + TextHeight(SpeedStr) + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(SpeedStr) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                             ScreenUpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                             SpeedStr);
                    TextOut(ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                            ScreenUpY + SpeedRestrictionVerticalSpacingScaled - HorizontalArrowAdjustment - ScrollBarYAdjustment,
                            RightArrowCh);
                  END;

                  IF SouthArrowNeededOnLeft THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextRect(Rect(ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(SpeedStr) - TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                                  ScreenUpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                                  ScreenUpY + TextHeight(SpeedStr) + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(SpeedStr) - TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                             ScreenUpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                             SpeedStr);
                    TextOut(ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                            ScreenUpY + SpeedRestrictionVerticalSpacingScaled - HorizontalArrowAdjustment - ScrollBarYAdjustment,
                            SouthArrowCh);
                  END;

                  IF NorthArrowNeededOnLeft THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextRect(Rect(ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(SpeedStr) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                                  ScreenUpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                                  ScreenUpY + TextHeight(SpeedStr) + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(SpeedStr) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                             ScreenUpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                             SpeedStr);
                    TextOut(ScreenUpX + (ScreenDownX - ScreenUpX) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                            ScreenUpY + SpeedRestrictionVerticalSpacingScaled - HorizontalArrowAdjustment - ScrollBarYAdjustment,
                            NorthArrowCh);
                  END;
                END;
              END; {WITH}
            END;
          END; {WITH}
        END; {FOR}
        Font.Name := SaveLineFontName;
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawSpeedRestrictions:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawSpeedRestrictions }

PROCEDURE DrawSignalData(S : Integer; Str : String; Colour : Integer);
{ Put the signal name or other supplied data on the diagram }
BEGIN
  TRY
    InitialiseScreenDrawingVariables;
    WITH RailWindowBitmap.Canvas DO BEGIN
      Font.Style := [fsBold];
      Font.Color := Colour;
      Brush.Color := BackgroundColour;
      Font.Height := -MulDiv(FWPRailWindow.ClientHeight, LineFontHeight, ZoomScalefactor);

      WITH Signals[S] DO BEGIN
        IF Signal_Direction = Up THEN
          TextOut(Signal_PostMouseRect.Right + MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor) - ScrollBarXAdjustment,
                  Signal_PostMouseRect.Top + ((Signal_PostMouseRect.Bottom - Signal_PostMouseRect.Top - TextHeight(Str)) DIV 2) - ScrollBarYAdjustment,
                  Str)
        ELSE
          IF Signal_Direction = Down THEN
            TextOut(Signal_PostMouseRect.Left - TextWidth(Str) - MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor) - ScrollBarXAdjustment,
                    Signal_PostMouseRect.Top + ((Signal_PostMouseRect.Bottom - Signal_PostMouseRect.Top - TextHeight(Str)) DIV 2) - ScrollBarYAdjustment,
                    Str);
      END; {WITH}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawSignalData:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawSignalData }

PROCEDURE DrawAllSignals(ShowSignalAndBufferStopNums, ShowTheatreDestinations : Boolean);
{ Draw all the signals }
VAR
  S : Integer;

BEGIN
  TRY
    { Draw the signals }
    FOR S := 0 TO High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        { initialise them to red - to force SetSignal to switch them on and draw them }
        IF ProgramStarting THEN
          SetSignal(UnknownLocoChipStr, S, RedAspect, LogSignalData, NOT ForceAWrite);

        DrawSignal(S);
        IF ShowSignalAndBufferStopNums THEN
          DrawSignalData(S, IntToStr(S), SignalNumberColour)
        ELSE
          IF ShowTheatreDestinations THEN
            DrawSignalData(S, Signals[S].Signal_AsTheatreDestination, SignalNumberColour);

       { Draw a rectangle around any signal highlighted by the input procedure }
       IF SignalHighlighted <> UnknownSignal THEN
        WITH Signals[SignalHighlighted] DO
          DrawOutline(Signal_MouseRect, clWhite, NOT UndrawRequired, NOT UndrawToBeAutomatic);
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawAllSignals:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawAllSignals }

PROCEDURE DrawSignalPost(S : Integer);
{ Draws the signal post using Signal_PostColour }
BEGIN
  TRY
    IF S <> UnknownSignal THEN BEGIN
      InitialiseScreenDrawingVariables;

      WITH RailWindowBitmap.Canvas DO BEGIN
        WITH Signals[S] DO BEGIN
          IF Signal_Direction = Up THEN BEGIN
            { only erase a path for the signal post if part of a signal is not also going to be erased }
            Pen.Color := BackgroundColour;
            Brush.Color := BackgroundColour;
            Rectangle(Signal_LineX + SignalRadiusScaled - ScrollBarXAdjustment,
                      Signal_LineWithVerticalSpacingY - SignalVerticalSpacingScaled + RailWindowBitmapCanvasPenWidth - ScrollBarYAdjustment,
                      Signal_LineX + SignalRadiusScaled + MulDiv(FWPRailWindow.ClientWidth, 10, ZoomScalefactor) - ScrollBarXAdjustment,
                      Signal_LineWithVerticalSpacingY + SignalRadiusScaled - ScrollBarYAdjustment);

            IF ShowSignalsFromWhichUserMustDrive AND Signal_FromWhichUserMustDrive THEN
              Pen.Color := SignalsFromWhichUserMustDriveSignalPostColour
            ELSE
              Pen.Color := Signals[S].Signal_PostColour;

            IF Signal_FromWhichUserMustDrive THEN
              Pen.Style := SignalsFromWhichUserMustDriveSignalPostPenStyle;

            { Pen.Width is the width of the line outlining the signal }
            IF (Signal_Type = SemaphoreHome) OR (Signal_Type = SemaphoreDistant) THEN
              MoveTo(Signal_LineX + SignalRadiusScaled - SignalSemaphoreHeightScaled - Pen.Width - ScrollBarXAdjustment,
                     Signal_LineWithVerticalSpacingY - ScrollBarYAdjustment)
            ELSE
              MoveTo(Signal_LineX + SignalRadiusScaled - Pen.Width - ScrollBarXAdjustment,
                     Signal_LineWithVerticalSpacingY - ScrollBarYAdjustment);
            LineTo(Signal_LineX + SignalRadiusScaled - Pen.Width + MulDiv(FWPRailWindow.ClientWidth, 8, ZoomScalefactor) - ScrollBarXAdjustment,
                   Signal_LineWithVerticalSpacingY - ScrollBarYAdjustment);
            LineTo(Signal_LineX + SignalRadiusScaled - Pen.Width + MulDiv(FWPRailWindow.ClientWidth, 8, ZoomScalefactor) - ScrollBarXAdjustment,
                   Signal_LineWithVerticalSpacingY - SignalVerticalSpacingScaled + (RailWindowBitmapCanvasPenWidth DIV 2) - ScrollBarYAdjustment);
          END ELSE
            IF Signal_Direction = Down THEN BEGIN
              { only erase a path for the signal post if part of a signal is not also going to be erased }
              Pen.Color := BackgroundColour;
              Brush.Color := BackgroundColour;
              Rectangle(Signal_LineX - SignalRadiusScaled - MulDiv(FWPRailWindow.ClientWidth, 10, ZoomScalefactor) - ScrollBarXAdjustment,
                        Signal_LineWithVerticalSpacingY - SignalRadiusScaled - ScrollBarYAdjustment,
                        Signal_LineX - SignalRadiusScaled - ScrollBarXAdjustment,
                        Signal_LineWithVerticalSpacingY + SignalVerticalSpacingScaled - RailWindowBitmapCanvasPenWidth - ScrollBarYAdjustment);

              IF ShowSignalsFromWhichUserMustDrive AND Signal_FromWhichUserMustDrive THEN
                Pen.Color := SignalsFromWhichUserMustDriveSignalPostColour
              ELSE
                Pen.Color := Signals[S].Signal_PostColour;

              IF Signal_FromWhichUserMustDrive THEN
                Pen.Style := SignalsFromWhichUserMustDriveSignalPostPenStyle;

              { Pen.Width is the width of the line outlining the signal }
              IF (Signal_Type = SemaphoreHome) OR (Signal_Type = SemaphoreDistant) THEN
                MoveTo(Signal_LineX - SignalRadiusScaled + SignalSemaphoreHeightScaled + Pen.Width - ScrollBarXAdjustment,
                       Signal_LineWithVerticalSpacingY - ScrollBarYAdjustment)
              ELSE
                MoveTo(Signal_LineX - SignalRadiusScaled + Pen.Width - ScrollBarXAdjustment,
                       Signal_LineWithVerticalSpacingY - ScrollBarYAdjustment);
              LineTo(Signal_LineX - SignalRadiusScaled + Pen.Width - MulDiv(FWPRailWindow.ClientWidth, 8, ZoomScalefactor) - ScrollBarXAdjustment,
                     Signal_LineWithVerticalSpacingY - ScrollBarYAdjustment);
              LineTo(Signal_LineX - SignalRadiusScaled + Pen.Width - MulDiv(FWPRailWindow.ClientWidth, 8, ZoomScalefactor) - ScrollBarXAdjustment,
                     Signal_LineWithVerticalSpacingY + SignalVerticalSpacingScaled - (RailWindowBitmapCanvasPenWidth DIV 2) - ScrollBarYAdjustment);
            END;
        END; {WITH}

        { and reset any screen drawing variables we've changed }
        InitialiseScreenDrawingVariables;
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawSignalPost:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawSignalPost }

PROCEDURE DrawSignal(S : Integer);
{ Draw a signal at the current position. We need to know if it is for up or down traffic, a home or distant or calling on, and what aspect it is. Signal_Line.X is the
  position of the main aspect
}
TYPE
  PivotType = (LeftPivot, RightPivot);

VAR
  Aspect : AspectType;
  EllipseX1, EllipseY1, EllipseX2, EllipseY2 : Integer;
  Indicator : JunctionIndicatorType;
  LowerX : Integer;
  MiddleX : Integer;
  RevisedIndicatorX : Integer;
  SavePenWidth : Integer;
  SColour1, SColour2  : Integer;
  SignalBottom : Integer;
  SignalLeft : Integer;
  SignalRight : Integer;
  SignalTop : Integer;
  TheatreIndicatorX1, TheatreIndicatorX2 : Integer;
  TheatreIndicatorY1, TheatreIndicatorY2 : Integer;
  TopAspectX, IndicatorX : Integer;
  UpperX : Integer;
//Region : HRGN;

  PROCEDURE DrawSemaphore(X, Y, AngleInDegrees : Integer; Pivot : PivotType);
  CONST
    LeftPivotSquareArray : ARRAY[0..7] OF Extended = (0, 0, -1, 0, -1, 1, 0, 1);
    RightPivotSquareArray : ARRAY[0..7] OF Extended = (0, 0, 1, 0, 1, 1, 0, 1);

  VAR
    I : Integer;
    Radians : Extended;
    TPointArray : ARRAY[0..3] OF TPoint;
    TransformArray : ARRAY[0..3] OF Extended;
    XOffset : Integer;

  BEGIN
    Radians := AngleInDegrees * Pi / 180;
    TransformArray[0] := Cos(Radians);
    TransformArray[1] := -Sin(Radians);
    TransformArray[2] := Sin(Radians);
    TransformArray[3] := Cos(Radians);


    FOR I := 0 TO 3 DO BEGIN
      IF Pivot = RightPivot THEN BEGIN
        XOffset := 0;

        TPointArray[I].X := Round((TransformArray[0] * RightPivotSquareArray[I  * 2]      * SignalSemaphoreHeightScaled * 2)
                                + (TransformArray[1] * RightPivotSquareArray[(I * 2) + 1] * SignalSemaphoreWidthScaled * 2) + X - ScrollBarXAdjustment - XOffset);
        TPointArray[I].Y := Round((TransformArray[2] * RightPivotSquareArray[I  * 2]      * SignalSemaphoreHeightScaled * 2)
                                + (TransformArray[3] * RightPivotSquareArray[(I * 2) + 1] * SignalSemaphoreWidthScaled * 2) + Y - ScrollBarYAdjustment);
      END ELSE BEGIN
        { Pivot = LeftPivot }
        IF (AngleInDegrees >= 90) AND (AngleInDegrees < 270) THEN
          XOffset := (2 * SignalSemaphoreHeightScaled)
        ELSE
          XOffset := -(2 * SignalSemaphoreHeightScaled);

        TPointArray[I].X := Round((TransformArray[0] * LeftPivotSquareArray[I  * 2]      * SignalSemaphoreHeightScaled * 2)
                                + (TransformArray[1] * LeftPivotSquareArray[(I * 2) + 1] * SignalSemaphoreWidthScaled * 2) + X - ScrollBarXAdjustment - XOffset);
        TPointArray[I].Y := Round((TransformArray[2] * LeftPivotSquareArray[I  * 2]      * SignalSemaphoreHeightScaled * 2)
                                + (TransformArray[3] * LeftPivotSquareArray[(I * 2) + 1] * SignalSemaphoreWidthScaled * 2) + Y - ScrollBarYAdjustment);
      END;
    END; {FOR}

    WITH RailWindowBitmap.Canvas DO
      Polygon(TPointArray);
  END; { DrawSemaphore }

  PROCEDURE DrawIndicators(S : Integer);
  VAR
    Indicator : JunctionIndicatorType;

  BEGIN
    WITH RailWindowBitmap.Canvas DO BEGIN
      WITH Signals[S] DO BEGIN
        IF Signal_Direction = Up THEN BEGIN
          IndicatorX := Signal_LineX - IndicatorHorizontalSpacingScaled;
          IF Signal_Type = FourAspect THEN
            IndicatorX := IndicatorX - SignalHorizontalSpacingScaled;
        END ELSE
          IF Signal_Direction = Down THEN BEGIN
            IndicatorX := Signal_LineX + IndicatorHorizontalSpacingScaled;
            IF Signal_Type = FourAspect THEN
              IndicatorX := IndicatorX + SignalHorizontalSpacingScaled;
          END;
        MoveTo(IndicatorX - ScrollBarXAdjustment, Signal_LineWithVerticalSpacingY - ScrollBarYAdjustment);

        IF Signal_Indicator = TheatreIndicator THEN BEGIN
          Brush.Color := BackgroundColour;

          IF Signal_Direction = Up THEN BEGIN
            TheatreIndicatorX2 := Signal_LineX - TheatreIndicatorHorizontalSpacingScaled;
            IF Signal_Type = FourAspect THEN
              TheatreIndicatorX2 := TheatreIndicatorX2 - SignalHorizontalSpacingScaled;
            TheatreIndicatorX1 := TheatreIndicatorX2 - TheatreBoxWidth;
          END ELSE BEGIN
            TheatreIndicatorX1 := Signal_LineX + TheatreIndicatorHorizontalSpacingScaled;
            IF Signal_Type = FourAspect THEN
              TheatreIndicatorX1 := TheatreIndicatorX1 + SignalHorizontalSpacingScaled;
            TheatreIndicatorX2 := TheatreIndicatorX1 + TheatreBoxWidth;
          END;

          TheatreIndicatorY1 := Signal_LineWithVerticalSpacingY - (TheatreBoxHeight DIV 2);
          TheatreIndicatorY2 := Signal_LineWithVerticalSpacingY + (TheatreBoxHeight DIV 2);

          { First clear the area for the theatre indicator }
          Brush.Color := BackgroundColour;
          Pen.Color := BackgroundColour;
          Rectangle(Rect(TheatreIndicatorX1, TheatreIndicatorY1, TheatreIndicatorX2, TheatreIndicatorY2));

          Font.Height := -MulDiv(FWPRailWindow.ClientHeight, LineFontHeight, ZoomScalefactor);
          Font.Color := clWhite;
          IF Signal_IndicatorState = NoIndicatorLit THEN
            { Now the outline of the theatre indicator box }
            DrawOutline(Rect(TheatreIndicatorX1,
                                        TheatreIndicatorY1,
                                        TheatreIndicatorX2,
                                        TheatreIndicatorY2),
                                   ForegroundColour,
                                   NOT UndrawRequired,
                                   NOT UndrawToBeAutomatic)
          ELSE
            IF Signal_IndicatorState = QueryIndicatorLit THEN
              { Now draw the query }
              TextOut(TheatreIndicatorX1 - ScrollBarXAdjustment,
                      Signal_LineWithVerticalSpacingY - (TextHeight('?') DIV 2) - ScrollBarYAdjustment,
                      '?')
            ELSE
              IF Signal_Direction = Up THEN
                TextOut(TheatreIndicatorX2 - ScrollBarXAdjustment - TextWidth(Signal_TheatreIndicatorString),
                        Signal_LineWithVerticalSpacingY - (TextHeight('T') DIV 2) - ScrollBarYAdjustment,
                        Signal_TheatreIndicatorString)
              ELSE
                TextOut(TheatreIndicatorX1 - ScrollBarXAdjustment,
                        Signal_LineWithVerticalSpacingY - (TextHeight('T') DIV 2) - ScrollBarYAdjustment,
                        Signal_TheatreIndicatorString);
        END ELSE BEGIN
          { First clear the area for the indicators }
          Brush.Color := BackgroundColour;
          Pen.Color := BackgroundColour;
          Rectangle(Rect(Signal_IndicatorMouseRect.Left - ScrollBarXAdjustment,
                         Signal_IndicatorMouseRect.Top - ScrollBarYAdjustment,
                         Signal_IndicatorMouseRect.Right - ScrollBarXAdjustment,
                         Signal_IndicatorMouseRect.Bottom - ScrollBarYAdjustment));

          SavePenWidth := Pen.Width;
          Pen.Width := 2;

          RevisedIndicatorX := IndicatorX;

          { Adjust the X spacing of the indicators based on how many indicators there are - without this, if there were only upper indicators they would be a long way
            from the signal
          }
          LowerX := 0;
          MiddleX := 0;
          IF Signal_Direction = Up THEN BEGIN
            IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_Exists
            OR Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_Exists
            THEN BEGIN
              LowerX := IndicatorX;
              RevisedIndicatorX := IndicatorX - MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor);

              IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists
              OR Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists
              THEN BEGIN
                MiddleX := RevisedIndicatorX;
                UpperX := RevisedIndicatorX - MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor);
              END ELSE
                UpperX := RevisedIndicatorX + MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor);
            END ELSE
              { a slight adjustment to bring the middle arms nearer the signal }
              IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists
              OR Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists
              THEN BEGIN
                MiddleX := RevisedIndicatorX + MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor);
                UpperX := RevisedIndicatorX;
              END ELSE
                UpperX := RevisedIndicatorX + MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor);
          END ELSE BEGIN
            { Signal_Direction = Down }
            IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_Exists
            OR Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_Exists
            THEN BEGIN
              LowerX := IndicatorX;
              RevisedIndicatorX := IndicatorX + MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor);

              IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists
              OR Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists
              THEN BEGIN
                MiddleX := RevisedIndicatorX;
                UpperX := RevisedIndicatorX + MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor);
              END ELSE
                UpperX := RevisedIndicatorX - MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor);
            END ELSE
              { a slight adjustment to bring the middle arms nearer the signal }
              IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists
              OR Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists
              THEN BEGIN
                MiddleX := RevisedIndicatorX - MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor);
                UpperX := RevisedIndicatorX;
              END ELSE
                UpperX := RevisedIndicatorX - MulDiv(FWPRailWindow.ClientWidth, 3, ZoomScalefactor);
          END;

          FOR Indicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
            WITH Signal_JunctionIndicators[Indicator] DO BEGIN
              IF JunctionIndicator_Exists THEN BEGIN
                CASE Indicator OF
                  UpperLeftIndicator:
                    IF Signal_Direction = Up THEN BEGIN
                      SignalLeft := UpperX;
                      SignalTop := Signal_LineWithVerticalSpacingY + MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := UpperX - MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScalefactor);
                      SignalBottom := Signal_LineWithVerticalSpacingY + IndicatorVerticalSpacingScaled;
                    END ELSE BEGIN
                      SignalLeft := UpperX;
                      SignalTop := Signal_LineWithVerticalSpacingY - MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := UpperX + MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScalefactor);
                      SignalBottom := Signal_LineWithVerticalSpacingY - IndicatorVerticalSpacingScaled;
                    END;
                  MiddleLeftIndicator:
                    IF Signal_Direction = Up THEN BEGIN
                      SignalLeft := MiddleX;
                      SignalTop := Signal_LineWithVerticalSpacingY + MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := MiddleX;
                      SignalBottom := Signal_LineWithVerticalSpacingY + IndicatorVerticalSpacingScaled;
                    END ELSE BEGIN
                      SignalLeft := MiddleX;
                      SignalTop := Signal_LineWithVerticalSpacingY - MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := MiddleX;
                      SignalBottom := Signal_LineWithVerticalSpacingY - IndicatorVerticalSpacingScaled;
                    END;
                  LowerLeftIndicator:
                    IF Signal_Direction = Up THEN BEGIN
                      SignalLeft := LowerX;
                      SignalTop := Signal_LineWithVerticalSpacingY + MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := LowerX + MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScalefactor);
                      SignalBottom := Signal_LineWithVerticalSpacingY + IndicatorVerticalSpacingScaled;
                    END ELSE BEGIN
                      SignalLeft := LowerX;
                      SignalTop := Signal_LineWithVerticalSpacingY - MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := LowerX - MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScalefactor);
                      SignalBottom := Signal_LineWithVerticalSpacingY - IndicatorVerticalSpacingScaled;
                    END;
                  UpperRightIndicator:
                    IF Signal_Direction = Up THEN BEGIN
                      SignalLeft := UpperX;
                      SignalTop := Signal_LineWithVerticalSpacingY - MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := UpperX - MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScalefactor);
                      SignalBottom := Signal_LineWithVerticalSpacingY - IndicatorVerticalSpacingScaled;
                    END ELSE BEGIN
                      SignalLeft := UpperX;
                      SignalTop := Signal_LineWithVerticalSpacingY + MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := UpperX + MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScalefactor);
                      SignalBottom := Signal_LineWithVerticalSpacingY + IndicatorVerticalSpacingScaled
                    END;
                  MiddleRightIndicator:
                    IF Signal_Direction = Up THEN BEGIN
                      SignalLeft := MiddleX;
                      SignalTop := Signal_LineWithVerticalSpacingY - MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := MiddleX;
                      SignalBottom := Signal_LineWithVerticalSpacingY - IndicatorVerticalSpacingScaled;
                    END ELSE BEGIN
                      SignalLeft := MiddleX;
                      SignalTop := Signal_LineWithVerticalSpacingY + MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := MiddleX;
                      SignalBottom := Signal_LineWithVerticalSpacingY + IndicatorVerticalSpacingScaled;
                    END;
                  LowerRightIndicator:
                    IF Signal_Direction = Up THEN BEGIN
                      SignalLeft := LowerX;
                      SignalTop := Signal_LineWithVerticalSpacingY - MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := LowerX + MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScalefactor);
                      SignalBottom := Signal_LineWithVerticalSpacingY - IndicatorVerticalSpacingScaled;
                    END ELSE BEGIN
                      SignalLeft := LowerX;
                      SignalTop := Signal_LineWithVerticalSpacingY + MulDiv(FWPRailWindow.ClientHeight, 3, ZoomScalefactor);
                      SignalRight := LowerX - MulDiv(FWPRailWindow.ClientWidth, 5, ZoomScalefactor);
                      SignalBottom := Signal_LineWithVerticalSpacingY + IndicatorVerticalSpacingScaled;
                    END;
                END; {CASE}

                Pen.Color := ForegroundColour;
                CASE Indicator OF
                  UpperLeftIndicator:
                    IF Signal_IndicatorState = UpperLeftIndicatorLit THEN
                      Pen.Color := clWhite;
                  MiddleLeftIndicator:
                    IF Signal_IndicatorState = MiddleLeftIndicatorLit THEN
                      Pen.Color := clWhite;
                  LowerLeftIndicator:
                    IF Signal_IndicatorState = LowerLeftIndicatorLit THEN
                      Pen.Color := clWhite;
                  UpperRightIndicator:
                    IF Signal_IndicatorState = UpperRightIndicatorLit THEN
                      Pen.Color := clWhite;
                  MiddleRightIndicator:
                    IF Signal_IndicatorState = MiddleRightIndicatorLit THEN
                      Pen.Color := clWhite;
                  LowerRightIndicator:
                    IF Signal_IndicatorState = LowerRightIndicatorLit THEN
                      Pen.Color := clWhite;
                END; {CASE}

                MoveTo(SignalLeft - ScrollBarXAdjustment, SignalTop - ScrollBarYAdjustment);
                LineTo(SignalRight - ScrollBarXAdjustment, SignalBottom - ScrollBarYAdjustment);

                { Orient the rectangles }
                IF SignalLeft < SignalRight THEN BEGIN
                  JunctionIndicator_MouseRect.Left := SignalLeft;
                  JunctionIndicator_MouseRect.Right := SignalRight;
                END ELSE BEGIN
                  JunctionIndicator_MouseRect.Left := SignalRight;
                  JunctionIndicator_MouseRect.Right := SignalLeft;
                END;

                IF SignalTop < SignalBottom THEN BEGIN
                  JunctionIndicator_MouseRect.Top := SignalTop;
                  JunctionIndicator_MouseRect.Bottom := SignalBottom;
                END ELSE BEGIN
                  JunctionIndicator_MouseRect.Top := SignalBottom;
                  JunctionIndicator_MouseRect.Bottom := SignalTop;
                END;

                { special case for these two indicators as otherwise the mouse is trying to click on an exact vertical line }
                IF (Indicator = MiddleLeftIndicator) OR (Indicator = MiddleRightIndicator) THEN BEGIN
                  JunctionIndicator_MouseRect.Left := JunctionIndicator_MouseRect.Left - MulDiv(FWPRailWindow.ClientWidth, 1, ZoomScalefactor);
                  JunctionIndicator_MouseRect.Right := JunctionIndicator_MouseRect.Right + MulDiv(FWPRailWindow.ClientWidth, 1, ZoomScalefactor);
                END;
              END;
            END; {WITH}
          END; {FOR}

          Pen.Width := SavePenWidth;
        END;
      END; {WITH}
    END; {WITH}
  END; { DrawIndicators }

BEGIN { DrawSignal }
  TRY
    TopAspectX := 0;
    IndicatorX := 0;
    SignalLeft := 0;
    SignalTop := 0;
    SignalRight := 0;
    SignalBottom := 0;

    InitialiseScreenDrawingVariables;
    WITH RailWindowBitmap.Canvas DO BEGIN
      SColour1 := BackgroundColour;
      SColour2 := BackgroundColour;
      Font.Style := [fsBold];
      Font.Color := clWhite;
      Font.Height := -MulDiv(FWPRailWindow.ClientHeight, TheatreFontHeight, ZoomScalefactor);

      WITH Signals[S] DO BEGIN
//Region := CreateRectRgn(Signal_MouseRect.Left, Signal_MouseRect.Top, Signal_MouseRect.Right, Signal_MouseRect.Bottom);
//SelectClipRgn(Canvas.Handle, Region);
        { store the data for debugging }

        DrawSignalPost(S);

        IF InRecordLineDrawingMode OR ProgramStarting OR Signal_StateChanged THEN
          Log('S S=' + IntToStr(S)
                 + ' A=' + AspectToStr(Signals[S].Signal_Aspect, ShortStringType)
                 + ' I=' + IndicatorStateToStr(Signals[S].Signal_IndicatorState));

        IF Signal_StateChanged THEN
          Signal_StateChanged := False;

        IF NOT ShowSignalHiddenStationSignalAspects THEN
          Aspect := Signal_Aspect
        ELSE
          Aspect := Signal_HiddenStationSignalAspect;

//        MoveTo(Signal_LineX - ScrollBarXAdjustment, Signal_LineWithVerticalSpacingY - ScrollBarYAdjustment);
//        MoveTo(Signal_LineX, Signal_LineWithVerticalSpacingY);

        IF (Signal_Type = SemaphoreHome) OR (Signal_Type = SemaphoreDistant) THEN BEGIN
          { First clear the area for the semaphore signal }
          Brush.Color := BackgroundColour;
          Pen.Color := BackgroundColour;
          Rectangle(Rect(Signal_MouseRect.Left - ScrollBarXAdjustment,
                         Signal_MouseRect.Top - ScrollBarYAdjustment,
                         Signal_MouseRect.Right - ScrollBarXAdjustment,
                         Signal_MouseRect.Bottom - ScrollBarYAdjustment));

          Pen.Color := clBlack;
          IF EditedSignal = S THEN
            Brush.Color := clAqua
          ELSE
            IF Signal_Type = SemaphoreHome THEN
              Brush.Color := SignalAspectRed
            ELSE
              Brush.Color := SignalAspectYellow;

          { Now draw the rectangle }
          IF (Aspect = RedAspect) OR (Aspect = NoAspect) THEN BEGIN
            IF Signal_Direction = Up THEN
              DrawSemaphore(Signal_LineX, Signal_LineWithVerticalSpacingY, 0, RightPivot)
            ELSE
              DrawSemaphore(Signal_LineX, Signal_LineWithVerticalSpacingY, 180, RightPivot);
          END ELSE BEGIN
            Pen.Color := BackgroundColour;

            IF Signal_Quadrant = UpperQuadrant THEN BEGIN
              IF Signal_Direction = Up THEN
                DrawSemaphore(Signal_LineX, Signal_LineWithVerticalSpacingY, 45, RightPivot)
              ELSE
                DrawSemaphore(Signal_LineX, Signal_LineWithVerticalSpacingY, 225, RightPivot);
            END;

            IF Signal_Quadrant = LowerQuadrant THEN BEGIN
              IF Signal_Direction = Up THEN
                DrawSemaphore(Signal_LineX, Signal_LineWithVerticalSpacingY, 315, LeftPivot)
              ELSE
                DrawSemaphore(Signal_LineX, Signal_LineWithVerticalSpacingY, 135, LeftPivot);
            END;
          END;

          IF Signal_OutOfUse
          OR (signal_Aspect = NoAspect)
          OR { a safeguard - turn the signal off altogether! }
             ((Signal_AdjacentLine <> UnknownLine) AND (Lines[Signal_AdjacentLine].Line_TC = UnknownTrackCircuit))
          THEN BEGIN
            { setting a semaphore to no aspect is meaningless, so since white crosses are used on real railways, we can do the same }
            Pen.Color := clWhite;

            WITH Signal_MouseRect DO BEGIN
              MoveTo(Left - ScrollBarXAdjustment, Top - ScrollBarYAdjustment);
              LineTo(Right - ScrollBarXAdjustment, Bottom - ScrollBarYAdjustment);
              MoveTo(Right - ScrollBarXAdjustment, Top - ScrollBarYAdjustment);
              LineTo(Left - ScrollBarXAdjustment, Bottom - ScrollBarYAdjustment);
            END; {WITH}
          END;
        END ELSE BEGIN
         { Now draw indicators if any }
          IF Signal_Indicator <> NoIndicator THEN
            DrawIndicators(S);

          { Sort out what colour the signal aspect should be }
          IF EditedSignal = S THEN BEGIN
            SColour1 := ScreenComponentEditedColour1;
            SColour2 := ScreenComponentEditedColour1;
          END ELSE BEGIN
            IF Signal_OutOfUse
            OR { a safeguard - turn the signal off altogether! }
               ((Signal_AdjacentLine <> UnknownLine) AND (Lines[Signal_AdjacentLine].Line_TC = UnknownTrackCircuit))
            THEN BEGIN
              SColour1 := SignalAspectUnlit;
              SColour2 := SignalAspectUnlit;
            END ELSE BEGIN
              CASE Aspect OF
                RedAspect:
                  BEGIN
                    SColour1 := SignalAspectRed;
                    SColour2 := SignalAspectUnlit;
                  END;
                SingleYellowAspect:
                  BEGIN
                    SColour1 := SignalAspectYellow;
                    SColour2 := SignalAspectUnlit;
                  END;
                FlashingSingleYellowAspect:
                  BEGIN
                    IF Signal_LampIsOn THEN
                      SColour1 := SignalAspectYellow
                    ELSE
                      SColour1 := SignalAspectUnlit;
                    SColour2 := SignalAspectUnlit;
                  END;
                DoubleYellowAspect:
                  BEGIN
                    SColour1 := SignalAspectYellow;
                    SColour2 := SignalAspectYellow;
                  END;
                FlashingDoubleYellowAspect:
                  BEGIN
                    IF Signal_LampIsOn THEN BEGIN
                      SColour1 := SignalAspectYellow;
                      SColour2 := SignalAspectYellow;
                    END ELSE BEGIN
                      SColour1 := SignalAspectUnlit;
                      SColour2 := SignalAspectUnlit;
                    END;
                  END;
                GreenAspect:
                  BEGIN
                    SColour1 := SignalAspectGreen;
                    SColour2 := SignalAspectUnlit;
                  END;
                NoAspect:
                  BEGIN
                    SColour1 := SignalAspectUnlit;
                    SColour2 := SignalAspectUnlit;
                  END;
              END; {CASE}
            END;
          END;

          { All other signals except for calling-on signals, which are not yet written. First clear the area for the signal. }
          Brush.Color := BackgroundColour;
          Pen.Color := BackgroundColour;
          Rectangle(Rect(Signal_MouseRect.Left - ScrollBarXAdjustment,
                         Signal_MouseRect.Top - ScrollBarYAdjustment,
                         Signal_MouseRect.Right - ScrollBarXAdjustment,
                         Signal_MouseRect.Bottom - ScrollBarYAdjustment));

          { Now draw the circles and fill them in - 2 or 3 aspect signals first }
          Brush.Color := SColour1;
          Pen.Color := clBlack;
          EllipseX1 := Signal_LineX - SignalRadiusScaled;
          EllipseY1 := Signal_LineWithVerticalSpacingY - SignalRadiusScaled;
          EllipseX2 := Signal_LineX + SignalRadiusScaled;
          EllipseY2 := Signal_LineWithVerticalSpacingY + SignalRadiusScaled;
          Ellipse(EllipseX1 - ScrollBarXAdjustment,
                  EllipseY1 - ScrollBarYAdjustment,
                  EllipseX2 - ScrollBarXAdjustment,
                  EllipseY2 - ScrollBarYAdjustment);

          IF Signal_Type = FourAspect THEN BEGIN
            IF Signal_Direction = Up THEN
              TopAspectX := Signal_LineX - SignalHorizontalSpacingScaled
            ELSE
              IF Signal_Direction = Down THEN
                TopAspectX := Signal_LineX + SignalHorizontalSpacingScaled;

            Brush.Color := SColour2;
            EllipseX1 := TopAspectX - SignalRadiusScaled;
            EllipseY1 := Signal_LineWithVerticalSpacingY - SignalRadiusScaled;
            EllipseX2 := TopAspectX + SignalRadiusScaled;
            EllipseY2 := Signal_LineWithVerticalSpacingY + SignalRadiusScaled;
            Ellipse(EllipseX1 - ScrollBarXAdjustment,
                    EllipseY1 - ScrollBarYAdjustment,
                    EllipseX2 - ScrollBarXAdjustment,
                    EllipseY2 - ScrollBarYAdjustment);
          END;
        END;

        { Draw mouse access rectangles if required }
        IF ShowMouseRectangles THEN BEGIN
          DrawOutline(Signal_MouseRect, clYellow, NOT UndrawRequired, NOT UndrawToBeAutomatic);
          DrawOutline(Signal_PostMouseRect, clRed, NOT UndrawRequired, NOT UndrawToBeAutomatic);
          IF (Signal_Indicator <> NoIndicator) AND (Signal_Indicator <> JunctionIndicator) THEN
            DrawOutline(Signal_IndicatorMouseRect, clAqua, NOT UndrawRequired, NOT UndrawToBeAutomatic);

          FOR Indicator := UpperLeftIndicator TO LowerRightIndicator DO
            WITH Signal_JunctionIndicators[Indicator] DO
              IF JunctionIndicator_Exists THEN BEGIN
                DrawOutline(JunctionIndicator_MouseRect, clAqua, NOT UndrawRequired, NOT UndrawToBeAutomatic);

          END;
        END;
//SelectClipRgn(Canvas.Handle, 0);
//DeleteObject(Region);
      END; {WITH}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawSignal: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DrawSignal }

PROCEDURE DrawConnectionCh(Line : Integer; Direction : DirectionType);
{ Draw character at line starts/ends to indicate where lines are going when they disappear off the screen }
VAR
  ConnectionCh : String;
  ConnectionChRect : TRect;

BEGIN
  TRY
    IF FWPRailWindow <> NIL THEN BEGIN
      WITH RailWindowBitmap.Canvas DO BEGIN
        IF Direction = Up THEN BEGIN
          ConnectionCh := Lines[Line].Line_UpConnectionCh;
          ConnectionChRect := Lines[Line].Line_UpConnectionChRect;
        END ELSE BEGIN
          ConnectionCh := Lines[Line].Line_DownConnectionCh;
          ConnectionChRect := Lines[Line].Line_DownConnectionChRect;
        END;

        IF Lines[Line].Line_UpConnectionChBold OR Lines[Line].Line_DownConnectionChBold THEN BEGIN
          { undraw the old character }
          Font.Color := BackgroundColour;
          TextOut(ConnectionChRect.Left - ScrollBarXAdjustment, ConnectionChRect.Top - ScrollBarYAdjustment, ConnectionCh);
          { now draw the new one in bold }
          Font.Color := clWhite;
          Font.Style := [fsBold];
          Font.Height := -MulDiv(FWPRailWindow.ClientHeight, FWPRailWindowFontHeight * 2, ZoomScalefactor);

          TextOut(ConnectionChRect.Left - ((ConnectionChRect.Right - ConnectionChRect.Left)) - ScrollBarXAdjustment,
                  ConnectionChRect.Top - ((ConnectionChRect.Bottom - ConnectionChRect.Top) DIV 2) - ScrollBarYAdjustment,
                  ConnectionCh);
        END ELSE BEGIN
          { undraw the old character if there is one by redrawing the screen }
          Font.Color := BackgroundColour;
          Font.Style := [fsBold];
          Font.Height := -MulDiv(FWPRailWindow.ClientHeight, FWPRailWindowFontHeight * 2, ZoomScalefactor);

          TextOut(ConnectionChRect.Left - ((ConnectionChRect.Right - ConnectionChRect.Left)) - ScrollBarXAdjustment,
                  ConnectionChRect.Top - ((ConnectionChRect.Bottom - ConnectionChRect.Top) DIV 2) - ScrollBarYAdjustment,
                  ConnectionCh);

          { Draw the new one }
          Font.Color := ForegroundColour;
          Font.Style := [];
          Font.Height := -MulDiv(FWPRailWindow.ClientHeight, FWPRailWindowFontHeight, ZoomScalefactor);

          TextOut(ConnectionChRect.Left - ScrollBarXAdjustment, ConnectionChRect.Top - ScrollBarYAdjustment, ConnectionCh);
        END;

        IF ShowMouseRectangles THEN
          DrawOutline(ConnectionChRect, clFuchsia, NOT UndrawRequired, NOT UndrawToBeAutomatic);
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawConnectionCh:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawConnectionCh }

PROCEDURE DrawLineMainProcedure(Line : Integer; NewLineColour : Integer; ActiveTrain : Boolean; TempLineText : String);
{ Draw an individual line, with headcode if required, and store the line colour }
VAR
//  ActiveTrainText : Char;
//  DeltaX, DeltaY : Extended;
//  DownLineColour : TColor;
//  LineTextStr : String;
//  T : TrainIndex;
//  ScreenUpX : Integer;
//  ScreenUpY : Integer;
//  ScreenDownX : Integer;
//  ScreenDownY : Integer;
//  TempLine : Integer;
//  UpLineColour : TColor;
//  X1, X2, Y1, Y2 : Integer;
  ActiveTrainText : Char;
  AdjX, AdjY : Extended;
  DeltaX, DeltaY : Extended;
  DownLineColour : TColor;
  LineTextStr : String;
  T : TrainIndex;
  TextCos, TextSin : Extended;
  TextX, TextY : Integer;
  Theta : Extended;
  ScreenUpX : Integer;
  ScreenUpY : Integer;
  ScreenDownX : Integer;
  ScreenDownY : Integer;
  TempLine : Integer;
  UpLineColour : TColor;
  X1, X2, Y1, Y2 : Integer;
topleftx, toprightx, bottomleftx, bottomrightx : integer;
toplefty, toprighty, bottomlefty, bottomrighty : integer;
	AX, AY, BX, BY, DX, DY, MX, MY : Integer;
label endlabel;

  PROCEDURE DrawDottedLine(UpX, UpY, DownX, DownY : Integer);
  { Draw a dotted line - Delphi does not provide this option at a greater line width than one }
  VAR
    SavePenWidth, PenWidthCount : Integer;

  BEGIN
    TRY
      WITH RailWindowBitmap.Canvas DO BEGIN
        SavePenWidth := Pen.Width;
        Pen.Width := 1;
        { Draw the line pen.width times, each time a little lower }
        Dec(UpY);
        Dec(DownY);
        Dec(UpY);
        Dec(DownY);

        PenWidthCount := 0;
        WHILE PenWidthCount < SavePenWidth DO BEGIN
          MoveTo(UpX - ScrollBarXAdjustment, UpY - ScrollBarYAdjustment);
          LineTo(DownX - ScrollBarXAdjustment, DownY - ScrollBarYAdjustment);
          Inc(PenWidthCount);
          Inc(UpY);
          Inc(DownY);
        END;
        Pen.Width := SavePenWidth;
      END; {WITH}
    EXCEPT
      ON E : Exception DO
        Log('EG DrawDottedLine:' + E.ClassName + ' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { DrawDottedLine }

BEGIN
  TRY
    IF FWPRailWindow <> NIL THEN BEGIN
      LineTextStr := '';
      InitialiseScreenDrawingVariables;
      WITH RailWindowBitmap.Canvas DO BEGIN
        WITH Lines[Line] DO BEGIN
          ScreenUpX := MapGridXToScreenX(Line_GridUpX);
          ScreenUpY := MapGridYToScreenY(Line_GridUpY);
          ScreenDownX := MapGridXToScreenX(Line_GridDownX);
          ScreenDownY := MapGridYToScreenY(Line_GridDownY);

          IF Line_TypeOfLine = SidingLine THEN
            Pen.Style := SidingPenStyle
          ELSE
            IF Line_TypeOfLine = FiddleyardLine THEN
              Pen.Style := FiddleyardLinePenStyle
            ELSE
              IF (Line_TypeOfLine = ProjectedLine) THEN
                Pen.Style := ProjectedLinePenStyle;

          { Work out what to display on the line }
          IF Line_TC <> UnknownTrackCircuit THEN BEGIN
            WITH TrackCircuits[Lines[Line].Line_TC] DO BEGIN
              CASE TC_OccupationState OF
                TCFeedbackOccupationButOutOfUse:
                  BEGIN
                    Font.Color := TCFeedbackOccupationButOutOfUseColour;
                    LineTextStr := 'X';
                  END;
                TCOutOfUseSetByUser:
                  BEGIN
                    Font.Color := TCOutOfUseSetByUserColour;
                    LineTextStr := 'X';
                    Pen.Style := TCOutOfUseSetByUserPenStyle;
                  END;
                TCOutOfUseAsNoFeedbackreceived:
                  BEGIN
                    Font.Color := TCOutOfUseAsNoFeedbackReceivedColour;
                    LineTextStr := 'X';
                    Pen.Style := TCOutOfUseAsNoFeedbackReceivedPenStyle;
                  END;
                TCPermanentFeedbackOccupation:
                  BEGIN
                    Font.Color := TCPermanentFeedbackOccupationColour;
                    IF TC_LocoChip <> UnknownLocoChip THEN
                      LineTextStr := LocoChipToStr(TC_LocoChip)
                    ELSE
                      LineTextStr := '?';
                    Pen.Style := TCPermanentFeedbackOccupationPenStyle;
                  END;
                TCPermanentOccupationSetByUser:
                  BEGIN
                    Font.Color := TCPermanentOccupationSetByUserColour;
                    IF TC_LocoChip <> UnknownLocoChip THEN
                      LineTextStr := LocoChipToStr(TC_LocoChip)
                    ELSE
                      LineTextStr := '';
                    Pen.Style := TCPermanentOccupationSetByUserPenStyle;
                  END;
                TCPermanentSystemOccupation:
                  BEGIN
                    Font.Color := TCPermanentSystemOccupationColour;
                    IF TC_LocoChip <> UnknownLocoChip THEN
                      LineTextStr := LocoChipToStr(TC_LocoChip)
                    ELSE
                      LineTextStr := '?';
                    Pen.Style := TCPermanentSystemOccupationPenStyle;
                  END;
                TCLocoOutOfPlaceOccupation:
                  BEGIN
                    Font.Color := TCLocoOutOfPlaceOccupationColour;
                    IF TC_LocoChip <> UnknownLocoChip THEN
                      LineTextStr := IntToStr(TC_LocoChip);
                    Pen.Style := TCLocoOutOfPlaceOccupationPenStyle;
                  END;
              ELSE {CASE}
                BEGIN
                  IF DisplayLocoChipNums THEN BEGIN
                    Font.Color := NewLineColour;
                    IF TC_LocoChip <> UnknownLocoChip THEN
                      LineTextStr := IntToStr(TC_LocoChip);
                  END ELSE
                    IF DisplayLocoHeadcodes THEN BEGIN
                      IF ActiveTrain THEN
                        Font.Color := TrainActiveColour
                      ELSE
                        Font.Color := TrainInactiveColour;
                      IF TC_Headcode <> '' THEN
                        LineTextStr := TC_Headcode;
                    END ELSE
                      IF DisplayRoutesAndJourneys THEN BEGIN
                        IF TC_LocoChip <> UnknownLocoChip THEN BEGIN
                          T := GetTrainIndexFromLocoChip(TC_LocoChip);
                          IF T <> UnknownTrainIndex THEN
                            LineTextStr := IntToStr(TrackCircuits[Line_TC].TC_LockedForRoute) + ',' + IntToStr(TC_Journey);
                        END;
                      END;
                END;
              END; {CASE}
            END; {WITH}
          END;

          IF ThinLineMode THEN
            Pen.Width := WindowPenWidth
          ELSE
            Pen.Width := FullScreenPenWidth;

          IF (TempLineText <> '') AND (TempLineText <> ClearLineString) THEN BEGIN
            { throw away the original line text and substitute the temporary text }
            LineTextStr := TempLineText;
            Font.Color := NewLineColour;
          END;

          IF Line_OutOfUseState = OutOfUse THEN BEGIN
            { Draw a red lamp and line across the track }
            X1 := ScreenUpX;
            Y1 := ScreenUpY - BufferStopVerticalSpacingScaled;
            Y2 := ScreenUpY + BufferStopVerticalSpacingScaled;
            DrawRedLampAndVerticalLine(X1, Y1, Y2, ForegroundColour);

            X1 := ScreenDownX;
            Y1 := ScreenDownY - BufferStopVerticalSpacingScaled;
            Y2 := ScreenDownY + BufferStopVerticalSpacingScaled;
            DrawRedLampAndVerticalLine(X1, Y1, Y2, ForegroundColour);

            Pen.Style := psDot;
          END;

          { Save current and old colour data }
          IF Line_CurrentColour <> NewLineColour THEN BEGIN
            Line_OldColour := Line_CurrentColour;
            Line_CurrentColour := NewLineColour;
          END;

          IF InRecordLineDrawingMode THEN BEGIN
            IF ActiveTrain THEN
              ActiveTrainText := 'Y'
            ELSE
              ActiveTrainText := 'N';

            IF Lines[Line].Line_TC <> UnknownTrackCircuit THEN
              Log('T ' + StringOfChar(' ', 68) + '<<' + LineToStr(Line) + ' (' + IntToStr(Lines[Line].Line_TC) + ')'
                       + ' ' + ColourToStr(NewLineColour) + ' ' + LineTextStr + ' ' + ActiveTrainText + '>>')
            ELSE
              Log('T ' + StringOfChar(' ', 68) + '<<' + LineToStr(Line) + ' ' + ColourToStr(NewLineColour) + ' ' + LineTextStr
                       + ' ' + ActiveTrainText + '>>');
          END;

          { Clear any previous text away }
          IF (LineTextStr <> '') OR (TempLineText <> '') THEN BEGIN
            IF (ScreenUpY = ScreenDownY)
            AND ((ScreenDownX - ScreenUpX > TextWidth('---- ')) OR (ScreenUpX - ScreenDownX > TextWidth('---- ')))
            THEN BEGIN
              X1 := ScreenUpX + ((ScreenDownX - ScreenUpX - TextWidth('MMMM')) DIV 2) - ScrollBarXAdjustment;
              Y1 := ScreenUpY - (TextHeight('M') DIV 2) - ScrollBarYAdjustment;
              X2 := ScreenDownX - ((ScreenDownX - ScreenUpX - TextWidth('MMMM')) DIV 2) - ScrollBarXAdjustment;
              Y2 := ScreenUpY + (TextHeight('M') DIV 2) - ScrollBarYAdjustment;
              Brush.Color := BackgroundColour;
              FillRect(Rect(X1 - ScrollBarXAdjustment,
                            Y1 - ScrollBarYAdjustment,
                            X2 - ScrollBarXAdjustment,
                            Y2 - ScrollBarYAdjustment));
            END;
          END;

          { Draw this line in the colour of the adjacent lines if it is not track circuited }
          IF Lines[Line].Line_TC = UnknownTrackCircuit THEN BEGIN
            IF (Line_NextUpLine <> UnknownLine) AND (Line_NextDownLine <> UnknownLine) THEN BEGIN
              UpLineColour := Lines[Line_NextUpLine].Line_CurrentColour;
              DownLineColour := Lines[Line_NextDownLine].Line_CurrentColour;
              IF (UpLineColour = DownLineColour) AND (UpLineColour <> NewLineColour) THEN
                NewLineColour := UpLineColour;
            END;
          END;

          IF ScreenColoursSetForPrinting THEN
            Font.Color := clBlack
          ELSE
            Pen.Color := NewLineColour;

          IF ThinLineMode THEN BEGIN
//            MoveTo(ScreenUpX, ScreenUpY - ScrollBarYAdjustment);
//            LineTo(ScreenDownX, ScreenDownY - ScrollBarYAdjustment);
            MoveTo(ScreenUpX - ScrollBarXAdjustment, ScreenUpY - ScrollBarYAdjustment);
            LineTo(ScreenDownX - ScrollBarXAdjustment, ScreenDownY - ScrollBarYAdjustment);
          END ELSE BEGIN
            CASE Pen.Style OF
              psDashDot, psDot, psDashDotDot:
                DrawDottedLine(ScreenUpX, ScreenUpY, ScreenDownX, ScreenDownY);
              psSolid:
                BEGIN
                  MoveTo(ScreenUpX - ScrollBarXAdjustment, ScreenUpY - ScrollBarYAdjustment);
                  LineTo(ScreenDownX - ScrollBarXAdjustment, ScreenDownY - ScrollBarYAdjustment);
                END;
            END; {CASE}
          END;
//          IF ShowLineOccupationDetail AND (LineTextStr <> '') AND (TempLineText <> ClearLineString) THEN BEGIN
            { needs text if there's room }
begin
//linetextstr := 'hello';
            IF ((ScreenDownX - ScreenUpX > TextWidth(LineTextStr)) OR (ScreenUpX - ScreenDownX > TextWidth(LineTextStr)))
            THEN BEGIN
              { clear space for the text }
              Brush.Color := BackgroundColour;
              IF ScreenColoursSetForPrinting THEN
                Font.Color := clBlack;
              Font.Height := -MulDiv(FWPRailWindow.ClientHeight, LineFontHeight, ZoomScalefactor);

              { this might not be needed if (ScreenUpX<ScreenDownX) is constant }
              if ScreenUpX<ScreenDownX THEN BEGIN
 	              AX := ScreenUpX;
 	              AY := ScreenUpY;
 	              BX := ScreenDownX;
 	              BY := ScreenDownY;
 	            END ELSE BEGIN
 	              AX := ScreenDownX;
 	              AY := ScreenDownY;
 	              BX := ScreenUpX;
 	              BY := ScreenUpY;
 	            END;

              DX := BX-AX;
              DY := BY-AY;
              Theta := ArcTan2(-DY, DX); // -DY because ArcTan2 uses up as +ve y, not down.

							AdjX := -TextWidth(LineTextStr)/2;
							AdjY := -abs(Font.Height/2);
							TextCos := cos(Theta);
							TextSin := sin(Theta);
							TextX := ((AX+BX) DIV 2) + Round(AdjX*TextCos - AdjY*TextSin);
							TextY := ((AY+BY) DIV 2) - Round(AdjX*TextSin - AdjY*TextCos);

              Font.Orientation := Round(RadToDeg(Theta) * 10);
              TextOut(TextX - ScrollBarXAdjustment, TextY - ScrollBarYAdjustment, LineTextStr);

            END;
          END;

          { Draw characters at line starts/ends to indicate where lines are going when they disappear off the screen. (Although it is unlikely that a line would have a
            character at both ends, this eventuality is catered for).
          }
          IF (Lines[Line].Line_UpConnectionCh <> '') AND (Lines[Line].Line_UpConnectionCh <> ' ') THEN
            DrawConnectionCh(Line, Up);

          IF (Lines[Line].Line_DownConnectionCh <> '') AND (Lines[Line].Line_DownConnectionCh <> ' ') THEN
            DrawConnectionCh(Line, Down);

          IF ShowMouseRectangles THEN
            DrawOutline(Line_MousePolygon, clGreen, NOT UndrawRequired, NOT UndrawToBeAutomatic);

          IF CreateLineMode THEN BEGIN
            { If we're editing the line in create line mode, draw handles so the line can be moved }
            IF Lines[Line].Line_ShowHandles THEN BEGIN
              CalculateLinePolygons(Line);
              DrawOutline(Line_UpHandlePolygon, clGreen, NOT UndrawRequired, NOT UndrawToBeAutomatic);
              DrawOutline(Line_DownHandlePolygon, clGreen, NOT UndrawRequired, NOT UndrawToBeAutomatic);
              DrawOutline(Line_MidHandlePolygon, clGreen, NOT UndrawRequired, NOT UndrawToBeAutomatic);
            END;

            { And show any overlapping lines, at least for horizontal lines }
            TempLine := 0;
            WHILE TempLine <= High(Lines) DO BEGIN
              IF Line <> TempLine THEN BEGIN
                IF (Line_GridUpY = Line_GridDownY)
                AND ((Line_GridUpY = Lines[TempLine].Line_GridUpY) AND (Line_GridDownY = Lines[TempLine].Line_GridDownY))
                THEN BEGIN
                  IF ((Line_GridUpX < Lines[TempLine].Line_GridDownX) AND (Line_GridDownX > Lines[TempLine].Line_GridUpX)) THEN BEGIN
                    Pen.Color := clFWPPink;
                    MoveTo(MapGridXToScreenX(Max(Line_GridUpX, Lines[TempLine].Line_GridUpX)), MapGridYToScreenY(Line_GridUpY));
                    LineTo(MapGridXToScreenX(Min(Line_GridDownX, Lines[TempLine].Line_GridDownX)), MapGridYToScreenY(Line_GridDownY));
                  END;
                END;
              END;
              Inc(TempLine);
            END; {WHILE}
          END;
        END; {WITH}
      END; {WITH}
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DrawLineMainProcedure: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawLineMainProcedure }

PROCEDURE DrawLine{1}(Line : Integer; NewLineColour : Integer; ActiveTrain : Boolean); Overload;
{ Draw an individual line, with headcode if required, and store the line colour }
CONST
  TempLineText = '';

BEGIN
  DrawLineMainProcedure(Line, NewLineColour, ActiveTrain, TempLineText);
END; { DrawLine-1 }

PROCEDURE DrawLine{2}(Line : Integer; NewLineColour : Integer; ActiveTrain : Boolean; TempLineText : String); Overload;
{ Draw an individual line, with headcode if required, and store the line colour }
BEGIN
  DrawLineMainProcedure(Line, NewLineColour, ActiveTrain, TempLineText);
END; { DrawLine-2 }

PROCEDURE DrawTrackCircuit{1}(TC : Integer; TCColour : TColour); Overload;
{ Draws a given track circuit }
VAR
  Line : Integer;

BEGIN
  TRY
    IF TC <> UnknownTrackCircuit THEN BEGIN
      Line := 0;
      WHILE Line <= High(Lines) DO BEGIN
        IF Lines[Line].Line_TC = TC THEN
          DrawLine(Line, TCColour, ActiveTrain);
        Inc(Line);
      END;
    END;
    InvalidateScreen(UnitRef, 'DrawTrackCircuit-1');
  EXCEPT
    ON E : Exception DO
      Log('EG DrawTrackCircuit-1:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawTrackCircuit-1 }

PROCEDURE DrawTrackCircuit{2}(TC : Integer; TCColour : TColour; TempLineText : String); Overload;
{ Draws a given track circuit - this version is used by Replay to add train descriptions }
VAR
  Line : Integer;

BEGIN
  TRY
    IF TC <> UnknownTrackCircuit THEN BEGIN
      Line := 0;
      WHILE Line <= High(Lines) DO BEGIN
        IF Lines[Line].Line_TC = TC THEN
          DrawLine(Line, TCColour, ActiveTrain, TempLineText);
        Inc(Line);
      END;
    END;
    InvalidateScreen(UnitRef, 'DrawTrackCircuit-2');
  EXCEPT
    ON E : Exception DO
      Log('EG DrawTrackCircuit-2:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawTrackCircuit-2 }

PROCEDURE DrawTrackCircuitsWithAdjoiningTrackCircuits(TC : Integer; TCColour1, TCColour2 : TColour);
{ Draw a track circuit and show which track circuits adjoin it }
VAR
  AdjacentUpTC, AdjacentDownTC : Integer;

BEGIN
  TRY
    { Draw the original track circuit }
    DrawTrackCircuit(TC, TCColour1);

    FindAdjoiningTrackCircuits(TC, AdjacentUpTC, AdjacentDownTC);
    { and draw the adjacent ones }
    DrawTrackCircuit(AdjacentUpTC, TCColour2);
    DrawTrackCircuit(AdjacentDownTC, TCColour2);

    Debug('Drawing original TC=' + IntToStr(TC)
          + ' and adjacent TCs:' + IntToStr(AdjacentUpTC) + ' and ' + IntToStr(AdjacentDownTC));
  EXCEPT
    ON E : Exception DO
      Log('EG DrawTrackCircuitsWithAdjoiningTrackCircuits:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawTrackCircuitsWithAdjoiningTrackCircuits }

PROCEDURE DrawPoint(P : Integer; Colour : TColour);
{ Draw a point }
VAR
  RouteLockingArray : IntegerArrayType;
  ScreenX : Integer;
  ScreenY : Integer;
  ScreenFarX : Integer;
  ScreenFarY : Integer;

BEGIN
  TRY
    InitialiseScreenDrawingVariables;
    WITH RailWindowBitmap.Canvas DO BEGIN
      WITH Points[P] DO BEGIN
        ScreenX := MapGridXToScreenX(Point_X);
        ScreenY := MapGridYToScreenY(Point_Y);
        ScreenFarX := MapGridXToScreenX(Point_FarX);
        ScreenFarY := MapGridYToScreenY(Point_FarY);

        { Undraw the previous state by increasing the pen width when rubbing out the line - otherwise a faint trace of the line gets left behind (I know this is a hack,
          but it works!)
        }
        Pen.Color := PointUndrawColour;
        IF ThinLineMode THEN
          Pen.Width := WindowPenWidth + 1
        ELSE
          Pen.Width := FullScreenPenWidth + 1;

        MoveTo(ScreenX, ScreenY);
        IF Point_PresentState = Straight THEN BEGIN
          { clear away the faint color from where we're going to draw }
          Pen.Color := BackgroundColour;
          LineTo(ScreenFarX, ScreenY);
          { now rub out the previous point setting, but leave a faint line! }
          Pen.Color := PointUndrawColour;
          MoveTo(ScreenX, ScreenY);
          LineTo(ScreenFarX, ScreenFarY)
        END ELSE BEGIN
          IF Point_PresentState = Diverging THEN BEGIN
            { clear away the faint color from where we're going to draw }
            Pen.Color := BackgroundColour;
            LineTo(ScreenFarX, ScreenFarY);
            { now rub out the previous point setting, but leave a faint line! }
            Pen.Color := PointUndrawColour;
            MoveTo(ScreenX, ScreenY);
            LineTo(ScreenFarX, ScreenY);
          END ELSE BEGIN
            { PointState = PointStateUnknown }
            Pen.Color := PointUndrawColour;
            LineTo(ScreenFarX, ScreenFarY);
            MoveTo(ScreenX, ScreenY);
            LineTo(ScreenFarX, ScreenY);
          END;
        END;

        { Now draw the point }
        IF ThinLineMode THEN
          Pen.Width := WindowPenWidth
        ELSE
          Pen.Width := FullScreenPenWidth;

        Pen.Style := psSolid;

        IF Point_OutOfUse THEN
          Pen.Color := PointOutOfUseColour
        ELSE
          IF Point_LockedByUser THEN
            Pen.Color := PointLockedByUserColour
          ELSE
            IF Point_ManualOperation THEN
              Pen.Color := PointManualOperationColour
            ELSE
              IF ShowPointDefaultState THEN BEGIN
                Point_PresentState := Point_DefaultState;
                Pen.Color := PointDefaultStateColour;
              END ELSE BEGIN
                { if it's not a catch point, see if the point is track occupied }
                IF PointIsCatchPoint(P) THEN
                  Pen.Color := GetTrackCircuitStateColour(Lines[Point_StraightLine].Line_TC)
                ELSE BEGIN
                  IF (Point_PresentState = Diverging)
                  AND (Lines[Point_DivergingLine].Line_TC <> UnknownTrackCircuit)
                  AND (TrackCircuits[Lines[Point_DivergingLine].Line_TC].TC_OccupationState <> TCUnoccupied)
                  THEN
                    Pen.Color := GetTrackCircuitStateColour(Lines[Point_DivergingLine].Line_TC)
                  ELSE
                    IF (Point_PresentState = Straight)
                    AND (Lines[Point_StraightLine].Line_TC <> UnknownTrackCircuit)
                    AND (TrackCircuits[Lines[Point_StraightLine].Line_TC].TC_OccupationState <> TCUnoccupied)
                    THEN
                      Pen.Color := GetTrackCircuitStateColour(Lines[Point_StraightLine].Line_TC)
                    ELSE
                      IF (Point_PresentState = Diverging)
                      AND (Lines[Point_DivergingLine].Line_TC = UnknownTrackCircuit)
                      AND ((Lines[Point_HeelLine].Line_TC <> UnknownTrackCircuit) AND (TrackCircuits[Lines[Point_HeelLine].Line_TC].TC_OccupationState <> TCUnoccupied))
                      THEN
                        Pen.Color := GetTrackCircuitStateColour(Lines[Point_HeelLine].Line_TC)
                      ELSE
                        IF (Point_PresentState = Straight)
                        AND (Lines[Point_StraightLine].Line_TC = UnknownTrackCircuit)
                        AND ((Lines[Point_HeelLine].Line_TC <> UnknownTrackCircuit) AND (TrackCircuits[Lines[Point_HeelLine].Line_TC].TC_OccupationState <> TCUnoccupied))
                        THEN
                          Pen.Color := GetTrackCircuitStateColour(Lines[Point_HeelLine].Line_TC)
                        ELSE
                          { Standard drawing : if the point is locked by any route then colour it as the route }
                          IF PointIsLockedByAnyRoute(P, RouteLockingArray) THEN
                            Pen.Color := clWhite
                          ELSE
                            Pen.Color := Colour;
          END;
        END;

        MoveTo(ScreenX, ScreenY);

        IF ShowPointsStraightAndDiverging THEN BEGIN
          Pen.Color := clFWPOrange;
          LineTo(ScreenFarX, ScreenY);
          MoveTo(ScreenX, ScreenY);
          Pen.Color := clYellow;
          LineTo(ScreenFarX, ScreenFarY);
        END ELSE
          IF P = EditedPoint THEN BEGIN
            Pen.Color := ScreenComponentEditedColour1;
            Pen.Width := 2;
            LineTo(ScreenFarX, ScreenY);
            MoveTo(ScreenX, ScreenY);
            LineTo(ScreenFarX, ScreenFarY);
          END ELSE
            IF Point_PresentState = PointOutOfAction THEN BEGIN
              Pen.Width := 2;
              LineTo(ScreenFarX, ScreenY);
              MoveTo(ScreenX, ScreenY);
              LineTo(ScreenFarX, ScreenFarY);
            END ELSE BEGIN
              { a normal state }
              IF NOT ShowPointDefaultState THEN BEGIN
                IF Point_PresentState = Straight THEN
                  LineTo(ScreenFarX, ScreenY)
                ELSE
                  IF Point_PresentState = Diverging THEN
                    LineTo(ScreenFarX, ScreenFarY)
                  ELSE BEGIN
                    { PresentState = PointStateUnknown }
                    IF ShowPointDefaultState THEN
                      Pen.Color := ForegroundColour;
                    LineTo(ScreenFarX, ScreenY);
                    MoveTo(ScreenX, ScreenY);
                    LineTo(ScreenFarX, ScreenFarY);
                  END;
              END ELSE BEGIN
                { ShowPointDefaultState }
                IF Point_DefaultState = Straight THEN
                  LineTo(ScreenFarX, ScreenY)
                ELSE
                  IF Point_DefaultState = Diverging THEN
                    LineTo(ScreenFarX, ScreenFarY);
              END;
            END;
      END; {WITH}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawPoint: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DrawPoint }

PROCEDURE DrawPointNum(P : Integer; Colour : TColour);
{ Put the number of the point on the diagram }
VAR
  I, J : Integer;
  LockingMsg : String;
  NumberText : String;

BEGIN
  TRY
    InitialiseScreenDrawingVariables;
    WITH RailWindowBitmap.Canvas DO BEGIN
      Brush.Color := BackgroundColour;
      Font.Color := Colour;
      Font.Style := [fsBold];
      NumberText := '';

      WITH Points[P] DO BEGIN
        IF ShowLenzPointNumbers THEN BEGIN
          { displaying Lenz point numbers }
          IF ScreenColoursSetForPrinting THEN
            Font.Color := clBlack
          ELSE
            IF Point_ManualOperation THEN
              Font.Color := PointManualOperationColour;
          NumberText := IntToStr(Point_LenzNum);
        END ELSE
          IF ShowPointType THEN BEGIN
            { displaying what sort of point it is }
            CASE Point_Type OF
              OrdinaryPoint:
                BEGIN
                  NumberText := IntToStr(P);
                  Font.Color := clYellow;
                END;
              CrossOverPoint:
                BEGIN
                  NumberText := 'CO' + IntToStr(P) + '/' + IntToStr(Point_RelatedPoint);
                  Font.Color := clAqua;
                END;
              ThreeWayPointA:
                BEGIN
                  NumberText := '3a ' + IntToStr(P) + '/' + IntToStr(Point_RelatedPoint);
                  Font.Color := clRed;
                END;
              ThreeWayPointB:
                BEGIN
                  NumberText := '3b ' + IntToStr(P) + '/' + IntToStr(Point_RelatedPoint);
                  Font.Color := clMoneyGreen;
                END;
              SingleSlip:
                BEGIN
                  NumberText := 'S ' + IntToStr(P) + '/' + IntToStr(Point_RelatedPoint);
                  Font.Color := clFuchsia;
                END;
              DoubleSlip:
                BEGIN
                  NumberText := 'D ' + IntToStr(P) + '/' + IntToStr(Point_RelatedPoint);
                  Font.Color := clSkyBlue;
                END;
              ProtectedPoint:
                BEGIN
                  NumberText := 'PP ' + IntToStr(P) + '/' + IntToStr(Point_RelatedPoint);
                  Font.Color := clFWPPink;
                END;
              CatchPointUp:
                BEGIN
                  NumberText := 'CPU ' + IntToStr(P) + '/' + IntToStr(Point_RelatedPoint);
                  Font.Color := clFWPPink;
                END;
              CatchPointDown:
                BEGIN
                  NumberText := 'CPD ' + IntToStr(P) + '/' + IntToStr(Point_RelatedPoint);
                  Font.Color := clLime;
                END;
            ELSE
              BEGIN
                NumberText := '?';
                Font.Color := clFWPOrange;
              END;
            END; {CASE}
          END ELSE
            IF ShowPointFeedbackDataInUse THEN BEGIN
              { displaying point feedback data }
              NumberText := '';
              FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
                FOR J := 1 TO 8 DO BEGIN
                  IF FeedbackUnitRecords[I].Feedback_InputTypeArray[J] = PointFeedback THEN BEGIN
                    NumberText := IntToStr(I) + IntToStr(J);
                    IF ScreenColoursSetForPrinting THEN
                      Font.Color := clBlack
                    ELSE
//                      IF FeedbackUnitInUseArray[I] THEN
//                        Font.Color := PointFeedbackDataInUseColour
//                      ELSE
//                        Font.Color := PointFeedbackDataOutOfUseColour;
                  END;
                END;
              END; {FOR}
              IF NumberText = '' THEN BEGIN
                { no feedback unit assigned }
                Font.Color := PointsWithoutFeedbackColour;
                NumberText := '0000';
              END;
            END ELSE BEGIN
              IF ShowPointsThatAreLocked THEN BEGIN
                IF PointIsLocked(P, LockingMsg) THEN
                  NumberText := IntToStr(P);
              END ELSE
                { just displaying all point numbers }
                NumberText := IntToStr(P);
            END;

        WITH Point_MouseRect DO BEGIN
          Font.Height := -MulDiv(FWPRailWindow.ClientHeight, LineFontHeight, ZoomScaleFactor);
          IF Point_FarY < Point_Y THEN
            TextOut(Right,
                    Top + ((Bottom - Top - TextHeight(NumberText)) DIV 2),
                    NumberText)
          ELSE
            TextOut(Left - TextWidth(NumberText),
                    Top + ((Bottom - Top - TextHeight(NumberText)) DIV 2),
                    NumberText);
        END; {WITH}
      END; {WITH}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawPointNum:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawPointNum }

PROCEDURE DrawPointFeedbackDataInSeparateColours;
{ Put the number of the point on the diagram }
VAR
  ColourNum : Integer;
  I, J : Integer;
  NumberText : String;
  P : Integer;
  TempNum : Integer;

BEGIN
  TRY
    InitialiseScreenDrawingVariables;
    WITH RailWindowBitmap.Canvas DO BEGIN
      Brush.Color := BackgroundColour;
      Font.Style := [fsBold];
      { show which Lenz feedback unit is being used }
      Font.Height := -MulDiv(FWPRailWindow.ClientHeight, LineFontHeight, ZoomScaleFactor);

      NumberText := '';
      { Go through all the colours in sequence, but start randomly - this is better than choosing colours at random, as one can end up with too many colours the same that
        way. Note: the From in RandomRange is included in the results, but the To is not.
      }
      ColourNum := RandomRange(1, MaxColourNum + 1);
      FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
        Inc(ColourNum);

        { Add another bit of randomness to the process, as otherwise the adjoining units might always come out the same colour. Note: the From in RandomRange is included in
          the results, but the To is not.
        }
        TempNum := RandomRange(1, 3);
        IF TempNum = 1 THEN
          Inc(ColourNum);
        IF ColourNum > MaxColourNum THEN
          ColourNum := 1;
        Font.Color := ColoursArray[ColourNum];

        FOR J := 1 TO 8 DO BEGIN
          IF FeedbackUnitRecords[I].Feedback_InputTypeArray[J] = PointFeedback THEN BEGIN
            FOR P := 0 TO High(Points) DO BEGIN
              WITH Points[P] DO BEGIN
                IF (Point_FeedbackUnit = I) AND (Point_FeedbackInput = J) THEN BEGIN
                  NumberText := IntToStr(I) + IntToStr(J);
                  WITH Point_MouseRect DO BEGIN
                    IF Point_FarY < Point_Y THEN
                      TextOut(Right - ScrollBarXAdjustment,
                              Top + ((Bottom - Top - TextHeight(NumberText)) DIV 2) - ScrollBarYAdjustment,
                              NumberText)
                    ELSE
                      TextOut(Left - TextWidth(NumberText) - ScrollBarXAdjustment,
                              Top + ((Bottom - Top - TextHeight(NumberText)) DIV 2) - ScrollBarYAdjustment,
                              NumberText);
                  END; {WITH}
                END;
              END; {WITH}
            END; {FOR}
          END;
        END; {FOR}
      END; {FOR}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawPointFeedbackDataInSeparateColours:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawPointFeedbackDataInSeparateColours }

PROCEDURE DrawLenzPointUnitGroups;
{ Put the number of the point Lenz unit on the diagram }
VAR
  Colour : TColour;
  ColourNum : Integer;
  P1, P2 : Integer;
  TempPointArray : ARRAY Of Boolean;
  UnitNum : Integer;

BEGIN
  TRY
    InitialiseScreenDrawingVariables;
    ShowLenzPointNumbers := True;

    WITH RailWindowBitmap.Canvas DO BEGIN
      Brush.Color := BackgroundColour;
      Font.Style := [fsBold];
      Font.Height := -MulDiv(FWPRailWindow.ClientHeight, LineFontHeight, ZoomScaleFactor);

      { Go through all the colours in sequence, but start randomly - this is better than choosing colours at random, as one can end up with too many colours the same that
        way. Note: the From in RandomRange is included in the results, but the To is not.
      }
      ColourNum := RandomRange(1, MaxColourNum + 1);
      Colour := ColoursArray[ColourNum];

      SetLength(TempPointArray, Length(Points));
      FOR P1 := 0 TO High(TempPointArray) DO
        TempPointArray[P1] := False;

      UnitNum := 0;
      FOR P1 := 0 TO High(Points) DO BEGIN
        IF NOT TempPointArray[P1] THEN BEGIN
          DrawPointNum(P1, Colour);
          TempPointArray[P1] := True;
          UnitNum := Points[P1].Point_LenzUnit;
        END;

        FOR P2 := 0 TO High(Points) DO BEGIN
          IF NOT TempPointArray[P2] THEN BEGIN
            IF Points[P2].Point_LenzUnit = UnitNum THEN BEGIN
              DrawPointNum(P2, Colour);
              TempPointArray[P2] := True;
            END;
          END;
        END; {FOR}

        Inc(ColourNum);
        IF ColourNum > MaxColourNum THEN
          ColourNum := 1;
        Colour := ColoursArray[ColourNum];
      END; {FOR}

  //      IF Points[P].Point_LenzUnit = UnitNum THEN BEGIN
  //
  //          Inc(ColourNum);
  //          IF ColourNum > MaxColourNum THEN
  //            ColourNum := 1;
  //          Colour := ColoursArray[ColourNum];
  //
  //      END;
  //
  //      DrawPointNum(P, Colour);
    END; {WITH}

    ShowLenzPointNumbers := False;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawLenzPointUnitGroups:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawLenzPointUnitGroups }

PROCEDURE DrawAllPoints;
{ Draw all the points }
VAR
  P : Integer;

BEGIN
  TRY
    { Now draw them all }
    FOR P := 0 TO High(Points) DO BEGIN
      DrawPoint(P, PointColour);
      IF ShowPointDetail AND Points[P].Point_OutOfUse THEN
        DrawPointNum(P, PointOutOfUseColour)
      ELSE
        IF ShowPointDetail AND (Points[P].Point_FacingDirection = Up) THEN
          DrawPointNum(P, PointUpFacingColour)
        ELSE
          IF ShowPointDetail AND (Points[P].Point_FacingDirection = Down) THEN
            DrawPointNum(P, PointDownFacingColour)
          ELSE
            IF ShowLenzPointNumbers THEN
              DrawPointNum(P, PointLenzNumberColour)
            ELSE
              IF ShowPointType THEN
                DrawPointNum(P, PointLenzNumberColour)
              ELSE
                IF ShowPointFeedbackDataInSeparateColours THEN
                  DrawPointFeedbackDataInSeparateColours
                ELSE
                  IF ShowPointFeedbackDataInUse THEN
                    DrawPointNum(P, PointLenzNumberColour)
                  ELSE
                    IF ShowLenzPointUnitGroups THEN
                      DrawLenzPointUnitGroups
                    ELSE
                      IF ShowPointsThatAreLocked THEN
                        DrawPointNum(P, PointLockedBySystemColour)
                      ELSE
                        IF ShowPointDefaultState AND (Points[P].Point_DefaultState <> PointStateUnknown) THEN
                          { only draw the point number if the point has a default state }
                          DrawPointNum(P, PointDefaultStateColour);

       { Draw a rectangle around any point highlighted by the input procedure }
       IF PointHighlighted <> UnknownPoint THEN
        WITH Points[PointHighlighted] DO
          DrawOutline(Point_MouseRect, clWhite, NOT UndrawRequired, NOT UndrawToBeAutomatic);

      { Draw mouse access rectangles }
      IF ShowMouseRectangles THEN
        DrawOutline(Points[P].Point_MouseRect, clWhite, NOT UndrawRequired, NOT UndrawToBeAutomatic);
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawAllPoints:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawAllPoints }

PROCEDURE DrawPlatforms;
{ Draw the platforms }

  PROCEDURE DrawPlatformNumber(P : Integer; ActualPlatformNumberStr : String; Where : PlatformNumberPositionType);
  { Draw the number in the required place }
  VAR
    PlatformNumberX : Integer;
    PlatformNumberY : Integer;

  BEGIN
    TRY
      WITH RailWindowBitmap.Canvas DO BEGIN
        WITH Platforms[P].Platform_Rect DO BEGIN
          Font.Height := -MulDiv(FWPRailWindow.ClientHeight, PlatformNumberFontHeight, ZoomScaleFactor);
          Font.Color := PlatformNumberColour;
          PlatformNumberX := 0;
          PlatformNumberY := 0;

          CASE Where OF
            LeftTop:
              BEGIN
                PlatformNumberX := Left + PlatformNumberEdgeHorizontalSpacingScaled;
                PlatformNumberY := Top + PlatformNumberEdgeVerticalSpacingScaled;
              END;
            RightTop:
              BEGIN
                PlatformNumberX := Right - PlatformNumberEdgeHorizontalSpacingScaled;
                PlatformNumberY := Top + PlatformNumberEdgeVerticalSpacingScaled;
              END;
            CentreTop:
              BEGIN
                PlatformNumberX := Left + ((Right - Left) DIV 2);
                PlatformNumberY := Top + PlatformNumberEdgeVerticalSpacingScaled;
              END;
            LeftBottom:
              BEGIN
                PlatformNumberX := Left + PlatformNumberEdgeHorizontalSpacingScaled;
                PlatformNumberY := Bottom - PlatformNumberEdgeVerticalSpacingScaled - TextHeight(ActualPlatformNumberStr);
              END;
            RightBottom:
              BEGIN
                PlatformNumberX := Right - PlatformNumberEdgeHorizontalSpacingScaled;
                PlatformNumberY := Bottom - PlatformNumberEdgeVerticalSpacingScaled - TextHeight(ActualPlatformNumberStr);
              END;
            CentreBottom:
              BEGIN
                PlatformNumberX := Left + ((Right - Left) DIV 2);
                PlatformNumberY := Bottom - PlatformNumberEdgeVerticalSpacingScaled - TextHeight(ActualPlatformNumberStr);
              END;
          END; {CASE}

          TextOut(PlatformNumberX - ScrollBarXAdjustment, PlatformNumberY - ScrollBarYAdjustment, ActualPlatformNumberStr);
        END; {WITH}
      END; {WITH}
    EXCEPT
      ON E : Exception DO
        Log('EG DrawPlatformNumber:' + E.ClassName + ' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { DrawPlatformNumber }

VAR
  P : Integer;

BEGIN
  TRY
    InitialiseScreenDrawingVariables;
    WITH RailWindowBitmap.Canvas DO BEGIN
      { draw the platforms themselves }
      Brush.Color := PlatformColour;
      Brush.Style := bsSolid;

      FOR P := 0 TO High(Platforms) DO BEGIN
        WITH Platforms[P] DO BEGIN
          WITH Platform_Rect DO
            FillRect(Rect(Left - ScrollBarXAdjustment, Top - ScrollBarYAdjustment,
                          Right - ScrollBarXAdjustment, Bottom - ScrollBarYAdjustment));

          DrawPlatformNumber(P, Platform_NumberAStr, Platform_NumberPositionA);
          IF Platform_NumberAStr <> '' THEN
            DrawPlatformNumber(P, Platform_NumberBStr, Platform_NumberPositionB);
        END; {WITH}
      END; {FOR}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawPlatforms:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawPlatforms }

PROCEDURE DrawTRSPlunger(Location : Integer; Pressed : Boolean);
{ Indicate on a platform that a train-ready-to-start plunger has been pressed }
VAR
  X, Y : Integer;

BEGIN
  TRY
    InitialiseScreenDrawingVariables;

    WITH RailWindowBitmap.Canvas DO BEGIN
      WITH MainPlatformPlungers[Location] DO BEGIN
        X := TRSPlunger_Triangle.PlungerXScaled;
        Y := TRSPlunger_Triangle.PlungerYScaled;

        Pen.Color := TRSPlungerOutlineColour;
        { Draw the outline of the triangle... }
        IF TRSPlunger_Direction = Up THEN
          PolyLine([Point(X, Y),
                    Point(X, Y + TRSPlungerLength),
                    Point(X - TRSPlungerLength, Y + (TRSPlungerLength DIV 2))])
        ELSE
          PolyLine([Point(X, Y),
                    Point(X, Y + TRSPlungerLength),
                    Point(X + TRSPlungerLength, Y + (TRSPlungerLength DIV 2)),
                    Point(X, Y)]);

        IF Pressed THEN
          Brush.Color := TRSPlungerPressedColour
        ELSE
          Brush.Color := TRSPlungerColour;

        IF TRSPlunger_Direction = Up THEN
          { Fill the triangle facing the Up direction }
          Polygon([Point(X, Y),
                   Point(X, Y + TRSPlungerLength),
                   Point(X - TRSPlungerLength, Y + (TRSPlungerLength DIV 2))])
        ELSE
          { Fill the triangle facing the Down direction }
          Polygon([Point(X, Y),
                   Point(X, Y + TRSPlungerLength),
                   Point(X + TRSPlungerLength, Y + (TRSPlungerLength DIV 2))]);

        IF ShowMouseRectangles THEN
          DrawOutline(TRSPlunger_MouseRect, clYellow, NOT UndrawRequired, NOT UndrawToBeAutomatic);
      END; {WITH}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawTRSPlunger:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawTRSPlunger }

PROCEDURE TFWPRailWindow.FWPRailWindowPaint(Sender: TObject);
{ Notes on methods of screen drawing taken from http://stackoverflow.com/questions/1251009/whats-the-difference-between-refresh-update-repaint 4/8/14

  Refresh - Repaints the control on the screen.

  Call Refresh method to repaint the control immediately. Refresh calls the Repaint method. Use the Refresh and Repaint methods interchangeably.

  Repaint - Forces the control to repaint its image on the screen.

  Call Repaint to force the control to repaint its image immediately. If the ControlStyle property includes csOpaque, the control paints itself directly. Otherwise, the
  Repaint method calls the Invalidate method and then the Update method so that any visible portions of controls beneath the control will be repainted as well.

  Update - Processes any pending paint messages immediately.

  Call Update to force the control to be repainted before any more, possibly time-consuming, processing takes place. Use Update to provide immediate feedback to the user
  that cannot wait for the Windows paint message to arrive.

  Update does not invalidate the control, but simply forces a repaint of any regions that have already been invalidated. Call Repaint instead to invalidate the control
  as well.
}
BEGIN
  IF MainWindow.MainTimer.Enabled THEN
    DrawMap;

  { And copy the the bitmap image to the screen }
  Canvas.Draw(0, 0, RailWindowBitmap);

  IF IsSplashFormVisible THEN BEGIN
    HideSplashForm;
    FWPRailWindow.Show;
  END;
END; { FWPRailWindowPaint }

PROCEDURE TFWPRailWindow.FWPRailWindowShortCut(VAR Msg: TWMKey; VAR Handled: Boolean);
{ Replaced by RailApplicationEventsShortcut 16/6/14 }
//VAR
//  ShiftState : TShiftState;

BEGIN
//  TRY
//    ShiftState := [];
//    IF GetKeyState(vk_Shift) < 0 THEN
//      ShiftState := [ssShift];
//    IF GetKeyState(vk_Control) < 0 THEN
//      ShiftState := ShiftState + [ssCtrl];
//    IF GetKeyState(vk_Menu) < 0 THEN
//      ShiftState := ShiftState + [ssAlt];
//
//    CASE Msg.CharCode OF
//      vk_Tab:
//        BEGIN
//          Handled := True;
//          KeyPressedDown(msg.Charcode, ShiftState);
//        END;
//      vk_F4:
//        BEGIN
//          KeyPressedDown(Msg.Charcode, ShiftState);
//          Handled := True;
//        END;
//      vk_F10:
//        BEGIN
//          KeyPressedDown(Msg.Charcode, ShiftState);
//          Handled := True;
//        END;
//    END; {CASE}
//  EXCEPT
//    ON E : Exception DO
//      Log('EG FWPRailWindowShortCut:' + E.ClassName + ' error raised, with message: '+ E.Message);
//  END; {TRY}
END; { FWPRailWindowShortCut }

//  PROCEDURE TFWPRailWindow.ApplicationMessage(VAR Msg: TMsg; VAR Handled: Boolean);
//  { Intercept messages - only way of getting at the tab key! Now replaced by ShortCut above Sept 2009 }
//  VAR
//    Button: TUDBtnType;
//    OK : Boolean;
//    ApplicationMessageShiftState : TShiftState = [];
//
//BEGIN
//  CASE Msg.Message OF
//    WM_KEYDOWN:
//      { The WM_KEYDOWN message is posted to the window with the keyboard focus when a non-system key is pressed. A non-system key is a key that is pressed when the Alt
//        key is not pressed. Used by FWP to capture any key apart from the Alt key and the F10 key.
//      }
//      CASE Msg.wParam OF
//        vk_F12:
//          ;
//        vk_Up: { up arrow key - need to handle specially in loco dialogue boxes }
//          IF LocoDialogueWindow.Visible AND LocoDialogueWindow.LocoDialogueSpeedButtons.Enabled THEN BEGIN
//            IF ssShift IN ApplicationMessageShiftState THEN BEGIN
//              IF LocoDialogueWindow.LocoDialogueUpDownButton.Enabled THEN
//                LocoDialogueWindow.LocoDialogueUpDownButtonClick(Self);
//            END ELSE BEGIN
//              Button := btNext;
//              WITH LocoDialogueWindow.LocoDialogueSpeedButtons DO BEGIN
//                IF Position < Max THEN
//                  Position := Position + 1;
//              END;
//              LocoDialogueWindow.LocoDialogueSpeedButtonsClick(Self, Button);
//            END;
//            ApplicationMessageShiftState := [];
//            Handled := True;
//          END;
//        vk_Down: { down arrow key - need to handle specially in loco dialogue boxes }
//          IF LocoDialogueWindow.Visible AND LocoDialogueWindow.LocoDialogueSpeedButtons.Enabled THEN BEGIN
//            Button := btPrev;
//            IF ssShift IN ApplicationMessageShiftState THEN BEGIN
//              IF LocoDialogueWindow.LocoDialogueUpDownButton.Enabled THEN
//                LocoDialogueWindow.LocoDialogueUpDownButtonClick(Self);
//            END ELSE BEGIN
//              WITH LocoDialogueWindow.LocoDialogueSpeedButtons DO BEGIN
//                IF Position > Min THEN
//                  Position := Position - 1;
//              END;
//              LocoDialogueWindow.LocoDialogueSpeedButtonsClick(Self, Button);
//            END;
//            ApplicationMessageShiftState := [];
//            Handled := True;
//          END;
//        vk_Space: { space bar - need to handle specially in loco dialogue boxes }
//          debug;
//          IF LocoDialogueWindow.Visible THEN BEGIN
//            IF ssShift IN ApplicationMessageShiftState THEN BEGIN
//              IF MessageDialogueWithDefault('Resume operations?', NOT StopTimer, mtConfirmation, [mbOK, mbAbort],
//                                            ['&Resume', '&Don''t resume'], mbAbort) = mrOK
//              THEN BEGIN
//                ResumeOperations(OK);
//                IF OK THEN
//                  Log('AG Operations resumed')
//                ELSE
//                  Log('AG Operations not resumed');
//                InvalidateScreen(UnitRef, 'ApplicationMessage');
//                ApplicationMessageShiftState := [];
//                Handled := True;
//              END;
//            END ELSE BEGIN
//              StopOperations;
//              Log('AG ' + DescribeKey(Msg.wParam, ApplicationMessageShiftState) + ': all operations stopped');
//              Handled := True;
//            END;
//          END;
//        vk_Shift: { shift key }
//          ApplicationMessageShiftState := [ssShift];
//        vk_Tab: { tab key }
//          BEGIN
//            { pass the shift state on, but cancel it immediately afterwards, as otherwise, for some peculiar reason, it stays on }
//            KeyPressedDown(vk_Tab, ApplicationMessageShiftState);
//            ApplicationMessageShiftState := [];
//            { set Handled to false in case a form wants to process the tab instead }
//            Handled := False;
//          END;
//      END; {CASE}
//    WM_SYSKEYDOWN:
//      { The WM_SYSKEYDOWN message is posted to the window with the keyboard focus when the user presses the F10 key (which activates the menu bar) or holds down the
//         ALT key and then presses another key. It also occurs when no window currently has the keyboard focus; in this case, the WM_SYSKEYDOWN message is sent to the
//         active window. The window that receives the message can distinguish between these two contexts by checking the context code in the lParam parameter. Used by
//         FWP to capture the Alt key and F10.
//      }
//      { This intercepts Alt F4, which by default closes the program }
//      IF Msg.wParam = vk_F4 THEN BEGIN
//        { pass the shift state on, but cancel it immediately afterwards, as otherwise, for some peculiar reason, it stays on }
//        ApplicationMessageShiftState := ApplicationMessageShiftState + [ssAlt];
//        KeyPressedDown(Msg.wParam, ApplicationMessageShiftState);
//        ApplicationMessageShiftState := [];
//        Handled := True;
//      END ELSE
//        { This intercepts F10, which by default activates the menu bar as Alt does }
//        IF Msg.wParam = vk_F10 THEN BEGIN
//          { pass the shift state on, but cancel it immediately afterwards, as otherwise, for some peculiar reason, it stays on }
//          KeyPressedDown(Msg.wParam, ApplicationMessageShiftState);
//          ApplicationMessageShiftState := [];
//          Handled := True;
//        END ELSE
//          IF NOT MainDisplayMenu.Visible THEN
//            { This intercepts certain keystrokes involving Alt, and stops a system beep. Don't know why it's needed. }
//            CASE Msg.wParam OF
//              Ord('A')..Ord('Z'), Ord(13):
//                BEGIN
//                  ApplicationMessageShiftState := ApplicationMessageShiftState + [ssAlt];
//                  KeyPressedDown(Msg.wParam, ApplicationMessageShiftState);
//                  ApplicationMessageShiftState := [];
//                  Handled := True;
//                END;
//            END; {CASE}
//    WM_KEYUP:
//      { The WM_KEYUP message is posted to the window with the keyboard focus when a non-system key is released. A non-system key is a key that is pressed when the ALT key
//        is not pressed, or a keyboard key that is pressed when a window has the keyboard focus.
//      };
//    WM_ACTIVATE:
//      { The WM_ACTIVATE message is sent to both the window being activated and the window being deactivated. If the windows use the same input queue, the message is sent
//        synchronously, first to the window procedure of the top-level window being deactivated, then to the window procedure of the top-level window being activated. If
//        the windows use different input queues, the message is sent asynchronously, so the window is activated immediately.
//      };
//    WM_CHAR:
//      { The WM_CHAR message is posted to the window with the keyboard focus when a WM_KEYDOWN message is translated by the TranslateMessage function. The WM_CHAR message
//        contains the character code of the key that was pressed.
//      }
//      // Debug
//      ;
//    WM_DEADCHAR:
//      { The WM_DEADCHAR message is posted to the window with the keyboard focus when a WM_KEYUP message is translated by the TranslateMessage function. WM_DEADCHAR
//        specifies a character code generated by a dead key. A dead key is a key that generates a character, such as the umlaut (double-dot), that is combined with another
//        character to form a composite character. For example, the umlaut-O character (√É∆í√Ü‚Äô√É‚Äö√¢‚Ç¨‚Äú) is generated by typing the dead key for the umlaut
//        character, and then typing the O key.
//      };
//    WM_HOTKEY:
//      { The WM_HOTKEY message is posted when the user presses a hot key registered by the RegisterHotKey function. The message is placed at the top of the message queue
//        associated with the thread that registered the hot key.
//      };
//    WM_KILLFOCUS:
//       { The WM_KILLFOCUS message is sent to a window immediately before it loses the keyboard focus. }
//       ;
//    WM_SETFOCUS:
//       { The WM_SETFOCUS message is sent to a window after it has gained the keyboard focus. }
//       ;
//    WM_SYSDEADCHAR:
//       { The WM_SYSDEADCHAR message is sent to the window with the keyboard focus when a WM_SYSKEYDOWN message is translated by the TranslateMessage function.
//         WM_SYSDEADCHAR specifies the character code of a system dead key √É∆í√¢‚Ç¨≈°√É‚Äö√¢‚Ç¨‚Äù that is, a dead key that is pressed while holding down the ALT key.
//       };
//    WM_SYSKEYUP:
//       { The WM_SYSKEYUP message is posted to the window with the keyboard focus when the user releases a key that was pressed while the ALT key was held down. It also
//         occurs when no window currently has the keyboard focus; in this case, the WM_SYSKEYUP message is sent to the active window. The window that receives the message
//         can distinguish between these two contexts by checking the context code in the lParam parameter. A window receives this message through its WindowProc function.
//       };
//  END; {CASE}
//END; { ApplicationMessage }

PROCEDURE TFWPRailWindow.FWPRailWindowMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
{ If the mouse moves into the main window, move the focus there, except from the Debug Window. (We can't use Activate as the main window remains activated until we click
  on the diagrams or debug windows).
}
BEGIN
  TRY
    IF NOT KeyboardAndMouseLocked AND (FWPRailWindow <> NIL) THEN BEGIN
      IF NOT FWPRailWindow.Active
      AND NOT (ClockWindow.Active OR ClockWindow.Visible)
      AND NOT (DebuggingOptionsWindow.Active OR DebuggingOptionsWindow.Visible)
      AND NOT (FeedbackWindow.Active OR FeedbackWindow.Visible)
      AND NOT (HelpWindow.Active OR HelpWindow.Visible)
      AND NOT (LocationDataWindow.Active OR LocationDataWindow.Visible)
      AND NOT (LockListWindow.Active OR LockListWindow.Visible)
      AND NOT (LocoUtilsWindow.Active OR LocoUtilsWindow.Visible)
      AND NOT (RailDriverWindow.Active OR RailDriverWindow.Visible)
      AND NOT (OptionsWindow.Active OR OptionsWindow.Visible)
      AND NOT (LoggingWindow.Active OR LoggingWindow.Visible)
      THEN
        FWPRailWindow.SetFocus;

      IF NOT ProgramStarting THEN
        WhatIsUnderMouse(X, Y, ShiftState);
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FWPRailWindowMouseMove:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FWPRailWindowMouseMove }

PROCEDURE TFWPRailWindow.FWPRailWindowMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  { WhatIsUnderMouse needs to be repeated, as for some reason MouseButtonMove doesn't capture new mouse positions when a popup menu is "popped up" }
  WhatIsUnderMouse(X, Y, ShiftState);
  MouseButtonPressed(Button, X, Y, ShiftState);
END; { FWPRailWindowMouseDown }

PROCEDURE TFWPRailWindow.FWPRailWindowMouseUp(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  MouseButtonReleased(Button, X, Y, ShiftState);
END; { FWPRailWindowMouseUp }

PROCEDURE TFWPRailWindow.FWPRailWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
CONST
  HelpRequired = True;

BEGIN
  TRY
    CASE Key OF
      { Exclude most non-alphanumeric keys }
      vk_Shift, vk_Control, vk_Menu { Alt }, vk_Pause, vk_SnapShot { PrtSc }, vk_LWin, vk_RWin, vk_Apps { Windows Applications }, vk_Numlock, vk_Scroll,
      vk_Cancel {Ctrl-Break}, vk_Capital { Caps Lock }:
        { do nothing };
    ELSE {CASE}
      IF (Key = vk_Escape) AND LockListWindow.Visible THEN
        LockListWindow.Hide
      ELSE
        IF (Key = vk_Return) AND LocoDialogueWindow.Visible AND NOT InAutoMode THEN
          { we don't want the clock to start if Enter is accidentally pressed }
          EmergencyStopInLocoDialogue
        ELSE
          KeyPressedDown(Key, ShiftState);
    END; {CASE}
  EXCEPT
    ON E : Exception DO
      Log('EG FWPRailWindowKeyDown:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FWPRailWindowKeyDown }

PROCEDURE TFWPRailWindow.FWPRailWindowResize(Sender: TObject);
{ This is called after a window is resized }
BEGIN
  TRY
    IF NOT ProgramStarting THEN BEGIN
      { Resize is called when we start up, so don't set ResizeMap then }
      ResizeMap := True;
      FWPRailWindow.FWPRailWindowStatusBar.Visible := True;

      IF (ScreenMode = DefaultWindowedScreenMode) OR (ScreenMode = CustomWindowedScreenMode) THEN BEGIN
        ScreenMode := CustomWindowedScreenMode;
        WriteToStatusBarPanel(StatusBarPanel2, 'Screen resized');
      END;
      InvalidateScreen(UnitRef, 'FWPRailWindowResize');
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FWPRailWindowResize:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FWPRailWindowResize }

PROCEDURE TFWPRailWindow.FWPRailWindowClose(Sender: TObject; VAR Action: TCloseAction);
BEGIN
  TRY
    Log('A Shutdown requested by user clicking on exit button or pressing Alt F4');
    IF MessageDlg('Close FWP''s Railway Program?', mtConfirmation, [mbYes, mbNo], 0) = mrYes THEN BEGIN
      Action := caFree;
      ShutDownProgram(UnitRef, 'FWPRailWindowClose');
    END ELSE BEGIN
      Log('A Shutdown request cancelled by user {BLANKLINEBEFORE}');
      Action := caNone;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FWPRailWindowClose:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FWPRailWindowClose }

PROCEDURE ChangeCursor(NewCursor : TCursor);
{ Change the shape of the cursor (from the Delphi Help system) }
BEGIN
  SaveCursor := Screen.Cursor;
  Screen.Cursor := NewCursor;
  Application.ProcessMessages;
END; { ChangeCursor }

PROCEDURE TFWPRailWindow.HelpMenuAboutClick(Sender: TObject);
BEGIN
  MessageDlg(ProgramTitle
             + CRLF
             + 'Version ' + GetVersionInfoAsString + ' Build ' + GetBuildInfoAsString
             + CRLF
             + CRLF
             + CopyrightStatementForDisplaying
             + CRLF
             + 'All Rights Reserved',
             mtInformation, [mbOK], 0);
END; { FWPRailWindowAboutClick }

PROCEDURE TFWPRailWindow.FWPRailWindowDragDrop(Sender, Source: TObject; X, Y: Integer);
BEGIN
  IF Source IS TImage THEN
    Debug('drag drop x=' + inttostr(X) + ' y=' + inttostr(y));
  Debug(TImage(Source).Name);
END; { FWPRailWindowDragDrop }

PROCEDURE TFWPRailWindow.FWPRailWindowDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; VAR Accept: Boolean);
BEGIN
  Accept := (Source IS TImage);
END; { FWPRailWindowDragOver }

PROCEDURE TFWPRailWindow.MainDropdownMenuDisplayDiagramsWindowClick(Sender: TObject);
BEGIN
  IF DiagramsWindow.Visible THEN BEGIN
    DiagramsWindow.Hide;
    MainDropdownMenuDisplayDiagramsWindow.Checked := False;
  END ELSE BEGIN
    DiagramsWindow.Show;
    MainDropdownMenuDisplayDiagramsWindow.Checked := True;
    DrawDiagrams(UnitRef, 'MainDisplayMenuDiagramsClick');
  END;
END; { MainDisplayMenuDiagramsClick }

PROCEDURE TFWPRailWindow.MainDropdownMenuDisplayWorkingTimetableWindowClick(Sender: TObject);
BEGIN
  IF WorkingTimetableWindow.Visible THEN BEGIN
    WorkingTimetableWindow.Hide;
    MainDropdownMenuDisplayWorkingTimetableWindow.Checked := False;
  END ELSE BEGIN
    WorkingTimetableWindow.Show;
    MainDropdownMenuDisplayWorkingTimetableWindow.Checked := True;
    DrawWorkingTimetable(UnitRef, 'MainDisplayMenuWorkingTimetableClick');
  END;
END; { MainDisplayMenuWorkingTimetableWindowClick }

(*
var
  NewItem: TMenuItem;
  I : Integer;
begin
  { first create the separator }
  NewItem := TMenuItem.Create(Self);
  NewItem.Caption := '-';
  { add the new item to the Windows menu }
  Windows.Add(NewItem);
  { now create and add a menu item for each form }
  for I := 0 to Screen.FormCount-1 do
  begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := Screen.Forms[I].Name;
    Windows.Add(NewItem);

  end;

end;
*)
PROCEDURE TFWPRailWindow.MainDropdownMenuDisplayShowDebugOutputWindowClick(Sender: TObject);
BEGIN
  IF DebugWindow.Visible THEN BEGIN
    DebugWindow.Hide;
    MainDropdownMenuDisplayShowDebugOutputWindow.Checked := False;
  END ELSE BEGIN
    DebugWindow.Show;
    MainDropdownMenuDisplayShowDebugOutputWindow.Checked := True;
  END;
END; { MainDisplayMenuDebugClick }

PROCEDURE TFWPRailWindow.LocoInfoMenuItemClick(Sender: TObject);
BEGIN
(*
  IF LocoInfoWindow.Visible THEN BEGIN
    LocoInfoWindow.Visible := False;
    LocoInfoMenuItem.Checked := False;
  END ELSE BEGIN
    LocoInfoWindow.Visible := True;
    LocoInfoMenuItem.Checked := True;
  END;
*)
END; { LocoInfoMenuItemClick }

PROCEDURE TFWPRailWindow.ShowStatusBarClick(Sender: TObject);
BEGIN
  IF FWPRailWindow.FWPRailWindowStatusBar.Visible THEN BEGIN
    FWPRailWindow.FWPRailWindowStatusBar.Hide;
    MainDropdownMenuDisplayShowStatusBar.Checked := False;
  END ELSE BEGIN
    FWPRailWindow.FWPRailWindowStatusBar.Show;
    MainDropdownMenuDisplayShowStatusBar.Checked := True;
  END;
END; { ShowStatusBarClick }

PROCEDURE TFWPRailWindow.MainDropdownMenuDisplayShowMainMenuClick(Sender: TObject);
{ Make menus visible if they're not and vice versa }
BEGIN
  IF MenusVisible THEN
    HideMenus
  ELSE
    ShowMenus;
END; { MainDropdownMenuDisplayShowMainMenuClick }

PROCEDURE TFWPRailWindow.MainDropdownMenuDisplayZoomScreenClick(Sender: TObject);
BEGIN
  MainDropdownMenuDisplayZoomScreen.Checked := NOT MainDropdownMenuDisplayZoomScreen.Checked;
  IF MainDropdownMenuDisplayZoomScreen.Checked THEN
    ScreenMode := FullScreenMode
  ELSE BEGIN
    ScreenMode := DefaultWindowedScreenMode;
    InvalidateScreen(UnitRef, 'MainDisplayMenuZoomClick');
  END;
END; { MainDropdownMenuDisplayZoomScreenClick }

PROCEDURE TFWPRailWindow.MainRunMenuResumeOperationsClick(Sender: TObject);
VAR
  OK : Boolean;

BEGIN
  ResumeOperations(OK);
  IF OK THEN
    Log('AG Operations resumed')
  ELSE
    Log('AG Operations not resumed');
  InvalidateScreen(UnitRef, 'MainRunMenuResumeOperationsClick');
END; { MainRunMenuResumeOperationsClick }

PROCEDURE TFWPRailWindow.MainDropdownMenuHelpRailHelpClick(Sender: TObject);
BEGIN
  Application.HelpCommand(HELP_FINDER, 0);
END; { MainHelpMenuRailHelpClick }

PROCEDURE InvalidateScreen(UnitRefParam, CallingStr : String);
{ Draw the screen by invalidating it }
BEGIN
  RedrawScreen := True;
  FWPRailWindow.Invalidate;
//  Log('XG Invalidate Screen - call ' + CallingStr + ' from Unit ' + UnitRefParam);
  RedrawScreen := False;
END; { InvalidateScreen }

PROCEDURE TFWPRailWindow.FlashTimerTick(Sender: TObject);
{ Do any necessary flashing of signals or other on-screen detail }
VAR
  Line : Integer;
  S : Integer;

BEGIN
  TRY
    IF NOT ProgramStarting THEN BEGIN
      { Deal with flashing signals }
      FOR S := 0 TO High(Signals) DO BEGIN
        IF NOT Signals[S].Signal_OutOfUse THEN BEGIN
          IF (Signals[S].Signal_Aspect = FlashingSingleYellowAspect)
          OR (Signals[S].Signal_Aspect = FlashingDoubleYellowAspect)
          THEN BEGIN
            Signals[S].Signal_LampIsOn := NOT Signals[S].Signal_LampIsOn;
            SetSignalFunction(UnknownLocoChipStr, S);
            InvalidateScreen(UnitRef, 'FlashTimerTick 1');
          END;
        END;
      END;

      { Also any lines set to flash }
      FOR Line := 0 TO High(Lines) DO BEGIN
        IF Lines[Line].Line_TC <> UnknownTrackCircuit THEN BEGIN
          IF DisplayFlashingTrackCircuits AND (TrackCircuits[Lines[Line].Line_TC].TC_Flashing) THEN BEGIN
            IF TrackCircuits[Lines[Line].Line_TC].TC_LitUp THEN
              TrackCircuits[Lines[Line].Line_TC].TC_LitUp := False
            ELSE
              TrackCircuits[Lines[Line].Line_TC].TC_LitUp := True;
            InvalidateScreen(UnitRef, 'FlashTimerTick 2');
          END;
        END;
      END; {FOR }
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FlashTimerTick:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FlashTimerTick }

PROCEDURE TFWPRailWindow.wmSize(VAR Msg: TWMSize);
{ The window will get a notification message, WM_SIZE, when it is minimized. It will also get this message on other occasions, you have to check the message parameters to
  figure out why you got it. This is an after-the-fact notification, so you cannot use to prevent the window from being minimized. You can restore it and minimize the
  application, however. (https://groups.google.com/forum/#!msg/borland.public.delphi.nativeapi/xnJJNfhrPrc/Exlqfmmpx5AJ)
}
BEGIN
  Inherited;

  CASE Msg.SizeType OF
    SIZE_MAXHIDE:
      Log('A Main Window Msg.SizeType=SIZE_MAXHIDE');
    SIZE_MINIMIZED:
      BEGIN
        Log('A Main Window Msg.SizeType=SIZE_MINIMIZED');
        WindowState := wsNormal;
        IF NOT IsIconic(Application.Handle) THEN
          Application.Minimize;
      END;
    SIZE_MAXSHOW:
      Log('A Main Window Msg.SizeType=SIZE_MAXSHOW');
    SIZE_MAXIMIZED:
      Log('A Main Window Msg.SizeType=SIZE_MAXIMIZED');
    SIZE_RESTORED:
      Log('A Main Window Msg.SizeType=SIZE_RESTORED');
  ELSE {CASE}
    Log('A Main Window Msg.SizeType is of unknown size type');
  END; {CASE}
END; { wmSize }

PROCEDURE TFWPRailWindow.FWPRailWindowDestroy(Sender: TObject);
BEGIN
  RailWindowBitmap.Free;
END; { FWPRailWindowDestroy }

PROCEDURE TFWPRailWindow.PopupTimerTick(Sender: TObject);
{ This is needed to prevent the click that causes the popup menu to pop up also then activating one of the items on the popped-up menu }
BEGIN
  Inc(PopupTimerCount);
END; { PopupTimerTick }

FUNCTION AddMainMenuItem(PopupMenu : TPopupMenu; Caption : String; MenuPopupType : MenuPopupTypes; Enabled, Visible, Checked : Boolean;
                         Click : TNotifyEvent) : TMenuItemExtended;
{ Add a dynamic main menu item }
BEGIN
  Result := TMenuItemExtended.Create(PopupMenu);
  Result.Caption := Caption;
  Result.Value := 'test string'; { not used }
  Result.Enabled := Enabled;
  Result.Visible := Visible;
  Result.Checked := Checked;
  Result.MenuPopupType := MenuPopupType;
  Result.PenStylePopupType := NoPenStylePopupType;
  Result.OnClick := Click;
  PopupMenu.Items.Add(Result) ;
END; { AddMainMenuItem }

FUNCTION AddSubMenuItem{1}(SubMenuItem : TMenuItemExtended; Caption : String; MenuPopupType : MenuPopupTypes; Enabled, Visible, Checked : Boolean;
                           PenStylePopupType : PenStylePopupTypes; Click : TNotifyEvent) : TMenuItemExtended; Overload;
{ Add a dynamic sub-menu item }
BEGIN
  Result := TMenuItemExtended.Create(SubMenuItem);
  Result.Caption := Caption;
  Result.Value := 'test string'; { not used }
  Result.Enabled := Enabled;
  Result.Visible := Visible;
  Result.Checked := Checked;
  Result.MenuPopupType := MenuPopupType;
  Result.PenStylePopupType := PenStylePopupType;
  Result.OnClick := Click;
  SubMenuItem.Add(Result) ;
END; { AddSubMenuItem-1 }

FUNCTION AddSubMenuItem{2}(SubMenuItem : TMenuItemExtended; Caption : String; PopupType : MenuPopupTypes; Enabled, Visible, Checked : Boolean;
                           Click : TNotifyEvent) : TMenuItemExtended; Overload;
{ Add a dynamic sub-menu item }
BEGIN
  Result := AddSubMenuItem(SubMenuItem, Caption, PopupType, Enabled, Visible, Checked, NoPenStylePopupType, Click);
END; { AddSubMenuItem-2 }

PROCEDURE TFWPRailWindow.GeneralPopupItemClick(Sender: TObject);

  PROCEDURE ChangeColour(VAR Colour : TColor);
  BEGIN
    { Show the default }
    FWPRailWindowColourDialogue.Color := Colour;
    { Allow the user to change it }
    IF FWPRailWindowColourDialogue.Execute THEN BEGIN
      Colour := FWPRailWindowColourDialogue.Color;
      InvalidateScreen(UnitRef, 'GeneralPopupItemClick Change ' + ColourToStr(Colour) + 'PopupType');
    END;
  END; { ChangeColour }

  PROCEDURE RestoreColour(VAR Colour : TColor; DefaultColour : TColor);
  BEGIN
    Colour := DefaultColour;
    InvalidateScreen(UnitRef, 'GeneralPopupItemClick Restore' + ColourToStr(Colour) + 'PopupType');
  END; { RestoreColour }

BEGIN
  { Wait before popping up to avoid the same click that activates the popup menu also providing the menu item click }
  IF PopupTimerCount < 1 THEN BEGIN
    Debug('!*Delay required before selecting menu entry');
    PopupTimer.Enabled := False;
    Exit;
  END;

  WITH Sender AS TMenuItemExtended DO BEGIN
    CASE MenuPopupType OF
      ShowMainMenusPopupType:
        IF NOT MenusVisible THEN
          ShowMenus
        ELSE
          HideMenus;

      StartClockPopupType:
        TurnAutoModeOn;

      StopClockPopupType:
        TurnAutoModeOff(ByUser);

      RunClockNormallyPopupType:
        SetRailwayTimeInterval(Normal);

      RunClockSlowerPopupType:
        SetRailwayTimeInterval(Slower);

      RunClockFasterPopupType:
        SetRailwayTimeInterval(Faster);

      RunClockFastestPopupType:
        SetRailwayTimeInterval(Fastest);

      SetCurrentRailwayTimePopupType:
        NewSetCurrentRailwayTime;

      SetCurrentRailwayDayOfTheWeekPopupType:
        SetCurrentRailwayDayOfTheWeek;

      SetProgramStartTimePopupType:
        NewSetProgramStartTime;

      SetDaylightStartTimePopupType:
        NewSetDaylightStartTime;

      SetDaylightEndTimePopupType:
        NewSetDaylightEndTime;

      ChangeBackgroundColourPopupType:
        ChangeColour(BackgroundColour);

      RestoreBackgroundColourPopupType:
        RestoreColour(BackgroundColour, DefaultBackgroundColour);

      ChangeForegroundColourPopupType:
        ChangeColour(ForegroundColour);

      RestoreForegroundColourPopupType:
        RestoreColour(ForegroundColour, DefaultForegroundColour);

      ChangeBufferStopColourPopupType:
        ChangeColour(BufferStopColour);

      RestoreBufferStopColourPopupType:
        RestoreColour(BufferStopColour, DefaultBufferStopColour);

      ChangeBufferStopNumberColourPopupType:
        ChangeColour(BufferStopNumberColour);

      RestoreBufferStopNumberColourPopupType:
        RestoreColour(BufferStopNumberColour, DefaultBufferStopNumberColour);

      ChangeBufferStopRedPopupType:
        ChangeColour(BufferStopRed);

      RestoreBufferStopRedPopupType:
        RestoreColour(BufferStopRed, DefaultBufferStopRed);

      ChangeLineRoutedOverColourPopupType:
        ChangeColour(LineRoutedOverColour);

      RestoreLineRoutedOverColourPopupType:
        RestoreColour(LineRoutedOverColour, DefaultLineRoutedOverColour);

      ChangeLineNotAvailableColourPopupType:
        ChangeColour(LineNotAvailableColour);

      RestoreLineNotAvailableColourPopupType:
        RestoreColour(LineNotAvailableColour, DefaultLineNotAvailableColour);

      ChangeLocoStalledColourPopupType:
        ChangeColour(LocoStalledColour);

      RestoreLocoStalledColourPopupType:
        RestoreColour(LocoStalledColour, DefaultLocoStalledColour);

      ChangeTCFeedbackOccupationColourPopupType:
        ChangeColour(TCFeedbackOccupationColour);

      RestoreTCFeedbackOccupationColourPopupType:
        RestoreColour(TCFeedbackOccupationColour, DefaultTCFeedbackOccupationColour);

      ChangeTCFeedbackOccupationButOutOfUseColourPopupType:
        ChangeColour(TCFeedbackOccupationButOutOfUseColour);

      RestoreTCFeedbackOccupationButOutOfUseColourPopupType:
        RestoreColour(TCFeedbackOccupationButOutOfUseColour, DefaultTCFeedbackOccupationButOutOfUseColour);

      ChangeTCFeedbackDataInUseColourPopupType:
        ChangeColour(TCFeedbackDataInUseColour);

      RestoreTCFeedbackDataInUseColourPopupType:
        RestoreColour(TCFeedbackDataInUseColour, DefaultTCFeedbackDataInUseColour);

      ChangeTCFeedbackDataOutOfUseColourPopupType:
        ChangeColour(TCFeedbackDataOutOfUseColour);

      RestoreTCFeedbackDataOutOfUseColourPopupType:
        RestoreColour(TCFeedbackDataOutOfUseColour, DefaultTCFeedbackDataOutOfUseColour);

      ChangeTCMissingOccupationColourPopupType:
        ChangeColour(TCMissingOccupationColour);

      RestoreTCMissingOccupationColourPopupType:
        RestoreColour(TCMissingOccupationColour, DefaultTCMissingOccupationColour);

      ChangeTCPermanentFeedbackOccupationColourPopupType:
        ChangeColour(TCPermanentFeedbackOccupationColour);

      RestoreTCPermanentFeedbackOccupationColourPopupType:
        RestoreColour(TCPermanentFeedbackOccupationColour, DefaultTCPermanentFeedbackOccupationColour);

      ChangeTCPermanentOccupationSetByUserColourPopupType:
        ChangeColour(TCPermanentOccupationSetByUserColour);

      RestoreTCPermanentOccupationSetByUserColourPopupType:
        RestoreColour(TCPermanentOccupationSetByUserColour, DefaultTCPermanentOccupationSetByUserColour);

      ChangeTCPermanentSystemOccupationColourPopupType:
        ChangeColour(TCPermanentSystemOccupationColour);

      RestoreTCPermanentSystemOccupationColourPopupType:
        RestoreColour(TCPermanentSystemOccupationColour, DefaultTCPermanentSystemOccupationColour);

      ChangeTCSpeedRestrictionColourPopupType:
        ChangeColour(TCSpeedRestrictionColour);

      RestoreTCSpeedRestrictionColourPopupType:
        RestoreColour(TCSpeedRestrictionColour, DefaultTCSpeedRestrictionColour);

      ChangeTCSystemOccupationColourPopupType:
        ChangeColour(TCSystemOccupationColour);

      RestoreTCSystemOccupationColourPopupType:
        RestoreColour(TCSystemOccupationColour, DefaultTCSystemOccupationColour);

      ChangeTCOutOfUseSetByUserColourPopupType:
        ChangeColour(TCOutOfUseSetByUserColour);

      RestoreTCOutOfUseSetByUserColourPopupType:
        RestoreColour(TCOutOfUseSetByUserColour, DefaultTCOutOfUseSetByUserColour);

      ChangeTCOutOfUseAsNoFeedbackReceivedColourPopupType:
        ChangeColour(TCOutOfUseAsNoFeedbackReceivedColour);

      RestoreTCOutOfUseAsNoFeedbackReceivedColourPopupType:
        RestoreColour(TCOutOfUseAsNoFeedbackReceivedColour, DefaultTCOutOfUseAsNoFeedbackReceivedColour);

      ChangeTCUnoccupiedColourPopupType:
        ChangeColour(TCUnoccupiedColour);

      RestoreTCUnoccupiedColourPopupType:
        RestoreColour(TCUnoccupiedColour, DefaultTCUnoccupiedColour);

      ChangePlatformColourPopupType:
        ChangeColour(PlatformColour);

      RestorePlatformColourPopupType:
        RestoreColour(PlatformColour, DefaultPlatformColour);

      ChangeTRSPlungerColourPopupType:
        ChangeColour(TRSPlungerColour);

      RestoreTRSPlungerColourPopupType:
        RestoreColour(TRSPlungerColour, DefaultTRSPlungerColour);

      ChangeTRSPlungerPressedColourPopupType:
        ChangeColour(TRSPlungerPressedColour);

      RestoreTRSPlungerPressedColourPopupType:
        RestoreColour(TRSPlungerPressedColour, DefaultTRSPlungerPressedColour);

      ChangeTRSPlungerOutlineColourPopupType:
        ChangeColour(TRSPlungerOutlineColour);

      RestoreTRSPlungerOutlineColourPopupType:
        RestoreColour(TRSPlungerOutlineColour, DefaultTRSPlungerOutlineColour);

      ChangeDefaultPointColourPopupType:
        ChangeColour(PointColour);

      RestoreDefaultPointColourPopupType:
        RestoreColour(PointColour, DefaultPointColour);

      ChangePointDefaultStateColourPopupType:
        ChangeColour(PointDefaultStateColour);

      RestorePointDefaultStateColourPopupType:
        RestoreColour(PointDefaultStateColour, DefaultPointDefaultStateColour);

      ChangePointDivergingLineColourPopupType:
        ChangeColour(PointDivergingLineColour);

      RestorePointDivergingLineColourPopupType:
        RestoreColour(PointDivergingLineColour, DefaultPointDivergingLineColour);

      ChangePointDownFacingColourPopupType:
        ChangeColour(PointDownFacingColour);

      RestorePointDownFacingColourPopupType:
        RestoreColour(PointDownFacingColour, DefaultPointDownFacingColour);

      ChangePointFeedbackDataInUseColourPopupType:
        ChangeColour(PointFeedbackDataInUseColour);

      RestorePointFeedbackDataInUseColourPopupType:
        RestoreColour(PointFeedbackDataInUseColour, DefaultPointFeedbackDataInUseColour);

      ChangePointFeedbackDataOutOfUseColourPopupType:
        ChangeColour(PointFeedbackDataOutOfUseColour);

      RestorePointFeedbackDataOutOfUseColourPopupType:
        RestoreColour(PointFeedbackDataOutOfUseColour, DefaultPointFeedbackDataOutOfUseColour);

      ChangePointHeelLineColourPopupType:
        ChangeColour(PointHeelLineColour);

      RestorePointHeelLineColourPopupType:
        RestoreColour(PointHeelLineColour, DefaultPointHeelLineColour);

      ChangePointLenzNumberColourPopupType:
        ChangeColour(PointLenzNumberColour);

      RestorePointLenzNumberColourPopupType:
        RestoreColour(PointLenzNumberColour, DefaultPointLenzNumberColour);

      ChangePointLockedBySystemColourPopupType:
        ChangeColour(PointLockedBySystemColour);

      RestorePointLockedBySystemColourPopupType:
        RestoreColour(PointLockedBySystemColour, DefaultPointLockedBySystemColour);

      ChangePointLockedByUserColourPopupType:
        ChangeColour(PointLockedByUserColour);

      RestorePointLockedByUserColourPopupType:
        RestoreColour(PointLockedByUserColour, DefaultPointLockedByUserColour);

      ChangePointManualOperationColourPopupType:
        ChangeColour(PointManualOperationColour);

      RestorePointManualOperationColourPopupType:
        RestoreColour(PointManualOperationColour, DefaultPointManualOperationColour);

      ChangePointOutOfUseColourPopupType:
        ChangeColour(PointOutOfUseColour);

      RestorePointOutOfUseColourPopupType:
        RestoreColour(PointOutOfUseColour, DefaultPointOutOfUseColour);

      ChangePointStraightLineColourPopupType:
        ChangeColour(PointStraightLineColour);

      RestorePointStraightLineColourPopupType:
        RestoreColour(PointStraightLineColour, DefaultPointStraightLineColour);

      ChangePointUndrawColourPopupType:
        ChangeColour(PointUndrawColour);

      RestorePointUndrawColourPopupType:
        RestoreColour(PointUndrawColour, DefaultPointUndrawColour);

      ChangePointUpFacingColourPopupType:
        ChangeColour(PointUpFacingColour);

      RestorePointUpFacingColourPopupType:
        RestoreColour(PointUpFacingColour, DefaultPointUpFacingColour);

      ChangePointsWithoutFeedbackColourPopupType:
        ChangeColour(PointsWithoutFeedbackColour);

      RestorePointsWithoutFeedbackColourPopupType:
        RestoreColour(PointsWithoutFeedbackColour, DefaultPointsWithoutFeedbackColour);

      ChangeSignalAspectRedPopupType:
        ChangeColour(SignalAspectRed);

      RestoreSignalAspectRedPopupType:
        RestoreColour(SignalAspectRed, DefaultSignalAspectRed);

      ChangeSignalAspectGreenPopupType:
        ChangeColour(SignalAspectGreen);

      RestoreSignalAspectGreenPopupType:
        RestoreColour(SignalAspectGreen, DefaultSignalAspectGreen);

      ChangeSignalAspectYellowPopupType:
        ChangeColour(SignalAspectYellow);

      RestoreSignalAspectYellowPopupType:
        RestoreColour(SignalAspectYellow, DefaultSignalAspectYellow);

      ChangeSignalAspectUnlitPopupType:
        ChangeColour(SignalAspectUnlit);

      RestoreSignalAspectUnlitPopupType:
        RestoreColour(SignalAspectUnlit, DefaultSignalAspectUnlit);

      ChangeSignalNumberColourPopupType:
        ChangeColour(SignalNumberColour);

      RestoreSignalNumberColourPopupType:
        RestoreColour(SignalNumberColour, DefaultSignalNumberColour);

      ChangeSignalPostBaseColourPopupType:
        ChangeColour(SignalPostColour);

      RestoreSignalPostBaseColourPopupType:
        RestoreColour(SignalPostColour, DefaultSignalPostColour);

      ChangeSignalPostRouteSettingColourPopupType:
        ChangeColour(SignalPostRouteSettingColour);

      RestoreSignalPostRouteSettingColourPopupType:
        RestoreColour(SignalPostRouteSettingColour, DefaultSignalPostRouteSettingColour);

      ChangeSignalPostEmergencyRouteSettingColourPopupType:
        ChangeColour(SignalPostEmergencyRouteSettingColour);

      RestoreSignalPostEmergencyRouteSettingColourPopupType:
        RestoreColour(SignalPostEmergencyRouteSettingColour, DefaultSignalPostEmergencyRouteSettingColour);

      ChangeSignalPostTheatreSettingColourPopupType:
        ChangeColour(SignalPostTheatreSettingColour);

      RestoreSignalPostTheatreSettingColourPopupType:
        RestoreColour(SignalPostTheatreSettingColour, DefaultSignalPostTheatreSettingColour);

      ChangeSignalsFromWhichUserMustDriveSignalPostColourPopupType:
        ChangeColour(SignalsFromWhichUserMustDriveSignalPostColour);

      RestoreSignalsFromWhichUserMustDriveSignalPostColourPopupType:
        RestoreColour(SignalsFromWhichUserMustDriveSignalPostColour, DefaultSignalsFromWhichUserMustDriveSignalPostColour);

      ChangeTrainActiveColourPopupType:
        ChangeColour(TrainActiveColour);

      RestoreTrainActiveColourPopupType:
        RestoreColour(TrainActiveColour, DefaultTrainActiveColour);

      ChangeTrainInactiveColourPopupType:
        ChangeColour(TrainInactiveColour);

      RestoreTrainInactiveColourPopupType:
        RestoreColour(TrainInactiveColour, DefaultTrainInactiveColour);

      ChangeScreenComponentEditedColour1PopupType:
        ChangeColour(ScreenComponentEditedColour1);

      RestoreScreenComponentEditedColour1PopupType:
        RestoreColour(ScreenComponentEditedColour1, DefaultScreenComponentEditedColour1);

      ChangeScreenComponentEditedColour2PopupType:
        ChangeColour(ScreenComponentEditedColour2);

      RestoreScreenComponentEditedColour2PopupType:
        RestoreColour(ScreenComponentEditedColour2, DefaultScreenComponentEditedColour2);

      SolidPenStylePopupType:
        BEGIN
          CASE PenStylePopupType OF
            SidingPenStylePopupType:
              SidingPenStyle := psSolid;
            FiddleyardLinePenStylePopupType:
              FiddleyardLinePenStyle := psSolid;
            ProjectedLinePenStylePopupType:
              ProjectedLinePenStyle := psSolid;
            TCOutOfUseAsNoFeedbackReceivedPenStylePopupType:
              TCOutOfUseAsNoFeedbackReceivedPenStyle := psSolid;
            TCOutOfUseSetByUserPenStylePopupType:
              TCOutOfUseSetByUserPenStyle := psSolid;
            TCPermanentFeedbackOccupationPenStylePopupType:
              TCPermanentFeedbackOccupationPenStyle := psSolid;
            TCPermanentOccupationSetByUserPenStylePopupType:
              TCPermanentOccupationSetByUserPenStyle := psSolid;
            TCPermanentSystemOccupationPenStylePopupType:
              TCPermanentSystemOccupationPenStyle := psSolid;
            TCLocoOutOfPlacePenStylePopupType:
              TCLocoOutOfPlaceOccupationPenStyle := psSolid;
          END; {CASE}
          InvalidateScreen(UnitRef, 'GeneralPopupItemClick');
        END;

      DashPenStylePopupType:
        BEGIN
          CASE PenStylePopupType OF
            SidingPenStylePopupType:
              SidingPenStyle := psDash;
            FiddleyardLinePenStylePopupType:
              FiddleyardLinePenStyle := psDash;
            ProjectedLinePenStylePopupType:
              ProjectedLinePenStyle := psDash;
            TCOutOfUseAsNoFeedbackReceivedPenStylePopupType:
              TCOutOfUseAsNoFeedbackReceivedPenStyle := psDash;
            TCOutOfUseSetByUserPenStylePopupType:
              TCOutOfUseSetByUserPenStyle := psDash;
            TCPermanentFeedbackOccupationPenStylePopupType:
              TCPermanentFeedbackOccupationPenStyle := psDash;
            TCPermanentOccupationSetByUserPenStylePopupType:
              TCPermanentOccupationSetByUserPenStyle := psDash;
            TCPermanentSystemOccupationPenStylePopupType:
              TCPermanentSystemOccupationPenStyle := psDash;
            TCLocoOutOfPlacePenStylePopupType:
              TCLocoOutOfPlaceOccupationPenStyle := psDash;
          END; {CASE}
          InvalidateScreen(UnitRef, 'GeneralPopupItemClick');
        END;

      DotPenStylePopupType:
        BEGIN
          CASE PenStylePopupType OF
            SidingPenStylePopupType:
              SidingPenStyle := psDot;
            FiddleyardLinePenStylePopupType:
              FiddleyardLinePenStyle := psDot;
            ProjectedLinePenStylePopupType:
              ProjectedLinePenStyle := psDot;
            TCOutOfUseAsNoFeedbackReceivedPenStylePopupType:
              TCOutOfUseAsNoFeedbackReceivedPenStyle := psDot;
            TCOutOfUseSetByUserPenStylePopupType:
              TCOutOfUseSetByUserPenStyle := psDot;
            TCPermanentFeedbackOccupationPenStylePopupType:
              TCPermanentFeedbackOccupationPenStyle := psDot;
            TCPermanentOccupationSetByUserPenStylePopupType:
              TCPermanentOccupationSetByUserPenStyle := psDot;
            TCPermanentSystemOccupationPenStylePopupType:
              TCPermanentSystemOccupationPenStyle := psDot;
            TCLocoOutOfPlacePenStylePopupType:
              TCLocoOutOfPlaceOccupationPenStyle := psDot;
          END; {CASE}
          InvalidateScreen(UnitRef, 'GeneralPopupItemClick');
        END;

      DashDotPenStylePopupType:
        BEGIN
          CASE PenStylePopupType OF
            SidingPenStylePopupType:
              SidingPenStyle := psDashDot;
            FiddleyardLinePenStylePopupType:
              FiddleyardLinePenStyle := psDashDot;
            ProjectedLinePenStylePopupType:
              ProjectedLinePenStyle := psDashDot;
            TCOutOfUseAsNoFeedbackReceivedPenStylePopupType:
              TCOutOfUseAsNoFeedbackReceivedPenStyle := psDashDot;
            TCOutOfUseSetByUserPenStylePopupType:
              TCOutOfUseSetByUserPenStyle := psDashDot;
            TCPermanentFeedbackOccupationPenStylePopupType:
              TCPermanentFeedbackOccupationPenStyle := psDashDot;
            TCPermanentOccupationSetByUserPenStylePopupType:
              TCPermanentOccupationSetByUserPenStyle := psDashDot;
            TCPermanentSystemOccupationPenStylePopupType:
              TCPermanentSystemOccupationPenStyle := psDashDot;
            TCLocoOutOfPlacePenStylePopupType:
              TCLocoOutOfPlaceOccupationPenStyle := psDashDot;
          END; {CASE}
          InvalidateScreen(UnitRef, 'GeneralPopupItemClick');
        END;

      DashDotDotPenStylePopupType:
        BEGIN
          CASE PenStylePopupType OF
            SidingPenStylePopupType:
              SidingPenStyle := psDashDotDot;
            FiddleyardLinePenStylePopupType:
              FiddleyardLinePenStyle := psDashDotDot;
            ProjectedLinePenStylePopupType:
              ProjectedLinePenStyle := psDashDotDot;
            TCOutOfUseAsNoFeedbackReceivedPenStylePopupType:
              TCOutOfUseAsNoFeedbackReceivedPenStyle := psDashDotDot;
            TCOutOfUseSetByUserPenStylePopupType:
              TCOutOfUseSetByUserPenStyle := psDashDotDot;
            TCPermanentFeedbackOccupationPenStylePopupType:
              TCPermanentFeedbackOccupationPenStyle := psDashDotDot;
            TCPermanentOccupationSetByUserPenStylePopupType:
              TCPermanentOccupationSetByUserPenStyle := psDashDotDot;
            TCPermanentSystemOccupationPenStylePopupType:
              TCPermanentSystemOccupationPenStyle := psDashDotDot;
            TCLocoOutOfPlacePenStylePopupType:
              TCLocoOutOfPlaceOccupationPenStyle := psDashDotDot;
          END; {CASE}
          InvalidateScreen(UnitRef, 'GeneralPopupItemClick');
        END;

      ClearPenStylePopupType:
        BEGIN
          CASE PenStylePopupType OF
            SidingPenStylePopupType:
              SidingPenStyle := psClear;
            FiddleyardLinePenStylePopupType:
              FiddleyardLinePenStyle := psClear;
            ProjectedLinePenStylePopupType:
              ProjectedLinePenStyle := psClear;
            TCOutOfUseAsNoFeedbackReceivedPenStylePopupType:
              TCOutOfUseAsNoFeedbackReceivedPenStyle := psClear;
            TCOutOfUseSetByUserPenStylePopupType:
              TCOutOfUseSetByUserPenStyle := psClear;
            TCPermanentFeedbackOccupationPenStylePopupType:
              TCPermanentFeedbackOccupationPenStyle := psClear;
            TCPermanentOccupationSetByUserPenStylePopupType:
              TCPermanentOccupationSetByUserPenStyle := psClear;
            TCPermanentSystemOccupationPenStylePopupType:
              TCPermanentSystemOccupationPenStyle := psClear;
            TCLocoOutOfPlacePenStylePopupType:
              TCLocoOutOfPlaceOccupationPenStyle := psClear;
          END; {CASE}
          InvalidateScreen(UnitRef, 'GeneralPopupItemClick');
        END;

      InsideFramePenStylePopupType:
        BEGIN
          CASE PenStylePopupType OF
            SidingPenStylePopupType:
              SidingPenStyle := psInsideFrame;
            FiddleyardLinePenStylePopupType:
              FiddleyardLinePenStyle := psInsideFrame;
            ProjectedLinePenStylePopupType:
              ProjectedLinePenStyle := psInsideFrame;
            TCOutOfUseAsNoFeedbackReceivedPenStylePopupType:
              TCOutOfUseAsNoFeedbackReceivedPenStyle := psInsideFrame;
            TCOutOfUseSetByUserPenStylePopupType:
              TCOutOfUseSetByUserPenStyle := psInsideFrame;
            TCPermanentFeedbackOccupationPenStylePopupType:
              TCPermanentFeedbackOccupationPenStyle := psInsideFrame;
            TCPermanentOccupationSetByUserPenStylePopupType:
              TCPermanentOccupationSetByUserPenStyle := psInsideFrame;
            TCPermanentSystemOccupationPenStylePopupType:
              TCPermanentSystemOccupationPenStyle := psInsideFrame;
            TCLocoOutOfPlacePenStylePopupType:
              TCLocoOutOfPlaceOccupationPenStyle := psInsideFrame;
          END; {CASE}
          InvalidateScreen(UnitRef, 'GeneralPopupItemClick');
        END;

        ChangePointPopupType:
            SetAndShowInputDialogueBox(PointDialogueBox);

        ChangeSignalPopupType:
            SetAndShowInputDialogueBox(SignalDialogueBox);

        ListLocomotivesPopupType:
          IF LocoUtilsWindow.Visible THEN BEGIN
            LocoUtilsWindow.Visible := False;
            Log('A "?" key hides List of Locos');
          END ELSE BEGIN
            LocoUtilsWindow.Visible := True;
            Log('A "?" key makes List of Locos visible');
          END;

        ShowTrackCircuitPopupType:
          SetAndShowInputDialogueBox(TrackCircuitDialogueBox);

        DebugOptionsPopupType:
          Startup.DebuggingOptionsWindow.Show;

        ResetMainWindowSizeAndPositionPopupType:
          BEGIN
            ResetFWPRailWindowSizeAndPosition;
            InvalidateScreen(UnitRef, 'ResetFWPRailWindowSizeClick');
          END;

        ResetSizeAndPositionOfAllWindowsPopupType:
          ResetAllWindowsSizeAndPosition;

        RestoreAllScreenDrawingDefaultSettingsPopupType:
          IF MessageDialogueWithDefault('Do you wish to continue?', NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes THEN
            RestoreScreenDefaults;
    END; {CASE}
  END; {WITH}
END; { GeneralPopupItemClick }

PROCEDURE TFWPRailWindow.GeneralPopupMenuOnPopup(Sender: TObject);
{ This has been extracted into a subroutine as it is called multiple times }

  PROCEDURE AddPenStylesSubmenu(VAR SubSubMenuItems : TMenuItemExtended; PenStylePopupType : PenStylePopupTypes);
  BEGIN
    AddSubMenuItem(SubSubMenuItems, 'Solid', SolidPenStylePopupType, Enabled, Visible, NOT Checked, PenStylePopupType, GeneralPopupItemClick);
    AddSubMenuItem(SubSubMenuItems, 'Dash', DashPenStylePopupType, Enabled, Visible, NOT Checked, PenStylePopupType, GeneralPopupItemClick);
    AddSubMenuItem(SubSubMenuItems, 'Dot', DotPenStylePopupType, Enabled, Visible, NOT Checked, PenStylePopupType, GeneralPopupItemClick);
    AddSubMenuItem(SubSubMenuItems, 'Dash Dot', DashDotPenStylePopupType, Enabled, Visible, NOT Checked, PenStylePopupType, GeneralPopupItemClick);
    AddSubMenuItem(SubSubMenuItems, 'DashDotDot', DashDotDotPenStylePopupType, Enabled, Visible, NOT Checked, PenStylePopupType, GeneralPopupItemClick);
    AddSubMenuItem(SubSubMenuItems, 'Clear', ClearPenStylePopupType, Enabled, Visible, NOT Checked, PenStylePopupType, GeneralPopupItemClick);
    AddSubMenuItem(SubSubMenuItems, 'Inside Frame', InsideFramePenStylePopupType, Enabled, Visible, NOT Checked, PenStylePopupType, GeneralPopupItemClick);
  END; { AddPenStylesSubmenu }

VAR
  MainMenuItemExtended : TMenuItemExtended;
  SubMenuItems : TMenuItemExtended;
  SubSubMenuItems : TMenuItemExtended;
  WhetherChecked : Boolean;
  WhetherEnabled : Boolean;

BEGIN
  GeneralPopupMenu.Items.Clear;

  Caption := 'General Menu';
  AddMainMenuItem(GeneralPopupMenu, Caption, NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
  AddMainMenuItem(GeneralPopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

  IF NOT MenusVisible THEN
    AddMainMenuItem(GeneralPopupMenu, 'Show Main Menus', ShowMainMenusPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick)
  ELSE
    AddMainMenuItem(GeneralPopupMenu, 'Hide Main Menus', ShowMainMenusPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

  MainMenuItemExtended := AddMainMenuItem(GeneralPopupMenu, 'Clock', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    WhetherEnabled := InAutoMode;
    AddSubMenuItem(MainMenuItemExtended, 'Start Clock', StartClockPopupType, NOT WhetherEnabled, Visible, NOT Checked, GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Stop Clock', StopClockPopupType, WhetherEnabled, Visible, NOT Checked, GeneralPopupItemClick);

    AddSubMenuItem(MainMenuItemExtended, '-', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    WhetherChecked := RailwayTimeInterval = Normal;
    AddSubMenuItem(MainMenuItemExtended, 'Run Clock Normally', RunClockNormallyPopupType, Enabled, Visible, WhetherChecked, GeneralPopupItemClick);
    WhetherChecked := RailwayTimeInterval = Slower;
    AddSubMenuItem(MainMenuItemExtended, 'Run Clock Slower', RunClockSlowerPopupType, Enabled, Visible, WhetherChecked, GeneralPopupItemClick);
    WhetherChecked := RailwayTimeInterval = Faster;
    AddSubMenuItem(MainMenuItemExtended, 'Run Clock Faster', RunClockFasterPopupType, Enabled, Visible, WhetherChecked, GeneralPopupItemClick);
    WhetherChecked := RailwayTimeInterval = Fastest;
    AddSubMenuItem(MainMenuItemExtended, 'Run Clock Fastest', RunClockFastestPopupType, Enabled, Visible, WhetherChecked, GeneralPopupItemClick);

    AddSubMenuItem(MainMenuItemExtended, '-', NoClickPopupType, Enabled, Visible, WhetherChecked, GeneralPopupItemClick);

    AddSubMenuItem(MainMenuItemExtended, 'Set Current Railway Time', SetCurrentRailwayTimePopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Set Current Railway Day Of The Week', SetCurrentRailwayDayOfTheWeekPopupType, Enabled, Visible, NOT Checked,
                   GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Set Program Start Time', SetProgramStartTimePopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Set Daylight Start Time', SetDaylightStartTimePopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Set Daylight End Time', SetDaylightEndTimePopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

  MainMenuItemExtended := AddMainMenuItem(GeneralPopupMenu, 'Change Colours', NoclickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Background Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      AddSubMenuItem(SubMenuItems, 'Change Background Colour', ChangeBackgroundColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      AddSubMenuItem(SubMenuItems, 'Restore Background Colour', RestoreBackgroundColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Foreground Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      AddSubMenuItem(SubMenuItems, 'Change Foreground Colour', ChangeForegroundColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      AddSubMenuItem(SubMenuItems, 'Restore Foreground Colour', RestoreForegroundColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    AddSubMenuItem(MainMenuItemExtended, '-', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Buffer Stop Colours', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Buffer Stop Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Buffer Stop Colour', ChangeBufferStopColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Buffer Stop Colour', RestoreBufferStopColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Buffer Stop Number Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Buffer Stop Number Colour', ChangeBufferStopNumberColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Buffer Stop Number Colour', RestoreBufferStopNumberColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Buffer Stop Red', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Buffer Stop Red', ChangeBufferStopRedPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Buffer Stop Red', RestoreBufferStopRedPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Line and Track Circuit Colours', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubMenuItems, 'N.B. Default Line Colour = TCUnoccupiedColour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubMenuItems, '-', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Line Routed Over Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Line Routed Over Colour', ChangeLineRoutedOverColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Line Routed Over Colour', RestoreLineRoutedOverColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Line Not Available Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Line Not Available Colour', ChangeLineNotAvailableColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Line Not Available Colour', RestoreLineNotAvailableColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Loco Stalled Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Loco Stalled Colour', ChangeLocoStalledColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Loco Stalled Colour', RestoreLocoStalledColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Feedback Occupation Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Feedback Occupation Colour', ChangeTCFeedbackOccupationColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Feedback Occupation Colour', RestoreTCFeedbackOccupationColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Feedback Occupation But Out Of Use Colour', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Feedback Occupation But Out Of Use Colour', ChangeTCFeedbackOccupationButOutOfUseColourPopupType, Enabled,
                       Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Feedback Occupation But Out Of Use Colour', RestoreTCFeedbackOccupationButOutOfUseColourPopupType, Enabled,
                       Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Feedback Data In Use Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Feedback Data In Use Colour', ChangeTCFeedbackDataInUseColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Feedback Data In Use Colour', RestoreTCFeedbackDataInUseColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Data Out Of Use Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Feedback Data Out Of Use Colour', ChangeTCFeedbackDataOutOfUseColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Feedback Data Out Of Use Colour', RestoreTCFeedbackDataOutOfUseColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Missing Occupation Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Missing Occupation Colour', ChangeTCMissingOccupationColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Missing Occupation Colour', RestoreTCMissingOccupationColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Permanent Feedback Occupation Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Permanent Feedback Occupation Colour', ChangeTCPermanentFeedbackOccupationColourPopupType, Enabled, Visible,
                       NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Permanent Feedback Occupation Colour', RestoreTCPermanentFeedbackOccupationColourPopupType, Enabled, Visible,
                       NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Permanent Occupation Set By User Colour', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Permanent Occupation Set By User Colour', ChangeTCPermanentOccupationSetByUserColourPopupType, Enabled, Visible,
                       NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Permanent Occupation Set By User Colour', RestoreTCPermanentOccupationSetByUserColourPopupType, Enabled, Visible,
                       NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Permanent System Occupation Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Permanent System Occupation Colour', ChangeTCPermanentSystemOccupationColourPopupType, Enabled, Visible,
                       NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Permanent System Occupation Colour', RestoreTCPermanentSystemOccupationColourPopupType, Enabled, Visible,
                       NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Speed Restriction Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Speed Restriction Colour', ChangeTCSpeedRestrictionColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Speed Restriction Colour', RestoreTCSpeedRestrictionColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC System Occupation Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC System Occupation Colour', ChangeTCSystemOccupationColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC System Occupation Colour', RestoreTCSystemOccupationColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Out Of Use Set By User Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Out Of Use Set By User Colour', ChangeTCOutOfUseSetByUserColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Out Of Use Set By User Colour', RestoreTCOutOfUseSetByUserColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Out Of Use As No Feedback Received Colour', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Out Of Use As No Feedback Received Colour', ChangeTCOutOfUseAsNoFeedbackReceivedColourPopupType, Enabled, Visible,
                       NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Out Of Use As No Feedback Received Colour', RestoreTCOutOfUseAsNoFeedbackReceivedColourPopupType, Enabled,
                       Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TC Unoccupied Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TC Unoccupied Colour', ChangeTCUnoccupiedColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TC Unoccupied Colour', RestoreTCUnoccupiedColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Platform Colours', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Platform Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Platform Colour', ChangePlatformColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Platform Colour', RestorePlatformColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TRS Plunger Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TRS Plunger Colour', ChangeTRSPlungerColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TRS Plunger Colour', RestoreTRSPlungerColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TRS Plunger Pressed Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TRS Plunger Pressed Colour', ChangeTRSPlungerPressedColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TRS Plunger Pressed Colour', RestoreTRSPlungerPressedColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'TRS Plunger Outline Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change TRS Plunger Outline Colour', ChangeTRSPlungerOutlineColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore TRS Plunger Outline Colour', RestoreTRSPlungerOutlineColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Point Colours', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Default Point Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Default Point Colour', ChangeDefaultPointColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Default Point Colour', RestoreDefaultPointColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Default State Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Default State Colour', ChangePointDefaultStateColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Default State Colour', RestorePointDefaultStateColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Diverging Line Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Diverging Line Colour', ChangePointDivergingLineColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Diverging Line Colour', RestorePointDivergingLineColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Down Facing Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Down Facing Colour', ChangePointDownFacingColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Down Facing Colour', RestorePointDownFacingColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Feedback Data In Use Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Feedback Data In Use Colour', ChangePointFeedbackDataInUseColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Feedback Data In Use Colour', RestorePointFeedbackDataInUseColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Feedback Data Out Of Use Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Feedback Data Out Of Use Colour', ChangePointFeedbackDataOutOfUseColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Feedback Data Out Of Use Colour', RestorePointFeedbackDataOutOfUseColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Heel Line Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Heel Line Point Colour', ChangePointHeelLineColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Heel Line Point Colour', RestorePointHeelLineColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Lenz Number Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Lenz Number Colour', ChangePointLenzNumberColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Lenz Number Colour', RestorePointLenzNumberColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Locked By System Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Locked By System Colour', ChangePointLockedBySystemColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Locked By System Colour', RestorePointLockedBySystemColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Locked By User Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Locked By User Colour', ChangePointLockedByUserColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Locked By User Colour', RestorePointLockedByUserColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Manual Operation Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Manual Operation Colour', ChangePointManualOperationColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Manual Operation Colour', RestorePointManualOperationColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Out Of Use Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Out Of Use Colour', ChangePointOutOfUseColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Out Of Use Colour', RestorePointOutOfUseColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Straight Line Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Straight Line Colour', ChangePointStraightLineColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Straight Line Colour', RestorePointStraightLineColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Undraw Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Undraw Colour', ChangePointUndrawColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Undraw Colour', RestorePointUndrawColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Point Up Facing Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Point Up Facing Colour', ChangePointUpFacingColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Point Up Facing Colour', RestorePointUpFacingColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Points Without Feedback Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Points Without Feedback Colour', ChangePointsWithoutFeedbackColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Points Without Feedback Colour', RestorePointsWithoutFeedbackColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Signal Colours', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Signal Aspect Red', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Signal Aspect Red', ChangeSignalAspectRedPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Signal Aspect Red', RestoreSignalAspectRedPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Signal Aspect Green', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Signal Aspect Green', ChangeSignalAspectGreenPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Signal Aspect Green', RestoreSignalAspectGreenPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Signal Aspect Yellow', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Signal Aspect Yellow', ChangeSignalAspectYellowPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Signal Aspect Yellow', RestoreSignalAspectYellowPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Signal Aspect Unlit', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Signal Aspect Unlit', ChangeSignalAspectUnlitPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Signal Aspect Unlit', RestoreSignalAspectUnlitPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Signal Number Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Signal Number Colour', ChangeSignalNumberColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Signal Number Colour', RestoreSignalNumberColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Signal Post Base Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Signal Post Base Colour', ChangeSignalPostBaseColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Signal Post Base Colour', RestoreSignalPostBaseColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Signal Post Route Setting Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Signal Post Route Setting Colour', ChangeSignalPostRouteSettingColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Signal Post Route Setting Colour', RestoreSignalPostRouteSettingColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Signal Post Emergency Route Setting Colour', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Signal Post Emergency Route Setting Colour', ChangeSignalPostEmergencyRouteSettingColourPopupType, Enabled,
                       Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Signal Post Emergency Route Setting Colour', RestoreSignalPostEmergencyRouteSettingColourPopupType, Enabled,
                       Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Signal Post Theatre Setting Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Signal Post Theatre Setting Colour', ChangeSignalPostTheatreSettingColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Signal Post Theatre Setting Colour', RestoreSignalPostTheatreSettingColourPopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Signals From Which User Must Drive Signal Post Colour', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Signals From Which User Must Drive Signal Post Colour', ChangeSignalsFromWhichUserMustDriveSignalPostColourPopupType,
                       Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Signals From Which User Must Drive Signal Post Colour',
                       RestoreSignalsFromWhichUserMustDriveSignalPostColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Train Colours', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Train Active Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Train Active Colour', ChangeTrainActiveColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Train Active Colour', RestoreTrainActiveColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Train Inactive Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Train Inactive Colour', ChangeTrainInactiveColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Train Inactive Colour', RestoreTrainInactiveColourPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Editing Colours', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Screen Component Edited 1 Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Screen Component Edited 1 Colour', ChangeScreenComponentEditedColour1PopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Screen Component Edited 1 Colour', RestoreScreenComponentEditedColour1PopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Screen Component Edited 2 Colour', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Change Screen Component Edited 2 Colour', ChangeScreenComponentEditedColour2PopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);
        AddSubMenuItem(SubSubMenuItems, 'Restore Screen Component Edited 2 Colour', RestoreScreenComponentEditedColour2PopupType, Enabled, Visible, NOT Checked,
                       GeneralPopupItemClick);

    AddSubMenuItem(MainMenuItemExtended, '-', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Restore All Default Colours', RestoreAllDefaultColoursPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

  MainMenuItemExtended := AddMainMenuItem(GeneralPopupMenu, 'Change Pen Styles', NoclickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Siding Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Change Siding Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, SidingPenStylePopupType);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Restore Siding Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, SidingPenStylePopupType);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Signals From Which User Must Drive Signal Post Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                   GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Change Signals From Which User Must Drive Signal Post Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, SignalsFromWhichUserMustDriveSignalPostPenStylePopupType);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Restore Signals From Which User Must Drive Signal Post Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, SignalsFromWhichUserMustDriveSignalPostPenStylePopupType);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Fiddleyard-Line Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Change Fiddleyard-Line Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, FiddleyardLinePenStylePopupType);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Restore Fiddleyard-Line Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, FiddleyardLinePenStylePopupType);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'Projected-Line Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Change Projected-Line Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, ProjectedLinePenStylePopupType);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Restore Projected-Line Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, ProjectedLinePenStylePopupType);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'TC Out Of Use As No Feedback Received Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                   GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Change TC Out Of Use As No Feedback Received Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCOutOfUseAsNoFeedbackReceivedPenStylePopupType);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Restore TC Out Of Use As No Feedback Received Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCOutOfUseAsNoFeedbackReceivedPenStylePopupType);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'TC Out Of Use Set By User Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Change TC Out Of Use Set By User Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCOutOfUseSetByUserPenStylePopupType);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Restore TC Out Of Use Set By User Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCOutOfUseSetByUserPenStylePopupType);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'TC Permanent Feedback Occupation Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                   GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Change TC Permanent Feedback Occupation Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCPermanentFeedbackOccupationPenStylePopupType);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems,'Restore TC Permanent Feedback Occupation Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCPermanentFeedbackOccupationPenStylePopupType);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'TC Permanent Occupation Set By User Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                   GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Change TC Permanent Occupation Set By User Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCPermanentOccupationSetByUserPenStylePopupType);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Restore TC Permanent Occupation Set By User Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCPermanentOccupationSetByUserPenStylePopupType);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'TC Permanent System Occupation Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                   GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Change TC Permanent System Occupation Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCPermanentSystemOccupationPenStylePopupType);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Restore TC Permanent System Occupation Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked,
                                        GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCPermanentSystemOccupationPenStylePopupType);

    SubMenuItems := AddSubMenuItem(MainMenuItemExtended, 'TC Loco Out Of Place Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Change TC Loco Out Of Place Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCLocoOutOfPlacePenStylePopupType);
      SubSubMenuItems := AddSubMenuItem(SubMenuItems, 'Restore TC Loco Out Of Place Pen Style', NoClickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
        AddPenStylesSubMenu(SubSubMenuItems, TCLocoOutOfPlacePenStylePopupType);

  MainMenuItemExtended := AddMainMenuItem(GeneralPopupMenu, 'Operations', NoclickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    AddSubMenuItem(MainMenuItemExtended, 'Change Point', ChangePointPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Change Signal', ChangeSignalPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'List Locomotives', ListLocomotivesPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Show TrackCircuit', ShowTrackCircuitPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, '-', NoClickPopupType, Enabled, Visible, WhetherChecked, GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Debug Options', DebugOptionsPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

  MainMenuItemExtended := AddMainMenuItem(GeneralPopupMenu, 'Resetting Options', NoclickPopupType, Enabled, Visible, NOT Checked, GeneralPopupItemClick);

    WhetherEnabled := (FWPRailWindow.Top <> DefaultFWPRailWindowTop)
                       OR (FWPRailWindow.Height <> DefaultFWPRailWindowHeight)
                       OR (FWPRailWindow.Left <> DefaultFWPRailWindowLeft)
                       OR (FWPRailWindow.Top <> DefaultFWPRailWindowTop)
                       OR (FWPRailWindow.Width <> DefaultFWPRailWindowWidth);
    AddSubMenuItem(MainMenuItemExtended, 'Reset Main Window Size And Position', ResetMainWindowSizeAndPositionPopupType, WhetherEnabled, Visible, NOT Checked,
                   GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Reset Size And Position Of All Windows', ResetSizeAndPositionOfAllWindowsPopupType, Enabled, Visible, NOT Checked,
                   GeneralPopupItemClick);
    AddSubMenuItem(MainMenuItemExtended, 'Restore All Screen Drawing Default Settings', RestoreAllScreenDrawingDefaultSettingsPopupType, Enabled, Visible, NOT Checked,
                   GeneralPopupItemClick);

  PopupTimerCount := 0;
  PopupTimer.Enabled := True;
END; { GeneralPopupMenuPopup }

PROCEDURE TFWPRailWindow.SignalPopupItemClick(Sender: TObject);
CONST
  SaveVariables = True;

BEGIN
  { Wait before popping up to avoid the same click that activates the popup menu also providing the menu item click }
  IF PopupTimerCount < 1 THEN BEGIN
    Debug('!*Delay required before selecting menu entry');
    PopupTimer.Enabled := False;
    Exit;
  END;

  WITH Signals[SignalPopupNum] DO BEGIN
    WITH Sender AS TMenuItemExtended DO BEGIN
      CASE MenuPopupType OF
        SignalChangeDirectionPopupType:
          ChangeSignalDirection(SignalPopupNum);

        SignalDeletePopupType:
          DeleteSignal(SignalPopupNum);

        SignalEditPopupType:
          TurnEditModeOn(SignalPopupNum, UnknownPoint, UnknownBufferStop, UnknownLine, UnknownTrackCircuit);

        SignalOutOfUsePopupType:
          SwitchSignalOutOfUseState(SignalPopupNum);

        SignalUserMustDriveFromPopupType:
          SwitchSignalUserMustDriveFromState(SignalPopupNum);

        SignalUndoChangesPopupType:
          UndoEditChanges;
      ELSE {CASE}
        Log('BG Invalid popup type ' + IntToStr(Tag) + ' in SignalPopupItemClick');
      END; {CASE}
    END; {WITH}
  END; {WITH}
END; { SignalPopupItemClick }

PROCEDURE TFWPRailWindow.SignalPopupMenuOnPopup(Sender: TObject);
BEGIN
  WITH Signals[SignalPopupNum] DO BEGIN
    SignalPopupMenu.Items.Clear;

    IF NOT EditMode THEN BEGIN
      { Add the caption... }
      IF SignalPopupNum = UnknownSignal THEN
        Caption := 'Signals'
      ELSE
        Caption := 'Signal ' + IntToStr(SignalPopupNum);
      AddMainMenuItem(SignalPopupMenu, Caption, NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
      AddMainMenuItem(SignalPopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

      { ...and now the individual items }
      IF NOT Signal_OutOfUse THEN
        AddMainMenuItem(SignalPopupMenu, 'Set Signal ' + IntToStr(SignalPopupNum) + ' Out Of Use', SignalOutOfUsePopupType, Enabled, Visible, NOT Checked,
                        SignalPopupItemClick)
      ELSE
        AddMainMenuItem(SignalPopupMenu, 'Set Signal ' + IntToStr(SignalPopupNum) + ' Back In Use', SignalOutOfUsePopupType, Enabled, Visible, NOT Checked,
                        SignalPopupItemClick);

      IF NOT Signal_FromWhichUserMustDrive THEN
        AddMainMenuItem(SignalPopupMenu, 'Set Signal ' + IntToStr(SignalPopupNum) + ' "User Must Drive From Here"', SignalUserMustDriveFromPopupType, Enabled, Visible,
                        NOT Checked, SignalPopupItemClick)
      ELSE
        AddMainMenuItem(SignalPopupMenu, 'Cancel Signal ' + IntToStr(SignalPopupNum) + ' "User Must Drive From Here"', SignalUserMustDriveFromPopupType, Enabled, Visible,
                        NOT Checked, SignalPopupItemClick);

      AddMainMenuItem(SignalPopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);
      AddMainMenuItem(SignalPopupMenu, 'Edit Signal ' + IntToStr(SignalPopupNum) + ' Details', SignalEditPopupType, Enabled, Visible, NOT Checked, SignalPopupItemClick);
    END ELSE BEGIN
      { EditMode }

      { Add the caption... }
      IF SignalPopupNum = UnknownSignal THEN
        Caption := 'Editing Signals'
      ELSE
        Caption := 'Editing Signal ' + IntToStr(SignalPopupNum);
      AddMainMenuItem(SignalPopupMenu, Caption, NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
      AddMainMenuItem(SignalPopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

      { ...and now the individual items }
      IF NOT Signal_OutOfUse THEN
        AddMainMenuItem(SignalPopupMenu, 'Set Signal ' + IntToStr(SignalPopupNum) + ' Out Of Use', SignalOutOfUsePopupType, Enabled, Visible, NOT Checked,
                        SignalPopupItemClick)
      ELSE
        AddMainMenuItem(SignalPopupMenu, 'Set Signal ' + IntToStr(SignalPopupNum) + ' Back In Use', SignalOutOfUsePopupType, Enabled, Visible, NOT Checked,
                        SignalPopupItemClick);

      AddMainMenuItem(SignalPopupMenu, 'Change Signal ' + IntToStr(SignalPopupNum) + ' Direction', SignalChangeDirectionPopupType, Enabled, Visible, NOT Checked,
                      SignalPopupItemClick);

      IF Signal_PreviousLineX <> 0 THEN
        { the signal has been moved }
        AddMainMenuItem(SignalPopupMenu, 'Undo Changes', SignalUndoChangesPopupType, Enabled, Visible, NOT Checked, SignalPopupItemClick);

      AddMainMenuItem(SignalPopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

      AddMainMenuItem(SignalPopupMenu, 'Delete Signal ' + IntToStr(SignalPopupNum), SignalDeletePopupType, Enabled, Visible, NOT Checked, SignalPopupItemClick);
    END;
  END; {WITH}

  PopupTimerCount := 0;
  PopupTimer.Enabled := True;
END; { SignalPopupMenuOnPopup }

PROCEDURE TFWPRailWindow.PointPopupItemClick(Sender: TObject);
BEGIN
  { Wait before popping up to avoid the same click that activates the popup menu also providing the menu item click }
  IF PopupTimerCount < 1 THEN BEGIN
    Debug('!*Delay required before selecting menu entry');
    PopupTimer.Enabled := False;
    Exit;
  END;

  WITH Points[PointPopupNum] DO BEGIN
    WITH Sender AS TMenuItemExtended DO BEGIN
      CASE MenuPopupType OF
        PointOutOfUsePopupType:
          BEGIN
            IF NOT Point_OutOfUse THEN
              Point_OutOfUse := True
            ELSE
              Point_OutOfUse := False;
            InvalidateScreen(UnitRef, 'PointPopupItemClick PointOutOfUsePopupType');
          END;

        PointToManualPopupType:
          BEGIN
            IF NOT Point_ManualOperation THEN
              Point_ManualOperation := True
            ELSE
              Point_ManualOperation := False;
            Point_DataChanged := True;
            InvalidateScreen(UnitRef, 'PointPopupItemClick PointToManualPopupType');
          END;

        PointUnlockPopupType:
          BEGIN
            IF Point_LockedByUser THEN
              Point_LockedByUser := False
            ELSE
              Point_LockedByUser := True;
            Point_DataChanged := True;
            InvalidateScreen(UnitRef, 'PointPopupLockPointClick PointUnlockPopupType');
          END;

        PointDeletePopupType:
          DeletePoint(PointPopupNum);

        PointEditPopupType:
          TurnEditModeOn(UnknownSignal, PointPopupNum, UnknownBufferStop, UnknownLine, UnknownTrackCircuit);
      ELSE {CASE}
        Log('BG Invalid popup type ' + IntToStr(Tag) + ' in PointPopupItemClick');
      END; {CASE}
    END; {WITH}
  END; {WITH}
END; { PointPopupItemClick }

PROCEDURE TFWPRailWindow.PointPopupMenuOnPopup(Sender: TObject);
BEGIN
  WITH Points[PointPopupNum] DO BEGIN
    PointPopupMenu.Items.Clear;

    IF NOT EditMode THEN BEGIN
      { Add the caption... }
      Caption := 'Point ' + IntToStr(PointPopupNum);
      AddMainMenuItem(PointPopupMenu, Caption, NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
      AddMainMenuItem(PointPopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

      { ...and now the individual items }
      IF Point_OutOfUse THEN
        AddMainMenuItem(PointPopupMenu, 'Set Point ' + IntToStr(PointPopupNum) + ' Back In Use', PointOutOfUsePopupType, Enabled, Visible, NOT Checked, PointPopupItemClick)
      ELSE
        AddMainMenuItem(PointPopupMenu, 'Set Point ' + IntToStr(PointPopupNum) + ' Out Of Use', PointOutOfUsePopupType, Enabled, Visible, NOT Checked, PointPopupItemClick);

      IF Point_ManualOperation THEN
        AddMainMenuItem(PointPopupMenu, 'Set Point ' + IntToStr(PointPopupNum) + ' To Manual', PointToManualPopupType, Enabled, Visible, NOT Checked, PointPopupItemClick)
      ELSE
        AddMainMenuItem(PointPopupMenu, 'Set Point ' + IntToStr(PointPopupNum) + ' To Automatic', PointToManualPopupType, Enabled, Visible, NOT Checked, PointPopupItemClick);

      IF Point_LockedByUser THEN
        AddMainMenuItem(PointPopupMenu, 'Unlock Point ' + IntToStr(PointPopupNum), PointUnlockPopupType, Enabled, Visible, NOT Checked, PointPopupItemClick)
      ELSE
        AddMainMenuItem(PointPopupMenu, 'Lock Point ' + IntToStr(PointPopupNum), PointUnlockPopupType, Enabled, Visible, NOT Checked, PointPopupItemClick);

      AddMainMenuItem(PointPopupMenu, 'Edit Point ' + IntToStr(PointPopupNum), PointEditPopupType, Enabled, Visible, NOT Checked, PointPopupItemClick);
    END ELSE BEGIN
      { EditMode }

      { Add the caption... }
      Caption := 'Editing Point ' + IntToStr(PointPopupNum);
      AddMainMenuItem(PointPopupMenu, Caption, NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
      AddMainMenuItem(PointPopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

      { ...and now the individual items }
      AddMainMenuItem(PointPopupMenu, 'Delete Point ' + intToStr(PointPopupNum), PointDeletePopupType, Enabled, Visible, NOT Checked, PointPopupItemClick);
    END;
  END; {WITH}

  PopupTimerCount := 0;
  PopupTimer.Enabled := True;
END; { PointPopupMenuOnPopup }

PROCEDURE TFWPRailWindow.BufferStopPopupItemClick(Sender: TObject);
BEGIN
  { Wait before popping up to avoid the same click that activates the popup menu also providing the menu item click }
  IF PopupTimerCount < 1 THEN BEGIN
    Debug('!*Delay required before selecting menu entry');
    PopupTimer.Enabled := False;
    Exit;
  END;

  WITH BufferStops[BufferStopPopupNum] DO BEGIN
    WITH Sender AS TMenuItem DO BEGIN
      CASE Tag OF
        1:
          TurnEditModeOn(UnknownSignal, unknownPoint, BufferStopPopupNum, UnknownLine, UnknownTrackCircuit);
      ELSE {CASE}
        Log('BG Invalid tag ' + IntToStr(Tag) + ' in BufferStopPopupItemClick');
      END; {CASE}
    END; {WITH}
  END; {WITH}
END; { BufferStopPopupItemClick }

PROCEDURE TFWPRailWindow.BufferStopMenuOnPopup(Sender: TObject);
BEGIN
  WITH BufferStops[BufferStopPopupNum] DO BEGIN
    BufferStopPopupMenu.Items.Clear;

    IF NOT EditMode THEN BEGIN
      { Add the caption... }
      Caption := 'BufferStop ' + IntToStr(BufferStopPopupNum);
      AddMainMenuItem(BufferStopPopupMenu, Caption, NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
      AddMainMenuItem(BufferStopPopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

      { ...and now the individual items }
      AddMainMenuItem(BufferStopPopupMenu, 'Edit ' + IntToStr(BufferStopPopupNum) + '  BufferStop', BufferStopEditPopupType, Enabled, Visible, NOT Checked, BufferStopPopupItemClick)
    END ELSE BEGIN
      { EditMode }

      { Add the caption... }
      Caption := 'Editing BufferStop ' + IntToStr(BufferStopPopupNum);
      AddMainMenuItem(BufferStopPopupMenu, Caption, NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
      AddMainMenuItem(BufferStopPopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

      { ...and now the individual items }

    END;
  END; {WITH}

  PopupTimerCount := 0;
  PopupTimer.Enabled := True;
END; { BufferStopMenuOnPopup }

PROCEDURE TFWPRailWindow.MainDropDownMenuFileExitClick(Sender: TObject);
CONST
  ExitProgram = True;

BEGIN
  Log('A Shutdown requested by user selecting exit menu item');
  ShutDownProgram(UnitRef, 'FWPRailWindowExitClick');
END; { MainDropDownMenuFileExitClick }

PROCEDURE SetOrClearTrackCircuitSpeedRestriction(Line : Integer);
VAR
  DefaultDirectionStr : String;
  DefaultSpeedStr : String;
  Direction : DirectionType;
  DirectionStr : String;
  OK : Boolean;
  Speed : Integer;
  SpeedStr : String;

BEGIN
  TRY
    OK := False;
    DirectionStr := '';

    WITH Lines[Line] DO BEGIN
      IF Line_TC <> UnknownTrackCircuit THEN BEGIN
        WITH TrackCircuits[Line_TC] DO BEGIN
          IF TC_SpeedRestrictionInMPH <> NoSpecifiedSpeed THEN BEGIN
            IF MessageDialogueWithDefault('Do you wish to clear the ' + MPHToStr(TC_SpeedRestrictionInMPH) + ' mph speed restriction at TC=' + IntToStr(Line_TC) + '?',
                                          NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
            THEN BEGIN
              DefaultSpeedStr := MPHTOStr(TC_SpeedRestrictionInMPH);
              DirectionStr := DirectionToStr(TC_SpeedRestrictionDirection);
            END ELSE BEGIN
              TC_SpeedRestrictionInMPH := NoSpecifiedSpeed;
              TC_SpeedRestrictionDirection := UnknownDirection;
            END;
          END ELSE BEGIN
            DefaultSpeedStr := MPHTOStr(TC_SpeedRestrictionInMPH);
            DefaultDirectionStr := DirectionToStr(TC_SpeedRestrictionDirection);

            SpeedStr := InputBox('Speed Restriction at Track-Circuit Occupation', 'Maximum Speed?', DefaultSpeedStr);
            IF NOT TryStrToInt(SpeedStr, Speed) THEN
              ShowMessage('"' + SpeedStr + '" is not a valid speed')
            ELSE BEGIN
              CASE Speed OF
                10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120:
                  OK := True;
                ELSE
                  ShowMessage('"' + SpeedStr + '" is not a valid speed');
              END; {CASE}
            END;

            IF OK THEN BEGIN
              DirectionStr := InputBox('Direction of Speed Restriction', 'Restriction Direction (U[p], D[own] or B[idirectional])?', DefaultDirectionStr);
              Direction := StrToDirectionType(DirectionStr);
              IF Direction = UnknownDirection THEN
                ShowMessage('"' + DirectionStr + '" is not a valid direction')
              ELSE BEGIN
                { success! }
                TC_SpeedRestrictionInMPH := IntToMPH(Speed);
                TC_SpeedRestrictionDirection := Direction;
                CASE Direction OF
                  Up:
                    Log('T TC=' + IntToStr(Line_TC) + ' set to ' + IntToStr(Speed) + ' mph Up');
                  Down:
                    Log('T TC=' + IntToStr(Line_TC) + ' set to ' + IntToStr(Speed) + ' mph Down');
                  Bidirectional:
                    Log('T TC=' + IntToStr(Line_TC) + ' set to ' + IntToStr(Speed) + ' mph in both directions');
                END; {CASE}
              END;
            END;
          END;
        END; {WITH}
        InvalidateScreen(UnitRef, 'SetOrClearTrackCircuitSpeedRestriction');
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG SetOrClearTrackCircuitSpeedRestriction:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetOrClearTrackCircuitSpeedRestriction }

PROCEDURE ClearLocoFromTrackCircuit(TC : Integer);
{ Clear a loco allocation from a given track circuit and other associated locations }
VAR
  T : TrainIndex;

BEGIN
  TRY
    T := GetTrainIndexFromLocoChip(TrackCircuits[TC].TC_LocoChip);
    IF T <> UnknownTrainIndex THEN BEGIN
      WITH Trains[T] DO BEGIN
        Train_CurrentTC := UnknownTrackCircuit;
        Train_SavedLocation := UnknownLocation;

        Log(LocoChipToStr(TrackCircuits[TC].TC_LocoChip) + ' DG Loco chip cleared from TC' + IntToStr(TC));
        InvalidateScreen(UnitRef, 'ClearLocoFromTrackCircuit');
      END; {WITH}
    END;

    TrackCircuits[TC].TC_LocoChip := UnknownLocoChip;
    TrackCircuits[TC].TC_OccupationState := TrackCircuits[TC].TC_PreviousOccupationState;
  EXCEPT
    ON E : Exception DO
      Log('EG ClearLocoFromTrackCircuit:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ClearLocoFromTrackCircuit }

PROCEDURE AllocateLocoToTrackCircuit(Line : Integer);
{ Allocate (or remove) a loco chip number from a given track circuit }
CONST
  AllLocos = True;

VAR
  AdjacentUpTC, AdjacentDownTC : Integer;
  DebugStr : String;
  I : Integer;
  InputQueryLocoChipStr : String;
  NewTrackCircuitState : TrackCircuitStateType;
  PossibleLocoChip : Integer;
  PossibleT : TrainIndex;
  SavePossibleLocoChip : Integer;
  TC : Integer;
  TCArray : IntegerArrayType;

BEGIN
  TRY
    WITH Lines[Line] DO BEGIN
      { If we don't know which loco is on a particular line }
      IF Line_TC <> UnknownTrackCircuit THEN BEGIN
        { save chip number previously allocated to this track circuit }
        PossibleLocoChip := TrackCircuits[Line_TC].TC_LocoChip;
        SavePossibleLocoChip := PossibleLocoChip;

        InputQueryLocoChipStr := IntToStr(PossibleLocoChip);
        IF InputQueryLocoChipStr = IntToStr(UnknownLocoChip) THEN
          InputQueryLocoChipStr := '';
        IF InputQuery('Track-Circuit Occupation', 'Loco chip no?', InputQueryLocoChipStr)THEN BEGIN
          IF InputQueryLocoChipStr = '' THEN
            { we're presumably clearing the loco chip from the occupation }
            PossibleLocoChip := UnknownLocoChip
          ELSE
            IF NOT TryStrToInt(InputQueryLocoChipStr, PossibleLocoChip) THEN BEGIN
              Debug('!Invalid number');
              PossibleLocoChip := UnknownLocoChip;
            END;

          IF PossibleLocoChip = UnknownLocoChip THEN BEGIN
            IF SavePossibleLocoChip = UnknownLocoChip THEN
              { no number was there to start with }
              ShowMessage('No loco chip number added to TC=' + IntToStr(Line_TC))
            ELSE
              ClearLocoFromTrackCircuit(Line_TC);
          END ELSE BEGIN
            { see if it's recorded as being somewhere else - but see if that somewhere else is adjacent }
            PossibleT := GetTrainIndexFromLocoChip(PossibleLocoChip);
            IF PossibleT = UnknownTrainIndex THEN
              Debug('!Loco ' + LocoChipToStr(PossibleLocoChip) + ' is not in the loco table')
            ELSE
              IF NOT Locos[Trains[PossibleT].Train_LocoIndex].Loco_Active THEN
                Debug('!Loco ' + LocoChipToStr(PossibleLocoChip) + ' is in the loco table but is not active')
              ELSE BEGIN
                FindAdjoiningTrackCircuits(Line_TC, AdjacentUpTC, AdjacentDownTC);
                IF (Trains[PossibleT].Train_CurrentTC <> UnknownTrackCircuit)
                AND (Trains[PossibleT].Train_CurrentTC <> Line_TC)
                AND (TrackCircuits[Trains[PossibleT].Train_CurrentTC].TC_LocoChip <> UnknownLocoChip)
                AND (Trains[PossibleT].Train_CurrentTC <> AdjacentUpTC)
                AND (Trains[PossibleT].Train_CurrentTC <> AdjacentDownTC)
                AND (TrackCircuits[Line_TC].TC_OccupationState <> TCSystemOccupation)
                AND (MessageDialogueWithDefault('Loco ' + IntToStr(PossibleLocoChip) + ' is already recorded as being at TC=' + IntToStr(Trains[PossibleT].Train_CurrentTC)
                                                + ': has it moved?',
                                                NOT StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo)
                THEN
                  ShowMessage('Loco ' + IntToStr(PossibleLocoChip) + ' not be allocated to TC=' + IntToStr(Line_TC))
                ELSE BEGIN
                  { otherwise change the allocation, unless it's a system allocation }
                  IF TrackCircuits[Line_TC].TC_OccupationState = TCSystemOccupation THEN BEGIN
                    TrackCircuits[Line_TC].TC_LocoChip := PossibleLocoChip;
                    InvalidateScreen(UnitRef, 'AllocateLocoToTrackCircuit');
                    Log(LocoChipToStr(PossibleLocoChip) + ' T System allocated to TC=' + IntToStr(Line_TC) + ' by user');
                  END ELSE BEGIN
                    Trains[PossibleT].Train_CurrentTC := Line_TC;

                    CASE GetTrackCircuitState(Line_TC) OF
                      TCFeedbackOccupation, TCLocoOutOfPlaceOccupation, TCPermanentFeedbackOccupation:
                        NewTrackCircuitState := TCPermanentFeedbackOccupation;
                      TCPermanentSystemOccupation:
                        NewTrackCircuitState := TCPermanentSystemOccupation;
                      TCUnoccupied, TCPermanentOccupationSetByUser:
                        NewTrackCircuitState := TCPermanentOccupationSetByUser;
                    ELSE
                      NewTrackCircuitState := TCUnoccupied;
                    END; {CASE}

                    { and de-allocate it from where it was }
                    FOR TC := 0 TO High(TrackCircuits) DO BEGIN
                      IF TC <> Line_TC THEN BEGIN
                        IF TrackCircuits[TC].TC_LocoChip = Trains[PossibleT].Train_LocoChip THEN BEGIN
                          TrackCircuits[TC].TC_LocoChip := UnknownLocoChip;
                          IF NOT TrackCircuitStateIsPermanentlyOccupied(TrackCircuits[TC].TC_OccupationState) THEN
                            SetTrackCircuitState(TC, TCUnoccupied);
                        END;
                      END;
                    END; {FOR}

                    SetTrackCircuitState(PossibleLocoChip, Line_TC, NewTrackCircuitState);
                    SetLength(TCArray, 0);
                    { and any other adjacent track circuits in the same location, unless they're already occupied. First search up the line: }
                    REPEAT
                      TC := AdjacentUpTC;
                      IF TC <> UnknownTrackCircuit THEN BEGIN
                        IF TrackCircuits[AdjacentUpTC].TC_OccupationState = TCFeedbackOccupation THEN BEGIN
                          SetTrackCircuitState(PossibleLocoChip, TC, NewTrackCircuitState);
                          AppendToIntegerArray(TCArray, TC);
                        END;
                        FindAdjoiningTrackCircuits(TC, AdjacentUpTC, AdjacentDownTC);
                      END;
                    UNTIL (AdjacentUpTC = UnknownTrackCircuit) OR (TrackCircuits[AdjacentUpTC].TC_OccupationState = TCUnoccupied);

                    { and then down: }
                    FindAdjoiningTrackCircuits(Line_TC, AdjacentUpTC, AdjacentDownTC);
                    REPEAT
                      TC := AdjacentDownTC;
                      IF TC <> UnknownTrackCircuit THEN BEGIN
                        IF TrackCircuits[AdjacentDownTC].TC_OccupationState = TCFeedbackOccupation THEN BEGIN
                          SetTrackCircuitState(PossibleLocoChip, TC, NewTrackCircuitState);
                          AppendToIntegerArray(TCArray, TC);
                        END;
                        FindAdjoiningTrackCircuits(TC, AdjacentUpTC, AdjacentDownTC);
                      END;
                    UNTIL (AdjacentDownTC = UnknownTrackCircuit) OR (TrackCircuits[AdjacentDownTC].TC_OccupationState = TCUnoccupied);

                    InvalidateScreen(UnitRef, 'AllocateLocoToTrackCircuit');
                    DebugStr := 'Loco ' + IntToStr(PossibleLocoChip) + ' allocated to TC=' + IntToStr(Line_TC);
                    IF Length(TCArray) > 0 THEN BEGIN
                      DebugStr := DebugStr + ' and also allocated to TC=' + IntToStr(TCArray[0]);
                      IF Length(TCArray) > 1 THEN BEGIN
                        FOR I := 1 TO High(TCArray) DO
                          DebugStr := DebugStr + ', TC=' + IntToStr(TCArray[I]);
                      END;
                    END;
                    Log(LocoChipToStr(PossibleLocoChip) + ' TG ' + DebugStr);
                  END;
                END;
              END;
          END;
        END;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG AllocateLocoToTrackCircuit:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { AllocateLocoToTrackCircuit }

PROCEDURE ChangeInternalLocoDirectionToUp(LocoChip : Integer);
CONST
  NoValue = 0;

VAR
  L : LocoIndex;

BEGIN
  TRY
    IF MessageDialogueWithDefault('Change loco ' + LocoChipToStr(LocoChip) + '''s internal direction to up - are you sure?',
                                  NOT StopTimer, mtWarning, [mbYes, mbNo], mbNo) <> mrYes THEN
      Debug('Cancelling change of loco ' + LocoChipToStr(LocoChip) + '''s internal direction changed to up')
    ELSE BEGIN
      L := GetLocoIndexFromLocoChip(LocoChip);
      IF L <> UnknownLocoIndex THEN BEGIN
        WITH Locos[L] DO BEGIN
          IF Loco_LightsType <> LightsOperatedByTwoChips THEN BEGIN
            ProgramOnTheMain(Locos[L], ChangeDirectionToUp, NoValue);
            Log(LocoChipToStr(L) + ' XG Internal direction changed to up');
          END ELSE BEGIN
            ProgramOnTheMain(Locos[Loco_LightingChipUpIndex], ChangeDirectionToUp, NoValue);
            Log(LocoChipToStr(Loco_LightingChipUpIndex) + ' XG Internal direction changed to up');

            ProgramOnTheMain(Locos[Loco_LightingChipDownIndex], ChangeDirectionToUp, NoValue);
            Log(LocoChipToStr(Loco_LightingChipDownIndex) + ' XG Internal direction changed to up');
          END;
        END; {WITH}
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG ChangeLocoDirectionToUp:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ChangeInternalLocoDirectionToUp }

PROCEDURE ChangeInternalLocoDirectionToDown(LocoChip : Integer);
CONST
  NoValue = 0;

VAR
  L : LocoIndex;

BEGIN
  TRY
    IF MessageDialogueWithDefault('Change loco ' + LocoChipToStr(LocoChip) + '''s internal direction to down - are you sure?',
                                  NOT StopTimer, mtWarning, [mbYes, mbNo], mbNo) <> mrYes THEN
      Debug('Cancelling change of loco ' + LocoChipToStr(LocoChip) + '''s internal direction changed to down')
    ELSE BEGIN
      L := GetLocoIndexFromLocoChip(LocoChip);
      IF L <> UnknownLocoIndex THEN BEGIN
        WITH Locos[L] DO BEGIN
          IF Loco_LightsType <> LightsOperatedByTwoChips THEN BEGIN
            ProgramOnTheMain(Locos[L], ChangeDirectionToDown, NoValue);
            Log(Loco_LocoChipStr + ' XG Internal direction changed to down');
          END ELSE BEGIN
            ProgramOnTheMain(Locos[Loco_LightingChipUpIndex], ChangeDirectionToDown, NoValue);
            Log(LocoChipToStr(Loco_LightingChipUp) + ' XG Internal direction changed to down');

            ProgramOnTheMain(Locos[Loco_LightingChipDownIndex], ChangeDirectionToDown, NoValue);
            Log(LocoChipToStr(Loco_LightingChipDown) + ' XG Internal direction changed to down');
          END;
        END; {WITH}
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG ChangeLocoDirectionToDown:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ChangeInternalLocoDirectionToDown }

PROCEDURE TFWPRailWindow.LinePopupItemClick(Sender: TObject);
CONST
  NewTrackCircuit = True;

VAR
  LineCount : Integer;
  OK : Boolean;
  T : TrainIndex;

BEGIN
  { Wait before popping up to avoid the same click that activates the popup menu also providing the menu item click }
  IF PopupTimerCount < 1 THEN BEGIN
    Debug('!*Delay required before selecting menu entry');
    PopupTimer.Enabled := False;
    Exit;
  END;

  WITH Sender AS TMenuItemExtended DO BEGIN
    { this code has been separated out from the case statement below as we may well not be on a line, and the "WITH Lines[LinePopupNumArray[0]]" statement would fail }
    IF MenuPopupType = LineExitEditModePopupType THEN BEGIN
      TurnEditModeOff;
      TurnCreateLineModeOff;
    END ELSE
      IF MenuPopupType = LineEnterCreateLineModePopupType THEN
        TurnCreateLineModeOn
      ELSE
        IF MenuPopupType = LineExitCreateLineModePopupType THEN BEGIN
          TurnCreateLineModeOff
        END ELSE BEGIN
          WITH Lines[LinePopupNumArray[0]] DO BEGIN
            CASE MenuPopupType OF
              LineAllocateLocoToTrackCircuitPopupType:
                IF TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip THEN
                  AllocateLocoToTrackCircuit(LinePopupNumArray[0])
                ELSE
                  ClearLocoFromTrackCircuit(Line_TC);

              LineChangeInternalLocoDirectionToUpPopupType:
                IF TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip THEN
                  ChangeInternalLocoDirectionToUp(TrackCircuits[Line_TC].TC_LocoChip);
              LineChangeInternalLocoDirectionToDownPopupType:
                IF TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip THEN
                  ChangeInternalLocoDirectionToDown(TrackCircuits[Line_TC].TC_LocoChip);

              LineCreateUpSignalPopupType:
                CreateSignal(Up, LinePopupNumArray[0]);
              LineCreateDownSignalPopupType:
                CreateSignal(Down, LinePopupNumArray[0]);

              LineCreateOrdinaryPointPopupType:
                CreatePoint(LinePopupNumArray, OrdinaryPoint, MouseX, MouseY);
              LineCreateCrossOverPointPopupType:
                CreatePoint(LinePopupNumArray, CrossOverPoint, MouseX, MouseY);
              LineCreateThreeWayPointAPopupType:
                CreatePoint(LinePopupNumArray, ThreeWayPointA, MouseX, MouseY);
              LineCreateThreeWayPointBPopupType:
                CreatePoint(LinePopupNumArray, ThreeWayPointB, MouseX, MouseY);
              LineCreateCatchPointUpPopupType:
                CreatePoint(LinePopupNumArray, CatchPointUp, MouseX, MouseY);
              LineCreateCatchPointDownPopupType:
                CreatePoint(LinePopupNumArray, CatchPointDown, MouseX, MouseY);

              LineDeletePopupType:
                DeleteLine(LinePopupNumArray[0], OK);

              LineJoinPopupType:
                JoinLine(LinePopupNumArray, MapScreenXToGridX(MouseX), MapScreenYToGridY(MouseY));
              LineSplitPopupType:
                SplitLine(LinePopupNumArray[0], MapScreenXToGridX(MouseX), MapScreenYToGridY(MouseY));

              LineEditPopupType:
                TurnEditModeOn(UnknownSignal, UnknownPoint, UnknownBufferStop, LinePopupNumArray[0], UnknownTrackCircuit);

              LineLocationOutOfUsePopupType:
                IF Line_Location <> UnknownLocation THEN BEGIN
                  WITH Locations[Line_Location] DO BEGIN
                    IF Location_OutOfUse THEN BEGIN
                      Location_OutOfUse := False;

                      { Now restore all the lines within the location to their previous state }
                      FOR LineCount := 0 TO High(Lines) DO BEGIN
                        IF Lines[LineCount].Line_Location = Line_Location THEN BEGIN
                          IF Lines[LineCount].Line_SaveOutOfUseState = OutOfUse THEN
                            Lines[LineCount].Line_OutOfUseState := OutOfUse
                          ELSE
                            Lines[LineCount].Line_OutOfUseState := InUse;
                        END;
                      END;
                    END ELSE BEGIN
                      Location_OutOfUse := True;

                      { Now set all the lines within the location to out of use too }
                      FOR LineCount := 0 TO High(Lines) DO BEGIN
                        IF Lines[LineCount].Line_Location = Line_Location THEN BEGIN
                          Lines[LineCount].Line_SaveOutOfUseState := Lines[LineCount].Line_OutOfUseState;
                          Lines[LineCount].Line_OutOfUseState := OutOfUse;
                        END;
                      END;
                    END;

                    InvalidateScreen(UnitRef, 'LinePopupItemClick LineLocationOutOfUsePopupType');
                  END; {WITH}
                END;

              LineOutOfUsePopupType:
                BEGIN
                  IF Line_OutOfUseState = OutOfUse THEN
                    Line_OutOfUseState := InUse
                  ELSE
                    Line_OutOfUseState := OutOfUse;
                  InvalidateScreen(UnitRef, 'LinePopupItemClick LineOutOfUsePopupType');
                END;

              LineShowLocoLastErrorMessagePopupType:
                IF Line_TC <> UnknownTrackCircuit THEN BEGIN
                  T := GetTrainIndexFromLocoChip(TrackCircuits[Line_TC].TC_LocoChip);
                  IF T <> UnknownTrainIndex THEN BEGIN
                    WITH Trains[T] DO BEGIN
                      IF Train_LastRouteLockedMsgStr <> '' THEN
                        Debug(Train_LocoChipStr + ': ' +  Train_LastRouteLockedMsgStr);
                      IF Train_RouteCreationHoldMsg <> '' THEN
                        Debug(Train_LocoChipStr + ': ' + Train_RouteCreationHoldMsg);
                    END;
                  END;
                END;

              LineTCFeedbackOccupationPopupType:
                IF Line_TC <> UnknownTrackCircuit THEN BEGIN
                  SetTrackCircuitState(Line_TC, TCFeedbackOccupation);
                  InvalidateScreen(UnitRef, 'LinePopupItemClick LineTCFeedbackOccupationPopupType');
                END;

              LineTCSystemOccupationPopupType:
                IF Line_TC <> UnknownTrackCircuit THEN BEGIN
                  SetTrackCircuitState(Line_TC, TCSystemOccupation);
                  InvalidateScreen(UnitRef, 'LinePopupItemClick LineTCSystemOccupationPopupType');
                END;

              LineTCOutOfUsePopupType:
                IF Line_TC <> UnknownTrackCircuit THEN BEGIN
                  SetTrackCircuitState(Line_TC, TCOutOfUseSetByUser);
                  InvalidateScreen(UnitRef, 'LinePopupItemClick LineTCOutOfUsePopupType');
                END;

              LineTCPermanentOccupationPopupType:
                IF Line_TC <> UnknownTrackCircuit THEN BEGIN
                  SetTrackCircuitState(Line_TC, TCPermanentOccupationSetByUser);
                  InvalidateScreen(UnitRef, 'LinePopupItemClick LineTCPermanentOccupationPopupType:');
                END;

              LineTCSpeedRestrictionPopupType:
                IF Line_TC <> UnknownTrackCircuit THEN
                  SetOrClearTrackCircuitSpeedRestriction(LinePopupNumArray[0]);

              LineTCUnoccupiedPopupType:
                IF Line_TC <> UnknownTrackCircuit THEN BEGIN
                  SetTrackCircuitState(Line_TC, TCUnoccupied);
                  InvalidateScreen(UnitRef, 'LinePopupItemClick LineTCUnoccupiedPopupType');
                END;

              LineAllocateExistingTrackCircuitPopupType:
                IF Line_TC = UnknownTrackCircuit THEN
                  AddTrackCircuit(LinePopupNumArray[0], NOT NewTrackCircuit);

              LineAllocateNewTrackCircuitPopupType:
                IF Line_TC = UnknownTrackCircuit THEN
                  AddTrackCircuit(LinePopupNumArray[0], NewTrackCircuit);

              LineRemoveTrackCircuitPopupType:
                IF MessageDialogueWithDefault('Remove Track Circuit ' + IntToStr(Line_TC) + ' From Line ' + LineToStr(LinePopupNumArray[0]) + '?',
                                              StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
                THEN BEGIN
                  { remove the track circuit from the database if it's not in use somewhere else }
                  DeleteLineTrackCircuit(LinePopupNumArray[0]);
                  Line_TC := UnknownTrackCircuit;
                END;
            ELSE {CASE}
              Log('BG Invalid popup type ' + IntToStr(Tag) + ' in LinePopupItemClick');
            END; {CASE}
          END; {WITH}
        END;
  END; {WITH}
END; { LinePopupItemClick }

PROCEDURE TFWPRailWindow.LinePopupMenuOnPopup(Sender: TObject);
VAR
  NextLine : Integer;
  WhetherEnabled : Boolean;

  FUNCTION LinesAreParallel(Line1, Line2 : Integer) : Boolean;
  VAR
    M1, M2 : Extended;

  BEGIN
    { Find the gradient of each line }
    WITH Lines[Line1] DO
      M1 := (Line_GridUpY - Line_GridDownY) / (Line_GridUpX - Line_GridDownX);
    WITH Lines[Line2] DO
      M2 := (Line_GridUpY - Line_GridDownY) / (Line_GridUpX - Line_GridDownX);

    Result := Abs(M1 - M2) < 0.1;
  END; { LinesAreParallel }

BEGIN
  LinePopupMenu.Items.Clear;

  IF Length(LinePopupNumArray) = 0 THEN BEGIN
    { we're not on a line, so all we can do is create a line here }
    IF EditMode THEN BEGIN
      AddMainMenuItem(LinePopupMenu, 'Create a New Line', NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
      AddMainMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

      IF NOT CreateLineMode THEN
        AddMainMenuItem(LinePopupMenu, 'Enter Create Line Mode', LineEnterCreateLineModePopupType, Enabled, Visible, NOT Checked, LinePopupItemClick)
      ELSE
        AddMainMenuItem(LinePopupMenu, 'Exit Create Line Mode', LineExitCreateLineModePopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);

      IF EditMode THEN
        AddMainMenuItem(LinePopupMenu, 'Exit Edit Mode', LineExitEditModePopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);
    END;
  END ELSE BEGIN
    WITH Lines[LinePopupNumArray[0]] DO BEGIN
      IF NOT EditMode THEN BEGIN
        { Add the caption... }
        Caption := 'Line ' + LineToStr(LinePopupNumArray[0]) + ' ' + IfThen(Line_TC <> UnknownTrackCircuit,
                                                                    'TC' + IntToStr(Line_TC));
        AddMainMenuItem(LinePopupMenu, Caption, NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
        AddMainMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

        { ...and now the individual items }
        IF Line_TC <> UnknownTrackCircuit THEN BEGIN
          WhetherEnabled := TrackCircuits[Line_TC].TC_OccupationState <> TCUnOccupied;
          AddMainMenuItem(LinePopupMenu, 'Set Track Circuit ' + IntToStr(Line_TC) + ' To Unoccupied', LineTCUnoccupiedPopupType, WhetherEnabled, Visible, NOT Checked,
                        LinePopupItemClick);

          WhetherEnabled := TrackCircuits[Line_TC].TC_OccupationState <> TCFeedbackOccupation;
          AddMainMenuItem(LinePopupMenu, 'Set Track Circuit ' + IntToStr(Line_TC) + ' To Feedback Occupation', LineTCFeedbackOccupationPopupType, WhetherEnabled,
                      Visible, NOT Checked, LinePopupItemClick);

          WhetherEnabled := TrackCircuits[Line_TC].TC_OccupationState <> TCSystemOccupation;
          AddMainMenuItem(LinePopupMenu, 'Set Track Circuit ' + IntToStr(Line_TC) + ' To System Occupation', LineTCSystemOccupationPopupType, WhetherEnabled, Visible, NOT Checked,
                      LinePopupItemClick);

          WhetherEnabled := (TrackCircuits[Line_TC].TC_OccupationState <> TCPermanentFeedbackOccupation)
                             AND (TrackCircuits[Line_TC].TC_OccupationState <> TCPermanentOccupationSetByUser);

          AddMainMenuItem(LinePopupMenu, 'Set Track Circuit ' + IntToStr(Line_TC) + ' To Permanent Occupation', LineTCPermanentOccupationPopupType, WhetherEnabled,
                      Visible, NOT Checked, LinePopupItemClick);

          WhetherEnabled := TrackCircuits[Line_TC].TC_OccupationState <> TCOutOfUseSetByUser;
          AddMainMenuItem(LinePopupMenu, 'Set Track Circuit ' + IntToStr(Line_TC) + ' Out Of Use', LineTCOutOfUsePopupType, WhetherEnabled, Visible, NOT Checked,
                      LinePopupItemClick);

          IF TrackCircuits[Line_TC].TC_SpeedRestrictionInMPH = NoSpecifiedSpeed THEN
            AddMainMenuItem(LinePopupMenu, 'Set Track Circuit ' + IntToStr(Line_TC) + ' Speed Restriction', LineTCSpeedRestrictionPopupType, Enabled, Visible, NOT Checked,
                        LinePopupItemClick)
          ELSE
            AddMainMenuItem(LinePopupMenu, 'Clear Track Circuit ' + IntToStr(Line_TC) + ' Speed Restriction', LineTCSpeedRestrictionPopupType, Enabled, Visible, NOT Checked,
                        LinePopupItemClick);
        END;

        AddMainMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

        { Can't have both locations and lines out of use at the same time }
        IF (Line_Location <> UnknownLocation) AND Locations[Line_Location].Location_OutOfUse THEN BEGIN
          AddMainMenuItem(LinePopupMenu, 'Put Line ''' + LineToStr(LinePopupNumArray[0]) + ''' Out Of Use', LineTCOutOfUsePopupType, NOT Enabled, Visible, NOT Checked,
                      LinePopupItemClick);
          AddMainMenuItem(LinePopupMenu, 'Return Location ''' + LocationToStr(Line_Location) + ''' To Use', LineLocationOutOfUsePopupType, Enabled, Visible, NOT Checked,
                      LinePopupItemClick);
        END ELSE
          IF Line_OutOfUseState = OutOfUse THEN BEGIN
            AddMainMenuItem(LinePopupMenu, 'Return Line ''' + LineToStr(LinePopupNumArray[0]) + ''' To Use', LineTCOutOfUsePopupType, Enabled, Visible, NOT Checked,
                        LinePopupItemClick);
            AddMainMenuItem(LinePopupMenu, 'Put Location ''' + LocationToStr(Line_Location) + ''' Out Of Use', LineLocationOutOfUsePopupType, NOT Enabled, Visible, NOT Checked,
                        LinePopupItemClick);
          END ELSE BEGIN
            AddMainMenuItem(LinePopupMenu, 'Put Line ''' + LineToStr(LinePopupNumArray[0]) + ''' Out Of Use', LineTCOutOfUsePopupType, Enabled, Visible, NOT Checked,
                        LinePopupItemClick);
            AddMainMenuItem(LinePopupMenu, 'Put Location ''' + LocationToStr(Line_Location) + ''' Out Of Use', LineLocationOutOfUsePopupType, Enabled, Visible, NOT Checked,
                        LinePopupItemClick);
          END;

        AddMainMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

        IF Line_TC <> UnknownTrackCircuit THEN BEGIN
          IF TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip THEN
            AddMainMenuItem(LinePopupMenu, 'Allocate Loco To Track Circuit', LineAllocateLocoToTrackCircuitPopupType, Enabled, Visible, NOT Checked, LinePopupItemClick)
          ELSE
            AddMainMenuItem(LinePopupMenu, 'Clear Loco Allocation From Track Circuit', LineAllocateLocoToTrackCircuitPopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);

          IF (Line_TC = UnknownTrackCircuit) OR (TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip) THEN BEGIN
            AddMainMenuItem(LinePopupMenu, 'Change Internal Loco Chip Direction to Up', LineChangeInternalLocoDirectionToUpPopupType, Enabled, Visible, NOT Checked,
                        LinePopupItemClick);
            AddMainMenuItem(LinePopupMenu, 'Change Internal Loco Chip Direction to Down', LineChangeInternalLocoDirectionToDownPopupType, Enabled, Visible, NOT Checked,
                        LinePopupItemClick);
          END ELSE BEGIN
            AddMainMenuItem(LinePopupMenu, 'Change ' + LocoChipToStr(TrackCircuits[Line_TC].TC_LocoChip) + '''s Internal Loco Chip Direction to Up',
                        LineChangeInternalLocoDirectionToUpPopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);
            AddMainMenuItem(LinePopupMenu, 'Change ' + LocoChipToStr(TrackCircuits[Line_TC].TC_LocoChip) + '''s Internal Loco Chip Direction to Down',
                        LineChangeInternalLocoDirectionToDownPopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);
          END;

          WhetherEnabled := TrackCircuits[Line_TC].TC_LocoChip <> UnknownLocoChip;
          AddMainMenuItem(LinePopupMenu, 'Show Loco''s Last Error Message', LineShowLocoLastErrorMessagePopupType, WhetherEnabled, Visible, NOT Checked, LinePopupItemClick);
        END;

        AddMainMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

        AddMainMenuItem(LinePopupMenu, 'Edit Line ' + LineToStr(LinePopupNumArray[0]), LineEditPopupType, Enabled, Visible, NOT Checked, LinePopupItemClick)
      END ELSE BEGIN
        { EditMode }

        { Add the caption }
        IF Length(LinePopupNumArray) > 1 THEN BEGIN
          { if we're at the junction of two lines, add point-type items }
          Caption := 'Creating new points at junction of lines ' + LineToStr(LinePopupNumArray[0]) + ' and ' + LineToStr(LinePopupNumArray[1]);

          AddMainMenuItem(LinePopupMenu, Caption, NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
          AddMainMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

          AddMainMenuItem(LinePopupMenu, 'Create Ordinary Point', LineCreateOrdinaryPointPopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);
          AddMainMenuItem(LinePopupMenu, 'Create Cross-Over Point', LineCreateCrossOverPointPopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);
          AddMainMenuItem(LinePopupMenu, 'Create Three-Way Point A', LineCreateThreeWayPointAPopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);
          AddMainMenuItem(LinePopupMenu, 'Create Three-Way Point B', LineCreateThreeWayPointBPopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);
        END ELSE BEGIN
          { otherwise add line-type items (and catch points) }
          Caption := 'Editing Line ' + LineToStr(LinePopupNumArray[0]) + ' ' + IfThen(Line_TC <> UnknownTrackCircuit,
                                                                                      'TC' + IntToStr(Line_TC));
          AddMainMenuItem(LinePopupMenu, Caption, NoClickPopupType, NOT Enabled, Visible, NOT Checked, NIL);
          AddMainMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

          { Join two lines - see if the cursor is in one of the handle polygons, and the next line is at the same angle }
          WhetherEnabled := False;
          IF PointInPolygon(Line_UpHandlePolygon, Point(MouseX, MouseY)) THEN BEGIN
            NextLine := Lines[LinePopupNumArray[0]].Line_NextUpLine;
            IF NextLine <> UnknownLine THEN BEGIN
              IF LinesAreParallel(LinePopupNumArray[0], NextLine) THEN BEGIN
                SetLength(LinePopupNumArray, 2);
                { move the existing up-line name so that the array is for these purposes up line followed by down line }
                LinePopupNumArray[1] := LinePopupNumArray[0];
                LinePopupNumArray[0] := NextLine;
                WhetherEnabled := True;
              END;
            END;
          END ELSE
            IF PointInPolygon(Line_DownHandlePolygon, Point(MouseX, MouseY)) THEN BEGIN
              NextLine := Lines[LinePopupNumArray[0]].Line_NextDownLine;
              IF NextLine <> UnknownLine THEN BEGIN
                IF LinesAreParallel(LinePopupNumArray[0], NextLine) THEN BEGIN
                  SetLength(LinePopupNumArray, 2);
                  { add the new down-line name so that the array is for these purposes up line followed by down line }
                  LinePopupNumArray[1] := NextLine;
                  WhetherEnabled := True;
                END;
              END;
            END;

          IF Length(LinePopupNumArray) > 1 THEN
            AddMainMenuItem(LinePopupMenu, 'Join Lines ' + LineToStr(LinePopupNumArray[0]) + ' and ' + LineToStr(LinePopupNumArray[1]), LineJoinPopupType, WhetherEnabled,
                        Visible, NOT Checked, LinePopupItemClick);
          AddMainMenuItem(LinePopupMenu, 'Split Line ' + LineToStr(LinePopupNumArray[0]), LineSplitPopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);
          AddMainMenuItem(LinePopupMenu, 'Delete Line ' + LineToStr(LinePopupNumArray[0]), LineDeletePopupType, Enabled, Visible, NOT Checked, LinePopupItemClick);

          WhetherEnabled := Lines[LinePopupNumArray[0]].Line_TC = UnknownTrackCircuit;
          AddMainMenuItem(LinePopupMenu, 'Allocate Existing Track Circuit To Line ' + LineToStr(LinePopupNumArray[0]), LineAllocateExistingTrackCircuitPopupType,
                      WhetherEnabled, Visible, NOT Checked, LinePopupItemClick);
          WhetherEnabled := Lines[LinePopupNumArray[0]].Line_TC = UnknownTrackCircuit;
          AddMainMenuItem(LinePopupMenu, 'Allocate New Track Circuit To Line ' + LineToStr(LinePopupNumArray[0]), LineAllocateNewTrackCircuitPopupType, WhetherEnabled,
                      Visible, NOT Checked, LinePopupItemClick);
          WhetherEnabled := Lines[LinePopupNumArray[0]].Line_TC <> UnknownTrackCircuit;
          AddMainMenuItem(LinePopupMenu, 'Remove Track Circuit ' + IntToStr(Line_TC) + ' From Line ' + LineToStr(LinePopupNumArray[0]), LineRemoveTrackCircuitPopupType,
                      Visible, NOT Checked, WhetherEnabled, LinePopupItemClick);

          AddMainMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, Visible, NOT Checked, NIL);

          WhetherEnabled := SignalAdjacentLineOK(LinePopupNumArray[0]);
          AddMainMenuItem(LinePopupMenu, 'Create Up Signal', LineCreateUpSignalPopupType, WhetherEnabled, Visible, NOT Checked, LinePopupItemClick);
          AddMainMenuItem(LinePopupMenu, 'Create Down Signal', LineCreateDownSignalPopupType, WhetherEnabled, Visible, NOT Checked, LinePopupItemClick);

          { Catch points: check to see if the line we're on is horizontal, and use the handle polygons to work out whether we want the up or down end of the line }
          WhetherEnabled := (Lines[LinePopupNumArray[0]].Line_GridUpY = Lines[LinePopupNumArray[0]].Line_GridDownY)
                            AND ((PointInPolygon(Line_UpHandlePolygon, Point(MouseX, MouseY)))
                                OR (PointInPolygon(Line_DownHandlePolygon, Point(MouseX, MouseY))));
          AddMainMenuItem(LinePopupMenu, 'Create Up Catch Point', LineCreateCatchPointUpPopupType, WhetherEnabled, Visible, NOT Checked, LinePopupItemClick);
          AddMainMenuItem(LinePopupMenu, 'Create Down Catch Point', LineCreateCatchPointDownPopupType, WhetherEnabled, Visible, NOT Checked, LinePopupItemClick);
        END;
      END;
    END; {WITH}
  END;

  PopupTimerCount := 0;
  PopupTimer.Enabled := True;
END; { LinePopupMenuOnPopup }

PROCEDURE TFWPRailWindow.FWPRailApplicationEventsShortCut(VAR Msg: TWMKey; VAR Handled: Boolean);
{ This is called regardless of which window has focus }
VAR
  ShiftState : TShiftState;

BEGIN
  TRY
    ShiftState := [];
    IF GetKeyState(vk_Shift) < 0 THEN
      ShiftState := [ssShift];
    IF GetKeyState(vk_Control) < 0 THEN
      ShiftState := ShiftState + [ssCtrl];
    IF GetKeyState(vk_Menu) < 0 THEN
      ShiftState := ShiftState + [ssAlt];

    { Loco dialogue boxes need special treatment }
    IF (LocoDialogueWindow <> NIL) AND LocoDialogueWindow.Visible { AND LocoDialogueWindow.Focused AND LocoDialogueWindow.ActiveControl } THEN BEGIN
      CASE Msg.CharCode OF
        vk_Up: { up arrow key - need to handle specially in loco dialogue boxes }
          BEGIN
            IF LocoDialogueWindow.LocoDialogueUpButton.Enabled THEN BEGIN
              LocoDialogueIncreaseSpeed;
              Handled := True;
            END;
          END;
        vk_Down: { down arrow key - need to handle specially in loco dialogue boxes }
          BEGIN
            IF LocoDialogueWindow.LocoDialogueDownButton.Enabled THEN BEGIN
              LocoDialogueDecreaseSpeed;
              Handled := True;
            END;
          END;
        vk_Space: { space bar - need to handle specially in loco dialogue boxes }
          StopOrResumeAllOperations(DescribeKey(Msg.Charcode, ShiftState));
        vk_Return: { enter key - need to handle specially in loco dialogue boxes }
          IF LocoDialogueWindow.LocoDialogueLocoMaskEdit.Focused THEN BEGIN
            LocoDialogueChangeOrSelectLoco;
            Handled := True;
          END;
      END; {CASE}
    END;

    CASE Msg.CharCode OF
      vk_Tab:
        BEGIN
          Handled := True;
          KeyPressedDown(msg.Charcode, ShiftState);
        END;
      vk_F4:
        BEGIN
          KeyPressedDown(Msg.Charcode, ShiftState);
          Handled := True;
        END;
      vk_F10:
        BEGIN
          KeyPressedDown(Msg.Charcode, ShiftState);
          Handled := True;
        END;
    END; {CASE}
  EXCEPT
    ON E : Exception DO
      Log('EG RailApplicationEventsShortCut:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { RailApplicationEventsShortCut }

PROCEDURE ResetScreenColoursToDefault;
BEGIN
  BackgroundColour := DefaultBackgroundColour;
  BufferStopColour := DefaultBufferStopColour;
  BufferStopNumberColour := DefaultBufferStopNumberColour;
  BufferStopRed := DefaultBufferStopRed;
  ForegroundColour := DefaultForegroundColour;
  LineNotAvailableColour := DefaultLineNotAvailableColour;
  LineRoutedOverColour := DefaultLineRoutedOverColour;
  LocoStalledColour := DefaultLocoStalledColour;
  PlatformColour := DefaultPlatformColour;
  PointColour := DefaultPointColour;
  PointDefaultStateColour := DefaultPointDefaultStateColour;
  PointDivergingLineColour := DefaultPointDivergingLineColour;
  PointDownFacingColour := DefaultPointDownFacingColour;
  PointFeedbackDataInUseColour := DefaultPointFeedbackDataInUseColour;
  PointFeedbackDataOutOfUseColour := DefaultPointFeedbackDataOutOfUseColour;
  PointHeelLineColour := DefaultPointHeelLineColour;
  PointLenzNumberColour := DefaultPointLenzNumberColour;
  PointManualOperationColour := DefaultPointManualOperationColour;
  PointStraightLineColour :=DefaultPointStraightLineColour;
  PointsWithoutFeedbackColour := DefaultPointsWithoutFeedbackColour;
  PointUpFacingColour := DefaultPointUpFacingColour;
  ScreenComponentEditedColour1 := DefaultScreenComponentEditedColour1;
  ScreenComponentEditedColour2 := DefaultScreenComponentEditedColour2;
  PointLockedBySystemColour := DefaultPointLockedBySystemColour;
  SignalAspectGreen := DefaultSignalAspectGreen;
  SignalAspectRed := DefaultSignalAspectRed;
  SignalAspectUnlit := DefaultSignalAspectUnlit;
  SignalAspectYellow := DefaultSignalAspectYellow;
  SignalNumberColour := DefaultSignalNumberColour;
  SignalPostColour := DefaultSignalPostColour;
  SignalPostRouteSettingColour := DefaultSignalPostRouteSettingColour;
  SignalPostTheatreSettingColour := DefaultSignalPostTheatreSettingColour;
  SignalsFromWhichUserMustDriveSignalPostColour := DefaultSignalsFromWhichUserMustDriveSignalPostColour;
  TCMissingOccupationColour := DefaultTCMissingOccupationColour;
  TCFeedbackOccupationColour := DefaultTCFeedbackOccupationColour;
  TCOutOfUseSetByUserColour := DefaultTCOutOfUseSetByUserColour;
  TCOutOfUseAsNoFeedbackReceivedColour := DefaultTCOutOfUseAsNoFeedbackReceivedColour;
  TCPermanentFeedbackOccupationColour := DefaultTCPermanentFeedbackOccupationColour;
  TCPermanentOccupationSetByUserColour := DefaultTCPermanentOccupationSetByUserColour;
  TCPermanentSystemOccupationColour := DefaultTCPermanentSystemOccupationColour;
  TCSystemOccupationColour := DefaultTCSystemOccupationColour;
  TCUnoccupiedColour := DefaultTCUnoccupiedColour;
  TrainActiveColour := DefaultTrainActiveColour;
  TrainInactiveColour := DefaultTrainInactiveColour;
  TRSPlungerColour := DefaultTRSPlungerColour;
  TRSPlungerOutlineColour := DefaultTRSPlungerOutlineColour;
  TRSPlungerPressedColour := DefaultTRSPlungerPressedColour;

  InvalidateScreen(UnitRef, 'ResetScreenColoursToDefault');
END; { ResetScreenColoursToDefault }

PROCEDURE ResetScreenColoursAfterPrinting;
{ Restore the colours to those saved before printing the screen in printer-friendly colours }
BEGIN
  BackgroundColour := SaveBackgroundColourForPrinting;
  BufferStopColour := SaveBufferStopColourForPrinting;
  BufferStopNumberColour := SaveBufferStopNumberColourForPrinting;
  BufferStopRed := SaveBufferStopRedForPrinting;
  ForegroundColour := SaveForegroundColourForPrinting;
  PointLenzNumberColour := SavePointLenzNumberColourForPrinting;
  LineNotAvailableColour := SaveLineNotAvailableColourForPrinting;
  LineRoutedOverColour := SaveLineRoutedOverColourForPrinting;
  LocoStalledColour := SaveLocoStalledColourForPrinting;
  PlatformColour := SavePlatformColourForPrinting;
  PointColour := SavePointColourForPrinting;
  PointDefaultStateColour := SavePointDefaultStateColourForPrinting;
  PointDivergingLineColour := SavePointDivergingLineColourForPrinting;
  PointDownFacingColour := SavePointDownFacingColourForPrinting;
  PointFeedbackDataInUseColour := SavePointFeedbackDataInUseColourForPrinting;
  PointFeedbackDataOutOfUseColour := SavePointFeedbackDataOutOfUseColourForPrinting;
  PointHeelLineColour := SavePointHeelLineColourForPrinting;
  PointLockedBySystemColour := SavePointLockedBySystemColourForPrinting;
  PointManualOperationColour := SavePointManualOperationColourForPrinting;
  PointStraightLineColour := SavePointStraightLineColourForPrinting;
  PointsWithoutFeedbackColour := SavePointsWithoutFeedbackColourForPrinting;
  PointUpFacingColour := SavePointUpFacingColourForPrinting;
  ScreenComponentEditedColour1 := SaveScreenComponentEditedColour1ForPrinting;
  ScreenComponentEditedColour2 := SaveScreenComponentEditedColour2ForPrinting;
  SignalNumberColour := SaveSignalNumberColourForPrinting;
  SignalPostColour := SaveSignalPostColourForPrinting;
  SignalPostRouteSettingColour := SaveSignalPostRouteSettingColourForPrinting;
  SignalPostTheatreSettingColour := SaveSignalPostTheatreSettingColourForPrinting;
  SignalsFromWhichUserMustDriveSignalPostColour := SaveSignalsFromWhichUserMustDriveSignalPostColour;
  TCMissingOccupationColour := SaveTCMissingOccupationColourForPrinting;
  TCFeedbackOccupationColour := SaveTCFeedbackOccupationColourForPrinting;
  TCOutOfUseSetByUserColour := SaveTCOutOfUseSetByUserColourForPrinting;
  TCOutOfUseAsNoFeedbackReceivedColour := SaveTCOutOfUseAsNoFeedbackReceivedColourForPrinting;
  TCPermanentFeedbackOccupationColour := SaveTCPermanentFeedbackOccupationColourForPrinting;
  TCPermanentOccupationSetByUserColour := SaveTCPermanentOccupationSetByUserColourForPrinting;
  TCPermanentSystemOccupationColour := SaveTCPermanentSystemOccupationColourForPrinting;
  TCSystemOccupationColour := SaveTCSystemOccupationColourForPrinting;
  TCUnoccupiedColour := SaveTCUnoccupiedColourForPrinting;
  TrainActiveColour := SaveTrainActiveColourForPrinting;
  TrainInactiveColour := SaveTrainInactiveColourForPrinting;
  TRSPlungerColour := SaveTRSPlungerColourForPrinting;
  TRSPlungerOutlineColour := SaveTRSPlungerOutlineColourForPrinting;
  TRSPlungerPressedColour := SaveTRSPlungerPressedColourForPrinting;

  ScreenColoursSetForPrinting := False;
  Debug('Resetting screen colours');
END; { ResetScreenColoursAfterPrinting }

PROCEDURE SetScreenColoursBeforePrinting;
{ Save the screen colours before printing the screen in printer-friendly colours }
BEGIN
  SaveBackgroundColourForPrinting := BackgroundColour;
  BackgroundColour := clWhite;

  SaveBufferStopColourForPrinting := BufferStopColour;
  BufferStopColour := clBlack;

  SaveBufferStopNumberColourForPrinting := BufferStopNumberColour;
  BufferStopNumberColour := clBlack;

  SaveBufferStopRedForPrinting := BufferStopRed;
  BufferStopRed := clBlack;

  SaveForegroundColourForPrinting := ForegroundColour;
  ForegroundColour := clBlack;

  SavePointLenzNumberColourForPrinting := PointLenzNumberColour;
  PointLenzNumberColour := clBlack;

  SaveLineNotAvailableColourForPrinting := LineNotAvailableColour;
  LineNotAvailableColour := clBlack;

  SaveLineRoutedOverColourForPrinting := LineRoutedOverColour;
  LineRoutedOverColour := clBlack;

  SaveLocoStalledColourForPrinting := LocoStalledColour;
  LocoStalledColour := clBlack;

  SavePlatformColourForPrinting := PlatformColour;
  PlatformColour := clYellow;

  SavePointColourForPrinting := PointColour;
  PointColour := clBlack;

  SavePointDivergingLineColourForPrinting := PointDivergingLineColour;
  PointDivergingLineColour := clBlack;

  SavePointDownFacingColourForPrinting := PointDownFacingColour;
  PointDownFacingColour := clBlack;

  SavePointFeedbackDataInUseColourForPrinting := PointFeedbackDataInUseColour;
  PointFeedbackDataInUseColour := clBlack;

  SavePointFeedbackDataOutOfUseColourForPrinting := PointFeedbackDataOutOfUseColour;
  PointFeedbackDataOutOfUseColour := clBlack;

  SavePointHeelLineColourForPrinting := PointHeelLineColour;
  PointHeelLineColour := clBlack;

  SavePointManualOperationColourForPrinting := PointManualOperationColour;
  PointManualOperationColour := clBlack;

  SavePointStraightLineColourForPrinting := PointStraightLineColour;
  PointStraightLineColour := clBlack;

  SavePointsWithoutFeedbackColourForPrinting := PointsWithoutFeedbackColour;
  PointsWithoutFeedbackColour := clBlack;

  SavePointUpFacingColourForPrinting := PointUpFacingColour;
  PointUpFacingColour := clBlack;

  SaveScreenComponentEditedColour1ForPrinting := ScreenComponentEditedColour1;
  ScreenComponentEditedColour1 := clBlack;

  SaveScreenComponentEditedColour2ForPrinting := ScreenComponentEditedColour2;
  ScreenComponentEditedColour2 := clBlack;

  SavePointDefaultStateColourForPrinting := PointDefaultStateColour;
  PointDefaultStateColour := clBlack;

  SavePointLockedBySystemColourForPrinting := PointLockedBySystemColour;
  PointLockedBySystemColour := clBlack;

  SaveSignalNumberColourForPrinting := SignalNumberColour;
  SignalNumberColour := clBlack;

  SaveSignalPostColourForPrinting := SignalPostColour;
  SignalPostColour := clBlack;

  SaveSignalPostRouteSettingColourForPrinting := SignalPostRouteSettingColour;
  SignalPostRouteSettingColour := clBlack;

  SaveSignalPostTheatreSettingColourForPrinting := SignalPostTheatreSettingColour;
  SignalPostTheatreSettingColour := clBlack;

  SaveSignalsFromWhichUserMustDriveSignalPostColour := SignalsFromWhichUserMustDriveSignalPostColour;
  SignalsFromWhichUserMustDriveSignalPostColour := clBlack;

  SaveTCMissingOccupationColourForPrinting := TCMissingOccupationColour;
  TCMissingOccupationColour := clBlack;

  SaveTCFeedbackOccupationColourForPrinting := TCFeedbackOccupationColour;
  TCFeedbackOccupationColour := clBlack;

  SaveTCOutOfUseSetByUserColourForPrinting := TCOutOfUseSetByUserColour;
  TCOutOfUseSetByUserColour := clBlack;

  SaveTCOutOfUseAsNoFeedbackReceivedColourForPrinting := TCOutOfUseAsNoFeedbackReceivedColour;
  TCOutOfUseAsNoFeedbackReceivedColour := clBlack;

  SaveTCPermanentFeedbackOccupationColourForPrinting := TCPermanentFeedbackOccupationColour;
  TCPermanentFeedbackOccupationColour := clBlack;

  SaveTCPermanentOccupationSetByUserColourForPrinting := TCPermanentOccupationSetByUserColour;
  TCPermanentOccupationSetByUserColour := clBlack;

  SaveTCPermanentSystemOccupationColourForPrinting := TCPermanentSystemOccupationColour;
  TCPermanentSystemOccupationColour := clBlack;

  SaveTCSystemOccupationColourForPrinting := TCSystemOccupationColour;
  TCSystemOccupationColour := clBlack;

  SaveTCUnoccupiedColourForPrinting := TCUnoccupiedColour;
  TCUnoccupiedColour := clBlack;

  SaveTrainActiveColourForPrinting := TrainActiveColour;
  TrainActiveColour := clBlack;

  SaveTrainInactiveColourForPrinting := TrainInactiveColour;
  TrainInactiveColour := clBlack;

  SaveTRSPlungerColourForPrinting := TRSPlungerColour;
  TRSPlungerColour := clBlack;

  SaveTRSPlungerOutlineColourForPrinting := TRSPlungerOutlineColour;
  TRSPlungerOutlineColour := clBlack;

  SaveTRSPlungerPressedColourForPrinting := TRSPlungerPressedColour;
  TRSPlungerPressedColour := clBlack;

  Debug('Resetting screen colours');
END; { ResetScreenColoursBeforePrinting }

PROCEDURE TFWPRailWindow.FWPRailWindowStatusBarMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
{ Save the XY position of the cursor as it passes over a status bar to work out where it is should the mouse key be pressed }
BEGIN
  StatusBarX := X;
  StatusBarY := X;
END; { FWPRailWindowStatusBarMouseMove }

PROCEDURE TFWPRailWindow.FWPRailWindowStatusBarClick(Sender: TObject);
VAR
  Panel3X : Integer;

BEGIN
  { This is a work-around to find out if the cursor is within status bar Panel 0 as the proper test:
    "IF Sender = FWPRailWindow.FWPRailWindowStatusBar.Panels[n] THEN..." does not work.
  }
  IF (StatusBarX > 0) AND (StatusBarX <= FWPRailWindowStatusBar.Panels[StatusBarPanel0].Width) THEN
    GetTime.ClockWindow.Visible := True
  ELSE BEGIN
    { Work out where panel 3 is [not currently in use ] }
    WITH FWPRailWindowStatusBar DO BEGIN
      Panel3X := Panels[StatusBarPanel0].Width + Panels[StatusBarPanel1].Width + Panels[StatusBarPanel2].Width;
      IF (StatusBarX > 0) AND (StatusBarX >= Panel3X) THEN BEGIN
        null;
      END;
    END; {WITH}
  END;
END; { FWPRailWindowStatusBarClick }

PROCEDURE ResetFWPRailWindowSizeAndPosition;
{ Reset the window's size and position }
BEGIN
  FWPRailWindow.Height := MulDiv(Screen.WorkAreaHeight, 80, 100);
  FWPRailWindow.Width := Screen.WorkAreaWidth;
  FWPRailWindow.Top := 0;
  FWPRailWindow.Left := 0;
  InvalidateScreen(UnitRef, 'ResetFWPRailWindowSizeClick');
END; { ResetFWPRailWindowSizeAndPosition }

PROCEDURE ResetAllWindowsSizeAndPosition;
{ Reset all the windows to their default state }
BEGIN
  ResetFWPRailWindowSizeAndPosition;
  ResetEditWindowSizeAndPosition;
  ResetFWPRailWindowSizeAndPosition;
  ResetDiagramsWindowSizeAndPosition;
  ResetDebugWindowSizeAndPosition;
END; { ResetAllWindowsSizeAndPosition }

PROCEDURE TFWPRailWindow.ResetSizeAndPositionOfAllWindowsClick(Sender: TObject);
BEGIN
  ResetAllWindowsSizeAndPosition;
END; { ResetSizeAndPositionOfAllWindowsClick }

PROCEDURE TFWPRailWindow.FWPRailWindowMouseWheel(Sender: TObject; ShiftState: TShiftState; WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
BEGIN
  IF (LocoDialogueWindow <> NIL) AND (LocoDialogueWindow.Visible) THEN
    ControlSpeedByMouseWheel(WheelDelta, MousePos)
  ELSE
    IF FWPRailWindow.VertScrollBar.Visible THEN BEGIN
      IF WheelDelta > 0 THEN
        FWPRailWindow.VertScrollBar.Position := FWPRailWindow.VertScrollBar.Position - 25
      ELSE
        IF WheelDelta < 0 THEN
          FWPRailWindow.VertScrollBar.Position := FWPRailWindow.VertScrollBar.Position + 25;
    END;
END; { FWPRailWindowMouseWheel }

PROCEDURE CanvasTextOutAngle(X, Y : Integer; D : Word; S : String);
{ D is in tenths if a degree - i.e. 450 - 45 degrees. This is not used, but might come in useful }
VAR
  LogRec: TLOGFONT;     {* Storage area for font information *}
  OldFontHandle,        {* The old font handle *}
  NewFontHandle: HFONT; {* Temporary font handle *}

BEGIN
  IF Application.Terminated THEN
    Exit;

  WITH RailWindowBitmap.Canvas DO BEGIN
    { Get the current font information. We only want to modify the angle }
    GetObject(Font.Handle, SizeOf(LogRec), Addr(LogRec));
    { Modify the angle. "The angle, in tenths of a degrees, between the base line of a character and the x-axis." (Windows API Help) }
    LogRec.lfEscapement := D;
    { Create a new font handle using the modified old font handle }
    NewFontHandle := CreateFontIndirect(LogRec);
    { Save the old font handle! We have to put it back when we are done! }
    OldFontHandle := SelectObject(Handle, NewFontHandle);
    { Finally. Output the text! }
    Brush.Style := bsClear;
    TextOut(X, Y, S);
    { Put the font back the way we found it! }
    NewFontHandle := SelectObject(Handle, OldFontHandle);
    { Delete the temporary (NewFontHandle) that we created }
    DeleteObject(NewFontHandle);
  END; {WITH}
END; { CanvasTextOutAngle }

PROCEDURE HideStatusBarAndUpDownIndications;
{ Before a zoomed screen move, hide the status bar and the "up" and "down" markers }
BEGIN
  WITH FWPRailWindow DO BEGIN
    FWPRailWindowStatusBar.Visible := False;

    IF UpDownMarkersVisible THEN BEGIN
      UpDownMarkersVisible := False;

      WITH RailWindowBitmap.Canvas DO BEGIN
        Font.Color := BackgroundColour;
        Font.Height := -MulDiv(ClientHeight, FWPRailWindowFontHeight, 1000);
        TextOut(0, ClientHeight DIV 2, 'Up');
        TextOut(ClientWidth - TextWidth('Down'), ClientHeight DIV 2, 'Down');
      END; {WITH}
    END;
  END; {WITH}
END; { HideStatusBarAndUpDownIndications }

PROCEDURE ShowStatusBarAndUpDownIndications;
{ After a zoomed screen move, restore the status bar and the "up" and "down" markers }
BEGIN
  WITH FWPRailWindow DO BEGIN
    FWPRailWindowStatusBar.Visible := True;

    IF NOT UpDownMarkersVisible THEN BEGIN
      UpDownMarkersVisible := True;

      WITH RailWindowBitmap.Canvas DO BEGIN
        Font.Color := clWhite;
        Font.Height := -MulDiv(ClientHeight, FWPRailWindowFontHeight, 1000);
        TextOut(0, ClientHeight DIV 2, 'Up');
        TextOut(ClientWidth - TextWidth('Down'), ClientHeight DIV 2, 'Down');
      END; {WITH}
    END;
  END; {WITH}
END; { ShowStatusBarAndUpDownIndications }

PROCEDURE TFWPRailWindow.WMHScroll(VAR ScrollData: TMessage);
{ Added to allow interception of scroll bar events }
BEGIN
  INHERITED;

  WITH ScrollData DO BEGIN
    CASE WParamLo OF
      SB_ENDSCROLL:
        { Ends scroll }
        ShowStatusBarAndUpDownIndications;
      SB_LEFT:
        { Scrolls to the upper left }
        HideStatusBarAndUpDownIndications;
      SB_RIGHT:
        { Scrolls to the lower right }
        HideStatusBarAndUpDownIndications;
      SB_LINELEFT:
        { Scrolls left by one unit }
        HideStatusBarAndUpDownIndications;
      SB_LINERIGHT:
        { Scrolls right by one unit }
        HideStatusBarAndUpDownIndications;
      SB_PAGELEFT:
        { Scrolls left by the width of the window }
        HideStatusBarAndUpDownIndications;
      SB_PAGERIGHT:
        { Scrolls right by the width of the window }
        HideStatusBarAndUpDownIndications;
      SB_THUMBPOSITION:
        { The user has dragged the scroll box (thumb) and released the mouse button. The high-order word indicates the position of the scroll box at the end of the drag
          operation.
        };
      SB_THUMBTRACK:
        { The user is dragging the scroll box. This message is sent repeatedly until the user releases the mouse button. The high-order word indicates the position that
          the scroll box has been dragged to.
        }
        HideStatusBarAndUpDownIndications;
     END; {CASE}
   END; {WITH}
END; { WMHScroll }

PROCEDURE TFWPRailWindow.WMVScroll(VAR ScrollData: TMessage);
{ Added to allow interception of scroll bar events }
BEGIN
  INHERITED;

  WITH ScrollData DO BEGIN
    CASE WParamLo OF
      SB_BOTTOM:
        { Scrolls to the lower right }
        HideStatusBarAndUpDownIndications;
      SB_ENDSCROLL:
        { Ends scroll }
        ShowStatusBarAndUpDownIndications;
      SB_LINEDOWN:
        { Scrolls one line down }
        HideStatusBarAndUpDownIndications;
      SB_LINEUP:
        { Scrolls one line up }
        HideStatusBarAndUpDownIndications;
      SB_PAGEDOWN:
        { Scrolls one page down }
        HideStatusBarAndUpDownIndications;
      SB_PAGEUP:
        { Scrolls one page up }
        HideStatusBarAndUpDownIndications;
      SB_THUMBPOSITION:
        { The user has dragged the scroll box (thumb) and released the mouse button. The high-order word indicates the position of the scroll box at the end of the drag
          operation.
        };
      SB_THUMBTRACK:
        { The user is dragging the scroll box. This message is sent repeatedly until the user releases the mouse button. The high-order word indicates the position that
          the scroll box has been dragged to.
        }
        HideStatusBarAndUpDownIndications;
      SB_TOP:
        { Scrolls to the upper left }
        HideStatusBarAndUpDownIndications;
     END; {CASE}
   END; {WITH}
END; { WMVScroll }

PROCEDURE SetScreenMode;
{ Set the screen mode }
VAR
  WindowsTaskbar: HWND;

BEGIN
  WITH FWPRailWindow DO BEGIN
    SaveScreenMode := ScreenMode;
    CASE ScreenMode OF
      CustomWindowedScreenMode:
        BEGIN
          { If the screen has been restored to its normal size, restore the screen mode to default }
          IF (Height = MulDiv(Screen.WorkAreaHeight, 80, 100)) AND (Width = Screen.WorkAreaWidth) THEN BEGIN
            ScreenMode := DefaultWindowedScreenMode;
            WriteToStatusBarPanel(StatusBarPanel2, 'Screen set to default size');
            Log('A Main Window set to default size');
          END;
          ThinLineMode := True;
          Borderstyle := bsSizeable;
          IF NOT FWPRailWindowStatusBar.Visible THEN
            FWPRailWindowStatusBar.Show;
          Log('A Main Window set to user-defined size');

          IF WindowsTaskbarDisabled THEN BEGIN

            { Find handle of TASKBAR }
            WindowsTaskBar := FindWindow('Shell_TrayWnd', NIL);
            { Enable the taskbar }
            EnableWindow(WindowsTaskBar, True);
            { Show the taskbar }
            ShowWindow(WindowsTaskbar, SW_SHOW);

            WindowsTaskbarDisabled := False;
          END;
        END;
      DefaultWindowedScreenMode:
        BEGIN
          ThinLineMode := True;
          Borderstyle := bsSizeable;
          Height := MulDiv(Screen.WorkAreaHeight, 80, 100);
          IF NOT FWPRailWindowStatusBar.Visible THEN
            FWPRailWindowStatusBar.Show;
          Log('A Main Window set to default size');

          IF WindowsTaskbarDisabled THEN BEGIN
            { Find handle of TASKBAR }
            WindowsTaskBar := FindWindow('Shell_TrayWnd', NIL);
            { Enable the taskbar }
            EnableWindow(WindowsTaskBar, True);
            { Show the taskbar }
            ShowWindow(WindowsTaskbar, SW_SHOW);

            WindowsTaskbarDisabled := False;
          END;
        END;
      FullScreenMode:
        BEGIN
          IF LineThicknessInFullScreenMode = 'Thin' THEN
            ThinLineMode := True
          ELSE
            ThinLineMode := False;
          Borderstyle := bsNone;
          Height := Screen.DeskTopHeight;
          Top := 0;
          Left := 0;
          IF FWPRailWindowStatusBar.Visible THEN
            FWPRailWindowStatusBar.Hide;
          Log('A Main Window now full screen');
          Width := Screen.DeskTopWidth;

          IF NOT WindowsTaskbarDisabled THEN BEGIN
            { Find handle of TASKBAR }
            WindowsTaskBar := FindWindow('Shell_TrayWnd', NIL);
            { Disable the taskbar }
            EnableWindow(WindowsTaskBar, False);
            { Hide the taskbar }
            ShowWindow(WindowsTaskbar, SW_HIDE);

            WindowsTaskbarDisabled := True;
          END;
        END;
      FullScreenWithStatusBarMode:
        BEGIN
          { Use for checking track circuits. etc., as displays track-circuit number on the status bar }
          IF LineThicknessInFullScreenMode = 'Thin' THEN
            ThinLineMode := True
          ELSE
            ThinLineMode := False;
          Borderstyle := bsNone;
          Height := Screen.DeskTopHeight;
          IF NOT FWPRailWindowStatusBar.Visible THEN
            FWPRailWindowStatusBar.Show;
          Log('A Main Window now full screen with border');
        END;
    END; {CASE}
  END; {WITH}
END; { SetScreenMode }

PROCEDURE InitialiseRailDrawUnit;
{ Initialises the unit }
VAR
  DiagramsMissing : Boolean;
  DiagramsOK : Boolean;
  ErrorMsg : String;
  LocoDataTableOK : Boolean;
  WorkingTimetableMissing : Boolean;
  WorkingTimetableOK : Boolean;

BEGIN
  TRY
    WITH FWPRailWindow DO BEGIN
      ChangeCursor(crHourGlass);

//      { Intercept messages }
//      Application.OnMessage := ApplicationMessage;

      { initialize parameters }
      Screen.Cursors[crCrossHair] := LoadCursor(HInstance, 'CrossHair');
      Screen.Cursors[crCrossHairForUpSignal] := LoadCursor(HInstance, 'CrossHair-UpSignal');
      Screen.Cursors[crCrossHairForDownSignal] := LoadCursor(HInstance, 'CrossHair-DownSignal');
      Screen.Cursors[crPointLever] := LoadCursor(HInstance, 'PointLever');

      { Set up default window size }
      Position := poDesigned;

      Height := FWPRailWindowHeight;
      Width := FWPRailWindowWidth;
      Top := FWPRailWindowTop;
      Left := FWPRailWindowLeft;

      RailWindowBitmap := TBitmap.Create;
      RailWindowBitmap.Width := FWPRailWindowWidth;
      RailWindowBitmap.Height := FWPRailWindowHeight;

      ResizeMap := False;

      { Set up the status bar panels - the combined width should be 100% of the screen width }
      WITH FWPRailWindowStatusBar DO BEGIN
        Panels[StatusBarPanel0].Width := (MulDiv(Screen.WorkAreaWidth, 50, ZoomScaleFactor));
        Panels[StatusBarPanel1].Width := (MulDiv(Screen.WorkAreaWidth, 447, ZoomScaleFactor));
        Panels[StatusBarPanel2].Width := (MulDiv(Screen.WorkAreaWidth, 403, ZoomScaleFactor));
        Panels[StatusBarPanel3].Width := (MulDiv(Screen.WorkAreaWidth, 100, ZoomScaleFactor));
        { and right justify the third panel }
        Panels[StatusBarPanel3].Alignment := taRightJustify;
      END; {WITH}

      { Set up menus - hide them until they are requested }
      MainDropdownMenuDisplay.Visible := False;
      MainDropdownMenuFile.Visible := False;
      MainDropdownMenuHelp.Visible := False;

      InitialiseScreenDrawingVariables;

      FWPRailWindow.Visible := True;

      Randomize;
      ReadInAreasDataFromDatabase; { ************ problem for all file loading if registry entry empty - no prompt to find directory they're in - 21/1/14 }
      ReadInLocationDataFromDatabase;
      SetLength(LocationOccupations, Length(Locations));
      ReadInLocoDataFromDatabase(LocoDataTableOK);
      SetUpLineDrawingVars;

      { Acquiring the feedback has to precede acquiring signal, line and point data as we need to know which feedback units to assign points and signals to }
      ReadInFeedbackDataFromDatabase;
      ReadInLineDataFromDatabase;
      IF NOT TrackCircuitsInitialised THEN BEGIN
        { only initialise track circuit once, as doing so a second time removes the data **** }
        TrackCircuitsInitialised := True;
        ReadInTrackCircuitDataFromDatabase;
        CheckLineConnectionsAreOK;
      END;
      ReadInPointDataFromDatabase;
      IF PreviousPointSettingsMode THEN
        DisplayPreviousPointSettings;
      ReadInPlatformDataFromDatabase;
      ReadInSignalDataFromDatabase(NOT NewData);
      ReadInRouteingExceptionsFromDatabase;
      IF NOT LocationLinesInitialised THEN BEGIN
        Log('A INITIALISING LOCATION LINES {BLANKLINEBEFORE}');
        InitialiseLocationLines;
        LocationLinesInitialised := True;
      END;

      { Load feedback data and the diagrams datat and compare the data (these routines are here, as the various windows are created by this stage) }
      DiagramsCheckingInProgress := True;

      { but change the state of any that we know are out-of-use }
      ReadIniFileForTrackCircuitData;

      { and see if any are out of use because the detectors are out of use }
      NoteOutOfUseFeedbackUnitTrackCircuitsAtStartup;

      WorkingTimetableOK := False;
      DiagramsOK := False;
      IF NOT WorkingTimetableMode THEN
        Log('AG Starting without the working timetable - WorkingTimetableMode is not set {BLANKLINEBEFORE}')
      ELSE BEGIN
        LoadWorkingTimetableFromDatabase(WorkingTimetableMissing, WorkingTimetableOK);
        IF NOT WorkingTimetableMissing AND WorkingTimetableOK THEN BEGIN
          ProcessWorkingTimetable;
          ProcessDiagrams(ErrorMsg, DiagramsOK);
        END;
      END;

      IF WorkingTimetableOK THEN
        Log('AG Working timetable loaded - so starting without loading the diagrams from disc {BLANKLINEBEFORE}')
      ELSE
        IF NOT StartWithDiagrams THEN BEGIN
          Log('AG Starting without the diagrams loaded - StartWithDiagrams is not set {BLANKLINEBEFORE}');
          CheckOccupiedLinesAndDiagrams;
        END ELSE BEGIN
          InitialiseDiagramsUnit;
          ReadInDiagramsFromDatabase(ErrorMsg, DiagramsMissing, DiagramsOK);
          IF DiagramsOK AND NOT DiagramsMissing THEN
            ProcessDiagrams(ErrorMsg, DiagramsOK);
          IF NOT DiagramsOK THEN BEGIN
            IF MessageDialogueWithDefault('The diagrams file ''' + DiagramsFilename + '.' + DiagramsFilenameSuffix
                                          + IfThen(DiagramsMissing,
                                                   ' cannot be found',
                                                   ''' has errors:')
                                          + CRLF
                                          + '"' + ErrorMsg + '"'
                                          + CRLF
                                          + 'Do you wish to start without it or exit the program?',
                                          StopTimer, mtError, [mbYes, mbNo], ['&Start', '&Exit'], mbNo)
                                          = mrYes
            THEN BEGIN
              Log('XG Diagrams are faulty and have not been loaded, but starting without them');
              DiagramsLoaded := False;
              InvalidateScreen(UnitRef, 'DrawMap');
            END ELSE BEGIN
              Log('X Exiting - Diagrams are faulty');
              ShutDownProgram(UnitRef, 'DrawMap');
            END;
          END ELSE
            Log('A Diagrams loaded - StartWithoutDiagrams is not set');
        END;

      DiagramsCheckingInProgress := False;

      ChangeCursor(crDefault);
      SetScreenMode;
      ProgramStarting := False;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG FWPRailWindowCreate:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { InitialiseRailDrawUnit }

PROCEDURE DrawMap;
{ Draws the track layout }
CONST
  ForceDraw = True;
  ShowNums = True;
  ShowTheatreDestinationChar = True;
  StartOfLine = True;
  Up = True;

VAR
  AdjacentDownTC : Integer;
  AdjacentUpTC : Integer;
  B, I, J : Integer;
  ShowPointNum : Boolean;
  Line, Line2 : Integer;
  LinesArray : IntegerArrayType;
  P : Integer;
  S : Integer;
  SaveLineOldColour : Integer;
  SaveRecordLineDrawingMode : Boolean;
  ScreenDownX : Integer;
  ScreenDownY : Integer;
  ScreenUpX : Integer;
  ScreenUpY : Integer;
  ShowArea : Boolean;
  SegmentText : String;
  TempLocationYArray : IntegerArrayType;

  PROCEDURE DrawAllBufferStopData(ShowSignalAndBufferStopNums, ShowTheatreDestinations : Boolean);
  { Draw all the buffer stops }
  VAR
    B : Integer;

  BEGIN
    TRY
      { Draw the bufferstops }
      FOR B := 0 TO High(BufferStops) DO BEGIN
        WITH BufferStops[B] DO BEGIN
          IF ShowSignalAndBufferStopNums THEN
            DrawBufferStopData(B, IntToStr(BufferStop_Number), BufferStopNumberColour)
          ELSE
            IF ShowTheatreDestinations THEN
              DrawBufferStopData(B, BufferStops[B].BufferStop_AsTheatreDestination, BufferStopNumberColour);
        END;
      END;
    EXCEPT
      ON E : Exception DO
        Log('EG DrawAllBufferStopData:' + E.ClassName + ' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { DrawAllBufferStopData }

(* Zarko Gajic's Full Screen code (at: http://delphi.about.com/od/delphitips2010/qt/delphi-application-full-screen-mode-f11.htm)

  {$J+} //writeable constants on
  const
    rect: TRect = (Left:0; Top:0; Right:0; Bottom:0);
    ws : TWindowState = wsNormal;
  {$J-} //writeable constants off
  var
    r : TRect;
  begin
    if BorderStyle <> bsNone then
    begin
      ws := WindowState;
      rect := BoundsRect;

      BorderStyle := bsNone;
      r := Screen.MonitorFromWindow(Handle).BoundsRect;
      SetBounds(r.Left, r.Top, r.Right-r.Left, r.Bottom-r.Top) ;
    end
    else
    begin
      BorderStyle := bsSizeable;
      if ws = wsMaximized then
        WindowState := wsMaximized
      else
        SetBounds(rect.Left, rect.Top, rect.Right-rect.Left, rect.Bottom-rect.Top) ;
    end;
  end;
*)

BEGIN { Main drawing procedure }
  TRY
    // RailWindowBitmap.Canvas.FillRect(RailWindowBitmap.Canvas.ClipRect);
    WITH FWPRailWindow DO BEGIN
      WITH RailWindowBitmap.Canvas DO BEGIN
        { Do not record the line drawing detail each time DrawMap is called }
        SaveRecordLineDrawingMode := InRecordLineDrawingMode;

        IF ScreenMode <> SaveScreenMode THEN
          SetScreenMode;

        RailWindowBitmapCanvasPenWidth := Canvas.Pen.Width;

// if testregion then begin
//   Region := CreateRectRgn(220, 240, 320, 300);
//   SelectClipRgn(Canvas.Handle, Region);
// end;

        IF NOT Zooming THEN BEGIN
          HorzScrollBar.Visible := False;
          VertScrollBar.Visible := False;
          HorzScrollBar.Range := 0;
          VertScrollBar.Range := 0;
        END ELSE BEGIN
          { The level of zoom has changed }
          IF ZoomScaleFactor <> SaveZoomScaleFactor THEN BEGIN
            SaveZoomScaleFactor := ZoomScaleFactor;
            HorzScrollBar.Visible := True;
            VertScrollBar.Visible := True;
            HorzScrollBar.Range := MulDiv(1000, ClientWidth, ZoomScaleFactor);
            VertScrollBar.Range := MulDiv(1000, ClientHeight, ZoomScaleFactor);

            { If there's a zoom rectangle, centre the enlarged picture on it }
            WITH GetZoomRect DO BEGIN
              IF (Left <> 0) OR (Top <> 0) OR (Right <> 0) OR (Bottom <> 0) THEN BEGIN
                HorzScrollBar.Position := MulDiv(1000, Left - ((Right - Left) DIV 2), ZoomScaleFactor);
                VertScrollBar.Position := MulDiv(1000, Top - ((Bottom - Top) DIV 2), ZoomScaleFactor);

                SetZoomRect(Rect(0, 0, 0, 0));
              END;
            END; {WITH}
          END;
        END;

        ScrollBarXAdjustment := HorzScrollBar.Position;
        ScrollBarYAdjustment := VertScrollBar.Position;

        { Set the background colour of the form }
        Brush.Color := BackgroundColour;
        FillRect(Rect(0, 0, ClientWidth, ClientHeight));

        { Position the status bar allowing for the window scrolling }
        FWPRailWindowStatusBar.Left := 0;
        FWPRailWindowStatusBar.Top := ClientHeight - FWPRailWindowStatusBar.Height;
        FWPRailWindowStatusBar.Width := ClientWidth;

        IF ResizeMap OR ReinitialiseFWPRailWindowVariables THEN BEGIN
          SetUpLineDrawingVars;
          CalculateLinePositions;
          CalculatePointPositions;
          CalculatePlatformPositions;
          CalculateAllSignalPositions;

          IF ReinitialiseFWPRailWindowVariables THEN
            ReinitialiseFWPRailWindowVariables := False;
        END;

        { Draw the Y grid where lines could be created. This procedure has to be executed before the line drawing routines, as otherwise these temporary lines would
          overwrite the real lines representing tracks.
        }
        IF CreateLineMode THEN BEGIN
          SetLength(TempLocationYArray, 0);
          Pen.Color := clFWPDkBlue;
          J := 0;
          FOR I := 1 TO WindowRows DO BEGIN
            J := J + InterLineSpacing;
            MoveTo(0, J);
            LineTo(ClientWidth, J);
            TextOut(0, J, IntToStr(I));
          END; {FOR}
        END;

        { Draw the individual lines }
        FOR Line := 0 TO High(Lines) DO BEGIN
          WITH Lines[Line] DO BEGIN
            { save the LineOldColour because otherwise DrawLine might update it erroneously }
            SaveLineOldColour := Line_OldColour;

            IF ReplayMode THEN
              DrawLine(Line, Line_OldColour, ActiveTrain)
            ELSE BEGIN
              { If the line is not associated with a track circuit }
              IF Line_TC = UnknownTrackCircuit THEN BEGIN
                IF Line_RouteSet <> UnknownRoute THEN
                  Line_CurrentColour := LineRoutedOverColour
                ELSE
                  Line_CurrentColour := SaveLineOldColour;
                DrawLine(Line, Line_CurrentColour, ActiveTrain);
              END ELSE BEGIN
                { LineTC <> UnknownTrackCircuit - see if it's an occupied track circuit etc. If in auto mode, only highlight the bits that are routed over. }
                IF NOT DisplayFlashingTrackCircuits OR TrackCircuits[Line_TC].TC_LitUp THEN
                  Line_CurrentColour := GetTrackCircuitStateColour(Line_TC)
                ELSE
                  Line_CurrentColour := BackgroundColour;

                IF Line_CurrentColour = TCUnoccupiedColour THEN BEGIN
                  { if not, see if a subroute is set over the line }
                  IF Line_RouteSet <> UnknownRoute THEN
                    Line_CurrentColour := LineRoutedOverColour
                END;
              END;

              IF (EditedTrackCircuit <> UnknownTrackCircuit) AND (Lines[Line].Line_TC = EditedTrackCircuit) THEN
                DrawLine(Line, ScreenComponentEditedColour2, ActiveTrain)
              ELSE
                IF Line = EditedLine THEN
                  DrawLine(Line, ScreenComponentEditedColour1, ActiveTrain)
                ELSE
                  DrawLine(Line, Line_CurrentColour, ActiveTrain);

              { Draw a rectangle around any line highlighted by the input procedure }
              IF LineHighlighted <> UnknownLine THEN
                WITH Lines[LineHighlighted] DO
                  DrawOutline(Line_MousePolygon, clWhite, NOT UndrawRequired, NOT UndrawToBeAutomatic);

              { Draw a rectangle around any track circuit highlighted by the input procedure }
              IF TrackCircuitHighlighted <> UnknownLine THEN
                LinesArray := GetLinesForTrackCircuit(TrackCircuitHighlighted);
                Line2 := 0;
                WHILE Line2 <= High(LinesArray) DO BEGIN
                  DrawOutline(Lines[LinesArray[Line2]].Line_MousePolygon, clWhite, NOT UndrawRequired, NOT UndrawToBeAutomatic);
                  Inc(Line2);
                END; {WHILE}

                IF InShowAdjacentTrackCircuitMode THEN BEGIN
                  FindAdjoiningTrackCircuits(TrackCircuitHighlighted, AdjacentUpTC, AdjacentDownTC);
                  LinesArray := GetLinesForTrackCircuit(AdjacentUpTC);
                  Line2 := 0;
                  WHILE Line2 <= High(LinesArray) DO BEGIN
                    DrawOutline(Lines[LinesArray[Line2]].Line_MousePolygon, clAqua, NOT UndrawRequired, NOT UndrawToBeAutomatic);
                    Inc(Line2);
                  END; {WHILE}

                  FindAdjoiningTrackCircuits(TrackCircuitHighlighted, AdjacentUpTC, AdjacentDownTC);
                  LinesArray := GetLinesForTrackCircuit(AdjacentDownTC);
                  Line2 := 0;
                  WHILE Line2 <= High(LinesArray) DO BEGIN
                    DrawOutline(Lines[LinesArray[Line2]].Line_MousePolygon, clYellow, NOT UndrawRequired, NOT UndrawToBeAutomatic);
                    Inc(Line2);
                  END; {WHILE}
                END;
            END;

            { And restore the saved colour }
            Line_OldColour := SaveLineOldColour;
          END; {WITH}
        END; {FOR}

        { Draw any bufferstops }
        FOR B := 0 TO High(BufferStops) DO
          DrawBufferStop(B, BufferStops[B].BufferStop_CurrentColour);

        { Draw the platforms }
        DrawPlatforms;
        // SelectClipRgn(Canvas.Handle, 0);
        // DeleteObject(Region);

        DrawSpeedRestrictions;

        { now some debugging stuff }
        IF ShowSignalAndBufferStopNums THEN BEGIN
          DrawAllSignals(ShowNums, NOT ShowTheatreDestinationChar);
          DrawAllBufferStopData(ShowNums, NOT ShowTheatreDestinationChar);
        END ELSE
          IF ShowSignalJunctionDestinations THEN BEGIN
            FOR S := 0 TO High(Signals) DO BEGIN
              IF Signals[S].Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_Exists
              OR Signals[S].Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists
              OR Signals[S].Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_Exists
              OR Signals[S].Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_Exists
              OR Signals[S].Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists
              OR Signals[S].Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_Exists
              THEN BEGIN
                Signals[S].Signal_PostColour := clLime;
                DrawSignalPost(S);
                DrawAllSignals(NOT ShowNums, NOT ShowTheatreDestinationChar);

                SaveShowSignalJunctionDestinations := True;
              END;
            END;
          END ELSE
            IF SaveShowSignalJunctionDestinations THEN BEGIN
              { reset the signal post colours }
              FOR S := 0 TO High(Signals) DO BEGIN
                Signals[S].Signal_PostColour := DefaultSignalPostColour;
                DrawSignalPost(S);
                DrawAllSignals(NOT ShowNums, NOT ShowTheatreDestinationChar);
              END; {FOR}

              SaveShowSignalJunctionDestinations := False;
            END ELSE
              IF ShowSignalsWithAdjacentTrackCircuits THEN BEGIN
                DrawAllSignals(ShowNums, NOT ShowTheatreDestinationChar);
                FOR S := 0 TO High(Signals) DO BEGIN
                  IF NOT Signals[S].Signal_OutOfUse THEN BEGIN
                    { needs fixing - leaves TCs with signal numbers after exit **** }
                    { also needs headcodes not loco numbers (F10) turned on *** }
                    IF Signals[S].Signal_AdjacentTC <> UnknownTrackCircuit THEN
                      DrawTrackCircuit(Signals[S].Signal_AdjacentTC, clLime);
                    IF Signals[S].Signal_AdjacentTC <> UnknownTrackCircuit THEN
                      Log('X S=' + IntToStr(S) + ' TC=' + IntToStr(Signals[S].Signal_AdjacentTC));
                  END;
                END; {FOR}
              END ELSE
                IF ShowSignalsAndBufferStopsWithTheatreDestinations THEN BEGIN
                  DrawAllSignals(NOT ShowNums, ShowTheatreDestinationChar);
                  DrawAllBufferStopData(NOT ShowNums, ShowTheatreDestinationChar);
                END ELSE
                  IF ShowSignalsWhereRouteingCanBeHeldAndThoseNotUsedForRouteing THEN BEGIN
                    FOR S := 0 TO High(Signals) DO BEGIN
                      ShowArea := False;

                      IF Signals[S].Signal_PossibleStationStartRouteHold THEN BEGIN
                        Signals[S].Signal_PostColour := clLime;
                        ShowArea := True;
                      END ELSE
                        IF Signals[S].Signal_PossibleRouteHold THEN BEGIN
                          Signals[S].Signal_PostColour := clYellow;
                          ShowArea := True;
                        END ELSE
                          IF Signals[S].Signal_NotUsedForRouteing THEN
                            Signals[S].Signal_PostColour := clAqua;

                      DrawSignal(S);

                      { Also draw the associated area }
                      IF ShowArea THEN BEGIN
                        FOR Line := 0 TO High(Lines) DO BEGIN
                          SegmentText := '';
                          WITH Lines[Line] DO BEGIN
                            IF GetLineAdjacentSignal(Line) = S THEN BEGIN
                              Font.Color := clLime;
                              IF Lines[Line].Line_Location <> UnknownLocation THEN BEGIN
                                IF Locations[Line_Location].Location_Area <> UnknownArea THEN
                                  SegmentText := AreaToStr(Locations[Line_Location].Location_Area, ShortStringType);
                              END ELSE
                                SegmentText := '?';

                              IF SegmentText <> '' THEN BEGIN
                                ScreenUpX := MapGridXToScreenX(Line_GridUpX);
                                ScreenUpY := MapGridYToScreenY(Line_GridUpY);
                                ScreenDownX := MapGridXToScreenX(Line_GridDownX);
                                ScreenDownY := MapGridYToScreenY(Line_GridDownY);
                                TextOut(ScreenUpX + (ScreenDownX - ScreenUpX) DIV 2 - TextWidth(SegmentText) DIV 2 - ScrollBarXAdjustment,
                                       (ScreenUpY + (ScreenDownY - ScreenUpY) DIV 2) - TextHeight(SegmentText) DIV 2 - ScrollBarYAdjustment,
                                        SegmentText);
                              END;
                            END;
                          END; {WITH}
                        END; {FOR}
                      END;
                    END;
                  END ELSE BEGIN
                    { the main calling point }
                    DrawAllSignals(NOT ShowNums, NOT ShowTheatreDestinationChar);
                    DrawAllBufferStopData(NOT ShowNums, NOT ShowTheatreDestinationChar);
                  END;

        IF ShowLinesWhichLockPoints THEN BEGIN
          FOR P := 0 To High(Points) DO BEGIN
            ShowPointNum := False;

            IF Points[P].Point_LockedIfHeelTCOccupied THEN BEGIN
              IF (Points[P].Point_HeelLine <> UnknownLine) AND (Lines[Points[P].Point_HeelLine].Line_TC <> UnknownTrackCircuit) THEN BEGIN
                DrawLine(Points[P].Point_HeelLine, clYellow, NOT ActiveTrain);
                ShowPointNum := True;
              END;
            END;

            IF Points[P].Point_LockedIfNonHeelTCsOccupied THEN BEGIN
              IF (Points[P].Point_StraightLine <> UnknownLine) AND (Lines[Points[P].Point_StraightLine].Line_TC <> UnknownTrackCircuit) THEN BEGIN
                DrawLine(Points[P].Point_StraightLine, clFWPPink, NOT ActiveTrain);
                ShowPointNum := True;
              END;
            END;

            IF Points[P].Point_LockedIfNonHeelTCsOccupied THEN BEGIN
              IF (Points[P].Point_DivergingLine <> UnknownLine) AND (Lines[Points[P].Point_DivergingLine].Line_TC <> UnknownTrackCircuit) THEN BEGIN
                DrawLine(Points[P].Point_DivergingLine, clAqua, NOT ActiveTrain);
                ShowPointNum := True;
              END;
            END;

            IF ShowPointNum THEN
              DrawPointNum(P, clWhite);
          END; {FOR}
        END;
    //    FOR Location := FirstMainPlatformLocation TO LastMainPlatformLocation DO
    //      IF MainPlatformPlungers[Location].TRSPlunger_Present THEN
    //        DrawTRSPlunger(Location, MainPlatformPlungers[Location].TRSPlunger_Pressed);

        { Write the directions on screen }
        Brush.Color := BackgroundColour;
        IF ScreenColoursSetForPrinting THEN
          Font.Color := clBlack
        ELSE
          Font.Color := clWhite;

        { Keep the font size for the "up" and "down" screen guidance at the minimum size }
        Font.Height := -MulDiv(FWPRailWindow.ClientHeight, FWPRailWindowFontHeight, 1000);
        IF UpDownMarkersVisible THEN BEGIN
          TextOut(0, ClientHeight DIV 2, 'Up');
          TextOut(ClientWidth - TextWidth('Down'), ClientHeight DIV 2, 'Down');
        END;

        { Finally, some test detail }
        IF ShowAreas
        OR ShowLineDetail
        OR ShowLineNumbers
        OR ShowLinesUpXAbsoluteValue
        OR ShowLinesWhichLockPoints
        OR ShowLineDirectionDetail
        OR ShowLocationLengthDetail
        OR ShowLineGradients
        OR ShowLinesWithoutTrackCircuits
        OR ShowLocations
        OR ShowTrackCircuitLengths
        OR ShowTrackCircuits
        OR ShowTrackCircuitsRoutedOver
        OR ShowTrackCircuitFeedbackDataInUse
        OR ShowTrackCircuitFeedbackDataInSeparateColours
        OR ShowOneFeedbackUnitOnly
        THEN BEGIN
          InitialiseScreenDrawingVariables;
          Font.Style := [fsBold];
          { Show how line is subdivided into sectors }
          ShowLinePos;
          { interesting piece of history in the next comment: (fwp 3/11/01) }
          { a version of this was printed (with Colour turned off in Startup.pas) using WP Grab /m then edited and colours inverted in WP51 on 5/9/90 }
          { Grid for test purposes }
          Pen.Color := clWhite;
          FOR I := 0 TO 10 DO BEGIN
            FOR J := 0 TO 10 DO BEGIN
              MoveTo(MulDiv(I, ClientWidth, 10) - ScrollBarXAdjustment,
                     MulDiv(J, ClientHeight, 10) - ScrollBarYAdjustment);
              LineTo(MulDiv(I, ClientWidth, 10) + 1 - ScrollBarXAdjustment,
                     MulDiv(J, ClientHeight, 10) + 1 - ScrollBarYAdjustment);
            END;
          END; {FOR}
        END;

        IF NOT ShowTrackCircuits AND NOT ShowLineDetail AND NOT ShowLineNumbers AND NOT ShowLinesUpXAbsoluteValue THEN
          DrawAllPoints;

        IF ResizeMap THEN
          ResizeMap := False;

        SetMode(RecordLineDrawing, SaveRecordLineDrawingMode);
      END; {WITH Canvas}

      IF MenusVisible THEN
        ShowMenus
      ELSE
        HideMenus;

      IF FWPRailWindow.Visible = False THEN
        FWPRailWindow.Visible := True;
    END; { WITH FWPRailWindow }
  EXCEPT
    ON E : Exception DO
      Log('EG DrawMap: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DrawMap }

PROCEDURE TFWPRailWindow.WMCopyData(VAR Msg: TWMCopyData);
{ Receives data from the Watchdog program. (This code is here as the RailDraw window is the de facto main window visible to other programs). }
VAR
  S : String;

BEGIN
  SetString(S, PChar(Msg.CopyDataStruct.lpData), Msg.CopyDataStruct.cbData DIV SizeOf(Char));
  Log('XG FWPRail Watchdog message: "' + S + '"');

  { And send an acknowledgment }
  Msg.Result := 2;
END; { WMCopyData }

INITIALIZATION

END { RailDraw }.
