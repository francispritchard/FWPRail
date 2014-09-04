UNIT Raildraw;
{ Handles graphics display for railway program

  Copyright © F.W. Pritchard 1988-2014. All Rights Reserved.

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
    ChangePoint: TMenuItem;
    ChangeSignal: TMenuItem;
    FWPRailApplicationEvents: TApplicationEvents;
    FWPRailWindowColourDialogue: TColorDialog;
    FWPRailWindowMenu: TMainMenu;
    FWPRailWindowPopupOpenDialogue: TOpenDialog;
    FWPRailWindowStatusBar: TStatusBar;
    GeneralPopupBackgroundColour: TMenuItem;
    GeneralPopupBufferStopColours: TMenuItem;
    GeneralPopupChangeBackgroundColour: TMenuItem;
    GeneralPopupChangeColours: TMenuItem;
    GeneralPopupChangeDefaultPointColour: TMenuItem;
    GeneralPopupChangeForegroundColour: TMenuItem;
    GeneralPopupChangePenStyles: TMenuItem;
    GeneralPopupChangePoint: TMenuItem;
    GeneralPopupChangePointLockedBySystemColour: TMenuItem;
    GeneralPopupChangePointLockedByUserColour: TMenuItem;
    GeneralPopupChangePointManualOperationColour: TMenuItem;
    GeneralPopupChangePointOutOfUseColour: TMenuItem;
    GeneralPopupChangePointUndrawColour: TMenuItem;
    GeneralPopupChangeSidingPenStyle: TMenuItem;
    GeneralPopupChangeSignalAspectUnlitColour: TMenuItem;
    GeneralPopupChangeSignalPostRouteSettingColour: TMenuItem;
    GeneralPopupChangeTCFeedbackDataInUseColour: TMenuItem;
    GeneralPopupChangeTCFeedbackDataOutOfUseColour: TMenuItem;
    GeneralPopupChangeTCFeedbackOccupationColour: TMenuItem;
    GeneralPopupChangeTCUserMustDriveColour: TMenuItem;
    GeneralPopupChangeTCOutOfUseSetByUserColour: TMenuItem;
    GeneralPopupChangeTCPermanentFeedbackOccupationColour: TMenuItem;
    GeneralPopupChangeTCPermanentOccupationSetByUserColour: TMenuItem;
    GeneralPopupChangeTCPermanentOccupationSetByUserPenStyle: TMenuItem;
    GeneralPopupChangeTCPermanentSystemOccupationColour: TMenuItem;
    GeneralPopupChangeTCSpeedRestrictionColour: TMenuItem;
    GeneralPopupClock: TMenuItem;
    GeneralPopupDebugOptions: TMenuItem;
    GeneralPopupDefaultLineColourTCUnoccupiedColourMessage: TMenuItem;
    GeneralPopupDefaultPointColours: TMenuItem;
    GeneralPopupForegroundColour: TMenuItem;
    GeneralPopupLineColours: TMenuItem;
    GeneralPopupLineRoutedOverColour: TMenuItem;
    GeneralPopupListLocomotives: TMenuItem;
    GeneralPopupMenu: TPopupMenu;
    GeneralPopupOperations: TMenuItem;
    GeneralPopupPlatformColour: TMenuItem;
    GeneralPopupPlungerColour: TMenuItem;
    GeneralPopupPlungerOutlineColour: TMenuItem;
    GeneralPopupPointColours: TMenuItem;
    GeneralPopupPointFeedbackDataInUseColour: TMenuItem;
    GeneralPopupPointLockedBySystemColour: TMenuItem;
    GeneralPopupPointLockedByUserColour: TMenuItem;
    GeneralPopupPointOutOfUseColour: TMenuItem;
    GeneralPopupPointUndrawColour: TMenuItem;
    GeneralPopupResetFWPRailWindowSizeAndPosition: TMenuItem;
    GeneralPopupRestoreAllProgramDefaultSettings: TMenuItem;
    GeneralPopupRestoreAllScreenDrawingDefaultSettings: TMenuItem;
    GeneralPopupRestoreDefaultBackgroundColour: TMenuItem;
    GeneralPopupRestoreForegroundColour: TMenuItem;
    GeneralPopupRestorePointDefaultColour: TMenuItem;
    GeneralPopupRestorePointLockedBySystemColour: TMenuItem;
    GeneralPopupRestorePointLockedByUserColour: TMenuItem;
    GeneralPopupRestorePointOutOfUseColour: TMenuItem;
    GeneralPopupRestorePointUndrawColour: TMenuItem;
    GeneralPopupRestoreSidingPenStyle: TMenuItem;
    GeneralPopupRestoreSignalAspectUnlitColour: TMenuItem;
    GeneralPopupRestoreSignalPostRouteSettingColour: TMenuItem;
    GeneralPopupRestoreTCFeedbackDataInUseColour: TMenuItem;
    GeneralPopupRestoreTCFeedbackDataOutOfUseColour: TMenuItem;
    GeneralPopupRestoreTCFeedbackOccupationColour: TMenuItem;
    GeneralPopupRestoreTCUserMustDriveColour: TMenuItem;
    GeneralPopupRestoreTCOutOfUseSetByUserColour: TMenuItem;
    GeneralPopupRestoreTCPermanentFeedbackOccupationColour: TMenuItem;
    GeneralPopupRestoreTCPermanentOccupationSetByUserColour: TMenuItem;
    GeneralPopupRestoreTCPermanentOccupationSetByUserPenStyle: TMenuItem;
    GeneralPopupRestoreTCPermanentSystemOccupationColour: TMenuItem;
    GeneralPopupRestoreTCSpeedRestrictionColour: TMenuItem;
    GeneralPopupRuler5: TMenuItem;
    GeneralPopupRunClockFaster: TMenuItem;
    GeneralPopupRunClockFastest: TMenuItem;
    GeneralPopupRunClockNormally: TMenuItem;
    GeneralPopupRunClockSlower: TMenuItem;
    GeneralPopupSetCurrentRailwayDayOfTheWeek: TMenuItem;
    GeneralPopupSetCurrentRailwayTime: TMenuItem;
    GeneralPopupSetDaylightEndTime: TMenuItem;
    GeneralPopupSetDaylightStartTime: TMenuItem;
    GeneralPopupSetLogFileMaximumWidth: TMenuItem;
    GeneralPopupSetProgramStartTime: TMenuItem;
    GeneralPopupShowMainMenu: TMenuItem;
    GeneralPopupShowTrackcircuit: TMenuItem;
    GeneralPopupSidingPenStyle: TMenuItem;
    GeneralPopupSidingPenStyleSolid: TMenuItem;
    GeneralPopupSignalPostRouteSettingColour: TMenuItem;
    GeneralPopupStartClock: TMenuItem;
    GeneralPopupStopClock: TMenuItem;
    GeneralPopupTCFeedbackDataInUseColour: TMenuItem;
    GeneralPopupTCFeedbackDataOutOfUseColour: TMenuItem;
    GeneralPopupTCFeedbackOccupationColour: TMenuItem;
    GeneralPopupTCUserMustDriveColour: TMenuItem;
    GeneralPopupTCOutOfUseSetByUserColour: TMenuItem;
    GeneralPopupTCPermanentFeedbackOccupationColour: TMenuItem;
    GeneralPopupTCPermanentOccupationSetByUserColour: TMenuItem;
    GeneralPopupTCPermanentOccupationSetByUserPenStyle: TMenuItem;
    GeneralPopupTCPermanentSystemOccupation: TMenuItem;
    GeneralPopupTCSpeedRestrictionColour: TMenuItem;
    GeneralPopupTrainColours: TMenuItem;
    LinePopupMenu: TPopupMenu;
    MainClockMenu: TMenuItem;
    MainClockMenuRunClockFastest: TMenuItem;
    MainClockMenuRunTimeFaster: TMenuItem;
    MainClockMenuRunTimeNormally: TMenuItem;
    MainClockMenuRunTimeSlower: TMenuItem;
    MainClockMenuSetCurrentRailwayTime: TMenuItem;
    MainClockMenuSetStartupTime: TMenuItem;
    MainClockMenuStartClock: TMenuItem;
    MainDisplayMenu: TMenuItem;
    MainDisplayMenuDebug: TMenuItem;
    MainDisplayMenuDiagramsWindow: TMenuItem;
    MainDisplayMenuShow: TMenuItem;
    MainDisplayMenuShowStatusbar: TMenuItem;
    MainDisplayMenuWorkingTimetableWindow: TMenuItem;
    MainDisplayMenuZoom: TMenuItem;
    MainFileMenu: TMenuItem;
    MainFileMenuExit: TMenuItem;
    MainHelpMenu: TMenuItem;
    MainHelpMenuAboutRail: TMenuItem;
    MainHelpMenuRailHelp: TMenuItem;
    MainMenuStopClock: TMenuItem;
    MainOperationsMenu: TMenuItem;
    MainOperationsMenuChangeSignal: TMenuItem;
    MainOperationsMenuDebugOptions: TMenuItem;
    MainOperationsMenuListLocomotives: TMenuItem;
    MainOperationsMenuShowTrackCircuit: TMenuItem;
    MainRunMenu: TMenuItem;
    MainRunMenuHaltOperations: TMenuItem;
    MainRunMenuResumeOperations: TMenuItem;
    PointPopupMenu: TPopupMenu;
    RestorePointFeedbackDataInUseColour: TMenuItem;
    SetDaylightEnd: TMenuItem;
    SetDayLightStart: TMenuItem;
    SignalPopupMenu: TPopupMenu;
    LineRoutedOverColour1: TMenuItem;
    RestoreLineRoutedOverColour2: TMenuItem;
    ChangeLineRoutedOverColour2: TMenuItem;
    GeneralPopupScreenComponentEditedColour: TMenuItem;
    GeneralPopupChangeScreenComponentEditedColour: TMenuItem;
    GeneralPopupRestoreScreenComponentEditedColour: TMenuItem;

    PROCEDURE BufferStopPopupItemClick(Sender: TObject);
    PROCEDURE BufferStopMenuOnPopup(Sender: TObject);
    PROCEDURE FlashTimerTick(Sender: TObject);
    PROCEDURE FWPRailApplicationEventsShortCut(VAR Msg: TWMKey; VAR Handled: Boolean);
    PROCEDURE FWPRailWindowCreate(Sender: TObject);
    PROCEDURE FWPRailWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE FWPRailWindowDestroy(Sender: TObject);
    PROCEDURE FWPRailWindowDragDrop(Sender, Source: TObject; X, Y: Integer);
    PROCEDURE FWPRailWindowDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    PROCEDURE FWPRailWindowExitClick(Sender: TObject);
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
    PROCEDURE GeneralPopupChangeBackgroundColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeBufferStopColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeBufferStopNumberColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeBufferStopRedClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeDefaultPointColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeForegroundColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointLenzNumberColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeLineNotAvailableColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeLineRoutedOverColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeLocoStalledColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePlatformColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePlungerColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePlungerOutlineColour(Sender: TObject);
    PROCEDURE GeneralPopupChangePlungerPressedColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointDivergingLineColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointDownFacingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointFeedbackDataInUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointFeedbackDataOutOfUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointHeelLineColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointLockedBySystemColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointLockedByUserColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointManualOperationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointOutOfUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointStraightLineColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointsWithoutFeedbackColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointUndrawColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointUpFacingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeScreenComponentEditedColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeShowPointDefaultStateColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeSignalAspectGreenClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeSignalAspectRedClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeSignalAspectUnlitColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeSignalAspectYellowClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeSignalClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeSignalNumberColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeSignalPostEmergencyRouteSettingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeSignalPostRouteSettingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeSignalPostTheatreSettingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCFeedbackDataInUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCFeedbackDataOutOfUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCFeedbackOccupationButOutOfUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCFeedbackOccupationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCLocoOutOfPlaceColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCMissingOccupationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCOutOfUseAsNoFeedbackReceivedColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCOutOfUseSetByUserColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCPermanentFeedbackOccupationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCPermanentOccupationSetByUserColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCPermanentSystemOccupationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCSpeedRestrictionColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCSystemOccupationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCUnoccupiedColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTCUserMustDriveColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTrainActiveColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeTrainInactiveColourClick(Sender: TObject);
    PROCEDURE GeneralPopupClockClick(Sender: TObject);
    PROCEDURE GeneralPopupDebugOptionsClick(Sender: TObject);
    PROCEDURE GeneralPopupFiddleyardLinePenStyleClearClick(Sender: TObject);
    PROCEDURE GeneralPopupFiddleyardLinePenStyleDashClick(Sender: TObject);
    PROCEDURE GeneralPopupFiddleyardLinePenStyleDashDotClick(Sender: TObject);
    PROCEDURE GeneralPopupFiddleyardLinePenStyleDashDotDotClick(Sender: TObject);
    PROCEDURE GeneralPopupFiddleyardLinePenStyleDotClick(Sender: TObject);
    PROCEDURE GeneralPopupFiddleyardLinePenStyleInsideFrameClick(Sender: TObject);
    PROCEDURE GeneralPopupFiddleyardLinePenStyleSolidClick(Sender: TObject);
    PROCEDURE GeneralPopupListLocomotivesClick(Sender: TObject);
    PROCEDURE GeneralPopupMenuOnPopup(Sender: TObject);
    PROCEDURE GeneralPopupProjectedLinePenStyleClearClick(Sender: TObject);
    PROCEDURE GeneralPopupProjectedLinePenStyleDashClick(Sender: TObject);
    PROCEDURE GeneralPopupProjectedLinePenStyleDashDotClick(Sender: TObject);
    PROCEDURE GeneralPopupProjectedLinePenStyleDashDotDotClick(Sender: TObject);
    PROCEDURE GeneralPopupProjectedLinePenStyleDotClick(Sender: TObject);
    PROCEDURE GeneralPopupProjectedLinePenStyleInsideFrameClick(Sender: TObject);
    PROCEDURE GeneralPopupProjectedLinePenStyleSolidClick(Sender: TObject);
    PROCEDURE GeneralPopupResetFWPRailWindowSizeAndPositionClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreAllDefaultColoursClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreAllProgramDefaultSettingsClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreAllScreenDrawingDefaultSettingsClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreBufferStopColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreBufferStopNumberColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreBufferStopRedClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreDefaultBackgroundColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreFiddleyardLinePenStyleClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreForegroundColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointLenzNumberColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreLineNotAvailableColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreLineRoutedOverColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreLocoStalledColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePlatformColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePlungerColour(Sender: TObject);
    PROCEDURE GeneralPopupRestorePlungerOutlineColour(Sender: TObject);
    PROCEDURE GeneralPopupRestorePlungerPressedColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointDefaultColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointDivergingLineColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointDownFacingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointFeedbackDataInUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointFeedbackDataOutOfUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointHeelLineColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointLockedBySystemColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointLockedByUserColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointManualOperationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointOutOfUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointStraightLineColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointsWithoutFeedbackColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointUndrawColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointUpFacingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreProjectedLinePenStyleClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreScreenComponentEditedColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreShowPointDefaultStateColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreSidingPenStyleClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreSignalAspectGreenClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreSignalAspectRedClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreSignalAspectUnlitColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreSignalAspectYellowClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreSignalNumberColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreSignalPostEmergencyRouteSettingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreSignalPostRouteSettingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreSignalPostTheatreSettingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCFeedbackDataInUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCFeedbackDataOutOfUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCFeedbackOccupationButOutOfUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCFeedbackOccupationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCLocoOutOfPlaceColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCLocoOutOfPlaceOccupationPenStyleClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCMissingOccupationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedPenStyleClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCOutOfUseSetByUserColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCOutOfUseSetByUserPenStyleClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCPermanentFeedbackOccupationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCPermanentFeedbackOccupationPenStyleClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCPermanentOccupationSetByUserColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCPermanentOccupationSetByUserPenStyleClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCPermanentSystemOccupationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCPermanentSystemOccupationPenStyleClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCSpeedRestrictionColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCSystemOccupationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCUnoccupiedColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTCUserMustDriveColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTrainActiveColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreTrainInactiveColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRunClockFasterClick(Sender: TObject);
    PROCEDURE GeneralPopupRunClockFastestClick(Sender: TObject);
    PROCEDURE GeneralPopupRunClockNormallyClick(Sender: TObject);
    PROCEDURE GeneralPopupRunClockSlowerClick(Sender: TObject);
    PROCEDURE GeneralPopupSetCurrentRailwayDayOfTheWeekClick(Sender: TObject);
    PROCEDURE GeneralPopupSetLogFileMaximumWidthClick(Sender: TObject);
    PROCEDURE GeneralPopupShowMainMenuClick(Sender: TObject);
    PROCEDURE GeneralPopupShowTrackcircuitClick(Sender: TObject);
    PROCEDURE GeneralPopupSidingPenStyleClearClick(Sender: TObject);
    PROCEDURE GeneralPopupSidingPenStyleDashClick(Sender: TObject);
    PROCEDURE GeneralPopupSidingPenStyleDashDotClick(Sender: TObject);
    PROCEDURE GeneralPopupSidingPenStyleDashDotDotClick(Sender: TObject);
    PROCEDURE GeneralPopupSidingPenStyleDotClick(Sender: TObject);
    PROCEDURE GeneralPopupSidingPenStyleInsideFrameClick(Sender: TObject);
    PROCEDURE GeneralPopupSidingPenStyleSolidClick(Sender: TObject);
    PROCEDURE GeneralPopupTCLocoOutOfPlaceOccupationPenStyleClearClick(Sender: TObject);
    PROCEDURE GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashClick(Sender: TObject);
    PROCEDURE GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCLocoOutOfPlaceOccupationPenStyleInsideFrameClick(Sender: TObject);
    PROCEDURE GeneralPopupTCLocoOutOfPlaceOccupationPenStyleSolidClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleClearClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleInsideFrameClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleSolidClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseSetByUserPenStyleClearClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseSetByUserPenStyleDashClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseSetByUserPenStyleDashDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseSetByUserPenStyleDashDotDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseSetByUserPenStyleDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseSetByUserPenStyleInsideFrameClick(Sender: TObject);
    PROCEDURE GeneralPopupTCOutOfUseSetByUserPenStyleSolidClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentFeedbackOccupationPenStyleClearClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentFeedbackOccupationPenStyleDashClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentFeedbackOccupationPenStyleDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentFeedbackOccupationPenStyleInsideFrameClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentFeedbackOccupationPenStyleSolidClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentOccupationSetByUserPenStyleClearClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentOccupationSetByUserPenStyleDashClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentOccupationSetByUserPenStyleDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentOccupationSetByUserPenStyleInsideFrameClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentOccupationSetByUserPenStyleSolidClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentSystemOccupationPenStyleClearClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentSystemOccupationPenStyleDashClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentSystemOccupationPenStyleDashDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentSystemOccupationPenStyleDashDotDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentSystemOccupationPenStyleDotClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentSystemOccupationPenStyleInsideFrameClick(Sender: TObject);
    PROCEDURE GeneralPopupTCPermanentSystemOccupationPenStyleSolidClick(Sender: TObject);
    PROCEDURE HelpMenuAboutClick(Sender: TObject);
    PROCEDURE LinePopupItemClick(Sender: TObject);
    PROCEDURE LinePopupMenuOnPopup(Sender: TObject);
    PROCEDURE LocoInfoMenuItemClick(Sender: TObject);
    PROCEDURE MainDisplayMenuDebugClick(Sender: TObject);
    PROCEDURE MainDisplayMenuDiagramsWindowClick(Sender: TObject);
    PROCEDURE MainDisplayMenuShowClick(Sender: TObject);
    PROCEDURE MainDisplayMenuWorkingTimetableWindowClick(Sender: TObject);
    PROCEDURE MainDisplayMenuZoomClick(Sender: TObject);
    PROCEDURE MainHelpMenuRailHelpClick(Sender: TObject);
    PROCEDURE MainOperationsMenuDriveLocomotiveClick(Sender: TObject);
    PROCEDURE MainRunMenuResumeOperationsClick(Sender: TObject);
    PROCEDURE PointPopupMenuOnPopup(Sender: TObject);
    PROCEDURE PointPopupItemClick(Sender: TObject);
    PROCEDURE SetCurrentRailwayTime(Sender: TObject);
    PROCEDURE SetDaylightEndTime(Sender: TObject);
    PROCEDURE SetDaylightStartTime(Sender: TObject);
    PROCEDURE SetProgramStartTime(Sender: TObject);
    PROCEDURE ShowStatusBarClick(Sender: TObject);
    PROCEDURE SignalPopupItemClick(Sender: TObject);
    PROCEDURE SignalPopupMenuOnPopup(Sender: TObject);
    PROCEDURE StartClock(Sender: TObject);
    PROCEDURE StopClock(Sender: TObject);

  PRIVATE
    { Private declarations }
//    PROCEDURE ApplicationMessage(VAR Msg: TMsg; VAR Handled: Boolean);
//    { Intercept messages - only way of getting at the tab key! Now replaced by ShortCut above Sept 2009 }

    PROCEDURE ApplicationRestore(Sender: TObject);
    { First of three routines which allow the program to be minimised by pressing the "minimize" button and then maximised from the taskbar }

    PROCEDURE WMCopyData(VAR Msg : TWMCopyData); Message WM_COPYDATA;
    { Receives data from the Watchdog program }

//    { Added to handle Windows messages }
//    PROCEDURE CM_MenuClosed(VAR Msg : TMessage); MESSAGE CM_MENU_CLOSED;
//    PROCEDURE CM_EnterMenuLoop(VAR Msg : TMessage); MESSAGE CM_ENTER_MENU_LOOP;
//    PROCEDURE CM_ExitMenuLoop(VAR Msg : TMessage); MESSAGE CM_EXIT_MENU_LOOP;
  PUBLIC
    { Public declarations }
    PROCEDURE WMHScroll(VAR ScrollData: TMessage); MESSAGE wm_HScroll;
    { Added to allow interception of scroll bar events }
    PROCEDURE WMVScroll(VAR ScrollData: TMessage); MESSAGE wm_VScroll;
    { Added to allow interception of scroll bar events }
    PROCEDURE WMSysCommand(VAR Msg: TWMSysCommand); MESSAGE WM_SYSCOMMAND;
    { Third of three routines which allow the program to be minimised by pressing the "minimize" button and then maximised from the taskbar }
  END;

 PopupTypes = (NoClickPopupType,
               SignalChangeDirectionPopupType, SignalDeletePopupType, SignalEditPopupType, SignalOutOfUsePopupType, SignalUndoChangesPopupType,
               PointEditPopupType, PointOutOfUsePopupType, PointToManualPopupType, PointUnlockPopupType,
               BufferStopEditPopupType,
               LineAllocateLocoToTrackCircuitPopupType, LineChangeInternalLocoDirectionToDownPopupType, LineChangeInternalLocoDirectionToUpPopupType,
               LineCreateLinePopupType, LineCreateDownPointPopupType, LineCreateUpPointPopupType, LineCreateDownSignalPopupType, LineCreateUpSignalPopupType,
               LineEditPopupType, LineLocationOutOfUsePopupType, LineOutOfUsePopupType, LineShowLocoLastErrorMessagePopupType, LineTCFeedbackOccupationPopupType,
               LineTCOutOfUsePopupType, LineTCPermanentOccupationPopupType, LineTCSpeedRestrictionPopupType, LineTCSystemOccupationPopupType, LineTCUnoccupiedPopupType,
               LineTCUserMustDrivePopupType);

 TMenuItemExtended = CLASS(TMenuItem)
 PRIVATE
   fValue: String;
   fPopupType : PopupTypes;

 PUBLISHED
   PROPERTY Value : String Read fValue Write fValue;
   PROPERTY PopupType : PopupTypes Read fPopupType Write fPopupType;
 END;

PROCEDURE CanvasTextOutAngle(X, Y : Integer; D : Word; S : String);
{ D is in tenths if a degree - i.e. 450 - 45 degrees. This is not used, but might come in useful }

PROCEDURE ChangeCursor(NewCursor : TCursor);
{ Change the shape of the cursor (from the Delphi Help system) }

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

FUNCTION GetDiagramsCheckingInProgress : Boolean;
{ Return the DiagramsCheckingInProgress variable state }

FUNCTION GetSaveCursor : TCursor;
{ Return the SaveCursor variable state }

PROCEDURE HideStatusBarAndUpDownIndications;
{ Before a zoomed screen move, hide the status bar and the "up" and "down" markers }

PROCEDURE InitialiseRaildrawUnit;
{ Initialises the unit }

PROCEDURE InvalidateScreen(UnitRefParam, CallingStr : String);
{ Draw the screen by invalidating it }

PROCEDURE ResetFWPRailWindowSizeAndPosition;
{ Reset the window's size and position }

PROCEDURE ResetScreenColoursAfterPrinting;
{ Restore the colours to those saved before printing the screen in printer-friendly colours }

PROCEDURE SetBufferStopPopupNum(Num : Integer);
{ Set the buffer stop popup number }

PROCEDURE SetCaption(Window : TForm; Caption : String);
{ Sets a window caption }

PROCEDURE SetLinePopupNum(Num : Integer);
{ Set the LinePopupNum variable state }

PROCEDURE SetPointPopupNum(Num : Integer);
{ Set the PointPopupNum variable state }

PROCEDURE SetSignalPopupNum(Num : Integer);
{ Set the SignalPopupNum variable state }

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
  SavePointDivergingLineColourForPrinting : TColor;
  SavePointDownFacingColourForPrinting : TColor;
  SavePointFeedbackDataInUseColourForPrinting : TColor;
  SavePointFeedbackDataOutOfUseColourForPrinting : TColor;
  SavePointHeelLineColourForPrinting : TColor;
  SavePointLenzNumberColourForPrinting : TColor;
  SavePointManualOperationColourForPrinting : TColor;
  SavePointStraightLineColourForPrinting :TColor;
  SavePointsWithoutFeedbackColourForPrinting : TColor;
  SavePointUpFacingColourForPrinting : TColor;
  SaveShowPointDefaultStateColourForPrinting : TColor;
  SaveShowPointLockedColourForPrinting : TColor;
  SaveShowSignalJunctionDestinations : Boolean = False;
  SaveScreenComponentEditedColourForPrinting : TColor;
  SaveSignalNumberColourForPrinting : TColor;
  SaveSignalPostColourForPrinting : TColor;
  SaveSignalPostRouteSettingColourForPrinting : TColor;
  SaveSignalPostTheatreSettingColourForPrinting : TColor;
  SaveTCMissingOccupationColourForPrinting : TColor;
  SaveTCFeedbackOccupationColourForPrinting : TColor;
  SaveTCOutOfUseSetByUserColourForPrinting : TColor;
  SaveTCOutOfUseAsNoFeedbackReceivedColourForPrinting : TColor;
  SaveTCPermanentFeedbackOccupationColourForPrinting : TColor;
  SaveTCPermanentOccupationSetByUserColourForPrinting : TColor;
  SaveTCPermanentSystemOccupationColourForPrinting : TColor;
  SaveTCSystemOccupationColourForPrinting : TColor;
  SaveTCUnoccupiedColourForPrinting : TColor;
  SaveTCUserMustDriveColourForPrinting : TColor;
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
  LinePopupNum : Integer;
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
  TrackCircuitPopupMenuActive : Boolean = False;
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

PROCEDURE SetLinePopupNum(Num : Integer);
{ Set the GetLinePopupNum variable state }
BEGIN
  LinePopupNum := Num;
END; { SetLinePopupNum }

PROCEDURE SetPointPopupNum(Num : Integer);
{ Set the GetPointPopupNum variable state }
BEGIN
  PointPopupNum := Num;
END; { SetPointPopupNum }

PROCEDURE SetSignalPopupNum(Num : Integer);
{ Set the GetSignalPopupNum variable state }
BEGIN
  SignalPopupNum := Num;
END; { SetSignalPopupNum }

FUNCTION GetDiagramsCheckingInProgress : Boolean;
{ Return the DiagramsCheckingInProgress variable state }
BEGIN
  Result := DiagramsCheckingInProgress;
END; { GetDiagramsCheckingInProgress }

PROCEDURE SetBufferStopPopupNum(Num : Integer);
{ Set the buffer stop popup number }
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
      IF RecordLineDrawingMode THEN
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
      IF NOT TestingMode THEN
        FWPRailWindow.Caption := 'FWP''s Railway Program ' + Caption
      ELSE
        FWPRailWindow.Caption := 'FWP''s Railway Program (Version ' + GetVersionInfoAsString + ' Build ' + GetBuildInfoAsString + ') ' + Caption;
    END;
  END;
END; { SetCaption }

PROCEDURE ShowLinePos;
{ Showing how line segments are sub-divided - for development }
VAR
  FeedbackData : FeedbackRec;
  FeedbackNum : Integer;
  FeedbackUnitFound : Boolean;
  Line : Integer;
  LineNameWidth : Word;
  SegmentText : String;
  TrackCircuitNumbered : Boolean;
  X, Y : Integer;

  PROCEDURE ShowTrackCircuitData;
  { Display data relating to track circuits }
  VAR
    ColourNum : Integer;
    FeedbackType : TypeOfFeedBackType;
    I, J, K : Integer;
    Line : Integer;
    LineDrawn : Boolean;
    P : Integer;
    Pos : Word;
    TC : Integer;
    TCAboveFeedbackUnit : Integer;
    TempNum : Integer;

  BEGIN
    WITH RailWindowBitmap.Canvas DO BEGIN
      { First clear existing line detail, as it may obscure the data we're writing out }
      ShowLineOccupationDetail := False;
      FOR Line := 0 TO High(Lines) DO
        DrawLine(Line, Lines[Line].Line_CurrentColour, False);

      IF ShowOneFeedbackUnitOnly OR ShowTrackCircuitFeedbackDataInSeparateColours THEN BEGIN
          { show which Lenz feedback unit is being used }
        IF ShowOneFeedbackUnitOnly THEN BEGIN
          I := ShowFeedbackUnitNum;
          K := ShowFeedbackUnitNum;
        END ELSE BEGIN
          I := FirstFeedbackUnit;
          K := LastFeedbackUnit;
        END;

        { Go through all the colours in sequence, but start randomly - this is better than choosing colours at random, as one can end up with too many colours the same
          that way. Note: the From in RandomRange is included in the results, but the To is not.
        }
        ColourNum := RandomRange(1, MaxColourNum + 1);
        WHILE I <= K DO BEGIN
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

          SegmentText := IntToStr(I);
          FOR J := 1 TO 8 DO BEGIN
            FeedbackData.Feedback_Unit := I;
            FeedbackData.Feedback_Input := J;
            ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum);
            IF FeedbackType = PointFeedbackDetector THEN BEGIN
              IF ShowOneFeedbackUnitOnly THEN BEGIN
                Font.Color := clAqua;
                FOR P := 0 TO High(Points) DO BEGIN
                  WITH Points[P] DO BEGIN
                    IF (Point_FeedbackUnit = I) AND (Point_FeedbackInput = J) THEN BEGIN
                      SegmentText := IntToStr(I) + IntToStr(J);
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
                    END;
                  END; {WITH}
                END; {FOR}
              END;
            END ELSE
              IF FeedbackType = TrackCircuitFeedbackDetector THEN BEGIN
                IF ShowOneFeedbackUnitOnly THEN
                  Font.Color := clYellow;
                IF (FeedbackNum >= 0) AND (FeedbackNum <= High(TrackCircuits)) THEN BEGIN
                  SegmentText := IntToStr(I) + IntToStr(J);
                  IF Length(TrackCircuits[FeedbackNum].TC_LineArray) > 0 THEN BEGIN
                    Line := TrackCircuits[FeedbackNum].TC_LineArray[0];
                    WITH Lines[Line] DO
                      TextOut(Line_UpX + (Line_DownX - Line_UpX) DIV 2 - TextWidth(SegmentText) DIV 2 - ScrollBarXAdjustment,
                              (Line_UpY + (Line_DownY - Line_UpY) DIV 2) - TextHeight(SegmentText) DIV 2 - ScrollBarYAdjustment,
                              SegmentText);
                    SegmentText := '';
                  END;
                END;
              END;
          END; {FOR}
          Inc(I);
        END; {WHILE}

        IF ShowOneFeedbackUnitOnly THEN
          ShowOneFeedbackUnitOnly := False;
      END ELSE BEGIN
        FOR TC := 0 TO High(TrackCircuits) DO BEGIN
          { Write out the track circuit number once only }
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
                    X := Line_UpX + MulDiv(Line_DownX - Line_UpX, Pos, 10);
                    Y := Line_UpY + MulDiv(Line_DownY - Line_UpY, Pos, 10);
                    IF Pos = 0 THEN BEGIN
                      MoveTo(X - ScrollBarXAdjustment, Y - 6 - ScrollBarYAdjustment);
                      LineTo(X - ScrollBarXAdjustment, Y + 6 - ScrollBarYAdjustment);
                    END;
                  END; {WITH}
                END; {FOR}
              END;

              IF NOT ShowTrackCircuits THEN BEGIN
                { want TCs coloured all the same }
                IF TrackCircuits[TC].TC_UserMustDrive THEN
                  DrawLine(Line, TCUserMustDriveColour, NOT ActiveTrain)
                ELSE
                  DrawLine(Line, TCUnoccupiedColour, NOT ActiveTrain);
              END ELSE BEGIN
                { colour TCs differently to distinguish them }
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
                          FeedbackData.Feedback_Unit := I;
                          FeedbackData.Feedback_Input := J;
                          ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum);
                          IF FeedbackNum = Line_TC THEN BEGIN
                            FeedbackUnitFound := True;
                            SegmentText := IntToStr(I) + IntToStr(J);
                            IF FeedbackUnitInUseArray[I] THEN
                              Font.Color := TCFeedbackDataInUseColour
                            ELSE
                              Font.Color := TCFeedbackDataOutOfUseColour;
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

                  TextOut(Line_UpX + (Line_DownX - Line_UpX) DIV 2 - TextWidth(SegmentText) DIV 2 - ScrollBarXAdjustment,
                         (Line_UpY + (Line_DownY - Line_UpY) DIV 2) - TextHeight(SegmentText) DIV 2 - ScrollBarYAdjustment,
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
  END; { ShowTrackCircuitData }

  PROCEDURE ShowLineData;
  { show various line display options }
  VAR
    ElementPos : Integer;
    Line : Integer;
    Pos : Integer;
    SaveLineFontName : String;
    TempLocationArray : IntegerArrayType;

  BEGIN
    WITH RailWindowBitmap.Canvas DO BEGIN
      { First clear existing line detail, as it may obscure the data we're writing out }
      ShowLineOccupationDetail := False;
      SetLength(TempLocationArray, 0);

      FOR Line := 0 TO High(Lines) DO
        DrawLine(Line, Lines[Line].Line_CurrentColour, False);

      FOR Line := 0 TO High(Lines) DO BEGIN
        SegmentText := '';
        WITH Lines[Line] DO BEGIN
          { how which track circuits have routes set over them }
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
            IF Line_DownX > Line_UpX THEN BEGIN
              IF (Line_DownX - Line_UpX) > LineNameWidth THEN
                SegmentText := LineToStr(Line)
              ELSE
                SegmentText := Copy(LineToStr(Line), Length(LineToStr(Line)), 1);
            END ELSE BEGIN
              IF (Line_UpX - Line_DownX) > LineNameWidth THEN
                SegmentText := LineToStr(Line)
              ELSE
                SegmentText := Copy(LineToStr(Line), Length(LineToStr(Line)), 1);
            END;
          END;

          IF ShowLineNumbers THEN BEGIN
            { show the internal numbers for each track segment }
            Font.Color := GetLineTypeColour(Line_TypeOfLine);
            LineNameWidth := TextWidth(IntToStr(Line));
            IF Line_DownX > Line_UpX THEN BEGIN
              IF (Line_DownX - Line_UpX) > LineNameWidth THEN
                SegmentText := IntToStr(Line)
              ELSE
                SegmentText := Copy(IntToStr(Line), Length(IntToStr(Line)), 1);
            END ELSE BEGIN
              IF (Line_UpX - Line_DownX) > LineNameWidth THEN
                SegmentText := LineToStr(Line)
              ELSE
                SegmentText := Copy(IntToStr(Line), Length(IntToStr(Line)), 1);
            END;
          END;

          IF ShowLinesWhereUpXValueSpecified THEN BEGIN
            Font.Color := GetLineTypeColour(Line_TypeOfLine);
            LineNameWidth := TextWidth(LineToStr(Line));
            IF Line_UpXValueSpecified THEN
              SegmentText := LineToStr(Line)
            ELSE
              SegmentText := '';
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
                      IF Line_UpY = Line_DownY THEN
                        SegmentText := '¨' { left arrow }
                      ELSE
                        IF Line_UpY > Line_DownY THEN
                          SegmentText := 'Ø' { down arrow }
                        ELSE
                          SegmentText := '≠'; { up arrow }
                    END ELSE
                      IF Lines[Line].Line_Direction = Down THEN BEGIN
                        IF Line_UpY = Line_DownY THEN
                          SegmentText := 'Æ' { right arrow }
                        ELSE
                          IF Line_UpY > Line_DownY THEN
                            SegmentText := '≠' { up arrow }
                          ELSE
                            SegmentText := 'Ø'; { down arrow }
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

          IF ShowLocationLengthDetail THEN BEGIN
            IF Lines[Line].Line_Location <> UnknownLocation THEN BEGIN
              IF NOT IsElementInLocationArray(TempLocationArray, Lines[Line].Line_Location, ElementPos) THEN BEGIN
                { only display the length once for each location }
                AppendToLocationArray(TempLocationArray, Lines[Line].Line_Location);
                Font.Color := clAqua;
                SegmentText := LocationToStr(Lines[Line].Line_Location, ShortStringType) + ' ' + FloatToStr(Locations[Lines[Line].Line_Location].Location_LengthInInches);
              END;
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

          IF ShowLocations THEN BEGIN
            Font.Color := clLime;
            IF Lines[Line].Line_Location <> UnknownLocation THEN
              SegmentText := LocationToStr(Line_Location, ShortStringType)
            ELSE
              SegmentText := '?';
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

            SaveLineFontName := Font.Name;
            { arrows need to be in the Symbol typeface }
            IF (SegmentText = '¨') OR (SegmentText = 'Æ') OR (SegmentText = '≠') OR (SegmentText = 'Ø') OR (SegmentText = '≠Ø') THEN
              Font.Name := 'Symbol';

            TextOut(Line_UpX + (Line_DownX - Line_UpX) DIV 2 - TextWidth(SegmentText) DIV 2 - ScrollBarXAdjustment,
                   (Line_UpY + (Line_DownY - Line_UpY) DIV 2) - TextHeight(SegmentText) DIV 2 - ScrollBarYAdjustment,
                    SegmentText);
            Font.Name := SaveLineFontName;
          END;

          { Draw vertical lines to show the line segments }
          FOR Pos := 0 TO 10 DO BEGIN
            X := Line_UpX + MulDiv(Line_DownX - Line_UpX, Pos, 10);
            Y := Line_UpY + MulDiv(Line_DownY - Line_UpY, Pos, 10);
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
              TextOut(Line_UpX + (Line_DownX - Line_UpX) DIV 2 - TextWidth(SegmentText) DIV 2 - ScrollBarXAdjustment,
                     (Line_UpY + (Line_DownY - Line_UpY) DIV 2) - TextHeight(SegmentText) DIV 2 - ScrollBarYAdjustment,
                      SegmentText);
            END; {WITH}
          END;
          Inc(Line);
        END; {WHILE}
      END;

      { The following options cycle through the various lines }
      IF ShowAreas
      OR ShowLineDetail
      OR ShowLineNumbers
      OR ShowLinesWhereUpXValueSpecified
      OR ShowLinesWhichLockPoints
      OR ShowLineDirectionDetail
      OR ShowLocationLengthDetail
      OR ShowLineGradients
      OR ShowLocations
      OR ShowTrackCircuitsRoutedOver
      THEN
        ShowLineData;
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
                  IF LeftArrowNeeded THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextOut(Line_UpX + SpeedRestrictionHorizontalSpacingScaled - ScrollBarXAdjustment,
                            Line_UpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                            LeftArrowCh);
                    TextRect(Rect(Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(LeftArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(LeftArrowCh) + TextWidth(SpeedStr) - ScrollBarXAdjustment,
                                  Line_UpY - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(LeftArrowCh) - ScrollBarXAdjustment,
                             Line_UpY - TextHeight(SpeedStr) - (SpeedRestrictionVerticalSpacingScaled) + HorizontalArrowAdjustment - ScrollBarYAdjustment,
                             SpeedStr);
                  END;
                  IF NorthArrowNeededOnRight THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextOut(Line_UpX + SpeedRestrictionHorizontalSpacingScaled - ScrollBarXAdjustment,
                            Line_UpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                            NorthArrowCh);
                    TextRect(Rect(Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(NorthArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(NorthArrowCh) + TextWidth(SpeedStr) - ScrollBarXAdjustment,
                                  Line_UpY - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(NorthArrowCh) - ScrollBarXAdjustment,
                             Line_UpY - TextHeight(SpeedStr) - (SpeedRestrictionVerticalSpacingScaled) + HorizontalArrowAdjustment - ScrollBarYAdjustment,
                             SpeedStr);
                  END;
                  IF SouthArrowNeededOnRight THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextOut(Line_UpX + SpeedRestrictionHorizontalSpacingScaled - ScrollBarXAdjustment,
                            Line_UpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                            SouthArrowCh);
                    TextRect(Rect(Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(SouthArrowCh) + TextWidth(SpeedStr) - ScrollBarXAdjustment,
                                  Line_UpY - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                             Line_UpY - TextHeight(SpeedStr) - (SpeedRestrictionVerticalSpacingScaled) + HorizontalArrowAdjustment - ScrollBarYAdjustment,
                             SpeedStr);
                  END;
                  IF RightArrowNeeded THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextRect(Rect(Line_UpX + (Line_DownX - Line_UpX) - TextWidth(SpeedStr) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  Line_UpX + (Line_DownX - Line_UpX) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY + TextHeight(SpeedStr) + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             Line_UpX + (Line_DownX - Line_UpX) - TextWidth(SpeedStr) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                             Line_UpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                             SpeedStr);
                    TextOut(Line_UpX + (Line_DownX - Line_UpX) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                            Line_UpY + SpeedRestrictionVerticalSpacingScaled - HorizontalArrowAdjustment - ScrollBarYAdjustment,
                            RightArrowCh);
                  END;

                  IF SouthArrowNeededOnLeft THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextRect(Rect(Line_UpX + (Line_DownX - Line_UpX) - TextWidth(SpeedStr) - TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  Line_UpX + (Line_DownX - Line_UpX) - TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY + TextHeight(SpeedStr) + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             Line_UpX + (Line_DownX - Line_UpX) - TextWidth(SpeedStr) - TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                             Line_UpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                             SpeedStr);
                    TextOut(Line_UpX + (Line_DownX - Line_UpX) - TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                            Line_UpY + SpeedRestrictionVerticalSpacingScaled - HorizontalArrowAdjustment - ScrollBarYAdjustment,
                            SouthArrowCh);
                  END;

                  IF NorthArrowNeededOnLeft THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextRect(Rect(Line_UpX + (Line_DownX - Line_UpX) - TextWidth(SpeedStr) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  Line_UpX + (Line_DownX - Line_UpX) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY + TextHeight(SpeedStr) + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             Line_UpX + (Line_DownX - Line_UpX) - TextWidth(SpeedStr) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                             Line_UpY + SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                             SpeedStr);
                    TextOut(Line_UpX + (Line_DownX - Line_UpX) - TextWidth(RightArrowCh) - ScrollBarXAdjustment,
                            Line_UpY + SpeedRestrictionVerticalSpacingScaled - HorizontalArrowAdjustment - ScrollBarYAdjustment,
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
        IF NOT FWPRailWindowInitialised THEN
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

            Pen.Color := Signals[S].Signal_PostColour;

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

              Pen.Color := Signals[S].Signal_PostColour;

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

        IF RecordLineDrawingMode OR NOT FWPRailWindowInitialised OR Signal_StateChanged THEN
          Log('S S=' + IntToStr(S)
                 + ' A=' + AspectToStr(Signals[S].Signal_Aspect, ShortStringType)
                 + ' I=' + IndicatorStateToStr(Signals[S].Signal_IndicatorState));

        IF Signal_StateChanged THEN
          Signal_StateChanged := False;

        IF NOT ShowSignalHiddenStationSignalAspects THEN
          Aspect := Signal_Aspect
        ELSE
          Aspect := Signal_HiddenStationSignalAspect;

        MoveTo(Signal_LineX - ScrollBarXAdjustment, Signal_LineWithVerticalSpacingY - ScrollBarYAdjustment);

        IF Signal_Type = FourAspect THEN BEGIN
          IF Signal_Direction = Up THEN
            TopAspectX := Signal_LineX - SignalHorizontalSpacingScaled
          ELSE
            IF Signal_Direction = Down THEN
              TopAspectX := Signal_LineX + SignalHorizontalSpacingScaled;
        END;

        { Now draw indicators if any }
        IF Signal_Indicator <> NoIndicator THEN BEGIN
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
        END;

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
          IF Aspect = RedAspect THEN BEGIN
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
          OR { a safeguard - turn the signal off altogether! }
             ((Signal_AdjacentLine <> UnknownLine) AND (Lines[Signal_AdjacentLine].Line_TC = UnknownTrackCircuit))
          THEN BEGIN
            { setting a semaphore to no aspect is meaningless, so since white wooden crosses are used on real railways, we can do the same }
            Pen.Color := clWhite;

            WITH Signal_MouseRect DO BEGIN
              MoveTo(Left - ScrollBarXAdjustment, Top - ScrollBarYAdjustment);
              LineTo(Right - ScrollBarXAdjustment, Bottom - ScrollBarYAdjustment);
              MoveTo(Right - ScrollBarXAdjustment, Top - ScrollBarYAdjustment);
              LineTo(Left - ScrollBarXAdjustment, Bottom - ScrollBarYAdjustment);
            END; {WITH}
          END;
        END ELSE BEGIN
          IF EditedSignal = S THEN BEGIN
            SColour1 := ScreenComponentEditedColour;
            SColour2 := ScreenComponentEditedColour;
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
  ActiveTrainText : Char;
  DownLineColour : TColor;
  LineTextStr : String;
  LineTextStrRect : TRect;
  T : TrainIndex;
  UpLineColour : TColor;
  X1, X2, Y1, Y2 : Integer;

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
            X1 := Lines[Line].Line_UpX;
            Y1 := Lines[Line].Line_UpY - BufferStopVerticalSpacingScaled;
            Y2 := Lines[Line].Line_UpY + BufferStopVerticalSpacingScaled;
            DrawRedLampAndVerticalLine(X1, Y1, Y2, ForegroundColour);

            X1 := Lines[Line].Line_DownX;
            Y1 := Lines[Line].Line_DownY - BufferStopVerticalSpacingScaled;
            Y2 := Lines[Line].Line_DownY + BufferStopVerticalSpacingScaled;
            DrawRedLampAndVerticalLine(X1, Y1, Y2, ForegroundColour);

            Pen.Style := psDot;
          END;

          { Save current and old colour data }
          IF Line_CurrentColour <> NewLineColour THEN BEGIN
            Line_OldColour := Line_CurrentColour;
            Line_CurrentColour := NewLineColour;
          END;

          IF RecordLineDrawingMode THEN BEGIN
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

          IF LineTextStr = '' THEN BEGIN
            IF ShowTrackCircuitsWhereUserMustDrive THEN
              IF Lines[Line].Line_TC <> UnknownTrackCircuit THEN
                IF TrackCircuits[Lines[Line].Line_TC].TC_UserMustDrive THEN
                  LineTextStr := 'U';
          END;

          { Clear any previous text away }
          IF (LineTextStr <> '') OR (TempLineText <> '') THEN BEGIN
            IF (Line_UpY = Line_DownY) AND ((Line_DownX - Line_UpX > TextWidth('---- ')) OR (Line_UpX - Line_DownX > TextWidth('---- '))) THEN BEGIN
              X1 := Line_UpX + ((Line_DownX - Line_UpX - TextWidth('MMMM')) DIV 2) - ScrollBarXAdjustment;
              Y1 := Line_UpY - (TextHeight('M') DIV 2) - ScrollBarYAdjustment;
              X2 := Line_DownX - ((Line_DownX - Line_UpX - TextWidth('MMMM')) DIV 2) - ScrollBarXAdjustment;
              Y2 := Line_UpY + (TextHeight('M') DIV 2) - ScrollBarYAdjustment;
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
            MoveTo(Line_UpX - ScrollBarXAdjustment, Line_UpY - ScrollBarYAdjustment);
            LineTo(Line_DownX - ScrollBarXAdjustment, Line_DownY - ScrollBarYAdjustment);
          END ELSE BEGIN
            CASE Pen.Style OF
              psDashDot, psDot, psDashDotDot:
                DrawDottedLine(Line_UpX, Line_UpY, Line_DownX, Line_DownY);
              psSolid:
                BEGIN
                  MoveTo(Line_UpX - ScrollBarXAdjustment, Line_UpY - ScrollBarYAdjustment);
                  LineTo(Line_DownX - ScrollBarXAdjustment, Line_DownY - ScrollBarYAdjustment);
                END;
            END; {CASE}
          END;

          { if there's some text and there's room for it, and the line is horizontal, then add it }
          IF ShowLineOccupationDetail AND (LineTextStr <> '') AND (TempLineText <> ClearLineString) THEN BEGIN
            { needs text if there's room }
            IF (Line_UpY = Line_DownY)
            AND ((Line_DownX - Line_UpX > TextWidth(LineTextStr)) OR (Line_UpX - Line_DownX > TextWidth(LineTextStr)))
            THEN BEGIN
              { clear space for the text }
              Brush.Color := BackgroundColour;
              IF ScreenColoursSetForPrinting THEN
                Font.Color := clBlack;
              Font.Height := -MulDiv(FWPRailWindow.ClientHeight, LineFontHeight, ZoomScalefactor);

              { the following Rect is not used *** }
              LineTextStrRect := Rect(Line_UpX + ((Line_DownX - Line_UpX - TextWidth(LineTextStr + StringOfChar(' ', 2))) DIV 2) - ScrollBarXAdjustment,
                                      Line_UpY - (LineFontHeight DIV 2) - ScrollBarYAdjustment,
                                      Line_DownX - ((Line_DownX - Line_UpX - TextWidth(LineTextStr + StringOfChar(' ', 2))) DIV 2) - ScrollBarXAdjustment,
                                      Line_UpY + (LineFontHeight DIV 2) - ScrollBarYAdjustment);

              TextOut(Line_UpX + ((Line_DownX - Line_UpX - TextWidth(LineTextStr)) DIV 2) - ScrollBarXAdjustment,
                      Line_UpY - (LineFontHeight DIV 2) - ScrollBarYAdjustment,
                      LineTextStr);
            END;
          END;

          { what if the line is not horizontal? *** }

          { Draw adjacent lines if they are not track circuited }
          IF (Line_NextUpLine <> UnknownLine) AND (Line_NextDownLine <> UnknownLine) THEN BEGIN
            IF (Lines[Line_NextUpLine].Line_TC = UnknownTrackCircuit) AND (Lines[Line_NextDownLine].Line_TC = UnknownTrackCircuit) THEN BEGIN
              MoveTo(Lines[Line_NextUpLine].Line_UpX - ScrollBarXAdjustment,
                     Lines[Line_NextUpLine].Line_UpY - ScrollBarYAdjustment);
              LineTo(Lines[Line_NextUpLine].Line_DownX - ScrollBarXAdjustment,
                     Lines[Line_NextUpLine].Line_DownY - ScrollBarYAdjustment);
              MoveTo(Lines[Line_NextDownLine].Line_DownX - ScrollBarXAdjustment,
                     Lines[Line_NextDownLine].Line_DownY - ScrollBarYAdjustment);
              LineTo(Lines[Line_NextDownLine].Line_DownX - ScrollBarXAdjustment,
                     Lines[Line_NextDownLine].Line_DownY - ScrollBarYAdjustment);
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

BEGIN
  TRY
    InitialiseScreenDrawingVariables;
    WITH RailWindowBitmap.Canvas DO BEGIN
      WITH Points[P] DO BEGIN
        { Undraw the previous state by increasing the pen width when rubbing out the line - otherwise a faint trace of the line gets left behind (I know this is a hack,
          but it works!)
        }
        Pen.Color := PointUndrawColour;
        IF ThinLineMode THEN
          Pen.Width := WindowPenWidth + 1
        ELSE
          Pen.Width := FullScreenPenWidth + 1;

        MoveTo(Point_X - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
        IF Point_PresentState = Straight THEN BEGIN
          { clear away the faint color from where we're going to draw }
          Pen.Color := BackgroundColour;
          LineTo(Point_FarX - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
          { now rub out the previous point setting, but leave a faint line! }
          Pen.Color := PointUndrawColour;
          MoveTo(Point_X - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
          LineTo(Point_FarX - ScrollBarXAdjustment, Point_FarY - ScrollBarYAdjustment)
        END ELSE BEGIN
          IF Point_PresentState = Diverging THEN BEGIN
            { clear away the faint color from where we're going to draw }
            Pen.Color := BackgroundColour;
            LineTo(Point_FarX - ScrollBarXAdjustment, Point_FarY - ScrollBarYAdjustment);
            { now rub out the previous point setting, but leave a faint line! }
            Pen.Color := PointUndrawColour;
            MoveTo(Point_X - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
            LineTo(Point_FarX - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
          END ELSE BEGIN
            { PointState = PointStateUnknown }
            Pen.Color := PointUndrawColour;
            LineTo(Point_FarX - ScrollBarXAdjustment, Point_FarY - ScrollBarYAdjustment);
            MoveTo(Point_X - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
            LineTo(Point_FarX - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
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
                Pen.Color := ShowPointDefaultStateColour;
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

        MoveTo(Point_X - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);

        IF ShowPointsStraightAndDiverging THEN BEGIN
          Pen.Color := clFWPOrange;
          LineTo(Point_FarX - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
          MoveTo(Point_X - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
          Pen.Color := clYellow;
          LineTo(Point_FarX - ScrollBarXAdjustment, Point_FarY - ScrollBarYAdjustment);
        END ELSE
          IF Point_PresentState = PointOutOfAction THEN BEGIN
            Pen.Width := 2;
            LineTo(Point_FarX - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
            MoveTo(Point_X - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
            LineTo(Point_FarX - ScrollBarXAdjustment, Point_FarY - ScrollBarYAdjustment);
          END ELSE BEGIN
            { a normal state }
            IF NOT ShowPointDefaultState THEN BEGIN
              IF Point_PresentState = Straight THEN
                LineTo(Point_FarX - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment)
              ELSE
                IF Point_PresentState = Diverging THEN
                  LineTo(Point_FarX - ScrollBarXAdjustment, Point_FarY - ScrollBarYAdjustment)
                ELSE BEGIN
                  { PresentState = PointStateUnknown }
                  IF ShowPointDefaultState THEN
                    Pen.Color := ForegroundColour;
                  LineTo(Point_FarX - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
                  MoveTo(Point_X - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment);
                  LineTo(Point_FarX - ScrollBarXAdjustment, Point_FarY - ScrollBarYAdjustment);
                END;
            END ELSE BEGIN
              { ShowPointDefaultState }
              IF Point_DefaultState = Straight THEN
                LineTo(Point_FarX - ScrollBarXAdjustment, Point_Y - ScrollBarYAdjustment)
              ELSE
                IF Point_DefaultState = Diverging THEN
                  LineTo(Point_FarX - ScrollBarXAdjustment, Point_FarY - ScrollBarYAdjustment);
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
  FeedbackData : FeedbackRec;
  FeedbackNum : Integer;
  FeedbackType : TypeOfFeedBackType;
  I, J : Integer;
  LockingMsg : String;
  NumberText : String;
  TCAboveFeedbackUnit : Integer;

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
                FeedbackData.Feedback_Unit := I;
                FOR J := 1 TO 8 DO BEGIN
                  FeedbackData.Feedback_Input := J;
                  { extract what kind of feedback it is (FeedbackNum is only use for track circuits) }
                  ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum);
                  IF FeedbackType = PointFeedbackDetector THEN BEGIN
                    IF (Point_FeedbackUnit = I) AND (Point_FeedbackInput = J) THEN BEGIN
                      NumberText := IntToStr(I) + IntToStr(J);
                      IF ScreenColoursSetForPrinting THEN
                        Font.Color := clBlack
                      ELSE
                        IF FeedbackUnitInUseArray[I] THEN
                          Font.Color := PointFeedbackDataInUseColour
                        ELSE
                          Font.Color := PointFeedbackDataOutOfUseColour;
                    END;
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
            TextOut(Right - ScrollBarXAdjustment,
                    Top + ((Bottom - Top - TextHeight(NumberText)) DIV 2) - ScrollBarYAdjustment,
                    NumberText)
          ELSE
            TextOut(Left - TextWidth(NumberText) - ScrollBarXAdjustment,
                    Top + ((Bottom - Top - TextHeight(NumberText)) DIV 2) - ScrollBarYAdjustment,
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
  FeedbackData : FeedbackRec;
  FeedbackNum : Integer;
  FeedbackType : TypeOfFeedBackType;
  I, J : Integer;
  NumberText : String;
  P : Integer;
  TCAboveFeedbackUnit : Integer;
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
          FeedbackData.Feedback_Unit := I;
          FeedbackData.Feedback_Input := J;
          { extract what kind of feedback it is (FeedbackNum is only use for track circuits) }
          ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum);
          IF FeedbackType = PointFeedbackDetector THEN BEGIN
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
                        DrawPointNum(P, ShowPointLockedColour)
                      ELSE
                        IF ShowPointDefaultState AND (Points[P].Point_DefaultState <> PointStateUnknown) THEN
                          { only draw the point number if the point has a default state }
                          DrawPointNum(P, ShowPointDefaultStateColour);

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

PROCEDURE TFWPRailWindow.FWPRailWindowExitClick(Sender: TObject);
CONST
  ExitProgram = True;

BEGIN
  Log('A Shutdown requested by user selecting exit menu item');
  ShutDownProgram(UnitRef, 'FWPRailWindowExitClick');
END; { FWPRailWindowExitClick }

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
      AND NOT TrackCircuitPopupMenuActive { a global variable, owing to the special nature of GeneralPopup menus which means one cannot normally detect whether they are
                                            "popped up" or not }
      THEN
        FWPRailWindow.SetFocus;

      IF NOT ProgramStartup THEN
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
    IF FWPRailWindowInitialised THEN BEGIN
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

PROCEDURE TFWPRailWindow.MainDisplayMenuDiagramsWindowClick(Sender: TObject);
BEGIN
  IF DiagramsWindow.Visible THEN BEGIN
    DiagramsWindow.Hide;
    MainDisplayMenuDiagramsWindow.Checked := False;
  END ELSE BEGIN
    DiagramsWindow.Show;
    MainDisplayMenuDiagramsWindow.Checked := True;
    DrawDiagrams(UnitRef, 'MainDisplayMenuDiagramsClick');
  END;
END; { MainDisplayMenuDiagramsClick }

PROCEDURE TFWPRailWindow.MainDisplayMenuWorkingTimetableWindowClick(Sender: TObject);
BEGIN
  IF WorkingTimetableWindow.Visible THEN BEGIN
    WorkingTimetableWindow.Hide;
    MainDisplayMenuWorkingTimetableWindow.Checked := False;
  END ELSE BEGIN
    WorkingTimetableWindow.Show;
    MainDisplayMenuWorkingTimetableWindow.Checked := True;
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
PROCEDURE TFWPRailWindow.MainDisplayMenuDebugClick(Sender: TObject);
BEGIN
  IF DebugWindow.Visible THEN BEGIN
    DebugWindow.Hide;
    MainDisplayMenuDebug.Checked := False;
  END ELSE BEGIN
    DebugWindow.Show;
    MainDisplayMenuDebug.Checked := True;
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
    MainDisplayMenuShowStatusBar.Checked := False;
  END ELSE BEGIN
    FWPRailWindow.FWPRailWindowStatusBar.Show;
    MainDisplayMenuShowStatusBar.Checked := True;
  END;
END; { ShowStatusBarClick }

PROCEDURE TFWPRailWindow.MainDisplayMenuShowClick(Sender: TObject);
{ Make menus visible if they're not and vice versa }
BEGIN
  IF MenusVisible THEN
    HideMenus
  ELSE
    ShowMenus;
END; { ShowMenuItemClick }

PROCEDURE TFWPRailWindow.MainDisplayMenuZoomClick(Sender: TObject);
BEGIN
  MainDisplayMenuZoom.Checked := NOT MainDisplayMenuZoom.Checked;
  IF MainDisplayMenuZoom.Checked THEN
    ScreenMode := FullScreenMode
  ELSE BEGIN
    ScreenMode := DefaultWindowedScreenMode;
    InvalidateScreen(UnitRef, 'MainDisplayMenuZoomClick');
  END;
END; { MainDisplayMenuZoomClick }

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

PROCEDURE TFWPRailWindow.MainHelpMenuRailHelpClick(Sender: TObject);
BEGIN
  Application.HelpCommand(HELP_FINDER, 0);
END; { MainHelpMenuRailHelpClick }

PROCEDURE TFWPRailWindow.MainOperationsMenuDriveLocomotiveClick(Sender: TObject);
BEGIN
  { **** }
END; { MainOperationsMenuDriveLocomotiveClick }

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
    IF NOT ProgramStartup THEN BEGIN
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
            IF TrackCircuits[Lines[Line].Line_TC].TC_LitUp THEN BEGIN
              TrackCircuits[Lines[Line].Line_TC].TC_LitUp := False;
            END ELSE BEGIN
              TrackCircuits[Lines[Line].Line_TC].TC_LitUp := True;
            END;
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

PROCEDURE TFWPRailWindow.ApplicationRestore(Sender: TObject);
{ First of three routines which allow the program to be minimised by pressing the "minimize" button and then maximised from the taskbar }
BEGIN
 IF FWPRailWindow <> NIL THEN
   FWPRailWindow.WindowState := wsNormal;
 IF DebugWindow <> NIL THEN
   DebugWindow.WindowState := wsNormal;
 IF DiagramsWindow <> NIL THEN
   DiagramsWindow.WindowState := wsNormal;
END; { ApplicationRestore }

PROCEDURE TFWPRailWindow.FWPRailWindowCreate(Sender: TObject);
{ Second of three routines which allow the program to be minimised by pressing the "minimize" button and then maximised from the taskbar }
BEGIN
  Application.OnRestore := ApplicationRestore;
END; { FWPRailWindowCreate }

PROCEDURE TFWPRailWindow.WMSysCommand;
{ Third of three routines which allow the program to be minimised by pressing the "minimize" button and then maximised from the taskbar }
BEGIN
  CASE Msg.CmdType OF
    SC_MINIMIZE:
      BEGIN
        IF FWPRailWindow <> NIL THEN
          FWPRailWindow.WindowState := wsMinimized;
        IF DebugWindow <> NIL THEN
          DebugWindow.WindowState := wsMinimized;
        IF DiagramsWindow <> NIL THEN
           DiagramsWindow.WindowState := wsMinimized;
      END;
  END; {CASE}

  { This is here as we want normal minimising/maximising to continue too }
  DefaultHandler(Msg);
END; { WMSysCommand }

PROCEDURE TFWPRailWindow.FWPRailWindowDestroy(Sender: TObject);
BEGIN
  RailWindowBitmap.Free;
END; { FWPRailWindowDestroy }

PROCEDURE TFWPRailWindow.GeneralPopupMenuOnPopup(Sender: TObject);
BEGIN
  IF (FWPRailWindow.Top <> DefaultFWPRailWindowTop)
  OR (FWPRailWindow.Height <> DefaultFWPRailWindowHeight)
  OR (FWPRailWindow.Left <> DefaultFWPRailWindowLeft)
  OR (FWPRailWindow.Top <> DefaultFWPRailWindowTop)
  OR (FWPRailWindow.Width <> DefaultFWPRailWindowWidth)
  THEN
    GeneralPopupResetFWPRailWindowSizeAndPosition.Enabled := True
  ELSE
    GeneralPopupResetFWPRailWindowSizeAndPosition.Enabled := False;
END; { FWPRailWindowPopupMenuOnPopup }

PROCEDURE AddMenuItem(PopupMenu : TPopupMenu; Caption : String; PopupType : PopupTypes; Enabled : Boolean; Click : TNotifyEvent);
{ Add a dynamic menu item }
VAR
  MenuItemEx : TMenuItemExtended;

BEGIN
  MenuItemEx := TMenuItemExtended.Create(PopupMenu);
  MenuItemEx.Caption := Caption;
  MenuItemEx.Value := 'test string'; { not used }
  MenuItemEx.Enabled := Enabled;
  MenuItemEx.PopupType := PopupType;
  MenuItemEx.OnClick := Click;
  PopupMenu.Items.Add(MenuItemEx) ;
END; { AddMenuItem }

PROCEDURE TFWPRailWindow.SignalPopupItemClick(Sender: TObject);
CONST
  SaveVariables = True;

BEGIN
  WITH Signals[SignalPopupNum] DO BEGIN
    WITH sender As TMenuItemExtended DO BEGIN
      CASE PopupType OF
        SignalChangeDirectionPopupType:
          ChangeSignalDirection(SignalPopupNum);

        SignalDeletePopupType:
          DeleteSignal(SignalPopupNum);

        SignalEditPopupType:
          TurnEditModeOn(SignalPopupNum, UnknownPoint, UnknownBufferStop, UnknownLine, UnknownTrackCircuit);

        SignalOutOfUsePopupType:
          BEGIN
            IF NOT Signal_OutOfUse THEN                         { should these be here? ££££££££££ }
              Signal_OutOfUse := True
            ELSE BEGIN
              Signal_OutOfUse := False;
              Signal_Aspect := RedAspect;
            END;
            Signal_DataChanged := True;
            InvalidateScreen(UnitRef, 'SignalPopupItemClick SignalOutOfUsePopupType');
          END;

        SignalUndoChangesPopupType:
          UndoEditChanges;
      ELSE {CASE}
        Log('BG Invalid popup type ' + IntToStr(Tag) + ' in LinePopupItemClick');
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
      AddMenuItem(SignalPopupMenu, Caption, NoClickPopupType, NOT Enabled, NIL);
      AddMenuItem(SignalPopupMenu, '-', NoClickPopupType, Enabled, NIL);

      { ...and now the individual items }
      IF NOT Signal_OutOfUse THEN
        AddMenuItem(SignalPopupMenu, 'Set Signal Out Of Use', SignalOutOfUsePopupType, Enabled, SignalPopupItemClick)
      ELSE
        AddMenuItem(SignalPopupMenu, 'Set Signal Back In Use', SignalOutOfUsePopupType, Enabled, SignalPopupItemClick);

      AddMenuItem(SignalPopupMenu, '-', NoClickPopupType, Enabled, NIL);
      AddMenuItem(SignalPopupMenu, 'Edit Signal Details', SignalEditPopupType, Enabled, SignalPopupItemClick);
    END ELSE BEGIN
      { EditMode }

      { Add the caption... }
      IF SignalPopupNum = UnknownSignal THEN
        Caption := 'Editing Signals'
      ELSE
        Caption := 'Editing Signal ' + IntToStr(SignalPopupNum);
      AddMenuItem(SignalPopupMenu, Caption, NoClickPopupType, NOT Enabled, NIL);
      AddMenuItem(SignalPopupMenu, '-', NoClickPopupType, Enabled, NIL);

      { ...and now the individual items }
      IF NOT Signal_OutOfUse THEN
        AddMenuItem(SignalPopupMenu, 'Set Signal Out Of Use', SignalOutOfUsePopupType, Enabled, SignalPopupItemClick)
      ELSE
        AddMenuItem(SignalPopupMenu, 'Set Signal Back In Use', SignalOutOfUsePopupType, Enabled, SignalPopupItemClick);

      AddMenuItem(SignalPopupMenu, 'Change Signal Direction', SignalChangeDirectionPopupType, Enabled, SignalPopupItemClick);

      IF Signal_PreviousLineX <> 0 THEN
        { the signal has been moved }
        AddMenuItem(SignalPopupMenu, 'Undo Changes', SignalUndoChangesPopupType, Enabled, SignalPopupItemClick);

      AddMenuItem(SignalPopupMenu, '-', NoClickPopupType, Enabled, NIL);

      AddMenuItem(SignalPopupMenu, 'Delete Signal', SignalDeletePopupType, Enabled, SignalPopupItemClick);
    END;
  END; {WITH}
END; { SignalPopupMenuOnPopup }

PROCEDURE TFWPRailWindow.PointPopupItemClick(Sender: TObject) ;
BEGIN
  WITH Points[PointPopupNum] DO BEGIN
    WITH Sender As TMenuItemExtended DO BEGIN
      CASE PopupType OF
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

        PointEditPopupType:
          TurnEditModeOn(UnknownSignal, PointPopupNum, UnknownBufferStop, UnknownLine, UnknownTrackCircuit);
      ELSE {CASE}
        Log('BG Invalid popup type ' + IntToStr(Tag) + ' in LinePopupItemClick');
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
      { ...and now the individual items }
      IF PointPopupNum = UnknownPoint THEN
        Caption := 'Points'
      ELSE
        Caption := 'Point ' + IntToStr(PointPopupNum);
      AddMenuItem(PointPopupMenu, Caption, NoClickPopupType, NOT Enabled, NIL);
      AddMenuItem(PointPopupMenu, '-', NoClickPopupType, Enabled, NIL);

      { ...and now the individual items }
      IF Point_OutOfUse THEN
        AddMenuItem(PointPopupMenu, 'Set Point Back In Use', PointOutOfUsePopupType, Enabled, PointPopupItemClick)
      ELSE
        AddMenuItem(PointPopupMenu, 'Set Point Out Of Use', PointOutOfUsePopupType, Enabled, PointPopupItemClick);

      IF Point_ManualOperation THEN
        AddMenuItem(PointPopupMenu, 'Set Point To Manual', PointToManualPopupType, Enabled, PointPopupItemClick)
      ELSE
        AddMenuItem(PointPopupMenu, 'Set Point To Automatic', PointToManualPopupType, Enabled, PointPopupItemClick);

      IF Point_LockedByUser THEN
        AddMenuItem(PointPopupMenu, 'Unlock Point', PointUnlockPopupType, Enabled, PointPopupItemClick)
      ELSE
        AddMenuItem(PointPopupMenu, 'Lock Point', PointUnlockPopupType, Enabled, PointPopupItemClick);

      AddMenuItem(PointPopupMenu, 'Edit Point', PointEditPopupType, Enabled, PointPopupItemClick);
    END ELSE BEGIN
      { EditMode }

      { Add the caption... }
      IF (PointPopupNum = UnknownPoint) OR (EditedPoint = UnknownPoint) THEN
        Caption := 'Editing Points'
      ELSE
        Caption := 'Editing Point ' + IntToStr(PointPopupNum);
      AddMenuItem(PointPopupMenu, Caption, NoClickPopupType, NOT Enabled, NIL);
      AddMenuItem(PointPopupMenu, '-', NoClickPopupType, Enabled, NIL);

      { ...and now the individual items }
    END;
  END; {WITH}
END; { PointPopupMenuOnPopup }

PROCEDURE TFWPRailWindow.BufferStopPopupItemClick(Sender: TObject);
BEGIN
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
      IF BufferStopPopupNum = UnknownBufferStop THEN
        Caption := 'BufferStops'
      ELSE
        Caption := 'BufferStop ' + IntToStr(BufferStopPopupNum);
      AddMenuItem(BufferStopPopupMenu, Caption, NoClickPopupType, NOT Enabled, NIL);
      AddMenuItem(BufferStopPopupMenu, '-', NoClickPopupType, Enabled, NIL);

      { ...and now the individual items }
      AddMenuItem(BufferStopPopupMenu, 'Edit BufferStop', BufferStopEditPopupType, Enabled, BufferStopPopupItemClick)
    END ELSE BEGIN
      { EditMode }

      { Add the caption... }
      IF (BufferStopPopupNum = UnknownBufferStop) OR (EditedBufferStop = UnknownBufferStop) THEN
        Caption := 'Editing BufferStops'
      ELSE
        Caption := 'Editing BufferStop ' + IntToStr(BufferStopPopupNum);
      AddMenuItem(BufferStopPopupMenu, Caption, NoClickPopupType, NOT Enabled, NIL);
      AddMenuItem(BufferStopPopupMenu, '-', NoClickPopupType, Enabled, NIL);

      { ...and now the individual items }

    END;
  END; {WITH}
END; { BufferStopMenuOnPopup }

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

            SpeedStr := InputBox('Speed Restriction at TrackCircuit Occupation', 'Maximum Speed?', DefaultSpeedStr);
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
        IF InputQuery('TrackCircuit Occupation', 'Loco chip no?', InputQueryLocoChipStr)THEN BEGIN
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
VAR
  LineCount : Integer;
  T : TrainIndex;

BEGIN
  WITH Sender AS TMenuItemExtended DO BEGIN
    { this code has been separated out from the case statement below as we may well not be on a line, and the WITH Lines[LinePopupNum] would fail }
    IF PopupType = LineCreateLinePopupType THEN
      CreateLine
    ELSE BEGIN
      WITH Lines[LinePopupNum] DO BEGIN
        CASE PopupType OF
          LineAllocateLocoToTrackCircuitPopupType:
            IF TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip THEN
              AllocateLocoToTrackCircuit(LinePopupNum)
            ELSE
              ClearLocoFromTrackCircuit(Line_TC);

          LineChangeInternalLocoDirectionToUpPopupType:
            IF TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip THEN
              ChangeInternalLocoDirectionToUp(TrackCircuits[Line_TC].TC_LocoChip);

          LineChangeInternalLocoDirectionToDownPopupType:
            IF TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip THEN
              ChangeInternalLocoDirectionToDown(TrackCircuits[Line_TC].TC_LocoChip);

          LineCreateUpPointPopupType:
            CreatePoint(Up, LinePopupNum);

          LineCreateDownPointPopupType:
            CreatePoint(Down, LinePopupNum);

          LineCreateUpSignalPopupType:
            CreateSignal(Up, LinePopupNum);

          LineCreateDownSignalPopupType:
            CreateSignal(Down, LinePopupNum);

//          LineCreateLinePopupType:
//            CreateLine;

          LineEditPopupType:
            TurnEditModeOn(UnknownSignal, UnknownPoint, UnknownBufferStop, LinePopupNum, UnknownTrackCircuit);

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
              SetOrClearTrackCircuitSpeedRestriction(LinePopupNum);

          LineTCUnoccupiedPopupType:
            IF Line_TC <> UnknownTrackCircuit THEN BEGIN
              SetTrackCircuitState(Line_TC, TCUnoccupied);
              InvalidateScreen(UnitRef, 'LinePopupItemClick LineTCUnoccupiedPopupType');
            END;

          LineTCUserMustDrivePopupType:
            BEGIN
              IF TrackCircuits[Line_TC].TC_UserMustDrive THEN BEGIN
                TrackCircuits[Line_TC].TC_UserMustDrive := False;
                Log('T TC=' + IntToStr(Line_TC) + ' set to automatic operation');
              END ELSE BEGIN
                TrackCircuits[Line_TC].TC_UserMustDrive := True;
                Log('T TC=' + IntToStr(Line_TC) + ' set to manual operation');
              END;

              IF ShowTrackCircuitsWhereUserMustDrive THEN
                InvalidateScreen(UnitRef, 'LinePopupItemClick LineTCUserMustDrivePopupType');
            END;

        ELSE {CASE}
          Log('BG Invalid popup type ' + IntToStr(Tag) + ' in LinePopupItemClick');
        END; {CASE}
      END;
    END;
  END; {WITH}
END; { LinePopupItemClick }

PROCEDURE TFWPRailWindow.LinePopupMenuOnPopup(Sender: TObject);
VAR
  WhetherEnabled : Boolean;

BEGIN
  LinePopupMenu.Items.Clear;

  IF LinePopupNum = UnknownLine THEN BEGIN
    { we're not on a line, so all we can do is create a line here }
    IF NOT EditMode THEN
      beep
    ELSE BEGIN
      AddMenuItem(LinePopupMenu, 'Create a New Line', NoClickPopupType, NOT Enabled, NIL);
      AddMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, NIL);

      AddMenuItem(LinePopupMenu, 'Create Line', LineCreateLinePopupType, Enabled, LinePopupItemClick);
    END;
  END ELSE BEGIN
    WITH Lines[LinePopupNum] DO BEGIN
      IF NOT EditMode THEN BEGIN
        { Add the caption... }

        IF LinePopupNum = UnknownLine THEN
          Caption := 'Lines'
        ELSE
          Caption := 'Line ' + LineToStr(LinePopupNum) + ' ' + IfThen(Line_TC <> UnknownTrackCircuit,
                                                                      'TC' + IntToStr(Line_TC));
        AddMenuItem(LinePopupMenu, Caption, NoClickPopupType, NOT Enabled, NIL);
        AddMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, NIL);

        { ...and now the individual items }
        WhetherEnabled := TrackCircuits[Line_TC].TC_OccupationState <> TCUnOccupied;
        AddMenuItem(LinePopupMenu, 'Set Track Circuit To Unoccupied', LineTCUnoccupiedPopupType, WhetherEnabled, LinePopupItemClick);

        WhetherEnabled := TrackCircuits[Line_TC].TC_OccupationState <> TCFeedbackOccupation;
        AddMenuItem(LinePopupMenu, 'Set Track Circuit To Feedback Occupation', LineTCFeedbackOccupationPopupType, WhetherEnabled, LinePopupItemClick);

        WhetherEnabled := TrackCircuits[Line_TC].TC_OccupationState <> TCSystemOccupation;
        AddMenuItem(LinePopupMenu, 'Set Track Circuit To System Occupation', LineTCSystemOccupationPopupType, WhetherEnabled, LinePopupItemClick);

        WhetherEnabled := (TrackCircuits[Line_TC].TC_OccupationState <> TCPermanentFeedbackOccupation)
                                                                                            AND (TrackCircuits[Line_TC].TC_OccupationState <> TCPermanentOccupationSetByUser);
        AddMenuItem(LinePopupMenu, 'Set Track Circuit To Permanent Occupation', LineTCPermanentOccupationPopupType, WhetherEnabled, LinePopupItemClick);

        WhetherEnabled := TrackCircuits[Line_TC].TC_OccupationState <> TCOutOfUseSetByUser;
        AddMenuItem(LinePopupMenu, 'Set Track Circuit Out Of Use', LineTCOutOfUsePopupType, WhetherEnabled, LinePopupItemClick);

        IF TrackCircuits[Line_TC].TC_SpeedRestrictionInMPH = NoSpecifiedSpeed THEN
          AddMenuItem(LinePopupMenu, 'Set Track Circuit Speed Restriction', LineTCSpeedRestrictionPopupType, Enabled, LinePopupItemClick)
        ELSE
          AddMenuItem(LinePopupMenu, 'Clear Track Circuit Speed Restriction', LineTCSpeedRestrictionPopupType, Enabled, LinePopupItemClick);

        IF NOT TrackCircuits[Line_TC].TC_UserMustDrive THEN
           { how do we show this is in effect? ****** 6/8/14 }
          AddMenuItem(LinePopupMenu, 'Set Track Circuit To User Must Drive', LineTCUserMustDrivePopupType, Enabled, LinePopupItemClick)
        ELSE
          AddMenuItem(LinePopupMenu, 'Set Track Circuit To Auto', LineTCUserMustDrivePopupType, Enabled, LinePopupItemClick);

        AddMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, NIL);

        { Can't have both locations and lines out of use at the same time }
        IF (Line_Location <> UnknownLocation) AND Locations[Line_Location].Location_OutOfUse THEN BEGIN
          AddMenuItem(LinePopupMenu, 'Put Line ''' + LineToStr(Line_TC) + ''' Out Of Use', LineTCOutOfUsePopupType, NOT Enabled, LinePopupItemClick);
          AddMenuItem(LinePopupMenu, 'Return Location ''' + LocationToStr(Line_Location) + ''' To Use', LineLocationOutOfUsePopupType, Enabled, LinePopupItemClick);
        END ELSE
          IF Line_OutOfUseState = OutOfUse THEN BEGIN
            AddMenuItem(LinePopupMenu, 'Return Line ''' + LineToStr(Line_TC) + ''' To Use', LineTCOutOfUsePopupType, Enabled, LinePopupItemClick);
            AddMenuItem(LinePopupMenu, 'Put Location ''' + LocationToStr(Line_Location) + ''' Out Of Use', LineLocationOutOfUsePopupType, NOT Enabled, LinePopupItemClick);
          END ELSE BEGIN
            AddMenuItem(LinePopupMenu, 'Put Line ''' + LineToStr(Line_TC) + ''' Out Of Use', LineTCOutOfUsePopupType, Enabled, LinePopupItemClick);
            AddMenuItem(LinePopupMenu, 'Put Location ''' + LocationToStr(Line_Location) + ''' Out Of Use', LineLocationOutOfUsePopupType, Enabled, LinePopupItemClick);
          END;

        AddMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, NIL);

        IF TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip THEN
          AddMenuItem(LinePopupMenu, 'Allocate Loco To Track Circuit', LineAllocateLocoToTrackCircuitPopupType, Enabled, LinePopupItemClick)
        ELSE
          AddMenuItem(LinePopupMenu, 'Clear Loco Allocation From Track Circuit', LineAllocateLocoToTrackCircuitPopupType, Enabled, LinePopupItemClick);

        IF (Line_TC = UnknownTrackCircuit) OR (TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip) THEN BEGIN
          AddMenuItem(LinePopupMenu, 'Change Internal Loco Chip Direction to Up', LineChangeInternalLocoDirectionToUpPopupType, Enabled, LinePopupItemClick);
          AddMenuItem(LinePopupMenu, 'Change Internal Loco Chip Direction to Down', LineChangeInternalLocoDirectionToDownPopupType, Enabled, LinePopupItemClick);
        END ELSE BEGIN
          AddMenuItem(LinePopupMenu, 'Change ' + LocoChipToStr(TrackCircuits[Line_TC].TC_LocoChip) + '''s Internal Loco Chip Direction to Up',
                                                                                                 LineChangeInternalLocoDirectionToUpPopupType, Enabled, LinePopupItemClick);
          AddMenuItem(LinePopupMenu, 'Change ' + LocoChipToStr(TrackCircuits[Line_TC].TC_LocoChip) + '''s Internal Loco Chip Direction to Down',
                                                                                               LineChangeInternalLocoDirectionToDownPopupType, Enabled, LinePopupItemClick);
        END;
        WhetherEnabled := TrackCircuits[Line_TC].TC_LocoChip <> UnknownLocoChip;
        AddMenuItem(LinePopupMenu, 'Show Loco''s Last Error Message', LineShowLocoLastErrorMessagePopupType, WhetherEnabled, LinePopupItemClick);

        AddMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, NIL);

        AddMenuItem(LinePopupMenu, 'Edit Line', LineEditPopupType, Enabled, LinePopupItemClick)
      END ELSE BEGIN
        { EditMode }

        { Add the caption... }
        IF (LinePopupNum = UnknownLine) OR (EditedLine = UnknownLine) THEN
          Caption := 'Editing Lines'
        ELSE
          Caption := 'Editing Line ' + LineToStr(LinePopupNum) + ' ' + IfThen(Line_TC <> UnknownTrackCircuit,
                                                                              'TC' + IntToStr(Line_TC));
        AddMenuItem(LinePopupMenu, Caption, NoClickPopupType, NOT Enabled, NIL);
        AddMenuItem(LinePopupMenu, '-', NoClickPopupType, Enabled, NIL);

        { ...and now the individual items }
        WhetherEnabled := SignalAdjacentLineOK(LinePopupNum);
        AddMenuItem(LinePopupMenu, 'Create Line', LineCreateLinePopupType, WhetherEnabled, LinePopupItemClick);

        AddMenuItem(LinePopupMenu, 'Create Up Signal', LineCreateUpSignalPopupType, WhetherEnabled, LinePopupItemClick);
        AddMenuItem(LinePopupMenu, 'Create Down Signal', LineCreateDownSignalPopupType, WhetherEnabled, LinePopupItemClick);
      END;
    END; {WITH}
  END;
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

PROCEDURE TFWPRailWindow.GeneralPopupChangePointLockedBySystemColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := ShowPointLockedColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    ShowPointLockedColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointLockedColourClick');
  END;
END; { GeneralPopupChangePointLockedBySystemColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointLockedBySystemColourClick(Sender: TObject);
BEGIN
  ShowPointLockedColour := DefaultShowPointLockedColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointLockedColourClick');
END; { GeneralPopupRestorePointLockedBySystemColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointLockedByUserColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointLockedByUserColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointLockedByUserColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointLockedByUserColourClick');
  END;
END; { GeneralPopupChangePointLockedByUserColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointLockedByUserColourClick(Sender: TObject);
BEGIN
  PointLockedByUserColour := DefaultPointLockedByUserColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointLockedByUserColourClick');
END; { GeneralPopupRestorePointLockedByUserColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeDefaultPointColourClick(Sender: TObject);
BEGIN
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointDefaultColourClick');
  END;
END; { GeneralPopupChangePointDefaultColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointDefaultColourClick(Sender: TObject);
BEGIN
  PointColour := DefaultPointColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointDefaultColourClick');
END; { GeneralPopupRestorePointDefaultColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreDefaultBackgroundColourClick(Sender: TObject);
BEGIN
  BackgroundColour := DefaultBackgroundColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreDefaultBackgroundColourClick');
END; { GeneralPopupRestoreDefaultBackgroundColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeBackgroundColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := BackgroundColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    BackgroundColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeBackgroundColourClick');
  END;
END; { GeneralPopupChangeBackgroundColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeForegroundColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := ForegroundColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    ForegroundColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeForegroundColourClick');
  END;
END; { GeneralPopupChangeForegroundColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreForegroundColourClick(Sender: TObject);
BEGIN
  ForegroundColour := DefaultForegroundColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreForegroundColourClick');
END; { GeneralPopupRestoreForegroundColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePlatformColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PlatformColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PlatformColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePlatformColourClick');
  END;
END; { GeneralPopupChangePlatformColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePlatformColourClick(Sender: TObject);
BEGIN
  PlatformColour := DefaultPlatformColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePlatformColourClick');
END; { GeneralPopupRestorePlatformColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCMissingOccupationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCMissingOccupationColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCMissingOccupationColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCMissingOccupationColourClick');
  END;
END; { GeneralPopupChangeMissingOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCMissingOccupationColourClick(Sender: TObject);
BEGIN
  TCMissingOccupationColour := DefaultTCMissingOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCMissingOccupationColourClick');
END; { GeneralPopupRestoreTCMissingOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeLocoStalledColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := LocoStalledColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    LocoStalledColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeLocoStalledColourClick');
  END;
END; { GeneralPopupChangeLocoStalledColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreLocoStalledColourClick(Sender: TObject);
BEGIN
  LocoStalledColour := DefaultLocoStalledColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreLocoStalledColourClick');
END; { GeneralPopupRestoreLocoStalledColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCPermanentFeedbackOccupationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCPermanentFeedbackOccupationColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCPermanentFeedbackOccupationColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCPermanentFeedbackOccupationColourClick');
  END;
END; { GeneralPopupChangeTCPermanentFeedbackOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCPermanentFeedbackOccupationColourClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationColour := DefaultTCPermanentFeedbackOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentFeedbackOccupationColourClick');
END; { GeneralPopupRestoreTCPermanentFeedbackOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCPermanentSystemOccupationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCPermanentSystemOccupationColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCPermanentSystemOccupationColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCPermanentSystemOccupationColourClick');
  END;
END; { GeneralPopupChangeTCPermanentSystemOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCPermanentSystemOccupationColourClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationColour := DefaultTCPermanentSystemOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentSystemOccupationColourClick');
END; { GeneralPopupRestoreTCPermanentSystemOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCPermanentOccupationSetByUserColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCPermanentOccupationSetByUserColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCPermanentOccupationSetByUserColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCPermanentOccupationSetByUserColourClick');
  END;
END; { GeneralPopupChangeTCPermanentOccupationSetByUserColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCPermanentOccupationSetByUserColourClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserColour := DefaultTCPermanentOccupationSetByUserColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentOccupationSetByUserColourClick');
END; { GeneralPopupRestoreTCPermanentOccupationSetByUserColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCSpeedRestrictionColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCSpeedRestrictionColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCSpeedRestrictionColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCSpeedRestrictionColourClick');
  END;
END; { GeneralPopupChangeTCSpeedRestrictionColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCSpeedRestrictionColourClick(Sender: TObject);
BEGIN
  TCSpeedRestrictionColour := DefaultTCSpeedRestrictionColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCSpeedRestrictionColourClick');
END; { GeneralPopupRestoreTCSpeedRestrictionColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCLocoOutOfPlaceColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCLocoOutOfPlaceOccupationColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCLocoOutOfPlaceOccupationColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCLocoOutOfPlaceColourClick');
  END;
END; { GeneralPopupChangeTCLocoOutOfPlaceColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCLocoOutOfPlaceColourClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationColour := DefaultTCLocoOutOfPlaceOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCLocoOutOfPlaceColourClick');
END; { GeneralPopupRestoreTCLocoOutOfPlaceColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCFeedbackDataInUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCFeedbackDataInUseColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCFeedbackDataInUseColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCFeedbackDataInUseColourClick');
  END;
END; { GeneralPopupChangeTCFeedbackDataInUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCFeedbackDataInUseColourClick(Sender: TObject);
BEGIN
  TCFeedbackDataInUseColour := DefaultTCFeedbackDataInUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCFeedbackDataInUseColourClick');
END; { GeneralPopupRestoreTCFeedbackDataInUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCFeedbackDataOutOfUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCFeedbackDataOutOfUseColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCFeedbackDataOutOfUseColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCFeedbackDataOutOfUseColourClick');
  END;
END; { GeneralPopupChangeTCFeedbackDataOutOfUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCFeedbackDataOutOfUseColourClick(Sender: TObject);
BEGIN
  TCFeedbackDataOutOfUseColour := DefaultTCFeedbackDataOutOfUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCFeedbackDataOutOfUseColourClick');
END; { GeneralPopupRestoreTCFeedbackDataOutOfUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCFeedbackOccupationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCFeedbackOccupationColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCFeedbackOccupationColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCFeedbackOccupationColourClick');
  END;
END; { GeneralPopupChangeFeedbackOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCFeedbackOccupationColourClick(Sender: TObject);
BEGIN
  TCFeedbackOccupationColour := DefaultTCFeedbackOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCFeedbackOccupationColourClick');
END; { GeneralPopupRestoreTCFeedbackOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCFeedbackOccupationButOutOfUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCFeedbackOccupationButOutOfUseColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCFeedbackOccupationButOutOfUseColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCFeedbackOccupationButOutOfUseColourClick');
  END;
END; { GeneralPopupChangeFeedbackOccupationButOutOfUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCFeedbackOccupationButOutOfUseColourClick(Sender: TObject);
BEGIN
  TCFeedbackOccupationButOutOfUseColour := DefaultTCFeedbackOccupationButOutOfUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCFeedbackOccupationButOutOfUseColourClick');
END; { GeneralPopupRestoreTCFeedbackOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCSystemOccupationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCSystemOccupationColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCSystemOccupationColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCSystemOccupationColourClick');
  END;
END; { GeneralPopupChangeTCSystemOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCSystemOccupationColourClick(Sender: TObject);
BEGIN
  TCSystemOccupationColour := DefaultTCSystemOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCSystemOccupationColourClick');
END; { GeneralPopupRestoreTCSystemOccupationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCUnoccupiedColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCUnoccupiedColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCUnoccupiedColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCUnoccupiedColourClick');
  END;
END; { GeneralPopupChangeTCUnoccupiedColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCUnoccupiedColourClick(Sender: TObject);
BEGIN
  TCUnoccupiedColour := DefaultTCUnoccupiedColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCUnoccupiedColourClick');
END; { GeneralPopupRestoreTCUnoccupiedColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCOutOfUseSetByUserColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCOutOfUseSetByUserColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCOutOfUseSetByUserColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCOutOfUseSetByUserColourClick');
  END;
END; { GeneralPopupChangeTCOutOfUseSetByUserColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCOutOfUseSetByUserColourClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserColour := DefaultTCOutOfUseSetByUserColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCOutOfUseSetByUserColourClick');
END; { GeneralPopupRestoreTCOutOfUseSetByUserColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCUserMustDriveColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCUserMustDriveColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCUserMustDriveColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCUserMustDriveColourClick');
  END;
END; { GeneralPopupChangeTCUserMustDriveColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCUserMustDriveColourClick(Sender: TObject);
BEGIN
  TCUserMustDriveColour := DefaultTCUserMustDriveColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCUserMustDriveColourClick');
END; { GeneralPopupRestoreTCUserMustDriveColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeScreenComponentEditedColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := ScreenComponentEditedColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    ScreenComponentEditedColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeScreenComponentEditedColourClick');
  END;
END; { GeneralPopupChangeScreenComponentEditedColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreScreenComponentEditedColourClick(Sender: TObject);
BEGIN
  ScreenComponentEditedColour := DefaultScreenComponentEditedColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreScreenComponentEditedColourClick');
END; { GeneralPopupRestoreScreenComponentEditedColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTCOutOfUseAsNoFeedbackReceivedColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TCOutOfUseAsNoFeedbackReceivedColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TCOutOfUseAsNoFeedbackReceivedColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCOutOfUseAsNoFeedbackReceivedColourClick');
  END;
END; { GeneralPopupChangeMysteryOccupationColour2Click }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedColourClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedColour := DefaultTCOutOfUseAsNoFeedbackReceivedColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedColourClick');
END; { GeneralPopupRestoreTCOutOfUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeBufferStopColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := BufferStopColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    BufferStopColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeBufferStopColourClick');
  END;
END; { GeneralPopupChangeBufferStopColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreBufferStopColourClick(Sender: TObject);
BEGIN
  BufferStopColour := DefaultBufferStopColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreBufferStopColourClick');
END; { GeneralPopupRestoreBufferStopColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeBufferStopNumberColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := BufferStopNumberColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    BufferStopNumberColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeBufferStopNumberColourClick');
  END;
END; { GeneralPopupChangeBufferStopNumberColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreBufferStopNumberColourClick(Sender: TObject);
BEGIN
  BufferStopNumberColour := DefaultBufferStopNumberColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreBufferStopNumberColourClick');
END; { GeneralPopupRestoreBufferStopNumberColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeBufferStopRedClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := BufferStopRed;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    BufferStopRed := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupRestoreBufferStopNumberColourClick');
  END;
END; { GeneralPopupChangeBufferStopRedClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreBufferStopRedClick(Sender: TObject);
BEGIN
  BufferStopRed := DefaultBufferStopRed;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreBufferStopRedClick');
END; { GeneralPopupRestoreBufferStopRedClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeLineNotAvailableColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := LineNotAvailableColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    LineNotAvailableColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeLineNotAvailableColourClick');
  END;
END; { GeneralPopupChangeLineNotAvailableColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreLineNotAvailableColourClick(Sender: TObject);
BEGIN
  LineNotAvailableColour := DefaultLineNotAvailableColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreLineNotAvailableColourClick');
END; { GeneralPopupRestoreLineNotAvailableColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePlungerPressedColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TRSPlungerPressedColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TRSPlungerPressedColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePlungerPressedColourClick');
  END;
END; { GeneralPopupChangePlungerPressedColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePlungerPressedColourClick(Sender: TObject);
BEGIN
  TRSPlungerPressedColour := DefaultTRSPlungerPressedColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePlungerPressedColourClick');
END; { GeneralPopupRestorePlungerPressedColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeSignalAspectUnlitColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := SignalAspectUnlit;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    SignalAspectUnlit := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalAspectUnlitColourClick');
  END;
END; { GeneralPopupChangeSignalAspectUnlitClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreSignalAspectUnlitColourClick(Sender: TObject);
BEGIN
  SignalAspectUnlit := DefaultSignalAspectUnlit;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalAspectUnlitColourClick');
END; { GeneralPopupRestoreSignalAspectUnlitColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeSignalAspectYellowClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := SignalAspectYellow;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    SignalAspectYellow := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalAspectYellowClick');
  END;
END; { GeneralPopupChangeSignalAspectYellowClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreSignalAspectYellowClick(Sender: TObject);
BEGIN
  SignalAspectYellow := DefaultSignalAspectYellow;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalAspectYellowClick');
END; { GeneralPopupRestoreSignalAspectYellowClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeSignalAspectRedClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := SignalAspectRed;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    SignalAspectRed := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalAspectRedClick');
  END;
END; { GeneralPopupChangeSignalAspectRedClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreSignalAspectRedClick(Sender: TObject);
BEGIN
  SignalAspectRed := DefaultSignalAspectRed;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalAspectRedClick');
END; { GeneralPopupRestoreSignalAspectRedClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeSignalAspectGreenClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := SignalAspectGreen;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    SignalAspectGreen := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalAspectGreenClick');
  END;
END; { GeneralPopupChangeSignalAspectGreenClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreSignalAspectGreenClick(Sender: TObject);
BEGIN
  SignalAspectGreen := DefaultSignalAspectGreen;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalAspectGreenClick');
END; { GeneralPopupRestoreSignalAspectGreenClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeSignalPostRouteSettingColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := SignalPostRouteSettingColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    SignalPostRouteSettingColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalPostRouteSettingColourClick');
  END;
END; { GeneralPopupChangeSignalPostRouteSettingColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreSignalPostRouteSettingColourClick(Sender: TObject);
BEGIN
  SignalPostRouteSettingColour := DefaultSignalPostRouteSettingColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalPostRouteSettingColourClick');
END; { GeneralPopupRestoreSignalPostRouteSettingColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeSignalPostEmergencyRouteSettingColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := SignalPostRouteSettingColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    SignalPostRouteSettingColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalPostEmergencyRouteSettingColourClick');
  END;
END; { GeneralPopupChangeSignalPostEmergencyRouteSettingColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreSignalPostEmergencyRouteSettingColourClick(Sender: TObject);
BEGIN
  SignalPostRouteSettingColour := DefaultSignalPostRouteSettingColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalPostEmergencyRouteSettingColourClick');
END; { GeneralPopupRestoreSignalPostEmergencyRouteSettingColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeSignalPostTheatreSettingColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := SignalPostTheatreSettingColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    SignalPostTheatreSettingColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalPostTheatreSettingColourClick');
  END;
END; { GeneralPopupChangePostTheatreSettingColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreSignalPostTheatreSettingColourClick(Sender: TObject);
BEGIN
  SignalPostTheatreSettingColour := DefaultSignalPostTheatreSettingColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalPostTheatreSettingColourClick');
END; { GeneralPopupRestoreSignalPostTheatreSettingColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeSignalNumberColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := SignalNumberColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    SignalNumberColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalNumberColourClick');
  END;
END; { GeneralPopupChangeSignalberColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreSignalNumberColourClick(Sender: TObject);
BEGIN
  SignalNumberColour := DefaultSignalNumberColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalNumberColourClick');
END; { GeneralPopupRestoreSignalNumberColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTrainActiveColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TrainActiveColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TrainActiveColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTrainActiveColourClick');
  END;
END; { GeneralPopupChangeTrainActiveColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTrainActiveColourClick(Sender: TObject);
BEGIN
  TrainActiveColour := DefaultTrainActiveColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTrainActiveColourClick');
END; { GeneralPopupRestoreTrainActiveColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeTrainInactiveColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TrainInactiveColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TrainInactiveColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTrainInactiveColourClick');
  END;
END; { GeneralPopupChangeTrainInactiveColourClick }

procedure TFWPRailWindow.GeneralPopupClockClick(Sender: TObject);
begin
;
end;

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTrainInactiveColourClick(Sender: TObject);
BEGIN
  TrainInactiveColour := DefaultTrainInactiveColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTrainInactiveColourClick');
END; { GeneralPopupRestoreTrainInactiveColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointUpFacingColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointUpFacingColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointUpFacingColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointUpFacingColourClick');
  END;
END; { GeneralPopupChangeUpFacingPointColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointUpFacingColourClick(Sender: TObject);
BEGIN
  PointUpFacingColour := DefaultPointUpFacingColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointUpFacingColourClick');
END; { GeneralPopupRestorePointUpFacingColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointDownFacingColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointDownFacingColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointDownFacingColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointDownFacingColourClick');
  END;
END; { GeneralPopupChangeDownFacingPointColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointDownFacingColourClick(Sender: TObject);
BEGIN
  PointDownFacingColour := DefaultPointDownFacingColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointDownFacingColourClick');
END; { GeneralPopupRestorePointDownFacingColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeShowPointDefaultStateColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := ShowPointDefaultStateColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    ShowPointDefaultStateColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeShowPointDefaultStateColourClick');
  END;
END; { GeneralPopupChangeShowPointDefaultStateColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreShowPointDefaultStateColourClick(Sender: TObject);
BEGIN
  ShowPointDefaultStateColour := DefaultShowPointDefaultStateColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreShowPointDefaultStateColourClick');
END; { GeneralPopupRestoreShowPointDefaultStateColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointLenzNumberColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointLenzNumberColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointLenzNumberColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointLenzNumberColourClick');
  END;
END; { GeneralPopupChangePointLenzNumberColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointLenzNumberColourClick(Sender: TObject);
BEGIN
  PointLenzNumberColour := DefaultPointLenzNumberColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointLenzNumberColourClick');
END; { GeneralPopupRestorePointLenzNumberColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointManualOperationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointManualOperationColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointManualOperationColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointManualOperationColourClick');
  END;
END; { GeneralPopupChangePointManualOperationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointManualOperationColourClick(Sender: TObject);
BEGIN
  PointManualOperationColour := DefaultPointManualOperationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointManualOperationColourClick');
END; { GeneralPopupRestorePointManualOperationColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointFeedbackDataInUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointFeedbackDataInUseColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointFeedbackDataInUseColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointFeedbackDataInUseColourClick');
  END;
END; { GeneralPopupChangePointFeedbackDataInUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointFeedbackDataInUseColourClick(Sender: TObject);
BEGIN
  PointFeedbackDataInUseColour := DefaultPointFeedbackDataInUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointFeedbackDataInUseColourClick');
END; { GeneralPopupRestorePointFeedbackDataInUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointFeedbackDataOutOfUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointFeedbackDataOutOfUseColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointFeedbackDataOutOfUseColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointFeedbackDataOutOfUseColourClick');
  END;
END; { GeneralPopupChangePointFeedbackDataOutOfUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointFeedbackDataOutOfUseColourClick(Sender: TObject);
BEGIN
  PointFeedbackDataOutOfUseColour := DefaultPointFeedbackDataOutOfUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointFeedbackDataOutOfUseColourClick');
END; { GeneralPopupRestorePointFeedbackDataOutOfUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointsWithoutFeedbackColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointsWithoutFeedbackColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointsWithoutFeedbackColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointsWithoutFeedbackColourClick');
  END;
END; { GeneralPopupChangePointsWithoutFeedbackColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointsWithoutFeedbackColourClick(Sender: TObject);
BEGIN
  PointsWithoutFeedbackColour := DefaultPointsWithoutFeedbackColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointsWithoutFeedbackColourClick');
END; { GeneralPopupRestorePointsWithoutFeedbackColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointHeelLineColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointHeelLineColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointHeelLineColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointHeelLineColourClick');
  END;
END; { GeneralPopupChangePointHeelLineColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointHeelLineColourClick(Sender: TObject);
BEGIN
  PointHeelLineColour := DefaultPointHeelLineColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointHeelLineColourClick');
END; { GeneralPopupRestorePointHeelLineColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointStraightLineColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointStraightLineColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointStraightLineColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointStraightLineColourClick');
  END;
END; { GeneralPopupChangePointStraightLineColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointStraightLineColourClick(Sender: TObject);
BEGIN
  PointStraightLineColour := DefaultPointStraightLineColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointStraightLineColourClick');
END; { GeneralPopupRestorePointStraightLineColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangeLineRoutedOverColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := LineRoutedOverColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    LineRoutedOverColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeLineRoutedOverColourClick');
  END;
END; { GeneralPopupChangeLineRoutedOverColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreLineRoutedOverColourClick(Sender: TObject);
BEGIN
  LineRoutedOverColour := DefaultLineRoutedOverColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreLineRoutedOverColourClick');
END; { GeneralPopupRestoreLineRoutedOverColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointDivergingLineColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointDivergingLineColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointDivergingLineColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointDivergingLineColourClick');
  END;
END; { GeneralPopupChangePointDivergingLineColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointDivergingLineColourClick(Sender: TObject);
BEGIN
  PointDivergingLineColour := DefaultPointDivergingLineColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointDivergingLineColourClick');
END; { GeneralPopupRestorePointDivergingLineColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointUndrawColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointUndrawColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointUndrawColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointUndrawColourClick');
  END;
END; { GeneralPopupChangePointUndrawColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointUndrawColourClick(Sender: TObject);
BEGIN
  PointUndrawColour := DefaultPointUndrawColour;
  InvalidateScreen(UnitRef, 'END; { GeneralPopupChangePointU');
END; { GeneralPopupRestorePointUndrawColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointOutOfUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := PointOutOfUseColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    PointOutOfUseColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointOutOfUseColourClick');
  END;
END; { GeneralPopupChangePointOutOfUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePointOutOfUseColourClick(Sender: TObject);
BEGIN
  PointOutOfUseColour := DefaultPointOutOfUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointOutOfUseColourClick');
END; { GeneralPopupRestorePointOutOfUseColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupChangePlungerColourClick(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TRSPlungerColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TRSPlungerColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePlungerColourClick');
  END;
END; { GeneralPopupChangePlungerColourClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePlungerColour(Sender: TObject);
BEGIN
  TRSPlungerColour := DefaultTRSPlungerColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePlungerColour');
END; { GeneralPopupRestorePlungerColour }

PROCEDURE TFWPRailWindow.GeneralPopupChangePlungerOutlineColour(Sender: TObject);
BEGIN
  { Show the default }
  FWPRailWindowColourDialogue.Color := TRSPlungerOutlineColour;
  { Allow the user to change it }
  IF FWPRailWindowColourDialogue.Execute THEN BEGIN
    TRSPlungerOutlineColour := FWPRailWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePlungerOutlineColour');
  END;
END; { GeneralPopupChangePlungerOutlineColour }

PROCEDURE TFWPRailWindow.GeneralPopupRestorePlungerOutlineColour(Sender: TObject);
BEGIN
  TRSPlungerOutlineColour := DefaultTRSPlungerOutlineColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePlungerOutlineColour');
END; { GeneralPopupRestorePlungerOutlineColour }

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
  ScreenComponentEditedColour := DefaultScreenComponentEditedColour;
  ShowPointDefaultStateColour := DefaultShowPointDefaultStateColour;
  ShowPointLockedColour := DefaultShowPointLockedColour;
  SignalAspectGreen := DefaultSignalAspectGreen;
  SignalAspectRed := DefaultSignalAspectRed;
  SignalAspectUnlit := DefaultSignalAspectUnlit;
  SignalAspectYellow := DefaultSignalAspectYellow;
  SignalNumberColour := DefaultSignalNumberColour;
  SignalPostColour := DefaultSignalPostColour;
  SignalPostRouteSettingColour := DefaultSignalPostRouteSettingColour;
  SignalPostTheatreSettingColour := DefaultSignalPostTheatreSettingColour;
  TCMissingOccupationColour := DefaultTCMissingOccupationColour;
  TCFeedbackOccupationColour := DefaultTCFeedbackOccupationColour;
  TCOutOfUseSetByUserColour := DefaultTCOutOfUseSetByUserColour;
  TCOutOfUseAsNoFeedbackReceivedColour := DefaultTCOutOfUseAsNoFeedbackReceivedColour;
  TCPermanentFeedbackOccupationColour := DefaultTCPermanentFeedbackOccupationColour;
  TCPermanentOccupationSetByUserColour := DefaultTCPermanentOccupationSetByUserColour;
  TCPermanentSystemOccupationColour := DefaultTCPermanentSystemOccupationColour;
  TCSystemOccupationColour := DefaultTCSystemOccupationColour;
  TCUnoccupiedColour := DefaultTCUnoccupiedColour;
  TCUserMustDriveColour := DefaultTCUserMustDriveColour;
  TrainActiveColour := DefaultTrainActiveColour;
  TrainInactiveColour := DefaultTrainInactiveColour;
  TRSPlungerColour := DefaultTRSPlungerColour;
  TRSPlungerOutlineColour := DefaultTRSPlungerOutlineColour;
  TRSPlungerPressedColour := DefaultTRSPlungerPressedColour;

  InvalidateScreen(UnitRef, 'ResetScreenColoursToDefault');
END; { ResetScreenColoursToDefault }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreAllDefaultColoursClick(Sender: TObject);
BEGIN
  ResetScreenColoursToDefault;
END; { RestoreAllDefaultColoursClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreAllProgramDefaultSettingsClick(Sender: TObject);
BEGIN

END;{ GeneralPopupRestoreAllProgramDefaultSettingsClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreAllScreenDrawingDefaultSettingsClick(Sender: TObject);
BEGIN
  RestoreScreenDefaults;
END; { GeneralPopupRestoreAllScreenDrawingDefaultSettingsClick }

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
  PointDivergingLineColour := SavePointDivergingLineColourForPrinting;
  PointDownFacingColour := SavePointDownFacingColourForPrinting;
  PointFeedbackDataInUseColour := SavePointFeedbackDataInUseColourForPrinting;
  PointFeedbackDataOutOfUseColour := SavePointFeedbackDataOutOfUseColourForPrinting;
  PointHeelLineColour := SavePointHeelLineColourForPrinting;
  PointManualOperationColour := SavePointManualOperationColourForPrinting;
  PointStraightLineColour := SavePointStraightLineColourForPrinting;
  PointsWithoutFeedbackColour := SavePointsWithoutFeedbackColourForPrinting;
  PointUpFacingColour := SavePointUpFacingColourForPrinting;
  ScreenComponentEditedColour := SaveScreenComponentEditedColourForPrinting;
  ShowPointDefaultStateColour := SaveShowPointDefaultStateColourForPrinting;
  ShowPointLockedColour := SaveShowPointLockedColourForPrinting;
  SignalNumberColour := SaveSignalNumberColourForPrinting;
  SignalPostColour := SaveSignalPostColourForPrinting;
  SignalPostRouteSettingColour := SaveSignalPostRouteSettingColourForPrinting;
  SignalPostTheatreSettingColour := SaveSignalPostTheatreSettingColourForPrinting;
  TCMissingOccupationColour := SaveTCMissingOccupationColourForPrinting;
  TCFeedbackOccupationColour := SaveTCFeedbackOccupationColourForPrinting;
  TCOutOfUseSetByUserColour := SaveTCOutOfUseSetByUserColourForPrinting;
  TCOutOfUseAsNoFeedbackReceivedColour := SaveTCOutOfUseAsNoFeedbackReceivedColourForPrinting;
  TCPermanentFeedbackOccupationColour := SaveTCPermanentFeedbackOccupationColourForPrinting;
  TCPermanentOccupationSetByUserColour := SaveTCPermanentOccupationSetByUserColourForPrinting;
  TCPermanentSystemOccupationColour := SaveTCPermanentSystemOccupationColourForPrinting;
  TCSystemOccupationColour := SaveTCSystemOccupationColourForPrinting;
  TCUnoccupiedColour := SaveTCUnoccupiedColourForPrinting;
  TCUserMustDriveColour := SaveTCUserMustDriveColourForPrinting;
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

  SaveScreenComponentEditedColourForPrinting := ScreenComponentEditedColour;
  ScreenComponentEditedColour := clBlack;

  SaveShowPointDefaultStateColourForPrinting := ShowPointDefaultStateColour;
  ShowPointDefaultStateColour := clBlack;

  SaveShowPointLockedColourForPrinting := ShowPointLockedColour;
  ShowPointLockedColour := clBlack;

  SaveSignalNumberColourForPrinting := SignalNumberColour;
  SignalNumberColour := clBlack;

  SaveSignalPostColourForPrinting := SignalPostColour;
  SignalPostColour := clBlack;

  SaveSignalPostRouteSettingColourForPrinting := SignalPostRouteSettingColour;
  SignalPostRouteSettingColour := clBlack;

  SaveSignalPostTheatreSettingColourForPrinting := SignalPostTheatreSettingColour;
  SignalPostTheatreSettingColour := clBlack;

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

  SaveTCUserMustDriveColourForPrinting := TCUserMustDriveColour;
  TCUserMustDriveColour := clBlack;

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

PROCEDURE TFWPRailWindow.GeneralPopupSidingPenStyleSolidClick(Sender: TObject);
BEGIN
  SidingPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleSolidClick');
END; { GeneralPopupSidingPenStyleSolidClick }

PROCEDURE TFWPRailWindow.GeneralPopupSidingPenStyleDashClick(Sender: TObject);
BEGIN
  SidingPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleDashClick');
END; { GeneralPopupSidingPenStyleDashClick }

PROCEDURE TFWPRailWindow.GeneralPopupSidingPenStyleDotClick(Sender: TObject);
BEGIN
  SidingPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleDotClick');
END; { GeneralPopupSidingPenStyleDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupSidingPenStyleDashDotClick(Sender: TObject);
BEGIN
  SidingPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleDashDotClick');
END; { GeneralPopupSidingPenStyleDashDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupSidingPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  SidingPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleDashDotDotClick');
END; { GeneralPopupSidingPenStyleDashDotDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupSidingPenStyleClearClick(Sender: TObject);
BEGIN
  SidingPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleClearClick');
END; { GeneralPopupSidingPenStyleClearClick }

PROCEDURE TFWPRailWindow.GeneralPopupSidingPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  SidingPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleInsideFrameClick');
END; { GeneralPopupSidingPenStyleInsideFrameClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreSidingPenStyleClick(Sender: TObject);
BEGIN
  SidingPenStyle := DefaultSidingPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSidingPenStyleClick');
END; { GeneralPopupRestoreSidingPenStyleClick }

PROCEDURE TFWPRailWindow.GeneralPopupFiddleyardLinePenStyleSolidClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleSolidClick');
END; { GeneralPopupFiddleyardLinePenStyleSolidClick }

PROCEDURE TFWPRailWindow.GeneralPopupFiddleyardLinePenStyleDashClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleDashClick');
END; { GeneralPopupFiddleyardLinePenStyleDashClick }

PROCEDURE TFWPRailWindow.GeneralPopupFiddleyardLinePenStyleDotClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleDotClick');
END; { GeneralPopupFiddleyardLinePenStyleDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupFiddleyardLinePenStyleDashDotClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleDashDotClick');
END; { GeneralPopupFiddleyardLinePenStyleDashDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupFiddleyardLinePenStyleDashDotDotClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleDashDotDotClick');
END; { GeneralPopupFiddleyardLinePenStyleDashDotDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupFiddleyardLinePenStyleClearClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleClearClick');
END; { GeneralPopupFiddleyardLinePenStyleClearClick }

PROCEDURE TFWPRailWindow.GeneralPopupFiddleyardLinePenStyleInsideFrameClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleInsideFrameClick');
END; { GeneralPopupFiddleyardLinePenStyleInsideFrameClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreFiddleyardLinePenStyleClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := DefaultFiddleyardLinePenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreFiddleyardLinePenStyleClick');
END; { GeneralPopupRestoreFiddleyardLinePenStyleClick }

PROCEDURE TFWPRailWindow.GeneralPopupProjectedLinePenStyleSolidClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleSolidClick');
END; { GeneralPopupProjectedLinePenStyleSolidClick }

PROCEDURE TFWPRailWindow.GeneralPopupProjectedLinePenStyleDashClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleDashClick');
END; { GeneralPopupProjectedLinePenStyleDashClick }

PROCEDURE TFWPRailWindow.GeneralPopupProjectedLinePenStyleDotClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleDotClick');
END; { GeneralPopupProjectedLinePenStyleDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupProjectedLinePenStyleDashDotClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleDashDotClick');
END; { GeneralPopupProjectedLinePenStyleDashDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupProjectedLinePenStyleDashDotDotClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleDashDotDotClick');
END; { GeneralPopupProjectedLinePenStyleDashDotDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupProjectedLinePenStyleClearClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleClearClick');
END; { GeneralPopupProjectedLinePenStyleClearClick }

PROCEDURE TFWPRailWindow.GeneralPopupProjectedLinePenStyleInsideFrameClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleInsideFrameClick');
END; { GeneralPopupProjectedLinePenStyleInsideFrameClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreProjectedLinePenStyleClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := DefaultProjectedLinePenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreProjectedLinePenStyleClick');
END; { GeneralPopupRestoreProjectedLinePenStyleClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleSolidClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleSolidClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleSolidClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDotClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDotClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotDotClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleClearClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleClearClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleClearClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleInsideFrameClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleInsideFrameClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedPenStyleClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := DefaultTCOutOfUseAsNoFeedbackReceivedPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedPenStyleClick');
END; { GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedPenStyleClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseSetByUserPenStyleSolidClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleSolidClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleSolidClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseSetByUserPenStyleDashClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleDashClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleDashClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseSetByUserPenStyleDotClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleDotClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseSetByUserPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleDashDotClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleDashDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseSetByUserPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleDashDotDotClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleDashDotDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseSetByUserPenStyleClearClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleClearClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleClearClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCOutOfUseSetByUserPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleInsideFrameClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleInsideFrameClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCOutOfUseSetByUserPenStyleClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := DefaultTCOutOfUseSetByUserPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCOutOfUseSetByUserPenStyleClick');
END; { GeneralPopupRestoreTCOutOfUseSetByUserPenStyleClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleSolidClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleSolidClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleSolidClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleDashClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleDashClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleDashClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleDotClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleDotClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotDotClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleClearClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleClearClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleClearClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleInsideFrameClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleInsideFrameClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCPermanentFeedbackOccupationPenStyleClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := DefaultTCPermanentFeedbackOccupationPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentFeedbackOccupationPenStyleClick');
END; { GeneralPopupRestoreTCPermanentFeedbackOccupationPenStyleClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentSystemOccupationPenStyleSolidClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleSolidClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleSolidClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentSystemOccupationPenStyleDashClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleDashClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleDashClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentSystemOccupationPenStyleDotClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleDotClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentSystemOccupationPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleDashDotClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleDashDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentSystemOccupationPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleDashDotDotClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleDashDotDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentSystemOccupationPenStyleClearClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleClearClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleClearClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentSystemOccupationPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleInsideFrameClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleInsideFrameClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCPermanentSystemOccupationPenStyleClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := DefaultTCPermanentSystemOccupationPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentSystemOccupationPenStyleClick');
END; { GeneralPopupRestoreTCPermanentSystemOccupationPenStyleClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleSolidClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleSolidClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleSolidClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDotClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDotClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotDotClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleClearClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleClearClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleClearClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleInsideFrameClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleInsideFrameClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCLocoOutOfPlaceOccupationPenStyleClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := DefaultTCLocoOutOfPlaceOccupationPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCLocoOutOfPlaceOccupationPenStyleClick');
END; { GeneralPopupRestoreTCLocoOutOfPlaceOccupationPenStyleClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleSolidClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleSolidClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleSolidClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleDashClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleDashClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleDashClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleDotClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleDotClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotDotClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotDotClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleClearClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleClearClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleClearClick }

PROCEDURE TFWPRailWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleInsideFrameClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleInsideFrameClick }

PROCEDURE TFWPRailWindow.GeneralPopupRestoreTCPermanentOccupationSetByUserPenStyleClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := DefaultTCPermanentOccupationSetByUserPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentOccupationSetByUserPenStyleClick');
END; { GeneralPopupRestoreTCPermanentOccupationSetByUserPenStyleClick }

PROCEDURE TFWPRailWindow.GeneralPopupShowMainMenuClick(Sender: TObject);
BEGIN
  IF NOT MenusVisible THEN BEGIN
    ShowMenus;
    GeneralPopupShowMainMenu.Caption := 'Hide Main Menus';
  END ELSE BEGIN
    HideMenus;
    GeneralPopupShowMainMenu.Caption := 'Show Main Menus';
  END;
END; { GeneralPopupShowMainMenuClick }

PROCEDURE TFWPRailWindow.GeneralPopupSetCurrentRailwayDayOfTheWeekClick(Sender: TObject);
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
END; { GeneralPopupSetCurrentRailwayDayOfTheWeekClick }

PROCEDURE TFWPRailWindow.GeneralPopupSetLogFileMaximumWidthClick(Sender: TObject);
VAR
  SaveLogFileMaxWidthInChars : Integer;
  TempInput : String;

BEGIN
  SaveLogFileMaxWidthInChars := LogFileMaxWidthInChars;
  TempInput := InputBox('Maximum Log File Width', 'Maximum Log File Width (In Characters)?', IntToStr(LogFileMaxWidthInChars));
  IF NOT TryStrToInt(TempInput, LogFileMaxWidthInChars) THEN BEGIN
    ShowMessage('Input must be an integer: ' + TempInput + ' is not valid');
    LogFileMaxWidthInChars := SaveLogFileMaxWidthInChars;
  END;
END; { GeneralPopupSetLogFileMaximumWidthClick }

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
    { Work out where panel 3 is } { why? **** }
    WITH FWPRailWindowStatusBar DO BEGIN
      Panel3X := Panels[StatusBarPanel0].Width + Panels[StatusBarPanel1].Width + Panels[StatusBarPanel2].Width;
      IF (StatusBarX > 0) AND (StatusBarX >= Panel3X) THEN BEGIN
        null;
      END;
    END; {WITH}
  END;
END; { FWPRailWindowStatusBarClick }

PROCEDURE TFWPRailWindow.SetCurrentRailwayTime(Sender: TObject);
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  GetTime.ClockWindow.Clock.Time := StrToTime(CurrentRailwayTimeStr);
  GetTime.ClockWindow.Caption := SetCurrentRailwayTimeCaption;
  GetTime.ClockWindow.Visible := True;
  GetTime.ClockWindow.OKButton.SetFocus;
END; { SetCurrentRailwayTime }

PROCEDURE TFWPRailWindow.SetProgramStartTime(Sender: TObject);
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  GetTime.ClockWindow.Clock.Time := ProgramStartTime;
  GetTime.ClockWindow.Caption := SetProgramStartTimeCaption;
  GetTime.ClockWindow.Visible := True;
  GetTime.ClockWindow.OKButton.SetFocus;
END; { SetProgramStartTime }

PROCEDURE TFWPRailWindow.SetDaylightStartTime(Sender: TObject);
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  GetTime.ClockWindow.Clock.Time := StrToTime(DaylightStartTimeStr);
  GetTime.ClockWindow.Caption := SetDaylightStartTimeCaption;
  GetTime.ClockWindow.Visible := True;
  GetTime.ClockWindow.OKButton.SetFocus;
END; { SetDaylightStartTime }

PROCEDURE TFWPRailWindow.SetDaylightEndTime(Sender: TObject);
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  GetTime.ClockWindow.Clock.Time := StrToTime(DaylightEndTimeStr);
  GetTime.ClockWindow.Caption := SetDaylightEndTimeCaption;
  GetTime.ClockWindow.Visible := True;
  GetTime.ClockWindow.OKButton.SetFocus;
END; { SetDaylightEndTime }

PROCEDURE TFWPRailWindow.GeneralPopupRunClockNormallyClick(Sender: TObject);
BEGIN
  GeneralPopupRunClockNormally.Checked := True;
  GeneralPopupRunClockSlower.Checked := False;
  GeneralPopupRunClockFaster.Checked := False;
  GeneralPopupRunClockFastest.Checked := False;
  SetRailwayTimeInterval(Normal);
END; { GeneralPopupRunClockNormallyClick }

PROCEDURE TFWPRailWindow.GeneralPopupRunClockSlowerClick;
BEGIN
  GeneralPopupRunClockNormally.Checked := False;
  GeneralPopupRunClockSlower.Checked := True;
  GeneralPopupRunClockFaster.Checked := False;
  GeneralPopupRunClockFastest.Checked := False;
  SetRailwayTimeInterval(Slower);
END; { GeneralPopupRunClockSlowerExecute }

PROCEDURE TFWPRailWindow.GeneralPopupRunClockFasterClick;
BEGIN
  GeneralPopupRunClockNormally.Checked := False;
  GeneralPopupRunClockSlower.Checked := False;
  GeneralPopupRunClockFaster.Checked := True;
  GeneralPopupRunClockFastest.Checked := False;
  SetRailwayTimeInterval(Faster);
END; { GeneralPopupRunClockFasterClick }

PROCEDURE TFWPRailWindow.GeneralPopupRunClockFastestClick;
BEGIN
  GeneralPopupRunClockNormally.Checked := False;
  GeneralPopupRunClockSlower.Checked := False;
  GeneralPopupRunClockFaster.Checked := False;
  GeneralPopupRunClockFastest.Checked := True;
  SetRailwayTimeInterval(Fastest);
END; { GeneralPopupRunClockFastestClick }

PROCEDURE TFWPRailWindow.StartClock(Sender: TObject);
BEGIN
  GeneralPopupStartClock.Visible := False;
  GeneralPopupStopClock.Visible := True;
  TurnAutoModeOn;
END; { StartClock }

PROCEDURE TFWPRailWindow.StopClock(Sender: TObject);
CONST
  UserInCharge = True;

BEGIN
  GeneralPopupStartClock.Visible := True;
  GeneralPopupStopClock.Visible := False;
  TurnAutoModeOff(UserInCharge);
END; { StopClock }

PROCEDURE TFWPRailWindow.GeneralPopupChangePointClick(Sender: TObject);
BEGIN
  SetAndShowInputDialogueBox(PointDialogueBox);
END; { GeneralPopupChangePoint }

PROCEDURE TFWPRailWindow.GeneralPopupChangeSignalClick(Sender: TObject);
BEGIN
  SetAndShowInputDialogueBox(SignalDialogueBox);
END; { GeneralPopupChangeSignalClick }

PROCEDURE TFWPRailWindow.GeneralPopupListLocomotivesClick(Sender: TObject);
BEGIN
  IF LocoUtilsWindow.Visible THEN BEGIN
    LocoUtilsWindow.Visible := False;
    Log('A "?" key hides List of Locos');
  END ELSE BEGIN
    LocoUtilsWindow.Visible := True;
    Log('A "?" key makes List of Locos visible');
  END;
END; { GeneralPopupListLocomotivesClick }

PROCEDURE TFWPRailWindow.GeneralPopupShowTrackcircuitClick(Sender: TObject);
BEGIN
  SetAndShowInputDialogueBox(TrackCircuitDialogueBox);
END; { GeneralPopupShowTrackcircuitClick }

PROCEDURE TFWPRailWindow.GeneralPopupDebugOptionsClick(Sender: TObject);
BEGIN
  Startup.DebuggingOptionsWindow.Show;
END; { GeneralPopupDebugOptionsClick }

PROCEDURE ResetFWPRailWindowSizeAndPosition;
{ Reset the window's size and position }
BEGIN
  FWPRailWindow.Height := MulDiv(Screen.WorkAreaHeight, 80, 100);
  FWPRailWindow.Width := Screen.WorkAreaWidth;
  FWPRailWindow.Top := 0;
  FWPRailWindow.Left := 0;
  InvalidateScreen(UnitRef, 'ResetFWPRailWindowSizeClick');
END; { ResetFWPRailWindowSizeAndPosition }

PROCEDURE TFWPRailWindow.GeneralPopupResetFWPRailWindowSizeAndPositionClick(Sender: TObject);
BEGIN
  ResetFWPRailWindowSizeAndPosition;
  FWPRailWindow.Height := MulDiv(Screen.WorkAreaHeight, 80, 100);
  FWPRailWindow.Width := Screen.WorkAreaWidth;
  FWPRailWindow.Top := 0;
  FWPRailWindow.Left := 0;
  InvalidateScreen(UnitRef, 'ResetFWPRailWindowSizeClick');
END; { ResetFWPRailWindowSizeClick }

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

//PROCEDURE TFWPRailWindow.CM_EnterMenuLoop(var Msg : TMessage);
//BEGIN
//  TrackCircuitPopupMenuActive := True;
//  Debug('PopupMenu entered');
//END; { CM_EnterMenuLoop }
//
//PROCEDURE TFWPRailWindow.CM_ExitMenuLoop(var Msg : TMessage);
//BEGIN
//  TrackCircuitPopupMenuActive := False;
//  Debug('PopMenu exited');
//END; { CM_ExitMenuLoop }
//
//PROCEDURE TFWPRailWindow.CM_MenuClosed(var Msg : TMessage);
//BEGIN
//  Debug('PopMenu closed');
//END; { CM_MenuClosed }

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

PROCEDURE LoadIcons;
{ The icons are held in the system resource file, itself compiled by using "brcc32 -v rail.rc" from the command prompt. The file "rail.rc" is the resource script file. }
BEGIN
  EditIcon := TIcon.Create;
  OnlineIcon := TIcon.Create;
  OfflineIcon := TIcon.Create;

  EditIcon.Handle := LoadIcon(hInstance, 'EditIcon');
  OnlineIcon.Handle := LoadIcon(hInstance, 'OnlineIcon');
  OfflineIcon.Handle := LoadIcon(hInstance, 'OfflineIcon');
END; { LoadIcons }

PROCEDURE InitialiseRaildrawUnit;
{ Initialises the unit }
BEGIN
  TRY
    WITH FWPRailWindow DO BEGIN
      LoadIcons;

//      { Intercept messages to be able to use the tab key! }
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

      FWPRailWindowInitialised := False;
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
      MainClockMenu.Visible := False;
      MainDisplayMenu.Visible := False;
      MainFileMenu.Visible := False;
      MainHelpMenu.Visible := False;
      MainOperationsMenu.Visible := False;
      MainRunMenu.Visible := False;

      InitialiseScreenDrawingVariables;

      FWPRailWindow.Visible := True;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG FWPRailWindowCreate:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { InitialiseRaildrawUnit }

PROCEDURE DrawMap;
{ Draws the track layout }
CONST
  ForceDraw = True;
  NewSignalData = True;
  ShowNums = True;
  ShowTheatreDestinationChar = True;
  StartOfLine = True;
  Up = True;

VAR
  AdjacentDownTC : Integer;
  AdjacentUpTC : Integer;
  B, I, J : Integer;
  DiagramsMissing : Boolean;
  DiagramsOK : Boolean;
  ShowPointNum : Boolean;
  ErrorMsg : String;
  Line, Line2 : Integer;
  LinesArray : LineArrayType;
  LocoDataTableOK : Boolean;
  P : Integer;
  S : Integer;
  SaveLineOldColour : Integer;
  SaveRecordLineDrawingMode : Boolean;
  ShowArea : Boolean;
  SegmentText : String;
  WindowsTaskbar: HWND;
  TempLocationYArray : IntegerArrayType;
  WorkingTimetableMissing : Boolean;
  WorkingTimetableOK : Boolean;
  // PreviousDebugTime : TDateTime;

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
    // Log('X (1) ' + TimeToHMSZStr(Time));
    // PreviousDebugTime := Time;
    WITH FWPRailWindow DO BEGIN
      WITH RailWindowBitmap.Canvas DO BEGIN
        { Do not record the line drawing detail each time DrawMap is called }
        SaveRecordLineDrawingMode := RecordLineDrawingMode;
//        RecordLineDrawingMode := False;

        IF NOT FWPRailWindowInitialised THEN
          IF Screen.Cursor <> crHourGlass THEN
            ChangeCursor(crHourGlass);

        RailWindowBitmapCanvasPenWidth := Canvas.Pen.Width;

        IF (ScreenMode <> SaveScreenMode) OR NOT FWPRailWindowInitialised THEN BEGIN
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
                { Use for checking track circuits. etc., as displays track circuit number on the status bar }
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
        END;
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

        // Canvas.Brush.Color := clAqua;
        // Canvas.FillRect(Canvas.ClipRect);

        // Log('X (2) ' + TimeToHMSZStr(Time));
        // PreviousDebugTime := Time;

        IF NOT FWPRailWindowInitialised THEN BEGIN
          Randomize;
          ReadInAreasDataFromDatabase; { ************ problem for all file loading if registry entry empty - no prompt to find directory they're in - 21/1/14 }
          ReadInLocationDataFromDatabase;
          SetLength(LocationOccupations, Length(Locations));
          ReadInLocoDataFromDatabase(LocoDataTableOK);
          SetUpLineDrawingVars;
          ReadInLineDataFromDatabase;
          ReadInFeedbackDataFromDatabase;
          IF NOT TrackCircuitsInitialised THEN BEGIN
            { only initialise track circuit once, as doing so a second time removes the data **** }
            TrackCircuitsInitialised := True;
            ReadInTrackCircuitDataFromDatabase;
            CheckLineConnectionsAreOK;
          END;
          ReadInPointDataFromDatabase;
          IF PreviousPointSettingsMode THEN
            LoadPreviousPointSettings;
          ReadInPlatformDataFromDatabase;
          ReadInSignalDataFromDatabase(NOT NewSignalData);
          ReadInRouteingExceptionsFromDatabase;
          IF NOT LocationLinesInitialised THEN BEGIN
            Log('A INITIALISING LOCATION LINES {BLANKLINEBEFORE}');
            InitialiseLocationLines;
            LocationLinesInitialised := True;
          END;
        END;

        { Position the status bar allowing for the window scrolling }
        FWPRailWindowStatusBar.Left := 0;
        FWPRailWindowStatusBar.Top := ClientHeight - FWPRailWindowStatusBar.Height;
        FWPRailWindowStatusBar.Width := ClientWidth;

        IF ResizeMap OR ReinitialiseFWPRailWindowVariables THEN BEGIN
          SetUpLineDrawingVars;
          CalculateLocationPositions;
          CalculateLinePositions;
          CalculateBufferStopPositions;
          CalculatePointPositions;
          CalculatePlatformPositions;
          CalculateAllSignalPositions;

          IF ReinitialiseFWPRailWindowVariables THEN
            ReinitialiseFWPRailWindowVariables := False;
        END;
  //TransparentColorValue := BackgroundColour;
  //TransparentColor := True;
        // Log('X (3a) ' + TimeToHMSZStr(Time));
        // PreviousDebugTime := Time;
        IF NOT FWPRailWindowInitialised AND NOT DiagramsCheckingInProgress AND LocoDataTableOK THEN BEGIN
          { Load feedback data and the diagrams datat and compare the data (these routines are here, as the various windows are created by this stage) }
          DiagramsCheckingInProgress := True;
          // Log('X (3b) ' + TimeToHMSZStr(Time));

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
        END;

        { Draw the positions where lines could be created }
        IF EditMode THEN BEGIN
          CalculateLocationPositions;
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

                IF Line = EditedLine THEN
                  DrawLine(Line, ScreenComponentEditedColour, ActiveTrain)
                ELSE
                  DrawLine(Line, Line_CurrentColour, ActiveTrain);
              END;

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

                IF ShowAdjacentTrackCircuitMode THEN BEGIN
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
                                TextOut(Line_UpX + (Line_DownX - Line_UpX) DIV 2 - TextWidth(SegmentText) DIV 2 - ScrollBarXAdjustment,
                                       (Line_UpY + (Line_DownY - Line_UpY) DIV 2) - TextHeight(SegmentText) DIV 2 - ScrollBarYAdjustment,
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
  //debug('called from drawmap ' + TestcOunTstr);
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
        OR ShowLinesWhereUpXValueSpecified
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

        IF NOT ShowTrackCircuits AND NOT ShowLineDetail AND NOT ShowLineNumbers AND NOT ShowLinesWhereUpXValueSpecified THEN
          DrawAllPoints;

        IF ResizeMap THEN
          ResizeMap := False;

        RecordLineDrawingMode := SaveRecordLineDrawingMode;

        IF NOT FWPRailWindowInitialised THEN BEGIN
          { and finally... }
          FWPRailWindowInitialised := True;
          ChangeCursor(crDefault);
          ProgramStartup := False;
          InvalidateScreen(UnitRef, 'DrawMap 4');
        END;
      END; { WITH Canvas }

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
{ Receives data from the Watchdog program. (This code is here as the Raildraw window is the de facto main window visible to other programs). }
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
