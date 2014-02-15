UNIT RailDraw;

{ Handles graphics display for railway program

  Copyright © F.W. Pritchard 1988-2014. All Rights Reserved.

  v0.1  00/00/88 Unit created
  v0.2  24/04/13 header added; semaphore signal routine rewritten by JDK and FWP
}

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Forms, Dialogs, Menus, Initvars, StdCtrls, ComCtrls, ExtCtrls, Controls, ActnList, DB, ADODB, Buttons,
     System.UITypes;

TYPE
  TMainWindow = CLASS(TForm)
    FlashTimer: TTimer;

    BufferStopPopupBufferStopNum: TMenuItem;
    BufferStopPopupMenu: TPopupMenu;
    ChangePoint: TMenuItem;
    ChangeSignal: TMenuItem;
    CreateOrDeleteMenuItemRuler: TMenuItem;
    CreateOrDeleteItemPopupMenu: TPopupMenu;
    CreateLineMenuItem: TMenuItem;
    CreatePointMenuItem: TMenuItem;
    CreateSignalMenuItem: TMenuItem;
    DeleteLineMenuItem: TMenuItem;
    DeletePointMenuItem: TMenuItem;
    DeleteSignalMenuItem: TMenuItem;
    GeneralPopupBackgroundColour: TMenuItem;
    GeneralPopupBufferStopColours: TMenuItem;
    GeneralPopupChangeBackgroundColour: TMenuItem;
    GeneralPopupChangeColours: TMenuItem;
    GeneralPopupChangeDefaultPointColour: TMenuItem;
    GeneralPopupChangeForegroundColour: TMenuItem;
    GeneralPopupChangePenStyles: TMenuItem;
    GeneralPopupChangePoint: TMenuItem;
    GeneralPopupChangePointLockedColour: TMenuItem;
    GeneralPopupChangePointManualOperationColour: TMenuItem;
    GeneralPopupChangePointOutOfUseColour: TMenuItem;
    GeneralPopupChangePointUndrawColour: TMenuItem;
    GeneralPopupChangeSidingPenStyle: TMenuItem;
    GeneralPopupChangeSignalAspectUnlitColour: TMenuItem;
    GeneralPopupChangeSignalPostRouteSettingColour: TMenuItem;
    GeneralPopupChangeTCFeedbackDataInUseColour: TMenuItem;
    GeneralPopupChangeTCFeedbackDataOutOfUseColour: TMenuItem;
    GeneralPopupChangeTCFeedbackOccupationColour: TMenuItem;
    GeneralPopupChangeTCLocoOutOfPlaceColour: TMenuItem;
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
    GeneralPopupPointLockedColour: TMenuItem;
    GeneralPopupPointOutOfUseColour: TMenuItem;
    GeneralPopupPointUndrawColour: TMenuItem;
    GeneralPopupResetMainWindowSizeAndPosition: TMenuItem;
    GeneralPopupRestoreDefaultBackgroundColour: TMenuItem;
    GeneralPopupRestoreForegroundColour: TMenuItem;
    GeneralPopupRestorePointDefaultColour: TMenuItem;
    GeneralPopupRestorePointLockedColour: TMenuItem;
    GeneralPopupRestorePointOutOfUseColour: TMenuItem;
    GeneralPopupRestorePointUndrawColour: TMenuItem;
    GeneralPopupRestoreSidingPenStyle: TMenuItem;
    GeneralPopupRestoreSignalAspectUnlitColour: TMenuItem;
    GeneralPopupRestoreSignalPostRouteSettingColour: TMenuItem;
    GeneralPopupRestoreTCFeedbackDataInUseColour: TMenuItem;
    GeneralPopupRestoreTCFeedbackDataOutOfUseColour: TMenuItem;
    GeneralPopupRestoreTCFeedbackOccupationColour: TMenuItem;
    GeneralPopupRestoreTCLocoOutOfPlaceColour: TMenuItem;
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
    GeneralPopupTCLocoOutOfPlaceColour: TMenuItem;
    GeneralPopupTCOutOfUseSetByUserColour: TMenuItem;
    GeneralPopupTCPermanentFeedbackOccupationColour: TMenuItem;
    GeneralPopupTCPermanentOccupationSetByUserColour: TMenuItem;
    GeneralPopupTCPermanentOccupationSetByUserPenStyle: TMenuItem;
    GeneralPopupTCPermanentSystemOccupation: TMenuItem;
    GeneralPopupTCSpeedRestrictionColour: TMenuItem;
    GeneralPopupTrainColours: TMenuItem;
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
    MainDisplayMenuShow: TMenuItem;
    MainDisplayMenuShowStatusbar: TMenuItem;
    MainDisplayMenuDiagramsWindow: TMenuItem;
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
    MainTimer: TTimer;
    MainWindowColourDialogue: TColorDialog;
    MainWindowMenu: TMainMenu;
    MainWindowPopupOpenDialogue: TOpenDialog;
    MainWindowStatusBar: TStatusBar;
    PointPopupLockPoint: TMenuItem;
    PointPopupMenu: TPopupMenu;
    PointPopupPointNum: TMenuItem;
    PointPopupRuler1: TMenuItem;
    PointPopupSetPointBackInUse: TMenuItem;
    PointPopupSetPointOutOfUse: TMenuItem;
    PointPopupSetPointToAutomatic: TMenuItem;
    PointPopupSetPointToManual: TMenuItem;
    RestorePointFeedbackDataInUseColour: TMenuItem;
    SetDaylightEnd: TMenuItem;
    SetDayLightStart: TMenuItem;
    SignalPopupEditSignalDetails: TMenuItem;
    SignalPopupMenu: TPopupMenu;
    SignalPopupRuler1: TMenuItem;
    SignalPopupRuler2: TMenuItem;
    SignalPopupSetSignalBackInUse: TMenuItem;
    SignalPopupSetSignalOutOfUse: TMenuItem;
    SignalPopupSignalNum: TMenuItem;
    TCPopupAllocateLocoToTrackCircuit: TMenuItem;
    TCPopupChangeInternalLocoDirectionToDown: TMenuItem;
    TCPopupChangeInternalLocoDirectionToUp: TMenuItem;
    TCPopupClearLocoAllocationFromTrackCircuit: TMenuItem;
    TCPopupClearTrackCircuitSpeedRestriction: TMenuItem;
    TCPopupMenu: TPopupMenu;
    TCPopupRuler2: TMenuItem;
    TCPopupRuler3: TMenuItem;
    TCPopupSetLineOutOfUse: TMenuItem;
    TCPopupSetLocationOutOfUse: TMenuItem;
    TCPopupSetTrackCircuitOutOfUseSetByUser: TMenuItem;
    TCPopupSetTrackCircuitSpeedRestriction: TMenuItem;
    TCPopupSetTrackCircuitToFeedbackOccupation: TMenuItem;
    TCPopupSetTrackCircuitToPermanentOccupation: TMenuItem;
    TCPopupSetTrackCircuitToSystemOccupation: TMenuItem;
    TCPopupSetTrackCircuitToUserDriving: TMenuItem;
    TCPopupSetTrackCircuitUnoccupied: TMenuItem;
    TCPopupTrackCircuitNumber: TMenuItem;
    PointPopupRuler2: TMenuItem;
    PointPopupEditPointDetails: TMenuItem;
    BufferStopPopupRuler: TMenuItem;

    PROCEDURE BufferStopMenuOnPopup(Sender: TObject);
    PROCEDURE CreateLineMenuItemClick(Sender: TObject);
    PROCEDURE CreateOrDeleteItemMenuOnPopup(Sender: TObject);
    PROCEDURE CreateSignalMenuItemClick(Sender: TObject);
    PROCEDURE CreatePointMenuItemClick(Sender: TObject);
    PROCEDURE DeleteSignalMenuItemClick(Sender: TObject);
    PROCEDURE FlashTimerTick(Sender: TObject);
    PROCEDURE GeneralPopupChangeBackgroundColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeBufferStopColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeBufferStopNumberColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeBufferStopRedClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeDefaultPointColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeForegroundColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangeLenzPointNumberColourClick(Sender: TObject);
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
    PROCEDURE GeneralPopupChangePointLockedColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointManualOperationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointOutOfUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointStraightLineColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointsWithoutFeedbackColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointUndrawColourClick(Sender: TObject);
    PROCEDURE GeneralPopupChangePointUpFacingColourClick(Sender: TObject);
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
    PROCEDURE GeneralPopupResetMainWindowSizeAndPositionClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreAllDefaultColoursClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreBufferStopColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreBufferStopNumberColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreBufferStopRedClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreDefaultBackgroundColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreFiddleyardLinePenStyleClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreForegroundColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreLenzPointNumberColourClick(Sender: TObject);
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
    PROCEDURE GeneralPopupRestorePointLockedColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointManualOperationColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointOutOfUseColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointStraightLineColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointsWithoutFeedbackColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointUndrawColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestorePointUpFacingColourClick(Sender: TObject);
    PROCEDURE GeneralPopupRestoreProjectedLinePenStyleClick(Sender: TObject);
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
    PROCEDURE LocoInfoMenuItemClick(Sender: TObject);
    PROCEDURE MainDisplayMenuDebugClick(Sender: TObject);
    PROCEDURE MainDisplayMenuShowClick(Sender: TObject);
    PROCEDURE MainDisplayMenuDiagramsWindowClick(Sender: TObject);
    PROCEDURE MainDisplayMenuZoomClick(Sender: TObject);
    PROCEDURE MainDisplayMenuWorkingTimetableWindowClick(Sender: TObject);
    PROCEDURE MainHelpMenuRailHelpClick(Sender: TObject);
    PROCEDURE MainRunMenuResumeOperationsClick(Sender: TObject);
    PROCEDURE MainTimerTick(Sender: TObject);
    PROCEDURE MainWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE MainWindowCreate(Sender: TObject);
    PROCEDURE MainWindowDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    PROCEDURE MainWindowDragDrop(Sender, Source: TObject; X, Y: Integer);
    PROCEDURE MainWindowExitClick(Sender: TObject);
    PROCEDURE MainWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE MainWindowMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE MainWindowMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE MainWindowMouseUp(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE MainWindowMouseWheel(Sender: TObject; ShiftState: TShiftState; WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
    PROCEDURE MainWindowPaint(Sender: TObject);
    PROCEDURE MainWindowResize(Sender: TObject);
    PROCEDURE MainWindowShortCut(VAR Msg: TWMKey; VAR Handled: Boolean);
    PROCEDURE MainWindowStatusBarDblClick(Sender: TObject);
    PROCEDURE MainWindowStatusBarMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE PointPopupMenuOnPopup(Sender: TObject);
    PROCEDURE PointPopupLockPointClick(Sender: TObject);
    PROCEDURE PointPopupSetPointBackInUseClick(Sender: TObject);
    PROCEDURE PointPopupSetPointOutOfUseClick(Sender: TObject);
    PROCEDURE PointPopupSetPointToAutomaticClick(Sender: TObject);
    PROCEDURE PointPopupSetPointToManualClick(Sender: TObject);
    PROCEDURE SetCurrentRailwayTime(Sender: TObject);
    PROCEDURE SetDaylightEndTime(Sender: TObject);
    PROCEDURE SetDaylightStartTime(Sender: TObject);
    PROCEDURE SetProgramStartTime(Sender: TObject);
    PROCEDURE ShowStatusBarClick(Sender: TObject);
    PROCEDURE SignalPopupMenuOnPopup(Sender: TObject);
    PROCEDURE SignalPopupEditSignalDetailsClick(Sender: TObject);
    PROCEDURE SignalPopupSetSignalBackInUseClick(Sender: TObject);
    PROCEDURE SignalPopupSetSignalOutOfUseClick(Sender: TObject);
    PROCEDURE StartClock(Sender: TObject);
    PROCEDURE StopClock(Sender: TObject);
    PROCEDURE TCPopupAllocateLocoToTrackCircuitClick(Sender: TObject);
    PROCEDURE TCPopupChangeInternalLocoDirectionToDownClick(Sender: TObject);
    PROCEDURE TCPopupChangeInternalLocoDirectionToUpClick(Sender: TObject);
    PROCEDURE TCPopupClearLocoAllocationFromTrackCircuitClick(Sender: TObject);
    PROCEDURE TCPopupClearTrackCircuitSpeedRestrictionClick(Sender: TObject);
    PROCEDURE TCPopupMenuOnPopup(Sender: TObject);
    PROCEDURE TCPopupSetLineOutOfUseClick(Sender: TObject);
    PROCEDURE TCPopupSetLocationOutOfUseClick(Sender: TObject);
    PROCEDURE TCPopupSetTrackCircuitOutOfUseSetByUserClick(Sender: TObject);
    PROCEDURE TCPopupSetTrackCircuitSpeedRestrictionClick(Sender: TObject);
    PROCEDURE TCPopupSetTrackCircuitToFeedbackOccupationClick(Sender: TObject);
    PROCEDURE TCPopupSetTrackCircuitToPermanentOccupationClick(Sender: TObject);
    PROCEDURE TCPopupSetTrackCircuitToSystemOccupationClick(Sender: TObject);
    PROCEDURE TCPopupSetTrackCircuitToUserDrivingClick(Sender: TObject);
    PROCEDURE TCPopupSetTrackCircuitUnoccupiedClick(Sender: TObject);
    PROCEDURE TCPopupShowLocosLastErrorMessageClick(Sender: TObject);
    PROCEDURE TCPopupTrackCircuitNumberClick(Sender: TObject);
    procedure DeletePointMenuItemClick(Sender: TObject);
    procedure DeleteLineMenuItemClick(Sender: TObject);
    procedure PointPopupEditPointDetailsClick(Sender: TObject);

  PRIVATE
    { Private declarations }
    PROCEDURE ApplicationMessage(VAR Msg: TMsg; VAR Handled: Boolean);
    { Added to handle Windows messages }
//    PROCEDURE CM_MenuClosed(VAR msg: TMessage); MESSAGE CM_MENU_CLOSED;
//    PROCEDURE CM_EnterMenuLoop(VAR msg: TMessage); MESSAGE CM_ENTER_MENU_LOOP;
//    PROCEDURE CM_ExitMenuLoop(VAR msg: TMessage); MESSAGE CM_EXIT_MENU_LOOP;
  PUBLIC
    { Public declarations }
    PROCEDURE WMHScroll(VAR ScrollData: TMessage); MESSAGE wm_HScroll;
   { Added to allow interception of scroll bar events }
    PROCEDURE WMVScroll(VAR ScrollData: TMessage); MESSAGE wm_VScroll;
   { Added to allow interception of scroll bar events }

//    PROCEDURE FWPExceptionHandler(Sender: TObject; E: Exception);
  END;

procedure CanvasTextOutAngle(x,y: Integer; d: Word; s: string);

PROCEDURE ChangeCursor(NewCursor : TCursor);
{ Change the shape of the cursor (from the Delphi Help system) }

PROCEDURE CheckLineConnectionsAreOK;
{ To check each line has the requisite connection data, as locking depends on this }

PROCEDURE DrawAllPoints;
{ Draw all the points }

PROCEDURE DrawAllSignals(ShowSignalAndBufferStopNums, ShowTheatreDestinations : Boolean);
{ Draw all the signals }

PROCEDURE DrawBufferStop(BufferStopNum : Integer; Colour : TColour);
{ Draw a buffer stop }

PROCEDURE DrawBufferStopData(BufferStopNum : Integer; BufferStopText : String; Colour : TColor);
{ Put the bufferstop name or other supplied data on the diagram }

PROCEDURE DrawConnectionCh(ConnectionCh : String; ConnectionChRect : TRect; Bold : Boolean);
{ Draw character at line starts/ends to indicate where lines are going when they disappear off the screen }

PROCEDURE DrawFailure(Device : Integer; ActionCh : String);
{ Describes the offending items in the status bar }

PROCEDURE DrawLine{1}(L : Integer; NewLineColour : Integer; ActiveTrain : Boolean); Overload;
{ Draw an individual line, with headcode if required, and store the line colour }

PROCEDURE DrawLine{2}(L : Integer; NewLineColour : Integer; ActiveTrain : Boolean; TempLineText : String); Overload;
{ Draw an individual line, with headcode if required, and store the line colour }

PROCEDURE DrawMap;
{ Draws the track layout }

PROCEDURE DrawPlatforms;
{ Draw the platforms }

PROCEDURE DrawPoint(P : Integer; Colour : TColour);
{ Draw a point }

PROCEDURE DrawPointNum(P : Integer; Colour : TColour);
{ Put the number of the point on the diagram }

PROCEDURE DrawRectangularOutline(NewRect : TRect; Colour : TColour; UndrawRequired, UndrawToBeAutomatic : Boolean);
{ We need this as the default Delphi Rectangle is filled in }

PROCEDURE DrawSignal(S : Integer);
{ Draw a signal at the current position. We need to know if it is for up or down traffic, a home or distant or CallingOn, and what aspect it is. Location.X is the position
  of the main aspect
}
PROCEDURE DrawSignalData(S : Integer; Str : String; Colour : Integer);
{ Put the signal name or other supplied data on the diagram }

PROCEDURE DrawSignalPost(S : Integer);
{ Draws the signal post }

PROCEDURE DrawSpeedRestrictions;
{ Draw speed restrictions next to lines - but only draw one sign per track circuit }

PROCEDURE DrawTrackCircuit{1}(TC : Integer; TCColour : TColour); Overload;
{ Draws a given trackcircuit }

PROCEDURE DrawTrackCircuit{2}(TC : Integer; TCColour : TColour; TempLineText : String); Overload;
{ Draws a given trackcircuit - this version is used by Replay to add train descriptions }

PROCEDURE DrawTrackCircuitsWithAdjoiningTrackCircuits(TC : Integer; TCColour1, TCColour2 : TColour);
{ Draw a trackcircuit and show which trackcircuits adjoin it }

PROCEDURE DrawTRSPlunger(Location : Integer; Pressed : Boolean);
{ Indicate on a platform that a train-ready-to-start plunger has been pressed }

PROCEDURE FindAdjoiningTrackCircuits(TC : Integer; OUT AdjoiningUpTC, AdjoiningDownTC : Integer);
{ Work out which are the adjacent trackcircuits. Does not trace along all lines, just the way points are set. }

FUNCTION GetBufferStopDirection(BufferStopNum : Integer) : DirectionType;
{ Return a buffer stop's direction }

PROCEDURE HideStatusBarAndUpDownIndications;
{ Before a zoomed screen move, hide the status bar and the "up" and "down" markers }

PROCEDURE InitialiseLocationLines;
{ Now we have set up the lines and signals, we can work out how they relate to locations. Some long locations (UFY for instance) will have multiple lines attached *** }

PROCEDURE InvalidateScreen(UnitRefParam, CallingStr : String);
{ Draw the screen by invalidating it }

PROCEDURE RestoreAllSignalsToPreviousState;
{ Sets all off signals to how they were before short circuit }

PROCEDURE RestoreCursor;
{ Restores the shape of the cursor (from the Delphi Help system) }

PROCEDURE SaveSignalsCurrentState;
{ Save all the previous state of all signals }

PROCEDURE SetAllSignalsToDanger;
{ Sets all off signals to on }

PROCEDURE SetCaption(Window : TForm; Caption : String);
{ Sets a window caption }

PROCEDURE SetSignal(LocoChip, S : Integer; NewAspect : AspectType; NoLog : Boolean);
{ Set the state of a particular signal }

PROCEDURE SetTrackCircuitState{1}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the trackcircuit is occupied, and mark it with a loco chip number }

PROCEDURE SetTrackCircuitState{2}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the trackcircuit is occupied, mark it with a loco chip number, and give an explanation }

PROCEDURE SetTrackCircuitState{3}(TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the trackcircuit is occupied }

PROCEDURE SetTrackCircuitState{4}(TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the trackcircuit is occupied and give an explanation. Also see whether we want it recorded in the Location occupation array - this is to avoid
  duplicate recordings at startup.
}
PROCEDURE ShowStatusBarAndUpDownIndications;
{ After a zoomed screen move, restore the status bar and the "up" and "down" markers }

PROCEDURE TurnAllSignalsOff;
{ Turn off the LEDs in the signals }

PROCEDURE TurnAutoModeOff(User : Boolean);
{ Turns auto mode off }

PROCEDURE TurnAutoModeOn;
{ Turns auto mode on }

PROCEDURE WriteToStatusBarPanel(PanelNum : Integer; Str : String);
{ Write the text in the chosen panel }

CONST
  crCrossHair = 5;
  crCrossHairForUpSignal = 6;
  crCrossHairForDownSignal = 7;
  crPointLever = 8;
  crArrowRed = 9;

VAR
  ApplicationMessageShiftState : TShiftState = [];
  BufferStopPopupNum : Integer;
  DiagramsCheckingInProgress : Boolean = False;
  LastPointResetTime : TDateTime = 0;
  LinePopupNum : Integer;
  MainWindow : TMainWindow;
  PointPopupNum : Integer;
  RestartProgram : Boolean = False;
  SaveCursor : TCursor = crDefault;
  SaveMainWindowStatusBarState : StatusBarStateType = Visible;
  SavePanel0Str : String = '';
  SavePanel1Str : String = '';
  SavePanel2Str : String = '';
  SavePanel3Str : String = '';
  ScrollBarXAdjustment : Integer = 0;
  ScrollBarYAdjustment : Integer = 0;
  SignalPopupNum : Integer;
  TimerT : Train = NIL;
  TrackCircuitPopupLine : Integer;
  WindowsTaskbarDisabled : Boolean = False;
  ZoomScaleFactor : Integer = 1000;
//firstpass : boolean = true;
Region : HRGN;
testregion : boolean = false;

IMPLEMENTATION

{$R *.dfm}

USES MiscUtils, Startup, Lenz, Input, Locks, Cuneo, Movement, GetTime, CreateRoute, Diagrams, RDC, Types, Feedback, Route, LocoUtils, DateUtils, IniFiles, LocoDialogue,
     StrUtils, Help, Math {sic}, LocationData, FWPShowMessageUnit, Replay, TestUnit, WorkingTimetable, Options, Registry, Edit;

CONST
  UnitRef = 'RailDraw';
  UndrawRequired = True;
  UndrawToBeAutomatic = True;

VAR
  LocationLinesInitialised : Boolean = False;
  NumbersArrayCounter : Integer = -1;
  SaveScreenMode : ScreenModeType = DefaultWindowedScreenMode;
  SaveSystemStatusEmergencyOff : Boolean;
  SaveUndrawRectColour : TColour;
  SaveZoomScaleFactor : Integer = 1000;
  StatusBarX : Integer = 0;
  StatusBarY : Integer = 0;
  TimeRectangleDrawn : Cardinal;
  TrackCircuitPopupMenuActive : Boolean = False;
  UndrawRect : TRect;
  UpDownMarkersVisible : Boolean = True;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE DrawRedLampAndVerticalLine(X, Y1, Y2 : Integer; Colour : TCOlour);
{ Draw a red lamp and vertical line where there is a buffer stop or line obstruction }
BEGIN
  WITH MainWindow.Canvas DO BEGIN
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
  WITH MainWindow.Canvas DO BEGIN
    WITH BufferStops[BufferStopNum] DO BEGIN
      { record the current colour }
      BufferStop_CurrentColour := Colour;
      IF RecordLineDrawingMode THEN
        Log('X BS=' + IntToStr(BufferStopNum) + ' ' + ColourToStr(Colour));

      { Draw the line and red lamp }
      DrawRedLampAndVerticalLine(BufferStop_X, BufferStop_Y1, BufferStop_Y2, Colour);

      IF ShowMouseRectangles THEN
        DrawRectangularOutline(BufferStop_MouseRect, clFWPOrange, NOT UndrawRequired,NOT UndrawToBeAutomatic);
    END; {WITH}
  END; {WITH}
END; { DrawBufferStop }

PROCEDURE DrawBufferStopData(BufferStopNum : Integer; BufferStopText : String; Colour : TColor);
{ Put the bufferstop name or other supplied data on the diagram }
BEGIN
  InitialiseScreenDrawingVariables;
  WITH MainWindow.Canvas DO BEGIN
    Font.Style := [fsBold];
    Font.Color := Colour;
    Brush.Color := BackgroundColour;
    Font.Height := -MulDiv(MainWindow.ClientHeight, LineFontHeight, ZoomScaleFactor);

    WITH BufferStops[BufferStopNum] DO BEGIN
      IF BufferStop_Direction = Down THEN
        TextOut(BufferStop_X - TextWidth(BufferStopText) - MulDiv(MainWindow.ClientWidth, 5, ZoomScaleFactor) - ScrollBarXAdjustment,
                ((BufferStop_Y2 - BufferStop_Y1) DIV 2) + BufferStop_Y1 - (TextHeight(BufferStopText) DIV 2) - ScrollBarYAdjustment,
                BufferStopText)
      ELSE
        TextOut(BufferStop_X + MulDiv(MainWindow.ClientWidth, 5, ZoomScaleFactor) - ScrollBarXAdjustment,
                ((BufferStop_Y2 - BufferStop_Y1) DIV 2) + BufferStop_Y1 - (TextHeight(BufferStopText) DIV 2) - ScrollBarYAdjustment,
                BufferStopText);
    END; {WITH}
  END; {WITH}
END; { DrawBufferStopData }

PROCEDURE SetCaption(Window : TForm; Caption : String);
{ Sets a window caption }
BEGIN
  IF Window <> NIL THEN BEGIN
    IF Window <> MainWindow THEN
      Window.Caption := Caption
    ELSE BEGIN
      IF NOT TestingMode THEN
        MainWindow.Caption := 'FWP''s Railway Program ' + Caption
      ELSE
        MainWindow.Caption := 'FWP''s Railway Program (Version ' + GetVersionInfoAsString + ' Build ' + GetBuildInfoAsString + ') ' + Caption;
    END;
  END;
END; { SetCaption }

PROCEDURE ShowLinePos;
{ Showing how line segments are sub-divided - for development }
CONST
  ActiveTrain = True;

VAR
  FeedbackData : FeedbackRec;
  FeedbackNum : Integer;
  FeedbackUnitFound : Boolean;
  L : Integer;
  LineNameWidth : Word;
  SegmentText : String;
  TrackCircuitNumbered : Boolean;
  X, Y : Integer;

  PROCEDURE ShowTrackCircuitData;
  { Display data relating to trackcircuits }
  VAR
    ColourNum : Integer;
    FeedbackType : TypeOfFeedBackType;
    I, J, K : Integer;
    L : Integer;
    LineDrawn : Boolean;
    P : Integer;
    Pos : Word;
    TC : Integer;
    TCAboveFeedbackUnit : Integer;
    TempNum : Integer;

  BEGIN
    WITH MainWindow.Canvas DO BEGIN
      { First clear existing line detail, as it may obscure the data we're writing out }
      ShowLineOccupationDetail := False;
      FOR L := 0 TO High(Lines) DO
        DrawLine(L, Lines[L].Line_CurrentColour, False);

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
                    IF (Point_FeedbackUnit = I)
                    AND (Point_FeedbackInput = J)
                    THEN BEGIN
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
                IF (FeedbackNum >= 0)
                AND (FeedbackNum <= High(TrackCircuits))
                THEN BEGIN
                  SegmentText := IntToStr(I) + IntToStr(J);
                  IF Length(TrackCircuits[FeedbackNum].TC_LineArray) > 0 THEN BEGIN
                    L := TrackCircuits[FeedbackNum].TC_LineArray[0];
                    WITH Lines[L] DO
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
          { Write out the trackcircuit number once only }
          L := 0;
          TrackCircuitNumbered := False;
          WHILE L <= High(Lines) DO BEGIN
            IF Lines[L].Line_TC = TC THEN BEGIN
              { Draw vertical lines as separators }
              IF (Lines[L].Line_NextUpLine <> UnknownLine)
              AND (Lines[Lines[L].Line_NextUpLine].Line_TC <> TC)
              THEN BEGIN
                Pen.Color := clWhite;
                Pen.Style := psSolid;
                FOR Pos := 0 TO 10 DO BEGIN
                  WITH Lines[L] DO BEGIN
                    X := Line_UpX + MulDiv(Line_DownX - Line_UpX, Pos, 10);
                    Y := Line_UpY + MulDiv(Line_DownY - Line_UpY, Pos, 10);
                    IF Pos = 0 THEN BEGIN
                      MoveTo(X - ScrollBarXAdjustment, Y - 6 - ScrollBarYAdjustment);
                      LineTo(X - ScrollBarXAdjustment, Y + 6 - ScrollBarYAdjustment);
                    END;
                  END; {WITH}
                END; {FOR}
              END;

              IF NOT ShowTrackCircuits THEN
                { want TCs coloured all the same }
                DrawLine(L, TCUnoccupiedColour, NOT ActiveTrain)
              ELSE BEGIN
                { colour TCs differently to distinguish them }
                LineDrawn := False;
                IF Lines[L].Line_NextUpLine <> UnknownLine THEN BEGIN
                  IF Lines[Lines[L].Line_NextUpLine].Line_TC <> TC THEN BEGIN
                    IF Lines[Lines[L].Line_NextUpLine].Line_CurrentColour <> clLime THEN BEGIN
                      DrawLine(L, clLime, NOT ActiveTrain);
                      LineDrawn := True;
                      Font.Color := clLime;
                    END ELSE
                      IF Lines[Lines[L].Line_NextUpLine].Line_CurrentColour <> clRed THEN BEGIN
                        DrawLine(L, clRed, NOT ActiveTrain);
                        LineDrawn := True;
                        Font.Color := clRed;
                      END ELSE BEGIN
                        DrawLine(L, clYellow, NOT ActiveTrain);
                        LineDrawn := True;
                        Font.Color := clYellow;
                      END;
                  END;
                END;

                IF Lines[L].Line_NextDownLine <> UnknownLine THEN BEGIN
                  IF Lines[Lines[L].Line_NextDownLine].Line_TC <> TC THEN BEGIN
                    IF Lines[Lines[L].Line_NextDownLine].Line_CurrentColour <> clLime THEN BEGIN
                      DrawLine(L, clLime, NOT ActiveTrain);
                      LineDrawn := True;
                      Font.Color := clLime;
                    END ELSE
                      IF Lines[Lines[L].Line_NextDownLine].Line_CurrentColour <> clRed THEN BEGIN
                        DrawLine(L, clRed, NOT ActiveTrain);
                        LineDrawn := True;
                        Font.Color := clRed;
                      END ELSE BEGIN
                        DrawLine(L, clYellow, NOT ActiveTrain);
                        LineDrawn := True;
                        Font.Color := clYellow;
                      END;
                  END;
                END;

                IF NOT LineDrawn THEN BEGIN
                  DrawLine(L, clYellow, NOT ActiveTrain);
                  Font.Color := clYellow;
                END;
              END;

              IF NOT TrackCircuitNumbered OR (ScreenMode = FullScreenMode) OR (ScreenMode = FullScreenWithStatusBarMode) THEN BEGIN
                WITH Lines[L] DO BEGIN
                  Font.Style := [fsBold];
                  IF ShowTrackCircuits THEN
                    SegmentText := IntToStr(TC);

                  { note: the following options are mutually exclusive, so it doesn't need to be IF THEN ELSE }
                  IF ShowTrackCircuitLengths THEN BEGIN
                    IF Line_TC <> UnknownTC THEN BEGIN
                      { how long (in inches) each trackcircuit is }
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
                    IF Line_TC <> UnknownTC THEN BEGIN
                      SegmentText := '';
                      FeedbackUnitFound := False;
                      I := FirstFeedbackUnit;
                      WHILE (I <= LastFeedbackUnit)
                      AND NOT FeedbackUnitFound
                      DO BEGIN
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
            Inc(L);
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
    L : Integer;
    Pos : Integer;
    SaveLineFontName : String;
    TempLocationArray : IntegerArrayType;

  BEGIN
    WITH MainWindow.Canvas DO BEGIN
      { First clear existing line detail, as it may obscure the data we're writing out }
      ShowLineOccupationDetail := False;
      SetLength(TempLocationArray, 0);

      FOR L := 0 TO High(Lines) DO
        DrawLine(L, Lines[L].Line_CurrentColour, False);

      FOR L := 0 TO High(Lines) DO BEGIN
        SegmentText := '';
        WITH Lines[L] DO BEGIN
          { how which trackcircuits have routes set over them }
          IF ShowTrackCircuitsRoutedOver THEN BEGIN
            IF Line_TC <> UnknownTC THEN BEGIN
              Font.Color := clAqua;
              IF TrackCircuits[Line_TC].TC_LockedForRoute <> UnknownRoute THEN
                SegmentText := IntToStr(TrackCircuits[Line_TC].TC_LockedForRoute);
            END;
          END;

          IF ShowLineDetail THEN BEGIN
            { show the internal name for each track segment }
            Font.Color := GetIntegerColour(Line_TypeOfLine);
            LineNameWidth := TextWidth(LineToStr(L));
            IF Line_DownX > Line_UpX THEN BEGIN
              IF (Line_DownX - Line_UpX) > LineNameWidth THEN
                SegmentText := LineToStr(L)
              ELSE
                SegmentText := Copy(LineToStr(L), Length(LineToStr(L)), 1);
            END ELSE BEGIN
              IF (Line_UpX - Line_DownX) > LineNameWidth THEN
                SegmentText := LineToStr(L)
              ELSE
                SegmentText := Copy(LineToStr(L), Length(LineToStr(L)), 1);
            END;
          END;

          IF ShowLineNumbers THEN BEGIN
            { show the internal numbers for each track segment }
            Font.Color := GetIntegerColour(Line_TypeOfLine);
            LineNameWidth := TextWidth(IntToStr(L));
            IF Line_DownX > Line_UpX THEN BEGIN
              IF (Line_DownX - Line_UpX) > LineNameWidth THEN
                SegmentText := IntToStr(L)
              ELSE
                SegmentText := Copy(IntToStr(L), Length(IntToStr(L)), 1);
            END ELSE BEGIN
              IF (Line_UpX - Line_DownX) > LineNameWidth THEN
                SegmentText := LineToStr(L)
              ELSE
                SegmentText := Copy(IntToStr(L), Length(IntToStr(L)), 1);
            END;
          END;

          IF ShowLinesWhereUpXValueSpecified THEN BEGIN
            Font.Color := GetIntegerColour(Line_TypeOfLine);
            LineNameWidth := TextWidth(LineToStr(L));
            IF Line_UpXValueSpecified THEN
              SegmentText := LineToStr(L)
            ELSE
              SegmentText := '';
          END;

          IF ShowLineDirectionDetail THEN BEGIN
            { Indicate lines that are not designated as through lines }
            IF Lines[L].Line_Location <> UnknownLocation THEN BEGIN
              CASE Locations[Lines[L].Line_Location].Location_ThroughLocationState OF
                NonThroughLocation:
                  BEGIN
                    Font.Color := clRed;
                    SegmentText := 'X';
                  END;
                ThroughLocation:
                  BEGIN
                    { if the line direction is one way, which way it is }
                    Font.Color := clAqua;
                    IF Lines[L].Line_Direction = Up THEN BEGIN
                      IF Line_UpY = Line_DownY THEN
                        SegmentText := '¨' { left arrow }
                      ELSE
                        IF Line_UpY > Line_DownY THEN
                          SegmentText := 'Ø' { down arrow }
                        ELSE
                          SegmentText := '≠'; { up arrow }
                    END ELSE
                      IF Lines[L].Line_Direction = Down THEN BEGIN
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
            IF Lines[L].Line_Location <> UnknownLocation THEN BEGIN
              IF NOT IsElementInLocationArray(TempLocationArray, Lines[L].Line_Location, ElementPos) THEN BEGIN
                { only display the length once for each location }
                AppendToLocationArray(TempLocationArray, Lines[L].Line_Location);
                Font.Color := clAqua;
                SegmentText := LocationToStr(Lines[L].Line_Location, ShortStringType) + ' ' + FloatToStr(Locations[Lines[L].Line_Location].Location_LengthInInches);
              END;
            END;
          END;

          IF ShowLineGradients THEN BEGIN
            { Indicate lines which are marked as being on a gradient }
            Font.Color := clLime;
            IF Lines[L].Line_Gradient = RisingIfDown THEN BEGIN
              IF Line_Direction = Down THEN
                SegmentText := '≠' { up arrow }
              ELSE
                IF Line_Direction = Up THEN
                  SegmentText := 'Ø' { down arrow }
                ELSE
                  IF Line_Direction = Bidirectional THEN
                    SegmentText := '≠Ø' { Up and down arrows }
            END ELSE BEGIN
              IF Lines[L].Line_Gradient = RisingIfUp THEN BEGIN
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
            IF Lines[L].Line_Location <> UnknownLocation THEN
              SegmentText := LocationToStr(Line_Location, ShortStringType)
            ELSE
              SegmentText := '?';
          END;

          IF ShowAreas THEN BEGIN
            IF Lines[L].Line_Location <> UnknownLocation THEN BEGIN
              IF Locations[Line_Location].Location_Area <> UnknownArea THEN BEGIN
                IF (Areas[Locations[Line_Location].Location_Area].Area_IsHoldingArea)
                AND (Areas[Locations[Line_Location].Location_Area].Area_IsReversingArea)
                THEN
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
            IF Zooming THEN
              Font.Size := 14;

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

    WITH MainWindow.Canvas DO BEGIN
      Font.Color := clYellow;
      Font.Style := [fsBold];
      Font.Height := -MulDiv(MainWindow.ClientHeight, LineFontHeight, ZoomScalefactor);

      IF ShowTrackCircuits
      OR ShowTrackCircuitLengths
      OR ShowTrackCircuitFeedbackDataInUse
      OR ShowTrackCircuitFeedbackDataInSeparateColours
      OR ShowOneFeedbackUnitOnly
      THEN
        ShowTrackCircuitData;

      IF ShowLinesWithoutTrackCircuits THEN BEGIN
        L := 0;
        WHILE L <= High(Lines) DO BEGIN
          IF Lines[L].Line_TC = UnknownTC THEN BEGIN
            Font.Color := LinesWithoutTrackCircuitsColour;
            SegmentText := 'X';
            WITH Lines[L] DO BEGIN
              TextOut(Line_UpX + (Line_DownX - Line_UpX) DIV 2 - TextWidth(SegmentText) DIV 2 - ScrollBarXAdjustment,
                     (Line_UpY + (Line_DownY - Line_UpY) DIV 2) - TextHeight(SegmentText) DIV 2 - ScrollBarYAdjustment,
                      SegmentText);
            END; {WITH}
          END;
          Inc(L);
        END; {WHILE}
      END;

      { The following options cycle through the various lines }
      IF ShowAreas
      OR ShowLineDetail
      OR ShowLineNumbers
      OR ShowLinesWhereUpXValueSpecified
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
      Log('EG ShowLinePos:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ShowLinePos }

PROCEDURE DrawRectangularOutline(NewRect : TRect; Colour : TColour; UndrawRequired, UndrawToBeAutomatic : Boolean);
{ We need this as the default Delphi Rectangle is filled in }
BEGIN
  TRY
    InitialiseScreenDrawingVariables;

    { If the timer is running on the automatic undrawing of a previous draw, undraw it now before we start the timer on another drawing }
    IF UndrawToBeAutomatic
    AND (TimeRectangleDrawn <> 0)
    THEN BEGIN
      TimeRectangleDrawn := 0;
      WITH MainWindow.Canvas DO BEGIN
        Pen.Color := SaveUndrawRectColour;
        Brush.Color := BackgroundColour;
        WITH UndrawRect DO
          Polyline([Point(Left - ScrollBarXAdjustment, Top - ScrollBarYAdjustment),
                    Point(Left - ScrollBarXAdjustment, Bottom - ScrollBarYAdjustment),
                    Point(Right - ScrollBarXAdjustment, Bottom - ScrollBarYAdjustment),
                    Point(Right - ScrollBarXAdjustment, Top - ScrollBarYAdjustment),
                    Point(Left - ScrollBarXAdjustment, Top - ScrollBarYAdjustment)]);
      END; {WITH}
    END;

    { Now draw what we've been asked to do }
    WITH MainWindow.Canvas DO BEGIN
      IF UndrawRequired THEN
        Pen.Mode := pmNotXor;
      Pen.Color := Colour;
      Brush.Color := BackgroundColour;
      WITH NewRect DO
        Polyline([Point(Left - ScrollBarXAdjustment, Top - ScrollBarYAdjustment),
                  Point(Left - ScrollBarXAdjustment, Bottom - ScrollBarYAdjustment),
                  Point(Right - ScrollBarXAdjustment, Bottom - ScrollBarYAdjustment),
                  Point(Right - ScrollBarXAdjustment, Top - ScrollBarYAdjustment),
                  Point(Left - ScrollBarXAdjustment, Top - ScrollBarYAdjustment)]);
    END; {WITH}

    IF UndrawRequired
    AND UndrawToBeAutomatic
    THEN BEGIN
      { If automatic undraw required, do it after a set time }
      TimeRectangleDrawn := GetTickCount;
      UndrawRect := NewRect;
      SaveUndrawRectColour := Colour;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawRectangularOutline:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawRectangularOutline }

PROCEDURE WriteToStatusBarPanel(PanelNum : Integer; Str : String);
{ Write the text in the chosen panel if the text has changed. Note: Debug can only be used in this routine with a second Boolean parameter turning DoNoWriteToStatusPanel
  on, as otherwise both routines call each other and the stack overflows.
}
CONST
  DoNotWriteToStatusBarPanel = True;

BEGIN
  TRY
    CASE PanelNum OF
      0:
        IF SavePanel0Str <> Str THEN BEGIN
          MainWindow.MainWindowStatusBar.Panels[StatusBarPanel0].Text := Str;
          SavePanel0Str := Str;
        END;
      1:
        IF SavePanel1Str <> Str THEN BEGIN
          MainWindow.MainWindowStatusBar.Panels[StatusBarPanel1].Text := Str;
          SavePanel1Str := Str;
        END;
      2:
        IF SavePanel2Str <> Str THEN BEGIN
          MainWindow.MainWindowStatusBar.Panels[StatusBarPanel2].Text := Str;
          SavePanel2Str := Str;
        END;
    END; {CASE}
  EXCEPT
    ON E : Exception DO
      Log('EG WriteToStatusBarPanel:' + E.ClassName +' error raised, with message: '+ E.Message);
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
            DrawRectangularOutline(Point_MouseRect, clBlack, UndrawRequired, UndrawToBeAutomatic);
      END; {CASE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawFailure:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawFailure }

PROCEDURE StartSystemTimer;
{ Starts the system timer only }
BEGIN
  MainWindow.MainTimer.Enabled := True;
END; { StartSystemTimer }

{$O-}
PROCEDURE TurnAutoModeOff(User : Boolean);
{ Turns auto mode off }
CONST
  TrainListOnly = True;

VAR
  DebugStr : String;
  R : Integer;

BEGIN
  TRY
    InAutoMode := False;

    { Stop the timers }
    IF ClockWindow <> NIL THEN
      ClockWindow.GetTimeTimer.Enabled := False;

    { If we press the Stop key, stop current routeing too }
    R := 0;
    IF Routes_RouteCounter > -1 THEN BEGIN
      WHILE R <= Routes_RouteCounter DO BEGIN
        IF Routes_RouteSettingsInProgress[R] THEN
          RouteingSuspendedWhenStopPressed := True;
        Inc(R);
      END; {WHILE}
    END;

    SaveSignalsCurrentState;

    IF NOT SystemOnline THEN
      Log('GG Auto mode turned off - clock stopped')
    ELSE BEGIN
      StopLocos('auto mode turn off');
      IF User THEN
        DebugStr := 'Auto mode turned off by user'
      ELSE
        DebugStr := 'Auto mode turned off by system';
      DebugStr := DebugStr + ' - clock stopped, and all locos stopped';
      Log('GG ' + DebugStr);
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG TurnAutoModeOff:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TurnAutoModeOff }

PROCEDURE TurnAutoModeOn;
{ Turns auto mode on }
BEGIN
  TRY
    AutoModeInitiated := True;

    { Allow routeing to continue }
    RouteingSuspendedWhenStopPressed := False;
    InAutoMode := True;

    IF NOT Restart
    AND LogsCurrentlyKept
    THEN BEGIN
      { First a replay command - this doesn't use Log, as we don;t want the time etc. }
      WriteLn(LargeLogFile, '{Replay Write}');

      Log('GG AutoOn Button pressed - clock started')
    END ELSE BEGIN
      IF SystemOnline THEN BEGIN
        Log('GG AutoOn Button pressed - clock restarted')
      END ELSE BEGIN
        StartLocos(Restart);
        Log('GG AutoOn Button pressed - clock restarted, and all locos restarted');
      END;

      RestoreAllSignalsToPreviousState;
      Log('G All signals now set to previous state');
    END;
    LocosStopped := False;
    IF Restart THEN
      IF SystemOnline THEN
        StartLocos(Restart);
    Restart := True;
    RouteClearingOnlyMode := False;
    { and restart the auto clock }
    ClockWindow.GetTimeTimer.Enabled := True;
    { and the system timer }
    StartSystemTimer;
  EXCEPT
    ON E : Exception DO
      Log('EG TurnAutoModeOn:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TurnAutoModeOn }

PROCEDURE SaveSignalsCurrentState;
{ Save all the previous state of all signals }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF NOT Signal_OutOfUse THEN BEGIN
          Signal_PreviousAspect := Signals[S].Signal_Aspect;
          Signal_PreviousIndicatorState := Signals[S].Signal_IndicatorState;
          Signal_PreviousTheatreIndicatorString := Signal_TheatreIndicatorString;
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG SaveSignalsCurrentState:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SaveSignalsCurrentState }

PROCEDURE TurnAllSignalsOff;
{ Turn off the LEDs in the signals }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      SetSignal(NoLocoChip, S, NoAspect, NOT NoLog);
      IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
        SetIndicator(NoLocoChip, S, NoIndicatorLit, '', NoRoute, NOT ByUser);
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG TurnAllSignalsOff:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TurnAllSignalsOff }

PROCEDURE RestoreAllSignalsToPreviousState;
{ Sets all off signals to how they were before short circuit }
VAR
  S : Integer;

BEGIN
  TRY
    Log('S Restoring all signals to their previous aspects');
    FOR S := 0 TO High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF NOT Signal_OutOfUse THEN BEGIN
          { have to set state to NoAspect, or SetSignal won't redraw the SignalAspect/indicator }
          Signal_Aspect := NoAspect;
          Signal_IndicatorState := NoIndicatorLit;
          SetSignal(NoLocoChip, S, Signal_PreviousAspect, NoLog);
          IF Signal_PreviousIndicatorState <> NoIndicatorLit THEN
            SetIndicator(NoLocoChip, S, Signal_PreviousIndicatorState, Signal_PreviousTheatreIndicatorString, NoRoute, NOT ByUser);
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG RestoreAllSignalsToPreviousState:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { RestoreAllSignalsToPreviousState }

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
  L : Integer;
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
    IF MainWindow <> NIL THEN BEGIN
      InitialiseScreenDrawingVariables;
      SetLength(TCArray, 0);

      WITH MainWindow.Canvas DO BEGIN
        SaveLineFontName := Font.Name;
        Font.Name := 'Symbol';
        Font.Height := -MulDiv(MainWindow.ClientHeight, LineFontHeight, ZoomScalefactor);

        FOR L := 0 TO High(Lines) DO BEGIN
          WITH Lines[L] DO BEGIN
            IF (Line_TC <> UnknownTC)
            AND NOT IsElementInIntegerArray(TCArray, Line_TC)
            THEN BEGIN
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
                  HorizontalArrowAdjustment := MulDiv(MainWindow.ClientWidth, 1, ZoomScalefactor);
                  IF LeftArrowNeeded THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextOut(Line_UpX + SpeedRestrictionHorizontalSpacingScaled - ScrollBarXAdjustment,
                            Line_UpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                            LeftArrowCh);
                    TextRect(Rect(Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(LeftArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY  - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(LeftArrowCh) + TextWidth(SpeedStr) - ScrollBarXAdjustment,
                                  Line_UpY - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             Line_UpX  + SpeedRestrictionHorizontalSpacingScaled + TextWidth(LeftArrowCh) - ScrollBarXAdjustment,
                             Line_UpY  - TextHeight(SpeedStr) - (SpeedRestrictionVerticalSpacingScaled) + HorizontalArrowAdjustment - ScrollBarYAdjustment,
                             SpeedStr);
                  END;
                  IF NorthArrowNeededOnRight THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextOut(Line_UpX + SpeedRestrictionHorizontalSpacingScaled - ScrollBarXAdjustment,
                            Line_UpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                            NorthArrowCh);
                    TextRect(Rect(Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(NorthArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY  - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(NorthArrowCh) + TextWidth(SpeedStr) - ScrollBarXAdjustment,
                                  Line_UpY - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             Line_UpX  + SpeedRestrictionHorizontalSpacingScaled + TextWidth(NorthArrowCh) - ScrollBarXAdjustment,
                             Line_UpY  - TextHeight(SpeedStr) - (SpeedRestrictionVerticalSpacingScaled) + HorizontalArrowAdjustment - ScrollBarYAdjustment,
                             SpeedStr);
                  END;
                  IF SouthArrowNeededOnRight THEN BEGIN
                    { Note: we write the data on the wrong side of the track to avoid signals overwriting it }
                    TextOut(Line_UpX + SpeedRestrictionHorizontalSpacingScaled - ScrollBarXAdjustment,
                            Line_UpY - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                            SouthArrowCh);
                    TextRect(Rect(Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                                  Line_UpY  - TextHeight(SpeedStr) - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment,
                                  Line_UpX + SpeedRestrictionHorizontalSpacingScaled + TextWidth(SouthArrowCh) + TextWidth(SpeedStr) - ScrollBarXAdjustment,
                                  Line_UpY - SpeedRestrictionVerticalSpacingScaled - ScrollBarYAdjustment),
                             Line_UpX  + SpeedRestrictionHorizontalSpacingScaled + TextWidth(SouthArrowCh) - ScrollBarXAdjustment,
                             Line_UpY  - TextHeight(SpeedStr) - (SpeedRestrictionVerticalSpacingScaled) + HorizontalArrowAdjustment - ScrollBarYAdjustment,
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
      Log('EG DrawSpeedRestrictions:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawSpeedRestrictions }

PROCEDURE SetAllSignalsToDanger;
{ Sets all off signals to on }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      IF NOT Signals[S].Signal_OutOfUse THEN BEGIN
        IF (GetSignalAspect(S) <> RedAspect) THEN
          SetSignal(NoLocoChip, S, RedAspect, NOT NoLog);
        IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
          SetIndicator(NoLocoChip, S, NoIndicatorLit, '', NoRoute, NOT ByUser);
      END;
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG SetAllSignalsToDanger:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetAllSignalsToDanger }

PROCEDURE DrawSignalData(S : Integer; Str : String; Colour : Integer);
{ Put the signal name or other supplied data on the diagram }
BEGIN
  TRY
    InitialiseScreenDrawingVariables;
    WITH MainWindow.Canvas DO BEGIN
      Font.Style := [fsBold];
      Font.Color := Colour;
      Brush.Color := BackgroundColour;
      Font.Height := -MulDiv(MainWindow.ClientHeight, LineFontHeight, ZoomScalefactor);

      WITH Signals[S] DO BEGIN
        IF Signal_Direction = Up THEN
          TextOut(Signal_PostMouseRect.Right + MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor) - ScrollBarXAdjustment,
                  Signal_PostMouseRect.Top + ((Signal_PostMouseRect.Bottom - Signal_PostMouseRect.Top - TextHeight(Str)) DIV 2) - ScrollBarYAdjustment,
                  Str)
        ELSE
          IF Signal_Direction = Down THEN
            TextOut(Signal_PostMouseRect.Left - TextWidth(Str) - MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor) - ScrollBarXAdjustment,
                    Signal_PostMouseRect.Top + ((Signal_PostMouseRect.Bottom - Signal_PostMouseRect.Top - TextHeight(Str)) DIV 2) - ScrollBarYAdjustment,
                    Str);
      END; {WITH}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawSignalData:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawSignalData }

PROCEDURE DrawAllSignals(ShowSignalAndBufferStopNums, ShowTheatreDestinations : Boolean);
{ Draw all the signals }
CONST
  NoLog = True;

VAR
  S : Integer;

BEGIN
  TRY
    { Draw the signals }
    FOR S := 0 TO High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        { initialise them to red - to force SetSignal to switch them on and draw them }
        IF NOT MainWindowInitialised THEN BEGIN
          IF Signals[S].Signal_OutOfUse THEN
            SetSignal(NoLocoChip, S, NoAspect, NOT NoLog)
          ELSE
            SetSignal(NoLocoChip, S, RedAspect, NOT NoLog);
        END ELSE BEGIN
          { otherwise just draw them }
          DrawSignal(S);
          IF ShowSignalAndBufferStopNums THEN
            DrawSignalData(S, IntToStr(S), SignalNumberColour)
          ELSE
            IF ShowTheatreDestinations THEN
              DrawSignalData(S, Signals[S].Signal_AsTheatreDestination, SignalNumberColour);
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawAllSignals:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawAllSignals }

PROCEDURE DrawSignalPost(S : Integer);
{ Draws the signal post }
BEGIN
  TRY
    IF S <> UnknownSignal THEN BEGIN
      InitialiseScreenDrawingVariables;
      WITH MainWindow.Canvas DO BEGIN
        WITH Signals[S] DO BEGIN
          IF Signal_Direction = Up THEN BEGIN
            { only erase a path for the signal post if part of a signal is not also going to be erased }
            Pen.Color := BackgroundColour;
            Brush.Color := BackgroundColour;
            Rectangle(Signal_LocationX + SignalRadiusScaled - ScrollBarXAdjustment,
                      Signal_LocationY - Signal_VerticalSpacing + MainWindowCanvasPenWidth - ScrollBarYAdjustment,
                      Signal_LocationX + SignalRadiusScaled + MulDiv(MainWindow.ClientWidth, 10, ZoomScalefactor) - ScrollBarXAdjustment,
                      Signal_LocationY + SignalRadiusScaled - ScrollBarYAdjustment);

            IF Signals[S].Signal_HiddenAspect = RedAspect THEN
              Pen.Color := clRed
            ELSE
              IF (Signals[S].Signal_HiddenAspect = SingleYellowAspect) OR (Signals[S].Signal_HiddenAspect = DoubleYellowAspect) THEN
                Pen.Color := clYellow
              ELSE
                IF Signals[S].Signal_ApproachLocked THEN
                  Pen.Color := clAqua
                ELSE
                  Pen.Color := Signal_PostColour;

            { Pen.width is the width of the line outlining the signal }
            IF (Signal_Type = SemaphoreHome) OR (Signal_Type = SemaphoreDistant) THEN
              MoveTo(Signal_LocationX + SignalRadiusScaled - SignalSemaphoreHeightScaled - Pen.Width - ScrollBarXAdjustment,
                     Signal_LocationY - ScrollBarYAdjustment)
            ELSE
              MoveTo(Signal_LocationX + SignalRadiusScaled - Pen.Width - ScrollBarXAdjustment,
                     Signal_LocationY - ScrollBarYAdjustment);
            LineTo(Signal_LocationX + SignalRadiusScaled - Pen.Width + MulDiv(MainWindow.ClientWidth, 8, ZoomScalefactor) - ScrollBarXAdjustment,
                   Signal_LocationY - ScrollBarYAdjustment);
            LineTo(Signal_LocationX + SignalRadiusScaled - Pen.Width + MulDiv(MainWindow.ClientWidth, 8, ZoomScalefactor) - ScrollBarXAdjustment,
                   Signal_LocationY - Signal_VerticalSpacing + (MainWindowCanvasPenWidth DIV 2) - ScrollBarYAdjustment);
          END ELSE
            IF Signal_Direction = Down THEN BEGIN
              { only erase a path for the signal post if part of a signal is not also going to be erased }
              Pen.Color := BackgroundColour;
              Brush.Color := BackgroundColour;
              Rectangle(Signal_LocationX - SignalRadiusScaled - MulDiv(MainWindow.ClientWidth, 10, ZoomScalefactor) - ScrollBarXAdjustment,
                        Signal_LocationY - SignalRadiusScaled - ScrollBarYAdjustment,
                        Signal_LocationX - SignalRadiusScaled - ScrollBarXAdjustment,
                        Signal_LocationY + Signal_VerticalSpacing - MainWindowCanvasPenWidth - ScrollBarYAdjustment);

              IF Signals[S].Signal_HiddenAspect = NoAspect THEN
                Pen.Color := Signal_PostColour
              ELSE
                IF Signals[S].Signal_HiddenAspect = RedAspect THEN
                  Pen.Color := clRed
                ELSE
                  Pen.Color := clYellow;

              { Pen.Width is the width of the line outlining the signal }            IF (Signal_Type = SemaphoreHome) OR (Signal_Type = SemaphoreDistant) THEN
                MoveTo(Signal_LocationX - SignalRadiusScaled + SignalSemaphoreHeightScaled + Pen.Width - ScrollBarXAdjustment,
                       Signal_LocationY - ScrollBarYAdjustment)
              ELSE
                MoveTo(Signal_LocationX - SignalRadiusScaled + Pen.Width - ScrollBarXAdjustment,
                       Signal_LocationY - ScrollBarYAdjustment);
              LineTo(Signal_LocationX - SignalRadiusScaled + Pen.Width - MulDiv(MainWindow.ClientWidth, 8, ZoomScalefactor) - ScrollBarXAdjustment,
                     Signal_LocationY - ScrollBarYAdjustment);
              LineTo(Signal_LocationX - SignalRadiusScaled + Pen.Width - MulDiv(MainWindow.ClientWidth, 8, ZoomScalefactor) - ScrollBarXAdjustment,
                     Signal_LocationY + Signal_VerticalSpacing - (MainWindowCanvasPenWidth DIV 2) - ScrollBarYAdjustment);
            END;
        END; {WITH}
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawSignalPost:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawSignalPost }

PROCEDURE DrawSignal(S : Integer);
{ Draw a signal at the current position. We need to know if it is for up or down traffic, a home or distant or calling on, and what aspect it is. SignalLocation.X is the
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
  SavePenWidth : Integer;
  SColour1, SColour2  : Integer;
  SignalBottom : Integer;
  SignalLeft : Integer;
//  SignalPointA, SignalPointB, SignalPointC, SignalPointD : TPoint;
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
    PointArray : ARRAY[0..3] OF TPoint;
    Radians : Extended;
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

        PointArray[I].X := Round((TransformArray[0] * RightPivotSquareArray[I  * 2]      * SignalSemaphoreHeightScaled * 2)
                               + (TransformArray[1] * RightPivotSquareArray[(I * 2) + 1] * SignalSemaphoreWidthScaled * 2) + X - ScrollBarXAdjustment - XOffset);
        PointArray[I].Y := Round((TransformArray[2] * RightPivotSquareArray[I  * 2]      * SignalSemaphoreHeightScaled * 2)
                               + (TransformArray[3] * RightPivotSquareArray[(I * 2) + 1] * SignalSemaphoreWidthScaled * 2) + Y - ScrollBarYAdjustment);
      END ELSE BEGIN
        { Pivot = LeftPivot }
        IF (AngleInDegrees >= 90) AND (AngleInDegrees < 270) THEN
          XOffset := (2 * SignalSemaphoreHeightScaled)
        ELSE
          XOffset := -(2 * SignalSemaphoreHeightScaled);

        PointArray[I].X := Round((TransformArray[0] * LeftPivotSquareArray[I  * 2]      * SignalSemaphoreHeightScaled * 2)
                               + (TransformArray[1] * LeftPivotSquareArray[(I * 2) + 1] * SignalSemaphoreWidthScaled * 2) + X - ScrollBarXAdjustment - XOffset);
        PointArray[I].Y := Round((TransformArray[2] * LeftPivotSquareArray[I  * 2]      * SignalSemaphoreHeightScaled * 2)
                               + (TransformArray[3] * LeftPivotSquareArray[(I * 2) + 1] * SignalSemaphoreWidthScaled * 2) + Y - ScrollBarYAdjustment);
      END;
    END; {FOR}

    WITH MainWindow.Canvas DO
      Polygon(PointArray);
  END; { DrawSemaphore }

BEGIN
  TRY
    TopAspectX := 0;
    IndicatorX := 0;
    SignalLeft := 0;
    SignalTop := 0;
    SignalRight := 0;
    SignalBottom := 0;

    InitialiseScreenDrawingVariables;
    WITH MainWindow.Canvas DO BEGIN
      SColour1 := BackgroundColour;
      SColour2 := BackgroundColour;
      Font.Style := [fsBold];
      Font.Color := clWhite;
      Font.Height := -MulDiv(MainWindow.ClientHeight, TheatreFontHeight, ZoomScalefactor);

      WITH Signals[S] DO BEGIN
//Region := CreateRectRgn(Signal_MouseRect.Left, Signal_MouseRect.Top, Signal_MouseRect.Right, Signal_MouseRect.Bottom);
//SelectClipRgn(Canvas.Handle, Region);
        { store the data for debugging }

        DrawSignalPost(S);

        IF RecordLineDrawingMode OR NOT MainWindowInitialised OR Signal_StateChanged THEN
          Log('S S=' + IntToStr(S)
                 + ' A=' + AspectToStr(Signals[S].Signal_Aspect, ShortStringType)
                 + ' I=' + IndicatorStateToStr(Signals[S].Signal_IndicatorState));

        IF Signal_StateChanged THEN
          Signal_StateChanged := False;

        IF NOT ShowSignalHiddenAspects THEN
          Aspect := Signal_Aspect
        ELSE
          Aspect := Signal_HiddenAspect;

        MoveTo(Signal_LocationX - ScrollBarXAdjustment, Signal_LocationY - ScrollBarYAdjustment);

        IF Signal_Type = FourAspect THEN BEGIN
          IF Signal_Direction = Up THEN
            TopAspectX := Signal_LocationX - SignalHorizontalSpacingScaled
          ELSE
            IF Signal_Direction = Down THEN
              TopAspectX := Signal_LocationX + SignalHorizontalSpacingScaled;
        END;

        { Now draw indicators if any }
        IF Signal_Indicator <> NoIndicator THEN BEGIN
          IF Signal_Direction = Up THEN BEGIN
            IndicatorX := Signal_LocationX - IndicatorHorizontalSpacingScaled;
            IF Signal_Type = FourAspect THEN
              IndicatorX := IndicatorX - SignalHorizontalSpacingScaled;
          END ELSE
            IF Signal_Direction = Down THEN BEGIN
              IndicatorX := Signal_LocationX + IndicatorHorizontalSpacingScaled;
              IF Signal_Type = FourAspect THEN
                IndicatorX := IndicatorX + SignalHorizontalSpacingScaled;
            END;
          MoveTo(IndicatorX - ScrollBarXAdjustment, Signal_LocationY - ScrollBarYAdjustment);

          IF Signal_Indicator = TheatreIndicator THEN BEGIN
            Brush.Color := BackgroundColour;

            IF Signal_Direction = Up THEN BEGIN
              TheatreIndicatorX2 := Signal_LocationX - TheatreIndicatorHorizontalSpacingScaled;
              IF Signal_Type = FourAspect THEN
                TheatreIndicatorX2 := TheatreIndicatorX2 - SignalHorizontalSpacingScaled;
              TheatreIndicatorX1 := TheatreIndicatorX2 - TheatreBoxWidth;
            END ELSE BEGIN
              TheatreIndicatorX1 := Signal_LocationX + TheatreIndicatorHorizontalSpacingScaled;
              IF Signal_Type = FourAspect THEN
                TheatreIndicatorX1 := TheatreIndicatorX1 + SignalHorizontalSpacingScaled;
              TheatreIndicatorX2 := TheatreIndicatorX1 + TheatreBoxWidth;
            END;

            TheatreIndicatorY1 := Signal_LocationY - (TheatreBoxHeight DIV 2);
            TheatreIndicatorY2 := Signal_LocationY + (TheatreBoxHeight DIV 2);

            { First clear the area for the theatre indicator }
            Brush.Color := BackgroundColour;
            Pen.Color := BackgroundColour;
            Rectangle(Rect(TheatreIndicatorX1, TheatreIndicatorY1, TheatreIndicatorX2, TheatreIndicatorY2));

            Font.Height := -MulDiv(MainWindow.ClientHeight, LineFontHeight, ZoomScalefactor);
            Font.Color := clWhite;
            IF Signal_IndicatorState = NoIndicatorLit THEN
              { Now the outline of the theatre indicator box }
              DrawRectangularOutline(Rect(TheatreIndicatorX1,
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
                        Signal_LocationY - (TextHeight('?') DIV 2) - ScrollBarYAdjustment,
                        '?')
              ELSE
                IF Signal_Direction = Up THEN
                  TextOut(TheatreIndicatorX2 - ScrollBarXAdjustment - TextWidth(Signal_TheatreIndicatorString),
                          Signal_LocationY - (TextHeight('T') DIV 2) - ScrollBarYAdjustment,
                          Signal_TheatreIndicatorString)
                ELSE
                  TextOut(TheatreIndicatorX1 - ScrollBarXAdjustment,
                          Signal_LocationY - (TextHeight('T') DIV 2) - ScrollBarYAdjustment,
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
                IndicatorX := IndicatorX - MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor);

                IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists
                OR Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists
                THEN BEGIN
                  MiddleX := IndicatorX;
                  UpperX := IndicatorX - MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor);
                END ELSE
                  UpperX := IndicatorX + MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor);
              END ELSE
                { a slight adjustment to bring the middle arms nearer the signal }
                IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists
                OR Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists
                THEN BEGIN
                  MiddleX := IndicatorX + MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor);
                  UpperX := IndicatorX;
                END ELSE
                  UpperX := IndicatorX + MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor);
            END ELSE BEGIN
              { Signal_Direction = Down }
              IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_Exists
              OR Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_Exists
              THEN BEGIN
                LowerX := IndicatorX;
                IndicatorX := IndicatorX + MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor);

                IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists
                OR Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists
                THEN BEGIN
                  MiddleX := IndicatorX;
                  UpperX := IndicatorX + MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor);
                END ELSE
                  UpperX := IndicatorX - MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor);
              END ELSE
                { a slight adjustment to bring the middle arms nearer the signal }
                IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists
                OR Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists
                THEN BEGIN
                  MiddleX := IndicatorX - MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor);
                  UpperX := IndicatorX;
                END ELSE
                  UpperX := IndicatorX - MulDiv(MainWindow.ClientWidth, 3, ZoomScalefactor);
            END;

            FOR Indicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
              WITH Signal_JunctionIndicators[Indicator] DO BEGIN
                IF JunctionIndicator_Exists THEN BEGIN
                  CASE Indicator OF
                    UpperLeftIndicator:
                      IF Signal_Direction = Up THEN BEGIN
                        SignalLeft := UpperX;
                        SignalTop := Signal_LocationY + MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := UpperX - MulDiv(MainWindow.ClientWidth, 5, ZoomScalefactor);
                        SignalBottom := Signal_LocationY + IndicatorVerticalSpacingScaled;
                      END ELSE BEGIN
                        SignalLeft := UpperX;
                        SignalTop := Signal_LocationY - MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := UpperX + MulDiv(MainWindow.ClientWidth, 5, ZoomScalefactor);
                        SignalBottom := Signal_LocationY - IndicatorVerticalSpacingScaled;
                      END;
                    MiddleLeftIndicator:
                      IF Signal_Direction = Up THEN BEGIN
                        SignalLeft := MiddleX;
                        SignalTop := Signal_LocationY + MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := MiddleX;
                        SignalBottom := Signal_LocationY + IndicatorVerticalSpacingScaled;
                      END ELSE BEGIN
                        SignalLeft := MiddleX;
                        SignalTop := Signal_LocationY - MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := MiddleX;
                        SignalBottom := Signal_LocationY - IndicatorVerticalSpacingScaled;
                      END;
                    LowerLeftIndicator:
                      IF Signal_Direction = Up THEN BEGIN
                        SignalLeft := LowerX;
                        SignalTop := Signal_LocationY + MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := LowerX + MulDiv(MainWindow.ClientWidth, 5, ZoomScalefactor);
                        SignalBottom := Signal_LocationY + IndicatorVerticalSpacingScaled;
                      END ELSE BEGIN
                        SignalLeft := LowerX;
                        SignalTop := Signal_LocationY - MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := LowerX - MulDiv(MainWindow.ClientWidth, 5, ZoomScalefactor);
                        SignalBottom := Signal_LocationY - IndicatorVerticalSpacingScaled;
                      END;
                    UpperRightIndicator:
                      IF Signal_Direction = Up THEN BEGIN
                        SignalLeft := UpperX;
                        SignalTop := Signal_LocationY - MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := UpperX - MulDiv(MainWindow.ClientWidth, 5, ZoomScalefactor);
                        SignalBottom := Signal_LocationY - IndicatorVerticalSpacingScaled;
                      END ELSE BEGIN
                        SignalLeft := UpperX;
                        SignalTop := Signal_LocationY + MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := UpperX + MulDiv(MainWindow.ClientWidth, 5, ZoomScalefactor);
                        SignalBottom := Signal_LocationY + IndicatorVerticalSpacingScaled
                      END;
                    MiddleRightIndicator:
                      IF Signal_Direction = Up THEN BEGIN
                        SignalLeft := MiddleX;
                        SignalTop := Signal_LocationY - MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := MiddleX;
                        SignalBottom := Signal_LocationY - IndicatorVerticalSpacingScaled;
                      END ELSE BEGIN
                        SignalLeft := MiddleX;
                        SignalTop := Signal_LocationY + MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := MiddleX;
                        SignalBottom := Signal_LocationY + IndicatorVerticalSpacingScaled;
                      END;
                    LowerRightIndicator:
                      IF Signal_Direction = Up THEN BEGIN
                        SignalLeft := LowerX;
                        SignalTop := Signal_LocationY - MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := LowerX + MulDiv(MainWindow.ClientWidth, 5, ZoomScalefactor);
                        SignalBottom := Signal_LocationY - IndicatorVerticalSpacingScaled;
                      END ELSE BEGIN
                        SignalLeft := LowerX;
                        SignalTop := Signal_LocationY + MulDiv(MainWindow.ClientHeight, 3, ZoomScalefactor);
                        SignalRight := LowerX - MulDiv(MainWindow.ClientWidth, 5, ZoomScalefactor);
                        SignalBottom := Signal_LocationY + IndicatorVerticalSpacingScaled;
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
                    JunctionIndicator_MouseRect.Left := JunctionIndicator_MouseRect.Left - MulDiv(MainWindow.ClientWidth, 1, ZoomScalefactor);
                    JunctionIndicator_MouseRect.Right := JunctionIndicator_MouseRect.Right + MulDiv(MainWindow.ClientWidth, 1, ZoomScalefactor);
                  END;
                END;
              END; {WITH}
            END; {FOR}

            Pen.Width := SavePenWidth;
          END;
        END;

        IF Signal_OutOfUse
        OR ((Signal_AdjacentLine <> UnknownLine)
        AND (Lines[Signal_AdjacentLine].Line_TC = UnknownTC))
        THEN BEGIN
          { a safeguard - turn the signal off altogether! }
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

        IF (Signal_Type = SemaphoreHome) OR (Signal_Type = SemaphoreDistant) THEN BEGIN
          { First clear the area for the semaphore signal }
          Brush.Color := BackgroundColour;
          Pen.Color := BackgroundColour;
          Rectangle(Rect(Signal_MouseRect.Left - ScrollBarXAdjustment,
                         Signal_MouseRect.Top - ScrollBarYAdjustment,
                         Signal_MouseRect.Right - ScrollBarXAdjustment,
                         Signal_MouseRect.Bottom - ScrollBarYAdjustment));

          Pen.Color := clBlack;
          IF Signal_type = SemaphoreHome THEN
            Brush.Color := SignalAspectRed
          ELSE
            Brush.Color := SignalAspectYellow;

          { Now draw the rectangle }
          IF Aspect = RedAspect THEN BEGIN
            IF Signal_Direction = Up THEN
              DrawSemaphore(Signal_LocationX, Signal_LocationY, 0, RightPivot)
            ELSE
              DrawSemaphore(Signal_LocationX, Signal_LocationY, 180, RightPivot);
          END ELSE BEGIN
            Pen.Color := BackgroundColour;

            IF Signal_Quadrant = UpperQuadrant THEN BEGIN
              IF Signal_Direction = Up THEN
                DrawSemaphore(Signal_LocationX, Signal_LocationY, 45, RightPivot)
              ELSE
                DrawSemaphore(Signal_LocationX, Signal_LocationY, 225, RightPivot);
            END;

            IF Signal_Quadrant = LowerQuadrant THEN BEGIN
              IF Signal_Direction = Up THEN
                DrawSemaphore(Signal_LocationX, Signal_LocationY, 315, LeftPivot)
              ELSE
                DrawSemaphore(Signal_LocationX, Signal_LocationY, 135, LeftPivot);
            END;
          END;
        END ELSE BEGIN
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
          EllipseX1 := Signal_LocationX - SignalRadiusScaled;
          EllipseY1 := Signal_LocationY - SignalRadiusScaled;
          EllipseX2 := Signal_LocationX + SignalRadiusScaled;
          EllipseY2 := Signal_LocationY + SignalRadiusScaled;
          Ellipse(EllipseX1 - ScrollBarXAdjustment,
                  EllipseY1 - ScrollBarYAdjustment,
                  EllipseX2 - ScrollBarXAdjustment,
                  EllipseY2 - ScrollBarYAdjustment);

          IF Signal_Type = FourAspect THEN BEGIN
            Brush.Color := SColour2;
            EllipseX1 := TopAspectX - SignalRadiusScaled;
            EllipseY1 := Signal_LocationY - SignalRadiusScaled;
            EllipseX2 := TopAspectX + SignalRadiusScaled;
            EllipseY2 := Signal_LocationY + SignalRadiusScaled;
            Ellipse(EllipseX1 - ScrollBarXAdjustment,
                    EllipseY1 - ScrollBarYAdjustment,
                    EllipseX2 - ScrollBarXAdjustment,
                    EllipseY2 - ScrollBarYAdjustment);
          END;
        END;

//        DrawSignalPost(S);

        { Draw mouse access rectangles if required }
        IF ShowMouseRectangles THEN BEGIN
          DrawRectangularOutline(Signal_MouseRect, clYellow, NOT UndrawRequired, NOT UndrawToBeAutomatic);
          DrawRectangularOutline(Signal_PostMouseRect, clRed, NOT UndrawRequired, NOT UndrawToBeAutomatic);
          IF (Signal_Indicator <> NoIndicator)
          AND (Signal_Indicator <> JunctionIndicator)
          THEN
            DrawRectangularOutline(Signal_IndicatorMouseRect, clAqua, NOT UndrawRequired, NOT UndrawToBeAutomatic);

          FOR Indicator := UpperLeftIndicator TO LowerRightIndicator DO
            WITH Signal_JunctionIndicators[Indicator] DO
              IF JunctionIndicator_Exists THEN BEGIN
                DrawRectangularOutline(JunctionIndicator_MouseRect, clAqua, NOT UndrawRequired, NOT UndrawToBeAutomatic);

          END;
        END;
//SelectClipRgn(Canvas.Handle, 0);
//DeleteObject(Region);
      END; {WITH}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawSignal: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DrawSignal }

PROCEDURE SetSignal(LocoChip, S : Integer; NewAspect : AspectType; NoLog : Boolean);
{ Set the state of a particular signal and draws it }
CONST
  ShowNames = True;

BEGIN
  TRY
    WITH Signals[S] DO BEGIN
      IF Signal_Aspect <> NewAspect THEN BEGIN
        Signal_Aspect := NewAspect;
        IF NOT ProgramStartup
        AND NOT NoLog
        THEN
          Log(LocoChipToStr(LocoChip) + ' S S=' + IntToStr(S) + ' successfully set to ' + AspectToStr(Signals[S].Signal_Aspect));

        IF SystemOnline
        AND NOT ResizeMap
        THEN
          IF Signal_DecoderNum <> 0 THEN
            { uses LF100 decoders - bits usually set as follows:
              green is bit 1, red 2, single yellow 3, double yellow 3 + 4; the indicator is bit 4 (not necessarily on same decoder though)
            }
            SetSignalFunction(LocoChip, S)
          ELSE
            IF Signal_AccessoryAddress <> 0 THEN
              { uses TrainTech SC3 units for controlling Dapol semaphores }
              IF NewAspect = RedAspect THEN
                MakeSemaphoreSignalChange(LocoChip, S, Signal_AccessoryAddress, SignalOff)
              ELSE
                MakeSemaphoreSignalChange(LocoChip, S, Signal_AccessoryAddress, SignalOn);

        IF NOT ProgramStartup THEN
          InvalidateScreen(UnitRef, 'SetSignal');
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG SetSignal:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetSignal }

PROCEDURE DrawConnectionCh(ConnectionCh : String; ConnectionChRect : TRect; Bold : Boolean);
{ Draw character at line starts/ends to indicate where lines are going when they disappear off the screen }
BEGIN
  TRY
    IF MainWindow <> NIL THEN BEGIN
      WITH MainWindow.Canvas DO BEGIN
        IF Bold THEN BEGIN
          { undraw the old character }
          Font.Color := BackgroundColour;
          TextOut(ConnectionChRect.Left - ScrollBarXAdjustment, ConnectionChRect.Top - ScrollBarYAdjustment, ConnectionCh);
          { now draw the new one in bold }
          Font.Color := clWhite;
          Font.Style := [fsBold];
          Font.Height := -MulDiv(MainWindow.ClientHeight, MainWindowFontHeight * 2, ZoomScalefactor);

          TextOut(ConnectionChRect.Left - ((ConnectionChRect.Right - ConnectionChRect.Left)) - ScrollBarXAdjustment,
                  ConnectionChRect.Top - ((ConnectionChRect.Bottom - ConnectionChRect.Top) DIV 2) - ScrollBarYAdjustment,
                  ConnectionCh);
        END ELSE BEGIN
          { undraw the old character if there is one by redrawing the screen }
          Font.Color := BackgroundColour;
          Font.Style := [fsBold];
          Font.Height := -MulDiv(MainWindow.ClientHeight, MainWindowFontHeight * 2, ZoomScalefactor);

          TextOut(ConnectionChRect.Left - ((ConnectionChRect.Right - ConnectionChRect.Left)) - ScrollBarXAdjustment,
                  ConnectionChRect.Top - ((ConnectionChRect.Bottom - ConnectionChRect.Top) DIV 2) - ScrollBarYAdjustment,
                  ConnectionCh);

          { Draw the new one }
          Font.Color := ForegroundColour;
          Font.Style := [];
          Font.Height := -MulDiv(MainWindow.ClientHeight, MainWindowFontHeight, ZoomScalefactor);

          TextOut(ConnectionChRect.Left - ScrollBarXAdjustment, ConnectionChRect.Top - ScrollBarYAdjustment, ConnectionCh);
        END;

        IF ShowMouseRectangles THEN
          DrawRectangularOutline(ConnectionChRect, clFuchsia, NOT UndrawRequired, NOT UndrawToBeAutomatic);
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawConnectionCh:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawConnectionCh }

PROCEDURE DrawLineMainProcedure(L : Integer; NewLineColour : Integer; ActiveTrain : Boolean; TempLineText : String);
{ Draw an individual line, with headcode if required, and store the line colour }
CONST
  Bold = True;

VAR
  ActiveTrainText : Char;
  DownLineColour : TColor;
  LineTextStr : String;
  LineTextStrRect : TRect;
  T : Train;
  UpLineColour : TColor;
  X1, X2, Y1, Y2 : Integer;

  PROCEDURE DrawDottedLine(UpX, UpY, DownX, DownY : Integer);
  { Draw a dotted line - Delphi does not provide this option at a greater line width than one }
  VAR
    SavePenWidth, PenWidthCount : Integer;

  BEGIN
    TRY
      WITH MainWindow.Canvas DO BEGIN
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
        Log('EG END; { DrawDottedLine:' + E.ClassName +' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { DrawDottedLine }

BEGIN
  TRY
    IF MainWindow <> NIL THEN BEGIN
      LineTextStr := '';
      InitialiseScreenDrawingVariables;
      WITH MainWindow.Canvas DO BEGIN
        WITH Lines[L] DO BEGIN
          IF Line_TypeOfLine = SidingLine THEN
            Pen.Style := SidingPenStyle
          ELSE
            IF Line_TypeOfLine = FiddleyardLine THEN
              Pen.Style := FiddleyardLinePenStyle
            ELSE
              IF (Line_TypeOfLine = ProjectedLine) THEN
                Pen.Style := ProjectedLinePenStyle;

          { Work out what to display on the line }
          IF Line_TC <> UnknownTC THEN BEGIN
            WITH TrackCircuits[Lines[L].Line_TC] DO BEGIN
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
                          T := GetTrainRecord(TC_LocoChip);
                          IF T <> NIL THEN
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

          IF (TempLineText <> '')
          AND (TempLineText <> ClearLineString)
          THEN BEGIN
            { throw away the original line text and substitute the temporary text }
            LineTextStr := TempLineText;
            Font.Color := NewLineColour;
          END;

          IF Line_OutOfUseState = OutOfUse THEN BEGIN
            { Draw a red lamp and line across the track }
            X1 := Lines[L].Line_UpX;
            Y1 := Lines[L].Line_UpY - BufferStopVerticalSpacingScaled;
            Y2 := Lines[L].Line_UpY + BufferStopVerticalSpacingScaled;
            DrawRedLampAndVerticalLine(X1, Y1, Y2, ForegroundColour);

            X1 := Lines[L].Line_DownX;
            Y1 := Lines[L].Line_DownY - BufferStopVerticalSpacingScaled;
            Y2 := Lines[L].Line_DownY + BufferStopVerticalSpacingScaled;
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

            IF Lines[L].Line_TC <> UnknownTC THEN
              Log('T ' + StringOfChar(' ', 68) + '<<' + LineToStr(L) + ' (' + IntToStr(Lines[L].Line_TC) + ')'
                       + ' ' + ColourToStr(NewLineColour) + ' ' + LineTextStr + ' ' + ActiveTrainText + '>>')
            ELSE
              Log('T ' + StringOfChar(' ', 68) + '<<' + LineToStr(L) + ' ' + ColourToStr(NewLineColour) + ' ' + LineTextStr
                       + ' ' + ActiveTrainText + '>>');
          END;

          IF LineTextStr = '' THEN BEGIN
            IF ShowTrackCircuitsWhereUserMustDrive THEN
              IF Lines[L].Line_TC <> UnknownTC THEN
                IF TrackCircuits[Lines[L].Line_TC].TC_UserMustDrive THEN
                  LineTextStr := 'U';
          END;

          { Clear any previous text away }
          IF (LineTextStr <> '') OR (TempLineText <> '') THEN BEGIN
            IF (Line_UpY = Line_DownY)
            AND ((Line_DownX - Line_UpX > TextWidth('---- ')) OR (Line_UpX - Line_DownX > TextWidth('---- ')))
            THEN BEGIN
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

          { Draw this line in the colour of the adjacent lines if it is not trackcircuited }
          IF Lines[L].Line_TC = UnknownTC THEN BEGIN
            IF (Line_NextUpLine <> UnknownLine)
            AND (Line_NextDownLine <> UnknownLine)
            THEN BEGIN
              UpLineColour := Lines[Line_NextUpLine].Line_CurrentColour;
              DownLineColour := Lines[Line_NextDownLine].Line_CurrentColour;
              IF (UpLineColour = DownLineColour)
              AND (UpLineColour <> NewLineColour)
              THEN
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
          IF ShowLineOccupationDetail
          AND (LineTextStr <> '')
          AND (TempLineText <> ClearLineString)
          THEN BEGIN
            { needs text if there's room }
            IF (Line_UpY = Line_DownY)
            AND ((Line_DownX - Line_UpX > TextWidth(LineTextStr))
                 OR (Line_UpX - Line_DownX > TextWidth(LineTextStr)))
            THEN BEGIN
              { clear space for the text }
              Brush.Color := BackgroundColour;
              IF ScreenColoursSetForPrinting THEN
                Font.Color := clBlack;
              Font.Height := -MulDiv(MainWindow.ClientHeight, LineFontHeight, ZoomScalefactor);

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

          { Draw adjacent lines if they are not trackcircuited }
          IF (Line_NextUpLine <> UnknownLine)
          AND (Line_NextDownLine <> UnknownLine)
          THEN BEGIN
            IF (Lines[Line_NextUpLine].Line_TC = UnknownTC)
            AND (Lines[Line_NextDownLine].Line_TC = UnknownTC)
            THEN BEGIN
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
          IF (Lines[L].Line_UpConnectionCh <> '')
          AND (Lines[L].Line_UpConnectionCh <> ' ')
          THEN
            DrawConnectionCh(Lines[L].Line_UpConnectionCh, Lines[L].Line_UpConnectionChRect, NOT Bold);
          IF (Lines[L].Line_DownConnectionCh <> '')
          AND (Lines[L].Line_DownConnectionCh <> ' ')
          THEN
            DrawConnectionCh(Lines[L].Line_DownConnectionCh, Lines[L].Line_DownConnectionChRect, NOT Bold);

          IF ShowMouseRectangles THEN BEGIN
            IF Line_UpY = Line_DownY THEN
              { horizontal lines }
              DrawRectangularOutline(Line_MouseRect, clGreen, NOT UndrawRequired, NOT UndrawToBeAutomatic)
            ELSE
              { diagonal lines }
              DrawRectangularOutline(Line_MouseRect, clLime, NOT UndrawRequired, NOT UndrawToBeAutomatic);
          END;
        END; {WITH}
      END; {WITH}
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DrawLineMainProcedure: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawLineMainProcedure }

PROCEDURE DrawLine{1}(L : Integer; NewLineColour : Integer; ActiveTrain : Boolean); Overload;
{ Draw an individual line, with headcode if required, and store the line colour }
CONST
  TempLineText = '';

BEGIN
  DrawLineMainProcedure(L, NewLineColour, ActiveTrain, TempLineText);
END; { DrawLine-1 }

PROCEDURE DrawLine{2}(L : Integer; NewLineColour : Integer; ActiveTrain : Boolean; TempLineText : String); Overload;
{ Draw an individual line, with headcode if required, and store the line colour }
BEGIN
  DrawLineMainProcedure(L, NewLineColour, ActiveTrain, TempLineText);
END; { DrawLine-2 }

PROCEDURE DrawTrackCircuit{1}(TC : Integer; TCColour : TColour); Overload;
{ Draws a given trackcircuit }
CONST
  ActiveTrain = True;

VAR
  L : Integer;

BEGIN
  TRY
    IF TC <> UnknownTC THEN BEGIN
      L := 0;
      WHILE L <= High(Lines) DO BEGIN
        IF Lines[L].Line_TC = TC THEN
          DrawLine(L, TCColour, ActiveTrain);
        Inc(L);
      END;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawTrackCircuit-1:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawTrackCircuit-1 }

PROCEDURE DrawTrackCircuit{2}(TC : Integer; TCColour : TColour; TempLineText : String); Overload;
{ Draws a given trackcircuit - this version is used by Replay to add train descriptions }
CONST
  ActiveTrain = True;

VAR
  L : Integer;

BEGIN
  TRY
    IF TC <> UnknownTC THEN BEGIN
      L := 0;
      WHILE L <= High(Lines) DO BEGIN
        IF Lines[L].Line_TC = TC THEN
          DrawLine(L, TCColour, ActiveTrain, TempLineText);
        Inc(L);
      END;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawTrackCircuit-2:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawTrackCircuit-2 }

PROCEDURE DrawTrackCircuitsWithAdjoiningTrackCircuits(TC : Integer; TCColour1, TCColour2 : TColour);
{ Draw a trackcircuit and show which trackcircuits adjoin it }
VAR
  AdjacentUpTC, AdjacentDownTC : Integer;

BEGIN
  TRY
    { Draw the original trackcircuit }
    DrawTrackCircuit(TC, TCColour1);

    FindAdjoiningTrackCircuits(TC, AdjacentUpTC, AdjacentDownTC);
    { and draw the adjacent ones }
    DrawTrackCircuit(AdjacentUpTC, TCColour2);
    DrawTrackCircuit(AdjacentDownTC, TCColour2);

    Debug('Drawing original TC=' + IntToStr(TC)
          + ' and adjacent TCs:' + IntToStr(AdjacentUpTC) + ' and ' + IntToStr(AdjacentDownTC));
  EXCEPT
    ON E : Exception DO
      Log('EG DrawTrackCircuitsWithAdjoiningTrackCircuits:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawTrackCircuitsWithAdjoiningTrackCircuits }

PROCEDURE FindAdjoiningTrackCircuits(TC : Integer; OUT AdjoiningUpTC, AdjoiningDownTC : Integer);
{ Work out which are the adjacent trackcircuits. Does not trace along all lines, just the way points are set. }
VAR
  L : Integer;
  Next : NextLineRouteingType;
  NextPoint : Integer;
  TCFound : Boolean;

  PROCEDURE FollowThatLine(CurrentLine : Integer; SearchDirection : DirectionType);
  { Follow lines and points until we find a different trackcircuit }
  VAR
    ExitFunction : Boolean;

    FUNCTION FoundAdjacentTrackCircuit(CurrentLine : Integer) : Boolean;
    BEGIN
      Result := False;
      TRY
        IF (Lines[CurrentLine].Line_TC <> UnknownTC)
        AND (Lines[CurrentLine].Line_TC <> TC)
        THEN BEGIN
          Result := True;
          CASE SearchDirection OF
            Up:
              AdjoiningUpTC := Lines[CurrentLine].Line_TC;
            Down:
              AdjoiningDownTC := Lines[CurrentLine].Line_TC;
          END; {CASE}
        END;
      EXCEPT
        ON E : Exception DO
          Log('EG END; { FoundAdjacentTrackCircuit:' + E.ClassName +' error raised, with message: '+ E.Message);
      END; {TRY}
    END; { FoundAdjacentTrackCircuit }

  BEGIN
    TRY
      ExitFunction := False;
      WHILE NOT ExitFunction DO BEGIN
        IF SearchDirection = Up THEN
          Next := Lines[CurrentLine].Line_NextUpType
        ELSE
          Next := Lines[CurrentLine].Line_NextDownType;

        CASE Next OF
          PointIsNext:
            BEGIN
              IF SearchDirection = Up THEN
                NextPoint := Lines[CurrentLine].Line_NextUpPoint
              ELSE
                NextPoint := Lines[CurrentLine].Line_NextDownPoint;

              { where to go next }
              IF SearchDirection = Points[NextPoint].Point_FacingDirection THEN BEGIN
                { a facing point - see which way it's set }
                IF Points[NextPoint].Point_PresentState = Straight THEN BEGIN
                  IF LockDebuggingMode THEN
                    DrawPoint(NextPoint, clLime);

                  CurrentLine := Points[NextPoint].Point_StraightLine;
                  IF FoundAdjacenttrackCircuit(CurrentLine) THEN
                    ExitFunction := True;
                END ELSE
                  IF Points[NextPoint].Point_PresentState = Diverging THEN BEGIN
                    IF LockDebuggingMode THEN
                      DrawPoint(NextPoint, clLime);

                    CurrentLine := Points[NextPoint].Point_DivergingLine;
                    IF FoundAdjacenttrackCircuit(CurrentLine) THEN
                      ExitFunction := True;
                  END ELSE BEGIN
                    { Points[NextPoint].Point_PresentState = PointStateUnknown }
                    IF LockDebuggingMode THEN
                      DrawPoint(NextPoint, clRed);
                    ExitFunction := True;
                  END;
              END ELSE BEGIN
                { a trailing point - if it's not set in our direction, stop searching here }
                IF ((CurrentLine = Points[NextPoint].Point_StraightLine)
                AND (Points[NextPoint].Point_PresentState = Straight))
                THEN BEGIN
                  CurrentLine := Points[NextPoint].Point_HeelLine;
                  IF FoundAdjacenttrackCircuit(CurrentLine) THEN
                      ExitFunction := True;

                  IF LockDebuggingMode THEN
                    DrawPoint(NextPoint, clLime)
                  ELSE
                    DrawPoint(NextPoint, ForegroundColour);
                END ELSE
                  IF ((CurrentLine = Points[NextPoint].Point_DivergingLine)
                  AND (Points[NextPoint].Point_PresentState = Diverging))
                  THEN BEGIN
                    CurrentLine := Points[NextPoint].Point_HeelLine;
                    IF FoundAdjacenttrackCircuit(CurrentLine) THEN
                        ExitFunction := True;

                    IF LockDebuggingMode THEN
                      DrawPoint(NextPoint, clLime)
                    ELSE
                      DrawPoint(NextPoint, ForegroundColour);
                  END ELSE BEGIN
                    { if it's not set in our direction, stop searching here }
                    IF LockDebuggingMode THEN
                      DrawPoint(NextPoint, clRed)
                    ELSE
                      DrawPoint(NextPoint, ForegroundColour);
                    ExitFunction := True;
                  END;
              END;
            END;
          LineIsNext:
            BEGIN
              { where to go next }
              IF SearchDirection = Up THEN
                CurrentLine := Lines[CurrentLine].Line_NextUpLine
              ELSE
                CurrentLine := Lines[CurrentLine].Line_NextDownLine;

              IF FoundAdjacenttrackCircuit(CurrentLine) THEN
                ExitFunction := True;
            END;
          EndOfLineIsNext:
            ExitFunction := True;
          UnknownNextLineRouteingType:
            ShowMessage('UnknownNextLineRouteingType at ' + LineToStr(CurrentLine));
        END; {CASE}
      END;
    EXCEPT
      ON E : Exception DO
        Log('EG END; { FollowThatLine:' + E.ClassName +' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { FollowThatLine }

BEGIN
  TRY
    AdjoiningUptc := UnknownTC;
    AdjoiningDowntc := UnknownTC;

    IF TC <> UnknownTC THEN BEGIN
      L := 0;
      TCFound := False;
      WHILE (L <= High(Lines))
      AND NOT TCFound
      DO BEGIN
        IF Lines[L].Line_TC = TC THEN BEGIN
          TCFound := True;

          FollowThatLine(L, Up);
          FollowThatLine(L, Down);
        END;
        Inc(L);
      END; {WHILE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FindAdjoiningTrackCircuits:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FindAdjoiningTrackCircuits }

PROCEDURE SetTrackCircuitStateMainProcedure(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType; Explanation : String);
{ Set whether and how the trackcircuit is occupied and give an explanation if any }
CONST
  DoNotWriteMessage = True;
  DiagramsLoading = True;

VAR
  AdjacentTrackCircuits : IntegerArrayType;
  AdjacentUpTC, AdjacentDownTC : Integer;
  ErrorMsg : String;
  I, J : Integer;
  LineArray : LineArrayType;
  Location : Integer;
  LocationOccupationCount : Integer;
  TempLocationLocoChips : IntegerArrayType;
  LocoFound : Boolean;
  OK : Boolean;
  P : Integer;
  T : Train;

BEGIN
  TRY
    IF (TC < 0) OR (TC > High(TrackCircuits)) THEN BEGIN
      ShowMessage('Non-existent TC=' + IntToStr(TC) + ' is trying to be set to ' + TrackCircuitStateToStr(NewState));
      Log('X Non-existent TC=' + IntToStr(TC) + ' is trying to be set to ' + TrackCircuitStateToStr(NewState));
      InvalidateScreen(UnitRef, 'SetTrackCircuitStateMainProcedure');
    END ELSE BEGIN
      WITH TrackCircuits[TC] DO BEGIN
        IF (NewState <> TrackCircuits[TC].TC_OccupationState) OR (LocoChip <> TrackCircuits[TC].TC_LocoChip) THEN BEGIN
          { Store the state in case we have to revert, but don't do it on both occupation and end of occupation or we can't revert }
          TC_PreviousOccupationState := TC_OccupationState;
          IF TC_LocoChip <> UnknownLocoChip THEN BEGIN
            Log('T TC=' + IntToStr(TC) + '''s TC_PreviousLocoChip set to ' + LocoChipToStr(TC_LocoChip));
            TC_PreviousLocoChip := TC_LocoChip;
          END;

          { If track circuit is now unoccupied, having been system occupied, see if part of a route needs clearing in advance of the full route tidying up in ClearARoute }
          IF NOT RedrawScreen THEN BEGIN
            T := GetTrainRecord(TC_LocoChip);
            IF (TC_PreviousOccupationState = TCSystemOccupation)
            OR ((T <> NIL)
            AND NOT T^.Train_UseTrailingTrackCircuits
            AND (TC_PreviousOccupationState = TCFeedbackOccupation))
            THEN BEGIN
              TC_OccupationState := NewState;
              IF NewState = TCUnoccupied THEN BEGIN
                IF TrackCircuits[TC].TC_LockedForRoute <> UnknownRoute THEN BEGIN
                  FOR P := 0 TO High(Points) DO BEGIN
                    { using a FOR loop here, as there may be more than one point locked by the same TC route }
                    IF Points[P].Point_TCAtHeel = TC THEN BEGIN
                      IF Points[P].Point_TCAtHeel = TC THEN BEGIN
                        IF PointIsLockedByASpecificRoute(P, TrackCircuits[TC].TC_LockedForRoute) THEN BEGIN
                          Log(LocoChipToStr(TC_LocoChip) + ' R Point ' + IntToStr(P) + ' was locked by'
                                                         + ' R=' + IntToStr(TrackCircuits[TC].TC_LockedForRoute) + ' - now unlocked');
                          UnlockPointLockedBySpecificRoute(P, TrackCircuits[TC].TC_LockedForRoute, DoNotWriteMessage);

                          { and any related crossover or three-way points too }
                          IF Points[P].Point_OtherPoint <> UnknownPoint THEN
                            IF PointIsLockedByASpecificRoute(Points[P].Point_OtherPoint, TrackCircuits[TC].TC_LockedForRoute) THEN
                              UnlockPointLockedBySpecificRoute(Points[P].Point_OtherPoint, TrackCircuits[TC].TC_LockedForRoute, DoNotWriteMessage);
                        END;
                      END;
                    END;
                  END; {FOR}

                  { and see if any route-locking drawing needs to be removed too }
                  LineArray := GetLinesForTrackCircuit(TC);
                  FOR I := 0 TO High(LineArray) DO BEGIN
                    IF Lines[LineArray[I]].Line_RouteLockingForDrawing = TrackCircuits[TC].TC_LockedForRoute THEN BEGIN
                      Log(LocoChipToStr(TC_LocoChip) + ' R L=' + LineToStr(LineArray[I]) + ' RouteLockingForDrawing was '
                                                     + IntToStr(Lines[LineArray[I]].Line_RouteLockingForDrawing) + ' now set to unknown route');
                      Lines[LineArray[I]].Line_RouteLockingForDrawing := UnknownRoute;
                    END;
                    IF Lines[LineArray[I]].Line_RouteSet <> - 1 THEN BEGIN
                      Log(LocoChipToStr(TC_LocoChip) + ' R L=' + LineToStr(LineArray[I]) + ' SubRouteSet set to False');
                      Lines[LineArray[I]].Line_RouteSet := UnknownRoute;
                    END;
                  END;

                  { or if a signal needs unlocking too }
                  FOR I := 0 TO High(TC_AdjacentSignals) DO BEGIN
                    IF SignalIsLockedBySpecificRoute(TC_AdjacentSignals[I], TrackCircuits[TC].TC_LockedForRoute) THEN BEGIN
                      Log(LocoChipToStr(TC_LocoChip) + ' R S=' + IntToStr(TC_AdjacentSignals[I]) + ' unlocked by R=' + IntToStr(TrackCircuits[TC].TC_LockedForRoute));
                      UnlockSignalLockedBySpecificRoute(TC_AdjacentSignals[I], TrackCircuits[TC].TC_LockedForRoute);
                    END;
                  END;

                  UnlockTrackCircuitRouteLocking(TC);
                END;
              END;
            END;

            TC_OccupationState := NewState;

            IF (NewState = TCOutOfUseSetByUser) OR (NewState = TCOutOfUseAsNoFeedbackReceived) THEN BEGIN
              Location := GetLocationFromTrackCircuit(TC);
              { reallocate any train expected to occupy the location }
              IF Location <> UnknownLocation THEN BEGIN
                FOR LocationOccupationCount := 0 TO High(LocationOccupations[Location]) DO BEGIN
                  { Store any locochip numbers now as we have to clear the array before we can reallocate them. (If we don't, it is possible that they will be reallocated
                    to the same, defunct, location.
                  }
                  IF LocationOccupations[Location, LocationOccupationCount].LocationOccupation_LocoChip <> NoLocoChip THEN
                    AppendToIntegerArray(TempLocationLocoChips, LocationOccupations[Location, LocationOccupationCount].LocationOccupation_LocoChip);
                END;

                { Now mark the location out of use }
                ClearLocationOccupationsForSpecificLocation(Location);
                SetLocationOccupationAllDayState(NoLocoChip, Location, LocationOutOfUseOccupation, ErrorMsg, OK);
              END;

              IF Length(TempLocationLocoChips) > 0 THEN BEGIN
                FOR I := 0 TO High(TempLocationLocoChips) DO BEGIN
                  T := GetTrainRecord(TempLocationLocoChips[I]);
                  IF T <> NIL THEN
                    SetUpTrainLocationOccupationsAbInitio(T, OK);
                END;
              END;
            END;

            IF NewState <> TCUnoccupied THEN BEGIN
              TC_LocoChip := LocoChip;
              TC_Headcode := '????'; { **** }
            END ELSE BEGIN
              { TC is now unoccupied - reset its loco occupation - but not if it's a system occupation }
              IF (TC_LocoChip <> UnknownLocoChip)
              AND (NewState = TCUnoccupied)
              THEN BEGIN
                Log('T TC=' + IntToStr(TC) + ' TC_LocoChip=' + LocoChipToStr(TC_LocoChip) + ' set to unknown loco Chip');
                TC_LocoChip := UnknownLocoChip;
              END;

              TC_Headcode := '----';

              IF ((TrackCircuits[TC].TC_PreviousOccupationState = TCOutOfUseSetByUser)
              OR (TrackCircuits[TC].TC_PreviousOccupationState = TCOutOfUseAsNoFeedbackReceived))
              THEN BEGIN
                Location := GetLocationFromTrackCircuit(TC);
                ClearLocationOccupationsForSpecificLocation(Location);
              END;
            END;

            { Is this a signal-resetting trackcircuit? If so, reset the signal. And put in a timing update here - for delays **** }
            IF (NewState = TCFeedbackOccupation)
            AND (TC_ResettingSignal <> UnknownSignal)
            THEN BEGIN
              WITH Signals[TC_ResettingSignal] DO BEGIN
                IF Signal_Aspect <> RedAspect THEN BEGIN
                  PullSignal(NoLocoChip, TC_ResettingSignal, NoIndicatorLit, NoRoute, NoSubRoute, UnknownLine, TC, NOT ByUser, OK);
                  IF OK THEN BEGIN
                    Log('S S=' + IntToStr(TC_ResettingSignal) + ' reset by TC=' + IntToStr(TC));
                    { also reset any hidden aspects }
                    IF Signals[TC_ResettingSignal].Signal_HiddenAspect <> RedAspect THEN BEGIN
                      Signals[TC_ResettingSignal].Signal_HiddenAspect := NoAspect;
                    END;
                  END ELSE BEGIN
                    IF NOT Signal_FailedToResetFlag THEN BEGIN
                      Log('SG S=' + IntToStr(TC_ResettingSignal) + ' failed to reset');
                      Signal_FailedToResetFlag := True;
                    END;
                  END;
                END; {WITH}
              END;
              TC_ResettingSignal := UnknownSignal;
            END;

            IF NOT InAutoMode
            AND ((NewState = TCFeedbackOccupation) OR (NewState = TCLocoOutOfPlaceOccupation))
            AND (TC_LocoChip = UnknownLocoChip)
            AND NOT ProgramStartup
            AND NOT LocoSpeedTimingMode
            THEN BEGIN
              { we've not been given a loco, so see if we can work out which loco it is }
              Log('E TC=' + IntToStr(TC) + ' feedback occupied but no loco number supplied - searching adjacent TCs...');
              FindAdjoiningTrackCircuits(TC, AdjacentUpTC, AdjacentDownTC);

              LocoFound := False;
              I := 0;
              J := 0;
              { see if any adjacent trackcircuits are marked as occupied }
              SetLength(AdjacentTrackCircuits, 0);
              IF AdjacentUptc <> UnknownTC THEN
                AppendToIntegerArray(AdjacentTrackCircuits, AdjacentUpTC);
              IF AdjacentDowntc <> UnknownTC THEN
                AppendToIntegerArray(AdjacentTrackCircuits, AdjacentDownTC);
              WHILE (I <= High(AdjacentTrackCircuits))
              AND NOT LocoFound
              DO BEGIN
                IF ((TrackCircuits[AdjacentTrackCircuits[I]].TC_OccupationState = TCFeedbackOccupation)
                    OR (TrackCircuits[AdjacentTrackCircuits[I]].TC_OccupationState = TCLocoOutOfPlaceOccupation))
                AND (TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip <> UnknownLocoChip)
                THEN BEGIN
                  Log('E ...adjacent TC=' + IntToStr(AdjacentTrackCircuits[I])
                         + ' is marked as feedback occupied by ' + LocoChipToStr(TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip)
                         + ', so maybe this is the loco we want');
                  { we've putatively found it }
                  LocoFound := True;
                  { but make sure the other adjacent TCs are unoccupied, or we won't know which loco it is that's moving }
                  WHILE J <= High(AdjacentTrackCircuits) DO BEGIN
                    IF I <> J THEN BEGIN
                      IF ((TrackCircuits[AdjacentTrackCircuits[J]].TC_OccupationState = TCFeedbackOccupation)
                         OR (TrackCircuits[AdjacentTrackCircuits[J]].TC_OccupationState = TCLocoOutOfPlaceOccupation))
                      AND (TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip <> UnknownLocoChip)
                      AND (TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip <> TrackCircuits[AdjacentTrackCircuits[J]].TC_LocoChip)
                      THEN BEGIN
                        LocoFound := False;
                        Log('E ...adjacent TC=' + IntToStr(AdjacentTrackCircuits[J])
                               + ' is marked as feedback occupied by ' + LocoChipToStr(TrackCircuits[AdjacentTrackCircuits[J]].TC_LocoChip)
                               + ', so do not know which loco to select');
                      END;
                    END;
                    Inc(J);
                  END; {WHILE}
                  IF NOT LocoFound THEN
                    Log('E ... but no loco found in adjacent TCs')
                  ELSE BEGIN
                    TC_LocoChip := TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip;
                    T := GetTrainRecord(TC_LocoChip);
                    IF T <> NIL THEN
                      T^.Train_CurrentTC := TC;
                    Log(LocoChipToStr(TC_LocoChip) + ' E ...found loco in adjacent TC=' + IntToStr(AdjacentTrackCircuits[I])
                                                   + ' so assuming it has moved to TC=' + IntToStr(TC));
                  END;
                END;
                Inc(I);
              END; {WHILE}
            END;
          END;

          IF Explanation <> '' THEN
            Log(LocoChipToStr(TC_LocoChip) + ' T Setting TC=' + IntToStr(TC) + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
                                           + ' ' + TrackCircuitStateToStr(NewState) + ' (' + Explanation + ')')
          ELSE
            Log(LocoChipToStr(TC_LocoChip) + ' T Setting TC=' + IntToStr(TC) + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
                                           + ' ' + TrackCircuitStateToStr(NewState));

          IF NOT ProgramStartup THEN
            InvalidateScreen(UnitRef, 'SetTrackCircuitStateMainProcedure');
        END;
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG SetTrackCircuitStateMainProcedure: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SetTrackCircuitStateMainProcedure }

PROCEDURE SetTrackCircuitState{1}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the trackcircuit is occupied, and mark it with a loco chip number }
CONST
  Explanation = '';

BEGIN
  SetTrackCircuitStateMainProcedure(LocoChip, TC, NewState, Explanation);
END; { SetTrackCircuitState-1 }

PROCEDURE SetTrackCircuitState{2}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the trackcircuit is occupied, mark it with a loco chip number, and give an explanation }
BEGIN
  SetTrackCircuitStateMainProcedure(LocoChip, TC, NewState, Explanation);
END; { SetTrackCircuitState-2 }

PROCEDURE SetTrackCircuitState{3}(TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the trackcircuit is occupied }
CONST
  Explanation = '';

VAR
  LocoChip : Integer;
  T : Train;
  TCFound : Boolean;

BEGIN
  TRY
    { See if it belongs to a loco }
    LocoChip := UnknownLocoChip;
    TCFound := False;
    T := TrainList;
    WHILE (T <> NIL)
    AND NOT TCFound
    DO BEGIN
      IF T^.Train_DiagramFound THEN BEGIN
        IF T^.Train_CurrentTC = TC THEN BEGIN
          TCFound := True;
          LocoChip := T^.Train_LocoChip;
          Log('T TC=' + IntToStr(TC) + ' allocated to loco ' + LocoChipToStr(LocoChip) + ' as it is that loco''s Train_CurrentTC');
        END;
      END;
      T := T^.Train_NextRecord;
    END; {WHILE}

    SetTrackCircuitStateMainProcedure(LocoChip, TC, NewState, Explanation);
  EXCEPT
    ON E : Exception DO
      Log('EG SetTrackCircuitState-3:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetTrackCircuitState-3 }

PROCEDURE SetTrackCircuitState{4}(TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the trackcircuit is occupied and give an explanation. Also see whether we want it recorded in the Location occupation array - this is to avoid
  duplicate recordings at startup.
}
BEGIN
  SetTrackCircuitStateMainProcedure(NoLocoChip, TC, NewState, Explanation);
END; { SetTrackCircuitState-4 }

PROCEDURE NoteOutOfUseFeedbackUnitTrackCircuitsAtStartup;
{ Work out which track circuits are unavailable because we're not getting initial feedback from them }
VAR
  FeedbackData : FeedbackRec;
  FeedbackType : TypeOfFeedBackType;
  I, J : Integer;
  TC : Integer;
  TCAboveFeedbackUnit : Integer;

BEGIN
  TRY
    FOR I := 0 TO High(NoFeedbackList) DO BEGIN
      FeedbackData.Feedback_Unit := StrToInt(NoFeedbackList[I]);
      FOR J := 1 TO 8 DO BEGIN
        FeedbackData.Feedback_Input := J;
        ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, TC);
        IF FeedbackType = TrackCircuitFeedbackDetector THEN BEGIN
          IF TC <> UnknownTC THEN
            IF GetTrackCircuitState(TC) <> TCOutOfUseSetByUser THEN
              SetTrackCircuitState(TC, TCOutOfUseAsNoFeedbackReceived, 'no feedback obtained at startup');
        END;
      END;
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG NoteOutOfUseFeedbackUnitTrackCircuitsAtStartup:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { NoteOutOfUseFeedbackUnitTrackCircuitsAtStartup }

PROCEDURE DrawPoint(P : Integer; Colour : TColour);
{ Draw a point }
VAR
  RouteLockingArray : IntegerArrayType;

BEGIN
  TRY
    InitialiseScreenDrawingVariables;
    WITH MainWindow.Canvas DO BEGIN
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
                  AND (Lines[Point_DivergingLine].Line_TC <> UnknownTC)
                  AND (TrackCircuits[Lines[Point_DivergingLine].Line_TC].TC_OccupationState <> TCUnoccupied)
                  THEN
                    Pen.Color := GetTrackCircuitStateColour(Lines[Point_DivergingLine].Line_TC)
                  ELSE
                    IF (Point_PresentState = Straight)
                    AND (Lines[Point_StraightLine].Line_TC <> UnknownTC)
                    AND (TrackCircuits[Lines[Point_StraightLine].Line_TC].TC_OccupationState <> TCUnoccupied)
                    THEN
                      Pen.Color := GetTrackCircuitStateColour(Lines[Point_StraightLine].Line_TC)
                    ELSE
                      IF (Point_PresentState = Diverging)
                      AND (Lines[Point_DivergingLine].Line_TC = UnknownTC)
                      AND ((Lines[Point_HeelLine].Line_TC <> UnknownTC)
                           AND (TrackCircuits[Lines[Point_HeelLine].Line_TC].TC_OccupationState <> TCUnoccupied))
                      THEN
                        Pen.Color := GetTrackCircuitStateColour(Lines[Point_HeelLine].Line_TC)
                      ELSE
                        IF (Point_PresentState = Straight)
                        AND (Lines[Point_StraightLine].Line_TC = UnknownTC)
                        AND ((Lines[Point_HeelLine].Line_TC <> UnknownTC)
                             AND (TrackCircuits[Lines[Point_HeelLine].Line_TC].TC_OccupationState <> TCUnoccupied))
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
      Log('EG DrawPoint: ' + E.ClassName +' error raised, with message: ' + E.Message);
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
    WITH MainWindow.Canvas DO BEGIN
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
                  NumberText := 'CO' + IntToStr(P) + '/' + IntToStr(Point_OtherPoint);
                  Font.Color := clAqua;
                END;
              ThreeWayPointA:
                BEGIN
                  NumberText := '3a ' + IntToStr(P) + '/' + IntToStr(Point_OtherPoint);
                  Font.Color := clRed;
                END;
              ThreeWayPointB:
                BEGIN
                  NumberText := '3b ' + IntToStr(P) + '/' + IntToStr(Point_OtherPoint);
                  Font.Color := clMoneyGreen;
                END;
              SingleSlip:
                BEGIN
                  NumberText := 'S ' + IntToStr(P) + '/' + IntToStr(Point_OtherPoint);
                  Font.Color := clFuchsia;
                END;
              DoubleSlip:
                BEGIN
                  NumberText := 'D ' + IntToStr(P) + '/' + IntToStr(Point_OtherPoint);
                  Font.Color := clSkyBlue;
                END;
              ProtectedPoint:
                BEGIN
                  NumberText := 'PP ' + IntToStr(P) + '/' + IntToStr(Point_OtherPoint);
                  Font.Color := clFWPPink;
                END;
              CatchPointUp:
                BEGIN
                  NumberText := 'CPU ' + IntToStr(P) + '/' + IntToStr(Point_OtherPoint);
                  Font.Color := clFWPPink;
                END;
              CatchPointDown:
                BEGIN
                  NumberText := 'CPD ' + IntToStr(P) + '/' + IntToStr(Point_OtherPoint);
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
                  { extract what kind of feedback it is (FeedbackNum is only use for trackcircuits) }
                  ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum);
                  IF FeedbackType = PointFeedbackDetector THEN BEGIN
                    IF (Point_FeedbackUnit = I)
                    AND (Point_FeedbackInput = J)
                    THEN BEGIN
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
                Font.Color := clYellow;
                IF PointIsLocked(P, LockingMsg) THEN
                  NumberText := IntToStr(P);
              END ELSE
                { just displaying all point numbers }
                NumberText := IntToStr(P);
            END;

        WITH Point_MouseRect DO BEGIN
          Font.Height := -MulDiv(MainWindow.ClientHeight, LineFontHeight, ZoomScaleFactor);
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
      Log('EG DrawPointNum:' + E.ClassName +' error raised, with message: '+ E.Message);
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
    WITH MainWindow.Canvas DO BEGIN
      Brush.Color := BackgroundColour;
      Font.Style := [fsBold];
      { show which Lenz feedback unit is being used }
      Font.Height := -MulDiv(MainWindow.ClientHeight, LineFontHeight, ZoomScaleFactor);

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
          { extract what kind of feedback it is (FeedbackNum is only use for trackcircuits) }
          ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum);
          IF FeedbackType = PointFeedbackDetector THEN BEGIN
            FOR P := 0 TO High(Points) DO BEGIN
              WITH Points[P] DO BEGIN
                IF (Point_FeedbackUnit = I)
                AND (Point_FeedbackInput = J)
                THEN BEGIN
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
      Log('EG DrawPointFeedbackDataInSeparateColours:' + E.ClassName +' error raised, with message: '+ E.Message);
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

    WITH MainWindow.Canvas DO BEGIN
      Brush.Color := BackgroundColour;
      Font.Style := [fsBold];
      Font.Height := -MulDiv(MainWindow.ClientHeight, LineFontHeight, ZoomScaleFactor);

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
      Log('EG DrawLenzPointUnitGroups:' + E.ClassName +' error raised, with message: '+ E.Message);
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

      IF ShowPointDetail
      AND Points[P].Point_OutOfUse
      THEN
        DrawPointNum(P, PointOutOfUseColour)
      ELSE
        IF ShowPointDetail
        AND (Points[P].Point_FacingDirection = Up)
        THEN
          DrawPointNum(P, PointUpFacingColour)
        ELSE
          IF ShowPointDetail
          AND (Points[P].Point_FacingDirection = Down)
          THEN
            DrawPointNum(P, PointDownFacingColour)
          ELSE
            IF ShowLenzPointNumbers THEN
              DrawPointNum(P, LenzPointNumberColour)
            ELSE
              IF ShowPointType THEN
                DrawPointNum(P, LenzPointNumberColour)
              ELSE
                IF ShowPointFeedbackDataInSeparateColours THEN
                  DrawPointFeedbackDataInSeparateColours
                ELSE
                  IF ShowPointFeedbackDataInUse THEN
                    DrawPointNum(P, LenzPointNumberColour)
                  ELSE
                    IF ShowLenzPointUnitGroups THEN
                      DrawLenzPointUnitGroups
                    ELSE
                      IF ShowPointsThatAreLocked THEN
                        DrawPointNum(P, ShowPointLockedColour)
                      ELSE
                        IF ShowPointDefaultState
                        AND (Points[P].Point_DefaultState <> PointStateUnknown)
                        THEN
                          { only draw the point number if the point has a default state }
                          DrawPointNum(P, ShowPointDefaultStateColour);

      { Draw mouse access rectangles }
      IF ShowMouseRectangles THEN
        DrawRectangularOutline(Points[P].Point_MouseRect, clWhite, NOT UndrawRequired, NOT UndrawToBeAutomatic);
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawAllPoints:' + E.ClassName +' error raised, with message: '+ E.Message);
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
      WITH MainWindow.Canvas DO BEGIN
        WITH Platforms[P].Platform_Rect DO BEGIN
          Font.Height := -MulDiv(MainWindow.ClientHeight, PlatformNumberFontHeight, ZoomScaleFactor);
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
        Log('EG END; { DrawPlatformNumber:' + E.ClassName +' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { DrawPlatformNumber }

VAR
  P : Integer;

BEGIN
  TRY
    InitialiseScreenDrawingVariables;
    WITH MainWindow.Canvas DO BEGIN
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
      Log('EG DrawPlatforms:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawPlatforms }

PROCEDURE CheckLineConnectionsAreOK;
{ To check each line has the requisite connection data, as locking depends on this }
VAR
  L : Integer;

BEGIN
  TRY
    Log('G CHECKING LINE CONNECTIONS ARE OK');

    FOR L := 0 TO High(Lines) DO BEGIN
      WITH Lines[L] DO BEGIN
        IF (Line_NextUpLine = UnknownLine)
        AND (Line_NextUpPoint = UnknownPoint)
        AND (Line_NextUpIsEndOfLine = NotEndOfLine)
        THEN BEGIN
          Log('X No line continuation data for up of line ' + LineToStr(L));
          IF MessageDialogueWithDefault('No line continuation data for up of line ' + LineToStr(L),
                                        StopTimer, mtWarning, [mbOK, mbAbort], ['&Continue', '&Exit'], mbOK) = mrAbort
          THEN
            ShutDownProgram(UnitRef, 'CheckLInesAreOK');
        END;
        IF (Line_NextDownLine = UnknownLine)
        AND (Line_NextDownPoint = UnknownPoint)
        AND (Line_NextDownIsEndOfLine = NotEndOfLine)
        THEN BEGIN
          Log('X No line continuation data for down of line ' + LineToStr(L));
          IF MessageDialogueWithDefault('No line continuation data for down of line ' + LineToStr(L),
                                        StopTimer, mtWarning, [mbOK, mbAbort], ['&Continue', '&Exit'], mbOK) = mrAbort
          THEN
            ShutDownProgram(UnitRef, 'CheckLinesAreOK');
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG CheckLinesAreOK:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CheckLinesAreOK }

FUNCTION GetBufferStopDirection(BufferStopNum : Integer) : DirectionType;
{ Return a buffer stop's direction }
BEGIN
  Result := BufferStops[BufferStopNum].BufferStop_Direction;
END; { GetBufferStopDirection }

PROCEDURE DrawTRSPlunger(Location : Integer; Pressed : Boolean);
{ Indicate on a platform that a train-ready-to-start plunger has been pressed }
VAR
  X, Y : Integer;

BEGIN
  TRY
    InitialiseScreenDrawingVariables;

    WITH MainWindow.Canvas DO BEGIN
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
          DrawRectangularOutline(TRSPlunger_MouseRect, clYellow, NOT UndrawRequired, NOT UndrawToBeAutomatic);
      END; {WITH}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DrawTRSPlunger:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DrawTRSPlunger }

PROCEDURE InitialiseLocationLines;
{ Now we have set up the lines and signals, we can work out how they relate to locations. Some long locations (UFY for instance) will have multiple lines attached *** }
VAR
  L : Integer;
  LineAtDownStr : String;
  LineAtUpStr : String;
  Location : Integer;
  SaveDownX : Integer;
  SaveUpX : Integer;

BEGIN
  TRY
    FOR Location := 0 TO High(Locations) DO BEGIN
      WITH Locations[Location] DO BEGIN
        Location_LineAtUp := UnknownLine;
        Location_LineAtDown := UnknownLine;
        SaveUpX := MainWindow.ClientWidth;
        SaveDownX := 0;
        { Set up L to contain the lines at the ends of locations - if there is more than one line related to a location, use the one nearest to up or down }
        FOR L := 0 TO High(Lines) DO BEGIN
          WITH Lines[L] DO BEGIN
            IF Lines[L].Line_Location = Location THEN BEGIN
              IF ((GetLineAdjacentSignal(L) <> UnknownSignal)
                  AND (Signals[GetLineAdjacentSignal(L)].Signal_Direction = Up)
                  AND NOT Signals[GetLineAdjacentSignal(L)].Signal_OutOfUse)
              AND (Lines[L].Line_UpX < SaveUpX)
              THEN BEGIN
                Location_LineAtUp := L;
                SaveUpX := Lines[L].Line_UpX;
              END ELSE
                IF ((Lines[L].Line_AdjacentBufferStop <> UnknownBufferStop)
                    AND (GetBufferStopDirection(Lines[L].Line_AdjacentBufferStop) = Up))
                AND (BufferStops[Lines[L].Line_AdjacentBufferStop].BufferStop_X <= SaveUpX)
                THEN
                  Location_LineAtUp := L
                ELSE
                  IF ((GetLineAdjacentSignal(L) <> UnknownSignal)
                      AND (Signals[GetLineAdjacentSignal(L)].Signal_Direction = Down)
                      AND NOT Signals[GetLineAdjacentSignal(L)].Signal_OutOfUse)
                  AND (Lines[L].Line_DownX > SaveDownX)
                  THEN BEGIN
                    Location_LineAtDown := L;
                    SaveDownX := Lines[L].Line_DownX;
                  END ELSE
                    IF ((Lines[L].Line_AdjacentBufferStop <> UnknownBufferStop)
                    { remember that BufferStops array is dynamic and starts at zero }
                        AND (GetBufferStopDirection(Lines[L].Line_AdjacentBufferStop) = Down))
                    AND (BufferStops[Lines[L].Line_AdjacentBufferStop].BufferStop_X >= SaveDownX)
                    THEN
                      Location_LineAtDown := L;

            END; {WITH}
          END;
        END;

        { Now record whether platforms have other platforms up or down from them - this is needed as otherwise, for example, the routeing will attempt to route up trains
          from the down lines into the furthest up platform (PlatformA) even if the adjacent and nearer platform (PlatformB) is already occupied.
        }
        Location_PlatformOrFiddleyardAtUp := UnknownLocation;
        Location_PlatformOrFiddleyardAtDown := UnknownLocation;

        IF Location_IsPlatform THEN BEGIN
          IF Location_LineAtUp = UnknownLine THEN BEGIN
            LineAtUpStr := 'Unknown Line';
            Location_PlatformOrFiddleyardAtUp := UnknownLocation;
            IF Location_DirectionPriority <> DownOnly THEN
              Log('E Line up of ' + LocationToStr(Location, ShortStringType) + ' is unknown (is there an adjacent signal missing?)');
          END ELSE BEGIN
            LineAtUpStr := LineToStr(Location_LineAtUp);
            IF Lines[Location_LineAtUp].Line_NextUpLine = UnknownLine THEN
              Location_PlatformOrFiddleyardAtUp := UnknownLocation
            ELSE
              IF (Lines[Location_LineAtUp].Line_NextUpLine <> UnknownLine)
              AND (Lines[Lines[Location_LineAtUp].Line_NextUpLine].Line_Location <> UnknownLocation)
              AND (Lines[Lines[Location_LineAtUp].Line_NextUpLine].Line_Location <> Location)
              AND (Locations[Lines[Lines[Location_LineAtUp].Line_NextUpLine].Line_Location].Location_IsPlatform)
              THEN
                Location_PlatformOrFiddleyardAtUp := Lines[Lines[Location_LineAtUp].Line_NextUpLine].Line_Location;
          END;
        END;

        IF Location_IsPlatform THEN BEGIN
          IF Location_LineAtDown = UnknownLine THEN BEGIN
            LineAtDownStr := 'Unknown Line';
            Location_PlatformOrFiddleyardAtDown := UnknownLocation;
            IF Location_DirectionPriority <> UpOnly THEN
              Log('E Line down of ' + LocationToStr(Location, ShortStringType) + ' is unknown (is there an adjacent signal missing?)');
          END ELSE BEGIN
            LineAtDownStr := LineToStr(Location_LineAtDown);
            IF Lines[Location_LineAtDown].Line_NextDownLine = UnknownLine THEN
              Location_PlatformOrFiddleyardAtDown := UnknownLocation
            ELSE
              IF (Lines[Location_LineAtDown].Line_NextDownLine <> UnknownLine)
              AND (Lines[Lines[Location_LineAtDown].Line_NextDownLine].Line_Location <> UnknownLocation)
              AND (Lines[Lines[Location_LineAtDown].Line_NextDownLine].Line_Location <> Location)
              AND (Locations[Lines[Lines[Location_LineAtDown].Line_NextDownLine].Line_Location].Location_IsPlatform)
              THEN
                Location_PlatformOrFiddleyardAtDown := Lines[Lines[Location_LineAtDown].Line_NextDownLine].Line_Location;
          END;
        END;

        { Now record if locations are cul-de-sacs }
        IF (Locations[Location].Location_LineAtUp <> UnknownLine)
        AND (Lines[Locations[Location].Location_LineAtUp].Line_NextUpIsEndOfLine <> NotEndOfLine)
        THEN
          Locations[Location].Location_LineAtUpIsEndOfLine := True
        ELSE
          Locations[Location].Location_LineAtUpIsEndOfLine := False;

        IF (Locations[Location].Location_LineAtDown <> UnknownLine)
        AND (Lines[Locations[Location].Location_LineAtDown].Line_NextDownIsEndOfLine <> NotEndOfLine)
        THEN
          Locations[Location].Location_LineAtDownIsEndOfLine := True
        ELSE
          Locations[Location].Location_LineAtDownIsEndOfLine := False;

        IF DebuggingMode THEN
          Log('* Location: ' + LocationToStr(Location, ShortStringType)
                 + IfThen(Location_LineAtUp <> UnknownLine,
                          IfThen(Locations[Location].Location_LineAtUpIsEndOfLine,
                                 ' - End of line at Up',
                                 ' - Line at Up is ' + Lines[Location_LineAtUp].Line_Str))
                 + IfThen(Location_LineAtUp <> UnknownLine,
                          IfThen(Locations[Location].Location_LineAtDownIsEndOfLine,
                                 ' - End of line at Down',
                                 ' - Line at Down is ' + Lines[Location_LineAtDown].Line_Str))
                 + IfThen(Location_PlatformOrFiddleyardAtUp <> UnknownLocation,
                          ' - Location at Up is ' + LocationToStr(Location_PlatformOrFiddleyardAtUp))
                 + IfThen(Location_PlatformOrFiddleyardAtDown <> UnknownLocation,
                          ' - Location at Down is ' + LocationToStr(Location_PlatformOrFiddleyardAtDown)));
      END; {WITH}
    END; {FOR}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG InitialiseLocationLines: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { InitialiseLocationLines }

PROCEDURE TMainWindow.MainWindowPaint(Sender: TObject);
BEGIN
  IF MainWindow.MainTimer.Enabled THEN
    DrawMap;
END; { MainWindowPaint }

PROCEDURE TMainWindow.MainWindowExitClick(Sender: TObject);
CONST
  ExitProgram = True;

BEGIN
  Log('G Shutdown requested by user selecting exit menu item');
  ShutDownProgram(UnitRef, 'MainWindowExitClick');
END; { MainWindowExitClick }

PROCEDURE TMainWindow.MainWindowShortCut(VAR Msg: TWMKey; VAR Handled: Boolean);
VAR
  ShiftState : TShiftState;
  OK : Boolean;

BEGIN
  TRY
    ShiftState := [];
    IF GetKeyState(vk_Shift) < 0 THEN
      ShiftState := [ssShift];
    IF GetKeyState(vk_Control) < 0 THEN
      ShiftState := ShiftState + [ssCtrl];
    IF GetKeyState(vk_Menu) < 0 THEN
      ShiftState := ShiftState + [ssAlt];

    CASE Msg.CharCode OF
      vk_Tab:
        BEGIN
          Handled := True;
          KeyPressedDown(msg.Charcode, ShiftState);
        END;
      vk_Up: { up arrow key - need to handle specially in loco dialogue boxes }
        IF LocoDialogueWindow.Visible
        AND LocoDialogueWindow.LocoDialogueUpButton.Enabled
        THEN BEGIN
          LocoDialogueIncreaseSpeed;
          Handled := True;
        END;
      vk_Down: { down arrow key - need to handle specially in loco dialogue boxes }
        IF LocoDialogueWindow.Visible
        AND LocoDialogueWindow.LocoDialogueDownButton.Enabled
        THEN BEGIN
          LocoDialogueDecreaseSpeed;
          Handled := True;
        END;
      vk_Return: { enter key - need to handle specially in loco dialogue boxes }
        IF LocoDialogueWindow.LocoDialogueLocoMaskEdit.Focused THEN BEGIN
          LocoDialogueChangeOrSelectLoco;
          Handled := True;
        END;
      vk_Space: { space bar - need to handle specially in loco dialogue boxes }
        IF LocoDialogueWindow.Visible THEN BEGIN
          IF ssShift IN ShiftState THEN BEGIN
            IF MessageDialogueWithDefault('Resume operations?', NOT StopTimer, mtConfirmation, [mbOK, mbAbort],
                                          ['&Resume', '&Don''t resume'], mbAbort) = mrOK
            THEN BEGIN
              ResumeOperations(OK);
              IF OK THEN
                Log('GG Operations resumed')
              ELSE
                Log('GG Operations not resumed');
              InvalidateScreen(UnitRef, 'ApplicationMessage');
              Handled := True;
            END;
          END ELSE BEGIN
            StopOperations;
            Log('GG ' + DescribeKey(Msg.Charcode, ShiftState) + ': all operations stopped');
            Handled := True;
          END;
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
      Log('EG MainWindowShortCut:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { MainWindowShortCut }

PROCEDURE TMainWindow.ApplicationMessage(VAR Msg: TMsg; VAR Handled: Boolean);
{ Intercept messages - only way of getting at the tab key! Now replaced by ShortCut above Sept 2009 }
//VAR
//  Button: TUDBtnType;
//  OK : Boolean;

BEGIN
  CASE Msg.Message OF
    WM_KEYDOWN:
//      { The WM_KEYDOWN message is posted to the window with the keyboard focus when a nonsystem key is pressed. A nonsystem key is a key that is pressed when the Alt key
//        is not pressed. Used by FWP to capture any key apart from the Alt key and the F10 key.
//      }
      CASE Msg.wParam OF
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
//          IF LocoDialogueWindow.Visible
//          AND LocoDialogueWindow.LocoDialogueSpeedButtons.Enabled
//          THEN BEGIN
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
        vk_Space: { space bar - need to handle specially in loco dialogue boxes }
          debug;
//          IF LocoDialogueWindow.Visible THEN BEGIN
//            IF ssShift IN ApplicationMessageShiftState THEN BEGIN
//              IF MessageDialogueWithDefault('Resume operations?', NOT StopTimer, mtConfirmation, [mbOK, mbAbort],
//                                            ['&Resume', '&Don''t resume'], mbAbort) = mrOK
//              THEN BEGIN
//                ResumeOperations(OK);
//                IF OK THEN
//                  Log('GG Operations resumed')
//                ELSE
//                  Log('GG Operations not resumed');
//                InvalidateScreen(UnitRef, 'ApplicationMessage');
//                ApplicationMessageShiftState := [];
//                Handled := True;
//              END;
//            END ELSE BEGIN
//              StopOperations;
//              Log('GG ' + DescribeKey(Msg.wParam, ApplicationMessageShiftState) + ': all operations stopped');
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
      END; {CASE}
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
//      { The WM_KEYUP message is posted to the window with the keyboard focus when a nonsystem key is released. A nonsystem key is a key that is pressed when the ALT key
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
  END; {CASE}
END; { ApplicationMessage }

PROCEDURE TMainWindow.MainWindowMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
{ If the mouse moves into the main window, move the focus there, except from the Debug Window. (We can't use Activate as the main window remains activated until we click
  on the diagrams or debug windows).
}
BEGIN
  TRY
    IF NOT KeyboardAndMouseLocked
    AND (MainWindow <> NIL)
    THEN BEGIN
      IF NOT MainWindow.Active
      AND NOT (ClockWindow.Active
               OR ClockWindow.Visible)
      AND NOT (DebuggingOptionsWindow.Active
               OR DebuggingOptionsWindow.Visible)
      AND NOT (FeedbackWindow.Active
               OR FeedbackWindow.Visible)
      AND NOT (HelpWindow.Active
               OR HelpWindow.Visible)
      AND NOT (LocationDataWindow.Active
               OR LocationDataWindow.Visible)
      AND NOT (LockListWindow.Active
               OR LockListWindow.Visible)
      AND NOT (LocoUtilsWindow.Active
               OR LocoUtilsWindow.Visible)
      AND NOT (RailDriverWindow.Active
               OR RailDriverWindow.Visible)
      AND NOT (OptionsWindow.Active
               OR OptionsWindow.Visible)
      AND NOT TrackCircuitPopupMenuActive { a global variable, owing to the special nature of GeneralPopup menus which means one cannot normally detect whether they are
                                            "popped up" or not }
      THEN
        MainWindow.SetFocus;

      IF NOT ProgramStartup THEN
        WhatIsUnderMouse(X, Y, ShiftState);
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG MainWindowMouseMove:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { MainWindowMouseMove }

PROCEDURE TMainWindow.MainWindowMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  MouseButtonDown := True;

  { WhatIsUnderMouse needs to be repeated, as for some reason MouseButtonMove doesn't capture new mouse positions when a popup menu is "popped up" }
  WhatIsUnderMouse(X, Y, ShiftState);
  MouseButtonPressed(Button, X, Y, ShiftState);
END; { MainWindowMouseDown }

PROCEDURE TMainWindow.MainWindowMouseUp(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
BEGIN
  MouseButtonDown := False;
  MouseButtonReleased(Button, X, Y, ShiftState);
END; { MainWindowMouseUp }

PROCEDURE TMainWindow.MainWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
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
      IF (Key = vk_Escape)
      AND LockListWindow.Visible
      THEN
        LockListWindow.Hide
      ELSE
        IF (Key = vk_Return)
        AND LocoDialogueWindow.Visible AND NOT InAutoMode
        THEN
          { we don't want the clock to start if Enter is accidentally pressed }
          EmergencyStopInLocoDialogue
        ELSE
          KeyPressedDown(Key, ShiftState);
    END; {CASE}
  EXCEPT
    ON E : Exception DO
      Log('EG MainWindowKeyDown:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { MainWindowKeyDown }

PROCEDURE TMainWindow.MainWindowResize(Sender: TObject);
{ This is called after a window is resized }
BEGIN
  TRY
    IF MainWindowInitialised THEN BEGIN
      { Resize is called when we start up, so don't set ResizeMap then }
      ResizeMap := True;
        MainWindow.MainWindowStatusBar.Visible := True;

      IF (ScreenMode = DefaultWindowedScreenMode) OR (ScreenMode = CustomWindowedScreenMode) THEN BEGIN
        ScreenMode := CustomWindowedScreenMode;
        WriteToStatusBarPanel(StatusBarPanel2, 'Screen resized');
      END;
      InvalidateScreen(UnitRef, 'MainWindowResize');
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG MainWindowResize:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { MainWindowResize }

PROCEDURE TMainWindow.MainWindowClose(Sender: TObject; VAR Action: TCloseAction);
BEGIN
  TRY
    Log('G Shutdown requested by user clicking on exit button or pressing Alt F4');
    IF MessageDlg('Close FWP''s Railway Program?', mtConfirmation, [mbYes, mbNo], 0) = mrYes THEN BEGIN
      Action := caFree;
      ShutDownProgram(UnitRef, 'MainWindowClose');
    END ELSE BEGIN
      Log('G Shutdown request cancelled by user {BLANKLINEBEFORE}');
      Action := caNone;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG MainWindowClose:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { MainWindowClose }

PROCEDURE ChangeCursor(NewCursor : TCursor);
{ Change the shape of the cursor (from the Delphi Help system) }
BEGIN
  SaveCursor := Screen.Cursor;
  Screen.Cursor := NewCursor;
  Application.ProcessMessages;
END; { ChangeCursor }

PROCEDURE RestoreCursor;
{ Restores the shape of the cursor (from the Delphi Help system) - not in use }
BEGIN
  Screen.Cursor := crDefault; { Show default cursor }
END; { RestoreCursor }

PROCEDURE TMainWindow.HelpMenuAboutClick(Sender: TObject);
BEGIN
  MessageDlg(ProgramTitle
             + CRLF
             + 'Version ' + GetVersionInfoAsString + ' Build ' + GetBuildInfoAsString
             + CRLF
             + CRLF
             + CopyrightStatementForDisplaying
             + CRLF
             + 'All Rights Reserved'
             , mtInformation, [mbOK], 0);
END; { MainWindowAboutClick }

PROCEDURE TMainWindow.MainWindowDragDrop(Sender, Source: TObject; X, Y: Integer);
BEGIN
  IF Source IS TImage THEN
    Debug('drag drop x=' + inttostr(X) + ' y=' + inttostr(y));
  Debug(TImage(Source).Name);
END; { MainWindowDragDrop }

PROCEDURE TMainWindow.MainWindowDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; VAR Accept: Boolean);
BEGIN
  Accept := (Source IS TImage);
END; { MainWindowDragOver }

PROCEDURE TMainWindow.MainDisplayMenuDiagramsWindowClick(Sender: TObject);
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

PROCEDURE TMainWindow.MainDisplayMenuWorkingTimetableWindowClick(Sender: TObject);
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
PROCEDURE TMainWindow.MainDisplayMenuDebugClick(Sender: TObject);
BEGIN
  IF DebugWindow.Visible THEN BEGIN
    DebugWindow.Hide;
    MainDisplayMenuDebug.Checked := False;
  END ELSE BEGIN
    DebugWindow.Show;
    MainDisplayMenuDebug.Checked := True;
  END;
END; { MainDisplayMenuDebugClick }

PROCEDURE TMainWindow.LocoInfoMenuItemClick(Sender: TObject);
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

PROCEDURE TMainWindow.ShowStatusBarClick(Sender: TObject);
BEGIN
  IF MainWindow.MainWindowStatusBar.Visible THEN BEGIN
    MainWindow.MainWindowStatusBar.Hide;
    MainDisplayMenuShowStatusBar.Checked := False;
  END ELSE BEGIN
    MainWindow.MainWindowStatusBar.Show;
    MainDisplayMenuShowStatusBar.Checked := True;
  END;
END; { ShowStatusBarClick }

PROCEDURE TMainWindow.MainDisplayMenuShowClick(Sender: TObject);
{ Make menus visible if they're not and vice versa }
BEGIN
  IF MenusVisible THEN
    HideMenus
  ELSE
    ShowMenus;
END; { ShowMenuItemClick }

PROCEDURE TMainWindow.MainDisplayMenuZoomClick(Sender: TObject);
BEGIN
  MainDisplayMenuZoom.Checked := NOT MainDisplayMenuZoom.Checked;
  IF MainDisplayMenuZoom.Checked THEN
    ScreenMode := FullScreenMode
  ELSE BEGIN
    ScreenMode := DefaultWindowedScreenMode;
    InvalidateScreen(UnitRef, 'MainDisplayMenuZoomClick');
  END;
END; { MainDisplayMenuZoomClick }

PROCEDURE TMainWindow.MainRunMenuResumeOperationsClick(Sender: TObject);
VAR
  OK : Boolean;
  
BEGIN
  ResumeOperations(OK);
  IF OK THEN
    Log('GG Operations resumed')
  ELSE
    Log('GG Operations not resumed');
  InvalidateScreen(UnitRef, 'MainRunMenuResumeOperationsClick');
END; { MainRunMenuResumeOperationsClick }

PROCEDURE TMainWindow.MainHelpMenuRailHelpClick(Sender: TObject);
BEGIN
  Application.HelpCommand(HELP_FINDER, 0);
END; { MainHelpMenuRailHelpClick }

PROCEDURE InvalidateScreen(UnitRefParam, CallingStr : String);
{ Draw the screen by invalidating it }
BEGIN
  MainWindow.Invalidate;
//  Log('X Invalidate Screen - call ' + CallingStr + ' from Unit ' + UnitRefParam);
END; { InvalidateScreen }

PROCEDURE TMainWindow.FlashTimerTick(Sender: TObject);
{ Do any necessary flashing of signals or other on-screen detail }
CONST
  Bold = True;

VAR
  L : Integer;
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
            SetSignalFunction(NoLocoChip, S);
            InvalidateScreen(UnitRef, 'FlashTimerTick 1');
          END;
        END;
      END;

      { Also any lines set to flash }
      FOR L := 0 TO High(Lines) DO BEGIN
        IF Lines[L].Line_TC <> UnknownTC THEN BEGIN
          IF DisplayFlashingTrackCircuits
          AND (TrackCircuits[Lines[L].Line_TC].TC_Flashing)
          THEN BEGIN
            IF TrackCircuits[Lines[L].Line_TC].TC_LitUp THEN BEGIN
              TrackCircuits[Lines[L].Line_TC].TC_LitUp := False;
            END ELSE BEGIN
              TrackCircuits[Lines[L].Line_TC].TC_LitUp := True;
            END;
            InvalidateScreen(UnitRef, 'FlashTimerTick 2');
          END;
        END;
      END; {FOR }
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FlashTimerTick:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FlashTimerTick }

PROCEDURE CheckPointsAwaitingFeedback;
{ See if any point changes are pending - i.e. we're waiting for feedback that confirms the change }
CONST
  ErrorMessageRequired = True;

VAR
  DebugStr : String;
  LocoChipStr : String;
  OK : Boolean;
  P : Integer;
  PointFeedbackWaitInSeconds : Double;
  PointResultPending : Boolean;
  RouteFound : Boolean;
  Route : Integer;
  SubRoute : Integer;
  T : Train;
  TempDivergingLineStr : String;

BEGIN
  TRY
    IF NOT ProgramStartup THEN BEGIN
      FOR P := 0 TO High(Points) DO BEGIN
        WITH Points[P] DO BEGIN
          LocoChipStr := LocoChipToStr(Point_RouteLockedByLocoChip);
          IF Point_FeedbackPending THEN BEGIN
            IF Point_PresentState = Point_RequiredState THEN BEGIN
              IF Point_ManualOperation THEN BEGIN
                Point_FeedbackPending := False;
                Point_AwaitingManualChange := False;
                Log(LocoChipStr + ' P P=' + IntToStr(P) + ' pending change to ' + PointStateToStr(Point_RequiredState) + ' was successful');
                IF FWPShowMessageWindow.Visible THEN
                  FWPShowMessageWindow.Visible := False;
              END ELSE BEGIN
                { No need to draw it, as the change will have made the system draw it }
                DebugStr := 'P=' + IntToStr(P) + ' pending change to ';
                DebugStr := DebugStr + PointStateToStr(Point_RequiredState) + ' was successful after a '
                                     + FloatToStr(MilliSecondsBetween(Time, Point_FeedbackStartTime) / 1000) + ' second wait';
                Log(LocoChipStr + ' P ' + DebugStr);
                Point_FeedbackPending := False;
                Point_FeedbackPendingMsgWritten := False;
              END;

              IF Point_RequiredState = Straight THEN
                Log(LocoChipStr + ' P P=' + IntToStr(P) + ' D=S')
              ELSE
                Log(LocoChipStr + ' P P=' + IntToStr(P) + ' D=D');

            END ELSE BEGIN
              IF NOT Point_ManualOperation THEN BEGIN
                { PresentState <> RequiredState: a five second wait should be sufficient }
                PointFeedbackWaitInSeconds := Round(SecondSpan(Time, Point_FeedbackStartTime));
                IF PointFeedbackWaitInSeconds >= PointFeedbackMaximumWaitInSeconds THEN BEGIN
                  DebugStr := DebugStr + 'P=' + IntToStr(P) + ' pending change to ' + PointStateToStr(Point_RequiredState)
                                                              + ' failed after a ' + FloatToStr(PointFeedbackWaitInSeconds) + ' second wait';
                  Point_FeedbackPending := False;
                  Point_FeedbackPendingMsgWritten := False;
                  Log(LocoChipStr + ' P ' + DebugStr);

                  IF NOT Point_SecondAttempt THEN BEGIN
                    { have one more attempt at making it switch }
                    Point_SecondAttempt := True;
                    Log(LocoChipStr + ' P Second attempt to switch P=' + IntToStr(P));
                    PullPoint(P, Point_RouteLockedByLocoChip, NoRoute, NoSubRoute, NOT ForcePoint, ByUser, NOT ErrorMessageRequired, PointResultPending, DebugStr, OK);
                    Exit;
                  END;

                  { Illustrate where the offending turnout is; note: blue is the new yellow here (!) - as clBlue comes out as pmNotXored clYellow }
                  DrawRectangularOutline(Point_MouseRect, clBlue, UndrawRequired, NOT UndrawToBeAutomatic);
                  Point_MaybeBeingSetToManual := True;

                  IF (Points[P].Point_Type <> CatchPointUp) AND (Points[P].Point_Type <> CatchPointDown) THEN
                    { we need to do this as an IfThen clause seems to evaluate the whole expression and we therefore get a range check error when a catch point does not
                      have a diverging line
                    }
                    TempDivergingLineStr := ' ' + Lines[Points[P].Point_DivergingLine].Line_Str
                  ELSE
                    TempDivergingLineStr := '';

                  { and produce a pop up message for the user, as this is important }
                  MakeSound(1);
                  CASE MessageDialogueWithDefault('P=' + IntToStr(P) + ' [Lenz ' + IntToStr(Points[P].Point_LenzNum)
                                                  + '] ('
                                                  + Lines[Points[P].Point_HeelLine].Line_Str
                                                  + ' ' + Lines[Points[P].Point_StraightLine].Line_Str
                                                  + TempDivergingLineStr
                                                  + ') has failed to change to ' + PointStateToStr(Point_RequiredState) + ':' + CRLF
                                                  + 'Retry, or set this point manually to ' + PointStateToStr(Point_RequiredState)
                                                  + ' and press ''Now Set'' when completed, or Retry or Ignore the problem',
                                                  NOT StopTimer, mtWarning, [mbAbort, mbOK, mbRetry],
                                                  ['&Now Set', '&Ignore', '&Retry'], mbAbort)
                  OF
                    mrOK: { Point Set }
                      BEGIN
                        Log(LocoChipStr + ' P P=' + IntToStr(P) + ' manually set to ' + PointStateToStr(Point_RequiredState));
                        Point_PreviousState := Point_PresentState;
                        Point_PresentState := Point_RequiredState;
                        InvalidateScreen(UnitRef, 'CheckPointsAwaitingFeedback');
                        IF MessageDialogueWithDefault('Set this point to manual for the rest of this session?',
                                                      NOT StopTimer, mtWarning, [mbYes, mbNo], mbYes) = mrYes
                        THEN BEGIN
                          Point_ManualOperation := True;
                          Log('P P=' + IntToStr(P) + ' set to manual operation');
                        END;
                      END;
                    mrRetry:
                      BEGIN
                        { Try forcing the point to switch the other way first - sometimes unsticks a point }
                        IF Point_RequiredState = Diverging THEN
                          Point_RequiredState := Straight
                        ELSE
                          Point_RequiredState := Diverging;
                        PullPoint(P, Point_RouteLockedByLocoChip, NoRoute, NoSubRoute, ForcePoint, ByUser, NOT ErrorMessageRequired,
                                  PointResultPending, DebugStr, OK);

                        IF Point_RequiredState = Diverging THEN
                          Point_RequiredState := Straight
                        ELSE
                          Point_RequiredState := Diverging;
                        PullPoint(P, Point_RouteLockedByLocoChip, NoRoute, NoSubRoute, NOT ForcePoint, ByUser, NOT ErrorMessageRequired,
                                  PointResultPending, DebugStr, OK);
                      END;
                    mrAbort: { Ignore }
                      { a major problem: cancel the route, and the train }
                      BEGIN
                        Route := 0;
                        RouteFound := False;
                        WHILE (Route <= High(Routes_Routes))
                        AND NOT RouteFound
                        DO BEGIN
                          IF Routes_PointResultPendingPoint[Route] <> UnknownPoint THEN BEGIN
                            Log(LocoChipToStr(Routes_LocoChips[Route]) + ' R P=' + IntToStr(Routes_PointResultPendingPoint[Route])
                                                                       + ' pending point change and R=' + IntToStr(Route)
                                                                       + ' setting up cancelled by user');
                            Routes_RouteSettingsInProgress[Route] := False;
                            Routes_RouteClearingsInProgress[Route] := True;
                            FOR SubRoute := 0 TO High(Routes_SubRouteStates[Route]) DO BEGIN
                              CreateClearingSubRouteArray(Route, SubRoute);
                              Routes_SubRouteStates[Route, SubRoute] := SubRouteToBeCleared;
                            END;
                            IF MessageDialogueWithDefault('R=' + IntToStr(Route) + ' has been cancelled as a result of the failure'
                                                          + ' of point ' + IntToStr(Routes_PointResultPendingPoint[Route]) + ':'
                                                          + CRLF
                                                          + 'Do you want to cancel this train?',
                                                          NOT StopTimer, mtError, [mbYes, mbNo], ['&Cancel', '&Don''t Cancel'], mbYes)
                                                          = mrNo
                            THEN BEGIN

                            { may want to set the route by hand *** }

                            END ELSE BEGIN
                              IF Routes_LocoChips[Route] <> UnknownLocoChip THEN BEGIN
                                Log(LocoChipToStr(Routes_LocoChips[Route]) + ' DG System occupation and diagram entry cancelled by user');
                                { look for our train }
                                T := GetTrainRecord(Routes_LocoChips[Route]);
                                IF T <> NIL THEN
                                  CancelTrain(T, ByUser, TrainExists);
                              END;
                            END;
                            Routes_PointResultPendingPoint[Route] := UnknownPoint;
                            RouteFound := True;
                          END;
                          Inc(Route);
                        END; {WHILE}
                      END;
                  END; {CASE}
                  Point_MaybeBeingSetToManual := False;
                END;
              END;
            END;
          END;
        END; {WITH}
      END; {FOR}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG CheckPointsAwaitingFeedback:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CheckPointsAwaitingFeedback }

PROCEDURE TMainWindow.GeneralPopupMenuOnPopup(Sender: TObject);
BEGIN
  IF (MainWindow.Top <> DefaultMainWindowTop)
  OR (MainWindow.Height <> DefaultMainWindowHeight)
  OR (MainWindow.Left <> DefaultMainWindowLeft)
  OR (MainWindow.Top <> DefaultMainWindowTop)
  OR (MainWindow.Width <> DefaultMainWindowWidth)
  THEN
    GeneralPopupResetMainWindowSizeAndPosition.Enabled := True
  ELSE
    GeneralPopupResetMainWindowSizeAndPosition.Enabled := False;
END; { MainWindowPopupMenuOnPopup }

PROCEDURE TMainWindow.CreateOrDeleteItemMenuOnPopup(Sender: TObject);
VAR
  P : Integer;
  S : Integer;
  LineFound : Boolean;

BEGIN
  IF SignalPopupNum = UnknownSignal THEN BEGIN
    IF LinePopupNum <> UnknownLine THEN BEGIN
      { only create a signal next to a line, and then only if there isn't one already attached to it }
      S := 0;
      LineFound := False;
      WHILE S <= High(Signals) DO BEGIN
        IF Signals[S].Signal_AdjacentLine = LinePopupNum THEN
          LineFound := True;
        Inc(S);
      END; {WHILE}
      IF LineFound THEN
        CreateSignalMenuItem.Enabled := False
      ELSE
        CreateSignalMenuItem.Enabled := True;
    END;

    DeleteSignalMenuItem.Enabled := False;
    DeleteSignalMenuItem.Caption := 'Delete Signal';
  END ELSE BEGIN
    CreateSignalMenuItem.Enabled := False;
    DeleteSignalMenuItem.Enabled := True;
    DeleteSignalMenuItem.Caption := 'Delete Signal ' + IntToStr(SignalPopupNum)
  END;

  IF PointPopupNum = UnknownPoint THEN BEGIN
    IF LinePopupNum <> UnknownLine THEN
      { only create a point next to a line }
      CreatePointMenuItem.Enabled := True;
    DeletePointMenuItem.Enabled := False;
    DeletePointMenuItem.Caption := 'Delete Point';
  END ELSE BEGIN
    CreatePointMenuItem.Enabled := False;
    DeletePointMenuItem.Enabled := True;
    DeletePointMenuItem.Caption := 'Delete Point ' + IntToStr(PointPopupNum)
  END;

  IF LinePopupNum = UnknownLine THEN BEGIN
    CreateLineMenuItem.Enabled := True;
    DeleteLineMenuItem.Enabled := False;
    DeleteLineMenuItem.Caption := 'Delete Line';
  END ELSE BEGIN
    CreateLineMenuItem.Enabled := False;

    { but we can't delete a line if there's a signal or point attached to it }
    S := 0;
    LineFound := False;
    WHILE S <= High(Signals) DO BEGIN
      IF Signals[S].Signal_AdjacentLine = LinePopupNum THEN
        LineFound := True;
      Inc(S);
    END; {WHILE}

    IF NOT LineFound THEN BEGIN
      P := 0;
      LineFound := False;
      WHILE P <= High(Points) DO BEGIN
        IF Points[P].Point_DivergingLine = LinePopupNum THEN
          LineFound := True;
        IF Points[P].Point_StraightLine = LinePopupNum THEN
          LineFound := True;
        IF Points[P].Point_HeelLine = LinePopupNum THEN
          LineFound := True;
        Inc(P);
      END; {WHILE}
    END;

    IF LineFound THEN
      DeleteLineMenuItem.Enabled := False
    ELSE
      DeleteLineMenuItem.Enabled := True;
    DeleteLineMenuItem.Caption := 'Delete Line ' + LineToStr(LinePopupNum)
  END;
END; { CreateOrDeleteItemMenuOnPopup }

PROCEDURE TMainWindow.CreateSignalMenuItemClick(Sender: TObject);
BEGIN
  CreateSignal;
END; { CreateSignalMenuItemClick }

PROCEDURE TMainWindow.DeleteSignalMenuItemClick(Sender: TObject);
BEGIN
  DeleteSignal(SignalPopupNum);
END; { DeleteSignalMenuItemClick }

PROCEDURE TMainWindow.CreatePointMenuItemClick(Sender: TObject);
BEGIN
  CreatePoint;
END; { CreatePointMenuItemClick }

PROCEDURE TMainWindow.DeletePointMenuItemClick(Sender: TObject);
BEGIN
  DeletePoint(PointPopupNum);
END; { DeletePointMenuItemClick }

PROCEDURE TMainWindow.CreateLineMenuItemClick(Sender: TObject);
BEGIN
  IF LinePopupNum = UnknownLine THEN
    CreateLineMenuItem.Enabled := True
  ELSE
    CreateLineMenuItem.Enabled := False;
END; { CreateLineMenuItemClick }

PROCEDURE TMainWindow.DeleteLineMenuItemClick(Sender: TObject);
BEGIN

END; { DeleteLineMenuItemClick }

PROCEDURE TMainWindow.SignalPopupMenuOnPopup(Sender: TObject);
BEGIN
  WITH Signals[SignalPopupNum] DO BEGIN
    IF SignalPopupNum = UnknownSignal THEN BEGIN
      SignalPopupSignalNum.Caption := '';
      SignalPopupSignalNum.Enabled := False;
    END ELSE BEGIN
      SignalPopupSignalNum.Caption := 'Signal ' + IntToStr(SignalPopupNum);
      SignalPopupSignalNum.Enabled := True;
    END;

    IF Signal_OutOfUse THEN BEGIN
      SignalPopupSetSignalOutOfUse.Enabled := False;
      SignalPopupSetSignalBackInUse.Enabled := True;
    END ELSE BEGIN
      SignalPopupSetSignalOutOfUse.Enabled := True;
      SignalPopupSetSignalBackInUse.Enabled := False;
    END;
  END; {WITH}
END; { SignalPopupMenuOnPopup }

PROCEDURE TMainWindow.SignalPopupSetSignalOutOfUseClick(Sender: TObject);
BEGIN
  WITH Signals[SignalPopupNum] DO BEGIN
    IF NOT Signal_OutOfUse THEN BEGIN
      Signal_OutOfUse := True;
      Signal_DataChanged := True;
      InvalidateScreen(UnitRef, 'SignalPopupSetSignalOutOfUseClick');
    END;
  END; {WITH}
END; { SignalPopupSetSignalOutOfUseClick }

PROCEDURE TMainWindow.SignalPopupSetSignalBackInUseClick(Sender: TObject);
BEGIN
  WITH Signals[SignalPopupNum] DO BEGIN
    IF Signal_OutOfUse THEN BEGIN
      Signal_OutOfUse := False;
      Signal_DataChanged := True;
      Signal_Aspect := RedAspect;
      InvalidateScreen(UnitRef, 'SignalPopupSetSignalBackInUseClick');
    END;
  END; {WITH}
END; { SignalPopupSetSignalBackInUseClick }

PROCEDURE TMainWindow.SignalPopupEditSignalDetailsClick(Sender: TObject);
BEGIN
  TurnEditModeOn(SignalPopupNum, UnknownPoint);
END; { SignalPopupEditSignalDetailsClick }

PROCEDURE TMainWindow.BufferStopMenuOnPopup(Sender: TObject);
BEGIN
  WITH BufferStops[BufferStopPopupNum] DO BEGIN
    IF BufferStopPopupNum = UnknownBufferStop THEN BEGIN
      BufferStopPopupBufferStopNum.Caption := '';
      BufferStopPopupBufferStopNum.Enabled := False;
    END ELSE BEGIN
      BufferStopPopupBufferStopNum.Caption := 'BufferStop ' + IntToStr(BufferStopPopupNum);
      BufferStopPopupBufferStopNum.Enabled := True;
    END;
  END; {WITH}
END; { BufferStopMenuOnPopup }

PROCEDURE TMainWindow.PointPopupMenuOnPopup(Sender: TObject);
BEGIN
  WITH Points[PointPopupNum] DO BEGIN
    IF PointPopupNum = Unknownpoint THEN BEGIN
      PointPopupPointNum.Caption := '';
      PointPopupPointNum.Enabled := False;
    END ELSE BEGIN
      PointPopupPointNum.Caption := 'Point ' + IntToStr(PointPopupNum);
      PointPopupPointNum.Enabled := True;
    END;

    IF Point_OutOfUse THEN BEGIN
      PointPopupSetPointOutOfUse.Enabled := False;
      PointPopupSetPointBackInUse.Enabled := True;
    END ELSE BEGIN
      PointPopupSetPointOutOfUse.Enabled := True;
      PointPopupSetPointBackInUse.Enabled := False;
    END;

    IF Point_ManualOperation THEN BEGIN
      PointPopupSetPointToManual.Enabled := False;
      PointPopupSetPointToAutomatic.Enabled := True;
    END ELSE BEGIN
      PointPopupSetPointToManual.Enabled := True;
      PointPopupSetPointToAutomatic.Enabled := False;
    END;

    IF Point_LockedByUser THEN
      PointPopupLockPoint.Caption := 'Unlock Point'
    ELSE
      PointPopupLockPoint.Caption := 'Lock Point';
  END; {WITH}
END; { PointPopupMenuOnPopup }

PROCEDURE TMainWindow.PointPopupEditPointDetailsClick(Sender: TObject);
BEGIN
  TurnEditModeOn(UnknownSignal, PointPopupNum);
END; { PointPopupEditPointDetailsClick }

PROCEDURE TMainWindow.PointPopupLockPointClick(Sender: TObject);
BEGIN
  WITH Points[PointPopupNum] DO BEGIN
    IF Point_LockedByUser THEN BEGIN
      Point_LockedByUser := False;
      PointPopupLockPoint.Caption := 'Lock Point';
    END ELSE BEGIN
      Point_LockedByUser := True;
      PointPopupLockPoint.Caption := 'Unlock Point';
    END;
    InvalidateScreen(UnitRef, 'PointPopupLockPointClick');
  END; {WITH}
END; { PointPopupLockPointClick }

PROCEDURE TMainWindow.PointPopupSetPointOutOfUseClick(Sender: TObject);
BEGIN
  WITH Points[PointPopupNum] DO BEGIN
    IF NOT Point_OutOfUse THEN BEGIN
      Point_OutOfUse := True;
      Point_DataChanged := True;
      InvalidateScreen(UnitRef, 'PointPopupSetPointOutOfUseClick');
    END;
  END; {WITH}
END; { PointPopupSetPointOutOfUseClick }

PROCEDURE TMainWindow.PointPopupSetPointBackInUseClick(Sender: TObject);
BEGIN
  WITH Points[PointPopupNum] DO BEGIN
    IF Point_OutOfUse THEN BEGIN
      Point_OutOfUse := False;
      Point_DataChanged := True;
      InvalidateScreen(UnitRef, 'PointPopupSetPointBackInUseClick');
    END;
  END; {WITH}
END; { PointPopupSetPointBackInUseClick }

PROCEDURE TMainWindow.PointPopupSetPointToManualClick(Sender: TObject);
BEGIN
  WITH Points[PointPopupNum] DO BEGIN
    IF NOT Point_ManualOperation THEN BEGIN
      Point_ManualOperation := True;
      InvalidateScreen(UnitRef, 'PointPopupSetPointToManualClick');
    END;
  END; {WITH}
END; { PointPopupSetPointToManualClick }

PROCEDURE TMainWindow.GeneralPopupChangePointLockedColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := ShowPointLockedColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    ShowPointLockedColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointLockedColourClick');
  END;
END; { GeneralPopupChangePointLockedColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointLockedColourClick(Sender: TObject);
BEGIN
  ShowPointLockedColour := DefaultShowPointLockedColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointLockedColourClick');
END; { GeneralPopupRestorePointLockedColourClick }

PROCEDURE TMainWindow.PointPopupSetPointToAutomaticClick(Sender: TObject);
BEGIN
  WITH Points[PointPopupNum] DO BEGIN
    IF Point_ManualOperation THEN BEGIN
      Point_ManualOperation := False;
      InvalidateScreen(UnitRef, 'PointPopupSetPointToAutomaticClick');
    END;
  END; {WITH}
END; { PointPopupSetPointToAutomaticClick }

PROCEDURE TMainWindow.TCPopupMenuOnPopup(Sender: TObject);
BEGIN
  TRY
    WITH Lines[TrackCircuitPopupLine] DO BEGIN
      IF Line_TC = UnknownTC THEN BEGIN
        TCPopupTrackCircuitNumber.Caption := 'Track Circuit Number';
        TCPopupTrackCircuitNumber.Enabled := False;
        TCPopupAllocateLocoToTrackCircuit.Enabled := False;
        TCPopupClearLocoAllocationFromTrackCircuit.Enabled := False;
        TCPopupClearTrackCircuitSpeedRestriction.Enabled := False;
        TCPopupSetLineOutOfUse.Enabled := False;
        TCPopupSetLocationOutOfUse.Enabled := False;
        TCPopupSetTrackCircuitOutOfUseSetByUser.Enabled := False;
        TCPopupSetTrackCircuitToPermanentOccupation.Enabled := False;
        TCPopupSetTrackCircuitSpeedRestriction.Enabled := False;
        TCPopupSetTrackCircuitToSystemOccupation.Enabled := False;
        TCPopupSetTrackCircuitToFeedbackOccupation.Enabled := False;
        TCPopupSetTrackCircuitToUserDriving.Enabled := False;
        TCPopupSetTrackCircuitUnoccupied.Enabled := False;
        TCPopupChangeInternalLocoDirectionToUp.Enabled := False;
        TCPopupChangeInternalLocoDirectionToDown.Enabled := False;
        TCPopupChangeInternalLocoDirectionToUp.Caption := 'Change Internal Loco Direction to Up';
        TCPopupChangeInternalLocoDirectionToDown.caption := 'Change Internal Loco Direction to Down';
      END ELSE BEGIN
        TCPopupTrackCircuitNumber.Enabled := True;

        TCPopupSetTrackCircuitToUserDriving.Enabled := True;
        TCPopupSetTrackCircuitSpeedRestriction.Enabled := True;

        IF TrackCircuits[Line_TC].TC_OccupationState <> TCOutOfUseSetByUser THEN
          TCPopupSetTrackCircuitOutOfUseSetByUser.Enabled := True
        ELSE
          TCPopupSetTrackCircuitOutOfUseSetByUser.Enabled := False;

        IF (TrackCircuits[Line_TC].TC_OccupationState <> TCPermanentFeedbackOccupation)
        AND (TrackCircuits[Line_TC].TC_OccupationState <> TCPermanentOccupationSetByUser)
        THEN
          TCPopupSetTrackCircuitToPermanentOccupation.Enabled := True
        ELSE
          TCPopupSetTrackCircuitToPermanentOccupation.Enabled := False;

        IF TrackCircuits[Line_TC].TC_OccupationState <> TCUnoccupied THEN
          TCPopupSetTrackCircuitUnoccupied.Enabled := True
        ELSE
          TCPopupSetTrackCircuitUnoccupied.Enabled := False;

        IF TrackCircuits[Line_TC].TC_OccupationState <> TCFeedbackOccupation THEN
          TCPopupSetTrackCircuitToFeedbackOccupation.Enabled := True
        ELSE
          TCPopupSetTrackCircuitToFeedbackOccupation.Enabled := False;

        IF TrackCircuits[Line_TC].TC_OccupationState <> TCSystemOccupation THEN
          TCPopupSetTrackCircuitToSystemOccupation.Enabled := True
        ELSE
          TCPopupSetTrackCircuitToSystemOccupation.Enabled := False;

        TCPopupAllocateLocoToTrackCircuit.Enabled := True;

        IF TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip THEN BEGIN
          TCPopupTrackCircuitNumber.Caption := 'Track Circuit ' + IntToStr(Line_TC) + ' (' + LineToStr(TrackCircuitPopupLine) + ') '
                                               + LocationToStr(Lines[TrackCircuitPopupLine].Line_Location, ShortStringType);
          TCPopupChangeInternalLocoDirectionToUp.Enabled := False;
          TCPopupChangeInternalLocoDirectionToDown.Enabled := False;
          TCPopupChangeInternalLocoDirectionToUp.Caption := 'Change Loco''s Internal Direction to Up';
          TCPopupChangeInternalLocoDirectionToDown.caption := 'Change Loco''s Internal Direction to Down';
        END ELSE BEGIN
          TCPopupTrackCircuitNumber.Caption := 'Track Circuit ' + IntToStr(Line_TC) + ' (' + LineToStr(TrackCircuitPopupLine) + ') '
                                               + 'occupied by Loco ' + LocoChipToStr(TrackCircuits[Line_TC].TC_LocoChip);
          TCPopupClearLocoAllocationFromTrackCircuit.Enabled := True;

          TCPopupChangeInternalLocoDirectionToUp.Enabled := True;
          TCPopupChangeInternalLocoDirectionToUp.Caption := 'Change Internal Direction For Loco ' + LocoChipToStr(TrackCircuits[Line_TC].TC_LocoChip) + ' To Up';
          TCPopupChangeInternalLocoDirectionToDown.Enabled := True;
          TCPopupChangeInternalLocoDirectionToDown.caption := 'Change Internal Direction For Loco ' + LocoChipToStr(TrackCircuits[Line_TC].TC_LocoChip) + ' To Down';
        END;

        IF TrackCircuits[Line_TC].TC_UserMustDrive THEN
          TCPopupSetTrackCircuitToUserDriving.Caption := 'Set Track Circuit To Auto'
        ELSE
          TCPopupSetTrackCircuitToUserDriving.Caption := 'Set Track Circuit To User Must Drive';

        IF TrackCircuits[Line_TC].TC_SpeedRestrictionInMPH = NoSpecifiedSpeed THEN BEGIN
          TCPopupSetTrackCircuitSpeedRestriction.Caption := 'Set Track Circuit Speed Restriction';
          TCPopupClearTrackCircuitSpeedRestriction.Enabled := False;
        END ELSE BEGIN
          TCPopupSetTrackCircuitSpeedRestriction.Caption := 'Change Track Circuit Speed Restriction';
          TCPopupClearTrackCircuitSpeedRestriction.Enabled := True;
        END;

        IF (Line_Location = UnknownLocation) OR NOT Locations[Line_Location].Location_OutOfUse THEN BEGIN
          TCPopupSetLineOutOfUse.Enabled := True;
          IF Lines[TrackCircuitPopupLine].Line_OutOfUseState = OutOfUse THEN
            TCPopupSetLineOutOfUse.Caption := 'Return Line ''' + LineToStr(TrackCircuitPopupLine) + ''' to use'
          ELSE
            TCPopupSetLineOutOfUse.Caption := 'Put Line ''' + LineToStr(TrackCircuitPopupLine) + ''' out of use';

          IF Line_Location = UnknownLocation THEN BEGIN
            TCPopupSetLocationOutOfUse.Caption := 'Unknown Location';
            TCPopupSetLocationOutOfUse.Enabled := False;
          END ELSE BEGIN
            TCPopupSetLocationOutOfUse.Caption := 'Put Location ''' + LocationToStr(Lines[TrackCircuitPopupLine].Line_Location) + ''' out of use';
            TCPopupSetLocationOutOfUse.Enabled := True;
          END;
        END ELSE BEGIN
          TCPopupSetLocationOutOfUse.Enabled := True;
          TCPopupSetLocationOutOfUse.Caption := 'Return Location ''' + LocationToStr(Lines[TrackCircuitPopupLine].Line_Location) + ''' to use';
          TCPopupSetLineOutOfUse.Enabled := False;
        END;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG TCPopupMenuOnPopup:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TCPopupMenuOnPopup }

PROCEDURE TMainWindow.TCPopupSetTrackCircuitToSystemOccupationClick(Sender: TObject);
BEGIN
  WITH Lines[TrackCircuitPopupLine] DO BEGIN
    IF Line_TC <> UnknownTC THEN BEGIN
      SetTrackCircuitState(Line_TC, TCSystemOccupation);
      TCPopupSetTrackCircuitToSystemOccupation.Enabled := False;
      InvalidateScreen(UnitRef, 'TCPopupSetTrackCircuitToFeedbackOccupationClick');
    END;
  END; {WITH}
END; { TCPopupSetTrackCircuitToSystemOccupationClick }

PROCEDURE TMainWindow.TCPopupSetTrackCircuitToFeedbackOccupationClick(Sender: TObject);
BEGIN
  WITH Lines[TrackCircuitPopupLine] DO BEGIN
    IF Line_TC <> UnknownTC THEN BEGIN
      SetTrackCircuitState(Line_TC, TCFeedbackOccupation);
      TCPopupSetTrackCircuitToFeedbackOccupation.Enabled := False;
      InvalidateScreen(UnitRef, 'TCPopupSetTrackCircuitToFeedbackOccupationClick');
    END;
  END; {WITH}
END; { TCPopupSetTrackCircuitToFeedbackOccupationClick }

PROCEDURE TMainWindow.TCPopupSetTrackCircuitUnoccupiedClick(Sender: TObject);
BEGIN
  WITH Lines[TrackCircuitPopupLine] DO BEGIN
    IF Line_TC <> UnknownTC THEN BEGIN
      SetTrackCircuitState(Line_TC, TCUnoccupied);
      TCPopupSetTrackCircuitUnoccupied.Enabled := False;
      InvalidateScreen(UnitRef, 'TCPopupSetTrackCircuitOccupiedClick');
    END;
  END; {WITH}
END; { TCPopupSetTrackCircuitOccupiedClick }

PROCEDURE TMainWindow.TCPopupSetTrackCircuitOutOfUseSetByUserClick(Sender: TObject);
BEGIN
  { Set the trackcircuit out of use, regardless of its current state }
  WITH Lines[TrackCircuitPopupLine] DO BEGIN
    IF Line_TC <> UnknownTC THEN BEGIN
      SetTrackCircuitState(Line_TC, TCOutOfUseSetByUser);
      TCPopupSetTrackCircuitOutOfUseSetByUser.Enabled := False;
      InvalidateScreen(UnitRef, 'TCPopupSetTrackCircuitOutOfUseClick');
    END;
  END;
END; { TCPopupSetTrackCircuitOutOfUseClick }

PROCEDURE TMainWindow.TCPopupSetTrackCircuitToPermanentOccupationClick(Sender: TObject);
BEGIN
  { Set the trackcircuit permanently occupied, regardless of its current state }
  WITH Lines[TrackCircuitPopupLine] DO BEGIN
    IF Line_TC <> UnknownTC THEN BEGIN
      IF GetTrackCircuitState(Line_TC) = TCFeedbackOccupation THEN
        SetTrackCircuitState(Line_TC, TCPermanentFeedbackOccupation)
      ELSE
        SetTrackCircuitState(Line_TC, TCPermanentOccupationSetByUser);
      TCPopupSetTrackCircuitToPermanentOccupation.Enabled := False;
      InvalidateScreen(UnitRef, 'TCPopupSetTrackCircuitOutOfUseClick');
    END;
  END; {WITH}
END; { TCPopupSetTrackCircuitToPermanentOccupationClick }

PROCEDURE TMainWindow.TCPopupShowLocosLastErrorMessageClick(Sender: TObject);
VAR
  T : Train;

BEGIN
  WITH Lines[TrackCircuitPopupLine] DO BEGIN
    IF Line_TC <> UnknownTC THEN BEGIN
      T := GetTrainRecord(TrackCircuits[Line_TC].TC_LocoChip);
      IF T <> NIL THEN BEGIN
        WITH T^ DO BEGIN
          IF Train_LastRouteLockedMsgStr <> '' THEN
            Debug(Train_LocoChipStr + ': ' +  Train_LastRouteLockedMsgStr);
          IF Train_RouteCreationHoldMsg <> '' THEN
            Debug(Train_LocoChipStr + ': ' + Train_RouteCreationHoldMsg);
        END;
      END;
    END;
  END; {WITH}
END;
{ ShowLocosLastErrorMessageClick }

PROCEDURE TMainWindow.TCPopupTrackCircuitNumberClick(Sender: TObject);
BEGIN
  WITH Lines[TrackCircuitPopupLine] DO
    IF TrackCircuits[Line_TC].TC_LocoChip = UnknownLocoChip THEN
      Log('X User clicked on TC=' + IntToStr(Line_TC) + ' (' + LineToStr(TrackCircuitPopupLine) + ') '
                                                      + LocationToStr(Lines[TrackCircuitPopupLine].Line_Location, ShortStringType) + ' in TCPopup')
    ELSE
      Log('X User clicked on TC=' + IntToStr(Line_TC) + ' (' + LineToStr(TrackCircuitPopupLine) + ') '
                                                      + 'occupied by Loco ' + LocoChipToStr(TrackCircuits[Line_TC].TC_LocoChip) + ' in TCPopup');
END; { TCPopupTrackCircuitNumberClick }

PROCEDURE ClearLocoFromTrackCircuit(TC : Integer);
{ Clear a loco allocation from a given trackcircuit and other associated locations }
VAR
  I : Integer;
  T : Train;
  TCArray : IntegerArrayType;

BEGIN
  TRY
    { no loco chip number has been entered, so clear any existing one }
    T := GetTrainRecord(TrackCircuits[TC].TC_LocoChip);
    IF T <> NIL THEN BEGIN
      T^.Train_CurrentTC := UnknownTC;
      T^.Train_SavedLocation := UnknownLocation;

      { and clear it from other location trackcircuits except the one we're at (are we right to use Train_LastLocation here? **** ) }
      TCArray := GetTrackCircuitsForLocation(T^.Train_LastLocation);
      FOR I := 0 TO High(TCArray) DO BEGIN
        IF TCArray[I] <> TC THEN
          SetTrackCircuitState(T^.Train_LocoChip, TCArray[I], TCUnoccupied);
      END;
      Log('DG Loco chip number has been cleared from ' + LocationToStr(T^.Train_LastLocation));
      InvalidateScreen(UnitRef, 'ClearLocoFromTrackCircuit');
    END;

    TrackCircuits[TC].TC_LocoChip := UnknownLocoChip;
  EXCEPT
    ON E : Exception DO
      Log('EG ClearLocoFromTrackCircuit:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ClearLocoFromTrackCircuit }

PROCEDURE TMainWindow.TCPopupClearLocoAllocationFromTrackCircuitClick(Sender: TObject);
BEGIN
  WITH Lines[TrackCircuitPopupLine] DO BEGIN
    IF TrackCircuits[Line_TC].TC_LocoChip <> UnknownLocoChip THEN
      ClearLocoFromTrackCircuit(Line_TC);
  END;
END; { TCPopupClearLocoAllocationFromTrackCircuitClick }

PROCEDURE TMainWindow.TCPopupAllocateLocoToTrackCircuitClick(Sender: TObject);
{ Allocate (or remove) a loco chip number from a given trackcircuit }
CONST
  AllLocos = True;

VAR
  AdjacentUpTC, AdjacentDownTC : Integer;
  DebugStr : String;
  I : Integer;
  InputQueryLocoChipStr : String;
  NewTrackCircuitState : TrackCircuitStateType;
  PossibleLocoChip : Integer;
  PossibleT : Train;
  SavePossibleLocoChip : Integer;
  TC : Integer;
  TCArray : IntegerArrayType;

BEGIN
  TRY
    WITH Lines[TrackCircuitPopupLine] DO BEGIN
      { If we don't know which loco is on a particular line }
      IF Line_TC <> UnknownTC THEN BEGIN
        { save chip number previously allocated to this trackcircuit }
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
            PossibleT := GetTrainRecord(PossibleLocoChip, AllLocos);
            IF PossibleT = NIL THEN
              Debug('!Loco ' + LocoChipToStr(PossibleLocoChip) + ' is not in the loco table')
  //          ELSE
  //            IF NOT PossibleT^.Train_Active THEN
  //              Debug('!Loco ' + LocoChipToStr(PossibleLocoChip) + ' is in the loco table but is not active')
              ELSE BEGIN
                FindAdjoiningTrackCircuits(Line_TC, AdjacentUpTC, AdjacentDownTC);
                IF (PossibleT^.Train_CurrentTC <> UnknownTC)
                AND (PossibleT^.Train_CurrentTC <> Line_TC)
                AND (TrackCircuits[PossibleT^.Train_CurrentTC].TC_LocoChip <> UnknownLocoChip)
                AND (PossibleT^.Train_CurrentTC <> AdjacentUpTC)
                AND (PossibleT^.Train_CurrentTC <> AdjacentDownTC)
                AND (TrackCircuits[Line_TC].TC_OccupationState <> TCSystemOccupation)
                AND (MessageDialogueWithDefault('Loco ' + IntToStr(PossibleLocoChip) + ' is already recorded as being at TC=' + IntToStr(PossibleT^.Train_CurrentTC)
                                                + ': has it moved?',
                                                NOT StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo)
                THEN
                  ShowMessage('Loco ' + IntToStr(PossibleLocoChip) + ' not be allocated to TC=' + IntToStr(Line_TC))
                ELSE BEGIN
                  { otherwise change the allocation, unless it's a system allocation }
                  IF TrackCircuits[Line_TC].TC_OccupationState = TCSystemOccupation THEN BEGIN
                    TrackCircuits[Line_TC].TC_LocoChip := PossibleLocoChip;
                    InvalidateScreen(UnitRef, 'TCPopupAllocateLocoToTrackCircuitClick');
                    Log(LocoChipToStr(PossibleLocoChip) + ' T System allocated to TC=' + IntToStr(Line_TC) + ' by user');
                  END ELSE BEGIN
                    PossibleT^.Train_CurrentTC := Line_TC;

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
                        IF TrackCircuits[TC].TC_LocoChip = PossibleT^.Train_LocoChip THEN BEGIN
                          TrackCircuits[TC].TC_LocoChip := UnknownLocoChip;
                          IF NOT TrackCircuitStateIsPermanentlyOccupied(TrackCircuits[TC].TC_OccupationState) THEN
                            SetTrackCircuitState(TC, TCUnoccupied);
                        END;
                      END;
                    END; {FOR}

                    SetTrackCircuitState(PossibleLocoChip, Line_TC, NewTrackCircuitState);
                    SetLength(TCArray, 0);
                    { and any other adjacent trackcircuits in the same location, unless they're already occupied. First search up the line: }
                    REPEAT
                      TC := AdjacentUpTC;
                      IF TC <> UnknownTC THEN BEGIN
                        IF TrackCircuits[AdjacentUpTC].TC_OccupationState = TCFeedbackOccupation THEN BEGIN
                          SetTrackCircuitState(PossibleLocoChip, TC, NewTrackCircuitState);
                          AppendToIntegerArray(TCArray, TC);
                        END;
                        FindAdjoiningTrackCircuits(TC, AdjacentUpTC, AdjacentDownTC);
                      END;
                    UNTIL (AdjacentUpTC = UnknownTC) OR (TrackCircuits[AdjacentUpTC].TC_OccupationState = TCUnoccupied);

                    { and then down: }
                    FindAdjoiningTrackCircuits(Line_TC, AdjacentUpTC, AdjacentDownTC);
                    REPEAT
                      TC := AdjacentDownTC;
                      IF TC <> UnknownTC THEN BEGIN
                        IF TrackCircuits[AdjacentDownTC].TC_OccupationState = TCFeedbackOccupation THEN BEGIN
                          SetTrackCircuitState(PossibleLocoChip, TC, NewTrackCircuitState);
                          AppendToIntegerArray(TCArray, TC);
                        END;
                        FindAdjoiningTrackCircuits(TC, AdjacentUpTC, AdjacentDownTC);
                      END;
                    UNTIL (AdjacentDownTC = UnknownTC) OR (TrackCircuits[AdjacentDownTC].TC_OccupationState = TCUnoccupied);

                    InvalidateScreen(UnitRef, 'TCPopupAllocateLocoToTrackCircuitClick');
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
      Log('EG TCPopupAllocateLocoToTrackCircuitClick:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TCPopupAllocateLocoToTrackCircuitClick }

PROCEDURE TMainWindow.TCPopupChangeInternalLocoDirectionToUpClick(Sender: TObject);
CONST
  NoValue = 0;

VAR
  LocoChip : Integer;
  T : Train;

BEGIN
  TRY
    WITH Lines[TrackCircuitPopupLine] DO BEGIN
      IF Line_TC <> UnknownTC THEN BEGIN
        LocoChip := TrackCircuits[Line_TC].TC_LocoChip;
        IF LocoChip <> UnknownLocoChip THEN BEGIN
          IF MessageDialogueWithDefault('Change loco ' + LocoChipToStr(LocoChip) + '''s internal direction to up - are you sure?',
                                        NOT StopTimer, mtWarning, [mbYes, mbNo], mbNo) <> mrYes THEN
            Debug('Cancelling change of loco ' + LocoChipToStr(LocoChip) + '''s internal direction changed to up')
          ELSE BEGIN
            T := GetTrainRecord(LocoChip);
            WITH T^ DO BEGIN
              IF Train_LightsType <> LightsOperatedByTwoChips THEN BEGIN
                ProgramOnTheMain(LocoChip, ChangeDirectionToUp, NoValue);
                Log(LocoChipToStr(LocoChip) + ' XG Internal direction changed to up');
              END ELSE BEGIN
                ProgramOnTheMain(Train_LightingChipUp, ChangeDirectionToUp, NoValue);
                Log(LocoChipToStr(Train_LightingChipUp) + ' XG Internal direction changed to up');

                ProgramOnTheMain(Train_LightingChipDown, ChangeDirectionToUp, NoValue);
                Log(LocoChipToStr(Train_LightingChipDown) + ' XG Internal direction changed to up');
              END;
            END; {WITH}
          END;
        END;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG TCPopupChangeLocoDirectionToUpClick:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TCPopupChangeLocoDirectionToUpClick }

PROCEDURE TMainWindow.TCPopupChangeInternalLocoDirectionToDownClick(Sender: TObject);
CONST
  NoValue = 0;

VAR
  LocoChip : Integer;
  T : Train;

BEGIN
  TRY
    WITH Lines[TrackCircuitPopupLine] DO BEGIN
      IF Line_TC <> UnknownTC THEN BEGIN
        LocoChip := TrackCircuits[Line_TC].TC_LocoChip;
        IF LocoChip <> UnknownLocoChip THEN BEGIN
          IF MessageDialogueWithDefault('Change loco ' + LocoChipToStr(LocoChip) + '''s internal direction to down - are you sure?',
                                        NOT StopTimer, mtWarning, [mbYes, mbNo], mbNo) <> mrYes THEN
            Debug('Cancelling change of loco ' + LocoChipToStr(LocoChip) + '''s internal direction changed to down')
          ELSE BEGIN
            T := GetTrainRecord(LocoChip);
            WITH T^ DO BEGIN
              IF Train_LightsType <> LightsOperatedByTwoChips THEN BEGIN
                ProgramOnTheMain(LocoChip, ChangeDirectionToDown, NoValue);
                Log(LocoChipToStr(LocoChip) + ' XG Internal direction changed to down');
              END ELSE BEGIN
                ProgramOnTheMain(Train_LightingChipUp, ChangeDirectionToDown, NoValue);
                Log(LocoChipToStr(Train_LightingChipUp) + ' XG Internal direction changed to down');

                ProgramOnTheMain(Train_LightingChipDown, ChangeDirectionToDown, NoValue);
                Log(LocoChipToStr(Train_LightingChipDown) + ' XG Internal direction changed to down');
              END;
            END; {WITH}
          END;
        END;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG TCPopupChangeLocoDirectionToDownClick:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TCPopupChangeLocoDirectionToDownClick }

PROCEDURE TMainWindow.TCPopupSetTrackCircuitToUserDrivingClick(Sender: TObject);
BEGIN
  TRY
    WITH Lines[TrackCircuitPopupLine] DO BEGIN
      IF TrackCircuits[Line_TC].TC_UserMustDrive THEN BEGIN
        TrackCircuits[Line_TC].TC_UserMustDrive := False;
        Log('T TC=' + IntToStr(Line_TC) + ' set to automatic operation');
        TCPopupSetTrackCircuitToUserDriving.Caption := 'Set Track Circuit To User Must Drive';
      END ELSE BEGIN
        TrackCircuits[Line_TC].TC_UserMustDrive := True;
        Log('T TC=' + IntToStr(Line_TC) + ' set to manual operation');
        TCPopupSetTrackCircuitToUserDriving.Caption := 'Set Track Circuit To Auto';
      END;

      IF ShowTrackCircuitsWhereUserMustDrive THEN
        InvalidateScreen(UnitRef, 'TCPopupSetTrackCircuitToUserDrivingClick');
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG TCPopupSetTrackCircuitToUserDrivingClick:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TCPopupSetTrackCircuitToUserDrivingClick }

PROCEDURE TMainWindow.TCPopupSetTrackCircuitSpeedRestrictionClick(Sender: TObject);
VAR
  ClearRestriction : Boolean;
  DefaultDirectionStr : String;
  DefaultSpeedStr : String;
  Direction : DirectionType;
  DirectionStr : String;
  OK : Boolean;
  Speed : Integer;
  SpeedStr : String;

BEGIN
  TRY
    ClearRestriction := False;
    OK := False;
    DirectionStr := '';

    WITH Lines[TrackCircuitPopupLine] DO BEGIN
      IF Line_TC <> UnknownTC THEN BEGIN
        WITH TrackCircuits[Line_TC] DO BEGIN
          IF TC_SpeedRestrictionInMPH = NoSpecifiedSpeed THEN BEGIN
            DefaultSpeedStr := '';
            DefaultDirectionStr := DirectionToStr(Line_Direction);
          END ELSE BEGIN
            DefaultSpeedStr := MPHTOStr(TC_SpeedRestrictionInMPH);
            DefaultDirectionStr := DirectionToStr(TC_SpeedRestrictionDirection);
          END;

          SpeedStr := InputBox('Speed Restriction at TrackCircuit Occupation', 'Maximum Speed?', DefaultSpeedStr);
          IF NOT TryStrToInt(SpeedStr, Speed) THEN
            ShowMessage('"' + SpeedStr + '" is not a valid speed')
          ELSE BEGIN
            CASE Speed OF
              0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120:
                OK := True;
              ELSE
                ShowMessage('"' + SpeedStr + '" is not a valid speed');
            END; {CASE}
          END;
          IF OK THEN BEGIN
            IF Speed = 0 THEN BEGIN
              IF TC_SpeedRestrictionInMPH = NoSpecifiedSpeed THEN
                Exit
              ELSE BEGIN
                IF MessageDialogueWithDefault('Do you wish to clear the ' + MPHToStr(TC_SpeedRestrictionInMPH) + ' mph speed restriction at TC=' + IntToStr(Line_TC) + '?',
                                              NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
                THEN BEGIN
                  DefaultSpeedStr := MPHTOStr(TC_SpeedRestrictionInMPH);
                  DirectionStr := DirectionToStr(TC_SpeedRestrictionDirection);
                END ELSE BEGIN
                  TC_SpeedRestrictionInMPH := NoSpecifiedSpeed;
                  TC_SpeedRestrictionDirection := UnknownDirection;
                  ClearRestriction := True;
                END;
              END;
            END;

            IF NOT ClearRestriction THEN BEGIN
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
        InvalidateScreen(UnitRef, 'TCPopupSetTCSpeedRestrictionClick');
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG TCPopupSetTCSpeedRestrictionClick:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TCPopupSetTCSpeedRestrictionClick }

PROCEDURE TMainWindow.TCPopupClearTrackCircuitSpeedRestrictionClick(Sender: TObject);
BEGIN
  TRY
    WITH Lines[TrackCircuitPopupLine] DO BEGIN
      IF Line_TC <> UnknownTC THEN BEGIN
        WITH TrackCircuits[Line_TC] DO BEGIN
          IF TC_SpeedRestrictionInMPH <> NoSpecifiedSpeed THEN BEGIN
             IF MessageDialogueWithDefault('Do you wish to clear the ' + MPHToStr(TC_SpeedRestrictionInMPH) + ' mph speed restriction at TC=' + IntToStr(Line_TC) + '?',
                                           NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
            THEN BEGIN
              TC_SpeedRestrictionInMPH := NoSpecifiedSpeed;
              TC_SpeedRestrictionDirection := UnknownDirection;
            END;
          END;
        END; {WITH}
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG ClearTrackCircuitSpeedRestrictionClick:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ClearTrackCircuitSpeedRestrictionClick }

PROCEDURE TMainWindow.TCPopupSetLineOutOfUseClick(Sender: TObject);
BEGIN
  TRY
    WITH Lines[TrackCircuitPopupLine] DO BEGIN
      IF Line_OutOfUseState = OutOfUse THEN BEGIN
        TCPopupSetLineOutOfUse.Caption := 'Put Line ''' + LineToStr(TrackCircuitPopupLine) + ''' out of use';
        Line_OutOfUseState := InUse;
      END ELSE BEGIN
        TCPopupSetLineOutOfUse.Caption := 'Return Line ''' + LineToStr(TrackCircuitPopupLine) + ''' to use';
        Line_OutOfUseState := OutOfUse;
      END;
      InvalidateScreen(UnitRef, 'TCPopupSetLineOutOfUseClick');
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG TCPopupSetLineOutOfUseClick:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TCPopupSetLineOutOfUseClick }

PROCEDURE TMainWindow.TCPopupSetLocationOutOfUseClick(Sender: TObject);
VAR
  LineCount : Integer;

BEGIN
  TRY
    WITH Lines[TrackCircuitPopupLine] DO BEGIN
      IF Line_Location <> UnknownLocation THEN BEGIN
        WITH Locations[Line_Location] DO BEGIN
          IF Location_OutOfUse THEN BEGIN
            TCPopupSetLocationOutOfUse.Caption := 'Put Location ''' + LineToStr(TrackCircuitPopupLine) + ''' out of use';
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
            TCPopupSetLocationOutOfUse.Caption := 'Return Location ''' + LineToStr(TrackCircuitPopupLine) + ''' to use';
            Location_OutOfUse := True;

            { Now set all the lines within the location to out of use too }
            FOR LineCount := 0 TO High(Lines) DO BEGIN
              IF Lines[LineCount].Line_Location = Line_Location THEN BEGIN
                Lines[LineCount].Line_SaveOutOfUseState := Lines[LineCount].Line_OutOfUseState;
                Lines[LineCount].Line_OutOfUseState := OutOfUse;
              END;
            END;
          END;
          InvalidateScreen(UnitRef, 'TCPopupSetLineOutOfUseClick');
        END; {WITH}
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG TCPopupSetLocationOutOfUseClick:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TCPopupSetLocationOutOfUseClick }

PROCEDURE TMainWindow.GeneralPopupChangeDefaultPointColourClick(Sender: TObject);
BEGIN
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointDefaultColourClick');
  END;
END; { GeneralPopupChangePointDefaultColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointDefaultColourClick(Sender: TObject);
BEGIN
  PointColour := DefaultPointColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointDefaultColourClick');
END; { GeneralPopupRestorePointDefaultColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreDefaultBackgroundColourClick(Sender: TObject);
BEGIN
  BackgroundColour := DefaultBackgroundColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreDefaultBackgroundColourClick');
END; { GeneralPopupRestoreDefaultBackgroundColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeBackgroundColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := BackgroundColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    BackgroundColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeBackgroundColourClick');
  END;
END; { GeneralPopupChangeBackgroundColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeForegroundColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := ForegroundColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    ForegroundColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeForegroundColourClick');
  END;
END; { GeneralPopupChangeForegroundColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreForegroundColourClick(Sender: TObject);
BEGIN
  ForegroundColour := DefaultForegroundColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreForegroundColourClick');
END; { GeneralPopupRestoreForegroundColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePlatformColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PlatformColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PlatformColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePlatformColourClick');
  END;
END; { GeberalPopupChangePlatformColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePlatformColourClick(Sender: TObject);
BEGIN
  PlatformColour := DefaultPlatformColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePlatformColourClick');
END; { GeneralPopupRestorePlatformColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCMissingOccupationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCMissingOccupationColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCMissingOccupationColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCMissingOccupationColourClick');
  END;
END; { ChangeMissingOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCMissingOccupationColourClick(Sender: TObject);
BEGIN
  TCMissingOccupationColour := DefaultTCMissingOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCMissingOccupationColourClick');
END; { GeneralPopupRestoreTCMissingOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeLocoStalledColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := LocoStalledColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    LocoStalledColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeLocoStalledColourClick');
  END;
END; { ChangeLocoStalledColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreLocoStalledColourClick(Sender: TObject);
BEGIN
  LocoStalledColour := DefaultLocoStalledColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreLocoStalledColourClick');
END; { GeneralPopupRestoreLocoStalledColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCPermanentFeedbackOccupationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCPermanentFeedbackOccupationColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCPermanentFeedbackOccupationColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCPermanentFeedbackOccupationColourClick');
  END;
END; { GeneralPopupChangeTCPermanentFeedbackOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCPermanentFeedbackOccupationColourClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationColour := DefaultTCPermanentFeedbackOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentFeedbackOccupationColourClick');
END; { GeneralPopupRestoreTCPermanentFeedbackOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCPermanentSystemOccupationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCPermanentSystemOccupationColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCPermanentSystemOccupationColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCPermanentSystemOccupationColourClick');
  END;
END; { GeneralPopupChangeTCPermanentSystemOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCPermanentSystemOccupationColourClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationColour := DefaultTCPermanentSystemOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentSystemOccupationColourClick');
END; { GeneralPopupRestoreTCPermanentSystemOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCPermanentOccupationSetByUserColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCPermanentOccupationSetByUserColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCPermanentOccupationSetByUserColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCPermanentOccupationSetByUserColourClick');
  END;
END; { GeneralPopupChangeTCPermanentOccupationSetByUserColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCPermanentOccupationSetByUserColourClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserColour := DefaultTCPermanentOccupationSetByUserColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentOccupationSetByUserColourClick');
END; { GeneralPopupRestoreTCPermanentOccupationSetByUserColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCSpeedRestrictionColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCSpeedRestrictionColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCSpeedRestrictionColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCSpeedRestrictionColourClick');
  END;
END; { GeneralPopupChangeTCSpeedRestrictionColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCSpeedRestrictionColourClick(Sender: TObject);
BEGIN
  TCSpeedRestrictionColour := DefaultTCSpeedRestrictionColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCSpeedRestrictionColourClick');
END; { GeneralPopupRestoreTCSpeedRestrictionColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCLocoOutOfPlaceColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCLocoOutOfPlaceOccupationColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCLocoOutOfPlaceOccupationColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCLocoOutOfPlaceColourClick');
  END;
END; { GeneralPopupChangeTCLocoOutOfPlaceColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCLocoOutOfPlaceColourClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationColour := DefaultTCLocoOutOfPlaceOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCLocoOutOfPlaceColourClick');
END; { GeneralPopupRestoreTCLocoOutOfPlaceColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCFeedbackDataInUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCFeedbackDataInUseColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCFeedbackDataInUseColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCFeedbackDataInUseColourClick');
  END;
END; { ChangeTCFeedbackDataInUseColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCFeedbackDataInUseColourClick(Sender: TObject);
BEGIN
  TCFeedbackDataInUseColour := DefaultTCFeedbackDataInUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCFeedbackDataInUseColourClick');
END; { GeneralPopupRestoreTCFeedbackDataInUseColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCFeedbackDataOutOfUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCFeedbackDataOutOfUseColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCFeedbackDataOutOfUseColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCFeedbackDataOutOfUseColourClick');
  END;
END; { ChangeTCFeedbackDataOutOfUseColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCFeedbackDataOutOfUseColourClick(Sender: TObject);
BEGIN
  TCFeedbackDataOutOfUseColour := DefaultTCFeedbackDataOutOfUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCFeedbackDataOutOfUseColourClick');
END; { GeneralPopupRestoreTCFeedbackDataOutOfUseColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCFeedbackOccupationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCFeedbackOccupationColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCFeedbackOccupationColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCFeedbackOccupationColourClick');
  END;
END; { ChangeFeedbackOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCFeedbackOccupationColourClick(Sender: TObject);
BEGIN
  TCFeedbackOccupationColour := DefaultTCFeedbackOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCFeedbackOccupationColourClick');
END; { GeneralPopupRestoreTCFeedbackOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCFeedbackOccupationButOutOfUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCFeedbackOccupationButOutOfUseColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCFeedbackOccupationButOutOfUseColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCFeedbackOccupationButOutOfUseColourClick');
  END;
END; { ChangeFeedbackOccupationButOutOfUseColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCFeedbackOccupationButOutOfUseColourClick(Sender: TObject);
BEGIN
  TCFeedbackOccupationButOutOfUseColour := DefaultTCFeedbackOccupationButOutOfUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCFeedbackOccupationButOutOfUseColourClick');
END; { GeneralPopupRestoreTCFeedbackOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCSystemOccupationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCSystemOccupationColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCSystemOccupationColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCSystemOccupationColourClick');
  END;
END; { ChangeTCSystemOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCSystemOccupationColourClick(Sender: TObject);
BEGIN
  TCSystemOccupationColour := DefaultTCSystemOccupationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCSystemOccupationColourClick');
END; { GeneralPopupRestoreTCSystemOccupationColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCUnoccupiedColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCUnoccupiedColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCUnoccupiedColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCUnoccupiedColourClick');
  END;
END; { GeneralPopupChangeTCUnoccupiedColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCUnoccupiedColourClick(Sender: TObject);
BEGIN
  TCUnoccupiedColour := DefaultTCUnoccupiedColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCUnoccupiedColourClick');
END; { GeneralPopupRestoreTCUnoccupiedColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCOutOfUseSetByUserColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCOutOfUseSetByUserColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCOutOfUseSetByUserColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCOutOfUseSetByUserColourClick');
  END;
END; { GeneralPopupChangeTCOutOfUseSetByUserColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCOutOfUseSetByUserColourClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserColour := DefaultTCOutOfUseSetByUserColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCOutOfUseSetByUserColourClick');
END; { GeneralPopupRestoreTCOutOfUseSetByUserColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTCOutOfUseAsNoFeedbackReceivedColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TCOutOfUseAsNoFeedbackReceivedColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TCOutOfUseAsNoFeedbackReceivedColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTCOutOfUseAsNoFeedbackReceivedColourClick');
  END;
END; { ChangeMysteryOccupationColour2Click }

PROCEDURE TMainWindow.GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedColourClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedColour := DefaultTCOutOfUseAsNoFeedbackReceivedColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedColourClick');
END; { GeneralPopupRestoreTCOutOfUseColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeBufferStopColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := BufferStopColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    BufferStopColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeBufferStopColourClick');
  END;
END; { ChangeBufferStopColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreBufferStopColourClick(Sender: TObject);
BEGIN
  BufferStopColour := DefaultBufferStopColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreBufferStopColourClick');
END; { GeneralPopupRestoreBufferStopColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeBufferStopNumberColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := BufferStopNumberColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    BufferStopNumberColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeBufferStopNumberColourClick');
  END;
END; { ChangeBufferStopNumberColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreBufferStopNumberColourClick(Sender: TObject);
BEGIN
  BufferStopNumberColour := DefaultBufferStopNumberColour;
  DrawMap;
END; { GeneralPopupRestoreBufferStopNumberColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeBufferStopRedClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := BufferStopRed;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    BufferStopRed := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupRestoreBufferStopNumberColourClick');
  END;
END; { ChangeBufferStopRedClick }

PROCEDURE TMainWindow.GeneralPopupRestoreBufferStopRedClick(Sender: TObject);
BEGIN
  BufferStopRed := DefaultBufferStopRed;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreBufferStopRedClick');
END; { GeneralPopupRestoreBufferStopRedClick }

PROCEDURE TMainWindow.GeneralPopupChangeLineNotAvailableColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := LineNotAvailableColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    LineNotAvailableColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeLineNotAvailableColourClick');
  END;
END; { ChangeLineNotAvailableColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreLineNotAvailableColourClick(Sender: TObject);
BEGIN
  LineNotAvailableColour := DefaultLineNotAvailableColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreLineNotAvailableColourClick');
END; { GeneralPopupRestoreLineNotAvailableColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePlungerPressedColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TRSPlungerPressedColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TRSPlungerPressedColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePlungerPressedColourClick');
  END;
END; { ChangePlungerPressedColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePlungerPressedColourClick(Sender: TObject);
BEGIN
  TRSPlungerPressedColour := DefaultTRSPlungerPressedColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePlungerPressedColourClick');
END; { GeneralPopupRestorePlungerPressedColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeSignalAspectUnlitColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := SignalAspectUnlit;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    SignalAspectUnlit := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalAspectUnlitColourClick');
  END;
END; { ChangeSignalAspectUnlitClick }

PROCEDURE TMainWindow.GeneralPopupRestoreSignalAspectUnlitColourClick(Sender: TObject);
BEGIN
  SignalAspectUnlit := DefaultSignalAspectUnlit;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalAspectUnlitColourClick');
END; { GeneralPopupRestoreSignalAspectUnlitColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeSignalAspectYellowClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := SignalAspectYellow;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    SignalAspectYellow := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalAspectYellowClick');
  END;
END; { ChangeSignalAspectYellowClick }

PROCEDURE TMainWindow.GeneralPopupRestoreSignalAspectYellowClick(Sender: TObject);
BEGIN
  SignalAspectYellow := DefaultSignalAspectYellow;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalAspectYellowClick');
END; { GeneralPopupRestoreSignalAspectYellowClick }

PROCEDURE TMainWindow.GeneralPopupChangeSignalAspectRedClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := SignalAspectRed;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    SignalAspectRed := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalAspectRedClick');
  END;
END; { ChangeSignalAspectRedClick }

PROCEDURE TMainWindow.GeneralPopupRestoreSignalAspectRedClick(Sender: TObject);
BEGIN
  SignalAspectRed := DefaultSignalAspectRed;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalAspectRedClick');
END; { GeneralPopupRestoreSignalAspectRedClick }

PROCEDURE TMainWindow.GeneralPopupChangeSignalAspectGreenClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := SignalAspectGreen;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    SignalAspectGreen := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalAspectGreenClick');
  END;
END; { ChangeSignalAspectGreenClick }

PROCEDURE TMainWindow.GeneralPopupRestoreSignalAspectGreenClick(Sender: TObject);
BEGIN
  SignalAspectGreen := DefaultSignalAspectGreen;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalAspectGreenClick');
END; { GeneralPopupRestoreSignalAspectGreenClick }

PROCEDURE TMainWindow.GeneralPopupChangeSignalPostRouteSettingColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := SignalPostRouteSettingColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    SignalPostRouteSettingColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalPostRouteSettingColourClick');
  END;
END; { ChangeSignalPostRouteSettingColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreSignalPostRouteSettingColourClick(Sender: TObject);
BEGIN
  SignalPostRouteSettingColour := DefaultSignalPostRouteSettingColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalPostRouteSettingColourClick');
END; { GeneralPopupRestoreSignalPostRouteSettingColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeSignalPostEmergencyRouteSettingColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := SignalPostRouteSettingColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    SignalPostRouteSettingColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalPostEmergencyRouteSettingColourClick');
  END;
END; { ChangeSignalPostEmergencyRouteSettingColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreSignalPostEmergencyRouteSettingColourClick(Sender: TObject);
BEGIN
  SignalPostRouteSettingColour := DefaultSignalPostRouteSettingColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalPostEmergencyRouteSettingColourClick');
END; { GeneralPopupRestoreSignalPostEmergencyRouteSettingColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeSignalPostTheatreSettingColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := SignalPostTheatreSettingColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    SignalPostTheatreSettingColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalPostTheatreSettingColourClick');
  END;
END; { ChangePostTheatreSettingColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreSignalPostTheatreSettingColourClick(Sender: TObject);
BEGIN
  SignalPostTheatreSettingColour := DefaultSignalPostTheatreSettingColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalPostTheatreSettingColourClick');
END; { GeneralPopupRestoreSignalPostTheatreSettingColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeSignalNumberColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := SignalNumberColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    SignalNumberColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeSignalNumberColourClick');
  END;
END; { ChangeSignalberColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreSignalNumberColourClick(Sender: TObject);
BEGIN
  SignalNumberColour := DefaultSignalNumberColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSignalNumberColourClick');
END; { GeneralPopupRestoreSignalNumberColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTrainActiveColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TrainActiveColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TrainActiveColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTrainActiveColourClick');
  END;
END; { ChangeTrainActiveColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTrainActiveColourClick(Sender: TObject);
BEGIN
  TrainActiveColour := DefaultTrainActiveColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTrainActiveColourClick');
END; { GeneralPopupRestoreTrainActiveColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeTrainInactiveColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TrainInactiveColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TrainInactiveColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeTrainInactiveColourClick');
  END;
END; procedure TMainWindow.GeneralPopupClockClick(Sender: TObject);
begin

end;

{ ChangeTrainInactiveColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTrainInactiveColourClick(Sender: TObject);
BEGIN
  TrainInactiveColour := DefaultTrainInactiveColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTrainInactiveColourClick');
END; { GeneralPopupRestoreTrainInactiveColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointUpFacingColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointUpFacingColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointUpFacingColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointUpFacingColourClick');
  END;
END; { ChangeUpFacingPointColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointUpFacingColourClick(Sender: TObject);
BEGIN
  PointUpFacingColour := DefaultPointUpFacingColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointUpFacingColourClick');
END; { GeneralPopupRestorePointUpFacingColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointDownFacingColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointDownFacingColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointDownFacingColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointDownFacingColourClick');
  END;
END; { ChangeDownFacingPointColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointDownFacingColourClick(Sender: TObject);
BEGIN
  PointDownFacingColour := DefaultPointDownFacingColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointDownFacingColourClick');
END; { GeneralPopupRestorePointDownFacingColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeShowPointDefaultStateColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := ShowPointDefaultStateColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    ShowPointDefaultStateColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeShowPointDefaultStateColourClick');
  END;
END; { ChangeShowPointDefaultStateColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreShowPointDefaultStateColourClick(Sender: TObject);
BEGIN
  ShowPointDefaultStateColour := DefaultShowPointDefaultStateColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreShowPointDefaultStateColourClick');
END; { GeneralPopupRestoreShowPointDefaultStateColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeLenzPointNumberColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := LenzPointNumberColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    LenzPointNumberColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeLenzPointNumberColourClick');
  END;
END; { ChangeLenzPointNumberColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreLenzPointNumberColourClick(Sender: TObject);
BEGIN
  LenzPointNumberColour := DefaultLenzPointNumberColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreLenzPointNumberColourClick');
END; { GeneralPopupRestoreLenzPointNumberColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointManualOperationColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointManualOperationColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointManualOperationColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointManualOperationColourClick');
  END;
END; { ChangePointManualOperationColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointManualOperationColourClick(Sender: TObject);
BEGIN
  PointManualOperationColour := DefaultPointManualOperationColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointManualOperationColourClick');
END; { GeneralPopupRestorePointManualOperationColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointFeedbackDataInUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointFeedbackDataInUseColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointFeedbackDataInUseColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointFeedbackDataInUseColourClick');
  END;
END; { ChangePointFeedbackDataInUseColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointFeedbackDataInUseColourClick(Sender: TObject);
BEGIN
  PointFeedbackDataInUseColour := DefaultPointFeedbackDataInUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointFeedbackDataInUseColourClick');
END; { GeneralPopupRestorePointFeedbackDataInUseColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointFeedbackDataOutOfUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointFeedbackDataOutOfUseColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointFeedbackDataOutOfUseColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointFeedbackDataOutOfUseColourClick');
  END;
END; { ChangePointFeedbackDataOutOfUseColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointFeedbackDataOutOfUseColourClick(Sender: TObject);
BEGIN
  PointFeedbackDataOutOfUseColour := DefaultPointFeedbackDataOutOfUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointFeedbackDataOutOfUseColourClick');
END; { GeneralPopupRestorePointFeedbackDataOutOfUseColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointsWithoutFeedbackColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointsWithoutFeedbackColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointsWithoutFeedbackColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointsWithoutFeedbackColourClick');
  END;
END; { ChangePointsWithoutFeedbackColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointsWithoutFeedbackColourClick(Sender: TObject);
BEGIN
  PointsWithoutFeedbackColour := DefaultPointsWithoutFeedbackColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointsWithoutFeedbackColourClick');
END; { GeneralPopupRestorePointsWithoutFeedbackColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointHeelLineColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointHeelLineColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointHeelLineColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointHeelLineColourClick');
  END;
END; { ChangePointHeelLineColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointHeelLineColourClick(Sender: TObject);
BEGIN
  PointHeelLineColour := DefaultPointHeelLineColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointHeelLineColourClick');
END; { GeneralPopupRestorePointHeelLineColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointStraightLineColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointStraightLineColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointStraightLineColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointStraightLineColourClick');
  END;
END; { ChangePointStraightLineColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointStraightLineColourClick(Sender: TObject);
BEGIN
  PointStraightLineColour := DefaultPointStraightLineColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointStraightLineColourClick');
END; { GeneralPopupRestorePointStraightLineColourClick }

PROCEDURE TMainWindow.GeneralPopupChangeLineRoutedOverColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := LineRoutedOverColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    LineRoutedOverColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangeLineRoutedOverColourClick');
  END;
END; { GeneralPopupChangeLineRoutedOverColourClick }

PROCEDURE TMainWindow.GeneralPopupRestoreLineRoutedOverColourClick(Sender: TObject);
BEGIN
  LineRoutedOverColour := DefaultLineRoutedOverColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreLineRoutedOverColourClick');
END; { GeneralPopupRestoreLineRoutedOverColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointDivergingLineColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointDivergingLineColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointDivergingLineColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointDivergingLineColourClick');
  END;
END; { ChangePointDivergingLineColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointDivergingLineColourClick(Sender: TObject);
BEGIN
  PointDivergingLineColour := DefaultPointDivergingLineColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointDivergingLineColourClick');
END; { GeneralPopupRestorePointDivergingLineColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointUndrawColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointUndrawColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointUndrawColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointUndrawColourClick');
  END;
END; { GeneralPopupChangePointUndrawColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointUndrawColourClick(Sender: TObject);
BEGIN
  PointUndrawColour := DefaultPointUndrawColour;
  InvalidateScreen(UnitRef, 'END; { GeneralPopupChangePointU');
END; { GeneralPopupRestorePointUndrawColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePointOutOfUseColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := PointOutOfUseColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    PointOutOfUseColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePointOutOfUseColourClick');
  END;
END; { GeneralPopupChangePointOutOfUseColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePointOutOfUseColourClick(Sender: TObject);
BEGIN
  PointOutOfUseColour := DefaultPointOutOfUseColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePointOutOfUseColourClick');
END; { GeneralPopupRestorePointOutOfUseColourClick }

PROCEDURE TMainWindow.GeneralPopupChangePlungerColourClick(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TRSPlungerColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TRSPlungerColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePlungerColourClick');
  END;
END; { GeneralPopupChangePlungerColourClick }

PROCEDURE TMainWindow.GeneralPopupRestorePlungerColour(Sender: TObject);
BEGIN
  TRSPlungerColour := DefaultTRSPlungerColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePlungerColour');
END; { GeneralPopupRestorePlungerColour }

PROCEDURE TMainWindow.GeneralPopupChangePlungerOutlineColour(Sender: TObject);
BEGIN
  { Show the default }
  MainWindowColourDialogue.Color := TRSPlungerOutlineColour;
  { Allow the user to change it }
  IF MainWindowColourDialogue.Execute THEN BEGIN
    TRSPlungerOutlineColour := MainWindowColourDialogue.Color;
    InvalidateScreen(UnitRef, 'GeneralPopupChangePlungerOutlineColour');
  END;
END; { GeneralPopupChangePlungerOutlineColour }

PROCEDURE TMainWindow.GeneralPopupRestorePlungerOutlineColour(Sender: TObject);
BEGIN
  TRSPlungerOutlineColour := DefaultTRSPlungerOutlineColour;
  InvalidateScreen(UnitRef, 'GeneralPopupRestorePlungerOutlineColour');
END; { GeneralPopupRestorePlungerOutlineColour }

PROCEDURE TMainWindow.GeneralPopupRestoreAllDefaultColoursClick(Sender: TObject);
BEGIN
  BackgroundColour := DefaultBackgroundColour;
  BufferStopColour := DefaultBufferStopColour;
  BufferStopNumberColour := DefaultBufferStopNumberColour;
  BufferStopRed := DefaultBufferStopRed;
  ForegroundColour := DefaultForegroundColour;
  LenzPointNumberColour := DefaultLenzPointNumberColour;
  LineNotAvailableColour := DefaultLineNotAvailableColour;
  LocoStalledColour := DefaultLocoStalledColour;
  PlatformColour := DefaultPlatformColour;
  PointColour := DefaultPointColour;
  PointDivergingLineColour := DefaultPointDivergingLineColour;
  PointDownFacingColour := DefaultPointDownFacingColour;
  PointFeedbackDataInUseColour := DefaultPointFeedbackDataInUseColour;
  PointFeedbackDataOutOfUseColour := DefaultPointFeedbackDataOutOfUseColour;
  PointHeelLineColour := DefaultPointHeelLineColour;
  PointManualOperationColour := DefaultPointManualOperationColour;
  PointStraightLineColour :=DefaultPointStraightLineColour;
  PointsWithoutFeedbackColour := DefaultPointsWithoutFeedbackColour;
  PointUpFacingColour := DefaultPointUpFacingColour;
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
  TrainActiveColour := DefaultTrainActiveColour;
  TrainInactiveColour := DefaultTrainInactiveColour;
  TRSPlungerColour := DefaultTRSPlungerColour;
  TRSPlungerOutlineColour := DefaultTRSPlungerOutlineColour;
  TRSPlungerPressedColour := DefaultTRSPlungerPressedColour;

  InvalidateScreen(UnitRef, 'RestoreAllDefaultColoursClick');
END; { RestoreAllDefaultColoursClick }

PROCEDURE TMainWindow.GeneralPopupSidingPenStyleSolidClick(Sender: TObject);
BEGIN
  SidingPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleSolidClick');
END; { GeneralPopupSidingPenStyleSolidClick }

PROCEDURE TMainWindow.GeneralPopupSidingPenStyleDashClick(Sender: TObject);
BEGIN
  SidingPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleDashClick');
END; { GeneralPopupSidingPenStyleDashClick }

PROCEDURE TMainWindow.GeneralPopupSidingPenStyleDotClick(Sender: TObject);
BEGIN
  SidingPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleDotClick');
END; { GeneralPopupSidingPenStyleDotClick }

PROCEDURE TMainWindow.GeneralPopupSidingPenStyleDashDotClick(Sender: TObject);
BEGIN
  SidingPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleDashDotClick');
END; { GeneralPopupSidingPenStyleDashDotClick }

PROCEDURE TMainWindow.GeneralPopupSidingPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  SidingPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleDashDotDotClick');
END; { GeneralPopupSidingPenStyleDashDotDotClick }

PROCEDURE TMainWindow.GeneralPopupSidingPenStyleClearClick(Sender: TObject);
BEGIN
  SidingPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleClearClick');
END; { GeneralPopupSidingPenStyleClearClick }

PROCEDURE TMainWindow.GeneralPopupSidingPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  SidingPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupSidingPenStyleInsideFrameClick');
END; { GeneralPopupSidingPenStyleInsideFrameClick }

PROCEDURE TMainWindow.GeneralPopupRestoreSidingPenStyleClick(Sender: TObject);
BEGIN
  SidingPenStyle := DefaultSidingPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreSidingPenStyleClick');
END; { GeneralPopupRestoreSidingPenStyleClick }

PROCEDURE TMainWindow.GeneralPopupFiddleyardLinePenStyleSolidClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleSolidClick');
END; { GeneralPopupFiddleyardLinePenStyleSolidClick }

PROCEDURE TMainWindow.GeneralPopupFiddleyardLinePenStyleDashClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleDashClick');
END; { GeneralPopupFiddleyardLinePenStyleDashClick }

PROCEDURE TMainWindow.GeneralPopupFiddleyardLinePenStyleDotClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleDotClick');
END; { GeneralPopupFiddleyardLinePenStyleDotClick }

PROCEDURE TMainWindow.GeneralPopupFiddleyardLinePenStyleDashDotClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleDashDotClick');
END; { GeneralPopupFiddleyardLinePenStyleDashDotClick }

PROCEDURE TMainWindow.GeneralPopupFiddleyardLinePenStyleDashDotDotClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleDashDotDotClick');
END; { GeneralPopupFiddleyardLinePenStyleDashDotDotClick }

PROCEDURE TMainWindow.GeneralPopupFiddleyardLinePenStyleClearClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleClearClick');
END; { GeneralPopupFiddleyardLinePenStyleClearClick }

PROCEDURE TMainWindow.GeneralPopupFiddleyardLinePenStyleInsideFrameClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupFiddleyardLinePenStyleInsideFrameClick');
END; { GeneralPopupFiddleyardLinePenStyleInsideFrameClick }

PROCEDURE TMainWindow.GeneralPopupRestoreFiddleyardLinePenStyleClick(Sender: TObject);
BEGIN
  FiddleyardLinePenStyle := DefaultFiddleyardLinePenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreFiddleyardLinePenStyleClick');
END; { GeneralPopupRestoreFiddleyardLinePenStyleClick }

PROCEDURE TMainWindow.GeneralPopupProjectedLinePenStyleSolidClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleSolidClick');
END; { GeneralPopupProjectedLinePenStyleSolidClick }

PROCEDURE TMainWindow.GeneralPopupProjectedLinePenStyleDashClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleDashClick');
END; { GeneralPopupProjectedLinePenStyleDashClick }

PROCEDURE TMainWindow.GeneralPopupProjectedLinePenStyleDotClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleDotClick');
END; { GeneralPopupProjectedLinePenStyleDotClick }

PROCEDURE TMainWindow.GeneralPopupProjectedLinePenStyleDashDotClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleDashDotClick');
END; { GeneralPopupProjectedLinePenStyleDashDotClick }

PROCEDURE TMainWindow.GeneralPopupProjectedLinePenStyleDashDotDotClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleDashDotDotClick');
END; { GeneralPopupProjectedLinePenStyleDashDotDotClick }

PROCEDURE TMainWindow.GeneralPopupProjectedLinePenStyleClearClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleClearClick');
END; { GeneralPopupProjectedLinePenStyleClearClick }

PROCEDURE TMainWindow.GeneralPopupProjectedLinePenStyleInsideFrameClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupProjectedLinePenStyleInsideFrameClick');
END; { GeneralPopupProjectedLinePenStyleInsideFrameClick }

PROCEDURE TMainWindow.GeneralPopupRestoreProjectedLinePenStyleClick(Sender: TObject);
BEGIN
  ProjectedLinePenStyle := DefaultProjectedLinePenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreProjectedLinePenStyleClick');
END; { GeneralPopupRestoreProjectedLinePenStyleClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleSolidClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleSolidClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleSolidClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDotClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDotClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDotClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotDotClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotDotClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleClearClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleClearClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleClearClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleInsideFrameClick');
END; { GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleInsideFrameClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedPenStyleClick(Sender: TObject);
BEGIN
  TCOutOfUseAsNoFeedbackReceivedPenStyle := DefaultTCOutOfUseAsNoFeedbackReceivedPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedPenStyleClick');
END; { GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedPenStyleClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseSetByUserPenStyleSolidClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleSolidClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleSolidClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseSetByUserPenStyleDashClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleDashClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleDashClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseSetByUserPenStyleDotClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleDotClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleDotClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseSetByUserPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleDashDotClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleDashDotClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseSetByUserPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleDashDotDotClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleDashDotDotClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseSetByUserPenStyleClearClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleClearClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleClearClick }

PROCEDURE TMainWindow.GeneralPopupTCOutOfUseSetByUserPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCOutOfUseSetByUserPenStyleInsideFrameClick');
END; { GeneralPopupTCOutOfUseSetByUserPenStyleInsideFrameClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCOutOfUseSetByUserPenStyleClick(Sender: TObject);
BEGIN
  TCOutOfUseSetByUserPenStyle := DefaultTCOutOfUseSetByUserPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCOutOfUseSetByUserPenStyleClick');
END; { GeneralPopupRestoreTCOutOfUseSetByUserPenStyleClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleSolidClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleSolidClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleSolidClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleDashClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleDashClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleDashClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleDotClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleDotClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleDotClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotDotClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotDotClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleClearClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleClearClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleClearClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentFeedbackOccupationPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentFeedbackOccupationPenStyleInsideFrameClick');
END; { GeneralPopupTCPermanentFeedbackOccupationPenStyleInsideFrameClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCPermanentFeedbackOccupationPenStyleClick(Sender: TObject);
BEGIN
  TCPermanentFeedbackOccupationPenStyle := DefaultTCPermanentFeedbackOccupationPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentFeedbackOccupationPenStyleClick');
END; { GeneralPopupRestoreTCPermanentFeedbackOccupationPenStyleClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentSystemOccupationPenStyleSolidClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleSolidClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleSolidClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentSystemOccupationPenStyleDashClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleDashClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleDashClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentSystemOccupationPenStyleDotClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleDotClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleDotClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentSystemOccupationPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleDashDotClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleDashDotClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentSystemOccupationPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleDashDotDotClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleDashDotDotClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentSystemOccupationPenStyleClearClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleClearClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleClearClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentSystemOccupationPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentSystemOccupationPenStyleInsideFrameClick');
END; { GeneralPopupTCPermanentSystemOccupationPenStyleInsideFrameClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCPermanentSystemOccupationPenStyleClick(Sender: TObject);
BEGIN
  TCPermanentSystemOccupationPenStyle := DefaultTCPermanentSystemOccupationPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentSystemOccupationPenStyleClick');
END; { GeneralPopupRestoreTCPermanentSystemOccupationPenStyleClick }

PROCEDURE TMainWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleSolidClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleSolidClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleSolidClick }

PROCEDURE TMainWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashClick }

PROCEDURE TMainWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDotClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDotClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDotClick }

PROCEDURE TMainWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotClick }

PROCEDURE TMainWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotDotClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotDotClick }

PROCEDURE TMainWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleClearClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleClearClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleClearClick }

PROCEDURE TMainWindow.GeneralPopupTCLocoOutOfPlaceOccupationPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCLocoOutOfPlaceOccupationPenStyleInsideFrameClick');
END; { GeneralPopupTCLocoOutOfPlaceOccupationPenStyleInsideFrameClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCLocoOutOfPlaceOccupationPenStyleClick(Sender: TObject);
BEGIN
  TCLocoOutOfPlaceOccupationPenStyle := DefaultTCLocoOutOfPlaceOccupationPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCLocoOutOfPlaceOccupationPenStyleClick');
END; { GeneralPopupRestoreTCLocoOutOfPlaceOccupationPenStyleClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleSolidClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psSolid;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleSolidClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleSolidClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleDashClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psDash;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleDashClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleDashClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleDotClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleDotClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleDotClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psDashDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotDotClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psDashDotDot;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotDotClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotDotClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleClearClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psClear;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleClearClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleClearClick }

PROCEDURE TMainWindow.GeneralPopupTCPermanentOccupationSetByUserPenStyleInsideFrameClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := psInsideFrame;
  InvalidateScreen(UnitRef, 'GeneralPopupTCPermanentOccupationSetByUserPenStyleInsideFrameClick');
END; { GeneralPopupTCPermanentOccupationSetByUserPenStyleInsideFrameClick }

PROCEDURE TMainWindow.GeneralPopupRestoreTCPermanentOccupationSetByUserPenStyleClick(Sender: TObject);
BEGIN
  TCPermanentOccupationSetByUserPenStyle := DefaultTCPermanentOccupationSetByUserPenStyle;
  InvalidateScreen(UnitRef, 'GeneralPopupRestoreTCPermanentOccupationSetByUserPenStyleClick');
END; { GeneralPopupRestoreTCPermanentOccupationSetByUserPenStyleClick }

PROCEDURE TMainWindow.GeneralPopupShowMainMenuClick(Sender: TObject);
BEGIN
  IF NOT MenusVisible THEN BEGIN
    ShowMenus;
    GeneralPopupShowMainMenu.Caption := 'Hide Main Menus';
  END ELSE BEGIN
    HideMenus;
    GeneralPopupShowMainMenu.Caption := 'Show Main Menus';
  END;
END; { GeneralPopupShowMainMenuClick }

PROCEDURE TMainWindow.GeneralPopupSetCurrentRailwayDayOfTheWeekClick(Sender: TObject);
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

PROCEDURE TMainWindow.GeneralPopupSetLogFileMaximumWidthClick(Sender: TObject);
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

PROCEDURE TMainWindow.MainWindowStatusBarMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
{ Save the XY position of the cursor as it passes over a status bar to work out where it is should the mouse key be pressed }
BEGIN
  StatusBarX := X;
  StatusBarY := X;
END; { MainWindowStatusBarMouseMove }

PROCEDURE TMainWindow.MainWindowStatusBarDblClick(Sender: TObject);
BEGIN
  { This is a work-around to find out if the cursor is within status bar Panel 0 as the proper test:
    "IF Sender = MainWindow.MainWindowStatusBar.Panels[0] THEN..." does not work.
  }
  IF (StatusBarX > 0)
  AND (StatusBarX <= MainWindowStatusBar.Panels[StatusBarPanel0].Width)
  THEN
    GetTime.ClockWindow.Visible := True;
END; { MainWindowStatusBarDblClick }

PROCEDURE TMainWindow.SetCurrentRailwayTime(Sender: TObject);
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  GetTime.ClockWindow.Clock.Time := StrToTime(CurrentRailwayTimeStr);
  GetTime.ClockWindow.Caption := SetCurrentRailwayTimeCaption;
  GetTime.ClockWindow.Visible := True;
  GetTime.ClockWindow.OKButton.SetFocus;
END; { SetCurrentRailwayTime }

PROCEDURE TMainWindow.SetProgramStartTime(Sender: TObject);
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  GetTime.ClockWindow.Clock.Time := ProgramStartTime;
  GetTime.ClockWindow.Caption := SetProgramStartTimeCaption;
  GetTime.ClockWindow.Visible := True;
  GetTime.ClockWindow.OKButton.SetFocus;
END; { SetProgramStartTime }

PROCEDURE TMainWindow.SetDaylightStartTime(Sender: TObject);
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  GetTime.ClockWindow.Clock.Time := StrToTime(DaylightStartTimeStr);
  GetTime.ClockWindow.Caption := SetDaylightStartTimeCaption;
  GetTime.ClockWindow.Visible := True;
  GetTime.ClockWindow.OKButton.SetFocus;
END; { SetDaylightStartTime }

PROCEDURE TMainWindow.SetDaylightEndTime(Sender: TObject);
{ The result from this action is picked up in the TClockWindow.OKButtonClick routine in the GetTime unit }
BEGIN
  GetTime.ClockWindow.Clock.Time := StrToTime(DaylightEndTimeStr);
  GetTime.ClockWindow.Caption := SetDaylightEndTimeCaption;
  GetTime.ClockWindow.Visible := True;
  GetTime.ClockWindow.OKButton.SetFocus;
END; { SetDaylightEndTime }

PROCEDURE TMainWindow.GeneralPopupRunClockNormallyClick(Sender: TObject);
BEGIN
  GeneralPopupRunClockNormally.Checked := True;
  GeneralPopupRunClockSlower.Checked := False;
  GeneralPopupRunClockFaster.Checked := False;
  GeneralPopupRunClockFastest.Checked := False;
  SetRailwayTimeInterval(Normal);
END; { GeneralPopupRunClockNormallyClick }

PROCEDURE TMainWindow.GeneralPopupRunClockSlowerClick;
BEGIN
  GeneralPopupRunClockNormally.Checked := False;
  GeneralPopupRunClockSlower.Checked := True;
  GeneralPopupRunClockFaster.Checked := False;
  GeneralPopupRunClockFastest.Checked := False;
  SetRailwayTimeInterval(Slower);
END; { GeneralPopupRunClockSlowerExecute }

PROCEDURE TMainWindow.GeneralPopupRunClockFasterClick;
BEGIN
  GeneralPopupRunClockNormally.Checked := False;
  GeneralPopupRunClockSlower.Checked := False;
  GeneralPopupRunClockFaster.Checked := True;
  GeneralPopupRunClockFastest.Checked := False;
  SetRailwayTimeInterval(Faster);
END; { GeneralPopupRunClockFasterClick }

PROCEDURE TMainWindow.GeneralPopupRunClockFastestClick;
BEGIN
  GeneralPopupRunClockNormally.Checked := False;
  GeneralPopupRunClockSlower.Checked := False;
  GeneralPopupRunClockFaster.Checked := False;
  GeneralPopupRunClockFastest.Checked := True;
  SetRailwayTimeInterval(Fastest);
END; { GeneralPopupRunClockFastestClick }

PROCEDURE TMainWindow.StartClock(Sender: TObject);
BEGIN
  GeneralPopupStartClock.Visible := False;
  GeneralPopupStopClock.Visible := True;
  TurnAutoModeOn;
END; { StartClock }

PROCEDURE TMainWindow.StopClock(Sender: TObject);
CONST
  UserInCharge = True;

BEGIN
  GeneralPopupStartClock.Visible := True;
  GeneralPopupStopClock.Visible := False;
  TurnAutoModeOff(UserInCharge);
END; { StopClock }

PROCEDURE TMainWindow.GeneralPopupChangePointClick(Sender: TObject);
BEGIN
  InputDialogueBoxRequired := PointDialogueBox;
  InputDialogueBox.Show;
END; { GeneralPopupChangePoint }

PROCEDURE TMainWindow.GeneralPopupChangeSignalClick(Sender: TObject);
BEGIN
  InputDialogueBoxRequired := SignalDialogueBox;
  InputDialogueBox.Show;
END; { GeneralPopupChangeSignalClick }

PROCEDURE TMainWindow.GeneralPopupListLocomotivesClick(Sender: TObject);
BEGIN
  IF LocoUtilsWindow.Visible THEN BEGIN
    LocoUtilsWindow.Visible := False;
    Log('G "?" key hides List of Locos');
  END ELSE BEGIN
    LocoUtilsWindow.Visible := True;
    Log('G "?" key makes List of Locos visible');
  END;
END; { GeneralPopupListLocomotivesClick }

PROCEDURE TMainWindow.GeneralPopupShowTrackcircuitClick(Sender: TObject);
BEGIN
  InputDialogueBoxRequired := TrackCircuitDialogueBox;
  InputDialogueBox.Show;
END; { GeneralPopupShowTrackcircuitClick }

PROCEDURE TMainWindow.GeneralPopupDebugOptionsClick(Sender: TObject);
BEGIN
  Startup.DebuggingOptionsWindow.Show;
END; { GeneralPopupDebugOptionsClick }

PROCEDURE TMainWindow.GeneralPopupResetMainWindowSizeAndPositionClick(Sender: TObject);
BEGIN
  MainWindow.Height := MulDiv(Screen.WorkAreaHeight, 80, 100);
  MainWindow.Width := Screen.WorkAreaWidth;
  MainWindow.Top := 0;
  MainWindow.Left := 0;
  InvalidateScreen(UnitRef, 'ResetMainWindowSizeClick');
END; { ResetMainWindowSizeClick }

PROCEDURE TMainWindow.MainWindowMouseWheel(Sender: TObject; ShiftState: TShiftState; WheelDelta: Integer; MousePos: TPoint; VAR Handled: Boolean);
BEGIN
  IF (LocoDialogueWindow <> NIL)
  AND (LocoDialogueWindow.Visible)
  THEN
    ControlSpeedByMouseWheel(WheelDelta, MousePos);
END;

//PROCEDURE TMainWindow.CM_EnterMenuLoop(var msg: TMessage);
//BEGIN
//  TrackCircuitPopupMenuActive := True;
////  Debug('PopupMenu entered');
//END; { CM_EnterMenuLoop }
//
//PROCEDURE TMainWindow.CM_ExitMenuLoop(var msg: TMessage);
//BEGIN
//  TrackCircuitPopupMenuActive := False;
////  Debug('PopMenu exited');
//END; { CM_ExitMenuLoop }
//
//PROCEDURE TMainWindow.CM_MenuClosed(var msg: TMessage);
//BEGIN
////  Debug('PopMenu closed');
//END; { CM_MenuClosed }

PROCEDURE CanvasTextOutAngle(X, Y : Integer; D : Word; S : string);
{ d is in tenths if a degree - i.e. 450 - 45 degrees }
VAR
  LogRec: TLOGFONT;     {* Storage area for font information *}
  OldFontHandle,        {* The old font handle *}
  NewFontHandle: HFONT; {* Temporary font handle *}

BEGIN
  IF Application.Terminated THEN
    Exit;

  { Get the current font information. We only want to modify the angle }
  GetObject(MainWindow.Canvas.Font.Handle, SizeOf(LogRec), Addr(LogRec));
  { Modify the angle. "The angle, in tenths of a degrees, between the base line of a character and the x-axis." (Windows API Help) }
  LogRec.lfEscapement := D;
  { Create a new font handle using the modified old font handle }
  NewFontHandle := CreateFontIndirect(LogRec);
  { Save the old font handle! We have to put it back when we are done! }
  OldFontHandle := SelectObject(MainWindow.Canvas.Handle, NewFontHandle);
  { Finally. Output the text! }
  MainWindow.Canvas.Brush.Style := bsClear;
  MainWindow.Canvas.TextOut(X, Y, S);
  { Put the font back the way we found it! }
  NewFontHandle := SelectObject(MainWindow.Canvas.Handle, OldFontHandle);
  { Delete the temporary (NewFontHandle) that we created }
  DeleteObject(NewFontHandle);
END; { CanvasTextOutAngle }

PROCEDURE HideStatusBarAndUpDownIndications;
{ Before a zoomed screen move, hide the status bar and the "up" and "down" markers }
BEGIN
  WITH MainWindow DO BEGIN
    MainWindowStatusBar.Visible := False;

    IF UpDownMarkersVisible THEN BEGIN
      UpDownMarkersVisible := False;

      WITH Canvas DO BEGIN
        Font.Color := BackgroundColour;
        Font.Height := -MulDiv(ClientHeight, MainWindowFontHeight, 1000);
        TextOut(0, ClientHeight DIV 2, 'Up');
        TextOut(ClientWidth - MainWindow.Canvas.TextWidth('Down'), ClientHeight DIV 2, 'Down');
      END; {WITH}
    END;
  END; {WITH}
END; { HideStatusBarAndUpDownIndications }

PROCEDURE ShowStatusBarAndUpDownIndications;
{ After a zoomed screen move, restore the status bar and the "up" and "down" markers }
BEGIN
  WITH MainWindow DO BEGIN
    MainWindowStatusBar.Visible := True;

    IF NOT UpDownMarkersVisible THEN BEGIN
      UpDownMarkersVisible := True;

      WITH Canvas DO BEGIN
        Font.Color := clWhite;
        Font.Height := -MulDiv(ClientHeight, MainWindowFontHeight, 1000);
        TextOut(0, ClientHeight DIV 2, 'Up');
        TextOut(ClientWidth - MainWindow.Canvas.TextWidth('Down'), ClientHeight DIV 2, 'Down');
      END; {WITH}
    END;
  END; {WITH}
END; { ShowStatusBarAndUpDownIndications }

PROCEDURE TMainWindow.WMHScroll(VAR ScrollData: TMessage);
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

PROCEDURE TMainWindow.WMVScroll(VAR ScrollData: TMessage);
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

PROCEDURE TMainWindow.MainWindowCreate(Sender: TObject);
//VAR
//PreviousDebugTime : TDateTime;
BEGIN
  TRY
    WITH MainWindow DO BEGIN
      LoadIcons;

      // Application.OnException := FWPExceptionHandler;
  Log('#1 ' + TimeToHMSZStr(Time));
      //PreviousDebugTime := Time;
      { Intercept messages to be able to use the tab key! }
      Application.OnMessage := ApplicationMessage;

      { initialize parameters }
  //    CrossHairCursor := LoadCursor(HInstance, 'CrossHair');
      Screen.Cursors[crCrossHair] := LoadCursor(HInstance, 'CrossHair');
      Screen.Cursors[crCrossHairForUpSignal] := LoadCursor(HInstance, 'CrossHair-UpSignal');
      Screen.Cursors[crCrossHairForDownSignal] := LoadCursor(HInstance, 'CrossHair-DownSignal');
      Screen.Cursors[crPointLever] := LoadCursor(HInstance, 'PointLever');

      SaveSystemStatusEmergencyOff := False;

      { Initialise lots of things before we start }
      InitialiseInitVarsUnit;

      { but now read in details from the .ini file, which may amend data from the Initvars unit }
      ReadIniFile;

      { Set up default window size }
      Position := poDesigned;

      Height := MainWindowHeight;
      Width := MainWindowWidth;
      Top := MainWindowTop;
      Left := MainWindowLeft;

      MainWindowInitialised := False;
      ResizeMap := False;

      { Set up the status bar panels }
      MainWindowStatusBar.Panels[StatusBarPanel0].Width := (MulDiv(Screen.WorkAreaWidth, 50, ZoomScaleFactor));
      MainWindowStatusBar.Panels[StatusBarPanel1].Width := (MulDiv(Screen.WorkAreaWidth, 447, ZoomScaleFactor));
      MainWindowStatusBar.Panels[StatusBarPanel2].Width := (MulDiv(Screen.WorkAreaWidth, 503, ZoomScaleFactor));

      { Set up menus - hide them until they are requested }
      MainClockMenu.Visible := False;
      MainDisplayMenu.Visible := False;
      MainFileMenu.Visible := False;
      MainHelpMenu.Visible := False;
      MainOperationsMenu.Visible := False;
      MainRunMenu.Visible := False;
      // Log('#2 ' + TimeToHMSZStr(Time) + ' ' + IntToStr(MilliSecondsBetween(Time, PreviousDebugTime)));
      // PreviousDebugTime := Time;

      // Log('#3 ' + TimeToHMSZStr(Time) + ' ' + IntToStr(MilliSecondsBetween(Time, PreviousDebugTime)));
      // PreviousDebugTime := Time;

      { Now start up the various Units - done here explicitly so we know what order it's being done in }
      InitialiseGetTimeUnit;
      InitialiseStartupUnit;
      InitialiseInitVarsUnit;
      InitialiseLenzUnit;
      InitialiseDiagramsUnit;
      InitialiseScreenDrawingVariables;
      // Log('#5 ' + TimeToHMSZStr(Time) + ' ' + IntToStr(MilliSecondsBetween(Time, PreviousDebugTime)));
      // DrawMap;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG MainWindowCreate:' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { MainWindowCreate }

PROCEDURE DrawMap;
{ Draws the track layout }
CONST
  ActiveTrain = True;
  ForceDraw = True;
  NewSignalData = True;
  ShowNums = True;
  ShowTheatreDestinationChar = True;
  StartOfLine = True;
  Up = True;

VAR
  B, I, J : Integer;
  DiagramsMissing : Boolean;
  DiagramsOK : Boolean;
  ErrorMsg : String;
  L : Integer;
  LocoDataTableOK : Boolean;
  S : Integer;
  SaveLineOldColour : Integer;
  SaveRecordLineDrawingMode : Boolean;
  ShowArea : Boolean;
  SegmentText : String;
  WindowsTaskbar: HWND;
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
        IF ShowSignalAndBufferStopNums THEN
          DrawBufferStopData(B, IntToStr(B), BufferStopNumberColour)
        ELSE
          IF ShowTheatreDestinations THEN
            DrawBufferStopData(B, BufferStops[B].BufferStop_AsTheatreDestination, BufferStopNumberColour);
      END;
    EXCEPT
      ON E : Exception DO
        Log('EG END; { DrawAllBufferStopData:' + E.ClassName +' error raised, with message: '+ E.Message);
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
    // MainWindow.Canvas.FillRect(MainWindow.Canvas.ClipRect);
    // Log('(1) ' + TimeToHMSZStr(Time));
    // PreviousDebugTime := Time;
    WITH MainWindow DO BEGIN
      WITH Canvas DO BEGIN
        { Do not record the line drawing detail each time DrawMap is called }
        SaveRecordLineDrawingMode := RecordLineDrawingMode;
//        RecordLineDrawingMode := False;

        IF NOT MainWindowInitialised THEN
          IF Screen.Cursor <> crHourGlass THEN
            ChangeCursor(crHourGlass);

        MainWindowCanvasPenWidth := Canvas.Pen.Width;

        IF (ScreenMode <> SaveScreenMode) OR NOT MainWindowInitialised THEN BEGIN
          SaveScreenMode := ScreenMode;
          CASE ScreenMode OF
            CustomWindowedScreenMode:
              BEGIN
                { If the screen has been restored to its normal size, restore the screen mode to default }
                IF (Height = MulDiv(Screen.WorkAreaHeight, 80, 100))
                AND (Width = Screen.WorkAreaWidth)
                THEN BEGIN
                  ScreenMode := DefaultWindowedScreenMode;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Screen restored to default size');
                  Log('G Main window restored to default size');
                END;
                ThinLineMode := True;
                Borderstyle := bsSizeable;
                IF NOT MainWindowStatusBar.Visible THEN
                  MainWindowStatusBar.Show;
                Log('G Main window set to user-defined size');

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
                IF NOT MainWindowStatusBar.Visible THEN
                  MainWindowStatusBar.Show;
                Log('G Main window set to default size');

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
                IF MainWindowStatusBar.Visible THEN
                  MainWindowStatusBar.Hide;
                Log('G Main window now full screen');
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
                { Use for checking trackcircuits. etc., as displays trackcircuit number on the status bar }
                IF LineThicknessInFullScreenMode = 'Thin' THEN
                  ThinLineMode := True
                ELSE
                  ThinLineMode := False;
                Borderstyle := bsNone;
                Height := Screen.DeskTopHeight;
                IF NOT MainWindowStatusBar.Visible THEN
                  MainWindowStatusBar.Show;
                Log('G Main window now full screen with border');
              END;
          END; {CASE}
        END;
        // if testregion then begin
        //   Region := CreateRectRgn(50, 50, 250, 250);
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
            WITH ZoomRect DO BEGIN
              IF (Left <> 0) OR (Top <> 0) OR (Right <> 0) OR (Bottom <> 0) THEN BEGIN
                HorzScrollBar.Position := MulDiv(1000, Left - ((Right - Left) DIV 2), ZoomScaleFactor);
                VertScrollBar.Position := MulDiv(1000, Top - ((Bottom - Top) DIV 2), ZoomScaleFactor);

                ZoomRect := Rect(0, 0, 0, 0);
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

        // Log('(2) ' + TimeToHMSZStr(Time) + ' ' + IntToStr(MilliSecondsBetween(Time, PreviousDebugTime)));
        // PreviousDebugTime := Time;

        IF NOT MainWindowInitialised THEN BEGIN
          Randomize;
          ReadInAreasDataFromDatabase; { ************ problem for all file loading if registry entry empty - no prompt to find directory they're in - 21/1/14 }
          ReadInLocationDataFromDatabase;
          SetLength(LocationOccupations, Length(Locations));
          ReadInLocoData(LocoDataTableOK);
          SetUpLineDrawingVars(1000);
          ReadInLineDataFromDatabase;
          ReadInFeedbackDataFromDatabase;
          IF NOT TrackCircuitsInitialised THEN BEGIN
            { only initialise track circuit once, as doing so a second time removes the data **** }
            TrackCircuitsInitialised := True;
            ReadInTrackCircuitDataFromDatabase;
            CheckLineConnectionsAreOK;
          END;
          ReadInPointDataFromDatabase;
          ReadInPlatformDataFromDatabase;
          ReadInSignalDataFromDatabase(NOT NewSignalData);
          ReadInRouteingExceptionsFromDatabase;
          IF NOT LocationLinesInitialised THEN BEGIN
            Log('G INITIALISING LOCATION LINES {BLANKLINEBEFORE}');
            InitialiseLocationLines;
            LocationLinesInitialised := True;
          END;
        END;

        { Position the status bar allowing for the window scrolling }
        MainWindowStatusBar.Left := 0;
        MainWindowStatusBar.Top := ClientHeight - MainWindowStatusBar.Height;
        MainWindowStatusBar.Width := ClientWidth;

        IF ResizeMap OR ReinitialiseMainWindowVariables THEN BEGIN
          SetUpLineDrawingVars(ZoomScaleFactor);
          CalculateLocationPositions(ZoomScaleFactor);
          CalculateLinePositions(ZoomScaleFactor);
          CalculateBufferStopPositions(ZoomScaleFactor);
          CalculatePointPositions;
          CalculatePlatformPositions(ZoomScaleFactor);
          CalculateSignalPositions(ZoomScaleFactor);

          IF ReinitialiseMainWindowVariables THEN
            ReinitialiseMainWindowVariables := False;
        END;
  //TransparentColorValue := BackgroundColour;
  //TransparentColor := True;
        // Log('(3a) ' + TimeToHMSZStr(Time) + ' ' + IntToStr(MilliSecondsBetween(Time, PreviousDebugTime)));
        // PreviousDebugTime := Time;
        IF NOT MainWindowInitialised
        AND NOT DiagramsCheckingInProgress
        AND LocoDataTableOK
        THEN BEGIN
          { Load feedback data and the diagrams datat and compare the data (these routines are here, as the various windows are created by this stage) }
          DiagramsCheckingInProgress := True;
          GetInitialFeedback;
          // Log('(3b) ' + TimeToHMSZStr(Time) + ' ' + IntToStr(MilliSecondsBetween(Time, PreviousDebugTime)));

          { but change the state of any that we know are out-of-use }
          ReadIniFileForTrackCircuitData;

          { and see if any are out of use because the detectors are out of use }
          NoteOutOfUseFeedbackUnitTrackCircuitsAtStartup;

          WorkingTimetableOK := False;
          DiagramsOK := False;
          IF NOT WorkingTimetableMode THEN
            Log('GG Starting without the working timetable - WorkingTimetableMode is not set')
          ELSE BEGIN
            LoadWorkingTimetable(WorkingTimetableMissing, WorkingTimetableOK);
            IF NOT WorkingTimetableMissing
            AND WorkingTimetableOK
            THEN BEGIN
              ProcessWorkingTimetable;
              ProcessDiagrams(ErrorMsg, DiagramsOK);
            END;
          END;

          IF WorkingTimetableOK THEN
            Log('GG Working timetable loaded - so starting without loading the diagrams from disc')
          ELSE
            IF NOT StartWithDiagrams THEN BEGIN
              Log('GG Starting without the diagrams loaded - StartWithDiagrams is not set');
              CheckOccupiedLinesAndDiagrams;
            END ELSE BEGIN
              InitialiseDiagramsUnit;
              ReadInDiagramsFromAccessDatabase(ErrorMsg, DiagramsMissing, DiagramsOK);
              IF DiagramsOK
              AND NOT DiagramsMissing
              THEN
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
                Log('G Diagrams loaded - StartWithoutDiagrams is not set');
            END;

          DiagramsCheckingInProgress := False;
        END;

        { Draw the individual lines }
        FOR L := 0 TO High(Lines) DO BEGIN
          WITH Lines[L] DO BEGIN
            { save the LineOldColour because otherwise DrawLine might update it erroneously }
            SaveLineOldColour := Line_OldColour;
            IF ReplayMode THEN
              DrawLine(L, Line_OldColour, ActiveTrain)
            ELSE BEGIN
              { If the line is not associated with a trackcircuit }
              IF Line_TC = UnknownTC THEN BEGIN
                IF Line_RouteSet <> UnknownRoute THEN
                  Line_CurrentColour := LineRoutedOverColour
                ELSE
                  Line_CurrentColour := SaveLineOldColour;
                DrawLine(L, Line_CurrentColour, ActiveTrain);
              END ELSE BEGIN
                { LineTC <> UnknownTC - see if it's an occupied trackcircuit etc. If in auto mode, only highlight the bits that are routed over. }
                IF NOT DisplayFlashingTrackCircuits OR TrackCircuits[Line_TC].TC_LitUp THEN
                  Line_CurrentColour := GetTrackCircuitStateColour(Line_TC)
                ELSE
                  Line_CurrentColour := BackgroundColour;

                IF Line_CurrentColour = TCUnoccupiedColour THEN BEGIN
                  { if not, see if a subroute is set over the line }
                  IF Line_RouteSet <> UnknownRoute THEN
                    Line_CurrentColour := LineRoutedOverColour
                END;
                DrawLine(L, Line_CurrentColour, ActiveTrain);
              END;
            END;
            { And restore it }
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
              OR  Signals[S].Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists
              OR  Signals[S].Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_Exists
              OR Signals[S].Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_Exists
              OR  Signals[S].Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists
              OR  Signals[S].Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_Exists
              THEN BEGIN
                Signals[S].Signal_PostColour := clLime;
                DrawSignalPost(S);
                DrawAllSignals(NOT ShowNums, NOT ShowTheatreDestinationChar);
              END;
            END;
          END ELSE
            IF ShowSignalsWithAdjacentTrackCircuits THEN BEGIN
              DrawAllSignals(ShowNums, NOT ShowTheatreDestinationChar);
              FOR S := 0 TO High(Signals) DO BEGIN
                IF NOT Signals[S].Signal_OutOfUse THEN BEGIN
                  { needs fixing - leaves TCs with signal numbers after exit **** }
                  { also needs headcodes not loco numbers (F10) turned on *** }
                  IF Signals[S].Signal_AdjacentTC <> UnknownTC THEN
                    DrawTrackCircuit(Signals[S].Signal_AdjacentTC, clLime);
                  IF Signals[S].Signal_AdjacentTC <> UnknownTC THEN
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
                      FOR L := 0 TO High(Lines) DO BEGIN
                        SegmentText := '';
                        WITH Lines[L] DO BEGIN
                          IF GetLineAdjacentSignal(L) = S THEN BEGIN
                            Font.Color := clLime;
                            IF Lines[L].Line_Location <> UnknownLocation THEN BEGIN
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
        Font.Height := -MulDiv(MainWindow.ClientHeight, MainWindowFontHeight, 1000);
        IF UpDownMarkersVisible THEN BEGIN
          TextOut(0, ClientHeight DIV 2, 'Up');
          TextOut(ClientWidth - TextWidth('Down'), ClientHeight DIV 2, 'Down');
        END;

        { Finally, some test detail }
        IF ShowAreas
        OR ShowLineDetail
        OR ShowLineNumbers
        OR ShowLinesWhereUpXValueSpecified
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

        IF NOT ShowTrackCircuits
        AND NOT ShowLineDetail
        AND NOT ShowLineNumbers
        AND NOT ShowLinesWhereUpXValueSpecified
        THEN
          DrawAllPoints;

        IF ResizeMap THEN
          ResizeMap := False;

        RecordLineDrawingMode := SaveRecordLineDrawingMode;

        IF NOT MainWindowInitialised THEN BEGIN
          { and finally... }
          MainWindowInitialised := True;
          ChangeCursor(crDefault);
          ProgramStartup := False;
          InvalidateScreen(UnitRef, 'DrawMap 4');
        END;
      END; { WITH Canvas }
    END; { WITH MainWindow }
  EXCEPT
    ON E : Exception DO
      Log('EG DrawMap: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DrawMap }

PROCEDURE TMainWindow.MainTimerTick(Sender: TObject);
{ Runs the main loop }
CONST
  ErrorMsgRequired = True;
  ForceDisplay = True;
  LightsOn = True;
  TrackCircuitOccupation = True;

VAR
  ErrorMsg : String;
  I : Integer;
  KeyOut : Boolean;
  LockingMsg : String;
  OK : Boolean;
  PointResultPending : Boolean;
  TempTrainArrayElementFound : Boolean;
  TempRoute : Integer;

  PROCEDURE CheckSystemStatus;
  { Ask for current system status and deal with abnormal states }
  VAR
    SystemStatus : SystemRec;
    TC : Integer;

  BEGIN
    TRY
      ReturnSystemStatus(SystemStatus);
      IF (SystemStatus.EmergencyOff
      AND NOT SaveSystemStatusEmergencyOff)
      THEN BEGIN
        Log('E Emergency - saving trackcircuit settings');
        FOR TC := 0 TO High(TrackCircuits) DO BEGIN
          TrackCircuits[TC].TC_EmergencyState := TrackCircuits[TC].TC_OccupationState;
          TrackCircuits[TC].TC_EmergencyLocoChip := TrackCircuits[TC].TC_LocoChip;
        END;
        IF SystemStatus.EmergencyOff THEN
          SaveSystemStatusEmergencyOff := True;
      END;

      IF (SystemStatus.EmergencyOff
         OR SystemStatus.EmergencyStop
         OR SystemStatus.EmergencyOff
         OR SystemStatus.EmergencyStop)
      AND NOT LocosStopped THEN BEGIN
        { And now stop all active trains }
        LocosStopped := True;
        TurnAutoModeOff(NOT ByUser);
        IF NOT EmergencyStopMsgDisplayed THEN BEGIN
          MessageDialogueWithDefault('All locos emergency stopped', StopTimer, mtInformation, [mbOK], ['&OK'], mbOK);
          EmergencyStopMsgDisplayed := True;
        END;
        Log('EG ALL LOCOS EMERGENCY STOPPED');
      END;

      IF LocosStopped
      AND NOT SystemStatus.EmergencyOff
      AND NOT SystemStatus.EmergencyStop
      AND NOT SystemStatus.EmergencyOff
      AND NOT SystemStatus.EmergencyStop
      THEN BEGIN
        { Emergency over - can now reload trackcircuit data after a short wait, then restart trains slowly }
        IF SaveSystemStatusEmergencyOff THEN BEGIN
          SaveSystemStatusEmergencyOff := False;
          EmergencyStopMsgDisplayed := False;
          StartSystemTimer;

          { The problem is that, after a restart, the trackcircuit info comes in automatically - we want to check it only once it has arrived - as there is no way to tell
            when it has finished arriving, the answer seems to be to allow a five second delay.
          }
          PostEmergencyTime := IncSecond(Time, 5);
          PostEmergencyTimeSet := True;
          DrawMap;
        END;
      END;
      IF PostEmergencyTimeSet
      AND (Time >= PostEmergencyTime)
      THEN BEGIN
        { Restore the trackcircuit data after an emergency (but wait a short time before doing so, as the feedback system will read in the feedback data again after a system
          reset, and we need to restore our data after that). Do we need to save and restore other TC data (like PreviousTCstate)? ****
        }
        PostEmergencyTimeSet := False;
        Log('EG Emergency over - restoring trackcircuit data');
        FOR TC := 0 TO High(TrackCircuits) DO BEGIN
          IF (TrackCircuits[TC].TC_OccupationState <> TrackCircuits[TC].TC_EmergencyState)
          THEN BEGIN
            IF TrackCircuits[TC].TC_OccupationState = TCFeedbackOccupation THEN BEGIN
              IF TrackCircuits[TC].TC_EmergencyState = TCPermanentFeedbackOccupation THEN
                TrackCircuits[TC].TC_OccupationState := TCPermanentFeedbackOccupation;
            END ELSE BEGIN
              IF TrackCircuits[TC].TC_EmergencyState = TCFeedbackOccupation THEN
                { don't want to restore feedback occupation when there's no current feedback }
                Log('E Not restoring TC=' + IntToStr(TC)
                       + ' from ' + TrackCircuitStateToStr(TrackCircuits[TC].TC_OccupationState)
                       + ' to ' + TrackCircuitStateToStr(TrackCircuits[TC].TC_EmergencyState)
                       + ' as there is no current feedback there')
              ELSE BEGIN
                Log('E Restoring TC=' + IntToStr(TC)
                       + ' from ' + TrackCircuitStateToStr(TrackCircuits[TC].TC_OccupationState)
                       + ' to ' + TrackCircuitStateToStr(TrackCircuits[TC].TC_EmergencyState));
                TrackCircuits[TC].TC_OccupationState := TrackCircuits[TC].TC_EmergencyState;
              END;
            END;
          END;
          IF TrackCircuits[TC].TC_LocoChip <> TrackCircuits[TC].TC_EmergencyLocoChip THEN BEGIN
            Log('E Restoring TC=' + IntToStr(TC) + ' locochip was ' + IntToStr(TrackCircuits[TC].TC_LocoChip)
                   + '; is now ' + IntToStr(TrackCircuits[TC].TC_EmergencyLocoChip));
            TrackCircuits[TC].TC_LocoChip := TrackCircuits[TC].TC_EmergencyLocoChip;
          END;
        END;
        DrawMap;
      END;
    EXCEPT
      ON E : Exception DO
        Log('EG END; { CheckSystemStatus:' + E.ClassName +' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { CheckSystemStatus }

  PROCEDURE CheckStationStartMode;
  { See if station start mode is being turned on or off - press button held down for five seconds does it }
//  VAR
//    Location : Integer;

  BEGIN
    IF (StationStartModeSetUpTime <> 0)
    AND (Time > IncSecond(StationStartModeSetUpTime, 5))
    THEN BEGIN
      StationStartModeSetUpTime := 0;
      IF StationStartMode THEN BEGIN
        StationStartMode := False;
        Log('GG Station Start Mode = off');
      END ELSE BEGIN
        StationStartMode := True;
        Log('G! Station Start Mode = on');
        { and set all button presses to false }
//        FOR Location := FirstMainPlatformLocation TO LastMainPlatformLocation DO
//          MainPlatformPlungers[Location].TRSPlunger_Pressed := False;
      END;
    END;
  END; { CheckStationStartMode }

BEGIN
  TRY
    IF RunTestUnitOnStartup THEN BEGIN
      Debug('Running test unit on startup');
      TestProc(KeyOut);
      RunTestUnitOnStartup := False;
    END;

    IF TimerT = NIL THEN
      TimerT := TrainList;

    { See if any rectangles need to be undrawn }
    IF TimeRectangleDrawn > 0 THEN BEGIN
      IF ((GetTickCount - TimeRectangleDrawn) > MaxRectangleUndrawTime) THEN BEGIN
        DrawRectangularOutline(UndrawRect, SaveUndrawRectColour, UndrawRequired, NOT UndrawToBeAutomatic);
        TimeRectangleDrawn := 0;
      END;
    END;

    CheckSystemStatus;

    DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 1');

    IF InAutoMode THEN
      MoveAllTrains;

    { See if any point changes are pending - i.e. we're waiting for feedback that confirms the change }
    CheckPointsAwaitingFeedback;

    { See if any trains have strayed }
    IF InAutoMode THEN
      LookOutForStrayingTrains;

    DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 2');

    IF InAutoMode THEN BEGIN
      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 3');
      MoveAllTrains;

      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 4');
      CheckTrainsHaveArrived;

      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 5');
      CheckTrainsHaveDeparted;

      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 6');
      MoveAllTrains;

      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 7');
      CheckTrainsReadyToDepart;
    END;

    { On each tick go through one of the routes currently active, and continue route setting if required }
    IF NOT RouteingSuspendedWhenStopPressed
    AND NOT RouteingSuspendedForModalDialogue
    THEN BEGIN
      IF Length(Routes_Routes) > 0 THEN BEGIN
        { Work out which route we're doing on this iteration }
        Inc(NumbersArrayCounter);
        IF NumbersArrayCounter > High(Routes_Routes) THEN
          { Reset the counter if it's too high }
          NumbersArrayCounter := 0;
        TempRoute := Routes_Routes[NumbersArrayCounter];

        { Set up routes }
        IF Routes_RouteSettingsInProgress[TempRoute] THEN
          { there is some route setting to be done }
          SetUpASubRoute(TempRoute);

        IF InAutoMode THEN BEGIN
          DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 8');
          MoveAllTrains;
        END;

        { Clear routes }
        IF InAutoMode OR RouteingByUser OR True THEN
          IF Routes_RouteClearingsInProgress[TempRoute] THEN
            { there is some route clearing to be done }
            ClearARoute(TempRoute);

  //      IF InAutoMode THEN
  //        TestClearARoute(TempRoute);

        IF InAutoMode THEN BEGIN
          DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 9');
          MoveAllTrains;
        END;

        { and set any signals needed because of approach control being in operation }
        IF Routes_ApproachControlsSet[TempRoute] THEN
          ProcessApproachLockedSignals(TempRoute);
      END;
    END;

    { And see if any points need to be reset (this is done here as sometimes points do not otherwise reset, because although the route can be reset, the point may still be
      locked at that stage).
    }
    IF InAutoMode OR ResetAllSwitchedPoints THEN BEGIN
      IF NOT PointResettingMode THEN
        SetLength(PointResettingToDefaultStateArray, 0)
      ELSE BEGIN
//        IF CompareTime(IncSecond(LastPointResetTime, 20), Time) < 0 THEN BEGIN
          I := 0;
          WHILE I <= High(PointResettingToDefaultStateArray) DO BEGIN
            IF NOT PointIsLocked(PointResettingToDefaultStateArray[I], LockingMsg) THEN BEGIN
              WITH Points[PointResettingToDefaultStateArray[I]] DO BEGIN
                IF Point_PresentState = Point_RequiredState THEN
                  DeleteElementFromIntegerArray(PointResettingToDefaultStateArray, I)
                ELSE BEGIN
                  IF Point_ResettingTime = 0 THEN
                    Point_ResettingTime := Time
                  ELSE
                    IF (Point_ResettingTime <> 0)
                    AND (CompareTime(IncSecond(Point_ResettingTime, 5), Time) < 0)
                    THEN BEGIN
                      Log('P Resetting P=' + IntToStr(PointResettingToDefaultStateArray[I]) + ' one minute after unlocking');
                      PullPoint(PointResettingToDefaultStateArray[I], NoLocoChip, NoRoute, NoSubRoute, NOT ForcePoint, NOT ByUser,
                                NOT ErrorMsgRequired, PointResultPending, ErrorMsg, OK);
//                      IF OK THEN
//                        LastPointResetTime := Time;

                      Point_ResettingTime := 0;
                      DeleteElementFromIntegerArray(PointResettingToDefaultStateArray, I);
                    END;
                END;
              END; {WITH}
            END;
            Inc(I);
          END; {WHILE}
//        END;
      END;
    END;

    IF InAutoMode THEN BEGIN
      { See if any train lights are due to be switched on }
      I := 0;
      WHILE I <= High(LightsToBeSwitchedOnArray) DO BEGIN
        WITH LightsToBeSwitchedOnArray[I] DO BEGIN
          WITH LightsToBeSwitchedOn_Train^ DO BEGIN
            IF CurrentRailwayTime >= LightsToBeSwitchedOn_SwitchOnTime THEN BEGIN
              Log(Train_LocoChipStr + ' L Now switching on ' + LightsToBeSwitchedOn_ColourStr1 + ' lights at up'
                                    + ' and ' + LightsToBeSwitchedOn_ColourStr2 + ' lights at down');
              SetTrainDirection(LightsToBeSwitchedOn_Train, LightsToBeSwitchedOn_Direction1, ForceAWrite, OK);
              IF LightsToBeSwitchedOn_Direction2 <> UnknownDirection THEN
                SetTwoLightingChips(Train_LocoChip, LightsToBeSwitchedOn_Direction1, LightsToBeSwitchedOn_Direction2, LightsOn);
              TurnLightsOn(Train_LocoChip, OK);

              IF Train_HasCabLights
              AND CabLightsAreOn(Train_LocoChip)
              THEN
                TurnCablightsOff(Train_LocoChip);

              DeleteElementFromLightsToBeSwitchedOnArray(I);
            END;
          END; {WITH}
        END; {WITH}
        Inc(I);
      END; {WHILE}
    END;

    IF InAutoMode THEN BEGIN
      IF NOT RouteClearingOnlyMode
      AND NOT RouteingSuspendedForModalDialogue
      THEN BEGIN
        { Create routes that need creating, but don't do so if RouteClearingOnly is on (mode whereby routes are automatically cleared when train pass, but no routes are
          created - problem with the mode though - doesn't clear the system-occupied TCs when the train passes **** FWP 16/10/06)
        }
        IF TimerT = NIL THEN
          { cycle through the trains afresh, if there are any }
          TimerT := TrainList;

        TRY
          IF TimerT <> NIL THEN BEGIN
            IF TimerT^.Train_DiagramFound THEN BEGIN
              { this is temporarily here to check for mad address errors FWP 14/2/08 **** }
              TempTrainArrayElementFound := False;
              I := 0;
              WHILE I <= High(TempTrainArray) DO BEGIN
                IF TimerT = TempTrainArray[I] THEN
                  TempTrainArrayElementFound := True;
                Inc(I);
              END; {WHILE}
              IF NOT TempTrainArrayElementFound THEN BEGIN
                Debug;
                { A permanent breakpoint in case the existing one gets moved or lost: }
                IF DebugHook <> 0 THEN BEGIN
                  ASM
                    Int 3
                  END; {ASM}
                END;
              END;

              CreateRouteArraysForTrain(TimerT);
            END;
            TimerT := TimerT^.Train_NextRecord;
          END;
        EXCEPT
          ON E : Exception DO
            Log('EG Raildraw timer: ' + E.ClassName +' error raised, with message: '+ E.Message);
        END; {TRY}
      END;

      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 10');
      MoveAllTrains;

      { If any train has passed each trackcircuit, the subroute can be released }
      ReleaseSubRoutes;

      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 11');
      MoveAllTrains;

      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 12');
      MoveAllTrains;

      DoCheckForUnexpectedData(UnitRef, 'MainTimerTick 13');
      MoveAllTrains;

      IF ReplayMode THEN BEGIN
        IF ReplayScrollDown THEN
          AdvanceLogFileByOneLine
        ELSE
          IF ReplayScrollUp THEN
            ReverseLogFileByOneLine
          ELSE
            IF ReplaySignalChangeSearch THEN
              AdvanceLogFileByOneLine;
      END;

      CheckStationStartMode;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG MainTimerTick: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { MainTimerTick }

INITIALIZATION

END { RailDraw }.
