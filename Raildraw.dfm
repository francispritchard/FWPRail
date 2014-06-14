object FWPRailWindow: TFWPRailWindow
  Left = 606
  Top = 560
  HorzScrollBar.Tracking = True
  VertScrollBar.Tracking = True
  Caption = 'FWP'#39's Railway Program'
  ClientHeight = 303
  ClientWidth = 732
  Color = clBtnFace
  Constraints.MinHeight = 132
  Constraints.MinWidth = 340
  UseDockManager = True
  DefaultMonitor = dmDesktop
  DockSite = True
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  HelpFile = 'rail'
  Menu = FWPRailWindowMenu
  OldCreateOrder = False
  PopupMenu = GeneralPopupMenu
  Position = poScreenCenter
  Visible = True
  OnClose = FWPRailWindowClose
  OnDestroy = FWPRailWindowDestroy
  OnDragDrop = FWPRailWindowDragDrop
  OnDragOver = FWPRailWindowDragOver
  OnKeyDown = FWPRailWindowKeyDown
  OnMouseDown = FWPRailWindowMouseDown
  OnMouseMove = FWPRailWindowMouseMove
  OnMouseUp = FWPRailWindowMouseUp
  OnMouseWheel = FWPRailWindowMouseWheel
  OnPaint = FWPRailWindowPaint
  OnResize = FWPRailWindowResize
  OnShortCut = FWPRailWindowShortCut
  PixelsPerInch = 96
  TextHeight = 13
  object FWPRailWindowStatusBar: TStatusBar
    Left = 0
    Top = 0
    Width = 656
    Height = 19
    Align = alCustom
    Panels = <
      item
        Alignment = taCenter
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
    OnDblClick = FWPRailWindowStatusBarDblClick
    OnMouseMove = FWPRailWindowStatusBarMouseMove
  end
  object FWPRailWindowMenu: TMainMenu
    Left = 60
    Top = 48
    object MainFileMenu: TMenuItem
      Caption = '&File'
      Visible = False
      object MainFileMenuExit: TMenuItem
        Caption = 'E&xit'
        OnClick = FWPRailWindowExitClick
      end
    end
    object MainOperationsMenu: TMenuItem
      Caption = '&Operations'
      Visible = False
      object MainOperationsMenuDriveLocomotive: TMenuItem
        Caption = '&Drive Locomotive'
        OnClick = MainOperationsMenuDriveLocomotiveClick
      end
      object ChangePoint: TMenuItem
        Caption = 'Change &Point'
        OnClick = GeneralPopupChangePointClick
      end
      object MainOperationsMenuChangeSignal: TMenuItem
        Caption = 'Change &Signal'
        OnClick = GeneralPopupChangeSignalClick
      end
      object MainOperationsMenuListLocomotives: TMenuItem
        Caption = '&List Locomotives'
        OnClick = GeneralPopupListLocomotivesClick
      end
      object MainOperationsMenuShowTrackCircuit: TMenuItem
        Caption = 'Show &Trackcircuit'
        OnClick = GeneralPopupShowTrackcircuitClick
      end
      object MainOperationsMenuDebugOptions: TMenuItem
        Caption = '&Debug Options'
        OnClick = GeneralPopupDebugOptionsClick
      end
    end
    object MainClockMenu: TMenuItem
      Caption = '&Clock'
      Visible = False
      object MainClockMenuStartClock: TMenuItem
        Caption = 'St&art Clock'
        OnClick = StartClock
      end
      object MainMenuStopClock: TMenuItem
        Caption = 'St&op Clock'
        Visible = False
        OnClick = StopClock
      end
      object MainClockMenuRuler1: TMenuItem
        Caption = '-'
      end
      object MainClockMenuRunTimeNormally: TMenuItem
        AutoCheck = True
        Caption = 'Run Clock Normally'
        Checked = True
        OnClick = GeneralPopupRunClockNormallyClick
      end
      object MainClockMenuRunTimeFaster: TMenuItem
        AutoCheck = True
        Caption = 'Run Clock Faster'
        OnClick = GeneralPopupRunClockFasterClick
      end
      object MainClockMenuRunClockFastest: TMenuItem
        Caption = 'Run Clock Fastest'
        OnClick = GeneralPopupRunClockFastestClick
      end
      object MainClockMenuRunTimeSlower: TMenuItem
        AutoCheck = True
        Caption = 'Run Clock Slower'
        OnClick = GeneralPopupRunClockSlowerClick
      end
      object MainClockMenuRuler2: TMenuItem
        Caption = '-'
      end
      object MainClockMenuSetCurrentRailwayTime: TMenuItem
        Caption = 'Set Current Railway Time'
        OnClick = SetCurrentRailwayTime
      end
      object MainClockMenuSetStartupTime: TMenuItem
        Caption = 'Set &Program Start Time'
      end
      object SetDayLightStart: TMenuItem
        Caption = 'Set Daylight Start Time'
      end
      object SetDaylightEnd: TMenuItem
        Caption = 'Set Daylight End Time'
      end
    end
    object MainDisplayMenu: TMenuItem
      Caption = '&Display'
      object MainDisplayMenuDebug: TMenuItem
        Caption = 'Show De&bug Output Window'
        Checked = True
        OnClick = MainDisplayMenuDebugClick
      end
      object MainDisplayMenuShowStatusbar: TMenuItem
        Caption = 'Show Status &Bar'
        Checked = True
        OnClick = ShowStatusBarClick
      end
      object MainDisplayMenuShow: TMenuItem
        AutoCheck = True
        Caption = 'Show &Main Menu'
        OnClick = MainDisplayMenuShowClick
      end
      object MainDisplayMenuDiagramsWindow: TMenuItem
        Caption = '&Diagrams Window'
        Checked = True
        OnClick = MainDisplayMenuDiagramsWindowClick
      end
      object MainDisplayMenuWorkingTimetableWindow: TMenuItem
        Caption = '&Working Timetable Window'
        OnClick = MainDisplayMenuWorkingTimetableWindowClick
      end
      object MainDisplayMenuZoom: TMenuItem
        Caption = '&Zoom Screen'
        OnClick = MainDisplayMenuZoomClick
      end
    end
    object MainRunMenu: TMenuItem
      Caption = '&Run'
      Visible = False
      object MainRunMenuResumeOperations: TMenuItem
        Caption = '&Resume Operations'
        OnClick = MainRunMenuResumeOperationsClick
      end
      object MainRunMenuHaltOperations: TMenuItem
        Caption = '&Halt All Operations'
      end
    end
    object MainHelpMenu: TMenuItem
      Caption = 'Help'
      Hint = #39'Isn'#39#39't'#39' FWP wonderful?'#39
      Visible = False
      object MainHelpMenuRailHelp: TMenuItem
        Caption = 'Rail Help'
        OnClick = MainHelpMenuRailHelpClick
      end
      object MainHelpMenuAboutRail: TMenuItem
        Caption = 'About Rail'
        OnClick = HelpMenuAboutClick
      end
    end
  end
  object FlashTimer: TTimer
    Interval = 500
    OnTimer = FlashTimerTick
    Left = 36
    Top = 100
  end
  object GeneralPopupMenu: TPopupMenu
    AutoPopup = False
    OnPopup = GeneralPopupMenuOnPopup
    Left = 142
    Top = 100
    object GeneralPopupShowMainMenu: TMenuItem
      Caption = '&Show Main Menus'
      OnClick = GeneralPopupShowMainMenuClick
    end
    object GeneralPopupClock: TMenuItem
      Caption = 'Cloc&k'
      OnClick = GeneralPopupClockClick
      object GeneralPopupStartClock: TMenuItem
        Caption = 'St&art Clock'
        OnClick = StartClock
      end
      object GeneralPopupStopClock: TMenuItem
        Caption = 'St&op Clock'
        Visible = False
        OnClick = StopClock
      end
      object GeneralPopupRuler1: TMenuItem
        Caption = '-'
      end
      object GeneralPopupRunClockNormally: TMenuItem
        AutoCheck = True
        Caption = 'Run Clock Normally'
        Checked = True
        OnClick = GeneralPopupRunClockNormallyClick
      end
      object GeneralPopupRunClockSlower: TMenuItem
        AutoCheck = True
        Caption = 'Run Clock Slower'
        OnClick = GeneralPopupRunClockSlowerClick
      end
      object GeneralPopupRunClockFaster: TMenuItem
        AutoCheck = True
        Caption = 'Run Clock Faster'
        OnClick = GeneralPopupRunClockFasterClick
      end
      object GeneralPopupRunClockFastest: TMenuItem
        AutoCheck = True
        Caption = 'Run Clock Fastest'
        OnClick = GeneralPopupRunClockFastestClick
      end
      object GeneralPopupRuler2: TMenuItem
        Caption = '-'
      end
      object GeneralPopupSetCurrentRailwayTime: TMenuItem
        Caption = 'Set Current Railway Time'
        OnClick = SetCurrentRailwayTime
      end
      object GeneralPopupSetCurrentRailwayDayOfTheWeek: TMenuItem
        Caption = 'Set Current Railway Day Of The Week'
        OnClick = GeneralPopupSetCurrentRailwayDayOfTheWeekClick
      end
      object GeneralPopupSetProgramStartTime: TMenuItem
        Caption = 'Set Program Start Time'
      end
      object GeneralPopupSetDaylightStartTime: TMenuItem
        Caption = 'Set Daylight Start Time'
      end
      object GeneralPopupSetDaylightEndTime: TMenuItem
        Caption = 'Set Daylight End Time'
      end
    end
    object GeneralPopupChangeColours: TMenuItem
      Caption = '&Change Colours'
      OnClick = GeneralPopupChangeColoursClick
      object GeneralPopupBackgroundColour: TMenuItem
        Caption = '&Background Colour'
        object GeneralPopupChangeBackgroundColour: TMenuItem
          Caption = 'Change Background Colour'
          OnClick = GeneralPopupChangeBackgroundColourClick
        end
        object GeneralPopupRestoreDefaultBackgroundColour: TMenuItem
          Caption = 'Restore Default Background Colour'
          OnClick = GeneralPopupRestoreDefaultBackgroundColourClick
        end
      end
      object GeneralPopupForegroundColour: TMenuItem
        Caption = '&Foreground Colour'
        object GeneralPopupChangeForegroundColour: TMenuItem
          Caption = 'Change Foreground Colour'
          OnClick = GeneralPopupChangeForegroundColourClick
        end
        object GeneralPopupRestoreForegroundColour: TMenuItem
          Caption = 'Restore Default Foreground Colour'
          OnClick = GeneralPopupRestoreForegroundColourClick
        end
      end
      object GeneralPopupRuler3: TMenuItem
        Caption = '-'
      end
      object GeneralPopupBufferStopColours: TMenuItem
        Caption = 'B&uffer Stop Colours'
        object GeneralPopupBufferStopColour: TMenuItem
          Caption = 'Buffer Stop Colour'
          object GeneralPopupChangeBufferStopColour: TMenuItem
            Caption = 'Change Buffer Stop Colour'
            OnClick = GeneralPopupChangeBufferStopColourClick
          end
          object GeneralPopupRestoreBufferStopColour: TMenuItem
            Caption = 'Restore Buffer Stop Colour'
            OnClick = GeneralPopupRestoreBufferStopColourClick
          end
        end
        object GeneralPopupBufferStopNumberColour: TMenuItem
          Caption = 'Buffer Stop Number Colour'
          object GeneralPopupChangeBufferStopNumberColour: TMenuItem
            Caption = 'Change Buffer Stop Number Colour'
            OnClick = GeneralPopupChangeBufferStopNumberColourClick
          end
          object GeneralPopupRestoreBufferStopNumberColour: TMenuItem
            Caption = 'Restore Buffer Stop Number Colour'
            OnClick = GeneralPopupRestoreBufferStopNumberColourClick
          end
        end
        object GeneralPopupBufferStopRed: TMenuItem
          Caption = 'Buffer Stop Red'
          object GeneralPopupChangeBufferStopRed: TMenuItem
            Caption = 'Change Buffer Stop Red'
            OnClick = GeneralPopupChangeBufferStopRedClick
          end
          object GeneralPopupRestoreBufferStopRed: TMenuItem
            Caption = 'Restore Buffer Stop Red'
            OnClick = GeneralPopupRestoreBufferStopRedClick
          end
        end
      end
      object GeneralPopupLineColours: TMenuItem
        Caption = '&Line && TrackCircuit Colours'
        object GeneralPopupDefaultLineColourTCUnoccupiedColourMessage: TMenuItem
          Caption = 'NB: Default Line Colour = TCUnoccupiedColour'
        end
        object GeneralPopupRuler6: TMenuItem
          Caption = '-'
        end
        object GeneralPopupLineRoutedOverColour: TMenuItem
          Caption = 'Line Routed Over Colour'
          object ChangeLineRoutedOverColour1: TMenuItem
            Caption = 'Change Line RoutedOver Colour'
            OnClick = GeneralPopupChangeLineRoutedOverColourClick
          end
          object RestoreLineRoutedOverColour1: TMenuItem
            Caption = 'Restore Line Routed Over Colour'
            OnClick = GeneralPopupRestoreLineRoutedOverColourClick
          end
        end
        object GeneralPopupLineNotAvailableColour: TMenuItem
          Caption = 'Line Not Available Colour'
          object GeneralPopupChangeLineNotAvailableColour: TMenuItem
            Caption = 'Change Line Not Available Colour'
            OnClick = GeneralPopupChangeLineNotAvailableColourClick
          end
          object GeneralPopupRestoreLineNotAvailableColour: TMenuItem
            Caption = 'Restore Line Not Available Colour'
            OnClick = GeneralPopupRestoreLineNotAvailableColourClick
          end
        end
        object GeneralPopupLocoStalledColour: TMenuItem
          Caption = 'Loco Stalled Colour'
          object GeneralPopupChangeLocoStalledColour: TMenuItem
            Caption = 'Change Loco Stalled Colour'
            OnClick = GeneralPopupChangeLocoStalledColourClick
          end
          object GeneralPopupRestoreLocoStalledColour: TMenuItem
            Caption = 'Restore LocoStalled Colour'
            OnClick = GeneralPopupRestoreLocoStalledColourClick
          end
        end
        object GeneralPopupTCFeedbackOccupationColour: TMenuItem
          Caption = 'TC Feedback Occupation Colour'
          object GeneralPopupChangeTCFeedbackOccupationColour: TMenuItem
            Caption = 'Change TC Feedback Occupation Colour'
            OnClick = GeneralPopupChangeTCFeedbackOccupationColourClick
          end
          object GeneralPopupRestoreTCFeedbackOccupationColour: TMenuItem
            Caption = 'Restore TC Feedback Occupation Colour'
            OnClick = GeneralPopupRestoreTCFeedbackOccupationColourClick
          end
        end
        object GeneralPopupTCFeedbackOccupationButOutOfUseColour: TMenuItem
          Caption = 'TC Feedback Occupation But Out Of Use Colour'
          object GeneralPopupChangeTCFeedbackOccupationButOutOfUseColour: TMenuItem
            Caption = 'Change TC Feedback Occupation But Out Of Use Colour'
            OnClick = GeneralPopupChangeTCFeedbackOccupationButOutOfUseColourClick
          end
          object GeneralPopupRestoreTCFeedbackOccupationButOutOfUseColour: TMenuItem
            Caption = 'Restore TC Feedback Occupation But Out Of UseColour'
            OnClick = GeneralPopupRestoreTCFeedbackOccupationButOutOfUseColourClick
          end
        end
        object GeneralPopupTCFeedbackDataInUseColour: TMenuItem
          Caption = 'TC Feedback Data In Use Colour'
          object GeneralPopupChangeTCFeedbackDataInUseColour: TMenuItem
            Caption = 'Change TC Feedback Data In Use Colour'
            OnClick = GeneralPopupChangeTCFeedbackDataInUseColourClick
          end
          object GeneralPopupRestoreTCFeedbackDataInUseColour: TMenuItem
            Caption = 'Restore TC Feedback Data In Use Colour'
            OnClick = GeneralPopupRestoreTCFeedbackDataInUseColourClick
          end
        end
        object GeneralPopupTCFeedbackDataOutOfUseColour: TMenuItem
          Caption = 'TC Feedback Data Out Of Use Colour'
          object GeneralPopupChangeTCFeedbackDataOutOfUseColour: TMenuItem
            Caption = 'Change TC Feedback Data Out Of Use Colour'
            OnClick = GeneralPopupChangeTCFeedbackDataOutOfUseColourClick
          end
          object GeneralPopupRestoreTCFeedbackDataOutOfUseColour: TMenuItem
            Caption = 'Restore TC Feedback Data Out Of Use Colour'
            OnClick = GeneralPopupRestoreTCFeedbackDataOutOfUseColourClick
          end
        end
        object GeneralPopupTCMissingOccupationColour: TMenuItem
          Caption = 'TC Missing Occupation Colour'
          object GeneralPopupChangeTCMissingOccupationColour: TMenuItem
            Caption = 'Change TC Missing Occupation Colour'
            OnClick = GeneralPopupChangeTCMissingOccupationColourClick
          end
          object GeneralPopupRestoreTCMissingOccupationColour: TMenuItem
            Caption = 'Restore TC Missing Occupation Colour'
            OnClick = GeneralPopupRestoreTCMissingOccupationColourClick
          end
        end
        object GeneralPopupTCPermanentFeedbackOccupationColour: TMenuItem
          Caption = 'TC Permanent Feedback Occupation'
          object GeneralPopupChangeTCPermanentFeedbackOccupationColour: TMenuItem
            Caption = 'Change TC Permanent Feedback Occupation Colour'
            OnClick = GeneralPopupChangeTCPermanentFeedbackOccupationColourClick
          end
          object GeneralPopupRestoreTCPermanentFeedbackOccupationColour: TMenuItem
            Caption = 'Restore TC Permanent Feedback Occupation Colour'
            OnClick = GeneralPopupRestoreTCPermanentFeedbackOccupationColourClick
          end
        end
        object GeneralPopupTCPermanentOccupationSetByUserColour: TMenuItem
          Caption = 'TC Permanent Occupation Set By User Colour'
          object GeneralPopupChangeTCPermanentOccupationSetByUserColour: TMenuItem
            Caption = 'Change TC Permanent Occupation Set By User Colour'
            OnClick = GeneralPopupChangeTCPermanentOccupationSetByUserColourClick
          end
          object GeneralPopupRestoreTCPermanentOccupationSetByUserColour: TMenuItem
            Caption = 'Restore TC Permanent Occupation Set By User Colour'
            OnClick = GeneralPopupRestoreTCPermanentOccupationSetByUserColourClick
          end
        end
        object GeneralPopupTCPermanentSystemOccupation: TMenuItem
          Caption = 'TC Permanent System Occupation'
          object GeneralPopupChangeTCPermanentSystemOccupationColour: TMenuItem
            Caption = 'Change TC Permanent System Occupation Colour'
            OnClick = GeneralPopupChangeTCPermanentSystemOccupationColourClick
          end
          object GeneralPopupRestoreTCPermanentSystemOccupationColour: TMenuItem
            Caption = 'Restore TC Permanent System Occupation Colour'
            OnClick = GeneralPopupRestoreTCPermanentSystemOccupationColourClick
          end
        end
        object GeneralPopupTCLocoOutOfPlaceColour: TMenuItem
          Caption = 'TC Loco Out Of Place Colour'
          object GeneralPopupChangeTCLocoOutOfPlaceColour: TMenuItem
            Caption = 'Change TC Loco Out Of Place Colour'
            OnClick = GeneralPopupChangeTCLocoOutOfPlaceColourClick
          end
          object GeneralPopupRestoreTCLocoOutOfPlaceColour: TMenuItem
            Caption = 'Restore TC Loco Out Of PlaceColour'
            OnClick = GeneralPopupRestoreTCLocoOutOfPlaceColourClick
          end
        end
        object GeneralPopupTCSpeedRestrictionColour: TMenuItem
          Caption = 'TC Speed Restriction Colour'
          object GeneralPopupChangeTCSpeedRestrictionColour: TMenuItem
            Caption = 'Change TC Speed Restriction Colour'
            OnClick = GeneralPopupChangeTCSpeedRestrictionColourClick
          end
          object GeneralPopupRestoreTCSpeedRestrictionColour: TMenuItem
            Caption = 'Restore TC Speed Restriction Colour'
            OnClick = GeneralPopupRestoreTCSpeedRestrictionColourClick
          end
        end
        object GeneralPopupTCSystemOccupationColour: TMenuItem
          Caption = 'TC System Occupation Colour'
          object GeneralPopupChangeTCSystemOccupationColour: TMenuItem
            Caption = 'Change TC System Occupation Colour'
            OnClick = GeneralPopupChangeTCSystemOccupationColourClick
          end
          object GeneralPopupRestoreTCSystemOccupationColour: TMenuItem
            Caption = 'Restore TC System Occupation Colour'
            OnClick = GeneralPopupRestoreTCSystemOccupationColourClick
          end
        end
        object GeneralPopupTCOutOfUseSetByUserColour: TMenuItem
          Caption = 'TC Out Of Use Set By User Colour'
          object GeneralPopupChangeTCOutOfUseSetByUserColour: TMenuItem
            Caption = 'Change TC Out Of Use Set By User Colour'
            OnClick = GeneralPopupChangeTCOutOfUseSetByUserColourClick
          end
          object GeneralPopupRestoreTCOutOfUseSetByUserColour: TMenuItem
            Caption = 'Restore TC Out Of Use Set By User Colour'
            OnClick = GeneralPopupRestoreTCOutOfUseSetByUserColourClick
          end
        end
        object GeneralPopupTCOutOfUseAsNoFeedbackReceivedColour: TMenuItem
          Caption = 'TC Out Of Use As No Feedback Received Colour'
          object GeneralPopupChangeTCOutOfUseAsNoFeedbackReceivedColour: TMenuItem
            Caption = 'Change TC Out Of Use As No Feedback Received Colour'
            OnClick = GeneralPopupChangeTCOutOfUseAsNoFeedbackReceivedColourClick
          end
          object GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedColour: TMenuItem
            Caption = 'Restore TC Out Of Use As No Feedback Received Colour'
            OnClick = GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedColourClick
          end
        end
        object GeneralPopupTCUnoccupiedColour: TMenuItem
          Caption = 'TC Unoccupied Colour'
          object GeneralPopupChangeTCUnoccupiedColour: TMenuItem
            Caption = 'Change TC Unoccupied Colour'
            OnClick = GeneralPopupChangeTCUnoccupiedColourClick
          end
          object GeneralPopupRestoreTCUnoccupiedColour: TMenuItem
            Caption = 'Restore TC Unoccupied Colour'
            OnClick = GeneralPopupRestoreTCUnoccupiedColourClick
          end
        end
      end
      object GeneralPopupPlatformColours: TMenuItem
        Caption = 'Plat&form Colours'
        object GeneralPopupPlatformColour: TMenuItem
          Caption = 'Platform Colour'
          object GeneralPopupChangePlatformColour: TMenuItem
            Caption = 'Change Platform Colour'
            OnClick = GeneralPopupChangePlatformColourClick
          end
          object GeneralPopupRestorePlatformColour: TMenuItem
            Caption = 'Restore Platform Colour'
            OnClick = GeneralPopupRestorePlatformColourClick
          end
        end
        object GeneralPopupPlungerColour: TMenuItem
          Caption = 'Plunger Colour'
          object GeneralPopupChangePlungerColour: TMenuItem
            AutoCheck = True
            Caption = 'Change Plunger Colour'
            OnClick = GeneralPopupChangePlungerColourClick
          end
          object GeneralPopupRestorePlungerColour: TMenuItem
            AutoCheck = True
            Caption = 'Restore Plunger Colour'
            OnClick = GeneralPopupRestorePlungerColour
          end
        end
        object GeneralPopupPlungerPressedColour: TMenuItem
          Caption = 'Plunger Pressed Colour'
          object GeneralPopupChangePlungerPressedColour: TMenuItem
            Caption = 'Change Plunger-Pressed Colour'
            OnClick = GeneralPopupChangePlungerPressedColourClick
          end
          object GeneralPopupRestorePlungerPressedColour: TMenuItem
            Caption = 'Restore Plunger-Pressed Colour'
            OnClick = GeneralPopupRestorePlungerPressedColourClick
          end
        end
        object GeneralPopupPlungerOutlineColour: TMenuItem
          Caption = 'Plunger Outline Colour'
          object GeneralPopupChangePlungerOutlineColour: TMenuItem
            AutoCheck = True
            Caption = 'Change Plunger Outline Colour'
            OnClick = GeneralPopupChangePlungerOutlineColour
          end
          object GeneralPopupRestorePlungerOutlineColour: TMenuItem
            AutoCheck = True
            Caption = 'Restore Plunger Outline Colour'
            OnClick = GeneralPopupRestorePlungerOutlineColour
          end
        end
      end
      object GeneralPopupPointColours: TMenuItem
        Caption = '&Point Colours'
        object GeneralPopupDefaultPointColours: TMenuItem
          Caption = 'Default Point Colour'
          object GeneralPopupChangeDefaultPointColour: TMenuItem
            Caption = 'Change Default Point Colour'
            OnClick = GeneralPopupChangeDefaultPointColourClick
          end
          object GeneralPopupRestorePointDefaultColour: TMenuItem
            Caption = 'Restore Point Default Colour'
            OnClick = GeneralPopupRestorePointDefaultColourClick
          end
        end
        object GeneralPopupLenzPointNumberColour: TMenuItem
          Caption = 'Lenz Point Number Colour'
          object GeneralPopupChangeLenzPointNumberColour: TMenuItem
            Caption = 'Change Lenz Point Number Colour'
            OnClick = GeneralPopupChangeLenzPointNumberColourClick
          end
          object GeneralPopupRestoreLenzPointNumberColour: TMenuItem
            Caption = 'Restore Lenz Point Number Colour'
            OnClick = GeneralPopupRestoreLenzPointNumberColourClick
          end
        end
        object GeneralPopupPointDivergingLineColour: TMenuItem
          Caption = 'Point Diverging Line Colour'
          object GeneralPopupChangePointDivergingLineColour: TMenuItem
            Caption = 'Change Point Diverging Line Colour'
            OnClick = GeneralPopupChangePointDivergingLineColourClick
          end
          object GeneralPopupRestorePointDivergingLineColour: TMenuItem
            Caption = 'Restore Point Diverging Line Colour'
            OnClick = GeneralPopupRestorePointDivergingLineColourClick
          end
        end
        object GeneralPopupPointDownFacingColour: TMenuItem
          Caption = 'Point Down Facing Colour'
          object GeneralPopupChangePointDownFacingColour: TMenuItem
            Caption = 'Change Point Down Facing Colour'
            OnClick = GeneralPopupChangePointDownFacingColourClick
          end
          object GeneralPopupRestorePointDownFacingColour: TMenuItem
            Caption = 'Restore Point Down Facing Colour'
            OnClick = GeneralPopupRestorePointDownFacingColourClick
          end
        end
        object GeneralPopupPointFeedbackDataInUseColour: TMenuItem
          Caption = 'Point Feedback Data In Use Colour'
          object ChangePointFeedbackDataInUseColour: TMenuItem
            Caption = 'Change Point Feedback Data In Use Colour'
            OnClick = GeneralPopupChangePointFeedbackDataInUseColourClick
          end
          object RestorePointFeedbackDataInUseColour: TMenuItem
            Caption = 'Restore Point Feedback Data In Use Colour'
            OnClick = GeneralPopupRestorePointFeedbackDataInUseColourClick
          end
        end
        object GeneralPopupPointFeedbackDataOutOfUseColour: TMenuItem
          Caption = 'Point Feedback Data Out Of Use Colour'
          object GeneralPopupChangePointFeedbackDataOutOfUseColour: TMenuItem
            Caption = 'Change Point Feedback Data Out Of Use Colour'
            OnClick = GeneralPopupChangePointFeedbackDataOutOfUseColourClick
          end
          object GeneralPopupRestorePointFeedbackDataOutOfUseColour: TMenuItem
            Caption = 'Restore Point Feedback Data Out Of Use Colour'
            OnClick = GeneralPopupRestorePointFeedbackDataOutOfUseColourClick
          end
        end
        object GeneralPopupPointHeelLineColour: TMenuItem
          Caption = 'Point Heel Line Colour'
          object GeneralPopupChangePointHeelLineColour: TMenuItem
            Caption = 'Change Point Heel Line Colour'
            OnClick = GeneralPopupChangePointHeelLineColourClick
          end
          object GeneralPopupRestorePointHeelLineColour: TMenuItem
            Caption = 'Restore Point Heel Line Colour'
            OnClick = GeneralPopupRestorePointHeelLineColourClick
          end
        end
        object GeneralPopupPointLockedBySystemColour: TMenuItem
          Caption = 'Point Locked By System Colour'
          object GeneralPopupChangePointLockedBySystemColour: TMenuItem
            Caption = 'Change Point Locked By System Colour'
            OnClick = GeneralPopupChangePointLockedBySystemColourClick
          end
          object GeneralPopupRestorePointLockedBySystemColour: TMenuItem
            Caption = 'Restore Point Locked By System Colour'
            OnClick = GeneralPopupRestorePointLockedBySystemColourClick
          end
        end
        object GeneralPopupPointLockedByUserColour: TMenuItem
          Caption = 'Point Locked By User Colour'
          object GeneralPopupChangePointLockedByUserColour: TMenuItem
            Caption = 'Change Point Locked By User Colour'
            OnClick = GeneralPopupChangePointLockedByUserColourClick
          end
          object GeneralPopupRestorePointLockedByUserColour: TMenuItem
            Caption = 'Restore Point Locked By User Colour'
            OnClick = GeneralPopupRestorePointLockedByUserColourClick
          end
        end
        object GeneralPopupPointManualOperationColour: TMenuItem
          Caption = 'Point Manual Operation Colour'
          object GeneralPopupChangePointManualOperationColour: TMenuItem
            Caption = 'Change Point Manual Operation Colour'
            OnClick = GeneralPopupChangePointManualOperationColourClick
          end
          object GeneralPopupRestorePointManualOperationColour: TMenuItem
            Caption = 'Restore Point Manual Operation Colour'
            OnClick = GeneralPopupRestorePointManualOperationColourClick
          end
        end
        object GeneralPopupPointOutOfUseColour: TMenuItem
          Caption = 'Point Out Of Use Colour'
          object GeneralPopupChangePointOutOfUseColour: TMenuItem
            Caption = 'Change Point Out Of Use Colour'
            OnClick = GeneralPopupChangePointOutOfUseColourClick
          end
          object GeneralPopupRestorePointOutOfUseColour: TMenuItem
            Caption = 'Restore Point Out Of Use Colour'
            OnClick = GeneralPopupRestorePointOutOfUseColourClick
          end
        end
        object GeneralPopupPointStraightLineColour: TMenuItem
          Caption = 'Point Straight Line Colour'
          object GeneralPopupChangePointStraightLineColour: TMenuItem
            Caption = 'Change Point Straight Line Colour'
            OnClick = GeneralPopupChangePointStraightLineColourClick
          end
          object GeneralPopupRestorePointStraightLineColour: TMenuItem
            Caption = 'Restore Point Straight Line Colour'
            OnClick = GeneralPopupRestorePointStraightLineColourClick
          end
        end
        object GeneralPopupPointUndrawColour: TMenuItem
          Caption = 'Point Undraw Colour'
          object GeneralPopupChangePointUndrawColour: TMenuItem
            AutoCheck = True
            Caption = 'Change Point Undraw Colour'
            OnClick = GeneralPopupChangePointUndrawColourClick
          end
          object GeneralPopupRestorePointUndrawColour: TMenuItem
            Caption = 'Restore Point Undraw Colour'
            OnClick = GeneralPopupRestorePointUndrawColourClick
          end
        end
        object GeneralPopupPointUpFacingColour: TMenuItem
          Caption = 'Point Up Facing Colour'
          object GeneralPopupChangePointUpFacingColour: TMenuItem
            Caption = 'Change Up Facing Point Colour'
            OnClick = GeneralPopupChangePointUpFacingColourClick
          end
          object GeneralPopupRestorePointUpFacingColour: TMenuItem
            Caption = 'Restore Up Facing Point Colour'
            OnClick = GeneralPopupRestorePointUpFacingColourClick
          end
        end
        object GeneralPopupPointsWithoutFeedbackColour: TMenuItem
          Caption = 'Points Without Feedback Colour'
          object GeneralPopupChangePointsWithoutFeedbackColour: TMenuItem
            Caption = 'Change Points Without Feedback Colour'
            OnClick = GeneralPopupChangePointsWithoutFeedbackColourClick
          end
          object GeneralPopupRestorePointsWithoutFeedbackColour: TMenuItem
            Caption = 'Restore Points Without Feedback Colour'
            OnClick = GeneralPopupRestorePointsWithoutFeedbackColourClick
          end
        end
        object GeneralPopupShowPointDefaultStateColour: TMenuItem
          Caption = 'Show Point Default State Colour'
          object GeneralPopupChangeShowPointDefaultStateColour: TMenuItem
            Caption = 'Change Show Point Default State Colour'
            OnClick = GeneralPopupChangeShowPointDefaultStateColourClick
          end
          object GeneralPopupRestoreShowPointDefaultStateColour: TMenuItem
            Caption = 'Restore Show Point Default State Colour'
            OnClick = GeneralPopupRestoreShowPointDefaultStateColourClick
          end
        end
      end
      object GeneralPopupSignalColours: TMenuItem
        Caption = '&Signal Colours'
        object GeneralPopupSignalAspectRed: TMenuItem
          Caption = 'Signal Aspect Red'
          object GeneralPopupChangeSignalAspectRed: TMenuItem
            Caption = 'Change Signal Aspect Red'
            OnClick = GeneralPopupChangeSignalAspectRedClick
          end
          object GeneralPopupRestoreSignalAspectRed: TMenuItem
            Caption = 'Restore Signal Aspect Red'
            OnClick = GeneralPopupRestoreSignalAspectRedClick
          end
        end
        object GeneralPopupSignalAspectGreen: TMenuItem
          Caption = 'Signal Aspect Green'
          object GeneralPopupChangeSignalAspectGreen: TMenuItem
            Caption = 'Change Signal Aspect Green'
            OnClick = GeneralPopupChangeSignalAspectGreenClick
          end
          object GeneralPopupRestoreSignalAspectGreen: TMenuItem
            Caption = 'Restore Signal Aspect Green'
            OnClick = GeneralPopupRestoreSignalAspectGreenClick
          end
        end
        object GeneralPopupSignalAspectYellow: TMenuItem
          Caption = 'Signal Aspect Yellow'
          object GeneralPopupChangeSignalAspectYellow: TMenuItem
            Caption = 'Change Signal Aspect Yellow'
            OnClick = GeneralPopupChangeSignalAspectYellowClick
          end
          object GeneralPopupRestoreSignalAspectYellow: TMenuItem
            Caption = 'Restore Signal Aspect Yellow'
            OnClick = GeneralPopupRestoreSignalAspectYellowClick
          end
        end
        object GeneralPopupSignalAspectUnlitColour: TMenuItem
          Caption = 'Signal Aspect Unlit'
          object GeneralPopupChangeSignalAspectUnlitColour: TMenuItem
            Caption = 'Change Signal Aspect Unlit Colour'
            OnClick = GeneralPopupChangeSignalAspectUnlitColourClick
          end
          object GeneralPopupRestoreSignalAspectUnlitColour: TMenuItem
            Caption = 'Restore Signal Aspect Unlit Colour'
            OnClick = GeneralPopupRestoreSignalAspectUnlitColourClick
          end
        end
        object GeneralPopupSignalNumberColour: TMenuItem
          Caption = 'Signal Number Colour'
          object GeneralPopupChangeSignalNumberColour: TMenuItem
            Caption = 'Change Signal Number Colour'
            OnClick = GeneralPopupChangeSignalNumberColourClick
          end
          object GeneralPopupRestoreSignalNumberColour: TMenuItem
            Caption = 'Restore Signal Number Colour'
            OnClick = GeneralPopupRestoreSignalNumberColourClick
          end
        end
        object GeneralPopupSignalPostBaseColour: TMenuItem
          Caption = 'Signal Post Base Colour'
          object GeneralPopupChangeSignalPostBaseColour: TMenuItem
            Caption = 'Change Signal Post Base Colour'
          end
          object GeneralPopupRestoreSignalPostBaseColour: TMenuItem
            Caption = 'Restore Signal Post Base Colour'
          end
        end
        object GeneralPopupSignalPostRouteSettingColour: TMenuItem
          Caption = 'Signal Post Route Setting Colour'
          object GeneralPopupChangeSignalPostRouteSettingColour: TMenuItem
            Caption = 'Change Signal Post Route Setting Colour'
          end
          object GeneralPopupRestoreSignalPostRouteSettingColour: TMenuItem
            Caption = 'Restore Signal Post Route Setting Colour'
          end
        end
        object GeneralPopupSignalPostEmergencyRouteSettingColour: TMenuItem
          Caption = 'Signal Post Emergency Route Setting Colour'
          object GeneralPopupChangeSignalPostEmergencyRouteSettingColour: TMenuItem
            Caption = 'Change Signal Post Emergency Route Setting Colour'
            OnClick = GeneralPopupChangeSignalPostEmergencyRouteSettingColourClick
          end
          object GeneralPopupRestoreSignalPostEmergencyRouteSettingColour: TMenuItem
            Caption = 'Restore Signal Post Emergency Route Setting Colour'
            OnClick = GeneralPopupRestoreSignalPostEmergencyRouteSettingColourClick
          end
        end
        object GeneralPopupSignalPostTheatreSettingColour: TMenuItem
          Caption = 'Signal Post Theatre Setting Colour'
          object GeneralPopupChangeSignalPostTheatreSettingColour: TMenuItem
            Caption = 'Change Signal Post TheatreSetting Colour'
            OnClick = GeneralPopupChangeSignalPostTheatreSettingColourClick
          end
          object GeneralPopupRestoreSignalPostTheatreSettingColour: TMenuItem
            Caption = 'Restore Signal Post Theatre Setting Colour'
            OnClick = GeneralPopupRestoreSignalPostTheatreSettingColourClick
          end
        end
      end
      object GeneralPopupTrainColours: TMenuItem
        Caption = '&Train Colours'
        object GeneralPopupTrainActiveColour: TMenuItem
          Caption = 'Train Active Colour'
          object GeneralPopupChangeTrainActiveColour: TMenuItem
            Caption = 'Change Train Active Colour'
            OnClick = GeneralPopupChangeTrainActiveColourClick
          end
          object GeneralPopupRestoreTrainActiveColour: TMenuItem
            Caption = 'Restore Train Active Colour'
            OnClick = GeneralPopupRestoreTrainActiveColourClick
          end
        end
        object GeneralPopupTrainInactiveColour: TMenuItem
          Caption = 'Train Inactive Colour'
          object GeneralPopupChangeTrainInactiveColour: TMenuItem
            Caption = 'Change Train Inactive Colour'
            OnClick = GeneralPopupChangeTrainInactiveColourClick
          end
          object GeneralPopupRestoreTrainInactiveColour: TMenuItem
            Caption = 'Restore Train Inactive Colour'
            OnClick = GeneralPopupRestoreTrainInactiveColourClick
          end
        end
      end
      object GeneralPopupRuler4: TMenuItem
        Caption = '-'
      end
      object RestoreAllDefaultColours: TMenuItem
        Caption = '&Restore All Default Colours'
        OnClick = GeneralPopupRestoreAllDefaultColoursClick
      end
    end
    object GeneralPopupChangePenStyles: TMenuItem
      Caption = 'Change Pen Styles'
      object GeneralPopupSidingPenStyle: TMenuItem
        Caption = 'Siding Pen Style'
        object GeneralPopupChangeSidingPenStyle: TMenuItem
          Caption = 'Change Siding Pen Style'
          object GeneralPopupSidingPenStyleSolid: TMenuItem
            Caption = 'Solid'
            OnClick = GeneralPopupSidingPenStyleSolidClick
          end
          object GeneralPopupSidingPenStyleDash: TMenuItem
            Caption = 'Dash'
            OnClick = GeneralPopupSidingPenStyleDashClick
          end
          object GeneralPopupSidingPenStyleDot: TMenuItem
            Caption = 'Dot'
            OnClick = GeneralPopupSidingPenStyleDotClick
          end
          object GeneralPopupSidingPenStyleDashDot: TMenuItem
            Caption = 'Dash Dot'
            OnClick = GeneralPopupSidingPenStyleDashDotClick
          end
          object GeneralPopupSidingPenStyleDashDotDot: TMenuItem
            Caption = 'Dash Dot Dot'
            OnClick = GeneralPopupSidingPenStyleDashDotDotClick
          end
          object GeneralPopupSidingPenStyleClear: TMenuItem
            Caption = 'Clear'
            OnClick = GeneralPopupSidingPenStyleClearClick
          end
          object GeneralPopupSidingPenStyleInsideFrame: TMenuItem
            Caption = 'Inside Frame'
            OnClick = GeneralPopupSidingPenStyleInsideFrameClick
          end
        end
        object GeneralPopupRestoreSidingPenStyle: TMenuItem
          Caption = 'Restore Siding Pen Style'
          OnClick = GeneralPopupRestoreSidingPenStyleClick
        end
      end
      object GeneralPopupFiddleyardLinePenStyle: TMenuItem
        Caption = 'FiddleyardLine Pen Style'
        object GeneralPopupChangeFiddleyardLinePenStyle: TMenuItem
          Caption = 'Change FiddleyardLine Pen Style'
          object GeneralPopupFiddleyardLinePenStyleSolid: TMenuItem
            Caption = 'Solid'
            OnClick = GeneralPopupFiddleyardLinePenStyleSolidClick
          end
          object GeneralPopupFiddleyardLinePenStyleDash: TMenuItem
            Caption = 'Dash'
            OnClick = GeneralPopupFiddleyardLinePenStyleDashClick
          end
          object GeneralPopupFiddleyardLinePenStyleDot: TMenuItem
            Caption = 'Dot'
            OnClick = GeneralPopupFiddleyardLinePenStyleDotClick
          end
          object GeneralPopupFiddleyardLinePenStyleDashDot: TMenuItem
            Caption = 'Dash Dot'
            OnClick = GeneralPopupFiddleyardLinePenStyleDashDotClick
          end
          object GeneralPopupFiddleyardLinePenStyleDashDotDot: TMenuItem
            Caption = 'Dash Dot Dot'
            OnClick = GeneralPopupFiddleyardLinePenStyleDashDotDotClick
          end
          object GeneralPopupFiddleyardLinePenStyleClear: TMenuItem
            Caption = 'Clear'
            OnClick = GeneralPopupFiddleyardLinePenStyleClearClick
          end
          object GeneralPopupFiddleyardLinePenStyleInsideFrame: TMenuItem
            Caption = 'Inside Frame'
            OnClick = GeneralPopupFiddleyardLinePenStyleInsideFrameClick
          end
        end
        object GeneralPopupRestoreFiddleyardLinePenStyle: TMenuItem
          Caption = 'Restore FiddleyardLine Pen Style'
          OnClick = GeneralPopupRestoreFiddleyardLinePenStyleClick
        end
      end
      object GeneralPopupProjectedLinePenStyle: TMenuItem
        Caption = 'ProjectedLine Pen Style'
        object GeneralPopupChangeProjectedLinePenStyle: TMenuItem
          Caption = 'Change ProjectedLine Pen Style'
          object GeneralPopupProjectedLinePenStyleSolid: TMenuItem
            Caption = 'Solid'
            OnClick = GeneralPopupProjectedLinePenStyleSolidClick
          end
          object GeneralPopupProjectedLinePenStyleDash: TMenuItem
            Caption = 'Dash'
            OnClick = GeneralPopupProjectedLinePenStyleDashClick
          end
          object GeneralPopupProjectedLinePenStyleDot: TMenuItem
            Caption = 'Dot'
            OnClick = GeneralPopupProjectedLinePenStyleDotClick
          end
          object GeneralPopupProjectedLinePenStyleDashDot: TMenuItem
            Caption = 'Dash Dot'
            OnClick = GeneralPopupProjectedLinePenStyleDashDotClick
          end
          object GeneralPopupProjectedLinePenStyleDashDotDot: TMenuItem
            Caption = 'Dash Dot Dot'
            OnClick = GeneralPopupProjectedLinePenStyleDashDotDotClick
          end
          object GeneralPopupProjectedLinePenStyleClear: TMenuItem
            Caption = 'Clear'
            OnClick = GeneralPopupProjectedLinePenStyleClearClick
          end
          object GeneralPopupProjectedLinePenStyleInsideFrame: TMenuItem
            Caption = 'Inside Frame'
            OnClick = GeneralPopupProjectedLinePenStyleInsideFrameClick
          end
        end
        object GeneralPopupRestoreProjectedLinePenStyle: TMenuItem
          Caption = 'Restore ProjectedLine Pen Style'
          OnClick = GeneralPopupRestoreProjectedLinePenStyleClick
        end
      end
      object GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyle: TMenuItem
        Caption = 'TC Out Of Use As No Feedback Received Pen Style'
        object GeneralPopupChangeTCOutOfUseAsNoFeedbackReceivedPenStyle: TMenuItem
          Caption = 'Change TC Out Of Use As No Feedback Received Pen Style'
          object GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleSolid: TMenuItem
            Caption = 'Solid'
            OnClick = GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleSolidClick
          end
          object GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDash: TMenuItem
            Caption = 'Dash'
            OnClick = GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashClick
          end
          object GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDot: TMenuItem
            Caption = 'Dot'
            OnClick = GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDotClick
          end
          object GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDot: TMenuItem
            Caption = 'Dash Dot'
            OnClick = GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotClick
          end
          object GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotDot: TMenuItem
            Caption = 'Dash Dot Dot'
            OnClick = GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleDashDotDotClick
          end
          object GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleClear: TMenuItem
            Caption = 'Clear'
            OnClick = GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleClearClick
          end
          object GeneralPopupTCOutOfUseAsNoFeedbackReceivedPenStyleInsideFrame: TMenuItem
            Caption = 'Inside Frame'
          end
        end
        object GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedPenStyle: TMenuItem
          Caption = 'Restore TC Out Of Use As No Feedback Received Pen Style'
          OnClick = GeneralPopupRestoreTCOutOfUseAsNoFeedbackReceivedPenStyleClick
        end
      end
      object GeneralPopupTCOutOfUseSetByUserPenStyle: TMenuItem
        Caption = 'TC Out Of Use Set By User Pen Style'
        object GeneralPopupChangeTCOutOfUseSetByUserPenStyle: TMenuItem
          Caption = 'Change TC Out Of Use As No Feedback Received Pen Style'
          object GeneralPopupTCOutOfUseSetByUserPenStyleSolid: TMenuItem
            Caption = 'Solid'
            OnClick = GeneralPopupTCOutOfUseSetByUserPenStyleSolidClick
          end
          object GeneralPopupTCOutOfUseSetByUserPenStyleDash: TMenuItem
            Caption = 'Dash'
            OnClick = GeneralPopupTCOutOfUseSetByUserPenStyleDashClick
          end
          object GeneralPopupTCOutOfUseSetByUserPenStyleDot: TMenuItem
            Caption = 'Dot'
            OnClick = GeneralPopupTCOutOfUseSetByUserPenStyleDotClick
          end
          object GeneralPopupTCOutOfUseSetByUserPenStyleDashDot: TMenuItem
            Caption = 'Dash Dot'
            OnClick = GeneralPopupTCOutOfUseSetByUserPenStyleDashDotClick
          end
          object GeneralPopupTCOutOfUseSetByUserPenStyleDashDotDot: TMenuItem
            Caption = 'Dash Dot Dot'
            OnClick = GeneralPopupTCOutOfUseSetByUserPenStyleDashDotDotClick
          end
          object GeneralPopupTCOutOfUseSetByUserPenStyleClear: TMenuItem
            Caption = 'Clear'
            OnClick = GeneralPopupTCOutOfUseSetByUserPenStyleClearClick
          end
          object GeneralPopupTCOutOfUseSetByUserPenStyleInsideFrame: TMenuItem
            Caption = 'InsideFrame'
            OnClick = GeneralPopupTCOutOfUseSetByUserPenStyleInsideFrameClick
          end
        end
        object GeneralPopupRestoreTCOutOfUseSetByUserPenStyle: TMenuItem
          Caption = 'Restore TC Out Of Use As No Feedback Received Pen Style'
          OnClick = GeneralPopupRestoreTCOutOfUseSetByUserPenStyleClick
        end
      end
      object GeneralPopupTCPermanentFeedbackOccupationPenStyle: TMenuItem
        Caption = 'TC Permanent Feedback Occupation Style'
        object GeneralPopupChangeTCPermanentFeedbackOccupationPenStyle: TMenuItem
          Caption = 'Change TC Permanent Feedback occupation Pen Style'
          object GeneralPopupTCPermanentFeedbackOccupationPenStyleSolid: TMenuItem
            Caption = 'Solid'
            OnClick = GeneralPopupTCPermanentFeedbackOccupationPenStyleSolidClick
          end
          object GeneralPopupTCPermanentFeedbackOccupationPenStyleDash: TMenuItem
            Caption = 'Dash'
            OnClick = GeneralPopupTCPermanentFeedbackOccupationPenStyleDashClick
          end
          object GeneralPopupTCPermanentFeedbackOccupationPenStyleDot: TMenuItem
            Caption = 'Dot'
            OnClick = GeneralPopupTCPermanentFeedbackOccupationPenStyleDotClick
          end
          object GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDot: TMenuItem
            Caption = 'Dash Dot'
            OnClick = GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotClick
          end
          object GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotDot: TMenuItem
            Caption = 'Dash Dot Dot'
            OnClick = GeneralPopupTCPermanentFeedbackOccupationPenStyleDashDotDotClick
          end
          object GeneralPopupTCPermanentFeedbackOccupationPenStyleClear: TMenuItem
            Caption = 'Clear'
            OnClick = GeneralPopupTCPermanentFeedbackOccupationPenStyleClearClick
          end
          object GeneralPopupTCPermanentFeedbackOccupationPenStyleInsideFrame: TMenuItem
            Caption = 'Inside Frame'
            OnClick = GeneralPopupTCPermanentFeedbackOccupationPenStyleInsideFrameClick
          end
        end
        object GeneralPopupRestoreTCPermanentFeedbackOccupationPenStyle: TMenuItem
          Caption = 'Restore TC Permanent Feedback Occupation Pen Style'
          OnClick = GeneralPopupRestoreTCPermanentFeedbackOccupationPenStyleClick
        end
      end
      object GeneralPopupTCPermanentOccupationSetByUserPenStyle: TMenuItem
        Caption = 'TC Permanent Occupation Set By User Style'
        object GeneralPopupChangeTCPermanentOccupationSetByUserPenStyle: TMenuItem
          Caption = 'Change TC Permanent Occupation Set By User Pen Style'
          object GeneralPopupTCPermanentOccupationSetByUserPenStyleSolid: TMenuItem
            Caption = 'Solid'
            OnClick = GeneralPopupTCPermanentOccupationSetByUserPenStyleSolidClick
          end
          object GeneralPopupTCPermanentOccupationSetByUserPenStyleDash: TMenuItem
            Caption = 'Dash'
            OnClick = GeneralPopupTCPermanentOccupationSetByUserPenStyleDashClick
          end
          object GeneralPopupTCPermanentOccupationSetByUserPenStyleDot: TMenuItem
            Caption = 'Dot'
            OnClick = GeneralPopupTCPermanentOccupationSetByUserPenStyleDotClick
          end
          object GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDot: TMenuItem
            Caption = 'Dash Dot'
            OnClick = GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotClick
          end
          object GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotDot: TMenuItem
            Caption = 'Dash Dot Dot'
            OnClick = GeneralPopupTCPermanentOccupationSetByUserPenStyleDashDotDotClick
          end
          object GeneralPopupTCPermanentOccupationSetByUserPenStyleClear: TMenuItem
            Caption = 'Clear'
            OnClick = GeneralPopupTCPermanentOccupationSetByUserPenStyleClearClick
          end
          object GeneralPopupTCPermanentOccupationSetByUserPenStyleInsideFrame: TMenuItem
            Caption = 'Inside Frame'
            OnClick = GeneralPopupTCPermanentOccupationSetByUserPenStyleInsideFrameClick
          end
        end
        object GeneralPopupRestoreTCPermanentOccupationSetByUserPenStyle: TMenuItem
          Caption = 'Restore TC Permanent Occupation Set By User Pen Style'
          OnClick = GeneralPopupRestoreTCPermanentOccupationSetByUserPenStyleClick
        end
      end
      object GeneralPopupTCPermanentSystemOccupationPenStyle: TMenuItem
        Caption = 'TC Permanent System Occupation Style'
        object GeneralPopupChangeTCPermanentSystemOccupationPenStyle: TMenuItem
          Caption = 'Change TC Permanent System Occupation Pen Style'
          object GeneralPopupTCPermanentSystemOccupationPenStyleSolid: TMenuItem
            Caption = 'Solid'
            OnClick = GeneralPopupTCPermanentSystemOccupationPenStyleSolidClick
          end
          object GeneralPopupTCPermanentSystemOccupationPenStyleDash: TMenuItem
            Caption = 'Dash'
            OnClick = GeneralPopupTCPermanentSystemOccupationPenStyleDashClick
          end
          object GeneralPopupTCPermanentSystemOccupationPenStyleDot: TMenuItem
            Caption = 'Dot'
            OnClick = GeneralPopupTCPermanentSystemOccupationPenStyleDotClick
          end
          object GeneralPopupTCPermanentSystemOccupationPenStyleDashDot: TMenuItem
            Caption = 'Dash Dot'
            OnClick = GeneralPopupTCPermanentSystemOccupationPenStyleDashDotClick
          end
          object GeneralPopupTCPermanentSystemOccupationPenStyleDashDotDot: TMenuItem
            Caption = 'Dash Dot Dot'
            OnClick = GeneralPopupTCPermanentSystemOccupationPenStyleDashDotDotClick
          end
          object GeneralPopupTCPermanentSystemOccupationPenStyleClear: TMenuItem
            Caption = 'Clear'
            OnClick = GeneralPopupTCPermanentSystemOccupationPenStyleClearClick
          end
          object GeneralPopupTCPermanentSystemOccupationPenStyleInsideFrame: TMenuItem
            Caption = 'Inside Frame'
            OnClick = GeneralPopupTCPermanentSystemOccupationPenStyleInsideFrameClick
          end
        end
        object GeneralPopupRestoreTCPermanentSystemOccupationPenStyle: TMenuItem
          Caption = 'Restore TC Permanent System Occupation Pen Style'
          OnClick = GeneralPopupRestoreTCPermanentSystemOccupationPenStyleClick
        end
      end
      object GeneralPopupTCLocoOutOfPlaceOccupationPenStyle: TMenuItem
        Caption = 'TC Loco Out Of Place Pen Style'
        object GeneralPopupChangeTCLocoOutOfPlaceOccupationPenStyle: TMenuItem
          Caption = 'Change TC Loco Out Of Place Pen Style'
          object GeneralPopupTCLocoOutOfPlaceOccupationPenStyleSolid: TMenuItem
            Caption = 'Solid'
            OnClick = GeneralPopupTCLocoOutOfPlaceOccupationPenStyleSolidClick
          end
          object GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDash: TMenuItem
            Caption = 'Dash'
            OnClick = GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashClick
          end
          object GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDot: TMenuItem
            Caption = 'Dot'
            OnClick = GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDotClick
          end
          object GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDot: TMenuItem
            Caption = 'Dash Dot'
            OnClick = GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotClick
          end
          object GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotDot: TMenuItem
            Caption = 'Dash Dot Dot'
            OnClick = GeneralPopupTCLocoOutOfPlaceOccupationPenStyleDashDotDotClick
          end
          object GeneralPopupTCLocoOutOfPlaceOccupationPenStyleClear: TMenuItem
            Caption = 'Clear'
            OnClick = GeneralPopupTCLocoOutOfPlaceOccupationPenStyleClearClick
          end
          object GeneralPopupTCLocoOutOfPlaceOccupationPenStyleInsideFrame: TMenuItem
            Caption = 'Inside Frame'
            OnClick = GeneralPopupTCLocoOutOfPlaceOccupationPenStyleInsideFrameClick
          end
        end
        object GeneralPopupRestoreTCLocoOutOfPlaceOccupationPenStyle: TMenuItem
          Caption = 'Restore TC Loco Out Of Place Pen Style'
          OnClick = GeneralPopupRestoreTCLocoOutOfPlaceOccupationPenStyleClick
        end
      end
    end
    object GeneralPopupOperations: TMenuItem
      Caption = '&Operations'
      object GeneralPopupChangePoint: TMenuItem
        Caption = 'Change &Point'
        OnClick = GeneralPopupChangePointClick
      end
      object ChangeSignal: TMenuItem
        Caption = 'Change &Signal'
        OnClick = GeneralPopupChangeSignalClick
      end
      object GeneralPopupListLocomotives: TMenuItem
        Caption = '&List Locomotives'
        OnClick = GeneralPopupListLocomotivesClick
      end
      object GeneralPopupShowTrackcircuit: TMenuItem
        Caption = 'Show &Trackcircuit'
        OnClick = GeneralPopupShowTrackcircuitClick
      end
      object GeneralPopupRuler5: TMenuItem
        Caption = '-'
      end
      object GeneralPopupDebugOptions: TMenuItem
        Caption = '&Debug Options'
        OnClick = GeneralPopupDebugOptionsClick
      end
    end
    object GeneralPopupSetLogFileMaximumWidth: TMenuItem
      Caption = 'Set Log File Maximum Width'
      OnClick = GeneralPopupSetLogFileMaximumWidthClick
    end
    object GeneralPopupResetFWPRailWindowSizeAndPosition: TMenuItem
      Caption = 'Reset Window Size && Position'
      Enabled = False
      OnClick = GeneralPopupResetFWPRailWindowSizeAndPositionClick
    end
    object GeneralPopupRestoreAllScreenDrawingDefaultSettings: TMenuItem
      Caption = 'Restore All Screen Drawing Default Settings'
      OnClick = GeneralPopupRestoreAllScreenDrawingDefaultSettingsClick
    end
    object GeneralPopupRestoreAllProgramDefaultSettings: TMenuItem
      Caption = 'Restore All Program'#39's Default Settings'
      OnClick = GeneralPopupRestoreAllProgramDefaultSettingsClick
    end
  end
  object FWPRailWindowPopupOpenDialogue: TOpenDialog
    Filter = 'FWP Rail Timetable Files|*.tim'
    OptionsEx = [ofExNoPlacesBar]
    Title = 'Load Timetable File'
    Left = 252
    Top = 48
  end
  object FWPRailWindowColourDialogue: TColorDialog
    Left = 436
    Top = 49
  end
  object TCPopupMenu: TPopupMenu
    OnPopup = TCPopupMenuOnPopup
    Left = 336
    Top = 108
    object TCPopupTrackCircuitNumber: TMenuItem
      Caption = 'TrackCircuit Number'
      OnClick = TCPopupTrackCircuitNumberClick
    end
    object TCPopupRuler1: TMenuItem
      Caption = '-'
    end
    object TCPopupSetTrackCircuitToFeedbackOccupation: TMenuItem
      Caption = 'Set Track Circuit to Feedback Occupation'
      OnClick = TCPopupSetTrackCircuitToFeedbackOccupationClick
    end
    object TCPopupSetTrackCircuitUnoccupied: TMenuItem
      Caption = 'Set Track Circuit Unoccupied'
      OnClick = TCPopupSetTrackCircuitUnoccupiedClick
    end
    object TCPopupSetTrackCircuitOutOfUseSetByUser: TMenuItem
      Caption = 'Set Track Circuit Out Of Use'
      OnClick = TCPopupSetTrackCircuitOutOfUseSetByUserClick
    end
    object TCPopupSetTrackCircuitToSystemOccupation: TMenuItem
      Caption = 'Set Track Circuit to System Occupation'
      OnClick = TCPopupSetTrackCircuitToSystemOccupationClick
    end
    object TCPopupSetTrackCircuitToPermanentOccupation: TMenuItem
      Caption = 'Set Track Circuit to Permanent Occupation'
      OnClick = TCPopupSetTrackCircuitToPermanentOccupationClick
    end
    object TCPopupSetTrackCircuitSpeedRestriction: TMenuItem
      Caption = 'Set Track Circuit Speed Restriction'
      OnClick = TCPopupSetTrackCircuitSpeedRestrictionClick
    end
    object TCPopupClearTrackCircuitSpeedRestriction: TMenuItem
      Caption = 'Clear Track Circuit Speed Restriction'
      OnClick = TCPopupClearTrackCircuitSpeedRestrictionClick
    end
    object TCPopupSetTrackCircuitToUserDriving: TMenuItem
      Caption = 'Set Track Circuit to User Must Drive'
      OnClick = TCPopupSetTrackCircuitToUserDrivingClick
    end
    object TCPopupRuler2: TMenuItem
      Caption = '-'
    end
    object TCPopupSetLineOutOfUse: TMenuItem
      Caption = 'Set Line Out Of Use'
      OnClick = TCPopupSetLineOutOfUseClick
    end
    object TCPopupSetLocationOutOfUse: TMenuItem
      Caption = 'Set Location Out Of Use'
      OnClick = TCPopupSetLocationOutOfUseClick
    end
    object TCPopupRuler3: TMenuItem
      Caption = '-'
    end
    object TCPopupAllocateLocoToTrackCircuit: TMenuItem
      Caption = 'Allocate Loco to TrackCircuit'
      OnClick = TCPopupAllocateLocoToTrackCircuitClick
    end
    object TCPopupClearLocoAllocationFromTrackCircuit: TMenuItem
      Caption = 'Clear Loco Allocation From Track Circuit'
      Enabled = False
      OnClick = TCPopupClearLocoAllocationFromTrackCircuitClick
    end
    object TCPopupRuler4: TMenuItem
      Caption = '-'
    end
    object TCPopupChangeInternalLocoDirectionToUp: TMenuItem
      Caption = 'Change Internal Loco Direction to Up'
      OnClick = TCPopupChangeInternalLocoDirectionToUpClick
    end
    object TCPopupChangeInternalLocoDirectionToDown: TMenuItem
      Caption = 'Change Internal Loco Direction to Down'
      OnClick = TCPopupChangeInternalLocoDirectionToDownClick
    end
  end
  object PointPopupMenu: TPopupMenu
    OnPopup = PointPopupMenuOnPopup
    Left = 336
    Top = 168
    object PointPopupPointNum: TMenuItem
      Caption = 'Point '
    end
    object PointPopupRuler1: TMenuItem
      Caption = '-'
    end
    object PointPopupSetPointOutOfUse: TMenuItem
      Caption = 'Set Point Out Of Use'
      OnClick = PointPopupSetPointOutOfUseClick
    end
    object PointPopupSetPointBackInUse: TMenuItem
      Caption = 'Set Point Back In Use'
      OnClick = PointPopupSetPointBackInUseClick
    end
    object PointPopupSetPointToManual: TMenuItem
      Caption = 'Set Point To Manual'
      OnClick = PointPopupSetPointtoManualClick
    end
    object PointPopupSetPointToAutomatic: TMenuItem
      Caption = 'Set Point To Automatic'
      OnClick = PointPopupSetPointToAutomaticClick
    end
    object PointPopupLockPoint: TMenuItem
      Caption = 'Lock Point'
      OnClick = PointPopupLockPointClick
    end
    object PointPopupRuler2: TMenuItem
      Caption = '-'
    end
    object PointPopupEditPointDetails: TMenuItem
      Caption = 'Edit Point Details'
      OnClick = PointPopupEditPointDetailsClick
    end
  end
  object SignalPopupMenu: TPopupMenu
    OnPopup = SignalPopupMenuOnPopup
    Left = 336
    Top = 216
    object SignalPopupSignalNum: TMenuItem
      Caption = 'Signals'
    end
    object SignalPopupRuler1: TMenuItem
      Caption = '-'
    end
    object SignalPopupSetSignalOutOfUse: TMenuItem
      Caption = 'Set Signal Out Of Use'
      OnClick = SignalPopupSetSignalOutOfUseClick
    end
    object SignalPopupSetSignalBackInUse: TMenuItem
      Caption = 'Set Signal Back In Use'
      OnClick = SignalPopupSetSignalBackInUseClick
    end
    object SignalPopupRuler2: TMenuItem
      Caption = '-'
    end
    object SignalPopupEditSignalDetails: TMenuItem
      Caption = 'Edit Signal Details'
      OnClick = SignalPopupEditSignalDetailsClick
    end
  end
  object BufferStopPopupMenu: TPopupMenu
    OnPopup = BufferStopMenuOnPopup
    Left = 480
    Top = 112
    object BufferStopPopupBufferStopNum: TMenuItem
      Caption = 'Buffer Stop'
    end
    object BufferStopPopupRuler: TMenuItem
      Caption = '-'
    end
  end
  object CreateOrDeleteItemPopupMenu: TPopupMenu
    OnPopup = CreateOrDeleteItemMenuOnPopup
    Left = 472
    Top = 176
    object CreateSignalMenuItem: TMenuItem
      Caption = 'Create Signal'
      Enabled = False
      OnClick = CreateSignalMenuItemClick
    end
    object CreatePointMenuItem: TMenuItem
      Caption = 'Create Point'
      Enabled = False
      OnClick = CreatePointMenuItemClick
    end
    object CreateLineMenuItem: TMenuItem
      Caption = 'Create Line'
      Enabled = False
      OnClick = CreateLineMenuItemClick
    end
    object CreateOrDeleteMenuItemRuler: TMenuItem
      Caption = '-'
    end
    object DeleteSignalMenuItem: TMenuItem
      Caption = 'Delete Signal'
      OnClick = DeleteSignalMenuItemClick
    end
    object DeletePointMenuItem: TMenuItem
      Caption = 'Delete Point'
      OnClick = DeletePointMenuItemClick
    end
    object DeleteLineMenuItem: TMenuItem
      Caption = 'Delete Line'
      OnClick = DeleteLineMenuItemClick
    end
  end
end
