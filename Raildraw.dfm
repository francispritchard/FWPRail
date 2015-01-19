object FWPRailWindow: TFWPRailWindow
  Left = 606
  Top = 560
  HorzScrollBar.Tracking = True
  VertScrollBar.Smooth = True
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
  Position = poScreenCenter
  Visible = True
  OnClose = FWPRailWindowClose
  OnCreate = FWPRailWindowCreate
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
    SizeGrip = False
    OnClick = FWPRailWindowStatusBarClick
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
      object ChangePoint: TMenuItem
        Caption = 'Change &Point'
      end
      object MainOperationsMenuChangeSignal: TMenuItem
        Caption = 'Change &Signal'
      end
      object MainOperationsMenuListLocomotives: TMenuItem
        Caption = '&List Locomotives'
      end
      object MainOperationsMenuShowTrackCircuit: TMenuItem
        Caption = 'Show &Trackcircuit'
      end
      object MainOperationsMenuDebugOptions: TMenuItem
        Caption = '&Debug Options'
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
      end
      object MainClockMenuRunTimeFaster: TMenuItem
        AutoCheck = True
        Caption = 'Run Clock Faster'
      end
      object MainClockMenuRunClockFastest: TMenuItem
        Caption = 'Run Clock Fastest'
      end
      object MainClockMenuRunTimeSlower: TMenuItem
        AutoCheck = True
        Caption = 'Run Clock Slower'
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
  object LinePopupMenu: TPopupMenu
    OnPopup = LinePopupMenuOnPopup
    Left = 336
    Top = 108
  end
  object PointPopupMenu: TPopupMenu
    OnPopup = PointPopupMenuOnPopup
    Left = 336
    Top = 168
  end
  object SignalPopupMenu: TPopupMenu
    OnPopup = SignalPopupMenuOnPopup
    Left = 336
    Top = 216
  end
  object BufferStopPopupMenu: TPopupMenu
    OnPopup = BufferStopMenuOnPopup
    Left = 480
    Top = 112
  end
  object FWPRailApplicationEvents: TApplicationEvents
    OnShortCut = FWPRailApplicationEventsShortCut
    Left = 112
    Top = 208
  end
  object PopupTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = PopupTimerTick
    Left = 496
    Top = 200
  end
  object GeneralPopupMenu: TPopupMenu
    OnPopup = GeneralPopupMenuOnPopup
    Left = 176
    Top = 160
  end
end
