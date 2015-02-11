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
  OnDestroy = FWPRailWindowDestroy
  OnDragDrop = FWPRailWindowDragDrop
  OnDragOver = FWPRailWindowDragOver
  OnKeyDown = FWPRailWindowKeyDown
  OnMouseDown = FWPRailWindowMouseDown
  OnMouseLeave = FWPRailWindowMouseLeave
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
    object MainDropDownMenuFile: TMenuItem
      Caption = '&File'
      Visible = False
      object MainDropDownMenuFileExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MainDropDownMenuFileExitClick
      end
    end
    object MainDropDownMenuDisplay: TMenuItem
      Caption = '&Display'
      object MainDropdownMenuDisplayShowDebugOutputWindow: TMenuItem
        Caption = 'Show De&bug Output Window'
        Checked = True
        OnClick = MainDropdownMenuDisplayShowDebugOutputWindowClick
      end
      object MainDropdownMenuDisplayShowStatusBar: TMenuItem
        Caption = 'Show Status &Bar'
        Checked = True
        OnClick = ShowStatusBarClick
      end
      object MainDropdownMenuDisplayShowMainMenu: TMenuItem
        AutoCheck = True
        Caption = 'Show &Main Menu'
        OnClick = MainDropdownMenuDisplayShowMainMenuClick
      end
      object MainDropdownMenuDisplayDiagramsWindow: TMenuItem
        Caption = '&Diagrams Window'
        Checked = True
        OnClick = MainDropdownMenuDisplayDiagramsWindowClick
      end
      object MainDropdownMenuDisplayWorkingTimetableWindow: TMenuItem
        Caption = '&Working Timetable Window'
        OnClick = MainDropdownMenuDisplayWorkingTimetableWindowClick
      end
      object MainDropdownMenuDisplayZoomScreen: TMenuItem
        Caption = '&Zoom Screen'
        OnClick = MainDropdownMenuDisplayZoomScreenClick
      end
    end
    object MainDropdownMenuHelp: TMenuItem
      Caption = 'Help'
      Hint = #39'Isn'#39#39't'#39' FWP wonderful?'#39
      Visible = False
      object MainDropdownMenuHelpRailHelp: TMenuItem
        Caption = 'Rail Help'
        OnClick = MainDropdownMenuHelpRailHelpClick
      end
      object MainDropDownMenuHelpAboutRail: TMenuItem
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
