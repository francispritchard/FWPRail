object MainUnitWindow: TMainUnitWindow
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsToolWindow
  Caption = 'Options'
  ClientHeight = 606
  ClientWidth = 456
  Color = clBtnFace
  Enabled = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = MainUnitWindowClose
  OnCreate = MainUnitWindowCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MainUnitWindowPageControl: TPageControl
    Left = 0
    Top = 0
    Width = 456
    Height = 606
    ActivePage = MainUnitWindowPageControlGeneralDebuggingTabSheet
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object MainUnitWindowPageControlGeneralDebuggingTabSheet: TTabSheet
      Caption = 'Debugging'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      object MainUnitWindowPageControlGeneralDebuggingCheckListBox: TCheckListBox
        Left = 0
        Top = 0
        Width = 448
        Height = 578
        OnClickCheck = MainUnitWindowPageControlGeneralDebuggingCheckListBoxClickCheck
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyDown = MainUnitWindowPageControlGeneralDebuggingCheckListBoxKeyDown
        OnMouseDown = MainUnitWindowPageControlGeneralDebuggingCheckListBoxMouseDown
      end
    end
    object MainUnitWindowPageControlFeedbackDebuggingTabSheet: TTabSheet
      Caption = 'Feedback '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageIndex = 1
      ParentFont = False
      object MainUnitWindowPageControlFeedbackDebuggingCheckListBox: TCheckListBox
        Left = 0
        Top = 0
        Width = 448
        Height = 578
        OnClickCheck = MainUnitWindowPageControlFeedbackDebuggingCheckListBoxClickCheck
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyDown = MainUnitWindowPageControlFeedbackDebuggingCheckListBoxKeyDown
        OnMouseDown = MainUnitWindowPageControlFeedbackDebuggingCheckListBoxMouseDown
      end
    end
  end
  object MainUnitTimer: TTimer
    Interval = 1
    OnTimer = MainUnitTimerTick
    Left = 132
    Top = 48
  end
  object MainUnitWatchdogOneSecondTimer: TTimer
    OnTimer = MainUnitWatchdogOneSecondTimerTick
    Left = 272
    Top = 48
  end
end
