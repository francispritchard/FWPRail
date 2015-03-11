object FWPRailSpeechWindow: TFWPRailSpeechWindow
  Left = 433
  Top = 228
  Caption = 'FWP Rail Speech Window'
  ClientHeight = 554
  ClientWidth = 919
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  WindowState = wsMinimized
  OnClose = SpeechUnitWindowClose
  OnCreate = SpeechUnitWindowCreate
  OnShow = SpeechUnitWindowShow
  DesignSize = (
    919
    554)
  PixelsPerInch = 96
  TextHeight = 13
  object IncomingGB: TGroupBox
    Left = 3
    Top = 8
    Width = 908
    Height = 545
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Log:'
    TabOrder = 0
    DesignSize = (
      908
      545)
    object SpeechUnitMemo: TMemo
      Left = 0
      Top = 34
      Width = 451
      Height = 492
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object ClearButton: TButton
      Left = 370
      Top = 3
      Width = 95
      Height = 25
      Caption = 'Clear Log'
      TabOrder = 1
      OnClick = ClearButtonClick
    end
    object SpeechUnitListBox: TListBox
      Left = 471
      Top = 32
      Width = 426
      Height = 513
      ItemHeight = 13
      TabOrder = 2
    end
    object SpeechUnitRefreshListBoxButton: TButton
      Left = 727
      Top = 1
      Width = 170
      Height = 25
      Caption = 'Refresh List of Current Windows'
      TabOrder = 3
      OnClick = SpeechUnitRefreshListBoxButtonClick
    end
  end
  object SpeechUnitTimer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = SpeechUnitTimerTick
    Left = 166
    Top = 248
  end
  object SpeechUnitTrayIcon: TTrayIcon
    BalloonTitle = 'Rail Watchdog'
    Visible = True
    OnClick = SpeechUnitTrayIconClick
    Left = 259
    Top = 248
  end
end
