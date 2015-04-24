object WatchdogUnitWindow: TWatchdogUnitWindow
  Left = 433
  Top = 228
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Watchdog Unit Window'
  ClientHeight = 593
  ClientWidth = 933
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
  OnCreate = WatchdogUnitWindowCreate
  OnShow = WatchdogUnitWindowShow
  DesignSize = (
    933
    593)
  PixelsPerInch = 96
  TextHeight = 13
  object WatchdogUnitIncomingGroupBox: TGroupBox
    Left = 3
    Top = 168
    Width = 922
    Height = 422
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Traffic log : '
    TabOrder = 0
    DesignSize = (
      922
      422)
    object WatchdogUnitMemo: TMemo
      Left = 3
      Top = 19
      Width = 454
      Height = 363
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
    object WatchdogUnitClearButton: TButton
      Left = 0
      Top = 388
      Width = 95
      Height = 25
      Caption = 'Clear'
      TabOrder = 1
      OnClick = WatchdogUnitClearButtonClick
    end
    object WatchdogUnitListBox: TListBox
      Left = 463
      Top = 19
      Width = 429
      Height = 366
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object WatchdogUnitConnectPanel: TPanel
    Left = 3
    Top = 5
    Width = 922
    Height = 157
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      922
      157)
    object WatchdogUnitLabelTextEntry: TLabel
      Left = 8
      Top = 43
      Width = 75
      Height = 13
      Caption = 'TCP Text Entry:'
    end
    object WatchdogUnitSendTextButton: TSpeedButton
      Left = 8
      Top = 125
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Send Text'
      OnClick = WatchdogUnitSendTextButtonClick
    end
    object WatchdogUnitSendStatusRequestButton: TSpeedButton
      Left = 152
      Top = 125
      Width = 121
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Send Status Request'
      OnClick = WatchdogUnitSendStatusRequestButtonClick
    end
    object WatchdogUnitUSBConnectButton: TButton
      Left = 8
      Top = 12
      Width = 95
      Height = 25
      Caption = 'USB Connect'
      TabOrder = 0
      OnClick = WatchdogUnitUSBConnectButtonClick
    end
    object WatchdogUnitTCPCommand: TMemo
      Left = 8
      Top = 59
      Width = 905
      Height = 60
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object WatchdogUnitEthernetConnectButton: TButton
      Left = 134
      Top = 12
      Width = 95
      Height = 25
      Caption = 'Ethernet Connect'
      TabOrder = 2
      OnClick = WatchdogUnitEthernetConnectButtonClick
    end
    object WatchdogUnitRefreshListBoxButton: TButton
      Left = 463
      Top = 125
      Width = 178
      Height = 25
      Caption = 'Refresh List of Current Windows'
      TabOrder = 3
      OnClick = WatchdogUnitRefreshListBoxButtonClick
    end
    object WatchdogUnitCloseButton: TButton
      Left = 817
      Top = 12
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 4
      OnClick = WatchdogUnitCloseButtonClick
    end
  end
  object WatchdogUnitMinimiseButton: TButton
    Left = 728
    Top = 17
    Width = 75
    Height = 25
    Caption = 'Minimise'
    TabOrder = 2
    OnClick = WatchdogUnitMinimiseButtonClick
  end
  object WatchdogUnitTCPIPTimer: TTimer
    Enabled = False
    Interval = 30000
    OnTimer = WatchdogUnitTCPIPTimerOnTick
    Left = 94
    Top = 248
  end
  object WatchdogUnitTimer: TTimer
    Interval = 1
    OnTimer = WatchdogUnitTimerTick
    Left = 206
    Top = 256
  end
  object WatchdogUnitTrayIcon: TTrayIcon
    BalloonTitle = 'Rail Watchdog'
    Visible = True
    OnMouseDown = WatchdogUnitTrayIconMouseDown
    Left = 339
    Top = 248
  end
  object WatchdogUnitPopupMenu: TPopupMenu
    Left = 99
    Top = 360
    object WatchdogUnitPopupClose: TMenuItem
      Caption = 'Close'
      OnClick = WatchdogUnitPopupCloseClick
    end
  end
end
