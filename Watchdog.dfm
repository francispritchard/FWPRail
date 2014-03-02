object WatchdogWindow: TWatchdogWindow
  Left = 433
  Top = 228
  Caption = 'Rail Watchdog Window'
  ClientHeight = 537
  ClientWidth = 557
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = WatchdogWindowClose
  OnShow = WatchdogWindowShow
  DesignSize = (
    557
    537)
  PixelsPerInch = 96
  TextHeight = 13
  object IncomingGB: TGroupBox
    Left = 6
    Top = 168
    Width = 546
    Height = 366
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Traffic log : '
    TabOrder = 0
    DesignSize = (
      546
      366)
    object MSGMemo: TMemo
      Left = 3
      Top = 19
      Width = 529
      Height = 307
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
      Left = 3
      Top = 332
      Width = 95
      Height = 25
      Caption = 'Clear'
      TabOrder = 1
      OnClick = ClearButtonClick
    end
  end
  object ConnectPanel: TPanel
    Left = 3
    Top = 5
    Width = 546
    Height = 157
    Anchors = [akLeft, akTop, akRight]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      546
      157)
    object LabelTextEntry: TLabel
      Left = 8
      Top = 43
      Width = 75
      Height = 13
      Caption = 'TCP Text Entry:'
    end
    object SendTextButton: TSpeedButton
      Left = 8
      Top = 125
      Width = 100
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Send Text'
      OnClick = SendTextButtonClick
    end
    object SendStatusRequestButton: TSpeedButton
      Left = 128
      Top = 125
      Width = 121
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Send Status Request'
      OnClick = SendStatusRequestButtonClick
    end
    object TCPConnectButton: TButton
      Left = 6
      Top = 7
      Width = 95
      Height = 25
      Caption = 'TCP Connect'
      TabOrder = 0
      OnClick = TCPConnectButtonClick
    end
    object TCPCommand: TMemo
      Left = 8
      Top = 59
      Width = 529
      Height = 60
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object TCPIPTimer: TTimer
    Interval = 30000
    OnTimer = TCPIPTimerOnTimer
    Left = 94
    Top = 248
  end
end
