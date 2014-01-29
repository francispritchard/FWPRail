object RailDriverWindow: TRailDriverWindow
  Left = 397
  Top = 229
  Align = alCustom
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'RailDriver Window'
  ClientHeight = 259
  ClientWidth = 775
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = RailDriverWindowClose
  OnCreate = RailDriverWindowCreate
  OnKeyDown = RailDriverWindowKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object ThreeWayA: TLabel
    Left = 676
    Top = 12
    Width = 76
    Height = 13
    Caption = '3 Way Switch A'
  end
  object ThreeWayB: TLabel
    Left = 676
    Top = 68
    Width = 76
    Height = 13
    Caption = '3 Way Switch B'
  end
  object BailOffLabel: TLabel
    Left = 568
    Top = 12
    Width = 34
    Height = 13
    Alignment = taCenter
    Caption = 'Bail Off'
  end
  object LocoBrakeLabel: TLabel
    Left = 484
    Top = 12
    Width = 55
    Height = 13
    Alignment = taCenter
    Caption = 'Loco Brake'
  end
  object TrainBrakeLabel: TLabel
    Left = 404
    Top = 12
    Width = 55
    Height = 13
    Alignment = taCenter
    Caption = 'Train Brake'
  end
  object RegulatorLabel: TLabel
    Left = 332
    Top = 12
    Width = 46
    Height = 13
    Alignment = taCenter
    Caption = 'Regulator'
  end
  object ReverserLabel: TLabel
    Left = 259
    Top = 12
    Width = 43
    Height = 13
    Alignment = taCenter
    Caption = 'Reverser'
  end
  object ReverserValue: TLabel
    Left = 252
    Top = 28
    Width = 61
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object RegulatorValue: TLabel
    Left = 328
    Top = 28
    Width = 61
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object TrainBrakeValue: TLabel
    Left = 404
    Top = 28
    Width = 61
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LocoBrakeValue: TLabel
    Left = 480
    Top = 28
    Width = 61
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object BailOffValue: TLabel
    Left = 556
    Top = 28
    Width = 61
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ThreeWaySwitchAValue: TLabel
    Left = 684
    Top = 28
    Width = 61
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ThreeWaySwitchBValue: TLabel
    Left = 684
    Top = 80
    Width = 61
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object WriteButton: TButton
    Left = 648
    Top = 152
    Width = 113
    Height = 25
    Caption = 'Test Write to LED'
    TabOrder = 0
    OnClick = WriteButtonClick
  end
  object Button15: TButton
    Left = 56
    Top = 220
    Width = 33
    Height = 25
    Caption = '15'
    TabOrder = 1
  end
  object Button16: TButton
    Left = 96
    Top = 220
    Width = 33
    Height = 25
    Caption = '16'
    TabOrder = 2
  end
  object Button17: TButton
    Left = 136
    Top = 220
    Width = 33
    Height = 25
    Caption = '17'
    TabOrder = 3
  end
  object Button18: TButton
    Left = 176
    Top = 220
    Width = 33
    Height = 25
    Caption = '18'
    TabOrder = 4
  end
  object Button22: TButton
    Left = 336
    Top = 220
    Width = 33
    Height = 25
    Caption = '22'
    TabOrder = 5
  end
  object Button19: TButton
    Left = 216
    Top = 220
    Width = 33
    Height = 25
    Caption = '19'
    TabOrder = 6
  end
  object Button20: TButton
    Left = 256
    Top = 220
    Width = 33
    Height = 25
    Caption = '20'
    TabOrder = 7
  end
  object Button21: TButton
    Left = 296
    Top = 220
    Width = 33
    Height = 25
    Caption = '21'
    TabOrder = 8
  end
  object Button26: TButton
    Left = 496
    Top = 220
    Width = 33
    Height = 25
    Caption = '26'
    TabOrder = 9
  end
  object Button23: TButton
    Left = 376
    Top = 220
    Width = 33
    Height = 25
    Caption = '23'
    TabOrder = 10
  end
  object Button24: TButton
    Left = 416
    Top = 220
    Width = 33
    Height = 25
    Caption = '24'
    TabOrder = 11
  end
  object Button25: TButton
    Left = 456
    Top = 220
    Width = 33
    Height = 25
    Caption = '25'
    TabOrder = 12
  end
  object Button27: TButton
    Left = 536
    Top = 220
    Width = 33
    Height = 25
    Caption = '27'
    TabOrder = 13
  end
  object Button3: TButton
    Left = 136
    Top = 188
    Width = 33
    Height = 25
    Caption = '3'
    TabOrder = 14
  end
  object Button1: TButton
    Left = 56
    Top = 188
    Width = 33
    Height = 25
    Caption = '1'
    TabOrder = 15
  end
  object Button2: TButton
    Left = 96
    Top = 188
    Width = 33
    Height = 25
    Caption = '2'
    TabOrder = 16
  end
  object Button4: TButton
    Left = 176
    Top = 188
    Width = 33
    Height = 25
    Caption = '4'
    TabOrder = 17
  end
  object Button5: TButton
    Left = 216
    Top = 188
    Width = 33
    Height = 25
    Caption = '5'
    TabOrder = 18
  end
  object Button6: TButton
    Left = 256
    Top = 188
    Width = 33
    Height = 25
    Caption = '6'
    TabOrder = 19
  end
  object Button7: TButton
    Left = 296
    Top = 188
    Width = 33
    Height = 25
    Caption = '7'
    TabOrder = 20
  end
  object Button8: TButton
    Left = 336
    Top = 188
    Width = 33
    Height = 25
    Caption = '8'
    TabOrder = 21
  end
  object Button9: TButton
    Left = 376
    Top = 188
    Width = 33
    Height = 25
    Caption = '9'
    TabOrder = 22
  end
  object Button10: TButton
    Left = 416
    Top = 188
    Width = 33
    Height = 25
    Caption = '10'
    TabOrder = 23
  end
  object Button11: TButton
    Left = 456
    Top = 188
    Width = 33
    Height = 25
    Caption = '11'
    TabOrder = 24
  end
  object Button12: TButton
    Left = 496
    Top = 188
    Width = 33
    Height = 25
    Caption = '12'
    TabOrder = 25
  end
  object Button13: TButton
    Left = 536
    Top = 188
    Width = 33
    Height = 25
    Caption = '13'
    TabOrder = 26
  end
  object Button28: TButton
    Left = 592
    Top = 188
    Width = 33
    Height = 25
    Caption = '28'
    TabOrder = 27
  end
  object Button29: TButton
    Left = 592
    Top = 220
    Width = 33
    Height = 25
    Caption = '29'
    TabOrder = 28
  end
  object Button30: TButton
    Left = 688
    Top = 188
    Width = 33
    Height = 25
    Caption = '30'
    TabOrder = 29
  end
  object Button32: TButton
    Left = 688
    Top = 220
    Width = 33
    Height = 25
    Caption = '32'
    TabOrder = 30
  end
  object Button33: TButton
    Left = 648
    Top = 204
    Width = 33
    Height = 25
    Caption = '33'
    TabOrder = 31
  end
  object Button31: TButton
    Left = 728
    Top = 204
    Width = 33
    Height = 25
    Caption = '31'
    TabOrder = 32
  end
  object Button0: TButton
    Left = 16
    Top = 188
    Width = 33
    Height = 25
    Caption = '0'
    TabOrder = 33
  end
  object Button14: TButton
    Left = 16
    Top = 220
    Width = 33
    Height = 25
    Caption = '14'
    TabOrder = 34
  end
  object Button34: TButton
    Left = 32
    Top = 16
    Width = 65
    Height = 17
    Caption = '34'
    TabOrder = 35
  end
  object Button35: TButton
    Left = 32
    Top = 32
    Width = 65
    Height = 17
    Caption = '35'
    TabOrder = 36
  end
  object Button36: TButton
    Left = 96
    Top = 16
    Width = 65
    Height = 17
    Caption = '36'
    TabOrder = 37
  end
  object Button37: TButton
    Left = 96
    Top = 32
    Width = 65
    Height = 17
    Caption = '37'
    TabOrder = 38
  end
  object Button39: TButton
    Left = 112
    Top = 76
    Width = 41
    Height = 33
    Caption = '39'
    TabOrder = 39
  end
  object Button38: TButton
    Left = 40
    Top = 64
    Width = 57
    Height = 49
    Caption = '38'
    TabOrder = 40
  end
  object Button40: TButton
    Left = 52
    Top = 140
    Width = 25
    Height = 25
    Caption = '40'
    TabOrder = 41
  end
  object Button41: TButton
    Left = 104
    Top = 124
    Width = 57
    Height = 49
    Caption = '41'
    TabOrder = 42
  end
  object Button42: TButton
    Left = 184
    Top = 16
    Width = 33
    Height = 25
    Caption = '42'
    TabOrder = 43
  end
  object Button43: TButton
    Left = 184
    Top = 48
    Width = 33
    Height = 25
    Caption = '43'
    TabOrder = 44
  end
  object ReverserForwardProgressBar: TProgressBar
    Left = 276
    Top = 68
    Width = 17
    Height = 49
    Orientation = pbVertical
    Smooth = True
    TabOrder = 45
  end
  object RegulatorForwardProgressBar: TProgressBar
    Left = 336
    Top = 68
    Width = 17
    Height = 49
    Orientation = pbVertical
    Smooth = True
    TabOrder = 46
  end
  object ReverserReverseProgressBar: TProgressBar
    Left = 276
    Top = 120
    Width = 17
    Height = 49
    Orientation = pbVertical
    Smooth = True
    TabOrder = 47
  end
  object RegulatorReverseProgressBar: TProgressBar
    Left = 336
    Top = 120
    Width = 17
    Height = 49
    Orientation = pbVertical
    Smooth = True
    TabOrder = 48
  end
  object RegulatorPosProgressBar: TProgressBar
    Left = 364
    Top = 68
    Width = 17
    Height = 101
    Max = 9
    Orientation = pbVertical
    TabOrder = 49
  end
  object RailDriverTimer: TTimer
    Enabled = False
    Interval = 10
    OnTimer = RailDriverTimerTick
    Left = 192
    Top = 128
  end
  object LEDTimer: TTimer
    Interval = 250
    OnTimer = LEDTimerTick
    Left = 192
    Top = 84
  end
end
