object MainUnitWindow: TMainUnitWindow
  Left = 0
  Top = 0
  Caption = 'MainWindow'
  ClientHeight = 336
  ClientWidth = 527
  Color = clBtnFace
  Enabled = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = MainUnitWindowCreate
  PixelsPerInch = 96
  TextHeight = 13
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
