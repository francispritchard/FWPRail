object MainWindow: TMainWindow
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
  OnCreate = MainWindowCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MainTimer: TTimer
    Interval = 1
    OnTimer = MainTimerTick
    Left = 132
    Top = 48
  end
end
