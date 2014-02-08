object LenzWindow: TLenzWindow
  Left = 655
  Top = 609
  Caption = 'LenzWindow'
  ClientHeight = 106
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LenzWatchdogTimer: TTimer
    Interval = 60000
    OnTimer = OnLenzWatchdogTimerInterval
    Left = 80
    Top = 48
  end
  object LenzOneMilliSecondTimerIntervalTimer: TTimer
    OnTimer = OnLenzOneMilliSecondTimerInterval
    Left = 264
    Top = 48
  end
end
