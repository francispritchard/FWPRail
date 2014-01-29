object CuneoWindow: TCuneoWindow
  Left = 473
  Top = 372
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Cuneo Window'
  ClientHeight = 386
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CuneoTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = CuneoTimerTick
    Left = 224
    Top = 16
  end
end
