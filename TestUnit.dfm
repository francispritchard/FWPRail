object TestUnitForm: TTestUnitForm
  Left = 0
  Top = 0
  Align = alClient
  Caption = 'TestUnitForm'
  ClientHeight = 885
  ClientWidth = 1013
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  object TestUnitTimer: TTimer
    Interval = 100
    OnTimer = TestUnitTimerTick
    Left = 152
    Top = 88
  end
end
