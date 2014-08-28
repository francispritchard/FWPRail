object ClockWindow: TClockWindow
  Left = 749
  Top = 652
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Caption and time set by calling routines'
  ClientHeight = 60
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = ClockWindowKeyDown
  OnMouseWheel = ClockWindowMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object Clock: TDateTimePicker
    Left = 1
    Top = 2
    Width = 185
    Height = 50
    BevelOuter = bvRaised
    BevelKind = bkSoft
    Date = 0.000011574074074074
    Time = 0.000011574074074074
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -37
    Font.Name = 'Arial'
    Font.Style = []
    Kind = dtkTime
    ParentFont = False
    TabOrder = 0
  end
  object OKButton: TButton
    Left = 192
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 192
    Top = 32
    Width = 75
    Height = 21
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
  object GetTimeTimer: TTimer
    Enabled = False
    Interval = 220
    OnTimer = GetTimeTimerTick
    Left = 212
    Top = 12
  end
end
