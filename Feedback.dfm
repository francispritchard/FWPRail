object FeedbackWindow: TFeedbackWindow
  Left = 419
  Top = 619
  Anchors = []
  AutoSize = True
  BorderStyle = bsNone
  ClientHeight = 107
  ClientWidth = 687
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnKeyDown = FeedbackWindowKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object FeedbackLabel: TLabel
    Left = 0
    Top = 0
    Width = 687
    Height = 107
    Align = alCustom
    Caption = 'Feedback on/off'
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -96
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object FeedbackMediaPlayer: TMediaPlayer
    Left = 368
    Top = 64
    Width = 253
    Height = 30
    Visible = False
    TabOrder = 0
  end
  object FeedbackWindowTimer: TTimer
    Enabled = False
    Interval = 2500
    OnTimer = FeedbackWindowTimerTick
    Left = 28
    Top = 56
  end
end
