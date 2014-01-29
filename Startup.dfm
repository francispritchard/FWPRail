object DebuggingOptionsWindow: TDebuggingOptionsWindow
  Left = 785
  Top = 415
  BorderIcons = [biSystemMenu]
  Caption = 'Debugging Options'
  ClientHeight = 354
  ClientWidth = 217
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnHide = DebuggingOptionsWindowHide
  OnKeyDown = DebuggingOptionsWindowKeyDown
  OnShow = DebuggingOptionsWindowShow
  PixelsPerInch = 96
  TextHeight = 13
  object Startup_DebuggingCheckBox: TCheckBox
    Left = 28
    Top = 16
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = '&Debugging'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Startup_DebuggingCheckBoxClick
  end
  object Startup_FeedbackDebuggingCheckBox: TCheckBox
    Left = 28
    Top = 40
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = '&Feedback Debugging'
    TabOrder = 1
    OnClick = Startup_FeedbackDebuggingCheckBoxClick
  end
  object Startup_LockDebuggingCheckBox: TCheckBox
    Left = 28
    Top = 88
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = 'L&ock Debugging'
    TabOrder = 3
    OnClick = Startup_LockDebuggingCheckBoxClick
  end
  object Startup_PointDebuggingCheckBox: TCheckBox
    Left = 28
    Top = 112
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = '&Point Debugging'
    TabOrder = 4
    OnClick = Startup_PointDebuggingCheckBoxClick
  end
  object Startup_LineDebuggingCheckBox: TCheckBox
    Left = 28
    Top = 64
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = 'L&ine Debugging'
    TabOrder = 2
    OnClick = Startup_LineDebuggingCheckBoxClick
  end
  object Startup_RouteDebuggingCheckBox: TCheckBox
    Left = 28
    Top = 136
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = '&Route Debugging'
    TabOrder = 5
    OnClick = Startup_RouteDebuggingCheckBoxClick
  end
  object Startup_TestingCheckBox: TCheckBox
    Left = 28
    Top = 312
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = '&Testing'
    TabOrder = 7
    OnClick = Startup_TestingCheckBoxClick
  end
  object Startup_LockingCheckBox: TCheckBox
    Left = 28
    Top = 240
    Width = 161
    Height = 18
    Alignment = taLeftJustify
    Caption = '&Locking Off'
    TabOrder = 6
    OnClick = Startup_LockingCheckBoxClick
  end
  object Startup_RouteDrawingCheckBox: TCheckBox
    Left = 28
    Top = 208
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Route Dra&wing'
    TabOrder = 8
    OnClick = Startup_RouteDrawingCheckBoxClick
  end
  object Startup_RecordLineDrawingCheckBox: TCheckBox
    Left = 28
    Top = 288
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Record Li&ne Drawing'
    TabOrder = 9
    OnClick = Startup_RecordLineDrawingCheckBoxClick
  end
  object Startup_AllRouteDebuggingCheckBox: TCheckBox
    Left = 28
    Top = 184
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = '&All Route Debugging'
    TabOrder = 10
    OnClick = Startup_AllRouteDebuggingCheckBoxClick
  end
  object Startup_RouteBacktrackDebuggingCheckBox: TCheckBox
    Left = 28
    Top = 160
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Route &Backtrack Debugging'
    TabOrder = 11
    OnClick = Startup_RouteBacktrackDebuggingCheckBoxClick
  end
  object Startup_LogsKeptCheckBox: TCheckBox
    Left = 28
    Top = 264
    Width = 161
    Height = 18
    Alignment = taLeftJustify
    Caption = '&Logs Kept'
    TabOrder = 12
    OnClick = Startup_LogsKeptCheckBoxClick
  end
end
