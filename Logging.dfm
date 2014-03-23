object LoggingWindow: TLoggingWindow
  Left = 0
  Top = 0
  Caption = 'Log'
  ClientHeight = 336
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = LoggingWindowClose
  OnShow = LoggingWindowShow
  PixelsPerInch = 96
  TextHeight = 13
  object LoggingWindowRichEdit: TRichEdit
    Left = 0
    Top = 0
    Width = 527
    Height = 336
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Lucida Console'
    Font.Pitch = fpFixed
    Font.Style = []
    HideSelection = False
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyDown = LoggingWindowRichEditKeyDown
    OnMouseDown = LoggingWindowRichEditMouseDown
  end
  object LoggingWindowPopupMenu: TPopupMenu
    Left = 168
    Top = 184
    object LoggingWindowPopupFontSize: TMenuItem
      Caption = 'Font Size'
      object LoggingWindowPopupChangeFontSize: TMenuItem
        Caption = 'Change Font Attributes'
        OnClick = LoggingWindowPopupChangeFontSizeClick
      end
      object LoggingWindowPopupFontSizeRestoreDefault: TMenuItem
        Caption = 'Restore Default Font'
        OnClick = LoggingWindowPopupFontSizeRestoreDefaultClick
      end
    end
  end
  object LoggingWindowFontDialogue: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdFixedPitchOnly, fdForceFontExist]
    Left = 344
    Top = 176
  end
  object LoggingWindowFindDialog: TFindDialog
    OnClose = LoggingWindowFindDialogClose
    OnShow = LoggingWindowFindDialogShow
    OnFind = LoggingWindowFindDialogFind
    Left = 280
    Top = 256
  end
end
