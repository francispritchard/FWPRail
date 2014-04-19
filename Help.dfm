object HelpWindow: THelpWindow
  Left = 0
  Top = 0
  Caption = 'HelpWindow'
  ClientHeight = 663
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object HelpRichEdit: TRichEdit
    Left = 0
    Top = 0
    Width = 900
    Height = 663
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'HelpEdit')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyDown = HelpRichEditKeyDown
    OnMouseDown = HelpRichEditMouseDown
    ExplicitHeight = 644
  end
  object HelpWindowFindDialog: TFindDialog
    OnClose = HelpWindowFindDialogClose
    OnShow = HelpWindowFindDialogShow
    OnFind = HelpWindowFindDialogFind
    Left = 132
    Top = 176
  end
end
