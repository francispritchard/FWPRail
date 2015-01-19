object DebugWindow: TDebugWindow
  Left = 544
  Top = 731
  ActiveControl = DebugRichEdit
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Debug Output '
  ClientHeight = 179
  ClientWidth = 543
  Color = clBtnFace
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = DebugWindowClose
  OnResize = DebugWindowResize
  PixelsPerInch = 96
  TextHeight = 13
  object DebugRichEdit: TRichEdit
    Left = 0
    Top = 0
    Width = 543
    Height = 179
    Align = alClient
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Pitch = fpFixed
    Font.Style = []
    HideScrollBars = False
    ParentFont = False
    PopupMenu = DebugRichEditPopupMenu
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyDown = DebugRichEditKeyDown
    OnMouseDown = DebugRichEditMouseDown
    OnMouseMove = DebugRichEditMouseMove
  end
  object DebugRichEditColourDialogue: TColorDialog
    Options = [cdPreventFullOpen, cdSolidColor]
    Left = 96
    Top = 32
  end
  object DebugRichEditPopupMenu: TPopupMenu
    Left = 336
    Top = 52
    object PopupDebugWindowResetSizeAndPosition: TMenuItem
      Caption = 'Reset Window Size && Position'
      OnClick = PopupDebugWindowResetSizeAndPositionClick
    end
    object PopupDebugWindowCopy: TMenuItem
      Caption = 'Copy To Clipboard'
      OnClick = PopupDebugWindowCopyClick
    end
  end
end
