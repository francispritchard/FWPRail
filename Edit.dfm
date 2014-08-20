object EditWindow: TEditWindow
  Left = 0
  Top = 0
  Caption = 'Edit Window'
  ClientHeight = 302
  ClientWidth = 750
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = EditWindowPopupMenu
  OnResize = EditWindowResize
  OnShow = EditWindowShow
  PixelsPerInch = 96
  TextHeight = 13
  object EditWindowLabel: TLabel
    Left = 24
    Top = 59
    Width = 6
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object EditValueListEditor: TValueListEditor
    Left = 128
    Top = 8
    Width = 450
    Height = 177
    TabOrder = 0
    OnEditButtonClick = EditValueListEditorEditButtonClick
    OnExit = EditValueListEditorExit
    OnStringsChange = EditValueListEditorStringsChange
    OnValidate = EditValueListEditorValidate
    ColWidths = (
      245
      199)
  end
  object SaveChangesAndExitButton: TButton
    Left = 600
    Top = 94
    Width = 142
    Height = 25
    Caption = 'Save Changes and Exit'
    Enabled = False
    TabOrder = 1
    OnClick = SaveChangesAndExitButtonClick
  end
  object ExitWithoutSavingButton: TButton
    Left = 600
    Top = 125
    Width = 142
    Height = 25
    Caption = 'Exit Without Saving'
    Enabled = False
    TabOrder = 2
    OnClick = ExitWithoutSavingButtonClick
  end
  object UndoChangesButton: TButton
    Left = 600
    Top = 60
    Width = 142
    Height = 28
    Caption = 'Undo Changes'
    Enabled = False
    TabOrder = 3
    OnClick = UndoChangesButtonClick
  end
  object EditWindowPopupMenu: TPopupMenu
    OnPopup = EditWindowPopupMenuPopup
    Left = 584
    Top = 216
    object PopupEditWindowResetSizeAndPosition: TMenuItem
      Caption = 'Reset Window Size And Position'
      OnClick = EditWindowPopupResetSizeAndPositionClick
    end
  end
end
