object EditWindow: TEditWindow
  Left = 0
  Top = 0
  Caption = 'Edit Window'
  ClientHeight = 302
  ClientWidth = 750
  Color = clBtnFace
  Enabled = False
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
    Top = 20
    Width = 89
    Height = 24
    Caption = 'Editing...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ShowAccelChar = False
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
  object EditWindowButtonPanel: TPanel
    Left = 592
    Top = 48
    Width = 137
    Height = 89
    Caption = 'EditWindowButtonPanel'
    TabOrder = 1
    object SaveChangesAndExitButton: TButton
      Left = 0
      Top = 31
      Width = 137
      Height = 25
      Caption = 'Save Changes and Exit'
      Enabled = False
      TabOrder = 0
      OnClick = SaveChangesAndExitButtonClick
    end
    object UndoChangesButton: TButton
      Left = 0
      Top = 0
      Width = 137
      Height = 25
      Caption = 'Undo Changes'
      Enabled = False
      TabOrder = 1
      OnClick = UndoChangesButtonClick
    end
    object ExitWithoutSavingButton: TButton
      Left = 0
      Top = 62
      Width = 137
      Height = 25
      Caption = 'Exit Without Saving'
      Enabled = False
      TabOrder = 2
      OnClick = ExitWithoutSavingButtonClick
    end
  end
  object EditWindowPopupMenu: TPopupMenu
    Left = 584
    Top = 216
    object PopupEditWindowResetSizeAndPosition: TMenuItem
      Caption = 'Reset Window Size And Position'
      OnClick = EditWindowPopupResetSizeAndPositionClick
    end
  end
end
