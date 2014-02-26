object EditWindow: TEditWindow
  Left = 0
  Top = 0
  Caption = 'Edit Window'
  ClientHeight = 302
  ClientWidth = 710
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = EditWindowCreate
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
  object SaveChangesButton: TButton
    Left = 597
    Top = 89
    Width = 105
    Height = 25
    Caption = 'Save Changes'
    Enabled = False
    TabOrder = 1
    OnClick = SaveChangesButtonClick
  end
  object ExitWithoutSavingButton: TButton
    Left = 597
    Top = 120
    Width = 105
    Height = 25
    Caption = 'Exit Without Saving'
    TabOrder = 2
    OnClick = ExitWithoutSavingButtonClick
  end
  object UndoChangesButton: TButton
    Left = 597
    Top = 58
    Width = 105
    Height = 25
    Caption = 'Undo Changes'
    Enabled = False
    TabOrder = 3
    OnClick = UndoChangesButtonClick
  end
end
