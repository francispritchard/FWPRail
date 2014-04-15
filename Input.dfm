object InputDialogueBox: TInputDialogueBox
  Left = 794
  Top = 567
  Anchors = [akLeft, akBottom]
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Find/Change Item'
  ClientHeight = 233
  ClientWidth = 602
  Color = clBtnFace
  DockSite = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnHide = InputDialogueBoxHide
  OnKeyDown = InputDialogueBoxKeyDown
  OnKeyUp = InputDialogueBoxKeyUp
  OnMouseDown = InputDialogueMouseDown
  OnMouseMove = InputDialogueMouseMove
  OnShow = InputDialogueBoxShow
  PixelsPerInch = 96
  TextHeight = 13
  object InputDialogueMaskEditLabel: TLabel
    Left = 12
    Top = 8
    Width = 63
    Height = 13
    Caption = 'Item Number:'
  end
  object InputDialogueChangeOrSelectButton: TButton
    Left = 4
    Top = 36
    Width = 85
    Height = 25
    Caption = 'Change/Select'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = InputDialogueChangeOrSelectButtonClick
  end
  object InputDialogueCancelButton: TButton
    Left = 100
    Top = 36
    Width = 57
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = InputDialogueCancelButtonClick
  end
  object InputDialogueMaskEdit: TMaskEdit
    Left = 81
    Top = 8
    Width = 23
    Height = 21
    TabOrder = 0
    Text = ''
    OnChange = InputDialogueMaskEditChange
    OnKeyPress = InputDialogueMaskEditKeyPress
  end
  object InputDialogueShowAdjacentTrackCircuitsCheckBox: TCheckBox
    Left = 8
    Top = 75
    Width = 161
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Show Adjacent Track Circuits'
    TabOrder = 3
    OnClick = InputDialogueShowAdjacentTrackCircuitsCheckBoxClick
  end
end
