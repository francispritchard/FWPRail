object MovementWindow: TMovementWindow
  Left = 400
  Top = 420
  Caption = 'Rail Edit Checklistbox'
  ClientHeight = 835
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = MovementWindowCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SignalLocationsToMonitorCheckListBox: TCheckListBox
    Left = 0
    Top = 0
    Width = 436
    Height = 835
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
  end
  object SignalLocationsToMonitorOKButton: TButton
    Left = 312
    Top = 160
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = SignalLocationsToMonitorOKButtonClick
  end
  object SignalLocationsToMonitorCancelButton: TButton
    Left = 312
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = SignalLocationsToMonitorCancelButtonClick
  end
end
