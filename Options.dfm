object OptionsWindow: TOptionsWindow
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Rail Options'
  ClientHeight = 824
  ClientWidth = 488
  Color = clSkyBlue
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  OnShow = OptionsWindowShow
  PixelsPerInch = 96
  TextHeight = 13
  object OptionsValueListEditor: TValueListEditor
    Left = 0
    Top = 0
    Width = 488
    Height = 824
    Align = alClient
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goThumbTracking]
    TabOrder = 0
    OnDrawCell = OptionsValueListEditorDrawCell
    OnKeyDown = OptionsValueListEditorKeyDown
    OnValidate = OptionsValueListEditorValidate
    ColWidths = (
      150
      332)
  end
  object OptionsWindowFindDialog: TFindDialog
    OnClose = OptionsWindowFindDialogClose
    OnShow = OptionsWindowFindDialogShow
    OnFind = OptionsWindowFindDialogFind
    Left = 264
    Top = 304
  end
end
