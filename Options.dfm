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
  OnCreate = OptionsWindowCreate
  OnHide = OptionsWindowHide
  OnShow = OptionsWindowShow
  PixelsPerInch = 96
  TextHeight = 13
  object OptionsValueListEditor: TValueListEditor
    Left = 0
    Top = 0
    Width = 488
    Height = 824
    Align = alClient
    Color = clCaptionText
    Strings.Strings = (
      '=')
    TabOrder = 0
    TitleCaptions.Strings = (
      'Option'
      'Value')
    OnDrawCell = OptionsValueListEditorDrawCell
    OnKeyDown = OptionsValueListEditorKeyDown
    OnValidate = OptionsValueListEditorValidate
    ColWidths = (
      285
      197)
  end
  object OptionsWindowFindDialog: TFindDialog
    OnClose = OptionsWindowFindDialogClose
    OnShow = OptionsWindowFindDialogShow
    OnFind = OptionsWindowFindDialogFind
    Left = 264
    Top = 304
  end
end
