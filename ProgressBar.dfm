object RoutesWritingProgressBarWindow: TRoutesWritingProgressBarWindow
  Left = 0
  Top = 0
  Width = 434
  Height = 127
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Writing Out Routes'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnDblClick = RoutesWritingProgressBarDblClick
  PixelsPerInch = 96
  TextHeight = 13
  object RoutesWritingProgressBar: TProgressBar
    Left = 24
    Top = 16
    Width = 377
    Height = 25
    TabOrder = 0
  end
  object RoutesWritingCancelButton: TButton
    Left = 180
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = RoutesWritingCancelButtonClick
  end
end
