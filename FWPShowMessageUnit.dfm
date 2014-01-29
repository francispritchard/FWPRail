object FWPShowMessageWindow: TFWPShowMessageWindow
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = '='
  ClientHeight = 100
  ClientWidth = 194
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object FWPShowMessageLabel: TLabel
    Left = 16
    Top = 16
    Width = 171
    Height = 16
    Caption = 'FWP'#39's ShowMessage Window'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object OKButton: TButton
    Left = 16
    Top = 56
    Width = 89
    Height = 25
    Caption = 'Button not in use'
    TabOrder = 0
  end
end
