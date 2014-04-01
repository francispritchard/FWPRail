object TestUnitForm: TTestUnitForm
  Left = 0
  Top = 0
  Align = alClient
  Caption = 'TestUnitForm'
  ClientHeight = 915
  ClientWidth = 1013
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 272
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 1013
    Height = 915
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
  object TestUnitFormFontDialogue: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 180
    Top = 148
  end
end
