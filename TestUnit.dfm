object TestUnitForm: TTestUnitForm
  Left = 0
  Top = 0
  Align = alClient
  Caption = 'TestUnitForm'
  ClientHeight = 885
  ClientWidth = 1013
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object TButton
    Left = 272
    Top = 96
    Width = 75
    Height = 25
    TabOrder = 0
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 1013
    Height = 885
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
  object Button1: TButton
    Left = 280
    Top = 488
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 456
    Top = 488
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 632
    Top = 488
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Memo1: TMemo
    Left = 264
    Top = 136
    Width = 489
    Height = 321
    Lines.Strings = (
      '---LOG---'
      '')
    ScrollBars = ssVertical
    TabOrder = 5
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
  object TcpServer1: TTcpServer
    LocalHost = '192.168.0.3'
    LocalPort = '80'
    OnAccept = TcpServer1Accept
    Left = 584
    Top = 328
  end
end
