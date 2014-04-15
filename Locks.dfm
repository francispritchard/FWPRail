object LockListWindow: TLockListWindow
  Left = 410
  Top = 443
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Locking List'
  ClientHeight = 1002
  ClientWidth = 722
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LockListWindowMemo: TMemo
    Left = 0
    Top = 0
    Width = 722
    Height = 1002
    Align = alClient
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyDown = LockListWindowMemoKeyDown
  end
end
