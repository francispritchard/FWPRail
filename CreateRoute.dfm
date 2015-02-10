object DisplayLineColoursWindow: TDisplayLineColoursWindow
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsNone
  Caption = 'DisplayLineColoursWindow'
  ClientHeight = 178
  ClientWidth = 156
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
  object DisplayLineColoursWindowRichEdit: TRichEdit
    Left = 0
    Top = 0
    Width = 156
    Height = 178
    Align = alClient
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    OnClick = DisplayLineColoursWindowRichEditClick
    OnKeyDown = DisplayLineColoursWindowRichEditKeyDown
  end
  object RouteingExceptionDataSource: TDataSource
    Left = 92
    Top = 64
  end
  object RouteingExceptionDataADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\RouteingExceptionData.mdb' +
      ';Persist Security Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 84
    Top = 12
  end
  object RouteingExceptionDataADOTable: TADOTable
    Connection = RouteingExceptionDataADOConnection
    CursorType = ctStatic
    TableName = 'RouteingExceptionDataTable'
    Left = 84
    Top = 120
  end
end
