object CreateRouteDisplayColoursWindow: TCreateRouteDisplayColoursWindow
  Left = 0
  Top = 0
  Caption = 'CreateRouteDisplayColoursWindow'
  ClientHeight = 539
  ClientWidth = 600
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
  object CreateRouteDisplayColoursWindowRichEdit: TRichEdit
    Left = 0
    Top = 0
    Width = 600
    Height = 539
    Align = alClient
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    OnKeyDown = CreateRouteDisplayColoursWindowRichEditKeyDown
  end
  object RouteingExceptionDataSource: TDataSource
    Left = 308
    Top = 40
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
    Left = 356
    Top = 36
  end
  object RouteingExceptionDataADOTable: TADOTable
    Connection = RouteingExceptionDataADOConnection
    CursorType = ctStatic
    TableName = 'RouteingExceptionDataTable'
    Left = 404
    Top = 40
  end
end
