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
  object TrackCircuitsADOTable: TADOTable
    Connection = TrackCircuitsADOConnection
    CursorType = ctStatic
    BeforeDelete = TrackCircuitsADOTableBeforeDelete
    AfterDelete = TrackCircuitsADOTableAfterDelete
    TableName = 'TrackCircuitTable'
    Left = 344
    Top = 72
  end
  object TrackCircuitsADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\TrackCircuitData.mdb;Pers' +
      'ist Security Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 196
    Top = 76
  end
  object TrackCircuitDataSource: TDataSource
    Left = 52
    Top = 72
  end
end
