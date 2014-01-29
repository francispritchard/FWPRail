object InitVarsWindow: TInitVarsWindow
  Left = 400
  Top = 484
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'InitVars Window'
  ClientHeight = 536
  ClientWidth = 635
  Color = clBtnFace
  Enabled = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -9
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object UnderwayOrCompleted1Label: TLabel
    Left = 234
    Top = 158
    Width = 5
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -9
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object UnderwayOrCompleted2Label: TLabel
    Left = 234
    Top = 184
    Width = 5
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -9
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object SaveDialogue: TSaveDialog
    DefaultExt = 'tim'
    Filter = 'Timetable Files (*.tim)|*.tim'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoReadOnlyReturn, ofEnableSizing]
    Title = 'Save Timetable File'
    Left = 44
    Top = 416
  end
  object SignalsDataSource: TDataSource
    Left = 44
    Top = 312
  end
  object SignalsADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\SignalData.mdb;Persist Se' +
      'curity Info=False;'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 164
    Top = 308
  end
  object SignalsADOTable: TADOTable
    Connection = SignalsADOConnection
    CursorType = ctStatic
    TableName = 'SignalTable'
    Left = 252
    Top = 304
  end
  object PointsADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\PointData.mdb;Persist Sec' +
      'urity Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 164
    Top = 260
  end
  object PointsDataSource: TDataSource
    Left = 44
    Top = 264
  end
  object PointsADOTable: TADOTable
    Connection = PointsADOConnection
    CursorType = ctStatic
    TableName = 'PointTable'
    Left = 248
    Top = 256
  end
  object LocationsDataSource: TDataSource
    Left = 44
    Top = 216
  end
  object LocationsADOConnection: TADOConnection
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 164
    Top = 204
  end
  object LocationsADOTable: TADOTable
    Connection = LocationsADOConnection
    CursorType = ctStatic
    TableName = 'LocationTable'
    Left = 240
    Top = 208
  end
  object AreasDataSource: TDataSource
    Left = 44
    Top = 160
  end
  object AreasADOConnection: TADOConnection
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 164
    Top = 148
  end
  object AreasADOTable: TADOTable
    Connection = AreasADOConnection
    CursorType = ctStatic
    TableName = 'AreaTable'
    Left = 240
    Top = 152
  end
  object TrackCircuitDataSource: TDataSource
    Left = 52
    Top = 96
  end
  object TrackCircuitDataADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\TrackCircuitData.mdb;Pers' +
      'ist Security Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 156
    Top = 92
  end
  object TrackCircuitDataADOTable: TADOTable
    Connection = TrackCircuitDataADOConnection
    CursorType = ctStatic
    TableName = 'TrackCircuitTable'
    Left = 224
    Top = 88
  end
  object PlatformDataSource: TDataSource
    Left = 44
    Top = 48
  end
  object PlatformDataADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\PlatformData.mdb;Persist ' +
      'Security Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 156
    Top = 44
  end
  object PlatformDataADOTable: TADOTable
    Connection = PlatformDataADOConnection
    CursorType = ctStatic
    TableName = 'PlatformTable'
    Left = 232
    Top = 40
  end
  object FeedbackUnitDataSource: TDataSource
    Left = 44
    Top = 360
  end
  object FeedbackUnitDataADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\FeedbackUnitData.mdb;Pers' +
      'ist Security Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 164
    Top = 356
  end
  object FeedbackUnitDataADOTable: TADOTable
    Connection = FeedbackUnitDataADOConnection
    CursorType = ctStatic
    TableName = 'FeedbackUnitTable'
    Left = 252
    Top = 360
  end
  object LineDataSource: TDataSource
    Left = 52
    Top = 16
  end
  object LineDataADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\3.0\Projects\Rail Data Files\LineData.mdb;Persist Secu' +
      'rity Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 228
    Top = 12
  end
  object LineDataADOTable: TADOTable
    Connection = LineDataADOConnection
    CursorType = ctStatic
    TableName = 'LineTable'
    Left = 152
    Top = 8
  end
  object SignalsDataSource2: TDataSource
    Left = 316
    Top = 304
  end
  object SignalsADOTable2: TADOTable
    Connection = SignalsADOConnection2
    CursorType = ctStatic
    TableName = 'SignalTable'
    Left = 412
    Top = 304
  end
  object SignalsADOConnection2: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\SignalData.mdb;Persist Se' +
      'curity Info=False;'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 364
    Top = 308
  end
  object PointsADOTable2: TADOTable
    Connection = PointsADOConnection2
    CursorType = ctStatic
    TableName = 'PointTable'
    Left = 408
    Top = 256
  end
  object PointsADOConnection2: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\PointData.mdb;Persist Sec' +
      'urity Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 364
    Top = 252
  end
  object PointsDataSource2: TDataSource
    Left = 308
    Top = 256
  end
end
