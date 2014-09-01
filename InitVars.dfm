object InitVarsWindow: TInitVarsWindow
  Left = 400
  Top = 484
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'InitVars Window'
  ClientHeight = 931
  ClientWidth = 635
  Color = clBtnFace
  Enabled = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -9
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
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
  object StationMonitorsWebDiagnosticsMemo: TMemo
    Left = 0
    Top = 0
    Width = 635
    Height = 931
    Align = alClient
    Lines.Strings = (
      '---LOG---'
      '')
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitLeft = -8
    ExplicitTop = -16
  end
  object SaveDialogue: TSaveDialog
    DefaultExt = 'tim'
    Filter = 'Timetable Files (*.tim)|*.tim'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoReadOnlyReturn, ofEnableSizing]
    Title = 'Save Timetable File'
    Left = 532
    Top = 576
  end
  object SignalsDataSource: TDataSource
    Left = 84
    Top = 368
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
    Left = 380
    Top = 372
  end
  object SignalsADOTable: TADOTable
    Connection = SignalsADOConnection
    CursorType = ctStatic
    TableName = 'SignalTable'
    Left = 252
    Top = 376
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
    Left = 236
    Top = 308
  end
  object PointsDataSource: TDataSource
    Left = 84
    Top = 296
  end
  object PointsADOTable: TADOTable
    Connection = PointsADOConnection
    CursorType = ctStatic
    TableName = 'PointTable'
    Left = 376
    Top = 304
  end
  object LocationsDataSource: TDataSource
    Left = 68
    Top = 240
  end
  object LocationsADOConnection: TADOConnection
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 212
    Top = 236
  end
  object LocationsADOTable: TADOTable
    Connection = LocationsADOConnection
    CursorType = ctStatic
    TableName = 'LocationTable'
    Left = 368
    Top = 232
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
    Left = 204
    Top = 164
  end
  object AreasADOTable: TADOTable
    Connection = AreasADOConnection
    CursorType = ctStatic
    TableName = 'AreaTable'
    Left = 344
    Top = 160
  end
  object TrackCircuitDataSource: TDataSource
    Left = 52
    Top = 96
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
    Top = 84
  end
  object TrackCircuitsADOTable: TADOTable
    Connection = TrackCircuitsADOConnection
    CursorType = ctStatic
    TableName = 'TrackCircuitTable'
    Left = 352
    Top = 80
  end
  object PlatformDataSource: TDataSource
    Left = 44
    Top = 24
  end
  object PlatformsADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\PlatformData.mdb;Persist ' +
      'Security Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 196
    Top = 28
  end
  object PlatformsADOTable: TADOTable
    Connection = PlatformsADOConnection
    CursorType = ctStatic
    TableName = 'PlatformTable'
    Left = 352
    Top = 16
  end
  object FeedbackUnitDataSource: TDataSource
    Left = 60
    Top = 536
  end
  object FeedbackUnitsADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\FeedbackUnitData.mdb;Pers' +
      'ist Security Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 332
    Top = 532
  end
  object FeedbackUnitsADOTable: TADOTable
    Connection = FeedbackUnitsADOConnection
    CursorType = ctStatic
    TableName = 'FeedbackUnitTable'
    Left = 188
    Top = 536
  end
  object LinesDataSource: TDataSource
    Left = 52
    Top = 616
  end
  object LinesADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\3.0\Projects\Rail Data Files\LineData.mdb;Persist Secu' +
      'rity Info=False;'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 324
    Top = 612
  end
  object LinesADOTable: TADOTable
    Connection = LinesADOConnection
    CursorType = ctStatic
    TableName = 'LineTable'
    Left = 176
    Top = 608
  end
  object SignalsDataSource2: TDataSource
    Left = 252
    Top = 440
  end
  object SignalsADOTable2: TADOTable
    Connection = SignalsADOConnection2
    CursorType = ctStatic
    TableName = 'SignalTable'
    Left = 76
    Top = 424
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
    Left = 380
    Top = 428
  end
  object PointsADOTable2: TADOTable
    Connection = PointsADOConnection2
    CursorType = ctStatic
    TableName = 'PointTable'
    Left = 176
    Top = 744
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
    Left = 316
    Top = 740
  end
  object PointsDataSource2: TDataSource
    Left = 44
    Top = 744
  end
  object LinesADOConnection2: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\3.0\Projects\Rail Data Files\LineData.mdb;Persist Secu' +
      'rity Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 316
    Top = 676
  end
  object LinesADOTable2: TADOTable
    Connection = LinesADOConnection2
    CursorType = ctStatic
    TableName = 'LineTable'
    Left = 176
    Top = 672
  end
  object LineDataSource2: TDataSource
    Left = 52
    Top = 672
  end
end
