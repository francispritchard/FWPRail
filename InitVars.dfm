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
    Top = 214
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
    Top = 240
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
  end
  object SaveDialogue: TSaveDialog
    DefaultExt = 'tim'
    Filter = 'Timetable Files (*.tim)|*.tim'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoReadOnlyReturn, ofEnableSizing]
    Title = 'Save Timetable File'
    Left = 532
    Top = 632
  end
  object SignalsDataSource: TDataSource
    Left = 84
    Top = 424
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
    Left = 228
    Top = 412
  end
  object SignalsADOTable: TADOTable
    Connection = SignalsADOConnection
    CursorType = ctStatic
    TableName = 'SignalTable'
    Left = 380
    Top = 408
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
    Top = 364
  end
  object PointsDataSource: TDataSource
    Left = 84
    Top = 352
  end
  object PointsADOTable: TADOTable
    Connection = PointsADOConnection
    CursorType = ctStatic
    TableName = 'PointTable'
    Left = 376
    Top = 360
  end
  object LocationsDataSource: TDataSource
    Left = 68
    Top = 296
  end
  object LocationsADOConnection: TADOConnection
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 212
    Top = 292
  end
  object LocationsADOTable: TADOTable
    Connection = LocationsADOConnection
    CursorType = ctStatic
    TableName = 'LocationTable'
    Left = 368
    Top = 288
  end
  object AreasDataSource: TDataSource
    Left = 44
    Top = 216
  end
  object AreasADOConnection: TADOConnection
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 204
    Top = 220
  end
  object AreasADOTable: TADOTable
    Connection = AreasADOConnection
    CursorType = ctStatic
    TableName = 'AreaTable'
    Left = 344
    Top = 216
  end
  object TrackCircuitDataSource: TDataSource
    Left = 52
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
  object TrackCircuitsADOTable: TADOTable
    Connection = TrackCircuitsADOConnection
    CursorType = ctStatic
    TableName = 'TrackCircuitTable'
    Left = 344
    Top = 72
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
    Left = 84
    Top = 488
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
    Left = 220
    Top = 476
  end
  object FeedbackUnitsADOTable: TADOTable
    Connection = FeedbackUnitsADOConnection
    CursorType = ctStatic
    TableName = 'FeedbackUnitTable'
    Left = 372
    Top = 472
  end
  object LinesDataSource: TDataSource
    Left = 76
    Top = 536
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
    Left = 204
    Top = 532
  end
  object LinesADOTable: TADOTable
    Connection = LinesADOConnection
    CursorType = ctStatic
    TableName = 'LineTable'
    Left = 368
    Top = 528
  end
  object SignalsDataSource2: TDataSource
    Left = 76
    Top = 592
  end
  object SignalsADOTable2: TADOTable
    Connection = SignalsADOConnection2
    CursorType = ctStatic
    TableName = 'SignalTable'
    Left = 300
    Top = 592
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
    Left = 196
    Top = 596
  end
  object PointsADOTable2: TADOTable
    Connection = PointsADOConnection2
    CursorType = ctStatic
    TableName = 'PointTable'
    Left = 312
    Top = 712
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
    Left = 188
    Top = 724
  end
  object PointsDataSource2: TDataSource
    Left = 68
    Top = 728
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
    Left = 196
    Top = 668
  end
  object LinesADOTable2: TADOTable
    Connection = LinesADOConnection2
    CursorType = ctStatic
    TableName = 'LineTable'
    Left = 328
    Top = 648
  end
  object LinesDataSource2: TDataSource
    Left = 76
    Top = 672
  end
  object LocationsDataSource2: TDataSource
    Left = 76
    Top = 808
  end
  object LocationsADOConnection2: TADOConnection
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 196
    Top = 792
  end
  object LocationsADOTable2: TADOTable
    Connection = LocationsADOConnection2
    CursorType = ctStatic
    TableName = 'LocationTable'
    Left = 328
    Top = 784
  end
  object TrackCircuitsADOConnection2: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\TrackCircuitData.mdb;Pers' +
      'ist Security Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 196
    Top = 140
  end
  object TrackCircuitDataSource2: TDataSource
    Left = 52
    Top = 136
  end
  object TrackCircuitsADOTable2: TADOTable
    Connection = TrackCircuitsADOConnection2
    CursorType = ctStatic
    TableName = 'TrackCircuitTable'
    Left = 344
    Top = 136
  end
end
