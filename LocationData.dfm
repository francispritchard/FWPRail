object LocationDataWindow: TLocationDataWindow
  Left = 0
  Top = 0
  Align = alClient
  Caption = 'LocationDataWindow'
  ClientHeight = 237
  ClientWidth = 619
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object LocationDataWindowGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 619
    Height = 237
    Align = alClient
    FixedColor = clHighlightText
    FixedCols = 0
    RowCount = 6
    FixedRows = 0
    TabOrder = 0
    OnDrawCell = LocationDataWindowGridDrawCell
    OnKeyDown = LocationDataWindowGridKeyDown
    ExplicitWidth = 591
    RowHeights = (
      24
      24
      25
      24
      24
      24)
  end
  object PlatformDataADOTable: TADOTable
    Connection = PlatformDataADOConnection
    CursorType = ctStatic
    TableName = 'PlatformTable'
    Left = 496
    Top = 32
  end
  object PlatformDataDataSource: TDataSource
    DataSet = PlatformDataADOTable
    Left = 500
    Top = 80
  end
  object PlatformDataADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;User ID=Admin;Data Source=C:\Pr' +
      'ogram Files\Borland\BDS\3.0\Projects\Rail Data Files\PlatformDat' +
      'a.mdb;Mode=Share Deny None;Extended Properties="";Jet OLEDB:Syst' +
      'em database="";Jet OLEDB:Registry Path="";Jet OLEDB:Database Pas' +
      'sword="";Jet OLEDB:Engine Type=5;Jet OLEDB:Database Locking Mode' +
      '=1;Jet OLEDB:Global Partial Bulk Ops=2;Jet OLEDB:Global Bulk Tra' +
      'nsactions=1;Jet OLEDB:New Database Password="";Jet OLEDB:Create ' +
      'System Database=False;Jet OLEDB:Encrypt Database=False;Jet OLEDB' +
      ':Don'#39't Copy Locale on Compact=False;Jet OLEDB:Compact Without Re' +
      'plica Repair=False;Jet OLEDB:SFP=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 508
    Top = 132
  end
end
