object LocoUtilsWindow: TLocoUtilsWindow
  Left = 695
  Top = 380
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'List of Locomotives'
  ClientHeight = 677
  ClientWidth = 707
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LocoUtilsSortLabel: TLabel
    Left = 4
    Top = 4
    Width = 45
    Height = 16
    Caption = 'Sort by:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LocoStringGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 707
    Height = 677
    Align = alClient
    ColCount = 4
    DefaultDrawing = False
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected]
    TabOrder = 0
    OnDblClick = LocoStringGridDblClick
    OnDrawCell = LocoStringGridDrawCell
    OnKeyDown = LocoStringGridKeyDown
    OnMouseUp = LocoStringGridMouseUp
  end
  object LocoLocationsADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\3.0\Projects\Rail Output Files\LocoLocations.mdb;Persi' +
      'st Security Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 68
    Top = 48
  end
  object LocoLocationsADOTable: TADOTable
    Connection = LocoLocationsADOConnection
    CursorType = ctStatic
    TableName = 'LocoLocationsTable'
    Left = 68
    Top = 100
  end
  object LocoLocationsDataSource: TDataSource
    DataSet = LocoLocationsADOTable
    Left = 64
    Top = 156
  end
  object LocoDataADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\3.0\Projects\Rail Data Files\LocoData.mdb;Persist Secu' +
      'rity Info=False'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 64
    Top = 232
  end
  object LocoDataADOTable: TADOTable
    Connection = LocoDataADOConnection
    CursorType = ctStatic
    TableName = 'LocoTable'
    Left = 68
    Top = 292
  end
  object LocoDataDataSource: TDataSource
    DataSet = LocoDataADOTable
    Left = 68
    Top = 356
  end
end
