object DiagramsWindow: TDiagramsWindow
  Left = 605
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Diagrams'
  ClientHeight = 304
  ClientWidth = 766
  Color = clBtnFace
  UseDockManager = True
  DockSite = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnClose = DiagramsWindowClose
  OnCreate = DiagramsWindowCreate
  OnDeactivate = DiagramsWindowDeactivate
  OnHide = DiagramsWindowHide
  OnResize = DiagramsWindowResize
  OnShow = DiagramsWindowShow
  PixelsPerInch = 96
  TextHeight = 13
  object DiagramsWindowGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 766
    Height = 304
    Align = alClient
    Color = clBtnFace
    ColCount = 19
    DefaultColWidth = 60
    DefaultRowHeight = 15
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 6
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    PopupMenu = DiagramsPopupMenu
    ScrollBars = ssVertical
    TabOrder = 0
    OnDrawCell = DiagramsWindowGridDrawCell
    OnKeyDown = DiagramsWindowGridKeyDown
    OnMouseDown = DiagramsWindowGridMouseDown
    OnMouseMove = DiagramsWindowGridMouseMove
    OnSelectCell = DiagramsWindowGridSelectCell
    ColWidths = (
      34
      34
      36
      37
      31
      32
      35
      25
      44
      25
      33
      17
      14
      23
      26
      60
      60
      60
      60)
  end
  object DiagramsPopupMenu: TPopupMenu
    AutoPopup = False
    OnPopup = DiagramsPopupMenuOnPopup
    Left = 72
    Top = 88
    object PopupTrainLocoChip: TMenuItem
    end
    object PopupTrainRule1: TMenuItem
      Caption = '-'
    end
    object PopupDriveTrain: TMenuItem
      Caption = '&Drive Train'
      OnClick = PopupDriveTrainClick
    end
    object PopupSuspendTrain: TMenuItem
      Caption = 'Suspend Train'
      OnClick = PopupSuspendTrainClick
    end
    object PopupMarkTrainNotMissing: TMenuItem
      Caption = 'Mark Train Not Missing'
      Enabled = False
      OnClick = PopupMarkTrainNotMissingClick
    end
    object PopupCancelTrain: TMenuItem
      Caption = 'Cancel Train'
      OnClick = PopupCancelTrainClick
    end
    object PopupTrainUserDriving: TMenuItem
      Caption = 'Mark Train User Driven'
      OnClick = PopupTrainUserDrivingClick
    end
    object PopupSupplyUserTrainInstructions: TMenuItem
      Caption = 'Supply User Train Instructions'
      OnClick = PopupSupplyUserTrainInstructionsClick
    end
    object PopupShowLastTrainErrorMessage: TMenuItem
      Caption = 'Show Last Train Error Message'
      OnClick = PopupShowLastTrainErrorMessageClick
    end
    object PopupTrainRule2: TMenuItem
      Caption = '-'
    end
    object PopupShowNonMovingTrains: TMenuItem
      Caption = '&Show Non-Moving Trains'
      Enabled = False
      OnClick = PopupShowNonMovingTrainsClick
    end
    object PopupShowCancelledTrains: TMenuItem
      Caption = 'Show Cancelled Trains'
      Enabled = False
      OnClick = PopupShowCancelledTrainsClick
    end
    object PopupShowNonStops: TMenuItem
      Caption = 'Show Non-Stops'
      Enabled = False
      OnClick = PopupShowNonStopsClick
    end
    object PopupTrainRule4: TMenuItem
      Caption = '-'
    end
    object PopupChangeDiagramsColours: TMenuItem
      Caption = 'Change Diagrams Colour'
      object PopupChangeDiagramsColour: TMenuItem
        Caption = 'Change Diagrams Colour'
        OnClick = PopupChangeDiagramsColourClick
      end
      object PopupRestoreDefaultColour: TMenuItem
        Caption = 'Restore Default Colour'
        OnClick = PopupRestoreDefaultColourClick
      end
    end
    object PopupSelectDiagramsWindowSize: TMenuItem
      Caption = 'Select Diagrams Window Size'
      OnClick = PopupSelectDiagramsWindowSizeClick
    end
    object PopupResetDiagramsWindowSizeAndPosition: TMenuItem
      Caption = 'Reset Window Size && Position'
      OnClick = PopupResetDiagramsWindowSizeAndPositionClick
    end
  end
  object DiagramsWindowColourDialogue: TColorDialog
    Left = 216
    Top = 88
  end
  object DiagramsADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\Diagrams.mdb;Persist Secu' +
      'rity Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 488
    Top = 88
  end
  object DiagramsDataSource: TDataSource
    DataSet = DiagramsADOTable
    Left = 368
    Top = 88
  end
  object DiagramsADOTable: TADOTable
    Connection = DiagramsADOConnection
    CursorType = ctStatic
    TableName = 'DiagramsTable'
    Left = 612
    Top = 88
  end
end
