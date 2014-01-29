object WorkingTimetableWindow: TWorkingTimetableWindow
  Left = 605
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'OldWorkingTimetable'
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
  OnClose = WorkingTimetableWindowClose
  OnCreate = WorkingTimetableWindowCreate
  OnHide = WorkingTimetableWindowHide
  OnResize = WorkingTimetableWindowResize
  OnShow = WorkingTimetableWindowShow
  PixelsPerInch = 96
  TextHeight = 13
  object WorkingTimetableWindowGrid: TStringGrid
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
    PopupMenu = WorkingTimetablePopupMenu
    ScrollBars = ssVertical
    TabOrder = 0
    OnDrawCell = WorkingTimetableWindowGridDrawCell
    OnKeyDown = WorkingTimetableWindowGridKeyDown
    OnMouseDown = WorkingTimetableWindowGridMouseDown
    OnMouseMove = WorkingTimetableWindowGridMouseMove
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
  object WorkingTimetablePopupMenu: TPopupMenu
    AutoPopup = False
    OnPopup = WorkingTimetablePopupMenuOnPopup
    Left = 72
    Top = 88
    object PopupShowIncorrectDayOfTheWeekEntries: TMenuItem
      Caption = 'Hide/Show Incorrect Day Of The Week Entries'
      OnClick = PopupShowIncorrectDayOfTheWeekEntriesClick
    end
    object PopupChangeWorkingTimetableColours: TMenuItem
      Caption = 'Change Working Timetable Colour'
      object PopupChangeWorkingTimetableColour: TMenuItem
        Caption = 'Change Working Timetable Colour'
        OnClick = PopupChangeWorkingTimetableColourClick
      end
      object PopupRestoreDefaultColour: TMenuItem
        Caption = 'Restore Default Colour'
        OnClick = PopupRestoreDefaultColourClick
      end
    end
    object PopupSelectWorkingTimetableWindowSize: TMenuItem
      Caption = 'Select Working Timetable Window Size'
      OnClick = PopupSelectWorkingTimetableWindowSizeClick
    end
    object PopupResetWorkingTimetableWindowSizeAndPosition: TMenuItem
      Caption = 'Reset Window Size && Position'
      OnClick = PopupResetWorkingTimetableWindowSizeAndPositionClick
    end
  end
  object WorkingTimetableWindowColourDialogue: TColorDialog
    Left = 216
    Top = 88
  end
  object WorkingTimetableADOConnection: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Program Files\Bo' +
      'rland\BDS\4.0\Projects\Rail Data Files\WorkingTimetable.mdb;Pers' +
      'ist Security Info=False'
    KeepConnection = False
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 488
    Top = 88
  end
  object WorkingTimetableDataSource: TDataSource
    DataSet = WorkingTimetableADOTable
    Left = 368
    Top = 88
  end
  object WorkingTimetableADOTable: TADOTable
    Connection = WorkingTimetableADOConnection
    CursorType = ctStatic
    TableName = 'WorkingTimetableTable'
    Left = 612
    Top = 88
  end
end
