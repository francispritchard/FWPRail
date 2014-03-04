UNIT WorkingTimetable;

INTERFACE

USES Windows, Messages, Variants, Classes, Graphics, Controls, Forms, StdCtrls, Grids, Menus, InitVars, GetTime, Dialogs, SysUtils, DB, ADODB, DBGrids, ExtCtrls, DBCtrls;

TYPE
  TWorkingTimetableWindow = CLASS(TForm)
    PopupChangeWorkingTimetableColour: TMenuItem;
    PopupChangeWorkingTimetableColours: TMenuItem;
    PopupResetWorkingTimetableWindowSizeAndPosition: TMenuItem;
    PopupRestoreDefaultColour: TMenuItem;
    PopupSelectWorkingTimetableWindowSize: TMenuItem;
    PopupShowIncorrectDayOfTheWeekEntries: TMenuItem;

    WorkingTimetableADOConnection: TADOConnection;
    WorkingTimetableADOTable: TADOTable;
    WorkingTimetableDataSource: TDataSource;
    WorkingTimetablePopupMenu: TPopupMenu;
    WorkingTimetableWindowColourDialogue: TColorDialog;
    WorkingTimetableWindowGrid: TStringGrid;

    PROCEDURE PopupChangeWorkingTimetableColourClick(Sender: TObject);
    PROCEDURE PopupResetWorkingTimetableWindowSizeAndPositionClick(Sender: TObject);
    PROCEDURE PopupRestoreDefaultColourClick(Sender: TObject);
    PROCEDURE PopupSelectWorkingTimetableWindowSizeClick(Sender: TObject);
    PROCEDURE PopupShowIncorrectDayOfTheWeekEntriesClick(Sender: TObject);

    PROCEDURE WorkingTimetablePopupMenuOnPopup(Sender: TObject);
    PROCEDURE WorkingTimetableWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE WorkingTimetableWindowCreate(Sender: TObject);
    PROCEDURE WorkingTimetableWindowGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    PROCEDURE WorkingTimetableWindowGridKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE WorkingTimetableWindowGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    PROCEDURE WorkingTimetableWindowGridMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE WorkingTimetableWindowHide(Sender: TObject);
    PROCEDURE WorkingTimetableWindowResize(Sender: TObject);
    PROCEDURE WorkingTimetableWindowShow(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE DrawWorkingTimetable(UnitRef : String; Str : String);
{ Write the complete WorkingTimetable to the screen after it has been changed }

PROCEDURE LoadWorkingTimetableFromDatabase(OUT WorkingTimetableMissing, WorkingTimetableOK : Boolean);
{ Load the working timetable }

PROCEDURE ProcessWorkingTimetable;
{ Convert the working timetable to train diagrams }

VAR
  TrainClickedOn : Train = NIL;
  WorkingTimetableWindow: TWorkingTimetableWindow;

IMPLEMENTATION

{$R *.dfm}

USES ComObj, Lenz, MiscUtils, Startup, LocoUtils, IDGlobal, RailDraw, Input, Movement, CreateRoute, DateUtils, Math {sic}, Route, Types, StrUtils, StationMonitors, Locks,
     LocoDialogue, LocationData, Help, Options, Diagrams;

TYPE
  TextAlignment = (LeftAlign, RightAlign, CentreAlign);

CONST
  BoldStr = '<B>';
  GreyedOutStr = '<G>';
  ItalicStr = '<I>';
  CentreAlignStr ='<C>';
  LeftAlignStr = '<L>';
  RightAlignStr = '<R>';

  UnitRef = 'WorkingTimetable';

  LocoCol = 0;
  EntryCol = LocoCol + 1;
  FromCol = EntryCol + 1;
  FromTimeCol = FromCol + 1;
  ToCol = FromTimeCol + 1;
  StoppingAtCol = ToCol + 1;
  TypeCol = StoppingAtCol + 1;
  PossibleClassesCol = TypeCol + 1;
  PreferredLengthCol = PossibleClassesCol + 1;
  DaysOfTheWeekCol = PreferredLengthCol + 1;

VAR
  EscapePressed : Boolean = False;
  HighlightRow : Integer = -1;
  ShowArrivalTimes : Boolean = False;
  MaxColumnCount : Integer = 0;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE TWorkingTimetableWindow.WorkingTimetableWindowGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
{ We identify which items we want to be highlighted or marked as invalid by prefixing them with a marker; this is necessary as this routine just draws the data it's given
  without knowing where that data has come from.
}
CONST
  CharOffset = 3;

VAR
  CellText: String;
  LeftOffset: Integer;
  DefaultWorkingTimetableGridFontColour : TColor;
  WidthOfText: Integer;
  WidthOfCell: Integer;

BEGIN
  WITH WorkingTimetableWindowGrid DO BEGIN
    WITH WorkingTimetableWindowGrid.Canvas DO BEGIN
      { Make the column headings bold }
      IF ARow = 0 THEN
        Font.Style := [fsBold]
      ELSE
        Font.Style := [];

      DefaultWorkingTimetableGridFontColour := clBlack;
      CellText := Cells[ACol, ARow];

      { If it's a cancelled entry then we use "greyed-out" font colour }
      IF LeftStr(CellText, 3) = GreyedOutStr THEN BEGIN
        { if it's a cancelled entry then we use "greyed-out" font colour }
        Font.Color := clGrayText;
        Brush.Color := WorkingTimetableWindowGridBackgroundColour;
        CellText := Copy(CellText, 4);
      END ELSE BEGIN
        Font.Color := DefaultWorkingTimetableGridFontColour;
        Brush.Color := WorkingTimetableWindowGridBackgroundColour;
      END;

      IF (HighlightRow > -1)
      AND (HighlightRow = ARow)
      THEN BEGIN
        { highlight the whole line if required }
        Font.Color := clWhite;
        Brush.Color := clBlue;
      END;

      { Now look for a bold marker... }
      IF LeftStr(CellText, 3) = BoldStr THEN BEGIN
        CellText := Copy(CellText, 4);
        Font.Style := [fsBold];
      END;

      { ...or an italic marker }
      IF LeftStr(CellText, 3) = ItalicStr THEN BEGIN
        CellText := Copy(CellText, 4);
        Font.Style := [fsItalic];
      END;

      WidthOfText := TextWidth(CellText);
      WidthOfCell := ColWidths[ACol];

      LeftOffset := 0;
      IF RightStr(CellText, 3) = LeftAlignStr THEN BEGIN
        { do not centre }
        CellText := Copy(CellText, 1, Length(CellText) - Length(LeftAlignStr));
      END ELSE
        IF RightStr(CellText, 3) = RightAlignStr THEN BEGIN
          LeftOffset := WidthOfCell - WidthOfText;
          CellText := Copy(CellText, 1, Length(CellText) - Length(RightAlignStr));
        END ELSE
          IF RightStr(CellText, 3) = CentreAlignStr THEN BEGIN
            LeftOffset := (WidthOfCell - WidthOfText) DIV 2;
            CellText := Copy(CellText, 1, Length(CellText) - Length(CentreAlignStr));
          END;
          
      TextRect(Rect, Rect.Left + LeftOffset, Rect.Top, CellText);
      Font.Color := DefaultWorkingTimetableGridFontColour;
    END; {WITH}
  END; {WITH}
END; { WorkingTimetableWindowGridDrawCell }

PROCEDURE DrawWorkingTimetable(UnitRef : String; Str : String);
{ Write the complete WorkingTimetable to the screen after it has been changed }
TYPE
  CellStyleType = (NormalStyle, BoldStyle, ItalicStyle, GreyedOutStyle);

CONST
  FixedLength = True;
  NoSpeedStr = '';
  OmitUnit = True;

VAR
  I, J : Integer;
  TempGridRowCount : Integer;
  TempStr : String;
  WorkingTimetableCount : Integer;

  FUNCTION DescribeClass(C : String) : String;
  { Return a truncated description of a class of train }
  BEGIN
    IF Pos('Class ', C) > 0 THEN
      Result := Copy(C, 7, 6)
    ELSE
      Result := Copy(C, 1, 6);
  END; { DescribeClass }

  PROCEDURE SetUpWorkingTimetableGridCell(Status : WorkingTimetableStatusType; WorkingTimetableCol, WorkingTimetableRow : Integer; Str : String; Style : CellStyleType;
                                          Alignment : TextAlignment);
  { May add an cancelled train marker or highlight some text before passing to the DrawWorkingTimetableGridCell routine (this would not be necessary if that routine knew
    where the text was coming from).
  }
  BEGIN
    WITH WorkingTimetableWindow.WorkingTimetableWindowGrid DO BEGIN
      IF (Status = EntryCancelled) OR (Status = IncorrectDayOfTheWeekForEntry) THEN
        { override other styles }
        Style := GreyedOutStyle
      ELSE
        IF Status = AdditionalEntryCreated THEN
          { override other styles }
          Style := ItalicStyle;

      CASE Style OF
        NormalStyle:
          Str := Trim(Str);
        BoldStyle:
          Str := BoldStr + Trim(Str);
        ItalicStyle:
          Str := ItalicStr + Trim(Str);
        GreyedOutStyle:
          Str := GreyedOutStr + Trim(Str);
      END; {CASE}

      CASE Alignment OF
        LeftAlign:
          Str := Trim(Str) + LeftAlignStr;
        RightAlign:
          Str := Trim(Str) + RightAlignStr;
        CentreAlign:
          Str := Trim(Str) + CentreAlignStr;
      END; {CASE}

      Cells[WorkingTimetableCol, WorkingTimetableRow - 1] := Str;
    END; {WITH}
  END; { SetUpWorkingTimetableGridCell }

BEGIN
  TRY
    WITH WorkingTimetableWindow.WorkingTimetableWindowGrid DO BEGIN
      { clear any existing cells, since this routine is called with different diagram sizes, and text otherwise gets left behind }
      FOR I := 0 TO ColCount DO
        { now only clear row 2 onwards, or the headings disappear }
        FOR J := 1 TO RowCount DO
          Cells[I, J] := ' ';

      Log('W ----------------- new WorkingTimetable start');

      TempGridRowCount := 1;

      { Remove lines between the cells }
      GridLineWidth := 0;

      WorkingTimetableCount := 0;
      WHILE WorkingTimetableCount <= High(WorkingTimetableRecArray) DO BEGIN
        WITH WorkingTimetableRecArray[WorkingTimetableCount] DO BEGIN
          IF ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable
          OR (WorkingTimetable_Status <> IncorrectDayOfTheWeekForEntry)
          THEN BEGIN
            TempGridRowCount := TempGridRowCount + 1;
            FOR I := 0 TO ColCount DO
              { clear the new cells }
              Cells[I, TempGridRowCount] := ' ';

            { Draw the cells, If we want a cell to be in bold, etc., add <B> - the OnDrawCell routine will substitute Bold text for it (there is no easier way to do this,
              as the OnDrawCell routine doesn't know anything about the antecedents of the text it's been given).
            }
            IF WorkingTimetable_LocoChip <> UnknownLocoChip THEN
              SetUpWorkingTimetableGridCell(WorkingTimetable_Status, LocoCol, TempGridRowCount, LocoChipToStr(WorkingTimetable_LocoChip), NormalStyle, CentreAlign);
            SetUpWorkingTimetableGridCell(WorkingTimetable_Status, EntryCol, TempGridRowCount, WorkingTimetable_EntryNumStr, NormalStyle, CentreAlign);
            SetUpWorkingTimetableGridCell(WorkingTimetable_Status, FromCol, TempGridRowCount, AreaToStr(WorkingTimetable_FirstStationArea), NormalStyle, LeftAlign);
            SetUpWorkingTimetableGridCell(WorkingTimetable_Status, FromTimeCol, TempGridRowCount, TimeToHMSStr(WorkingTimetable_FirstStationDepartureTime), NormalStyle,
                                          LeftAlign);
            SetUpWorkingTimetableGridCell(WorkingTimetable_Status, ToCol, TempGridRowCount, AreaToStr(WorkingTimetable_LastStationArea), NormalStyle, LeftAlign);
            TempStr := '';
            IF Length(WorkingTimetable_StoppingAtStationAreas) > 0 THEN BEGIN
              FOR I := 0 TO High(WorkingTimetable_StoppingAtStationAreas) - 1 DO
                TempStr := TempStr + AreaToStr(WorkingTimetable_StoppingAtStationAreas[I]) + ', ';
              TempStr := TempStr + AreaToStr(WorkingTimetable_StoppingAtStationAreas[High(WorkingTimetable_StoppingAtStationAreas)]);
              SetUpWorkingTimetableGridCell(WorkingTimetable_Status, StoppingAtCol, TempGridRowCount, TempStr, NormalStyle, LeftAlign);
            END;

            SetUpWorkingTimetableGridCell(WorkingTimetable_Status, TypeCol, TempGridRowCount, TrainTypeNumToStr(WorkingTimetable_TrainTypeNum), NormalStyle, LeftAlign);
            TempStr := '';
            IF Length(WorkingTimetable_PossibleLocoClasses) > 0 THEN BEGIN
              FOR I := 0 TO High(WorkingTimetable_PossibleLocoClasses) - 1 DO
                TempStr := TempStr + WorkingTimetable_PossibleLocoClasses[I] + ', ';
              TempStr := TempStr + WorkingTimetable_PossibleLocoClasses[High(WorkingTimetable_PossibleLocoClasses)];
              SetUpWorkingTimetableGridCell(WorkingTimetable_Status, PossibleClassesCol, TempGridRowCount, TempStr, NormalStyle, CentreAlign);
            END;

            SetUpWorkingTimetableGridCell(WorkingTimetable_Status, PreferredLengthCol, TempGridRowCount, IntToStr(WorkingTimetable_PreferredNumberOfCarriages), NormalStyle,
                                          CentreAlign);
            SetUpWorkingTimetableGridCell(WorkingTimetable_Status, DaysOfTheWeekCol, TempGridRowCount, DaysOfTheWeekSetToStr(WorkingTimetable_DaysOfTheWeekSet),
                                          NormalStyle, CentreAlign);

          END;
        END; {WITH}
        Inc(WorkingTimetableCount);
      END; {WHILE}
      WorkingTimetableWindow.WorkingTimetableWindowGrid.RowCount := TempGridRowCount;
    END; {WITH}
    Log('W ----------------- new WorkingTimetable end');
  EXCEPT
    ON E : Exception DO
      Log('EG DrawWorkingTimetable: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DrawWorkingTimetable }

PROCEDURE DrawWorkingTimetableWindow;
{ Set up default window and grid sizes }
VAR
  DaysOfTheWeekColWidth : Integer;
  LocoColWidth : Integer;
  EntryColWidth : Integer;
  FromColWidth : Integer;
  FromTimeColWidth : Integer;
  I, J : Integer;
  PossibleClassesColWidth : Integer;
  PreferredLengthColWidth : Integer;
  StoppingAtColWidth : Integer;
  ToColWidth : Integer;
  TotalWidthOfColumns : Integer;
  TypeColWidth : Integer;

BEGIN
  WITH WorkingTimetableWindow DO BEGIN
    { reset the column count }
    MaxColumnCount := 0;

    Height := WorkingTimetableWindowHeight;
    Top := WorkingTimetableWindowTop;
    Left := WorkingTimetableWindowLeft;
    IF LargeWorkingTimetableWindowSelected THEN
      Width := WorkingTimetableLargeWindowWidth
    ELSE
      Width := WorkingTimetableSmallWindowWidth;

    WITH WorkingTimetableWindowGrid DO BEGIN
      IF LargeWorkingTimetableWindowSelected THEN BEGIN
        { clear existing grid before resizing it (only clear row 2 onwards, or the headings disappear) }
        FOR I := 0 TO ColCount DO
          FOR J := 1 TO RowCount DO
            Cells[I, J] := ' ';

        RowCount := 2;
        ColCount := 10;

        LocoColWidth := 50;
        EntryColWidth := 50;
        FromColWidth := 110;
        FromTimeColWidth := 60;
        ToColWidth := 110;
        StoppingAtColWidth := 480;
        TypeColWidth := 120;
        PossibleClassesColWidth := 70;
        PreferredLengthColWidth := 50;
        DaysOfTheWeekColWidth := 85;
      END ELSE BEGIN
        { Small WorkingTimetable Window }

        { clear existing grid before resizing it (only clear row 2 onwards, or the headings disappear) }
        FOR I := 0 TO ColCount DO
          FOR J := 1 TO RowCount DO
            Cells[I, J] := ' ';

        RowCount := 2;
        ColCount := 10;

        LocoColWidth := 40;
        EntryColWidth := 40;
        FromColWidth := 110;
        FromTimeColWidth := 60;
        ToColWidth := 110;
        StoppingAtColWidth := 250;
        TypeColWidth := 120;
        PossibleClassesColWidth := 70;
        PreferredLengthColWidth := 50;
        DaysOfTheWeekColWidth := 85;
      END;

      TotalWidthOfColumns := LocoColWidth + EntryColWidth + FromColWidth + FromTimeColWidth + ToColWidth + TypeColWidth + StoppingAtColWidth + PossibleClassesColWidth
                             + PreferredLengthColWidth + DaysOfTheWeekColWidth

                             - (BevelWidth * 2) { why does width of all the columns slightly exceed width of the small WorkingTimetable window  *** }
                             - (BorderWidth * 2);

      { Now the column widths and titles }
      ColWidths[LocoCol] := MulDiv(Width, LocoColWidth, TotalWidthOfColumns);
      ColWidths[EntryCol] := MulDiv(Width, EntryColWidth, TotalWidthOfColumns);
      ColWidths[FromCol] := MulDiv(Width, FromColWidth, TotalWidthOfColumns);
      ColWidths[FromTimeCol] := MulDiv(Width, FromTimeColWidth, TotalWidthOfColumns);
      ColWidths[ToCol] := MulDiv(Width, ToColWidth, TotalWidthOfColumns);
      ColWidths[TypeCol] := MulDiv(Width, TypeColWidth, TotalWidthOfColumns);
      ColWidths[StoppingAtCol] := MulDiv(Width, StoppingAtColWidth, TotalWidthOfColumns);
      ColWidths[TypeCol] := MulDiv(Width, TypeColWidth, TotalWidthOfColumns);
      ColWidths[PossibleClassesCol] := MulDiv(Width, PossibleClassesColWidth, TotalWidthOfColumns);
      ColWidths[PreferredLengthCol] := MulDiv(Width, PreferredLengthColWidth, TotalWidthOfColumns);
      ColWidths[DaysOfTheWeekCol] := MulDiv(Width, DaysOfTheWeekColWidth, TotalWidthOfColumns);

      { Now the column headings }
      Cells[LocoCol, 0] := 'Loco';
      Cells[EntryCol, 0] := 'Entry';
      Cells[FromCol, 0] := 'First Station';
      Cells[FromTimeCol, 0] := 'Time';
      Cells[ToCol, 0] := 'Last Station';
      Cells[StoppingAtCol, 0] := 'Stopping At';
      Cells[TypeCol, 0] := 'Type';
      Cells[PossibleClassesCol, 0] := 'Classes';
      Cells[PreferredLengthCol, 0] := 'PrefLen';
      Cells[DaysOfTheWeekCol, 0] := 'Days';

      { Clear the grid }
      FOR I := 0 TO 6 DO
        FOR J:= 1 to 2 DO
          Cells[I, J] := '';
    END; {WITH}
    IF NOT DisplayWorkingTimetable THEN
      Show;

    { and move the focus back to the main window }
    IF FWPRailMainWindow.Visible THEN
      FWPRailMainWindow.SetFocus;
  END; {WITH}
END; { DrawWorkingTimetableWindow }

PROCEDURE TWorkingTimetableWindow.WorkingTimetableWindowCreate(Sender: TObject);
BEGIN
  WorkingTimetableWindow.Left := WorkingTimetableWindowLeft;
  WorkingTimetableWindow.Top := WorkingTimetableWindowTop;
  WorkingTimetableWindow.Height := WorkingTimetableWindowHeight;
  IF LargeWorkingTimetableWindowSelected THEN
    WorkingTimetableWindow.Width := WorkingTimetableLargeWindowWidth
  ELSE
    WorkingTimetableWindow.Width := WorkingTimetableSmallWindowWidth;

  FWPRailMainWindow.MainDisplayMenuDiagramsWindow.Checked := False;
  FWPRailMainWindow.MainDisplayMenuWorkingTimetableWindow.Checked := True;
  WorkingTimetableWindowGrid.Color := WorkingTimetableWindowGridBackgroundColour;

  IF CurrentRailwayDayOfTheWeek <> UnknownDayOfTheWeek THEN
    WorkingTimetableWindow.Caption := WorkingTimetableWindow.Caption + ' for ' + DayOfTheWeekToStr(CurrentRailwayDayOfTheWeek);

  IF DisplayWorkingTimetable THEN BEGIN
    DrawWorkingTimetableWindow;
    WorkingTimetableWindow.Show;
  END;
END; { WorkingTimetableWindowCreate }

PROCEDURE TWorkingTimetableWindow.WorkingTimetableWindowClose(Sender: TObject; VAR Action: TCloseAction);
{ Hides but does not close the window }
BEGIN
  Action := caHide;
END; { WorkingTimetableWindowClose }

PROCEDURE TWorkingTimetableWindow.WorkingTimetableWindowGridKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    { Exclude most non-alphanumeric keys }
    vk_Shift, vk_Control, vk_Menu { Alt }, vk_Pause, vk_SnapShot { PrtSc }, vk_LWin, vk_RWin, vk_Apps { Windows Applications }, vk_Numlock, vk_Scroll,
    vk_Cancel { Ctrl-Break}, vk_Capital { Caps Lock }, vk_Back { Backspace }:
      { do nothing };
  ELSE
    KeyPressedDown(Key, ShiftState);
  END; {CASE}
END; { WorkingTimetableWindowGridKeyDown }

PROCEDURE TWorkingTimetableWindow.WorkingTimetableWindowHide(Sender: TObject);
{ Un-check the window menu item }
BEGIN
  FWPRailMainWindow.MainDisplayMenuWorkingTimetableWindow.Checked := False;
END; { WorkingTimetableWindowHide }

PROCEDURE TWorkingTimetableWindow.WorkingTimetableWindowShow(Sender: TObject);
{ Check the window menu item }
BEGIN
  WorkingTimetableWindow.Left := WorkingTimetableWindowLeft;
  WorkingTimetableWindow.Top := WorkingTimetableWindowTop;
  WorkingTimetableWindow.Height := WorkingTimetableWindowHeight;
  IF LargeWorkingTimetableWindowSelected THEN
    WorkingTimetableWindow.Width := WorkingTimetableLargeWindowWidth
  ELSE
    WorkingTimetableWindow.Width := WorkingTimetableSmallWindowWidth;

  FWPRailMainWindow.MainDisplayMenuDiagramsWindow.Checked := False;
  FWPRailMainWindow.MainDisplayMenuWorkingTimetableWindow.Checked := True;
  WorkingTimetableWindowGrid.Color := WorkingTimetableWindowGridBackgroundColour;

  DrawWorkingTimetableWindow;
END; { WorkingTimetableWindowShow }

PROCEDURE TWorkingTimetableWindow.WorkingTimetablePopupMenuOnPopup(Sender: TObject);
BEGIN
  IF LargeWorkingTimetableWindowSelected THEN
    PopupSelectWorkingTimetableWindowSize.Caption := 'Select Narrow Working Timetable Window'
  ELSE
    PopupSelectWorkingTimetableWindowSize.Caption := 'Select Wide Working Timetable Window';

  IF ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable THEN
    PopupShowIncorrectDayOfTheWeekEntries.Caption := 'Hide Incorrect Day Of The Week Entries'
  ELSE
    PopupShowIncorrectDayOfTheWeekEntries.Caption := 'Show Incorrect Day Of The Week Entries';

  IF WorkingTimetableWindow.Top <> DefaultWorkingTimetableWindowTop THEN
    PopupResetWorkingTimetableWindowSizeAndPosition.Enabled := True
  ELSE
    PopupResetWorkingTimetableWindowSizeAndPosition.Enabled := False;
END; { WorkingTimetablePopupMenuOnPopup }

PROCEDURE TWorkingTimetableWindow.PopupShowIncorrectDayOfTheWeekEntriesClick(Sender: TObject);
BEGIN
  IF ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable THEN BEGIN
    ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable := False;
    PopupShowIncorrectDayOfTheWeekEntries.Caption := 'Show Incorrect Day Of The Week Entries';
  END ELSE BEGIN
    ShowIncorrectDayOfTheWeekEntriesInWorkingTimetable := True;
    PopupShowIncorrectDayOfTheWeekEntries.Caption := 'Hide Incorrect Day Of The Week Entries';
  END;

  DrawWorkingTimetableWindow;
  DrawWorkingTimetable(UnitRef, 'PopupShowInvalidDaysOfTheWeekInWorkingTimetableClick');
END; { PopupPopupShowIncorrectDayOfTheWeekEntries }

PROCEDURE TWorkingTimetableWindow.PopupChangeWorkingTimetableColourClick(Sender: TObject);
BEGIN
  { Show the default }
  WorkingTimetableWindowColourDialogue.Color := WorkingTimetableWindowGridBackgroundColour;
  { Allow the user to change it }
  IF WorkingTimetableWindowColourDialogue.Execute THEN BEGIN
    WorkingTimetableWindowGrid.Color := WorkingTimetableWindowColourDialogue.Color;
    WorkingTimetableWindowGridBackgroundColour := WorkingTimetableWindowColourDialogue.Color;
  END;
END; { PopupChangeWorkingTimetableColourClick }

PROCEDURE TWorkingTimetableWindow.PopupRestoreDefaultColourClick(Sender: TObject);
BEGIN
  WorkingTimetableWindowGrid.Color := DefaultWorkingTimetableWindowGridBackgroundColour;
  WorkingTimetableWindowGridBackgroundColour := DefaultWorkingTimetableWindowGridBackgroundColour;
END; { RestoreDefaultColourClick }

PROCEDURE TWorkingTimetableWindow.PopupSelectWorkingTimetableWindowSizeClick(Sender: TObject);
BEGIN
  IF LargeWorkingTimetableWindowSelected THEN BEGIN
    LargeWorkingTimetableWindowSelected := False;
    PopupSelectWorkingTimetableWindowSize.Caption := 'Select Wide Working Timetable Window';
  END ELSE BEGIN
    LargeWorkingTimetableWindowSelected := True;
    PopupSelectWorkingTimetableWindowSize.Caption := 'Select Narrow Working Timetable Window';
  END;
  DrawWorkingTimetableWindow;
  DrawWorkingTimetable(UnitRef, 'popupSelectWorkingTimetableWindowSizeClick');
END; { PopupSelectWorkingTimetableWindowSizeClick }

PROCEDURE TWorkingTimetableWindow.PopupResetWorkingTimetableWindowSizeAndPositionClick(Sender: TObject);
BEGIN
  WorkingTimetableWindowHeight := DefaultWorkingTimetableWindowHeight;
  WorkingTimetableLargeWindowWidth := DefaultWorkingTimetableLargeWindowWidth;
  WorkingTimetableSmallWindowWidth := DefaultWorkingTimetableSmallWindowWidth;
  WorkingTimetableWindowTop := DefaultWorkingTimetableWindowTop;
  WorkingTimetableWindowLeft := DefaultWorkingTimetableWindowLeft;

  DrawWorkingTimetableWindow;
  DrawWorkingTimetable(UnitRef, 'PopupResetWorkingTimetableWindowSizeAndPositionClick');
END; { PopupResetWorkingTimetableWindowSizeAndPositionClick }

PROCEDURE TWorkingTimetableWindow.WorkingTimetableWindowResize(Sender: TObject);
BEGIN
  { Enable or disable the popup menu item allowing us to return the window to its default size }
  IF (WorkingTimetableWindow.Height <> DefaultWorkingTimetableWindowHeight)
  OR (NOT LargeWorkingTimetableWindowSelected
      AND (WorkingTimetableWindow.Width <> DefaultWorkingTimetableSmallWindowWidth))
  OR (LargeWorkingTimetableWindowSelected
      AND (WorkingTimetableWindow.Width <> DefaultWorkingTimetableLargeWindowWidth))
  OR (WorkingTimetableWindow.Top <> DefaultWorkingTimetableWindowTop)
  OR (WorkingTimetableWindow.Left <> DefaultWorkingTimetableWindowLeft)
  THEN
    PopupResetWorkingTimetableWindowSizeAndPosition.Enabled := True
  ELSE
    PopupResetWorkingTimetableWindowSizeAndPosition.Enabled := False;

  { Also allow the diagram window to be maximised *** }

END; { WorkingTimetableSmallWindowResize }

PROCEDURE TWorkingTimetableWindow.WorkingTimetableWindowGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
VAR
  WorkingTimetableColWidth : Integer;

BEGIN
  IF Button = mbRight THEN BEGIN
    { Move the popup over by the width of the column }
    WorkingTimetableColWidth := WorkingTimetableWindowGrid.ColWidths[WorkingTimetableWindowGrid.MouseCoord(X, Y).Y];
    { and pop it up }
    WorkingTimetablePopupMenu.Popup(X + WorkingTimetableColWidth, Y + Top);
  END;
END; { WorkingTimetableWindowGridMouseDown }

PROCEDURE TWorkingTimetableWindow.WorkingTimetableWindowGridMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
{ If the mouse moves into the WorkingTimetable window, move the focus there; also find out which columns and rows we are hovering over }
CONST
  HeadingRow = 0;

VAR
  ColsAndRows : TGridCoord;

BEGIN
  WITH WorkingTimetableWindow.WorkingTimetableWindowGrid DO BEGIN
    ColsAndRows := WorkingTimetableWindowGrid.MouseCoord(X, Y);

    IF (ColsAndRows.X > -1)
    AND (ColsAndRows.Y > -1)
    AND (SaveCursor <> crHandPoint)
    THEN
      ChangeCursor(crHandPoint)
    ELSE
      ChangeCursor(crDefault);
  END; {WITH}

END; { WorkingTimetableWindowGridMouseMove }

PROCEDURE LoadWorkingTimetableFromDatabase(OUT WorkingTimetableMissing, WorkingTimetableOK : Boolean);
{ Load the working timetable }
VAR
  ClassFound : Boolean;
  DebugStr : String;
  ErrorMsg : String;
  FieldName : String;
  I : Integer;
  T : Train;
  TempStr : String;
  TempStrArray : StringArrayType;

BEGIN
  TRY
    WITH WorkingTimetableWindow DO BEGIN
      ErrorMsg := '';
      WorkingTimetableOK := True;
      TempStr := '';

      TRY
        IF NOT FileExists(PathToRailDataFiles + WorkingTimetableFilename + '.' + WorkingTimetableFilenameSuffix) THEN BEGIN
          WorkingTimetableOK := False;
          WorkingTimetableMissing := True;
          Exit;
        END;

        WorkingTimetableADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                          + PathToRailDataFiles + WorkingTimetableFilename + '.' + WorkingTimetableFilenameSuffix
                                                          + ';Persist Security Info=False';
        WorkingTimetableADOConnection.Connected := True;
        WorkingTimetableADOTable.Open;
        Log('W Working Timetable table and connection opened to initialise the working timetable data');

        WorkingTimetableADOTable.Sort := 'First_Station_Departure_Time ASC';
        WorkingTimetableADOTable.First;
        WHILE WorkingTimetableOK
        AND NOT WorkingTimetableWindow.WorkingTimetableADOTable.EOF
        DO BEGIN
          WITH WorkingTimetableADOTable DO BEGIN
            IF FieldByName('Entry Active').AsBoolean THEN BEGIN
              SetLength(WorkingTimetableRecArray, Length(WorkingTimetableRecArray) + 1);

              WITH WorkingTimetableRecArray[High(WorkingTimetableRecArray)] DO BEGIN
                WorkingTimetable_LocoChip := UnknownLocoChip;
                WorkingTimetable_EntryNumStr := IntToStr(High(WorkingTimetableRecArray));
                WorkingTimetable_Status := EntryCreatedFromWorkingTimetable;
                WorkingTimetable_TravelTimeInMinutes := 0;

                FieldName := 'First Station';
                IF FieldByName(FieldName).AsString = '' THEN
                  ErrorMsg := 'Missing first station area'
                ELSE BEGIN
                  WorkingTimetable_FirstStationArea := StrToArea(FieldByName(FieldName).AsString);
                  IF WorkingTimetable_FirstStationArea = UnknownArea THEN
                    ErrorMsg := 'Unknown first station area ''' + FieldByName(FieldName).AsString + '''';
                END;

                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Last Station';
                  IF FieldByName(FieldName).AsString = '' THEN
                    ErrorMsg := 'Missing last station area'
                  ELSE BEGIN
                    WorkingTimetable_LastStationArea := StrToArea(FieldByName(FieldName).AsString);
                    IF WorkingTimetable_LastStationArea = UnknownArea THEN
                      ErrorMsg := 'Unknown last station area ''' + FieldByName(FieldName).AsString + '''';
                  END;
                END;

                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'First_Station_Departure_Time'; { this field name is hyphenated to allow the sort above to work }
                  WorkingTimetable_FirstStationDepartureTime := FieldByName(FieldName).AsDateTime;
                END;

                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'StoppingAt';
                  TempStr := FieldByName(FieldName).AsString;
                  SetLength(WorkingTimetable_StoppingAtStationAreas, 0);
                  SetLength(TempStrArray, 0);
                  IF TempStr <> '' THEN BEGIN
                    { Extract the data into a string array for now - we expect a comma as a delimiter }
                    ExtractSubStringsFromString(TempStr, ',', TempStrArray);

                    FOR I := 0 TO High(TempStrArray) DO BEGIN
                      IF StrToArea(TempStrArray[I]) = UnknownLocation THEN
                        ErrorMsg := 'Unknown area ''' + TempStrArray[I] + ''' in Stopping At field'
                      ELSE
                        AppendToIntegerArray(WorkingTimetable_StoppingAtStationAreas, StrToArea(TempStrArray[I]));
                    END; {FOR}
                  END;

                  IF ErrorMsg = '' THEN BEGIN
                    { Check that the first or last station area isn't one of the intermediate station areas too }
                    I := 0;
                    WHILE (I <= High(WorkingTimetable_StoppingAtStationAreas))
                    AND (ErrorMsg = '')
                    DO BEGIN
                      IF WorkingTimetable_StoppingAtStationAreas[I] = WorkingTimetable_FirstStationArea THEN
                        ErrorMsg := 'Cannot have first station area ' + AreaToStr(WorkingTimetable_FirstStationArea) + ' as an intermediate station area too'
                      ELSE
                        IF WorkingTimetable_StoppingAtStationAreas[I] = WorkingTimetable_LastStationArea THEN
                          ErrorMsg := 'Cannot have last station area ' + AreaToStr(WorkingTimetable_LastStationArea) + ' as an intermediate station area too';
                      Inc(I);
                    END; {WHILE}
                  END;
                END;

                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Type of Train';
                  WorkingTimetable_TrainTypeNum := StrToTrainTypeNum(FieldByName(FieldName).AsString);
                END;

                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Possible Train Classes';
                  TempStr := FieldByName(FieldName).AsString;

                  SetLength(WorkingTimetable_PossibleLocoClasses, 0);
                  SetLength(TempStrArray, 0);
                  IF TempStr <> '' THEN BEGIN
                    { Extract the data into a string array for now - we expect a comma as a delimiter }
                    ExtractSubStringsFromString(TempStr, ',', WorkingTimetable_PossibleLocoClasses);

                    { See if the class is recognised }
                    FOR I := 0 TO High(WorkingTimetable_PossibleLocoClasses) DO BEGIN
                      T := TrainList;
                      ClassFound := False;
                      WHILE (T <> NIL)
                      AND NOT ClassFound
                      DO BEGIN
                        IF WorkingTimetable_PossibleLocoClasses[I] = T^.Train_LocoClassStr THEN
                          ClassFound := True;
                        T := T^.Train_NextRecord;
                      END; {WHILE}
                      IF NOT ClassFound THEN
                        ErrorMsg := 'Invalid train class ''' + WorkingTimetable_PossibleLocoClasses[I] + ''''
                    END; {FOR}
                  END;
                END;

                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Preferred Train Length';
                   IF NOT TryStrToInt(FieldByName(FieldName).AsString, WorkingTimetable_PreferredNumberOfCarriages) THEN
                     ErrorMsg := 'Invalid integer ''' + FieldByName(FieldName).AsString + ''' in Preferred Train Length field';
                END;

                WorkingTimetable_DaysOfTheWeekSet := [];

                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Monday';
                  IF FieldByName(FieldName).AsBoolean THEN
                    WorkingTimetable_DaysOfTheWeekSet := WorkingTimetable_DaysOfTheWeekSet + [Monday];
                END;
                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Tuesday';
                  IF FieldByName(FieldName).AsBoolean THEN
                    WorkingTimetable_DaysOfTheWeekSet := WorkingTimetable_DaysOfTheWeekSet + [Tuesday];
                END;
                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Wednesday';
                  IF FieldByName(FieldName).AsBoolean THEN
                    WorkingTimetable_DaysOfTheWeekSet := WorkingTimetable_DaysOfTheWeekSet + [Wednesday];
                END;
                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Thursday';
                  IF FieldByName(FieldName).AsBoolean THEN
                    WorkingTimetable_DaysOfTheWeekSet := WorkingTimetable_DaysOfTheWeekSet + [Thursday];
                END;
                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Friday';
                  IF FieldByName(FieldName).AsBoolean THEN
                    WorkingTimetable_DaysOfTheWeekSet := WorkingTimetable_DaysOfTheWeekSet + [Friday];
                END;
                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Saturday';
                  IF FieldByName(FieldName).AsBoolean THEN
                    WorkingTimetable_DaysOfTheWeekSet := WorkingTimetable_DaysOfTheWeekSet + [Saturday];
                END;
                IF ErrorMsg = '' THEN BEGIN
                  FieldName := 'Sunday';
                  IF FieldByName(FieldName).AsBoolean THEN
                    WorkingTimetable_DaysOfTheWeekSet := WorkingTimetable_DaysOfTheWeekSet + [Sunday];
                END;

                IF ErrorMsg <> '' THEN BEGIN
                  IF MessageDialogueWithDefault('Error in Working Timetable entry ' + WorkingTimetable_EntryNumStr + ' : '
                                                + ErrorMsg
                                                + CRLF
                                                + 'Do you wish to continue?',
                                                StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
                  THEN
                    ShutDownProgram(UnitRef, 'LoadWorkingTimetable');
                END ELSE BEGIN
                  DebugStr := 'W=' + WorkingTimetable_EntryNumStr + ': '
                               + AreaToStr(WorkingTimetable_FirstStationArea) + ' to ' + AreaToStr(WorkingTimetable_LastStationArea)
                               + ' ' + TimeToHMStr(WorkingTimetable_FirstStationDepartureTime) + ' ' + TrainTypeNumToStr(WorkingTimetable_TrainTypeNum);
                  FOR I := 0 TO High(WorkingTimetable_PossibleLocoClasses) DO
                    DebugStr := DebugStr + ' ' + WorkingTimetable_PossibleLocoClasses[I] + ';';
                  DebugStr := DebugStr + ' PreferredNumberOfCarriages=' + IntToStr(WorkingTimetable_PreferredNumberOfCarriages);
                  Log('W ' + DebugStr);
                END;
              END;
            END; {WITH}
          END; {WITH}
          WorkingTimetableADOTable.Next;
        END; {WHILE}

        { Tidy up the database }
        WorkingTimetableADOTable.Close;
        WorkingTimetableADOConnection.Connected := False;
        Log('W Working Timetable table and connection closed');
      EXCEPT {TRY}
        ON E : Exception DO
          Log('EG InitialiseWorkingTimetable: ' + E.ClassName +' error raised, with message: '+ E.Message);
      END; {TRY}
    END; {WITH}

    IF DisplayWorkingTimetable THEN
      DrawWorkingTimetable(UnitRef, 'LoadWorkingTimetable');
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG InitialiseWorkingTimetable: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { LoadWorkingTimetableFromDatabase }

FUNCTION GetRequiredStationsMaxLength(VAR W : WorkingTimetableRecType) : Extended;
{ Work out the maximum length for all the specified stations }
VAR
  Location : Integer;
  MiddleStationCount : Integer;
  TempResult : Real;

BEGIN
  Result := 0.0;

  WITH W DO BEGIN
    { Check the first station first, then use that value as the maximum, reducing it if other stations are not as big }
    FOR Location := 0 TO High(Locations) DO BEGIN
      IF Locations[Location].Location_Area = WorkingTimetable_FirstStationArea THEN
        IF Locations[Location].Location_LengthInInches > Result THEN
          Result  := Locations[Location].Location_LengthInInches;
    END; {FOR}
    Log('W W=' + WorkingTimetable_EntryNumStr + ':    '
           + 'maximum platform length at the first station is ' + FloatToStr(Result));

    FOR MiddleStationCount := 0 TO High(WorkingTimetable_StoppingAtStationAreas) DO BEGIN
      TempResult := 0.0;
      FOR Location := 0 TO High(Locations) DO BEGIN
        IF Locations[Location].Location_Area = WorkingTimetable_StoppingAtStationAreas[MiddleStationCount] THEN BEGIN
          IF Locations[Location].Location_LengthInInches > TempResult THEN
            TempResult  := Locations[Location].Location_LengthInInches;

          IF Locations[Location].Location_PlatformOrFiddleyardAtUp <> UnknownLocation THEN BEGIN
            IF Locations[Location].Location_LengthInInches + Locations[Locations[Location].Location_PlatformOrFiddleyardAtUp].Location_LengthInInches > TempResult
            THEN
              TempResult  := Locations[Location].Location_LengthInInches + Locations[Locations[Location].Location_PlatformOrFiddleyardAtUp].Location_LengthInInches;
          END;

          IF Locations[Location].Location_PlatformOrFiddleyardAtDown <> UnknownLocation THEN BEGIN
            IF Locations[Location].Location_LengthInInches + Locations[Locations[Location].Location_PlatformOrFiddleyardAtDown].Location_LengthInInches > TempResult
            THEN
              TempResult  := Locations[Location].Location_LengthInInches + Locations[Locations[Location].Location_PlatformOrFiddleyardAtDown].Location_LengthInInches;
          END;
        END;
      END; {FOR}
      IF TempResult < Result THEN BEGIN
        Log('W W=' + WorkingTimetable_EntryNumStr + ':    '
               + 'maximum platform length at intermediate station ' + AreaToStr(WorkingTimetable_StoppingAtStationAreas[MiddleStationCount])
               + ' is ' + FloatToStr(TempResult) + ' which is less than the max platform length ' + FloatToStr(Result) + ' so far');
        Result := TempResult;
      END;
    END; {FOR}

    TempResult := 0.0;
    FOR Location := 0 TO High(Locations) DO BEGIN
      IF Locations[Location].Location_Area = WorkingTimetable_LastStationArea THEN BEGIN
        IF Locations[Location].Location_LengthInInches > TempResult THEN
          TempResult  := Locations[Location].Location_LengthInInches;

        IF Locations[Location].Location_PlatformOrFiddleyardAtUp <> UnknownLocation THEN BEGIN
          IF Locations[Location].Location_LengthInInches + Locations[Locations[Location].Location_PlatformOrFiddleyardAtUp].Location_LengthInInches > TempResult
          THEN
            TempResult  := Locations[Location].Location_LengthInInches + Locations[Locations[Location].Location_PlatformOrFiddleyardAtUp].Location_LengthInInches;
        END;

        IF Locations[Location].Location_PlatformOrFiddleyardAtDown <> UnknownLocation THEN BEGIN
          IF Locations[Location].Location_LengthInInches + Locations[Locations[Location].Location_PlatformOrFiddleyardAtDown].Location_LengthInInches > TempResult
          THEN
            TempResult  := Locations[Location].Location_LengthInInches + Locations[Locations[Location].Location_PlatformOrFiddleyardAtDown].Location_LengthInInches;
        END;
      END;
    END; {FOR}
    IF TempResult = 0 THEN BEGIN
      Log('W W=' + WorkingTimetable_EntryNumStr + ':    ' + 'platform length for ' + AreaToStr(WorkingTimetable_LastStationArea) + ' is zero! - entry cancelled');
      WorkingTimetable_Status := EntryCancelled;
    END ELSE
      IF TempResult < Result THEN BEGIN
        Log('W W=' + WorkingTimetable_EntryNumStr + ':    '
               + 'maximum platform length at last station ' + AreaToStr(WorkingTimetable_LastStationArea) + ' is ' + FloatToStr(TempResult)
               + ' which is less than the max platform length ' + FloatToStr(Result) + ' at the previous stations');
        Result := TempResult;
      END;
  END; {WITH}
END; { GetRequiredStationsMaxLength }

FUNCTION GetAreaToAreaRouteLengthInInches(EntryNumStr : String; Area1, Area2, TrainTypeNum : Integer; Direction : DirectionType; TrainLength : Integer) : Real;
{ Work out how far it is from one area to another }
CONST
  NoJourney = -1;
  IncludeOutOfUseLines = True;
  UseEmergencyRouteing = True;

VAR
  EndLine : Integer;
  ErrorMsg : String;
  I : Integer;
  LinesNotAvailableStr : String;
  Location : Integer;
  OK : Boolean;
  RouteArray : StringArrayType;
  StartLine : Integer;
  SuitableLocationFound : Boolean;
  TempLocationsArray : IntegerArrayType;
  TempLocationsCount : Integer;

BEGIN
  { The default journey time }
  Result := 0;

  TRY
    StartLine := UnknownLine;
    EndLine := UnknownLine;
    OK := True;

    { See where the start area's locations allow routeing to - note that the routeing alogorithm needs actual lines to work with }
    Location := 0;
    SuitableLocationFound := False;
    SetLength(TempLocationsArray, 0);
    WHILE (Location <= High(Locations))
    AND NOT SuitableLocationFound
    DO BEGIN
      IF Locations[Location].Location_Area = Area1 THEN BEGIN
        IF Direction = Up THEN
          FOR I := 0 TO High(Locations[Location].Location_AccessibleLocationsUp) DO
            AppendToIntegerArray(TempLocationsArray, Locations[Location].Location_AccessibleLocationsUp[I])
        ELSE
          FOR I := 0 TO High(Locations[Location].Location_AccessibleLocationsDown) DO
            AppendToIntegerArray(TempLocationsArray, Locations[Location].Location_AccessibleLocationsDown[I]);

        TempLocationsCount := 0;
        WHILE (TempLocationsCount <= High(TempLocationsArray))
        AND NOT SuitableLocationFound
        DO BEGIN
          IF Locations[TempLocationsArray[TempLocationsCount]].Location_Area = Area2 THEN BEGIN
            SuitableLocationFound := True;
            IF Direction = Up THEN BEGIN
              StartLine := Locations[Location].Location_LineAtUp;
              EndLine := Locations[TempLocationsArray[TempLocationsCount]].Location_LineAtDown;
            END ELSE BEGIN
              StartLine := Locations[Location].Location_LineAtDown;
              EndLine := Locations[TempLocationsArray[TempLocationsCount]].Location_LineAtUp;
            END;
          END;
          Inc(TempLocationsCount);
        END; {WHILE}

      END;
      Inc(Location);
    END; {WHILE}

    IF StartLine = UnknownLine THEN BEGIN
      Log('W W=' + EntryNumStr + ':    '
             + 'No start line found for Area1=' + AreaToStr(Area1) + ' in GetJourneyTimeInMinutesFromRouteLength');
      OK := False;
    END;

    IF EndLine = UnknownLine THEN BEGIN
      Log('W W=' + EntryNumStr + ':    '
             + 'No end line found for Area2=' + AreaToStr(Area2) + ' in GetJourneyTimeInMinutesFromRouteLength');
      OK := False;
    END;

    IF OK THEN BEGIN
       { now work out the route }
      FindRouteFromLineAToLineB(NoLocoChip, NoJourney, UnknownSignal, StartLine, EndLine, Direction, TrainTypeNumToTrainType(TrainTypeNum), TrainLength,
                                UseEmergencyRouteing, NOT IncludeOutOfUseLines, RouteArray, LinesNotAvailableStr, ErrorMsg, OK);
      IF NOT OK THEN
        Log('W W=' + EntryNumStr + ':    '
               + 'no route found between ' + LineToStr(StartLine) + ' and ' + LineToStr(EndLine) + ' In GetJourneyTimeInMinutesFromRouteLength (' + ErrorMsg + ')')
      ELSE
        Result := CalculateRouteLength(RouteArray);
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG GetAreaToAreaRouteLengthInInches: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { GetAreaToAreaRouteLengthInInches }

FUNCTION GetJourneyTimeInMinutesFromRouteLength(EntryNumStr : String; StartArea, EndArea, TrainTypeNum : Integer; RouteLengthInInches : Real; Direction : DirectionType)
                                                                                                                                                                  : Integer;
{ Work out how long it should take to get from one area to another }
BEGIN
  { The default journey time }
  Result := CalculateJourneyTimeInMinutes(TrainTypeNum, RouteLengthInInches);

  { add an extra minute to allow for variations in route length when the diagrams are constructed }
  Result :=  Result + 1;
  Log('W W=' + EntryNumStr + ':    '
         + 'route ' + DirectionToStr(Direction) + ' between ' + AreaToStr(StartArea) + ' and ' + AreaToStr(EndArea) + ' is ' + FloatToStr(RouteLengthInInches) + ' inches;'
         + ' time is ' + IntToStr(Result) + IfThen(Result = 1,
                                                   ' minute',
                                                   ' minutes'));
END; { GetJourneyTimeInMinutesFromRouteLength }

FUNCTION GetRouteDirection(FirstArea, SecondArea : Integer) : DirectionType;
{ Check that the proposed journeys work }
VAR
  AreasCount : Integer;

BEGIN
  AreasCount := 0;
  Result := UnknownDirection;
  WHILE (AreasCount <= High(Areas[FirstArea].Area_AccessibleAreasUp))
  AND NOT (Result <> UnknownDirection)
  DO BEGIN
    IF SecondArea = Areas[FirstArea].Area_AccessibleAreasUp[AreasCount] THEN
      Result := Up;
    Inc(AreasCount);
  END; {WHILE}

  IF Result = UnknownDirection THEN BEGIN
    AreasCount := 0;
    WHILE (AreasCount <= High(Areas[FirstArea].Area_AccessibleAreasDown))
    AND NOT (Result <> UnknownDirection)
    DO BEGIN
      IF SecondArea = Areas[FirstArea].Area_AccessibleAreasDown[AreasCount] THEN
        Result := Down;
      Inc(AreasCount);
    END; {WHILE}
  END;
END; { GetRouteDirection }

FUNCTION WhichTrainsAreAvailableAtRequiredTime(EntryNumStr : String; Area : Integer; DepartureTime : TDateTime) : IntegerArrayType;
{ See which of the locos are in the right starting place and if they are available at the required departure time for the required journey time }
VAR
  T : Train;

BEGIN
  SetLength(Result, 0);
  T := TrainList;
  WHILE T <> NIL DO BEGIN
    WITH T^ DO BEGIN
      { Is the train there to start with? }
      IF Train_WorkingTimetableLastArrivalArea = UnknownArea THEN BEGIN
        { no train journeys have been created }
        IF (T^.Train_LastLocation <> UnknownLocation)
        AND (Locations[T^.Train_LastLocation].Location_Area = Area)
        THEN BEGIN
          AppendToIntegerArray(Result, T^.Train_LocoChip);
          Log('W W=' + EntryNumStr + ':      '
                 + LocoChipToStr(T^.Train_LocoChip) + ' added as ' + AreaToStr(Area) + ' is its initial last location');
        END;
      END ELSE BEGIN
        { see where train journeys end up, and when, allowing time for passengers to detrain/entrain }
        { should it be allowed to delay train departures to allow the same train to be used? **** }
        IF T^.Train_WorkingTimetableLastArrivalArea = Area THEN BEGIN
          IF CompareTime(T^.Train_WorkingTimetableLastArrivalTime, DepartureTime) <= 0 THEN BEGIN
            AppendToIntegerArray(Result, T^.Train_LocoChip);
            Log('W W=' + EntryNumStr + ':      '
                   + LocoChipToStr(T^.Train_LocoChip)
                   + ' added as its last arrival time of ' + TimeToHMSStr(T^.Train_WorkingTimetableLastArrivalTime)
                   + ' at ' + AreaToStr(Area)
                   + ' is equal to or earlier than required departure time of ' + TimeToHMSStr(DepartureTime));
          END ELSE
            Log('W W=' + EntryNumStr + ':      '
                   + LocoChipToStr(T^.Train_LocoChip)
                   + ' not added as its last arrival time of ' + TimeToHMSStr(T^.Train_WorkingTimetableLastArrivalTime)
                   + ' at ' + AreaToStr(Area)
                   + ' is later than the required departure time of ' + TimeToHMSStr(DepartureTime));
        END;
      END;
    END; {WITH}
    T := T^.Train_NextRecord;
  END; {WHILE}
END; { WhichTrainsAreAvailableAtRequiredTime }

FUNCTION WhichAvailableTrainsAreSuitable(EntryNumStr : String; AllTrains : IntegerArrayType; PossibleLocoClasses : StringArrayType; MaxLength : Real) : IntegerArrayType;
{ Select those trains match the classes required and which fit the platforms }

  PROCEDURE SortTrainsInIntegerArrayByLength(EntryNumStr, LocoClassStr : String; VAR TrainsArray : IntegerArrayType);
  { Sort an array of train locochip numbers by train length }
  VAR
    I, J, K : Integer;
    MaxTrainLength : Real;
    SortedTrainsArray : IntegerArrayType;
    TempPos : Integer;
    T : Train;
    UnsortedTrainsArray : IntegerArrayType;

  BEGIN
    TRY
      IF Length(TrainsArray) = 1 THEN
        { there's no point in sorting a list of one }
        Exit
      ELSE BEGIN
        { Copy the old array across or can't alter the length of it }
        SetLength(UnsortedTrainsArray, Length(TrainsArray));
        FOR I := 0 TO High(TrainsArray) DO
          UnsortedTrainsArray[I] := TrainsArray[I];

        SetLength(SortedTrainsArray, 0);

        { Now do the sort }
        FOR I := 0 TO High(UnsortedTrainsArray) DO BEGIN
          TempPos := -1;
          MaxTrainLength := 999;
          FOR J := 0 TO High(UnsortedTrainsArray) DO BEGIN
            T := GetTrainRecord(UnsortedTrainsArray[J]);
            IF T^.Train_FixedLengthInInches <> 0 THEN
              T^.Train_CurrentLengthInInches := T^.Train_FixedLengthInInches
            ELSE
              T^.Train_CurrentLengthInInches := T^.Train_LastLengthInInches;

            IF T^.Train_CurrentLengthInInches < MaxTrainLength THEN BEGIN
              MaxTrainLength := T^.Train_CurrentLengthInInches;
              TempPos := J;
            END;
          END; {FOR}

          SetLength(SortedTrainsArray, Length(SortedTrainsArray) + 1);
          SortedTrainsArray[High(SortedTrainsArray)] := UnsortedTrainsArray[TempPos];

          { and remove the element from the original array }
          FOR K := TempPos TO (Length(UnsortedTrainsArray) - 2) DO
            UnsortedTrainsArray[K] := UnsortedTrainsArray[K + 1];
          SetLength(UnsortedTrainsArray, Length(UnsortedTrainsArray) -1);
        END; {FOR}

        { and copy the sorted data back }
        SetLength(TrainsArray, Length(SortedTrainsArray));
        FOR I := 0 TO High(SortedTrainsArray) DO
          TrainsArray[I] := SortedTrainsArray[I];

        IF Length(SortedTrainsArray) > 1 THEN
          Log('W W=' + EntryNumStr + ':    '
                 + 'train class ' + LocoClassStr + ' sorted by train length=' + ListLocoChipsInIntegerArray(SortedTrainsArray));
      END;
    EXCEPT
      ON E : Exception DO
        Log('EG SortTrainsInIntegerArrayByLength: ' + E.ClassName +' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { SortTrainsInIntegerArrayByLength }

VAR
  ClassCount : Integer;
  DebugStr : String;
  ElementPos : Integer;
  I : Integer;
//    InaccessibleTrainFound : Boolean;
  PossiblySuitableTrains : IntegerArrayType;
  SuitableTrainsCount : Integer;
  T : Train;
  TempArray1 : IntegerArrayType;
  TempArray2 : IntegerArrayType;
  TempLocoChip : Integer;
//    TrainCount : Integer;
  TrainsSelected : Integer;
  UnsuitableClassArray : IntegerArrayType;
  UnsuitableLengthArray : IntegerArrayType;

BEGIN { WhichAvailableTrainsAreSuitable }
  SetLength(Result, 0);
  FOR ClassCount := 0 TO High(PossibleLocoClasses) DO BEGIN
    { use a temporary array for each class otherwise the length sort jumbles the classes up }
    SetLength(PossiblySuitableTrains, 0);

    { For each class select available locos }
    SuitableTrainsCount := 0;
    TrainsSelected := 0;
    SetLength(UnsuitableClassArray, 0);
    SetLength(UnsuitableLengthArray, 0);
    SetLength(TempArray1, 0);
    SetLength(TempArray2, 0);

    WHILE SuitableTrainsCount <= High(AllTrains) DO BEGIN
      T := GetTrainRecord(AllTrains[SuitableTrainsCount]);
      IF T^.Train_LocoClassStr <> PossibleLocoClasses[ClassCount] THEN
        AppendToIntegerArray(UnsuitableClassArray, T^.Train_LocoChip)
      ELSE
        IF (T^.Train_CurrentLengthInInches > MaxLength)
        OR (T^.Train_FixedLengthInInches > MaxLength)
        OR (T^.Train_LastLengthInInches > MaxLength)
        THEN BEGIN
          AppendToIntegerArray(UnsuitableLengthArray, T^.Train_LocoChip);
          Log('W W=' + EntryNumStr + ':    '
                 + LocoChipToStr(T^.Train_LocoChip) + ' is from class ' + PossibleLocoClasses[ClassCount] + ' but is too long');
        END ELSE BEGIN
          Inc(TrainsSelected);
          AppendToIntegerArray(TempArray1, AllTrains[SuitableTrainsCount]);
          AppendToIntegerArray(TempArray2, AllTrains[SuitableTrainsCount]);
        END;
      Inc(SuitableTrainsCount);
    END; {WHILE}

    IF TrainsSelected = 0 THEN
      Log('W W=' + EntryNumStr + ':      '
             + 'no trains from class ' + PossibleLocoClasses[ClassCount] + ' to sort')
    ELSE BEGIN
      { Make the list of locos for each supplied class random, or otherwise the same train will always be selected first }
      REPEAT
        TempLocoChip := RandomFrom(TempArray1);
        { remove the element from the temporary array so we aren't given it again, or the list would never shrink }
        IsElementInIntegerArray(TempArray1, TempLocoChip, ElementPos);
        DeleteElementFromIntegerArray(TempArray1, ElementPos);
        AppendToIntegerArray(PossiblySuitableTrains, TempLocoChip);
      UNTIL Length(TempArray1) = 0;

      DebugStr := '';
      IF Length(UnsuitableClassArray) > 0 THEN BEGIN
        FOR I := 0 TO High(UnsuitableClassArray) DO
          DebugStr := DebugStr + LocoChipToStr(UnsuitableClassArray[I]) + ' ';
        Log('W W=' + EntryNumStr + ':      '
               + 'looking for class ' + PossibleLocoClasses[ClassCount] + ': '
               + IfThen(Length(UnsuitableClassArray) = 1,
                        'train ' + DebugStr + 'is ineligible',
                        'trains ' + DebugStr + 'are ineligible'));
      END;

      DebugStr := '';
      IF Length(UnsuitableLengthArray) > 0 THEN BEGIN
        FOR I := 0 TO High(UnsuitableLengthArray) DO
          DebugStr := DebugStr + LocoChipToStr(UnsuitableLengthArray[I]) + ' ';
        Log('W W=' + EntryNumStr + ':      '
               + 'looking for trains shorter than ' + FloatToStr(MaxLength) + ': '
               + IfThen(Length(UnsuitableLengthArray) = 1,
                        'train ' + DebugStr + 'exceeds maximum station length',
                        'trains ' + DebugStr + 'exceed maximum station length'));
      END;

      { List possible loco class... }
      DebugStr := DebugStr + 'trains in class ' + PossibleLocoClasses[ClassCount] + ' are ';

      { ...and suitable trains (but not in random order or it makes comparison between different log files difficult) }
      IF Length(TempArray2) > 0 THEN BEGIN
        FOR I := 0 TO High(TempArray2) - 1 DO
          DebugStr := DebugStr + LocoChipToStr(TempArray2[I]) + ', ';
        DebugStr := DebugStr + LocoChipToStr(TempArray2[High(TempArray2)]);
      END;
      Log('W W=' + EntryNumStr + ':      '
             + DebugStr);

//        { See if the train is accessible }
//        TrainCount := 0;
//        WHILE TrainCount <= High(PossiblySuitableTrains) DO BEGIN
//          InaccessibleTrainFound := False;
//          T := GetTrainRecord(PossiblySuitableTrains[TrainCount]);
//          IF T^.Train_WorkingTimetableLastArrivalArea = UnknownArea THEN BEGIN
//            { We can only use last location if the train hasn't been relocated as part of this process }
//            IF T^.Train_LastLocation <> UnknownLocation THEN
//              IF Locations[T^.Train_LastLocation].Location_LineAtUpIsEndOfLine
//              AND (RouteDirection = Up)
//              THEN
//                InaccessibleTrainFound := True
//              ELSE
//                IF Locations[T^.Train_LastLocation].Location_LineAtDownIsEndOfLine
//                AND (RouteDirection = Down)
//                HEN
//                  InaccessibleTrainFound := True;
//          END;
//
//          IF InaccessibleTrainFound THEN BEGIN
//            DeleteElementFromIntegerArray(PossiblySuitableTrains, TrainCount);
//            Log('W W=' + EntryNumStr + ':    '
//                   + 'deleting ' + LocoChipToStr(T^.Train_LocoChip) + ' from PossiblySuitableTrainsArray as the train is inaccessible');
//
//            Dec(TrainCount);
//          END;
//          Inc(TrainCount);
//        END; {WHILE}

      SortTrainsInIntegerArrayByLength(EntryNumStr, PossibleLocoClasses[ClassCount], PossiblySuitableTrains);
    END;

    FOR I := 0 TO High(PossiblySuitableTrains) DO
      AppendToIntegerArray(Result, PossiblySuitableTrains[I]);
  END; {FOR}
END; { WhichAvailableTrainsAreSuitable }

FUNCTION GetFirstTrainCurrentlyInUse(EntryNumStr : String; SuitableTrains : IntegerArrayType) : Integer;
{ Ascertain which trains are currently in use - this means we don't have to warm up or crew unnecessary locos }
VAR
  I : Integer;
  InUseTrainFound : Boolean;
  T : Train;

BEGIN
  Result := UnknownLocoChip;

  I := 0;
  InUseTrainFound := False;
  WHILE (I <= High(SuitableTrains))
  AND NOT InUseTrainFound
  DO BEGIN
    T := GetTrainRecord(SuitableTrains[I]);
    IF T^.Train_WorkingTimetableLastArrivalTime <> 0 THEN BEGIN
      InUseTrainFound := True;
      Result := SuitableTrains[I];
      Log('W W=' + EntryNumStr + ': '
             + LocoChipToStr(Result) + ' is already in use so is selected');
    END;
    Inc(I);
  END; {WHILE}
END; { GetFirstTrainCurrentlyInUse }

PROCEDURE InsertAdditionalWorkingTimetableEntry(EntryNumStr : String; SelectedLocoChip, WorkingTimetableCount, StartArea, EndArea, TravelTimeInMinutes : Integer;
                                                DepartureTime : TDateTime; Direction : DirectionType);
{ Insert a new working timetable entry to get us to the point of initial departure. The entry takes some data from the entry it precedes. }
BEGIN
  TRY
    Log('W W=' + EntryNumStr + ': '
               + 'NEW WORKING TIMETABLE ENTRY:'
               + ' inserting additional journey ' + DirectionToStr(Direction)
               + ' from ' + UpperCase(AreaToStr(StartArea))
               + ' to ' + UpperCase(AreaToStr(EndArea))
               + ' at ' + TimeToHMSStr(DepartureTime)
               + ' on ' + DaysOfTheWeekSetToStr(WorkingTimetableRecArray[WorkingTimetableCount].WorkingTimetable_DaysOfTheWeekSet)
               + ' using loco ' + LocoChipToStr(SelectedLocoChip));

    InsertEntryInWorkingTimetable(WorkingTimetableRecArray, WorkingTimetableCount);
    Debug('Insert no. ' + TestCountStr);
    WITH WorkingTimetableRecArray[WorkingTimetableCount] DO BEGIN
      WorkingTimetable_LocoChip := SelectedLocoChip;
      WorkingTimetable_EntryNumStr := EntryNumStr;
      WorkingTimetable_Status := AdditionalEntryCreated;
      WorkingTimetable_Direction := Direction;
      WorkingTimetable_LastStationArea := EndArea;
      WorkingTimetable_FirstStationArea := StartArea;
      WorkingTimetable_TrainTypeNum := StrToTrainTypeNum('Empty Coaching Stock');
      SetLength(WorkingTimetable_StoppingAtStationAreas, 0);
      WorkingTimetable_FirstStationDepartureTime := DepartureTime;
//      IF Length(WorkingTimetableRecArray) > (WorkingTimetableCount + 1) THEN BEGIN
        WorkingTimetable_PossibleLocoClasses := WorkingTimetableRecArray[WorkingTimetableCount + 1].WorkingTimetable_PossibleLocoClasses;
        WorkingTimetable_PreferredNumberOfCarriages := WorkingTimetableRecArray[WorkingTimetableCount + 1].WorkingTimetable_PreferredNumberOfCarriages;
        WorkingTimetable_DaysOfTheWeekSet := WorkingTimetableRecArray[WorkingTimetableCount + 1].WorkingTimetable_DaysOfTheWeekSet;
//      END;
      WorkingTimetable_TravelTimeInMinutes := TravelTimeInMinutes;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG InsertAdditionalWorkingTimetableEntry: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { InsertAdditionalWorkingTimetableEntry }

PROCEDURE FindAdditionalTrains(EntryNumStr : String; StationsMaxLength : Real; WorkingTimetableCount : Integer; WorkingTimetableRec : WorkingTimetableRecType;
                               OUT SuitableAdditionalTrains : SuitableAdditionalTrainsArrayType; OUT OK : Boolean);
{ Find an additional train two moves away from the first station }
TYPE
  AreasWithTrainsRecType = RECORD
                             AreasWithTrains_Area : Integer;
                             AreasWithTrains_AreaAccessible : Boolean;
                             AreasWithTrains_FirstReversingAreaToFinalReversingAreaDirection : DirectionType;
                             AreasWithTrains_NearestReversingArea : Integer;
                             AreasWithTrains_RequiredTime : TDateTime;
                             AreasWithTrains_SuitableTrains : IntegerArrayType;
                             AreasWithTrains_ToNearestReversingAreaDirection : DirectionType;
                             AreasWithTrains_TravelTimeFromFirstReversingAreaToFinalReversingAreaInMinutes : Integer;
                             AreasWithTrains_TravelTimeToEndAreaInMinutes : Integer;
                             AreasWithTrains_TravelTimeToNearestReversingAreaInMinutes : Integer;
                           END;

  PROCEDURE CheckAccessForAreasWithTrains(EntryNumStr : String; WorkingTimetableRec : WorkingTimetableRecType; EndArea : Integer;
                                          VAR AreasWithTrainsArray : ARRAY OF AreasWithTrainsRecType; OUT RouteDirection : DirectionType;
                                          OUT AllAccessibleAreas : IntegerArrayType);
  { See whether the areas with trains can directly access the given area, i.e. without changing direction }
  VAR
    I, J : Integer;
    T : Train;
    TempLengthInInches : Real;

  BEGIN
    WITH WorkingTimetableRec DO BEGIN
      FOR I := 0 TO High(AreasWithTrainsArray) DO BEGIN
        WITH AreasWithTrainsArray[I] DO BEGIN
          RouteDirection := GetRouteDirection(AreasWithTrains_Area, EndArea);
          IF RouteDirection = UnknownDirection THEN
            Log('W W=' + EntryNumStr + ':    '
                   + 'No route between ' + AreaToStr(AreasWithTrains_Area) + ' and ' + AreaToStr(EndArea))
          ELSE BEGIN
            { See if any of the supplied trains are not able to leave in this direction - use the record of the train's last resting place to find this out }
            J := 0;
            WHILE J <= High(AreasWithTrains_SuitableTrains) DO BEGIN
              T := GetTrainRecord(AreasWithTrains_SuitableTrains[J]);
              IF T^.Train_WorkingTimetableLastArrivalArea = UnknownArea THEN BEGIN
                { We can only use last location if the train hasn't been relocated as part of this process }
                IF T^.Train_LastLocation <> UnknownLocation THEN
                  IF Locations[T^.Train_LastLocation].Location_LineAtUpIsEndOfLine
                  AND (RouteDirection = Up)
                  THEN BEGIN
                    Log('W W=' + EntryNumStr + ':    '
                           + 'train ' + LocochipToStr(AreasWithTrains_SuitableTrains[J])
                           + ' invalid as route direction is up and train''s exit from last location is down only');
                    DeleteElementFromIntegerArray(AreasWithTrains_SuitableTrains, J);
                    Dec(J);
                END ELSE
                  IF Locations[T^.Train_LastLocation].Location_LineAtDownIsEndOfLine
                  AND (RouteDirection = Down)
                  THEN BEGIN
                    Log('W W=' + EntryNumStr + ':    '
                           + 'train ' + LocochipToStr(AreasWithTrains_SuitableTrains[J])
                           + ' invalid as route direction is down and train''s exit from last location is up only');
                    DeleteElementFromIntegerArray(AreasWithTrains_SuitableTrains, J);
                    Dec(J);
                  END;
              END;
              Inc(J);
            END; {WHILE}

            IF Length(AreasWithTrains_SuitableTrains) = 0 THEN BEGIN
              AreasWithTrains_AreaAccessible := False;
              Log('W W=' + EntryNumStr + ':    '
                     + 'Area with trains ' + AreaToStr(AreasWithTrains_Area) + ' excluded: all suitable trains have been deleted');
            END ELSE BEGIN
              TempLengthInInches := GetAreaToAreaRouteLengthInInches(EntryNumStr,
                                                                     AreasWithTrains_Area,
                                                                     EndArea,
                                                                     WorkingTimetable_TrainTypeNum,
                                                                     RouteDirection,
                                                                     WorkingTimetable_PreferredNumberOfCarriages);
              AreasWithTrains_TravelTimeToEndAreaInMinutes := GetJourneyTimeInMinutesFromRouteLength(EntryNumStr,
                                                                                                     AreasWithTrains_Area,
                                                                                                     EndArea,
                                                                                                     WorkingTimetable_TrainTypeNum,
                                                                                                     TempLengthInInches,
                                                                                                     RouteDirection);
              IF AreasWithTrains_TravelTimeToEndAreaInMinutes = 0 THEN BEGIN
                AreasWithTrains_AreaAccessible := False;
                Log('W W=' + EntryNumStr + ':    '
                       + 'Area with trains ' + AreaToStr(AreasWithTrains_Area) + ' invalidated: '
                       + 'no journey time is available to ' + AreaToStr(EndArea));
              END;
            END;

            IF AreasWithTrains_AreaAccessible THEN
              AppendToIntegerArray(AllAccessibleAreas, AreasWithTrains_Area);
          END;
        END; {WITH}
      END; {FOR}
    END; {WITH}
  END; { CheckAccessForAreasWithTrains }

VAR
  AllAccessibleAreas : IntegerArrayType;
  Area : Integer;
  AreasWithTrainsArray : ARRAY OF AreasWithTrainsRecType;
  AvailableTrainsThatAreSuitable : IntegerArrayType;
  DebugStr : String;
  I, J : Integer;
  MinLengthInInches : Real;
  NearestReversingAreaToAreaWithTrainsFound : Boolean;
  NearestReversingAreaToFirstStationArea : Integer;
  NearestReversingAreaToFirstStationDirection : DirectionType;
  NearestReversingAreaToFirstStationTravelTimeInMinutes : Integer;
  RequiredTime : TDateTime;
  T : Train;
  TempAccessibleAreas : IntegerArrayType;
  TempDirection : DirectionType;
  TempLengthInInches : Real;
  TempTravelTimeInMinutes : Integer;
  TrainsAvailableAtRequiredTime : IntegerArrayType;

BEGIN { FindAdditionalTrains }
  WITH WorkingTimetableRec DO BEGIN
    OK := False;
    SetLength(AllAccessibleAreas, 0);
    SetLength(AreasWithTrainsArray, 0);
    NearestReversingAreaToFirstStationTravelTimeInMinutes := 0;

    { First choose those areas where there are potentially suitable trains available half-an-hour before they are required. Half-an-hour is chosen at random, though it
      would probably not be sensible to move trains into position over a much longer period than this.
    }
    Log('W W=' + EntryNumStr + 'A: '
         + 'NEW WORKING TIMETABLE ENTRY:'
         + ' checking possible additional journey to ' + UpperCase(AreaToStr(WorkingTimetable_FirstStationArea)) + ' by means of making one movement');

    RequiredTime := IncMinute(WorkingTimetable_FirstStationDepartureTime, -30);
    Area := 0;
    WHILE Area <= High(Areas) DO BEGIN
      Log('W W=' + EntryNumStr + 'A:    '
             + 'Seeking trains available at area ' + AreaToStr(Area) + ' at required time of ' + TimeToHMStr(RequiredTime));
      IF Area <> WorkingTimetable_FirstStationArea THEN BEGIN
        SetLength(TrainsAvailableAtRequiredTime, 0);
        TrainsAvailableAtRequiredTime := WhichTrainsAreAvailableAtRequiredTime(EntryNumStr + 'A',
                                                                               Area,
                                                                               RequiredTime);
        IF Length(TrainsAvailableAtRequiredTime) > 0 THEN BEGIN
          AvailableTrainsThatAreSuitable := WhichAvailableTrainsAreSuitable(EntryNumStr + 'A',
                                                                            TrainsAvailableAtRequiredTime,
                                                                            WorkingTimetable_PossibleLocoClasses,
                                                                            StationsMaxLength);
          IF Length(AvailableTrainsThatAreSuitable) = 0 THEN
            Log('W W=' + EntryNumStr + 'A:      '
                   + 'no trains are suitable')
          ELSE BEGIN
            SetLength(AreasWithTrainsArray, Length(AreasWithTrainsArray) + 1);
            WITH AreasWithTrainsArray[High(AreasWithTrainsArray)] DO BEGIN
              AreasWithTrains_Area := Area;
              AreasWithTrains_AreaAccessible := True;
              AreasWithTrains_RequiredTime := 0;
              AreasWithTrains_SuitableTrains := AvailableTrainsThatAreSuitable;
              AreasWithTrains_TravelTimeToEndAreaInMinutes := 0;
            END; {WITH}
          END;
        END;
      END;
      Inc(Area);
    END; {WHILE}

    IF Length(AreasWithTrainsArray) = 0 THEN
      Log('W W=' + EntryNumStr + 'A:    '
             + 'No trains are available at the required time of ' + TimeToHMStr(RequiredTime))
    ELSE BEGIN
      Log('W W=' + EntryNumStr + 'A:    '
             + 'The following trains are available at the required time of ' + TimeToHMStr(RequiredTime) + ':');
      FOR I := 0 TO High(AreasWithTrainsArray) DO
        Log('W W=' + EntryNumStr + 'A:      '
               + AreaToStr(AreasWithTrainsArray[I].AreasWithTrains_Area) + ': ' + ListLocoChipsInIntegerArray(AreasWithTrainsArray[I].AreasWithTrains_SuitableTrains));

      Log('W W=' + EntryNumStr + 'A: '
             + 'Checking access for areas with trains:');
      CheckAccessForAreasWithTrains(EntryNumStr + 'A', WorkingTimetableRec, WorkingTimetable_FirstStationArea, AreasWithTrainsArray, TempDirection, AllAccessibleAreas);

      FOR I := 0 TO High(AreasWithTrainsArray) DO
        WITH AreasWithTrainsArray[I] DO
          AreasWithTrains_RequiredTime := IncMinute(WorkingTimetable_FirstStationDepartureTime,
                                                    -AreasWithTrains_TravelTimeToEndAreaInMinutes
                                                    -StationOppositeDirectionExitMinimumWaitTimeInMinutes);
      IF Length(AllAccessibleAreas) > 0 THEN BEGIN

        Log('W W=' + EntryNumStr + 'A: '
               + IfThen(Length(AllAccessibleAreas) = 1,
                        'Only accessible area (in one move) is ',
                        'Accessible areas (in one move) are ')
               + AreaArrayToStr(AllAccessibleAreas));
        I := 0;
        WHILE I <= High(AllAccessibleAreas) DO BEGIN
          WITH AreasWithTrainsArray[I] DO BEGIN
            IF AreasWithTrains_AreaAccessible THEN BEGIN
              OK := True;
              J := 0;
              WHILE J <= High(AreasWithTrains_SuitableTrains) DO BEGIN
                SetLength(SuitableAdditionalTrains, Length(SuitableAdditionalTrains) + 1);
                WITH SuitableAdditionalTrains[High(SuitableAdditionalTrains)] DO BEGIN
                  SuitableAdditionalTrains_LocoChip := AreasWithTrains_SuitableTrains[J];
                  SuitableAdditionalTrains_StartArea := AreasWithTrains_Area;
                  SuitableAdditionalTrains_EndArea1 := WorkingTimetable_FirstStationArea;
                  SuitableAdditionalTrains_EndArea2 := UnknownArea;
                  SuitableAdditionalTrains_EndArea3 := UnknownArea;
                  SuitableAdditionalTrains_Direction1 := TempDirection;
                  SuitableAdditionalTrains_Direction2 := UnknownDirection;
                  SuitableAdditionalTrains_Direction3 := UnknownDirection;
                  SuitableAdditionalTrains_Weighting := 0;
                  SuitableAdditionalTrains_WeightingStr:= '';
                  SuitableAdditionalTrains_TravelTimeInMinutes1 := AreasWithTrains_TravelTimeToEndAreaInMinutes;
                  SuitableAdditionalTrains_TravelTimeInMinutes2 := 0;
                  SuitableAdditionalTrains_TravelTimeInMinutes3 := 0;
                  SuitableAdditionalTrains_DepartureTime1 := AreasWithTrains_RequiredTime;
                  SuitableAdditionalTrains_DepartureTime2 := 0;
                  SuitableAdditionalTrains_DepartureTime3 := 0;

                  T := GetTrainRecord(AreasWithTrains_SuitableTrains[J]);
                  IF T^.Train_FixedLengthInInches <> 0 THEN
                    SuitableAdditionalTrains_LengthInInches := T^.Train_FixedLengthInInches
                  ELSE
                    SuitableAdditionalTrains_LengthInInches := T^.Train_LastLengthInInches;
                  SuitableAdditionalTrains_NumberOfCarriages := T^.Train_NumberOfCarriages;
                  SuitableAdditionalTrains_InUse := (T^.Train_WorkingTimetableLastArrivalTime <> 0);
                END; {WITH}

                Log('W W=' + EntryNumStr + 'A:    '
                       + 'adding ' + LocoChipToStr(SuitableAdditionalTrains[J].SuitableAdditionalTrains_LocoChip) + ' to list of suitable additional trains');
                Inc(J);
              END {WHILE}
            END;
          END; {WITH}

          Inc(I);
        END; {WHILE}
      END ELSE BEGIN
        Log('W W=' + EntryNumStr + 'A: '
               + 'No accessible areas' + ' {LINE=AFTER}');

        { No accessible areas in one move, so try two moves }
        Log('W W=' + EntryNumStr + 'B: '
             + 'NEW WORKING TIMETABLE ENTRY:'
             + ' checking possible additional journey to ' + UpperCase(AreaToStr(WorkingTimetable_FirstStationArea)) + ' by means of making two movements');

        Log('W W=' + EntryNumStr + 'A:    '
               + 'The following trains are available at the required time of ' + TimeToHMStr(RequiredTime) + ':');
        FOR I := 0 TO High(AreasWithTrainsArray) DO
          Log('W W=' + EntryNumStr + 'A:      '
                 + AreaToStr(AreasWithTrainsArray[I].AreasWithTrains_Area) + ': ' + ListLocoChipsInIntegerArray(AreasWithTrainsArray[I].AreasWithTrains_SuitableTrains));

        IF Length(ReversingAreas) = 0 THEN
          Log('W W=' + EntryNumStr + 'B:      '
                 + 'no reversing areas found')
        ELSE
          DebugStr := '';
          FOR I := 0 TO High(ReversingAreas) - 1 DO
            DebugStr := DebugStr + ' ' + AreaToStr(ReversingAreas[I].ReversingAreas_Area) + ';';
          DebugStr := DebugStr + ' ' + AreaToStr(ReversingAreas[High(ReversingAreas)].ReversingAreas_Area);
          Log('W W=' + EntryNumStr + 'B:      '
                 + 'reversing areas are:' + DebugStr);

          IF (Length(ReversingAreas) = 1)
          AND (ReversingAreas[1].ReversingAreas_Area = WorkingTimetable_FirstStationArea)
          THEN
            Log('W W=' + EntryNumStr + 'B:      '
                   + 'only one reversing area found but it is also the main station area')
          ELSE BEGIN
            { Reset some of the AreasWithTrainsArray variables possibly already initialised }
            FOR I := 0 TO High(AreasWithTrainsArray) DO BEGIN
              WITH AreasWithTrainsArray[I] DO BEGIN
                AreasWithTrains_AreaAccessible := True;
                AreasWithTrains_ToNearestReversingAreaDirection := UnknownDirection;
                AreasWithTrains_RequiredTime := 0;
                AreasWithTrains_TravelTimeToEndAreaInMinutes := 0;
              END; {WITH}
            END; {FOR}

//GetNearestReversingAreaToFirstStationArea

            { Reset most of the ReversingAreas variables possibly already initialised }
            FOR I := 0 TO High(ReversingAreas) DO BEGIN
              WITH ReversingAreas[I] DO BEGIN
                ReversingAreas_AccessibleToFirstStationArea := True;
                ReversingAreas_NearestToFirstStationArea := False;
                ReversingAreas_TimeToFirstStationAreaInMinutes := 0;
                ReversingAreas_ToFirstStationAreaDirection := UnknownDirection;
              END; {WITH}
            END; {FOR}

            { Add the time from each reversing area to the first station area here. This cannot be done globally as it will change when the first station area changes. }
            Log('W W=' + EntryNumStr + 'B: '
                   + 'Add the time from each reversing area to the first station area');
            MinLengthInInches := 999.0;
            FOR I := 0 TO High(ReversingAreas) DO BEGIN
              WITH ReversingAreas[I] DO BEGIN
                IF ReversingAreas_Area <> WorkingTimetable_FirstStationArea THEN BEGIN
                  ReversingAreas_ToFirstStationAreaDirection := GetRouteDirection(ReversingAreas_Area, WorkingTimetable_FirstStationArea);
                  IF ReversingAreas_ToFirstStationAreaDirection = UnknownDirection THEN BEGIN
                    ReversingAreas_AccessibleToFirstStationArea := False;
                    Log('W W=' + EntryNumStr + 'B:    '
                           + 'First station area ' + AreaToStr(WorkingTimetable_FirstStationArea)
                           + ' is not accessible from reversing area ' + AreaToStr(ReversingAreas_Area));
                  END ELSE BEGIN
                    TempLengthInInches := GetAreaToAreaRouteLengthInInches(EntryNumStr + 'B',
                                                                           ReversingAreas_Area,
                                                                           WorkingTimetable_FirstStationArea,
                                                                           WorkingTimetable_TrainTypeNum,
                                                                           ReversingAreas_ToFirstStationAreaDirection,
                                                                           WorkingTimetable_PreferredNumberOfCarriages);
                    IF TempLengthInInches = 0 THEN
                      Log('W W=' + EntryNumStr + 'B:    '
                             + 'No route from reversing area ' + AreaToStr(ReversingAreas_Area)
                             + ' to first station area ' + AreaToStr(WorkingTimetable_FirstStationArea))
                    ELSE BEGIN
                      ReversingAreas_TimeToFirstStationAreaInMinutes := GetJourneyTimeInMinutesFromRouteLength(
                                                                                          EntryNumStr + 'B',
                                                                                          ReversingAreas_Area,
                                                                                          WorkingTimetable_FirstStationArea,
                                                                                          WorkingTimetable_TrainTypeNum,
                                                                                          TempLengthInInches,
                                                                                          ReversingAreas_ToFirstStationAreaDirection);
                      IF TempLengthInInches < MinLengthInInches THEN BEGIN
                        MinLengthInInches := TempLengthInInches;
                        NearestReversingAreaToFirstStationArea := ReversingAreas_Area;
                        NearestReversingAreaToFirstStationTravelTimeInMinutes := ReversingAreas_TimeToFirstStationAreaInMinutes;
                        NearestReversingAreaToFirstStationDirection := ReversingAreas_ToFirstStationAreaDirection;
                      END;
                    END;
                  END;
                END;
              END; {WITH}
            END; {FOR}

            Log('W W=' + EntryNumStr + 'B: '
                   + 'Reversing area ' + AreaToStr(NearestReversingAreaToFirstStationArea)
                   + ' is the nearest to the first station area ' + AreaToStr(WorkingTimetable_FirstStationArea));


                     
            Log('W W=' + EntryNumStr + 'B: '
                   + 'Add the time from each reversing area to each area with trains');
            MinLengthInInches := 999.0;
            FOR I := 0 TO High(ReversingAreas) DO BEGIN
              WITH ReversingAreas[I] DO BEGIN
                FOR J := 0 TO High(AreasWithTrainsArray) DO BEGIN
                  WITH AreasWithTrainsArray[J] DO BEGIN
                    IF ReversingAreas_Area = AreasWithTrains_Area THEN
                      Log('W W=' + EntryNumStr + 'B:    '
                             + 'First station area ' + AreaToStr(WorkingTimetable_FirstStationArea) + ' is the same as reversing area - excluding it')
                    ELSE BEGIN
                      TempDirection := GetRouteDirection(AreasWithTrains_Area, ReversingAreas_Area);
                      IF TempDirection = UnknownDirection THEN BEGIN
                        AreasWithTrains_AreaAccessible := False;
                        Log('W W=' + EntryNumStr + 'B:    '
                               + 'Area with trains ' + AreaToStr(AreasWithTrains_Area) + ' is not accessible from reversing area ' + AreaToStr(ReversingAreas_Area));
                      END ELSE BEGIN
                        TempLengthInInches := GetAreaToAreaRouteLengthInInches(EntryNumStr + 'B',
                                                                               AreasWithTrains_Area,
                                                                               ReversingAreas_Area,
                                                                               WorkingTimetable_TrainTypeNum,
                                                                               TempDirection,
                                                                               WorkingTimetable_PreferredNumberOfCarriages);
                        IF TempLengthInInches = 0 THEN
                          Log('W W=' + EntryNumStr + 'B:    '
                                 + 'No route area with trains ' + AreaToStr(AreasWithTrains_Area)
                                 + ' to reversing area ' + AreaToStr(ReversingAreas_Area))
                        ELSE BEGIN
                          TempTravelTimeInMinutes := GetJourneyTimeInMinutesFromRouteLength(EntryNumStr + 'B',
                                                                                            AreasWithTrains_Area,
                                                                                            ReversingAreas_Area,
                                                                                            WorkingTimetable_TrainTypeNum,
                                                                                            TempLengthInInches,
                                                                                            TempDirection);
                          IF TempLengthInInches < MinLengthInInches THEN BEGIN
                            MinLengthInInches := TempLengthInInches;
                            AreasWithTrains_NearestReversingArea := AreasWithTrains_Area;
                            AreasWithTrains_ToNearestReversingAreaDirection := TempDirection;
                            AreasWithTrains_TravelTimeToNearestReversingAreaInMinutes := TempTravelTimeInMinutes;
                          END;
                        END;
                      END;
                    END;
                    Log('W W=' + EntryNumStr + 'B: '
                           + 'Area with trains ' + AreaToStr(AreasWithTrains_NearestReversingArea)
                           + ' is the nearest to reversing area ' + AreaToStr(ReversingAreas_Area));
                  END; {WITH}
                END; {FOR}
              END; {WITH}
            END; {FOR}

            IF NOT NearestReversingAreaToAreaWithTrainsFound THEN
                Log('W W=' + EntryNumStr + 'B: '
                       + 'No reversing area near to areas with trains found')
            ELSE BEGIN
              FOR I := 0 TO High(ReversingAreas) DO BEGIN
                Log('W W=' + EntryNumStr + 'B: '
                       + 'Checking access to reversing area ' + AreaToStr(ReversingAreas[I].ReversingAreas_Area) + ' for areas with trains:');
                CheckAccessForAreasWithTrains(EntryNumStr + 'B', WorkingTimetableRec, ReversingAreas[I].ReversingAreas_Area, AreasWithTrainsArray, TempDirection,
                                              TempAccessibleAreas);
                IF Length(TempAccessibleAreas) = 0 THEN BEGIN
                  OK := True;
                  AppendIntegerArray2ToIntegerArray1(AllAccessibleAreas, TempAccessibleAreas);
                END;
              END; {FOR}
            END;

            IF OK THEN BEGIN
              Log('W W=' + EntryNumStr + 'B: '
                     + IfThen(Length(AllAccessibleAreas) = 1,
                              'Only accessible area (in two moves) is ',
                              'Accessible areas (in two moves) are ')
                     + AreaArrayToStr(AllAccessibleAreas));
              I := 0;
              WHILE I <= High(AreasWithTrainsArray) DO BEGIN
                WITH AreasWithTrainsArray[I] DO BEGIN
                  IF AreasWithTrains_AreaAccessible THEN BEGIN
                    J := 0;
                    WHILE J <= High(AreasWithTrains_SuitableTrains) DO BEGIN
                      SetLength(SuitableAdditionalTrains, Length(SuitableAdditionalTrains) + 1);
                      WITH SuitableAdditionalTrains[High(SuitableAdditionalTrains)] DO BEGIN
                        SuitableAdditionalTrains_LocoChip := AreasWithTrains_SuitableTrains[J];
                        SuitableAdditionalTrains_StartArea := AreasWithTrains_Area;
                        SuitableAdditionalTrains_EndArea1 := AreasWithTrains_NearestReversingArea;
                        SuitableAdditionalTrains_EndArea2 := WorkingTimetable_FirstStationArea;
                        SuitableAdditionalTrains_EndArea3 := UnknownArea;

                        SuitableAdditionalTrains_Direction1 := AreasWithTrains_ToNearestReversingAreaDirection;
                        SuitableAdditionalTrains_Direction2 := NearestReversingAreaToFirstStationDirection;
                        SuitableAdditionalTrains_Direction3 := UnknownDirection;

                        SuitableAdditionalTrains_TravelTimeInMinutes1 := AreasWithTrains_TravelTimeToNearestReversingAreaInMinutes;
                        SuitableAdditionalTrains_TravelTimeInMinutes2 := NearestReversingAreaToFirstStationTravelTimeInMinutes;
                        SuitableAdditionalTrains_TravelTimeInMinutes3 := 0;

                        SuitableAdditionalTrains_DepartureTime1 := IncMinute(WorkingTimetable_FirstStationDepartureTime,
                                                                             -StationOppositeDirectionExitMinimumWaitTimeInMinutes
                                                                             -NearestReversingAreaToFirstStationTravelTimeInMinutes
                                                                             -StationOppositeDirectionExitMinimumWaitTimeInMinutes
                                                                             -AreasWithTrains_TravelTimeToNearestReversingAreaInMinutes);
                        SuitableAdditionalTrains_DepartureTime2 := IncMinute(WorkingTimetable_FirstStationDepartureTime,
                                                                             -StationOppositeDirectionExitMinimumWaitTimeInMinutes
                                                                             -NearestReversingAreaToFirstStationTravelTimeInMinutes);
                        SuitableAdditionalTrains_DepartureTime3 := 0;

                        SuitableAdditionalTrains_Weighting := 0;
                        SuitableAdditionalTrains_WeightingStr:= '';

                        T := GetTrainRecord(AreasWithTrains_SuitableTrains[J]);
                        IF T^.Train_FixedLengthInInches <> 0 THEN
                          SuitableAdditionalTrains_LengthInInches := T^.Train_FixedLengthInInches
                        ELSE
                          SuitableAdditionalTrains_LengthInInches := T^.Train_LastLengthInInches;
                        SuitableAdditionalTrains_NumberOfCarriages := T^.Train_NumberOfCarriages;
                        SuitableAdditionalTrains_InUse := (T^.Train_WorkingTimetableLastArrivalTime <> 0);

                        Log('W W=' + EntryNumStr + 'B:    '
                               + 'adding ' + LocoChipToStr(SuitableAdditionalTrains[J].SuitableAdditionalTrains_LocoChip) + ' to list of suitable additional trains: '
                               + AreaToStr(SuitableAdditionalTrains_StartArea)
                               + ' ' + DirectionToStr(SuitableAdditionalTrains_Direction1)
                               + ' to ' + AreaToStr(SuitableAdditionalTrains_EndArea1)
                               + ' at ' + TimeToHMStr(SuitableAdditionalTrains_DepartureTime1)
                               + ' (' + IntToStr(SuitableAdditionalTrains_TravelTimeInMinutes1) + ' mins)'
                               + ' ' + DirectionToStr(SuitableAdditionalTrains_Direction2)
                               + ' to ' + AreaToStr(SuitableAdditionalTrains_EndArea2)
                               + ' at ' + TimeToHMStr(SuitableAdditionalTrains_DepartureTime2)
                               + ' (' + IntToStr(SuitableAdditionalTrains_TravelTimeInMinutes2) + ' mins)');
                      END; {WITH}
                      Inc(J);
                    END {WHILE}
                  END;
                END; {WITH}

                Inc(I);
              END; {WHILE}
            END ELSE BEGIN
              Log('W W=' + EntryNumStr + 'B: '
                     + 'No accessible AREAS' + ' {LINE=AFTER}');

              { No accessible areas in two moves, so try three moves }
              Log('W W=' + EntryNumStr + 'C: '
                   + 'NEW WORKING TIMETABLE ENTRY:'
                   + ' checking possible additional journey to ' + UpperCase(AreaToStr(WorkingTimetable_FirstStationArea)) + ' by means of making three movements');

              { Reset some of the AreasWithTrainsArray variables possibly already initialised }
              FOR I := 0 TO High(AreasWithTrainsArray) DO BEGIN
                WITH AreasWithTrainsArray[I] DO BEGIN
                  AreasWithTrains_AreaAccessible := True;
                  AreasWithTrains_ToNearestReversingAreaDirection := UnknownDirection;
                  AreasWithTrains_RequiredTime := 0;
                  AreasWithTrains_TravelTimeToEndAreaInMinutes := 0;
                END; {WITH}
              END; {FOR}

              Log('W W=' + EntryNumStr + 'C: '
                     + 'See which reversing areas are the nearest to the various areas with trains:');
              { See which reversing areas are the nearest to the various areas with trains }
              NearestReversingAreaToAreaWithTrainsFound := False;
              FOR I := 0 TO High(AreasWithTrainsArray) DO BEGIN
                MinLengthInInches := 999.0;
                WITH AreasWithTrainsArray[I] DO BEGIN
                  Log('W W=' + EntryNumStr + 'C:    '
                         + 'Area with trains ' + AreaToStr(AreasWithTrains_Area) + ':');
                  FOR J := 0 TO High(ReversingAreas) DO BEGIN
                    WITH ReversingAreas[J] DO BEGIN
                      Log('W W=' + EntryNumStr + 'C:      '
                             + 'Checking reversing area ' + AreaToStr(ReversingAreas_Area) + ':');

                      { Exclude a reversing area in certain circumstances }
                      IF ReversingAreas_Area = WorkingTimetable_FirstStationArea THEN
                        Log('W W=' + EntryNumStr + 'C:        '
                               + 'excluding reversing area ' + AreaToStr(ReversingAreas_Area) + ' as it is also the first station area')
                      ELSE BEGIN
                        { see which are accessible }
                        TempDirection := GetRouteDirection(AreasWithTrainsArray[I].AreasWithTrains_Area, ReversingAreas_Area);
                        IF TempDirection = UnknownDirection THEN
                          Log('W W=' + EntryNumStr + 'C:        '
                                 + 'excluding reversing area ' + AreaToStr(ReversingAreas[J].ReversingAreas_Area)
                                 + ': no route from area with trains ' + AreaToStr(AreasWithTrains_Area))
                        ELSE BEGIN
                          { and which is the nearest }
                          TempLengthInInches := GetAreaToAreaRouteLengthInInches(EntryNumStr + 'B',
                                                                                 AreasWithTrains_Area,
                                                                                 ReversingAreas_Area,
                                                                                 WorkingTimetable_TrainTypeNum,
                                                                                 TempDirection,
                                                                                 WorkingTimetable_PreferredNumberOfCarriages);
                          Log('W W=' + EntryNumStr + 'C:        '
                                 + 'distance to reversing area ' + AreaToStr(ReversingAreas_Area) + ' is ' + FloatToStr(TempLengthInInches) + ' inches');

                          TempTravelTimeInMinutes := GetJourneyTimeInMinutesFromRouteLength(EntryNumStr + 'C',
                                                                                          AreasWithTrains_Area,
                                                                                          ReversingAreas_Area,
                                                                                          WorkingTimetable_TrainTypeNum,
                                                                                          WorkingTimetable_PreferredNumberOfCarriages,
                                                                                          TempDirection);
                          IF TempLengthInInches < MinLengthInInches THEN BEGIN
                            MinLengthInInches := TempLengthInInches;
                            AreasWithTrains_NearestReversingArea := ReversingAreas_Area;
                            AreasWithTrains_ToNearestReversingAreaDirection := TempDirection;
                            AreasWithTrains_TravelTimeToNearestReversingAreaInMinutes := TempTravelTimeInMinutes;

                            Log('W W=' + EntryNumStr + 'C:        '
                                   + 'checking whether there''s a route between given reversing area ' + AreaToStr(ReversingAreas_Area)
                                   + ' and the nearest reversing area to the first station ' + AreaToStr(NearestReversingAreaToFirstStationArea));
                            TempDirection := GetRouteDirection(ReversingAreas_Area, NearestReversingAreaToFirstStationArea);
                            IF TempDirection = UnknownDirection THEN
                              Log('W W=' + EntryNumStr + 'C:        '
                                     + 'excluding reversing area ' + AreaToStr(ReversingAreas_Area)
                                     + ': no route to nearest reversing area to first station' + AreaToStr(NearestReversingAreaToFirstStationArea))
                            ELSE BEGIN
                              AreasWithTrains_FirstReversingAreaToFinalReversingAreaDirection := TempDirection;

                              { and which is the nearest }
                              TempLengthInInches := GetAreaToAreaRouteLengthInInches(EntryNumStr + 'B',
                                                                                     ReversingAreas_Area,
                                                                                     NearestReversingAreaToFirstStationArea,
                                                                                     WorkingTimetable_TrainTypeNum,
                                                                                     TempDirection,
                                                                                     WorkingTimetable_PreferredNumberOfCarriages);
                              Log('W W=' + EntryNumStr + 'C:        '
                                     + 'distance from reversing area ' + AreaToStr(ReversingAreas_Area)
                                     + ' to the nearest reversing area to the first station ' + AreaToStr(NearestReversingAreaToFirstStationArea)
                                     + ' is ' + FloatToStr(TempLengthInInches) + ' inches');

                              TempTravelTimeInMinutes := GetJourneyTimeInMinutesFromRouteLength(EntryNumStr + 'B',
                                                                                                ReversingAreas_Area,
                                                                                                NearestReversingAreaToFirstStationArea,
                                                                                                WorkingTimetable_TrainTypeNum,
                                                                                                TempLengthInInches,
                                                                                                TempDirection);

                              AreasWithTrains_TravelTimeFromFirstReversingAreaToFinalReversingAreaInMinutes := TempTravelTimeInMinutes;
                            END;
                          END;
                          NearestReversingAreaToAreaWithTrainsFound := True;
                        END;
                      END;
                    END; {WITH}
                  END; {FOR}

                  IF NearestReversingAreaToAreaWithTrainsFound THEN
                    Log('W W=' + EntryNumStr + 'C:      '
                           + 'Nearest reversing area is ' + AreaToStr(AreasWithTrainsArray[I].AreasWithTrains_NearestReversingArea)
                           + '; area with trains direction to it is ' + DirectionTostr(AreasWithTrainsArray[I].AreasWithTrains_ToNearestReversingAreaDirection));
                END; {WITH}
              END; {FOR}

              IF NOT NearestReversingAreaToAreaWithTrainsFound THEN
                  Log('W W=' + EntryNumStr + 'C: '
                         + 'No reversing area near to areas with trains found')
              ELSE BEGIN
                FOR I := 0 TO High(ReversingAreas) DO BEGIN
                  Log('W W=' + EntryNumStr + 'C: '
                         + 'Checking access to reversing area ' + AreaToStr(ReversingAreas[I].ReversingAreas_Area)
                         + ' for areas with trains:');
                  CheckAccessForAreasWithTrains(EntryNumStr + 'C', WorkingTimetableRec, ReversingAreas[I].ReversingAreas_Area, AreasWithTrainsArray, TempDirection,
                                                TempAccessibleAreas);
                  IF Length(TempAccessibleAreas) = 0 THEN BEGIN
                    OK := True;
                    AppendIntegerArray2ToIntegerArray1(AllAccessibleAreas, TempAccessibleAreas);
                  END;
                END; {FOR}
              END;

              IF OK THEN BEGIN
                Log('W W=' + EntryNumStr + 'C: '
                       + IfThen(Length(AllAccessibleAreas) = 1,
                                'Only accessible area (in two moves) is ',
                                'Accessible areas (in two moves) are ')
                       + AreaArrayToStr(AllAccessibleAreas));
                I := 0;
                WHILE I <= High(AreasWithTrainsArray) DO BEGIN
                  WITH AreasWithTrainsArray[I] DO BEGIN
                    IF AreasWithTrains_AreaAccessible THEN BEGIN
                      J := 0;
                      WHILE J <= High(AreasWithTrains_SuitableTrains) DO BEGIN
                        SetLength(SuitableAdditionalTrains, Length(SuitableAdditionalTrains) + 1);
                        WITH SuitableAdditionalTrains[High(SuitableAdditionalTrains)] DO BEGIN
                          SuitableAdditionalTrains_LocoChip := AreasWithTrains_SuitableTrains[J];
                          SuitableAdditionalTrains_StartArea := AreasWithTrains_Area;
                          SuitableAdditionalTrains_EndArea1 := AreasWithTrains_NearestReversingArea;
                          SuitableAdditionalTrains_EndArea2 := NearestReversingAreaToFirstStationArea;
                          SuitableAdditionalTrains_EndArea3 := WorkingTimetable_FirstStationArea;

                          SuitableAdditionalTrains_Direction1 := AreasWithTrains_ToNearestReversingAreaDirection;
                          SuitableAdditionalTrains_Direction2 := AreasWithTrains_FirstReversingAreaToFinalReversingAreaDirection;
                          SuitableAdditionalTrains_Direction3 := NearestReversingAreaToFirstStationDirection;

                          SuitableAdditionalTrains_TravelTimeInMinutes1 := AreasWithTrains_TravelTimeToNearestReversingAreaInMinutes;
                          SuitableAdditionalTrains_TravelTimeInMinutes2 := AreasWithTrains_TravelTimeFromFirstReversingAreaToFinalReversingAreaInMinutes;
                          SuitableAdditionalTrains_TravelTimeInMinutes3 := NearestReversingAreaToFirstStationTravelTimeInMinutes;

                          SuitableAdditionalTrains_DepartureTime1 := IncMinute(WorkingTimetable_FirstStationDepartureTime,
                                                                               -StationOppositeDirectionExitMinimumWaitTimeInMinutes
                                                                               -SuitableAdditionalTrains_TravelTimeInMinutes3
                                                                               -StationOppositeDirectionExitMinimumWaitTimeInMinutes
                                                                               -SuitableAdditionalTrains_TravelTimeInMinutes2
                                                                               -StationOppositeDirectionExitMinimumWaitTimeInMinutes
                                                                               -SuitableAdditionalTrains_TravelTimeInMinutes1);
                          SuitableAdditionalTrains_DepartureTime2 := IncMinute(WorkingTimetable_FirstStationDepartureTime,
                                                                               -StationOppositeDirectionExitMinimumWaitTimeInMinutes
                                                                               -SuitableAdditionalTrains_TravelTimeInMinutes3
                                                                               -StationOppositeDirectionExitMinimumWaitTimeInMinutes
                                                                               -SuitableAdditionalTrains_TravelTimeInMinutes2);
                          SuitableAdditionalTrains_DepartureTime3 := IncMinute(WorkingTimetable_FirstStationDepartureTime,
                                                                               -StationOppositeDirectionExitMinimumWaitTimeInMinutes
                                                                               -SuitableAdditionalTrains_TravelTimeInMinutes3);
                          SuitableAdditionalTrains_Weighting := 0;
                          SuitableAdditionalTrains_WeightingStr:= '';

                          T := GetTrainRecord(AreasWithTrains_SuitableTrains[J]);
                          IF T^.Train_FixedLengthInInches <> 0 THEN
                            SuitableAdditionalTrains_LengthInInches := T^.Train_FixedLengthInInches
                          ELSE
                            SuitableAdditionalTrains_LengthInInches := T^.Train_LastLengthInInches;
                          SuitableAdditionalTrains_NumberOfCarriages := T^.Train_NumberOfCarriages;
                          SuitableAdditionalTrains_InUse := (T^.Train_WorkingTimetableLastArrivalTime <> 0);

                          Log('W W=' + EntryNumStr + 'C:    '
                                 + 'adding ' + LocoChipToStr(SuitableAdditionalTrains[J].SuitableAdditionalTrains_LocoChip) + ' to list of suitable additional trains: '
                                 + AreaToStr(SuitableAdditionalTrains_StartArea, ShortStringType)
                                 + ' ' + DirectionToStr(SuitableAdditionalTrains_Direction1)
                                 + ' to ' + AreaToStr(SuitableAdditionalTrains_EndArea1, ShortStringType)
                                 + ' at ' + TimeToHMStr(SuitableAdditionalTrains_DepartureTime1)
                                 + ' (' + IntToStr(SuitableAdditionalTrains_TravelTimeInMinutes1) + ' mins)'
                                 + ' ' + DirectionToStr(SuitableAdditionalTrains_Direction2)
                                 + ' to ' + AreaToStr(SuitableAdditionalTrains_EndArea2, ShortStringType)
                                 + ' at ' + TimeToHMStr(SuitableAdditionalTrains_DepartureTime2)
                                 + ' (' + IntToStr(SuitableAdditionalTrains_TravelTimeInMinutes2) + ' mins)'
                                 + ' ' + DirectionToStr(SuitableAdditionalTrains_Direction3)
                                 + ' to ' + AreaToStr(SuitableAdditionalTrains_EndArea3, ShortStringType)
                                 + ' at ' + TimeToHMStr(SuitableAdditionalTrains_DepartureTime3)
                                 + ' (' + IntToStr(SuitableAdditionalTrains_TravelTimeInMinutes3) + ' mins)');
                        END; {WITH}
                        Inc(J);
                      END {WHILE}
                    END;
                  END; {WITH}

                  Inc(I);
                END; {WHILE}
              END;
            END;
          END;
      END;
    END;
  END; {WITH}
  debug;
END; { FindAdditionalTrains }

PROCEDURE SelectMostSuitableAdditionalTrain(EntryNumStr : String; SuitableAdditionalTrains : SuitableAdditionalTrainsArrayType;
                                            OUT SelectedLocoChip, StartArea, EndArea1, EndArea2, EndArea3 : Integer; OUT Direction1, Direction2, Direction3 : DirectionType;
                                            OUT TravelTimeInMinutes1, TravelTimeInMinutes2, TravelTimeInMinutes3 : Integer;
                                            OUT DepartureTime1, DepartureTime2, DepartureTime3 : TDateTime; PreferredNumberOfCarriages : Integer);
{ Choose the most suitable additional train if there's more than one, or the only one there is }
TYPE
  SuitableTrainsSortType = (ByNumberOfCarriages, ByTrainLength, ByNearestTrain);

    PROCEDURE SortTrainsInSuitableTrainsArray(EntryNumStr : String; SortType : SuitableTrainsSortType; VAR SuitableTrainsArray : SuitableAdditionalTrainsArrayType);
    { Sort an array of train locochip numbers by train length }
    VAR
      DebugStr : String;
      I, J, K : Integer;
      MaxTrainLength : Real;
      MaxTravelTime : Integer;
      SortedSuitableTrainsArray : SuitableAdditionalTrainsArrayType;
      TempPos : Integer;
      UnsortedSuitableTrainsArray : SuitableAdditionalTrainsArrayType;

    BEGIN
      TRY
        IF Length(SuitableTrainsArray) = 1 THEN
          { there's no point in sorting a list of one }
          Exit
        ELSE BEGIN
          { Copy the old array across or can't alter the length of it }
          SetLength(UnsortedSuitableTrainsArray, Length(SuitableTrainsArray));
          FOR I := 0 TO High(SuitableTrainsArray) DO
            UnsortedSuitableTrainsArray[I] := SuitableTrainsArray[I];
          SetLength(SortedSuitableTrainsArray, 0);

          { Now do the sort }
          TempPos := 0;
          FOR I := 0 TO High(UnsortedSuitableTrainsArray) DO BEGIN
            CASE SortType OF
              ByTrainLength:
                BEGIN
                  TempPos := -1;
                  MaxTrainLength := 999;
                  FOR J := 0 TO High(UnsortedSuitableTrainsArray) DO BEGIN
                    IF UnsortedSuitableTrainsArray[J].SuitableAdditionalTrains_LengthInInches < MaxTrainLength THEN BEGIN
                      MaxTrainLength := UnsortedSuitableTrainsArray[J].SuitableAdditionalTrains_LengthInInches;
                      TempPos := J;
                    END;
                  END; {FOR}
                END;
              ByNumberOfCarriages:
                BEGIN
                  TempPos := -1;
                  MaxTrainLength := 999;
                  FOR J := 0 TO High(UnsortedSuitableTrainsArray) DO BEGIN
                    IF UnsortedSuitableTrainsArray[J].SuitableAdditionalTrains_NumberOfCarriages < MaxTrainLength THEN BEGIN
                      MaxTrainLength := UnsortedSuitableTrainsArray[J].SuitableAdditionalTrains_NumberOfCarriages;
                      TempPos := J;
                    END;
                  END; {FOR}
                END;
              ByNearestTrain:
                BEGIN
                  TempPos := -1;
                  MaxTravelTime := 0;
                  FOR J := 0 TO High(UnsortedSuitableTrainsArray) DO BEGIN
                    IF (UnsortedSuitableTrainsArray[J].SuitableAdditionalTrains_TravelTimeInMinutes1
                                                                            + UnsortedSuitableTrainsArray[J].SuitableAdditionalTrains_TravelTimeInMinutes2 >= MaxTravelTime)
                    THEN BEGIN
                      MaxTravelTime := UnsortedSuitableTrainsArray[J].SuitableAdditionalTrains_TravelTimeInMinutes1
                                                                                             + UnsortedSuitableTrainsArray[J].SuitableAdditionalTrains_TravelTimeInMinutes2;
                      TempPos := J;
                    END;
                  END; {FOR}
                END;
            END; {CASE}

            SetLength(SortedSuitableTrainsArray, Length(SortedSuitableTrainsArray) + 1);
            SortedSuitableTrainsArray[High(SortedSuitableTrainsArray)] := UnsortedSuitableTrainsArray[TempPos];

            { and remove the element from the original array }
            FOR K := TempPos TO (Length(UnsortedSuitableTrainsArray) - 2) DO
              UnsortedSuitableTrainsArray[K] := UnsortedSuitableTrainsArray[K + 1];
            SetLength(UnsortedSuitableTrainsArray, Length(UnsortedSuitableTrainsArray) -1);
          END; {FOR}

          { and copy the sorted data back }
          SetLength(SuitableTrainsArray, Length(SortedSuitableTrainsArray));
          DebugStr := '';
          FOR I := 0 TO High(SortedSuitableTrainsArray) DO BEGIN
            SuitableTrainsArray[I] := SortedSuitableTrainsArray[I];
            DebugStr := DebugStr + ' ' + LocoChipToStr(SuitableTrainsArray[I].SuitableAdditionalTrains_LocoChip);
          END; {FOR}

          IF Length(SortedSuitableTrainsArray) > 1 THEN BEGIN
            CASE SortType OF
              ByTrainLength:
                Log('W W=' + EntryNumStr + ':       '
                       + 'trains sorted by train length=' + DebugStr);
              ByNearestTrain:
                Log('W W=' + EntryNumStr + ':       '
                       + 'trains sorted by nearest train=' + DebugStr);
              ByNumberOfCarriages:
                Log('W W=' + EntryNumStr + ':       '
                       + 'trains sorted by number of carriages=' + DebugStr);
            END; {CASE}
          END;
        END;
      EXCEPT
        ON E : Exception DO
          Log('EG SortTrainsInSuitableTrainsArrayByLength: ' + E.ClassName +' error raised, with message: '+ E.Message);
      END; {TRY}
    END; { SortTrainsInSuitableTrainsArrayByLength }

VAR
  DebugStr : String;
  I : Integer;
  MaxWeighting : Integer;
  TempWeighting : Integer;

BEGIN { SelectMostSuitableAdditionalTrain }
  Log('W W=' + EntryNumStr + ': '
         + 'Selecting most suitable train');

  StartArea := UnknownArea;
  IF Length(SuitableAdditionalTrains) = 1 THEN BEGIN
    { there's only one candidate so select it }
    WITH SuitableAdditionalTrains[0] DO BEGIN
      SelectedLocoChip := SuitableAdditionalTrains_LocoChip;
      StartArea := SuitableAdditionalTrains_StartArea;
      EndArea1 := SuitableAdditionalTrains_EndArea1;
      EndArea2 := SuitableAdditionalTrains_EndArea2;
      EndArea3 := SuitableAdditionalTrains_EndArea3;
      Direction1 := SuitableAdditionalTrains_Direction1;
      Direction2 := SuitableAdditionalTrains_Direction2;
      Direction3 := SuitableAdditionalTrains_Direction3;
      DepartureTime1 := SuitableAdditionalTrains_DepartureTime1;
      DepartureTime2 := SuitableAdditionalTrains_DepartureTime2;
      DepartureTime3 := SuitableAdditionalTrains_DepartureTime3;
      TravelTimeInMinutes1 := SuitableAdditionalTrains_TravelTimeInMinutes1;
      TravelTimeInMinutes2 := SuitableAdditionalTrains_TravelTimeInMinutes2;
      TravelTimeInMinutes3 := SuitableAdditionalTrains_TravelTimeInMinutes3;
    END; {WITH}
    Log('W W=' + EntryNumStr + ':    '
           + 'the only suitable train found is ' + LocoChipToStr(SelectedLocoChip) + ' so it is selected');
  END ELSE BEGIN
    { otherwise choose the one nearest that matches the length preference and is already in use - we have to use a weighting system to make the selection }
    DebugStr := '';
    I := 0;
    WHILE I <= High(SuitableAdditionalTrains) DO BEGIN
      DebugStr := DebugStr + ' ' + LocoChipToStr(SuitableAdditionalTrains[I].SuitableAdditionalTrains_LocoChip);
      Inc(I);
    END; {WHILE}
    Log('W W=' + EntryNumStr + ':    '
           + 'suitable trains found are ' + DebugStr);

    { Sort by train length... }
    SortTrainsInSuitableTrainsArray(EntryNumStr, ByTrainLength, SuitableAdditionalTrains);

    I := 0;
    WHILE I <= High(SuitableAdditionalTrains) DO BEGIN
      WITH SuitableAdditionalTrains[I] DO BEGIN
        IF SuitableAdditionalTrains_NumberOfCarriages = PreferredNumberOfCarriages THEN BEGIN
          SuitableAdditionalTrains_Weighting := 20;
          SuitableAdditionalTrains_WeightingStr := 'Length=10';
        END ELSE BEGIN
          { add weighting based on how near the train is to the preferred length }
          IF (SuitableAdditionalTrains_NumberOfCarriages = (PreferredNumberOfCarriages - 1))
          OR (SuitableAdditionalTrains_NumberOfCarriages = (PreferredNumberOfCarriages + 1))
          THEN BEGIN
            SuitableAdditionalTrains_Weighting := 10;
            SuitableAdditionalTrains_WeightingStr := 'Length=5';
          END ELSE BEGIN
            SuitableAdditionalTrains_Weighting := 0;
            SuitableAdditionalTrains_WeightingStr := '';
          END;
        END;
      END; {WITH}
      Inc(I);
    END; {WHILE}

    { ...then by proximity to the first station area }
    SortTrainsInSuitableTrainsArray(EntryNumStr, ByNearestTrain, SuitableAdditionalTrains);
    WITH SuitableAdditionalTrains[0] DO
      SuitableAdditionalTrains_WeightingStr := SuitableAdditionalTrains_WeightingStr + ' Nearest=0';

    I := 1;
    TempWeighting := 0;
    WHILE I <= High(SuitableAdditionalTrains) DO BEGIN
      WITH SuitableAdditionalTrains[I] DO BEGIN
        IF (SuitableAdditionalTrains_TravelTimeInMinutes1 + SuitableAdditionalTrains_TravelTimeInMinutes2 <
                                                                                          (SuitableAdditionalTrains[I - 1].SuitableAdditionalTrains_TravelTimeInMinutes1
                                                                                           + SuitableAdditionalTrains[I - 1].SuitableAdditionalTrains_TravelTimeInMinutes2))
        THEN
          TempWeighting := TempWeighting + 10;
        SuitableAdditionalTrains_Weighting := SuitableAdditionalTrains_Weighting + TempWeighting;
        SuitableAdditionalTrains_WeightingStr := SuitableAdditionalTrains_WeightingStr + ' Nearest=' + IntToStr(TempWeighting);
      END; {WITH}
      Inc(I);
    END; {WHILE}

    { ...then give extra weighting to trains that have previously been in use }
    I := 0;
    WHILE I <= High(SuitableAdditionalTrains) DO BEGIN
      WITH SuitableAdditionalTrains[I] DO BEGIN
        IF NOT SuitableAdditionalTrains_InUse THEN
          SuitableAdditionalTrains_WeightingStr := SuitableAdditionalTrains_WeightingStr + ' In Use=0'
        ELSE BEGIN
          SuitableAdditionalTrains_Weighting := SuitableAdditionalTrains_Weighting + 10;
          SuitableAdditionalTrains_WeightingStr := SuitableAdditionalTrains_WeightingStr + ' In Use=10';
        END;
      END; {WITH}
      Inc(I);
    END; {WHILE}

    I := 0;
    WHILE I <= High(SuitableAdditionalTrains) DO BEGIN
      WITH SuitableAdditionalTrains[I] DO
        Log('W W=' + EntryNumStr + ':    '
               + LocoChipToStr(SuitableAdditionalTrains_LocoChip) + ': weighting=' + IntToStr(SuitableAdditionalTrains_Weighting)
               + ' (' + SuitableAdditionalTrains_WeightingStr + ')');
      Inc(I);
    END; {WHILE}

    { Choose the last train with highest weighting }
    I := 0;
    MaxWeighting := 0;
    SelectedLocoChip := SuitableAdditionalTrains[0]. SuitableAdditionalTrains_LocoChip;
    WHILE I <= High(SuitableAdditionalTrains) DO BEGIN
      WITH SuitableAdditionalTrains[I] DO
        IF SuitableAdditionalTrains_Weighting >= MaxWeighting THEN BEGIN
          SelectedLocoChip := SuitableAdditionalTrains_LocoChip;
          MaxWeighting := SuitableAdditionalTrains_Weighting;

          StartArea := SuitableAdditionalTrains_StartArea;
          EndArea1 := SuitableAdditionalTrains_EndArea1;
          EndArea2 := SuitableAdditionalTrains_EndArea2;
          EndArea3 := SuitableAdditionalTrains_EndArea3;
          Direction1 := SuitableAdditionalTrains_Direction1;
          Direction2 := SuitableAdditionalTrains_Direction2;
          Direction3 := SuitableAdditionalTrains_Direction3;
          DepartureTime1 := SuitableAdditionalTrains_DepartureTime1;
          DepartureTime2 := SuitableAdditionalTrains_DepartureTime2;
          DepartureTime3 := SuitableAdditionalTrains_DepartureTime3;
          TravelTimeInMinutes1 := SuitableAdditionalTrains_TravelTimeInMinutes1;
          TravelTimeInMinutes2 := SuitableAdditionalTrains_TravelTimeInMinutes2;
          TravelTimeInMinutes3 := SuitableAdditionalTrains_TravelTimeInMinutes3;
        END;
      Inc(I);
    END; {WHILE}

    Log('W W=' + EntryNumStr + ':    '
           + 'the most suitable additional train is ' + LocoChipToStr(SelectedLocoChip) + ' so it is selected');
  END;

  Log('W W=' + EntryNumStr + ':    '
         + LocoChipToStr(SelectedLocoChip)
         + ': start=' + AreaToStr(StartArea)
         + ' end1=' + AreaToStr(EndArea1)
         + ' end2=' + AreaToStr(EndArea2)
         + ' end3=' + AreaToStr(EndArea3)
         + ' time1=' + IntToStr(TravelTimeInMinutes1)
         + ' time2=' + IntToStr(TravelTimeInMinutes2)
         + ' time3=' + IntToStr(TravelTimeInMinutes3)
         + ' dir1=' + DirectionToStr(Direction1)
         + ' dir2=' + DirectionToStr(Direction2)
         + ' dir3=' + DirectionToStr(Direction3)
         + ' dep1=' + TimeToHMStr(DepartureTime1)
         + ' dep2=' + TimeToHMStr(DepartureTime2)
         + ' dep3=' + TimeToHMStr(DepartureTime3));

END; { SelectMostSuitableAdditionalTrain }

PROCEDURE SetUpTrainDiagramsRecord(WorkingTimetableRec : WorkingTimetableRecType;OUT OK : Boolean);
{ Now create the diagrams record }
VAR
  DirectionsArray : DirectionArrayType;
  DoubleHeaderLocoChip : Integer;
  EndLocationsStrArray : StringArrayType;
  I : Integer;
  JourneyCount : Integer;
  LengthOfTrainInCarriages : Integer;
  LightsOnTime : TDateTime;
  LightsRemainOn : Boolean;
  NotForPublicUseArray : BooleanArrayType;
  StartLocationStr : String;
  StartOfRepeatJourney : Boolean;
  StoppingArray : BooleanArrayType;
   T : Train;
  TrainNonMoving : Boolean;
  TypeOfTrainNum : Integer;
  UserDriving : Boolean;
  UserRequiresInstructions : Boolean;
  UserSpecifiedDepartureTimesArray : DateTimeArrayType;

BEGIN
  WITH WorkingTimetableRec DO BEGIN
    OK := True;

    IF WorkingTimetable_Status = EntryCancelled THEN BEGIN
      OK := False;
      Log('W W=' + WorkingTimetable_EntryNumStr + ': ENTRY CANCELLED - DIAGRAM NOT CREATED');
    END;

    IF OK
    AND (WorkingTimetable_LocoChip = UnknownLocoChip)
    THEN BEGIN
      OK := False;
      Log('W W=' + WorkingTimetable_EntryNumStr + ': NO LOCOCHIP SELECTED - DIAGRAM NOT CREATED');
      WorkingTimetable_Status := EntryCancelled;
    END;

    IF OK THEN BEGIN
      T := GetTrainRecord(WorkingTimetable_LocoChip);
      IF T = NIL THEN BEGIN
        OK := False;
        Log('W W=' + WorkingTimetable_EntryNumStr + ': NO TRAIN RECORD FOR LOCO ' + LocoChipToStr(WorkingTimetable_LocoChip) + ' - DIAGRAM NOT CREATED');
      END ELSE BEGIN
        { Create the diagram }
        Log('W W=' + WorkingTimetable_EntryNumStr + ': CREATING DIAGRAM ENTRY for ' + LocoChipToStr(WorkingTimetable_LocoChip));

        DoubleHeaderLocoChip := UnknownLocoChip;
        JourneyCount := 0;
        LightsOnTime := 0;
        LightsRemainOn := True;
        TrainNonMoving := False;

        { If the first move is to get the train in place, mark it accordingly }
        SetLength(NotForPublicUseArray, 0);
        IF WorkingTimetable_Status = AdditionalEntryCreated THEN
          AppendToBooleanArray(NotForPublicUseArray, True)
        ELSE
          AppendToBooleanArray(NotForPublicUseArray, False);

        LengthOfTrainInCarriages := T^.Train_CurrentLengthInInches DIV 12;
        StartLocationStr := AreaToStr(WorkingTimetable_FirstStationArea);
        TypeOfTrainNum := WorkingTimetable_TrainTypeNum;
        UserDriving := False;
        UserRequiresInstructions := False;
        StartOfRepeatJourney := False;

        SetLength(DirectionsArray, 0);
        AppendToDirectionArray(DirectionsArray, WorkingTimetable_Direction);
        SetLength(StoppingArray, 0);
        AppendToBooleanArray(StoppingArray, True);
        SetLength(UserSpecifiedDepartureTimesArray, 0);
        AppendToDateTimeArray(UserSpecifiedDepartureTimesArray, WorkingTimetable_FirstStationDepartureTime);

        SetLength(EndLocationsStrArray, 0);
        IF Length(WorkingTimetable_StoppingAtStationAreas) > 0 THEN BEGIN
          FOR I := 0 TO High(WorkingTimetable_StoppingAtStationAreas) DO BEGIN
            AppendToStringArray(EndLocationsStrArray, AreaToStr(WorkingTimetable_StoppingAtStationAreas[I]));
            AppendToBooleanArray(StoppingArray, True);
            AppendToDirectionArray(DirectionsArray, WorkingTimetable_Direction);
            AppendToBooleanArray(NotForPublicUseArray, False);
            AppendToDateTimeArray(UserSpecifiedDepartureTimesArray, 0);
            Inc(JourneyCount);
          END;
        END;
        AppendToStringArray(EndLocationsStrArray, AreaToStr(WorkingTimetable_LastStationArea));

        CreateTrainDiagramsRecord(WorkingTimetable_LocoChip, DoubleHeaderLocoChip, JourneyCount, UserSpecifiedDepartureTimesArray, LightsOnTime, EndLocationsStrArray,
                                  DirectionsArray, LightsRemainOn, TrainNonMoving, NotForPublicUseArray, StartLocationStr, StoppingArray, LengthOfTrainInCarriages,
                                  TypeOfTrainNum, UserDriving, UserRequiresInstructions, StartOfRepeatJourney);

        Log('W W=' + WorkingTimetable_EntryNumStr + ': ' +  LocoChipToStr(WorkingTimetable_LocoChip)
               + ' ' + StartLocationStr + ' to ' + EndLocationsStrArray[High(EndLocationsStrArray)]
               + ' at ' + TimeToHMSStr(UserSpecifiedDepartureTimesArray[0])
               + ' arriving at ' + TimeToHMSStr(IncMinute(UserSpecifiedDepartureTimesArray[0], WorkingTimetable_TravelTimeInMinutes))
               + IfThen(NotForPublicUseArray[0],
                        ' [Not for Public Use]',
                        '')
               + ' created');
      END;
    END;

    IF NOT OK THEN BEGIN
      WorkingTimetable_Status := EntryCancelled;
      Log('W W=' + WorkingTimetable_EntryNumStr + ': '
             + 'Problem with creating entry - entry cancelled');
    END;
  END; {WITH}
END; { SetUpTrainDiagramsRecord }

PROCEDURE ProcessWorkingTimetable;
{ Convert the working timetable to train diagrams by locating and allocating trains }
VAR
  DebugStr : String;
  I : Integer;
  OK : Boolean;
  T : Train;
  WorkingTimetableCount : Integer;

BEGIN { ProcessWorkingTimetable }
  TRY
    { First see what trains are available }

    { and when they are available }

    { and where they are currently }



    WorkingTimetableCount := 0;
    WHILE WorkingTimetableCount <= High(WorkingTimetableRecArray) DO BEGIN
      DrawLineInLogFile(NoLocoChip, 'W', '~~', UnitRef);

      WITH WorkingTimetableRecArray[WorkingTimetableCount] DO BEGIN
      END; {WITH}
      Inc(WorkingTimetableCount);
    END; {WHILE}

    { Log the Working Timetable entries }
    WorkingTimetableCount := 0;
    WHILE WorkingTimetableCount <= High(WorkingTimetableRecArray) DO BEGIN
      WITH WorkingTimetableRecArray[WorkingTimetableCount] DO BEGIN
        Log('W W=' + WorkingTimetable_EntryNumStr + ':'
               + ' Setting up train diagrams record:' + ' {LINE=BEFORE}');

        IF WorkingTimetable_Status = IncorrectDayOfTheWeekForEntry THEN
          Log('W W=' + WorkingTimetable_EntryNumStr + ': Incorrect day of the week'
                 + ' (' + DayOfTheWeekToStr(CurrentRailwayDayOfTheWeek) + ') for entry')
        ELSE
          IF WorkingTimetable_Status = EntryCancelled THEN
            Log('W W=' + WorkingTimetable_EntryNumStr + ': Entry cancelled')
          ELSE BEGIN
            T := GetTrainRecord(WorkingTimetable_LocoChip);
            IF T = NIL THEN
              Log('W W=' + WorkingTimetable_EntryNumStr + ': no train record')
            ELSE BEGIN
              Log('W W=' + WorkingTimetable_EntryNumStr + ': LocoChip=' + LocoChipToStr(WorkingTimetable_LocoChip));
              Log('W W=' + WorkingTimetable_EntryNumStr + ': Direction=' + DirectionToStr(WorkingTimetable_Direction));
              Log('W W=' + WorkingTimetable_EntryNumStr + ': FirstStationArea=' + AreaToStr(WorkingTimetable_FirstStationArea));
              Log('W W=' + WorkingTimetable_EntryNumStr + ': FirstStationDepartureTime=' + TimeToHMStr(WorkingTimetable_FirstStationDepartureTime));
              Log('W W=' + WorkingTimetable_EntryNumStr + ': LastStationArea=' + AreaToStr(WorkingTimetable_LastStationArea));

              DebugStr := 'PossibleLocoClasses=';
              FOR I := 0 TO High(WorkingTimetable_PossibleLocoClasses) DO
                DebugStr := DebugStr + WorkingTimetable_PossibleLocoClasses[I];
              Log('W W=' + WorkingTimetable_EntryNumStr + ': ' + DebugStr);
              Log('W W=' + WorkingTimetable_EntryNumStr + ': PreferredNumberOfCarriages=' + IntToStr(WorkingTimetable_PreferredNumberOfCarriages));
              Log('W W=' + WorkingTimetable_EntryNumStr + ': Status=' + WorkingTimetableStatusToStr(WorkingTimetable_Status));
              Log('W W=' + WorkingTimetable_EntryNumStr + ': StoppingAtStationAreas=' + AreaArrayToStr(WorkingTimetable_StoppingAtStationAreas));
              Log('W W=' + WorkingTimetable_EntryNumStr + ': TrainTypeNum=' + IntToStr(WorkingTimetable_TrainTypeNum));
              Log('W W=' + WorkingTimetable_EntryNumStr + ': TravelTimeInMinutes=' + IntToStr(WorkingTimetable_TravelTimeInMinutes));
              Log('W W=' + WorkingTimetable_EntryNumStr + ': T^.Train_WorkingTimetableLastArrivalTime=' + TimeToHMStr(T^.Train_WorkingTimetableLastArrivalTime));
              Log('W W=' + WorkingTimetable_EntryNumStr + ': T^.Train_WorkingTimetableLastArrivalArea=' + AreaToStr(T^.Train_WorkingTimetableLastArrivalArea));
              DrawLineInLogFile(NoLocoChip, 'W', '-', UnitRef);
            END;

          { Now create the diagrams record }
          SetUpTrainDiagramsRecord(WorkingTimetableRecArray[WorkingTimetableCount], OK);
        END;
      END; {WITH}

      Inc(WorkingTimetableCount);
    END; {WHILE}
  EXCEPT
    ON E : Exception DO
      Log('EG ProcessWorkingTimetable: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ProcessWorkingTimetable }

INITIALIZATION

END { NewWorkingTimetable }.