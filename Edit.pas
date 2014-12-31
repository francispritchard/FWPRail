UNIT Edit;
{ Unit that allows the user to construct a layout in diagrammatic form on the screen

  Copyright © F.W. Pritchard 2009-2014. All Rights Reserved.

  v0.1  15/07/09 Unit created
  v0.2  10/03/13 writing signal editing code
  v0.3  26/03/13 writing point editing code
  v0.4  30/09/14 implemented new database renumbering method proposed by DJW
}
INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Options, ExtCtrls, Grids, ValEdit, InitVars, MiscUtils, Movement, StdCtrls, RailDraw,
  Vcl.Menus;

TYPE
  TEditWindow = CLASS(TForm)
    EditValueListEditor : TValueListEditor;
    EditWindowButtonPanel: TPanel;
    EditWindowLabel : TLabel;
    EditWindowPopupMenu : TPopupMenu;
    ExitWithoutSavingButton: TButton;
    PopupEditWindowResetSizeAndPosition : TMenuItem;
    SaveChangesAndExitButton: TButton;
    UndoChangesButton: TButton;
    PROCEDURE EditValueListEditorEditButtonClick(Sender : TObject);
    PROCEDURE EditValueListEditorExit(Sender : TObject);
    PROCEDURE EditValueListEditorStringsChange(Sender : TObject);
    PROCEDURE EditValueListEditorValidate(Sender: TObject; ACol, ARow : Integer; CONST KeyName, KeyValue : String);
    PROCEDURE EditWindowPopupMenuPopup(Sender: TObject);
    PROCEDURE EditWindowPopupResetSizeAndPositionClick(Sender: TObject);
    PROCEDURE EditWindowResize(Sender : TObject);
    PROCEDURE EditWindowShow(Sender : TObject);
    PROCEDURE ExitWithoutSavingButtonClick(Sender : TObject);
    PROCEDURE SaveChangesAndExitButtonClick(Sender : TObject);
    PROCEDURE UndoChangesButtonClick(Sender : TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE AddTrackCircuit(Line : Integer; NewTrackCircuit : Boolean);
{ Add a track circuit to a line }

PROCEDURE ChangeSignalDirection(S : Integer);
{ Change a selected signal's direction }

FUNCTION CheckIfAnyEditedDataHasChanged : Boolean;
{ Ask whether we want to save amended data (if any) before selecting another signal }

PROCEDURE ClearEditValueListAndEditedItem;
{ Empty the value list so as not to display anyting if we click on an unrecognised item, or a blank bit of screen }

PROCEDURE CreatePoint(LineArray : IntegerArrayType; TempPointType : TypeOfPoint; X, Y : Integer);
{ Creates a point from scratch }

PROCEDURE CreateSignal(Direction : DirectionType; Line : Integer);
{ Creates a signal from scratch }

PROCEDURE DeleteLine(LineToDeleteNum : Integer; OUT CanDelete: Boolean);
{ Delete a line after appropriate checks }

PROCEDURE DeleteLineTrackCircuit(Line : Integer);
{ Delete a track circuit from a particular line after appropriate checks }

PROCEDURE DeletePoint(PointToDeleteNum : Integer);
{ Delete a point after appropriate checks }

PROCEDURE DeleteSignal(SignalToDeleteNum : Integer);
{ Delete a signal after appropriate checks }

PROCEDURE DeselectLine;
{ Ensures handles are switched off, etc. }

PROCEDURE DragEndOfLine(GridX, GridY : Integer; ShiftState : TShiftState);
{ Allows a line end to be moved by the mouse }

PROCEDURE DragSignal(S, MouseX, MouseY : Integer; OUT NearestLineToSignal, TooNearSignal : Integer);
{ Allows a signal to be moved by the mouse }

PROCEDURE DragWholeLine(GridX, GridY : Integer);
{ Move a whole line by dragging its middle handle }

PROCEDURE DropSignal;
{ Drops the signal at the nearest adjacent line }

FUNCTION GetNearestLine(X, Y : Integer) : Integer;
{ Returns the line number nearest to a given position if the line is horizontal and the position is within a line's mouse rectangle }

PROCEDURE InitialiseEditUnit;
{ Initialises the unit }

PROCEDURE JoinLine(LineArray : IntegerArrayType; GridX, GridY : Integer);
{ Join two lines at the point indicated. The checks to make sure that the lines are next to each other and are at the same angle have been done in the Popup code. }

PROCEDURE LineDraggingComplete(ShiftState : TShiftState);
{ Called when when we have finished line creation by mouse }

PROCEDURE MoveObjectLeft;
{ Moves whatever is currently being edited to the left }

PROCEDURE MoveObjectRight;
{ Moves whatever is currently being edited to the right }

PROCEDURE ProcessLocationsCheckListBoxChecks;
{ See which locations are ticked and update the array }

PROCEDURE SplitLine(OldLine, GridX, GridY : Integer);
{ Split a line at the point indicated }

PROCEDURE StartSignalEdit(S : Integer);
{ Set up a signal edit - this is where we save the signal's original state so we can revert to it regardless of how many edits there are }

PROCEDURE StartPointEdit(P : Integer);
{ Set up a point edit - this is where we save the point's original state so we can revert to it regardless of how many edits there are }

PROCEDURE StartLineEdit(Line : Integer);
{ Set up a line edit - this is where we save the line's original state so we can revert to it regardless of how many edits there are }

PROCEDURE StartTrackCircuitEdit(TC : Integer);
{ Set up a track-circuit edit - this is where we save the track circuit's original state so we can revert to it regardless of how many edits there are }

PROCEDURE TurnCreateLineModeOn;
{ Turn create-line mode on }

PROCEDURE TurnCreateLineModeOff;
{ Turn create-line mode off }

PROCEDURE TurnEditModeOn(S, P, BS, Line, TC : Integer);
{ Turn edit Mode on }

PROCEDURE TurnEditModeOff;
{ Turn edit Mode off }

PROCEDURE UndoEditChanges;
{ Undo any changes made by moving the item on screen or editing it in the value list editor }

VAR
  EditedBufferStop : Integer = UnknownBufferStop;
  EditedLine : Integer = UnknownLine;
  EditedPoint : Integer = UnknownPoint;
  EditedSignal : Integer = UnknownSignal;
  EditedTrackCircuit : Integer = UnknownTrackCircuit;
  EditingExistingLine : Boolean = False;
  EditWindow: TEditWindow;
  EndOfLineDragging : Boolean = False;
  NewLineX : Integer = 0;
  NewLineY : Integer = 0;
  SaveDragX : Integer = 0;
  SaveDragY : Integer = 0;
  WholeLineDragging : Boolean = False;

IMPLEMENTATION

{$R *.dfm}

USES Diagrams, Input, Cuneo, Lenz, Types, Math {sic}, Main;

CONST
  UnitRef = 'Edit';

VAR
  SaveEvent : TNotifyEvent;
  SaveIconHandle : HICON;
  SaveLineNum : Integer;
  SaveLineRec : LineRec;
  SavePointNum : Integer;
  SavePointRec : PointRec;
  SaveSignalNum : Integer;
  SaveSignalRec : SignalRec;
  SaveSystemOnlineState : Boolean;
  SaveTrackCircuitNum : Integer;
  SaveTrackCircuitRec : TrackCircuitRec;
  ValueListToBeCleared : Boolean = False;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

{ MaskEdit characters

! If a ! character appears in the mask, optional characters are represented in the text as leading blanks. If a ! character is not present, optional characters are
  represented in the text as trailing blanks.
> If a > character appears in the mask, all characters that follow are in uppercase until the end of the mask or until a < character is encountered.
< If a < character appears in the mask, all characters that follow are in lowercase until the end of the mask or until a > character is encountered.
<> If these two characters appear together in a mask, no case checking is done and the data is formatted with the case the user uses to enter the data.
\  The character that follows a \ character is a literal character. Use this character to use any of the mask special characters as a literal in the data.
L The L character requires an alphabetic character only in this position. For the US, this is A-Z, a-z.
l The l character permits only an alphabetic character in this position, but doesn't require it.
A The A character requires an alphanumeric character only in this position. For the US, this is A-Z, a-z, 0-9.
a The a character permits an alphanumeric character in this position, but doesn't require it.
C The C character requires an arbitrary character in this position.
c The c character permits an arbitrary character in this position, but doesn't require it.
0 The 0 character requires a numeric character only in this position.
9 The 9 character permits a numeric character in this position, but doesn't require it.
# The # character permits a numeric character or a plus or minus sign in this position, but doesn't require it.
: The : character is used to separate hours, minutes, and seconds in times. If the character that separates hours, minutes, and seconds is different in the regional
  settings of the Control Panel utility on your computer system, that character is used instead.
/ The / character is used to separate months, days, and years in dates. If the character that separates months, days, and years is different in the regional settings of
  the Control Panel utility on your computer system, that character is used instead.
; The ; character is used to separate the three fields of the mask.
_ The _ character automatically inserts spaces into the text. When the user enters characters in the field, the cursor skips the _ character.
}

PROCEDURE InitialiseEditUnit;
{ Initialises the unit }
BEGIN
  EditWindow.Height := EditWindowHeight;
  EditWindow.Top := EditWindowTop;
  EditWindow.Left := EditWindowLeft;
  EditWindow.Width := EditWindowWidth;
END; { InitialiseEditUnit }

PROCEDURE WriteEditWindowLabelCaption(S : String);
BEGIN
  WITH Edit.EditWindow DO BEGIN
    EditWindowLabel.Caption := S;
    EditWindowLabel.Top := (EditWindow.Height - EditWindowLabel.Height) DIV 2;
  END; {WITH}
END; { WriteEditWindowLabelCaption }

PROCEDURE TurnEditModeOn(S, P, BS, Line, TC : Integer);
{ Turn edit mode on }
VAR
  EditValueListEditorRight : Integer;
  EditWindowRight : Integer;
  EditWindowButtonPanelRight : Integer;

BEGIN
  IF NOT EditMode THEN BEGIN
    EditMode := True;
    Diagrams.DiagramsWindow.Visible := False;

    { The tags are used to tell EditWindow.Visible which value list to edit }
    Edit.EditWindow.Tag := -1;
    IF S <> UnknownSignal THEN BEGIN
      Edit.EditWindow.Tag := 1;
      StartSignalEdit(S);
    END ELSE
      IF P <> UnknownPoint THEN BEGIN
        Edit.EditWindow.Tag := 2;
        StartPointEdit(P);
      END ELSE
        IF Line <> UnknownLine THEN BEGIN
          Edit.EditWindow.Tag := 3;
          StartLineEdit(Line);
        END ELSE
          IF TC <> UnknownTrackCircuit THEN BEGIN
            Edit.EditWindow.Tag := 4;
            StartTrackCircuitEdit(TC);
          END;

    WITH Edit.EditWindow DO BEGIN
      { Position the window based on the available window size }
      Visible := True;

      { Resize the list editor depending on the size of the window }
      EditValueListEditor.Height := EditWindow.Height - (EditWindow.Height DIV 4);

      { Space the buttons too }
      EditWindowButtonPanel.Top := EditValueListEditor.Top + ((EditValueListEditor.Height - EditWindowButtonPanel.Height) DIV 2);

      EditWindowRight := EditWindow.Left + EditWindow.Width;
      EditValueListEditorRight := EditValueListEditor.Left + EditValueListEditor.Width;
      EditWindowButtonPanelRight := EditWindowButtonPanel.Left + EditWindowButtonPanel.Width;

      EditWindowButtonPanel.Left := EditValueListEditorRight + (((EditWindowRight - EditValueListEditorRight) - EditWindowButtonPanel.Width) DIV 2);
      EditWindowLabel.Left := EditWindow.Left + (((EditValueListEditor.Left - EditWindow.Left) - EditWindowLabel.Width) DIV 2);
    END; {WITH}

    SaveSystemOnlineState := SystemOnline;
    IF SystemOnline THEN
      SetSystemOffline('System offline as edit mode starting', NOT SoundWarning);

    SetCaption(FWPRailWindow, 'EDITING...');
    EditWindow.EditWindowLabel.Caption := '';
    SaveIconHandle := Application.Icon.Handle;
    Application.Icon.Handle := EditIcon.Handle;
  END;
END; { TurnEditModeOn }

PROCEDURE TurnEditModeOff;
{ Turn edit mode off }
BEGIN
  TRY
    IF EditMode THEN BEGIN
      SetCaption(FWPRailWindow, '');

      EditedPoint := UnknownSignal;
      EditedPoint := UnknownPoint;
      DeselectLine;
      EditedTrackCircuit := UnknownTrackCircuit;

      EditWindow.Visible := False;
      Diagrams.DiagramsWindow.Visible := True;

      EditMode := False;
      Application.Icon.Handle := SaveIconHandle;

      ClearEditValueListAndEditedItem;

      { and force a redraw so that the highlighted signal/point etc. is de-highlighted }
      FWPRailWindow.Repaint;

      IF SaveSystemOnlineState THEN BEGIN
        IF SetSystemOnline THEN
          Log('A Edit mode off so system now online again')
        ELSE
          Log('A Edit mode off but system failed to go online');
      END;

      WITH EditWindow DO BEGIN
        UndoChangesButton.Enabled := False;
        SaveChangesAndExitButton.Enabled := False;
        ExitWithoutSavingButton.Enabled := False;
      END; {WITH}
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG TurnEditModeOff: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TurnEditModeOff }

PROCEDURE TurnCreateLineModeOn;
{ Turn create-line mode on }
BEGIN
  CreateLineMode := True;
  InvalidateScreen(UnitRef, 'TurnCreateLineModeOn');
END; { TurnCreateLineModeOn }

PROCEDURE TurnCreateLineModeOff;
{ Turn create-line mode off }
BEGIN
  CreateLineMode := False;
  ShowLineHandles := False;
  InvalidateScreen(UnitRef, 'TurnCreateLineModeOff');
END; { TurnCreateLineModeOff }

PROCEDURE NoteThatDataHasChanged;
{ Note that data has changed and enable the undo button }
BEGIN
  IF EditedSignal <> UnknownSignal THEN
    Signals[EditedSignal].Signal_DataChanged := True
  ELSE
    IF EditedPoint <> UnknownPoint THEN
      Points[EditedPoint].Point_DataChanged := True
    ELSE
      IF EditedLine <> UnknownLine THEN
        Lines[EditedLine].Line_DataChanged := True
      ELSE
        IF EditedTrackCircuit <> UnknownTrackCircuit THEN
          TrackCircuits[EditedTrackCircuit].TC_DataChanged := True;

  WITH EditWindow DO BEGIN
    UndoChangesButton.Enabled := True;
    SaveChangesAndExitButton.Enabled := True;
    ExitWithoutSavingButton.Enabled := True;
  END; {WITH}
END; { NoteThatDataHasChanged }

PROCEDURE TEditWindow.EditValueListEditorStringsChange(Sender: TObject);   { what's this for? ***}
BEGIN
  IF NOT ValueListToBeCleared THEN BEGIN
    { clearing the value list changes the strings in it, but we can't then save or undo them }
    UndoChangesButton.Enabled := True;
    SaveChangesAndExitButton.Enabled := True;
    ExitWithoutSavingButton.Enabled := True;
  END;
END; { EditWindowValueListEditorStringsChange }

PROCEDURE CreateLocationCheckList(LocationsToAddToCheckList : IntegerArrayType);
{ Create a location check list with one or more of the entries ticked }
VAR
  ArrayCount : Integer;
  Done : Boolean;
  I : Integer;
  Loc : Integer;
  StrList: TStringList;

BEGIN
  TRY
    WITH MovementWindow DO BEGIN
      MovementWindow.Visible := True;

      { Use a hack to sort the list box }
      StrList := TStringList.Create;

      { Populate the box, platforms first }
      FOR Loc := 0 TO High(Locations) DO
        IF Locations[Loc].Location_IsPlatform THEN
          StrList.Add(Locations[Loc].Location_LongNameStr);

      StrList.Sort;

      LocationsCheckListBox.Clear;

      FOR I := 0 TO StrList.Count - 1 DO
        LocationsCheckListBox.Items.Add(StrList.Strings[I]);

      StrList.Clear;

      { Now the fiddleyard items }
      FOR Loc := 0 TO High(Locations) DO
        IF Locations[Loc].Location_IsFiddleyard THEN
          StrList.Add(Locations[Loc].Location_LongNameStr);

      StrList.Sort;

      FOR I := 0 TO StrList.Count - 1 DO
        LocationsCheckListBox.Items.Add(StrList.Strings[I]);

      StrList.Destroy;

      IF Length(LocationsToAddToCheckList) = 0 THEN
        Log('X No locations supplied to CreateLocationCheckList routine')
      ELSE BEGIN
        { Now tick those locations that are already set }
        ArrayCount := 0;
        WHILE ArrayCount < Length(LocationsToAddToCheckList) DO BEGIN
          { find it in the checklistbox and select it }
          I := 0;
          Done := False;
          WHILE (I < LocationsCheckListBox.Items.Count - 1) AND NOT Done DO BEGIN
            IF LocationsCheckListBox.Items[I] = Locations[LocationsToAddToCheckList[ArrayCount]].Location_LongNameStr THEN BEGIN
              LocationsCheckListBox.Checked[I] := True;
              Done := True;
            END;
            Inc(I);
          END; {WHILE}
          Inc(ArrayCount);
        END; {WHILE}
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CreateLocationCheckList: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CreateLocationCheckList }

PROCEDURE DeselectLine;
{ Ensures handles are switched off, etc. }
BEGIN
  IF EditedLine <> UnknownLine THEN BEGIN
    Lines[EditedLine].Line_ShowHandles := False;
    Lines[EditedLine].Line_IsBeingMovedByHandle := NoHandle;
  END;
  EditingExistingLine := False;
  EditedLine := UnknownLine;
  EndOfLineDragging := False;
  WholeLineDragging := False;
END; { DeselectLine }

PROCEDURE TEditWindow.EditValueListEditorEditButtonClick(Sender: TObject);
{ This is only used for dealing with the entry for the Locations Check Box }
VAR
  KeyName : String;
  KeyValue : String;

  PROCEDURE FindKeys(Row : Integer; OUT KeyName, KeyValue : String);
  VAR
    DelimiterPos : Integer;

  BEGIN
    WITH EditWindow.EditValueListEditor DO BEGIN
      DelimiterPos := FindDelimiter('=', Strings[Row - 1], 1);
      KeyName := Copy(Strings[Row - 1], 1, DelimiterPos - 1);
      KeyValue := Copy(Strings[Row - 1], DelimiterPos + 1);
    END; {FOR}
  END; { FindKeys }

BEGIN
  TRY
    { Work out which field we're editing from the row we're on (there seems to be no other way of finding out) }
    IF EditedSignal <> UnknownSignal THEN
      FindKeys(EditValueListEditor.Row, KeyName, KeyValue);

      WITH MovementWindow DO BEGIN
        MovementWindow.Visible := True;

        IF KeyName = 'Signal Locations To Monitor' THEN BEGIN
          CreateLocationCheckList(Signals[EditedSignal].Signal_LocationsToMonitorArray);

      END; {WITH}
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG EditValueListEditorEditButtonClick: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { EditValueListEditorEditButtonClick }

PROCEDURE WriteIntegerValueExcludingZero(Str : String; I : Integer; EditMask : String);
BEGIN
  TRY
    WITH EditWindow.EditValueListEditor DO BEGIN
      IF I <> 0 THEN
        Values[Str] := IntToStr(I)
      ELSE
        Values[Str] := '';

      IF EditMask <> '' THEN
        ItemProps[Str].EditMask := EditMask;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteIntegerValueExcludingZero: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WriteIntegerValueExcludingZero }

PROCEDURE WriteIntegerValueIncludingZero(Str : String; I : Integer; EditMask : String);
BEGIN
  WITH EditWindow.EditValueListEditor DO BEGIN
    Values[Str] := IntToStr(I);

    IF EditMask <> '' THEN
      ItemProps[Str].EditMask := EditMask;
  END; {WITH}
END; { WriteIntegerValueINcludingZero }

PROCEDURE WriteExtendedValueIncludingZero(Str : String; F : Extended; EditMask : String);
BEGIN
  WITH EditWindow.EditValueListEditor DO BEGIN
    Values[Str] := FloatToStr(F);

    IF EditMask <> '' THEN
      ItemProps[Str].EditMask := EditMask;
  END; {WITH}
END; { WriteExtendedValueIncludingZero }

PROCEDURE WriteBooleanValue(Str : String; Bool : Boolean);
BEGIN
  WITH EditWindow.EditValueListEditor DO BEGIN
    IF Bool THEN
      Values[Str] := 'True'
    ELSE
      Values[Str] := 'False';

    ItemProps[Str].ReadOnly := True;
    ItemProps[Str].PickList.Add('True');
    ItemProps[Str].PickList.Add('False');
    ItemProps[Str].EditStyle := esPickList;
  END; {WITH}
END; { WriteBooleanValue }

PROCEDURE WriteIntegerArrayValues(IntegerArrayStr : String; IntegerArray : IntegerArrayType; NoValuesStr, ValuesStr : String);
VAR
  I : Integer;

BEGIN
  WITH EditWindow.EditValueListEditor DO BEGIN
    IF Length(IntegerArray) = 0 THEN
      Values[IntegerArrayStr] := NoValuesStr
    ELSE BEGIN
      FOR I := 0 TO High(IntegerArray) DO
        AppendToIntegerArray(IntegerArray, IntegerArray[I]);
      Values[IntegerArrayStr] := ValuesStr;
    END;
    ItemProps[IntegerArrayStr].ReadOnly := True;
    ItemProps[IntegerArrayStr].EditStyle := esEllipsis;
  END; {WITH}
END; { WriteIntegerArrayValues }

PROCEDURE WritePickListValue(Str1, Str2 : String; PickListStrs : ARRAY OF String);
VAR
  I : Integer;

BEGIN
  WITH EditWindow.EditValueListEditor DO BEGIN
    Values[Str1] := Str2;

    ItemProps[Str1].PickList.Clear;
    ItemProps[Str1].ReadOnly := True;
    FOR I := 0 TO High(PickListStrs) DO
      ItemProps[Str1].PickList.Add(PickListStrs[I]);
    ItemProps[Str1].EditStyle := esPickList;
  END; {WITH}
END; { WritePickListValue }

PROCEDURE ClearEditValueListAndEditedItem;
{ Empty the value list so as not to display anything if we click on an unrecognised item, or a blank bit of screen }
BEGIN
  ValueListToBeCleared := True;

  WITH EditWindow DO BEGIN
    EditValueListEditor.Strings.Clear;
    EditWindowLabel.Caption := '';
  END; {WITH}

  IF EditedSignal <> UnknownSignal THEN
    EditedSignal := UnknownSignal
  ELSE
    IF EditedPoint <> UnknownPoint THEN
      EditedPoint := UnknownPoint
    ELSE
      IF EditedLine <> UnknownLine THEN
        DeselectLine
      ELSE
        IF EditedTrackCircuit <> UnknownTrackCircuit THEN
          EditedTrackCircuit := UnknownTrackCircuit;

  EditingExistingLine := False;

  WITH EditWindow DO BEGIN
    UndoChangesButton.Enabled := False;
    SaveChangesAndExitButton.Enabled := False;
    ExitWithoutSavingButton.Enabled := False;
  END; {WITH}

  InvalidateScreen(UnitRef, 'ClearEditValueListAndEditedItem');
END; { ClearEditValueListAndEditedItem }

PROCEDURE ProcessLocationsCheckListBoxChecks;
{ See which locations are ticked and update the array. This shouldn't need validation as the user has not been given the opportunity of introducing errors. }
VAR
  I : Integer;
  Loc : Integer;

BEGIN
  TRY
    WITH MovementWindow DO BEGIN
      SetLength(Signals[EditedSignal].Signal_LocationsToMonitorArray, 0);

      I := 0;
      WHILE I < LocationsCheckListBox.Items.Count - 1 DO BEGIN
        IF LocationsCheckListBox.Checked[I] THEN BEGIN
          Loc := 0;
          WHILE Loc < Length(Locations) DO BEGIN
            IF Locations[Loc].Location_LongNameStr = LocationsCheckListBox.Items[I] THEN
              AppendToIntegerArray(Signals[EditedSignal].Signal_LocationsToMonitorArray, Loc);
            Inc(Loc);
          END;
        END;
        Inc(I);
      END; {WHILE}
    END; {WITH}

    WITH EditWindow DO BEGIN
      UndoChangesButton.Enabled := True;
      SaveChangesAndExitButton.Enabled := True;
      ExitWithoutSavingButton.Enabled := True;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ProcessSignalLocationsToMonitorCheckListBoxChecks: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ProcessSignalLocationsToMonitorCheckListBoxChecks }

PROCEDURE WriteLineValuesToValueList;
{ Create a value list in the edit window with the appropriate values }
BEGIN
  TRY
    WITH EditWindow DO BEGIN
      { Temporarily deactivate the OnStringsChange event so as not to enable the various buttons until we've written the values to the value list - otherwise the act of
        writing them switches the buttons on before we've done any editing
      }
      SaveEvent := EditValueListEditorStringsChange;
      EditValueListEditor.OnStringsChange := NIL;

      WITH EditValueListEditor DO BEGIN
        WITH Lines[EditedLine] DO BEGIN
          WriteIntegerValueExcludingZero(Line_NumberFieldName, EditedLine, '');
          { make the line number read only }
          ItemProps[Line_NumberFieldName].ReadOnly := True;

          WriteEditWindowLabelCaption('Editing Line ' + IntToStr(EditedLine) + ' (' + LineToStr(EditedLine) + ')');

          Values[Line_NameStrFieldName] := Line_NameStr;

          WriteIntegerValueExcludingZero(Line_GridUpXFieldName, Line_GridUpX, '9999');
          WriteIntegerValueExcludingZero(Line_GridDownXFieldName, Line_GridDownX, '9999');

          WriteIntegerValueExcludingZero(Line_TCFieldName, Line_TC, '9999');

          WritePickListValue(Line_EndOfLineMarkerFieldName, EndOfLineToStr(Line_EndOfLineMarker), [BufferStopAtUpStr, BufferStopAtDownStr, ProjectedLineAtUpStr,
                                                            ProjectedLineAtDownStr, NotEndOfLineStr]);

          Values[Line_BufferStopTheatreDestinationStrFieldName] := Line_BufferStopTheatreDestinationStr;

          WritePickListValue(Line_DirectionFieldName, DirectionToStr(Line_Direction), ['Up', 'Down', 'Bidirectional']);

          WritePickListValue(Line_TypeOfLineFieldName, TypeOfLineToStr(Line_TypeOfLine),
                                                       [MainOrGoodsLineStr, MainLineStr, GoodsLineStr, BranchLineDoubleStr, BranchLineSingleStr, IslandStationLineStr,
                                                        MainStationLineStr, BranchStationLineStr, WindowStationLineStr, SidingLineStr, FiddleyardLineStr,
                                                        SidingsApproachLineStr, StationAvoidingLineStr, ProjectedLineStr]);

          Values[Line_UpConnectionChFieldName] := Line_UpConnectionCh;
          ItemProps[Line_UpConnectionChFieldName].EditMask := '>L';

          Values[Line_DownConnectionChFieldName] := Line_DownConnectionCh;
          ItemProps[Line_DownConnectionChFieldName].EditMask := '>L';

          WritePickListValue(Line_GradientFieldName, GradientToStr(Line_Gradient), [LevelStr, RisingIfUpStr, RisingIfDownStr]);

          WriteBooleanValue(Line_OutOfUseFieldName, Line_OutOfUseState = OutOfUse);

          WriteIntegerValueExcludingZero(Line_InUseFeedbackUnitFieldName, Line_InUseFeedbackUnit, '9999');
        END; {WITH}
      END; {WITH}

      ExitWithoutSavingButton.Enabled := True;

      { Reactivate this event so as to enable the various buttons if we do any editing }
      EditValueListEditor.OnStringsChange := SaveEvent;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteLineValuesToValueList: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WriteLineValuesToValueList }

PROCEDURE UpdateLineValueList(Line : Integer { add other items here });
{ Update the value list with the appropriate values }
BEGIN
  TRY
    WITH EditWindow DO BEGIN
      { Temporarily deactivate the OnStringsChange event so as not to enable the various buttons until we've written the values to the value list - otherwise the act of
        writing them switches the buttons on before we've done any editing
      }
      SaveEvent := EditWindow.EditValueListEditorStringsChange;
      EditWindow.EditValueListEditor.OnStringsChange := NIL;

      WITH EditValueListEditor DO BEGIN
//        IF AdjacentLine <> UnknownLine THEN
//          Values[Line_AdjacentLineFieldName] := LineToStr(AdjacentLine);
//
//        WriteIntegerValueExcludingZero(Line_AdjacentLineXOffsetFieldName, AdjacentLineXOffset, '!#99');
//
//        IF Direction <> UnknownDirection THEN
//         WritePickListValue(Line_DirectionFieldName, DirectionToStr(Direction), ['Up', 'Down']);
      END; {WITH}

      NoteThatDataHasChanged;

      EditWindow.EditValueListEditor.OnStringsChange := SaveEvent;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG UpdateLineValueList: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { UpdateLineValueList }

PROCEDURE WritePointValuesToValueList;
{ Create a value list in the edit window with the appropriate values }

  PROCEDURE WritePointValue(Str : String; P : Integer; EditMask : String);
  BEGIN
    WITH EditWindow.EditValueListEditor DO BEGIN
      IF P <> UnknownPoint THEN
        Values[Str] := IntToStr(P)
      ELSE
        Values[Str] := '';

      IF EditMask <> '' THEN
        ItemProps[Str].EditMask := EditMask;
    END; {WITH}
  END; { WritePointValue }

BEGIN { WritePointValuesToValueList }
  TRY
    WITH EditWindow DO BEGIN
      { Temporarily deactivate the OnStringsChange event so as not to enable the various buttons until we've written the values to the value list - otherwise the act of
        writing them switches the buttons on before we've done any editing
      }
      SaveEvent := EditValueListEditorStringsChange;
      EditValueListEditor.OnStringsChange := NIL;

      WITH EditValueListEditor DO BEGIN
        WriteEditWindowLabelCaption('Editing Point ' + IntToStr(EditedPoint));

        WITH Points[EditedPoint] DO BEGIN
          WriteIntegerValueExcludingZero(Point_NumberFieldName, EditedPoint, '');
          ItemProps[Point_NumberFieldName].ReadOnly := True;

          Values[Point_DivergingLineFieldName] := LineToStr(Point_DivergingLine);
          Values[Point_HeelLineFieldName] := LineToStr(Point_HeelLine);
          Values[Point_StraightLineFieldName] := LineToStr(Point_StraightLine);

          WriteIntegerValueExcludingZero(Point_LenzNumFieldName, Point_LenzNum, '9999');
          WriteIntegerValueExcludingZero(Point_LenzUnitFieldName, Point_LenzUnit, '9999');
          WritePickListValue(Point_LenzUnitTypeFieldName, Point_LenzUnitType, ['LS101', 'LS150']);
          WriteBooleanValue(Point_ManualOperationFieldName, Point_ManualOperation);
          WriteIntegerValueExcludingZero(Point_FeedbackUnitFieldName, Point_FeedbackUnit, '9999');
          WriteIntegerValueExcludingZero(Point_FeedbackInputFieldName, Point_FeedbackInput, '9999');
          WriteBooleanValue(Point_FeedbackOnIsStraightFieldName, Point_FeedbackOnIsStraight);
          WriteBooleanValue(Point_WiringReversedFlagFieldName, Point_WiringReversedFlag);
          WritePickListValue(Point_TypeFieldName, PointTypeToStr(Point_Type), [OrdinaryPointStr, CrossOverPointStr, ThreeWayPointAStr, ThreeWayPointBStr, SingleSlipStr,
                                                                               DoubleSlipStr, ProtectedPointStr, CatchPointUpStr, CatchPointDownStr, UnknownPointTypeStr]);
          WritePointValue(Point_RelatedPointFieldName, Point_RelatedPoint, '9999');
          WritePickListValue(Point_DefaultStateFieldName, PointStateToStr(Point_DefaultState), [StraightStr, DivergingStr, UnknownStr, OutOfActionStr]);
          WriteBooleanValue(Point_LockedIfHeelTCOccupiedFieldName, Point_LockedIfHeelTCOccupied);
          WriteBooleanValue(Point_OutOfUseFieldName, Point_OutOfUse);
          WriteBooleanValue(Point_LockedIfNonHeelTCsOccupiedFieldName, Point_LockedIfNonHeelTCsOccupied);
          Values[Point_NotesFieldName] := Point_Notes;
        END; {WITH}
      END; {WITH}
    END; {WITH}

    { Reactivate this event so as to enable the various buttons if we do any editing }
    EditWindow.EditValueListEditor.OnStringsChange := SaveEvent;

  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WritePointValuesToValueList: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WritePointValuesToValueList }

PROCEDURE WriteSignalValuesToValueList;
{ Create or update a value list in the edit window with the appropriate values }

  PROCEDURE WriteSignalValue(Str : String; S : Integer; EditMask : String);
  BEGIN
    WITH EditWindow.EditValueListEditor DO BEGIN
      IF S <> UnknownSignal THEN
        Values[Str] := 'S' + IntToStr(S)
      ELSE
        Values[Str] := '';

      IF EditMask <> '' THEN
        ItemProps[Str].EditMask := EditMask;
    END; {WITH}
  END; { WriteSignalValue }

  PROCEDURE WriteJunctionIndicatorValue(S : Integer; Str : String; JunctionIndicator : JunctionIndicatorType);
  BEGIN
    WITH EditWindow.EditValueListEditor DO BEGIN
      WITH Signals[S] DO BEGIN
        IF NOT Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_Exists THEN
          Values[Str] := ''
        ELSE BEGIN
          IF Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
            Values[Str] := 'S' + IntToStr(Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal)
          ELSE
            IF Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetBufferStop <> UnknownBufferStop THEN
              Values[Str] := 'BS' + IntToStr(Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetBufferStop);
        END;
        ItemProps[Str].EditMask := 'LA999';
      END; {WITH}
    END; {WITH}
  END; { WriteJunctionIndicatorValue }

VAR
  I : Integer;
  TempStr : string;

BEGIN { WriteSignalValuesToValueList }
  TRY
    { See if it's merely a reselection of the same signal - in which case do not save its current state }
    WITH EditWindow DO BEGIN
      { Temporarily deactivate the OnStringsChange event so as not to enable the various buttons until we've written the values to the value list - otherwise the act of
        writing them switches the buttons on before we've done any editing
      }
      SaveEvent := EditWindow.EditValueListEditorStringsChange;
      EditWindow.EditValueListEditor.OnStringsChange := NIL;

      WITH EditValueListEditor DO BEGIN
        WITH Signals[EditedSignal] DO BEGIN
          WriteIntegerValueIncludingZero(Signal_NumberFieldName, EditedSignal, ''); { as the signal number might be zero }
          { make the signal number read only }
          ItemProps[Signal_NumberFieldName].ReadOnly := True;

          WriteEditWindowLabelCaption('Editing Signal ' + IntToStr(EditedSignal));

          WriteIntegerValueExcludingZero(Signal_AccessoryAddressFieldName, Signal_AccessoryAddress, '9999');

          Values[Signal_AdjacentLineFieldName] := LineToStr(Signal_AdjacentLine);
          WriteIntegerValueExcludingZero(Signal_AdjacentLineXOffsetFieldName, Signal_AdjacentLineXOffset, '!#99');

          IF Signal_ApproachControlAspect <> NoAspect THEN
            Values[Signal_ApproachControlAspectFieldName] := AspectToStr(Signal_ApproachControlAspect)
          ELSE
            Values[Signal_ApproachControlAspectFieldName] := '';

          Values[Signal_AsTheatreDestinationFieldName] := Signal_AsTheatreDestination;
          WriteBooleanValue(Signal_AutomaticFieldName + ' [not yet in use]', Signal_Automatic); { not yet in use }
          WriteIntegerValueExcludingZero(Signal_DecoderNumFieldName, Signal_DecoderNum, '9999');
          WritePickListValue(Signal_DirectionFieldName, DirectionToStr(Signal_Direction), ['Up', 'Down']);

          IF Length(Signal_SemaphoreDistantHomesArray) = 0 THEN
            Values[Signal_SemaphoreDistantHomesArrayFieldName] := '(No Home Signals)'
          ELSE BEGIN
            TempStr := 'S' + IntToStr(Signal_SemaphoreDistantHomesArray[0]);
            IF Length(Signal_SemaphoreDistantHomesArray) > 1 THEN BEGIN
              FOR I := 1 TO High(Signal_SemaphoreDistantHomesArray) DO
                TempStr := TempStr + ', S' + IntToStr(Signal_SemaphoreDistantHomesArray[I]);
            END;
            Values[Signal_SemaphoreDistantHomesArrayFieldName] := TempStr;
          END;

          WriteIntegerValueExcludingZero(Signal_IndicatorDecoderFunctionNumFieldName, Signal_IndicatorDecoderFunctionNum, '9999');
          WriteIntegerValueExcludingZero(Signal_IndicatorDecoderNumFieldName, Signal_IndicatorDecoderNum, '9999');
          WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType), [NoIndicatorStr, JunctionIndicatorStr, TheatreIndicatorStr]);
          WritePickListValue(Signal_IndicatorSpeedRestrictionFieldName, MPHToStr(Signal_IndicatorSpeedRestriction),
                                                                                ['10', '20', '30', '40', '50', '60', '70', '80', '90', '100', '110']);
          WriteSignalValue(Signal_NextSignalIfNoIndicatorFieldName, Signal_NextSignalIfNoIndicator, '999');
          WriteJunctionIndicatorValue(EditedSignal, Signal_UpperLeftIndicatorTargetFieldName, UpperLeftIndicator);
          WriteJunctionIndicatorValue(EditedSignal, Signal_MiddleLeftIndicatorTargetFieldName, MiddleLeftIndicator);
          WriteJunctionIndicatorValue(EditedSignal, Signal_LowerLeftIndicatorTargetFieldName, LowerLeftIndicator);
          WriteJunctionIndicatorValue(EditedSignal, Signal_UpperRightIndicatorTargetFieldName, UpperRightIndicator);
          WriteJunctionIndicatorValue(EditedSignal, Signal_MiddleRightIndicatorTargetFieldName, MiddleRightIndicator);
          WriteJunctionIndicatorValue(EditedSignal, Signal_LowerRightIndicatorTargetFieldName, LowerRightIndicator);
          WriteIntegerArrayValues(Signal_LocationsToMonitorFieldName, Signal_LocationsToMonitorArray, '(No Locations)', '(Locations)');
          Values[Signal_NotesFieldName] := Signal_Notes;
          WriteBooleanValue(Signal_NotUsedForRouteingFieldName, Signal_NotUsedForRouteing);
          WriteSignalValue(Signal_OppositePassingLoopSignalFieldName, Signal_OppositePassingLoopSignal, 'S999');
          WriteBooleanValue(Signal_OutOfUseFieldName, Signal_OutOfUse);
          WriteBooleanValue(Signal_PossibleRouteHoldFieldName, Signal_PossibleRouteHold);
          WriteBooleanValue(Signal_PossibleStationStartRouteHoldFieldName, Signal_PossibleStationStartRouteHold);
          WritePickListValue(Signal_QuadrantFieldName, SignalQuadrantToStr(Signal_Quadrant), ['', 'Upper', 'Lower']);
          WritePickListValue(Signal_TypeFieldName, SignalTypeToStr(Signal_Type, LongStringType),
                                                                                 [CallingOnStr, TwoAspectStr, ThreeAspectStr, FourAspectStr, SemaphoreHomeStr,
                                                                                  SemaphoreDistantStr]);
        END; {WITH}
      END; {WITH}

      ExitWithoutSavingButton.Enabled := True;

      { Reactivate this event so as to enable the various buttons if we do any editing }
      EditValueListEditor.OnStringsChange := SaveEvent;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteSignalValuesToValueList: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WriteSignalValuesToValueList }

PROCEDURE UpdateSignalValueList(S : Integer; AdjacentLine : Integer; AdjacentLineXOffset : Integer; Direction : DirectionType);
{ Update the value list with the appropriate values }
BEGIN
  TRY
    WITH EditWindow DO BEGIN
      { Temporarily deactivate the OnStringsChange event so as not to enable the various buttons until we've written the values to the value list - otherwise the act of
        writing them switches the buttons on before we've done any editing
      }
      SaveEvent := EditWindow.EditValueListEditorStringsChange;
      EditWindow.EditValueListEditor.OnStringsChange := NIL;

      WITH EditValueListEditor DO BEGIN
        IF AdjacentLine <> UnknownLine THEN
          Values[Signal_AdjacentLineFieldName] := LineToStr(AdjacentLine);

        WriteIntegerValueExcludingZero(Signal_AdjacentLineXOffsetFieldName, AdjacentLineXOffset, '!#99');

        IF Direction <> UnknownDirection THEN
         WritePickListValue(Signal_DirectionFieldName, DirectionToStr(Direction), ['Up', 'Down']);
      END; {WITH}

      NoteThatDataHasChanged;

      EditWindow.EditValueListEditor.OnStringsChange := SaveEvent;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG UpdateSignalValueList: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { UpdateSignalValueList }

PROCEDURE WriteTrackCircuitValuesToValueList;
{ Create a value list in the edit window with the appropriate values }
BEGIN
  TRY
    WITH EditWindow DO BEGIN
      { Temporarily deactivate the OnStringsChange event so as not to enable the various buttons until we've written the values to the value list - otherwise the act of
        writing them switches the buttons on before we've done any editing
      }
      SaveEvent := EditValueListEditorStringsChange;
      EditValueListEditor.OnStringsChange := NIL;

      WITH EditValueListEditor DO BEGIN
        WriteEditWindowLabelCaption('Editing TC ' + IntToStr(EditedTrackCircuit));

        WITH TrackCircuits[EditedTrackCircuit] DO BEGIN
          WriteIntegerValueExcludingZero(TC_NumberFieldName, EditedTrackCircuit, '');
          WriteExtendedValueIncludingZero(TC_LengthInInchesFieldName, TC_LengthInInches, '9.9');
          WriteIntegerValueExcludingZero(TC_FeedbackUnitFieldName, TC_FeedbackUnit, '999');
          WriteIntegerValueExcludingZero(TC_FeedbackInputFieldName, TC_FeedbackInput, '999');
        END; {WITH}
      END; {WITH}

      ExitWithoutSavingButton.Enabled := True;

      { Reactivate this event so as to enable the various buttons if we do any editing }
      EditWindow.EditValueListEditor.OnStringsChange := SaveEvent;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteTrackCircuitValuesToValueList: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WriteTrackCircuitValuesToValueList }

PROCEDURE DoEditValidation(KeyName, NewKeyValue : String);
{ Do the validation here so it can be called by OnValidate and when we exit the value list }
CONST
  Init = True;

VAR
  ErrorMsg : String;
  JunctionIndicatorFound : Boolean;
  TempJunctionIndicator : JunctionIndicatorType;
  TempStrArray : ARRAY [0..5] OF String;

VAR
  I : Integer;

BEGIN
  TRY
    { We need this as occasionally the mask edit in value list returns a line of spaces when nothing is meant to be returned }
    NewKeyValue := Trim(NewKeyValue);
    ErrorMsg := '';

    WITH EditWindow.EditValueListEditor DO BEGIN
      IF EditedSignal <> UnknownSignal THEN BEGIN
        WITH Signals[EditedSignal] DO BEGIN
          IF KeyName = Signal_QuadrantFieldName THEN
            Signal_Quadrant := ValidateSignalQuadrant(NewKeyValue, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Signal_TypeFieldName THEN
              Signal_Type := ValidateSignalType(NewKeyValue, Signal_Quadrant, Signal_SemaphoreDistantHomesArray, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_IndicatorFieldName THEN
              Signal_Indicator := ValidateSignalIndicator(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_UpperLeftIndicatorTargetFieldName THEN BEGIN
              TempStrArray[0] := NewKeyValue;
              Signal_JunctionIndicators[UpperLeftIndicator] := ValidateSignalJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               [NoIndicatorStr, JunctionIndicatorStr, TheatreIndicatorStr]);
                END;
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_MiddleLeftIndicatorTargetFieldName THEN BEGIN
              TempStrArray[1] := NewKeyValue;
              Signal_JunctionIndicators[MiddleLeftIndicator] := ValidateSignalJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               [NoIndicatorStr, JunctionIndicatorStr, TheatreIndicatorStr]);
                END;
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_LowerLeftIndicatorTargetFieldName THEN BEGIN
              TempStrArray[2] := NewKeyValue;
              Signal_JunctionIndicators[LowerLeftIndicator] := ValidateSignalJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               [NoIndicatorStr, JunctionIndicatorStr, TheatreIndicatorStr]);
                END;
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_UpperRightIndicatorTargetFieldName THEN BEGIN
              TempStrArray[3] := NewKeyValue;
              Signal_JunctionIndicators[UpperRightIndicator] := ValidateSignalJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               [NoIndicatorStr, JunctionIndicatorStr, TheatreIndicatorStr]);
                END;
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_MiddleRightIndicatorTargetFieldName THEN BEGIN
              TempStrArray[4] := NewKeyValue;
              Signal_JunctionIndicators[MiddleRightIndicator] := ValidateSignalJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               [NoIndicatorStr, JunctionIndicatorStr, TheatreIndicatorStr]);
                END;
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_LowerRightIndicatorTargetFieldName THEN BEGIN
              TempStrArray[5] := NewKeyValue;
              Signal_JunctionIndicators[LowerRightIndicator] := ValidateSignalJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               [NoIndicatorStr, JunctionIndicatorStr, TheatreIndicatorStr]);
                END;
              END;
            END;
          END;

          JunctionIndicatorFound := False;
          FOR TempJunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
            IF Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_Exists THEN
              JunctionIndicatorFound := True;
          END; {FOR}

          IF JunctionIndicatorFound = False THEN
            IF Signal_Indicator = JunctionIndicator THEN
              Signal_Indicator := NoIndicator;

          IF ErrorMsg = '' THEN
            ValidateSignalJunctionIndicators2(TempStrArray, Signal_Indicator, Signal_JunctionIndicators, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_AdjacentLineFieldName THEN BEGIN
              Signal_AdjacentLine := ValidateSignalAdjacentLine(EditedSignal , NewKeyValue, ErrorMsg);
              IF Signal_AdjacentLine = UnknownLine THEN BEGIN
                Signal_AdjacentLine := SaveSignalRec.Signal_AdjacentLine;
                Values[KeyName] := LineToStr(SaveSignalRec.Signal_AdjacentLine);
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_AdjacentLineXOffsetFieldName THEN
              Signal_AdjacentLineXOffset := ValidateSignalAdjacentLineXOffset(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN
            IF KeyName = Signal_DirectionFieldName THEN
              Signal_Direction := ValidateSignalDirection(NewKeyValue, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Signal_IndicatorSpeedRestrictionFieldName THEN
              Signal_IndicatorSpeedRestriction := ValidateSignalIndicatorSpeedRestriction(NewKeyValue, Signal_Indicator, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_NextSignalIfNoIndicatorFieldName THEN BEGIN
              Signal_NextSignalIfNoIndicator := ValidateNextSignalIfNoIndicator(NewKeyValue, NOT Init, ErrorMsg);
              IF ErrorMsg = '' THEN
                ErrorMsg := ValidateSignalNum(Signal_NextSignalIfNoIndicator);
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_OppositePassingLoopSignalFieldName THEN BEGIN
              Signal_OppositePassingLoopSignal := ValidateSignalOppositePassingLoopSignal(NewKeyValue, NOT Init, ErrorMsg);
              IF ErrorMsg = '' THEN
                ErrorMsg := ValidateSignalNum(Signal_OppositePassingLoopSignal);
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_AsTheatreDestinationFieldName THEN
              Signal_AsTheatreDestination := ValidateSignalAsTheatreDestination(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_OutOfUseFieldName THEN
              Signal_OutOfUse := ValidateSignalOutOfUseAndAddAdjacentTC(StrToBool(NewKeyValue), Signal_AdjacentLine, Signal_AdjacentTC, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_NotesFieldName THEN
              Signal_Notes := NewKeyValue;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_NotUsedForRouteingFieldName THEN
              Signal_NotUsedForRouteing := StrToBool(NewKeyValue);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_AccessoryAddressFieldName THEN
              Signal_AccessoryAddress := ValidateSignalAccessoryAddress(NewKeyValue, Signal_Type, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_DecoderNumFieldName THEN
              Signal_DecoderNum := ValidateSignalDecoderNum(NewKeyValue, Signal_AccessoryAddress, Signal_Type, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_IndicatorDecoderNumFieldName THEN
              Signal_IndicatorDecoderNum := ValidateSignalIndicatorDecoderNum(NewKeyValue, Signal_AccessoryAddress, Signal_Type, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_IndicatorDecoderFunctionNumFieldName THEN
              Signal_IndicatorDecoderFunctionNum := ValidateSignalIndicatorDecoderFunctionNum(NewKeyValue, Signal_AccessoryAddress, Signal_Type, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_ApproachControlAspectFieldName THEN
              Signal_ApproachControlAspect := ValidateSignalApproachControlAspect(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_PossibleRouteHoldFieldName THEN
              Signal_PossibleRouteHold := StrToBool(NewKeyValue);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_PossibleStationStartRouteHoldFieldName THEN
              Signal_PossibleStationStartRouteHold := ValidateSignalPossibleStationStartRouteHold(StrToBool(NewKeyValue), Signal_PossibleRouteHold, ErrorMsg);
          END;

          { We don't need to validate Signal_LocationsToMonitorArray as the user hasn't been allowed to introduce errors to it }

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_AutomaticFieldName THEN
              Signal_Automatic := StrToBool(NewKeyValue); { not yet in use }
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_SemaphoreDistantHomesArrayFieldName THEN
              Signal_SemaphoreDistantHomesArray := ValidateSignalDistantHomesArray(EditedSignal, Signal_Type, UpperCase(NewKeyValue), ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                I := 0;
                WHILE (I <= High(Signal_SemaphoreDistantHomesArray)) AND (ErrorMsg = '') DO BEGIN
                  ErrorMsg := ValidateSignalNum(Signal_SemaphoreDistantHomesArray[I]);
                  Inc(I);
                END; {WHILE}
              END;
          END;

          IF ErrorMsg = '' THEN
            NoteThatDataHasChanged;

          IF ErrorMsg = '' THEN BEGIN
            { Redraw the screen to display the change}
            CalculateAllSignalPositions;
            InvalidateScreen(UnitRef, 'EditSaveButtonClick');
            Log('D Screen invalidated by Edit save');
          END ELSE BEGIN
            IF MessageDialogueWithDefault('Error in creating/amending S=' + IntToStr(EditedSignal) + ': '
                                          + ErrorMsg
                                          + CRLF
                                          + 'Do you wish to continue?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrYes
            THEN
              Signal_OutOfUse := True
            ELSE
              TurnEditModeOff;
          END;
        END; {WITH}
      END;

      IF EditedPoint <> UnknownPoint THEN BEGIN
        WITH Points[EditedPoint] DO BEGIN
          ErrorMsg := '';

          IF KeyName = Point_HeelLineFieldName THEN BEGIN
            Point_HeelLine := ValidatePointHeelLineName(NewKeyValue, ErrorMsg);
            IF ErrorMsg = '' THEN
              Point_TCAtHeel := Lines[Point_HeelLine].Line_TC;
            IF (ErrorMsg <> '') AND (Point_HeelLine = UnknownLine) THEN BEGIN
              Point_HeelLine := SavePointRec.Point_HeelLine;
              Values[KeyName] := LineToStr(SavePointRec.Point_HeelLine);
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Point_StraightLineFieldName THEN BEGIN
              Point_StraightLine := ValidatePointStraightLineName(NewKeyValue, ErrorMsg);
              IF (ErrorMsg <> '') AND (Point_StraightLine = UnknownLine) THEN BEGIN
                Point_StraightLine := SavePointRec.Point_StraightLine;
                Values[KeyName] := LineToStr(SavePointRec.Point_StraightLine);
              END;
            END;
          END;

          IF ErrorMsg = '' THEN
            IF KeyName = Point_TypeFieldName THEN
              Point_Type := ValidatePointType(NewKeyValue, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Point_DivergingLineFieldName THEN BEGIN
              Point_DivergingLine := ValidatePointDivergingLineName(NewKeyValue, Point_Type, ErrorMsg);
              IF (ErrorMsg <> '') AND (Point_DivergingLine = UnknownLine) THEN BEGIN
                Point_DivergingLine := SavePointRec.Point_DivergingLine;
                Values[KeyName] := LineToStr(SavePointRec.Point_DivergingLine);
              END;
            END;
          END;

          IF ErrorMsg = '' THEN
            IF KeyName = Point_OutOfUseFieldName THEN
              Point_OutOfUse := StrToBool(NewKeyValue);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_ManualOperationFieldName THEN
              Point_ManualOperation := StrToBool(NewKeyValue);

          IF ErrorMsg = '' THEN
            IF KeyName =  Point_LastFeedbackStateAsReadInFieldName THEN
              Point_LastFeedbackStateAsReadIn := ValidatePointLastFeedbackStateAsReadIn(NewKeyValue, Point_ManualOperation, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName =  Point_LastManualStateAsReadInFieldName THEN
              Point_LastManualStateAsReadIn := ValidatePointLastManualStateAsReadIn(NewKeyValue, Point_ManualOperation, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName =  Point_LenzNumFieldName THEN
              Point_LenzNum := ValidatePointLenzNum(NewKeyValue, Point_LastManualStateAsReadIn, Point_ManualOperation, Point_PresentState, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_LenzUnitFieldName THEN
              Point_LenzUnit := ValidatePointLenzUnit(NewKeyValue, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_LenzUnitTypeFieldName THEN
              Point_LenzUnitType := ValidatePointLenzUnitType(NewKeyValue, Point_ManualOperation, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_FeedbackUnitFieldName THEN
              Point_FeedbackUnit := ValidateFeedbackUnit(NewKeyValue, Point_HasFeedback, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_FeedbackInputFieldName THEN
              Point_FeedbackInput := ValidateFeedbackInput(NewKeyValue, Point_HasFeedback, Point_FeedbackUnit, PointFeedback, EditedPoint, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_FeedbackOnIsStraightFieldName THEN
              Point_FeedbackOnIsStraight := StrToBool(NewKeyValue);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_WiringReversedFlagFieldName THEN
              Point_WiringReversedFlag := StrToBool(NewKeyValue);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_RelatedPointFieldName THEN
              Point_RelatedPoint := ValidatePointRelatedPoint(EditedPoint, NewKeyValue, Point_Type, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_NotesFieldName THEN
              Point_Notes := NewKeyValue;

          IF ErrorMsg = '' THEN
            IF KeyName = Point_DefaultStateFieldName THEN
              Point_DefaultState := ValidatePointDefaultState(NewKeyValue, Point_HeelLine, Point_StraightLine, Point_DivergingLine, Point_PresentState, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_LockedIfHeelTCOccupiedFieldName THEN
              Point_LockedIfHeelTCOccupied := StrToBool(NewKeyValue);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_LockedIfNonHeelTCsOccupiedFieldName THEN
              Point_LockedIfNonHeelTCsOccupied := StrToBool(NewKeyValue);

          IF ErrorMsg = '' THEN BEGIN
            NoteThatDataHasChanged;

            { And redraw the screen to display the change}
            CalculatePointPositions;
            InvalidateScreen(UnitRef, 'EditSaveButtonClick');
            Log('D Screen invalidated by Edit save');
          END ELSE BEGIN
            IF MessageDialogueWithDefault('Error in creating/amending P=' + IntToStr(EditedPoint) + ': '
                                          + ErrorMsg
                                          + CRLF
                                          + 'Do you wish to continue editing?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrYes
            THEN
              Point_OutOfUse := True
            ELSE
              TurnEditModeOff;
          END;
        END; {WITH}
      END;

      IF EditedLine <> UnknownLine THEN BEGIN
        WITH Lines[EditedLine] DO BEGIN
          ErrorMsg := '';

          IF KeyName = Line_NameStrFieldName THEN
            Line_NameStr := ValidateLineName(NewKeyValue, EditedLine, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_GridUpXFieldName THEN
              Line_GridUpX := ValidateGridX(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_GridDownXFieldName THEN
              Line_GridDownX := ValidateGridX(NewKeyValue, ErrorMsg);
          END;

          { We don't need to validate Line_Location as the user hasn't been allowed to introduce errors to it }

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_TCFieldName THEN
              Line_TC := ValidateLineTrackCircuit(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_EndOfLineMarkerFieldName THEN
              Line_EndOfLineMarker := ValidateLineEndOfLineMarker(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_BufferStopTheatreDestinationStrFieldName THEN
              Line_BufferStopTheatreDestinationStr := NewKeyvalue;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_DirectionFieldName THEN
              Line_Direction := ValidateLineDirection(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_TypeOfLineFieldName THEN
              Line_TypeOfLine := ValidateLineType(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_UpConnectionChFieldName THEN
              Line_UpConnectionCh := ValidateLineConnectionCh(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_DownConnectionChFieldName THEN
              Line_DownConnectionCh := ValidateLineConnectionCh(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_OutOfUseFieldName THEN BEGIN
              IF StrToBool(NewKeyValue) = True THEN BEGIN
                Line_InitialOutOfUseState := OutOfUse; { this is used only in deciding whether to save a changed state on exit }
                Line_OutOfUseState := OutOfUse;
                Line_SaveOutOfUseState := OutOfUse;
              END ELSE BEGIN
                Line_InitialOutOfUseState := InUse;
                Line_OutOfUseState := InUse;
                Line_SaveOutOfUseState := InUse;
              END;

              { Even if the line itself is in use, that can be overridden by the location being out of use }
              IF Line_Location <> UnknownLocation THEN
                IF Locations[Line_Location].Location_OutOfUse THEN
                  Line_OutOfUseState := OutOfUse;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Line_InUseFeedbackUnitFieldName THEN
              Line_InUseFeedbackUnit := ValidateLineInUseFeedbackUnit(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            NoteThatDataHasChanged;

            { And redraw the screen to display the change}
            CalculateLinePositions;
            InvalidateScreen(UnitRef, 'EditSaveButtonClick');
            Log('D Screen invalidated by Edit save');
          END ELSE BEGIN
            IF MessageDialogueWithDefault('Error in creating/amending Line=' + IntToStr(EditedLine) + ': '
                                          + ErrorMsg
                                          + CRLF
                                          + 'Do you wish to continue editing?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrYes
            THEN
              WriteLineValuesToValueList
            ELSE
              TurnEditModeOff;
          END;
        END; {WITH}
      END;

      IF EditedTrackCircuit <> UnknownTrackCircuit THEN BEGIN
        WITH TrackCircuits[EditedTrackCircuit] DO BEGIN
          ErrorMsg := '';

          { ... }

          IF ErrorMsg = '' THEN BEGIN
            NoteThatDataHasChanged;

            { And redraw the screen to display the change}
//            CalculateTrackCircuitPositions;
            InvalidateScreen(UnitRef, 'EditSaveButtonClick');
            Log('D Screen invalidated by Edit save');
          END ELSE BEGIN
            IF MessageDialogueWithDefault('Error in creating/amending TC=' + IntToStr(EditedTrackCircuit) + ': '
                                          + ErrorMsg
                                          + CRLF
                                          + 'Do you wish to continue editing?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrYes
            THEN
//              TrackCircuit_OutOfUse := True
            ELSE
              TurnEditModeOff;
          END;
        END; {WITH}
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DoEditValidation: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DoEditValidation }

PROCEDURE TEditWindow.EditValueListEditorExit(Sender: TObject);
VAR
  DelimiterPos : Integer;
  KeyName : String;
  KeyValue : String;

BEGIN
  TRY
    { Do the validation for the item we are on when we exit the window here rather than use OnValidate, as OnValidate is not called when a change is made to a keyvalue and
      the Value List is immediately exited
    }
    IF (EditedPoint <> UnknownPoint) OR (EditedPoint <> UnknownPoint) OR (EditedLine <> UnknownLine) THEN BEGIN
      WITH EditValueListEditor DO BEGIN
        IF EditValueListEditor.Strings.Count > 0 THEN BEGIN
          DelimiterPos := FindDelimiter('=', EditValueListEditor.Strings[Row - 1], 1);
          KeyName := Copy(EditValueListEditor.Strings[Row - 1], 1, DelimiterPos - 1);
          KeyValue := Copy(EditValueListEditor.Strings[Row - 1], DelimiterPos + 1);
          DoEditValidation(KeyName, KeyValue);
        END;
      END; {FOR}
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG EditValueListEditorExit: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { EditValueListEditorExit }

PROCEDURE TEditWindow.EditValueListEditorValidate(Sender: TObject; ACol, ARow: Integer; CONST KeyName, KeyValue: String);
{ We don't need to validate pick-list values, as we have set them to be read only, thereby stopping any new ones being entered }
BEGIN
  TRY
    DoEditValidation(KeyName, KeyValue);
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG EditValueListEditorValidate: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { EditValueListEditorValidate }

PROCEDURE TEditWindow.EditWindowShow(Sender: TObject);
BEGIN
  TRY
    { Tag represents the signal number, and is set by whichever procedure sets the edit window to visible }
    CASE Tag OF
      1:
        WriteSignalValuesToValueList;
      2:
        WritePointValuesToValueList;
      3:
        WriteLineValuesToValueList;
      4:
        WriteTrackCircuitValuesToValueList;
    END; {CASE}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG EditWindowShow: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { EditWindowShow }

PROCEDURE TEditWindow.UndoChangesButtonClick(Sender: TObject);
BEGIN
  TRY
    UndoEditChanges;

    { And redraw the screen }
    InvalidateScreen(UnitRef, 'UndoChangesButtonClick');
    Log('D Screen invalidated by Edit Undo Changes');

    EditWindow.UndoChangesButton.Enabled := False;
    EditWindow.SaveChangesAndExitButton.Enabled := False;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG UndoChangesButtonClick: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { UndoChangesButtonClick }

PROCEDURE TEditWindow.SaveChangesAndExitButtonClick(Sender: TObject);
BEGIN
  TRY
    { Update the database }
    IF EditedSignal <> UnknownSignal THEN BEGIN
      WriteOutSignalDataToDatabase;
      CalculateTCAdjacentSignals;
      CalculateTCAdjacentBufferStops;
      CalculateAllSignalPositions;
    END;

    IF EditedPoint <> UnknownPoint THEN BEGIN
      WriteOutPointDataToDatabase;
      CalculatePointPositions;
    END;

    IF EditedLine <> UnknownLine THEN BEGIN
      WriteOutLineDataToDatabase;
      CalculateLinePositions;
    END;

    IF EditedTrackCircuit <> UnknownTrackCircuit THEN BEGIN
//      WriteOutTrackCircuitDataToDatabase;
//      CalculateTrackCircuitPositions;
    END;

    ClearEditValueListAndEditedItem;

    { And redraw the screen }
    CalculateTCAdjacentSignals;
    CalculateTCAdjacentBufferStops;
    CalculateAllSignalPositions;
    InvalidateScreen(UnitRef, 'EditSaveButtonClick');
    Log('D Screen invalidated by Edit Save And Exit');

    UndoChangesButton.Enabled := False;
    SaveChangesAndExitButton.Enabled := False;
    ExitWithoutSavingButton.Enabled := False;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG SaveChangesAndExitButtonClick: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SaveChangesAndExitButtonClick }

PROCEDURE TEditWindow.ExitWithoutSavingButtonClick(Sender: TObject);
BEGIN
  TRY
    UndoEditChanges;

    ExitWithoutSavingButton.Enabled := False;
    UndoChangesButton.Enabled := False;
    SaveChangesAndExitButton.Enabled := False;

    ClearEditValueListAndEditedItem;

    { And redraw the screen }
    InvalidateScreen(UnitRef, 'EditSaveButtonClick');
    Log('D Screen invalidated by Edit Without Saving');
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ExitWithSavingButtonClick: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ExitWithoutSavingButtonClick }

PROCEDURE ResetEditWindowSizeAndPosition;
{ Reset the window's size and position }
BEGIN
  EditWindowHeight := DefaultEditWindowHeight;
  EditWindowWidth := DefaultEditWindowWidth;
  EditWindowTop := DefaultEditWindowTop;
  EditWindowLeft := DefaultEditWindowLeft;

  EditWindow.Height := EditWindowHeight;
  EditWindow.Width := EditWindowWidth;
  EditWindow.Top := EditWindowTop;
  EditWindow.Left := EditWindowLeft;

  EditWindow.Visible := True;
  EditWindow.Invalidate;
END; { ResetEditWindowSizeAndPosition }

PROCEDURE TEditWindow.EditWindowPopupResetSizeAndPositionClick(Sender: TObject);
BEGIN
  ResetEditWindowSizeAndPosition;
END; { EditWindowPopupResetSizeAndPositionClick }

PROCEDURE TEditWindow.EditWindowPopupMenuPopup(Sender: TObject);
BEGIN
  IF EditWindow.Top <> DefaultEditWindowTop THEN
    PopupEditWindowResetSizeAndPosition.Enabled := True
  ELSE
    PopupEditWindowResetSizeAndPosition.Enabled := False;
END; { EditWindowPopupMenupPopup }

PROCEDURE TEditWindow.EditWindowResize(Sender: TObject);
BEGIN
  { Enable or disable the popup menu item allowing us to return the window to its default size }
  IF (EditWindow.Height <> DefaultEditWindowHeight)
  OR (EditWindow.Width <> DefaultEditWindowWidth)
  OR (EditWindow.Top <> DefaultEditWindowTop)
  OR (EditWindow.Left <> DefaultEditWindowLeft)
  THEN
    PopupEditWindowResetSizeAndPosition.Enabled := True
  ELSE
    PopupEditWindowResetSizeAndPosition.Enabled := False;
END; { EditWindowResize }

PROCEDURE CreateSignal(Direction : DirectionType; Line : Integer);
{ Creates a signal from scratch }
VAR
  ExistingSignalFoundAttachedToLine : Boolean;
  S : Integer;

BEGIN
  TRY
    IF Line = UnknownLine THEN
      { this shouldn't happen, but just in case it does... }
      ShowMessage('A new signal must be near an existing line')
    ELSE BEGIN
      { is there already a signal attached to this section of track - it's not advisable to have more than one }
      ExistingSignalFoundAttachedToLine := False;
      S := 0;
      WHILE (S <= High(Signals)) AND NOT ExistingSignalFoundAttachedToLine DO BEGIN
        IF Signals[S].Signal_AdjacentLine = Line THEN
          ExistingSignalFoundAttachedToLine := True
        ELSE
          Inc(S);
      END; {WHILE}

      IF ExistingSignalFoundAttachedToLine THEN
        ShowMessage('Cannot create a signal adjacent to line ' + LineToStr(Line) + ' as S=' + IntToStr(S) + ' is already marked as being adjacent to it')
      ELSE BEGIN
        { Now create and save a basic signal which must then be added to using the value list editor }
        SetLength(Signals, Length(Signals) + 1);
        EditedSignal := High(Signals);

        InitialiseSignalVariables(EditedSignal);
        WITH Signals[EditedSignal] DO BEGIN
          Signal_AdjacentLine := Line;
          Signal_Aspect := RedAspect;
          Signal_DataChanged := True;
          Signal_Direction := Direction;
          Signal_Notes := 'Created by user on ' + DateToStr(Date);
          Signal_Number := UnknownSignal;
        END; {WITH}

        NoteThatDataHasChanged;
        CalculateTCAdjacentSignals;
        CalculateTCAdjacentBufferStops;
        CalculateAllSignalPositions;
        AddNewRecordToSignalDatabase;
        WriteOutSignalDataToDatabase;

        SaveSignalRec := Signals[EditedSignal];
        WriteSignalValuesToValueList;

        InvalidateScreen(UnitRef, 'CreateSignal');
        Log('D Screen invalidated by CreateSignal');
      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CreateSignal: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CreateSignal }

PROCEDURE DeleteSignal(SignalToDeleteNum : Integer);
{ Delete a signal after appropriate checks }
VAR
  CanDelete : Boolean;
  JunctionIndicator : JunctionIndicatorType;
  OtherSignal : Integer;
  SignalPos : Integer;

BEGIN
  TRY
    { Ask for confirmation }
    IF MessageDialogueWithDefault('Delete signal ' + IntToStr(SignalToDeleteNum) + '?',
                                  StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
    THEN
      Debug('Signal ' + IntToStr(SignalToDeleteNum) + ' not deleted')
    ELSE BEGIN
      { We need to check if this signal is referred to elsewhere }
      OtherSignal := 0;
      CanDelete := True;
      WHILE OtherSignal <= High(Signals) DO BEGIN
        { Look at the individual fields that might need renumbering }
        IF Signals[OtherSignal].Signal_OppositePassingLoopSignal = SignalToDeleteNum THEN BEGIN
          IF MessageDialogueWithDefault('Cannot delete S=' + IntToStr(SignalToDeleteNum)
                                        + ' until it ceases to be the opposite passing loop signal for S=' + IntToStr(OtherSignal)
                                        + CRLF
                                        + 'Do you wish to remove the reference?',
                                        StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
          THEN
            CanDelete := False
          ELSE BEGIN
            { Remove the reference to this signal from the other signal's database entry }
            Signals[OtherSignal].Signal_OppositePassingLoopSignal := UnknownSignal;
            Signals[OtherSignal].Signal_DataChanged := True;
          END;
        END;

        IF CanDelete THEN BEGIN
          IF Signals[OtherSignal].Signal_NextSignalIfNoIndicator = SignalToDeleteNum THEN BEGIN
            IF MessageDialogueWithDefault('Cannot delete S' + IntToStr(SignalToDeleteNum)
                                          + ' until it ceases to be the next signal if no indicator for S' + IntToStr(OtherSignal)
                                          + CRLF
                                          + 'Do you wish to remove the reference?',
                                          StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
            THEN
              CanDelete := False
            ELSE BEGIN
              { Remove the reference to this signal from the other signal's database entry }
              Signals[OtherSignal].Signal_NextSignalIfNoIndicator := UnknownSignal;
              Signals[OtherSignal].Signal_DataChanged := True;
           END;
          END;
        END;

        IF CanDelete THEN BEGIN
          FOR JunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
            IF Signals[OtherSignal].Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_Exists THEN BEGIN
              IF Signals[OtherSignal].Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal = SignalToDeleteNum THEN BEGIN
                IF MessageDialogueWithDefault('Cannot delete S' + IntToStr(SignalToDeleteNum)
                                              + ' until it ceases to be the target signal for'
                                              + ' S' + IntToStr(OtherSignal) + '''s ' + JunctionIndicatorTypeToStr(JunctionIndicator)
                                              + CRLF
                                              + 'Do you wish to remove the reference?',
                                              StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
                THEN
                  CanDelete := False
                ELSE BEGIN
                  { Remove the reference to this signal from the other signal's database entry }
                  Signals[OtherSignal].Signal_Indicator := NoIndicator;
                  Signals[OtherSignal].Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_Exists := False;
                  Signals[OtherSignal].Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal := UnknownSignal;
                  Signals[OtherSignal].Signal_DataChanged := True;

                  { and notify if there is a consequential change to the signal's data - otherwise there will be an error when the signal data is reloaded }
                  IF Signals[OtherSignal].Signal_IndicatorSpeedRestriction <> NoSpecifiedSpeed THEN
                    Signals[OtherSignal].Signal_IndicatorSpeedRestriction := NoSpecifiedSpeed;
                END;
              END;
            END;
          END; {FOR}

          IF CanDelete THEN BEGIN
            SignalPos := GetElementPosInIntegerArray(Signals[OtherSignal].Signal_SemaphoreDistantHomesArray, SignalToDeleteNum);
            IF SignalPos > -1 THEN BEGIN
              IF MessageDialogueWithDefault('Cannot delete S' + IntToStr(SignalToDeleteNum)
                                            + ' until it ceases to be a home signal in S' + IntToStr(OtherSignal) + '''s DistantHomesArray'
                                            + CRLF
                                            + 'Do you wish to remove the reference?',
                                            StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
              THEN
                CanDelete := False
              ELSE BEGIN
                { Remove the reference to this signal from the other signal's database entry }
                DeleteElementFromIntegerArray(Signals[OtherSignal].Signal_SemaphoreDistantHomesArray, SignalPos);
                Signals[OtherSignal].Signal_DataChanged := True;
              END;
            END;
          END;
        END;

        IF Signals[OtherSignal].Signal_DataChanged THEN
          WriteOutSignalDataToDatabase;

        Inc(OtherSignal);
      END;

      IF CanDelete THEN BEGIN
        IF NOT DeleteRecordFromSignalDatabase(SignalToDeleteNum) THEN BEGIN
          Log('S! Cannot delete S=' + IntToStr(SignalToDeleteNum));

          { and restore the variables }
          Signals[SignalToDeleteNum].Signal_NextSignalIfNoIndicator := SaveSignalRec.Signal_NextSignalIfNoIndicator;
          Signals[SignalToDeleteNum].Signal_OppositePassingLoopSignal := SaveSignalRec.Signal_OppositePassingLoopSignal;
          Signals[SignalToDeleteNum].Signal_Indicator := SaveSignalRec.Signal_Indicator;
          FOR JunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
            Signals[SignalToDeleteNum].Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_Exists :=
                                                                                        SaveSignalRec.Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_Exists;
            Signals[SignalToDeleteNum].Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal :=
                                                                                  SaveSignalRec.Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal;
          END; {FOR}
          Signals[SignalToDeleteNum].Signal_IndicatorSpeedRestriction := SaveSignalRec.Signal_IndicatorSpeedRestriction;

          FOR SignalPos := 0 TO High(Signals[SignalToDeleteNum].Signal_SemaphoreDistantHomesArray) DO
            Signals[SignalToDeleteNum].Signal_SemaphoreDistantHomesArray[SignalPos] := SaveSignalRec.Signal_SemaphoreDistantHomesArray[SignalPos];

          Signals[SignalToDeleteNum].Signal_DataChanged := True;
          WriteOutSignalDataToDatabase;
        END ELSE BEGIN
          { Now we need to renumber signal references in the database that have changed because of the deletion }
          FOR OtherSignal := 0 TO High(Signals) DO BEGIN
            IF Signals[OtherSignal].Signal_OppositePassingLoopSignal = High(Signals) THEN BEGIN
              Signals[OtherSignal].Signal_OppositePassingLoopSignal := SignalToDeleteNum;
              Signals[OtherSignal].Signal_DataChanged := True;
            END;

            IF Signals[OtherSignal].Signal_NextSignalIfNoIndicator = High(Signals) THEN BEGIN
              Signals[OtherSignal].Signal_NextSignalIfNoIndicator := SignalToDeleteNum;
              Signals[OtherSignal].Signal_DataChanged := True;
            END;

            FOR JunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
              IF Signals[OtherSignal].Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_Exists THEN BEGIN
                IF Signals[OtherSignal].Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal = High(Signals) THEN BEGIN
                  Signals[OtherSignal].Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal := SignalToDeleteNum;
                  Signals[OtherSignal].Signal_DataChanged := True;
                END;
              END;
            END; {FOR}

            SignalPos := GetElementPosInIntegerArray(Signals[OtherSignal].Signal_SemaphoreDistantHomesArray, High(Signals));
            IF SignalPos > -1 THEN BEGIN
              Signals[OtherSignal].Signal_SemaphoreDistantHomesArray[SignalPos] := SignalToDeleteNum;
              Signals[OtherSignal].Signal_DataChanged := True;
            END;

            IF Signals[OtherSignal].Signal_DataChanged THEN
              WriteOutSignalDataToDatabase;
          END; {FOR}

          { Renumber the last entry so it is the same as the record already deleted (a DJW suggestion of 3/9/14), and also renumber any signal records that point to it }
          Signals[High(Signals)].Signal_Number := SignalToDeleteNum;
          Signals[High(Signals)].Signal_DataChanged := True;
          WriteOutSignalDataToDatabase;

          { Clear the data from the value-list editor }
          WITH EditWindow DO BEGIN
            ClearEditValueListAndEditedItem;
            UndoChangesButton.Enabled := False;
            SaveChangesAndExitButton.Enabled := False;
            ExitWithoutSavingButton.Enabled := False;
          END; {WITH}

          { Reload all the signals }
          ReadInSignalDataFromDatabase(NewData);

          CalculateTCAdjacentSignals;
          CalculateTCAdjacentBufferStops;
          CalculateAllSignalPositions;
          InvalidateScreen(UnitRef, 'DeleteSignal');
          Log('D Screen invalidated by Delete Signal');
        END;
      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DeleteSignal: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DeleteSignal }

PROCEDURE CreatePoint(LineArray : IntegerArrayType; TempPointType : TypeOfPoint; X, Y : Integer);
{ Creates a point from scratch }
VAR
  CommonGridX : Integer;
  CommonGridY : Integer;
  DivergingLine : Integer;
  HeelLine : Integer;
  Line : Integer;
  LineFound : Boolean;
  RelatedPoint : Integer;
  StraightLine : Integer;

BEGIN
  TRY
    { Create  basic point which must then be added to using the value list editor }
    SetLength(Points, Length(Points) + 1);
    EditedPoint := High(Points);

    StraightLine := UnknownLine;
    DivergingLine := UnknownLine;
    HeelLine := UnknownLine;
    RelatedPoint := UnknownPoint;
    LineFound := False;

    IF (TempPointType = CatchPointUp) OR (TempPointType = CatchPointDown) THEN BEGIN
      HeelLine := LineArray[0];
      IF PointInPolygon(Lines[LineArray[0]].Line_UpHandlePolygon, Point(X, Y)) THEN BEGIN
        { Now find the straight line }
        Line := 0;
        WHILE (Line <= High(Lines)) AND NOT LineFound DO BEGIN
          IF (Lines[LineArray[0]].Line_GridUpX = Lines[Line].Line_GridDownX)
          AND (Lines[LineArray[0]].Line_GridUpY = Lines[Line].Line_GridDownY)
          THEN BEGIN
            LineFound := True;
            StraightLine := Line;
          END;
          Inc(Line);
        END; {WHILE}
      END ELSE
        IF PointInPolygon(Lines[LineArray[0]].Line_DownHandlePolygon, Point(X, Y)) THEN BEGIN
          { Now find the straight line }
          Line := 0;
          WHILE (Line <= High(Lines)) AND NOT LineFound DO BEGIN
            IF (Lines[LineArray[0]].Line_GridDownX = Lines[Line].Line_GridUpX)
            AND (Lines[LineArray[0]].Line_GridUpY = Lines[Line].Line_GridDownY)
            THEN BEGIN
              LineFound := True;
              StraightLine := Line;
            END;
            Inc(Line);
          END; {WHILE}
        END;

      IF NOT LineFound THEN BEGIN
        SetLength(Points, Length(Points) - 1);
        Exit;
      END;

      { Now take a guess at which point the catch point is protecting }
      IF TempPointType = CatchPointUp THEN
        FindNextPoint(Lines[StraightLine].Line_TC, Up, RelatedPoint)
      ELSE
        IF TempPointType = CatchPointDown THEN
          FindNextPoint(Lines[StraightLine].Line_TC, Down, RelatedPoint);

      IF RelatedPoint <> UnknownPoint THEN BEGIN
        { add the catch point to the related point's "Other Point" field }
        Points[RelatedPoint].Point_Type := ProtectedPoint;
        Points[RelatedPoint].Point_RelatedPoint := Editedpoint;
        Points[RelatedPoint].Point_DataChanged := True;
        WriteOutPointDataToDatabase;
      END;
    END ELSE BEGIN
      { now we can work out the lines: if two Up Xs and Ys are the same, see which DownY is the same as the UpY - straight - and which is greater or less - diverging }
      CommonGridX := 0;
      CommonGridY := 0;

      IF (Lines[LineArray[0]].Line_GridUpX = Lines[LineArray[1]].Line_GridUpX)
      AND (Lines[LineArray[0]].Line_GridUpY = Lines[LineArray[1]].Line_GridUpY)
      THEN BEGIN
        CommonGridX := Lines[LineArray[0]].Line_GridUpX;
        CommonGridY := Lines[LineArray[0]].Line_GridUpY;
        IF (Lines[LineArray[0]].Line_GridUpY = Lines[LineArray[0]].Line_GridDownY) THEN BEGIN
          StraightLine := LineArray[0];
          DivergingLine := LineArray[1];
        END ELSE BEGIN
          StraightLine := LineArray[1];
          DivergingLine := LineArray[0];
        END;
      END ELSE
        IF (Lines[LineArray[0]].Line_GridDownX = Lines[LineArray[1]].Line_GridDownX)
        AND (Lines[LineArray[0]].Line_GridDownY = Lines[LineArray[1]].Line_GridDownY)
        THEN BEGIN
          CommonGridX := Lines[LineArray[0]].Line_GridDownX;
          CommonGridY := Lines[LineArray[0]].Line_GridDownY;
          IF (Lines[LineArray[0]].Line_GridUpY = Lines[LineArray[0]].Line_GridDownY) THEN BEGIN
            StraightLine := LineArray[0];
            DivergingLine := LineArray[1];
          END ELSE BEGIN
            StraightLine := LineArray[1];
            DivergingLine := LineArray[0];
          END;
        END;

      { Now we can find the heel line }
      Line := 0;
      LineFound := False;
      WHILE (Line <= High(Lines)) AND NOT LineFound DO BEGIN
        IF (Line <> LineArray[0]) AND (Line <> LineArray[1]) THEN BEGIN
          IF ((Lines[Line].Line_GridUpX = CommonGridX) AND (Lines[Line].Line_GridUpY = CommonGridY))
          OR ((Lines[Line].Line_GridDownX = CommonGridX) AND (Lines[Line].Line_GridDownY = CommonGridY))
          THEN BEGIN
            LineFound := True;
            HeelLine := Line;
          END;
        END;

        Inc(Line);
      END; {WHILE}
    END;

    InitialisePointVariables(EditedPoint);

    WITH Points[EditedPoint] DO BEGIN
      Point_DivergingLine := DivergingLine;
      Point_HeelLine := HeelLine;
      Point_StraightLine := StraightLine;
      Point_DataChanged := True;
      Point_DefaultState := PointStateUnknown;

      Point_FacingDirection := UnknownDirection;
      Point_FarX := 0;
      Point_FarY := 0;
      Point_FeedbackOnIsStraight := False;
      Point_FeedbackStartTime := 0;
      Point_FeedbackUnit := 0;
      Point_FeedbackInput := 0;
      Point_HasFeedback := False;
      Point_LastFeedbackStateAsReadIn := PointStateUnknown;
      Point_LastManualStateAsReadIn := PointStateUnknown;
      Point_LenzNum := 0;
      Point_LenzUnit := 0;
      Point_LenzUnitType := '';
      Point_LockedIfHeelTCOccupied := False;
      Point_LockedIfNonHeelTCsOccupied := False;
      Point_ManualOperation := False;
      Point_Notes := 'Created by user on ' + DateToStr(Date);
      Point_Number := EditedPoint;
      Point_OutOfUse := True;
      Point_PreviousState := PointStateUnknown;
      Point_RelatedPoint := RelatedPoint;
      Point_TCAtHeel := 0;
      Point_Type := TempPointType;
      Point_WiringReversedFlag := False;
      Point_X := 0;
      Point_Y := 0;
    END; {WITH}

    NoteThatDataHasChanged;
    AddNewRecordToPointDatabase;
    WriteOutPointDataToDatabase;

    SavePointRec := Points[EditedPoint];
    WritePointValuesToValueList;

    CalculatePointPositions;

    { and we also need to tell the appropriate lines that there is now a point attached to them }
    CalculateLinePositions;
    InvalidateScreen(UnitRef, 'CreatePoint');
    Log('D Screen invalidated by CreatePoint');

    CASE TempPointType OF
      CatchPointUp, CatchPointDown, ThreeWayPointA, ThreeWayPointB, CrossOverPoint:
        IF Points[EditedPoint].Point_RelatedPoint = UnknownPoint THEN
          ShowMessage('Now add the related point')
        ELSE
          ShowMessage('Related Point has been set to be P' + IntToStr(Points[EditedPoint].Point_RelatedPoint) + ' - please check this is the case');
    END; {CASE}

  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CreatePoint: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CreatePoint }

PROCEDURE DeletePoint(PointToDeleteNum : Integer);
{ Delete a point after appropriate checks }
CONST
  NewPointData = True;

VAR
  CanDelete : Boolean;
  OtherPoint : Integer;

BEGIN
  TRY
    { Ask for confirmation }
    CanDelete := True;
    IF MessageDialogueWithDefault('Delete Point ' + IntToStr(PointToDeleteNum) + '?',
                                  StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
    THEN BEGIN
      Debug('Point ' + IntToStr(PointToDeleteNum) + ' not deleted');
      CanDelete := False;
    END ELSE BEGIN
      { We need to check if this point is referred to elsewhere }
      OtherPoint := 0;
      WHILE OtherPoint <= High(Points) DO BEGIN
        IF PointToDeleteNum = Points[OtherPoint].Point_RelatedPoint THEN BEGIN
          IF MessageDialogueWithDefault('Cannot delete P=' + IntToStr(PointToDeleteNum) + ' until it ceases to be the "Related Point" for P=' + IntToStr(OtherPoint)
                                        + CRLF
                                        + 'and we change the other point''s type from ' + PointTypeToStr(Points[OtherPoint].Point_Type) + ' to "Ordinary Point"'
                                        + CRLF
                                        + 'Do you wish to remove the reference and change the other point''s type?',
                                        StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
          THEN
            CanDelete := False
          ELSE BEGIN
            Points[OtherPoint].Point_Type := OrdinaryPoint;
            Points[OtherPoint].Point_RelatedPoint := UnknownPoint;
            Points[OtherPoint].Point_DataChanged := True;

            WriteOutPointDataToDatabase;
          END;
        END;
        Inc(OtherPoint);
      END; {WHILE}
    END;

    IF CanDelete THEN
      DeleteRecordFromPointDatabase(PointToDeleteNum);

    IF CanDelete THEN BEGIN
      WITH InitVarsWindow DO BEGIN
        { Now we need to renumber point references in the database that have changed because of the deletion }
        FOR OtherPoint := 0 TO High(Points) DO BEGIN
          IF Points[OtherPoint].Point_RelatedPoint = High(Points) THEN BEGIN
            Points[OtherPoint].Point_RelatedPoint := PointToDeleteNum;
            Points[OtherPoint].Point_DataChanged := True;

            WriteOutPointDataToDatabase;
          END;
        END; {FOR}

        { Renumber the last entry so it is the same as the record already deleted (a DJW suggestion of 3/9/14), and also renumber any point records that point to it }
        Points[High(Points)].Point_Number := PointToDeleteNum;
        Points[High(Points)].Point_DataChanged := True;
        WriteOutPointDataToDatabase;

        { Clear the data from the value-list editor }
        WITH EditWindow DO BEGIN
          ClearEditValueListAndEditedItem;
          UndoChangesButton.Enabled := False;
          SaveChangesAndExitButton.Enabled := False;
          ExitWithoutSavingButton.Enabled := False;
        END; {WITH}

        { Reload all the points }
        ReadInPointDataFromDatabase;
        IF PreviousPointSettingsMode THEN
          LoadPreviousPointSettings;
        InvalidateScreen(UnitRef, 'DeletePoint');
        Log('D Screen invalidated by DeletePoint');
      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DeletePoint: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DeletePoint }

FUNCTION IsNearRow(Y : Integer) : Integer;
{ Returns the row number if the Y position is close to it }
VAR
  Row : Integer;
  RowFound : Boolean;

BEGIN
  Result := UnknownRow;

  Row := 1;
  RowFound := False;
  WHILE (Row <= WindowRows) AND NOT RowFound DO BEGIN
    IF (Row * InterLineSpacing > (Y - MulDiv(FWPRailWindow.ClientHeight, 5, 1000))) AND (Row * InterLineSpacing < (Y + MulDiv(FWPRailWindow.ClientHeight, 5, 1000))) THEN
      RowFound := True
    ELSE
      IF (((Row * InterLineSpacing) + InterLineSpacing / 0.25) > (Y - MulDiv(FWPRailWindow.ClientHeight, 5, 1000))) AND (Row * InterLineSpacing < (Y + MulDiv(FWPRailWindow.ClientHeight, 5, 1000))) THEN
        RowFound := True;
    Inc(Row);
  END; {WHILE}

  IF RowFound THEN
    Result := Row;

  Debug('row=' + inttostr(row));
END; { IsNearRow }

FUNCTION GetNearestLine(X, Y : Integer) : Integer;
{ Returns the line number nearest to a given position if the line is horizontal and the position is within a line's mouse rectangle }
VAR
  Line : Integer;
  LineFound : Boolean;

BEGIN
  Result := UnknownLine;
  TRY
    LineFound := False;
    Line := 0;
    WHILE (Line <= High(Lines)) AND NOT LineFound DO BEGIN
      IF (Lines[Line].Line_GridUpY = Lines[Line].Line_GridDownY)
      AND PointInPolygon(Lines[Line].Line_MousePolygon, Point(X, Y))
      THEN
        LineFound := True
      ELSE
        Inc(Line);
    END; {WHILE}

    IF LineFound THEN
      Result := Line;
  EXCEPT
    ON E : Exception DO
      Log('EG GetNearestLine:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { GetNearestLine }

FUNCTION TooNearOtherSignal(S, NearestLine : Integer) : Integer;
{ Returns true if the signal we're dragging/dropping is too close to another signal }
VAR
  OtherSignal1 : Integer;
  OtherSignalFound : Boolean;

BEGIN
  Result := UnknownSignal;
  TRY
    OtherSignalFound := False;
    OtherSignal1 := 0;
    WHILE (OtherSignal1 <= High(Signals)) AND NOT OtherSignalFound DO BEGIN
      IF (OtherSignal1 <> S) AND PtInRect(Signals[OtherSignal1].Signal_MouseRect, Point(Signals[S].Signal_LineX, Signals[S].Signal_LineWithVerticalSpacingY))
      OR (Signals[OtherSignal1].Signal_AdjacentLine = NearestLine)
      THEN
        OtherSignalFound := True
      ELSE
        Inc(OtherSignal1);
    END; {WHILE}

    IF OtherSignalFound THEN
      Result := OtherSignal1;
  EXCEPT
    ON E : Exception DO
      Log('EG TooNearOtherSignal:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TooNearOtherSignal }

PROCEDURE DragSignal(S, MouseX, MouseY : Integer; OUT NearestLineToSignal, TooNearSignal : Integer);
{ Allows a signal to be moved by the mouse }
BEGIN
  TRY
    NearestLineToSignal := UnknownLine;
    TooNearSignal := UnknownLine;

    IF (EditedSignal <> UnknownSignal) AND SignalDragging THEN BEGIN
      WITH Signals[EditedSignal] DO BEGIN
        Signal_LineX := MouseX;
        Signal_LineWithVerticalSpacingY := MouseY;
        IF Signal_Direction = Up THEN
          Signal_LineY := MouseY - SignalVerticalSpacingScaled + ScrollBarYAdjustment
        ELSE
          Signal_LineY := MouseY + SignalVerticalSpacingScaled - ScrollBarYAdjustment;

        { Indicate whether or not it can be dropped - first check the signal's X position }
        NearestLineToSignal := GetNearestLine(Signals[S].Signal_LineX, Signals[S].Signal_LineWithVerticalSpacingY);
        IF NearestLineToSignal = UnknownLine THEN
          { see if the base of the signal post is within the polygon }
          NearestLineToSignal := GetNearestLine(Signals[S].Signal_LineX, Signals[S].Signal_LineY);

        TooNearSignal := TooNearOtherSignal(EditedSignal, NearestLineToSignal);
      END; {WITH}

      DrawSignal(EditedSignal);

//      IF (NearestLineToSignal = UnknownLine) OR (TooNearSignal <> UnknownLine) THEN
//        ChangeCursor(crNoDrop)
//      ELSE
//        ChangeCursor(crDrag);

      FWPRailWindow.Repaint;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DragSignal:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DragSignal }

PROCEDURE DropSignal;
{ Drops the signal at the nearest adjacent line }
VAR
  NearestLineToSignal : Integer;
  NewSignalX : Integer;
  NewSignalY : Integer;
  TooNearSignal : Integer;

BEGIN
  TRY
    SignalDragging := False;
    ChangeCursor(crDefault);

    WITH Signals[EditedSignal] DO BEGIN
      { See if the signal is within a line polygon }
      NearestLineToSignal := GetNearestLine(Signals[EditedSignal].Signal_LineX, Signals[EditedSignal].Signal_LineWithVerticalSpacingY);

      IF NearestLineToSignal = UnknownSignal THEN BEGIN
        { put the signal back whence it came }
        Debug('!Cannot find a line anywhere near the dragged signal');
        Signal_LineX := SaveDragX;
        Signal_LineWithVerticalSpacingY := SaveDragY;
        CalculateSignalPosition(EditedSignal);
        FWPRailWindow.Repaint;
      END ELSE BEGIN
        { Is another one at the exact spot? Record where it is going to be placed to carry out this test. }
        IF Signal_Direction = Up THEN
          NewSignalX := MapGridXToScreenX(Lines[NearestLineToSignal].Line_GridUpX) + SignalRadiusScaled
        ELSE
          { Down }
          NewSignalX := MapGridXToScreenX(Lines[NearestLineToSignal].Line_GridDownX) - SignalRadiusScaled;

        NewSignalY := MapGridYToScreenY(Lines[NearestLineToSignal].Line_GridUpY);

        { See whether we're trying to drop the signal over another signal }
        TooNearSignal := TooNearOtherSignal(EditedSignal, NearestLineToSignal);
        IF TooNearSignal <> UnknownSignal THEN BEGIN
          { put the newly-moved signal back whence it came }
          Debug('!Cannot drop the signal where there is an existing signal (S=' + IntToStr(TooNearSignal) +')');
          Signal_LineX := SaveDragX;
          Signal_LineWithVerticalSpacingY := SaveDragY;
          CalculateSignalPosition(EditedSignal);
          FWPRailWindow.Repaint;
        END ELSE BEGIN
          { it's ok to place the signal here }
          Signal_PreviousLineX := NewSignalX;
          Signal_PreviousLineY := NewSignalY;
          Signal_PreviousLineWithVerticalSpacingY := Signal_LineWithVerticalSpacingY;

          Signal_AdjacentLine := NearestLineToSignal;
          Signal_AdjacentLineXOffset := 0;
          CalculateSignalPosition(EditedSignal);

          NoteThatDataHasChanged;

          DrawSignal(EditedSignal);

          UpdateSignalValueList(EditedSignal, Signal_AdjacentLine, Signal_AdjacentLineXOffset, UnknownDirection);
          FWPRailWindow.Repaint;
        END;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DropSignal:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DropSignal }

PROCEDURE MoveObjectLeft;
{ Moves whatever is currently being edited to the left }
BEGIN
  IF EditedSignal <> UnknownSignal THEN BEGIN
    WITH Signals[EditedSignal] DO BEGIN
      NoteThatDataHasChanged;

      Dec(Signals[EditedSignal].Signal_AdjacentLineXOffset);

      CalculateSignalPosition(EditedSignal);

      UpdateSignalValueList(EditedSignal, Signal_AdjacentLine, Signal_AdjacentLineXOffset, UnknownDirection);
      FWPRailWindow.Repaint;
    END; {WITH}
  END;
END; { MoveObjectLeft }

PROCEDURE MoveObjectRight;
{ Moves whatever is currently being edited to the right }
BEGIN
  IF EditedSignal <> UnknownSignal THEN BEGIN
    WITH Signals[EditedSignal] DO BEGIN
      NoteThatDataHasChanged;

      Inc(Signals[EditedSignal].Signal_AdjacentLineXOffset);

      CalculateSignalPosition(EditedSignal);

      UpdateSignalValueList(EditedSignal, Signal_AdjacentLine, Signal_AdjacentLineXOffset, UnknownDirection);
      FWPRailWindow.Repaint;
    END; {WITH}
  END;
END; { MoveObjectRight }

PROCEDURE ChangeSignalDirection(S : Integer);
{ Change a selected signal's direction }
BEGIN
  StartSignalEdit(S);
  NoteThatDataHasChanged;

  WITH Signals[S] DO BEGIN
    IF Signal_Direction = Up THEN
      Signal_Direction := Down
    ELSE
      Signal_Direction := Up;

    CalculateSignalPosition(EditedSignal);
    UpdateSignalValueList(EditedSignal, UnknownLine, 0, Signal_Direction);
  END; {WITH}

  FWPRailWindow.Repaint;
END; { ChangeSignalDirection }

FUNCTION CheckIfAnyEditedDataHasChanged : Boolean;
{ Ask whether we want to save amended data (if any) before selecting another signal }
BEGIN
  Result := False;

  IF EditedSignal <> UnknownSignal THEN BEGIN
    IF Signals[EditedSignal].Signal_DataChanged THEN BEGIN
      IF MessageDialogueWithDefault('You have made changes to S' + IntToStr(EditedSignal) + '.'
                                    + CRLF
                                    + 'Do you want to save those changes?',
                                    StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
      THEN BEGIN
        Result := True;
        WriteOutSignalDataToDatabase
      END ELSE
        { We'd better undo them, then }
        UndoEditChanges;

      EditedSignal := UnknownSignal;
    END;
  END;

  IF EditedPoint <> UnknownPoint THEN BEGIN
    IF Points[EditedPoint].Point_DataChanged THEN BEGIN
      IF MessageDialogueWithDefault('You have made changes to P' + IntToStr(EditedPoint) + '.'
                                    + CRLF
                                    + 'Do you want to save those changes?',
                                    StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
      THEN BEGIN
        Result := True;
        WriteOutPointDataToDatabase;
      END ELSE
        { We'd better undo them, then }
        UndoEditChanges;

      EditedPoint := UnknownPoint;
    END;
  END;

  IF EditedLine <> UnknownLine THEN BEGIN
    IF Lines[EditedLine].Line_DataChanged THEN BEGIN
      IF MessageDialogueWithDefault('You have made changes to Line ' + IntToStr(EditedLine) + '.'
                                    + CRLF
                                    + 'Do you want to save those changes?',
                                    StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
      THEN BEGIN
        Result := True;
        WriteOutLineDataToDatabase;
      END ELSE
        { We'd better undo them, then }
        UndoEditChanges;

      DeselectLine;
    END;
  END;

  IF EditedTrackCircuit <> UnknownTrackCircuit THEN BEGIN
    IF TrackCircuits[EditedTrackCircuit].TC_DataChanged THEN BEGIN
      IF MessageDialogueWithDefault('You have made changes to TC' + IntToStr(EditedTrackCircuit) + '.'
                                    + CRLF
                                    + 'Do you want to save those changes?',
                                    StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
      THEN BEGIN
        Result := True;
//        WriteOutTrackCircuitDataToDatabase;
      END ELSE
        { We'd better undo them, then }
        UndoEditChanges;

      EditedTrackCircuit := UnknownTrackCircuit;
    END;
  END;
END; { CheckIfAnyEditedDataHasChanged }

PROCEDURE UndoEditChanges;
{ Undo any changes made by moving the item on screen or editing it in the value list editor }
BEGIN
  IF EditedSignal <> UnknownSignal THEN BEGIN
    Signals[EditedSignal] := SaveSignalRec;
    Signals[EditedSignal].Signal_DataChanged := False;
    WriteSignalValuesToValueList;
  END ELSE
    IF EditedPoint <> UnknownPoint THEN BEGIN
      Points[EditedPoint] := SavePointRec;
      Points[EditedPoint].Point_DataChanged := False;
      WritePointValuesToValueList;
    END ELSE
      IF EditedLine <> UnknownLine THEN BEGIN
        Lines[EditedLine] := SaveLineRec;
        Lines[EditedLine].Line_DataChanged := False;
        WriteLineValuesToValueList;
      END ELSE
        IF EditedTrackCircuit <> UnknownTrackCircuit THEN BEGIN
          TrackCircuits[EditedTrackCircuit] := SaveTrackCircuitRec;
          TrackCircuits[EditedTrackCircuit].TC_DataChanged := False;
          WriteTrackCircuitValuesToValueList;
        END;

  WITH EditWindow DO BEGIN
    UndoChangesButton.Enabled := False;
    SaveChangesAndExitButton.Enabled := False;
    ExitWithoutSavingButton.Enabled := False;
  END; {WITH}
END; { UndoEditChanges }

PROCEDURE StartLineEdit(Line : Integer);
{ Set up a line edit - this is where we save the line's original state so we can revert to it regardless of how many edits there are }
CONST
  DrawHandles = True;

VAR
  SaveEditedLine : Integer;
  TempLine : Integer;

BEGIN
  SaveEditedLine := EditedLine;
  ClearEditValueListAndEditedItem;

  { If the line was already being edited, it has now been deselected by ClearEditValueListAndEditedItem, otherwise begin edit it }
  IF Line <> SaveEditedLine THEN BEGIN
    ClearEditValueListAndEditedItem;
    EditedLine := Line;
    SaveLineRec := Lines[EditedLine];
    SaveLineNum := EditedLine;
    WriteLineValuesToValueList;

    IF CreateLineMode THEN BEGIN
      { first remove any other editing handles }
      FOR TempLine := 0 TO High(Lines) DO
        Lines[TempLine].Line_ShowHandles := False;

      { now add them for the current edited line }
      Lines[EditedLine].Line_ShowHandles := True;
    END;
  END;
END; { StartLineEdit }

PROCEDURE StartPointEdit(P : Integer);
{ Set up a point edit - this is where we save the signal's original state so we can revert to it regardless of how many edits there are }
VAR
  SaveEditedPoint : Integer;

BEGIN
  SaveEditedPoint := EditedPoint;
  ClearEditValueListAndEditedItem;

  { If the point was already being edited, it has now been deselected by ClearEditValueListAndEditedItem, otherwise begin edit it }
  IF P <> SaveEditedPoint THEN BEGIN
    EditedPoint := P;
    SavePointRec := Points[EditedPoint];
    SavePointNum := EditedPoint;
    WritePointValuesToValueList;
  END;
END; { StartPointEdit }

PROCEDURE StartSignalEdit(S : Integer);
{ Set up a signal edit - this is where we save the signal's original state so we can revert to it regardless of how many edits there are }
VAR
  SaveEditedSignal : Integer;

BEGIN
  SaveEditedSignal := EditedSignal;
  ClearEditValueListAndEditedItem;

  { If the signal was already being edited, it has now been deselected by ClearEditValueListAndEditedItem, otherwise begin edit it }
  IF S <> SaveEditedSignal THEN BEGIN
    EditedSignal := S;
    SaveSignalRec := Signals[EditedSignal];
    SaveSignalNum := EditedSignal;
    WriteSignalValuesToValueList;
  END;
END; { StartSignalEdit }

PROCEDURE StartTrackCircuitEdit(TC : Integer);
{ Set up a track-circuit edit - this is where we save the track circuit's original state so we can revert to it regardless of how many edits there are }
VAR
  SaveEditedTrackCircuit : Integer;

BEGIN
  SaveEditedTrackCircuit := EditedTrackCircuit;
  ClearEditValueListAndEditedItem;

  { If the track circuit was already being edited, it has now been deselected by ClearEditValueListAndEditedItem, otherwise begin edit it }
  IF TC <> SaveEditedTrackCircuit THEN BEGIN
    EditedTrackCircuit := TC;
    SaveTrackCircuitRec := TrackCircuits[EditedTrackCircuit];
    SaveTrackCircuitNum := EditedTrackCircuit;
    WriteTrackCircuitValuesToValueList;
  END;
END; { StartTrackCircuitEdit }

PROCEDURE AddTrackCircuit(Line : Integer; NewTrackCircuit : Boolean);
{ Add a track circuit to a line }
VAR
  AddTrackCircuitCancelled : Boolean;
  FeedbackDataOK : Boolean;
  NewExtendedValue : Extended;
  NewIntValue : Integer;
  TC : Integer;

BEGIN
  AddTrackCircuitCancelled := False;
  TC := UnknownTrackCircuit;

  IF NOT NewTrackCircuit THEN BEGIN
    IF NOT GetIntegerDataFromUser('Enter existing track-circuit number', '', 0, High(TrackCircuits), 'is not a valid track-circuit number', NewIntValue) THEN
      AddTrackCircuitCancelled := True
    ELSE
      TC := NewIntValue;
  END ELSE BEGIN
    SetLength(TrackCircuits, Length(TrackCircuits) + 1);
    TC := High(TrackCircuits);
    InitialiseTrackCircuitVariables(TC);

    WITH TrackCircuits[TC] DO BEGIN
      TC_DataChanged := True;
      TC_Number := TC;

      IF NOT GetExtendedDataFromUser('Enter Track-Circuit Length', '', 0, 0, '', NewExtendedValue) THEN
        TC_LengthInInches := 0
      ELSE
        TC_LengthInInches := NewExtendedValue;

      FeedbackDataOK := True;
      IF NOT GetIntegerDataFromUser('Enter Track-Circuit Feedback Unit', '', FirstFeedbackUnit, LastFeedbackUnit, 'is not a valid feedback unit', NewIntValue) THEN
        FeedbackDataOK := False
      ELSE
        TC_FeedbackUnit := NewIntValue;

      IF FeedbackDataOK AND NOT GetIntegerDataFromUser('Enter Track Circuit Feedback Input', '', 1, 8, 'is not a valid feedback input number', NewIntValue) THEN
        FeedbackDataOK := False
      ELSE
        TC_FeedbackInput := NewIntValue;

      IF FeedbackDataOK THEN BEGIN
        { Also check the track-circuit unit record here, to make sure this input is set to be a track-circuit input }
        IF (TC_FeedbackUnit < FirstFeedbackUnit) OR (TC_FeedbackUnit > LastFeedbackUnit) THEN
          Log('X! Cannot find feedback unit ' + IntToStr(TC_FeedbackUnit))
        ELSE
          IF FeedbackUnitRecords[TC_FeedbackUnit].Feedback_InputTypeArray[TC_FeedbackInput] <> TrackCircuitFeedback THEN
            Log('X! FeedbackUnitRecords database will need to be changed for unit ' + IntToStr(TC_FeedbackUnit) + ' input ' + IntToStr(TC_FeedbackInput)
                    + ' as the input is recorded in the Feedback Unit database as being a '
                    + FeedbackTypeToStr(FeedbackUnitRecords[TC_FeedbackUnit].Feedback_InputTypeArray[TC_FeedbackInput]));
      END;

      { do the same for points ********** }

      AddNewRecordToTrackCircuitDatabase;
      WriteOutTrackCircuitDataToDatabase;
    END; {WITH}
  END;

  IF NOT AddTrackCircuitCancelled THEN BEGIN
    Lines[Line].Line_TC := TC;
    Lines[Line].Line_DataChanged := True;
    WriteOutLineDataToDatabase;
    Log('TG Track circuit ' + IntToStr(TC) + ' now added');
  END;
END; { AddTrackCircuit }

PROCEDURE DeleteLineTrackCircuit(Line : Integer);
{ Delete a track circuit from a particular line after appropriate checks }
VAR
  TempLine : Integer;

BEGIN
  WITH Lines[Line] DO BEGIN
    { See if the track circuit it represents is in use by the Line database }
    FOR TempLine := 0 TO High(Lines) DO BEGIN
      IF Lines[TempLine].Line_TC = High(TrackCircuits) THEN BEGIN
        Log('T Replacing the database''s last TC ' + IntToStr(Lines[TempLine].Line_TC) + ' for Line ' + LineToStr(TempLine) + ' with TC ' + IntToStr(Line_TC));
        Lines[TempLine].Line_TC := Line_TC;
        Lines[TempLine].Line_DataChanged := True;
        WriteOutLineDataToDatabase;
      END;
    END; {FOR}

    { Now see if the feedback unit/input formerly used by the track circuit can be marked as free }
    Log('TG Feedback Unit ' + IntToStr(TrackCircuits[Line_TC].TC_FeedbackUnit) + ' input ' + IntToStr(TrackCircuits[Line_TC].TC_FeedbackInput) + ' no longer in use');

    { Move the last record from the track-circuit database into the deleted record's place in the array  (a DJW suggestion) }
    TrackCircuits[Line_TC] := TrackCircuits[High(TrackCircuits)];

    WITH TrackCircuits[Line_TC] DO BEGIN
      TC_Number := Line_TC;
      TC_DataChanged := True;

      WriteOutTrackCircuitDataToDatabase;

      { and delete the last record which is now a duplicate }
      DeleteRecordFromTrackCircuitDatabase(High(TrackCircuits));

      { Reload all the track circuits }
      ReadInTrackCircuitDataFromDatabase;
    END; {WITH}

    { Finally, remove the track circuit from the line it was attached to }
    Line_TC := UnknownTrackCircuit;
    Line_DataChanged := True;
    WriteOutLineDataToDatabase;
  END; {WITH}
END; { DeleteLineTrackCircuit }

PROCEDURE DeleteLine(LineToDeleteNum : Integer; OUT CanDelete: Boolean);
{ Delete a line after appropriate checks. NB: this doesn't yet deal with the routeing exceptions database or platform positions database. }
VAR
  Line : Integer;
  MultipleLineTrackCircuits : IntegerArrayType;
  P : Integer;
  QueryStr : String;
  S : Integer;

BEGIN
  TRY
    CanDelete := True;

    FOR S := 0 TO High(Signals) DO BEGIN
      IF Signals[S].Signal_AdjacentLine = LineToDeleteNum THEN BEGIN
        Log('L! Cannot delete line ' + LineToStr(LineToDeleteNum) + ' as it is used by signal ' + IntToStr(S));
        CanDelete := False;
      END;
    END; {FOR}

    IF CanDelete THEN BEGIN
      { see if the line is used either by a signal or by a point }
      FOR P := 0 TO High(Points) DO BEGIN
        IF (Points[P].Point_HeelLine = LineToDeleteNum)
        OR (Points[P].Point_StraightLine = LineToDeleteNum)
        OR (Points[P].Point_DivergingLine = LineToDeleteNum)
        THEN BEGIN
          Log('L! Cannot delete line ' + LineToStr(LineToDeleteNum) + ' as it is used by point ' + IntToStr(P));
          CanDelete := False;
        END;
      END; {FOR}
    END;

    IF CanDelete THEN BEGIN
      { Ask for confirmation, but first see whether an associated track circuit can be deleted - but only if it is not attached to another line }
      SetLength(MultipleLineTrackCircuits, 0);
      FOR Line := 0 TO High(Lines) DO BEGIN
        IF Lines[Line].Line_TC = Lines[LineToDeleteNum].Line_TC THEN
          AppendToIntegerArray(MultipleLineTrackCircuits, Lines[Line].Line_TC);
      END; {FOR}

      IF Length(MultipleLineTrackCircuits) <> 1 THEN
        { there's only one reference to it, so it can't be attached to another line }
        QueryStr := 'Delete line ' + LineToStr(LineToDeleteNum) + '?'
      ELSE
        QueryStr := 'Delete line ' + LineToStr(LineToDeleteNum) + ' and associated track circuit ' + IntToStr(MultipleLineTrackCircuits[0]) + '?';

      IF MessageDialogueWithDefault(QueryStr, StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo THEN BEGIN
        Debug('Line ' + IntToStr(LineToDeleteNum) + ' not deleted');
        CanDelete := False;
      END;
    END;

    IF CanDelete THEN BEGIN
      IF Length(MultipleLineTrackCircuits) = 1 THEN
        DeleteLineTrackCircuit(LineToDeleteNum);

      WITH InitVarsWindow DO BEGIN
        { change the line endings for associated lines }
        WITH Lines[LineToDeleteNum] DO BEGIN
          IF NOT Line_NextUpLine <> UnknownLine THEN BEGIN
            IF Lines[LineToDeleteNum].Line_NextUpLine <> UnknownLine THEN BEGIN
              Lines[Lines[LineToDeleteNum].Line_NextUpLine].Line_NextDownLine := UnknownLine;
              Lines[Lines[LineToDeleteNum].Line_NextUpLine].Line_NextDownIsEndOfLine := BufferStopAtDown;
              Lines[Lines[LineToDeleteNum].Line_NextUpLine].Line_EndOfLineMarker := BufferStopAtDown;
              Lines[Lines[LineToDeleteNum].Line_NextUpLine].Line_DataChanged := True;
              WriteOutLineDataToDatabase;
            END;
          END;
          IF NOT Line_NextDownLine <> UnknownLine THEN BEGIN
            IF Lines[LineToDeleteNum].Line_NextDownLine <> UnknownLine THEN BEGIN
              Lines[Lines[LineToDeleteNum].Line_NextDownLine].Line_NextUpLine := UnknownLine;
              Lines[Lines[LineToDeleteNum].Line_NextDownLine].Line_NextUpIsEndOfLine := BufferStopAtUp;
              Lines[Lines[LineToDeleteNum].Line_NextDownLine].Line_EndOfLineMarker := BufferStopAtUp;
              Lines[Lines[LineToDeleteNum].Line_NextDownLine].Line_DataChanged := True;
              WriteOutLineDataToDatabase;
            END;
          END;
        END; {WITH}

        { Now delete the last record }
        Lines[LineToDeleteNum] := Lines[High(Lines)];
        Lines[LineToDeleteNum].Line_DataChanged := True;
        WriteOutLineDataToDatabase;
        DeleteRecordFromLineDatabase(High(Lines));

        Log('L Line ' + IntToStr(LineToDeleteNum) + ' deleted');

        { Clear the data from the value-list editor }
        WITH EditWindow DO BEGIN
          ClearEditValueListAndEditedItem;
          UndoChangesButton.Enabled := False;
          SaveChangesAndExitButton.Enabled := False;
          ExitWithoutSavingButton.Enabled := False;
        END; {WITH}

        { Reload all the lines }
        ReadInLineDataFromDatabase;

        { And the points and signals as these will have lines that need to be been renumbered }
        ReadInSignalDataFromDatabase(NewData);
        ReadInPointDataFromDatabase;
        IF PreviousPointSettingsMode THEN
          LoadPreviousPointSettings;

        InvalidateScreen(UnitRef, 'DeleteLine');
        Log('D Screen invalidated by Delete Line');
      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DeleteLine: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DeleteLine }

PROCEDURE DragEndOfLine(GridX, GridY : Integer; ShiftState : TShiftState);
{ Allows a line end to be moved by the mouse }
VAR
  NearestLineToNewLine : Integer;

BEGIN
  TRY
    BEGIN
      WITH FWPRailWindow.Canvas DO BEGIN
        WITH Lines[EditedLine] DO BEGIN
          IF Line_IsTempNewLine THEN BEGIN
            Line_GridDownX := GridX;

            IF ssAlt IN ShiftState THEN
              { make it an exact horizontal line }
              Line_GridDownY := Line_GridUpY
            ELSE
              Line_GridDownY := GridY;
          END ELSE
            IF Line_IsBeingMovedByHandle = UpHandle THEN BEGIN
              Line_GridUpX := GridX;

              IF ssShift IN ShiftState THEN
                { make it an exact horizontal line }
                Line_GridUpY := Line_GridDownY
              ELSE
                Line_GridUpY := GridY;
            END ELSE
              IF Line_IsBeingMovedByHandle = DownHandle THEN BEGIN
                Line_GridDownX := GridX;

                IF ssShift IN ShiftState THEN
                  { make it an exact horizontal line }
                  Line_GridDownY := Line_GridUpY
                ELSE
                  Line_GridDownY := GridY;
              END;
        END; {WITH}
      END; {WITH}

//      NearestLineToNewLine := GetNearestLine(X, Y);
//      IF NearestLineToNewLine = UnknownLine THEN
//        ChangeCursor(crNoDrop)
//      ELSE
//        ChangeCursor(crDrag);

      FWPRailWindow.Repaint;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DragEndOfLine:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DragEndOfLine }

PROCEDURE DragWholeLine(GridX, GridY : Integer);
{ Move a whole line by dragging its middle handle }
VAR
  Line : Integer;

BEGIN
  TRY
    BEGIN
      WITH FWPRailWindow.Canvas DO BEGIN
        FOR Line := 0 TO High(Lines) DO BEGIN
          WITH Lines[Line] DO BEGIN
            IF Line_IsBeingMovedByHandle <> NoHandle THEN BEGIN
              Line_GridUpX := Line_GridUpX + GridX - SaveGridClickPosX;
              Line_GridUpY := Line_GridUpY + GridY - SaveGridClickPosY;
              Line_GridDownX := Line_GridDownX + GridX - SaveGridClickPosX;
              Line_GridDownY := Line_GridDownY + GridY - SaveGridClickPosY;

              SaveGridClickPosX := GridX;
              SaveGridClickPosY := GridY;
            END;
          END; {WITH}
        END; {FOR}

        FWPRailWindow.Repaint;
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DragWholeLine:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DragWholeLine }

FUNCTION GetYPositionOfNearestGridLine(GridY : Integer) : Integer;
BEGIN
  Result := Round(GridY / GridInterLineSpacing) * GridInterLineSpacing;
END; { GetPositionOfNearestGridLine }

FUNCTION GetXPositionOfNearestLineIfAny(Line, X, Y : Integer) : Integer;
{ So we can see if a newly-created or extended line can be "snapped" to an existing one. This returns 0 (so there's no "snap") if there's no line near }
VAR
  ScreenX : Integer;
  ScreenY : Integer;
  LineFound : Boolean;
  TempLine : Integer;

BEGIN
  TempLine := 0;
  LineFound := False;
  Result := X;
  ScreenX := MapGridXToScreenX(X);
  ScreenY := MapGridYToScreenY(Y);

  WHILE (TempLine <= High(Lines)) AND NOT LineFound DO BEGIN
    IF TempLine <> Line THEN BEGIN
      WITH Lines[TempLine] DO BEGIN
        IF PointInPolygon(Line_UpHandlePolygon, Point(ScreenX, ScreenY)) THEN BEGIN
          Result := Line_GridUpX;
          LineFound := True;
        END ELSE
          IF PointInPolygon(Line_DownHandlePolygon, Point(ScreenX, ScreenY)) THEN BEGIN
            Result := Line_GridDownX;
            LineFound := True;
          END;
      END; {WITH}
    END;
    Inc(TempLine);
  END; {WHILE}
END; { GetXPositionOfNearestLineIfAny }

PROCEDURE CreateLine(Line : Integer);
{ Create a basic line which must then be added to using the value list editor }
BEGIN
  CalculateLinePositions;
  InitialiseLineVariables(Line);
  Lines[Line].Line_DataChanged := True;

  NoteThatDataHasChanged;
  CalculateLinePositions;
  AddNewRecordToLineDatabase;
END; { CreateLine }

PROCEDURE LineDraggingComplete(ShiftState : TShiftState);
{ Called when when we have finished line creation by mouse }
VAR
  OldGridUpX : Integer;
  OldGridUpY : Integer;
  OldGridDownX : Integer;
  OldGridDownY : Integer;
  OldDownRow : Extended;
  OldUpRow : Extended;
  TempVal : Integer;

BEGIN
  WITH Lines[EditedLine] DO BEGIN
    Line_IsBeingMovedByHandle := NoHandle;

    { We may need to reverse the X/Y values if the down X value is greater than the up X value }
    IF Line_GridDownX < Line_GridUpX THEN BEGIN
      TempVal := Line_GridUpX;
      Line_GridUpX := Line_GridDownX;
      Line_GridDownX := TempVal;

      TempVal := Line_GridUpY;
      Line_GridUpY := Line_GridDownY;
      Line_GridDownY := TempVal;
    END;

    { save where the line is in case we need to revert }
    OldGridUpX := Line_GridUpX;
    OldGridUpY := Line_GridUpY;
    OldGridDownX := Line_GridDownX;
    OldGridDownY := Line_GridDownY;
    OldDownRow := Line_UpRow;
    OldUpRow := Line_DownRow;

    { Now update the new line record }
    Line_UpRow := MapGridYToRow(Line_GridUpY);
    Line_DownRow := MapGridYToRow(Line_GridDownY);

    IF DistanceBetween(Line_GridUpX, Line_GridUpY, Line_GridDownX, Line_GridDownY) < 5 THEN BEGIN
      { the cursor has been clicked without it moving - if we're on a line, then mark it as edited and add the handles }
      IF EditingExistingLine THEN BEGIN
        { return to existing length }
        Line_GridUpX := OldGridUpX;
        Line_GridUpY := OldGridUpY;
        Line_GridDownX := OldGridDownX;
        Line_GridDownY := OldGridDownY;
        Line_UpRow := OldUpRow;
        Line_DownRow := OldDownRow;

        DeselectLine;
      END ELSE BEGIN
        DeselectLine;
        SetLength(Lines, Length(Lines) - 1);
      END;
    END ELSE BEGIN
      { Unless Ctrl is held down, see if we are near enough a horizontal grid line or the beginning or the end of an actual line to "snap" to it }
      IF NOT (ssCtrl IN ShiftState) THEN BEGIN
debug('Line_GridUpY=' + inttostr(Line_GridUpY));
debug('GetYPositionOfNearestGridLine(Line_GridUpY)=' + inttostr(GetYPositionOfNearestGridLine(Line_GridUpY)));
        Line_UpRow := MapGridYToRow(GetYPositionOfNearestGridLine(Line_GridUpY));
debug('Line_UpRow=' + floattostr(Line_upRow));
        Line_DownRow := MapGridYToRow(GetYPositionOfNearestGridLine(Line_GridDownY));
        Line_GridUpX := GetXPositionOfNearestLineIfAny(EditedLine, Line_GridUpX, Line_GridUpY);
        Line_GridDownX := GetXPositionOfNearestLineIfAny(EditedLine, Line_GridDownX, Line_GridDownY);
      END ELSE BEGIN
        Line_UpRow := MapGridYToRow(Line_GridUpY);
        Line_DownRow := MapGridYToRow(Line_GridDownY);
      END;

      Line_IsTempNewLine := False;

      IF NOT EditingExistingLine THEN BEGIN
        CreateLine(EditedLine);
        EditingExistingLine := True;
      END;
    END;

    IF EditedLine <> UnknownLine THEN BEGIN
      Lines[EditedLine].Line_DataChanged := True;
      WriteOutLineDataToDatabase;
      SaveLineRec := Lines[EditedLine];
      WriteLineValuesToValueList;
    END;
    InvalidateScreen(UnitRef, 'LineDraggingComplete');
  END; {WITH}
END; { LineDraggingComplete }

PROCEDURE JoinLine(LineArray : IntegerArrayType; GridX, GridY : Integer);
{ Join a down line to an up line at the point indicated. The checks to make sure that the lines are next to each other and are at the same angle have been done in the
  Popup code. The line array is set up to be up-line name followed by down-line name.
}
VAR
  DebugStr : String;
  OK : Boolean;
  P : Integer;

BEGIN
  DebugStr := '';
  { If the two lines have different settings, e.g. track circuits, inform the user }
  IF Lines[LineArray[0]].Line_TC <> Lines[LineArray[1]].Line_TC THEN
    DebugStr := DebugStr + ' track circuits do not match;';
  IF (Lines[LineArray[0]].Line_AdjacentBufferStop <> UnknownBufferStop) AND (Lines[LineArray[1]].Line_AdjacentBufferStop <> UnknownBufferStop) THEN
    DebugStr := DebugStr + ' both lines have bufferstops;';
  IF ((Lines[LineArray[0]].Line_BufferStopTheatreDestinationStr <> '') AND (Lines[LineArray[1]].Line_BufferStopTheatreDestinationStr = ''))
  OR ((Lines[LineArray[0]].Line_BufferStopTheatreDestinationStr = '') AND (Lines[LineArray[1]].Line_BufferStopTheatreDestinationStr <> ''))
  THEN
    DebugStr := DebugStr + ' Buffer stop destination strings do not match;';
  IF GetLineAdjacentSignal(LineArray[1]) <> UnknownSignal THEN
    DebugStr := DebugStr + ' line ' + LineToStr(LineArray[1]) + ' has an adjacent signal;';
  IF Lines[LineArray[0]].Line_Direction <> Lines[LineArray[1]].Line_Direction THEN
    DebugStr := DebugStr + ' incompatible line directions;';
  IF Lines[LineArray[0]].Line_DownConnectionCh <> Lines[LineArray[1]].Line_DownConnectionCh THEN
    DebugStr := DebugStr + ' incompatible line connection strings;';
  IF Lines[LineArray[0]].Line_UpConnectionCh <> Lines[LineArray[1]].Line_UpConnectionCh THEN
    DebugStr := DebugStr + ' incompatible line connection strings;';
  IF Lines[LineArray[0]].Line_Gradient <> Lines[LineArray[1]].Line_Gradient THEN
    DebugStr := DebugStr + ' incompatible line gradient;';
  IF Lines[LineArray[0]].Line_InUseFeedbackUnit <> Lines[LineArray[1]].Line_InUseFeedbackUnit THEN
    DebugStr := DebugStr + ' in use feedback units do not match;';
  IF Lines[LineArray[0]].Line_InUseFeedbackInput <> Lines[LineArray[1]].Line_InUseFeedbackInput THEN
    DebugStr := DebugStr + ' in use feedback inputs do not match;';
  IF Lines[LineArray[0]].Line_Location <> Lines[LineArray[1]].Line_Location THEN
    DebugStr := DebugStr + ' line locations do not match;';
  IF Lines[LineArray[0]].Line_OutOfUseState <> Lines[LineArray[1]].Line_OutOfUseState THEN
    DebugStr := DebugStr + ' out-of-use states do not match;';
  IF Lines[LineArray[0]].Line_TypeOfLine <> Lines[LineArray[1]].Line_TypeOfLine THEN
    DebugStr := DebugStr + ' out-of-use states do not match;';

  FOR P := 0 TO High(Points) DO BEGIN
    IF Points[P].Point_StraightLine = LineArray[1] THEN
      DebugStr := DebugStr + ' point ' + IntToStr(P) + ' is attached to ' + LineToStr(LineArray[1]);
    IF Points[P].Point_DivergingLine = LineArray[1] THEN
      DebugStr := DebugStr + ' point ' + IntToStr(P) + ' is attached to ' + LineToStr(LineArray[1]);
    IF Points[P].Point_HeelLine = LineArray[1] THEN
      DebugStr := DebugStr + ' point ' + IntToStr(P) + ' is attached to ' + LineToStr(LineArray[1]);
  END; {FOR}

  { Remove any final semicolon for neatness }
  IF DebugStr[Length(DebugStr)] = ';' THEN
    DebugStr[Length(DebugStr)] := '.';

  IF DebugStr <> '' THEN BEGIN
    DebugStr := 'Problems in joining lines ' + LineToStr(LineArray[0]) + ' and ' + LineToStr(LineArray[1]) + ': ' + DebugStr;
    Log('X ' + DebugStr + ' {WRAP=SCREENWIDTH}');
    ShowMessage('Cannot join lines ' + LineToStr(LineArray[0]) + ' and ' + LineToStr(LineArray[1]) + ': incompatibilities found are recorded in the Log file');
  END ELSE BEGIN
    Lines[LineArray[0]].Line_GridDownX := Lines[LineArray[1]].Line_GridDownX;
    Lines[LineArray[0]].Line_GridDownY := Lines[LineArray[1]].Line_GridDownY;
    Lines[LineArray[0]].Line_EndOfLineMarker := Lines[LineArray[1]].Line_EndOfLineMarker;
    Lines[LineArray[0]].Line_NextDownIsEndOfLine := Lines[LineArray[1]].Line_NextDownIsEndOfLine;
    Lines[LineArray[0]].Line_NextDownLine := Lines[LineArray[1]].Line_NextDownLine;
    Lines[LineArray[0]].Line_NextDownPoint := Lines[LineArray[1]].Line_NextDownPoint;
    Lines[LineArray[0]].Line_NextDownType := Lines[LineArray[1]].Line_NextDownType;

    DeleteLine(LineArray[1], OK);
    IF OK THEN BEGIN
      Lines[LineArray[0]].Line_DataChanged := True;
      WriteOutLineDataToDatabase;
    END;
  END;
END; { JoinLine }

PROCEDURE SplitLine(OldLine, GridX, GridY : Integer);
{ Split a line at the point indicated }
VAR
  AX, AY, BX, BY : Integer;
  C : Extended;
  M : Extended;
  PX, PY : Integer;
  NewLine : Integer;

BEGIN
  TRY
    { First create a new one using the new co-ordinates }
    SetLength(Lines, Length(Lines) + 1);
    CreateLine(High(Lines));
    NewLine := High(Lines);

    IF Lines[OldLine].Line_GridUpY <> Lines[OldLine].Line_GridDownY THEN BEGIN
      WITH Lines[OldLine] DO BEGIN
        AX := Line_GridDownX;
        AY := Line_GridDownY;
        BX := Line_GridUpX;
        BY := Line_GridUpY;
        M := (BY - AY) / (BX - AX); { gradient of the line }
        C := AY - (M * AX);  { Y intercept }

        PY := GridY;
        PX := Round((PY - C) / M);
      END; {WITH}
    END ELSE BEGIN
      PX := GridX;
      PY := Lines[OldLine].Line_GridUpY;
    END;

    { Adjust the X/Y co-ordinates for the new line - but make sure the Y position is on a grid line }
    Lines[NewLine].Line_GridUpX := PX;
    Lines[NewLine].Line_GridUpY := PY;
    Lines[NewLine].Line_UpRow := MapGridYToRow(Lines[NewLine].Line_GridUpY);

    Lines[NewLine].Line_GridDownX := Lines[OldLine].Line_GridDownX;
    Lines[NewLine].Line_GridDownY := Lines[OldLine].Line_GridDownY;
    Lines[NewLine].Line_DownRow := Lines[OldLine].Line_DownRow;

    { Then truncate the existing old line }
    Lines[OldLine].Line_GridDownX := Lines[NewLine].Line_GridUpX;
    Lines[OldLine].Line_GridDownY := Lines[NewLine].Line_GridUpY;
    Lines[OldLine].Line_DownRow := Lines[NewLine].Line_UpRow;

    { Now adjust the other details for the new line }
    WITH Lines[NewLine] DO BEGIN
      Line_AdjacentBufferStop := UnknownBufferStop;
      Line_DownConnectionCh := Lines[OldLine].Line_DownConnectionCh;
      Line_DownConnectionChBold := False;
      Line_EndOfLineMarker := BufferStopAtDown;
      Line_LockFailureNotedInSubRouteUnit := False;
      Line_NameStr := '*' + IntToStr(NewLine) + '*';
      Line_NextDownIsEndOfLine := Lines[OldLine].Line_NextDownIsEndOfLine;
      Line_NextDownPoint := Lines[OldLine].Line_NextDownPoint;
      Line_NextDownLine := Lines[OldLine].Line_NextDownLine;
      Line_NextDownType := Lines[OldLine].Line_NextDownType;
      Line_NextUpIsEndofLine := NotEndOfLine;
      Line_NextUpLine := OldLine;
      Line_NextUpPoint := UnknownPoint;
      Line_NextUpType := UnknownNextLineRouteingType;
      Line_RouteLockingForDrawing := UnknownRoute;
      Line_RouteSet := UnknownRoute;
      Line_TC := UnknownTrackCircuit;
      Line_TypeOfLine := Lines[OldLine].Line_TypeOfLine;
      Line_UpConnectionCh := '';
      Line_UpConnectionChBold := False;
    END; {WITH}

    { And for any connected points at the Down end of the new line or signals attached to old line that has become the new line }
    { ************** 22/10/14 }

    { Also TCs }

    { And for the existing line }
    WITH Lines[OldLine] DO BEGIN
      Line_NextDownIsEndOfLine := NotEndOfLine;
      Line_NextDownPoint := UnknownPoint;
      Line_NextDownLine := NewLine;
      Line_NextDownType := LineIsNext;
    END; {WITH}

    CalculateLinePositions;

    { Now write both sets of revised data to the database }
    Lines[OldLine].Line_DataChanged := True;
    Lines[NewLine].Line_DataChanged := True;
    WriteOutLineDataToDatabase;

    InvalidateScreen(UnitRef, 'LineDraggingComplete');
  EXCEPT
    ON E : Exception DO
      Log('EG SplitLine:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SplitLine }

{ note: we need to deal with lines that overlap on a different plane
  snap lines to other lines?
  ask about adding points?
}

END { Edit }.


