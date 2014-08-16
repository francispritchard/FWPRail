UNIT Edit;
{ Unit that allows the user to construct a layout in diagrammatic form on the screen

  Copyright © F.W. Pritchard 2009-2014. All Rights Reserved.

  v0.1  15/07/09 Unit created
  v0.2  10/03/13 writing signal editing code
  v0.3  26/03/13 writing point editing code
}
INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Options, ExtCtrls, Grids, ValEdit, InitVars, MiscUtils, Movement, StdCtrls, RailDraw,
  Vcl.Menus;

TYPE
  TEditWindow = CLASS(TForm)
    EditValueListEditor: TValueListEditor;
    EditWindowLabel: TLabel;
    EditWindowPopupMenu: TPopupMenu;
    ExitWithoutSavingButton: TButton;
    PopupEditWindowResetSizeAndPosition: TMenuItem;
    SaveChangesAndExitButton: TButton;
    UndoChangesButton: TButton;
    PROCEDURE EditValueListEditorEditButtonClick(Sender: TObject);
    PROCEDURE EditValueListEditorExit(Sender: TObject);
    PROCEDURE EditValueListEditorStringsChange(Sender: TObject);
    PROCEDURE EditValueListEditorValidate(Sender: TObject; ACol, ARow: Integer; CONST KeyName, KeyValue: String);
    PROCEDURE EditWindowPopupMenuPopup(Sender: TObject);
    PROCEDURE EditWindowPopupResetSizeAndPositionClick(Sender: TObject);
    PROCEDURE EditWindowResize(Sender: TObject);
    PROCEDURE EditWindowShow(Sender: TObject);
    PROCEDURE ExitWithoutSavingButtonClick(Sender: TObject);
    PROCEDURE SaveChangesAndExitButtonClick(Sender: TObject);
    PROCEDURE SignalImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    PROCEDURE UndoChangesButtonClick(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE CheckWhetherEditedSignalDataHasChanged;
{ Ask whether we want to save any amended data before selecting another signal }

PROCEDURE ClearValueList;
{ Empty the value list so as not to display anyting if we click on an unrecognised item, or a blank bit of screen }

PROCEDURE CreatePoint(Direction : DirectionType; Line : Integer);
{ Creates a point from scratch }

PROCEDURE CreateSignal(Direction : DirectionType; Line : Integer);
{ Creates a signal from scratch }

PROCEDURE DeletePoint(PointToDeleteNum : Integer);
{ Delete a point after appropriate checks }

PROCEDURE DeleteSignal(SignalToDeleteNum : Integer);
{ Delete a signal after appropriate checks }

PROCEDURE DisplaySignalOptionsInValueList(S : Integer; SaveVariablesOnExit : Boolean);
{ Create a value list in the edit window with the appropriate values }

PROCEDURE DisplayPointOptionsInValueList(P : Integer; SaveVariables : Boolean);
{ Create a value list in the edit window with the appropriate values }

PROCEDURE DragSignal(S, MouseX, MouseY : Integer; OUT NearestLine, TooNearSignal : Integer);
{ { Allows a signal to be moved by the mouse }

PROCEDURE DropSignal;
{ Drops the signal at the nearest adjacent line }

PROCEDURE InitialiseEditUnit;
{ Initialises the unit }

PROCEDURE MoveObjectLeft;
{ Moves whatever is currently being edited to the left }

PROCEDURE MoveObjectRight;
{ Moves whatever is currently being edited to the right }

PROCEDURE ProcessSignalLocationsToMonitorCheckListBoxChecks;
{ See which locations are ticked and update the array }

PROCEDURE TurnEditModeOn(SignalNum, PointNum, BufferStopNum, LineNum, TrackCircuitNum : Integer);
{ Turn edit Mode on }

PROCEDURE TurnEditModeOff;
{ Turn edit Mode off }

VAR
  DragSignalNum : Integer = UnknownSignal;
  EditedBufferStop : Integer = UnknownBufferStop;
  EditedLine : Integer = UnknownLine;
  EditedPoint : Integer = UnknownPoint;
  EditedSignal : Integer = UnknownSignal;
  EditedTrackCircuit : Integer = UnknownTrackCircuit;
  EditWindow: TEditWindow;
  SaveDragX : Integer = 0;
  SaveDragY : Integer = 0;

IMPLEMENTATION

{$R *.dfm}

USES Diagrams, Input, Cuneo, Lenz, Types;

CONST
  UnitRef = 'Edit';

VAR
  SaveEvent : TNotifyEvent;
  SaveSystemOnlineState : Boolean;
  ValueListToBeCleared : Boolean = False;

  CreateNewSignalRecord : Boolean = False;
  SaveSignalAccessoryAddress : Integer;
  SaveSignalAdjacentLine : Integer;
  SaveSignalAdjacentLineXOffset : Integer;
  SaveSignalApproachControlAspect : AspectType;
  SaveSignalAsTheatreDestination : String;
  SaveSignalAutomatic : Boolean;
  SaveSignalDecoderNum : Integer;
  SaveSignalDirection : DirectionType;
  SaveSignalIndicator : IndicatorType;
  SaveSignalIndicatorDecoderFunctionNum : Integer;
  SaveSignalIndicatorDecoderNum : Integer;
  SaveSignalIndicatorSpeedRestriction : MPHType;
  SaveSignalJunctionIndicators : ARRAY [JunctionIndicatorType] OF JunctionIndicatorRec;
  SaveSignalLocationsToMonitorArray : IntegerArrayType;
  SaveSignalNextSignalIfNoIndicator : Integer;
  SaveSignalNotUsedForRouteing : Boolean;
  SaveSignalNotes : String;
  SaveSignalOppositePassingLoopSignal : Integer;
  SaveSignalOutOfUse : Boolean;
  SaveSignalPossibleRouteHold : Boolean;
  SaveSignalPossibleStationStartRouteHold : Boolean;
  SaveSignalSemaphoreDistantHomesArray : IntegerArrayType;
  SaveSignalType : TypeOfSignal;

  CreateNewPointRecord : Boolean = False;
  SavePointFoundNum : Integer = UnknownPoint;
  SavePointDivergingLine : Integer;
  SavePointHeelLine : Integer;
  SavePointStraightLine : Integer;
  SavePointLenzNum : Integer;
  SavePointLenzUnit : Integer;
  SavePointLenzUnitType : String;
  SavePointManualOperation : Boolean;
  SavePointFeedbackUnit : Integer;
  SavePointFeedbackInput : Integer;
  SavePointFeedbackOnIsStraight : Boolean;
  SavePointWiringReversedFlag : Boolean;
  SavePointType : TypeOfPoint;
  SavePointOtherPoint : Integer;
  SavePointDefaultState : PointStateType;
  SavePointLockedIfHeelTCOccupied : Boolean;
  SavePointOutOfUse : Boolean;
  SavePointLockedIfNonHeelTCsOccupied : Boolean;
  SavePointNotes : String;
  SavePointLastManualStateAsReadIn : PointStateType;
  SavePointLastFeedbackStateAsReadIn : PointStateType;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE SaveSignalVariables(S : Integer);
{ Save the signal data in temporary variables, so that we can later undo any changes we've made }
BEGIN
  WITH Signals[S] DO BEGIN
    SaveSignalAccessoryAddress := Signal_AccessoryAddress;
    SaveSignalAdjacentLine := Signal_AdjacentLine;
    SaveSignalAdjacentLineXOffset := Signal_AdjacentLineXOffset;
    SaveSignalApproachControlAspect := Signal_ApproachControlAspect;
    SaveSignalAsTheatreDestination := Signal_AsTheatreDestination;
    SaveSignalAutomatic := Signal_Automatic;
    SaveSignalDecoderNum := Signal_DecoderNum;
    SaveSignalDirection := Signal_Direction;
    SaveSignalIndicator := Signal_Indicator;
    SaveSignalIndicatorDecoderFunctionNum := Signal_IndicatorDecoderFunctionNum;
    SaveSignalIndicatorDecoderNum := Signal_IndicatorDecoderNum;
    SaveSignalIndicatorSpeedRestriction := Signal_IndicatorSpeedRestriction;
    SaveSignalJunctionIndicators[UpperLeftIndicator] := Signal_JunctionIndicators[UpperLeftIndicator];
    SaveSignalJunctionIndicators[MiddleLeftIndicator] := Signal_JunctionIndicators[MiddleLeftIndicator];
    SaveSignalJunctionIndicators[LowerLeftIndicator] := Signal_JunctionIndicators[LowerLeftIndicator];
    SaveSignalJunctionIndicators[UpperRightIndicator] := Signal_JunctionIndicators[UpperRightIndicator];
    SaveSignalJunctionIndicators[MiddleRightIndicator] := Signal_JunctionIndicators[MiddleRightIndicator];
    SaveSignalJunctionIndicators[LowerRightIndicator] := Signal_JunctionIndicators[LowerRightIndicator];
    SaveSignalLocationsToMonitorArray := Signal_LocationsToMonitorArray;
    SaveSignalNextSignalIfNoIndicator := Signal_NextSignalIfNoIndicator;
    SaveSignalNotUsedForRouteing := Signal_NotUsedForRouteing;
    SaveSignalNotes := Signal_Notes;
    SaveSignalOppositePassingLoopSignal := Signal_OppositePassingLoopSignal;
    SaveSignalOutOfUse := Signal_OutOfUse;
    SaveSignalPossibleRouteHold := Signal_PossibleRouteHold;
    SaveSignalPossibleStationStartRouteHold := Signal_PossibleStationStartRouteHold;
    SaveSignalSemaphoreDistantHomesArray := Signal_SemaphoreDistantHomesArray;

    SaveSignalType := Signal_Type;
  END; {WITH}
END; { SaveSignalVariables; }

PROCEDURE SavePointVariables(P : Integer);
{ Save the point data in temporary variables, so that we can later undo any changes we've made }
BEGIN
  WITH Points[P] DO BEGIN
    SavePointDivergingLine := Point_DivergingLine;
    SavePointHeelLine := Point_HeelLine;
    SavePointStraightLine := Point_StraightLine;
    SavePointLastFeedbackStateAsReadIn := Point_LastFeedbackStateAsReadIn;
    SavePointLastManualStateAsReadIn := Point_LastManualStateAsReadIn;
    SavePointLenzNum := Point_LenzNum;
    SavePointLenzUnit := Point_LenzUnit;
    SavePointLenzUnitType := Point_LenzUnitType;
    SavePointManualOperation := Point_ManualOperation;
    SavePointFeedbackUnit := Point_FeedbackUnit;
    SavePointFeedbackInput := Point_FeedbackInput;
    SavePointFeedbackOnIsStraight := Point_FeedbackOnIsStraight;
    SavePointWiringReversedFlag := Point_WiringReversedFlag;
    SavePointType := Point_Type;
    SavePointOtherPoint := Point_OtherPoint;
    SavePointDefaultState := Point_DefaultState;
    SavePointLockedIfHeelTCOccupied := Point_LockedIfHeelTCOccupied;
    SavePointOutOfUse := Point_OutOfUse;
    SavePointLockedIfNonHeelTCsOccupied := Point_LockedIfNonHeelTCsOccupied;
    SavePointNotes := Point_Notes;
    SavePointLastManualStateAsReadIn := Point_LastManualStateAsReadIn;
  END; {WITH}
END; { SavePointVariables }

PROCEDURE TurnEditModeOn(SignalNum, PointNum, BufferStopNum, LineNum, TrackCircuitNum : Integer);
{ Turn edit Mode on }
BEGIN
  IF NOT EditMode THEN BEGIN
    EditMode := True;
    IF SignalNum <> UnknownSignal THEN
      EditedSignal := SignalNum;
    IF PointNum <> UnknownPoint THEN
      EditedPoint := PointNum;
    IF LineNum <> UnknownLine THEN
      EditedLine := LineNum;
    IF TrackCircuitNum <> UnknownTrackCircuit THEN
      EditedTrackCircuit := TrackCircuitNum;

    Diagrams.DiagramsWindow.Visible := False;

    { The tags are used to tell EditWindow.Visible which value list to edit }
    Edit.EditWindow.Tag := -1;
    IF SignalNum <> UnknownSignal THEN BEGIN
      Edit.EditWindow.Tag := 1;
    END ELSE
      IF PointNum <> UnknownPoint THEN BEGIN
        Edit.EditWindow.Tag := 2;
        SavePointFoundNum := PointNum;
      END;

    Edit.EditWindow.Visible := True;

    SaveSystemOnlineState := SystemOnline;
    IF SystemOnline THEN
      SetSystemOffline('System offline as edit mode starting');

    SetCaption(FWPRailWindow, 'EDITING...');
    EditWindow.EditWindowLabel.caption := '';
    Application.Icon := EditIcon;
  END;
END; { TurnEditModeOn }

PROCEDURE TurnEditModeOff;
{ Turn edit mode off }
BEGIN
  TRY
    IF EditMode THEN BEGIN
      EditedSignal := UnknownSignal;
      EditedPoint := UnknownPoint;
      EditedLine := UnknownLine;
      EditedTrackCircuit := UnknownTrackCircuit;

      EditWindow.Visible := False;
      Diagrams.DiagramsWindow.Visible := True;
      EditMode := False;

      SetCaption(FWPRailWindow, '');
      EditWindow.EditValueListEditor.Strings.Clear;

      { and force a redraw so that the highglighted signal/point etc. is de-highlighted }
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

PROCEDURE InitialiseEditUnit;
{ Initialises the unit }
BEGIN
  EditWindow.Height := EditWindowHeight;
  EditWindow.Top := EditWindowTop;
  EditWindow.Left := EditWindowLeft;
  EditWindow.Width := EditWindowWidth;
END; { InitialiseEditUnit }

PROCEDURE TEditWindow.EditValueListEditorStringsChange(Sender: TObject);
BEGIN
  IF NOT ValueListToBeCleared THEN BEGIN
    { clearing the value list changes the strings in it, but we can't then save or undo them }
    UndoChangesButton.Enabled := True;
    SaveChangesAndExitButton.Enabled := True;
    ExitWithoutSavingButton.Enabled := True;
  END;
END; { EditWindowValueListEditorStringsChange }

PROCEDURE TEditWindow.SignalImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
BEGIN
//  SignalImage.BeginDrag(True);
END; { SignalImageMouseDown }

PROCEDURE TEditWindow.EditValueListEditorEditButtonClick(Sender: TObject);
{ This is only used for dealing with the entry for Signal Locations To Monitor }
VAR
  ArrayCount : Integer;
  Done : Boolean;
  I : Integer;
  KeyName : String;
  KeyValue : String;
  Loc : Integer;
  StrList: TStringList;

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

    IF KeyName = 'Signal Locations To Monitor' THEN BEGIN
      WITH MovementWindow DO BEGIN
        MovementWindow.Visible := True;

        { Use a hack to sort the list box }
        StrList := TStringList.Create;

        { Populate the box, platforms first }
        FOR Loc := 0 TO High(Locations) DO
          IF Locations[Loc].Location_IsPlatform THEN
            StrList.Add(Locations[Loc].Location_LongStr);

        StrList.Sort;

        SignalLocationsToMonitorCheckListBox.Clear;

        FOR I := 0 TO StrList.Count - 1 DO
          SignalLocationsToMonitorCheckListBox.Items.Add(StrList.Strings[I]);

        StrList.Clear;

        { Now the fiddleyard items }
        FOR Loc := 0 TO High(Locations) DO
          IF Locations[Loc].Location_IsFiddleyard THEN
            StrList.Add(Locations[Loc].Location_LongStr);

        StrList.Sort;

        FOR I := 0 TO StrList.Count - 1 DO
          SignalLocationsToMonitorCheckListBox.Items.Add(StrList.Strings[I]);

        StrList.Destroy;

        { Now tick those locations that are already set }
        ArrayCount := 0;
        WHILE ArrayCount < Length(Signals[EditedSignal ].Signal_LocationsToMonitorArray) DO BEGIN
          { find it in the checklistbox and select it }
          I := 0;
          Done := False;
          WHILE (I < SignalLocationsToMonitorCheckListBox.Items.Count - 1) AND NOT Done DO BEGIN
            IF SignalLocationsToMonitorCheckListBox.Items[I] = Locations[Signals[EditedSignal].Signal_LocationsToMonitorArray[ArrayCount]].Location_LongStr THEN BEGIN
              SignalLocationsToMonitorCheckListBox.Checked[I] := True;
              Done := True;
            END;
            Inc(I);
          END; {WHILE}
          Inc(ArrayCount);
        END; {WHILE}
      END; {WITH}
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG EditValueListEditorEditButtonClick: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { EditValueListEditorEditButtonClick }

PROCEDURE ProcessSignalLocationsToMonitorCheckListBoxChecks;
{ See which locations are ticked and update the array. This shouldn't need validation as the user has not been given the opportunity of introducing errors. }
VAR
  I : Integer;
  Loc : Integer;

BEGIN
  TRY
    WITH MovementWindow DO BEGIN
      SetLength(Signals[EditedSignal].Signal_LocationsToMonitorArray, 0);

      I := 0;
      WHILE I < SignalLocationsToMonitorCheckListBox.Items.Count - 1 DO BEGIN
        IF SignalLocationsToMonitorCheckListBox.Checked[I] THEN BEGIN
          Loc := 0;
          WHILE Loc < Length(Locations) DO BEGIN
            IF Locations[Loc].Location_LongStr = SignalLocationsToMonitorCheckListBox.Items[I] THEN
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

          IF KeyName = Signal_TypeFieldName THEN
            Signal_Type := ValidateSignalType(NewKeyValue, Signal_Quadrant, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_IndicatorFieldName THEN
              Signal_Indicator := ValidateSignalIndicator(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_UpperLeftIndicatorTargetFieldName THEN BEGIN
              TempStrArray[0] := NewKeyValue;
              Signal_JunctionIndicators[UpperLeftIndicator] := ValidateJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               ['No Indicator', 'Junction Indicator', 'Theatre Indicator']);
                END;
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_MiddleLeftIndicatorTargetFieldName THEN BEGIN
              TempStrArray[1] := NewKeyValue;
              Signal_JunctionIndicators[MiddleLeftIndicator] := ValidateJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               ['No Indicator', 'Junction Indicator', 'Theatre Indicator']);
                END;
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_LowerLeftIndicatorTargetFieldName THEN BEGIN
              TempStrArray[2] := NewKeyValue;
              Signal_JunctionIndicators[LowerLeftIndicator] := ValidateJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               ['No Indicator', 'Junction Indicator', 'Theatre Indicator']);
                END;
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_UpperRightIndicatorTargetFieldName THEN BEGIN
              TempStrArray[3] := NewKeyValue;
              Signal_JunctionIndicators[UpperRightIndicator] := ValidateJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               ['No Indicator', 'Junction Indicator', 'Theatre Indicator']);
                END;
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_MiddleRightIndicatorTargetFieldName THEN BEGIN
              TempStrArray[4] := NewKeyValue;
              Signal_JunctionIndicators[MiddleRightIndicator] := ValidateJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               ['No Indicator', 'Junction Indicator', 'Theatre Indicator']);
                END;
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_LowerRightIndicatorTargetFieldName THEN BEGIN
              TempStrArray[5] := NewKeyValue;
              Signal_JunctionIndicators[LowerRightIndicator] := ValidateJunctionIndicators1(NewKeyValue, KeyName, Signal_Indicator, ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                IF Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_Exists THEN BEGIN
                  Signal_Indicator := JunctionIndicator;
                  WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType),
                                                                                                               ['No Indicator', 'Junction Indicator', 'Theatre Indicator']);
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
            ValidateJunctionIndicators2(TempStrArray, Signal_Indicator, Signal_JunctionIndicators, ErrorMsg);

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_AdjacentLineFieldName THEN BEGIN
              Signal_AdjacentLine := ValidateSignalAdjacentLine(EditedSignal , NewKeyValue, ErrorMsg);
              IF Signal_AdjacentLine = UnknownLine THEN BEGIN
                Signal_AdjacentLine := SaveSignalAdjacentLine;
                Values[KeyName] := LineToStr(SaveSignalAdjacentLine);
              END;
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Signal_AdjacentLineXOffsetFieldName THEN
              Signal_AdjacentLineXOffset := ValidateSignalAdjacentLineXOffset(NewKeyValue, ErrorMsg);
          END;

          IF ErrorMsg = '' THEN
            IF KeyName = Signal_DirectionFieldName THEN
              Signal_Direction := ValidateDirection(NewKeyValue, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Signal_IndicatorSpeedRestrictionFieldName THEN
              Signal_IndicatorSpeedRestriction := ValidateIndicatorSpeedRestriction(NewKeyValue, Signal_Indicator, ErrorMsg);

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
              Signal_SemaphoreDistantHomesArray := ValidateSignalDistantHomesArray(EditedSignal , Signal_Type, UpperCase(NewKeyValue), ErrorMsg);
              IF ErrorMsg = '' THEN BEGIN
                I := 0;
                WHILE (I <= High(Signal_SemaphoreDistantHomesArray)) AND (ErrorMsg = '') DO BEGIN
                  ErrorMsg := ValidateSignalNum(Signal_SemaphoreDistantHomesArray[I]);
                  Inc(I);
                END; {WHILE}
              END;
          END;

          IF ErrorMsg = '' THEN BEGIN
//            Signal_DataChanged := True;

            { And redraw the screen to display the change}
            RedrawScreen := True;
            CalculateAllSignalPositions;
            InvalidateScreen(UnitRef, 'EditSaveButtonClick');
            Log('D Screen invalidated by Edit save');
            RedrawScreen := False;
          END ELSE BEGIN
            IF NOT MessageDialogueWithDefault('Error in creating/amending S=' + IntToStr(EditedSignal) + ': '
                                              + ErrorMsg
                                              + CRLF
                                              + 'Do you wish to continue?',
                                              StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
            THEN
              Signal_OutOfUse := True
            ELSE
              TurnEditModeOff;
          END;
        END; {WITH}
      END;

      IF SavePointFoundNum <> UnknownPoint THEN BEGIN
        WITH Points[SavePointFoundNum] DO BEGIN
          ErrorMsg := '';

          IF KeyName = Point_HeelLineFieldName THEN BEGIN
            Point_HeelLine := ValidatePointHeelLineName(NewKeyValue, ErrorMsg);
            IF ErrorMsg = '' THEN
              Point_TCAtHeel := Lines[Point_HeelLine].Line_TC;
            IF (ErrorMsg <> '') AND (Point_HeelLine = UnknownLine) THEN BEGIN
              Point_HeelLine := SavePointHeelLine;
              Values[KeyName] := LineToStr(SavePointHeelLine);
            END;
          END;

          IF ErrorMsg = '' THEN BEGIN
            IF KeyName = Point_StraightLineFieldName THEN BEGIN
              Point_StraightLine := ValidatePointStraightLineName(NewKeyValue, ErrorMsg);
              IF (ErrorMsg <> '') AND (Point_StraightLine = UnknownLine) THEN BEGIN
                Point_StraightLine := SavePointStraightLine;
                Values[KeyName] := LineToStr(SavePointStraightLine);
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
                Point_DivergingLine := SavePointDivergingLine;
                Values[KeyName] := LineToStr(SavePointDivergingLine);
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
              Point_LastFeedbackStateAsReadIn := ValidateLastPointFeedbackStateAsReadIn(NewKeyValue, Point_ManualOperation, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName =  Point_LastManualStateAsReadInFieldName THEN
              Point_LastManualStateAsReadIn := ValidateLastPointManualStateAsReadIn(NewKeyValue, Point_ManualOperation, ErrorMsg);

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
              Point_FeedbackUnit := ValidatePointFeedbackUnit(NewKeyValue, Point_HasFeedback, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_FeedbackInputFieldName THEN
              Point_FeedbackInput := ValidatePointFeedbackInput(NewKeyValue, Point_HasFeedback, ErrorMsg);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_FeedbackOnIsStraightFieldName THEN
              Point_FeedbackOnIsStraight := StrToBool(NewKeyValue);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_WiringReversedFlagFieldName THEN
              Point_WiringReversedFlag := StrToBool(NewKeyValue);

          IF ErrorMsg = '' THEN
            IF KeyName = Point_OtherPointFieldName THEN
              Point_OtherPoint := ValidatePointOtherPoint(NewKeyValue, Point_Type, ErrorMsg);

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

          IF ErrorMsg = '' THEN
            Point_DataChanged := True;

          IF ErrorMsg = '' THEN BEGIN
            { And redraw the screen to display the change}
            RedrawScreen := True;
            CalculatePointPositions;
            InvalidateScreen(UnitRef, 'EditSaveButtonClick');
            Log('D Screen invalidated by Edit save');
            RedrawScreen := False;
          END ELSE BEGIN
            IF NOT MessageDialogueWithDefault('Error in creating/amending P=' + IntToStr(SavePointFoundNum) + ': '
                                              + ErrorMsg
                                              + CRLF
                                              + 'Do you wish to continue editing?',
                                              StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
            THEN
              Point_OutOfUse := True
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
    IF EditedSignal <> UnknownSignal THEN BEGIN
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

PROCEDURE WriteIntegerValue(Str : String; I : Integer; EditMask : String);
BEGIN
  WITH EditWindow.EditValueListEditor DO BEGIN
    IF I <> 0 THEN
      Values[Str] := IntToStr(I)
    ELSE
      Values[Str] := '';

    IF EditMask <> '' THEN
      ItemProps[Str].EditMask := EditMask;
  END; {WITH}
END; { WriteIntegerValue }

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

PROCEDURE ClearValueList;
{ Empty the value list so as not to display anything if we click on an unrecognised item, or a blank bit of screen }
BEGIN
  ValueListToBeCleared := True;

  WITH EditWindow DO BEGIN
    EditValueListEditor.Strings.Clear;
    EditWindowLabel.Caption := '';
  END; {WITH}
END; { ClearValueList }

PROCEDURE DisplayPointOptionsInValueList(P : Integer; SaveVariables : Boolean);
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

BEGIN { DisplayPointOptionsInValueList }
  TRY
    WITH EditWindow DO BEGIN
      { Temporarily deactivate the OnstringsChange event so as not to enable the various buttons until we've written the values to the value list - otherwise the act of
        writing them switches the buttons on before we've done any editing
      }
      EditValueListEditor.OnStringsChange := NIL;
      SaveEvent := EditValueListEditorStringsChange;

      WITH EditValueListEditor DO BEGIN
        EditValueListEditor.Strings.Clear;
        EditWindowLabel.Caption := 'Editing Point ' + IntToStr(P);

        WITH Points[P] DO BEGIN
          WriteIntegerValue(Point_NumberFieldName, P, '');
          ItemProps[Point_NumberFieldName].ReadOnly := True;

          Values[Point_DivergingLineFieldName] := LineToStr(Point_DivergingLine);
          Values[Point_HeelLineFieldName] := LineToStr(Point_HeelLine);
          Values[Point_StraightLineFieldName] := LineToStr(Point_StraightLine);

          WriteIntegerValue(Point_LenzNumFieldName, Point_LenzNum, '9999');
          WriteIntegerValue(Point_LenzUnitFieldName, Point_LenzUnit, '9999');
          WritePickListValue(Point_LenzUnitTypeFieldName, Point_LenzUnitType, ['Ls101', 'LS150']);
          WriteBooleanValue(Point_ManualOperationFieldName, Point_ManualOperation);
          WriteIntegerValue(Point_FeedbackUnitFieldName, Point_FeedbackUnit, '9999');
          WriteIntegerValue(Point_FeedbackInputFieldName, Point_FeedbackInput, '9999');
          WriteBooleanValue(Point_FeedbackOnIsStraightFieldName, Point_FeedbackOnIsStraight);
          WriteBooleanValue(Point_WiringReversedFlagFieldName, Point_WiringReversedFlag);
          WritePickListValue(Point_TypeFieldName, PointTypeToStr(Point_Type), ['Ordinary Point', 'Cross Over Point', 'Three Way Point A', 'Three Way Point B',
                                                                               'Single Slip', 'Double Slip', 'Protected Point', 'Catch Point Up', 'Catch Point Down',
                                                                               'Unknown Point Type']);
          WritePointValue(Point_OtherPointFieldName, Point_OtherPoint, '9999');
          WritePickListValue(Point_DefaultStateFieldName, PointStateToStr(Point_DefaultState), ['straight', 'diverging', 'unknown', 'out of action']);
          WriteBooleanValue(Point_LockedIfHeelTCOccupiedFieldName, Point_LockedIfHeelTCOccupied);
          WriteBooleanValue(Point_OutOfUseFieldName, Point_OutOfUse);
          WriteBooleanValue(Point_LockedIfNonHeelTCsOccupiedFieldName, Point_LockedIfNonHeelTCsOccupied);
          Values[Point_NotesFieldName] := Point_Notes;
        END; {WITH}
      END; {WITH}
    END; {WITH}

    IF SaveVariables THEN
      SavePointVariables(P);
    SavePointFoundNum := P;

    { Reactivate this event so as to enable the various buttons if we do any editing }
    EditWindow.EditValueListEditor.OnStringsChange := SaveEvent;

  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DisplayPointOptionsInValueList: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DisplayPointOptionsInValueList }

PROCEDURE DisplaySignalOptionsInValueList(S : Integer; SaveVariablesOnExit : Boolean);
{ Create a value list in the edit window with the appropriate values }

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

BEGIN { DisplaySignalOptionsInValueList }
  TRY
    WITH EditWindow DO BEGIN
      { Temporarily deactivate the OnstringsChange event so as not to enable the various buttons until we've written the values to the value list - otherwise the act of
        writing them switches the buttons on before we've done any editing
      }
      EditWindow.EditValueListEditor.OnStringsChange := NIL;
      SaveEvent := EditWindow.EditValueListEditorStringsChange;

      WITH EditValueListEditor DO BEGIN
        EditValueListEditor.Strings.Clear;

        WITH Signals[S] DO BEGIN
          WriteIntegerValue(Signal_NumberFieldName, S, '');
          { make the signal number read only }
          ItemProps[Signal_NumberFieldName].ReadOnly := True;

          EditWindowLabel.Caption := 'Editing Signal ' + IntToStr(S);

          WriteIntegerValue(Signal_AccessoryAddressFieldName, Signal_AccessoryAddress, '9999');

          Values[Signal_AdjacentLineFieldName] := LineToStr(Signal_AdjacentLine);
          WriteIntegerValue(Signal_AdjacentLineXOffsetFieldName, Signal_AdjacentLineXOffset, '!#99');

          IF Signal_ApproachControlAspect <> NoAspect THEN
            Values[Signal_ApproachControlAspectFieldName] := AspectToStr(Signal_ApproachControlAspect)
          ELSE
            Values[Signal_ApproachControlAspectFieldName] := '';

          Values[Signal_AsTheatreDestinationFieldName] := Signal_AsTheatreDestination;
          WriteBooleanValue(Signal_AutomaticFieldName + ' [not yet in use]', Signal_Automatic); { not yet in use }
          WriteIntegerValue(Signal_DecoderNumFieldName, Signal_DecoderNum, '9999');
          WritePickListValue(Signal_DirectionFieldName, DirectionToStr(Signal_Direction), ['Up', 'Down']);
          WriteIntegerValue(Signal_IndicatorDecoderFunctionNumFieldName, Signal_IndicatorDecoderFunctionNum, '9999');
          WriteIntegerValue(Signal_IndicatorDecoderNumFieldName, Signal_IndicatorDecoderNum, '9999');
          WritePickListValue(Signal_IndicatorFieldName, IndicatorToStr(Signal_Indicator, LongStringType), ['No Indicator', 'Junction Indicator', 'Theatre Indicator']);
          WritePickListValue(Signal_IndicatorSpeedRestrictionFieldName, MPHToStr(Signal_IndicatorSpeedRestriction),
                                                                                ['10', '20', '30', '40', '50', '60', '70', '80', '90', '100', '110']);
          WriteSignalValue(Signal_NextSignalIfNoIndicatorFieldName, Signal_NextSignalIfNoIndicator, '999');
          WriteJunctionIndicatorValue(S, Signal_UpperLeftIndicatorTargetFieldName, UpperLeftIndicator);
          WriteJunctionIndicatorValue(S, Signal_MiddleLeftIndicatorTargetFieldName, MiddleLeftIndicator);
          WriteJunctionIndicatorValue(S, Signal_LowerLeftIndicatorTargetFieldName, LowerLeftIndicator);
          WriteJunctionIndicatorValue(S, Signal_UpperRightIndicatorTargetFieldName, UpperRightIndicator);
          WriteJunctionIndicatorValue(S, Signal_MiddleRightIndicatorTargetFieldName, MiddleRightIndicator);
          WriteJunctionIndicatorValue(S, Signal_LowerRightIndicatorTargetFieldName, LowerRightIndicator);
          WriteIntegerArrayValues(Signal_LocationsToMonitorFieldName, Signal_LocationsToMonitorArray, '(No Locations)', '(Locations)');
          Values[Signal_NotesFieldName] := Signal_Notes;
          WriteBooleanValue(Signal_NotUsedForRouteingFieldName, Signal_NotUsedForRouteing);
          WriteSignalValue(Signal_OppositePassingLoopSignalFieldName, Signal_OppositePassingLoopSignal, 'S999');
          WriteBooleanValue(Signal_PossibleRouteHoldFieldName, Signal_PossibleRouteHold);
          WriteBooleanValue(Signal_PossibleStationStartRouteHoldFieldName, Signal_PossibleStationStartRouteHold);

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

          WritePickListValue(Signal_TypeFieldName, SignalTypeToStr(Signal_Type, LongStringType),
                                                                                 ['Calling On', '2 Aspect', '3 Aspect', '4 Aspect', 'Home Semaphore', 'Distant Semaphore']);
          WritePickListValue(Signal_QuadrantFieldName, SignalQuadrantToStr(Signal_Quadrant), ['Upper', 'Lower']);
          WriteBooleanValue(Signal_OutOfUseFieldName, Signal_OutOfUse);
        END; {WITH}
      END; {WITH}

      IF SaveVariablesOnExit THEN
        SaveSignalVariables(S);
      EditedSignal := S;
      ExitWithoutSavingButton.Enabled := True;

      { Reactivate this event so as to enable the various buttons if we do any editing }
      EditValueListEditor.OnStringsChange := SaveEvent;
    END; {WEITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DisplaySignalOptionsInValueList: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DisplaySignalOptionsInValueList }

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
CONST
  SaveVariables = True;

BEGIN
  TRY
    { Tag represents the signal number, and is set by whichever procedure sets the edit window to visible }
    CASE Tag OF
      1:
        DisplaySignalOptionsInValueList(EditedSignal, SaveVariables);
      2:
        DisplayPointOptionsInValueList(SavePointFoundNum, SaveVariables);
    END; {CASE}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG EditWindowShow: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { EditWindowShow }

PROCEDURE UndoChanges;
CONST
  SaveVariables = True;

BEGIN
  TRY
    { Undo the changes to the global variables and redisplay the value list }
    IF EditedSignal <> UnknownSignal THEN BEGIN
      WITH Signals[EditedSignal] DO BEGIN
        Signal_AccessoryAddress := SaveSignalAccessoryAddress;
        Signal_AdjacentLine := SaveSignalAdjacentLine;
        Signal_AdjacentLineXOffset := SaveSignalAdjacentLineXOffset;
        Signal_ApproachControlAspect := SaveSignalApproachControlAspect;
        Signal_AsTheatreDestination := SaveSignalAsTheatreDestination;
        Signal_Automatic := SaveSignalAutomatic;
        Signal_DecoderNum := SaveSignalDecoderNum;
        Signal_Direction := SaveSignalDirection;
        Signal_Indicator := SaveSignalIndicator;
        Signal_IndicatorDecoderFunctionNum := SaveSignalIndicatorDecoderFunctionNum;
        Signal_IndicatorDecoderNum := SaveSignalIndicatorDecoderNum;
        Signal_IndicatorSpeedRestriction := SaveSignalIndicatorSpeedRestriction;
        Signal_JunctionIndicators[LowerLeftIndicator] := SaveSignalJunctionIndicators[LowerLeftIndicator];
        Signal_JunctionIndicators[LowerRightIndicator] := SaveSignalJunctionIndicators[LowerRightIndicator];
        Signal_JunctionIndicators[MiddleLeftIndicator] := SaveSignalJunctionIndicators[MiddleLeftIndicator];
        Signal_JunctionIndicators[MiddleRightIndicator] := SaveSignalJunctionIndicators[MiddleRightIndicator];
        Signal_JunctionIndicators[UpperLeftIndicator] := SaveSignalJunctionIndicators[UpperLeftIndicator];
        Signal_JunctionIndicators[UpperRightIndicator] := SaveSignalJunctionIndicators[UpperRightIndicator];
        Signal_LocationsToMonitorArray := SaveSignalLocationsToMonitorArray;
        Signal_NextSignalIfNoIndicator := SaveSignalNextSignalIfNoIndicator;
        Signal_NotUsedForRouteing := SaveSignalNotUsedForRouteing;
        Signal_Notes := SaveSignalNotes;
        Signal_OppositePassingLoopSignal := SaveSignalOppositePassingLoopSignal;
        Signal_OutOfUse := SaveSignalOutOfUse;
        Signal_PossibleRouteHold := SaveSignalPossibleRouteHold;
        Signal_PossibleStationStartRouteHold := SaveSignalPossibleStationStartRouteHold;
        Signal_SemaphoreDistantHomesArray := SaveSignalSemaphoreDistantHomesArray;
        Signal_Type := SaveSignalType;
      END; {WITH}

      CreateNewSignalRecord := False;
      DisplaySignalOptionsInValueList(EditedSignal, SaveVariables);
      CalculateTCAdjacentSignals;
      CalculateAllSignalPositions;
    END;

    IF SavePointFoundNum <> UnknownPoint THEN BEGIN
      WITH Points[SavePointFoundNum] DO BEGIN
        Point_DefaultState := SavePointDefaultState;
        Point_DivergingLine := SavePointDivergingLine;
        Point_FeedbackInput := SavePointFeedbackInput;
        Point_FeedbackOnIsStraight := SavePointFeedbackOnIsStraight;
        Point_FeedbackUnit := SavePointFeedbackUnit;
        Point_HeelLine := SavePointHeelLine;
        Point_LastFeedbackStateAsReadIn := SavePointLastManualStateAsReadIn;
        Point_LastManualStateAsReadIn := SavePointLastManualStateAsReadIn;
        Point_LenzNum := SavePointLenzNum;
        Point_LenzUnit := SavePointLenzUnit;
        Point_LenzUnitType := SavePointLenzUnitType;
        Point_LockedIfHeelTCOccupied := SavePointLockedIfHeelTCOccupied;
        Point_LockedIfNonHeelTCsOccupied := SavePointLockedIfNonHeelTCsOccupied;
        Point_ManualOperation := SavePointManualOperation;
        Point_Notes := SavePointNotes;
        Point_OtherPoint := SavePointOtherPoint;
        Point_OutOfUse := SavePointOutOfUse;
        Point_StraightLine := SavePointStraightLine;
        Point_Type := SavePointType;
        Point_WiringReversedFlag := SavePointWiringReversedFlag;
      END; {WITH}

      CreateNewPointRecord := False;
      DisplayPointOptionsInValueList(SavePointFoundNum, SaveVariables);
      CalculatePointPositions;
    END;

    { And redraw the screen }
    RedrawScreen := True;
    InvalidateScreen(UnitRef, 'UndoChangesButtonClick');
    Log('D Screen invalidated by Edit Undo Changes');
    RedrawScreen := False;

    EditWindow.UndoChangesButton.Enabled := False;
    EditWindow.SaveChangesAndExitButton.Enabled := False;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG UndoChangesButtonClick: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { UndoChangesButtonClick }

PROCEDURE TEditWindow.UndoChangesButtonClick(Sender: TObject);
BEGIN
  UndoChanges;
END; { UndoChangesButtonClick }

PROCEDURE TEditWindow.SaveChangesAndExitButtonClick(Sender: TObject);
BEGIN
  TRY
    { Update the database }
    IF CreateNewSignalRecord THEN BEGIN
      CreateNewSignalRecord := False;
      AddNewRecordToSignalDatabase;
    END;

    IF EditedSignal <> UnknownSignal THEN BEGIN
      WriteOutSignalDataToDatabase;
      CalculateTCAdjacentSignals;
      CalculateAllSignalPositions;

      EditedSignal := UnknownSignal;
    END;

    IF SavePointFoundNum <> UnknownPoint THEN BEGIN
      WriteOutPointDataToDatabase;
      CalculatePointPositions;

      EditedPoint := UnknownPoint;
    END;

    ClearValueList;

    { And redraw the screen }
    RedrawScreen := True;
    CalculateTCAdjacentSignals;
    CalculateAllSignalPositions;
    InvalidateScreen(UnitRef, 'EditSaveButtonClick');
    Log('D Screen invalidated by Edit Save And Exit');
    RedrawScreen := False;

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
    UndoChanges;

    IF CreateNewSignalRecord THEN BEGIN
      CreateNewSignalRecord := False;

      { Truncate the signal data array to get rid of the new signal we added in draft }
      SetLength(Signals, Length(Signals) - 1);
    END;

    { Undo the data-changed settings }
    IF EditedSignal <> UnknownSignal THEN BEGIN
      Signals[EditedSignal].Signal_DataChanged := False;
      EditedSignal := UnknownSignal;

      FWPRailWindow.Repaint;
    END;

    IF SavePointFoundNum <> UnknownPoint THEN BEGIN
      Points[SavePointFoundNum].Point_DataChanged := False;
      SavePointFoundNum := UnknownPoint;
    END;

    IF CreateNewPointRecord THEN BEGIN
      CreateNewPointRecord := False;

      { Truncate the point data array to get rid of the new point we added in draft }
      SetLength(Points, Length(Points) - 1);
    END;

    ExitWithoutSavingButton.Enabled := False;
    UndoChangesButton.Enabled := False;
    SaveChangesAndExitButton.Enabled := False;
    ClearValueList;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ExitWithSavingButtonClick: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ExitWithSavingButtonClick }

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
CONST
  SaveVariables = True;

VAR
  ExistingSignalFoundAttachedToLine : Boolean;
  S : Integer;
  TempJunctionIndicator : JunctionIndicatorType;

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
        { Now create a basic signal which must then be added to using the value list editor }
        SetLength(Signals, Length(Signals) + 1);
        WITH Signals[High(Signals)] DO BEGIN
          Signal_AccessoryAddress := 0;
          Signal_AdjacentLine := Line;
          Signal_AdjacentLineXOffset := 0;
          Signal_AdjacentTC := UnknownTrackCircuit;
          Signal_ApproachControlAspect := NoAspect;
          Signal_ApproachLocked := False;
          Signal_AsTheatreDestination := '';
          Signal_Aspect := RedAspect;
          Signal_Automatic := False; { not yet implemented }
          Signal_DataChanged := True;
          Signal_DecoderNum := 0;
          Signal_Direction := Direction;
          Signal_Energised := False;
          Signal_EnergisedTime := 0;
          Signal_FailMsgWritten := False;
          Signal_FailedToResetFlag := False;
          Signal_FindNextSignalBufferStopMsgWritten := False;
          Signal_HiddenStationSignalAspect := NoAspect;
          Signal_Indicator := NoIndicator;
          Signal_IndicatorDecoderFunctionNum := 0;
          Signal_IndicatorDecoderNum := 0;
          Signal_IndicatorSpeedRestriction := NoSpecifiedSpeed;
          Signal_IndicatorState := NoIndicatorLit;
          FOR TempJunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
            Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_Exists := False;
            Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetSignal := UnknownSignal;
            Signal_JunctionIndicators[TempJunctionIndicator].JunctionIndicator_TargetBufferStop := UnknownSignal;
          END; {FOR}
          Signal_LampIsOn := True;
          SetLength(Signal_LocationsToMonitorArray, 0);
          Signal_LockFailureNotedInRouteUnit := False;
          SetLength(Signal_LockedArray, 0);
          Signal_NextSignalIfNoIndicator := UnknownSignal;
          Signal_NotUsedForRouteing := False;
          Signal_Notes := 'Created by user on ' + DateToStr(Date);
          Signal_OppositePassingLoopSignal := UnknownSignal;
          Signal_OutOfUse := True;
          Signal_OutOfUseMsgWritten := False;
          Signal_PossibleRouteHold := False;
          Signal_PossibleStationStartRouteHold := False;
          Signal_PostColour := ForegroundColour;
          Signal_PreviousAspect := NoAspect;
          Signal_PreviousHiddenStationSignalAspectSignal1 := UnknownSignal;
          Signal_PreviousHiddenStationSignalAspectSignal2 := UnknownSignal;
          Signal_PreviousIndicatorState := NoIndicatorLit;
          Signal_PreviousSignal1 := UnknownSignal;
          Signal_PreviousSignal2 := UnknownSignal;
          Signal_PreviousTheatreIndicatorString := '';
          Signal_ResettingTC := UnknownTrackCircuit;
          SetLength(Signal_RouteLockingNeededArray, 0);
          SetLength(Signal_SemaphoreDistantHomesArray, 0);
          Signal_SemaphoreDistantLocking := UnknownSignal;
          Signal_StateChanged := False;
          Signal_TRSHeld := False;
          Signal_TRSHeldMsgWritten := False;
          Signal_TRSReleased := False;
          Signal_TRSReleasedMsgWritten := False;
          Signal_TheatreIndicatorString := '';
          Signal_Type := TwoAspect;
        END; {WITH}

        CreateNewSignalRecord := True;
        DisplaySignalOptionsInValueList(High(Signals), SaveVariables);      { should this be "EditedSignal"? **** }

        RedrawScreen := True;
        CalculateTCAdjacentSignals;
        CalculateAllSignalPositions;
        InvalidateScreen(UnitRef, 'CreateSignal');
        Log('D Screen invalidated by CreateSignal');
        RedrawScreen := False;
      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CreateSignal: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CreateSignal }

PROCEDURE DeleteSignal(SignalToDeleteNum : Integer);
{ Delete a signal after appropriate checks }
CONST
  NewSignalData = True;

VAR
  CanDelete : Boolean;
  JunctionIndicator : JunctionIndicatorType;
  OtherSignal : Integer;
  S : Integer;
  SignalDeletedNum : Integer;
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
        WITH Signals[OtherSignal] DO BEGIN
          { Save various variables in case the deletion fails }
          SaveSignalVariables(OtherSignal);

          { Now look at the individual fields that might need renumbering }
          IF Signal_OppositePassingLoopSignal = SignalToDeleteNum THEN BEGIN
            IF MessageDialogueWithDefault('Cannot delete S=' + IntToStr(SignalToDeleteNum)
                                          + ' until it ceases to be the opposite passing loop signal for S=' + IntToStr(OtherSignal)
                                          + CRLF
                                          + 'Do you wish to remove the reference?',
                                          StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
            THEN
              CanDelete := False
            ELSE BEGIN
              { Remove the reference to this signal from the other signal's database entry }
              Signal_OppositePassingLoopSignal := UnknownSignal;
              Signal_DataChanged := True;
            END;
          END;

          IF CanDelete THEN BEGIN
            IF Signal_NextSignalIfNoIndicator = SignalToDeleteNum THEN BEGIN
              IF MessageDialogueWithDefault('Cannot delete S' + IntToStr(SignalToDeleteNum)
                                            + ' until it ceases to be the next signal if no indicator for S' + IntToStr(OtherSignal)
                                            + CRLF
                                            + 'Do you wish to remove the reference?',
                                            StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
              THEN
                CanDelete := False
              ELSE BEGIN
                { Remove the reference to this signal from the other signal's database entry }
                Signal_NextSignalIfNoIndicator := UnknownSignal;
                Signal_DataChanged := True;
                WriteOutSignalDataToDatabase;
              END;
            END;
          END;

          IF CanDelete THEN BEGIN
            FOR JunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
              IF Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_Exists THEN BEGIN
                IF Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal = SignalToDeleteNum THEN BEGIN
                  IF MessageDialogueWithDefault('Cannot delete S=' + IntToStr(SignalToDeleteNum)
                                                + ' until it ceases to be the target signal for'
                                                + ' S' + IntToStr(OtherSignal) + '''s ' + JunctionIndicatorTypeToStr(JunctionIndicator)
                                                + CRLF
                                                + 'Do you wish to remove the reference?',
                                                StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
                  THEN
                    CanDelete := False
                  ELSE BEGIN
                    { Remove the reference to this signal from the other signal's database entry }
                    Signal_Indicator := NoIndicator;
                    Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_Exists := False;
                    Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal := UnknownSignal;
                    Signal_DataChanged := True;

                    { and notify if there is a consequential change to the signal's data - otherwise there will be an error when the signal data is reloaded }
                    IF Signal_IndicatorSpeedRestriction <> NoSpecifiedSpeed THEN
                      Signal_IndicatorSpeedRestriction := NoSpecifiedSpeed;
                  END;
                END;
              END;
            END; {FOR}
          END;

          IF CanDelete THEN BEGIN
            SignalPos := GetElementPosInIntegerArray(Signal_SemaphoreDistantHomesArray, SignalToDeleteNum);
            IF SignalPos > -1 THEN BEGIN
              IF MessageDialogueWithDefault('Cannot delete S=' + IntToStr(SignalToDeleteNum)
                                            + ' until it ceases to be a home signal in S' + IntToStr(OtherSignal) + '''s DistantHomesArray'
                                            + CRLF
                                            + 'Do you wish to remove the reference?',
                                            StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
              THEN
                CanDelete := False
              ELSE BEGIN
                { Remove the reference to this signal from the other signal's database entry }
                DeleteElementFromIntegerArray(Signal_SemaphoreDistantHomesArray, SignalPos);
                Signal_DataChanged := True;
              END;
            END;
          END;

          IF NOT CanDelete THEN
            Signal_DataChanged := False;

          IF Signal_DataChanged THEN
            WriteOutSignalDataToDatabase;
        END; {WITH}
        Inc(OtherSignal);
      END;

      IF CanDelete THEN BEGIN
        { Now we need to renumber signal references in the database that have changed because of the deletion }
        FOR S := 0 TO High(Signals) DO BEGIN
          WITH Signals[S] DO BEGIN
            { we only need to decrement signal numbers from the signal we deleted to the signal number equal to the end of the new array - plus one, the signal we deleted }
            FOR SignalDeletedNum := SignalToDeleteNum TO High(Signals) + 1 DO BEGIN
              IF Signals[S].Signal_OppositePassingLoopSignal = SignalDeletedNum THEN
                Signal_OppositePassingLoopSignal := SignalDeletedNum - 1;

              IF Signal_NextSignalIfNoIndicator = SignalDeletedNum THEN
                Signal_NextSignalIfNoIndicator := SignalDeletedNum - 1;

              FOR JunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
                IF Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_Exists THEN BEGIN
                  IF Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal = SignalDeletedNum THEN BEGIN
                    Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal := SignalDeletedNum - 1;
                    Signal_DataChanged := True;
                  END;
                END;
              END; {FOR}

              SignalPos := GetElementPosInIntegerArray(Signal_SemaphoreDistantHomesArray, SignalDeletedNum);
              IF SignalPos > -1 THEN BEGIN
                Signal_SemaphoreDistantHomesArray[SignalPos] := SignalDeletedNum - 1;
                Signal_DataChanged := True;
              END;

            END; {FOR}
            IF Signal_DataChanged THEN
              WriteOutSignalDataToDatabase;
          END; {WITH}
        END; {FOR}
      END;

      IF CanDelete THEN BEGIN
        IF NOT DeleteRecordFromSignalDatabaseAndRenumberSignals(SignalToDeleteNum) THEN BEGIN
          Log('S! Cannot delete S=' + IntToStr(SignalToDeleteNum));

          { and restore the variables }
          WITH Signals[SignalToDeleteNum] DO BEGIN
            Signal_NextSignalIfNoIndicator := SaveSignalNextSignalIfNoIndicator;
            Signal_OppositePassingLoopSignal := SaveSignalOppositePassingLoopSignal;
            Signal_Indicator := SaveSignalIndicator;
            FOR JunctionIndicator := UpperLeftIndicator TO LowerRightIndicator DO BEGIN
              Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_Exists := SaveSignalJunctionIndicators[JunctionIndicator].JunctionIndicator_Exists;
              Signal_JunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal := SaveSignalJunctionIndicators[JunctionIndicator].JunctionIndicator_TargetSignal;
            END; {FOR}
            Signal_IndicatorSpeedRestriction := SaveSignalIndicatorSpeedRestriction;
            FOR SignalPos := 0 TO High(Signal_SemaphoreDistantHomesArray) DO
              Signal_SemaphoreDistantHomesArray[SignalPos] := SaveSignalSemaphoreDistantHomesArray[SignalPos];

            Signal_DataChanged := True;
            WriteOutSignalDataToDatabase;
          END; {WITH}
        END ELSE BEGIN
          { Clear the data from the value-list editor }
          WITH EditWindow DO BEGIN
            EditWindowLabel.Caption := '';
            EditValueListEditor.Strings.Clear;
            UndoChangesButton.Enabled := False;
            SaveChangesAndExitButton.Enabled := False;
            ExitWithoutSavingButton.Enabled := False;
          END; {WITH}

          { Reload all the signals }
          ReadInSignalDataFromDatabase(NewSignalData);

          RedrawScreen := True;
          CalculateTCAdjacentSignals;
          CalculateAllSignalPositions;
          InvalidateScreen(UnitRef, 'DeleteSignal');
          Log('D Screen invalidated by Delete Signal');
          RedrawScreen := False;
        END;
      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DeleteSignal: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DeleteSignal }

PROCEDURE CreatePoint(Direction : DirectionType; Line : Integer);
{ Creates a point from scratch }
//VAR
//  P : Integer;

BEGIN
  TRY
    IF Line = UnknownLine THEN
      { this shouldn't happen, but just in case it does... }
      ShowMessage('A new point must be near an existing line')
    ELSE BEGIN
      Debug(LineToStr(Line));
      Debug('Line_UpXLine=' + Lines[Line].Line_UpXLineStr);


(*
      P := 0;

      { Now create a basic point which must then be added to using the value list editor }
      SetLength(Points, Length(Points) + 1);
      WITH Points[High(Points)] DO BEGIN
        Point_DivergingLine := UnknownLine;
        Point_HeelLine := UnknownLine;
        Point_StraightLine := UnknownLine;

        Point_AwaitingManualChange := False;
        Point_DataChanged := True;
        Point_DefaultState := PointStateUnknown;
        Point_Energised := False;
        Point_EnergisedTime := 0;
        Point_FacingDirection := UnknownDirection;
        Point_FarX := 0;
        Point_FarY := 0;
        Point_FeedbackOnIsStraight := False;
        Point_FeedbackPending := False;
        Point_FeedbackPendingMsgWritten := False;
        Point_FeedbackStartTime := 0;
        Point_FeedbackUnit := 0;
        Point_FeedbackInput := 0;
        Point_ForcedDelayMsg1Written := False;
        Point_ForcedDelayMsg2Written := False;
        Point_HasFeedback := False;
        Point_LastChangedTime := 0;
        Point_LenzNum := 0;
        Point_LenzUnit := 0;
        Point_LenzUnitType := '';
        Point_LockedByUser := False;
        Point_LockedIfHeelTCOccupied := False;
        Point_LockedIfNonHeelTCsOccupied := False;
        Point_LockFailureNotedInLocksUnit := False;
        Point_LockFailureNotedInSubRouteUnit := False;
        SetLength(Point_LockingArray, 0);
        Point_LockingState := PointStateUnknown;
        Point_ManualOperation := False;
        Point_ManualStateAsReadIn := PointStateUnknown;
        Point_MaybeBeingSetToManual := False;
        Point_MovedWhenLocked := False;
        Point_Notes := 'Created by user on ' + DateToStr(Date);
        Point_OtherPoint := UnknownPoint;
        Point_OutOfUse := True;
        Point_PresentState := PointStateUnknown;
        Point_PreviousState := PointStateUnknown;
        Point_RequiredState := PointStateUnknown;
        Point_ResettingTime := 0;
        Point_RouteLockedByLocoChip := UnknownLocoChip;
        Point_TCAtHeel := 0;
        Point_Type := PointTypeUnknown;
        Point_SecondAttempt := False;
        Point_SetASecondTime := False;
        Point_WaitTime := 0;
        Point_WiringReversedFlag := False;
        Point_X := 0;
        Point_Y := 0;
      END; {WITH}

      CreateNewPointRecord := True;
      DisplayPointOptionsInValueList(High(Points));

      RedrawScreen := True;
      CalculatePointPositions;
      InvalidateScreen(UnitRef, 'CreatePoint');
      Log('D Screen invalidated by CreatePoint');
      RedrawScreen := False;
*)
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CreatePoint: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CreatePoint }

PROCEDURE DeletePoint(PointToDeleteNum : Integer);
{ Delete a point after appropriate checks }
CONST
  NewPointData = True;

//VAR
//  CanDelete : Boolean;

BEGIN
  TRY
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DeletePoint: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DeletePoint }

FUNCTION NearestLineToSignal(S : Integer) : Integer;
{ Returns the line number nearest to a dragged signal if the signal or its post are within a line's mouse rectangle }
VAR
  Line : Integer;
  LineFound : Boolean;

BEGIN
  Result := UnknownLine;
  TRY
    LineFound := False;
    Line := 0;
    WHILE (Line <= High(Lines)) AND NOT LineFound DO BEGIN
      IF PointInPolygon(Lines[Line].Line_MousePolygon, Point(Signals[S].Signal_LineX, Signals[S].Signal_LineWithVerticalSpacingY)) THEN
        LineFound := True
      ELSE
        Inc(Line);
    END; {WHILE}

    IF NOT LineFound THEN BEGIN
      { see if the base of the signal post is within the polygon }
      Line := 0;
      WHILE (Line <= High(Lines)) AND NOT LineFound DO BEGIN
        IF PointInPolygon(Lines[Line].Line_MousePolygon, Point(Signals[S].Signal_LineX, Signals[S].Signal_LineY)) THEN
          LineFound := True
        ELSE
          Inc(Line);
      END; {WHILE}
    END;

    IF LineFound THEN
      Result := Line;
  EXCEPT
    ON E : Exception DO
      Log('EG NearestLineToSignal:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { NearestLineToSignal }

FUNCTION TooNearOtherSignal(S : Integer) : Integer;
{ Returns true if the signal we're dragging/dropping is too close to another signal }
VAR
  OtherSignal : Integer;
  OtherSignalFound : Boolean;

BEGIN
  Result := UnknownSignal;
  TRY
    OtherSignalFound := False;
    OtherSignal := 0;
    WHILE (OtherSignal <= High(Signals)) AND NOT OtherSignalFound DO BEGIN
      IF (OtherSignal <> S) AND PtInRect(Signals[OtherSignal].Signal_MouseRect, Point(Signals[S].Signal_LineX, Signals[S].Signal_LineWithVerticalSpacingY)) THEN
        OtherSignalFound := True
      ELSE
        Inc(OtherSignal);
    END; {WHILE}

    IF OtherSignalFound THEN
      Result := OtherSignal;
  EXCEPT
    ON E : Exception DO
      Log('EG TooNearOtherSignal:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TooNearOtherSignal }

PROCEDURE DragSignal(S, MouseX, MouseY : Integer; OUT NearestLine, TooNearSignal : Integer);
{ Allows a signal to be moved by the mouse }
BEGIN
  TRY
    NearestLine := UnknownLine;
    TooNearSignal := UnknownLine;

    IF (DragSignalNum <> UnknownSignal) AND SignalDragging THEN BEGIN
      WITH Signals[DragSignalNum] DO BEGIN
        Signal_LineX := MouseX;
        Signal_LineWithVerticalSpacingY := MouseY;
        IF Signal_Direction = Up THEN
          Signal_LineY := MouseY - SignalVerticalSpacingScaled + ScrollBarYAdjustment
        ELSE
          Signal_LineY := MouseY + SignalVerticalSpacingScaled - ScrollBarYAdjustment;

        { Indicate whether or not it can be dropped }
        NearestLine := NearestLineToSignal(DragSignalNum);
        TooNearSignal := TooNearOtherSignal(DragSignalNum);
      END; {WITH}

      CalculateSignalMouseRectangles(DragSignalNum);
      DrawSignal(DragSignalNum);

      FWPRailWindow.Repaint;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DragSignal:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { DragSignal }

PROCEDURE DropSignal;
{ Drops the signal at the nearest adjacent line }
CONST
  SaveVariables = True;

VAR
  NearestLine : Integer;
  NewSignalX : Integer;
  NewSignalY : Integer;
  TooNearSignal : Integer;
  SaveSignalAdjacentLine : Integer;

BEGIN
  TRY
    SignalDragging := False;
    ChangeCursor(crDefault);

    WITH Signals[DragSignalNum] DO BEGIN
      { See if the signal is within a line polygon }
      NearestLine := NearestLineToSignal(DragSignalNum);

      IF NearestLine = UnknownSignal THEN BEGIN
        { put the signal back whence it came }
        Debug('!Cannot find a line anywhere near the dragged signal');
        Signal_LineX := SaveDragX;
        Signal_LineWithVerticalSpacingY := SaveDragY;
        CalculateSignalMouseRectangles(DragSignalNum);
        FWPRailWindow.Repaint;
      END ELSE BEGIN
        { Is another one at the exact spot? Record where it is going to be placed to carry out this test. }
        IF Signal_Direction = Up THEN
          NewSignalX := Lines[NearestLine].Line_UpX + SignalRadiusScaled
        ELSE
          { Down }
          NewSignalX := Lines[NearestLine].Line_DownX - SignalRadiusScaled;

        NewSignalY := Lines[NearestLine].Line_UpY;

        { See whether we're trying to drop the signal over another signal }
        TooNearSignal := TooNearOtherSignal(DragSignalNum);
        IF TooNearSignal <> UnknownSignal THEN BEGIN
          { put the newly-moved signal back whence it came }
          Debug('!Cannot drop the signal where there is an existing signal (S=' + IntToStr(TooNearSignal) +')');
          Signal_LineX := SaveDragX;
          Signal_LineWithVerticalSpacingY := SaveDragY;
          CalculateSignalMouseRectangles(DragSignalNum);
          FWPRailWindow.Repaint;
        END ELSE BEGIN
          { it's ok to place the signal here }
          SaveSignalAdjacentLine := Signal_AdjacentLine;

          Signal_PreviousLineX := NewSignalX;
          Signal_PreviousLineY := NewSignalY;
          Signal_PreviousLineWithVerticalSpacingY := Signal_LineWithVerticalSpacingY;

          Signal_AdjacentLine := NearestLine;
          Signal_AdjacentLineXOffset := 0;
          CalculateSignalPosition(DragSignalNum);
          CalculateSignalMouseRectangles(DragSignalNum);

          DrawSignal(DragSignalNum);
          Log('SG S' + IntToStr(DragSignalNum) + ' dragged from X=' + IntToStr(SaveDragX) + ' Y=' + IntToStr(SaveDragY) + ' (Line=' + LineToStr(SaveSignalAdjacentLine) + ')'
              + ' and dropped at X=' + IntToStr(Signal_LineX) + ' Y=' + IntToStr(Signal_LineY)  + ' (Line=' + LineToStr(Signal_AdjacentLine) + ')');

          DisplaySignalOptionsInValueList(EditedSignal, NOT SaveVariables);
          DragSignalNum := UnknownSignal;
          Signal_DataChanged := True;
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
CONST
  SaveVariables = True;

BEGIN
  IF (EditedSignal <> UnknownSignal) AND (DragSignalNum = UnknownSignal) THEN BEGIN
    WITH Signals[EditedSignal] DO BEGIN
      Dec(Signals[EditedSignal].Signal_AdjacentLineXOffset);

      CalculateSignalPosition(EditedSignal);
      CalculateSignalMouseRectangles(EditedSignal);

      DisplaySignalOptionsInValueList(EditedSignal, NOT SaveVariables);

      Signal_DataChanged := True;
      FWPRailWindow.Repaint;
    END; {WITH}
    EditWindow.UndoChangesButton.Enabled := True;
    EditWindow.SaveChangesAndExitButton.Enabled := True;
  END;
END; {MoveObjectLeft }

PROCEDURE MoveObjectRight;
{ Moves whatever is currently being edited to the right }
CONST
  SaveVariables = True;

BEGIN
  IF (EditedSignal <> UnknownSignal) AND (DragSignalNum = UnknownSignal) THEN BEGIN
    WITH Signals[EditedSignal] DO BEGIN
      Signal_DataChanged := True;

      Inc(Signals[EditedSignal].Signal_AdjacentLineXOffset);

      CalculateSignalPosition(EditedSignal);
      CalculateSignalMouseRectangles(EditedSignal);

      DisplaySignalOptionsInValueList(EditedSignal, NOT SaveVariables);

      Signal_DataChanged := True;
      FWPRailWindow.Repaint;
    END; {WITH}
    EditWindow.UndoChangesButton.Enabled := True;
    EditWindow.SaveChangesAndExitButton.Enabled := True;
  END;
END; {MoveObjectRight }

PROCEDURE CheckWhetherEditedSignalDataHasChanged;
{ Ask whether we want to save any amended data before selecting another signal }
BEGIN
  IF EditedSignal <> UnknownSignal THEN BEGIN
    IF Signals[EditedSignal].Signal_DataChanged THEN BEGIN
      IF MessageDialogueWithDefault('You have made changes to S' + IntToStr(EditedSignal) + '.'
                                    + CRLF
                                    + 'Do you want to save those changes?',
                                    StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
      THEN
        WriteOutSignalDataToDatabase
      ELSE BEGIN
        Signals[EditedSignal].Signal_DataChanged := False;

        { We'd better undo them, then }
        UndoChanges;
      END;

      EditedSignal := UnknownSignal;
    END;
  END;
END; { CheckWhetherEditedSignalDataHasChanged }

END { Edit }.
