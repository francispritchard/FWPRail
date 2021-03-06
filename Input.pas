﻿UNIT Input;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Mask, ComCtrls, ExtCtrls, Buttons, Types, System.UITypes;

TYPE
  TInputDialogueBox = CLASS(TForm)
    InputDialogueCancelButton: TButton;
    InputDialogueChangeOrSelectButton: TButton;
    InputDialogueMaskEdit: TMaskEdit;
    InputDialogueMaskEditLabel: TLabel;
    InputDialogueShowAdjacentTrackCircuitsCheckBox: TCheckBox;
    PROCEDURE InputDialogueBoxHide(Sender: TObject);
    PROCEDURE InputDialogueBoxKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE InputDialogueBoxKeyUp(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE InputDialogueBoxShow(Sender: TObject);
    PROCEDURE InputDialogueCancelButtonClick(Sender: TObject);
    PROCEDURE InputDialogueChangeOrSelectButtonClick(Sender: TObject);
    PROCEDURE InputDialogueMaskEditChange(Sender: TObject);
    PROCEDURE InputDialogueMaskEditKeyPress(Sender: TObject; VAR Key: Char);
    PROCEDURE InputDialogueMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE InputDialogueMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE InputDialogueShowAdjacentTrackCircuitsCheckBoxClick(Sender: TObject);
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

TYPE
  InputDialogueBoxType = (SignalDialogueBox, PointDialogueBox, TrackCircuitDialogueBox, LineDialogueBox, NoDialogueBox);

FUNCTION DescribeKey(Key : Word; ShiftState : TShiftState) : String;
{ Return a description of the key that's been pressed }

PROCEDURE KeyPressedDown{1}(KeyToTest : Word; InputShiftState : TShiftState); Overload;
{ Called when a key is pressed }

PROCEDURE KeyPressedDown{2}(KeyToTest : Word; InputShiftState : TShiftState; HelpRequired : Boolean; OUT HelpMsg : String); Overload;
{ Called when a key is pressed or help on keys is required }

PROCEDURE SetAndShowInputDialogueBox(InputDialogueBox : InputDialogueBoxType);
{ Set and the display the appropriate input dialogue box }

PROCEDURE SetUpStationMonitors(ShiftState : TShiftState; DisplayOrderNum : Integer);
{ Chooses which station we're going to or from and how to show the results }

VAR
  InputDialogueBox: TInputDialogueBox;

IMPLEMENTATION

{$R *.dfm}

USES InitVars, Locks, RailDraw, MiscUtils, Cuneo, LocoUtils, Lenz, MaskUtils, Startup, Diagrams, GetTime, CreateRoute, Feedback, IDGlobal, RDCUnit, Route, StrUtils, Menus,
     DateUtils, TestUnit, StationMonitors, LocoDialogue, Help, LocationsUnit, Replay, Options, Edit, WorkingTimetable, TCPIP, Logging, Main, Train, DataCheck, SignalsUnit,
     PointsUnit, LinesUnit, TrackCircuitsUnit;

CONST
  UnitRef = 'Input';

VAR
  AllHeldRouteingReleased : Boolean = False;
  IncludeLocationOccupationStateFlag : Boolean = False;
  InputDialogueBoxRequired : InputDialogueBoxType;
  InputDialogueCharValid : Boolean = False;
  InputDialogueLine : Integer = UnknownLine;
  InputDialogueLineFound : Boolean = False;
  InputDialoguePoint : Integer = UnknownPoint;
  InputDialoguePointFound : Boolean = False;
  InputDialogueShiftState : TShiftState = [];
  InputDialogueSignal : Integer = UnknownSignal;
  InputDialogueSignalFound : Boolean = False;
  InputDialogueTrackCircuit : Integer = 0;
  InputDialogueTrackCircuitFound : Boolean = False;
  LastKeyPressed : Integer;
  ListLineColours : Boolean = False;
  PreviouslyDisplayedInputDialogueBox : InputDialogueBoxType;
  SaveDebugWindowDebugRichEditColour : TColor = clBlack;
  SaveTCSpeedRestrictionColour : TColour;
  ZoomLevel : Integer = 0;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE SetAndShowInputDialogueBox(InputDialogueBox : InputDialogueBoxType);
{ Set and the display the appropriate input dialogue box }
BEGIN
  InputDialogueBoxRequired := InputDialogueBox;
  Input.InputDialogueBox.Show;
END; { SetAndShowInputDialogueBox}

PROCEDURE TInputDialogueBox.InputDialogueCancelButtonClick(Sender: TObject);
BEGIN
  InputDialogueBox.Hide;
END; { InputDialogueCancelButtonClick }

PROCEDURE TInputDialogueBox.InputDialogueMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
{ If the mouse moves into the input dialogue window, move the focus there }
BEGIN
  IF NOT KeyboardAndMouseLocked AND (InputDialogueBox <> NIL) THEN BEGIN
    IF NOT InputDialogueBox.Active THEN
      InputDialogueBox.SetFocus;
  END;
END; { InputDialogueMouseMove }

PROCEDURE TInputDialogueBox.InputDialogueShowAdjacentTrackCircuitsCheckBoxClick(Sender: TObject);
BEGIN
  { Allow ShowAdjacentTrackCircuits mode to be turned on and off in the dialogue box }
  IF InputDialogueShowAdjacentTrackCircuitsCheckBox.Checked THEN
    SetMode(ShowAdjacentTrackCircuitModeType, True)
  ELSE
    SetMode(ShowAdjacentTrackCircuitModeType, False);
END; { InputDialogueShowAdjacentTrackCircuitsCheckBoxClick }

PROCEDURE TInputDialogueBox.InputDialogueMaskEditChange(Sender: TObject);
CONST
  SetUp = True;
  StartButton = True;
  Surround = True;

BEGIN
  IF NOT InputDialogueCharValid THEN BEGIN
    MakeSound(1);
    Exit;
  END;

 CASE InputDialogueBoxRequired OF
    PointDialogueBox:
      BEGIN
        InputDialogueChangeOrSelectButton.Enabled := False;
        IF PointHighlighted <> UnknownPoint THEN BEGIN
          { we've been here before - note that the point needs to be de-highlighted }
          PointHighlighted := UnknownPoint;
          InvalidateScreen(UnitRef, 'InputDialogueMaskEditChange 1');
        END;

        IF InputDialogueMaskEdit.Text <> '' THEN BEGIN
          InputDialoguePoint := 0;
          InputDialoguePointFound := False;
          WHILE (InputDialoguePoint <= High(Points)) AND NOT InputDialoguePointFound
          DO BEGIN
            IF InputDialoguePoint <> StrToInt(InputDialogueMaskEdit.Text) THEN BEGIN
              InputDialogueChangeOrSelectButton.Enabled := False;
              Inc(InputDialoguePoint);
            END ELSE BEGIN
              { found a point }
              InputDialoguePointFound := True;
              InputDialogueChangeOrSelectButton.Enabled := True;

              { Note that the point needs to be highlighted }
              PointHighlighted := InputDialoguePoint;
              InvalidateScreen(UnitRef, 'InputDialogueMaskEditChange 2');
            END;
          END; {WHILE}
        END;
      END;

    SignalDialogueBox:
      BEGIN
        InputDialogueChangeOrSelectButton.Enabled := False;
        IF SignalHighlighted <> UnknownSignal THEN BEGIN
          { we've been here before - note that the signal needs to be de-highlighted }
          SignalHighlighted := UnknownSignal;
          InvalidateScreen(UnitRef, 'InputDialogueMaskEditChange 3');
        END;

        IF InputDialogueMaskEdit.Text <> '' THEN BEGIN
          InputDialogueSignal := 0;
          InputDialogueSignalFound := False;
          WHILE (InputDialogueSignal <= High(Signals)) AND NOT InputDialogueSignalFound DO BEGIN
            IF InputDialogueSignal <> StrToInt(InputDialogueMaskEdit.Text) THEN BEGIN
              InputDialogueChangeOrSelectButton.Enabled := False;
              Inc(InputDialogueSignal);
            END ELSE BEGIN
              { found a signal }
              InputDialogueSignalFound := True;
              InputDialogueChangeOrSelectButton.Enabled := True;

              { Note that the signal needs to be highlighted }
              SignalHighlighted := InputDialogueSignal;
              InvalidateScreen(UnitRef, 'InputDialogueMaskEditChange 4');
            END;
          END; {WHILE}
        END;

      END;

    LineDialogueBox:
      BEGIN
        InputDialogueChangeOrSelectButton.Enabled := False;
        IF LineHighlighted <> UnknownLine THEN BEGIN
          { we've been here before - note that the line needs to be de-highlighted }
          LineHighlighted := UnknownLine;
          InvalidateScreen(UnitRef, 'InputDialogueMaskEditChange 5');
        END;

        IF InputDialogueMaskEdit.Text <> '' THEN BEGIN
          InputDialogueLine := 0;
          InputDialogueLineFound := False;

          { See whether it's a line name or a line number }
          IF TryStrToInt(InputDialogueMaskEdit.Text[1], InputDialogueLine) THEN BEGIN
            { set the edit mask to only allow other numbers to follow }
            InputDialogueMaskEdit.EditMask := '9999';

            InputDialogueLine := StrToInt(Trim(InputDialogueMaskEdit.Text));
            IF (InputDialogueLine < -1) OR (InputDialogueLine > High(Lines)) THEN
              InputDialogueChangeOrSelectButton.Enabled := False
            ELSE
              { found a line number }
              InputDialogueLineFound := True;
          END ELSE BEGIN
            WHILE (InputDialogueLine <= High(Lines)) AND NOT InputDialogueLineFound DO BEGIN
              IF UpperCase(LineToStr(InputDialogueLine)) <> UpperCase(InputDialogueMaskEdit.Text) THEN BEGIN
                InputDialogueChangeOrSelectButton.Enabled := False;
                Inc(InputDialogueLine);
              END ELSE
                { found a line name }
                InputDialogueLineFound := True;
            END; {WHILE}
          END;

          IF InputDialogueLineFound THEN BEGIN
            InputDialogueChangeOrSelectButton.Enabled := True;

            { Note that the line needs to be highlighted }
            LineHighlighted := InputDialogueLine;
            InvalidateScreen(UnitRef, 'InputDialogueMaskEditChange 6');
          END;
        END;

        InputDialogueMaskEdit.Text := Trim(InputDialogueMaskEdit.Text);
        IF Length(Trim(InputDialogueMaskEdit.Text)) = 0 THEN
          { if there's no entry, clear the mask }
          InputDialogueMaskEdit.EditMask := '';
      END;

    TrackCircuitDialogueBox:
      BEGIN
        InputDialogueChangeOrSelectButton.Enabled := False;
        IF TrackCircuitHighlighted <> UnknownTrackCircuit THEN BEGIN
          { we've been here before - note that the track circuit needs to be de-highlighted }
          TrackCircuitHighlighted := UnknownTrackCircuit;
          InvalidateScreen(UnitRef, 'InputDialogueMaskEditChange 7');
        END;

        IF InputDialogueMaskEdit.Text <> '' THEN BEGIN
          InputDialogueTrackCircuit := 0;
          InputDialogueTrackCircuitFound := False;
          WHILE (InputDialogueTrackCircuit <= High(TrackCircuits)) AND NOT InputDialogueTrackCircuitFound DO BEGIN
            IF InputDialogueTrackCircuit <> StrToInt(InputDialogueMaskEdit.Text) THEN BEGIN
              InputDialogueChangeOrSelectButton.Enabled := False;
              Inc(InputDialogueTrackCircuit);
            END ELSE BEGIN
              { found a track circuit }
              InputDialogueTrackCircuitFound := True;
              InputDialogueChangeOrSelectButton.Enabled := True;

              { Note that the track circuit needs to be highlighted }
              TrackCircuitHighlighted := InputDialogueTrackCircuit;
              InvalidateScreen(UnitRef, 'InputDialogueMaskEditChange 8');
            END;
          END; {WHILE}
        END;
      END;
  END; {CASE}
END; { InputDialogueMaskEditChange }

PROCEDURE TInputDialogueBox.InputDialogueChangeOrSelectButtonClick(Sender: TObject);
CONST
  ErrorMessageRequired = True;
  RecordInLocationOccupationArray = True;
  StartButton = True;
  Surround = True;

VAR
  DebugStr : String;
  OK : Boolean;
  L : Integer;
  PointResultPending : Boolean;

BEGIN
  CASE InputDialogueBoxRequired OF
    PointDialogueBox:
      BEGIN
        IF Points[InputDialoguePoint].Point_PresentState = Straight THEN
          Points[InputDialoguePoint].Point_RequiredState := Diverging
        ELSE
          { PresentState = Diverging or PointStateUnknown }
          Points[InputDialoguePoint].Point_RequiredState := Straight;

        PullPoint(UnknownLocoChipStr, InputDialoguePoint, NoSubRoute, NoRoute, NOT ForcePoint, ByUser, ErrorMessageRequired, PointResultPending,
                  DebugStr, OK);

        { Undraw the rectangle }
        IF PointHighlighted <> UnknownPoint THEN
          WITH Points[PointHighlighted] DO
            DrawOutline(Point_MouseRect, BackgroundColour, UndrawRequired, NOT UndrawToBeAutomatic);
        PointHighlighted := UnknownPoint;
      END;
    SignalDialogueBox:
      BEGIN
        PullSignal(UnknownLocoChipStr, InputDialogueSignal, NoIndicatorLit, NoRoute, NoSubRoute, UnknownLine, UnknownTrainType, NOT ByUser, OK);
        { and need to undraw the rectangle - this won't be needed when DrawSignal uses Invalidate }
        InvalidateScreen(UnitRef, 'InputDialogueChangeOrSelectButtonClick');
      END;
    TrackCircuitDialogueBox:
      BEGIN
        IF NOT InShowAdjacentTrackCircuitMode THEN BEGIN
          WITH TrackCircuits[InputDialogueTrackCircuit] DO BEGIN
            IF TC_OccupationState = TCFeedbackOccupation THEN
              SetTrackCircuitState(InputDialogueTrackCircuit, TCUnoccupied, 'set by user')
            ELSE
              IF TC_OccupationState = TCUnoccupied THEN
                SetTrackCircuitState(InputDialogueTrackCircuit, TCFeedbackOccupation, 'set by user');

            FOR L := 0 TO High(Lines) DO BEGIN
              WITH Lines[L] DO BEGIN
//                IF Lines[L].Line_TC = InputDialogueTrackCircuit THEN
//                  DrawOutline(Line_MouseRect, clWhite, UndrawRequired, NOT UndrawToBeAutomatic);
              END; {WITH}
            END;
          END; {WITH}
        END ELSE BEGIN

        END;

        IF GetTrackCircuitState(InputDialogueTrackCircuit) = TCFeedbackOccupation THEN
          InputDialogueChangeOrSelectButton.Caption := 'Reset TC'
        ELSE
          InputDialogueChangeOrSelectButton.Caption := 'Set TC';
      END;
  END; {CASE}
END; { InputDialogueChangeOrSelectButtonClick }

PROCEDURE TInputDialogueBox.InputDialogueBoxHide(Sender: TObject);
CONST
  StartButton = True;
  SetUp = True;

BEGIN
  { Need to undraw the rectangle if any }
  CASE InputDialogueBoxRequired OF
    PointDialogueBox:
      BEGIN
        PointDialogueBoxLeft := Left;
        PointDialogueBoxTop := Top;
        IF PointHighlighted <> UnknownPoint THEN
          PointHighlighted := UnknownPoint;
        InvalidateScreen(UnitRef, 'InputDialogueBoxHide 1');
      END;
    LineDialogueBox:
      BEGIN
        LineDialogueBoxLeft := Left;
        LineDialogueBoxTop := Top;
        IF LineHighlighted <> UnknownLine THEN
          LineHighlighted := UnknownLine;
        InvalidateScreen(UnitRef, 'InputDialogueBoxHide 1');
      END;
    SignalDialogueBox:
      BEGIN
        SignalDialogueBoxLeft := Left;
        SignalDialogueBoxTop := Top;
        IF SignalHighlighted <> UnknownSignal THEN
          SignalHighlighted := UnknownSignal;
        InvalidateScreen(UnitRef, 'InputDialogueBoxHide 1');
      END;
    TrackCircuitDialogueBox:
      BEGIN
        TrackCircuitDialogueBoxLeft := Left;
        TrackCircuitDialogueBoxTop := Top;
        IF TrackCircuitHighlighted <> UnknownTrackCircuit THEN
          TrackCircuitHighlighted := UnknownTrackCircuit;
        InvalidateScreen(UnitRef, 'InputDialogueBoxHide 1');

        IF InShowAdjacentTrackCircuitMode THEN BEGIN
          SetMode(ShowAdjacentTrackCircuitModeType, False);
          WriteToStatusBarPanel(StatusBarPanel2, 'Showing adjacent-track-circuit mode = OFF');
          InvalidateScreen(UnitRef, 'InputDialogueBoxHide 4');
        END;
      END;
  END; {CASE}
  { restore the mouse cursor to its original position only if it's still within the Dialogue box when we hide the Dialogue box }
  IF PtInRect(Rect(InputDialogueBox.Left,
                   InputDialogueBox.Top,
                   InputDialogueBox.Left + Width,
                   InputDialogueBox.Top + Height),
              Mouse.CursorPos)
  THEN
    Mouse.CursorPos := SaveMouseCursorPos;
END; { InputDialogueBoxHide }

PROCEDURE TInputDialogueBox.InputDialogueBoxShow(Sender: TObject);
BEGIN
  { Set up size of box contents first } { Lots of magic numbers! ******** }
  WITH InputDialogueMaskEdit DO BEGIN
    Left := 100;
    Top := 4;
    Width := 33;
    Height := 21;
    TabOrder := 0;
    OnChange := InputDialogueMaskEditChange;
  END; {WITH}
  WITH InputDialogueChangeOrSelectButton DO BEGIN
    Left := 4;
    Top := 36;
    Width := 85;
    Height := 25;
    Caption := 'Change Button';
    Default := True;
    Enabled := False;
    TabOrder := 1;
    OnClick := InputDialogueChangeOrSelectButtonClick;
  END; {WITH}
  WITH InputDialogueCancelButton DO BEGIN
    Left := 100;
    Top := 36;
    Width := 53;
    Height := 25;
    Cancel := True;
    Caption := 'Cancel';
    TabOrder := 2;
    OnClick := InputDialogueCancelButtonClick;
  END; {WITH}

  CASE InputDialogueBoxRequired OF
    PointDialogueBox:
      BEGIN
        Left := PointDialogueBoxLeft;
        Top := PointDialogueBoxTop;
        ClientWidth := 157;
        ClientHeight := 71;
      END;
    LineDialogueBox:
      BEGIN
        Left := LineDialogueBoxLeft;
        Top := LineDialogueBoxTop;
        ClientWidth := 157;
        ClientHeight := 71;
      END;
    SignalDialogueBox:
      BEGIN
        Left := SignalDialogueBoxLeft;
        Top := SignalDialogueBoxTop;
        ClientWidth := 157;
        ClientHeight := 71;
      END;
    TrackCircuitDialogueBox:
      BEGIN
        Left := TrackCircuitDialogueBoxLeft;
        Top := TrackCircuitDialogueBoxTop;
        InputDialogueShowAdjacentTrackCircuitsCheckBox.Visible := True;
        ClientWidth := InputDialogueShowAdjacentTrackCircuitsCheckBox.Width + 20;
        ClientHeight := 99;
      END;
  END; {CASE}

  CASE InputDialogueBoxRequired OF
    PointDialogueBox:
      BEGIN
        IF PreviouslyDisplayedInputDialogueBox <> PointDialogueBox THEN BEGIN
          { only display the same number in the edit box if it's the same dialogue box }
          InputDialogueMaskEdit.Text := '';
          PreviouslyDisplayedInputDialogueBox := PointDialogueBox;
        END;
        InputDialogueBox.Caption := 'Find/Change Point';
        InputDialogueMaskEditLabel.Caption := 'Point Number:';
        InputDialogueChangeOrSelectButton.Visible := True;
        InputDialogueChangeOrSelectButton.Caption := 'Change Point';
        InputDialogueMaskEdit.MaxLength := 3;
        InputDialogueMaskEdit.Width := MulDiv(FWPRailWindow.ClientWidth, 20, 1000);
        InputDialogueMaskEdit.SelectAll;
      END;
    LineDialogueBox:
      BEGIN
        IF PreviouslyDisplayedInputDialogueBox <> LineDialogueBox THEN BEGIN
          { only display the same number in the edit box if it's the same dialogue box }
          InputDialogueMaskEdit.Text := '';
          PreviouslyDisplayedInputDialogueBox := LineDialogueBox;
        END;
        InputDialogueBox.Caption := 'Find Line Name or Number';
        InputDialogueMaskEditLabel.Caption := 'Name/Number:';
        InputDialogueChangeOrSelectButton.Visible := False;
        InputDialogueMaskEdit.MaxLength := 6;

        { This needs to be wide enough to accommodate long line names, e.g. DFYD11 }
        InputDialogueMaskEdit.Width := MulDiv(FWPRailWindow.ClientWidth, 30, 1000);
        InputDialogueMaskEdit.SelectAll;
      END;
    SignalDialogueBox:
      BEGIN
        IF PreviouslyDisplayedInputDialogueBox <> SignalDialogueBox THEN BEGIN
          { only display the same number in the edit box if it's the same dialogue box }
          InputDialogueMaskEdit.Text := '';
          PreviouslyDisplayedInputDialogueBox := SignalDialogueBox;
        END;
        InputDialogueBox.Caption := 'Find/Change Signal';
        InputDialogueMaskEditLabel.Caption := 'Signal Number:';
        InputDialogueChangeOrSelectButton.Visible := True;
        InputDialogueChangeOrSelectButton.Caption := 'Change Signal';
        InputDialogueMaskEdit.MaxLength := 3;
        InputDialogueMaskEdit.Width := MulDiv(FWPRailWindow.ClientWidth, 20, 1000);
        InputDialogueMaskEdit.SelectAll;
      END;
    TrackCircuitDialogueBox:
      BEGIN
        IF PreviouslyDisplayedInputDialogueBox <> TrackCircuitDialogueBox THEN BEGIN
          { only display the same number in the edit box if it's the same dialogue box }
          InputDialogueMaskEdit.Text := '';
          PreviouslyDisplayedInputDialogueBox := TrackCircuitDialogueBox;
        END;
        InputDialogueBox.Caption := 'Find/Set TrackCircuit';
        InputDialogueMaskEditLabel.Caption := 'TrackCircuit:';
        IF InputDialogueMaskEdit.Text <> '' THEN
          InputDialogueChangeOrSelectButton.Enabled := True;
        InputDialogueChangeOrSelectButton.Visible := True;
        InputDialogueChangeOrSelectButton.Caption := 'Set TC';
        InputDialogueMaskEdit.MaxLength := 3;
        InputDialogueMaskEdit.Width := MulDiv(FWPRailWindow.ClientWidth, 20, 1000);
        InputDialogueMaskEdit.SelectAll;
      END;
  END; {CASE};
  InputDialogueMaskEdit.SetFocus;

  { Save where the mouse cursor is }
  SaveMouseCursorPos := Mouse.CursorPos;
  { And move it to the debug window }
  Mouse.CursorPos := Point(InputDialogueBox.Left + (InputDialogueBox.Width DIV 2),
                           InputDialogueBox.Top + (InputDialogueBox.Height DIV 2));

END; { InputDialogueBoxShow }

PROCEDURE TInputDialogueBox.InputDialogueMaskEditKeyPress(Sender: TObject; VAR Key: Char);
BEGIN
  IF InputDialogueBoxRequired = LineDialogueBox THEN
    InputDialogueCharValid := True
  ELSE BEGIN
    InputDialogueCharValid := False;
    CASE Key OF
      '0'..'9', Chr(vk_Back):
        InputDialogueCharValid := True;
    ELSE {CASE}
      Key := #0; { seems to work by turning the naughty non-numeric key into a null keystroke }
    END; {CASE}
  END;
END; { InputDialogueMaskEditKeyPress }

PROCEDURE TInputDialogueBox.InputDialogueBoxKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      InputDialogueBox.Hide;
  END; {CASE}

  InputDialogueShiftState := ShiftState;
END; { InputDialogueBoxKeyDown }

PROCEDURE TInputDialogueBox.InputDialogueBoxKeyUp(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  InputDialogueShiftState := [];
END; { InputDialogueBoxKeyUp }

PROCEDURE TInputDialogueBox.InputDialogueMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X,Y: Integer);
{ May want to reset default position }
BEGIN
  IF Button = mbRight THEN BEGIN
    IF MessageDialogueWithDefault('Reset dialogue box position to default?',
                                  NOT StopTimer, mtConfirmation, [mbOK, mbAbort], ['&Reset', '&Abort'], mbAbort) = mrOK THEN BEGIN
      InputDialogueBox.Hide;
      CASE InputDialogueBoxRequired OF
        PointDialogueBox:
          BEGIN
            PointDialogueBoxLeft := DefaultPointDialogueBoxLeft;
            PointDialogueBoxTop := DefaultPointDialogueBoxTop;
          END;
        LineDialogueBox:
          BEGIN
            LineDialogueBoxLeft := DefaultLineDialogueBoxLeft;
            LineDialogueBoxTop := DefaultLineDialogueBoxTop;
          END;
        SignalDialogueBox:
          BEGIN
            SignalDialogueBoxLeft := DefaultSignalDialogueBoxLeft;
            SignalDialogueBoxTop := DefaultSignalDialogueBoxTop;
          END;
        TrackCircuitDialogueBox:
          BEGIN
            TrackCircuitDialogueBoxLeft := DefaultTrackCircuitDialogueBoxLeft;
            TrackCircuitDialogueBoxTop := DefaultTrackCircuitDialogueBoxTop;
          END;
      END; {CASE}
      InputDialogueBox.Show;
    END;
  END;
END; { InputDialogueMouseDown }

FUNCTION MissingTrainFoundByUser(KeyToTest : Integer) : Boolean;
{ Returns true if a numeric keypress matches a missing train }
VAR
  I : Integer;
  FoundKey, FoundTrain : Boolean;
  T : TrainIndex;

BEGIN
  Result := False;
  I := 1;
  FoundKey := False;
  WHILE (I <= 9) AND NOT FoundKey DO BEGIN
    IF (MissingTrainArray[I] = True) AND (KeyToTest = I) THEN BEGIN
      { we've found one }
      FoundKey := True;
      T := 0;
      FoundTrain := False;
      WHILE (T <= High(Trains)) AND NOT FoundTrain DO BEGIN
        WITH Trains[T] DO BEGIN
          IF Train_DiagramFound THEN BEGIN
            IF (Train_CurrentStatus = Missing) OR (Train_CurrentStatus = MissingAndSuspended) THEN BEGIN
              IF Train_MissingNum = KeyToTest THEN BEGIN
                { Undo the disappearance }
                FoundTrain := True;
                Result := True;
                MissingTrainArray[KeyToTest] := False;
                ReturnTrainFromMissing(T);
                Log(Train_LocoChipStr + ' L reappeared - user has restarted it by pressing ''' + IntToStr(KeyToTest) + '''');
              END;
            END;
          END;
        END; {WITH}
        Inc(T);
      END; {WHILE}
    END;
    Inc(I);
  END; {WHILE}
END; { MissingTrainFoundByUser }

PROCEDURE SetUpStationMonitors(ShiftState : TShiftState; DisplayOrderNum : Integer);
{ Chooses which station we're going to or from and how to show the results }
VAR
  Area : Integer;
  FoundArea : Boolean;

BEGIN
  IF ShiftState = [] THEN
    StationMonitorDisplay := StationArrivalsAndDeparturesDisplay
  ELSE
    IF ShiftState = [ssShift] THEN
      StationMonitorDisplay := StationDeparturesDisplay
    ELSE
      IF ShiftState = [ssCtrl] THEN
        StationMonitorDisplay := StationArrivalsDisplay
      ELSE
        IF ShiftState = [ssAlt] THEN
          StationMonitorDisplay := StationClockDisplay;

  Area := 0;
  FoundArea := False;
  WHILE (Area <= High(Areas)) AND NOT FoundArea DO BEGIN
    IF Areas[Area].Area_StationMonitorsDisplayOrderNum = DisplayOrderNum THEN BEGIN
      StationMonitorsCurrentArea := Area;
      FoundArea := True;
    END;
    Inc(Area)
  END; {WHILE}

  StationMonitorsAlreadySetUp := True;
  DrawStationMonitorsWindow(StationMonitorsCurrentArea);
END; { SetUpStationMonitors }

FUNCTION DescribeKey(Key : Word; ShiftState : TShiftState) : String;
{ Return a description of the key that's been pressed }
BEGIN
  Result := '';

  CASE Key OF
    Ord('1'):
      IF ssShift IN ShiftState THEN
        Result := '!!' { as ! in Debug means turn bold on! }
      ELSE
        Result := '1';
    Ord('2'):
      IF ssShift IN ShiftState THEN
        Result := '"'
      ELSE
        Result := '2';
    Ord('3'):
      IF ssShift IN ShiftState THEN
        Result := '£'
      ELSE
        Result := '3';
    Ord('4'):
      IF ssShift IN ShiftState THEN
        Result := '$'
      ELSE
        Result := '4';
    Ord('5'):
      IF ssShift IN ShiftState THEN
        Result := '%'
      ELSE
        Result := '5';
    Ord('6'):
      IF ssShift IN ShiftState THEN
        Result := '^'
      ELSE
        Result := '6';
    Ord('7'):
      IF ssShift IN ShiftState THEN
        Result := '&'
      ELSE
        Result := '7';
    Ord('8'):
      IF ssShift IN ShiftState THEN
        Result := '*'
      ELSE
        Result := '8';
    Ord('9'):
      IF ssShift IN ShiftState THEN
        Result := '('
      ELSE
        Result := '9';
    Ord('0'):
      IF ssShift IN ShiftState THEN
        Result := ')'
      ELSE
        Result := '0';
    Ord(186):
      IF ssShift IN ShiftState THEN
        Result := ':'
      ELSE
        Result := ';';
    Ord(192):
      IF ssShift IN ShiftState THEN
        Result := '@'
      ELSE
        Result := '''';
    Ord(222):
      IF ssShift IN ShiftState THEN
        Result := '~'
      ELSE
        Result := '#';
    Ord(219):
      IF ssShift IN ShiftState THEN
        Result := '{'
      ELSE
        Result := '[';
    Ord(221):
      IF ssShift IN ShiftState THEN
        Result := '}'
      ELSE
        Result := ']';
    Ord(220):
      IF ssShift IN ShiftState THEN
        Result := '|'
      ELSE
        Result := '\';
    Ord(96):
      Result := 'numeric 0';
    Ord(97):
      Result := 'numeric 1';
    Ord(98):
      Result := 'numeric 2';
    Ord(99):
      Result := 'numeric 3';
    Ord(100):
      Result := 'numeric 4';
    Ord(101):
      Result := 'numeric 5';
    Ord(102):
      Result := 'numeric 6';
    Ord(103):
      Result := 'numeric 7';
    Ord(104):
      Result := 'numeric 8';
    Ord(105):
      Result := 'numeric 9';
    Ord(110):
      Result := 'numeric .';
    Ord(111):
      Result := 'numeric /';
    vk_Add:
      Result := 'Numeric +';
    vk_Subtract:
      Result := 'Numeric -';
    vk_Multiply:
      Result := 'Numeric *';
    Ord(223):
      IF ssShift IN ShiftState THEN
        Result := '¬'
      ELSE
        Result := '`';
    Ord(191):
      IF ssShift IN ShiftState THEN
        Result := '?'
      ELSE
        Result := '/';
    Ord(187):
      IF ssShift IN ShiftState THEN
        Result := '='
      ELSE
        Result := '+';
    Ord(189):
      IF ssShift IN ShiftState THEN
        Result := '_'
      ELSE
        Result := '-';
    Ord(188):
      IF ssShift IN ShiftState THEN
        Result := '<'
      ELSE
        Result := ',';
    Ord(190):
      IF ssShift IN ShiftState THEN
        Result := '>'
      ELSE
        Result := '.';
    Ord(','):
      Result := ',';
  END; {CASE}

  { Function and other shifted keys separated off to add the nomenclature "Shift", "Alt", etc. }
  IF Result = '' THEN BEGIN
    CASE Key OF
      Ord('A'):
        Result := 'A';
      Ord('B'):
        Result := 'B';
      Ord('C'):
        Result := 'C';
      Ord('D'):
        Result := 'D';
      Ord('E'):
        Result := 'E';
      Ord('F'):
        Result := 'F';
      Ord('G'):
        Result := 'G';
      Ord('H'):
        Result := 'H';
      Ord('I'):
        Result := 'I';
      Ord('J'):
        Result := 'J';
      Ord('K'):
        Result := 'K';
      Ord('L'):
        Result := 'L';
      Ord('M'):
        Result := 'M';
      Ord('N'):
        Result := 'N';
      Ord('O'):
        Result := 'O';
      Ord('P'):
        Result := 'P';
      Ord('Q'):
        Result := 'Q';
      Ord('R'):
        Result := 'R';
      Ord('S'):
        Result := 'S';
      Ord('T'):
        Result := 'T';
      Ord('U'):
        Result := 'U';
      Ord('V'):
        Result := 'V';
      Ord('W'):
        Result := 'W';
      Ord('X'):
        Result := 'X';
      Ord('Y'):
        Result := 'Y';
      Ord('Z'):
        Result := 'Z';
      vk_F1:
        Result := 'F1';
      vk_F2:
        Result := 'F2';
      vk_F3:
        Result := 'F3';
      vk_F4:
        Result := 'F4';
      vk_F5:
        Result := 'F5';
      vk_F6:
        Result := 'F6';
      vk_F7:
        Result := 'F7';
      vk_F8:
        Result := 'F8';
      vk_F9:
        Result := 'F9';
      vk_F10:
        Result := 'F10';
      vk_F11:
        Result := 'F11';
      vk_F12:
        Result := 'F12';
      vk_Space:
        Result := 'Spacebar';
      vk_Tab:
        Result := 'Tab';
      vk_Return:
        Result := 'Enter';
      vk_Insert:
        Result := 'Insert';
      vk_Delete:
        Result := 'Delete';
      vk_Back:
        Result := 'Backspace';
      vk_Escape:
        Result := 'Escape';
      vk_Left:
        Result := 'Left Arrow';
      vk_Right:
        Result := 'Right Arrow';
      vk_Up:
        Result := 'Up Arrow';
      vk_Down:
        Result := 'Down Arrow';
      Ord(36):
        Result := 'Home';
      Ord(35):
        Result := 'End';
      Ord(33):
        Result := 'PgUp';
      Ord(34):
        Result := 'PgDn';
    END; {CASE}

    IF (ssShift IN ShiftState) AND (ssCtrl IN ShiftState) AND (ssAlt IN ShiftState) THEN
      Result := 'Ctrl Shift Alt ' + Result
    ELSE
      IF (ssShift IN ShiftState) AND (ssAlt IN ShiftState) THEN
        Result := 'Shift Alt ' + Result
      ELSE
        IF (ssCtrl IN ShiftState) AND (ssAlt IN ShiftState) THEN
          Result := 'Ctrl Alt ' + Result
        ELSE
          IF (ssShift IN ShiftState) AND (ssCtrl IN ShiftState) THEN
            Result := 'Ctrl Shift ' + Result
          ELSE
            IF (ssShift IN ShiftState) THEN
              Result := 'Shift ' + Result
            ELSE
              IF ssCtrl IN ShiftState THEN
                Result := 'Ctrl ' + Result
              ELSE
                IF ssAlt IN ShiftState THEN
                  Result := 'Alt ' + Result;
  END;
  IF Result = '' THEN
    Result := 'Keycode ' + IntToStr(Ord(Key)) + ' has no key description';
END; { DescribeKey }

PROCEDURE KeyPressedDownMainProcedure(KeyToTest : Word; InputShiftState : TShiftState; HelpRequired : Boolean; OUT HelpMsg : String);
{ Called when a key is pressed or help on keys is required }
CONST
  ErrorMessageRequired = True;
  ExitProgram = True;
  ForceDisplay = True;
  FullRecord = True;
  GoingForward = True;
  Highlight = True;
  IncludeFirstOccupation = True;
  Indent = True;
  Init = True;
  IrrelevantNum : Integer = 0;
  MouseHasMoved = True;
  NewSignalData = True;
  NoNextSubRouteCh = ' ';
  ProcessMessages = True;
  Rebuild = True;
  Start = True;
  StopTimer = True;
  Test = True;
  TimetableLoading = True;
  TrackCircuitOccupation = True;
  Undo = True;
  UpLine = True;
  WarnUser = True;
  WriteToFile = True;

TYPE
  ShiftKeysType = (NoShiftKeys, Alt, Shift, Ctrl, ShiftAlt, CtrlShift, CtrlAlt, CtrlAltShift);

VAR
  CloseAction : TCloseAction;
  DebugStr : String;
  ErrorMsg : String;
  I : Integer;
//  IrrelevantShiftState : TShiftState;
  L : Integer;
  OK : Boolean;
  P : Integer;
  R : Integer;
  PointFeedbackWaitInSeconds : Int64;
  PointResultPending : Boolean;
//  RichLineAdded : Boolean;
  S : Integer;
//  SaveHelpMsg : String;
  ShiftKeys : ShiftKeysType;
  StatusBarPanelText : String;
  Str : String;
  T : TrainIndex;
  TC : Integer;
//  TempKey : Word;
  XTypeOfLine : TypeOfLine;
  ZoomAmountStr : String;

  Hour, Min, Sec, MSec : Word;

  PROCEDURE ProcessLocationOccupations(RebuildFlag, WriteToFileFlag : Boolean);
  { Call the routine to do the writing }
  BEGIN
    Log('X Writing occupation window'
           + IfThen(Rebuild,
                    ', rebuilding occupations first')
           + IfThen(WriteToFileFlag,
                    ' and write them to file'));


    IF Rebuild THEN BEGIN
      DrawLineInLogFile(UnknownLocoChipAsZeroesStr, '*', '+', UnitRef);
      SetUpAllLocationOccupationsAbInitio(NOT TimetableLoading, OK);
      DrawLineInLogFile(UnknownLocoChipAsZeroesStr, '*', '+', UnitRef);
    END;

    IF LocationsUnitWindow.Visible THEN BEGIN
      IF IncludeLocationOccupationStateFlag THEN BEGIN
        IncludeLocationOccupationStateFlag := False;
        WriteLocationOccupations(IncludeLocationOccupationStateFlag, WriteToFileFlag);
      END ELSE BEGIN
        IncludeLocationOccupationStateFlag := True;
        WriteLocationOccupations(IncludeLocationOccupationStateFlag, WriteToFileFlag);
      END;
    END ELSE BEGIN
      IncludeLocationOccupationStateFlag := False;
      WriteLocationOccupations(IncludeLocationOccupationStateFlag, WriteToFileFlag);
      LocationsUnitWindow.Show;
    END;
  END; { ProcessLocationOccupations }

  { Main Loop }
BEGIN { KeyPressedDown }
  HelpMsg := '';
  TRY
    IF ProgramShuttingDown THEN BEGIN
      Log('XG Cannot accept key press as program shutting down');
      Exit;
    END;

    LastKeyPressed := KeyToTest;

    BEGIN
      ShiftKeys := NoShiftKeys;
      IF (ssShift IN InputShiftState) AND (ssCtrl IN InputShiftState) AND (ssAlt IN InputShiftState) THEN
        ShiftKeys := CtrlAltShift
      ELSE
        IF (ssShift IN InputShiftState) AND (ssAlt IN InputShiftState) THEN
          ShiftKeys := ShiftAlt
        ELSE
          IF (ssCtrl IN InputShiftState) AND (ssAlt IN InputShiftState) THEN
            ShiftKeys := CtrlAlt
          ELSE
            IF (ssShift IN InputShiftState) AND (ssCtrl IN InputShiftState) THEN
              ShiftKeys := CtrlShift
            ELSE
              IF (ssShift IN InputShiftState) THEN
                ShiftKeys := Shift
              ELSE
                IF ssCtrl IN InputShiftState THEN
                  ShiftKeys := Ctrl
                ELSE
                  IF ssAlt IN InputShiftState THEN
                    ShiftKeys := Alt;
    END;

    { See if the keyboard is locked }
    IF KeyBoardandMouseLocked THEN
      IF KeyToTest <> Ord('K') THEN BEGIN
        Debug('Keyboard locked - press Shift + ''K'' to unlock ');
        Exit;
      END;

    { See if we wish to close the program }
    IF EscKeyStored AND (KeyToTest <> vk_Escape) THEN BEGIN
      EscKeyStored := False;
      Debug('Second key press was not the Escape key - close program cancelled');
    END;

    { any key resets replay scrolling, but is otherwise ignored }
    IF ReplayScrollUp THEN BEGIN
      ReplayScrollUp := False;
      KeyToTest := 0;
    END;
    IF ReplayScrollDown THEN BEGIN
      ReplayScrollDown := False;
      KeyToTest := 0;
    END;

    IF ReplayMode THEN
      ReplayKeyPressed(KeyToTest, InputShiftState)
    ELSE BEGIN
      { Not Replaying: }

      { Check these keypresses whether replaying or not }
      CASE KeyToTest OF
        vk_F1, vk_F2, vk_F3, vk_F4, vk_F5, vk_F6, vk_F7, vk_F8, vk_F9, vk_F10, vk_F11, vk_F12, vk_Escape:
          BEGIN
            IF ShowTrackCircuits THEN BEGIN
              { need to recolour all the lines with their original colours }
              FOR L := 0 TO High(Lines) DO
                DrawLine(L, Lines[L].Line_OldColour, NOT ActiveTrain);
            END;

            IF DisplayLineColoursWindow.Visible THEN
              DisplayLineColoursWindow.Visible := False;

            IF ShowSignalsWhereRouteingCanBeHeldAndThoseNotUsedForRouteing THEN
              FOR S := 0 TO High(Signals) DO
                Signals[S].Signal_PostColour := SignalPostColour;

            IF HighlightTrackCircuitSpeedRestrictions THEN
              TCSpeedRestrictionColour := DefaultTCSpeedRestrictionColour;

            { If any of the following states are true, Escape exits from the state, and does not (necessarily) mean we wish to exit from the program }
            IF HighlightTrackCircuitSpeedRestrictions
            OR ShowAreas
            OR ShowLenzPointNumbers
            OR ShowLenzPointUnitGroups
            OR ShowLineDetail
            OR ShowLineDirectionDetail
            OR ShowLineGradients
            OR ShowLineNumbers
            OR ShowLinesWithoutTrackCircuits
            OR ShowLinesUpXAbsoluteValue
            OR ShowLinesWhichLockPoints
            OR ShowLocationLengthDetail
            OR ShowLocations
            OR ShowMouseRectangles
            OR ShowOneFeedbackUnitOnly
            OR ShowPointDefaultState
            OR ShowPointDetail
            OR ShowPointFeedbackDataInSeparateColours
            OR ShowPointFeedbackDataInUse
            OR ShowPointsStraightAndDiverging
            OR ShowPointsThatAreLocked
            OR ShowPointType
            OR ShowSignalAndBufferStopNums
            OR ShowSignalsAndBufferStopsWithTheatreDestinations
            OR ShowSignalJunctionDestinations
            OR ShowSignalsWhereRouteingCanBeHeldAndThoseNotUsedForRouteing
            OR ShowSignalsWithAdjacentTrackCircuits
            OR ShowTrackCircuits
            OR ShowTrackCircuitFeedbackDataInSeparateColours
            OR ShowTrackCircuitFeedbackDataInUse
            OR ShowTrackCircuitLengths
            OR ShowTrackCircuitsRoutedOver
            THEN BEGIN
              { Now reset all the states }
              ShowAreas := False;
              ShowLenzPointNumbers := False;
              ShowLineDirectionDetail := False;
              ShowLineGradients := False;
              ShowLineNumbers := False;
              ShowLinesWithoutTrackCircuits := False;
              ShowLinesUpXAbsoluteValue := False;
              ShowLinesWhichLockPoints := False;
              ShowLocationLengthDetail := False;
              ShowLocations := False;
              ShowMouseRectangles := False;
              ShowOneFeedbackUnitOnly := False;
              ShowPointDefaultState := False;
              ShowPointDetail := False;
              ShowPointFeedbackDataInSeparateColours := False;
              ShowPointFeedbackDataInUse := False;
              ShowPointsStraightAndDiverging := False;
              ShowPointsThatAreLocked := False;
              ShowLenzPointUnitGroups := False;
              ShowPointType := False;
              ShowSignalAndBufferStopNums := False;
              ShowSignalsAndBufferStopsWithTheatreDestinations := False;
              ShowSignalJunctionDestinations := False;
              ShowSignalsWhereRouteingCanBeHeldAndThoseNotUsedForRouteing := False;
              ShowSignalsWithAdjacentTrackCircuits := False;
              ShowTrackCircuits := False;
              ShowTrackCircuitFeedbackDataInSeparateColours := False;
              ShowTrackCircuitFeedbackDataInUse := False;
              ShowTrackCircuitLengths := False;
              ShowTrackCircuitsRoutedOver := False;
              HighlightTrackCircuitSpeedRestrictions := False;

              { This is a special case, as pressing F4 a second time adds the box which lists the colours }
              IF KeyToTest <> vk_F4 THEN
                ShowLineDetail := False;

              WriteToStatusBarPanel(StatusBarPanel2, '');
              InvalidateScreen(UnitRef, 'KeyPressedDown 1');

              IF KeyToTest = vk_Escape THEN
                Exit;
            END;
          END;
      END; {CASE}

      CASE KeyToTest OF
        vk_Add: { plus key }
          CASE ShiftKeys OF
            CtrlAltShift: { ctrl alt shift and the '+' sign on the numeric keypad }
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            NoShiftKeys, Ctrl, Alt, Shift: { the '+' sign on the numeric keypad }
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Subtract: { minus key }
          BEGIN
            HelpMsg := '';
            IF NOT HelpRequired THEN BEGIN
            END;
          END;
        Ord('1'):
          CASE ShiftKeys OF
            NoShiftKeys: {1}
              BEGIN
                HelpMsg := 'clear missing train 1';
                IF NOT HelpRequired THEN
                  MissingTrainFoundByUser(1);
              END;
            Ctrl: {1}
              BEGIN
                HelpMsg := 'test sound 1';
                IF NOT HelpRequired THEN BEGIN
                  Log('XG Testing sound 1');
                  MakeSound(1);
                END;
              END;
          END; {CASE}
        Ord('2'):
          CASE ShiftKeys OF
            NoShiftKeys: {2}
              BEGIN
                HelpMsg := 'clear missing train 2';
                IF NOT HelpRequired THEN
                  MissingTrainFoundByUser(2);
              END;
            Ctrl: {2}
              BEGIN
                HelpMsg := 'test sound 2';
                IF NOT HelpRequired THEN BEGIN
                  Log('XG Testing sound 2');
                  MakeSound(2);
                END;
              END;
          END; {CASE}
        Ord('3'):
          CASE ShiftKeys OF
            NoShiftKeys: {3}
              BEGIN
                HelpMsg := 'clear missing train 3';
                IF NOT HelpRequired THEN
                  MissingTrainFoundByUser(3);
              END;
            Ctrl: {3}
              BEGIN
                HelpMsg := 'test sound 3';
                IF NOT HelpRequired THEN BEGIN
                  Log('XG Testing sound 3');
                  MakeSound(3);
                END;
              END;
          END; {CASE}
        Ord('4'):
          CASE ShiftKeys OF
            NoShiftKeys: {4}
              BEGIN
                HelpMsg := 'clear missing train 4';
                IF NOT HelpRequired THEN
                  MissingTrainFoundByUser(4);
              END;
            Ctrl: {4}
              BEGIN
                HelpMsg := 'test sound 4';
                IF NOT HelpRequired THEN BEGIN
                  Log('XG Testing sound 4');
                  MakeSound(4);
                END;
              END;
          END; {CASE}
        Ord('5'):
          CASE ShiftKeys OF
            NoShiftKeys: {5}
              BEGIN
                HelpMsg := 'clear missing train 5';
                IF NOT HelpRequired THEN
                  MissingTrainFoundByUser(5);
              END;
            Ctrl: {5}
              BEGIN
                HelpMsg := 'test sound 5';
                IF NOT HelpRequired THEN BEGIN
                  Log('XG Testing sound 5');
                  MakeSound(5);
                END;
              END;
          END; {CASE}
        Ord('6'):
          CASE ShiftKeys OF
            NoShiftKeys: {6}
              BEGIN
                HelpMsg := 'clear missing train 6';
                MissingTrainFoundByUser(6);
              END;
            Ctrl: {6}
              BEGIN
                HelpMsg := 'test sound 6';
                IF NOT HelpRequired THEN BEGIN
                  Log('XG Testing sound 6');
                  MakeSound(6);
                END;
              END;
          END; {CASE}
        Ord('7'):
          CASE ShiftKeys OF
            NoShiftKeys: {7}
              BEGIN
                HelpMsg := 'clear missing train 7';
                MissingTrainFoundByUser(7);
              END;
            Ctrl: {7}
              BEGIN
                HelpMsg := 'test sound 7';
                IF NOT HelpRequired THEN BEGIN
                  Log('XG Testing sound 7');
                  MakeSound(7);
                END;
              END;
          END; {CASE}
        Ord('8'): { '*' key if shifted}
          CASE ShiftKeys OF
            NoShiftKeys:
              BEGIN
                HelpMsg := 'clear missing train 8';
                IF NOT HelpRequired THEN
                  MissingTrainFoundByUser(8);
              END;
            Shift:
              BEGIN
                { can press either '*' or the star key on the numeric keypad to add line of asterisks to the log }
                HelpMsg := 'insert line of asterisks into log';
                IF NOT HelpRequired THEN BEGIN
                  WriteToLogFileAndTestFile := True;
                  DrawLineInLogFile(UnknownLocoChipAsZeroesStr, 'X', '*', UnitRef);
                  WriteToLogFileAndTestFile := False;
                  Debug('!****');
                END;
              END;
            Ctrl: {8}
              BEGIN
                HelpMsg := 'test sound 8';
                IF NOT HelpRequired THEN BEGIN
                  Log('XG Testing sound 8');
                  MakeSound(8);
                END;
              END;
          END; {CASE}
        Ord('9'):
          CASE ShiftKeys OF
            NoShiftKeys: {9}
              BEGIN
                HelpMsg := 'clear missing train 9';
                IF NOT HelpRequired THEN
                  MissingTrainFoundByUser(9);
              END;
            Ctrl: {9}
              BEGIN
                HelpMsg := 'test sound 9';
                IF NOT HelpRequired THEN BEGIN
                  Log('XG Testing sound 9');
                  MakeSound(9);
                END;
              END;
          END; {CASE}
        Ord('A'):
          CASE ShiftKeys OF
            NoShiftKeys: {A}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {A}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {A}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {A}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {A}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {A}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {A}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {A}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('B'):
          CASE ShiftKeys OF
            NoShiftKeys: {B}
              BEGIN
                HelpMsg := 'toggle breakpoints in MakeSound routine';
                IF NOT HelpRequired THEN BEGIN
                  IF BreakPointRequiredInMakeSoundRoutine THEN BEGIN
                    BreakPointRequiredInMakeSoundRoutine := False;
                    Log('XG BreakPointRequiredInMakeSoundRoutine = OFF');
                  END ELSE BEGIN
                    BreakPointRequiredInMakeSoundRoutine := True;
                    Log('XG BreakPointRequiredInMakeSoundRoutine = ON');
                  END;
                END;
              END;
            CtrlAltShift: {B}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {B}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {B}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {B}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {B}
              BEGIN
                HelpMsg := 'Save bitmap to file';
                IF NOT HelpRequired THEN BEGIN
                  RailWindowBitmap.SaveToFile('C:\Doc\RAD Studio\Projects\Rail\Bitmap.bmp');
                  Debug('Bitmap saved at C:\Doc\RAD Studio\Projects\Rail\RailTemp.Bitmap.bmp');
                END;
              END;
            Ctrl: {B}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {B}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('C'):
          CASE ShiftKeys OF
            NoShiftKeys: {C}
              BEGIN
                HelpMsg := 'set clock';
                IF NOT HelpRequired THEN BEGIN
                  IF GetTime.ClockWindow.Visible THEN
                    GetTime.ClockWindow.Visible := False
                  ELSE BEGIN
                    GetTime.ClockWindow.Visible := True;
                    GetTime.ClockWindow.Clock.Time := CurrentRailwayTime;
                    GetTime.ClockWindow.Caption := SetCurrentRailwayTimeCaption;
                  END;
                END;
              END;
            CtrlAltShift: {C}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {C}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {C}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {C}
              BEGIN
                HelpMsg := 'clear a specific route whether it is in the route array or not';
                IF NOT HelpRequired THEN BEGIN
                  IF Length(Routes_Routes) = 0 THEN
                    Debug('!No routes to clear')
                  ELSE BEGIN
                    Str := '';
                    IF InputQuery('Route Clearing', 'Enter the number of the route to be cleared', Str) THEN BEGIN
                      IF NOT TryStrToInt(Str, R) THEN
                        ShowMessage('"' + Str + '" is not a valid integer')
                      ELSE
                        { clear the route by brute force }
                        ClearARouteByBruteForce(R);
                    END;
                  END;
                END;
              END;
            Shift: {C}
              BEGIN
                HelpMsg := 'select create line mode';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT CreateLineMode THEN BEGIN
                    TurnEditModeOn(UnknownSignal, UnknownPoint, UnknownBufferStop, UnknownLine, UnknownTrackCircuit);
                    TurnCreateLineModeOn;
                  END ELSE BEGIN
                    TurnEditModeOff;
                    TurnCreateLineModeOff;
                  END;
                  InvalidateScreen(UnitRef, 'select create line mode');
                END;
              END;
            Ctrl: {C}
              BEGIN
                HelpMsg := 'clear a specific route by brute force';
                IF NOT HelpRequired THEN BEGIN
                  Str := '';
                  IF InputQuery('Route Clearing', 'Enter the number of the route to be cleared', Str) THEN BEGIN
                    IF NOT TryStrToInt(Str, R) THEN
                      ShowMessage('"' + Str + '" is not a valid number')
                    ELSE BEGIN
                      IF NOT IsElementInIntegerArray(Routes_Routes, R) THEN
                        ShowMessage('"' + Str + '" is not a valid route number')
                      ELSE
                        { clear the route by brute force }
                        ClearARouteByBruteForce(R);
                    END;
                  END;
                END;
              END;
            Alt: {C}
              BEGIN
                HelpMsg := 'clear all routes by brute force';
                IF NOT HelpRequired THEN BEGIN
                  IF MessageDialogueWithDefault('Clear all routes?', NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes THEN BEGIN
                    DebugStr := 'Clearing all routes';
                    FOR I := 0 TO 999 DO { an arbitrary nunber! ***** }
                      ClearARouteByBruteForce(I);
                  END;
                END;
              END;
          END; {CASE}
        Ord('D'):
          CASE ShiftKeys OF
            NoShiftKeys: {D}
              BEGIN
                HelpMsg := 'show debug options';
                IF NOT HelpRequired THEN
                  DisplayGeneralDebuggingTabSheet;
              END;
            CtrlAltShift: {D}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {D}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {D}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {D}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {D}
              BEGIN
                HelpMsg := 'show diagrams window';
                IF NOT HelpRequired THEN BEGIN
                  IF DisplayDiagrams THEN BEGIN
                    DiagramsWindow.Hide;
                    DisplayDiagrams := False;
                  END ELSE BEGIN
                    DiagramsWindow.Show;
                    DisplayDiagrams := True;
                  END;
                  DrawDiagrams(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {D}
              BEGIN
                HelpMsg := 'select large or small diagrams window';
                IF NOT HelpRequired THEN BEGIN
                  IF LargeDiagramsWindowSelected THEN
                    LargeDiagramsWindowSelected := False
                  ELSE
                    LargeDiagramsWindowSelected := True;
                  DrawDiagramsWindow;
                  DrawDiagrams(UnitRef, 'KeyPressedDown 1');
                END;
              END;
            Alt: {D}
              BEGIN
                HelpMsg := 'automatically set focus when in Debug Window on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF AutomaticallySetFocusWhenInDebugWindow THEN BEGIN
                    AutomaticallySetFocusWhenInDebugWindow := False;
                    Log('AG Automatically set focus when in Debug Window = OFF');
                  END ELSE BEGIN
                    AutomaticallySetFocusWhenInDebugWindow := True;
                    Log('AG Automatically set focus when in Debug Window = ON');
                  END;
                END;
              END;
            END; {CASE}
        Ord('E'):
          CASE ShiftKeys OF
            NoShiftKeys: {E}
              BEGIN
                HelpMsg := 'open loco dialogue box';
                IF NOT HelpRequired THEN
                  LocoDialogue.LocoDialogueWindow.Show;
              END;
            CtrlAltShift: {E}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {E}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {E}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {E}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {E}
              BEGIN
                HelpMsg := 'toggle edit mode';
                IF NOT HelpRequired THEN BEGIN
                  IF EditMode THEN BEGIN
                    IF MessageDialogueWithDefault('Turn editing off - are you sure?',
                                                  StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
                    THEN
                      Debug('Edit Mode remains ON')
                    ELSE
                      TurnEditModeOff;
                  END ELSE BEGIN
                    IF MessageDialogueWithDefault('Turn editing on - are you sure?',
                                                  StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
                    THEN
                      Debug('Edit Mode remains OFF')
                    ELSE
                      TurnEditModeOn(UnknownSignal, UnknownPoint, UnknownBufferStop, UnknownLine, UnknownTrackCircuit);
                  END;
                END;
              END;
            Ctrl: {E}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {E}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('F'):
          CASE ShiftKeys OF
            NoShiftKeys: {F}
              BEGIN
                HelpMsg := 'show feedback options';
                IF NOT HelpRequired THEN
                  DisplayFeedbackDebuggingTabSheet;
              END;
            CtrlAltShift: {F}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {F}
               BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {F}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {F}
               BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
           Alt: {F}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {F}
              ; { do not use as causes confusion with Ctrl-F used for searches }
           END; {CASE}
        Ord('G'):
          CASE ShiftKeys OF
            NoShiftKeys: {G}
              BEGIN
                HelpMsg := 'Open/close the logging window';
                IF NOT HelpRequired THEN BEGIN
                  LoggingWindow.Visible := NOT LoggingWindow.Visible;
                END;
              END;
            CtrlAltShift: {G}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {G}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {G}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {G}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {G}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {G}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {G}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('H'):
          CASE ShiftKeys OF
            NoShiftKeys: {H}
              BEGIN
                HelpMsg := 'toggle line headcodes, journeys and routes, and chip numbers';
                IF NOT HelpRequired THEN BEGIN
                  IF DisplayLocoChipNums THEN BEGIN
                    StatusBarPanelText := 'Showing headcodes';
                    DisplayLocoHeadcodes := True;
                    DisplayLocoChipNums := False;
                    DisplayRoutesAndJourneys := False;
                  END ELSE
                    IF DisplayLocoHeadcodes THEN BEGIN
                      StatusBarPanelText := 'Showing journeys and routes';
                      DisplayRoutesAndJourneys := True;
                      DisplayLocoChipNums := False;
                      DisplayLocoHeadcodes := False;
                    END ELSE
                      IF DisplayRoutesAndJourneys THEN BEGIN
                        StatusBarPanelText := 'Showing chip numbers';
                        DisplayLocoChipNums := True;
                        DisplayLocoHeadcodes := False;
                        DisplayRoutesAndJourneys := False;
                      END;
                  WriteToStatusBarPanel(StatusBarPanel2, StatusBarPanelText);
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {H}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {H}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {H}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {H}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {H}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {H}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {H}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            END; {CASE}
        Ord('I'):
          CASE ShiftKeys OF
            NoShiftKeys: {I}
              BEGIN
                HelpMsg := 'invalidate (redraw) screen';
                IF NOT HelpRequired THEN BEGIN
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                  Log('AG Screen invalidated by user');
                END;
              END;
            CtrlAltShift: {I}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {I}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {I}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {I}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {I}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {I}
              BEGIN
                HelpMsg := 'invalidate (redraw) screen after re-acquiring the feedback data';
                IF NOT HelpRequired THEN BEGIN
                  IF MessageDialogueWithDefault('Redraw screen after re-acquiring the feedback data - are you sure?',
                                                NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
                  THEN BEGIN
                    GetInitialFeedback(OK);
                    InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                    Log('AG User requested screen redraw after initial feedback sought ' + IfThen(OK,
                                                                                                  ' and obtained',
                                                                                                  ' but not provided'));
                  END;
                END;
              END;
            Alt: {I}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('J'):
          CASE ShiftKeys OF
            NoShiftKeys: {J}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {J}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {J}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {J}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {J}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {J}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {J}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {J}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('K'):
          CASE ShiftKeys OF
            CtrlAltShift: {K}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {K}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {K}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {K}
              BEGIN
                HelpMsg := 'initiate general check';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT InLogsCurrentlyKeptMode THEN
                    Debug('Cannot initiate general check with LogsCurrentlyKept turned off')
                  ELSE BEGIN
                    WriteToStatusBarPanel(StatusBarPanel2, 'General check initiated');
                    DoGeneralCheck;
                  END;
                END;
              END;
            Shift: {K}
              BEGIN
                HelpMsg := 'lock keyboard';
                IF NOT HelpRequired THEN BEGIN
                  IF KeyboardAndMouseLocked THEN BEGIN
                    Debug('*** Keyboard & mouse unlocked ***');
                    ChangeCursor(crDefault);
                    KeyBoardandMouseLocked := False;
                  END ELSE BEGIN
                    IF InAutoMode THEN BEGIN
                      TurnAutoModeOff(ByUser);
                      Debug('*** Keyboard & mouse locked, and auto mode off - press Shift + ''K'' to unlock ***');
                    END ELSE
                      Debug('*** Keyboard & mouse locked - press Shift + ''K'' to unlock ***');
                    ChangeCursor(crNo);
                    KeyBoardandMouseLocked := True;
                  END;
                END;
              END;
            Ctrl: {K}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {K}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            END; {CASE}
        Ord('L'):
          CASE ShiftKeys OF
            NoShiftKeys: {L}
              BEGIN
                HelpMsg := 'show line dialogue box';
                IF NOT HelpRequired THEN BEGIN
                  InputDialogueBoxRequired := LineDialogueBox;
                  InputDialogueBox.Show;
                END;
              END;
            CtrlAltShift: {L}
              BEGIN
                HelpMsg := 'logging on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF InLogsCurrentlyKeptMode THEN BEGIN
                    Log('AG LogsCurrentlyKept = OFF');
                    SetMode(LogsCurrentlyKeptModeType, False);
                  END ELSE BEGIN
                    setMode(LogsCurrentlyKeptModeType, True);
                    Log('AG LogsCurrentlyKept = ON');
                  END;
                END;
              END;
            ShiftAlt: {L}
              BEGIN
                HelpMsg := 'turn all loco lights out';
                IF NOT HelpRequired THEN BEGIN
                  IF MessageDialogueWithDefault('Turn all loco lights off?', NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes THEN BEGIN
                    T := 0;
                    WHILE T <= High(Trains) DO BEGIN
                      WITH Trains[T] DO
                        IF Train_HasLights THEN
                          TurnTrainLightsOff(T, OK);
                      Inc(T);
                    END; {WHILE}
                  END;
                END;
              END;
            CtrlAlt: {L}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {L}
              BEGIN
                HelpMsg := 'toggle line thickness';
                IF NOT HelpRequired THEN BEGIN
                  IF ThinLineMode THEN BEGIN
                    ThinLineMode := False;
                    Log('XG ThinLineMode = OFF');
                  END ELSE BEGIN
                    ThinLineMode := True;
                    Log('XG ThinLineMode = ON');
                  END;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {L}
              BEGIN
                HelpMsg := 'point/signal locking on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF InLockingMode THEN
                    SetMode(LockingModeType, TurnOff)
                  ELSE
                    SetMode(LockingModeType, TurnOn);
                END;
              END;
            Ctrl: {L}
              BEGIN
                HelpMsg := 'Rebuild all location occupations';
                IF NOT HelpRequired THEN BEGIN
                  IF MessageDialogueWithDefault('Rebuild all location occupations?',
                                                NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
                  THEN BEGIN
                    Log('A Rebuilding all location occupations');
                    T := 0;
                    WHILE T <= High(Trains) DO BEGIN
                      Log(Trains[T].Train_LocoChipStr + ' G Rebuilding location occupations');
                      SetUpAllLocationOccupationsAbInitio(NOT TimetableLoading, OK);
                      Inc(T);
                    END; {WHILE}
                    Debug('Location occupations rebuilt');
                  END;
                END;
              END;
            Alt: {L}
               BEGIN
                HelpMsg := 'log current time mode on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF LogCurrentTimeMode THEN BEGIN
                    Log('A! LogCurrentTimeMode = OFF');
                    LogCurrentTimeMode := False;
                  END ELSE BEGIN
                    LogCurrentTimeMode := True;
                    Log('A! LogCurrentTimeMode = ON');
                  END;
                END;
              END;
          END; {CASE}
        Ord('M'):
          CASE ShiftKeys OF
            NoShiftKeys: {M}
              BEGIN
                HelpMsg := 'toggle monitoring of straying trains';
                IF NOT HelpRequired THEN BEGIN
                  IF MonitorStrayingTrains THEN BEGIN
                    MonitorStrayingTrains := False;
                    Log('XG MonitorStrayingTrains = OFF');
                  END ELSE BEGIN
                    MonitorStrayingTrains := True;
                    Log('XG MonitorStrayingTrains = ON');
                  END;
                END;
              END;
            CtrlAltShift: {M}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {M}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {M}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {M}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {M}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                  IF MainUnitWindow.Visible THEN
                    MainUnitWindow.Hide
                  ELSE
                    MainUnitWindow.Show;
                  Debug('Main Unit window visible = ' + BoolToStr(MainUnitWindow.Visible, True));
                END;
              END;
            Ctrl: {M}
              BEGIN
                HelpMsg := 'Toggle the StationMonitor web-diagnostics Window visibility';
                IF NOT HelpRequired THEN BEGIN
                  IF InitVarsWindow.StationMonitorsWebDiagnosticsMemo.Visible THEN
                    InitVarsWindow.Hide // StationMonitorsWebDiagnosticsMemo.Hide
                  ELSE
                    InitVarsWindow.Show; //StationMonitorsWebDiagnosticsMemo.Show;
                END;
              END;
            Alt: {M}
              BEGIN
                HelpMsg := 'toggle visibility of menus';
                IF NOT HelpRequired THEN BEGIN
                  IF MenusVisible THEN
                    HideMenus
                  ELSE
                    ShowMenus;
                END;
              END;
          END; {CASE}
        Ord('N'):
          CASE ShiftKeys OF
            NoShiftKeys: {N}
              BEGIN
                { switch night time - should normally be reset by train lights changing }
                HelpMsg := 'toggle night time/day time';
                IF NOT HelpRequired THEN BEGIN
                  IF NightTimeSetByUser THEN BEGIN
                    NightTimeSetByUser := False;
                    Log('AG NightTimeSetByUser = OFF');
                  END ELSE BEGIN
                    DayTimeSetByUser := False;
                    NightTimeSetByUser := True;
                    Log('AG NightTimeSetByUser = ON');
                  END;
                END;
              END;
            CtrlAltShift: {N}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {N}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {N}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {N}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {N}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {N}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {N}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('O'):
          CASE ShiftKeys OF
            NoShiftKeys: {O}
              BEGIN
                HelpMsg := 'show options window';
                IF NOT HelpRequired THEN BEGIN
                  IF InAutoMode THEN
                    MessageDlg('Cannot display options window if in Auto Mode', mtInformation, [mbOK], 0)
                  ELSE BEGIN
                    IF OptionsWindow.Visible THEN
                      OptionsWindow.Visible := False
                    ELSE
                      OptionsWindow.Visible := True;
                  END;
                END;
              END;
            CtrlAltShift: {O}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {O}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {O}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {O}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {O}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {O}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {O}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('P'):
          CASE ShiftKeys OF
            NoShiftKeys: {P}
              BEGIN
                HelpMsg := 'set points';
                IF NOT HelpRequired THEN BEGIN
                  InputDialogueBoxRequired := PointDialogueBox;
                  InputDialogueBox.Show;
                END;
              END;
            CtrlAltShift: {P}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {P}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {P}
              BEGIN
                HelpMsg := 'test fire all points and say which is which';
                IF NOT HelpRequired THEN BEGIN
                  IF MessageDialogueWithDefault('Test fire all points and describe them??',
                                                NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
                  THEN BEGIN
                    FOR P := 0 TO High(Points) DO BEGIN
                      IF Points[P].Point_ManualOperation THEN BEGIN
                        Log('*G P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Points[P].Point_LenzNum) + ') omitting as marked as manual operation');
                        Pause(1000, ProcessMessages);
                      END ELSE BEGIN
                        IF P <> 0 THEN
                          { P is not the first point in the array }
                          Pause(1000, ProcessMessages);

                        IF Points[P].Point_PresentState = Straight THEN
                          Points[P].Point_RequiredState := Diverging
                        ELSE
                          Points[P].Point_RequiredState := Straight;

                        PullPoint(UnknownLocoChipStr, P, NoRoute, NoSubRoute, NOT ForcePoint, ByUser, ErrorMessageRequired, PointResultPending, DebugStr, OK);

                        IF NOT OK THEN
                          Log('*G P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Points[P].Point_LenzNum) + ') not ok ' + PointStateToStr(Points[P].Point_RequiredState))
                        ELSE
                          IF PointResultPending THEN BEGIN
                            { PresentState <> RequiredState: a five second wait should be sufficient }
                            PointFeedbackWaitInSeconds := Round(SecondSpan(Time, Points[P].Point_FeedbackStartTime));
                            IF PointFeedbackWaitInSeconds >= PointFeedbackMaximumWaitInSeconds THEN BEGIN
                              DebugStr := DebugStr + 'P=' + IntToStr(P) + ' pending change to ' + PointStateToStr(Points[P].Point_RequiredState)
                                                   + ' failed after a ' + IntToStr(PointFeedbackWaitInSeconds) + ' second wait';
                              Log('A ' + DebugStr);
                            END;
                          END ELSE
                            Debug('P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Points[P].Point_LenzNum) + ') ok ' + PointStateToStr(Points[P].Point_RequiredState));

                        Pause(1000, ProcessMessages);

                        IF OK THEN BEGIN
                          IF Points[P].Point_PresentState = Straight THEN
                            Points[P].Point_RequiredState := Diverging
                          ELSE
                            Points[P].Point_RequiredState := Straight;
                        END ELSE BEGIN
                          { change the point state if the previous change was unsuccessful }
                          IF Points[P].Point_RequiredState = Straight THEN
                            Points[P].Point_RequiredState := Diverging
                          ELSE
                            Points[P].Point_RequiredState := Straight;
                        END;

                        PullPoint(UnknownLocoChipStr, P, NoRoute, NoSubRoute, NOT ForcePoint, ByUser,  ErrorMessageRequired, PointResultPending, DebugStr, OK);

                        IF NOT OK THEN
                          Log('*G P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Points[P].Point_LenzNum) + ') not ok ' + PointStateToStr(Points[P].Point_RequiredState))
                        ELSE
                          IF PointResultPending THEN BEGIN
                            { PresentState <> RequiredState: a five second wait should be sufficient }
                            PointFeedbackWaitInSeconds := Round(SecondSpan(Time, Points[P].Point_FeedbackStartTime));
                            IF PointFeedbackWaitInSeconds >= PointFeedbackMaximumWaitInSeconds THEN BEGIN
                              DebugStr := DebugStr + 'P=' + IntToStr(P) + ' pending change to ' + PointStateToStr(Points[P].Point_RequiredState)
                                                   + ' failed after a ' + IntToStr(PointFeedbackWaitInSeconds) + ' second wait';
                              Log('A ' + DebugStr);
                            END;
                          END ELSE
                            Debug('P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Points[P].Point_LenzNum) + ') ok ' + PointStateToStr(Points[P].Point_RequiredState));
                      END;
                    END;
                    Debug('+End of points test');
                  END;
                END;
              END;
            CtrlShift: {P}
              BEGIN
                HelpMsg := 'turn point resetting mode on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF PointResettingMode THEN BEGIN
                    PointResettingMode := False;
                    Log('XG PointResettingMode = OFF');
                  END ELSE BEGIN
                    PointResettingMode := True;
                    Log('XG PointResettingMode = ON');
                  END;
                END;
              END;
            Shift: {P}
              BEGIN
                HelpMsg := 'Display latest point settings';
                IF NOT HelpRequired THEN BEGIN
                  SetMode(PreviousPointSettingsModeType, TurnOn);
                  DisplayPreviousPointSettings;
                  InvalidateScreen(UnitRef, 'Display latest point settings in offline mode');
                END;
              END;
            Ctrl: {P}
              BEGIN
                HelpMsg := 'turn point resetting mode on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF PointResettingMode THEN BEGIN
                    PointResettingMode := False;
                    Log('XG PointResettingMode = OFF');
                  END ELSE BEGIN
                    PointResettingMode := True;
                    Log('XG PointResettingMode = ON');
                  END;
                END;
              END;
            Alt: {P}
              BEGIN
                HelpMsg := 'reset all switched points to default state';
                IF NOT HelpRequired THEN BEGIN
                  IF ResetAllSwitchedPoints THEN BEGIN
                    ResetAllSwitchedPoints := False;
                    Log('XG ResetAllSwitchedPoints = OFF');
                  END ELSE
                    IF MessageDialogueWithDefault('Reset all switched points to default state?',
                                                  StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
                    THEN BEGIN
                      ResetAllSwitchedPoints := True;
                      Log('XG ResetAllSwitchedPoints = ON');
                    END;
                END;
              END;
          END; {CASE}
        Ord('Q'):
          CASE ShiftKeys OF
            NoShiftKeys: {Q}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {Q}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Q}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Q}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Q}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Q}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Q}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Q}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('R'):
          CASE ShiftKeys OF
            NoShiftKeys: {R}
              BEGIN
                HelpMsg := 'replay';
                IF NOT HelpRequired THEN BEGIN
                  IF MessageDialogueWithDefault('Play through the saved program?',
                                                NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes
                  THEN BEGIN
                    InitialiseReplay(OK, ErrorMsg);
                    IF NOT OK THEN
                      ShowMessage('Replay mode not enabled: ' + ErrorMsg)
                    ELSE BEGIN
                      Log('AG REPLAY MODE ON');
                      ReplayMode := True;
                      RestoreLogsToPreviousState := True;
                      ShutDownProgram(UnitRef, 'KeyPressedDown');
                      StopSystemTimer;
                      IF InAutoMode THEN
                        TurnAutoModeOff(NOT ByUser);
                      SetMode(LogsCurrentlyKeptModeType, False);
                      StartWithDiagrams := False;
                      SetSystemOffline('System set offline by selecting replay', NOT SoundWarning);
                    END;
                  END;
                END;
              END;
            CtrlAlt: {R}
              BEGIN
                HelpMsg := 'reinitialise all data';
                IF NOT HelpRequired THEN BEGIN
                  Debug('Reinitialising all data');

                  Log('A READ IN AREAS DATA FROM DATABASE');
                  ReadInAreasDataFromDatabase;

                  Log('A READ IN LOCATION DATA FROM DATABASE');
                  IF NOT ResizeMap THEN
                    ReadInLocationDataFromDatabase;

                  SetLength(LocationOccupations, Length(Locations));

                  SetUpLineDrawingVars;

                  Log('A READ IN LINE DATA FROM DATABASE');
                  IF NOT ResizeMap THEN
                    ReadInLineDataFromDatabase
                  ELSE
                    CalculateLinePositions;

                  Log('A READ IN FEEDBACK DATA FROM DATABASE');
                  ReadInFeedbackDataFromDatabase;

                  IF NOT TrackCircuitsInitialised THEN BEGIN
                    { only initialise track circuit once, as doing so a second time removes the data **** }
                    TrackCircuitsInitialised := True;
                    Log('A READ IN TRACK CIRCUIT DATA FROM DATABASE');
                    ReadInTrackCircuitDataFromDatabase;
                  END;

                  Log('A CHECKING LINE CONNECTIONS ARE OK');
                  CheckLineConnectionsAreOK;

                  Log('A READ IN POINT DATA FROM DATABASE');
                  ReadInPointDataFromDatabase;
                  IF PreviousPointSettingsMode THEN
                    DisplayPreviousPointSettings;

                  Log('A READ IN PLATFORM DATA FROM DATABASE');
                  ReadInPlatformDataFromDatabase;

                  Log('A READ IN SIGNAL DATA FROM DATABASE');
                  ReadInSignalDataFromDatabase(NewSignalData);

                  Log('A READ IN ROUTEING EXCEPTIONS FROM DATABASE');
                  ReadInRouteingExceptionsFromDatabase;

                  Log('A INITIALISING LOCATION LINES');
                  InitialiseLocationLines;

                  Debug('All data reinitialised');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {R}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {R}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {R}
              BEGIN
                HelpMsg := 'Reset all window size and positions';
                IF NOT HelpRequired THEN
                  ResetAllWindowsSizeAndPosition;
              END;
            Ctrl: {R}
              BEGIN
                HelpMsg := 'release all held routeing';
                IF NOT HelpRequired THEN BEGIN
                  IF AllHeldRouteingReleased THEN
                    AllHeldRouteingReleased := False
                  ELSE
                    IF MessageDialogueWithDefault('Release all held routeing?',
                                                  NOT StopTimer, mtConfirmation, [mbOK, mbAbort], mbAbort) = mrOK
                    THEN
                      AllHeldRouteingReleased := True;
                END;
              END;
            Shift: {R}
              BEGIN
                HelpMsg := 'allow route messages (if any) to be displayed';
                IF NOT HelpRequired THEN BEGIN
                  DescribeRouteingStatus;
                  R := 0;
                  WHILE R <= Routes_RouteCounter DO BEGIN
                    IF NOT Routes_Cleared[R] THEN BEGIN
                      IF Routes_ApproachControlSignalsMsgWrittenArray[R] THEN BEGIN
                        Routes_ApproachControlSignalsMsgWrittenArray[R] := False;
                        Log('X User resetting Routes_ApproachControlSignalsMsgWrittenArray for R=' + IntToStr(R));
                      END;

                      IF Routes_RoutesSettingUpStalledMsgWrittenArray[R] THEN BEGIN
                        Routes_RoutesSettingUpStalledMsgWrittenArray[R] := False;
                        Log('X User resetting Routes_RouteSettingUpStalledMsgWrittenArray for R=' + IntToStr(R));
                      END;

                      IF Routes_SettingUpFailuresMsgWrittenArray[R] THEN BEGIN
                        Routes_SettingUpFailuresMsgWrittenArray[R] := False;
                        Log('X User resetting Routes_SettingUpFailuresMsgWrittenArray for R=' + IntToStr(R));
                      END;

                      IF Routes_RoutesSettingUpHeldMsgWrittenArray[R] THEN BEGIN
                        Routes_RoutesSettingUpHeldMsgWrittenArray[R] := False;
                        Log('X User resetting Routes_RoutesSettingUpHeldMsgWrittenArray for R=' + IntToStr(R));
                      END;

                      IF Routes_ClearingFailuresMsg1WrittenArray[R] THEN BEGIN
                        Routes_ClearingFailuresMsg1WrittenArray[R] := False;
                        Log('X User resetting Routes_ClearingFailuresMsg1WrittenArray for R=' + IntToStr(R));
                      END;

                      IF Routes_ClearingFailuresMsg2WrittenArray[R] THEN BEGIN
                        Routes_ClearingFailuresMsg2WrittenArray[R] := False;
                        Log('X User resetting Routes_ClearingFailuresMsg2WrittenArray for R=' + IntToStr(R));
                      END;

                      IF Routes_ClearingFailuresMsg3WrittenArray[R] THEN BEGIN
                        Routes_ClearingFailuresMsg3WrittenArray[R] := False;
                        Log('X User resetting Routes_ClearingFailuresMsg3WrittenArray for R=' + IntToStr(R));
                      END;

                      FOR S := 0 TO High(Signals) DO
                        IF Signals[S].Signal_LockFailureNotedInRouteUnit THEN
                          Signals[S].Signal_LockFailureNotedInRouteUnit := False;

                      FOR L := 0 TO High(Lines) DO
                        IF Lines[L].Line_LockFailureNotedInSubRouteUnit THEN
                          Lines[L].Line_LockFailureNotedInSubRouteUnit := False;

                      FOR TC := 0 TO High(TrackCircuits) DO
                        IF TrackCircuits[TC].TC_LockFailureNotedInSubRouteUnit THEN
                          TrackCircuits[TC].TC_LockFailureNotedInSubRouteUnit := False;
                    END;
                    Inc(R);
                  END; {WHILE}

                  T := 0;
                  WHILE T <= High(Trains) DO BEGIN
                    WITH Trains[T] DO BEGIN
                      IF Trains[T].Train_DiagramFound THEN BEGIN
                        FOR I := FirstRouteCreationHeldMsgNumber TO LastRouteCreationHeldMsgNumber DO BEGIN
                          IF Train_RouteCreationHeldMsgWrittenArray[I] THEN BEGIN
                            Train_RouteCreationHeldMsgWrittenArray[I] := False;
                            Log('X User resetting Train_RouteCreationHeldMsgWrittenArray[' + IntToStr(I) + ']');
                          END;
                        END;
                      END;
                    END; {WITH}
                    Inc(T);
                  END; {WHILE}

                  Debug('Logging current routeing status and republishing any route messages');
                END;
              END;
            Alt: {R}
              BEGIN
                HelpMsg := 'toggle showing of route lengths';
                IF NOT HelpRequired THEN BEGIN
                  IF ShowRouteLengths THEN BEGIN
                    ShowRouteLengths := False;
                    Debug('show route lengths = OFF');
                  END ELSE BEGIN
                    ShowRouteLengths := True;
                    Debug('show route lengths = ON');
                  END;
                END;
              END;
          END; {CASE}
        Ord('S'):
          CASE ShiftKeys OF
            NoShiftKeys: {S}
              BEGIN
                HelpMsg := 'set signals on/off';
                IF NOT HelpRequired THEN BEGIN
                  InputDialogueBoxRequired := SignalDialogueBox;
                  InputDialogueBox.Show;
                END;
              END;
            CtrlAltShift: {S}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {S}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {S}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {S}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {S}
              BEGIN
                HelpMsg := 'toggle station start mode';
                IF NOT HelpRequired THEN BEGIN
                  SetMode(StationStartModeType, NOT InStationStartMode);
                  IF InStationStartMode THEN
                    Log('AG Station Start Mode = ON')
                  ELSE
                    Log('AG Station Start Mode = OFF');
                END;
              END;
            Ctrl: {S}
              BEGIN
                HelpMsg := 'stop all locomotives when debug called';
                IF NOT HelpRequired THEN BEGIN
                  StopAllLocomotivesWhenDebugCalled := NOT StopAllLocomotivesWhenDebugCalled;
                  IF StopAllLocomotivesWhenDebugCalled THEN
                    Log('AG Stop All Locomotives When Debug Called = ON')
                  ELSE
                    Log('AG Stop All Locomotives When Debug Called = OFF');
                END;
              END;
            Alt: {S}
              BEGIN
                HelpMsg := 'suspend all active trains? (Y/N)';
                IF NOT HelpRequired THEN BEGIN
                  IF MessageDialogueWithDefault('Suspend all active trains?',
                                                NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbYes) = mrYes
                  THEN BEGIN
                    Log('X Suspending all active trains');
                    T := 0;
                    WHILE T <= High(Trains) DO BEGIN
                      WITH Trains[T] DO BEGIN
                        IF Train_DiagramFound THEN BEGIN
                          IF (Train_CurrentStatus <> Missing) AND (Train_CurrentStatus <> MissingAndSuspended) THEN
                            ChangeTrainStatus(T, Suspended);
                        END;
                      END; {WITH}
                      Inc(T);
                    END; {WHILE}
                    Log('XG All active trains suspended');
                  END;
                END;
              END;
          END; {CASE}
        Ord('T'):
          CASE ShiftKeys OF
            NoShiftKeys: {T}
              BEGIN
                HelpMsg := 'set track circuits on/off';
                IF NOT HelpRequired THEN BEGIN
                  InputDialogueBoxRequired := TrackCircuitDialogueBox;
                  InputDialogueBox.Show;
                END;
              END;
            CtrlAltShift: {T}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {T}
              BEGIN
                HelpMsg := 'turn main timer off/on';
                IF NOT HelpRequired THEN BEGIN
                  IF MainUnitWindow.MainUnitTimer.Enabled THEN BEGIN
                    StopSystemTimer;
                    Log('XG Main Timer tuned off by user');
                  END ELSE BEGIN
                    StartSystemTimer;
                    Log('XG Main Timer tuned on by user');
                  END;
                END;
              END;
            CtrlAlt: {T}
              BEGIN
                HelpMsg := 'switch flashing track circuits on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF DisplayFlashingTrackCircuits THEN
                    DisplayFlashingTrackCircuits := False
                  ELSE
                    DisplayFlashingTrackCircuits := True;
                END;
              END;
            CtrlShift: {T}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {T}
              BEGIN
                HelpMsg := 'toggle display of not for public use trains in station monitors';
                IF NOT HelpRequired THEN BEGIN
                  IF DisplayNotForPublicUseTrainsInStationMonitors THEN BEGIN
                    DisplayNotForPublicUseTrainsInStationMonitors := False;
                    Log('AG DisplayNotForPublicUseTrainsInStationMonitors = OFF');
                    DrawStationMonitorsWindow(StationMonitorsCurrentArea);
                  END ELSE BEGIN
                    DisplayNotForPublicUseTrainsInStationMonitors := True;
                    Log('AG DisplayNotForPublicUseTrainsInStationMonitors = ON');
                    DrawStationMonitorsWindow(StationMonitorsCurrentArea);
                  END;
                END;
              END;
            Ctrl: {T}
              BEGIN
                HelpMsg := 'Open/close TCPIP Window';
                IF NOT HelpRequired THEN BEGIN
                  IF TCPIPForm = NIL THEN
                    ShowMessage('The TCPIP Form has not been created because the link is not live')
                  ELSE BEGIN
                    IF TCPIPForm.Visible THEN
                      TCPIPForm.Visible := False
                    ELSE
                      TCPIPForm.Visible := True;
                  END;
                END;
              END;
            Alt: {T}
              BEGIN
                HelpMsg := 'toggle start repeat journeys on new line in diagram window';
                IF NOT HelpRequired THEN BEGIN
                  IF StartRepeatJourneysOnNewLineInDiagrams THEN BEGIN
                    StartRepeatJourneysOnNewLineInDiagrams:= False;
                    Log('AG StartRepeatJourneysOnNewLineInDiagrams = OFF');
                    DrawStationMonitorsWindow(StationMonitorsCurrentArea);
                  END ELSE BEGIN
                    StartRepeatJourneysOnNewLineInDiagrams:= True;
                    Log('AG StartRepeatJourneysOnNewLineInDiagrams = ON');
                  END;
                  DrawDiagrams(UnitRef, 'KeyPressed (StartRepeatJourneysOnNewLineInDiagrams)');
                END;
              END;
          END; {CASE}
        Ord('U'):
          CASE ShiftKeys OF
            NoShiftKeys: {U}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {U}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {U}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {U}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {U}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {U}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {U}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {U}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('V'):
          CASE ShiftKeys OF
            NoShiftKeys: {V}
              BEGIN
                HelpMsg := 'switch flashing track circuits on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF DisplayFlashingTrackCircuits THEN
                    DisplayFlashingTrackCircuits := False
                  ELSE
                    DisplayFlashingTrackCircuits := True;
                  LOg('A Verbose flag set to ' + BoolToStr(DisplayFlashingTrackCircuits, True));
                END;
              END;
            CtrlAltShift: {V}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {V}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {V}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {V}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {V}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {V}
              { this is here just to duplicate the spacebar stop: the wheel button on the Wireless mouse can't send the Space keystroke for some peculiar reason so it has
                been set up to send Ctrl-V.
              }
              BEGIN
                HelpMsg := 'stop all operations';
                IF NOT HelpRequired THEN
                  StopOrResumeAllOperations('Mouse wheel or Ctrl-V');
              END;
            Alt: {V}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('W'):
          CASE ShiftKeys OF
            NoShiftKeys: {W}
              BEGIN
                HelpMsg := 'show working timetable';
                IF NOT HelpRequired THEN BEGIN
                  IF DisplayWorkingTimetable THEN BEGIN
                    WorkingTimetableWindow.Hide;
                    DisplayWorkingTimetable := False;
                  END ELSE BEGIN
                    WorkingTimetableWindow.Show;
                    DisplayWorkingTimetable := True;
                  END;
                  DrawWorkingTimetable(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {W}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {W}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {W}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {W}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {W}
              BEGIN
                HelpMsg := 'change working timetable window width';
                IF NOT HelpRequired THEN BEGIN
                  IF LargeWorkingTimetableWindowSelected THEN
                    LargeWorkingTimetableWindowSelected := False
                  ELSE
                    LargeWorkingTimetableWindowSelected := True;

                  WorkingTimetableWindow.Hide;
                  WorkingTimetableWindow.Show;
                  DrawWorkingTimetable(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {W}
              BEGIN
                HelpMsg := 'write some variable data To file';
                IF NOT HelpRequired THEN
                  WriteVariableDataToFile;
              END;
            Alt: {W}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('X'):
          CASE ShiftKeys OF
            NoShiftKeys: {X}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {X}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {X}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {X}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {X}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {X}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {X}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {X}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('Y'):
          CASE ShiftKeys OF
            NoShiftKeys: {Y}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {Y}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Y}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Y}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Y}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Y}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Y}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Y}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord('Z'):
          CASE ShiftKeys OF
            NoShiftKeys: {Z}
              BEGIN
                HelpMsg := 'zoom the whole screen';
                IF NOT HelpRequired THEN BEGIN
                  HideStatusBarAndUpDownIndications;

                  CASE ZoomScaleFactor OF
                    1000:
                      BEGIN
                        ZoomScaleFactor := 750; { 1.3 }
                        WindowPenWidth := 1;
                        ZoomAmountStr := '130%';
                      END;
                    750:
                      BEGIN
                        ZoomScaleFactor := 666; { 1.5 }
                        WindowPenWidth := 2;
                        ZoomAmountStr := '150%';
                      END;
                    666:
                      BEGIN
                        ZoomScaleFactor := 500; { 2.0 }
                        WindowPenWidth := 3;
                        ZoomAmountStr := '200%';
                      END;
                    500:
                      BEGIN
                        ZoomScaleFactor := 400; { 2.5 }
                        WindowPenWidth := 4;
                        ZoomAmountStr := '250%';
                      END;
                    400:
                      BEGIN
                        ZoomScaleFactor := 333; { 3.0 }
                        WindowPenWidth := 5;
                        ZoomAmountStr := '300%';
                      END;
                    333:
                      BEGIN
                        ZoomScaleFactor := 285; { 3.5 }
                        WindowPenWidth := 6;
                        ZoomAmountStr := '350%';
                      END;
                    285:
                      BEGIN
                        ZoomScaleFactor := 250; { 4.0 }
                        WindowPenWidth := 7;
                        ZoomAmountStr := '400%';
                      END;
                    250:
                      BEGIN
                        ZoomScaleFactor := 222; { 4.5 }
                        WindowPenWidth := 8;
                        ZoomAmountStr := '450%';
                      END;
                    222:
                      BEGIN
                        ZoomScaleFactor := 200; { 5.0 }
                        WindowPenWidth := 9;
                        ZoomAmountStr := '500%';
                      END;
                    200:
                      BEGIN
                        ZoomScaleFactor := 1000; { 1.0 }
                        WindowPenWidth := 1;
                      END;
                  END; {CASE}

                  IF ZoomScaleFactor = 1000 THEN BEGIN
                    Zooming := False;
                    WriteToStatusBarPanel(StatusBarPanel2, 'Screen zoom off');
                  END ELSE BEGIN
                    Zooming := True;
                    WriteToStatusBarPanel(StatusBarPanel2, 'Screen set to ' + ZoomAmountStr + IfThen(ZoomAmountStr = '500%',
                                                                                                     ' (maximum size) ',
                                                                                                     ''));
                  END;

                  ReinitialiseFWPRailWindowVariables := True;
                  ShowStatusBarAndUpDownIndications;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {Z}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Z}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Z}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Z}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Z}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Z}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Z}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
         END; {CASE}
        Ord(187): { = or + } { used by numeric + and minus }
          CASE ShiftKeys OF
            NoShiftKeys: {=}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {+}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {+}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {=}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {+}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {+}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {=}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {=}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord(189): { - or _ }
          CASE ShiftKeys OF
            NoShiftKeys: {-}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {_}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {_}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {-}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {_}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {_}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {-}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {-}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord(188): { , or < }
          CASE ShiftKeys OF
            NoShiftKeys: {,}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {<}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {<}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {,}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {<}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {<}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {,}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {,}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord(190): { . or > }
          CASE ShiftKeys OF
            NoShiftKeys: {.}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {>}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {>}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {.}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {>}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {>}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {.}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {.}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord(191): { / or ? }
          CASE ShiftKeys OF
            NoShiftKeys: {/}
              BEGIN
                { used for testing functions and procedures - the code is in TestUnit.pas }
                HelpMsg := 'test procedures';
                IF NOT HelpRequired THEN
                  TestProc(OK);
              END;
            CtrlAltShift: {?}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {?}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {/}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {?}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {?}
              BEGIN
                HelpMsg := 'toggle list of locos';
                IF NOT HelpRequired THEN BEGIN
                  LocoUtilsWindow.Visible := True;
                  LocoUtilsWindow.BringToFront;
                  ListLocosByChip;
                  Log('A List of locos made visible');
                END;
              END;
            Ctrl: {/}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {/}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord(192): { ' or @ }
          CASE ShiftKeys OF
            NoShiftKeys: {'}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {@}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {@}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {'}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {@}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {@}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {'}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {'}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord(220): {\ or |}
          CASE ShiftKeys OF
            NoShiftKeys: {\}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {|}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {|}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {|}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {|}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {|}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {\}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {\}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        Ord(222): {#}
          CASE ShiftKeys OF
            NoShiftkeys: {#}
              BEGIN
                HelpMsg := 'advance time to five seconds before next minute';
                IF NOT HelpRequired THEN BEGIN
                  DecodeTime(CurrentRailwayTime, Hour, Min, Sec, MSec);
                  IF SecondOf(CurrentRailwayTime) < 55 THEN
                    Sec := 55;
                  CurrentRailwayTime := EncodeTime(Hour, Min, Sec, MSec);
                END;
              END;
            CtrlAltShift: {~}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {~}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {#}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {~}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {~}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {#}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {#}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Multiply:
          CASE ShiftKeys OF
            NoShiftKeys: {Multiply}
              BEGIN
                { can press either '*', Insert, or the star key on the numeric keypad to add line of asterisks to the log }
                HelpMsg := 'insert line of asterisks into log';
                IF NOT HelpRequired THEN BEGIN
                  WriteToLogFileAndTestFile := True;
                  DrawLineInLogFile(UnknownLocoChipAsZeroesStr, 'X', '*', UnitRef);
                  WriteToLogFileAndTestFile := False;
                  Debug('!*');
                END;
              END;
            CtrlAltShift: {Multiply}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Multiply}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Multiply}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Multiply}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Multiply}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Multiply}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Multiply}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Insert:
          CASE ShiftKeys OF
            NoShiftKeys: {Insert}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {Insert}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Insert}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Insert}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Insert}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Insert}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Insert}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Insert}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Delete:
          CASE ShiftKeys OF
            NoShiftKeys: {Delete}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {Delete}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Delete}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Delete}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Delete}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Delete}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Delete}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Delete}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Return: {Enter}
          CASE ShiftKeys OF
            NoShiftKeys: {Enter}
              BEGIN
                HelpMsg := 'toggle auto state';
                IF NOT HelpRequired THEN BEGIN
                  IF InAutoMode THEN
                    TurnAutoModeOff(ByUser)
                  ELSE
                    TurnAutoModeOn;
                END;
              END;
            CtrlAltShift: {Enter}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Enter}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Enter}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Enter}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Enter}
              BEGIN
                HelpMsg := 'toggle full screen with border';
                IF NOT HelpRequired THEN BEGIN
                  IF ScreenMode <> FullScreenWithStatusBarMode THEN
                    ScreenMode := FullScreenWithStatusBarMode
                  ELSE
                    ScreenMode := DefaultWindowedScreenMode;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {Enter}
              BEGIN
                HelpMsg := 'Set System Online/Offline';
                IF NOT HelpRequired THEN BEGIN
                  IF SystemOnline THEN BEGIN
                    Log('XG Setting system offline');
                    SetSystemOffline('System set offline by user', NOT SoundWarning)
                  END ELSE BEGIN
                    Log('XG Attempting to set system online...');
                    IF SetSystemOnline THEN
                      Log('XG Attempt to set system online succeeded')
                    ELSE
                      Log('XG Attempt to set system online failed');
                  END;
                END;
              END;
            Alt: {Enter}
              BEGIN
                HelpMsg := 'toggle full screen';
                IF NOT HelpRequired THEN BEGIN
                  IF ScreenMode <> FullScreenMode THEN
                    ScreenMode := FullScreenMode
                  ELSE
                    ScreenMode := DefaultWindowedScreenMode;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
          END; {CASE}
        vk_Back:
          CASE ShiftKeys OF
            NoShiftKeys: {Backspace}
              BEGIN
        //        { Backspace - starts mode whereby routes set up by hand are cleared by trains passing, but timetable doesn't operate }
                  HelpMsg := 'toggle route clearing mode';
        //        IF NOT HelpRequired THEN BEGIN
        //          IF RouteClearingOnlyMode THEN BEGIN
        //            TurnAutoModeOff(ByUser);
        //            RouteClearingOnlyMode := False;
        //            Log('A Route clearing mode = off');
        //          END ELSE BEGIN
        //            TurnAutoModeOn;
        //            RouteClearingOnlyMode := True;
        //            { Reset the clock to midnight - doesn't need to run at all, but it shows something is happening, and is useful for the debug output file }
        //            SetCurrentRailwayTime(EncodeTime(0, 0, 0, 0));
        //            Log('A Route clearing mode = on, clock set to midnight');
        //          END;
        //        END;
              END;
            CtrlAltShift: {Backspace}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Backspace}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Backspace}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Backspace}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Backspace}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Backspace}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Backspace}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Escape:
          CASE ShiftKeys OF
            NoShiftKeys: {Escape}
              BEGIN
                HelpMsg := 'exit program';
                IF NOT HelpRequired THEN BEGIN
                  IF ScreenColoursSetForPrinting THEN BEGIN
                    ResetScreenColoursAfterPrinting;
                    FWPRailWindow.Refresh;
                  END ELSE
                    { If the screen is zoomed, then return it to normal }
                    IF ZoomScaleFactor <> 1000 THEN BEGIN
                      ZoomScaleFactor := 1000;
                      Zooming := False;
                      SetCaption(FWPRailWindow, '');
                      WindowPenWidth := 1;

                      ReinitialiseFWPRailWindowVariables := True;
                      InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                    END ELSE BEGIN
                      { Close the program after two consecutive escapes }
                      IF EscKeyStored THEN BEGIN
                        Log('A Shutdown requested by user pressing Escape twice {BLANKLINEBEFORE}');
                        ShutDownProgram(UnitRef, 'KeyPressedDown');
                      END ELSE BEGIN
                        EscKeyStored := True;
                        Debug('Shutdown requested by user pressing Escape - press it again to confirm');
                      END;
                    END;
                END;
              END;
            ShiftAlt: {Escape}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Escape}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Escape}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Escape}
              BEGIN
                HelpMsg := 'exit program with log restored to previous state';
                IF NOT HelpRequired THEN BEGIN
                  { close the program after two consecutive escapes }
                  IF EscKeyStored THEN BEGIN
                    Log('A Shutdown requested by user pressing Escape twice - log to be restored to previous state');
                    RestoreLogsToPreviousState := True;
                    ShutDownProgram(UnitRef, 'KeyPressedDown');
                  END ELSE BEGIN
                    EscKeyStored := True;
                    Debug('Shutdown requested by user pressing Escape - press it again to confirm');
                  END;
                END;
              END;
            Ctrl: {Escape}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Escape}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Left:
          CASE ShiftKeys OF
            NoShiftKeys: {Left}
              BEGIN
                HelpMsg := 'Move edited object to the left';
                IF NOT HelpRequired THEN BEGIN
                  IF EditMode THEN
                    MoveObjectLeft;
                END;
              END;
            ShiftAlt: {Left}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {Left}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Left}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Left}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Left}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Left}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Left}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Right:
          CASE ShiftKeys OF
            NoShiftKeys: {Right}
              BEGIN
                HelpMsg := 'Move edited object to the right';
                IF NOT HelpRequired THEN BEGIN
                  IF EditMode THEN
                    MoveObjectRight;
                END;
              END;
            CtrlAltShift: {Right}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Right}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Right}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Right}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Right}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Right}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Right}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Up:
          CASE ShiftKeys OF
            NoShiftKeys: {Up}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {Up}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Up}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Up}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Up}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Up}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Up}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Up}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Down:
          CASE ShiftKeys OF
            NoShiftKeys: {Down}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {Down}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Down}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Down}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Down}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Down}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Down}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {Down}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Space:
          CASE ShiftKeys OF
            NoShiftKeys: {Space}
              BEGIN
                HelpMsg := 'stop or resume all operations';
                IF NOT HelpRequired THEN
                  StopOrResumeAllOperations(DescribeKey(KeyToTest, InputShiftState));
              END;
            CtrlAltShift: {Space}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Space}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Space}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Space}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Space}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {Space}
              BEGIN
                HelpMsg := 'switch all points and signals off (in case they''ve been left energised)';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT SystemOnline THEN
                    Debug('Cannot deselect points - system offline')
                  ELSE BEGIN
                    FOR P := 0 TO High(Points) DO
                      EmergencyDeselectPoint(P, OK);
                    Log('PG User has switched all points off');

                    FOR S := 0 TO High(Signals) DO
                      EmergencyDeselectSignal(S, OK);
                    Debug('All signal now switched off');
                    Log('S User has switched all signals off');
                  END;
                END;
              END;
            Alt: {Space}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_Tab:
          CASE ShiftKeys OF
            NoShiftKeys: {Tab}
              BEGIN
                HelpMsg := 'toggle clock speed normal/fastest';
                IF NOT HelpRequired THEN BEGIN
                  IF (RailwayTimeInterval = Faster) OR (RailwayTimeInterval = Fastest) OR (RailwayTimeInterval = Slower) THEN
                    SetRailwayTimeInterval(Normal)
                  ELSE
                    SetRailwayTimeInterval(Faster)
                END;
              END;
            CtrlAltShift: {Tab}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Tab}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {Tab}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {Tab}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {Tab}
              BEGIN
                HelpMsg := 'toggle clock speed fastest';
                IF NOT HelpRequired THEN
                  SetRailwayTimeInterval(Fastest)
              END;
            Ctrl: {Tab}
              BEGIN
                HelpMsg := 'toggle clock speed slower';
                IF NOT HelpRequired THEN
                  SetRailwayTimeInterval(Slower)
              END;
            Alt: {Tab}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}

        { vk_Insert, vk_Multiply - see above under Ord('8') }

        vk_F1: { primitive help }
          CASE ShiftKeys OF
            NoShiftKeys: {F1}
              BEGIN
                HelpMsg := 'show key descriptions';
                IF NOT HelpRequired THEN
                  WriteHelpText(HelpMsg);
              END;
            CtrlAltShift: {F1}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F1}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {F1}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {F1}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {F1}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {F1}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {F1}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_F2: { Signals }
          CASE ShiftKeys OF
            NoShiftKeys: {F2}
              BEGIN
                HelpMsg := 'show signal and bufferstop numbers';
                IF NOT HelpRequired THEN BEGIN
                  ShowSignalAndBufferStopNums := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing signal and bufferstop numbers');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {F2}
              BEGIN
                HelpMsg := 'show signals from which user must drive';
                IF NOT HelpRequired THEN BEGIN
                  IF ShowSignalsFromWhichUserMustDrive THEN
                    ShowSignalsFromWhichUserMustDrive := False
                  ELSE
                    ShowSignalsFromWhichUserMustDrive := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            ShiftAlt: {F2}
              BEGIN
                HelpMsg := 'show signal resetting track circuits in status bar';
                IF NOT HelpRequired THEN BEGIN
                  IF ShowSignalResettingTrackCircuitsInStatusBar THEN
                    ShowSignalResettingTrackCircuitsInStatusBar := False
                  ELSE
                    ShowSignalResettingTrackCircuitsInStatusBar := True;
                END;
              END;
            CtrlAlt: {F2}
              BEGIN
                HelpMsg := 'show signals where there are hold markers and those not used for routeing';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing signals where there are hold markers and those not used for routeing');
                  ShowSignalsWhereRouteingCanBeHeldAndThoseNotUsedForRouteing := True;

                  { List the colours for the forgetful }
                  DisplayLineColoursWindow.Visible := True;
                  AddRichLine(DisplayLineColoursWindow.DisplayLineColoursWindowRichedit,
                                                                       '<colour=' + ColourToStr(clLime) + '>' + ColourToStr(clLime) + '= station start hold' + '</colour>');
                  AddRichLine(DisplayLineColoursWindow.DisplayLineColoursWindowRichedit,
                                                                           '<colour=' + ColourToStr(clYellow) + '>' + ColourToStr(clYellow) + '= route hold' + '</colour>');
                  AddRichLine(DisplayLineColoursWindow.DisplayLineColoursWindowRichedit,
                                                                  '<colour=' + ColourToStr(clAqua) + '>' + ColourToStr(clAqua) + '= not in use for routeing' + '</colour>');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlShift: {F2}
              BEGIN
                HelpMsg := 'show where signals'' junction indicators point';
                IF NOT HelpRequired THEN BEGIN
//                  IF ShowSignalJunctionDestinations THEN
//                    ShowSignalJunctionDestinations := False
//                  ELSE
                    ShowSignalJunctionDestinations := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {F2}
              BEGIN
                HelpMsg := 'show signal numbers and adjacent track circuits';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing signal numbers and adjacent track circuits');
                  ShowSignalsWithAdjacentTrackCircuits := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {F2}
              BEGIN
                HelpMsg := 'show signals with theatre destinations';
                IF NOT HelpRequired THEN BEGIN
                  ShowSignalsAndBufferStopsWithTheatreDestinations := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing theatre destinations');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Alt: {F2}
              BEGIN
                HelpMsg := 'show/hide hidden station signal aspects';
                IF NOT HelpRequired THEN BEGIN
                  IF ShowSignalHiddenStationSignalAspects THEN BEGIN
                    WriteToStatusBarPanel(StatusBarPanel2, 'Not displaying hidden station signal aspects');
                    ShowSignalHiddenStationSignalAspects := False;
                  END ELSE BEGIN
                    WriteToStatusBarPanel(StatusBarPanel2, 'Showing hidden station signal aspects');
                    ShowSignalHiddenStationSignalAspects := True;
                  END;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
          END; {CASE}
        vk_F3: { Points }
          CASE ShiftKeys OF
            NoShiftKeys:
              BEGIN
                HelpMsg := 'show point numbers';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing point numbers');
                  ShowPointDetail := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {F3}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F3}
              BEGIN
                HelpMsg := 'show Lenz point unit groups';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing Lenz points grouped by point unit');
                  ShowLenzPointUnitGroups := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAlt: {F3}
              BEGIN
                HelpMsg := 'show points straight and diverging states';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing points (straight and diverging highlighted)');
                  ShowPointsStraightAndDiverging := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlShift: {F3}
              BEGIN
                HelpMsg := 'show point default state';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing point default state');
                  ShowPointDefaultState := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {F3}
              BEGIN
                HelpMsg := 'show Lenz points numbers';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing Lenz point numbers');
                  ShowLenzPointNumbers := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {F3}
              BEGIN
                HelpMsg := 'show points that are locked';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing points that are locked');
                  ShowPointsThatAreLocked := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Alt: {F3}
              BEGIN
                HelpMsg := 'show point type';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing point type');
                  ShowPointType := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
          END; {CASE}
        vk_F4: { Lines }
          CASE ShiftKeys OF
            NoShiftKeys: {F4}
              BEGIN
                HelpMsg := 'show line detail';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT ShowLineDetail THEN BEGIN
                    ShowLineDetail := True;
                    ListLineColours := False;
                  END ELSE
                    IF ShowLineDetail AND NOT ListLineColours THEN
                      ListLineColours := True
                    ELSE BEGIN
                      ShowLineDetail := False;
                      ListLineColours := False;
                    END;

                  { List the line colours for the forgetful (added this bit 12/12/07 because FWP forgot which was which!) }
                  IF ListLineColours THEN BEGIN
                    WriteToStatusBarPanel(StatusBarPanel2, 'Showing line detail');
                    DisplayLineColoursWindowHeight := 0;
                    DisplayLineColoursWindow.Width := 0;
                    DisplayLineColoursWindow.Visible := True;
                    DisplayLineColoursWindow.DisplayLineColoursWindowRichedit.Clear;

                    FOR XTypeOfLine := FirstTypeOfLine TO LastTypeOfLine DO BEGIN
                      WITH DisplayLineColoursWindow.Canvas DO BEGIN
                        Str := ColourToStrForUser(GetLineTypeColour(XTypeOfLine)) + ' = ' + TypeOfLineToStr(XTypeOfLine);

                        IF TextWidth(Str) > DisplayLineColoursWindow.Width THEN
                          DisplayLineColoursWindow.Width := TextWidth(Str);
                        AddRichLine(DisplayLineColoursWindow.DisplayLineColoursWindowRichedit,
                                    '<colour=' + ColourToStr(GetLineTypeColour(XTypeOfLine)) + '>'
                                    + Str
                                     + '</colour>');
                      END; {WITH}
                    END; {FOR}
                  END;

                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {F4}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F4}
              BEGIN
                HelpMsg := 'Show which heel and non-heel lines that lock points';
                IF NOT HelpRequired THEN BEGIN
                  ShowLinesWhichLockPoints := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing which heel and non-heel lines lock points');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAlt: {F4}
              BEGIN
                HelpMsg := 'show the absolute Up X values for lines (in 1/1000)';
                IF NOT HelpRequired THEN BEGIN
                  ShowLinesUpXAbsoluteValue := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing the absolute Up X values for lines (in 1/1000)');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlShift: {F4}
              BEGIN
                HelpMsg := 'show line directions and non-through locations';
                IF NOT HelpRequired THEN BEGIN
                  ShowLineDirectionDetail := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing line directions (blue) and non-through locations (red)');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {F4}
              BEGIN
                HelpMsg := 'show line numbers';
                IF NOT HelpRequired THEN BEGIN
                  ShowLineNumbers := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing line numbers');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {F4}
              BEGIN
                HelpMsg := 'show line gradients';
                IF NOT HelpRequired THEN BEGIN
                  ShowLineGradients := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing line gradients (bidirectional lines show U/D)');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Alt: {F4}
              BEGIN
                HelpMsg := 'close the program';
                IF NOT HelpRequired THEN BEGIN
                  { close the program after two consecutive escapes }
                  FWPRailWindow.FWPRailWindowClose(NIL, CloseAction);
                  Log('A Shutdown requested by user pressing Alt F4 {BLANKLINEBEFORE}');
                END;
              END;
          END; {CASE}
        vk_F5: { Track Circuits }
          CASE ShiftKeys OF
            NoShiftKeys: {F5}
              BEGIN
                HelpMsg := 'show track circuits';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing track circuits');
                  ShowTrackCircuits := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {F5}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F5}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {F5}
              BEGIN
                HelpMsg := 'toggle show adjacent-track-circuit mode';
                IF NOT HelpRequired THEN BEGIN
                  IF InShowAdjacentTrackCircuitMode THEN BEGIN
                    SetMode(ShowAdjacentTrackCircuitModeType, False);
                    WriteToStatusBarPanel(StatusBarPanel2, 'Showing adjacent-track-circuit mode = OFF');
                  END ELSE BEGIN
                    SetMode(ShowAdjacentTrackCircuitModeType, True);
                    WriteToStatusBarPanel(StatusBarPanel2, 'Showing adjacent-track-circuit mode = ON');
                  END;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlShift: {F5}
              BEGIN
                HelpMsg := 'show track circuits routed over';
                IF NOT HelpRequired THEN BEGIN
                  ShowTrackCircuitsRoutedOver := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing track circuits routed over');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {F5}
              BEGIN
                HelpMsg := 'show track-circuits lengths in inches';
                IF NOT HelpRequired THEN BEGIN
                  ShowTrackCircuitLengths := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing track-circuit lengths in inches');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {F5}
              BEGIN
                HelpMsg := 'highlight speed restrictions';
                IF NOT HelpRequired THEN BEGIN
                  HighlightTrackCircuitSpeedRestrictions := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Highlight speed restrictions');
                  SaveTCSpeedRestrictionColour := TCSpeedRestrictionColour;
                  TCSpeedRestrictionColour := clYellow;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Alt: {F5}
              BEGIN
                HelpMsg := 'show lines without track circuits';
                IF NOT HelpRequired THEN BEGIN
                  ShowLinesWithoutTrackCircuits := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing lines without track circuits');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
          END; {CASE}
        vk_F6: { Locations }
          CASE ShiftKeys OF
            NoShiftKeys: {F6}
              BEGIN
                HelpMsg := 'show locations';
                IF NOT HelpRequired THEN BEGIN
                  ShowLocations := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing locations');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {F6}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F6}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {F6}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {F6}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {F6}
              BEGIN
                HelpMsg := 'show location lengths';
                IF NOT HelpRequired THEN BEGIN
                  ShowLocationLengthDetail := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing location lengths');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {F6}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {F6}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_F7: { Areas }
          CASE ShiftKeys OF
            NoShiftKeys: {F7}
              BEGIN
                HelpMsg := 'show areas';
                IF NOT HelpRequired THEN BEGIN
                  ShowAreas := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing areas (Holding=Yellow; Reversing=Red; H&R=Aqua; Neither=Green)');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {F7}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F7}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {F7}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {F7}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {F7}
              BEGIN
                HelpMsg := 'RDC Mode on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF InRDCMode THEN
                    SetMode(RDCModeType, TurnOff)
                  ELSE BEGIN
                    SetMode(RDCModeType, TurnOn);
                    StartRailDriver;
                  END;
                END;
              END;
            Ctrl: {F7}
              BEGIN
                HelpMsg := 'show RailDriver window';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT InRDCMode THEN
                    Debug('RDC Mode not on')
                  ELSE BEGIN
                    IF RailDriverWindow.Visible THEN
                      RailDriverWindow.Visible := False
                    ELSE
                      RailDriverWindow.Visible := True;
                  END;
                END;
              END;
            Alt: {F7}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_F8: { Feedback Data }
          CASE ShiftKeys OF
            NoShiftKeys: {F8}
              BEGIN
                HelpMsg := 'show track-circuit feedback data';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing track-circuit feedback data');
                  ShowTrackCircuitFeedbackDataInSeparateColours := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAltShift: {F8}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F8}
              BEGIN
              END;
            CtrlAlt: {F8}
              BEGIN
                HelpMsg := 'show chosen feedback units';
                IF NOT HelpRequired THEN BEGIN
                  IF InputQuery('Feedback Units', 'Enter a valid feedback unit number', Str) THEN BEGIN
                    IF NOT TryStrToInt(Str, ShowFeedbackUnitNum) THEN
                      ShowMessage('"' + Str + '" is not a valid integer')
                    ELSE
                      IF (ShowFeedbackUnitNum < FirstFeedbackUnit) OR (ShowFeedbackUnitNum > LastFeedbackUnit) THEN
                        ShowMessage('"' + Str + '" is not a valid feedback unit number')
                      ELSE BEGIN
                        ShowOneFeedbackUnitOnly := True;
                        InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                      END;
                  END;
                END;
              END;
            CtrlShift: {F8}
              BEGIN
                HelpMsg := 'show feedback strings in debug window';
                IF NOT HelpRequired THEN BEGIN
                  IF DisplayFeedbackStringsInDebugWindow THEN
                    DisplayFeedbackStringsInDebugWindow := False
                  ELSE BEGIN
                    DisplayFeedbackStringsInDebugWindow := True;
                    WriteToStatusBarPanel(StatusBarPanel2, 'Showing feedback strings in debug window');
                  END;
                END;
              END;
            Shift: {F8}
              BEGIN
                HelpMsg := 'show track-circuit feedback data (' + ColourToStrForUser(TCFeedbackDataOutOfUseColour) + ' = out of use)';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing track-circuit feedback data' + ' ('
                                                                                                     + ColourToStrForUser(TCFeedbackDataOutOfUseColour) + ' = out of use)');
                  ShowTrackCircuitFeedbackDataInUse := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {F8}
              BEGIN
                HelpMsg := 'show point feedback data in separate colours';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing point feedback data');
                  ShowPointFeedbackDataInSeparateColours := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Alt: {F8}
              BEGIN
                HelpMsg := 'show point feedback data (' + ColourToStrForUser(PointFeedbackDataOutOfUseColour) + ' = out of use, '
                                                                                                      + ColourToStrForUser(PointsWithoutFeedbackColour) + ' = no feedback)';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing point feedback data' + ' (' + ColourToStrForUser(PointFeedbackDataOutOfUseColour) + ' = out of use, '
                                                         + ColourToStrForUser(PointsWithoutFeedbackColour) + ' = no feedback)');
                  ShowPointFeedbackDataInUse := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
          END; {CASE}
        vk_F9: { journeys }
          CASE ShiftKeys OF
            NoShiftKeys: {F9}
              ; {avoid using F9 as that's the Delphi run key }
            CtrlAltShift: {F8}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F8}
              BEGIN
              END;
            CtrlAlt: {F9}
              BEGIN
                HelpMsg := 'write train journeys record to file';
                IF NOT HelpRequired THEN BEGIN
                  T := 0;
                  WHILE T <= High(Trains) DO BEGIN
                    IF Trains[T].Train_DiagramFound THEN
                      WriteTrainJourneysRecordToLogFile(T, FullRecord);
                    Inc(T);
                  END; {WHILE}
                END;
              END;
            CtrlShift: {F9}
              BEGIN
                HelpMsg := 'show partial journey record visibility';
                IF NOT HelpRequired THEN BEGIN
                  T := 0;
                  WHILE T <= High(Trains) DO BEGIN
                    IF Trains[T].Train_DiagramFound THEN
                      WriteTrainJourneysRecordToLockListWindow(T, FullRecord);
                    Inc(T);
                  END; {WHILE}
                END;
              END;
            Shift: {F9}
              BEGIN
                HelpMsg := 'write partial train journeys record to file';
                IF NOT HelpRequired THEN BEGIN
                  T := 0;
                  WHILE T <= High(Trains) DO BEGIN
                    IF Trains[T].Train_DiagramFound THEN
                      WriteTrainJourneysRecordToLogFile(T, NOT FullRecord);
                    Inc(T);
                  END; {WHILE}
                END;
              END;
            Ctrl: {F9}
              BEGIN
                HelpMsg := 'show full journey record window';
                IF NOT HelpRequired THEN BEGIN
                  T := 0;
                  WHILE T <= High(Trains) DO BEGIN
                    IF Trains[T].Train_DiagramFound THEN
                      WriteTrainJourneysRecordToLockListWindow(T, NOT FullRecord);
                    Inc(T);
                  END; {WHILE}
                END;
              END;
            Alt: {F9}
              BEGIN
                HelpMsg := 'show options window';
                IF NOT HelpRequired THEN BEGIN
                  IF OptionsWindow.Visible THEN
                    OptionsWindow.Visible := False
                  ELSE
                    OptionsWindow.Visible := True;
                END;
              END;
          END; {CASE}
        vk_F10: { Location occupations }
          CASE ShiftKeys OF
            NoShiftKeys: {F10}
              BEGIN
                HelpMsg := 'Write location occupations to screen';
                IF NOT HelpRequired THEN
                  ProcessLocationOccupations(NOT Rebuild, NOT WriteToFile);
              END;
            CtrlAltShift: {F10}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F10}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {F10}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {F10}
              BEGIN
                HelpMsg := 'Write location occupations to screen and file, rebuilding first';
                IF NOT HelpRequired THEN
                  ProcessLocationOccupations(Rebuild, WriteToFile);
              END;
            Shift: {F10}
              BEGIN
                HelpMsg := 'Write location occupations to screen and file';
                IF NOT HelpRequired THEN
                  ProcessLocationOccupations(NOT Rebuild, WriteToFile);
              END;
            Ctrl: {F10}
              BEGIN
                HelpMsg := 'Write location occupations to screen, rebuilding first';
                IF NOT HelpRequired THEN
                  ProcessLocationOccupations(Rebuild, NOT WriteToFile);
              END;
            Alt: {F10}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END;
        vk_F11: { Miscellaneous items }
          CASE ShiftKeys OF
            NoShiftKeys: {F11}
              BEGIN
                HelpMsg := 'toggle lock list window visibility';
                IF NOT HelpRequired THEN
                  WriteRouteInfoToLockListWindow;
              END;
            CtrlAltShift: {F11}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F11}
              BEGIN
                HelpMsg := 'switch signals off/on';     { sets semaphores off for some funny reason ********** }
                IF NOT HelpRequired THEN BEGIN
                  IF NOT AllSignalsSwitchedOff THEN BEGIN
                    SaveSignalsCurrentState;
                    TurnAllSignalsOff;
                    AllSignalsSwitchedOff := True;
                    WriteToStatusBarPanel(StatusBarPanel2, 'All signals switched off');
                    Log('A All signals switched off');
                  END ELSE BEGIN
                    RestoreAllSignalsToPreviousState;
                    AllSignalsSwitchedOff := False;
                    Log('A All signals restored to previous state');
                    WriteToStatusBarPanel(StatusBarPanel2, 'All signals restored to previous state');
                  END;
                END;
              END;
            CtrlAlt: {F11}
              BEGIN
                HelpMsg := 'toggle screen colours for printing';
                IF NOT HelpRequired THEN BEGIN
                  IF ScreenColoursSetForPrinting THEN
                    ResetScreenColoursAfterPrinting
                  ELSE BEGIN
                    SetScreenColoursBeforePrinting;
                    ScreenColoursSetForPrinting := True;
                    Debug('Screen colours set for printing - press Escape to reset');
                  END;
                  FWPRailWindow.Refresh;
                END;
              END;
            CtrlShift: {F11}
              BEGIN
                HelpMsg := 'show mouse rectangles';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Showing mouse rectangles');
                  ShowMouseRectangles := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {F11}
              BEGIN
                HelpMsg := 'show which equipment is locked and by whom';
                IF NOT HelpRequired THEN
                  WriteLockingDataToLockListWindow;
              END;
            Ctrl: {F11}
              BEGIN
                HelpMsg := 'Compare Two Databases';
                IF NOT HelpRequired THEN BEGIN
                  CompareTwoLineDatabases('LineData', 'mdb', 'LineData - Copy', 'mdb');
                  CompareTwoLocationDatabases('LocationData', 'mdb', 'LocationData - Copy', 'mdb');
                  CompareTwoSignalDatabases('SignalData', 'mdb', 'SignalData - Copy', 'mdb');
                  CompareTwoPointDatabases('PointData', 'mdb', 'PointData - Copy', 'mdb');
                  CompareTwoTrackCircuitDatabases('TrackCircuitData', 'mdb', 'TrackCircuitData - Copy', 'mdb');
                END;
              END;
            Alt: {F11}
              BEGIN
                HelpMsg := 'Format Check all Files';
                IF NOT HelpRequired THEN
                  FormatCheckAllFiles
              END;
          END; {CASE}
        vk_F12:
          { Note: this used to start the debugger if running in Delphi IDE debugger mode }
          CASE ShiftKeys OF
            NoShiftKeys: {F12}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAltShift: {F12}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {F12}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {F12}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlShift: {F12}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Shift: {F12}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Ctrl: {F12}
              BEGIN
                HelpMsg := 'Write out the source code .pas files to ensure correct CR/LF line endings';
                IF NOT HelpRequired THEN
                  EnsureCorrectLineEndings;
              END;
            Alt: {F12}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
      END; {CASE}

      IF NOT HelpRequired AND (HelpMsg = '') AND (KeyToTest <> vk_Shift) AND (KeyToTest <> vk_Control) AND (KeyToTest <> vk_Menu {Alt}) THEN
        Debug('Please do not press that key again.');
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG KeyPressedDown: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { KeyPressedDownMainProcedure }

//    VK_NUMPAD0:
//      Debug('VK_NUMPAD0: 0 key (numeric keypad)');
//    VK_NUMPAD1:
//      Debug('VK_NUMPAD1: 1 key (numeric keypad)');
//    VK_NUMPAD2:
//      Debug('VK_NUMPAD2: 2 key (numeric keypad)');
//    VK_NUMPAD3:
//      Debug('VK_NUMPAD3: 3 key (numeric keypad)');
//    VK_NUMPAD4:
//      Debug('VK_NUMPAD4: 4 key (numeric keypad)');
//    VK_NUMPAD5:
//      Debug('VK_NUMPAD5: 5 key (numeric keypad)');
//    VK_NUMPAD6:
//      Debug('VK_NUMPAD6: 6 key (numeric keypad)');
//    VK_NUMPAD7:
//      Debug('VK_NUMPAD7: 7 key (numeric keypad)');
//    VK_NUMPAD8:
//      Debug('VK_NUMPAD8: 8 key (numeric keypad)');
//    VK_NUMPAD9:
//      Debug('VK_NUMPAD9: 9 key (numeric keypad)');
//
//    VK_MULTIPLY:
//      Debug('VK_MULTIPLY: Multiply key (numeric keypad)');
//    VK_ADD:
//      Debug('VK_ADD: Add key (numeric keypad)');
//    VK_SEPARATOR:
//      Debug('VK_SEPARATOR: Separator key (numeric keypad)');
//    VK_SUBTRACT:
//      Debug('VK_SUBTRACT: Subtract key (numeric keypad)');
//    VK_DECIMAL:
//      Debug('VK_DECIMAL: Decimal key (numeric keypad)');
//    VK_DIVIDE:
//      Debug('VK_DIVIDE: Divide key (numeric keypad)');

//  AppendToStringArray(KeyDescriptionArray, '{R}');
//  AppendToStringArray(KeyDescriptionArray, '{R}<b><u>Mouse Keys</b></u>');
//  AppendToStringArray(KeyDescriptionArray, '{R}<b>Left Mouse on post with or without Shift, and route not set</b>:'
//                                           + ' start setting up a route (signal post then flashes)');
//  AppendToStringArray(KeyDescriptionArray, '{R}<b>Left Mouse on post without Shift, and route set</b>: '
//                                           + ' cancel route');
//  AppendToStringArray(KeyDescriptionArray, '{R}<b>Left Mouse on post with Shift, and route set</b>:'
//                                           + ' cancel route but do not reset signals');
//
//  AppendToStringArray(KeyDescriptionArray, '{R}<b>Right Mouse on post with or without Shift, and route not set</b>:'

PROCEDURE KeyPressedDown{1}(KeyToTest : Word; InputShiftState : TShiftState); Overload;
{ Called when a key is pressed }
CONST
  HelpRequired = True;

VAR
  HelpMsg : String;

BEGIN
  KeyPressedDownMainProcedure(KeyToTest, InputShiftState, NOT HelpRequired, HelpMsg);
END; { KeyPressedDown-1 }

PROCEDURE KeyPressedDown{2}(KeyToTest : Word; InputShiftState : TShiftState; HelpRequired : Boolean; OUT HelpMsg : String); Overload;
{ Called when a key is pressed or help on keys is required }
BEGIN
  KeyPressedDownMainProcedure(KeyToTest, InputShiftState, HelpRequired, HelpMsg);
END; { KeyPressedDown-2 }

END { Input }.
