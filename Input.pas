UNIT Input;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Mask, ComCtrls, ExtCtrls, Buttons, Types, System.UITypes;

TYPE
  TInputDialogueBox = CLASS(TForm)
    InputDialogueCancelButton: TButton;
    InputDialogueChangeOrSelectButton: TButton;
    InputDialogueMaskEdit: TMaskEdit;
    InputDialogueMaskEditLabel: TLabel;
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
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

FUNCTION DescribeKey(Key : Word; ShiftState : TShiftState) : String;
{ Return a description of the key that's been pressed }

PROCEDURE KeyPressedDown{1}(KeyToTest : Word; InputShiftState : TShiftState); Overload;
{ Called when a key is pressed }

PROCEDURE KeyPressedDown{2}(KeyToTest : Word; InputShiftState : TShiftState; HelpRequired : Boolean; OUT HelpMsg : String); Overload;
{ Called when a key is pressed or help on keys is required }

PROCEDURE SetUpStationMonitors(ShiftState : TShiftState; DisplayOrderNum : Integer);
{ Chooses which station we're going to or from and how to show the results }

TYPE
  InputDialogueBoxType = (SignalDialogueBox, PointDialogueBox, TrackCircuitDialogueBox, LineDialogueBox, NoDialogueBox);

VAR
  InputDialogueBox: TInputDialogueBox;
  InputDialogueBoxRequired : InputDialogueBoxType;
  InputDialogueShiftState : TShiftState = [];
  OldScrollbarXPos : Integer;
  OldScrollbarYPos : Integer;
  PreviouslyDisplayedInputDialogueBox : InputDialogueBoxType;

IMPLEMENTATION

{$R *.dfm}

USES InitVars, Locks, RailDraw, MiscUtils, Cuneo, LocoUtils, Lenz, MaskUtils, Startup, Diagrams, GetTime, CreateRoute, Feedback, IDGlobal, RDC, Route, StrUtils, Menus,
     DateUtils, TestUnit, StationMonitors, LocoDialogue, Help, LocationData, Replay, Options, Edit, WorkingTimetable, TCPIP, Logging;

CONST
  UnitRef = 'Input';

VAR
  IncludeLocationOccupationStateFlag : Boolean = False;
  InputDialogueCharValid : Boolean = False;
  InputDialogueLine : Integer = UnknownLine;
  InputDialogueLineFound : Boolean = False;
  InputDialoguePoint : Integer = UnknownPoint;
  InputDialoguePointFound : Boolean = False;
  InputDialogueSignal : Integer = UnknownSignal;
  InputDialogueSignalFound : Boolean = False;
  InputDialogueTC : Integer = 0;
  InputDialogueTCFound : Boolean = False;
  LastKeyPressed : Integer;
  LineRectangleDrawnNum : Integer = UnknownLine;
  PointRectangleDrawnNum : Integer = UnknownPoint;
  SaveBackgroundColourForPrinting : TColor;
  SaveDebugWindowDebugRichEditColour : TColor = clBlack;
  SaveForegroundColourForPrinting : TColor;
  SaveTCSpeedRestrictionColour : TColour;
  SignalRectangleDrawnNum : Integer = UnknownSignal;
  TrackCircuitDrawnNum : Integer = UnknownTC;
  ZoomLevel : Integer = 0;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE TInputDialogueBox.InputDialogueCancelButtonClick(Sender: TObject);
BEGIN
  InputDialogueBox.Hide;
END; { InputDialogueCancelButtonClick }

PROCEDURE TInputDialogueBox.InputDialogueMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
{ If the mouse moves into the input dialogue window, move the focus there }
BEGIN
  IF NOT KeyboardAndMouseLocked
  AND (InputDialogueBox <> NIL)
  THEN BEGIN
    IF NOT InputDialogueBox.Active THEN
      InputDialogueBox.SetFocus;
  END;
END; { InputDialogueWindowMouseMove }

PROCEDURE TInputDialogueBox.InputDialogueMaskEditChange(Sender: TObject);
CONST
  SetUp = True;
  StartButton = True;
  Surround = True;
  UndrawRequired = True;
  UndrawToBeAutomatic = True;

VAR
  L : Integer;

BEGIN
  IF NOT InputDialogueCharValid THEN BEGIN
    MakeSound(1);
    Exit;
  END;

 CASE InputDialogueBoxRequired OF
    PointDialogueBox:
      BEGIN
        InputDialogueChangeOrSelectButton.Enabled := False;
        IF PointRectangleDrawnNum <> UnknownPoint THEN BEGIN
          { we've been here before - need to undraw the rectangle }
          WITH Points[PointRectangleDrawnNum] DO
            DrawOutline(Point_MouseRect, BackgroundColour, UndrawRequired, NOT UndrawToBeAutomatic);
          PointRectangleDrawnNum := UnknownPoint;
        END;

        IF InputDialogueMaskEdit.Text <> '' THEN BEGIN
          InputDialoguePoint := 0;
          InputDialoguePointFound := False;
          WHILE (InputDialoguePoint <= High(Points))
          AND NOT InputDialoguePointFound
          DO BEGIN
            IF InputDialoguePoint <> StrToInt(InputDialogueMaskEdit.Text) THEN BEGIN
              InputDialogueChangeOrSelectButton.Enabled := False;
              Inc(InputDialoguePoint);
            END ELSE BEGIN
              { found a point }
              InputDialoguePointFound := True;
              InputDialogueChangeOrSelectButton.Enabled := True;
              { draw a rectangle around it }
              WITH Points[InputDialoguePoint] DO
                DrawOutline(Point_MouseRect, BackgroundColour, UndrawRequired, NOT UndrawToBeAutomatic);
              PointRectangleDrawnNum := InputDialoguePoint;
            END;
          END; {WHILE}
        END;
      END;
    LineDialogueBox:
      BEGIN
        InputDialogueChangeOrSelectButton.Enabled := False;
        IF LineRectangleDrawnNum <> UnknownLine THEN BEGIN
          { we've been here before - need to undraw the rectangle }
          WITH Lines[LineRectangleDrawnNum] DO
            DrawOutline(Line_MousePolygon, BackgroundColour, UndrawRequired, NOT UndrawToBeAutomatic);
          LineRectangleDrawnNum := UnknownLine;
        END;

        IF InputDialogueMaskEdit.Text <> '' THEN BEGIN
          InputDialogueLine := 0;
          InputDialogueLineFound := False;
          WHILE (InputDialogueLine <= High(Lines))
          AND NOT InputDialogueLineFound
          DO BEGIN
            IF InputDialogueLine <> StrToLine(InputDialogueMaskEdit.Text) THEN BEGIN
              InputDialogueChangeOrSelectButton.Enabled := False;
              Inc(InputDialogueLine);
            END ELSE BEGIN
              { found a Line }
              InputDialogueLineFound := True;
              InputDialogueChangeOrSelectButton.Enabled := True;
              { draw a rectangle around it }
              WITH Lines[InputDialogueLine] DO
                DrawOutline(Line_MousePolygon, BackgroundColour, UndrawRequired, NOT UndrawToBeAutomatic);
              LineRectangleDrawnNum := InputDialogueLine;
            END;
          END; {WHILE}
        END;
      END;
    SignalDialogueBox:
      BEGIN
        InputDialogueChangeOrSelectButton.Enabled := False;
        IF SignalRectangleDrawnNum <> UnknownSignal THEN BEGIN
          { we've been here before - need to undraw the rectangle }
          WITH Signals[InputDialogueSignal] DO
            DrawOutline(Signal_MouseRect, BackgroundColour, UndrawRequired, NOT UndrawToBeAutomatic);
          SignalRectangleDrawnNum := UnknownSignal;
        END;

        IF InputDialogueMaskEdit.Text <> '' THEN BEGIN
          InputDialogueSignal := 0;
          InputDialogueSignalFound := False;
          WHILE (InputDialogueSignal <= High(Signals))
          AND NOT InputDialogueSignalFound
          DO BEGIN
            IF InputDialogueSignal <> StrToInt(InputDialogueMaskEdit.Text) THEN BEGIN
              InputDialogueChangeOrSelectButton.Enabled := False;
              Inc(InputDialogueSignal);
            END ELSE BEGIN
              { found a signal }
              InputDialogueSignalFound := True;
              InputDialogueChangeOrSelectButton.Enabled := True;
              { Draw a rectangle around it }
              WITH Signals[InputDialogueSignal] DO
                DrawOutline(Signal_MouseRect, BackgroundColour, UndrawRequired, NOT UndrawToBeAutomatic);
              SignalRectangleDrawnNum := InputDialogueSignal;
            END;
          END; {WHILE}
        END;
      END;
    TrackCircuitDialogueBox:
      BEGIN
        InputDialogueChangeOrSelectButton.Enabled := False;
        IF TrackCircuitDrawnNum <> UnknownTC THEN BEGIN
          { we've been here before - need to undraw either the trackcircuits or the rectangle }
          IF ShowAdjacentTrackCircuitMode THEN
            DrawTrackCircuitsWithAdjoiningTrackCircuits(InputDialogueTC, ForegroundColour, ForegroundColour)
          ELSE BEGIN
            FOR L := 0 TO High(Lines) DO BEGIN
              WITH Lines[L] DO
                IF Line_TC = InputDialogueTC THEN
                  DrawOutline(Line_MousePolygon, BackgroundColour, UndrawRequired, NOT UndrawToBeAutomatic);
            END;
          END;
          TrackCircuitDrawnNum := UnknownTC;
        END;
        IF InputDialogueMaskEdit.Text <> '' THEN BEGIN
          InputDialogueTC := 0;
          InputDialogueTCFound := False;
          WHILE (InputDialogueTC <= High(TrackCircuits))
          AND NOT InputDialogueTCFound
          DO BEGIN
            IF InputDialogueTC <> StrToInt(InputDialogueMaskEdit.Text) THEN BEGIN
              InputDialogueChangeOrSelectButton.Enabled := False;
              Inc(InputDialogueTC);
            END ELSE BEGIN
              { found a trackcircuit }
              InputDialogueTCFound := True;

              IF ShowAdjacentTrackCircuitMode THEN
                DrawTrackCircuitsWithAdjoiningTrackCircuits(InputDialogueTC, clYellow, clRed)
              ELSE BEGIN
                InputDialogueChangeOrSelectButton.Enabled := True;
                { Draw a rectangle around it }
                FOR L := 0 TO High(Lines) DO BEGIN
                  WITH Lines[L] DO BEGIN
                    IF Lines[L].Line_TC = InputDialogueTC THEN
                      DrawOutline(Line_MousePolygon, BackgroundColour, UndrawRequired, NOT UndrawToBeAutomatic);
                  END;
                END;
              END;

              TrackCircuitDrawnNum := InputDialogueTC;
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
  UndrawRequired = True;
  UndrawToBeAutomatic = True;

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

        PullPoint(InputDialoguePoint, NoLocoChip, NoSubRoute, NoRoute, NOT ForcePoint, ByUser, ErrorMessageRequired, PointResultPending, 
                  DebugStr, OK);

        { Undraw the rectangle }
        IF PointRectangleDrawnNum <> UnknownPoint THEN
          WITH Points[PointRectangleDrawnNum] DO
            DrawOutline(Point_MouseRect, BackgroundColour, UndrawRequired, NOT UndrawToBeAutomatic);
        PointRectangleDrawnNum := UnknownPoint;
      END;
    SignalDialogueBox:
      BEGIN
        PullSignal(NoLocoChip, InputDialogueSignal, NoIndicatorLit, NoRoute, NoSubRoute, UnknownLine, UnknownTrainType, NOT ByUser, OK);
        { and need to undraw the rectangle - this won't be needed when DrawSignal uses Invalidate }
        InvalidateScreen(UnitRef, 'InputDialogueChangeOrSelectButtonClick');
      END;
    TrackCircuitDialogueBox:
      BEGIN
        IF NOT ShowAdjacentTrackCircuitMode THEN BEGIN
          WITH TrackCircuits[InputDialogueTC] DO BEGIN
            IF TC_OccupationState = TCFeedbackOccupation THEN
              SetTrackCircuitState(InputDialogueTC, TCUnoccupied, 'set by user')
            ELSE
              IF TC_OccupationState = TCUnoccupied THEN
                SetTrackCircuitState(InputDialogueTC, TCFeedbackOccupation, 'set by user');

            FOR L := 0 TO High(Lines) DO BEGIN
              WITH Lines[L] DO BEGIN
//                IF Lines[L].Line_TC = InputDialogueTC THEN
//                  DrawOutline(Line_MouseRect, clWhite, UndrawRequired, NOT UndrawToBeAutomatic);
              END; {WITH}
            END;
          END; {WITH}
        END ELSE BEGIN

        END;

        IF GetTrackCircuitState(InputDialogueTC) = TCFeedbackOccupation THEN
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
        IF PointRectangleDrawnNum <> UnknownPoint THEN BEGIN
          InvalidateScreen(UnitRef, 'InputDialogueBoxHide 1');
          PointRectangleDrawnNum := UnknownPoint;
        END;
      END;
    LineDialogueBox:
      BEGIN
        LineDialogueBoxLeft := Left;
        LineDialogueBoxTop := Top;
        IF LineRectangleDrawnNum <> UnknownLine THEN BEGIN
          InvalidateScreen(UnitRef, 'InputDialogueBoxHide 2');
          LineRectangleDrawnNum := UnknownLine;
        END;
      END;
    SignalDialogueBox:
      BEGIN
        SignalDialogueBoxLeft := Left;
        SignalDialogueBoxTop := Top;
        IF SignalRectangleDrawnNum <> UnknownSignal THEN BEGIN
          InvalidateScreen(UnitRef, 'InputDialogueBoxHide 3');
          SignalRectangleDrawnNum := UnknownSignal;
        END;
      END;
    TrackCircuitDialogueBox:
      BEGIN
        TrackCircuitDialogueBoxLeft := Left;
        TrackCircuitDialogueBoxTop := Top;
        IF TrackCircuitDrawnNum <> UnknownTC THEN BEGIN
          InvalidateScreen(UnitRef, 'InputDialogueBoxHide 4');
          TrackCircuitDrawnNum := UnknownTC;
        END;
        IF ShowAdjacentTrackCircuitMode THEN BEGIN
          ShowAdjacentTrackCircuitMode := False;
          WriteToStatusBarPanel(StatusBarPanel2, 'Displaying adjacent trackcircuit mode = OFF');
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
  { Set up size of box contents first }
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
        ClientWidth := 157;
        ClientHeight := 71;
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
        InputDialogueMaskEdit.Width := MulDiv(FWPRailMainWindow.ClientWidth, 20, 1000);
        InputDialogueMaskEdit.SelectAll;
      END;
    LineDialogueBox:
      BEGIN
        IF PreviouslyDisplayedInputDialogueBox <> LineDialogueBox THEN BEGIN
          { only display the same number in the edit box if it's the same dialogue box }
          InputDialogueMaskEdit.Text := '';
          PreviouslyDisplayedInputDialogueBox := LineDialogueBox;
        END;
        InputDialogueBox.Caption := 'Find Line Name';
        InputDialogueMaskEditLabel.Caption := 'Line Name:';
        InputDialogueChangeOrSelectButton.Visible := False;
        InputDialogueMaskEdit.MaxLength := 6;

        { This needs to be wide enough to accommodate long line names, e.g. DFYD11 }
        InputDialogueMaskEdit.Width := MulDiv(FWPRailMainWindow.ClientWidth, 30, 1000);
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
        InputDialogueMaskEdit.Width := MulDiv(FWPRailMainWindow.ClientWidth, 20, 1000);
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
        InputDialogueMaskEditLabel.Caption := 'Trackcircuit:';
        IF InputDialogueMaskEdit.Text <> '' THEN
          InputDialogueChangeOrSelectButton.Enabled := True;
        InputDialogueChangeOrSelectButton.Visible := True;
        InputDialogueChangeOrSelectButton.Caption := 'Set TC';
        InputDialogueMaskEdit.MaxLength := 3;
        InputDialogueMaskEdit.Width := MulDiv(FWPRailMainWindow.ClientWidth, 20, 1000);
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
  T : Train;

BEGIN
  Result := False;
  I := 1;
  FoundKey := False;
  WHILE (I <= 9)
  AND NOT FoundKey
  DO BEGIN
    IF (MissingTrainArray[I] = True)
    AND (KeyToTest = I)
    THEN BEGIN
      { we've found one }
      FoundKey := True;
      T := TrainList;
      FoundTrain := False;
      WHILE (T <> NIL)
      AND NOT FoundTrain
      DO BEGIN
        WITH T^ DO BEGIN
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
        T := T^.Train_NextRecord;
      END; {WHILE}
    END;
    Inc(I);
  END; {WHILE}
END; { MissingTrainFoundByUser }

PROCEDURE DoGeneralCheck;
{ Debug check for various things including two different feedback units serving the same trackcircuit }
CONST
  IndicatorToBeSet = True;
  SuppressMessage = True;
  UpLine = True;

VAR
  ErrorCount : Integer;
  FeedbackData : FeedbackRec;
  FeedbackNum1 : Integer;
  FeedbackNumFound : Boolean;
  FeedbackType : TypeOfFeedBackType;
  I, J : Integer;
  L : Integer;
  LenzPointNumFound : Boolean;
  P : Integer;
  PointCount : Integer;
  S : Integer;
  SaveLine : Integer;
  SaveTypeOfLine : TypeOfLine;
  TC : Integer;
  TCAboveFeedbackUnit : Integer;
  TCHasFeedback : Boolean;
  TCInUse : Boolean;
  TempDraftRouteArray : StringArrayType;
  TempLinesNotAvailableStr : String;

BEGIN
  WriteToStatusBarPanel(StatusBarPanel2, 'Feedback check');
  ErrorCount := 0;
  Debug('Commencing feedback unit check - please wait...');
  FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
    FeedbackData.Feedback_Unit := I;
    ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum1);
    IF FeedbackType = FeedbackDetectorOutOfUse THEN
      Log('XG Feedback unit ' + IntToStr(FeedbackData.Feedback_Unit) + ' is marked as out of use');
  END;

  IF ErrorCount > 0 THEN
    Debug('Feedback check completed - report in LogFile with header ''FFFF'' includes ' + IntToStr(ErrorCount) + ' errors found')
  ELSE
    Log('FFFF XG Feedback check completed - no feedback units serving more than one trackcircuit');

  { Also check for line sections trackcircuit numbers unused }
  ErrorCount := 0;
  Debug('Commencing trackcircuit check - please wait...');
  FOR TC := 0 TO High(TrackCircuits) DO BEGIN
    TCInUse := False;
    TCHasFeedback := False;

    { see if the trackcircuit number is in use }
    FOR L := 0 TO High(Lines) DO BEGIN
      IF Lines[L].Line_TC = TC THEN
        TCInUse := True;
    END;

    { now see if the trackcircuit has feedback }
    FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
      FOR J := 1 TO 8 DO BEGIN
        FeedbackData.Feedback_Unit := I;
        FeedbackData.Feedback_Input := J;
        ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum1);
        IF FeedbackType = TrackCircuitFeedbackDetector THEN BEGIN
          IF FeedbackNum1 = TC THEN
            TCHasFeedback := True;
        END;
      END;
    END;

    IF NOT TCInUse
    AND NOT TCHasFeedback
    THEN BEGIN
      Inc(ErrorCount);
      Log('TTTT X TC=' + IntToStr(TC) + ' not in use (and has no feedback)');
    END ELSE
      IF NOT TCInUse THEN BEGIN
        Inc(ErrorCount);
        Log('TTTT X TC=' + IntToStr(TC) + ' not in use');
      END ELSE
        IF NOT TCHasFeedback THEN BEGIN
          Inc(ErrorCount);
          Log('TTTT X TC=' + IntToStr(TC) + ' has no feedback');
        END;
  END;
  IF ErrorCount > 0 THEN
    Debug('Trackcircuit check completed - ' + IntToStr(ErrorCount) + ' errors noted in LogFile with header ''TTTT''')
  ELSE
    Log('TTTT XG Trackcircuit check completed - all feedback inputs accounted for');

  { Also check for trackcircuit feedback inputs unused }
  ErrorCount := 0;
  Debug('Commencing trackcircuit feedback input check - please wait...');
  FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
    FOR J := 1 TO 8 DO BEGIN
      FeedbackData.Feedback_Unit := I;
      FeedbackData.Feedback_Input := J;
      ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum1);
      IF FeedbackType = TrackCircuitFeedbackDetector THEN BEGIN
        FeedbackNumFound := False;
        FOR TC := 0 TO High(TrackCircuits) DO BEGIN
          IF TC = FeedbackNum1 THEN
            FeedbackNumFound := True;
        END;
        IF NOT FeedbackNumFound THEN BEGIN
          Log('TFTF X Trackcircuit feedback unit ' + IntToStr(I) + ' input no. ' + IntToStr(J) + ' not in use ');
          Inc(ErrorCount);
        END;
      END;
    END;
  END;
  IF ErrorCount > 0 THEN
    Debug('Trackcircuit feedback check completed - ' + IntToStr(ErrorCount) + ' errors noted in LogFile with header ''TFTF''')
  ELSE BEGIN
    Log('TFTF XG Trackcircuit feedback check completed - all feedback inputs accounted for');
  END;

  { Debug point data }
  ErrorCount := 0;
  Debug('Commencing point feedback check - please wait...');
  { now see if the point has feedback }
  FOR P := 0 TO High(Points) DO BEGIN
    IF NOT Points[P].Point_HasFeedback THEN BEGIN
      Inc(ErrorCount);
      Log('PPPP Point ' + IntToStr(P) + ' has no feedback');
    END;
  END; {FOR}

  IF ErrorCount > 0 THEN
    Debug('Point check completed - ' + IntToStr(ErrorCount) + ' errors noted in LogFile with header ''PPPP''')
  ELSE BEGIN
    Debug('Point check completed - all feedback inputs accounted for');
    Log('PPPP Point check completed - all feedback inputs accounted for');
  END;

  { Also check for point feedback inputs unused }
  ErrorCount := 0;
  Debug('Commencing point feedback input check - please wait...');
  FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
    FOR J := 1 TO 8 DO BEGIN
      FeedbackData.Feedback_Unit := I;
      FeedbackData.Feedback_Input := J;
      ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum1);
      IF FeedbackType = PointFeedbackDetector THEN BEGIN
        FeedbackNumFound := False;
        FOR P := 0 TO High(Points) DO BEGIN
          IF (Points[P].Point_FeedbackUnit = I)
          AND (Points[P].Point_FeedbackInput = J)
          THEN
            FeedbackNumFound := True;
        END; {FOR}
        IF NOT FeedbackNumFound THEN BEGIN
          Log('PFPF X Point feedback unit ' + IntToStr(I) + ' input no. ' + IntToStr(J) + ' not in use ');
          Inc(ErrorCount);
        END;
      END;
    END; {FOR}
  END; {FOR}

  IF ErrorCount > 0 THEN
    Debug('Point feedback unit check completed - ' + IntToStr(ErrorCount) + ' errors noted in LogFile with header ''PFPF''')
  ELSE
    Log('PFPF XG Point feedback unit check completed - all feedback inputs in use');

  ErrorCount := 0;
  { See if the same feedback unit is attached to two different points }
  FOR I := 0 TO High(Points) DO BEGIN
    FOR J := 0 TO High(Points) DO BEGIN
      IF (I <> J)
      AND (Points[I].Point_FeedbackUnit <> 0)
      AND (Points[I].Point_FeedbackInput <> 0)
      AND (Points[I].Point_FeedbackUnit = Points[J].Point_FeedbackUnit)
      AND (Points[I].Point_FeedbackInput = Points[J].Point_FeedbackInput)
      THEN BEGIN
        Log('PFPF X Point ' + IntToStr(I) + ' and point ' + IntToStr(J) + ' have the same feedback input');
        Inc(ErrorCount);
      END;
    END; {FOR}
  END; {FOR}

  IF ErrorCount > 0 THEN
    Debug('Point feedback check completed - ' + IntToStr(ErrorCount) + ' errors noted in LogFile with header ''PFPF''')
  ELSE
    Log('PFPF X Point feedback check completed - all feedback inputs accounted for');
  { And show which points are not installed (by looking at the Lenz point numbers) }

  { Need to count down to find the last Lenz point num }
  ErrorCount := 0;
  I := 1000;
  PointCount := 0;
  WHILE (I > 0)
  AND (PointCount = 0)
  DO BEGIN
    J := 0;
    WHILE J <= High(Points) DO BEGIN
      IF Points[J].Point_LenzNum = I THEN
        PointCount := I;
      Inc(J);
    END; {WHILE}
    Dec(I);
  END; {WHILE}

  { Now record missing ones }
  FOR I := 1 TO PointCount DO BEGIN
    LenzPointNumFound := False;
    J := 0;
    WHILE J <= High(Points) DO BEGIN
      IF Points[J].Point_LenzNum = I THEN BEGIN
        LenzPointNumFound := True;
      END;
      Inc(J);
    END; {WHILE}
    IF NOT LenzPointNumFound THEN BEGIN
      Log('LPLP X No Lenz point num ' + IntToStr(I));
      Inc(ErrorCount);
    END;
  END;

  IF ErrorCount > 0 THEN
    Debug('Missing Lenz point numbers found - ' + IntToStr(ErrorCount) + ' errors noted in LogFile with header ''LPLP''')
  ELSE
    Log('LPLP XG All Lenz point numbers accounted for');

  { Show any signals which do not have resetting track circuits }
  Debug('Checking for signal resetting track circuits...');
  FOR S := 0 TO High(Signals) DO BEGIN
    IF NOT FindNextSignalOrBufferStop(S, UnknownSignal, UnknownBufferStop, NOT IndicatorToBeSet, TempLinesNotAvailableStr, TempDraftRouteArray)
    THEN BEGIN
      IF NOT Signals[S].Signal_OutOfUse THEN
        Log('S No resetting TC for S=' + IntToStr(S) + ' [' + LineToStr(Signals[S].Signal_AdjacentLine) + '] found as no next signal or bufferstop found')
      ELSE
        Log('S No resetting TC for S=' + IntToStr(S) + ' [' + LineToStr(Signals[S].Signal_AdjacentLine) + '] found as no next signal or bufferstop found but signal is '
               + 'marked as out of use');
    END ELSE BEGIN
      CreateLockingArrayFromDraftRouteArray(NoLocoChip, TempDraftRouteArray, Signals[S].Signal_RouteLockingNeededArray);
      TC := GetResettingTrackCircuit(NoLocoChip, S, SuppressMessage);
      IF TC = UnknownTC THEN
        Log('S No resetting TC for S=' + IntToStr(S) + ' [' + LineToStr(Signals[S].Signal_AdjacentLine) + '] found');
    END;
  END; {FOR}

  { See if any trackcircuits have different line characteristics, i.e. TC=126 occuping both UFYU5 (classified as a main/goods line) and FYX5 (classified as a fiddleyard).
    This causes a problem in terms of speeds for track circuits.
  }
  SaveLine := UnknownLine;
  Debug('Checking track circuit line types...');
  FOR TC := 0 TO High(TrackCircuits) DO BEGIN
    SaveTypeOfLine := UnknownTypeOfLine;
    FOR L := 0 TO High(Lines) DO BEGIN
      IF Lines[L].Line_TC = TC THEN
        IF SaveTypeOfLine = UnknownTypeOfLine THEN BEGIN
          SaveLine := L;
          SaveTypeOfLine := Lines[L].Line_TypeOfLine;
        END ELSE
          IF Lines[L].Line_TypeOfLine <> SaveTypeOfLine THEN
            Log('X TC=' + IntToStr(TC) + ' has line type of ' + TypeOfLineToStr(Lines[L].Line_TypeOfLine) + ' at ' + LineToStr(L)
                   + ' but line type of ' + TypeOfLineToStr(SaveTypeOfLine) + ' at ' + LineToStr(SaveLine))
          ELSE
            SaveTypeOfLine := Lines[L].Line_TypeOfLine;
    END; {FOR}
  END; {FOR}

  Log('XG All checks concluded');
END; { DoGeneralCheck }

PROCEDURE ResetScreenColours;
BEGIN
  BackgroundColour := SaveBackgroundColourForPrinting;
  ForegroundColour := SaveForegroundColourForPrinting;
  ScreenColoursSetForPrinting := False;
  Debug('Resetting screen colours');
END; { ResetScreenColours }

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
  WHILE (Area <= High(Areas))
  AND NOT FoundArea
  DO BEGIN
    IF Areas[Area].Area_StationMonitorsDisplayOrderNum = DisplayOrderNum THEN BEGIN
      StationMonitorsCurrentArea := Area;
      FoundArea := True;
    END;
    Inc(Area)
  END; {WHILE}

  StationMonitorsAlreadySetUp := True;
  DrawStationMonitorsWindow;
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

    IF (ssShift IN ShiftState)
    AND (ssCtrl IN ShiftState)
    AND (ssAlt IN ShiftState)
    THEN
      Result := 'Ctrl Shift Alt ' + Result
    ELSE
      IF (ssShift IN ShiftState)
      AND (ssAlt IN ShiftState)
      THEN
        Result := 'Shift Alt ' + Result
      ELSE
        IF (ssCtrl IN ShiftState)
        AND (ssAlt IN ShiftState)
        THEN
          Result := 'Ctrl Alt ' + Result
        ELSE
          IF (ssShift IN ShiftState)
          AND (ssCtrl IN ShiftState)
          THEN
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
  ActiveTrain = True;
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
  Start = True;
  StopTimer = True;
  Test = True;
  TimetableLoading = True;
  TrackCircuitOccupation = True;
  Undo = True;
  UpLine = True;
  WarnUser = True;

TYPE
  ShiftKeysType = (NoShiftKeys, Alt, Shift, Ctrl, ShiftAlt, CtrlShift, CtrlAlt, CtrlAltShift);

VAR
  Area : Integer;
  CloseAction : TCloseAction;
  DebugStr : String;
  ErrorMsg : String;
  FoundDisplayOrderNum : Boolean;
  FoundTrainInArea : Boolean;
  I, J : Integer;
//  IrrelevantShiftState : TShiftState;
  L : Integer;
  MaxDisplayOrderNum : Integer;
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
  T : Train;
  TC : Integer;
//  TempKey : Word;
  XTypeOfLine : TypeOfLine;
  WriteLocationOccupationsToFileFlag : Boolean;
  ZoomAmountStr : String;

  Hour, Min, Sec, MSec : Word;

  PROCEDURE ClearRoute(R : Integer);
  { Clear a route by brute force }
  CONST
    ErrorMessageRequired = True;

  VAR
    L : Integer;
    P : Integer;
    S : Integer;
    SubRoute : Integer;
    TC : Integer;

  BEGIN
    Log('A User clearing R=' + IntToStr(R));

    { Set any signals locked by the route to on }
    FOR S := 0 TO High(Signals) DO BEGIN
      IF SignalIsLockedBySpecificRoute(S, R) THEN BEGIN
        UnlockSignalLockedBySpecificRoute(S, R);
        IF Signals[S].Signal_Aspect <> RedAspect THEN BEGIN
          Signals[S].Signal_Aspect := RedAspect;
          Signals[S].Signal_PreviousAspect := RedAspect;
        END;
        IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN BEGIN
          Signals[S].Signal_IndicatorState := NoIndicatorLit;
          Signals[S].Signal_PreviousIndicatorState := NoIndicatorLit;
        END;
        UnlockPointsLockedBySignal(S);
      END;
    END; {FOR}

    { Unlock anything else locked by the route }
    FOR P := 0 TO High(Points) DO
      UnlockPointLockedBySpecificRoute(P, R, ErrorMessageRequired);

    FOR TC := 0 TO High(TrackCircuits) DO BEGIN
      IF TrackCircuits[TC].TC_LockedForRoute = R THEN
        UnlockTrackCircuitRouteLocking(TC);
    END; {FOR}

    { Clear the line highlighting too }
    FOR L := 0 TO High(Lines) DO BEGIN
      IF Lines[L].Line_RouteLockingForDrawing = R THEN BEGIN
        Lines[L].Line_RouteLockingForDrawing := UnknownRoute;
        Lines[L].Line_RouteSet := UnknownRoute;
      END;
    END; {FOR}

    { and also clear the line just before the start of the subroute, which isn't in the array, but would have been highlighted to make the route clearer }
    IF R <= Length(Routes_SubRouteStartSignals) THEN BEGIN
      IF R <= High(Routes_SubRouteStartSignals) THEN
        IF Routes_SubRouteStartSignals[R, 0] <> UnknownSignal THEN
          Lines[Signals[Routes_SubRouteStartSignals[R, 0]].Signal_AdjacentLine].Line_RouteSet := UnknownRoute;
    END;

    FOR SubRoute := 0 TO (Routes_TotalSubRoutes[R] - 1) DO
      Routes_SubRouteStates[R, SubRoute] := SubRouteCleared;

    Routes_RouteClearingsInProgress[R] := False;
    Routes_Cleared[R] := True;
    Log('RG R=' + IntToStr(R) + ' has been cleared by the user');

    InvalidateScreen(UnitRef, 'ClearRoute');
  END; { ClearRoute }

  { Main Loop }
BEGIN { KeyPressedDown }
  HelpMsg := '';
  TRY
    LastKeyPressed := KeyToTest;

    BEGIN
      ShiftKeys := NoShiftKeys;
      IF (ssShift IN InputShiftState)
      AND (ssCtrl IN InputShiftState)
      AND (ssAlt IN InputShiftState)
      THEN
        ShiftKeys := CtrlAltShift
      ELSE
        IF (ssShift IN InputShiftState)
        AND (ssAlt IN InputShiftState)
        THEN
          ShiftKeys := ShiftAlt
        ELSE
          IF (ssCtrl IN InputShiftState)
          AND (ssAlt IN InputShiftState)
          THEN
            ShiftKeys := CtrlAlt
          ELSE
            IF (ssShift IN InputShiftState)
            AND (ssCtrl IN InputShiftState)
            THEN
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
    IF EscKeyStored
    AND (KeyToTest <> vk_Escape)
    THEN BEGIN
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

            IF CreateRouteDisplayColoursWindow.Visible THEN
              CreateRouteDisplayColoursWindow.Visible := False;

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
            OR ShowLinesWhereUpXValueSpecified
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
              HighlightTrackCircuitSpeedRestrictions := False;
              ShowAreas := False;
              ShowLenzPointNumbers := False;
              ShowLineDetail := False;
              ShowLineDirectionDetail := False;
              ShowLineGradients := False;
              ShowLineNumbers := False;
              ShowLinesWithoutTrackCircuits := False;
              ShowLinesWhereUpXValueSpecified := False;
              ShowLocationLengthDetail := False;
              ShowLocations := False;
              ShowMouseRectangles := False;
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

              WriteToStatusBarPanel(StatusBarPanel2, '');
              InvalidateScreen(UnitRef, 'KeyPressedDown 1');

              IF KeyToTest = vK_Escape THEN
                Exit;
            END;
          END;
      END; {CASE}

      CASE KeyToTest OF
        vk_Add: { plus key }
          CASE ShiftKeys OF
            CtrlAltShift: { ctrl alt shift and the '+' sign on the numeric keypad }
              BEGIN
                HelpMsg := 'cycle station monitors displays backwards';
                IF NOT HelpRequired THEN BEGIN
                  { If we are returning to the timetable after a short gap, we probably want to return to the same one we left }
                  { note: we use the NewMinutesBetween system routine as the system one is known to be buggy }
                  IF NOT StationMonitorsWindow.Visible
                  AND (NewMinutesBetween(Time, StationMonitorsExitTime) < 1)
                  THEN
                    SetUpStationMonitors(InputShiftState, StationMonitorsCurrentDisplayOrderNum)
                  ELSE BEGIN
                    { Find a valid area number - not all areas need timetables }
                    Dec(StationMonitorsCurrentDisplayOrderNum);
                    FoundDisplayOrderNum := False;
                    Area := 0;
                    WHILE (Area <= High(Areas))
                    AND NOT FoundDisplayOrderNum
                    DO BEGIN
                      IF StationMonitorsCurrentDisplayOrderNum = Areas[Area].Area_StationMonitorsDisplayOrderNum THEN
                        FoundDisplayOrderNum := True;
                      Inc(Area);
                    END; {WHILE}

                    IF NOT FoundDisplayOrderNum THEN BEGIN
                      { we have to find the highest area number, so we can step backwards from there }
                      MaxDisplayOrderNum := 0;
                      FOR Area := 0 TO High(Areas) DO BEGIN
                        IF MaxDisplayOrderNum < Areas[Area].Area_StationMonitorsDisplayOrderNum THEN
                          MaxDisplayOrderNum := Areas[Area].Area_StationMonitorsDisplayOrderNum;
                      END;

                      StationMonitorsCurrentDisplayOrderNum := MaxDisplayOrderNum;
                    END;
                    SetUpStationMonitors(InputShiftState, StationMonitorsCurrentDisplayOrderNum);
                  END;
                  StationMonitorsWindow.visible := True;
                END;
              END;
            NoShiftKeys, Ctrl, Alt, Shift: { the '+' sign on the numeric keypad }
              BEGIN
                { cycle forwards through the various station monitors }
                HelpMsg := 'cycle timetable displays forwards';
                IF NOT HelpRequired THEN BEGIN
                  { If we are returning to the station monitors after a short gap, we probably want to return to the same one we left }
                  { note: we use the NewMinutesBetween routine as the system MinutesBetween routine is known to be buggy }
                  IF NOT StationMonitorsWindow.Visible
                  AND (NewMinutesBetween(Time, StationMonitorsExitTime) < 1)
                  THEN
                    SetUpStationMonitors(InputShiftState, StationMonitorsCurrentDisplayOrderNum)
                  ELSE BEGIN
                    { We have to find the highest area number }
                    MaxDisplayOrderNum := 0;
                    FOR Area := 0 TO High(Areas) DO BEGIN
                      IF MaxDisplayOrderNum < Areas[Area].Area_StationMonitorsDisplayOrderNum THEN
                        MaxDisplayOrderNum := Areas[Area].Area_StationMonitorsDisplayOrderNum;
                    END; {FOR}

                    FoundTrainInArea := False;
                    I := StationMonitorsCurrentDisplayOrderNum;
                    Inc(I);
                    IF I > MaxDisplayOrderNum THEN
                      I := 1;

                    WHILE (I <= MaxDisplayOrderNum)
                    AND NOT FoundTrainInArea
                    DO BEGIN
                      { Find the area referred to by the current display order number }
                      Area := 0;
                      FoundDisplayOrderNum := False;
                      WHILE (Area <= High(Areas))
                      AND NOT FoundDisplayOrderNum
                      DO BEGIN
                        IF (Areas[Area].Area_StationMonitorsDisplayOrderNum > 0)
                        AND (I = Areas[Area].Area_StationMonitorsDisplayOrderNum)
                        THEN
                          FoundDisplayOrderNum := True
                        ELSE
                          Inc(Area);
                      END; {WHILE}

                      { Are there any trains in that area? }
                      T := TrainList;
                      FoundTrainInArea := False;
                      WHILE (T <> NIL)
                      AND NOT FoundTrainInArea
                      DO BEGIN
                        WITH T^ DO BEGIN
                          IF (Train_CurrentStatus <> Cancelled)
                          AND (Train_CurrentStatus <> NonMoving)
                          THEN BEGIN
                            IF Length(Train_JourneysArray) > 0 THEN BEGIN
                              J := 0;
                              WHILE (J <= High(Train_JourneysArray))
                              AND NOT FoundTrainInArea
                              DO BEGIN
                                { Check first journey starting area, last journey arriving area, and intermediate stops }
                                IF (t^.Train_JourneysArray[0].TrainJourney_StartArea = Area)
                                OR (t^.Train_JourneysArray[High(Train_JourneysArray)].TrainJourney_EndArea = Area)
                                OR ((J > 0)
                                    AND (t^.Train_JourneysArray[J - 1].TrainJourney_StoppingOnArrival)
                                    AND (t^.Train_JourneysArray[J].TrainJourney_StartArea = Area))
                                THEN
                                  FoundTrainInArea := True;

                                Inc(J);
                              END; {WHILE}
                            END;
                          END;
                        END; {WITH}
                        T := T^.Train_NextRecord;
                      END; {WHILE}

                      IF NOT FoundTrainInArea THEN
                        StationMonitorsCurrentDisplayOrderNum := 1
                      ELSE
                        StationMonitorsCurrentDisplayOrderNum := I;

                      { If not, check the next area }
                      Inc(I);
                    END; {WHILE}

                    SetUpStationMonitors(InputShiftState, StationMonitorsCurrentDisplayOrderNum);
                  END;
                  StationMonitorsWindow.visible := True;
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
                  DrawLineInLogFile(NoLocoChip, 'X', '*', UnitRef);
                  WriteToLogFileAndTestFile := False;
                  Debug('!***');
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
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
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
              IF NOT FWPRailMainWindow.MainClockMenu.Visible THEN BEGIN
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
                        ClearRoute(R);
                    END;
                  END;
                END;
              END;
            Shift: {C}
              BEGIN
                HelpMsg := 'initiate general check';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT LogsCurrentlyKept THEN
                    Debug('Cannot initiate general check with LogsCurrentlyKept turned off')
                  ELSE BEGIN
                    WriteToStatusBarPanel(StatusBarPanel2, 'General check initiated');
                    DoGeneralCheck;
                  END;
                END;
              END;
            Ctrl: {C}
              BEGIN
                HelpMsg := 'clear a specific route';
                IF NOT HelpRequired THEN BEGIN
                  IF Length(Routes_Routes) = 0 THEN
                    Debug('!No routes to clear')
                  ELSE BEGIN
                    Str := '';
                    IF InputQuery('Route Clearing', 'Enter the number of the route to be cleared', Str) THEN BEGIN
                      IF NOT TryStrToInt(Str, R) THEN
                        ShowMessage('"' + Str + '" is not a valid number')
                      ELSE BEGIN
                        IF NOT IsElementInIntegerArray(Routes_Routes, R) THEN
                          ShowMessage('"' + Str + '" is not a valid route number')
                        ELSE
                          { clear the route by brute force }
                          ClearRoute(R);
                      END;
                    END;
                  END;
                END;
              END;
            Alt: {C}
              BEGIN
                HelpMsg := 'clear all routes';
                IF NOT HelpRequired THEN BEGIN
                  IF Length(Routes_Routes) = 0 THEN
                    Debug('!No routes to clear')
                  ELSE BEGIN
                    IF MessageDialogueWithDefault('Clear all routes?', NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes THEN BEGIN
                      DebugStr := 'Clearing all routes';
                      FOR I := 0 TO High(Routes_Routes) DO
                        ClearRoute(Routes_Routes[I]);
                    END;
                  END;
                END;
              END;
          END; {CASE}
        Ord('D'):
          CASE ShiftKeys OF
            NoShiftKeys: {D}
              BEGIN
                HelpMsg := 'set debugging mode on';
                IF NOT HelpRequired THEN BEGIN
                  Startup.DebuggingOptionsWindow.Show;
                  Startup.DebuggingOptionsWindow.SetFocus;
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
                HelpMsg := 'display diagrams window';
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
                      TurnEditModeOn(UnknownSignal, UnknownPoint);
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
                HelpMsg := 'switch feedback debugging mode on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT FeedbackDebuggingMode THEN BEGIN
                    FeedbackDebuggingMode := True;
                    Debug('Feedback debugging ON');
                    WriteDataToFeedbackWindow('Feedback ON');
                  END ELSE BEGIN
                    ReadOutTCInFull := False;
                    ReadOutTCOnce := False;
                    ReadOutDecoderNumber := False;

                    WriteDataToFeedbackWindow('Feedback OFF');
                    FeedbackDebuggingMode := False;
                    Debug('Feedback debugging OFF');
                    FeedbackWindow.FeedbackWindowTimer.Enabled := True;
                  END;
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
                HelpMsg := 'Read out the feedback once per unit on/off with adjacent signal data';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT ReadOutAdjacentSignalNumber THEN BEGIN
                    FeedbackDebuggingMode := True;
                    ReadOutTCOnce := True;
                    ReadOutDecoderNumber := False;
                    ReadOutAdjacentSignalNumber := True;

                    Debug('Read out the feedback (with adjacent signal) once ON');
                    WriteDataToFeedbackWindow('Read out the feedback (with adjacent signal) once ON)');
                  END ELSE BEGIN
                    ReadOutTCOnce := False;
                    ReadOutAdjacentSignalNumber := False;
                    FeedbackDebuggingMode := False;

                    WriteDataToFeedbackWindow('Read out the feedback (with adjacent signal) once OFF)');
                    Debug('Read out the feedback (with adjacent signal) once OFF');
                    FeedbackWindow.FeedbackWindowTimer.Enabled := True;
                  END;
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
                HelpMsg := 'Read out the feedback once per unit on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT ReadOutTCOnce THEN BEGIN
                    FeedbackDebuggingMode := True;
                    ReadOutTCOnce := True;
                    ReadOutDecoderNumber := False;
                    ReadOutAdjacentSignalNumber := False;

                    Debug('Read out the feedback once ON');
                    WriteDataToFeedbackWindow('Read out the feedback once ON)');
                  END ELSE BEGIN
                    ReadOutTCOnce := False;
                    FeedbackDebuggingMode := False;

                    WriteDataToFeedbackWindow('Read out the feedback once OFF)');
                    Debug('Read out the feedback once OFF');
                    FeedbackWindow.FeedbackWindowTimer.Enabled := True;
                  END;
                END;
              END;
           Alt: {F}
              BEGIN
                HelpMsg := 'Read out decoder unit number on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT ReadOutDecoderNumber THEN BEGIN
                    FeedbackDebuggingMode := True;
                    ReadOutTCInFull := False;
                    ReadOutTCOnce := False;
                    ReadOutDecoderNumber := True;
                    ReadOutAdjacentSignalNumber := False;

                    Debug('Read out decoder number ON');
                    WriteDataToFeedbackWindow('Read out decoder number ON');
                  END ELSE BEGIN
                    ReadOutDecoderNumber := False;
                    FeedbackDebuggingMode := False;

                    WriteDataToFeedbackWindow('Read out decoder number OFF');
                    Debug('Read out decoder number OFF');
                    FeedbackWindow.FeedbackWindowTimer.Enabled := True;
                  END;
                END;
              END;
            Ctrl: {F}
              BEGIN
                HelpMsg := 'Read out all feedback from units on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT ReadOutTCInFull THEN BEGIN
                    FeedbackDebuggingMode := True;
                    ReadOutTCInFull := True;
                    ReadOutTCOnce := False;
                    ReadOutDecoderNumber := False;
                    ReadOutAdjacentSignalNumber := False;

                    Debug('Read out all feedback ON');
                    WriteDataToFeedbackWindow('Read out all feedback ON)');
                  END ELSE BEGIN
                    ReadOutTCInFull := False;
                    FeedbackDebuggingMode := False;

                    WriteDataToFeedbackWindow('Read out all feedback OFF)');
                    Debug('Feedback with read out TC OFF');
                    FeedbackWindow.FeedbackWindowTimer.Enabled := True;
                  END;
                END;
              END;
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
                    StatusBarPanelText := 'Displaying headcodes';
                    DisplayLocoHeadcodes := True;
                    DisplayLocoChipNums := False;
                    DisplayRoutesAndJourneys := False;
                  END ELSE
                    IF DisplayLocoHeadcodes THEN BEGIN
                      StatusBarPanelText := 'Displaying journeys and routes';
                      DisplayRoutesAndJourneys := True;
                      DisplayLocoChipNums := False;
                      DisplayLocoHeadcodes := False;
                    END ELSE
                      IF DisplayRoutesAndJourneys THEN BEGIN
                        StatusBarPanelText := 'Displaying chip numbers';
                        DisplayLocoChipNums := True;
                        DisplayLocoHeadcodes := False;
                        DisplayRoutesAndJourneys := False;
                      END;
                  WriteToStatusBarPanel(StatusBarPanel2, StatusBarPanelText);
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
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
                  RedrawScreen := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                  Log('AG Screen invalidated by user');
                  RedrawScreen := False;
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
                    RedrawScreen := True;
                    GetInitialFeedback;
                    InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                    Log('AG User requested screen redraw after initial feedback sought');
                    RedrawScreen := False;
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
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
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
                  IF LogsCurrentlyKept THEN BEGIN
                    Log('AG LogsCurrentlyKept = OFF');
                    LogsCurrentlyKept := False;
                  END ELSE BEGIN
                    LogsCurrentlyKept := True;
                    Log('AG LogsCurrentlyKept = ON');
                  END;
                END;
              END;
            ShiftAlt: {L}
              BEGIN
                HelpMsg := 'turn all loco lights out';
                IF NOT HelpRequired THEN BEGIN
                  IF MessageDialogueWithDefault('Turn all loco lights off?', NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrYes THEN BEGIN
                    T := TrainList;
                    WHILE T <> NIL DO BEGIN
                      WITH T^ DO
                        IF Train_LightsType <> NoLights THEN
                          TurnLightsOff(Train_LocoChip);
                      T := T^.Train_NextRecord;
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
                  IF LockingMode THEN BEGIN
                    LockingMode := False;
                    Log('AG Locking = OFF');
                  END ELSE BEGIN
                    LockingMode := True;
                    Log('AG Locking = ON');
                  END;
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
                    T := TrainList;
                    WHILE T <> NIL DO BEGIN
                      Log(T^.Train_LocoChipStr + ' G Rebuilding location occupations');
                      SetUpAllLocationOccupationsAbInitio(NOT TimetableLoading, OK);
                      T := T^.Train_NextRecord;
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
                    LogCurrentTimeMode  := False;
                  END ELSE BEGIN
                    LogCurrentTimeMode  := True;
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
                END;
              END;
            Ctrl: {M}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
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
                HelpMsg := 'display options window';
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
            ShiftAlt: {P}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {P}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
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

                        PullPoint(P, NoLocoChip, NoRoute, NoSubRoute, NOT ForcePoint, ByUser, ErrorMessageRequired, PointResultPending, DebugStr, OK);

                        IF NOT OK THEN
                          Log('*G P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Points[P].Point_LenzNum) + ') not ok ' + PointStateToStr(Points[P].Point_RequiredState))
                        ELSE
                          IF PointResultPending THEN BEGIN
                            { PresentState <> RequiredState: a five second wait should be sufficient }
                            PointFeedbackWaitInSeconds := Round(SecondSpan(Time, Points[P].Point_FeedbackStartTime));
                            IF PointFeedbackWaitInSeconds >= PointFeedbackMaximumWaitInSeconds THEN BEGIN
                              DebugStr := DebugStr + 'P=' + IntToStr(P) + ' pending change to ' + PointStateToStr( Points[P].Point_RequiredState)
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

                        PullPoint(P, NoLocoChip, NoRoute, NoSubRoute, NOT ForcePoint, ByUser,  ErrorMessageRequired, PointResultPending, DebugStr, OK);

                        IF NOT OK THEN
                          Log('*G P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Points[P].Point_LenzNum) + ') not ok ' + PointStateToStr(Points[P].Point_RequiredState))
                        ELSE
                          IF PointResultPending THEN BEGIN
                            { PresentState <> RequiredState: a five second wait should be sufficient }
                            PointFeedbackWaitInSeconds := Round(SecondSpan(Time, Points[P].Point_FeedbackStartTime));
                            IF PointFeedbackWaitInSeconds >= PointFeedbackMaximumWaitInSeconds THEN BEGIN
                              DebugStr := DebugStr + 'P=' + IntToStr(P) + ' pending change to ' + PointStateToStr( Points[P].Point_RequiredState)
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
                HelpMsg := 'write all train records to .csv file';
                IF NOT HelpRequired THEN BEGIN
                  T := TrainList;
                  WHILE T <> NIL DO BEGIN
                    WITH T^ DO
                      WriteTrainRecord(T);
                    T := T^.Train_NextRecord;
                  END; {WHILE}
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
                HelpMsg := 'write train only records to .csv file';
                IF NOT HelpRequired THEN BEGIN
                  T := TrainList;
                  WHILE T <> NIL DO BEGIN
                    IF T^.Train_DiagramFound THEN
                      WriteTrainRecord(T);
                    T := T^.Train_NextRecord;
                  END; {WHILE}
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
                      FWPRailMainWindow.MainTimer.Enabled := False;
                      IF InAutoMode THEN
                        TurnAutoModeOff(NOT ByUser);
                      LogsCurrentlyKept := False;
                      StartWithDiagrams := False;
                      SetSystemOffline('');
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
                    ReadInLocationDataFromDatabase
                  ELSE
                    CalculateLocationPositions(750);

                  SetLength(LocationOccupations, Length(Locations));

                  SetUpLineDrawingVars(1000);

                  Log('A READ IN LINE DATA FROM DATABASE');
                  IF NOT ResizeMap THEN
                    ReadInLineDataFromDatabase
                  ELSE BEGIN
                    CalculateLinePositions(750);
                    CalculateBufferStopPositions(750);
                  END;

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

                  T := TrainList;
                  WHILE T <> NIL DO BEGIN
                    WITH T^ DO BEGIN
                      IF T^.Train_DiagramFound THEN BEGIN
                        FOR I := FirstRouteCreationHeldMsgNumber TO LastRouteCreationHeldMsgNumber DO BEGIN
                          IF Train_RouteCreationHeldMsgWrittenArray[I] THEN BEGIN
                            Train_RouteCreationHeldMsgWrittenArray[I] := False;
                            Log('X User resetting Train_RouteCreationHeldMsgWrittenArray[' + IntToStr(I) + ']');
                          END;
                        END;
                      END;
                    END; {WITH}
                    T := T^.Train_NextRecord;
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
                  StationStartMode := NOT StationStartMode;
                  IF StationStartMode THEN
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
                    T := TrainList;
                    WHILE T <> NIL DO BEGIN
                      WITH T^ DO BEGIN
                        IF Train_DiagramFound THEN BEGIN
                          IF (Train_CurrentStatus <> Missing)
                          AND (Train_CurrentStatus <> MissingAndSuspended)
                          THEN
                            ChangeTrainStatus(T, Suspended);
                        END;
                      END; {WITH}
                      T := T^.Train_NextRecord;
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
                HelpMsg := 'set trackcircuits on/off';
                IF NOT HelpRequired THEN BEGIN
                  InputDialogueBoxRequired := TrackCircuitDialogueBox;
                  InputDialogueBox.Show;
                END;
              END;
            ShiftAlt: {T}
              BEGIN
                HelpMsg := 'turn main timer off/on';
                IF NOT HelpRequired THEN BEGIN
                  IF FWPRailMainWindow.MainTimer.Enabled THEN BEGIN
                    FWPRailMainWindow.MainTimer.Enabled := False;
                    Log('XG Main Timer tuned off by user');
                  END ELSE BEGIN
                    FWPRailMainWindow.MainTimer.Enabled := True;
                    Log('XG Main Timer tuned on by user');
                  END;
                END;
              END;
            CtrlAlt: {T}
              BEGIN
                HelpMsg := 'switch flashing trackcircuits on/off';
                IF NOT HelpRequired THEN BEGIN
                  IF DisplayFlashingTrackCircuits THEN
                    DisplayFlashingTrackCircuits := False
                  ELSE
                    DisplayFlashingTrackCircuits := True;
                END;
              END;
            CtrlShift: {T}
              BEGIN
                HelpMsg := 'toggle loco chip display in station monitors display';
                IF NOT HelpRequired THEN BEGIN
                  IF IncludeLocoChipInStationMonitors THEN BEGIN
                    IncludeLocoChipInStationMonitors := False;
                    Log('AG IncludeLocoChipInStationMonitors = Off');
                  END ELSE BEGIN
                    IncludeLocoChipInStationMonitors := True;
                    Log('AG IncludeLocoChipInStationMonitors = ON');
                  END;
                END;
              END;
            Shift: {T}
              BEGIN
                HelpMsg := 'toggle display of not for public use trains in station monitors';
                IF NOT HelpRequired THEN BEGIN
                  IF DisplayNotForPublicUseTrainsInStationMonitors THEN BEGIN
                    DisplayNotForPublicUseTrainsInStationMonitors := False;
                    Log('AG DisplayNotForPublicUseTrainsInStationMonitors = OFF');
                    DrawStationMonitorsWindow;
                  END ELSE BEGIN
                    DisplayNotForPublicUseTrainsInStationMonitors := True;
                    Log('AG DisplayNotForPublicUseTrainsInStationMonitors = ON');
                    DrawStationMonitorsWindow;
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
                    Log('AG DisplayNotForPublicUseTrainsInStationMonitors = OFF');
                    DrawStationMonitorsWindow;
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
//                newreadout('100');
//                newreadout('103');
//                newreadout('180');
//                newreadout('902');
//                newreadout('912');
//                readout('922');
//                readout('fred');
//                readout('ShortCircuit');
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
                IF NOT HelpRequired THEN BEGIN
                  StopOperations;
                  Log('A Mouse wheel pressed: all operations stopped');
                  Debug('All operations stopped');
                  IF NOT SystemOnline THEN
                    Debug('Cannot deselect points - system offline')
                  ELSE BEGIN
                    FOR P := 0 TO High(Points) DO
                      EmergencyDeselectPoint(P, OK);
                    Debug('All points now switched off');
                    Log('P User has switched all points off');

                    FOR S := 0 TO High(Signals) DO
                      EmergencyDeselectSignal(S, OK);
                    Debug('All signal now switched off');
                    Log('S User has switched all signals off');
                  END;
                END;
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
                HelpMsg := 'display working timetable';
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
                HelpMsg := 'Change Working Timetable Window width';
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
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
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
                    SetCaption(FWPRailMainWindow, '');
                  END ELSE BEGIN
                    Zooming := True;
                    SetCaption(FWPRailMainWindow, 'Zoom level ' + ZoomAmountStr);
                  END;

                  ReinitialiseMainWindowVariables := True;
                  ShowStatusBarAndUpDownIndications;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
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
                  DrawLineInLogFile(NoLocoChip, 'X', '*', UnitRef);
                  WriteToLogFileAndTestFile := False;
                  Debug('!***');
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
                HelpMsg := 'Set System Online';
                IF NOT HelpRequired THEN BEGIN
                  SetSystemOnline(OK);
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
                HelpMsg := 'reset';
                IF NOT HelpRequired THEN BEGIN
                  IF SystemOnline THEN BEGIN
                    SetSystemOffline('');
                    { put offline indication on screen }

                    { and mark the state of points as not known, as we might change them on screen without changing them on the ground }
                    FOR I := 0 TO High(Points) DO
                      Points[I].Point_PresentState := PointStateUnknown;
                    Log('AG System offline');
                  END ELSE BEGIN
                    TurnAutoModeOff(NOT ByUser);

//                    IF LenzWindow.CommsSetUpOK THEN BEGIN
//                      Log('AG Comms restarted');
//                    END ELSE
//                      { still some problem }
//                      SetSystemOffline('');
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
                  { If the screen is zoomed, then return it to normal }
                  IF ZoomScaleFactor <> 1000 THEN BEGIN
                    ZoomScaleFactor := 1000;
                    Zooming := False;
                    SetCaption(FWPRailMainWindow, '');
                    WindowPenWidth := 1;

                    ReinitialiseMainWindowVariables := True;
                    InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                  END ELSE BEGIN
                    { Close the program after two consecutive escapes }
                    IF EscKeyStored THEN BEGIN
                      IF ScreenColoursSetForPrinting THEN
                        ResetScreenColours;
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
                    IF ScreenColoursSetForPrinting THEN
                      ResetScreenColours;
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
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            ShiftAlt: {Left}
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
                HelpMsg := 'stop all operations';
                IF NOT HelpRequired THEN BEGIN
                  StopOperations;
                  Log('A ' + DescribeKey(KeyToTest, InputShiftState) + ' : all operations stopped');
                  Debug('All operations stopped');
                  IF NOT SystemOnline THEN
                    Debug('Cannot deselect points - system offline')
                  ELSE BEGIN
                    FOR P := 0 TO High(Points) DO
                      EmergencyDeselectPoint(P, OK);
                    Log('P! User has switched all points off');

                    FOR S := 0 TO High(Signals) DO
                      EmergencyDeselectSignal(S, OK);
                    Debug('All signal now switched off');
                    Log('S User has switched all signals off');
                  END;
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
                HelpMsg := 'resume all operations';
                IF NOT HelpRequired THEN BEGIN
                  IF MessageDialogueWithDefault('Resume operations?', NOT StopTimer, mtConfirmation, [mbOK, mbAbort], mbAbort) = mrOK THEN BEGIN
                    ResumeOperations(OK);
                    IF OK THEN BEGIN
                      Log('AG Operations resumed');
                    END ELSE BEGIN
                      Log('A! Operations not resumed');
                    END;
                    InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                  END;
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
                HelpMsg := 'display key descriptions';
                IF NOT HelpRequired THEN
                  WriteHelpText(HelpMsg);
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
                HelpMsg := 'display signal and bufferstop numbers';
                IF NOT HelpRequired THEN BEGIN
                  ShowSignalAndBufferStopNums := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying signal and bufferstop numbers');
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
                HelpMsg := 'display signals where there are hold markers and those not used for routeing';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying signals where there are hold markers and those not used for routeing');
                  ShowSignalsWhereRouteingCanBeHeldAndThoseNotUsedForRouteing := True;

                  { List the colours for the forgetful }
                  CreateRouteDisplayColoursWindow.Visible := True;
                  AddRichLine(CreateRouteDisplayColoursWindow.CreateRouteDisplayColoursWindowRichedit,
                                                                       '<colour=' + ColourToStr(clLime) + '>' + ColourToStr(clLime) + '= station start hold' + '</colour>');
                  AddRichLine(CreateRouteDisplayColoursWindow.CreateRouteDisplayColoursWindowRichedit,
                                                                           '<colour=' + ColourToStr(clYellow) + '>' + ColourToStr(clYellow) + '= route hold' + '</colour>');
                  AddRichLine(CreateRouteDisplayColoursWindow.CreateRouteDisplayColoursWindowRichedit,
                                                                  '<colour=' + ColourToStr(clAqua) + '>' + ColourToStr(clAqua) + '= not in use for routeing' + '</colour>');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlShift: {F2}
              BEGIN
                HelpMsg := 'display where signals'' junction indicators point';
                IF NOT HelpRequired THEN BEGIN
                  IF ShowSignalJunctionDestinations THEN
                    ShowSignalJunctionDestinations := False
                  ELSE
                    ShowSignalJunctionDestinations := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {F2}
              BEGIN
                HelpMsg := 'display signal numbers and adjacent trackcircuits';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying signal numbers and adjacent trackcircuits');
                  ShowSignalsWithAdjacentTrackCircuits := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {F2}
              BEGIN
                HelpMsg := 'display signals with theatre destinations';
                IF NOT HelpRequired THEN BEGIN
                  ShowSignalsAndBufferStopsWithTheatreDestinations := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying theatre destinations');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Alt: {F2}
              BEGIN
                HelpMsg := 'display/hide signal hidden aspects';
                IF NOT HelpRequired THEN BEGIN
                  IF ShowSignalHiddenAspects THEN BEGIN
                    WriteToStatusBarPanel(StatusBarPanel2, 'Not displaying signal hidden aspects');
                    ShowSignalHiddenAspects := False;
                  END ELSE BEGIN
                    WriteToStatusBarPanel(StatusBarPanel2, 'Displaying signal hidden aspects');
                    ShowSignalHiddenAspects := True;
                  END;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
          END; {CASE}
        vk_F3: { Points }
          CASE ShiftKeys OF
            NoShiftKeys:
              BEGIN
                HelpMsg := 'display point numbers';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying point numbers');
                  ShowPointDetail := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            ShiftAlt: {F3}
              BEGIN
                HelpMsg := 'display Lenz point unit groups';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying Lenz points grouped by point unit');
                  ShowLenzPointUnitGroups := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAlt: {F3}
              BEGIN
                HelpMsg := 'display points straight and diverging states';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying points (straight and diverging highlighted)');
                  ShowPointsStraightAndDiverging := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlShift: {F3}
              BEGIN
                HelpMsg := 'display point default state';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying point default state');
                  ShowPointDefaultState := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {F3}
              BEGIN
                HelpMsg := 'display Lenz points numbers';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying Lenz point numbers');
                  ShowLenzPointNumbers := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {F3}
              BEGIN
                HelpMsg := 'display points that are locked';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying points that are locked');
                  ShowPointsThatAreLocked := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Alt: {F3}
              BEGIN
                HelpMsg := 'display point type';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying point type');
                  ShowPointType := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
          END; {CASE}
        vk_F4: { Lines }
          CASE ShiftKeys OF
            NoShiftKeys: {F4}
              BEGIN
                HelpMsg := 'display line detail';
                IF NOT HelpRequired THEN BEGIN
                  ShowLineDetail := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying line detail');
                  { List the line colours for the forgetful (added this bit 12/12/07 because FWP forgot which was which!) }
                  CreateRouteDisplayColoursWindow.Visible := True;
                  CreateRouteDisplayColoursWindow.CreateRouteDisplayColoursWindowRichedit.Clear;
                  FOR XTypeOfLine := FirstTypeOfLine TO LastTypeOfLine DO
                    AddRichLine(CreateRouteDisplayColoursWindow.CreateRouteDisplayColoursWindowRichedit, ('<colour=' + ColourToStr(GetIntegerColour(XTypeOfLine)) + '>'
                                                                                                                     + ColourToStrForUser(GetIntegerColour(XTypeOfLine))
                                                                                                                     + ' = ' + TypeOfLineToStr(XTypeOfLine)
                                                                                                                     + '</colour>'));
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            ShiftAlt: {F4}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            CtrlAlt: {F4}
              BEGIN
                HelpMsg := 'display lines where UpX value specified';
                IF NOT HelpRequired THEN BEGIN
                  ShowLinesWhereUpXValueSpecified := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying line numbers where UpX value is specified');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlShift: {F4}
              BEGIN
                HelpMsg := 'display line numbers';
                IF NOT HelpRequired THEN BEGIN
                  ShowLineNumbers := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying line numbers');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {F4}
              BEGIN
                HelpMsg := 'display line directions and non-through locations';
                IF NOT HelpRequired THEN BEGIN
                  ShowLineDirectionDetail := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying line directions (blue) and non-through locations (red)');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {F4}
              BEGIN
                HelpMsg := 'display line gradients';
                IF NOT HelpRequired THEN BEGIN
                  ShowLineGradients := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying line gradients (bidirectional lines show U/D)');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Alt: {F4}
              BEGIN
                HelpMsg := 'close the program';
                IF NOT HelpRequired THEN BEGIN
                  { close the program after two consecutive escapes }
                  FWPRailMainWindow.MainWindowClose(NIL, CloseAction);
                  Log('A Shutdown requested by user pressing Alt F4 {BLANKLINEBEFORE}');
                END;
              END;
          END; {CASE}
        vk_F5: { Trackcircuits }
          CASE ShiftKeys OF
            NoShiftKeys: {F5}
              BEGIN
                HelpMsg := 'display trackcircuits';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying trackcircuits');
                  ShowTrackCircuits := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            ShiftAlt: {F5}
              BEGIN
                HelpMsg := 'toggle showing trackcircuits where user must drive';
                IF NOT HelpRequired THEN BEGIN
                  IF ShowTrackCircuitsWhereUserMustDrive THEN BEGIN
                    ShowTrackCircuitsWhereUserMustDrive := False;
                    WriteToStatusBarPanel(StatusBarPanel2, 'Show trackcircuits where user must drive = OFF');
                  END ELSE BEGIN
                    ShowTrackCircuitsWhereUserMustDrive := True;
                    WriteToStatusBarPanel(StatusBarPanel2, 'Show trackcircuits where user must drive = ON');
                  END;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlAlt: {F5}
              BEGIN
                HelpMsg := 'toggle adjacent trackcircuit mode';
                IF NOT HelpRequired THEN BEGIN
                  IF ShowAdjacentTrackCircuitMode THEN BEGIN
                    ShowAdjacentTrackCircuitMode := False;
                    WriteToStatusBarPanel(StatusBarPanel2, 'Displaying adjacent trackcircuit mode = OFF');
                  END ELSE BEGIN
                    ShowAdjacentTrackCircuitMode := True;
                    WriteToStatusBarPanel(StatusBarPanel2, 'Displaying adjacent trackcircuit mode = ON');
                  END;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlShift: {F5}
              BEGIN
                HelpMsg := 'display trackcircuits routed over';
                IF NOT HelpRequired THEN BEGIN
                  ShowTrackCircuitsRoutedOver := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying trackcircuits routed over');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {F5}
              BEGIN
                HelpMsg := 'display trackcircuits lengths in inches';
                IF NOT HelpRequired THEN BEGIN
                  ShowTrackCircuitLengths := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying trackcircuit lengths in inches');
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
                HelpMsg := 'display lines without trackcircuits';
                IF NOT HelpRequired THEN BEGIN
                  ShowLinesWithoutTrackCircuits := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying lines without trackcircuits');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
          END; {CASE}
        vk_F6: { Locations }
          CASE ShiftKeys OF
            NoShiftKeys: {F6}
              BEGIN
                HelpMsg := 'display locations';
                IF NOT HelpRequired THEN BEGIN
                  ShowLocations := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying locations');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
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
                HelpMsg := 'display location lengths';
                IF NOT HelpRequired THEN BEGIN
                  ShowLocationLengthDetail := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying location lengths');
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
                HelpMsg := 'display areas';
                IF NOT HelpRequired THEN BEGIN
                  ShowAreas := True;
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying areas (Holding=Yellow; Reversing=Red; H&R=Aqua; Neither=Green)');
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
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
                  IF RDCMode THEN BEGIN
                    RDCMode := False;
                    Log('AG RDC Mode off');
                  END ELSE BEGIN
                    RDCMode := True;
                    StartRailDriver;
                    Log('AG RDC Mode on');
                  END;
                END;
              END;
            Ctrl: {F7}
              BEGIN
                HelpMsg := 'show RailDriver window';
                IF NOT HelpRequired THEN BEGIN
                  IF NOT RDCMode THEN
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
                HelpMsg := 'display trackcircuit feedback data';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying trackcircuit feedback data');
                  ShowTrackCircuitFeedbackDataInSeparateColours := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            ShiftAlt: {F8}
              BEGIN
              END;
            CtrlAlt: {F8}
              BEGIN
                HelpMsg := 'display chosen feedback units';
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
                HelpMsg := 'display feedback strings in debug window';
                IF NOT HelpRequired THEN BEGIN
                  IF DisplayFeedbackStringsInDebugWindow THEN
                    DisplayFeedbackStringsInDebugWindow := False
                  ELSE BEGIN
                    DisplayFeedbackStringsInDebugWindow := True;
                    WriteToStatusBarPanel(StatusBarPanel2, 'Displaying feedback strings in debug window');
                  END;
                END;
              END;
            Shift: {F8}
              BEGIN
                HelpMsg := 'display trackcircuit feedback data (' + ColourToStrForUser(TCFeedbackDataOutOfUseColour) + ' = out of use)';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying trackcircuit feedback data'
                                                                                               + ' (' + ColourToStrForUser(TCFeedbackDataOutOfUseColour) + ' = out of use)');
                  ShowTrackCircuitFeedbackDataInUse := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Ctrl: {F8}
              BEGIN
                HelpMsg := 'display point feedback data in separate colours';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying point feedback data');
                  ShowPointFeedbackDataInSeparateColours := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Alt: {F8}
              BEGIN
                HelpMsg := 'display point feedback data (' + ColourToStrForUser(PointFeedbackDataOutOfUseColour) + ' = out of use, '
                                                                                                       + ColourToStrForUser(PointsWithoutFeedbackColour) + ' = no feedback)';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying point feedback data'
                                                                                            + ' (' + ColourToStrForUser(PointFeedbackDataOutOfUseColour) + ' = out of use, '
                                                                                            + ColourToStrForUser(PointsWithoutFeedbackColour) + ' = no feedback)');
                  ShowPointFeedbackDataInUse := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
          END; {CASE}
        vk_F9:
          CASE ShiftKeys OF
            NoShiftKeys: {F9}
              BEGIN
                HelpMsg := 'show full journey record window';
                IF NOT HelpRequired THEN BEGIN
                  T := TrainList;
                  WHILE T <> NIL DO BEGIN
                    IF T^.Train_DiagramFound THEN
                      WriteTrainJourneysRecordToLockListWindow(T, NOT FullRecord);
                    T := T^.Train_NextRecord;
                  END; {WHILE}
                END;
              END;
            ShiftAlt: {F8}
              BEGIN
              END;
            CtrlAlt: {F9}
              BEGIN
                HelpMsg := 'write train journeys record to file';
                IF NOT HelpRequired THEN BEGIN
                  T := TrainList;
                  WHILE T <> NIL DO BEGIN
                    IF T^.Train_DiagramFound THEN
                      WriteTrainJourneysRecordToLogFile(T, FullRecord);
                    T := T^.Train_NextRecord;
                  END; {WHILE}
                END;
              END;
            CtrlShift: {F9}
              BEGIN
                HelpMsg := 'show partial journey record visibility';
                IF NOT HelpRequired THEN BEGIN
                  T := TrainList;
                  WHILE T <> NIL DO BEGIN
                    IF T^.Train_DiagramFound THEN
                      WriteTrainJourneysRecordToLockListWindow(T, FullRecord);
                    T := T^.Train_NextRecord;
                  END; {WHILE}
                END;
              END;
            Shift: {F9}
              BEGIN
                HelpMsg := 'write partial train journeys record to file';
                IF NOT HelpRequired THEN BEGIN
                  T := TrainList;
                  WHILE T <> NIL DO BEGIN
                    IF T^.Train_DiagramFound THEN
                      WriteTrainJourneysRecordToLogFile(T, NOT FullRecord);
                    T := T^.Train_NextRecord;
                  END; {WHILE}
                END;
              END;
            Ctrl: {F9}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
            Alt: {F9}
              BEGIN
                HelpMsg := 'display options window';
                IF NOT HelpRequired THEN BEGIN
                  IF OptionsWindow.Visible THEN
                    OptionsWindow.Visible := False
                  ELSE
                    OptionsWindow.Visible := True;
                END;
              END;
          END; {CASE}
        vk_F10:
          { note: F10 does not pass on the shift key state. (F10 is being intercepted by TFWPRailMainWindow.ApplicationMessage in Raildraw as otherwise it would do the same as
            Alt, i.e. activate the menu bar). This doesn't work for any shifted keys as F10 processed by TFWPRailMainWindow.ApplicationMessage deosn't seem to notice what the
            shift keys are ***.
          }
          BEGIN
            HelpMsg := 'Compare Two Databases';
            IF NOT HelpRequired THEN BEGIN
//              CompareTwoLocationDatabases('LocationData', 'mdb', 'LocationData - Saved', 'mdb');
//              CompareTwoLineDatabases('LineData', 'mdb', 'LineData - Saved', 'mdb');
              CompareTwoSignalDatabases('SignalData', 'mdb', 'SignalData - saved', 'mdb');
              CompareTwoPointDatabases('PointData', 'mdb', 'PointData - Saved', 'mdb');
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
            ShiftAlt: {F11}
              BEGIN
                HelpMsg := 'switch signals off/on';
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
                    ResetScreenColours
                  ELSE BEGIN
                    SaveBackgroundColourForPrinting := BackgroundColour;
                    SaveForegroundColourForPrinting := ForegroundColour;
                    BackgroundColour := clWhite;
                    ForegroundColour := clBlack;
                    ScreenColoursSetForPrinting := True;
                    Debug('Screen colours set for printing - press ' + DescribeKey(KeyToTest, InputShiftState) + ' to reset');
                  END;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            CtrlShift: {F11}
              BEGIN
                HelpMsg := 'display mouse rectangles';
                IF NOT HelpRequired THEN BEGIN
                  WriteToStatusBarPanel(StatusBarPanel2, 'Displaying mouse rectangles');
                  ShowMouseRectangles := True;
                  InvalidateScreen(UnitRef, 'key ''' + DescribeKey(KeyToTest, InputShiftState) + ''' in KeyPressed: ' + HelpMsg);
                END;
              END;
            Shift: {F11}
              BEGIN
                HelpMsg := 'display which equipment is locked and by whom';
                IF NOT HelpRequired THEN
                  WriteLockingDataToLockListWindow;
              END;
            Ctrl: {F11}
              BEGIN
                CASE ShiftKeys OF
                  NoShiftKeys:
                    HelpMsg := 'show occupation window';
                  Ctrl:
                    HelpMsg := 'show occupation window after rebuilding the location occupation array';
                  Shift:
                    HelpMsg := 'show occupation window and also write it to file';
                END; {CASE}

                IF NOT HelpRequired THEN BEGIN
                  Log('X Writing occupation window');
                  DrawLineInLogFile(NoLocoChip, '*', '+', UnitRef);
                  IF ShiftKeys = Ctrl THEN
                    SetUpAllLocationOccupationsAbInitio(NOT TimetableLoading, OK);

                  DrawLineInLogFile(NoLocoChip, '*', '+', UnitRef);

                  IF ShiftKeys = Shift THEN
                    WriteLocationOccupationsToFileFlag  := True
                  ELSE
                    WriteLocationOccupationsToFileFlag  := False;

                  IF LocationDataWindow.Visible THEN BEGIN
                    IF IncludeLocationOccupationStateFlag THEN BEGIN
                      IncludeLocationOccupationStateFlag := False;
                      WriteLocationOccupations(IncludeLocationOccupationStateFlag, WriteLocationOccupationsToFileFlag);
                    END ELSE BEGIN
                      IncludeLocationOccupationStateFlag := True;
                      WriteLocationOccupations(IncludeLocationOccupationStateFlag, WriteLocationOccupationsToFileFlag);
                    END;
                  END ELSE BEGIN
                    IncludeLocationOccupationStateFlag := False;
                    WriteLocationOccupations(IncludeLocationOccupationStateFlag, WriteLocationOccupationsToFileFlag);
                    LocationDataWindow.Show;
                  END;
                END;
              END;
            Alt: {F11}
              BEGIN
                HelpMsg := '';
                IF NOT HelpRequired THEN BEGIN
                END;
              END;
          END; {CASE}
        vk_F12:
          BEGIN
            HelpMsg := 'Starts the debugger if running in Delphi IDE debugger mode';
            IF NOT HelpRequired THEN BEGIN
            END;
          END;
      END; {CASE}

      IF NOT HelpRequired
      AND (HelpMsg = '')
      AND (KeyToTest <> vk_Shift)
      AND (KeyToTest <> vk_Control)
      AND (KeyToTest <> vk_Menu {Alt})
      THEN
        Debug('Please do not press that key again.');
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG KeyPressedDown: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { KeyPressedDown }

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
