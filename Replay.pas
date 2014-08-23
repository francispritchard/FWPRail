UNIT Replay;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs;

TYPE
  TReplayForm = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE AdvanceLogFileByOneLine;
{ Advance one line of the Replay file }

PROCEDURE InitialiseReplay(OUT OK : Boolean; OUT ErrorMsg : String);
{ Set the program up for replaying }

PROCEDURE ReplayKeyPressed(KeyToTest : Word; InputShiftState : TShiftState);
{ Interpret replay key presses }

PROCEDURE ReverseLogFileByOneLine;
{ Go back one line of the Replay file }

VAR
  ReplayForm: TReplayForm;

IMPLEMENTATION

USES MiscUtils, Initvars, RailDraw, Diagrams, Lenz, Options, Main;

{$R *.dfm}

CONST
  UnitRef = 'Replay';

VAR
  DisplayAllLines : Boolean = True;
  LineStr : String = '';
  MapDrawn : Boolean = False;
  ProvisionallyAddingExistingFeedbackOccupations : Boolean = False;
  ReplayCount : LongWord = 0;
  WritingToWindow : Boolean = True;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE InitialiseReplay(OUT OK : Boolean; OUT ErrorMsg : String);
{ Set the program up for replaying }
CONST
  Init = True;

VAR
  TC : Integer;

BEGIN
  AssignFile(ReplayFile, PathToLogFiles + ReplayFilename + '.' + ReplayFilenameSuffix);
  {$I-}
  Reset(ReplayFile);
  {$I+}
  IF IOError(ReplayFilename + '.' + ReplayFilenameSuffix, IOResult, ErrorMsg) THEN BEGIN
    Log('X Replayfile ' + ErrorMsg);
    OK := False;
  END ELSE BEGIN
    OK := True;
    RecordLineDrawingMode := False;

    LenzWindow.LenzOneSecondTimerTick.Enabled := False;
    LenzWindow.LenzWatchdogTimer.Enabled := False;

    { all track occupations off }
    FOR TC := 0 TO High(TrackCircuits) DO
      SetTrackCircuitState(TC, TCUnoccupied);

    { if the timetable fills the lower screen, reduce it to half size }
    IF LargeDiagramsWindowSelected THEN BEGIN
      LargeDiagramsWindowSelected := False;
      DrawDiagramsWindow;
    END;

    { Redraw the screen }
    DrawMap;
  END;
END; { InitialiseReplay }

PROCEDURE ReplayKeyPressed(KeyToTest : Word; InputShiftState : TShiftState);
{ Interpret replay key presses }
VAR
  I : Integer;
  KeyOK : Boolean;

BEGIN
  KeyOK := False;
  IF NOT MapDrawn THEN BEGIN
    DrawMap;
    MapDrawn := True;
  END;

  CASE KeyToTest OF
    Ord('S'):
      BEGIN
        KeyOK := True;
        { advance to next signal change }
        ReplaySignalChangeSearch := True;
      END;
    vk_Down:
      BEGIN
        KeyOK := True;
        IF ssShift IN InputShiftState THEN BEGIN
          { run the replay forward ten lines at a time }
          FOR I := 1 TO 10 DO
            AdvanceLogFileByOneLine;
        END ELSE
          AdvanceLogFileByOneLine;
      END;
    vk_Up:
      BEGIN
        KeyOK := True;
        IF ssShift IN InputShiftState THEN BEGIN
        { run the replay backwards ten lines at a time }
          FOR I := 10 DOWNTO 1 DO
            ReverseLogFileByOneLine;
        END ELSE
          ReverseLogFileByOneLine;
      END;
    vk_Next:
      { Page Dn key }
      BEGIN
        KeyOK := True;
        { scroll down the Replay file }
        ReplayScrollDown := True;
      END;
    vk_Prior:
      { Page Up key }
      BEGIN
        KeyOK := True;
        { scroll up the Replay file }
        ReplayScrollup := True;
      END;
    vk_Add: { numeric plus key }
      BEGIN
        KeyOK := True;
        DisplayAllLines := True;
      END;
    vk_Subtract: { numeric minus key }
      BEGIN
        KeyOK := True;
        DisplayAllLines := False;
      END;
    vk_Escape:
      BEGIN
        KeyOK := True;
        { close the program after two consecutive escapes }
        IF EscKeyStored THEN BEGIN
          { can't call ShutDownProgram as it was called before replaying began }
          IF NOT Application.Terminated THEN
            Halt(2)
        END ELSE BEGIN
          EscKeyStored := True;
          Debug('Press ESC again to exit');
        END;
      END;
    Ord('I'):
      BEGIN
        KeyOK := True;
        DrawMap;
      END;
    Ord('Z'):
      BEGIN
        KeyOK := True;
        IF ScreenMode <> FullScreenMode THEN
          ScreenMode := FullScreenMode
        ELSE
          ScreenMode := DefaultWindowedScreenMode;
        InvalidateScreen(UnitRef, '');
      END;
  END; {CASE}

  IF NOT KeyOK THEN BEGIN
    Debug('Please do not press that key again.');
    MakeSound(1);
  END;
END; { ReplayKeyPressed }

PROCEDURE InterpretReplay(ReplayLineStr : String; OUT OK : Boolean);
{ Redraw the screen based on the info in the replay file - returns True if something is actioned }
CONST
  Init = True;

VAR
  I : Integer;
  LocoChip : Integer;
  LocoChipStr : String;
  SettingTCPos : Integer;
  P : Integer;
  S : Integer;
  Str : String;
  SuccessPos : Integer;
  TC : Integer;
  TCsArray : IntegerArrayType;
  TestPos : Integer;
  TestStr : String;
  UpperCaseLineStr : String;

BEGIN
  OK := False;

  { See if we need to continue writing out the line, or whether we can speed down until we find the appropriate marker }
  UpperCaseLineStr := UpperCase(LineStr);
  IF Pos('{REPLAY WRITE}', UpperCaseLineStr) > 0 THEN BEGIN
    WritingToWindow := True;
    OK := True;
  END ELSE
    IF Pos('{REPLAY NOWRITE}', UpperCaseLineStr) > 0 THEN BEGIN
      WritingToWindow := False;
      OK := True;
    END;

  IF Pos('* * * *', LineStr) > 0 THEN
    MakeSound(1);

  { Find a locochip }
  LocoChip := UnknownLocoChip;
  IF Copy(LineStr, 12, 4) = StringOfChar(' ', 4) THEN
    LocoChipStr := ''
  ELSE BEGIN
    LocoChipStr := Copy(LineStr, 12, 4);
    IF NOT TryStrToInt(LocoChipStr, LocoChip) THEN
      LocoChipStr := '';
  END;

  { Track circuits }
  SettingTCPos := Pos('Setting TC=', ReplayLineStr);
  IF SettingTCPos > 0 THEN BEGIN
    TC := StrToInt(GetFollowingChars(LineStr, 'TC=', ' '));

    IF Pos('feedback occupation', LineStr) > 0 THEN BEGIN
      DrawTrackCircuit(TC, TCFeedbackOccupationColour, LocoChipStr);
      OK := True;
    END ELSE BEGIN
      IF Pos('loco out of place', LineStr) > 0 THEN
        DrawTrackCircuit(TC, TCLocoOutOfPlaceOccupationColour)
      ELSE
        IF Pos('missing occupation', LineStr) > 0 THEN
          DrawTrackCircuit(TC, TCMissingOccupationColour)
        ELSE
          IF Pos('out of use set by user', LineStr) > 0 THEN
            DrawTrackCircuit(TC, TCOutOfUseSetByUserColour)
          ELSE
            IF Pos('out of use as no feedback recieved', LineStr) > 0 THEN
              DrawTrackCircuit(TC, TCOutOfUseAsNoFeedbackReceivedColour)
            ELSE
              IF Pos('permanent feedback occupation', LineStr) > 0 THEN BEGIN
                TrackCircuits[TC].TC_OccupationState := TCPermanentFeedbackOccupation;
                DrawTrackCircuit(TC, TCPermanentFeedbackOccupationColour)
              END ELSE
                IF Pos('permanent occupation set by user', LineStr) > 0 THEN BEGIN
                  TrackCircuits[TC].TC_OccupationState := TCPermanentOccupationSetByUser;
                  DrawTrackCircuit(TC, TCPermanentOccupationSetByUserColour)
                END ELSE
                  IF Pos('permanent system occupation', LineStr) > 0 THEN BEGIN
                    TrackCircuits[TC].TC_OccupationState := TCPermanentSystemOccupation;
                    DrawTrackCircuit(TC, TCPermanentSystemOccupationColour)
                  END ELSE
                    IF Pos('system occupation', LineStr) > 0 THEN
                      DrawTrackCircuit(TC, TCSystemOccupationColour, ClearLineString)
                    ELSE
                      IF Pos('unoccupied', LineStr) > 0 THEN
                        DrawTrackCircuit(TC, TCUnoccupiedColour, ClearLineString);
      OK := True;
    END;
  END;
  IF Pos('occupation recorded', LineStr) > 0 THEN BEGIN
    IF Copy(LineStr, 12, 4) <> StringOfChar(' ', 4) THEN BEGIN
      LocoChipStr := Copy(LineStr, 12, 4);
      TC := StrToInt(GetFollowingChars(LineStr, 'TC=', ' '));
      DrawTrackCircuit(TC, TCFeedbackOccupationColour, LocoChipStr);
      OK := True;
    END;
  END;

  IF Pos('Provisionally adding existing feedback occupations to location occupations: [Timetable]', LineStr) > 0 THEN
    ProvisionallyAddingExistingFeedbackOccupations := True;
  IF Pos('Provisionally allocating trains to location occupations: [Timetable]', LineStr) > 0 THEN
    ProvisionallyAddingExistingFeedbackOccupations := False;

  IF ProvisionallyAddingExistingFeedbackOccupations THEN BEGIN
    { extract data from the provisional location occupation data }
    IF Pos('(ST=00:01 ET=23:59)', LineStr) > 0 THEN BEGIN
      Str := GetFollowingChars(LineStr, 'adding ', ' ');
      TCsArray := GetTrackCircuitsForLocation(StrToLocation(Str));
      FOR I := 0 TO High(TCsArray) DO
        IF TrackCircuits[TCsArray[I]].TC_OccupationState = TCPermanentFeedbackOccupation THEN
          DrawTrackCircuit(TCsArray[I], TCPermanentFeedbackOccupationColour, LocoChipStr)
        ELSE
          IF TrackCircuits[TCsArray[I]].TC_OccupationState = TCPermanentOccupationSetByUser THEN
            DrawTrackCircuit(TCsArray[I], TCPermanentOccupationSetByUserColour, LocoChipStr)
          ELSE
            DrawTrackCircuit(TCsArray[I], TCPermanentSystemOccupationColour, LocoChipStr);
    END ELSE
      IF Pos('(ST=00:01', LineStr) > 0 THEN BEGIN
        Str := GetFollowingChars(LineStr, 'adding ', ' ');
        TCsArray := GetTrackCircuitsForLocation(StrToLocation(Str));
        FOR I := 0 TO High(TCsArray) DO
          DrawTrackCircuit(TCsArray[I], TCFeedbackOccupationColour, LocoChipStr)
      END;
  END;

  { Signals }
  TestStr := 'S=';
  TestPos := Pos(TestStr, LineStr);
  IF TestPos > 0 THEN BEGIN
    IF (Pos('Indicator for S=', LineStr) > 0) AND (Pos('Theatre', LineStr) > 0) THEN BEGIN
      { using "Indicator for S=87 set to Theatre (F) by R=49 [Locks]" }
      S := StrToInt(GetFollowingChars(LineStr, 'S=', ' '));
      Str := GetFollowingChars(LineStr, 'Theatre (', ')');
      Signals[S].Signal_IndicatorState := TheatreIndicatorLit;
      Signals[S].Signal_TheatreIndicatorString := Str;
      DrawSignal(S);
      OK := True;
    END ELSE
      IF (Pos('Indicator for S=', LineStr) > 0)
      AND ((Pos('Left', LineStr) > 0) OR (Pos('Right', LineStr) > 0))
      THEN BEGIN
        { using "Indicator for S=124 set to Left by R=11 [Locks]" }
        S := StrToInt(GetFollowingChars(LineStr, 'S=', ' '));
        Str := GetFollowingChars(LineStr, 'set to ', ' ');
        IF Str = 'Left' THEN
          Signals[S].Signal_IndicatorState := LeftIndicatorLit
        ELSE
          Signals[S].Signal_IndicatorState := RightIndicatorLit;
        DrawSignal(S);
        OK := True;
      END ELSE BEGIN
        SuccessPos := Pos('successfully set to', LineStr);
        IF SuccessPos > 0 THEN BEGIN
          S := StrToInt(GetFollowingChars(LineStr, 'S=', ' '));
          { using "S=92 successfully set to single yellow [RailDraw]" }
          IF Pos('single yellow', LineStr) > 0 THEN BEGIN
            Signals[S].Signal_Aspect := SingleYellowAspect;
            DrawSignal(S);
          END ELSE
            IF Pos('double yellow', LineStr) > 0 THEN BEGIN
              Signals[S].Signal_Aspect := DoubleYellowAspect;
              DrawSignal(S);
            END ELSE
              IF Pos('green', LineStr) > 0 THEN BEGIN
                Signals[S].Signal_Aspect := GreenAspect;
                DrawSignal(S);
              END ELSE
                IF Pos('red', LineStr) > 0 THEN BEGIN
                  Signals[S].Signal_Aspect := RedAspect;
                  Signals[S].Signal_IndicatorState := NoIndicatorLit;
                  DrawSignal(S);
                END;
        OK := True;
      END;
    END;
  END;

  { Points }
  IF Pos('P=', LineStr) > 0 THEN BEGIN
    IF Pos('Successfully changed P=', LineStr) > 0 THEN BEGIN
      P := StrToInt(GetFollowingChars(LineStr, 'P=', ' '));
      IF Pos('straight', LineStr) > 0 THEN
        Points[P].Point_PresentState := Straight
      ELSE
        IF Pos('diverging', LineStr) > 0 THEN
          Points[P].Point_PresentState := Diverging;

      DrawPoint(P, PointColour);
      OK := True;
    END ELSE
      IF Pos('now locked', LineStr) > 0 THEN BEGIN
        P := StrToInt(GetFollowingChars(LineStr, 'P=', ' '));
        IF Pos('straight', LineStr) > 0 THEN
          Points[P].Point_PresentState := Straight
        ELSE
          IF Pos('diverging', LineStr) > 0 THEN
            Points[P].Point_PresentState := Diverging;

        DrawPoint(P, PointColour);
        OK := True;
      END;
  END;
END; { InterpretReplay }

PROCEDURE AdvanceLogFileByOneLine;
{ Advance one LineStr of the replay file }
VAR
  OK : Boolean;

BEGIN
  IF NOT EOF(ReplayFile) THEN BEGIN
    ReadLn(ReplayFile, LineStr);
    Inc(ReplayCount);
    IF NOT WritingToWindow THEN BEGIN
      REPEAT
        InterpretReplay(LineStr, OK);
        ReadLn(ReplayFile, LineStr);
        Inc(ReplayCount);
      UNTIL WritingToWindow OR EOF(ReplayFile);
    END;

    InterpretReplay(LineStr, OK);
    IF OK OR DisplayAllLines THEN BEGIN
      DebugWindow.DebugRichEdit.Perform(EM_SCROLLCARET, 0, 0);
      DebugWindow.DebugRichEdit.Lines.Add(LineStr);
    END ELSE BEGIN
      REPEAT
        ReadLn(ReplayFile, LineStr);
        Inc(ReplayCount);
        InterpretReplay(LineStr, OK);
      UNTIL OK OR EOF(ReplayFile);
      IF OK THEN BEGIN
        DebugWindow.DebugRichEdit.Perform(EM_SCROLLCARET, 0, 0);
        DebugWindow.DebugRichEdit.Lines.Add(LineStr);
      END;
    END;
  END;
END; { AdvanceLogFileByOneLine }

PROCEDURE ReverseLogFileByOneLine;
{ Go back one LineStr of the replay file }
VAR
  I : Integer;
  OK : Boolean;
  S : Integer;
  TC : Integer;

BEGIN
  IF ReplayCount > 0 THEN BEGIN
    Dec(ReplayCount);
//    IF NOT WritingToWindow THEN
    DebugWindow.DebugRichEdit.Lines.clear;
    { and reposition the file pointer the hard way }
    { all track occupations off }
    FOR S := 0 TO High(Signals) DO BEGIN
      Signals[S].Signal_Aspect := RedAspect;
      DrawSignal(S);
    END;
    FOR TC := 0 TO High(TrackCircuits) DO
      SetTrackCircuitState(TC, TCUnoccupied);

    Reset(ReplayFile);
    FOR I := 1 TO ReplayCount DO BEGIN
      ReadLn(ReplayFile, LineStr);
      InterpretReplay(LineStr, OK);
    END;

//    DebugWindow.DebugRichEdit.Perform(EM_SCROLLCARET, 0, 0);
  END;
END; { ReverseLogFileByOneLine }

END { Replay }.
