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

USES MiscUtils, Initvars, RailDraw, Diagrams, Lenz, Options;

{$R *.dfm}

CONST
  UnitRef = 'Replay';

VAR
  DisplayAllLines : Boolean = True;
  Line : String = '';
  MapDrawn : Boolean = False;
  ProvisionallyAddingExistingFeedbackOccupations : Boolean = False;
  ReplayCount : LongWord = 0;
  WritingToWindow : Boolean = True;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' <Unit=' + UnitRef + '>');
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

    LenzWindow.LenzOneMilliSecondTimerIntervalTimer.Enabled := False;
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

PROCEDURE InterpretReplay(ReplayLine : String; OUT OK : Boolean);
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
  UpperCaseLine : String;

BEGIN
  OK := False;

  { See if we need to continue writing out the line, or whether we can speed down until we find the appropriate marker }
  UpperCaseLine := UpperCase(Line);
  IF Pos('{REPLAY WRITE}', UpperCaseLine) > 0 THEN BEGIN
    WritingToWindow := True;
    OK := True;
  END ELSE
    IF Pos('{REPLAY NOWRITE}', UpperCaseLine) > 0 THEN BEGIN
      WritingToWindow := False;
      OK := True;
    END;

  IF Pos('* * * *', Line) > 0 THEN
    MakeSound(1);

  { Find a locochip }
  LocoChip := UnknownLocoChip;
  IF Copy(Line, 12, 4) = StringOfChar(' ', 4) THEN
    LocoChipStr := ''
  ELSE BEGIN
    LocoChipStr := Copy(Line, 12, 4);
    IF NOT TryStrToInt(LocoChipStr, LocoChip) THEN
      LocoChipStr := '';
  END;

  { Track circuits }
  SettingTCPos := Pos('Setting TC=', ReplayLine);
  IF SettingTCPos > 0 THEN BEGIN
    TC := StrToInt(GetFollowingChars(Line, 'TC=', ' '));

    IF Pos('feedback occupation', Line) > 0 THEN BEGIN
      DrawTrackCircuit(TC, TCFeedbackOccupationColour, LocoChipStr);
      OK := True;
    END ELSE BEGIN
      IF Pos('loco out of place', Line) > 0 THEN
        DrawTrackCircuit(TC, TCLocoOutOfPlaceOccupationColour)
      ELSE
        IF Pos('missing occupation', Line) > 0 THEN
          DrawTrackCircuit(TC, TCMissingOccupationColour)
        ELSE
          IF Pos('out of use set by user', Line) > 0 THEN
            DrawTrackCircuit(TC, TCOutOfUseSetByUserColour)
          ELSE
            IF Pos('out of use as no feedback recieved', Line) > 0 THEN
              DrawTrackCircuit(TC, TCOutOfUseAsNoFeedbackReceivedColour)
            ELSE
              IF Pos('permanent feedback occupation', Line) > 0 THEN BEGIN
                TrackCircuits[TC].TC_OccupationState := TCPermanentFeedbackOccupation;
                DrawTrackCircuit(TC, TCPermanentFeedbackOccupationColour)
              END ELSE
                IF Pos('permanent occupation set by user', Line) > 0 THEN BEGIN
                  TrackCircuits[TC].TC_OccupationState := TCPermanentOccupationSetByUser;
                  DrawTrackCircuit(TC, TCPermanentOccupationSetByUserColour)
                END ELSE
                  IF Pos('permanent system occupation', Line) > 0 THEN BEGIN
                    TrackCircuits[TC].TC_OccupationState := TCPermanentSystemOccupation;
                    DrawTrackCircuit(TC, TCPermanentSystemOccupationColour)
                  END ELSE
                    IF Pos('system occupation', Line) > 0 THEN
                      DrawTrackCircuit(TC, TCSystemOccupationColour, ClearLineString)
                    ELSE
                      IF Pos('unoccupied', Line) > 0 THEN
                        DrawTrackCircuit(TC, TCUnoccupiedColour, ClearLineString);
      OK := True;
    END;
  END;
  IF Pos('occupation recorded', Line) > 0 THEN BEGIN
    IF Copy(Line, 12, 4) <> StringOfChar(' ', 4) THEN BEGIN
      LocoChipStr := Copy(Line, 12, 4);
      TC := StrToInt(GetFollowingChars(Line, 'TC=', ' '));
      DrawTrackCircuit(TC, TCFeedbackOccupationColour, LocoChipStr);
      OK := True;
    END;
  END;

  IF Pos('Provisionally adding existing feedback occupations to location occupations: [Timetable]', Line) > 0 THEN
    ProvisionallyAddingExistingFeedbackOccupations := True;
  IF Pos('Provisionally allocating trains to location occupations: [Timetable]', Line) > 0 THEN
    ProvisionallyAddingExistingFeedbackOccupations := False;

  IF ProvisionallyAddingExistingFeedbackOccupations THEN BEGIN
    { extract data from the provisional location occupation data }
    IF Pos('(ST=00:01 ET=23:59)', Line) > 0 THEN BEGIN
      Str := GetFollowingChars(Line, 'adding ', ' ');
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
      IF Pos('(ST=00:01', Line) > 0 THEN BEGIN
        Str := GetFollowingChars(Line, 'adding ', ' ');
        TCsArray := GetTrackCircuitsForLocation(StrToLocation(Str));
        FOR I := 0 TO High(TCsArray) DO
          DrawTrackCircuit(TCsArray[I], TCFeedbackOccupationColour, LocoChipStr)
      END;
  END;

  { Signals }
  TestStr := 'S=';
  TestPos := Pos(TestStr, Line);
  IF TestPos > 0 THEN BEGIN
    IF (Pos('Indicator for S=', Line) > 0) AND (Pos('Theatre', Line) > 0) THEN BEGIN
      { using "Indicator for S=87 set to Theatre (F) by R=49 [Locks]" }
      S := StrToInt(GetFollowingChars(Line, 'S=', ' '));
      Str := GetFollowingChars(Line, 'Theatre (', ')');
      Signals[S].Signal_IndicatorState := TheatreIndicatorLit;
      Signals[S].Signal_TheatreIndicatorString := Str;
      DrawSignal(S);
      OK := True;
    END ELSE
      IF (Pos('Indicator for S=', Line) > 0)
      AND ((Pos('Left', Line) > 0) OR (Pos('Right', Line) > 0))
      THEN BEGIN
        { using "Indicator for S=124 set to Left by R=11 [Locks]" }
        S := StrToInt(GetFollowingChars(Line, 'S=', ' '));
        Str := GetFollowingChars(Line, 'set to ', ' ');
        IF Str = 'Left' THEN
          Signals[S].Signal_IndicatorState := LeftIndicatorLit
        ELSE
          Signals[S].Signal_IndicatorState := RightIndicatorLit;
        DrawSignal(S);
        OK := True;
      END ELSE BEGIN
        SuccessPos := Pos('successfully set to', Line);
        IF SuccessPos > 0 THEN BEGIN
          S := StrToInt(GetFollowingChars(Line, 'S=', ' '));
          { using "S=92 successfully set to single yellow [RailDraw]" }
          IF Pos('single yellow', Line) > 0 THEN BEGIN
            Signals[S].Signal_Aspect := SingleYellowAspect;
            DrawSignal(S);
          END ELSE
            IF Pos('double yellow', Line) > 0 THEN BEGIN
              Signals[S].Signal_Aspect := DoubleYellowAspect;
              DrawSignal(S);
            END ELSE
              IF Pos('green', Line) > 0 THEN BEGIN
                Signals[S].Signal_Aspect := GreenAspect;
                DrawSignal(S);
              END ELSE
                IF Pos('red', Line) > 0 THEN BEGIN
                  Signals[S].Signal_Aspect := RedAspect;
                  Signals[S].Signal_IndicatorState := NoIndicatorLit;
                  DrawSignal(S);
                END;
        OK := True;
      END;
    END;
  END;

  { Points }
  IF Pos('P=', Line) > 0 THEN BEGIN
    IF Pos('Successfully changed P=', Line) > 0 THEN BEGIN
      P := StrToInt(GetFollowingChars(Line, 'P=', ' '));
      IF Pos('straight', Line) > 0 THEN
        Points[P].Point_PresentState := Straight
      ELSE
        IF Pos('diverging', Line) > 0 THEN
          Points[P].Point_PresentState := Diverging;

      DrawPoint(P, PointColour);
      OK := True;
    END ELSE
      IF Pos('now locked', Line) > 0 THEN BEGIN
        P := StrToInt(GetFollowingChars(Line, 'P=', ' '));
        IF Pos('straight', Line) > 0 THEN
          Points[P].Point_PresentState := Straight
        ELSE
          IF Pos('diverging', Line) > 0 THEN
            Points[P].Point_PresentState := Diverging;

        DrawPoint(P, PointColour);
        OK := True;
      END;
  END;
END; { InterpretReplay }

PROCEDURE AdvanceLogFileByOneLine;
{ Advance one line of the replay file }
VAR
  OK : Boolean;

BEGIN
  IF NOT EOF(ReplayFile) THEN BEGIN
    ReadLn(ReplayFile, Line);
    Inc(ReplayCount);
    IF NOT WritingToWindow THEN BEGIN
      REPEAT
        InterpretReplay(Line, OK);
        ReadLn(ReplayFile, Line);
        Inc(ReplayCount);
      UNTIL WritingToWindow OR EOF(ReplayFile);
    END;

    InterpretReplay(Line, OK);
    IF OK OR DisplayAllLines THEN BEGIN
      DebugWindow.DebugRichEdit.Perform(EM_SCROLLCARET, 0, 0);
      DebugWindow.DebugRichEdit.Lines.Add(Line);
    END ELSE BEGIN
      REPEAT
        ReadLn(ReplayFile, Line);
        Inc(ReplayCount);
        InterpretReplay(Line, OK);
      UNTIL OK OR EOF(ReplayFile);
      IF OK THEN BEGIN
        DebugWindow.DebugRichEdit.Perform(EM_SCROLLCARET, 0, 0);
        DebugWindow.DebugRichEdit.Lines.Add(Line);
      END;
    END;
  END;
END; { AdvanceLogFileByOneLine }

PROCEDURE ReverseLogFileByOneLine;
{ Go back one line of the replay file }
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
      ReadLn(ReplayFile, Line);
      InterpretReplay(Line, OK);
    END;

//    DebugWindow.DebugRichEdit.Perform(EM_SCROLLCARET, 0, 0);
  END;
END; { ReverseLogFileByOneLine }

END { Replay }.
