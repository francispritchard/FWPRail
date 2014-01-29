UNIT TestUnit;
{ Used for testing stuff }

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, WSC32;

TYPE
  TTestUnitForm = CLASS(TForm)
    TestUnitFormFontDialogue: TFontDialog;
    Button1: TButton;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  TestUnitForm: TTestUnitForm;

PROCEDURE TestProc(OUT KeyOK : Boolean);
{ Used to call whatever routine we are testing. Set KeyOK to false if not in use. }

IMPLEMENTATION

{$R *.dfm}

USES InitVars, RailDraw, MiscUtils, Feedback, DateUtils, StrUtils, Diagrams, Lenz, StationMonitors, CreateRoute, GetTime, MMSystem,
     Registry, LocationData, Types, Locks, Options, Grids, FWPShowMessageUnit, Math {sic}, Cuneo;

CONST
  UnitRef = 'TestUnit';

VAR
  RepeatCount : integer = 0;
  SaveReadArray : ARRAY OF Byte;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' <Unit=' + UnitRef + '>');
END; { Log }

PROCEDURE InputBoxes;
{ Box test procedure

  Use the InputBox function when there is a default value that should be used when the user chooses the Cancel button (or
  presses Esc) to exit the dialog. If the application needs to know whether the user chooses OK or Cancel, use the
  InputQuery function instead

  InputBox('Caption', 'Prompt', 'DefaultText');

  InputQuery returns true if the user chooses OK, and false if the user chooses Cancel or presses the Esc key; if a default
  value should be used when the user cancels out of the dialog, use InputBox instead
  InputQuery('Caption', 'Prompt', 'StartingAndReturnedValue');
}
VAR
  Str : String;
  TC : Integer;
  AdjacentTrackCircuitUp, AdjacentTrackCircuitDown : Integer;

BEGIN
  Str := InputBox('Caption', 'Prompt', 'DefaultText');
  IF NOT TryStrToInt(Str, TC) THEN
    ShowMessage('"' + Str + '" is not a valid integer')
  ELSE BEGIN
    FindAdjoiningTrackCircuits(TC, AdjacentTrackCircuitUp, AdjacentTrackCircuitDown);
    Debug('adj tc up=' + IntToStr(AdjacentTrackCircuitUp) + ' adj tc down=' + IntToStr(AdjacentTrackCircuitDown));
  END;
END; { InputBoxes }


(* ******************************************************************** *)

{ Syntax Check stuff: }
VAR
  InputFile, OutputFile : Text;

PROCEDURE OpenFiles(InputFileName, OutputFileName : String);
VAR
  ErrorMsg : String;

BEGIN
  OpenInputFileOK(InputFile, InputFileName, ErrorMsg);
  Reset(InputFile);
  AssignFile(OutputFile, OutputFileName);
  IF FileExists(OutputFileName) THEN
    Erase(OutputFile);
  { create it so we can write to it when we exit }
  Rewrite(OutputFile);
END; { OpenFiles }

PROCEDURE CloseFiles;
BEGIN
  Close(InputFile);
  Close(OutputFile);
END; { CloseFiles }

PROCEDURE WriteStuff;
{ Test procedure that writes stuff out }
CONST
  AppendToFile = True;

VAR
  ErrorMsg : String;
  Stuff : String;
  TempFile : Text;
  TempFilename : String;

BEGIN
  TempFilename := 'c:\railtempfile.csv'; { add to .ini **** }
  OpenOutputFileOK(TempFile, TempFilename, ErrorMsg, NOT AppendToFile);
  WriteLn(TempFile, Stuff);
  CloseFile(TempFile);
END; { WriteStuff }

PROCEDURE CheckIndents;
VAR
  FirstNonSpacePos : Integer;
  Found : Boolean;
  IndentPos : Integer;
  Line : String;
  LineNum : Integer;
  TestStarted1, TestStarted2 : Boolean;

BEGIN
  IndentPos := 1;
  LineNum := 0;
  TestStarted1 := False;
  TestStarted2 := False;

  OpenFiles('c:\program files\borland\bds\3.0\projects\rail\startup.pas', 'c:\program files\borland\bds\3.0\projects\rail\Test.new.pas');

  REPEAT
    ReadLn(InputFile, Line);
    Line := UpperCase(Line);
    Inc(LineNum);

    { Start up when a Uses statement follows an Implementation statement }
    IF NOT TestStarted2 THEN BEGIN
      IF Pos('IMPLEMENTATION', Line) > 0 THEN
        TestStarted1 := True;
      IF TestStarted1 THEN
        IF Pos('USES', Line) > 0 THEN
          TestStarted2 := True;
    END;

    IF TestStarted2 THEN BEGIN
      { Find first non-space character and hence the indent }
      Found := False;
      FirstNonSpacePos := 1;
      WHILE (FirstNonSpacePos <= Length(Line))
      AND NOT Found
      DO BEGIN
        IF Line[FirstNonSpacePos] <> ' ' THEN BEGIN
          Found := True;
          IF FirstNonSpacePos <> IndentPos THEN
            Line[FirstNonSpacePos] := '*';
        END;
        Inc(FirstNonSpacePos);
      END; {WHILE}

      IF Copy(Line, Length(Line) - 5, 5) = 'BEGIN' THEN
        IndentPos := IndentPos + 2;

//      IF Copy(Line, Length(Line) - 3, 3) = 'END' THEN
//        IF IndentPos >  3 THEN
//          IndentPos := IndentPos - 2;


      Debug(IntToStr(LineNum) + ': ' + Line);
    END;
  UNTIL EOF(InputFile);
  CloseFiles;
END; { CheckIndents }

(* ******************************************************************** *)

PROCEDURE Test3;
//VAR
BEGIN
  CheckIndents;
END; { Test3 }

PROCEDURE WriteOutFeedbackToAccessDatabase;
CONST
  AppendToFile = True;

VAR
  DebugStr : String;
  ErrorMsg : String;
  FeedbackData : FeedbackRec;
  FeedbackNum : Integer;
  FeedbackType : TypeOfFeedBackType;
  I, J : Integer;
  TCAboveFeedbackUnit : Integer;
  TempFile : Text;
  TempFilename : String;

BEGIN
  Debug('F8 does nothing');
{-------------------------------------}
  TempFilename := 'c:\railtempfile.csv'; { add to .ini **** }
  OpenOutputFileOK(TempFile, TempFilename, ErrorMsg, NOT AppendToFile);
  FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
    DebugStr := IntToStr(I);
    FOR J := 1 TO 8 DO BEGIN
      FeedbackData.Feedback_Unit := I;
      FeedbackData.Feedback_Input := J;
      ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, FeedbackNum);

      CASE FeedbackType OF
        TrackCircuitFeedbackDetector:
          DebugStr := DebugStr + ','+ IntToStr(FeedbackNum);
        TRSPlungerFeedbackDetector:
          DebugStr := DebugStr + ',TRS';
        PointFeedbackDetector:
          DebugStr := DebugStr + ',P';
      END; {CASE}
    END;
    WriteLn(TempFile, DebugStr);
  END;

//    WITH FeedbackData DO
//      WriteLn(TempFile, IntToStr(I) + ',' + IntToStr(Feedback_Input)

  CloseFile(TempFile);
END; { WriteOutFeedbackToAccessDatabase }

PROCEDURE WriteOutNamesForUCUExcelSpeadsheet;
CONST
  AppendToFile = True;

VAR
  I : Integer;
  Line, NewLine : String;

BEGIN
  OpenFiles('c:\UCUSpreadsheet.in', 'c:\UCUSpreadsheet.out');
  REPEAT
    ReadLn(InputFile, Line);
    I := Pos(',', Line);
    IF I > 0 THEN
      NewLine := Copy(Line, I + 2) + ' ' + Copy(Line, 1, I - 1)
    ELSE
      NewLine := Line;

    WriteLn(outputFile, NewLine);
  UNTIL EoF(InputFile);
  CloseFiles;

  OpenFiles('c:\UCUSpreadsheet.in', 'c:\UCUSpreadsheet.out2');
  REPEAT
    ReadLn(InputFile, Line);
    I := Pos(',', Line);
    IF I > 0 THEN
      NewLine := Line
    ELSE BEGIN
      I := LastDelimiter(' ', Line);
      NewLine := Copy(Line, I + 1) + ', ' + Copy(Line, 1, I - 1);
    END;

    WriteLn(outputFile, NewLine);
  UNTIL EoF(InputFile);
  CloseFiles;
END; { WriteOutNamesForUCUExcelSpeadsheet }

PROCEDURE RegionTest;
BEGIN
if testregion then begin
  testregion := false;
  debug('tr=off');
end else begin
  testregion := true;
  debug('tr=on');
end;
  Region := CreateRectRgn(50, 50, 250, 250);
  SelectClipRgn(MainWindow.Canvas.Handle, Region);
  MainWindow.Canvas.Brush.Color := clFWPPink;
  MainWindow.Canvas.FillRect(MainWindow.Canvas.ClipRect);
  MainWindow.Canvas.MoveTo(100, 100);
  MainWindow.Canvas.LineTo(600, 600);

//  Region := CreateRectRgn(50, 50, 250, 250);
//  SelectClipRgn(MainWindow.Canvas.Handle, Region);
  MainWindow.Canvas.Pen.Color := clAqua;
//  MainWindow.Canvas.FillRect(Rect(100, 500, 100, 500));
  MainWindow.Canvas.MoveTo(50, 100);
  MainWindow.Canvas.LineTo(600, 650);
END; { RegionTest }

PROCEDURE TestSort;
{ Test procedure }
VAR
  I : Integer;
  OldArray : IntegerArrayType;
  NewArray : IntegerArrayType;

  PROCEDURE Sort(OldArray : IntegerArrayType; OUT NewArray : IntegerArrayType);
  VAR
    I, J, K : Integer;
    MaxNumStr : Integer;
    TempOldArray : IntegerArrayType;
    TempPos : Integer;

  BEGIN
    { Copy the old array across or can't alter the length of it }
    SetLength(TempOldArray, Length(OldArray));
    FOR I := 0 TO High(OldArray) DO
      TempOldArray[I] := OldArray[I];
    SetLength(NewArray, 0);

    { Now do the sort }
    FOR I := 0 TO High(TempOldArray) DO BEGIN
      TempPos := -1;
      MaxNumStr := 999;
      FOR J := 0 TO High(TempOldArray) DO BEGIN
        IF TempOldArray[J] < MaxNumStr THEN BEGIN
          MaxNumStr := TempOldArray[J];
          TempPos := J;
        END;
      END; {FOR}
      SetLength(NewArray, Length(NewArray) + 1);
      NewArray[High(NewArray)] := TempOldArray[TempPos];

      { and remove the element from the original array }
      FOR K := TempPos TO (Length(TempOldArray) - 2) DO
        TempOldArray[K] := TempOldArray[K + 1];
      SetLength(TempOldArray, Length(TempOldArray) -1);
    END; {FOR}
  END; { Sort }

BEGIN
  SetLength(OldArray, 12);
  OldArray[0] := 99;
  OldArray[1] := 55;
  OldArray[2] := 31;
  OldArray[3] := 98;
  OldArray[4] := 55;
  OldArray[5] := 21;
  OldArray[6] := 39;
  OldArray[7] := 85;
  OldArray[8] := 51;
  OldArray[9] := 69;
  OldArray[10] := 35;
  OldArray[11] := 21;

  FOR I := 0 TO High(OldArray) DO
    Debug(IntToStr(I) + ': ' + IntToStr(OldArray[I]));

  Sort(OldArray, NewArray);
  FOR I := 0 TO High(NewArray) DO
    Debug(IntToStr(I) + ': ' + IntToStr(NewArray[I]));
END; { TestSort }

PROCEDURE ExceptionTest;
{ Test procedure }
var
 I : Integer;
 A : Array[0..1] OF Integer;

BEGIN
  I := 5;
  TRY
    Debug('1:' + TimeToHMStr(0.19999));
    A[I] := 8;
    Debug('2:' + TimeToHMStr(1.19999));
  EXCEPT
    ON E : Exception DO
      Log('EG : ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ExceptionTest }

procedure TTestUnitForm.Button1Click(Sender: TObject) ;
var
   soundAlias : string;
begin
   if ListBox1.ItemIndex = -1 then
   begin
     ShowMessage('Select a sound alias from the list...') ;
     Exit;
   end;

   soundAlias := ListBox1.Items[ListBox1.ItemIndex];

//   PlaySound(PAnsiChar(soundAlias), 0, SND_ALIAS or SND_ASYNC); Doesn't work in Delphi XE 31/3/11
end;

//SystemSoundPlayerForm ... OnCreate
procedure TTestUnitForm.FormCreate(Sender: TObject) ;
var
   reg : TRegistry;
begin
   reg := TRegistry.Create;
   try
     reg.RootKey := HKEY_CURRENT_USER;
reg.OpenKeyReadOnly('\AppEvents\EventLabels') ;
     reg.GetKeyNames(ListBox1.Items) ;
   finally
     reg.Free;
   end;
end;

PROCEDURE MakeNoise;
{ Used to call whatever routine we are testing. Set KeyOK to false if not in use. }
VAR
  Sound : Cardinal;
//  SoundStr : PAnsiChar;

  PROCEDURE Wait;
  VAR
    I : Integer;

  BEGIN

    FOR I := 0 TO 1000000000 DO ;
  END; { Wait }

BEGIN
  Debug('DeviceFail');
  PlaySound('DeviceFail', 0, SND_ALIAS or SND_ASYNC);
  Wait;
//
//  Debug('CriticalBatteryAlarm');
//  PlaySound('CriticalBatteryAlarm' {"dum dum"}, 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  Debug('LowBatteryAlarm');
//  PlaySound('LowBatteryAlarm' {"dum"}, 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  Debug('SystemAsterisk');
//  PlaySound('SystemAsterisk' {"beep dum"}, 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  Debug('SystemExclamation');
//  PlaySound('SystemExclamation' {"beep beep"}, 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  Debug('SystemHand');
//  PlaySound('SystemHand' {deep "dum"}, 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  Debug('SystemStart');
//  PlaySound('SystemStart', 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  Debug('SystemExit');
//  PlaySound('SystemExit', 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  Debug('SystemDefault');
//  PlaySound('SystemDefault', 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  SoundStr := 'PrintComplete';
//  Debug(SoundStr);
//  PlaySound(SoundStr, {deep "dum"} 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  SoundStr := 'Reminder';
//  Debug(SoundStr);
//  PlaySound(SoundStr, 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  SoundStr := 'CallWaiting';
//  Debug(SoundStr);
//  PlaySound(SoundStr, 0, SND_ALIAS or SND_ASYNC);
//  Wait;
//
//  SoundStr := 'BusyTone';
//  Debug(SoundStr);
//  PlaySound(SoundStr, 0, SND_ALIAS or SND_ASYNC);
//  Wait;

  Sound := 1024;
  Debug(IntToStr(Sound));
  MessageBeep(Sound);
  Wait;

  Sound := 512;
  Debug(IntToStr(Sound));
  MessageBeep(Sound);
  Wait;

  Sound := 256;
  Debug(IntToStr(Sound));
  MessageBeep(Sound);
  Wait;

  Sound := 128;
  Debug(IntToStr(Sound));
  MessageBeep(Sound);
  Wait;

  Sound := MB_ICONASTERISK;
  Debug(IntToStr(Sound));
  MessageBeep(Sound);
  Wait;

  Sound := MB_ICONQUESTION;
  Debug(IntToStr(Sound));
  MessageBeep(Sound);
  Wait;

  Sound := MB_OK;
  Debug(IntToStr(Sound));
  MessageBeep(Sound);
  Wait;

END; { MakeNoise }

PROCEDURE GetFollowingCharsTest;
{ Test procedure }
VAR
  Line : String;
  Str : String;

  FUNCTION GetFollowingChars(Line: String; Str : String; EndStr : String) : String;
  { Return the characters after the given characters up to the delimiter supplied (or "CRLF" if it's at a line end) }
  VAR
    EndStrPos : Integer;
    TestPos : Integer;

  BEGIN
    TestPos := Pos(Str, Line);
    IF TestPos = 0 THEN
      Result := ''
    ELSE BEGIN
      EndStrPos := Pos(EndStr, Copy(Line, TestPos + Length(Str)));
      IF EndStrPos = 0 THEN
        EndStrPos := Length(Line) - 1;

      Result := Copy(Line, TestPos + Length(Str), EndStrPos - 1);
    END;
  END; { GetFollowingChars }

BEGIN
  Line := 'Setting S=111 Feed';
  Str := GetFollowingChars(Line, 'S=', ' ');
  Debug(Str);

  Line := 'Setting= 22 ';
  Str := GetFollowingChars(Line, 'Setting= ', ' ');
  Debug(Str);

  Line := 'Setting S=3 Feed';
  Str := GetFollowingChars(Line, 'S=', '');
  Debug(Str);

  Line := 'Setting S=444Feed';
  Str := GetFollowingChars(Line, 'S=', 'Feed');
  Debug(Str);

  Line := 'S=55 Feed';
  Str := GetFollowingChars(Line, 'S=', ' ');
  Debug(Str);

  Line := 'S=6';
  Str := GetFollowingChars(Line, 'S=', ']');
  Debug(Str);

  Line := 'S=7';
  Str := GetFollowingChars(Line, 'S=', ' ');
  Debug(Str);

  Line := '';
  Str := GetFollowingChars(Line, 'S=', ' ');
  Debug(Str);
END; { GetFollowingCharsTest }

//      FOR I := 0 TO High(WorkingTimetable_PossibleLocoClasses) DO BEGIN
//        { For each class select available locos }
//        T := TrainList;
//        WHILE T <> NIL DO BEGIN
//          IF WorkingTimetable_PossibleLocoClasses[I] = T^.Train_LocoClassStr THEN BEGIN
//            AppendToIntegerArray(TempArray1, T^.Train_LocoChip);
//            AppendToIntegerArray(TempArray2, T^.Train_LocoChip);
//          END;
//          T := T^.Train_NextRecord;
//        END; {WHILE}
//
//        { Make the list of locos for each supplied class random, or otherwise the same train will always be selected first }
//        REPEAT
//          TempLocoChip := RandomFrom(TempArray1);
//          { remove the element from the temporary array so we aren't given it again, or the list would never shrink }
//          IsElementInIntegerArray(TempArray1, TempLocoChip, ElementPos);
//          DeleteElementFromIntegerArray(TempArray1, ElementPos);
//          AppendToIntegerArray(AllSuitableTrainsByClass, TempLocoChip);
//        UNTIL Length(TempArray1) = 0;
//      END; {FOR}
//
//      { List suitable trains (but not in random order or it makes comparison between different log files difficult) ... }
//      DebugStr := '';
//      IF Length(TempArray2) > 0 THEN BEGIN
//        FOR I := 0 TO High(TempArray2) - 1 DO
//          DebugStr := DebugStr + LocoChipToStr(TempArray2[I]) + ', ';
//        DebugStr := DebugStr + LocoChipToStr(TempArray2[High(TempArray2)]);
//      END;
//
//      { ...and possible loco classes }
//      DebugStr := DebugStr + ' [class ';
//      FOR I := 0 TO High(WorkingTimetable_PossibleLocoClasses) - 1 DO
//        DebugStr := DebugStr + WorkingTimetable_PossibleLocoClasses[I] + ' ';
//      DebugStr := DebugStr + WorkingTimetable_PossibleLocoClasses[High(WorkingTimetable_PossibleLocoClasses)];
//      DebugStr := DebugStr + ']';
//      Log('W W=' + EntryNumStr + ':    '
//             + 'trains in the specified class(es) are: ' + DebugStr);

FUNCTION GetCommandLen(LengthByte : Byte) : Integer;
{ Find out the length of the command array from bits 3-0 of first byte; add one to include the check byte }
BEGIN
  Result := (LengthByte AND NOT $F0) + 2; { 240 - 11110000 }
END; { GetCommandLen }

PROCEDURE TestProc3;
// VAR

BEGIN


END; { TestProc3 }

PROCEDURE TestProc2;
BEGIN

END; { TestProc2 }

PROCEDURE TestProc(OUT KeyOK : Boolean);
{ Used to call whatever routine we are testing. Set KeyOK to false if not in use. }
//VAR
//  I : Integer;

BEGIN
  KeyOK := True;
  TestProc2;
END; { TestProc }

INITIALIZATION
  SetLength(SaveReadArray, 0);
  TestProc3;

FINALIZATION


END { TestUnit }.
