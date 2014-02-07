UNIT Help;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, Types;

TYPE
  THelpWindow = CLASS(TForm)
    HelpRichEdit: TRichEdit;
    HelpWindowFindDialog: TFindDialog;
    HelpStatusBar: TStatusBar;
    PROCEDURE HelpRichEditKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE HelpRichEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    PROCEDURE HelpMemoKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE HelpWindowFindDialogClose(Sender: TObject);
    PROCEDURE HelpWindowFindDialogFind(Sender: TObject);
    PROCEDURE HelpWindowFindDialogShow(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE FindDialogFind(FindDialog : TFindDialog; Text : String; OUT FoundPos : Integer);
{ Find a given serach string - designed to be use on more than just this window }

PROCEDURE WriteHelpText(HelpMsg : String);
{ write the text to the help window }

VAR
  HelpWindow: THelpWindow;
  HelpWindowFindDialogActive : Boolean = False;

IMPLEMENTATION

{$R *.dfm}

USES MiscUtils, Input, Options, Lenz, InitVars, Cuneo;

CONST
  UnitRef = 'Help';

VAR
  PreviousFoundPos : Integer = 0;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' <Unit=' + UnitRef + '>');
END; { Log }

PROCEDURE THelpWindow.HelpMemoKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      HelpWindow.Hide;
  END; {CASE}
END; { HelpMemoKeyDown }

PROCEDURE SearchHelpText(S : String);
{ Search the Help Window text }
BEGIN
  PreviousFoundPos := 0;
  WITH HelpWindow DO BEGIN
    HelpWindowFindDialog.Position := Point(HelpWindow.Left + (HelpWindow.Width DIV 2), HelpWindow.Top);
    HelpWindowFindDialog.Execute;
  END; {WITH}
END; { SearchHelpText }

PROCEDURE FindDialogFind(FindDialog : TFindDialog; Text : String; OUT FoundPos : Integer);
{ Find a given search string - designed to be used on more than just this window }
VAR
  SText: String;
  StartFrom : Integer;
  TempFoundPos : Integer;

BEGIN
  WITH FindDialog DO BEGIN
    { If saved position is 0, this must be a "Find First" operation. }
    IF PreviousFoundPos = 0 THEN
      { Reset the frFindNext flag of the FindDialog }
      Options := Options - [frFindNext];

    IF frDown IN Options THEN BEGIN
      { we're seeking down the list }
      IF NOT (frFindNext IN Options) THEN BEGIN
        { is it a Find "First" ? }
        SText := Text;
        StartFrom := 1;
      END ELSE BEGIN
        { it's a Find "Next" - calculate from where to start searching: start after the end of the last found position }
        StartFrom := PreviousFoundPos + Length(Findtext);
        { get text from the RichEdit, starting from StartFrom }
        SText := Copy(Text, StartFrom, Length(Text) - StartFrom + 1);
      END;

      IF frMatchCase IN Options THEN
        { case-sensitive search? Search position of FindText in SText. Note that function Pos() is case-sensitive. }
        FoundPos := Pos(FindText, SText)
      ELSE
        { Search position of FindText, converted to uppercase, in SText, also converted to uppercase }
        FoundPos := Pos(UpperCase(FindText), UpperCase(SText));

      { If found, calculate position of FindText in the RichEdit }
      PreviousFoundPos := FoundPos + StartFrom - 1;

    END ELSE BEGIN
      { we're seeking up the list  - reverse the list }
      IF NOT (frFindNext IN Options) THEN
        { is it a Find "First" ? }
        SText := Text
      ELSE BEGIN
        { it's a Find "Next" - calculate from where to start searching: start before the end of the last found position }
        StartFrom := PreviousFoundPos;
        { get text from the RichEdit, starting from StartFrom }
        SText := Copy(Text, 0, StartFrom - 1);
      END;

      IF frMatchCase IN Options THEN BEGIN
        { case-sensitive search? Search position of FindText in SText. Note that function Pos() is case-sensitive. }
        FoundPos := 0;
        REPEAT
          TempFoundPos := Pos(FindText, SText);
          IF TempFoundPos > 0 THEN BEGIN
            FoundPos := TempFoundPos;
            { we need to find the last occurrence of the FindText - do this by converting the search string in the text to spaces unless we're searching for spaces! }
            IF FindText <> StringOfChar(' ', Length(FindText)) THEN
              SText := Copy(SText, 0, TempFoundPos - 1) + StringOfChar(' ', Length(FindText)) + Copy(SText, TempFoundPos + 3)
            ELSE
              SText := Copy(SText, 0, TempFoundPos - 1) + StringOfChar('X', Length(FindText)) + Copy(SText, TempFoundPos + 3);
          END;
        UNTIL TempFoundPos = 0;
      END ELSE BEGIN
        FoundPos := 0;
        REPEAT
          { search position of FindText, converted to uppercase, in SText, also converted to uppercase }
          TempFoundPos := Pos(UpperCase(FindText), UpperCase(SText));
          IF TempFoundPos > 0 THEN BEGIN
            FoundPos := TempFoundPos;
            { we need to find the last occurrence of the FindText - do this by converting the search string in the text to spaces unless we're searching for spaces! }
            IF FindText <> StringOfChar(' ', Length(FindText)) THEN
              SText := Copy(SText, 0, TempFoundPos - 1) + StringOfChar(' ', Length(FindText)) + Copy(SText, TempFoundPos + 3)
            ELSE
              SText := Copy(SText, 0, TempFoundPos - 1) + StringOfChar('X', Length(FindText)) + Copy(SText, TempFoundPos + 3);
          END;
        UNTIL TempFoundPos = 0;
      END;

      { If found, calculate position of FindText in the RichEdit }
      PreviousFoundPos := FoundPos;
    END;
  END; {WITH}
END; { FindDialogFind }

PROCEDURE THelpWindow.HelpWindowFindDialogFind(Sender: TObject);
VAR
  FoundPos : Integer;

BEGIN
  WITH HelpWindow DO BEGIN
    FindDialogFind(HelpWindowFindDialog, HelpRichEdit.Text, FoundPos);
    IF FoundPos > 0 THEN BEGIN
      IF NOT HelpWindow.Visible THEN
        HelpWindow.Visible := True;
      HelpRichEdit.SelStart := PreviousFoundPos - 1;
      HelpRichEdit.SelLength := Length(HelpWindowFindDialog.FindText);
      HelpRichEdit.SetFocus;
      HelpRichEdit.Perform(EM_SCROLLCARET, 0, 0);
    END ELSE
      ShowMessage('Could not find "' + HelpWindowFindDialog.FindText + '"');
  END; {WITH}
END; { HelpWindowFindDialogFind }

PROCEDURE THelpWindow.HelpRichEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
BEGIN
  Debug('x=' + InttOstr(X) + ' y=' + inttostr(Y));
END;

PROCEDURE THelpWindow.HelpWindowFindDialogShow(Sender: TObject);
BEGIN
  HelpWindowFindDialogActive := True;
END; { HelpWindowFindDialogShow }

PROCEDURE THelpWindow.HelpWindowFindDialogClose(Sender: TObject);
BEGIN
  HelpWindowFindDialogActive := False;
END; { HelpWindowFindDialogClose }

PROCEDURE IdentifyLocoOnProgrammingTrack;    { half written 1/2/13 }
CONST
  StopTimer = True;

BEGIN
  DrawLineInLogFile(NoLocoChip, 'X', '-', UnitRef);

  IF MessageDialogueWithDefault('Read loco address from programming track?', StopTimer, mtError, [mbYes, mbNo], mbNo) = mrYes THEN
    ShowMessage(RequestProgrammingModeCV(1));

  DrawLineInLogFile(NoLocoChip, 'X', '-', UnitRef);
END; { IdentifyLocoOnProgrammingTrack }

PROCEDURE THelpWindow.HelpRichEditKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
VAR
  S : String;

BEGIN
  CASE Key OF
    vk_Escape:
      IF HelpWindowFindDialogActive THEN
        HelpWindowFindDialog.CloseDialog
      ELSE
        HelpWindow.Hide;
    Ord('F'):
      IF ssCtrl IN ShiftState THEN BEGIN
        S := Chr(Key);
        SearchHelpText(S);
      END;
    Ord('L'):    { half written 1/2/13 }
      IF ssCtrl IN ShiftState THEN BEGIN
        S := Chr(Key);
        IdentifyLocoOnProgrammingTrack;
      END;
  END; {CASE}
END; { HelpEditKeyDown }

PROCEDURE WriteHelpText(HelpMsg : String);
{ write the text to the help window }
VAR
  IrrelevantShiftState : TShiftState;
  RichLineAdded : Boolean;
  SaveHelpMsg : String;
  TempKey : Word;

BEGIN
  HelpWindow.HelpRichEdit.Lines.Clear;
  AddRichLine(HelpWindow.HelpRichEdit, '<b>KEYBOARD HELP</b>');
  AddRichLine(HelpWindow.HelpRichEdit, '');

  FOR TempKey := 2 TO 255 DO BEGIN
    IF TempKey = Ord('1') THEN
      { insert a blank line before the numeral '1' to assist readability }
      AddRichLine(HelpWindow.HelpRichEdit, '');

    RichLineAdded := False;
    KeyPressedDown(TempKey, [], True, HelpMsg);
    IF (HelpMsg <> '')
    AND (HelpMsg <> SaveHelpMsg)
    THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, []) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssShift], True, HelpMsg);
    IF (HelpMsg <> '')
    AND (HelpMsg <> SaveHelpMsg)
    THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssShift]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssCtrl], True, HelpMsg);
    IF (HelpMsg <> '')
    AND (HelpMsg <> SaveHelpMsg)
    THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssCtrl]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssAlt], True, HelpMsg);
    IF (HelpMsg <> '')
    AND (HelpMsg <> SaveHelpMsg)
    THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssAlt]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssShift, ssAlt], True, HelpMsg);
    IF (HelpMsg <> '')
    AND (HelpMsg <> SaveHelpMsg)
    THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssShift, ssAlt]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssCtrl, ssAlt], True, HelpMsg);
    IF (HelpMsg <> '')
    AND (HelpMsg <> SaveHelpMsg)
    THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssCtrl, ssAlt]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssCtrl, ssShift], True, HelpMsg);
    IF (HelpMsg <> '')
    AND (HelpMsg <> SaveHelpMsg)
    THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssCtrl, ssShift]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssCtrl, ssShift, ssAlt], True, HelpMsg);
    IF (HelpMsg <> '')
    AND (HelpMsg <> SaveHelpMsg)
    THEN BEGIN
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    IF RichLineAdded THEN BEGIN
      CASE TempKey OF
        vk_F1, vk_F2, vk_F3, vk_F4, vk_F5, vk_F6, vk_F7, vk_F8, vk_F9, vk_F10, vk_F11, vk_F12,
        Ord('A')..Ord('Z'):
        { insert a blank line after to assist readability }
        AddRichLine(HelpWindow.HelpRichEdit, '');
      END; {CASE}
    END;
  END; {FOR}

  { Also add mouse help }
  AddRichLine(HelpWindow.HelpRichEdit, '');
  AddRichLine(HelpWindow.HelpRichEdit, '<b>MOUSE HELP</b>');
  ChangeStateOfWhatIsUnderMouse(IrrelevantShiftState, True);

  HelpWindow.Show;

  { Move the cursor to the top of the screen }
  HelpWindow.HelpRichEdit.Perform(EM_SETSEL, 0, 0);
  HelpWindow.HelpRichEdit.Perform(EM_SCROLLCARET, 0, 0);
END; {WriteHelpText }

END { HelpWindow }.
