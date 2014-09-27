UNIT Help;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, Types;

TYPE
  THelpWindow = CLASS(TForm)
    HelpRichEdit: TRichEdit;
    HelpWindowFindDialog: TFindDialog;
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

//PROCEDURE FindDialogFind(FindDialog : TFindDialog; Text : String; OUT FoundPos : Integer);
//{ Find a given serach string - designed to be use on more than just this window }

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
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE THelpWindow.HelpMemoKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      HelpWindow.Hide;
  END; {CASE}
END; { HelpMemoKeyDown }

PROCEDURE THelpWindow.HelpWindowFindDialogFind(Sender: TObject);
{ From "http://docwiki.embarcadero.com/CodeExamples/XE3/en/FindText_%28Delphi%29" }
VAR
  FoundAt : LongInt;
  mySearchTypes : TSearchTypes;
  StartPos : Integer;
  ToEnd : Integer;

BEGIN
  mySearchTypes := [];

  WITH HelpRichEdit DO BEGIN
    IF frMatchCase IN HelpWindowFindDialog.Options THEN
      mySearchTypes := mySearchTypes + [stMatchCase];
    IF frWholeWord IN HelpWindowFindDialog.Options THEN
      mySearchTypes := mySearchTypes + [stWholeWord];

    { Begin the search after the current selection, if there is one, otherwise, begin at the start of the text }
    IF SelLength <> 0 THEN
      StartPos := SelStart + SelLength
    ELSE
      StartPos := 0;

    { ToEnd is the length from StartPos through the end of the text in the rich edit control }
    ToEnd := Length(Text) - StartPos;
    FoundAt := FindText(HelpWindowFindDialog.FindText, StartPos, ToEnd, mySearchTypes);
    IF FoundAt = -1 THEN
      MakeSound(1)
    ELSE BEGIN
      SetFocus;
      SelStart := FoundAt;
      SelLength := Length(HelpWindowFindDialog.FindText);
    END;
  END; {WITH}
END; { HelpWindowFindDialogFind }

PROCEDURE THelpWindow.HelpRichEditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
BEGIN
  Debug('x=' + InttOstr(X) + ' y=' + inttostr(Y));
END; { HelpRichEditMouseDown }

PROCEDURE THelpWindow.HelpWindowFindDialogShow(Sender: TObject);
BEGIN
  { There is no FindDialog.Width so the positioning here is a fudge to get it to the right of the Help window }
  HelpWindowFindDialog.Left := HelpWindow.Left + (HelpWindow.Width DIV 2);

  HelpWindowFindDialog.Top := HelpWindow.Top;
  HelpWindowFindDialogActive := True;
END; { HelpWindowFindDialogShow }

PROCEDURE THelpWindow.HelpWindowFindDialogClose(Sender: TObject);
BEGIN
  HelpWindowFindDialogActive := False;
END; { HelpWindowFindDialogClose }

PROCEDURE IdentifyLocoOnProgrammingTrack;    { ********** half written 1/2/13 }
CONST
  StopTimer = True;

BEGIN
  DrawLineInLogFile(UnknownLocoChipStr, 'X', '-', UnitRef);

  IF MessageDialogueWithDefault('Read loco address from programming track?', StopTimer, mtError, [mbYes, mbNo], mbNo) = mrYes THEN
    ShowMessage(RequestProgrammingModeCV(1));

  DrawLineInLogFile(UnknownLocoChipStr, 'X', '-', UnitRef);
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
        HelpWindowFindDialog.Position := Point(HelpRichEdit.Left + HelpRichEdit.Width, HelpRichEdit.Top);
        HelpWindowFindDialog.Execute;
      END;
    Ord('L'):    { half written 1/2/13 }
      IF ssCtrl IN ShiftState THEN BEGIN
        S := Chr(Key);
        IdentifyLocoOnProgrammingTrack;
      END;
  END; {CASE}
END; { HelpRichEditKeyDown }

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

    IF TempKey = Ord('A') THEN
      { insert a blank line before the letter 'a' to assist readability }
      AddRichLine(HelpWindow.HelpRichEdit, '');

    RichLineAdded := False;
    KeyPressedDown(TempKey, [], True, HelpMsg);
    IF (HelpMsg <> '') AND (HelpMsg <> SaveHelpMsg) THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, []) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssShift], True, HelpMsg);
    IF (HelpMsg <> '') AND (HelpMsg <> SaveHelpMsg) THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssShift]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssCtrl], True, HelpMsg);
    IF (HelpMsg <> '') AND (HelpMsg <> SaveHelpMsg) THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssCtrl]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssAlt], True, HelpMsg);
    IF (HelpMsg <> '') AND (HelpMsg <> SaveHelpMsg) THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssAlt]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssShift, ssAlt], True, HelpMsg);
    IF (HelpMsg <> '') AND (HelpMsg <> SaveHelpMsg) THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssShift, ssAlt]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssCtrl, ssAlt], True, HelpMsg);
    IF (HelpMsg <> '') AND (HelpMsg <> SaveHelpMsg) THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssCtrl, ssAlt]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssCtrl, ssShift], True, HelpMsg);
    IF (HelpMsg <> '') AND (HelpMsg <> SaveHelpMsg) THEN BEGIN
      AddRichLine(HelpWindow.HelpRichEdit, '<b>' + DescribeKey(TempKey, [ssCtrl, ssShift]) + '</b> - ' + HelpMsg);
      SaveHelpMsg := HelpMsg;
      RichLineAdded := True;
    END;

    KeyPressedDown(TempKey, [ssCtrl, ssShift, ssAlt], True, HelpMsg);
    IF (HelpMsg <> '') AND (HelpMsg <> SaveHelpMsg) THEN BEGIN
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
  ChangeStateOfWhatIsUnderMouse(0, 0, IrrelevantShiftState, True);

  HelpWindow.Show;

  { Move the cursor to the top of the screen }
  HelpWindow.HelpRichEdit.Perform(EM_SETSEL, 0, 0);
  HelpWindow.HelpRichEdit.Perform(EM_SCROLLCARET, 0, 0);
END; { WriteHelpText }

END { HelpWindow }.
