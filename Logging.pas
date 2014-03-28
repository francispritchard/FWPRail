UNIT Logging;

INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
     Options, Vcl.Menus, InitVars;

TYPE
  TLoggingWindow = CLASS(TForm)
    LoggingWindowFindDialogue: TFindDialog;
    LoggingWindowFontDialogue : TFontDialog;
    LoggingWindowPopupChangeFontSize : TMenuItem;
    LoggingWindowPopupFontSize : TMenuItem;
    LoggingWindowPopupFontSizeRestoreDefault : TMenuItem;
    LoggingWindowPopupMenu : TPopupMenu;
    LoggingWindowRichEdit : TRichEdit;
    PROCEDURE LoggingWindowClose(Sender: TObject; var Action: TCloseAction);
    PROCEDURE LoggingWindowFindDialogueClose(Sender: TObject);
    PROCEDURE LoggingWindowFindDialogueFind(Sender: TObject);
    PROCEDURE LoggingWindowFindDialogueShow(Sender: TObject);
    PROCEDURE LoggingWindowPopupChangeFontSizeClick(Sender : TObject);
    PROCEDURE LoggingWindowPopupFontSizeRestoreDefaultClick(Sender : TObject);
    PROCEDURE LoggingWindowRichEditKeyDown(Sender : TObject; VAR Key : Word; ShiftState : TShiftState);
    PROCEDURE LoggingWindowRichEditMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; MouseX, MouseY : Integer);
    PROCEDURE LoggingWindowRichEditMouseEnter(Sender: TObject);
    PROCEDURE LoggingWindowRichEditMouseLeave(Sender: TObject);
    PROCEDURE LoggingWindowShow(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  LoggingWindow: TLoggingWindow;
  LoggingWindowFindDialogueActive : Boolean = False;
  StoreRichEditLoggingText : Boolean = False;

IMPLEMENTATION

{$R *.dfm}

USES MiscUtils, System.Types, Raildraw, RichEdit, ClipBrd;

PROCEDURE TLoggingWindow.LoggingWindowShow(Sender : TObject);
BEGIN
  LoggingWindow.Height := LoggingWindowHeight;
  LoggingWindow.Width := LoggingWindowWidth;
  LoggingWindow.Top := LoggingWindowTop;
  LoggingWindow.Left := LoggingWindowLeft;

  LoggingWindowRichEdit.Font.Name := LoggingWindowFontName;
  LoggingWindowRichEdit.Font.Size := LoggingWindowFontSize;
  LoggingWindowRichEdit.Font.Style := [];
END; { LoggingWindowShow }

PROCEDURE TLoggingWindow.LoggingWindowPopupChangeFontSizeClick(Sender : TObject);
BEGIN
  { Show the default }
  LoggingWindowFontDialogue.Font.Name := LoggingWindowFontName;
  LoggingWindowFontDialogue.Font.Size := LoggingWindowFontSize;

  { Allow the user to change it }
  IF LoggingWindowFontDialogue.Execute THEN BEGIN
    LoggingWindowFontName := LoggingWindowFontDialogue.Font.Name;
    LoggingWindowFontSize := LoggingWindowFontDialogue.Font.Size;
    LoggingWindowRichEdit.Font.Style := [];

    LoggingWindowRichEdit.Font := LoggingWindowFontDialogue.Font;
  END;
END; { LoggingWindowPopupChangeFontSizeClick }

PROCEDURE TLoggingWindow.LoggingWindowPopupFontSizeRestoreDefaultClick(Sender : TObject);
BEGIN
  LoggingWindowRichEdit.Font.Name := DefaultLoggingWindowFontName;
  LoggingWindowFontName := DefaultLoggingWindowFontName;
  LoggingWindowRichEdit.Font.Size := DefaultLoggingWindowFontSize;
  LoggingWindowFontSize := DefaultLoggingWindowFontSize;
  LoggingWindowRichEdit.Font.Style := [];
END; { LoggingWindowPopupFontSizeRestoreDefaultClick }

PROCEDURE TLoggingWindow.LoggingWindowClose(Sender: TObject; VAR Action: TCloseAction);
BEGIN
  { Store where we want the window to be in case we move it, close it then want to reopen it }
  LoggingWindowHeight := LoggingWindow.Height;
  LoggingWindowWidth := LoggingWindow.Width;
  LoggingWindowTop := LoggingWindow.Top;
  LoggingWindowLeft := LoggingWindow.Left;
END; { LoggingWindowClose }

PROCEDURE TLoggingWindow.LoggingWindowFindDialogueClose(Sender: TObject);
BEGIN
  LoggingWindowFindDialogueActive := False;
END; { LoggingWindowFindDialogClose }

PROCEDURE TLoggingWindow.LoggingWindowFindDialogueShow(Sender: TObject);
BEGIN
  { There is no FindDialog.Width so the positioning here is a fudge to get it to the right of the Help window }
  LoggingWindowFindDialogue.Left := LoggingWindow.Left + (LoggingWindow.Width DIV 2);

  LoggingWindowFindDialogue.Top := LoggingWindow.Top;
  LoggingWindowFindDialogueActive := True;
END; { LoggingWindowFindDialogShow }

PROCEDURE TLoggingWindow.LoggingWindowFindDialogueFind(Sender: TObject);
{ From "http://docwiki.embarcadero.com/CodeExamples/XE3/en/FindText_%28Delphi%29" }
VAR
  FoundAt : LongInt;
  mySearchTypes : TSearchTypes;
  StartPos : Integer;
  ToEnd : Integer;

BEGIN
  mySearchTypes := [];

  WITH LoggingWindowRichEdit DO BEGIN
    IF frMatchCase IN LoggingWindowFindDialogue.Options THEN
      mySearchTypes := mySearchTypes + [stMatchCase];
    IF frWholeWord IN LoggingWindowFindDialogue.Options THEN
      mySearchTypes := mySearchTypes + [stWholeWord];

    { Begin the search after the current selection, if there is one, otherwise, begin at the start of the text }
    IF SelLength <> 0 THEN
      StartPos := SelStart + SelLength
    ELSE
      StartPos := 0;

    { ToEnd is the length from StartPos through the end of the text in the rich edit control }
    ToEnd := Length(Text) - StartPos;
    FoundAt := FindText(LoggingWindowFindDialogue.FindText, StartPos, ToEnd, mySearchTypes);
    IF FoundAt = -1 THEN
      Beep
    ELSE BEGIN
      SetFocus;
      SelStart := FoundAt;
      SelLength := Length(LoggingWindowFindDialogue.FindText);
    END;
  END; {WITH}
END; { LoggingWindowFindDialogFind }

PROCEDURE TLoggingWindow.LoggingWindowRichEditKeyDown(Sender : TObject; VAR Key : Word; ShiftState : TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      IF LoggingWindowFindDialogueActive THEN
        LoggingWindowFindDialogue.CloseDialog
      ELSE
        LoggingWindow.Hide;
    Ord('F'), Ord('f'):
      IF ssCtrl IN ShiftState THEN BEGIN
        LoggingWindowFindDialogue.Position := Point(LoggingWindowRichEdit.Left + LoggingWindowRichEdit.Width, LoggingWindowRichEdit.Top);
        LoggingWindowFindDialogue.Execute;
      END;
    Ord('G'), Ord('g'):
      LoggingWindow.Visible := False;
  END; {CASE}
END; { LoggingWindowRichEditKeyDown }

PROCEDURE TLoggingWindow.LoggingWindowRichEditMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; MouseX, MouseY : Integer);
BEGIN
  IF Button = mbRight THEN
    LoggingWindowPopupMenu.Popup(LoggingWindow.Left + MouseX, LoggingWindow.Top + MouseY);
END; { LoggingWindowRichEditMouseDown }

PROCEDURE TLoggingWindow.LoggingWindowRichEditMouseEnter(Sender: TObject);
BEGIN
  { Reset the cursor in case we enter the logging window with a non-default mouse cursor, e.g. having immediately previously hovered over a point }
  ChangeCursor(crDefault);

  { Prevent new additions to the log automatically scrolling the logging window if the mouse cursor is within it - otherwise the window automatically scrolls to the bottom
    of the text even if we're examing text further up.
  }
  StoreRichEditLoggingText := True;

  { And let the user know what is happening }
  AddRichLine(LoggingWindow.LoggingWindowRichEdit, '<B>Pausing writing the log to the logging window screen until the cursor is returned to the logging window</B>');
END; { LoggingWindowRichEditMouseEnter }

PROCEDURE TLoggingWindow.LoggingWindowRichEditMouseLeave(Sender: TObject);
VAR
  TempStr: String;

BEGIN
  StoreRichEditLoggingText := False;

  { Remove the "paused" message }
  LoggingWindow.LoggingWindowRichEdit.Lines.Delete(LoggingWindow.LoggingWindowRichEdit.Lines.Count - 1);

  { Now add any stored rich-edit data to the logging window }
  AddStoredRichEditLoggingTextToLoggingWindow;
END; { LoggingWindowRichEditMouseLeave }

END { Logging }.
