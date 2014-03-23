UNIT Logging;

INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
     Options, Vcl.Menus, InitVars;

TYPE
  TLoggingWindow = CLASS(TForm)
    LoggingWindowRichEdit : TRichEdit;
    LoggingWindowPopupMenu : TPopupMenu;
    LoggingWindowPopupFontSize : TMenuItem;
    LoggingWindowPopupChangeFontSize : TMenuItem;
    LoggingWindowPopupFontSizeRestoreDefault : TMenuItem;
    LoggingWindowFontDialogue : TFontDialog;
    LoggingWindowFindDialog: TFindDialog;
    PROCEDURE LoggingWindowFindDialogClose(Sender: TObject);
    PROCEDURE LoggingWindowFindDialogFind(Sender: TObject);
    PROCEDURE LoggingWindowFindDialogShow(Sender: TObject);
    PROCEDURE LoggingWindowPopupChangeFontSizeClick(Sender : TObject);
    PROCEDURE LoggingWindowPopupFontSizeRestoreDefaultClick(Sender : TObject);
    PROCEDURE LoggingWindowRichEditKeyDown(Sender : TObject; VAR Key : Word; ShiftState : TShiftState);
    PROCEDURE LoggingWindowRichEditMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; MouseX, MouseY : Integer);
    PROCEDURE LoggingWindowShow(Sender: TObject);
    procedure LoggingWindowClose(Sender: TObject; var Action: TCloseAction);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  LoggingWindow: TLoggingWindow;
  LoggingWindowFindDialogActive : Boolean = False;

IMPLEMENTATION

{$R *.dfm}

USES MiscUtils;

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

PROCEDURE TLoggingWindow.LoggingWindowFindDialogClose(Sender: TObject);
BEGIN
  LoggingWindowFindDialogActive := False;
END; { LoggingWindowFindDialogClose }

PROCEDURE TLoggingWindow.LoggingWindowFindDialogShow(Sender: TObject);
BEGIN
  { There is no FindDialog.Width so the positioning here is a fudge to get it to the right of the Help window }
  LoggingWindowFindDialog.Left := LoggingWindow.Left + (LoggingWindow.Width DIV 2);

  LoggingWindowFindDialog.Top := LoggingWindow.Top;
  LoggingWindowFindDialogActive := True;
END; { LoggingWindowFindDialogShow }

PROCEDURE TLoggingWindow.LoggingWindowFindDialogFind(Sender: TObject);
{ From "http://docwiki.embarcadero.com/CodeExamples/XE3/en/FindText_%28Delphi%29" }
VAR
  FoundAt : LongInt;
  mySearchTypes : TSearchTypes;
  StartPos : Integer;
  ToEnd : Integer;

BEGIN
  mySearchTypes := [];

  WITH LoggingWindowRichEdit DO BEGIN
    IF frMatchCase IN LoggingWindowFindDialog.Options THEN
      mySearchTypes := mySearchTypes + [stMatchCase];
    IF frWholeWord IN LoggingWindowFindDialog.Options THEN
      mySearchTypes := mySearchTypes + [stWholeWord];

    { Begin the search after the current selection, if there is one, otherwise, begin at the start of the text }
    IF SelLength <> 0 THEN
      StartPos := SelStart + SelLength
    ELSE
      StartPos := 0;

    { ToEnd is the length from StartPos through the end of the text in the rich edit control }
    ToEnd := Length(Text) - StartPos;
    FoundAt := FindText(LoggingWindowFindDialog.FindText, StartPos, ToEnd, mySearchTypes);
    IF FoundAt = -1 THEN
      Beep
    ELSE BEGIN
      SetFocus;
      SelStart := FoundAt;
      SelLength := Length(LoggingWindowFindDialog.FindText);
    END;
  END; {WITH}
END; { LoggingWindowFindDialogFind }

PROCEDURE TLoggingWindow.LoggingWindowRichEditKeyDown(Sender : TObject; VAR Key : Word; ShiftState : TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      IF LoggingWindowFindDialogActive THEN
        LoggingWindowFindDialog.CloseDialog
      ELSE
        LoggingWindow.Hide;
    Ord('F'), Ord('f'):
      IF ssCtrl IN ShiftState THEN BEGIN
        LoggingWindowFindDialog.Position := Point(LoggingWindowRichEdit.Left + LoggingWindowRichEdit.Width, LoggingWindowRichEdit.Top);
        LoggingWindowFindDialog.Execute;
      END;
    Ord('G'), Ord('g'):
      LoggingWindow.Visible := False;
  END; {CASE}
END; { LoggingWindowRichEditKeyDown }

PROCEDURE TLoggingWindow.LoggingWindowRichEditMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; MouseX, MouseY : Integer);
BEGIN
  IF Button = mbRight THEN
    LoggingWindowPopupMenu.Popup(MouseX, MouseY);
END; { LoggingWindowRichEditMouseDown }

END { Logging }.
