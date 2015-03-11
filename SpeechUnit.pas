UNIT SpeechUnit;
{ This is the independently running speech program that reads out supplied speech without causing the railway program temporarily to hang }

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ScktComp, ShellAPI;

CONST
  CRLF = #13#10;

TYPE
  TFWPRailSpeechWindow = CLASS(TForm)
    ClearButton: TButton;
    IncomingGB: TGroupBox;
    SpeechUnitMemo: TMemo;
    SpeechUnitTimer: TTimer;
    SpeechUnitTrayIcon: TTrayIcon;
    SpeechUnitListBox: TListBox;
    SpeechUnitRefreshListBoxButton: TButton;
    PROCEDURE ClearButtonClick(Sender: TObject);
    PROCEDURE SpeechUnitRefreshListBoxButtonClick(Sender: TObject);
    PROCEDURE SpeechUnitTimerTick(Sender: TObject);
    PROCEDURE SpeechUnitTrayIconClick(Sender: TObject);
    PROCEDURE SpeechUnitWindowClose(Sender: TObject; var Action: TCloseAction);
    PROCEDURE SpeechUnitWindowCreate(Sender: TObject);
    PROCEDURE SpeechUnitWindowShow(Sender: TObject);

  PRIVATE
    PROCEDURE WMCopyData(VAR Msg : TWMCopyData); Message WM_COPYDATA;
    { Receives data from FWPRail }
    PROCEDURE OnMinimize(VAR Msg : TWMSysCommand); Message WM_SYSCOMMAND;
    PROCEDURE SendMsgToRailProgram(Msg : String);

  PUBLIC

  END;

FUNCTION ReadDataFromTCPIPList : String;
{ Return string from data read in list }

VAR
  DataReadInList : TStringList;
  FWPRailSpeechShuttingDownStr  : String;
  TCPBuf : String = '';
  FWPRailSpeechWindow : TFWPRailSpeechWindow;

IMPLEMENTATION

{$R *.dfm}

USES
  OLEAuto;

VAR
  FirstMessageAlreadyReceivedFromFWPRail : Boolean = False;
  FWPRailRunning : Boolean = False;
  LastMemoText : String = '';
  LineWrittenToSpeechUnitMemo : Boolean = False;
  MessageReceivedFromFWPRail : Boolean = False;
  ReceiverTypeString : PWideChar = '';
  SaveLastMsgReceivedTimeStr : String = '00:00:00';
  StartupFlag : Boolean = True;
  UnitRef : String = 'SpeechUnit';

PROCEDURE WriteMemoText(S : String);
BEGIN
  WITH FWPRailSpeechWindow DO BEGIN
    IF S <> LastMemoText THEN BEGIN
      SpeechUnitMemo.Text := SpeechUnitMemo.Text + S + CRLF;
      LastMemoText := S;
    END;
    LineWrittenToSpeechUnitMemo := True;
  END; {WITH}
END; { WriteMemoText }

FUNCTION FillSpace(S : WideString; Len : Integer) : WideString;
BEGIN
  WHILE Length(S) < Len DO
    S := ' ' + S;
  Result := S;
END; { FillSpace }

PROCEDURE TFWPRailSpeechWindow.SpeechUnitWindowShow(Sender: TObject);
BEGIN
  SetBounds((Monitor.Left + Monitor.Width) - Width, Monitor.Top, Width, Height);
END; { SpeechUnitWindowShow }

FUNCTION ReadDataFromTCPIPList : String;
{ Return string from data-read-in list }
BEGIN
  IF DataReadInList.Count < 1 THEN
    Result := ''
  ELSE BEGIN
    Result := DataReadInList[0];
    DataReadInList.Delete(0);
  END;
END; { ReadDataFromTCPIPList }

PROCEDURE TFWPRailSpeechWindow.ClearButtonClick(Sender: TObject);
BEGIN
  SpeechUnitMemo.Clear;
END; { ClearButtonClick }

FUNCTION EnumWindowsProc(wHandle: HWND; lb: TListBox): BOOL; STDCALL;
{ List all the open windows }
VAR
  Title, ClassName: ARRAY[0..255] OF char;

BEGIN
  GetWindowText(wHandle, Title, 255);
  GetClassName(wHandle, ClassName, 255);
  IF IsWindowVisible(wHandle) THEN
    lb.Items.Add(String(Title) + '-' + String(ClassName));
  Result := True;
END; { EnumWindowsProc }

PROCEDURE TFWPRailSpeechWindow.SendMsgToRailProgram(Msg : String);
{ Talk to the Rail program }
VAR
  CopyData: TCopyDataStruct;
  ReceiverHandle : THandle;
  ReceiverTypeString : PWideChar;
  Res : Integer;

BEGIN
  ReceiverTypeString := 'TFWPRailWindow';
  ReceiverHandle := FindWindow(ReceiverTypeString, NIL);
  IF ReceiverHandle = 0 THEN BEGIN
    WriteMemoText('Cannot find FWPRail so cannot send message "' + Msg + '" to it');
    Exit;
  END;

  { We have found the rail window program }
  WriteMemoText('Sending "' + Msg + '" message to Rail program');

  CopyData.lpData := PChar(Msg);
  CopyData.cbdata := Bytelength(Msg);
  CopyData.dwData := ReceiverHandle;

  Res := SendMessage(ReceiverHandle, WM_COPYDATA, Application.Handle, LPARAM(@CopyData));
  IF Res = 2 THEN
    WriteMemoText('FWPRailSpeech has acknowledged the message')
  ELSE
    WriteMemoText('FWPRailSpeech has not acknowledged the message');
END; { SendMsgToRailProgram }

PROCEDURE TFWPRailSpeechWindow.SpeechUnitTimerTick(Sender: TObject);
VAR
  ReceiverHandle : THandle;

BEGIN
  TRY
    IF SpeechUnitTimer.Interval = 1 THEN
      { start off with it set to 1 to make the timer tick immediately, then set it to a more reasonable interval }
      SpeechUnitTimer.Interval := 5000;

    { At each tick we check if FWPRail is running, and if so whether the Lenz LAN/USB server is running. If they both are then we expect a message every ten seconds. If we
      do not receive one we assume that FWPRail has crashed or otherwise stopped and we put the Lenz system into power-off mode.
    }
    ReceiverHandle := FindWindow(ReceiverTypeString, NIL);
    IF ReceiverHandle = 0 THEN BEGIN
      { we can't find FWPRail - if it was running it's presumably been closed down, so stop close our connection to the LAN/USB Server }
      IF MessageReceivedFromFWPRail THEN
        WriteMemoText('Cannot find FWPRail - if it was running it''s presumably been closed down, so close connection to the LAN/USB Server');;

      FWPRailRunning := False;
      MessageReceivedFromFWPRail := False;

      Application.Terminate;
    END;
  EXCEPT
    ON E : Exception DO
      WriteMemoText('SpeechUnitTimerTick:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SpeechUnitTimerTick }

PROCEDURE ReadOut(SoundStr : String);
{ Uses Microsoft built-in SAPI }
VAR
  Voice : OleVariant;

BEGIN
  Voice := CreateOLEObject('SAPI.SPVoice');
  Voice.Speak(SoundStr);
END; { ReadOut }

PROCEDURE TFWPRailSpeechWindow.WMCopyData(VAR Msg: TWMCopyData);
{ Receives data from FWPRail }
VAR
  S : String;

BEGIN
  SetString(S, PChar(Msg.CopyDataStruct.lpData), Msg.CopyDataStruct.cbData DIV SizeOf(Char));
  IF Pos('FWPRail is starting', S) = 0 THEN
    ReadOut(S)
  ELSE
    SpeechUnitTimer.Enabled := True;

  WriteMemoText(S);

  { And send an acknowledgment }
  Msg.Result := 1;
END; { WMCopyData }

PROCEDURE TFWPRailSpeechWindow.SpeechUnitWindowClose(Sender: TObject; VAR Action: TCloseAction);
BEGIN
  SendMsgToRailProgram(FWPRailSpeechShuttingDownStr);
END; { SpeechUnitWindowClose }

PROCEDURE TFWPRailSpeechWindow.OnMinimize(VAR Msg: TWMSysCommand);
BEGIN
  { This hides the application from taskbar }
  IF Msg.CmdType = SC_MINIMIZE THEN BEGIN
    Hide;
    FWPRailSpeechWindow.SpeechUnitTrayIcon.Visible := True;
  END ELSE
    Inherited;
END; { OnMinimize }

PROCEDURE TFWPRailSpeechWindow.SpeechUnitTrayIconClick(Sender : TObject);
BEGIN
  IF Visible THEN BEGIN
    { minimize to TrayIcon }
    Application.Minimize;
  END ELSE BEGIN
    { restore it }
    FWPRailSpeechWindow.Show;
    FWPRailSpeechWindow.SpeechUnitTrayIcon.Visible := False;
    Application.Restore;
  END;
END; { SpeechUnitTrayIconClick }

PROCEDURE TFWPRailSpeechWindow.SpeechUnitRefreshListBoxButtonClick(Sender: TObject);
BEGIN
  SpeechUnitListBox.Clear;

  EnumWindows(@EnumWindowsProc, LPARAM(SpeechUnitListBox));
END; { SpeechUnitRefreshListBoxButtonClick }

PROCEDURE TFWPRailSpeechWindow.SpeechUnitWindowCreate(Sender : TObject);
VAR
  Buffer : ARRAY[0..255] OF Char;
  Res : Integer;

BEGIN
  { List all the current windows - this is only really needed for debugging (to set the Receiver Strings in this and FWPRail), but no harm in having it each time we run }
  EnumWindows(@EnumWindowsProc, LPARAM(SpeechUnitListBox));

  FWPRailSpeechWindow.SpeechUnitTrayIcon.Visible := False;
  FWPRailSpeechWindow.SpeechUnitTrayIcon.Icon.Handle := LoadIcon(hInstance, 'LoudSpeakerIcon');
  FWPRailSpeechWindow.SpeechUnitTrayIcon.Visible := True;

  Res := LoadString(hInstance, 1000, Buffer, SizeOf(Buffer));
  FWPRailSpeechShuttingDownStr := Buffer;

  Application.ShowMainForm := False;
END; { SpeechUnitWindowCreate }

INITIALIZATION
  DataReadInList := TStringList.Create;
  ReceiverTypeString := 'TFWPRailWindow';

END { SpeechUnit }.
