UNIT WatchdogUnit;
{ This is the independently running watchdog program that monitors Rail.exe and shut down the Lenz system if there's a problem }

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ScktComp, ShellAPI,
  Vcl.Menus;

CONST
  CRLF = #13#10;

TYPE
  LenzConnectionType = (EthernetConnection, USBConnection, NoConnection);

  TWatchdogUnitWindow = CLASS(TForm)
    WatchdogUnitClearButton: TButton;
    WatchdogUnitCloseButton: TButton;
    WatchdogUnitConnectPanel: TPanel;
    WatchdogUnitEthernetConnectButton: TButton;
    WatchdogUnitIncomingGroupBox: TGroupBox;
    WatchdogUnitLabelTextEntry: TLabel;
    WatchdogUnitListBox: TListBox;
    WatchdogUnitMemo: TMemo;
    WatchdogUnitRefreshListBoxButton: TButton;
    WatchdogUnitSendStatusRequestButton: TSpeedButton;
    WatchdogUnitSendTextButton: TSpeedButton;
    WatchdogUnitTCPCommand: TMemo;
    WatchdogUnitTCPIPTimer: TTimer;
    WatchdogUnitTimer: TTimer;
    WatchdogUnitTrayIcon: TTrayIcon;
    WatchdogUnitUSBConnectButton: TButton;
    WatchdogUnitMinimiseButton: TButton;
    WatchdogUnitPopupMenu: TPopupMenu;
    WatchdogUnitPopupClose: TMenuItem;
    PROCEDURE WatchdogUnitClearButtonClick(Sender: TObject);
    PROCEDURE WatchdogUnitCloseButtonClick(Sender: TObject);
    PROCEDURE WatchdogUnitEthernetConnectButtonClick(Sender: TObject);
    PROCEDURE WatchdogUnitMinimiseButtonClick(Sender: TObject);
    PROCEDURE WatchdogUnitPopupCloseClick(Sender: TObject);
    PROCEDURE WatchdogUnitRefreshListBoxButtonClick(Sender: TObject);
    PROCEDURE WatchdogUnitSendStatusRequestButtonClick(Sender: TObject);
    PROCEDURE WatchdogUnitSendTextButtonClick(Sender: TObject);
    PROCEDURE WatchdogUnitTCPIPTimerOnTick(Sender: TObject);
    PROCEDURE WatchdogUnitTimerTick(Sender: TObject);
    PROCEDURE WatchdogUnitTrayIconMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    PROCEDURE WatchdogUnitUSBConnectButtonClick(Sender: TObject);
    PROCEDURE WatchdogUnitWindowCreate(Sender: TObject);
    PROCEDURE WatchdogUnitWindowShow(Sender: TObject);

  PRIVATE
    PROCEDURE WMCopyData(VAR Msg : TWMCopyData); Message WM_COPYDATA;
    { Receives data from FWPRail }

    PROCEDURE OnMinimize(VAR Msg : TWMSysCommand); Message WM_SYSCOMMAND;

    PROCEDURE SendMsgToRailProgram(Msg : String);

  PUBLIC
    PROCEDURE CreateTCPClients(Connection : LenzConnectionType);
    PROCEDURE DestroyTCPClient;

    PROCEDURE ResponsesTCPClientConnect(Sender: TObject; Socket : TCustomWinSocket);
    PROCEDURE ResponsesTCPClientDisconnect(Sender: TObject; Socket : TCustomWinSocket);
    PROCEDURE ResponsesTCPClientRead(Sender: TObject; Socket : TCustomWinSocket);
    PROCEDURE ResponsesTCPClientError(Sender: TObject; Socket : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);

    PROCEDURE ResponsesTCPSendBuffer(Buffer : ARRAY OF Byte; BufferLen : Integer);
    PROCEDURE ResponsesTCPSendText(S : String);
  END;

FUNCTION ReadDataFromTCPIPList : String;
{ Return string from data read in list }

VAR
  BroadcastsTCPClient : TClientSocket = NIL;
  ConnectTS : Int64;
  DataReadInList : TStringList;
  FWPRailWatchdogShuttingDownStr : String = '';
  ReceiverTypeString : PWideChar = '';
  ResponsesTCPClient : TClientSocket = NIL;
  TCPBuf : String = '';
  TCPIPConnected : Boolean = True;
  TCPSocket : TCustomWinSocket = NIL;
  WatchdogUnitWindow : TWatchdogUnitWindow;

IMPLEMENTATION

{$R *.dfm}

CONST
  ConnectedViaUSBStr = 'via USB';
  ConnectedViaEthernetStr = 'via Ethernet';
  NotConnectedMsg = 'The Watchdog program cannot connect to the Lenz system';

VAR
  FirstMessageAlreadyReceivedFromFWPRail : Boolean = False;
  FWPRailLenzOperationsStopped : Boolean = False;
  FWPRailRunning : Boolean = False;
  LastMemoText : String = '';
  LenzConnection : LenzConnectionType = NoConnection;
  LineWrittenToWatchdogUnitMemo : Boolean = False;
  MessageReceivedFromFWPRail : Boolean = False;
  SaveLastMsgReceivedTimeStr : String = '00:00:00';
  SaveLenzConnection : LenzConnectionType = NoConnection;
  StartupFlag : Boolean = True;
  UnitRef : String = 'Watchdog';

PROCEDURE WriteMemoText(S : String);
BEGIN
  WITH WatchdogUnitWindow DO BEGIN
    IF S <> LastMemoText THEN BEGIN
      WatchdogUnitMemo.Text := WatchdogUnitMemo.Text + S + CRLF;
      LastMemoText := S;
    END;
    LineWrittenToWatchdogUnitMemo := True;
  END; {WITH}
END; { WriteMemoText }

FUNCTION LenzConnectionToStr(LenzConnection : LenzConnectionType) : String;
{ Return the kind of connection }
BEGIN
  CASE LenzConnection OF
    USBConnection:
      Result := 'USB Connection';
    EthernetConnection:
      Result := 'Ethernet Connection';
    NoConnection:
      Result := 'No Connection';
  END; {CASE}
END; { LenzConnectionToStr }

FUNCTION IOError(Filename : String; SaveIOResult : Integer; OUT ErrorMsg : String) : Boolean;
{ Returns the IO error message }
BEGIN
  IF SaveIOResult = 0 THEN BEGIN
    Result := False;
    ErrorMsg := '';
  END ELSE BEGIN
    Result := True;
    CASE SaveIOResult OF
      2:
        ErrorMsg := 'File: ' + Filename + ' not found';
      3:
        ErrorMsg := 'Path not found';
      5:
        ErrorMsg := 'File access denied - file ' + Filename + ' is a directory, or is read-only';
      13:
        ErrorMsg := 'Permission denied';
      20:
        ErrorMsg := 'is not a directory';
      21:
        ErrorMsg := 'is a directory';
      32:
        ErrorMsg := 'Sharing violation';
      100:
        ErrorMsg := 'Disk read error';
      101:
        ErrorMsg := 'Disk write error - is disk full?';
      102:
        ErrorMsg := 'File not assigned';
      103:
        ErrorMsg := 'File not open';
      104:
        ErrorMsg := 'File not open for input';
      105:
        ErrorMsg := 'File not open for output';
    ELSE
      ErrorMsg := 'I/O Error no.' + IntToStr(SaveIOResult);
    END; { CASE}
  END;
END; { IOError }

{$O-}
FUNCTION OpenOutputFileOK(VAR OutputFilename : Text; Filename : String; OUT ErrorMsg : String; AppendToFile : Boolean) : Boolean;
{ Open (and create if necessary) a file }
BEGIN
  Result := False;

  TRY
    { If file exists, append to it, else create it }
    {$I-}
    AssignFile(OutputFilename, Filename);
    IF AppendToFile THEN BEGIN
      Append(OutputFilename);
      {$I+}
      IF NOT IOError(Filename, IOResult, ErrorMsg) THEN
        Result := True;
    END;

    IF NOT AppendToFile OR (Result = False) THEN BEGIN
      {$I-}
      Rewrite(OutputFileName);
      {$I+}
      Result := NOT IOError(Filename, IOResult, ErrorMsg);
    END;

    IF Result = False THEN
     ShowMessage('Warning! Unable to open file "' + Filename + '" for writing: ' + ErrorMsg);
  EXCEPT
    ON E : Exception DO
      ShowMessage('OpenOutputFileOK: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { OpenOutputFileOK }

PROCEDURE CloseOutputFile(VAR OutputFile : Text; Filename : String);
{ Close an output file, capturing the error message if any }
VAR
  ErrorMsg : String;

BEGIN
  TRY
    {$I-}
    CloseFile(OutputFile);
    {$I+}
    IF IOError(Filename, IOResult, ErrorMsg) THEN
      ShowMessage('Error in closing file ' + Filename + ': ' + ErrorMsg);
  EXCEPT
    ON E : Exception DO
      ShowMessage('CloseOutputFile: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CloseOutputFile }

PROCEDURE Debug(DebugStr : String);
{ Write an error message to a given file }
CONST
  Append = True;

VAR
  ErrorMsg : String;
  TestOutputFile : Text;
  TestoutputfileName : String;

BEGIN
  TestOutputFileName := 'c:\doc\rad studio\projects\rail\watchdogoutput.txt';
  OpenOutputFileOK(TestOutputFile, TestOutputFileName, ErrorMsg, Append);
  WriteLn(TestOutPutFile, DebugStr);
  CloseOutputFile(TestOutputFile, TestOutputFileName);
END; { Debug }

FUNCTION FillSpace(S : WideString; Len : Integer) : WideString;
BEGIN
  WHILE Length(S) < Len DO
    S := ' ' + S;
  Result := S;
END; { FillSpace }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitUSBConnectButtonClick(Sender: TObject);
BEGIN
  IF TCPSocket = NIL THEN
    CreateTCPClients(USBConnection)
  ELSE
    DestroyTCPClient;
END; { USBConnectButtonClick }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitEthernetConnectButtonClick(Sender: TObject);
BEGIN
  IF TCPSocket = NIL THEN
    CreateTCPClients(EthernetConnection)
  ELSE
    DestroyTCPClient;
END; { WatchdogUnitEthernetConnectButtonClick }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitWindowShow(Sender: TObject);
BEGIN
  SetBounds((Monitor.Left + Monitor.Width) - Width, Monitor.Top, Width, Height);
END; { WatchdogUnitWindowShow }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitSendStatusRequestButtonClick(Sender: TObject);
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    WriteMemoText('OUT [System Status Request]');
    ResponsesTCPSendText('21-24-05' +CRLF);
    WatchdogUnitTCPCommand.Clear;
  END;
END; { WatchdogUnitSendStatusRequestButtonClick }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitSendTextButtonClick(Sender: TObject);
VAR
  I : Integer;
  S : WideString;

BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    IF WatchdogUnitTCPCommand.Lines.Count = 0 THEN
      Exit;

    IF WatchdogUnitTCPCommand.Lines.Count > 1 THEN BEGIN
      S := '';
      FOR I := 0 TO WatchdogUnitTCPCommand.Lines.Count-1 DO
        S := S + '"' + WatchdogUnitTCPCommand.Lines[I] + '" ';
    END ELSE
      S := WatchdogUnitTCPCommand.Lines[0];

    WriteMemoText('OUT ' + S);
    ResponsesTCPSendText(WatchdogUnitTCPCommand.Text);
    WatchdogUnitTCPCommand.Clear;
  END;
END; { WatchdogUnitSendTextButtonClick }

PROCEDURE TWatchdogUnitWindow.CreateTCPClients(Connection : LenzConnectionType);
{ Set the TCPIP clients up }
VAR
  I : Integer;

BEGIN
  IF ResponsesTCPClient = NIL THEN BEGIN
    ResponsesTCPClient              := TClientSocket.Create(WatchdogUnitWindow);
    ResponsesTCPClient.ClientType   := ctNonBlocking;
    ResponsesTCPClient.OnConnect    := ResponsesTCPClientConnect;
    ResponsesTCPClient.OnDisconnect := ResponsesTCPClientDisconnect;
    ResponsesTCPClient.OnRead       := ResponsesTCPClientRead;
    ResponsesTCPClient.OnError      := ResponsesTCPClientError;
  END;

  ResponsesTCPClient.Port           := 5550;
  IF Connection = USBConnection THEN
    ResponsesTCPClient.Address      := '127.0.0.1'
  ELSE
    ResponsesTCPClient.Address      := '192.168.0.200';

  TRY
    ResponsesTCPClient.Active := True;
    I := 0;
    WHILE (TCPSocket = NIL) AND (I < 50) DO BEGIN
      Application.ProcessMessages;
      IF TCPSocket = NIL THEN
        Sleep(100);
      Inc(I);
    END;
  EXCEPT
    FreeAndNIL(ResponsesTCPClient);
    WriteMemoText('*** Unable to Connect to Client 1');
  END;
END; { CreateTCPClients }

PROCEDURE TWatchdogUnitWindow.DestroyTCPClient;
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    IF TCPSocket <> NIL THEN BEGIN
      TCPSocket.Close;
      TCPSocket := NIL;
    END;

    ResponsesTCPClient.Active := False;
    FreeAndNIL(ResponsesTCPClient);
  END;
END; { DestroyTCPClient }

PROCEDURE TWatchdogUnitWindow.ResponsesTCPClientConnect(Sender: TObject; Socket : TCustomWinSocket);
BEGIN
  ConnectTS := GetTickCount;
  TCPSocket := Socket;
  WriteMemoText('TCPClient 1 Connected');
  IF LenzConnection = EthernetConnection THEN BEGIN
    WatchdogUnitEthernetConnectButton.Enabled := True;
    WatchdogUnitEthernetConnectButton.Caption := 'TCP Disconnect';
  END ELSE BEGIN
    WatchdogUnitUSBConnectButton.Enabled := True;
    WatchdogUnitUSBConnectButton.Caption := 'TCP Disconnect';
  END;
END; { ResponsesTCPClientConnect }

PROCEDURE TWatchdogUnitWindow.ResponsesTCPClientDisconnect(Sender: TObject; Socket : TCustomWinSocket);
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    WriteMemoText('TCPClient Disconnected');
    IF LenzConnection = EthernetConnection THEN BEGIN
      WatchdogUnitEthernetConnectButton.Enabled := True;
      WatchdogUnitEthernetConnectButton.Caption := 'TCP Connect';
    END ELSE BEGIN
      WatchdogUnitUSBConnectButton.Enabled := True;
      WatchdogUnitUSBConnectButton.Caption := 'TCP Connect';
    END;
    TCPSocket := NIL;
  END;
END; { ResponsesTCPClientDisconnect }

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

PROCEDURE TWatchdogUnitWindow.ResponsesTCPClientRead(Sender: TObject; Socket : TCustomWinSocket);
VAR
  AnsiStr : AnsiString;
  I : Integer;
  S : String;

BEGIN
  IF Socket.Connected = True THEN BEGIN
    AnsiStr := Socket.ReceiveText;
    S := String(AnsiStr);
    TCPBuf := TCPBuf + S;

    { Strip off the carriage returns }
    WHILE Pos(CRLF, TCPBuf) > 0 DO BEGIN
      I := Pos(CRLF, TCPBuf);
      S := Copy(TCPBuf, 1, I - 1);
      Delete(TCPBuf, 1, I + 1);
      WriteMemoText('IN ' + S);

      { Put what's been read in at the top of the list, and note that it was a response to a request for data }
      DataReadInList.Add('R ' + S);
    END;
  END;
END; { ResponsesTCPClientRead }

PROCEDURE TWatchdogUnitWindow.ResponsesTCPClientError(Sender: TObject; Socket : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);
BEGIN
  WriteMemoText('*** Unable to Connect: ' + SysErrorMessage(ErrorCode));
  ErrorCode := 0;
  SendMsgToRailProgram(NotConnectedMsg);
END; { ResponsesTCPClientError }

PROCEDURE TWatchdogUnitWindow.ResponsesTCPSendText(S : String);
VAR
  AnsiStr : AnsiString;

BEGIN
  AnsiStr := AnsiString(S);
  IF TCPSocket <> NIL THEN BEGIN
    TCPSocket.SendText(AnsiStr + CRLF);
    WriteMemoText('OUT ' + S);
  END;
END; { ResponsesTCPSendText }

PROCEDURE TWatchdogUnitWindow.ResponsesTCPSendBuffer(Buffer : ARRAY OF Byte; BufferLen : Integer);
VAR
  I : Integer;
  S : String;

BEGIN
  IF TCPSocket <> NIL THEN BEGIN
    TCPSocket.SendBuf(Buffer[0], BufferLen);

    S := '';
    FOR I := 0 TO BufferLen - 1 DO
      S := S + IntToStr(Buffer[I]) + ' ';
    WriteMemoText('OUT ' + S);
  END;
END; { ResponsesTCPSendText }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitTCPIPTimerOnTick(Sender: TOBJECT);
{ Send something every 500 miliseconds or the TCPIP ports time out }
BEGIN
  IF TCPSocket <> NIL THEN BEGIN
    WriteMemoText('OUT Sending Watchdog');
    TCPSocket.SendText(' ');
  END;
END; { TCPIPTimerOnTimer }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitClearButtonClick(Sender: TObject);
BEGIN
  WatchdogUnitMemo.Clear;
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

PROCEDURE TWatchdogUnitWindow.SendMsgToRailProgram(Msg : String);
{ Let the Rail program know if we can't start because we're not talking to the Lenz system }
VAR
  CopyData: TCopyDataStruct;
  ReceiverHandle : THandle;
//  ReceiverTypeString : PWideChar;
  Res : Integer;

BEGIN
//  ReceiverTypeString := 'TFWPRailWindow';
  ReceiverHandle := FindWindow(ReceiverTypeString, NIL);
  IF ReceiverHandle = 0 THEN BEGIN
    WriteMemoText('Cannot find FWPRail so cannot send message "' + Msg + '" to it');
    Exit;
  END;

  { We have found the watchdog program }
  WriteMemoText('Sending "' + Msg + '" message to Rail program');

  CopyData.lpData := PChar(Msg);
  CopyData.cbdata := Bytelength(Msg);
  CopyData.dwData := ReceiverHandle;

  Res := SendMessage(ReceiverHandle, WM_COPYDATA, Application.Handle, LPARAM(@CopyData));
  IF Res = 2 THEN
    WriteMemoText('FWPRail Watchdog has acknowledged the message')
  ELSE
    WriteMemoText('FWPRail Watchdog has not acknowledged the message');
END; { SendMsgToRailProgram }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitTimerTick(Sender: TObject);
VAR
  ReceiverHandle : THandle;
  S : String;
  TempWriteArray : ARRAY[0..4] OF Byte;

BEGIN
  TRY
    IF WatchdogUnitTimer.Interval = 1 THEN
      { start off with it set to 1 to make the timer tick immediately, then set it to a more reasonable interval }
      WatchdogUnitTimer.Interval := 5000;

    { At each tick we check if FWPRail is running, and if so whether the Lenz LAN/USB server is running. If they both are then we expect a message every ten seconds. If we
      do not receive one we assume that FWPRail has crashed or otherwise stopped and we put the Lenz system into power-off mode.
    }
    IF FWPRailRunning THEN BEGIN
      ReceiverHandle := FindWindow(ReceiverTypeString, NIL);
      IF ReceiverHandle = 0 THEN BEGIN
        { we can't find FWPRail - if it was running it's presumably been closed down, so close our connection to the LAN/USB Server }
        IF MessageReceivedFromFWPRail THEN
          WriteMemoText('Cannot find FWPRail - if it was running it''s presumably been closed down, so close connection to the LAN/USB Server');;

        FWPRailRunning := False;
        MessageReceivedFromFWPRail := False;
        IF TCPSocket <> NIL THEN
          WatchdogUnitWindow.DestroyTCPClient;

        WatchdogUnitWindow.WatchdogUnitTrayIcon.Visible := False;
        Application.Terminate;
      END;

      IF NOT MessageReceivedFromFWPRail THEN BEGIN
        IF NOT FWPRailLenzOperationsStopped THEN BEGIN
          { Panic! }
          FWPRailLenzOperationsStopped := True;
          IF TCPSocket = NIL THEN BEGIN
            WriteMemoText('FWPRail has stopped running - Lenz system would have been sent a "Stop Operations" command had the Watchdog been connected to it');
            ShowMessage('FWPRail has stopped running - Lenz system would have been sent a "Stop Operations" command had the Watchdog been connected to it');
          END ELSE BEGIN
            WriteMemoText('Watchdog -> Lenz: Stop Operations');
            IF LenzConnection = USBConnection THEN BEGIN
              { Send the data as a string }
              S := '21-80-A1' + CRLF;
              ResponsesTCPSendText(S);
            END ELSE
              IF LenzConnection = EthernetConnection THEN BEGIN
                { Add the two required header elements then send the data as an array of bytes }
                TempWriteArray[0] := 255;
                TempWriteArray[1] := 254;
                TempWriteArray[2] := $21;
                TempWriteArray[3] := $80;
                TempWriteArray[4] := $A1;
                ResponsesTCPSendBuffer(TempWriteArray, 5);
              END;

            WatchdogUnitTCPCommand.Clear;
            WriteMemoText('FWPRail has stopped running - Lenz system has been sent a "Stop Operations" command and FWPRail will be informed');
            Beep;
            ShowMessage('FWPRail has stopped running - Lenz system has been sent a "Stop Operations" command');

            { We might have to wait for a response here, because if FWPRail has stopped (for whatever reason), using SendMessage will cause the watchdog to hang, as
              it waits for a response from FWPRail
            }
            SendMsgToRailProgram('Watchdog has detected that FWPRail has stopped running so has sent the Lenz system a "Stop Operations" command');
          END;
        END;
      END;
    END;

    { and set the running flag to false - it should be set to true by the incoming message before the watchdog timer ticks again }
    MessageReceivedFromFWPRail := False;
    Application.ProcessMessages;
  EXCEPT
    ON E : Exception DO
      WriteMemoText('WatchdogUnitTimerTick:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WatchdogUnitTimerTick }

PROCEDURE TWatchdogUnitWindow.WMCopyData(VAR Msg: TWMCopyData);
{ Receives data from FWPRail }
VAR
  S : String;
  StartPos : Integer;

BEGIN
  SetString(S, PChar(Msg.CopyDataStruct.lpData), Msg.CopyDataStruct.cbData DIV SizeOf(Char));
  IF Pos('FWPRail is running', S) = 0 THEN
    WriteMemoText('Invalid message: "' + S)
  ELSE BEGIN
    { First see what sport of connection the railway program is using, if any. This saves us having to try first one then the other connection method, with an undefined
      timeout between tries.
    }
    IF Pos(ConnectedViaUSBStr, S) <> 0 THEN
      LenzConnection := USBConnection
    ELSE
      IF Pos(ConnectedViaEthernetStr, S) <> 0 THEN
        LenzConnection := EthernetConnection
      ELSE
        LenzConnection := NoConnection;

    IF NOT FirstMessageAlreadyReceivedFromFWPRail THEN BEGIN
      WriteMemoText('First msg from FWPRail received: "' + S + '"');

      IF LenzConnection = NoConnection THEN
        WriteMemoText('Connecting to Lenz system as FWPRail is not connected')
      ELSE BEGIN
        WriteMemoText('Connecting');
        IF TCPSocket = NIL THEN BEGIN
          WriteMemoText('Creating TCP Client via ' + LenzConnectionToStr(LenzConnection));
          WatchdogUnitWindow.CreateTCPClients(LenzConnection);
        END;
      END;
      SaveLenzConnection := LenzConnection;

      FWPRailRunning := True;
      WatchdogUnitTimer.Enabled := True;
      FirstMessageAlreadyReceivedFromFWPRail := True;
      MessageReceivedFromFWPRail := True;
    END ELSE BEGIN
      { Replace the time rather than write a new line out every second }
      StartPos := Pos(SaveLastMsgReceivedTimeStr, WatchdogUnitMemo.Lines.Text);
      IF (StartPos = 0) OR LineWrittenToWatchdogUnitMemo THEN BEGIN
        WriteMemoText('Msg from FWPRail received at ' + TimeToStr(Time));
        SaveLastMsgReceivedTimeStr := TimeToStr(Time);
        LineWrittenToWatchdogUnitMemo := False;
      END ELSE BEGIN
        WatchdogUnitMemo.SelStart := StartPos - 1;
        WatchdogUnitMemo.SelLength := Length(SaveLastMsgReceivedTimeStr);

        { Replace previous time with new time }
        SaveLastMsgReceivedTimeStr := TimeToStr(Time);
        WatchdogUnitMemo.SelText := SaveLastMsgReceivedTimeStr;
      END;
      MessageReceivedFromFWPRail := True;

      IF LenzConnection <> SaveLenzConnection THEN BEGIN
        CASE LenzConnection OF
          NoConnection:
            BEGIN
              IF TCPSocket <> NIL THEN
                DestroyTCPClient;
              WriteMemoText('Now connected to Lenz system as FWPRail is not connected');
            END;
          USBConnection, EthernetConnection:
            BEGIN
              IF TCPSocket <> NIL THEN
                DestroyTCPClient;

              WriteMemoText('Reconnecting');
              WriteMemoText('Creating TCP Client via ' + LenzConnectionToStr(LenzConnection));
              WatchdogUnitWindow.CreateTCPClients(LenzConnection);
            END;
        END; {CASE}
        SaveLenzConnection := LenzConnection;
      END;
    END;
  END;

  { And send an acknowledgment }
  Msg.Result := 1;
END; { WMCopyData }

PROCEDURE TWatchdogUnitWindow.OnMinimize(VAR Msg: TWMSysCommand);
BEGIN
  { This hides the application from taskbar }
  IF Msg.CmdType = SC_MINIMIZE THEN
    Hide
  ELSE
    Inherited;
END; { OnMinimize }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitTrayIconMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
VAR
  PopupPoint : TPoint;

BEGIN
  CASE Button OF
    mbLeft:
      IF Visible THEN BEGIN
        { minimize to TrayIcon }
        Application.Minimize;
      END ELSE BEGIN
        { restore it }
        WatchdogUnitWindow.Show;
        Application.Restore;
      END;

    mbRight:
      BEGIN
        GetCursorPos(PopupPoint);
        WatchdogUnitPopupMenu.Popup(PopupPoint.X, PopupPoint.Y);
      END;
  END; {CASE}
END; { WatchdogUnitTrayIconMouseDown }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitRefreshListBoxButtonClick(Sender: TObject);
BEGIN
  WatchdogUnitListBox.Clear;

  EnumWindows(@EnumWindowsProc, LParam(WatchdogUnitListBox));
END; { WatchdogUnitRefreshListBoxButtonClick }

PROCEDURE CloseFWPRailWatchdog;
BEGIN
  WatchdogUnitWindow.SendMsgToRailProgram(FWPRailWatchdogShuttingDownStr);

  IF ResponsesTCPClient <> NIL THEN
    WatchdogUnitWindow.DestroyTCPClient;

  WatchdogUnitWindow.Free;
  Application.Terminate;
END; { CloseFWPRailWatchdog }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitCloseButtonClick(Sender: TObject);
BEGIN
  CloseFWPRailWatchdog;
END; { WatchdogUnitCloseButtonClick }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitMinimiseButtonClick(Sender: TObject);
BEGIN
  WatchdogUnitWindow.Hide;
END; { WatchdogUnitMinimiseButtonClick }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitPopupCloseClick(Sender: TObject);
BEGIN
  CloseFWPRailWatchdog;
END; { WatchdogUnitPopupCloseClick }

PROCEDURE TWatchdogUnitWindow.WatchdogUnitWindowCreate(Sender : TObject);
VAR
  Buffer : ARRAY[0..255] OF Char;
  Res : Integer;

BEGIN
  { List all the current windows - this is only really needed for debugging (to set the Receiver Strings in this and FWPRail), but no harm in having it each time we run }
  EnumWindows(@EnumWindowsProc, LPARAM(WatchdogUnitListBox));

  WatchdogUnitWindow.WatchdogUnitTrayIcon.Visible := False;
  WatchdogUnitWindow.WatchdogUnitTrayIcon.Icon.Handle := LoadIcon(hInstance, 'WatchdogIcon');
  WatchdogUnitWindow.WatchdogUnitTrayIcon.Hint := 'FWPRailWatchdog';
  WatchdogUnitWindow.WatchdogUnitTrayIcon.Visible := True;

  Res := LoadString(hInstance, 1001, Buffer, SizeOf(Buffer));
  FWPRailWatchdogShuttingDownStr := Buffer;
  Application.ShowMainForm := False;
END; { WatchdogUnitWindowCreate }

INITIALIZATION
  DataReadInList := TStringList.Create;
  ReceiverTypeString := 'TFWPRailWindow';

END { Watchdog }.
