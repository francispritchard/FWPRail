UNIT Watchdog;
{ This is the independently running watchdog program that monitors Rail.exe and shut down the Lenz system if there's a problem }

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ScktComp;

CONST
  CRLF = #13#10;

TYPE
  LenzConnectionType = (EthernetConnection, USBConnection, NoConnection);

  TWatchdogWindow = CLASS(TForm)
    ClearButton: TButton;
    ConnectPanel: TPanel;
    EthernetConnectButton: TButton;
    IncomingGB: TGroupBox;
    LabelTextEntry: TLabel;
    SendStatusRequestButton: TSpeedButton;
    SendTextButton: TSpeedButton;
    TCPCommand: TMemo;
    TCPIPTimer: TTimer;
    USBConnectButton: TButton;
    WatchdogListBox: TListBox;
    WatchdogMemo: TMemo;
    WatchdogTimer: TTimer;
    PROCEDURE ClearButtonClick(Sender: TObject);
    PROCEDURE EthernetConnectButtonClick(Sender: TObject);
    PROCEDURE SendStatusRequestButtonClick(Sender: TObject);
    PROCEDURE SendTextButtonClick(Sender: TObject);
    PROCEDURE USBConnectButtonClick(Sender: TObject);
    PROCEDURE TCPIPTimerOnTimer(Sender: TObject);
    PROCEDURE WatchdogTimerTick(Sender: TObject);
    PROCEDURE WatchdogWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE WatchdogWindowCreate(Sender: TObject);
    PROCEDURE WatchdogWindowShow(Sender: TObject);

  PRIVATE
    PROCEDURE WMCopyData(VAR Msg : TWMCopyData); Message WM_COPYDATA;
    { Receives data from FWPRail }

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
  ReceiverTypeString : PWideChar = '';
  ResponsesTCPClient : TClientSocket = NIL;
  TCPBuf : String = '';
  TCPIPConnected : Boolean = True;
  TCPSocket : TCustomWinSocket = NIL;
  WatchdogWindow : TWatchdogWindow;

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
  LineWrittenToWatchdogMemo : Boolean = False;
  MessageReceivedFromFWPRail : Boolean = False;
  SaveLastMsgReceivedTimeStr : String = '00:00:00';
  SaveLenzConnection : LenzConnectionType = NoConnection;
  StartupFlag : Boolean = True;
  UnitRef : String = 'Watchdog';

PROCEDURE WriteMemoText(S : String);
BEGIN
  WITH WatchdogWindow DO BEGIN
    IF S <> LastMemoText THEN BEGIN
      WatchdogMemo.Text := WatchdogMemo.Text + S + CRLF;
      LastMemoText := S;
    END;
    LineWrittenToWatchdogMemo := True;
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
  TestOutputFileName := 'c:\doc\google drive\rad studio\projects\rail\watchdogoutput.txt';
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

PROCEDURE TWatchdogWindow.WatchdogWindowClose(Sender: TObject; VAR Action: TCloseAction);
BEGIN
  IF ResponsesTCPClient <> NIL THEN
    DestroyTCPClient;
END; { WatchdogWindowClose }

PROCEDURE TWatchdogWindow.USBConnectButtonClick(Sender: TObject);
BEGIN
  IF TCPSocket = NIL THEN
    CreateTCPClients(USBConnection)
  ELSE
    DestroyTCPClient;
END; { USBConnectButtonClick }

PROCEDURE TWatchdogWindow.EthernetConnectButtonClick(Sender: TObject);
BEGIN
  IF TCPSocket = NIL THEN
    CreateTCPClients(EthernetConnection)
  ELSE
    DestroyTCPClient;
END; { EthernetConnectButtonClick }

PROCEDURE TWatchdogWindow.WatchdogWindowShow(Sender: TObject);
BEGIN
  SetBounds((Monitor.Left + Monitor.Width) - Width, Monitor.Top, Width, Height);
END; { WatchdogWindowShow }

PROCEDURE TWatchdogWindow.SendStatusRequestButtonClick(Sender: TObject);
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    WriteMemoText('OUT [System Status Request]');
    ResponsesTCPSendText('21-24-05' +CRLF);
    TCPCommand.Clear;
  END;
END; { SendStatusRequestButtonClick }

PROCEDURE TWatchdogWindow.SendTextButtonClick(Sender: TObject);
VAR
  I : Integer;
  S : WideString;

BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    IF TCPCommand.Lines.Count = 0 THEN
      Exit;

    IF TCPCommand.Lines.Count > 1 THEN BEGIN
      S := '';
      FOR I := 0 TO TCPCommand.Lines.Count-1 DO
        S := S + '"' + TCPCommand.Lines[I] + '" ';
    END ELSE
      S := TCPCommand.Lines[0];

    WriteMemoText('OUT ' + S);;
    ResponsesTCPSendText(TCPCommand.Text);
    TCPCommand.Clear;
  END;
END; { SendTextButtonClick }

PROCEDURE TWatchdogWindow.CreateTCPClients(Connection : LenzConnectionType);
{ Set the TCPIP clients up }
VAR
  I : Integer;

BEGIN
  IF ResponsesTCPClient = NIL THEN BEGIN
    ResponsesTCPClient              := TClientSocket.Create(WatchdogWindow);
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

PROCEDURE TWatchdogWindow.DestroyTCPClient;
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

PROCEDURE TWatchdogWindow.ResponsesTCPClientConnect(Sender: TObject; Socket : TCustomWinSocket);
BEGIN
  ConnectTS := GetTickCount;
  TCPSocket := Socket;
  WriteMemoText('TCPClient 1 Connected');
  IF LenzConnection = EthernetConnection THEN BEGIN
    EthernetConnectButton.Enabled := True;
    EthernetConnectButton.Caption := 'TCP Disconnect';
  END ELSE BEGIN
    USBConnectButton.Enabled := True;
    USBConnectButton.Caption := 'TCP Disconnect';
  END;
END; { ResponsesTCPClientConnect }

PROCEDURE TWatchdogWindow.ResponsesTCPClientDisconnect(Sender: TObject; Socket : TCustomWinSocket);
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    WriteMemoText('TCPClient Disconnected');
    IF LenzConnection = EthernetConnection THEN BEGIN
      EthernetConnectButton.Enabled := True;
      EthernetConnectButton.Caption := 'TCP Connect';
    END ELSE BEGIN
      USBConnectButton.Enabled := True;
      USBConnectButton.Caption := 'TCP Connect';
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

PROCEDURE TWatchdogWindow.ResponsesTCPClientRead(Sender: TObject; Socket : TCustomWinSocket);
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

PROCEDURE TWatchdogWindow.ResponsesTCPClientError(Sender: TObject; Socket : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);
BEGIN
  WriteMemoText('*** Unable to Connect: ' + SysErrorMessage(ErrorCode));
  ErrorCode := 0;
  SendMsgToRailProgram(NotConnectedMsg);
END; { ResponsesTCPClientError }

PROCEDURE TWatchdogWindow.ResponsesTCPSendText(S : String);
VAR
  AnsiStr : AnsiString;

BEGIN
  AnsiStr := AnsiString(S);
  IF TCPSocket <> NIL THEN BEGIN
    TCPSocket.SendText(AnsiStr + CRLF);
    WriteMemoText('OUT ' + S);
  END;
END; { ResponsesTCPSendText }

PROCEDURE TWatchdogWindow.ResponsesTCPSendBuffer(Buffer : ARRAY OF Byte; BufferLen : Integer);
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

PROCEDURE TWatchdogWindow.TCPIPTimerOnTimer(Sender: TOBJECT);
{ Send something every 500 miliseconds or the TCPIP ports time out }
BEGIN
  IF TCPSocket <> NIL THEN BEGIN
    WriteMemoText('OUT Sending Watchdog');
    TCPSocket.SendText(' ');
  END;
END; { TCPIPTimerOnTimer }

PROCEDURE TWatchdogWindow.ClearButtonClick(Sender: TObject);
BEGIN
  WatchdogMemo.Clear;
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

PROCEDURE TWatchdogWindow.SendMsgToRailProgram(Msg : String);
{ Let the Rail program know if we can't start because we're not talking to the Lenz system }
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

PROCEDURE TWatchdogWindow.WatchdogTimerTick(Sender: TObject);
VAR
  ReceiverHandle : THandle;
  S : String;
  TempWriteArray : ARRAY[0..4] OF Byte;

BEGIN
  TRY
    IF WatchdogTimer.Interval = 1 THEN
      { start off with it set to 1 to make the timer tick immediately, then set it to a more reasonable interval }
      WatchdogTimer.Interval := 5000;

    { At each tick we check if FWPRail is running, and if so whether the Lenz LAN/USB server is running. If they both are then we expect a message every ten seconds. If we
      do not receive one we assume that FWPRail has crashed or otherwise stopped and we put the Lenz system into power-off mode.
    }
    ReceiverHandle := FindWindow(ReceiverTypeString, NIL);
    IF ReceiverHandle = 0 THEN BEGIN
      { we can't find FWPRail - if it was running it's presumably been closed down, so stop close our connection to the LAN/USB Server }
      IF MessageReceivedFromFWPRail THEN BEGIN
        WriteMemoText('Cannot find FWPRail - if it was running it''s presumably been closed down, so close connection to the LAN/USB Server');;

        FWPRailRunning := False;
        MessageReceivedFromFWPRail := False;
        IF TCPSocket <> NIL THEN
          WatchdogWindow.DestroyTCPClient;
      END;

      Exit;
    END;

    IF NOT MessageReceivedFromFWPRail THEN BEGIN
      IF NOT FWPRailLenzOperationsStopped THEN BEGIN
        { Panic! }
        FWPRailLenzOperationsStopped := True;
        IF TCPSocket = NIL THEN BEGIN
          WriteMemoText('FWPRail has stopped running - Lenz system would have been sent a "Stop Operations" command had the Watchdog been connected to it');
          Beep;
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

          TCPCommand.Clear;
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

    { and set the running flag to false - it should be set to true by the incoming message before the watchdog timer ticks again }
    MessageReceivedFromFWPRail := False;
    Application.ProcessMessages;
  EXCEPT
    ON E : Exception DO
      WriteMemoText('WatchdogTimerTick:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WatchdogTimerTick }

PROCEDURE TWatchdogWindow.WMCopyData(VAR Msg: TWMCopyData);
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
          WatchdogWindow.CreateTCPClients(LenzConnection);
        END;
      END;
      SaveLenzConnection := LenzConnection;

      FWPRailRunning := True;
      WatchdogTimer.Enabled := True;
      FirstMessageAlreadyReceivedFromFWPRail := True;
      MessageReceivedFromFWPRail := True;
    END ELSE BEGIN
      { Replace the time rather than write a new line out every second }
      StartPos := Pos(SaveLastMsgReceivedTimeStr, WatchdogMemo.Lines.Text);
      IF (StartPos = 0) OR LineWrittenToWatchdogMemo THEN BEGIN
        WriteMemoText('Msg from FWPRail received at ' + TimeToStr(Time));
        SaveLastMsgReceivedTimeStr := TimeToStr(Time);
        LineWrittenToWatchdogMemo := False;
      END ELSE BEGIN
        WatchdogMemo.SelStart := StartPos - 1;
        WatchdogMemo.SelLength := Length(SaveLastMsgReceivedTimeStr);

        { Replace previous time with new time }
        SaveLastMsgReceivedTimeStr := TimeToStr(Time);
        WatchdogMemo.SelText := SaveLastMsgReceivedTimeStr;
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
              WatchdogWindow.CreateTCPClients(LenzConnection);
            END;
        END; {CASE}
        SaveLenzConnection := LenzConnection;
      END;
    END;
  END;

  { And send an acknowledgment }
  Msg.Result := 1;
END; { WMCopyData }

PROCEDURE TWatchdogWindow.WatchdogWindowCreate(Sender: TObject);
{ List all the current windows - this is only really needed for debugging (to set the Receiver Strings in this and FWPRail), but no harm in having it each time we run }
BEGIN
  EnumWindows(@EnumWindowsProc, LPARAM(WatchdogListBox));
END; { WatchdogWindowCreate }

INITIALIZATION
  DataReadInList := TStringList.Create;
  ReceiverTypeString := 'TFWPRailWindow';

END { Watchdog }.
