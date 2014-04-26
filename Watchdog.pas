UNIT Watchdog;
{ This is the independently running watchdog program that monitors Rail.exe and shut down the Lenz system if there's a problem }

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ScktComp;

CONST
  CRLF = #13#10;

TYPE
  TWatchdogWindow = CLASS(TForm)
    ClearButton: TButton;
    ConnectPanel: TPanel;
    IncomingGB: TGroupBox;
    LabelTextEntry: TLabel;
    MSGMemo: TMemo;
    SendStatusRequestButton: TSpeedButton;
    SendTextButton: TSpeedButton;
    TCPCommand: TMemo;
    TCPConnectButton: TButton;
    TCPIPTimer: TTimer;
    WatchdogListBox: TListBox;
    WatchdogTimer: TTimer;
    PROCEDURE ClearButtonClick(Sender: TObject);
    PROCEDURE SendStatusRequestButtonClick(Sender: TObject);
    PROCEDURE SendTextButtonClick(Sender: TObject);
    PROCEDURE TCPConnectButtonClick(Sender: TObject);
    PROCEDURE TCPIPTimerOnTimer(Sender: TObject);
    PROCEDURE WatchdogTimerTick(Sender: TObject);
    PROCEDURE WatchdogWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE WatchdogWindowCreate(Sender: TObject);
    PROCEDURE WatchdogWindowShow(Sender: TObject);

  PRIVATE
    PROCEDURE WMCopyData(VAR Msg : TWMCopyData); Message WM_COPYDATA;
    { Receives data from FWPRail }

    PROCEDURE SendNotConnectedMsgToRailProgram;

  PUBLIC
    PROCEDURE CreateTCPClients;
    PROCEDURE DestroyTCPClients;

    PROCEDURE ResponsesTCPClientConnect(Sender: TObject; Socket1 : TCustomWinSocket);
    PROCEDURE ResponsesTCPClientDisconnect(Sender: TObject; Socket1 : TCustomWinSocket);
    PROCEDURE ResponsesTCPClientRead(Sender: TObject; Socket1 : TCustomWinSocket);
    PROCEDURE ResponsesTCPClientError(Sender: TObject; Socket1 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);

    PROCEDURE BroadcastsTCPClientConnect(Sender: TObject; Socket2 : TCustomWinSocket);
    PROCEDURE BroadcastsTCPClientDisconnect(Sender: TObject; Socket2 : TCustomWinSocket);
    PROCEDURE BroadcastsTCPClientRead(Sender: TObject; Socket2 : TCustomWinSocket);
    PROCEDURE BroadcastsTCPClientError(Sender: TObject; Socket2 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);

    PROCEDURE ResponsesTCPSendText(S : String);
  END;

FUNCTION ReadDataFromTCPIPList : String;
{ Return string from data read in list }

PROCEDURE StartLANUSBServer;
{ Start the server programatically }

PROCEDURE StopLANUSBServer;
{ Stop the server programatically }

VAR
  BroadcastsTCPClient : TClientSocket = NIL;
  ConnectTS : Int64;
  DataReadInList : TStringList;
  ReceiverTypeString : PWideChar = '';
  ResponsesTCPClient : TClientSocket = NIL;
  TCPBuf1 : String = '';
  TCPBuf2 : String = '';
  TCPIPConnected : Boolean = True;
  TCPSocket1 : TCustomWinSocket = NIL;
  TCPSocket2 : TCustomWinSocket = NIL;
  WatchdogWindow : TWatchdogWindow;

IMPLEMENTATION

{$R *.dfm}

VAR
  FirstMessageReceivedFromFWPRail : Boolean = False;
  FWPRailLenzOperationsStopped : Boolean = False;
  FWPRailRunning : Boolean = False;
  LastMemoText : String = '';
  MessageReceivedFromFWPRail : Boolean = False;
  StartupFlag : Boolean = True;
  UnitRef : String = 'Watchdog';

PROCEDURE WriteMemoText(S : String);
BEGIN
  WITH WatchdogWindow DO BEGIN
    IF S <> LastMemoText THEN BEGIN
      MSGMemo.Text := MSGMemo.Text + S + CRLF;
      LastMemoText := S;
    END;
  END; {WITH}
END; { WriteMemoText }

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
    DestroyTCPClients;
END; { WatchdogWindowClose }

PROCEDURE TWatchdogWindow.TCPConnectButtonClick(Sender: TObject);
BEGIN
  IF TCPSocket1 = NIL THEN
    CreateTCPClients
  ELSE
    DestroyTCPClients;
END; { TCPConnectButtonClick }

PROCEDURE TWatchdogWindow.WatchdogWindowShow(Sender: TObject);
BEGIN
  SetBounds((Monitor.Left + Monitor.Width) - Width, Monitor.Top, Width, Height);
END; { WatchdogWindowShow }

PROCEDURE TWatchdogWindow.SendStatusRequestButtonClick(Sender: TObject);
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    WriteMemoText('OUT ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : [System Status Request]');
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

    WriteMemoText('OUT ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);;
    ResponsesTCPSendText(TCPCommand.Text);
    TCPCommand.Clear;
  END;
END; { SendTextButtonClick }

PROCEDURE TWatchdogWindow.CreateTCPClients;
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

  ResponsesTCPClient.Port         := 5550;
  ResponsesTCPClient.Address      := '127.0.0.1';

  TRY
    ResponsesTCPClient.Active := True;
    I := 0;
    WHILE (TCPSocket1 = NIL) AND (I < 50) DO BEGIN
      Application.ProcessMessages;
      IF TCPSocket1 = NIL THEN
        Sleep(100);
      Inc(I);
    END;
  EXCEPT
    FreeAndNIL(ResponsesTCPClient);
    WriteMemoText('*** Unable to Connect to Client 1');
  END;

  IF BroadcastsTCPClient = NIL THEN BEGIN
    BroadcastsTCPClient              := TClientSocket.Create(WatchdogWindow);
    BroadcastsTCPClient.ClientType   := ctNonBlocking;
    BroadcastsTCPClient.OnConnect    := BroadcastsTCPClientConnect;
    BroadcastsTCPClient.OnDisconnect := BroadcastsTCPClientDisconnect;
    BroadcastsTCPClient.OnRead       := BroadcastsTCPClientRead;
    BroadcastsTCPClient.OnError      := BroadcastsTCPClientError;
  END;

  BroadcastsTCPClient.Port         := 5551;
  BroadcastsTCPClient.Address      := '127.0.0.1';

  TRY
    BroadcastsTCPClient.Active := True;
    I := 0;
    WHILE (TCPSocket2 = NIL) AND (I < 50) DO BEGIN
      Application.ProcessMessages;
      IF TCPSocket2 = NIL THEN
        Sleep(100);
      Inc(I);
    END;
  EXCEPT
    FreeAndNIL(BroadcastsTCPClient);
    WriteMemoText('*** Unable to Connect to Client 2');
  END;
END; { CreateTCPClients }

PROCEDURE TWatchdogWindow.DestroyTCPClients;
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    IF TCPSocket1 <> NIL THEN BEGIN
      TCPSocket1.Close;
      TCPSocket1 := NIL;
    END;

    ResponsesTCPClient.Active := False;
    FreeAndNIL(ResponsesTCPClient);
  END;

  IF BroadcastsTCPClient <> NIL THEN BEGIN
    IF TCPSocket2 <> NIL THEN BEGIN
      TCPSocket2.Close;
      TCPSocket2 := NIL;
    END;

    BroadcastsTCPClient.Active := False;
    FreeAndNIL(BroadcastsTCPClient);
  END;
END; { DestroyTCPClients }

PROCEDURE TWatchdogWindow.ResponsesTCPClientConnect(Sender: TObject; Socket1 : TCustomWinSocket);
BEGIN
  ConnectTS := GetTickCount;
  TCPSocket1 := Socket1;
  TCPConnectButton.Enabled := True;
  TCPConnectButton.Caption := 'TCP 1 Disconnect';
END; { ResponsesTCPClientConnect }

PROCEDURE TWatchdogWindow.BroadcastsTCPClientConnect(Sender: TObject; Socket2 : TCustomWinSocket);
BEGIN
  ConnectTS := GetTickCount;
  TCPSocket2 := Socket2;
  WriteMemoText('TCPClient 2 Connected');
  TCPConnectButton.Enabled := True;
  TCPConnectButton.Caption := 'TCP 2 Disconnect';
END; { BroadcastsTCPClientConnect }

PROCEDURE TWatchdogWindow.ResponsesTCPClientDisconnect(Sender: TObject; Socket1 : TCustomWinSocket);
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    WriteMemoText('TCPClient 1 Disconnected');
    TCPConnectButton.Enabled := True;
    TCPConnectButton.Caption := 'TCP 1 Connect';
    TCPSocket1 := NIL;
  END;
END; { ResponsesTCPClientDisconnect }

PROCEDURE TWatchdogWindow.BroadcastsTCPClientDisconnect(Sender: TObject; Socket2 : TCustomWinSocket);
BEGIN
  IF BroadcastsTCPClient <> NIL THEN BEGIN
    WriteMemoText('G TCPClient 2 Disconnected');
    TCPConnectButton.Enabled := True;
    TCPConnectButton.Caption := 'TCP 2 Connect';
    TCPSocket2 := NIL;
  END;
END; { BroadcastsTCPClientDisconnect }

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

PROCEDURE TWatchdogWindow.ResponsesTCPClientRead(Sender: TObject; Socket1 : TCustomWinSocket);
VAR
  AnsiStr : AnsiString;
  I : Integer;
  S : String;

BEGIN
  IF Socket1.Connected = True THEN BEGIN
    AnsiStr := Socket1.ReceiveText;
    S := String(AnsiStr);
    TCPBuf1 := TCPBuf1 + S;

    { Strip off the carriage returns }
    WHILE Pos(CRLF, TCPBuf1) > 0 DO BEGIN
      I := Pos(CRLF, TCPBuf1);
      S := Copy(TCPBuf1, 1, I - 1);
      Delete(TCPBuf1, 1, I + 1);
      WriteMemoText('IN1  ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);

      { Put what's been read in at the top of the list, and note that it was a response to a request for data }
      DataReadInList.Add('R ' + S);
    END;
  END;
END; { ResponsesTCPClientRead }

PROCEDURE TWatchdogWindow.BroadcastsTCPClientRead(Sender: TObject; Socket2 : TCustomWinSocket);
VAR
  AnsiStr : AnsiString;
  S : String;
  I : Integer;

BEGIN
  IF Socket2.Connected = True THEN BEGIN
    AnsiStr := Socket2.ReceiveText;
    S := String(AnsiStr);
    TCPBuf2 := TCPBuf2 + S;

    WHILE Pos(CRLF, TCPBuf2) > 0 DO BEGIN
      I := Pos(CRLF, TCPBuf2);
      S := Copy(TCPBuf2, 1, I - 1);
      Delete(TCPBuf2, 1, I + 1);
      WriteMemoText('IN2  ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);

      { Put what's been read in at the top of the list, and note that it was a broadcast }
      DataReadInList.Add('B ' + S);
    END;
  END;
END; { BroadcastsTCPClientRead }

PROCEDURE TWatchdogWindow.ResponsesTCPClientError(Sender: TObject; Socket1 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);
BEGIN
  WriteMemoText('*** Unable to Connect: ' + SysErrorMessage(ErrorCode));
  ErrorCode := 0;
  SendNotConnectedMsgToRailProgram;
END; { ResponsesTCPClientError }

PROCEDURE TWatchdogWindow.BroadcastsTCPClientError(Sender: TObject; Socket2 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);
BEGIN
  WriteMemoText('*** Unable to Connect: ' + SysErrorMessage(ErrorCode));
  ErrorCode := 0;
END; { BroadcastsTCPClientError }

PROCEDURE TWatchdogWindow.ResponsesTCPSendText(S : String);
VAR
  AnsiStr : AnsiString;

BEGIN
  AnsiStr := AnsiString(S);
  IF TCPSocket1 <> NIL THEN BEGIN
    TCPSocket1.SendText(AnsiStr + CRLF);
    WriteMemoText('OUT1 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + ' ' + S);
  END;
END; { ResponsesTCPSendText }

PROCEDURE TWatchdogWindow.TCPIPTimerOnTimer(Sender: TOBJECT);
{ Send something every 500 miliseconds or the TCPIP ports time out }
BEGIN
  IF TCPSocket1 <> NIL THEN BEGIN
    WriteMemoText('OUT1 ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : Sending Watchdog');
    TCPSocket1.SendText(' ');
  END;

  IF TCPSocket2 <> NIL THEN BEGIN
    WriteMemoText('OUT2 ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : Sending Watchdog');
    TCPSocket2.SendText(' ');
  END;
END; { TCPIPTimerOnTimer }

PROCEDURE TWatchdogWindow.ClearButtonClick(Sender: TObject);
BEGIN
  MSGMemo.Clear;
END; { ClearButtonClick }

PROCEDURE StartLANUSBServer;
{ Start the server programatically - but start it minimised or its window sits in front of the railway program window }
BEGIN
//  TRY
//    ShellExecute(WatchdogWindow.Handle,
//                 'open',
//                 '"C:\Program Files (x86)\LI-USB\LI-Server\LI-Server.exe"',
//                 NIL,
//                 NIL,
//                 SW_SHOWMINIMIZED);
//    REPEAT
//      Application.ProcessMessages;
//    UNTIL IsProgramRunning('LI-Server');
//  EXCEPT
//    ON E : Exception DO
//      ShowMessage('StartLANUSBServer: ' + E.ClassName +' error raised, with message: ' + E.Message);
//  END; {TRY}
END;

PROCEDURE StopLANUSBServer;
{ Stop the server programatically }
VAR
  AppHandle : THandle;
  OK : Boolean;

BEGIN
  TRY
    AppHandle := FindWindow(NIL, 'LI-Server'); //AppName);
    IF AppHandle <> 0 THEN
      OK := PostMessage(AppHandle, WM_QUIT, 0, 0)
    ELSE
      Exit;

    IF OK THEN
      WriteMemoText('G LAN-USB Server stopped programmatically')
    ELSE
      WriteMemoText('G LAN-USB Server failed to stop programmatically');
  EXCEPT
    ON E : Exception DO
      ShowMessage('StopLANUSBServer: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END;

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

PROCEDURE TWatchdogWindow.SendNotConnectedMsgToRailProgram;
{ Let the Rail program know if we can't start because we're not talking to the Lenz system }
CONST
  NotConnectedMsg = 'The Watchdog program cannot connect to the Lenz system';

VAR
  CopyData: TCopyDataStruct;
  ReceiverHandle : THandle;
  ReceiverTypeString : PWideChar;
  Res : Integer;

BEGIN
  ReceiverTypeString := 'TFWPRailWindow';
  ReceiverHandle := FindWindow(ReceiverTypeString, NIL);
  IF ReceiverHandle = 0 THEN BEGIN
    WriteMemoText('Cannot find Rail program so cannot warn it we''re not connected to the Lenz system');
    Exit;
  END;

  { We have found the watchdog program }
  WriteMemoText('Sending "' + NotConnectedMsg + '" message to Rail program');

  CopyData.lpData := PChar(NotConnectedMsg);
  CopyData.cbdata := Bytelength(NotConnectedMsg);
  CopyData.dwData := ReceiverHandle;

  Res := SendMessage(ReceiverHandle, WM_COPYDATA, Application.Handle, LPARAM(@CopyData));
  IF Res = 2 THEN
    WriteMemoText('FWPRail Watchdog has acknowledged the message')
  ELSE
    WriteMemoText('FWPRail Watchdog has not acknowledged the message');
END; { SendNotConnectedMsgToRailProgram }

PROCEDURE TWatchdogWindow.WatchdogTimerTick(Sender: TObject);
VAR
  ReceiverHandle : THandle;

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
        IF TCPSocket1 <> NIL THEN
          WatchdogWindow.DestroyTCPClients;
      END;

      Exit;
    END;

    IF NOT MessageReceivedFromFWPRail THEN BEGIN
      IF NOT FWPRailLenzOperationsStopped THEN BEGIN
        { Panic! }
        FWPRailLenzOperationsStopped := True;
        IF TCPSocket1 = NIL THEN BEGIN
          Beep;
          ShowMessage('FWPRail has stopped running - Lenz system would have been sent a "Stop Operations" command had the Watchdog been connected to it');
        END ELSE BEGIN
          WriteMemoText('Watchdog -> Lenz: Stop Operations');
          ResponsesTCPSendText('21-80-A1' + CRLF);
          TCPCommand.Clear;
          Beep;
          ShowMessage('FWPRail has stopped running - Lenz system has been sent a "Stop Operations" command');
        END;

        { We might have sent a message to FWPRail at this point, but if FWPRail has stopped (for whatever reason), using SendMessage will cause the watchdog to hang, as it
          waits for a response from the other end
        }
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

BEGIN
  SetString(S, PChar(Msg.CopyDataStruct.lpData), Msg.CopyDataStruct.cbData DIV SizeOf(Char));
  IF Pos('FWPRail is running', S) = 0 THEN
    WriteMemoText('Invalid message: "' + S)
  ELSE BEGIN
    IF FirstMessageReceivedFromFWPRail THEN BEGIN
      WriteMemoText('Msg from FWPRail received at ' + TimeToStr(Time));
      MessageReceivedFromFWPRail := True;;
    END ELSE BEGIN
      WriteMemoText('First msg from FWPRail received: ' + S);

      IF TCPSocket1 = NIL THEN BEGIN
        WriteMemoText('Creating TCP Client');
        WatchdogWindow.CreateTCPClients;
      END;

      FWPRailRunning := True;
      WatchdogTimer.Enabled := True;
      FirstMessageReceivedFromFWPRail := True;
      MessageReceivedFromFWPRail := True;
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
