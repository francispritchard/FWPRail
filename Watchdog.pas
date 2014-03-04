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
    WatchdogTimer: TTimer;
    MSGMemo: TMemo;
    SendStatusRequestButton: TSpeedButton;
    SendTextButton: TSpeedButton;
    TCPCommand: TMemo;
    TCPConnectButton: TButton;
    TCPIPTimer: TTimer;
    WatchdogListBox: TListBox;
    PROCEDURE ClearButtonClick(Sender: TObject);
    PROCEDURE SendStatusRequestButtonClick(Sender: TObject);
    PROCEDURE SendTextButtonClick(Sender: TObject);
    PROCEDURE TCPConnectButtonClick(Sender: TObject);
    PROCEDURE TCPIPTimerOnTimer(Sender: TObject);
    PROCEDURE WatchdogTimerTick(Sender: TObject);
    PROCEDURE WatchdogWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE WatchdogWindowShow(Sender: TObject);

  PRIVATE
    PROCEDURE WMCopyData(VAR Msg : TWMCopyData); Message WM_COPYDATA;
    { Receives data from FWPRail }

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

//    PROCEDURE SendDataToRailProgram(CONST copyDataStruct: TCopyDataStruct);
    PROCEDURE SendStringToRailProgram(S : String);
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
  FWPRailRunning : Boolean = False;
  ResponsesTCPClient : TClientSocket = NIL;
  TCPIPConnected : Boolean = True;
  WatchdogWindow : TWatchdogWindow;
  TCPBuf1 : String = '';
  TCPBuf2 : String = '';
  TCPSocket1 : TCustomWinSocket = NIL;
  TCPSocket2 : TCustomWinSocket = NIL;

IMPLEMENTATION

{$R *.dfm}

VAR
  StartupFlag : Boolean = True;
  UnitRef : String = 'Watchdog';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
//  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

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
    MSGMemo.Text := MSGMemo.Text + 'OUT ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : [System Status Request]' + CRLF;
    Log('G OUT ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : [System Status Request]' + CRLF);
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

    MSGMemo.Text := MSGMemo.Text + 'OUT ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S + CRLF;
    Log('G OUT ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S + CRLF);
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
    MSGMemo.Lines.Add('*** Unable to Connect to Client 1');
    Log('G *** Unable to Connect to Client 1');
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
    MSGMemo.Lines.Add('*** Unable to Connect to Client 2');
    Log('G *** Unable to Connect to Client 2');
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
  MSGMemo.Lines.Add('*** 5550 Connected');
  Log('G TCPClient 1 Connected');
  TCPConnectButton.Enabled := True;
  TCPConnectButton.Caption := 'TCP 1 Disconnect';
END; { ResponsesTCPClientConnect }

PROCEDURE TWatchdogWindow.BroadcastsTCPClientConnect(Sender: TObject; Socket2 : TCustomWinSocket);
BEGIN
  ConnectTS := GetTickCount;
  TCPSocket2 := Socket2;
  MSGMemo.Lines.Add('*** 5551 Connected');
  Log('G TCPClient 2 Connected');
  TCPConnectButton.Enabled := True;
  TCPConnectButton.Caption := 'TCP 2 Disconnect';
END; { BroadcastsTCPClientConnect }

PROCEDURE TWatchdogWindow.ResponsesTCPClientDisconnect(Sender: TObject; Socket1 : TCustomWinSocket);
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    MSGMemo.Lines.Add('*** 5551 Disconnected' + CRLF);
    Log('G TCPClient 1 Disconnected' + CRLF);
    TCPConnectButton.Enabled := True;
    TCPConnectButton.Caption := 'TCP 1 Connect';
    TCPSocket1 := NIL;
  END;
END; { ResponsesTCPClientDisconnect }

PROCEDURE TWatchdogWindow.BroadcastsTCPClientDisconnect(Sender: TObject; Socket2 : TCustomWinSocket);
BEGIN
  IF BroadcastsTCPClient <> NIL THEN BEGIN
    MSGMemo.Lines.Add('*** 5551 Disconnected' + CRLF);
    Log('G TCPClient 2 Disconnected' + CRLF);
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

    { strip off the carriage returns }
    WHILE Pos(CRLF, TCPBuf1) > 0 DO BEGIN
      I := Pos(CRLF, TCPBuf1);
      S := Copy(TCPBuf1, 1, I - 1);
      Delete(TCPBuf1, 1, I + 1);
      MSGMemo.Lines.Add('IN1  ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);
      Log('+ IN1 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);

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
      MSGMemo.Lines.Add('IN2  ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);
      Log('+ IN2 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);

      { Put what's been read in at the top of the list, and note that it was a broadcast }
      DataReadInList.Add('B ' + S);
    END;
  END;
END; { BroadcastsTCPClientRead }

PROCEDURE TWatchdogWindow.ResponsesTCPClientError(Sender: TObject; Socket1 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);
BEGIN
  IF ErrorCode = 10061 THEN BEGIN
    MSGMemo.Lines.Add('*** Unable to Connect');
    Log('G *** Unable to Connect');
    ErrorCode := 0;
    TCPIPConnected := False;
  END;

  IF ErrorCode = 10053 THEN BEGIN
    MSGMemo.Lines.Add('*** Server has disconnected/shutdown.');
    Log('G *** Server has disconnected/shutdown.');
    ErrorCode := 0;
    TCPIPConnected := False;
  END;
END; { ResponsesTCPClientError }

PROCEDURE TWatchdogWindow.BroadcastsTCPClientError(Sender: TObject; Socket2 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);
BEGIN
 TRY
  IF ErrorCode = 10061 THEN BEGIN
    MSGMemo.Lines.Add('*** Unable to Connect');
    Log('G *** Unable to Connect');
    ErrorCode := 0;
    TCPIPConnected := False;
  END;

  IF ErrorCode = 10053 THEN BEGIN
    MSGMemo.Lines.Add('*** Server has disconnected/shutdown.');
    Log('G *** Server has disconnected/shutdown.');
    ErrorCode := 0;
    TCPIPConnected := False;
  END;

 EXCEPT
    MSGMemo.Lines.Add('*** Unable to Connect to Client 1');
    Log('G *** Unable to Connect to Client 1');
  END;
END; { BroadcastsTCPClientError }

PROCEDURE TWatchdogWindow.ResponsesTCPSendText(S : String);
VAR
  AnsiStr : AnsiString;

BEGIN
  AnsiStr := AnsiString(S);
  IF TCPSocket1 <> NIL THEN BEGIN
    TCPSocket1.SendText(AnsiStr + CRLF);
    Log('+ OUT1 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + ' ' + S);
  END;
END; { ResponsesTCPSendText }

PROCEDURE TWatchdogWindow.TCPIPTimerOnTimer(Sender: TOBJECT);
{ Send something every 500 miliseconds or the TCPIP ports time out }
BEGIN
  IF TCPSocket1 <> NIL THEN BEGIN
    MSGMemo.Lines.Add('OUT1 ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : Sending Watchdog');
    Log('+ OUT1 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : Sending Watchdog');
    TCPSocket1.SendText(' ');
  END;

  IF TCPSocket2 <> NIL THEN BEGIN
    MSGMemo.Lines.Add('OUT2 ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : Sending Watchdog');
    Log('+ OUT2 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : Sending Watchdog');
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
      Log('G LAN-USB Server stopped programmatically')
    ELSE
      Log('G LAN-USB Server failed to stop programmatically');
  EXCEPT
    ON E : Exception DO
      ShowMessage('StopLANUSBServer: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END;

PROCEDURE TWatchdogWindow.SendStringToRailProgram(S : String);
VAR
  copyData: TCopyDataStruct;
  HH : Word;
  MM : Word;
  MS : Word;
  ReceiverHandle : THandle;
  ReceiverTypeString : PWideChar;
  Res : Integer;
  SS : Word;
  TimeStr : String;

BEGIN
  ReceiverTypeString := 'TFWPRailMainWindow';
  ReceiverHandle := FindWindow(ReceiverTypeString, NIL);
  DecodeTime(Time, HH, MM, SS, MS);
  TimeStr := IntToStr(HH) + ':' + IntToStr(MM) + ':' + IntToStr(SS);
  IF ReceiverHandle = 0 THEN BEGIN
    MSGMemo.Text := MSGMemo.Text + 'OUT ' + FillSpace(TimeStr, 8) + ' ' + ReceiverTypeString + ' not found!' + CRLF;
    Exit;
  END;

  S := TimeStr + ': ' + S;
  CopyData.lpData := PChar(S);
  CopyData.cbdata := Bytelength(S);
  CopyData.dwData := ReceiverHandle;

  Res := SendMessage(ReceiverHandle, WM_COPYDATA, Application.Handle, LPARAM(@CopyData));
END; { SendDataToRailProgram }

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

PROCEDURE TWatchdogWindow.WatchdogTimerTick(Sender: TObject);
VAR
  ReceiverHandle : THandle;
  ReceiverTypeString : PWideChar;

BEGIN
  IF WatchdogTimer.Interval = 1 THEN BEGIN
    { start off with it set to 1 to make the timer tick immediately, then set it to a more reasonable interval }
    WatchdogTimer.Interval := 5000;

    { and list all the current windows - this is only really needed for debugging (to set the Receiver Strings in this and FWPRail), but no harm in having it each time
      we run
    }
    EnumWindows(@EnumWindowsProc, LPARAM(WatchdogListBox));
  END;

  { At each tick we check if FWPRail is running, and if so whether the Lenz LAN/USB server is running. If they both are then we expect a message every ten seconds. If we
    do not receive one we assume that FWPRail has crashed or otherwise stopped and we put the Lenz system into power-off mode.
  }
  ReceiverTypeString := 'TFWPRailMainWindow';
  ReceiverHandle := FindWindow(ReceiverTypeString, NIL);
  IF ReceiverHandle = 0 THEN BEGIN
    { we can't find FWPRail - if it was running it's presumably been closed down, so stop close our connection to the LAN/USB Server }
    IF FWPRailRunning THEN BEGIN
      FWPRailRunning := False;
      IF TCPSocket1 <> NIL THEN
        WatchdogWindow.DestroyTCPClients;
    END;

    Exit;
  END;

  { FWPRail is running, so see if the LAN/USN Server is running }
  IF TCPSocket1 = NIL THEN
    WatchdogWindow.CreateTCPClients;

  IF TCPSocket1 = NIL THEN BEGIN
    ShowMessage('Unable to connect to the LAN/USB server');
    Exit;
  END;

  IF NOT FWPRailRunning THEN BEGIN
    { Panic! }
    IF ResponsesTCPClient <> NIL THEN BEGIN
      MSGMemo.Text := MSGMemo.Text + 'OUT ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : [Stop Operations]' + CRLF;
      Log('G OUT ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : [Stop Operations]' + CRLF);
      ResponsesTCPSendText('21-80-A1' + CRLF);
      TCPCommand.Clear;
    END;

    ShowMessage('FWPRail has stopped running - Lenz system has been sent a "Stop Operations" command');

    { and send a message to FWPRail just in case! }
    SendStringToRailProgram('Hello FWPRail! Lenz system has been sent a "Stop Operations" command');
  END;

  { and set the running flag to false - it should be set to true by the incoming message before the watchdog timer ticks again }
  FWPRailRunning := False;
END; { WatchdogTimerTick }

PROCEDURE TWatchdogWindow.WMCopyData(VAR Msg: TWMCopyData);
{ Receives data from FWPRail }
VAR
  S : String;

BEGIN
  FWPRailRunning := False;

  SetString(S, PChar(Msg.CopyDataStruct.lpData), Msg.CopyDataStruct.cbData DIV SizeOf(Char));
  IF Pos('FWPRail is running', S) = 0 THEN
    ShowMessage('Invalid message "' + S + '" received from ')
  ELSE
    FWPRailRunning := True;
  { And send an acknowledgment }
  Msg.Result := 1234;
END; { WMCopyData }

INITIALIZATION
  DataReadInList := TStringList.Create;

END { Watchdog }.
