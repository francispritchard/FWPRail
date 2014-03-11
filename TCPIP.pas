UNIT TCPIP;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ScktComp, InitVars, ShellAPI, MiscUtils;

TYPE
  TTCPIPForm = CLASS(TForm)
    ClearButton: TButton;
    ConnectPanel: TPanel;
    IncomingGB: TGroupBox;
    LabelConnectTo: TLABEL;
    LabelTextEntry: TLabel;
    MSGMemo: TMemo;
    PortEdit: TEdit;
    SendButton: TSpeedButton;
    TCPAddress: TEdit;
    TCPCommand: TMemo;
    TCPConnectButton: TButton;
    TCPIPTimer: TTimer;
    PROCEDURE ClearButtonClick(Sender: TObject);
    PROCEDURE TCPIPFormClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE TCPIPFormShow(Sender: TObject);
    PROCEDURE SendButtonClick(Sender: TObject);
    PROCEDURE TCPConnectButtonClick(Sender: TObject);
    PROCEDURE TCPIPTimerOnTimer(Sender: TObject);

  PRIVATE

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
  ResponsesTCPClient : TClientSocket = NIL;
  TCPIPConnected : Boolean = True;
  TCPIPForm : TTCPIPForm;
  TCPBuf1 : String = '';
  TCPBuf2 : String = '';
  TCPSocket1 : TCustomWinSocket = NIL;
  TCPSocket2 : TCustomWinSocket = NIL;

IMPLEMENTATION

{$R *.dfm}

USES Lenz;

VAR
  UnitRef : String = 'TCPIP';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION FillSpace(S : WideString; Len : Integer) : WideString;
BEGIN
  WHILE Length(S) < Len DO
    S := ' ' + S;
  Result := S;
END; { FillSpace }

PROCEDURE TTCPIPForm.TCPIPFormClose(Sender: TObject; VAR Action: TCloseAction);
BEGIN
  IF ResponsesTCPClient <> NIL THEN
    DestroyTCPClients;
END; { TCPIPFormClose }

PROCEDURE TTCPIPForm.TCPConnectButtonClick(Sender: TObject);
BEGIN
  IF TCPSocket1 = NIL THEN
    CreateTCPClients
  ELSE
    DestroyTCPClients;
END; { TCPConnectButtonClick }

PROCEDURE TTCPIPForm.TCPIPFormShow(Sender: TObject);
BEGIN
  SetBounds((Monitor.Left + Monitor.Width) - Width, Monitor.Top, Width, Height);
END; { TCPIPFormShow }

PROCEDURE TTCPIPForm.SendButtonClick(Sender: TObject);
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
    Log('A OUT ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S + CRLF);
    ResponsesTCPSendText(TCPCommand.Text);
    TCPCommand.Clear;
  END;
END; { SendButtonClick }

PROCEDURE TTCPIPForm.CreateTCPClients;
{ Set the TCPIP clients up }
VAR
  I : Integer;

BEGIN
  IF ResponsesTCPClient = NIL THEN BEGIN
    ResponsesTCPClient              := TClientSocket.Create(TCPIPForm);
    ResponsesTCPClient.ClientType   := ctNonBlocking;
    ResponsesTCPClient.OnConnect    := ResponsesTCPClientConnect;
    ResponsesTCPClient.OnDisconnect := ResponsesTCPClientDisconnect;
    ResponsesTCPClient.OnRead       := ResponsesTCPClientRead;
    ResponsesTCPClient.OnError      := ResponsesTCPClientError;
  END;

  ResponsesTCPClient.Port         := 5550;
  ResponsesTCPClient.Address      := '127.0.0.1'; // TCPAddress.Text;

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
    Log('A *** Unable to Connect to Client 1');
  END;

  IF BroadcastsTCPClient = NIL THEN BEGIN
    BroadcastsTCPClient              := TClientSocket.Create(TCPIPForm);
    BroadcastsTCPClient.ClientType   := ctNonBlocking;
    BroadcastsTCPClient.OnConnect    := BroadcastsTCPClientConnect;
    BroadcastsTCPClient.OnDisconnect := BroadcastsTCPClientDisconnect;
    BroadcastsTCPClient.OnRead       := BroadcastsTCPClientRead;
    BroadcastsTCPClient.OnError      := BroadcastsTCPClientError;
  END;

  BroadcastsTCPClient.Port         := 5551;
  BroadcastsTCPClient.Address      := '127.0.0.1'; // TCPAddress.Text;

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
    Log('A *** Unable to Connect to Client 2');
  END;
END; { CreateTCPClients }

PROCEDURE TTCPIPForm.DestroyTCPClients;
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

PROCEDURE TTCPIPForm.ResponsesTCPClientConnect(Sender: TObject; Socket1 : TCustomWinSocket);
BEGIN
  ConnectTS := GetTickCount;
  TCPSocket1 := Socket1;
  MSGMemo.Lines.Add('*** 1 Connected');
  Log('A TCPClient 1 Connected');
  TCPConnectButton.Enabled := True;
  TCPConnectButton.Caption := 'TCP 1 Disconnect';
END; { ResponsesTCPClientConnect }

PROCEDURE TTCPIPForm.BroadcastsTCPClientConnect(Sender: TObject; Socket2 : TCustomWinSocket);
BEGIN
  ConnectTS := GetTickCount;
  TCPSocket2 := Socket2;
  Log('A TCPClient 2 Connected');
  TCPConnectButton.Enabled := True;
  TCPConnectButton.Caption := 'TCP 2 Disconnect';
END; { BroadcastsTCPClientConnect }

PROCEDURE TTCPIPForm.ResponsesTCPClientDisconnect(Sender: TObject; Socket1 : TCustomWinSocket);
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    MSGMemo.Lines.Add('*** 1 Disconnected' + CRLF);
    Log('A TCPClient 1 Disconnected' + CRLF);
    TCPConnectButton.Enabled := True;
    TCPConnectButton.Caption := 'TCP 1 Connect';
    TCPSocket1 := NIL;
  END;
END; { ResponsesTCPClientDisconnect }

PROCEDURE TTCPIPForm.BroadcastsTCPClientDisconnect(Sender: TObject; Socket2 : TCustomWinSocket);
BEGIN
  IF BroadcastsTCPClient <> NIL THEN BEGIN
    MSGMemo.Lines.Add('*** 2 Disconnected' + CRLF);
    Log('A TCPClient 2 Disconnected' + CRLF);
    TCPConnectButton.Enabled := True;
    TCPConnectButton.Caption := 'TCP 2 Connect';
    TCPSocket2 := NIL;

    SetSystemOffline('TCP Client has disconnected');
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

PROCEDURE TTCPIPForm.ResponsesTCPClientRead(Sender: TObject; Socket1 : TCustomWinSocket);
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

PROCEDURE TTCPIPForm.BroadcastsTCPClientRead(Sender: TObject; Socket2 : TCustomWinSocket);
VAR
  AnsiStr : AnsiString;
  I : Integer;
  S : String;

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

PROCEDURE TTCPIPForm.ResponsesTCPClientError(Sender: TObject; Socket1 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);
BEGIN
  IF ErrorCode = 10061 THEN BEGIN
    MSGMemo.Lines.Add('*** Unable to Connect');
    Log('A *** Unable to Connect');
    //('G *** Unable to Connect');
    ErrorCode := 0;
    TCPIPConnected := False;
  END;

  IF ErrorCode = 10053 THEN BEGIN
    MSGMemo.Lines.Add('*** Server has disconnected/shutdown.');
    Log('A *** Server has disconnected/shutdown.');
    ErrorCode := 0;
    TCPIPConnected := False;
  END;
END; { ResponsesTCPClientError }

PROCEDURE TTCPIPForm.BroadcastsTCPClientError(Sender: TObject; Socket2 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);
BEGIN
 TRY
  IF ErrorCode = 10061 THEN BEGIN
    MSGMemo.Lines.Add('*** Unable to Connect');
    Log('A *** Unable to Connect');
    ErrorCode := 0;
    TCPIPConnected := False;
  END;

  IF ErrorCode = 10053 THEN BEGIN
    MSGMemo.Lines.Add('*** Server has disconnected/shutdown.');
    Log('A *** Server has disconnected/shutdown.');
    ErrorCode := 0;
    TCPIPConnected := False;
  END;

 EXCEPT
    MSGMemo.Lines.Add('*** Unable to Connect to Client 1');
    Log('A *** Unable to Connect to Client 1');
  END;
END; { BroadcastsTCPClientError }

PROCEDURE TTCPIPForm.ResponsesTCPSendText(S : String);
CONST
  CRLF = #13#10;

VAR
  AnsiStr : AnsiString;

BEGIN
  AnsiStr := AnsiString(S);
  IF TCPSocket1 <> NIL THEN BEGIN
    TCPSocket1.SendText(AnsiStr + CRLF);
    Log('+ OUT1 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + ' ' + S);
  END;
END; { ResponsesTCPSendText }

PROCEDURE TTCPIPForm.TCPIPTimerOnTimer(Sender: TOBJECT);
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

PROCEDURE TTCPIPForm.ClearButtonClick(Sender: TObject);
BEGIN
  MSGMemo.Clear;
END; { ClearButtonClick }

PROCEDURE StartLANUSBServer;
{ Start the server programatically - but start it minimised or its window sits in front of the railway program window }
BEGIN
  TRY
    ShellExecute(TCPIPForm.Handle,
                 'open',
                 '"C:\Program Files (x86)\LI-USB\LI-Server\LI-Server.exe"',
                 NIL,
                 NIL,
                 SW_SHOWMINIMIZED);
    REPEAT
      Application.ProcessMessages;
    UNTIL IsProgramRunning('LI-Server');
  EXCEPT
    ON E : Exception DO
      ShowMessage('StartLANUSBServer: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
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
      Log('A Lan-USB Server stopped programmatically')
    ELSE
      Log('A Lan-USB Server failed to stop programmatically');
  EXCEPT
    ON E : Exception DO
      ShowMessage('StopLANUSBServer: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END;

INITIALIZATION
  DataReadInList := TStringList.Create;

END { TCPIP }.
