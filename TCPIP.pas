UNIT TCPIP;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ScktComp, InitVars, ShellAPI, MiscUtils;

TYPE
  TTCPIPForm = CLASS(TForm)
    ClearButton: TButton;
    ConnectPanel: TPanel;
    EthernetConnectButton: TButton;
    IncomingGB: TGroupBox;
    LabelTextEntry: TLabel;
    MSGMemo: TMemo;
    SendButton: TSpeedButton;
    TCPCommand: TMemo;
    TCPIPTimer: TTimer;
    USBConnectButton: TButton;
    PROCEDURE ClearButtonClick(Sender: TObject);
    PROCEDURE EthernetConnectButtonClick(Sender: TObject);
    PROCEDURE SendButtonClick(Sender: TObject);
    PROCEDURE TCPIPFormClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE TCPIPFormShow(Sender: TObject);
    PROCEDURE TCPIPTimerOnTimer(Sender: TObject);
    PROCEDURE USBConnectButtonClick(Sender: TObject);

  PRIVATE

  PUBLIC
    PROCEDURE CreateTCPClients(Connection : LenzConnectionType);
    PROCEDURE DestroyTCPClients;

    PROCEDURE ResponsesTCPClientConnect(Sender: TObject; Socket1 : TCustomWinSocket);
    PROCEDURE ResponsesTCPClientDisconnect(Sender: TObject; Socket1 : TCustomWinSocket);
    PROCEDURE ResponsesTCPClientRead(Sender: TObject; Socket1 : TCustomWinSocket);
    PROCEDURE ResponsesTCPClientError(Sender: TObject; Socket1 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);

    PROCEDURE BroadcastsTCPClientConnect(Sender: TObject; Socket2 : TCustomWinSocket);
    PROCEDURE BroadcastsTCPClientDisconnect(Sender: TObject; Socket2 : TCustomWinSocket);
    PROCEDURE BroadcastsTCPClientRead(Sender: TObject; Socket2 : TCustomWinSocket);
    PROCEDURE BroadcastsTCPClientError(Sender: TObject; Socket2 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);

    PROCEDURE ResponsesTCPSendBuffer(Buffer : ARRAY OF Byte; BufferLen : Integer);
    PROCEDURE ResponsesTCPSendText(S : String);
  END;

FUNCTION ReadAndDeleteDataFromTCPIPList : String;
{ Return string from data-read-in list and delete it from the list }

FUNCTION ReadDataFromTCPIPList : String;
{ Return string from data-read-in list but do not delete it - used for testing }

PROCEDURE StartLANUSBServer;
{ Start the server programatically }

PROCEDURE StopLANUSBServer;
{ Stop the server programatically }

VAR
  BroadcastsTCPClient : TClientSocket = NIL;
  ConnectTS : Int64;
  DataReadInList : TStringList;
  ResponsesTCPClient : TClientSocket = NIL;
  TCPIPForm : TTCPIPForm;
  TCPBuf1 : String = '';
  TCPBuf2 : String = '';
  TCPSocket1 : TCustomWinSocket = NIL;
  TCPSocket2 : TCustomWinSocket = NIL;

IMPLEMENTATION

{$R *.dfm}

USES Lenz, Options;

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

PROCEDURE TTCPIPForm.USBConnectButtonClick(Sender: TObject);
BEGIN
  IF TCPSocket1 = NIL THEN
    CreateTCPClients(USBConnection)
  ELSE
    DestroyTCPClients;
END; { TCPConnectButtonClick }

PROCEDURE TTCPIPForm.EthernetConnectButtonClick(Sender: TObject);
BEGIN
  IF TCPSocket1 = NIL THEN
    CreateTCPClients(EthernetConnection)
  ELSE
    DestroyTCPClients;
END; { EthernetConnectButtonClick }

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

PROCEDURE TTCPIPForm.CreateTCPClients(Connection : LenzConnectionType);
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

  ResponsesTCPClient.Port           := 5550;
  IF Connection = USBConnection THEN
    ResponsesTCPClient.Address      := '127.0.0.1'
  ELSE
    ResponsesTCPClient.Address      := '192.168.0.200';

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

  IF Connection = USBConnection THEN BEGIN
    IF BroadcastsTCPClient = NIL THEN BEGIN
      BroadcastsTCPClient              := TClientSocket.Create(TCPIPForm);
      BroadcastsTCPClient.ClientType   := ctNonBlocking;
      BroadcastsTCPClient.OnConnect    := BroadcastsTCPClientConnect;
      BroadcastsTCPClient.OnDisconnect := BroadcastsTCPClientDisconnect;
      BroadcastsTCPClient.OnRead       := BroadcastsTCPClientRead;
      BroadcastsTCPClient.OnError      := BroadcastsTCPClientError;
    END;

    BroadcastsTCPClient.Port           := 5551;
    BroadcastsTCPClient.Address        := '127.0.0.1';

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
  Log('A& TCPClient 1 Connected');
  IF LenzConnection = EthernetConnection THEN BEGIN
    EthernetConnectButton.Enabled := True;
    EthernetConnectButton.Caption := 'TCP 1 Disconnect';
  END ELSE BEGIN
    USBConnectButton.Enabled := True;
    USBConnectButton.Caption := 'TCP 1 Disconnect';
  END;
END; { ResponsesTCPClientConnect }

PROCEDURE TTCPIPForm.BroadcastsTCPClientConnect(Sender: TObject; Socket2 : TCustomWinSocket);
BEGIN
  ConnectTS := GetTickCount;
  TCPSocket2 := Socket2;
  Log('A TCPClient 2 Connected');
  IF LenzConnection = EthernetConnection THEN BEGIN
    EthernetConnectButton.Enabled := True;
    EthernetConnectButton.Caption := 'TCP 2 Disconnect';
  END ELSE BEGIN
    USBConnectButton.Enabled := True;
    USBConnectButton.Caption := 'TCP 2 Disconnect';
  END;
END; { BroadcastsTCPClientConnect }

PROCEDURE TTCPIPForm.ResponsesTCPClientDisconnect(Sender: TObject; Socket1 : TCustomWinSocket);
BEGIN
  IF ResponsesTCPClient <> NIL THEN BEGIN
    MSGMemo.Lines.Add('*** 1 Disconnected' + CRLF);
    Log('A TCPClient 1 Disconnected');
    IF LenzConnection = EthernetConnection THEN BEGIN
      EthernetConnectButton.Enabled := True;
      EthernetConnectButton.Caption := 'TCP 1 Connect';
    END ELSE BEGIN
      USBConnectButton.Enabled := True;
      USBConnectButton.Caption := 'TCP 1 Connect';
    END;
    TCPSocket1 := NIL;
  END;
END; { ResponsesTCPClientDisconnect }

PROCEDURE TTCPIPForm.BroadcastsTCPClientDisconnect(Sender: TObject; Socket2 : TCustomWinSocket);
BEGIN
  IF BroadcastsTCPClient <> NIL THEN BEGIN
    MSGMemo.Lines.Add('*** 2 Disconnected' + CRLF);
    Log('A TCPClient 2 Disconnected');
    IF LenzConnection = EthernetConnection THEN BEGIN
      EthernetConnectButton.Enabled := True;
      EthernetConnectButton.Caption := 'TCP 2 Connect';
    END ELSE BEGIN
      USBConnectButton.Enabled := True;
      USBConnectButton.Caption := 'TCP 2 Connect';
    END;
    TCPSocket2 := NIL;

    SetSystemOffline('TCP Client has disconnected', SoundWarning);
  END;
END; { BroadcastsTCPClientDisconnect }

FUNCTION ReadAndDeleteDataFromTCPIPList : String;
{ Return string from data-read-in list and delete it from the list }
BEGIN
  IF DataReadInList.Count < 1 THEN
    Result := ''
  ELSE BEGIN
    Result := DataReadInList[0];
    DataReadInList.Delete(0);
  END;
END; { ReadAndDeleteDataFromTCPIPList }

FUNCTION ReadDataFromTCPIPList : String;
{ Return string from data-read-in list but do not delete it - used for testing }
BEGIN
  IF DataReadInList.Count < 1 THEN
    Result := ''
  ELSE
    Result := DataReadInList[0];
END; { ReadDataFromTCPIPList }

PROCEDURE TTCPIPForm.ResponsesTCPClientRead(Sender: TObject; Socket1 : TCustomWinSocket);
VAR
  AnsiStr : AnsiString;
  Broadcast : Boolean;
  Buffer : ARRAY[0..999] OF Byte;
  I : Integer;
  J : Integer;
  Response : Boolean;
  ResponseOrBroadcastSize : Integer;
  S : String;

  FUNCTION ByteToHex(InByte : Byte) : String;
  CONST
    Digits : ARRAY[0..15] OF Char='0123456789ABCDEF';

  BEGIN
    Result := Digits[InByte SHR 4] + Digits[InByte AND $0F];
  END; { ByteToHex }

BEGIN
  IF Socket1.Connected = True THEN BEGIN
    IF LenzConnection = USBConnection THEN BEGIN
      AnsiStr := Socket1.ReceiveText;
      S := String(AnsiStr);
      TCPBuf1 := TCPBuf1 + S;

      { If there's more than one response received in one go, strip off the carriage returns to create strings consisting of hex digits }
      WHILE Pos(CRLF, TCPBuf1) > 0 DO BEGIN
        I := Pos(CRLF, TCPBuf1);
        S := Copy(TCPBuf1, 1, I - 1);
        Delete(TCPBuf1, 1, I + 1);

        MSGMemo.Lines.Add('Response: ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);
        Log('+ Response: ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);

        { Put what's been read in at the top of the list, and note that it was a response to a request for data }
        DataReadInList.Add('R ' + S);
      END;
    END ELSE
      IF LenzConnection = EthernetConnection THEN BEGIN
        ResponseOrBroadcastSize := Socket1.ReceiveBuf(Buffer[0], 999);

        { Convert the bytes to a hex string so what we return matches what is returned when the USB Connection is used - but ignore the first two bytes as they are the
          header bytes, $FF $FE for responses and $FF $FD for broadcasts
        }
        S := '';
        FOR I := 0 TO ResponseOrBroadcastSize - 1 DO
          S := S + ByteToHex(Buffer[I]);

        TCPBuf1 :=  S;
        IF LogCurrentTimeMode THEN
          Log('X Data Received: ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S + ' {BlankLineBefore}');

        { If there's more than one response or broadcast received in one go, separate the data to create strings consisting of hex digits - bear in mind that the data may
          be a mixture of responses and broadcasts
        }
        WHILE (Pos('FFFE', TCPBuf1) = 1) OR (Pos('FFFD', TCPBuf1) = 1) DO BEGIN
          Response := False;
          Broadcast := False;

          I := Pos('FFFE', TCPBuf1);
          J := Pos('FFFD', TCPBuf1);
          IF I = 1 THEN
            Response := True
          ELSE
            IF J = 1 THEN
              Broadcast := True;

          { Remove the header bytes which are no longer useful }
          TCPBuf1 := Copy(TCPBuf1, 5);

          { Look for the next header bytes, if any }
          I := Pos('FFFE', TCPBuf1);
          J := Pos('FFFD', TCPBuf1);

          IF (I = 0) AND (J = 0) THEN
            { just process the rest of the string }
            S := TCPBuf1
          ELSE
            { extract the string up to the next header bytes }
            IF (J = 0) OR ((I > 0) AND (I < J)) THEN BEGIN
              { a $FF $FE comes first }
              S := Copy(TCPBuf1, 1, I - 1);
              TCPBuf1 := Copy(TCPBuf1, I);
            END ELSE BEGIN
              { a $FF $FD comes first }
              S := Copy(TCPBuf1, 1, J - 1);
              TCPBuf1 := Copy(TCPBuf1, J);
            END;

          { Put what's been read in at the top of the list, and note that it was a response to a request for data }
          IF Response THEN BEGIN
            DataReadInList.Add('R ' + S);
            MSGMemo.Lines.Add('Response: ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);
            Log('+ Response: ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);
          END ELSE
            IF Broadcast THEN BEGIN
              DataReadInList.Add('B ' + S);
              MSGMemo.Lines.Add('Broadcast: ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);
              Log('+ Broadcast: ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);
            END;
        END; {WHILE}
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
      MSGMemo.Lines.Add('Broadcast: ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);
      Log('+ Broadcast: ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);

      { Put what's been read in at the top of the list, and note that it was a broadcast }
      DataReadInList.Add('B ' + S);
    END;
  END;
END; { BroadcastsTCPClientRead }

PROCEDURE TTCPIPForm.ResponsesTCPClientError(Sender: TObject; Socket1 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);
BEGIN
  Log('A Unable to Connect: ' + SysErrorMessage(ErrorCode));
  MSGMemo.Lines.Add('*** Unable to Connect: ' + SysErrorMessage(ErrorCode));
  ErrorCode := 0;
END; { ResponsesTCPClientError }

PROCEDURE TTCPIPForm.BroadcastsTCPClientError(Sender: TObject; Socket2 : TCustomWinSocket; ErrorEvent: TErrorEvent; VAR ErrorCode: Integer);
BEGIN
  Log('A Unable to Connect: ' + SysErrorMessage(ErrorCode));
  MSGMemo.Lines.Add('*** Unable to Connect: ' + SysErrorMessage(ErrorCode));
  ErrorCode := 0;
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
    Log('+ OUT1 *** ' + S);
    MSGMemo.Lines.Add('OUT1 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);
  END;
END; { ResponsesTCPSendText }

PROCEDURE TTCPIPForm.ResponsesTCPSendBuffer(Buffer : ARRAY OF Byte; BufferLen : Integer);
VAR
  I : Integer;
  S : String;

BEGIN
  IF TCPSocket1 <> NIL THEN BEGIN
    TCPSocket1.SendBuf(Buffer[0], BufferLen);

    S := '';
    FOR I := 0 TO BufferLen - 1 DO
      S := S + IntToStr(Buffer[I]) + ' ';
    Log('+ OUT1 *** ' + S);
    MSGMemo.Lines.Add('OUT1 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : ' + S);
  END;
END; { ResponsesTCPSendText }

PROCEDURE TTCPIPForm.TCPIPTimerOnTimer(Sender: TObject);
{ Send something every 500 miliseconds or the TCPIP ports time out }
BEGIN
  IF LenzConnection = EthernetConnection THEN
    { stop any further ticks }
    TCPIPTimer.Enabled := False
  ELSE BEGIN
    IF TCPSocket1 <> NIL THEN BEGIN
      MSGMemo.Lines.Add('OUT1 ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : Sending Watchdog');
      Log('A OUT1 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : Sending Watchdog');
      TCPSocket1.SendText(' ');
    END;

    IF TCPSocket2 <> NIL THEN BEGIN
      MSGMemo.Lines.Add('OUT2 ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : Sending Watchdog');
      Log('A OUT2 *** ' + FillSpace(IntToStr(GetTickCount - ConnectTS), 8) + 'ms : Sending Watchdog');
      TCPSocket2.SendText(' ');
    END;
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
      ShowMessage('StartLANUSBServer: ' + E.ClassName + ' error raised, with message: ' + E.Message);
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
      ShowMessage('StopLANUSBServer: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END;

INITIALIZATION
  DataReadInList := TStringList.Create;

END { TCPIP }.
