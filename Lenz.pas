UNIT Lenz;
{ Unit that talks to a Lenz Digital Plus LI101F interface via a serial port or to an LI-USB interface

  Copyright © F.W. Pritchard 1998-2014. All Rights Reserved.
  (CheckSum subroutine conceived by A.M. Stokes, implemented by FWP).

  v0.2  02/08/99 serial unit found on web did not work - look for another one!
  v0.3  04/08/99 emergency stop works
  v0.4  08/08/99 first (non-working!) version of controlling loco
  v0.5  10/08/99 controlling loco works
  v0.55 12/08/99 controlling loco works better!
  v0.6  13/08/99 feedback works
  v0.7  14/08/99 setting points/signals works
  v0.8  21/08/99 using MarshallSoft's serial unit (shareware version)
  v0.9  21/08/99 changed to a unit; chunks exported to calling program
  v1.0  23/08/99 changed the way data read in is processed
  v1.1  26/08/99 add double slip commands - get undocumented reply 198
  v1.2  23/09/99 introduce locorec, and rewrite getting of unexpected data
  v1.3  11/12/99 add time out to try to catch Lenz unit being switched off
  v1.4  01/01/00 added to main Rail program for first time
  v1.5  22/02/00 added more control of timeouts
  v1.6  14/05/00 signals now reset when passed
  v2.0  29/06/02 revised for use with new LI100F interface
  v2.1  14/07/02 made LI100F work (set up DTR and DTS) with AMS's help
  v3.0  03/08/03 revised for use & worked for the first time with Delphi 6
  v4.0  11/11/04 added read computer interface version command for use with LI101F
  v4.1  26/09/05 now compiled by Delphi2005

  To remind FWP: OR sets bits; AND NOT resets them; XOR swaps values.
  IF (B and 96) = 96 (0110 0000) THEN - checks if bits 6 and 5 are set
}

INTERFACE

USES Initvars, Forms, ExtCtrls, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Dialogs, DB, ADODB, DBCtrls, Grids, DBGrids, StdCtrls, ComCtrls,
     TCPIP;

TYPE
  TLenzWindow = CLASS(TForm)
    PROCEDURE OnLenzOneMilliSecondTimerInterval(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  LenzWindow: TLenzWindow;

TYPE
  SystemRec = RECORD
                EmergencyStop : Boolean;
                EmergencyOff : Boolean;
                ProgrammingMode : Boolean;
                StartMode : Boolean;
              END;

FUNCTION AdjustLenzSpeed(LocoChip, DoubleHeaderLocoChip : Integer; Value : Integer; TrainDirection : DirectionType; OUT OK : Boolean) : Integer;
{ Increase or decrease the speed by given amount }

PROCEDURE DoCheckForUnexpectedData(UnitRef : String; CallingStr : String);
{ See if any has arrived }

PROCEDURE EmergencyDeselectPoint(P : Integer; VAR OK : Boolean);
{ for use in emergency, when a point remains energised }

PROCEDURE EmergencyDeselectSignal(S : Integer; VAR OK : Boolean);
{ for use in emergency, when a signal remains energised }

PROCEDURE GetInitialFeedback;
{ Read in the feedback before we start, or after an interruption }

PROCEDURE GetLocoFunctions(LocoChip : Integer; ForceRead : Boolean; VAR FunctionArray : ARRAY OF Boolean; VAR OK : Boolean);
{ Read all the functions }

FUNCTION GetLenzSpeed(LocoChip : Integer; ForceRead : Boolean) : Integer;
{ Returns the given loco's speed }

PROCEDURE InitialiseLenzUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

FUNCTION LocoHasBeenTakenOverByProgram(LocoChip : Integer) : Boolean;
{ Returns true if the loco has been taken over by the program }

FUNCTION LocoHasBeenTakenOverByUser(LocoChip : Integer) : Boolean;
{ Returns true if the loco has been taken over by an LH100 }

FUNCTION MakePointChange(LocoChip : Integer; P : Integer; Direction : PointStateType; VAR Count : Integer) : Boolean;
{ Select which decoder, which output and which direction (LS100/100 decoders have four outputs. Need to select then deselect after 200 ms! }

PROCEDURE MakeSemaphoreSignalChange(LocoChip, S, AccessoryAddress : Integer; OnOrOff : SignalStateType);
{ Pull a semaphore signal or or off using TrainTech's SC3 }

PROCEDURE ProgramOnTheMain(LocoChip : Integer; ProgramOnTheMainRequest : ProgramOnTheMainType; NewValue : Integer);
{ Program a loco anywhere on the layout (i.e. not on the programming track) }

PROCEDURE ReadInData{3}; Overload;

FUNCTION RequestProgrammingModeCV(CV : Integer) : String;
{ Ask for programming mode (aka service mode) data }

PROCEDURE ResumeOperations(OUT OK : Boolean);
{ Turns the power back on }

FUNCTION ReturnFeedbackData(UnitNum : Byte; Input : Integer) : Boolean;
{ Pass data from the feedback array back }

PROCEDURE ReturnSystemStatus(VAR S : SystemRec);
{ Return the present System status }

PROCEDURE SetLenzSpeed(LocoChip, DoubleHeaderLocoChip : Integer; LenzSpeed : Integer; TrainDirection : DirectionType; QuickStopFlag : Boolean; VAR OK : Boolean);
{ Sets the speed by changing bits 4 - 0 - if quickstop, don't bother to read in the loco details first, as we're not interested in what the speed used to be }

PROCEDURE SetSignalFunction(LocoChip, S : Integer);
{ Set a numbered function on or off - used for LED signals controlled by LF100XF function only decoders }

PROCEDURE SetSignalRouteFunction(LocoChip, S: Integer);
{ Set a numbered function on or off - used for LED signals controlled by LF100XF function only decoders }

PROCEDURE SetSingleLocoFunction(LocoChip, FunctionNum : Integer; TurnOn : Boolean; OUT OK : Boolean);
{ Set a numbered function on or off }

PROCEDURE SetTrainDirection(T : Train; DirectionRequired : DirectionType; ForceWrite : Boolean; VAR OK : Boolean);
{ Sets/resets bit 7 - up is (arbitrarily) on }

FUNCTION SingleLocoFunctionIsOn(LocoChip, FunctionNum : Integer; ForceRead : Boolean; OUT OK : Boolean) : Boolean;
{ Read whether a numbered function is on or off }

PROCEDURE StopAllLocomotives(VAR OK : Boolean);
{ Emergency stops all trains, but leaves the power on }

PROCEDURE StopAParticularLocomotive(LocoChip : Integer; VAR OK : Boolean);
{ Stops a particular loco }

PROCEDURE StopOperations;
{ Turns the power off to the track and to I/O devices; it tells the system to stop sending DCC packets to the track and to switch off the DCC track power. }

PROCEDURE TurnPointOff(P : Integer; VAR OK : Boolean);
{ Deselect a given point }

IMPLEMENTATION

{$R *.dfm}

USES RailDraw, Feedback, GetTime, Startup, MiscUtils, Diagrams, LocoUtils, IDGlobal, Movement, MMSystem, DateUtils, StrUtils, Input;

CONST
  UnitRef = 'Lenz';
  ReservedStr = 'Reserved';
  FeedbackEncoderStr = 'a feedback encoder';
  AccessoryDecoderWithFeedbackStr = 'an accessory decoder with feedback';
  AccessoryDecoderWithoutFeedbackStr = 'an accessory decoder without feedback';

CONST
  BinaryCounter : ARRAY [0..4] OF Integer = (0, 1, 2, 4, 8);
  ExitProgram = True;
  SpeedStep28 = 2; { binary 10 = 28 steps }
  SpeedStep28Table : ARRAY [0..28] OF Word =
    (0,    { Step 0 = 00000 }
     2,    { Step 1 = 00010 }
     18,   { Step 2 = 10010 }
     3,    { Step 3 = 00011 }
     19,   { Step 4 = 10011 }
     4,    { Step 5 = 00100 }
     20,   { Step 6 = 10100 }
     5,    { Step 7 = 00101 }
     21,   { Step 8 = 10101 }
     6,    { Step 9 = 00110 }
     22,   { Step 10 = 10110 }
     7,    { Step 11 = 00111 }
     23,   { Step 12 = 10111 }
     8,    { Step 13 = 01000 }
     24,   { Step 14 = 11000 }
     9,    { Step 15 = 01001 }
     25,   { Step 16 = 11001 }
     10,   { Step 17 = 01010 }
     26,   { Step 18 = 11010 }
     11,   { Step 19 = 01011 }
     27,   { Step 20 = 11011 }
     12,   { Step 21 = 01100 }
     28,   { Step 22 = 11100 }
     13,   { Step 23 = 01101 }
     29,   { Step 24 = 11101 }
     14,   { Step 25 = 01110 }
     30,   { Step 26 = 11110 }
     15,   { Step 27 = 01111 }
     31);  { Step 28 = 11111 }

  LocoMaxSpeed = 28;
  LocoMinSpeed = 0;
  Input1 = 1;
  Input8 = 8;
  ReadArrayLen = 16; { should be 15, but get range-check errors - test *** }

TYPE
  ReplyType = (Acknowledgment, LocoAcknowledgment, PointAcknowledgment, SignalAcknowledgment, CommandStationSoftwareReply, ComputerInterfaceSoftwareReply,
               EmergencyStopReply, EverythingTurnedOnReply, FeedbackReply, LocoReply, LocoTakenoverReply, PointReply, ProgrammingModeReply, SystemStatusReply,
               TrackPowerOffReply, NoReplyExpected);
VAR
  ExpectedFeedbackAddress : Byte = 0;
  FeedbackArray : ARRAY [0..127, 0..1] OF Byte; { needs to store upper and lower nibble of data }
  FeedbackDataArray : ARRAY [1..LastFeedbackUnit + 1, Input1..Input8] OF Boolean;
  FirstByteErrorMsgWritten : Boolean = False;
  ReadArray, WriteArray : ARRAY [0..ReadArrayLen] OF Byte;
  SaveTimeCTSLastFoundSet : Cardinal = 0;
  SaveTimeLastDataReceived : Cardinal = 0;
  ShowEmergencyOffMessageVisible : Boolean = False;
  ShowEmergencyStopMessageVisible : Boolean = False;
  SystemStatus : SystemRec;
  TimeCTSLastFoundSet : Cardinal = 0;
  TimeLastUSBDataWritten : Cardinal = 0;
  UnrequestedDataFound : Boolean = False;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' <Unit=' + UnitRef + '>');
END; { Log }

FUNCTION GetLocoChipHighByte(LocoChip : Word) : Byte;
{ Returns the high byte derived from the loco number }
BEGIN
  IF LocoChip < 100 THEN
    Result := 0
  ELSE BEGIN
    { set the top two bits }
    LocoChip := LocoChip OR $C000;
    { and AMS's way of getting the high byte }
    Result := LocoChip shr 8;
  END;
END; { GetLocoChipHighByte }

FUNCTION GetLocoChipLowByte(LocoChip : Integer) : Byte;
{ Returns the low byte derived from the loco number }
BEGIN
  IF LocoChip < 100 THEN
    Result := LocoChip
  ELSE
    { and AMS' way of getting the low byte }
    Result := LocoChip AND 255;
END; { GetLocoChipLowByte }

FUNCTION GetLocoChipFromTwoBytes(HighByte, LowByte : Byte) : Word;
{ Given two bytes, return a loco number }
VAR
  TempLocoChip : Integer;

BEGIN
  IF (HighByte AND $C0) = $C0 { 1100 0000 } THEN BEGIN
    { it's a number > 99, as top two bits are set, so clear them }
    HighByte := HighByte AND NOT $C0; { 1100 0000 }
    { and return the number }
    TempLocoChip := (HighByte * 256) + LowByte;
  END ELSE
    { it's a loco number < 100 }
    TempLocoChip :=  LowByte;
  Result := TempLocoChip;
END; { GetLocoChipFromTwoBytes }

FUNCTION DoBitPattern(B : Byte) : String;
{ Write out the bit pattern for a given number }
BEGIN
   CASE B OF
     0 : Result := '0000 0000';  { $00 }
     1 : Result := '0000 0001';  { $01 }
     2 : Result := '0000 0010';  { $02 }
     3 : Result := '0000 0011';  { $03 }
     4 : Result := '0000 0100';  { $04 }
     5 : Result := '0000 0101';  { $05 }
     6 : Result := '0000 0110';  { $06 }
     7 : Result := '0000 0111';  { $07 }
     8 : Result := '0000 1000';  { $08 }
     9 : Result := '0000 1001';  { $09 }
     10 : Result := '0000 1010'; { $0A }
     11 : Result := '0000 1011'; { $0B }
     12 : Result := '0000 1100'; { $0C }
     13 : Result := '0000 1101'; { $0D }
     14 : Result := '0000 1110'; { $0E }
     15 : Result := '0000 1111'; { $0F }
     16 : Result := '0001 0000'; { $10 }
     17 : Result := '0001 0001'; { $11 }
     18 : Result := '0001 0010'; { $12 }
     19 : Result := '0001 0011'; { $13 }
     20 : Result := '0001 0100'; { $14 }
     21 : Result := '0001 0101'; { $15 }
     22 : Result := '0001 0110'; { $16 }
     23 : Result := '0001 0111'; { $17 }
     24 : Result := '0001 1000'; { $18 }
     25 : Result := '0001 1001'; { $19 }
     26 : Result := '0001 1010'; { $1A }
     27 : Result := '0001 1011'; { $1B }
     28 : Result := '0001 1100'; { $1C }
     29 : Result := '0001 1101'; { $1D }
     30 : Result := '0001 1110'; { $1E }
     31 : Result := '0001 1111'; { $1F }
     32 : Result := '0010 0000'; { $20 }
     33 : Result := '0010 0001'; { $21 }
     34 : Result := '0010 0010'; { $22 }
     35 : Result := '0010 0011'; { $23 }
     36 : Result := '0010 0100'; { $24 }
     37 : Result := '0010 0101'; { $25 }
     38 : Result := '0010 0110'; { $26 }
     39 : Result := '0010 0111'; { $27 }
     40 : Result := '0010 1000'; { $28 }
     41 : Result := '0010 1001'; { $29 }
     42 : Result := '0010 1010'; { $2A }
     43 : Result := '0010 1011'; { $2B }
     44 : Result := '0010 1100'; { $2C }
     45 : Result := '0010 1101'; { $2D }
     46 : Result := '0010 1110'; { $2E }
     47 : Result := '0010 1111'; { $2F }
     48 : Result := '0011 0000'; { $30 }
     49 : Result := '0011 0001'; { $31 }
     50 : Result := '0011 0010'; { $32 }
     51 : Result := '0011 0011'; { $33 }
     52 : Result := '0011 0100'; { $34 }
     53 : Result := '0011 0101'; { $35 }
     54 : Result := '0011 0110'; { $36 }
     55 : Result := '0011 0111'; { $37 }
     56 : Result := '0011 1000'; { $38 }
     57 : Result := '0011 1001'; { $39 }
     58 : Result := '0011 1010'; { $3A }
     59 : Result := '0011 1011'; { $3B }
     60 : Result := '0011 1100'; { $3C }
     61 : Result := '0011 1101'; { $3D }
     62 : Result := '0011 1110'; { $3E }
     63 : Result := '0011 1111'; { $3F }
     64 : Result := '0100 0000'; { $40 }
     65 : Result := '0100 0001'; { $41 }
     66 : Result := '0100 0010'; { $42 }
     67 : Result := '0100 0011'; { $43 }
     68 : Result := '0100 0100'; { $44 }
     69 : Result := '0100 0101'; { $45 }
     70 : Result := '0100 0110'; { $46 }
     71 : Result := '0100 0111'; { $47 }
     72 : Result := '0100 1000'; { $48 }
     73 : Result := '0100 1001'; { $49 }
     74 : Result := '0100 1010'; { $4A }
     75 : Result := '0100 1011'; { $4B }
     76 : Result := '0100 1100'; { $4C }
     77 : Result := '0100 1101'; { $4D }
     78 : Result := '0100 1110'; { $4E }
     79 : Result := '0100 1111'; { $4F }
     80 : Result := '0101 0000'; { $50 }
     81 : Result := '0101 0001'; { $51 }
     82 : Result := '0101 0010'; { $52 }
     83 : Result := '0101 0011'; { $53 }
     84 : Result := '0101 0100'; { $54 }
     85 : Result := '0101 0101'; { $55 }
     86 : Result := '0101 0110'; { $56 }
     87 : Result := '0101 0111'; { $57 }
     88 : Result := '0101 1000'; { $58 }
     89 : Result := '0101 1001'; { $59 }
     90 : Result := '0101 1010'; { $5A }
     91 : Result := '0101 1011'; { $5B }
     92 : Result := '0101 1100'; { $5C }
     93 : Result := '0101 1101'; { $5D }
     94 : Result := '0101 1110'; { $5E }
     95 : Result := '0101 1111'; { $5F }
     96 : Result := '0110 0000'; { $60 }
     97 : Result := '0110 0001'; { $61 }
     98 : Result := '0110 0010'; { $62 }
     99 : Result := '0110 0011'; { $63 }
     100 : Result := '0110 0100'; { $64 }
     101 : Result := '0110 0101'; { $65 }
     102 : Result := '0110 0110'; { $66 }
     103 : Result := '0110 0111'; { $67 }
     104 : Result := '0110 1000'; { $68 }
     105 : Result := '0110 1001'; { $69 }
     106 : Result := '0110 1010'; { $6A }
     107 : Result := '0110 1011'; { $6B }
     108 : Result := '0110 1100'; { $6C }
     109 : Result := '0110 1101'; { $6D }
     110 : Result := '0110 1110'; { $6E }
     111 : Result := '0110 1111'; { $6F }
     112 : Result := '0111 0000'; { $70 }
     113 : Result := '0111 0001'; { $71 }
     114 : Result := '0111 0010'; { $72 }
     115 : Result := '0111 0011'; { $73 }
     116 : Result := '0111 0100'; { $74 }
     117 : Result := '0111 0101'; { $75 }
     118 : Result := '0111 0110'; { $76 }
     119 : Result := '0111 0111'; { $77 }
     120 : Result := '0111 1000'; { $78 }
     121 : Result := '0111 1001'; { $79 }
     122 : Result := '0111 1010'; { $7A }
     123 : Result := '0111 1011'; { $7B }
     124 : Result := '0111 1100'; { $7C }
     125 : Result := '0111 1101'; { $7D }
     126 : Result := '0111 1110'; { $7E }
     127 : Result := '0111 1111'; { $7F }
     128 : Result := '1000 0000'; { $80 }
     129 : Result := '1000 0001'; { $81 }
     130 : Result := '1000 0010'; { $82 }
     131 : Result := '1000 0011'; { $83 }
     132 : Result := '1000 0100'; { $84 }
     133 : Result := '1000 0101'; { $85 }
     134 : Result := '1000 0110'; { $86 }
     135 : Result := '1000 0111'; { $87 }
     136 : Result := '1000 1000'; { $88 }
     137 : Result := '1000 1001'; { $89 }
     138 : Result := '1000 1010'; { $8A }
     139 : Result := '1000 1011'; { $8B }
     140 : Result := '1000 1100'; { $8C }
     141 : Result := '1000 1101'; { $8D }
     142 : Result := '1000 1110'; { $8E }
     143 : Result := '1000 1111'; { $8F }
     144 : Result := '1001 0000'; { $90 }
     145 : Result := '1001 0001'; { $91 }
     146 : Result := '1001 0010'; { $92 }
     147 : Result := '1001 0011'; { $93 }
     148 : Result := '1001 0100'; { $94 }
     149 : Result := '1001 0101'; { $95 }
     150 : Result := '1001 0110'; { $96 }
     151 : Result := '1001 0111'; { $97 }
     152 : Result := '1001 1000'; { $98 }
     153 : Result := '1001 1001'; { $99 }
     154 : Result := '1001 1010'; { $9A }
     155 : Result := '1001 1011'; { $9B }
     156 : Result := '1001 1100'; { $9C }
     157 : Result := '1001 1101'; { $9D }
     158 : Result := '1001 1110'; { $9E }
     159 : Result := '1001 1111'; { $9F }
     160 : Result := '1010 0000'; { $A0 }
     161 : Result := '1010 0001'; { $A1 }
     162 : Result := '1010 0010'; { $A2 }
     163 : Result := '1010 0011'; { $A3 }
     164 : Result := '1010 0100'; { $A4 }
     165 : Result := '1010 0101'; { $A5 }
     166 : Result := '1010 0110'; { $A6 }
     167 : Result := '1010 0111'; { $A7 }
     168 : Result := '1010 1000'; { $A8 }
     169 : Result := '1010 1001'; { $A9 }
     170 : Result := '1010 1010'; { $AA }
     171 : Result := '1010 1011'; { $AB }
     172 : Result := '1010 1100'; { $AC }
     173 : Result := '1010 1101'; { $AD }
     174 : Result := '1010 1110'; { $AE }
     175 : Result := '1010 1111'; { $AF }
     176 : Result := '1011 0000'; { $B0 }
     177 : Result := '1011 0001'; { $B1 }
     178 : Result := '1011 0010'; { $B2 }
     179 : Result := '1011 0011'; { $B3 }
     180 : Result := '1011 0100'; { $B4 }
     181 : Result := '1011 0101'; { $B5 }
     182 : Result := '1011 0110'; { $B6 }
     183 : Result := '1011 0111'; { $B7 }
     184 : Result := '1011 1000'; { $B8 }
     185 : Result := '1011 1001'; { $B9 }
     186 : Result := '1011 1010'; { $BA }
     187 : Result := '1011 1011'; { $BB }
     188 : Result := '1011 1100'; { $BC }
     189 : Result := '1011 1101'; { $BD }
     190 : Result := '1011 1110'; { $BE }
     191 : Result := '1011 1111'; { $BF }
     192 : Result := '1100 0000'; { $C0 }
     193 : Result := '1100 0001'; { $C1 }
     194 : Result := '1100 0010'; { $C2 }
     195 : Result := '1100 0011'; { $C3 }
     196 : Result := '1100 0100'; { $C4 }
     197 : Result := '1100 0101'; { $C5 }
     198 : Result := '1100 0110'; { $C6 }
     199 : Result := '1100 0111'; { $C7 }
     200 : Result := '1100 1000'; { $C8 }
     201 : Result := '1100 1001'; { $C9 }
     202 : Result := '1100 1010'; { $CA }
     203 : Result := '1100 1011'; { $CB }
     204 : Result := '1100 1100'; { $CC }
     205 : Result := '1100 1101'; { $CD }
     206 : Result := '1100 1110'; { $CE }
     207 : Result := '1100 1111'; { $CF }
     208 : Result := '1101 0000'; { $D0 }
     209 : Result := '1101 0001'; { $D1 }
     210 : Result := '1101 0010'; { $D2 }
     211 : Result := '1101 0011'; { $D3 }
     212 : Result := '1101 0100'; { $D4 }
     213 : Result := '1101 0101'; { $D5 }
     214 : Result := '1101 0110'; { $D6 }
     215 : Result := '1101 0111'; { $D7 }
     216 : Result := '1101 1000'; { $D8 }
     217 : Result := '1101 1001'; { $D9 }
     218 : Result := '1101 1010'; { $DA }
     219 : Result := '1101 1011'; { $DB }
     220 : Result := '1101 1100'; { $DC }
     221 : Result := '1101 1101'; { $DD }
     222 : Result := '1101 1110'; { $DE }
     223 : Result := '1101 1111'; { $DF }
     224 : Result := '1110 0000'; { $E0 }
     225 : Result := '1110 0001'; { $E1 }
     226 : Result := '1110 0010'; { $E2 }
     227 : Result := '1110 0011'; { $E3 }
     228 : Result := '1110 0100'; { $E4 }
     229 : Result := '1110 0101'; { $E5 }
     230 : Result := '1110 0110'; { $E6 }
     231 : Result := '1110 0111'; { $E7 }
     232 : Result := '1110 1000'; { $E8 }
     233 : Result := '1110 1001'; { $E9 }
     234 : Result := '1110 1010'; { $EA }
     235 : Result := '1110 1011'; { $EB }
     236 : Result := '1110 1100'; { $EC }
     237 : Result := '1110 1101'; { $ED }
     238 : Result := '1110 1110'; { $EE }
     239 : Result := '1110 1111'; { $EF }
     240 : Result := '1111 0000'; { $F0 }
     241 : Result := '1111 0001'; { $F1 }
     242 : Result := '1111 0010'; { $F2 }
     243 : Result := '1111 0011'; { $F3 }
     244 : Result := '1111 0100'; { $F4 }
     245 : Result := '1111 0101'; { $F5 }
     246 : Result := '1111 0110'; { $F6 }
     247 : Result := '1111 0111'; { $F7 }
     248 : Result := '1111 1000'; { $F8 }
     249 : Result := '1111 1001'; { $F9 }
     250 : Result := '1111 1010'; { $FA }
     251 : Result := '1111 1011'; { $FB }
     252 : Result := '1111 1100'; { $FC }
     253 : Result := '1111 1101'; { $FD }
     254 : Result := '1111 1110'; { $FE }
     255 : Result := '1111 1111'; { $FF }
   END {CASE}
END; { DoBitPattern }

FUNCTION GetCommandLen(LengthByte : Byte) : Integer;
{ Find out the length of the command array from bits 3-0 of first byte }
BEGIN
  Result := LengthByte AND NOT $F0; { 240 - 11110000 }
END; { GetCommandLen }

FUNCTION CheckSum(Data : ARRAY OF Byte) : Byte;
{ Calculate checksum - an AMS non-copyright concept! (too small to have the requisite skill, judgement and labour! And not patentable, as it's a mathematical method) }
VAR
  I : Integer;
  TempCheck : Byte;

BEGIN
  TempCheck := 0;
  FOR I := 0 TO GetCommandLen(Data[0]) DO
    { don't know how this works, but it seems to! }
    TempCheck := TempCheck XOR Data[I];
  Result := TempCheck;
END; { CheckSum }

FUNCTION ByteArraysCompareOK(FirstArray : ARRAY OF Byte; SecondArray : ARRAY OF Byte) : Boolean;
{ Does a byte-by-byte comparison [not in use at present] }
VAR
  I, L : Word;

BEGIN
  Result := False;
  L := GetCommandLen(FirstArray[0]);
  FOR I := 1 TO L DO
    IF FirstArray[I - 1] <> SecondArray[I - 1] THEN
      Exit;
  Result := True;
END; { ByteArraysCompareOK }

FUNCTION WhatSortOfDecoderIsIt(UnitNum, B : Byte) : String;
{ Looks at bits 6 and 5, which gives the answer }

BEGIN
  { bits 6 and 5 in Data2 tell us }
  IF (B AND 96) = 96 THEN { bits 6 and 5 }
    Result := ReservedStr
  ELSE
    IF (B AND 64) = 64 THEN { bit 6 set }
      Result := FeedbackEncoderStr
    ELSE
      IF (B AND 32) = 32 THEN { bit 5 set }
        Result := AccessoryDecoderWithFeedbackStr
      ELSE
        IF (B AND 96) <> 96 THEN { neither bits 5 or 6 set }
          Result := AccessoryDecoderWithoutFeedbackStr;
END; { WhatSortOfDecoderIsIt }

PROCEDURE GetFeedbackReply(ReadArray : ARRAY OF Byte);
{ When feedback has comes in, work out for which device, and what the new status is }
VAR
  I : Integer;
  NumberOfInputs : Integer;

  PROCEDURE WhichFeedbackInputsHaveChanged(UnitNum, NewData : Byte);
  { Checks against the stored feedback data to report any changes }
  VAR
    B : Byte;
    FeedbackChanges : FeedbackRec;
    I, Input, Nibble : Integer;

  BEGIN
    IF ((UnitNum + 1) < FirstFeedbackUnit) OR ((UnitNum + 1) > LastFeedbackUnit) THEN
      Log('GG Feedback unit number ' + IntToStr(UnitNum + 1) + ' outside designated range')
    ELSE BEGIN
      { Upper or lower nibble? }
      IF (NewData AND 16) <> 16 THEN BEGIN
        { If bit 4 set, inputs are 5 to 8, so increment Input accordingly }
        Input := 0;
        Nibble := 0;
      END ELSE BEGIN
        Input := 4;
        Nibble := 1;
      END;

      { XORing only sets in B the bits which are not the same }
      B := FeedbackArray[UnitNum, Nibble] XOR NewData;

      IF B = 0 THEN
        // Log('G Feedback unit ' + IntToStr(UnitNum + 1) + ': no change') { caused by lots of *78*s }
      ELSE BEGIN
        { There is a change - cycle through the four inputs }
        FOR I := 1 TO 4 DO BEGIN
          IF (B AND BinaryCounter[I]) = BinaryCounter[I] THEN BEGIN
            IF (NewData AND BinaryCounter[I]) = BinaryCounter[I] THEN BEGIN
              { store the data }
              FeedbackDataArray[UnitNum + 1, Input + I] := True;

              FeedbackChanges.Feedback_Unit := UnitNum + 1;
              FeedbackChanges.Feedback_Input := Input + I;
              FeedbackChanges.Feedback_InputOn := True;
              { decode it and record it for debugging }
              DecodeFeedback(FeedbackChanges);
            END ELSE BEGIN
              FeedbackDataArray[UnitNum + 1, Input + I] := False;

              FeedbackChanges.Feedback_Unit := UnitNum + 1;
              FeedbackChanges.Feedback_Input := Input + I;
              FeedbackChanges.Feedback_InputOn := False;
              { decode it and record it for debugging }
              DecodeFeedback(FeedbackChanges);
            END;
          END;
        END;
        { now store the new data instead }
        FeedbackArray[UnitNum, Nibble] := NewData;
      END;
    END;
  END; { WhichFeedbackInputsHaveChanged }

BEGIN
  { ReadArray can be 66-78, even nos only, as up to 7 feedback inputs can arrive at once - calculate where the data is and whether it has changed }
  NumberOfInputs := (ReadArray[0] - 64) DIV 2;
  { Now see which if any have changed, and store the new version }
  FOR I := 1 TO NumberOfInputs DO
    { decoder address is in first byte (and first parameter), data in second byte (and second parameter) }
    WhichFeedbackInputsHaveChanged(ReadArray[(I * 2) - 1], ReadArray[I * 2]);
END; { GetFeedbackReply }

{$O-}
PROCEDURE ReadInDataMainProcedure(VAR ReadArray : ARRAY OF Byte; ExpectedReply : ReplyType; VAR CheckTimeOut : Boolean; OUT ExpectedDataReceived : Boolean);
{ If data arrives, process it. This routine is called either when data is known to have arrived, or when we know it is about to, because we've issued a command that
  produces a known response. NB: it is possible for unexpected data to turn up before the expected response so the routine has to cater for that eventuality.
}
TYPE
  ResponseOrBroadcastType = (Response, Broadcast, NoResponse);

CONST
  WarnUser = True;

VAR
  CommandLen : Integer;
  DebugStr, UnknownReplyString : String;
  ErrorFound : Boolean;
  ErrorMsg : String;
  I : Integer;
  LocoChip : Integer;
  OK, TimedOut : Boolean;
  PortStr : String;
  ResponseOrBroadcast : ResponseOrBroadcastType;
  StartTimer : Cardinal;
  T : Train;
  TempByte : Byte;
  TempStr : String;
  TempInt : Integer;
  TickCount : Cardinal;
  TypeOfLogChar : Char;

BEGIN
  TRY
    { Loop if the received data is not what's expected - aim to read in what's expected second, maybe third, time around }
    ErrorFound := False;
    TimedOut := False;
    ResponseOrBroadcast := NoResponse;

    { Disable the main timer or other reads are attempted while we're waiting for an existing read }
//    MainWindow.MainTimer.Enabled := False;
//    REPEAT
//      Application.ProcessMessages;
//      // Timeout **************
//    UNTIL RequestAndRepliesDataReceived OR BroadcastsDataReceivedc OR (ExpectedReply = NoReplyExpected);
//    MainWindow.MainTimer.Enabled := True;
//
//    IF (ExpectedReply = NoReplyExpected) AND (BroadcastsData = '') AND (RequestAndRepliesData = '') THEN
//      Exit;
//
////InvalidateScreen(UnitRef, 'DrawMap');
//
//    IF (RequestAndRepliesData = '') AND (BroadcastsData = '') THEN
//      debug;
//    Log('X RequestAndRepliesData = ' + RequestAndRepliesData + ' - BroadcastsData = ' + BroadcastsData);
//
//    RequestAndRepliesDataReceived := False;
//    BroadcastsDataReceived := False;

//  RequestAndRepliesData : String;

    REPEAT
      { Initialisations }
      DebugStr := '';
      ExpectedDataReceived := False;
      UnrequestedDataFound := False;
      { Clears read array in case data left behind is processed a second time }
      FOR I := 0 TO (ReadArrayLen - 1) DO
        ReadArray[I] := 0;



//      { Time out stuff }
//      IF (ExpectedReply <> NoReplyExpected)
//      AND (ExpectedReply <> TrackPowerOffReply)
//      THEN BEGIN
//        { something is expected, so wait for it }
//        StartTimer := GetTickCount();
        TimedOut := False;
//        REPEAT
//          TickCount := (GetTickCount - StartTimer);
//          IF TickCount > (10000) THEN BEGIN
//            TimedOut := True;
//
//            DebugStr := 'timed out in wait for ExpectedReply of ';
//            CASE ExpectedReply OF
//               Acknowledgment:
//                 DebugStr := DebugStr + '''Acknowledgment''';
//               CommandStationSoftwareReply:
//                 DebugStr := DebugStr + '''Command Station Software Reply''';
//               ComputerInterfaceSoftwareReply:
//                 DebugStr := DebugStr + '''Computer InterfaceS oftware Reply''';
//               EmergencyStopReply:
//                 DebugStr := DebugStr + '''Emergency Stop Reply''';
//               EverythingTurnedOnReply:
//                 DebugStr := DebugStr + '''Everything Turned On Reply''';
//               FeedbackReply:
//                 DebugStr := DebugStr + '''Feedback Reply''';
//               LocoAcknowledgment:
//                 DebugStr := DebugStr + '''Loco Acknowledgment''';
//               LocoReply:
//                 DebugStr := DebugStr + '''Loco Reply''';
//               LocoTakenoverReply:
//                 DebugStr := DebugStr + '''Loco Taken Over Reply''';
//               PointAcknowledgment:
//                 DebugStr := DebugStr + '''Point Acknowledgment''';
//               PointReply:
//                 DebugStr := DebugStr + '''Point Reply''';
//               ProgrammingModeReply:
//                 DebugStr := DebugStr + '''Programmimg Mode Reply''';
//               SignalAcknowledgment:
//                 DebugStr := DebugStr + '''Signal Acknowledgment''';
//               SystemStatusReply:
//                 DebugStr := DebugStr + '''System Status Reply''';
//               { The following two won't ever be reached, but are here for completeness }
//               NoReplyExpected:
//                 DebugStr := DebugStr + '''No Reply Expected''';
//               TrackPowerOffReply:
//                 DebugStr := DebugStr + '''Track Power Off Reply''';
//            END; {CASE}
//            Log('X ' + DebugStr);
//          END;
//
//          Application.ProcessMessages;
//        UNTIL RequestAndRepliesDataReceived;
//
//        IF TimedOut THEN
//          Log('X **** Timed Out');
//
//        IF CheckTimeOut THEN
//          Exit;
//      END;

      { If data found, read in first byte returned, to get the length. Read it in as an integer to see if it's -1, then save it as a byte (AMS 10/8/00) }

      FirstByteErrorMsgWritten := False;

//      IF RequestAndRepliesData <> '' THEN
//        TempStr := RequestAndRepliesData
//      ELSE
//        IF BroadcastsData <> '' THEN
//          TempStr := BroadcastsData;



      TempStr := ReadDataFromList;
      IF TempStr = '' THEN BEGIN
        IF ExpectedReply = NoReplyExpected THEN
          Exit;
        MainWindow.MainTimer.Enabled := False;
        Application.ProcessMessages;
        MainWindow.MainTimer.Enabled := True;
        Continue;
      END;

      IF Pos('R ', TempStr) > 0 THEN BEGIN
        ResponseOrBroadcast := Response;
        TempStr := Copy(TempStr, 3);
      END ELSE
        IF Pos('B ', TempStr) > 0 THEN BEGIN
          ResponseOrBroadcast := Broadcast;
          TempStr := Copy(TempStr, 3);
        END ELSE BEGIN
          Log('X Invalid character in data from TCPIP unit: ' + TempStr);
          Exit;
        END;

      I := 0;
      REPEAT
        IF NOT TryStrToInt('$' + Copy(TempStr, 1, 2), TempInt) THEN
          debug
        ELSE BEGIN
          ReadArray[I] := TempInt;
          Inc(I);
          TempStr:= Copy(TempStr, 3);
        END;
      UNTIL TempStr= '';

      CommandLen := GetCommandLen(ReadArray[0]);

      { See if last byte is valid before returning it }
      OK := (CheckSum(ReadArray) = ReadArray[CommandLen + 1]);

      IF NOT OK THEN BEGIN
        TRY
          OK := False;
          DebugStr := 'Checksum failure: ';
          ASM
            Int 3;
          END; {ASM}
          { need to recover from this by doing the read again? **** 6/11/06 }

          { write out the string }
          FOR I := 0 TO CommandLen + 1 DO
            DebugStr := DebugStr + IntToStr(ReadArray[I]) + '=';
          Log('G ' + DebugStr);
        EXCEPT
          ON E : Exception DO
            Log('EG ReadInDataMainProcedure: ' + E.ClassName +' error raised, with message: ' + E.Message);
        END; {TRY}
      END;

      IF OK THEN BEGIN
        { Examine the first byte returned to identify it, and see if it is what was expected }
        CASE ReadArray[0] OF
          1:
            { Maybe an acknowledgment or an error - depends what follows the '1' }
             CASE ReadArray[1] OF
              1:
                BEGIN
                  ErrorMsg := 'Error between interface and PC';
                  Log('XG ' + ErrorMsg);
                END;
              2:
                BEGIN
                  ErrorMsg := 'Error between interface and command station';
                  Log('XG ' + ErrorMsg);
                END;
              3:
                BEGIN
                  ErrorMsg := 'Error: Command unknown';
                  Log('XG ' + ErrorMsg);
                END;
              4:
                IF (ExpectedReply = Acknowledgment)
                OR (ExpectedReply = LocoAcknowledgment)
                OR (ExpectedReply = PointAcknowledgment)
                OR (ExpectedReply = SignalAcknowledgment)
                THEN
                  ExpectedDataReceived := True;
              5:
                BEGIN
                  ErrorMsg := 'Error: Command station does not respond';
                  Log('XG ' + ErrorMsg);
                END;
            END;
          2:
            { software version of computer interface (LI100F and LI101F) }
            BEGIN
              DebugStr := 'Computer interface software version reply:';
              IF ExpectedReply = ComputerInterfaceSoftwareReply THEN BEGIN
                ExpectedDataReceived := True;
                { answer is in BCD, 4 upper bits the tens, four lower the units }
                TempByte := ReadArray[1] SHR 4;
                DebugStr := DebugStr + ' hardware version=' + IntToStr(TempByte);
                TempByte := ReadArray[2] AND NOT $F0;
                DebugStr := DebugStr + ', software version=' + IntToStr(TempByte);
              END;
              Log('G ' + DebugStr);
            END;
          66: { $42 }
            BEGIN
              IF (ExpectedReply = NoReplyExpected)
              AND (WhatSortOfDecoderIsIt(ReadArray[1], ReadArray[2]) = AccessoryDecoderWithoutFeedbackStr)
              THEN BEGIN
                { see if it's point data, as that seems to be an acknowledgment }
                ExpectedDataReceived := True;
                Log('T Feedback broadcast for point selection has arrived');
              END ELSE
                { see if we've asked for the data (during startup, for instance) }
                IF (ExpectedReply = FeedbackReply)
                AND (ExpectedFeedbackAddress = ReadArray[1])
                THEN BEGIN
                  ExpectedDataReceived := True;
                  Log('T Requested feedback for unit ' + IntToStr(ReadArray[1] + 1) + ' has arrived');
                END ELSE BEGIN
                  UnrequestedDataFound := True;
                  { see who it's from, etc. }
                  GetFeedbackReply(ReadArray);
                END;
            END;
          68: { $44 }
            BEGIN
              IF (ExpectedReply = FeedbackReply) THEN
                ExpectedDataReceived := True
              ELSE
                UnrequestedDataFound := True;
              { see who it's from, etc. }
              GetFeedbackReply(ReadArray);
            END;
          70: { $46 }
            BEGIN
              IF (ExpectedReply = FeedbackReply) THEN
                ExpectedDataReceived := True
              ELSE
                UnrequestedDataFound := True;
              GetFeedbackReply(ReadArray);
            END;
          72: { $48 }
            BEGIN
              IF (ExpectedReply = FeedbackReply) THEN
                ExpectedDataReceived := True
              ELSE BEGIN
                UnrequestedDataFound := True;
                Log('TG -72-');
  //              MakeSound(1);
                WriteDataToFeedbackWindow('*72*');
              END;
              GetFeedbackReply(ReadArray);
            END;
          74: { $4A }
            BEGIN
              IF (ExpectedReply = FeedbackReply) THEN
                ExpectedDataReceived := True
              ELSE BEGIN
                UnrequestedDataFound := True;
                Log('TG -74-');
  //              MakeSound(1);
                WriteDataToFeedbackWindow('*74*');
              END;
              GetFeedbackReply(ReadArray);
           END;
          76: { $4C }
            BEGIN
              IF (ExpectedReply = FeedbackReply) THEN
                ExpectedDataReceived := True
              ELSE BEGIN
                UnrequestedDataFound := True;
                Log('TG -76-');
  //              MakeSound(1);
                WriteDataToFeedbackWindow('*76*');
              END;
              GetFeedbackReply(ReadArray);
            END;
          78: { $4E }
            BEGIN
              IF (ExpectedReply = FeedbackReply) THEN
                ExpectedDataReceived := True
              ELSE BEGIN
                UnrequestedDataFound := True;
                Log('TG -78-');
                WriteDataToFeedbackWindow('*78*');
  //              MakeSound(1);
              END;
              GetFeedbackReply(ReadArray);
            END;
          97: { $61 }
            { Some 97 unrequested msgs will arrive four times - just note }
            CASE ReadArray[1] OF
              0:
                IF ExpectedReply = TrackPowerOffReply THEN
                  ExpectedDataReceived := True
                ELSE BEGIN
                  UnrequestedDataFound := True;
                  IF NOT SystemStatus.EmergencyOff THEN
                    ReadOut('ShortCircuit');
                  SystemStatus.EmergencyOff := True;
                  ErrorMsg := '*** power off ***';
                  Log('EG '+ ErrorMsg);
                  WriteDataToFeedbackWindow(ErrorMsg);
                END;
              1:
                IF ExpectedReply = EverythingTurnedOnReply THEN
                  ExpectedDataReceived := True { when would this happen? }
                ELSE BEGIN
                  UnrequestedDataFound := True;
                  SystemStatus.EmergencyOff := False;
                  SystemStatus.EmergencyStop := False;
                  ErrorMsg := '*** power on ***';
                  Log('EG '+ ErrorMsg);
                  WriteDataToFeedbackWindow(ErrorMsg);
                END;
              2:
                BEGIN
                  Log('EG Programming (Service) Mode');
                  ErrorFound := True;
                END;
              17: { $11 }
                BEGIN
                  ErrorMsg := 'Command station ready [Command not yet implemented]';
                  Log('EG ' + ErrorMsg);
                  ErrorFound := True;
                END;
              18: { $12 }
                BEGIN
                  ErrorMsg := 'Error during programming - short circuit';
                  Log('EG ' + ErrorMsg);
                  ErrorFound := True;
                END;
              19: { $13 }
                BEGIN
                  ErrorMsg := 'Error during programming - data byte not found';
                  Log('EG ' + ErrorMsg);
                  { *** programming should be broken off or tried again *** }
                  ErrorFound := True;
                END;
              31: { $1F }
                BEGIN
                  ErrorMsg := 'Command station busy [Command not yet implemented]';
                  Log('EG ' + ErrorMsg);
                  ErrorFound := True;
                END;
              128: { $80 }
                BEGIN
                  ErrorMsg := 'Transfer error (XOR not correct)';
                  Log('EG ' + ErrorMsg);
                  ErrorFound := True;
                  { should retransmit command **** }
                END;
              129: { $81 }
                BEGIN
                  ErrorMsg := 'Command station busy';
                  Log('EG ' + ErrorMsg);
                  ErrorFound := True;
  //                MakeSound(1);
                  { should retransmit command **** }
                END;
              130: { $82 }
                BEGIN
                  ErrorMsg := 'Instruction not supported by command station';
                  Log('EG ' + ErrorMsg);
                  ErrorFound := True;
                END;
              131: { $83 }
                BEGIN
                  ErrorMsg := 'Error with double header - no operation command';
                  Log('EG ' + ErrorMsg);
                  ErrorFound := True;
                END;
              132: { $84 }
                BEGIN
                  ErrorMsg := 'Error with double header - double header occupied';
                  Log('EG ' + ErrorMsg);
                  ErrorFound := True;
                END;
              133: { $85 }
                BEGIN
                  ErrorMsg := 'Error with double header - loco already in double header';
                  Log('EG ' + ErrorMsg);
                  ErrorFound := True;
                END;
              134: { $86 }
                BEGIN
                  ErrorMsg := 'Error with double header - locomotive still moving';
                  Log('EG ' + ErrorMsg);
                  ErrorFound := True;
                END;
            END; {CASE}
          98: { $62 }
            CASE ReadArray[1] OF
              34: { $22 }
                BEGIN
                  IF ExpectedReply = SystemStatusReply THEN BEGIN
                    ExpectedDataReceived := True;
                    SystemStatusStr := '';
                    { answer tells us what is currently happening }
                    IF (ReadArray[2] AND $80) = $80 THEN { 1000 0000 }
                      { bit 7 set }
                      SystemStatusStr := 'RAM check error in command station';
                    IF (ReadArray[2] AND $40) = $40 THEN { 0100 0000 }
                      { bit 6 set }
                      SystemStatusStr := 'command station powering up';
                    IF (ReadArray[2] AND $08) = $08 THEN { 0000 1000 }
                      { bit 3 set }
                      SystemStatusStr := 'in programming (service) mode';
                    IF (ReadArray[2] AND $02) = $02 THEN { 0000 0010 }
                      { bit 1 set }
                      SystemStatusStr := 'emergency stop is on';
                    IF (ReadArray[2] AND $01) = $01 THEN { 0000 0001 }
                      { bit 0 set }
                      SystemStatusStr := 'emergency off is on';

                    IF (ReadArray[2] AND $04) <> $04 THEN { 0000 0100 }
                      { bit 2 not set - this one is logged, but not written to the screen on start up }
                      Log('G Returning system status for ' + PortStr +': start mode=manual')
                    ELSE
                      SystemStatusStr := 'start mode=auto - may need to send start-mode command (see s.2.1.4 of LI101F manual)';

                    IF SystemStatusStr <> '' THEN
                      Log('G Returning system status for ' + PortStr + ': ' + SystemStatusStr);
                  END;
                END;
            END; {CASE}
          99: { $63 }
            CASE ReadArray[1] OF
              16:  { $10 }
                { Programming (service) mode response for Register and Page mode }
                BEGIN
                  ErrorMsg := 'Programming service) mode response for Register & Page mode ';
                  Log('EG ' + ErrorMsg);
                  { Needs more code if to be used }
                  ErrorFound := True;
                END;
              20: { $14 }
                { Programming (service) mode response for Direct CV mode }
                BEGIN
                  ErrorMsg := 'Programming (service mode) response for Direct CV mode ';
                  Log('EG ' + ErrorMsg);
                  { Needs more code if to be used }
                  ErrorFound := True;
                END;
              33: { $21 }
                { software version of command station (v.3) }
                BEGIN
                  DebugStr := 'Command Station software version reply:';
                  IF ExpectedReply = CommandStationSoftwareReply THEN BEGIN
                    ExpectedDataReceived := True;
                    { answer is in BCD, 4 upper bits the tens, four lower the units }
                    TempByte := ReadArray[2] shr 4;
                    DebugStr := DebugStr + ' version=' + IntToStr(TempByte);
                    TempByte := ReadArray[2] and not $F0;
                    DebugStr := DebugStr + '.' + IntToStr(TempByte);
                    { could use following info to see if certain commands work }
                    CASE ReadArray[3] OF
                      0:
                        DebugStr := DebugStr + ' - LZ100';
                      1:
                        DebugStr := DebugStr + ' - LH200';
                      2:
                        DebugStr := DebugStr + ' - DPC';
                    END; {CASE}
                  END;
                  Log('G ' + DebugStr);
                END;
            END; {CASE}
          129: { $81 }{ Emergency stop - msg will arrive four times }
            IF ExpectedReply = EmergencyStopReply THEN
              ExpectedDataReceived := True
            ELSE IF (ReadArray[1] = 0) THEN BEGIN
              UnrequestedDataFound := True;
              SystemStatus.EmergencyStop := True;
              ErrorMsg := '*** emergency stop ***';
              Log('EG ' + ErrorMsg);
            END;
          198: { $C6 } { [undocumented] Information returned from double-headed locos }
            IF ExpectedReply = LocoReply THEN
              ExpectedDataReceived := True;
          225: { $E1 }
            BEGIN
              CASE ReadArray[1] OF
                129: { $81 }
                  BEGIN
                    ErrorMsg := 'Dbl-hdr error: one loco not operated by LI101F or loco 0';
                    Log('EG ' + ErrorMsg);
                  END;
                130: { $82 }
                  BEGIN
                    ErrorMsg := 'Dbl-hdr error: one loco operated by another device';
                    Log('EG ' + ErrorMsg);
                  END;
                131: { $83 }
                  BEGIN
                    ErrorMsg := 'Dbl-hdr error: loco already in dbl-hdr or m/u';
                    Log('EG ' + ErrorMsg);
                  END;
                132: { $84 }
                  BEGIN
                    ErrorMsg := 'Dbl-hdr error: speed of one loco not zero';
                    Log('EG ' + ErrorMsg);
                  END;
                133: { $85 }
                  BEGIN
                    ErrorMsg := 'Multi-Unit error: loco not in a multi-unit';
                    Log('EG ' + ErrorMsg);
                  END;
                134: { $86 }
                  BEGIN
                    ErrorMsg := 'Multi-Unit error: loco address not a m/u base address';
                    Log('EG ' + ErrorMsg);
                  END;
                135: { $87 }
                  BEGIN
                    ErrorMsg := 'Dbl-Hdr/Multi-Unit error: not possible to delete loco';
                    Log('EG ' + ErrorMsg);
                  END;
                136: { $88 }
                  BEGIN
                    ErrorMsg := 'Dbl-Hdr/Multi-Unit error: command station stack full';
                    Log('EG ' + ErrorMsg);
                  END;
              END; {CASE}
            END;
          226: { $E2 } { information returned from a multi-unit address }
            IF ExpectedReply = LocoReply THEN
              ExpectedDataReceived := True;
          227: { $E3 }
            CASE ReadArray[1] OF
              48..52: { $30-$34 }
                { response to an address search } { *** }
                BEGIN
                END;
              64: { $40 }
                BEGIN
                  { arrives unrequested if the LH100 takes over control }
                  LocoChip := GetLocoChipFromTwoBytes(ReadArray[2], ReadArray[3]);

                  { and record it }
                  Log(LocoChipToStr(LocoChip) + ' L taken over');
                  T := GetTrainRecord(LocoChip);
                  IF T <> NIL THEN BEGIN
                    T^.Train_PreviouslyControlledByProgram := True;
                    SetTrainControlledByProgram(T, False);
                  END;
                END;
              80: { $50 }
                { Function status response }
                BEGIN
                  { need to write **** }
                END;
            END; {CASE}
          228: { $E4 } { information returned from a loco under our control }
            IF ExpectedReply = LocoReply THEN
              ExpectedDataReceived := True;
          229: { $E5 } { information returned from a loco in a multi-unit }
            IF ExpectedReply = LocoReply THEN
              ExpectedDataReceived := True;
          230: { $E6 } { information returned from locos in a double header }
            IF ExpectedReply = LocoReply THEN
              ExpectedDataReceived := True;
        ELSE {CASE}
          BEGIN
            { write it out }
            UnknownReplyString := '';
            FOR I := 0 TO (CommandLen + 1) DO
              UnknownReplyString := UnknownReplyString + IntToStr(ReadArray[I])+ ' +';
            Log('XG **** Unknown reply: ' + UnknownReplyString);
          END;
        END; {CASE}

        CheckTimeOut := False;

        { Write out the debugging string and translate it into hex }
        DebugStr := DebugStr + ' ';
        FOR I := 0 TO CommandLen DO
          DebugStr := DebugStr + IntToStr(ReadArray[I]) + '-';
        DebugStr := DebugStr + IntToStr(ReadArray[CommandLen + 1]);

        { Now the hex }
        DebugStr := DebugStr + ' [';
        FOR I := 0 TO CommandLen DO
          DebugStr := DebugStr + IntToHex(ReadArray[I], 2) + '-';
        DebugStr := DebugStr + IntToHex(ReadArray[CommandLen + 1], 2);
        DebugStr := DebugStr + ']';

        { Add the appropriate Type Of Log char }
        CASE ReadArray[0] OF
          1..2:
            CASE ExpectedReply OF
              LocoAcknowledgment:
                TypeOfLogChar := 'L';
              PointAcknowledgment:
                TypeOfLogChar := 'P';
              SignalAcknowledgment:
                TypeOfLogChar := 'S';
            ELSE
              TypeOfLogChar := 'G';
            END; {CASE}
          66..78:
            TypeOfLogChar := 'T'; { trackcircuit }
          198..230:
            TypeOfLogChar := 'L'; { loco }
        ELSE
          TypeOfLogChar := 'G'; { general }
        END; {CASE}

        IF ResponseOrBroadcast = Response THEN
          Log(TypeOfLogChar + ' ' + StringOfChar(' ', 104) + 'Lenz response: ' + DebugStr)
        ELSE
          IF ResponseOrBroadcast = Broadcast THEN
            Log('G ' + StringOfChar(' ', 104) + 'Lenz broadcast: ' + DebugStr);

        { Write out bytes as bits if run-time parameter Y is set }
        IF ShowByteParam <> '' THEN BEGIN
          IF ShowByteParam = 'ALL' THEN BEGIN
            { Write all the bytes as bits }
            DebugStr := '';
            FOR I := 1 TO (CommandLen + 1) DO
              DebugStr := DebugStr + '[' + DoBitPattern(ReadArray[I]) + '] ';
            Log('G ' + StringOfChar(' ', 64) + 'All bytes: ' + DebugStr); { 64 shouldn't be a magic number *** }
          END ELSE
            IF (StrToInt(ShowByteParam) >= 0)
            AND (StrToInt(ShowByteParam) <= 14)
            THEN BEGIN
              { Write a single byte as bits }
              IF StrToInt(ShowByteParam) > (CommandLen + 1) THEN
                DebugStr := '---- ----'
              ELSE
                DebugStr := DoBitPattern(ReadArray[StrToInt(ShowByteParam)]);

              IF ResponseOrBroadcast = Response THEN
                Log('G *** Response ***' + StringOfChar(' ', 48) + 'Byte ' + ShowByteParam + ': [' + DebugStr + ']')
              ELSE
                IF ResponseOrBroadcast = Broadcast THEN
                  Log('G *** Broadcast ***' + StringOfChar(' ', 47) + 'Byte ' + ShowByteParam + ': [' + DebugStr + ']');
            END;
        END;

        { loop if necessary - not needed if no reply was awaited, or if the reply received was the one that was expected }
      END;
    UNTIL (ExpectedReply = NoReplyExpected) OR ExpectedDataReceived OR TimedOut OR ErrorFound;

    IF UnRequestedDataFound THEN BEGIN
//    Debug('UnRequestedDataFound');
      IF TestingMode THEN
        DebugWindow.Caption := DebugWindow.Caption + '.';
      IF NOT SystemOnline THEN
        SetSystemOnline;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG : ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadInDataMainProcedure }
{$O+}

PROCEDURE ReadInData{1}(VAR ReadArray : ARRAY OF Byte; ExpectedReply : ReplyType; VAR ExpectedDataReceived : Boolean); Overload
VAR
  CheckTimeOut : Boolean;

BEGIN
  CheckTimeOut := False;
  ReadInDataMainProcedure(ReadArray, ExpectedReply, CheckTimeOut, ExpectedDataReceived);
END; { ReadInData-1 }

PROCEDURE ReadInData{2}(VAR ReadArray : ARRAY OF Byte; ExpectedReply : ReplyType; VAR CheckTimeOut, ExpectedDataReceived : Boolean); Overload;
BEGIN
  CheckTimeOut := False;
  ReadInDataMainProcedure(ReadArray, ExpectedReply, CheckTimeOut, ExpectedDataReceived);
END; { ReadInData-2 }

PROCEDURE ReadInData{3}; Overload;
VAR
  CheckTimeOut : Boolean;
  ExpectedDataReceived : Boolean;
  ExpectedReply : ReplyType;
  ReadArray : ARRAY[0..15] OF Byte;

BEGIN
  ReadInDataMainProcedure(ReadArray, ExpectedReply, CheckTimeOut, ExpectedDataReceived);
END; { ReadInData-3 }

{$O-}
PROCEDURE DataIOMainProcedure(TypeOfLogChar : Char; VAR WriteArray, ReadArray : ARRAY OF Byte; ExpectedReply : ReplyType; VAR CheckTimeOut : Boolean;
                              WriteRead : WriteReadType; OUT ExpectedDataReceived : Boolean);
{ Write out supplied data, then wait for a reply. Also process data that has been broadcast without being requested. NB: it is possible for unexpected data to turn up
  before the expected response so the routine has to cater for that eventuality.
}
TYPE
  ResponseOrBroadcastType = (Response, Broadcast, NoResponse);

CONST
  ExitProgram = True;
  WarnUser = True;

VAR
  CommandLen : Integer;
  DebugStr, UnknownReplyString : String;
  ErrorFound : Boolean;
  ErrorMsg : String;
  I : Integer;
  LocoChip : Integer;
  OK, TimedOut : Boolean;
  PortStr : String;
  ResponseOrBroadcast : ResponseOrBroadcastType;
  S : String;
  StartTimer : Cardinal;
  T : Train;
  TempByte : Byte;
  TempStr : String;
  TempInt : Integer;
  TickCount : Cardinal;

BEGIN
  TRY
    ResponseOrBroadcast := NoResponse;

    { Writes out data first, unless unrequested data has been received }
    IF (WriteRead = WriteThenRead) OR (WriteRead = WriteOnly) THEN BEGIN
      { Send any output waiting to the LI101 - check first that it can be accepted }
      IF WriteArray[0] <> 0 THEN BEGIN
        { get the length first from bits 3-0, and add the checkbyte }
        WriteArray[GetCommandLen(WriteArray[0]) + 1] := CheckSum(WriteArray);
        { and write it out }
        S := '';
        FOR I := 0 TO GetCommandLen(WriteArray[0]) + 1 DO
          S := S + IntToHex(WriteArray[I], 2);
        TCPIPForm.ResponsesTCPSendText(S);
      END;

      { Now compose the "From PC" string }
      DebugStr := StringOfChar(' ', 104) + 'PC request: '; { 104 shouldn't be a magic number *** }

      CommandLen := GetCommandLen(WriteArray[0]);

      FOR I := 0 TO CommandLen DO
        DebugStr := DebugStr + IntToStr(WriteArray[I]) + '-';
      DebugStr := DebugStr + IntToStr(WriteArray[CommandLen + 1]);

      { now the hex }
      DebugStr := DebugStr + ' [';
      FOR I := 0 TO CommandLen DO
        DebugStr := DebugStr + IntToHex(WriteArray[I], 2) + '-';
      DebugStr := DebugStr + IntToHex(WriteArray[CommandLen + 1], 2);
      DebugStr := DebugStr + ']';

      Log(TypeOfLogChar + ' ' + DebugStr); {+ ' byte 2= ' + DoBitPattern(WriteArray[2]) + ' byte 4= ' + DoBitPattern(WriteArray[4]));}

      { Loop if the received data is not what's expected - aim to read in what's expected second, maybe third, time around }
      ErrorFound := False;
      TimedOut := False;
      ResponseOrBroadcast := NoResponse;
    END;

    IF (WriteRead = WriteThenRead) OR (WriteRead = ReadOnly) THEN BEGIN
      REPEAT
        { Initialisations }
        DebugStr := '';
        ExpectedDataReceived := False;
        UnrequestedDataFound := False;
        { Clears read array in case data left behind is processed a second time }
        FOR I := 0 TO (ReadArrayLen - 1) DO
          ReadArray[I] := 0;



  //      { Time out stuff }
  //      IF (ExpectedReply <> NoReplyExpected)
  //      AND (ExpectedReply <> TrackPowerOffReply)
  //      THEN BEGIN
  //        { something is expected, so wait for it }
  //        StartTimer := GetTickCount();
          TimedOut := False;
  //        REPEAT
  //          TickCount := (GetTickCount - StartTimer);
  //          IF TickCount > (10000) THEN BEGIN
  //            TimedOut := True;
  //
  //            DebugStr := 'timed out in wait for ExpectedReply of ';
  //            CASE ExpectedReply OF
  //               Acknowledgment:
  //                 DebugStr := DebugStr + '''Acknowledgment''';
  //               CommandStationSoftwareReply:
  //                 DebugStr := DebugStr + '''Command Station Software Reply''';
  //               ComputerInterfaceSoftwareReply:
  //                 DebugStr := DebugStr + '''Computer InterfaceS oftware Reply''';
  //               EmergencyStopReply:
  //                 DebugStr := DebugStr + '''Emergency Stop Reply''';
  //               EverythingTurnedOnReply:
  //                 DebugStr := DebugStr + '''Everything Turned On Reply''';
  //               FeedbackReply:
  //                 DebugStr := DebugStr + '''Feedback Reply''';
  //               LocoAcknowledgment:
  //                 DebugStr := DebugStr + '''Loco Acknowledgment''';
  //               LocoReply:
  //                 DebugStr := DebugStr + '''Loco Reply''';
  //               LocoTakenoverReply:
  //                 DebugStr := DebugStr + '''Loco Taken Over Reply''';
  //               PointAcknowledgment:
  //                 DebugStr := DebugStr + '''Point Acknowledgment''';
  //               PointReply:
  //                 DebugStr := DebugStr + '''Point Reply''';
  //               ProgrammingModeReply:
  //                 DebugStr := DebugStr + '''Programmimg Mode Reply''';
  //               SignalAcknowledgment:
  //                 DebugStr := DebugStr + '''Signal Acknowledgment''';
  //               SystemStatusReply:
  //                 DebugStr := DebugStr + '''System Status Reply''';
  //               { The following two won't ever be reached, but are here for completeness }
  //               NoReplyExpected:
  //                 DebugStr := DebugStr + '''No Reply Expected''';
  //               TrackPowerOffReply:
  //                 DebugStr := DebugStr + '''Track Power Off Reply''';
  //            END; {CASE}
  //            Log('X ' + DebugStr);
  //          END;
  //
  //          Application.ProcessMessages;
  //        UNTIL RequestAndRepliesDataReceived;
  //
  //        IF TimedOut THEN
  //          Log('X **** Timed Out');
  //
  //        IF CheckTimeOut THEN
  //          Exit;
  //      END;

        { If data found, read in first byte returned, to get the length. Read it in as an integer to see if it's -1, then save it as a byte (AMS 10/8/00) }

        FirstByteErrorMsgWritten := False;

        TempStr := ReadDataFromList;
        IF TempStr = '' THEN BEGIN
          IF ExpectedReply = NoReplyExpected THEN
            Exit;
          MainWindow.MainTimer.Enabled := False;
          Application.ProcessMessages;
          MainWindow.MainTimer.Enabled := True;
          Continue;
        END;

        IF Pos('R ', TempStr) > 0 THEN BEGIN
          ResponseOrBroadcast := Response;
          TempStr := Copy(TempStr, 3);
        END ELSE
          IF Pos('B ', TempStr) > 0 THEN BEGIN
            ResponseOrBroadcast := Broadcast;
            TempStr := Copy(TempStr, 3);
          END ELSE BEGIN
            Log('X Invalid character in data from TCPIP unit: ' + TempStr);
            Exit;
          END;

        I := 0;
        REPEAT
          IF NOT TryStrToInt('$' + Copy(TempStr, 1, 2), TempInt) THEN
            debug
          ELSE BEGIN
            ReadArray[I] := TempInt;
            Inc(I);
            TempStr:= Copy(TempStr, 3);
          END;
        UNTIL TempStr= '';

        CommandLen := GetCommandLen(ReadArray[0]);

        { See if last byte is valid before returning it }
        OK := (CheckSum(ReadArray) = ReadArray[CommandLen + 1]);

        IF NOT OK THEN BEGIN
          TRY
            OK := False;
            DebugStr := 'Checksum failure: ';
            ASM
              Int 3;
            END; {ASM}
            { need to recover from this by doing the read again? **** 6/11/06 }

            { write out the string }
            FOR I := 0 TO CommandLen + 1 DO
              DebugStr := DebugStr + IntToStr(ReadArray[I]) + '=';
            Log('G ' + DebugStr);
          EXCEPT
            ON E : Exception DO
              Log('EG ReadInDataMainProcedure: ' + E.ClassName +' error raised, with message: ' + E.Message);
          END; {TRY}
        END;

        IF OK THEN BEGIN
          { Examine the first byte returned to identify it, and see if it is what was expected }
          CASE ReadArray[0] OF
            1:
              { Maybe an acknowledgment or an error - depends what follows the '1' }
               CASE ReadArray[1] OF
                1:
                  BEGIN
                    ErrorMsg := 'Error between interface and PC';
                    Log('XG ' + ErrorMsg);
                  END;
                2:
                  BEGIN
                    ErrorMsg := 'Error between interface and command station';
                    Log('XG ' + ErrorMsg);
                  END;
                3:
                  BEGIN
                    ErrorMsg := 'Error: Command unknown';
                    Log('XG ' + ErrorMsg);
                  END;
                4:
                  IF (ExpectedReply = Acknowledgment)
                  OR (ExpectedReply = LocoAcknowledgment)
                  OR (ExpectedReply = PointAcknowledgment)
                  OR (ExpectedReply = SignalAcknowledgment)
                  THEN
                    ExpectedDataReceived := True;
                5:
                  BEGIN
                    ErrorMsg := 'Error: Command station does not respond';
                    Log('XG ' + ErrorMsg);
                  END;
              END;
            2:
              { software version of computer interface (LI100F and LI101F) }
              BEGIN
                DebugStr := 'Computer interface software version reply:';
                IF ExpectedReply = ComputerInterfaceSoftwareReply THEN BEGIN
                  ExpectedDataReceived := True;
                  { answer is in BCD, 4 upper bits the tens, four lower the units }
                  TempByte := ReadArray[1] SHR 4;
                  DebugStr := DebugStr + ' hardware version=' + IntToStr(TempByte);
                  TempByte := ReadArray[2] AND NOT $F0;
                  DebugStr := DebugStr + ', software version=' + IntToStr(TempByte);
                END;
                Log('G ' + DebugStr);
              END;
            66: { $42 }
              BEGIN
                IF (ExpectedReply = NoReplyExpected)
                AND (WhatSortOfDecoderIsIt(ReadArray[1], ReadArray[2]) = AccessoryDecoderWithoutFeedbackStr)
                THEN BEGIN
                  { see if it's point data, as that seems to be an acknowledgment }
                  ExpectedDataReceived := True;
                  Log('T Feedback broadcast for point selection has arrived');
                END ELSE
                  { see if we've asked for the data (during startup, for instance) }
                  IF (ExpectedReply = FeedbackReply)
                  AND (ExpectedFeedbackAddress = ReadArray[1])
                  THEN BEGIN
                    ExpectedDataReceived := True;
                    Log('T Requested feedback for unit ' + IntToStr(ReadArray[1] + 1) + ' has arrived');
                  END ELSE BEGIN
                    UnrequestedDataFound := True;
                    Log('T Unrequested feedback has arrived <BlankLineBefore>');
                    { see who it's from, etc. }
                  END;
                  GetFeedbackReply(ReadArray);
              END;
            68: { $44 }
              BEGIN
                IF (ExpectedReply = FeedbackReply) THEN
                  ExpectedDataReceived := True
                ELSE BEGIN
                  UnrequestedDataFound := True;
                  Log('T Unrequested feedback has arrived <BlankLineBefore>');
                END;
                { see who it's from, etc. }
                GetFeedbackReply(ReadArray);
              END;
            70: { $46 }
              BEGIN
                IF (ExpectedReply = FeedbackReply) THEN
                  ExpectedDataReceived := True
                ELSE BEGIN
                  UnrequestedDataFound := True;
                  Log('T Unrequested feedback has arrived <BlankLineBefore>');
                END;
                GetFeedbackReply(ReadArray);
              END;
            72: { $48 }
              BEGIN
                IF (ExpectedReply = FeedbackReply) THEN
                  ExpectedDataReceived := True
                ELSE BEGIN
                  UnrequestedDataFound := True;
                  Log('T Unrequested feedback has arrived <BlankLineBefore>');
                  Log('TG -72-');
                  WriteDataToFeedbackWindow('*72*');
                END;
                GetFeedbackReply(ReadArray);
              END;
            74: { $4A }
              BEGIN
                IF (ExpectedReply = FeedbackReply) THEN
                  ExpectedDataReceived := True
                ELSE BEGIN
                  UnrequestedDataFound := True;
                  Log('T Unrequested feedback has arrived <BlankLineBefore>');
                  Log('TG -74-');
                  WriteDataToFeedbackWindow('*74*');
                END;
                GetFeedbackReply(ReadArray);
             END;
            76: { $4C }
              BEGIN
                IF (ExpectedReply = FeedbackReply) THEN
                  ExpectedDataReceived := True
                ELSE BEGIN
                  UnrequestedDataFound := True;
                  Log('T Unrequested feedback has arrived <BlankLineBefore>');
                  Log('TG -76-');
                  WriteDataToFeedbackWindow('*76*');
                END;
                GetFeedbackReply(ReadArray);
              END;
            78: { $4E }
              BEGIN
                IF (ExpectedReply = FeedbackReply) THEN
                  ExpectedDataReceived := True
                ELSE BEGIN
                  UnrequestedDataFound := True;
                  Log('T Unrequested feedback has arrived <BlankLineBefore>');
                  Log('TG -78-');
                  WriteDataToFeedbackWindow('*78*');
                END;
                GetFeedbackReply(ReadArray);
              END;
            97: { $61 }
              { Some 97 unrequested msgs will arrive four times - just note }
              CASE ReadArray[1] OF
                0:
                  IF ExpectedReply = TrackPowerOffReply THEN
                    ExpectedDataReceived := True
                  ELSE BEGIN
                    UnrequestedDataFound := True;
                    IF NOT SystemStatus.EmergencyOff THEN
                      ReadOut('ShortCircuit');
                    SystemStatus.EmergencyOff := True;
                    ErrorMsg := '*** power off ***';
                    Log('EG '+ ErrorMsg + ' <BlankLineBefore>');
                    WriteDataToFeedbackWindow(ErrorMsg);
                  END;
                1:
                  IF ExpectedReply = EverythingTurnedOnReply THEN
                    ExpectedDataReceived := True { when would this happen? }
                  ELSE BEGIN
                    UnrequestedDataFound := True;
                    SystemStatus.EmergencyOff := False;
                    SystemStatus.EmergencyStop := False;
                    ErrorMsg := '*** power on ***';
                    Log('EG '+ ErrorMsg + ' <BlankLineBefore>');
                    WriteDataToFeedbackWindow(ErrorMsg);
                  END;
                2:
                  BEGIN
                    Log('EG Programming (Service) Mode');
                    ErrorFound := True;
                  END;
                17: { $11 }
                  BEGIN
                    ErrorMsg := 'Command station ready [Command not yet implemented]';
                    Log('EG ' + ErrorMsg);
                    ErrorFound := True;
                  END;
                18: { $12 }
                  BEGIN
                    ErrorMsg := 'Error during programming - short circuit';
                    Log('EG ' + ErrorMsg);
                    ErrorFound := True;
                  END;
                19: { $13 }
                  BEGIN
                    ErrorMsg := 'Error during programming - data byte not found';
                    Log('EG ' + ErrorMsg);
                    { *** programming should be broken off or tried again *** }
                    ErrorFound := True;
                  END;
                31: { $1F }
                  BEGIN
                    ErrorMsg := 'Command station busy [Command not yet implemented]';
                    Log('EG ' + ErrorMsg);
                    ErrorFound := True;
                  END;
                128: { $80 }
                  BEGIN
                    ErrorMsg := 'Transfer error (XOR not correct)';
                    Log('EG ' + ErrorMsg);
                    ErrorFound := True;
                    { should retransmit command **** }
                  END;
                129: { $81 }
                  BEGIN
                    ErrorMsg := 'Command station busy';
                    Log('EG ' + ErrorMsg);
                    ErrorFound := True;
    //                MakeSound(1);
                    { should retransmit command **** }
                  END;
                130: { $82 }
                  BEGIN
                    ErrorMsg := 'Instruction not supported by command station';
                    Log('EG ' + ErrorMsg);
                    ErrorFound := True;
                  END;
                131: { $83 }
                  BEGIN
                    ErrorMsg := 'Error with double header - no operation command';
                    Log('EG ' + ErrorMsg);
                    ErrorFound := True;
                  END;
                132: { $84 }
                  BEGIN
                    ErrorMsg := 'Error with double header - double header occupied';
                    Log('EG ' + ErrorMsg);
                    ErrorFound := True;
                  END;
                133: { $85 }
                  BEGIN
                    ErrorMsg := 'Error with double header - loco already in double header';
                    Log('EG ' + ErrorMsg);
                    ErrorFound := True;
                  END;
                134: { $86 }
                  BEGIN
                    ErrorMsg := 'Error with double header - locomotive still moving';
                    Log('EG ' + ErrorMsg);
                    ErrorFound := True;
                  END;
              END; {CASE}
            98: { $62 }
              CASE ReadArray[1] OF
                34: { $22 }
                  BEGIN
                    IF ExpectedReply = SystemStatusReply THEN BEGIN
                      ExpectedDataReceived := True;
                      SystemStatusStr := '';
                      { answer tells us what is currently happening }
                      IF (ReadArray[2] AND $80) = $80 THEN { 1000 0000 }
                        { bit 7 set }
                        SystemStatusStr := 'RAM check error in command station';
                      IF (ReadArray[2] AND $40) = $40 THEN { 0100 0000 }
                        { bit 6 set }
                        SystemStatusStr := 'command station powering up';
                      IF (ReadArray[2] AND $08) = $08 THEN { 0000 1000 }
                        { bit 3 set }
                        SystemStatusStr := 'in programming (service) mode';
                      IF (ReadArray[2] AND $02) = $02 THEN { 0000 0010 }
                        { bit 1 set }
                        SystemStatusStr := 'emergency stop is on';
                      IF (ReadArray[2] AND $01) = $01 THEN { 0000 0001 }
                        { bit 0 set }
                        SystemStatusStr := 'emergency off is on';

                      IF (ReadArray[2] AND $04) <> $04 THEN { 0000 0100 }
                        { bit 2 not set - this one is logged, but not written to the screen on start up }
                        Log('G Returning system status for ' + PortStr +': start mode=manual')
                      ELSE
                        SystemStatusStr := 'start mode=auto - may need to send start-mode command (see s.2.1.4 of LI101F manual)';

                      IF SystemStatusStr <> '' THEN
                        Log('G Returning system status for ' + PortStr + ': ' + SystemStatusStr);
                    END;
                  END;
              END; {CASE}
            99: { $63 }
              CASE ReadArray[1] OF
                16:  { $10 }
                  { Programming (service) mode response for Register and Page mode }
                  BEGIN
                    ErrorMsg := 'Programming service) mode response for Register & Page mode ';
                    Log('EG ' + ErrorMsg);
                    { Needs more code if to be used }
                    ErrorFound := True;
                  END;
                20: { $14 }
                  { Programming (service) mode response for Direct CV mode }
                  BEGIN
                    ErrorMsg := 'Programming (service mode) response for Direct CV mode ';
                    Log('EG ' + ErrorMsg);
                    { Needs more code if to be used }
                    ErrorFound := True;
                  END;
                33: { $21 }
                  { software version of command station (v.3) }
                  BEGIN
                    DebugStr := 'Command Station software version reply:';
                    IF ExpectedReply = CommandStationSoftwareReply THEN BEGIN
                      ExpectedDataReceived := True;
                      { answer is in BCD, 4 upper bits the tens, four lower the units }
                      TempByte := ReadArray[2] shr 4;
                      DebugStr := DebugStr + ' version=' + IntToStr(TempByte);
                      TempByte := ReadArray[2] and not $F0;
                      DebugStr := DebugStr + '.' + IntToStr(TempByte);
                      { could use following info to see if certain commands work }
                      CASE ReadArray[3] OF
                        0:
                          DebugStr := DebugStr + ' - LZ100';
                        1:
                          DebugStr := DebugStr + ' - LH200';
                        2:
                          DebugStr := DebugStr + ' - DPC';
                      END; {CASE}
                    END;
                    Log('G ' + DebugStr);
                  END;
              END; {CASE}
            129: { $81 }{ Emergency stop - msg will arrive four times }
              IF ExpectedReply = EmergencyStopReply THEN
                ExpectedDataReceived := True
              ELSE IF (ReadArray[1] = 0) THEN BEGIN
                UnrequestedDataFound := True;
                SystemStatus.EmergencyStop := True;
                ErrorMsg := '*** emergency stop ***';
                Log('EG ' + ErrorMsg + ' <BlankLineBefore>');
              END;
            198: { $C6 } { [undocumented] Information returned from double-headed locos }
              IF ExpectedReply = LocoReply THEN
                ExpectedDataReceived := True;
            225: { $E1 }
              BEGIN
                CASE ReadArray[1] OF
                  129: { $81 }
                    BEGIN
                      ErrorMsg := 'Dbl-hdr error: one loco not operated by LI101F or loco 0';
                      Log('EG ' + ErrorMsg);
                    END;
                  130: { $82 }
                    BEGIN
                      ErrorMsg := 'Dbl-hdr error: one loco operated by another device';
                      Log('EG ' + ErrorMsg);
                    END;
                  131: { $83 }
                    BEGIN
                      ErrorMsg := 'Dbl-hdr error: loco already in dbl-hdr or m/u';
                      Log('EG ' + ErrorMsg);
                    END;
                  132: { $84 }
                    BEGIN
                      ErrorMsg := 'Dbl-hdr error: speed of one loco not zero';
                      Log('EG ' + ErrorMsg);
                    END;
                  133: { $85 }
                    BEGIN
                      ErrorMsg := 'Multi-Unit error: loco not in a multi-unit';
                      Log('EG ' + ErrorMsg);
                    END;
                  134: { $86 }
                    BEGIN
                      ErrorMsg := 'Multi-Unit error: loco address not a m/u base address';
                      Log('EG ' + ErrorMsg);
                    END;
                  135: { $87 }
                    BEGIN
                      ErrorMsg := 'Dbl-Hdr/Multi-Unit error: not possible to delete loco';
                      Log('EG ' + ErrorMsg);
                    END;
                  136: { $88 }
                    BEGIN
                      ErrorMsg := 'Dbl-Hdr/Multi-Unit error: command station stack full';
                      Log('EG ' + ErrorMsg);
                    END;
                END; {CASE}
              END;
            226: { $E2 } { information returned from a multi-unit address }
              IF ExpectedReply = LocoReply THEN
                ExpectedDataReceived := True;
            227: { $E3 }
              CASE ReadArray[1] OF
                48..52: { $30-$34 }
                  { response to an address search } { *** }
                  BEGIN
                  END;
                64: { $40 }
                  BEGIN
                    { arrives unrequested if the LH100 takes over control }
                    LocoChip := GetLocoChipFromTwoBytes(ReadArray[2], ReadArray[3]);

                    { and record it }
                    Log(LocoChipToStr(LocoChip) + ' L taken over');
                    T := GetTrainRecord(LocoChip);
                    IF T <> NIL THEN BEGIN
                      T^.Train_PreviouslyControlledByProgram := True;
                      SetTrainControlledByProgram(T, False);
                    END;
                  END;
                80: { $50 }
                  { Function status response }
                  BEGIN
                    { need to write **** }
                  END;
              END; {CASE}
            228: { $E4 } { information returned from a loco under our control }
              IF ExpectedReply = LocoReply THEN
                ExpectedDataReceived := True;
            229: { $E5 } { information returned from a loco in a multi-unit }
              IF ExpectedReply = LocoReply THEN
                ExpectedDataReceived := True;
            230: { $E6 } { information returned from locos in a double header }
              IF ExpectedReply = LocoReply THEN
                ExpectedDataReceived := True;
          ELSE {CASE}
            BEGIN
              { write it out }
              UnknownReplyString := '';
              FOR I := 0 TO (CommandLen + 1) DO
                UnknownReplyString := UnknownReplyString + IntToStr(ReadArray[I])+ ' +';
              Log('XG **** Unknown reply: ' + UnknownReplyString);
            END;
          END; {CASE}

          CheckTimeOut := False;

          { Write out the debugging string and translate it into hex }
          DebugStr := DebugStr + ' ';
          FOR I := 0 TO CommandLen DO
            DebugStr := DebugStr + IntToStr(ReadArray[I]) + '-';
          DebugStr := DebugStr + IntToStr(ReadArray[CommandLen + 1]);

          { Now the hex }
          DebugStr := DebugStr + ' [';
          FOR I := 0 TO CommandLen DO
            DebugStr := DebugStr + IntToHex(ReadArray[I], 2) + '-';
          DebugStr := DebugStr + IntToHex(ReadArray[CommandLen + 1], 2);
          DebugStr := DebugStr + ']';

          { Add the appropriate Type Of Log char }
          CASE ReadArray[0] OF
            1..2:
              CASE ExpectedReply OF
                LocoAcknowledgment:
                  TypeOfLogChar := 'L';
                PointAcknowledgment:
                  TypeOfLogChar := 'P';
                SignalAcknowledgment:
                  TypeOfLogChar := 'S';
              ELSE
                TypeOfLogChar := 'G';
              END; {CASE}
            66..78:
              TypeOfLogChar := 'T'; { trackcircuit }
            198..230:
              TypeOfLogChar := 'L'; { loco }
          ELSE
            TypeOfLogChar := 'G'; { general }
          END; {CASE}

          IF ResponseOrBroadcast = Response THEN
            Log(TypeOfLogChar + ' ' + StringOfChar(' ', 104) + 'Lenz response: ' + DebugStr)
          ELSE
            IF ResponseOrBroadcast = Broadcast THEN
              Log('G ' + StringOfChar(' ', 104) + 'Lenz broadcast: ' + DebugStr);

          { Write out bytes as bits if run-time parameter Y is set }
          IF ShowByteParam <> '' THEN BEGIN
            IF ShowByteParam = 'ALL' THEN BEGIN
              { Write all the bytes as bits }
              DebugStr := '';
              FOR I := 1 TO (CommandLen + 1) DO
                DebugStr := DebugStr + '[' + DoBitPattern(ReadArray[I]) + '] ';
              Log('G ' + StringOfChar(' ', 64) + 'All bytes: ' + DebugStr); { 64 shouldn't be a magic number *** }
            END ELSE
              IF (StrToInt(ShowByteParam) >= 0)
              AND (StrToInt(ShowByteParam) <= 14)
              THEN BEGIN
                { Write a single byte as bits }
                IF StrToInt(ShowByteParam) > (CommandLen + 1) THEN
                  DebugStr := '---- ----'
                ELSE
                  DebugStr := DoBitPattern(ReadArray[StrToInt(ShowByteParam)]);

                IF ResponseOrBroadcast = Response THEN
                  Log('G *** Response ***' + StringOfChar(' ', 48) + 'Byte ' + ShowByteParam + ': [' + DebugStr + ']')
                ELSE
                  IF ResponseOrBroadcast = Broadcast THEN
                    Log('G *** Broadcast ***' + StringOfChar(' ', 47) + 'Byte ' + ShowByteParam + ': [' + DebugStr + ']');
              END;
          END;

          { loop if necessary - not needed if no reply was awaited, or if the reply received was the one that was expected }
        END;
      UNTIL (ExpectedReply = NoReplyExpected) OR ExpectedDataReceived OR TimedOut OR ErrorFound;

      IF UnRequestedDataFound THEN BEGIN
  //    Debug('UnRequestedDataFound');
        IF TestingMode THEN
          DebugWindow.Caption := DebugWindow.Caption + '.';
        IF NOT SystemOnline THEN
          SetSystemOnline;
      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG : ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DataIOMainProcedure }
{$O+}

PROCEDURE DataIO{1}(WriteRead : WriteReadType); Overload
{ This is called by the regular check for broadcasts }
VAR
  CheckTimeOut : Boolean;
  ExpectedDataReceived : Boolean;
  ExpectedReply : ReplyType;
  ReadArray : ARRAY[0..15] OF Byte;
  WriteArray : ARRAY[0..15] OF Byte;

BEGIN
  CheckTimeOut := False;
  DataIOMainProcedure('G', WriteArray, ReadArray, NoReplyExpected, CheckTimeOut, WriteRead, ExpectedDataReceived);
END; { DataIO-1 }

PROCEDURE DataIO{2}(TypeOfLogChar : Char; WriteArray : ARRAY OF Byte; ExpectedReply : ReplyType; WriteRead : WriteReadType; OUT ExpectedDataReceived : Boolean); Overload;
VAR
  CheckTimeOut : Boolean;
  ReadArray : ARRAY[0..15] OF Byte;

BEGIN
  CheckTimeOut := False;
  DataIOMainProcedure(TypeOfLogChar, WriteArray, ReadArray, ExpectedReply, CheckTimeOut, WriteThenRead, ExpectedDataReceived);
END; { DataIO -2 }

PROCEDURE DataIO{3}(TypeOfLogChar : Char; WriteArray : ARRAY OF Byte; VAR ReadArray : ARRAY OF Byte; ExpectedReply : ReplyType; OUT ExpectedDataReceived : Boolean); Overload;
VAR
  CheckTimeOut : Boolean;

BEGIN
  DataIOMainProcedure(TypeOfLogChar, WriteArray, ReadArray, ExpectedReply, CheckTimeOut, WriteThenRead, ExpectedDataReceived);
END; { DataIO-3 }

PROCEDURE XWriteOutData(TypeOfLogChar : Char; VAR WriteArray : ARRAY OF Byte; VAR TimeOut : Boolean);
{ Writes out data }
CONST
  ExitProgram = True;
  WarnUser = True;

VAR
  CommandLen : Integer;
  DebugStr : String;
  I : Integer;
  S : String;

BEGIN
  TimeOut := False;

  { Send any output waiting to the LI101 - check first that it can be accepted }
  IF WriteArray[0] <> 0 THEN BEGIN
    { get the length first from bits 3-0, and add the checkbyte }
    WriteArray[GetCommandLen(WriteArray[0]) + 1] := CheckSum(WriteArray);
    { and write it out }
    S := '';
    FOR I := 0 TO GetCommandLen(WriteArray[0]) + 1 DO
      S := S + IntToHex(WriteArray[I], 2);
    TCPIPForm.ResponsesTCPSendText(S);
  END;

  { Now compose the "From PC" string }
  DebugStr := StringOfChar(' ', 104) + 'PC request: '; { 104 shouldn't be a magic number *** }

  CommandLen := GetCommandLen(WriteArray[0]);

  FOR I := 0 TO CommandLen DO
    DebugStr := DebugStr + IntToStr(WriteArray[I]) + '-';
  DebugStr := DebugStr + IntToStr(WriteArray[CommandLen + 1]);

  { now the hex }
  DebugStr := DebugStr + ' [';
  FOR I := 0 TO CommandLen DO
    DebugStr := DebugStr + IntToHex(WriteArray[I], 2) + '-';
  DebugStr := DebugStr + IntToHex(WriteArray[CommandLen + 1], 2);
  DebugStr := DebugStr + ']';

  Log(TypeOfLogChar + ' ' + DebugStr); {+ ' byte 2= ' + DoBitPattern(WriteArray[2]) + ' byte 4= ' + DoBitPattern(WriteArray[4]));}
END; { WriteOutData }

PROCEDURE DoCheckForUnexpectedData(UnitRef : String; CallingStr : String);
{ See if any data has arrived }
BEGIN
  IF SystemOnline THEN
    DataIO(ReadOnly);
END; { DoCheckForUnexpectedData }

PROCEDURE ProgramOnTheMain(LocoChip : Integer; ProgramOnTheMainRequest : ProgramOnTheMainType; NewValue : Integer);
{ Program a loco anywhere on the layout (i.e. not on the programming track) }
VAR
  OK : Boolean;
  T : Train;
  TimeOut : Boolean;

BEGIN
  T := GetTrainRecord(LocoChip);
  IF T <> NIL THEN BEGIN
    WriteArray[0] := 230; { header byte }
    WriteArray[1] := 48;  { identification byte }
    WriteArray[2] := GetLocoChipHighByte(LocoChip); { data byte 1 - AH }
    WriteArray[3] := GetLocoChipLowByte(LocoChip);  { data byte 2 - AL }

    CASE ProgramOnTheMainRequest OF
      ChangeDirectionToUp:
        BEGIN
          Log(LocoChipToStr(LocoChip) + ' LG Programming on the Main : changing direction to up');

          { a bit mode write request: change of direction is CV 29 bit 0, so need to send 29-1, as CVs start here at 0 }
          WriteArray[4] := 232; { data byte 3 : 1110 1000 (1110 10CC where CC is top two bits of CV 0-1023) }
          WriteArray[5] := 28;  { data byte 4 : 0001 1100 - CCCC CCCC - remaining bits of CV }
          WriteArray[6] := 240; { data byte 5 : 1111 0000 - 1111 WBBB - W=bit value 0 or 1, BBB=bit location (000=bit 0, 111=bit 7) }
        END;
      ChangeDirectionToDown:
        BEGIN
          Log(LocoChipToStr(LocoChip) + ' LG Programming on the Main : changing direction to down');

          { a bit mode write request: change of direction is CV 29 bit 0, so need to send 29-1, as CVs start here at 0 }
          WriteArray[4] := 232; { data byte 3 : 1110 1000 (1110 10CC where CC is top two bits of CV 0-1023) }
          WriteArray[5] := 28;  { data byte 4 : 0001 1100 - CCCC CCCC - remaining bits of CV }
          WriteArray[6] := 248; { data byte 5 : 1111 1000 - 1111 WBBB - W=bit value 0 or 1, BBB=bit location (000=bit 0, 111=bit 7) }
        END;
      ChangeAcceleration:
        { a byte mode write request: change of acceleration is CV 3, so need to send 3-1, as CVs start here at 0 }
        BEGIN
          Log(LocoChipToStr(LocoChip) + ' LG Programming on the Main : changing acceleration to ' + IntToStr(NewValue));

          WriteArray[4] := 236;      { data byte 3 : 1110 1100 (1110 11CC where CC is top two bits of CV 0-1023) }
          WriteArray[5] := 2;        { data byte 4 : 0000 0010 - CCCC CCCC - remaining bits of CV }
          WriteArray[6] := NewValue; { data byte 5 : the new value of the CV }
        END;
      ChangeDeceleration:
        { a byte mode write request: change of acceleration is CV 4, so need to send 4-1, as CVs start here at 0 }
        BEGIN
          Log(LocoChipToStr(LocoChip) + ' LG Programming on the Main : changing deceleration to ' + IntToStr(NewValue));

          WriteArray[4] := 236;      { data byte 3 : 1110 1100 (1110 11CC where CC is top two bits of CV 0-1023) }
          WriteArray[5] := 3;        { data byte 4 : 0000 0011 - CCCC CCCC - remaining bits of CV }
          WriteArray[6] := NewValue; { data byte 5 : the new value of the CV }
        END;
    END; {CASE}

    DataIO('L', WriteArray, ComputerInterfaceSoftwareReply, WriteOnly, OK);
  END;
END; { ProgramOnTheMain }

PROCEDURE ReadInLocoDetails(LocoChip : Integer; VAR TempSpeedByte : Byte; OUT OK : Boolean);
{ Read in the supplied details of the specified loco - error if not }

  FUNCTION Decode128SpeedStep(TempSpeedByte : Byte) : Integer;
  { Decode the speed - new 128 step (v.3) rate }
  BEGIN
    { Clear top bit first }
    IF TempSpeedByte = 0 THEN
      Result := 0
    ELSE
      IF TempSpeedByte = 0 THEN
        Result := -1 { emergency stop *** }
      ELSE
        { don't include the top bit in the calculation }
        Result := (TempSpeedByte AND NOT $80) - 1; { 128 - 1000 0000 }
  END; { Decode128SpeedStep }

  FUNCTION Decode28SpeedStep(TempSpeedByte : Byte) : Integer;
  { Decode the speed }
  CONST
    ExitProgram = True;

  VAR
    TestByte : Byte;
    Counter : Integer;
    FoundSpeed : Boolean;
    Speed : Integer;

  BEGIN
    Speed := 0;

    { For speedstepmode of 28. Odd system - presumably because 28 speed mode added later: speed 1 is 00010, All even nos have bit 4 set and the same value as the previous
      odd number so 2 = 10010. 3 is then 00011, 4 10011 etc.
    }
    { Clear top three bits for the test }
    TestByte := TempSpeedByte AND NOT $E0; { 224 - 1110 0000 }

    Counter := 0;
    FoundSpeed := False;
    REPEAT
      { Look up the speed in the speed table to check it's valid }
      IF TestByte = 1 THEN BEGIN
        { an emergency stop, so not in the speed table }
        Speed := 0;
        FoundSpeed := True;
      END ELSE
        IF TestByte = SpeedStep28Table[Counter] THEN BEGIN
          { if there's a match, record it }
          Speed := Counter;
          FoundSpeed := True;
        END;
      IF Counter = 29 THEN BEGIN
        Debug('Error - speed not in table!');
        { ***** what else? }
      END;
      Inc(Counter);
    UNTIL (Counter = 29) OR FoundSpeed;
    Result := Speed;
  END; { Decode28SpeedStep }

CONST
  ExitProgram = True;

VAR
  DebugStr : String;
  FunctionNum : Integer;
  IDByte : Byte;
  ReadArray, WriteArray : ARRAY [0..ReadArrayLen] OF Byte;
  SpeedNum : Integer;
  T : Train;
  TestByte : Byte;
  TimeOut : Boolean;

BEGIN
  TestByte := 0;

  T := GetTrainRecord(LocoChip);
  IF T <> NIL THEN BEGIN
    WITH T^ DO BEGIN
      Log(LocoChipToStr(LocoChip) + ' L Requesting loco details <BlankLineBefore>');

      WriteArray[0] := 227;
      WriteArray[1] := 0;
      WriteArray[2] := GetLocoChipHighByte(LocoChip);
      WriteArray[3] := GetLocoChipLowByte(LocoChip);

      DataIO('L', WriteArray, ReadArray, LocoReply, OK);

      IF OK THEN BEGIN
        DebugStr := 'Received loco details:';

        CASE ReadArray[0] OF
          226:
            Log(LocoChipToStr(LocoChip) + ' L (Multi-unit address');
              { but will need to store speed of multi-unit separately ? **** }
          229:
            Log(LocoChipToStr(LocoChip) + ' L (Loco forming part of a multi-unit with a multi-unit address of ' + IntToStr(ReadArray[5]) + ')');
              { but will need to store speed of multi-unit separately ? **** }
          230:
            { report if forming part of a double header }
            Log(LocoChipToStr(LocoChip) + ' L (Loco forming part of a double header with ' + IntToStr(GetLocoChipFromTwoBytes(ReadArray[5], ReadArray[6])) + ')');
        END; {CASE}

        CASE ReadArray[0] OF
          226, 228..230:
            BEGIN
              IDByte := ReadArray[1];

              { See if loco controlled by another device **** }
              IF (IDByte AND 8) = 8 THEN BEGIN
                { testing bit 3 - loco controlled by something else }
                SetTrainControlledByProgram(T, False);
                DebugStr := DebugStr + ' [previously taken over]';
              END ELSE
                SetTrainControlledByProgram(T, True);

              { get its speed step mode from bits 0-2 }
              IF (IDByte AND 4) = 4 THEN
                Train_SpeedStepMode := 128
              ELSE
                IF (IDByte AND 2) = 2 THEN
                  Train_SpeedStepMode := 28
                ELSE
                  IF (IDByte AND 1) = 1 THEN
                    Train_SpeedStepMode := 27
                  ELSE
                    IF (IDByte AND 0) = 0 THEN
                      Train_SpeedStepMode := 14;

              IF (Train_SpeedStepMode <> 128)
              AND (Train_SpeedStepMode <> 28)
              THEN BEGIN
                Debug('Error - using wrong speed mode!');
                { what else **** }
              END;

              TempSpeedByte := ReadArray[2];
              Train_SpeedByte := TempSpeedByte;
              Train_SpeedByteReadIn := True;

              { and now its speed }
              IF Train_SpeedStepMode = 28 THEN
                SpeedNum := Decode28SpeedStep(TempSpeedByte)
              ELSE
                SpeedNum := Decode128SpeedStep(TempSpeedByte);

              Train_CurrentLenzSpeed := SpeedNum;

              DebugStr := DebugStr + ' speed=' + IntToStr(SpeedNum);

              { and direction of travel }
              IF (TempSpeedByte AND 128) = 128 THEN BEGIN
                Train_CurrentDirection := Up;
                DebugStr := DebugStr + ' Up'
              END ELSE BEGIN
                Train_CurrentDirection := Down;
                DebugStr := DebugStr + ' Down';
              END;

              { and loco functions set }
              IF ReadArray[0] <> 226 THEN BEGIN
                { add the data to the loco record - not applicable to multi-unit addresses *** ? *** }
                Train_Functions0To4Byte := ReadArray[3];
                Train_Functions5To12Byte := ReadArray[4];
                FOR FunctionNum := 0 TO 4 DO BEGIN
                  { see if particular bits are set }
                  CASE FunctionNum OF
                    0:
                      TestByte := $10; { 0001 0000 }
                    1:
                      TestByte := $01; { 0000 0001 }
                    2:
                      TestByte := $02; { 0000 0010 }
                    3:
                      TestByte := $04; { 0000 0100 }
                    4:
                      TestByte := $08; { 0000 1000 }
                  END; {CASE}
                  IF ((Train_Functions0To4Byte AND TestByte) = TestByte) THEN
                    Train_Functions[FunctionNum] := True
                  ELSE
                    Train_Functions[FunctionNum] := False;
                END; {FOR}
                FOR FunctionNum := 5 TO 12 DO BEGIN
                  CASE FunctionNum OF
                    5:
                      TestByte := $01; { 0000 0001 }
                    6:
                      TestByte := $02; { 0000 0010 }
                    7:
                      TestByte := $04; { 0000 0100 }
                    8:
                      TestByte := $08; { 0000 1000 }
                    9:
                      TestByte := $01; { 0000 0001 }
                    10:
                      TestByte := $02; { 0000 0010 }
                    11:
                      TestByte := $04; { 0000 0100 }
                    12:
                      TestByte := $08; { 0000 1000 }
                  END; {CASE}
                  IF ((Train_Functions5To12Byte AND TestByte) = TestByte) THEN
                    Train_Functions[FunctionNum] := True
                  ELSE
                    Train_Functions[FunctionNum] := False;
                END; {FOR}
              END;
          END;
        END; {CASE}

        CASE ReadArray[0] OF
          229:
            { **** multi-unit address (between 1 and 99) is in byte 4 - speed and direction commands should be sent to the multi-unit address (function instructions to the
              loco address 2.1.14.2)
            }
            ;
        END; {CASE}
        Log(LocoChipToStr(LocoChip) + ' L ' + DebugStr);
      END;
    END; {WITH}
  END;
END; { ReadInLocoDetails }

PROCEDURE ReadInFunctionDecoderDetails(DecoderNum : Integer; OUT TempF0_F4_Byte, TempF5_F12_Byte : Byte; VAR OK : Boolean);
{ Read in the supplied details of the specified function decoder - error if not }
{ not used as of 24/8/05 **** }
CONST
  ExitProgram = True;

VAR
  DebugStr : String;
  DecoderNumString : String;
  ReadArray, WriteArray : ARRAY [0..ReadArrayLen] OF Byte;
  TimeOut : Boolean;

BEGIN
  { Ask for details }
  DecoderNumString := LocoChipToStr(DecoderNum);
  Log('L L=' + DecoderNumString + ': requesting function decoder details <BlankLineBefore>');
  WriteArray[0] := 227;
  WriteArray[1] := 0;
  WriteArray[2] := GetLocoChipHighByte(DecoderNum);
  WriteArray[3] := GetLocoChipLowByte(DecoderNum);

  DataIO('L', WriteArray, ReadArray, LocoReply, OK);

  IF OK THEN BEGIN
    DebugStr := 'Received decoder details: ';

    IF ReadArray[0] = 228 THEN BEGIN
      { any functions set? }
      TempF0_F4_Byte := ReadArray[3];
      TempF5_F12_Byte := ReadArray[4];
    END;
  END;
  IF NOT OK THEN
    Debug('Not OK');
END; { ReadInFunctionDecoderDetails }

FUNCTION LocoHasBeenTakenOverByProgram(LocoChip : Integer) : Boolean;
{ Returns true if the loco has been taken over by the program }
VAR
  T : Train;

BEGIN
  Result := False;

  T := GetTrainRecord(LocoChip);
  IF T <> NIL THEN BEGIN
    WITH T^ DO BEGIN
      IF Train_ControlledByProgram THEN BEGIN
        Train_PreviouslyControlledByProgram := False;
        Train_TakenOverByUserMsgWritten := False;
        Result := True;
      END;
    END; {WITH}
  END;
END; { LocoHasBeenTakenOverByProgram }

FUNCTION LocoHasBeenTakenOverByUser(LocoChip : Integer) : Boolean;
{ Returns true if the loco has been taken over by an LH100 }
VAR
  T : Train;

BEGIN
  Result := False;

  T := GetTrainRecord(LocoChip);
  IF T <> NIL THEN BEGIN
    WITH T^ DO BEGIN
      IF NOT Train_ControlledByProgram
      AND Train_PreviouslyControlledByProgram
      THEN BEGIN
        Train_PreviouslyControlledByProgram := True;
        IF NOT Train_TakenOverByUserMsgWritten THEN BEGIN
          Log(LocoChipToStr(LocoChip) + ' LG taken over by user');
          Train_TakenOverByUserMsgWritten := True;
        END;
        Result := True;
      END;
    END; {WITH}
  END;
END; { LocoHasBeenTakenOver }

FUNCTION GetLenzSpeed(LocoChip : Integer; ForceRead : Boolean) : Integer;
{ Returns the given loco's speed - if ForceRead set, reads it in even if we think we know what it is }
VAR
  OK : Boolean;
  T : Train;
  TempSpeedByte : Byte;

BEGIN
  { First check whether it's been ever been asked for, whether it's been taken over by another controller and changed - otherwise no point continually asking the system for
    it.
  }
  Result := 0;

  IF LocoHasBeenTakenOverByUser(LocoChip) OR ForceRead THEN BEGIN
    ReadInLocoDetails(LocoChip, TempSpeedByte, OK);
    IF NOT OK THEN
      Log(LocoChipToStr(LocoChip) + ' XG "GetLenzSpeed" routine failed');
  END;
  { Either return the newly obtained speed, or the stored speed }
  T := GetTrainRecord(LocoChip);
  IF T <> NIL THEN
    Result := T^.Train_CurrentLenzSpeed;
//    Result := T^.Train_LenzSpeed;
END; { GetLenzSpeed }

PROCEDURE WriteLocoSpeedOrDirection(LocoChip : Integer; TempSpeedByte : Byte; VAR OK : Boolean);
{ Write out the speed or direction of a given locomotive }
VAR
  T : Train;
  TimeOut : Boolean;

BEGIN
  T := GetTrainRecord(LocoChip);
  IF T <> NIL THEN BEGIN
    WriteArray[0] := 228;
    IF T^.Train_SpeedStepMode = 28 THEN
      WriteArray[1] := 18
    ELSE
      { 128 speed steps }
      WriteArray[1] := 19;

    WriteArray[2] := GetLocoChipHighByte(LocoChip);
    WriteArray[3] := GetLocoChipLowByte(LocoChip);
    WriteArray[4] := TempSpeedByte;

    { Now read details in - look for acknowledgment }
    DataIO('L', WriteArray, ReadArray, LocoAcknowledgment, OK);

    IF NOT OK THEN
      Log(LocoChipToStr(LocoChip) + ' L Data not acknowledged')
    ELSE BEGIN
      SetTrainControlledByProgram(T, True);
      T^.Train_SpeedByte := TempSpeedByte;
    END;
    Log(LocoChipToStr(LocoChip) + ' L Data acknowledged');
  END;
END; { WriteLocoSpeedOrDirection }

PROCEDURE ReadComputerInterfaceSoftwareVersion(VAR OK : Boolean);
{ Ask the LI101 its own software version }
BEGIN
  Log('G Requesting computer interface software version <BlankLineBefore>');
  WriteArray[0] := 240;
  DataIO('G', WriteArray, ComputerInterfaceSoftwareReply, WriteThenRead, OK);
END; { ReadComputerInterfaceSoftwareVersion }

PROCEDURE ReadCommandStationSoftwareVersion(VAR OK : Boolean);
{ Ask the LI101 for the software version of the command station }
BEGIN
  Log('G Requesting command station software version <BlankLineBefore>');
  WriteArray[0] := 33;
  WriteArray[1] := 33;
  DataIO('G', WriteArray, CommandStationSoftwareReply, WriteThenRead, OK);
END; { ReadCommandStationSoftwareVersion }

FUNCTION RequestProgrammingModeCV(CV : Integer) : String;      { half written 1/2/13 }
{ Ask for programming mode (aka service mode) data }
VAR
  OK : Boolean;
  TimeOut : Boolean;

BEGIN
  IF (CV < 1) OR (CV > 256) THEN
    ShowMessage('CVs should be between 1 and 256')
  ELSE BEGIN
    IF CV = 256 THEN
      CV := 0;

    Log('G Initialising request for CV from loco on programming track - direct mode CV read request <BlankLineBefore>');
    WriteArray[0] := 34;
    WriteArray[1] := 21;
    WriteArray[2] := CV;

    DataIO(WriteOnly);

    Log('G Finalising request for CV from loco on programming track - requesting result from programming mode <BlankLineBefore>');
    WriteArray[0] := 33;
    WriteArray[1] := 16;
    WriteArray[2] := 49;

    DataIO('G', WriteArray, ReadArray, ProgrammingModeReply, OK);

    ResumeOperations(OK);
  END;
END; { RequestProgrammingModeCV }

PROCEDURE SetLenzSpeed(LocoChip, DoubleHeaderLocoChip : Integer; LenzSpeed : Integer; TrainDirection : DirectionType; QuickStopFlag : Boolean; VAR OK : Boolean);
{ Sets the speed by changing bits 4 - 0 - if quickstop, don't bother to read in the loco details first, as we're not interested in what the speed used to be }
VAR
  DebugStr : String;
  DoubleHeaderT : Train;
  T : Train;
  TempSpeedByte : Byte;

BEGIN
  TRY
    OK := True;
    IF LocoChip <> UnknownLocoChip THEN BEGIN
      T := GetTrainRecord(LocoChip);
      IF T = NIL THEN
        Log('No train record for locochip ' + LocoChipToStr(LocoChip))
      ELSE BEGIN
        WITH T^ DO BEGIN
          IF (T^.Train_SpeedStepMode = 28)
          AND (LenzSpeed > 28)
          THEN BEGIN
            Log(LocoChipToStr(LocoChip) + ' XG  Fatal Error: Lenz speed ' + IntToStr(LenzSpeed) + ' supplied');
            OK := False;
          END;
          IF DoubleHeaderLocoChip <> UnknownLocoChip THEN BEGIN
            DoubleHeaderT := GetTrainRecord(DoubleHeaderLocoChip);
            IF (DoubleHeaderT^.Train_SpeedStepMode = 28)
            AND (LenzSpeed > 28)
            THEN BEGIN
              Log(LocoChipToStr(DoubleHeaderLocoChip) + ' XG Fatal Error: Lenz speed ' + IntToStr(LenzSpeed) + ' supplied');
              OK := False;
            END;
          END;

          IF OK THEN BEGIN
            IF NOT SystemOnline THEN BEGIN
              Debug('Speed for loco ' + LocoChipToStr(LocoChip) + ' would be set to ' + IntToStr(LenzSpeed));
              IF DoubleHeaderLocoChip <> UnknownLocoChip THEN
                Debug('Speed for loco ' + LocoChipToStr(LocoChip) + ' and for double header loco ' + LocoChipToStr(DoubleHeaderLocoChip)
                      + ' would be set to ' + IntToStr(LenzSpeed))
            END ELSE BEGIN
              DebugStr := '';
              OK := False;
              { If we're controlling it, we know its speed }
              IF T^.Train_SpeedByteReadIn
              AND NOT LocoHasBeenTakenOverByUser(LocoChip)
              THEN
                TempSpeedByte := T^.Train_SpeedByte
              ELSE BEGIN
                ReadInLocoDetails(LocoChip, TempSpeedByte, OK);
                IF NOT OK THEN
                  Log(LocoChipToStr(LocoChip) + ' XG Locoread not ok-'); { ****** }
              END;

              IF NOT QuickStopFlag THEN BEGIN
                { now set the appropriate ones }
                IF T^.Train_SpeedStepMode = 28 THEN BEGIN
                  { first clear bits 0-4 }
                  TempSpeedByte := TempSpeedByte AND NOT $1F; { 0001 1111 }
                  { and set new ones }
                  TempSpeedByte := TempSpeedByte OR SpeedStep28Table[LenzSpeed];
                END ELSE BEGIN
                  { SpeedStepMode = 128 }
                  { first clear bits 0-6 }
                  TempSpeedByte := TempSpeedByte AND NOT $7F; { 0111 1111 }
                  { and set new ones }
                  TempSpeedByte := TempSpeedByte OR (LenzSpeed + 1);
                END;
              END ELSE BEGIN
                { quickstop }
                TempSpeedByte := T^.Train_SpeedByte;
                { reset bits 1-6, leaving bit 7 as it indicates direction }
                TempSpeedByte := TempSpeedByte AND NOT 126; { 0111 1110 }
                { set bit 0 for emergency stop }
                TempSpeedByte := TempSpeedByte OR 1; { 0000 0001 }
              END;

              { set top bit for direction }
              IF TrainDirection = Up THEN BEGIN
                TempSpeedByte := TempSpeedByte OR 128; { 1000 000 }
                DebugStr := ' Up'
              END ELSE BEGIN
                TempSpeedByte := TempSpeedByte AND NOT 128; { 0000 000 }
                DebugStr := ' Down';
              END;

              Log(LocoChipToStr(LocoChip) + ' L Writing speed data ' + IntToStr(LenzSpeed) + ' [' + DoBitPattern(TempSpeedByte) + ']' + DebugStr);
              { now write the byte back }
              WriteLocoSpeedOrDirection(LocoChip, TempSpeedByte, OK);
              IF NOT OK THEN
                Log(LocoChipToStr(LocoChip) + ' LG Data for loco ' + LocoChipToStr(LocoChip) + ' not written');

              IF OK THEN BEGIN
                IF DoubleHeaderLocoChip <> UnknownLocoChip THEN BEGIN
                  { this makes use of the calculations above - we may have to do them again for the double header loco **** 11/10/06 }
                  Log(LocoChipToStr(DoubleHeaderLocoChip) + ' L Writing speed data ' + IntToStr(LenzSpeed) + ' [' + DoBitPattern(TempSpeedByte) + ']' + DebugStr);
                  WriteLocoSpeedOrDirection(DoubleHeaderLocoChip, TempSpeedByte, OK);
                  IF NOT OK THEN
                    Log(LocoChipToStr(DoubleHeaderLocoChip) + ' LG Data for loco ' + LocoChipToStr(DoubleHeaderLocoChip) + ' not written');
                END;
                T^.Train_CurrentLenzSpeed := LenzSpeed;

                { Need to update the loco direction variable, as there may well not have been an explicit direction change by means of SetDirection, rather a speed change
                  with the direction bit set differently.
                }
                IF TrainDirection <> T^.Train_CurrentDirection THEN
                  T^.Train_CurrentDirection := TrainDirection;
              END;
            END;
          END;
        END; {WITH}
      END;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG SetLenzSpeed: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SetLenzSpeed }

FUNCTION AdjustLenzSpeed(LocoChip, DoubleHeaderLocoChip : Integer; Value : Integer; TrainDirection : DirectionType; OUT OK : Boolean) : Integer;
{ Increase or decrease the speed by given amount }
VAR
  NewSpeed : Integer;

BEGIN
  NewSpeed := 0;
  IF NOT SystemOnline THEN BEGIN
    IF Value > 0 THEN BEGIN
      Debug('Speed for ' + IntToStr(LocoChip) + ' would be increased by ' + IntToStr(Value));
      IF DoubleHeaderLocoChip = UnknownLocoChip THEN
        Debug('Speed for ' + IntToStr(DoubleHeaderLocoChip) + ' would also be increased by ' + IntToStr(Value));
    END ELSE
      IF Value < 0 THEN BEGIN
        Debug('Speed for ' + IntToStr(LocoChip) + ' would be decreased by ' + IntToStr(Abs(Value)));
        IF DoubleHeaderLocoChip <> UnknownLocoChip THEN
          Debug('Speed for ' + IntToStr(DoubleHeaderLocoChip) + ' would be also decreased by ' + IntToStr(Value));
      END ELSE BEGIN
        Debug('val=0');
        Log(LocoChipToStr(LocoChip) + ' L Can''t increase or decrease by zero');
      END;
    OK := True;
  END ELSE BEGIN
    IF Value = 0 THEN BEGIN
      { can't increase or decrease by zero }
      Debug('val=0');
      Log(LocoChipToStr(LocoChip) + ' L Can''t increase or decrease by zero');
    END ELSE BEGIN
      NewSpeed := GetLenzSpeed(LocoChip, NOT ForceARead);
      NewSpeed := NewSpeed + Value;

      IF NewSpeed > LocoMaxSpeed THEN
        NewSpeed := LocoMaxSpeed
      ELSE
        IF NewSpeed < LocoMinSpeed THEN
          NewSpeed := LocoMinSpeed;

      SetLenzSpeed(LocoChip, DoubleHeaderLocoChip, NewSpeed, TrainDirection, NOT QuickStop, OK);
    END;
  END;
  Result := NewSpeed;
END; { AdjustLenzSpeed }

PROCEDURE SetDirection(LocoChip, DoubleHeaderLocoChip : Integer; DirectionRequired : DirectionType; OUT OK : Boolean);
{ Sets/resets bit 7 - up is (arbitrarily) on }
VAR
  DebugStr : String;
  T : Train;
  TempSpeedByte : Byte;
  TestByte : Byte;

BEGIN
  OK := True;

  IF DirectionRequired = Up THEN
    DebugStr := 'Setting loco direction to Up'
  ELSE
    DebugStr := 'Setting loco direction to Down';
  Log(LocoChipToStr(LocoChip) + ' L ' + DebugStr);

  IF SystemOnline THEN BEGIN
    IF LocoChip <> UnknownLocoChip THEN BEGIN
      T := GetTrainRecord(LocoChip);
      IF T = NIL THEN
        Log(LocoChipToStr(LocoChip) + ' L No train record for locochip ' + LocoChipToStr(LocoChip))
      ELSE BEGIN
        IF T^.Train_SpeedByteReadIn
        AND NOT LocoHasBeenTakenOverByUser(LocoChip)
        THEN
          TempSpeedByte := T^.Train_SpeedByte
        ELSE
          ReadInLocoDetails(LocoChip, TempSpeedByte, OK);

        TestByte := TempSpeedByte;

        { Needs to check loco is stationary first - check by looking at bits }
        IF T^.Train_SpeedStepMode = 28 THEN
          { clear the top 3 bits to do the test }
          TestByte := TestByte AND NOT $E0 { 1110 0000 }
        ELSE { SpeedStepMode = 128 }
          { clear the top bit to do the test }
          TestByte := TestByte AND NOT $80; { 1000 0000 }

        IF (TestByte = 0) { stationary }
        OR (TestByte = 1) { emergency stopped }
        THEN BEGIN
          { set top bit for direction }
          IF DirectionRequired = Up THEN BEGIN
            TempSpeedByte := TempSpeedByte OR 128; { 1000 000 }
            DebugStr := 'Up'
          END ELSE BEGIN
            TempSpeedByte := TempSpeedByte AND NOT 128; { 0000 000 }
            DebugStr := 'Down';
          END;

          WriteLocoSpeedOrDirection(LocoChip, TempSpeedByte, OK);
          IF NOT OK THEN BEGIN
            WriteLocoSpeedOrDirection(LocoChip, TempSpeedByte, OK);
            IF NOT OK THEN BEGIN
              StopAllLocomotives(OK);
              MakeSound(1);
              TurnAutoModeOff(NOT ByUser);
              Showmessage('Data for loco ' + LocoChipToStr(LocoChip) + ' not written - auto mode suspended');
            END;
          END;

          IF OK THEN BEGIN
            IF DoubleHeaderLocoChip <> UnknownLocoChip THEN BEGIN
              WriteLocoSpeedOrDirection(DoubleHeaderLocoChip, TempSpeedByte, OK);
              IF NOT OK THEN BEGIN
                StopAllLocomotives(OK);
                MakeSound(1);
                TurnAutoModeOff(NOT ByUser);
                Showmessage('Data for loco ' + LocoChipToStr(DoubleHeaderLocoChip) + ' not written - auto mode suspended');
              END;
            END;

            Log(LocoChipToStr(LocoChip) + ' L Loco direction changed to ' + DebugStr);
            T^.Train_SpeedByte := TempSpeedByte;
          END ELSE
            Log(LocoChipToStr(LocoChip) + ' L Loco direction not changed');
        END ELSE BEGIN
          Log(LocoChipToStr(LocoChip) + ' LG Loco direction not changed - loco not stationary');
          OK :=  False;
        END;
      END;
    END;
  END;
END; { SetDirection }

PROCEDURE SetTrainDirection(T : Train; DirectionRequired : DirectionType; ForceWrite : Boolean; VAR OK : Boolean);
{ Sets/resets bit 7 - up is (arbitrarily) on }
VAR
  TempSpeedByte : Byte;

BEGIN
  OK := True;
  IF (T <> NIL)
  AND SystemOnline
  THEN BEGIN
    IF (LocoHasBeenTakenOverByUser(T^.Train_LocoChip) OR (T^.Train_CurrentDirection = UnknownDirection)) THEN
      ReadInLocoDetails(T^.Train_LocoChip, TempSpeedByte, OK);

    IF (DirectionRequired <> T^.Train_CurrentDirection) OR ForceWrite THEN BEGIN
      SetDirection(T^.Train_LocoChip, T^.Train_DoubleHeaderLocoChip, DirectionRequired, OK);
      IF OK THEN
        T^.Train_CurrentDirection := DirectionRequired;
    END;
  END;
END; { SetTrainDirection }

PROCEDURE GetLocoFunctions(LocoChip : Integer; ForceRead : Boolean; VAR FunctionArray : ARRAY OF Boolean; VAR OK : Boolean);
{ Read all the functions }
VAR
  DebugStr : String;
  FunctionNum : Integer;
  T : Train;
  TempSpeedByte : Byte;
  TestByte : Byte;

BEGIN
  TestByte := 0;
  OK := True;
  IF LocoChip <> UnknownLocoChip THEN BEGIN
    T := GetTrainRecord(LocoChip);
    IF T <> NIL THEN BEGIN
      IF LocoHasBeenTakenOverByuser(LocoChip) OR ForceRead THEN
        ReadInLocoDetails(LocoChip, TempSpeedByte, OK);

      FOR FunctionNum := 0 TO 12 DO BEGIN
        { Now see if a particular bit is set (different set of values for 9-12 from writing out functions) }
        CASE FunctionNum OF
          0:
            TestByte := $10; { 0001 0000 }
          1:
            TestByte := $01; { 0000 0001 }
          2:
            TestByte := $02; { 0000 0010 }
          3:
            TestByte := $04; { 0000 0100 }
          4:
            TestByte := $08; { 0000 1000 }
          5:
            TestByte := $01; { 0000 0001 }
          6:
            TestByte := $02; { 0000 0010 }
          7:
            TestByte := $04; { 0000 0100 }
          8:
            TestByte := $08; { 0000 1000 }
          9:
            TestByte := $10; { 0001 0000 }
          10:
            TestByte := $20; { 0010 0000 }
          11:
            TestByte := $40; { 0100 0000 }
          12:
            TestByte := $80; { 1000 0000 }
        END; {CASE}

        CASE FunctionNum OF
          0, 1, 2, 3, 4:
           IF (TestByte AND T^.Train_Functions0To4Byte) <> TestByte THEN BEGIN
             FunctionArray[FunctionNum] := False;
             DebugStr := DebugStr + 'F' + IntToStr(FunctionNum) + ': off, '
           END ELSE BEGIN
             FunctionArray[FunctionNum] := True;
             DebugStr := DebugStr + 'F' + IntToStr(FunctionNum) + ': on, '
           END;

          5, 6, 7, 8, 9, 10, 11, 12:
           IF (TestByte AND T^.Train_Functions5To12Byte) <> TestByte THEN BEGIN
             FunctionArray[FunctionNum] := False;
             DebugStr := DebugStr + 'F' + IntToStr(FunctionNum) + ': off, '
           END ELSE BEGIN
             FunctionArray[FunctionNum] := True;
             DebugStr := DebugStr + 'F' + IntToStr(FunctionNum) + ': on, '
           END;
        END; {CASE}
      END; {FOR}

      { remove trailing comma and space }
      IF RightStr(DebugStr, 2) = ', ' THEN
        DebugStr := Copy(DebugStr, 1, Length(DebugStr) - 2);

      IF ForceRead THEN
        DebugStr := DebugStr + ' [force read]';

      Log(LocoChipToStr(LocoChip) + ' L ' + DebugStr);
    END;
  END;
END; { GetLocoFunctions }

FUNCTION SingleLocoFunctionIsOn(LocoChip, FunctionNum : Integer; ForceRead : Boolean; OUT OK : Boolean) : Boolean;
{ Read whether a numbered function is on or off }
VAR
  DebugStr : String;
  T : Train;
  TempSpeedByte : Byte;
  TestByte : Byte;

BEGIN
  Result := False;
  TestByte := 0;
  OK := True;
  IF LocoChip <> UnknownLocoChip THEN BEGIN
    T := GetTrainRecord(LocoChip);
    IF T <> NIL THEN BEGIN
      IF LocoHasBeenTakenOverByuser(LocoChip) OR ForceRead THEN
        ReadInLocoDetails(LocoChip, TempSpeedByte, OK);

      { Now see if a particular bit is set (different set of values for 9-12 from writing out functions) }
      CASE FunctionNum OF
        0:
          TestByte := $10; { 0001 0000 }
        1:
          TestByte := $01; { 0000 0001 }
        2:
          TestByte := $02; { 0000 0010 }
        3:
          TestByte := $04; { 0000 0100 }
        4:
          TestByte := $08; { 0000 1000 }
        5:
          TestByte := $01; { 0000 0001 }
        6:
          TestByte := $02; { 0000 0010 }
        7:
          TestByte := $04; { 0000 0100 }
        8:
          TestByte := $08; { 0000 1000 }
        9:
          TestByte := $10; { 0001 0000 }
        10:
          TestByte := $20; { 0010 0000 }
        11:
          TestByte := $40; { 0100 0000 }
        12:
          TestByte := $80; { 1000 0000 }
      END; {CASE}

      Result := False;
      CASE FunctionNum OF
        0, 1, 2, 3, 4:
         IF (TestByte AND T^.Train_Functions0To4Byte) = TestByte THEN
           Result := True;
        5, 6, 7, 8, 9, 10, 11, 12:
         IF (TestByte AND T^.Train_Functions5To12Byte) = TestByte THEN
           Result := True;
      END; {CASE}

      IF ForceRead THEN BEGIN
        IF Result = True THEN 
          DebugStr := 'Function ' + IntToStr(FunctionNum) + ' is on'
        ELSE
          DebugStr := 'Function ' + IntToStr(FunctionNum) + ' is off';

        DebugStr := DebugStr + ' [force read]';

        Log(LocoChipToStr(LocoChip) + ' L ' + DebugStr + '  <BlankLineBefore>');
      END;
    END;
  END;
END; { SingleLocoFunctionIsOn }

PROCEDURE SetSingleLocoFunction(LocoChip, FunctionNum : Integer; TurnOn : Boolean; OUT OK : Boolean);
{ Set a numbered function on or off }
VAR
  DebugStr : String;
  FunctionWasOn, FunctionWasOff : Boolean;
  IDByte, SpeedByte : Byte;
  T : Train;
  TestByte1, TestByte2 : Byte;
  TimeOut : Boolean;
  TurnOff : Boolean;

BEGIN
  TestByte1 := 0;
  TestByte2 := 0;
  IDByte := 0;

  OK := True;
  FunctionWasOn := False;
  FunctionWasOff := False;
  TurnOff := NOT TurnOn;

  IF LocoChip <> UnknownLocoChip THEN BEGIN
    T := GetTrainRecord(LocoChip);
    IF T <> NIL THEN BEGIN
      IF NOT T^.Train_ControlledByProgram THEN
        ReadInLocoDetails(LocoChip, SpeedByte, OK);

      { Now can use stored function bytes whether read in just now or previously }
      CASE FunctionNum OF
        0, 1, 2, 3, 4:
         TestByte2 := T^.Train_Functions0To4Byte;
        5, 6, 7, 8, 9, 10, 11, 12:
         TestByte2 := T^.Train_Functions5To12Byte;
      END; {CASE}

      { Now see if a particular bit is set (different set of values for 9-12 from reading in functions) }
      CASE FunctionNum OF
        0:
          TestByte1 := $10; { 0001 0000 }
        1:
          TestByte1 := $01; { 0000 0001 }
        2:
          TestByte1 := $02; { 0000 0010 }
        3:
          TestByte1 := $04; { 0000 0100 }
        4:
          TestByte1 := $08; { 0000 1000 }
        5:
          TestByte1 := $01; { 0000 0001 }
        6:
          TestByte1 := $02; { 0000 0010 }
        7:
          TestByte1 := $04; { 0000 0100 }
        8:
          TestByte1 := $08; { 0000 1000 }
        9:
          TestByte1 := $01; { 0000 0001 }
        10:
          TestByte1 := $02; { 0000 0010 }
        11:
          TestByte1 := $04; { 0000 0100 }
        12:
          TestByte1 := $08; { 0000 1000 }
      END; {CASE}

      IF ((TestByte2 AND TestByte1) = TestByte1) THEN
        FunctionWasOn := True
      ELSE
        FunctionWasOff := True;

      IF FunctionWasOn
      AND TurnOn
      THEN
        DebugStr := 'Setting function ' + IntToStr(FunctionNum) + ' unnecessary: it is already on'
      ELSE
        IF FunctionWasOff
        AND TurnOff
        THEN
          DebugStr := 'Setting function ' + IntToStr(FunctionNum) + ' unnecessary: it is already off'
        ELSE BEGIN
         { If the bit we're testing is set it's in the state we expect - now swap it }
          TestByte2 := TestByte2 XOR TestByte1;

          { Now write the data out }
          CASE FunctionNum OF
            0, 1, 2, 3, 4:
              IDByte := 32;
            5, 6, 7, 8:
              IDByte := 33;
            9, 10, 11, 12:
              IDByte := 34;
          END; {CASE}
          WriteArray[0] := 228;
          WriteArray[1] := IDByte;
          WriteArray[2] := GetLocoChipHighByte(LocoChip);
          WriteArray[3] := GetLocoChipLowByte(LocoChip);
          WriteArray[4] := TestByte2;

          DataIO('L', WriteArray, ReadArray, LocoAcknowledgment, OK);

          IF OK THEN BEGIN
            SetTrainControlledByProgram(T, True);
            CASE FunctionNum OF
              0, 1, 2, 3, 4:
                T^.Train_Functions0To4Byte := TestByte2;
              5, 6, 7, 8, 9, 10, 11, 12:
                T^.Train_Functions5To12Byte := TestByte2;
            END; {CASE}

            DebugStr := 'Function ' + IntToStr(FunctionNum);
            IF Turnon THEN
              DebugStr := DebugStr + ' was off - set to on'
            ELSE
              DebugStr := DebugStr + ' was on - set to off';
          END ELSE BEGIN
            DebugStr := 'Failure in setting function ' + IntToStr(FunctionNum);
            IF Turnon THEN
              DebugStr := DebugStr + ' on'
            ELSE
              DebugStr := DebugStr + ' off';
          END;
        END;
      Log(LocoChipToStr(LocoChip) + ' L ' + DebugStr + ' <BlankLineBefore>');
    END;
  END;
END; { SetSingleLocoFunction }

PROCEDURE WriteSignalData(LocoChip, SignalNum : Integer; DecoderNum : Integer; DataByte : Byte; DecoderNumString, AspectString : String; VAR OK : Boolean);
{ Write out the data for signal or route indicator changes }
VAR
  DebugStr : String;
  IDByte : Byte;
  TimeOut : Boolean;

BEGIN
  IDByte := 33; { for functions 5 - 8 }
  Log(LocoChipToStr(LocoChip) + ' S  Setting S=' + IntToStr(SignalNum) + ' (' + DecoderNumString + ')' + ' to ' + AspectString + ' <BlankLineBefore>');
  { Now write the data out }
  WriteArray[0] := 228;
  WriteArray[1] := IDByte;
  WriteArray[2] := GetLocoChipHighByte(DecoderNum);
  WriteArray[3] := GetLocoChipLowByte(DecoderNum);
  WriteArray[4] := DataByte;

  DataIO('S', WriteArray, ReadArray, SignalAcknowledgment, OK);

  DebugStr := 'S=' + IntToStr(SignalNum) + ' (' + DecoderNumString + '): ' + AspectString;
  IF OK THEN BEGIN
    FunctionDecoderBytes[DecoderNum] := DataByte;
    DebugStr := DebugStr + ' ok';
  END ELSE
    DebugStr := DebugStr + ' failed in change';
  Log(LocoChipToStr(LocoChip) + ' S  ' + DebugStr);
END; { WriteSignalData }

PROCEDURE ObtainSystemStatus(VAR SystemStatus : SystemRec; VAR Timeout : Boolean; StartStopTimer : Boolean);
{ Read in the present system status }
VAR
  OK : Boolean;

BEGIN
  Log('G Requesting system status <BlankLineBefore>');
  WriteArray[0] := 33;
  WriteArray[1] := 36;
  DataIO('G', WriteArray, SystemStatusReply, WriteThenRead, OK);

  WITH SystemStatus DO BEGIN
    IF (ReadArray[2] AND 8) = 8 THEN BEGIN
      ProgrammingMode := True;
      Log('XG Initial system status in programming (service) mode');
      MakeSound(1);
    END ELSE
      ProgrammingMode := False;

    IF (ReadArray[2] AND 4) = 4 THEN
      StartMode := True
    ELSE
      StartMode := False;

    IF (ReadArray[2] AND 2) = 2 THEN BEGIN
      EmergencyOff := True;
      Log('XG Initial system status emergency off');
      IF NOT ShowEmergencyOffMessageVisible THEN BEGIN
        MakeSound(1);
        ShowEmergencyOffMessageVisible := True;
        IF MessageDialogueWithDefault('Initial system status: emergency off - do you wish to try to reset it?',
                                      StartStopTimer, mtError, [mbYes, mbNo], mbNo) = mrYes
        THEN BEGIN
          ResumeOperations(OK);
          IF OK THEN
            Log('GG Operations resumed')
          ELSE
            Log('GG Operations not resumed');
        END;
      END;
    END ELSE BEGIN
      EmergencyOff := False;
      ShowEmergencyOffMessageVisible := False;
    END;

    IF (ReadArray[2] AND 1) = 1 THEN BEGIN
      EmergencyStop := True;
      Log('X+ Initial system status: emergency stop');

      IF NOT ShowEmergencyStopMessageVisible THEN BEGIN
        MakeSound(1);
        ShowEmergencyStopMessageVisible := True;
        IF MessageDialogueWithDefault('Initial system status: emergency stop - do you wish to try to reset it?',
                                      StartStopTimer, mtError, [mbYes, mbNo], mbNo) = mrYes
        THEN BEGIN
          ResumeOperations(OK);
          IF OK THEN
            Log('GG Operations resumed')
          ELSE
            Log('GG Operations not resumed');
        END;
      END;
    END ELSE
      EmergencyStop := False;
  END; {WITH}
END; { ObtainSystemStatus }

PROCEDURE TLenzWindow.OnLenzOneMilliSecondTimerInterval(Sender: TObject);
{ This is used to stop point motors burning out }
VAR
  P : Integer;
  OK : Boolean;
  S : Integer;

BEGIN
  IF NOT ProgramStartup THEN BEGIN
    FOR P := 0 TO High(Points) DO BEGIN
      IF Points[P].Point_Energised THEN BEGIN
//Log('P P=' + IntToStr(P) + ' energised at ' + TimeToHMSZStr(Points[P].Point_EnergisedTime));
//Log('P Current time=' + TimeToHMSZStr(Time));
        IF MilliSecondsBetween(Time, Points[P].Point_EnergisedTime) > 2 THEN BEGIN
//Log('X P=' + IntToStr(P) + ' ' + IntToStr(MilliSecondsBetween(Time, Points[P].Point_EnergisedTime)));
          { an error - it's been energised too long }
          EmergencyDeselectPoint(P, OK);
          Log('PG Point ' + IntToStr(P) + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum) + ']' + ' has been energised for too long so emergency deselection attempted');
          IF OK THEN BEGIN
            Points[P].Point_Energised := False;
            Log('PG Point ' + IntToStr(P) + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum) + ']' + ': emergency deselection was successful');
          END ELSE
            Log('PG Point ' + IntToStr(P) + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum) + ']' + ': emergency deselection failed');
        END;
      END;
    END;

    FOR S := 0 TO High(Signals) DO BEGIN
      IF Signals[S].Signal_Energised THEN BEGIN
  //    Log('S', 'S=' + IntToStr(S) + ' energised at ' + TimeHMToStr(Signals[S].EntergisedTime, 'hh:mm:ss:zzz')]);
  //    Log('S', 'Current time=' + TimeHMToStr(Time, 'hh:mm:ss:zzz')]);
        IF MilliSecondsBetween(Time, Signals[S].Signal_EnergisedTime) > 2 THEN BEGIN
  //      Log('X S=' + IntToStr(S) + ' ' + IntToStr(MilliSecondsBetween(Time, Signals[S].Signal_EnergisedTime))]);
          { an error - it's been energised too long }
          EmergencyDeselectSignal(S, OK);
          Log('SG Signal ' + IntToStr(S) + ' [accessory address=' + IntToStr(Signals[S].Signal_AccessoryAddress) + ']'
                           + ' has been energised for too long so emergency deselection attempted');
          IF OK THEN BEGIN
            Signals[S].Signal_Energised := False;
            Log('SG Signal ' + IntToStr(S) + ' [accessory address=' + IntToStr(Signals[S].Signal_AccessoryAddress) + ']' + ': emergency deselection was successful');
          END ELSE
            Log('SG Signal ' + IntToStr(S) + ' [accessory address=' + IntToStr(Signals[S].Signal_AccessoryAddress) + ']' + ': emergency deselection failed');
        END;
      END;
    END;
  END;
END; { OnLenzTimer }

PROCEDURE SetSignalFunction(LocoChip, S : Integer);
{ Set a numbered function on or off - used for LED signals controlled by LF100XF function only decoders, which are programmed to use functions 5-8 }
VAR
  AspectByte, DataByte : Byte;
  AspectString, DecoderNumString : String;
  OK : Boolean;

BEGIN
  WITH Signals[S] DO BEGIN
    IF Signal_AccessoryAddress <> 0 THEN BEGIN

    END ELSE
      IF Signal_DecoderNum <> 0 THEN BEGIN
        { initialise AspectByte }
        AspectByte := 0;
        DecodernumString := LocoChipToStr(Signal_DecoderNum);

        CASE Signal_Aspect OF
          RedAspect:
            AspectByte := 2; { function 6 - 0000 0010 }
          GreenAspect:
            AspectByte := 1; { function 5 - 0000 0001 }
          SingleYellowAspect:
            AspectByte := 4; { function 7 - 0000 0100 }
          DoubleYellowAspect:
            AspectByte := 12; { function 8 - 0000 1100 }

          FlashingSingleYellowAspect:
            IF Signal_LampIsOn THEN
              AspectByte := 4 { 0000 0100 }
            ELSE
              AspectByte := 0; { 0000 0000 }
          FlashingDoubleYellowAspect:
            IF Signal_LampIsOn THEN
              AspectByte := 12 { 0000 1100 }
            ELSE
              AspectByte := 0; { 0000 0000 }

          NoAspect:
            AspectByte := 0; { 0000 0000 }
        END; {CASE}
        AspectString := AspectToStr(Signals[S].Signal_Aspect);

        IF Signal_Type = ThreeAspect THEN BEGIN
          { reset lowest three bits first - don't change fourth, as may be a route indicator }
          DataByte := FunctionDecoderBytes[Signal_DecoderNum] AND NOT 7;
          DataByte := DataByte OR AspectByte;
          WriteSignalData(LocoChip, S, Signal_DecoderNum, DataByte, DecoderNumString, AspectString, OK)
        END ELSE
          IF Signal_Type = FourAspect THEN BEGIN
            { reset lowest four bits }
            IF (Signal_DecoderNum >= FirstFunctionDecoder)
            AND (Signal_DecoderNum <= LastFunctionDecoder)
            THEN BEGIN
              DataByte := FunctionDecoderBytes[Signal_DecoderNum] AND NOT 15;
              DataByte := DataByte OR AspectByte;
              WriteSignalData(LocoChip, S, Signal_DecoderNum, DataByte, DecoderNumString, AspectString, OK);
            END ELSE
              Log(LocoChipToStr(LocoChip) + ' S  Function decoder ' + IntToStr(Signal_DecoderNum) + ' specified is outside allowed range');
          END;
      END;
  END; {WITH}
END; { SetSignalFunction }

PROCEDURE SetSignalRouteFunction(LocoChip, S : Integer);
{ Set a numbered function on or off - used for LED signals controlled by LF100XF function only decoders }
VAR
  AspectString, IndicatorDecoderNumString : String;
  IndicatorByte, DataByte : Byte;
  OK : Boolean;

BEGIN
  WITH Signals[S] DO BEGIN
    IF Signal_IndicatorDecoderNum <> 0 THEN BEGIN
      OK := True;
      IndicatorByte := 0;
      IndicatorDecoderNumString := LocoChipToStr(Signal_IndicatorDecoderNum);

      CASE Signal_IndicatorDecoderFunctionNum OF
        1:
          IndicatorByte := 1; { 0000 0001 }
        2:
          IndicatorByte := 2; { 0000 0010 }
        3:
          IndicatorByte := 4; { 0000 0100 }
        4:
          IndicatorByte := 8; { 0000 1000 }
      END; {CASE}

      IF Signal_IndicatorState <> NoIndicatorLit THEN BEGIN
        AspectString := 'Indicator on';
        { now set our bit, but preserve other bits, as this decoder may be being used for more than one signal }
        DataByte := FunctionDecoderBytes[Signal_IndicatorDecoderNum] OR IndicatorByte;
      END ELSE BEGIN
        AspectString := 'Indicator off';
        { now reset our bit, but preserve other bits, as this decoder may be being used for more than one signal }
        DataByte := FunctionDecoderBytes[Signal_IndicatorDecoderNum] AND NOT IndicatorByte;
      END;
      WriteSignalData(LocoChip, S, Signal_IndicatorDecoderNum, DataByte, IndicatorDecoderNumString, AspectString, OK);
    END;
  END; {WITH}
END; { SetSignalRouteFunction }

FUNCTION ReturnFeedbackData(UnitNum : Byte; Input : Integer) : Boolean;
{ Pass data from the feedback array back }
BEGIN
  Result := FeedbackDataArray[UnitNum, Input];
END; { ReturnFeedbackData }

PROCEDURE EmergencyDeselectPoint(P : Integer; VAR OK : Boolean);
{ for use in emergency, when a point remains energised }
VAR
  DecoderUnitNum : Byte;
  TimeOut : Boolean;

BEGIN
  WITH Points[P] DO BEGIN
    DecoderUnitNum := (Point_LenzNum - 1) DIV 4;

    WriteArray[0] := 82;
    WriteArray[1] := DecoderUnitNum;

    { set bit 3 off to deselect }
    WriteArray[2] := 128; {1000 0000}
    Log('E Writing out data to EMERGENCY DESELECT ' + 'P=' + IntToStr(P) + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum - 1) + '] <BlankLineBefore>');
    DataIO('P', WriteArray, Acknowledgment, WriteThenRead, OK);
  END; {WITH}
END; { EmergencyDeselectPoint }

PROCEDURE EmergencyDeselectSignal(S : Integer; VAR OK : Boolean);
{ for use in emergency, when a signal remains energised }
VAR
  DecoderUnitNum : Byte;
  TimeOut : Boolean;

BEGIN
  WITH Signals[S] DO BEGIN
    DecoderUnitNum := (Signal_AccessoryAddress - 1) DIV 4;

    WriteArray[0] := 82;
    WriteArray[1] := DecoderUnitNum;

    { set bit 3 off to deselect }
    WriteArray[2] := 128; {1000 0000}
    Log('E Writing out data to EMERGENCY DESELECT ' + 'S=' + IntToStr(S) + ' [accessoryaddress=' + IntToStr(Signals[S].Signal_AccessoryAddress - 1) + '] <BlankLineBefore>');
    DataIO('E', WriteArray, Acknowledgment, WriteThenRead, OK);
  END; {WITH}
END; { EmergencyDeselectPoint }

PROCEDURE MakeSemaphoreSignalChange(LocoChip, S, AccessoryAddress : Integer; OnOrOff : SignalStateType);
{ Pull a semaphore signal or or off using TrainTech's SC3, which "learns" given an accessory address by the LH100 }
CONST
  ProcessMessages = True;

VAR
  B : Byte;
  DebugStr : String;
  DecoderUnitNum, DecoderOutputNum : Byte;
  OK : Boolean;
  TimeOut : Boolean;

  PROCEDURE ActivateSignal;
  BEGIN
    AccessoryAddress := AccessoryAddress - 1;
    DecoderUnitNum := AccessoryAddress DIV 4;
    DecoderOutputNum := (AccessoryAddress MOD 4) + 1;

    { select first }
    WriteArray[0] := 82;
    WriteArray[1] := DecoderUnitNum;
    { bit 7 is always on, bit 3 on = select }
    B := 136; {1000 1000}

    { set bits 2+1 = output no (0, 2, 4, 6) }
    CASE DecoderOutputNum OF
      1:;            { 1000 1000 }
        { does not need setting }
      2:
        B := B OR 2; { 1000 1010 }
      3:
        B := B OR 4; { 1000 1100 }
      4:
        B := B OR 6; { 1000 1110 }
    END; {CASE}

    DebugStr := 'Writing out data to switch semaphore S=' + IntToStr(S) + ' (accessory address ' + IntToStr(AccessoryAddress) + ') ';
    IF OnOrOff = SignalOn THEN
      DebugStr := DebugStr + 'on'
    ELSE
      DebugStr := DebugStr + 'off';
    DebugStr := DebugStr +  ' <BlankLineBefore>';

    { Bit 0 is the direction }
    IF OnOrOff = SignalOff THEN
      { Straight doesn't need setting so can stay 0 }
      B := B OR 1;

    WriteArray[2] := B;

    { Now start the burn out check }
    Signals[S].Signal_Energised := True;
    { and store the current time }
    Signals[S].Signal_EnergisedTime := Time;

    DebugStr := DebugStr + ' at ' + TimeToHMSZStr(Time);
    Log(LocoChipToStr(LocoChip) + ' S ' + DebugStr);

    DataIO('S', WriteArray, Acknowledgment, WriteThenRead, OK);
  END; { ActivateSignal}

  PROCEDURE DeactivateSignal;
  BEGIN
    AccessoryAddress := AccessoryAddress - 1;
    DecoderUnitNum := AccessoryAddress DIV 4;

    WriteArray[0] := 82;
    WriteArray[1] := DecoderUnitNum;

    { set bit 3 off to deselect }
    WriteArray[2] := 128; {1000 0000}
    Log('S Writing out data to deselect semaphore S=' + IntToStr(S)
           + ' (accessory address ' + IntToStr(AccessoryAddress) + '] at ' + TimeToHMSZStr(Time) + '  <BlankLineBefore>');
    DataIO('S', WriteArray, Acknowledgment, WriteThenRead, OK);

    { Now turn off the burn-out checking }
    IF OK THEN
      Signals[S].Signal_Energised := False;
    Log('S Semaphore S=' + IntToStr(S) + ' deselected'); // after ' + IntToStr(MilliSecondsBetween(Time, Points[P].Point_TimeEnergised)) + ' ms');
  END; { DeactivateSignal }

BEGIN
  ActivateSignal;

  DoCheckForUnexpectedData(UnitRef, 'MakeSemaphoreSignalChange');
  IF InAutoMode THEN
    MoveAllTrains;
  Pause(50, NOT ProcessMessages);

  DeactivateSignal;
END; { MakeSemaphoreSignalChange }

PROCEDURE TurnPointOff(P : Integer; VAR OK : Boolean);
{ Deselect a given point }
VAR
  DecoderUnitNum : Byte;
  InterfaceLenzPoint : Integer;
  TimeOut : Boolean;

BEGIN
  InterfaceLenzPoint := Points[P].Point_LenzNum - 1;
  DecoderUnitNum := InterfaceLenzPoint DIV 4;

  WriteArray[0] := 82;
  WriteArray[1] := DecoderUnitNum;

  { set bit 3 off to deselect }
  WriteArray[2] := 128; {1000 0000}
  Log('P Writing out data to deselect P=' + IntToStr(P) + ' [Lenz' + IntToStr(Points[P].Point_LenzNum) + '] at ' + TimeToHMSZStr(Time) + ' <BlankLineBefore>');
  DataIO('P', WriteArray, PointAcknowledgment, WriteThenRead, OK);

  { Now turn off the burn-out checking }
  IF OK THEN
    Points[P].Point_Energised := False;
  Log('P P=' + IntToStr(P) + ' deselected after ' + IntToStr(MilliSecondsBetween(Time, Points[P].Point_EnergisedTime)) + ' ms');
END; { TurnPointOff }

FUNCTION MakePointChange(LocoChip : Integer; P : Integer; Direction : PointStateType; VAR Count : Integer) : Boolean;
{ Select which decoder, which output and which direction (LS100/100 decoders have four outputs. Need to select then deselect after 200 ms!) }
CONST
  ProcessMessages = True;

VAR
  B : Byte;
  DebugStr : String;
  DecoderUnitNum, DecoderOutputNum : Byte;
  InterfaceLenzPoint : Integer;
  OK : Boolean;
  TimeOut : Boolean;

  PROCEDURE TurnPointOn(LocoChip : Integer; VAR OK : Boolean);
  BEGIN
    { select first }
    WriteArray[0] := 82;
    WriteArray[1] := DecoderUnitNum;
    { bit 7 is always on, bit 3 on = select }
    B := 136; {1000 1000}

    { set bits 2+1 = output no (0, 2, 4, 6) }
    CASE DecoderOutputNum OF
      1:;            { 1000 1000 }
        { does not need setting }
      2:
        B := B OR 2; { 1000 1010 }
      3:
        B := B OR 4; { 1000 1100 }
      4:
        B := B OR 6; { 1000 1110 }
    END; {CASE}

    DebugStr := 'Writing out data (' + IntToStr(Count) + ') to select P=' + IntToStr(P) + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum) + '] for change to ';
    IF Direction = Straight THEN
      DebugStr := DebugStr + 'straight'
    ELSE
      DebugStr := DebugStr + 'diverging';
    DebugStr := DebugStr + ' <BlankLineBefore>';

    { If the wiring is reversed, the point is switching the opposite way to the way we think it is }
    IF Points[P].Point_WiringReversedFlag THEN BEGIN
      IF Direction = Straight THEN
        Direction := Diverging
      ELSE
        Direction := Straight;
    END;

    { Bit 0 is the direction }
    IF Direction = Diverging THEN
      { Straight doesn't need setting so can stay 0 }
      B := B OR 1;

    WriteArray[2] := B;

    { Now start the burn out check }
    Points[P].Point_Energised := True;
    { and store the current time }
    Points[P].Point_EnergisedTime := Time;

    { and if the point is controlled by an LS150, store the time other points controlled by the same unit can be activated after }
    IF Points[P].Point_LenzUnitType = 'LS150' THEN BEGIN
      { **** }
    END;

    DebugStr := DebugStr + ' at ' + TimeToHMSZStr(Time);
    Log(LocoChipToStr(LocoChip) + ' P ' + DebugStr);

    DataIO('P', WriteArray, PointAcknowledgment, WriteThenRead, OK);
  END; { TurnPointOn }

  PROCEDURE TurnPointOff(LocoChip : Integer; VAR OK : Boolean);
  BEGIN
    WriteArray[0] := 82;
    WriteArray[1] := DecoderUnitNum;

    { set bit 3 off to deselect }
    WriteArray[2] := 128; {1000 0000}
    Log(LocoChipToStr(LocoChip) + ' P Writing out data (' + IntToStr(Count) + ') to deselect P=' + IntToStr(P)
                                + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum) + '] at ' + TimeToHMSZStr(Time) + ' <BlankLineBefore>');
    DataIO('P', WriteArray, PointAcknowledgment, WriteThenRead, OK);

    { Now turn off the burn-out checking }
    IF OK THEN
      Points[P].Point_Energised := False;
    Log(LocoChipToStr(LocoChip) + ' P P=' + IntToStr(P) + ' deselected after ' + IntToStr(MilliSecondsBetween(Time, Points[P].Point_EnergisedTime)) + ' ms');
  END; { TurnPointOff }

BEGIN { MakePointChange }
  { Convert device number to the Lenz address - start by decrementing the number by one, as (for some unexplained reason) the devices are numbered differently by the
    Interface and the LI101
  }
  InterfaceLenzPoint := Points[P].Point_LenzNum - 1;
  DecoderUnitNum := InterfaceLenzPoint DIV 4;
  DecoderOutputNum := (InterfaceLenzPoint MOD 4) + 1;
  { and increase the counter for the debug statements }
  Inc(Count);

  OK := False;
  TurnPointOn(LocoChip, OK);
  IF OK THEN BEGIN
    DoCheckForUnexpectedData(UnitRef, 'MakePointChange 1');
    IF InAutoMode THEN
      MoveAllTrains;
    Pause(50, NOT ProcessMessages);
    DoCheckForUnexpectedData(UnitRef, 'MakePointChange 2');
    IF InAutoMode THEN
      MoveAllTrains;
    Pause(50, NOT ProcessMessages);
    DoCheckForUnexpectedData(UnitRef, 'MakePointChange 3');
    IF InAutoMode THEN
      MoveAllTrains;

    OK := False;
    REPEAT
      TurnPointOff(LocoChip, OK);
      DoCheckForUnexpectedData(UnitRef, 'MakePointChange 4');
      IF InAutoMode THEN
        MoveAllTrains;
    UNTIL OK;
  END;
  Result := OK;
END; { MakePointChange }

PROCEDURE SetUpDoubleHeader(LocoChip1, LocoChip2 : Word; VAR OK : Boolean);
{ Sets up a double header - needs both locos standing still, both have already been controlled, and were the most recent ones controlled by the computer. This procedure is
  not used by the rail program as the program deals with double heading by sending the speed commands to both named locos.
}
VAR
  TimeOut : Boolean;
  T1, T2 : Train;

BEGIN
  Log(LocoChipToStr(LocoChip1) + ' L Requesting double header for locos ' + IntToStr(LocoChip1) + ' +' + IntToStr(LocoChip2) + ' <BlankLineBefore>');
  WriteArray[0] := 229;
  WriteArray[1] := 67;
  WriteArray[2] := GetLocoChipHighByte(LocoChip1);
  WriteArray[3] := GetLocoChipLowByte(LocoChip1);
  WriteArray[4] := GetLocoChipHighByte(LocoChip2);
  WriteArray[5] := GetLocoChipLowByte(LocoChip2);

  DataIO('L', WriteArray, ReadArray, Acknowledgment, OK);
  IF OK THEN BEGIN
    T1 := GetTrainRecord(LocoChip1);
    T2 := GetTrainRecord(LocoChip2);
    IF (T1 <> NIL)
    AND (T2 <> NIL)
    THEN BEGIN
      SetTrainControlledByProgram(T1, True);
      SetTrainControlledByProgram(T2, True);
      Log(LocoChipToStr(LocoChip1) + ' L Double header (locos ' + IntToStr(LocoChip1) + ' +' + IntToStr(LocoChip2) + ') set up')
    END;
  END ELSE
    Log(LocoChipToStr(LocoChip1) + ' L Double header (locos ' + IntToStr(LocoChip1) + ' +' + IntToStr(LocoChip2) + ') not set up');
END; { SetUpDoubleHeader }

PROCEDURE DissolveDoubleHeader(LocoChip : Integer; VAR OK : Boolean);
{ Dissolves a double header - needs both locos standing still. This procedure is not used by the rail program as the program deals with double heading by sending the speed
  commands to both named locos.
}
VAR
  TimeOut : Boolean;

BEGIN
  Log(LocoChipToStr(LocoChip) + ' L Requesting double header for ' + IntToStr(LocoChip) + ' be dissolved <BlankLineBefore>');
  WriteArray[0] := 229;
  WriteArray[1] := 67;
  WriteArray[2] := GetLocoChipHighByte(LocoChip);
  WriteArray[3] := GetLocoChipLowByte(LocoChip);
  WriteArray[4] := 0;
  WriteArray[5] := 0;

  DataIO('L', WriteArray, ReadArray, Acknowledgment, OK);

  IF OK THEN
    Log(LocoChipToStr(LocoChip) + ' L Double header (including loco ' + IntToStr(LocoChip) + ') dissolved')
  ELSE
    Log(LocoChipToStr(LocoChip) + ' L Double header (including loco ' + IntToStr(LocoChip) + ') not dissolved');
END; { DissolveDoubleHeader }

{$O+}

PROCEDURE StopOperations;
{ Turns the power off to the track and to I/O devices; it tells the system to stop sending DCC packets to the track and to switch off the DCC track power. }
VAR
  OK : Boolean;
  TimeOut : Boolean;

BEGIN
  Log('E Requesting stop operations <BlankLineBefore>');
  WriteArray[0] := 33;
  WriteArray[1] := 128;

  DataIO('E', WriteArray, ReadArray, TrackPowerOffReply, OK);
END; { StopOperations }

PROCEDURE ResumeOperations(OUT OK : Boolean);
{ Turns the power back on }
VAR
  TimeOut : Boolean;

BEGIN
  Log('E Requesting resume operations  <BlankLineBefore>');
  WriteArray[0] := 33;
  WriteArray[1] := 129;

  DataIO('E', WriteArray, ReadArray, EverythingTurnedOnReply, OK);
END; { ResumeOperations }

PROCEDURE StopAllLocomotives(VAR OK : Boolean);
{ Emergency stops all trains, but leaves the power on }
BEGIN
  Log('L Requesting stop all locomotives <BlankLineBefore>');
  WriteArray[0] := 128;

  DataIO('L', WriteArray, ReadArray, EmergencyStopReply, OK);
END; { StopAllLocomotives }

PROCEDURE StopAParticularLocomotive(LocoChip : Integer; VAR OK : Boolean);
{ Stops a particular loco }
BEGIN
  Log('L Requesting stop loco ' + LocoChipToStr(LocoChip) + ' <BlankLineBefore>');
  WriteArray[0] := 146;
  WriteArray[1] := GetLocoChipHighByte(LocoChip);
  WriteArray[2] := GetLocoChipLowByte(LocoChip);

  DataIO('L', WriteArray, ReadArray, Acknowledgment, OK);
  IF OK THEN
    LocoHasBeenTakenOverByProgram(LocoChip);
END; { StopAParticularLocomotive }

PROCEDURE ReturnSystemStatus(VAR S : SystemRec);
{ Return the present system status }
BEGIN
  S := SystemStatus;
END; { ReturnSystemStatus }

PROCEDURE WhichFeedbackInputsAreSet(UnitNum, B : Byte);
{ Looks at bits 4-0 to find out, and calls DecodeFeedback to set the on-screen tell-tales }
VAR
  I, Input : Integer;
  FeedbackData : FeedbackRec;

BEGIN
  { If bit 4 set, inputs are 5 to 8, so increment counter accordingly }
  IF (B AND 16) <> 16 THEN
    Input := 0
  ELSE
    Input := 4;

  { Cycle through the four inputs }
  FOR I := 1 TO 4 DO BEGIN
    FeedbackData.Feedback_Unit := UnitNum + 1;
    FeedbackData.Feedback_Input := Input + I;
    IF (B AND BinaryCounter[I]) = BinaryCounter[I] THEN BEGIN
      { store it first }
      {$R-}
      FeedbackDataArray[UnitNum + 1, Input + I] := True;
      {$R+}
      FeedbackData.Feedback_InputOn := True;
      { decode it and record it for debugging }
      DecodeFeedback(FeedbackData);
    END ELSE BEGIN
      { store it first }
      {$R-}
      FeedbackDataArray[UnitNum + 1, Input + I] := False;
      {$R+}
      FeedbackData.Feedback_InputOn := False;
      { decode it and record it for debugging }
      DecodeFeedback(FeedbackData);
    END;
  END;
END; { WhichFeedbackInputsAreSet }

PROCEDURE GetInitialFeedback;
{ Read in the feedback before we start, or after an interruption }
CONST
  ProcessMessages = True;

VAR
//  FeedbackData : FeedbackRec;
//  FeedbackNum : Integer;
//  FeedbackPort : PortType;
//  FeedbackType : TypeOfFeedBackType;
//  I, J : Integer;
  OK : Boolean;
//  TCAboveFeedbackUnit : Integer;
//  TempFeedbackNum : Integer;
  UnitNum : Integer;

  PROCEDURE ReadInFeedbackData(FeedbackAddress : Byte; OUT OK : Boolean);
  { Ask for feedback on specified device and store it. Usually only called on startup.

    Feedback comes in two nibbles - have to ask for both to get all the inputs, which comes in bits 3-0 (in nibble 1, bit 0 is input 1, bit 1 is input 2, etc. In nibble 2,
    bit 0 is input 5, bit 1 is input 6, etc.) FeedbackAddress is one less than the one shown on the LH100 and the one programmed into the device
  }
  VAR
    I : Integer;
    TimeOut : Boolean;

  BEGIN
    FOR I := 128 TO 129 DO BEGIN
      ExpectedFeedbackAddress := FeedbackAddress;
      WriteArray[0] := 66;
      WriteArray[1] := FeedbackAddress;
      WriteArray[2] := I;

      IF NOT SystemOnline THEN
        Exit
      ELSE BEGIN
        REPEAT
          DataIO('T', WriteArray, ReadArray, FeedbackReply, OK);

          { Check it's for the unit we specified, in case some unrequested feedback data comes in while we're starting up }
          IF OK
          AND (ReadArray[1] <> FeedbackAddress)
          THEN
            Log('TG Feedback for ' + IntToStr(ReadArray[1] + 1) + ' arrived when feedback for ' + IntToStr(FeedbackAddress + 1) + ' expected');
        UNTIL OK
        AND (ReadArray[1] = FeedbackAddress);
      END;

      IF OK THEN BEGIN
        IF ReadArray[2] = 0 THEN BEGIN
          { there's a problem - keep the user informed }
          OK := False;
          Log('T The feedback from unit ' + IntToStr(ReadArray[1] + 1) + ' has no data in it');
//          FeedbackUnitInUseArray[ReadArray[1] + 1] := False;
        END ELSE BEGIN
//          FeedbackUnitInUseArray[ReadArray[1] + 1] := True;
          WhichFeedbackInputsAreSet(FeedbackAddress, ReadArray[2]);

          { and store the data for future use - need to store in two halves }
          IF I = 128 THEN BEGIN
            { inputs 1 to 4 }
            FeedbackArray[FeedbackAddress, 0] := ReadArray[2];
          END ELSE BEGIN
            { inputs 5 to 8 }
            FeedbackArray[FeedbackAddress, 1] := ReadArray[2]
          END;
        END;
      END;
    END;
  END; { ReadInFeedbackData }

BEGIN
  FOR UnitNum := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
    Log('T Requesting feedback data for unit ' + IntToStr(UnitNum + 1) + ' <BlankLineBefore>');
    ReadInFeedbackData(UnitNum, OK);
    IF NOT OK THEN BEGIN
      Log('T Re-requesting feedback data for unit ' + IntToStr(UnitNum + 1) + ' <BlankLineBefore>');
      ReadInFeedbackData(UnitNum, OK);
      IF NOT OK THEN BEGIN
        Log('T No feedback from unit ' + IntToStr(UnitNum + 1));
//          IF (Length(NoFeedbackList) = 0) OR (IntToStr(ReadArray[1] + 1) <> NoFeedbackList[High(NoFeedbackList)]) THEN
//            AppendToStringArray(NoFeedbackList, IntToStr(ReadArray[1] + 1));
      END;
    END;
  END; {FOR}


//  SetLength(NoFeedbackList, 0);
//
//  UnitNum := FirstFeedbackUnit - 1;
//  PortNum := -1;
//
//  WHILE (UnitNum <= (LastFeedbackUnit - 1))
//  AND SystemOnline DO BEGIN
//    FeedbackData.Feedback_Unit := UnitNum + 1;
//    ExtractDataFromFeedback(FeedbackData, FeedbackPort, TCAboveFeedbackUnit, FeedbackType, TempFeedbackNum);
//    IF FeedbackType = FeedbackDetectorOutOfUse THEN
//      Log('T Not requesting feedback data for unit ' + IntToStr(UnitNum + 1) + ' as it is marked as being out of use')
//    ELSE BEGIN
//      IF FeedbackPort = USBPort THEN
//        PortNum := USBPortNum;
//
//      Log('T Requesting feedback data for unit ' + IntToStr(UnitNum + 1));
//      ReadInFeedbackData(PortNum, UnitNum, OK);
//      IF NOT OK THEN BEGIN
//        PortNum := USBPortNum
//        Log('T Re-requesting feedback data for unit ' + IntToStr(UnitNum + 1));
//        ReadInFeedbackData(PortNum, UnitNum, OK);
//        IF NOT OK THEN BEGIN
//          Log('T No feedback from unit ' + IntToStr(ReadArray[1] + 1) + ' on either port');
//          IF (Length(NoFeedbackList) = 0) OR (IntToStr(ReadArray[1] + 1) <> NoFeedbackList[High(NoFeedbackList)]) THEN
//            AppendToStringArray(NoFeedbackList, IntToStr(ReadArray[1] + 1));
//        END;
//      END;
//    END;
//
//    Pause(500, NOT ProcessMessages);
//
//    IF SystemOffline THEN
//      Log('G System offline - noted when reading startup data');
//    Inc(UnitNum);
//  END; {WHILE}
//
//  { Now to let the user know if there's a problem }
//  IF Length(NoFeedbackList) <> 0 THEN BEGIN
//    IF Length(NoFeedbackList) = 1 THEN BEGIN
//      IF TestingMode THEN
//        Log('X No feedback received from unit ' + NoFeedbackList[0])
//      ELSE
//        Log('X! No feedback received from unit ' + NoFeedbackList[0]);
//    END ELSE BEGIN
//      IF TestingMode THEN
//        Log('X No feedback received from the following units: ')
//      ELSE
//        Log('X! No feedback received from the following units: ');
//
//      DebugStr := '';
//      FOR I := 0 TO High(NoFeedbackList) DO BEGIN
//        DebugStr := DebugStr + NoFeedbackList[I];
//        IF I < High(NoFeedbackList) THEN
//          DebugStr := DebugStr + ', '
//        ELSE
//          DebugStr := DebugStr + '.';
//      END;
//      IF TestingMode THEN
//        Log('X ' + DebugStr)
//      ELSE
//        Log('XG ' + DebugStr);
//    END;
//  END;
END; { GetInitialFeedback }

PROCEDURE InitialiseLenzUnit;
{ Such routines as this allow us to initialises the units in the order we wish }
CONST
  StopTimer = True;
  WarnUser = True;

VAR
  I, J : Word;
  OK : Boolean;
  TimedOut : Boolean;

BEGIN
//  SystemInitiallySetOffline := True; // ************************************************

  { For initial data flow check }
  OK := False;

  IF SystemInitiallySetOffline THEN
    SystemOnline := False
  ELSE BEGIN
    { Create the TCPIP form here so we know it is available before we start using it }
    IF TCPIPForm = NIL THEN BEGIN
      TCPIPForm := TTCPIPForm.Create(Application);
      TCPIPForm.Update;
    END;

    { First see if the Lenz server program is running }
    IF IsProgramRunning('LI-Server') THEN BEGIN
      ShowMessage('The LI-Server.exe program is already running - make sure the server itself is running and then press OK');
      Log('X The LI-Server.exe program is already running - make sure the server itself is running and then press OK');
      OK := True;                                 // ************ this doesn't mean, of course, that the server itself is running }
    END ELSE BEGIN
      StartLANUSBServer;
      IF IsProgramRunning('LI-Server') THEN BEGIN
        OK := True;
        Log('XG LI-Server.exe is now running');
      END ELSE BEGIN
        OK := False;
        Log('XG LI-Server.exe is still not running');
      END;
    END;

    IF OK THEN BEGIN
      TCPIPForm.TCPIPFormShow(LenzWindow);
      TCPIPForm.CreateTCPClients;
    END;

    IF OK AND (TCPIPConnected = True) THEN
      SetSystemOnline
    ELSE BEGIN
      SetSystemOffline('System offline');
      Log('G System offline (1)');
    END;

      { provisionally... }

      { but now check the true state of affairs }
//      ObtainSystemStatus(USBPortNum, SystemStatus, Timedout, StopTimer);
//
//      IF SystemOnline THEN BEGIN
//        IF SystemStatus.MainPortEmergencyOff
//        OR SystemStatus.MainPortEmergencyStop
//        OR SystemStatus.SecondaryPortEmergencyOff
//        OR SystemStatus.SecondaryPortEmergencyStop
//        THEN BEGIN
//          { ask the user if he/she wants to try to reset the system }
//          //debug;
//        END;
//      END ELSE BEGIN
//        { The system status is presumed not to be in an emergency state }
//        SystemStatus.MainPortEmergencyOff := False;
//        SystemStatus.MainPortEmergencyStop := False;
//        SystemStatus.SecondaryPortEmergencyOff := False;
//        SystemStatus.SecondaryPortEmergencyStop := False;
//      END;
  END;

  { Initialise the feedback data Array }
  IF SystemOnline THEN BEGIN
    FOR I := (FirstFeedbackUnit - 1) TO (LastFeedbackUnit - 1) DO
      FOR J := 1 TO 8 DO
        FeedbackDataArray[I, J] := False;
  END;

  IF SystemOnline THEN BEGIN
    { Store settings of feedback units before starting }
    ReadCommandStationSoftwareVersion(OK);
    ReadComputerInterfaceSoftwareVersion(OK);
  END;

  FOR I := 0 TO (ReadArrayLen - 1) DO BEGIN
    ReadArray[I] := 0;
    WriteArray[I] := 0;
  END;
END; { InitialiseLenzUnit }

INITIALIZATION

END { Lenz }.
