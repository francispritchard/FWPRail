UNIT Lenz;
{ Unit that talks to a Lenz Digital Plus interface via a serial port or to an LI-USB interface

  Copyright � F.W. Pritchard 1998-2015. All Rights Reserved.
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
  v5.0  02/03/14 serial interface replaced by Lenz LAN/USB
  v5.1  01/04/14 ethernet interface also implemented
  v5.2  22/07/14 full record now passed to routine rather than just index to locos array

  To remind FWP: OR sets bits; AND NOT resets them; XOR swaps values.
  IF (B and 96) = 96 (0110 0000) THEN - checks if bits 6 and 5 are set
}

INTERFACE

USES Initvars, Forms, ExtCtrls, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Dialogs, DB, ADODB, DBCtrls, Grids, DBGrids, StdCtrls, ComCtrls,
     TCPIP, PointsUnit, SignalsUnit;

TYPE
  TLenzWindow = CLASS(TForm)
    LenzWatchdogTimer: TTimer;
    LenzOneSecondTimerTick: TTimer;
    PROCEDURE OnLenzOneSecondTimerTick(Sender: TObject);
    PROCEDURE OnLenzWatchdogTimerInterval(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  LenzWindow: TLenzWindow;

FUNCTION AdjustLenzSpeed(VAR Loco : LocoRec; Value : Integer; LocoDirection : DirectionType; OUT OK : Boolean) : Integer;
{ Increase or decrease the speed by given amount }

PROCEDURE DoCheckForUnexpectedData(UnitRef : String; CallingStr : String);
{ See if any has arrived }

PROCEDURE EmergencyDeselectPoint(P : Integer; VAR OK : Boolean);
{ for use in emergency, when a point remains energised }

PROCEDURE EmergencyDeselectSignal(S : Integer; VAR OK : Boolean);
{ for use in emergency, when a signal remains energised }

PROCEDURE GetInitialFeedback(OUT OK : Boolean);
{ Read in the feedback before we start, or after an interruption }

PROCEDURE GetLocoFunctions(VAR Loco : LocoRec; ForceRead : Boolean; VAR FunctionArray : ARRAY OF Boolean; VAR OK : Boolean);
{ Read all the functions }

FUNCTION GetLenzSpeed(VAR Loco : LocoRec; ForceRead : Boolean) : Integer;
{ Returns the given loco's speed }

PROCEDURE InitialiseLenzUnit;
{ Such routines as this allow us to initialises the units in the order we wish }

FUNCTION MakePointChange(LocoChipStr : String; P : Integer; Direction : PointStateType; VAR Count : Integer) : Boolean;
{ Select which decoder, which output and which direction (LS100/100 decoders have four outputs. Need to select then deselect after 200 ms! }

PROCEDURE MakeSemaphoreSignalChange(LocoChipStr : String; S, AccessoryAddress : Integer; OnOrOff : SignalStateType; OUT OK : Boolean);
{ Pull a semaphore signal or or off using TrainTech's SC3 }

PROCEDURE ProgramOnTheMain(VAR Loco : LocoRec; ProgramOnTheMainRequest : ProgramOnTheMainType; NewValue : Integer);
{ Program a loco anywhere on the layout (i.e. not on the programming track) }

FUNCTION RequestProgrammingModeCV(CV : Integer) : String;
{ Ask for programming mode (aka service mode) data }

PROCEDURE ResumeOperations(OUT OK : Boolean);
{ Turns the power back on }

FUNCTION ReturnSystemStatus : LenzSystemRec;
{ Return the present System status }

PROCEDURE SetLenzSpeedAndDirection(VAR Loco : LocoRec; LenzSpeed : Integer; LocoDirection : DirectionType; VAR OK : Boolean);
{ Sets the speed by changing bits 4 - 0 - if quickstop, don't bother to read in the loco details first, as we're not interested in what the speed used to be }

PROCEDURE SetLocoDirection(VAR Loco : LocoRec; DirectionRequired : DirectionType; OUT OK : Boolean);
{ Sets/resets bit 7 - up is (arbitrarily) on }

PROCEDURE SetSignalFunction(LocoChipStr : String; S : Integer);
{ Set a numbered function on or off - used for LED signals controlled by LF100XF function only decoders }

PROCEDURE SetSignalRouteFunction(LocoChipStr : String; S: Integer);
{ Set a numbered function on or off - used for LED signals controlled by LF100XF function only decoders }

PROCEDURE SetSingleLocoFunction(VAR Loco : LocoRec; FunctionNum : Integer; TurnOn : Boolean; OUT OK : Boolean);
{ Set a numbered function on or off }

PROCEDURE SetSystemOffline(OfflineMsg : String; Warning : Boolean);
{ Change the caption and the icons to show we're offline }

FUNCTION SetSystemOnline : Boolean;
{ Change the caption and the icons to show we're online - needs a test to see if we are, actually, online *************** 6/2/14 }

FUNCTION SingleLocoFunctionIsOn(VAR Loco : LocoRec; FunctionNum : Integer; ForceRead : Boolean; OUT OK : Boolean) : Boolean;
{ Read whether a numbered function is on or off }

PROCEDURE StopAllLocomotives(VAR OK : Boolean);
{ Emergency stops all trains, but leaves the power on }

PROCEDURE StopAParticularLocomotive(VAR Loco : LocoRec; VAR OK : Boolean);
{ Stops a particular loco }

PROCEDURE StopOperations(OUT OK : Boolean);
{ Turns the power off to the track and to I/O devices; it tells the system to stop sending DCC packets to the track and to switch off the DCC track power }

IMPLEMENTATION

{$R *.dfm}

USES RailDraw, Feedback, GetTime, Startup, MiscUtils, Diagrams, LocoUtils, IDGlobal, Movement, MMSystem, DateUtils, StrUtils, Input, Main, Locks, LocationsUnit, Options,
     LinesUnit, Train, Logging;

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
  FeedbackArray : ARRAY [0..127, 0..1] OF Byte; { needs to store upper and lower nibble of data }
  OneTimeCodeBeingExecuted : Boolean = False;
  SaveTimeCTSLastFoundSet : Cardinal = 0;
  SaveTimeLastDataReceived : Cardinal = 0;
  ShowEmergencyOffMessageVisible : Boolean = False;
  ShowEmergencyStopMessageVisible : Boolean = False;
  SystemStatus : LenzSystemRec;
  TimeCTSLastFoundSet : Cardinal = 0;
  TimeLastUSBDataWritten : Cardinal = 0;
  UnrequestedDataFound : Boolean = False;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
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
    { and AMS's way of getting the low byte }
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

PROCEDURE SetLocoControlledByState(VAR Loco : LocoRec; ControlledByState : LocoControlStateType);
{ Mark a given loco as controlled either by the software or by the LH100 or by the RDC }
BEGIN
  WITH Loco DO BEGIN
    Loco_PreviousControlState := Loco_ControlState;
    Loco_ControlState := ControlledByState;

    { Also mark the additional lighting chips if any as controlled. Note: the address of the additional chip may be NIL if it's the same as the loco chip }
//      IF Train_LightingChipUp <> UnknownLocoChip THEN
//        IF Train_LightingChipUpAddress <> 0 THEN
//          Trains[Train_LightingChipUpAddress].Train_ControlledByProgram := ControlledByProgram;
//
//      IF Train_LightingChipDown <> UnknownLocoChip THEN
//        IF Train_LightingChipDownAddress <> 0 THEN
//          Trains[Train_LightingChipDownAddress].Train_ControlledByProgram := ControlledByProgram; &&&&&
  END;
END; { SetLocoControlledByState }

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

  PROCEDURE WhichFeedbackInputsHaveChanged(XpressNetUnit, NewData : Byte);
  { Checks against the stored feedback data to report any changes }
  VAR
    B : Byte;
    FeedbackUnit : Integer;
    I, Input, Nibble : Integer;

  BEGIN
    FeedbackUnit := XpressNetUnit + 1;
    IF ((FeedbackUnit) < FirstFeedbackUnit) OR ((FeedbackUnit) > LastFeedbackUnit) THEN
      Log('AG Feedback unit number ' + IntToStr(FeedbackUnit) + ' outside designated range')
    ELSE BEGIN
      { upper or lower nibble? }
      IF (NewData AND 16) <> 16 THEN BEGIN
        { If bit 4 set, inputs are 5 to 8, so increment Input accordingly }
        Input := 0;
        Nibble := 0;
      END ELSE BEGIN
        Input := 4;
        Nibble := 1;
      END;

      { XORing only sets in B the bits which are not the same }
      B := FeedbackArray[FeedbackUnit, Nibble] XOR NewData;

      IF B = 0 THEN
        // Log('A Feedback unit ' + IntToStr(FeedbackUnit) + ': no change') { caused by lots of *78*s }
      ELSE BEGIN
        { there is a change - cycle through the four inputs }
        FOR I := 1 TO 4 DO BEGIN
          IF (B AND BinaryCounter[I]) = BinaryCounter[I] THEN BEGIN
            IF (NewData AND BinaryCounter[I]) = BinaryCounter[I] THEN BEGIN
              FeedbackUnitRecords[FeedbackUnit].Feedback_InputOnArray[Input + I] := true;
              DecodeFeedback(FeedbackUnit, Input + I);
            END ELSE BEGIN
              FeedbackUnitRecords[FeedbackUnit].Feedback_InputOnArray[Input + I] := false;
              DecodeFeedback(FeedbackUnit, Input + I);
            END;
          END;
        END;
        { now store the new data instead }
        FeedbackArray[FeedbackUnit, Nibble] := NewData;
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

FUNCTION ExpectedReplyToStr(ExpectedReply : ReplyType) : String;
{ Returns a string with the expected reply type }
BEGIN
  CASE ExpectedReply OF
     Acknowledgment:
       Result := 'Acknowledgment';
     CommandStationSoftwareReply:
       Result := 'Command Station Software Reply';
     ComputerInterfaceSoftwareReply:
       Result := 'Computer Interface Software Reply';
     EmergencyStopReply:
       Result := 'Emergency Stop Reply';
     EverythingTurnedOnReply:
       Result := 'Everything Turned On Reply';
     FeedbackReply:
       Result := 'Feedback Reply';
     LocoAcknowledgment:
       Result := 'Loco Acknowledgment';
     LocoReply:
       Result := 'Loco Reply';
     LocoTakenoverReply:
       Result := 'Loco Taken Over Reply';
     PointAcknowledgment:
       Result := 'Point Acknowledgment';
     PointReply:
       Result := 'Point Reply';
     ProgrammingModeReply:
       Result := 'Programmimg Mode Reply';
     SignalAcknowledgment:
       Result := 'Signal Acknowledgment';
     SystemStatusReply:
       Result := 'System Status Reply';
     { The following two won't ever be reached, but are here for completeness }
     NoReplyExpected:
       Result := 'No Reply Expected';
     TrackPowerOffReply:
       Result := 'Track Power Off Reply';
  END; {CASE}
END; { ExpectedReplyToStr }

{$O-}
PROCEDURE DataIOMainProcedure(TypeOfLogChar : Char; WriteReadVar : WriteReadType; VAR WriteArray, ReadArray : ARRAY OF Byte; ExpectedReply : ReplyType;
                              OUT TimedOut : Boolean; OUT ExpectedDataReceived : Boolean);
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
  ExpectedFeedbackAddress : Integer;
  I : Integer;
  Loco : LocoRec;
  LocoChip : Integer;
  OK : Boolean;
  PortStr : String;
  ResponseOrBroadcast : ResponseOrBroadcastType;
  RetryCount : Integer;
  RetryFlag : Boolean;
  S : String;
  StartTimer : Cardinal;
  TempByte : Byte;
  TempStr : String;
  TempInt : Integer;
  TempWriteArray : ARRAY [0..17] OF Byte;
  TickCount : Cardinal;

BEGIN
  TRY
    IF NOT SystemOnline THEN BEGIN
      ExpectedDataReceived := False;
      TimedOut := False;
      Exit;
    END;

    ResponseOrBroadcast := NoResponse;
    RetryCount := 0;
    ExpectedFeedbackAddress := 0;

    REPEAT
      RetryFlag := False;

      { Writes out data first, unless unrequested data has been received }
      IF (WriteReadVar = WriteThenRead) OR (WriteReadVar = WriteOnly) THEN BEGIN
        { send any output waiting to the LI101 - check first that it can be accepted }
        IF WriteArray[0] <> 0 THEN BEGIN
          { get the length first from bits 3-0, and add the checkbyte }
          WriteArray[GetCommandLen(WriteArray[0]) + 1] := CheckSum(WriteArray);
          { and write it out }
          IF LenzConnection = USBConnection THEN BEGIN
            { send the data as a string }
            S := '';
            FOR I := 0 TO GetCommandLen(WriteArray[0]) + 1 DO
              S := S + IntToHex(WriteArray[I], 2);
            TCPIPForm.ResponsesTCPSendText(S);
          END ELSE
            IF LenzConnection = EthernetConnection THEN BEGIN
              { add the two required header elements then send the data as an array of bytes }
              TempWriteArray[0] := 255;
              TempWriteArray[1] := 254;
              FOR I := 0 TO GetCommandLen(WriteArray[0]) + 1 DO
                TempWriteArray[I + 2] := WriteArray[I];
              TCPIPForm.ResponsesTCPSendBuffer(TempWriteArray, GetCommandLen(WriteArray[0]) + 4);
            END;
        END;

        { Now compose the "From PC" string }
        DebugStr := 'PC request: ';

        CommandLen := GetCommandLen(WriteArray[0]);

        { If we're requesting feedback data, note which unit's data we're looking for }
        IF WriteArray[0] = 66 THEN
          ExpectedFeedbackAddress := WriteArray[1];

        FOR I := 0 TO CommandLen DO
          DebugStr := DebugStr + IntToStr(WriteArray[I]) + '-';
        DebugStr := DebugStr + IntToStr(WriteArray[CommandLen + 1]);

        { now the hex }
        DebugStr := DebugStr + ' [';
        FOR I := 0 TO CommandLen DO
          DebugStr := DebugStr + IntToHex(WriteArray[I], 2) + '-';
        DebugStr := DebugStr + IntToHex(WriteArray[CommandLen + 1], 2);
        DebugStr := DebugStr + ']';

        CASE RetryCount OF
          0:
            ; { do nothing }
          1:
            DebugStr := DebugStr + ' (2nd attempt)';
          2:
            DebugStr := DebugStr + ' (3rd attempt)';
        END; {CASE}

        Log(TypeOfLogChar + ' ' + DebugStr); {+ ' byte 2= ' + DoBitPattern(WriteArray[2]) + ' byte 4= ' + DoBitPattern(WriteArray[4]));}

        ResponseOrBroadcast := NoResponse;
      END;

      TimedOut := False;
      StartTimer := GetTickCount;

      IF (WriteReadVar = WriteThenRead) OR (WriteReadVar = ReadOnly) THEN BEGIN
        REPEAT
          { Initialisations }
          DebugStr := '';
          ExpectedDataReceived := False;
          UnrequestedDataFound := False;
          ErrorFound := False;

          { Clears read array in case data left behind is processed a second time }
          FOR I := 0 TO (ReadArrayLen - 1) DO
            ReadArray[I] := 0;

          { Time out stuff }
          IF (ExpectedReply <> NoReplyExpected) AND (ExpectedReply <> TrackPowerOffReply) THEN BEGIN
            TickCount := (GetTickCount - StartTimer);
            IF TickCount > 5000 THEN BEGIN
              TimedOut := True;
              RetryFlag := True;

              DebugStr := 'timed out in wait for ExpectedReply of ' + ExpectedReplyToStr(ExpectedReply);
              TempStr := ReadAndDeleteDataFromTCPIPList;
              IF TempStr <> '' THEN
                DebugStr := DebugStr + ' [' + TempStr + ']';

              Log('XG ' + DebugStr);
            END;

            IF TimedOut THEN
              Log('XG **** Timed Out');

            IF TimedOut THEN
              Continue;
          END;

          TempStr := ReadAndDeleteDataFromTCPIPList;
          IF TempStr = '' THEN BEGIN
            IF ExpectedReply = NoReplyExpected THEN
              Exit;

            StopSystemTimer;

            Application.ProcessMessages;

            StartSystemTimer;
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
//              ASM
//                Int 3;
//              END; {ASM}
              { need to recover from this by doing the read again? **** 6/11/06 }

              { write out the string }
              FOR I := 0 TO CommandLen + 1 DO
                DebugStr := DebugStr + IntToStr(ReadArray[I]) + '=';
              Log('AG ' + DebugStr);
              Retryflag := True;
            EXCEPT
              ON E : Exception DO
                Log('EG ReadInDataMainProcedure: ' + E.ClassName + ' error raised, with message: ' + E.Message);
            END; {TRY}
          END;

          IF OK THEN BEGIN
            { Reset the watchdog timer and the timer interval, as some data has arrived - ignore system status messages, as we use those to get a result from Lenz system }
            IF (LenzWindow <> NIL) AND (ReadArray[0] <> 98) THEN BEGIN
              WatchdogTimerCount := 0;

              { this seems to reset the timer, though it's undocumented }
              LenzWindow.LenzWatchDogTimer.Enabled := False;
              LenzWindow.LenzWatchDogTimer.Enabled := True;

              LenzWindow.LenzWatchDogTimer.Interval := 60000; { default is one minute }
            END;

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
                  TypeOfLogChar := 'A';
                END; {CASE}
              66..78:
                TypeOfLogChar := 'T'; { track circuit }
              198..230:
                TypeOfLogChar := 'L'; { loco }
            ELSE
              TypeOfLogChar := 'A'; { general }
            END; {CASE}

            IF ResponseOrBroadcast = Response THEN
              Log(TypeOfLogChar + ' Lenz response: ' + DebugStr)
            ELSE
              IF ResponseOrBroadcast = Broadcast THEN
                Log('A Lenz broadcast: ' + DebugStr);

            { Write out bytes as bits if run-time parameter Y is set }
            IF ShowByteParam <> '' THEN BEGIN
              IF ShowByteParam = 'ALL' THEN BEGIN
                { Write all the bytes as bits }
                DebugStr := '';
                FOR I := 1 TO (CommandLen + 1) DO
                  DebugStr := DebugStr + '[' + DoBitPattern(ReadArray[I]) + '] ';
                Log('A ' + StringOfChar(' ', 64) + 'All bytes: ' + DebugStr); { 64 shouldn't be a magic number *** }
              END ELSE
                IF (StrToInt(ShowByteParam) >= 0) AND (StrToInt(ShowByteParam) <= 14) THEN BEGIN
                  { Write a single byte as bits }
                  IF StrToInt(ShowByteParam) > (CommandLen + 1) THEN
                    DebugStr := '---- ----'
                  ELSE
                    DebugStr := DoBitPattern(ReadArray[StrToInt(ShowByteParam)]);

                  IF ResponseOrBroadcast = Response THEN
                    Log('A *** Response ***' + StringOfChar(' ', 48) + 'Byte ' + ShowByteParam + ': [' + DebugStr + ']')
                  ELSE
                    IF ResponseOrBroadcast = Broadcast THEN
                      Log('A *** Broadcast ***' + StringOfChar(' ', 47) + 'Byte ' + ShowByteParam + ': [' + DebugStr + ']');
                END;
            END;

            { Examine the first byte returned to identify it, and see if it is what was expected }
            CASE ReadArray[0] OF
              1:
                { maybe an acknowledgment or an error - depends what follows the '1' }
                 CASE ReadArray[1] OF
                  1:
                    BEGIN
                      ErrorMsg := 'Error between interface and PC';
                      Log('XG ' + ErrorMsg);
                      RetryFlag := True;
                    END;
                  2:
                    BEGIN
                      ErrorMsg := 'Error between interface and command station';
                      Log('XG ' + ErrorMsg);
                      RetryFlag := True;
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
                      RetryFlag := True;
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
                  Log('A ' + DebugStr);
                END;
              66: { $42 }
                BEGIN
                  IF (ExpectedReply = NoReplyExpected) AND (WhatSortOfDecoderIsIt(ReadArray[1], ReadArray[2]) = AccessoryDecoderWithoutFeedbackStr) THEN BEGIN
                    { see if it's point data, as that seems to be an acknowledgment }
                    ExpectedDataReceived := True;
                    Log('T An unexpected feedback broadcast for point selection has arrived');
                  END ELSE
                    { see if we've asked for the data (during startup, for instance) }
                    IF (ExpectedReply = FeedbackReply) AND (ExpectedFeedbackAddress = ReadArray[1]) THEN BEGIN
                      ExpectedDataReceived := True;
                      Log('T An expected reply from unit ' + IntToStr(ReadArray[1] + 1) + ' has arrived');
                    END ELSE BEGIN
                      UnrequestedDataFound := True;
//                      Log('T Unrequested feedback has arrived');
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
//                    Log('T Unrequested feedback has arrived');
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
//                    Log('T Unrequested feedback has arrived');
                  END;
                  GetFeedbackReply(ReadArray);
                END;
              72: { $48 }
                BEGIN
                  IF (ExpectedReply = FeedbackReply) THEN
                    ExpectedDataReceived := True
                  ELSE BEGIN
                    UnrequestedDataFound := True;
//                    Log('T Unrequested feedback has arrived');
//                    Log('TG -72-');
                    WriteStringToFeedbackWindow('*72*');
                  END;
                  GetFeedbackReply(ReadArray);
                END;
              74: { $4A }
                BEGIN
                  IF (ExpectedReply = FeedbackReply) THEN
                    ExpectedDataReceived := True
                  ELSE BEGIN
                    UnrequestedDataFound := True;
//                    Log('T Unrequested feedback has arrived');
//                    Log('TG -74-');
                    WriteStringToFeedbackWindow('*74*');
                  END;
                  GetFeedbackReply(ReadArray);
               END;
              76: { $4C }
                BEGIN
                  IF (ExpectedReply = FeedbackReply) THEN
                    ExpectedDataReceived := True
                  ELSE BEGIN
                    UnrequestedDataFound := True;
//                    Log('T Unrequested feedback has arrived');
//                    Log('TG -76-');
                    WriteStringToFeedbackWindow('*76*');
                  END;
                  GetFeedbackReply(ReadArray);
                END;
              78: { $4E }
                BEGIN
                  IF (ExpectedReply = FeedbackReply) THEN
                    ExpectedDataReceived := True
                  ELSE BEGIN
                    UnrequestedDataFound := True;
//                    Log('T Unrequested feedback has arrived');
//                    Log('TG -78-');
                    WriteStringToFeedbackWindow('*78*');
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
                        MainUnitWindow.SendStringToSpeechProgram('ShortCircuit');
                      SystemStatus.EmergencyOff := True;
                      ErrorMsg := '*** power off ***';
                      Log('EG '+ ErrorMsg + ' {BLANKLINEBEFORE}');
                      WriteStringToFeedbackWindow(ErrorMsg);
                    END;
                  1:
                    IF ExpectedReply = EverythingTurnedOnReply THEN
                      ExpectedDataReceived := True { when would this happen? }
                    ELSE BEGIN
                      UnrequestedDataFound := True;
                      SystemStatus.EmergencyOff := False;
                      SystemStatus.EmergencyStop := False;
                      ErrorMsg := '*** power on ***';
                      Log('EG '+ ErrorMsg + ' {BLANKLINEBEFORE}');
                      WriteStringToFeedbackWindow(ErrorMsg);
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
                      RetryFlag := True;
                    END;
                  128: { $80 }
                    BEGIN
                      ErrorMsg := 'Transfer error (XOR not correct)';
                      Log('EG ' + ErrorMsg);
                      ErrorFound := True;
                      RetryFlag := True;
                    END;
                  129: { $81 }
                    BEGIN
                      ErrorMsg := 'Command station busy';
                      Log('EG ' + ErrorMsg);
                      ErrorFound := True;
                      RetryFlag := True;
                      Pause(2000, False);
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
                          Log('A Returning system status for ' + PortStr +': start mode=manual')
                        ELSE
                          SystemStatusStr := 'start mode=auto - may need to send start-mode command (see s.2.1.4 of LI101F manual)';

                        IF SystemStatusStr <> '' THEN
                          Log('A Returning system status for ' + PortStr + ': ' + SystemStatusStr);
                      END;
                    END;
                END; {CASE}
              99: { $63 }
                CASE ReadArray[1] OF
                  16:  { $10 }
                    { programming (service) mode response for Register and Page mode }
                    BEGIN
                      ErrorMsg := 'Programming (service) mode response for Register & Page mode ';
                      Log('EG ' + ErrorMsg);
                      { Needs more code if to be used }
                      ErrorFound := True;
                    END;
                  20: { $14 }
                    { programming (service) mode response for Direct CV mode }
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
                      Log('A ' + DebugStr);
                    END;
                END; {CASE}
              129: { $81 }{ Emergency stop - msg will arrive four times }
                IF ExpectedReply = EmergencyStopReply THEN
                  ExpectedDataReceived := True
                ELSE IF (ReadArray[1] = 0) THEN BEGIN
                  UnrequestedDataFound := True;
                  SystemStatus.EmergencyStop := True;
                  ErrorMsg := '*** emergency stop ***';
                  Log('EG ' + ErrorMsg + ' {BLANKLINEBEFORE}');
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
                      Log(LocoChipToStr(LocoChip) + ' L taken over by ' + ControlledByStateToStr(ControlledByUser));
                      IF GetLocoRecFromLocoChip(LocoChip, Loco) THEN BEGIN
                        SetLocoControlledByState(Loco, ControlledByUser);
                        Log(LocoChipToStr(LocoChip) + ' L taken over by ' + ControlledByStateToStr(ControlledByUser));
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

            { loop if necessary - not needed if no reply was awaited, or if the reply received was the one that was expected }
          END;
        UNTIL (ExpectedReply = NoReplyExpected) OR ExpectedDataReceived OR TimedOut OR ErrorFound OR (RetryFlag = True);

        IF UnRequestedDataFound THEN BEGIN
          IF InTestingMode THEN
            DebugWindow.Caption := DebugWindow.Caption + '.';
        END;
      END;

      IF RetryFlag = True THEN
        Inc(RetryCount);

    UNTIL (RetryFlag = False) OR (RetryCount > 2);

    IF (RetryCount > 12) AND SystemOnline THEN
      SetSystemOffline('3 failed attempts to write/read data from the LAN/USB interface - system now offline', SoundWarning);

  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG : ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DataIOMainProcedure }
{$O+}

PROCEDURE DataIO{1}; Overload
{ This is called by the regular check for broadcasts, and is therefore read only }
CONST
  CheckTimeOuts = True;

VAR
  ExpectedDataReceived : Boolean;
  ReadArray : ARRAY[0..15] OF Byte;
  TimedOut : Boolean;
  WriteArray : ARRAY[0..15] OF Byte;
  WriteReadVar : WriteReadType;

BEGIN
  WriteReadVar := ReadOnly;
  DataIOMainProcedure('A', WriteReadVar, WriteArray, ReadArray, NoReplyExpected, TimedOut, ExpectedDataReceived);
END; { DataIO-1 }

PROCEDURE DataIO{2}(WriteArray : ARRAY OF Byte); Overload
{ This is just called to write data with no reply anticipated }
CONST
  CheckTimeOuts = True;

VAR
  ExpectedDataReceived : Boolean;
  ReadArray : ARRAY[0..15] OF Byte;
  TimedOut : Boolean;
  WriteReadVar : WriteReadType;

BEGIN
  WriteReadVar := WriteOnly;
  DataIOMainProcedure('A', WriteReadVar, WriteArray, ReadArray, NoReplyExpected, TimedOut, ExpectedDataReceived);
END; { DataIO-2 }

PROCEDURE DataIO{3}(TypeOfLogChar : Char; WriteArray : ARRAY OF Byte; ExpectedReply : ReplyType; OUT ExpectedDataReceived : Boolean); Overload;
{ If there's an expected reply then this must be a WriteThenRead type }
CONST
  CheckTimeOuts = True;

VAR
  ReadArray : ARRAY[0..15] OF Byte;
  TimedOut : Boolean;
  WriteReadVar : WriteReadType;

BEGIN
  WriteReadVar := WriteThenRead;
  DataIOMainProcedure(TypeOfLogChar, WriteReadVar, WriteArray, ReadArray, ExpectedReply, TimedOut, ExpectedDataReceived);
END; { DataIO-3 }

PROCEDURE DataIO{4}(TypeOfLogChar : Char; WriteArray : ARRAY OF Byte; VAR ReadArray : ARRAY OF Byte; ExpectedReply : ReplyType; OUT ExpectedDataReceived : Boolean);
                    Overload;
{ If there's an expected reply then this must be a WriteThenRead type }
CONST
  CheckTimeOuts = True;

VAR
  TimedOut : Boolean;
  WriteReadVar : WriteReadType;

BEGIN
  WriteReadVar := WriteThenRead;
  DataIOMainProcedure(TypeOfLogChar, WriteReadVar, WriteArray, ReadArray, ExpectedReply, TimedOut, ExpectedDataReceived);
END; { DataIO-4 }

PROCEDURE DataIO{5}(TypeOfLogChar : Char; WriteArray : ARRAY OF Byte; VAR ReadArray : ARRAY OF Byte; ExpectedReply : ReplyType; OUT TimedOut : Boolean;
                    OUT ExpectedDataReceived : Boolean); Overload;
{ If there's an expected reply then this must be a WriteThenRead type }
VAR
  WriteReadVar : WriteReadType;

BEGIN
  WriteReadVar := WriteThenRead;
  DataIOMainProcedure(TypeOfLogChar, WriteReadVar, WriteArray, ReadArray, ExpectedReply, TimedOut, ExpectedDataReceived);
END; { DataIO-5 }

PROCEDURE DoCheckForUnexpectedData(UnitRef : String; CallingStr : String);
{ See if any data has arrived }
BEGIN
  IF SystemOnline THEN
    DataIO;
END; { DoCheckForUnexpectedData }

PROCEDURE ProgramOnTheMain(VAR Loco : LocoRec; ProgramOnTheMainRequest : ProgramOnTheMainType; NewValue : Integer);
{ Program a loco anywhere on the layout (i.e. not on the programming track) }
VAR
  OK : Boolean;
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  WITH Loco DO BEGIN
    WriteArray[0] := 230; { header byte }
    WriteArray[1] := 48;  { identification byte }
    WriteArray[2] := GetLocoChipHighByte(Loco_LocoChip); { data byte 1 - AH }
    WriteArray[3] := GetLocoChipLowByte(Loco_LocoChip);  { data byte 2 - AL }

    CASE ProgramOnTheMainRequest OF
      ChangeDirectionToUp:
        BEGIN
          Log(Loco_LocoChipStr + ' LG Programming on the Main : changing direction to up');

          { a bit mode write request: change of direction is CV 29 bit 0, so need to send 29-1, as CVs start here at 0 }
          WriteArray[4] := 232; { data byte 3 : 1110 1000 (1110 10CC where CC is top two bits of CV 0-1023) }
          WriteArray[5] := 28;  { data byte 4 : 0001 1100 - CCCC CCCC - remaining bits of CV }
          WriteArray[6] := 240; { data byte 5 : 1111 0000 - 1111 WBBB - W=bit value 0 or 1, BBB=bit location (000=bit 0, 111=bit 7) }
        END;
      ChangeDirectionToDown:
        BEGIN
          Log(Loco_LocoChipStr + ' LG Programming on the Main : changing direction to down');

          { a bit mode write request: change of direction is CV 29 bit 0, so need to send 29-1, as CVs start here at 0 }
          WriteArray[4] := 232; { data byte 3 : 1110 1000 (1110 10CC where CC is top two bits of CV 0-1023) }
          WriteArray[5] := 28;  { data byte 4 : 0001 1100 - CCCC CCCC - remaining bits of CV }
          WriteArray[6] := 248; { data byte 5 : 1111 1000 - 1111 WBBB - W=bit value 0 or 1, BBB=bit location (000=bit 0, 111=bit 7) }
        END;
      ChangeAcceleration:
        { a byte mode write request: change of acceleration is CV 3, so need to send 3-1, as CVs start here at 0 }
        BEGIN
          Log(Loco_LocoChipStr + ' LG Programming on the Main : changing acceleration to ' + IntToStr(NewValue));

          WriteArray[4] := 236;      { data byte 3 : 1110 1100 (1110 11CC where CC is top two bits of CV 0-1023) }
          WriteArray[5] := 2;        { data byte 4 : 0000 0010 - CCCC CCCC - remaining bits of CV }
          WriteArray[6] := NewValue; { data byte 5 : the new value of the CV }
        END;
      ChangeDeceleration:
        { a byte mode write request: change of acceleration is CV 4, so need to send 4-1, as CVs start here at 0 }
        BEGIN
          Log(Loco_LocoChipStr + ' LG Programming on the Main : changing deceleration to ' + IntToStr(NewValue));

          WriteArray[4] := 236;      { data byte 3 : 1110 1100 (1110 11CC where CC is top two bits of CV 0-1023) }
          WriteArray[5] := 3;        { data byte 4 : 0000 0011 - CCCC CCCC - remaining bits of CV }
          WriteArray[6] := NewValue; { data byte 5 : the new value of the CV }
        END;
    END; {CASE}

    DataIO('L', WriteArray, LocoAcknowledgment, OK);
  END; {WITH}
END; { ProgramOnTheMain }

{$O+}
PROCEDURE ReadInLocoDetails(VAR Loco : LocoRec; VAR TempSpeedByte : Byte; OUT OK : Boolean);
{ Read in the supplied details of the specified loco - error if not }

  FUNCTION Decode128SpeedStep(TempSpeedByte : Byte) : Integer;
  { Decode the speed - new 128 step (v.3) rate }
  BEGIN
    { clear top bit first }
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
  TestByte : Byte;

BEGIN { ReadInLocoDetails }
  TestByte := 0;

  WITH Loco DO BEGIN
    Log(Loco_LocoChipStr + ' L Requesting loco details {BLANKLINEBEFORE}');

    WriteArray[0] := 227;
    WriteArray[1] := 0;
    WriteArray[2] := GetLocoChipHighByte(Loco_LocoChip);
    WriteArray[3] := GetLocoChipLowByte(Loco_LocoChip);

    DataIO('L', WriteArray, ReadArray, LocoReply, OK);

    IF OK THEN BEGIN
      DebugStr := 'Received loco details:';

      CASE ReadArray[0] OF
        226:
          Log(Loco_LocoChipStr + ' L (Multi-unit address');
            { but will need to store speed of multi-unit separately ? **** }
        229:
          Log(Loco_LocoChipStr + ' L (Loco forming part of a multi-unit with a multi-unit address of ' + IntToStr(ReadArray[5]) + ')');
            { but will need to store speed of multi-unit separately ? **** }
        230:
          { report if forming part of a double header }
          Log(Loco_LocoChipStr + ' L (Loco forming part of a double header with ' + IntToStr(GetLocoChipFromTwoBytes(ReadArray[5], ReadArray[6])) + ')');
      END; {CASE}

      CASE ReadArray[0] OF
        226, 228..230:
          BEGIN
            IDByte := ReadArray[1];

            { See if loco controlled by another device }
            IF (IDByte AND 8) = 8 THEN BEGIN
              { testing bit 3 - loco controlled by something else }
              SetLocoControlledByState(Loco, ControlledByUnknownDevice);
              DebugStr := DebugStr + ' [previously taken over by ' + ControlledByStateToStr(ControlledByUnknownDevice) + ']';
            END ELSE
              SetLocoControlledByState(Loco, ControlledByProgram);

            { get its speed step mode from bits 0-2 }
            IF (IDByte AND 4) = 4 THEN
              Loco_SpeedStepMode := 128
            ELSE
              IF (IDByte AND 2) = 2 THEN
                Loco_SpeedStepMode := 28
              ELSE
                IF (IDByte AND 1) = 1 THEN
                  Loco_SpeedStepMode := 27
                ELSE
                  IF (IDByte AND 0) = 0 THEN
                    Loco_SpeedStepMode := 14;

            IF (Loco_SpeedStepMode <> 128) AND (Loco_SpeedStepMode <> 28) THEN BEGIN
              Debug('Error - using wrong speed mode!');
              { what else **** }
            END;

            TempSpeedByte := ReadArray[2];
            Loco_SpeedByte := TempSpeedByte;
            Loco_SpeedByteReadIn := True;

            { and now its speed }
            IF Loco_SpeedStepMode = 28 THEN
              SpeedNum := Decode28SpeedStep(TempSpeedByte)
            ELSE
              SpeedNum := Decode128SpeedStep(TempSpeedByte);

            Loco_CurrentLenzSpeed := SpeedNum;

            DebugStr := DebugStr + ' speed=' + IntToStr(SpeedNum);

            { and direction of travel }
            IF (TempSpeedByte AND 128) = 128 THEN BEGIN
              Loco_CurrentDirection := Up;
              DebugStr := DebugStr + ' Up'
            END ELSE BEGIN
              Loco_CurrentDirection := Down;
              DebugStr := DebugStr + ' Down';
            END;

            { and loco functions set }
            IF ReadArray[0] <> 226 THEN BEGIN
              { add the data to the loco record - not applicable to multi-unit addresses *** ? *** }
              Loco_Functions0To4Byte := ReadArray[3];
              Loco_Functions5To12Byte := ReadArray[4];
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
                IF ((Loco_Functions0To4Byte AND TestByte) = TestByte) THEN
                  Loco_Functions[FunctionNum] := True
                ELSE
                  Loco_Functions[FunctionNum] := False;
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
                IF ((Loco_Functions5To12Byte AND TestByte) = TestByte) THEN
                  Loco_Functions[FunctionNum] := True
                ELSE
                  Loco_Functions[FunctionNum] := False;
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
      Log(Loco_LocoChipStr + ' L ' + DebugStr);
    END;
  END; {WITH}
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

BEGIN
  { Ask for details }
  DecoderNumString := LocoChipToStr(DecoderNum);
  Log('L L=' + DecoderNumString + ': requesting function decoder details {BLANKLINEBEFORE}');
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

FUNCTION GetLenzSpeed(VAR Loco : LocoRec; ForceRead : Boolean) : Integer;
{ Returns the given loco's speed - if ForceRead set, reads it in even if we think we know what it is }
VAR
  OK : Boolean;
  TempSpeedByte : Byte;

BEGIN
  { First check whether it's been ever been asked for, whether it's been taken over by another controller and changed - otherwise no point continually asking the system for
    it.
  }
  WITH Loco DO BEGIN
    IF (Loco_ControlState = ControlledByUser)
    OR ForceRead
    THEN BEGIN
      ReadInLocoDetails(Loco, TempSpeedByte, OK);
      IF NOT OK THEN
        Log(Loco_LocoChipStr + ' XG "GetLenzSpeed" routine failed');
    END;

    { Either return the newly obtained speed, or the stored speed }
    Result := Loco_CurrentLenzSpeed;
  END; {WITH}
END; { GetLenzSpeed }

PROCEDURE WriteLocoSpeedOrDirection(VAR Loco : LocoRec; TempSpeedByte : Byte; VAR OK : Boolean);
{ Write out the speed or direction of a given locomotive }
VAR
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  WITH Loco DO BEGIN
    WriteArray[0] := 228;
    IF Loco_SpeedStepMode = 28 THEN
      WriteArray[1] := 18
    ELSE
      { 128 speed steps }
      WriteArray[1] := 19;

    WriteArray[2] := GetLocoChipHighByte(Loco_LocoChip);
    WriteArray[3] := GetLocoChipLowByte(Loco_LocoChip);
    WriteArray[4] := TempSpeedByte;

    { Now read details in - look for acknowledgment }
    DataIO('L', WriteArray, LocoAcknowledgment, OK);

    IF NOT OK THEN
      Log(Loco_LocoChipStr + ' L Data not acknowledged')
    ELSE BEGIN
      SetLocoControlledByState(Loco, ControlledByProgram);
      Loco_SpeedByte := TempSpeedByte;
    END;
    Log(Loco_LocoChipStr + ' L Data acknowledged');
  END; {WITH}
END; { WriteLocoSpeedOrDirection }

FUNCTION ReadComputerInterfaceSoftwareVersion : Boolean;
{ Ask the LI101 its own software version }
VAR
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  Log('A Requesting computer interface software version {BLANKLINEBEFORE}');
  WriteArray[0] := 240;
  DataIO('A', WriteArray, ComputerInterfaceSoftwareReply, Result);
END; { ReadComputerInterfaceSoftwareVersion }

FUNCTION ReadCommandStationSoftwareVersion : Boolean;
{ Ask the LI101 for the software version of the command station }
VAR
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  Log('A Requesting command station software version {BLANKLINEBEFORE}');
  WriteArray[0] := 33;
  WriteArray[1] := 33;
  DataIO('A', WriteArray, CommandStationSoftwareReply, Result);
END; { ReadCommandStationSoftwareVersion }

FUNCTION RequestProgrammingModeCV(CV : Integer) : String;      { half written 1/2/13 }
{ Ask for programming mode (aka service mode) data }
VAR
  OK : Boolean;
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  IF (CV < 1) OR (CV > 256) THEN
    ShowMessage('CVs should be between 1 and 256')
  ELSE BEGIN
    IF CV = 256 THEN
      CV := 0;

    Log('A Initialising request for CV from loco on programming track - direct mode CV read request {BLANKLINEBEFORE}');
    WriteArray[0] := 34;
    WriteArray[1] := 21;
    WriteArray[2] := CV;

    DataIO(WriteArray);

    Log('A Finalising request for CV from loco on programming track - requesting result from programming mode {BLANKLINEBEFORE}');
    WriteArray[0] := 33;
    WriteArray[1] := 16;
    WriteArray[2] := 49;

    DataIO('A', WriteArray, ProgrammingModeReply, OK);

    ResumeOperations(OK);
  END;
END; { RequestProgrammingModeCV }

PROCEDURE SetLenzSpeedAndDirection(VAR Loco : LocoRec; LenzSpeed : Integer; LocoDirection : DirectionType; VAR OK : Boolean);
{ Sets the speed by changing bits 4 - 0 - if quickstop, don't bother to read in the loco details first, as we're not interested in what the speed used to be }
VAR
  DebugStr : String;
  TempSpeedByte : Byte;

BEGIN
  TRY
    OK := True;

    WITH Loco DO BEGIN
      IF (Loco_SpeedStepMode = 28) AND (LenzSpeed > 28) THEN BEGIN
        Log(Loco_LocoChipStr + ' XG Fatal Error: Lenz speed ' + IntToStr(LenzSpeed) + ' supplied');
        OK := False;
      END;

      IF OK THEN BEGIN
        IF Loco_LocoChip = DapolCleaningWagonLocoChip THEN BEGIN
          { it's a special case - if it's non-zero set it to eight }
          IF LenzSpeed > 0 THEN
            LenzSpeed := 8;
        END;

        IF NOT SystemOnline THEN BEGIN
          IF LenzSpeed = QuickStop THEN
            Debug('Speed for loco ' + Loco_LocoChipStr + ' would be set to 0 with quick stop set')
          ELSE BEGIN
            IF Loco_LocoChip = DapolCleaningWagonLocoChip THEN
              Debug('Speed for Dapol Cleaning Wagon ' + Loco_LocoChipStr + ' would be set to ' + IntToStr(LenzSpeed))
            ELSE
              Debug('Speed for loco ' + Loco_LocoChipStr + ' would be set to ' + IntToStr(LenzSpeed));
          END;
        END ELSE BEGIN
          DebugStr := '';
          OK := False;
          { If we're controlling it, we know its speed }
          IF Loco_SpeedByteReadIn AND (Loco_ControlState = ControlledByProgram) THEN
            TempSpeedByte := Loco_SpeedByte
          ELSE BEGIN
            ReadInLocoDetails(Loco, TempSpeedByte, OK);
            IF NOT OK THEN
              Log(Loco_LocoChipStr + ' XG Locoread not ok-'); { ****** }
          END;

          IF LenzSpeed = QuickStop THEN BEGIN
            TempSpeedByte := Loco_SpeedByte;
            { reset bits 1-6, leaving bit 7 as it indicates direction }
            TempSpeedByte := TempSpeedByte AND NOT 126; { 0111 1110 }
            { set bit 0 for emergency stop }
            TempSpeedByte := TempSpeedByte OR 1; { 0000 0001 }
          END ELSE BEGIN
            { not quickstop - now set the appropriate bits }
            IF Loco_SpeedStepMode = 28 THEN BEGIN
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
          END;

          { set top bit for direction }
          IF LocoDirection = Up THEN BEGIN
            TempSpeedByte := TempSpeedByte OR 128; { 1000 000 }
            DebugStr := ' Up'
          END ELSE BEGIN
            TempSpeedByte := TempSpeedByte AND NOT 128; { 0000 000 }
            DebugStr := ' Down';
          END;

          IF LenzSpeed = QuickStop THEN BEGIN
            IF Loco_LocoChip = DapolCleaningWagonLocoChip THEN
              Log(Loco_LocoChipStr + ' L Dapol Cleaning Wagon - writing speed data 0 (with quick stop set) [' + DoBitPattern(TempSpeedByte) + ']' + DebugStr)
            ELSE
              Log(Loco_LocoChipStr + ' L Writing speed data 0 (with quick stop set) [' + DoBitPattern(TempSpeedByte) + ']' + DebugStr);
          END ELSE BEGIN
            IF Loco_LocoChip = DapolCleaningWagonLocoChip THEN
              Log(Loco_LocoChipStr + ' L Dapol Cleaning Wagon - writing speed data ' + IntToStr(LenzSpeed) + ' [' + DoBitPattern(TempSpeedByte) + ']' + DebugStr)
            ELSE
              Log(Loco_LocoChipStr + ' L Writing speed data ' + IntToStr(LenzSpeed) + ' [' + DoBitPattern(TempSpeedByte) + ']' + DebugStr);
          END;

          { now write the byte back }
          WriteLocoSpeedOrDirection(Loco, TempSpeedByte, OK);
          IF NOT OK THEN
            Log(Loco_LocoChipStr + ' LG Data for loco ' + Loco_LocoChipStr + ' not written');

          IF OK THEN BEGIN
            IF LenzSpeed = QuickStop THEN
              Loco_CurrentLenzSpeed := 0
            ELSE
              Loco_CurrentLenzSpeed := LenzSpeed;

            { Need to update the loco direction variable, as there may well not have been an explicit direction change by means of SetDirection, rather a speed change
              with the direction bit set differently.
            }
            IF LocoDirection <> Loco_CurrentDirection THEN
              Loco_CurrentDirection := LocoDirection;
          END;
        END;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG SetLenzSpeed: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SetLenzSpeedAndDirection }

FUNCTION AdjustLenzSpeed(VAR Loco : LocoRec; Value : Integer; LocoDirection : DirectionType; OUT OK : Boolean) : Integer;
{ Increase or decrease the speed by given amount }
VAR
  NewSpeed : Integer;

BEGIN
  NewSpeed := 0;

  WITH Loco DO BEGIN
    IF NOT SystemOnline THEN BEGIN
      IF Value > 0 THEN
        Debug('Speed for ' + Loco_LocoChipStr + ' would be increased by ' + IntToStr(Value))
      ELSE
        IF Value < 0 THEN
          Debug('Speed for ' + Loco_LocoChipStr + ' would be decreased by ' + IntToStr(Abs(Value)))
        ELSE BEGIN
          Debug('val=0');
          Log(Loco_LocoChipStr + ' L Can''t increase or decrease by zero');
        END;
      OK := True;
    END ELSE BEGIN
      IF Value = 0 THEN BEGIN
        { can't increase or decrease by zero }
        Debug('val=0');
        Log(Loco_LocoChipSTr + ' L Can''t increase or decrease by zero');
      END ELSE BEGIN
        NewSpeed := GetLenzSpeed(Loco, NOT ForceARead);
        NewSpeed := NewSpeed + Value;

        IF NewSpeed > LocoMaxSpeed THEN
          NewSpeed := LocoMaxSpeed
        ELSE
          IF NewSpeed < LocoMinSpeed THEN
            NewSpeed := LocoMinSpeed;

        SetLenzSpeedAndDirection(Loco, NewSpeed, LocoDirection, OK);
      END;
    END;
  END; {WITH}
  Result := NewSpeed;
END; { AdjustLenzSpeed }

PROCEDURE SetLocoDirection(VAR Loco : LocoRec; DirectionRequired : DirectionType; OUT OK : Boolean);
{ Sets/resets bit 7 - up is (arbitrarily) on }
VAR
  DebugStr : String;
  TempSpeedByte : Byte;
  TestByte : Byte;

BEGIN
  OK := True;

  WITH Loco DO BEGIN
    IF DirectionRequired = Up THEN
      DebugStr := 'Setting loco direction to Up'
    ELSE
      DebugStr := 'Setting loco direction to Down';
    Log(Loco_LocoChipStr + ' L ' + DebugStr);

    IF SystemOnline THEN BEGIN
      IF Loco_SpeedByteReadIn AND (Loco_ControlState = ControlledByProgram) AND (Loco_CurrentDirection <> UnknownDirection) THEN
        TempSpeedByte := Loco_SpeedByte
      ELSE
        ReadInLocoDetails(Loco, TempSpeedByte, OK);

      TestByte := TempSpeedByte;

      { Needs to check loco is stationary first - check by looking at bits }
      IF Loco_SpeedStepMode = 28 THEN
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

        WriteLocoSpeedOrDirection(Loco, TempSpeedByte, OK);
        IF NOT OK THEN BEGIN
          WriteLocoSpeedOrDirection(Loco, TempSpeedByte, OK);
          IF NOT OK THEN BEGIN
            StopAllLocomotives(OK);
            MakeSound(1);
            TurnAutoModeOff(NOT ByUser);
            ShowMessage('Data for loco ' + Loco_LocoChipStr + ' not written - auto mode suspended');
          END;
        END;

        IF OK THEN BEGIN
          Loco_CurrentDirection := DirectionRequired;
          Log(Loco_LocoChipStr + ' L Loco direction changed to ' + DebugStr);
          Loco_SpeedByte := TempSpeedByte;
        END ELSE
          Log(Loco_LocoChipStr + ' L Loco direction not changed');
      END ELSE BEGIN
        Log(Loco_LocoChipStr + ' LG Loco direction not changed - loco not stationary');
        OK :=  False;
      END;
    END;
  END; {WITH}
END; { SetLocoDirection }

PROCEDURE GetLocoFunctions(VAR Loco : LocoRec; ForceRead : Boolean; VAR FunctionArray : ARRAY OF Boolean; VAR OK : Boolean);
{ Read all the functions }
VAR
  DebugStr : String;
  FunctionNum : Integer;
  TempSpeedByte : Byte;
  TestByte : Byte;

BEGIN
  TestByte := 0;
  OK := True;
  WITH Loco DO BEGIN
    IF (Loco_ControlState = ControlledByProgram)
    OR ForceRead
    THEN
      ReadInLocoDetails(Loco, TempSpeedByte, OK);

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
         IF (TestByte AND Loco_Functions0To4Byte) <> TestByte THEN BEGIN
           FunctionArray[FunctionNum] := False;
           DebugStr := DebugStr + 'F' + IntToStr(FunctionNum) + ': off, '
         END ELSE BEGIN
           FunctionArray[FunctionNum] := True;
           DebugStr := DebugStr + 'F' + IntToStr(FunctionNum) + ': on, '
         END;

        5, 6, 7, 8, 9, 10, 11, 12:
         IF (TestByte AND Loco_Functions5To12Byte) <> TestByte THEN BEGIN
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

    Log(Loco_LocoChipStr + ' L ' + DebugStr);
  END; {WITH}
END; { GetLocoFunctions }

FUNCTION SingleLocoFunctionIsOn(VAR Loco : LocoRec; FunctionNum : Integer; ForceRead : Boolean; OUT OK : Boolean) : Boolean;
{ Read whether a numbered function is on or off }
VAR
  DebugStr : String;
  TempSpeedByte : Byte;
  TestByte : Byte;

BEGIN
  TestByte := 0;
  OK := True;
  WITH Loco DO BEGIN
    IF (Loco_ControlState = ControlledByProgram)
    OR ForceRead
    THEN
      ReadInLocoDetails(Loco, TempSpeedByte, OK);

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
       IF (TestByte AND Loco_Functions0To4Byte) = TestByte THEN
         Result := True;
      5, 6, 7, 8, 9, 10, 11, 12:
       IF (TestByte AND Loco_Functions5To12Byte) = TestByte THEN
         Result := True;
    END; {CASE}

    IF ForceRead THEN BEGIN
      IF Result = True THEN
        DebugStr := 'Function ' + IntToStr(FunctionNum) + ' is on'
      ELSE
        DebugStr := 'Function ' + IntToStr(FunctionNum) + ' is off';

      DebugStr := DebugStr + ' [force read]';

      Log(Loco_LocoChipStr + ' L ' + DebugStr + '  {BLANKLINEBEFORE}');
    END;
  END; {WITH}
END; { SingleLocoFunctionIsOn }

PROCEDURE SetSingleLocoFunction(VAR Loco : LocoRec; FunctionNum : Integer; TurnOn : Boolean; OUT OK : Boolean);
{ Set a numbered function on or off }
VAR
  DebugStr : String;
  FunctionWasOn, FunctionWasOff : Boolean;
  IDByte, SpeedByte : Byte;
  TestByte1, TestByte2 : Byte;
  TurnOff : Boolean;
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  TestByte1 := 0;
  TestByte2 := 0;
  IDByte := 0;

  OK := True;
  FunctionWasOn := False;
  FunctionWasOff := False;
  TurnOff := NOT TurnOn;

  WITH Loco DO BEGIN
    IF Loco_ControlState <> ControlledByProgram THEN
      ReadInLocoDetails(Loco, SpeedByte, OK);

    { Now can use stored function bytes whether read in just now or previously }
    CASE FunctionNum OF
      0, 1, 2, 3, 4:
       TestByte2 := Loco_Functions0To4Byte;
      5, 6, 7, 8, 9, 10, 11, 12:
       TestByte2 := Loco_Functions5To12Byte;
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

    IF FunctionWasOn AND TurnOn THEN
      DebugStr := 'Setting function ' + IntToStr(FunctionNum) + ' unnecessary: it is already on'
    ELSE
      IF FunctionWasOff AND TurnOff THEN
        DebugStr := 'Setting function ' + IntToStr(FunctionNum) + ' unnecessary: it is already off'
      ELSE BEGIN
       { if the bit we're testing is set it's in the state we expect - now swap it }
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
        WriteArray[2] := GetLocoChipHighByte(Loco_LocoChip);
        WriteArray[3] := GetLocoChipLowByte(Loco_LocoChip);
        WriteArray[4] := TestByte2;

        DataIO('L', WriteArray, LocoAcknowledgment, OK);

        IF OK THEN BEGIN
          SetLocoControlledByState(Loco, ControlledByProgram);
          CASE FunctionNum OF
            0, 1, 2, 3, 4:
              Loco_Functions0To4Byte := TestByte2;
            5, 6, 7, 8, 9, 10, 11, 12:
              Loco_Functions5To12Byte := TestByte2;
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
    Log(Loco_LocoChipStr + ' L ' + DebugStr + ' {BLANKLINEBEFORE}');
  END; {WITH}
END; { SetSingleLocoFunction }

PROCEDURE WriteSignalData(LocoChipStr : String; SignalNum : Integer; DecoderNum : Integer; DataByte : Byte; DecoderNumString, AspectString : String; VAR OK : Boolean);
{ Write out the data for signal or route indicator changes }
VAR
  DebugStr : String;
  IDByte : Byte;
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  IDByte := 33; { for functions 5 - 8 }
  Log(LocoChipStr + ' S Setting S=' + IntToStr(SignalNum) + ' (' + DecoderNumString + ')' + ' to ' + AspectString + ' {BLANKLINEBEFORE}');

  { Now write the data out }
  WriteArray[0] := 228;
  WriteArray[1] := IDByte;
  WriteArray[2] := GetLocoChipHighByte(DecoderNum);
  WriteArray[3] := GetLocoChipLowByte(DecoderNum);
  WriteArray[4] := DataByte;

  DataIO('S', WriteArray, SignalAcknowledgment, OK);

  DebugStr := 'S=' + IntToStr(SignalNum) + ' (' + DecoderNumString + '): ' + AspectString;
  IF OK THEN BEGIN
    FunctionDecoderBytes[DecoderNum - FunctionDecoderArrayOffset] := DataByte;
    DebugStr := DebugStr + ' ok';
  END ELSE
    DebugStr := DebugStr + ' failed in change';
  Log(LocoChipStr + ' S ' + DebugStr);
END; { WriteSignalData }

PROCEDURE ObtainSystemStatus(VAR SystemStatus : LenzSystemRec; OUT TimedOut : Boolean; StartStopTimer : Boolean);
{ Read in the present system status }
CONST
  CheckTimeOuts = True;

VAR
  OK : Boolean;
  ReadArray : ARRAY [0..ReadArrayLen] OF Byte;
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  Log('A Requesting system status');
  WriteArray[0] := 33;
  WriteArray[1] := 36;
  DataIO('A', WriteArray, ReadArray, SystemStatusReply, TimedOut, OK);

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
      Log('XG Initial system: status emergency off');
      IF NOT ShowEmergencyOffMessageVisible THEN BEGIN
        MakeSound(1);
        ShowEmergencyOffMessageVisible := True;
        IF MessageDialogueWithDefault('Initial system status: emergency off - do you wish to try to reset it?',
                                      StartStopTimer, mtError, [mbYes, mbNo], mbNo) = mrYes
        THEN BEGIN
          ResumeOperations(OK);
          IF OK THEN
            Log('AG Operations resumed')
          ELSE
            Log('AG Operations not resumed');
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
            Log('AG Operations resumed')
          ELSE
            Log('AG Operations not resumed');
        END;
      END;
    END ELSE
      EmergencyStop := False;
  END; {WITH}
END; { ObtainSystemStatus }

PROCEDURE TLenzWindow.OnLenzWatchdogTimerInterval(Sender: TObject);
CONST
  StopTimer = True;
  WarnUser = True;

VAR
  TimedOut : Boolean;

BEGIN
  IF SystemOnline THEN BEGIN
    Log('A Watchdog timer: requesting system status at ' + DescribeActualDateAndTime + ' {BLANKLINEBEFORE}');
    ObtainSystemStatus(SystemStatus, Timedout, NOT StopTimer);
    IF Timedout THEN
      Log('X Watchdog timer failed to obtain a response at ' + DescribeActualDateAndTime);

    Inc(WatchdogTimerCount);

    IF WatchdogTimerCount > 5 THEN
      { five minutes of inaction - only check every ten minutes now }
      LenzWatchDogTimer.Interval := 600000;
  END;
END; { OnLenzWatchdogTimerInterval }

PROCEDURE TLenzWindow.OnLenzOneSecondTimerTick(Sender: TObject);
{ This is used to stop point- and semaphore-signal motors burning out }
VAR
  P : Integer;
  OK : Boolean;

BEGIN
  IF OneTimeCodeBeingExecuted THEN    { **** use semaphores - not thread safe - DJW }
    Exit
  ELSE
    OneTimeCodeBeingExecuted := True;

  IF NOT ProgramStarting AND SystemOnline THEN BEGIN
    FOR P := 0 TO High(Points) DO BEGIN
      IF Points[P].Point_Energised THEN BEGIN
        IF MilliSecondsBetween(Time, Points[P].Point_EnergisedTime) > 2 THEN BEGIN
          { an error - it's been energised too long }
          Log('PG Point ' + IntToStr(P) + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum) + ']'
                  + ' has been energised for too long (' + IntToStr(MilliSecondsBetween(Time, Points[P].Point_EnergisedTime)) + 'ms)'
                  + ' so emergency deselection attempted at ' + TimeToHMSZStr(Time));
          IF NOT SystemOnline THEN
            Exit;

          EmergencyDeselectPoint(P, OK);

          IF OK THEN BEGIN
            Points[P].Point_Energised := False;
            Log('PG Point ' + IntToStr(P) + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum) + '] : emergency deselection was successful at ' + TimeToHMSZStr(Time)
                    + ' after ' + IntToStr(MilliSecondsBetween(Time, Points[P].Point_EnergisedTime)) + ' seconds');
          END ELSE
            Log('PG Point ' + IntToStr(P) + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum) + ']' + ': emergency deselection failed at ' + TimeToHMSZStr(Time));
        END;
      END;
    END;
  END;

  OneTimeCodeBeingExecuted := False;
END; { OnLenzOneSecondTimerTick }

PROCEDURE SetSignalFunction(LocoChipStr : String; S : Integer);
{ Set a numbered function on or off - used for LED signals controlled by LF100XF function only decoders, which are programmed to use functions 5-8 }
VAR
  AspectByte, DataByte : Byte;
  AspectString, DecoderNumString : String;
  OK : Boolean;

BEGIN
  TRY
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
            DataByte := FunctionDecoderBytes[Signal_DecoderNum - FunctionDecoderArrayOffset] AND NOT 7;
            DataByte := DataByte OR AspectByte;
            WriteSignalData(LocoChipStr, S, Signal_DecoderNum, DataByte, DecoderNumString, AspectString, OK)
          END ELSE
            IF Signal_Type = FourAspect THEN BEGIN
              { reset lowest four bits }
              IF (Signal_DecoderNum >= FirstFunctionDecoder) AND (Signal_DecoderNum <= LastFunctionDecoder) THEN BEGIN
                DataByte := FunctionDecoderBytes[Signal_DecoderNum - FunctionDecoderArrayOffset] AND NOT 15;
                DataByte := DataByte OR AspectByte;
                WriteSignalData(LocoChipStr, S, Signal_DecoderNum, DataByte, DecoderNumString, AspectString, OK);
              END ELSE
                Log(LocoChipStr + ' S Function decoder ' + IntToStr(Signal_DecoderNum) + ' specified is outside allowed range');
            END;
        END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG SetSignalFunction: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SetSignalFunction }

PROCEDURE SetSignalRouteFunction(LocoChipStr : String; S: Integer);
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
        DataByte := FunctionDecoderBytes[Signal_IndicatorDecoderNum - FunctionDecoderArrayOffset] OR IndicatorByte;
      END ELSE BEGIN
        AspectString := 'Indicator off';
        { now reset our bit, but preserve other bits, as this decoder may be being used for more than one signal }
        DataByte := FunctionDecoderBytes[Signal_IndicatorDecoderNum - FunctionDecoderArrayOffset] AND NOT IndicatorByte;
      END;
      WriteSignalData(LocoChipStr, S, Signal_IndicatorDecoderNum, DataByte, IndicatorDecoderNumString, AspectString, OK);
    END;
  END; {WITH}
END; { SetSignalRouteFunction }

PROCEDURE EmergencyDeselectPoint(P : Integer; VAR OK : Boolean);
{ for use in emergency, when a point remains energised }
VAR
  DecoderUnitNum : Byte;
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  WITH Points[P] DO BEGIN
    DecoderUnitNum := (Point_LenzNum - 1) DIV 4;

    WriteArray[0] := 82;
    WriteArray[1] := DecoderUnitNum;

    { set bit 3 off to deselect }
    WriteArray[2] := 128; {1000 0000}
    Log('E Writing out data to EMERGENCY DESELECT ' + 'P=' + IntToStr(P) + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum - 1)  + ']'
           + ' at ' + TimeToHMSZStr(Time)
           + ' {BLANKLINEBEFORE}');
    DataIO('P', WriteArray, Acknowledgment, OK);
  END; {WITH}
END; { EmergencyDeselectPoint }

PROCEDURE EmergencyDeselectSignal(S : Integer; VAR OK : Boolean);
{ for use in emergency, when a signal remains energised }
VAR
  DecoderUnitNum : Byte;
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  WITH Signals[S] DO BEGIN
    DecoderUnitNum := (Signal_AccessoryAddress - 1) DIV 4;

    WriteArray[0] := 82;
    WriteArray[1] := DecoderUnitNum;

    { set bit 3 off to deselect }
    WriteArray[2] := 128; {1000 0000}
    Log('E Writing out data to EMERGENCY DESELECT ' + 'S=' + IntToStr(S) + ' [accessoryaddress=' + IntToStr(Signals[S].Signal_AccessoryAddress)
           + '] at ' + TimeToHMSZStr(Time)
           + ' {BLANKLINEBEFORE}');
    DataIO('E', WriteArray, Acknowledgment, OK);
  END; {WITH}
END; { EmergencyDeselectSignal }

PROCEDURE MakeSemaphoreSignalChange(LocoChipStr : String; S, AccessoryAddress : Integer; OnOrOff : SignalStateType; OUT OK : Boolean);
{ Pull a semaphore signal or or off using TrainTech's SC3, which "learns" given an accessory address by the LH100 }
CONST
  ProcessMessages = True;

VAR
  B : Byte;
  DebugStr : String;
  DecoderUnitNum, DecoderOutputNum : Byte;
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

  PROCEDURE ActivateSignal;
  BEGIN
    DecoderUnitNum := (AccessoryAddress - 1) DIV 4;
    DecoderOutputNum := ((AccessoryAddress - 1) MOD 4) + 1;

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
    DebugStr := DebugStr +  ' {BLANKLINEBEFORE}';

    { Bit 0 is the direction }
    IF OnOrOff = SignalOn THEN
      { Straight doesn't need setting so can stay 0 }
      B := B OR 1;

    WriteArray[2] := B;

    { Now start the burn out check }
    Signals[S].Signal_Energised := True;
    { and store the current time }
    Signals[S].Signal_EnergisedTime := Time;

    DebugStr := DebugStr + ' at ' + TimeToHMSZStr(Time);
    Log(LocoChipStr + ' S ' + DebugStr);

    DataIO('S', WriteArray, Acknowledgment, OK);
  END; { ActivateSignal}

  PROCEDURE DeactivateSignal;
  BEGIN
    DecoderUnitNum := (AccessoryAddress - 1) DIV 4;

    WriteArray[0] := 82;
    WriteArray[1] := DecoderUnitNum;

    { set bit 3 off to deselect }
    WriteArray[2] := 128; {1000 0000}
    Log('S Writing out data to deselect semaphore S=' + IntToStr(S) + ' (accessory address ' + IntToStr(AccessoryAddress)
           + '] at ' + TimeToHMSZStr(Time) + '  {BLANKLINEBEFORE}');
    DataIO('S', WriteArray, Acknowledgment, OK);

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
  Pause(50, NOT ProcessMessages);  { ********** }

  DeactivateSignal;
END; { MakeSemaphoreSignalChange }

PROCEDURE TurnPointOff(P : Integer; VAR OK : Boolean);
{ Deselect a given point }
VAR
  DecoderUnitNum : Byte;
  InterfaceLenzPoint : Integer;
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  InterfaceLenzPoint := Points[P].Point_LenzNum - 1;
  DecoderUnitNum := InterfaceLenzPoint DIV 4;

  WriteArray[0] := 82;
  WriteArray[1] := DecoderUnitNum;

  { set bit 3 off to deselect }
  WriteArray[2] := 128; {1000 0000}
  Log('P Writing out data to deselect P=' + IntToStr(P) + ' [Lenz' + IntToStr(Points[P].Point_LenzNum) + '] at ' + TimeToHMSZStr(Time) + ' {BLANKLINEBEFORE}');
  DataIO('P', WriteArray, PointAcknowledgment, OK);

  { Now turn off the burn-out checking }
  IF OK THEN
    Points[P].Point_Energised := False;
  Log('P P=' + IntToStr(P) + ' deselected after ' + IntToStr(MilliSecondsBetween(Time, Points[P].Point_EnergisedTime)) + ' ms');
END; { TurnPointOff }

FUNCTION MakePointChange(LocoChipStr : String; P : Integer; Direction : PointStateType; VAR Count : Integer) : Boolean;
{ Select which decoder, which output and which direction (LS100/100 decoders have four outputs. Need to select then deselect after 200 ms!) }
CONST
  ProcessMessages = True;

VAR
  B : Byte;
  DebugStr : String;
  DecoderUnitNum, DecoderOutputNum : Byte;
  InterfaceLenzPoint : Integer;
  OK : Boolean;
  SavedTime : TDateTime;
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

  PROCEDURE TurnPointOn(LocoChipStr : String; VAR OK : Boolean);
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

    { If the wiring is reversed, the point is switching the opposite way to the way we think it is }
    IF Points[P].Point_WiringReversedFlag THEN BEGIN
      IF Direction = Straight THEN BEGIN
        Direction := Diverging;
        DebugStr := DebugStr + ' (wiring is reversed so actually requesting a change to diverging)';
      END ELSE BEGIN
        Direction := Straight;
        DebugStr := DebugStr + ' (wiring is reversed so actually requesting a change to straight)';
      END;
    END;

    DebugStr := DebugStr + ' at ' + TimeToHMSZStr(Time);

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

    Log(LocoChipStr + ' P ' + DebugStr + ' {BLANKLINEBEFORE}');

    DataIO('P', WriteArray, PointAcknowledgment, OK);
  END; { TurnPointOn }

  PROCEDURE TurnPointOff(LocoChipStr : String; VAR OK : Boolean);
  BEGIN
    WriteArray[0] := 82;
    WriteArray[1] := DecoderUnitNum;

    { set bit 3 off to deselect }
    WriteArray[2] := 128; {1000 0000}
    Log(LocoChipStr + ' P Writing out data (' + IntToStr(Count) + ') to deselect P=' + IntToStr(P)
                        + ' [Lenz=' + IntToStr(Points[P].Point_LenzNum) + '] at ' + TimeToHMSZStr(Time) + ' {BLANKLINEBEFORE}');
    DataIO('P', WriteArray, PointAcknowledgment, OK);

    { Now turn off the burn-out checking }
    IF OK THEN
      Points[P].Point_Energised := False;
    Log(LocoChipStr + ' P P=' + IntToStr(P) + ' deselected after ' + IntToStr(MilliSecondsBetween(Time, Points[P].Point_EnergisedTime)) + ' ms');
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
  TurnPointOn(LocoChipStr, OK);

  SavedTime := Time;
  IF OK THEN BEGIN
    WHILE MilliSecondsBetween(Time, SavedTime) < 100 DO BEGIN
      DoCheckForUnexpectedData(UnitRef, 'MakePointChange 1');
      IF InAutoMode THEN
        MoveAllTrains;
//      Pause(10, NOT ProcessMessages);
    END; {WHILE}

    OK := False;
    REPEAT
      TurnPointOff(LocoChipStr, OK);
      DoCheckForUnexpectedData(UnitRef, 'MakePointChange 2');
      IF InAutoMode THEN
        MoveAllTrains;
    UNTIL OK OR NOT SystemOnline;
  END;
  Result := OK;
END; { MakePointChange }

PROCEDURE SetUpDoubleHeader(Loco1, Loco2 : LocoRec; VAR OK : Boolean);
{ Sets up a double header - needs both locos standing still, both have already been controlled, and were the most recent ones controlled by the computer. This procedure is
  not used by the rail program as the program deals with double heading by sending the speed commands to both named locos.
}
VAR
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  WITH Loco1 DO BEGIN
    Log(Loco_LocoChipStr + ' L Requesting double header for locos ' + Loco_LocoChipStr + ' +' + Loco2.Loco_LocoChipStr + ' {BLANKLINEBEFORE}');
    WriteArray[0] := 229;
    WriteArray[1] := 67;
    WriteArray[2] := GetLocoChipHighByte(Loco_LocoChip);
    WriteArray[3] := GetLocoChipLowByte(Loco_LocoChip);
    WriteArray[4] := GetLocoChipHighByte(Loco2.Loco_LocoChip);
    WriteArray[5] := GetLocoChipLowByte(Loco2.Loco_LocoChip);

    DataIO('L', WriteArray, Acknowledgment, OK);
    IF OK THEN BEGIN
      SetLocoControlledByState(Loco1, ControlledByProgram);
      SetLocoControlledByState(Loco2, ControlledByProgram);
      Log(LocoChipToStr(Loco_LocoChip) + ' L Double header (locos ' + IntToStr(Loco_LocoChip) + ' +' + Loco2.Loco_LocoChipStr + ') set up')
    END ELSE
      Log(LocoChipToStr(Loco_LocoChip) + ' L Double header (locos ' + IntToStr(Loco_LocoChip) + ' +' + Loco2.Loco_LocoChipStr + ') not set up');
  END;
END; { SetUpDoubleHeader }

PROCEDURE DissolveDoubleHeader(Loco : LocoRec; VAR OK : Boolean);
{ Dissolves a double header - needs both locos standing still. This procedure is not used by the rail program as the program deals with double heading by sending the speed
  commands to both named locos.
}
VAR
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  WITH Loco DO BEGIN
    Log(Loco_LocoChipStr + ' L Requesting double header for ' + IntToStr(Loco_LocoChip) + ' be dissolved {BLANKLINEBEFORE}');
    WriteArray[0] := 229;
    WriteArray[1] := 67;
    WriteArray[2] := GetLocoChipHighByte(Loco_LocoChip);
    WriteArray[3] := GetLocoChipLowByte(Loco_LocoChip);
    WriteArray[4] := 0;
    WriteArray[5] := 0;

    DataIO('L', WriteArray, Acknowledgment, OK);

    IF OK THEN
      Log(Loco_LocoChipStr + ' L Double header (including loco ' + IntToStr(Loco_LocoChip) + ') dissolved')
    ELSE
      Log(Loco_LocoChipStr + ' L Double header (including loco ' + IntToStr(Loco_LocoChip) + ') not dissolved');
  END; {WITH}
END; { DissolveDoubleHeader }

{$O+}

PROCEDURE StopOperations(OUT OK : Boolean);
{ Turns the power off to the track and to I/O devices; it tells the system to stop sending DCC packets to the track and to switch off the DCC track power. }
VAR
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  Log('E Requesting stop operations {BLANKLINEBEFORE}');
  WriteArray[0] := 33;
  WriteArray[1] := 128;

  DataIO('E', WriteArray, TrackPowerOffReply, OK);
END; { StopOperations }

PROCEDURE ResumeOperations(OUT OK : Boolean);
{ Turns the power back on }
VAR
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  Log('E Requesting resume operations {BLANKLINEBEFORE}');
  WriteArray[0] := 33;
  WriteArray[1] := 129;

  DataIO('E', WriteArray, EverythingTurnedOnReply, OK);
END; { ResumeOperations }

PROCEDURE StopAllLocomotives(VAR OK : Boolean);
{ Emergency stops all trains, but leaves the power on }
VAR
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  Log('L Requesting stop all locomotives {BLANKLINEBEFORE}');
  WriteArray[0] := 128;

  DataIO('L', WriteArray, EmergencyStopReply, OK);
END; { StopAllLocomotives }

PROCEDURE StopAParticularLocomotive(VAR Loco : LocoRec; VAR OK : Boolean);
{ Stops a particular loco }
VAR
  WriteArray : ARRAY [0..ReadArrayLen] OF Byte;

BEGIN
  WITH Loco DO BEGIN
    Log(Loco_LocoChipStr + ' L Requesting stop loco {BLANKLINEBEFORE}');
    WriteArray[0] := 146;
    WriteArray[1] := GetLocoChipHighByte(Loco_LocoChip);
    WriteArray[2] := GetLocoChipLowByte(Loco_LocoChip);

    DataIO('L', WriteArray, Acknowledgment, OK);
    IF OK THEN
      SetLocoControlledByState(Loco, ControlledByProgram);
  END; {WITH}
END; { StopAParticularLocomotive }

FUNCTION ReturnSystemStatus : LenzSystemRec;
{ Return the present system status }
BEGIN
  Result := SystemStatus;
END; { ReturnSystemStatus }

PROCEDURE WhichFeedbackInputsAreSet(UnitNum, B : Byte);
{ Looks at bits 4-0 to find out, and calls DecodeFeedback to set the on-screen tell-tales }
VAR
  I, Input : Integer;

BEGIN
  { If bit 4 set, inputs are 5 to 8, so increment counter accordingly }
  IF (B AND 16) <> 16 THEN
    Input := 0
  ELSE
    Input := 4;

  { Cycle through the four inputs }
  FOR I := 1 TO 4 DO BEGIN
    IF (B AND BinaryCounter[I]) = BinaryCounter[I] THEN BEGIN
      FeedbackUnitRecords[UnitNum].Feedback_InputOnArray[Input + I] := True;
      DecodeFeedback(UnitNum, Input + I);
    END ELSE BEGIN
      FeedbackUnitRecords[UnitNum].Feedback_InputOnArray[Input + I] := False;
      DecodeFeedback(UnitNum, Input + I);
    END;
  END;
END; { WhichFeedbackInputsAreSet }

PROCEDURE GetInitialFeedback(OUT OK : Boolean);
{ Read in the feedback before we start, or after an interruption }

  PROCEDURE ReadInFeedbackData(TempUnit : Integer; OUT OK : Boolean);
  { Ask for feedback on specified device and store it. Usually only called on startup.

    Feedback comes in two nibbles - have to ask for both to get all the inputs, which comes in bits 3-0 (in nibble 1, bit 0 is input 1, bit 1 is input 2, etc. In nibble 2,
    bit 0 is input 5, bit 1 is input 6, etc.) The FeedbackAddress is one less than the one shown on the LH100 and the one programmed into the device.
  }
  VAR
    I : Integer;
    ReadArray : ARRAY [0..ReadArrayLen] OF Byte;
    WriteArray : ARRAY [0..ReadArrayLen] OF Byte;
    XpressNetUnit : Byte;

  BEGIN
    XpressNetUnit := TempUnit - 1;

    FOR I := 128 TO 129 DO BEGIN
      WriteArray[0] := 66;
      WriteArray[1] := XpressNetUnit;
      WriteArray[2] := I;

      IF NOT SystemOnline THEN
        Exit
      ELSE BEGIN
        REPEAT
          DataIO('T', WriteArray, ReadArray, FeedbackReply, OK);

          { Check it's for the unit we specified, in case some unrequested feedback data comes in while we're starting up }
          IF OK AND (ReadArray[1] <> XpressNetUnit) THEN
            Log('TG Feedback for ' + IntToStr(ReadArray[1] + 1) + ' arrived when feedback for ' + IntToStr(TempUnit) + ' expected');
        UNTIL (OK AND (ReadArray[1] = XpressNetUnit))
               OR NOT SystemOnline;

        IF NOT SystemOnline THEN
          OK := False;
      END;

      IF OK THEN BEGIN
        IF ReadArray[2] = 0 THEN BEGIN
          { there's a problem - keep the user informed }
          OK := False;
          Log('T The feedback from unit ' + IntToStr(TempUnit) + ' has no data in it');
        END ELSE BEGIN
          WhichFeedbackInputsAreSet(TempUnit, ReadArray[2]);

          { and store the data in the global feedback-data array for future use - need to store in two halves }
          IF I = 128 THEN
            { inputs 1 to 4 }
            FeedbackArray[TempUnit, 0] := ReadArray[2]
          ELSE
            { inputs 5 to 8 }
            FeedbackArray[TempUnit, 1] := ReadArray[2]
        END;
      END;
    END;
  END; { ReadInFeedbackData }

CONST
  ProcessMessages = True;

VAR
  NoFeedbackList : String;
  UnitNum : Integer;

BEGIN
  IF SystemOnline THEN BEGIN
    FOR UnitNum := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
      Log('T Requesting feedback data for unit ' + IntToStr(UnitNum) + ' {BLANKLINEBEFORE}');
      ReadInFeedbackData(UnitNum, OK);
      IF NOT OK THEN BEGIN
        Log('T Re-requesting feedback data for unit ' + IntToStr(UnitNum) + ' {BLANKLINEBEFORE}');
        ReadInFeedbackData(UnitNum, OK);
        IF NOT OK THEN BEGIN
          Log('T No feedback from unit ' + IntToStr(UnitNum));
          FeedbackUnitRecords[UnitNum].Feedback_DetectorOutOfUse := True;
        END;
      END;
    END; {FOR}
  END;

  IF SystemOnline THEN BEGIN
    { Keep the user informed as to which feedback units are out of use, if any }
    NoFeedbackList := '';
    FOR UnitNum := FirstFeedbackUnit TO LastFeedbackUnit DO
      IF FeedbackUnitRecords[UnitNum].Feedback_DetectorOutOfUse THEN
        NoFeedbackList := NoFeedbackList + IfThen(NoFeedbackList <> '', ', ') + IntToStr(UnitNum);

    IF NoFeedbackList <> '' THEN
      Log('XG The following feedback units are out of use: ' + NoFeedbackList);
  END;
END; { GetInitialFeedback }

PROCEDURE SetSystemOffline(OfflineMsg : String; Warning : Boolean);
{ Change the caption and the icons to show we're offline }
BEGIN
  LenzWindow.LenzOneSecondTimerTick.Enabled := False;

  SystemOnline := False;
  SetCaption(FWPRailWindow, 'OFFLINE');
  Application.Icon := OffLineIcon;
  IF OfflineMsg <> '' THEN BEGIN
    IF FeedbackWindow <> NIL THEN BEGIN
      WriteStringToFeedbackWindow(OfflineMsg);
      FeedbackWindow.FeedbackWindowTimer.Enabled := True;
    END;

    IF warning THEN
      Log('X! System set offline: ' + OfflineMsg)
    ELSE
      Log('XG System set offline: ' + OfflineMsg);
  END;

  IF LenzConnection = USBConnection THEN
    StopLANUSBServer;
END; { SetSystemOffline }

FUNCTION SetSystemOnline : Boolean;
{ Change the caption and the icons to show we're online - needs a test to see if we are, actually, online *************** 6/2/14 }
VAR
//  LockingMsg : String;
  OK : Boolean;
  P : Integer;
  S : Integer;
  T : TrainIndex;

BEGIN
  Result := False;

  TRY
    { Create the TCPIP form here so we know it is available before we start using it }
    IF TCPIPForm = NIL THEN BEGIN
      TCPIPForm := TTCPIPForm.Create(Application);
      TCPIPForm.Update;
    END;

    LenzConnection := NoConnection;

    CASE DesiredLenzConnection OF
      EthernetConnection:
        BEGIN
          Log('X! Checking Ethernet connection');
          { See if the Ethernet connection is up and running }
          TCPIPForm.TCPIPFormShow(LenzWindow);
          TCPIPForm.CreateTCPClients(EthernetConnection);
          LenzConnection := EthernetConnection;
        END;
      USBConnection:
        BEGIN
          Log('X! Checking USB connection');
          { First see if the Lenz server program is running via the USB Connection. (If it's connected, it's assumed that we wish to try it first). }
          IF IsProgramRunning('LI-Server') THEN
            { The LI-Server.exe program is already running - better kill it, as we can't programmaticaly start the server itself }
            StopLANUSBServer;

          StartLANUSBServer;
          IF IsProgramRunning('LI-Server') THEN BEGIN
            Log('X& LI-Server.exe is running');

            TCPIPForm.TCPIPFormShow(LenzWindow);
            TCPIPForm.CreateTCPClients(USBConnection);
            LenzConnection := USBConnection;
          END;
        END;
    END; {CASE}

    IF LenzConnection = NoConnection THEN
      SetSystemOffline('System offline as no connection to the Lenz system {BLANKLINEBEFORE}', SoundWarning)
    ELSE BEGIN
      SystemOnline := True;
      SetCaption(FWPRailWindow, '');
      Application.Icon := OnlineIcon;
    END;

    FOR P := 0 TO High(Points) DO
      IF Points[P].Point_PresentState <> PointStateUnknown THEN
        Points[P].Point_RequiredState := Points[P].Point_PresentState;

    GetInitialFeedback(OK);
    CheckOccupiedLinesAndDiagrams;

    IF OK THEN BEGIN
      SystemOnline := True;
      Result := LenzConnection <> NoConnection;
    END;

    IF SystemOnline THEN BEGIN
      { Read in all the feedback }
      { Update real signals to match virtual signals in case they were changed offline }
      S := 0;
      WHILE S <= High(Signals) DO BEGIN
        SetSignal(UnknownLocoChipStr, S, Signals[S].Signal_Aspect, True, ForceAWrite);
        Inc(S);
      END; {WHILE}
    END;

    { Ditto for points }   { &&&&& }
//    IF SystemOnline THEN BEGIN
//      Log('P Initially resetting all points that are not at their default state');
//      FOR P := 0 TO High(Points) DO BEGIN
//        IF NOT Points[P].Point_OutOfUse THEN BEGIN
//          IF PointIsLocked(P, LockingMsg) THEN
//            Log('P Not initially resetting P=' + PointToStr(P) + ' as point is ' + LockingMsg)
//          ELSE
//            PullPoint(P, NOT ForcePoint);
//        END;
//      END; {FOR}
//    END;

    IF NOT SystemOnline THEN
      Log('X& Not connected to Lenz system')
    ELSE BEGIN
      Log('X& System now connected via ' + LenzConnectionToStr(LenzConnection));
      LenzWindow.LenzOneSecondTimerTick.Enabled := True;

      SetUpAllLocationOccupationsAbInitio(False, OK);
      { and recalculate the journey times }
      T := 0;
      WHILE T <= High(Trains) DO BEGIN
        WITH Trains[T] DO BEGIN
          IF (Train_LocoChip <> UnknownLocoChip) AND Train_DiagramFound AND (Train_CurrentStatus <> Cancelled) AND (Train_CurrentStatus <> NonMoving) THEN
            RecalculateJourneyTimes(T, 'as the system is now online');

          Inc(T);
        END; {WITH}
      END; {WHILE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG SetSystemOnline:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetSystemOnline }

PROCEDURE InitialiseLenzUnit;
{ Such routines as this allow us to initialises the units in the order we wish }
CONST
  StopTimer = True;
  WarnUser = True;

BEGIN
  { For initial data flow check }
  IF NOT SystemSetOfflineByCommandLineParameter THEN BEGIN
    SetSystemOnline;

//      { provisionally... }
//
//      { but now check the true state of affairs }
//      ObtainSystemStatus(SystemStatus, StopTimer);
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

  IF SystemOnline THEN
    { Store settings of feedback units before starting }
    ReadCommandStationSoftwareVersion;

  IF SystemOnline THEN
    ReadComputerInterfaceSoftwareVersion;

//  FOR I := 0 TO (ReadArrayLen - 1) DO BEGIN
//    ReadArray[I] := 0;
//    WriteArray[I] := 0;
//  END;

  Log('A Lenz unit initialised');
END; { InitialiseLenzUnit }

INITIALIZATION

END { Lenz }.
