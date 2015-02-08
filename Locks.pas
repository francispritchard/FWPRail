UNIT Locks;
{ This unit controls the interlocking for points, signals etc. Also has notes on wiring Eckon Signals and LF decoders

  Wiring Eckon Signals and LF decoders: some notes

  Join the signal's red, green and yellow wires - they are all positive and must be common. Separate the black positive wire for upper yellow aspect, and also keep separate
  the black and blue from the route indicator if any.

  Blue is the common positive output from the LF100 and goes to the signal's positive red, green, yellow and black wires.

  The other individual negative outputs of the LF100 go to the three white negative feeds to the colours, and to the blue feed to the double yellow/route indicator.

  To connect to LF100s, need two resistors if 4 aspect. Feed red, green and yellow wires through one, and black through the other.

  Set LF100's CVs as follows: 51 to 1, 39 to 32 (6) [A=F5], 40 to 64 (7) [B=F6], 54 to 64 (7) [C=F7], 55 to 128 (8) [D=F8]


  The interlocking implements the following:
  Distant signals:
    The distant signal can only be clear if all signals following it are clear.
  Sequential locking:
    A signal cannot be cleared unless the next signal is at danger.
  Flank protection:
    A line cannot be cleared if a crossover point is pointing at it, so that an overrun on the crossover could give a collision.
  Points:
    A route can only be cleared if the points are set for it, and the points cannot be changed once a route is set over them.

  [ the following two are not implemented in the Garage code: ]
  Blocks:
    The signal allowing a train into the next block cannot be cleared until the next signalman clears his block instrument, and then it can only be pulled once.
  Welwyn control:
    Another train cannot be accepted until a train has occupied and cleared the outer home bay and the outer home is at danger.
}

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StartUp, Diagrams, GetTime, InitVars, StdCtrls, FWPShowMessageUnit, PointsUnit,
     LinesUnit;

TYPE
  TLockListWindow = CLASS(TForm)
    LockListWindowMemo: TMemo;
    PROCEDURE LockListWindowMemoKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  LockListWindow: TLockListWindow;

PROCEDURE Forbid;
{ Complains if user tries something stupid - no need if in auto mode }

PROCEDURE InitialiseLocksUnit;
{ Initialise the unit }

PROCEDURE UnlockTrackCircuitRouteLocking(TC : Integer);
{ Unlock a given track circuit }

PROCEDURE WriteLockingDataToLockListWindow;
{ Writes locking data to a popup screen }

PROCEDURE WriteRouteInfoToLockListWindow;
{ Writes routeing data to a popup screen }

VAR
  FindARouteFailMsgWritten : Boolean = False;

{$R *.dfm}

IMPLEMENTATION

USES RailDraw, MiscUtils, CreateRoute, Route, Lenz, StrUtils, DateUtils, Input, Options, Main, SignalsUnit, TrackCircuitsUnit;

VAR
  UnitRef : String = 'Locks';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE Forbid;
{ Complains if user tries something stupid - no need if in auto mode }
BEGIN
  IF NOT InAutoMode THEN
    MakeSound(1);
END; { Forbid }

PROCEDURE UnlockTrackCircuitRouteLocking(TC : Integer);
{ Unlock a given track circuit }
BEGIN
  IF TC <> UnknownTrackCircuit THEN BEGIN
    WITH TrackCircuits[TC] DO BEGIN
      IF TC_Journey <> UnknownJourney THEN BEGIN
        Log(LocoChipToStr(TC_LocoChip) + ' R TC=' + IntToStr(TC) + ': TC_Journey was ' + IntToStr(TC_Journey) + ' now set to unknown journey');
        TrackCircuits[TC].TC_Journey := UnknownJourney;
      END;

      IF TC_LockedForRoute <> UnknownRoute THEN BEGIN
        TC_SaveRouteLocking := UnknownRoute;
        Log(LocoChipToStr(TC_LocoChip) + ' R TC=' + IntToStr(TC) + ': TC_LockedForRoute was R=' + IntToStr(TC_LockedForRoute)
                            + ' (' + LocoChipToStr(Routes_LocoChips[TrackCircuits[TC].TC_LockedForRoute]) + ')' + ' now reset to unknown route');
        TC_LockedForRoute := UnknownRoute;
      END;
    END; {WITH}
  END;
END; { UnlockTrackCircuitRouteLocking }

PROCEDURE WriteLockingDataToLockListWindow;
{ Writes locking data to a popup screen }
VAR
  FoundSomething : Boolean;
  I : Integer;
  L : Integer;
  LockingString : String;

BEGIN
  LockListWindow.Caption := 'Locking Data';
  LockListWindow.LockListWindowMemo.Clear;
  LockListWindow.Visible := True;
  Log('A Lock list displayed');
  FoundSomething := False;

  FOR I := 0 TO High(Points) DO BEGIN
    IF PointIsLocked(I, LockingString) THEN BEGIN
      LockListWindow.LockListWindowMemo.Lines.Add('P=' + IntToStr(I) + ' ' + LockingString);
      FoundSomething := True;
    END;
  END;

  FOR I := 0 TO High(Signals) DO BEGIN
    IF NOT Signals[I].Signal_OutOfUse THEN BEGIN
      IF SignalIsLocked(I, LockingString) THEN BEGIN
        LockListWindow.LockListWindowMemo.Lines.Add('S=' + IntToStr(I) + ' ' + LockingString);
        FoundSomething := True;
      END;
    END;
    IF Signals[I].Signal_ApproachLocked THEN BEGIN
      LockListWindow.LockListWindowMemo.Lines.Add('S=' + IntToStr(I) + ' ' + 'AC locked');
      FoundSomething := True;
    END;
  END;

  FOR I := 0 TO High(TrackCircuits) DO BEGIN
    IF TrackCircuits[I].TC_LockedForRoute <> UnknownRoute THEN BEGIN
      LockListWindow.LockListWindowMemo.Lines.Add('TC=' + IntToStr(I) + ' locked by R=' + IntToStr(TrackCircuits[I].TC_LockedForRoute));
      FoundSomething := True;
    END;
  END;

  FOR L := 0 TO High(Lines) DO BEGIN
    IF Lines[L].Line_RouteLockingForDrawing <> UnknownRoute THEN BEGIN
      LockListWindow.LockListWindowMemo.Lines.Add('L=' + LineToStr(L) + ' locked by R=' + IntToStr(Lines[L].Line_RouteLockingForDrawing));
      FoundSomething := True;
    END;
  END;
  IF NOT FoundSomething THEN
    LockListWindow.LockListWindowMemo.Lines.Add('No locking found');
END; { WriteLockingDataToLockListWindow }

PROCEDURE WriteRouteInfoToLockListWindow;
{ Writes routeing data to a popup screen }
VAR
  DebugStr : String;
  FoundSomething : Boolean;
  I, J, K : Integer;

BEGIN
  LockListWindow.Caption := 'Routeing Info';
  LockListWindow.LockListWindowMemo.Clear;
  LockListWindow.LockListWindowMemo.Lines.Clear;
  LockListWindow.Visible := True;
  Log('A Routeing info displayed');
  FoundSomething := False;
  IF Length(Routes_Routes) > 0 THEN BEGIN
    FoundSomething := True;

    { write out the current route numbers }
    DebugStr := 'Routes_Routes= ';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + IntToStr(Routes_Routes[I]) + ', ';
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
    LockListWindow.LockListWindowMemo.Lines.Add('');

    FOR I := 0 TO High(Routes_Routes) DO BEGIN
      J := Routes_Routes[I];
      FOR K := 0 TO High(Routes_SubRouteStates[J]) DO BEGIN
        DebugStr := 'R=' + IntToStr(J) + '/' + IntToStr(K)
                    + ' J=' + IntToStr(Routes_Journeys[J])
                    + ' ' + SubRouteStateToStr(Routes_SubRouteStates[J, K])
                    + IfThen(Routes_LocoChips[J] <> UnknownLocoChip,
                             ' (' + LocoChipToStr(Routes_LocoChips[J]) + ')',
                             '')
                    + ' [' + DescribeJourneyAndRoute([J, K]) + ']'
                    + IfThen(Routes_SubRouteStates[J, K] = SubRouteSettingUpStalled,
                             Routes_RoutesSettingUpStalledMsgArray[J]);
        LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
      END; {FOR}
    END; {FOR}
    LockListWindow.LockListWindowMemo.Lines.Add('');

    { and the current route arrays }
    FOR I := 0 TO High(Routes_Routes) DO BEGIN
      DebugStr := 'Routes_SubRouteStrings[' + IntToStr(Routes_Routes[I]) + '] ';
      FOR J := 0 TO High(Routes_SubRouteSettingStrings[Routes_Routes[I]]) DO BEGIN
        DebugStr := DebugStr + ' {' + IntToStr(J) + '} ';
        FOR K := 0 TO High(Routes_SubRouteSettingStrings[Routes_Routes[I], J]) DO
          DebugStr := DebugStr + ' ' + Routes_SubRouteSettingStrings[Routes_Routes[I], J, K];
      END;
      LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
    END;

    { and the various start and end route arrays }
    DebugStr := 'Routes_StartSignal';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + ' [' + IntToStr(Routes_Routes[I]) + ']='
                  + IntToStr(Routes_StartSignals[Routes_Routes[I]]);
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    DebugStr := 'Routes_EndSignal';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + ' [' + IntToStr(Routes_Routes[I]) + ']='
                  + IntToStr(Routes_EndSignals[Routes_Routes[I]]);
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    DebugStr := 'Routes_EndBufferStopNum';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + ' [' + IntToStr(Routes_Routes[I]) + ']='
                 + IntToStr(Routes_EndBufferStops[Routes_Routes[I]]);
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
    LockListWindow.LockListWindowMemo.Lines.Add('');

    { and which if any subroutes are set }
    FOR I := 0 TO High(Routes_Routes) DO BEGIN
      DebugStr := 'Routes_WhetherSubRoutesSet[' + IntToStr(Routes_Routes[I]) + ']=';
      FOR J := 0 TO High(Routes_SubRouteStates[Routes_Routes[I]])
      DO
        DebugStr := DebugStr + ' (' + IntToStr(J) + ') ' + SubRouteStateToStr(Routes_SubRouteStates[Routes_Routes[I], J]);
      LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
    END;
    LockListWindow.LockListWindowMemo.Lines.Add('');

    DebugStr := 'Routes_CurrentSubRouteSettingPos ';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + '[' + IntToStr(Routes_Routes[I]) + ']='
                  + IntToStr(Routes_CurrentSubRouteSettingPos[Routes_Routes[I]]) + ' ';
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    DebugStr := 'Routes_CurrentSettingSubRoute ';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + '[' + IntToStr(Routes_Routes[I]) + ']='
                  + IntToStr(Routes_CurrentSettingSubRoute[Routes_Routes[I]]) + ' ';
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    DebugStr := 'Routes_CurrentClearingSubRoute ';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + '[' + IntToStr(Routes_Routes[I]) + ']='
                  + IntToStr(Routes_CurrentClearingSubRoute[Routes_Routes[I]]) + ' ';
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
    { And various variables }
    DebugStr := 'Routes_RouteSettingByHand=' + IfThen(Routes_RouteSettingByHand,
                                                      'yes',
                                                      'no');
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    DebugStr := 'Routes_RouteSettingsInProgress';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + ' [' + IntToStr(Routes_Routes[I]) + ']='
                  + IfThen(Routes_RouteSettingsInProgress[Routes_Routes[I]],
                           'yes',
                           'no');
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    DebugStr := 'Routes_RouteClearingsInProgress';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + ' [' + IntToStr(Routes_Routes[I]) + ']='
                  + IfThen(Routes_RouteClearingsInProgress[Routes_Routes[I]],
                           'yes',
                           'no');
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    DebugStr := 'Routes_SettingUpFailurseMsgWrittenArray';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + ' [' + IntToStr(Routes_Routes[I]) + ']='
                  + IfThen(Routes_SettingUpFailuresMsgWrittenArray[Routes_Routes[I]],
                           'yes',
                           'no');
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    DebugStr := 'Routes_ClearingFailuresMsg1WrittenArray';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + ' [' + IntToStr(Routes_Routes[I]) + ']='
                  + IfThen(Routes_ClearingFailuresMsg1WrittenArray[Routes_Routes[I]],
                           'yes',
                           'no');
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    DebugStr := 'Routes_ClearingFailuresMsg2WrittenArray';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + ' [' + IntToStr(Routes_Routes[I]) + ']='
                  + IfThen(Routes_ClearingFailuresMsg2WrittenArray[Routes_Routes[I]],
                           'yes',
                           'no');
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    DebugStr := 'Routes_RouteCounter=' + IntToStr(Routes_RouteCounter);
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
    LockListWindow.LockListWindowMemo.Lines.Add('');

    DebugStr := 'Routes_PointResultPendingPoint';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + '[' + IntToStr(Routes_Routes[I]) + ']='
                  + IntToStr(Routes_PointResultPendingPoint[Routes_Routes[I]]) + ' ';
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
    LockListWindow.LockListWindowMemo.Lines.Add('');

    DebugStr := 'Routes_ApproachControlSet';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + ' [' + IntToStr(Routes_Routes[I]) + ']='
                  + IfThen(Routes_ApproachControlsSet[Routes_Routes[I]],
                           'yes',
                           'no');
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);

    FOR I := 0 TO High(Routes_Routes) DO BEGIN
      DebugStr := 'Routes_ApproachControlledSignals[' + IntToStr(Routes_Routes[I]) + '] ';
      FOR J := 0 TO High(Routes_ApproachControlledSignals[Routes_Routes[I]]) DO
        DebugStr := DebugStr + ' {' + IntToStr(J) + '} ' + IntToStr(Routes_ApproachControlledSignals[I, J]);
      LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
    END;

    FOR I := 0 TO High(Routes_Routes) DO BEGIN
      DebugStr := 'Routes_ApproachControlSignalsWaitingToBeSet[' + IntToStr(Routes_Routes[I]) + '] ';
      FOR J := 0 TO High(Routes_ApproachControlSignalsWaitingToBeSet[Routes_Routes[I]]) DO
        DebugStr := DebugStr + ' {' + IntToStr(J) + '} ' + Routes_ApproachControlSignalsWaitingToBeSet[I, J];
      LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
    END;

    DebugStr := 'Routes_ApproachControlSignalsMsgWritten';
    FOR I := 0 TO High(Routes_Routes) DO
      DebugStr := DebugStr + ' [' + IntToStr(Routes_Routes[I]) + ']='
                  + IfThen(Routes_ApproachControlSignalsMsgWrittenArray[Routes_Routes[I]],
                           'yes',
                           'no');
    LockListWindow.LockListWindowMemo.Lines.Add(DebugStr);
  END;
  IF NOT FoundSomething THEN
    LockListWindow.LockListWindowMemo.Lines.Add('No routeing info found');
END; { WriteRouteInfoToLockListWindow }

PROCEDURE TLockListWindow.LockListWindowMemoKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      LockListWindow.Hide;

    vk_F11:
      BEGIN
        { This will refresh the window }
        LockListWindow.Hide;
        LockListWindow.Show;
        IF ssShift IN ShiftState THEN
          WriteLockingDataToLockListWindow
        ELSE
          WriteRouteInfoToLockListWindow;
      END;

    { Exclude most non-alphanumeric keys }
    vk_Shift, vk_Control, vk_Menu { Alt }, vk_Pause, vk_SnapShot { PrtSc }, vk_LWin, vk_RWin, vk_Apps { Windows Applications }, vk_Numlock, vk_Scroll,
    vk_Cancel { Ctrl-Break}, vk_Capital { Caps Lock }, vk_Back { Backspace }:
      { do nothing };
  ELSE {CASE}
    KeyPressedDown(Key, ShiftState);
  END; {CASE}
END; { LockListWindowMemOKeyDown }

PROCEDURE InitialiseLocksUnit;
{ Initialise the unit }
BEGIN
  LockListWindow.Height := LockListWindowHeight;
  LockListWindow.Width := LockListWindowWidth;
  LockListWindow.Top := LockListWindowTop;
  LockListWindow.Left := LockListWindowLeft;
END; { InitialiseLocksUnit }

INITIALIZATION

END { Locks }.
