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

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StartUp, Diagrams, GetTime, InitVars, StdCtrls, FWPShowMessageUnit;

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

PROCEDURE LockPointByRoute(LocoChip, P, Route : Integer; DoNotWriteMessage : Boolean);
{ Mark the point as locked by a specific route }

FUNCTION PointIsLocked{2}(P : Integer; OUT LockingMsg : String) : Boolean; Overload;
{ Returns true if the point is locked }

FUNCTION PointIsLockedByASpecificRoute(P, Route : Integer) : Boolean;
{ Returns true if the point is locked by the given route }

FUNCTION PointIsLockedByAnyRoute(P : Integer; OUT RouteLockingArray : IntegerArrayType) : Boolean;
{ Returns true if the point is locked by any route }

PROCEDURE PullPoint{1}(LocoChipStr: String; P : Integer; Route, SubRoute : Integer; ForcePoint, User, ErrorMessageRequired : Boolean; OUT PointResultPending : Boolean;
                       OUT ErrorMsg : String; OUT PointChangedOK : Boolean); Overload;
{ Changes the state of a point if legal }

PROCEDURE PullPoint{2}(P : Integer; ForcePoint : Boolean); Overload;
{ Changes the state of a point if legal }

PROCEDURE UnlockPointsLockedBySignal(S : Integer);
{ Remove the locking }

PROCEDURE UnlockPointLockedBySpecificRoute(P, Route : Integer; DoNotWriteMessage : Boolean);
{ Remove the locking }

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

USES RailDraw, MiscUtils, CreateRoute, Route, Lenz, StrUtils, DateUtils, Input, Options, Main, Signal;

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

FUNCTION PointIsLocked{1}(P : Integer; OUT LockingMsg : String; CheckCrossOverPoint : Boolean) : Boolean; Overload;
{ Returns true if the point is locked by whatever means }
VAR
  CatchPoint : Integer;
  PointArrayCount : Integer;
  TempLockingMsg : String;

BEGIN
  Result := False;
  LockingMsg := '';

  TRY
    WITH Points[P] DO BEGIN
      LockingMsg := '';

      IF Length(Point_LockingArray) > 0 THEN BEGIN
        LockingMsg := '';
        FOR PointArrayCount := 0 TO High(Point_LockingArray) DO BEGIN
          IF Pos('S=', Point_LockingArray[PointArrayCount]) > 0 THEN
            LockingMsg := LockingMsg + Point_LockingArray[PointArrayCount] + ','
          ELSE
            IF Pos('R=', Point_LockingArray[PointArrayCount]) > 0 THEN
              LockingMsg := LockingMsg + Point_LockingArray[PointArrayCount] + ',';
        END;
        IF RightStr(LockingMsg, 1) = ',' THEN
          LockingMsg := Copy(LockingMsg, 1, Length(LockingMsg) -1);
      END ELSE BEGIN
        { See if the heel line is occupied - depending on the point record this will mean that the point is locked }
        IF (Lines[Points[P].Point_HeelLine].Line_TC <> UnknownTrackCircuit)
        AND (TrackCircuits[Lines[Points[P].Point_HeelLine].Line_TC].TC_OccupationState <> TCUnoccupied)
        AND (TrackCircuits[Lines[Points[P].Point_HeelLine].Line_TC].TC_OccupationState <> TCOutOfUseSetByUser)
        AND (TrackCircuits[Lines[Points[P].Point_HeelLine].Line_TC].TC_OccupationState <> TCOutOfUseAsNoFeedbackReceived)
        AND Point_LockedIfHeelTCOccupied
        THEN
          LockingMsg := LockingMsg + ' TC=' + IntToStr(Lines[Point_HeelLine].Line_TC) + ' occupied at heel line';

        { Or if the straight or diverging lines is occupied - depending on the point record this will mean that the point is locked }
        IF (Lines[Points[P].Point_StraightLine].Line_TC <> UnknownTrackCircuit)
        AND (TrackCircuits[Lines[Points[P].Point_StraightLine].Line_TC].TC_OccupationState <> TCUnoccupied)
        AND (TrackCircuits[Lines[Points[P].Point_StraightLine].Line_TC].TC_OccupationState <> TCOutOfUseSetByUser)
        AND (TrackCircuits[Lines[Points[P].Point_StraightLine].Line_TC].TC_OccupationState <> TCOutOfUseAsNoFeedbackReceived)
        AND Point_LockedIfNonHeelTCsOccupied
        THEN
          LockingMsg := LockingMsg + ' TC=' + IntToStr(Lines[Point_StraightLine].Line_TC) + ' occupied at straight line';

        IF NOT PointIsCatchPoint(P) THEN BEGIN
          IF (Lines[Points[P].Point_DivergingLine].Line_TC <> UnknownTrackCircuit)
          AND (TrackCircuits[Lines[Points[P].Point_DivergingLine].Line_TC].TC_OccupationState <> TCUnoccupied)
          AND (TrackCircuits[Lines[Points[P].Point_DivergingLine].Line_TC].TC_OccupationState <> TCOutOfUseSetByUser)
          AND (TrackCircuits[Lines[Points[P].Point_DivergingLine].Line_TC].TC_OccupationState <> TCOutOfUseAsNoFeedbackReceived)
          AND Point_LockedIfNonHeelTCsOccupied
          THEN
            LockingMsg := LockingMsg + 'TC=' + IntToStr(Lines[Point_DivergingLine].Line_TC) + ' occupied at diverging line';
        END;

        { See that three-way points have the first, 'a' point, set to straight before the 'b' point is set }
        IF Point_Type = ThreeWayPointB THEN BEGIN
          IF Point_RelatedPoint = UnknownPoint THEN
            LockingMsg := LockingMsg + ' 3-way point A has no related point!'
          ELSE
            IF Points[Point_RelatedPoint].Point_PresentState <> Straight THEN
              LockingMsg := LockingMsg + ' 3-way point A (P=' + IntToStr(Point_RelatedPoint) + ') diverging';
        END ELSE
          IF Point_Type = ThreeWayPointA THEN BEGIN
            IF Point_RelatedPoint = UnknownPoint THEN
              LockingMsg := LockingMsg + ' 3-way point A has no related point!'
            ELSE
              IF Points[Point_RelatedPoint].Point_PresentState <> Straight THEN
                LockingMsg := LockingMsg + ' 3-way point B (P=' + IntToStr(Point_RelatedPoint) + ') diverging';
          END;


        { See if the point is affected by an adjoining catch point }
        CatchPoint := 0;
        WHILE CatchPoint <= High(Points) DO BEGIN
          IF ((Points[CatchPoint].Point_Type = CatchPointUp) OR (Points[CatchPoint].Point_Type = CatchPointDown))
          AND (Point_RelatedPoint <> UnknownPoint)
          THEN
            IF Points[CatchPoint].Point_RelatedPoint = P THEN
              IF Points[CatchPoint].Point_PresentState <> Diverging THEN
                LockingMsg := LockingMsg + ' catch point P=' + IntToStr(CatchPoint) + ' is not diverging';

          Inc(CatchPoint);
        END; {WHILE}

        { And see if a catch point is locked by its adjoining point }
        IF (Points[P].Point_Type = CatchPointUp) OR (Points[P].Point_Type = CatchPointDown) THEN BEGIN
          IF Point_RelatedPoint = UnknownPoint THEN
            LockingMsg := LockingMsg + ' catch point has no related point!'
          ELSE
            IF Points[Points[P].Point_RelatedPoint].Point_PresentState = Straight THEN
              LockingMsg := LockingMsg + ' protected point P=' + IntToStr(Points[P].Point_RelatedPoint) + ' is not diverging';
        END;

        { Also check if crossover points can change }
        IF CheckCrossOverPoint THEN BEGIN
          { pass false as a second argument to prevent PointIsLocked from being called recursively in an infinite loop }
          IF Point_RelatedPoint = UnknownPoint THEN
            LockingMsg := LockingMsg + ' cross-over point has no related point!'
          ELSE
            IF (Points[P].Point_Type = CrossOverPoint) AND PointIsLocked(Points[P].Point_RelatedPoint, TempLockingMsg, False) THEN
              { but also allow a cross-over point to change to be in agreement with its locked partner }
              IF Points[P].Point_PresentState = Points[Points[P].Point_RelatedPoint].Point_PresentState THEN
                LockingMsg := LockingMsg + ' cross-over point''s corresponding point P=' + PointToStr(Points[P].Point_RelatedPoint) + ' is locked';
        END;
      END;

      IF Points[P].Point_LockedByUser THEN
        LockingMsg := LockingMsg + ' P=' + IntToStr(P) + ' by user';

      IF LockingMsg = '' THEN
        LockingMsg := 'not locked'
      ELSE BEGIN
        LockingMsg := 'locked: ' + LockingMsg;
        Result := True;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG PointIsLocked: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { PointIsLocked-1 }

FUNCTION PointIsLocked{2}(P : Integer; OUT LockingMsg : String) : Boolean; Overload;
{ Returns true if the point is locked by whatever means. This function is needed to stop PointIsLocked calling itself recursively infinitely }
CONST
  CheckCrossOverPoint = True;

BEGIN
  Result := PointIsLocked(P, LockingMsg, NOT CheckCrossOverPoint);
END; { PointIsLocked-2 }

PROCEDURE LockPointByRoute(LocoChip, P, Route : Integer; DoNotWriteMessage : Boolean);
{ Mark the point as locked by a specific route }
VAR
  ElementPos : Integer;

BEGIN
  { First remove the point from the point resetting array if it's there }
  IF IsElementInIntegerArray(PointResettingToDefaultStateArray, P, ElementPos) THEN
    DeleteElementFromIntegerArray(PointResettingToDefaultStateArray, ElementPos);

  AppendToStringArray(Points[P].Point_LockingArray, 'R=' + IntToStr(Route));
  RemoveDuplicateElementsFromStringArray(Points[P].Point_LockingArray);
  Points[P].Point_LockingState := Points[P].Point_PresentState;
  IF NOT DoNotWriteMessage THEN
    Log(LocoChipToStr(LocoChip) + ' P P=' + IntToStr(P) + ' now locked ' + PointStateToStr(Points[P].Point_LockingState) + ' by R=' + IntToStr(Route));
  Points[P].Point_LocoChipLockingTheRoute := LocoChip;
  Points[P].Point_RouteLockedByLocoChip := Route;
END; { LockPointByRoute }

PROCEDURE UnlockPointsLockedBySignal(S : Integer);
{ Remove the locking }
VAR
  P : Integer;
  PointArrayCount : Integer;
  SignalFound : Boolean;

BEGIN
  FOR P := 0 TO High(Points) DO BEGIN
    { First find the array element to delete }
    PointArrayCount := -1;
    SignalFound := False;
    WHILE (PointArrayCount < High(Points[P].Point_LockingArray)) AND NOT SignalFound DO BEGIN
      Inc(PointArrayCount);
      IF Pos('S=' + IntToStr(S), Points[P].Point_LockingArray[PointArrayCount]) > 0 THEN BEGIN
        SignalFound := True;
        DeleteElementFromStringArray(Points[P].Point_LockingArray, PointArrayCount);
        Points[P].Point_LockingState := PointStateUnknown;
        IF NOT Signals[S].Signal_FailMsgWritten THEN
          Log('S P=' + IntToStr(P) + ' unlocked by S=' + IntToStr(S));
      END;
    END; {WHILE}
  END; {FOR}
END; { UnlockPointsLockedBySignal }

PROCEDURE UnlockPointLockedBySpecificRoute(P, Route : Integer; DoNotWriteMessage : Boolean);
{ Remove the locking }
VAR
  PointArrayCount : Integer;
  RouteFound : Boolean;

BEGIN
  { First find the array element to delete }
  PointArrayCount := -1;
  RouteFound := False;
  WHILE (PointArrayCount < High(Points[P].Point_LockingArray)) AND NOT RouteFound DO BEGIN
    Inc(PointArrayCount);
    IF Pos('R=' + IntToStr(Route), Points[P].Point_LockingArray[PointArrayCount]) > 0 THEN
      RouteFound := True;
  END; {WHILE}

  IF RouteFound THEN BEGIN
    DeleteElementFromStringArray(Points[P].Point_LockingArray, PointArrayCount);
    IF NOT DoNotWriteMessage THEN
      Log('P P=' + IntToStr(P) + ' now unlocked by R=' + IntToStr(Route));
  END;
  Points[P].Point_LocoChipLockingTheRoute := UnknownLocoChip;
  Points[P].Point_RouteLockedByLocoChip := UnknownRoute;
END; { UnlockPointLockedBySpecificRoute }

FUNCTION PointIsLockedByASpecificRoute(P, Route : Integer) : Boolean;
{ Returns true of the point is locked by the given route }
VAR
  PointLockCount : Integer;

BEGIN
  Result := False;
  WITH Points[P] DO BEGIN
    IF Route <> UnknownRoute THEN BEGIN
      { see if the point is locked by the given route }
      PointLockCount := 0;
      WHILE (PointLockCount < Length(Point_LockingArray)) AND (Result = False) DO BEGIN
        IF (Point_LockingArray[PointLockCount] = 'R=' + IntToStr(Route)) THEN
          Result := True;
        Inc(PointLockCount);
      END; {WHILE}
    END;
  END; {WITH}
END; { PointIsLockedByASpecificRoute }

FUNCTION PointIsLockedByAnyRoute(P : Integer; OUT RouteLockingArray : IntegerArrayType) : Boolean;
{ Returns true of the point is locked by any route }
VAR
  PointLockCount : Integer;

BEGIN
  Result := False;
  WITH Points[P] DO BEGIN
    { see if the point is locked by any route }
    PointLockCount := 0;
    WHILE (PointLockCount < Length(Point_LockingArray)) AND (Result = False) DO BEGIN
      IF Pos('R=', Point_LockingArray[PointLockCount]) > 0 THEN BEGIN
        AppendToIntegerArray(RouteLockingArray, ExtractRouteFromString(Point_LockingArray[PointLockCount]));
        Result := True;
      END;
      Inc(PointLockCount);
    END; {WHILE}
  END; {WITH}
END; { PointIsLockedByAnyRoute }

PROCEDURE PullPoint{1}(LocoChipStr : String; P : Integer; Route, SubRoute : Integer; ForcePoint, User, ErrorMessageRequired : Boolean; OUT PointResultPending : Boolean;
                       OUT ErrorMsg : String; OUT PointChangedOK : Boolean); Overload;
{ Changes the state of a point if legal }
CONST
  ChangeTwice = True;
  ProcessMessages = True;
  StopTimer = True;

VAR
  Count : Integer;
  DebugStr : String;
  EmergencyDeselectPointOK : Boolean;
  LockingFailureString : String;
  NewDirection : PointStateType;
//  PointDelayInMSS : Integer;

BEGIN
  IF Points[P].Point_RequiredState = PointStateUnknown THEN
    Exit;

  IF Points[P].Point_RequiredState = Points[P].Point_PresentState THEN
    Exit;

  WITH Points[P] DO BEGIN
    IF Point_OutOfUse THEN BEGIN
      PointChangedOK := False;
      Log(LocoChipStr + ' PG Point ' + IntToStr(P) + ' change failed - point is out of use');
    END ELSE BEGIN
      PointChangedOK := False;
      EmergencyDeselectPointOK := False;
      IF (InLockingMode AND PointIsLocked(P, LockingFailureString)) AND NOT ForcePoint THEN BEGIN
        IF User THEN
          Log(LocoChipStr + ' P Attempt by user to change P=' + IntToStr(P) + ' failed - ' + LockingFailureString);
        IF PointIsLocked(P, LockingFailureString) THEN BEGIN
          IF NOT Point_LockFailureNotedInLocksUnit THEN
            Log(LocoChipStr + ' P P=' + IntToStr(P) + ' failure: ' + LockingFailureString);
          Point_LockFailureNotedInLocksUnit := True;
        END;
        Forbid;
        PointResultPending := True;
      END ELSE BEGIN
        NewDirection := PointStateUnknown;
        DebugStr := '';
        Count := 0;

        { This prevents the points attached to a LS150 being fired too quickly one after the other, as doing so often short circuits the unit - an undocumented feature! }
//        PointDelayInMSS := 500;
//        IF NOT Point_SecondAttempt THEN BEGIN
//          IF CompareTime(Time, IncMilliSecond(LastTimeAnyPointChanged, PointDelayInMSS)) < 0 THEN BEGIN
//            IF LastPointChanged <> UnknownPoint THEN BEGIN
//              IF Points[LastPointChanged].Point_LenzUnitType = 'LS150' THEN BEGIN
//                IF Points[P].Point_LenzUnitType = 'LS150' THEN BEGIN
//                  IF Points[LastPointChanged].Point_LenzUnit = Points[P].Point_LenzUnit THEN BEGIN
//                    Log(LocoChip, 'PG P=' + IntToStr(P) + ': '
//                                     + IntToStr(PointDelayInMSS) + 'ms seconds forced delay after a previous point changed');
//                    Pause(PointDelayInMSS, NOT ProcessMessages);
//                  END;
//                END;
//              END;
//            END;
//          END;
//        END;

        { Also stop a point being changed over and over in quick succession by two competing routes }
        IF InAutoMode AND (Route <> UnknownRoute) AND InLockingMode AND NOT Point_SecondAttempt AND (IncSecond(Point_LastChangedTime, 10) > Time) THEN BEGIN
          IF NOT Point_ForcedDelayMsg2Written THEN BEGIN
            ErrorMsg := ': 10 seconds forced delay after previously changing';
            Log(LocoChipStr + ' P P=' + IntToStr(P) + ErrorMsg);
            Point_ForcedDelayMsg2Written := True;
          END;
          PointResultPending := True;
          Exit;
        END;

        Point_ForcedDelayMsg1Written := False;
        Point_ForcedDelayMsg2Written := False;

        IF Point_AwaitingManualChange AND NOT Point_FeedbackPending THEN BEGIN
          Log(LocoChipStr + ' P P=' + IntToStr(P) + ' manual change awaited');
          Debug('P=' + IntToStr(P) + ' manual change awaited');
          PointChangedOK := False;
        END ELSE BEGIN
          IF Point_FeedbackPending AND NOT ForcePoint THEN BEGIN
            { no point in trying again until the previous attempt has timed out }
            IF NOT Point_FeedbackPendingMsgWritten THEN BEGIN
              Log(LocoChipStr + ' P Attempted P=' + IntToStr(P) + ' change aborted - feedback still awaited from previous attempt');
              Point_FeedbackPendingMsgWritten := True;
            END;
            Debug('P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Points[P].Point_LenzNum) + ') feedback pending');
            PointChangedOK := False;
          END ELSE BEGIN
            IF (Point_PresentState = Point_RequiredState) AND NOT Point_SetASecondTime AND NOT ForcePoint THEN
              PointChangedOK := True
            ELSE BEGIN
              IF NOT ReplayMode THEN BEGIN
                IF NOT ForcePoint THEN
                  DebugStr := 'Changing P=' + IntToStr(P) + ' to ' + PointStateToStr(Point_RequiredState)
                ELSE
                  DebugStr := 'Forcing P=' + IntToStr(P) + ' to change to ' + PointStateToStr(Point_RequiredState);

                IF Route <> UnknownRoute THEN
                  DebugStr := DebugStr + ' ' + DescribeJourneyAndRoute([Route, SubRoute]);

                IF User THEN BEGIN
                  DebugStr := DebugStr + ' by user';
                  Log('P ' + DebugStr);
                END ELSE
                  Log(LocoChipStr + ' P ' + DebugStr);

                DebugStr := '';
              END;

              IF SystemOnline THEN BEGIN
                IF Point_ManualOperation AND NOT ForcePoint THEN BEGIN
                  PointResultPending := True;
                  RouteingSuspendedForModalDialogue := True;
                  MakeSound(1);
                  IF NOT Point_HasFeedback THEN BEGIN
                    CASE MessageDialogueWithDefault('User: P=' + IntToStr(P) + ' is not powered -' + CRLF
                                                    + 'change it to ' + PointStateToStr(Point_RequiredState) + ' and then press OK',
                                                    NOT StopTimer,
                                                    mtWarning, [mbOK, mbAbort], mbAbort)
                    OF
                      mrOK:
                        PointChangedOK := True;
                      mrAbort:
                        PointChangedOK := False;
                    END; {CASE}
                  END ELSE
                    IF NOT FWPShowMessageWindow.Visible THEN BEGIN
                      FWPShowMessage('User: P=' + IntToStr(P) + ' is not powered: change it to ' + PointStateToStr(Point_RequiredState));
                      Log('P P=' + IntToStr(P) + ' is not powered: user has been requested to change it to ' + PointStateToStr(Point_RequiredState));
                      MakeSound(1);
                      Point_FeedbackPending := True;
                      Point_AwaitingManualChange := True;
                    END;
                  RouteingSuspendedForModalDialogue := False;
                END ELSE BEGIN
                  { Points with feedback dealt with here, as are points that we want to "force" to change }
                  IF Point_HasFeedback
                  OR (NOT Point_ManualOperation AND ForcePoint)
                  THEN BEGIN
                    NewDirection := Point_RequiredState;
                    PointChangedOK := MakePointChange(LocoChipStr, P, NewDirection, Count);
                    IF PointChangedOK THEN BEGIN
                      { Point_PresentState is set by the point feedback detector reporting in }
                      IF Point_RequiredState = Point_PresentState THEN BEGIN
                        Point_SecondAttempt := False;
                        DebugStr := 'Successfully changed P=' + IntToStr(P) + ' to ' + PointStateToStr(Point_RequiredState);
                        IF User THEN
                          DebugStr := DebugStr + ' by user';

                        IF InRecordLineDrawingMode THEN BEGIN
                          IF Point_RequiredState = Straight THEN
                            Log(LocoChipStr + ' P P=' + IntToStr(P) + ' D=S')
                          ELSE
                            Log(LocoChipStr + ' P P=' + IntToStr(P) + ' D=D');
                        END;
                      END ELSE BEGIN
                        { Either the change hasn't happened, or the report of it happening is delayed, so we catch up with it later }
                        Point_FeedbackPending := True;
                        Point_FeedbackStartTime := Time;
                        IF User THEN
                          DebugStr := 'P=' + IntToStr(P) + ' change by user to ' + PointStateToStr(Point_RequiredState) + ' is pending'
                        ELSE
                          DebugStr := 'P=' + IntToStr(P) + ' change to ' + PointStateToStr(Point_RequiredState) + ' is pending';
                      END;
                    END ELSE
                      Debug('');

                    IF PointChangedOK AND ForcePoint THEN BEGIN
                      { can only be done by the user }
                      DebugStr := 'User forced P=' + IntToStr(P) + ' to change successfully to ' + PointStateToStr(NewDirection);
                      IF SubRoute <> UnknownRoute THEN
                        DebugStr := DebugStr + ' for SR=' + IntToStr(SubRoute);
                    END;
                  END ELSE
                    { Points without feedback dealt with here }
                    IF NOT Point_ManualOperation THEN BEGIN
                      PointChangedOK := MakePointChange(LocoChipStr, P, NewDirection, Count);
                      IF PointChangedOK THEN BEGIN
                        { if no feedback, do it another time, to be sure it changed }
                        Point_SetASecondTime := True;

                        PointChangedOK := MakePointChange(LocoChipStr, P, NewDirection, Count);
                        IF PointChangedOK AND NOT ForcePoint THEN BEGIN
                          { Need to check that the point has changed - ask the user for confirmation }
                          MakeSound(1);
                          RouteingSuspendedForModalDialogue := True;
                          CASE MessageDialogueWithDefault('User: P=' + IntToStr(P) + ' does not have feedback -' + CRLF
                                                          + 'press OK to confirm it has changed to ' + PointStateToStr(Point_RequiredState),
                                                          NOT StopTimer,
                                                          mtWarning, [mbOK, mbAbort], mbAbort)
                          OF
                            mrOK:
                              ;
                            mrAbort:
                              PointChangedOK := False;
                          END; {CASE}
                          RouteingSuspendedForModalDialogue := False;
                          IF PointChangedOK THEN BEGIN
                            DebugStr := 'Changed non-feedback P=' + IntToStr(P) + ' successfully to ' + PointStateToStr(Point_RequiredState);
                            IF SubRoute <> UnknownRoute THEN
                              DebugStr := DebugStr + ' for SR=' + IntToStr(SubRoute);
                            IF User THEN
                              DebugStr := DebugStr + ' by user';
                          END;
                        END;
                      END;

                      IF NOT PointChangedOK THEN BEGIN
                        DebugStr :=  'P=' + IntToStr(P) + ' failed once changing to ' + PointStateToStr(NewDirection);
                        Log(LocoChipStr + ' P ' + DebugStr);
                        EmergencyDeselectPoint(P, EmergencyDeselectPointOK);

                        { *** missing second test }
                        DebugStr :=  'System error: P=' + IntToStr(P) + ' failed twice changing to '  + PointStateToStr(NewDirection);
                        Debug('*** P=' + IntToStr(P) + ' Failure ***');
                        (*TP
                        { This is left over from the Turbo Pascal version }
                        QueueSound(900, 2);
                        TP*)
                        IF SubRoute <> UnknownRoute THEN
                          DebugStr := DebugStr + ' for SR=' + IntToStr(SubRoute);
                      END;
                    END;
                  END;
                END;
              END;

            IF PointChangedOK OR NOT SystemOnline THEN BEGIN
              IF NOT SystemOnline THEN
                PointChangedOK := True;

              IF NOT Point_HasFeedback OR NOT SystemOnline OR (Point_LenzNum = 0) OR ForcePoint OR Point_ManualOperation THEN BEGIN
                Point_PreviousState := Point_PresentState;
                IF Point_RequiredState = Straight THEN
                  Point_PresentState := Straight
                ELSE
                  Point_PresentState := Diverging;

                IF InRecordLineDrawingMode THEN BEGIN
                  IF Point_PresentState = Straight THEN
                    Log(LocoChipStr + ' P P=' + IntToStr(P) + ' D=S')
                  ELSE
                    Log(LocoChipStr + ' P P=' + IntToStr(P) + ' D=D');
                END;
              END;
              Point_LockFailureNotedInLocksUnit := False;
              Point_LockFailureNotedinSubRouteUnit := False;
              InvalidateScreen(UnitRef, 'PullPoint');
            END;
          END;

          IF DebugStr <> '' THEN BEGIN
            IF Pos('by user ', DebugStr) > 0 THEN
              Log('P ' + DebugStr)
            ELSE
              Log(LocoChipStr + ' P ' + DebugStr);
          END;
          PointResultPending := Point_FeedbackPending OR Point_AwaitingManualChange;
        END;

        IF PointChangedOK OR NOT SystemOnline THEN BEGIN
          { record when it happened, to give a delay between each point being switched }
          LastTimeAnyPointChanged := Time;
          LastPointChanged := P;
          { and between this particular point being repeatedly changed - needed to stop competing routes switching a point back and fore over and over again }
          Point_LastChangedTime := Time;
        END ELSE
          IF NOT Point_AwaitingManualChange AND SystemOnline AND NOT PointChangedOK THEN
            Log(LocoChipStr + ' P System failure in setting P=' + IntToStr(P));
      END;
    END;
  END; {WITH}
END; { PullPoint-1 }

PROCEDURE PullPoint{2}(P : Integer; ForcePoint : Boolean); Overload;
{ Changes the state of a point if legal }
CONST
  ErrorMessageRequired = True;

VAR
  DebugStr : String;
  PointResultPending : Boolean;
  OK : Boolean;

BEGIN
  DebugStr := '';
  PullPoint(UnknownLocoChipStr, P, NoRoute, NoSubRoute, ForcePoint, ByUser, NOT ErrorMessageRequired, PointResultPending, DebugStr, OK);
END; { PullPoint-2 }

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
