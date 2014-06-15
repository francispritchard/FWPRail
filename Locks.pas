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

PROCEDURE CheckRouteAheadLocking(T : TrainElement; RouteArray : StringArrayType; OUT RouteCurrentlyLocked, RoutePermanentlyLocked : Boolean; OUT LockingMsg : String);
{ Tests a given route array to see if anything on it is locked }

PROCEDURE ClearHiddenAspectSignals(T : TrainElement; HiddenAspectSignal : Integer);
{ Clear the hidden aspects of a given signal }

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

PROCEDURE PullPoint{1}(P, LocoChip : Integer; Route, SubRoute : Integer; ForcePoint, User, ErrorMessageRequired : Boolean; OUT PointResultPending : Boolean;
                       OUT ErrorMsg : String; OUT PointChangedOK : Boolean); Overload;
{ Changes the state of a point if legal }

PROCEDURE PullPoint{2}(P : Integer; ForcePoint : Boolean); Overload;
{ Changes the state of a point if legal }

PROCEDURE PullSignal{1}(LocoChip, S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        SettingString : String; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }

PROCEDURE PullSignal{2}(LocoChip, S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }

PROCEDURE PullSignal{3}(LocoChip, S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer; ResetTC : Integer;
                        User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal. This version is only used by signals resetting trackcircuits, which can happen even if the signal is locked by a route,}

PROCEDURE PullSignal{4}(LocoChip, S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer; SettingString : String;
                        TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal; includes the original setting string for saving if necessary }

FUNCTION RouteAheadOutOfUse(RouteArray : StringArrayType; OUT LockingMsg : String) : Boolean;
{ Tests a given route locking array to see if anything on it is out of use }

PROCEDURE SetHiddenAspectSignals(T : TrainElement; HiddenAspectSignal, Journey, Route : Integer);
{ Find and set current and previous hidden aspects }

PROCEDURE SetIndicator(LocoChip, S : Integer; IndicatorState : IndicatorStateType; TheatreIndicatorString : String; Route : Integer; User : Boolean);
{ Turn the indicator of a particular signal on or off }

PROCEDURE SetPreviousSignals(LocoChip : Integer; S : Integer);
{ Sees what previous signals are set to, and resets aspects accordingly. PreviousSignal1 is the nearer and PreviousSignal2 the further }

FUNCTION SignalIsLocked(S : Integer; OUT LockingMsg : String) : Boolean;
{ Returns true if the signal is locked }

FUNCTION SignalIsLockedByAnyRoute(S : Integer; OUT RouteLockingArray : IntegerArrayType) : Boolean;
{ Returns true if the signal is locked by any route (in which case the routes doing the locking are returned in RouteLockingArray) }

FUNCTION SignalIsLockedBySpecificRoute(S, Route : Integer) : Boolean;
{ Returns true if the signal is locked by the given route, unless Route is -1, when it returns true if the signal is locked by any route (in which case the routes doing the
  locking are returned in RouteLockingArray).
}
PROCEDURE UnlockPointsLockedBySignal(S : Integer);
{ Remove the locking }

PROCEDURE UnlockPointLockedBySpecificRoute(P, Route : Integer; DoNotWriteMessage : Boolean);
{ Remove the locking }

PROCEDURE UnlockSignalLockedBySpecificRoute(S, Route : Integer);
{ Remove the locking }

PROCEDURE UnlockSignalLockedByUser(S : Integer);
{ Remove the locking }

PROCEDURE UnlockTrackCircuitRouteLocking(TC : Integer);
{ Unlock a given track circuit }

PROCEDURE WriteLockingDataToLockListWindow;
{ Writes locking data to a popup screen }

PROCEDURE WriteRouteInfoToLockListWindow;
{ Writes routeing data to a popup screen }

{$R *.dfm}

IMPLEMENTATION

USES RailDraw, MiscUtils, CreateRoute, Route, Lenz, StrUtils, DateUtils, Input, Options, Main;

VAR
  UnitRef : String = 'Locks';
  FindARouteFailMsgWritten : Boolean = False;

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
        THEN BEGIN
          IF LockingMsg = '' THEN
            LockingMsg := 'locked:';
          LockingMsg := LockingMsg + ' TC=' + IntToStr(Lines[Point_HeelLine].Line_TC) + ' occupied at heel line';
        END;

        { Or if the straight or diverging lines is occupied - depending on the point record this will mean that the point is locked }
        IF (Lines[Points[P].Point_StraightLine].Line_TC <> UnknownTrackCircuit)
        AND (TrackCircuits[Lines[Points[P].Point_StraightLine].Line_TC].TC_OccupationState <> TCUnoccupied)
        AND (TrackCircuits[Lines[Points[P].Point_StraightLine].Line_TC].TC_OccupationState <> TCOutOfUseSetByUser)
        AND (TrackCircuits[Lines[Points[P].Point_StraightLine].Line_TC].TC_OccupationState <> TCOutOfUseAsNoFeedbackReceived)
        AND Point_LockedIfNonHeelTCsOccupied
        THEN BEGIN
          IF LockingMsg = '' THEN
            LockingMsg := 'locked:';
          LockingMsg := LockingMsg + ' TC=' + IntToStr(Lines[Point_StraightLine].Line_TC) + ' occupied at straight line';
        END;

        IF NOT PointIsCatchPoint(P) THEN BEGIN
          IF (Lines[Points[P].Point_DivergingLine].Line_TC <> UnknownTrackCircuit)
          AND (TrackCircuits[Lines[Points[P].Point_DivergingLine].Line_TC].TC_OccupationState <> TCUnoccupied)
          AND (TrackCircuits[Lines[Points[P].Point_DivergingLine].Line_TC].TC_OccupationState <> TCOutOfUseSetByUser)
          AND (TrackCircuits[Lines[Points[P].Point_DivergingLine].Line_TC].TC_OccupationState <> TCOutOfUseAsNoFeedbackReceived)
          AND Point_LockedIfNonHeelTCsOccupied
          THEN BEGIN
            IF LockingMsg = '' THEN
              LockingMsg := 'locked:';
            LockingMsg := LockingMsg + 'TC=' + IntToStr(Lines[Point_DivergingLine].Line_TC) + ' occupied at diverging line';
          END;
        END;

        { See that three-way points have the first, 'a' point, set to straight before the 'b' point is set }
        IF Point_Type = ThreeWayPointB THEN BEGIN
          IF Points[Point_OtherPoint].Point_PresentState <> Straight THEN BEGIN
            IF LockingMsg = '' THEN
              LockingMsg := 'locked:';
            LockingMsg := LockingMsg + ' 3-way point A (P=' + IntToStr(Point_OtherPoint) + ') diverging';
          END;
        END ELSE
          IF Point_Type = ThreeWayPointA THEN BEGIN
            IF Points[Point_OtherPoint].Point_PresentState <> Straight THEN BEGIN
              IF LockingMsg = '' THEN
                LockingMsg := 'locked:';
              LockingMsg := LockingMsg + ' 3-way point B (P=' + IntToStr(Point_OtherPoint) + ') diverging';
            END;
          END;

        { See if the point is affected by an adjoining catch point }
        CatchPoint := 0;
        WHILE CatchPoint <= High(Points) DO BEGIN
          IF (Points[CatchPoint].Point_Type = CatchPointUp) OR (Points[CatchPoint].Point_Type = CatchPointDown) THEN BEGIN
            IF Points[CatchPoint].Point_OtherPoint = P THEN BEGIN
              IF Points[CatchPoint].Point_PresentState <> Diverging THEN BEGIN
                IF LockingMsg = '' THEN
                  LockingMsg := 'locked:';
                LockingMsg := LockingMsg + ' catch point P=' + IntToStr(CatchPoint) + ' is not diverging';
              END;
            END;
          END;
          Inc(CatchPoint);
        END; {WHILE}

        { And see if a catch point is locked by its adjoining point }
        IF (Points[P].Point_Type = CatchPointUp) OR (Points[P].Point_Type = CatchPointDown) THEN BEGIN
          IF Points[Points[P].Point_OtherPoint].Point_PresentState = Straight THEN BEGIN
            IF LockingMsg = '' THEN
              LockingMsg := 'locked:';
            LockingMsg := LockingMsg + ' protected point P=' + IntToStr(Points[P].Point_OtherPoint) + ' is not diverging';
          END;
        END;

        { Also check if crossover points can change }
        IF CheckCrossOverPoint THEN
          { pass false as a second argument to prevent PointIsLocked from being called recursively in an infinite loop }
          IF (Points[P].Point_Type = CrossOverPoint) AND PointIsLocked(Points[P].Point_OtherPoint, TempLockingMsg, False) THEN BEGIN
            { but also allow a cross-over point to change to be in agreement with its locked partner }
            IF Points[P].Point_PresentState = Points[Points[P].Point_OtherPoint].Point_PresentState THEN BEGIN
              IF LockingMsg = '' THEN
                LockingMsg := 'locked:';
              LockingMsg := LockingMsg + ' cross-over point''s corresponding point P=' + PointToStr(Points[P].Point_OtherPoint) + ' is locked';
            END;
          END;
      END;

      IF Points[P].Point_LockedByUser THEN BEGIN
        IF LockingMsg = '' THEN
          LockingMsg := 'locked:';
        LockingMsg := LockingMsg + ' P=' + IntToStr(P) + ' by user';
      END;

      IF LockingMsg = '' THEN
        LockingMsg := 'not locked'
      ELSE
        Result := True;
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
  Result := PointIsLocked(P, LockingMsg, CheckCrossOverPoint);
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
  Points[P].Point_RouteLockedByLocoChip := LocoChip;
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
    WHILE (PointArrayCount < High(Points[P].Point_LockingArray))
    AND NOT SignalFound
    DO BEGIN
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
  WHILE (PointArrayCount < High(Points[P].Point_LockingArray))
  AND NOT RouteFound
  DO BEGIN
    Inc(PointArrayCount);
    IF Pos('R=' + IntToStr(Route), Points[P].Point_LockingArray[PointArrayCount]) > 0 THEN
      RouteFound := True;
  END; {WHILE}

  IF RouteFound THEN BEGIN
    DeleteElementFromStringArray(Points[P].Point_LockingArray, PointArrayCount);
    IF NOT DoNotWriteMessage THEN
      Log('P P=' + IntToStr(P) + ' now unlocked by R=' + IntToStr(Route));
  END;
  Points[P].Point_RouteLockedByLocoChip := UnknownLocoChip;
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
      WHILE (PointLockCount < Length(Point_LockingArray))
      AND (Result = False)
      DO BEGIN
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
    WHILE (PointLockCount < Length(Point_LockingArray))
    AND (Result = False)
    DO BEGIN
      IF Pos('R=', Point_LockingArray[PointLockCount]) > 0 THEN BEGIN
        AppendToIntegerArray(RouteLockingArray, ExtractRouteFromString(Point_LockingArray[PointLockCount]));
        Result := True;
      END;
      Inc(PointLockCount);
    END; {WHILE}
  END; {WITH}
END; { PointIsLockedByAnyRoute }

FUNCTION SignalIsLockedBySpecificRoute(S, Route : Integer) : Boolean;
{ Returns true if the signal is locked by the given route }
VAR
  LockCount : Integer;

BEGIN
  Result := False;
  WITH Signals[S] DO BEGIN
    IF Route <> UnknownRoute THEN BEGIN
      { see if the signal is locked by the given route }
      LockCount := 0;
      WHILE (LockCount < Length(Signal_LockedArray))
      AND (Result = False)
      DO BEGIN
        IF (Signal_LockedArray[LockCount] = 'R=' + IntToStr(Route)) THEN
          Result := True;
        Inc(LockCount);
      END; {WHILE}
    END;
  END; {WITH}
END; { SignalIsLockedBySpecificRoute }

FUNCTION SignalIsLockedByAnyRoute(S : Integer; OUT RouteLockingArray : IntegerArrayType) : Boolean;
{ Returns true if the signal is locked by any route (in which case the routes doing the locking are returned in RouteLockingArray) }
VAR
  LockCount : Integer;

BEGIN
  SetLength(RouteLockingArray, 0);
  Result := False;
  WITH Signals[S] DO BEGIN
    { see if the signal is locked by any route }
    LockCount := 0;
    WHILE LockCount < Length(Signal_LockedArray) DO BEGIN
      IF Pos('R=', Signal_LockedArray[LockCount]) > 0 THEN BEGIN
        Result := True;
        AppendToIntegerArray(RouteLockingArray, ExtractRouteFromString(Signal_LockedArray[LockCount]));
      END;
      Inc(LockCount);
    END; {WHILE}
  END; {WITH}
END; { SignalIsLockedByAnyRoute }

FUNCTION SignalIsLockedByOppositePassingLoopSignal(S : Integer; OUT OtherS : Integer) : Boolean;
{ Returns true if an opposite passing loop signal is off }
BEGIN
  Result := False;
  IF Signals[S].Signal_OppositePassingLoopSignal <> UnknownSignal THEN BEGIN
    IF Signals[Signals[S].Signal_OppositePassingLoopSignal].Signal_Aspect <> RedAspect THEN BEGIN
      Result := True;
      OtherS := Signals[S].Signal_OppositePassingLoopSignal;
    END;
  END;
END; { SignalisLockedByOppositePassingLoopSignal }

FUNCTION SignalIsLocked(S : Integer; OUT LockingMsg : String) : Boolean;
{ Returns true if the signal is locked }
VAR
  OtherS : Integer;
  Route : Integer;
  RoutePos : Integer;
  SArrayCount : Integer;

BEGIN
  LockingMsg := '';

  IF Length(Signals[S].Signal_LockedArray) = 0 THEN
    Result := False
  ELSE BEGIN
    Result := True;
    FOR SArrayCount := 0 TO High(Signals[S].Signal_LockedArray) DO BEGIN
      RoutePos := Pos('R=', Signals[S].Signal_LockedArray[SArrayCount]);
      IF RoutePos > 0 THEN BEGIN
        Route := ExtractRouteFromString(Signals[S].Signal_LockedArray[SArrayCount]);
        LockingMsg := ' ' + Signals[S].Signal_LockedArray[SArrayCount] + ' (' + LocoChipToStr(Routes_LocoChips[Route]) + ')';
      END ELSE
        IF Pos('USER', Signals[S].Signal_LockedArray[SArrayCount]) > 0 THEN
          LockingMsg := ' ' + Signals[S].Signal_LockedArray[SArrayCount]
    END;
  END;

  IF Signals[S].Signal_ApproachLocked THEN BEGIN
    Result := True;
    LockingMsg := LockingMsg + ' AC';
  END;

  IF SignalIsLockedByOppositePassingLoopSignal(S, OtherS) THEN BEGIN
    Result := True;
    LockingMsg := ' OPLS (S=' + IntToStr(Others) + ')';
  END;

  IF Result = False THEN
    LockingMsg := 'not locked'
  ELSE
    LockingMsg := 'locked by' + LockingMsg;
END; { SignalIsLocked }

PROCEDURE UnlockSignalLockedBySpecificRoute(S, Route : Integer);
{ Remove the locking }
VAR
  ArrayCount : Integer;
  RouteFound : Boolean;

BEGIN
  { First find the array element to delete }
  ArrayCount := -1;
  RouteFound := False;
  WHILE (ArrayCount < High(Signals[S].Signal_LockedArray))
  AND NOT RouteFound
  DO BEGIN
    Inc(ArrayCount);
    IF Pos('R=' + IntToStr(Route), Signals[S].Signal_LockedArray[ArrayCount]) > 0 THEN
      RouteFound := True;
  END; {WHILE}

  IF RouteFound THEN BEGIN
    DeleteElementFromStringArray(Signals[S].Signal_LockedArray, ArrayCount);
    Log('S S=' + IntToStr(S) + ' now unlocked by R=' + IntToStr(Route));
  END;
END; { UnlockSignalLockedBySpecificRoute }

PROCEDURE UnlockSignalLockedByUser(S : Integer);
{ Remove the locking }
VAR
  ArrayCount : Integer;
  UserLockingFound : Boolean;

BEGIN
  { First find the array element to delete }
  ArrayCount := -1;
  UserLockingFound := False;
  WHILE (ArrayCount < High(Signals[S].Signal_LockedArray))
  AND NOT UserLockingFound
  DO BEGIN
    Inc(ArrayCount);
    IF Pos('USER', Signals[S].Signal_LockedArray[ArrayCount]) > 0 THEN
      UserLockingFound := True;
  END; {WHILE}

  IF UserLockingFound THEN BEGIN
    DeleteElementFromStringArray(Signals[S].Signal_LockedArray, ArrayCount);
    Log('S S=' + IntToStr(S) + ' now unlocked by user ');
  END;
END; { UnlockSignalLockedBySpecificRoute }

PROCEDURE SetHiddenAspectSignals(T : TrainElement; HiddenAspectSignal, Journey, Route : Integer);
{ Find and set current and previous hidden aspects }

  PROCEDURE FindPreviousHiddenAspectSignals(RouteArray : StringArrayType; CurrentSignal : Integer; VAR PreviousSignal1, PreviousSignal2 : Integer);
  { Look through a journey to see if any previous aspects need to be set too }
  VAR
    Done : Boolean;
    RouteArrayPos : Integer;
    TempS : Integer;

  BEGIN
    RouteArrayPos := High(RouteArray);
    Done := False;
    WHILE (RouteArrayPos > -1)
    AND NOT Done
    DO BEGIN
      IF Pos('FS=', RouteArray[RouteArrayPos]) > 0 THEN BEGIN
        IF ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal THEN
          IF (PreviousSignal1 = UnknownSignal)
          AND (ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal)
          THEN
            PreviousSignal1 := ExtractSignalFromString(RouteArray[RouteArrayPos])
          ELSE
            IF PreviousSignal2 = UnknownSignal THEN BEGIN
              { assuming it's not the signal we've just found above }
              TempS := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              IF (TempS <> UnknownSignal)
              AND (TempS <> PreviousSignal1)
              THEN BEGIN
                Done := True;
                { only use it if it's a four aspect signal }
                IF Signals[TempS].Signal_Type = FourAspect THEN
                  PreviousSignal2 := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              END;
            END;
      END;
      Dec(RouteArrayPos)
    END; {WHILE}
  END; { FindPreviousHiddenAspectSignals }

VAR
  PreviousHiddenAspectSignal1 : Integer;
  PreviousHiddenAspectSignal2 : Integer;
  TempJourneyCount : Integer;
  TempLocoChipStr : String;

BEGIN
  WITH RailWindowBitmap.Canvas DO BEGIN
    WITH Trains[T] DO BEGIN
      IF T <= High(Trains) THEN
        TempLocoChipStr := LocoChipToStr(Train_LocoChip)
      ELSE
        TempLocoChipStr := '';

      IF Signals[HiddenAspectSignal].Signal_Aspect <> RedAspect THEN
        Log(TempLocoChipStr + ' X+ S=' + IntToStr(HiddenAspectSignal) + ': cannot set hidden aspect as signal is off')
      ELSE BEGIN
        Signals[HiddenAspectSignal].Signal_HiddenAspect := RedAspect;

        Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                            + ' S=' + IntToStr(HiddenAspectSignal) + ': hidden aspect set to red');
        IF ShowSignalHiddenAspects THEN
          DrawSignal(HiddenAspectSignal);

        PreviousHiddenAspectSignal1 := UnknownSignal;
        PreviousHiddenAspectSignal2 := UnknownSignal;
        TempJourneyCount := Journey;
        IF TempLocoChipStr <> '' THEN BEGIN
          WHILE ((PreviousHiddenAspectSignal1 = UnknownSignal) OR (PreviousHiddenAspectSignal2 = UnknownSignal))
          AND (TempJourneyCount > -1)
          AND NOT Train_JourneysArray[TempJourneyCount].TrainJourney_Cleared
          DO BEGIN
            FindPreviousHiddenAspectSignals(Train_JourneysArray[TempJourneyCount].TrainJourney_RouteArray, HiddenAspectSignal, PreviousHiddenAspectSignal1,
                                                                                                                                                PreviousHiddenAspectSignal2);
            Dec(TempJourneyCount);
          END; {WHILE}
        END;

        Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal1 := PreviousHiddenAspectSignal1;
        Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal2 := PreviousHiddenAspectSignal2;

        { If this signal has a hidden aspect that's on, make sure previous signals are included }
        IF (PreviousHiddenAspectSignal1 <> UnknownSignal)
        AND (Signals[PreviousHiddenAspectSignal1].Signal_Type = FourAspect)
        THEN BEGIN
          Signals[PreviousHiddenAspectSignal1].Signal_HiddenAspect := SingleYellowAspect;
          Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                              + ' S=' + IntToStr(HiddenAspectSignal) + '''s previous signal S='
                              + IntToStr(PreviousHiddenAspectSignal1) + ': hidden aspect set to single yellow');
          IF ShowSignalHiddenAspects THEN
            DrawSignal(Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal1);

          IF (PreviousHiddenAspectSignal2 <> UnknownSignal)
          AND ((Signals[PreviousHiddenAspectSignal2].Signal_Type = FourAspect)
               OR (Signals[PreviousHiddenAspectSignal2].Signal_Type = ThreeAspect))
          THEN BEGIN
            Signals[PreviousHiddenAspectSignal2].Signal_HiddenAspect := DoubleYellowAspect;
            Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                                + ' S=' + IntToStr(HiddenAspectSignal) + '''s previous signal but one S='
                                + IntToStr(PreviousHiddenAspectSignal2) + ': hidden aspect set to double yellow');
            IF ShowSignalHiddenAspects THEN
              DrawSignal(Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal2);
          END;
        END ELSE
          IF (PreviousHiddenAspectSignal1 <> UnknownSignal)
          AND (Signals[PreviousHiddenAspectSignal1].Signal_Type = ThreeAspect)
          THEN BEGIN
            Signals[PreviousHiddenAspectSignal1].Signal_HiddenAspect := SingleYellowAspect;
            Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                                + ' S=' + IntToStr(HiddenAspectSignal) + '''s previous signal S='
                                + IntToStr(PreviousHiddenAspectSignal1) + ' hidden aspect set to single yellow');
            IF ShowSignalHiddenAspects THEN
              DrawSignal(Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal1);
          END;
      END;
    END; {WITH}
  END; {WITH}
END; { SetHiddenAspectSignals }

PROCEDURE ClearHiddenAspectSignals(T : TrainElement; HiddenAspectSignal : Integer);
{ Clear the hidden aspects of a given signal }
VAR
  TempLocoChipStr : String;

BEGIN
  WITH Trains[T] DO BEGIN
    IF T <= High(Trains) THEN
      TempLocoChipStr := LocoChipToStr(Train_LocoChip)
    ELSE
      TempLocoChipStr := '';

    Signals[HiddenAspectSignal].Signal_HiddenAspect := NoAspect;
    IF ShowSignalHiddenAspects THEN
      DrawSignal(HiddenAspectSignal);
    Log(TempLocoChipStr + ' R S=' + IntToStr(HiddenAspectSignal) + ' hidden aspect set to no aspect');

    { and also reset any previous hidden aspect signals }
    IF (Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal1 <> UnknownSignal)
    AND (Signals[Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal1].Signal_HiddenAspect <> NoAspect)
    THEN BEGIN
      Signals[Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal1].Signal_HiddenAspect := NoAspect;
      Log(TempLocoChipStr + ' R S=' + IntToStr(HiddenAspectSignal) + '''s previous signal S='
                          + IntToStr(Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal1)
                          + ' hidden aspect set to no aspect');
      IF ShowSignalHiddenAspects THEN
        DrawSignal(Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal1);
    END;

    IF (Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal2 <> UnknownSignal)
    AND (Signals[Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal2].Signal_HiddenAspect <> NoAspect)
    THEN BEGIN
      Signals[Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal2].Signal_HiddenAspect := NoAspect;
      Log(TempLocoChipStr + ' R S=' + IntToStr(HiddenAspectSignal) + '''s previous signal but one S='
                          + IntToStr(Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal2)
                          + ' hidden aspect set to no aspect');
      IF ShowSignalHiddenAspects THEN
        DrawSignal(Signals[HiddenAspectSignal].Signal_PreviousHiddenAspectSignal2);
     END;
  END; {WITH}
END; { ClearHiddenAspectSignals }

PROCEDURE SetPreviousSignals(LocoChip : Integer; S : Integer);
{ Sees what previous signals are set to, and resets aspects accordingly. PreviousSignal1 is the nearer and PreviousSignal2 the further }
BEGIN
  WITH Signals[S] DO BEGIN
    CASE Signal_Aspect OF
      RedAspect:
        IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
          SetSignal(LocoChip, Signal_PreviousSignal1, SingleYellowAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousSignal2 <> UnknownSignal THEN BEGIN
            IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
              SetSignal(LocoChip, Signal_PreviousSignal2, DoubleYellowAspect, LogSignalData, NOT ForceAWrite);
          END;
        END;
      SingleYellowAspect:
        IF (Signal_IndicatorState <> NoIndicatorLit)
        AND (Signal_ApproachControlAspect = SingleYellowAspect)
        THEN BEGIN
          IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
            SetSignal(LocoChip, Signal_PreviousSignal1, FlashingSingleYellowAspect, LogSignalData, NOT ForceAWrite);
            IF Signal_PreviousSignal2 <> UnknownSignal THEN
              IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
                SetSignal(LocoChip, Signal_PreviousSignal2, FlashingDoubleYellowAspect, LogSignalData, NOT ForceAWrite);
          END;
        END ELSE BEGIN
          IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
            IF Signals[Signal_PreviousSignal1].Signal_Type = FourAspect THEN
              SetSignal(LocoChip, Signal_PreviousSignal1, DoubleYellowAspect, LogSignalData, NOT ForceAWrite)
            ELSE
              SetSignal(LocoChip, Signal_PreviousSignal1, GreenAspect, LogSignalData, NOT ForceAWrite);
            IF Signal_PreviousSignal2 <> UnknownSignal THEN
              IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
                SetSignal(LocoChip, Signal_PreviousSignal2, GreenAspect, LogSignalData, NOT ForceAWrite);
          END;
        END;
      DoubleYellowAspect:
        IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
          SetSignal(LocoChip, Signal_PreviousSignal1, GreenAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousSignal2 <> UnknownSignal THEN
            IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
              SetSignal(LocoChip, Signal_PreviousSignal2, GreenAspect, LogSignalData, NOT ForceAWrite);
        END;
      GreenAspect:
        IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
          SetSignal(LocoChip, Signal_PreviousSignal1, GreenAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousSignal2 <> UnknownSignal THEN
            IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
              SetSignal(LocoChip, Signal_PreviousSignal2, GreenAspect, LogSignalData, NOT ForceAWrite);
        END;
    END; {CASE}
  END; {WITH}
END; { SetPreviousSignals }

PROCEDURE SetIndicator(LocoChip, S : Integer; IndicatorState : IndicatorStateType; TheatreIndicatorString : String; Route : Integer; User : Boolean);
{ Turn the indicator of a particular signal on or off }
CONST
  ShowNames = True;

VAR
  DebugStr : String;
  I : Integer;
  OK : Boolean;
  RouteLockingArray : IntegerArrayType;

BEGIN
  OK := True;
  WITH Signals[S] DO BEGIN
    IF Signals[S].Signal_OutOfUse THEN BEGIN
      IF NOT Signals[S].Signal_OutOfUseMsgWritten THEN BEGIN
        IF NOT User THEN
          Log('S Cannot change an indicator for a signal (S=' + IntToStr(S) + ') which is out of use')
        ELSE
          Log('S+ User cannot change an indicator for a signal (S=' + IntToStr(S) + ') which is out of use');
        Signals[S].Signal_OutOfUseMsgWritten := True;
      END;
    END ELSE BEGIN
      IF (Route <> UnknownRoute)
      AND SignalIsLockedByAnyRoute(S, RouteLockingArray)
      THEN BEGIN
        IF (Length(RouteLockingArray) > 1) OR (RouteLockingArray[0] <> Route) THEN BEGIN
          OK := False;
          IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
            Signals[S].Signal_FailMsgWritten := True;
            DebugStr := 'Cannot set indicator for S=' + IntToStr(S) + ' as signal locked by';
            IF Length(RouteLockingArray) = 1 THEN
              DebugStr := DebugStr + ' R=' + IntToStr(RouteLockingArray[0])
            ELSE
              FOR I := 0 TO High(RouteLockingArray) DO
                DebugStr := DebugStr + ' R=' + IntToStr(RouteLockingArray[I]);

            Log(LocoChipToStr(LocoChip) + ' R ' + DebugStr);
          END;
        END;
      END;

      IF OK THEN BEGIN
        Signal_IndicatorState := IndicatorState;
        Signal_TheatreIndicatorString := TheatreIndicatorString;

        IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
          CASE Signal_IndicatorState OF
            NoIndicatorLit:
              DebugStr := 'No Indicator';
            UpperLeftIndicatorLit:
              DebugStr := 'Upper Left Indicator';
            MiddleLeftIndicatorLit:
              DebugStr := 'Middle Left Indicator';
            LowerLeftIndicatorLit:
              DebugStr := 'Lower Left Indicator';
            UpperRightIndicatorLit:
              DebugStr := 'Upper Right Indicator';
            MiddleRightIndicatorLit:
              DebugStr := 'Middle Right Indicator';
            LowerRightIndicatorLit:
              DebugStr := 'Lower Right Indicator';
            QueryIndicatorLit:
              DebugStr := 'Query';
            RightIndicatorLit:
              DebugStr := 'Right';
            LeftIndicatorLit:
              DebugStr := 'Left';
            TheatreIndicatorLit:
              DebugStr := 'Theatre (' + Signal_TheatreIndicatorString + ')';
          END; {CASE}

          IF Signal_IndicatorState <> NoIndicatorLit THEN
            DebugStr := 'Indicator for S=' + IntToStr(S) + ' set to ' + DebugStr
          ELSE
            DebugStr := 'Indicator for S=' + IntToStr(S) + ' cleared';
          IF Route <> UnknownRoute THEN
            DebugStr := DebugStr + ' by R=' + IntToStr(Route);
          Log(LocoChipToStr(LocoChip) + ' S ' + DebugStr);

          InvalidateScreen(UnitRef, 'SetIndicator');

          IF SystemOnline THEN
            { NB: Often route indicator will be operated by a different LF100 to the one operating the signal }
            SetSignalRouteFunction(LocoChip, S);
        END;
      END;
    END;
  END; {WITH}
END; { SetIndicator }

PROCEDURE PullSignalMainProcedure(LocoChip, S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                                  SettingString : String; ResetTC : Integer; TrainType : TypeOfTrainType; User : Boolean; OUT OK : Boolean);
{ Changes the state of a signal if legal }

  FUNCTION SignalIsLockedByUser(S : Integer) : Boolean;
  { Returns true if the signal is locked by the user }
  VAR
    LockCount : Integer;

  BEGIN
    Result := False;
    WITH Signals[S] DO BEGIN
      LockCount := 0;
      WHILE (LockCount < Length(Signal_LockedArray))
      AND (Result = False)
      DO BEGIN
        IF Signal_LockedArray[LockCount] = 'USER' THEN
          Result := True;
        Inc(LockCount);
      END; {WHILE}
    END; {WITH}
  END; { SignalIsLockedByUser }

  PROCEDURE LockSignalByRoute(LocoChip, S, Route : Integer; DoNotWriteMessage : Boolean);
  { Mark the signal as locked by a specific route }
  VAR
    ElementPos : Integer;

  BEGIN
    IF NOT IsElementInStringArray(Signals[S].Signal_LockedArray, 'R=' + IntToStr(Route), ElementPos) THEN BEGIN
      AppendToStringArray(Signals[S].Signal_LockedArray, 'R=' + IntToStr(Route));
      IF NOT DoNotWriteMessage THEN
        Log(LocoChipToStr(LocoChip) + ' S S=' + IntToStr(S) + ' now locked by R=' + IntToStr(Route));
    END;
  END; { LockSignalByRoute }

  PROCEDURE LockSignalByUser(S : Integer);
  { Mark the signal as locked by a user }
  BEGIN
    AppendToStringArray(Signals[S].Signal_LockedArray, 'USER');
    RemoveDuplicateElementsFromStringArray(Signals[S].Signal_LockedArray);
    Log('S S=' + IntToStr(S) + ' now locked by user');
  END; { LockSignalByUser }

  PROCEDURE FindPreviousHiddenAspectSignals(RouteArray : StringArrayType; CurrentSignal : Integer; VAR PreviousSignal1, PreviousSignal2 : Integer);
  { Look through a journey to see if any previous aspects need to be set too }
  VAR
    Done : Boolean;
    RouteArrayPos : Integer;
    TempS : Integer;

  BEGIN
    RouteArrayPos := High(RouteArray);
    Done := False;
    WHILE (RouteArrayPos > -1)
    AND NOT Done
    DO BEGIN
      IF Pos('FS=', RouteArray[RouteArrayPos]) > 0 THEN BEGIN
        IF ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal THEN
          IF (PreviousSignal1 = UnknownSignal)
          AND (ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal)
          THEN
            PreviousSignal1 := ExtractSignalFromString(RouteArray[RouteArrayPos])
          ELSE
            IF PreviousSignal2 = UnknownSignal THEN BEGIN
              { assuming it's not the signal we've just found above }
              TempS := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              IF (TempS <> UnknownSignal)
              AND (TempS <> PreviousSignal1)
              THEN BEGIN
                Done := True;
                { only use it if it's a four aspect signal }
                IF Signals[TempS].Signal_Type = FourAspect THEN
                  PreviousSignal2 := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              END;
            END;
      END;
      Dec(RouteArrayPos)
    END; {WHILE}
  END; { FindPreviousHiddenAspectSignals }

  FUNCTION SignalLockingOK(LocoChip, S : Integer; LockList : StringArrayType; ShowError : Boolean) : Boolean;
  { Accepts a string containing a list of locking requirements, consisting of pairs of points or signal names then '/' or '-', or '\' or '=' respectively. Also checks
    routeing.
  }
    PROCEDURE LockPointBySignal(P, S : Integer);
    { Mark the point as locked by a specific signal }
    VAR
      ElementPos : Integer;

    BEGIN
      { First remove the point from the point resetting array if it's there }
      IF IsElementInIntegerArray(PointResettingToDefaultStateArray, P, ElementPos) THEN
        DeleteElementFromIntegerArray(PointResettingToDefaultStateArray, ElementPos);

      AppendToStringArray(Points[P].Point_LockingArray, 'S=' + IntToStr(S));
      RemoveDuplicateElementsFromStringArray(Points[P].Point_LockingArray);
      Points[P].Point_LockingState := Points[P].Point_PresentState;
    END; { LockPointBySignal }

    FUNCTION PointIsLockedByASpecificSignal(P, S : Integer) : Boolean;
    { Returns true of the point is locked by the given signal }
    VAR
      PointLockCount : Integer;

    BEGIN
      Result := False;
      WITH Points[P] DO BEGIN
        IF S <> UnknownSignal THEN BEGIN
          PointLockCount := 0;
          WHILE (PointLockCount < Length(Point_LockingArray))
          AND (Result = False)
          DO BEGIN
            IF (Point_LockingArray[PointLockCount] = 'S=' + IntToStr(S)) THEN
              Result := True;
            Inc(PointLockCount);
          END; {WHILE}
        END;
      END; {WITH}
    END; { PointIsLockedByASpecificSignal }

  VAR
    ActionCh : Char;
    Device : Integer;
    IndicatorRequested : IndicatorStateType;
    LockListItem : String;
    LockListPos : Word;
    LocoChipStr : String;
    OK : Boolean;
    IndicatorStrPos : integer;
    SignalPutativeStateStr : String;

  BEGIN
    OK := True;
    ActionCh := ' ';
    Device := 99999;
    SignalPutativeStateStr := 'on';
    LocoChipStr := LocoChipToStr(LocoChip);

    IF Length(Signals[S].Signal_RouteLockingNeededArray) <> 0 THEN BEGIN
      { see whether we've pulling the signal off or not }
      IF Signals[S].Signal_Aspect = RedAspect THEN
        SignalPutativeStateStr := 'off';

      IF NOT Signals[S].Signal_FailMsgWritten THEN
        WriteStringArrayToLog(LocoChip, 'S', 'LA for S=' + IntToStr(S) + ': ', LockList, 2, 190);

      LockListPos := 0;
      WHILE OK
      AND (LockListPos <= High(LockList))
      DO BEGIN
        { "L=" is not there for locking, but needed later for route drawing using LockList; HoldMarker is used by the routeing routine }
        IF (Pos('L=', LockList[LockListPos]) > 0)
        OR (LockList[LockListPos] = HoldMarker)
        THEN
          Inc(LockListPos)
        ELSE BEGIN
          { remove the prefix (which is there for FWP's benefit, not the program's) }
          IF (Copy(LockList[LockListPos], 1, 3) = 'FP=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'TP=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'XP=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'TS=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'SR=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'BS=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'TC=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'FS=')
          OR (Copy(LockList[LockListPos], 1, 3) = 'FR=')
          THEN
            LockListItem := Copy(LockList[LockListPos], 4, 255)
          ELSE
            Log(LocoChipStr + ' S! Error in LockList at position ' + IntToStr(LockListPos) + ' : ' + LockList[LockListPos]);

          { Ignore buffer stops - info is there to help debug the list }
          IF (Copy(LockList[LockListPos], 1, 3) <> 'BS=')
          AND (Copy(LockList[LockListPos], 1, 3) <> 'SR=')
          THEN BEGIN
            IF (Copy(LockList[LockListPos], 1, 3) = 'FR=')
            AND (Pos('>', LockList[LockListPos]) > 0)
            THEN BEGIN
              { a theatre indicator }
              ActionCh := '>';
              Device := StrToInt(Copy(LockListItem, 1, Pos('>', LockList[LockListPos]) - 4));
            END ELSE BEGIN
              ActionCh := LockListItem[Length(LockListItem)];
              IndicatorStrPos := Pos('UL', LockListItem);
              IF IndicatorStrPos > 0 THEN
                Delete(LockListItem, IndicatorStrPos, 2)
              ELSE BEGIN
                IndicatorStrPos := Pos('ML', LockListItem);
                IF IndicatorStrPos > 0 THEN
                  Delete(LockListItem, IndicatorStrPos, 2)
                ELSE BEGIN
                  IndicatorStrPos := Pos('LL', LockListItem);
                  IF IndicatorStrPos > 0 THEN
                    Delete(LockListItem, IndicatorStrPos, 2)
                  ELSE BEGIN
                    IndicatorStrPos := Pos('UR', LockListItem);
                    IF IndicatorStrPos > 0 THEN
                      Delete(LockListItem, IndicatorStrPos, 2)
                    ELSE BEGIN
                      IndicatorStrPos := Pos('MR', LockListItem);
                      IF IndicatorStrPos > 0 THEN
                        Delete(LockListItem, IndicatorStrPos, 2)
                      ELSE BEGIN
                        IndicatorStrPos := Pos('LR', LockListItem);
                        IF IndicatorStrPos > 0 THEN
                          Delete(LockListItem, IndicatorStrPos, 2);
                      END;
                    END;
                  END;
                END;
              END;

              Device := StrToInt(Copy(LockListItem, 1, Length(LockListItem) - 1));
            END;

            IF (Device <> S)
            OR ((ActionCh <> '\')
                AND (ActionCh <> '=')
                AND (ActionCh <> '>')
                AND (ActionCh <> '|')
                AND (ActionCh <> '.'))
            THEN BEGIN
              { need to disregard the locking of the calling signal, or we find it can't be unlocked because it's already locked itself! }
              CASE ActionCh OF
                '\', '=', '>':
                  IF (ActionCh = '\') OR (ActionCh = '>') THEN BEGIN
                    IF GetSignalAspect(Device) = RedAspect THEN BEGIN
                      { the next signal - if it should be off, and it's actually on, it's a failure }
                      OK := False;
                      IF NOT Signals[S].Signal_FailMsgWritten THEN
                        Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' off because S=' + IntToStr(Device) + ' is on');
                    END;
                  END ELSE
                    { ActionCh = '=' }
                    IF NOT (GetSignalAspect(Device) = RedAspect) THEN BEGIN
                     { the next signal - if it should be on, and it's actually off, it's a failure }
                      OK := False;
                      IF NOT Signals[S].Signal_FailMsgWritten THEN
                        Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' on because S=' + IntToStr(Device) + ' is off');
                    END;
                '/', '-':
                  { a point }
                  { If the points are unlocked, or if they're locked by our signal, they're ok; they're also ok if they're locked by something else, and they're just an XP
                    [crossing point] as far as our signal is concerned
                  }
                  IF NOT PointIsLockedByASpecificSignal(Device, S) THEN BEGIN
                    IF Points[Device].Point_PresentState = PointStateUnknown THEN BEGIN
                      { the next point - if it should be known, and it's actually unknown, it's a failure }
                      OK := False;
                      IF NOT Signals[S].Signal_FailMsgWritten THEN
                        Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                                        + ' because P=' + IntToStr(Device) + ' is in an unknown state');
                    END ELSE BEGIN
                      IF ActionCh = '/' THEN BEGIN
                        IF Points[Device].Point_PresentState = Straight THEN BEGIN
                          { the next point - if it should be diverging, and it's straight, it's a failure }
                          OK := False;
                          IF NOT Signals[S].Signal_FailMsgWritten THEN
                            Log(LocoChipStr +  ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                                            + ' because P=' + IntToStr(Device) + ' is straight');
                        END;
                      END ELSE BEGIN
                        { ActionCh = '-' }
                        IF Points[Device].Point_PresentState = Diverging THEN BEGIN
                          { the next point - if it should be straight, and it's diverging, it's a failure }
                          OK := False;
                          IF NOT Signals[S].Signal_FailMsgWritten THEN
                            Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                                            + ' because P=' + IntToStr(Device) + ' is diverging');
                        END;
                      END;
                    END;
                    { Now lock them, if ok }
                    IF OK THEN BEGIN
                      LockPointBySignal(Device, S);
                      IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
                        IF Points[Device].Point_PresentState = Straight THEN
                          Log(LocoChipStr + ' S P=' + IntToStr(Device) + ' locked straight by S=' + IntToStr(S))
                        ELSE
                          Log(LocoChipStr + ' S P=' + IntToStr(Device) + ' locked diverging by S=' + IntToStr(S));
                      END;
                    END;
                  END;
                '|', '.':
                  BEGIN
                    IndicatorRequested := Signals[Device].Signal_IndicatorState;
                    { an indicator }
                    IF ActionCh = '|' THEN BEGIN
                      IF IndicatorRequested = NoIndicatorLit THEN BEGIN
                        IF NOT Signals[S].Signal_FailMsgWritten THEN
                          Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                                          + ' because indicator requested not set up');
                        OK := False;
                      END;
                    END ELSE
                      IF ActionCh = '.' THEN BEGIN
                        IF IndicatorRequested <> NoIndicatorLit THEN BEGIN
                          IF NOT Signals[S].Signal_FailMsgWritten THEN
                            Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                                            + ' because a different indicator is set');
                          OK := False;
                        END;
                      END;
                  END;
                '*':
                  { trackcircuit unoccupied - unless it's the one next the signal }
                  IF (TrackCircuits[Device].TC_OccupationState <> TCUnoccupied)
                  AND (Lines[Signals[S].Signal_AdjacentLine].Line_TC <> Device)
                  THEN BEGIN
                    IF NOT Signals[S].Signal_FailMsgWritten THEN
                      Log(LocoChipStr +  ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                                      + ' because TC=' + IntToStr(Device) + ' is occupied');
                    OK := False;
                  END;
              END; {CASE}
            END;
          END;
          Inc(LockListPos);
        END;
      END; {WHILE}

      { write out the first lock failure, or clear it }
      IF NOT OK
      AND ShowError
      AND NOT InAutoMode
      THEN
        DrawFailure(Device, ActionCh);
    END;
    Result := OK;
  END; { SignalLockingOK }

CONST
  EmergencyRouteing = True;
  IncludeOutOfUseLines = True;
  ShowError = True;
  SignalNotPoint = True;
  SuppressMessage = True;
  UnknownTrainLen = 0;
  WriteMessage = True;

VAR
  DebugStr : String;
  DraftRouteArray : StringArrayType;
  ErrorMsg : String;
  I : Integer;
  IndicatorToBeSet : Boolean;
  L : Integer;
  LinesNotAvailableStr : String;
  LocoChipStr : String;
  NewAspect : AspectType;
  NextBufferStop : Integer;
  NextSignal : Integer;
  OtherS : Integer;
  RouteLockingArray : IntegerArrayType;
  SignalPutativeStateStr : String;
  TempDestination : String;
  TempTheatreIndicatorString : String;
  TempS : Integer;

BEGIN
  OK := True;
  NextBufferStop := UnknownBufferStop;
  NextSignal := UnknownSignal;
  IndicatorToBeSet := (NewIndicatorState <> NoIndicatorLit);
  SignalPutativeStateStr := 'on';
  LocoChipStr := LocoChipToStr(LocoChip);

  { See whether we've pulling the signal off or not }
  IF Signals[S].Signal_Aspect = RedAspect THEN
    { this includes semaphore distants that are on }
    SignalPutativeStateStr := 'off';

  IF Signals[S].Signal_OutOfUse THEN BEGIN
    IF NOT Signals[S].Signal_OutOfUseMsgWritten THEN BEGIN
      IF NOT User THEN
        Log('S Cannot change a signal (S=' + IntToStr(S) + ') which is out of use')
      ELSE
        Log('S+ User cannot change a signal (S=' + IntToStr(S) + ') which is out of use');
      Signals[S].Signal_OutOfUseMsgWritten := True;
    END;
    OK := False;
  END ELSE BEGIN
    { Is signal being locked by the user, and it's already locked by a route, or is it locked by any route but the current one? }
    IF LockingMode
    AND (ResetTC = UnknownTrackCircuit)
    AND (((Route = UnknownRoute) AND SignalIsLockedByAnyRoute(S, RouteLockingArray))
         OR (SignalIsLockedByAnyRoute(S, RouteLockingArray)
         AND NOT SignalIsLockedBySpecificRoute(S, Route)))
    THEN BEGIN
      IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
        IF Length(RouteLockingArray) <> 0 THEN BEGIN
          IF NOT User THEN
            DebugStr := 'Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by'
          ELSE
            DebugStr := 'User cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by';
          IF Length(RouteLockingArray) = 1 THEN
            DebugStr := DebugStr + ' R=' + IntToStr(RouteLockingArray[0])
          ELSE
            FOR I := 0 TO High(RouteLockingArray) DO
              DebugStr := DebugStr + ' R=' + IntToStr(RouteLockingArray[I]);

          Log(LocoChipStr + ' R ' + DebugStr);
        END ELSE BEGIN
          IF NOT User THEN
            Log(LocoChipStr + ' RG Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                            + ' as it is locked by R=' + IntToStr(Route) + IfThen(SubRoute <> NoSubRoute,
                                                                                  IntToStr(SubRoute)))
          ELSE
            Log(LocoChipStr + ' RG User cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr
                            + ' as it is locked by R=' + IntToStr(Route) + IfThen(SubRoute <> NoSubRoute,
                                                                                  IntToStr(SubRoute)));
        END;
        Signals[S].Signal_FailMsgWritten := True;
      END;
      OK := False;
    END ELSE BEGIN
      IF LockingMode
      AND SignalIsLockedByOppositePassingLoopSignal(S, OtherS)
      THEN BEGIN
        OK := False;
        IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
          IF NOT User THEN
            Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by opposite passing loop signal S=' + IntToStr(OtherS))
          ELSE
            Log(LocoChipStr + ' S User cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by opposite passing loop signal S=' + IntToStr(OtherS));
          Signals[S].Signal_FailMsgWritten := True;
          Forbid;
        END;
      END ELSE BEGIN
        IF LockingMode
        AND SignalIsLockedByUser(S)
        AND (Route <> UnknownRoute)
        THEN BEGIN
          OK := False;
          IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
            Log(LocoChipStr + ' R R=' + IntToStr(Route) +
              IfThen(SubRoute <> NoSubRoute, IntToStr(SubRoute)) + ' cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by user');
            Signals[S].Signal_FailMsgWritten := True;
            Forbid;
          END;
        END ELSE BEGIN
          { Check that both the signals and the trackcircuit occupation resetting them, if locked, are locked by the same route }
          IF LockingMode
          AND (ResetTC <> UnknownTrackCircuit)
          THEN BEGIN
            IF SignalIsLockedByAnyRoute(S, RouteLockingArray) THEN BEGIN
              IF NOT IsElementInIntegerArray(RouteLockingArray, TrackCircuits[ResetTC].TC_LockedForRoute) THEN BEGIN
                IF NOT User THEN
                  DebugStr := 'Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' by TC=' + IntToStr(ResetTC) + ' as S=' + IntToStr(S) + ' is locked by'
                ELSE
                  DebugStr := 'User cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' by TC=' + IntToStr(ResetTC) + ' as S=' + IntToStr(S) + ' is locked by';
                FOR I := 0 TO High(RouteLockingArray) DO
                  DebugStr := DebugStr + ' R=' + IntToStr(RouteLockingArray[I]);
                IF TrackCircuits[ResetTC].TC_LockedForRoute <> UnknownRoute THEN
                  DebugStr := DebugStr + ' and TC=' + IntToStr(ResetTC) + ' is locked by R=' + IntToStr(TrackCircuits[ResetTC].TC_LockedForRoute);
                Log(LocoChipStr + ' R ' + DebugStr);
              END;
            END;
          END;

          IF Signals[S].Signal_Aspect = RedAspect THEN BEGIN
            { Pull signal off }
            DebugStr := 'Putatively setting S=' + IntToStr(S) + ' ';

            IF NewIndicatorState <> NoIndicatorLit THEN BEGIN
              CASE NewIndicatorState OF
                LeftIndicatorLit:
                  DebugStr := DebugStr + 'left indicator ';
                RightIndicatorLit:
                  DebugStr := DebugStr + 'right indicator ';
                UpperLeftIndicatorLit:
                  DebugStr := DebugStr + 'upper left indicator ';
                MiddleLeftIndicatorLit:
                  DebugStr := DebugStr + 'middle left indicator ';
                LowerLeftIndicatorLit:
                  DebugStr := DebugStr + 'lower left indicator ';
                UpperRightIndicatorLit:
                  DebugStr := DebugStr + 'upper right indicator ';
                MiddleRightIndicatorLit:
                  DebugStr := DebugStr + 'middle right indicator ';
                LowerRightIndicatorLit:
                  DebugStr := DebugStr + 'lower right indicator ';
                TheatreIndicatorLit:
                  BEGIN
                    DebugStr := DebugStr + 'theatre indicator to ';
                    IF GetLineAdjacentSignal(PlatformOrFiddleyardLine) <> UnknownSignal THEN
                      DebugStr := DebugStr + 'S=' + IntToStr(GetLineAdjacentSignal(PlatformOrFiddleyardLine)) + ' '
                    ELSE
                      DebugStr := DebugStr + 'BS=' + IntToStr(Lines[PlatformOrFiddleyardLine].Line_AdjacentBufferStop) + ' ';
                  END;
                QueryIndicatorLit:
                  DebugStr := DebugStr + 'query indicator ';
              END; {CASE}
            END;

            DebugStr := DebugStr + 'off';
            IF Route <> NoRoute THEN
              DebugStr := DebugStr + ' for R=' + IntToStr(Route) + IfThen(SubRoute <> NoSubRoute,
                                                                          '/' + IntToStr(SubRoute));
            IF NOT Signals[S].Signal_FailMsgWritten THEN
              Log(LocoChipStr + ' S ' + DebugStr);
            DebugStr := '';

            IF (Signals[S].Signal_IndicatorState = NoIndicatorLit)
            { need to see if a signal is approach locked here - if it is, the indicator will not (yet) be set }
            AND NOT Signals[S].Signal_ApproachLocked
            THEN BEGIN
              CASE NewIndicatorState OF
                UpperLeftIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_TargetBufferStop;
                MiddleLeftIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_TargetBufferStop;
                LowerLeftIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_TargetBufferStop;
                UpperRightIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_TargetBufferStop;
                MiddleRightIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_TargetBufferStop;
                LowerRightIndicatorLit:
                  IF Signals[S].Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal <> UnknownSignal THEN
                    NextSignal := Signals[S].Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetSignal
                  ELSE
                    NextBufferStop := Signals[S].Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_TargetBufferStop;
              ELSE {CASE}
                NextSignal := UnknownSignal;
              END; {CASE}

              { We need to find the next signal - DraftRouteArray will contain the data from which the signal locking array is created }
              IF FindNextSignalOrBufferStop(S, NextSignal, NextBufferStop, IndicatorToBeSet, LinesNotAvailableStr, DraftRouteArray) THEN BEGIN
                CreateLockingArrayFromDraftRouteArray(LocoChip, DraftRouteArray, Signals[S].Signal_RouteLockingNeededArray);
                Signals[S].Signal_ResettingTC := GetResettingTrackCircuit(LocoChip, S, NOT SuppressMessage);
                FindPreviousSignals(S, Signals[S].Signal_PreviousSignal1, Signals[S].Signal_PreviousSignal2);
                Signals[S].Signal_FindNextSignalBufferStopMsgWritten := False;
              END ELSE BEGIN
                { No signal or buffer stop found - should only reach here if the line is out of use ahead }
                IF NOT Signals[S].Signal_FindNextSignalBufferStopMsgWritten THEN BEGIN
                  Log(LocoChipStr + ' SG Find next signal/bufferstop to S=' + IntToStr(S) + ' failed:');
                  Log(LocoChipStr + ' S ' + LinesNotAvailableStr + ' {WRAP=SCREENWIDTH}');
                  Signals[S].Signal_FindNextSignalBufferStopMsgWritten := True;
                END;
                OK := False;
              END;
            END ELSE BEGIN
              IF Signals[S].Signal_IndicatorState = QueryIndicatorLit THEN BEGIN
                { we need to set a specific route up, not just look for the next signal }
                IF NOT Signals[S].Signal_FailMsgWritten THEN
                  Log(LocoChipStr + ' S Finding a route for theatre indicator for S=' + IntToStr(S));

                { DraftRouteArray will also contain the data from which the signal locking array is created }
                FindRouteFromLineAToLineB(LocoChip, UnknownJourney, S, Signals[S].Signal_AdjacentLine, PlatformOrFiddleyardLine, Signals[S].Signal_Direction, TrainType,
                                          UnknownTrainLength, NOT EmergencyRouteing, NOT IncludeOutOfUseLines, DraftRouteArray, LinesNotAvailableStr, ErrorMsg, OK);
                IF NOT OK THEN BEGIN
                  { try to find a route with emergency routeing }
                  FindRouteFromLineAToLineB(LocoChip, UnknownJourney, S, Signals[S].Signal_AdjacentLine, PlatformOrFiddleyardLine, Signals[S].Signal_Direction, TrainType,
                                            UnknownTrainLength, EmergencyRouteing, NOT IncludeOutOfUseLines, DraftRouteArray, LinesNotAvailableStr, ErrorMsg, OK);
                  IF NOT OK
                  AND NOT FindARouteFailMsgWritten
                  THEN BEGIN
                    FindARouteFailMsgWritten := True;
                    Log(LocoChipStr + ' SG Find next signal to S=' + IntToStr(S) + ' failed in FindARoute:');
                    Log(LocoChipStr + ' S ' + LinesNotAvailableStr + ' (' + ErrorMsg + ')');
                  END;
                END;

                IF OK THEN BEGIN
                  L := ExtractLineFromString(DraftRouteArray[High(DraftRouteArray)]);
                  IF L <> UnknownLine THEN
                    IF GetLineAdjacentSignal(L) <> UnknownSignal THEN
                      AppendToStringArray(DraftRouteArray, 'FS=' + IntToStr(GetLineAdjacentSignal(L)));

                  IF Length(Signals[S].Signal_RouteLockingNeededArray) = 0 THEN
                    CreateLockingArrayFromDraftRouteArray(LocoChip, DraftRouteArray, Signals[S].Signal_RouteLockingNeededArray);

                  Signals[S].Signal_ResettingTC := GetResettingTrackCircuit(LocoChip, S, NOT SuppressMessage);
                  FindPreviousSignals(S, Signals[S].Signal_PreviousSignal1, Signals[S].Signal_PreviousSignal2);

                  { and extract the theatre indicator text }
                  IF Signals[S].Signal_RouteLockingNeededArray[1] = HoldMarker THEN
                    { the 'HOLD' marker occupies the second position in the string }
                    TempDestination := Copy(Signals[S].Signal_RouteLockingNeededArray[2], Pos('>', Signals[S].Signal_RouteLockingNeededArray[2]) + 1, 255)
                  ELSE
                    TempDestination := Copy(Signals[S].Signal_RouteLockingNeededArray[1], Pos('>', Signals[S].Signal_RouteLockingNeededArray[1]) + 1, 255);

                  IF Pos('FS=', TempDestination) > 0 THEN BEGIN
                    TempTheatreIndicatorString := Signals[ExtractSignalFromString(TempDestination)].Signal_AsTheatreDestination;
                    Log(LocoChipStr + ' R S=' + IntToStr(S) + ' theatre indicator ''' + TempTheatreIndicatorString
                                    + ''' set from Signal_AsTheatreDestination field in signal record for S=' + IntToStr(ExtractSignalFromString(TempDestination)));
                  END ELSE BEGIN
                    IF ExtractBufferStopFromString(TempDestination) <> UnknownBufferStop THEN
                      TempTheatreIndicatorString := BufferStops[ExtractBufferStopFromString(TempDestination)].BufferStop_AsTheatreDestination;
                    Log(LocoChipStr + ' R S=' + IntToStr(S) + ' theatre indicator ''' + TempTheatreIndicatorString
                                    + ''' set from BufferStop_AsTheatreDestination field in buffer stop record for BS='
                                    + IntToStr(ExtractBufferStopFromString(TempDestination)));
                  END;
                END;
              END ELSE
                FindARouteFailMsgWritten := False;
            END;
          END ELSE BEGIN
            { Aspect <> RedAspect : push signal on }
            IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
              IF NOT User THEN
                DebugStr := 'Setting S=' + IntToStr(S) + ' on'
              ELSE
                DebugStr := 'User setting S=' + IntToStr(S) + ' on';
              IF Route <> NoRoute THEN
                DebugStr := DebugStr + ' for R=' + IntToStr(Route);
              Log(LocoChipStr + ' S ' + DebugStr);
            END;

            { make sure the preceding signal is not off - if it is, we can't set the current one on }
            IF Signals[S].Signal_PreviousSignal1 <> UnknownSignal THEN BEGIN
              SetLength(Signals[S].Signal_RouteLockingNeededArray, 1);
              Signals[S].Signal_RouteLockingNeededArray[0] := 'FS=' + IntToStr(Signals[S].Signal_PreviousSignal1) + '=';
            END ELSE BEGIN
              { it's ok }
              SetLength(Signals[S].Signal_RouteLockingNeededArray, 0);
              Signals[S].Signal_TheatreIndicatorString := '';
            END;
          END;

          IF OK
          AND LockingMode
          THEN BEGIN
            { If there's nothing in the locking array, it's ok, otherwise test the locking }
            OK := SignalLockingOK(LocoChip, S, Signals[S].Signal_RouteLockingNeededArray, ShowError);
            IF OK
            AND (Signals[S].Signal_Aspect = RedAspect)
            THEN BEGIN
              Signals[S].Signal_StateChanged := True;
              IF Route = UnknownRoute THEN
                LockSignalByUser(S)
              ELSE
                LockSignalByRoute(LocoChip, S, Route, NOT WriteMessage);
            END;
          END;
        END;

        { Check that the previous signal is not a theatre in the process of being set up - a bit esoteric, this check, but necessary, as the previous signal could not
          otherwise be completely pulled off
        }
        IF OK
        AND LockingMode
        THEN BEGIN
          IF (Signals[S].Signal_PreviousSignal1 <> UnknownSignal)
          AND (Signals[Signals[S].Signal_PreviousSignal1].Signal_Aspect = RedAspect)
          AND (Signals[Signals[S].Signal_PreviousSignal1].Signal_Indicator = TheatreIndicator)
          AND (Signals[Signals[S].Signal_PreviousSignal1].Signal_IndicatorState = TheatreIndicatorLit)
          AND (Signals[Signals[S].Signal_PreviousSignal1].Signal_TheatreIndicatorString = Signals[S].Signal_AsTheatreDestination)
          THEN BEGIN
            IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
              IF NOT User THEN
                Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' as previous signal ' + IntToStr(Signals[S].Signal_PreviousSignal1)
                                + '''s theatre indicator is on but the signal is not yet off')
              ELSE
                Log(LocoChipStr + ' S User cannot set S=' + IntToStr(S) + ' as previous signal ' + IntToStr(Signals[S].Signal_PreviousSignal1)
                                + '''s theatre indicator is on but the signal is not yet off');
            END;
            OK := False;
            DrawFailure(Signals[S].Signal_PreviousSignal1, 'T');
          END;
        END;

        IF NOT OK OR (Signals[S].Signal_Aspect <> RedAspect) THEN BEGIN
          { if the theatre indicator was set to query, set it back on }
          IF (Signals[S].Signal_IndicatorState = QueryIndicatorLit) THEN BEGIN
            SetIndicator(LocoChip, S, NoIndicatorLit, '', Route, User);
            IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
              IF NOT User THEN
                Log(LocoChipStr + ' S Turning off theatre query indication for S=' + IntToStr(S))
              ELSE
                Log(LocoChipStr + ' S User turning off theatre query indication for S=' + IntToStr(S));
            END;
          END;

          { and unlock any points we previously locked, either because the signal setting has failed, or because we're unsetting it }
          IF NOT OK
          AND NOT Signals[S].Signal_FailMsgWritten
          THEN BEGIN
            Signals[S].Signal_FailMsgWritten := True;
            IF Signals[S].Signal_Aspect <> RedASpect THEN BEGIN
              IF NOT User THEN
                DebugStr := 'Setting S=' + IntToStr(S) + ' on failed'
              ELSE
                DebugStr := 'User setting S=' + IntToStr(S) + ' on failed'
            END ELSE BEGIN
              IF NOT User THEN
                DebugStr := 'Setting S=' + IntToStr(S) + ' off failed'
              ELSE
                DebugStr := 'User setting S=' + IntToStr(S) + ' off failed';
            END;
            IF Route <> NoRoute THEN
              DebugStr := DebugStr + ' for R=' + IntToStr(Route);
            Log(LocoChipStr + ' S ' + DebugStr)
          END;

          UnlockPointsLockedBySignal(S);
          UnlockSignalLockedByUser(S);
        END;

        IF NOT OK
        AND LockingMode
        THEN BEGIN
          SetLength(Signals[S].Signal_RouteLockingNeededArray, 0);
          IF (Route <> UnknownRoute)
          AND Routes_RouteSettingsInProgress[Route]
          THEN BEGIN
            Signals[S].Signal_StateChanged := False;
            Forbid;
          END;
        END ELSE BEGIN
          { If we're route-setting, and approach control mode has been set, store the signals and don't set them yet }
          IF (Route <> UnknownRoute)
          AND Routes_RouteSettingsInProgress[Route]
          AND Routes_ApproachControlsSet[Route]
          THEN BEGIN
            Log(LocoChipStr + ' R ' + SettingString + ' added to approach setting signals list for R=' + IntToStr(Route) + ' and not yet set off');
            AppendToStringArray(Routes_ApproachControlSignalsWaitingToBeSet[Route], SettingString);
            Signals[S].Signal_ApproachLocked := True;
            DrawSignalPost(S);
            WriteStringArrayToLog(Routes_LocoChips[Route], 'R', 'Signals held by approach control for R=' + IntToStr(Route) + ':',
                                                                Routes_ApproachControlSignalsWaitingToBeSet[Route]);
          END ELSE BEGIN
            { Either deal with route and theatre setting ... }
            IF NewIndicatorState <> NoIndicatorLit THEN BEGIN
              IF IndicatorToBeSet THEN
                SetIndicator(LocoChip, S, NewIndicatorState, TempTheatreIndicatorString, Route, user)
              ELSE
                SetIndicator(LocoChip, S, NoIndicatorLit, '', Route, User);
            { ... or with query theatre indicator setting ... }
            END ELSE BEGIN
              IF (Signals[S].Signal_IndicatorState = QueryIndicatorLit) THEN BEGIN
                SetIndicator(LocoChip, S, NewIndicatorState, TempTheatreIndicatorString, Route, user)
              END ELSE BEGIN
                { ... or with signal setting - if no particular aspect selected already, these are the defaults }
                IF Signals[S].Signal_Aspect = RedAspect THEN BEGIN
                  IF (Signals[S].Signal_Type = TwoAspect)
                  THEN
                    NewAspect := GreenAspect
                  ELSE
                    NewAspect := SingleYellowAspect;
                END ELSE
                  NewAspect := RedAspect;

                SetSignal(LocoChip, S, NewAspect, NOT LogSignalData, NOT ForceAWrite);
                IF NOT User THEN
                  DebugStr := 'S=' + IntToStr(S) + ' successfully set to ' + AspectToStr(Signals[S].Signal_Aspect)
                ELSE
                  DebugStr := 'S=' + IntToStr(S) + ' User successfully set to ' + AspectToStr(Signals[S].Signal_Aspect);
                IF Route <> NoRoute THEN
                  DebugStr := DebugStr + ' for R=' + IntToStr(Route);
                Log(LocoChipStr + ' S ' + DebugStr);

                { reset several things }
                Signals[S].Signal_FailMsgWritten := False;
                SetLength(Signals[S].Signal_RouteLockingNeededArray, 0);

                IF (Signals[S].Signal_PreviousSignal1 <> UnknownSignal)
                AND (Signals[S].Signal_Type <> SemaphoreDistant)
                THEN
                  SetPreviousSignals(LocoChip, S);

                { If the signal is being reset to on, reset the other data too }
                IF NewAspect = RedAspect THEN BEGIN
                  IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
                    SetIndicator(LocoChip, S, NoIndicatorLit, '', Route, user);
                  Signals[S].Signal_PreviousSignal1 := UnknownSignal;
                  Signals[S].Signal_PreviousSignal2 := UnknownSignal;

                  { and see if it affects any other signal's previous signals - otherwise, a previous signal may be reset, then be set to off for a different route, and our
                    signal would treat the previous signal as still being off, and wouldn't reset.
                  }
                  FOR TempS := 0 TO High(Signals) DO BEGIN
                    IF Signals[TempS].Signal_PreviousSignal1 = S THEN
                      Signals[TempS].Signal_PreviousSignal1 := UnknownSignal;
                    IF Signals[TempS].Signal_PreviousSignal2 = S THEN
                      Signals[TempS].Signal_PreviousSignal2 := UnknownSignal;
                  END; {FOR}
                END;
              END;
            END;
          END;
        END;
      END;
    END;
  END;
END; { PullSignalMainProcedure }

PROCEDURE PullSignal{1}(LocoChip, S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer; SettingString : String;
                        User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }
CONST
  ResetTC = UnknownTrackCircuit;

BEGIN
  PullSignalMainProcedure(LocoChip, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, SettingString, ResetTC, UnknownTrainType, User, OK);
END; { PullSignal-1 }

PROCEDURE PullSignal{2}(LocoChip, S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }
CONST
  NoSettingString = '';
  ResetTC = UnknownTrackCircuit;

BEGIN
  PullSignalMainProcedure(LocoChip, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, NoSettingString, ResetTC, TrainTypeForRouteing, User, OK);
END; { PullSignal-2 }

PROCEDURE PullSignal{3}(LocoChip, S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer; ResetTC : Integer;
                        User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal. This version is only used by signals resetting trackcircuits, which can happen even if the signal is locked by a route }
CONST
  NoSettingString = '';

BEGIN
  PullSignalMainProcedure(LocoChip, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, NoSettingString, ResetTC, UnknownTrainType, User, OK);
END; { PullSignal-3 }

PROCEDURE PullSignal{4}(LocoChip, S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        SettingString : String; TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal; includes the original setting string for saving if necessary }
CONST
  ResetTC = UnknownTrackCircuit;

BEGIN
  PullSignalMainProcedure(LocoChip, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, SettingString, ResetTC, TrainTypeForRouteing, User, OK);
END; { PullSignal-4 }

PROCEDURE PullPoint{1}(P, LocoChip : Integer; Route, SubRoute : Integer; ForcePoint, User, ErrorMessageRequired : Boolean; OUT PointResultPending : Boolean;
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
  LocoChipStr : String;
  NewDirection : PointStateType;
//  PointDelayInMSS : Integer;

BEGIN
  LocoChipStr := LocoChipToStr(LocoChip);

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
      IF (LockingMode
      AND PointIsLocked(P, LockingFailureString)) AND NOT ForcePoint
      THEN BEGIN
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
        IF InAutoMode
        AND (Route <> UnknownRoute)
        AND LockingMode
        AND NOT Point_SecondAttempt
        AND (IncSecond(Point_LastChangedTime, 10) > Time)
        THEN BEGIN
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

        IF Point_AwaitingManualChange
        AND NOT Point_FeedbackPending
        THEN BEGIN
          Log(LocoChipStr + ' P P=' + IntToStr(P) + ' manual change awaited');
          Debug('P=' + IntToStr(P) + ' manual change awaited');
          PointChangedOK := False;
        END ELSE BEGIN
          IF Point_FeedbackPending
          AND NOT ForcePoint
          THEN BEGIN
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
                IF Point_ManualOperation
                AND NOT ForcePoint
                THEN BEGIN
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
                    PointChangedOK := MakePointChange(LocoChip, P, NewDirection, Count);
                    IF PointChangedOK THEN BEGIN
                      { Point_PresentState is set by the point feedback detector reporting in }
                      IF Point_RequiredState = Point_PresentState THEN BEGIN
                        Point_SecondAttempt := False;
                        DebugStr := 'Successfully changed P=' + IntToStr(P) + ' to ' + PointStateToStr(Point_RequiredState);
                        IF User THEN
                          DebugStr := DebugStr + ' by user';

                        IF RecordLineDrawingMode THEN BEGIN
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

                    IF PointChangedOK
                    AND ForcePoint
                    THEN BEGIN
                      { can only be done by the user }
                      DebugStr := 'User forced P=' + IntToStr(P) + ' to change successfully to ' + PointStateToStr(NewDirection);
                      IF SubRoute <> UnknownRoute THEN
                        DebugStr := DebugStr + ' for SR=' + IntToStr(SubRoute);
                    END;
                  END ELSE
                    { Points without feedback dealt with here }
                    IF NOT Point_ManualOperation THEN BEGIN
                      PointChangedOK := MakePointChange(LocoChip, P, NewDirection, Count);
                      IF PointChangedOK THEN BEGIN
                        { if no feedback, do it another time, to be sure it changed }
                        Point_SetASecondTime := True;

                        PointChangedOK := MakePointChange(LocoChip, P, NewDirection, Count);
                        IF PointChangedOK
                        AND NOT ForcePoint
                        THEN BEGIN
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

                IF RecordLineDrawingMode THEN BEGIN
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
          IF NOT Point_AwaitingManualChange
          AND SystemOnline AND NOT PointChangedOK
          THEN
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
  PullPoint(P, NoLocoChip, NoRoute, NoSubRoute, ForcePoint, ByUser, NOT ErrorMessageRequired, PointResultPending, DebugStr, OK);
END; { PullPoint-2 }

PROCEDURE CheckRouteAheadLocking(T : TrainElement; RouteArray : StringArrayType; OUT RouteCurrentlyLocked, RoutePermanentlyLocked : Boolean; OUT LockingMsg : String);
{ Tests a given route array to see if anything on it is locked. The test stops at any subsequent signal which is a route holding signal though. }

  FUNCTION PointIsLockedByAnySignal(P : Integer; OUT SignalLockingArray : IntegerArrayType) : Boolean;
  { Returns true of the point is locked by any signal }
  VAR
    PointLockCount : Integer;

  BEGIN
    Result := False;
    WITH Points[P] DO BEGIN
      PointLockCount := 0;
      WHILE (PointLockCount < Length(Point_LockingArray)) DO BEGIN
        IF Pos('S=', Point_LockingArray[PointLockCount]) > 0 THEN BEGIN
          AppendToIntegerArray(SignalLockingArray, ExtractSignalFromString(Point_LockingArray[PointLockCount]));
          Result := True;
        END;
        Inc(PointLockCount);
      END; {WHILE}
    END; {WITH}
  END; { PointIsLockedByAnySignal }

  FUNCTION TrackCircuitLocked(LocoChip, TC : Integer; OUT LockingMsg : String) : Boolean;
  { Returns true if the trackcircuit is locked by anything other than the given loco }
  BEGIN
    Result := False;
    LockingMsg := 'not locked';

    IF TrackCircuits[TC].TC_LockedForRoute <> UnknownRoute THEN BEGIN
      IF LocoChip <> Routes_LocoChips[TrackCircuits[TC].TC_LockedForRoute] THEN BEGIN
        { ignore locking by the supplied loco }
        Result := True;
        LockingMsg := 'locked by ' + LocoChipToStr(Routes_LocoChips[TrackCircuits[TC].TC_LockedForRoute]) + ' for R=' + IntToStr(TrackCircuits[TC].TC_LockedForRoute);
      END;
    END;
  END; { TrackCircuitLocked }

VAR
  FirstRouteHoldSignalFound : Boolean;
  I : Integer;
  L : Integer;
  P : Integer;
  RouteLockingArray : IntegerArrayType;
  S : Integer;
  SecondRouteHoldSignalFound : Boolean;
  SignalLockingArray : IntegerArrayType;
  TC : Integer;
  TempRouteArray : StringArrayType;

BEGIN
  FirstRouteHoldSignalFound := False;
  SecondRouteHoldSignalFound := False;
  RouteCurrentlyLocked := False;
  RoutePermanentlyLocked := False;

  SetLength(TempRouteArray, 0);
  I := 0;
  WHILE (I <= High(RouteArray))
  AND NOT SecondRouteHoldSignalFound
  DO BEGIN
    { Look out for subsequent route holding signals, as it doesn't matter if the route is locked beyond one of those }
    S := ExtractSignalFromString(RouteArray[I]);
    IF S <> UnknownSignal THEN BEGIN
      IF Signals[S].Signal_PossibleRouteHold THEN BEGIN
        IF NOT FirstRouteHoldSignalFound THEN
          FirstRouteHoldSignalFound := True
        ELSE
          SecondRouteHoldSignalFound := True;
      END;
    END;

    IF NOT SecondRouteHoldSignalFound THEN BEGIN
      SetLength(TempRouteArray, Length(TempRouteArray) + 1);
      TempRouteArray[High(TempRouteArray)] := RouteArray[I];
      Inc(I)
    END;
  END;

  I := 0;
  WHILE (I <= High(TempRouteArray))
  AND NOT RouteCurrentlyLocked
  AND NOT RoutePermanentlyLocked
  DO BEGIN
    { Test each element in turn, having first seen what kind of element it is }
    S := ExtractSignalFromString(TempRouteArray[I]);
    { signals which are to stay on and are already locked on are ok }
    IF (S <> UnknownSignal)
    AND (ExtractSignalStateFromString(TempRouteArray[I]) <> SignalOn)
    THEN BEGIN
      IF SignalIsLocked(S, LockingMsg) THEN BEGIN
        LockingMsg := 'S=' + IntToStr(S) + ' (' + LockingMsg + ')';
        RouteCurrentlyLocked := True;
      END;
    END ELSE BEGIN
      P := ExtractPointFromString(TempRouteArray[I]);
      IF (P <> UnknownPoint)
      AND (Points[P].Point_PresentState <> ExtractPointStateFromString(TempRouteArray[I]))
      THEN BEGIN
        { we need to deal with three way points specially here - one half of the point is normally locked if the other half is diverging, but for the purpose of checking
          whether it can be changed if the route is set up in due course, the test is really whether the other half is locked - because if so, the half we're testing can't
          be moved. If the other half is not actually locked, then our half can be moved if we first move the other half. (The usual point lock test doesn't check this, and
          thus three way points are always returned as locked if called by this subroutine).
        }
        IF Points[P].Point_Type = ThreeWayPointA THEN BEGIN
          IF PointIsLocked(Points[P].Point_OtherPoint, LockingMsg) THEN
            { check that the point B isn't just locked by point A }
            IF PointIsLockedByAnySignal(Points[P].Point_OtherPoint, SignalLockingArray) OR PointIsLockedByAnyRoute(P, RouteLockingArray)
            THEN
              RouteCurrentlyLocked := True;
        END ELSE
          IF Points[P].Point_Type = ThreeWayPointB THEN BEGIN
            IF PointIsLockedByAnySignal(P, SignalLockingArray) OR PointIsLockedByAnyRoute(P, RouteLockingArray) THEN
              RouteCurrentlyLocked := True;
          END ELSE
            IF (Points[P].Point_Type = CatchPointUp) OR (Points[P].Point_Type = CatchPointDown) THEN BEGIN
              { the problem described above also applies to catch points: the test is whether the catch point is locked other than by the point it is protecting. }
              IF PointIsLocked(P, LockingMsg) THEN
                IF PointIsLockedByAnySignal(P, SignalLockingArray) OR PointIsLockedByAnyRoute(P, RouteLockingArray) THEN
                  RouteCurrentlyLocked := True;
            END ELSE
              IF PointIsLocked(P, LockingMsg) THEN BEGIN
                LockingMsg := 'P=' + IntToStr(P) + ' (' + LockingMsg + ')';
                RouteCurrentlyLocked := True;
              END;
      END ELSE BEGIN
        L := ExtractLineFromString(TempRouteArray[I]);
        IF L <> UnknownLine THEN BEGIN
          TC := Lines[L].Line_TC;
          IF TC <> UnknownTrackCircuit THEN BEGIN
            IF TrackCircuitLocked(Trains[T].Train_LocoChip, TC, LockingMsg) THEN BEGIN
              LockingMsg := 'TC=' + IntToStr(TC) + ' (' + LockingMsg + ')';
              RouteCurrentlyLocked := True;
            END ELSE
              IF (TrackCircuits[TC].TC_OccupationState <> TCUnoccupied)
              AND (TrackCircuits[TC].TC_LocoChip <> Trains[T].Train_LocoChip)
              THEN BEGIN
                IF (TrackCircuits[TC].TC_OccupationState = TCPermanentFeedbackOccupation)
                OR (TrackCircuits[TC].TC_OccupationState = TCPermanentOccupationSetByUser)
                OR (TrackCircuits[TC].TC_OccupationState = TCPermanentSystemOccupation)
                OR (TrackCircuits[TC].TC_OccupationState = TCOutOfUseSetByUser)
                OR (TrackCircuits[TC].TC_OccupationState = TCOutOfUseAsNoFeedbackReceived)
                THEN BEGIN
                  RoutePermanentlyLocked := True;
                  LockingMsg := 'TC=' + IntToStr(TC) + ' locked (' + TrackCircuitStateToStr(TrackCircuits[TC].TC_OccupationState) + ')';
                END ELSE
                  IF TrackCircuits[TC].TC_OccupationState = TCFeedbackOccupation THEN BEGIN
                    RouteCurrentlyLocked := True;
                    LockingMsg := 'TC=' + IntToStr(TC) + ' locked (' + TrackCircuitStateToStr(TrackCircuits[TC].TC_OccupationState) + ')';
                  END;
              END;
          END;
        END;
      END;
    END;
    Inc(I);
  END; {WHILE}

  IF RouteCurrentlyLocked OR RoutePermanentlyLocked THEN
    Trains[T].Train_LastRouteLockedMsgStr := LockingMsg;
END; { CheckRouteAheadLocking }

FUNCTION RouteAheadOutOfUse(RouteArray : StringArrayType; OUT LockingMsg : String) : Boolean;
{ Tests a given route locking array to see if anything on it is out of use }
VAR
  I : Integer;
  L : Integer;

BEGIN
  I := 0;
  Result := False;
  WHILE (I <= High(RouteArray))
  AND (Result <> True)
  DO BEGIN
    L := ExtractLineFromString(RouteArray[I]);
    IF L <> UnknownLine THEN BEGIN
      IF Lines[L].Line_TC <> UnknownTrackCircuit THEN BEGIN
        IF (TrackCircuits[Lines[L].Line_TC].TC_OccupationState = TCOutOfUseSetByUser)
        OR (TrackCircuits[Lines[L].Line_TC].TC_OccupationState = TCOutOfUseAsNoFeedbackReceived)
        THEN BEGIN
          Result := True;
          LockingMsg := 'TC=' + IntToStr(Lines[L].Line_TC) + ' is out of use';
        END;
      END;
    END;
    Inc(I);
  END; {WHILE}
END; { RouteAheadOutOfUse }

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
                    + IfThen(Routes_LocoChips[J] <> NoLocoChip,
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
