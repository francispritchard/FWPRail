UNIT PointsUnit;
{ Controls the various pointss and point-related things

  Copyright © F.W. Pritchard 2015. All Rights Reserved.

  v0.1  08/02/15 Code mainly extracted from Main and Locks
}
INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Initvars;

TYPE
  TPointForm = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

  PointStateType = (Diverging, Straight, PointOutOfAction, PointStateUnknown);
  TypeOfPoint = (OrdinaryPoint, CrossOverPoint, ThreeWayPointA, ThreeWayPointB, SingleSlip, DoubleSlip, ProtectedPoint, CatchPointUp, CatchPointDown, PointTypeUnknown);

PROCEDURE AddNewRecordToPointDatabase;
{ Append a record to the point database }

PROCEDURE CalculatePointPositions;
{ Create where the points are on the screen }

PROCEDURE CheckPointsAwaitingFeedback;
{ See if any point changes are pending - i.e. we're waiting for feedback that confirms the change }

FUNCTION DeleteRecordFromPointDatabase(PointToDeleteNum : Integer) : Boolean;
{ Remove a record from the point database }

PROCEDURE DisplayPreviousPointSettings;
{ Display the previous settings - generally used when starting up offline }

PROCEDURE FindNextPoint(TC : Integer; SearchDirection : DirectionType; OUT NextPoint : Integer);
{ Work out which is the next point }

PROCEDURE InitialisePointVariables(P : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }

FUNCTION IsPointInStringArray(StringArray : StringArrayType; Point : Integer) : Boolean;
{ Returns whether the given point is found in a string array }

PROCEDURE LockPointByRoute(LocoChip, P, Route : Integer; DoNotWriteMessage : Boolean);
{ Mark the point as locked by a specific route }

FUNCTION PointIsCatchPoint(P : Integer) : Boolean;
{ Returns whether a given point is a catch point }

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

PROCEDURE ReadInPointDataFromDatabase;
{ Create all the points }

PROCEDURE UnlockPointsLockedBySignal(S : Integer);
{ Remove the locking }

PROCEDURE UnlockPointLockedBySpecificRoute(P, Route : Integer; DoNotWriteMessage : Boolean);
{ Remove the locking }

FUNCTION ValidatePointDefaultState(NewStateStr : String; HeelLine, StraightLine, DivergingLine : Integer; OUT PresentState : PointStateType;
                                   OUT ErrorMsg : String) : PointStateType;
{ Check whether the point state supplied is valid }

FUNCTION ValidatePointDivergingLineName(LineName : String; PointType : TypeOfPoint; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Diverging Line name is valid }

FUNCTION ValidatePointHeelLineName(LineName : String; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Heel Line name is valid }

FUNCTION ValidatePointLastFeedbackStateAsReadIn(PointStateStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : PointStateType;
{ Check whether the last point state read in is valid }

FUNCTION ValidatePointLastManualStateAsReadIn(PointStateStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : PointStateType;
{ Check whether the last point state read in is valid }

FUNCTION ValidatePointLenzNum(LenzNumStr : String; PointLastManualStateAsReadIn : PointStateType; OUT PointManualOperation : Boolean;
                              OUT PointPresentState : PointStateType; OUT ErrorMsg : String) : Integer;
{ Check whether the Lenz point number is valid }

FUNCTION ValidatePointLenzUnit(LenzUnitStr : String; OUT ErrorMsg : String) : Integer;
{ Check whether a Lenz point unit is valid }

FUNCTION ValidatePointLenzUnitType(LenzUnitTypeStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : String;
{ Check whether a Lenz point unit type is valid }

FUNCTION ValidatePointRelatedPoint(P : Integer; RelatedPointStr : String; PointType : TypeOfPoint; OUT ErrorMsg : String) : Integer;
{ Check whether the value of the connected point (if any) is valid }

FUNCTION ValidatePointStraightLineName(LineName : String; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Straight Line name is valid }

FUNCTION ValidatePointType(PointTypeStr : String; OUT ErrorMsg : String) : TypeOfPoint;
{ Check that the supplied point type is valid }

PROCEDURE WriteOutPointDataToDatabase;
{ If a point's data has been changed, record it in the database }

TYPE
  PointRec = RECORD
    Point_AwaitingManualChange : Boolean;
    Point_DataChanged : Boolean;
    Point_DefaultState : PointStateType;
    Point_DivergingLine : Integer;
    Point_Energised : Boolean;
    Point_EnergisedTime : TDateTime;
    Point_FacingDirection : DirectionType;
    Point_FarX : Integer; { Position of other ends }
    Point_FarY : Integer; { Position of other ends }
    Point_FeedbackOnIsStraight : Boolean;
    Point_FeedbackPending : Boolean;
    Point_FeedbackPendingMsgWritten : Boolean;
    Point_FeedbackStartTime : TDateTime;
    Point_FeedbackUnit : Integer;
    Point_FeedbackInput : Integer;
    Point_ForcedDelayMsg1Written : Boolean;
    Point_ForcedDelayMsg2Written : Boolean;
    Point_HasFeedback : Boolean;
    Point_HeelLine : Integer;
    Point_LastChangedTime : TDateTime;
    Point_LastFeedbackStateAsReadIn : PointStateType;
    Point_LastManualStateAsReadIn : PointStateType;
    Point_LenzNum : Integer;
    Point_LenzUnit : Integer;
    Point_LenzUnitType : String;
    Point_LockedByUser : Boolean;
    Point_LockedIfHeelTCOccupied : Boolean;
    Point_LockedIfNonHeelTCsOccupied : Boolean;
    Point_LockFailureNotedInLocksUnit : Boolean;
    Point_LockFailureNotedInSubRouteUnit : Boolean;
    Point_LockingArray : StringArrayType;
    Point_LockingState : PointStateType; { used to detect points that have moved while locked }
    Point_LocoChipLockingTheRoute : Integer;
    Point_ManualOperation : Boolean;
    Point_MaybeBeingSetToManual : Boolean;
    Point_MouseRect : TRect; { mouse access rectangle }
    Point_MovedWhenLocked : Boolean;
    Point_Notes : String;
    Point_Number : Integer;
    Point_OutOfUse : Boolean;
    Point_PresentState : PointStateType;
    Point_PreviousState : PointStateType;
    Point_RelatedPoint : Integer;
    Point_RequiredState : PointStateType;
    Point_ResettingTime : TDateTime;
    Point_RouteLockedByLocoChip : Integer;
    Point_StraightLine : Integer;
    Point_TCAtHeel : Integer;
    Point_Type : TypeOfPoint;
    Point_SecondAttempt : Boolean;
    Point_SetASecondTime : Boolean;
    Point_WaitTime : TDateTime;
    Point_WiringReversedFlag : Boolean;
    Point_X : Integer; { position of common point }
    Point_Y : Integer; { position of common point }
  END;

CONST
  Point_DefaultStateFieldName : String = 'Default State';
  Point_DivergingLineFieldName : String = 'Diverging Line';
  Point_FeedbackInputFieldName : String = 'Feedback Input';
  Point_FeedbackOnIsStraightFieldName : String = 'On Is Straight';
  Point_FeedbackUnitFieldName : String = 'Feedback Unit';
  Point_HeelLineFieldName : String = 'Heel Line';
  Point_LastFeedbackStateAsReadInFieldName : String = 'Last Feedback State';
  Point_LastManualStateAsReadInFieldName : String = 'Last Manual State';
  Point_LenzNumFieldName : String = 'Lenz Point Number';
  Point_LenzUnitFieldName : String = 'Lenz Point Unit';
  Point_LenzUnitTypeFieldName : String = 'Lenz Point Unit Type';
  Point_LockedByUserFieldName : String = 'Locked By User';
  Point_LockedIfHeelTCOccupiedFieldName : String = 'Locked If Heel TC Occupied';
  Point_LockedIfNonHeelTCsOccupiedFieldName : String = 'Locked If Non-Heel TCs Occupied';
  Point_ManualOperationFieldName : String = 'Manual Operation';
  Point_NotesFieldName : String = 'Notes';
  Point_NumberFieldName : String = 'Point Number';
  Point_OutOfUseFieldName : String = 'Out Of Use';
  Point_RelatedPointFieldName : String = 'Related Point';
  Point_StraightLineFieldName : String = 'Straight Line';
  Point_TypeFieldName : String = 'Point Type';
  Point_WiringReversedFlagFieldName : String = 'Wiring Reversed';

VAR
  PointForm: TPointForm;
  Points : ARRAY OF PointRec;

IMPLEMENTATION

{$R *.dfm}

USES Route, FWPShowMessageUnit, AnsiStrings, MiscUtils, Locks, DateUtils, Lenz, RailDraw, Main, LinesUnit, Options, Data.DB, StrUtils, TrackCircuitsUnit, SignalsUnit,
     CreateRoute, Feedback, Train, Diagrams, Logging;

CONST
  UnitRef = 'PointUnit';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION IsPointInStringArray(StringArray : StringArrayType; Point : Integer) : Boolean;
{ Returns whether the given point is found in a string array }
VAR
  I : Integer;

BEGIN
  I := 0;
  Result := False;
  WHILE (I <= High(StringArray)) AND (Result = False) DO BEGIN
    IF (Point <> UnknownPoint) AND (Point = ExtractPointFromString(StringArray[I])) THEN
      Result := True
    ELSE
      Inc(I);
  END; {WHILE}
END; { IsPointInStringArray }

PROCEDURE DisplayPreviousPointSettings;
{ Displays on screen the previous settings - generally used when starting up offline }
VAR
  P : Integer;

BEGIN
  IF SystemOnline THEN BEGIN
    IF MessageDialogueWithDefault('Display previous point settings even though the system is online?', NOT StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
    THEN BEGIN
      Debug('Cannot display previous point settings if system online');
      Exit;
    END;
  END;

  FOR P := 0 TO High(Points) DO BEGIN
    IF Points[P].Point_ManualOperation THEN
      Points[P].Point_PresentState := Points[P].Point_LastManualStateAsReadIn
    ELSE
      Points[P].Point_PresentState := Points[P].Point_LastFeedbackStateAsReadIn;
  END; {FOR}
  Debug('Previous point settings displayed');

  InvalidateScreen(UnitRef, 'Display latest point settings in offline mode');
END; { DisplayPreviousPointSettings }

PROCEDURE CheckPointsAwaitingFeedback;
{ See if any point changes are pending - i.e. we're waiting for feedback that confirms the change }
CONST
  ErrorMessageRequired = True;

VAR
  DebugStr : String;
  LocoChipStr : String;
  OK : Boolean;
  P : Integer;
  PointFeedbackWaitInSeconds : Double;
  PointResultPending : Boolean;
  RouteFound : Boolean;
  Route : Integer;
  SubRoute : Integer;
  T : TrainIndex;
  TempDivergingLineStr : String;

BEGIN
  TRY
    IF NOT ProgramStarting THEN BEGIN
      FOR P := 0 TO High(Points) DO BEGIN
        WITH Points[P] DO BEGIN
          LocoChipStr := LocoChipToStr(Point_LocoChipLockingTheRoute);
          IF Point_FeedbackPending THEN BEGIN
            IF Point_PresentState = Point_RequiredState THEN BEGIN
              IF Point_ManualOperation THEN BEGIN
                Point_FeedbackPending := False;
                Point_AwaitingManualChange := False;
                Log(LocoChipStr + ' P P=' + IntToStr(P) + ' pending change to ' + PointStateToStr(Point_RequiredState) + ' was successful');
                IF FWPShowMessageWindow.Visible THEN
                  FWPShowMessageWindow.Visible := False;
              END ELSE BEGIN
                { No need to draw it, as the change will have made the system draw it }
                DebugStr := 'P=' + IntToStr(P) + ' pending change to ';
                DebugStr := DebugStr + PointStateToStr(Point_RequiredState) + ' was successful after a '
                                     + FloatToStr(MilliSecondsBetween(Time, Point_FeedbackStartTime) / 1000) + ' second wait';
                Log(LocoChipStr + ' P ' + DebugStr);
                Point_FeedbackPending := False;
                Point_FeedbackPendingMsgWritten := False;
              END;

              IF Point_RequiredState = Straight THEN
                Log(LocoChipStr + ' P P=' + IntToStr(P) + ' D=S')
              ELSE
                Log(LocoChipStr + ' P P=' + IntToStr(P) + ' D=D');

            END ELSE BEGIN
              IF NOT Point_ManualOperation THEN BEGIN
                { PresentState <> RequiredState: a five second wait should be sufficient }
                PointFeedbackWaitInSeconds := Round(SecondSpan(Time, Point_FeedbackStartTime));
                IF PointFeedbackWaitInSeconds >= PointFeedbackMaximumWaitInSeconds THEN BEGIN
                  DebugStr := DebugStr + 'P=' + IntToStr(P) + ' pending change to ' + PointStateToStr(Point_RequiredState)
                                                              + ' failed after a ' + FloatToStr(PointFeedbackWaitInSeconds) + ' second wait';
                  Point_FeedbackPending := False;
                  Point_FeedbackPendingMsgWritten := False;
                  Log(LocoChipStr + ' P ' + DebugStr);

                  IF NOT Point_SecondAttempt THEN BEGIN
                    { have one more attempt at making it switch }
                    Point_SecondAttempt := True;
                    Log(LocoChipStr + ' P Second attempt to switch P=' + IntToStr(P));
                    PullPoint(LocoChipToStr(Point_LocoChipLockingTheRoute), P, NoRoute, NoSubRoute, NOT ForcePoint, ByUser, NOT ErrorMessageRequired, PointResultPending,
                              DebugStr, OK);
                    Exit;
                  END;

                  { Illustrate where the offending turnout is; note: blue is the new yellow here (!) - as clBlue comes out as pmNotXored clYellow }
                  DrawOutline(Point_MouseRect, clBlue, UndrawRequired, NOT UndrawToBeAutomatic);
                  Point_MaybeBeingSetToManual := True;

                  IF (Points[P].Point_Type <> CatchPointUp) AND (Points[P].Point_Type <> CatchPointDown) THEN
                    { we need to do this as an IfThen clause seems to evaluate the whole expression and we therefore get a range check error when a catch point does not
                      have a diverging line
                    }
                    TempDivergingLineStr := ' ' + Lines[Points[P].Point_DivergingLine].Line_NameStr
                  ELSE
                    TempDivergingLineStr := '';

                  { and produce a pop up message for the user, as this is important }
                  MakeSound(1);
                  CASE MessageDialogueWithDefault('P=' + IntToStr(P) + ' [Lenz ' + IntToStr(Points[P].Point_LenzNum)
                                                  + '] ('
                                                  + Lines[Points[P].Point_HeelLine].Line_NameStr
                                                  + ' ' + Lines[Points[P].Point_StraightLine].Line_NameStr
                                                  + TempDivergingLineStr
                                                  + ') has failed to change to ' + PointStateToStr(Point_RequiredState) + ':' + CRLF
                                                  + 'Retry, or set this point manually to ' + PointStateToStr(Point_RequiredState)
                                                  + ' and press ''Now Set'' when completed, or Retry or Ignore the problem',
                                                  NOT StopTimer, mtWarning, [mbAbort, mbOK, mbRetry],
                                                  ['&Now Set', '&Ignore', '&Retry'], mbAbort)
                  OF
                    mrOK: { Point Set }
                      BEGIN
                        Log(LocoChipStr + ' P P=' + IntToStr(P) + ' manually set to ' + PointStateToStr(Point_RequiredState));
                        Point_PreviousState := Point_PresentState;
                        Point_PresentState := Point_RequiredState;
                        InvalidateScreen(UnitRef, 'CheckPointsAwaitingFeedback');
                        IF MessageDialogueWithDefault('Set this point to manual for the rest of this session?',
                                                      NOT StopTimer, mtWarning, [mbYes, mbNo], mbYes) = mrYes
                        THEN BEGIN
                          Point_ManualOperation := True;
                          Log('P P=' + IntToStr(P) + ' set to manual operation');
                        END;
                      END;
                    mrRetry:
                      BEGIN
                        { Try forcing the point to switch the other way first - sometimes unsticks a point }
                        IF Point_RequiredState = Diverging THEN
                          Point_RequiredState := Straight
                        ELSE
                          Point_RequiredState := Diverging;
                        PullPoint(LocoChipToStr(Point_LocoChipLockingTheRoute), P, NoRoute, NoSubRoute, ForcePoint, ByUser, NOT ErrorMessageRequired,
                                                PointResultPending, DebugStr, OK);

                        IF Point_RequiredState = Diverging THEN
                          Point_RequiredState := Straight
                        ELSE
                          Point_RequiredState := Diverging;
                        PullPoint(LocoChipToStr(Point_LocoChipLockingTheRoute), P, NoRoute, NoSubRoute, {NOT} ForcePoint, ByUser, NOT ErrorMessageRequired,
                                                PointResultPending, DebugStr, OK);
                      END;
                    mrAbort: { Ignore }
                      { a major problem: cancel the route, and the train }
                      BEGIN
                        Route := 0;
                        RouteFound := False;
                        WHILE (Route <= High(Routes_Routes)) AND NOT RouteFound DO BEGIN
                          IF Routes_PointResultPendingPoint[Route] <> UnknownPoint THEN BEGIN
                            Log(LocoChipToStr(Routes_LocoChips[Route]) + ' R P=' + IntToStr(Routes_PointResultPendingPoint[Route])
                                                                       + ' pending point change and R=' + IntToStr(Route)
                                                                       + ' setting up cancelled by user');
                            Routes_RouteSettingsInProgress[Route] := False;
                            Routes_RouteClearingsInProgress[Route] := True;
                            FOR SubRoute := 0 TO High(Routes_SubRouteStates[Route]) DO BEGIN
                              CreateClearingSubRouteArray(Route, SubRoute);
                              Routes_SubRouteStates[Route, SubRoute] := SubRouteToBeCleared;
                            END;
                            IF MessageDialogueWithDefault('R=' + IntToStr(Route) + ' has been cancelled as a result of the failure'
                                                          + ' of point ' + IntToStr(Routes_PointResultPendingPoint[Route]) + ':'
                                                          + CRLF
                                                          + 'Do you want to cancel the train as well?',
                                                          NOT StopTimer, mtError, [mbYes, mbNo], ['&Cancel', '&Don''t Cancel'], mbYes) = mrYes
                            THEN BEGIN
                              IF Routes_LocoChips[Route] <> UnknownLocoChip THEN BEGIN
                                Log(LocoChipToStr(Routes_LocoChips[Route]) + ' DG System occupation and diagram entry cancelled by user');
                                { look for our train }
                                T := GetTrainIndexFromLocoChip(Routes_LocoChips[Route]);
                                IF T <> UnknownTrainIndex THEN
                                  CancelTrain(T, ByUser, TrainExists);
                              END;
                            END ELSE BEGIN

                              { may want to set the route by hand *** }

                            END;
                            Routes_PointResultPendingPoint[Route] := UnknownPoint;
                            RouteFound := True;
                          END;
                          Inc(Route);
                        END; {WHILE}
                      END;
                  END; {CASE}
                  Point_MaybeBeingSetToManual := False;
                END;
              END;
            END;
          END;
        END; {WITH}
      END; {FOR}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG CheckPointsAwaitingFeedback:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { CheckPointsAwaitingFeedback }

FUNCTION PointIsCatchPoint(P : Integer) : Boolean;
{ Returns whether a given point is a catch point }
BEGIN
  IF (Points[P].Point_Type = CatchPointUp) OR (Points[P].Point_Type = CatchPointDown) THEN
    Result := True
  ELSE
    Result := False;
END; { PointIsCatchPoint }

PROCEDURE FindNextPoint(TC : Integer; SearchDirection : DirectionType; OUT NextPoint : Integer);
{ Work out which is the next point }
CONST
  FindPoint = True;

VAR
  AdjoiningUpTrackCircuit : Integer;
  AdjoiningDownTrackCircuit : Integer;
  Line : Integer;
  TCFound : Boolean;

BEGIN
  TRY
    NextPoint := UnknownPoint;

    IF TC <> UnknownTrackCircuit THEN BEGIN
      Line := 0;
      TCFound := False;
      WHILE (Line <= High(Lines)) AND NOT TCFound DO BEGIN
        IF Lines[Line].Line_TC = TC THEN BEGIN
          TCFound := True;

          IF SearchDirection = Up THEN
            FollowThatLine(Line, TC, Up, FindPoint, AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit, NextPoint)
          ELSE
            FollowThatLine(Line, TC, Down, FindPoint, AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit, NextPoint);
        END;
        Inc(Line);
      END; {WHILE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FindNextPoint:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FindNextPoint }

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

PROCEDURE CalculatePointPositions;
{ Create where the points are on the screen. Note that three-way points must have the first, 'a' point, set to straight before the 'b' point can be set. }
VAR
  DebugStr : String;
  DeltaX, XLength, YLength : LongInt;
  P : Integer;
  UpX, UpY, OtherX, OtherY : Integer;

BEGIN
  TRY
    FOR P := 0 TO High(Points) DO BEGIN
      WITH Points[P] DO BEGIN
        { Mark the position of the heel of the point as UpX and UpY and add the appropriate data to the data for each line to show if there's a point at its start or end }
        UpX := 0;
        IF (Lines[Point_HeelLine].Line_GridUpX = Lines[Point_StraightLine].Line_GridDownX) THEN BEGIN
          UpX := Lines[Point_HeelLine].Line_GridUpX;

          Lines[Point_HeelLine].Line_NextUpType := PointIsNext;
          Lines[Point_HeelLine].Line_NextUpPoint := P;

          Lines[Point_StraightLine].Line_NextDownType := PointIsNext;
          Lines[Point_StraightLine].Line_NextDownPoint := P;

          IF NOT PointIsCatchPoint(P) THEN BEGIN
            Lines[Point_DivergingLine].Line_NextDownType := PointIsNext;
            Lines[Point_DivergingLine].Line_NextDownPoint := P;

            Point_FacingDirection := Up;

            IF Lines[Point_StraightLine].Line_GridDownX <> Lines[Point_DivergingLine].Line_GridDownX THEN BEGIN
              DebugStr := 'Lines do not properly intersect at P=' + IntToStr(P) + ': '
                          + 'DownX for straight line ' + LineToStr(Point_StraightLine) + ' is ' + IntToStr(Lines[Point_StraightLine].Line_GridDownX)
                          + ' and DownX for diverging line ' + LineToStr(Point_DivergingLine) + ' is ' + IntToStr(Lines[Point_DivergingLine].Line_GridDownX)
                          + CRLF
                          + 'Do you wish to continue?';
              IF MessageDialogueWithDefault(DebugStr, StopTimer, mtWarning, [mbOK, mbAbort], mbOK) = mrAbort THEN
                ShutDownProgram(UnitRef, 'CreatePoint');
            END;
          END;
        END ELSE
          IF (Lines[Point_HeelLine].Line_GridDownX = Lines[Point_StraightLine].Line_GridUpX) THEN BEGIN
            UpX := Lines[Point_StraightLine].Line_GridUpX;

            Lines[Point_HeelLine].Line_NextDownType := PointIsNext;
            Lines[Point_HeelLine].Line_NextDownPoint := P;

            Lines[Point_StraightLine].Line_NextUpType := PointIsNext;
            Lines[Point_StraightLine].Line_NextUpPoint := P;

            IF NOT PointIsCatchPoint(P) THEN BEGIN
              Lines[Point_DivergingLine].Line_NextUpType := PointIsNext;
              Lines[Point_DivergingLine].Line_NextUpPoint := P;

              Point_FacingDirection := Down;

              IF Lines[Point_StraightLine].Line_GridUpX <> Lines[Point_DivergingLine].Line_GridUpX THEN BEGIN
                DebugStr := 'Lines do not properly intersect at P=' + IntToStr(P) + ': ' + 'UpX for straight line ' + LineToStr(Point_StraightLine)
                            + ' is ' + IntToStr(Lines[Point_StraightLine].Line_GridUpX) + ' and UpX for diverging line ' + LineToStr(Point_DivergingLine)
                            + ' is ' + IntToStr(Lines[Point_DivergingLine].Line_GridUpX)
                            + CRLF
                            + 'Do you wish to continue?';
                IF MessageDialogueWithDefault(DebugStr, StopTimer, mtWarning, [mbOK, mbAbort], mbOK) = mrAbort THEN
                  ShutDownProgram(UnitRef, 'CreatePoint');
              END;
            END;
          END
          ELSE
            { shouldn't get here }
            Log('X! Failure in creating P=' + IntToStr(P)
                    + ' (' + LineToStr(Point_HeelLine) + '/' + LineToStr(Point_StraightLine) + '/' + LineToStr(Point_DivergingLine) + ')');

        UpY := Lines[Point_StraightLine].Line_GridUpY;

        { Now see if whether the diverging line shares that common point - if it does, swap UpX and DownX }
        IF NOT PointIsCatchPoint(P) THEN BEGIN
          IF UpX = Lines[Point_DivergingLine].Line_GridUpX THEN BEGIN
            OtherX := Lines[Point_DivergingLine].Line_GridDownX;
            OtherY := Lines[Point_DivergingLine].Line_GridDownY;
          END ELSE BEGIN
            OtherX := Lines[Point_DivergingLine].Line_GridUpX;
            OtherY := Lines[Point_DivergingLine].Line_GridUpY;
          END;
        END ELSE BEGIN
          IF Points[P].Point_Type = CatchPointUp THEN BEGIN
            OtherX := Lines[Point_StraightLine].Line_GridUpX - 100;
            OtherY := Lines[Point_StraightLine].Line_GridUpY - 100;
          END ELSE BEGIN
            OtherX := Lines[Point_StraightLine].Line_GridDownX + 100;
            OtherY := Lines[Point_StraightLine].Line_GridDownY + 100;
          END;
        END;

        Point_X := UpX;
        Point_Y := UpY;

        IF OtherX < UpX THEN
          DeltaX := -DeltaPointX
        ELSE
          DeltaX := +DeltaPointX;

        XLength := LongInt(OtherX) - LongInt(UpX);
        YLength := LongInt(OtherY) - LongInt(UpY);
        IF XLength = 0 THEN
          Debug('!Failure in creating P=' + IntToStr(P) + ' - UpX and OtherX are the same');

        Point_FarX := UpX + DeltaX;
        Point_FarY := UpY + (DeltaX * YLength + XLength DIV 2) DIV XLength;

        WITH Point_MouseRect DO BEGIN
          { Set up rectangles for mouse access }
          IF Point_FarY < Point_Y THEN BEGIN
            IF Point_FarX < Point_X THEN BEGIN
              Left := MapGridXToScreenX(Point_FarX);
              Top := MapGridYToScreenY(Point_FarY);
              Right := MapGridXToScreenX(Point_X);
              Bottom := MapGridYToScreenY(Point_Y);
            END ELSE BEGIN
              Left := MapGridXToScreenX(Point_X);
              Top := MapGridYToScreenY(Point_FarY);
              Right := MapGridXToScreenX(Point_FarX);
              Bottom := MapGridYToScreenY(Point_Y);
            END;
          END ELSE BEGIN
            IF Point_FarX < Point_X THEN BEGIN
              Left := MapGridXToScreenX(Point_FarX);
              Top := MapGridYToScreenY(Point_Y);
              Right := MapGridXToScreenX(Point_X);
              Bottom := MapGridYToScreenY(Point_FarY);
            END ELSE BEGIN
              Left := MapGridXToScreenX(Point_X);
              Top := MapGridYToScreenY(Point_Y);
              Right := MapGridXToScreenX(Point_FarX);
              Bottom := MapGridYToScreenY(Point_FarY);
            END;
          END;
        END; {WITH}
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('E : ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CalculatePointPositions }

FUNCTION ValidatePointStraightLineName(LineName : String; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Straight Line name is valid }
BEGIN
  ErrorMsg := '';
  Result := UnknownLine;

  IF LineName = '' THEN
    ErrorMsg := 'ValidatePointStraightLineName: no Straight Line name given'
  ELSE BEGIN
    Result := StrToLine(LineName);
    IF Result = UnknownLine THEN
      ErrorMsg := 'ValidatePointStraightLineName: unknown Straight Line name "' + LineName + '"';
  END;
END; { ValidatePointStraightLineName }

FUNCTION ValidatePointHeelLineName(LineName : String; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Heel Line name is valid }
BEGIN
  ErrorMsg := '';
  Result := UnknownLine;

  IF LineName = '' THEN
    ErrorMsg := 'ValidatePointHeelLineName: no Heel Line name given'
  ELSE BEGIN
    Result := StrToLine(LineName);
    IF Result = UnknownLine THEN
      ErrorMsg := 'ValidatePointHeelLineName: unknown Heel Line name "' + LineName + '"';
  END;
END; { ValidatePointHeelLineName }

FUNCTION ValidatePointDivergingLineName(LineName : String; PointType : TypeOfPoint; OUT ErrorMsg : String) : Integer;
{ Check that a given point's Diverging Line name is valid }
BEGIN
  ErrorMsg := '';
  Result := UnknownLine;

  IF LineName = '' THEN BEGIN
    IF (PointType <> CatchPointUp) AND (PointType <> CatchPointDown) THEN
      ErrorMsg := 'ValidatePointDivergingLineName: no Diverging Line name given'
  END ELSE BEGIN
    Result := StrToLine(LineName);
    IF Result = UnknownLine THEN
      ErrorMsg := 'ValidatePointDivergingLineName: unknown Diverging Line name "' + LineName + '"'
    ELSE
      IF (PointType = CatchPointUp) OR (PointType = CatchPointDown) THEN
        ErrorMsg := 'ValidatePointDivergingLineName: cannot have a Diverging Line if the point type is catch point up or down';
  END;
END; { ValidatePointDivergingLineName }

FUNCTION ValidatePointType(PointTypeStr : String; OUT ErrorMsg : String) : TypeOfPoint;
{ Check that the supplied point type is valid }
BEGIN
  ErrorMsg := '';
  Result := StrToPointType(PointTypeStr);

  IF Result = PointTypeUnknown THEN
    ErrorMsg := 'ValidatePointType: unknown point type: ''' + PointTypeStr;
END; { ValidatePointType }

FUNCTION ValidatePointLastFeedbackStateAsReadIn(PointStateStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : PointStateType;
{ Check whether the last feedback point state read in is valid }
BEGIN
  ErrorMsg := '';
  Result := PointStateUnknown;

  IF PointStateStr <> '' THEN BEGIN
    IF PointManualOperation THEN
      ErrorMsg := 'ValidatePointLastFeedbackStateAsReadIn : last feedback state recorded but point marked as being manually operated'
    ELSE
      Result := StrToPointState(PointStateStr);
  END;
END; { ValidatePointLastFeedbackStateAsReadIn }

FUNCTION ValidatePointLastManualStateAsReadIn(PointStateStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : PointStateType;
{ Check whether the last manual point state read in is valid }
BEGIN
  ErrorMsg := '';
  Result := PointStateUnknown;

  IF PointStateStr <> '' THEN BEGIN
    IF NOT PointManualOperation THEN
      ErrorMsg := 'ValidatePointLastManualStateAsReadIn : last manual state recorded but point not marked as being manually operated'
    ELSE
      Result := StrToPointState(PointStateStr);
  END;
END; { ValidatePointLastManualStateAsReadIn }

FUNCTION ValidatePointLenzNum(LenzNumStr : String; PointLastManualStateAsReadIn : PointStateType; OUT PointManualOperation : Boolean;
                              OUT PointPresentState : PointStateType; OUT ErrorMsg : String) : Integer;
{ Check whether the Lenz point number is valid }
BEGIN
  ErrorMsg := '';

  IF LenzNumStr = '' THEN
    Result := 0
  ELSE
    IF NOT TryStrToInt(LenzNumStr, Result) THEN
      ErrorMsg := 'ValidatePointLenzNum: invalid integer "' + LenzNumStr + '"';

  IF ErrorMsg = '' THEN BEGIN
    { If manual operation is ticked, it overrides the given Lenz data: this is useful if a Lenz point decoder fails (e.g. at 6/3/08) }
    IF NOT PointManualOperation THEN
      PointManualOperation := (Result = 0)
    ELSE BEGIN
      PointManualOperation := True;
      PointPresentState := PointLastManualStateAsReadIn;
    END;
  END;
END; { ValidatePointLenzNum }

FUNCTION ValidatePointLenzUnit(LenzUnitStr : String; OUT ErrorMsg : String) : Integer;
{ Check whether a Lenz point unit is valid }
BEGIN
  ErrorMsg := '';

  IF LenzUnitStr = '' THEN
    Result := 0
  ELSE
    IF NOT TryStrToInt(LenzUnitStr, Result) THEN
      ErrorMsg := 'ValidatePointLenzUnit: invalid integer "' + LenzUnitStr + '"';
END; { ValidatePointLenzUnit }

FUNCTION ValidatePointLenzUnitType(LenzUnitTypeStr : String; PointManualOperation : Boolean; OUT ErrorMsg : String) : String;
{ Check whether a Lenz point unit type is valid }
BEGIN
  ErrorMsg := '';
  Result := '';

  IF PointManualOperation THEN BEGIN
    IF LenzUnitTypeStr <> '' THEN
      ErrorMsg := 'cannot have a Lenz point unit type if the point is set to manual operation'
  END ELSE
    IF (LenzUnitTypeStr <> 'LS101') AND (LenzUnitTypeStr <> 'LS150') THEN
      ErrorMsg := 'Invalid Lenz point unit type ''' + LenzUnitTypeStr + ''''
    ELSE
      Result := LenzUnitTypeStr;
END; { ValidatePointLenzUnitType }

FUNCTION ValidatePointRelatedPoint(P : Integer; RelatedPointStr : String; PointType : TypeOfPoint; OUT ErrorMsg : String) : Integer;
{ Check whether the value of the connected point (if any) is valid }
BEGIN
  ErrorMsg := '';

  IF RelatedPointStr = '' THEN
    Result := UnknownPoint
  ELSE
    IF NOT TryStrToInt(RelatedPointStr, Result) THEN
      ErrorMsg := 'ValidatePointRelatedPoint: invalid integer "' + RelatedPointStr + '"';

  IF ErrorMsg = '' THEN
    IF P = Result THEN
      ErrorMsg := 'ValidatePointRelatedPoint: value of "other point" cannot be the same as the point''s own number';

  IF ErrorMsg = '' THEN BEGIN
    IF (PointType <> OrdinaryPoint) AND (PointType <> CatchPointUp) AND (PointType <> CatchPointDown) AND (Result = UnknownPoint) THEN
      ErrorMsg := 'ValidatePointRelatedPoint: value of "other point" is missing'
    ELSE
      IF (PointType = ProtectedPoint) AND (Result = UnknownPoint) THEN
        ErrorMsg := 'ValidatePointRelatedPoint: value of "other point" is missing'
      ELSE
        IF (PointType = OrdinaryPoint) AND (Result <> UnknownPoint) THEN
          ErrorMsg := 'ValidatePointRelatedPoint: value of "other point" is invalid';
  END;
END; { ValidatePointRelatedPoint }

FUNCTION ValidatePointDefaultState(NewStateStr : String; HeelLine, StraightLine, DivergingLine : Integer; OUT PresentState : PointStateType; OUT ErrorMsg : String)
                                   : PointStateType;
{ Check whether the point state supplied is valid }
BEGIN
  ErrorMsg := '';

  Result := StrToPointState(NewStateStr);
  IF (NewStateStr <> '') AND (Result = PointStateUnknown) THEN
    ErrorMsg := 'ValidatePointDefaultState: invalid state "' + NewStateStr + '"'
  ELSE
    IF Result = PointOutOfAction THEN BEGIN
      { the point doesn't go anywhere }
      Lines[HeelLine].Line_NextUpType := EndOfLineIsNext;
      Lines[HeelLine].Line_NextDownType := EndOfLineIsNext;
      Lines[StraightLine].Line_NextUpType := EndOfLineIsNext;
      Lines[StraightLine].Line_NextDownType := EndOfLineIsNext;
      Lines[DivergingLine].Line_NextUpType := EndOfLineIsNext;
      Lines[DivergingLine].Line_NextDownType := EndOfLineIsNext;
      PresentState := PointOutOfAction;
    END;
END; { ValidatePointDefaultState }

PROCEDURE InitialisePointVariables(P : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }
BEGIN
  WITH Points[P] DO BEGIN
    Point_AwaitingManualChange := False;
    Point_DataChanged := False;
    Point_Energised := False;
    Point_EnergisedTime := 0;
    Point_FeedbackPending := False;
    Point_FeedbackPendingMsgWritten := False;
    Point_ForcedDelayMsg1Written := False;
    Point_ForcedDelayMsg2Written := False;
    Point_LastChangedTime := 0;
    Point_LocoChipLockingTheRoute := UnknownLocoChip;
    Point_LockedByUser := False;
    Point_LockFailureNotedInLocksUnit := False;
    Point_LockFailureNotedInSubRouteUnit := False;
    Point_LockingState := PointStateUnknown;
    Point_MaybeBeingSetToManual := False;
    Point_MovedWhenLocked := False;
    Point_PresentState := PointStateUnknown;
    Point_RequiredState := PointStateUnknown;
    Point_ResettingTime := 0;
    Point_RouteLockedByLocoChip := UnknownRoute;
    Point_SecondAttempt := False;
    Point_SetASecondTime := False;
    Point_WaitTime := 0;

    SetLength(Point_LockingArray, 0);
  END; {WITH}
END; { InitialisePointVariables }

PROCEDURE ReadInPointDataFromDatabase;
{ Create all the points; three numbers in each call are Lenz point number, feedback unit and input number on feedback unit.
  FeedbackOnIsStraight indicates whether feedback is on when point is straight; WiredCorrectly shows if point when on = straight or diverging.

  Next Lenz Point Num:
  Next LS150 is 167; (they go in sixes so need two together or LS110s cannot follow the numbering: 113 & 120 are a pair, 155 and 161, 167 and 173, 179 and 185 are).

  Next LS110 (if any) is 167 or 179; (they go in fours). (32, 83 not used).

  Points currently not responding: 37-41 [??], 127-129 (route to new station not yet installed),
  133-136 [not yet installed - temporarily in use - see note of 12/4/09 below], 138 [failed], 141-142 [failed], 146 [failed]

  temporarily rearranging feedback below FY 11-12/3/08:
    pt 001 - L=17 F=107-2 On WR
    pt 112 - L=9 F=107-6 - WR
    pt 134 - L=137 F=109-5 On -
    pt 135 - L=148 F=109-6 - -
    pt 129 - L=12 F=107-3 On WR

  another temporary fix after failure of an LS110  12/4/09 (no idea what last year's fix was for but it would appear to be permanent!)
    pt 47  - was 53 - 14 - 85 - 5 - on - on
    pt 53  - was 55 - 14 - 85 - 8 - off - off
    pt 55  - was 56 - 14 - 85 - 6 - off - off
    pt 109 - was 54 - 14 - 85 - 7 - off - off

  To set up points to appear properly on screen, first set Wiring Reversed and OnIsStraight to False. Then adjust each physical point to be straight. Any points that do
  not appear straight on screen set OnIsStraight to True. Then test each point via the screen, and any that do not switch as planned set WiringReversed to True. All should
  then work properly! (FWP 17/6/09)
}
CONST
  StopTimer = True;

VAR
  Bookmark : TBookmark;
  ErrorMsg : String;
  LastManualStateStr : String;
  NextInDatabaseP : Integer;
  P : Integer;
  TempPointNumber : Integer;

BEGIN
  TRY
    Log('A INITIALISING POINTS {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Point database file "' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInPointDataFromDatabase')
        ELSE
          Exit;
      END;

      PointsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix
                                              + ';Persist Security Info=False';
      PointsADOConnection.Connected := True;
      PointsADOTable.Open;
      Log('P Point data table and connection opened to initialise the point data');

      { First see if the point numbers in the MSAccess file are sequential and, if not, renumber it - we need this or deletions from the MSAccess file will cause problems }
      P := -1;
      PointsADOTable.Sort := '[' + Point_NumberFieldName + '] ASC';
      PointsADOTable.First;
      WHILE NOT PointsADOTable.EOF DO BEGIN
        Inc(P);
        NextInDatabaseP := PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger;
        IF NextInDatabaseP <> P THEN BEGIN
          TRY
            Bookmark := PointsADOTable.GetBookmark;
            { firstly see if any points cross-refer to the missing point at P, and note the fact }
            PointsADOTable.First;
            { at the start of the database }
            WHILE NOT PointsADOTable.EOF DO BEGIN
              IF PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsInteger = P THEN BEGIN
                Log('A! P=' + IntToStr(PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger) + '''s Related Point refers to '
                        + ' P=' + IntToStr(PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsInteger)
                        + ' which did not exist prior to point renumbering - Related Point has been set to unknown');
                PointsADOTable.Edit;
                PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsInteger := UnknownPoint;
                PointsADOTable.Post;
              END;
              PointsADOTable.Next;
            END; {WHILE}

            { secondly we have to renumber any points that cross-refer to this point - allocate memory and assign where we are in the database to the bookmark }
            PointsADOTable.First;
            { at the start of the database }
            WHILE NOT PointsADOTable.EOF DO BEGIN
              IF InitVarsWindow.PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsInteger = NextInDatabaseP THEN BEGIN
                IF NextInDatabaseP = P THEN
                  { this shouldn't happen }
                  Log('A! Related Point is ' + IntToStr(NextInDatabaseP) + ' is the same as P' + IntToStr(P));
                PointsADOTable.Edit;
                PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsInteger := P;
                PointsADOTable.Post;
              END;
              PointsADOTable.Next;
            END; {WHILE}
          FINALLY
            { and return whence we came }
            PointsADOTable.GotoBookmark(Bookmark);
          END;

          { and now we need to renumber this (and in due course all subsequent) entries }
          PointsADOTable.Edit;
          PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger := P;
          PointsADOTable.Post;
        END;
        PointsADOTable.Next;
      END; {WHILE}

      P := -1;
      PointsADOTable.Sort := '[' + Point_NumberFieldName + '] ASC';
      PointsADOTable.First;

      SetLength(Points, 0);
      WHILE NOT PointsADOTable.EOF DO BEGIN
        ErrorMsg := '';
        LastManualStateStr := '';
        Inc(P);

        TempPointNumber := PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger;
        IF PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger <> TempPointNumber THEN
          ErrorMsg := 'it does not match the point number in the database (' + IntToStr(TempPointNumber) + ')'
        ELSE
          SetLength(Points, Length(Points) + 1);

        WITH Points[P] DO BEGIN
          InitialisePointVariables(P);
          Point_Number := TempPointNumber;

          IF ErrorMsg = '' THEN
            Point_HeelLine := ValidatePointHeelLineName(PointsADOTable.FieldByName(Point_HeelLineFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_TCAtHeel := Lines[Point_HeelLine].Line_TC;

          IF ErrorMsg = '' THEN
            Point_StraightLine := ValidatePointStraightLineName(PointsADOTable.FieldByName(Point_StraightLineFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_Type := ValidatePointType(PointsADOTable.FieldByName(Point_TypeFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_DivergingLine := ValidatePointDivergingLineName(PointsADOTable.FieldByName(Point_DivergingLineFieldName).AsString, Point_Type, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_LockedByUser := PointsADOTable.FieldByName(Point_LockedByUserFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_OutOfUse := PointsADOTable.FieldByName(Point_OutOfUseFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_ManualOperation := PointsADOTable.FieldByName(Point_ManualOperationFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_LastFeedbackStateAsReadIn := ValidatePointLastFeedbackStateAsReadIn(PointsADOTable.FieldByName(Point_LastFeedbackStateAsReadInFieldName).AsString,
                                                                                      Point_ManualOperation, ErrorMsg);
          IF ErrorMsg = '' THEN
            Point_LastManualStateAsReadIn := ValidatePointLastManualStateAsReadIn(PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString,
                                                                                  Point_ManualOperation, ErrorMsg);
          IF ErrorMsg = '' THEN
            Point_LenzNum := ValidatePointLenzNum(PointsADOTable.FieldByName(Point_LenzNumFieldName).AsString, Point_LastManualStateAsReadIn, Point_ManualOperation,
                                                  Point_PresentState, ErrorMsg);
          IF ErrorMsg = '' THEN
            Point_LenzUnit := ValidatePointLenzUnit(PointsADOTable.FieldByName(Point_LenzUnitFieldName).AsString, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_LenzUnitType := ValidatePointLenzUnitType(PointsADOTable.FieldByName(Point_LenzUnitTypeFieldName).AsString, Point_ManualOperation, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_FeedbackUnit := ValidateFeedbackUnit(PointsADOTable.FieldByName(Point_FeedbackUnitFieldName).AsString, Point_HasFeedback, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_FeedbackInput := ValidateFeedbackInput(PointsADOTable.FieldByName(Point_FeedbackInputFieldName).AsString, Point_HasFeedback, Point_FeedbackUnit,
                                                         PointFeedback, Point_Number, ErrorMsg);
          IF ErrorMsg = '' THEN
            Point_FeedbackOnIsStraight := PointsADOTable.FieldByName(Point_FeedbackOnIsStraightFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_WiringReversedFlag := PointsADOTable.FieldByName(Point_WiringReversedFlagFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_RelatedPoint := ValidatePointRelatedPoint(P, PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsString, Point_Type, ErrorMsg);

          IF ErrorMsg = '' THEN
            Point_Notes := PointsADOTable.FieldByName(Point_NotesFieldName).AsString;

          IF ErrorMsg = '' THEN
            Point_DefaultState := ValidatePointDefaultState(PointsADOTable.FieldByName(Point_DefaultStateFieldName).AsString, Point_HeelLine, Point_StraightLine,
                                                            Point_DivergingLine, Point_PresentState, ErrorMsg);
          IF ErrorMsg = '' THEN
            Point_LockedIfHeelTCOccupied := PointsADOTable.FieldByName(Point_LockedIfHeelTCOccupiedFieldName).AsBoolean;

          IF ErrorMsg = '' THEN
            Point_LockedIfNonHeelTCsOccupied := PointsADOTable.FieldByName(Point_LockedIfNonHeelTCsOccupiedFieldName).AsBoolean;

          IF ErrorMsg = '' THEN BEGIN
            IF Point_ManualOperation THEN BEGIN
              IF Point_LastManualStateAsReadIn <> PointStateUnknown THEN
                Point_RequiredState := Point_LastManualStateAsReadIn
              ELSE
                Point_RequiredState := Point_DefaultState;
            END ELSE
              Point_RequiredState := Point_DefaultState;
          END;

          IF ErrorMsg <> '' THEN BEGIN
            IF MessageDialogueWithDefault('Error in creating P=' + IntToStr(P) + ': '
                                          + ErrorMsg
                                          + CRLF
                                          + 'Do you wish to continue?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
            THEN
              ShutDownProgram(UnitRef, 'ReadInPointDataFromDatabase');
          END;

          PointsADOTable.Next;
        END; {WITH}
      END; {WHILE}

      { Tidy up the database }
      PointsADOTable.Close;
      PointsADOConnection.Connected := False;
      Log('P Point data table and connection closed');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInPointDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}

  CalculatePointPositions;
END; { ReadInPointDataFromDatabase }

FUNCTION DeleteRecordFromPointDatabase(PointToDeleteNum : Integer) : Boolean;
{ Remove a record from the point database }
BEGIN
  Result := False;
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Point database file "' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'DeleteRecordFromPointDatabase')
        ELSE
          Exit;
      END;

      PointsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix
                                               + ';Persist Security Info=False';
      PointsADOConnection.Connected := True;
      PointsADOTable.Open;
      IF NOT PointsADOTable.Locate(Point_NumberFieldName, IntToStr(PointToDeleteNum), []) THEN BEGIN
        Log('P Point data table and connection opened to delete P' + IntToStr(PointToDeleteNum) + ' but it cannot be found');
      END ELSE BEGIN
        Log('P Point data table and connection opened to delete P' + IntToStr(PointToDeleteNum));

        { Now delete the point - we have already checked, in the Edit unit, whether deleting it will cause knock-on problems with other Points }
        PointsADOTable.Delete;
        Log('PG P' + IntToStr(PointToDeleteNum) + ' has been deleted');
        Result := True;
      END;

      { Tidy up the database }
      PointsADOTable.Close;
      PointsADOConnection.Connected := False;
      Log('S Point Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToPointDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DeleteRecordFromPointDatabase }

PROCEDURE AddNewRecordToPointDatabase;
{ Append a record to the point database }
BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Point database file "' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'AddNewRecordToPointDatabase')
        ELSE
          Exit;
      END;

      PointsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source=' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix
                                               + ';Persist Security Info=False';
      PointsADOConnection.Connected := True;
      PointsADOTable.Open;
      PointsADOTable.Append;
      PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger := High(Points);
      PointsADOTable.Post;

      Log('S Point data table and connection opened to write out point data that has changed');
      { Tidy up the database }
      PointsADOTable.Close;
      PointsADOConnection.Connected := False;
      Log('S Point Data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToPointDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { AddNewRecordToPointDatabase }

PROCEDURE WriteOutPointDataToDatabase;
{ If a point's data has been changed, record it in the database }
VAR
  P : Integer;
  PointDatabaseNeedsUpdating : Boolean;
  TempStr : String;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      PointDatabaseNeedsUpdating := False;

      { First see if we need to open the database }
      P := 0;
      WHILE (P <= High(Points)) AND NOT PointDatabaseNeedsUpdating DO BEGIN
        WITH Points[P] DO BEGIN
          IF Point_DataChanged THEN
            PointDatabaseNeedsUpdating := True;

          IF Point_ManualOperation AND (Point_LastManualStateAsReadIn <> Point_PresentState) THEN
            PointDatabaseNeedsUpdating := True;
          IF NOT Point_ManualOperation AND (Point_LastManualStateAsReadIn <> PointStateUnknown) THEN
            PointDatabaseNeedsUpdating := True;

          IF NOT Point_ManualOperation AND (Point_LastFeedbackStateAsReadIn <> Point_PresentState) THEN
            PointDatabaseNeedsUpdating := True;
        END; {WITH}
        Inc(P);
      END; {WHILE}

      IF PointDatabaseNeedsUpdating THEN BEGIN
        IF NOT FileExists(PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix) THEN BEGIN
          IF MessageDialogueWithDefault('Point database file "' + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix + '" cannot be located'
                                        + CRLF
                                        + 'Do you wish to continue?',
                                        StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
          THEN
            ShutDownProgram(UnitRef, 'WriteOutPointDataToDatabase')
          ELSE
            Exit;
        END;

        PointsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                + PathToRailDataFiles + PointDataFilename + '.' + PointDataFilenameSuffix
                                                + ';Persist Security Info=False';
        PointsADOConnection.Connected := True;
        PointsADOTable.Open;
        PointsADOTable.Edit;
        Log('P Point data table and connection opened to write out point data that has changed');

        PointsADOTable.First;
        WHILE NOT PointsADOTable.EOF DO BEGIN
          P := PointsADOTable.FieldByName(Point_NumberFieldName).AsInteger;
          WITH Points[P] DO BEGIN
            IF Point_DataChanged THEN BEGIN
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_NumberFieldName).AsString := IntToStr(Point_Number);
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + ' is now ' + IntToStr(Point_Number));

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_StraightLineFieldName).AsString := LineToStr(Point_StraightLine);
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Straight Line is now ' + LineToStr(Point_StraightLine));

              IF Point_DivergingLine <> UnknownLine THEN
                TempStr := LineToStr(Point_DivergingLine)
              ELSE
                TempStr := '';
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_DivergingLineFieldName).AsString := TempStr;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Diverging Line is now "' + TempStr + '"');

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_HeelLineFieldName).AsString := LineToStr(Point_HeelLine);
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Heel Line is now ' + LineToStr(Point_HeelLine));

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_OutOfUseFieldName).AsBoolean := Point_OutOfUse;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + ' is now ' + IfThen(Point_OutOfUse, 'out of use', 'back in use'));

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_LockedByUserFieldName).AsBoolean := Point_LockedByUser;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + ' is now ' + IfThen(Point_LockedByUser, 'locked by user', 'back in use'));

              IF Point_RelatedPoint <> UnknownPoint THEN
                TempStr := IntToStr(Point_RelatedPoint)
              ELSE
                TempStr := '';
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_RelatedPointFieldName).AsString := TempStr;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Related Point is now "' + TempStr + '"');

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_TypeFieldName).AsString := PointTypeToStr(Point_Type);
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Point Type is now ' + PointTypeToStr(Point_Type));

              IF Point_DefaultState <> PointStateUnknown THEN
                TempStr := PointStateToStr(Point_DefaultState)
              ELSE
                TempStr := '';
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_DefaultStateFieldName).AsString := TempStr;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Default State is now "' + TempStr + '"');

              IF Point_FeedbackInput <> 0 THEN
                TempStr := IntToStr(Point_FeedbackInput)
              ELSE
                TempStr := '';
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_FeedbackInputFieldName).AsString := TempStr;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Feedback Input is now "' + TempStr + '"');

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_FeedbackOnIsStraightFieldName).AsBoolean := Point_FeedbackOnIsStraight;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Feedback On Is Straight flag is now ' + BoolToStr(Point_FeedbackOnIsStraight, True));

              IF Point_FeedbackUnit <> 0 THEN
                TempStr := IntToStr(Point_FeedbackUnit)
              ELSE
                TempStr := '';
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_FeedbackUnitFieldName).AsString := TempStr;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Feedback Unit is now "' + Tempstr + '"');

              IF Point_LenzNum <> 0 THEN
                TempStr := IntToStr(Point_LenzNum)
              ELSE
                TempStr := '';
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_LenzNumFieldName).AsString := TempStr;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Lenz Point Number is now "' + TempStr + '"');

              IF Point_LenzUnit <> 0 THEN
                TempStr := IntToStr(Point_LenzUnit)
              ELSE
                TempStr := '';
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_LenzUnitFieldName).AsString := TempStr;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Lenz Point Unit is now "' + TempStr + '"');

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_LenzUnitTypeFieldName).AsString := Point_LenzUnitType;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Lenz Unit Type is now ' + Point_LenzUnitType);

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_LockedIfHeelTCOccupiedFieldName).AsBoolean := Point_LockedIfHeelTCOccupied;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Locked If Heel TC Occupied flag is now ' + BoolToStr(Point_LockedIfHeelTCOccupied, True));

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_LockedIfNonHeelTCsOccupiedFieldName).AsBoolean := Point_LockedIfNonHeelTCsOccupied;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Locked If Non-Heel TCs Occupied flag is now ' + BoolToStr(Point_LockedIfNonHeelTCsOccupied,
                                                                                                                                                                     True));
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_ManualOperationFieldName).AsBoolean := Point_ManualOperation;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Manual Operation is now ' + BoolToStr(Point_ManualOperation, True));

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_NotesFieldName).AsString := Point_Notes;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s notes are now ' + Point_Notes);

              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_WiringReversedFlagFieldName).AsBoolean := Point_WiringReversedFlag;
              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s Wiring Reversed flag is now ' + BoolToStr(Point_WiringReversedFlag, True));
            END;

            IF NOT Point_ManualOperation AND (Point_LastManualStateAsReadIn <> PointStateUnknown) THEN BEGIN
              PointsADOTable.Edit;
              PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString := '';
              PointsADOTable.Post;
            END
              ELSE
                IF (Point_ManualOperation) AND (Point_LastManualStateAsReadIn <> Point_PresentState) THEN BEGIN
                  PointsADOTable.Edit;
                  IF Points[P].Point_PresentState = PointStateUnknown THEN
                    PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString := ''
                  ELSE
                    IF Points[P].Point_PresentState = Straight THEN
                      PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString := 'Straight'
                    ELSE
                      PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString := 'Diverging';
                  PointsADOTable.Post;
                  IF InPointDebuggingMode THEN
                    Log('P Recording in point database that manual P=' + IntToStr(P) + '''s state is now ' + PointStateToStr(Points[P].Point_PresentState));
                END;

            { And of points from which we've had feedback }
            IF NOT Point_ManualOperation THEN BEGIN
              PointsADOTable.Edit;

              { but only update this if we received feedback from the point at the last startup &&&&&&&&&& }
              IF Points[P].Point_PresentState = PointStateUnknown THEN BEGIN
                PointsADOTable.FieldByName(Point_LastManualStateAsReadInFieldName).AsString := '';
                PointsADOTable.FieldByName(Point_LastFeedbackStateAsReadInFieldName).AsString := '';
              END ELSE
                IF Points[P].Point_PresentState = Straight THEN
                  PointsADOTable.FieldByName(Point_LastFeedbackStateAsReadInFieldName).AsString := StraightStr
                ELSE
                  IF Points[P].Point_PresentState = Diverging THEN
                    PointsADOTable.FieldByName(Point_LastFeedbackStateAsReadInFieldName).AsString := DivergingStr;

              PointsADOTable.Post;
              IF InPointDebuggingMode THEN
                Log('P Recording in point database that P=' + IntToStr(P) + '''s feedback state is now ' + PointStateToStr(Points[P].Point_PresentState));
            END;
          END; {WITH}
          PointsADOTable.Next;
        END; {WHILE}

        { Tidy up the database }
        PointsADOTable.Close;
        PointsADOConnection.Connected := False;
        Log('P Point data table and connection closed');
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutPointDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutPointDataToDatabase }

END { PointsUnit }.
