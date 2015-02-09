UNIT TrackCircuitsUnit;
{ Controls the various track circuits and track circuit-related things

  Copyright © F.W. Pritchard 2015. All Rights Reserved.

  v0.1  02/02/15 Code extracted mainly from InitVars
}
INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, InitVars, Options, LinesUnit,
     Train;

TYPE
  TTrackCircuitsUnitForm = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

  TrackCircuitStateType = (TCFeedbackOccupation, TCFeedbackOccupationButOutOfUse, TCPermanentFeedbackOccupation, TCPermanentOccupationSetByUser, TCSystemOccupation,
                           TCPermanentSystemOccupation, TCMissingOccupation, TCOutOfUseSetByUser, TCOutOfUseAsNoFeedbackReceived, TCLocoOutOfPlaceOccupation, TCUnoccupied);

PROCEDURE AddNewRecordToTrackCircuitDatabase;
{ Append a record to the track-circuit database }

PROCEDURE CalculateTCAdjacentSignals;
{ Work out which track circuits are next the signal }

PROCEDURE CalculateTCAdjacentBufferStops;
{ Work out which track circuits are next a buffer stop }

FUNCTION DeleteRecordFromTrackCircuitDatabase(TrackCircuitToDeleteNum : Integer) : Boolean;
{ Remove a record from the track-circuit database }

PROCEDURE FindAdjoiningTrackCircuits(TC : Integer; OUT AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit : Integer);
{ Work out which are the adjacent track circuits. Does not trace along all lines, just the way points are set. }

FUNCTION GetLocationFromTrackCircuit(TC : Integer) : Integer;
{ Return a location given a track-circuit number }

FUNCTION GetTrackCircuitsForLocation(Location : Integer) : IntegerArrayType;
{ Return all the track circuits for a given location }

FUNCTION GetTrackCircuitState(TC : Integer) : TrackCircuitStateType;
{ Return whether and how the track circuit is occupied }

FUNCTION GetTrackCircuitStateColour(TC : Integer) : TColour;
{ Return whether and how the track circuit is occupied }

PROCEDURE InitialiseTrackCircuitVariables(TC : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }

FUNCTION IsTrackCircuitInStringArray(StringArray : StringArrayType; TC : Integer; OUT Pos : Integer) : Boolean;
{ Returns whether and where the given track circuit is found in a string array }

PROCEDURE ReadInTrackCircuitDataFromDatabase;
{ Initialise the track circuit data which depends on lines being initialised first }

PROCEDURE SetTrackCircuitState{1}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the track circuit is occupied, and mark it with a loco chip number }

PROCEDURE SetTrackCircuitState{2}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the track circuit is occupied, mark it with a loco chip number, and give an explanation }

PROCEDURE SetTrackCircuitState{3}(TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the track circuit is occupied }

PROCEDURE SetTrackCircuitState{4}(TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the track circuit is occupied and give an explanation. Also see whether we want it recorded in the Location occupation array - this is to avoid
  duplicate recordings at startup.
}
FUNCTION TrackCircuitStateIsPermanentlyOccupied(State : TrackCircuitStateType) : Boolean;
{ Returns true if a given track-circuit state is not set as permanently occupied }

FUNCTION TrackCircuitStateIsTemporarilyOccupied(State : TrackCircuitStateType) : Boolean;
{ Returns true if a given track circuit-state is set as temporarily occupied }

PROCEDURE UnlockTrackCircuitRouteLocking(TC : Integer);
{ Unlock a given track circuit }

PROCEDURE WriteOutTrackCircuitDataToDatabase;
{ Write out some track-circuit data to the track-circuit data file }

TYPE
  TrackCircuitRec = RECORD
    TC_AdjacentBufferStop : Integer;
    TC_AdjacentSignals : IntegerArrayType;
    TC_DataChanged : Boolean;
    TC_EmergencyLocoChip : Integer;
    TC_EmergencyState : TrackCircuitStateType;
    TC_FeedbackInput : Integer;
    TC_FeedbackUnit : Integer;
    TC_FeedbackOccupation : Boolean;
    TC_Flashing : Boolean;
    TC_Gradient : GradientType;
    TC_HeadCode : String;
    TC_Journey : Integer;
    TC_LengthInInches : Extended;
    TC_LineArray : IntegerArrayType;
    TC_LitUp : Boolean; { used for flashing }
    TC_Location : Integer;
    TC_LockedForRoute : Integer;
    TC_LockFailureNotedInSubRouteUnit : Boolean;
    TC_LocoChip : Integer;
    TC_LocoStalled : Boolean;
    TC_MissingTrainNoted : Boolean;
    TC_MysteriouslyOccupied : Boolean;
    TC_Number : Integer;
    TC_OccupationStartTime : TDateTime;
    TC_OccupationState : TrackCircuitStateType;
    TC_PreviousLocoChip : Integer;
    TC_PreviousOccupationState : TrackCircuitStateType;
    TC_ResettingSignal : Integer;
    TC_SaveRouteLocking : Integer;
    TC_SaveTrackCircuitHeadCode : String;
    TC_SpeedRestrictionInMPH : MPHType;
    TC_SpeedRestrictionDirection : DirectionType;
  END;

CONST
  TC_NumberFieldName : String = 'TCNum';
  TC_LengthInInchesFieldName : String = 'Length';
  TC_FeedbackUnitFieldName : String = 'FeedbackUnit';
  TC_FeedbackInputFieldName : String = 'FeedbackInput';

VAR
  TrackCircuitsUnitForm: TTrackCircuitsUnitForm;

  TrackCircuitHighlighted : Integer = UnknownTrackCircuit;
  TrackCircuits : ARRAY OF TrackCircuitRec;
  TrackCircuitsInitialised : Boolean = False;

IMPLEMENTATION

{$R *.dfm}

USES MiscUtils, PointsUnit, SignalsUnit, RailDraw, Route, LocationsUnit, Feedback, Main, Logging;

CONST
  UnitRef = 'TrackCircuitsUnit';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION IsTrackCircuitInStringArray(StringArray : StringArrayType; TC : Integer; OUT Pos : Integer) : Boolean;
{ Returns whether and where the given track circuit is found in a string array }
BEGIN
  Pos := 0;
  Result := False;
  WHILE (Pos <= High(StringArray)) AND (Result = False) DO BEGIN
    IF (TC <> UnknownTrackCircuit) AND (TC = ExtractTrackCircuitFromString(StringArray[Pos])) THEN
      Result := True
    ELSE
      Inc(Pos);
  END; {WHILE}
END; { IsTrackCircuitInStringArray }

FUNCTION GetLocationFromTrackCircuit(TC : Integer) : Integer;
{ Return a location given a track-circuit number }
VAR
  Line : Integer;
  LocationFound : Boolean;

BEGIN
  Line := 0;
  Result := UnknownLocation;

  LocationFound := False;
  WHILE (Line <= High(Lines)) AND NOT LocationFound DO BEGIN
    IF Lines[Line].Line_TC = TC THEN BEGIN
      IF Lines[Line].Line_Location <> UnknownLocation THEN BEGIN
        LocationFound := True;
        Result := Lines[Line].Line_Location;
      END;
    END;
    Inc(Line);
  END; {WHILE}
END; { GetLocationFromTrackCircuit }

FUNCTION TrackCircuitStateIsPermanentlyOccupied(State : TrackCircuitStateType) : Boolean;
{ Returns true if a given track-circuit state is not set as permanently occupied }
BEGIN
  CASE State OF
    TCOutOfUseSetByUser, TCOutOfUseAsNoFeedbackReceived, TCLocoOutOfPlaceOccupation, TCPermanentFeedbackOccupation, TCPermanentOccupationSetByUser,
    TCPermanentSystemOccupation:
      Result := True;
  ELSE
    { all other occupation types }
    Result := False;
  END; {CASE}
END; { TrackCircuitStateIsPermanentlyOccupied }

FUNCTION TrackCircuitStateIsTemporarilyOccupied(State : TrackCircuitStateType) : Boolean;
{ Returns true if a given track-circuit state is set as temporarily occupied }

BEGIN
  CASE State OF
    TCFeedbackOccupation, TCFeedbackOccupationButOutOfUse, TCSystemOccupation:
      Result := True;
  ELSE
    { all other occupation types }
    Result := False;
  END; {CASE}
END; { TrackCircuitStateIsTemporarilyOccupied }

FUNCTION GetTrackCircuitStateColour(TC : Integer) : TColour;
{ Return whether and how the track circuit is occupied }
BEGIN
  IF TC = UnknownTrackCircuit THEN
    GetTrackCircuitStateColour := TCUnoccupiedColour
  ELSE
    CASE GetTrackCircuitState(TC) OF
      TCFeedbackOccupation:
        Result := TCFeedbackOccupationColour;
      TCFeedbackOccupationButOutOfUse:
        Result := TCFeedbackOccupationButOutOfUseColour;
      TCLocoOutOfPlaceOccupation:
        Result := TCLocoOutOfPlaceOccupationColour;
      TCMissingOccupation:
        Result := TCMissingOccupationColour;
      TCOutOfUseSetByUser:
        Result := TCOutOfUseSetByUserColour;
      TCOutOfUseAsNoFeedbackReceived:
        Result := TCOutOfUseAsNoFeedbackReceivedColour;
      TCPermanentFeedbackOccupation:
        Result := TCPermanentFeedbackOccupationColour;
      TCPermanentOccupationSetByUser:
        Result := TCPermanentOccupationSetByUserColour;
      TCPermanentSystemOccupation:
        Result := TCPermanentSystemOccupationColour;
      TCSystemOccupation:
        Result := TCSystemOccupationColour;
      TCUnoccupied:
        Result := TCUnoccupiedColour;
    ELSE
      Result := TCUnoccupiedColour;
    END; {CASE}
END; { GetTrackCircuitStateColour }

FUNCTION GetTrackCircuitState(TC : Integer) : TrackCircuitStateType;
{ Return whether and how the track circuit is occupied }
BEGIN
  Result := TCUnoccupied;
  TRY
    IF TC <> UnknownTrackCircuit THEN BEGIN
      Result := TrackCircuits[TC].TC_OccupationState;
      IF DisplayFlashingTrackCircuits AND (TrackCircuits[TC].TC_Headcode = '?') THEN
        { only mystery occupation flashes }
        TrackCircuits[TC].TC_Flashing := True
      ELSE BEGIN
        TrackCircuits[TC].TC_Flashing := False;
        { and, in case it had been flashing, mark it as being in the lit-up state up so it will continue to be drawn }
        TrackCircuits[TC].TC_LitUp := True;
      END;
    END;
  EXCEPT
    ON E : Exception DO
      ShowMessage('GetTrackCircuitState: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { GetTrackCircuitState }

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

PROCEDURE FindAdjoiningTrackCircuits(TC : Integer; OUT AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit : Integer);
{ Work out which are the adjacent track circuits. Does not trace along all lines, just the way points are set. }
CONST
  FindPoint = True;

VAR
  Line : Integer;
  NextPoint : Integer;
  TCFound : Boolean;

BEGIN
  TRY
    AdjoiningUpTrackCircuit := UnknownTrackCircuit;
    AdjoiningDownTrackCircuit := UnknownTrackCircuit;

    IF TC <> UnknownTrackCircuit THEN BEGIN
      Line := 0;
      TCFound := False;
      WHILE (Line <= High(Lines)) AND NOT TCFound DO BEGIN
        IF Lines[Line].Line_TC = TC THEN BEGIN
          TCFound := True;

          FollowThatLine(Line, TC, Up, NOT FindPoint, AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit, NextPoint);
          FollowThatLine(Line, TC, Down, NOT FindPoint, AdjoiningUpTrackCircuit, AdjoiningDownTrackCircuit, NextPoint);
        END;
        Inc(Line);
      END; {WHILE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG FindAdjoiningTrackCircuits:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { FindAdjoiningTrackCircuits }

PROCEDURE SetTrackCircuitStateMainProcedure(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType; Explanation : String);
{ Set whether and how the track circuit is occupied and give an explanation if any }
CONST
  DoNotWriteMessage = True;
  DiagramsLoading = True;

VAR
  AdjacentTrackCircuits : IntegerArrayType;
  AdjacentUpTC, AdjacentDownTC : Integer;
  ErrorMsg : String;
  I, J : Integer;
  LineArray : IntegerArrayType;
  Location : Integer;
  LocationOccupationCount : Integer;
  TempLocationLocoChips : IntegerArrayType;
  LocoFound : Boolean;
  OK : Boolean;
  P : Integer;
  T : TrainIndex;

BEGIN
  TRY
    IF (TC < 0) OR (TC > High(TrackCircuits)) THEN BEGIN
      ShowMessage('Non-existent TC=' + IntToStr(TC) + ' is trying to be set to ' + TrackCircuitStateToStr(NewState));
      Log('X Non-existent TC=' + IntToStr(TC) + ' is trying to be set to ' + TrackCircuitStateToStr(NewState));
      InvalidateScreen(UnitRef, 'SetTrackCircuitStateMainProcedure');
    END ELSE BEGIN
      WITH TrackCircuits[TC] DO BEGIN
        IF (NewState <> TrackCircuits[TC].TC_OccupationState) OR (LocoChip <> TrackCircuits[TC].TC_LocoChip) THEN BEGIN
          { Store the state in case we have to revert, but don't do it on both occupation and end of occupation or we can't revert }
          TC_PreviousOccupationState := TC_OccupationState;
          IF TC_LocoChip <> UnknownLocoChip THEN BEGIN
            Log('T TC=' + IntToStr(TC) + '''s TC_PreviousLocoChip set to ' + LocoChipToStr(TC_LocoChip));
            TC_PreviousLocoChip := TC_LocoChip;
          END;

          { If track circuit is now unoccupied, having been system occupied, see if part of a route needs clearing in advance of the full route tidying up in ClearARoute }
          IF NOT RedrawScreen THEN BEGIN
            T := GetTrainIndexFromLocoChip(TC_LocoChip);
            IF (TC_PreviousOccupationState = TCSystemOccupation)
            OR ((T <> UnknownTrainIndex)
                AND NOT Trains[T].Train_UseTrailingTrackCircuits
                AND (TC_PreviousOccupationState = TCFeedbackOccupation))
            THEN BEGIN
              TC_OccupationState := NewState;
              IF NewState = TCUnoccupied THEN BEGIN
                IF TrackCircuits[TC].TC_LockedForRoute <> UnknownRoute THEN BEGIN
                  FOR P := 0 TO High(Points) DO BEGIN
                    { using a FOR loop here, as there may be more than one point locked by the same TC route }
                    IF Points[P].Point_TCAtHeel = TC THEN BEGIN
                      IF Points[P].Point_TCAtHeel = TC THEN BEGIN
                        IF PointIsLockedByASpecificRoute(P, TrackCircuits[TC].TC_LockedForRoute) THEN BEGIN
                          Log(LocoChipToStr(TC_LocoChip) + ' R Point ' + IntToStr(P) + ' was locked by'
                                                         + ' R=' + IntToStr(TrackCircuits[TC].TC_LockedForRoute) + ' - now unlocked');
                          UnlockPointLockedBySpecificRoute(P, TrackCircuits[TC].TC_LockedForRoute, DoNotWriteMessage);

                          { and any related crossover or three-way points too }
                          IF Points[P].Point_RelatedPoint <> UnknownPoint THEN
                            IF PointIsLockedByASpecificRoute(Points[P].Point_RelatedPoint, TrackCircuits[TC].TC_LockedForRoute) THEN
                              UnlockPointLockedBySpecificRoute(Points[P].Point_RelatedPoint, TrackCircuits[TC].TC_LockedForRoute, DoNotWriteMessage);
                        END;
                      END;
                    END;
                  END; {FOR}

                  { and see if any route-locking drawing needs to be removed too }
                  LineArray := GetLinesForTrackCircuit(TC);
                  FOR I := 0 TO High(LineArray) DO BEGIN
                    IF Lines[LineArray[I]].Line_RouteLockingForDrawing = TrackCircuits[TC].TC_LockedForRoute THEN BEGIN
                      Log(LocoChipToStr(TC_LocoChip) + ' R L=' + LineToStr(LineArray[I]) + ' RouteLockingForDrawing was '
                                                     + IntToStr(Lines[LineArray[I]].Line_RouteLockingForDrawing) + ' now set to unknown route');
                      Lines[LineArray[I]].Line_RouteLockingForDrawing := UnknownRoute;
                    END;
                    IF Lines[LineArray[I]].Line_RouteSet <> - 1 THEN BEGIN
                      Log(LocoChipToStr(TC_LocoChip) + ' R L=' + LineToStr(LineArray[I]) + ' SubRouteSet set to False');
                      Lines[LineArray[I]].Line_RouteSet := UnknownRoute;
                    END;
                  END;

                  { or if a signal needs unlocking too }
                  FOR I := 0 TO High(TC_AdjacentSignals) DO BEGIN
                    IF SignalIsLockedBySpecificRoute(TC_AdjacentSignals[I], TrackCircuits[TC].TC_LockedForRoute) THEN BEGIN
                      Log(LocoChipToStr(TC_LocoChip) + ' R S=' + IntToStr(TC_AdjacentSignals[I]) + ' unlocked by R=' + IntToStr(TrackCircuits[TC].TC_LockedForRoute));
                      UnlockSignalLockedBySpecificRoute(TC_AdjacentSignals[I], TrackCircuits[TC].TC_LockedForRoute);
                    END;
                  END;

                  UnlockTrackCircuitRouteLocking(TC);
                END;
              END;
            END;

            TC_OccupationState := NewState;

            IF (NewState = TCOutOfUseSetByUser) OR (NewState = TCOutOfUseAsNoFeedbackReceived) THEN BEGIN
              Location := GetLocationFromTrackCircuit(TC);
              { reallocate any train expected to occupy the location }
              IF Location <> UnknownLocation THEN BEGIN
                FOR LocationOccupationCount := 0 TO High(LocationOccupations[Location]) DO BEGIN
                  { Store any locochip numbers now as we have to clear the array before we can reallocate them. (If we don't, it is possible that they will be reallocated
                    to the same, defunct, location.
                  }
                  IF LocationOccupations[Location, LocationOccupationCount].LocationOccupation_LocoChip <> UnknownLocoChip THEN
                    AppendToIntegerArray(TempLocationLocoChips, LocationOccupations[Location, LocationOccupationCount].LocationOccupation_LocoChip);
                END;

                { Now mark the location out of use }
                ClearLocationOccupationsForSpecificLocation(Location);
                SetLocationOccupationAllDayState(UnknownLocoIndex, Location, LocationOutOfUseOccupation, ErrorMsg, OK);
              END;

              IF Length(TempLocationLocoChips) > 0 THEN BEGIN
                FOR I := 0 TO High(TempLocationLocoChips) DO BEGIN
                  T := GetTrainIndexFromLocoChip(TempLocationLocoChips[I]);
                  IF T <> UnknownTrainIndex THEN
                    SetUpTrainLocationOccupationsAbInitio(T, OK);
                END;
              END;
            END;

            IF NewState <> TCUnoccupied THEN BEGIN
              TC_LocoChip := LocoChip;
              TC_Headcode := '????'; { **** }
            END ELSE BEGIN
              { TC is now unoccupied - reset its loco occupation - but not if it's a system occupation }
              IF (TC_LocoChip <> UnknownLocoChip) AND (NewState = TCUnoccupied) THEN BEGIN
                Log('T TC=' + IntToStr(TC) + ' TC_LocoChip=' + LocoChipToStr(TC_LocoChip) + ' set to unknown loco Chip');
                TC_LocoChip := UnknownLocoChip;
              END;

              TC_Headcode := '----';

              IF ((TrackCircuits[TC].TC_PreviousOccupationState = TCOutOfUseSetByUser)
              OR (TrackCircuits[TC].TC_PreviousOccupationState = TCOutOfUseAsNoFeedbackReceived))
              THEN BEGIN
                Location := GetLocationFromTrackCircuit(TC);
                ClearLocationOccupationsForSpecificLocation(Location);
              END;
            END;

            { Is this a signal-resetting track circuit? If so, reset the signal. And put in a timing update here - for delays **** }
            IF (NewState = TCFeedbackOccupation) AND (TC_ResettingSignal <> UnknownSignal) THEN BEGIN
              WITH Signals[TC_ResettingSignal] DO BEGIN
                IF Signal_Aspect <> RedAspect THEN BEGIN
                  { check first for any semaphore homes locked by semaphore distants, which would block the homes being reset }
                  CheckSemaphoreDistantBeforeSemaphoreHomeCleared(TC_ResettingSignal);
                  PullSignal(UnknownLocoChipStr, TC_ResettingSignal, NoIndicatorLit, NoRoute, NoSubRoute, UnknownLine, TC, NOT ByUser, OK);
                  IF OK THEN BEGIN
                    Log('S S=' + IntToStr(TC_ResettingSignal) + ' reset by TC=' + IntToStr(TC));
                    { also reset any hidden station signal aspects }
                    IF Signals[TC_ResettingSignal].Signal_HiddenStationSignalAspect <> RedAspect THEN BEGIN
                      Signals[TC_ResettingSignal].Signal_HiddenStationSignalAspect := NoAspect;
                      Signals[TC_ResettingSignal].Signal_PostColour := SignalPostColour;
                    END;
                  END ELSE BEGIN
                    IF NOT Signal_FailedToResetFlag THEN BEGIN
                      Log('SG S=' + IntToStr(TC_ResettingSignal) + ' failed to reset');
                      Signal_FailedToResetFlag := True;
                    END;
                  END;
                END; {WITH}
              END;
              TC_ResettingSignal := UnknownSignal;
            END;

            IF NOT InAutoMode
            AND ((NewState = TCFeedbackOccupation) OR (NewState = TCLocoOutOfPlaceOccupation))
            AND (TC_LocoChip = UnknownLocoChip)
            AND NOT ProgramStarting
            AND NOT InLocoSpeedTimingMode
            THEN BEGIN
              { we've not been given a loco, so see if we can work out which loco it is }
              Log('E TC=' + IntToStr(TC) + ' feedback occupied but no loco number supplied - searching adjacent TCs...');
              FindAdjoiningTrackCircuits(TC, AdjacentUpTC, AdjacentDownTC);

              LocoFound := False;
              I := 0;
              J := 0;
              { see if any adjacent track circuits are marked as occupied }
              SetLength(AdjacentTrackCircuits, 0);
              IF AdjacentUptc <> UnknownTrackCircuit THEN
                AppendToIntegerArray(AdjacentTrackCircuits, AdjacentUpTC);
              IF AdjacentDowntc <> UnknownTrackCircuit THEN
                AppendToIntegerArray(AdjacentTrackCircuits, AdjacentDownTC);
              WHILE (I <= High(AdjacentTrackCircuits)) AND NOT LocoFound DO BEGIN
                IF ((TrackCircuits[AdjacentTrackCircuits[I]].TC_OccupationState = TCFeedbackOccupation)
                    OR (TrackCircuits[AdjacentTrackCircuits[I]].TC_OccupationState = TCLocoOutOfPlaceOccupation))
                AND (TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip <> UnknownLocoChip)
                THEN BEGIN
                  Log('E ...adjacent TC=' + IntToStr(AdjacentTrackCircuits[I])
                         + ' is marked as feedback occupied by ' + LocoChipToStr(TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip)
                         + ', so maybe this is the loco we want');
                  { we've putatively found it }
                  LocoFound := True;
                  { but make sure the other adjacent TCs are unoccupied, or we won't know which loco it is that's moving }
                  WHILE J <= High(AdjacentTrackCircuits) DO BEGIN
                    IF I <> J THEN BEGIN
                      IF ((TrackCircuits[AdjacentTrackCircuits[J]].TC_OccupationState = TCFeedbackOccupation)
                         OR (TrackCircuits[AdjacentTrackCircuits[J]].TC_OccupationState = TCLocoOutOfPlaceOccupation))
                      AND (TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip <> UnknownLocoChip)
                      AND (TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip <> TrackCircuits[AdjacentTrackCircuits[J]].TC_LocoChip)
                      THEN BEGIN
                        LocoFound := False;
                        Log('E ...adjacent TC=' + IntToStr(AdjacentTrackCircuits[J])
                               + ' is marked as feedback occupied by ' + LocoChipToStr(TrackCircuits[AdjacentTrackCircuits[J]].TC_LocoChip)
                               + ', so do not know which loco to select');
                      END;
                    END;
                    Inc(J);
                  END; {WHILE}
                  IF NOT LocoFound THEN
                    Log('E ... but no loco found in adjacent TCs')
                  ELSE BEGIN
                    TC_LocoChip := TrackCircuits[AdjacentTrackCircuits[I]].TC_LocoChip;
                    T := GetTrainIndexFromLocoChip(TC_LocoChip);
                    IF T <> UnknownTrainIndex THEN
                      Trains[T].Train_CurrentTC := TC;
                    Log(LocoChipToStr(TC_LocoChip) + ' E ...found loco in adjacent TC=' + IntToStr(AdjacentTrackCircuits[I])
                                                   + ' so assuming it has moved to TC=' + IntToStr(TC));
                  END;
                END;
                Inc(I);
              END; {WHILE}
            END;
          END;

          IF Explanation <> '' THEN
            Log(LocoChipToStr(TC_LocoChip) + ' T Setting TC=' + IntToStr(TC) + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
                                           + ' ' + TrackCircuitStateToStr(NewState) + ' (' + Explanation + ')')
          ELSE
            Log(LocoChipToStr(TC_LocoChip) + ' T Setting TC=' + IntToStr(TC) + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
                                           + ' ' + TrackCircuitStateToStr(NewState));

          IF NOT ProgramStarting THEN
            InvalidateScreen(UnitRef, 'SetTrackCircuitStateMainProcedure');
        END;
      END; {WITH}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG SetTrackCircuitStateMainProcedure: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SetTrackCircuitStateMainProcedure }

PROCEDURE SetTrackCircuitState{1}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the track circuit is occupied, and mark it with a loco chip number }
CONST
  Explanation = '';

BEGIN
  SetTrackCircuitStateMainProcedure(LocoChip, TC, NewState, Explanation);
END; { SetTrackCircuitState-1 }

PROCEDURE SetTrackCircuitState{2}(LocoChip : Integer; TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the track circuit is occupied, mark it with a loco chip number, and give an explanation }
BEGIN
  SetTrackCircuitStateMainProcedure(LocoChip, TC, NewState, Explanation);
END; { SetTrackCircuitState-2 }

PROCEDURE SetTrackCircuitState{3}(TC : Integer; NewState : TrackCircuitStateType); Overload;
{ Set whether and how the track circuit is occupied }
CONST
  Explanation = '';

VAR
  LocoChip : Integer;
  T : TrainIndex;
  TCFound : Boolean;

BEGIN
  TRY
    { See if it belongs to a loco }
    LocoChip := UnknownLocoChip;
    TCFound := False;
    T := 0;
    WHILE (T <= High(Trains)) AND NOT TCFound DO BEGIN
      WITH Trains[T] DO BEGIN
        IF Train_DiagramFound THEN BEGIN
          IF Train_CurrentTC = TC THEN BEGIN
            TCFound := True;
            LocoChip := Train_LocoChip;
            Log('T TC=' + IntToStr(TC) + ' allocated to loco ' + LocoChipToStr(LocoChip) + ' as it is that loco''s Train_CurrentTC');
          END;
        END;
        Inc(T);
      END; {WITH}
    END; {WHILE}

    SetTrackCircuitStateMainProcedure(LocoChip, TC, NewState, Explanation);
  EXCEPT
    ON E : Exception DO
      Log('EG SetTrackCircuitState-3:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetTrackCircuitState-3 }

PROCEDURE SetTrackCircuitState{4}(TC : Integer; NewState : TrackCircuitStateType; Explanation : String); Overload;
{ Set whether and how the track circuit is occupied and give an explanation. Also see whether we want it recorded in the Location occupation array - this is to avoid
  duplicate recordings at startup.
}
BEGIN
  SetTrackCircuitStateMainProcedure(UnknownLocoChip, TC, NewState, Explanation);
END; { SetTrackCircuitState-4 }

PROCEDURE CalculateTCAdjacentBufferStops;
{ Work out which track circuits are next a buffer stop }
VAR
  L : Integer;
  TC : Integer;
  TCLocationFound : Boolean;

BEGIN
  FOR TC := 0 TO High(TrackCircuits) DO BEGIN
    TCLocationFound := False;
    L := 0;
    WHILE (L <= High(Lines)) AND NOT TCLocationFound DO BEGIN
      IF Lines[L].Line_TC = TC THEN BEGIN
        TCLocationFound := True;
        AppendToLineArray(TrackCircuits[TC].TC_LineArray, L);
        TrackCircuits[TC].TC_Location := Lines[L].Line_Location;

        { also initialise the buffer stop data }
        TrackCircuits[TC].TC_AdjacentBufferStop := Lines[L].Line_AdjacentBufferStop;

        { and the gradient - give the track circuit the benefit of the doubt so that if any line comprising it is not level, that gradient is the one recorded as being at
          the track circuit
        }
        IF Lines[L].Line_Gradient <> Level THEN
          TrackCircuits[TC].TC_Gradient := Lines[L].Line_Gradient;
      END;
      Inc(L);
    END; {WHILE}
  END; {FOR}
END; { CalculateTCAdjacentBufferStops }

PROCEDURE CalculateTCAdjacentSignals;
{ Work out which track circuits are next the signal and/or a bufferstop }
VAR
  S : Integer;
  TC : Integer;

BEGIN
  TRY
    FOR TC := 0 TO High(TrackCircuits) DO
      SetLength(TrackCircuits[TC].TC_AdjacentSignals, 0);

    FOR S := 0 TO High(Signals) DO
      IF Signals[S].Signal_AdjacentLine <> UnknownLine THEN
        AppendToIntegerArray(TrackCircuits[Lines[Signals[S].Signal_AdjacentLine].Line_TC].TC_AdjacentSignals, S);
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CalculateTCAdjacentSignals: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CalculateTCAdjacentSignals }

PROCEDURE InitialiseTrackCircuitVariables(TC : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }
BEGIN
  WITH TrackCircuits[TC] DO BEGIN
    SetLength(TC_AdjacentSignals, 0);
    TC_AdjacentBufferStop := UnknownBufferStop;
    TC_DataChanged := False;
    TC_EmergencyLocoChip := UnknownLocoChip;
    TC_EmergencyState := TCUnoccupied;
    TC_FeedbackOccupation := False;
    TC_Flashing := False;
    TC_Gradient := Level;
    TC_HeadCode := '';
    TC_Journey := UnknownJourney;
    TC_LitUp := True;
    TC_Location := UnknownLocation;
    TC_LockedForRoute := UnknownRoute;
    TC_LockFailureNotedInSubRouteUnit := False;
    TC_LocoChip := UnknownLocoChip;
    TC_LocoStalled := False;
    TC_MissingTrainNoted := False;
    TC_MysteriouslyOccupied := False;
    TC_OccupationStartTime := 0;
    TC_OccupationState := TCUnoccupied;
    TC_PreviousLocoChip := UnknownLocoChip;
    TC_PreviousOccupationState := TCUnoccupied;
    TC_ResettingSignal := UnknownSignal;
    TC_SaveRouteLocking := UnknownRoute;
    TC_SaveTrackCircuitHeadCode := '';
    TC_SpeedRestrictionInMPH := NoSpecifiedSpeed;
    TC_SpeedRestrictionDirection := Bidirectional;

    SetLength(TC_LineArray, 0);
  END; {WITH}
END; { InitialiseTrackCircuitVariables(TC); }

PROCEDURE ReadInTrackCircuitDataFromDatabase;
{ Initialise the track-circuit data which depends on lines being initialised first.

  Interesting notes from the past here:

  Unit 77 inputs 5-8 free
  Unit 78 (tunnel) for UMC
  Unit 86 input 5 does not have a corresponding LB101 as 6-8 are photocell inputs
  Unit 94 input 6 reserved for new siding point (1 to 5 are points already)
  Unit 112 input 8 (FY) is free [TC input]

  Unit 93 inputs 2-3 reserved for new siding A
  Unit 95 inputs 5-6 reserved for new siding B
  Unit 99 inputs 3-4 reserved for cross over points from platforms 5 to 6
  Unit 116 input 2 free [point input]
}
CONST
  StopTimer = True;

VAR
  ErrorMsg : String;
  HasFeedback : Boolean;
  Location : Integer;
  TC : Integer;
  TempStr : String;

BEGIN
  TRY
    Log('A INITIALISING TRACK CIRCUITS {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track-circuit database file "' + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInTrackCircuitDataFromDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;
      Log('T Track-circuit data table and connection opened to initialise the track circuits');

      TrackCircuitsADOTable.Sort := '[' + TC_NumberFieldName + '] ASC';
      TrackCircuitsADOTable.First;
      SetLength(TrackCircuits, 0);

      WHILE NOT TrackCircuitsADOTable.EOF DO BEGIN
        WITH TrackCircuitsADOTable DO BEGIN
          SetLength(TrackCircuits, Length(TrackCircuits) + 1);
          TC := High(TrackCircuits);
          WITH TrackCircuits[TC] DO BEGIN
            ErrorMsg := '';

            TC_Number := TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsInteger;
            IF TC_Number <> TC THEN
              ErrorMsg := 'it does not match the line number in the database (' + IntToStr(TC_Number) + ')';

            IF ErrorMsg = '' THEN BEGIN
              TempStr := FieldByName(TC_LengthInInchesFieldName).AsString;
              IF TempStr = '' THEN
                TC_LengthInInches := 0
              ELSE
                IF NOT TryStrToFloat(TempStr, TC_LengthInInches) THEN
                  Debug('TC=' + IntToStr(TC_Number) + ': invalid length String "' + TempStr + '"');
            END;

            IF ErrorMsg = '' THEN
              TC_FeedbackUnit := ValidateFeedbackUnit(FieldByName(TC_FeedbackUnitFieldName).AsString, HasFeedback, ErrorMsg);

            IF ErrorMsg = '' THEN
              TC_FeedbackInput := ValidateFeedbackInput(FieldByName(TC_FeedbackInputFieldName).AsString, HasFeedback, TC_FeedbackUnit, TrackCircuitFeedback,
                                                        TC_Number, ErrorMsg);
            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating TC=' + IntToStr(High(TrackCircuits)) + ' (' + IntToStr(TC_Number) + '): '
                                            + '[' + ErrorMsg + ']:'
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
              THEN
                ShutDownProgram(UnitRef, 'ReadInTrackCircuitDataFromDatabase');
            END;
          END; {WITH}
        END; {WITH}
        TrackCircuitsADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('T Track-circuit data table and connection closed');
    END; {WITH}

    FOR TC := 0 TO High(TrackCircuits) DO
      InitialiseTrackCircuitVariables(TC);

    { Initialise track-circuit lines and locations }
    CalculateTCAdjacentBufferStops;

    { Check that all track circuits have a TC_LineArray value }
    TC := 0;
    WHILE TC <= High(TrackCircuits) DO BEGIN
      IF Length(TrackCircuits[TC].TC_LineArray) = 0 THEN
        Debug('!TC=' + IntToStr(TC) + ': has no entry in the LineData file');
      Inc(TC);
    END; {WHILE}

    { Work out how long individual locations are based on track-circuit length. This can be overriden by a specific length given below, however. }
    FOR Location := 0 TO High(Locations) DO BEGIN
      { Check that the location doesn't already have a designated preset length - this is necessary in platforms, for instance, as the total track-circuit length may well
        include the line beyond signals.
      }
      IF Locations[Location].Location_LengthInInches = 0 THEN
        FOR TC := 0 TO High(TrackCircuits) DO BEGIN
          IF TrackCircuits[TC].TC_Location = Location THEN BEGIN
            Locations[Location].Location_LengthInInches := Locations[Location].Location_LengthInInches + TrackCircuits[TC].TC_LengthInInches;
          END;
        END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInTrackCircuitDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadInTrackCircuitDataFromDatabase }

PROCEDURE AddNewRecordToTrackCircuitDatabase;
{ Append a record to the track-circuit database }
BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track-circuit database file "' + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                      + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'AddNewRecordToTrackCircuitDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;
      TrackCircuitsADOTable.Append;
      TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsInteger := High(TrackCircuits);
      TrackCircuitsADOTable.Post;

      Log('S Track-circuit data table and connection opened to create record for TrackCircuit ' + IntToStr(High(TrackCircuits)));
      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('S Track-circuit data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AddNewRecordToTrackCircuitDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { AddNewRecordToTrackCircuitDatabase }

PROCEDURE WriteOutTrackCircuitDataToDatabase;
{ Write out some track-circuit data to the track-circuit data file }
VAR
  TC : Integer;

BEGIN
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track-circuit database file "' + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                      + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'WriteOutTrackCircuitDataFromDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;
      Log('T Track-circuit data table and connection opened to write out track-circuit data');

      TrackCircuitsADOTable.First;
      TC := 0;

      WHILE TC <= High(TrackCircuits) DO BEGIN
        WITH TrackCircuits[TC] DO BEGIN
          IF TC_DataChanged THEN BEGIN
            TC_DataChanged := False;

            IF NOT TrackCircuitsADOTable.Locate(TC_NumberFieldName, IntToStr(TC), []) THEN BEGIN
              Log('T Track-circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TC) + ' but it cannot be found');
            END ELSE BEGIN
              Log('T Track-circuit data table and connection opened to record that ' + TC_NumberFieldName + ' is ''' + IntToStr(TC_Number) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_NumberFieldName).AsString := IntToStr(TC_Number);
              TrackCircuitsADOTable.Post;

              Log('T Track-circuit data table and connection opened to record that ' + TC_LengthInInchesFieldName + ' is ''' + FloatToStr(TC_LengthInInches) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_LengthInInchesFieldName).AsString := FloatToStr(TC_LengthInInches);
              TrackCircuitsADOTable.Post;

              Log('T Track-circuit data table and connection opened to record that ' + TC_FeedbackUnitFieldName + ' is ''' + IntToStr(TC_FeedbackUnit) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_FeedbackUnitFieldName).AsString := IntToStr(TC_FeedbackUnit);
              TrackCircuitsADOTable.Post;

              Log('T Track-circuit data table and connection opened to record that ' + TC_FeedbackInputFieldName + ' is ''' + IntToStr(TC_FeedbackInput) + '''');
              TrackCircuitsADOTable.Edit;
              TrackCircuitsADOTable.FieldByName(TC_FeedbackInputFieldName).AsString := IntToStr(TC_FeedbackInput);
              TrackCircuitsADOTable.Post;
            END;
          END;
        END; {WITH}

        Inc(TC);
      END; {WHILE}

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('L Track-circuit data table and connection closed after writing track-circuit data');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutTrackCircuitData: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutTrackCircuitDataToDatabase }

FUNCTION DeleteRecordFromTrackCircuitDatabase(TrackCircuitToDeleteNum : Integer) : Boolean;
{ Remove a record from the track-circuit database }
BEGIN
  Result := False;
  TRY
    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Track-circuit database file "' + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                      + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'DeleteRecordFromTrackCircuitDatabase')
        ELSE
          Exit;
      END;

      TrackCircuitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                     + PathToRailDataFiles + TrackCircuitDataFilename + '.' + TrackCircuitDataFilenameSuffix
                                                     + ';Persist Security Info=False';
      TrackCircuitsADOConnection.Connected := True;
      TrackCircuitsADOTable.Open;

      IF NOT TrackCircuitsADOTable.Locate(TC_NumberFieldName, IntToStr(TrackCircuitToDeleteNum), []) THEN BEGIN
        Log('T Track-circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TrackCircuitToDeleteNum) + ' but it cannot be found');
      END ELSE BEGIN
        Log('T Track-circuit data table and connection opened to delete TC ' + TrackCircuitToStr(TrackCircuitToDeleteNum));

        { Now delete the track circuit - we have already checked, in the Edit unit, whether deleting it will cause knock-on problems with other track circuits }
        TrackCircuitsADOTable.Delete;
        Log('TG TrackCircuit ' + IntToStr(TrackCircuitToDeleteNum) + ' has been deleted');
        Result := True;
      END;

      { Tidy up the database }
      TrackCircuitsADOTable.Close;
      TrackCircuitsADOConnection.Connected := False;
      Log('T Track-circuit data table and connection closed');
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG DeleteRecordFromTrackCircuitDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DeleteRecordFromTrackCircuitDatabase }

FUNCTION GetTrackCircuitsForLocation(Location : Integer) : IntegerArrayType;
{ Return all the track circuits for a given location }
VAR
  L : Integer;

BEGIN
  SetLength(Result, 0);
  IF Location <> UnknownLocation THEN BEGIN
    FOR L := 0 TO High(Lines) DO BEGIN
      { only append a new TC if it's not there already }
      IF Lines[L].Line_Location = Location THEN BEGIN
        IF (Length(Result) = 0) OR (Result[High(Result)] <> Lines[L].Line_TC) THEN
          IF Lines[L].Line_TC <> UnknownTrackCircuit THEN
            AppendToIntegerArray(Result, Lines[L].Line_TC)
      END;
    END;
  END;
END; { GetTrackCircuitsForLocation }

END { TrackCircuitsUnit }.
