UNIT Train;
{ Routines that deal specifically with whole trains as opposed to individual locomotives

  v1.0  04/07/14 First written
}

INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, InitVars;

TYPE
  TTrainForm = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

  TrainIndex = Integer;

  { Note: we need Missing, Suspended, and MissingAndSuspended as otherwise the status can oscillate between Missing and Suspended if a train is suspended while missing - in
    that case, unsuspending renders it missing even though it may no longer be missing, and it's then a status we can't get out of.
  }
  TrainStatusType = (ReadyForCreation, WaitingForLightsOn, WaitingForHiddenStationSignalAspectToClear, WaitingForRouteing, InLightsOnTime, ReadyForRouteing,
                     CommencedRouteing, ReadyToDepart, Departed, RouteingWhileDeparted, RouteCompleted, WaitingForRemovalFromDiagrams, ToBeRemovedFromDiagrams,
                     RemovedFromDiagrams, Missing, MissingAndSuspended, Suspended, NonMoving, Cancelled, UnknownTrainStatus);
  TypeOfTrainType = (LightLocoType, ExpressPassengerType, OrdinaryPassengerType, ExpressFreightType, Freight75mphType, EmptyCoachingStockType, Freight60mphType,
                     Freight45mphType, Freight35mphType, InternationalType, UnknownTrainType);
  TrainTypeArray = ARRAY OF TypeOfTrainType;

VAR
  TrainForm: TTrainForm;

PROCEDURE ChangeTrainStatus(T : TrainIndex; NewStatus : TrainStatusType);
{ Change the current train status and record it }

FUNCTION GetTrainIndexFromLocoChip(LocoChip : Integer): TrainIndex;
{ Look for a matching train record given a locochip }

FUNCTION GetTrainTypeFromLocoChip(LocoChip : Integer) : TypeOfTrainType;
{ Returns the train type given the loco number }

PROCEDURE InitialiseTrainRecord(T : TrainIndex);
{ Do the initialisations, or reinitialisations if specified }

PROCEDURE ReturnTrainFromMissing(T : TrainIndex);
{ Set a train as being no longer missing }

PROCEDURE SetTrainDirection(T : TrainIndex; DirectionRequired : DirectionType; ForceWrite : Boolean; VAR OK : Boolean);
{ Change the direction for a train's locos }

PROCEDURE StopAParticularTrain(T : TrainIndex);
{ Stops just one train }

FUNCTION TrainHasCabLights(T : TrainIndex) : Boolean;
{ Returns true if one or both of a train's locos have cab lights }

FUNCTION TrainHasLightsOperatedByTwoChips(T : TrainIndex) : Boolean;
{ Returns true if one or both of a train's lights are operated by two chips }

PROCEDURE TurnTrainCabLightsOff(T : TrainIndex; OUT OK : Boolean);
{ Turn the cab lights off for a train's locos, if they have them }

PROCEDURE TurnTrainCabLightsOn(T : TrainIndex; OUT OK : Boolean);
{ Turn the cab lights on for a train's locos, if they have them }

PROCEDURE TurnTrainLightsOff(T : TrainIndex; OUT OK : Boolean);
{ Turn the lights off for a train's locos }

PROCEDURE TurnTrainLightsOn(T : TrainIndex; OUT OK : Boolean);
{ Turn the lights on for a train's locos }

TYPE
  { Train-related type declarations }
  TrainJourneyRec = RECORD
    TrainJourney_ActualArrivalTime : TDateTime;
    TrainJourney_ActualDepartureTime : TDateTime;
    TrainJourney_AdditionalRequiredStationWaitInMinutes : Integer;
    TrainJourney_Cleared : Boolean;
    { TrainJourney_Commenced : Boolean; - use TrainJourney_ActualDepartureTime <> 0 instead }
    TrainJourney_Created : Boolean;
    TrainJourney_CurrentArrivalTime : TDateTime;
    TrainJourney_CurrentDepartureTime : TDateTime;
    TrainJourney_DiagrammedArrivalTime : TDateTime;
    TrainJourney_DiagrammedDepartureTime : TDateTime;
    TrainJourney_DiagrammedStartLocation : Integer;
    TrainJourney_DiagrammedEndLocation : Integer;
    TrainJourney_Direction : DirectionType;
    TrainJourney_DurationInMinutes : Integer;
    TrainJourney_EndArea : Integer;
    TrainJourney_EndBufferStop : Integer;
    TrainJourney_EndLine : Integer;
    TrainJourney_EndLocation : Integer;
    TrainJourney_EndStationName : String;
    TrainJourney_EndSignal : Integer;
    TrainJourney_FirstTC : Integer;
    TrainJourney_LengthInInches : Real;
    TrainJourney_LocationsPending : Boolean;
    TrainJourney_LockingArray : StringArrayType;
    TrainJourney_NotForPublicUse : Boolean;
    TrainJourney_Route : Integer;
    TrainJourney_RouteArray : StringArrayType;
    TrainJourney_SetUp : Boolean;
    TrainJourney_StartArea : Integer;
    TrainJourney_StartLine : Integer;
    TrainJourney_StartLocation : Integer;
    TrainJourney_StartStationName : String;
    TrainJourney_StartOfRepeatJourney : Boolean;
    TrainJourney_StartSignal : Integer;
    TrainJourney_StoppingOnArrival : Boolean;
    TrainJourney_UserToDrive : Boolean;
  END;

  TrainJourneyRecArrayType = ARRAY OF TrainJourneyRec;

  TrainRec = RECORD
    Train_LocoChip : Integer;
    Train_DoubleHeaderLocoChip : Integer;
    Train_LocoChipStr : String; { from loco record }
    Train_DoubleHeaderLocoChipStr : String; { from loco record }
    Train_LocoIndex : LocoIndex;
    Train_DoubleHeaderLocoIndex : LocoIndex;

//    Train_Accelerating : Boolean;
//    Train_AccelerationAdjustRange : Integer;
//    Train_AccelerationStartTime : TDateTime;
//    Train_AccelerationStr : String;
    Train_AccelerationTimeInSeconds : Real;
//    Train_AccelerationTimeInterval : Real;
    Train_ActualNumStr : String; { from loco record }
    Train_AtCurrentBufferStop : Integer;
    Train_AtCurrentSignal : Integer;
    Train_AtHiddenStationSignalAspectSignal : Integer; { to stop trains at signals that otherwise would be off }
    Train_BeingAdvanced : Boolean;
    Train_BeingAdvancedTC : Integer;
    Train_CabLightsAreOn : Boolean;
    Train_CabLightsHaveBeenOn : Boolean;
//    Train_ControlState : LocoControlStateType;
    Train_CurrentArrivalTime : TDateTime;
    Train_CurrentBufferStop : Integer;
    Train_CurrentDirection : DirectionType;
    Train_CurrentJourney : Integer;
    Train_CurrentLengthInInches : Integer;
    Train_CurrentRoute : Integer;
    Train_CurrentSignal : Integer;
    Train_CurrentSourceLocation : Integer;
    Train_CurrentSpeedInMPH : MPHType;
    Train_CurrentStatus : TrainStatusType;
    Train_CurrentTC : Integer;
//    Train_Decelerating : Boolean;
    Train_Description : String;
    Train_DesiredSpeedInMPH : MPHType;
    Train_DiagramFound : Boolean;
    Train_DiagramsGridRowNums : IntegerArrayType;
    Train_DistanceToCurrentSignalOrBufferStop : Real;
    Train_DistanceToNextSignalButOneOrBufferStop : Real;
    Train_DistanceToNextSignalOrBufferStop : Real;
    Train_EmergencyRouteing : Boolean;
    Train_ExtraPowerAdjustment : Integer; { used temporarily to increase the train speed where necessary }
    Train_FirstStationSpecifiedStartTime : TDateTime;
    Train_FixedDirection : DirectionType;
    Train_FixedLengthInInches : Integer; { from loco record }
//    Train_Functions : ARRAY [0..12] OF Boolean;
//    Train_Functions0To4Byte : Byte;
//    Train_Functions5To12Byte : Byte;
    Train_GradientSpeedAdjustment : Integer;
    Train_GradientSpeedAdjustmentMsgWritten : Boolean;
    Train_HasLights : Boolean;
    Train_Headcode : String;
    Train_InitialTrackCircuits : ARRAY [1..5] OF Integer;
    Train_InLightsOnTime : Boolean; { train inactive but for lights being on }
    Train_JourneysArray : TrainJourneyRecArrayType;
    Train_LastLengthInInches : Integer; { from loco record }
    Train_LastLocation : Integer;
    Train_LastMissingTC : Integer;
    Train_LastRouteLockedMsgStr : String;
    Train_LastSignal : Integer;
    Train_LastTC : Integer; { from loco record }
    Train_LightsOn : Boolean;
    Train_LightsOnTime : TDateTime;
    Train_LightsRemainOnWhenJourneysComplete : Boolean;
    Train_LocatedAtStartup : Boolean;
    Train_Locations : IntegerArrayType;
    Train_LocoClassStr : String; { from loco record }
    Train_LocoName : String; { from loco record }
    Train_LocoTypeStr : String; { from loco record }
    Train_MaximumSpeedInMPH : MPHType;
    Train_MinimumAccelerationTimeInSeconds : Integer; { needed as we only calculate it once when we enter a track circuit }
    Train_MissingMessage : Boolean;
    Train_MissingNum : Integer;
    Train_NextTC : Integer;
    Train_NextButOneTC : Integer;
    Train_NotInPlaceMsgWritten : Boolean;
    Train_NotLocatedAtStartupMsgWritten : Boolean;
    Train_NumberOfCarriages : Integer;
    Train_PossibleRerouteTime : TDateTime;
//    Train_PreviousControlState : LocoControlStateType;
    Train_PreviousStatus : TrainStatusType;
    Train_PreviousTC : Integer;
    Train_Reversing : Boolean;
    Train_ReversingDelayInSeconds : Cardinal;
    Train_ReversingStartTime : TDateTime;
    Train_ReversingWaitStarted : Boolean;
    Train_RouteCheckedTime : TDateTime;
    Train_RouteCreationHeldJourney : Integer;
    Train_RouteCreationHeldMsgWrittenArray : ARRAY [FirstRouteCreationHeldMsgNumber..LastRouteCreationHeldMsgNumber] OF Boolean;
    Train_RouteCreationHoldNum : Integer;
    Train_RouteCreationHoldMsg : String;
    Train_RouteCreationPlatformHeldStr : String;
    Train_RouteCreationReleasedMsg : String;
    Train_RouteingHeldAtSignal : Integer;
    Train_SaveCurrentTC : Integer;
    Train_SavedLocation : Integer;
    Train_SavedRoute : Integer;
    Train_SaveSpeedInFiddleyardMsg : String;
    Train_SaveTCsClearedStr : String;
    Train_SaveTCsForReleaseStr : String;
    Train_SaveTCsOccupiedStr : String;
    Train_SectionStartTime : TDateTime;
    Train_SpeedString : String;
    Train_StalledMsgWritten : Boolean;
    Train_SubRouteAheadCheckedTime : TDateTime;
    Train_TakenOverByUserMsgWritten : Boolean;
    Train_TCsAndSignalsNotClearedArray : StringArrayType;
    Train_TCsAndSignalsNotClearedStr : String;
    Train_TCsNotClearedArray : StringArrayType;
    Train_TCsNotClearedStr : String;
    Train_TCsOccupiedOrClearedArray : StringArrayType;
    Train_TCsOccupiedOrClearedStr : String;
    Train_TCsReleasedArray : StringArrayType;
    Train_TCsReleasedStr : String;
    Train_TempDraftRouteArray : StringArrayType;
    Train_TempLockingArray : StringArrayType;
    Train_TerminatingSpeedReductionMsgWritten : Boolean;
    Train_TotalJourneys : Integer; { starts at 0 }
    Train_Type : TypeOfTrainType;
    Train_TypeNum : Integer;
    Train_UserDriving : Boolean;
    Train_UserPowerAdjustment : Integer; { used by the user to increase or decrease the train speed where necessary }
    Train_UserRequiresInstructions : Boolean;
    Train_UserSpeedInstructionMsg : String;
    Train_UseTrailingTrackCircuits : Boolean;
    { where a train doesn't have lights at both ends, it may need artificial track-circuit activation }
    Train_WaitingForHiddenStationSignalAspectStartTime : TDateTime;
    Train_WorkingTimetableLastArrivalArea : Integer;
    Train_WorkingTimetableLastArrivalTime : TDateTime;
    Train_WorkingTimetableLastEntryNumStr : String;
  END;

  LightsToBeSwitchedOnRec = RECORD
    LightsToBeSwitchedOn_Train: TrainIndex;
    LightsToBeSwitchedOn_ColourStr1 : String;
    LightsToBeSwitchedOn_ColourStr2 : String;
    LightsToBeSwitchedOn_Direction1 : DirectionType;
    LightsToBeSwitchedOn_Direction2 : DirectionType;
    LightsToBeSwitchedOn_SwitchOnTime : TDateTime;
  END;

  TrainArrayType = ARRAY OF TrainIndex;

VAR
  LightsToBeSwitchedOnArray : ARRAY OF LightsToBeSwitchedOnRec;
  TempTrainArray : ARRAY OF TrainIndex;
  Trains : ARRAY OF TrainRec;

IMPLEMENTATION

{$R *.dfm}

USES Lenz, MiscUtils, Diagrams, TrackCircuitsUnit;

CONST
  UnitRef = 'Train';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE StopAParticularTrain(T : TrainIndex);
{ Stops just one train }
VAR
  DebugStr : String;
  OK : Boolean;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('StopAParticularTrain')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF SystemOnline THEN BEGIN
        DebugStr := 'Train stop requested';
        StopAParticularLocomotive(Locos[Train_LocoIndex], OK);
        IF Train_DoubleHeaderLocoChip <> UnknownLocoChip THEN BEGIN
          StopAParticularLocomotive(Locos[Train_DoubleHeaderLocoIndex], OK);
          DebugStr := DebugStr + '. DH Loco ' + LocoChipToStr(Train_DoubleHeaderLocoChip) + ' also stopped';
        END;

        Log(Train_LocoChipStr + ' L ' + DebugStr);
      END;
    END; {WITH}
  END;
END; { StopAParticularTrain }

PROCEDURE ReturnTrainFromMissing(T : TrainIndex);
{ Set a train as being no longer missing }
VAR
  TC : Integer;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('ReturnTrainFromMissing')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF Train_CurrentStatus = MissingAndSuspended THEN
        ChangeTrainStatus(T, Suspended)
      ELSE
        IF Train_CurrentStatus = Missing THEN
          ChangeTrainStatus(T, Train_PreviousStatus);

      Train_MissingMessage := False;
      Train_LastMissingTC := Train_CurrentTC;
      Dec(MissingTrainCounter);

      FOR TC := 0 TO High(TrackCircuits) DO BEGIN
        IF (TrackCircuits[TC].TC_LocoChip = Train_LocoChip) AND (TrackCircuits[TC].TC_OccupationState = TCMissingOccupation) THEN BEGIN
          TrackCircuits[TC].TC_MissingTrainNoted := False;
          SetTrackCircuitState(Train_LocoChip, TC, TCFeedbackOccupation);
        END;
      END;

      Log(Train_LocoChipStr + ' LG Train has been restarted');
      DrawDiagramsStatusCell(T, NormalStyle);
    END; {WITH}
  END;
END; { ReturnTrainFromMissing }

FUNCTION GetTrainTypeFromLocoChip(LocoChip : Integer) : TypeOfTrainType;
{ Returns the train type given the loco number }
VAR
  LocoChipFound : Boolean;
  T : TrainIndex;

BEGIN
  Result := UnknownTrainType;
  T := 0;
  LocoChipFound := False;
  WHILE (T <= High(Trains)) AND NOT LocoChipFound DO BEGIN
    IF Trains[T].Train_LocoChip = LocoChip THEN BEGIN
      LocoChipFound := True;
      Result := Trains[T].Train_Type;
    END;
   Inc(T);
  END; {WHILE}
END; { GetTrainTypeFromLocoChip }

FUNCTION GetTrainIndexFromLocoChip(LocoChip : Integer) : TrainIndex;
{ Look for a matching train index given a locochip }
VAR
  T : TrainIndex;
  TrainFound : Boolean;

BEGIN
  Result := UnknownTrainIndex;
  TRY
    IF LocoChip = UnknownLocoChip THEN
      Exit
    ELSE BEGIN
      T := 0;
      TrainFound := False;
      WHILE (T <= High(Trains)) AND NOT TrainFound DO BEGIN
        { run through the train list, to find our train }
        IF Trains[T].Train_LocoChip = LocoChip THEN
          TrainFound := True
        ELSE
          Inc(T);
      END; {WHILE}

      IF TrainFound THEN
        Result := T;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG GetTrainIndexFromLocoChip: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { GetTrainIndexFromLocoChip }

PROCEDURE ChangeTrainStatus(T : TrainIndex; NewStatus : TrainStatusType);
 { Change the current train status and record it }
VAR
  DebugStr : String;
  OK : Boolean;

BEGIN
  DebugStr := '';

  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('ChangeTrainStatus')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      { we need to make special provision for missing trains, as their status can switch back and fore continuously from Missing to MissingAndSuspended }
      IF (Train_CurrentStatus = Missing) OR (Train_CurrentStatus = MissingAndSuspended) THEN
        DebugStr := ' (immediate previous status was ' + TrainStatusToStr(Train_CurrentStatus) + ')'
      ELSE
        Train_PreviousStatus := Train_CurrentStatus;

      Train_CurrentStatus := NewStatus;
      Log(Train_LocoChipStr + ' L New status: ' + TrainStatusToStr(Train_CurrentStatus)
                            + '; previous status: ' + TrainStatusToStr(Train_PreviousStatus) + DebugStr);
      { update the diagrams window }
      DrawDiagramsStatusCell(T, Normalstyle);

      { And make any necessary changes to the train record, etc. }
      CASE Train_CurrentStatus OF
        ToBeRemovedFromDiagrams:
          BEGIN
            { Take it off the diagram grid }
            RemoveTrainFromDiagrams(T);
            ChangeTrainStatus(T, RemovedFromDiagrams);
            DrawDiagrams(UnitRef, 'Change Train Status');

            { Deal with the loco's lights (if any) }
            IF NOT Train_LightsRemainOnWhenJourneysComplete THEN
              TurnTrainLightsOff(T, OK);

            IF TrainHasCablights(T) AND Train_CabLightsAreOn THEN
              TurnTrainCabLightsOff(T, OK);
          END;
      END; {CASE}
    END; {WITH}
  END;
END; { ChangeTrainStatus }

PROCEDURE InitialiseTrainRecord(T : TrainIndex);
{ Do the initialisations, or reinitialisations if specified }
VAR
  I : Integer;

BEGIN
  TRY
    IF T = UnknownTrainIndex THEN
      UnknownTrainRecordFound('InitialiseTrainRecord')
    ELSE BEGIN
      WITH Trains[T] DO BEGIN
        { Set up defaults }
        Train_LocoChip := UnknownLocoChip;
        Train_LocoChipStr := '';
        Train_DoubleHeaderLocoChip := UnknownLocoChip;
        Train_DoubleHeaderLocoChipStr := '';
        Train_LocoIndex := UnknownLocoIndex;
        Train_DoubleHeaderLocoIndex := UnknownLocoIndex;

  //      Train_Accelerating := False;
  //      Train_AccelerationAdjustRange := 0;
  //      Train_AccelerationStartTime := 0;
  //      Train_AccelerationStr := '';
        Train_AccelerationTimeInSeconds := 0.0;
  //      Train_AccelerationTimeInterval := 0.0;
        Train_ActualNumStr := '';
        Train_AtCurrentBufferStop := UnknownBufferStop;
        Train_AtCurrentSignal := UnknownSignal;
        Train_AtHiddenStationSignalAspectSignal := UnknownSignal;
        Train_BeingAdvanced := False;
        Train_BeingAdvancedTC := UnknownTrackCircuit;
        Train_CabLightsAreOn := False;
        Train_CabLightsHaveBeenOn := False;
//        Train_ControlledByState := ControlledByProgram;
        Train_CurrentArrivalTime := 0;
        Train_CurrentBufferStop := UnknownBufferStop;
        Train_CurrentDirection := UnknownDirection;
        Train_CurrentJourney := 0;
        Train_CurrentLengthInInches := 0;
        Train_CurrentRoute := UnknownRoute;
        Train_CurrentSignal := UnknownSignal;
        Train_CurrentSourceLocation := UnknownLocation;
        Train_CurrentSpeedInMPH := MPH0;
        Train_CurrentStatus := UnknownTrainStatus;
        Train_CurrentTC := UnknownTrackCircuit;
  //      Train_Decelerating := False;
        Train_Description := '';
        Train_DesiredSpeedInMPH := MPH0;
        Train_DiagramFound := False;
        Train_DistanceToCurrentSignalOrBufferStop := 0.0;
        Train_DistanceToNextSignalButOneOrBufferStop := 0.0;
        Train_DistanceToNextSignalOrBufferStop := 0.0;
        Train_EmergencyRouteing := False;
        Train_ExtraPowerAdjustment := 0; { used temporarily to increase the train speed where necessary }
        Train_FirstStationSpecifiedStartTime := 0;
        Train_FixedDirection := UnknownDirection;
        Train_FixedLengthInInches := 0;
  //      FOR I := 0 TO 12 DO
  //        Train_Functions[I] := False;
  //      Train_Functions0To4Byte := 0;
  //      Train_Functions5To12Byte := 0;
        Train_GradientSpeedAdjustment := 0;
        Train_GradientSpeedAdjustmentMsgWritten := False;
        Train_HasLights := False;
        Train_Headcode := '';
        FOR I := 1 TO 5 DO
          Train_InitialTrackCircuits[I] := UnknownTrackCircuit;
        Train_InLightsOnTime := False; { train inactive but for lights being on }
        SetLength(Train_JourneysArray, 0);
        Train_LastLengthInInches := 0;
        Train_LastLocation := UnknownLocation;
        Train_LastMissingTC := UnknownTrackCircuit;
        Train_LastRouteLockedMsgStr := '';
        Train_LastSignal := UnknownSignal;
        Train_LastTC := UnknownTrackCircuit;
        Train_LightsOn := False;
        Train_LightsOnTime := 0;
        Train_LightsRemainOnWhenJourneysComplete := False;
        Train_LocatedAtStartup := True;
        SetLength(Train_Locations, 0);
        Train_LocoClassStr := '';
        Train_LocoName := '';
        Train_LocoTypeStr := '';
        Train_MaximumSpeedInMPH := MPH0;
        Train_MinimumAccelerationTimeInSeconds := 0; { needed as we only calculate it once when we enter a track circuit }
        Train_MissingMessage := False;
        Train_MissingNum := 0; { set if train missing, and used to restart train if found }
        Train_NextTC := UnknownTrackCircuit;
        Train_NextButOneTC := UnknownTrackCircuit;
        Train_NotInPlaceMsgWritten := False;
        Train_NotLocatedAtStartupMsgWritten := False;
        Train_NumberOfCarriages := 0;
        Train_PossibleRerouteTime := 0;
//        Train_PreviouslyControlledByState := ControlledByProgram;
        Train_PreviousStatus := ReadyForCreation;
        Train_PreviousTC := UnknownTrackCircuit;
        Train_Reversing := False;
        Train_ReversingDelayInSeconds := 0;
        Train_ReversingStartTime := 0;
        Train_ReversingWaitStarted := False;
        Train_RouteCheckedTime := 0;
        Train_RouteCreationHeldJourney := UnknownJourney;
        Train_RouteCreationHoldMsg := '';
        FOR I := 1 TO 9 DO
          Train_RouteCreationHeldMsgWrittenArray[I] := False;
        Train_RouteCreationHoldNum := 0;
        Train_RouteCreationPlatformHeldStr := '';
        Train_RouteCreationReleasedMsg := '';
        Train_RouteingHeldAtSignal := UnknownSignal;
        Train_SaveCurrentTC := UnknownTrackCircuit;
        Train_SavedLocation := UnknownLocation;
        Train_SavedRoute := UnknownRoute;
        Train_SaveSpeedInFiddleyardMsg := '';
        Train_SaveTCsClearedStr := '';
        Train_SaveTCsForReleaseStr := '';
        Train_SaveTCsOccupiedStr := '';
        Train_SectionStartTime := 0;
        Train_StalledMsgWritten := False;
        Train_TakenOverByUserMsgWritten := False;

        SetLength(Train_TCsAndSignalsNotClearedArray, 0);
        Train_TCsAndSignalsNotClearedStr := '';

        SetLength(Train_TCsNotClearedArray, 0);
        Train_TCsNotClearedStr := '';

        SetLength(Train_TCsOccupiedOrClearedArray, 0);
        Train_TCsOccupiedOrClearedStr := '';

        SetLength(Train_TCsReleasedArray, 0);
        Train_TCsReleasedStr := '';

        SetLength(Train_TempDraftRouteArray, 0);
        SetLength(Train_TempLockingArray, 0);
        Train_TerminatingSpeedReductionMsgWritten := False;
        SetLength(Train_DiagramsGridRowNums, 0);
        Train_TotalJourneys := -1;
        Train_Type := UnknownTrainType;
        Train_TypeNum := 0;
        Train_UserDriving := False;
        Train_UserPowerAdjustment := 0; { used by the user to increase or decrease the train speed where necessary }
        Train_UserRequiresInstructions := False;
        Train_UserSpeedInstructionMsg := '';
        Train_UseTrailingTrackCircuits := False; { where a train doesn't have lights at both ends, it may need artificial track-circuit activation to allow the whole length
                                                   of the train to be detected }
        Train_WaitingForHiddenStationSignalAspectStartTime := 0;
        Train_WorkingTimetableLastArrivalArea := UnknownArea;
        Train_WorkingTimetableLastArrivalTime := 0;
        Train_WorkingTimetableLastEntryNumStr := '';
      END; {WITH}
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG InitialiseTrainRecord: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { InitialiseTrainRecord }

PROCEDURE SetTrainDirection(T : TrainIndex; DirectionRequired : DirectionType; ForceWrite : Boolean; VAR OK : Boolean);
{ Change the direction for a train's locos }
BEGIN
  OK := True;
  IF SystemOnline THEN BEGIN
    IF T = UnknownTrainIndex THEN
      UnknownTrainRecordFound('SetTrainDirection')
    ELSE BEGIN
      WITH Trains[T] DO BEGIN
        IF (DirectionRequired <> Train_CurrentDirection) OR ForceWrite THEN BEGIN
          SetLocoDirection(Locos[Train_LocoIndex], DirectionRequired, OK);
          IF OK AND (Train_DoubleHeaderLocoIndex <> UnknownLocoIndex) THEN
            SetLocoDirection(Locos[Train_DoubleHeaderLocoIndex], DirectionRequired, OK);
          IF OK THEN
            Train_CurrentDirection := DirectionRequired;
        END;
      END; {WITH}
    END;
  END;
END; { SetTrainDirection }

PROCEDURE TurnTrainLightsOn(T : TrainIndex; OUT OK : Boolean);
{ Turn the lights on for a train's locos }
VAR
  LightLoco : Boolean;
  NonMovingTrain : Boolean;
  UserMsg : String;
  UserMsgRequired : Boolean;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('TurnTrainLightsOn')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      NonMovingTrain := False;
      LightLoco := False;

      IF Train_UserDriving AND Train_UserRequiresInstructions THEN
        UserMsgRequired := True
      ELSE
        UserMsgRequired := False;

      IF Train_CurrentStatus = NonMoving THEN
        NonMovingTrain := True;
      IF Train_TypeNum = 0 THEN
        LightLoco := True;

      TurnLocoLightsOn(Train_LocoIndex, NonMovingTrain, LightLoco, UserMsgRequired, UserMsg, OK);
      IF UserMsgRequired AND (UserMsg <> '') THEN
        Log('L= ' + UserMsg);

      IF OK AND (Train_DoubleHeaderLocoIndex <> UnknownLocoIndex) THEN BEGIN
        TurnLocoLightsOn(Train_DoubleHeaderLocoIndex, NonMovingTrain, LightLoco, UserMsgRequired, UserMsg, OK);

        IF UserMsgRequired AND (UserMsg <> '') THEN BEGIN
          Log('L= ' + UserMsg);
          Train_LightsOn := False;
        END;
      END;

      IF OK THEN BEGIN
        Train_LightsOn := True;
        Log(Train_LocoChipStr + ' L Lights turned on');
      END ELSE
        Log(Train_LocoChipStr + ' L Lights did not turn on');
    END; {WITH}
  END;
END; { TurnTrainLightsOn }

PROCEDURE TurnTrainLightsOff(T : TrainIndex; OUT OK : Boolean);
{ Turn the lights off for a train's locos }
CONST
  UserMsgRequired = True;

VAR
  UserMsg : String;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('TurnTrainLightsOff')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF Train_UserDriving AND Train_UserRequiresInstructions THEN BEGIN
        TurnLocoLightsOff(Train_LocoIndex, UserMsgRequired, UserMsg, OK);
        IF UserMsg <> '' THEN
          Log('L= ' + UserMsg);
        IF Train_DoubleHeaderLocoIndex <> UnknownLocoIndex THEN BEGIN
          TurnLocoLightsOff(Train_DoubleHeaderLocoIndex, UserMsgRequired, UserMsg, OK);
          IF UserMsg <> '' THEN
            Log('L= ' + UserMsg);
        END;
        Train_LightsOn := False;
      END ELSE BEGIN
        TurnLocoLightsOff(Train_LocoIndex, NOT UserMsgRequired, UserMsg, OK);
        IF OK AND (Train_DoubleHeaderLocoIndex <> UnknownLocoIndex) THEN
          TurnLocoLightsOff(Train_DoubleHeaderLocoIndex, NOT UserMsgRequired, UserMsg, OK);
        IF OK THEN BEGIN
          Train_LightsOn := False;
          Log(Train_LocoChipStr + ' L Lights turned off');
        END ELSE
          Log(Train_LocoChipStr + ' L Lights did not turn off');
      END;
    END; {WITH}
  END;
END; { TurnTrainLightsOff }

FUNCTION TrainHasLightsOperatedByTwoChips(T : TrainIndex) : Boolean;
{ Returns true if one or both of a train's lights are operated by two chips }
BEGIN
  Result := False;

  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('TrainHasLightsOperatedByTwoChips')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF Locos[Train_LocoIndex].Loco_LightsType = LightsOperatedByTwoChips THEN
        Result := True
      ELSE BEGIN
        IF Locos[Train_LocoIndex].Loco_LightsType = LightsOperatedByTwoChips THEN
          Result := True;
      END;
    END; {WITH}
  END;
END; { TrainHasLightsOperatedByTwoChips }

FUNCTION TrainHasCabLights(T : TrainIndex) : Boolean;
{ Returns true if one or both of a train's locos have cab lights }
BEGIN
  Result := False;

  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('TrainHasCabLights')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF Locos[Train_LocoIndex].Loco_HasCabLights THEN
        Result := True
      ELSE BEGIN
        IF Trains[T].Train_DoubleHeaderLocoIndex <> UnknownLocoIndex THEN BEGIN
          IF Locos[Train_DoubleHeaderLocoIndex].Loco_HasCabLights THEN
            Result := True;
        END;
      END;
    END; {WITH}
  END;
END; { TrainHasCabLights }

PROCEDURE TurnTrainCabLightsOn(T : TrainIndex; OUT OK : Boolean);
{ Turn the cab lights on for a train's locos, if they have them }
CONST
  UserMsgRequired = True;

VAR
  UserMsg : String;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('TurnTrainCabLightsOn')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF Train_UserDriving AND Train_UserRequiresInstructions THEN BEGIN
        TurnLocoCabLightsOn(Train_LocoIndex, UserMsgRequired, UserMsg, OK);
        IF UserMsg <> '' THEN
          Log('L= ' + UserMsg);
        IF Train_DoubleHeaderLocoIndex <> UnknownLocoIndex THEN BEGIN
          TurnLocoCabLightsOn(Train_DoubleHeaderLocoindex, UserMsgRequired, UserMsg, OK);
          IF UserMsg <> '' THEN
            Log('L= ' + UserMsg);
        END;
      END ELSE BEGIN
        TurnLocoCabLightsOn(Train_LocoIndex, NOT UserMsgRequired, UserMsg, OK);
        IF OK AND (Train_DoubleHeaderLocoIndex <> UnknownLocoIndex) THEN
          TurnLocoCabLightsOn(Train_DoubleHeaderLocoIndex, NOT UserMsgRequired, UserMsg, OK);
        IF OK THEN BEGIN
          Train_CabLightsAreOn := True;
          Log(Train_LocoChipStr + ' L Cab lights turned on')
        END ELSE
          Log(Train_LocoChipStr + ' L Cab lights did not turn on');
      END;
    END; {WITH}
  END;
END; { TurnTrainCabLightsOn }

PROCEDURE TurnTrainCabLightsOff(T : TrainIndex; OUT OK : Boolean);
{ Turn the cab lights on for a train's locos, if they have them }
CONST
  UserMsgRequired = True;

VAR
  UserMsg : String;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('TurnTrainCabLightsOn')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF Train_UserDriving AND Train_UserRequiresInstructions THEN BEGIN
        TurnLocoCabLightsOff(Train_LocoIndex, UserMsgRequired, UserMsg, OK);
        IF UserMsg <> '' THEN
          Log('L= ' + UserMsg);
        IF Train_DoubleHeaderLocoIndex <> UnknownLocoIndex THEN BEGIN
          TurnLocoCabLightsOff(Train_DoubleHeaderLocoIndex, UserMsgRequired, UserMsg, OK);
          IF UserMsg <> '' THEN
            Log('L= ' + UserMsg);
        END;
      END ELSE BEGIN
        TurnLocoCabLightsOff(Train_LocoIndex, NOT UserMsgRequired, UserMsg, OK);
        IF OK AND (Train_DoubleHeaderLocoIndex <> UnknownLocoIndex) THEN
          TurnLocoCabLightsOff(Train_DoubleHeaderLocoIndex, NOT UserMsgRequired, UserMsg, OK);
        IF OK THEN BEGIN
          Train_CabLightsAreOn := False;
          Log(Train_LocoChipStr + ' L Cab lights turned off')
        END ELSE
          Log(Train_LocoChipStr + ' L Cab lights did not turn off');
      END;
    END; {WITH}
  END;
END; { TurnTrainCabLightsOn }

END { Train }.
