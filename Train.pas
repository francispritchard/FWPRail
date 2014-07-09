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

VAR
  TrainForm: TTrainForm;

PROCEDURE InitialiseTrainRecord(T : TrainIndex);
{ Do the initialisations, or reinitialisations if specified }

PROCEDURE SetTrainDirection(T : TrainIndex; DirectionRequired : DirectionType; ForceWrite : Boolean; VAR OK : Boolean);
{ Change the direction for a train's locos }

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

IMPLEMENTATION

{$R *.dfm}

USES Lenz, MiscUtils;

CONST
  UnitRef = 'Train';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

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
        Train_AtHiddenAspectSignal := UnknownSignal;
        Train_BeingAdvanced := False;
        Train_BeingAdvancedTC := UnknownTrackCircuit;
        Train_CabLightsAreOn := False;
        Train_CabLightsHaveBeenOn := False;
        Train_ControlledByProgram := False;
        Train_ControlledByRDC := False;
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
        Train_PreviouslyControlledByProgram := False;
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
        Train_UseTrailingTrackCircuits := False; { where a train doesn't have lights at both ends, it may need artificial track circuit activation to allow the whole length
                                                   of the train to be detected }
        Train_WaitingForHiddenAspectStartTime := 0;
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
          SetLocoDirection(Train_LocoIndex, DirectionRequired, OK);
          IF OK AND (Train_DoubleHeaderLocoIndex <> UnknownLocoIndex) THEN
            SetLocoDirection(Train_DoubleHeaderLocoIndex, DirectionRequired, OK);
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
