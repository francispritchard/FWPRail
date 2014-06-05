UNIT Movement;
{ Handles movement of trains over the line }

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, InitVars, ExtCtrls, StdCtrls, CheckLst;

TYPE
  TMovementWindow = CLASS(TForm)
    SignalLocationsToMonitorCheckListBox: TCheckListBox;
    SignalLocationsToMonitorOKButton: TButton;
    SignalLocationsToMonitorCancelButton: TButton;
    PROCEDURE SignalLocationsToMonitorCancelButtonClick(Sender: TObject);
    PROCEDURE SignalLocationsToMonitorOKButtonClick(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE CheckTrainsHaveArrived;
{ Runs through the list of trains, and sees if any have arrived in the route's final trackcircuit and have stopped; also sees whether to increment the journey number }

PROCEDURE CheckTrainsHaveDeparted;
{ Runs through the list of trains, and sees if any have departed at the start of a route }

PROCEDURE CheckTrainsReadyToDepart;
{ Runs through the list of trains, and sees if any are ready to depart }

PROCEDURE InitialiseMovementUnit;
{ Initialises the unit }

PROCEDURE LookOutForStrayingTrains;
{ See if any track circuits are occupied without our knowing who's doing the occupying }

PROCEDURE MoveAllTrains;
{ Move all active trains }

PROCEDURE PruneTrainList;
{ Remove inactive trains from list }

PROCEDURE SetDesiredTrainSpeed(T : Train);
{ Set the loco to accelerate/decelerate as required }

VAR
  MovementWindow: TMovementWindow;

{$R *.dfm}

IMPLEMENTATION

USES Locks, GetTime, Startup, MiscUtils, Diagrams, LocoUtils, IDGlobal, RDC, Route, DateUtils, RailDraw, Lenz, Input, StrUtils, LocationData, Options, Edit, Main;

CONST
  UnitRef = 'Movement';

VAR
  AutoModeMsgWritten : Boolean = False;
  AccelerationElapsedTime : TDateTime;
  NextSignal : Integer;
  NoAspectMsgWritten : Boolean = False;
  SaveMissingTrainStr : String = '';
  SaveTempDirectionStr : String;
  SaveTrainDesiredSpeedInMPH : MPHType = MPH0;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE LookOutForStrayingTrains;
{ See if any track circuits are occupied without our knowing who's doing the occupying }
VAR
  AdjoiningTCUp : Integer;
  AdjoiningTCDown : Integer;
  I : Integer;
  OK : Boolean;
  SaveAdjoiningTCUp : Integer;
  SaveAdjoiningTCDown : Integer;
  TCOccupationTimeInMilliSeconds : Int64;
  TC : Integer;

  FUNCTION AdjoiningTrackCircuitsOK(AdjoiningTCUp, AdjoiningTCDown : Integer) : Boolean;
  { Checks the adjoining track circuits for signs of previous occupation }
  VAR
    T : Train;

  BEGIN
    OK := False;
    
    IF AdjoiningTCUp <> UnknownTrackCircuit THEN BEGIN
      IF (TrackCircuits[TC].TC_PreviousLocoChip <> UnknownLocoChip)
      AND (TrackCircuits[TC].TC_PreviousLocoChip = TrackCircuits[AdjoiningTCUp].TC_LocoChip)
      THEN BEGIN
        T := GetTrainRecord(TrackCircuits[TC].TC_PreviousLocoChip);
        IF T <> NIL THEN BEGIN
          IF T^.Train_CurrentDirection = Up THEN BEGIN
            { we can assume the occupation is the same as the previous one }
            OK := True;
            Log('X TC=' + IntToStr(TC) + ' would have been considered anonymously occupied for '
                   + IntToStr(TCOccupationTimeInMilliSeconds) + ' ms'
                   + ' except that adjoining up TC=' + IntToStr(AdjoiningTCUp)
                   + ' is marked as being occupied by TC=' + IntToStr(TC) + '''s'
                   + ' previous loco ' + LocoChipToStr(TrackCircuits[AdjoiningTCUp].TC_LocoChip)
                   + ' which is travelling in the up direction'
                   + ' {WRAP=SCREENWIDTH}');
          END;
        END;
      END;

      IF NOT OK THEN BEGIN
        T := GetTrainRecord(TrackCircuits[AdjoiningTCUp].TC_LocoChip);
        IF T <> NIL THEN BEGIN
          I := 1;
          WHILE (I <= Length(T^.Train_InitialTrackCircuits))
          AND NOT OK
          DO BEGIN
            IF T^.Train_InitialTrackCircuits[I] = TC THEN BEGIN
              OK := True;
              Log('X TC=' + IntToStr(TC) + ' would have been considered anonymously occupied for '
                     + IntToStr(TCOccupationTimeInMilliSeconds) + ' ms'
                     + ' but for the fact that adjoining up TC=' + IntToStr(AdjoiningTCUp)
                     + ' is marked as being an initial track circuit of loco '
                     + LocoChipToStr(TrackCircuits[AdjoiningTCUp].TC_LocoChip)
                     + ' {WRAP=SCREENWIDTH}');
            END ELSE
              IF (TrackCircuits[AdjoiningTCUp].TC_OccupationState = TCPermanentFeedbackOccupation)
              OR (TrackCircuits[AdjoiningTCUp].TC_OccupationState = TCPermanentSystemOccupation)
              THEN BEGIN
                { we need this exception, because it is unlikely that a stationary loco will stray, and because quite often adjoining trackcircuits are activated by
                  stationary locos which parked next to the adjoining track circuit
                }
                OK := True;
                Log('X TC=' + IntToStr(TC) + ' would have been considered anonymously occupied for ' + IntToStr(TCOccupationTimeInMilliSeconds) + ' ms'
                       + ' but for the fact that adjoining up TC=' + IntToStr(AdjoiningTCUp) + ' is marked as being permanently occupied by loco '
                       + LocoChipToStr(TrackCircuits[AdjoiningTCUp].TC_LocoChip)
                       + ' {WRAP=SCREENWIDTH}');
              END;
            Inc(I);
          END;
        END;
      END;
    END;

    IF NOT OK THEN BEGIN
      IF AdjoiningTCDown <> UnknownTrackCircuit THEN BEGIN
        IF (TrackCircuits[TC].TC_PreviousLocoChip <> UnknownLocoChip)
        AND (TrackCircuits[TC].TC_PreviousLocoChip = TrackCircuits[AdjoiningTCDown].TC_LocoChip)
        THEN BEGIN
          T := GetTrainRecord(TrackCircuits[TC].TC_PreviousLocoChip);
          IF T <> NIL THEN BEGIN
            IF T^.Train_CurrentDirection = Down THEN BEGIN
              { we can assume the occupation is the same as the previous one }
              OK := True;
              Log('X TC=' + IntToStr(TC) + ' would have been considered anonymously occupied for ' + IntToStr(TCOccupationTimeInMilliSeconds) + ' ms'
                     + ' except that adjoining down TC=' + IntToStr(AdjoiningTCDown) + ' is marked as being occupied by TC=' + IntToStr(TC) + '''s'
                     + ' previous loco ' + LocoChipToStr(TrackCircuits[AdjoiningTCDown].TC_LocoChip) + ' which is travelling in the down direction'
                     + ' {WRAP=SCREENWIDTH}');
            END;
          END;
        END;

        IF NOT OK THEN BEGIN
          T := GetTrainRecord(TrackCircuits[AdjoiningTCDown].TC_LocoChip);
          IF T <> NIL THEN BEGIN
            I := 1;
            WHILE (I <= Length(T^.Train_InitialTrackCircuits))
            AND NOT OK
            DO BEGIN
              IF T^.Train_InitialTrackCircuits[I] = TC THEN BEGIN
                OK := True;
                Log('X TC=' + IntToStr(TC) + ' would have been considered anonymously occupied for ' + IntToStr(TCOccupationTimeInMilliSeconds) + ' ms'
                       + ' but for the fact that adjoining down TC=' + IntToStr(AdjoiningTCDown) + ' is marked as being an initial track circuit of loco '
                       + LocoChipToStr(TrackCircuits[AdjoiningTCDown].TC_LocoChip)
                       + ' {WRAP=SCREENWIDTH}');
              END ELSE
                IF (TrackCircuits[AdjoiningTCDown].TC_OccupationState = TCPermanentFeedbackOccupation)
                OR (TrackCircuits[AdjoiningTCDown].TC_OccupationState = TCPermanentSystemOccupation)
                THEN BEGIN
                  { we need this exception, because it is unlikely that a stationary loco will stray, and because quite often adjoining trackcircuits are activated by
                    stationary locos which parked next to the adjoining track circuit
                  }
                  OK := True;
                  Log('X TC=' + IntToStr(TC) + ' would have been considered anonymously occupied for ' + IntToStr(TCOccupationTimeInMilliSeconds) + ' ms'
                         + ' but for the fact that adjoining down TC=' + IntToStr(AdjoiningTCDown) + ' is marked as being permanently occupied by loco '
                         + LocoChipToStr(TrackCircuits[AdjoiningTCDown].TC_LocoChip)
                         + ' {WRAP=SCREENWIDTH}');
                END;
              Inc(I);
            END;
          END;
        END;
      END;
    END;
    Result := OK;
  END; { AdjoiningTrackCircuitsOK }

BEGIN
  IF MonitorStrayingTrains THEN BEGIN
    FOR TC := 0 TO High(TrackCircuits) DO BEGIN
      WITH TrackCircuits[TC] DO BEGIN
        IF (TC_OccupationStartTime <> 0)
        AND NOT TC_MysteriouslyOccupied
        THEN BEGIN
          TCOccupationTimeInMilliSeconds := NewMilliSecondsBetween(TC_OccupationStartTime, Time);
          IF (TC_LocoChip = UnknownLocoChip)
          AND (TC_PreviousLocoChip = UnknownLocoChip)
          AND (TCOccupationTimeInMilliSeconds > 1000)
          THEN BEGIN
            OK := False;
            TC_MysteriouslyOccupied := True;

            { see if we can work out who's doing the occupying }
            FindAdjoiningTrackCircuits(TC, AdjoiningTCUp, AdjoiningTCDown);
            OK := AdjoiningTrackCircuitsOK(AdjoiningTCUp, AdjoiningTCDown);
            IF NOT OK THEN BEGIN
              { check the adjoining adjoining track circuits }
              SaveAdjoiningTCUp := AdjoiningTCUp;
              SaveAdjoiningTCDown := AdjoiningTCDown;

              FindAdjoiningTrackCircuits(SaveAdjoiningTCUp, AdjoiningTCUp, AdjoiningTCDown);
              OK := AdjoiningTrackCircuitsOK(AdjoiningTCUp, AdjoiningTCDown);
              IF NOT OK THEN BEGIN
                FindAdjoiningTrackCircuits(SaveAdjoiningTCDown, AdjoiningTCUp, AdjoiningTCDown);
                OK := AdjoiningTrackCircuitsOK(AdjoiningTCUp, AdjoiningTCDown);
              END;
            END;

            { If the occupation has lasted more than a second and we don't know who's there, call a temporary halt }
            IF NOT OK
            AND AnonymousOccupationMode
            THEN BEGIN
              Log('XG TC=' + IntToStr(TC) + ' has been anonymously occupied for ' + IntToStr(TCOccupationTimeInMilliSeconds) + ' ms');
              TurnAutoModeOff(NOT ByUser);
              ShowMessage('TC=' + IntToStr(TC) + ' has been anonymously occupied for ' + IntToStr(TCOccupationTimeInMilliSeconds) + ' ms' + ': auto mode suspended');

              { and reset the occupation time, as otherwise it can be impossible to restart the program }
              TC_OccupationStartTime := 0;
            END;
          END;
        END;
      END; {WITH}
    END; {FOR}
  END;
END; { LookOutForStrayingTrains }

PROCEDURE SetDesiredTrainSpeed(T : Train);
{ Set the loco to accelerate/decelerate as required }
VAR
  OK : Boolean;

  PROCEDURE AdjustSpeed(T : Train);
  { Adjusts the speed over the given time span (in seconds) }

    FUNCTION CurrentLenzSpeedMatchesSpeedInMPH(T : Train; CurrentLenzSpeed : Integer) : MPHType;
    { Returns true if the current Lenz speed number matches one of the defined speed steps }
    BEGIN
      Result := UnknownMPH;
      WITH T^ DO BEGIN
        IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH0) THEN
          Result := MPH0
        ELSE
          IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH10) THEN
            Result := MPH10
          ELSE
            IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH20) THEN
              Result := MPH20
            ELSE
              IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH30) THEN
                Result := MPH30
              ELSE
                IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH40) THEN
                  Result := MPH40
                ELSE
                  IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH50) THEN
                    Result := MPH50
                  ELSE
                    IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH60) THEN
                      Result := MPH60
                    ELSE
                      IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH70) THEN
                        Result := MPH70
                      ELSE
                        IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH80) THEN
                          Result := MPH80
                        ELSE
                          IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH90) THEN
                            Result := MPH90
                          ELSE
                            IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH100) THEN
                              Result := MPH100
                            ELSE
                              IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH110) THEN
                                Result := MPH110
                              ELSE
                                IF CurrentLenzSpeed = TrainSpeedInMPHToLenzSpeed(T, MPH120) THEN
                                  Result := MPH120;
      END; {WITH}
    END; { CurrentLenzSpeedMatchesSpeedInMPH }

  VAR
    NoOfSecondsTrain_Accelerating : Real;
    OK : Boolean;
    SpeedStep : Integer;
    SpeedStepInMPH : MPHType;

  BEGIN { AdjustSpeed }
    WITH T^ DO BEGIN
      SpeedStep := 1;
      IF Train_Accelerating OR Train_Decelerating THEN BEGIN
        NoOfSecondsTrain_Accelerating := SecondSpan(Train_AccelerationStartTime, Time);
        IF NoOfSecondsTrain_Accelerating > Train_AccelerationTimeInterval THEN BEGIN
          IF Train_AccelerationTimeInterval = 0.0 THEN
            { one big speed step }
            SpeedStep := Train_AccelerationAdjustRange
          ELSE
            IF T^.Train_AccelerationTimeInSeconds <= 1.0 THEN
              { we change the speed by number of speed steps }
              SpeedStep := Train_AccelerationAdjustRange DIV 3;

          IF Train_DesiredLenzSpeed > Train_CurrentLenzSpeed THEN BEGIN
            IF SpeedStep = 0 THEN BEGIN
              { this deals with cases where something's gone wrong - the speed step should never be 0, or we can't increase speed! }
              SpeedStep := 1;
              Log(Train_LocoChipStr + ' L Emergency speed step of 1 in use');
            END;
            Train_CurrentLenzSpeed := AdjustLenzSpeed(Train_LocoChip, Train_DoubleHeaderLocoChip, SpeedStep, Train_CurrentDirection, OK);

            IF Train_CurrentLenzSpeed < Train_DesiredLenzSpeed THEN
              Log(Train_LocoChipStr + ' L AdjustSpeed routine: speed increased by ' + IntToStr(SpeedStep) + ' to ' + IntToStr(Train_CurrentLenzSpeed)
                                    + ' target=' + IntToStr(Train_DesiredLenzSpeed) + ' acceleration time=' + FloatToStr(Train_AccelerationTimeInterval))
            ELSE BEGIN
              Train_CurrentLenzSpeed := Train_DesiredLenzSpeed;
              Log(Train_LocoChipStr + ' L AdjustSpeed routine: final speed increase to ' + IntToStr(Train_CurrentLenzSpeed)
                                    + ' target=' + IntToStr(Train_DesiredLenzSpeed) + ' acceleration time=' + FloatToStr(Train_AccelerationTimeInterval));
            END;
            DrawDiagramsSpeedCell(T);
          END ELSE
            IF Train_DesiredLenzSpeed < Train_CurrentLenzSpeed THEN BEGIN
              Train_CurrentLenzSpeed := AdjustLenzSpeed(Train_LocoChip, Train_DoubleHeaderLocoChip, -SpeedStep, Train_CurrentDirection, OK);

              DrawDiagramsSpeedCell(T);
              Log(Train_LocoChipStr + ' L AdjustSpeed routine: speed decreased by ' + IntToStr(SpeedStep) + ' to ' + IntToStr(Train_CurrentLenzSpeed)
                                    + ' target=' + IntToStr(Train_DesiredLenzSpeed) + ' - deceleration time=' + FloatToStr(T^.Train_AccelerationTimeInSeconds));
            END;

          IF OK THEN BEGIN
            { See if the current Lenz speed number matches a known programmatical speed step, to keep CurrentSpeed current }
            SpeedStepInMPH := CurrentLenzSpeedMatchesSpeedInMPH(T, Train_CurrentLenzSpeed);
            IF SpeedStepInMPH <> UnknownMPH THEN
              Train_CurrentSpeedInMPH := SpeedStepInMPH;
          END;
          { Reset the start time }
          Train_AccelerationStartTime := Time;
        END;
      END;
    END; {WITH}
  END; { AdjustSpeed }

  PROCEDURE CalculateTimeInterval(T : Train);
  { Calculate the time it should take to achieve the new speed }
  VAR
    OK : Boolean;

  BEGIN
    WITH T^ DO BEGIN
      AccelerationElapsedTime := Time;
      IF Train_DesiredLenzSpeed > Train_CurrentLenzSpeed THEN BEGIN
        Train_Accelerating := True;
        Train_Decelerating := False;
      END ELSE
        IF Train_DesiredLenzSpeed < Train_CurrentLenzSpeed THEN BEGIN
          Train_Decelerating := True;
          Train_Accelerating := False;
        END;

      { Calculate the range of the speed change - minus one, as the first speed change is about to happen }
      Train_AccelerationAdjustRange := Abs(Train_DesiredLenzSpeed - Train_CurrentLenzSpeed - 1);
      IF Train_AccelerationAdjustRange = 0 THEN
        { If there's no change to make, schedule no time for it! (shouldn't get here ****) }
        Train_AccelerationTimeInterval := 0.0
      ELSE
        IF T^.Train_AccelerationTimeInSeconds > 1.0 THEN
          { Speed is increased/decreased by one unit, over varying periods of time }
          Train_AccelerationTimeInterval := T^.Train_AccelerationTimeInSeconds / Train_AccelerationAdjustRange
        ELSE
          IF T^.Train_AccelerationTimeInSeconds > 0.0 THEN
            { speed is increased/decreased by a number of units in 2 steps }
            Train_AccelerationTimeInterval := T^.Train_AccelerationTimeInSeconds / 2
          ELSE
            { an immediate stop }
            Train_AccelerationTimeInterval := 0.0;

      { Make the first adjustment without delay }
      IF Train_Accelerating THEN BEGIN
        Train_CurrentLenzSpeed := AdjustLenzSpeed(Train_LocoChip, Train_DoubleHeaderLocoChip, 1, Train_CurrentDirection, OK);
        DrawDiagramsSpeedCell(T);
        Log(Train_LocoChipStr + ' L First adjustment in Train_Accelerating to '
                              + IntToStr(Train_DesiredLenzSpeed)
                              +  ' (' + MPHToStr(Train_DesiredSpeedInMPH) + ' mph)');
      END ELSE BEGIN
        { decelerating }
        Train_CurrentLenzSpeed := AdjustLenzSpeed(Train_LocoChip, Train_DoubleHeaderLocoChip, -1, Train_CurrentDirection, OK);
        DrawDiagramsSpeedCell(T);
        Log(Train_LocoChipStr + ' L First adjustment in Train_Decelerating to '
                              + IntToStr(Train_DesiredLenzSpeed)
                              +  ' (' + MPHToStr(Train_DesiredSpeedInMPH) + ' mph)');
      END;

      Log(Train_LocoChipStr + ' L Train_CurrentSpeedInMPH=' + IntToStr(Train_CurrentLenzSpeed)
                            + ' Train_DesiredLenzSpeed=' + IntToStr(TrainSpeedInMPHToLenzSpeed(T, Train_DesiredSpeedInMPH))
                            + ' (Train_DesiredSpeedInMPH=' + MPHToStr(Train_DesiredSpeedInMPH) + ' mph'
                            + ') AdjustRange=' + IntToStr(Train_AccelerationAdjustRange)
                            + ' TimeInterval=' + FloatToStr(Train_AccelerationTimeInterval)
                            + ' (AccelTime=' + FloatToStr(Train_AccelerationTimeInSeconds) + ')'); //
      { Now start the timer }
      Train_AccelerationStartTime := Time;
    END; {WITH}
  END; { CalculateTimeInterval }

BEGIN { SetDesiredTrainSpeed }
  WITH T^ DO BEGIN
    IF LocosStopped THEN BEGIN
      IF Train_UserDriving THEN BEGIN
        IF Train_UserRequiresInstructions THEN BEGIN
          Log(Train_LocoChipStr + ' L= User instructed to set speed to zero');
          DrawDiagramsSpeedCell(T);
          Train_CurrentLenzSpeed := 0;
        END;
      END ELSE BEGIN
        SetLenzSpeed(Train_LocoChip, Train_DoubleHeaderLocoChip, 0, Train_CurrentDirection, NOT QuickStop, OK);
        DrawDiagramsSpeedCell(T);
      END;
      Train_CurrentLenzSpeed := 0;
      Train_CurrentSpeedInMPH := Stop;
    END ELSE BEGIN
      { If we've reached the desired speed: comparison has to be between Lenz speed numbers, as MPH90 could equal MPH125 in Lenz speed terms if MPH90 is the top speed }
      IF Train_UserDriving THEN BEGIN
        IF Train_UserRequiresInstructions THEN BEGIN
          IF Train_CurrentLenzSpeed <> Train_DesiredLenzSpeed THEN BEGIN
            Log(Train_LocoChipStr + ' L= User instructed to set speed to ' + IntToStr(Train_CurrentLenzSpeed));
            DrawDiagramsSpeedCell(T);
          END;
        END;
      END ELSE BEGIN
        IF Train_CurrentLenzSpeed = Train_DesiredLenzSpeed THEN BEGIN
          IF Train_Accelerating THEN BEGIN
            Train_Accelerating := False;
            Log(Train_LocoChipStr + ' L Elapsed time after acceleration=' + FloatToStr(SecondSpan(AccelerationElapsedTime, Time)))
          END;
          IF Train_Decelerating THEN BEGIN
            Train_Decelerating := False;
            Log(Train_LocoChipStr + ' L Elapsed time after deceleration=' + FloatToStr(SecondSpan(AccelerationElapsedTime, Time)));
          END;

          { Display the speed }
          DrawDiagramsSpeedCell(T);
        END ELSE BEGIN
          { Initiate a change in speed if the target speed has changed }
          IF Train_SaveDesiredLenzSpeed = Train_DesiredLenzSpeed THEN
            AdjustSpeed(T)
          ELSE BEGIN
            IF (Train_AccelerationTimeInSeconds = 0.0)
            AND (Train_DesiredLenzSpeed = 0)
            THEN BEGIN
              { we want an immediate stop }
              SetLenzSpeed(Train_LocoChip, Train_DoubleHeaderLocoChip, 0, Train_CurrentDirection, NOT QuickStop, OK);
              Train_CurrentLenzSpeed := 0;
              Train_CurrentSpeedInMPH := Stop;
              Log(Train_LocoChipStr + ' L Immediate stop required');
              DrawDiagramsSpeedCell(T);
            END ELSE
              { calculate time between speed steps }
              CalculateTimeInterval(T);

            Train_SaveDesiredLenzSpeed := Train_DesiredLenzSpeed;
          END;
        END;
      END;
    END;

    IF (Train_DesiredLenzSpeed = 0)
    AND (Train_UserPowerAdjustment <> 0)
    THEN BEGIN
      Train_UserPowerAdjustment := 0;
      Log(Train_LocoChipStr + ' L Train desired speed = 0 so user increased speed setting turned off');
    END;
  END; {WITH}
END; { SetDesiredTrainSpeed }

PROCEDURE PruneTrainList;
{ Remove inactive trains from diagrams }
VAR
  T : Train;

BEGIN
  T := TrainList;
  WHILE T <> NIL DO BEGIN
    IF T^.Train_CurrentStatus = ReadyForRemovalFromDiagrams THEN BEGIN
      { Take it off the diagram grid }
      RemoveTrainFromDiagrams(T);
      ChangeTrainStatus(T, RemovedFromDiagrams);

      DrawDiagrams(UnitRef, 'PruneTrainList');

      { Turn off the loco's lights (if any) }
      IF NOT T^.Train_LightsRemainOnWhenJourneysComplete THEN
        TurnLightsOff(T^.Train_LocoChip);
    END;
    T := T^.Train_NextRecord;
  END; {WHILE}
END; { PruneTrainList }

PROCEDURE OldPruneTrainList;
{ Purge inactive trains from list - no longer in use as the train list is permanent, but might be useful for other lists in the future }

  PROCEDURE OldPrune(VAR L : Train);
  { Recursive prune of given list }
  VAR
    T : Train;

  BEGIN
    WHILE (L <> NIL)
    { AND (L^.Train_CurrentStatus = ReadyForPurging) }
    DO BEGIN
      { Check its speed is zero, and, if not, stop it! }
      { ****** 12/10/03 }
      RemoveTrainFromDiagrams(L); { Take off the diagrams }
      { but, if it is not timetabled to leave again from the same place, set up trackcircuits so that we still know where its carriages or wagons are
        ******** NOT FULLY TESTED 8/4/02
      }

      DrawDiagrams(UnitRef, 'OldPruneTrainList');
      T := L;

      { Turn off the loco's lights (if any) }
      IF NOT T^.Train_LightsRemainOnWhenJourneysComplete THEN
        TurnLightsOff(T^.Train_LocoChip);

      L := T^.Train_NextRecord;
      Log(T^.Train_LocoChipStr + ' L Train ' + T^.Train_Headcode + ' (Loco no. ' + T^.Train_LocoChipStr + ')' + ' now deleted');
      Dispose(T);
    END; {WHILE}
    IF L <> NIL THEN
      OldPrune(L^.Train_NextRecord);
  END; { Prune }

BEGIN
  OldPrune(TrainList);
END; { OldPruneTrainList }

PROCEDURE MoveAllTrains;
{ If in auto mode, move all active trains }

  PROCEDURE CalculateTrainSpeed(T : Train);
  { Set the speed of the train appropriately }

    FUNCTION SpeedAtSignal(VAR T : Train; S : Integer; RedAspectSpeed, SingleYellowAspectSpeed, DoubleYellowAspectSpeed, GreenAspectSpeed : MPHType) : MPHType;
    { Returns the appropriate speed for a given signal, considering normal and hidden aspects }
    BEGIN
      Result := MPH0;
      IF S = UnknownSignal THEN
        { no signal specified - probably a buffer stop! }
        Result := RedAspectSpeed
      ELSE
        WITH Signals[S] DO BEGIN
          { can't use a CASE statement here as considering two cases each time so that the aspect which is the more serious takes priority }
          IF (Signal_Aspect = RedAspect) OR (Signal_HiddenAspect = RedAspect) THEN BEGIN
            IF Signal_ApproachLocked THEN
              { it will shortly change, so this avoids the train stopping then having to start from stop immediately }
              Result := SingleYellowAspectSpeed
            ELSE
              Result := RedAspectSpeed;
          END ELSE
            IF (Signal_Aspect = SingleYellowAspect)
            OR (Signal_Aspect = FlashingSingleYellowAspect)
            OR (Signal_HiddenAspect = SingleYellowAspect)
            THEN
              Result := SingleYellowAspectSpeed
            ELSE
              IF (Signal_Aspect = DoubleYellowAspect)
              OR (Signal_Aspect = FlashingDoubleYellowAspect)
              OR (Signal_HiddenAspect = DoubleYellowAspect)
              THEN
                Result := DoubleYellowAspectSpeed
              ELSE
                IF Signal_Aspect = GreenAspect THEN BEGIN
                  { treat the calling-on signal as a distant signal for the purposes of assessing speed }
                  CASE Signals[S].Signal_Type OF
                    CallingOn:
                      Result := SingleYellowAspectSpeed;
                    TwoAspect, ThreeAspect, FourAspect, SemaphoreHome, SemaphoreDistant:
                      Result := GreenAspectSpeed;
                  END; {CASE}
                END ELSE BEGIN
                  { NoAspect: shouldn't happen, but if it does, best that we stop, but ask first }
                  Result := MPH0;
                  IF NOT NoAspectMsgWritten THEN BEGIN
                    NoAspectMsgWritten := True;
    Log('XG Signal ' + IntToStr(S) + ' has no aspect lit');
                  END;
                END;

          IF Signal_Aspect <> NoAspect THEN
            NoAspectMsgWritten := False;
        END; {WITH}
    END; { SpeedAtSignal }

    PROCEDURE ClearTrailingTrackCircuits(T : Train);
    { Goes through the trackcircuits cleared list, adding up the lengths of the sections that are still marked as occupied, to see if any can be released, if the total
      exceeds the length of the train.
    }
    VAR
      Count : Integer;
      DebugStr : String;
      DuplicateTCFound : Boolean;
      I : Integer;
      TC : Integer;
      TCReleased : Boolean;
      TCsOccupationLength : Real;
      TCsOccupiedArray, TCsForReleaseArray, TCsToTestArray : ARRAY OF Integer;

    BEGIN
      SetLength(TCsOccupiedArray, 0);
      SetLength(TCsForReleaseArray, 0);
      SetLength(TCsToTestArray, 0);

      WITH T^ DO BEGIN
        { First run through the list, removing earlier occurrences of occupation - it may well be that we are re-occupying a circuit on a later occasion - if we don't
          remove the record of the earlier releases, the latest occupation is disregarded
        }
        IF Length(Train_TCsOccupiedOrClearedArray) > 1 THEN BEGIN
          FOR Count := 0 TO (High(Train_TCsOccupiedOrClearedArray) - 1) DO BEGIN
            { If there's a later occurrence of TC, do not copy the earlier occurrence }
            TC := ExtractTrackCircuitFromString(Train_TCsOccupiedOrClearedArray[Count]);
            DuplicateTCFound := False;
            FOR I := (Count + 1) TO High(Train_TCsOccupiedOrClearedArray) DO BEGIN
              IF TC = ExtractTrackCircuitFromString(Train_TCsOccupiedOrClearedArray[I]) THEN
                DuplicateTCFound := True;
            END;
            IF NOT DuplicateTCFound THEN BEGIN
              SetLength(TCsToTestArray, Length(TCsToTestArray) + 1);
              TCsToTestArray[High(TCsToTestArray)] := TC;
            END;
          END;
          { but remember to copy the final TC anyway }
          SetLength(TCsToTestArray, Length(TCsToTestArray) + 1);
          TCsToTestArray[High(TCsToTestArray)] := ExtractTrackCircuitFromString(Train_TCsOccupiedOrClearedArray[High(Train_TCsOccupiedOrClearedArray)]);
        END;

        { Remove circuits marked as unoccupied, and also ones that this loco has already released }
        TCsOccupationLength := 0;

        IF Length(TCsToTestArray) > 0 THEN BEGIN
          FOR Count := 0 TO High(TCsToTestArray) DO BEGIN
            { Check the released list }
            TCReleased := False;
            FOR I := 0 TO High(Train_TCsReleasedArray) DO BEGIN
              IF TCsToTestArray[Count] = ExtractTrackCircuitFromString(Train_TCsReleasedArray[I]) THEN
                TCReleased := True;
            END;
            { Now see if it's occupied }
            IF (TrackCircuits[TCsToTestArray[Count]].TC_OccupationState <> TCUnoccupied)
            AND NOT TCReleased
            THEN BEGIN
              SetLength(TCsOccupiedArray, Length(TCsOccupiedArray) + 1);
              TCsOccupiedArray[High(TCsOccupiedArray)] := TCsToTestArray[Count];
            END;
          END;

          DebugStr := '';
          FOR Count := 0 TO High(TCsOccupiedArray) DO BEGIN
            DebugStr := DebugStr + ' ' + IntToStr(TCsOccupiedArray[Count]) + ':'
                                                                + FloatToStr(TrackCircuits[TCsOccupiedArray[Count]].TC_LengthInInches) + ' ';
          END;

          { Now count up the carriage lengths in the system-occupied circuits }
          IF Length(TCsOccupiedArray) >= 2 THEN BEGIN
            FOR Count := (High(TCsOccupiedArray) - 1) DOWNTO 1 DO BEGIN
              { count how many carriage-lengths there are between the currently occupied one and the first one (which we want to clear) }
              TCsOccupationLength := TCsOccupationLength + TrackCircuits[TCsOccupiedArray[Count]].TC_LengthInInches;
            END;
            DebugStr := DebugStr + ' TCOL=' + FloatToStr(TCsOccupationLength);

            { Now see if the total exceeds the stated length of the train in inches (a carriage-length is about twelve inches) }
            IF TCsOccupationLength >= Train_CurrentLengthInInches THEN BEGIN
              { can now reset any trailing ones, as the train length fits in before them (but only system occupied ones) }
              TCsOccupationLength := 0;
              SetLength(TCsForReleaseArray, 0);
              FOR Count := (High(TCsOccupiedArray) - 1) DOWNTO 1 DO BEGIN
                TCsOccupationLength := TCsOccupationLength + TrackCircuits[TCsOccupiedArray[Count]].TC_LengthInInches;
                IF TCsOccupationLength > Train_CurrentLengthInInches THEN BEGIN
                  { can now release all the remaining circuits }
                  SetLength(TCsForReleaseArray, Length(TCsForReleaseArray) + 1);

                  { move all the elements up one }
                  FOR I := High(TCsForReleaseArray) DOWNTO 1 DO
                    TCsForReleaseArray[I] := TCsForReleaseArray[I - 1];

                  { and add the new one at position 0 }
                  TCsForReleaseArray[0] := TCsOccupiedArray[Count - 1];
                END;
              END;

              IF DebugStr <> Train_SaveTCsOccupiedStr THEN BEGIN
                Log(Train_LocoChipStr + ' L ' + DebugStr);
                Train_SaveTCsOccupiedStr := DebugStr;

                DebugStr := '';
                FOR I := 0 TO High(TCsForReleaseArray) DO
                  DebugStr := DebugStr + ' ' + IntToStr(TCsForReleaseArray[I]);
                IF DebugStr <> Train_SaveTCsForReleaseStr THEN BEGIN
                  Log(Train_LocoChipStr + ' T Train_SaveTCsforRelease=' + DebugStr);
                  Train_SaveTCsForReleaseStr := DebugStr;
                END;
              END;
            END;
          END;

          { Clear length-released track circuits here }
          IF Length(TCsForReleaseArray) > 0 THEN BEGIN
            DebugStr := '';
            FOR Count := 0 TO High(TCsForReleaseArray) DO BEGIN
              IF TrackCircuits[TCsForReleaseArray[Count]].TC_OccupationState = TCSystemOccupation THEN BEGIN
                SetTrackCircuitState(TCsForReleaseArray[Count], TCUnoccupied);

                SetLength(Train_TCsReleasedArray, Length(Train_TCsReleasedArray) + 1);
                Train_TCsReleasedArray[High(Train_TCsReleasedArray)] := 'TC=' + IntToStr(TCsForReleaseArray[Count]);

                DebugStr := DebugStr + ' TC=' + IntToStr(TCsForReleaseArray[Count]);
              END;
            END;
            IF DebugStr <> '' THEN
              Log(Train_LocoChipStr + ' T Track circuits released=' + Trim(DebugStr) + ' {INDENT=0} {WRAP=SCREENWIDTH}');
          END;
        END;
      END; {WITH}
    END; { ClearTrailingTrackCircuits }

    FUNCTION WaitBeforeReversing(T : Train) : Boolean;
    { Makes Thunderbird stop for a minimum time before changing direction ****. Not in use 9/4/14 }
    BEGIN
      Result := True;
      WITH T^ DO BEGIN
        IF Train_ReversingWaitStarted THEN BEGIN
          { Waiting in platform }
          IF Round(SecondSpan(Time, Train_ReversingStartTime)) >= Train_ReversingDelayInSeconds THEN BEGIN
            Log(Train_LocoChipStr + ' L Reversing wait completed');
            Train_ReversingWaitStarted := False;
            DrawDiagrams(UnitRef, 'WaitBeforeReversing 1');
            Result := False;
          END;
        END ELSE
          IF Train_Reversing THEN BEGIN
            { Entering reversing area }
            Train_ReversingWaitStarted := True;
            DrawDiagrams(UnitRef, 'WaitBeforeReversing 2');
            Train_ReversingStartTime := Time;
            Log(Train_LocoChipStr + ' L Reversing wait started');
          END ELSE
            Result := False;
      END; {WITH}
    END; { WaitBeforeReversing }

    FUNCTION FindFirstOccurrenceInStringArray(StrToFind : String; StringArray : StringArrayType) : Integer;
    { Find the first occurrence of a given string in a string array }
    VAR
      I : Integer;

    BEGIN
      Result := -1;
      I := 0;
      WHILE (I <= High(StringArray))
      AND (Result = -1)
      DO BEGIN
        { Pos returns values starting from 1 - we need them from zero }
        IF Pos(StrToFind, StringArray[I]) > 0 THEN
          Result := I;
        Inc(I);
      END; {WHILE}
    END; { FindFirstOccurrenceInStringArray }

    PROCEDURE FindFirstFourOccurrencesInStringArray(StrToFind : String; StringArray : StringArrayType;
                                                    VAR FirstOccurrencePos, SecondOccurencePos, ThirdOccurencePos, FourthOccurencePos : Integer);
    { Find the first, second and third occurrences of a given string in a string array - return -1 if not found }
    VAR
      I : Integer;

    BEGIN
      FirstOccurrencePos := -1;
      SecondOccurencePos := -1;
      ThirdOccurencePos := -1;
      FourthOccurencePos := -1;
      I := 0;
      WHILE (I <= High(StringArray))
      AND (FourthOccurencePos = -1)
      DO BEGIN
        { Pos returns values starting from 1 - we need them from zero, so use I as the counter }
        IF Pos(StrToFind, StringArray[I]) > 0 THEN
          IF FirstOccurrencePos = -1 THEN
            FirstOccurrencePos := I
          ELSE
            IF SecondOccurencePos = -1 THEN
              SecondOccurencePos := I
            ELSE
              IF ThirdOccurencePos = -1 THEN
                ThirdOccurencePos := I
              ELSE
                FourthOccurencePos := I;
        Inc(I);
      END; {WHILE}
    END; { FindFirstFourOccurrencesInStringArray }

    PROCEDURE CalculateDesiredSpeed(VAR T : Train);
    { Set loco speed depending on where we are }
    VAR
      BufferStopPos : Integer;
      CurrentTC : Integer;
      DebugStr : String;
      DistanceToNextTCInInches : Real;
      FirstSignalPos, SecondSignalPos, ThirdSignalPos, FourthSignalPos : Integer;
      I : Integer;
      MinimumSpeedInMPH : MPHType;
      SpeedCalculationStr : String;
      StopStr : String;
      TCPos : Integer;
      TempMaximumSpeedInMPH : MPHType;
      TempStr : String;

      PROCEDURE AdjustSpeedForGradient(T : Train; TC : Integer);
      { Adjust for rising or falling gradients }
      BEGIN
        WITH TrackCircuits[TC] DO BEGIN
          WITH T^ DO BEGIN
            IF (Train_GradientSpeedAdjustment <> 0)
            AND (Train_DesiredSpeedInMPH = MPH0)
            THEN BEGIN
              { we have to suspend adjustment, or the Lenz speed will become -1 }
              IF NOT Train_GradientSpeedAdjustmentMsgWritten THEN BEGIN
                Train_GradientSpeedAdjustment := 0;
                Log(Train_LocoChipStr + ' L Gradient speed adjustment suspended at TC=' + IntToStr(TC) + ' as train is stopped');
                Train_GradientSpeedAdjustmentMsgWritten := True;
              END;
            END ELSE
              IF TC_Gradient = Level THEN BEGIN
                Train_GradientSpeedAdjustment := 0;
                IF Train_GradientSpeedAdjustmentMsgWritten THEN BEGIN
                  Log(Train_LocoChipStr + ' L Gradient speed adjustment cancelled at TC=' + IntToStr(TC));
                  Train_GradientSpeedAdjustmentMsgWritten := False;
                END;
              END ELSE BEGIN
                IF ((Train_CurrentDirection = Up)
                    AND (TC_Gradient = RisingIfUp))
                OR ((Train_CurrentDirection = Down)
                    AND (TC_Gradient = RisingIfDown))
                THEN
                  { uphill - increase speed by one Lenz number }
                  Train_GradientSpeedAdjustment := 1
                ELSE
                  IF ((Train_CurrentDirection = Up)
                      AND (TC_Gradient = RisingIfDown))
                  OR ((Train_CurrentDirection = Down)
                      AND (TC_Gradient = RisingIfUp))
                  THEN
                    { downhill - decrease speed by one Lenz number }
                    Train_GradientSpeedAdjustment := -1;

                IF NOT Train_GradientSpeedAdjustmentMsgWritten THEN BEGIN
                  IF Train_GradientSpeedAdjustment = 1 THEN
                    Log(Train_LocoChipStr + ' L Gradient speed adjustment at TC=' + IntToStr(TC) + ' = +1')
                  ELSE
                    IF Train_GradientSpeedAdjustment = -1 THEN
                      Log(Train_LocoChipStr + ' L Gradient speed adjustment at TC=' + IntToStr(TC) + ' = -1');
                  Train_GradientSpeedAdjustmentMsgWritten := True;
                END;
              END;
          END; {WITH}
        END; {WITH}
      END; { AdjustSpeedForGradient }

      PROCEDURE CheckForMaximumSpeeds(T : Train; CurrentTC : Integer; OUT TempMaximumSpeedInMPH : MPHType);
      { Check for possible maximum speeds }
      VAR
        ArrayPos : Integer;
        JunctionIndicator : JunctionIndicatorType;
        MaximumSpeedCheckCount : Integer;
        S : Integer;
        SaveMaximumSpeedInMPH : MPHType;
        SpeedRestriction : MPHType;
        TCBeingChecked : Integer;

      BEGIN
        WITH T^ DO BEGIN
          IF Length(Train_TCsAndSignalsNotClearedArray) > 0 THEN BEGIN
            { Look at the trackcircuits and junction signals up to a set distance ahead - allows for different speed restrictions in successive trackcircuits }
            TempMaximumSpeedInMPH := Train_MaximumSpeedInMPH;
            SaveMaximumSpeedInMPH := Train_MaximumSpeedInMPH;

            S := UnknownSignal;
            SpeedRestriction := MPH120;
            Train_MinimumAccelerationTimeInSeconds := 10;

            MaximumSpeedCheckCount := 0;
            ArrayPos := 0;
            DistanceToNextTCInInches := 0;
            WHILE (ArrayPos < High(Train_TCsAndSignalsNotClearedArray))
            { AND (DistanceToNextTCInInches <= 72) }
            DO BEGIN
              TCBeingChecked := ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[ArrayPos]);
              IF TCBeingChecked <> UnknownTrackCircuit THEN
                SpeedRestriction := TrackCircuits[TCBeingChecked].TC_SpeedRestrictionInMPH
              ELSE BEGIN
                { Perhaps a speed restriction overrides the normal speed at a junction signal }
                S := ExtractSignalFromString(Train_TCsAndSignalsNotClearedArray[ArrayPos]);
                IF S <> UnknownSignal THEN BEGIN
                  WITH Signals[S] DO BEGIN
                    IF (SignalHasLeftJunctionIndicator(S, JunctionIndicator)
                        AND ((Signals[S].Signal_IndicatorState = UpperLeftIndicatorLit)
                             OR (Signals[S].Signal_IndicatorState = MiddleLeftIndicatorLit)
                             OR (Signals[S].Signal_IndicatorState = LowerLeftIndicatorLit)))

                    OR (SignalHasLeftJunctionIndicator(S, JunctionIndicator)
                        AND ((Signals[S].Signal_IndicatorState = UpperRightIndicatorLit)
                             OR (Signals[S].Signal_IndicatorState = MiddleRightIndicatorLit)
                             OR (Signals[S].Signal_IndicatorState = LowerRightIndicatorLit)))
                    THEN BEGIN
                       IF Signal_IndicatorSpeedRestriction <> NoSpecifiedSpeed THEN BEGIN
                         TCBeingChecked := Signal_AdjacentTC;
                         SpeedRestriction := Signal_IndicatorSpeedRestriction;
                         Log(Train_LocoChipStr + ' L Checking proximity of S=' + IntToStr(S) + ' RI=' + IndicatorStateToStr(Signal_IndicatorState)
                                               + ' for speed restriction ' + MPHToStr(Signal_IndicatorSpeedRestriction));
                       END;
                    END;
                  END; {WITH}
                END;
              END;

              IF TCBeingChecked <> UnknownTrackCircuit THEN BEGIN
                { how far away is it? }
                DistanceToNextTCInInches := DistanceToNextTCInInches + TrackCircuits[TCBeingChecked].TC_LengthInInches;
                IF (SpeedRestriction <> NoSpecifiedSpeed)
                AND (((T^.Train_CurrentDirection = TrackCircuits[TCBeingChecked].TC_SpeedRestrictionDirection)
                     OR (TrackCircuits[TCBeingChecked].TC_SpeedRestrictionDirection = Bidirectional))
                   OR (T^.Train_CurrentDirection = Signals[S].Signal_Direction))
                THEN BEGIN
                  { if it's in range, or is the actual trackcircuit with the restriction, is the speed restricted? }
                  IF (CurrentTC = TCBeingChecked) OR (DistanceToNextTCInInches <= 24) THEN BEGIN
                    Inc(MaximumSpeedCheckCount);
                    IF SpeedRestriction < TempMaximumSpeedInMPH THEN
                      TempMaximumSpeedInMPH := SpeedRestriction;
                    Train_MinimumAccelerationTimeInSeconds := 0;
                  END ELSE
                    IF DistanceToNextTCInInches <= 48 THEN BEGIN
                      Inc(MaximumSpeedCheckCount);
                      IF SpeedRestriction < TempMaximumSpeedInMPH THEN
                        TempMaximumSpeedInMPH := SpeedRestriction;
                      IF Train_MinimumAccelerationTimeInSeconds <> 0 THEN
                        Train_MinimumAccelerationTimeInSeconds := 1;
                    END ELSE
                      IF DistanceToNextTCInInches <= 72 THEN BEGIN
                        Inc(MaximumSpeedCheckCount);
                        IF SpeedRestriction < TempMaximumSpeedInMPH THEN
                          TempMaximumSpeedInMPH := SpeedRestriction;
                          IF Train_MinimumAccelerationTimeInSeconds > 1 THEN
                            Train_MinimumAccelerationTimeInSeconds := 2;
                        END;
                END;

                IF TempMaximumSpeedInMPH <> SaveMaximumSpeedInMPH THEN BEGIN
                  SaveMaximumSpeedInMPH := TempMaximumSpeedInMPH;
                  Log(Train_LocoChipStr + ' L Speed set at TC=' + IntToStr(CurrentTC)
                                        + ' to ' + MPHToStr(TempMaximumSpeedInMPH) + ' mph'
                                        + ' (acc=' + IntToStr(Train_MinimumAccelerationTimeInSeconds)
                                        + ') '
                                        + IfThen(CurrentTC <> TCBeingChecked,
                                                 'which is the limit for TC=' + IntToStr(TCBeingChecked) + ' ' + FloatToStr(DistanceToNextTCInInches) + ' inches away',
                                                 '')
                                        + IfThen(MaximumSpeedCheckCount > 1,
                                                 '[' + GetOrdinalFromCardinal(MaximumSpeedCheckCount) + ' TC]',
                                                 ''));
                END;
              END;
              Inc(ArrayPos);
            END; {WHILE}
          END;
        END; {WITH}
      END; { CheckForMaximumSpeeds }

    BEGIN { CalculateDesiredSpeed }
      TRY
        WITH T^ DO BEGIN
          IF Length(Train_TCsAndSignalsNotClearedArray) > -1 THEN BEGIN
            Train_AccelerationTimeInSeconds := 0.0;

            CurrentTC := Train_CurrentTC;
            IF CurrentTC <> Train_SaveCurrentTC THEN BEGIN
              Train_CurrentTC := CurrentTC;

              { Now do some processing which is only done when we enter a new trackcircuit }
              Log(Train_LocoChipStr + ' T ***** In TC=' + IntToStr(CurrentTC) + ' (' + DescribeLineNamesForTrackCircuit(Train_CurrentTC) + ')');

              WriteStringArrayToLog(Train_LocoChip, 'T', Train_TCsAndSignalsNotClearedArray, 2, 190);

              { If the loco is marked as using trailing track circuits, clear them when the full train length allows }
              IF Train_UseTrailingTrackCircuits THEN
                ClearTrailingTrackCircuits(T);

              { Check for possible maximum speeds - do this now, before we remove the current trackcircuit from the array }
              CheckForMaximumSpeeds(T, CurrentTC, TempMaximumSpeedInMPH);

              IF IsElementInStringArray(Train_TCsAndSignalsNotClearedArray, 'TC=' + IntToStr(CurrentTC) + '*', TCPos) THEN
                DeleteElementFromStringArray(Train_TCsAndSignalsNotClearedArray, TCPos);

              IF Train_CurrentJourney <> UnknownJourney THEN
                IF TrackCircuits[CurrentTC].TC_Journey <> UnknownJourney THEN
                  Train_CurrentRoute := T^.Train_JourneysArray[TrackCircuits[CurrentTC].TC_Journey].TrainJourney_Route;

              { If we were given some extra power for the previous trackcircuit, turn it off }
              Train_ExtraPowerAdjustment := 0;

              { Initialise some variables }
              Train_DistanceToCurrentSignalOrBufferStop := 0.0;
              Train_DistanceToNextSignalOrBufferStop := 0.0;

              IF Train_CurrentSignal <> UnknownSignal THEN BEGIN
                Log(Train_LocoChipStr + ' L Resetting Train_CurrentSignal (it was S=' + IntToStr(Train_CurrentSignal) + ')');
                Train_CurrentSignal := UnknownSignal;
              END;
              IF Train_CurrentBufferStop <> UnknownBufferStop THEN BEGIN
                Log(Train_LocoChipStr + ' L Resetting Train_CurrentBufferStop (it was BS=' + IntToStr(Train_CurrentBufferStop) + ')');
                Train_CurrentBufferStop := UnknownBufferStop;
              END;

              { If we're in a trackcircuit following a signal, find the next signal or bufferstop }
              FindFirstFourOccurrencesInStringArray('FS=', Train_TCsAndSignalsNotClearedArray, FirstSignalPos, SecondSignalPos,
                                                    ThirdSignalPos, FourthSignalPos);
              IF FirstSignalPos > -1 THEN BEGIN
                IF ExtractSignalFromString(Train_TCsAndSignalsNotClearedArray[FirstSignalPos]) <> Train_LastSignal THEN
                  Train_CurrentSignal := ExtractSignalFromString(Train_TCsAndSignalsNotClearedArray[FirstSignalPos])
                ELSE
                  { in case the same signal appears twice in the list }
                  IF SecondSignalPos = -1 THEN BEGIN
                    Log(Train_LocoChipStr + ' L Resetting Train_CurrentSignal: the first signal S=' + IntToStr(Train_CurrentSignal)
                                          + ' is the same as the last signal - cannot replace with second signal as there isn''t one');
                    Train_CurrentSignal := UnknownSignal;
                  END ELSE BEGIN
                    Log(Train_LocoChipStr + ' L The first signal S=' + IntToStr(Train_CurrentSignal) + ' is the same as the last signal'
                                          + ' - replacing with second signal S=' + IntToStr(ExtractSignalFromString(Train_TCsAndSignalsNotClearedArray[SecondSignalPos])));
                    Train_CurrentSignal := ExtractSignalFromString(Train_TCsAndSignalsNotClearedArray[SecondSignalPos]);

                    FirstSignalPos := SecondSignalPos;
                    SecondSignalPos := ThirdSignalPos;
                    ThirdSignalPos := FourthSignalPos;
                  END;
              END;

              BufferStopPos := 0;
              IF Train_CurrentSignal <> UnknownSignal THEN BEGIN
                Log(Train_LocoChipStr + ' L Train_CurrentSignal is S=' + IntToStr(Train_CurrentSignal));
                WriteStringArrayToLog(Train_LocoChip, 'L', Train_TCsAndSignalsNotClearedArray, 2, 190)
              END ELSE BEGIN
                { look for a bufferstop }
                BufferStopPos := FindFirstOccurrenceInStringArray('BS=', Train_TCsAndSignalsNotClearedArray);
                IF BufferStopPos <> -1 THEN BEGIN
                  Train_CurrentBufferStop := ExtractBufferStopFromString(Train_TCsAndSignalsNotClearedArray[BufferStopPos]);
                  Log(Train_LocoChipStr + ' L Train_CurrentBufferStop is BS=' + IntToStr(Train_CurrentBufferStop));
                END ELSE
                  { Hmm. What do we do now? (Is this likely to happen?) **** }
                  Log(Train_LocoChipStr + ' LG No signal or buffer stop found in Train_TCsAndSignalsNotClearedArray');
              END;

              Train_SaveCurrentTC := CurrentTC;

              { Now work out if we've reached the signal }
              NextSignal := UnknownSignal;
              Train_AtCurrentSignal := UnknownSignal;
              Train_AtCurrentBufferStop := UnknownBufferStop;
              Train_AtHiddenAspectSignal := UnknownSignal;

              IF Train_CurrentSignal = UnknownSignal THEN BEGIN
                IF Train_CurrentBufferStop <> UnknownBufferStop THEN
                  IF TrackCircuits[CurrentTC].TC_AdjacentBufferStop = Train_CurrentBufferStop THEN
                    Train_AtCurrentBufferStop := Train_CurrentBufferStop;
              END ELSE BEGIN
                { we have a current signal }
                FindFirstFourOccurrencesInStringArray('FS=', Train_TCsAndSignalsNotClearedArray, FirstSignalPos, SecondSignalPos, ThirdSignalPos, FourthSignalPos);
                IF IsElementInIntegerArray(TrackCircuits[CurrentTC].TC_AdjacentSignals, Train_CurrentSignal) THEN
                  Train_AtCurrentSignal := Train_CurrentSignal;

                IF Train_AtCurrentSignal <> UnknownSignal THEN BEGIN
                  IF (Train_CurrentJourney <> UnknownJourney)
                  AND (((Train_CurrentSignal = Train_JourneysArray[Train_CurrentJourney].TrainJourney_StartSignal)
                       AND (Signals[Train_JourneysArray[Train_CurrentJourney].TrainJourney_StartSignal].Signal_HiddenAspect = RedAspect))
                    OR ((Train_CurrentSignal = Train_JourneysArray[Train_CurrentJourney].TrainJourney_EndSignal)
                       AND (Signals[Train_JourneysArray[Train_CurrentJourney].TrainJourney_EndSignal].Signal_HiddenAspect = RedAspect)))
                  AND ((Train_CurrentJourney < Train_TotalJourneys)
                      AND (Train_JourneysArray[Train_CurrentJourney].TrainJourney_Direction = Train_JourneysArray[Train_CurrentJourney + 1].TrainJourney_Direction))
                  THEN
                    Train_AtHiddenAspectSignal := Train_AtCurrentSignal;
                END;
              END;

              IF (Train_CurrentSignal = UnknownSignal)
              AND (NextSignal = UnknownSignal)
              AND (Train_CurrentBufferStop = UnknownBufferStop)
              THEN
                Log(Train_LocoChipStr + ' X! Train_CurrentSignal = UnknownSignal and NextSignal = UnknownSignal'
                                      + ' and Train_CurrentBufferStop = UnknownBufferStop - something wrong here');

              { Now work out how far away the next signals are - this needs to be recalculated each time the current trackcircuit changes; if the signal isn't at the start
                of the array, calculate the distance to it, but ignore the first trackcircuit, as we may already be in that.
              }
              IF (Train_AtCurrentSignal = UnknownSignal)
              AND (Train_AtCurrentBufferStop = UnknownBufferStop)
              THEN BEGIN
                IF FirstSignalPos = -1 THEN BEGIN
                  { it may be we're at the route end, or it may be there's a buffer stop ahead }
                  IF BufferStopPos <> -1 THEN BEGIN
                    { Now count up the lengths of all the trackcircuits preceding it }
                    FOR I := 0 TO (BufferStopPos - 1) DO
                      IF ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I]) <> UnknownTrackCircuit THEN
                        Train_DistanceToCurrentSignalOrBufferStop := Train_DistanceToCurrentSignalOrBufferStop
                                                                    + TrackCircuits[ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I])].TC_LengthInInches;
                  END;
                END ELSE BEGIN
                  { Now count up the lengths of all the trackcircuits preceding it, ignoring subroute markers }
                  FOR I := 0 TO (FirstSignalPos - 1) DO BEGIN
                    IF ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I]) <> UnknownTrackCircuit THEN
                      Train_DistanceToCurrentSignalOrBufferStop := Train_DistanceToCurrentSignalOrBufferStop
                                                                    + TrackCircuits[ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I])].TC_LengthInInches;
                  END;
                END;
              END;

              { Now work out how far it is to the next signal }
              Train_DistanceToNextSignalOrBufferStop := Train_DistanceToCurrentSignalOrBufferStop;
              IF SecondSignalPos = -1 THEN BEGIN
                { it may be we're at the route end, or it may be there's a buffer stop ahead }
                IF BufferStopPos <> -1 THEN BEGIN
                  { Now count up the lengths of all the trackcircuits preceding it }
                  FOR I := 0 TO (BufferStopPos - 1) DO BEGIN
                    IF ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I]) <> UnknownTrackCircuit THEN
                      Train_DistanceToNextSignalOrBufferStop := Train_DistanceToNextSignalOrBufferStop
                                                                    + TrackCircuits[ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I])].TC_LengthInInches;
                  END;
                END;
              END ELSE BEGIN
                NextSignal := ExtractSignalFromString(Train_TCsAndSignalsNotClearedArray[SecondSignalPos]);
                { Now count up the lengths of all the trackcircuits preceding it, ignoring subroute markers }
                FOR I := (FirstSignalPos + 1) TO (SecondSignalPos - 1) DO
                  IF ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I]) <> UnknownTrackCircuit THEN
                    Train_DistanceToNextSignalOrBufferStop := Train_DistanceToNextSignalOrBufferStop
                                                                    + TrackCircuits[ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I])].TC_LengthInInches;
              END;
              { And to the next signal but one }
              Train_DistanceToNextSignalButOneOrBufferStop := 0.0;
              IF ThirdSignalPos = -1 THEN BEGIN
                { it may be we're at the route end, or it may be there's a buffer stop ahead }
                IF BufferStopPos <> -1 THEN BEGIN
                  { Now count up the lengths of all the trackcircuits preceding it }
                  FOR I := 0 TO (BufferStopPos - 1) DO BEGIN
                    IF ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I]) <> UnknownTrackCircuit THEN
                      Train_DistanceToNextSignalButOneOrBufferStop := Train_DistanceToNextSignalButOneOrBufferStop
                                                                    + TrackCircuits[ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I])].TC_LengthInInches;
                  END;
                END;
              END ELSE BEGIN
                { Now count up the lengths of all the trackcircuits preceding it, ignoring subroute markers }
                FOR I := 0 TO ThirdSignalPos - 1 DO
                  IF ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I]) <> UnknownTrackCircuit THEN
                    Train_DistanceToNextSignalButOneOrBufferStop := Train_DistanceToNextSignalButOneOrBufferStop
                                                                    + TrackCircuits[ExtractTrackCircuitFromString(Train_TCsAndSignalsNotClearedArray[I])].TC_LengthInInches;
              END;

              IF Train_AtCurrentSignal <> UnknownSignal THEN BEGIN
                Log(Train_LocoChipStr + ' L At next signal S=' + IntToStr(Train_CurrentSignal));
                Train_LastSignal := Train_CurrentSignal;
                WriteStringArrayToLog(Train_LocoChip, 'L', Train_TCsAndSignalsNotClearedArray, 2, 190);
                DeleteElementFromStringArray(Train_TCsAndSignalsNotClearedArray, FirstSignalPos);
                WriteStringArrayToLog(Train_LocoChip, 'L', Train_TCsAndSignalsNotClearedArray, 2, 190);

                { Also see if the next array element is the same signal - e.g. FS=99=, FS=99\ - if it is, then remove it too }
                IF Length(Train_TCsAndSignalsNotClearedArray) > FirstSignalPos THEN BEGIN
                  IF ExtractSignalFromString(Train_TCsAndSignalsNotClearedArray[0]) = Train_CurrentSignal THEN BEGIN
                    Log(Train_LocoChipStr + ' L *** Deleting second occurrence of ' + IntToStr(Train_CurrentSignal));
                    DeleteElementFromStringArray(Train_TCsAndSignalsNotClearedArray, 0);
                    WriteStringArrayToLog(Train_LocoChip, 'L', Train_TCsAndSignalsNotClearedArray, 2, 190);
                  END;
                END;
              END ELSE
                IF Train_AtCurrentBufferStop <> UnknownBufferStop THEN BEGIN
                  Log(Train_LocoChipStr + ' L *** At next bufferstop BS=' + IntToStr(Train_CurrentBufferStop));
                  DeleteElementFromStringArray(Train_TCsAndSignalsNotClearedArray, BufferStopPos);
                END;
            END; { Processing which is only done when we enter a new trackcircuit }

            { If the train is in the adjacent TC to an approach-controlled signal, make sure its speed is suitably reduced **** }

            { Now continue processing whether we're in a new trackcircuit or not - a signal may change while we're traversing one. Our speed depends on the distance to
              next red aspect, if it's fairly close (i.e. YY, Y, R) or at a station we have to stop at it even if the adjacent aspect is not red. Set the minimum speed -
              bear in mind that scale mph and real mph are different - when speed-checking locos, it is the case that the minimum speed for most locos (i.e. before
              stalling) is 30mph.
            }
            SpeedCalculationStr := '';
            StopStr := '';

            IF (Train_CurrentSignal = UnknownSignal)
            AND (NextSignal = UnknownSignal)
            AND (Train_CurrentBufferStop = UnknownBufferStop)
            THEN BEGIN
              { we shouldn't ever get here, but sometimes this happens, and the train creeps passed red signals }
              Log(Train_LocoChipStr + ' X! Train_CurrentSignal = UnknownSignal and NextSignal = UnknownSignal'
                                    + ' and Train_CurrentBufferStop = UnknownBufferStop - something wrong here');
              WriteStringArrayToLog(Train_LocoChip, 'X', 'Train_TCsAndSignalsNotClearedArray', Train_TCsAndSignalsNotClearedArray, 2, 190);
              Train_DesiredSpeedInMPH := Stop;
            END ELSE BEGIN
              IF ((Train_AtCurrentSignal <> UnknownSignal)
                  AND (Signals[Train_AtCurrentSignal].Signal_Aspect = RedAspect))
              OR ((Train_AtHiddenAspectSignal <> UnknownSignal)
                  AND ((Train_JourneysArray[Train_CurrentJourney].TrainJourney_EndSignal <> UnknownSignal)
                  AND (Signals[Train_JourneysArray[Train_CurrentJourney].TrainJourney_EndSignal].Signal_HiddenAspect = RedAspect)))
              OR (Train_AtCurrentBufferStop <> UnknownBufferStop)
              THEN BEGIN
                MinimumSpeedInMPH := Stop;
                IF Train_AtCurrentBufferStop <> UnknownBufferStop THEN
                  StopStr := 'MinimumSpeedInMPH = Stop as Train_AtCurrentBufferStop BS=' + IntToStr(Train_AtCurrentBufferStop)
                ELSE
                  StopStr := 'MinimumSpeedInMPH = Stop as Train_AtCurrentSignal S=' + IntToStr(Train_AtCurrentSignal);
              END ELSE BEGIN
                TempStr := IfThen(Train_CurrentSignal <> UnknownSignal,
                                  'S=' + IntToStr(Train_CurrentSignal),
                                  'BS=' + IntToStr(Train_CurrentBufferStop));
                IF Train_DistanceToCurrentSignalOrBufferStop > 50.0 THEN BEGIN
                  MinimumSpeedInMPH := MPH40;
                  StopStr := 'MinimumSpeedInMPH = 40 (Lenz=' + IntToStr(TrainSpeedInMPHToLenzSpeed(T, MinimumSpeedInMPH))
                             + ') as Train_DistanceToCurrentSignalOrBufferStop ' + TempStr + ' > 50.0';
                END ELSE
                  IF Train_DistanceToCurrentSignalOrBufferStop > 40.0 THEN BEGIN
                    MinimumSpeedInMPH := MPH30;
                    StopStr := 'MinimumSpeedInMPH = 30 (Lenz=' + IntToStr(TrainSpeedInMPHToLenzSpeed(T, MinimumSpeedInMPH))
                               + ') as Train_DistanceToCurrentSignalOrBufferStop ' + TempStr + ' > 40.0';
                  END ELSE BEGIN
                    MinimumSpeedInMPH := MPH20;
                    StopStr := 'MinimumSpeedInMPH = 20 (Lenz=' + IntToStr(TrainSpeedInMPHToLenzSpeed(T, MinimumSpeedInMPH))
                               + ') as Train_DistanceToCurrentSignalOrBufferStop ' + TempStr + ' <= 40.0';
                  END;
              END;
        
              { Now calculate the speed per trackcircuit }
              SaveTrainDesiredSpeedInMPH := Train_DesiredSpeedInMPH;

              { TCLineNames[0] is only one of a number of possible lines **** }
              CASE Lines[TrackCircuits[CurrentTC].TC_LineArray[0]].Line_TypeOfLine OF
                MainLine, MainStationLine, MainOrGoods:
                  IF ((Train_DistanceToCurrentSignalOrBufferStop = 0)
                      OR (Train_DistanceToCurrentSignalOrBufferStop > 36.0))
                  AND ((Train_DistanceToNextSignalOrBufferStop = 0) OR (Train_DistanceToNextSignalOrBufferStop > 72.0))
                  AND ((Train_DistanceToNextSignalButOneOrBufferStop = 0) OR (Train_DistanceToNextSignalButOneOrBufferStop > 96.0))
                  THEN BEGIN
                    IF (Train_Type = ExpressPassenger) OR (Train_Type = ExpressFreight) THEN BEGIN
                      Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MinimumSpeedInMPH, MPH50, MPH70, MPH120);
                      SpeedCalculationStr := 'To Current S/BS = 0 or > 36.0 (is ' + FloatToStr(Train_DistanceToCurrentSignalOrBufferStop) + ')'
                                             + ' To Next S/BS =0 or > 72.0 (is ' + FloatToStr(Train_DistanceToNextSignalOrBufferStop) + ')'
                                             + ' To Next But One S/BS = 0 or > 96.0 (is ' + FloatToStr(Train_DistanceToNextSignalButOneOrBufferStop) + ')'
                                             + ' express - options are ' + MPHToStr(MinimumSpeedInMPH) + ', 50, 70, 120, result is ' + MPHToStr(Train_DesiredSpeedInMPH);
                    END ELSE BEGIN
                      Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MinimumSpeedInMPH, MPH50, MPH70, MPH90);
                      SpeedCalculationStr := 'To Current S/BS = 0 or > 36.0 (is ' + FloatToStr(Train_DistanceToCurrentSignalOrBufferStop) + ')'
                                             + ' To Next S/BS = 0 or > 72.0 (is ' + FloatToStr(Train_DistanceToNextSignalOrBufferStop) + ')'
                                             + ' To Next But One S/BS = 0 or > 96.0 (is ' + FloatToStr(Train_DistanceToNextSignalButOneOrBufferStop) + ')'
                                             + ' not express - options are ' + MPHToStr(MinimumSpeedInMPH) + ', 50, 70, 90, result is ' + MPHToStr(Train_DesiredSpeedInMPH);
                    END;
                  END ELSE BEGIN
                    { ((Train_DistanceToCurrentSignalOrBufferStop <> UnknownBufferStop
                        AND Train_DistanceToCurrentSignalOrBufferStop <= 36.0))
                      OR (Train_DistanceToNextSignalOrBufferStop <= 72.0)
                      OR (Train_DistanceToNextSignalButOneOrBufferStop <= 96.0)
                    }
                    IF (Train_Type = ExpressPassenger) OR (Train_Type = ExpressFreight) THEN BEGIN
                      Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MinimumSpeedInMPH, MPH40, MPH60, MPH120);
                      SpeedCalculationStr := 'To Current S/BS <= 36.0 (is ' + FloatToStr(Train_DistanceToCurrentSignalOrBufferStop) + ')'
                                             + ' To Next S/BS <= 72.0 (is ' + FloatToStr(Train_DistanceToNextSignalOrBufferStop) + ')'
                                             + ' To Next But One S/BS <= 96.0 (is ' + FloatToStr(Train_DistanceToNextSignalButOneOrBufferStop) + ')'
                                             + ' express - options are ' + MPHToStr(MinimumSpeedInMPH) + ', 40, 60, 120, result is ' + MPHToStr(Train_DesiredSpeedInMPH);
                    END ELSE BEGIN
                      Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MinimumSpeedInMPH, MPH30, MPH50, MPH90);
                      SpeedCalculationStr := 'To Current S/BS <= 36.0 (is ' + FloatToStr(Train_DistanceToCurrentSignalOrBufferStop) + ')'
                                             + ' To Next S/BS <= 72.0 (is ' + FloatToStr(Train_DistanceToNextSignalOrBufferStop) + ')'
                                             + ' To Next But One S/BS <= 96.0 (is ' + FloatToStr(Train_DistanceToNextSignalButOneOrBufferStop) + ')'
                                             + ' not express - options are ' + MPHToStr(MinimumSpeedInMPH) + ', 30, 50, 90, result is ' + MPHToStr(Train_DesiredSpeedInMPH);
                    END;
                  END;

                GoodsLine:
                  IF (Train_DistanceToCurrentSignalOrBufferStop > 36.0)
                  AND (Train_DistanceToNextSignalButOneOrBufferStop > 48.0)
                  THEN
                    Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MinimumSpeedInMPH, MPH50, MPH60, MPH70)
                  ELSE
                    { Train_DistanceToCurrentSignalOrBufferStop <= 36.0 }
                    Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MinimumSpeedInMPH, MPH30, MPH50, MPH70);

                BranchLineSingle, BranchLineDouble, StationAvoiding, IslandStationLine, BranchStationLine, WindowStationLine:
      //        IF (Train_AtCurrentSignal <> UnknownSignal)
      //        AND (Signals[Train_AtCurrentSignal].Signal_Aspect = RedAspect)
      //        OR (Train_AtCurrentBufferStop <> UnknownBufferStop)
      //        THEN BEGIN
      //          Train_AccelerationTimeInSeconds := 1.0;
      //          Train_DesiredSpeedInMPH := Stop;
      //          makesound(1);
      //        END ELSE
                  IF (Train_DistanceToCurrentSignalOrBufferStop > 36.0)
                  AND (Train_DistanceToNextSignalButOneOrBufferStop > 48.0)
                  THEN
                    Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MinimumSpeedInMPH, MPH40, MPH50, MPH60)
                  ELSE
                    { Train_DistanceToCurrentSignalOrBufferStop <= 36.0 }
                    Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MinimumSpeedInMPH, MPH30, MPH40, MPH60);

                FiddleyardLine:
                  { As the fiddleyard is a hidden final destination, we don't need to have scale speeds or acceleration }
                  BEGIN
                    DebugStr := '';
                    IF ((Train_AtCurrentSignal <> UnknownSignal)
                        AND (Signals[Train_AtCurrentSignal].Signal_Aspect = RedAspect))
                    OR (Train_AtCurrentBufferStop <> UnknownBufferStop)
                    THEN BEGIN
                      Train_AccelerationTimeInSeconds := 0.0;
                      Train_DesiredSpeedInMPH := Stop;

                      IF (Train_AtCurrentSignal <> UnknownSignal)
                      AND (Signals[Train_AtCurrentSignal].Signal_Aspect = RedAspect)
                      THEN
                        DebugStr := 'Train_AtCurrentSignal = ' + IntToStr(Train_AtCurrentSignal)
                                    + ' AND (Signals[' + IntToStr(Train_AtCurrentSignal) + '].Signal_Aspect = RedAspect so DesiredSpeedInMPH = Stop';
                      IF (Train_AtCurrentBufferStop <> UnknownBufferStop) THEN
                        DebugStr := 'Train_AtCurrentBufferStop = ' + IntToStr(Train_AtCurrentBufferStop) + ' so DesiredSpeedInMPH = Stop';
                    END ELSE
                      IF ((Train_AtCurrentSignal = UnknownSignal)
                          AND (Train_AtCurrentBufferStop = UnknownBufferStop))
                      AND (Train_DistanceToCurrentSignalOrBufferStop <= 24.0)
                      AND (((Train_CurrentSignal <> UnknownSignal)
                             AND (Signals[Train_CurrentSignal].Signal_Aspect = RedAspect))
                           OR ((Train_CurrentBufferStop <> UnknownBufferStop)))
                      THEN BEGIN
                        Train_AccelerationTimeInSeconds := 0.0;
                        Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MPH30, MPH30, MPH60, MPH60);
                        DebugStr := '((Train_AtCurrentSignal = UnknownSignal) AND (Train_AtCurrentBufferStop = UnknownBufferStop))'
                                    + ' AND (Train_DistanceToCurrentSignalOrBufferStop <= 24.0) ['
                                    + FloatToStr(Train_DistanceToCurrentSignalOrBufferStop) + ']';
                        IF ((Train_CurrentSignal <> UnknownSignal)
                            AND (Signals[Train_CurrentSignal].Signal_Aspect = RedAspect))
                        THEN
                          DebugStr := DebugStr + ' AND (((Train_CurrentSignal = ' + IntToStr(Train_CurrentSignal)
                                               + ' AND (Signals[' + IntToStr(Train_CurrentSignal) + '].Signal_Aspect = RedAspect))'
                        ELSE
                          DebugStr := DebugStr + ' AND (Train_CurrentBufferStop [' + IntToStr(Train_CurrentBufferStop) + '] <> UnknownBufferStop)))';
                      END ELSE BEGIN
                        Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MPH40, MPH60, MPH80, MPH90);
                        Train_AccelerationTimeInSeconds := 2.0;
                        DebugStr := 'Train_AccelerationTimeInSeconds := 1.0'
                                    + ' Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal [' + IntToStr(Train_CurrentSignal) + '],'
                                    + ' Train_DistanceToNextSignalOrBufferStop [' + FloatToStr(Train_DistanceToNextSignalOrBufferStop) + '],'
                                    + ' MPH40, MPH90, MPH90, MPH90);'
                                    + '[ = ' + MPHToStr(Train_DesiredSpeedInMPH) + ']';

                        IF Train_DesiredSpeedInMPH <> Train_CurrentSpeedInMPH THEN BEGIN
                          IF Train_DesiredSpeedInMPH > Train_CurrentSpeedInMPH THEN BEGIN
                            { we're accelerating }
                            Train_AccelerationTimeInSeconds := 2.0;
                            DebugStr := DebugStr + ' - accelerating';
                          END ELSE BEGIN
                            { decelerating }
                            Train_AccelerationTimeInSeconds := 0.0;
                            DebugStr := DebugStr + ' - decelerating';
                          END;
                        END;
                      END;
                  END;
                SidingLine, SidingsApproach:
                  Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MinimumSpeedInMPH, MPH30, MPH40, MPH40);
                ProjectedLine:
                  Train_DesiredSpeedInMPH := Stop;
              ELSE
                BEGIN
                  { in case we've forgotten a type of line }
                  { TCLineNames[0] is only one of a number of possible lines **** }
                  Log(Train_LocoChipStr + ' XG Unknown line type found in CalculateDesiredSpeed - line=' + LineToStr(TrackCircuits[CurrentTC].TC_LineArray[0]));
                  Train_DesiredSpeedInMPH := SpeedAtSignal(T, Train_CurrentSignal, MinimumSpeedInMPH, MPH10, MPH20, MPH30);
                END;
              END; {CASE}

              IF DebugStr <> Train_SaveSpeedInFiddleyardMsg THEN BEGIN
                DrawLineInLogFile(Train_LocoChip, 'X', '_', UnitRef);
                Log(Train_LocoChipStr + ' X ' + DebugStr);
                DrawLineInLogFile(Train_LocoChip, 'X', '_', UnitRef);
                Train_SaveSpeedInFiddleyardMsg := DebugStr;
              END;

              AdjustSpeedForGradient(T, CurrentTC);

              { Now implement maximum speeds }   { this doesn't work 4/3/13 ***** }
    //          IF TempMaximumSpeedInMPH <> Train_MaximumSpeedInMPH THEN BEGIN
    //            IF Train_DesiredSpeedInMPH > TempMaximumSpeedInMPH THEN BEGIN
    //              Train_DesiredSpeedInMPH := TempMaximumSpeedInMPH;
    //              Train_AccelerationTimeInSeconds := Train_MinimumAccelerationTimeInSeconds;
    //            END;
    //          END;
            END;

            { Now convert the speed to a Lenz speed number }
            Train_DesiredLenzSpeed := TrainSpeedInMPHToLenzSpeed(T, Train_DesiredSpeedInMPH);
            { and add any extra necessary speed steps }
            IF Train_DesiredLenzSpeed <> 0 THEN
              Train_DesiredLenzSpeed := Train_DesiredLenzSpeed + Train_GradientSpeedAdjustment + Train_ExtraPowerAdjustment + Train_UserPowerAdjustment;
            IF Train_DesiredLenzSpeed > 28 THEN
              Train_DesiredLenzSpeed := 28
            ELSE
              IF Train_DesiredLenzSpeed < 0 THEN
                Train_DesiredLenzSpeed := 0;

            IF Train_AccelerationTimeInSeconds = 0.0 THEN BEGIN
              { otherwise it might have been set by the maximum speed routine above }
              IF Train_DesiredLenzSpeed = Train_CurrentLenzSpeed THEN
                { this shouldn't be necessary, but was needed on at least one occasion }
                Train_AccelerationTimeInSeconds := 0.0
              ELSE
                IF (Train_DesiredLenzSpeed > Train_CurrentLenzSpeed) THEN
                  Train_AccelerationTimeInSeconds := 7.0
                ELSE BEGIN
                  { If we're decelerating, set the rate based on the distances to the signal - the default rate first }
                  Train_AccelerationTimeInSeconds := 5.0;

                  IF (Train_AtCurrentSignal <> UnknownSignal)
                  AND (Train_CurrentSignal <> UnknownSignal)
                  AND ((Signals[Train_CurrentSignal].Signal_Aspect = RedAspect)
                        OR (Signals[Train_CurrentSignal].Signal_HiddenAspect = RedAspect))
                  THEN BEGIN
                    { set a very short acceleration/deceleration time if we're stopping, but see how long the stop section is }
                    IF TrackCircuits[Signals[Train_CurrentSignal].Signal_AdjacentTC].TC_LengthInInches > 40.0 THEN
      begin
                      Train_AccelerationTimeInSeconds := 1.0
      ;
      // debug('at S=' + Inttostr(Train_AtCurrentSignal) + ' tc=' + inttostr(Signals[Train_CurrentSignal].Signal_AdjacentTC) + ' length > 40, time=' + FloatToStr(Train_AccelerationTimeInSeconds));
      end
                    ELSE
                      IF TrackCircuits[Signals[Train_CurrentSignal].Signal_AdjacentTC].TC_LengthInInches > 30.0 THEN
      begin
                        Train_AccelerationTimeInSeconds := 0.5
      ;
      // debug('at S=' + Inttostr(Train_AtCurrentSignal) + ' tc=' + inttostr(Signals[Train_CurrentSignal].Signal_AdjacentTC) + ' length > 30, time=' + FloatToStr(Train_AccelerationTimeInSeconds));
      end
                      ELSE
                        IF TrackCircuits[Signals[Train_CurrentSignal].Signal_AdjacentTC].TC_LengthInInches > 20.0 THEN
      begin
                          Train_AccelerationTimeInSeconds := 0.2
      ;
      // debug('at S=' + Inttostr(Train_AtCurrentSignal) + ' tc=' + inttostr(Signals[Train_CurrentSignal].Signal_AdjacentTC) + ' length > 20, time=' + FloatToStr(Train_AccelerationTimeInSeconds));
      end
                        ELSE
      begin
                          Train_AccelerationTimeInSeconds := 0.0;
      // debug('at S=' + Inttostr(Train_AtCurrentSignal) + ' tc=' + inttostr(Signals[Train_CurrentSignal].Signal_AdjacentTC) + ' length <= 20, time=' + FloatToStr(Train_AccelerationTimeInSeconds));
      end;
                  END ELSE
                    IF Train_AtCurrentBufferStop <> UnknownBufferStop THEN BEGIN
                      { set a very short acceleration/deceleration time if we're stopping, but see how long the stop section is }
                      IF TrackCircuits[BufferStops[Train_CurrentBufferStop].BufferStop_AdjacentTrackCircuit].TC_LengthInInches > 40.0 THEN
                        Train_AccelerationTimeInSeconds := 2.0
                      ELSE
                        IF TrackCircuits[BufferStops[Train_CurrentBufferStop].BufferStop_AdjacentTrackCircuit].TC_LengthInInches > 30.0 THEN
                          Train_AccelerationTimeInSeconds := 1.0
                        ELSE
                          IF TrackCircuits[BufferStops[Train_CurrentBufferStop].BufferStop_AdjacentTrackCircuit].TC_LengthInInches > 20.0 THEN
                            Train_AccelerationTimeInSeconds := 0.5
                          ELSE
                            Train_AccelerationTimeInSeconds := 0.0;
                    END ELSE BEGIN
                      IF (Train_CurrentSignal <> UnknownSignal)
                      AND ((Signals[Train_CurrentSignal].Signal_Aspect = RedAspect)
                           OR (Signals[Train_CurrentSignal].Signal_HiddenAspect = RedAspect))
                      OR (Train_CurrentBufferStop <> UnknownBufferStop) THEN BEGIN
                        IF Train_DistanceToCurrentSignalOrBufferStop > 60.0 THEN
                          Train_AccelerationTimeInSeconds := 3.0
                        ELSE
                          IF Train_DistanceToCurrentSignalOrBufferStop > 50.0 THEN
                            Train_AccelerationTimeInSeconds := 2.0
                          ELSE
                            Train_AccelerationTimeInSeconds := 1.0;
                      END ELSE BEGIN
                        IF Train_DistanceToNextSignalOrBufferStop > 60.0 THEN
                          Train_AccelerationTimeInSeconds := 5.0
                        ELSE
                          IF Train_DistanceToNextSignalOrBufferStop > 50.0 THEN
                            Train_AccelerationTimeInSeconds := 3.0
                          ELSE
                            Train_AccelerationTimeInSeconds := 2.0;
                      END;
                    END;
              END;
            END;

            { Finally, to stop too fast approaches to a red aspect - not in use as of 6/1/09 but keep in case needed at some time }
            IF TerminatingSpeedReductionMode THEN BEGIN
              IF (Train_AtCurrentSignal = UnknownSignal)
              AND (Train_CurrentSignal <> UnknownSignal)
              AND ((Signals[Train_CurrentSignal].Signal_Aspect = RedAspect)
                    OR (Signals[Train_CurrentSignal].Signal_HiddenAspect = RedAspect))
              AND (Train_DistanceToCurrentSignalOrBufferStop < 12.0)
              AND (Train_DesiredSpeedInMPH <> MPH0)
              THEN BEGIN
                Dec(Train_DesiredLenzSpeed);
                IF NOT Train_TerminatingSpeedReductionMsgWritten THEN BEGIN
                  Train_TerminatingSpeedReductionMsgWritten := True;
                  Train_AccelerationTimeInSeconds := 1.0;
                  Log(Train_LocoChipStr + ' L Terminating speed reduction at TC=' + IntToStr(CurrentTC));
                END;
              END ELSE
                Train_TerminatingSpeedReductionMsgWritten := False;
            END;

            DebugStr := 'Desired speed at TC=' + IntToStr(CurrentTC)
                        + ' [' + DescribeLineNamesForTrackCircuit(CurrentTC) + ']'
                        + ': ' + IntToStr(Train_DesiredLenzSpeed)
                        + IfThen(Train_ExtraPowerAdjustment > 0,
                                 ' [+' + IntToStr(Train_ExtraPowerAdjustment) + ']')
                        + IfThen(Train_UserPowerAdjustment > 0,
                                 ' [+' + IntToStr(Train_UserPowerAdjustment) + ']')
                        + IfThen(Train_UserPowerAdjustment < 0,
                                 ' [' + IntToStr(Train_UserPowerAdjustment) + ']')
                        + IfThen(Train_GradientSpeedAdjustment > 0,
                                 ' [+' + IntToStr(Train_GradientSpeedAdjustment) + '[')
                        + IfThen(Train_GradientSpeedAdjustment < 0,
                                 ' [' + IntToStr(Train_GradientSpeedAdjustment) + '}')
                        + ' (' + MPHToStr(Train_DesiredSpeedinMPH) + ' mph)'
                        + ' J=' + IntToStr(Train_CurrentJourney)
                        + ' R=' + IntToStr(Train_CurrentRoute);

            { One has to build up the DebugStr this way, rather than using the IfThen function as it does complete boolean evaluation on the whole expression }
            IF Train_CurrentSignal = UnknownSignal THEN
              DebugStr := DebugStr + ' CS=0'
            ELSE BEGIN
              DebugStr := DebugStr + IfThen(Train_CurrentSignal = UnknownSignal,
                                            ' [CS=-',
                                            ' [CS=' + IntToStr(Train_CurrentSignal) + ' a:' + AspectToStr(Signals[Train_CurrentSignal].Signal_Aspect, ShortStringType));
              IF Signals[Train_CurrentSignal].Signal_HiddenAspect <> NoAspect THEN
                DebugStr := DebugStr + ' ha=' + AspectToStr(Signals[Train_CurrentSignal].Signal_HiddenAspect, ShortStringType);
              DebugStr := DebugStr + ' sl=' + LineToStr(GetSignalAdjacentLine(Train_CurrentSignal)) + ']'
                                   + ' CSInd=' + IndicatorStateToStr(Signals[Train_CurrentSignal].Signal_IndicatorState);
            END;

            DebugStr := DebugStr + IfThen(NextSignal <> UnknownSignal,
                                          ' NS=' + IntToStr(NextSignal),
                                          ' NS-')
                                 + IfThen(Train_CurrentBufferStop <> UnknownBufferStop,
                                          ' BS=' + IntToStr(Train_CurrentBufferStop),
                                          ' BS=-')
                                 + ' AccTm=' + FloatToStr(Train_AccelerationTimeInSeconds);

            IF Train_CurrentSignal <> UnknownSignal THEN BEGIN
              DebugStr := DebugStr + ' S_ATC=' + IntToStr(Signals[Train_CurrentSignal].Signal_AdjacentTC);
              IF Signals[Train_CurrentSignal].Signal_AdjacentTC <> UnknownTrackCircuit THEN
                DebugStr := DebugStr + ' S_ATCLen=' + FloatToStr(TrackCircuits[Signals[Train_CurrentSignal].Signal_AdjacentTC].TC_LengthInInches);
            END;

            DebugStr := DebugStr + ' L_CTC=' + IntToStr(Train_CurrentTC)
                                 + ' DistToCS=' + FloatToStr(Train_DistanceToCurrentSignalOrBufferStop)
                                 + IfThen(Train_GradientSpeedAdjustment > 0,
                                          ' TGSA=' + IntToStr(Train_GradientSpeedAdjustment))
                                 + IfThen(Train_ExtraPowerAdjustment > 0,
                                          ' TEPA=' + IntToStr(Train_ExtraPowerAdjustment))
                                 + IfThen(Train_UserPowerAdjustment > 0,
                                          ' TUPA=' + IntToStr(Train_UserPowerAdjustment))
                                 + ' DesDir=' + DirectionToStr(T^.Train_CurrentDirection)
                                 + ' CurrDir=' + DirectionToStr(T^.Train_CurrentDirection);

            { and write out the data in a comprehensible way }
            IF Train_SpeedString <> DebugStr THEN BEGIN
              Log(Train_LocoChipStr + ' L ' + DebugStr + ' {INDENT=0} {WRAP=SCREENWIDTH}');
              Train_SpeedString := DebugStr;
              IF StopStr <> '' THEN
                Log(Train_LocoChipStr + ' L ' + StopStr);
              IF SpeedCalculationStr <> '' THEN
                Log(Train_LocoChipStr + ' L ' + SpeedCalculationStr);
            END;
          END; {WITH}
        END;
      EXCEPT {TRY}
        ON E : Exception DO
          Log('EG CalculateDesiredSpeed: ' + E.ClassName + ' error raised, with message: '+ E.Message);
      END; {TRY}
    END; { CalculateDesiredSpeed }

  CONST
    GoingForward = True;
    InchesPerMile = 63360;
    ScaleFactor = 76;
    ShowError = True;

  VAR
    DebugStr : String;
    I : Integer;
    MissingTrainFound : Boolean;
    MissingTrainStr : String;
    PossiblyMissing : Boolean;
    TC : Integer;
    TCFound : Boolean;
    TCPos : Integer;

  BEGIN { CalculateTrainSpeed }
    WITH T^ DO BEGIN
      IF Train_CurrentStatus <> ReadyForRemovalFromDiagrams THEN BEGIN
        { See if we've stalled - can only do so by timing gap between known sections }
        { Need to adjust for long sections **** }
        IF (Train_CurrentSpeedInMPH = Stop)
        OR (Train_SectionStartTime = 0)
        OR (Time <= IncSecond(Train_SectionStartTime, 10))
        THEN
          Train_StalledMsgWritten := False
        ELSE BEGIN
          IF Train_ExtraPowerAdjustment = 0 THEN BEGIN
            Log(Train_LocoChipStr + ' L Has loco stalled in TC=' + IntToStr(Train_CurrentTC) + '? Increasing speed by one speed step');
            Train_ExtraPowerAdjustment := 1;

            { Set acceleration to the minimum, to get an immediate increase in speed }
            Train_AccelerationTimeInSeconds := 0.0;
            { and restart the clock, in case we need to provide more power }
            Train_SectionStartTime := Time;

            { store data for replay }
            Log(Train_LocoChipStr + ' L TC=' + IntToStr(Train_CurrentTC) + ' StalledState 1');
          END ELSE
            IF Train_ExtraPowerAdjustment = 1 THEN BEGIN
              Log(Train_LocoChipStr + ' L Has loco stalled in TC=' + IntToStr(Train_CurrentTC) + '? Increasing speed by a second speed step');
              Train_ExtraPowerAdjustment := 2;

              { Set acceleration to the minimum, to get an immediate increase in speed }
              Train_AccelerationTimeInSeconds := 0.0;
              { and restart the clock, in case we need to provide even more power }
              Train_SectionStartTime := Time;

              { store data for replay }
              Log(Train_LocoChipStr + ' L TC=' + IntToStr(Train_CurrentTC) + ' StalledState 2');
            END ELSE
              IF Train_ExtraPowerAdjustment = 2 THEN BEGIN

                Log(Train_LocoChipStr + ' L Has loco stalled in TC=' + IntToStr(Train_CurrentTC) + '? Increasing speed by a third Lenz speed step');
                { Set acceleration to the minimum, to get an immediate increase in speed }
                Train_AccelerationTimeInSeconds := 0.0;
                Train_ExtraPowerAdjustment := 3;

                { store data for replay }
                Log(Train_LocoChipStr + ' L TC=' + IntToStr(Train_CurrentTC) + ' StalledState 3');
              END ELSE
                IF NOT Train_StalledMsgWritten THEN BEGIN
                  Debug('+Has ' + Train_LocoChipStr + ' stalled? Speed has now been increased by three Lenz speed steps');
                  Train_StalledMsgWritten := True;
                END;
        END;

        { If the train isn't going anywhere, don't process any movement - without this check, another train entering the next trackcircuit would be assumed by the system
          to be our loco.
        }
        IF Train_DesiredLenzSpeed <> 0 THEN BEGIN
          { Change the train's status if it has returned from missing }
          IF (Train_CurrentStatus = Missing)
          AND (Train_CurrentStatus = MissingAndSuspended)
          THEN
            ChangeTrainStatus(T, Train_PreviousStatus);

          IF Length(Train_TCsNotClearedArray) > 0 THEN BEGIN
            TC := ExtractTrackCircuitFromString(Train_TCsNotClearedArray[0]);

            { Finds the first of the list of train's sections to be occupied - has to be the next section expected, in case something else occupying it causes confusion }
            IF (TrackCircuits[TC].TC_OccupationState = TCFeedbackOccupation) THEN BEGIN
              Log(Train_LocoChipStr + ' T TC=' + IntToStr(TC) + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
                                    + ' occupation recorded');

              Train_PreviousTC := Train_CurrentTC;
              Train_CurrentTC := TC;
              TrackCircuits[Train_CurrentTC].TC_LocoChip := Train_LocoChip;

              { Store the headcode info so we can see who was there last }
              TrackCircuits[Train_CurrentTC].TC_SaveTrackCircuitHeadcode := TrackCircuits[Train_CurrentTC].TC_Headcode;
              { ...and add the new headcode data }
              TrackCircuits[TC].TC_Headcode := Train_Headcode;

              { ..and start the clock }
              Train_SectionStartTime := Time;

              TrackCircuits[Train_CurrentTC].TC_LocoStalled := False;
              TrackCircuits[Train_PreviousTC].TC_LocoStalled := False;
              { first remove it from the list of trackcircuits released, if it's there, in case we passed over it before and released it subsequently }
              TCPos := 0;
              TCFound := False;
              IF Length(Train_TCsReleasedArray) > 0 THEN BEGIN
                FOR I := 0 TO High(Train_TCsReleasedArray) DO BEGIN
                  IF TC = ExtractTrackCircuitFromString(Train_TCsReleasedArray[I]) THEN BEGIN
                    TCPos := I;
                    TCFound := True;
                  END;
                END;

                IF TCFound THEN BEGIN
                  IF TCPos < High(Train_TCsReleasedArray) THEN
                    { move everything else down }
                    FOR I := TCPos TO (High(Train_TCsReleasedArray) - 1) DO
                      Train_TCsReleasedArray[I] := Train_TCsReleasedArray[I + 1];
                  { and truncate the array }
                  SetLength(Train_TCsReleasedArray, Length(Train_TCsReleasedArray) - 1);
                END;
              END;

              { and remove it from the list of those sections not yet cleared, and add it to those list of those occupied or cleared }
              IF Length(Train_TCsNotClearedArray) > 0 THEN BEGIN
                { first add it to the list of those occupied or cleared }
                SetLength(Train_TCsOccupiedOrClearedArray, Length(Train_TCsOccupiedOrClearedArray) + 1);
                Train_TCsOccupiedOrClearedArray[High(Train_TCsOccupiedOrClearedArray)] := Train_TCsNotClearedArray[0];
                IF Length(Train_TCsNotClearedArray) = 1 THEN
                  { then empty the not cleared array }
                  SetLength(Train_TCsNotClearedArray, 0)
                ELSE BEGIN
                  { move the trackcircuits in the not cleared array down by one }
                  FOR I := 0 TO (High(Train_TCsNotClearedArray) - 1) DO
                    Train_TCsNotClearedArray[I] := Train_TCsNotClearedArray[I + 1];
                  SetLength(Train_TCsNotClearedArray, Length(Train_TCsNotClearedArray) - 1);
                  Train_NextTC := ExtractTrackCircuitFromString(Train_TCsNotClearedArray[0]);
                  IF Length(Train_TCsNotClearedArray) > 1 THEN
                    Train_NextButOneTC := ExtractTrackCircuitFromString(Train_TCsNotClearedArray[1])
                  ELSE
                    Train_NextButOneTC := UnknownTrackCircuit;
                END;
              END;
              IF Length(Train_TCsNotClearedArray) = 0 THEN
                Train_NextTC := UnknownTrackCircuit;

            END ELSE
              IF SystemOnline THEN BEGIN
                { Now see if we've strayed - if the trackcircuits we're supposed to be in are TCUnoccupied - it's maybe we're somewhere else by mistake }
                PossiblyMissing := True;

                IF Train_PreviousTC <> UnknownTrackCircuit THEN
                  IF TrackCircuits[Train_PreviousTC].TC_OccupationState = TCFeedbackOccupation THEN
                    PossiblyMissing := False;
                IF Train_CurrentTC <> UnknownTrackCircuit THEN
                  IF TrackCircuits[Train_CurrentTC].TC_OccupationState = TCFeedbackOccupation THEN
                    PossiblyMissing := False;
                IF Train_NextTC <> UnknownTrackCircuit THEN
                  IF TrackCircuits[Train_NextTC].TC_OccupationState = TCFeedbackOccupation THEN
                    PossiblyMissing := False;
                IF Train_InitialTrackCircuits[1] <> UnknownTrackCircuit THEN
                  IF TrackCircuits[Train_InitialTrackCircuits[1]].TC_OccupationState = TCFeedbackOccupation THEN
                    PossiblyMissing := False;
                IF Train_InitialTrackCircuits[2] <> UnknownTrackCircuit THEN
                  IF TrackCircuits[Train_InitialTrackCircuits[2]].TC_OccupationState = TCFeedbackOccupation THEN
                    PossiblyMissing := False;
                IF Train_InitialTrackCircuits[3] <> UnknownTrackCircuit THEN
                  IF TrackCircuits[Train_InitialTrackCircuits[3]].TC_OccupationState = TCFeedbackOccupation THEN
                    PossiblyMissing := False;
                IF Train_InitialTrackCircuits[4] <> UnknownTrackCircuit THEN
                  IF TrackCircuits[Train_InitialTrackCircuits[4]].TC_OccupationState = TCFeedbackOccupation THEN
                    PossiblyMissing := False;

                IF PossiblyMissing THEN BEGIN
                  { now probably missing! }
                  IF NOT TrackCircuits[Train_CurrentTC].TC_MissingTrainNoted THEN BEGIN
                    IF Train_LastMissingTC = Train_CurrentTC THEN
                      Log(Train_LocoChipStr + ' L TC=' + IntToStr(Train_CurrentTC) + ' (Train_CurrentTC): train missing but user not alerted'
                                            + ' as previous alert for the same loco and same track circuit was acknowledged')
                    ELSE BEGIN
                      IF Train_CurrentStatus = Suspended THEN
                        ChangeTrainStatus(T, MissingAndSuspended)
                      ELSE
                        IF Train_CurrentStatus <> Missing THEN
                          ChangeTrainStatus(T, Missing);

                      SetTrackCircuitState(Train_LocoChip, Train_CurrentTC, TCMissingOccupation);
                      Log(Train_LocoChipStr + ' L TC=' + IntToStr(Train_CurrentTC) + ' (Train_CurrentTC): train missing - user alerted');
                    END;

                    TrackCircuits[Train_CurrentTC].TC_MissingTrainNoted := True;
                  END;
                END;
              END;
          END;
        END;

        IF (Train_CurrentStatus = Missing) OR (Train_CurrentStatus = MissingAndSuspended) THEN BEGIN
          Train_DesiredLenzSpeed := 0;
          Train_DesiredSpeedInMPH := MPH0;
          T^.Train_AccelerationTimeInSeconds := 0;

          IF NOT Train_MissingMessage THEN BEGIN
            DrawDiagramsStatusCell(T, GreyedOutStyle);
            MissingTrainStr := 'missing: '
                               + 'Previous: TC=' + IntToStr(Train_PreviousTC)
                               + ', Current: TC=' + IntToStr(Train_CurrentTC)
                               + ', and Next: TC=' + IntToStr(Train_NextTC)
                               + ' not occupied';
            IF Train_NextButOneTC <> UnknownTrackCircuit THEN
              MissingTrainStr := MissingTrainstr + ' but next but one TC=' + IntToStr(Train_NextButOneTC) + ' is occupied';

            IF MissingTrainStr <> SaveMissingTrainStr THEN
              Log(Train_LocoChipStr + ' T ' + MissingTrainStr);

            Log(Train_LocoChipStr + ' L Spd=0' + ' [0 mph]' + ' TC=' + IntToStr(Train_CurrentTC));
            SaveMissingTrainStr := MissingTrainStr;

            { Arrange for it to appear again when the user is ready - perhaps by having redirected it to the correct track, or by making sure that power is reaching the
              chip
            }
            IF MissingTrainCounter = 9 THEN
              Log('XG Too many trains are missing')
            ELSE BEGIN
              Inc(MissingTrainCounter);
              { See which element is ready to marked }
              I := 1;
              MissingTrainFound := False;
              WHILE (I <= 9)
              AND NOT MissingTrainFound
              DO BEGIN
                IF MissingTrainArray[I] = False THEN BEGIN
                  MissingTrainFound := True;
                  MissingTrainArray[I] := True;
                  Train_MissingNum := I;
                END;
                Inc(I);
              END; {WHILE}
            END;
            Debug('+Loco ' + Train_LocoChipStr +' is missing - press ' + IntToStr(Train_MissingNum) + ' to reinstate it');
            Train_MissingMessage := True;
            TrackCircuits[Train_CurrentTC].TC_MissingTrainNoted := False;
          END;
        END ELSE BEGIN
          { Not a wrong move - now set the speed depending on where we are }
          CalculateDesiredSpeed(T);

          IF Train_CurrentLenzSpeed = 0 THEN
            { as we're starting from scratch, set the timer to catch stalls }
            Train_SectionStartTime := Time;

          IF Train_DesiredLenzSpeed = 0 THEN
            Train_SectionStartTime := 0;
        END;

        { Now some debugging stuff }
        DebugStr := '';
        FOR I := 0 TO High(Train_TCsNotClearedArray) DO
          DebugStr := DebugStr + ' ' + Train_TCsNotClearedArray[I];
        IF (DebugStr <> '')
        AND (DebugStr <> Train_TCsNotClearedStr)
        THEN BEGIN
          Log(Train_LocoChipStr + ' T TCnc:');
          WriteStringArrayToLog(Train_LocoChip, 'T', Train_TCsNotClearedArray, 2, 190);
          Train_TCsNotClearedStr := DebugStr;
        END;

        DebugStr := '';
        FOR I := 0 TO High(Train_TCsAndSignalsNotClearedArray) DO
          DebugStr := DebugStr + ' ' + Train_TCsAndSignalsNotClearedArray[I];
        IF (DebugStr <> '')
        AND (DebugStr <> Train_TCsAndSignalsNotClearedStr)
        THEN BEGIN
          Log(Train_LocoChipStr + ' T TC&SGnc:');
          WriteStringArrayToLog(Train_LocoChip, 'T', Train_TCsAndSignalsNotClearedArray, 2, 190, 'SR=');
          Train_TCsAndSignalsNotClearedStr := DebugStr;
        END;

        DebugStr := '';
        FOR I := 0 TO High(Train_TCsOccupiedOrClearedArray) DO
          DebugStr := DebugStr + ' TC=' + Train_TCsOccupiedOrClearedArray[I];
        IF (DebugStr <> '')
        AND (DebugStr <> Train_TCsOccupiedOrClearedStr)
        THEN BEGIN
          Log(Train_LocoChipStr + ' T TCo/c:');
          WriteStringArrayToLog(Train_LocoChip, 'T', Train_TCsOccupiedOrClearedArray, 2, 190);
          Train_TCsOccupiedOrClearedStr := DebugStr;
        END;

        DebugStr := '';
        FOR I := 0 TO High(Train_TCsReleasedArray) DO
          DebugStr := DebugStr + Train_TCsReleasedArray[I];
        IF (DebugStr <> '')
        AND (DebugStr <> Train_TCsReleasedStr)
        THEN BEGIN
          Log(Train_LocoChipStr + ' T TCr:');
          WriteStringArrayToLog(Train_LocoChip, 'T', Train_TCsReleasedArray, 2, 190);
          Train_TCsReleasedStr := DebugStr;
        END;
      END;
    END; {WITH}
  END; { CalculateTrainSpeed }

  PROCEDURE MoveTrain(T : Train);
  { Move a train along the track }
  CONST
    ForceDraw = True;
    Indent = True;

  VAR
    TempDirectionStr : String;
    TempUserRequiresInstructionMsg : String;

  BEGIN
    WITH T^ DO BEGIN
      IF (Train_CurrentStatus <> Suspended)
      AND (Train_CurrentStatus <> MissingAndSuspended) AND (Train_CurrentStatus <> Cancelled)
      THEN BEGIN
        { see which trains are where - find out speeds required }
        CalculateTrainSpeed(T);
        { See if loco taken over by an user - could be requested in the timetable too }
        IF SystemOnline THEN BEGIN
          IF NOT Train_UserDriving THEN
            SetDesiredTrainSpeed(T)
          ELSE BEGIN
            IF Train_UserRequiresInstructions THEN BEGIN
              TempUserRequiresInstructionMsg := IntToStr(Train_DesiredLenzSpeed);
              TempDirectionStr := DirectionToStr(Train_CurrentDirection, ShortStringType);
              IF TempDirectionStr <> SaveTempDirectionStr THEN BEGIN
                TempUserRequiresInstructionMsg := TempUserRequiresInstructionMsg + ' ' + TempDirectionStr;
                SaveTempDirectionStr := TempDirectionStr;
              END;

              IF Train_UserRequiresInstructionMsg <> TempUserRequiresInstructionMsg THEN BEGIN
                Log(Train_LocoChipStr + ' L= User instructed to set loco ' + Train_LocoChipStr + '''s speed to ' + TempUserRequiresInstructionMsg);
                Train_UserRequiresInstructionMsg := TempUserRequiresInstructionMsg;
              END;
            END;
            IF RDCMode
            AND RailDriverInitialised
            AND RailDriverCalibrated
            AND Train_ControlledByRDC
            THEN BEGIN
              Train_DesiredSpeedInMPH := Stop;
              Train_DesiredLenzSpeed := 0;
              SetSpeedByRailDriverConsole(T);
            END;
          END;
        END;
      END;
    END; {WITH}
  END; { MoveTrain }

VAR
  T : Train;

BEGIN
  IF NOT InAutoMode THEN BEGIN
    IF NOT AutoModeMsgWritten THEN BEGIN
      Log('EG MoveAllTrains called but not in auto mode');
      AutoModeMsgWritten := True;
    END;
  END ELSE BEGIN
    AutoModeMsgWritten := False;

    T := TrainList;
    WHILE T <> NIL DO BEGIN
      WITH T^ DO BEGIN
        IF (Train_LocoChip <> UnknownLocoChip)
        AND Train_DiagramFound
        AND (Train_CurrentStatus <> Suspended)
        AND (Train_CurrentStatus <> MissingAndSuspended)
        AND (Train_CurrentStatus <> Cancelled)
        THEN BEGIN
          IF (Train_CurrentStatus = ReadyToDepart)
          OR (Train_CurrentStatus = Departed)
          OR (Train_CurrentStatus = RouteCompleted)
          OR (Train_CurrentStatus = RouteingWhileDeparted)
          THEN
            MoveTrain(T);
        END;
      END; {WITH}
      T := T^.Train_NextRecord;
    END; {WHILE}
    PruneTrainList;

    { also check that the trains that are stationary are still occupying the sections they should be in }
    { CheckStationaryTrains; **** }
  END;
END; { MoveAllTrains }

FUNCTION TrainIsInPlace(T : Train) : Boolean;
{ Returns true if a given train exists  - not in use 9/4/14 **** }
VAR
  DebugStr : String;
  InitialTrackCircuitCount : Integer;

BEGIN
  WITH T^ DO BEGIN
    Result := False;
    IF SystemOnline THEN BEGIN
      { see if there's something in the trackcircuit we intend starting from - (can't tell what's there, unfortunately) - third, fourth and fifth initial ones may not
        exist so need to test that first.
      }
      FOR InitialTrackCircuitCount := 1 TO 5 DO BEGIN
        IF Train_InitialTrackCircuits[InitialTrackCircuitCount] <> UnknownTrackCircuit THEN
          IF TrackCircuits[Train_InitialTrackCircuits[InitialTrackCircuitCount]].TC_OccupationState = TCFeedbackOccupation THEN
            Result := True;
      END; {FOR}

      IF Result = False THEN BEGIN
        IF NOT Train_NotInPlaceMsgWritten THEN BEGIN
          Debug('!' + Train_LocoChipStr + ' not found in TC=' + IntToStr(Train_InitialTrackCircuits[1]));

          DebugStr := 'Train not found in TC=' + IntToStr(Train_InitialTrackCircuits[1]);
          FOR InitialTrackCircuitCount := 2 TO 5 DO
            IF Train_InitialTrackCircuits[InitialTrackCircuitCount] <> UnknownTrackCircuit THEN
              DebugStr := DebugStr + ' or TC=' + IntToStr(Train_InitialTrackCircuits[InitialTrackCircuitCount]);
          DebugStr := DebugStr + ' so not activating it yet';
          Log(Train_LocoChipStr + ' L ' + DebugStr);
          Train_NotInPlaceMsgWritten := True;
        END;
      END;
    END;
  END; {WITH}
END; { TrainIsInPlace }

PROCEDURE CheckTrainsReadyToDepart;
{ Runs through the list of trains, and sees if any are ready to depart. It sets directional lights the right way round, 90 seconds before departure. }
CONST
  Delayed = True;
  LightsOn = True;
  TimetableLoading = True;

VAR
  T : Train;

BEGIN
  T := TrainList;
  WHILE T <> NIL DO BEGIN
    WITH T^ DO BEGIN
      IF (Train_LocoChip <> UnknownLocoChip)
      AND Train_DiagramFound
      AND (Train_CurrentStatus <> Cancelled)
      AND (Train_CurrentStatus <> UnknownTrainStatus)
      AND (Train_CurrentStatus <> Departed)
      AND (Train_CurrentStatus <> NonMoving)
      AND (Train_CurrentStatus <> WaitingForRemovalFromDiagrams)
      AND (Train_CurrentStatus <> ReadyForRemovalFromDiagrams)
      AND (Train_CurrentStatus <> RemovedFromDiagrams)
      THEN BEGIN
        WITH Train_JourneysArray[Train_CurrentJourney] DO BEGIN
          IF TrainJourney_ActualDepartureTime = 0 THEN BEGIN
            IF Train_CurrentStatus = WaitingForSignalHiddenAspectToClear THEN BEGIN
              IF (CurrentRailwayTime > IncMinute(Train_WaitingForHiddenAspectStartTime, StationSameDirectionExitMinimumWaitTimeInMinutes))
              AND (CurrentRailwayTime > IncMinute(TrainJourney_CurrentDepartureTime, -1))
              THEN BEGIN
                ClearHiddenAspectSignals(T, Train_AtHiddenAspectSignal);

                { Now change the train's status }
                IF (Train_CurrentJourney < Train_TotalJourneys)
                AND NOT Train_JourneysArray[Train_CurrentJourney + 1].TrainJourney_Created
                THEN
                  ChangeTrainStatus(T, ReadyForRouteing)
                ELSE
                  ChangeTrainStatus(T, ReadyToDepart);
              END;
            END ELSE
              IF Train_LightsOnTime <> 0 THEN BEGIN
                IF Train_LightsOnTime <= CurrentRailwayTime THEN BEGIN
                  ChangeTrainStatus(T, InLightsOnTime);
                  AddLightsToLightsToBeSwitchedOnArray(T, Train_CurrentDirection, Train_CurrentDirection, 0, 0, Train_LightsOnTime);
                END;
              END ELSE
                IF (CurrentRailwayTime > IncSecond(TrainJourney_CurrentDepartureTime, -90))
                AND ((Train_CurrentStatus = ReadyForCreation) OR (Train_CurrentStatus = WaitingForLightsOn))
                THEN BEGIN
                  ChangeTrainStatus(T, WaitingForRouteing);
                  IF Train_LightsType <> NoLights THEN BEGIN
                    { If we're changing ends, allow time for the driver to walk from one end of the train to the other }
                    IF (DirectionWillChangeAfterGivenJourney(T, Train_CurrentJourney))
                    AND (Train_CurrentLengthInInches > 36) { at least four carriage lengths to walk so increase the time }
                    THEN
                      { the direction is changing after the previous journey }
                      AddLightsToLightsToBeSwitchedOnArray(T, Train_CurrentDirection, Train_CurrentDirection, -30, -10, TrainJourney_CurrentDepartureTime)
                    ELSE
                      AddLightsToLightsToBeSwitchedOnArray(T, Train_CurrentDirection, Train_CurrentDirection, -90, -60, TrainJourney_CurrentDepartureTime);
                  END;
                END;

            IF Train_HasCablights
            AND NOT CabLightsAreOn(Train_LocoChip)
            AND NOT Train_CabLightsHaveBeenOn
            THEN BEGIN
              IF ((Train_LightsOnTime <> 0)
                  AND (IncSecond(Train_LightsOnTime, -60) <= CurrentRailwayTime))
              OR (CurrentRailwayTime > IncSecond(TrainJourney_CurrentDepartureTime, -180))
              THEN BEGIN
                TurnCabLightsOn(Train_LocoChip);
                Train_CabLightsHaveBeenOn := True;
              END;
            END;

            IF (CurrentRailwayTime > IncSecond(TrainJourney_CurrentDepartureTime, -60))
            AND ((Train_CurrentStatus = InLightsOnTime)
                 OR (Train_CurrentStatus = WaitingForRouteing))
            THEN
              ChangeTrainStatus(T, ReadyForRouteing);

            IF CurrentRailwayTime > Train_JourneysArray[Train_CurrentJourney].TrainJourney_CurrentDepartureTime THEN BEGIN
              IF ((Train_CurrentJourney > 0)
                  AND NOT Train_JourneysArray[Train_CurrentJourney].TrainJourney_StoppingOnArrival)
              OR (CurrentRailwayTime >= TrainJourney_CurrentDepartureTime)
              THEN BEGIN
                IF Train_CurrentStatus = CommencedRouteing THEN BEGIN
                  Log(Train_LocoChipStr + ' T Current railway time (' + TimeToHMSStr(CurrentRailwayTime)
                                        + ') >= TrainJourney_CurrentDepartureTime ('
                                        + TimeToHMSStr(TrainJourney_CurrentDepartureTime) + ')');
                  ChangeTrainStatus(T, ReadyToDepart);
                END;
              END;
            END;

            IF CurrentRailwayTime > IncMinute(Train_JourneysArray[Train_CurrentJourney].TrainJourney_CurrentDepartureTime, 1) THEN BEGIN
              { If we're held up before the first departure, we may need to alter the timetable - do the test once a minute }
              Log(Train_LocoChipStr + ' T J=' + IntToStr(Train_CurrentJourney)
                                    + ': departure delayed: expected departure time of '
                                    + TimeToHMSStr(Train_JourneysArray[Train_CurrentJourney].TrainJourney_CurrentDepartureTime)
                                    + ' is now ' + TimeToHMSStr(CurrentRailwayTime));

              RecalculateJourneyTimes(T, 'as departure delayed from ' + TimeToHMSStr(Train_JourneysArray[Train_CurrentJourney].TrainJourney_CurrentDepartureTime)
                                         + ' to ' + TimeToHMSStr(CurrentRailwayTime));

              Train_JourneysArray[Train_CurrentJourney].TrainJourney_CurrentDepartureTime := CurrentRailwayTime;
            END;
          END;
        END;
      END; {WITH}
    END; {WITH}
    T := T^.Train_NextRecord;
  END; {WHILE}
END; { CheckTrainsReadyToDepart }

PROCEDURE CheckTrainsHaveDeparted;
{ Runs through the list of trains, and sees if any have departed at the start of a route }
CONST
  Delayed = True;
  TimetableLoading = True;

VAR
  OK : Boolean;
  T : Train;

BEGIN
  TRY
    T := TrainList;
    WHILE T <> NIL DO BEGIN
      WITH T^ DO BEGIN
        IF (Train_LocoChip <> UnknownLocoChip)
        AND Train_DiagramFound
        AND (Train_CurrentStatus <> Suspended)
        AND (Train_CurrentStatus <> MissingAndSuspended)
        AND (Train_CurrentStatus <> Cancelled)
        AND (Train_CurrentStatus <> NonMoving)
        THEN BEGIN
          WITH Train_JourneysArray[Train_CurrentJourney] DO BEGIN
            IF Train_CurrentStatus = ReadyToDepart THEN BEGIN
              IF TrainJourney_ActualDepartureTime = 0 THEN BEGIN
                IF CurrentRailwayTime > IncMinute(TrainJourney_CurrentDepartureTime, 1) THEN BEGIN
                  { If we're held up before departure, we may need to alter the timetable - do the test once a minute }
                  Log(Train_LocoChipStr + ' T J=' + IntToStr(Train_CurrentJourney)
                                        + ': departure delayed: expected departure time of ' + TimeToHMSStr(TrainJourney_CurrentDepartureTime)
                                        + ' is now ' + TimeToHMSStr(CurrentRailwayTime));

                  TrainJourney_CurrentDepartureTime := CurrentRailwayTime;
                  RecalculateJourneyTimes(T, 'as departure delayed: expected departure time of ' + TimeToHMSStr(TrainJourney_CurrentDepartureTime)
                                             + ' is now ' + TimeToHMSStr(CurrentRailwayTime));
                END;
              END;

              IF Train_CurrentLenzSpeed <> 0 THEN BEGIN
                ChangeTrainStatus(T, Departed);
                TrainJourney_ActualDepartureTime := CurrentRailwayTime;
                RecalculateJourneyTimes(T, 'following departure from ' + LocationToStr(Train_JourneysArray[Train_CurrentJourney].TrainJourney_StartLocation));

                Log(Train_LocoChipStr + ' T J=' + IntToStr(Train_CurrentJourney) + ' R=' + IntToStr(Train_CurrentRoute)
                                          + ' has departed from ' + LocationToStr(TrainJourney_StartLocation));
                DrawDiagrams(UnitRef, 'CheckTrainsHaveDeparted');

                { We must also update the LocationOccupations array if we've departed }
                SetUpTrainLocationOccupationsAbInitio(T, OK);

                { If we're offline, then we will want to clear the initial trackcircuits automatically, as there is no moving train to clear them }
                IF SystemSetOfflineByCommandLineParameter THEN BEGIN
                  IF Train_InitialTrackCircuits[1] <> UnknownTrackCircuit THEN BEGIN
                    SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[1], TCUnoccupied);
                    SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[2], TCUnoccupied);
                    IF Train_InitialTrackCircuits[3] <> UnknownTrackCircuit THEN
                      SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[3], TCUnoccupied);
                    IF Train_InitialTrackCircuits[4] <> UnknownTrackCircuit THEN
                      SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[4], TCUnoccupied);
                    IF Train_InitialTrackCircuits[5] <> UnknownTrackCircuit THEN
                      SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[5], TCUnoccupied);
                  END;
                END;
              END;
            END;
          END; {WITH}
        END;
      END; {WITH}
      T := T^.Train_NextRecord;
    END; {WHILE}
  EXCEPT
    ON E : Exception DO
      Log('EG CheckTrainsHaveDeparted:' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CheckTrainsHaveDeparted }

PROCEDURE CheckTrainsHaveArrived;
{ Runs through the list of trains, and sees if any have arrived in the route's final trackcircuit and have stopped; also sees whether to increment the journey number }
CONST
  AppendToFile = True;
  Delayed = True;
  LightsOn = True;
  TimetableLoading = True;

VAR
  DebugStr : String;
  HiddenAspectSignal : Integer;
  I : Integer;
  OccupiedOrClearedTC : Integer;
  OK : Boolean;
  T : Train;
  TCPos : Integer;

BEGIN
  T := TrainList;
  WHILE T <> NIL DO BEGIN
    WITH T^ DO BEGIN
      IF (Train_LocoChip <> UnknownLocoChip)
      AND Train_DiagramFound
      AND (Train_CurrentStatus <> Suspended)
      AND (Train_CurrentStatus <> MissingAndSuspended)
      AND (Train_CurrentStatus <> Cancelled)
      THEN BEGIN
        IF Train_CurrentStatus = WaitingForRemovalFromDiagrams THEN BEGIN
          { wait for one minute after train arrives from its final journey before purging it - this keeps it in the station monitors in time to say "arrived" }
          IF CurrentRailwayTime > IncMinute(Train_JourneysArray[Train_TotalJourneys].TrainJourney_ActualArrivalTime, 1) THEN BEGIN
            ChangeTrainStatus(T, ReadyForRemovalFromDiagrams);
            IF Train_HasCablights
            AND CabLightsAreOn(Train_LocoChip)
            THEN
              TurnCabLightsOff(Train_LocoChip);
            Log(Train_LocoChipStr + ' T Purging can commence as arrival time was one minute ago for'
                                  + ' J=' + IntToStr((Train_CurrentJourney) - 1) + ' R=' + IntToStr(Train_CurrentRoute));

            IF ((Length(Train_TCsNotClearedArray) = 0)
            AND (Train_CurrentSpeedInMPH = Stop) AND AllJourneysComplete(T))
            THEN BEGIN
              DebugStr := 'now inactive:';
              DebugStr := DebugStr + ' Set=';
              DebugStr := DebugStr + ' Cleared=';
              DebugStr := DebugStr + ' Speed=' + MPHTOStr(Train_CurrentSpeedInMPH) + ' mph';
              DebugStr := DebugStr + ' ToBeSet=';
              Log(Train_LocoChipStr + ' L ' + DebugStr);
            END;
          END;
        END ELSE BEGIN
          IF (Train_CurrentSpeedInMPH = Stop)
          AND (Train_CurrentStatus = RouteCompleted)
          THEN BEGIN
            { the route's been deleted, so we must have arrived }
            IF AllJourneysComplete(T) THEN BEGIN
              { We can't clear the train's remaining system-occupied-trackcircuits here, as at this time they may still be marked as having feedback occupation (they take
                some time to clear)
              }
              { See if there's additional loco chips - if so, switch direction on them so that both lights switch to red }
              IF Train_LightsType = LightsOperatedByTwoChips THEN
                AddLightsToLightsToBeSwitchedOnArray(T, Down, Up, 0, 5, CurrentRailwayTime);

              IF Train_HasCablights
              AND NOT CabLightsAreOn(Train_LocoChip)
              THEN BEGIN
                TurnCabLightsOn(Train_LocoChip);
                Train_CabLightsHaveBeenOn := True;
              END;

              ChangeTrainStatus(T, WaitingForRemovalFromDiagrams)
            END ELSE BEGIN
              { Reset all the trackcircuit occupation arrays... }
              Log(Train_LocoChipStr + ' T J=' + IntToStr(Train_CurrentJourney) + ': clearing track occupation arrays');
              SetLength(Train_TCsAndSignalsNotClearedArray, 0);
              SetLength(Train_TCsNotClearedArray, 0);
              { ...except for the Train_TCsOccupiedOrClearedArray, only delete those elements which have already been released }
              I := 0;
              WHILE I <= High(Train_TCsOccupiedOrClearedArray) DO BEGIN
                OccupiedOrClearedTC := ExtractTrackCircuitFromString(Train_TCsOccupiedOrClearedArray[I]);

                IF IsTrackCircuitInStringArray(Train_TCsReleasedArray, OccupiedOrClearedTC, TCPos) THEN
                  DeleteElementFromStringArray(Train_TCsOccupiedOrClearedArray, I)
                ELSE BEGIN
                  Log(Train_LocoChipStr + ' T ' + Train_TCsOccupiedOrClearedArray[I] + ' not deleted from Train_TCsOccupiedOrClearedArray as it has not been released');
                  Inc(I);
                END;
              END;

              { Note what's left in the Train_TCsOccupiedOrClearedArray }
              DebugStr := '';
              FOR I := 0 TO High(Train_TCsOccupiedOrClearedArray) DO
                DebugStr := DebugStr + ' TC=' + Train_TCsOccupiedOrClearedArray[I];
              Log(Train_LocoChipStr + ' T TCo/c:');
              WriteStringArrayToLog(Train_LocoChip, 'L', Train_TCsOccupiedOrClearedArray, 2, 190);
              Train_TCsOccupiedOrClearedStr := DebugStr;

              SetLength(Train_TCsReleasedArray, 0);

              IF (Train_AtHiddenAspectSignal = UnknownSignal)
              OR (Signals[Train_JourneysArray[Train_CurrentJourney].TrainJourney_EndSignal].Signal_HiddenAspect <> RedAspect)
              THEN
                ChangeTrainStatus(T, WaitingForLightsOn)
              ELSE BEGIN
                { we're at a hidden aspect }
                ChangeTrainStatus(T, WaitingForSignalHiddenAspectToClear);
                Train_WaitingForHiddenAspectStartTime := CurrentRailwayTime;
                HiddenAspectSignal := Train_JourneysArray[Train_CurrentJourney].TrainJourney_EndSignal;
                Log(Train_LocoChipStr + ' R Waiting for S=' + IntToStr(HiddenAspectSignal) + ' hidden aspect to clear');
              END;
            END;

            Log(Train_LocoChipStr + ' T Train has arrived at the end of' + ' J=' + IntToStr(Train_CurrentJourney) + ' R=' + IntToStr(Train_CurrentRoute));

            { Insert the correct arrival time }
            Train_JourneysArray[Train_CurrentJourney].TrainJourney_ActualArrivalTime := CurrentRailwayTime;
            Log(Train_LocoChipStr + ' T J=' + IntToStr(Train_CurrentJourney) + ': '
                                  + ' arrival time amended to ' + TimeToHMSStr(CurrentRailwayTime));

            RecalculateJourneyTimes(T, 'following arrival at ' + LocationToStr(Train_JourneysArray[Train_CurrentJourney].TrainJourney_EndLocation));
            DrawDiagrams(UnitRef, 'CheckTrainsHaveArrived');

            { We must also update the LocationOccupations array if we've arrived }
            SetUpTrainLocationOccupationsAbInitio(T, OK);

            { If there's another journey coming up, note the fact and adjust other details }
            IF NOT AllJourneysComplete(T) THEN BEGIN
              IF Train_CurrentJourney < Train_TotalJourneys THEN BEGIN
                { More journeys coming up - if the train is changing direction, do it now }
                IF Train_JourneysArray[Train_CurrentJourney + 1].TrainJourney_Direction <> Train_JourneysArray[Train_CurrentJourney].TrainJourney_Direction
                THEN BEGIN
                  Log(Train_LocoChipStr + ' R Train_CurrentJourney=' + DirectionToStr(Train_JourneysArray[Train_CurrentJourney + 1].TrainJourney_Direction)
                                        + ' <> previous Train_CurrentJourney =' + DirectionToStr(Train_JourneysArray[Train_CurrentJourney].TrainJourney_Direction));
                  Log(Train_LocoChipStr + ' R Train_CurrentDirection was ' + DirectionToStr(Train_CurrentDirection));
                  Train_CurrentDirection := Train_JourneysArray[Train_CurrentJourney + 1].TrainJourney_Direction;
                  Log(Train_LocoChipStr + ' R Train_CurrentDirection is now ' + DirectionToStr(Train_CurrentDirection));
                  Train_CurrentSignal := UnknownSignal;
                  Log(Train_LocoChipStr + ' L Resetting Train_CurrentSignal');

                  { See if there's additional loco chips - if so, switch direction on them so both lights switch to red - not necessarily immediately, though that sometimes
                    happens in real life
                  }
                  IF Train_LightsType = LightsOperatedByTwoChips THEN
                    AddLightsToLightsToBeSwitchedOnArray(T, Down, Up, 0, 5, CurrentRailwayTime);
                END;

                { Finally increment the journey counter - this is done in between stops by the ClearARoute subroutine }
                Inc(Train_CurrentJourney);
                Log('X Train_CurrentJourney incremented to ' + IntToStr(Train_CurrentJourney) + ' in CheckTrainsHaveArrived');
                DrawDiagrams(UnitRef, 'CheckTrainsHaveArrived');
              END;
            END;
          END;
        END;
      END;
    END; {WITH}
    T := T^.Train_NextRecord;
  END; {WHILE}
END; { CheckTrainsHaveArrived }

PROCEDURE InitialiseMovementUnit;
{ Initialises the unit }
BEGIN
  MovementWindow.Height := MovementWindowHeight;
  MovementWindow.Width := MovementWindowWidth;
  MovementWindow.Top := MovementWindowTop;
  MovementWindow.Left := MovementWindowLeft;
END; { InitialiseMovementUnit }

PROCEDURE TMovementWindow.SignalLocationsToMonitorCancelButtonClick(Sender: TObject);
BEGIN
  MovementWindow.Close;
END; { SignalLocationsToMonitorCancelButtonClick }

PROCEDURE TMovementWindow.SignalLocationsToMonitorOKButtonClick(Sender: TObject);
BEGIN
  ProcessSignalLocationsToMonitorCheckListBoxChecks;
  MovementWindow.Close;
END; { SignalLocationsToMonitorOKButtonClick }

INITIALIZATION

END { Movement }.
