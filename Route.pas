UNIT Route;
{ Does the routeing

  FWP 28/12/98 First written
      08/10/00 Renamed Path from Auto
      26/12/00 Routeing moved to separate unit
      20/02/05 Renamed SubRoute from Path
      06/04/05 Renamed Route from SubRoute
}
INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, RailDraw, GetTime, Lenz, InitVars, CreateRoute, StdCtrls;

TYPE
  TRouteWindow = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE ClearARoute(Route : Integer);
{ Use a string consisting of pairs of signal/point name then '/' or '-' etc. to clear the route }

PROCEDURE ClearARouteByBruteForce(R : Integer);
{ Clear a route by brute force - needed sometimes if the normal route clearing fails }

PROCEDURE DescribeRouteingStatus;
{ Write to the log the current state of each route and subroute }

PROCEDURE ProcessApproachLockedSignals(Route : Integer);
{ Set up the signals we were unable to do because of approach control being in operation }

PROCEDURE ReleaseSubRoutes;
{ If train has cleared last (or only) track circuit in a subroute, subroute can be released }

PROCEDURE SetUpASubRoute(Route : Integer);
{ Set up a subroute using the supplied route array }

VAR
  RouteWindow: TRouteWindow; { not in use }

IMPLEMENTATION

{$R *.dfm}

USES Locks, Startup, Diagrams, Movement, Input, MiscUtils, LocoUtils, IDGlobal, DateUtils, StrUtils, LocationData, Options, Main;

CONST
  UnitRef = 'Route';

VAR
  SaveRouteSettingArrayPos : Integer = 0;
  SaveSubRouteSettingArrayPos : Integer = 0;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE WriteRouteInfoToLog(LocoChipStr : String; TypeOfLogChar : Char; Str1, Str2 : String);
{ checks for duplicate setting up/clearing messages, then calls Log }
CONST
  NoUnitRef = '';

BEGIN
  IF Str1 <> SaveRouteInfoStr THEN BEGIN
    Log(LocoChipStr + ' ' + TypeOfLogChar + ' ' + Str1);
    SaveRouteInfoStr := Str1;
  END;
  Log(TypeOfLogChar + ' ' +  StringOfChar(' ', 2) + Str2 + ' {NOUNITREF}');
END; { WriteRouteInfoToLog }

PROCEDURE ProcessApproachLockedSignals(Route : Integer);
{ Set up the signals we were unable to do because of approach control being in operation. This shouldn't need any checking as that has already been done. }
VAR
  ActionCh : String;
  AdjacentTCOccupied : Boolean;
  ApproachControlAspect : AspectType;
  Device : Integer;
  ItemToCheck : String;
  JunctionIndicatorState : IndicatorStateType;
  L : Integer;
  LocoChip : Integer;
  LocoChipStr : String;
  NewAspect : AspectType;
  OK : Boolean;
  PreviousSignalApproachControlAspect : AspectType;
  PreviousSignalAdjacentTCOccupied : Boolean;
  PreviousSignalApproachLocked : Boolean;
  RouteIndicatorToBeSet : Boolean;
  S : Integer;
  StopProcessingSignals : Boolean;
  TheatreDestinationToConvert : String;

BEGIN
  LocoChip := Routes_LocoChips[Route];
  LocoChipStr := LocoChipToStr(LocoChip);
  JunctionIndicatorState := NoIndicatorLit;
  StopProcessingSignals := False;

  { Run through the list of signals waiting to be set - if there's one that stops the setting, then exit }
  WHILE (Length(Routes_ApproachControlSignalsWaitingToBeSet[Route]) > 0) AND NOT StopProcessingSignals DO BEGIN
    OK := False;
    ActionCh := '';
    L := UnknownLine;
    PreviousSignalApproachControlAspect := NoAspect;
    PreviousSignalAdjacentTCOccupied := False;
    PreviousSignalApproachLocked := False;

    ItemToCheck := Routes_ApproachControlSignalsWaitingToBeSet[Route, 0];

    { Only release an approach-locked signal if the conditions which appertain to the locking are satisfied; release all non-approach-controlled signals that follow. (This
      solves the problem of how to deal with more than one approach-controlled signal in a route).
    }
    S := ExtractSignalFromString(ItemToCheck);
    AdjacentTCOccupied := (TrackCircuits[Signals[S].Signal_AdjacentTC].TC_OccupationState = TCFeedbackOccupation);
    RouteIndicatorToBeSet := (Pos('|', ItemToCheck) <> 0);
    ApproachControlAspect := Signals[S].Signal_ApproachControlAspect;

    FindPreviousSignals(S, Signals[S].Signal_PreviousSignal1, Signals[S].Signal_PreviousSignal2);
    IF Signals[S].Signal_PreviousSignal1 <> UnknownSignal THEN BEGIN
      PreviousSignalApproachControlAspect := Signals[Signals[S].Signal_PreviousSignal1].Signal_ApproachControlAspect;
      PreviousSignalAdjacentTCOccupied := (TrackCircuits[Signals[Signals[S].Signal_PreviousSignal1].Signal_AdjacentTC].TC_OccupationState = TCFeedbackOccupation);
      PreviousSignalApproachLocked := Signals[Signals[S].Signal_PreviousSignal1].Signal_ApproachLocked;
    END;

    IF (ApproachControlAspect = NoAspect) AND (PreviousSignalApproachControlAspect <> SingleYellowAspect) THEN BEGIN
      OK := True;
      { and reset the lock }
      Signals[S].Signal_ApproachLocked := False;
    END;

    IF (ApproachControlAspect = RedAspect) AND (PreviousSignalApproachControlAspect = SingleYellowAspect) THEN BEGIN
      IF NOT PreviousSignalApproachLocked THEN BEGIN
        IF NOT RouteIndicatorToBeSet OR AdjacentTCOccupied THEN BEGIN
          OK := True;
          { and reset the previous signal's lock }
          PreviousSignalApproachLocked := False;
          Signals[Signals[S].Signal_PreviousSignal1].Signal_ApproachLocked := False;
          { and this one's too }
          Signals[S].Signal_ApproachLocked := False;
        END;
      END;
    END;

    IF (ApproachControlAspect = RedAspect) AND (PreviousSignalApproachControlAspect = NoAspect) THEN BEGIN
      IF NOT RouteIndicatorToBeSet OR AdjacentTCOccupied THEN BEGIN
        OK := True;
        Signals[S].Signal_ApproachLocked := False;
      END;
    END;

    IF PreviousSignalApproachControlAspect = SingleYellowAspect THEN BEGIN
      IF PreviousSignalAdjacentTCOccupied THEN BEGIN
        Signals[Signals[S].Signal_PreviousSignal1].Signal_ApproachLocked := False;
        PreviousSignalApproachLocked := False;
      END;
    END;

    IF ApproachControlAspect = SingleYellowAspect THEN BEGIN
      IF (PreviousSignalApproachControlAspect <> SingleYellowAspect)
      OR PreviousSignalAdjacentTCOccupied
      THEN
        OK := True;
    END;

    IF (ApproachControlAspect = NoAspect) AND (PreviousSignalApproachControlAspect = SingleYellowAspect) THEN BEGIN
      IF PreviousSignalApproachLocked THEN BEGIN
        OK := True;
        { and reset the lock }
        Signals[S].Signal_ApproachLocked := False;
      END;
    END;

    IF NOT OK THEN BEGIN
      IF NOT Routes_ApproachControlSignalsMsgWrittenArray[Route] THEN BEGIN
        Log(LocoChipToStr(LocoChip) + ' R Cannot release approach-controlled signal ' + IntToStr(S));
        Routes_ApproachControlSignalsMsgWrittenArray[Route] := True;
      END;
      StopProcessingSignals := True;
    END ELSE BEGIN
      { It's ok to set the signal: first check if the array element contains signal theatre data }
      IF Pos('>', ItemToCheck) > 0 THEN BEGIN
        { a theatre indication }
        ActionCh := '>';
        Device := StrToInt(Copy(ItemToCheck, 4, Pos('>', ItemToCheck) - 4));

        TheatreDestinationToConvert := Copy(ItemToCheck, Pos('>', ItemToCheck) + 1, 255);
        IF Pos('FS=', TheatreDestinationToConvert) > 0 THEN
          L := Signals[ExtractSignalFromString(TheatreDestinationToConvert)].Signal_AdjacentLine
        ELSE
          L := BufferStops[ExtractBufferStopFromString(TheatreDestinationToConvert)].BufferStop_AdjacentLine;
      END ELSE BEGIN
        { it doesn't contain theatre data }
        ActionCh := Copy(ItemToCheck, Length(ItemToCheck), 1);
        IF (Pos('UL', ItemToCheck) = 0)
        AND (Pos('ML', ItemToCheck) = 0)
        AND (Pos('LL', ItemToCheck) = 0)
        AND (Pos('UR', ItemToCheck) = 0)
        AND (Pos('MR', ItemToCheck) = 0)
        AND (Pos('LR', ItemToCheck) = 0)
        THEN
          Device := StrToInt(Copy(ItemToCheck, 4, Length(ItemToCheck) - 4))
        ELSE BEGIN
          IF Copy(ItemToCheck, Length(ItemToCheck) - 2, 2) = 'UL' THEN
            JunctionIndicatorState := UpperLeftIndicatorLit
          ELSE
            IF Copy(ItemToCheck, Length(ItemToCheck) - 2, 2) = 'ML' THEN
              JunctionIndicatorState := MiddleLeftIndicatorLit
            ELSE
              IF Copy(ItemToCheck, Length(ItemToCheck) - 2, 2) = 'LL' THEN
                JunctionIndicatorState := LowerLeftIndicatorLit
              ELSE
                IF Copy(ItemToCheck, Length(ItemToCheck) - 2, 2) = 'UR' THEN
                  JunctionIndicatorState := UpperRightIndicatorLit
                ELSE
                  IF Copy(ItemToCheck, Length(ItemToCheck) - 2, 2) = 'MR' THEN
                    JunctionIndicatorState := MiddleRightIndicatorLit
                  ELSE
                    IF Copy(ItemToCheck, Length(ItemToCheck) - 2, 2) = 'LR' THEN
                      JunctionIndicatorState := LowerRightIndicatorLit;

          Device := StrToInt(Copy(ItemToCheck, 4, Length(ItemToCheck) - 6));
        END;
      END;

      CASE ActionCh[1] OF
        '>':
          { deal with theatre indicators }
          BEGIN
            SetIndicator(LocoChipToStr(LocoChip), Device, QueryIndicatorLit, '', Route, NOT ByUser);

            PullSignal(LocoChipStr, Device, TheatreIndicatorLit, Route, UnknownSubRoute, L, ItemToCheck,
                       GetTrainTypeFromLocoChip(LocoChip), NOT ByUser, OK);
            IF OK THEN BEGIN
              IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
                Log(LocoChipToStr(LocoChip) + ' R Setting up route R=' + IntToStr(Route) + ': S=' + IntToStr(Device)
                                            + ': theatre indicator previously held by approach control now set to '
                                            + Signals[Device].Signal_TheatreIndicatorString);
            END;
          END;
        '\':
          BEGIN
            OK := True;
            IF Signals[Device].Signal_Type = TwoAspect THEN
              NewAspect := GreenAspect
            ELSE
              NewAspect := SingleYellowAspect;

            SetSignal(LocoChipStr, Device, NewAspect, LogSignalData, NOT ForceAWrite);
            Log(LocoChipStr + ' R Setting up route R=' + IntToStr(Route) + ': S=' + IntToStr(Device)
                                        + ' previously held by approach control now off');
            FindPreviousSignals(S, Signals[S].Signal_PreviousSignal1, Signals[S].Signal_PreviousSignal2);
            IF Signals[Device].Signal_PreviousSignal1 <> UnknownSignal THEN
              SetPreviousSignals(LocoChipStr, Device);
          END;
        '|':
          BEGIN
            SetIndicator(LocoChipStr, Device, JunctionIndicatorState, '', NoRoute, NOT ByUser);
            Log(LocoChipStr + ' R Setting up route R=' + IntToStr(Route) + ': S=' + IntToStr(Device)
                                        + ' previously held by approach control now routed ' + IndicatorStateToStr(JunctionIndicatorState));
          END;
      END; {CASE}

      DeleteElementFromStringArray(Routes_ApproachControlSignalsWaitingToBeSet[Route], 0);
    END;
  END; {WHILE}
END; { ProcessApproachControlSignals }

PROCEDURE DescribeRouteingStatus;
{ Write to the log the current state of each route and subroute }
VAR
  R : Integer;
  RouteCount : Integer;
  SubRouteCount : Integer;

BEGIN
  Log('R Current routeing status:');
  FOR RouteCount := 0 TO High(Routes_Routes) DO BEGIN
    R := Routes_Routes[RouteCount];
    FOR SubRouteCount := 0 TO High(Routes_SubRouteStates[R]) DO BEGIN
      Log(LocoChipToStr(Routes_LocoChips[R]) + ' R      R=' + IntToStr(R) + '/' + IntToStr(SubRouteCount)
                                             + IfThen(Routes_Journeys[R] <> UnknownJourney,
                                                      ' J=' + IntToStr(Routes_Journeys[R]))
                                             + ' ' + SubRouteStateToStr(Routes_SubRouteStates[R, SubRouteCount])
                                             + ' [' + DescribeJourneyAndRoute([R, SubRouteCount]) + ']'
                                             + IfThen(Routes_SubRouteStates[R, SubRouteCount] = SubRouteSettingUpStalled,
                                                      Routes_RoutesSettingUpStalledMsgArray[R])
                                             + ' {NOUNITREF}');
    END; {FOR}
  END; {FOR}
END; { DescribeRouteingStatus }

PROCEDURE SetUpASubRoute(Route : Integer);
{ Set up a subroute using the supplied route array }
VAR
  ActionCh : String;
  DebugStr : String;
  Device : Integer;
  FailedMsg : String;
  I, J : Integer;
  Journey : Integer;
  JunctionIndicatorState : IndicatorStateType;
  L : Integer;
  LocoChip : Integer;
  LocoChipStr : String;
  NextRoute : Integer;
  OK : Boolean;
  PointResultPending : Boolean;
  RouteCount : Integer;
  RouteOK : Boolean;
  S : Integer;
  SaveSubRouteSettingPos : Integer;
  SaveTCLocoChip : Integer;
  SemaphoreDistantToBeSetOff : Boolean;
  SemaphoreHomeSignalsOff : Boolean;
  SettingSubRouteArray : StringArrayType;
  SettingSubRoute : Integer;
  SubRouteCount : Integer;
  SubRouteItemToCheck : String;
  SubRoutesClearedCount : Integer;
  SubRouteSettingPos : Integer;
  T : TrainIndex;
  TCDebugStr : String;
  TheatreDestinationToConvert : String;

BEGIN
  TRY
    Device := 0;
    OK := True;
    L := UnknownLine;
    T := UnknownTrainIndex;

    SaveTCLocoChip := UnknownLocoChip;
    SettingSubRoute := Routes_CurrentSettingSubRoute[Route];
    SettingSubRouteArray := Routes_SubRouteSettingStrings[Route, SettingSubRoute];

    LocoChip := Routes_LocoChips[Route];
    LocoChipStr := LocoChipToStr(LocoChip);
    IF LocoChip <> UnknownLocoChip THEN
      T := GetTrainIndexFromLocoChip(LocoChip);

    IF (LocoChip = UnknownLocoChip)
    OR ((T <> UnknownTrainIndex) AND (Trains[T].Train_CurrentStatus <> Suspended) AND (Trains[T].Train_CurrentStatus <> MissingAndSuspended))
    THEN BEGIN
      { Is the route able to be set up ? }
      IF (Routes_SubRouteStates[Route, SettingSubRoute] = SubRouteSetUp)
      OR (Routes_SubRouteStates[Route, SettingSubRoute] = SubRouteCleared)
      OR (Routes_SubRouteStates[Route, SettingSubRoute] = SubRouteToBeCleared)
      OR (Routes_SubRouteStates[Route, SettingSubRoute] = SubRouteSettingUpCancelled)
      THEN
        { no point continuing }
        Exit;

      { Check if there are any previous routes for the same loco not been set up }
      RouteCount := 0;
      RouteOK := False;
      WHILE (RouteCount <= High(Routes_Routes)) AND NOT RouteOK DO BEGIN
        IF RouteCount = Route THEN
          RouteOK := True
        ELSE
          IF (Routes_LocoChips[RouteCount] <> UnknownLocoChip) AND (Routes_LocoChips[RouteCount] = Routes_LocoChips[Route]) THEN BEGIN
            IF Routes_RouteSettingsCompleted[RouteCount] THEN
              RouteOK := True
            ELSE BEGIN
              Debug;
              { A permanent breakpoint in case the existing one gets moved or lost: }
              IF DebugHook <> 0 THEN
                ASM
                  Int 3
                END;
              Exit;
            END;
          END;
        Inc(RouteCount);
      END; {FOR}

      IF InStationStartMode THEN BEGIN
        WITH Signals[Routes_StartSignals[Route]] DO BEGIN
          IF Signal_TRSHeld AND NOT Signal_TRSReleased THEN BEGIN
            { the train is waiting to be released by the station supervisor }
            IF NOT Signal_TRSHeldMsgWritten THEN BEGIN
              Log(LocoChipStr + ' R= J='+ IntToStr(Routes_Journeys[Route]) + ' R=' +IntToStr(Route)
                              + ': is waiting to be released by station supervisor');
              Signals[Routes_StartSignals[Route]].Signal_TRSHeld := True;
              Signal_TRSHeldMsgWritten := True;
            END;
            Exit;
          END ELSE
            { supervisor has now released the train }
            IF Signal_TRSHeld AND Signal_TRSReleased THEN BEGIN
              IF NOT Signal_TRSReleasedMsgWritten THEN BEGIN
                Log(LocoChipStr + ' RG R=' +IntToStr(Route) + ': station supervisor has released the route');
                Signal_TRSReleasedMsgWritten := True;
              END;
            END ELSE
              { hold the train }
              IF Signal_PossibleStationStartRouteHold AND NOT Signal_TRSHeld AND NOT Signal_TRSReleased THEN BEGIN
                Routes_SubRouteStates[Route, SettingSubRoute] := SubRouteSettingUpHeldByStationSupervisor;
                Signal_TRSHeld := True;
                Signal_PostColour := clFuchsia;
                DrawSignalPost(Routes_StartSignals[Route]);
                Exit;
              END;
        END; {WITH}
      END;

      IF Routes_SubRouteStates[Route, SettingSubRoute] = SubRouteSettingUpHeld THEN BEGIN
        Routes_SubRouteStates[Route, SettingSubRoute] := SubRouteSettingUpReleased;
        Routes_RoutesSettingUpHeldMsgWrittenArray[Route] := False;
      END;

      IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route]
      AND (Routes_PointResultPendingPoint[Route] = UnknownPoint)
      AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route]
      AND NOT Routes_RoutesSettingUpHeldMsgWrittenArray[Route]
      THEN BEGIN
        IF Routes_SubRouteStates[Route, SettingSubRoute] = SubRouteSettingUpReleased THEN
          Log(LocoChipStr + ' R ' + DescribeJourneyAndRoute([Route, SettingSubRoute]) + ': beginning set up after release')
        ELSE
          Log(LocoChipStr + ' R ' + DescribeJourneyAndRoute([Route, SettingSubRoute]) + ': beginning set up');
        { and enter the subroute in the array of subroutes belonging to this route }
        IF (Routes_SubRouteStates[Route, SettingSubRoute] <> SubRouteSettingUpStalled) AND (Routes_SubRouteStates[Route, SettingSubRoute] <> SubRouteSettingUpReleased) THEN
          Routes_SubRouteStates[Route, SettingSubRoute] := SubRouteNotYetSetUp;
      END;

      Journey := Routes_Journeys[Route];
//      IF Routes_AutoSettingMode[Route] THEN BEGIN
//        IF NOT Routes_RouteSettingByHandInitiated THEN BEGIN
//          IF Journey = UnknownJourney THEN BEGIN
//            Log('X! Unknown journey found in R=' + IntToStr(Route));
//          END;
//        END;
//      END;

      IF Routes_PointResultPendingPoint[Route] <> UnknownPoint THEN
        SubRouteSettingPos := Routes_CurrentSubRouteSettingPos[Route]
      ELSE
        SubRouteSettingPos := 0;

      ActionCh := '';
      WHILE OK AND (SubRouteSettingPos <= High(SettingSubRouteArray)) DO BEGIN
        ActionCh := '';
        DebugStr := '';
        Device := 99999; { covers all unknown devices }

        { ensure that trains are still being moved in the middle of subroute setting }
        DoCheckForUnexpectedData(UnitRef, 'SetUpASubRoute 1');
        IF InAutoMode THEN
          MoveAllTrains;

        { This makes it easier to work with the bit of the array we're interested in! }
        SubRouteItemToCheck := SettingSubRouteArray[SubRouteSettingPos];

        { Check if the array element contains signal or other data }
        IF Pos('>', SubRouteItemToCheck) > 0 THEN BEGIN
          { a theatre indication }
          ActionCh := '>';
          Device := StrToInt(Copy(SubRouteItemToCheck, 4, Pos('>', SubRouteItemToCheck) - 4));
          TheatreDestinationToConvert := Copy(SubRouteItemToCheck, Pos('>', SubRouteItemToCheck) + 1, 255);
          IF TheatreDestinationToConvert = '' THEN BEGIN
            { the destination hasn't been allocated a signal }
            OK := False;
            Log(LocoChipStr + ' R Theatre Destination for S=' + IntToStr(Device) + ' not allocated');
          END ELSE
            IF Pos('FS=', TheatreDestinationToConvert) > 0 THEN
              L := Signals[ExtractSignalFromString(TheatreDestinationToConvert)].Signal_AdjacentLine
            ELSE
              L := BufferStops[ExtractBufferStopFromString(TheatreDestinationToConvert)].BufferStop_AdjacentLine;
        END ELSE
          IF Pos('J=', SubRouteItemToCheck) <> 0 THEN
            TCDebugStr := 'TCs set to J=' + IntToStr(Journey) + ':'
          ELSE
            { Ignore items just there for subrouteing, and have no suffixes }
            IF (Pos('J=', SubRouteItemToCheck) = 0)
            AND (Pos('BS=', SubRouteItemToCheck) = 0)
            AND (Pos('SR=', SubRouteItemToCheck) = 0)
            AND (SubRouteItemToCheck <> HoldMarker)
            THEN BEGIN
              ActionCh := Copy(SubRouteItemToCheck, Length(SubRouteItemToCheck), 1);
              IF Pos('L=', SubRouteItemToCheck) > 0 THEN
                L := StrToLine(Copy(SubRouteItemToCheck, 3, Length(SubRouteItemToCheck) - 3))
              ELSE BEGIN
                IF (Pos('UL', SubRouteItemToCheck) = 0)
                AND (Pos('ML', SubRouteItemToCheck) = 0)
                AND (Pos('LL', SubRouteItemToCheck) = 0)
                AND (Pos('UR', SubRouteItemToCheck) = 0)
                AND (Pos('MR', SubRouteItemToCheck) = 0)
                AND (Pos('LR', SubRouteItemToCheck) = 0)
                THEN
                  Device := StrToInt(Copy(SubRouteItemToCheck, 4, Length(SubRouteItemToCheck) - 4))
                ELSE BEGIN
                  IF Copy(SubRouteItemToCheck, Length(SubRouteItemToCheck) - 2, 2) = 'UL' THEN
                    JunctionIndicatorState := UpperLeftIndicatorLit
                  ELSE
                    IF Copy(SubRouteItemToCheck, Length(SubRouteItemToCheck) - 2, 2) = 'ML' THEN
                      JunctionIndicatorState := MiddleLeftIndicatorLit
                    ELSE
                      IF Copy(SubRouteItemToCheck, Length(SubRouteItemToCheck) - 2, 2) = 'LL' THEN
                        JunctionIndicatorState := LowerLeftIndicatorLit
                      ELSE
                        IF Copy(SubRouteItemToCheck, Length(SubRouteItemToCheck) - 2, 2) = 'UR' THEN
                          JunctionIndicatorState := UpperRightIndicatorLit
                        ELSE
                          IF Copy(SubRouteItemToCheck, Length(SubRouteItemToCheck) - 2, 2) = 'MR' THEN
                            JunctionIndicatorState := MiddleRightIndicatorLit
                          ELSE
                            IF Copy(SubRouteItemToCheck, Length(SubRouteItemToCheck) - 2, 2) = 'LR' THEN
                              JunctionIndicatorState := LowerRightIndicatorLit;

                  Device := StrToInt(Copy(SubRouteItemToCheck, 4, Length(SubRouteItemToCheck) - 6));
                END;
              END;
            END;

        IF OK AND (ActionCh <> '') THEN BEGIN
          { the default state }
          CASE ActionCh[1] OF
            '>':
              { deal with theatre indicators }
              BEGIN
                IF GetSignalAspect(Device) = RedAspect THEN BEGIN
                  IF Signals[Device].Signal_Indicator = TheatreIndicator THEN BEGIN
                    SetIndicator(LocoChipStr, Device, QueryIndicatorLit, '', Route, NOT ByUser);
                    PullSignal(LocoChipStr, Device, TheatreIndicatorLit, Route, SettingSubRoute, L, SubRouteItemToCheck,
                               GetTrainTypeFromLocoChip(LocoChip), NOT ByUser, OK);
                    IF OK THEN BEGIN
                      IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
                        WriteRouteInfoToLog(LocoChipStr, 'R', 'Setting up ' + DescribeJourneyAndRoute([Route, SettingSubRoute]),
                                                              ': theatre indicator for S=' + IntToStr(Device)
                                                              + ' set to ' + Signals[Device].Signal_TheatreIndicatorString);
                    END;
                  END;
                END;
              END;
            '*':
              BEGIN
                { deals with specific track circuit occupations that might get in the way }
                IF (TrackCircuits[Device].TC_LockedForRoute <> UnknownRoute)
                { if the track circuit that is occupied is the one where the subroute starts, ignore it }
                AND NOT IsElementInIntegerArray(TrackCircuits[Device].TC_AdjacentSignals, Routes_SubRouteStartSignals[Route, SettingSubRoute])
                THEN BEGIN
                  IF TrackCircuits[Device].TC_LockedForRoute = Route THEN
                    OK := True
                  ELSE
                    DebugStr := ': TC=' + IntToStr(Device) + ' already locked by'
                                + ' J=' + IntToStr(Routes_Journeys[Route])
                                + ' R=' + IntToStr(TrackCircuits[Device].TC_LockedForRoute) + ': route setting failure'
                END ELSE BEGIN
                  IF (TrackCircuits[Device].TC_OccupationState <> TCUnoccupied)
                  { if the track circuit that is occupied is the one where the subroute starts, ignore it }
                  AND (NOT IsElementInIntegerArray(TrackCircuits[Device].TC_AdjacentSignals, Routes_SubRouteStartSignals[Route, SettingSubRoute]))
                  { and ignore it if it's already allocated to this train }
                  AND (TrackCircuits[Device].TC_LocoChip <> Routes_LocoChips[Route])
                  THEN
                    DebugStr := ': TC=' + IntToStr(Device) + ' is occupied:' + ' route setting failure'
                  ELSE BEGIN
                    { success! }
                    TrackCircuits[Device].TC_SaveRouteLocking := TrackCircuits[Device].TC_LockedForRoute;
                    TrackCircuits[Device].TC_LockedForRoute := Route;
                    SaveTCLocoChip := TrackCircuits[Device].TC_LocoChip;
                    TrackCircuits[Device].TC_LocoChip := Routes_LocoChips[Route];

                    { Set the track circuit journey }
                    IF TCDebugStr <> '' THEN
                      TCDebugStr := TCDebugStr + ' ';
                    { what's TCDebugStr for? ***** 25/7/14 }
                    TCDebugStr := TCDebugStr + 'TC=' + IntToStr(Device)
                                  + IfThen(TrackCircuits[Device].TC_Journey = UnknownJourney,
                                           ' TCJourney ',
                                           ' TCJourney ' + IntToStr(TrackCircuits[Device].TC_Journey) + ' set to ' + IntToStr(Journey) + '; ');
                    TrackCircuits[Device].TC_Journey := Journey;
                    DebugStr := ' : TC=' + IntToStr(Device) + ' now locked by R=' + IntToStr(Route);
                    OK := True;
                  END;
                END;

                IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN BEGIN
                  WriteRouteInfoToLog(LocoChipStr, 'R', 'Setting up ' + DescribeJourneyAndRoute([Route, SettingSubRoute]), DebugStr);
                  Routes_SettingUpFailuresMsgWrittenArray[Route] := True;
                END;

                IF DebugStr <> '' THEN
                  Log(LocoChipStr + ' R ' + DebugStr);
              END;
            '+':
              BEGIN
                OK := True;
                { deals with specific line occupations that might get in the way }
                IF (Lines[L].Line_RouteLockingForDrawing <> UnknownRoute) AND (Lines[L].Line_RouteLockingForDrawing <> Route) THEN BEGIN
                  DebugStr := ': L=' + LineToStr(L) + ' already locked by R=' + IntToStr(Lines[L].Line_RouteLockingForDrawing) + ': route setting failure';
                  OK := False;
                END ELSE BEGIN
                  { check if last line segment in the subroute is two-way: if so, check that subsequent line segments are not locked, or we may end up with an impasse. }
    {***}
                  { success! }
                  Lines[L].Line_RouteLockingForDrawing := Route;
                  DebugStr := ': L=' + LineToStr(L) + ' now locked for routeing by R=' + IntToStr(Route);
                END;
                IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN BEGIN
                  WriteRouteInfoToLog(LocoChipStr, 'R', 'Setting up ' + DescribeJourneyAndRoute([Route, SettingSubRoute]), DebugStr);
                  Routes_SettingUpFailuresMsgWrittenArray[Route] := True;
                END;
              END;
            '\', '=':
              BEGIN
                OK := True;
                IF ActionCh = '\' THEN BEGIN
                  IF GetSignalAspect(Device) <> RedAspect THEN BEGIN
                    DebugStr := ': S=' + IntToStr(Device) + ' already off';
                  END ELSE BEGIN
                    PullSignal(LocoChipStr, Device, NoIndicatorLit, Route, SettingSubRoute, UnknownLine, SubRouteItemToCheck, GetTrainTypeFromLocoChip(LocoChip),
                               NOT ByUser, OK);
                    IF OK THEN BEGIN
                      DebugStr := 'S=' + IntToStr(Device) + ' off';

                      { Now see if a semaphore distant needs to be pulled off too - this check needs to be done each time a signal on the route is pulled off, as the
                        distant may cover more than one subroute
                      }
                      IF Signals[Device].Signal_Type = SemaphoreHome THEN BEGIN
                        SemaphoreHomeSignalsOff := False;
                        { Check that all the semaphore homes are off before proceeding }
                        FOR I := 0 TO High(Signals[Signals[Device].Signal_SemaphoreDistantLocking].Signal_SemaphoreDistantHomesArray) DO BEGIN
                          IF Signals[Signals[Signals[Device].Signal_SemaphoreDistantLocking].Signal_SemaphoreDistantHomesArray[I]].Signal_Aspect <> RedAspect THEN
                            SemaphoreHomeSignalsOff := True
                          ELSE
                            SemaphoreHomeSignalsOff := False;
                        END; {FOR}

                        IF SemaphoreHomeSignalsOff THEN BEGIN
                          { lock the home signals }
                          FOR I := 0 TO High(Signals[Signals[Device].Signal_SemaphoreDistantLocking].Signal_SemaphoreDistantHomesArray) DO
                            Signals[Signals[Signals[Device].Signal_SemaphoreDistantLocking].Signal_SemaphoreDistantHomesArray[I]].Signal_LockedBySemaphoreDistant := True;

                          { and set the distant off }
                          Signals[Signals[Device].Signal_SemaphoreDistantLocking].Signal_Aspect := GreenAspect;
                        END;
                      END;
                    END;
                  END;
                END ELSE BEGIN
                  { ActionCh = '=' }
                  IF GetSignalAspect(Device) = RedAspect THEN BEGIN
                    DebugStr := 'S=' + IntToStr(Device) +' already on';
                  END ELSE BEGIN
                    PullSignal(LocoChipStr, Device, NoIndicatorLit, Route, SettingSubRoute, UnknownLine, SubRouteItemToCheck, GetTrainTypeFromLocoChip(LocoChip),
                               NOT ByUser, OK);
                    IF OK THEN
                      DebugStr := 'S=' + IntToStr(Device) + ' on';
                  END;
                END;
                IF OK THEN
                  Signals[Device].Signal_LockFailureNotedInRouteUnit := False;

                IF DebugStr <> '' THEN
                  Log(LocoChipStr + ' R ' + DebugStr);
              END;
            '|':
              BEGIN
                IF GetSignalAspect(Device) = RedAspect THEN BEGIN
                  IF Signals[Device].Signal_ApproachControlAspect <> NoAspect THEN BEGIN
                    { once we find a signal with approach control, all subsequent route signal setting has to be done using it }
                    Routes_ApproachControlsSet[Route] := True;
                    { but there may be more than one on the route }
                    AppendToIntegerArray(Routes_ApproachControlledSignals[Route], Device);
                  END;

                  IF Signals[Device].Signal_Indicator = JunctionIndicator THEN BEGIN
                    PullSignal(LocoChipStr, Device, JunctionIndicatorState, Route, SettingSubRoute, UnknownLine, SubRouteItemToCheck, GetTrainTypeFromLocoChip(LocoChip),
                               NOT ByUser, OK);
                    IF OK THEN BEGIN
                      IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
                        WriteRouteInfoToLog(LocoChipStr, 'R', 'Setting up ' + DescribeJourneyAndRoute([Route, SettingSubRoute]),
                                                              ': S=' + IntToStr(Device) + ' routed left')
                    END;
                  END;
                END;
                IF OK THEN
                  Signals[Device].Signal_LockFailureNotedInRouteUnit := False;
              END;
            '.':
              OK := True;
            '/', '-': { Point }
              { if we don't know where it's actually pointing, first set it to the state CreatePoint set it up - this is quite safe, as program assumes that all points are
                as setup at startup (but the reset is needed as we don't know what actual state they were left in. This is not needed if we want the point diverging, as it
                would otherwise set it straight first!)
              }
              BEGIN
                OK := True;
                { now interpret the routeing command }
                IF NOT Points[Device].Point_FeedbackPending THEN
                  IF ActionCh = '/' THEN
                    Points[Device].Point_RequiredState := Diverging
                  ELSE
                    { ActionCh = '-' }
                    Points[Device].Point_RequiredState := Straight;

                { If the point result is pending, first see if it's successful }
                IF Routes_PointResultPendingPoint[Route] <> UnknownPoint THEN BEGIN
                  IF Points[Device].Point_FeedbackPending THEN
                    { it's still pending - wait until it's timed out }
                    Exit
                  ELSE BEGIN
                    { Has it successfully changed? }
                    IF Points[Device].Point_MaybeBeingSetToManual THEN
                      { it's still pending - there's another wait }
                      Exit
                    ELSE BEGIN
                      Routes_PointResultPendingPoint[Route] := UnknownPoint;
                      IF Points[Device].Point_PresentState <> Points[Device].Point_RequiredState THEN BEGIN
                        IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN BEGIN
                          Debug('Setting up ' + DescribeJourneyAndRoute([Route, SettingSubRoute]) + ': P=' + IntToStr(Device)
                                + ' (Lenz=' + IntToStr(Points[Device].Point_LenzNum) + ')'
                                + ' result was pending: failed to change');
                          WriteRouteInfoToLog(LocoChipStr, 'R', 'Setting up ' + DescribeJourneyAndRoute([Route, SettingSubRoute]),
                                                                ': P=' + IntToStr(Device) + ' result was pending: failed to change');
                          OK := False;
                        END;
                      END ELSE BEGIN
                        IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN BEGIN
                          WriteRouteInfoToLog(LocoChipStr, 'R', 'Setting up ' + DescribeJourneyAndRoute([Route, SettingSubRoute]),
                                                                ': P=' + IntToStr(Device) + ' result was pending: change was successful');
                          Routes_SettingUpFailuresMsgWrittenArray[Route] := True;
                        END;
                      END;
                    END;
                  END;
                END;

                IF OK THEN BEGIN
                  IF Points[Device].Point_PresentState = Points[Device].Point_RequiredState THEN BEGIN
                    { no need to set it, but must now lock it so that any subroute crossing knows not to change it }
                    LockPointByRoute(LocoChip, Device, Route, Routes_RoutesSettingUpStalledMsgWrittenArray[Route]);
                    IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
                      WriteRouteInfoToLog(LocoChipStr, 'R', 'Setting up ' + DescribeJourneyAndRoute([Route, SettingSubRoute]),
                                                            ': P=' + IntToStr(Device) + ' already ' + PointStateToStr(Points[Device].Point_RequiredState)
                                                            + ' is now locked for routeing');
                  END ELSE BEGIN
                    PullPoint(LocoChipStr, Device, Route, SettingSubRoute, NOT ForcePoint, NOT ByUser, Routes_SettingUpFailuresMsgWrittenArray[Route], PointResultPending,
                              DebugStr, OK);
                    IF OK THEN BEGIN
                      IF PointResultPending THEN BEGIN
                        Routes_PointResultPendingPoint[Route] := Device;
                        { store how far we've got }
                        Routes_CurrentSubRouteSettingPos[Route] := SubRouteSettingPos;
                        IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
                          WriteRouteInfoToLog(LocoChipStr, 'R', 'Setting up ' + DescribeJourneyAndRoute([Route, SettingSubRoute]),
                                                                ': P=' + IntToStr(Device) + ' setting to ' + PointStateToStr(Points[Device].Point_RequiredState)
                                                                + ' - result pending');
                        { and drop out of the While loop }
                        Exit;
                      END ELSE BEGIN
                        IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
                          WriteRouteInfoToLog(LocoChipStr, 'R', 'Setting up ' + DescribeJourneyAndRoute([Route, SettingSubRoute]),
                                                                ': P=' + IntToStr(Device) + ' set to ' + PointStateToStr(Points[Device].Point_RequiredState)
                                                                + ' and locked for routeing');
                        { now lock it so that any subroute crossing knows not to change it }
                        LockPointByRoute(LocoChip, Device, Route, Routes_RoutesSettingUpStalledMsgWrittenArray[Route]);
                      END;
                    END;
                  END;
                END;

                IF DebugStr <> '' THEN
                  Log(LocoChipStr + ' R ' + DebugStr);
              END;
          END; {CASE}
        END;
        Inc(SubRouteSettingPos);
      END; {WHILE}

      { Now deal with the result of the subroute setting }
      IF Routes_PointResultPendingPoint[Route] <> UnknownPoint THEN BEGIN
        IF NOT Routes_PointResultPendingPointMsgWrittenArray[Route] THEN BEGIN
          Log(LocoChipStr + ' P P=' + IntToStr(Routes_PointResultPendingPoint[Route]) + '  : result pending: '
                          + DescribeJourneyAndRoute([Route, SettingSubRoute]));
          Routes_PointResultPendingPointMsgWrittenArray[Route] := True;
        END;
      END;

      IF OK THEN BEGIN
        { Success - the subroute is set up - but note how far we've got }
        Log(LocoChipToStr(Routes_LocoChips[Route]) + ' R R=' + IntToStr(Route) + '/' + IntToStr(SettingSubRoute) + ' successfuly set up'
                                               + ' [' + DescribeJourneyAndRoute([Route, SettingSubRoute]) + ']');

        Routes_SubRouteStates[Route, SettingSubRoute] := SubRouteSetUp;

        { Mark the lines as being routed over so we can draw the subroute }
        DebugStr := 'Lines routed over: ';
        FOR SubRouteSettingPos := 0 TO High(SettingSubRouteArray) DO BEGIN
          IF Pos('L=', SettingSubRouteArray[SubRouteSettingPos]) > 0 THEN BEGIN
            Lines[ExtractLineFromString(SettingSubRouteArray[SubRouteSettingPos])].Line_RouteSet := Route;
            DebugStr := DebugStr + ' L=' + LinetoStr(ExtractLineFromString(SettingSubRouteArray[SubRouteSettingPos]));
          END;
        END; {FOR}

        { and also mark the line just before the start of the subroute, which isn't in the array, but which should be highlighted to make the route clearer }
        Lines[Signals[Routes_SubRouteStartSignals[Route, SettingSubRoute]].Signal_AdjacentLine].Line_RouteSet := Route;
        DebugStr := DebugStr + ' and L=' + LinetoStr(Signals[Routes_SubRouteStartSignals[Route, SettingSubRoute]].Signal_AdjacentLine);
        Log(LocoChipStr + ' R ' + DebugStr);

        InvalidateScreen(UnitRef, 'SetUpASubRoute 1');

        { See if we've done the last subroute (Routes_SubRouteCount starts from 1) }
        IF SettingSubRoute <> (Routes_TotalSubRoutes[Route] - 1) THEN
          Inc(Routes_CurrentSettingSubRoute[Route])
        ELSE BEGIN
          Routes_RouteSettingsInProgress[Route] := False;
          Routes_RouteSettingsCompleted[Route] := True;
          Routes_RouteSettingByHand := False;
          Log(LocoChipStr + ' R R=' + IntToStr(Route) + ': route setting concluded');

          IF (T <> 0) AND (T <= High(Trains)) AND (Trains[T].Train_LocoChip <> UnknownLocoChip) THEN BEGIN
            { only set up hidden station signal aspect signals if there's a train involved }
            WITH Trains[T] DO BEGIN
              WITH Train_JourneysArray[Journey] DO BEGIN { ************************************************************* from MS5 to IS2 }
                TrainJourney_SetUp := True;
                Log(LocoChipStr + ' R J=' + IntToStr(Journey) + ': journey set up');

                { Now the journey is set up, see if we need to set any hidden station signal aspects - to stop trains at signals that otherwise would be off, if we have
                  actually to stop there. Exclude signals where we change direction as we have no choice but to stop at them.
                }
                IF (Train_JourneysArray[Journey].TrainJourney_StoppingOnArrival)
                AND ((Journey < Train_TotalJourneys) AND (Train_JourneysArray[Journey].TrainJourney_Direction = Train_JourneysArray[Journey + 1].TrainJourney_Direction))
                THEN
                  SetHiddenStationSignalAspectSignals(T, Train_JourneysArray[Journey].TrainJourney_EndSignal, Journey, Route);
              END;
            END; {WITH}
          END; {WITH}
        END;
        Routes_SettingUpFailuresMsgWrittenArray[Route] := False;
        Routes_RoutesSettingUpStalledMsgArray[Route] := '';
        Routes_RoutesSettingUpStalledMsgWrittenArray[Route] := False;
        Routes_RoutesSettingUpStalledTimeArray[Route] := 0;
      END ELSE BEGIN
        { Failure - write out the first lock failure, or clear it }
        CASE ActionCh[1] OF
          '*':
            IF NOT TrackCircuits[Device].TC_LockFailureNotedInSubRouteUnit THEN BEGIN
              DebugStr := 'TC=' + IntToStr(Device) + ' is occupied or locked';
              IF TrackCircuits[Device].TC_LockedForRoute <> UnknownRoute THEN
                DebugStr := DebugStr + ' by R=' + IntToStr(TrackCircuits[Device].TC_LockedForRoute);
              TrackCircuits[Device].TC_LockFailureNotedInSubRouteUnit := True;
            END;
          '+':
            IF NOT Lines[L].Line_LockFailureNotedInSubRouteUnit THEN BEGIN
              DebugStr := 'L=' + LineToStr(L) + ' is occupied or locked';
              IF Lines[L].Line_RouteLockingForDrawing <> UnknownRoute THEN
                DebugStr := DebugStr + ' ' + 'by R=' + IntToStr(Lines[L].Line_RouteLockingForDrawing);
              Lines[L].Line_LockFailureNotedInSubRouteUnit := True;
            END;
          '/', '-':
            DebugStr := 'P=' + IntToStr(Device) + ' is locked';
          '|', '=', '\', '>':
            IF NOT Signals[Device].Signal_LockFailureNotedInRouteUnit THEN BEGIN
              DebugStr := 'S=' + IntToStr(Device) + ' is locked';
              Signals[Device].Signal_LockFailureNotedInRouteUnit := True;
            END;
        END; {CASE}

        { As route setting has failed, unlock any item we've just locked, but need only deal with the array elements examined above (and not including the final element,
          because it was not locked)
        }
        SaveSubRouteSettingPos := SubRouteSettingPos - 2;
        SubRouteSettingPos := 0;

        IF Routes_AutoSettingMode[Route] THEN BEGIN
          { If the route is being set up by the program, allow it to complete the routeing in due course even if there's a temporary hiatus... }
          Routes_SubRouteStates[Route, SettingSubRoute] := SubRouteSettingUpStalled;
          IF Routes_RoutesSettingUpStalledTimeArray[Route] = 0 THEN
            Routes_RoutesSettingUpStalledTimeArray[Route] := CurrentRailwayTime
          ELSE BEGIN
            { see how long the train has been stalled - we may wish to try to reroute it }
            IF CompareTime(CurrentRailwayTime, IncMinute(Routes_RoutesSettingUpStalledTimeArray[Route], WaitBeforeRerouteInMinutes)) > 0
            THEN BEGIN
              Log(LocoChipStr + ' RG Train has been stalled for more than ' + IntToStr(WaitBeforeRerouteInMinutes) + ' minutes so attempting re-route');

              { First remove the existing route }


              { Reset to allow a further time before attempting another re-route }
              Routes_RoutesSettingUpStalledTimeArray[Route] := 0;
            END;
          END;

          IF NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN BEGIN
            Routes_RoutesSettingUpStalledMsgArray[Route] := ' (' + DebugStr +')';
            Log(LocoChipStr + ' R ' + DescribeJourneyAndRoute(Route)
                            + '  : route setting-up stalled' + Routes_RoutesSettingUpStalledMsgArray[Route]);
            Routes_RoutesSettingUpStalledMsgWrittenArray[Route] := True;
            DescribeRouteingStatus;
          END;
        END ELSE BEGIN
          { ...but if route is being set by a humanoid, cancel all subsequent settings up }
          Routes_SubRouteStates[Route, SettingSubRoute] := SubRouteSettingUpFailed;
          IF Routes_RouteSettingByHand THEN BEGIN
            Routes_RouteSettingByHand := False;
            Debug('Route setting-up failed (' + DebugStr + ')');
          END;
          Log(LocoChipStr + ' R ' + DescribeJourneyAndRoute([Route, SettingSubRoute])
                          + ': route setting-up failed (' + DebugStr + ')');
          { and if there are any more subroutes to be set up, mark their setting as being cancelled }
          IF Routes_TotalSubRoutes[Route] > (SettingSubRoute + 1) THEN
            FOR SubRouteCount := (SettingSubRoute + 1) TO (Routes_TotalSubRoutes[Route] - 1) DO
              Routes_SubRouteStates[Route, SubRouteCount] := SubRouteSettingUpCancelled;

          Routes_RouteSettingsInProgress[Route] := False;
        END;

        WHILE SubRouteSettingPos <= SaveSubRouteSettingPos DO BEGIN
        { need only go as far as we went above, not the whole array **** }
          { The following makes it easier to work with the bit of the array we're interested in! }
          SubRouteItemToCheck := SettingSubRouteArray[SubRouteSettingPos];
          { Check if the array element contains signal or other data }
          IF Pos('>', SubRouteItemToCheck) > 0 THEN
            { a theatre indication }
            ActionCh := '>'
          ELSE BEGIN
            { Ignore items just there for subrouteing, and have no suffixes }
            IF (Pos('J=', SubRouteItemToCheck) = 0)
            AND (Pos('BS=', SubRouteItemToCheck) = 0)
            AND (Pos('SR=', SubRouteItemToCheck) = 0)
            AND (SubRouteItemToCheck <> HoldMarker)
            THEN BEGIN
              ActionCh := Copy(SubRouteItemToCheck, Length(SubRouteItemToCheck), 1);
              IF (Pos('L=', SubRouteItemToCheck) = 0) THEN
                Device := StrToInt(Copy(SubRouteItemToCheck, 4, Length(SubRouteItemToCheck) - 4))
              ELSE
                L := StrToLine(Copy(SubRouteItemToCheck, 3, Length(SubRouteItemToCheck) - 3));

              CASE ActionCh[1] OF
                '/', '-':
                  { now unlock the points just locked, if any }
                  IF PointIsLockedByASpecificRoute(Device, Route) THEN BEGIN
                    UnlockPointLockedBySpecificRoute(Device, Route, Routes_RoutesSettingUpStalledMsgWrittenArray[Route]);
                    IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
                      Log(LocoChipStr + ' R As cannot set up R=' + IntToStr(Route) + '/' + IntToStr(SettingSubRoute) + ', P=' + IntToStr(Device) + ' locking cancelled');
                  END;
                '\', '=':
                  { now unlock the signals just locked, if any }
                  IF SignalIsLockedBySpecificRoute(Device, Route) THEN BEGIN
                    UnlockSignalLockedBySpecificRoute(Device, Route);
                    IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
                      Log(LocoChipStr + ' R As cannot set up R=' + IntToStr(Route) + '/' + IntToStr(SettingSubRoute) + ', S=' + IntToStr(Device) + ' locking cancelled');
                  END;
                '+':
                  { check if the line is marked as displaying a route }
                  IF Lines[L].Line_RouteLockingForDrawing = Route THEN BEGIN
                    Lines[L].Line_RouteLockingForDrawing := UnknownRoute;
                    IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
                      Log(LocoChipStr + ' R As cannot set up R=' + IntToStr(Route) + '/' + IntToStr(SettingSubRoute)
                                      + ', L=' + LineToStr(L) + ' route-locking drawing cancelled');
                    Lines[L].Line_LockFailureNotedInSubRouteUnit := False;
                  END;
                '*':
                  { and any track circuits }
                  IF TrackCircuits[Device].TC_LockedForRoute = Route THEN BEGIN
                    TrackCircuits[Device].TC_LockedForRoute := TrackCircuits[Device].TC_SaveRouteLocking;
                    TrackCircuits[Device].TC_LocoChip := SaveTCLocoChip;
                    TrackCircuits[Device].TC_Journey := UnknownJourney;

                    IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
                      Log(LocoChipStr + ' R As cannot set up R=' + IntToStr(Route) + '/' + IntToStr(SettingSubRoute) + ', TC=' + IntToStr(Device) + ' locking cancelled');
                  END;
                END; {CASE}
              END;
            END;
          Inc(SubRouteSettingPos);
        END; {WHILE}
      END;

      { Make a note of current routeing }
      IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN
        DescribeRouteingStatus;

      IF NOT OK THEN BEGIN
        IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] AND NOT Routes_RoutesSettingUpStalledMsgWrittenArray[Route] THEN BEGIN
          FailedMsg := DescribeJourneyAndRoute([Route, SettingSubRoute]) + ': setting up failed (' + DebugStr + ')';
          IF Routes_SubRouteStates[Route, SettingSubRoute] = SubRouteSettingUpFailed THEN BEGIN
            IF NOT Routes_SettingUpFailuresMsgWrittenArray[Route] THEN BEGIN
              Log(LocoChipStr + ' RG ' + FailedMsg);
              Routes_SettingUpFailuresMsgWrittenArray[Route] := True;
            END;
          END ELSE BEGIN
            IF Routes_RoutesSettingUpStalledMsgArray[Route] <> DebugStr THEN BEGIN
              Routes_RoutesSettingUpStalledMsgArray[Route] := ' (' + DebugStr +')';
              Log(LocoChipStr + ' R ' + DescribeJourneyAndRoute(Route) + '  : route setting-up stalled' + Routes_RoutesSettingUpStalledMsgArray[Route]);
              Routes_RoutesSettingUpStalledMsgWrittenArray[Route] := True;
            END;
          END;
        END;
      END;

      { Now delete any routes cancelled }
      RouteCount := 0;
      WHILE RouteCount <= High(Routes_Routes) DO BEGIN
        NextRoute := Routes_Routes[RouteCount];
        SubRoutesClearedCount := 0;
        FOR SubRouteCount := 0 TO High(Routes_SubRouteStates[NextRoute]) DO BEGIN
          IF (Routes_SubRouteStates[NextRoute, SubRouteCount] = SubRouteCleared)
          OR (Routes_SubRouteStates[NextRoute, SubRouteCount] = SubRouteSettingUpFailed)
          OR (Routes_SubRouteStates[NextRoute, SubRouteCount] = SubRouteSettingUpCancelled)
          THEN
            Inc(SubRoutesClearedCount);
          IF SubRoutesClearedCount = Routes_TotalSubRoutes[NextRoute] THEN BEGIN
            Log(LocoChipStr + ' R Now deleting cancelled R=' + IntToStr(NextRoute) + ': '
                            + SubRouteStateToStr(Routes_SubRouteStates[NextRoute, SubRouteCount]));
            { Delete the route number so we can't see it again }
            DeleteElementFromIntegerArray(Routes_Routes, RouteCount);
            { and delete the route date stored in the multi-dimensional dynamic arrays }
            SetLength(Routes_SubRouteSettingStrings[NextRoute], 0);
            SetLength(Routes_SubRouteStartSignals[NextRoute], 0);
            SetLength(Routes_SubRouteStartLines[NextRoute], 0);
            SetLength(Routes_SubRouteEndLines[NextRoute], 0);

            Log(LocoChipStr + ' R Cancelled R=' + IntToStr(NextRoute) + ' deleted');
          END;
        END; {FOR}
        Inc(RouteCount);
      END; {WHILE}
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG SetUpASubRoute: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SetUpASubRoute }

PROCEDURE ClearARouteByBruteForce(R : Integer);
{ Clear a route by brute force - needed sometimes if the normal route clearing fails }
CONST
  ErrorMessageRequired = True;

VAR
  L : Integer;
  P : Integer;
  S : Integer;
  SubRoute : Integer;
  T : Integer;
  TC : Integer;

BEGIN
  Log('A User clearing R=' + IntToStr(R));

  FOR T := 0 To High(Trains) DO
    IF Trains[T].Train_CurrentRoute = R THEN
      Trains[T].Train_CurrentRoute := UnknownRoute;

  { Set any signals locked by the route to on }
  FOR S := 0 TO High(Signals) DO BEGIN
    IF SignalIsLockedBySpecificRoute(S, R) THEN BEGIN
      UnlockSignalLockedBySpecificRoute(S, R);
      IF Signals[S].Signal_Aspect <> RedAspect THEN BEGIN
        Signals[S].Signal_Aspect := RedAspect;
        Signals[S].Signal_PreviousAspect := RedAspect;
      END;
      IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN BEGIN
        Signals[S].Signal_IndicatorState := NoIndicatorLit;
        Signals[S].Signal_PreviousIndicatorState := NoIndicatorLit;
      END;
      UnlockPointsLockedBySignal(S);
    END;
  END; {FOR}

  { Unlock anything else locked by the route }
  FOR P := 0 TO High(Points) DO
    UnlockPointLockedBySpecificRoute(P, R, ErrorMessageRequired);

  FOR TC := 0 TO High(TrackCircuits) DO BEGIN
    IF TrackCircuits[TC].TC_LockedForRoute = R THEN
      UnlockTrackCircuitRouteLocking(TC);
  END; {FOR}

  { Clear the line highlighting too }
  FOR L := 0 TO High(Lines) DO BEGIN
    IF Lines[L].Line_RouteLockingForDrawing = R THEN BEGIN
      Lines[L].Line_RouteLockingForDrawing := UnknownRoute;
      Lines[L].Line_RouteSet := UnknownRoute;
    END;
  END; {FOR}

//  IF (High(Routes_SubRouteStartSignals[R, 0]) > -1) AND (High(Routes_SubRouteStartSignals) <= R) THEN BEGIN
    { and also clear the line just before the start of the subroute, which isn't in the array, but would have been highlighted to make the route clearer }
    IF R <= Length(Routes_SubRouteStartSignals) THEN BEGIN
      IF R <= High(Routes_SubRouteStartSignals) THEN
        IF Routes_SubRouteStartSignals[R, 0] <> UnknownSignal THEN
          Lines[Signals[Routes_SubRouteStartSignals[R, 0]].Signal_AdjacentLine].Line_RouteSet := UnknownRoute;
    END;
//  END;

  IF (High(Routes_TotalSubRoutes) > -1) AND (High(Routes_TotalSubRoutes) <= R) THEN
    FOR SubRoute := 0 TO (Routes_TotalSubRoutes[R] - 1) DO
      Routes_SubRouteStates[R, SubRoute] := SubRouteCleared;

  IF (High(Routes_RouteClearingsInProgress) > -1) AND (High(Routes_RouteClearingsInProgress) <= R) THEN
    Routes_RouteClearingsInProgress[R] := False;
  IF (High(Routes_Cleared) > -1) AND (High(Routes_Cleared) <= R) THEN
    Routes_Cleared[R] := True;
  Log('RG R=' + IntToStr(R) + ' has been cleared by the user');

  InvalidateScreen(UnitRef, 'ClearRoute');
END; { ClearARouteByBruteForce }

PROCEDURE ClearARoute(Route : Integer);
{ Use a string consisting of pairs of signal/point name then '/' or '-' etc. to clear the route. (Most of the route may already have been cleared by the passage of the
  train, but sometimes this routine gets there first!)
}
CONST
  Delayed = True;
  ErrorMsgRequired = True;

VAR
  ActionCh : String;
  ClearingSubRouteArray : StringArrayType;
  ClearingSubRoute : Integer;
  DebugStr : String;
  Device : Integer;
  EarlierRouteToBeClearedFound : Boolean;
  LocoChip : Integer;
  LocoChipStr : String;
  OK : Boolean;
  SubRouteClearingPos : Integer;
  SubRouteCount : Integer;
  SubRouteItemToCheck, TheatreDestinationToConvert : String;
  SubRoutesClearedCount : Integer;
  TempL : Integer;
  TempP : Integer;
  TempR : Integer;
  TempS : Integer;
  TempRoute : Integer;

BEGIN
  TempR := 0; { we want this 0, not -1, as otherwise the test will operate even if this is the first route }
  EarlierRouteToBeClearedFound := False;
  WHILE (TempR < Route) AND NOT EarlierRouteToBeClearedFound DO BEGIN
    IF (Routes_LocoChips[TempR] = Routes_LocoChips[Route]) AND (Routes_LocoChips[TempR] <> UnknownLocoChip) THEN BEGIN
      IF NOT Routes_Cleared[TempR] THEN BEGIN
        Routes_RouteClearingsInProgress[TempR] := True;
        EarlierRouteToBeClearedFound := True;

        IF NOT Routes_ClearingFailuresMsg1WrittenArray[Route] THEN BEGIN
          Log(LocoChipToStr(Routes_LocoChips[TempR]) + ' RG R=' + IntToStr(TempR) + ' Routes_RouteClearingInProgress set to true,'
                                                     + ' as later route R=' + IntToStr(Route) + ' has been set to be cleared');
          Routes_ClearingFailuresMsg1WrittenArray[Route] := True;
        END;
      END;
    END;
    Inc(TempR);
  END;

  IF NOT EarlierRouteToBeClearedFound THEN BEGIN
    { Now deal with the given route }
    ActionCh := '';
    Device := 0;
    OK := True;
    TempL := UnknownLine;
    LocoChip := Routes_LocoChips[Route];
    LocoChipStr := LocoChipToStr(LocoChip);

    ClearingSubRoute := Routes_CurrentClearingSubRoute[Route];
    ClearingSubRouteArray := Routes_SubRouteClearingStrings[Route, ClearingSubRoute];
    IF Length(ClearingSubRouteArray) = 0 THEN BEGIN
      { shouldn't ever get here }
      Log(LocoChipStr + ' E+ Creating a new subroute clearing array for R=' + IntToStr(Route) + ' SR=' + IntToStr(ClearingSubRoute) + ' as the one provided was empty');
      CreateClearingSubRouteArray(Route, ClearingSubRoute);
      ClearingSubRouteArray := Routes_SubRouteClearingStrings[Route, ClearingSubRoute];

      Routes_SubRouteStates[Route, ClearingSubRoute] := SubRouteCleared;
      Exit;
    END;

    IF (Routes_SubRouteStates[Route, ClearingSubRoute] = SubRouteNotYetSetUp)
    OR (Routes_SubRouteStates[Route, ClearingSubRoute] = SubRouteSettingUpInProgress)
    OR (Routes_SubRouteStates[Route, ClearingSubRoute] = SubRouteSettingUpStalled)
    THEN
      { no point continuing }
      Exit;

    IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] AND (Routes_PointResultPendingPoint[Route] = UnknownPoint) THEN
      Log(LocoChipStr + ' R Beginning final clearing of ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]));
      

    { If there is a Routes_PointResultPendingPoint, we are re-entering the set subroute routine where we left it, so do not want to reset SubRouteClearingPos. }
    IF Routes_PointResultPendingPoint[Route] <> UnknownPoint THEN
      SubRouteClearingPos := Routes_CurrentSubRouteClearingPos[Route]
    ELSE
      SubRouteClearingPos := 0;

    IF (Routes_SubRouteStates[Route, ClearingSubRoute] <> SubRouteSetUp) AND (Routes_SubRouteStates[Route, ClearingSubRoute] <> SubRouteToBeCleared) THEN BEGIN
      { clearing, or cancelling if route not set up }
      Routes_SubRouteStates[Route, ClearingSubRoute] := SubRouteSettingUpCancelled;
      Routes_RouteingsCancelled[Route] := True;
    END;

    WHILE (Routes_SubRouteStates[Route, ClearingSubRoute] <> SubRouteSettingUpCancelled) AND (SubRouteClearingPos <= High(ClearingSubRouteArray)) DO BEGIN
      ActionCh := '';
      Device := 0;
      DebugStr := '';

      { ensure that trains are still being moved in the middle of subroute clearing }
      DoCheckForUnexpectedData(UnitRef, 'ClearARoute 1');
      IF InAutoMode THEN
        MoveAllTrains;

      { This makes it easier to work with the bit of the array we're interested in! }
      SubRouteItemToCheck := ClearingSubRouteArray[SubRouteClearingPos];

      { Check if the array element contains signal or other data }
      IF Pos('>', SubRouteItemToCheck) > 0 THEN BEGIN
        { a theatre indication }
        ActionCh := '>';
        Device := StrToInt(Copy(SubRouteItemToCheck, 4, Pos('>', SubRouteItemToCheck) - 4));

        TheatreDestinationToConvert := Copy(SubRouteItemToCheck, Pos('>', SubRouteItemToCheck) + 1, 255);
        IF Pos('FS=', TheatreDestinationToConvert) > 0 THEN
          TempL := Signals[ExtractSignalFromString(TheatreDestinationToConvert)].Signal_AdjacentLine
        ELSE
          TempL := BufferStops[ExtractBufferStopFromString(TheatreDestinationToConvert)].BufferStop_AdjacentLine;
      END ELSE
        { Ignore items just there for subrouteing, and have no suffixes }
        IF (Pos('J=', SubRouteItemToCheck) = 0) AND (Pos('BS=', SubRouteItemToCheck) = 0) AND (Pos('SR=', SubRouteItemToCheck) = 0) THEN BEGIN
          ActionCh := Copy(SubRouteItemToCheck, Length(SubRouteItemToCheck), 1);
          IF (Pos('L=', SubRouteItemToCheck) = 0) THEN
            Device := StrToInt(Copy(SubRouteItemToCheck, 4, Length(SubRouteItemToCheck) - 4))
          ELSE
            TempL := StrToLine(Copy(SubRouteItemToCheck, 3, Length(SubRouteItemToCheck) - 3));
        END;

        IF ActionCh <> '' THEN BEGIN
          { the default state }
          OK := False;
          CASE ActionCh[1] OF
            '*':
              BEGIN
                { check if the track circuit is locked - as it may already have been unlocked in SetTrackCircuitStateMainProc }
                IF TrackCircuits[Device].TC_LockedForRoute = Route THEN
                  UnlockTrackCircuitRouteLocking(Device);

                IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] THEN
                  WriteRouteInfoToLog(LocoChipStr, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
                                                        ': TC=' + IntToStr(Device) + ' now unlocked by R=' + IntToStr(Route));
                TrackCircuits[Device].TC_LockFailureNotedInSubRouteUnit := False;
                OK := True;
              END;

            '+':
              { check if the line is locked }
              IF Lines[TempL].Line_RouteLockingForDrawing = UnknownRoute THEN
                OK := True
              ELSE
                IF Lines[TempL].Line_RouteLockingForDrawing = Route THEN BEGIN
                  Lines[TempL].Line_RouteLockingForDrawing := UnknownRoute;
                  IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] THEN
                    WriteRouteInfoToLog(LocoChipStr, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
                                                          ': L=' + LineToStr(TempL) + ' now unlocked by R=' + IntToStr(Route));
                  Lines[TempL].Line_LockFailureNotedInSubRouteUnit := False;
                  OK := True;
                END;

            '=':
              BEGIN
                OK := True;
                { set a signal on: first unlock it }
                IF SignalIsLockedBySpecificRoute(Device, Route) THEN
                  UnlockSignalLockedBySpecificRoute(Device, Route);

                { Now see if a semaphore distant needs to be put on too - this check needs to be done each time a signal on the route is put on, as the distant may
                  cover more than one subroute
                }
                CheckSemaphoreDistantBeforeSemaphoreHomeCleared(Device);

                { Now reset it }
                IF NOT (GetSignalAspect(Device) = RedAspect) THEN BEGIN
                  PullSignal(LocoChipStr, Device, NoIndicatorLit, Route, ClearingSubRoute, UnknownLine, '', NOT ByUser, OK);
                  IF OK THEN BEGIN
                    IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] THEN
                      WriteRouteInfoToLog(LocoChipStr, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
                                                            ': S=' + IntToStr(Device) + ' on' + ' and now unlocked by R=' + IntToStr(Route));
                  END;
                END;

                IF OK THEN BEGIN
                  Signals[Device].Signal_LockFailureNotedInRouteUnit := False;
                  { and reset approach locking if set (though this would normally be done by the train passing it) }
                  IF Signals[Device].Signal_ApproachLocked THEN BEGIN
                    Signals[Device].Signal_ApproachLocked := False;
                    UnlockPointsLockedBySignal(Device);
                  END;
                END;
              END;

            '.':
              OK := True;

//            '/', '-': { reset points }
//              BEGIN
//                OK := True;
//                IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] THEN BEGIN
//                  IF NOT Points[Device].Point_FeedbackPending
//                  AND NOT Points[Device].Point_MaybeBeingSetToManual
//                  AND NOT (Points[Device].Point_DefaultState = PointStateUnknown)
//                  THEN
//                    WriteRouteInfoToLog(LocoChip, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
//                                                       ': resetting P=' + IntToStr(Device)
//                                                       + ' to default state '
//                                                       + PointStateToStr(Points[Device].Point_DefaultState));
//                END;
//                IF PointIsLockedByASpecificRoute(Device, Route) THEN BEGIN
//                  IF Points[Device].Point_RequiredState <> PointStateUnknown THEN BEGIN
//                    Points[Device].Point_RequiredState := Points[Device].Point_DefaultState;
//                    { now unlock it, so it will change }
//                    UnlockPointLockedBySpecificRoute(Device, Route, Routes_RoutesSettingUpStalledMsgWrittenArray[Route]);
//
//                    { First see if the point change is successful }
//                    IF Routes_PointResultPendingPoint[Route] <> UnknownPoint THEN BEGIN
//                      IF Points[Device].Point_FeedbackPending THEN
//                        { it's still pending - wait until it's timed out }
//                        Exit
//                      ELSE BEGIN
//                        { Has it successfully changed? }
//                        IF Points[Device].Point_MaybeBeingSetToManual THEN
//                          { it's still pending - there's another wait }
//                          Exit
//                        ELSE BEGIN
//                          Routes_PointResultPendingPoint[Route] := UnknownPoint;
//                          IF Points[Device].Point_PresentState <> Points[Device].Point_RequiredState THEN BEGIN
//                            IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] THEN BEGIN
//                              Debug('Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute])
//                                    + ': P=' + IntToStr(Device)
//                                    + ' (Lenz=' + IntToStr(Points[Device].Point_LenzNum) + ')'
//                                    + ' result was pending: failed to change');
//                              WriteRouteInfoToLog(LocoChip, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
//                                                                 ': P=' + IntToStr(Device) + ' result was pending: failed to change');
//                            END;
//                            OK := False;
//                          END ELSE BEGIN
//                            IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] THEN
//                              WriteRouteInfoToLog(LocoChip, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
//                                                                 ': P=' + IntToStr(Device) + ' result was pending: change was successful');
//                          END;
//                        END;
//                      END;
//                    END;
//                  END;
//                END;
//
//                IF OK THEN BEGIN
//                  IF Points[Device].Point_PresentState = Points[Device].Point_RequiredState THEN BEGIN
//                    IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] THEN
//                      WriteRouteInfoToLog(LocoChip, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
//                                                         ': P=' + IntToStr(Device) + ' now unlocked by R=' + IntToStr(Route));
//                    { now unlock it }
//                    UnlockPointLockedBySpecificRoute(Device, Route, Routes_RoutesSettingUpStalledMsgWrittenArray[Route]);
//                  END ELSE BEGIN
//                    IF (Points[Device].Point_RequiredState <> PointStateUnknown) AND NOT Routes_RouteClearingsWithoutPointResetting[Route] THEN BEGIN
//                      PullPoint(Device, LocoChip, Route, ClearingSubRoute, NOT ForcePoint, NOT ByUser,
//                                Routes_ClearingFailuresMsg2WrittenArray[Route], PointResultPending, DebugStr, OK);
//                      IF OK THEN BEGIN
//                        Routes_PointResultPendingPoint[Route] := UnknownPoint;
//                        Routes_ClearingFailuresMsg2WrittenArray[Route] := False;
//                        { now unlock it }
//                        UnlockPointLockedBySpecificRoute(Device, Route, Routes_RoutesSettingUpStalledMsgWrittenArray[Route]);
//                        IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] THEN
//                          WriteRouteInfoToLog(LocoChip, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
//                                                             ': P=' + IntToStr(Device) + ' set to '
//                                                             + PointStateToStr(Points[Device].Point_RequiredState)
//                                                             + ' and now unlocked by R=' + IntToStr(Route));
//                      END;
//                    END;
//                  END;
//                END;
//
//                { Check if the array element contains point data }
//                IF PointIsLockedByASpecificRoute(Device, Route) THEN BEGIN
//                  UnlockPointLockedBySpecificRoute(Device, Route, Routes_RoutesSettingUpStalledMsgWrittenArray[Route]);
//                  IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] THEN
//                    WriteRouteInfoToLog(LocoChip, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
//                                                       ': P=' + IntToStr(Device) + ' now unlocked by R=' + IntToStr(Route));
//                  OK := True;
//                END;
//              END;
            '/', '-': { reset points }
              BEGIN
                OK := True;
                IF NOT Routes_ClearingFailuresMsg2WrittenArray[Route] THEN BEGIN
                  IF NOT Points[Device].Point_FeedbackPending
                  AND NOT Points[Device].Point_MaybeBeingSetToManual
                  AND NOT (Points[Device].Point_DefaultState = PointStateUnknown)
                  AND NOT Routes_RouteClearingsWithoutPointResetting[Route]
                  THEN BEGIN
                    WriteRouteInfoToLog(LocoChipStr, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
                                                          ': resetting P=' + IntToStr(Device) + ' to default state ' + PointStateToStr(Points[Device].Point_DefaultState));
                    { store the points to be reset later if and when possible }
                    AppendToIntegerArray(PointResettingToDefaultStateArray, Device);
                  END;
                END;

                IF PointIsLockedByASpecificRoute(Device, Route) THEN BEGIN
                  IF Points[Device].Point_RequiredState <> PointStateUnknown THEN BEGIN
                    Points[Device].Point_RequiredState := Points[Device].Point_DefaultState;
                    { now unlock it, so it will change }
                    UnlockPointLockedBySpecificRoute(Device, Route, Routes_RoutesSettingUpStalledMsgWrittenArray[Route]);
                    WriteRouteInfoToLog(LocoChipStr, 'R', 'Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]),
                                                          ': P=' + IntToStr(Device) + ' now unlocked by R=' + IntToStr(Route));
                  END;
                END;
              END;
          END; {CASE}
        END;

      Inc(SubRouteClearingPos);
    END; {WHILE}

    IF OK THEN BEGIN
      { Success - note how far we've got }
      { If a subroute wasn't set, then mark it as cancelled, not cleared }
      IF Routes_RouteingsCancelled[Route] THEN
        Log(LocoChipStr + ' R New Cancelled ' + DescribeJourneyAndRoute(Route));

      IF Routes_SubRouteStates[Route, ClearingSubRoute] = SubRouteSettingUpCancelled THEN
        Log(LocoChipStr + ' R Old Cancelled ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]))
      ELSE BEGIN
        Log(LocoChipStr + ' R Successfully cleared ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]));
        Routes_SubRouteStates[Route, ClearingSubRoute] := SubRouteCleared;
      END;

      { Mark the lines as not being routed over - for undrawing the subroute }
      FOR SubRouteClearingPos := 0 TO High(ClearingSubRouteArray) DO
        IF Pos('L=', ClearingSubRouteArray[SubRouteClearingPos]) > 0 THEN
          IF Lines[ExtractLineFromString(ClearingSubRouteArray[SubRouteClearingPos])].Line_RouteSet <> UnknownRoute THEN
            Lines[ExtractLineFromString(ClearingSubRouteArray[SubRouteClearingPos])].Line_RouteSet := UnknownRoute;

      { and also mark the line just before the start of the subroute, which isn't in the array, but would have been highlighted to make the route clearer }
      Lines[Signals[Routes_SubRouteStartSignals[Route, 0]].Signal_AdjacentLine].Line_RouteSet := UnknownRoute;
      InvalidateScreen(UnitRef, 'SetUpASubRoute 2');

      { See if we've done the last subroute }
      IF ClearingSubRoute = (Routes_TotalSubRoutes[Route] - 1) THEN BEGIN
        Routes_RouteClearingsInProgress[Route] := False;
        Routes_RouteClearingsWithoutPointResetting[Route] := False;

        { Mark the associated journeys as to be cleared too (we can't mark them as "cleared", as routes are cleared when the final track circuit is occupied, whereas
          journeys are cleared when the following track circuit is occupied).
        }
        IF Routes_LocoChips[Route] <> UnknownLocoChip THEN BEGIN
          IF Routes_Trains[Route] = 0 THEN BEGIN
            Log(LocoChipToStr(Routes_LocoChips[Route]) + ' R! Routes_Trains[Route] = NIL though Routes_LocoChips[Route] = ' + LocoChipToStr(Routes_LocoChips[Route]));
          END ELSE BEGIN
            WITH Trains[Routes_Trains[Route]] DO BEGIN
              Train_JourneysArray[Routes_Journeys[Route]].TrainJourney_Cleared := True;
              Log(Train_LocoChipStr + ' R R=' + IntToStr(Route) + ' cleared');

              { Remove any remaining route lockings - there shouldn't be any at this stage but occasionally there are some left over }
              FOR TempP := 0 TO High(Points) DO BEGIN
                IF PointIsLockedByASpecificRoute(TempP, Route) THEN BEGIN
                  Log(Train_LocoChipStr + ' R P=' + IntToStr(TempP) + ' unlocked by R=' + IntToStr(Route));
                  UnlockPointLockedBySpecificRoute(TempP, Route, NOT ErrorMsgRequired);
                END;
              END; {FOR}

              FOR TempS := 0 TO High(Signals) DO BEGIN
                IF SignalIsLockedBySpecificRoute(TempS, Route) THEN BEGIN
                  Log(Train_LocoChipStr + ' R S=' + IntToStr(TempS) + ' unlocked by R=' + IntToStr(Route));
                  UnlockSignalLockedBySpecificRoute(TempS, Route);
                END;
              END; {FOR}

              FOR TempL := 0 TO High(Lines) DO BEGIN
                IF Lines[TempL].Line_RouteLockingForDrawing = Route THEN BEGIN
                  Log(Train_LocoChipStr + ' R L=' + LineToStr(TempL) + ' unlocked by R=' + IntToStr(Route));
                  Lines[TempL].Line_RouteLockingForDrawing := UnknownRoute;
                END;
              END; {FOR}
            END; {WITH}
          END;
        END;

        { ensure that trains are still being moved }
        DoCheckForUnexpectedData(UnitRef, 'ClearARoute 2');
        IF InAutoMode THEN
          MoveAllTrains;

      END ELSE BEGIN
        Inc(Routes_CurrentClearingSubRoute[Route]);
        { see if the next subroute is ready to be cleared }
        IF Routes_SubRouteStates[Route, ClearingSubRoute + 1] <> SubRouteToBeCleared THEN BEGIN
          Routes_RouteClearingsInProgress[Route] := False;
          Routes_RouteClearingsWithoutPointResetting[Route] := False;
        END;
      END;
      Routes_ClearingFailuresMsg1WrittenArray[Route] := False;
      Routes_ClearingFailuresMsg2WrittenArray[Route] := False;
      Routes_ClearingFailuresMsg3WrittenArray[Route] := False;
      Routes_Cleared[Route] := True;

      WITH Signals[Routes_StartSignals[Route]] DO BEGIN
        Signal_TRSHeld := False;
        Signal_TRSReleased := False;
        Signal_PostColour := SignalPostColour;
        DrawSignal(Routes_StartSignals[Route]);
        Signal_TRSHeldMsgWritten := False;
        Signal_TRSReleasedMsgWritten := False;
      END; {WITH}
    END ELSE BEGIN
      IF NOT Routes_ClearingFailuresMsg3WrittenArray[Route] THEN BEGIN
        Log(LocoChipStr + ' R **** Clearing ' + DescribeJourneyAndRoute([Route, ClearingSubRoute]) + ' failed: ' + DebugStr);
        Routes_ClearingFailuresMsg3WrittenArray[Route] := True;
      END;

      { Failure - write out the first lock failure, or clear it }
      DrawFailure(Device, ActionCh);
      { and write it to the log file, once }
      CASE ActionCh[1] OF
        '*':
          IF NOT TrackCircuits[Device].TC_LockFailureNotedInSubRouteUnit THEN BEGIN
            DebugStr := 'TC=' + IntToStr(Device) + ' is occupied or locked';
            TrackCircuits[Device].TC_LockFailureNotedInSubRouteUnit := True;
          END;

        '+':
          IF NOT Lines[TempL].Line_LockFailureNotedInSubRouteUnit THEN BEGIN
            DebugStr := 'L=' + LineToStr(TempL) + ' is occupied or locked';
            Lines[TempL].Line_LockFailureNotedInSubRouteUnit := True;
          END;

        '/', '-':
          IF NOT Points[Device].Point_LockFailureNotedInSubRouteUnit THEN BEGIN
            PointIsLocked(Device, DebugStr);
            DebugStr := 'P=' + IntToStr(Device) + ' cannot be changed: ' + DebugStr;
            Points[Device].Point_LockFailureNotedInSubRouteUnit := True;
          END;

        '=':
          IF NOT Signals[Device].Signal_LockFailureNotedInRouteUnit THEN BEGIN
            SignalIsLocked(Device, DebugStr);
            DebugStr := 'S=' + IntToStr(Device) + ' cannot be changed: ' + DebugStr;
            Signals[Device].Signal_LockFailureNotedInRouteUnit := True;
          END;
        ELSE
          Log(LocoChipStr + ' E Odd character "' + ActionCh[1] + '" in subroute clearing array');
      END; {CASE}

      IF DebugStr <> '' THEN
        { write it out but don't clear it as it is used later too }
        Log(LocoChipStr + ' R ' + DebugStr);
    END;

    { Make a note of current routeing }
    DescribeRouteingStatus;

    { Now delete any routes cleared }
    TempR := 0;
    WHILE TempR <= High(Routes_Routes) DO BEGIN
      TempRoute := Routes_Routes[TempR];
      SubRoutesClearedCount := 0;
      FOR SubRouteCount := 0 TO High(Routes_SubRouteStates[TempRoute]) DO BEGIN
        IF (Routes_SubRouteStates[TempRoute, SubRouteCount] = SubRouteCleared)
           OR (Routes_SubRouteStates[TempRoute, SubRouteCount] = SubRouteSettingUpFailed)
           OR (Routes_SubRouteStates[TempRoute, SubRouteCount] = SubRouteSettingUpCancelled)
        THEN
          Inc(SubRoutesClearedCount);

        IF SubRoutesClearedCount = Routes_TotalSubRoutes[TempRoute] THEN BEGIN
          { Delete the route number so we can't see it again }
          DeleteElementFromIntegerArray(Routes_Routes, TempR);
          { and delete the route data stored in the multi-dimensional dynamic arrays }
          SetLength(Routes_SubRouteClearingStrings[TempRoute], 0);
          SetLength(Routes_SubRouteStartSignals[TempRoute], 0);
          SetLength(Routes_SubRouteStartLines[TempRoute], 0);
          SetLength(Routes_SubRouteEndLines[TempRoute], 0);
          Log(LocoChipStr + ' R Cleared R=' + IntToStr(TempRoute) + ' deleted');

          IF Routes_Trains[TempRoute] <> 0 THEN BEGIN
            WITH Trains[Routes_Trains[TempRoute]] DO BEGIN
              Train_CurrentSourceLocation := UnknownLocation;

              { Update the train's status, but not if it's been cancelled (routes are cleared automatically when trains are cancelled) }
              IF Train_CurrentStatus <> Cancelled tHEN BEGIN
                Train_PreviousStatus := Train_CurrentStatus;
                IF Train_CurrentJourney = UnknownJourney THEN
                  { not sure if this is right, but can't think what else to do **** }
                  ChangeTrainStatus(Routes_Trains[TempRoute], NonMoving)
                ELSE BEGIN
                  IF Train_JourneysArray[Train_CurrentJourney].TrainJourney_StoppingOnArrival THEN
                    ChangeTrainStatus(Routes_Trains[TempRoute], RouteCompleted)
                  ELSE BEGIN
                    ChangeTrainStatus(Routes_Trains[TempRoute], Departed);

                    { Increment the journey counter - this is done at the end of routes by the CheckTrainsHaveArrived subroutine }
                    Train_JourneysArray[Train_CurrentJourney].TrainJourney_ActualArrivalTime := CurrentRailwayTime;
                    RecalculateJourneyTimes(Routes_Trains[TempRoute], 
                                            'as have arrived at ' + LocationToStr(Train_JourneysArray[Train_CurrentJourney].TrainJourney_EndLocation, LongStringType));

                    Inc(Train_CurrentJourney);
                    Log(LocoChipStr + ' R Train_CurrentJourney incremented to ' + IntToStr(Train_CurrentJourney) + ' in ClearARoute');
                    DrawDiagrams(UnitRef, 'ClearARoute');
                  END;
                END;
              END;
            END; {WITH}
          END;
        END;
      END; {FOR}
      Inc(TempR);
    END; {WHILE}
  END;
END; { ClearARoute }

PROCEDURE ReleaseSubRoutes;
{ If train has cleared all the track circuits in a subroute, subroute can be released }
CONST
  Undo = True;

VAR
  I : Integer;
  FirstSubRouteToBeCleared : Integer;
  LastTCOnRoute : Integer;
  RouteCount, SubRouteCount : Integer;
  Route : Integer;
  TestSubRoute : StringArrayType;

BEGIN
  LastTCOnRoute := UnknownTrackCircuit;
  SetLength(TestSubRoute, 0);

  IF InAutoMode THEN BEGIN
    IF Length(Routes_Routes) > 0 THEN BEGIN
      { Look at each route, and see if any subroutes are eligible for release }
      FOR RouteCount := 0 TO High(Routes_Routes) DO BEGIN
        Route := Routes_Routes[RouteCount];
        FOR SubRouteCount := 0 TO (Routes_TotalSubRoutes[Route] - 1) DO BEGIN
          IF Routes_SubRouteStates[Route, SubRouteCount] = SubRouteSetUp THEN BEGIN
            { Extract the subroute }
            TestSubRoute := Routes_SubRouteSettingStrings[Route, SubRouteCount];
            { Now find the last track circuit on the route }
            FOR I := 0 TO High(TestSubRoute) DO
              IF ExtractTrackCircuitFromString(TestSubRoute[I]) <> UnknownTrackCircuit THEN
                LastTCOnRoute := ExtractTrackCircuitFromString(TestSubRoute[I]);

            { If it's occupied, and the signal controlling it is back on, then the route has been used }
            IF (LastTCOnRoute <> UnknownTrackCircuit)
            AND (TrackCircuits[LastTCOnRoute].TC_LockedForRoute = Route)
            AND (TrackCircuits[LastTCOnRoute].TC_OccupationState = TCFeedbackOccupation)
            AND (GetSignalAspect(Routes_SubRouteStartSignals[Route, SubRouteCount]) = RedAspect)
            THEN BEGIN
              { also see if the previous subroute has cleared - it should have, but if not, perhaps clear it? Otherwise it will probably stay set, and have to be cleared
                by hand ***
              }
              IF (SubRouteCount > 1)
              AND (Routes_SubRouteStates[Route, SubRouteCount - 1] <> SubRouteToBeCleared)
              AND (Routes_SubRouteStates[Route, SubRouteCount - 1] <> SubRouteCleared)
              THEN BEGIN
                Log(LocoChipToStr(Routes_LocoChips[Route]) + ' XG Clearing R=' + IntToStr(Route)
                                                           + ' SR=' + IntToStr(SubRouteCount - 1)
                                                           + ' which has not been cleared before clearing SR=' + IntToStr(SubRouteCount));

                { now initiate the route clearing process for the previous subroute }
                CreateClearingSubRouteArray(Route, SubRouteCount - 1);
                Routes_SubRouteStates[Route, SubRouteCount - 1] := SubRouteToBeCleared;
                Routes_RouteClearingsInProgress[Route] := True;

                { telling the system to start with this subroute }
                Routes_CurrentClearingSubRoute[Route] := SubRouteCount - 1;
              END ELSE BEGIN
                { clear the one that has just been marked as needing clearing }
                Log(LocoChipToStr(Routes_LocoChips[Route]) + ' R Clearing ' + DescribeJourneyAndRoute([Route, SubRouteCount]) + ' as TC='
                                                           + IntToStr(LastTCOnRoute) + ' is marked as having feedback occupation');

                { Initiate the route clearing process, but make sure all previous subroutes are cleared too }
                FirstSubRouteToBeCleared := -1;
                FOR I := 0 TO SubRouteCount DO BEGIN
                  IF Routes_SubRouteStates[Route, I] <> SubRouteCleared THEN BEGIN
                    { telling the system to start with this subroute }
                    IF I <> SubRouteCount THEN
                      Log(LocoChipToStr(Routes_LocoChips[Route]) + ' R Previous subroute ' + IntToStr(I) + ' had not cleared - initiating clearing now');
                    CreateClearingSubRouteArray(Route, I);
                    Routes_SubRouteStates[Route, I] := SubRouteToBeCleared;

                    { telling the system to start with the first subroute }
                    IF FirstSubRouteToBeCleared = -1 THEN BEGIN
                      FirstSubRouteToBeCleared := I;
                      Routes_CurrentClearingSubRoute[Route] := FirstSubRouteToBeCleared;
                    END;

                    Routes_RouteClearingsInProgress[Route] := True;
                  END;
                END;
              END;
            END;
          END;
        END; {FOR}
      END; {FOR}
    END;
  END;
END; { ReleaseSubRoutes }

INITIALIZATION

END { Route }.