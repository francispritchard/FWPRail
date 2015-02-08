UNIT Signal;

INTERFACE

USES
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, InitVars;

TYPE
  TSignalForm = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE ClearHiddenStationSignalAspectSignals(T : TrainIndex; HiddenStationSignalAspectSignal : Integer);
{ Clear the hidden station aspects of a given signal }

PROCEDURE PullSignal{1}(LocoChipStr: String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        SettingString : String; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }

PROCEDURE PullSignal{2}(LocoChipStr: String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }

PROCEDURE PullSignal{3}(LocoChipStr: String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        ResetTC : Integer; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal. This version is only used by signals resetting track circuits, which can happen even if the signal is locked by a route,}

PROCEDURE PullSignal{4}(LocoChipStr: String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        SettingString : String; TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal; includes the original setting string for saving if necessary }

PROCEDURE RestoreAllSignalsToPreviousState;
{ Sets all off signals to how they were before short circuit }

PROCEDURE SaveSignalsCurrentState;
{ Save all the previous state of all signals }

PROCEDURE SetAllSignalsToDanger;
{ Sets all off signals to on }

PROCEDURE SetIndicator(LocoChipStr : String; S : Integer; IndicatorState : IndicatorStateType; TheatreIndicatorString : String; Route : Integer; User : Boolean);
{ Turn the indicator of a particular signal on or off }

PROCEDURE SetHiddenStationSignalAspectSignals(T : TrainIndex; HiddenStationSignalAspectSignal, Journey, Route : Integer);
{ Find and set current and previous hidden station aspects }

PROCEDURE SetPreviousSignals(LocoChipStr : String; S : Integer);
{ Sees what previous signals are set to, and resets aspects accordingly. PreviousSignal1 is the nearer and PreviousSignal2 the further }

PROCEDURE SetSignal(LocoChipStr : String; S : Integer; NewAspect : AspectType; LogSignalData, ForceWriting : Boolean);
{ Set the state of a particular signal and draws it }

FUNCTION SignalIsLocked(S : Integer; OUT LockingMsg : String) : Boolean;
{ Returns true if the signal is locked }

FUNCTION SignalIsLockedByAnyRoute(S : Integer; OUT RouteLockingArray : IntegerArrayType) : Boolean;
{ Returns true if the signal is locked by any route (in which case the routes doing the locking are returned in RouteLockingArray) }

FUNCTION SignalIsLockedByOppositePassingLoopSignal(S : Integer; OUT OtherS : Integer) : Boolean;
{ Returns true if an opposite passing loop signal is off }

FUNCTION SignalIsLockedBySpecificRoute(S, Route : Integer) : Boolean;
{ Returns true if the signal is locked by the given route, unless Route is -1, when it returns true if the signal is locked by any route (in which case the routes doing the
  locking are returned in RouteLockingArray).
}
PROCEDURE UnlockSignalLockedBySpecificRoute(S, Route : Integer);
{ Remove the locking }

PROCEDURE UnlockSignalLockedByUser(S : Integer);
{ Remove the locking }

PROCEDURE TurnAllSignalsOff;
{ Turn off the LEDs in the signals }

VAR
  SignalForm: TSignalForm;

IMPLEMENTATION

{$R *.dfm}

USES RailDraw, Route, MiscUtils, Lenz, StrUtils, Locks, CreateRoute, Options;

CONST
  UnitRef = 'Signal';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE SaveSignalsCurrentState;
{ Save all the previous state of all signals }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF NOT Signal_OutOfUse THEN BEGIN
          Signal_PreviousAspect := Signals[S].Signal_Aspect;
          Signal_PreviousIndicatorState := Signals[S].Signal_IndicatorState;
          Signal_PreviousTheatreIndicatorString := Signal_TheatreIndicatorString;
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG SaveSignalsCurrentState:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SaveSignalsCurrentState }

PROCEDURE SetSignal(LocoChipStr : String; S : Integer; NewAspect : AspectType; LogSignalData, ForceWriting : Boolean);
{ Set the state of a particular signal and draws it }
VAR
  OK : Boolean;

BEGIN
  TRY
    WITH Signals[S] DO BEGIN
      IF (Signal_Aspect <> NewAspect) OR ForceWriting THEN BEGIN
        Signal_Aspect := NewAspect;
        IF NOT ProgramStarting AND LogSignalData THEN BEGIN
          IF (Signals[S].Signal_Type <> SemaphoreHome) AND (Signals[S].Signal_Type <> SemaphoreDistant) THEN
            Log('S S=' + IntToStr(S) + ' successfully set to ' + AspectToStr(Signals[S].Signal_Aspect))
          ELSE
            Log('S S=' + IntToStr(S) + ' successfully set to ' + SemaphoreAspectToStr(Signals[S].Signal_Aspect));
        END;

        IF SystemOnline AND NOT ResizeMap THEN BEGIN
          IF Signal_DecoderNum <> 0 THEN
            { uses LF100 decoders - bits usually set as follows:
              green is bit 1, red 2, single yellow 3, double yellow 3 + 4; the indicator is bit 4 (not necessarily on same decoder though)
            }
            SetSignalFunction(LocoChipStr, S)
          ELSE
            IF Signal_AccessoryAddress <> 0 THEN
              { uses TrainTech SC3 units for controlling Dapol semaphores }
              IF NewAspect = RedAspect THEN
                MakeSemaphoreSignalChange(LocoChipStr, S, Signal_AccessoryAddress, SignalOn, OK)
              ELSE
                MakeSemaphoreSignalChange(LocoChipStr, S, Signal_AccessoryAddress, SignalOff, OK);
        END;

        IF NOT ProgramStarting THEN
          { calling invalidate here didn't redraw the signal in time - repaint causes the screen to be redrawn instantly and not via the message queue }
          FWPRailWindow.Repaint;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG SetSignal:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetSignal }

PROCEDURE SetAllSignalsToDanger;
{ Sets all off signals to on }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      IF NOT Signals[S].Signal_OutOfUse THEN BEGIN
        IF (GetSignalAspect(S) <> RedAspect) THEN
          SetSignal(UnknownLocoChipStr, S, RedAspect, LogSignalData, NOT ForceAWrite);
        IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
          SetIndicator(UnknownLocoChipStr, S, NoIndicatorLit, '', NoRoute, NOT ByUser);
      END;
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG SetAllSignalsToDanger:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { SetAllSignalsToDanger }

PROCEDURE SetPreviousSignals(LocoChipStr : String; S : Integer);
{ Sees what previous signals are set to, and resets aspects accordingly. PreviousSignal1 is the nearer and PreviousSignal2 the further }
BEGIN
  WITH Signals[S] DO BEGIN
    CASE Signal_Aspect OF
      RedAspect:
        IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
          SetSignal(LocoChipStr, Signal_PreviousSignal1, SingleYellowAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousSignal2 <> UnknownSignal THEN BEGIN
            IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
              SetSignal(LocoChipStr, Signal_PreviousSignal2, DoubleYellowAspect, LogSignalData, NOT ForceAWrite);
          END;
        END;
      SingleYellowAspect:
        IF (Signal_IndicatorState <> NoIndicatorLit) AND (Signal_ApproachControlAspect = SingleYellowAspect) THEN BEGIN
          IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
            SetSignal(LocoChipStr, Signal_PreviousSignal1, FlashingSingleYellowAspect, LogSignalData, NOT ForceAWrite);
            IF Signal_PreviousSignal2 <> UnknownSignal THEN
              IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
                SetSignal(LocoChipStr, Signal_PreviousSignal2, FlashingDoubleYellowAspect, LogSignalData, NOT ForceAWrite);
          END;
        END ELSE BEGIN
          IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
            IF Signals[Signal_PreviousSignal1].Signal_Type = FourAspect THEN
              SetSignal(LocoChipStr, Signal_PreviousSignal1, DoubleYellowAspect, LogSignalData, NOT ForceAWrite)
            ELSE
              SetSignal(LocoChipStr, Signal_PreviousSignal1, GreenAspect, LogSignalData, NOT ForceAWrite);
            IF Signal_PreviousSignal2 <> UnknownSignal THEN
              IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
                SetSignal(LocoChipStr, Signal_PreviousSignal2, GreenAspect, LogSignalData, NOT ForceAWrite);
          END;
        END;
      DoubleYellowAspect:
        IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
          SetSignal(LocoChipStr, Signal_PreviousSignal1, GreenAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousSignal2 <> UnknownSignal THEN
            IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
              SetSignal(LocoChipStr, Signal_PreviousSignal2, GreenAspect, LogSignalData, NOT ForceAWrite);
        END;
      GreenAspect:
        IF Signals[Signal_PreviousSignal1].Signal_Aspect <> RedAspect THEN BEGIN
          SetSignal(LocoChipStr, Signal_PreviousSignal1, GreenAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousSignal2 <> UnknownSignal THEN
            IF Signals[Signal_PreviousSignal2].Signal_Aspect <> RedAspect THEN
              SetSignal(LocoChipStr, Signal_PreviousSignal2, GreenAspect, LogSignalData, NOT ForceAWrite);
        END;
    END; {CASE}
  END; {WITH}
END; { SetPreviousSignals }

PROCEDURE SetIndicator(LocoChipStr : String; S : Integer; IndicatorState : IndicatorStateType; TheatreIndicatorString : String; Route : Integer; User : Boolean);
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
      IF (Route <> UnknownRoute) AND SignalIsLockedByAnyRoute(S, RouteLockingArray) THEN BEGIN
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

            Log(LocoChipStr + ' R ' + DebugStr);
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
          Log(LocoChipStr + ' S ' + DebugStr);

          InvalidateScreen(UnitRef, 'SetIndicator');

          IF SystemOnline THEN
            { NB: Often route indicator will be operated by a different LF100 to the one operating the signal }
            SetSignalRouteFunction(LocoChipStr, S);
        END;
      END;
    END;
  END; {WITH}
END; { SetIndicator }

PROCEDURE RestoreAllSignalsToPreviousState;
{ Sets all off signals to how they were before short circuit }
VAR
  S : Integer;

BEGIN
  TRY
    Log('S Restoring all signals to their previous aspects');
    FOR S := 0 TO High(Signals) DO BEGIN
      WITH Signals[S] DO BEGIN
        IF NOT Signal_OutOfUse THEN BEGIN
          { have to set state to NoAspect, or SetSignal won't redraw the SignalAspect/indicator }
          Signal_Aspect := NoAspect;
          Signal_IndicatorState := NoIndicatorLit;
          SetSignal(UnknownLocoChipStr, S, Signal_PreviousAspect, LogSignalData, NOT ForceAWrite);
          IF Signal_PreviousIndicatorState <> NoIndicatorLit THEN
            SetIndicator(UnknownLocoChipStr, S, Signal_PreviousIndicatorState, Signal_PreviousTheatreIndicatorString, NoRoute, NOT ByUser);
        END;
      END; {WITH}
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG RestoreAllSignalsToPreviousState:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { RestoreAllSignalsToPreviousState }

PROCEDURE TurnAllSignalsOff;
{ Turn off the LEDs in the signals }
VAR
  S : Integer;

BEGIN
  TRY
    FOR S := 0 TO High(Signals) DO BEGIN
      SetSignal(UnknownLocoChipStr, S, NoAspect, LogSignalData, NOT ForceAWrite);
      IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
        SetIndicator(UnknownLocoChipStr, S, NoIndicatorLit, '', NoRoute, NOT ByUser);
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG TurnAllSignalsOff:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TurnAllSignalsOff }

PROCEDURE SetHiddenStationSignalAspectSignals(T : TrainIndex; HiddenStationSignalAspectSignal, Journey, Route : Integer);
{ Find and set current and previous hidden station aspects }

  PROCEDURE FindPreviousHiddenStationSignalAspectSignals(RouteArray : StringArrayType; CurrentSignal : Integer; VAR PreviousSignal1, PreviousSignal2 : Integer);
  { Look through a journey to see if any previous aspects need to be set too }
  VAR
    Done : Boolean;
    RouteArrayPos : Integer;
    TempS : Integer;

  BEGIN
    RouteArrayPos := High(RouteArray);
    Done := False;
    WHILE (RouteArrayPos > -1) AND NOT Done DO BEGIN
      IF Pos('FS=', RouteArray[RouteArrayPos]) > 0 THEN BEGIN
        IF ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal THEN
          IF (PreviousSignal1 = UnknownSignal) AND (ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal) THEN
            PreviousSignal1 := ExtractSignalFromString(RouteArray[RouteArrayPos])
          ELSE
            IF PreviousSignal2 = UnknownSignal THEN BEGIN
              { assuming it's not the signal we've just found above }
              TempS := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              IF (TempS <> UnknownSignal) AND (TempS <> PreviousSignal1) THEN BEGIN
                Done := True;
                { only use it if it's a four aspect signal }
                IF Signals[TempS].Signal_Type = FourAspect THEN
                  PreviousSignal2 := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              END;
            END;
      END;
      Dec(RouteArrayPos)
    END; {WHILE}
  END; { FindPreviousHiddenStationSignalAspectSignals }

VAR
  PreviousHiddenStationSignalAspectSignal1 : Integer;
  PreviousHiddenStationSignalAspectSignal2 : Integer;
  TempJourneyCount : Integer;
  TempLocoChipStr : String;

BEGIN
  WITH RailWindowBitmap.Canvas DO BEGIN
    IF T = UnknownTrainIndex THEN
      UnknownTrainRecordFound('SetHiddenStationSignalAspectSignals')
    ELSE BEGIN
      WITH Trains[T] DO BEGIN
        IF T <= High(Trains) THEN
          TempLocoChipStr := LocoChipToStr(Train_LocoChip)
        ELSE
          TempLocoChipStr := '';

        IF Signals[HiddenStationSignalAspectSignal].Signal_Aspect <> RedAspect THEN
          Log(TempLocoChipStr + ' X+ S=' + IntToStr(HiddenStationSignalAspectSignal) + ': cannot set hidden station aspect as signal is off')
        ELSE BEGIN
          Signals[HiddenStationSignalAspectSignal].Signal_HiddenStationSignalAspect := RedAspect;
          Signals[HiddenStationSignalAspectSignal].Signal_PostColour := clRed;

          Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                              + ' S=' + IntToStr(HiddenStationSignalAspectSignal) + ': hidden station aspect set to red');
          IF ShowSignalHiddenStationSignalAspects THEN
            DrawSignal(HiddenStationSignalAspectSignal);

          PreviousHiddenStationSignalAspectSignal1 := UnknownSignal;
          PreviousHiddenStationSignalAspectSignal2 := UnknownSignal;
          TempJourneyCount := Journey;
          IF TempLocoChipStr <> '' THEN BEGIN
            WHILE ((PreviousHiddenStationSignalAspectSignal1 = UnknownSignal) OR (PreviousHiddenStationSignalAspectSignal2 = UnknownSignal))
            AND (TempJourneyCount > -1) AND NOT Train_JourneysArray[TempJourneyCount].TrainJourney_Cleared
            DO BEGIN
              FindPreviousHiddenStationSignalAspectSignals(Train_JourneysArray[TempJourneyCount].TrainJourney_RouteArray,
                                                     HiddenStationSignalAspectSignal, PreviousHiddenStationSignalAspectSignal1,
                                                     PreviousHiddenStationSignalAspectSignal2);
              Dec(TempJourneyCount);
            END; {WHILE}
          END;

          Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1 := PreviousHiddenStationSignalAspectSignal1;
          Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2 := PreviousHiddenStationSignalAspectSignal2;

          { If this signal has a hidden station aspect that's on, make sure previous signals are included }
          IF (PreviousHiddenStationSignalAspectSignal1 <> UnknownSignal) AND (Signals[PreviousHiddenStationSignalAspectSignal1].Signal_Type = FourAspect) THEN BEGIN
            Signals[PreviousHiddenStationSignalAspectSignal1].Signal_HiddenStationSignalAspect := SingleYellowAspect;
            Signals[PreviousHiddenStationSignalAspectSignal1].Signal_PostColour := clYellow;
            Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                                + ' S=' + IntToStr(HiddenStationSignalAspectSignal) + '''s previous signal S='
                                + IntToStr(PreviousHiddenStationSignalAspectSignal1) + ': hidden station aspect set to single yellow');
            IF ShowSignalHiddenStationSignalAspects THEN
              DrawSignal(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1);

            IF (PreviousHiddenStationSignalAspectSignal2 <> UnknownSignal)
            AND ((Signals[PreviousHiddenStationSignalAspectSignal2].Signal_Type = FourAspect)
                 OR (Signals[PreviousHiddenStationSignalAspectSignal2].Signal_Type = ThreeAspect))
            THEN BEGIN
              Signals[PreviousHiddenStationSignalAspectSignal2].Signal_HiddenStationSignalAspect := DoubleYellowAspect;
              Signals[PreviousHiddenStationSignalAspectSignal2].Signal_PostColour := clYellow;
              Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                                  + ' S=' + IntToStr(HiddenStationSignalAspectSignal) + '''s previous signal but one S='
                                  + IntToStr(PreviousHiddenStationSignalAspectSignal2) + ': hidden station aspect set to double yellow');
              IF ShowSignalHiddenStationSignalAspects THEN
                DrawSignal(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2);
            END;
          END ELSE
            IF (PreviousHiddenStationSignalAspectSignal1 <> UnknownSignal) AND (Signals[PreviousHiddenStationSignalAspectSignal1].Signal_Type = ThreeAspect) THEN BEGIN
              Signals[PreviousHiddenStationSignalAspectSignal1].Signal_HiddenStationSignalAspect := SingleYellowAspect;
              Signals[PreviousHiddenStationSignalAspectSignal1].Signal_PostColour := clYellow;
              Log(TempLocoChipStr + ' R J=' + IntToStr(Journey) + ' R=' + IntToStr(Route)
                                  + ' S=' + IntToStr(HiddenStationSignalAspectSignal) + '''s previous signal S='
                                  + IntToStr(PreviousHiddenStationSignalAspectSignal1) + ' hidden station aspect set to single yellow');
              IF ShowSignalHiddenStationSignalAspects THEN
                DrawSignal(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1);
            END;
        END;
      END; {WITH}
    END;
  END; {WITH}
END; { SetHiddenStationSignalAspectSignals }

PROCEDURE ClearHiddenStationSignalAspectSignals(T : TrainIndex; HiddenStationSignalAspectSignal : Integer);
{ Clear the hidden station aspects of a given signal }
VAR
  TempLocoChipStr : String;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('ClearHiddenStationSignalAspectSignals')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF T <= High(Trains) THEN
        TempLocoChipStr := LocoChipToStr(Train_LocoChip)
      ELSE
        TempLocoChipStr := '';

      Signals[HiddenStationSignalAspectSignal].Signal_HiddenStationSignalAspect := NoAspect;
      Signals[HiddenStationSignalAspectSignal].Signal_PostColour := SignalPostColour;
      IF ShowSignalHiddenStationSignalAspects THEN
        DrawSignal(HiddenStationSignalAspectSignal);
      Log(TempLocoChipStr + ' R S=' + IntToStr(HiddenStationSignalAspectSignal) + ' hidden station aspect set to no aspect');

      { and also reset any previous hidden station signal aspect signals }
      IF (Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1 <> UnknownSignal)
      AND (Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1].Signal_HiddenStationSignalAspect <> NoAspect)
      THEN BEGIN
        Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1].Signal_HiddenStationSignalAspect := NoAspect;
        Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1].Signal_PostColour := SignalPostColour;
        Log(TempLocoChipStr + ' R S=' + IntToStr(HiddenStationSignalAspectSignal) + '''s previous signal S='
                            + IntToStr(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1)
                            + ' hidden station aspect set to no aspect');
        IF ShowSignalHiddenStationSignalAspects THEN
          DrawSignal(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal1);
      END;

      IF (Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2 <> UnknownSignal)
      AND (Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2].Signal_HiddenStationSignalAspect <> NoAspect)
      THEN BEGIN
        Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2].Signal_HiddenStationSignalAspect := NoAspect;
        Signals[Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2].Signal_PostColour := SignalPostColour;
        Log(TempLocoChipStr + ' R S=' + IntToStr(HiddenStationSignalAspectSignal) + '''s previous signal but one S='
                            + IntToStr(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2)
                            + ' hidden station aspect set to no aspect');
        IF ShowSignalHiddenStationSignalAspects THEN
          DrawSignal(Signals[HiddenStationSignalAspectSignal].Signal_PreviousHiddenStationSignalAspectSignal2);
       END;
    END; {WITH}
  END;
END; { ClearHiddenStationSignalAspectSignals }

PROCEDURE PullSignalMainProcedure(LocoChipStr : String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
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
      WHILE (LockCount < Length(Signal_LockedArray)) AND (Result = False) DO BEGIN
        IF Signal_LockedArray[LockCount] = 'USER' THEN
          Result := True;
        Inc(LockCount);
      END; {WHILE}
    END; {WITH}
  END; { SignalIsLockedByUser }

  PROCEDURE LockSignalByRoute(LocoChipStr : String; S, Route : Integer; DoNotWriteMessage : Boolean);
  { Mark the signal as locked by a specific route }
  VAR
    ElementPos : Integer;

  BEGIN
    IF NOT IsElementInStringArray(Signals[S].Signal_LockedArray, 'R=' + IntToStr(Route), ElementPos) THEN BEGIN
      AppendToStringArray(Signals[S].Signal_LockedArray, 'R=' + IntToStr(Route));
      IF NOT DoNotWriteMessage THEN
        Log(LocoChipStr + ' S S=' + IntToStr(S) + ' now locked by R=' + IntToStr(Route));
    END;
  END; { LockSignalByRoute }

  PROCEDURE LockSignalByUser(S : Integer);
  { Mark the signal as locked by a user }
  BEGIN
    AppendToStringArray(Signals[S].Signal_LockedArray, 'USER');
    RemoveDuplicateElementsFromStringArray(Signals[S].Signal_LockedArray);
    Log('S S=' + IntToStr(S) + ' now locked by user');
  END; { LockSignalByUser }

  PROCEDURE FindPreviousHiddenStationSignalAspectSignals(RouteArray : StringArrayType; CurrentSignal : Integer; VAR PreviousSignal1, PreviousSignal2 : Integer);
  { Look through a journey to see if any previous hidden station aspects need to be set too }
  VAR
    Done : Boolean;
    RouteArrayPos : Integer;
    TempS : Integer;

  BEGIN
    RouteArrayPos := High(RouteArray);
    Done := False;
    WHILE (RouteArrayPos > -1) AND NOT Done DO BEGIN
      IF Pos('FS=', RouteArray[RouteArrayPos]) > 0 THEN BEGIN
        IF ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal THEN
          IF (PreviousSignal1 = UnknownSignal) AND (ExtractSignalFromString(RouteArray[RouteArrayPos]) <> CurrentSignal) THEN
            PreviousSignal1 := ExtractSignalFromString(RouteArray[RouteArrayPos])
          ELSE
            IF PreviousSignal2 = UnknownSignal THEN BEGIN
              { assuming it's not the signal we've just found above }
              TempS := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              IF (TempS <> UnknownSignal) AND (TempS <> PreviousSignal1) THEN BEGIN
                Done := True;
                { only use it if it's a four aspect signal }
                IF Signals[TempS].Signal_Type = FourAspect THEN
                  PreviousSignal2 := ExtractSignalFromString(RouteArray[RouteArrayPos]);
              END;
            END;
      END;
      Dec(RouteArrayPos)
    END; {WHILE}
  END; { FindPreviousHiddenStationSignalAspectSignals }

  FUNCTION SignalLockingOK(LocoChipStr : String; S : Integer; LockList : StringArrayType; ShowError : Boolean) : Boolean;
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
          WHILE (PointLockCount < Length(Point_LockingArray)) AND (Result = False) DO BEGIN
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
    OK : Boolean;
    IndicatorStrPos : Integer;
    SignalPutativeStateStr : String;

  BEGIN
    OK := True;
    ActionCh := ' ';
    Device := 99999;
    SignalPutativeStateStr := 'on';

    IF Length(Signals[S].Signal_RouteLockingNeededArray) <> 0 THEN BEGIN
      { see whether we've pulling the signal off or not }
      IF Signals[S].Signal_Aspect = RedAspect THEN
        SignalPutativeStateStr := 'off';

      IF NOT Signals[S].Signal_FailMsgWritten THEN
        WriteStringArrayToLog(LocoChipStr, 'S', 'LA for S=' + IntToStr(S) + ': ', LockList, 2, 190);

      LockListPos := 0;
      WHILE OK AND (LockListPos <= High(LockList)) DO BEGIN
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
          IF (Copy(LockList[LockListPos], 1, 3) <> 'BS=') AND (Copy(LockList[LockListPos], 1, 3) <> 'SR=') THEN BEGIN
            IF (Copy(LockList[LockListPos], 1, 3) = 'FR=') AND (Pos('>', LockList[LockListPos]) > 0) THEN BEGIN
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
            OR ((ActionCh <> '\') AND (ActionCh <> '=') AND (ActionCh <> '>') AND (ActionCh <> '|') AND (ActionCh <> '.'))
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
                        Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' because P=' + IntToStr(Device) + ' is in an unknown state');
                    END ELSE BEGIN
                      IF ActionCh = '/' THEN BEGIN
                        IF Points[Device].Point_PresentState = Straight THEN BEGIN
                          { the next point - if it should be diverging, and it's straight, it's a failure }
                          OK := False;
                          IF NOT Signals[S].Signal_FailMsgWritten THEN
                            Log(LocoChipStr +  ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' because P=' + IntToStr(Device) + ' is straight');
                        END;
                      END ELSE BEGIN
                        { ActionCh = '-' }
                        IF Points[Device].Point_PresentState = Diverging THEN BEGIN
                          { the next point - if it should be straight, and it's diverging, it's a failure }
                          OK := False;
                          IF NOT Signals[S].Signal_FailMsgWritten THEN
                            Log(LocoChipStr + ' S Cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' because P=' + IntToStr(Device) + ' is diverging');
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
                  { track circuit unoccupied - unless it's the one next the signal }
                  IF (TrackCircuits[Device].TC_OccupationState <> TCUnoccupied) AND (Lines[Signals[S].Signal_AdjacentLine].Line_TC <> Device) THEN BEGIN
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
      IF NOT OK AND ShowError AND NOT InAutoMode THEN
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
  NewAspect : AspectType;
  NextBufferStop : Integer;
  NextSignal : Integer;
  OtherS : Integer;
  RouteLockingArray : IntegerArrayType;
  SemaphoreDistantHomesArrayPos : Integer;
  SemaphoreDistantSignalOffFound : Boolean;
  SemaphoreHomeSignalOnFound : Boolean;
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
    IF InLockingMode
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
      IF InLockingMode AND SignalIsLockedByOppositePassingLoopSignal(S, OtherS) THEN BEGIN
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
        IF InLockingMode AND SignalIsLockedByUser(S) AND (Route <> UnknownRoute) THEN BEGIN
          OK := False;
          IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
            Log(LocoChipStr + ' R R=' + IntToStr(Route) +
              IfThen(SubRoute <> NoSubRoute, IntToStr(SubRoute)) + ' cannot set S=' + IntToStr(S) + ' ' + SignalPutativeStateStr + ' as it is locked by user');
            Signals[S].Signal_FailMsgWritten := True;
            Forbid;
          END;
        END ELSE BEGIN
          { Check that both the signals and the track-circuit occupation resetting them, if locked, are locked by the same route }
          IF InLockingMode AND (ResetTC <> UnknownTrackCircuit) THEN BEGIN
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

            IF (Signals[S].Signal_IndicatorState = NoIndicatorLit) AND NOT Signals[S].Signal_ApproachLocked THEN BEGIN
              { need to see if a signal is approach locked here - if it is, the indicator will not (yet) be set }
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

              { We need to deal with semaphore distants here, as the conventional locking for home signals doesn't apply }
              IF Signals[S].Signal_Type = SemaphoreDistant THEN BEGIN
                { See if the signals that control the distant are off - if not then it's locked }
                SemaphoreDistantHomesArrayPos := 0;
                SemaphoreHomeSignalOnFound := False;
                WHILE (SemaphoreDistantHomesArrayPos <= High(Signals[S].Signal_SemaphoreDistantHomesArray)) AND NOT SemaphoreHomeSignalOnFound DO BEGIN
                  IF Signals[Signals[S].Signal_SemaphoreDistantHomesArray[SemaphoreDistantHomesArrayPos]].Signal_Aspect = RedAspect THEN
                    SemaphoreHomeSignalOnFound := True
                  ELSE
                    Inc(SemaphoreDistantHomesArrayPos);
                END; {WHILE}

                IF SemaphoreHomeSignalOnFound THEN BEGIN
                  OK := False;
                  DrawFailure(Signals[S].Signal_SemaphoreDistantHomesArray[SemaphoreDistantHomesArrayPos], '=');
                  IF InLockingMode THEN
                    Log(LocoChipStr + ' S Cannot pull off semaphore distant S=' + IntToStr(S)
                                    + ' as semaphore home signal S=' + IntToStr(Signals[S].Signal_SemaphoreDistantHomesArray[SemaphoreDistantHomesArrayPos]) + ' is on');
                  Signals[S].Signal_FailMsgWritten := True;
                END ELSE BEGIN
                  { we now have to lock the home signals that are off, which are then locked by the semaphore distant }
                  SemaphoreDistantHomesArrayPos := 0;
                  WHILE (SemaphoreDistantHomesArrayPos <= High(Signals[S].Signal_SemaphoreDistantHomesArray)) DO BEGIN
                    Signals[Signals[S].Signal_SemaphoreDistantHomesArray[SemaphoreDistantHomesArrayPos]].Signal_LockedBySemaphoreDistant := True;
                    Inc(SemaphoreDistantHomesArrayPos);
                  END; {WHILE}
                END;
              END ELSE BEGIN
                { We need to find the next signal - DraftRouteArray will contain the data from which the signal locking array is created }
                IF FindNextSignalOrBufferStop(S, NextSignal, NextBufferStop, IndicatorToBeSet, LinesNotAvailableStr, DraftRouteArray) THEN BEGIN
                  CreateLockingArrayFromDraftRouteArray(LocoChipStr, DraftRouteArray, Signals[S].Signal_RouteLockingNeededArray);
                  Signals[S].Signal_ResettingTC := GetResettingTrackCircuit(LocoChipStr, S, NOT SuppressMessage);
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
              END;
            END ELSE BEGIN
              IF Signals[S].Signal_IndicatorState = QueryIndicatorLit THEN BEGIN
                { we need to set a specific route up, not just look for the next signal }
                IF NOT Signals[S].Signal_FailMsgWritten THEN
                  Log(LocoChipStr + ' S Finding a route for theatre indicator for S=' + IntToStr(S));

                { DraftRouteArray will also contain the data from which the signal locking array is created }
                FindRouteFromLineAToLineB(LocoChipStr, UnknownJourney, S, Signals[S].Signal_AdjacentLine, PlatformOrFiddleyardLine, Signals[S].Signal_Direction, TrainType,
                                          UnknownTrainLength, NOT EmergencyRouteing, NOT IncludeOutOfUseLines, DraftRouteArray, LinesNotAvailableStr, ErrorMsg, OK);
                IF NOT OK THEN BEGIN
                  { try to find a route with emergency routeing }
                  FindRouteFromLineAToLineB(LocoChipStr, UnknownJourney, S, Signals[S].Signal_AdjacentLine, PlatformOrFiddleyardLine, Signals[S].Signal_Direction, TrainType,
                                            UnknownTrainLength, EmergencyRouteing, NOT IncludeOutOfUseLines, DraftRouteArray, LinesNotAvailableStr, ErrorMsg, OK);
                  IF NOT OK AND NOT FindARouteFailMsgWritten THEN BEGIN
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
                    CreateLockingArrayFromDraftRouteArray(LocoChipStr, DraftRouteArray, Signals[S].Signal_RouteLockingNeededArray);

                  Signals[S].Signal_ResettingTC := GetResettingTrackCircuit(LocoChipStr, S, NOT SuppressMessage);
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

            SemaphoreDistantSignalOffFound := False;
            IF Signals[S].Signal_LockedBySemaphoreDistant THEN BEGIN
              IF Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_Aspect <> RedAspect THEN
                SemaphoreDistantSignalOffFound := True
              ELSE
                Signals[S].Signal_LockedBySemaphoreDistant := False;
            END;

            IF SemaphoreDistantSignalOffFound THEN BEGIN
              OK := False;
              DrawFailure(Signals[S].Signal_SemaphoreDistantLocking, '\');
              Log(LocoChipStr + ' S Cannot put on semaphore home S=' + IntToStr(S)
                              + ' as distant signal S=' + IntToStr(Signals[S].Signal_SemaphoreDistantLocking) + ' is off');
              Signals[S].Signal_FailMsgWritten := True;
            END;

            IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
              IF NOT User THEN
                DebugStr := 'Setting S=' + IntToStr(S) + ' on'
              ELSE
                DebugStr := 'User setting S=' + IntToStr(S) + ' on';
              IF Route <> NoRoute THEN
                DebugStr := DebugStr + ' for R=' + IntToStr(Route);
              Log(LocoChipStr + ' S ' + DebugStr);
            END;

            IF OK THEN BEGIN
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
          END;

          IF OK AND InLockingMode THEN BEGIN
            { If there's nothing in the locking array, it's ok, otherwise test the locking }
            OK := SignalLockingOK(LocoChipStr, S, Signals[S].Signal_RouteLockingNeededArray, ShowError);
            IF OK AND (Signals[S].Signal_Aspect = RedAspect) THEN BEGIN
              Signals[S].Signal_StateChanged := True;
              IF Route = UnknownRoute THEN
                LockSignalByUser(S)
              ELSE
                LockSignalByRoute(LocoChipStr, S, Route, NOT WriteMessage);
            END;
          END;
        END;

        { Check that the previous signal is not a theatre in the process of being set up - a bit esoteric, this check, but necessary, as the previous signal could not
          otherwise be completely pulled off
        }
        IF OK AND InLockingMode THEN BEGIN
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
            SetIndicator(LocoChipStr, S, NoIndicatorLit, '', Route, User);
            IF NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
              IF NOT User THEN
                Log(LocoChipStr + ' S Turning off theatre query indication for S=' + IntToStr(S))
              ELSE
                Log(LocoChipStr + ' S User turning off theatre query indication for S=' + IntToStr(S));
            END;
          END;

          { and unlock any points we previously locked, either because the signal setting has failed, or because we're unsetting it }
          IF NOT OK AND NOT Signals[S].Signal_FailMsgWritten THEN BEGIN
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

        IF NOT OK AND InLockingMode THEN BEGIN
          SetLength(Signals[S].Signal_RouteLockingNeededArray, 0);
          IF (Route <> UnknownRoute) AND Routes_RouteSettingsInProgress[Route] THEN BEGIN
            Signals[S].Signal_StateChanged := False;
            Forbid;
          END;
        END ELSE BEGIN
          { If we're route-setting, and approach control mode has been set, store the signals and don't set them yet }
          IF (Route <> UnknownRoute) AND Routes_RouteSettingsInProgress[Route] AND Routes_ApproachControlsSet[Route] THEN BEGIN
            Log(LocoChipStr + ' R ' + SettingString + ' added to approach setting signals list for R=' + IntToStr(Route) + ' and not yet set off');
            AppendToStringArray(Routes_ApproachControlSignalsWaitingToBeSet[Route], SettingString);
            Signals[S].Signal_ApproachLocked := True;
            Signals[S].Signal_PostColour := clAqua;
            DrawSignalPost(S);
            WriteStringArrayToLog(LocoChipToStr(Routes_LocoChips[Route]), 'R', 'Signals held by approach control for R=' + IntToStr(Route) + ':',
                                                                Routes_ApproachControlSignalsWaitingToBeSet[Route]);
          END ELSE BEGIN
            { Either deal with route and theatre setting ... }
            IF NewIndicatorState <> NoIndicatorLit THEN BEGIN
              IF IndicatorToBeSet THEN
                SetIndicator(LocoChipStr, S, NewIndicatorState, TempTheatreIndicatorString, Route, user)
              ELSE
                SetIndicator(LocoChipStr, S, NoIndicatorLit, '', Route, User);
            { ... or with query theatre indicator setting ... }
            END ELSE BEGIN
              IF (Signals[S].Signal_IndicatorState = QueryIndicatorLit) THEN BEGIN
                SetIndicator(LocoChipStr, S, NewIndicatorState, TempTheatreIndicatorString, Route, user)
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

                SetSignal(LocoChipStr, S, NewAspect, NOT LogSignalData, NOT ForceAWrite);
                IF Route <> NoRoute THEN
                  DebugStr := DebugStr + ' for R=' + IntToStr(Route);
                Log(LocoChipStr + ' S ' + DebugStr);

                { reset several things }
                Signals[S].Signal_FailMsgWritten := False;
                SetLength(Signals[S].Signal_RouteLockingNeededArray, 0);

                IF (Signals[S].Signal_PreviousSignal1 <> UnknownSignal) AND (Signals[S].Signal_Type <> SemaphoreDistant) THEN
                  SetPreviousSignals(LocoChipStr, S);

                { If the signal is being reset to on, reset the other data too }
                IF NewAspect = RedAspect THEN BEGIN
                  IF Signals[S].Signal_IndicatorState <> NoIndicatorLit THEN
                    SetIndicator(LocoChipStr, S, NoIndicatorLit, '', Route, user);
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

PROCEDURE PullSignal{1}(LocoChipStr : String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        SettingString : String; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }
CONST
  ResetTC = UnknownTrackCircuit;

BEGIN
  PullSignalMainProcedure(LocoChipStr, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, SettingString, ResetTC, UnknownTrainType, User, OK);
END; { PullSignal-1 }

PROCEDURE PullSignal{2}(LocoChipStr : String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal }
CONST
  NoSettingString = '';
  ResetTC = UnknownTrackCircuit;

BEGIN
  PullSignalMainProcedure(LocoChipStr, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, NoSettingString, ResetTC, TrainTypeForRouteing, User, OK);
END; { PullSignal-2 }

PROCEDURE PullSignal{3}(LocoChipStr : String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer; ResetTC : Integer;
                        User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal. This version is only used by signals resetting track circuits, which can happen even if the signal is locked by a route }
CONST
  NoSettingString = '';

BEGIN
  PullSignalMainProcedure(LocoChipStr, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, NoSettingString, ResetTC, UnknownTrainType, User, OK);
END; { PullSignal-3 }

PROCEDURE PullSignal{4}(LocoChipStr : String; S : Integer; NewIndicatorState : IndicatorStateType; Route, SubRoute : Integer; PlatformOrFiddleyardLine : Integer;
                        SettingString : String; TrainTypeForRouteing : TypeOfTrainType; User : Boolean; OUT OK : Boolean); Overload;
{ Changes the state of a signal if legal; includes the original setting string for saving if necessary }
CONST
  ResetTC = UnknownTrackCircuit;

BEGIN
  PullSignalMainProcedure(LocoChipStr, S, NewIndicatorState, Route, SubRoute, PlatformOrFiddleyardLine, SettingString, ResetTC, TrainTypeForRouteing, User, OK);
END; { PullSignal-4 }

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
      WHILE (LockCount < Length(Signal_LockedArray)) AND (Result = False) DO BEGIN
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
  I : Integer;
  OtherS : Integer;
  Route : Integer;
  RoutePos : Integer;
  SArrayCount : Integer;

BEGIN
  LockingMsg := '';

  IF Length(Signals[S].Signal_LockedArray) > 0 THEN BEGIN
    FOR SArrayCount := 0 TO High(Signals[S].Signal_LockedArray) DO BEGIN
      RoutePos := Pos('R', Signals[S].Signal_LockedArray[SArrayCount]);
      IF RoutePos > 0 THEN BEGIN
        Route := ExtractRouteFromString(Signals[S].Signal_LockedArray[SArrayCount]);
        IF (Route = UnknownRoute) OR (Length(Routes_LocoChips) = 0) THEN
          LockingMsg := ' ' + Signals[S].Signal_LockedArray[SArrayCount]
        ELSE
          LockingMsg := ' ' + Signals[S].Signal_LockedArray[SArrayCount] + ' (' + LocoChipToStr(Routes_LocoChips[Route]) + ')';
      END ELSE
        IF Pos('USER', Signals[S].Signal_LockedArray[SArrayCount]) > 0 THEN
          LockingMsg := ' ' + Signals[S].Signal_LockedArray[SArrayCount]
    END;
  END;

  IF Signals[S].Signal_ApproachLocked THEN
    LockingMsg := LockingMsg + ' AC';

  IF Signals[S].Signal_Type = SemaphoreDistant THEN BEGIN
    FOR I := 0 TO High(Signals[S].Signal_SemaphoreDistantHomesArray) DO BEGIN
      IF Signals[Signals[S].Signal_SemaphoreDistantHomesArray[I]].Signal_Aspect = RedAspect THEN
        LockingMsg := LockingMsg + ' S' + IntToStr(Signals[S].Signal_SemaphoreDistantHomesArray[I]);
    END; {FOR}
  END;

  IF Signals[S].Signal_LockedBySemaphoreDistant THEN
    LockingMsg := LockingMsg + ' S' + IntToStr(Signals[S].Signal_SemaphoreDistantLocking);

  IF SignalIsLockedByOppositePassingLoopSignal(S, OtherS) THEN
    LockingMsg := ' OPLS (S' + IntToStr(Others) + ')';

  IF LockingMsg = '' THEN BEGIN
    LockingMsg := 'not locked';
    Result := False;
  END ELSE BEGIN
    LockingMsg := 'locked by' + LockingMsg;
    Result := True;
  END;
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
  WHILE (ArrayCount < High(Signals[S].Signal_LockedArray)) AND NOT RouteFound DO BEGIN
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
  WHILE (ArrayCount < High(Signals[S].Signal_LockedArray)) AND NOT UserLockingFound DO BEGIN
    Inc(ArrayCount);
    IF Pos('USER', Signals[S].Signal_LockedArray[ArrayCount]) > 0 THEN
      UserLockingFound := True;
  END; {WHILE}

  IF UserLockingFound THEN BEGIN
    DeleteElementFromStringArray(Signals[S].Signal_LockedArray, ArrayCount);
    Log('S S=' + IntToStr(S) + ' now unlocked by user ');
  END;
END; { UnlockSignalLockedBySpecificRoute }

END { Signals }.
