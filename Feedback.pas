UNIT Feedback;
{ Note: point change detectors (LR101) must be programmed with all delays set to the minimum of 10 ms (1 in CVs 11-18)

  Converted to non J/K power:
  71, 74, 78, 86, 88, 92, 94, 96-7, 101, 103-5, 108, 111, 114
  66, 71, 73-4, 77-8, 86-7, 88-90, 91-7, 100-101, 103-5, 108, 111, 114 10/3/06
  66-78, 80-2, 84-101, 103-114 11/3/06
  66-114 13/3/06
}

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Lenz, StdCtrls, InitVars, ExtCtrls, MPlayer, DB, ADODB, ScktComp;

TYPE
  TFeedbackWindow = CLASS(TForm)
    FeedbackLabel: TLabel;
    FeedbackMediaPlayer: TMediaPlayer;
    FeedbackWindowTimer: TTimer;
    PROCEDURE FeedbackWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE FeedbackWindowTimerTick(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  FeedbackWindow: TFeedbackWindow;

PROCEDURE DecodeFeedback(FeedbackData : FeedbackRec);
{ Sets trackcircuits on or off from feedback data supplied }

PROCEDURE ExtractDataFromFeedback(Data : FeedbackRec; OUT TCAboveFeedbackUnit : Integer; OUT FeedbackType : TypeOfFeedBackType; OUT Num : Integer);
{ For trackcircuits only, returns the trackcircuit number - otherwise returns other data }

PROCEDURE InitialiseLocoSpeedTiming(LocoChip : Integer);
{ Set up the variables for timing locos to ascertain speed in MPH }

PROCEDURE WriteDataToFeedbackWindow{1}(FeedbackString : String); Overload;
{ Overloaded - this is version 1 (version 2 is not exported) - write text to the feedback window }

IMPLEMENTATION

{$R *.dfm}

USES RailDraw, GetTime, Startup, MiscUtils, Movement, IDGlobal, Input, Locks, DateUtils, mmSystem, LocoDialogue, LocoUtils, Diagrams, Options, Main;

CONST
  DefaultLocoSpeed = 24;
  UnitRef = 'Feedback';

VAR
  DefaultLocoSpeedSet : Boolean = False;
  LocoTimingInProgress : Boolean = False; { indicates in general whether loco timing is happening }
  LocoTimingCircleSpeed : Integer = 0;
  LocoTimingLenzSpeed : Integer = 0;
  LocoTimingStarted : Boolean = False; { indicates whether the specified timing run over designated track circuits has begun }
  LocoTimingStartTime : TDateTime = 0;
  LocoTimingSlowingTime : TDateTime = 0;
  LocoTimingStopTime : TDateTime = 0;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE ExtractDataFromFeedback(Data : FeedbackRec; OUT TCAboveFeedbackUnit : Integer; OUT FeedbackType : TypeOfFeedBackType; OUT Num : Integer);
{ For trackcircuits only, returns the trackcircuit number - otherwise returns other data }
VAR
  F : Integer;
  FeedbackUnitFound : Boolean;
  TC : Integer;
  TCFound : Boolean;

BEGIN
  TRY
    F := 0;
    FeedbackUnitFound := False;
    WHILE (F <= High(FeedbackUnitData))
    AND NOT FeedbackUnitFound
    DO BEGIN
      IF FeedbackUnitData[F].Feedback_Unit = Data.Feedback_Unit THEN
        FeedbackUnitFound := True
      ELSE
        Inc(F);
    END; {WHILE}

    IF NOT FeedbackUnitFound THEN BEGIN
      IF NOT FWPRailWindowInitialised
      AND NOT DiagramsCheckingInProgress
      THEN
        Log('X Data received from unknown feedback unit ' + IntToStr(Data.Feedback_Unit) + ' ignored as feedback data not yet initialised')
      ELSE
        Log('XG Data received from unknown feedback unit ' + IntToStr(Data.Feedback_Unit));
    END ELSE BEGIN
      TCAboveFeedbackUnit := FeedbackUnitData[F].Feedback_TCAboveUnit;
      FeedbackType := FeedbackUnitData[F].Feedback_Type;

      IF Data.Feedback_Input > 0 THEN BEGIN
        { sometimes this routine is called with Input set to 0 just to ascertain if the unit is active }
        IF FeedbackType = TrackCircuitFeedbackDetector THEN BEGIN
          Num := UnknownTC;
          TCFound := False;
          TC := 0;
          WHILE (TC <= High(TrackCircuits))
          AND NOT TCFound
          DO BEGIN
            IF TrackCircuits[TC].TC_FeedbackUnit = Data.Feedback_Unit THEN BEGIN
              IF TrackCircuits[TC].TC_FeedbackInput = Data.Feedback_Input THEN BEGIN
                TCFound := True;
                Num := TC;
              END;
            END;
            Inc(TC);
          END; {WHILE}
        END ELSE
          IF FeedbackType = MixedFeedbackDetectors THEN BEGIN
            IF FeedbackUnitData[F].Feedback_InputTypeArray[Data.Feedback_Input] = PointFeedbackDetector THEN
              FeedbackType := PointFeedbackDetector
            ELSE
              IF FeedbackUnitData[F].Feedback_InputTypeArray[Data.Feedback_Input] = TRSPlungerFeedbackDetector THEN
                FeedbackType := TRSPlungerFeedbackDetector
              ELSE
                IF FeedbackUnitData[F].Feedback_InputTypeArray[Data.Feedback_Input] = LineFeedbackDetector THEN
                  FeedbackType := LineFeedbackDetector
                ELSE
                  IF FeedbackUnitData[F].Feedback_InputTypeArray[Data.Feedback_Input] = TrackCircuitFeedbackDetector THEN BEGIN
                    FeedbackType := TrackCircuitFeedbackDetector;
                    Num := UnknownTC;
                    TCFound := False;
                    TC := 0;
                    WHILE (TC <= High(TrackCircuits))
                    AND NOT TCFound
                    DO BEGIN
                      IF TrackCircuits[TC].TC_FeedbackUnit = Data.Feedback_Unit THEN BEGIN
                        IF TrackCircuits[TC].TC_FeedbackInput = Data.Feedback_Input THEN BEGIN
                          TCFound := True;
                          Num := TC;
                        END;
                      END;
                      Inc(TC);
                    END; {WHILE}
                  END;
          END;
      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ExtractDataFromFeedback: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ExtractDataFromFeedback }

PROCEDURE WriteDataToFeedbackWindow{1}(FeedbackString : String); Overload;
{ Overloaded - this is version 1 - write sundry text to the feedback window }
BEGIN
  IF FeedbackDebuggingMode THEN BEGIN
    FeedbackWindow.Visible := True;
    FeedbackWindow.BringToFront;
    FeedbackWindow.FeedbackLabel.Caption := FeedbackString;
  END;
END; { WriteDataToFeedbackWindow-1 }

PROCEDURE WriteDataToFeedbackWindow{2}(FeedbackData : FeedbackRec; Input : Integer); Overload;
{ Overloaded - this is version 2 - write feedback data to the feedback window }
VAR
  FeedbackString : String;
  I : Integer;
  InputStr : String;

BEGIN
  IF FeedbackDebuggingMode THEN BEGIN
    InputStr := IntToStr(Input);
    WITH FeedbackData DO BEGIN
      CASE Feedback_Type OF
        TrackCircuitFeedbackDetector:
          FeedbackString := ' TC';
        PointFeedbackDetector:
          FeedbackString := ' Point';
        TRSPlungerFeedbackDetector:
          FeedbackString := ' SS';
      END; {CASE}

      IF Input <> UnknownTC THEN
        FeedbackString := FeedbackString + ' ' + InputStr
      ELSE
        FeedbackString := FeedbackString + ' ?';

      IF (Input <> UnknownTC)
      AND (Feedback_Type = TrackCircuitFeedbackDetector)
      THEN
        { TCLineArray[0] is only one of a number of possible lines **** }
        IF Length(TrackCircuits[Input].TC_LineArray) > 0 THEN
          FeedbackString := FeedbackString + ' (' + LineToStr(TrackCircuits[Input].TC_LineArray[0]) + ')'
        ELSE
          FeedbackString := FeedbackString + ' (TCLineArray empty)';

      FeedbackString := FeedbackString + ': ' + IntToStr(Feedback_Unit) + ' (' + IntToStr(Feedback_Input) + ')';

      IF FeedbackData.Feedback_InputOn THEN
        FeedbackString := FeedbackString + '+ '
      ELSE
        FeedbackString := FeedbackString + '- ';

      FeedbackWindow.Visible := True;
      FeedbackWindow.BringToFront;
      FeedbackWindow.FeedbackLabel.Caption := FeedbackString;

      IF ReadOutTCInFull THEN BEGIN
        { we want to know when it's activated and deactivated }
        IF FeedbackData.Feedback_InputOn THEN BEGIN
          IF Length(InputStr) <= 2 THEN
            ReadOut(InputStr)
          ELSE
            FOR I := 1 TO Length(InputStr) DO
              ReadOut(InputStr[I]);
          ReadOut('On');
        END ELSE BEGIN
          IF Length(InputStr) <= 2 THEN
            ReadOut(InputStr)
          ELSE
            FOR I := 1 TO Length(InputStr) DO
              ReadOut(InputStr[I]);
          ReadOut('Off');
        END;
      END ELSE BEGIN
        { read out the number }
        InputStr := IntToStr(Input);
        IF ReadOutTCOnce THEN BEGIN
          { all we want is to know where the TC is, and if its length is not known }
          IF FeedbackData.Feedback_InputOn THEN BEGIN
            IF (Input < 0) OR (Input > High(TrackCircuits)) THEN
              Readout('IsThatRight')
            ELSE
              IF TrackCircuits[Input].TC_LengthInInches <> 0.0 THEN
                ReadOut(InputStr)
              ELSE
                IF Length(InputStr) <= 2 THEN
                  ReadOut(InputStr)
                ELSE
                  FOR I := 1 TO Length(InputStr) DO
                    ReadOut(InputStr[I]);

            IF ReadOutAdjacentSignalNumber THEN BEGIN
              { and maybe the adjoining signal number too }
              IF Length(TrackCircuits[Input].TC_AdjacentSignals) > 0 THEN BEGIN
                ReadOut('With');
                FOR I := 0 TO High(TrackCircuits[Input].TC_AdjacentSignals) DO
                  IF TrackCircuits[Input].TC_AdjacentSignals[I] <> UnknownSignal THEN
                    ReadOut(IntToStr(TrackCircuits[Input].TC_AdjacentSignals[I]));
              END;
            END;
          END;
        END ELSE
          IF ReadOutDecoderNumber THEN BEGIN
            Application.ProcessMessages;
            { or which feedback unit is triggered }
            IF FeedbackData.Feedback_InputOn THEN BEGIN
              ReadOut(IntToStr(Feedback_Unit));
              ReadOut(IntToStr(Feedback_Input));
              ReadOut('On');
            END ELSE BEGIN
              ReadOut(IntToStr(Feedback_Unit));
              ReadOut(IntToStr(Feedback_Input));
              ReadOut('Off');
            END;
          END;
      END;
    END; {WITH}
  END;
END; { WriteDataToFeedbackWindow-2 }

PROCEDURE WriteFeedbackDataToDebugWindow(FeedbackData : FeedbackRec; Num : Integer);
{ If required, write feedback data to the debug window }
VAR
  FeedbackString : String;

BEGIN
  WITH FeedbackData DO BEGIN
    CASE Feedback_Type OF
      TrackCircuitFeedbackDetector:
        FeedbackString := ' TC';
      PointFeedbackDetector:
        FeedbackString := ' Point';
      TRSPlungerFeedbackDetector:
        FeedbackString := ' SS';
    END; {CASE}

    IF Num <> 0 THEN
      FeedbackString := FeedbackString + ' ' + IntToStr(Num)
    ELSE
      FeedbackString := FeedbackString + ' ?';

    FeedbackString := FeedbackString + ': ' + IntToStr(Feedback_Unit) + ' (' + IntToStr(Feedback_Input) + ')';

    IF FeedbackData.Feedback_InputOn THEN
      FeedbackString := FeedbackString + '+ '
    ELSE
      FeedbackString := FeedbackString + '- ';

    Debug('Feedback data = ' + FeedbackString);
  END; {WITH}
END; { WriteFeedbackDataToDebugWindow }

PROCEDURE InitialiseLocoSpeedTiming(LocoChip : Integer);
{ Set up the variables for timing locos to ascertain speed in MPH }
BEGIN
  DefaultLocoSpeedSet := False;
  LocoSpeedTimingMode := True;
  LocoTimingSlowingTime := 0;
  LocoTimingLenzSpeed := GetLenzSpeed(LocoChip, ForceARead);
END; { InitialiseLocoSpeedTiming }

PROCEDURE DecodeFeedback(FeedbackData : FeedbackRec);
{ Sets TrackCircuitFeedbackDetectors on or off from feedback data supplied }
CONST
  ForceRead = True;
  ForceWrite = True;
  Pressed = True;

VAR
  DebugStr : String;
  DecodedFeedbackNum : Integer;
  FeedbackType : TypeOfFeedBackType;
  L : Integer;
  LineFeedbackFound : Boolean;
  LineFound : Boolean;
  LockingFailureString : String;
  LocoChip : Integer;
  LocoTimingSlowingTC : Integer;
  LocoTimingStartTC : Integer;
  LocoTimingStopTC : Integer;
  OK : Boolean;
  P : Integer;
  PointFeedbackFound : Boolean;
  T : Train;
  TC : Integer;
  TCAboveFeedbackUnit : Integer;
  TempMPHSpeed : Real;

  FUNCTION CalculateTrueSpeed(LenzSpeed : Integer; StartingTC, EndingTC : Integer; TCLengthInInches : Real; LocoStartTime, LocoStopTime : TDateTime) : Real;
  { Stop the clock on the previous speed check and calculate the speed. The formula is as follows:
      ScaleFactor := 76
      InchesPerRealMile := 63360
      InchesPerScaleMile := InchesPerRealMile / ScaleFactor
      TCLengthInInches := 90.5
      DistanceInScaleMiles := TCLengthInInches / InchesPerScaleMile
      Elapsed time in hours = Mss / 1000 / 60 / 60
      MilesPerHour := DistanceInScaleMiles / ElapsedTimeInHours
  }
  VAR
    DistanceInScaleMiles : Real;
    ElapsedTimeInHours : Real;
    InchesPerRealMile : Integer;
    InchesPerScaleMile : Real;
    MilesPerHour : Real;
    MilliSeconds : Real;
    ScaleFactor : Integer;

  BEGIN
    ScaleFactor := 76;
    InchesPerRealMile := 63360;
    InchesPerScaleMile := InchesPerRealMile / ScaleFactor;
    DistanceInScaleMiles := TCLengthInInches / InchesPerScaleMile;
    
    MilliSeconds := MilliSecondsBetween(LocoStopTime, LocoStartTime);
    IF MilliSeconds = 0 THEN
      { if a TrackCircuitFeedbackDetector goes on and off with no movement, in zero time }
      Result := 0
    ELSE BEGIN
      ElapsedTimeInHours := MilliSeconds / 1000 / 60 / 60;
      MilesPerHour := DistanceInScaleMiles / ElapsedTimeInHours;
      Debug('TC=' + IntToStr(StartingTC) + ' to ' + IntToStr(EndingTC)
            + ' (' + FloatToStr(MilliSeconds) + 'ms over ' + FloatToStr(TCLengthInInches) + ' inches)'
            + ' Speed = ' + IntToStr(LenzSpeed) + ' : ' + FloatToStr(MilesPerHour) + ' mph');
      Result := MilesPerHour;
    END;
  END; { CalculateTrueSpeed }

  PROCEDURE ProcessSpeedTiming;
  VAR
    I : Integer;
    InterveningTrackCircuitFeedbackDetectorsArray : IntegerArrayType;
    InterveningTrackCircuitFeedbackDetectorsTotalLengthInInches : Real;
    LocoChipStr : String;

  BEGIN
    InterveningTrackCircuitFeedbackDetectorsTotalLengthInInches := 0;

    LocoChip := GetLocoDialogueLocoChip;
    IF LocoChip <> UnknownLocoChip THEN BEGIN
      LocoChipStr := LocoChipToStr(LocoChip);
      LocoTimingSlowingTC := 18;
      LocoTimingStartTC := 20;
      LocoTimingStopTC := 107;

      SetLength(InterveningTrackCircuitFeedbackDetectorsArray, 7);
      InterveningTrackCircuitFeedbackDetectorsArray[0] := 20;
      InterveningTrackCircuitFeedbackDetectorsArray[1] := 145;
      InterveningTrackCircuitFeedbackDetectorsArray[2] := 71;
      InterveningTrackCircuitFeedbackDetectorsArray[3] := 223;
      InterveningTrackCircuitFeedbackDetectorsArray[4] := 224;
      InterveningTrackCircuitFeedbackDetectorsArray[5] := 102;
      InterveningTrackCircuitFeedbackDetectorsArray[6] := 40;

      FOR I := 0 TO 6 DO
        InterveningTrackCircuitFeedbackDetectorsTotalLengthInInches := InterveningTrackCircuitFeedbackDetectorsTotalLengthInInches
                                                                       + TrackCircuits[InterveningTrackCircuitFeedbackDetectorsArray[I]].TC_LengthInInches;

      { First, see if we've been speed testing for too long }
      IF LocoTimingInProgress
      AND (LocoTimingSlowingTime <> 0)
      AND (Time > IncSecond(LocoTimingSlowingTime, LocoTimingTimeBeforeAutoStopInSeconds))
      THEN BEGIN
        SetLenzSpeed(LocoChip, UnknownLocoChip, 0, Up, QuickStop, OK);
        Log(LocoChipStr + ' L Loco speed test concluded as loco timing has taken more than '
                        + IntToStr(LocoTimingTimeBeforeAutoStopInSeconds) + ' seconds');
        Debug('+Loco speed test concluded as loco timing has taken more than ' + IntToStr(LocoTimingTimeBeforeAutoStopInSeconds) + ' seconds');
//        LocoDialogueBox.LocoDialogueSpeedButtons.Position := 0;
        LocoDialogueWindow.LocoDialogueSpeedDisplay.Color := clBtnFace;
        LocoDialogueWindow.LocoDialogueSpeedDisplay.Caption := '0';
        LocoSpeedTimingMode := False;
      END ELSE
        { To time loco down the fast straight, then circle the layout }
        IF (TC = LocoTimingSlowingTC)
        AND NOT LocoTimingStarted
        THEN BEGIN
          LocoTimingInProgress := True;
          LocoTimingSlowingTime := Time;
          SetLenzSpeed(LocoChip, UnknownLocoChip, LocoTimingLenzSpeed, Up, NOT QuickStop, OK);
          LocoDialogueWindow.LocoDialogueSpeedDisplay.Color := clYellow;
//          LocoDialogueWindow.LocoDialogueSpeedButtons.Position := LocoTimingLenzSpeed;
          LocoDialogueWindow.LocoDialogueSpeedDisplay.Caption := IntToStr(LocoTimingLenzSpeed);
          Log(LocoChipStr + ' L At LocoTimingSlowingTC TC=' + IntToStr(TC) + ' speed set to ' + IntToStr(LocoTimingLenzSpeed));
        END ELSE
          IF (TC = LocoTimingStartTC)
          AND NOT LocoTimingStarted
          THEN BEGIN
            LocoTimingStarted := True;
            LocoTimingStartTime := Time;
            Log(LocoChipStr + ' L At LocoTimingStartTC TC=' + IntToStr(TC));
            LocoDialogueWindow.LocoDialogueSpeedDisplay.Color := clLime;
          END ELSE
            IF (TC = LocoTimingStopTC)
            AND LocoTimingStarted
            THEN BEGIN
              Log(LocoChipStr + ' L At LocoTimingStopTC TC=' + IntToStr(TC));
              LocoTimingInProgress := False;
              LocoTimingStarted := False;

              LocoTimingStopTime := Time;
              LocoTimingLenzSpeed := GetLenzSpeed(LocoChip, ForceRead);
              TempMPHSpeed := CalculateTrueSpeed(LocoTimingLenzSpeed, LocoTimingStartTC, LocoTimingStopTC, InterveningTrackCircuitFeedbackDetectorsTotalLengthInInches,
                                                 LocoTimingStartTime, LocoTimingStopTime);

              Log(LocoChipStr + ' * TC=' + IntToStr(LocoTimingStartTC) + ' to TC=' + IntToStr(LocoTimingStopTC) + ': '
                              + IntToStr(MilliSecondsBetween(LocoTimingStopTime, LocoTimingStartTime)) + 'ms, '
                              + FloatToStr(TempMPHSpeed) + ' mph, speed='
                              + IntToStr(LocoTimingLenzSpeed)
                              + ' {NOUNITREF}');

              { Now reduce the speed for the next run }
              Dec(LocoTimingLenzSpeed);
              IF LocoTimingLenzSpeed > -1 THEN
                Log(LocoChipStr + ' LG Speed reduced to ' + IntToStr(LocoTimingLenzSpeed));

              { Store a reasonable speed setting for circling the layout before the next timing run begins }
              IF LocoTimingCircleSpeed = 0 THEN BEGIN
                IF (TempMPHSpeed > 70)
                AND (TempMPHSpeed < 80)
                THEN BEGIN
                  LocoTimingCircleSpeed := LocoTimingLenzSpeed;
                  Log(LocoChipStr + ' L LocoTimingCircleSpeed saved as ' + IntToStr(LocoTimingLenzSpeed));
                END ELSE BEGIN
                  { set a default speed }
                  SetLenzSpeed(LocoChip, UnknownLocoChip, DefaultLocoSpeed, Up, NOT QuickStop, OK);
                  LocoDialogueWindow.LocoDialogueSpeedDisplay.Color := clRed;
//                  LocoDialogueWindow.LocoDialogueSpeedButtons.Position := DefaultLocoSpeed;
                  LocoDialogueWindow.LocoDialogueSpeedDisplay.Caption := IntToStr(DefaultLocoSpeed);
                  DefaultLocoSpeedSet := True;
                  Log(LocoChipStr + ' L Speed set to ' + IntToStr(DefaultLocoSpeed));
                END;
              END ELSE BEGIN
                SetLenzSpeed(LocoChip, UnknownLocoChip, LocoTimingCircleSpeed, Up, NOT QuickStop, OK);
                Log(LocoChipStr + ' L Speed set to LocoTimingCircleSpeed (' + IntToStr(LocoTimingCircleSpeed) + ')');
                LocoDialogueWindow.LocoDialogueSpeedDisplay.Color := clRed;
//                LocoDialogueWindow.LocoDialogueSpeedButtons.Position := LocoTimingCircleSpeed;
                LocoDialogueWindow.LocoDialogueSpeedDisplay.Caption := IntToStr(LocoTimingCircleSpeed);
              END;
            END;
    END;
  END; { ProcessSpeedTiming }

BEGIN { DecodeFeedback }
  TRY
    DecodedFeedbackNum := 0;

    { Read in the stored data from all the units, in case some has changed whilst we've been busy elsewhere }
    WITH FeedbackData DO BEGIN
      IF (Feedback_Unit >= FirstFeedbackUnit)
      AND (Feedback_Unit <= LastFeedbackUnit + 1)
      THEN BEGIN
        ExtractDataFromFeedback(FeedbackData, TCAboveFeedbackUnit, FeedbackType, DecodedFeedbackNum);

        DebugStr := 'Feedback ' + IntToStr(Feedback_Unit) + ' Input ' + IntToStr(Feedback_Input);
        IF FeedbackData.Feedback_InputOn THEN
          DebugStr := DebugStr + ' = on'
        ELSE
          DebugStr := DebugStr + ' = off';

        CASE FeedbackType OF
          TrackCircuitFeedbackDetector:
            BEGIN
              TC := DecodedFeedbackNum;
              IF TC <> UnknownTC THEN
                DebugStr := DebugStr + ' (TC=' + IntToStr(TC) + ')' + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
              ELSE
                DebugStr := DebugStr + ' (*** no TC assigned)';
              Log('T ' + DebugStr);
              IF Length(TrackCircuits) > 0 THEN BEGIN
                IF TC <> UnknownTC THEN BEGIN
                  IF Feedback_InputOn THEN BEGIN
                    TrackCircuits[TC].TC_FeedbackOccupation := True;
                    IF RedrawScreen THEN BEGIN
                      RedrawScreen := False;

                      CASE TrackCircuits[TC].TC_PreviousOccupationState OF
                        TCFeedbackOccupation:
                          SetTrackCircuitstate(TC, TCFeedbackOccupation);
                        TCFeedbackOccupationButOutOfUse:
                          SetTrackCircuitstate(TC, TCFeedbackOccupationButOutOfUse);
                        TCLocoOutOfPlaceOccupation:
                          SetTrackCircuitstate(TC, TCLocoOutOfPlaceOccupation);
                        TCMissingOccupation:
                          SetTrackCircuitstate(TC, TCFeedbackOccupation);
                        TCOutOfUseSetByUser:
                          SetTrackCircuitstate(TC, TCOutOfUseSetByUser);
                        TCOutOfUseAsNoFeedbackReceived:
                          SetTrackCircuitstate(TC, TCOutOfUseAsNoFeedbackReceived);
                        TCPermanentFeedbackOccupation:
                          SetTrackCircuitstate(TC, TCPermanentFeedbackOccupation);
                        TCPermanentOccupationSetByUser:
                          SetTrackCircuitstate(TC, TCPermanentOccupationSetByUser);
                        TCPermanentSystemOccupation:
                          SetTrackCircuitstate(TC, TCPermanentSystemOccupation);
                        TCSystemOccupation:
                          SetTrackCircuitstate(TC, TCSystemOccupation);
                        TCUnoccupied:
                          SetTrackCircuitstate(TC, TCFeedbackOccupation);
                      END; {CASE}
                    END ELSE
                      IF NOT ProgramStartup
                      AND (TrackCircuits[TC].TC_LengthInInches <> 0.0)
                      THEN
                        TrackCircuits[TC].TC_OccupationStartTime := Time;

                    { mark it as occupied }
                    IF TrackCircuits[TC].TC_OccupationState <> TCOutOfUseSetByUser THEN
                      SetTrackCircuitstate(TC, TCFeedbackOccupation)
                    ELSE
                      SetTrackCircuitstate(TC, TCFeedbackOccupationButOutOfUse);

                    { Speed testing }
                    IF LocoSpeedTimingMode THEN
                      ProcessSpeedTiming;

                  END ELSE BEGIN
                    { FeedbackInput off }
                    TrackCircuits[TC].TC_MysteriouslyOccupied := False;
                    TrackCircuits[TC].TC_FeedbackOccupation := False;
                    TrackCircuits[TC].TC_OccupationStartTime := 0;

                    IF NOT InAutoMode THEN BEGIN
                      IF TrackCircuits[TC].TC_PreviousOccupationState = TCOutOfUseSetByUser THEN
                        SetTrackCircuitstate(NoLocoChip, TC, TCOutOfUseSetByUser)
                      ELSE
                        SetTrackCircuitstate(TC, TCUnoccupied)
                    END ELSE BEGIN
                      T := NIL;
                      IF TrackCircuits[TC].TC_LocoChip <> UnknownLocoChip THEN
                        T := GetTrainRecord(TrackCircuits[TC].TC_LocoChip);

                      IF TrackCircuits[TC].TC_PreviousOccupationState = TCOutOfUseSetByUser THEN
                        SetTrackCircuitstate(NoLocoChip, TC, TCOutOfUseSetByUser)
                      ELSE BEGIN
                        IF (T <> NIL)
                        AND T^.Train_UseTrailingTrackCircuits
                        THEN
                          SetTrackCircuitstate(TrackCircuits[TC].TC_LocoChip, TC, TCSystemOccupation)
                        ELSE
                          IF TrackCircuits[TC].TC_PreviousOccupationState <> TCSystemOccupation THEN
                            SetTrackCircuitstate(NoLocoChip, TC, TCUnoccupied)
                          ELSE BEGIN
                            { the track circuit has probably been set then unset then set again - which may happen with bad contacts }
                            TrackCircuits[TC].TC_LocoChip := TrackCircuits[TC].TC_PreviousLocoChip;
                            SetTrackCircuitstate(TrackCircuits[TC].TC_LocoChip, TC, TCSystemOccupation);
                          END;
                      END;
                    END;
                  END;
                END;
              END;
              IF InAutoMode THEN
                MoveAllTrains;
            END;

          LineFeedbackDetector:
            BEGIN
              LineFeedbackFound := False;
              WITH FeedbackData DO BEGIN
                L := 0;
                LineFound := False;
                WHILE (L <= High(Lines))
                AND NOT LineFound
                DO BEGIN
                  WITH Lines[L] DO BEGIN
                    IF Line_InUseFeedbackUnit = Feedback_Unit THEN BEGIN
                      IF Line_InUseFeedbackInput = Feedback_Input THEN BEGIN
                        LineFeedbackFound := True;
                        IF Feedback_InputOn THEN BEGIN
                          Line_OutOfUseState := InUse;
                          DebugStr := DebugStr + ' (L=' + LineToStr(L) + ')' + ' [line in use]';
                        END ELSE BEGIN
                          Line_OutOfUseState := OutOfUse;
                          DebugStr := DebugStr + ' (L=' + LineToStr(L) + ')' + ' [line not in use]';
                        END;
                        Log('T ' + DebugStr);
                        IF NOT ProgramStartup THEN
                          DrawMap;
                      END;
                    END;
                  Inc(L);
                  END; {WITH}
                END; {WHILE}
              END; {WITH}
              IF NOT LineFeedbackFound THEN BEGIN
                DebugStr := DebugStr + ' ( *** no line assigned)';
                Log('P ' + DebugStr);
              END;
            END;

          PointFeedbackDetector:
            { Returns the number equivalent to the Lenz point number, and redraws the point that's changed }
            BEGIN
              PointFeedbackFound := False;
              WITH FeedbackData DO BEGIN
                FOR P := 0 TO High(Points) DO BEGIN
                  WITH Points[P] DO BEGIN
                    IF (Point_FeedbackUnit = Feedback_Unit)
                    AND (Point_PresentState <> PointOutOfAction)
                    THEN BEGIN
                      IF Point_FeedbackInput = Feedback_Input THEN BEGIN
                        DecodedFeedbackNum := P;
                        IF Feedback_InputOn THEN BEGIN
                          IF Point_FeedbackOnIsStraight THEN
                            Point_PresentState := Straight
                          ELSE
                            Point_PresentState := Diverging;
                        END ELSE BEGIN
                          IF Point_FeedbackOnIsStraight THEN
                            Point_PresentState := Diverging
                          ELSE
                            Point_PresentState := Straight;
                        END;
                        IF ProgramStartup THEN
                          Point_PreviousState := Point_PresentState;

                        DebugStr := DebugStr + ' (P=' + IntToStr(P) + ' ' + PointStateToStr(Point_PresentState) + ')';
                        Log('P ' + DebugStr);
                        PointFeedbackFound := True;

                        { If the point has changed even though locked, notify user and suspend automode if on }
                        IF ProgramStartup
                        OR RedrawScreen
                        OR NOT PointIsLocked(P, LockingFailureString)
                        OR NOT LockingMode
                        THEN
                          Point_MovedWhenLocked := False
                        ELSE BEGIN
                          IF Point_PresentState <> Point_LockingState THEN BEGIN
                            IF Point_RouteLockedByLocoChip = UnknownLocoChip THEN BEGIN
                              IF NOT Point_MovedWhenLocked THEN BEGIN
                                Point_MovedWhenLocked := True;
                                MakeSound(1);
                                Log('XG <B>Serious error: P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Point_LenzNum) + ')'
                                    + ' [' + DescribeLineNamesForTrackCircuit(Point_TCAtHeel) + '] has changed to ' + PointStateToStr(Point_PresentState)
                                    + ' even though ' + LockingFailureString + ':');
                                Point_MovedWhenLocked := False;
                              END;
                            END ELSE BEGIN
                              T := GetTrainRecord(Point_RouteLockedByLocochip);
                              IF (T^.Train_CurrentStatus <> Suspended)
                              AND (T^.Train_CurrentStatus <> MissingAndSuspended)
                              THEN BEGIN
                                SuspendTrain(T, NOT ByUser);

                                IF NOT Point_MovedWhenLocked THEN BEGIN
                                  Point_MovedWhenLocked := True;
                                  MakeSound(1);
                                  Debug('<B><color=clRed>Serious error: P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Point_LenzNum) + ')'
                                        + ' [' + DescribeLineNamesForTrackCircuit(Point_TCAtHeel) + '] has changed to ' + PointStateToStr(Point_PresentState)
                                        + ' even though ' + LockingFailureString + ':'
                                        + CRLF
                                        + 'loco ' + LocoChipToStr(Point_RouteLockedByLocoChip) + ' has been suspended');
                                  Point_MovedWhenLocked := False;
                                END;
                              END;
                            END;
                          END;
                        END;
                        IF NOT ProgramStartup THEN BEGIN
                          RedrawScreen := True;
                          InvalidateScreen(UnitRef, 'DecodeFeedback');
                          RedrawScreen := False;
                        END;
                      END;
                    END;
                  END; {WITH}
                END;
              END; {WITH}
              IF NOT PointFeedbackFound THEN BEGIN
                DebugStr := DebugStr + ' ( *** no point assigned)';
                Log('P ' + DebugStr);
              END;
            END;

          TRSPlungerFeedbackDetector:
            { First see if we're starting/stopping the process - button held down for five seconds will stop/start it }
            BEGIN
              Log('A ' + DebugStr);
              { If it's a quick button press, it will return here before five seconds are up }
              IF NOT FeedbackData.Feedback_InputOn THEN
                { pressbutton is released }
                StationStartModeSetUpTime := 0
              ELSE BEGIN
                IF StationStartMode THEN BEGIN
                  { button is pressed down }
                  StationStartModeSetUpTime := Time;
  //                CASE DecodedFeedbackNum OF { Need to change the variable name *** }
  //                  998:
  //                    IF NOT MainPlatformPlungers[MainPlatform6A].TRSPlunger_Locked THEN BEGIN
  //                      MainPlatformPlungers[MainPlatform6A].TRSPlunger_Pressed := True;
  //                      Log('A Received plunger data: plunger for ' + LocationToStr(MainPlatform6A) + ' has been pressed');
  //                      DrawTRSPlunger(MainPlatform6A, Pressed);
  //                    END;
  //                  999:
  //                    IF NOT MainPlatformPlungers[MainPlatform6B].TRSPlunger_Locked THEN BEGIN
  //                      MainPlatformPlungers[MainPlatform6B].TRSPlunger_Pressed := True;
  //                      Log('A Received plunger data: plunger for ' + LocationToStr(MainPlatform6B) + ' has been pressed');
  //                      DrawTRSPlunger(MainPlatform6B, Pressed);
  //                    END;
  //                END; {CASE}
                END;
              END;
            END;
        END; {CASE}

        IF FeedbackDebuggingMode
        AND (FeedbackWindow <> NIL) AND NOT ProgramStartup
        THEN
          WriteDataToFeedbackWindow(FeedbackData, DecodedFeedbackNum)
        ELSE
          IF DisplayFeedbackStringsInDebugWindow THEN
            WriteFeedbackDataToDebugWindow(FeedbackData, DecodedFeedbackNum);
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DecodeFeedback: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DecodeFeedback }

PROCEDURE TFeedbackWindow.FeedbackWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    { Exclude most non-alphanumeric keys }
    vk_Shift, vk_Control, vk_Menu { Alt }, vk_Pause, vk_SnapShot { PrtSc }, vk_LWin, vk_RWin, vk_Apps { Windows Applications }, vk_Numlock, vk_Scroll,
    vk_Cancel { Ctrl-Break}, vk_Capital { Caps Lock }:
      { do nothing };
  ELSE {CASE}
    KeyPressedDown(Key, ShiftState);
  END; {CASE}
END; { FeedbackWindowKeyDown }

PROCEDURE TFeedbackWindow.FeedbackWindowTimerTick(Sender: TObject);
BEGIN
  FeedbackWindow.Hide;
  FeedbackWindowTimer.Enabled := False;
END; { FeedbackWindowTimerTick }

INITIALIZATION

END { Feedback }.
