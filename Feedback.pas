UNIT Feedback;
{ Note 1: point change detectors (LR101) must be programmed with all delays set to the minimum of 10 ms (1 in CVs 11-18).
  Note 2: do not set a feedback unit to 65, as if it resets itself, it reverts to 65, and thus causes an error.

  Converted to non J/K power:
  71, 74, 78, 86, 88, 92, 94, 96-7, 101, 103-5, 108, 111, 114
  66, 71, 73-4, 77-8, 86-7, 88-90, 91-7, 100-101, 103-5, 108, 111, 114 10/3/06
  66-78, 80-2, 84-101, 103-114 11/3/06
  66-114 13/3/06

  TC=39 not in use 12/7/14
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

TYPE
  TypeOfFeedback = (TrackCircuitFeedback, TRSPlungerFeedback, PointFeedback, LineFeedback, UnknownFeedbackType);

  TypeOfFeedbackDetector = (TrackCircuitFeedbackDetector, TRSPlungerFeedbackDetector, PointFeedbackDetector, LineFeedbackDetector, MixedFeedbackDetectors,
                            FeedbackDetectorOutOfUse, UnknownFeedbackDetectorType);
VAR
  FeedbackWindow: TFeedbackWindow;

PROCEDURE DecodeFeedback(FeedbackUnit, FeedbackInput : Integer);
{ Works out what to do from when feedback comes in }

PROCEDURE InitialiseLocoSpeedTiming(L : LocoIndex);
{ Set up the variables for timing locos to ascertain speed in MPH }

PROCEDURE NoteOutOfUseFeedbackUnitTrackCircuitsAtStartup;
{ Work out which track circuits are unavailable because we're not getting initial feedback from them }

PROCEDURE ReadInFeedbackDataFromDatabase;
{ Initialise the feedback unit data }

FUNCTION ValidateFeedbackInput(FeedbackInputStr : String; HasFeedback : Boolean; FeedbackUnit : Integer; FeedbackType : TypeOfFeedback; DataNumber : Integer;
                               OUT ErrorMsg : String) : Integer;
{ Check whether the feedback input number is valid }

FUNCTION ValidateFeedbackUnit(FeedbackUnitStr : String; OUT HasFeedback : Boolean; OUT ErrorMsg : String) : Integer;
{ Check whether the feedback unit exists and is valid }

PROCEDURE WriteDataToFeedbackWindow{1}(FeedbackString : String); Overload;
{ Overloaded - this is version 1 (version 2 is not exported) - write text to the feedback window }

TYPE
  FeedbackRec = RECORD
    Feedback_DetectorOutOfUse : Boolean;
    Feedback_InputTypeArray : ARRAY [1..8] OF TypeOfFeedback;
    Feedback_InputOnArray : ARRAY [1..8] OF Boolean;
    Feedback_InputLine : ARRAY [1..8] OF Integer;
    Feedback_InputPoint : ARRAY [1..8] OF Integer;
    Feedback_InputTrackCircuit : ARRAY [1..8] OF Integer;
    Feedback_InputTRSPlunger : ARRAY [1..8] OF Integer;
    Feedback_TCAboveUnit : Integer;
  END;

VAR
  FeedbackUnitRecords : ARRAY OF FeedbackRec;
  FirstFeedbackUnit : Integer = -1;
  LastFeedbackUnit : Integer = 99999;

IMPLEMENTATION

{$R *.dfm}

USES RailDraw, GetTime, Startup, MiscUtils, Movement, IDGlobal, Input, Locks, DateUtils, mmSystem, LocoDialogue, LocoUtils, Diagrams, Options, Main, PointsUnit,
     TrackCircuitsUnit, LinesUnit, Train;

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

FUNCTION ValidateFeedbackUnit(FeedbackUnitStr : String; OUT HasFeedback : Boolean; OUT ErrorMsg : String) : Integer;
{ Check whether the feedback unit exists and is valid }
BEGIN
  ErrorMsg := '';
  Result := 0;
  HasFeedback := False;

  IF (FeedbackUnitStr <> '') AND (FeedbackUnitStr <> '0') THEN BEGIN
    IF NOT TryStrToInt(FeedbackUnitStr, Result) THEN
      ErrorMsg := 'ValidateFeedbackUnit: invalid integer "' + FeedbackUnitStr + '"'
    ELSE
      IF Result < FirstFeedBackUnit THEN
        ErrorMsg := 'ValidateFeedbackUnit: ' + FeedbackUnitStr + ' is less than the first feedback unit (' + IntToStr(FirstFeedBackUnit) + ')'
      ELSE
        IF Result > LastFeedBackUnit THEN
          ErrorMsg := 'ValidateFeedbackUnit: ' + FeedbackUnitStr + ' is greater than the last feedback unit (' + IntToStr(LastFeedBackUnit) + ')'
        ELSE
          HasFeedback := True;
  END;
END; { ValidateFeedbackUnit }

FUNCTION ValidateFeedbackInput(FeedbackInputStr : String; HasFeedback : Boolean; FeedbackUnit : Integer; FeedbackType : TypeOfFeedback; DataNumber : Integer;
                               OUT ErrorMsg : String) : Integer;
{ Check whether the point feedback input number is valid }
VAR
  F : Integer;
  FeedbackInputFound : Boolean;

BEGIN
  ErrorMsg := '';

  IF NOT HasFeedback AND (FeedbackInputStr <> '') THEN
    ErrorMsg := 'ValidateFeedbackInput: a feedback input number has been declared but there is no feedback unit number'
  ELSE
    IF HasFeedback AND (FeedbackInputStr = '') THEN
      ErrorMsg := 'ValidateFeedbackInput: a feedback unit has been declared but there is no feedback input number'
    ELSE
      IF NOT HasFeedback THEN
        Result := 0
      ELSE
        IF FeedbackInputStr = '' THEN
          Result := 0
        ELSE
          IF NOT TryStrToInt(FeedbackInputStr, Result) THEN
            ErrorMsg := 'ValidateFeedbackInput: invalid integer "' + FeedbackInputStr + '"'
          ELSE
            IF (Result < 1) OR (Result > 8) THEN
              ErrorMsg := 'ValidateFeedbackInput:  feedback input number ' + IntToStr(Result) + ' is out of range';

  IF ErrorMsg = '' THEN BEGIN
    IF (FeedbackUnit <> 0) AND (Result <> 0) THEN BEGIN
      { check that the feedback unit data supplied is valid, and assign it to the appropriate feedback record }
      F := 0;
      FeedbackInputFound := False;
      WHILE (F <= High(FeedbackUnitRecords)) AND NOT FeedbackInputFound DO BEGIN
        IF F = FeedbackUnit THEN BEGIN
          IF FeedbackUnitRecords[F].Feedback_InputTypeArray[Result] <> FeedbackType THEN
            ErrorMsg := 'Cannot assign ' + FeedbackTypeToStr(FeedbackType) + ' to an input that is set to receive ' +
                        FeedbackTypeToStr(FeedbackUnitRecords[F].Feedback_InputTypeArray[Result])
          ELSE
            CASE FeedbackType OF
              LineFeedback:
                FeedbackUnitRecords[F].Feedback_InputLine[Result] := DataNumber;
              PointFeedback:
                FeedbackUnitRecords[F].Feedback_InputPoint[Result] := DataNumber;
              TrackCircuitFeedback:
                FeedbackUnitRecords[F].Feedback_InputTrackCircuit[Result] := DataNumber;
              TRSPlungerFeedback:
                FeedbackUnitRecords[F].Feedback_InputTRSPlunger[Result] := DataNumber;
            END; {CASE}
        END;
        Inc(F);
      END; {WHILE}
    END;
  END;
END; { ValidateFeedbackInput }

PROCEDURE ReadInFeedbackDataFromDatabase;
{ Initialise the feedback unit data }
CONST
  StopTimer = True;

VAR
  ErrorMsg : String;
  FieldName : String;
  FeedbackUnit : Integer;
  FirstFeedbackUnitFound : Boolean;
  Input : Integer;
  TempStr : String;

BEGIN
  TRY
    Log('A INITIALISING FEEDBACK UNIT DATA {BLANKLINEBEFORE}');

    WITH InitVarsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + FeedbackDataFilename + '.' + FeedbackDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Feedback database file "' + PathToRailDataFiles + FeedbackDataFilename + '.' + FeedbackDataFilenameSuffix + '" cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInFeedbackDataFromDatabase')
        ELSE
          Exit;
      END;

      FeedbackUnitsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                        + PathToRailDataFiles + FeedbackDataFilename + '.' + FeedbackDataFilenameSuffix
                                                        + ';Persist Security Info=False';
      FeedbackUnitsADOConnection.Connected := True;
      FeedbackUnitsADOTable.Open;
      Log('T Feedback data table and connection opened to initialise the feedback unit data');

      FeedbackUnitsADOTable.Sort := '[Unit] ASC';
      FeedbackUnitsADOTable.First;
      SetLength(FeedbackUnitRecords, 0);

      FirstFeedbackUnit := 99999;
      LastFeedbackUnit := -1;

      FirstFeedbackUnitFound := False;

      WHILE NOT FeedbackUnitsADOTable.EOF DO BEGIN
        WITH FeedbackUnitsADOTable DO BEGIN
          ErrorMsg := '';

          FieldName := 'Unit';
          FeedbackUnit := FieldByName(FieldName).AsInteger;
          IF NOT FirstFeedbackUnitFound THEN BEGIN
            { work out where the real start of the dynamic array is }
            FirstFeedBackUnit := FeedbackUnit;
            FirstFeedbackUnitFound := True;
          END;
          SetLength(FeedbackUnitRecords, FeedbackUnit + 1);
          LastFeedBackUnit := High(FeedbackUnitRecords);

          WITH FeedbackUnitRecords[High(FeedbackUnitRecords)] DO BEGIN
            Feedback_DetectorOutOfUse := False;

            FOR Input := 1 TO 8 DO BEGIN
              Feedback_InputLine[Input] := UnknownLine;
              Feedback_InputPoint[Input] := UnknownPoint;
              Feedback_InputTrackCircuit[Input] := UnknownTrackCircuit;
              Feedback_InputTRSPlunger[Input] := UnknownTRSPlunger;
            END; {FOR}
          END; {WHILE}

          FieldName := 'Input1Type';
          IF FieldByName(FieldName).AsString = '' THEN
            ErrorMsg := 'missing feedback type'
          ELSE BEGIN
            WITH Feedbackunitrecords[high(Feedbackunitrecords)] DO BEGIN
              Feedback_InputTypeArray[1] := StrToFeedbackType(FieldByName(FieldName).AsString);
              IF Feedback_InputTypeArray[1] = UnknownFeedbackType THEN
                ErrorMsg := 'unknown feedback type';
            END; {with}
          END;

          IF ErrorMsg = '' THEN BEGIN
            { propagate the other input types if only the first is occupied }
            Input := 2;
            WHILE (Input <= 8) AND (ErrorMsg = '') DO BEGIN
              FieldName := 'Input' + IntToStr(Input) + 'Type';
              WITH Feedbackunitrecords[high(Feedbackunitrecords)] DO
                IF FieldByName(FieldName).AsString = '' THEN
                  { propagate the other input types if only the first is occupied }
                  Feedback_InputTypeArray[Input] := Feedback_InputTypeArray[1]
                ELSE BEGIN
                  Feedback_InputTypeArray[Input] := StrToFeedbackType(FieldByName(FieldName).AsString);
                  IF Feedback_InputTypeArray[Input] = UnknownFeedbackType THEN
                    ErrorMsg := 'unknown feedback type "' + FieldByName(FieldName).AsString + '" for Input' + IntToStr(Input) + 'Type';
                END;
              Inc(Input);
            END; {WHILE}
          END;

          IF ErrorMsg = '' THEN BEGIN
            FieldName := 'TCAbove';
            TempStr := FieldByName(FieldName).AsString;
            IF TempStr = '' THEN
              Feedbackunitrecords[high(Feedbackunitrecords)].Feedback_TCAboveUnit := UnknownTrackCircuit
            ELSE
              IF NOT TryStrToInt(TempStr, Feedbackunitrecords[high(Feedbackunitrecords)].Feedback_TCAboveUnit) THEN
                ErrorMsg := 'invalid integer ''' + TempStr + ''' in TCAbove field';
          END;

          IF ErrorMsg <> '' THEN BEGIN
            IF MessageDialogueWithDefault('Error in creating Feedback Detector=' + IntToStr(high(Feedbackunitrecords)) + ': '
                                          + '[' + ErrorMsg + ']:'
                                          + CRLF
                                          + 'Do you wish to continue?',
                                          StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo
            THEN
              ShutDownProgram(UnitRef, 'InitialiseFeedback');
          END;
        END; {WITH}
        FeedbackUnitsADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      FeedbackUnitsADOTable.Close;
      FeedbackUnitsADOConnection.Connected := False;
      Log('T Feedback unit data table and connection closed');

      Log('T Reading in feedback data from unit ' + IntToStr(FirstFeedbackUnit) + ' to unit ' + IntToStr(LastFeedbackUnit) + ' from database');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInFeedbackDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadInFeedbackDataFromDatabase }

PROCEDURE NoteOutOfUseFeedbackUnitTrackCircuitsAtStartup;
{ Work out which track circuits are unavailable because we're not getting initial feedback from them }
VAR
  I, J : Integer;

BEGIN
  TRY
    FOR I := FirstFeedbackUnit TO LastFeedbackUnit DO BEGIN
      IF FeedbackUnitRecords[I].Feedback_DetectorOutOfUse THEN BEGIN
        FOR J := 1 TO 8 DO BEGIN
          IF FeedbackUnitRecords[I].Feedback_InputTypeArray[J] = TrackCircuitFeedback THEN
            IF GetTrackCircuitState(FeedbackUnitRecords[I].Feedback_InputTrackCircuit[J]) <> TCOutOfUseSetByUser THEN
              SetTrackCircuitState(FeedbackUnitRecords[I].Feedback_InputTrackCircuit[J], TCOutOfUseAsNoFeedbackReceived, 'no feedback obtained at startup');
        END; {FOR}
      END;
    END; {FOR}
  EXCEPT
    ON E : Exception DO
      Log('EG NoteOutOfUseFeedbackUnitTrackCircuitsAtStartup:' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { NoteOutOfUseFeedbackUnitTrackCircuitsAtStartup }

PROCEDURE WriteDataToFeedbackWindow{1}(FeedbackString : String); Overload;
{ Overloaded - this is version 1 - write sundry text to the feedback window }
BEGIN
  IF InFeedbackDebuggingMode THEN BEGIN
    FeedbackWindow.Visible := True;
    FeedbackWindow.BringToFront;
    FeedbackWindow.FeedbackLabel.Caption := FeedbackString;
  END;
END; { WriteDataToFeedbackWindow-1 }

PROCEDURE WriteDataToFeedbackWindow{2}(FeedbackUnit, FeedbackInput : Integer); Overload;
{ Overloaded - this is version 2 - write feedback data to the feedback window }
VAR
  FeedbackString : String;
  I : Integer;

BEGIN
  IF InFeedbackDebuggingMode THEN BEGIN
    WITH FeedbackUnitRecords[FeedbackUnit] DO BEGIN
      CASE Feedback_InputTypeArray[FeedbackInput] OF
        LineFeedback:
          FeedbackString := ' Line';
        PointFeedback:
          FeedbackString := ' Point';
        TrackCircuitFeedback:
          FeedbackString := ' TC';
        TRSPlungerFeedback:
          FeedbackString := ' SS';
      END; {CASE}

      IF (Feedback_InputTypeArray[FeedbackInput] = TrackCircuitFeedback) AND (Feedback_InputTrackCircuit[FeedbackInput] <> UnknownTrackCircuit) THEN
        { TCLineArray[0] is only one of a number of possible lines **** }
        IF Length(TrackCircuits[Feedback_InputTrackCircuit[FeedbackInput]].TC_LineArray) > 0 THEN
          FeedbackString := FeedbackString + ' (' + LineToStr(TrackCircuits[Feedback_InputTrackCircuit[FeedbackInput]].TC_LineArray[0]) + ')'
        ELSE
          FeedbackString := FeedbackString + ' (TCLineArray empty)';

      FeedbackString := FeedbackString + ': ' + IntToStr(FeedbackUnit) + ' (' + IntToStr(FeedbackInput) + ')';

      IF Feedback_InputOnArray[FeedbackInput] THEN
        FeedbackString := FeedbackString + '+ '
      ELSE
        FeedbackString := FeedbackString + '- ';

      FeedbackWindow.Visible := True;
      FeedbackWindow.BringToFront;
      FeedbackWindow.FeedbackLabel.Caption := FeedbackString;

      IF ReadOutTCInFull THEN BEGIN
        { we want to know when it's activated and deactivated }
        ReadOut(IntToStr(Feedback_InputTrackCircuit[FeedbackInput]));
        IF Feedback_InputOnArray[FeedbackInput] THEN
          ReadOut('On')
        ELSE
          ReadOut('Off');
      END ELSE BEGIN
        { just read out the number }
        ReadOut(IntToStr(Feedback_InputTrackCircuit[FeedbackInput]));
        IF ReadOutAdjacentSignalNumber THEN BEGIN
          IF Length(TrackCircuits[Feedback_InputTrackCircuit[FeedbackInput]].TC_AdjacentSignals) > 0 THEN BEGIN
            ReadOut('With');
            FOR I := 0 TO High(TrackCircuits[Feedback_InputTrackCircuit[FeedbackInput]].TC_AdjacentSignals) DO
              IF TrackCircuits[Feedback_InputTrackCircuit[FeedbackInput]].TC_AdjacentSignals[I] <> UnknownSignal THEN
                ReadOut(IntToStr(TrackCircuits[Feedback_InputTrackCircuit[FeedbackInput]].TC_AdjacentSignals[I]));
          END;
        END;

        IF ReadOutDecoderNumber THEN BEGIN
          Application.ProcessMessages;
          { or which feedback unit is triggered }
          IF Feedback_InputOnArray[FeedbackInput] THEN
            ReadOut(IntToStr(FeedbackUnit));
            ReadOut(IntToStr(FeedbackInput));
            ReadOut('On');
          END ELSE BEGIN
            ReadOut(IntToStr(FeedbackUnit));
            ReadOut(IntToStr(FeedbackInput));
            ReadOut('Off');
          END;
        END;
    END; {WITH}
  END;
END; { WriteDataToFeedbackWindow-2 }

PROCEDURE WriteFeedbackDataToDebugWindow(FeedbackUnit, FeedbackInput : Integer);
{ If required, write feedback data to the debug window }
VAR
  FeedbackString : String;

BEGIN
  WITH FeedbackUnitRecords[FeedbackUnit] DO BEGIN
    CASE Feedback_InputTypeArray[FeedbackInput] OF
      LineFeedback:
        FeedbackString := ' Line';
      PointFeedback:
        FeedbackString := ' Point';
      TrackCircuitFeedback:
        FeedbackString := ' TC';
      TRSPlungerFeedback:
        FeedbackString := ' SS';
    END; {CASE}

    FeedbackString := FeedbackString + ': ' + IntToStr(FeedbackUnit) + ' (' + IntToStr(FeedbackInput) + ')';

    IF Feedback_InputOnArray[FeedbackInput] THEN
      FeedbackString := FeedbackString + '+ '
    ELSE
      FeedbackString := FeedbackString + '- ';

    Debug('Feedback data = ' + FeedbackString);
  END; {WITH}
END; { WriteFeedbackDataToDebugWindow }

PROCEDURE InitialiseLocoSpeedTiming(L : LocoIndex);
{ Set up the variables for timing locos to ascertain speed in MPH }
BEGIN
  DefaultLocoSpeedSet := False;
  SetMode(LocoSpeedTiming, True);
  LocoTimingSlowingTime := 0;
  LocoTimingLenzSpeed := GetLenzSpeed(Locos[L], ForceARead);
END; { InitialiseLocoSpeedTiming }

PROCEDURE DecodeFeedback(FeedbackUnit, FeedbackInput : Integer);
{ Works out what to do from when feedback comes in }
CONST
  ForceRead = True;
  ForceWrite = True;
  Pressed = True;

VAR
  DebugStr : String;
  Line : Integer;
  LockingFailureString : String;
  LocoTimingSlowingTC : Integer;
  LocoTimingStartTC : Integer;
  LocoTimingStopTC : Integer;
  OK : Boolean;
  P : Integer;
  T: TrainIndex;
  TC : Integer;
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
    L : LocoIndex;
    InterveningTrackCircuitFeedbackDetectorsArray : IntegerArrayType;
    InterveningTrackCircuitFeedbackDetectorsTotalLengthInInches : Real;

  BEGIN
    InterveningTrackCircuitFeedbackDetectorsTotalLengthInInches := 0;

    L := GetLocoIndexFromLocoChip(GetLocoDialogueLocoChip);
    IF L = UnknownLocoIndex THEN
      UnknownLocoRecordFound('ProcessSpeedTiming')
    ELSE BEGIN
      WITH Locos[L] DO BEGIN
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
        IF LocoTimingInProgress AND (LocoTimingSlowingTime <> 0) AND (Time > IncSecond(LocoTimingSlowingTime, LocoTimingTimeBeforeAutoStopInSeconds)) THEN BEGIN
          SetLenzSpeedAndDirection(Locos[L], QuickStop, Up, OK);
          Log(Loco_LocoChipStr + ' L Loco speed test concluded as loco timing has taken more than '
                               + IntToStr(LocoTimingTimeBeforeAutoStopInSeconds) + ' seconds');
          Debug('+Loco speed test concluded as loco timing has taken more than ' + IntToStr(LocoTimingTimeBeforeAutoStopInSeconds) + ' seconds');
  //        LocoDialogueBox.LocoDialogueSpeedButtons.Position := 0;
          LocoDialogueWindow.LocoDialogueSpeedDisplay.Color := clBtnFace;
          LocoDialogueWindow.LocoDialogueSpeedDisplay.Caption := '0';
          SetMode(LocoSpeedTiming, False);
        END ELSE
          { To time loco down the fast straight, then circle the layout }
          IF (TC = LocoTimingSlowingTC) AND NOT LocoTimingStarted THEN BEGIN
            LocoTimingInProgress := True;
            LocoTimingSlowingTime := Time;
            SetLenzSpeedAndDirection(Locos[L], LocoTimingLenzSpeed, Up, OK);
            LocoDialogueWindow.LocoDialogueSpeedDisplay.Color := clYellow;
  //          LocoDialogueWindow.LocoDialogueSpeedButtons.Position := LocoTimingLenzSpeed;
            LocoDialogueWindow.LocoDialogueSpeedDisplay.Caption := IntToStr(LocoTimingLenzSpeed);
            Log(Loco_LocoChipStr + ' L At LocoTimingSlowingTC TC=' + IntToStr(TC) + ' speed set to ' + IntToStr(LocoTimingLenzSpeed));
          END ELSE
            IF (TC = LocoTimingStartTC) AND NOT LocoTimingStarted THEN BEGIN
              LocoTimingStarted := True;
              LocoTimingStartTime := Time;
              Log(Loco_LocoChipStr + ' L At LocoTimingStartTC TC=' + IntToStr(TC));
              LocoDialogueWindow.LocoDialogueSpeedDisplay.Color := clLime;
            END ELSE
              IF (TC = LocoTimingStopTC) AND LocoTimingStarted THEN BEGIN
                Log(Loco_LocoChipStr + ' L At LocoTimingStopTC TC=' + IntToStr(TC));
                LocoTimingInProgress := False;
                LocoTimingStarted := False;

                LocoTimingStopTime := Time;
                LocoTimingLenzSpeed := GetLenzSpeed(Locos[L], ForceRead);
                TempMPHSpeed := CalculateTrueSpeed(LocoTimingLenzSpeed, LocoTimingStartTC, LocoTimingStopTC, InterveningTrackCircuitFeedbackDetectorsTotalLengthInInches,
                                                   LocoTimingStartTime, LocoTimingStopTime);

                Log(Loco_LocoChipStr + ' * TC=' + IntToStr(LocoTimingStartTC) + ' to TC=' + IntToStr(LocoTimingStopTC) + ': '
                                     + IntToStr(MilliSecondsBetween(LocoTimingStopTime, LocoTimingStartTime)) + 'ms, '
                                     + FloatToStr(TempMPHSpeed) + ' mph, speed='
                                     + IntToStr(LocoTimingLenzSpeed)
                                     + ' {NOUNITREF}');

                { Now reduce the speed for the next run }
                Dec(LocoTimingLenzSpeed);
                IF LocoTimingLenzSpeed > -1 THEN
                  Log(Loco_LocoChipStr + ' LG Speed reduced to ' + IntToStr(LocoTimingLenzSpeed));

                { Store a reasonable speed setting for circling the layout before the next timing run begins }
                IF LocoTimingCircleSpeed = 0 THEN BEGIN
                  IF (TempMPHSpeed > 70) AND (TempMPHSpeed < 80) THEN BEGIN
                    LocoTimingCircleSpeed := LocoTimingLenzSpeed;
                    Log(Loco_LocoChipStr + ' L LocoTimingCircleSpeed saved as ' + IntToStr(LocoTimingLenzSpeed));
                  END ELSE BEGIN
                    { set a default speed }
                    SetLenzSpeedAndDirection(Locos[L], DefaultLocoSpeed, Up, OK);
                    LocoDialogueWindow.LocoDialogueSpeedDisplay.Color := clRed;
  //                  LocoDialogueWindow.LocoDialogueSpeedButtons.Position := DefaultLocoSpeed;
                    LocoDialogueWindow.LocoDialogueSpeedDisplay.Caption := IntToStr(DefaultLocoSpeed);
                    DefaultLocoSpeedSet := True;
                    Log(Loco_LocoChipStr + ' L Speed set to ' + IntToStr(DefaultLocoSpeed));
                  END;
                END ELSE BEGIN
                  SetLenzSpeedAndDirection(Locos[L], LocoTimingCircleSpeed, Up, OK);
                  Log(Loco_LocoChipStr + ' L Speed set to LocoTimingCircleSpeed (' + IntToStr(LocoTimingCircleSpeed) + ')');
                  LocoDialogueWindow.LocoDialogueSpeedDisplay.Color := clRed;
  //                LocoDialogueWindow.LocoDialogueSpeedButtons.Position := LocoTimingCircleSpeed;
                  LocoDialogueWindow.LocoDialogueSpeedDisplay.Caption := IntToStr(LocoTimingCircleSpeed);
                END;
              END;
      END;
    END;
  END; { ProcessSpeedTiming }

BEGIN { newDecodeFeedback }
  TRY
    { Read in the stored data from all the units, in case some has changed whilst we've been busy elsewhere }
    WITH FeedbackUnitRecords[FeedbackUnit] DO BEGIN
      DebugStr := 'Feedback ' + IntToStr(FeedbackUnit) + ' Input ' + IntToStr(FeedbackInput);

      IF Feedback_InputOnArray[FeedbackInput] THEN
        DebugStr := DebugStr + ' = on'
      ELSE
        DebugStr := DebugStr + ' = off';

      CASE Feedback_InputTypeArray[FeedbackInput] OF
        TrackCircuitFeedback:
          BEGIN
            TC := Feedback_InputTrackCircuit[FeedbackInput];

            IF TC <> UnknownTrackCircuit THEN
              DebugStr := DebugStr + ' (TC=' + IntToStr(TC) + ')' + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
            ELSE
              DebugStr := DebugStr + ' (*** no TC assigned)';
            Log('T ' + DebugStr);
            IF Length(TrackCircuits) > 0 THEN BEGIN
              IF TC <> UnknownTrackCircuit THEN BEGIN
                IF Feedback_InputOnArray[FeedbackInput] THEN BEGIN
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
                    IF NOT ProgramStarting AND (TrackCircuits[TC].TC_LengthInInches <> 0.0) THEN
                      TrackCircuits[TC].TC_OccupationStartTime := Time;

                  { mark it as occupied }
                  IF TrackCircuits[TC].TC_OccupationState <> TCOutOfUseSetByUser THEN
                    SetTrackCircuitstate(TC, TCFeedbackOccupation)
                  ELSE
                    SetTrackCircuitstate(TC, TCFeedbackOccupationButOutOfUse);

                  { Speed testing }
                  IF InLocoSpeedTimingMode THEN
                    ProcessSpeedTiming;

                END ELSE BEGIN
                  { FeedbackInput off }
                  TrackCircuits[TC].TC_MysteriouslyOccupied := False;
                  TrackCircuits[TC].TC_FeedbackOccupation := False;
                  TrackCircuits[TC].TC_OccupationStartTime := 0;

                  IF NOT InAutoMode THEN BEGIN
                    IF TrackCircuits[TC].TC_OccupationState = TCOutOfUseSetByUser THEN
                      SetTrackCircuitstate(UnknownLocoChip, TC, TCOutOfUseSetByUser)
                    ELSE
                      IF TrackCircuits[TC].TC_OccupationState = TCPermanentOccupationSetByUser THEN
                        SetTrackCircuitstate(UnknownLocoChip, TC, TCPermanentOccupationSetByUser)
                      ELSE
                        SetTrackCircuitstate(TC, TCUnoccupied)
                  END ELSE BEGIN
                    IF TrackCircuits[TC].TC_LocoChip <> UnknownLocoChip THEN
                      T := GetTrainIndexFromLocoChip(TrackCircuits[TC].TC_LocoChip)
                    ELSE
                      T := UnknownTrainIndex;

                    IF TrackCircuits[TC].TC_OccupationState = TCOutOfUseSetByUser THEN
                      SetTrackCircuitstate(UnknownLocoChip, TC, TCOutOfUseSetByUser)
                    ELSE
                      IF TrackCircuits[TC].TC_OccupationState = TCPermanentOccupationSetByUser THEN
                        SetTrackCircuitstate(UnknownLocoChip, TC, TCPermanentOccupationSetByUser)
                      ELSE
                        IF (T <= High(Trains)) AND Trains[T].Train_UseTrailingTrackCircuits THEN
                          SetTrackCircuitstate(TrackCircuits[TC].TC_LocoChip, TC, TCSystemOccupation)
                        ELSE
                          IF TrackCircuits[TC].TC_PreviousOccupationState <> TCSystemOccupation THEN
                            SetTrackCircuitstate(UnknownLocoChip, TC, TCUnoccupied)
                          ELSE BEGIN
                            { the track circuit has probably been set then unset then set again - which may happen with bad contacts }
                            TrackCircuits[TC].TC_LocoChip := TrackCircuits[TC].TC_PreviousLocoChip;
                            SetTrackCircuitstate(TrackCircuits[TC].TC_LocoChip, TC, TCSystemOccupation);
                          END;
                  END;
                END;
              END;
            END;
            IF InAutoMode THEN
              MoveAllTrains;
          END;

        LineFeedback:
          BEGIN
            IF Feedback_InputLine[FeedbackInput] = UnknownLine THEN BEGIN
              DebugStr := DebugStr + ' (*** no line assigned ***)';
              Log('L ' + DebugStr);
            END ELSE BEGIN
              Line := Feedback_InputLine[FeedbackInput];

              WITH Lines[Feedback_InputLine[FeedbackInput]] DO BEGIN
                IF Feedback_InputOnArray[FeedbackInput] THEN BEGIN
                  Line_OutOfUseState := InUse;
                  DebugStr := DebugStr + ' (Line=' + LineToStr(Line) + ')' + ' [line in use]';
                END ELSE BEGIN
                  Line_OutOfUseState := OutOfUse;
                  DebugStr := DebugStr + ' (Line=' + LineToStr(Line) + ')' + ' [line not in use]';
                END;
                Log('L ' + DebugStr);

                IF NOT ProgramStarting THEN
                  DrawMap;
              END; {WITH}
            END;
          END;

        PointFeedback:
          { Returns the number equivalent to the Lenz point number, and redraws the point that's changed }
          BEGIN
            IF Feedback_InputPoint[FeedbackInput] = UnknownPoint THEN BEGIN
              DebugStr := DebugStr + ' ( *** no point assigned ***)';
              Log('P ' + DebugStr);
            END ELSE BEGIN
              WITH Points[Feedback_InputPoint[FeedbackInput]] DO BEGIN
                P := Feedback_InputPoint[FeedbackInput];

                IF Feedback_InputOnArray[FeedbackInput] THEN BEGIN
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

                IF ProgramStarting THEN
                  Point_PreviousState := Point_PresentState;

                  DebugStr := DebugStr + ' (P=' + IntToStr(P) + ' ' + PointStateToStr(Point_PresentState) + ')';
                  Log('P ' + DebugStr);

                  { If the point has changed even though locked, notify user and suspend automode if on }
                  IF ProgramStarting
                  OR RedrawScreen
                  OR NOT PointIsLocked(P, LockingFailureString)
                  OR NOT InLockingMode
                  THEN
                    Point_MovedWhenLocked := False
                  ELSE BEGIN
                    IF (Point_LockingState <> PointStateUnknown) AND (Point_PresentState <> Point_LockingState) THEN BEGIN
                      IF Point_LocoChipLockingTheRoute = UnknownLocoChip THEN BEGIN
                        IF NOT Point_MovedWhenLocked THEN BEGIN
                          Point_MovedWhenLocked := True;
                          MakeSound(1);
                          Log('X! Serious error: P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Point_LenzNum) + ')'
                                  + ' [' + DescribeLineNamesForTrackCircuit(Point_TCAtHeel) + '] has changed to ' + PointStateToStr(Point_PresentState)
                                  + ' even though ' + LockingFailureString + ':');
                          Point_MovedWhenLocked := False;
                        END;
                      END ELSE BEGIN
                        T := GetTrainIndexFromLocoChip(Point_LocoChipLockingTheRoute);
                        IF T <> UnknownTrainIndex THEN BEGIN
                          IF (Trains[T].Train_CurrentStatus <> Suspended) AND (Trains[T].Train_CurrentStatus <> MissingAndSuspended) THEN BEGIN
                            SuspendTrain(T, NOT ByUser, 'point ' + PointToStr(P) + ' has moved');

                            IF NOT Point_MovedWhenLocked THEN BEGIN
                              Point_MovedWhenLocked := True;
                              MakeSound(1);
                              Log('X! Serious error: P=' + IntToStr(P) + ' (Lenz=' + IntToStr(Point_LenzNum) + ')'
                                      + ' [' + DescribeLineNamesForTrackCircuit(Point_TCAtHeel) + '] has changed to ' + PointStateToStr(Point_PresentState)
                                      + ' even though ' + LockingFailureString + ':' + 'loco ' + LocoChipToStr(Point_LocoChipLockingTheRoute)
                                      + ' has been suspended');
                              Point_MovedWhenLocked := False;
                            END;
                          END;
                        END;
                      END;
                    END;
                  END;
                  IF NOT ProgramStarting THEN
                    InvalidateScreen(UnitRef, 'DecodeFeedback');
                END; {WITH}
              END;
            END; {WITH}

        TRSPlungerFeedback:
          { First see if we're starting/stopping the process - button held down for five seconds will stop/start it }
          BEGIN
  //          Log('A ' + DebugStr);
  //          { If it's a quick button press, it will return here before five seconds are up }
  //          IF NOT TempFeedbackData.Feedback_InputOn THEN
  //            { pressbutton is released }
  //            StationStartModeSetUpTime := 0
  //          ELSE BEGIN
  //            IF InStationStartMode THEN BEGIN
  //              { button is pressed down }
  //              StationStartModeSetUpTime := Time;
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
  //            END;
  //          END;
  //        END;
        END; {CASE}

//        IF InFeedbackDebuggingMode AND (FeedbackWindow <> NIL) AND NOT ProgramStarting THEN
//          WriteDataToFeedbackWindow(FeedbackUnit, FeedbackInput)
//        ELSE
//          IF DisplayFeedbackStringsInDebugWindow THEN
//            WriteFeedbackDataToDebugWindow(FeedbackUnit, FeedbackInput);
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG DecodeFeedback: ' + E.ClassName + ' error raised, with message: ' + E.Message);
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
