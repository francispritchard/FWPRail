UNIT TrackCircuitsUnit;
{ Controls the various track circuits and track circuit-related things

  Copyright © F.W. Pritchard 2015. All Rights Reserved.

  v0.1  02/02/15 Code extracted mainly from InitVars
}
INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, InitVars, Options, LinesUnit;

TYPE
  TTrackCircuitsUnitForm = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE AddNewRecordToTrackCircuitDatabase;
{ Append a record to the track-circuit database }

PROCEDURE CalculateTCAdjacentSignals;
{ Work out which track circuits are next the signal }

PROCEDURE CalculateTCAdjacentBufferStops;
{ Work out which track circuits are next a buffer stop }

FUNCTION DeleteRecordFromTrackCircuitDatabase(TrackCircuitToDeleteNum : Integer) : Boolean;
{ Remove a record from the track-circuit database }

PROCEDURE InitialiseTrackCircuitVariables(TC : Integer);
{ Initialise all the variables where the data is not read in from the database or added during the edit process }

PROCEDURE ReadInTrackCircuitDataFromDatabase;
{ Initialise the track circuit data which depends on lines being initialised first }

PROCEDURE WriteOutTrackCircuitDataToDatabase;
{ Write out some track-circuit data to the track-circuit data file }

TYPE
  { Track-circuit-related type declarations }
  TrackCircuitStateType = (TCFeedbackOccupation, TCFeedbackOccupationButOutOfUse, TCPermanentFeedbackOccupation, TCPermanentOccupationSetByUser, TCSystemOccupation,
                           TCPermanentSystemOccupation, TCMissingOccupation, TCOutOfUseSetByUser, TCOutOfUseAsNoFeedbackReceived, TCLocoOutOfPlaceOccupation, TCUnoccupied);

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

USES MiscUtils, PointsUnit, SignalsUnit;

CONST
  UnitRef = 'TrackCircuitsUnit';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

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

END { TrackCircuitsUnit }.
