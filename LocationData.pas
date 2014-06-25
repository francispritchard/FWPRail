UNIT LocationData;
{ Unit that does the timetabling scheduling based on the supplied timetable platform/siding availability

  v0.1  21/12/08 Unit created
}

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, InitVars, DB, ADODB, StdCtrls, ExtCtrls, Grids;

TYPE
  TLocationDataWindow = CLASS(TForm)
    LocationDataWindowGrid: TStringGrid;
    PlatformDataADOTable: TADOTable;
    PlatformDataDataSource: TDataSource;
    PlatformDataADOConnection: TADOConnection;
    PROCEDURE LocationDataWindowGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    PROCEDURE LocationDataWindowGridKeyDown(Sender: TObject; VAR Key: Word; Shift: TShiftState);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

FUNCTION AlternativeAreaOrLocationAvailable(T : TrainElement; Journey : Integer; Area : Integer; OldLocation : Integer; OUT NewLocation : Integer;
                                            OUT NewLocationStartTime : TDateTime; PreRouteing, CurrentlyRouteing, OmitLocoTypeRestriction : Boolean;
                                            FindNextAvailableLocation, MayReselectOldLocation : Boolean; VAR ErrorMsg : String; OUT SuccessMsg : String): Boolean;
{ See if there's an adjacent platform/siding free at the required time. If PresentArea is not specified, we want to replace a specified location. ErrorMsg may tell us why
  we are being called.
}
PROCEDURE CheckLocationOccupation(T : TrainElement; JourneyA, JourneyB : Integer; Location : Integer; StartTime, EndTime : TDateTime; OUT OK: Boolean);
{ Checks whether it's ok to insert the data in the array }

PROCEDURE ClearLocationOccupationsForSpecificLocation(Location : Integer);
{ Clear a given location occupation arrays }

PROCEDURE DeleteTrainLocationOccupation(T : TrainElement);
{ Delete the location occupation arrays }

PROCEDURE FindPendingLocations(IsTimetableLoading : Boolean; OUT OK : Boolean);
{ Process those trains where some or all of the locations are waiting to be allocated }

PROCEDURE InsertDataInLocationOccupationArray(T : TrainElement; JourneyA, JourneyB : Integer; Location : Integer; StartTime, EndTime : TDateTime;
                                              LocationState : LocationOccupationStateType; OUT ErrorMsg : String; OUT OK : Boolean);
{ Insert data in the Location Occupation array }

PROCEDURE SetLocationOccupationAllDayState(LocoChip : Integer; Location : Integer; LocationOccupationState : LocationOccupationStateType; ErrorMsg : String; OK : Boolean);
{ Sets a given location's all day occupation state (to out-of-use or permanently occupied }

PROCEDURE SetUpAllLocationOccupationsAbInitio(IsTimetableLoading : Boolean; OUT OK : Boolean);
{ Set up all train and track circuit locations }

PROCEDURE SetUpTrainLocationOccupationsAbInitio(T : TrainElement; OUT OK : Boolean);
{ Set up a given train's locations }

PROCEDURE WriteLocationOccupations(IncludeLocationOccupationStatus, WriteToFile : Boolean);
{ Write the current location occupations to the screen and maybe to a .csv file }

VAR
  LocationDataWindow: TLocationDataWindow;

IMPLEMENTATION

USES MiscUtils, StrUtils, LocoUtils, Diagrams, DateUtils, RailDraw, Locks, Input, GetTime, Lenz, Options;

{$R *.dfm}

CONST
  BoldCh = '@';
  GreyedOutCh = '$';
  UnitRef = 'LocationData';

VAR
  SaveCheckLocationOccupationInsertionMsg : String = '';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE TLocationDataWindow.LocationDataWindowGridKeyDown(Sender: TObject; VAR Key: Word; Shift: TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      LocationDataWindow.Hide;
    vk_Return, vk_Space, vk_F10, Ord('8'), vk_Multiply { the star key }, Ord(222) { '#' key }:
      KeyPressedDown(Key, Shift);
  END; {CASE}
END; { LocationDataWindowGridKeyDown }

PROCEDURE TLocationDataWindow.LocationDataWindowGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
{ We identify which items we want to be highlighted or marked as invalid by prefixing them with a marker; this is necessary as this routine just draws the data it's given
  without knowing where that data has come from).
}
CONST
  CharOffset = 3;

VAR
  CellText: String;
  LeftOffset: Integer;
  WidthOfText: Integer;
  WidthOfCell: Integer;

BEGIN
  WITH LocationDataWindowGrid DO BEGIN
    WITH LocationDataWindowGrid.Canvas DO BEGIN
      Font.Name := RailFontName;
      Font.Size := 10;
      Font.Color := clBlack;
      CellText := Cells[ACol, ARow];

      { Put the top row in bold }
      IF ARow = 0 THEN
        Font.Style := [fsBold]
      ELSE
        { or look for a bold marker }
        IF LeftStr(CellText, 1) = BoldCh THEN BEGIN
          CellText := Copy(CellText, 2);
          Font.Style := [fsBold];
        END ELSE
          IF LeftStr(CellText, 1) = GreyedOutCh THEN BEGIN
            Font.Color := clGrayText;
            CellText := Copy(CellText, 2);
          END;

      WidthOfText := TextWidth(CellText);
      WidthOfCell := ColWidths[ACol];

      IF ACol = 0 THEN
        { do not justify the first column }
        LeftOffset := 10
      ELSE
        LeftOffset := (WidthOfCell - WidthOfText) DIV 2;

      TextRect(Rect, Rect.Left + LeftOffset, Rect.Top, CellText);
      Font.Color := clBlack;
    END; {WITH}
  END; {WITH}
END; { LocationDataWindowGridDrawCell }

PROCEDURE WriteLocationOccupations(IncludeLocationOccupationStatus, WriteToFile : Boolean);
{ Write the current location occupations to the screen and if desired also to a .csv file }
CONST
  AppendToFile = True;

VAR
  CellCount : Integer;
  CellWidth : Integer;
  ErrorMsg : String;
  I, J : Integer;
  Location : Integer;
  LocationOccupationArrayPos : Integer;
  TempOccupationFile1, TempOccupationFile2 : Text;
  TempOccupationFilename1, TempOccupationFilename2 : String;
  TempCh : String;

BEGIN
  TRY
    WITH LocationDataWindow.LocationDataWindowGrid DO BEGIN
      WITH LocationDataWindow.Canvas DO BEGIN
        CellWidth := 0;
        FOR I := 0 TO ColCount DO
          FOR J := 0 TO RowCount DO
            Cells[I, J] := ' ';

        LocationDataWindow.Height := Screen.Height;
        LocationDataWindow.Width := Screen.Width;
        LocationDataWindow.Left := 0;
        LocationDataWindow.Top := 0;

        FOR I := 0 TO RowCount - 1 DO
          RowHeights[I] := LocationDataWindow.Height DIV (RowCount + 5);

        ColCount := 0;
        RowCount := 0;

        { Get the width of the the longest location name }
        Font.Name := RailFontName;
        Font.Size := 11;
        DefaultColWidth := TextWidth('<- 1: 0000 (j-1) 06:30');

        FOR Location := 0 TO High(Locations) DO BEGIN
          IF Locations[Location].Location_RecordInLocationOccupationArray THEN BEGIN
            IF ColWidths[0] < TextWidth(LocationToStr(Location)) THEN
              ColWidths[0] := TextWidth(LocationToStr(Location));
            IF IncludeLocationOccupationStatus THEN BEGIN
              IF ColCount < 1 + (Length(LocationOccupations[Location]) * 3) THEN
                ColCount := 1 + (Length(LocationOccupations[Location]) * 3);
            END ELSE BEGIN
              IF ColCount < 1 + (Length(LocationOccupations[Location]) * 2) THEN
                ColCount := 1 + (Length(LocationOccupations[Location]) * 2);
            END;
          END;
        END;

        Cells[0, 0] := 'Location';
        FOR Location := 0 TO High(Locations) DO BEGIN
          IF Locations[Location].Location_RecordInLocationOccupationArray THEN BEGIN
            CellCount := 0;
            TempCh := '';
            IF (Length(LocationOccupations[Location]) = 0)
            OR ((LocationOccupations[Location][0].LocationOccupation_JourneyA = UnknownJourney)
                 AND (LocationOccupations[Location][0].LocationOccupation_JourneyB = UnknownJourney))
            THEN
              TempCh := GreyedOutCh;
            Cells[0, RowCount] := TempCh + LocationToStr(Location);

            FOR LocationOccupationArrayPos := 0 TO High(LocationOccupations[Location]) DO BEGIN
              WITH LocationOccupations[Location][LocationOccupationArrayPos] DO BEGIN
                Inc(CellCount);
                Cells[CellCount, 0] := 'Start Time';
                IF (LocationOccupation_StartTime = 0)
                AND (LocationOccupation_EndTime <> 0)
                THEN
                    Cells[CellCount, RowCount] := TempCh + '<-' + IntToStr(LocationOccupationArrayPos) + ': ' + LocoChipToStr(LocationOccupation_LocoChip)
                                                         + IfThen(LocationOccupation_JourneyA <> UnknownJourney, ' (j' + IntToStr(LocationOccupation_JourneyA) + ')')
                                                         + ' ' + '00:01'
                ELSE
                  IF LocationOccupation_StartTime <> 0 THEN
                    Cells[CellCount, RowCount] := TempCh + '<-' + IntToStr(LocationOccupationArrayPos) + ': ' + LocoChipToStr(LocationOccupation_LocoChip)
                                                         + IfThen(LocationOccupation_JourneyA <> UnknownJourney, ' (j' + IntToStr(LocationOccupation_JourneyA) + ')')
                                                         + ' ' + TimeToHMStr(LocationOccupation_StartTime);

                IF IncludeLocationOccupationStatus THEN BEGIN
                  Inc(CellCount);
                  Cells[CellCount, 0] := 'Occupation';
                  IF TextWidth(LocationOccupationStateToStr(LocationOccupation_State)) > CellWidth THEN
                    CellWidth := TextWidth(LocationOccupationStateToStr(LocationOccupation_State));
                  ColWidths[CellCount] := CellWidth;
                  Cells[CellCount, RowCount] := TempCh + LocationOccupationStateToStr(LocationOccupation_State);
                END;

                Inc(CellCount);
                Cells[CellCount, 0] := 'End Time';
                IF LocationOccupation_EndTime <> 0 THEN BEGIN
                  Cells[CellCount, RowCount] := TempCh + IntToStr(LocationOccupationArrayPos) + ': ' + LocoChipToStr(LocationOccupation_LocoChip)
                                                       + IfThen(LocationOccupation_JourneyB <> UnknownJourney, ' (j' + IntToStr(LocationOccupation_JourneyB) + ')')
                                                       + ' ' + TimeToHMStr(LocationOccupation_EndTime) + '->';
                END;
              END; {WITH}
            END; {FOR}
            RowCount := RowCount + 1;
          END;
        END; {FOR}

        FOR I := 0 TO RowCount - 1 DO
          RowHeights[I] := LocationDataWindow.Height DIV (RowCount + 5);

      END; {WITH}
    END; {WITH}

    IF WriteToFile THEN BEGIN
      { Write out the current location occupations to a .csv file }
      TempOccupationFilename1 := 'LocationOccupationCount1.csv'; { add to .ini **** }
      OpenOutputFileOK(TempOccupationFile1, TempOccupationFilename1, ErrorMsg, NOT AppendToFile);
      {$I-}
      Rewrite(TempOccupationFile1);
      {$I+}
      IF IOError(TempOccupationFilename1, IOResult, ErrorMsg) THEN
        Debug('Cannot open ' + TempOccupationFilename1 + ': ' + ErrorMsg)
      ELSE BEGIN
        TempOccupationFilename2 := 'LocationOccupationCount2.csv'; { add to .ini **** }
        OpenOutputFileOK(TempOccupationFile2, TempOccupationFilename2, ErrorMsg, NOT AppendToFile);
        {$I-}
        Rewrite(TempOccupationFile2);
        WriteLn(TempOccupationFile2, 'Location,ArrayNo,Loco,Journey,Start,End,Status');
        {$I+}
        IF IOError(TempOccupationFilename2, IOResult, ErrorMsg) THEN
          Debug('Cannot open ' + TempOccupationFilename2 + ': ' + ErrorMsg)
        ELSE BEGIN
          FOR Location := 0 TO High(Locations) DO BEGIN
            IF Locations[Location].Location_RecordInLocationOccupationArray THEN BEGIN
              Write(TempOccupationFile1, LocationToStr(Location) + ',');
              BEGIN
                FOR LocationOccupationArrayPos := 0 TO High(LocationOccupations[Location]) DO BEGIN
                  WITH LocationOccupations[Location][LocationOccupationArrayPos] DO BEGIN
                    IF LocationOccupation_StartTime <> 0 THEN
                      Write(TempOccupationFile1, '<-' + IntToStr(LocationOccupationArrayPos) + ': ' + LocoChipToStr(LocationOccupation_LocoChip)
                                                 + ' (j' + IntToStr(LocationOccupation_JourneyA) + ') ' + TimeToHMStr(LocationOccupation_StartTime) +',');
                    IF LocationOccupation_EndTime <> 0 THEN
                      Write(TempOccupationFile1, IntToStr(LocationOccupationArrayPos) + ': ' + LocoChipToStr(LocationOccupation_LocoChip)
                                                 + ' (j' + IntToStr(LocationOccupation_JourneyB) + ') ' + TimeToHMStr(LocationOccupation_EndTime) + 'to,');
                  END; {WITH}
                END;
              END; {FOR}
              WriteLn(TempOccupationFile1);
            END;
          END; {FOR}
          CloseFile(TempOccupationFile1);
          //Debug('Location occupations (1) written to file ' + TempOccupationFilename);

          FOR Location := 0 TO High(Locations) DO BEGIN
            IF Locations[Location].Location_RecordInLocationOccupationArray THEN BEGIN
              IF Length(LocationOccupations[Location]) > 0 THEN BEGIN
                FOR LocationOccupationArrayPos := 0 TO High(LocationOccupations[Location]) DO BEGIN
                  WITH LocationOccupations[Location][LocationOccupationArrayPos] DO BEGIN
                    Write(TempOccupationFile2, LocationToStr(Location) + ',');
                    Write(TempOccupationFile2, IntToStr(LocationOccupationArrayPos)
                                               + ',' + LocoChipToStr(LocationOccupation_LocoChip)
                                               + ',' + IntToStr(LocationOccupation_JourneyA)
                                               + ',' + TimeToHMStr(LocationOccupation_StartTime)
                                               + ',' + IntToStr(LocationOccupation_JourneyB)
                                               + ',' + TimeToHMStr(LocationOccupation_EndTime)
                                               + ',' + LocationOccupationStateToStr(LocationOccupation_State));
                    WriteLn(TempOccupationFile2);
                  END; {WITH}
                END; {FOR}
              END;
            END;
          END; {FOR}
          CloseFile(TempOccupationFile2);
          //Debug('Location occupations (2) written to file ' + TempOccupationFilename);
        END;
      END;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG WriteLocationOccupations: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteLocationOccupations }

PROCEDURE ClearLocationOccupationsForSpecificLocation(Location : Integer);
{ Clear a given location occupation arrays }
BEGIN
  TRY
    Log('T Clearing location occupation for ' + LocationToStr(Location));
    SetLength(LocationOccupations[Location], 0);
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ClearAllLocationOccupations: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ClearAllLocationOccupations }

PROCEDURE ClearAllLocationOccupations;
{ Clear the location occupation arrays }
VAR
  Location : Integer;

BEGIN
  TRY
    Log('T Clearing all location occupations');
    FOR Location := 0 TO High(LocationOccupations) DO
      { we only record locations where trains might be stationary }
      IF Locations[Location].Location_RecordInLocationOccupationArray THEN
        SetLength(LocationOccupations[Location], 0);
  EXCEPT
    ON E : Exception DO
      Log('EG ClearAllLocationOccupations: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ClearAllLocationOccupations }

PROCEDURE DeleteTrainLocationOccupation(T : TrainElement);
{ Clear the location occupation array for a given train, apart from the first, start of day, occupation }
VAR
  Location : Integer;
  LocationOccupationArrayPos : Integer;

  PROCEDURE DeleteIndividualLocationOccupation(Location : Integer; Position : Integer);
  { Removes the selected data from the various Location Occupation arrays }
  VAR
    I : Integer;

  BEGIN
    { Move all existing elements down one }
    FOR I := Position TO (Length(LocationOccupations[Location]) - 2) DO
      LocationOccupations[Location, I] := LocationOccupations[Location, I + 1];
    SetLength(LocationOccupations[Location], Length(LocationOccupations[Location]) - 1);
  END; { DeleteElementFromLocationOccupationStateArray }

BEGIN
  WITH Trains[T] DO BEGIN
    Log(Train_LocoChipStr + ' D Deleting location occupation');
    FOR Location := 0 TO High(Locations) DO BEGIN
      LocationOccupationArrayPos := 0;
      WHILE LocationOccupationArrayPos <= High(LocationOccupations[Location]) DO BEGIN
        WITH LocationOccupations[Location, LocationOccupationArrayPos] DO BEGIN
          IF LocationOccupation_LocoChip = Train_LocoChip THEN BEGIN
            Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumbers(T, LocationOccupation_JourneyA, LocationOccupation_JourneyB)
                                  + 'Deleting occupation at pos=' + IntToStr(LocationOccupationArrayPos)
                                  + ' of ' + LocationToStr(Location)
                                  + ' at ' + TimeToHMStr(LocationOccupation_StartTime)
                                  + ' to ' + TimeToHMStr(LocationOccupation_EndTime));

            DeleteIndividualLocationOccupation(Location, LocationOccupationArrayPos);
          END ELSE
            Inc(LocationOccupationArrayPos);
        END; {WITH}
      END; {WHILE}
    END; {FOR}
  END; {WITH}
END; { DeleteTrainLocationOccupation }

PROCEDURE CheckLocationOccupation(T : TrainElement; JourneyA, JourneyB : Integer; Location : Integer; StartTime, EndTime : TDateTime; OUT OK: Boolean);
{ Checks whether it's ok to insert the data in the array. If the location has an unknown occupation starting at 00:01, and our occupation starts at 00:01, it's ok, as it's
  presumably us.
}
VAR
  LocationOccupationArrayPos : Integer;
  CheckLocationOccupationInsertionMsg : String;
  ErrorMsg : String;

BEGIN
  WITH Trains[T] DO BEGIN
    IF Location = UnknownLocation THEN BEGIN
      OK := True;
      Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumbers(T, JourneyA, JourneyB)
                            + 'unknown location supplied to CheckLocationOccupation routine');
    END ELSE BEGIN
      OK := True;
      LocationOccupationArrayPos := 0;
      WHILE OK
      AND (LocationOccupationArrayPos <= High(LocationOccupations[Location]))
      DO BEGIN
        WITH LocationOccupations[Location, LocationOccupationArrayPos] DO BEGIN
          IF Locations[Location].Location_RecordInLocationOccupationArray THEN BEGIN
            IF LocationOccupation_State = LocationOutOfUseOccupation THEN BEGIN
              OK := False;
              Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumbers(T, JourneyA, JourneyB)
                                    + LocationToStr(Location, ShortStringType) + ' is out of use')
            END ELSE BEGIN
              IF (LocationOccupation_State = LocationUnknownOccupation)
              AND (LocationOccupation_StartTime = StrToTime('00:00'))
              AND (StartTime = StrToTime('00:00'))
              THEN
                Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumbers(T, JourneyA, JourneyB)
                                      + 'location occupation at ' + LocationToStr(Location, ShortStringType)
                                      + ' from 00:00 to ' + TimeToHMStr(EndTime)
                                      + ' is ok, as the previous occupation was an initial unknown occupation')
              ELSE BEGIN
                { If the existing occupation is by us, then it's ok }
                IF (LocationOccupation_State <> LocationUnoccupied)
                AND (Train_LocoChip <> LocationOccupation_LocoChip)
                THEN BEGIN
                  { are we starting and ending within an existing occupation? }
                  IF StartTime >= LocationOccupation_StartTime THEN BEGIN
                    IF EndTime <= LocationOccupation_EndTime THEN BEGIN
                      OK := False;
                      ErrorMsg := DisplayJourneyNumbers(T, JourneyA, JourneyB)
                                  + LocationToStr(Location, ShortStringType) + ' is not available'
                                  + ' from ' + TimeToHMStr(StartTime) + ' to ' + TimeToHMStr(EndTime)
                                  + ' as the start time and end time are both within an existing occupation: '
                                  + IfThen(LocationOccupation_State = LocationOutOfUseOccupation,
                                           'location is out of use',
                                           IfThen(LocationOccupation_LocoChip = UnknownLocoChip,
                                                  'an unknown loco is at ' + LocationToStr(Location, ShortStringType),
                                                  'loco ' + LocoChipToStr(LocationOccupation_LocoChip)))
                                  + ' from ' + TimeToHMStr(LocationOccupation_StartTime) + ' to ' + TimeToHMStr(LocationOccupation_EndTime);
                      Log(Train_LocoChipStr + ' D ' + ErrorMsg);
                    END;
                  END;

                  { or is just the start time within an existing occupation? }
                  IF OK THEN BEGIN
                    IF StartTime >= LocationOccupation_StartTime THEN BEGIN
                      IF StartTime <= LocationOccupation_EndTime THEN BEGIN
                        OK := False;
                        ErrorMsg := DisplayJourneyNumbers(T, JourneyA, JourneyB)
                                    + LocationToStr(Location, ShortStringType) + ' is not available'
                                    + ' from ' + TimeToHMStr(StartTime) + ' to ' + TimeToHMStr(EndTime)
                                    + ' as the start time is within an existing occupation: '
                                    + IfThen(LocationOccupation_State = LocationOutOfUseOccupation,
                                             'location is out of use',
                                             IfThen(LocationOccupation_LocoChip = UnknownLocoChip,
                                                    'an unknown loco is at ' + LocationToStr(Location, ShortStringType),
                                                    'loco ' + LocoChipToStr(LocationOccupation_LocoChip)))
                                    + ' from ' + TimeToHMStr(LocationOccupation_StartTime) + ' to ' + TimeToHMStr(LocationOccupation_EndTime);
                        Log(Train_LocoChipStr + ' D ' + ErrorMsg);
                      END;
                    END;
                  END;

                  IF OK THEN BEGIN
                    { or is just the end time within an existing occupation? }
                    IF EndTime >= LocationOccupation_StartTime THEN BEGIN
                      IF EndTime <= LocationOccupation_EndTime THEN BEGIN
                        OK := False;
                        ErrorMsg := DisplayJourneyNumbers(T, JourneyA, JourneyB)
                                    + LocationToStr(Location, ShortStringType) + ' is not available'
                                    + ' from ' + TimeToHMStr(StartTime) + ' to ' + TimeToHMStr(EndTime)
                                    + ' as the end time is within an existing occupation: '
                                    + IfThen(LocationOccupation_State = LocationOutOfUseOccupation,
                                             'location is out of use',
                                             IfThen(LocationOccupation_LocoChip = UnknownLocoChip,
                                                    'an unknown loco is at ' + LocationToStr(Location, ShortStringType),
                                                    'loco ' + LocoChipToStr(LocationOccupation_LocoChip)))
                                    + ' from ' + TimeToHMStr(LocationOccupation_StartTime) + ' to ' + TimeToHMStr(LocationOccupation_EndTime);
                        Log(Train_LocoChipStr + ' D ' + ErrorMsg);
                      END;
                    END;
                  END;

                  IF OK THEN BEGIN
                    { or are we starting before and ending after an existing occupation? }
                    IF StartTime <= LocationOccupation_StartTime THEN BEGIN
                      IF EndTime >= LocationOccupation_StartTime THEN BEGIN
                        OK := False;
                        ErrorMsg := DisplayJourneyNumbers(T, JourneyA, JourneyB)
                                    + LocationToStr(Location, ShortStringType) + ' is not available'
                                    + ' from ' + TimeToHMStr(StartTime) + ' to ' + TimeToHMStr(EndTime)
                                    + ' as the start time is before and the end time after an existing occupation: '
                                    + IfThen(LocationOccupation_State = LocationOutOfUseOccupation,
                                             'location is out of use',
                                             IfThen(LocationOccupation_LocoChip = UnknownLocoChip,
                                                    'an unknown loco is at ' + LocationToStr(Location, ShortStringType),
                                                    'loco ' + LocoChipToStr(LocationOccupation_LocoChip)))
                                    + ' from ' + TimeToHMStr(LocationOccupation_StartTime) + ' to ' + TimeToHMStr(LocationOccupation_EndTime);
                        Log(Train_LocoChipStr + ' D ' + ErrorMsg);
                      END;
                    END;
                  END;
                END;
              END;
            END;
          END;
        END; {WITH}
        Inc(LocationOccupationArrayPos);
      END; {WHILE}

      IF OK THEN BEGIN
        CheckLocationOccupationInsertionMsg := DisplayJourneyNumbers(T, JourneyA, JourneyB)
                                               + 'ok to insert at ' + LocationToStr(Location, ShortStringType)
                                               + ' (' + TimeToHMStr(StartTime) + ' to ' + TimeToHMStr(EndTime) + ')';
        IF SaveCheckLocationOccupationInsertionMsg <> CheckLocationOccupationInsertionMsg THEN BEGIN
          Log(Train_LocoChipStr + ' D ' + CheckLocationOccupationInsertionMsg);
          SaveCheckLocationOccupationInsertionMsg := CheckLocationOccupationInsertionMsg;
        END;
      END;
    END;
  END; {WITH}
END; { CheckLocationOccupation }

PROCEDURE CheckJourneyLocations(T : TrainElement; JourneyA : Integer; IsTimetableLoading : Boolean; OUT OK : Boolean);
{ Checks whether a train on a particular journey has somewhere to end up }
CONST
  CurrentlyRouteing = True;
  EmergencyRouteing = True;
  FindNextAvailableLocation = True;
  MayReselectOldLocation = True;
  NewJourney = True;
  OmitLocoTypeRestriction = True;
  PreRouteing = True;
  RebuildRouteArray = True;
  SetUpTimetableArrangements = True;
  TrainExists = True;

VAR
  I : Integer;
  ErrorMsg : String;
  JourneyB : Integer;
  LinesNotAvailableStr : String;
  NewLocation : Integer;
  NewOccupationStartTime : TDateTime;
  RequiredDelayInMinutes : Integer;
  StoppingLocationFound : Boolean;
  SuccessMsg : String;

BEGIN
  WITH Trains[T] DO BEGIN
    OK := True;

    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyA) + 'CHECKING JOURNEY LOCATIONS:');
    IF JourneyA < High(Train_JourneysArray) THEN
      JourneyB := JourneyA + 1
    ELSE
      JourneyB := UnknownJourney;

    WITH Train_JourneysArray[JourneyA] DO BEGIN
      { Only check future arrivals - actual arrivals are there anyway }
      IF (TrainJourney_ActualArrivalTime = 0)
      AND NOT TrainJourney_created
      THEN BEGIN
        IF JourneyA < High(Train_JourneysArray) THEN BEGIN
          IF TrainJourney_StoppingOnArrival THEN BEGIN
            { add a minute at both ends of the visit to allow for entering/leaving the station }
            CheckLocationOccupation(T, JourneyA, JourneyB,
                                    TrainJourney_EndLocation,
                                    IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                               TrainJourney_ActualArrivalTime,
                                               IncMinute(TrainJourney_CurrentArrivalTime, -1)),
                                    IncMinute(Train_JourneysArray[JourneyB].TrainJourney_CurrentDepartureTime, 1),
                                    OK);

            { Also check adjoining locations if the train is occupying them as well }
            IF OK THEN BEGIN
              IF Train_CurrentLengthInInches > Locations[TrainJourney_EndLocation].Location_LengthInInches THEN BEGIN
                IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp <> UnknownLocation THEN BEGIN
                  CheckLocationOccupation(T, JourneyA, JourneyB,
                                          Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp,
                                          IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                                     TrainJourney_ActualArrivalTime,
                                                     IncMinute(TrainJourney_CurrentArrivalTime, -1)),
                                          IncMinute(Train_JourneysArray[JourneyB].TrainJourney_CurrentDepartureTime, 1),
                                          OK);
                END ELSE
                  IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown <> UnknownLocation THEN BEGIN
                    CheckLocationOccupation(T, JourneyA, JourneyB,
                                            Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown,
                                            IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                                       TrainJourney_ActualArrivalTime,
                                                       IncMinute(TrainJourney_CurrentArrivalTime, -1)),
                                            IncMinute(Train_JourneysArray[JourneyB].TrainJourney_CurrentDepartureTime, 1),
                                            OK);
                  END;
              END;
            END;

            { And if we're passing through a platform on our way to an adjoining platform, record that passage to protect the platform against being occupied by something
              else that will obstruct our access, but not if we've already arrived.
            }
            IF OK
            AND (TrainJourney_ActualArrivalTime = 0)
            THEN BEGIN
              IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp <> UnknownLocation THEN BEGIN
                IF TrainJourney_Direction = Down THEN
                  CheckLocationOccupation(T, JourneyA, JourneyB,
                                          Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp,
                                          IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                          IncMinute(TrainJourney_CurrentArrivalTime, 1),
                                          OK);
              END;

              IF OK THEN BEGIN
                IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown <> UnknownLocation THEN BEGIN
                  IF TrainJourney_Direction = Up THEN
                    CheckLocationOccupation(T, JourneyA, JourneyB,
                                            Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown,
                                            IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                            IncMinute(TrainJourney_CurrentArrivalTime, 1),
                                            OK);
                END;
              END;
            END;

            { And if we're going to pass through an adjoining platform on leaving the station, record that passage to protect the
              platform against being occupied by something else that will obstruct our egress
            }
            IF OK THEN BEGIN
              IF JourneyA < High(Train_JourneysArray) THEN BEGIN
                IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp <> UnknownLocation THEN BEGIN
                  IF (TrainJourney_Direction = Up)
                  AND (Train_JourneysArray[JourneyB].TrainJourney_Direction = Up)
                  THEN
                    CheckLocationOccupation(T, JourneyA, JourneyB,
                                            Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp,
                                            IncMinute(Train_JourneysArray[JourneyB].TrainJourney_CurrentDepartureTime, -1),
                                            IncMinute(Train_JourneysArray[JourneyB].TrainJourney_CurrentDepartureTime, 1),
                                            OK);
                END;

                IF OK THEN BEGIN
                  IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown <> UnknownLocation THEN BEGIN
                    IF (TrainJourney_Direction = Down)
                    AND (Train_JourneysArray[JourneyB].TrainJourney_Direction = Down)
                    THEN
                      CheckLocationOccupation(T, JourneyA, JourneyB,
                                              Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown,
                                              IncMinute(Train_JourneysArray[JourneyB].TrainJourney_CurrentDepartureTime, -1),
                                              IncMinute(Train_JourneysArray[JourneyB].TrainJourney_CurrentDepartureTime, 1),
                                              OK);
                  END;
                END;
              END;
            END;
          END;
        END ELSE
          { check the final location }
          IF OK THEN
            CheckLocationOccupation(T, JourneyA, UnknownJourney,
                                    TrainJourney_EndLocation,
                                    TrainJourney_CurrentArrivalTime,
                                    StrToTime('23:59'),
                                    OK);
      END;

      IF NOT OK THEN BEGIN
        Log(Train_LocoChipStr + ' DG J=' + IntToStr(JourneyA)
                              + ': problem with occupation at ' + LocationToStr(TrainJourney_EndLocation) + ' so looking for an alternative location');
        IF AlternativeAreaOrLocationAvailable(T, JourneyA, TrainJourney_EndArea, TrainJourney_EndLocation, NewLocation, NewOccupationStartTime, NOT PreRouteing,
                                              NOT CurrentlyRouteing, NOT OmitLocoTypeRestriction, NOT FindNextAvailableLocation, MayReselectOldLocation, ErrorMsg,
                                              SuccessMsg)
        THEN BEGIN
          OK := True;
          { We now have to amend the end parameters for the journey into the area concerned - we use CreateJourney as that also rebuilds the RouteArray }
          Log(Train_LocoChipStr + ' DG J=' + IntToStr(JourneyA)
                                + ': problem with location occupation at ' + LocationToStr(TrainJourney_EndLocation)
                                + ' solved by substituting ' + LocationToStr(NewLocation));

          WITH Train_JourneysArray[JourneyA] DO
            CreateJourney(T, JourneyA, NOT NewJourney,
                          Locations[Train_JourneysArray[JourneyA].TrainJourney_StartLocation].Location_Area,
                          Locations[NewLocation].Location_Area,
                          TrainJourney_StartLocation, NewLocation,
                          TrainJourney_DiagrammedStartLocation, TrainJourney_DiagrammedEndLocation,
                          TrainJourney_StartLine, UnknownLine,
                          TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime, TrainJourney_CurrentArrivalTime,
                          TrainJourney_Direction,
                          TrainJourney_RouteArray,
                          RebuildRouteArray,
                          TrainJourney_StoppingOnArrival,
                          TrainJourney_NotForPublicUse,
                          NOT EmergencyRouteing,
                          TrainJourney_StartOfRepeatJourney,
                          IsTimetableLoading,
                          ErrorMsg, LinesNotAvailableStr, OK);

          DrawLineInLogFile(Train_LocoChip, 'R', '-', UnitRef);

          { and amend the start parameters for the onward journey (if any) too }
          IF JourneyA < High(Train_JourneysArray) THEN BEGIN
            WITH Train_JourneysArray[JourneyB] DO
              CreateJourney(T, JourneyB, NOT NewJourney,
                            Locations[NewLocation].Location_Area, TrainJourney_EndArea,
                            NewLocation, TrainJourney_EndLocation,
                            TrainJourney_DiagrammedStartLocation, TrainJourney_DiagrammedEndLocation,
                            UnknownLine, TrainJourney_EndLine,
                            TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime, TrainJourney_CurrentArrivalTime,
                            TrainJourney_Direction,
                            TrainJourney_RouteArray,
                            RebuildRouteArray,
                            TrainJourney_StoppingOnArrival,
                            TrainJourney_NotForPublicUse,
                            NOT EmergencyRouteing,
                            TrainJourney_StartOfRepeatJourney,
                            IsTimetableLoading,
                            ErrorMsg, LinesNotAvailableStr, OK);
          END;
          DrawLineInLogFile(Train_LocoChip, 'R', '-', UnitRef);

          IF NOT IsTimetableLoading THEN
            DrawDiagrams(UnitRef, 'CheckJourneyLocations');
        END ELSE BEGIN
          { try finding an available location by altering the train's timings }
          Log(Train_LocoChipStr + ' DG J=' + IntToStr(JourneyA)
                                + ': no alternative area or location available to replace ' + LocationToStr(TrainJourney_EndLocation)
                                + ' so trying retiming the journey');
          IF NOT AlternativeAreaOrLocationAvailable(T, JourneyA, TrainJourney_EndArea, TrainJourney_EndLocation, NewLocation, NewOccupationStartTime, NOT PreRouteing,
                                                    NOT CurrentlyRouteing, OmitLocoTypeRestriction, FindNextAvailableLocation, MayReselectOldLocation, ErrorMsg, SuccessMsg)
          THEN
            SuspendTrain(T, NOT ByUser, ' J=' + IntToStr(JourneyA) + ' no alternative area or location available to replace ' + LocationToStr(TrainJourney_EndLocation))
          ELSE BEGIN
            Log(Train_LocoChipStr + ' D J-' + IntToStr(JourneyA)
                                  + ': problem with occupation at ' + LocationToStr(TrainJourney_EndLocation)
                                  + ' solved by substituting ' + LocationToStr(NewLocation) + ': ' + SuccessMsg);
            OK := True;

            { note: we use the NewMinutesBetween system routine as the system one is known to be buggy }
            RequiredDelayInMinutes := 1 + NewMinutesBetween(TrainJourney_CurrentArrivalTime, NewOccupationStartTime);

            Train_JourneysArray[JourneyA].TrainJourney_EndLocation := NewLocation;
            IF JourneyA < High(Train_JourneysArray) THEN
              Train_JourneysArray[JourneyB].TrainJourney_StartLocation := NewLocation;

            { Now we have to recalculate the departure time from the previous stopping location, and restart the journey location check, as this change may cause problems
              with the previous location.
            }
            IF JourneyA = 0 THEN
              { it's the first journey, so need to alter the initial starting time }
              Train_JourneysArray[JourneyA].TrainJourney_CurrentDepartureTime :=
                                                                          IncMinute(Train_JourneysArray[JourneyA].TrainJourney_CurrentDepartureTime, RequiredDelayInMinutes)
            ELSE BEGIN
              { otherwise step backwards to the previous journey where we stop }
              I := JourneyA;
              StoppingLocationFound := False;
              WHILE (I > 0)
              AND NOT StoppingLocationFound
              DO BEGIN
                Dec(I);
                IF Train_JourneysArray[I].TrainJourney_StoppingonArrival THEN BEGIN
                  Train_JourneysArray[I].TrainJourney_AdditionalRequiredStationWaitInMinutes := RequiredDelayInMinutes;
                  StoppingLocationFound := True;
                END;
              END; {WHILE}
              IF NOT StoppingLocationFound THEN
                { we'll just have to delay our initial starting time }
                Train_JourneysArray[0].TrainJourney_CurrentDepartureTime := IncMinute(Train_JourneysArray[0].TrainJourney_CurrentDepartureTime, RequiredDelayInMinutes);
            END;
            
            RecalculateJourneyTimes(T, 'in substituting ' + LocationToStr(NewLocation) + ' for ' + LocationToStr(TrainJourney_EndLocation));
            IF NOT IsTimetableLoading THEN
              DrawDiagrams(UnitRef, 'CheckJourneyLocations');

            WITH Train_JourneysArray[JourneyA] DO
              CreateJourney(T, JourneyA, NOT NewJourney,
                            Locations[Train_JourneysArray[JourneyA].TrainJourney_StartLocation].Location_Area,
                            Locations[NewLocation].Location_Area,
                            TrainJourney_StartLocation, NewLocation,
                            TrainJourney_DiagrammedStartLocation, NewLocation,
                            TrainJourney_StartLine, UnknownLine,
                            TrainJourney_CurrentDepartureTime, Train_JourneysArray[JourneyA].TrainJourney_DiagrammedDepartureTime,
                            TrainJourney_CurrentArrivalTime,
                            TrainJourney_Direction,
                            TrainJourney_RouteArray,
                            RebuildRouteArray,
                            TrainJourney_StoppingOnArrival,
                            TrainJourney_NotForPublicUse,
                            NOT EmergencyRouteing,
                            TrainJourney_StartOfRepeatJourney,
                            IsTimetableLoading,
                            ErrorMsg, LinesNotAvailableStr, OK);

            DrawLineInLogFile(Train_LocoChip, 'R', '-', UnitRef);

            { and amend the start parameters for the onward journey (if any) too }
            IF JourneyA < High(Train_JourneysArray) THEN BEGIN
              WITH Train_JourneysArray[JourneyB] DO
                CreateJourney(T, JourneyB, NOT NewJourney,
                              Locations[NewLocation].Location_Area, TrainJourney_EndArea,
                              NewLocation, TrainJourney_EndLocation,
                              TrainJourney_DiagrammedStartLocation, NewLocation,
                              UnknownLine, TrainJourney_EndLine,
                              TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime,
                              TrainJourney_CurrentArrivalTime,
                              TrainJourney_Direction,
                              TrainJourney_RouteArray,
                              RebuildRouteArray,
                              TrainJourney_StoppingOnArrival,
                              TrainJourney_NotForPublicUse,
                              NOT EmergencyRouteing,
                              TrainJourney_StartOfRepeatJourney,
                              IsTimetableLoading,
                              ErrorMsg, LinesNotAvailableStr, OK);
              DrawLineInLogFile(Train_LocoChip, 'R', '-', UnitRef);
            END;
          END;
        END;
      END;
    END; {WITH}
  END; {WITH}
END; { CheckJourneyLocations }

PROCEDURE InsertDataInLocationOccupationArray(T : TrainElement; JourneyA, JourneyB : Integer; Location : Integer; StartTime, EndTime : TDateTime;
                                              LocationState : LocationOccupationStateType; OUT ErrorMsg : String; OUT OK : Boolean);
{ Insert data in the Location Occupation array }
VAR
  DebugStr : String;
  I : Integer;
  LocationOccupationArrayPos : Integer;
  NewLocationOccupationRecord : LocationOccupationRec;
  Position : Integer;

BEGIN
  TRY
    WITH Trains[T] DO BEGIN
      OK := True;
      Position := 0;

      IF Location = UnknownLocation THEN
        Log(Train_LocoChipStr + ' X Unknown location in InsertDataInLocationOccupationArray')
      ELSE BEGIN
        { Only record locations where trains might be stationary }
        IF Locations[Location].Location_RecordInLocationOccupationArray THEN BEGIN
          IF Length(LocationOccupations) = 0 THEN
            { add the first entry }
            Position := 0
          ELSE BEGIN
            { work our way through the location occupation array }
            LocationOccupationArrayPos := -1;
            WHILE OK
            AND (LocationOccupationArrayPos < High(LocationOccupations[Location]))
            DO BEGIN
              Inc(LocationOccupationArrayPos);
              { check if the location is already occupied, except when we add out-of-use occupations, which delete all other occupations }
              IF LocationState = LocationOutOfUseOccupation THEN BEGIN
                IF LocationOccupations[Location, LocationOccupationArrayPos].LocationOccupation_State = LocationOutOfUseOccupation THEN
                  { no point reinserting it }
                  Exit;
              END ELSE
                IF (T <= High(Trains))
                AND (Train_LocoChip <> LocationOccupations[Location][LocationOccupationArrayPos].LocationOccupation_LocoChip)
                THEN BEGIN
                  CheckLocationOccupation(T, JourneyA, JourneyB,
                                          Location,
                                          StartTime,
                                          EndTime,
                                          OK);
                  IF NOT OK THEN
                    ErrorMsg := 'Location ' + LocationToStr(Location) + ' is not free between ' + TimeToHMStr(StartTime) + ' and ' + TimeToHMStr(EndTime);
                END;
            END; {WHILE}
          END;

          IF OK AND (Length(LocationOccupations[Location]) > 0) THEN BEGIN
            { subsequent entries }
            OK := False;
            { now to work out where the insertion should be: is it before the first element in the array? }
            IF StartTime < LocationOccupations[Location][0].LocationOccupation_StartTime THEN BEGIN
              Position := 0;
              OK := True;
            END ELSE
              IF StartTime > LocationOccupations[Location][High(LocationOccupations[Location])].LocationOccupation_StartTime THEN BEGIN
                { or after the last element in the array? }
                Position := High(LocationOccupations[Location]) + 1;
                OK := True;
              END ELSE BEGIN
                IF (High(LocationOccupations[Location]) <> 0)
                AND (StartTime = LocationOccupations[Location][High(LocationOccupations[Location])].LocationOccupation_StartTime)
                THEN
                  { if it's the same start time, something is amiss, unless it's the first start time of the day (as an initial holding record may have been added at start
                    up) where we can substitute the new data.
                  }
                  ErrorMsg := 'start time ' + TimeToHMStr(StartTime) + ' is the same as an existing start time for the same loco'
                ELSE
                  IF StartTime = LocationOccupations[Location][High(LocationOccupations[Location])].LocationOccupation_StartTime
                  THEN BEGIN
                    { if it's the same but for the same loco, no point in flagging it as an error }
                    IF NOT SystemOnline
                    OR ((T <= High(Trains))
                        AND (EndTime = LocationOccupations[Location][High(LocationOccupations[Location])].LocationOccupation_EndTime)
                        AND (Train_LocoChip = LocationOccupations[Location][High(LocationOccupations[Location])].LocationOccupation_LocoChip))
                      THEN
                        Exit
                      ELSE
                        ErrorMsg := 'start time ' + TimeToHMStr(StartTime) + ' is the same as an existing start time for '
                                    + LocoChipToStr(LocationOccupations[Location][High(LocationOccupations[Location])].LocationOccupation_LocoChip)
                                    + ' (attempting to replace '
                                    + LocationOccupationStateToStr(LocationOccupations[Location][High(LocationOccupations[Location])].LocationOccupation_State)
                                    + ' with ' + LocationOccupationStateToStr(LocationState) +')';
                  END ELSE BEGIN
                    { so it should be somewhere in the middle }
                    LocationOccupationArrayPos := 0;
                    WHILE LocationOccupationArrayPos < High(LocationOccupations[Location]) DO BEGIN
                      IF (StartTime > LocationOccupations[Location][LocationOccupationArrayPos].LocationOccupation_StartTime)
                      AND (StartTime < LocationOccupations[Location][LocationOccupationArrayPos + 1].LocationOccupation_StartTime)
                      THEN BEGIN
                        Position := LocationOccupationArrayPos + 1;
                        OK := True;
                      END;
                      Inc(LocationOccupationArrayPos);
                    END; {WHILE}
                  END;
              END;
          END;

          IF NOT OK THEN BEGIN
            IF T = 0 THEN BEGIN
              ErrorMsg := IfThen(JourneyA <> UnknownJourney,
                                 'Start journey ' + IntToStr(JourneyA) + ': ')
                          + TimeToHMStr(StartTime) + ' to ' + TimeToHMStr(EndTime) + ' at ' + LocationToStr(Location, ShortStringType)
                          + IfThen(Length(GetTrackCircuitsForLocation(Location)) = 0,
                                   '',
                                   ' (' + DisplayTrackCircuitsForLocation(Location) + ')')
                          + ' data not inserted: '
                          + ErrorMsg;
              Log('E ' + ErrorMsg + ' {INDENT=0} {WRAP=SCREENWIDTH}');
            END ELSE BEGIN
              ErrorMsg := 'Start journey ' + IntToStr(JourneyA) + ': '
                          + TimeToHMStr(StartTime) + ' to ' + TimeToHMStr(EndTime) + ' at ' + LocationToStr(Location, ShortStringType)
                          + IfThen(Length(GetTrackCircuitsForLocation(Location)) = 0,
                                   '',
                                   ' (' + DisplayTrackCircuitsForLocation(Location) + ')')
                          + ' data not inserted: '
                          + ErrorMsg;
              Log(Train_LocoChipStr + ' E ' + ErrorMsg + ' {INDENT=0} {WRAP=SCREENWIDTH}');
            END;
          END ELSE BEGIN
            IF T = 0 THEN BEGIN
              { probably being added before we read in the timetable, so we don't know if we know which locos these are }
              DebugStr := IfThen(JourneyA <> UnknownJourney,
                                 DisplayJourneyNumber(JourneyA) + 'adding ',
                                 'Adding ')
                          + LocationToStr(Location, ShortStringType)
                          + IfThen(Length(GetTrackCircuitsForLocation(Location)) = 0,
                                   '',
                                   ' (' + DisplayTrackCircuitsForLocation(Location) + ')')
                          + ' to LocationOccupations array at pos=0'
                          + ' (' + TimeToHMStr(StartTime) + ' to ' + TimeToHMStr(EndTime)
                          + ') [' + LocationOccupationStateToStr(LocationState)
                          + ']';

              Log('D ' + DebugStr);
            END ELSE BEGIN
              DebugStr := DisplayJourneyNumber(JourneyA) + 'adding '
                          + LocationToStr(Location, ShortStringType)
                          + IfThen(Length(GetTrackCircuitsForLocation(Location)) = 0,
                                   '',
                                   ' (' + DisplayTrackCircuitsForLocation(Location) + ')')
                          + ' to LocationOccupations array at pos=' + IntToStr(Position)
                          + ' (' + TimeToHMStr(StartTime) + ' to ' + TimeToHMStr(EndTime)
                          + ') [' + LocationOccupationStateToStr(LocationState)
                          + ']';

              Log(Train_LocoChipStr + ' D ' + DebugStr);
            END;

            WITH NewLocationOccupationRecord DO BEGIN
              IF T = 0 THEN
                LocationOccupation_LocoChip := UnknownLocoChip
              ELSE
                LocationOccupation_LocoChip := Train_LocoChip;
              LocationOccupation_JourneyA := JourneyA;
              LocationOccupation_JourneyB := JourneyB;
              LocationOccupation_StartTime := StartTime;
              LocationOccupation_EndTime := EndTime;
              LocationOccupation_State := LocationState;
            END; {WITH}

            SetLength(LocationOccupations[Location], Length(LocationOccupations[Location]) + 1);
            { Move all existing elements up one }
            IF Length(LocationOccupations[Location]) = 1 THEN
              LocationOccupations[Location][0] := NewLocationOccupationRecord
            ELSE BEGIN
              FOR I := (Length(LocationOccupations[Location]) - 1) DOWNTO (Position + 1) DO
                LocationOccupations[Location][I] := LocationOccupations[Location][I - 1];
              { and insert the new element }
              LocationOccupations[Location][Position] := NewLocationOccupationRecord;
            END;
          END;
        END;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG InsertDataInLocationOccupationArray: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { InsertDataInLocationOccupationArray }

FUNCTION AlternativeAreaOrLocationAvailable(T : TrainElement; Journey : Integer; Area : Integer; OldLocation : Integer; OUT NewLocation : Integer;
                                            OUT NewLocationStartTime : TDateTime; PreRouteing, CurrentlyRouteing, OmitLocoTypeRestriction : Boolean;
                                            FindNextAvailableLocation, MayReselectOldLocation : Boolean; VAR ErrorMsg : String; OUT SuccessMsg : String): Boolean;
{ See if there's an adjacent platform/siding free at the required time. If PresentArea is not specified, we want to replace a specified location. ErrorMsg may tell us why
  we are being called.
}
TYPE
  PossibleLocationRecType = RECORD
                              PossibleLocation_AdjoiningPlatformOrFiddleyardLocation : Integer;
                              PossibleLocation_AdjoiningPlatformOrFiddleyardRequired : Boolean;
                              PossibleLocation_Available : Boolean;
                              PossibleLocation_DestinationPriorityAreas : IntegerArrayType;
                              PossibleLocation_DirectionPriority : DirectionPriorityType;
                              PossibleLocation_LengthLessThanTrainLength : Boolean;
                              PossibleLocation_LocosPreferred : IntegerArrayType;
                              PossibleLocation_NextAvailableTime : TDateTime;
                              PossibleLocation_PlatformOrFiddleyardLocation : Integer;
                              PossibleLocation_PlatformPriority : Integer;
                              PossibleLocation_ThroughOrStoppingPriority : ThroughOrStoppingPriorityType;
                              PossibleLocation_TrainPriority : TrainPriorityType;
                              PossibleLocation_Weighting : Integer;
                              PossibleLocation_WeightingStr : String;
                            END;

  PossibleLocationsArrayType = ARRAY OF PossibleLocationRecType;

CONST
  UseEmergencyRouteing = True;
  NoUnitRef = True;

  PROCEDURE FindNextAvailabilityForLocation(T : TrainElement; Journey : Integer; MainLocation : Integer; AdjoiningLocationRequired : Boolean; AdjoiningLocation : Integer;
                                            RequestedStartTime : TDateTime; RequiredDurationInMinutes : Integer; OUT ReturnedStartTime : TDateTime;
                                            OUT SuccessMsg : String; OUT OK : Boolean);
  { Returns when the given location is next available, and the adjoining one too if that's required }

    FUNCTION GetMinuteFromTime(TimeToConvert : TDateTime) : Integer;
    VAR
      Hour, Min, Sec, MSec: Word;

    BEGIN
      DecodeTime(TimeToConvert, Hour, Min, Sec, MSec);
      Result := (Hour * 60) + Min;
    END; { GetMinuteFromTime }

    FUNCTION GetTimeFromMinute(Minute : Integer) : TDateTime;
    VAR
      Hour, Min, Sec, MSec: Word;

    BEGIN
      Hour := Minute DIV 60;
      Min := Minute MOD 60;
      Sec := 0;
      MSec := 0;
      Result := EncodeTime(Hour, Min, Sec, MSec);
    END; { GetTimeFromMinute }

  CONST
    EndOfDayInMinutes = 1439;

  VAR
    AvailabilityArray : LocationOccupationArrayType;
    AvailabilityArrayPos : Integer;
    AvailabilityFound : Boolean;
    DebugStr : String;
    I : Integer;
    LocationOccupationArrayPos : Integer;
    FromMinute : Integer;
    MinuteCounter : Integer;
    MinutesOccupiedArray : ARRAY [0..1440] OF Boolean;
    SaveMinuteOccupationState : Boolean;
    SavedMinute : Integer;
    ToMinute : Integer;

  BEGIN
    WITH Trains[T] DO BEGIN
      SuccessMsg := '';
      
      IF DebuggingMode THEN
        Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey) + 'Finding next available location:');

      { Clear the array first }
      FOR I := 0 TO EndOfDayInMinutes DO
        MinutesOccupiedArray[I] := False;

      { Now add the occupations to an array of each minute in the day }
      LocationOccupationArrayPos := 0;
      WHILE LocationOccupationArrayPos <= High(LocationOccupations[MainLocation]) DO BEGIN
        FromMinute := GetMinuteFromTime(LocationOccupations[MainLocation, LocationOccupationArrayPos].LocationOccupation_StartTime);
        ToMinute := GetMinuteFromTime(LocationOccupations[MainLocation, LocationOccupationArrayPos].LocationOccupation_EndTime);

        FOR I := FromMinute TO ToMinute DO
          MinutesOccupiedArray[I] := True;

        Inc(LocationOccupationArrayPos);
      END; {WHILE}

      IF AdjoiningLocationRequired THEN BEGIN
        LocationOccupationArrayPos := 0;
        WHILE LocationOccupationArrayPos <= High(LocationOccupations[AdjoiningLocation]) DO BEGIN
          FromMinute := GetMinuteFromTime(LocationOccupations[AdjoiningLocation, LocationOccupationArrayPos].LocationOccupation_StartTime);
          ToMinute := GetMinuteFromTime(LocationOccupations[AdjoiningLocation, LocationOccupationArrayPos].LocationOccupation_EndTime);

          FOR I := FromMinute TO ToMinute DO
            MinutesOccupiedArray[I] := True;

          Inc(LocationOccupationArrayPos);
        END; {WHILE}
      END;

      SaveMinuteOccupationState := MinutesOccupiedArray[0];
      FOR I := 0  TO EndOfDayInMinutes DO BEGIN
        IF MinutesOccupiedArray[I] <> SaveMinuteOccupationState THEN BEGIN
          { the start of an availability }
          SaveMinuteOccupationState := MinutesOccupiedArray[I];
          SetLength(AvailabilityArray, Length(AvailabilityArray) + 1);
          IF MinutesOccupiedArray[I] THEN
            AvailabilityArray[High(AvailabilityArray)].LocationOccupation_EndTime := GetTimeFromMinute(I - 1)
          ELSE BEGIN
            AvailabilityArray[High(AvailabilityArray)].LocationOccupation_StartTime := GetTimeFromMinute(I);
          END;
        END;
      END;

      IF DebuggingMode THEN
        FOR AvailabilityArrayPos := 0 TO High(AvailabilityArray) DO
          Log('X ' + TimeToHMStr(AvailabilityArray[AvailabilityArrayPos].LocationOccupation_StartTime)
                 + 'to' + TimeToHMStr(AvailabilityArray[AvailabilityArrayPos].LocationOccupation_EndTime));

      ReturnedStartTime := 0;
      WITH Locationoccupations[MainLocation][LocationOccupationArrayPos] DO BEGIN
        IF RequiredDurationInMinutes = -1 THEN
          DebugStr := LocationToStr(MainLocation)
                      + IfThen(AdjoiningLocationRequired,
                               '/' + LocationToStr(AdjoiningLocation), '')
                      + ': looking for occupation from ' + TimeToHMStr(RequestedStartTime) + ' until 23:59'
        ELSE
          DebugStr := LocationToStr(MainLocation)
                      + IfThen(AdjoiningLocationRequired,
                               '/' + LocationToStr(AdjoiningLocation), '')
                      + ': looking for ' + IntToStr(RequiredDurationInMinutes) + ' mins at ' + TimeToHMStr(RequestedStartTime);
      END; {WITH}

      FromMinute := GetMinuteFromTime(RequestedStartTime);
      AvailabilityFound := False;
      MinuteCounter := 0;
      SavedMinute := 0;

      I := FromMinute;
      WHILE (I <= EndOfDayInMinutes)
      AND NOT AvailabilityFound
      DO BEGIN
        { See if the minute is occupied }
        IF NOT MinutesOccupiedArray[I] THEN BEGIN
          IF SavedMinute = 0 THEN BEGIN
            IF DebuggingMode THEN
              Log('X found gap at ' + TimeToHMStr(GetTimeFromMinute(I)));
            SavedMinute := I;
          END;

          Inc(MinuteCounter);
          IF (RequiredDurationInMinutes <> 0)
          AND (MinuteCounter = RequiredDurationInMinutes + 1)
          THEN BEGIN
            ReturnedStartTime := GetTimeFromMinute(SavedMinute);
            Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                  + DebugStr + ' - returned time=' + TimeToHMStr(ReturnedStartTime));
            AvailabilityFound := True;
            SuccessMsg := 'substituted time=' + TimeToHMStr(ReturnedStartTime);
          END ELSE
            IF I = EndOfDayInMinutes THEN BEGIN
              ReturnedStartTime := GetTimeFromMinute(SavedMinute);
              Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                    + DebugStr + ' - returned time=' + TimeToHMStr(ReturnedStartTime));
              AvailabilityFound := True;
              SuccessMsg := 'substituted time=' + TimeToHMStr(ReturnedStartTime);
            END;
        END ELSE BEGIN
          { the minute is occupied }
          MinuteCounter := 0;
          SavedMinute := 0;
        END;

        Inc(I);
      END; {WHILE}

      IF AvailabilityFound THEN
        OK := True
      ELSE BEGIN
        OK := False;
        Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey) + DebugStr + ' - no availability found');
      END;
    END; {WITH}
  END; { FindNextAvailabilityForLocation }

  PROCEDURE DeleteElementFromPossibleLocationsArray(VAR PossibleLocationsArray : PossibleLocationsArrayType; Position : Integer);
  { Removes the selected element from the possible locations array }
  VAR
    I : Integer;

  BEGIN
    { Move all existing elements down one }
    FOR I := Position TO (Length(PossibleLocationsArray) - 2) DO
      PossibleLocationsArray[I] := PossibleLocationsArray[I + 1];
    SetLength(PossibleLocationsArray, Length(PossibleLocationsArray) - 1);
  END; { DeleteElementFromPossibleLocationsArray }

  PROCEDURE FindPossibleAlternatives(T : TrainElement; Journey : Integer; Area : Integer; OldLocation : Integer; MayReselectOldLocation : Boolean;
                                     OUT PossibleAlternativesArray : PossibleLocationsArrayType);
  { Work out which are the possible alternatives }
  VAR
    AreaCount : Integer;
    AreaFoundForWeighting : Boolean;
    ChangingDirection : Boolean;
    ErrorMsg : String;
    InitialAreaFound : Boolean;
    I : Integer;
    LocationCount : Integer;
    OK : Boolean;
    PlatformPriorityWeighting : Integer;
    PossibleAlternativesCount : Integer;
    TempLocationWeighting : Integer;

  BEGIN
    WITH Trains[T] DO BEGIN
      WITH Train_JourneysArray[Journey] DO BEGIN
        { Note the direction the train is going in }
        IF Journey = High(Train_JourneysArray) THEN
          { the final journey }
          ChangingDirection := False
        ELSE
          { if this is not the last journey, we may be changing direction }
          ChangingDirection := TrainJourney_Direction <> Train_JourneysArray[Journey + 1].TrainJourney_Direction;

        { Work out all the possible locations first. Note that it no longer matters if the original location is chosen as an alternative, as the system will now attempt to
          retime a visit there.
        }
        SetLength(PossibleAlternativesArray, 0);
        LocationCount := 0;
        InitialAreaFound := False;
        WHILE LocationCount <= High(Locations) DO BEGIN
          WITH Locations[LocationCount] DO BEGIN
            IF Area = Locations[LocationCount].Location_Area THEN BEGIN
              InitialAreaFound := True;
              Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumbers(T, Journey, Journey + 1)
                                    + 'Checking PlatformOrFiddleyard_Location=' + Locations[LocationCount].Location_LongStr);
              SetLength(PossibleAlternativesArray, Length(PossibleAlternativesArray) + 1);
              WITH PossibleAlternativesArray[High(PossibleAlternativesArray)] DO BEGIN
                OK := True;
                ErrorMsg := '';

                PossibleLocation_PlatformOrFiddleyardLocation := LocationCount;
                PossibleLocation_AdjoiningPlatformOrFiddleyardLocation := Location_AdjoiningPlatform;
                PossibleLocation_Available := True;
                PossibleLocation_DirectionPriority := Location_DirectionPriority;
                PossibleLocation_LengthLessThanTrainLength := False;
                PossibleLocation_NextAvailableTime := 0;
                PossibleLocation_PlatformPriority := Location_PlatformPriority;
                PossibleLocation_ThroughOrStoppingPriority := Location_ThroughOrStoppingPriority;
                PossibleLocation_TrainPriority := Location_TrainPriority;
                PossibleLocation_WeightingStr := '';

                SetLength(PossibleLocation_DestinationPriorityAreas, Length(Location_DestinationPriorityAreas));
                FOR AreaCount := 0 TO High(Location_DestinationPriorityAreas) DO
                  PossibleLocation_DestinationPriorityAreas[AreaCount] := Location_DestinationPriorityAreas[AreaCount];

                { now add the default (low) preference for this location }
                PossibleLocation_Weighting := 0;

                { See if some can be eliminated }
                IF OK THEN BEGIN
                  IF NOT MayReselectOldLocation
                  AND (LocationCount = OldLocation)
                  THEN BEGIN
                    ErrorMsg := 'may not reselect old location ' + LocationToStr(OldLocation);
                    OK := False;
                  END;
                END;

                IF OK THEN BEGIN
                  IF (TrainJourney_Direction = Up)
                  AND (Location_DirectionPriority = DownOnly)
                  THEN BEGIN
                    ErrorMsg := 'it is down only whereas current train direction is up ';
                    OK := False;
                  END;
                END;

                IF OK THEN BEGIN
                  IF (TrainJourney_Direction = Up)
                  AND (Location_DirectionPriority = TerminatingAtDown)
                  THEN BEGIN
                    ErrorMsg := 'it is terminating at down whereas current train direction is up';
                    OK := False;
                  END;
                END;

                IF OK THEN BEGIN
                  IF (TrainJourney_Direction = Down)
                  AND (Location_DirectionPriority = UpOnly)
                  THEN BEGIN
                    ErrorMsg := 'it is up only whereas current train direction is down )';
                    OK := False;
                  END;
                END;

                IF OK THEN BEGIN
                  IF (TrainJourney_Direction = Down)
                  AND (Location_DirectionPriority = TerminatingAtUp)
                  THEN BEGIN
                    ErrorMsg := 'it is terminating at up whereas current train direction is down';
                    OK := False;
                  END;
                END;

                { Remove any locations we're not able to use }
                IF OK THEN BEGIN
                  IF IsElementInIntegerArray(Locations[LocationCount].Location_LocosNotAbleToUse, Train_LocoChip) THEN BEGIN
                    ErrorMsg := 'it cannot be used by loco ' + LocoChipToStr(Train_LocoChip);
                    OK := False;
                  END;
                END;

                { and any that are out of use }
                IF Locations[LocationCount].Location_OutOfUse THEN BEGIN
                  ErrorMsg := 'location is recorded as being out of use';
                  OK := False;
                END;

                { If it's the final journey, and the end area is the same as the start area, exclude any trains that terminate facing the same direction that the start
                  journey took, as otherwise repeating the timetable won't work for that loco: i.e. if we start in the down direction from FY10 (which is marked as
                  terminating at up), and end at FY12 (which is marked as terminating at down), if we repeat the sequence we can no longer go down).
                }
                IF OK THEN BEGIN
                  IF (Journey > 0)
                  AND (Journey = High(Train_JourneysArray))
                  AND (Train_JourneysArray[0].TrainJourney_StartArea = Train_JourneysArray[High(Train_JourneysArray)].TrainJourney_EndArea)
                  THEN BEGIN
                    IF Train_JourneysArray[0].TrainJourney_Direction = Up THEN BEGIN
                      IF PossibleLocation_DirectionPriority = TerminatingAtUp THEN BEGIN
                        ErrorMsg := 'it is terminating at up whereas originating train direction was up';
                        OK := False;
                      END;
                    END ELSE BEGIN
                      { the down direction }
                      IF PossibleLocation_DirectionPriority = TerminatingAtDown THEN BEGIN
                        ErrorMsg := 'it is terminating at down whereas originating train direction was down';
                        OK := False;
                      END;
                    END;
                  END;
                END;

                { Also exclude any areas that we're not allowed to use because we're not of the required class }
                IF OK
                AND (TrainJourney_EndArea <> UnknownArea)
                THEN BEGIN
                  IF Length(Location_LocoClassesReservedFor) > 0 THEN BEGIN
                    IF NOT IsElementInStringArray(Location_LocoClassesReservedFor, Train_LocoClassStr) THEN BEGIN
                      OK := False;
                      ErrorMsg := 'the loco''s class (' + Train_LocoClassStr + ') is not in the permitted classes for this location (';
                      FOR I := 0 TO (High(Location_LocoClassesReservedFor) - 1) DO
                        ErrorMsg := ErrorMsg + Location_LocoClassesReservedFor[I] + '; ';
                      ErrorMsg := ErrorMsg + Location_LocoClassesReservedFor[High(Location_LocoClassesReservedFor)] + ')';
                    END;
                  END;
                END;

                { Also exclude any areas that we're not allowed to use }
                IF OK
                AND (TrainJourney_StartArea <> UnknownArea)
                THEN BEGIN
                  AreaCount := 0;
                  OK := False;
                  WHILE (AreaCount <= High(Location_DestinationPriorityAreas))
                  AND NOT OK
                  DO BEGIN
                    IF Areas[TrainJourney_StartArea].Area_IsHoldingArea
                    OR (TrainJourney_StartArea = Location_DestinationPriorityAreas[AreaCount])
                    THEN
                      OK := True;
                    Inc(AreaCount);
                  END; {WHILE}
                  IF NOT OK THEN
                    ErrorMsg := 'the journey start area ' + AreaToStr(TrainJourney_StartArea)
                                + ' is not in the list of accessible start areas for location=' + Location_LongStr;
                END;

                { And remove any locations we can't get actually to... }
                IF OK
                AND (TrainJourney_StartArea <> UnknownArea)
                THEN BEGIN
                  IF Journey < Train_TotalJourneys THEN BEGIN
                    AreaCount := 0;
                    OK := False;
                    WHILE (AreaCount <= High(Location_DestinationPriorityAreas))
                    AND NOT OK
                    DO BEGIN
                      IF Areas[TrainJourney_StartArea].Area_IsHoldingArea
                      OR (Train_JourneysArray[Journey + 1].TrainJourney_EndArea = Location_DestinationPriorityAreas[AreaCount])
                      THEN
                        OK := True;
                      Inc(AreaCount);
                    END; {WHILE}
                    IF NOT OK THEN
                      ErrorMsg := 'the next journey end area ' + AreaToStr(Train_JourneysArray[Journey + 1].TrainJourney_EndArea)
                                  + ' is not in the list of accessible end areas for location=' + Location_LongStr;
                  END;
                END;

                { ...and any that are occupied when we want to be there. If we're looking for the next available location, don't do this test, though. }
                IF NOT TrainJourney_LocationsPending
                AND NOT FindNextAvailableLocation
                THEN BEGIN
                  IF OK THEN BEGIN
                    IF Journey < High(Train_JourneysArray) THEN
                      CheckLocationOccupation(T, Journey, Journey + 1,
                                              PossibleLocation_PlatformOrFiddleyardLocation,
                                              IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                              IncMinute(Train_JourneysArray[Journey + 1].TrainJourney_CurrentDepartureTime, 1),
                                              OK)
                    ELSE
                      CheckLocationOccupation(T, Journey, Journey + 1,
                                              PossibleLocation_PlatformOrFiddleyardLocation,
                                              IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                              StrToTime('23:59'),
                                              OK);
                  END;

                  { Also check adjoining locations if the train is occupying them as well }
                  IF OK
                  AND (Journey < High(Train_JourneysArray))
                  THEN BEGIN
                    IF Train_CurrentLengthInInches > Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LengthInInches
                    THEN BEGIN
                      IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtUp <> UnknownLocation
                      THEN BEGIN
                        CheckLocationOccupation(T, Journey, Journey + 1,
                                                Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtUp,
                                                IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                                           TrainJourney_ActualArrivalTime,
                                                           IncMinute(TrainJourney_CurrentArrivalTime, -1)),
                                                IncMinute(Train_JourneysArray[Journey + 1].TrainJourney_CurrentDepartureTime, 1),
                                                OK);
                      END ELSE
                        IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtDown <> UnknownLocation
                        THEN BEGIN
                          CheckLocationOccupation(T, Journey, Journey + 1,
                                                  Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtDown,
                                                  IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                                             TrainJourney_ActualArrivalTime,
                                                             IncMinute(TrainJourney_CurrentArrivalTime, -1)),
                                                  IncMinute(Train_JourneysArray[Journey + 1].TrainJourney_CurrentDepartureTime, 1),
                                                  OK);

                        END;
                    END;
                  END;

                  { And if we're passing through a platform on our way to an adjoining platform, record that passage to protect the platform against being occupied by
                    something else that will obstruct our access, but not if we've already arrived.
                  }
                  IF OK
                  AND (Journey < High(Train_JourneysArray))
                  THEN BEGIN
                    IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtUp <> UnknownLocation
                    THEN BEGIN
                      IF TrainJourney_Direction = Down THEN
                        CheckLocationOccupation(T, Journey, Journey + 1,
                                                Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtUp,
                                                IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                                IncMinute(TrainJourney_CurrentArrivalTime, 1),
                                                OK);
                    END;

                    IF OK THEN BEGIN
                      IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtDown <> UnknownLocation
                      THEN BEGIN
                        IF TrainJourney_Direction = Up THEN
                          CheckLocationOccupation(T, Journey, Journey + 1,
                                                  Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtDown,
                                                  IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                                  IncMinute(TrainJourney_CurrentArrivalTime, 1),
                                                  OK);
                      END;
                    END;
                  END;

                  { And if we're going to pass through an adjoining platform on leaving the station, record that passage to protect the platform against being occupied by
                    something else that will obstruct our egress
                  }
                  IF OK THEN BEGIN
                    IF Journey < High(Train_JourneysArray) THEN BEGIN
                      IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtUp <> UnknownLocation
                      THEN BEGIN
                        IF (TrainJourney_Direction = Up)
                        AND (Train_JourneysArray[Journey + 1].TrainJourney_Direction = Up)
                        THEN
                          CheckLocationOccupation(T, Journey, Journey + 1,
                                                  Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtUp,
                                                  IncMinute(Train_JourneysArray[Journey + 1].TrainJourney_CurrentDepartureTime, -1),
                                                  IncMinute(Train_JourneysArray[Journey + 1].TrainJourney_CurrentDepartureTime, 1),
                                                  OK);
                      END;

                      IF OK THEN BEGIN
                        IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtDown <> UnknownLocation
                        THEN BEGIN
                          IF (TrainJourney_Direction = Down)
                          AND (Train_JourneysArray[Journey + 1].TrainJourney_Direction = Down)
                          THEN
                            CheckLocationOccupation(T, Journey, Journey + 1,
                                                    Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAtDown,
                                                    IncMinute(Train_JourneysArray[Journey + 1].TrainJourney_CurrentDepartureTime, -1),
                                                    IncMinute(Train_JourneysArray[Journey + 1].TrainJourney_CurrentDepartureTime, 1),
                                                    OK);
                        END;
                      END;
                    END;
                  END;
                END;

                IF NOT OK THEN BEGIN
                  PossibleLocation_Available := False;
                  IF ErrorMsg <> '' THEN
                    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                          + Locations[LocationCount].Location_ShortStr + ' not possible as an alternative location: ' + ErrorMsg);
                END;
              END;
            END;
          END; {WITH}
          Inc(LocationCount);
        END; {WHILE}
        IF NOT OK
        AND NOT InitialAreaFound
        THEN
          Log('D No search area found to match area=' + AreaToStr(Area));

        IF (OK OR InitialAreaFound)
        AND NOT Areas[Area].Area_SortByLocationLength
        THEN BEGIN
          { Work out the weighting based on various factors }
          Log(Train_LocoChipStr + ' D WORKING OUT THE WEIGHTING');
          PossibleAlternativesCount := 0;
          WHILE (PossibleAlternativesCount <= High(PossibleAlternativesArray)) DO BEGIN
            WITH PossibleAlternativesArray[PossibleAlternativesCount] DO BEGIN
              IF PossibleLocation_Available THEN BEGIN
                PossibleLocation_WeightingStr := '(';

                IF PossibleLocation_PlatformPriority <> 0 THEN BEGIN
                  { this assumes there are no more than twenty platforms! }
                  PlatformPriorityWeighting := 20 - PossibleLocation_PlatformPriority;

                  PossibleLocation_Weighting := PossibleLocation_Weighting + PlatformPriorityWeighting;
                  PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'PlatformPriority' + IntToStr(PossibleLocation_PlatformPriority)
                                                   + '=' + IntToStr(PlatformPriorityWeighting) + ' ';
                END;

                IF Train_Type = ExpressPassenger THEN BEGIN
                  IF NOT OmitLocoTypeRestriction
                  AND (PossibleLocation_TrainPriority = PassengerOnly)
                  THEN BEGIN
                    PossibleLocation_Available := False;
                    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                          + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation) + ' not available as the train is an express');
                  END ELSE
                    IF (PossibleLocation_TrainPriority = ExpressPreferred) OR (PossibleLocation_TrainPriority = ExpressOnly) THEN BEGIN
                      IF OmitLocoTypeRestriction THEN
                        PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + '[omitting express train restriction] '
                      ELSE BEGIN
                        PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                        PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'Express=20 ';
                      END;
                    END;
                END;

                IF Train_Type = OrdinaryPassenger THEN BEGIN
                  IF NOT OmitLocoTypeRestriction
                  AND (PossibleLocation_TrainPriority = ExpressOnly)
                  THEN BEGIN
                    IF NOT Train_journeysArray[Journey].TrainJourney_StoppingOnArrival THEN
                      Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                            + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation)
                                            + ' available even though the train is not an express as it is not stopping')
                    ELSE BEGIN
                      PossibleLocation_Available := False;
                      Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                            + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation)
                                            + ' not available as the train is not an express');
                    END;
                  END ELSE
                    IF (PossibleLocation_TrainPriority = PassengerPreferred) OR (PossibleLocation_TrainPriority = PassengerOnly)
                    THEN BEGIN
                      IF OmitLocoTypeRestriction THEN
                        PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + '[omitting passenger train restriction] '
                      ELSE BEGIN
                        PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                        PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'Passenger=20 ';
                      END;
                    END;
                END;

                IF TrainJourney_StoppingOnArrival
                AND (PossibleLocation_ThroughOrStoppingPriority = StoppingPriority)
                THEN BEGIN
                  PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                  PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'Stopping=20 ';
                END;
                IF NOT TrainJourney_StoppingOnArrival
                AND (PossibleLocation_ThroughOrStoppingPriority = ThroughPriority)
                THEN BEGIN
                  PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                  PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'Through=20 ';
                END;

                IF Journey = High(Train_JourneysArray) THEN BEGIN
                  { the final journey - put in terminating platform ideally }
                  IF Train_JourneysArray[Journey].TrainJourney_Direction = Up THEN BEGIN
                    IF PossibleLocation_DirectionPriority = TerminatingAtDown THEN BEGIN
                      PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                      PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'FinalJourneyTerminatingAtUp=20 ';
                    END ELSE
                      IF (PossibleLocation_DirectionPriority = PreferablyUp) OR (PossibleLocation_DirectionPriority = UpOnly) THEN BEGIN
                        PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                        PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'FinalJourneyTerminatingAtUp=20 ';
                      END;
                  END ELSE
                    IF Train_JourneysArray[Journey].TrainJourney_Direction = Down THEN BEGIN
                      IF PossibleLocation_DirectionPriority = TerminatingAtDown THEN BEGIN
                        PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                        PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'FinalJourneyTerminatingAtDown=20 ';
                      END ELSE
                        IF (PossibleLocation_DirectionPriority = PreferablyDown)
                        OR (PossibleLocation_DirectionPriority = DownOnly)
                        THEN BEGIN
                          PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                          PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'FinalJourneyTerminatingAtDown=20 ';
                        END;
                    END;
                END ELSE
                  { look at the next journey }
                  IF NOT ChangingDirection THEN BEGIN
                    IF Train_JourneysArray[Journey].TrainJourney_Direction = Up THEN BEGIN
                      IF (PossibleLocation_DirectionPriority = PreferablyUp) OR (PossibleLocation_DirectionPriority = UpOnly) THEN BEGIN
                        PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                        PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'NextJourneyUp=20 ';
                      END;
                    END ELSE
                      IF (PossibleLocation_DirectionPriority = PreferablyDown) OR (PossibleLocation_DirectionPriority = DownOnly)
                      THEN BEGIN
                        PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                        PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'NextJourneyDown=20 ';
                      END;
                  END ELSE
                    { changing direction }
                    IF Train_JourneysArray[Journey].TrainJourney_Direction = Up THEN BEGIN
                      IF PossibleLocation_DirectionPriority = TerminatingAtUp THEN BEGIN
                        PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                        PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'ChangingDirectionTerminatingAtUp=20 ';
                      END ELSE
                        IF (PossibleLocation_DirectionPriority = PreferablyDown) OR (PossibleLocation_DirectionPriority = DownOnly)
                        THEN BEGIN
                          PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                          PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'NextJourneyDown=20 ';
                        END;
                    END ELSE
                      IF Train_JourneysArray[Journey].TrainJourney_Direction = Down THEN BEGIN
                        IF PossibleLocation_DirectionPriority = TerminatingAtDown THEN BEGIN
                          PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                          PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'ChangingDirectionTerminatingAtDown=20 ';
                        END ELSE
                          IF (PossibleLocation_DirectionPriority = PreferablyUp) OR (PossibleLocation_DirectionPriority = UpOnly)
                          THEN BEGIN
                            PossibleLocation_Weighting := PossibleLocation_Weighting + 20;
                            PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'NextJourneyUp=20 ';
                          END;
                    END;

                AreaFoundForWeighting := False;
                IF Journey < Train_TotalJourneys THEN BEGIN
                  AreaCount := 0;
                  WHILE (AreaCount <= High(PossibleLocation_DestinationPriorityAreas))
                  AND NOT AreaFoundForWeighting
                  DO BEGIN
                    IF PossibleLocation_DestinationPriorityAreas[AreaCount] = Train_JourneysArray[Journey + 1].TrainJourney_EndArea THEN BEGIN
                      CASE AreaCount OF
                        0:
                          TempLocationWeighting := 20;
                        1:
                          TempLocationWeighting := 15;
                        2:
                          TempLocationWeighting := 10;
                        3:
                          TempLocationWeighting := 5;
                        4:
                          TempLocationWeighting := 0;
                      ELSE {CASE}
                        TempLocationWeighting := 0;
                      END; {CASE}
                      AreaFoundForWeighting := True;
                      PossibleLocation_WeightingStr := PossibleLocation_WeightingStr + 'AreaMatch=' + IntToStr(TempLocationWeighting);
                      PossibleLocation_Weighting := PossibleLocation_Weighting + TempLocationWeighting;
                    END;
                    Inc(AreaCount);
                  END; {WHILE}
                END;
              END;
              PossibleLocation_WeightingStr := Trim(PossibleLocation_WeightingStr) + ')';
            END; {WITH}
            Inc(PossibleAlternativesCount);
          END; {WHILE}

          FOR I := 0 TO High(PossibleAlternativesArray) DO BEGIN
            WITH PossibleAlternativesArray[I] DO
              IF PossibleLocation_Available THEN
                Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                      + 'possible location='
                                      + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation)
                                      + '; Weighting=' + IntToStr(PossibleLocation_Weighting)
                                      + IfThen(PossibleLocation_Weighting > 0,
                                               ' ' + PossibleLocation_WeightingStr));
          END; {FOR}
        END;
      END; {WITH}
    END; {WITH}
  END; { FindPossibleAlternatives }

  TYPE
    AlternativeLocationsSortOrderType = (SortByWeighting, SortByLocationLength, SortByDepartureTime);

  PROCEDURE SortPossibleAlternatives(LocoChip : Integer; VAR PossibleLocationsArray : PossibleLocationsArrayType;
                                     AlternativeLocationsSortOrder : AlternativeLocationsSortOrderType);
  { Sort the alternative locations in given order of preference or by fiddleyard length }
  VAR
    DebugStr : String;
    I, J, K : Integer;
    LocoChipStr : String;
    MinWeighting : Integer;
    MaxAvailableTime : TDateTime;
    MaxLocationLength : Real;
    SortedPossibleLocationsArray : PossibleLocationsArrayType;
    TempPos : Integer;
    UnsortedPossibleLocationsArray : PossibleLocationsArrayType;

  BEGIN
    TRY
      LocoChipStr := LocoChipToStr(LocoChip);

      { Copy the old array across or can't alter the length of it }
      SetLength(UnsortedPossibleLocationsArray, Length(PossibleLocationsArray));

      DebugStr := '';
      FOR I := 0 TO High(PossibleLocationsArray) DO BEGIN
        UnsortedPossibleLocationsArray[I] := PossibleLocationsArray[I];
        DebugStr := DebugStr + ' ' + LocationToStr(PossibleLocationsArray[I].PossibleLocation_PlatformOrFiddleyardLocation, ShortStringType);
      END; {FOR}
      Log(LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)  + 'unsorted=' + DebugStr);

      SetLength(SortedPossibleLocationsArray, 0);

      { Now do the sort }
      FOR I := 0 TO High(UnsortedPossibleLocationsArray) DO BEGIN
        TempPos := -1;
        MinWeighting := 0;
        IF AlternativeLocationsSortOrder = SortByWeighting THEN BEGIN
          FOR J := 0 TO High(UnsortedPossibleLocationsArray) DO BEGIN
            IF UnsortedPossibleLocationsArray[J].PossibleLocation_Weighting >= MinWeighting THEN BEGIN
              MinWeighting := UnsortedPossibleLocationsArray[J].PossibleLocation_Weighting;
              TempPos := J;
            END;
          END; {FOR}
        END ELSE
          IF AlternativeLocationsSortOrder = SortByLocationLength THEN BEGIN
            TempPos := -1;
            MaxLocationLength := 9999.99;
            FOR J := 0 TO High(UnsortedPossibleLocationsArray) DO BEGIN
              IF Locations[UnsortedPossibleLocationsArray[J].PossibleLocation_PlatformOrFiddleyardLocation].Location_LengthInInches < MaxLocationLength
              THEN BEGIN
                MaxLocationLength := Locations[UnsortedPossibleLocationsArray[J].PossibleLocation_PlatformOrFiddleyardLocation].Location_LengthInInches;
                TempPos := J;
              END;
            END; {FOR}
          END ELSE
            IF AlternativeLocationsSortOrder = SortByDepartureTime THEN BEGIN
              TempPos := -1;
              MaxAvailableTime := StrToTime('23:59');
              FOR J := 0 TO High(UnsortedPossibleLocationsArray) DO BEGIN
                IF UnsortedPossibleLocationsArray[J].PossibleLocation_NextAvailableTime <= MaxAvailableTime THEN BEGIN
                  MaxAvailableTime := UnsortedPossibleLocationsArray[J].PossibleLocation_NextAvailableTime;
                  TempPos := J;
                END;
              END; {FOR}
            END;

        SetLength(SortedPossibleLocationsArray, Length(SortedPossibleLocationsArray) + 1);
        SortedPossibleLocationsArray[High(SortedPossibleLocationsArray)] := UnsortedPossibleLocationsArray[TempPos];

        { and remove the element from the original array }
        FOR K := TempPos TO (Length(UnsortedPossibleLocationsArray) - 2) DO
          UnsortedPossibleLocationsArray[K] := UnsortedPossibleLocationsArray[K + 1];
        SetLength(UnsortedPossibleLocationsArray, Length(UnsortedPossibleLocationsArray) -1);
      END; {FOR}

      { and copy the sorted data back }
      SetLength(PossibleLocationsArray, Length(SortedPossibleLocationsArray));
      DebugStr := '';
      FOR I := 0 TO High(SortedPossibleLocationsArray) DO BEGIN
        PossibleLocationsArray[I] := SortedPossibleLocationsArray[I];
        DebugStr := DebugStr  + ' ' + LocationToStr(PossibleLocationsArray[I].PossibleLocation_PlatformOrFiddleyardLocation, ShortStringType);
        IF AlternativeLocationsSortOrder = SortByLocationLength THEN BEGIN
          { add the order of preference too }
          PossibleLocationsArray[I].PossibleLocation_Weighting := I;
        END ELSE
          IF AlternativeLocationsSortOrder = SortByDepartureTime THEN
            { add the departure time too }
            DebugStr := DebugStr + ':' + TimeToHMStr(PossibleLocationsArray[I].PossibleLocation_NextAvailableTime);
      END; {FOR}

      IF AlternativeLocationsSortOrder = SortByWeighting THEN
        Log(LocoChipStr + ' D ' + DisplayJourneyNumber(Journey) + 'sorted in order of weighting=' + DebugStr)
      ELSE
        IF AlternativeLocationsSortOrder = SortByLocationLength THEN
          Log(LocoChipStr + ' D ' + DisplayJourneyNumber(Journey) + 'sorted by location length=' + DebugStr)
        ELSE
          IF AlternativeLocationsSortOrder = SortByDepartureTime THEN
            Log(LocoChipStr + ' D ' + DisplayJourneyNumber(Journey) + 'sorted by departure time=' + DebugStr);
    EXCEPT
      ON E : Exception DO
        Log('EG SortPossibleAlternatives: ' + E.ClassName + ' error raised, with message: '+ E.Message);
    END; {TRY}
  END; { SortPossibleAlternatives }

VAR
  AdjoiningPlatformOrFiddleyard : Integer;
  AdjoiningPlatformOrFiddleyardFound : Boolean;
  AdjoiningPlatformOrFiddleyardOK : Boolean;
  DebugStr : String;
  I : Integer;
  JourneyArray : StringArrayType;
  LinesNotAvailableStr : String;
  OK : Boolean;
  OutOfUseStr : String;
  OutOfUseTC : Integer;
  PlatformOK : Boolean;
  PossibleLocationsArray : PossibleLocationsArrayType;
  PossibleLocationsCount : Integer;
  PossibleNewLocation : Integer;
  PossibleNewStartTime : TDateTime;
  RequestedLocationOccupationStartTime : TDateTime;
  RequiredLocationOccupationTotalTimeInMinutes : Integer;
  ReturnedLocationOccupationStartTime : TDateTime;
  RouteCurrentlyLocked : Boolean;
  RoutePermanentlyLocked : Boolean;
  SecondPossibleLocationsCount : Integer;
  TempLocationRec : PossibleLocationRecType;

BEGIN
  TRY
    OK := False;
    NewLocation := UnknownLocation;

    WITH Trains[T] DO BEGIN
      DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);

      Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': ' + DescribeJourney(T, Journey));

      { Deal with requests to replace individual locations }
      IF Area <> UnknownArea THEN BEGIN
        IF NOT FindNextAvailableLocation THEN
          Log(Train_LocoChipStr + ' D SEEKING A REPLACEMENT LOCATION AT AREA=' + AreaToStr(Area)
                                + IfThen(ErrorMsg <> '',
                                         ' because ' + ErrorMsg, ''))
        ELSE
          Log(Train_LocoChipStr + ' D SEEKING NEXT AVAILABLE LOCATION AT AREA=' + AreaToStr(Area)
                                + IfThen(ErrorMsg <> '',
                                         ' because ' + ErrorMsg, ''));
      END ELSE
        IF OldLocation <> UnknownLocation THEN BEGIN
          Area := Locations[OldLocation].Location_Area;
          IF NOT FindNextAvailableLocation THEN
            Log(Train_LocoChipStr + ' D SEEKING A REPLACEMENT LOCATION FOR TRAIN TYPE ' + TrainTypeNumToStr(TrainTypeToTrainTypeNum(Train_Type))
                                  + ' at ' + LocationToStr(OldLocation) + ' in area=' + AreaToStr(Area)
                                  + IfThen(ErrorMsg <> '',
                                           ' because ' + ErrorMsg, ''))
          ELSE
            Log(Train_LocoChipStr + ' D SEEKING NEXT AVAILABLE LOCATION FOR TRAIN TYPE ' + TrainTypeNumToStr(TrainTypeToTrainTypeNum(Train_Type))
                                  + ' at ' + LocationToStr(OldLocation) + ' in area=' + AreaToStr(Area)
                                  + IfThen(ErrorMsg <> '',
                                           ' because ' + ErrorMsg, ''))
        END;

      WITH Train_JourneysArray[Journey] DO BEGIN
        ErrorMsg := '';

        FindPossibleAlternatives(T, Journey, Area, OldLocation, MayReselectOldLocation, PossibleLocationsArray);

        { Note any platforms/fiddleyards that are not possible to use - firstly ones that have track circuits that are out of use }
        PossibleLocationsCount := 0;
        WHILE PossibleLocationsCount <= High(PossibleLocationsArray) DO BEGIN
          IF PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_Available THEN BEGIN
            IF LocationOutOfUse(PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_PlatformOrFiddleyardLocation, OutOfUseTC, OutOfUseStr)
            THEN BEGIN
              PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_Available := False;
              Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                    + LocationToStr(PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_PlatformOrFiddleyardLocation)
                                    + ' is out of use as some of its track circuits are out of use, e.g. TC=' + IntToStr(OutOfUseTC)
                                    + ' is marked as ' + OutOfUseStr);
            END;
          END;
          Inc(PossibleLocationsCount);
        END; {WHILE}

        { Now mark as out of use the platforms/fiddleyards that connect to the existing out-of-use platforms/fiddleyards if the train cannot use them }
        PossibleLocationsCount := 0;
        WHILE PossibleLocationsCount <= High(PossibleLocationsArray) DO BEGIN
          WITH PossibleLocationsArray[PossibleLocationsCount] DO BEGIN
            IF PossibleLocation_Available THEN BEGIN
              PlatformOK := False;
              AdjoiningPlatformOrFiddleyardOK := False;
              DebugStr := '';

              IF (PossibleLocation_AdjoiningPlatformOrFiddleyardLocation <> UnknownLocation)
              AND LocationOutOfUse(PossibleLocation_AdjoiningPlatformOrFiddleyardLocation, OutOfUseTC, OutOfUseStr)
              THEN BEGIN
                AdjoiningPlatformOrFiddleyard := PossibleLocation_AdjoiningPlatformOrFiddleyardLocation;

                { See if the adjoining platform/fiddleyard is out of use }

                IF NOT AdjoiningPlatformOrFiddleyardOK THEN BEGIN
                  { Find the requisite location and mark it out of use unless a train is just coming in to the nearest platform/fiddleyard and leaving again having changed
                    direction...
                  }
                  IF NOT FinalJourney(T, Journey) OR NOT DirectionWillChangeAfterGivenJourney(T, Journey) THEN
                    DebugStr := ' train not terminating or changing direction, and ' + LocationToStr(AdjoiningPlatformOrFiddleyard) + ' is out of use'
                  ELSE
                    IF TrainJourney_Direction <> Locations[AdjoiningPlatformOrFiddleyard].Location_PlatformOrFiddleyardDirection THEN
                      DebugStr := ' train cannot pass through ' + LocationToStr(AdjoiningPlatformOrFiddleyard) + ' which is out of use'
                    ELSE
                      PlatformOK := True;

                  IF NOT PlatformOK THEN BEGIN
                    { ...or can transfer to a parallel platform by means of a crossover }
                    IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyard = UnknownLocation THEN
                      DebugStr := ': no crossover at platform or fiddleyard'
                    ELSE
                      IF LocationOutOfUse(Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardAccessViaParallelPlatformOrFiddleyard,
                                          OutOfUseTC, OutOfUseStr)
                      THEN
                        DebugStr := ': train cannot exit from the crossover platform or fiddleyard as, e.g., TC=' + IntToStr(OutOfUseTC) + ' is marked as ' + OutOfUseStr
                      ELSE
                        PlatformOK := True;
                  END;
                END;

                IF NOT PlatformOK THEN BEGIN
                  PossibleLocation_Available := False;
                  Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey) + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation) + ' out of use: ' + DebugStr);
                END;
              END;
            END;
          END; {WITH}
          Inc(PossibleLocationsCount);
        END; {WHILE}

        { See if the platform/fiddleyard is a dead end }
        IF NOT FinalJourney(T, Journey) THEN BEGIN
          PossibleLocationsCount := 0;
          WHILE PossibleLocationsCount <= High(PossibleLocationsArray) DO BEGIN
            WITH PossibleLocationsArray[PossibleLocationsCount] DO BEGIN
              IF Train_JourneysArray[Journey + 1].TrainJourney_Direction = Up THEN BEGIN
                IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LineAtUp = UnknownLine THEN
                  Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                        + 'Cannot route up via ' + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation) + ' as there is an unknown line there')
                ELSE
                  IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LineAtUpIsEndOfLine THEN BEGIN
                    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                          + 'Cannot route up via ' + Lines[Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LineAtUp].Line_Str
                                          + ' as it is a dead end');
                    PossibleLocation_Available := False;
                  END;
              END ELSE
                IF Train_JourneysArray[Journey + 1].TrainJourney_Direction = Down THEN BEGIN
                  IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LineAtDown = UnknownLine THEN BEGIN
                    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                          + 'Cannot route down via ' + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation) + ' as there is an unknown line there');
                    PossibleLocation_Available := False;
                  END ELSE
                    IF Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LineAtDownIsEndOfLine THEN BEGIN
                      Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                            + 'Cannot route down via ' + Lines[Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LineAtDown].Line_Str
                                            + ' as it is a dead end');
                      PossibleLocation_Available := False;
                    END;
                END;
            END; {WITH}
            Inc(PossibleLocationsCount);
          END; {WHILE}
        END;

        { Remove any platforms/fiddleyards where the train won't fit, provided we're stopping, and note adjoining platforms that we might have to pass through }
        Log(Train_LocoChipStr + ' D CHECKING TRAIN FIT');
        IF TrainJourney_StoppingOnArrival THEN BEGIN
          PossibleLocationsCount := 0;
          WHILE PossibleLocationsCount <= High(PossibleLocationsArray) DO BEGIN
            WITH PossibleLocationsArray[PossibleLocationsCount] DO BEGIN
              PossibleLocation_LengthLessThanTrainLength := False;

              IF PossibleLocation_Available THEN BEGIN
                { See if the train will fit in the given platform/fiddleyard }
                IF Train_CurrentLengthInInches > (Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LengthInInches)
                THEN BEGIN
                  AdjoiningPlatformOrFiddleyardOK := False;
                  { If there's an adjoining platform/fiddleyard and see if it's available }
                  IF PossibleLocation_AdjoiningPlatformOrFiddleyardLocation <> UnknownLocation THEN BEGIN
                    SecondPossibleLocationsCount := 0;
                    AdjoiningPlatformOrFiddleyardFound := False;
                    WHILE (SecondPossibleLocationsCount <= High(PossibleLocationsArray))
                    AND NOT AdjoiningPlatformOrFiddleyardFound
                    DO BEGIN
                      IF PossibleLocationsArray[SecondPossibleLocationsCount].PossibleLocation_PlatformOrFiddleyardLocation =
                                                                                                                      PossibleLocation_AdjoiningPlatformOrFiddleyardLocation
                      THEN BEGIN
                        AdjoiningPlatformOrFiddleyardFound := True;
                        IF PossibleLocationsArray[SecondPossibleLocationsCount].PossibleLocation_Available THEN
                          AdjoiningPlatformOrFiddleyardOK := True;
                      END;
                      Inc(SecondPossibleLocationsCount);
                    END; {WHILE}
                  END;

                  IF NOT AdjoiningPlatformOrFiddleyardOK THEN BEGIN
                    PossibleLocation_Available := False;
                    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                          + 'train (len=' + IntToStr(Train_CurrentLengthInInches) + ') does not fit in '
                                          + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation)
                                          + ' (len=' + FloatToStr(Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LengthInInches) + ')');
                  END ELSE BEGIN
                    PossibleLocation_AdjoiningPlatformOrFiddleyardRequired := True;

                    { Will the train fit into both? }
                    IF Train_CurrentLengthInInches > (Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LengthInInches
                                                                                 + Locations[PossibleLocation_AdjoiningPlatformOrFiddleyardLocation].Location_LengthInInches
                                                                                 + 15) { an arbitrary additional number of inches when two platforms are combined }
                    THEN BEGIN
                      Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                            + 'train (len=' + IntToStr(Train_CurrentLengthInInches)
                                            + ') does not fit in either or both ' + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation)
                                            + ' (len=' + FloatToStr(Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LengthInInches)
                                            + ') and ' + LocationToStr(PossibleLocation_AdjoiningPlatformOrFiddleyardLocation)
                                            + ' (len=' + FloatToStr(Locations[PossibleLocation_AdjoiningPlatformOrFiddleyardLocation].Location_LengthInInches) + ')');
                      PossibleLocation_Available := False;
                    END ELSE BEGIN
                      Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                            + 'train (len=' + IntToStr(Train_CurrentLengthInInches) + ') fits in both '
                                            + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation)
                                            + ' (len=' + FloatToStr(Locations[PossibleLocation_PlatformOrFiddleyardLocation].Location_LengthInInches)
                                            + ') and ' + LocationToStr(PossibleLocation_AdjoiningPlatformOrFiddleyardLocation)
                                            + ' (len=' + FloatToStr(Locations[PossibleLocation_AdjoiningPlatformOrFiddleyardLocation].Location_LengthInInches)
                                            + ')');
                      { note which end of the station to send it }
                      IF TrainJourney_Direction = Locations[PossibleLocation_AdjoiningPlatformOrFiddleyardLocation].Location_PlatformOrFiddleyardDirection
                      THEN BEGIN
                        PossibleLocation_LengthLessThanTrainLength := True;
                        Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                              + 'cannot use ' + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation)
                                              + ' as the train is coming from the ' + DirectionToStr(OppositeDirection(TrainJourney_Direction))
                                              + ' direction and therefore needs to go into an ' + DirectionToStr(TrainJourney_Direction) + ' platform');
                      END;

                      { It can't fit in both if it's changing direction in the station, though, as it would therefore start from the wrong signal, and the initial track
                        circuits get confused
                      }
                      IF NOT FinalJourney(T, Journey)
                      AND (TrainJourney_Direction <> Train_JourneysArray[Journey + 1].TrainJourney_Direction)
                      THEN
                        Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                              + '...but train cannot use both ' + LocationToStr(PossibleLocation_PlatformOrFiddleyardLocation)
                                              + ' and ' + LocationToStr(PossibleLocation_AdjoiningPlatformOrFiddleyardLocation)
                                              + ' as it is changing direction before the next journey');
                    END;
                  END;
                END ELSE BEGIN
                  { Or see if we need to use an adjoining platform for access }
                  IF TrainJourney_EndLocation <> UnknownLocation THEN BEGIN
                    IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp <> UnknownLocation THEN BEGIN
                      IF TrainJourney_Direction = Down THEN
                        PossibleLocation_AdjoiningPlatformOrFiddleyardRequired := True;
                    END ELSE
                      IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown <> UnknownLocation THEN BEGIN
                        IF TrainJourney_Direction = Up THEN
                          PossibleLocation_AdjoiningPlatformOrFiddleyardRequired := True;
                      END;
                    END;
                  END;
              END;
            END; {WITH}
            Inc(PossibleLocationsCount);
          END;
        END; {WHILE}

        { Do not send the train into the platform/fiddleyard nearest to where it is coming from, or it won't fit. (This can't be done in the above loop, or half
          platforms/fiddleyards would be invalidated before the other half could be assessed).
        }
        PossibleLocationsCount := 0;
        WHILE PossibleLocationsCount <= High(PossibleLocationsArray) DO BEGIN
          IF PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_LengthLessThanTrainLength THEN
            PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_Available := False;

          Inc(PossibleLocationsCount);
        END; {WHILE}

        { If there's no other option, look for the next location that's available }
        IF FindNextAvailableLocation THEN BEGIN
          FOR PossibleLocationsCount := 0 TO High(PossibleLocationsArray) DO BEGIN
            WITH PossibleLocationsArray[PossibleLocationsCount] DO BEGIN
              IF PossibleLocation_Available THEN BEGIN
                PossibleLocation_NextAvailableTime := 0;

                { Find out how long we need to occupy it }
                RequestedLocationOccupationStartTime := IncMinute(TrainJourney_CurrentArrivalTime, -1);
                IF Journey = High(Train_JourneysArray) THEN
                  RequiredLocationOccupationTotalTimeInMinutes := -1
                ELSE
                  { note: we use the NewMinutesBetween system routine as the system one is known to be buggy }
                  RequiredLocationOccupationTotalTimeInMinutes := NewMinutesBetween(IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                                                                    IncMinute(Train_JourneysArray[Journey + 1].TrainJourney_CurrentDepartureTime, +1));
                FindNextAvailabilityForLocation(T, Journey,
                                                PossibleLocation_PlatformOrFiddleyardLocation,
                                                PossibleLocation_AdjoiningPlatformOrFiddleyardRequired,
                                                PossibleLocation_AdjoiningPlatformOrFiddleyardLocation,
                                                RequestedLocationOccupationStartTime,
                                                RequiredLocationOccupationTotalTimeInMinutes,
                                                ReturnedLocationOccupationStartTime,
                                                SuccessMsg,
                                                OK);
                IF OK THEN
                  PossibleLocation_NextAvailableTime := ReturnedLocationOccupationStartTime
                ELSE
                  PossibleLocation_Available := False;
              END;
            END; {WITH}
          END; {FOR}
        END;

        { Now remove all those that are not possible to use }
        PossibleLocationsCount := 0;
        WHILE PossibleLocationsCount <= High(PossibleLocationsArray) DO BEGIN
          IF NOT PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_Available THEN
            DeleteElementFromPossibleLocationsArray(PossibleLocationsArray, PossibleLocationsCount)
          ELSE
            Inc(PossibleLocationsCount);
        END; {WHILE}

        IF Length(PossibleLocationsArray) > 0 THEN BEGIN
          Log(Train_LocoChipStr + ' D SORTING AVAILABLE LOCATIONS');
          IF (Length(PossibleLocationsArray) > 1) THEN BEGIN
            { If there's more than one possible location, sort them in the required order }
            IF Areas[Area].Area_SortByLocationLength THEN BEGIN
              SortPossibleAlternatives(Train_LocoChip, PossibleLocationsArray, SortByLocationLength);
              DebugStr := ' in order of location length are:';
            END ELSE
              IF FindNextAvailableLocation THEN BEGIN
                SortPossibleAlternatives(Train_LocoChip, PossibleLocationsArray, SortByWeighting);
                SortPossibleAlternatives(Train_LocoChip, PossibleLocationsArray, SortByDepartureTime);
                DebugStr := ' in order of departure time are:';
              END ELSE BEGIN
                SortPossibleAlternatives(Train_LocoChip, PossibleLocationsArray, SortByWeighting);
                DebugStr := ' in order of weighting are:';
              END;

            Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey) + 'alternative locations at ' + IfThen(Area = UnknownArea,
                                                                                                                 LocationToStr(OldLocation, ShortStringType),
                                                                                                                 AreaToStr(Area))
                                                                                                        + DebugStr);
            DebugStr := '';
            FOR PossibleLocationsCount := 0 TO High(PossibleLocationsArray) DO
              IF FindNextAvailableLocation THEN
                DebugStr := DebugStr
                            + LocationToStr(PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_PlatformOrFiddleyardLocation, ShortStringType)
                            + ':' + TimeToHMStr(PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_NextAvailableTime) + ' '
              ELSE
                DebugStr := DebugStr + IntToStr(PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_Weighting)
                            + '=' + LocationToStr(PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_PlatformOrFiddleyardLocation, ShortStringType) + ' ';

            Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey) + DebugStr + ' {WRAP=SCREENWIDTH}');
          END;

          { Now other tests }
          PossibleLocationsCount := 0;
          OK := False;
          PossibleNewLocation := UnknownLocation;
          PossibleNewStartTime := 0;

          WHILE (PossibleLocationsCount <= High(PossibleLocationsArray))
          AND NOT OK
          DO BEGIN
            OK := True;
            PossibleNewLocation := PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_PlatformOrFiddleyardLocation;
            PossibleNewStartTime := PossibleLocationsArray[PossibleLocationsCount].PossibleLocation_NextAvailableTime;

            IF NOT PreRouteing THEN BEGIN
              { Test whether a route exists to the new location }
              IF OK THEN BEGIN
                Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                      + 'checking whether routeing from '
                                      + LocationToStr(TrainJourney_StartLocation, ShortStringType)
                                      + ' ' + DirectionToStr(TrainJourney_Direction)
                                      + ' to ' + LocationToStr(PossibleNewLocation, ShortStringType) + ' is possible...');
                CheckJourney(T, Journey, TrainJourney_Direction, TrainJourney_StartLocation, PossibleNewLocation, UnknownLine, UnknownLine, ErrorMsg, LinesNotAvailableStr,
                             NOT UseEmergencyRouteing, JourneyArray, OK);
                IF NOT OK THEN BEGIN
                  Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                        + 'CheckJourney failed: ' + ErrorMsg + ' - trying emergency routeing');
                  CheckJourney(T, Journey, TrainJourney_Direction, TrainJourney_StartLocation, PossibleNewLocation, UnknownLine, UnknownLine, ErrorMsg, LinesNotAvailableStr,
                               UseEmergencyRouteing, JourneyArray, OK);
                  IF NOT OK THEN
                    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                          + 'CheckJourney failed even with emergency routeing: ' + ErrorMsg);
                    Log(Train_LocoChipStr + ' D ' + LinesNotAvailableStr + ' {INDENT=9} {WRAP=SCREENWIDTH}');
                END;

                IF OK THEN BEGIN
                  Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                        + 'journey route found' + ' ' + DirectionToStr(TrainJourney_Direction)
                                        + ' between ' + LocationToStr(TrainJourney_StartLocation, ShortStringType)
                                        + ' and possible new location ' + LocationToStr(PossibleNewLocation, ShortStringType)
                                        + ' in LocationAvailable check');

                  { Check the whole route for any sort of permanent locking }
                  CheckRouteAheadLocking(T, JourneyArray, RouteCurrentlyLocked, RoutePermanentlyLocked, ErrorMsg);
                  IF RoutePermanentlyLocked THEN BEGIN
                    OK := False;
                    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                          + 'routeing from ' + LocationToStr(TrainJourney_StartLocation, ShortStringType) + ' ' + DirectionToStr(TrainJourney_Direction)
                                          + ' to ' + LocationToStr(PossibleNewLocation, ShortStringType) + ' is not possible as the route is permanently locked: '
                                          + ErrorMsg);
                    DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);
                  END ELSE
                    IF CurrentlyRouteing
                    AND RouteCurrentlyLocked
                    THEN BEGIN
                      OK := False;
                      Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                            + 'routeing from ' + LocationToStr(TrainJourney_StartLocation, ShortStringType) + ' ' + DirectionToStr(TrainJourney_Direction)
                                            + ' to ' + LocationToStr(PossibleNewLocation, ShortStringType) + ' is not possible as the route is currently locked: '
                                            + ErrorMsg);
                      DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);
                    END;

                  IF OK THEN BEGIN
                    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                          + 'routeing from ' + LocationToStr(TrainJourney_StartLocation, ShortStringType) + ' ' + DirectionToStr(TrainJourney_Direction)
                                          + ' to ' + LocationToStr(PossibleNewLocation, ShortStringType) + ' is possible');

                    { Now we need to check the following journey, if any }
                    IF (Journey < Train_TotalJourneys)
                    AND (Train_JourneysArray[Journey + 1].TrainJourney_EndLocation <> UnknownLocation)
                    THEN BEGIN
                      Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey + 1)
                                            + 'now checking whether routeing from ' + LocationToStr(PossibleNewLocation, ShortStringType)
                                            + ' ' + DirectionToStr(TrainJourney_Direction)
                                            + ' to ' + LocationToStr(Train_JourneysArray[Journey + 1].TrainJourney_EndLocation, ShortStringType)
                                            + ' is possible...');

                      { Test whether a route exists from the new location }
                      CheckJourney(T, Journey + 1,
                                      Train_JourneysArray[Journey + 1].TrainJourney_Direction,
                                      PossibleNewLocation,
                                      Train_JourneysArray[Journey + 1].TrainJourney_EndLocation,
                                      UnknownLine, UnknownLine, ErrorMsg, LinesNotAvailableStr, NOT UseEmergencyRouteing,
                                      JourneyArray, OK);
                      IF NOT OK THEN BEGIN
                        Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey + 2)
                                              + 'CheckJourney failed - trying emergency routeing:');
                        CheckJourney(T, Journey + 1,
                                        Train_JourneysArray[Journey + 1].TrainJourney_Direction,
                                        PossibleNewLocation,
                                        Train_JourneysArray[Journey + 1].TrainJourney_EndLocation,
                                        UnknownLine, UnknownLine, ErrorMsg, LinesNotAvailableStr, UseEmergencyRouteing,
                                        JourneyArray, OK);
                        IF NOT OK THEN
                          Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey + 2)
                                                + 'CheckJourney failed even with emergency routeing: '
                                                + ErrorMsg + ' (' + LinesNotAvailableStr + ')');
                      END;

                      IF OK THEN BEGIN
                        { now see if the route is in use }
                        IF RouteAheadOutOfUse(JourneyArray, ErrorMsg) THEN BEGIN
                          OK := False;
                          Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey + 1)
                                                + 'routeing from ' + LocationToStr(PossibleNewLocation, ShortStringType)
                                                + ' ' + DirectionToStr(Train_JourneysArray[Journey + 1].TrainJourney_Direction)
                                                + ' to ' + LocationToStr(Train_JourneysArray[Journey + 1].TrainJourney_EndLocation, ShortStringType)
                                                + ' is not possible as part of the route is out of use: ' + ErrorMsg);
                        END ELSE
                          Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey + 1)
                                                + 'journey route found between possible new location ' + LocationToStr(PossibleNewLocation, ShortStringType)
                                                + ' and ' + LocationToStr(Train_JourneysArray[Journey + 1].TrainJourney_EndLocation, ShortStringType)
                                                + ' in LocationAvailable check');
                      END;
                    END;
                  END;
                END;
              END;
            END; {WITH}
            Inc(PossibleLocationsCount)
          END; {WHILE}
          NewLocation := PossibleNewLocation;
          NewLocationStartTime := PossibleNewStartTime;
        END;

        IF NOT OK THEN
          Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': no alternative locations')
        ELSE
          Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                + 'area ' + Trim(AreaToStr(Area) + ' replacement location is ' + LocationToStr(NewLocation)));
      END; {WITH}
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG AlternativeAreaOrLocationAvailable: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
  Result := OK;
END; { AlternativeAreaOrLocationAvailable }

PROCEDURE SetLocationOccupationAllDayState(LocoChip : Integer; Location : Integer; LocationOccupationState : LocationOccupationStateType; ErrorMsg : String; OK : Boolean);
{ Sets a given location's all day occupation state (to out-of-use or permanently occupied }
CONST
  NoTrain = NIL;

VAR
  T : TrainElement;

BEGIN
  T := GetTrainRecord(LocoChip);
  InsertDataInLocationOccupationArray(T,
                                      UnknownJourney, UnknownJourney,
                                      Location,
                                      StrToTime('00:00'),
                                      StrToTime('23:59'),
                                      LocationOccupationState,
                                      ErrorMsg, OK);
END; { SetLocationOccupationAllDayState }

PROCEDURE AddTrainOccupationsToLocationOccupations(T : TrainElement; IsTimetableLoading : Boolean; OUT ErrorMsg : String; OUT OK : Boolean);
{ Find a loco and record all its locations }
VAR
  JourneyA : Integer;
  JourneyB : Integer;

BEGIN
  { This routine assumes that the timetable is in departure time order }
  OK := True;
  JourneyA := 0;

  WITH Trains[T] DO BEGIN
    IF (Train_LocoChip <> UnknownLocoChip)
    AND Train_DiagramFound
    THEN BEGIN
      { find a loco, record its locations, and then look through the timetable for all instances of it }
      IF (Train_CurrentStatus = Cancelled)
      OR (Train_LocatedAtStartup
      AND ((Train_CurrentStatus = NonMoving) OR (Train_CurrentStatus = Suspended) OR (Train_CurrentStatus = MissingAndSuspended)))
      THEN
        InsertDataInLocationOccupationArray(T,
                                            UnknownJourney, UnknownJourney,
                                            GetLocationFromTrackCircuit(Train_CurrentTC),
                                            StrToTime('00:00'),
                                            StrToTime('23:59'),
                                            LocationPermanentOccupationWithFeedback, ErrorMsg, OK)
      ELSE BEGIN
        WHILE OK
        AND (JourneyA <= Train_TotalJourneys)
        DO BEGIN
          WITH Train_JourneysArray[JourneyA] DO BEGIN
            JourneyB := JourneyA + 1;
            IF (JourneyB <= Train_TotalJourneys)
            AND (Train_JourneysArray[JourneyB].TrainJourney_ActualDepartureTime <> 0)
            THEN
              Log('X J=' + IntToStr(JourneyA ) + ' to be cleared')
            ELSE BEGIN
              { add the first entry }
              IF (JourneyA = 0)
              AND (Train_JourneysArray[0].TrainJourney_ActualDepartureTime = 0)
              THEN
                InsertDataInLocationOccupationArray(T,
                                                    UnknownJourney, JourneyA,
                                                    GetLocationFromTrackCircuit(Train_CurrentTC),
                                                    StrToTime('00:00'),
                                                    IncMinute(Train_JourneysArray[JourneyA].TrainJourney_CurrentDepartureTime, 1),
                                                    LocationStartOfDayoccupation, ErrorMsg, OK);
              IF OK THEN BEGIN
                { add all subsequent locations bar the last }
                IF JourneyA < Train_TotalJourneys THEN BEGIN
                  IF TrainJourney_EndLocation = UnknownLocation THEN
                    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyA)
                                          + 'not yet setting up a location occupation as a location allocation is pending')
                  ELSE BEGIN
                    { Add the intervening locations, but first check they all work, except for ones that have already been set up. Do this in advance of inserting the data
                      in the array as we may want to change the location, which may affect the adjoining location if the train length means it will occupy that too, or if
                      we want to protect a train's access to its sub-platform.
                    }
                    IF OK
                    AND TrainJourney_StoppingOnArrival
                    AND (Train_JourneysArray[JourneyB].TrainJourney_ActualDepartureTime = 0)
                    THEN BEGIN
                      CheckJourneyLocations(T, JourneyA, IsTimetableLoading, OK);
                      IF NOT OK THEN
                        Log(Train_LocoChipStr + ' DG ' + ' J=' + IntToStr(JourneyA) + ': CheckJourneyLocations not ok');
                    END;

                    IF OK
                    AND TrainJourney_StoppingOnArrival
                    AND (Train_JourneysArray[JourneyB].TrainJourney_ActualDepartureTime = 0)
                    THEN BEGIN
                      { add a minute at both ends of the visit to allow for entering/leaving the station, unless we've arrived }
                      InsertDataInLocationOccupationArray(T, JourneyA, JourneyB,
                                                          TrainJourney_EndLocation,
                                                          IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                                                     TrainJourney_ActualArrivalTime,
                                                                     IncMinute(TrainJourney_CurrentArrivalTime, -1)),
                                                          IncMinute(Train_JourneysArray[JourneyB].TrainJourney_CurrentDepartureTime, 1),
                                                          LocationTemporaryOccupation, ErrorMsg, OK);

                      { Also record adjoining locations if the train is occupying them too }
                      IF Train_CurrentLengthInInches > Locations[TrainJourney_EndLocation].Location_LengthInInches THEN BEGIN
                        IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp <> UnknownLocation THEN BEGIN
                          Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyA)
                                                + ' needs to occupy both ' + LocationToStr(TrainJourney_EndLocation)
                                                + ' and '
                                                + LocationToStr(Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp));
                          InsertDataInLocationOccupationArray(T, JourneyA, JourneyB,
                                                              Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp,
                                                              IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                                                         TrainJourney_ActualArrivalTime,
                                                                         IncMinute(TrainJourney_CurrentArrivalTime, -1)),
                                                              IncMinute(Train_JourneysArray[JourneyB].TrainJourney_CurrentDepartureTime, 1),
                                                              LocationTemporaryOccupation, ErrorMsg, OK);
                        END ELSE
                          IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown <> UnknownLocation THEN BEGIN
                            Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyA)
                                                  + ' needs to occupy both ' + LocationToStr(TrainJourney_EndLocation)
                                                  + ' and '
                                                  + LocationToStr(Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp));
                            InsertDataInLocationOccupationArray(T,
                                                                JourneyA, JourneyB,
                                                                Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown,
                                                                IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                                                           TrainJourney_ActualArrivalTime,
                                                                           IncMinute(TrainJourney_CurrentArrivalTime, -1)),
                                                                IncMinute(Train_JourneysArray[JourneyB].TrainJourney_CurrentDepartureTime, 1),
                                                                LocationTemporaryOccupation, ErrorMsg, OK);
                          END;
                      END ELSE BEGIN
                        { Or, if we're passing through a platform on our way to an adjoining platform, record that passage to protect the platform against being occupied by
                          something else that will obstruct our access, but not if we've already arrived.
                        }
                        IF OK
                        AND (TrainJourney_ActualArrivalTime = 0)
                        THEN BEGIN
                          IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp <> UnknownLocation THEN BEGIN
                            IF TrainJourney_Direction = Down THEN BEGIN
                              Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyA)
                                                    + LocationToStr(Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp)
                                                    + ' recorded as occupied until the train arrives, as it passes through it to reach '
                                                    + LocationToStr(TrainJourney_EndLocation));
                              InsertDataInLocationOccupationArray(T,
                                                                  JourneyA, JourneyB,
                                                                  Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp,
                                                                  IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                                                  IncMinute(TrainJourney_CurrentArrivalTime, +1),
                                                                  LocationTemporaryOccupation, ErrorMsg, OK);
                            END;
                          END;

                          IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown <> UnknownLocation THEN BEGIN
                            IF TrainJourney_Direction = Up THEN BEGIN
                              Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyA)
                                                    + LocationToStr(Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown)
                                                    + ' recorded as occupied until the train arrives, as it passes through it to reach '
                                                    + LocationToStr(TrainJourney_EndLocation));
                              InsertDataInLocationOccupationArray(T,
                                                                  JourneyA, JourneyB,
                                                                  Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown,
                                                                  IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                                                  IncMinute(TrainJourney_CurrentArrivalTime, +1),
                                                                  LocationTemporaryOccupation, ErrorMsg, OK);
                            END;
                          END;
                        END;

                        { And, if we're going to pass through an adjoining platform on our way out of the station, record that passage to protect the platform against being
                          occupied by something else that will obstruct our egress
                        }
                        IF OK THEN BEGIN
                          IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp <> UnknownLocation THEN BEGIN
                            IF (TrainJourney_Direction = Up)
                            AND (Train_JourneysArray[JourneyA + 1].TrainJourney_Direction = Up)
                            THEN BEGIN
                              Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyA)
                                                    + LocationToStr(Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp)
                                                    + ' recorded as occupied as the train passes through it to leave ' + LocationToStr(TrainJourney_EndLocation));
                              InsertDataInLocationOccupationArray(T,
                                                                  JourneyA, JourneyB,
                                                                  Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtUp,
                                                                  IncMinute(Train_JourneysArray[JourneyA + 1].TrainJourney_CurrentDepartureTime, -1),
                                                                  IncMinute(Train_JourneysArray[JourneyA + 1].TrainJourney_CurrentDepartureTime, +1),
                                                                  LocationTemporaryOccupation, ErrorMsg, OK);
                            END;
                          END ELSE
                            IF Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown <> UnknownLocation THEN BEGIN
                              IF (TrainJourney_Direction = Down)
                              AND (Train_JourneysArray[JourneyA + 1].TrainJourney_Direction = Down)
                              THEN BEGIN
                                Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyA)
                                                      + LocationToStr(Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown)
                                                      + ' recorded as occupied as the train passes through it to leave ' + LocationToStr(TrainJourney_EndLocation));
                                InsertDataInLocationOccupationArray(T,
                                                                    JourneyA, JourneyB,
                                                                    Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardAtDown,
                                                                    IncMinute(Train_JourneysArray[JourneyA + 1].TrainJourney_CurrentDepartureTime, -1),
                                                                    IncMinute(Train_JourneysArray[JourneyA + 1].TrainJourney_CurrentDepartureTime, +1),
                                                                    LocationTemporaryOccupation, ErrorMsg, OK);
                              END;
                            END;
                        END;
                      END;
                    END;
                  END;
                END ELSE BEGIN
                  { add the last location }
                  IF OK THEN
                    CheckJourneyLocations(T, JourneyA, IsTimetableLoading, OK);

                  IF OK OR TrainJourney_Setup THEN
                    InsertDataInLocationOccupationArray(T, JourneyA, UnknownJourney,
                                                        TrainJourney_EndLocation,
                                                        IfThenTime(TrainJourney_ActualArrivalTime <> 0,
                                                                   TrainJourney_ActualArrivalTime,
                                                                   TrainJourney_CurrentArrivalTime),
                                                        StrToTime('23:59'),
                                                        LocationEndOfDayOccupation, ErrorMsg, OK);
                END;
              END;
            END;
            IF OK OR TrainJourney_Setup THEN
              Inc(JourneyA);
          END; {WITH}
        END; {WHILE}
      END;
    END;
  END; {WITH}
END; { AddTrainOccupationsToLocationOccupations }

PROCEDURE SetUpAllLocationOccupationsAbInitio(IsTimetableLoading : Boolean; OUT OK : Boolean);
{ Set up all train locations }

  PROCEDURE AddStationaryOccupationsToLocationOccupations(OUT OK : Boolean);
  { Add existing stationary feedback and other occupations to the Location Occupation data }
  CONST
    NoTrain = NIL;
    StopTimer = True;

  VAR
    DataInserted : Boolean;
    ErrorMsg : String;
    I : Integer;
    Location : Integer;
    LocationTCs : IntegerArrayType;

  BEGIN
    OK := True;
    Location := 0;
    WHILE Location <= High(Locations) DO BEGIN
      IF Locations[Location].Location_RecordInLocationOccupationArray THEN BEGIN
        { Only record locations where trains might be stationary }
        LocationTCs := GetTrackCircuitsForLocation(Location);
        I := 0;
        DataInserted := False;
        WHILE (I <= High(LocationTCs))
        AND NOT DataInserted
        DO BEGIN
          IF (TrackCircuits[LocationTCs[I]].TC_OccupationState = TCOutOfUseSetByUser)
          OR (TrackCircuits[LocationTCs[I]].TC_OccupationState = TCOutOfUseAsNoFeedbackReceived)
          THEN BEGIN
            SetLocationOccupationAllDayState(NoLocoChip, Location, LocationOutOfUseOccupation, ErrorMsg, OK);
            IF NOT OK THEN
              Debug('!' + ErrorMsg);
          END ELSE
            IF TrackCircuits[LocationTCs[I]].TC_OccupationState = TCPermanentOccupationSetByUser THEN BEGIN
              IF TrackCircuits[LocationTCs[I]].TC_LocoChip <> UnknownLocoChip THEN
                SetLocationOccupationAllDayState(TrackCircuits[LocationTCs[I]].TC_LocoChip, Location, LocationPermanentOccupationSetByUser, ErrorMsg, OK)
              ELSE
                SetLocationOccupationAllDayState(NoLocoChip, Location, LocationPermanentOccupationSetByUser, ErrorMsg, OK);
              IF OK THEN
                DataInserted := True
              ELSE
                Debug('!' + ErrorMsg);
            END ELSE BEGIN
              IF (TrackCircuits[LocationTCs[I]].TC_OccupationState = TCPermanentFeedbackOccupation)
              OR (TrackCircuits[LocationTCs[I]].TC_OccupationState = TCPermanentSystemOccupation)
              THEN BEGIN
                IF TrackCircuits[LocationTCs[I]].TC_LocoChip <> UnknownLocoChip THEN
                  SetLocationOccupationAllDayState(TrackCircuits[LocationTCs[I]].TC_LocoChip, Location, LocationPermanentOccupationWithFeedback, ErrorMsg, OK)
                ELSE
                  SetLocationOccupationAllDayState(NoLocoChip, Location, LocationPermanentOccupationWithFeedback, ErrorMsg, OK);
                IF OK THEN
                  DataInserted := True
                ELSE
                  Debug('!' + ErrorMsg);
              END ELSE
                { We also need to record locations where there's a track circuit occupation and we don't know which train it belongs to, so we have to assume it's there
                  all day
                }
                IF (TrackCircuits[LocationTCs[I]].TC_OccupationState = TCFeedbackOccupation)
                OR (TrackCircuits[LocationTCs[I]].TC_OccupationState = TCSystemOccupation)
                THEN BEGIN
                  IF (TrackCircuits[LocationTCs[I]].TC_LocoChip = UnknownLocoChip)
                  OR (TrackCircuits[LocationTCs[I]].TC_MysteriouslyOccupied)
                  THEN
                    SetLocationOccupationAllDayState(NoLocoChip, Location, LocationPermanentOccupationWithFeedback, ErrorMsg, OK);
                END;
            END;
          Inc(I);
        END; {WHILE}
      END;
      Inc(Location);
    END; {WHILE}

    IF NOT OK THEN BEGIN
      { Now need to sort out the problem }
      Log('E Location occupation error: ' + ErrorMsg);
      IF MessageDialogueWithDefault(ErrorMsg
                                    + CRLF
                                    + 'Do you wish to start anyway or exit the program?',
                                    StopTimer, mtError, [mbYes, mbNo], ['Start', 'Exit'], mbNo) = mrNo
      THEN
        ShutDownProgram(UnitRef, 'AddStationaryOccupationsToLocationOccupations');
    END; {WITH}
  END; { AddStationaryOccupationsToLocationOccupations }

CONST
  IncludeLocationOccupationStatus = True;
  TrainExists = True;
  WriteToFile = True;

VAR
  ErrorMsg : String;
  T : TrainElement;

BEGIN
  DoCheckForUnexpectedData(UnitRef, 'SetUpAllLocationOccupationsAbInitio 1');
  ClearAllLocationOccupations;
  Log('D ADDING STATIONARY OCCUPATIONS TO LOCATION OCCUPATIONS ARRAY {BLANKLINEBEFORE}');
  AddStationaryOccupationsToLocationOccupations(OK);
  IF OK THEN BEGIN
    Log('D ADDING NON-STATIONARY OCCUPATIONS TO LOCATION OCCUPATIONS ARRAY {BLANKLINEBEFORE}');
    T := 0;
    WHILE T <= High(Trains) DO BEGIN
      WITH Trains[T] DO
        AddTrainOccupationsToLocationOccupations(T, IsTimetableLoading, ErrorMsg, OK);
      Inc(T);
    END; {WHILE}
  END;
  WriteLocationOccupations(NOT IncludeLocationOccupationStatus, NOT WriteToFile);
  DoCheckForUnexpectedData(UnitRef, 'SetUpAllLocationOccupationsAbInitio 2');
END; { SetUpAllLocationOccupationsAbInitio }

PROCEDURE SetUpTrainLocationOccupationsAbInitio(T : TrainElement; OUT OK : Boolean);
{ Set up a given train's locations }
CONST
  IsTimetableLoading = True;

VAR
  ErrorMsg : String;

BEGIN
  Log(Trains[T].Train_LocoChipStr + ' D REPLACING NON-STATIONARY OCCUPATIONS IN LOCATION OCCUPATIONS ARRAY');
  DeleteTrainLocationOccupation(T);
  AddTrainOccupationsToLocationOccupations(T, NOT IsTimetableLoading, ErrorMsg, OK);
END; { SetUpTrainLocationOccupationsAbInitio }

PROCEDURE FindPendingLocations(IsTimetableLoading : Boolean; OUT OK : Boolean);
{ Process those trains where some or all of the locations are waiting to be allocated }
CONST
  CurrentlyRouteing = True;
  EmergencyRouteing = True;
  FindNextAvailableLocation = True;
  MayReselectOldLocation = True;
  NewJourney = True;
  OmitLocoTypeRestriction = True;
  PreRouteing = True;
  RebuildRouteArray = True;
  StopTimer = True;

VAR
  ErrorMsg : String;
  Journey : Integer;
  LinesNotAvailableStr : String;
  NewLocation : Integer;
  SuccessMsg : String;
  T : TrainElement;
  TempTime : TDateTime;

BEGIN
  Log('D FINDING PENDING LOCATIONS: {BLANKLINEBEFORE}');
  T := 0;
  WHILE T <= High(Trains) DO BEGIN
    WITH Trains[T] DO BEGIN
      IF (Train_LocoChip <> UnknownLocoChip)
      AND Train_DiagramFound
      AND (Train_CurrentStatus <> Cancelled)
      AND (Train_CurrentStatus <> NonMoving)
      THEN BEGIN
        OK := True;
        IF (Train_LocoChip <> UnknownLocoChip)
        AND Train_DiagramFound
        THEN BEGIN
          Journey := 0;
          WHILE OK
          AND (Journey <= Train_TotalJourneys)
          DO BEGIN
            WITH Train_JourneysArray[Journey] DO BEGIN
              IF TrainJourney_LocationsPending THEN BEGIN
                IF TrainJourney_EndLocation = UnknownLocation THEN BEGIN
                  { Find an alternative }
                  IF NOT AlternativeAreaOrLocationAvailable(T, Journey, Train_JourneysArray[Journey].TrainJourney_EndArea, UnknownLocation, NewLocation, TempTime,
                                                            PreRouteing, NOT CurrentlyRouteing, NOT OmitLocoTypeRestriction, NOT FindNextAvailableLocation,
                                                            MayReselectOldLocation, ErrorMsg, SuccessMsg)
                  THEN
                    { try again with the loco type restriction lifted }
                    AlternativeAreaOrLocationAvailable(T, Journey, Train_JourneysArray[Journey].TrainJourney_EndArea, UnknownLocation, NewLocation, TempTime, PreRouteing,
                                                       NOT CurrentlyRouteing, OmitLocoTypeRestriction, FindNextAvailableLocation, MayReselectOldLocation, ErrorMsg,
                                                       SuccessMsg);

                  IF NewLocation = UnknownLocation THEN
                    OK := False;

                  IF OK THEN BEGIN
                    Train_JourneysArray[Journey].TrainJourney_EndLocation := NewLocation;
                    Train_JourneysArray[Journey].TrainJourney_EndArea := Locations[NewLocation].Location_Area;
                    Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                          + 'unknown end location has been replaced by ' + LocationToStr(NewLocation, ShortStringType));

                    { Recreate the journey, to add the missing location }
                    CreateJourney(T, Journey,
                                  NOT NewJourney,
                                  TrainJourney_StartArea, TrainJourney_EndArea,
                                  TrainJourney_StartLocation, TrainJourney_EndLocation,
                                  TrainJourney_DiagrammedStartLocation, TrainJourney_DiagrammedEndLocation,
                                  UnknownLine, UnknownLine,
                                  TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime,
                                  TrainJourney_CurrentArrivalTime,
                                  TrainJourney_Direction,
                                  TrainJourney_RouteArray,
                                  NOT RebuildRouteArray,
                                  TrainJourney_StoppingOnArrival,
                                  TrainJourney_NotForPublicUse,
                                  NOT EmergencyRouteing,
                                  TrainJourney_StartOfRepeatJourney,
                                  IsTimetableLoading,
                                  ErrorMsg,
                                  LinesNotAvailableStr, OK);

                    IF NOT OK THEN
                      Log(Train_LocoChipStr + ' XG J=' + IntToStr(Journey) + ': error in creating journey: ' + ErrorMsg)
                    ELSE BEGIN
                      IF Journey < Train_TotalJourneys THEN BEGIN
                        { If there's an end location unknown, then the following start location (the same place) will also been unknown - but we need to make sure we fully
                          fit in it - for instance, a long train arriving down into platform 5B will, if it's leaving in the up direction, have to start from platform 5A -
                          otherwise the up signal at platform 5B would need to come off - and it couldn't, as the train would probably be occupying the preceding track
                          circuit.
                        }
                        IF Locations[NewLocation].Location_Area = StrToArea('MainStationArea') THEN BEGIN
                          IF Train_JourneysArray[Journey + 1].TrainJourney_Direction <> Train_JourneysArray[Journey].TrainJourney_Direction
                          THEN BEGIN
                            IF Train_CurrentLengthInInches > Locations[NewLocation].Location_LengthInInches THEN BEGIN
                              IF Train_JourneysArray[Journey + 1].TrainJourney_Direction = Up THEN BEGIN
                                Log(Train_LocoChipStr + ' D Replacing ' + LocationToStr(NewLocation, ShortStringType) + ' with '
                                                      + LocationToStr(Locations[NewLocation].Location_PlatformOrFiddleyardAtUp, ShortStringType)
                                                      + 'as the train will not fit in the former location');
                                NewLocation := Locations[NewLocation].Location_PlatformOrFiddleyardAtUp;
                              END ELSE
                                IF Train_JourneysArray[Journey + 1].TrainJourney_Direction = Down THEN BEGIN
                                  Log(Train_LocoChipStr + ' D Replacing ' + LocationToStr(NewLocation, ShortStringType) + ' with '
                                                        + LocationToStr(Locations[NewLocation].Location_PlatformOrFiddleyardAtUp, ShortStringType)
                                                        + 'as the train will not fit in the former location');
                                  NewLocation := Locations[NewLocation].Location_PlatformOrFiddleyardAtDown;
                                END;
                              IF NewLocation = UnknownLocation THEN
                                { hopefully we will not get here! }
                                Log(Train_LocoChipStr + ' XG Error in FindPendingLocations - replacement location is unknown');
                            END;
                          END;
                        END;

                        Train_JourneysArray[Journey + 1].TrainJourney_StartLocation := NewLocation;
                        Train_JourneysArray[Journey + 1].TrainJourney_StartArea := Locations[NewLocation].Location_Area;
                        Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey + 1)
                                              + 'unknown start location has been replaced by ' + LocationToStr(NewLocation, ShortStringType));
                        WITH Train_JourneysArray[Journey + 1] DO
                          { recreate the next journey, to add the missing location - it doesn't matter it may just be the start location we're adding, as we may already
                            know what the end location is.
                          }
                          CreateJourney(T, Journey + 1, NOT NewJourney,
                                        TrainJourney_StartArea, TrainJourney_EndArea,
                                        TrainJourney_StartLocation, TrainJourney_EndLocation,
                                        TrainJourney_DiagrammedStartLocation, TrainJourney_DiagrammedEndLocation,
                                        UnknownLine, UnknownLine,
                                        TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime,
                                        TrainJourney_CurrentArrivalTime,
                                        TrainJourney_Direction,
                                        TrainJourney_RouteArray,
                                        NOT RebuildRouteArray,
                                        TrainJourney_StoppingOnArrival,
                                        TrainJourney_NotForPublicUse,
                                        NOT EmergencyRouteing,
                                        TrainJourney_StartOfRepeatJourney,
                                        IsTimetableLoading,
                                        ErrorMsg, LinesNotAvailableStr, OK);

                          { but the new start location doesn't match the existing end location (perhaps because there's no route between them) then we'll have to backtrack }
                          IF NOT OK THEN BEGIN
                            Log(Train_LocoChipStr + ' X Error in FindPendingLocations');
                            { A permanent breakpoint in case the temporary one gets moved or lost: }
                            IF DebugHook <> 0 THEN BEGIN
                              ASM
                                Int 3
                              END; {ASM}
                            END;
                          END;
                      END;
                    END;
                  END;
                END;
              END;
            END; {WITH}
            IF OK THEN
              Inc(Journey);
          END; {WHILE}

          IF NOT OK THEN BEGIN
            { Now need to sort out the problem }
            Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(Journey)
                                  + 'did not find a suitable destination at ' + AreaToStr(Train_JourneysArray[Journey].TrainJourney_EndArea)
                                  + ' in journey ' + DirectionToStr(Train_JourneysArray[Journey].TrainJourney_Direction)
                                  + ' from ' + LocationToStr(Train_JourneysArray[Journey].TrainJourney_StartLocation)
                                  + IfThen(ErrorMsg <> '',
                                           ': ' + ErrorMsg));
            IF MessageDialogueWithDefault('Loco ' + LocoChipToStr(Train_LocoChip) + ' J=' + IntToStr(Journey) + ': '
                                          + 'problem with finding a suitable destination at ' + AreaToStr(Train_JourneysArray[Journey].TrainJourney_EndArea)
                                          + ' in journey ' + DirectionToStr(Train_JourneysArray[Journey].TrainJourney_Direction)
                                          + ' from ' + LocationToStr(Train_JourneysArray[Journey].TrainJourney_StartLocation)
                                          + IfThen(ErrorMsg <> '',
                                                   ': ' + ErrorMsg)
                                          + CRLF
                                          + 'Do you wish to start (and cancel loco ' + LocoChipToStr(Train_LocoChip) + ') or exit the program?',
                                          StopTimer, mtError, [mbYes, mbNo], ['&Start', '&Exit'], mbYes) = mrNo
            THEN
              ShutDownProgram(UnitRef, 'FindPendingLocations')
            ELSE BEGIN
              OK := True;
              CancelTrain(T, ByUser, TrainExists);
            END;
          END;
        END;
      END;
    END; {WITH}
    Inc(T);
  END; {WHILE}
END; { FindPendingLocations }

END { LocationData }.
