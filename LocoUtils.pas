UNIT LocoUtils;
{ Various loco utilities are here, including maintaining the various loco tables

  LE131XF fitted 31/12/02 to 429 and 6607;
  Lights too bright, so dimmed - cvs 51 + 57 - set to 6 (bits 2 and 3 set) (dimming allowed); cvs 52 + 58 (brightness) set to 32 (bit 6)
}

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Startup, InitVars, StdCtrls, Route, DB, ADODB, Grids, DBGrids;

TYPE
  TLocoUtilsWindow = CLASS(TForm)
    LocoDataADOConnection : TADOConnection;
    LocoDataADOTable : TADOTable;
    LocoDataDataSource : TDataSource;
    LocoLocationsADOConnection : TADOConnection;
    LocoLocationsADOTable : TADOTable;
    LocoLocationsDataSource : TDataSource;
    LocoStringGrid : TStringGrid;
    LocoUtilsSortLabel : TLabel;
    PROCEDURE LocoStringGridDblClick(Sender: TObject);
    PROCEDURE LocoStringGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    PROCEDURE LocoStringGridKeyDown(Sender : TObject; VAR Key : Word; ShiftState : TShiftState);
    PROCEDURE LocoStringGridMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    PROCEDURE LocoUtilsLocoChipButtonKeyDown(Sender : TObject; VAR Key : Word; ShiftState : TShiftState);
    PROCEDURE LocoUtilsLocoNumButtonKeyDown(Sender : TObject; VAR Key : Word; ShiftState : TShiftState);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

PROCEDURE InitialiseLocoUtilsUnit;
{ Initialises the unit }

PROCEDURE ListLocosByChip;
{ Create a list by loco chip }

PROCEDURE ReadInLocoDataFromDatabase(VAR OK : Boolean);
{ Read in the loco table data from the MSAccess file - the data is not read in loco chip order, but that does not matter }

PROCEDURE WriteOutLocoDataToDatabase;
{ Write out some loco data to the loco data file }

VAR
  LocoUtilsWindow: TLocoUtilsWindow;

IMPLEMENTATION

{$R *.dfm}

USES GetTime, Lenz, Diagrams, MiscUtils, RailDraw, Types, Math {sic}, IDGlobal, StrUtils, Feedback, Input, LocoDialogue, Options, Main;

CONST
  UnitRef = 'LocoUtils';

TYPE
  LocoUtilsWindowType = (ListedByLocoClass, ListedByLocoName, ListedByChip, ListedByLocoNumber);
  SortGridDirectionType = (Up, Down);

VAR
  SaveLocoStringGridSearchCol : Integer = 0;
  SaveLocoStringGridSearchRow : Integer = 0;
  SaveLocoStringGridSearchStr : String = '';
  SaveSortGridColNum : Integer = 0;
  SortGridDirection : SortGridDirectionType = Up;
  TypeOfLocoUtilsWindow : LocoUtilsWindowType;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE InitialiseLocoRecord(L : LocoIndex);
{ Do the initialisations, or reinitialisations if specified }
VAR
  I : Integer;

BEGIN
  TRY
    IF L = UnknownLocoIndex THEN
      UnknownLocoRecordFound('InitialiseLocoRecord')
    ELSE BEGIN
      WITH Locos[L] DO BEGIN
  //      Train_DiagramFound := False;

        { Set up defaults }
        Loco_Accelerating := False;
        Loco_AccelerationAdjustRange := 0;
        Loco_AccelerationStartTime := 0;
        Loco_AccelerationStr := '';
        Loco_AccelerationTimeInterval := 0.0;
        Loco_Active := False;
        Loco_ActualNumStr := '';
  //      Train_AtCurrentBufferStop := UnknownBufferStop;
  //      Train_AtCurrentSignal := UnknownSignal;
  //      Train_AtHiddenAspectSignal := UnknownSignal;
  //      Train_BeingAdvanced := False;
  //      Train_BeingAdvancedTC := UnknownTrackCircuit;
        Loco_ControlState := ControlledByProgram;
  //      Train_ControlledByProgram := False;
  //      Train_ControlledByRDC := False;
  //      Train_CurrentArrivalTime := 0;
  //      Train_CurrentBufferStop := UnknownBufferStop;
  //      Train_CurrentJourney := 0;
  //      Train_CurrentLengthInInches := 0;
        Loco_CurrentLenzSpeed := 0;
  //      Train_CurrentRoute := UnknownRoute;
  //      Train_CurrentSignal := UnknownSignal;
  //      Train_CurrentSourceLocation := UnknownLocation;
  //      Train_CurrentSpeedInMPH := MPH0;
  //      Train_CurrentStatus := UnknownTrainStatus;
  //      Train_CurrentTC := UnknownTrackCircuit;
        Loco_Decelerating := False;
        Loco_Description := '';
        Loco_DesiredLenzSpeed := 0;
  //      Train_DesiredSpeedInMPH := MPH0;
        Loco_DiagramFound := False;
        Loco_CurrentDirection := UnknownDirection;
  //      Train_DistanceToCurrentSignalOrBufferStop := 0.0;
  //      Train_DistanceToNextSignalButOneOrBufferStop := 0.0;
  //      Train_DistanceToNextSignalOrBufferStop := 0.0;
  //      Train_DoubleHeaderLocoChip := UnknownLocoChip;
  //      Train_EmergencyRouteing := False;
  //      Train_ExtraPowerAdjustment := 0; { used temporarily to increase the train speed where necessary }
  //      Train_FirstStationSpecifiedStartTime := 0;
        Loco_FixedDirection := UnknownDirection;
        Loco_FixedLengthInInches := 0;
  //      FOR I := 0 TO 12 DO
  //        Train_Functions[I] := False;
  //
  //      Train_Functions0To4Byte := 0;
  //      Train_Functions5To12Byte := 0;
  //      Train_GradientSpeedAdjustment := 0;
  //      Train_GradientSpeedAdjustmentMsgWritten := False;
        Loco_HasCabLights := False;
  //      Train_Headcode := '';
        Loco_HomeArea := UnknownArea;
  //      FOR I := 1 TO 5 DO
  //        Train_InitialTrackCircuits[I] := UnknownTrackCircuit;
  //
  //      Train_InLightsOnTime := False; { train inactive but for lights being on }
  //      FOR I := 1 TO 9 DO
  //        Train_RouteCreationHeldMsgWrittenArray[I] := False;
  //
  //      Train_RouteCreationPlatformHeldStr := '';
  //      SetLength(Train_JourneysArray, 0);
        Loco_LastLengthInInches := 0;
        Loco_LastLocation := UnknownLocation;
  //      Train_LastMissingTC := UnknownTrackCircuit;
  //      Train_LastRouteLockedMsgStr := '';
  //      Train_LastSignal := UnknownSignal;
        Loco_LastTC := UnknownTrackCircuit;
        Loco_LightingChipDown := UnknownLocoChip;
        Loco_LightingChipDownIndex := UnknownLocoIndex;
        Loco_LightingChipUp := UnknownLocoChip;
        Loco_LightingChipUpIndex := UnknownLocoIndex;
        Loco_LightingChipRecordForChip := UnknownLocoChip;
        Loco_LightsMsg := '';
        Loco_LightsOn := False;
  //      Train_LightsOnTime := 0;
  //      Train_LightsRemainOnWhenJourneysComplete := True;
        Loco_LightsType := NoLights;
  //      Train_LocatedAtStartup := True;
  //      SetLength(Train_Locations, 0);
        Loco_LocoChip := UnknownLocoChip;
        Loco_LocoChipStr := '';
        Loco_LocoClassStr := '';
        Loco_LocoName := '';
        Loco_LocoTypeStr := '';
  //      Train_MaximumSpeedInMPH := MPH0;
  //      Train_MinimumAccelerationTimeInSeconds := 0; { needed as we only calculate it once when we enter a track circuit }
  //      Train_MissingMessage := False;
  //      Train_MissingNum := 0; { set if train missing, and used to restart train if found }
  //      Train_NextTC := UnknownTrackCircuit;
  //      Train_NextButOneTC := UnknownTrackCircuit;
  //      Train_NotInPlaceMsgWritten := False;
  //      Train_NotLocatedAtStartupMsgWritten := False;
        Loco_NumberOfCarriages := 0;
  //      Train_PossibleRerouteTime := 0;
  //      Train_PreviouslyControlledByProgram := False;
        Loco_PreviousControlState := ControlledByProgram;
  //      Train_PreviousStatus := ReadyForCreation;
  //      Train_PreviousTC := UnknownTrackCircuit;
  //      Train_Reversing := False;
  //      Train_ReversingDelayInSeconds := 0;
  //      Train_ReversingStartTime := 0;
  //      Train_ReversingWaitStarted := False;
  //      Train_RouteCheckedTime := 0;
  //      Train_RouteCreationHeldJourney := UnknownJourney;
  //      Train_RouteCreationHoldMsg := '';
  //      Train_RouteCreationHoldNum := 0;
  //      Train_RouteCreationReleasedMsg := '';
  //      Train_RouteingHeldAtSignal := UnknownSignal;
  //      Train_SaveCurrentTC := UnknownTrackCircuit;
        Loco_SaveDesiredLenzSpeed := 0;
        Loco_SavedDirection := UnknownDirection;
  //      Train_SavedLocation := UnknownLocation;
  //      Train_SavedRoute := UnknownRoute;
  //      Train_SaveSpeedInFiddleyardMsg := '';
  //      Train_SaveTCsClearedStr := '';
  //      Train_SaveTCsForReleaseStr := '';
  //      Train_SaveTCsOccupiedStr := '';
  //      Train_SectionStartTime := 0;
        Loco_Speed10 := 0;
        Loco_Speed20 := 0;
        Loco_Speed30 := 0;
        Loco_Speed40 := 0;
        Loco_Speed50 := 0;
        Loco_Speed60 := 0;
        Loco_Speed70 := 0;
        Loco_Speed80 := 0;
        Loco_Speed90 := 0;
        Loco_Speed100 := 0;
        Loco_Speed110 := 0;
        Loco_Speed120 := 0;
        FOR I := 1 TO 12 DO
          Loco_SpeedArray[I] := 0;
        Loco_SpeedByte := 0;
        Loco_SpeedByteReadIn := False;
        Loco_SpeedSettingsMissing := False;
        Loco_SpeedStepMode := 28; { this is the default - it could be added to the loco database *** }
        Loco_SpeedString := '';

  //      Train_StalledMsgWritten := False;
  //      Train_TakenOverByUserMsgWritten := False;
  //
  //      SetLength(Train_TCsAndSignalsNotClearedArray, 0);
  //      Train_TCsAndSignalsNotClearedStr := '';
  //
  //      SetLength(Train_TCsNotClearedArray, 0);
  //      Train_TCsNotClearedStr := '';
  //
  //      SetLength(Train_TCsOccupiedOrClearedArray, 0);
  //      Train_TCsOccupiedOrClearedStr := '';
  //
  //      SetLength(Train_TCsReleasedArray, 0);
  //      Train_TCsReleasedStr := '';
  //
  //      SetLength(Train_TempDraftRouteArray, 0);
  //      SetLength(Train_TempLockingArray, 0);
  //      Train_TerminatingSpeedReductionMsgWritten := False;
  //      SetLength(Train_DiagramsGridRowNums, 0);
        Loco_TrainIndex := UnknownTrainIndex;
  //      Train_TotalJourneys := -1;
  //      Train_Type := UnknownTrainType;
  //      Train_TypeNum := 0;
  //      Train_UserDriving := False;
  //      Train_UserPowerAdjustment := 0; { used by the user to increase or decrease the train speed where necessary }
  //      Train_UserRequiresInstructions := False;
  //      Train_UserRequiresInstructionMsg := '';
        Loco_UseTrailingTrackCircuits := False; { where a train doesn't have lights at both ends, it may need artificial track circuit activation to allow the whole length
                                                  of the train to be detected }
  //      Train_WaitingForHiddenAspectStartTime := 0;
  //      Train_WorkingTimetableLastArrivalArea := UnknownArea;
  //      Train_WorkingTimetableLastArrivalTime := 0;
  //      Train_WorkingTimetableLastEntryNumStr := '';
  //    END; {WITH}
  //  EXCEPT {TRY}
  //    ON E : Exception DO
  //      Log('EG InitialiseTrainRecord: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  //  END; {TRY}
  //END; { InitialiseTrainRecord }
      END; {WITH}
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG InitialiseLocoRecord: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { InitialiseLocoRecord }

PROCEDURE ReadInLocoDataFromDatabase(VAR OK : Boolean);
{ Read in the loco table data from the MSAccess file - the data is not read in loco chip order, but that does not matter }
CONST
  DescribeFullTrainList = True;
  StopTimer = True;

VAR
  ElementPos : Integer;
  ErrorMsg : String;
  I : Integer;
  InputErrorMsg : String;
  InputFoundOrCancelled : Boolean;
  J : Integer;
  L : LocoIndex;
  LightSettingCount : Integer;
  NewLocationStr : String;
  SpeedFound : Boolean;
  TempL : TrainIndex;
  TempStr : String;
  TempLocations : IntegerArrayType;
  TempLocationsLocoChips : IntegerArrayType;
  TempTCs : IntegerArrayType;
  TempTCsLocoChips : IntegerArrayType;
  TestInt : Integer;

BEGIN
  TRY
    OK := False;

    WITH LocoUtilsWindow DO BEGIN
      Log('L INITIALISING LOCO DATA {BLANKLINEBEFORE}');
      SetLength(TempLocations, 0);
      SetLength(TempLocationsLocoChips, 0);
      SetLength(TempTCs, 0);
      SetLength(TempTCsLocoChips, 0);

      IF NOT FileExists(PathToRailDataFiles + LocoDataFilename + '.' + LocoDataFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Loco database file "' + PathToRailDataFiles + LocoDataFilename + '.' + LocoDataFilenameSuffix + '"'
                                      + ' cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInLocoDataFromDatabase')
        ELSE BEGIN
          OK := False;
          Exit;
        END;
      END;

      LocoDataADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                + PathToRailDataFiles + LocoDataFilename + '.' + LocoDataFilenameSuffix
                                                + ';Persist Security Info=False';
      LocoUtilsWindow.LocoDataADOConnection.Connected := True;
      LocoDataADOTable.Open;
      Log('L Loco Data table and connection opened to initialise the loco data');

      ErrorMsg := '';

      { First do a check to see if there's a Dapol cleaning wagon - we need to do this first before we can see if any loco is pulling it } { *********** }
//      LocoDataADOTable.First;
//      WHILE NOT LocoDataADOTable.EOF DO BEGIN
//        WITH LocoDataADOTable DO BEGIN
//          IF FieldByName('Non-Loco').AsBoolean THEN BEGIN
//            IF FieldByName('Non-Loco').AsBoolean AND (FieldByName('Non-LocoType').AsString = '') THEN
//               ErrorMsg := 'Error in database: cannot have a non-loco without a non-loco type';
//
//            IF ErrorMsg = '' THEN BEGIN
//              IF FieldByName('Non-LocoType').AsString = 'Dapol Cleaning Wagon' THEN BEGIN
//                { Check one hasn't been found already, as there can't be more than one or we will get terribly confused }
//                IF DapolCleaningWagonLocoChip <> UnknownLocoChip THEN
//                  ErrorMsg := 'Error in database - cannot have two Dapol cleaning wagons - we already have ' + LocoChipToStr(DapolCleaningWagonLocoChip)
//                ELSE
//                  DapolCleaningWagonLocoChip := FieldByName('LocoChip').AsInteger;
//              END;
//            END;
//          END;
//        END; {WITH}
//        LocoDataADOTable.Next;
//      END; {WHILE}

      { Now read in the real loco data }
      LocoDataADOTable.First;
      WHILE NOT LocoDataADOTable.EOF DO BEGIN
        WITH LocoDataADOTable DO BEGIN

          { Create a new loco record }
          SetLength(Locos, Length(Locos) + 1);
          L := High(Locos);

          WITH Locos[L] DO BEGIN
            IF ErrorMsg = '' THEN BEGIN
              InitialiseLocoRecord(L);

              { Now the data from the database }
              Loco_LocoChip := FieldByName('LocoChip').AsInteger;
              Loco_LocoChipStr := LocoChipToStr(Loco_LocoChip);
              Loco_ActualNumStr := FieldByName('ActualLocoNum').AsString;
              Loco_LocoName := FieldByName('LocoName').AsString;
              Loco_LocoClassStr := FieldByName('LocoClass').AsString;
              Loco_LocoTypeStr := FieldByName('LocoType').AsString;

              { Where the loco last stopped and how its last train was }
              TempStr := FieldByName('LastTC').AsString;
              IF TempStr <> '' THEN
                Loco_LastTC := StrToInt(TempStr);

              { but check that something else isn't already there }
              IF IsElementInIntegerArray(TempTCs, Loco_LastTC, ElementPos) THEN
                ErrorMsg := 'Error in database for loco ' + Loco_LocoChipStr + ': last TC=' + IntToStr(Loco_LastTC)
                            + ' is already occupied by loco ' + LocoChipToStr(TempTCsLocoChips[ElementPos])
              ELSE
                IF Loco_LastTC <> UnknownTrackCircuit THEN BEGIN
                  AppendToIntegerArray(TempTCs, Loco_LastTC);
                  AppendToIntegerArray(TempTCsLocoChips, Loco_LocoChip);
                END;
            END;

            IF ErrorMsg = '' THEN
              Loco_HomeArea := StrToArea(FieldByName('HomeArea').AsString);

            IF ErrorMsg = '' THEN BEGIN
              Loco_LastLocation := StrToLocation(FieldByName('LastLocation').AsString);
              IF Loco_LastLocation <> UnknownLocation THEN BEGIN
                IF NOT IsElementInLocationArray(TempLocations, Loco_LastLocation, ElementPos) THEN BEGIN
                  AppendToLocationArray(TempLocations, Loco_LastLocation);
                  AppendToIntegerArray(TempLocationsLocoChips, Loco_LocoChip);
                END ELSE BEGIN
                  CASE MessageDialogueWithDefault('Error in database for loco ' + Loco_LocoChipStr + ': last location - '
                                                  + LocationToStr(Loco_LastLocation) + ' - is already occupied by loco ' + LocoChipToStr(TempLocationsLocoChips[ElementPos])
                                                  + '.'
                                                  + CRLF
                                                  + 'Amend last location record for ' + Loco_LocoChipStr
                                                  + ' or delete last location (' + LocationToStr(Loco_LastLocation)
                                                  + ') for ' + LocoChipToStr(TempLocationsLocoChips[ElementPos]) + '?',
                                                  StopTimer, mtError, [mbYes, mbNo], ['&Amend', '&Delete'], mbYes)
                  OF
                    mrYes: { Amend }
                      BEGIN
                        StopSystemTimer;
                        InputFoundOrCancelled := False;
                        InputErrorMsg := 'Enter new location for loco ' + Loco_LocoChipStr;
                        REPEAT
                          IF NOT InputQuery('Enter train location', InputErrorMsg, NewLocationStr) THEN
                            InputFoundOrCancelled := True
                          ELSE BEGIN
                            IF NewLocationStr = 'CANCEL' THEN BEGIN
                              Loco_LastTC := UnknownTrackCircuit;
                              Loco_LastLocation := UnknownLocation;
                              ErrorMsg := '';
                              InputFoundOrCancelled := True;
                              Log(Loco_LocoChipStr + ' D No start location set by user');
                              { Note: DrawScreen needed here as InputQuery leaves the screen sized as a small rectangle }
                              InvalidateScreen(UnitRef, 'CreateTrainRecord');
                            END ELSE BEGIN
                              Loco_LastLocation := StrToLocation(NewLocationStr);
                              IF Loco_LastLocation = UnknownLocation THEN
                                InputErrorMsg := 'Invalid location entered. Please type in present location of ' + Loco_LocoChipStr + ' or CANCEL to suspend it'
                              ELSE BEGIN
                                Loco_LastTC := UnknownTrackCircuit;
                                ErrorMsg := '';
                                InputFoundOrCancelled := True;
                                Log(Loco_LocoChipStr + ' D Start location set by user to ' + NewLocationStr);
                                { Note: DrawScreen needed here as InputQuery leaves the screen sized as a small rectangle }
                                InvalidateScreen(UnitRef, 'CreateTrainRecord');
                              END;
                            END;
                          END;
                        UNTIL InputFoundOrCancelled;

                        IF Loco_LastLocation = UnknownLocation THEN
                          Log(Loco_LocoChipStr + ' LG Deleting last location record as location is marked as occupied by '
                                                + LocoChipToStr(TempLocationsLocoChips[ElementPos]));
                        StartSystemTimer;
                      END;
                    mrNo: { Delete }
                      BEGIN
                        Log(LocoChipToStr(TempLocationsLocoChips[ElementPos])
                            + ' LG Deleting last location record as location is marked as occupied by ' + Loco_LocoChipStr);
                        TempL := 0;
                        WHILE TempL <= High(Locos) DO BEGIN
                          IF (ElementPos <= High(TempLocationsLocoChips)) AND (Locos[TempL].Loco_LocoChip = TempLocationsLocoChips[ElementPos]) THEN BEGIN
                            DeleteElementFromIntegerArray(TempLocationsLocoChips, ElementPos);
                            DeleteElementFromIntegerArray(TempLocations, ElementPos);
                            ErrorMsg := '';

                            Locos[TempL].Loco_LastTC := UnknownTrackCircuit;
                            Locos[TempL].Loco_LastLocation := UnknownLocation;
                          END;
                          Inc(TempL);
                        END; {WHILE}
                      END;
                  END; {CASE}
                END;
              END;
            END;

            IF ErrorMsg = '' THEN BEGIN
              IF NOT FieldByName('LocoActive').AsBoolean THEN
                Loco_SpeedSettingsMissing := True
              ELSE BEGIN
                Loco_Active := True;

                { "Carriages" is stored as a string in the database to avoid cluttering up the column with zeroes }
                IF FieldByName('Carriages').AsString = '' THEN
                  Loco_NumberOfCarriages := 0
                ELSE
                  IF NOT TryStrToInt(FieldByName('Carriages').AsString, Loco_NumberOfCarriages) THEN
                    ErrorMsg := 'Error in database for loco ' + Loco_LocoChipStr + ': number of carriages is not an integer';

                IF ErrorMsg = '' THEN BEGIN
                  { FixedTrainLength is stored as a string in the database to avoid cluttering up the column with zeroes }
                  IF FieldByName('FixedTrainLength').AsString = '' THEN
                    Loco_FixedLengthInInches := 0
                  ELSE
                    IF NOT TryStrToInt(FieldByName('FixedTrainLength').AsString, Loco_FixedLengthInInches) THEN
                      ErrorMsg := 'Error in database for loco ' + Loco_LocoChipStr + ': last loco/train length is not an integer';
                END;

                IF ErrorMsg = '' THEN BEGIN
                  { LastTrainLength is stored as a string in the database to avoid cluttering up the column with zeroes }
                  IF FieldByName('LastTrainLength').AsString = '' THEN
                    Loco_LastLengthInInches := 0
                  ELSE
                    IF NOT TryStrToInt(FieldByName('LastTrainLength').AsString, Loco_LastLengthInInches) THEN
                      ErrorMsg := 'Error in database for loco ' + Loco_LocoChipStr + ': last loco/train length is not an integer';
                END;

                IF ErrorMsg = '' THEN BEGIN
                  { Note the fixed train direction (if any) which is needed for trains hauling coaches or wagons }
                  Loco_FixedDirection := StrToDirectionType(FieldByName('FixedTrainDirection').AsString);
                  Loco_SavedDirection := StrToDirectionType(FieldByName('LastTrainDirection').AsString);

                  IF FieldByName('Description').AsString <> '' THEN
                    Loco_Description := FieldByName('Description').AsString
                  ELSE
                    Loco_Description := '';

                  IF FieldByName('NoTrailingTrackCircuits').AsBoolean THEN
                    Loco_UseTrailingTrackCircuits := False
                  ELSE
                    Loco_UseTrailingTrackCircuits := True;

                  IF FieldByName('CabLights').AsBoolean THEN
                    Loco_HasCabLights := True;
                END;

                IF ErrorMsg = '' THEN BEGIN
                  { The following are mutually exclusive - LightSettingCount is used to detect if more than one is selected }
                  LightSettingCount := 0;

                  IF FieldByName('OrdinaryLights').AsBoolean THEN BEGIN
                    Loco_LightsType := HeadlightsAndTailLightsConnected;
                    Inc(LightSettingCount);
                  END;

                  IF FieldByName('LightsDimmed').AsBoolean THEN BEGIN
                    Loco_LightsType := LightsShouldBeDimmed;
                    Inc(LightSettingCount);
                  END;

                  IF FieldByName('ExpressModelsKit').AsBoolean THEN BEGIN
                    Loco_LightsType := ExpressModelsSeparateHeadlights;
                    Inc(LightSettingCount);
                  END;

                  IF FieldByName('TailLightsSwitched').AsBoolean THEN BEGIN
                    Loco_LightsType := HeadlightsAndTailLightsSeparatelySwitched;
                    Inc(LightSettingCount);
                  END;

                  IF FieldByName('CustomLightingKit').AsBoolean THEN BEGIN
                    Loco_LightsType := CustomLightingkit;
                    Inc(LightSettingCount);
                  END;

                  IF FieldByName('LightsFromTwoChips').AsBoolean THEN BEGIN
                    Loco_LightsType := LightsOperatedByTwoChips;
                    TempStr := FieldByName('LightingChipUp').AsString;
                    IF (TempStr = '') OR (TempStr = '0') THEN
                      ErrorMsg := 'Error in database for loco ' + Loco_LocoChipStr + ': ''Lights From Two Chips'' ticked but no chip number supplied'
                    ELSE BEGIN
                      Loco_LightingChipUp := StrToInt(TempStr);
                      TempStr := FieldByName('LightingChipDown').AsString;
                      IF (TempStr = '') OR (TempStr = '0') THEN
                        ErrorMsg := 'Error in database for loco ' + Loco_LocoChipStr + ': ''Lights From Two Chips'' ticked but no chip number supplied'
                      ELSE
                        Loco_LightingChipDown := StrToInt(TempStr);
                    END;

                    Inc(LightSettingCount);
                  END ELSE BEGIN
                    IF FieldByName('LightingChipUp').AsInteger > 0 THEN
                      ErrorMsg := 'Error in database for loco ' + Loco_LocoChipStr
                                  + ': lighting chip number up ' + LocoChipToStr(FieldByName('LightingChipUp').AsInteger)
                                  + ' supplied but ''Lights From Two Chips'' not ticked'
                    ELSE
                      IF FieldByName('LightingChipDown').AsInteger > 0 THEN
                        ErrorMsg := 'Error in database for loco ' + Loco_LocoChipStr
                                    + ': lighting chip number down ' + LocoChipToStr(FieldByName('LightingChipDown').AsInteger)
                                    + ' supplied but ''Lights From Two Chips'' not ticked';
                  END;

                  IF LightSettingCount > 1 THEN
                    ErrorMsg := 'Error in database for loco ' + Loco_LocoChipStr + ': more than one lighting option ticked';
                END;

                IF ErrorMsg = '' THEN BEGIN
                  IF NOT FieldByName('Non-Loco').AsBoolean THEN BEGIN
                    { Now deal with speed settings }
                    Loco_SpeedArray[1] := FieldByName('Speed10').AsInteger;
                    FOR I := 2 TO 12 DO
                      Loco_SpeedArray[I] := FieldByName('Speed' + IntToStr(I) + '0').AsInteger;

                    { First see if there any speeds here (speeds are added by hand to the database once the speed test is completed) by seeing if any of the speed settings
                      are non-zero
                    }
                    TestInt := 0;
                    FOR I := 1 TO 12 DO
                      TestInt := TestInt + Loco_SpeedArray[I];
                    IF TestInt = 0 THEN BEGIN
                      { there are no speeds in the database }
                      Log(Loco_LocoChipStr + ' L Loco has no speed settings in the loco database');
                      Loco_SpeedSettingsMissing := True;
                    END ELSE BEGIN
                      { Find the minimum speed and add to all lower speed settings }
                      I := 1;
                      SpeedFound := False;
                      WHILE (I <= 12) AND NOT SpeedFound DO BEGIN
                        IF Loco_SpeedArray[I] <> 0 THEN BEGIN
                          SpeedFound := True;
                          { now add to all speed settings below }
                          FOR J := 1 TO (I - 1) DO
                            Loco_SpeedArray[J] := Loco_SpeedArray[I];
                        END;
                        Inc(I);
                      END; {WHILE}

                      { Find the maximum speed, and add to all higher speed settings }
                      I := 12;
                      SpeedFound := False;
                      WHILE (I >= 1) AND NOT SpeedFound DO BEGIN
                        IF Loco_SpeedArray[I] <> 0 THEN BEGIN
                          SpeedFound := True;
                          { and store the maximum speed }
                          CASE I OF
                            1:
                              Loco_MaximumSpeedInMPH := MPH10;
                            2:
                              Loco_MaximumSpeedInMPH := MPH20;
                            3:
                              Loco_MaximumSpeedInMPH := MPH30;
                            4:
                              Loco_MaximumSpeedInMPH := MPH40;
                            5:
                              Loco_MaximumSpeedInMPH := MPH50;
                            6:
                              Loco_MaximumSpeedInMPH := MPH60;
                            7:
                              Loco_MaximumSpeedInMPH := MPH70;
                            8:
                              Loco_MaximumSpeedInMPH := MPH80;
                            9:
                              Loco_MaximumSpeedInMPH := MPH90;
                            10:
                              Loco_MaximumSpeedInMPH := MPH100;
                            11:
                              Loco_MaximumSpeedInMPH := MPH110;
                            12:
                              Loco_MaximumSpeedInMPH := MPH120;
                          END; {CASE}

                          { now add to all speed settings below }
                          FOR J := (I + 1) TO 12 DO
                            Loco_SpeedArray[J] := Loco_SpeedArray[I];
                        END;
                        Dec(I);
                      END; {WHILE}

                      { But it might happen (has happened!) that, accidentally, an intervening speed setting is missing, or a speed step is lower than the one preceding it }
                      I := 1;
                      SpeedFound := True;
                      WHILE (I <= 12) AND SpeedFound DO BEGIN
                        IF Loco_SpeedArray[I] = 0 THEN BEGIN
                          SpeedFound := False;
                          Log(Loco_LocoChipStr + ' L Missing speed step at position ' + IntToStr(I - 1));
                          ErrorMsg := 'Loco ' + Loco_LocoChipStr + ': missing speed step at position ' + IntToStr(I - 1);
                          Loco_SpeedSettingsMissing := True;
                        END ELSE
                          IF (I > 1) AND (Loco_SpeedArray[I] < Loco_SpeedArray[I - 1]) THEN BEGIN
                            SpeedFound := False;
                            Log(Loco_LocoChipStr + ' L Speed step at position ' + IntToStr(I - 1) + ' is less than speed step at position ' + IntToStr(I - 2));
                            ErrorMsg := 'Loco ' + Loco_LocoChipStr + ': speed step at position ' + IntToStr(I - 1)
                                                                                                                + ' is less than speed step at position ' + IntToStr(I - 2);
                            Loco_SpeedSettingsMissing := True;
                          END;
                        Inc(I);
                      END; {WHILE}
                    END;
                  END;
                END;
              END;
            END;

            { Now add the speed settings to the appropriate MPH variable }
            IF ErrorMsg = '' THEN BEGIN
              Loco_Speed10 := Loco_SpeedArray[1];
              Loco_Speed20 := Loco_SpeedArray[2];
              Loco_Speed30 := Loco_SpeedArray[3];
              Loco_Speed40 := Loco_SpeedArray[4];
              Loco_Speed50 := Loco_SpeedArray[5];
              Loco_Speed60 := Loco_SpeedArray[6];
              Loco_Speed70 := Loco_SpeedArray[7];
              Loco_Speed80 := Loco_SpeedArray[8];
              Loco_Speed90 := Loco_SpeedArray[9];
              Loco_Speed100 := Loco_SpeedArray[10];
              Loco_Speed110 := Loco_SpeedArray[11];
              Loco_Speed120 := Loco_SpeedArray[12];
            END;
//
            { Add the record to the trainlist }
            IF ErrorMsg <> '' THEN BEGIN
              IF MessageDialogueWithDefault('Error in creating Loco=' + Loco_LocoChipStr + ': '
                                            + ErrorMsg
                                            + CRLF
                                            + 'Do you wish to continue?',
                                            StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrYes
              THEN
                ErrorMsg := ''
              ELSE BEGIN
                LocoDataADOConnection.Connected := False;
                ShutDownProgram(UnitRef, 'ReadInLocoDataFromDatabase');
              END;
            END;
          END; {WITH}
        END; {WITH}
        LocoDataADOTable.Next;
      END; {WHILE}

      { Tidy up the database }
      LocoDataADOTable.Close;
      LocoDataADOConnection.Connected := False;
      Log('L Loco Data table and connection closed');

      { and write out the list }
      Log('L ' + DescribeTrainList(LocoChipAndDepartureTimeSort, DescribeFullTrainList) + ' {WRAP=SCREENWIDTH} {NUMBER}');
      OK := True;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInLocoDataFromDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadInLocoDataFromDatabase }

PROCEDURE WriteOutLocoDataToDatabase;
{ Write out some loco data to the loco data file }
VAR
  L : LocoIndex;
  LineFound : Boolean;
  LocationStr : String;
  LocoFound : Boolean;
  TempLine : Integer;

BEGIN
  TRY
    WITH LocoUtilsWindow DO BEGIN
      IF NOT SystemOnline THEN BEGIN
        ShowMessage('System offline so not writing out loco locations');
        Log('X System offline so not writing out loco locations');
      END ELSE BEGIN
        IF NOT FileExists(PathToRailDataFiles + LocoDataFilename + '.' + LocoDataFilenameSuffix) THEN BEGIN
          IF MessageDialogueWithDefault('Loco database file "' + PathToRailDataFiles + LocoDataFilename + '.' + LocoDataFilenameSuffix + '"'
                                        + ' cannot be located'
                                        + CRLF
                                        + 'Do you wish to continue?',
                                        StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
          THEN
            ShutDownProgram(UnitRef, 'WriteOutLocoDataToDatabase')
          ELSE
            Exit;
        END;

        LocoDataADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                  + PathToRailDataFiles + LocoDataFilename + '.' + LocoDataFilenameSuffix
                                                  + ';Persist Security Info=False';
        LocoDataADOConnection.Connected := True;
        LocoDataADOTable.Open;
        LocoDataADOTable.Edit;
        Log('L Loco Data table and connection opened to write out loco locations');

        LocoDataADOTable.First;
        WHILE NOT LocoDataADOTable.EOF DO BEGIN
          WITH LocoDataADOTable DO BEGIN
            L := 0;
            LocoFound := False;
            WHILE (L <= High(Locos)) AND NOT LocoFound DO BEGIN
              IF FieldByName('LocoChip').AsInteger = Locos[L].Loco_LocoChip THEN
                LocoFound := True;

              IF NOT LocoFound THEN
                Inc(L);
            END; {WHILE}

            IF NOT LocoFound THEN BEGIN
              Log('XG Error: no locochip data for ' + LocoChipToStr(FieldByName('LocoChip').AsInteger) + ' in the loco array');
              Log('X ' + DescribeTrainList(Unsorted, True) + '{INDENT=0} {WRAP=SCREENWIDTH}');
            END ELSE BEGIN
              WITH Locos[L] DO BEGIN
                IF Loco_TrainIndex <> UnknownTrainIndex THEN BEGIN
                  WITH Trains[Loco_TrainIndex] DO BEGIN
                    { Add the current track circuit from the Trains record }
                    Edit;
                    IF Train_CurrentTC = UnknownTrackCircuit THEN
                      FieldByName('LastTC').AsString := ''
                    ELSE
                      FieldByName('LastTC').AsInteger := Trains[Loco_TrainIndex].Train_CurrentTC;
                    Post;

                    { And the loco's current location, taken from its current track circuit }
                    LineFound := False;
                    IF Train_CurrentTC = UnknownTrackCircuit THEN BEGIN
                      Edit;
                      FieldByName('LastLocation').AsString := '';
                      Post;
                    END ELSE BEGIN
                      LocationStr := LocationToStr(GetLocationFromTrackCircuit(Train_CurrentTC));
                      IF LocationStr <> UnknownLocationStr THEN BEGIN
                        Edit;
                        FieldByName('LastLocation').AsString := LocationStr;
                        Post;
                      END ELSE BEGIN
                        { this may be one of those track circuits that's attached to more than one line, not all of which have a named location }
                        TempLine := 0;
                        WHILE (TempLine <= High(Lines)) AND NOT LineFound DO BEGIN
                          IF Lines[TempLine].Line_TC = Train_CurrentTC THEN BEGIN
                            IF Lines[TempLine].Line_Location <> UnknownLocation THEN BEGIN
                              Edit;
                              FieldByName('LastLocation').AsString := LocationToStr(Lines[TempLine].Line_Location);
                              Post;
                              LineFound := True;
                            END;
                          END;
                          Inc(TempLine);
                        END; {WHILE}
                      END;
                    END;

                    { Write out the last train length - needed for trains that don't appear in the next timetable }
                    Edit;
                    IF Train_CurrentLengthInInches = 0 THEN
                      FieldByName('LastTrainLength').AsString := ''
                    ELSE
                      IF Loco_FixedLengthInInches = Train_CurrentLengthInInches THEN
                        FieldByName('LastTrainLength').AsString := ''
                      ELSE
                        FieldByName('LastTrainLength').AsString := IntToStr(Train_CurrentLengthInInches);
                    Post;
                    Edit;
                    IF Loco_FixedDirection = UnknownDirection THEN BEGIN
                      IF Loco_CurrentDirection = UnknownDirection THEN
                        FieldByName('LastTrainDirection').AsString := ''
                      ELSE
                        FieldByName('LastTrainDirection').AsString := DirectionToStr(Loco_CurrentDirection);
                    END;
                    Post;
                  END; {WITH}
                END;
              END; {WITH}
            END;
          END; {WITH}
          LocoDataADOTable.Next;
        END; {WHILE}

        { Tidy up the database }
        LocoDataADOTable.Close;
        LocoDataADOConnection.Connected := False;
        Log('L Loco Data table and connection closed after writing loco locations');
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutLocoDataToDatabase: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteOutLocoDataToDatabase }

PROCEDURE SortGrid(Grid : TStringGrid; SortCol : Integer);
{ A simple exchange sort of grid rows }
VAR
  I, J : Integer;
  Temp : TStringList;

BEGIN
  TRY
    Temp := TStringList.Create;

    WITH Grid DO BEGIN
      FOR I := FixedRows TO RowCount - 2 DO BEGIN { because last row has no next row }
        FOR J := I + 1 TO RowCount - 1 DO BEGIN { from next row to end }
          IF SortGridDirection = Up THEN BEGIN
            IF (AnsiCompareText(Cells[SortCol, I], Cells[SortCol, J]) > 0) AND (Cells[SortCol, J] <> '') THEN BEGIN
              Temp.Assign(Rows[J]);
              Rows[J].Assign(Rows[I]);
              Rows[I].Assign(Temp);
            END;
          END ELSE BEGIN
            IF AnsiCompareText(Cells[SortCol, I], Cells[SortCol, J]) < 0 THEN BEGIN
              Temp.Assign(Rows[J]);
              Rows[J].Assign(Rows[I]);
              Rows[I].Assign(Temp);
            END;
          END;
        END; {FOR}
      END; {FOR}
    END; {WITH}

    Temp.Free;

    IF SortGridDirection = Up THEN
      SortGridDirection := Down
    ELSE
      SortGridDirection := Up;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG SortGrid: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SortGrid }

PROCEDURE TLocoUtilsWindow.LocoStringGridDblClick(Sender: TObject);
{ Select a loco from the loco list and insert it in the loco dialogue }
VAR
  TempLocoChipStr : String;

BEGIN
  TRY
    IF LocoStringGrid.Col <> 1 THEN
      Debug('!Cell clicked does not contain a loco number')
    ELSE BEGIN
      TempLocoChipStr := LocoStringGrid.Cells[LocoStringGrid.Col, LocoStringGrid.Row];

      LocoUtilsWindow.Hide;
      IF Tag = -1 THEN BEGIN
        LocoDialogue.LocoDialogueWindow.LocoDialogueLocoMaskEdit.Clear;
        LocoDialogue.LocoDialogueWindow.LocoDialogueLocoMaskEdit.SelText := TempLocoChipStr;
        EnableLocoDialogueLocoButtonsAndBoxes;
      END ELSE
        IF Tag = 1 THEN BEGIN
          LocoDialogue.LocoDialogueWindow.LocoDialogueDHLocoMaskEdit.Clear;
          LocoDialogue.LocoDialogueWindow.LocoDialogueDHLocoMaskEdit.SelText := TempLocoChipStr;
          EnableLocoDialogueLocoButtonsAndBoxes;
        END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG LocoUtilsStringGridDblClick: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { LocoUtilsStringGridDblClick }

PROCEDURE TLocoUtilsWindow.LocoStringGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
VAR
  CellLeftMargin, CellTopMargin : Integer;

BEGIN
  TRY
    WITH LocoStringGrid DO BEGIN
      IF ACol > 0 THEN BEGIN
        CellLeftMargin := (ColWidths[ACol] - Canvas.TextWidth(Cells[ACol, ARow])) DIV 2;
        CellTopMargin := (RowHeights[ARow] - Canvas.TextHeight(Cells[ACol, ARow])) DIV 2;

        IF Cells[0, ARow] = '+' THEN
          Canvas.Font.Style := [fsBold, fsItalic]
        ELSE
          IF Cells[0, ARow] = '*' THEN
            Canvas.Font.Style := [fsBold]
          ELSE
            Canvas.Font.Style := [];

        LocoStringGrid.Canvas.Brush.Color := clWindow;

        Canvas.FillRect(Rect);
        IF ACol = 3 THEN
          { only centre the text vertically }
          Canvas.TextOut(Rect.Left + Canvas.TextWidth('i'),
                         Rect.Top + CellTopMargin,
                         LocoStringGrid.Cells[ACol, ARow])
        ELSE
          { centre the text horizontally and vertically }
          Canvas.TextOut(Rect.Left + CellLeftMargin,
                         Rect.Top + CellTopMargin,
                         LocoStringGrid.Cells[ACol, ARow]);
      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG LocoStringGridDrawCell: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { LocoStringGridDrawCell }

PROCEDURE TLocoUtilsWindow.LocoStringGridKeyDown(Sender : TObject; VAR Key : Word; ShiftState : TShiftState);
{ Search in the loco grid }

  PROCEDURE DoSearch(ColNum, RowNum : Integer);
  { Do the search }
  VAR
    Done : Boolean;

  BEGIN
    TRY
      Done := False;
      WHILE (RowNum < LocoStringGrid.RowCount) AND NOT Done DO BEGIN
        ColNum := 0;
        WHILE (ColNum < LocoStringGrid.ColCount) AND NOT Done DO BEGIN
          IF Pos(UpperCase(SaveLocoStringGridSearchStr), UpperCase(LocoStringGrid.Cells[ColNum, RowNum])) > 0 THEN BEGIN
            LocoStringGrid.SetFocus;
            LocoStringGrid.Row := RowNum;
            LocoStringGrid.Col := ColNum;
            Done := True;
          END;
          Inc(ColNum);
        END; {WHILE}
        Inc(RowNum);
      END; {WHILE}
      SaveLocoStringGridSearchCol := ColNum;
      SaveLocoStringGridSearchRow := RowNum;
    EXCEPT {TRY}
      ON E : Exception DO
        Log('EG DoSearch: ' + E.ClassName + ' error raised, with message: ' + E.Message);
    END; {TRY}
  END; { DoSearch }

BEGIN
  TRY
    CASE Key OF
      vk_F3:
        IF SaveLocoStringGridSearchStr <> '' THEN
          DoSearch(SaveLocoStringGridSearchCol + 1, SaveLocoStringGridSearchRow);
      Ord('F'):
        IF ssCtrl IN ShiftState THEN BEGIN
          IF InputQuery('Enter search string', 'Search Loco Grid', SaveLocoStringGridSearchStr) THEN BEGIN
            SaveLocoStringGridSearchCol := 0;
            SaveLocoStringGridSearchRow := 1;
            DoSearch(SaveLocoStringGridSearchCol, SaveLocoStringGridSearchRow)
          END;
        END;
      vk_Escape:
        LocoUtilsWindow.Visible := False;
    END; {CASE}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG LocoStringGridKeyDown: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { LocoStringGridKeyDown }

PROCEDURE TLocoUtilsWindow.LocoStringGridMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
VAR
  ColNum : Integer;
  I : Integer;
  Rect : TRect;

BEGIN
  TRY
    ColNum := 0;

    WITH LocoStringGrid DO BEGIN
      { Make sure row 0 was clicked }
      IF Y < RowHeights[0] THEN BEGIN
        { determine which column was clicked }
        FOR I := 0 TO ColCount - 1 DO BEGIN
          Rect := CellRect(I, 0);
          IF (Rect.Left < X) AND (Rect.Right > X) THEN BEGIN
            ColNum := I;
            Break;
          END;
        END;

        IF SaveSortGridColNum <> ColNum THEN
          SortGridDirection := Up;

        SortGrid(LocoStringGrid, ColNum);
        SaveSortGridColNum := ColNum;
      END;
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG LocoStringGridMouseUp: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { LocoStringGridMouseUp }

PROCEDURE ListLocosByChip;
{ Create a list of locos by chip }
VAR
  I : Integer;
  ColNum : Integer;
  L : LocoIndex;
  RowNum : Integer;
  TempCell : String;
  WindowWidth : Integer;

  PROCEDURE AddItemToGrid(Str : String; ColNum, RowNum : Integer);
  BEGIN
    TRY
      WITH LocoUtilsWindow DO BEGIN
        LocoStringGrid.Cells[ColNum, RowNum] := Str;
        IF Canvas.TextWidth(Str) > LocoStringGrid.ColWidths[ColNum] THEN
          LocoStringGrid.ColWidths[ColNum] := Canvas.TextWidth(Str) + (LocoStringGrid.BevelWidth * 10) + LocoStringGrid.GridLineWidth;
      END; {WITH}
    EXCEPT {TRY}
      ON E : Exception DO
        Log('EG AddItemToGrid: ' + E.ClassName + ' error raised, with message: ' + E.Message);
    END; {TRY}
  END; { AddItemToGrid }

BEGIN
  TRY
    TypeOfLocoUtilsWindow := ListedByChip;

    WITH LocoUtilsWindow DO BEGIN
      RowNum := 0;
      LocoStringGrid.RowCount := 2;
      LocoStringGrid.ColCount := 10;

      ColNum := 0;
      AddItemToGrid('', ColNum, RowNum);
      LocoStringGrid.ColWidths[0] := 0;

      Inc(ColNum);
      AddItemToGrid('Chip', ColNum, RowNum);

      Inc(ColNum);
      AddItemToGrid('Actual Number', ColNum, RowNum);

      Inc(ColNum);
      AddItemToGrid('Loco Name', ColNum, RowNum);

      Inc(ColNum);
      AddItemToGrid('Class', ColNum, RowNum);

      Inc(ColNum);
      AddItemToGrid('LocoType', ColNum, RowNum);

      Inc(ColNum);
      AddItemToGrid('Description', ColNum, RowNum);

      Inc(ColNum);
      AddItemToGrid('Lighting Chip Up', ColNum, RowNum);

      Inc(ColNum);
      AddItemToGrid('Lighting Chip Down', ColNum, RowNum);

      Inc(ColNum);
      { note: these "notes" are automatically generated here and not taken from the database }
      AddItemToGrid('To note', ColNum, RowNum);

      L := 0;
      WHILE L <= High(Locos) DO BEGIN
        WITH Locos[L] DO BEGIN
          IF Loco_LocoChip <> UnknownLocoChip THEN BEGIN
            LocoStringGrid.RowCount := LocoStringGrid.RowCount + 1;
            Inc(RowNum);

            ColNum := 0;

            { Set up the first column but don't display it - it is used to make the row bold or italicised }
            TempCell := '';

            AddItemToGrid(TempCell, ColNum, RowNum);
            LocoStringGrid.ColWidths[0] := 0;

            Inc(ColNum);
            AddItemToGrid(Loco_LocoChipStr, ColNum, RowNum);

            Inc(ColNum);
            AddItemToGrid(Loco_ActualNumStr, ColNum, RowNum);

            Inc(ColNum);
            AddItemToGrid(Loco_LocoName, ColNum, RowNum);

            Inc(ColNum);
            AddItemToGrid(Loco_LocoClassStr, ColNum, RowNum);

            Inc(ColNum);
            AddItemToGrid(Loco_LocoTypeStr, ColNum, RowNum);

            Inc(ColNum);
            AddItemToGrid(Loco_Description, ColNum, RowNum);

            Inc(ColNum);
            AddItemToGrid(IfThen(Loco_LightingChipUp = UnknownLocoChip,
                                 '',
                                 IfThen(Loco_LightingChipUp = Loco_LocoChip,
                                        '[' + IntToStr(Loco_LocoChip) + ']',
                                        IntToStr(Loco_LightingChipUp))),
                          ColNum, RowNum);

            Inc(ColNum);
            AddItemToGrid(IfThen(Loco_LightingChipDown = UnknownLocoChip,
                                 '',
                                 IfThen(Loco_LightingChipDown = Loco_LocoChip,
                                        '[' + IntToStr(Loco_LocoChip) + ']',
                                        IntToStr(Loco_LightingChipDown))),
                          ColNum, RowNum);

            Inc(ColNum);
          END;
        END; {WITH}
        Inc(L);
      END; {WHILE}

      { Resize the LocoStringGrid window }
      WindowWidth := 0;
      FOR I := 1 TO ColNum DO
        WindowWidth := WindowWidth + LocoStringGrid.ColWidths[I] + (BevelWidth * 6);
      LocoUtilsWindow.Width := WindowWidth;

      SortGrid(LocoStringGrid, 1);
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ListLocosByChip: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ListLocosByChip }

FUNCTION CompareNumbers(List: TStringList; Index1, Index2: Integer): Integer;
{ Part of the SortNumbersInsertingLeadingZeroes routine. (For some reason this cannot be a local procedure, as when it is it produces the error "Local procedure/function
  assigned to procedure variable").
}
VAR
  I : Integer;
  J : Integer;
  NumberFound1 : Boolean;
  NumberFound2 : Boolean;
  NumberStr1A, NumberStr1B : String;
  NumberStr2A, NumberStr2B : String;
  NumberToTest1 : Integer;
  NumberToTest2 : Integer;
  TempNum : Integer;

BEGIN
  Result := 0;

  TRY
    { Isolate the number we are comparing from the rest of the string }
    IF (List[Index1][1] = ' ') OR (List[Index2][1] = ' ') THEN
      Exit;

    NumberFound1 := False;
    NumberToTest1 := 0;
    I := 2;
    WHILE (I < Length(List[Index1])) AND NOT NumberFound1 DO BEGIN
      { look for the first space }
      IF List[Index1][I] = ' ' THEN BEGIN
        NumberFound1 := True;
        NumberStr1A := Copy(List[Index1], 1, I - 1);
        NumberStr1B := '';

        { but the "number" might have a letter or two in it - e.g. D1934 (a class 47) or DR73278 (a tamper) so remove it }
        NumberToTest1 := 0;
        FOR J := 1 TO Length(NumberStr1A) DO
          IF TryStrToInt(NumberStr1A[J], TempNum) THEN
            NumberStr1B := NumberStr1B + NumberStr1A[J];
      END;
      Inc(I);
    END; {WHILE}
    IF NOT TryStrToInt(NumberStr1B, NumberToTest1) THEN
      Exit;

    NumberFound2 := False;
    NumberToTest2 := 0;
    I := 2;
    WHILE (I < Length(List[Index2])) AND NOT NumberFound2 DO BEGIN
      { look for the first space }
      IF List[Index2][I] = ' ' THEN BEGIN
        NumberFound2 := True;
        NumberStr2A := Copy(List[Index2], 1, I - 1);
        NumberStr2B := '';

        { but the "number" might have a letter or two in it - e.g. D1934 (a class 47) or DR73278 (a tamper) so remove it }
        NumberToTest2 := 0;
        FOR J := 1 TO Length(NumberStr2A) DO
          IF TryStrToInt(NumberStr2A[J], TempNum) THEN
            NumberStr2B := NumberStr2B + NumberStr2A[J];
      END;
      Inc(I);
    END; {WHILE}
    IF NOT TryStrToInt(NumberStr2B, NumberToTest2) THEN
      Exit;

    { Now do the comparison }
    IF NOT NumberFound1
    OR NOT NumberFound2
    OR (NumberToTest1 = 0)
    OR (NumberToTest2 = 0)
    THEN
      Result := 0
    ELSE
      IF NumberToTest1 < NumberToTest2 THEN
        Result := -1
      ELSE
        IF NumberToTest1 > NumberToTest2 THEN
          Result := 1
        ELSE
          Result := 0;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG CompareNumbers: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CompareNumbers }

PROCEDURE SortNumbersInsertingLeadingZeroes(ListBox : TListBox);
{ Sorts numbers so that, for instance, class 158 does not precede class 47 }
VAR
  StringList : TStringList;

BEGIN
  TRY
    StringList := TStringList.Create;
    TRY
      // ListBox.Sorted := False !
      StringList.Assign(ListBox.Items);
      StringList.CustomSort(CompareNumbers);
      { the above works as follows:
        PROCEDURE CustomSort(Compare : TStringListSortCompare); Virtual;
        TYPE TStringListSortCompare = FUNCTION(List : TStringList; Index1, Index2 : Integer): Integer;
      }
      ListBox.Items.Assign(StringList);
    FINALLY
      StringList.Free
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG SortNumbersInsertingLeadingZeroes: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { SortNumbersInsertingLeadingZeroes }

PROCEDURE InitialiseLocoUtilsUnit;
{ Initialises the unit }
BEGIN
  LocoUtilsWindow.Height := LocoUtilsWindowHeight;
  LocoUtilsWindow.Width := LocoUtilsWindowWidth;
  LocoUtilsWindow.Top := LocoUtilsWindowTop;
  LocoUtilsWindow.Left := LocoUtilsWindowLeft;
END; { InitialiseLocoUtilsUnit }

PROCEDURE TLocoUtilsWindow.LocoUtilsLocoChipButtonKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  IF Key = vk_Escape THEN BEGIN
    LocoUtilsWindow.Hide;
    Log('A List of locos hidden');
  END;
END; { LocoListChipButtonKeyDown }

PROCEDURE TLocoUtilsWindow.LocoUtilsLocoNumButtonKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  IF Key = vk_Escape THEN
    LocoUtilsWindow.Hide;
END; { LocoListLocoNumButtonKeyDown }

INITIALIZATION

END { LocoUtils }.
