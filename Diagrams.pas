UNIT Diagrams;

INTERFACE

USES Windows, Messages, Variants, Classes, Graphics, Controls, Forms, StdCtrls, Grids, Menus, InitVars, GetTime, Dialogs, SysUtils, DB, ADODB, DBGrids, ExtCtrls, DBCtrls;

TYPE
  TDiagramsWindow = CLASS(TForm)
    DiagramsADOConnection: TADOConnection;
    DiagramsADOTable: TADOTable;
    DiagramsDataSource: TDataSource;
    DiagramsPopupMenu: TPopupMenu;
    DiagramsWindowColourDialogue: TColorDialog;
    DiagramsWindowGrid: TStringGrid;

    PopupCancelTrain: TMenuItem;
    PopupChangeDiagramsColour: TMenuItem;
    PopupChangeDiagramsColours: TMenuItem;
    PopupDriveTrain: TMenuItem;
    PopupShowNonStops: TMenuItem;
    PopupMarkTrainNotMissing: TMenuItem;
    PopupResetDiagramsWindowSizeAndPosition: TMenuItem;
    PopupRestoreDefaultColour: TMenuItem;
    PopupSelectDiagramsWindowSize: TMenuItem;
    PopupShowCancelledTrains: TMenuItem;
    PopupShowLastTrainErrorMessage: TMenuItem;
    PopupShowNonMovingTrains: TMenuItem;
    PopupSupplyUserTrainInstructions: TMenuItem;
    PopupSuspendTrain: TMenuItem;
    PopupTrainLocoChip: TMenuItem;
    PopupTrainRule1: TMenuItem;
    PopupTrainRule2: TMenuItem;
    PopupTrainRule4: TMenuItem;
    PopupTrainUserDriving: TMenuItem;

    PROCEDURE DiagramsPopupMenuOnPopup(Sender: TObject);
    PROCEDURE DiagramsWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE DiagramsWindowCreate(Sender: TObject);
    PROCEDURE DiagramsWindowDeactivate(Sender: TObject);
    PROCEDURE DiagramsWindowGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    PROCEDURE DiagramsWindowGridKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE DiagramsWindowGridMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE DiagramsWindowGridMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE DiagramsWindowGridSelectCell(Sender: TObject; ACol, ARow: Integer; VAR CanSelect: Boolean);
    PROCEDURE DiagramsWindowHide(Sender: TObject);
    PROCEDURE DiagramsWindowResize(Sender: TObject);
    PROCEDURE DiagramsWindowShow(Sender: TObject);

    PROCEDURE PopupChangeDiagramsColourClick(Sender: TObject);
    PROCEDURE PopupDriveTrainClick(Sender: TObject);
    PROCEDURE PopupShowNonStopsClick(Sender: TObject);
    PROCEDURE PopupCancelTrainClick(Sender: TObject);
    PROCEDURE PopupMarkTrainNotMissingClick(Sender: TObject);
    PROCEDURE PopupResetDiagramsWindowSizeAndPositionClick(Sender: TObject);
    PROCEDURE PopupRestoreDefaultColourClick(Sender: TObject);
    PROCEDURE PopupSelectDiagramsWindowSizeClick(Sender: TObject);
    PROCEDURE PopupShowCancelledTrainsClick(Sender: TObject);
    PROCEDURE PopupShowLastTrainErrorMessageClick(Sender: TObject);
    PROCEDURE PopupShowNonMovingTrainsClick(Sender: TObject);
    PROCEDURE PopupSupplyUserTrainInstructionsClick(Sender: TObject);
    PROCEDURE PopupSuspendTrainClick(Sender: TObject);
    PROCEDURE PopupTrainUserDrivingClick(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

TYPE
  CellStyleType = (NormalStyle, BoldStyle, ItalicStyle, BoldAndItalicStyle, GreyedOutStyle);

PROCEDURE AddTrainToTrainList(T : Train; DescribeFullTrainList : Boolean);
{ Add a descriptor for each active loco to the train list. List the full train list (locos only and locos and trains if
  DescribeFullTrainList is set.
}
FUNCTION CalculateJourneyTimeinMinutes(TrainTypeNum : Integer; RouteLengthInInches : Real) : Integer;
{ Given the journey length in inches, calculate the journey time in minutes }

FUNCTION CalculateRouteLength(RouteArray : StringArrayType) : Real;
{ Using the route array, calculate the route length. (We need this to calculate how long journeys take). }

PROCEDURE CancelTrain(T : Train; UserInstruction, TrainExists : Boolean);
{ Mark a train as cancelled, and enable the appropriate popup to display it in the diagrams window }

FUNCTION CheckJourney(T : Train; Journey : Integer; Direction : DirectionType; StartLocation, EndLocation : Integer; StartLine, EndLine : Integer; OUT ErrorMsg : String;
                      OUT LinesNotAvailableStr : String; UseEmergencyRouteing : Boolean; OUT JourneyArray : StringArrayType; OUT OK : Boolean) : Boolean;
{ See if each projected journey is correct }

PROCEDURE CheckOccupiedLinesAndDiagrams;
{ (1) If a feedback occupation does not has a loco recorded as occupying it or adjacent to it, mark it as a query, and if a feedback occupation does not has a diagrammed
      train occupying it or adjacent to it, mark it as permanently occupied;
  (2) If a loco in the loco locations file does not a feedback occupation, note the fact;
  (3) If a diagrammed train does not have a feedback occupation in any of its initial trackcircuits, ask if it actually exists (in case of faulty trackcircuits).  Also
      check whether a diagrammed train has a feedback occupation in any of its initial trackcircuits, but is recorded in the loco locations file as being elsewhere.
}
PROCEDURE CreateJourney(VAR T : Train; Journey : Integer; NewJourney: Boolean; StartArea, EndArea, StartLocation, EndLocation: Integer;
                        DiagrammedStartLocation, DiagrammedEndLocation, StartLine, EndLine: Integer;
                        CurrentDepartureTime, DiagrammedDepartureTime, CurrentArrivalTime: TDateTime; TrainDirection : DirectionType; JourneyRouteArray : StringArrayType;
                        RebuildRouteArray : Boolean; StoppingOnArrival, NotForPublicUse, RouteingInEmergency, StartOfRepeatJourney, AreDiagramsLoading : Boolean;
                        OUT ErrorMsg : String; OUT LinesNotAvailableStr : String; OUT OK : Boolean);
{ Given various parameters, set up each individual journey within the overall train record; returns ok if the journey route is clear }

FUNCTION CreateTrainDiagramsRecord(LocoChip, DoubleHeaderLocoChip, JourneyCount : Integer; UserSpecifiedDepartureTimesArray : DateTimeArrayType; LightsOnTime : TDateTime;
                                   EndLocationsStrArray : StringArrayType; DirectionsArray : DirectionArrayType; LightsRemainOn : Boolean; TrainNonMoving : Boolean;
                                   NotForPublicUseArray : BooleanArrayType; StartLocationStr : String; StoppingArray : BooleanArrayType; LengthOfTrainInCarriages : Integer;
                                   TypeOfTrainNum : Integer; UserDriving, UserRequiresInstructions, StartOfRepeatJourney : Boolean)
                                   : Train;
{ Creates a new train record for the diagram - NB the array parameters being passed are open array parameters and therefore start at zero }

FUNCTION DescribeJourney(T : Train; Journey : Integer) : String;
{ Given various parameters, set up each individual journey within the overall train record }

PROCEDURE DiscardDiagrams(CheckIfOK : Boolean);
{ Throw away the current diagrams, and do appropriate tidying up }

PROCEDURE DrawDiagramsWindow;
{ Write the complete diagrams to the screen after they have been changed }

PROCEDURE DrawDiagrams(UnitRef : String; Str : String);
{ Write the complete diagrams to the screen after they have been changed }

PROCEDURE DrawDiagramsSpeedCell(T : Train);
{ Draw the speed cell for a given loco }

PROCEDURE DrawDiagramsStatusCell(T : Train; CellStyle : CellStyleType);
{ Draw the status cell for a given loco - this routine is to avoid having to draw all the diagrams for a minor change }

FUNCTION GetStationNameFromArea(Area : Integer) : String;
{ Return a station name given the Area it is in }

PROCEDURE InitialiseDiagramsUnit;
{ Initialises the unit }

PROCEDURE ProcessDiagrams(OUT ErrorMsg : String; OUT DiagramsOK : Boolean);
{ Once read in, process the diagrams }

PROCEDURE ReadInDiagramsFromDatabase(OUT ErrorMsg : String; OUT DiagramsMissing, DiagramsOK : Boolean);
{ Read the diagrams in from the supplied database }

PROCEDURE RecalculateJourneyTimes(T : Train; ExplanatoryStr : String);
{ Recalculate the journey times to allow for any inserted journeys or other changes }

PROCEDURE RemoveTrainFromDiagrams(T : Train);
{ Called to indicate a train is no longer of interest; it is removed from the diagrams, the others moved to fill the gap and a new train generated. The diagrams will be
  redrawn on screen.
}
PROCEDURE SetInitialTrackCircuits(T : Train);
{ Sets the initial trackcircuits for a given route }

PROCEDURE SuspendTrain(T : Train; User : Boolean);
{ Suspend a given train - it can be reactivated in the diagrams window }

FUNCTION TrainFoundInDiagrams(LocoChip : Integer) : DiagramsEntryType;
{ Search the diagrams for a particular loco and return where it is on the layout and in the diagram window }

PROCEDURE WriteTrainJourneysRecordToLogFile(T : Train; FullRecord : Boolean);
{ Writes the journey record out to the log }

PROCEDURE WriteTrainJourneysRecordToLockListWindow(T : Train; FullRecord : Boolean);
{ Writes the journey record to the locklist window }

PROCEDURE WriteTrainRecord(T : Train);
{ Writes the main train record out }

VAR
  DiagramsWindow: TDiagramsWindow;
  TrainClickedOn : Train = NIL;

IMPLEMENTATION

{$R *.dfm}

USES ComObj, Lenz, MiscUtils, Startup, LocoUtils, IDGlobal, RailDraw, Input, Movement, CreateRoute, DateUtils, Math {sic}, Route, Types, StrUtils, StationMonitors, Locks,
     LocoDialogue, LocationData, Help, Options;

CONST
  BoldStyleStr = '<B>';
  GreyedOutStyleStr = '<G>';
  ItalicStyleStr = '<I>';
  BoldAndItalicStyleStr = '<BI>';

  UnitRef = 'Diagrams';

  LocoChipCol = 0;
  LocoClassCol = LocoChipCol + 1;
  HeadcodeCol = LocoClassCol + 1;
  JourneyCol = HeadcodeCol + 1;
  SourceCol = JourneyCol + 1;

VAR
  DiagramsChosenTrain : Train;
  EscapePressed : Boolean = False;
  HeadcodeARunningNum : Integer = 0;
  HeadcodeERunningNum : Integer = 0;
  HeadcodeMRunningNum : Integer = 0;
  HeadcodeORunningNum : Integer = 0;
  HeadcodeSRunningNum : Integer = 0;
  HeadcodeVRunningNum : Integer = 0;
  HeadcodeXRunningNum : Integer = 0;
  HeadcodeZRunningNum : Integer = 0;
  HighlightRow : Integer = -1;
  InitDiagramsWindow : Boolean = True;
  ShowArrivalTimes : Boolean = False;
  ShowTrainLength : Boolean = True;

  NumberOfRepeatColumns : Integer;
  DestinationCol : IntegerArrayType;
  MaxColumnCount : Integer = 0;
  SpeedCol : Integer;
  StatusCol : Integer;
  TimeCol : IntegerArrayType;
  UpDownCol : IntegerArrayType;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION TrainFoundInDiagrams(LocoChip : Integer) : DiagramsEntryType;
{ Search the diagrams for a particular loco and return where it is on the layout and in the diagram window }
VAR
  TrainFound : Boolean;
  TestList : DiagramsEntryType;

BEGIN
  TestList := DiagramsList;
  TrainFound := False;

  WHILE (TestList <> NIL)
  AND NOT TrainFound
  DO BEGIN
    IF TestList^.TrainPtr^.Train_DiagramFound
    AND (TestList^.TrainPtr^.Train_LocoChip = LocoChip)
    THEN
      TrainFound := True
    ELSE
      TestList := TestList^.NextDiagramsRecord;
  END; {WHILE}

  Result := TestList;
  IF TrainFound THEN
    Log(LocoChipToStr(LocoChip) + ' D Train found in diagrams');
END; { TrainFoundInDiagrams }

PROCEDURE CheckOccupiedLinesAndDiagrams;
{ (1) If a feedback occupation does not has a loco recorded as occupying it or adjacent to it, mark it as a query, and if a feedback occupation does not has a diagrammed
      train occupying it or adjacent to it, mark it as permanently occupied;
  (2) If a loco in the loco locations file does not a feedback occupation, note the fact;
  (3) If a diagrammed train does not have a feedback occupation in any of its initial trackcircuits, ask if it actually exists (in case of faulty trackcircuits). Also
      check whether a diagrammed train has a feedback occupation in any of its initial trackcircuits, but is recorded in the loco locations file as being elsewhere.
}
VAR
  DiagramsEntry : DiagramsEntryType;
  I : Integer;
  InitialTrackCircuitCount : Integer;
  LocationFound : Boolean;
  LocationTCs : IntegerArrayType;
  LocoTC : Integer;
  MatchingLocationFound : Boolean;
  OccupiedTrackCircuitFound : Boolean;
  RemoveRecordedLocationsQueryPut : Boolean;
  StopCheckingRecordedLocations : Boolean;
  T : Train;
  TC : Integer;
  TempTCs : IntegerArrayType;
  TrainFound : Boolean;
  TrainInitialTrackCircuitCount : Integer;

BEGIN
  { (1) If a feedback occupation does not has a loco recorded as occupying it or adjacent to it, mark it as a query, and if a feedback occupation does not has a diagrammed
        train occupying it or adjacent to it, mark it as permanently occupied.
  }
  IF SystemOnline THEN BEGIN
    FOR TC := 0 TO High(TrackCircuits) DO BEGIN
      IF TrackCircuits[TC].TC_OccupationState = TCFeedbackOccupation THEN BEGIN
        { Is there a loco recorded as occupying this trackcircuit? }
        MatchingLocationFound := False;
        T := TrainList;
        WHILE (T <> NIL)
        AND NOT MatchingLocationFound
        DO BEGIN
          WITH T^ DO BEGIN
            IF (GetLocationFromTrackCircuit(TC) <> UnknownLocation)
            AND (GetLocationFromTrackCircuit(TC) = Train_LastLocation)
            THEN BEGIN
              MatchingLocationFound := True;
              TrackCircuits[TC].TC_LocoChip := Train_LocoChip;
              { If a feedback occupation does not has a diagrammed train occupying or adjacent to it, mark it as permanently occupied }
              DiagramsEntry := TrainFoundInDiagrams(TrackCircuits[TC].TC_LocoChip);
              IF DiagramsEntry = NIL THEN BEGIN
                TrackCircuits[TC].TC_OccupationState := TCPermanentFeedbackOccupation;
                Log(LocoChipToStr(TrackCircuits[TC].TC_LocoChip) + ' T recorded as being at TC=' + IntToStr(TC)
                                                                 + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
                                                                 + ' not found in the diagrams - TC=' + IntToStr(TC)
                                                                 + ' set to TCPermanentFeedbackOccupation');
                { and set up the train length if recorded in the loco database }
                ChangeTrainStatus(T, NonMoving);
                IF Train_LastLengthInInches > 0 THEN BEGIN
                  Train_CurrentSourceLocation := Train_LastLocation;

                  { This next bit just orientates the non-moving train so that SetInitialTrackCircuits works without error - otherwise the routine complains that there is
                    no adjacent signal to the first track circuit (as there wouldn't be, as the line doesn't go anywhere).
                  }
                  IF (Locations[Train_LastLocation].Location_LineAtUpIsEndOfLine)
                  AND (Train_CurrentDirection = Up)
                  THEN
                    Train_CurrentDirection := Down
                  ELSE
                    IF (Locations[Train_LastLocation].Location_LineAtDownIsEndOfLine)
                    AND (Train_CurrentDirection = Down)
                    THEN
                      Train_CurrentDirection := Up;

                  DiagramsWindow.PopupShowNonMovingTrains.Enabled := True;
                  Log(Train_LocoChipStr + ' D marked as non-moving when feedback found');
                  IF Train_InitialTrackCircuits[1] = UnknownTC THEN
                    SetInitialTrackCircuits(T);
                END;
              END ELSE BEGIN
                { it's in the diagrams, but does its location match? }
                IF DiagramsEntry^.TrainPtr^.Train_CurrentSourceLocation = GetLocationFromTrackCircuit(TC) THEN
                  TrackCircuits[TC].TC_OccupationState := TCFeedbackOccupation
                ELSE BEGIN
                  { no, it doesn't }
                  IF Train_CurrentStatus <> Cancelled THEN BEGIN
                    TrackCircuits[TC].TC_OccupationState := TCLocoOutOfPlaceOccupation;
                    CancelTrain(DiagramsEntry^.TrainPtr, NOT ByUser, TrainExists);
                    Debug('Train ' + DiagramsEntry^.TrainPtr^.Train_Headcode
                          + ' (loco ' + IntToStr(TrackCircuits[TC].TC_LocoChip)
                          + ') found in the diagrams but its recorded location'
                          + ' ' + LocationToStr(GetLocationFromTrackCircuit(TC))
                          + ' does not match the diagrams: train entry cancelled');
                  END;
                END;
              END;
            END;

            InitialTrackCircuitCount := 1;
            WHILE (InitialTrackCircuitCount <= 5)
            AND NOT MatchingLocationFound
            DO BEGIN
              { see if it's an initial track circuit }
              IF TC = Train_InitialTrackCircuits[InitialTrackCircuitCount] THEN
                MatchingLocationFound := True;
              Inc(InitialTrackCircuitCount);
            END; {END}
          END; {WITH}
          T := T^.Train_NextRecord;
        END; {WHILE}
        { If a feedback occupation does not has a loco recorded as occupying it or adjacent to it, mark it as permanently occupied and as a query }
        IF NOT MatchingLocationFound THEN BEGIN
          TrackCircuits[TC].TC_Headcode := '?';
          TrackCircuits[TC].TC_OccupationState := TCPermanentFeedbackOccupation;
          Log(LocoChipToStr(TrackCircuits[TC].TC_LocoChip) + ' T TC=' + IntToStr(TC)
                                                           + ' [' + DescribeLineNamesForTrackCircuit(TC) + ']'
                                                           + ' set to TCPermanentFeedbackOccupation'
                                                           + ' as no diagrammed train occupying it');
        END;
      END;
    END; {FOR}
  END;

  { (2) If a loco in the loco locations file does not have feedback occupation, note the fact, unless we are starting intentionally in offline mode }
  LocoTC := UnknownTC;
  T := TrainList;
  StopCheckingRecordedLocations := False;
  RemoveRecordedLocationsQueryPut := False;
  WHILE (T <> NIL)
  AND NOT StopCheckingRecordedLocations
  DO BEGIN
    WITH T^ DO BEGIN
      IF (Train_LocoChip <> UnknownLocoChip)
      AND (Train_LastLocation <> UnknownLocation)
      THEN BEGIN
        IF NOT SystemOnline THEN BEGIN
          { it's all academic but useful for testing }
          IF Train_CurrentStatus = ReadyForCreation THEN
            Log(Train_LocoChipStr + ' T TC=' + IntToStr(Train_LastTC)
                                  + ' [' + DescribeLineNamesForTrackCircuit(Train_LastTC) + ']'
                                  + ' not set to TCPermanentFeedbackOccupation as train is marked as ready for creation')
          ELSE
            IF Train_LastTC <> UnknownTC THEN BEGIN
              SetTrackCircuitState(Train_LocoChip, Train_LastTC, TCPermanentFeedbackOccupation);
              Log(Train_LocoChipStr + ' T TC=' + IntToStr(Train_LastTC)
                                    + ' ['  + DescribeLineNamesForTrackCircuit(Train_LastTC) + ']'
                                    + ' set to TCPermanentFeedbackOccupation');

              { and set up the train length if recorded in the loco database }
              IF Train_CurrentLengthInInches > 0 THEN BEGIN
                Train_CurrentSourceLocation := Train_LastLocation;
                IF Train_InitialTrackCircuits[1] = UnknownTC THEN
                  SetInitialTrackCircuits(T);
              END;
            END;
        END ELSE
          { online - see if we can find a known trackcircuit from the Train_LastLocation }
          LocoTC := 0;
          OccupiedTrackCircuitFound := False;
          WHILE SystemOnline
          AND (LocoTC <= High(TrackCircuits))
          AND NOT OccupiedTrackCircuitFound
          DO BEGIN
            IF GetLocationFromTrackCircuit(LocoTC) = Train_LastLocation THEN BEGIN
              IF TrackCircuits[LocoTC].TC_OccupationState = TCPermanentOccupationSetByUser THEN BEGIN
                IF TrackCircuits[LocoTC].TC_FeedbackOccupation THEN BEGIN
                  OccupiedTrackCircuitFound := True;
                  SetTrackCircuitState(Train_LocoChip, LocoTC, TCPermanentFeedbackOccupation);
                  Log(Train_LocoChipStr + ' T TC=' + IntToStr(LocoTC) + ' [' + DescribeLineNamesForTrackCircuit(LocoTC) + ']'
                                        + ' set to TCPermanentFeedbackOccupation');
                END ELSE BEGIN
                  SetTrackCircuitState(Train_LocoChip, LocoTC, TCPermanentOccupationSetByUser);
                  Log(Train_LocoChipStr + ' T TC=' + IntToStr(LocoTC) + ' [' + DescribeLineNamesForTrackCircuit(LocoTC) + ']'
                                        + ' set to TCPermanentOccupationSetByUser');
                END;
              END ELSE
                IF TrackCircuits[LocoTC].TC_FeedbackOccupation THEN
                  OccupiedTrackCircuitFound := True;
            END;
            Inc(LocoTC);
          END; {WHILE}

        IF SystemOnline
        AND NOT OccupiedTrackCircuitFound
        THEN BEGIN
          IF RemoveRecordedLocationsQueryPut THEN BEGIN
            { mark track circuits as being occupied }
            TempTCs := GetTrackCircuitsForLocation(Train_LastLocation);
            FOR I := 0 TO High(TempTCs) DO
              SetTrackCircuitState(Train_LocoChip, TempTCs[I], TCPermanentSystemOccupation);
          END ELSE BEGIN
            IF NOT RemoveRecordedLocationsQueryPut
            AND (MessageDialogueWithDefault('There is at least one recorded loco location with no feedback.'
                                            + CRLF
                                            + 'Do you want to see the details?',
                                            StopTimer, mtError, [mbYes, mbNo], mbNo) = mrNo)
            THEN BEGIN
              StopCheckingRecordedLocations := True;
              { mark track circuits as being occupied }
              TempTCs := GetTrackCircuitsForLocation(Train_LastLocation);
              FOR I := 0 TO High(TempTCs) DO
                SetTrackCircuitState(Train_LocoChip, TempTCs[I], TCPermanentSystemOccupation);
            END ELSE BEGIN
              RemoveRecordedLocationsQueryPut := True;

              IF NOT StopCheckingRecordedLocations THEN BEGIN
                IF MessageDialogueWithDefault('Loco ' + Train_LocoChipStr
                                              + ' is recorded as being at ' + LocationToStr(Train_LastLocation) + ' but that location is not marked as being occupied.'
                                              + CRLF
                                              + 'Do you want to remove the recorded location?',
                                              StopTimer, mtError, [mbYes, mbNo], mbNo) = mrNo
                THEN BEGIN
                  { mark track circuits as being occupied }
                  TempTCs := GetTrackCircuitsForLocation(Train_LastLocation);
                  FOR I := 0 TO High(TempTCs) DO
                    SetTrackCircuitState(Train_LocoChip, TempTCs[I], TCPermanentSystemOccupation);
                END ELSE BEGIN
                  { delete the occupation }
                  Log(Train_LocoChipStr + ' L removed from'
                                        + ' ' + IfThen(Train_LastTC = UnknownTC,
                                                       '',
                                                       'TC=' + IntToStr(LocoTC))
                                        + ' ' + IfThen(Train_LastLocation = UnknownLocation,
                                                       '',
                                                       LocationToStr(Train_LastLocation)));
                  Train_LastLocation := UnknownLocation;
                  Train_SavedLocation := UnknownLocation;
                  Train_LastTC := UnknownTC;
                  Train_CurrentTC := UnknownTC;
                  Train_CurrentDirection := UnknownDirection;
                  Train_CurrentLengthInInches := 0;
                END;
              END;
            END;
          END;
        END;
      END;
    END; {WITH}
    T := T^.Train_NextRecord;
  END; {WHILE}

  { (3) If a diagrammed train does not have a feedback occupation in any of its initial trackcircuits, ask if it actually exists (in case of faulty trackcircuits). Also
        check whether a diagrammed train has a feedback occupation in any of its initial trackcircuits, but is recorded in the loco locations file as being elsewhere.
  }
  IF SystemOnline THEN BEGIN
    T := TrainList;
    WHILE T <> NIL DO BEGIN
      WITH T^ DO BEGIN
        IF Train_DiagramFound
        AND (Train_LocoChip <> UnknownLocoChip)
        AND (Train_CurrentStatus <> Suspended)
        AND (Train_CurrentStatus <> MissingAndSuspended)
        AND (Train_CurrentStatus = Cancelled)
        THEN BEGIN
          TrainInitialTrackCircuitCount := 1;
          TrainFound := False;
          WHILE (TrainInitialTrackCircuitCount <= 5)
          AND (Train_InitialTrackCircuits[TrainInitialTrackCircuitCount] <> UnknownTC)
          AND NOT TrainFound
          DO BEGIN
            IF (TrackCircuits[Train_InitialTrackCircuits[TrainInitialTrackCircuitCount]].TC_OccupationState = TCPermanentFeedbackOccupation)
            OR (TrackCircuits[Train_InitialTrackCircuits[TrainInitialTrackCircuitCount]].TC_OccupationState = TCFeedbackOccupation)
            THEN
              TrainFound := True
            ELSE
              Inc(TrainInitialTrackCircuitCount);
          END; {WHILE}

          IF TrainFound THEN BEGIN
            { does its initial feedback location match the loco locations file? }
            IF Train_LastLocation <> UnknownLocation THEN BEGIN
              LocationFound := False;
              TrainInitialTrackCircuitCount := 1;
              WHILE (TrainInitialTrackCircuitCount <= 5)
              AND (Train_InitialTrackcircuits[TrainInitialTrackCircuitCount] <> UnknownTC)
              AND NOT LocationFound
              DO BEGIN
                IF GetLocationFromTrackCircuit(Train_InitialTrackCircuits[TrainInitialTrackCircuitCount]) = Train_LastLocation
                THEN
                  LocationFound := True;
                Inc(TrainInitialTrackCircuitCount);
              END; {WHILE}

              IF NOT LocationFound THEN BEGIN
                CASE MessageDialogueWithDefault('Loco ' + Train_LocoChipStr
                                                + ' has trackcircuits marked as having feedback occupation at '
                                                + LocationToStr(GetLocationFromTrackCircuit(Train_InitialTrackCircuits[1]))
                                                + ',' + CRLF
                                                + 'but the loco is recorded as being at ' + LocationToStr(Train_LastLocation)
                                                + CRLF
                                                + 'Do you want to cancel this train, change its occupation to '
                                                + LocationToStr(GetLocationFromTrackCircuit(Train_InitialTrackCircuits[1]))
                                                + CRLF
                                                + 'and activate it, or just ignore the problem?',
                                                StopTimer, mtError, [mbYes, mbNo, mbAbort], ['&Cancel', 'C&hange', '&Ignore'], mbNo)
                OF
                  mrYes: { Cancel }
                    BEGIN
                      Log(Train_LocoChipStr + ' DG System occupation and diagrams entry cancelled by user');
                      CancelTrain(T, ByUser, TrainExists);
                    END;
                  mrNo: { Change }
                    BEGIN
                      { clear the trackcircuits set up from the loco locations file }
                      LocationTCs := GetTrackCircuitsForLocation(Train_LastLocation);
                      FOR I := 0 TO High(LocationTCs) DO BEGIN
                        SetTrackCircuitState(Train_LocoChip, LocationTCs[I], TCUnoccupied,
                                             'Train has trackcircuits marked as having feedback occupation at '
                                             + LocationToStr(GetLocationFromTrackCircuit(Train_InitialTrackCircuits[1]))
                                             + ' but the loco is recorded as being at ' + LocationToStr(Train_LastLocation));
                      END; {FOR}
                      { and need to tell the loco locations file where we really are }
                      Train_LastLocation := GetLocationFromTrackCircuit(Train_InitialTrackCircuits[1]);
                      Train_CurrentTC := Train_InitialTrackCircuits[1];
                      InvalidateScreen(UnitRef, 'CheckOccupiedLinesAndDiagrams');
                    END;
                  mrAbort: { Ignore }
                    BEGIN
                      { clear the trackcircuits set up from the loco locations file }
                      LocationTCs := GetTrackCircuitsForLocation(Train_LastLocation);
                      FOR I := 0 TO High(LocationTCs) DO
                        SetTrackCircuitState(Train_LocoChip, LocationTCs[I], TCUnoccupied,
                                             'Train has TC=' + IntToStr(LocationTCs[I])
                                             + ' marked as having feedback occupation at '
                                             + LocationToStr(GetLocationFromTrackCircuit(Train_InitialTrackCircuits[1]))
                                             + ' but the loco is recorded as being at ' + LocationToStr(Train_LastLocation));
                      { and need to tell the loco locations file where we really are }
                      Train_LastLocation := GetLocationFromTrackCircuit(Train_InitialTrackCircuits[1]);
                      Train_CurrentTC := Train_InitialTrackCircuits[1];
                      { and set up the proper trackcircuits }
                      FOR I := 1 TO 5 DO BEGIN
                        IF Train_InitialTrackCircuits[I] <> UnknownTC THEN BEGIN
                          IF GetTrackCircuitState(Train_InitialTrackCircuits[I]) = TCPermanentFeedbackOccupation THEN BEGIN
                            SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[I],
                                                 TCFeedbackOccupation,
                                                 'TC=' + IntToStr(Train_InitialTrackCircuits[I])
                                                 + ' previously marked as having permanent occupation with feedback at '
                                                 + LocationToStr(GetLocationFromTrackCircuit(Train_InitialTrackCircuits[1]))
                                                 + ' has been set to feedback occupation');
                            TrackCircuits[Train_InitialTrackCircuits[I]].TC_LocoChip := Train_LocoChip;
                          END;
                        END;
                      END;
                    END;

                END; {CASE}

                InvalidateScreen(UnitRef, 'CheckOccupiedLinesAndDiagrams');
              END;
            END;
          END ELSE BEGIN
            { NOT TrainFound }
            IF Train_CurrentStatus <> Cancelled THEN BEGIN
              IF CancelAllTrainsWithNoFeedbackOccupation THEN BEGIN
                Log(Train_LocoChipStr + ' DG System occupation and diagrams entry cancelled (CancelAllTrainsWithNoFeedbackOccupation = ON)');
                CancelTrain(T, ByUser, NOT TrainExists);
              END ELSE
                IF DoNotCancelTrainsWithNoFeedbackOccupation THEN
                  Log(Train_LocoChipStr + ' DG No trackcircuits are marked as having feedback occupation but system occupation and diagrams entry not cancelled as'
                                        + ' "Do Not Cancel trains With No Feedback Occupation" = ON')
                ELSE BEGIN
                  { Turn off painting, or the dialogue is constantly refreshed and one cannot select either "yes" or "no" }
                  IF MessageDialogueWithDefault('No trackcircuits are marked as having feedback occupation for loco ' + Train_LocoChipStr
                                                + CRLF
                                                + 'Do you want to cancel this train or ignore the problem?',
                                                StopTimer, mtError, [mbYes, mbNo], ['&Cancel', '&Ignore'], mbNo) = mrYes
                  THEN BEGIN
                    Log(Train_LocoChipStr + ' DG System occupation and diagrams entry cancelled by user');
                    CancelTrain(T, ByUser, NOT TrainExists);
                  END;
                END;
            END;
          END;
        END;
      END; {WITH}
      T := T^.Train_NextRecord;
    END; {WHILE}
  END;
END; { CheckOccupiedLinesAndDiagrams }

PROCEDURE DiscardDiagrams(CheckIfOK : Boolean);
{ Throw away the current diagrams, and do appropriate tidying up }
VAR
  DiscardOK : Boolean;
  T : Train;
  TrainFound : Boolean;

BEGIN
  DiscardOK := True;
  IF NOT DiagramsLoaded THEN BEGIN
    ShowMessage('No diagrams to discard')
  END ELSE
    IF CheckIfOK THEN
      IF MessageDialogueWithDefault('Discard diagrams - are you sure?', StopTimer, mtWarning, [mbYes, mbNo], mbNo) = mrNo THEN
        DiscardOK := False;

  IF DiscardOK THEN BEGIN
    { now check to see if there are any trains operating or waiting to operate }
    IF DiagramsFileName <> '' THEN
      Log('AG Discarding diagrams "' + DiagramsFileName + '"')
    ELSE
      Log('AG Discarding diagrams');

    T := TrainList;
    TrainFound := False;
    WHILE (T <> NIL)
    AND NOT TrainFound
    DO BEGIN
      WITH T^ DO BEGIN
        IF (Train_LocoChip <> UnknownLocoChip)
        AND Train_DiagramFound
        THEN BEGIN
          { Turn off lights (if any) before clearing loco record }
          IF Train_LightsOn THEN
            TurnLightsOff(Train_LocoChip);

          IF (Train_CurrentStatus <> Suspended)
          AND (Train_CurrentStatus <> MissingAndSuspended)
          AND (Train_CurrentStatus <> Cancelled)
          THEN BEGIN
            TrainFound := True;
            IF MessageDialogueWithDefault('Discard Diagrams Warning'
                                          + CRLF
                                          + 'Operating trains found - cancel them or ignore the problem? ',
                                          StopTimer, mtWarning, [mbOK, mbAbort], ['&Cancel', '&Ignore'], mbAbort) = mrOK
            THEN BEGIN
              Log(Train_LocoChipStr + ' GG Discarding diagrams so cancelling train ' + Train_Headcode);
              ChangeTrainStatus(T, WaitingForRemovalFromDiagrams);
            END;
          END ELSE BEGIN
            Log(Train_LocoChipStr + ' GG Discarding diagrams so cancelling train ' + Train_Headcode);
            ChangeTrainStatus(T, WaitingForRemovalFromDiagrams);
          END;
        END;
      END; {WITH}
      T := T^.Train_NextRecord;
    END; {WHILE}

    { And clear the diagrams }
    PruneTrainList;

    DrawDiagrams(UnitRef, 'DiscardDiagrams');
    DiagramsLoaded := False;
    DiagramsChanged := False;
  END;
END; { DiscardDiagrams }

PROCEDURE SetInitialTrackCircuits(T : Train);
{ Sets the initial trackcircuits for a given route }
VAR
  Done : Boolean;
  FoundEndOfLine : Boolean;
  I : Integer;
  L : Integer;
  NextTC : Integer;
  PreviousLine : Integer;
  TempL : Integer;

  PROCEDURE SetUpOccupancy(VAR T : Train);
  { Sets the initial track occupancy }
  VAR
    Set2nd, Set3rd, Set4th, Set5th : Boolean;
    TempLocation : Integer;

  BEGIN
    Set2nd := False;
    Set3rd := False;
    Set4th := False;
    Set5th := False;

    WITH T^ DO BEGIN
      { Now set initial track occupancy depending on the length of the train. Ignore the first circuit, as the train may only be occupying the last few yards of it }
      IF Train_InitialTrackCircuits[2] = UnknownTC THEN
        Log(Train_LocoChipStr + ' T! Problem with initial trackcircuits: second TC is an unknown TC - is the train direction correct?')
      ELSE BEGIN
        Set2nd := True;

        IF Train_InitialTrackCircuits[3] <> UnknownTC THEN BEGIN
          TempLocation := GetLocationFromTrackcircuit(Train_InitialTrackCircuits[3]);
          IF (TempLocation <> UnknownLocation)
          AND (Locations[TempLocation].Location_IsPlatform
               OR Locations[TempLocation].Location_IsSiding
               OR Locations[TempLocation].Location_IsFiddleyard)
          THEN BEGIN
            IF (TrackCircuits[t^.Train_InitialTrackCircuits[2]].TC_OccupationState = TCFeedbackOccupation)
            OR (Train_CurrentLengthInInches > TrackCircuits[T^.Train_InitialTrackCircuits[2]].TC_LengthInInches)
            THEN BEGIN
              Set3rd := True;
              Log(Train_LocoChipStr + ' T TrainLengthInInches=' + IntToStr(Train_CurrentLengthInInches)
                                    + ' > 2rd track circuit length=' + FloatToStr(TrackCircuits[Train_InitialTrackCircuits[2]].TC_LengthInInches));
            END;
          END;
        END;

        IF Train_InitialTrackCircuits[4] <> UnknownTC THEN BEGIN
          TempLocation := GetLocationFromTrackcircuit(Train_InitialTrackCircuits[4]);
          IF (TempLocation <> UnknownLocation)
          AND (Locations[TempLocation].Location_IsPlatform
               OR Locations[TempLocation].Location_IsSiding
               OR Locations[TempLocation].Location_IsFiddleyard)
          THEN BEGIN
            IF Train_CurrentLengthInInches > (TrackCircuits[T^.Train_InitialTrackCircuits[2]].TC_LengthInInches
                                              + TrackCircuits[t^.Train_InitialTrackCircuits[3]].TC_LengthInInches)
            THEN BEGIN
              Set4th := True;
              Log(Train_LocoChipStr + ' T TrainLengthInInches=' + IntToStr(Train_CurrentLengthInInches)
                                    + ' > 2nd and 3rd track circuit lengths=' + FloatToStr(TrackCircuits[Train_InitialTrackCircuits[2]].TC_LengthInInches
                                                                              + TrackCircuits[t^.Train_InitialTrackCircuits[3]].TC_LengthInInches));
            END;
          END;
        END;

        IF (Train_InitialTrackCircuits[5] <> UnknownTC) THEN BEGIN
          TempLocation := GetLocationFromTrackcircuit(Train_InitialTrackCircuits[5]);
          IF (TempLocation <> UnknownLocation)
          AND (Locations[TempLocation].Location_IsPlatform
               OR Locations[TempLocation].Location_IsSiding
               OR Locations[TempLocation].Location_IsFiddleyard)
          THEN BEGIN
            IF Train_CurrentLengthInInches > (TrackCircuits[t^.Train_InitialTrackCircuits[2]].TC_LengthInInches
                                              + TrackCircuits[T^.Train_InitialTrackCircuits[3]].TC_LengthInInches
                                              + TrackCircuits[T^.Train_InitialTrackCircuits[4]].TC_LengthInInches)
            THEN BEGIN
              Set5th := True;
              Log(Train_LocoChipStr + ' T TrainLengthInInches=' + IntToStr(Train_CurrentLengthInInches)
                                    + ' > 2nd, 3rd and 4th trackcircuit lengths=' + FloatToStr(TrackCircuits[Train_InitialTrackCircuits[2]].TC_LengthInInches
                                                                                  + TrackCircuits[t^.Train_InitialTrackCircuits[3]].TC_LengthInInches
                                                                                  + TrackCircuits[t^.Train_InitialTrackCircuits[4]].TC_LengthInInches));
            END;
          END;
        END;
      END;

      { Always set first and second trackcircuits }
      TrackCircuits[Train_InitialTrackCircuits[1]].TC_LocoChip := Train_LocoChip;
      TrackCircuits[Train_InitialTrackCircuits[1]].TC_Headcode := Train_Headcode;
      IF Set2nd THEN BEGIN
        TrackCircuits[Train_InitialTrackCircuits[2]].TC_LocoChip := Train_LocoChip;
        TrackCircuits[Train_InitialTrackCircuits[2]].TC_Headcode := Train_Headcode;
      END;

      IF (TrackCircuits[Train_InitialTrackCircuits[1]].TC_OccupationState <> TCFeedbackOccupation)
      AND (TrackCircuits[Train_InitialTrackCircuits[1]].TC_OccupationState <> TCPermanentFeedbackOccupation)
      THEN BEGIN
        { means the train ain't there, but that's a problem dealt with elsewhere! - it may also mean we're offline, though }
        IF SystemSetOfflineByCommandLineParameter THEN
          SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[1], TCFeedbackOccupation)
        ELSE BEGIN
          IF Train_UseTrailingTrackCircuits THEN BEGIN
            IF Train_CurrentStatus <> NonMoving THEN
              SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[1], TCSystemOccupation)
            ELSE
              SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[1], TCPermanentSystemOccupation);
          END;
        END;
      END;

      { Second TC }
      IF Set2nd THEN BEGIN
        IF (TrackCircuits[Train_InitialTrackCircuits[2]].TC_OccupationState <> TCFeedbackOccupation)
        AND (TrackCircuits[Train_InitialTrackCircuits[2]].TC_OccupationState <> TCPermanentFeedbackOccupation)
        THEN BEGIN
          IF SystemSetOfflineByCommandLineParameter THEN
            SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[2], TCFeedbackOccupation)
          ELSE BEGIN
            IF Train_UseTrailingTrackCircuits THEN BEGIN
              IF Train_CurrentStatus <> NonMoving THEN
                SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[2], TCSystemOccupation)
              ELSE
                SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[2], TCPermanentSystemOccupation);
            END;
          END;
        END;
      END;

      IF NOT Set3rd THEN
        { initialise the trackcircuit if the train isn't occupying it }
        Train_InitialTrackCircuits[3] := UnknownTC
      ELSE BEGIN
        IF Train_UseTrailingTrackCircuits THEN BEGIN
          IF (TrackCircuits[Train_InitialTrackCircuits[3]].TC_OccupationState <> TCFeedbackOccupation)
          AND (TrackCircuits[Train_InitialTrackCircuits[3]].TC_OccupationState <> TCPermanentFeedbackOccupation)
          THEN BEGIN
            TrackCircuits[Train_InitialTrackCircuits[3]].TC_LocoChip := Train_LocoChip;
            TrackCircuits[Train_InitialTrackCircuits[3]].TC_Headcode := Train_Headcode;
            IF Train_CurrentStatus <> NonMoving THEN
              SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[3], TCSystemOccupation)
            ELSE
              SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[3], TCPermanentSystemOccupation);
          END;
        END ELSE
          { see if the trackcircuit is actually occupied }
          IF (Train_InitialTrackCircuits[3] <> UnknownTC)
          AND ((TrackCircuits[Train_InitialTrackCircuits[3]].TC_OccupationState = TCFeedbackOccupation)
                OR (TrackCircuits[Train_InitialTrackCircuits[3]].TC_OccupationState = TCPermanentFeedbackOccupation))
          THEN BEGIN
            TrackCircuits[Train_InitialTrackCircuits[3]].TC_LocoChip := Train_LocoChip;
            TrackCircuits[Train_InitialTrackCircuits[3]].TC_Headcode := Train_Headcode;
          END;
       END;

      IF NOT Set4th THEN
        { initialise the trackcircuit if the train isn't occupying it }
        Train_InitialTrackCircuits[4] := UnknownTC
      ELSE BEGIN
        IF Train_UseTrailingTrackCircuits THEN BEGIN
          IF (TrackCircuits[Train_InitialTrackCircuits[4]].TC_OccupationState <> TCFeedbackOccupation)
          AND (TrackCircuits[Train_InitialTrackCircuits[4]].TC_OccupationState <> TCPermanentFeedbackOccupation)
          THEN BEGIN
            SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[4], TCSystemOccupation);
            TrackCircuits[Train_InitialTrackCircuits[4]].TC_LocoChip := Train_LocoChip;
            TrackCircuits[Train_InitialTrackCircuits[4]].TC_Headcode := Train_Headcode;
            IF Train_CurrentStatus <> NonMoving THEN
              SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[4], TCSystemOccupation)
            ELSE
              SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[4], TCPermanentSystemOccupation);
          END;
        END ELSE
          { see if the trackcircuit is actually occupied }
          IF (Train_InitialTrackCircuits[4] <> UnknownTC)
          AND ((TrackCircuits[Train_InitialTrackCircuits[4]].TC_OccupationState = TCFeedbackOccupation)
                OR (TrackCircuits[Train_InitialTrackCircuits[4]].TC_OccupationState = TCPermanentFeedbackOccupation))
          THEN BEGIN
            TrackCircuits[Train_InitialTrackCircuits[4]].TC_LocoChip := Train_LocoChip;
            TrackCircuits[Train_InitialTrackCircuits[4]].TC_Headcode := Train_Headcode;
          END;
        END;

      IF NOT Set5th THEN
        { initialise the trackcircuit if the train isn't occupying it }
        Train_InitialTrackCircuits[5] := UnknownTC
      ELSE BEGIN
        IF Train_UseTrailingTrackCircuits THEN BEGIN
          IF (TrackCircuits[Train_InitialTrackCircuits[5]].TC_OccupationState <> TCFeedbackOccupation)
          AND (TrackCircuits[Train_InitialTrackCircuits[5]].TC_OccupationState <> TCPermanentFeedbackOccupation)
          THEN BEGIN
            TrackCircuits[Train_InitialTrackCircuits[5]].TC_LocoChip := Train_LocoChip;
            TrackCircuits[Train_InitialTrackCircuits[5]].TC_Headcode := Train_Headcode;
            IF Train_CurrentStatus <> NonMoving THEN
              SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[5], TCSystemOccupation)
            ELSE
              SetTrackCircuitState(Train_LocoChip, Train_InitialTrackCircuits[5], TCPermanentSystemOccupation);
          END;
        END ELSE
          { see if the trackcircuit is actually occupied }
          IF (Train_InitialTrackCircuits[5] <> UnknownTC)
          AND (TrackCircuits[Train_InitialTrackCircuits[5]].TC_OccupationState = TCFeedbackOccupation)
          THEN BEGIN
            TrackCircuits[Train_InitialTrackCircuits[5]].TC_LocoChip := Train_LocoChip;
            TrackCircuits[Train_InitialTrackCircuits[5]].TC_Headcode := Train_Headcode;
          END;
        END;
    END; {WITH}
  END; { SetUpOccupancy }

  PROCEDURE FindNextTrackCircuit(VAR CurrentLine, PreviousLine : Integer;
                                 OUT CurrentTrackCircuit : Integer;
                                 OUT FoundEndOfLine : Boolean;
                                 SearchDirection : DirectionType);
  { Return the next trackcircuit up or down }
  VAR
    Done : Boolean;
    Next : NextLineRouteingType;

  BEGIN
    { Find the first line which the trackcircuit is part of - it may be part of other adjacent lines too }
    FoundEndOfLine := False;
    Done := False;
    CurrentTrackCircuit := Lines[CurrentLine].Line_TC;
    PreviousLine := CurrentLine;

    REPEAT
      { Find the next one up or down }
      IF SearchDirection = Up THEN
        Next := Lines[CurrentLine].Line_NextUpType
      ELSE
        Next := Lines[CurrentLine].Line_NextDownType;

      CASE Next OF
        PointIsNext, LineIsNext:
          BEGIN
            IF SearchDirection = Up THEN
              CurrentLine := Lines[CurrentLine].Line_NextUpLine
            ELSE
              CurrentLine := Lines[CurrentLine].Line_NextDownLine;

              IF Lines[CurrentLine].Line_TC <> CurrentTrackCircuit THEN BEGIN
                CurrentTrackCircuit := Lines[CurrentLine].Line_TC;
                Done := True;
              END;
          END;
        EndOfLineIsNext:
          BEGIN
            CurrentTrackCircuit := Lines[CurrentLine].Line_TC;
            Done := True;
            FoundEndOfLine := True;
          END;
       END; {CASE}
    UNTIL Done;
  END; { FindNextTrackCircuit }

BEGIN
  WITH T^ DO BEGIN
    IF Train_CurrentSourceLocation = UnknownLocation THEN
      Log(Train_LocoChipStr + ' XG Cannot find initial track circuits when Train_CurrentSourceLocation = UnknownLocation')
    ELSE BEGIN
      { Because not all routes have more than two trackcircuits }
      t^.Train_InitialTrackCircuits[1] := UnknownTC;
      t^.Train_InitialTrackCircuits[2] := UnknownTC;
      t^.Train_InitialTrackCircuits[3] := UnknownTC;
      t^.Train_InitialTrackCircuits[4] := UnknownTC;
      t^.Train_InitialTrackCircuits[5] := UnknownTC;

      { Find the first trackcircuit in the list of lines which is the same location }
      L := 0;
      Done := False;
      WHILE (L <= High(Lines))
      AND NOT Done
      DO BEGIN
        TempL := L;
        IF Train_CurrentSourceLocation = Lines[TempL].Line_Location THEN BEGIN
          { Now find the next trackcircuit }
          REPEAT
            IF Train_CurrentDirection = Up THEN
              FindNextTrackCircuit(TempL, PreviousLine, NextTC, FoundEndOfLine, Up)
            ELSE
              FindNextTrackCircuit(TempL, PreviousLine, NextTC, FoundEndOfLine, Down);

            { and see if its location is the same - if it isn't, save it }
            IF (Train_CurrentSourceLocation <> Lines[TempL].Line_Location) OR FoundEndOfline THEN BEGIN
              { it's different, so the previous line must be the start }
              Train_InitialTrackCircuits[1] := Lines[PreviousLine].Line_TC;
              Done := True;
            END;
          UNTIL Done OR FoundEndOfLine;

          { Now look for ones in the other direction }
          TempL := PreviousLine;
          Done := False;
          FOR I := 1 TO 4 DO BEGIN
            IF Train_InitialTrackCircuits[I] <> UnknownTC THEN BEGIN
              REPEAT
                IF Train_CurrentDirection = Up THEN
                  FindNextTrackCircuit(TempL, PreviousLine, NextTC, FoundEndOfLine, Down)
                ELSE
                  FindNextTrackCircuit(TempL, PreviousLine, NextTC, FoundEndOfLine, Up);

                IF NextTC <> Train_InitialTrackCircuits[I] THEN BEGIN
                  Train_InitialTrackCircuits[I + 1] := NextTC;
                  Done := True;
                END;
              UNTIL Done OR FoundEndOfLine;
            END;
          END;
        END;
        Inc(L);
      END; {WHILE}

      { But see if the first initial trackcircuit is beyond a signal - that may cause the train in that trackcircuit to start
        moving, as it is not immediately adjacent to a stop signal.
      }
      IF Train_CurrentStatus <> NonMoving THEN BEGIN
        IF Length(TrackCircuits[Train_InitialTrackCircuits[1]].TC_AdjacentSignals) = 0 THEN BEGIN
          Log(Train_LocoChipStr + ' EG First initial TC=' + IntToStr(Train_InitialTrackCircuits[1]) + ' has no adjacent signal so trying second initial TC');

          { Try the preceding one }
          IF Length(TrackCircuits[Train_InitialTrackCircuits[2]].TC_AdjacentSignals) = 0 THEN BEGIN
            Log(Train_LocoChipStr + ' EG Second initial TC=' + IntToStr(Train_InitialTrackCircuits[2]) + ' has no adjacent signal so initial trackcircuits cancelled and train cancelled');
            CancelTrain(T, NOT ByUser, TrainExists);
          END ELSE BEGIN
            Log(Train_LocoChipStr + ' EG Second initial TC=' + IntToStr(Train_InitialTrackCircuits[2]) + ' has an adjacent signal so substituting it');
            IF Train_CurrentStatus <> NonMoving THEN BEGIN
              Train_InitialTrackCircuits[1] := Train_InitialTrackCircuits[2];
              Train_InitialTrackCircuits[2] := Train_InitialTrackCircuits[3];
              Train_InitialTrackCircuits[3] := Train_InitialTrackCircuits[4];
              Train_InitialTrackCircuits[4] := Train_InitialTrackCircuits[5];
              Train_InitialTrackCircuits[5] := UnknownTC;
            END;
          END;
        END;
      END;

      SetUpOccupancy(T);

      { and initialise other train variables }
      Train_CurrentTC := Train_InitialTrackCircuits[1];
      Log(Train_LocoChipStr + ' T LocoCurrentTC set to ' + IntToStr(Train_CurrentTC) + ' in SetInitialTrackCircuitsOK routine');
      Train_PreviousTC := Train_InitialTrackCircuits[2];
      Log(Train_LocoChipStr + ' T LocoPreviousTC set to ' + IntToStr(Train_PreviousTC) + ' in SetInitialTrackCircuitsOK routine');

      FOR I := 1 TO 5 DO
        IF Train_InitialTrackCircuits[I] <> UnknownTC THEN
          Log(Train_LocoChipStr + ' T TrainInitialTrackCircuit ' + IntToStr(I) + ' is TC=' + IntToStr(Train_InitialTrackCircuits[I]));

      IF Train_InitialTrackCircuits[5] <> UnknownTC THEN
        AppendToStringArray(Train_TCsOccupiedOrClearedArray, 'TC=' + IntToStr(Train_InitialTrackCircuits[5]));
      IF Train_InitialTrackCircuits[4] <> UnknownTC THEN
        AppendToStringArray(Train_TCsOccupiedOrClearedArray, 'TC=' + IntToStr(Train_InitialTrackCircuits[4]));
      IF Train_InitialTrackCircuits[3] <> UnknownTC THEN
        AppendToStringArray(Train_TCsOccupiedOrClearedArray, 'TC=' + IntToStr(Train_InitialTrackCircuits[3]));
      AppendToStringArray(Train_TCsOccupiedOrClearedArray, 'TC=' + IntToStr(Train_InitialTrackCircuits[2]));
      AppendToStringArray(Train_TCsOccupiedOrClearedArray, 'TC=' + IntToStr(Train_InitialTrackCircuits[1]));
    END;
  END; {WITH}
END; { SetInitialTrackCircuits }

FUNCTION PlatformToString(P : Integer) : String;
{ Convert platform variable to text for output }
BEGIN
//  Result := MainPlatformPlungers[P].TRSPlunger_PlatformNumStr;
END; { PlatformToString }

FUNCTION GetHeadcode(LocoChip, TrainTypeNum : Integer; FinalDestinationStation : String) : String;
{ Return a headcode based on the final destination and train type - so far X (Royal or out-of-gauge) and Z (other special) not used }

  FUNCTION HeadcodeNumToStr(HeadcodeRunningNum : Integer) : String;
  BEGIN
    IF HeadcodeRunningNum < 10 THEN
      Result := '0' + IntToStr(HeadcodeRunningNum)
    ELSE
      Result := IntToStr(HeadcodeRunningNum);
    { truncate the number in the unlikely event it's over 99 }
    IF Length(Result) > 2 THEN
      Result := Copy(Result, 2, 2);
  END; { HeadcodeNumToStr }

BEGIN
  IF TrainTypeNum = 0 THEN BEGIN
    Inc(HeadcodeZRunningNum);
    Result := IntToStr(TrainTypeNum) + 'Z' + HeadcodeNumToStr(HeadcodeZRunningNum);
  END ELSE
    IF (FinalDestinationStation = 'Edinburgh') OR (FinalDestinationStation = 'Glasgow') THEN BEGIN
      { S = Scottish Region }
      Inc(HeadcodeSRunningNum);
      Result := IntToStr(TrainTypeNum) + 'S' + HeadcodeNumToStr(HeadcodeSRunningNum);
    END ELSE
      IF (FinalDestinationStation = 'Newcastle') OR (FinalDestinationStation = 'Sunderland') THEN BEGIN
        { E + Eastern Region }
        Inc(HeadcodeERunningNum);
        Result := IntToStr(TrainTypeNum) + 'E' + HeadcodeNumToStr(HeadcodeERunningNum);
      END ELSE
        IF (FinalDestinationStation = 'King''s Cross')
        OR (FinalDestinationStation = 'Doncaster')
        THEN BEGIN
          { A = London }
          Inc(HeadcodeARunningNum);
          Result := IntToStr(TrainTypeNum) + 'A' + HeadcodeNumToStr(HeadcodeARunningNum);
        END ELSE
          IF (FinalDestinationStation = 'Birmingham')
          OR (FinalDestinationStation = 'Bristol')
          OR (FinalDestinationStation = 'Penzance')
          OR (FinalDestinationStation = 'Swansea')
          OR (FinalDestinationStation = 'Reading')
          THEN BEGIN
            { V = Western Region }
            Inc(HeadcodeVRunningNum);
            Result := IntToStr(TrainTypeNum) + 'V' + HeadcodeNumToStr(HeadcodeVRunningNum);
          END ELSE
            IF (FinalDestinationStation = 'Doncaster')
            OR (FinalDestinationStation = 'Manchester')
            OR (FinalDestinationStation = 'Sheffield')
            OR (FinalDestinationStation = 'Leeds')
            THEN BEGIN
              { M = Midland Region }
              Inc(HeadcodeMRunningNum);
              Result := IntToStr(TrainTypeNum) + 'M' + HeadcodeNumToStr(HeadcodeMRunningNum);
            END ELSE
              IF (FinalDestinationStation = 'Middlesbro')
              OR (FinalDestinationStation = 'Redcar')
              OR (FinalDestinationStation = 'Darlington')
              OR (FinalDestinationStation = 'York')
              OR (FinalDestinationStation = 'Bishop Auckland')
              OR (FinalDestinationStation = 'Northallerton')
              THEN BEGIN
                { E = Eastern Region }
                Inc(HeadcodeERunningNum);
                Result := IntToStr(TrainTypeNum) + 'E' + HeadcodeNumToStr(HeadcodeERunningNum);
              END ELSE
                IF (FinalDestinationStation = 'Bournemouth') THEN BEGIN
                  { O = Southern Region }
                  Inc(HeadcodeORunningNum);
                  Result := IntToStr(TrainTypeNum) + 'O' + HeadcodeNumToStr(HeadcodeORunningNum);
                END ELSE
                  IF (FinalDestinationStation = 'Yard') THEN BEGIN
                    Inc(HeadcodeXRunningNum);
                    Result := IntToStr(TrainTypeNum) + 'X' + HeadcodeNumToStr(HeadcodeERunningNum);
                  END ELSE BEGIN
                    Result := '0000';
                    Log('X No headcode created for ' + LocoChipToStr(LocoChip) //+ ', train type: ' + TrainTypeToNumStr(TrainTypeNum)
                           + ', final destination: ' + FinalDestinationStation);
                  END;
  { Record the headcode }
  Log(LocoChipToStr(LocoChip) + ' D New headcode created: ' + Result);
END; { GetHeadcode }

PROCEDURE AddTrainToTrainList(T : Train; DescribeFullTrainList : Boolean);
{ Add a descriptor for each active loco to the train list. List the full train list (locos only and locos and trains if DescribeFullTrainList is set. }
CONST
  Indent = True;

BEGIN
  Log('D Adding train ' + LocoChipToStr(T^.Train_LocoChip) + ' to the train list');

  { Add train to front of train list }
  T^.Train_NextRecord := TrainList;
  TrainList := T;
END; { AddTrainToTrainList }

PROCEDURE AddTrainToDiagramsList(T : Train);
{ Create the diagrams list }
VAR
  DiagramsEntry : DiagramsEntryType;

BEGIN
  IF T^.Train_DiagramFound THEN BEGIN
    IF T^.Train_LocoChipStr = '' THEN BEGIN
      ASM
        Int 3;
      END; {ASM}
    END;
    Log('D Adding train ' + T^.Train_Headcode + ' (' + T^.Train_LocoChipStr + ')' + ' to the diagrams');
    New(DiagramsEntry);
    DiagramsEntry^.TrainPtr := T;
    DiagramsEntry^.NextDiagramsRecord := DiagramsList;
    DiagramsList := DiagramsEntry;

    { and re-sort it after adding the new entry }
    DiagramsList := ListSort_SortDiagramsList(DiagramsList, DepartureTimeAndTrainTypeSort);
  END;
END; { AddTrainToDiagramsList }

PROCEDURE CancelTrain(T : Train; UserInstruction, TrainExists : Boolean);
{ Mark a train as cancelled, and enable the appropriate popup to display it in the diagrams }
CONST
  IncludeFirstOccupation = True;

VAR
  L : Integer;
  OK : Boolean;
  OtherT : Train;
  R : Integer;
  SR : Integer;
  TC : Integer;

BEGIN
  WITH T^ DO BEGIN
    ChangeTrainStatus(T, Cancelled);
    Log(Train_LocoChipStr + ' D Train cancelled');

    DiagramsWindow.PopupShowCancelledTrains.Enabled := True;
    IF UserInstruction THEN
      Log(Train_LocoChipStr + ' D Train ' + Train_LocoChipStr + ' cancelled by user')
    ELSE
      Log(Train_LocoChipStr + ' D Train ' + Train_LocoChipStr + ' cancelled');

    IF NOT TrainExists THEN BEGIN
      Train_CurrentTC := UnknownTC;

      { Turn its lights off if they've been turned on at program start }
      IF Train_LightsOn THEN
        TurnLightsOff(Train_LocoChip);
    END ELSE BEGIN
      { Record where this train is now permanently situated }
      SetUpTrainLocationOccupationsAbInitio(T, OK);

      { and add the rest }
      OtherT := TrainList;
      WHILE OtherT <> NIL DO BEGIN
        IF T <> OtherT THEN
          SetUpTrainLocationOccupationsAbInitio(OtherT, OK);
        OtherT := OtherT^.Train_NextRecord;
      END;

      Train_TotalJourneys := -1;

      { And delete any routes created for this train }
      FOR R := 0 TO High(Routes_LocoChips) DO BEGIN
        IF Routes_LocoChips[R] = Train_LocoChip THEN BEGIN
          Routes_RouteSettingsInProgress[R] := False;
          Routes_RouteClearingsInProgress[R] := True;

          { Clear the routes without resetting points, as the train may be sitting on one }
          Routes_RouteClearingsWithoutPointResetting[R] := True;
          FOR SR := 0 TO High(Routes_SubRouteStates[R]) DO BEGIN
            IF Routes_SubRouteStates[R, SR] <> SubRouteCleared THEN
              CreateClearingSubRouteArray(R, SR);
            Routes_SubRouteStates[R, SR] := SubRouteToBeCleared;
          END;

          { And also clear any track circuits marked as locked by the route just in case they're not cleared by route clearing }
          FOR TC := 0 TO High(TrackCircuits) DO BEGIN
            IF TrackCircuits[TC].TC_LocoChip = Train_LocoChip THEN
              IF TrackCircuits[TC].TC_OccupationState = TCUnoccupied THEN BEGIN
                TrackCircuits[TC].TC_LocoChip := UnknownLocoChip;
                UnlockTrackCircuitRouteLocking(TC);
              END;
          END; {FOR}

          FOR L := 0 TO High(Lines) DO BEGIN
            IF Lines[L].Line_RouteLockingForDrawing = R THEN
              Lines[L].Line_RouteLockingForDrawing := UnknownRoute;
            IF Lines[L].Line_RouteSet = R THEN
              Lines[L].Line_RouteSet := UnknownRoute;
          END; {FOR}

          Log(Train_LocoChipStr + ' L R=' + IntToStr(R) + ' for cancelled train now deleted');
        END;
      END;
    END;

    DrawDiagrams(UnitRef, 'CancelTrain');
  END; {WITH}
END; { CancelTrain }

PROCEDURE RemoveTrainFromDiagrams(T : Train);
{ Called to indicate a train is no longer of interest; it is removed from the diagrams list, the others moved to fill the gap and a new train generated. The diagrams will
  be redrawn on screen.
}
  PROCEDURE PurgeDiagrams(VAR DiagramsEntry : DiagramsEntryType);
  VAR
    TempDiagramsEntry : DiagramsEntryType;

  BEGIN
    WHILE (DiagramsEntry <> NIL)
    AND (DiagramsEntry^.TrainPtr = T)
    DO BEGIN
      TempDiagramsEntry := DiagramsEntry;
      Log(DiagramsEntry^.TrainPtr^.Train_LocoChipStr + ' D Removing train ' + TempDiagramsEntry^.TrainPtr^.Train_Headcode
                                                     + ' (' + TempDiagramsEntry^.TrainPtr^.Train_LocoChipStr + ')' + ' from the diagrams');
      DiagramsEntry := DiagramsEntry^.NextDiagramsRecord;
      Dispose(TempDiagramsEntry);
    END; {WHILE}

    IF DiagramsEntry <> NIL THEN
      PurgeDiagrams(DiagramsEntry^.NextDiagramsRecord);
  END; { PurgeDiagrams }

BEGIN
  PurgeDiagrams(DiagramsList);
  DrawDiagrams(UnitRef, 'RemoveTrainFromDiagrams');
END; { RemoveTrainFromDiagrams }

PROCEDURE TDiagramsWindow.DiagramsWindowGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
{ We identify which items we want to be highlighted or marked as invalid by prefixing them with a marker; this is necessary as this routine just draws the data it's given
  without knowing where that data has come from).
}
CONST
  CharOffset = 3;

VAR
  CellText: String;
  LeftOffset: Integer;
  DefaultDiagramsGridFontColour : TColor;
  WidthOfText: Integer;
  WidthOfCell: Integer;

BEGIN
  WITH DiagramsWindowGrid DO BEGIN
    WITH DiagramsWindowGrid.Canvas DO BEGIN
      { Make the column headings bold }
      IF ARow = 0 THEN
        Font.Style := [fsBold]
      ELSE
        Font.Style := [];

      DefaultDiagramsGridFontColour := clBlack;
      CellText := Cells[ACol, ARow];

      { If it's a cancelled or suspended train then we use "greyed-out" font colour }
      IF LeftStr(CellText, 3) = GreyedOutStyleStr THEN BEGIN
        { if it's a cancelled or suspended train then we use "greyed-out" font colour }
        Font.Color := clGrayText;
        Brush.Color := DiagramsWindowGridBackgroundColour;
        CellText := Copy(CellText, Length(GreyedOutStyleStr) + 1);
      END ELSE BEGIN
        Font.Color := DefaultDiagramsGridFontColour;
        Brush.Color := DiagramsWindowGridBackgroundColour;
      END;

      IF (HighlightRow > -1)
      AND (HighlightRow = ARow)
      THEN BEGIN
        { highlight the whole line if required }
        Font.Color := clWhite;
        Brush.Color := clBlue;
      END;

      { Now look for a bold marker... }
      IF LeftStr(CellText, Length(BoldStyleStr)) = BoldStyleStr THEN BEGIN
        CellText := Copy(CellText, Length(BoldStyleStr) + 1);
        Font.Style := [fsBold];
      END;

      { ...or Italic }
      IF LeftStr(CellText, Length(ItalicStyleStr)) = ItalicStyleStr THEN BEGIN
        CellText := Copy(CellText, Length(ItalicStyleStr) + 1);
        Font.Style := [fsItalic];
      END;

      { ...or both }
      IF LeftStr(CellText, Length(BoldAndItalicStyleStr)) = BoldAndItalicStyleStr THEN BEGIN
        CellText := Copy(CellText, Length(BoldAndItalicStyleStr) + 1);
        Font.Style := [fsItalic] + [fsBold];
      END;

      WidthOfText := TextWidth(CellText);
      WidthOfCell := ColWidths[ACol];

      IF (ACol = StatusCol) OR (ACol = SpeedCol) THEN
        { do not justify }
        LeftOffset := 0
      ELSE
        LeftOffset := (WidthOfCell - WidthOfText) DIV 2;

      TextRect(Rect, Rect.Left + LeftOffset, Rect.Top, CellText);
      Font.Color := DefaultDiagramsGridFontColour;
    END; {WITH}
  END; {WITH}
END; { DiagramsWindowGridDrawCell }

PROCEDURE DrawDiagramsSpeedCell(T : Train);
{ Draw the speed cell for a given loco - this routine is to avoid having to draw all the diagrams for a minor change }
BEGIN
  WITH T^ DO BEGIN
    IF Train_CurrentStatus <> Cancelled THEN BEGIN
      IF Length(T^.Train_DiagramsGridRowNums) > 0 THEN BEGIN
        WITH DiagramsWindow.DiagramsWindowGrid DO BEGIN
          IF LocosStopped THEN BEGIN
            Cells[SpeedCol, T^.Train_DiagramsGridRowNums[0]] :=
                                  '0'
                                  + IfThen(Train_CurrentLenzSpeed <> 0,
                                           '[' + IntToStr(Train_CurrentLenzSpeed) + ']')
                                  + IfThen(Train_CurrentLenzSpeed <> Train_DesiredLenzSpeed,
                                           ':' + IntToStr(Train_DesiredLenzSpeed))
                                  + IfThen(Train_Accelerating,
                                           ' (' + FloatToStr(Train_AccelerationTimeInSeconds) + 's)')
                                  + IfThen(Train_Decelerating,
                                           ' (' + FloatToStr(Train_AccelerationTimeInSeconds) + 's)')
                                  + IfThen(Train_ExtraPowerAdjustment > 0,
                                           ' [+' + IntToStr(Train_ExtraPowerAdjustment) + ']')
                                  + IfThen(Train_UserPowerAdjustment > 0,
                                           ' {+' + IntToStr(Train_UserPowerAdjustment) + '}')
                                  + IfThen(Train_UserPowerAdjustment < 0,
                                           ' {' + IntToStr(Train_UserPowerAdjustment) + '}')
                                  + IfThen(Train_GradientSpeedAdjustment > 0,
                                           ' <+' + IntToStr(Train_GradientSpeedAdjustment) + '>')
                                  + IfThen(Train_GradientSpeedAdjustment < 0,
                                           ' <' + IntToStr(Train_GradientSpeedAdjustment) + '>')
                                  + IfThen(Train_CurrentLenzSpeed <> 0,
                                           ' [' + MPHToStr(Train_CurrentSpeedInMPH) + ' mph]')
          END ELSE BEGIN
            Cells[SpeedCol, T^.Train_DiagramsGridRowNums[0]] :=
                                  IntToStr(Train_CurrentLenzSpeed)
                                  + IfThen(Train_CurrentLenzSpeed <> Train_DesiredLenzSpeed,
                                           ':' + IntToStr(Train_DesiredLenzSpeed))
                                  + IfThen(Train_Accelerating,
                                           ' (' + FloatToStr(Train_AccelerationTimeInSeconds) + 's)')
                                  + IfThen(Train_Decelerating,
                                           ' (' + FloatToStr(Train_AccelerationTimeInSeconds) + 's)')
                                  + IfThen(Train_ExtraPowerAdjustment > 0,
                                           ' [+' + IntToStr(Train_ExtraPowerAdjustment) + ']')
                                  + IfThen(Train_UserPowerAdjustment > 0,
                                           ' {+' + IntToStr(Train_UserPowerAdjustment) + '}')
                                  + IfThen(Train_UserPowerAdjustment < 0,
                                           ' {' + IntToStr(Train_UserPowerAdjustment) + '}')
                                  + IfThen(Train_GradientSpeedAdjustment > 0,
                                           ' <+' + IntToStr(Train_GradientSpeedAdjustment) + '>')
                                  + IfThen(Train_GradientSpeedAdjustment < 0,
                                           ' <' + IntToStr(Train_GradientSpeedAdjustment) + '>')
                                  + IfThen(Train_CurrentSpeedInMPH <> MPH0,
                                           ' [' + MPHToStr(Train_CurrentSpeedinMPH) + ' mph]');
          END;
        END; {WITH}
      END;
    END;
  END; {WITH}
END; { DrawDiagramsSpeedCell }

PROCEDURE SetUpDiagramsGridCell(T : Train; DiagramsCol, DiagramsRow : Integer; Str : String; Style : CellStyleType);
{ May add an cancelled train marker or highlight some text before passing to the DrawDiagramsGridCell routine (this would not be necessary if that routine knew where
  the text was coming from).
}
BEGIN
  WITH DiagramsWindow.DiagramsWindowGrid DO BEGIN
    IF (T^.Train_CurrentStatus = Suspended)
    OR (T^.Train_CurrentStatus = MissingAndSuspended)
    OR (T^.Train_CurrentStatus = Cancelled)
    THEN
      { override other styles }
      Style := GreyedOutStyle;

    CASE Style OF
      NormalStyle:
        Cells[DiagramsCol, DiagramsRow - 1] := Trim(Str);
      BoldStyle:
        Cells[DiagramsCol, DiagramsRow - 1] := BoldStyleStr + Trim(Str);
      ItalicStyle:
        Cells[DiagramsCol, DiagramsRow - 1] := ItalicStyleStr + Trim(Str);
      BoldAndItalicStyle:
        Cells[DiagramsCol, DiagramsRow - 1] := BoldAndItalicStyleStr + Trim(Str);
      GreyedOutStyle:
        Cells[DiagramsCol, DiagramsRow - 1] := GreyedOutStyleStr + Trim(Str);
    END; {CASE}
  END; {WITH}
END; { SetUpDiagramsGridCell }

PROCEDURE DrawDiagramsStatusCell(T : Train; CellStyle : CellStyleType);
{ Draw the status cell for a given loco - this routine is to avoid having to draw all the diagrams for a minor change }
VAR
  StatusText : String;

BEGIN
  WITH T^ DO BEGIN
    { Options and status }
    StatusText := ' ';
    CASE Train_CurrentStatus OF
      Cancelled:
        StatusText := 'CA';
      CommencedRouteing:
        StatusText := 'CR';
      Departed:
        StatusText := 'DP';
      Missing:
        StatusText := 'Missing';
      MissingAndSuspended:
        StatusText := 'Mis/Sus';
      NonMoving:
        StatusText := 'NM';
      InLightsOnTime:
        StatusText := 'LO';
      ReadyForCreation:
        StatusText := 'RY';
      ReadyForRemovalFromDiagrams:
        { it is unlikely this will be displayed, as it's a status that should only last a few milliseconds, but it is here for the sake of completeness }
        StatusText := 'RT';
      ReadyForRouteing:
        StatusText := 'RR';
      ReadyToDepart:
        StatusText := 'RD';
      RemovedFromDiagrams:
        { this can never be displayed but again it is here for the sake of completeness }
        StatusText := 'XX';
      RouteCompleted:
        StatusText := 'RC';
      RouteingWhileDeparted:
        StatusText := 'RW';
      Suspended:
        StatusText := 'Suspend';
      WaitingForLightsOn:
        StatusText := 'WL';
      WaitingForRouteing:
        StatusText := 'WR';
      WaitingForRemovalFromDiagrams:
        StatusText := 'WT';
    ELSE {CASE}
      StatusText := ' ';
    END; {CASE}

    IF Train_UserDriving THEN
      StatusText := StatusText + ' US';
    IF Train_ReversingWaitStarted THEN
      StatusText := StatusText + ' RW';

    IF Length(T^.Train_DiagramsGridRowNums) > 0 THEN
      SetUpDiagramsGridCell(T, StatusCol, T^.Train_DiagramsGridRowNums[0] + 1, StatusText, CellStyle);

//      WITH DiagramsWindow.DiagramsWindowGrid DO
//        Cells[StatusCol, T^.Train_DiagramsGridRowNums[0]] := StatusText;
  END; {WITH}
END; { DrawDiagramsStatusCell }

PROCEDURE DrawDiagrams(UnitRef : String; Str : String);
{ Write the complete diagrams to the screen after they have been changed }
CONST
  FixedLength = True;
  NoSpeedStr = '';
  OmitUnit = True;

VAR
  CellStyle : CellStyleType;
  ColumnCount : Integer;
  DebugStr : String;
  DebugStrSaveStartLocation : Integer;
  DestinationStr : String;
  DiagramsEntry : DiagramsEntryType;
  JourneyCount : Integer;
  I, J : Integer;
  NewRow : Boolean;
  T : Train;
  TempGridRowCount : Integer;

  FUNCTION DescribeClass(C : String) : String;
  { Return a truncated description of a class of train }
  BEGIN
    IF Pos('Class ', C) > 0 THEN
      Result := Copy(C, 7, 6)
    ELSE
      Result := Copy(C, 1, 6);
  END; { DescribeClass }

BEGIN
  TRY
    WITH DiagramsWindow.DiagramsWindowGrid DO BEGIN
      { clear any existing cells, since this routine is called with different diagram sizes, and text otherwise gets left behind }
      FOR I := 0 TO ColCount DO
        { now only clear row 2 onwards, or the headings disappear }
        FOR J := 1 TO RowCount DO
          Cells[I, J] := ' ';

      Log('D ----------------- new diagrams start');

      TempGridRowCount := 1;

      { Remove lines between the cells }
      GridLineWidth := 0;

      DiagramsEntry := DiagramsList;
      WHILE DiagramsEntry <> NIL DO BEGIN
        T := DiagramsEntry^.TrainPtr;
        WITH T^ DO BEGIN
          IF Train_LocoChip <> UnknownLocoChip THEN BEGIN
            { Omit non-moving and cancelled trains unless told to display them }
            IF Train_DiagramFound THEN BEGIN
              IF ((Train_CurrentStatus = NonMoving)
                  AND ShowNonMovingTrainsInDiagrams)
              OR ((Train_CurrentStatus = Cancelled)
                  AND ShowCancelledTrainsInDiagrams)
              OR ((Train_CurrentStatus <> NonMoving)
                  AND (Train_CurrentStatus <> Cancelled))
              THEN BEGIN
                SetLength(Train_DiagramsGridRowNums, 0);
                TempGridRowCount := TempGridRowCount + 1;
                FOR I := 0 TO ColCount DO
                  { clear the new cells }
                  Cells[I, TempGridRowCount] := ' ';

                AppendToIntegerArray(Train_DiagramsGridRowNums, TempGridRowCount - 1);

                NewRow := False;
                { Draw the cells, If we want a cell to be in bold, add an @ - the OnDrawCell routine will substitute Bold text for it (there is no easier way to do this, as
                  the OnDrawCell routine doesn't know anything about the antecedents of the text it's been given).
                }
                IF Train_LocoChip = UnknownLocoChip THEN
                  SetUpDiagramsGridCell(T, LocoChipCol, TempGridRowCount, '-', BoldStyle)
                ELSE
                  SetUpDiagramsGridCell(T, LocoChipCol, TempGridRowCount, LocoChipToStr(Train_LocoChip), BoldStyle);

                IF NOT ShowTrainLength THEN
                  SetUpDiagramsGridCell(T, HeadcodeCol, TempGridRowCount, Train_Headcode, NormalStyle)
                ELSE
                  SetUpDiagramsGridCell(T, HeadcodeCol, TempGridRowCount, IntToStr(Train_CurrentLengthInInches), NormalStyle);

                SetUpDiagramsGridCell(T, LocoClassCol, TempGridRowCount, DescribeClass(Train_LocoClassStr), NormalStyle);
                SetUpDiagramsGridCell(T, LocoClassCol, TempGridRowCount, DescribeClass(Train_LocoClassStr), NormalStyle);

                IF Train_Locations[0] <> UnknownLocation THEN BEGIN
                  IF Length(Train_JourneysArray) > 0 THEN BEGIN
                    IF Train_JourneysArray[0].TrainJourney_NotForPublicUse THEN
                      SetUpDiagramsGridCell(T, SourceCol, TempGridRowCount, LocationToStr(Train_Locations[0], ShortStringType), BoldAndItalicStyle)
                    ELSE
                      SetUpDiagramsGridCell(T, SourceCol, TempGridRowCount, LocationToStr(Train_Locations[0], ShortStringType), BoldStyle);
                  END;
                END;

                SetUpDiagramsGridCell(T, JourneyCol, TempGridRowCount, IntToStr(T^.Train_CurrentJourney), NormalStyle);
                DebugStrSaveStartLocation := Train_CurrentSourceLocation;

                DrawDiagramsStatusCell(T, NormalStyle);
                SetUpDiagramsGridCell(T, SpeedCol, TempGridRowCount, NoSpeedStr, NormalStyle);

                { and save the same data to write to the log file }
                DebugStr := LocoChipToStr(Train_LocoChip) + ' ' + Train_Headcode;

                { Draw as many as we can }
                ColumnCount := 0;
                DestinationStr := '';
                JourneyCount := 0;
                WHILE JourneyCount <= Train_TotalJourneys DO BEGIN
                  { Maybe hide a diagrams stage if the train doesn't stop there }
  //                IF HideNonStopsInDiagrams
  //                AND (TempJourneyCount < Train_TotalJourneys)
  //                AND ((TempJourneyCount > 0)
  //                     AND NOT Train_JourneysArray[TempJourneyCount - 1].TrainJourney_StoppingOnArrival)
  //                THEN
  //                  Inc(TempJourneyCount)
  //                ELSE BEGIN
                  BEGIN
                    { Write out each diagram stage whether the train stops there or not }
                    DebugStr := DebugStr + ' | ' + ReturnFixedLengthStr(LocationToStr(DebugStrSaveStartLocation, ShortStringType), 4);
                    IF NewRow
                    AND (JourneyCount > 0)
                    THEN BEGIN
                      { Start the row with the next starting point }
                      IF t^.Train_JourneysArray[JourneyCount - 1].TrainJourney_StoppingOnArrival THEN
                        CellStyle := BoldStyle
                      ELSE
                        IF (t^.Train_JourneysArray[JourneyCount - 1].TrainJourney_StoppingOnArrival) THEN
                          CellStyle := BoldStyle
                        ELSE
                          CellStyle := NormalStyle;

                      IF t^.Train_JourneysArray[JourneyCount].TrainJourney_NotForPublicUse THEN
                        CellStyle := ItalicStyle;

                      SetUpDiagramsGridCell(T, SourceCol, TempGridRowCount, DestinationStr, CellStyle);
                      NewRow := False;
                    END;

                    { Set up the style the cell is drawn in }
                    IF Train_JourneysArray[JourneyCount].TrainJourney_Cleared THEN BEGIN
                      CellStyle := GreyedOutStyle;
                      { and make the original train source "greyed out" too }
                      SetUpDiagramsGridCell(T, SourceCol, TempGridRowCount, LocationToStr(Train_Locations[0], ShortStringType), CellStyle);
                    END ELSE
                      IF t^.Train_JourneysArray[JourneyCount].TrainJourney_NotForPublicUse THEN
                        CellStyle := ItalicStyle
                      ELSE
                        CellStyle := NormalStyle;

                    { Arrival/departure times }
                    IF ShowArrivalTimes THEN
                      SetUpDiagramsGridCell(T, TimeCol[ColumnCount], TempGridRowCount, TimeToHMStr(Train_JourneysArray[JourneyCount].TrainJourney_CurrentArrivalTime),
                                            CellStyle)
                    ELSE
                      SetUpDiagramsGridCell(T, TimeCol[ColumnCount], TempGridRowCount, TimeToHMStr(Train_JourneysArray[JourneyCount].TrainJourney_CurrentDepartureTime),
                                            CellStyle);

                    { Up or down }
                    SetUpDiagramsGridCell(T, UpDownCol[ColumnCount], TempGridRowCount, DirectionToStr(Train_JourneysArray[JourneyCount].TrainJourney_Direction,
                                          ShortStringType), CellStyle);

                    DestinationStr := LocationToStr(Train_JourneysArray[JourneyCount].TrainJourney_EndLocation, ShortStringType);
                    DebugStrSaveStartLocation := Train_JourneysArray[JourneyCount].TrainJourney_EndLocation;
                    IF DestinationStr = UnknownLocationStr THEN
                      DestinationStr := '?';

                    { And destination }
                    IF Train_JourneysArray[JourneyCount].TrainJourney_Cleared THEN
                      CellStyle := GreyedOutStyle
                    ELSE
                      IF (JourneyCount < Train_TotalJourneys)
                      AND (Train_JourneysArray[JourneyCount + 1].TrainJourney_Cleared)
                      THEN
                        CellStyle := GreyedOutStyle
                      ELSE
                        IF Train_JourneysArray[JourneyCount].TrainJourney_StoppingOnArrival
                        AND (JourneyCount < Train_TotalJourneys)
                        AND t^.Train_JourneysArray[JourneyCount + 1].TrainJourney_NotForPublicUse THEN
                          CellStyle := BoldAndItalicStyle
                        ELSE
                          IF Train_JourneysArray[JourneyCount].TrainJourney_StoppingOnArrival THEN
                            CellStyle := BoldStyle
                          ELSE
                            IF t^.Train_JourneysArray[JourneyCount].TrainJourney_NotForPublicUse THEN
                              CellStyle := ItalicStyle
                            ELSE
                              CellStyle := NormalStyle;

                    SetUpDiagramsGridCell(T, DestinationCol[ColumnCount], TempGridRowCount, DestinationStr, CellStyle);
                    Inc(ColumnCount);
                    IF ColumnCount > MaxColumnCount THEN
                      MaxColumnCount := ColumnCount;

                    IF ((ColumnCount = NumberOfRepeatColumns)
                       AND (JourneyCount < Train_TotalJourneys))
                    OR (StartRepeatJourneysOnNewLineInDiagrams
                       AND (JourneyCount < Train_TotalJourneys)
                       AND Train_JourneysArray[JourneyCount + 1].TrainJourney_StartOfRepeatJourney)
                    THEN BEGIN
                      { start a new row }
                      ColumnCount := 0;
                      TempGridRowCount := TempGridRowCount + 1;
                      AppendToIntegerArray(Train_DiagramsGridRowNums, TempGridRowCount - 1);
                      NewRow := True;
                    END;

                    { and save the same data to write to the log file }
                    DebugStr := DebugStr + ' d:' +  TimeToHMStr(Train_JourneysArray[JourneyCount].TrainJourney_CurrentDepartureTime);
                    DebugStr := DebugStr + ' ' + DirectionToStr(Train_JourneysArray[JourneyCount].TrainJourney_Direction, ShortStringType);
                    DebugStr := DebugStr + ' ' + ReturnFixedLengthStr(LocationToStr(Train_JourneysArray[JourneyCount].TrainJourney_EndLocation, ShortStringType), 4);
                    DebugStr := DebugStr + ' a:' + TimeToHMStr(Train_JourneysArray[JourneyCount].TrainJourney_CurrentArrivalTime);

                    Inc(JourneyCount);
                  END;
                END; {WHILE}
              END;
            END;
            DrawDiagramsSpeedCell(T);
          END;
        END; {WITH}

        { Copy it to the log file }
        IF DebugStr <> '' THEN BEGIN
          Log('D ' + DebugStr + ' {NOUNITREF}');
          DebugStr := '';
        END;
        DiagramsEntry := DiagramsEntry^.NextDiagramsRecord;
      END; {WHILE}
      DiagramsWindow.DiagramsWindowGrid.RowCount := TempGridRowCount;
    END; {WITH}
    Log('D ----------------- new diagrams end');


    IF StationMonitorsWindow.Visible THEN
      { redraw it erasing the window first }
      DrawStationMonitorsWindow
    ELSE BEGIN
      IF RecordingMonitorScreens THEN BEGIN
        SaveStationMonitorsCurrentArea := StationMonitorsCurrentArea;
        StationMonitorsCurrentArea := StrToArea('IslandStationArea');
        DrawStationMonitorsWindow;
        StationMonitorsCurrentArea := StrToArea('MainStationArea');
        DrawStationMonitorsWindow;
        StationMonitorsCurrentArea := SaveStationMonitorsCurrentArea;
      END;

      { Move the focus back to the main window }
      IF (FWPRailMainWindow.Visible
      AND NOT LocationDataWindow.Visible)
      AND NOT (HelpWindow.Active OR HelpWindow.Visible)
      THEN
        FWPRailMainWindow.SetFocus;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawDiagrams: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DrawDiagrams }

PROCEDURE DrawDiagramsWindow;
{ Set up default window and grid sizes }
VAR
  I, J : Integer;
  DestinationColWidth : Integer;
  LocoChipColWidth : Integer;
  LocoClassColWidth : Integer;
  HeadcodeColWidth : Integer;
  LocoJobNumColWidth : Integer;
  SourceColWidth : Integer;
  SpeedColWidth : Integer;
  StatusColWidth : Integer;
  TimeColWidth : Integer;
  UpDownColWidth : Integer;
  TotalWidthOfColumns : Integer;

BEGIN
  WITH DiagramsWindow DO BEGIN
    { reset the column count }
    MaxColumnCount := 0;

    Height := DiagramsWindowHeight;
    Top := DiagramsWindowTop;
    Left := DiagramsWindowLeft;
    IF LargeDiagramsWindowSelected THEN
      Width := DiagramsLargeWindowWidth
    ELSE
      Width := DiagramsSmallWindowWidth;

    WITH DiagramsWindowGrid DO BEGIN
      IF LargeDiagramsWindowSelected THEN BEGIN
        { clear existing grid before resizing it (only clear row 2 onwards, or the headings disappear) }
        FOR I := 0 TO ColCount DO
          FOR J := 1 TO RowCount DO
            Cells[I, J] := ' ';

        NumberOfRepeatColumns := 10;
        RowCount := 2;
        ColCount := 7 + (3 * NumberOfRepeatColumns);

        LocoChipColWidth := 25;
        LocoClassColWidth := 30;
        HeadcodeColWidth := 25;
        LocoJobNumColWidth := 12;
        SourceColWidth := 27;

        DestinationColWidth := 27;
        TimeColWidth := 30;
        UpDownColWidth := 15;

        StatusColWidth := 40;
        SpeedColWidth := 110;
      END ELSE BEGIN
        { Small Diagrams Window }

        { clear existing grid before resizing it (only clear row 2 onwards, or the headings disappear) }
        FOR I := 0 TO ColCount DO
          FOR J := 1 TO RowCount DO
            Cells[I, J] := ' ';

        NumberOfRepeatColumns := 4;
        RowCount := 2;
        ColCount := 7 + (3 * NumberOfRepeatColumns);

        LocoChipColWidth := 50;
        LocoClassColWidth := 60;
        HeadcodeColWidth := 50;
        LocoJobNumColWidth := 25;
        SourceColWidth := 55;

        DestinationColWidth := 55;
        TimeColWidth := 60;
        UpDownColWidth := 30;

        StatusColWidth := 70;
        SpeedColWidth := 200;
      END;

      TotalWidthOfColumns := LocoChipColWidth + LocoClassColWidth + HeadcodeColWidth + LocoJobNumColWidth + SourceColWidth
                             + (DestinationColWidth * NumberOfRepeatColumns)
                             + (TimeColWidth * NumberOfRepeatColumns)
                             + (UpDownColWidth * NumberOfRepeatColumns)
                             + StatusColWidth + SpeedColWidth
                             - (BevelWidth * 2) { why does width of all the columns slightly exceed width of the small diagrams window  *** }
                             - (BorderWidth * 2);

      SetLength(TimeCol, 0);
      SetLength(UpDownCol, 0);
      SetLength(DestinationCol, 0);

      FOR I := 0 TO (NumberOfRepeatColumns - 1) DO BEGIN
        IF I = 0 THEN
          AppendToIntegerArray(TimeCol, SourceCol + 1)
        ELSE
          AppendToIntegerArray(TimeCol, DestinationCol[I - 1] + 1);
        AppendToIntegerArray(UpDownCol, TimeCol[I] + 1);
        AppendToIntegerArray(DestinationCol, UpDownCol[I] + 1);
      END;

      { now the column widths and titles }
      ColWidths[LocoChipCol] := MulDiv(Width, LocoChipColWidth, TotalWidthOfColumns);
      ColWidths[LocoClassCol] := MulDiv(Width, LocoClassColWidth, TotalWidthOfColumns);
      ColWidths[HeadcodeCol] := MulDiv(Width, HeadcodeColWidth, TotalWidthOfColumns);
      ColWidths[JourneyCol] := MulDiv(Width, LocoJobNumColWidth, TotalWidthOfColumns);
      ColWidths[SourceCol] := MulDiv(Width, SourceColWidth, TotalWidthOfColumns);

      FOR I := 0 TO (NumberOfRepeatColumns - 1) DO BEGIN
        ColWidths[TimeCol[I]] := MulDiv(Width, TimeColWidth, TotalWidthOfColumns);
        ColWidths[UpDownCol[I]] := MulDiv(Width, UpDownColWidth, TotalWidthOfColumns);
        ColWidths[DestinationCol[I]] := MulDiv(Width, DestinationColWidth, TotalWidthOfColumns);
      END;

      StatusCol := DestinationCol[NumberOfRepeatColumns - 1] + 1;
      ColWidths[StatusCol] := MulDiv(Width, StatusColWidth, TotalWidthOfColumns);
      SpeedCol := StatusCol + 1;
      ColWidths[SpeedCol] := MulDiv(Width, SpeedColWidth, TotalWidthOfColumns);

      { Now the column headings }
      Cells[LocoChipCol, 0] := 'Loco';
      Cells[LocoClassCol, 0] := 'Class';
      Cells[JourneyCol, 0] := 'J';
      Cells[SourceCol, 0] := 'From';
      IF ShowTrainLength THEN
        Cells[HeadCodeCol, 0] := 'Len'
      ELSE
        Cells[HeadcodeCol, 0] := 'H/C';

      FOR I := 0 TO (NumberOfRepeatColumns - 1) DO BEGIN
        IF ShowArrivalTimes THEN
          Cells[TimeCol[I], 0] := 'Arr'
        ELSE
          Cells[TimeCol[I], 0] := 'Dep';
        Cells[UpDownCol[I], 0] := 'U';
        Cells[DestinationCol[I], 0] := 'To';
      END;

      Cells[StatusCol, 0] := 'Status';
      Cells[SpeedCol, 0] := 'Speed';

      { Clear the grid }
      FOR I := 0 TO (NumberOfRepeatColumns - 1) DO
        FOR J:= 1 to 2 DO
          Cells[I, J] := '';
    END; {WITH}
    IF NOT DisplayWorkingTimetable THEN
      Show;

    { and move the focus back to the main window }
    IF FWPRailMainWindow.Visible THEN
      FWPRailMainWindow.SetFocus;
  END; {WITH}
END; { DrawDiagramsWindow }

PROCEDURE TDiagramsWindow.DiagramsWindowCreate(Sender: TObject);
BEGIN
  DrawDiagramsWindow;
END; { DiagramsWindowCreate }

PROCEDURE TDiagramsWindow.DiagramsWindowClose(Sender: TObject; VAR Action: TCloseAction);
{ Hides but does not close the window }
BEGIN
  Action := caHide;
END; { DiagramsWindowClose }

PROCEDURE TDiagramsWindow.DiagramsWindowGridKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    { Exclude most non-alphanumeric keys }
    vk_Shift, vk_Control, vk_Menu { Alt }, vk_Pause, vk_SnapShot { PrtSc }, vk_LWin, vk_RWin, vk_Apps { Windows Applications }, vk_Numlock, vk_Scroll,
    vk_Cancel { Ctrl-Break}, vk_Capital { Caps Lock }, vk_Back { Backspace }:
      { do nothing };
  ELSE
    KeyPressedDown(Key, ShiftState);
  END; {CASE}
END; { DiagramsWindowGridKeyDown }

PROCEDURE TDiagramsWindow.PopupDriveTrainClick(Sender: TObject);
BEGIN
  IF NOT SystemOnline THEN
    Debug('Cannot select a loco - system offline')
  ELSE BEGIN
    IF DiagramsChosenTrain <> NIL THEN BEGIN
      LocoDialogueWindow.Show;
      LocoDialogue.LocoDialogueWindow.LocoDialogueLocoMaskEdit.Clear;
      LocoDialogueWindow.LocoDialogueLocoMaskEdit.SelText := LocoChipToStr(DiagramsChosenTrain^.Train_LocoChip);
      EnableLocoDialogueLocoButtonsAndBoxes;
    END;
  END;
END; { PopupDriveTrainClick }

PROCEDURE SuspendTrain(T : Train; User : Boolean);
{ Suspend a given train - it can be reactivated in the diagrams window }
VAR
  DebugStr : String;
  OK : Boolean;

BEGIN
  WITH T^ DO BEGIN
    IF Train_CurrentStatus = Missing THEN
      ChangeTrainStatus(T, MissingAndSuspended)
    ELSE
      ChangeTrainStatus(T, Suspended);

    IF User THEN
      DebugStr := 'has been suspended by user'
    ELSE
      DebugStr := 'has been suspended by the system';

    Log(Train_LocoChipStr + ' DG ' + DebugStr);
    { and set its speed to zero }

    StopAParticularLocomotive(Train_LocoChip, OK);
  END; {WITH}
END; { SuspendTrain }

PROCEDURE TDiagramsWindow.PopupSuspendTrainClick(Sender: TObject);
BEGIN
  IF DiagramsChosenTrain <> NIL THEN BEGIN
    WITH DiagramsChosenTrain^ DO BEGIN
      IF (Train_CurrentStatus = Suspended) OR (Train_CurrentStatus = MissingAndSuspended) THEN BEGIN
        IF Train_CurrentStatus = Suspended THEN
          ChangeTrainStatus(DiagramsChosenTrain, Train_PreviousStatus)
        ELSE
          IF Train_CurrentStatus = MissingAndSuspended THEN
            ChangeTrainStatus(DiagramsChosenTrain, Missing);
        Log(Train_LocoChipStr + ' DG Train has been reactivated by user');
        PopupSuspendTrain.Caption := 'Suspend Train ' + LocoChipToStr(Train_LocoChip);
      END ELSE BEGIN
        SuspendTrain(DiagramsChosenTrain, ByUser);
        PopupSuspendTrain.Caption := 'Activate Suspended Train ' + LocoChipToStr(Train_LocoChip);
      END;
      DiagramsChosenTrain := NIL;
    END; {WITH}
  END;
  DrawDiagrams(UnitRef, 'PopupSuspendTrainClick');
END; { PopupSuspendTrainClick }

PROCEDURE TDiagramsWindow.PopupMarkTrainNotMissingClick(Sender: TObject);
BEGIN
  IF DiagramsChosenTrain <> NIL THEN BEGIN
    WITH DiagramsChosenTrain^ DO BEGIN
      IF Train_CurrentStatus = Missing THEN BEGIN
        PopupMarkTrainNotMissing.Enabled := False;
        ReturnTrainFromMissing(DiagramsChosenTrain);
      END;
    END; {WITH}
  END;
  DrawDiagrams(UnitRef, 'PopupMarkTrainNotMissingClick');
END; { PopupMarkTrainNotMissingClick }

PROCEDURE TDiagramsWindow.PopupCancelTrainClick(Sender: TObject);
VAR
  OK : Boolean;

BEGIN
  IF DiagramsChosenTrain <> NIL THEN BEGIN
    WITH DiagramsChosenTrain^ DO BEGIN
      CancelTrain(DiagramsChosenTrain, ByUser, TrainExists);
      Log(Train_LocoChipStr + ' DG Train has been cancelled by user');

      { and set its speed to zero }
      StopAParticularLocomotive(Train_LocoChip, OK);

      DiagramsChosenTrain := NIL;
    END; {WITH}
  END;
  DrawDiagrams(UnitRef, 'PopupCancelTrainClick');
END; { PopupCancelTrainClick }

PROCEDURE TDiagramsWindow.PopupTrainUserDrivingClick(Sender: TObject);
BEGIN
  IF DiagramsChosenTrain <> NIL THEN BEGIN
    WITH DiagramsChosenTrain^ DO BEGIN
      IF Train_UserDriving THEN BEGIN
        Train_UserDriving := False;
        { and reset the speed, in case we're already in motion }
        Train_CurrentSpeedInMPH := MPH0;
        Log(Train_LocoChipStr + ' DG Train no longer being driven by the user');
        PopupTrainUserDriving.Caption := 'Make Train ' + LocoChipToStr(Train_LocoChip) + ' User Driven';
      END ELSE BEGIN
        Train_UserDriving := True;
        Log(Train_LocoChipStr + ' DG Train now being driven by the user');
        PopupTrainUserDriving.Caption := 'Make Train ' + LocoChipToStr(Train_LocoChip) + ' Computer Driven';
      END;
      DiagramsChosenTrain := NIL;
    END; {WITH}
  END;
  DrawDiagrams(UnitRef, 'PopupTrainUserDrivenClick');
END; { PopupTrainUserDrivenClick }

PROCEDURE TDiagramsWindow.PopupSupplyUserTrainInstructionsClick(Sender: TObject);
BEGIN
  IF DiagramsChosenTrain <> NIL THEN BEGIN
    WITH DiagramsChosenTrain^ DO BEGIN
      IF Train_UserRequiresInstructions THEN BEGIN
        Train_UserRequiresInstructions := False;
        Log(Train_LocoChipStr + ' D Instructions no longer supplied to user');
        PopupSupplyUserTrainInstructions.Caption := 'Supply User Instructions';
      END ELSE BEGIN
        IF NOT Train_UserDriving THEN
          Debug('!Loco ' + LocoChipToStr(Train_LocoChip) + ': cannot supply user instructions if user is not driving the train')
        ELSE BEGIN
          Train_UserRequiresInstructions := True;
          Log(Train_LocoChipStr + ' DG User instructions being supplied');
          PopupSupplyUserTrainInstructions.Caption := 'Cancel Supply Of User Instructions';
        END;
      END;
      DiagramsChosenTrain := NIL;
    END; {WITH}
  END;
  DrawDiagrams(UnitRef, 'PopupTrainUserDrivenClick');
END; { PopupSupplyUserTrainInstructionsClick }

PROCEDURE TDiagramsWindow.PopupShowLastTrainErrorMessageClick(Sender: TObject);
VAR
  DebugStr : String;

BEGIN
  DebugStr := '';
  IF DiagramsChosenTrain <> NIL THEN BEGIN
    WITH DiagramsChosenTrain^ DO BEGIN
      IF Train_LastRouteLockedMsgStr <> '' THEN
        DebugStr := LocoChipToStr(Train_LocoChip) + ': ' +  Train_LastRouteLockedMsgStr;
      IF Train_RouteCreationHoldMsg <> '' THEN
        IF DebugStr = '' THEN
          DebugStr := LocoChipToStr(Train_LocoChip) + ': ' + Train_RouteCreationHoldMsg
        ELSE
          DebugStr := DebugStr + ', ' + LocoChipToStr(Train_LocoChip) + ': ' + Train_RouteCreationHoldMsg;
      IF DebugStr <> '' THEN
        Debug(DebugStr)
      ELSE
        Debug('No error messages for loco ' + LocoChipToStr(Train_LocoChip));
    END; {WITH}
  END;
END; { ShowLastTrainErrorMessageClick }

PROCEDURE TDiagramsWindow.DiagramsWindowGridMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
{ See if we're over a time, in which case display departure and arrival times alternatively, or perhaps we're clicking on a loco to advance it by hand }
CONST
  Delayed = True;
  HeadingRow = 0;
  NoRoute = 0;
  QuickStop = True;

VAR
  ColsAndRows : TGridCoord;
  DiagramsColWidth : Integer;
  I : Integer;
  Journey : Integer;
  JourneyFound : Boolean;
  T : Train;
  SaveTrainBeingAdvancedTC : Integer;
  StrToTest : String;

  FUNCTION FindTrainRecordFromColumnOne(TestY : Integer) : Train;
  { Find the loco by looking at the first column, stepping back through the grid if necessary }
  VAR
    LocoNumFound : Boolean;
    StrToTest : String;

  BEGIN
    LocoNumFound := False;
    Result := NIL;
    TestY := ColsAndRows.Y;
    IF TestY > -1 THEN BEGIN
      REPEAT
        StrToTest := DiagramsWindowGrid.Cells[0, TestY];
        IF (StrToTest <> '')
        AND (StrToTest <> ' ')
        THEN BEGIN
          LocoNumFound := True;
          { remove the formatting characters if any }
          IF Copy(StrToTest, 1, Length(BoldStyleStr)) = BoldStyleStr THEN
            StrToTest := Copy(StrToTest, Length(BoldStyleStr) + 1);
          IF Copy(StrToTest, 1, Length(ItalicStyleStr)) = ItalicStyleStr THEN
            StrToTest := Copy(StrToTest, Length(ItalicStyleStr) + 1);
          IF Copy(StrToTest, 1, Length(BoldAndItalicStyleStr)) = BoldAndItalicStyleStr THEN
            StrToTest := Copy(StrToTest, Length(BoldAndItalicStyleStr) + 1);
          IF Copy(StrToTest, 1, Length(GreyedOutStyleStr)) = GreyedOutStyleStr THEN
            StrToTest := Copy(StrToTest, Length(GreyedOutStyleStr) + 1);

          Result := GetTrainRecord(StrToInt(StrToTest));
        END;
        Dec(TestY);
      UNTIL (TestY = 0) OR LocoNumFound;
    END;
  END; { FindTrainRecordFromColumnOne }

  PROCEDURE EnableTrainOperations(T : Train);
  { Enable operations via the Diagrams popup }
  BEGIN
    WITH T^ DO BEGIN
      PopupTrainLocoChip.Caption := 'Train ' + LocoChipToStr(Train_LocoChip);
      PopupTrainLocoChip.Visible := True;
      PopupTrainRule1.Visible := True;

      PopupCancelTrain.Caption := 'Cancel Train';
      PopupCancelTrain.Enabled := True;

      IF (Train_CurrentStatus = Suspended) OR (Train_CurrentStatus = MissingAndSuspended) THEN
        PopupSuspendTrain.Caption := 'Activate Train'
      ELSE
        PopupSuspendTrain.Caption := 'Suspend Train';
      PopupSuspendTrain.Enabled := True;

      IF Train_CurrentStatus = Missing THEN
        PopupMarkTrainNotMissing.Enabled := True;

      PopupSupplyUserTrainInstructions.Enabled := True;

      PopupDriveTrain.Caption := 'Drive Train';
      PopupDriveTrain.Enabled := True;

      PopupShowLastTrainErrorMessage.Caption := 'Show Train Last Error Message';
      PopupShowLastTrainErrorMessage.Enabled := True;

      IF Train_UserDriving THEN
        PopupTrainUserDriving.Caption := 'Make Train Computer Driven'
      ELSE
        PopupTrainUserDriving.Caption := 'User Wishes To Drive Train';
      PopupTrainUserDriving.Enabled := True;
    END; {WITH}
  END; { EnableTrainOperations }

  PROCEDURE DisableTrainOperations;
  { Disable operations via the Diagrams popup }
  BEGIN
    PopupTrainLocoChip.Visible := False;
    PopupTrainRule1.Visible := False;

    PopupCancelTrain.Caption := 'Cancel Train';
    PopupCancelTrain.Enabled := False;

    PopupMarkTrainNotMissing.Enabled := False;
    PopupSupplyUserTrainInstructions.Enabled := False;

    PopupSuspendTrain.Caption := 'Suspend Train';
    PopupSuspendTrain.Enabled := False;

    PopupDriveTrain.Caption := 'Drive Train';
    PopupDriveTrain.Enabled := False;

    PopupShowLastTrainErrorMessage.Caption := 'Show Train Last Error Message';
    PopupShowLastTrainErrorMessage.Enabled := False;

    PopupTrainUserDriving.Caption := 'Make Train User Driven';
    PopupTrainUserDriving.Enabled := False;
  END; { DisableTrainOperations }

BEGIN
  { First check for right clicks when the loco window is visible }
  IF LocoDialogueWindow.Visible
  AND (GetLocoDialogueSelectedLocoSpeed > 0)
  AND (Button = mbRight)
  THEN
    CheckEmergencyStop(Button, ShiftState)
  ELSE BEGIN
    ColsAndRows := DiagramsWindowGrid.MouseCoord(X, Y);

    IF (ColsAndRows.Y <> HeadingRow)
    AND (ColsAndRows.X <> SpeedCol)
    AND (Button = mbRight)
    THEN BEGIN
      { If it's a right click, do nothing if we are in the speed column, otherwise see if we want to control a train via the popup }
      T := FindTrainRecordFromColumnOne(ColsAndRows.Y);
      IF T = NIL THEN
        DisableTrainOperations
      ELSE BEGIN
        WITH T^ DO BEGIN
          IF (Train_LocoChip <> UnknownLocoChip)
          AND Train_DiagramFound
          THEN BEGIN
            IF (Length(Train_DiagramsGridRowNums) = 0) OR (Train_DiagramsGridRowNums[0] <> ColsAndRows.Y) THEN
              DisableTrainOperations
            ELSE BEGIN
              DiagramsChosenTrain := T;
              IF Train_CurrentStatus <> Cancelled THEN
                EnableTrainOperations(DiagramsChosenTrain)
              ELSE
                DisableTrainOperations;
            END;
          END;
        END; {WITH}
      END;

      { Move the popup over by the width of the column }
      DiagramsColWidth := DiagramsWindowGrid.ColWidths[ColsAndRows.Y];
      { and pop it up }
      DiagramsPopupMenu.Popup(X + DiagramsColWidth, Y + Top);
    END ELSE
      { See if we're over a time, in which case display departure and arrival times alternatively }
      IF (ColsAndRows.Y = HeadingRow)
      AND (Button = mbLeft)
      THEN BEGIN
        IF ColsAndRows.X = HeadcodeCol THEN BEGIN
          ShowTrainLength := NOT ShowTrainLength;
          DrawDiagramsWindow;
          DrawDiagrams(UnitRef, 'DiagramsWindowGridMouseDown');
        END;

        FOR I := 0 TO (NumberOfRepeatColumns - 1) DO BEGIN
          IF ColsAndRows.X = TimeCol[I] THEN BEGIN
            ShowArrivalTimes := NOT ShowArrivalTimes;
            DrawDiagramsWindow;
            DrawDiagrams(UnitRef, 'DiagramsWindowGridMouseDown');
          END;
        END;
      END ELSE
        IF  (ColsAndRows.Y <> HeadingRow)
        AND (ColsAndRows.X = JourneyCol)
        THEN BEGIN
          { we may need to advance the journey (if, for example, a journey has been concluded but the system hasn't noticed) }
          T := FindTrainRecordFromColumnOne(ColsAndRows.Y);
          IF T <> NIL THEN BEGIN
            WITH T^ DO BEGIN
              IF NOT Train_JourneysArray[Train_CurrentJourney].TrainJourney_Cleared THEN BEGIN
                Train_JourneysArray[Train_CurrentJourney].TrainJourney_Cleared := True;
                Log(Train_LocoChipStr + ' LG J=' + IntToStr(Train_CurrentJourney) + ' cleared by user clicking on the diagram');
                DrawDiagrams(UnitRef, 'DiagramsWindowGridMouseDown');
              END;
            END; {WITH}
          END;
        END ELSE
          { perhaps we're clicking on a loco's speed to increase or decrease it }
          IF  (ColsAndRows.Y <> HeadingRow)
          AND (ColsAndRows.X = SpeedCol)
          THEN BEGIN
            T := FindTrainRecordFromColumnOne(ColsAndRows.Y);
            IF T <> NIL THEN BEGIN
              WITH T^ DO BEGIN
                IF (Train_CurrentStatus <> Cancelled)
                AND (Train_CurrentStatus <> Suspended)
                AND (Train_CurrentStatus <> MissingAndSuspended)
                THEN BEGIN
                  IF Train_CurrentLenzSpeed = 0 THEN
                    Debug('Cannot change speed for ' + Train_LocoChipStr + ' as it is stationary')
                  ELSE
                    IF Button = mbLeft THEN BEGIN
                      { increase the speed (but not if the loco is stationary) }
                      IF (Train_CurrentLenzSpeed > 0)
                      AND (Train_CurrentLenzSpeed < 28)
                      THEN BEGIN
                        Inc(Train_UserPowerAdjustment);
                        Log(Train_LocoChipStr + ' L User power adjustment is now set to '
                                              + IfThen(Train_UserPowerAdjustment > 0,
                                                       '+',
                                                       '')
                                              + IntToStr(Train_UserPowerAdjustment));
                      END;
                    END ELSE
                      IF Button = mbRight THEN BEGIN
                        { decrease the speed }
                        IF NOT (ssCtrl IN ShiftState) THEN BEGIN
                          IF Train_CurrentLenzSpeed > 0 THEN BEGIN
                            Dec(Train_UserPowerAdjustment);
                            Log(Train_LocoChipStr + ' L User power adjustment is now set to '
                                                  + IfThen(Train_UserPowerAdjustment > 0,
                                                           '+',
                                                           '')
                                                  + IntToStr(Train_UserPowerAdjustment));
                          END;
                        END ELSE BEGIN
                          { any other shift state means we want to remove the user power adjustment immediately }
                          Train_UserPowerAdjustment := 0;
                          Log(Train_LocoChipStr + ' L User power adjustment is now turned off');
                        END;
                      END;
                END;
              END; {WITH}
            END;
          END ELSE
            { Or perhaps we're clicking on a loco to select it for routeing or advance it by hand }
            IF (ColsAndRows.Y <> HeadingRow)
            AND (ColsAndRows.X = LocoChipCol) AND (Button = mbLeft)
            THEN BEGIN
              T := FindTrainRecordFromColumnOne(ColsAndRows.Y);
              IF T <> NIL THEN BEGIN
                { Now advance it if we can }
                WITH T^ DO BEGIN
                  IF Train_CurrentRoute = UnknownRoute THEN BEGIN
                    IF HighlightRow = ColsAndRows.Y THEN BEGIN
                      HighlightRow := -1;
                      TrainClickedOn := NIL;
                    END ELSE BEGIN
                      HighlightRow := ColsAndRows.Y;
                      TrainClickedOn := T;
                    END;
                    DrawDiagrams(UnitRef, 'DiagramsWindowGridMouseDown');
                  END ELSE BEGIN
                    Train_BeingAdvanced := True;
                    Log(Train_LocoChipStr + ' R Advancing train ' + Train_LocoChipStr + ' (loco ' + Train_LocoChipStr + ')');
                    { for each click, move the train on by one trackcircuit }
                    SaveTrainBeingAdvancedTC := Train_BeingAdvancedTC;
                    IF Length(Train_TCsNotClearedArray) > 0 THEN BEGIN
                      SetTrackCircuitState(Train_LocoChip, ExtractTrackCircuitFromString(Train_TCsNotClearedArray[0]), TCFeedbackOccupation);
                      Log(Train_LocoChipStr + ' TG TC=' + IntToStr(ExtractTrackCircuitFromString(Train_TCsNotClearedArray[0])) + ' marked as occupied by train advance');
                      Train_BeingAdvancedTC := ExtractTrackCircuitFromString(Train_TCsNotClearedArray[0]);
                    END;

                    IF (Train_BeingAdvancedTC <> SaveTrainBeingAdvancedTC)
                    AND (SaveTrainBeingAdvancedTC <> UnknownTC)
                    THEN BEGIN
                      IF InAutoMode THEN BEGIN
                        SetTrackCircuitState(SaveTrainBeingAdvancedTC, TCSystemOccupation);
                        Log(Train_LocoChipStr + ' T TC=' + IntToStr(SaveTrainBeingAdvancedTC) + ' marked as system occupation by train advance');
                      END ELSE BEGIN
                        SetTrackCircuitState(SaveTrainBeingAdvancedTC, TCUnoccupied);
                        Log(Train_LocoChipStr + ' T TC=' + IntToStr(SaveTrainBeingAdvancedTC) + ' marked as unoccupied by train advance');
                      END;
                    END;
                  END; {WITH}
                END;
              END;
            END ELSE
              IF ColsAndRows.Y <> HeadingRow THEN BEGIN
                { See if we want to change a train's stopping status }
                T := FindTrainRecordFromColumnOne(ColsAndRows.Y);
                IF T <> NIL THEN BEGIN
                  WITH T^ DO BEGIN
                    { Find which journey it is - start from the second journey }
                    Journey := 0;
                    JourneyFound := False;
                    WHILE (Journey < Train_TotalJourneys)
                    AND NOT JourneyFound
                    DO BEGIN
                      IF (ColsAndRows.X - 2 > -1)
                      AND (DiagramsWindowGrid.Cells[ColsAndRows.X - 2, ColsAndRows.Y] = TimeToHMStr(Train_JourneysArray[Journey].TrainJourney_CurrentDepartureTime))
                      THEN BEGIN
                        StrToTest := DiagramsWindowGrid.Cells[ColsAndRows.X, ColsAndRows.Y];
                        IF Copy(StrToTest, 1, 3) = BoldStyleStr THEN
                          { remove the Bold character if any }
                          StrToTest := Copy(StrToTest, 4);

                        IF StrToTest = LocationToStr(Train_JourneysArray[Journey].TrainJourney_EndLocation, ShortStringType) THEN BEGIN
                          JourneyFound := True;
                          IF T^.Train_JourneysArray[Journey].TrainJourney_StoppingOnArrival THEN BEGIN
                            Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': changed by user to not stopping on arrival');
                            T^.Train_JourneysArray[Journey].TrainJourney_StoppingOnArrival := False;
                            ClearHiddenAspectSignals(T, Train_JourneysArray[Journey].TrainJourney_EndSignal);
                          END ELSE BEGIN
                            IF Signals[Train_JourneysArray[Journey].TrainJourney_EndSignal].Signal_Aspect <> RedAspect THEN
                              Debug('!J=' + IntToStr(Journey)
                                    + ': cannot be changed by user to stopping on arrival as hidden aspect signal S='
                                    + IntToStr(Train_JourneysArray[Journey].TrainJourney_EndSignal) + ' is already off')
                            ELSE BEGIN
                              SetHiddenAspectSignals(T, Train_JourneysArray[Journey].TrainJourney_EndSignal, Journey, NoRoute);
                              Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': changed by user to stopping on arrival');
                              T^.Train_JourneysArray[Journey].TrainJourney_StoppingOnArrival := True;
                            END;
                          END;

                          RecalculateJourneyTimes(T, 'as journey has been incremented by the user');

                          DrawDiagrams(UnitRef, 'DiagramsWindowGridMouseDown');
                        END;
                      END;
                      Inc(Journey);
                    END; { WHILE}
                  END;
                END; {WITH}
              END;
  END;
END; { DiagramsWindowGridMouseDown }

PROCEDURE TDiagramsWindow.DiagramsWindowHide(Sender: TObject);
{ Un-check the window menu item }
BEGIN
  FWPRailMainWindow.MainDisplayMenuDiagramsWindow.Checked := False;
END; { DiagramsWindowHide }

PROCEDURE TDiagramsWindow.DiagramsWindowShow(Sender: TObject);
{ Check the window menu item }
BEGIN
  FWPRailMainWindow.MainDisplayMenuDiagramsWindow.Checked := True;
  FWPRailMainWindow.MainDisplayMenuWorkingTimetableWindow.Checked := False;
  DiagramsWindowGrid.Color := DiagramsWindowGridBackgroundColour;
END; { DiagramsWindowShow }

PROCEDURE TDiagramsWindow.DiagramsWindowGridSelectCell(Sender: TObject; ACol, ARow: Integer; VAR CanSelect: Boolean);
BEGIN
  { Now a hack to stop the whole row being highlighted when Escape is pressed }
  IF NOT EscapePressed THEN
    { Exclude the title row then... }
    IF ARow > 0 THEN BEGIN
      WITH DiagramsWindowGrid DO BEGIN
        { ...select the whole row for editing }
        Options := Options + [goRowSelect];
        { and save the loco chip num in case we are manually going to set a route up and need to know the train's characteristics **** }

      END; {WITH}
    END;
  EscapePressed := False;
END; { DiagramsWindowGridSelectCell }

PROCEDURE TDiagramsWindow.DiagramsWindowDeactivate(Sender: TObject);
VAR
  TempRect : TGridRect;

BEGIN
  { De-highlight the previously selected row when the diagrams window loses focus }
  WITH DiagramsWindow.DiagramsWindowGrid DO BEGIN
    { deselect the whole row for editing }
    Options := Options - [goRowSelect];
    TempRect.Left := 0;
    TempRect.Top := 0;
    TempRect.Right := 0;
    TempRect.Bottom := 0;
    Selection := TempRect;
  END; {WITH}
END; { DiagramsWindowDeactivate }

PROCEDURE TDiagramsWindow.PopupShowNonMovingTrainsClick(Sender: TObject);
BEGIN
  IF NOT ShowNonMovingTrainsInDiagrams THEN BEGIN
    PopupShowNonMovingTrains.Caption := '&Hide Non-Moving trains';
    ShowNonMovingTrainsInDiagrams := True;
  END ELSE BEGIN
    PopupShowNonMovingTrains.Caption := '&Show Non-Moving trains';
    ShowNonMovingTrainsInDiagrams := False;
  END;
  DrawDiagrams(UnitRef, 'PopupShowNonMovingTrainsClick');
END; { PopupShowNonMovingTrainsClick }

PROCEDURE TDiagramsWindow.PopupShowCancelledTrainsClick(Sender: TObject);
BEGIN
  IF NOT ShowCancelledTrainsInDiagrams THEN BEGIN
    PopupShowCancelledTrains.Caption := '&Hide Cancelled trains';
    ShowCancelledTrainsInDiagrams := True;
  END ELSE BEGIN
    PopupShowCancelledTrains.Caption := '&Show Cancelled trains';
    ShowCancelledTrainsInDiagrams := False;
  END;
  DrawDiagrams(UnitRef, 'PopupShowCancelledTrainsClick');
END; { PopupShowCancelledTrainsClick }

PROCEDURE TDiagramsWindow.PopupShowNonStopsClick(Sender: TObject);
BEGIN
  IF ShowNonStopsInDiagrams THEN BEGIN
    ShowNonStopsInDiagrams := True;
    PopupShowNonStops.Caption := 'Hide Non-Stops';
  END ELSE BEGIN
    ShowNonStopsInDiagrams := False;
    PopupShowNonStops.Caption := 'Show Non-Stops';
  END;
  DrawDiagrams(UnitRef, 'PopupShowNonStopsClick');
END; { PopupShowNonStopsClick }

PROCEDURE TDiagramsWindow.PopupSelectDiagramsWindowSizeClick(Sender: TObject);
BEGIN
  IF LargeDiagramsWindowSelected THEN BEGIN
    LargeDiagramsWindowSelected := False;
    PopupSelectDiagramsWindowSize.Caption := 'Select Wide Diagrams Window';
  END ELSE BEGIN
    LargeDiagramsWindowSelected := True;
    PopupSelectDiagramsWindowSize.Caption := 'Select Narrow Diagrams Window';
  END;
  DrawDiagramsWindow;
  DrawDiagrams(UnitRef, 'SelectDiagramsWindowSizeClick');
END; { PopupSelectDiagramsWindowSizeClick }

PROCEDURE TDiagramsWindow.DiagramsPopupMenuOnPopup(Sender: TObject);
BEGIN
  IF ShowNonStopsInDiagrams THEN
    PopupShowNonStops.Caption := 'Hide Non-Stops'
  ELSE
    PopupShowNonStops.Caption := 'Show Non-Stops';

  IF ShowCancelledTrainsInDiagrams THEN
    PopupShowCancelledTrains.Caption := 'Hide Cancelled Trains'
  ELSE
    PopupShowCancelledTrains.Caption := 'Show Cancelled Trains';
  IF ShowNonMovingTrainsInDiagrams THEN
    PopupShowNonMovingTrains.Caption := 'Hide Non-Moving Trains'
  ELSE
    PopupShowNonMovingTrains.Caption := 'Show Non-Moving Trains';

  IF LargeDiagramsWindowSelected THEN
    PopupSelectDiagramsWindowSize.Caption := 'Select Narrow Diagrams Window'
  ELSE
    PopupSelectDiagramsWindowSize.Caption := 'Select Wide Diagrams Window';

  IF DiagramsWindow.Top <> DefaultDiagramsWindowTop THEN
    PopupResetDiagramsWindowSizeAndPosition.Enabled := True
  ELSE
    PopupResetDiagramsWindowSizeAndPosition.Enabled := False;
END; { DiagramsPopupMenuOnPopup }

PROCEDURE TDiagramsWindow.PopupChangeDiagramsColourClick(Sender: TObject);
BEGIN
  { Show the default }
  DiagramsWindowColourDialogue.Color := DiagramsWindowGridBackgroundColour;
  { Allow the user to change it }
  IF DiagramsWindowColourDialogue.Execute THEN BEGIN
    DiagramsWindowGrid.Color := DiagramsWindowColourDialogue.Color;
    DiagramsWindowGridBackgroundColour := DiagramsWindowColourDialogue.Color;
  END;
END; { opupChangeDiagramsColourClick }

PROCEDURE TDiagramsWindow.PopupRestoreDefaultColourClick(Sender: TObject);
BEGIN
  DiagramsWindowGrid.Color := DefaultDiagramsWindowGridBackgroundColour;
  DiagramsWindowGridBackgroundColour := DefaultDiagramsWindowGridBackgroundColour;
END; { RestoreDefaultColourClick }

PROCEDURE TDiagramsWindow.PopupResetDiagramsWindowSizeAndPositionClick(Sender: TObject);
BEGIN
  DiagramsWindowHeight := DefaultDiagramsWindowHeight;
  DiagramsLargeWindowWidth := DefaultDiagramsLargeWindowWidth;
  DiagramsSmallWindowWidth := DefaultDiagramsSmallWindowWidth;
  DiagramsWindowTop := DefaultDiagramsWindowTop;
  DiagramsWindowLeft := DefaultDiagramsWindowLeft;

  DrawDiagramsWindow;
  DrawDiagrams(UnitRef, 'PopupResetDiagramsWindowSizeAndPositionClick');
END; { PopupResetDiagramsWindowSizeAndPositionClick }

PROCEDURE TDiagramsWindow.DiagramsWindowResize(Sender: TObject);
BEGIN
  { Enable or disable the popup menu item allowing us to return the window to its default size }
  IF (DiagramsWindow.Height <> DefaultDiagramsWindowHeight)
  OR (NOT LargeDiagramsWindowSelected
      AND (DiagramsWindow.Width <> DefaultDiagramsSmallWindowWidth))
  OR (LargeDiagramsWindowSelected
      AND (DiagramsWindow.Width <> DefaultDiagramsLargeWindowWidth))
  OR (DiagramsWindow.Top <> DefaultDiagramsWindowTop)
  OR (DiagramsWindow.Left <> DefaultDiagramsWindowLeft)
  THEN
    PopupResetDiagramsWindowSizeAndPosition.Enabled := True
  ELSE
    PopupResetDiagramsWindowSizeAndPosition.Enabled := False;

  { Also allow the diagram window to be maximised }

END; { DiagramsSmallWindowResize }

PROCEDURE TDiagramsWindow.DiagramsWindowGridMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
{ If the mouse moves into the Diagrams window, move the focus there; also find out which columns and rows we are hovering over }
CONST
  HeadingRow = 0;

VAR
  ColsAndRows : TGridCoord;

BEGIN
  WITH DiagramsWindow.DiagramsWindowGrid DO BEGIN
    ColsAndRows := DiagramsWindowGrid.MouseCoord(X, Y);

    IF (ColsAndRows.X > -1)
    AND (ColsAndRows.Y > -1)
    AND (SaveCursor <> crHandPoint)
    THEN
      ChangeCursor(crHandPoint)
    ELSE
      ChangeCursor(crDefault);
  END; {WITH}

//  { If the mouse moves into the window, move the focus there }
//  IF NOT KeyboardAndMouseLocked
//  AND (DiagramsWindow <> NIL)
//  THEN
//    IF NOT DiagramsWindow.Active THEN
//      DiagramsWindow.SetFocus;

//  ColsAndRows := DiagramsWindowGrid.MouseCoord(X, Y);
//  { see if we're over a departure time, in which case display arrival times instead (this works but has been moved to MouseDown instead - keeping it here to show how to
//    deal with "hovering" FWP 15/9/06).
//  }
//  IF (ColsAndRows.Y = HeadingRow)
//  AND ((ColsAndRows.X = Time1Col)
//     OR (ColsAndRows.X = Time2Col)
//     OR (ColsAndRws.X = Time3Col)
//     OR (ColsAndRows.X = Time4Col))
//  THEN
//    ShowArrivalTimes := True
//  ELSE
//    ShowArrivalTimes := False;
//  IF ShowArrivalTimes <> SaveShowArrivalTimes THEN BEGIN
//    DrawDiagramsWindow;
//    DrawDiagrams(UnitRef, 'DiagramsWindowGridMouseMove');
//    SaveShowArrivalTimes := ShowArrivalTimes;
//  END;
END; { DiagramsWindowGridMouseMove }

FUNCTION DescribeJourney(T : Train; Journey : Integer) : String;
{ Given various parameters, set up each individual journey within the overall train record }
BEGIN
  WITH T^.Train_JourneysArray[Journey] DO BEGIN
    Result := IfThen(TrainJourney_StartLocation <> UnknownLocation,
                     LocationToStr(TrainJourney_StartLocation, ShortStringType),
                     'Loc=?')
              + ' (tt ' + IfThen(TrainJourney_DiagrammedStartLocation <> UnknownLocation,
                                 LocationToStr(TrainJourney_DiagrammedStartLocation, ShortStringType),
                                'Loc=?')
              + ')'
              + ' L=' + IfThen(TrainJourney_StartLine <> UnknownLine,
                               LineToStr(TrainJourney_StartLine),
                               '?')
              + ' A=' + AreaToStr(TrainJourney_StartArea, ShortStringType)
              + ' ' + DirectionToStr(TrainJourney_Direction, ShortStringType)
              + IfThen(TrainJourney_StartStationName <> '',
                       ' [' + TrainJourney_StartStationName + ']')
              + ' curr ' + TimeToHMStr(TrainJourney_CurrentDepartureTime)
              + ' [tt ' + TimeToHMStr(TrainJourney_DiagrammedDepartureTime) + ']'
              + ' to '
              + IfThen(TrainJourney_EndLocation <> UnknownLocation,
                     LocationToStr(TrainJourney_EndLocation, ShortStringType),
                     'Loc=?')
              + ' (tt ' + IfThen(TrainJourney_DiagrammedEndLocation <> UnknownLocation,
                                 LocationToStr(TrainJourney_DiagrammedEndLocation, ShortStringType),
                                'Loc=?')
              + ')'
              + ' L=' + IfThen(TrainJourney_EndLine <> UnknownLine,
                               LineToStr(TrainJourney_EndLine),
                               '?')
              + ' A=' + AreaToStr(TrainJourney_EndArea, ShortStringType)
              + IfThen(TrainJourney_EndSignal <> UnknownSignal,
                       ' S=' + IntToStr(TrainJourney_EndSignal),
                       IfThen(TrainJourney_EndBufferStop <> UnknownBufferStop,
                              ' BS=' + IntToStr(TrainJourney_EndBufferStop),
                              ' no S/BS'))
              + IfThen(TrainJourney_EndStationName <> '',
                       ' [' + TrainJourney_EndStationName + ']')
              + ' curr ' + TimeToHMStr(TrainJourney_CurrentArrivalTime)
              + ' [tt ' + TimeToHMStr(TrainJourney_DiagrammedArrivalTime) + ']'
              + IfThen(TrainJourney_UserToDrive,
                       ' [U]')
              + IfThen(TrainJourney_StoppingOnArrival,
                       ' [SOA]');
    IF T^.Train_CurrentRoute <> UnknownRoute THEN
      Result := Result + ' R=' + IntToStr(T^.Train_CurrentRoute);
    IF NOT TrainJourney_Created THEN
      Result := Result + ' - not created or setup'
    ELSE
      IF NOT TrainJourney_SetUp THEN
        Result := Result + ' - created but not setup'
      ELSE
        Result := Result + ' - created and set up';
  END; {WITH}
END; { DescribeJourney }

FUNCTION CheckJourney(T : Train; Journey : Integer; Direction : DirectionType; StartLocation, EndLocation : Integer; StartLine, EndLine : Integer; OUT ErrorMsg : String;
                      OUT LinesNotAvailableStr : String; UseEmergencyRouteing : Boolean; OUT JourneyArray : StringArrayType; OUT OK : Boolean) : Boolean;
{ See if each projected journey is correct }
CONST
  IncludeOutOfUseLines = True;

BEGIN
  OK := True;

  IF Direction = UnknownDirection THEN BEGIN
    ErrorMsg := 'No direction supplied';
    OK := False;
  END ELSE BEGIN
    { See if it is a known route, or an approximate route }
    IF Direction = Up THEN BEGIN
      IF StartLine = UnknownLine THEN
        StartLine := Locations[StartLocation].Location_LineAtUp;
      IF EndLine = UnknownLine THEN
        EndLine := Locations[EndLocation].Location_LineAtUp;
    END ELSE BEGIN
      IF StartLine = UnknownLine THEN
        StartLine := Locations[StartLocation].Location_LineAtDown;
      IF EndLine = UnknownLine THEN
        EndLine := Locations[EndLocation].Location_LineAtDown;
    END;

    IF StartLine = UnknownLine THEN BEGIN
      IF Direction = Up THEN
        ErrorMsg := 'Problem with start location "' + LocationToStr(StartLocation) + '" - the line up from it is an unknown line'
      ELSE
        ErrorMsg := 'Problem with start location "' + LocationToStr(StartLocation) + '" - the line down from it is an unknown line';
      OK := False;
    END ELSE
      IF EndLine = UnknownLine THEN BEGIN
        IF Direction = Up THEN
          ErrorMsg := 'Problem with end location "' + LocationToStr(EndLocation) + '" - the line up from it is an unknown line'
        ELSE
          ErrorMsg := 'Problem with end location "' + LocationToStr(EndLocation) + '" - the line down from it is an unknown line';
        OK := False;
      END;

    IF OK THEN BEGIN
      WITH T^ DO
        FindRouteFromLineAToLineB(Train_LocoChip, Journey, UnknownSignal, StartLine, EndLine, Direction, Train_Type, Train_CurrentLengthInInches, UseEmergencyRouteing,
                                  NOT IncludeOutOfUseLines, JourneyArray, LinesNotAvailableStr, ErrorMsg, OK);
      IF NOT OK THEN
        ErrorMsg := ErrorMsg + ': no route ' + DirectionToStr(Direction) + ' found between ' + Lines[StartLine].Line_Str + ' and ' + Lines[EndLine].Line_Str;
    END;
  END;

  Result := OK;
END; { CheckJourney }

FUNCTION CalculateJourneyTimeInMinutes(TrainTypeNum : Integer; RouteLengthInInches : Real) : Integer;
{ Given the journey length in inches, calculate the journey time in minutes }
BEGIN
  Result := 1;

  CASE TrainTypeNum OF
    0: { light loco }
      Result := Round(RouteLengthInInches / 100);
    1:
      Result := Round(RouteLengthInInches / 150);
    2, 3, 5 { express freight / empty coaching stock }:
      Result := Round(RouteLengthInInches / 125);
    7 { medium fast freight }:
      Result := Round(RouteLengthInInches / 80);
    4, 6 { milk train }:
      Result := Round(RouteLengthInInches / 60);
    8, 9 { not fully fitted freight & unfitted freight }:
      Result := Round(RouteLengthInInches / 40);
  END; {CASE}

  IF Result < 1 THEN
    Result := 1;
END; { CalculateJourneyTimeInMinutes }

FUNCTION CalculateRouteLength(RouteArray : StringArrayType) : Real;
{ Using the route array, calculate the route length. (We need this to calculate how long journeys take). }
VAR
  RouteArrayPos : Integer;
  SaveTCToTest : Integer;
  TCToTest : Integer;
  TempLine : Integer;

BEGIN
  Result := 0;
  SaveTCToTest := UnknownTC;
  FOR RouteArrayPos := 0 TO High(RouteArray) DO BEGIN
    TempLine := ExtractLineFromString(RouteArray[RouteArrayPos]);
    IF TempLine <> UnknownLine THEN BEGIN
      TCToTest := Lines[TempLine].Line_TC;
      IF (TCToTest <> UnknownTC)
      AND (TCToTest <> SaveTCToTest)
      THEN BEGIN
        SaveTCToTest := TCToTest;
        Result:= Result + TrackCircuits[TCToTest].TC_LengthInInches;
      END;
    END;
  END;
END; { CalculateRouteLength }

PROCEDURE CreateJourney(VAR T : Train; Journey : Integer; NewJourney: Boolean; StartArea, EndArea, StartLocation, EndLocation: Integer;
                        DiagrammedStartLocation, DiagrammedEndLocation, StartLine, EndLine: Integer;
                        CurrentDepartureTime, DiagrammedDepartureTime, CurrentArrivalTime: TDateTime; TrainDirection : DirectionType;
                        JourneyRouteArray : StringArrayType; RebuildRouteArray : Boolean;
                        StoppingOnArrival, NotForPublicUse, RouteingInEmergency, StartOfRepeatJourney, AreDiagramsLoading : Boolean;
                        OUT ErrorMsg : String; OUT LinesNotAvailableStr : String; OUT OK : Boolean);
{ Given various parameters, set up each individual journey within the overall train record; returns ok if the journey route is clear }
CONST
  EmergencyRouteing = True;
  IncludeOutOfUseLines = True;

VAR
  DebugStr : String;
  I : Integer;
  FirstLineFound : Boolean;
  JourneyRouteArrayUserMustDriveTCFound : Boolean;
  JourneyRouteArrayPos : Integer;
  L : Integer;
  NextLine : Integer;
  RouteArrayPos : Integer;
  SaveCurrentArrivalTime : TDateTime;
  SaveCurrentDepartureTime : TDateTime;
  SaveDiagrammedDepartureTime : TDateTime;
  SaveDiagrammedEndLocation : Integer;
  SaveDiagrammedStartLocation : Integer;
  SaveRouteArray : StringArrayType;
  SaveStartArea, SaveEndArea : Integer;
  SaveStartLocation, SaveEndLocation : Integer;
  SaveStartLine, SaveEndLine : Integer;
  SaveTCToTest : Integer;
  SaveStoppingOnArrival : Boolean;
  SaveNotForPublicUse : Boolean;
  SaveStartOfRepeatJourney : Boolean;
  TCToTest : Integer;
  TempLine : Integer;
  TempStr : String;

BEGIN
  TRY
    WITH T^ DO BEGIN
      OK := True;

      IF NewJourney THEN BEGIN
        { If a journey number is supplied, it means that we have already created the skeleton of a new journey by means of the InsertElementInTrainJourneyRecArray routine
          which can insert a journey in the middle of the array
        }
        IF Journey = UnknownJourney THEN BEGIN
          { add a new array element at the end of the array }
          SetLength(Train_JourneysArray, Length(Train_JourneysArray) + 1);
          Journey := High(Train_JourneysArray);
        END;
        Log(Train_LocoChipStr + ' D CREATING NEW JOURNEY J=' + IntToStr(Journey));

        WITH Train_JourneysArray[Journey] DO BEGIN
          TrainJourney_ActualDepartureTime := 0;
          TrainJourney_ActualArrivalTime := 0;
          TrainJourney_DiagrammedDepartureTime := 0;
          TrainJourney_DiagrammedStartLocation := UnknownLocation;
          TrainJourney_DiagrammedEndLocation := UnknownLocation;
          TrainJourney_DurationInMinutes := 0;
          SetLength(TrainJourney_RouteArray, 0);
        END; {WITH}

        SaveStartArea := UnknownArea;
        SaveEndArea := UnknownArea;
        SaveStartLocation := UnknownLocation;
        SaveEndLocation := UnknownLocation;
        SaveDiagrammedStartLocation := UnknownLocation;
        SaveDiagrammedEndLocation := UnknownLocation;
        SaveStartLine := UnknownLine;
        SaveEndLine := UnknownLine;
        SaveCurrentDepartureTime := 0;
        SaveDiagrammedDepartureTime := 0;
        SaveCurrentArrivalTime := 0;
        SaveStoppingOnArrival := False;
        SaveNotForPublicUse := False;
        StartOfRepeatJourney := False;
      END ELSE BEGIN
        Log(Train_LocoChipStr + ' D AMENDING JOURNEY J=' + IntToStr(Journey));
        WITH Train_JourneysArray[Journey] DO BEGIN
          SaveStartArea := TrainJourney_StartArea;
          SaveEndArea := TrainJourney_EndArea;
          SaveStartLocation := TrainJourney_StartLocation;
          SaveEndLocation := TrainJourney_EndLocation;
          SaveDiagrammedStartLocation := TrainJourney_DiagrammedStartLocation;
          SaveDiagrammedEndLocation := TrainJourney_DiagrammedEndLocation;
          SaveStartLine := TrainJourney_StartLine;
          SaveEndLine := TrainJourney_EndLine;
          SaveCurrentDepartureTime := TrainJourney_CurrentDepartureTime;
          SaveCurrentArrivalTime := TrainJourney_CurrentArrivalTime;
          SaveNotForPublicUse := TrainJourney_NotForPublicUse;
          SaveStoppingOnArrival := TrainJourney_StoppingOnArrival;
          SaveDiagrammedDepartureTime := TrainJourney_DiagrammedDepartureTime;
          StartOfRepeatJourney := False;
          AppendStringArray2ToStringArray1(SaveRouteArray, TrainJourney_RouteArray);
        END;
      END; {WITH}

      WITH Train_JourneysArray[Journey] DO BEGIN
        TrainJourney_LocationsPending := False;

        IF StartLocation = UnknownLocation THEN BEGIN
          TrainJourney_LocationsPending := True;
          { we will check the journey again later once we know where the missing locations are }
          Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': start location (at ' + AreaToStr(StartArea, ShortStringType) + ') noted as pending');
        END;
        IF EndLocation = UnknownLocation THEN BEGIN
          TrainJourney_LocationsPending := True;
          { we will check the journey again later once we know where the missing locations are }
          Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': end location (at ' + AreaToStr(EndArea, ShortStringType) + ') noted as pending');
        END;

        { Only record the diagrammed start and end locations at the beginning of the operation - they may have been explicitly stated in the diagram, or may have been
          allocated later - but they are what will be in the original diagram.
        }
        IF AreDiagramsLoading THEN BEGIN
          TrainJourney_DiagrammedStartLocation := StartLocation;
          TrainJourney_DiagrammedEndLocation := EndLocation;
        END;

        { This is used to force additional stopping time at stations }
        TrainJourney_AdditionalRequiredStationWaitInMinutes := 0;

        IF (Journey = UnknownJourney) OR NOT TrainJourney_Created THEN
          { we mustn't recreate journeys whose details are merely being revised, or we end up with duplicate journeys }
          TrainJourney_Created := False;

        TrainJourney_SetUp := False;
        TrainJourney_Cleared := False;

        TrainJourney_CurrentDepartureTime := CurrentDepartureTime;
        TrainJourney_DiagrammedDepartureTime := DiagrammedDepartureTime;
        TrainJourney_CurrentArrivalTime := CurrentArrivalTime;

        TrainJourney_Direction := TrainDirection;

        TrainJourney_Route := UnknownRoute;

        TrainJourney_NotForPublicUse := NotForPublicUse;
        TrainJourney_UserToDrive := False;

        TrainJourney_StartArea := StartArea;
        TrainJourney_EndArea := EndArea;

        TrainJourney_StartLocation := StartLocation;
        TrainJourney_EndLocation := EndLocation;

        TrainJourney_StartOfRepeatJourney := StartOfRepeatJourney;
        TrainJourney_StoppingOnArrival := StoppingOnArrival;

        IF StartLine <> UnknownLine THEN
          TrainJourney_StartLine := StartLine
        ELSE
          IF StartLocation = UnknownLocation THEN
            TrainJourney_StartLine := UnknownLine
          ELSE BEGIN
            IF TrainDirection = Up THEN BEGIN
              TrainJourney_StartLine := Locations[StartLocation].Location_LineAtUp;
              IF TrainJourney_StartLine =  UnknownLine THEN BEGIN
                OK := False;
                Log(Train_LocoChipStr + ' XG J=' + IntToStr(Journey)
                                      + ': unknown start line as start location ' + LocationToStr(StartLocation) + ' does not have a known line at up');
              END;
            END ELSE BEGIN
              TrainJourney_StartLine := Locations[StartLocation].Location_LineAtDown;
              IF TrainJourney_StartLine =  UnknownLine THEN BEGIN
                OK := False;
                Log(Train_LocoChipStr + ' XJ=' + IntToStr(Journey)
                                      + ': unknown start line as start location ' + LocationToStr(StartLocation) + ' does not have a known line at down');
              END;
            END;
          END;

        IF OK THEN BEGIN
          IF EndLine <> UnknownLine THEN
            TrainJourney_EndLine := EndLine
          ELSE
            IF EndLocation = UnknownLocation THEN
              TrainJourney_EndLine := UnknownLine
            ELSE
              IF TrainDirection = Up THEN
                TrainJourney_EndLine := Locations[EndLocation].Location_LineAtUp
              ELSE
                TrainJourney_EndLine := Locations[EndLocation].Location_LineAtDown;

          { So we can set the signal's hidden aspect in due course - to make sure the train stops even if the signal is off }
          IF TrainJourney_StartLine = UnknownLine THEN
            TrainJourney_StartSignal := UnknownSignal
          ELSE
            TrainJourney_StartSignal := GetLineAdjacentSignal(TrainJourney_StartLine);

          IF TrainJourney_EndLine = UnknownLine THEN BEGIN
            TrainJourney_EndSignal := UnknownSignal;
            TrainJourney_EndBufferStop := UnknownBufferStop;
          END ELSE BEGIN
            TrainJourney_EndSignal := GetLineAdjacentSignal(TrainJourney_EndLine);
            TrainJourney_EndBufferStop := Lines[TrainJourney_EndLine].Line_AdjacentBufferStop;
          END;

          IF TrainJourney_LocationsPending THEN
            Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': journey partially created: ' + DescribeJourney(T, Journey));

          IF RebuildRouteArray THEN BEGIN
            IF (SaveStartLocation = TrainJourney_StartLocation)
            AND (SaveEndLocation = TrainJourney_EndLocation)
            THEN BEGIN
              Log(Train_LocoChipStr + ' X Route rebuilding not required: '
                                    + 'Start Location = ' + LocationToStr(SaveStartLocation) + ' to ' + 'End location = ' + LocationToStr(SaveEndLocation));
            END ELSE BEGIN
              Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': journey created, rebuilding the route array: '
                                    + DescribeJourney(T, Journey) + ' {WRAP=SCREENWIDTH}');

              SetLength(JourneyRouteArray, 0);

              FindRouteFromLineAToLineB(Train_LocoChip, Journey, UnknownSignal, TrainJourney_StartLine, TrainJourney_EndLine, TrainDirection, Train_Type,
                                        Train_CurrentLengthInInches, RouteingInEmergency, NOT IncludeOutOfUseLines, JourneyRouteArray, LinesNotAvailableStr, ErrorMsg, OK);
              IF NOT OK
              AND (RouteingInEmergency = False)
              THEN BEGIN
                { try emergency routeing }
                FindRouteFromLineAToLineB(Train_LocoChip, Journey, UnknownSignal, TrainJourney_StartLine, TrainJourney_EndLine, TrainDirection, Train_Type,
                                          Train_CurrentLengthInInches, EmergencyRouteing, NOT IncludeOutOfUseLines, JourneyRouteArray, LinesNotAvailableStr, ErrorMsg, OK);
              END;

              IF NOT OK THEN
                ErrorMsg := 'No route ' + DirectionToStr(TrainDirection) + ' found between start location' + ' "' + LocationToStr(StartLocation)
                            + '" and end location "' + LocationToStr(EndLocation) + '" in CreateJourney (' + ErrorMsg + ')'
              ELSE BEGIN
                WriteStringArrayToLog(Train_LocoChip, 'R', 'J=' + IntToStr(Journey) + ': '
                                                           + 'Journey Draft Route Array to set up'
                                                           + ' from ' + LineToStr(TrainJourney_StartLine)
                                                           + ' to ' + LineToStr(TrainJourney_EndLine)
                                                           + ': ', JourneyRouteArray, 2, 190, 'SR=');

                { See if any part of the journey is set for manual operation }
                JourneyRouteArrayPos := 0;
                JourneyRouteArrayUserMustDriveTCFound := False;
                WHILE (JourneyRouteArrayPos < High(JourneyRouteArray))
                AND NOT JourneyRouteArrayUserMustDriveTCFound
                DO BEGIN
                  L := ExtractLineFromString(JourneyRouteArray[JourneyRouteArrayPos]);
                  IF L <> UnknownLine THEN BEGIN
                    IF (Lines[L].Line_TC <> UnknownTC)
                    AND (TrackCircuits[Lines[L].Line_TC].TC_UserMustDrive)
                    THEN BEGIN
                      JourneyRouteArrayUserMustDriveTCFound := True;
                      Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': user set to drive as TC=' + IntToStr(Lines[L].Line_TC)
                                            + ' [' + LineToStr(L) + '] is marked as to ''User Must Drive''');
                      TrainJourney_UserToDrive := True;
                    END;
                  END;
                  Inc(JourneyRouteArrayPos);
                END; {WHILE}

                { Now locate the first trackcircuit in the array }
                I := 0;
                FirstLineFound := False;
                TrainJourney_FirstTC := UnknownTC;
                WHILE (I <= High(JourneyRouteArray))
                AND (TrainJourney_FirstTC = UnknownTC)
                DO BEGIN
                  NextLine := ExtractLineFromString(JourneyRouteArray[I]);
                  IF NextLine <> UnknownLine THEN BEGIN
                    IF FirstLineFound THEN BEGIN
                      IF Lines[NextLine].Line_TC <> UnknownTC THEN
                        TrainJourney_FirstTC := Lines[NextLine].Line_TC;
                    END ELSE
                      IF Lines[NextLine].Line_TC <> UnknownTC THEN
                        { we want the next TC (second) on the route }
                        FirstLineFound := True;
                  END;
                  Inc(I);
                END; {WHILE}
                IF TrainJourney_FirstTC = UnknownTC THEN
                  Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': first TC not found');

              END;
              IF Length(JourneyRouteArray) = 0 THEN
                Debug('!' + LocoChipToStr(Train_LocoChip) + ' J=' + IntToStr(Journey) + ': new JourneyRouteArray is empty');
            END;
          END;

          IF NewJourney THEN BEGIN
            IF Length(JourneyRouteArray) > 0 THEN BEGIN
              { set up a supplied route array }
              SetLength(TrainJourney_RouteArray, 0);
              AppendStringArray2ToStringArray1(TrainJourney_RouteArray, JourneyRouteArray);
            END;

            Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': creating journey data:');
            IF TrainJourney_StartArea <> SaveStartArea THEN
              Log('D ' + StringOfChar(' ', 5) + ': start area is ' + AreaToStr(TrainJourney_StartArea, ShortStringType));
            IF TrainJourney_EndArea <> SaveEndArea THEN
              Log('D ' + StringOfChar(' ', 5) + ': end area is ' + AreaToStr(TrainJourney_EndArea, ShortStringType));
            IF TrainJourney_StartLocation <> SaveStartLocation THEN
              Log('D ' + StringOfChar(' ', 5) + ': start location is ' + LocationToStr(TrainJourney_StartLocation, ShortStringType));
            IF TrainJourney_EndLocation <> SaveEndLocation THEN
              Log('D ' + StringOfChar(' ', 5) + ': end location is ' + LocationToStr(TrainJourney_EndLocation, ShortStringType));
            IF TrainJourney_DiagrammedStartLocation <> SaveDiagrammedStartLocation THEN
              Log('D ' + StringOfChar(' ', 5) + ': diagrammed start location is ' + LocationToStr(TrainJourney_DiagrammedStartLocation, ShortStringType));
            IF TrainJourney_DiagrammedEndLocation <> SaveDiagrammedEndLocation THEN
              Log('D ' + StringOfChar(' ', 5) + ': diagrammed end location is ' + LocationToStr(TrainJourney_DiagrammedEndLocation, ShortStringType));
            IF TrainJourney_StartLine <> SaveStartLine THEN
              Log('D ' + StringOfChar(' ', 5) + ': start line is ' + LineToStr(TrainJourney_StartLine));
            IF TrainJourney_EndLine <> SaveEndLine THEN
              Log('D ' + StringOfChar(' ', 5) + ': end line is ' + LineToStr(TrainJourney_EndLine));
            IF TrainJourney_CurrentDepartureTime <> SaveCurrentDepartureTime THEN
              Log('D ' + StringOfChar(' ', 5) + ': current departure time is ' + TimeToHMStr(TrainJourney_CurrentDepartureTime));
            IF TrainJourney_DiagrammedDepartureTime <> SaveDiagrammedDepartureTime THEN
              Log('D ' + StringOfChar(' ', 5) + ': diagrammed departure time is ' + TimeToHMStr(TrainJourney_DiagrammedDepartureTime));
            IF TrainJourney_CurrentArrivalTime <> SaveCurrentArrivalTime THEN
              Log('D ' + StringOfChar(' ', 5) + ': current arrival time is ' + TimeToHMStr(TrainJourney_CurrentArrivalTime));
            IF TrainJourney_StoppingOnArrival <> SaveStoppingOnArrival THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing StoppingOnArrival to ' + BoolToStr(SaveStoppingOnArrival, True));
            IF TrainJourney_NotForPublicUse <> SaveNotForPublicUse THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing NotForPublicUse to ' + BoolToStr(SaveNotForPublicUse, True));
            IF TrainJourney_StartOfRepeatJourney <> SaveStartOfRepeatJourney THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing StartOfRepeatJourney to ' + BoolToStr(SaveStartOfRepeatJourney, True));

            WriteStringArrayToLog(Train_LocoChip, 'R', 'J=' + IntToStr(Journey)
                                                       + ': journey route array is',
                                                       TrainJourney_RouteArray,
                                                       7, 190, 'SR=');
          END ELSE BEGIN
            { Revising the journey }
            Log(Train_LocoChipStr + ' D J=' + IntToStr(Journey) + ': revising journey data:');

            IF TrainJourney_StartArea <> SaveStartArea THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing start area from ' + AreaToStr(SaveStartArea, ShortStringType)
                                              + ' to ' + AreaToStr(TrainJourney_StartArea, ShortStringType));
            IF TrainJourney_EndArea <> SaveEndArea THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing end area from ' + AreaToStr(SaveEndArea, ShortStringType)
                                              + ' to ' + AreaToStr(TrainJourney_EndArea, ShortStringType));
            IF TrainJourney_StartLocation <> SaveStartLocation THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing start location from ' + LocationToStr(SaveStartLocation, ShortStringType)
                                              + ' to ' + LocationToStr(TrainJourney_StartLocation, ShortStringType));
            IF TrainJourney_EndLocation <> SaveEndLocation THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing end location from ' + LocationToStr(SaveEndLocation, ShortStringType)
                                              + ' to ' + LocationToStr(TrainJourney_EndLocation, ShortStringType));
            IF TrainJourney_DiagrammedStartLocation <> SaveDiagrammedStartLocation THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing diagrammed start location from ' + LocationToStr(SaveDiagrammedStartLocation, ShortStringType)
                                              + ' to ' + LocationToStr(TrainJourney_DiagrammedStartLocation, ShortStringType));
            IF TrainJourney_DiagrammedEndLocation <> SaveDiagrammedEndLocation THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing diagrammed end location from ' + LocationToStr(SaveDiagrammedEndLocation, ShortStringType)
                                              + ' to ' + LocationToStr(TrainJourney_DiagrammedEndLocation, ShortStringType));
            IF TrainJourney_StartLine <> SaveStartLine THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing start line from ' + LineToStr(SaveStartLine) + ' to ' + LineToStr(TrainJourney_StartLine));
            IF TrainJourney_EndLine <> SaveEndLine THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing end line from ' + LineToStr(SaveEndLine) + ' to ' + LineToStr(TrainJourney_EndLine));
            IF TrainJourney_CurrentDepartureTime <> SaveCurrentDepartureTime THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing departure time from ' + TimeToHMStr(SaveCurrentDepartureTime)
                                              + ' to ' + TimeToHMStr(TrainJourney_CurrentDepartureTime));
            IF TrainJourney_DiagrammedDepartureTime <> SaveDiagrammedDepartureTime THEN
              Log('D ' + StringOfChar(' ', 5) + ': diagrammed departure time is '+ TimeToHMStr(TrainJourney_DiagrammedDepartureTime));
            IF TrainJourney_CurrentArrivalTime <> SaveCurrentArrivalTime THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing end time from ' + TimeToHMStr(SaveCurrentArrivalTime) + ' to ' + TimeToHMStr(TrainJourney_CurrentArrivalTime));
            IF TrainJourney_StoppingOnArrival <> SaveStoppingOnArrival THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing StoppingOnArrival to ' + BoolToStr(SaveStoppingOnArrival, True));
            IF TrainJourney_NotForPublicUse <> SaveNotForPublicUse THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing NotForPublicUse to ' + BoolToStr(SaveNotForPublicUse, True));
            IF TrainJourney_StartOfRepeatJourney <> SaveStartOfRepeatJourney THEN
              Log('D ' + StringOfChar(' ', 5) + ': changing StartOfRepeatJourney to ' + BoolToStr(SaveStartOfRepeatJourney, True));

            IF Length(JourneyRouteArray) > 0 THEN BEGIN
              IF NOT StringArraysCompareOK(TrainJourney_RouteArray, JourneyRouteArray, TempStr) THEN BEGIN
                { set up the revised route array }
                WriteStringArrayToLog(NoLocoChip, 'R', StringOfChar(' ', 5) + ': changing journey route array from',
                                                       TrainJourney_RouteArray,
                                                       5, 190, 'SR=');
                WriteStringArrayToLog(NoLocoChip, 'R', StringOfChar(' ', 5) + ': to ',
                                                       JourneyRouteArray,
                                                       5, 190, 'SR=');
                SetLength(TrainJourney_RouteArray, 0);
                AppendStringArray2ToStringArray1(TrainJourney_RouteArray, JourneyRouteArray);
              END;
            END;
          END;
        END;
      END; {WITH}
    END; {WITH}
  EXCEPT
    ON E : Exception DO
      Log('EG CreateJourney: ' + E.ClassName +' error raised, with message: ' + E.Message);
  END; {TRY}
END; { CreateJourney }

PROCEDURE WriteTrainRecord(T : Train);
{ Writes the main train record out }
VAR
  I : Integer;
  TempStr : String;

BEGIN
  WITH T^ DO BEGIN
    Log('Train_LocoChip=' + LocoChipToStr(Train_LocoChip));
    Log('Train_DoubleHeaderLocoChip=' + LocoChipToStr(Train_DoubleHeaderLocoChip));

    Log('Train_Accelerating=' + BoolToStr(Train_Accelerating, True));
    Log('Train_AccelerationAdjustRange=' + IntToStr(Train_AccelerationAdjustRange));
    Log('Train_AccelerationStartTime=' + TimeToHMStr(Train_AccelerationStartTime));
    Log('Train_AccelerationStr=' + Train_AccelerationStr);
    Log('Train_AccelerationTimeInSeconds=' + FloatToStr(Train_AccelerationTimeInSeconds));
    Log('Train_AccelerationTimeInterval=' + FloatToStr(Train_AccelerationTimeInterval));
    Log('Train_AtCurrentBufferStop=' + IntToStr(Train_AtCurrentBufferStop));
    Log('Train_AtCurrentSignal=' + IntToStr(Train_AtCurrentSignal));
    Log('Train_AtHiddenAspectSignal=' + IntToStr(Train_AtHiddenAspectSignal));
    Log('Train_BeingAdvanced=' + BoolToStr(Train_BeingAdvanced, True));
    Log('Train_BeingAdvancedTC=' + IntToStr(Train_BeingAdvancedTC));
    Log('Train_ControlledByProgram=' + BoolToStr(Train_ControlledByProgram, True));
    Log('Train_ControlledByRDC=' + BoolToStr(Train_ControlledByRDC, True));
    Log('Train_CurrentArrivalTime=' + TimeToHMStr(Train_CurrentArrivalTime));
    Log('Train_CurrentBufferStop=' + IntToStr(Train_CurrentBufferStop));
    Log('Train_CurrentDirection=' + DirectionToStr(Train_CurrentDirection));
    Log('Train_CurrentJourney=' + IntToStr(Train_CurrentJourney));
    Log('Train_CurrentLengthInInches=' + IntToStr(Train_CurrentLengthInInches));
    Log('Train_CurrentLenzSpeed=' + IntToStr(Train_CurrentLenzSpeed));
    Log('Train_CurrentRoute=' + IntToStr(Train_CurrentRoute));
    Log('Train_CurrentSignal=' + IntToStr(Train_CurrentSignal));
    Log('Train_CurrentSourceLocation=' + LocationToStr(Train_CurrentSourceLocation, ShortStringType));
    Log('Train_CurrentSpeedInMPH=' + MPHToStr(Train_CurrentSpeedInMPH));
    Log('Train_CurrentStatus=' + TrainStatusToStr(Train_CurrentStatus));
    Log('Train_CurrentTC=' + IntToStr(Train_CurrentTC));
    Log('Train_Decelerating=' + BoolToStr(Train_Decelerating, True));
    Log('Train_Description=' + Train_Description);
    Log('Train_DesiredLenzSpeed=' + IntToStr(Train_DesiredLenzSpeed));
    Log('Train_SaveDesiredLenzSpeed=' + IntToStr(Train_SaveDesiredLenzSpeed));
    Log('Train_DesiredSpeedInMPH=' + MPHToStr(Train_DesiredSpeedInMPH));
    Log('Train_DistanceToCurrentSignalOrBufferStop=' + FloatToStr(Train_DistanceToCurrentSignalOrBufferStop));
    Log('Train_DistanceToNextSignalOrBufferStop=' + FloatToStr(Train_DistanceToNextSignalOrBufferStop));
    Log('Train_DistanceToNextSignalButOneOrBufferStop=' + FloatToStr(Train_DistanceToNextSignalButOneOrBufferStop));
    Log('Train_EmergencyRouteing=' + BoolToStr(Train_EmergencyRouteing, True));
    Log('Train_ExtraPowerAdjustment=' + IntToStr(Train_ExtraPowerAdjustment));

    TempStr := '';
    FOR I := 0 TO High(Train_Functions) DO
      TempStr := TempStr + BoolToStr(Train_Functions[I], True) + ', ';
    Log('Train_Functions=' + TempStr);

    Log('Train_Functions0To4Byte=' +  IntToStr(Train_Functions0To4Byte));
    Log('Train_Functions5To12Byte=' +  IntToStr(Train_Functions5To12Byte));
    Log('Train_GradientSpeedAdjustment=' + IntToStr(Train_GradientSpeedAdjustment));
    Log('Train_GradientSpeedAdjustmentMsgWritten=' + BoolToStr(Train_GradientSpeedAdjustmentMsgWritten, True));
    Log('Train_Headcode=' + '''' + Train_Headcode + '''');

    TempStr := '';
    FOR I := 1 TO Length(Train_InitialTrackCircuits) DO
      TempStr := TempStr + IntToStr(Train_InitialTrackCircuits[I]) + ', ';
    Log('Train_InitialTrackCircuits=' + TempStr);

    Log('Train_InLightsOnTime=' + BoolToStr(Train_InLightsOnTime, True));
    Log('Train_TotalJourneys [starts at 0]=' + IntToStr(Train_TotalJourneys));
    FOR I := 1 TO 9 DO
      Log('Train_RouteCreationHeldMsgWrittenArray[' + IntToStr(I) + ']=' + BoolToStr(Train_RouteCreationHeldMsgWrittenArray[I], True));
    Log('Train_RouteCreationPlatformHeldStr=' + Train_RouteCreationPlatformHeldStr);
    Log('Train_JourneysArray');
    Log('Train_LastLocation=' + LocationToStr(Train_LastLocation));
    Log('Train_LastTC=' + IntToStr(Train_LastTC));
    Log('Train_LastRouteLockedMsgStr=' + Train_LastRouteLockedMsgStr);
    Log('Train_LastSignal=' + IntToStr(Train_LastSignal));
    Log('Train_LightingChipDown=' + IntToStr(Train_LightingChipDown));
    Log('Train_LightingChipDownAddress');
    Log('Train_LightingChipUp=' +  IntToStr(Train_LightingChipUp));
    Log('Train_LightingChipUpAddress');
    Log('Train_LightsMsg=' + Train_LightsMsg);
    Log('Train_LightsOn=' + BoolToStr(Train_LightsOn, True));
    Log('Train_LightsOnTime=' + TimeToHMStr(Train_LightsOnTime));
    Log('Train_LightsRemainOnWhenJourneysComplete=' + BoolToStr(Train_LightsRemainOnWhenJourneysComplete, True));
    Log('Train_LightsType=' + LightsTypeToStr(Train_LightsType));
    Log('Train_LocatedAtStartup=' + BoolToStr(Train_LocatedAtStartup, True));

    TempStr := '';
    FOR I := 0 TO High(Train_Locations) DO BEGIN
      IF LocationToStr(Train_Locations[I], ShortStringType) = UnknownLocationStr THEN
        TempStr := TempStr + '-, '
      ELSE
        TempStr := TempStr + LocationToStr(Train_Locations[I], ShortStringType) + ', ';
    END;
    Log('Train_Locations=' + TempStr);

    Log('Train_DiagramFound=' + BoolToStr(Train_DiagramFound, True));
    Log('Train_LocoChipStr=' + Train_LocoChipStr);
    Log('Train_LocoClassStr=' + Train_LocoClassStr);
    Log('Train_LocoName=' + Train_LocoName);
    Log('Train_MaximumSpeedInMPH=' + MPHToStr(Train_MaximumSpeedInMPH));
    Log('Train_MinimumAccelerationTimeInSeconds=' + IntToStr(Train_MinimumAccelerationTimeInSeconds));
    Log('Train_MissingMessage=' + BoolToStr(Train_MissingMessage, True));
    Log('Train_MissingNum=' + IntToStr(Train_MissingNum));
    Log('Train_NextTC=' + IntToStr(Train_NextTC));
    Log('Train_NextButOneTC=' + IntToStr(Train_NextButOneTC));
    Log('Train_NotInPlaceMsgWritten=' + BoolToStr(Train_NotInPlaceMsgWritten, True));
    Log('Train_NotLocatedAtStartupMsgWritten=' + BoolToStr(Train_NotLocatedAtStartupMsgWritten, True));
    Log('Train_PreviouslyControlledByProgram=' + BoolToStr(Train_PreviouslyControlledByProgram, True));
    Log('Train_PreviousStatus=' + TrainStatusToStr(Train_PreviousStatus));
    Log('Train_PreviousTC=' + IntToStr(Train_PreviousTC));
    Log('Train_Reversing=' + BoolToStr(Train_Reversing, True));
    Log('Train_ReversingDelayInSeconds=' + IntToStr(Train_ReversingDelayInSeconds));
    Log('Train_ReversingStartTime=' + TimeToHMStr(Train_ReversingStartTime));
    Log('Train_ReversingWaitStarted=' + BoolToStr(Train_ReversingWaitStarted, True));
    Log('Train_RouteCreationHeldJourney=' + IntToStr(Train_RouteCreationHeldJourney));
    Log('Train_RouteCheckedTime=' + TimeToHMSStr(Train_RouteCheckedTime));
    Log('Train_RouteingHeldAtSignal=' + IntToStr(Train_RouteingHeldAtSignal));
    Log('Train_SaveCurrentTC=' + IntToStr(Train_SaveCurrentTC));
    Log('Train_SaveDesiredLenzSpeed=' + IntToStr(Train_SaveDesiredLenzSpeed));
    Log('Train_SavedLocation=' + LocationToStr(Train_SavedLocation));
    Log('Train_SavedRoute=' + IntToStr(Train_SavedRoute));
    Log('Train_SaveSpeedInFiddleyardMsg=' + Train_SaveSpeedInFiddleyardMsg);
    Log('Train_SaveTCsClearedStr=' + '''' + Train_SaveTCsClearedStr + '''');
    Log('Train_SaveTCsForReleaseStr=' + '''' + Train_SaveTCsForReleaseStr + '''');
    Log('Train_SaveTCsOccupiedStr,' + ''''+ Train_SaveTCsOccupiedStr + '''');
    Log('Train_SectionStartTime=' + TimeToHMSStr(Train_SectionStartTime));
    Log('Train_Speed10=' + IntToStr(Train_Speed10));
    Log('Train_Speed20=' + IntToStr(Train_Speed20));
    Log('Train_Speed30=' + IntToStr(Train_Speed30));
    Log('Train_Speed40=' + IntToStr(Train_Speed40));
    Log('Train_Speed50=' + IntToStr(Train_Speed50));
    Log('Train_Speed60=' + IntToStr(Train_Speed60));
    Log('Train_Speed70=' + IntToStr(Train_Speed70));
    Log('Train_Speed80=' + IntToStr(Train_Speed80));
    Log('Train_Speed90=' + IntToStr(Train_Speed90));
    Log('Train_Speed100=' + IntToStr(Train_Speed100));
    Log('Train_Speed110=' + IntToStr(Train_Speed110));
    Log('Train_Speed120=' + IntToStr(Train_Speed120));

    TempStr := '';
    FOR I := 1 TO 12 DO
        TempStr := TempStr + IntToStr(Train_SpeedArray[I]) + ', ';
    Log('Train_SpeedArray=' + TempStr);

    Log('Train_SpeedByte=' + IntToStr(Train_SpeedByte));
    Log('Train_SpeedSettingsMissing=' + BoolToStr(Train_SpeedSettingsMissing, True));
    Log('Train_SpeedStepMode=' + IntToStr(Train_SpeedStepMode));
    Log('Train_SpeedString=' + '''' + Train_SpeedString + '''');
    Log('Train_StalledMsgWritten=' + BoolToStr(Train_StalledMsgWritten, True));
    Log('Train_TakenOverByUserMsgWritten=' + BoolToStr(Train_TakenOverByUserMsgWritten, True));

    TempStr := '';
    FOR I := 0 TO High(Train_TCsAndSignalsNotClearedArray) DO
      TempStr := TempStr + Train_TCsAndSignalsNotClearedArray[I] + ', ';
    Log('Train_TCsAndSignalsNotClearedArray=' + TempStr);

    Log('Train_TCsAndSignalsNotClearedStr=' + '''' + Train_TCsAndSignalsNotClearedStr + '''');

    TempStr := '';
    FOR I := 0 TO High(Train_TCsNotClearedArray) DO
      TempStr := TempStr + Train_TCsNotClearedArray[I] + ', ';
    Log('Train_TCsNotClearedArray=' + TempStr);

    Log('Train_TCsNotClearedStr: String=' + '''' + Train_TCsNotClearedStr + '''');

    TempStr := '';
    FOR I := 0 TO High(Train_TCsOccupiedOrClearedArray) DO
      TempStr := TempStr + Train_TCsOccupiedOrClearedArray[I] + ', ';
    Log('Train_TCsOccupiedOrClearedArray=' + TempStr);

    Log('Train_TCsOccupiedOrClearedStr=' + '''' + Train_TCsOccupiedOrClearedStr + '''');

    TempStr := '';
    FOR I := 0 TO High(Train_TCsReleasedArray) DO
      TempStr := TempStr + Train_TCsReleasedArray[I] + ', ';
    Log('Train_TCsReleasedArray=' + TempStr);

    Log('Train_TCsReleasedStr=' + '''' + Train_TCsReleasedStr);

    TempStr := '';
    FOR I := 0 TO High(Train_TempDraftRouteArray) DO
      TempStr := TempStr + Train_TempDraftRouteArray[I] + ', ';
    Log('Train_TempDraftRouteArray=' + TempStr);

    TempStr := '';
    FOR I := 0 TO High(Train_TempLockingArray) DO
      TempStr := TempStr + Train_TempLockingArray[I] + ', ';
    Log('Train_TempLockingArray=' + TempStr);

    Log('Train_TerminatingSpeedReductionMsgWritten=' + BoolToStr(Train_TerminatingSpeedReductionMsgWritten, True));

    TempStr := '';
    FOR I := 0 TO High(Train_DiagramsGridRowNums) DO
      TempStr := TempStr + IntToStr(Train_DiagramsGridRowNums[I]) + ', ';
    Log('Train_DiagramsGridRowNums=' + TempStr);

    Log('Train_Type=' + TrainTypeNumToStr(TrainTypeToTrainTypeNum(Train_Type)));
    Log('Train_TypeNum=' + IntToStr(Train_TypeNum));
    Log('Train_UserDriving=' + BoolToStr(Train_UserDriving, True));
    Log('Train_UserPowerAdjustment=' + IntToStr(Train_UserPowerAdjustment));
    Log('Train_UserRequiresInstruction=' + BoolToStr(Train_UserRequiresInstructions, True));
    Log('Train_UserRequiresInstructionMsg=' + Train_UserRequiresInstructionMsg);
    Log('Train_UseTrailingTrackCircuits= ' + BoolToStr(Train_UseTrailingTrackCircuits, True));
    Log('Train_WaitingForHiddenAspectStartTime=' + TimeToHMSStr(Train_WaitingForHiddenAspectStartTime));
    Log('Train_NextRecord');
  END; {WITH}

  Debug('train record written to log file');
END; { WriteTrainRecord }

PROCEDURE WriteTrainJourneysRecordToLogFile(T : Train; FullRecord : Boolean);
{ Writes the journey record out }
VAR
  I : Integer;

BEGIN
  WITH T^ DO BEGIN
    FOR I := 0 TO High(Train_JourneysArray) DO BEGIN
      WITH Train_JourneysArray[I] DO BEGIN
        Log(' {NOUNITREF}');
        Log(LocoChipToStr(Train_LocoChip) + ' {NOUNITREF}');
        Log('TrainJourney=' + IntToStr(I) + ' {NOUNITREF}');
        WriteStringArrayToLog(NoLocoChip, '*', 'TrainJourney_RouteArray', TrainJourney_RouteArray);
        WriteStringArrayToLog(NoLocoChip, '*', 'TrainJourney_LockingArray', TrainJourney_LockingArray);

        IF FullRecord THEN BEGIN
          Log('TrainJourney_Cleared=' + BoolToStr(TrainJourney_Cleared, True)+ ' {NOUNITREF}');
          Log('TrainJourney_Created=' + BoolToStr(TrainJourney_Created, True) + ' {NOUNITREF}');
          Log('TrainJourney_Direction=' + DirectionToStr(TrainJourney_Direction) + ' {NOUNITREF}');
          Log('TrainJourney_EndLine=' + Lines[TrainJourney_EndLine].Line_Str + ' {NOUNITREF}');
          Log('TrainJourney_EndSignal=' + IntToStr(TrainJourney_EndSignal) + ' {NOUNITREF}');
          Log('TrainJourney_FirstTC=' + IntToStr(TrainJourney_FirstTC) + ' {NOUNITREF}');
          Log('TrainJourney_LengthInInches=' + FloatToStr(TrainJourney_LengthInInches) + ' {NOUNITREF}');
          Log('TrainJourney_LocationsPending=' + BoolToStr(TrainJourney_LocationsPending, True) + ' {NOUNITREF}');
          Log('TrainJourney_NotForPublicUse=' + BoolToStr(TrainJourney_NotForPublicUse, True) + ' {NOUNITREF}');
          Log('TrainJourney_Route=' + IntToStr(TrainJourney_Route) + ' {NOUNITREF}');
          Log('TrainJourney_SetUp=' + BoolToStr(TrainJourney_SetUp, True) + ' {NOUNITREF}');
          Log('TrainJourney_StartLine=' + Lines[TrainJourney_StartLine].Line_Str + ' {NOUNITREF}');
          Log('TrainJourney_StartOfRepeatJourney=' + BoolToStr(TrainJourney_StartOfRepeatJourney, True) + ' {NOUNITREF}');
          Log('TrainJourney_StoppingOnArrival=' + BoolToStr(TrainJourney_StoppingOnArrival, True) + ' {NOUNITREF}');
        END;

        Log('TrainJourney_StartArea=' + AreaToStr(TrainJourney_StartArea) + ' {NOUNITREF}');
        Log('TrainJourney_StartLocation=' + LocationToStr(TrainJourney_StartLocation) + ' {NOUNITREF}');
        Log('TrainJourney_CurrentDepartureTime=' + TimeToHMSStr(TrainJourney_CurrentDepartureTime) + ' {NOUNITREF}');
        Log('TrainJourney_DiagrammedDepartureTime=' + TimeToHMSStr(TrainJourney_DiagrammedDepartureTime) + ' {NOUNITREF}');
        Log('TrainJourney_ActualDepartureTime=' + TimeToHMSStr(TrainJourney_ActualDepartureTime) + ' {NOUNITREF}');
        Log('---------------' + ' {NOUNITREF}');
        Log('TrainJourney_EndArea=' + AreaToStr(TrainJourney_EndArea) + ' {NOUNITREF}');
        Log('TrainJourney_EndLocation=' + LocationToStr(TrainJourney_EndLocation) + ' {NOUNITREF}');
        Log('TrainJourney_CurrentArrivalTime=' + TimeToHMSStr(TrainJourney_CurrentArrivalTime) + ' {NOUNITREF}');
        Log('TrainJourney_ActualArrivalTime=' + TimeToHMSStr(TrainJourney_ActualArrivalTime) + ' {NOUNITREF}');
      END; {WITH}
    END; {FOR}
  END; {WITH}

  Debug('train journeys record written to log file');
END; { WriteTrainJourneysRecordToLogFile }

PROCEDURE WriteTrainJourneysRecordToLockListWindow(T : Train; FullRecord : Boolean);
{ Writes the journey record to the locklist window }
VAR
  I : Integer;

BEGIN
  LockListWindow.Caption := 'Journeys Info';
  LockListWindow.LockListWindowMemo.Clear;
  LockListWindow.LockListWindowMemo.Lines.Clear;
  LockListWindow.Visible := True;
  Log('A Train Journeys displayed');
  WITH T^ DO BEGIN
    FOR I := 0 TO High(Train_JourneysArray) DO BEGIN
      WITH Train_JourneysArray[I] DO BEGIN
        WITH LockListWindow.LockListWindowMemo.Lines DO BEGIN
          BeginUpdate;
          Add(LocoChipToStr(Train_LocoChip) + ' J=' + IntToStr(I));
          IF FullRecord THEN BEGIN
            Add('TrainJourney_Cleared=' + BoolToStr(TrainJourney_Cleared, True));
            Add('TrainJourney_Created=' + BoolToStr(TrainJourney_Created, True));
            Add('TrainJourney_Direction=' + DirectionToStr(TrainJourney_Direction));
            Add('TrainJourney_EndLine=' + Lines[TrainJourney_EndLine].Line_Str);
            Add('TrainJourney_EndSignal=' + IntToStr(TrainJourney_EndSignal));
            Add('TrainJourney_FirstTC=' + IntToStr(TrainJourney_FirstTC));
            Add('TrainJourney_LengthInInches=' + FloatToStr(TrainJourney_LengthInInches));
            Add('TrainJourney_LocationsPending=' + BoolToStr(TrainJourney_LocationsPending, True));
            Add('TrainJourney_NotForPublicUse=' + BoolToStr(TrainJourney_NotForPublicUse, True));
            Add('TrainJourney_Route=' + IntToStr(TrainJourney_Route));
            Add('TrainJourney_SetUp=' + BoolToStr(TrainJourney_SetUp, True));
            Add('TrainJourney_StartLine=' + Lines[TrainJourney_StartLine].Line_Str);
            Add('TrainJourney_StartOfRepeatJourney=' + BoolToStr(TrainJourney_StartOfRepeatJourney, True));
            Add('TrainJourney_StoppingOnArrival=' + BoolToStr(TrainJourney_StoppingOnArrival, True));
          END;

          Add('TrainJourney_StartArea=' + AreaToStr(TrainJourney_StartArea));
          Add('TrainJourney_StartLocation=' + LocationToStr(TrainJourney_StartLocation));
          Add('TrainJourney_CurrentDepartureTime=' + TimeToHMSStr(TrainJourney_CurrentDepartureTime));
          Add('TrainJourney_DiagrammedDepartureTime=' + TimeToHMSStr(TrainJourney_DiagrammedDepartureTime));
          Add('TrainJourney_ActualDepartureTime=' + TimeToHMSStr(TrainJourney_ActualDepartureTime));
          Add('---------------');
          Add('TrainJourney_EndArea=' + AreaToStr(TrainJourney_EndArea));
          Add('TrainJourney_EndLocation=' + LocationToStr(TrainJourney_EndLocation));
          Add('TrainJourney_CurrentArrivalTime=' + TimeToHMSStr(TrainJourney_CurrentArrivalTime));
          Add('TrainJourney_ActualArrivalTime=' + TimeToHMSStr(TrainJourney_ActualArrivalTime));
          Add('');
          EndUpdate;
        END; {WITH}
      END; {WITH}
    END; {FOR}
  END; {WITH}
END; { WriteTrainJourneysRecordToLockListWindow }

FUNCTION GetStationNameFromArea(Area : Integer) : String;
{ Return a station name given the area it is in }
BEGIN
  IF Area <> UnknownArea THEN BEGIN
    IF TestingMode THEN
      Result := Areas[Area].Area_LongStr
    ELSE BEGIN
//      Result := Areas[Area].AreaStationNamesUp;

  //    CASE Area OF
  //      MainStationArea:
  //        Result := 'Durham'; { names need to go in .ini *** }
  //      BranchTerminusArea:
  //        Result := 'Lanchester';
  //      BranchMidStationArea:
  //        Result := 'Witton Gilbert';
  //      IslandStationArea:
  //        Result := 'Chester-le-Street';
  //      WindowStationArea:
  //        Result := 'Bishop Auckland';
  //      FiddleyardArea:
  //        Result := 'Fiddleyard';
  //      FiddleyardArea:
  //        CASE T^.Train_Type OF
  //          ExpressPassenger:
  //            IF T^.Train_CurrentDirection = Up THEN
  //              Result := 'Branch Express Up' //'London'
  //            ELSE
  //              Result := 'Branch Express Down'; //'Edinburgh';
  //          OrdinaryPassenger:
  //            IF T^.Train_CurrentDirection = Up THEN
  //              Result := 'Branch Passenger Up' //'Leeds'
  //            ELSE
  //              Result := 'Branch Passenger Down'; //'Newcastle';
  //        END; {CASE}
  //      ASidingsArea, BSidingsArea, CSidingsArea, DSidingsArea, ESidingsArea, FSidingsArea:
  //        Result := 'Sidings';
  //    ELSE
  //      Result := 'Unknown Destination';
  //    END; {CASE}
    END;
  END;
END; { GetStationNameFromArea }

FUNCTION CreateTrainDiagramsRecord(LocoChip, DoubleHeaderLocoChip, JourneyCount : Integer; UserSpecifiedDepartureTimesArray : DateTimeArrayType; LightsOnTime : TDateTime;
                                   EndLocationsStrArray : StringArrayType; DirectionsArray : DirectionArrayType; LightsRemainOn : Boolean; TrainNonMoving : Boolean;
                                   NotForPublicUseArray : BooleanArrayType; StartLocationStr : String; StoppingArray : BooleanArrayType; LengthOfTrainInCarriages : Integer;
                                   TypeOfTrainNum : Integer; UserDriving, UserRequiresInstructions, StartOfRepeatJourney : Boolean)
                                   : Train;
{ Creates a new train record for the diagram - NB the array parameters being passed are open array parameters and therefore start at
  zero
}
CONST
  DescribeFullTrainList = True;
  DiagramsLoading = True;
  IncludeOutOfUseLines = True;
  MaxTrainLength = 144; { should be put in .ini **** }
  NewJourney = True;
  RepeatJourney = True;
  RebuildRouteArray = True;
  UseEmergencyRouteing = True;

VAR
  DebugStr : String;
  DepartureTimesArray : DateTimeArrayType;
  DirectionFound : Boolean;
  DummyRouteArray : StringArrayType;
  ErrorMsg : String;
  FirstTrainInstance : Boolean;
  I : Integer;
  InputFoundOrCancelled : Boolean;
  JourneysArray : StringArrayType;
  LinesNotAvailableStr : String;
  LocoFound : Boolean;
  NewLocationStr : String;
  OK : Boolean;
  OtherT : Train;
  StartArea : Integer;
  T : Train;
  TempArea : Integer;
  TempDirection1, TempDirection2 : DirectionType;
  TempEndAreas : IntegerArrayType;
  TempEndLocations : IntegerArrayType;
  TempStartAreas : IntegerArrayType;
  TempStartLocations : IntegerArrayType;
  TempStr : String;
  TempT : Train;
  TempTCs : IntegerArrayType;
  TrainFound : Boolean;

  PROCEDURE AppendToAreaArray(VAR AreaArray : IntegerArrayType; NewElement : Integer);
  { Appends a given area value to the array }
  BEGIN
    SetLength(AreaArray, Length(AreaArray) + 1);
    AreaArray[High(AreaArray)] := NewElement;
  END; { AppendToAreaArray }

//  PROCEDURE SetUpTrainSpeeds;
//  { Use the speed settings from the loco database }
//  BEGIN
//    WITH T^ DO BEGIN
//      Train_Speed10 := Train_SpeedArray[1];
//      Train_Speed20 := Train_SpeedArray[2];
//      Train_Speed30 := Train_SpeedArray[3];
//      Train_Speed40 := Train_SpeedArray[4];
//      Train_Speed50 := Train_SpeedArray[5];
//      Train_Speed60 := Train_SpeedArray[6];
//      Train_Speed70 := Train_SpeedArray[7];
//      Train_Speed80 := Train_SpeedArray[8];
//      Train_Speed90 := Train_SpeedArray[9];
//      Train_Speed100 := Train_SpeedArray[10];
//      Train_Speed110 := Train_SpeedArray[11];
//      Train_Speed120 := Train_SpeedArray[12];
//    END; {WITH}
//  END; { SetUpTrainSpeeds }

  PROCEDURE DiagramsError(Str : String);
  { Process diagram errors }
  BEGIN
    { May want to mark the cancelled train as cancelled, or load the diagrams anyway }
    IF T^.Train_CurrentStatus = Cancelled THEN
      Log('DG Diagrams Error: ' + Str)
    ELSE BEGIN
      CASE MessageDialogueWithDefault('Diagrams error: ' + Str + ' for loco ' + T^.Train_LocoChipStr
                                      + CRLF
                                      + 'Do you want to cancel this train, mark it as being non-moving or ignore the problem?',
                                      StopTimer, mtError, [mbYes, mbNo, mbAbort], ['&Cancel', '&Non-Moving', '&Ignore'], mbYes)
      OF
        mrYes:
          CancelTrain(T, ByUser, NOT TrainExists);
        mrNo:
          BEGIN
            ChangeTrainStatus(T, NonMoving);
            { and enable the appropriate popup to allow it to be displayed }
            DiagramsWindow.PopupShowNonMovingTrains.Enabled := True;
            Log(T^.Train_LocoChipStr + ' D Train marked as non-moving by user');
            DrawDiagrams(UnitRef, 'DiagramsError');
          END;
        mrAbort:
          { do nothing! }
          ;
      END; {CASE}
    END;
  END; { DiagramsError }

BEGIN
  Log(LocoChipToStr(LocoChip) + ' D CREATING DIAGRAMS RECORD');
  Log('D DoubleHeaderLocoChip=' + IntToStr(DoubleHeaderLocoChip) + ' {INDENT=2} {NOUNITREF}');
  Log('D JourneyCount=' + IntToStr(JourneyCount) + ' {INDENT=2} {NOUNITREF}');

  Log('D StartLocationStr=' + StartLocationStr + ' {INDENT=2} {NOUNITREF}');
  FOR I := 0 TO High(EndLocationsStrArray) DO
    DebugStr := DebugStr + EndLocationsStrArray[I] + ';';
  Log('D EndLocationsStrArray=' + DebugStr + ' {INDENT=2} {NOUNITREF}');

  DebugStr := '';
  FOR I := 0 TO High(UserSpecifiedDepartureTimesArray) DO
    DebugStr := DebugStr + TimeToHMStr(UserSpecifiedDepartureTimesArray[I]) + ';';
  Log('D UserSpecifiedDepartureTimesArray=' + DebugStr + ' {INDENT=2} {NOUNITREF}');

  DebugStr := '';
  FOR I := 0 TO High(DirectionsArray) DO
    DebugStr := DebugStr + DirectionToStr(DirectionsArray[I]) + ';';
  Log('D DirectionsArray=' + DebugStr + ' {INDENT=2} {NOUNITREF}');

  DebugStr := '';
  FOR I := 0 TO High(StoppingArray) DO
    DebugStr := DebugStr + BoolToStr(StoppingArray[I], True) + ';';
  Log('D StoppingArray=' + DebugStr + ' {INDENT=2} {NOUNITREF}');

  Log('D LengthOfTrainInCarriages=' + IntToStr(LengthOfTrainInCarriages) + ' {INDENT=2} {NOUNITREF}');
  Log('D LightsOnTime=' + TimeToHMStr(LightsOnTime) + ' {INDENT=2} {NOUNITREF}');
  Log('D LightsRemainOn=' + BoolToStr(LightsRemainOn, True) + ' {INDENT=2} {NOUNITREF}');

  DebugStr := '';
  FOR I := 0 TO High(NotForPublicUseArray) DO
    DebugStr := DebugStr + BoolToStr(NotForPublicUseArray[I], True) + ';';
  Log('D NotForPublicUseArray=' + DebugStr + ' {INDENT=2} {NOUNITREF}');

  Log('D StartOfRepeatJourney=' + BoolToStr(StartOfRepeatJourney, True) + ' {INDENT=2} {NOUNITREF}');
  Log('D TrainNonMoving=' + BoolToStr(TrainNonMoving, True) + ' {INDENT=2} {NOUNITREF}');
  Log('D TypeOfTrainNum=' + IntToStr(TypeOfTrainNum) + ' {INDENT=2} {NOUNITREF}');
  Log('D UserDriving=' + BoolToStr(UserDriving, True) + ' {INDENT=2} {NOUNITREF}');
  Log('D UserRequiresInstruction=' + BoolToStr(UserRequiresInstructions, True) + ' {INDENT=2} {NOUNITREF}');

  FirstTrainInstance := True;
  TrainFound := False;

  IF LocoChip = UnknownLocoChip THEN BEGIN
    { a train with no loco attached }
    New(T);
    T^.Train_LocoChip := UnknownLocoChip;
    ChangeTrainStatus(T, NonMoving);
    AddTrainToTrainList(T, NOT DescribeFullTrainList);
  END ELSE BEGIN
    { Have we already created a record for this train? }
    T := TrainList;
    WHILE (T <> NIL)
    AND NOT TrainFound
    DO BEGIN
      WITH T^ DO BEGIN
        IF LocoChip = Train_LocoChip THEN BEGIN
          TrainFound := True;
          IF NOT Train_DiagramFound THEN
            Train_DiagramFound := True
          ELSE
            FirstTrainInstance := False;
        END;

        IF NOT TrainFound THEN
          T := T^.Train_NextRecord;
      END; {WITH}
    END; {WHILE}
  END;

  IF NOT TrainFound OR (T = NIL) THEN
    ErrorMsg := 'No record found for train ' + LocoChipToStr(LocoChip)
  ELSE BEGIN
    ErrorMsg := '';
    SetLength(TempStartAreas, 0);
    SetLength(TempStartLocations, 0);
    SetLength(TempEndAreas, 0);
    SetLength(TempEndLocations, 0);
  END;

  WITH T^ DO BEGIN
    IF (ErrorMsg = '')
    AND FirstTrainInstance
    THEN BEGIN
      { Save the address of the train for debugging purposes }
      SetLength(TempTrainArray, Length(TempTrainArray) + 1);
      TempTrainArray[High(TempTrainArray)] := T;

      { First the initialisations not already done in LocoUtils }
      IF TrainNonMoving THEN
        ChangeTrainStatus(T, NonMoving);

      Train_TypeNum := TypeOfTrainNum;
      Train_Type := TrainTypeNumToTrainType(Train_TypeNum);

      { Find out where this loco is in the loco data table - the routine returns -1 if the loco is not in the loco table }
      IF (Train_CurrentStatus = NonMoving)
      AND (LocoChip = UnknownLocoChip)
      THEN BEGIN
        ChangeTrainStatus(T, NonMoving);
        Train_DoubleHeaderLocoChip := UnknownLocoChip;
      END ELSE
        IF Train_CurrentStatus <> NonMoving THEN BEGIN
          ChangeTrainStatus(T, ReadyForCreation);
          { Check valid loconum for double-heading - applicable to trains not intended to move, too }
          IF DoubleHeaderLocoChip = UnknownLocoChip THEN
            Train_DoubleHeaderLocoChip := UnknownLocoChip
          ELSE
            IF Train_LocoChip = DoubleHeaderLocoChip THEN
              ErrorMsg := 'double header loco cannot be the same as the initial loco'
            ELSE
              Train_DoubleHeaderLocoChip := DoubleHeaderLocoChip;
        END;

      IF ErrorMsg = '' THEN BEGIN
        { Set up the first train location }
        IF StartLocationStr = '' THEN
          ErrorMsg := 'no start location supplied'
        ELSE
          IF StrToLocation(StartLocationStr) = UnknownLocation THEN BEGIN
            StartArea := StrToArea(StartLocationStr);
            { see if an area is specified }
            IF StartArea = UnknownArea THEN
              ErrorMsg := 'unknown start area/location ' + StartLocationStr
            ELSE BEGIN
              { see if the given train is anywhere in that area }
              IF (Train_LastLocation <> UnknownLocation)
              AND (Locations[Train_LastLocation].Location_Area = StartArea)
              AND (LocationOccupied(Train_LastLocation)
                   OR NOT SystemOnline)
              THEN BEGIN
                AppendToLocationArray(Train_Locations, Train_LastLocation);
                Train_CurrentSourceLocation := Train_Locations[0];

                { See if we can exit from the location }
                IF ((DirectionsArray[0] = Up)
                    AND Locations[Train_Locations[0]].Location_LineAtUpIsEndOfLine)
                OR ((DirectionsArray[0] = Down)
                    AND Locations[Train_Locations[0]].Location_LineAtDownIsEndOfLine)
                THEN
                  ErrorMsg := LocationToStr(Train_Locations[0])
                              + ': desired direction is ' + DirectionToStr(DirectionsArray[0])
                              + ' but line at ' + DirectionToStr(DirectionsArray[0]) + ' is marked as being an end of line';

                IF ErrorMsg = '' THEN BEGIN
                  Train_LocatedAtStartup := True;
                  Log(Train_LocoChipStr + ' D Start location changed' + ' from ' + StartLocationStr + ' to ' + LocationToStr(Train_Locations[0], ShortStringType));
                  StartLocationStr := LocationToStr(Train_Locations[0], ShortStringType);
                  IF Train_CurrentTC = UnknownTC THEN BEGIN
                    TempTCs := GetTrackCircuitsForLocation(Train_Locations[0]);
                    IF Length(TempTCs) > 0 THEN
                      Train_CurrentTC := TempTCs[0];
                  END;
                END;
              END;
            END;
          END;

        IF ErrorMsg = '' THEN BEGIN
          IF (Length(Train_Locations) = 0) OR (Train_Locations[0] = UnknownLocation) THEN BEGIN
            AppendToLocationArray(Train_Locations, StrToLocation(StartLocationStr));
            IF SystemOnline
            AND (Train_CurrentStatus <> NonMoving)
            THEN BEGIN
              IF NOT LocationOccupied(Train_LastLocation) THEN BEGIN
                InputFoundOrCancelled := False;
                NewLocationStr := '';
                ErrorMsg := 'No feedback found for loco ' + LocoChipToStr(Train_LocoChip) + '. Please type in its present location or CANCEL to suspend it';
                FWPRailMainWindow.MainTimer.Enabled := False;
                REPEAT
                  IF NOT InputQuery('Enter train location', ErrorMsg, NewLocationStr) THEN BEGIN
                    InputFoundOrCancelled := True;
                    ErrorMsg := 'No feedback found for loco ' + LocoChipToStr(Train_LocoChip);
                    Train_LocatedAtStartup := False;
                  END ELSE BEGIN
                    Train_Locations[0] := StrToLocation(NewLocationStr);
                    IF Train_Locations[0] = UnknownLocation THEN
                      ErrorMsg := 'Invalid location entered. Please type in present location of ' + LocoChipToStr(Train_LocoChip) + ' or CANCEL to suspend it'
                    ELSE BEGIN
                      IF NOT LocationOccupied(StrToLocation(NewLocationStr)) THEN BEGIN
                        Debug('!No feedback found at ' + NewLocationStr);
                        ErrorMsg := 'No feedback found at ' + NewLocationStr + '. Please type in present location of ' +  LocoChipToStr(Train_LocoChip)
                                    + ' or CANCEL to suspend it';
                      END ELSE BEGIN
                        InputFoundOrCancelled := True;
                        StartLocationStr := NewLocationStr;
                        Log(Train_LocoChipStr + ' D As no feedback found at train''s last location, start location set by user to ' + NewLocationStr);
                        Train_LastLocation := Train_Locations[0];
                        Train_LocatedAtStartup := True;
                        ErrorMsg := '';
                        { Note: InvalidateScreen needed here as InputQuery leaves the screen sized as a small rectangle }
                        InvalidateScreen(UnitRef, 'CreateTrainDiagramsRecord');
                      END;
                    END;
                  END;
                UNTIL InputFoundOrCancelled;
                FWPRailMainWindow.MainTimer.Enabled := True;
              END ELSE BEGIN
                IF Train_LastLocation <> Train_Locations[0] THEN BEGIN
                  CASE MessageDialogueWithDefault('Possible diagrams error: loco ' + LocoChipToStr(LocoChip) + ': ' + ErrorMsg
                                                  + CRLF
                                                  + 'Do you wish to change the diagrams so the loco departs from '
                                                  + LocationToStr(Train_LastLocation, ShortStringType) + ', '
                                                  +  CRLF
                                                  + 'start the program anyway (and cancel the train), or exit the program?',
                                                  StopTimer, mtError, [mbYes, mbNo, mbAbort], ['&Change', '&Start', '&Exit'], mbNo)
                  OF
                    mrYes:
                      BEGIN
                        Train_LocatedAtStartup := True;
                        Log(Train_LocoChipStr + ' D Start location changed'
                                              + ' to ' + LocationToStr(Train_LastLocation, ShortStringType)
                                              + ' from ' + LocationToStr(Train_Locations[0], ShortStringType));
                        Train_Locations[0] := Train_LastLocation;
                        IF Train_CurrentTC = UnknownTC THEN BEGIN
                          TempTCs := GetTrackCircuitsForLocation(Train_Locations[0]);
                          IF Length(TempTCs) > 0 THEN
                            Train_CurrentTC := TempTCs[0];
                        END;

                        StartLocationStr := LocationToStr(Train_LastLocation, ShortStringType);
                        ErrorMsg := '';
                      END;
                    mrNo:
                      BEGIN
                        Train_LocatedAtStartup := False;
                        ChangeTrainStatus(T, Cancelled);
                      END;
                    mrAbort:
                      ShutDownProgram(UnitRef, 'CreateTrainDiagramsRecord')
                  END; {CASE}
                END;
              END;

              { Is there a feedback location at the given start location? }
              IF NOT LocationOccupied(Train_Locations[0]) THEN BEGIN
                IF T^.Train_LastLocation = T^.Train_Locations[0] THEN
                  ErrorMsg := 'no feedback found at start location ' + LocationToStr(Train_Locations[0], ShortStringType)
                              + ' even though loco ' + LocoChipToStr(Train_LocoChip) + ' is recorded as being there'
                ELSE
                  IF Train_LastLocation <> UnknownLocation THEN BEGIN
                    IF Length(Train_Locations) = 0 THEN
                      ErrorMsg := 'no start location found - loco ' + LocoChipToStr(Train_LocoChip)
                                  + '''s last recorded location was ' + LocationToStr(Train_LastLocation, ShortStringType)
                    ELSE
                      IF Train_Locations[0] = UnknownLocation THEN BEGIN
                        { we've presumably been supplied only with an area }
                        TempArea := StrToArea(StartLocationStr);
                        IF TempArea <> UnknownArea THEN
                          ErrorMsg := 'loco not recorded as being in start area ' + AreaToStr(TempArea)
                                    + ': loco ' + LocoChipToStr(Train_LocoChip) + '''s last recorded location was ' + LocationToStr(Train_LastLocation, ShortStringType)
                        ELSE
                          ErrorMsg := 'no feedback found for loco ' + LocoChipToStr(Train_LocoChip)
                                    + ': it''s last recorded location was ' + LocationToStr(Train_LastLocation, ShortStringType)
                      END ELSE
                        ErrorMsg := 'no feedback found at start location ' + LocationToStr(Train_Locations[0], ShortStringType)
                                    + ': loco ' + LocoChipToStr(Train_LocoChip) + '''s last recorded location was ' + LocationToStr(Train_LastLocation, ShortStringType);
                  END ELSE
                    ErrorMsg := 'no feedback found at start location ' + LocationToStr(Train_Locations[0], ShortStringType)
                                + ': loco ' + LocoChipToStr(Train_LocoChip) + ' has no last recorded location';
                { But check if we want to continue anyway, as we may want to change the diagrams, or move a train to the original location before the start of operations.
                  If the latter, mark the train suspended for the time being.
                }
                { Is there feedback at the loco's last location? }
                
              END ELSE BEGIN
                LocoFound := False;
                OtherT := TrainList;
                WHILE (OtherT <> NIL)
                AND NOT LocoFound
                DO BEGIN
                  IF OtherT^.Train_LocoChip = Train_LocoChip THEN BEGIN
                    LocoFound := True;
                    IF Train_LastLocation <> Train_Locations[0] THEN BEGIN
                      { See if its last recorded location is currently occupied }
                      IF LocationOccupied(Train_LastLocation) THEN BEGIN
                        CASE MessageDialogueWithDefault('Loco ' + T^.Train_LocoChipStr
                                                        + ' is recorded as being at ' + LocationToStr(Train_LastLocation)
                                                        + ' but the diagram states that it is at ' + LocationToStr(Train_Locations[0])
                                                        + CRLF
                                                        + 'Do you want to change the Diagrams so that it is at ' + LocationToStr(Train_LastLocation)
                                                        + CRLF
                                                        + ' alter the train''s location to ' + LocationToStr(Train_Locations[0])
                                                        + CRLF
                                                        + ' or ignore the problem?',
                                                        StopTimer, mtError, [mbYes, mbNo, mbAbort], ['C&hange', '&Alter', '&Ignore'],
                                                        mbYes)
                        OF
                          mrYes:
                            BEGIN
                              StartLocationStr := LocationToStr(Train_LastLocation, ShortStringType);
                              Train_Locations[0] := Train_LastLocation;
                              Train_LocatedAtStartup := True;
                            END;
                          mrNo:
                            BEGIN
                              Train_LastLocation := Train_Locations[0];
                              Train_LastTC := GetTrackCircuitsForLocation(Train_Locations[0])[0];
                              Train_LocatedAtStartup := True;
                            END;
                          mrAbort:
                            BEGIN
                              Train_LocatedAtStartup := False;
                              ErrorMsg := 'Loco ' + T^.Train_LocoChipStr + ' has trackcircuits marked as having feedback occupation at ' + LocationToStr(Train_Locations[0])
                                          + ', but the loco is recorded as being at ' + LocationToStr(Train_LastLocation) + ': user aborted startup';
                            END;
                        END; {CASE}
                      END ELSE BEGIN
                        IF MessageDialogueWithDefault('Loco ' + T^.Train_LocoChipStr
                                                      + ' has trackcircuits marked as having feedback occupation at ' + LocationToStr(Train_Locations[0])
                                                      + ',' + CRLF
                                                      + IfThen(Train_LastLocation <> UnknownLocation,
                                                               'but the loco is recorded as being at ' + LocationToStr(Train_LastLocation),
                                                               'but the loco has no recorded location')
                                                      + CRLF
                                                      + 'Do you want to change its occupation to '
                                                      + LocationToStr(Train_Locations[0])
                                                      + ' or cancel the it?',
                                                      StopTimer, mtError, [mbYes, mbNo], ['C&hange', '&Cancel'], mbYes) = mrNo
                        THEN
                          ChangeTrainStatus(T, Cancelled)
                        ELSE BEGIN
                          Train_LastLocation := Train_Locations[0];
                          Train_LastTC := GetTrackCircuitsForLocation(Train_Locations[0])[0];
                          Train_LocatedAtStartup := True;
                        END;
                      END;
                    END;

                    { See if any other loco is supposed to be occupying that trackcircuit }
                    TempT := TrainList;
                    WHILE TempT <> NIL DO BEGIN
                      IF (TempT^.Train_LocoChip <> UnknownLocoChip)
                      AND ((TempT^.Train_LastLocation = Train_Locations[0])
                           AND (Train_Locations[0] <> UnknownLocation))
                      AND (TempT^.Train_LocoChip <> T^.Train_LocoChip)
                      THEN BEGIN
                        IF MessageDialogueWithDefault('Loco ' + LocoChipToStr(TempT^.Train_LocoChip)
                                                      + ' is recorded as being at' + ' ' + LocationToStr(Train_Locations[0])
                                                      + ',' + CRLF
                                                      + 'Do you want to replace it with ' + ' ' + LocoChipToStr(Train_LocoChip)
                                                      + ' or exit the program?',
                                                      StopTimer, mtError, [mbYes, mbAbort], ['&Replace', '&Exit'], mbYes) = mrAbort
                        THEN BEGIN
                          ErrorMsg := 'loco ' + LocoChipToStr(TempT^.Train_LocoChip)
                                      + ' is recorded as being at' + ' ' + LocationToStr(Train_Locations[0])
                                      + ': user did not want to substitute' + ' ' + LocoChipToStr(Train_LocoChip)
                                      + ' so aborted startup';
                          Train_LocatedAtStartup := False;
                        END ELSE BEGIN
                          TempT^.Train_LastLocation := UnknownLocation;
                          TempT^.Train_LastTC := UnknownTC;
                          Train_LastLocation := Train_Locations[0];
                          Train_LastTC := GetTrackCircuitsForLocation(Train_Locations[0])[0];
                          Train_LocatedAtStartup := True;
                        END;
                      END;
                      TempT := TempT^.Train_NextRecord;
                    END; {WHILE}
                  END;
                  OtherT := OtherT^.Train_NextRecord;
                END; {WHILE}
              END;
            END;
          END;
          Train_CurrentSourceLocation := Train_Locations[0];
        END;
      END;
    END;

    { If this train is moving then see if any previous instances of it are non-moving and vice versa }
    IF NOT FirstTrainInstance THEN BEGIN
      IF (TrainNonMoving
      AND (Train_CurrentStatus <> NonMoving))
      OR (NOT TrainNonMoving
          AND (Train_CurrentStatus = NonMoving))
      THEN
        ErrorMsg := 'train cannot be both moving and non-moving';
    END;

    IF (ErrorMsg = '')
    AND (Train_CurrentStatus <> Cancelled)
    THEN BEGIN
      { Now add data regardless of whether this is a new record or not }
      IF FirstTrainInstance THEN BEGIN
        { First see if it's a stationary train }
        IF Train_CurrentStatus = NonMoving THEN BEGIN
          Log(Train_LocoChipStr + ' D noted as non-moving');
          { and enable the appropriate popup to allow it to be displayed }
          DiagramsWindow.PopupShowNonMovingTrains.Enabled := True;
        END;
      END;

      { If a loco has no speed settings set up, it can't run }
      IF (Train_CurrentStatus <> NonMoving)
      AND Train_SpeedSettingsMissing
      THEN BEGIN
        Debug('!Loco ' + LocoChipToStr(Train_LocoChip) + ' has no speed settings in the loco database - cancelled');
        ErrorMsg := 'Loco ' + LocoChipToStr(Train_LocoChip) + ' has no speed settings in the loco database';
      END;

      { Locations and areas }
      IF ErrorMsg = '' THEN BEGIN
        I := 0;
        WHILE (ErrorMsg = '')
        AND (I <= JourneyCount)
        DO BEGIN
          IF I = 0 THEN BEGIN
            AppendToLocationArray(TempStartLocations, StrToLocation(StartLocationStr));
            IF TempStartLocations[0] <> UnknownLocation THEN
              AppendToAreaArray(TempStartAreas, Locations[TempStartLocations[0]].Location_Area)
            ELSE
              IF StrToArea(StartLocationStr) <> UnknownArea THEN
                AppendToAreaArray(TempStartAreas, StrToArea(StartLocationStr));

            AppendToLocationArray(TempEndLocations, StrToLocation(EndLocationsStrArray[0]));
            IF TempEndLocations[0] <> UnknownLocation THEN
              AppendToAreaArray(TempEndAreas, Locations[TempEndLocations[0]].Location_Area)
            ELSE BEGIN
              IF StrToArea(EndLocationsStrArray[0]) <> UnknownArea THEN
                AppendToAreaArray(TempEndAreas, StrToArea(EndLocationsStrArray[0]))
              ELSE
                ErrorMsg := 'unknown location/area "' + EndLocationsStrArray[0] + '"';

                { add opportunity to correct it? **** }
            END;
          END ELSE BEGIN
            AppendToLocationArray(TempStartLocations, StrToLocation(EndLocationsStrArray[I - 1]));
            IF TempStartLocations[High(TempStartLocations)] <> UnknownLocation THEN
              AppendToAreaArray(TempStartAreas, Locations[TempStartLocations[High(TempStartLocations)]].Location_Area)
            ELSE
              AppendToAreaArray(TempStartAreas, StrToArea(EndLocationsStrArray[I - 1]));

            AppendToLocationArray(TempEndLocations, StrToLocation(EndLocationsStrArray[I]));
            IF TempEndLocations[High(TempEndLocations)] <> UnknownLocation THEN
              AppendToAreaArray(TempEndAreas, Locations[TempEndLocations[High(TempEndLocations)]].Location_Area)
            ELSE BEGIN
              IF StrToArea(EndLocationsStrArray[I]) <> UnknownArea THEN
                AppendToAreaArray(TempEndAreas, StrToArea(EndLocationsStrArray[I]))
              ELSE
                ErrorMsg := 'unknown location/area "' + EndLocationsStrArray[I] + '"';

                { add opportunity to correct it? **** }
            END;
          END;

          Inc(I);
        END; {WHILE}
      END;

      { Now the database-related initialisations - firstly direction and stopping }
      IF ErrorMsg = '' THEN BEGIN
        IF Train_CurrentStatus = NonMoving THEN BEGIN
          IF DirectionsArray[0] <> UnknownDirection THEN
            Train_CurrentDirection := DirectionsArray[0];
        END ELSE BEGIN
          { If the loco can only go in one direction (perhaps because it's hauling coaches or wagons), that's the direction we have to go in }
          IF Train_FixedDirection <> UnknownDirection THEN BEGIN
            FOR I := 0 TO High(DirectionsArray) DO
              DirectionsArray[I] := Train_FixedDirection;
            Log(Train_LocoChipStr + ' D Inserting fixed direction=' + DirectionToStr(DirectionsArray[0]) + ' to all journeys');
          END ELSE BEGIN
            FOR I := 0 TO JourneyCount DO BEGIN
              IF DirectionsArray[I] = UnknownDirection THEN BEGIN
                { We have to work the direction out from the Areas or Locations supplied }
                TempDirection1 := UnknownDirection;
                TempDirection2 := UnknownDirection;

                IF TempStartLocations[I] <> UnknownLocation THEN BEGIN
                  { we've been given a specific start location, so there may be a specific up or down route to it }
                  IF Length(Locations[TempStartLocations[I]].Location_AccessibleLocationsUp) = 0 THEN
                    TempDirection1 := Down
                  ELSE
                    IF Length(Locations[TempStartLocations[I]].Location_AccessibleLocationsDown) = 0 THEN
                      TempDirection1 := Up
                    ELSE
                      TempDirection1 := BiDirectional;

                  Log(Train_LocoChipStr + ' D J=' + IntToStr(I) + ': inserting temporary direction=' + DirectionToStr(TempDirection1));
                END;

                IF TempEndLocations[I] <> UnknownLocation THEN BEGIN
                  { we've been given a specific end location, so there may be a specific up or down route to it }
                  IF Length(Locations[TempEndLocations[I]].Location_AccessibleLocationsUp) = 0 THEN
                    TempDirection2 := Up
                  ELSE
                    IF Length(Locations[TempEndLocations[I]].Location_AccessibleLocationsDown) = 0 THEN
                      TempDirection2 := Down
                    ELSE
                      TempDirection1 := BiDirectional;

                  IF (TempDirection1 <> UnknownDirection)
                  AND (TempDirection1 <> TempDirection2)
                  THEN
                    ErrorMsg := 'incompatibility in directions'
                  ELSE
                    Log(Train_LocoChipStr + ' D J=' + IntToStr(I) + ': inserting temporary direction=' + DirectionToStr(TempDirection2));
                END;

                IF TempDirection1 <> UnknownDirection THEN
                  DirectionsArray[I] := TempDirection1
                ELSE
                  IF TempDirection2 <> UnknownDirection THEN
                    DirectionsArray[I] := TempDirection2;

                IF (DirectionsArray[I] = Bidirectional) OR (DirectionsArray[I] = UnknownDirection) THEN BEGIN
                  IF IsElementInIntegerArray(Areas[TempStartAreas[I]].Area_AccessibleAreasUp, TempEndAreas[I])
                  AND IsElementInIntegerArray(Areas[TempStartAreas[I]].Area_AccessibleAreasDown, TempEndAreas[I])
                  THEN
                    DirectionsArray[I] := BiDirectional
                  ELSE
                    IF IsElementInIntegerArray(Areas[TempStartAreas[I]].Area_AccessibleAreasUp, TempEndAreas[I]) THEN
                      DirectionsArray[I] := Up
                    ELSE
                      IF IsElementInIntegerArray(Areas[TempStartAreas[I]].Area_AccessibleAreasDown, TempEndAreas[I]) THEN
                        DirectionsArray[I] := Down
                      ELSE
                        ErrorMsg := 'End area ''' + AreaToStr(TempEndAreas[I])
                                    + ''' is not accessible from ' + AreaToStr(TempStartAreas[I])
                                    + ' according to AccessibleAreasUp or AccessibleAreasDown for the ' + AreaToStr(TempStartAreas[I]);
                END;

                IF ErrorMsg = '' THEN
                  Log(Train_LocoChipStr + ' D J=' + IntToStr(I) + ': inserting temporary direction=' + DirectionToStr(DirectionsArray[I]));
              END;
            END; {FOR}

            { Where the journeys can be in either direction, replace with the next or previous direction that is specific. First deal with cases where there is only one
              journey and the initial direction is bidirectional.
            }
            IF ErrorMsg = '' THEN BEGIN
              IF Length(DirectionsArray) = 1 THEN BEGIN
                IF DirectionsArray[0] = Bidirectional THEN BEGIN
                  { Note: the From in RandomRange is included in the results, but the To is not }
                  I := RandomRange(1, 3);
                  CASE I OF
                    1:
                      DirectionsArray[0] := Up;
                    2:
                      DirectionsArray[0] := Down;
                  END; {CASE}
                END;
              END ELSE BEGIN
                { Now where the initial direction is bidirectional }
                IF DirectionsArray[0] = Bidirectional THEN BEGIN
                  I := 1;
                  DirectionFound := False;
                  WHILE (I <= High(DirectionsArray))
                  AND NOT DirectionFound
                  DO BEGIN
                    { look for a specific direction }
                    IF DirectionsArray[I] <> Bidirectional THEN
                      { apply it to the first direction }
                      DirectionFound := True;
                      DirectionsArray[0] := DirectionsArray[I];
                    Inc(I);
                  END; {WHILE}

                  IF NOT DirectionFound THEN BEGIN
                    { all the directions are bidirectional - just choose the first at random. Note: the From in RandomRange is included in the results, but the To is not. }
                    I := RandomRange(1, 3);
                    CASE I OF
                      1:
                        DirectionsArray[0] := Up;
                      2:
                        DirectionsArray[0] := Down;
                    END; {CASE}
                  END;
                END;

                { Now we know the directions begin with a specific direction, so maintain that direction until it specifically changes }
                IF Length(DirectionsArray) > 1 THEN BEGIN
                  I := 1;
                  WHILE I <= High(DirectionsArray) DO BEGIN
                    IF DirectionsArray[I] = Bidirectional THEN
                      DirectionsArray[I] := DirectionsArray[I - 1];

                    Inc(I);
                  END; {WHILE}
                END;
              END;
            END; {FOR}

            IF ErrorMsg = '' THEN
              FOR I := 0 TO JourneyCount DO
              Log(Train_LocoChipStr + ' D J=' + IntToStr(I) + ': inserting final direction=' + DirectionToStr(DirectionsArray[I]));
          END;

          { Finally initialise the current direction variable }
          IF FirstTrainInstance THEN
            Train_CurrentDirection := DirectionsArray[0];
        END;
      END;

      IF (ErrorMsg = '')
      AND FirstTrainInstance
      THEN BEGIN
        IF (Train_LastLengthInInches <> 0)
        AND (Train_FixedLengthInInches <> 0)
        THEN BEGIN
          IF Train_LastLengthInInches = Train_FixedLengthInInches THEN BEGIN
            Train_CurrentLengthInInches := Train_FixedLengthInInches;
            Log(Train_LocoChipStr + ' T Train_CurrentLengthInInches is ' + IntToStr(Train_CurrentLengthInInches) + ' (from Train_FixedLengthInInches)');
          END ELSE
            IF MessageDialogueWithDefault('Diagrams error: loco ' + LocoChipToStr(LocoChip) + ':'
                                          + ' last train length as read in from the LocoData database is ' + IntToStr(Train_LastLengthInInches) + ' inches'
                                          + CRLF
                                          + 'whereas the fixed length as read in from the LocoData database is ' + IntToStr(Train_FixedLengthInInches) + ' inches.'
                                          + CRLF
                                          + 'Please choose the current length below:',
                                          StopTimer, mtError, [mbYes, mbNo],
                                          [IntToStr(Train_LastLengthInInches) + ' inches',
                                           IntToStr(Train_FixedLengthInInches) + ' inches'],
                                          mbYes) = mrYes
            THEN BEGIN
              Train_CurrentLengthInInches := Train_LastLengthInInches;
              Log(Train_LocoChipStr + ' T Train_CurrentLengthInInches is ' + IntToStr(Train_CurrentLengthInInches) + ' (from Train_LastLengthInInches)');
            END ELSE BEGIN
              Train_CurrentLengthInInches := Train_FixedLengthInInches;
              Log(Train_LocoChipStr + ' T Train_CurrentLengthInInches is ' + IntToStr(Train_CurrentLengthInInches) + ' (from Train_FixedLengthInInches)');
            END;
        END;

        IF Train_CurrentLengthInInches = 0 THEN BEGIN
          IF Train_FixedLengthInInches <> 0 THEN BEGIN
            IF (LengthOfTrainInCarriages <> 0)
            AND (Train_FixedLengthInInches <> (LengthOfTrainInCarriages * CarriageLengthInInches))
            THEN BEGIN
              IF MessageDialogueWithDefault('Diagrams error: loco ' + LocoChipToStr(LocoChip) + ':'
                                            + ' train length in carriages as read in from the diagram is '
                                            + IfThen(LengthOfTrainInCarriages = 1,
                                                     'one carriage',
                                                     IntToStr(LengthOfTrainInCarriages) + ' carriages')
                                            + ' which works out at ' + IntToStr(LengthOfTrainInCarriages * CarriageLengthInInches) + ' inches'
                                            + CRLF
                                            + 'whereas the fixed length as read in from the LocoData database is ' + IntToStr(Train_FixedLengthInInches) + ' inches.'
                                            + CRLF
                                            + 'Please choose the current length below:',
                                            StopTimer, mtError, [mbYes, mbNo], [IntToStr(LengthOfTrainInCarriages * CarriageLengthInInches) + ' inches',
                                             IntToStr(Train_FixedLengthInInches) + ' inches'],
                                            mbYes) = mrNo
              THEN BEGIN
                Train_CurrentLengthInInches := Train_FixedLengthInInches;
                Log(Train_LocoChipStr + ' T Train_CurrentLengthInInches is ' + IntToStr(Train_CurrentLengthInInches) + ' (from Train_FixedLengthInInches)');
              END ELSE BEGIN
                Train_CurrentLengthInInches := LengthOfTrainInCarriages * CarriageLengthInInches;
                Log(Train_LocoChipStr + ' T Train_CurrentLengthInInches is ' + IntToStr(Train_CurrentLengthInInches)
                                      + ' (from LengthOfTrainInCarriages * CarriageLengthInInches)');
              END;
            END ELSE BEGIN
              Train_CurrentLengthInInches := Train_FixedLengthInInches;
              Log(Train_LocoChipStr + ' T Train_CurrentLengthInInches is ' + IntToStr(Train_CurrentLengthInInches) + ' (from Train_FixedLengthInInches)');
            END;
          END ELSE
            IF Train_LastLengthInInches <> 0 THEN BEGIN
              IF (LengthOfTrainInCarriages <> 0)
              AND (Train_LastLengthInInches <> (LengthOfTrainInCarriages * CarriageLengthInInches))
              THEN BEGIN
                IF MessageDialogueWithDefault('Diagrams error: loco ' + LocoChipToStr(LocoChip) + ':'
                                              + ' train length in carriages as read in from the diagram is '
                                              + IfThen(LengthOfTrainInCarriages = 1,
                                                       'one carriage',
                                                       IntToStr(LengthOfTrainInCarriages) + ' carriages')
                                              + ' which works out at ' + IntToStr(LengthOfTrainInCarriages * CarriageLengthInInches) + ' inches'
                                              + CRLF
                                              + 'whereas the last length as read in from the LocoData database is ' + IntToStr(Train_LastLengthInInches) + ' inches.'
                                              + CRLF
                                              + 'Please choose the current length below:',
                                              StopTimer, mtError, [mbYes, mbNo],
                                              [IntToStr(LengthOfTrainInCarriages * CarriageLengthInInches) + ' inches',
                                               IntToStr(Train_LastLengthInInches) + ' inches'],
                                              mbYes) = mrNo
                THEN BEGIN
                  Train_CurrentLengthInInches := Train_LastLengthInInches;
                  Log(Train_LocoChipStr + ' T Train_CurrentLengthInInches is ' + IntToStr(Train_CurrentLengthInInches) + ' (from Train_LastLengthInInches)');
                END ELSE BEGIN
                  Train_CurrentLengthInInches := LengthOfTrainInCarriages * CarriageLengthInInches;
                  Log(Train_LocoChipStr + ' T Train_CurrentLengthInInches is ' + IntToStr(Train_CurrentLengthInInches)
                                        + ' (from LengthOfTrainInCarriages * CarriageLengthInInches)');
                END;
              END ELSE BEGIN
                Train_CurrentLengthInInches := Train_LastLengthInInches;
                Log(Train_LocoChipStr + ' T Train_CurrentLengthInInches is ' + IntToStr(Train_CurrentLengthInInches) + ' (from Train_LastLengthInInches)');
              END;
            END ELSE
              IF LengthOfTrainInCarriages = 0 THEN BEGIN
                FWPRailMainWindow.MainTimer.Enabled := False;
                TempStr := InputBox('Caption',
                                    'Diagrams error: loco ' + LocoChipToStr(LocoChip) + ':'
                                    + CRLF
                                    + 'No train length supplied in location record or diagram record: enter number of carriages now:',
                                    '0');
                IF NOT TryStrToInt(TempStr, LengthOfTrainInCarriages) THEN
                  ErrorMsg := 'Invalid integer ''' + TempStr + ''' supplied in location record or diagrams record'
                ELSE
                  IF LengthOfTrainInCarriages = 0 THEN
                    ErrorMsg := 'No train length supplied in location record or diagram record'
                  ELSE
                    Train_CurrentLengthInInches := LengthOfTrainInCarriages * CarriageLengthInInches;
                FWPRailMainWindow.MainTimer.Enabled := True;
              END ELSE BEGIN
                Train_CurrentLengthInInches := LengthOfTrainInCarriages * CarriageLengthInInches;
                Log(Train_LocoChipStr + ' T Train_CurrentLengthInInches is ' + IntToStr(Train_CurrentLengthInInches)
                                      + ' (from LengthOfTrainInCarriages * CarriageLengthInInches)');
              END;
        END;
      END;

      IF (ErrorMsg = '')
      AND (Train_CurrentStatus <> NonMoving)
      THEN BEGIN
//        IF FirstTrainInstance THEN
//          SetUpTrainSpeeds;

        { Set up the first train destination }
        IF ErrorMsg = '' THEN BEGIN
          { but make sure the next starting point is the same as the previous location }
          IF NOT FirstTrainInstance THEN BEGIN
            IF (Train_Locations[High(Train_Locations)] <> StrToLocation(StartLocationStr))
            AND (Train_Locations[High(Train_Locations)] <> UnknownLocation)
            THEN
              ErrorMsg := 'train ' + Train_LocoChipStr + '''s start location at ' + StartLocationStr + ' is not the same as its previous location which was '
                          + LocationToStr(Train_Locations[High(Train_Locations)], ShortStringType) + '.';
          END;
        END;

        IF ErrorMsg = '' THEN BEGIN
          AppendToLocationArray(Train_Locations, StrToLocation(EndLocationsStrArray[0]));

          { Now the remaining locations }
          I := 1;
          WHILE (ErrorMsg = '')
          AND (I <= JourneyCount)
          DO BEGIN
            IF EndLocationsStrArray[I] <> '' THEN
              { add one to the journey counter in the train record first - it starts at one }
              AppendToLocationArray(Train_Locations, StrToLocation(EndLocationsStrArray[I]))
            ELSE
              ErrorMsg := 'No end location supplied';
            Inc(I);
          END; {WHILE}
        END;

        { Departure times }
        IF ErrorMsg = '' THEN BEGIN
          I := 0;
          WHILE I <= JourneyCount DO BEGIN
            AppendToDateTimeArray(DepartureTimesArray, UserSpecifiedDepartureTimesArray[I]);
            Inc(I);
          END; {WHILE}

          { See if the initial departure time is not specified }
          IF DepartureTimesArray[0] = 0 THEN BEGIN
            IF Length(DepartureTimesArray) = 1 THEN
              { if there is only one departure time and it's not specified, make it the program start time }
              DepartureTimesArray[0] := StrToTime(ProgramStartTimeStr)
            ELSE
              IF Length(DepartureTimesArray) > 1 THEN BEGIN
                { if there is more than one departure time }
                 IF DepartureTimesArray[1] = 0 THEN
                   { if the first two departure times are not specified, make the first the program start time }
                   DepartureTimesArray[0] := StrToTime(ProgramStartTimeStr)
                 ELSE
                   IF (DepartureTimesArray[1] <> 0)
                   AND NotForPublicUseArray[0]
                   THEN
                     { if there is more than one departure time and the first is not specified but the second is, and the first journey is marked 'not for public use',
                       i.e. its only purpose is to get to the second journey starting point
                     }
                     Train_FirstStationSpecifiedStartTime := DepartureTimesArray[1];
              END;
          END;
        END;

//        { Locations and areas }
//        IF ErrorMsg = '' THEN BEGIN
//          I := 0;
//          WHILE (ErrorMsg = '')
//          AND (I <= JourneyCount)
//          DO BEGIN
//            IF I = 0 THEN BEGIN
//              AppendToLocationArray(TempStartLocations, StrToLocation(StartLocationStr));
//              IF TempStartLocations[0] <> UnknownLocation THEN
//                AppendToAreaArray(TempStartAreas, Locations[TempStartLocations[0]].Location_Area)
//              ELSE
//                IF StrToArea(StartLocationStr) <> UnknownArea THEN
//                  AppendToAreaArray(TempStartAreas, StrToArea(StartLocationStr));
//
//              AppendToLocationArray(TempEndLocations, StrToLocation(EndLocationsStrArray[0]));
//              IF TempEndLocations[0] <> UnknownLocation THEN
//                AppendToAreaArray(TempEndAreas, Locations[TempEndLocations[0]].Location_Area)
//              ELSE BEGIN
//                IF StrToArea(EndLocationsStrArray[0]) <> UnknownArea THEN
//                  AppendToAreaArray(TempEndAreas, StrToArea(EndLocationsStrArray[0]))
//                ELSE
//                  ErrorMsg := 'unknown location/area "' + EndLocationsStrArray[0] + '"';
//
//                  { add opportunity to correct it? **** }
//              END;
//            END ELSE BEGIN
//              AppendToLocationArray(TempStartLocations, StrToLocation(EndLocationsStrArray[I - 1]));
//              IF TempStartLocations[High(TempStartLocations)] <> UnknownLocation THEN
//                AppendToAreaArray(TempStartAreas, Locations[TempStartLocations[High(TempStartLocations)]].Location_Area)
//              ELSE
//                AppendToAreaArray(TempStartAreas, StrToArea(EndLocationsStrArray[I - 1]));
//
//              AppendToLocationArray(TempEndLocations, StrToLocation(EndLocationsStrArray[I]));
//              IF TempEndLocations[High(TempEndLocations)] <> UnknownLocation THEN
//                AppendToAreaArray(TempEndAreas, Locations[TempEndLocations[High(TempEndLocations)]].Location_Area)
//              ELSE BEGIN
//                IF StrToArea(EndLocationsStrArray[I]) <> UnknownArea THEN
//                  AppendToAreaArray(TempEndAreas, StrToArea(EndLocationsStrArray[I]))
//                ELSE
//                  ErrorMsg := 'unknown location/area "' + EndLocationsStrArray[I] + '"';
//
//                  { add opportunity to correct it? **** }
//              END;
//            END;
//
//            Inc(I);
//          END; {WHILE}
//        END;

        { Lights on time }
        IF (ErrorMsg = '')
        AND FirstTrainInstance
        THEN BEGIN
          IF Train_LightsType = NoLights THEN BEGIN
            IF LightsOnTime <> 0 THEN
              Log(Train_LocoChipStr + ' X! Loco ' + LocoChipToStr(Train_LocoChip) + ' cannot have a lights on time as loco does not have lights');
          END ELSE
            { Check it's not before the current time }
            IF LightsOnTime <> 0 THEN BEGIN
              IF CurrentRailwayTime > LightsOnTime THEN
                Log(Train_LocoChipStr + ' X! Loco ' + LocoChipToStr(Train_LocoChip) + ': lights-on time ' + TimeToHMStr(LightsOnTime)
                                      + ' is earlier than current railway time ' + TimeToHMStr(CurrentRailwayTime))
              ELSE
                IF DepartureTimesArray[0] < LightsOnTime THEN
                  Log(Train_LocoChipStr + ' X! Loco ' + LocoChipToStr(Train_LocoChip) + ': lights-on time ' + TimeToHMStr(LightsOnTime)
                                        + ' should be earlier than initial departure time ' + TimeToHMStr(DepartureTimesArray[0]))
                ELSE
                  Train_LightsOnTime := LightsOnTime;
          END;
        END;

        { Lights remain on }
        Train_LightsRemainOnWhenJourneysComplete := LightsRemainOn;

        IF (ErrorMsg = '')
        AND FirstTrainInstance
        THEN BEGIN
          Train_UserDriving := UserDriving;
          Train_UserRequiresInstructions := UserRequiresInstructions;
          Train_UserRequiresInstructionMsg := '';
        END;

        { Set up the various individual journeys for this train }
        IF ErrorMsg = '' THEN BEGIN
          I := 0;
          OK := True;
          WHILE (ErrorMsg = '')
          AND (I <= JourneyCount)
          DO BEGIN
            { Add one to the journey counter in the train record first - it starts at one }
            Inc(Train_TotalJourneys);
            
            { Check the journey }
            IF (TempStartLocations[I] <> UnknownLocation)
            AND (TempEndLocations[I] <> UnknownLocation)
            THEN BEGIN
              CheckJourney(T, Train_TotalJourneys, DirectionsArray[I], TempStartLocations[I], TempEndLocations[I], UnknownLine, UnknownLine, ErrorMsg, LinesNotAvailableStr,
                           NOT UseEmergencyRouteing, JourneysArray, OK);
              IF NOT OK THEN
                CheckJourney(T, Train_TotalJourneys, DirectionsArray[I], TempStartLocations[I], TempEndLocations[I], UnknownLine, UnknownLine, ErrorMsg,
                             LinesNotAvailableStr, UseEmergencyRouteing, JourneysArray, OK);
            END;

            IF OK THEN BEGIN
              { NB: this test of OK is not dependent on the result coming from CheckJourney above, as that may not have been called }
              IF StartOfRepeatJourney
              AND (I = 0)
              THEN
                CreateJourney(T, UnknownJourney, NewJourney,
                              TempStartAreas[I], TempEndAreas[I],
                              TempStartLocations[I], TempEndLocations[I],
                              TempStartLocations[I], TempEndLocations[I],
                              UnknownLine, UnknownLine,
                              0, DepartureTimesArray[I],
                              0,
                              DirectionsArray[I],
                              DummyRouteArray,
                              NOT RebuildRouteArray,
                              StoppingArray[I],
                              NotForPublicUseArray[I],
                              NOT UseEmergencyRouteing,
                              RepeatJourney,
                              DiagramsLoading,
                              ErrorMsg,
                              LinesNotAvailableStr, OK)
              ELSE
                CreateJourney(T, UnknownJourney, NewJourney,
                              TempStartAreas[I], TempEndAreas[I],
                              TempStartLocations[I], TempEndLocations[I],
                              TempStartLocations[I], TempEndLocations[I],
                              UnknownLine, UnknownLine,
                              0, DepartureTimesArray[I],
                              0,
                              DirectionsArray[I],
                              DummyRouteArray,
                              NOT RebuildRouteArray,
                              StoppingArray[I],
                              NotForPublicUseArray[I],
                              NOT UseEmergencyRouteing,
                              NOT RepeatJourney,
                              DiagramsLoading,
                              ErrorMsg,
                              LinesNotAvailableStr, OK);

              IF NOT OK THEN BEGIN
                { the journey route wasn't ok, so we can try substituting an area for the End location }
                TempEndLocations[I] := UnknownLocation;
                OK := True;
              END;
            END;
            Inc(I);
          END;
        END;
      END;

      IF ErrorMsg = '' THEN BEGIN
        { this part is both for all trains, moving and non-moving, and those with pending locations too }
  //      Train_Headcode := GetHeadcode(Train_LocoChip, Train_TypeNum, Train_FinalEndStationName);

//        SetInitialTrackCircuits(T);
//        IF Train_InitialTrackcircuits[1] = UnknownTC THEN
//          ErrorMsg := 'no initial trackcircuits found';
      END;

      IF (ErrorMsg = '')
      AND (Train_CurrentStatus = NonMoving)
      THEN BEGIN
        { non-moving trains should have their lights on if not in sidings *** are these ever turned off? *** }
        { we need to know which way the train is facing for lighting purposes *** }
        Train_Headcode := '0000';
        IF DirectionsArray[0] = UnknownDirection THEN
          ErrorMsg := 'direction1 must be ''U'' or ''D'' even though train is non-moving';
        TurnLightsOn(Train_LocoChip, OK);
      END;

      T^.Train_Headcode := 'ZZZZ';

    END;

    IF ErrorMsg <> '' THEN BEGIN
      { Now need to sort out the problem }
      IF T <> NIL THEN
        Log(Train_LocoChipStr + ' E Loco ' + LocoChipToStr(LocoChip) + ': diagrams error: ' + ErrorMsg)
      ELSE
        Log('E Loco ' + LocoChipToStr(LocoChip) + ': diagrams error: ' + ErrorMsg);

      IF MessageDialogueWithDefault('Diagrams error: loco ' + LocoChipToStr(LocoChip) + ': ' + ErrorMsg
                                    + CRLF
                                    + 'Do you wish to start the program and cancel the train or exit the program?',
                                    StopTimer, mtError, [mbYes, mbNo], ['&Start', '&Exit'], mbYes) = mrNo
      THEN
        ShutDownProgram(UnitRef, 'CreateTrainDiagramsRecord')
      ELSE
        IF T <> NIL THEN
          CancelTrain(T, NOT ByUser, TrainExists);
    END;
  END; {WITH}

  { And return T for those subroutines that want it }
  Result := T;
END; { CreateTrainDiagramsRecord }

PROCEDURE ReadInDiagramsFromDatabase(OUT ErrorMsg : String; OUT DiagramsMissing, DiagramsOK : Boolean);
{ Read the diagrams in from the supplied database }
CONST
  EmergencyRouteing = True;
  MaxJourneys = 7;
  NoJourneyName = '';
  NoOldHeadcode = '';
  NoTrainJourney_SpecifiedDepartureTime = 0;
  PreRouteing = True;
  StartOfRepeatJourney = True;
  StoppingOnArrival = True;

VAR
  ActiveTrainCount : Integer;
  I : Integer;
  JourneyCount : Integer;
  MoveToStablingAfterLastJourney : Boolean;
  NoDestination : Boolean;
  NonActiveTrainCount : Integer;
  OmitTrain : Boolean;
  SaveDirection : DirectionType;
  SaveLastJourneyAreaOrDestinationStr : String;
  SaveLastJourneyDepartureTime : TDateTime;
  SaveLastJourneyDirection : DirectionType;
  T : Train;
  TempSourceLocation : Integer;
  T_DepartureTimesArray : DateTimeArrayType;
  T_DestinationAreaOrLocationsStrArray : StringArrayType;
  T_DirectionsArray : DirectionArrayType;
  T_DoubleHeaderLocoChip : Integer;
  T_LightsOnTime : TDateTime;
  T_LightsRemainOn : Boolean;
  T_LocoChip : Integer;
  T_NonMoving : Boolean;
  T_NotForPublicUseArray : BooleanArrayType;
  T_RepeatFrequencyInMinutes : Integer;
  T_RepeatUntilTime : TDateTime;
  T_StartAreaOrLocationStr : String;
  T_StoppingArray : BooleanArrayType;
  T_TrainActive : Boolean;
  T_TrainLengthInCarriages : Integer;
  T_TrainTypeNum : Integer;
  T_UserDriving : Boolean;
  T_UserRequiresInstructions : Boolean;

BEGIN
  TRY
    WITH DiagramsWindow DO BEGIN
      ErrorMsg := '';
      JourneyCount := -1;
      MoveToStablingAfterLastJourney := False;
      DiagramsOK := True;
      DiagramsMissing := False;
      T_TrainLengthInCarriages := 0;
      T_LightsOnTime := 0;
      T_LightsRemainOn := True;
      T_RepeatFrequencyInMinutes := 0;
      T_RepeatUntilTime := 0;
      T_TrainTypeNum := 0;
      T_UserDriving := False;
      T_UserRequiresInstructions := False;
      SaveLastJourneyDirection := UnknownDirection;
      SaveLastJourneyDepartureTime := 0;

      ActiveTrainCount := 0;
      NonActiveTrainCount := 0;
      { Now initialise the location occupation arrays }
      DiagramsLoaded := False;
      IF NOT FileExists(PathToRailDataFiles + DiagramsFilename + '.' + DiagramsFilenameSuffix) THEN BEGIN
        DiagramsOK := False;
        DiagramsMissing := True;
        Exit;
      END;

      IF NOT FileExists(PathToRailDataFiles + DiagramsFilename + '.' + DiagramsFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Diagrams database file "' + PathToRailDataFiles + DiagramsFilename + '.' + DiagramsFilenameSuffix + '"'
                                      + ' cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ReadInDiagramsFromDatabase')
        ELSE
          Exit;
      END;

      DiagramsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                 + PathToRailDataFiles + DiagramsFilename + '.' + DiagramsFilenameSuffix
                                                 + ';Persist Security Info=False';
      DiagramsADOConnection.Connected := True;

      DiagramsADOTable.Open;
      Log('D Diagrams table and connection opened to create the diagrams');

      DiagramsADOTable.Sort := 'LocoChip ASC, DepartureTime0 ASC';

      DiagramsADOTable.First;
      WHILE DiagramsOK
      AND NOT DiagramsADOTable.EOF
      DO BEGIN
        WITH DiagramsADOTable DO BEGIN
          OmitTrain := False;
          SetLength(T_DepartureTimesArray, 0);
          SetLength(T_DestinationAreaOrLocationsStrArray, 0);
          SetLength(T_DirectionsArray, 0);
          SetLength(T_NotForPublicUseArray, 0);
          SetLength(T_StoppingArray, 0);

          { See if the diagrams record is one we're supposed to be using, or one that's marked as out-of-use }
          T_TrainActive := FieldByName('TrainActive').AsBoolean;
          T_NonMoving := FieldByName('NonMoving').AsBoolean;
          T_LocoChip := FieldByName('LocoChip').AsInteger;

          IF T_TrainActive
          AND T_NonMoving
          THEN
            ErrorMsg := 'cannot be both active and stationary'
          ELSE BEGIN
            IF T_TrainActive OR T_NonMoving THEN BEGIN
              { Read in the data from the diagrams database }
              T_DoubleHeaderLocoChip := FieldByName('DoubleHeaderLocoChip').AsInteger;

              IF (T_LocoChip = UnknownLocoChip)
              AND T_NonMoving
              THEN

              ELSE
                IF (T_LocoChip < 0) OR (T_LocoChip > 9999) THEN
                  ErrorMsg := 'chip number is ' + IntToStr(T_LocoChip) + ': it must be between 1 and 9999';

              IF ErrorMsg = '' THEN BEGIN
                AppendToDirectionArray(T_DirectionsArray, StrToDirectionType(FieldByName('Direction0').AsString));
                T_StartAreaOrLocationStr := FieldByName('Source').AsString;

                TempSourceLocation := StrToLocation(T_StartAreaOrLocationStr);
                IF TempSourceLocation <> UnknownLocation THEN BEGIN
                  IF (T_DirectionsArray[0] = Up)
                  AND (Locations[TempSourceLocation].Location_LineAtUp <> UnknownLine)
                  AND (Lines[Locations[TempSourceLocation].Location_LineAtUp].Line_NextUpType = EndOfLineIsnext)
                  THEN
                    ErrorMsg := 'line up of source ' + T_StartAreaOrLocationStr + ' is marked as an end of line'
                  ELSE
                    IF (T_DirectionsArray[0] = Down)
                    AND (Locations[TempSourceLocation].Location_LineAtDown <> UnknownLine)
                    AND (Lines[Locations[TempSourceLocation].Location_LineAtDown].Line_NextUpType = EndOfLineIsnext)
                    THEN
                      ErrorMsg := 'line down of source ' + T_StartAreaOrLocationStr + ' is marked as an end of line';
                END;
              END;

              IF ErrorMsg = '' THEN BEGIN
                IF FieldByName('TrainLength').AsString = '' THEN
                  T_TrainLengthInCarriages := 0
                ELSE
                  IF NOT TryStrToInt(FieldByName('TrainLength').AsString, T_TrainLengthInCarriages) THEN
                    ErrorMsg := 'length in carriages ''' + FieldByName('TrainLength').AsString + ''' is not an integer';

                JourneyCount := -1;
                IF NOT T_NonMoving THEN BEGIN
                  { We can work out how many journeys there are from how many destinations there are. Only load MaxJourneys at any one time, though, as that's the number of
                    fields in the database.
                  }
                  NoDestination := False;
                  WHILE (JourneyCount < MaxJourneys)
                  AND NOT NoDestination
                  DO BEGIN
                    IF NOT NoDestination THEN
                      Inc(JourneyCount);
                    IF FieldByName('Destination' + IntToStr(JourneyCount)).AsString = '' THEN BEGIN
                      NoDestination := True;
                      Dec(JourneyCount);
                    END ELSE
                      AppendToStringArray(T_DestinationAreaOrLocationsStrArray, FieldByName('Destination' + IntToStr(JourneyCount)).AsString);
                  END; {WHILE}

                  IF JourneyCount = -1 THEN
                    { treat trains with no journeys as non-moving if they are not already recorded as that }
                    T_NonMoving := True;
                  END;
              END;

              IF ErrorMsg = '' THEN BEGIN
                IF NOT T_NonMoving THEN BEGIN
                  FOR I := 0 TO (JourneyCount - 1) DO
                    { stopping can be true even if we haven't ticked the box, but initialise the array anyway }
                    AppendToBooleanArray(T_StoppingArray, FieldByName('Stopping' + IntToStr(I)).AsBoolean);

                  { although we have to stop at the end of the last journey }
                  AppendToBooleanArray(T_StoppingArray, True);

                  { Deal with trains not for public use (e.g. coming from the sidings) }
                  FOR I := 0 TO JourneyCount DO
                    AppendToBooleanArray(T_NotForPublicUseArray, FieldByName('NotForPublicUse' + IntToStr(I)).AsBoolean);

                  FOR I := 0 TO JourneyCount DO BEGIN
                    { if we've been given a departure time, we have to stop before we can depart }
                    AppendToDateTimeArray(T_DepartureTimesArray, FieldByName('DepartureTime' + IntToStr(I)).AsDateTime);
                    IF (I > 0)
                    AND (T_DepartureTimesArray[I] <> 0)
                    THEN
                      T_StoppingArray[I - 1] := True;
                  END;

                  T_LightsOnTime := FieldByName('LightsOnTime').AsDateTime;

                  T_RepeatUntilTime := FieldByName('RepeatUntilTime').AsDateTime;
                  T_RepeatFrequencyInMinutes := FieldByName('RepeatFrequencyInMinutes').AsInteger;
                  IF (T_RepeatUntilTime <> 0)
                  AND (T_RepeatFrequencyInMinutes = 0)
                  THEN
                    ErrorMsg := 'repeat until time must also have frequency supplied'
                  ELSE
                    IF (T_RepeatUntilTime = 0)
                    AND (T_RepeatFrequencyInMinutes <> 0)
                    THEN
                      ErrorMsg := 'repeat until time must be supplied if frequency is supplied';
                END;

                IF ErrorMsg = '' THEN BEGIN
                  { If we change direction, we have to stop before we can depart }
                  SaveDirection := T_DirectionsArray[0];
                  FOR I := 1 TO JourneyCount DO BEGIN
                    { If there's no direction given, it's the same as the previous one }
                    IF FieldByName('Direction' + IntToStr(I)).AsString = '' THEN
                      AppendToDirectionArray(T_DirectionsArray, SaveDirection)
                    ELSE BEGIN
                      AppendToDirectionArray(T_DirectionsArray, StrToDirectionType(FieldByName('Direction' + IntToStr(I)).AsString));
                      SaveDirection := StrToDirectionType(FieldByName('Direction' + IntToStr(I)).AsString);
                    END;
                    IF T_DirectionsArray[I] <> T_DirectionsArray[I - 1] THEN
                      T_StoppingArray[I - 1] := True;
                  END; {FOR}

                  T_LightsRemainOn := FieldByName('LightsRemainOn').AsBoolean;
                  T_TrainTypeNum := FieldByName('TrainTypeNum').AsInteger;
                  IF (T_TrainTypeNum < 0) OR (T_TrainTypeNum > 12) THEN
                    ErrorMsg := 'train type number is ' + IntToStr(T_TrainTypeNum) + ' but it should be between 0 and 12';

                  T_UserDriving := FieldByName('UserDriving').AsBoolean;
                  T_UserRequiresInstructions := FieldByName('UserRequiresInstructions').AsBoolean;
                END;
              END; { not NonMoving }

              IF ErrorMsg <> '' THEN BEGIN
                IF MessageDialogueWithDefault('Loco ' + LocoChipToStr(T_LocoChip) + ': ' + ErrorMsg
                                              + CRLF
                                              + 'Do you wish to continue loading the Diagrams without it?',
                                              StopTimer, mtError, [mbYes, mbNo], mbYes) = mrNo
                THEN BEGIN
                  Log(LocoChipToStr(T_LocoChip) + ' D ' + ErrorMsg);
                  DiagramsOK := False;
                  Exit;
                END;
              END;

              IF NOT OmitTrain THEN BEGIN
                { If the final journey is just to move the train to a stabling point, and there are repeating journeys, only submit the final journey to
                  CreateTrainDiagramsRecord when the final repeat is requested.
                }
                IF (JourneyCount > 1)
                AND T_NotForPublicUseArray[High(T_NotForPublicUseArray)]
                THEN BEGIN
                  IF (T_RepeatUntilTime <> 0)
                  AND (T_RepeatFrequencyInMinutes <> 0)
                  THEN BEGIN
                    MoveToStablingAfterLastJourney := True;
                    Dec(JourneyCount);

                    SaveLastJourneyDepartureTime := T_DepartureTimesArray[High(T_DepartureTimesArray)];
                    SetLength(T_DepartureTimesArray, Length(T_DepartureTimesArray) - 1);
                    SaveLastJourneyAreaOrDestinationStr := T_DestinationAreaOrLocationsStrArray[High(T_DestinationAreaOrLocationsStrArray)];
                    SetLength(T_DestinationAreaOrLocationsStrArray, Length(T_DestinationAreaOrLocationsStrArray) - 1);
                    SaveLastJourneyDirection := T_DirectionsArray[High(T_DirectionsArray)];
                    SetLength(T_DirectionsArray, Length(T_DirectionsArray) - 1);
                  END;
                END;

                { Now process it to create a train - or a succession, if they are repeats }
                T := CreateTrainDiagramsRecord(T_LocoChip, T_DoubleHeaderLocoChip, JourneyCount, T_DepartureTimesArray, T_LightsOnTime, T_DestinationAreaOrLocationsStrArray,
                                               T_DirectionsArray, T_LightsRemainOn, T_NonMoving, T_NotForPublicUseArray, T_StartAreaOrLocationStr, T_StoppingArray,
                                               T_TrainLengthInCarriages, T_TrainTypeNum, T_UserDriving, T_UserRequiresInstructions, NOT StartOfRepeatJourney);

                IF NOT T_NonMoving
                AND (T_RepeatUntilTime <> 0)
                AND (T_RepeatFrequencyInMinutes <> 0)
                THEN BEGIN
                  { Amend the repeat details if the first station start time is specified and the first journey is not for public use, i.e. is just to reach the first
                    destination - thus what repeats are the subsequent journeys and not the first journey.
                  }
                  IF T_NotForPublicUseArray[0] THEN BEGIN
                    IF (T_DepartureTimesArray[0] = 0)
                    AND (T_DepartureTimesArray[1] <> 0)
                    THEN BEGIN
                      JourneyCount := JourneyCount -1;
                      T_StartAreaOrLocationStr := T_DestinationAreaOrLocationsStrArray[0];

                      { remove the first journey element from the other arrays }
                      FOR I := 0 TO (High(T_DepartureTimesArray) - 1) DO
                        T_DepartureTimesArray[I] := T_DepartureTimesArray[I + 1];
                      SetLength(T_DepartureTimesArray, Length(T_DepartureTimesArray) - 1);

                      FOR I := 0 TO (High(T_DestinationAreaOrLocationsStrArray) - 1) DO
                        T_DestinationAreaOrLocationsStrArray[I] := T_DestinationAreaOrLocationsStrArray[I + 1];
                      SetLength(T_DestinationAreaOrLocationsStrArray, Length(T_DestinationAreaOrLocationsStrArray) - 1);

                      FOR I := 0 TO (High(T_DirectionsArray) - 1) DO
                        T_DirectionsArray[I] := T_DirectionsArray[I + 1];
                      SetLength(T_DirectionsArray, Length(T_DirectionsArray) - 1);

                      FOR I := 0 TO (High(T_NotForPublicUseArray) - 1) DO
                        T_NotForPublicUseArray[I] := T_NotForPublicUseArray[I + 1];
                      SetLength(T_NotForPublicUseArray, Length(T_NotForPublicUseArray) -1);

                      FOR I := 0 TO (High(T_StoppingArray) - 1) DO
                        T_StoppingArray[I] := T_StoppingArray[I + 1];
                      SetLength(T_StoppingArray, Length(T_StoppingArray) - 1);
                    END;
                  END;

                  { See if the repeat is feasible in terms of destinations }
                  IF StrToArea(T_DestinationAreaOrLocationsStrArray[0]) = UnknownArea THEN
                    ErrorMsg := 'cannot create a repeat journey for loco ' + LocoChipToStr(T_LocoChip)
                                + ' as the source area  (' + T_StartAreaOrLocationStr + ') is unknown';

                  IF ErrorMsg = '' THEN BEGIN
                    IF StrToArea(T_DestinationAreaOrLocationsStrArray[High(T_DestinationAreaOrLocationsStrArray)]) = UnknownArea THEN
                      ErrorMsg := 'cannot create a repeat journey for loco ' + LocoChipToStr(T_LocoChip)
                                  + ' as the final destination area (' + T_DestinationAreaOrLocationsStrArray[High(T_DestinationAreaOrLocationsStrArray)] + ') is unknown';
                  END;

                  IF ErrorMsg = '' THEN BEGIN
                    IF T_StartAreaOrLocationStr <> T_DestinationAreaOrLocationsStrArray[High(T_DestinationAreaOrLocationsStrArray)] THEN
                      ErrorMsg := 'cannot create a repeat journey as the source area (' + T_StartAreaOrLocationStr
                                  + ') and final destination area (' + T_DestinationAreaOrLocationsStrArray[High(T_DestinationAreaOrLocationsStrArray)]
                                  + ') are not the same'
                                  + CRLF
                                  + '(which means the repeat train cannot start from where its previous instance stopped)';
                  END;

                  IF ErrorMsg <> '' THEN BEGIN
                    IF MessageDialogueWithDefault('Loco ' + LocoChipToStr(T_LocoChip) + ': ' + ErrorMsg
                                                  + CRLF
                                                  + 'Do you wish to continue loading the diagrams without it?',
                                                  StopTimer, mtError, [mbYes, mbNo], mbYes) = mrNo
                    THEN BEGIN
                      Log(LocoChipToStr(T_LocoChip) + ' D ' + ErrorMsg);
                      DiagramsOK := False;
                      Exit;
                    END;
                  END;

                  IF NOT OmitTrain THEN BEGIN
                    { Now add the repeated journeys }
                    WHILE T_RepeatUntilTime >= IncMinute(T_DepartureTimesArray[0], T_RepeatFrequencyInMinutes) DO BEGIN
                      FOR I := 0 TO High(T_DepartureTimesArray) DO
                        T_DepartureTimesArray[I] := IncMinute(T_DepartureTimesArray[I], T_RepeatFrequencyInMinutes);

                      T := CreateTrainDiagramsRecord(T_LocoChip, T_DoubleHeaderLocoChip, JourneyCount, T_DepartureTimesArray, T_LightsOnTime,
                                                     T_DestinationAreaOrLocationsStrArray, T_DirectionsArray, T_LightsRemainOn, T_NonMoving, T_NotForPublicUseArray,
                                                     T_StartAreaOrLocationStr, T_StoppingArray, T_TrainLengthInCarriages, T_TrainTypeNum, T_UserDriving,
                                                     T_UserRequiresInstructions, StartOfRepeatJourney);
                    END; {WHILE}

                    { And if there's a stored journey, add it now }
                    IF MoveToStablingAfterLastJourney THEN BEGIN
                      JourneyCount := 0;
                      T_StartAreaOrLocationStr := T_DestinationAreaOrLocationsStrArray[High(T_DestinationAreaOrLocationsStrArray)];

                      SetLength(T_DepartureTimesArray, 0);
                      AppendToDateTimeArray(T_DepartureTimesArray, SaveLastJourneyDepartureTime);

                      SetLength(T_DestinationAreaOrLocationsStrArray, 0);
                      AppendToStringArray(T_DestinationAreaOrLocationsStrArray, SaveLastJourneyAreaOrDestinationStr);

                      SetLength(T_NotForPublicUseArray, 0);
                      AppendToBooleanArray(T_NotForPublicUseArray, True);

                      SetLength(T_StoppingArray, 0);
                      AppendToBooleanArray(T_StoppingArray, True);

                      SetLength(T_DirectionsArray, 0);
                      AppendToDirectionArray(T_DirectionsArray, SaveLastJourneyDirection);
                      T := CreateTrainDiagramsRecord(T_LocoChip, T_DoubleHeaderLocoChip, JourneyCount, T_DepartureTimesArray, T_LightsOnTime,
                                                     T_DestinationAreaOrLocationsStrArray, T_DirectionsArray, T_LightsRemainOn, T_NonMoving, T_NotForPublicUseArray,
                                                     T_StartAreaOrLocationStr, T_StoppingArray, T_TrainLengthInCarriages, T_TrainTypeNum, T_UserDriving,
                                                     T_UserRequiresInstructions, NOT StartOfRepeatJourney);
                    END;
                  END;
                END;

                IF DiagramsOK
                AND (T <> NIL)
                THEN BEGIN
                  { Increment the counters }
                  IF T_NonMoving THEN
                    Inc(NonActiveTrainCount)
                  ELSE
                    Inc(ActiveTrainCount);
                END;
              END;
            END;
          END; {WITH}
          DiagramsADOTable.Next;
        END; {WHILE}
      END;

      { Tidy up the database }
      DiagramsADOTable.Close;
      DiagramsADOConnection.Connected := False;
      Log('D Diagrams table and connection closed');

      IF ActiveTrainCount = 0 THEN BEGIN
        IF NonActiveTrainCount = 0 THEN
          Log('D! No active or non-active trains in the supplied diagrams')
        ELSE
          Log('D! Only non-active trains in the supplied diagrams');
      END;
    END; {WITH}

  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ReadInDiagramsFromDatabase: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ReadInDiagramsFromDatabase }

PROCEDURE ClearDiagramsFromDatabase;
{ Empty the diagrams database before writing new records }
BEGIN
  TRY
    WITH DiagramsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + DiagramsFilename + '.' + DiagramsFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Diagrams database file "' + PathToRailDataFiles + DiagramsFilename + '.' + DiagramsFilenameSuffix + '"'
                                      + ' cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'ClearDiagramsFromDatabase')
        ELSE
          Exit;
      END;

      DiagramsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                 + PathToRailDataFiles + DiagramsFilename + '.' + DiagramsFilenameSuffix
                                                 + ';Persist Security Info=False';
      DiagramsADOConnection.Connected := True;

      DiagramsADOTable.Open;
      Log('D Diagrams table and connection opened - deleting existing records');

      DiagramsADOTable.First;
      WHILE NOT DiagramsADOTable.EOF DO
        DiagramsADOTable.Delete;

      { Tidy up the database }
      DiagramsADOTable.Close;
      DiagramsADOConnection.Connected := False;
      Log('D Diagrams table and connection closed');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG ClearDiagramsFromDatabase: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { ClearDiagramsFromDatabase }

PROCEDURE WriteOutDiagramsToDatabase(LocoChip, DoubleHeaderLocoChip, JourneyCount : Integer; UserSpecifiedDepartureTimesArray : DateTimeArrayType;
                                           LightsOnTime : TDateTime; EndLocationsStrArray : StringArrayType; DirectionsArray : DirectionArrayType; LightsRemainOn : Boolean;
                                           TrainNonMoving : Boolean; NotForPublicUseArray : BooleanArrayType; StartLocationStr : String; StoppingArray : BooleanArrayType;
                                           LengthOfTrainInCarriages : Integer; TypeOfTrainNum : Integer;
                                           UserDriving, UserRequiresInstructions, StartOfRepeatJourney : Boolean);
{ Write out the supplied diagrams }
VAR
  I : Integer;
  TempArea : Integer;
  TempLocation : Integer;
  TempStr : String;

BEGIN
  TRY
    WITH DiagramsWindow DO BEGIN
      IF NOT FileExists(PathToRailDataFiles + DiagramsFilename + '.' + DiagramsFilenameSuffix) THEN BEGIN
        IF MessageDialogueWithDefault('Diagrams database file "' + PathToRailDataFiles + DiagramsFilename + '.' + DiagramsFilenameSuffix + '"'
                                      + ' cannot be located'
                                      + CRLF
                                      + 'Do you wish to continue?',
                                      StopTimer, mtConfirmation, [mbYes, mbNo], mbNo) = mrNo
        THEN
          ShutDownProgram(UnitRef, 'WriteOutDiagramsToDatabase')
        ELSE
          Exit;
      END;

      DiagramsADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0; Data Source='
                                                 + PathToRailDataFiles + DiagramsFilename + '.' + DiagramsFilenameSuffix
                                                 + ';Persist Security Info=False';
      DiagramsADOConnection.Connected := True;

      DiagramsADOTable.Open;
      Log('D Diagrams table and connection opened - writing out the diagrams');

      WITH DiagramsADOTable DO BEGIN
        Insert;

        FieldByName('TrainActive').AsBoolean := True;

        FieldByName('NonMoving').AsBoolean := TrainNonMoving;

        FieldByName('LocoChip').AsInteger := LocoChip;

        IF DoubleHeaderLocoChip = UnknownLocoChip THEN
          FieldByName('DoubleHeaderLocoChip').AsInteger := 0
        ELSE
          FieldByName('DoubleHeaderLocoChip').AsInteger := DoubleHeaderLocoChip;

        FieldByName('TrainLength').AsInteger := LengthOfTrainInCarriages;

        FieldByName('TrainTypeNum').AsInteger := TypeOfTrainNum;

        TempStr := '';
        TempArea := StrToArea(StartLocationStr);
        IF TempArea <> UnknownArea THEN
          TempStr := AreaToStr(TempArea, ShortStringType)
        ELSE BEGIN
          TempLocation := StrToLocation(StartLocationStr);
          IF TempLocation <> UnknownArea THEN
            TempStr := LocationToStr(TempLocation, ShortStringType)
          ELSE
            Log('XG Unknown area or location: ' + StartLocationStr);
        END;

        FieldByName('Source').AsString := TempStr;

        I := 0;
        WHILE I <= High(DirectionsArray) DO BEGIN
          IF UserSpecifiedDepartureTimesArray[I] <> 0 THEN
            FieldByName('DepartureTime' + IntToStr(I)).AsDateTime := UserSpecifiedDepartureTimesArray[I];

          IF (I = 0)
          OR ((I > 0)
              AND (DirectionsArray[I] <> DirectionsArray[I - 1]))
          THEN
            FieldByName('Direction' + IntToStr(I)).AsString := DirectionToStr(DirectionsArray[I], VeryShortStringType);

          FieldByName('Stopping' + IntToStr(I)).AsBoolean := StoppingArray[I];

          TempArea := StrToArea(EndLocationsStrArray[I]);
          IF TempArea <> UnknownArea THEN
            TempStr := AreaToStr(TempArea, ShortStringType)
          ELSE BEGIN
            TempLocation := StrToLocation(EndLocationsStrArray[I]);
            IF TempLocation <> UnknownArea THEN
              Debug('Unknown area or location: ' + EndLocationsStrArray[I])
            ELSE
              TempStr := LocationToStr(TempLocation, ShortStringType);
          END;

          TempStr := '';
          TempArea := StrToArea(EndLocationsStrArray[I]);
          IF TempArea <> UnknownArea THEN
            TempStr := AreaToStr(TempArea, ShortStringType)
          ELSE BEGIN
            TempLocation := StrToLocation(EndLocationsStrArray[I]);
            IF TempLocation <> UnknownArea THEN
              TempStr := LocationToStr(TempLocation, ShortStringType)
            ELSE
              Log('XG Unknown area or location: ' + EndLocationsStrArray[I]);
          END;
          FieldByName('Destination' + IntToStr(I)).AsString := TempStr;

          FieldByName('NotForPublicUse' + IntToStr(I)).AsBoolean := NotForPublicUseArray[I];

          Inc(I);
        END; {WHILE}

        IF LightsOnTime <> 0 THEN
          FieldByName('LightsOnTime').AsDateTime := LightsOnTime;

        FieldByName('LightsRemainOn').AsBoolean := LightsRemainOn;

        FieldByName('UserDriving').AsBoolean := UserDriving;

        FieldByName('UserRequiresInstructions').AsBoolean := UserRequiresInstructions;

        { Omit the following two to keep the database cells blank
          FieldByName('RepeatUntilTime').AsDateTime := RepeatUntilTime;
          FieldByName('RepeatFrequencyInMinutes').AsInteger := 0;
        }

        Post;
      END; {WITH}

      { Tidy up the database }
      DiagramsADOTable.Close;
      DiagramsADOConnection.Connected := False;
      Log('D Diagrams table and connection closed');
    END; {WITH}
  EXCEPT {TRY}
    ON E : Exception DO
      Log('EG WriteOutDiagramsToDatabase: ' + E.ClassName +' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WriteOutDiagramsToDatabase }

PROCEDURE CalculatePublicJourneyStartTimes;
{ Calculate the first journey's starting time where it is is marked "not for public use" and there's a supplied departure time for the second journey }
VAR
  JourneyCount : Integer;
  JourneyMinutes : Integer;
  StartTimeChanged : Boolean;
  T : Train;

BEGIN
  StartTimeChanged := False;

  T := TrainList;
  WHILE T <> NIL DO BEGIN
    WITH T^ DO BEGIN
      IF (Train_LocoChip <> UnknownLocoChip)
      AND Train_DiagramFound
      AND (Train_CurrentStatus <> Cancelled)
      AND (Train_CurrentStatus <> NonMoving)
      THEN BEGIN
        IF Length(Train_JourneysArray) > 1 THEN BEGIN
          WITH Train_JourneysArray[0] DO BEGIN
            IF TrainJourney_NotForPublicUse THEN BEGIN
              IF Train_FirstStationSpecifiedStartTime <> 0 THEN BEGIN
                IF NOT StartTimeChanged THEN BEGIN
                  StartTimeChanged := True;
                  Log(Train_LocoChipStr + ' D CALCULATING PUBLIC JOURNEY START TIMES:');
                END;

                { Allow time to get to the specified station departure, and time to get the passengers on board }
                JourneyMinutes := CalculateJourneyTimeInMinutes(Train_TypeNum, TrainJourney_LengthInInches);
                Log(Train_LocoChipStr + ' L ' + DisplayJourneyNumber(0)
                                      + 'time from ' + AreaToStr(TrainJourney_StartArea)
                                      + ' to ' + AreaToStr(TrainJourney_EndArea)
                                      + ' is ' + IntToStr(JourneyMinutes)
                                      + IfThen(JourneyMinutes = 1,
                                               ' minute',
                                               ' minutes')
                                      + ' (length=' + FloatToStr(TrainJourney_LengthInInches) + ')');
                TrainJourney_CurrentDepartureTime := IncMinute(Train_FirstStationSpecifiedStartTime, -JourneyMinutes - (StationStartOfDayPassengerBoardingTimeInMinutes));
              END;
            END;
          END; {WITH}
        END;
      END;

      IF Train_FirstStationSpecifiedStartTime <> 0 THEN BEGIN
        DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);

        Log(Train_LocoChipStr + ' D List of journeys including journey 0 which has had new a start time applied:' + ' {LINE=BEFORE}');
        FOR JourneyCount:= 0 TO High(Train_JourneysArray) DO
          Log(Train_LocoChipStr + ' D    J=' + IntToStr(JourneyCount) + ': ' + DescribeJourney(T, JourneyCount) + ' {NOUNITREF}');

        DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);
      END;
      T := Train_NextRecord;
    END; {WITH}
  END; {WHILE}
END; { CalculatePublicJourneyStartTimes }

PROCEDURE RecalculateJourneyTimes(T : Train; ExplanatoryStr : String);
{ Recalculate the journey times to allow for any inserted journeys or other changes }
VAR
  ArrivalTime : TDateTime;
  DepartureTime : TDateTime;
  EndLocationStr : String;
  EndOfDayWaitingMinutes : Integer;
  FirstStartLocationStr : String;
  JourneyCount : Integer;
  LogStr : String;
  NextStartLocationStr : String;
  OtherT : Train;
  OtherTrainFound : Boolean;
  OtherTrainJourneyCount : Integer;
  SaveLogStr : String;
  TotalWaitingMinutes : Integer;

  PROCEDURE CheckIfTrainsAreDepartingAtTheSameTime(T : Train);
  { If any other trains are departing at the same time in the same direction with the same destination, allow time for each to depart }
  BEGIN
    WITH T^ DO BEGIN
      WITH Train_JourneysArray[JourneyCount] DO BEGIN
        OtherT := TrainList;
        OtherTrainFound := False;
        WHILE (OtherT <> NIL)
        AND NOT OtherTrainFound
        DO BEGIN
          IF (OtherT <> T)
          AND OtherT^.Train_DiagramFound
          AND (OtherT^.Train_CurrentStatus <> Cancelled)
          AND (OtherT^.Train_CurrentStatus <> Suspended)
          AND (OtherT^.Train_CurrentStatus <> MissingAndSuspended)
          THEN BEGIN
            OtherTrainJourneyCount := 0;
            WHILE OtherTrainJourneyCount <= High(OtherT^.Train_JourneysArray) DO BEGIN
              IF (OtherTrainJourneyCount = 0)
              OR (OtherT^.Train_JourneysArray[OtherTrainJourneyCount - 1].TrainJourney_StoppingOnArrival)
              THEN BEGIN
                IF (JourneyCount = 0)
                OR (T^.Train_JourneysArray[JourneyCount - 1].TrainJourney_StoppingOnArrival)
                THEN BEGIN
                  IF NOT OtherT^.Train_JourneysArray[OtherTrainJourneyCount].TrainJourney_Cleared THEN BEGIN
                    IF OtherT^.Train_JourneysArray[OtherTrainJourneyCount].TrainJourney_StartArea = TrainJourney_StartArea THEN BEGIN
                      IF OtherT^.Train_JourneysArray[OtherTrainJourneyCount].TrainJourney_Direction = TrainJourney_Direction THEN BEGIN
                        IF OtherT^.Train_JourneysArray[OtherTrainJourneyCount].TrainJourney_EndStationName = TrainJourney_EndStationName
                        THEN BEGIN
                          IF OtherT^.Train_JourneysArray[OtherTrainJourneyCount].TrainJourney_CurrentDepartureTime = TrainJourney_CurrentDepartureTime
                          THEN BEGIN
                            TrainJourney_CurrentDepartureTime := IncMinute(TrainJourney_CurrentDepartureTime, 2);
                            Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyCount)
                                                  + 'current departure time retimed to '
                                                  + TimeToHMSStr(TrainJourney_CurrentDepartureTime)
                                                  + ' as ' + LocoChipToStr(OtherT^.Train_LocoChip)
                                                  + ' J=' + IntToStr(OtherTrainJourneyCount)
                                                  + ' was also timed to leave ' + AreaToStr(TrainJourney_StartArea)
                                                  + ' going ' + DirectionToStr(TrainJourney_Direction)
                                                  + ' to ' + TrainJourney_EndStationName
                                                  + ' at '
                                                  + TimeToHMSStr(OtherT^.Train_JourneysArray[OtherTrainJourneyCount].TrainJourney_CurrentDepartureTime));
                          END;
                        END;
                      END;
                    END;
                  END;
                END;
              END;
              Inc(OtherTrainJourneyCount);
            END; {WHILE}
          END;
          OtherT := OtherT^.Train_NextRecord;
        END; {WHILE}
      END; {WITH}
    END; {WITH}
  END; { CheckIfTrainsAreDepartingAtTheSameTime }

BEGIN
  WITH T^ DO BEGIN
    Log(Train_LocoChipStr + ' D RECALCULATING JOURNEY TIMES ' + ExplanatoryStr);
    NextStartLocationStr := '';

    JourneyCount := High(Train_JourneysArray);
    JourneyCount := 0;
    WHILE JourneyCount <= High(Train_JourneysArray) DO BEGIN
      WITH Train_JourneysArray[JourneyCount] DO BEGIN
        FirstStartLocationStr := LocationToStr(TrainJourney_StartLocation);
        IF JourneyCount < High(Train_JourneysArray) THEN
          NextStartLocationStr := LocationToStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_StartLocation);
        IF TrainJourney_ActualDepartureTime <> 0 THEN BEGIN
          DepartureTime := TrainJourney_ActualDepartureTime;
          LogStr := DisplayJourneyNumber(JourneyCount) + 'actual departure time from ' + FirstStartLocationStr + ' is ' + TimeToHMSStr(DepartureTime);
          { avoid writing the same departure time out twice }
          IF Pos(LogStr, SaveLogStr) = 0 THEN
            Log(Train_LocoChipStr + ' D ' + LogStr + ' {NOUNITREF}');
        END ELSE BEGIN
          IF TrainJourney_CurrentDepartureTime = 0 THEN
            TrainJourney_CurrentDepartureTime := TrainJourney_DiagrammedDepartureTime
          ELSE
            CheckIfTrainsAreDepartingAtTheSameTime(T);

          DepartureTime := TrainJourney_CurrentDepartureTime;
          LogStr := DisplayJourneyNumber(JourneyCount) + 'current departure time from ' + FirstStartLocationStr + ' is ' + TimeToHMSStr(DepartureTime);
          { avoid writing the same departure time out twice }
          IF Pos(LogStr, SaveLogStr) = 0 THEN
            Log(Train_LocoChipStr + ' D ' + LogStr + ' {NOUNITREF}');
        END;
        TrainJourney_CurrentDepartureTime := DepartureTime;
        EndLocationStr := LocationToStr(TrainJourney_EndLocation);

        IF TrainJourney_ActualArrivalTime <> 0 THEN BEGIN
          ArrivalTime := TrainJourney_ActualArrivalTime;
          LogStr := DisplayJourneyNumber(JourneyCount) + 'actual arrival time at ' + EndLocationStr + ' is ' + TimeToHMSStr(ArrivalTime);
          { avoid writing the same arrival time out twice }
          IF Pos(LogStr, SaveLogStr) = 0 THEN
            Log(Train_LocoChipStr + ' D ' + LogStr + ' {NOUNITREF}');
        END ELSE BEGIN
          ArrivalTime := IncMinute(DepartureTime, TrainJourney_DurationInMinutes);
          LogStr := DisplayJourneyNumber(JourneyCount) + 'current arrival time at ' + EndLocationStr + ' is ' + TimeToHMSStr(ArrivalTime);
          { avoid writing the same arrival time out twice }
          IF Pos(LogStr, SaveLogStr) = 0 THEN
            Log(Train_LocoChipStr + ' D ' + LogStr + ' {NOUNITREF}');
        END;
        TrainJourney_CurrentArrivalTime := ArrivalTime;

        IF NOT TrainJourney_StoppingOnArrival THEN BEGIN
          { the train isn't stopping, so the departure time is the same as the arrival time }
          IF JourneyCount < Train_TotalJourneys THEN BEGIN
            Train_JourneysArray[JourneyCount + 1].TrainJourney_CurrentDepartureTime := ArrivalTime;
            SaveLogStr := DisplayJourneyNumber(JourneyCount + 1)
                          + 'current departure time from ' + NextStartLocationStr + ' is '
                          + TimeToHMSStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_CurrentDepartureTime)
                          + ' as train does not stop at the end of the previous journey';
            Log(Train_LocoChipStr + ' D ' + SaveLogStr + ' {NOUNITREF}');
          END;
        END ELSE BEGIN
          { The train is stopping on arrival }
          IF JourneyCount < Train_TotalJourneys THEN BEGIN
            IF TrainJourney_Direction = Train_JourneysArray[JourneyCount + 1].TrainJourney_Direction THEN BEGIN
              IF CompareTime(IncMinute(ArrivalTime, StationSameDirectionExitMinimumWaitTimeInMinutes),
                             Train_JourneysArray[JourneyCount + 1].TrainJourney_DiagrammedDepartureTime) <= 0
              THEN BEGIN
                Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyCount + 1)
                                      + 'not amending departure time from ' + NextStartLocationStr + ' to be '
                                      + IntToStr(StationSameDirectionExitMinimumWaitTimeInMinutes) + ' minutes'
                                      + ' after arrival at ' + TimeToHMSStr(ArrivalTime) + ' '
                                      + 'as train would leave either before or at the same time as the diagrammed departure time of '
                                      + TimeToHMSStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_DiagrammedDepartureTime)
                                      + ' {NOUNITREF}');
                Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyCount + 1)
                                      + 'setting departure time to be the diagrammed departure time of '
                                      + TimeToHMSStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_DiagrammedDepartureTime)
                                      + ' instead'
                                      + ' {NOUNITREF}');
                Train_JourneysArray[JourneyCount + 1].TrainJourney_CurrentDepartureTime := Train_JourneysArray[JourneyCount + 1].TrainJourney_DiagrammedDepartureTime;
              END ELSE BEGIN
                TotalWaitingMinutes := StationSameDirectionExitMinimumWaitTimeInMinutes + TrainJourney_AdditionalRequiredStationWaitInMinutes;
                EndOfDayWaitingMinutes := 0;
                IF (JourneyCount + 1 = Train_TotalJourneys)
                AND (Train_JourneysArray[JourneyCount + 1].TrainJourney_NotForPublicUse)
                THEN BEGIN
                  { add end of day minutes if necessary }
                  IF TotalWaitingMinutes < StationEndOfDayPassengerLeavingTimeInMinutes THEN BEGIN
                    EndOfDayWaitingMinutes := StationEndOfDayPassengerLeavingTimeInMinutes - TotalWaitingMinutes;
                    TotalWaitingMinutes := StationEndOfDayPassengerLeavingTimeInMinutes;
                  END;
                END;

                Train_JourneysArray[JourneyCount + 1].TrainJourney_CurrentDepartureTime := IncMinute(ArrivalTime, TotalWaitingMinutes);
                SaveLogStr := DisplayJourneyNumber(JourneyCount + 1)
                              + 'current departure time from ' + NextStartLocationStr + ' is '
                              + TimeToHMSStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_CurrentDepartureTime)
                              + IfThen(TrainJourney_AdditionalRequiredStationWaitInMinutes <> 0,
                                       ' (including ' + IntToStr(TrainJourney_AdditionalRequiredStationWaitInMinutes) + ' mins additional wait time)')
                              + ' as train stops but does not change direction after the previous journey'
                              + IfThen(EndOfDayWaitingMinutes <> 0,
                                       ' (including ' + IntToStr(EndOfDayWaitingMinutes) + ' mins end of day wait time)');
                Log(Train_LocoChipStr + ' D ' + SaveLogStr + ' {NOUNITREF}');
              END;
            END ELSE BEGIN
              IF CompareTime(IncMinute(ArrivalTime, StationOppositeDirectionExitMinimumWaitTimeInMinutes),
                             Train_JourneysArray[JourneyCount + 1].TrainJourney_DiagrammedDepartureTime) <= 0
              THEN BEGIN
                Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyCount + 1)
                                      + 'not amending departure time from ' + NextStartLocationStr + ' to be '
                                      + IntToStr(StationOppositeDirectionExitMinimumWaitTimeInMinutes) + ' minutes'
                                      + ' after arrival at ' + TimeToHMSStr(ArrivalTime)
                                      + ' as the train would leave earlier than the diagrammed departure time of '
                                      + TimeToHMSStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_DiagrammedDepartureTime)
                                      + ' {NOUNITREF}');
                Log(Train_LocoChipStr + ' D ' + DisplayJourneyNumber(JourneyCount + 1)
                                      + 'setting departure time to be the diagrammed departure time of '
                                      + TimeToHMSStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_DiagrammedDepartureTime)
                                      + ' instead'
                                      + ' {NOUNITREF}');
                Train_JourneysArray[JourneyCount + 1].TrainJourney_CurrentDepartureTime := Train_JourneysArray[JourneyCount + 1].TrainJourney_DiagrammedDepartureTime;
              END ELSE BEGIN
                TotalWaitingMinutes := StationOppositeDirectionExitMinimumWaitTimeInMinutes + TrainJourney_AdditionalRequiredStationWaitInMinutes;

                EndOfDayWaitingMinutes := 0;
                IF (JourneyCount + 1 = Train_TotalJourneys)
                AND (Train_JourneysArray[JourneyCount + 1].TrainJourney_NotForPublicUse)
                THEN BEGIN
                  { add end of day minutes if necessary }
                  IF TotalWaitingMinutes < StationEndOfDayPassengerLeavingTimeInMinutes THEN BEGIN
                    EndOfDayWaitingMinutes := StationEndOfDayPassengerLeavingTimeInMinutes - TotalWaitingMinutes;
                    TotalWaitingMinutes := StationEndOfDayPassengerLeavingTimeInMinutes;
                  END;
                END;

                Train_JourneysArray[JourneyCount + 1].TrainJourney_CurrentDepartureTime := IncMinute(ArrivalTime, TotalWaitingMinutes);
                SaveLogStr := DisplayJourneyNumber(JourneyCount + 1)
                              + 'current departure time from ' + NextStartLocationStr + ' is '
                              + TimeToHMSStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_CurrentDepartureTime)
                              + IfThen(TrainJourney_AdditionalRequiredStationWaitInMinutes <> 0,
                                       ' (including ' + IntToStr(TrainJourney_AdditionalRequiredStationWaitInMinutes) + ' mins additional wait time)')
                              + ' as train stops and changes direction after the previous journey'
                              + IfThen(EndOfDayWaitingMinutes <> 0,
                                       ' (including ' + IntToStr(EndOfDayWaitingMinutes) + ' mins end of day wait time)');
                Log(Train_LocoChipStr + ' D ' + SaveLogStr + ' {NOUNITREF}');
              END;
            END;

          END;
        END;
      END; {WITH}
      Inc(JourneyCount);
    END; {WHILE}
  END; {WITH}
END; { RecalculateJourneyTimes }

PROCEDURE AddStationNamesToJourneys;
{ Now add station names to each journey. This is more complicated than one might expect, as one doesn't want the names of intermediate, non-stopping, areas in the
  timetable.
}
VAR
  JourneyCount : Integer;
  SaveStartStationName : String;
  SaveEndStationName : String;
  T : Train;

BEGIN
  T := TrainList;
  WHILE T <> NIL DO BEGIN
    WITH T^ DO BEGIN
      IF (Train_LocoChip <> UnknownLocoChip)
      AND Train_DiagramFound
      AND (Train_CurrentStatus <> Cancelled)
      AND (Train_CurrentStatus <> NonMoving)
      THEN BEGIN
        FOR JourneyCount:= 0 TO High(Train_JourneysArray) DO BEGIN
          WITH Train_JourneysArray[JourneyCount] DO BEGIN
            IF JourneyCount = 0 THEN BEGIN
              TrainJourney_StartStationName := GetStationNameFromArea(Train_JourneysArray[JourneyCount].TrainJourney_StartArea);
              SaveStartStationName := TrainJourney_StartStationName;
            END ELSE
              IF T^.Train_JourneysArray[JourneyCount -1].TrainJourney_StoppingOnArrival THEN BEGIN
                TrainJourney_StartStationName := GetStationNameFromArea(Train_JourneysArray[JourneyCount - 1].TrainJourney_EndArea);
                SaveStartStationName := TrainJourney_StartStationName
              END ELSE
                TrainJourney_StartStationName := SaveStartStationName;
          END; {WITH}
        END; {FOR}

        FOR JourneyCount:= High(Train_JourneysArray) DOWNTO 0 DO BEGIN
          WITH Train_JourneysArray[JourneyCount] DO BEGIN
            IF JourneyCount = High(Train_JourneysArray) THEN BEGIN
              TrainJourney_EndStationName := GetStationNameFromArea(Train_JourneysArray[JourneyCount].TrainJourney_EndArea);
              SaveEndStationName := TrainJourney_EndStationName
            END ELSE
              IF T^.Train_JourneysArray[JourneyCount].TrainJourney_StoppingOnArrival THEN BEGIN
                TrainJourney_EndStationName := GetStationNameFromArea(Train_JourneysArray[JourneyCount].TrainJourney_EndArea);
                SaveEndStationName := TrainJourney_EndStationName
              END ELSE
                TrainJourney_EndStationName := SaveEndStationName;
          END; {WITH}
        END; {FOR}

        Log(Train_LocoChipStr + ' D List of fully revised journeys, timetable and station names:');
        FOR JourneyCount:= 0 TO High(Train_JourneysArray) DO
          Log(Train_LocoChipStr + ' D    J=' + IntToStr(JourneyCount) + ': ' + DescribeJourney(T, JourneyCount) + ' {NOUNITREF}');

        DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);
      END; {WITH}
      T := Train_NextRecord;
    END; {WHILE}
  END;
END; { AddStationNamesToJourneys }

PROCEDURE SplitJourneys(OUT ErrorMsg : String; OUT DiagramsOK : Boolean);
{ Where there is a route hold marker at a signal, split journey into two - in case the destination for the second half of the journey is not available when the route is
  set up - the route will then be created in two halves, the second half created only when an alternative destination is available. This can only be undertaken now, as we
  need the exact source and destination lines to work out the route.
}
CONST
  DiagramsLoading = True;
  EmergencyRouteing = True;
  NewJourney = True;
  NoTrainJourneySpecifiedDepartureTime = 0;
  RebuildRouteArray = True;
  StartOfRepeatJourney = True;
  StoppingOnArrival = True;

VAR
  I, J : Integer;
  JourneyArrayCount : Integer;
  JourneyCount : Integer;
  LinesNotAvailableStr : String;
  NewJourneyRec : TrainJourneyRec;
  NewJourneyRouteArray : StringArrayType;
  NewJourneyStartArea : Integer;
  NewJourneyStartLine : Integer;
  NewJourneyStartLocation : Integer;
  OldJourneyRouteArray : StringArrayType;
  S : Integer;
  SaveS : Integer;
  SignalFound : Boolean;
  T : Train;

BEGIN
  IF DiagramsOK THEN BEGIN
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
          DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);
          Log(Train_LocoChipStr + ' D INSERTING ADDITIONAL JOURNEYS:');

          Log(Train_LocoChipStr + ' D Initial journeys:');
          FOR JourneyCount := 0 TO High(Train_JourneysArray) DO BEGIN
            Log(Train_LocoChipStr + ' D J=' + IntToStr(JourneyCount) + ': ' + DescribeJourney(T, JourneyCount));
            IF Length(Train_JourneysArray[JourneyCount].TrainJourney_RouteArray) = 0 THEN BEGIN
              Log(Train_LocoChipStr + ' X! Serious error in splitting journey: TrainJourney_RouteArray for J=' + IntToStr(JourneyCount) + ' is empty - train cancelled');
              ChangeTrainStatus(T, Cancelled);
            END ELSE
              WriteStringArrayToLog(Train_LocoChip, 'R', 'J=' + IntToStr(JourneyCount)
                                                         + ': Train Journey Draft Route Array to set up'
                                                         + ' from ' + LineToStr(Train_JourneysArray[JourneyCount].TrainJourney_StartLine)
                                                         + ' to ' + LineToStr(Train_JourneysArray[JourneyCount].TrainJourney_EndLine)
                                                         + ': ',
                                                         Train_JourneysArray[JourneyCount].TrainJourney_RouteArray,
                                                         2, 190, 'SR=');
          END;

          JourneyCount := 0;
          SignalFound := False;
          IF Train_CurrentStatus <> Cancelled THEN BEGIN
            WHILE (JourneyCount <= High(Train_JourneysArray))
            AND DiagramsOK
            DO BEGIN
              JourneyArrayCount := 0;
              S := UnknownSignal;
              WHILE JourneyArrayCount <= High(Train_JourneysArray[JourneyCount].TrainJourney_RouteArray) DO BEGIN
                WITH Train_JourneysArray[JourneyCount] DO BEGIN
                  IF TrainJourney_RouteArray[JourneyArrayCount] = HoldMarker THEN BEGIN
                    { there is a hold marker, so we need to split the journey in two }
                    SaveS := S;
                    S := ExtractSignalFromString(TrainJourney_RouteArray[JourneyArrayCount + 1]);
                    IF S <> UnknownSignal THEN BEGIN
                      IF Signals[S].Signal_AdjacentLine = TrainJourney_StartLine THEN
                        Log(Train_LocoChipStr + ' D J=' + IntToStr(JourneyCount) + ': not inserting a new journey at hold marker before S=' + IntToStr(S)
                                              + ' (L=' + LineToStr(Signals[S].Signal_AdjacentLine) + ') as a journey already starts there')
                      ELSE
                        IF (SaveS <> UnknownSignal)
                        AND ((Locations[Lines[Signals[S].Signal_AdjacentLine].Line_Location].Location_PlatformOrFiddleyardAtUp =
                                                                                                                    Lines[Signals[SaveS].Signal_AdjacentLine].Line_Location)
                            OR (Locations[Lines[Signals[S].Signal_AdjacentLine].Line_Location].Location_PlatformOrFiddleyardAtDown =
                                                                                                                   Lines[Signals[SaveS].Signal_AdjacentLine].Line_Location))
                        THEN
                          Log(Train_LocoChipStr + ' D J=' + IntToStr(JourneyCount) + ': not inserting a new journey at hold marker before S=' + IntToStr(S)
                                                + ' (L=' + LineToStr(Signals[S].Signal_AdjacentLine)
                                                + ') as there is a hold marker at the previous platform signal S=' + IntToStr(SaveS)
                                                + ' (L=' + LineToStr(Signals[SaveS].Signal_AdjacentLine) + ')')
                        ELSE
                          IF (Locations[Lines[Signals[S].Signal_AdjacentLine].Line_Location].Location_PlatformOrFiddleyardAtUp = TrainJourney_EndLocation)
                          OR (Locations[Lines[Signals[S].Signal_AdjacentLine].Line_Location].Location_PlatformOrFiddleyardAtDown = TrainJourney_EndLocation)
                          THEN
                            Log(Train_LocoChipStr + ' D J=' + IntToStr(JourneyCount) + ': not inserting a new journey at hold marker before S=' + IntToStr(S)
                                                  + ' (L=' + LineToStr(Signals[S].Signal_AdjacentLine) + ') as the adjoining platform/fiddleyard is the journey end'
                                                  + ' (L=' + LineToStr(TrainJourney_EndLine)
                                                  + ')')
                          ELSE BEGIN
                            { insert a new journey at the right place in the array }
                            InsertElementInTrainJourneyRecArray(Train_JourneysArray, JourneyCount + 1, NewJourneyRec);
                            { we can't leave the diagrammed locations to CreateJourney, as on principle it would not alter them }
                            Train_JourneysArray[JourneyCount + 1].TrainJourney_DiagrammedStartLocation := UnknownLocation;
                            Train_JourneysArray[JourneyCount + 1].TrainJourney_DiagrammedEndLocation := UnknownLocation;

                            WITH Train_JourneysArray[JourneyCount] DO BEGIN
                              { now we can amend the new journey }
                              Log(Train_LocoChipStr + ' D New J=' + IntToStr(JourneyCount + 1)
                                                    + ' to be inserted as hold marker found before S=' + IntToStr(S)
                                                    + ' (L=' + LineToStr(Signals[S].Signal_AdjacentLine) + ')');

                              NewJourneyStartArea := Locations[Lines[Signals[S].Signal_AdjacentLine].Line_Location].Location_Area;
                              NewJourneyStartLocation := Lines[Signals[S].Signal_AdjacentLine].Line_Location;
                              NewJourneyStartLine := Signals[S].Signal_AdjacentLine;
                              SetLength(OldJourneyRouteArray, 0);
                              SetLength(NewJourneyRouteArray, 0);
                              { store the data up to the hold marker }
                              FOR I := 0 TO (JourneyArrayCount - 2) DO
                                AppendToStringArray(OldJourneyRouteArray, Train_JourneysArray[Journeycount].TrainJourney_RouteArray[I]);

                              { and use the rest if the data for the new journey }
                              FOR I := (JourneyArrayCount - 1) TO High(TrainJourney_RouteArray) DO
                                AppendToStringArray(NewJourneyRouteArray, Train_JourneysArray[Journeycount].TrainJourney_RouteArray[I]);

                              { and now insert it after the original journey }
                              CreateJourney(T, JourneyCount + 1, NewJourney,
                                            NewJourneyStartArea, TrainJourney_EndArea,
                                            NewJourneyStartLocation, TrainJourney_EndLocation,
                                            TrainJourney_DiagrammedStartLocation, TrainJourney_DiagrammedEndLocation,
                                            NewJourneyStartLine, UnknownLine,
                                            0, NoTrainJourneySpecifiedDepartureTime,
                                            TrainJourney_DiagrammedArrivalTime,
                                            TrainJourney_Direction,
                                            NewJourneyRouteArray,
                                            NOT RebuildRouteArray,
                                            TrainJourney_StoppingOnArrival,
                                            TrainJourney_NotForPublicUse,
                                            NOT EmergencyRouteing,
                                            NOT StartOfRepeatJourney,
                                            DiagramsLoading,
                                            ErrorMsg, LinesNotAvailableStr,
                                            DiagramsOK);
                              IF NOT DiagramsOK THEN
                                Log(Train_LocoChipStr + ' D Error in inserting new J=' + IntToStr(JourneyCount + 1) + ': ' + ErrorMsg)
                              ELSE BEGIN
                                { and curtail the original journey }
                                CreateJourney(T, JourneyCount, NOT NewJourney,
                                              TrainJourney_StartArea, NewJourneyStartArea,
                                              TrainJourney_StartLocation, NewJourneyStartLocation,
                                              TrainJourney_DiagrammedStartLocation, TrainJourney_DiagrammedEndLocation,
                                              TrainJourney_StartLine, NewJourneyStartLine,
                                              TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime,
                                              IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                              TrainJourney_Direction,
                                              OldJourneyRouteArray,
                                              NOT RebuildRouteArray,
                                              NOT StoppingOnArrival,
                                              TrainJourney_NotForPublicUse,
                                              NOT EmergencyRouteing,
                                              TrainJourney_StartOfRepeatJourney,
                                              DiagramsLoading,
                                              ErrorMsg, LinesNotAvailableStr,
                                              DiagramsOK);

                                IF DiagramsOK THEN BEGIN
                                  Log(Train_LocoChipStr + ' D Journeys now amended to include new journey ' + IntToStr(JourneyCount + 1)
                                                        + ' from ' +  LocationToStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_StartLocation, ShortStringType)
                                                        + ' to ' +  LocationToStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_EndLocation, ShortStringType));
                                  IF JourneyCount < (High(Train_JourneysArray) - 1) THEN BEGIN
                                    Log(Train_LocoChipStr + ' D List of journeys being revised:');
                                    FOR J := 0 TO High(Train_JourneysArray) DO
                                      Log('D J=' + IntToStr(J) + ': ' + DescribeJourney(T, J) + ' {NOUNITREF}');
                                  END;
                                  Inc(JourneyCount);

                                  { reset the array count as we're now looking at a different journey }
                                  JourneyArrayCount := -1;
                                  DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);
                                END ELSE BEGIN
                                  Log(Train_LocoChipStr + ' D Error in amending J=' + IntToStr(JourneyCount) + ': ' + ErrorMsg + ' - trying emergency routeing');
                                  { try emergency routeing in curtailing the original journey }
                                  CreateJourney(T, JourneyCount, NOT NewJourney,
                                                TrainJourney_StartArea, NewJourneyRec.TrainJourney_StartArea,
                                                TrainJourney_StartLocation, Lines[Signals[S].Signal_AdjacentLine].Line_Location,
                                                TrainJourney_DiagrammedStartLocation, TrainJourney_DiagrammedEndLocation,
                                                TrainJourney_StartLine, Signals[S].Signal_AdjacentLine,
                                                TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime,
                                                IncMinute(TrainJourney_CurrentArrivalTime, -1),
                                                TrainJourney_Direction,
                                                TrainJourney_RouteArray,
                                                NOT RebuildRouteArray,
                                                NOT StoppingOnArrival,
                                                TrainJourney_NotForPublicUse,
                                                EmergencyRouteing,
                                                TrainJourney_StartOfRepeatJourney,
                                                DiagramsLoading,
                                                ErrorMsg,
                                                LinesNotAvailableStr, DiagramsOK);
                                  IF NOT DiagramsOK THEN BEGIN
                                    Log(Train_LocoChipStr + ' D Error in amending J=' + IntToStr(JourneyCount) + ': ' + ErrorMsg + ' even using emergency routeing');
                                    CancelTrain(T, NOT ByUser, TrainExists);
                                    DiagramsOK := True;
                                  END ELSE BEGIN
                                    Log(Train_LocoChipStr + ' D Journeys now amended (using emergency routeing) to include new journey ' + IntToStr(JourneyCount + 1)
                                                          + ' from ' +  LocationToStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_StartLocation, ShortStringType)
                                                          + ' to ' +  LocationToStr(Train_JourneysArray[JourneyCount + 1].TrainJourney_EndLocation, ShortStringType));
                                    IF JourneyCount < (High(Train_JourneysArray) - 1) THEN BEGIN
                                      Log(Train_LocoChipStr + ' D List of journeys being revised:');
                                      FOR J := 0 TO High(Train_JourneysArray) DO
                                        Log('D J=' + IntToStr(J) + ': ' + DescribeJourney(T, J) + ' {NOUNITREF}');
                                    END;
                                    Inc(JourneyCount);

                                    { reset the array count as we're now looking at a different journey }
                                    JourneyArrayCount := -1;
                                    DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);
                                  END;
                                END;
                              END;
                            END; {WITH}
                          END;
                    END;
                  END;
                END;
                Inc(JourneyArrayCount);
              END; {WHILE}

              Inc(JourneyCount);
              Train_TotalJourneys := JourneyCount - 1;
            END; {WHILE}
          END;

          IF SignalFound THEN
            Train_TotalJourneys := JourneyCount - 1;

          Log(Train_LocoChipStr + ' D List of fully revised journeys:');
          FOR J := 0 TO High(Train_JourneysArray) DO
            Log(Train_LocoChipStr + ' D    J=' + IntToStr(J) + ': ' + DescribeJourney(T, J) + ' {NOUNITREF}');

          DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);
        END;
      END; {WITH}
      T := T^.Train_NextRecord;
    END; {WHILE}
  END;
END; { SplitJourneys }

PROCEDURE ProcessDiagrams(OUT ErrorMsg : String; OUT DiagramsOK : Boolean);
{ Once read in, process the diagrams }
CONST
  DiagramsLoading = True;
  EmergencyRouteing = True;
  IncludeOutOfUseLines = True;
  Indent = True;
  NoJourneyName = '';
  NoOldHeadcode = '';
  PreRouteing = True;
  StartOfRepeatJourney = True;
  TimetableIsloading = True;
  UserDriving = True;
  UserRequiresInstructions = True;

VAR
  DepartureTimeArray : DateTimeArrayType;
  DirectionsArray : DirectionArrayType;
  EndLocationsStrArray : StringArrayType;
  JourneyCount : Integer;
  LengthOfTrainInCarriages : Integer;
  LightsOnTime : TDateTime;
  LightsRemainOn : Boolean;
  LinesNotAvailableStr : String;
  NotForPublicUseArray : BooleanArrayType;
  StartLocationStr : String;
  StoppingArray : BooleanArrayType;
  T : Train;
  TrainNonMoving : Boolean;
  TypeOfTrainNum : Integer;

BEGIN
  DiagramsOK := True;

  IF WorkingTimetableMode THEN BEGIN
    Log('W Write out to '+ DiagramsFilename + '.' + DiagramsFilenameSuffix + ' diagrams created from the working timetable');

    ClearDiagramsFromDatabase;

    T := TrainList;
    WHILE T <> NIL DO BEGIN
      IF T^.Train_DiagramFound
      AND (T^.Train_CurrentStatus <> NonMoving)
      THEN BEGIN
        WITH T^ DO BEGIN
          SetLength(DepartureTimeArray, 0);
          SetLength(DirectionsArray, 0);
          SetLength(EndLocationsStrArray, 0);
          SetLength(NotForPublicUseArray, 0);
          SetLength(StoppingArray, 0);

          StartLocationStr := LocationToStr(t^.Train_JourneysArray[0].TrainJourney_StartLocation);
          IF StartLocationStr = UnknownLocationStr THEN
            StartLocationStr := AreaToStr(t^.Train_JourneysArray[0].TrainJourney_StartArea);

          JourneyCount := 0;
          WHILE JourneyCount <= High(Train_JourneysArray) DO BEGIN
            WITH Train_JourneysArray[JourneyCount] DO BEGIN
              AppendToDateTimeArray(DepartureTimeArray, TrainJourney_DiagrammedDepartureTime);
              IF TrainJourney_EndLocation <> UnknownLocation THEN
                AppendToStringArray(EndLocationsStrArray, LocationToStr(TrainJourney_EndLocation))
              ELSE
                AppendToStringArray(EndLocationsStrArray, AreaToStr(TrainJourney_EndArea));

              AppendToDirectionArray(DirectionsArray, TrainJourney_Direction);
              LightsRemainOn := True;
              TrainNonMoving := False;
              LightsOnTime := 0;

              AppendToBooleanArray(NotForPublicUseArray, TrainJourney_NotForPublicUse);

              AppendToBooleanArray(StoppingArray, TrainJourney_StoppingOnArrival);
              LengthOfTrainInCarriages := 0;
              TypeOfTrainNum := Train_TypeNum;

              IF (JourneyCount = High(Train_JourneysArray)) OR ((JourneyCount > 0)
              AND (JourneyCount MOD 7 = 0))
              THEN BEGIN
                Log('X ' + Locochiptostr(T^.Train_Locochip) + ' J=' + inttostr(journeycount) + ' ' + StartLocationStr);

                WriteOutDiagramsToDatabase(T^.Train_LocoChip, T^.Train_DoubleHeaderLocoChip, JourneyCount, DepartureTimeArray, LightsOnTime, EndLocationsStrArray,
                                                 DirectionsArray, LightsRemainOn, TrainNonMoving, NotForPublicUseArray, StartLocationStr, StoppingArray,
                                                 LengthOfTrainInCarriages, TypeOfTrainNum, NOT UserDriving, NOT UserRequiresInstructions, NOT StartOfRepeatJourney);

                IF (JourneyCount > 0)
                AND (JourneyCount MOD 7 = 0)
                THEN
                  { replace the source string if we're wrapping to a fresh line in the diagram }
                  StartLocationStr := EndLocationsStrArray[JourneyCount];

                SetLength(DepartureTimeArray, 0);
                SetLength(DirectionsArray, 0);
                SetLength(EndLocationsStrArray, 0);
                SetLength(NotForPublicUseArray, 0);
                SetLength(StoppingArray, 0);
              END;
            END; {WITH}
            Inc(JourneyCount);
          END; {WHILE}
        END; {WITH}
      END;
      T := T^.Train_NextRecord;
    END; {WHILE}
  END;

  IF DiagramsOK THEN BEGIN
    { Now add the trains to the diagrams - this is only valid for a short time before being replaced with the final version, but is very useful if one has to deal with
      an on-screen query about an unavailable destination
    }
    T := TrainList;
    WHILE T <> NIL DO BEGIN
      IF T^.Train_DiagramFound
      AND (T^.Train_CurrentStatus <> NonMoving)
      THEN
        AddTrainToDiagramsList(T);
      T := T^.Train_NextRecord;
    END; {WHILE}

    CheckOccupiedLinesAndDiagrams;
  END;

  IF DiagramsOK THEN
    FindPendingLocations(DiagramsLoading, DiagramsOK);

  IF DiagramsOK THEN BEGIN
    { Now create the train journey route array }
    T := TrainList;
    WHILE T <> NIL DO BEGIN
      WITH T^ DO BEGIN
        IF (Train_LocoChip <> UnknownLocoChip)
        AND Train_DiagramFound
        AND (Train_CurrentStatus <> Cancelled)
        AND (Train_CurrentStatus <> NonMoving)
        THEN BEGIN
          JourneyCount := 0;
          WHILE (JourneyCount <= High(Train_JourneysArray))
          AND (Train_CurrentStatus <> Cancelled)
          DO BEGIN
            WITH T^ DO BEGIN
              WITH Train_JourneysArray[JourneyCount] DO BEGIN
                FindRouteFromLineAToLineB(t^.Train_LocoChip, JourneyCount, UnknownSignal, TrainJourney_StartLine, TrainJourney_EndLine, TrainJourney_Direction, Train_Type,
                                          Train_CurrentLengthInInches, NOT EmergencyRouteing, NOT IncludeOutOfUseLines, TrainJourney_RouteArray, LinesNotAvailableStr,
                                          ErrorMsg, DiagramsOK);
                IF NOT DiagramsOK THEN BEGIN
                  { try emergency routeing }
                  Log(LocoChipToStr(Train_LocoChip) + ' T Problem in finding a route ' + DirectionToStr(TrainJourney_Direction)
                                                        + ' from ' + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine)
                                                        + ': trying emergency routeing');
                  FindRouteFromLineAToLineB(Train_LocoChip, JourneyCount, UnknownSignal, TrainJourney_StartLine, TrainJourney_EndLine, TrainJourney_Direction, Train_Type,
                                           Train_CurrentLengthInInches, EmergencyRouteing, NOT IncludeOutOfUseLines, TrainJourney_RouteArray, LinesNotAvailableStr,
                                           ErrorMsg, DiagramsOK);
                END;
                IF NOT DiagramsOK THEN BEGIN
                  IF MessageDialogueWithDefault('Loco ' + LocoChipToStr(Train_LocoChip) + ': problem in finding a route ' + DirectionToStr(TrainJourney_Direction)
                                                + ' from ' + LineToStr(TrainJourney_StartLine) + ' to ' + LineToStr(TrainJourney_EndLine) + ' even using emergency routeing'
                                                + '(' + ErrorMsg + '):'
                                                + CRLF
                                                + 'Do you wish to cancel the train and continue loading the diagrams without it or exit the program?',
                                                StopTimer, mtError, [mbYes, mbNo], ['&Cancel', '&Exit'], mbYes) = mrNo
                  THEN
                    ShutDownProgram(UnitRef, 'ProcessDiagrams')
                  ELSE BEGIN
                    ChangeTrainStatus(T, Cancelled);
                    DiagramsOK := True;
                  END;
                END;
              END; {WITH}
            END; {WITH}
            Inc(JourneyCount);
          END; {WHILE}
        END;

        T := Train_NextRecord;
      END; {WITH}
    END; {WHILE}
  END;

  IF DiagramsOK THEN
    SplitJourneys(ErrorMsg, DiagramsOK);

  IF DiagramsOK THEN
    AddStationNamesToJourneys;

  IF DiagramsOK THEN
    CalculatePublicJourneyStartTimes;

  IF DiagramsOK THEN BEGIN
    T := TrainList;
    WHILE T <> NIL DO BEGIN
      WITH T^ DO BEGIN
        IF (Train_LocoChip <> UnknownLocoChip)
        AND Train_DiagramFound
        AND (Train_CurrentStatus <> Cancelled)
        AND (Train_CurrentStatus <> NonMoving)
        THEN BEGIN
          RecalculateJourneyTimes(T, 'as part of journey creation');
          DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);

          Log(Train_LocoChipStr + ' D List of partly revised journeys and diagrams:');
          FOR JourneyCount := 0 TO High(Train_JourneysArray) DO
            Log(Train_LocoChipStr + ' D    J=' + IntToStr(JourneyCount) + ': ' + DescribeJourney(T, JourneyCount) + ' {NOUNITREF}');

          DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);
        END;

        T := Train_NextRecord;
      END; {WITH}
    END; {WHILE}
  END;

  IF DiagramsOK THEN BEGIN
    { Now clear the temporary diagrams, and add the trains to the real diagrams }
    DiagramsList := NIL;

    T := TrainList;
    WHILE T <> NIL DO BEGIN
      IF T^.Train_DiagramFound THEN //AND (T^.Train_CurrentStatus <> NonMoving) THEN
        AddTrainToDiagramsList(T);
      T := T^.Train_NextRecord;
    END; {WHILE}
    DiagramsList := ListSort_SortDiagramsList(DiagramsList, DepartureTimeAndTrainTypeSort);
  END;

  { Finally record the fact the diagrams are loaded }
  IF DiagramsOK THEN BEGIN
    DiagramsLoaded := True;
    DiagramsChanged := False;
    Log('D Diagrams loaded');
  END;

  IF DiagramsOK THEN BEGIN
    { Recalculate the journey length in inches }
    T := TrainList;
    WHILE T <> NIL DO BEGIN
      WITH T^ DO BEGIN
        IF (Train_LocoChip <> UnknownLocoChip)
        AND Train_DiagramFound
        AND (Train_CurrentStatus <> Cancelled)
        AND (Train_CurrentStatus <> NonMoving)
        THEN BEGIN
          JourneyCount := 0;
          WHILE (JourneyCount <= High(Train_JourneysArray))
          AND (Train_CurrentStatus <> Cancelled)
          DO BEGIN
            WITH T^ DO BEGIN
              WITH Train_JourneysArray[JourneyCount] DO BEGIN
                TrainJourney_LengthInInches := CalculateRouteLength(TrainJourney_RouteArray);
                IF TrainJourney_LengthInInches > 0 THEN BEGIN
                  TrainJourney_DurationInMinutes := CalculateJourneyTimeInMinutes(Train_TypeNum, TrainJourney_LengthInInches);
                  Log(Train_LocoChipStr + ' D J=' + IntToStr(JourneyCount) + ':'
                                        + ' calculating TrainJourney_LengthInInches:'
                                        + ' from ' + LocationToStr(TrainJourney_StartLocation)
                                        + ' to ' + LocationToStr(TrainJourney_EndLocation)
                                        + ': length=' + FloatToStr(TrainJourney_LengthInInches)
                                        + ' time=' + IntToStr(TrainJourney_DurationInMinutes)
                                        + IfThen(TrainJourney_DurationInMinutes = 1, ' minute', ' minutes'));
                END;
              END; {WITH}
            END; {WITH}
            Inc(JourneyCount);
          END; {WHILE}
        END;

        T := Train_NextRecord;
      END; {WITH}
    END; {WHILE}
  END;

  { And set up the location occupations }
  IF DiagramsOK THEN
    SetUpAllLocationOccupationsAbInitio(TimetableIsloading, DiagramsOK);

  IF DiagramsOK THEN BEGIN
    T := TrainList;
    WHILE T <> NIL DO BEGIN
      WITH T^ DO BEGIN
        IF (Train_LocoChip <> UnknownLocoChip)
        AND Train_DiagramFound
        AND (Train_CurrentStatus <> Cancelled)
        AND (Train_CurrentStatus <> NonMoving)
        THEN BEGIN
          RecalculateJourneyTimes(T, 'as part of journey creation');

          { Add the arrival and departure time data to the record kept for the station monitors }
          JourneyCount := 0;
          WHILE JourneyCount <= High(T^.Train_JourneysArray) DO BEGIN
            WITH T^.Train_JourneysArray[JourneyCount] DO BEGIN
              TrainJourney_DiagrammedDepartureTime := TrainJourney_CurrentDepartureTime;
              TrainJourney_DiagrammedArrivalTime := TrainJourney_CurrentArrivalTime;
            END; {WITH}
            Inc(JourneyCount);
          END; {WHILE}
          DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);

          Log(Train_LocoChipStr + ' D List of fully revised journeys and timetable:');
          FOR JourneyCount := 0 TO High(Train_JourneysArray) DO
            Log(Train_LocoChipStr + ' D    J=' + IntToStr(JourneyCount) + ': ' + DescribeJourney(T, JourneyCount) + ' {NOUNITREF}');

          DrawLineInLogFile(Train_LocoChip, 'D', '-', UnitRef);
        END;

        T := Train_NextRecord;
      END; {WITH}
    END; {WHILE}
  END;

  DrawDiagrams(UnitRef, 'ProcessDiagrams');
  IF NOT StationMonitorsAlreadySetUp THEN
    SetUpStationMonitors([], 1);
END; { ProcessDiagrams }

PROCEDURE InitialiseDiagramsUnit;
{ Initialises the unit }
BEGIN
  DiagramsList := NIL;
  DiagramsChosenTrain := NIL;
END; { InitDiagramsUnit }

INITIALIZATION

END { Diagrams }.