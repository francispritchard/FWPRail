UNIT MiscUtils;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, InitVars, Input, Menus, ComCtrls, System.UITypes, TLHelp32, Logging;

TYPE
  TDebugWindow = CLASS(TForm)
    PopupDebugWindowResetSizeAndPosition: TMenuItem;
    PopupDebugWindowCopy: TMenuItem;
    DebugRichEdit: TRichEdit;
    DebugRichEditColourDialogue: TColorDialog;
    DebugRichEditPopupMenu: TPopupMenu;
    PROCEDURE PopupDebugWindowCopyClick(Sender: TObject);
    PROCEDURE PopupDebugWindowResetSizeAndPositionClick(Sender: TObject);
    PROCEDURE DebugRichEditKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
    PROCEDURE DebugRichEditMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE DebugRichEditMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
    PROCEDURE DebugRichEditPopupMenuOnPopup(Sender: TObject);
    PROCEDURE DebugWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE DebugWindowHide(Sender: TObject);
    PROCEDURE DebugWindowResize(Sender: TObject);
    PROCEDURE DebugWindowShow(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  DebugWindow: TDebugWindow;

PROCEDURE AddLightsToLightsToBeSwitchedOnArray(T : TrainIndex; DesiredDirection1, DesiredDirection2 : DirectionType; MinSeconds, MaxSeconds : Integer;
                                               LightsOnTime : TDateTime);
{ Set up a train's lights to switch on at a random time ahead }

PROCEDURE AddRichLine(RichEdit: TRichEdit; StrToAdd: String);
{ Taken from Delpi Pages (http://www.delphipages.com/tips/thread.cfm?ID=186) - by Slavikn, WebPage: http://www.organizermp3.com }

FUNCTION AddSeparatorToTimeString(Str : String) : String;
{ Add a separator to a string so that, e.g., 0630 becomes 06:30 }

PROCEDURE AddStoredRichEditLoggingTextToLoggingWindow;
{ Add any stored rich-edit data to the logging window }

FUNCTION AllJourneysComplete(T : TrainIndex) : Boolean;
{ Returns true if all a train's journeys are complete }

PROCEDURE AppendIntegerArray2ToIntegerArray1(VAR IntegerArray1 : IntegerArrayType; IntegerArray2 : IntegerArrayType);
{ Join two integer arrays together }

PROCEDURE AppendStringArray2ToStringArray1(VAR StringArray1 : StringArrayType; StringArray2 : StringArrayType);
{ Join two string arrays together }

PROCEDURE AppendToAreaArray(VAR AreaArray : IntegerArrayType; NewElement : Integer);
{ Appends an area to the array }

PROCEDURE AppendToBooleanArray(VAR BooleanArray : BooleanArrayType; NewElement : Boolean);
{ Appends a boolean value to the array }

PROCEDURE AppendToDateTimeArray(VAR DateTimeArray : DateTimeArrayType; NewElement : TDateTime);
{ Appends a TDateTime value to the array }

PROCEDURE AppendToDirectionArray(VAR DirectionArray : DirectionArrayType; NewElement : DirectionType);
{ Appends a direction value to the array }

PROCEDURE AppendToIntegerArray(VAR IntegerArray : IntegerArrayType; NewElement : Integer);
{ Appends an integer value to the array }

PROCEDURE AppendToLineArray(VAR LineArray : IntegerArrayType; NewElement : Integer);
{ Appends a line to the array }

PROCEDURE AppendToLocationArray(VAR LocationArray : IntegerArrayType; NewElement : Integer);
{ Appends a location to the array }

PROCEDURE AppendToNewLineArray(VAR NewLineArray : IntegerArrayType; NewElement : Integer);
{ Appends a newly-created line to the temporary new-lines array }

PROCEDURE AppendToStringArray(VAR StringArray : StringArrayType; NewElement : String);
{ Appends a string value to the array }

PROCEDURE AppendToTrainArray(VAR TrainArray : TrainArrayType; NewElement : TrainIndex);
{ Appends a train value to the array }

PROCEDURE AppendToTrainTypeArray(VAR TrainTypes : TrainTypeArray; NewElement : TypeOfTrainType);
{ Appends a TrainType value to the array }

FUNCTION AreaArrayToStr(AreaArray : IntegerArrayType) : String;
{ Return an area array as a string }

FUNCTION AreaToStr{1}(Area : Integer) : String; Overload;
{ Return an area as a long string }

FUNCTION AreaToStr{2}(Area : Integer; LongOrShortString : StringType) : String; Overload;
{ Return an area as either a short or a long string }

FUNCTION AspectToStr{1}(Aspect : AspectType) : String; Overload;
{ Return the signal aspect as a long string }

FUNCTION AspectToStr{2}(Aspect : AspectType; LongOrShortString : StringType) : String; Overload;
{ Return the signal aspect as either a short or a long string }

FUNCTION ATS(Area : Integer) : String;
{ Return an area as a string - routine designed only for use in debugging }

FUNCTION ATSA(AreaArray : IntegerArrayType) : String;
{ Return an area array as a string - routine designed for use in debugging }

FUNCTION BufferStopToStr(BufferStop : Integer) : String;
{ Return the buffer stop number as a string }

FUNCTION CardinalMulDiv(A, B, C : Cardinal) : Cardinal;
{ Returns (A * B) / C; intermediate result is held double-length to avoid overflow. FWP version }

PROCEDURE ChangeTrainStatus(T : TrainIndex; NewStatus : TrainStatusType);
{ Change the current train status and record it }

PROCEDURE CheckSemaphoreDistantBeforeSemaphoreHomeCleared(S : Integer);
{ Sees if semaphore distants need automatically to be reset to allow a semaphore home to be put on }

PROCEDURE CloseInputOrOutputFile(VAR InputOrOutputFile : Text; Filename : String);
{ Close a file, capturing the error message if any }

FUNCTION ColourToStr(Colour : TColour) : String;
{ UK English version of Delphi subroutine }

FUNCTION ColourToStrForUser(Colour : TColour) : String;
{ Checks if it's a Delphi colour or an FWP one, and removes the "cl" or "clFWP" }

FUNCTION ControlledByStateToStr(ControlledByState : LocoControlStateType) : String;
{ Return a string describing loco control }

FUNCTION CountSubRoutes(RouteArray : StringArrayType) : Integer;
{ Return how many subroutes there are in a given route array }

FUNCTION CursorToStr(Cursor : TCursor) : String;
{ Returns a string describing the cursor supplied }

FUNCTION DayOfTheWeekToStr{2}(DayOfTheWeek : DayOfTheWeekType) : String; Overload;
{ Return a string with the given day of the week }

FUNCTION DaysOfTheWeekSetToStr(DaysOfTheWeek : DaysOfTheWeekSetType) : String; Overload;
{ Return a string with given days of the week }

FUNCTION DaysOfTheWeekSetToStr(DaysOfTheWeek : DaysOfTheWeekSetType; LongOrShortString : StringType) : String;  Overload;
{ Return a string with given days of the week }

PROCEDURE Debug{1}; Overload;
{ Does nothing - use it to set breakpoints on }

PROCEDURE Debug{2}(Str : String); Overload;
{ Write out debug text to the Debug Window }

PROCEDURE Debug{3}(Str : String; WriteToStatusBar : Boolean); Overload;
{ Write out debug text to the Debug Window }

PROCEDURE DeleteElementFromDateTimeArray(VAR DateTimeArray : DateTimeArrayType; Position : Integer);
{ Removes the selected element from a TDateTime array }

PROCEDURE DeleteElementFromBooleanArray(VAR BooleanArray : BooleanArrayType; Position : Integer);
{ Removes the selected element from a boolean array }

PROCEDURE DeleteElementFromIntegerArray(VAR IntegerArray : IntegerArrayType; Position : Integer);
{ Removes the selected element from an integer array }

PROCEDURE DeleteElementFromLightsToBeSwitchedOnArray(Position : Integer);
{ Removes the selected element from the lights to be switched on array }

PROCEDURE DeleteElementFromLocationArray(VAR LocationArray : IntegerArrayType; Position : Integer);
{ Removes the selected element from a location array }

PROCEDURE DeleteElementFromStringArray(VAR StringArray : StringArrayType; Position : Integer);
{ Removes the selected element from a string array }

FUNCTION DescribeIntegerArray(IntegerArray : IntegerArrayType) : String;
{ Return the contents of an integer array as a string }

FUNCTION DescribeJourneyAndRoute(Args: ARRAY OF Integer) : String;
{ Return a description of the route (and subroute if requested) }

FUNCTION DescribeLineNamesForTrackCircuit(TC : Integer) : String;
{ Return the line names for a track circuit }

FUNCTION DescribePolygon(CONST Polygon: ARRAY OF TPoint) : String;
{ Returns the points comprising a polygon as a string }

FUNCTION DescribeSubRoute(Route, SubRoute : Integer) : String;
{ Return a description of the subroute }

FUNCTION DescribeStartAndEndOfRoute(Route : Integer) : String;
{ Return the signal at the start of the route, and the signal or buffer stop that ends it }

FUNCTION DisplayJourneyNumber(Journey : Integer) : String;
{ Return the supplied journey number with an indent in a form that makes the debug output easier to read }

FUNCTION DisplayJourneyNumbers(T : TrainIndex; FirstJourney, SecondJourney : Integer) : String;
{ Return the supplied journey numbers with an indent in a form that makes the debug output easier to read }

FUNCTION DisplayTrackCircuitsForLocation(Location : Integer) : String;
{ Write out which track circuits the given location contains }

FUNCTION DescribeTrainList{1} : String; Overload;
{ Return the contents of the train list }

FUNCTION DescribeTrainList{2}(Sort : SortOrder; DescribeFullTrainList : Boolean) : String; Overload;
{ Return the contents of the train list, the full list of locos and trains if DescribeFullTrainList is set }

FUNCTION DirectionToStr{1}(Dir : DirectionType) : String; Overload;
{ Return the long name of a direction }

FUNCTION DirectionToStr{2}(Dir : DirectionType; LongOrShortString : StringType) : String; Overload;
{ Return the either the short or the long name of a direction }

FUNCTION DirectionWillChangeAfterGivenJourney(T : TrainIndex; CurrentJourney : Integer) : Boolean;
{ Returns true if the direction will change on the journey after the given journey }

FUNCTION DirectionArrayToStr(DirectionsArray : DirectionArrayType) : String;
{ List the contents of an array }

FUNCTION DistanceBetween(X1, Y1, X2, Y2 : Integer) : Extended;
{ Returns the Euclidean distance between (X1, Y1) and (X2, Y2) }

PROCEDURE DrawLineInLogFile(LocoChipStr : String; LogFileCh : Char; LineStr : String; OriginatingUnitRef : String);
{ Draw a line of a given character in the log file }

FUNCTION EndOfLineToStr(E : EndOfLineType) : String;
{ Return the end of line type as a string }

FUNCTION ExtractBufferStopFromString(Str : String): Integer;
{ Extract a signal post number from a string }

FUNCTION ExtractIntegerFromString(L : String) : Integer;
{ Returns a string with just the numbers, or -1 if there is no number }

FUNCTION ExtractJourneyFromString(Str : String): Integer;
{ Returns a journey number from a given string }

FUNCTION ExtractLineFromString(Str : String): Integer;
{ Returns a line from a given string }

FUNCTION ExtractPointFromString(Str : String): Integer;
{ Returns a point number from a given string }

FUNCTION ExtractPointStateFromString(Str : String) : PointStateType;
{ Returns a point state from a given array element }

FUNCTION ExtractRouteFromString(Str : String): Integer;
{ Returns a route number from a given string }

FUNCTION ExtractSignalFromString(Str : String): Integer;
{ Extract a signal number from a string }

FUNCTION ExtractSignalStateFromString(Str : String) : SignalStateType;
{ Returns a signal state from a given array element }

FUNCTION ExtractSubRouteFromString(Str : String): Integer;
{ Returns a subroute number from a given array element }

PROCEDURE ExtractSubStringsFromString(Str : String; DelimiterCh : Char; OUT StrArray : StringArrayType);
{ Goes through a given string, extracting substrings delimited by the given delimiter character }

FUNCTION ExtractTrackCircuitFromString(Str : String): Integer;
{ Returns a track circuit number from a given string }

FUNCTION FeedbackUnitTypeToStr(FeedbackType : TypeOfFeedbackDetector) : String;
{ Convert a feedback unit type to a string }

FUNCTION FinalJourney(T : TrainIndex; CurrentJourney : Integer) : Boolean;
{ Returns true if the current journey is the final one }

FUNCTION FindButton(CONST Dlg: TForm; CONST ModalResult: Integer): TButton;
{ FindButton finds a button in a Message Dialog that has been created with the Dialogs.CreateMessageDialog function, based on its ModalResult. The function returns the
  button, or NIL, if the button has not been found [from uDialogsExt from Borland website]
}
FUNCTION GetAreaFromStationMonitorsDisplayOrderNum(OrderNum : Integer) : Integer;
{ Return the Area indicated by the station monitors' display order number }

FUNCTION GetBuildInfoAsString : String;
{ Return the program's build number - this is auto incremented on each build }

FUNCTION GetComputerNetName : String;
{ Return the local computer name (by Zarko Gajic, About.com) }

FUNCTION GetElementPosInIntegerArray(IntegerArray : IntegerArrayType; Element : Integer) : Integer;
{ Returns where, if at all, the given array element is found in an integer array }

FUNCTION GetExtendedDataFromUser(Prompt, DefaultStr : String; LowerBound, UpperBound : Extended; BoundsErrorMsg : String; OUT NewValue : Extended) : Boolean;
{ Makes sure an extended valuer is returned by the user, and optionally that it is within the bounds set }

FUNCTION GetIntegerDataFromUser(Prompt, DefaultStr : String; LowerBound, UpperBound : Integer; BoundsErrorMsg : String; OUT NewValue : Integer) : Boolean;
{ Makes sure an integer is returned by the user, and optionally that it is within the bounds set }

FUNCTION GetFollowingChars(LineStr : String; Str : String; EndStr : String) : String;
{ Return the characters after the given characters up to the delimiter supplied (or "CRLF" if it's at a line end) }

FUNCTION GetLineTypeColour(T : TypeOfLine) : TColour;
{ Return the colour for a specific line type }

FUNCTION GetLinesForTrackCircuit(TC : Integer) : IntegerArrayType;
{ Return all the lines on a given track circuit }

FUNCTION GetLocationFromTrackCircuit(TC : Integer) : Integer;
{ Return a location given a track-circuit number }

FUNCTION GetLocoIndexFromLocoChip(LocoChip : Integer): LocoIndex;
{ Look for a matching loco record given a loco chip number }

FUNCTION GetLocoIndexFromTrainIndex(T : TrainIndex): LocoIndex;
{ Look for a matching loco record given a train index }

FUNCTION GetLocoRecFromLocoChip(LocoChip : Integer; OUT Loco : LocoRec) : Boolean;
{ Look for a matching loco record given a loco chip number }

FUNCTION GetMidPos(I1, I2 : Integer) : Integer;
{ Return the mid position between two given values }

FUNCTION GetNextDayOfTheWeek(DayOfTheWeek : DayOfTheWeekType) : DayOfTheWeekType;
{ Return the next day of the week to the one given }

FUNCTION GetOrdinalFromCardinal(Ordinal : Integer) : String;
{ Return the ordinal version of a cardinal number }

FUNCTION GetProgramTitle : String;
{ Returns the program title }

FUNCTION GetProgramVersion(Str : String) : String;
{ Returns the program version }

FUNCTION GetSignalAdjacentLine(S : Integer) : Integer;
{ Return the line adjacent to the given signal }

FUNCTION GetSignalAspect(S : Integer) : AspectType;
{ Return the state of a signal }

FUNCTION GetStationMonitorsDisplayOrderStr(OrderNum : Integer) : String;
{ Return the description of the Area indicated by the station monitors' display order number }

FUNCTION GetStationNumFromStationMonitorsDisplayOrderNum(StationMonitorsDisplayOrderStr : String) : Integer;
{ Return the station monitors' display order number from the description of the Area }

FUNCTION GetStringDataFromUser(Prompt, DefaultStr : String; PermissibleStrings : StringArrayType; OUT NewString : String) : Boolean;
{ Makes sure a non-empty string is returned by the user, and optionally that it is matches one of the strings supplied }

FUNCTION GetTrackCircuitsForLocation(Location : Integer) : IntegerArrayType;
{ Return all the track circuits for a given location }

FUNCTION GetTrackCircuitStateColour(TC : Integer) : TColour;
{ Return whether and how the track circuit is occupied }

FUNCTION GetTrackCircuitState(TC : Integer) : TrackCircuitStateType;
{ Return whether and how the track circuit is occupied }

FUNCTION GetTrainIndexFromLocoChip(LocoChip : Integer): TrainIndex;
{ Look for a matching train record given a locochip }

FUNCTION GetTrainTypeFromLocoChip(LocoChip : Integer) : TypeOfTrainType;
{ Returns the train type given the loco number }

FUNCTION GetUserFromWindows : String;
{ Return the local user name (by Zarko Gajic, About.com) }

FUNCTION GetVersionInfoAsString : String;
{ Return the program's version number }

FUNCTION GradientToStr(Gradient : GradientType) : String;
{ Return the gradient status of the line as a string }

PROCEDURE HideMenus;
{ Make all the menus invisible }

FUNCTION IfThenTime{1}(Test : Boolean; Time1, Time2 : TDateTime) : TDateTime; Overload;
{ Returns the first supplied time if the test is true, the second time if not. (The system IfThen routine doesn't work with TDateTime values) }

FUNCTION IfThenTime{2}(Test : Boolean; Time : TDateTime) : TDateTime; Overload;
{ Returns the supplied time if the test is true. (The system IfThen routine doesn't work with TDateTime values) }

FUNCTION InAllRouteDebuggingMode : Boolean;
{ Return the status of AllRouteDebuggingMode }

FUNCTION InAnonymousOccupationMode : Boolean;
{ Return the status of AnonymousOccupationMode }

FUNCTION InDebuggingMode : Boolean;
{ Return the status of DebuggingMode }

FUNCTION InFeedbackDebuggingMode : Boolean;
{ Return the status of FeedbackDebuggingMode }

FUNCTION InLineDebuggingMode : Boolean;
{ Return the status of LineDebuggingMode }

FUNCTION InLockDebuggingMode : Boolean;
{ Return the status of LockDebuggingMode }

FUNCTION InLockingMode : Boolean;
{ Return the status of LockingMode }

FUNCTION InLocoSpeedTimingMode : Boolean;
{ Return the status of LocoSpeedTimingMode }

FUNCTION InLogsCurrentlyKeptMode : Boolean;
{ Return the status of LogsCurrentlyKeptMode }

FUNCTION InPointDebuggingMode : Boolean;
{ Return the status of PointDebuggingMode }

FUNCTION InRDCMode : Boolean;
{ Return the status of RDCMode }

FUNCTION InRecordingMonitorScreensMode : Boolean;
{ Return the status of RecordingMonitorScreensMode }

FUNCTION InRecordLineDrawingMode : Boolean;
{ Return the status of RecordLineDrawingMode }

FUNCTION InRouteDebuggingMode : Boolean;
{ Return the status of RouteDebuggingMode }

FUNCTION InRouteBacktrackDebuggingMode : Boolean;
{ Return the status of RouteBacktrackDebuggingMode }

FUNCTION InRouteDrawingMode : Boolean;
{ Return the status of RouteDrawingMode }

FUNCTION InShowAdjacentTrackCircuitMode: Boolean;
{ Return the status of ShowAdjacentTrackCircuitMode }

FUNCTION InStationStartMode: Boolean;
{ Return the status of StationStartMode }

FUNCTION InTestingMode: Boolean;
{ Return the status of TestingMode }

FUNCTION IndicatorToStr(I : IndicatorType; LongOrShortString : StringType) : String;
{ Return the type a route indicator is }

FUNCTION IndicatorStateToStr(I : IndicatorStateType) : String;
{ Return the state of a route indicator }

PROCEDURE InitialiseMiscUtilsUnit;
{ Initialises the unit }

PROCEDURE InsertElementInIntegerArray(VAR IntegerArray : IntegerArrayType; Position : Integer; NewElement : Integer);
{ Adds an element to an integer array }

PROCEDURE InsertElementInStringArray(VAR StringArray : StringArrayType; Position : Integer; NewElement : String);
{ Adds an element to a string array }

PROCEDURE InsertElementInBooleanArray(VAR BooleanArray : BooleanArrayType; Position : Integer; NewElement : Boolean);
{ Adds an element to a boolean array }

PROCEDURE InsertElementInDateTimeArray(VAR DateTimeArray : DateTimeArrayType; Position : Integer; NewElement : TDateTime);
{ Adds an element to a TDateTime array }

PROCEDURE InsertElementInTrainJourneyRecArray(VAR JourneyRecArray : TrainJourneyRecArrayType; Position : Integer; NewElement : TrainJourneyRec);
{ Adds an element to a TrainJourneyRec array }

PROCEDURE InsertEntryInWorkingTimetable(VAR WorkingTimetableRecArray : WorkingTimetableRecArrayType; Position : Integer);
{ Adds an entry to the working timetable }

FUNCTION IntegerArrayToStr(IntegerArray : IntegerArrayType) : String;
{ Return the contents of an integer array }

FUNCTION IntToMPH(Speed : Integer) : MPHType;
{ Returns the given integer as an MPH value }

FUNCTION IOError(Filename : String; SaveIOResult : Integer; OUT ErrorMsg : String) : Boolean;
{ Returns the IO error message }

FUNCTION IsElementIndicator(StringArray : StringArrayType; StringArrayPos : Word) : Boolean;
{ Returns whether the given array element is a route indicator }

FUNCTION IsElementInIntegerArray{1}(IntegerArray : IntegerArrayType; Element : Integer) : Boolean; Overload;
{ Returns whether the given array element is found in an integer array }

FUNCTION IsElementInIntegerArray{2}(IntegerArray : IntegerArrayType; Element : Integer; OUT ElementPos : Integer) : Boolean; Overload;
{ Returns whether the given array element is found in an integer array and where it is in the array }

FUNCTION IsElementInLineArray(LineArray : IntegerArrayType; Element : Integer) : Boolean;
{ Returns whether the given array element is in a line array }

FUNCTION IsElementInLocationArray(LocationArray : IntegerArrayType; Element : Integer; OUT ElementPos : Integer) : Boolean;
{ Returns whether the given array element is in a location array }

FUNCTION IsElementInStringArray{1}(StringArray : StringArrayType; Element : String) : Boolean; Overload;
{ Returns whether the given array element is in a string array }

FUNCTION IsElementInStringArray{2}(StringArray : StringArrayType; Element : String; OUT ElementPos : Integer) : Boolean; Overload;
{ Returns where the given array element is in a string array }

FUNCTION IsElementSubRouteMarker(StringArray : StringArrayType; StringArrayPos : Word) : Boolean;
{ Returns whether the given array element is a subroute marker }

FUNCTION IsPointInStringArray(StringArray : StringArrayType; Point : Integer) : Boolean;
{ Returns whether the given point is found in a string array }

FUNCTION IsProgramRunning(ProgramName : String) : Boolean;
{ Checks to see if a given program is running }

FUNCTION IsSignalInStringArray(StringArray : StringArrayType; Signal : Integer) : Boolean;
{ Returns whether the given signal is found in a string array }

FUNCTION IsTrackCircuitInStringArray(StringArray : StringArrayType; TC : Integer; OUT Pos : Integer) : Boolean;
{ Returns whether and where the given track circuit is found in a string array }

FUNCTION JourneyToStr(Journey : Integer) : String;
{ Return a journey number as a string }

FUNCTION JunctionIndicatorLit(S : Integer) : Boolean;
{ Return true if a junction indicator is lit }

FUNCTION JunctionIndicatorTypeToStr(J : JunctionIndicatorType) : String;
{ Returns a junction indicator type as a string }

FUNCTION LenzConnectionToStr(LenzConnection : LenzConnectionType) : String;
{ Return the kind of connection }

FUNCTION LATS(LineArray : IntegerArrayType) : String;
{ Return line array as a string - routine designed for use in debugging }

FUNCTION LightsTypeToStr(TypeOfLights : LightsType) : String;
{ Return the type of lights the train has }

FUNCTION LineToStr(L : Integer) : String;
{ Return a line's name as a string }

FUNCTION LinesAreAdjacent(L1, L2 : Integer; ErrorMsg : String) : Boolean;
{ Returns true if the two given lines are adjacent }

FUNCTION ListLocoChipsInIntegerArray(IntegerArray : IntegerArrayType) : String;
{ Lists loco chips from an integer array }

PROCEDURE LoadPreviousPointSettings;
{ Load the previous settings - generally used when starting up offline }

FUNCTION LocationOccupationStateToStr(OccupationState : LocationOccupationStateType) : String;
{ Return the state of the Location Occupation as a string }

FUNCTION LocationOccupied(Location : Integer) : Boolean;
{ Returns true if the given location has a feedback occupation }

FUNCTION LocationOutOfUse(Location : Integer; OUT OutOfUseTC : Integer; OUT OutOfUseStr : String) : Boolean;
{ Returns true if the given location is out of use because of an out-of-use track-circuit occupation }

FUNCTION LocationToStr{1}(Location : Integer) : String; Overload;
{ Return a location as a long string }

FUNCTION LocationToStr{2}(Location : Integer; LongOrShortString : StringType) : String; Overload;
{ Return a location as a either a short or a long string }

FUNCTION LocoChipToStr{1}(LocoChip : Integer) : String; Overload;
{ Return the locomotive number as a string - if LocoChip less then 1000, add appropriate number of leading zeroes }

FUNCTION LocATS(LocationArray : IntegerArrayType) : String;
{ Return locations as long strings - routine designed for use in debugging }

FUNCTION LocoChipToStr{2}(LocoChip : Integer; UnknownAsZeroes : Boolean) : String; Overload;
{ Return the locomotive number as a string - if LocoChip less then 1000, add appropriate number of leading zeros }

FUNCTION LocoIndexToStr(L : LocoIndex) : String; Overload;
{ Return the locomotive index as a string }

FUNCTION LocTS(Location : Integer) : String;
{ Return a location as a long string - routine designed only for use in debugging }

FUNCTION LTS(L : Integer) : String;
{ Return a line as a string - routine designed only for use in debugging }

PROCEDURE MakeSound(SoundNum : Integer);
{ Make a warning sound }

FUNCTION MapGridYToRow(GridY : Integer) : Extended;
{ Map grid co-ordinate to row }

FUNCTION MapGridXToScreenX(GridX : Integer) : Integer;
{ Map grid co-ordinate to screen co-ordinate }

FUNCTION MapGridYToScreenY(GridY : Integer) : Integer;
{ Map grid co-ordinate to screen co-ordinate }

FUNCTION MapScreenXToGridX(ScreenX : Integer) : Integer;
{ Map screen co-ordinate to grid co-ordinate }

FUNCTION MapScreenYToGridY(ScreenY : Integer) : Integer;
{ Map screen co-ordinate to grid co-ordinate }

FUNCTION MessageDialogueWithDefault{1}(DialogueText: String; StopTimer : Boolean; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons; DefaultButton : TMsgDlgBtn) : Word;
                                       Overload;
{ Adapted from the Borland Delphi web site example - uses procedures from their uDialogsExt Unit. This version has a default button. }

FUNCTION MessageDialogueWithDefault{2}(DialogueText: String; StopTimer : Boolean; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons; ButtonText : ARRAY OF String;
                                       DefaultButton : TMsgDlgBtn) : Word; Overload;
{ Adapted from the Borland Delphi web site example - uses procedures from their uDialogsExt Unit. This version has replacement button text as well as a default button }

FUNCTION MPHToInt(MPH : MPHType) : Integer;
{ Returns the given MPH as a integer }

FUNCTION MPHToStr(MPH : MPHType) : String;
{ Returns the given MPH as a string }

FUNCTION MulDiv(A, B, C : Integer) : Integer;
{ Returns (A * B) / C; intermediate result is held double-length to avoid overflow }

FUNCTION NewMinutesBetween(CONST AThen, ANow: TDateTime): Int64;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
FUNCTION NewSecondsBetween(CONST AThen, ANow: TDateTime): Integer;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
FUNCTION NewMilliSecondsBetween(CONST AThen, ANow: TDateTime): Int64;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
FUNCTION NextButton{1}(CONST Dlg: TForm; VAR Context: Integer): TButton; Overload;
{ The NextButton can be used to traverse the buttons in a Message Dialogue that has been created with the Dialogs.CreateMessageDialogue function. Initialize the Context
  variable to 0 to get the first button. The routine updates the Context variable to cause the next call to NextButton to return the next button. If there are no (more)
  buttons, the function returns NIL [from uDialogsExt from Borland website]
}
FUNCTION NextButton{2}(CONST Dlg: TForm; VAR Context: Integer; OUT Button: TButton): Boolean; Overload;
{ This variant of NextButton returns the button in the Button variable. The function result indicates whether a button has been found [from uDialogsExt from Borland
  website]
}
FUNCTION NextLineRouteingToStr(NextLineRouteing : NextLineRouteingType) : String;
{ Return a description of the next line routeing type }

FUNCTION OpenInputFileOK(VAR InputFilename : Text; Filename : String; OUT ErrorMsg : String) : Boolean;
{ Open a existing file to read from }

FUNCTION OpenOutputFileOK(VAR OutputFilename : Text; Filename : String; OUT ErrorMsg : String; AppendToFile : Boolean) : Boolean;
{ Open (and create if necessary) a file }

FUNCTION OppositeDirection(Dir : DirectionType) : DirectionType;
{ Return the direction opposite to the one given }

PROCEDURE Pause(MilliSeconds : Cardinal; ProcessMessages : Boolean);
{ Pause for a given number of milliseconds }

FUNCTION PenStyleToStr(PenStyle : TPenStyle) : String;
{ Describes pen styles }

FUNCTION PointInPolygon(CONST Polygon: ARRAY OF TPoint; Point: TPoint): Boolean;
{ Returns true if a point is in a defined region }

FUNCTION PointIsCatchPoint(P : Integer) : Boolean;
{ Returns whether a given point is a catch point }

FUNCTION PointStateToStr(PointState : PointStateType) : String;
{ Return the point state as a string }

FUNCTION PointToStr(Point : Integer) : String;
{ Return the point number as a string }

FUNCTION PointTypeToStr(PType : TypeOfPoint) : String;
{ Return the point type }

PROCEDURE ReadOut(SoundStr : String);
{ Uses system API SndPlaySound to read out the given text, by playing a .wav file. text is held in the system resource file, itself compiled by using "brcc32 -v rail.rc"
  from the command prompt. The file "rail.rc" is the resource script file.
}
FUNCTION RemoveAllSpacesFromAString(Str : String) : String;
{ Removes all spaces from a given string }

PROCEDURE RemoveDuplicateElementsFromIntegerArray(VAR IntegerArray : IntegerArrayType);
{ Removes duplicate elements }

PROCEDURE RemoveDuplicateElementsFromStringArray{1}(VAR StringArray : StringArrayType); Overload;
{ Removes duplicate elements }

PROCEDURE RemoveDuplicateElementsFromStringArray{2}(VAR StringArray : StringArrayType; SubRouteCount : Integer); Overload
{ Removes duplicate elements - if SubRouteCount <> 0 do it for individual subroutes, not the whole array }

PROCEDURE RenameEarlierFiles(VAR SuppliedFileType : TextFile; SuppliedFileName, SuppliedFilenamePrefix : String);
{ Renames any existing earlier files, to give three consecutive backups }

PROCEDURE RenameLaterFiles(VAR SuppliedFileType : TextFile; SuppliedFileName, SuppliedFilenamePrefix : String);
{ This is used when a current log is thrown away: the older log file names revert to their previous names }

PROCEDURE ResetDebugWindowSizeAndPosition;
{ Reset the window's size and position }

PROCEDURE ResetTestCount;
{ Resets the number which is incremented each time the TestCount or TestCountStr routines are called - used for debugging }

FUNCTION ReturnFixedLengthStr(Str : String; FixedLength : Integer) : String;
{ Return a short string of a fixed length }

PROCEDURE ReturnTrainFromMissing(T : TrainIndex);
{ Set a train as being no longer missing }

FUNCTION RoundTimeToNextWholeMinute(Time : TDateTime) : TDateTime;
{ Round the time to the next whole minute, i.e. 06:30:00 becomes 0G:31:00, but 06:30:20 becomes 06:32:00 }

FUNCTION RouteToStr(Route : Integer) : String;
{ Return the Route number as a string }

FUNCTION SameTimeInHoursAndMinutesOnly(Time1, Time2 : TDateTime) : Boolean;
{ Compares two given times and returns true if they are the same in terms of hours and minutes (the system routine SameTime compares times down to the millisecond level,
  and comparing two times by means of Time1 = Time2 also fails if there is a franction of a difference between them).
}
FUNCTION SetDefaultButton(CONST Dlg: TForm; CONST ModalResult: Integer): Boolean;
{ SetDefaultButton sets the default button in a Message Dialogue that has been created with the Dialogs.CreateMessageDialogue function. The result is a success indicator.
  The function only fails, if the specified button is not present in the dialogue [from uDialogsExt from Borland website]
}
PROCEDURE SetFeedbackDebuggingModeOn(DebugStr : String; AdjacentSignalNumber, DecoderNumber, TCInFull, TCOnce : Boolean);
{ Turn on feedback debugging mode, updating the fourth status panel }

PROCEDURE SetFeedbackDebuggingModeOff(DebugStr : String);
{ Turn off feedback debugging mode, updating the fourth status panel }

PROCEDURE SetMode(TypeOfMode : ModeType; OnOrOff : Boolean);
{ Set up one of the various modes, updating the fourth status panel }

PROCEDURE SetTwoLightingChips(L : LocoIndex; LightsAtUp, LightsAtDown : DirectionType; LightsOn : Boolean);
{ When appropriate switch two lighting chips }

PROCEDURE ShowMenus;
{ Make all the menus visible }

PROCEDURE ShutDownProgram(UnitRef : String; SubroutineStr : String);
{ Shut down the program neatly }

FUNCTION SignalAdjacentLineOK(Line : Integer) : Boolean;
{ Returns true if a signal can be created next to the current line }

FUNCTION SignalHasLeftJunctionIndicator(S : Integer; OUT Indicator : JunctionIndicatorType) : Boolean;
{ Returns true if the signal has a left junction indicator }

FUNCTION SignalHasRightJunctionIndicator(S : Integer; OUT Indicator : JunctionIndicatorType) : Boolean;
{ Returns true if the signal has a right junction indicator }

FUNCTION SignalQuadrantToStr(Q : QuadrantType) : String;
{ Return a string with the supplied signal quadrant details }

FUNCTION SignalToStr(Signal : Integer) : String;
{ Return the signal number as a string }

FUNCTION SignalTypeToStr(ST : TypeOfSignal; LongOrShortString : StringType) : String;
{ Return the type of a signal in words }

FUNCTION SpeedInMPHToLocoLenzSpeed(L : LocoIndex; Speed : MPHType) : Integer;
{ Return the appropriate Lenz speed for the given loco }

PROCEDURE StartLocos(Restart : Boolean);
{ Restart all the locos that were running before the enforced stop }

PROCEDURE StopAParticularTrain(T : TrainIndex);
{ Stops just one train }

PROCEDURE StopLocos(Msg : String);
{ Stop all the locos currently in the diagram list. This is a better approach than the brute force approach which is to send a "StopAllLocomotives" command, which produces
  an "emergency off" situation.
}
PROCEDURE StopOrResumeAllOperations(Str : String);
{ Deal with emergency situations by stopping operations or restarting them }

FUNCTION StringArraysCompareOK(FirstArray, SecondArray : StringArrayType; OUT ErrorMsg : String) : Boolean;
{ Does an element by element comparison of two string arrays }

FUNCTION StringArrayToStr(StringArray : StringArrayType) : String;
{ List the contents of an array }

FUNCTION StrToArea(Str : String) : Integer;
{ Convert a string to an area }

FUNCTION StrToAspect(Str : String) : AspectType;
{ Return the signal aspect string as an aspect type }

FUNCTION StrToColour(Str : String) : TColour;
{ Checks if it's a Delphi colour or an FWP one }

FUNCTION StrToDayOfTheWeek(Str : String) : DayOfTheWeekType;
{ Return a day of the week from a given string }

FUNCTION StrToDirectionType(Str : String) : DirectionType;
{ Convert a string to a direction }

FUNCTION StrToEndOfLine(Str : String) : EndOfLineType;
{ Convert a string to an end of line }

FUNCTION StrToFeedbackUnitType(Str : String) : TypeOfFeedbackDetector;
{ Convert a string to a feedback unit type }

FUNCTION StrToGradient(Str : String) : GradientType;
{ Convert a string to a gradient }

FUNCTION StrToIndicatorType(Str : String) : IndicatorType;
{ Return the type a route indicator is }

FUNCTION StrToLine(Str : String) : Integer;
{ Convert a string to a line name }

FUNCTION StrToLocation(Str : String) : Integer;
{ Convert a string to a location }

FUNCTION StrToMPH(Str : String) : MPHType;
{ Returns the given string as a MPH }

FUNCTION StrToPenStyle(Str : String) : TPenStyle;
{ Converts strings to pen styles }

FUNCTION StrToPlatformNumberPosition(Str : String) : PlatformNumberPositionType;
{ Return the given platform number position value as a string }

FUNCTION StrToPointState(Str : String) : PointStateType;
{ Convert a string to a point state }

FUNCTION StrToPointType(Str : String) : TypeOfPoint;
{ Convert a string to a point state }

FUNCTION StrToSignalType(Str : String) : TypeOfSignal;
{ Return the type of a signal }

FUNCTION StrToThroughLocationState(Str : String) : ThroughLocationStateType;
{ Return the through state from a given string }

FUNCTION StrToTrainTypeNum(Str : String) : Integer;
{ Returns the train type as a number; an up-to-date list as of 5/10/05 }

FUNCTION StrToTypeOfLine(Str : String) : TypeOfLine;
{ Convert a string to a line type }

FUNCTION SubRouteStateToStr(SubRouteState : SubRouteStateType) : String;
{ Return the state of the subroute as a string }

FUNCTION TestCount : Integer;
{ Returns a number which is incremented each time the routine is called - used for debugging }

FUNCTION TestCountStr : String;
{ Returns as a string a number which is incremented each time the routine is called - used for debugging }

FUNCTION ThroughLocationStateToStr(ThroughLocationState : ThroughLocationStateType) : String;
{ Return the through state of the location as a string }

FUNCTION TimeIsValid(TimeStr : String) : Boolean;
{ Checking a given time string - an hour/min separator is permissible. Also ignore a trailing asterisk }

FUNCTION TimeToHMStr(Time : TDateTime) : String;
{ Return a time string as hh:mm }

FUNCTION TimeToHMSStr(Time : TDateTime) : String;
{ Return a time string as hh:mm:ss }

FUNCTION TimeToHMSZStr(Time : TDateTime) : String;
{ Return a time string as hh:mm:ss:zzz }

FUNCTION TrackCircuitStateIsPermanentlyOccupied(State : TrackCircuitStateType) : Boolean;
{ Returns true if a given track-circuit state is not set as permanently occupied }

FUNCTION TrackCircuitStateIsTemporarilyOccupied(State : TrackCircuitStateType) : Boolean;
{ Returns true if a given track circuit-state is set as temporarily occupied }

FUNCTION TrackCircuitStateToStr(State : TrackCircuitStateType) : String;
{ Describe the current state of a given track circuit }

FUNCTION TrackCircuitToStr(TrackCircuit : Integer) : String;
{ Return the track circuit number as a string }

FUNCTION TrainStatusToStr(Status : TrainStatusType) : String;
{ Return the given train status as a string }

FUNCTION TrainTypeNumToStr(TrainTypeNum : Integer) : String;
{ Returns the train type number as a string; an up-to-date list as of 5/10/05 }

FUNCTION TrainTypeToTrainTypeNum(TrainType : TypeOfTrainType) : Integer;
{ Returns the number for the kind of train it is; an up-to-date list as of 5/10/05 }

FUNCTION TrainTypeNumToTrainType(TrainTypeNum : Integer) : TypeOfTrainType;
{ Returns the train type; an up-to-date list as of 5/10/05 }

FUNCTION TrimRemoveSpacesAndMakeUpperCase(Str : String) : String;
{ Tidy up a string to make comparisons more accurate }

FUNCTION TTS(Time : TDateTime) : String;
{ Return a time string as hh:mm - abbreviated form of TimeToHMStr above with format 'hh:mm' for debugging }

FUNCTION TTSS(Time : TDateTime) : String;
{ Return a time string as hh:mm:ss - abbreviated form of TimeToHMSStr for debugging }

FUNCTION TTSZ(Time : TDateTime) : String;
{ Return a time string as hh:mm:ss:zzz - abbreviated form of TimeToHMSZStr for debugging }

FUNCTION TTSA(TimesArray : DateTimeArrayType) : String;
{ Return time strings as hh:mm - abbreviated form of TimeToHMStr above with format 'hh:mm' for debugging }

PROCEDURE TurnLocoCabLightsOff(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turns cab lights off. Assumes that the cab lights are operated by function one }

PROCEDURE TurnLocoCabLightsOn(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turns cab lights off. Assumes that the cab lights are operated by function one }

PROCEDURE TurnLocoHeadLightsOff(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turn off a loco's headlights }

PROCEDURE TurnLocoLightsOff(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turns the lights off by changing functions on the loco chip and optional second chip }

PROCEDURE TurnLocoLightsOn(L : LocoIndex; NonMoving, LightLoco : Boolean; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turns the lights on by changing functions on the loco chip and optional second chip }

FUNCTION TypeOfLineToStr(T : TypeOfLine) : String;
{ Describe a line type }

PROCEDURE UnknownLocoChipFound(RoutineName : String);
{ Called if an unknown loco chip is found at the beginning of a subroutine }

PROCEDURE UnknownLocoRecordFound(RoutineName : String);
{ Called if an unknown loco record is found at the beginning of a subroutine }

PROCEDURE UnknownTrainRecordFound(RoutineName : String);
{ Called if an unknown train record is found at the beginning of a subroutine }

FUNCTION WorkingTimetableStatusToStr(Status : WorkingTimetableStatusType) : String;
{ Return the given working timetable status as a string }

PROCEDURE WriteNextLineDetailToDebugWindow(L : Integer; HelpRequired : Boolean);
{ Indicate what the next line status is for the given line }

PROCEDURE WriteStringArrayToLog{1}(LocoChipStr : String; TypeOfLogChar : Char; StringArray : StringArrayType;
                                   Indent, WrapNum : Integer); Overload;
{ Write the contents of an array to the replay file, wrapping it at Wrapnum and indenting it by Indent: this version is not preceded by an explanatory string. }

PROCEDURE WriteStringArrayToLog{2}(LocoChipStr : String; TypeOfLogChar : Char; Str: String; StringArray : StringArrayType; Indent, WrapNum : Integer); Overload;
{ Write the contents of an array to the replay file, wrapping it at Wrapnum and indenting it by Indent: this version is preceded by an explanatory string. }

PROCEDURE WriteStringArrayToLog{3}(LocoChipStr : String; TypeOfLogChar : Char; Str: String; StringArray : StringArrayType; Indent, WrapNum : Integer; BreakOnStr : String);
                                   Overload;
{ Write the contents of an array to the replay file, wrapping it at Wrapnum and indenting it by Indent: this version is preceded by an explanatory string }

PROCEDURE WriteStringArrayToLog{4}(LocoChipStr : String; TypeOfLogChar : Char; StringArray : StringArrayType; Indent, WrapNum : Integer; BreakOnStr : String); Overload;
{ Write the contents of an array to the replay file, wrapping it at Wrapnum and indenting it by Indent: this version is not preceded by an explanatory string. If the
  BreakOnStr string appears in the text, start a new line before it.
}
PROCEDURE WriteStringArrayToLog{5}(LocoChipStr : String; TypeOfLogChar : Char; StringArray : StringArrayType); Overload;
{ Write the contents of an array to the replay file, wrapping it by default at 88 and indenting it by two - this version is not preceded by an explanatory string. }

PROCEDURE WriteStringArrayToLog{6}(LocoChipStr : String; TypeOfLogChar : Char; Str: String; StringArray : StringArrayType); Overload;
{ Write the contents of an array to the replay file, wrapping it by default at 88 and indenting it by two - this version is preceded by an explanatory string. }

PROCEDURE WriteStringArrayToLog{7}(LocoChipStr : String; TypeOfLogChar : Char; Str : String; StringArray : StringArrayType; NumberElements : Boolean); Overload;
{ Write the contents of an array to the replay file, wrapping it by default at 88 and indenting it by two - this version is preceded by an explanatory string. If
  NumberElements is true, add number in array to each element.
}
PROCEDURE WriteStringArrayToLog{8}(LocoChipStr : String; TypeOfLogChar : Char; Str : String; StringArray : StringArrayType; NumberElements : Boolean; BreakOnStr : String);
                                   Overload;
{ Write the contents of an array to the replay file, wrapping it by default at 150 and indenting it by two - this version is preceded by an explanatory string. If
  NumberElements is true, add number in array to each element. If the BreakOnStr string appears in the text, start a new line before it.
}
PROCEDURE WriteStringArrayToLog{9}(TypeOfLogChar : Char; Str: String; StringArray : StringArrayType; Indent, WrapNum : Integer; BreakOnStr : String); Overload;
{ Write the contents of an array to the replay file, wrapping it at Wrapnum and indenting it by Indent: this version is preceded by an explanatory string }

PROCEDURE WriteTimeToLog(Num : Integer);
{ A debugging procedure whereby the current time is written to the log, together with the elapsed time since the previous call to the routine }

PROCEDURE WriteToLogFile(LogStr : String);
{ Write the data to the log file, adding the LocoChip and UnitRef, and wrapping and indenting if required. If a tilde is to appear in the string other than as a delimiter,
  it should appear twice.
}
PROCEDURE WriteVariableDataToFile;
{ List some variable data to a given file }


PROCEDURE GetProjectVersionInfo(AVersionList: TStrings; AFileName: String= '');
FUNCTION GetBuildInfo(VAR V1, V2, V3, V4: Word; AFileName: String= ''): Boolean;

IMPLEMENTATION

{$R *.dfm}

USES GetTime, Lenz, Diagrams, RailDraw, Types, LocoUtils, Math {sic}, IDGlobal, StrUtils, Feedback, RDCUnit, CreateRoute, IniFiles, DateUtils, Startup, Cuneo, Movement,
     LocoDialogue, FWPShowMessageUnit, Options, Help, MMSystem, TCPIP, Main, StationMonitors, Train, Edit;

CONST
  UnitRef = 'MiscUtils';

TYPE
  PTransBuffer = ^TTransBuffer;
  TTransBuffer = ARRAY [1..13] OF SmallInt;

CONST
  CInfoStr : ARRAY [1..13] OF String =
    ('FileVersion',
     'CompanyName',
     'FileDescription',
     'InternalName',
     'LegalCopyright',
     'LegalTradeMarks',
     'OriginalFileName',
     'ProductName',
     'ProductVersion',
     'Comments',
     'CurrentProgramVersion',
     'CurrentDatabaseVersion',
     'VersionDetails');

  DebugWindowCaption = '&Debug Window';
  MaxSaveLogStrs = 10;
  RichEditChars = True;

VAR
  AllRouteDebuggingMode : Boolean = False;
  AnonymousOccupationMode : Boolean = True;
  DebuggingMode : Boolean = False;
  DrawLineInLogFileAfter : Boolean = False;
  DrawLineInLogFileBefore : Boolean = False;
  DrawLineInLogFileOnly : Boolean = False;
  FeedbackDebuggingMode : Boolean = False;
  IdenticalLineMsgWritten : Boolean = False;
  InitialisationCount : Integer;
  LastLogLineStr : String = '';
  LastSoundTime : TDateTime = 0;
  LineAfterJustDrawn : Boolean = False;
  LineDebuggingMode : Boolean = False;
  LockDebuggingMode : Boolean = False;
  LockingMode : Boolean = True;
  LocoSpeedTimingMode : Boolean = False;
  LogArray : StringArrayType;
  LogsCurrentlyKeptMode : Boolean = True;
  OldDebugStr : String = '';
  OperationsStopped : Boolean = False;
  PointDebuggingMode : Boolean = False;
  PreviousLogTime : TDateTime = 0;
  RDCMode : Boolean = False;
  RecordingMonitorScreensMode : Boolean = False;
  RecordLineDrawingMode : Boolean = False;
  RouteBacktrackDebuggingMode : Boolean = False;
  RouteDebuggingMode : Boolean = False;
  RouteDrawingMode : Boolean = False;
  SaveLogStrArray : ARRAY [1..MaxSaveLogStrs] OF String;
  ShowAdjacentTrackCircuitMode : Boolean = False;
  StationStartMode : Boolean;
  StoredRichEditLoggingTextArray : StringArrayType;
  TempSaveLogStr : String = '';
  TestCounter : Integer = 0; { used to test iterations in debugging }
  TestingMode : Boolean = False;
  UserDebugText : String = '';

FUNCTION InAllRouteDebuggingMode : Boolean;
{ Return the status of AllRouteDebuggingMode }
BEGIN
  Result := AllRouteDebuggingMode;
END; { InAllRouteDebuggingMode }

FUNCTION InAnonymousOccupationMode : Boolean;
{ Return the status of AnonymousOccupationMode }
BEGIN
  Result := AnonymousOccupationMode;
END; { InAnonymousOccupationMode }

FUNCTION InDebuggingMode : Boolean;
{ Return the status of DebuggingMode }
BEGIN
  Result := DebuggingMode;
END; { InDebuggingModeMode }

FUNCTION InFeedbackDebuggingMode : Boolean;
{ Return the status of FeedbackDebuggingMode }
BEGIN
  Result := FeedbackDebuggingMode;
END; { InFeedbackDebuggingModeMode }

FUNCTION InLineDebuggingMode : Boolean;
{ Return the status of LineDebuggingMode }
BEGIN
  Result := LineDebuggingMode;
END; { InLineDebuggingMode }

FUNCTION InLockDebuggingMode : Boolean;
{ Return the status of LockDebuggingMode }
BEGIN
  Result := LockDebuggingMode;
END; { InLockDebuggingMode }

FUNCTION InLockingMode : Boolean;
{ Return the status of LockingMode }
BEGIN
  Result := LockingMode;
END; { InLockingMode }

FUNCTION InLocoSpeedTimingMode : Boolean;
{ Return the status of LocoSpeedTimingMode }
BEGIN
  Result := LocoSpeedTimingMode;
END; { InLocoSpeedTimingMode }

FUNCTION InLogsCurrentlyKeptMode : Boolean;
{ Return the status of LogsCurrentlyKeptMode }
BEGIN
  Result := LogsCurrentlyKeptMode;
END; { InLogsCurrentlyKeptMode }

FUNCTION InPointDebuggingMode : Boolean;
{ Return the status of PointDebuggingMode }
BEGIN
  Result := PointDebuggingMode;
END; { InPointDebuggingMode }

FUNCTION InRDCMode : Boolean;
{ Return the status of RDCMode }
BEGIN
  Result := RDCMode;
END; { InRDCMode }

FUNCTION InRecordingMonitorScreensMode : Boolean;
{ Return the status of RecordingMonitorScreensMode }
BEGIN
  Result := RecordingMonitorScreensMode;
END; { InRecordingMonitorScreensMode }

FUNCTION InRecordLineDrawingMode : Boolean;
{ Return the status of RecordLineDrawingMode }
BEGIN
  Result := RecordingMonitorScreensMode;
END; { InRecordLineDrawingMode }

FUNCTION InRouteDebuggingMode : Boolean;
{ Return the status of RouteDebuggingMode }
BEGIN
  Result := RouteDebuggingMode;
END; { InRouteDebuggingMode }

FUNCTION InRouteDrawingMode : Boolean;
{ Return the status of RouteDrawingMode }
BEGIN
  Result := RouteDrawingMode;
END; { InRouteDrawingMode }

FUNCTION InRouteBacktrackDebuggingMode : Boolean;
{ Return the status of RouteBacktrackDebuggingMode }
BEGIN
  Result := RouteBacktrackDebuggingMode;
END; { InRouteBacktrackDebuggingMode }

FUNCTION InShowAdjacentTrackCircuitMode : Boolean;
{ Return the status of ShowAdjacentTrackCircuitMode }
BEGIN
  Result := ShowAdjacentTrackCircuitMode;
END; { InShowAdjacentTrackCircuitMode }

FUNCTION InStationStartMode : Boolean;
{ Return the status of StationStartMode }
BEGIN
  Result := StationStartMode;
END; { InStationStartMode }

FUNCTION InTestingMode : Boolean;
{ Return the status of TestingMode }
BEGIN
  Result := TestingMode;
END; { InTestingMode }

PROCEDURE WriteToLogFile(LogStr : String); Overload;
{ Write the data to the log file, adding the LocoChip and UnitRef, and wrapping and indenting if required. The syntax is: LocoChip, TypeOfLog, LogString, and parameters
  such as Indent=n Wrap=n Unit=UnitRef.
}
  PROCEDURE WriteOutError(ErrorStr : String);
  { Highlight any errors found in the syntax of Log requests }
  BEGIN
    Debug('*** ' + ErrorStr);
    IF LogFileOpen THEN BEGIN
      WriteLn(LargeLogFile, '*************** Error in log entry: ' + ErrorStr + ' [' + UnitRef + ']');
      Flush(LargeLogFile);
    END;
  END; { WriteOutError }

CONST
  EllipsisStr = '...';
  NoCurrentTimeStr = '';
  NoLocoChipStr = '';
  NoLogStr = '';

VAR
  BlankLineBefore : Boolean;
  Count : Integer;
  DrawLineChar : String;
  I, J : Integer;
  IdenticalLinesFound : Boolean;
  Indent : Integer;
  IndentStr : String;
  LocoChip : Integer;
  LocoChipStr : String;
  LogArrayCount : Integer;
  NoUnitRef : Boolean;
  PossibleEllipsisStr : String;
  TempInt : Integer;
  TempStr : string;
  TypeOfLog : String;
  UnitRef : String;
  WrapNum : Integer;
  WrapNumStr : String;

  PROCEDURE WriteToEachLogFile(TypeOfLog : Char; CurrentTimeStr, CurrentRailwayTimeStr, LocoChipStr, LogStrWithRichEditCommands : String);
  { Write to the various log files depending on the type of log }
  VAR
    LogStrWithoutRichEditCommands : String;

    PROCEDURE RemoveRichEditInstructionsFromLogStr(VAR TempLogStr : String);
    { Removes any instructions in angle brackets from the log string }
    VAR
      CloseAnglePos : Integer;
      OpenAnglePos : Integer;
      TempStr : String;

    BEGIN
      IF Pos('<B>', UpperCase(TempLogStr)) > 0 THEN
        TempLogStr := StringReplace(TempLogStr, '<B>', '', [rfIgnoreCase]);

      IF Pos('<COLOR=', UpperCase(TempLogStr)) > 0 THEN BEGIN
        TempStr := GetFollowingChars(TempLogStr, '<COLOR=', '>');
        TempLogStr := StringReplace(TempLogStr, '<COLOR=' + TempStr + '>', '', [rfIgnoreCase]);
      END;

      { If there is anything else between curly brackets, there's presumably a typo somewhere }
      OpenAnglePos := Pos('<', TempLogStr);
      CloseAnglePos := Pos('>', TempLogStr);
      IF (OpenAnglePos > 0) AND (CloseAnglePos > 0) THEN BEGIN
        Debug('Log file error: invalid command "' + Copy(TempLogStr, OpenAnglePos, CloseAnglePos - OpenAnglePos + 1) + '" found');
        AddRichLine(LoggingWindow.LoggingWindowRichEdit, 'Log file error: invalid command "' + Copy(TempLogStr, OpenAnglePos, CloseAnglePos - OpenAnglePos + 1) + '" found');
        WriteLn(LargeLogFile, 'Log file error: invalid command "' + Copy(TempLogStr, OpenAnglePos, CloseAnglePos - OpenAnglePos + 1) + '" found');
      END;

      { Finally remove any sundry spaces left, from, for example, between angle brackets }
      TempLogStr := StringReplace(TempLogStr, '  ', ' ', [rfIgnoreCase]);
      TempLogStr := StringReplace(TempLogStr, '  ', ' ', [rfIgnoreCase]);

      { and remove any intended extra spaces indicated by the underline character }
      TempLogStr := StringReplace(TempLogStr, '_', ' ', [rfIgnoreCase, rfReplaceAll]);
    END; { RemoveRichEditInstructionsFromLogStr }

  BEGIN
    IF NOT LogFileOpen THEN
      { Store the log until the file is opened }
      AppendToStringArray(LogArray, LogStrWithRichEditCommands)
    ELSE BEGIN
      { See if a completely blank line is required }
      IF Pos('{Blank Line}', LogStrWithRichEditCommands) > 0 THEN
        LogStrWithRichEditCommands := '';

      { Remove the rich edit commands for the file copy }
      LogStrWithOutRichEditCommands := LogStrWithRichEditCommands;
      RemoveRichEditInstructionsFromLogStr(LogStrWithOutRichEditCommands);

      LogStrWithRichEditCommands := IfThen(CurrentTimeStr = '',
                                           '',
                                           CurrentTimeStr + ' ')
                                    + CurrentRailwayTimeStr
                                    + ' ' + TypeOfLog
                                    + ' ' + IfThen(LocoChipStr = '',
                                                   '    ',
                                                   IfThen(LocoChipStr = '0000',
                                                          '    ',
                                                          LocoChipStr))
                                    + ' ' + LogStrWithRichEditCommands;

      LogStrWithOutRichEditCommands := IfThen(CurrentTimeStr = '',
                                              '',
                                              CurrentTimeStr + ' ')
                                       + CurrentRailwayTimeStr
                                       + ' ' + TypeOfLog
                                       + ' ' + IfThen(LocoChipStr = '',
                                                      '    ',
                                                      IfThen(LocoChipStr = '0000',
                                                             '    ',
                                                             LocoChipStr))
                                       + ' ' + LogStrWithOutRichEditCommands;

      { And now specific happenings }
      CASE TypeOfLog OF
        ' ':
          { ignore - this is for messages that we may at some stage want to log }
          ;
        'A', 'a', '$':
          { general happenings of relevance to all logs - includes '$' for quick debugging as it is easy to find in the logs }
          BEGIN
            IF StoreRichEditLoggingText THEN
              AppendToStringArray(StoredRichEditLoggingTextArray, LogStrWithRichEditCommands)
            ELSE
              AddRichLine(LoggingWindow.LoggingWindowRichEdit, LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            IF MultipleLogFilesRequired THEN BEGIN
              WriteLn(LocoLogFile, LogStrWithoutRichEditCommands);
              WriteLn(RouteLogFile, LogStrWithoutRichEditCommands);
              WriteLn(SignalPointAndTCLogFile, LogStrWithoutRichEditCommands);
              WriteLn(DiagramsLogFile, LogStrWithoutRichEditCommands);
              WriteLn(WorkingTimetableLogFile, LogStrWithoutRichEditCommands);
            END;
          END;
        'P', 'p', 'B', 'b', 'S', 's', 'T', 't':
          BEGIN
            IF StoreRichEditLoggingText THEN
              AppendToStringArray(StoredRichEditLoggingTextArray, LogStrWithRichEditCommands)
            ELSE
              AddRichLine(LoggingWindow.LoggingWindowRichEdit, LogStrWithRichEditCommands);
            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);

            IF MultipleLogFilesRequired THEN
              WriteLn(SignalPointAndTCLogFile, LogStrWithoutRichEditCommands);
          END;
        'D', 'd':
          BEGIN
            IF StoreRichEditLoggingText THEN
              AppendToStringArray(StoredRichEditLoggingTextArray, LogStrWithRichEditCommands)
            ELSE
              AddRichLine(LoggingWindow.LoggingWindowRichEdit, LogStrWithRichEditCommands);
            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);

            IF MultipleLogFilesRequired THEN
              WriteLn(DiagramsLogFile, LogStrWithoutRichEditCommands);
          END;
        'W', 'w':
          BEGIN
            IF StoreRichEditLoggingText THEN
              AppendToStringArray(StoredRichEditLoggingTextArray, LogStrWithRichEditCommands)
            ELSE
              AddRichLine(LoggingWindow.LoggingWindowRichEdit, LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            WriteLn(TestLogFile, LogStrWithoutRichEditCommands);
            IF MultipleLogFilesRequired THEN
              WriteLn(WorkingTimetableLogFile, LogStrWithoutRichEditCommands);
          END;
        'L', 'l':
          BEGIN
            IF StoreRichEditLoggingText THEN
              AppendToStringArray(StoredRichEditLoggingTextArray, LogStrWithRichEditCommands)
            ELSE
              AddRichLine(LoggingWindow.LoggingWindowRichEdit, LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            IF MultipleLogFilesRequired THEN
              WriteLn(LocoLogFile, LogStrWithoutRichEditCommands);
          END;
        'R', 'r':
          BEGIN
            IF StoreRichEditLoggingText THEN
              AppendToStringArray(StoredRichEditLoggingTextArray, LogStrWithRichEditCommands)
            ELSE
              AddRichLine(LoggingWindow.LoggingWindowRichEdit, LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            IF MultipleLogFilesRequired THEN
              WriteLn(RouteLogFile, LogStrWithoutRichEditCommands);
          END;
        '*':
          BEGIN
            IF StoreRichEditLoggingText THEN
              AppendToStringArray(StoredRichEditLoggingTextArray, LogStrWithRichEditCommands)
            ELSE
              AddRichLine(LoggingWindow.LoggingWindowRichEdit, LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            WriteLn(TestLogFile, LogStrWithoutRichEditCommands);
          END;
        '+':
          { Only write to the test log file }
          WriteLn(TestLogFile, LogStrWithoutRichEditCommands);
        'E', 'e', 'X', 'x': { unusual happenings of relevance to all the log files }
          BEGIN
            IF StoreRichEditLoggingText THEN
              AppendToStringArray(StoredRichEditLoggingTextArray, LogStrWithRichEditCommands)
            ELSE
              AddRichLine(LoggingWindow.LoggingWindowRichEdit, LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            IF MultipleLogFilesRequired THEN BEGIN
              WriteLn(LocoLogFile, LogStrWithoutRichEditCommands);
              WriteLn(RouteLogFile, LogStrWithoutRichEditCommands);
              WriteLn(SignalPointAndTCLogFile, LogStrWithoutRichEditCommands);
              WriteLn(DiagramsLogFile, LogStrWithoutRichEditCommands);
              WriteLn(ErrorLogFile, LogStrWithoutRichEditCommands);
            END;
          END;
      ELSE {CASE}
        BEGIN
          { a type of log we don't know about! }
          Debug('Unusual log type ''' + TypeOfLog + ''' found in line ''' + LogStr + '''');
          AddRichLine(LoggingWindow.LoggingWindowRichEdit, 'Unusual log type ''' + TypeOfLog + ''' found in line ''' + LogStr + '''');
          WriteLn(LargeLogFile, 'Unusual log type ''' + TypeOfLog + ''' found in line ''' + LogStr + '''');
        END;
      END; {CASE}

      IF WriteToLogFileAndTestFile THEN
        WriteLn(TestLogFile, LogStrWithoutRichEditCommands);
    END;
  END; { WriteToEachLogFile }

  PROCEDURE RemoveGeneralInstructionsFromLogStr(VAR LogStr : String);
  { Removes any instructions in curly brackets from the log string and process them }
  VAR
    CloseCurlyPos : Integer;
    OpenCurlyPos : Integer;
    TempStr : String;

  BEGIN
    { Now interpret the miscellaneous commands - first wrapping }
    IF Pos('{WRAP=', UpperCase(LogStr)) > 0 THEN BEGIN
      { Look for a space or a line end following }
      IF Pos('{WRAP=SCREENWIDTH}', UpperCase(LogStr)) > 0 THEN BEGIN
        { we want the maximum screen width }
        WrapNum := LogFileMaxWidthInChars;

        LogStr := StringReplace(LogStr, '{WRAP=SCREENWIDTH}', '', [rfIgnoreCase]);
      END ELSE BEGIN
        WrapNumStr := GetFollowingChars(LogStr, '{WRAP=', '}');
        IF NOT TryStrToInt(WrapNumStr, WrapNum) THEN BEGIN
          Debug('Log file error: WRAP= must be followed by "ScreenWidth" or a number (Log string="' + LogStr + '")');
          AddRichLine(LoggingWindow.LoggingWindowRichEdit, 'Log file error in "' + LogStr + '" WRAP= must be followed by "ScreenWidth" or a number'
                                                     + ' (Log string="' + LogStr + '")');
          WriteLn(LargeLogFile, 'Log file error in "' + LogStr + '" WRAP= must be followed by "ScreenWidth" or a number' + ' (Log string="' + LogStr + '")');
        END;

        LogStr := StringReplace(LogStr, '{WRAP=' + WrapNumStr + '}', '', [rfIgnoreCase]);
      END;
    END;

    BlankLineBefore := False;
    IF Pos(BlankLineBeforeLogStr, UpperCase(LogStr)) > 0 THEN BEGIN
      BlankLineBefore := True;
      LogStr := StringReplace(LogStr, BlankLineBeforeLogStr, '', [rfIgnoreCase]);
    END ELSE BEGIN
      DrawLineInLogFileBefore := False;
      DrawLineInLogFileOnly := False;
      DrawLineInLogFileAfter := False;

      IF Pos(LineLogStr, UpperCase(LogStr)) > 0 THEN BEGIN
        DrawLineChar := '-';

        { Draw the default line drawing character }
        IF NOT LineAfterJustDrawn THEN
          DrawLineInLogFileOnly := True;

        LogStr := StringReplace(LogStr, LineLogStr, '', [rfIgnoreCase]);
      END;

      { Drawing lines in the log file. First see if there is a drawn 'line after' immediately succeeding a 'line before' - avoid too many lines in the log }
      IF Pos(LineEqualsLogStr, UpperCase(LogStr)) > 0 THEN BEGIN
        TempStr := GetFollowingChars(LogStr, LineEqualsLogStr, '}');
        IF (UpperCase(TempStr) = 'BEFORE}') AND NOT LineAfterJustDrawn THEN BEGIN
          DrawLineInLogFileBefore := True;
          LogStr := StringReplace(LogStr, LineBeforeLogStr, '', [rfIgnoreCase]);
        END ELSE
          IF (UpperCase(TempStr) = 'BEFORE}') AND LineAfterJustDrawn THEN BEGIN
            DrawLineInLogFileBefore := False;
            LogStr := StringReplace(LogStr, LineBeforeLogStr, '', [rfIgnoreCase]);
          END ELSE
            IF UpperCase(TempStr) = 'AFTER}' THEN BEGIN
              DrawLineInLogFileAfter := True;
              LogStr := StringReplace(LogStr, LineAfterLogStr, '', [rfIgnoreCase]);
            END ELSE
              IF UpperCase(TempStr) = 'BEFOREANDAFTER}' THEN BEGIN
                DrawLineInLogFileAfter := True;
                IF NOT LineAfterJustDrawn THEN
                  DrawLineInLogFileBefore := True;
                LogStr := StringReplace(LogStr, LineBeforAndAfterLogStr, '', [rfIgnoreCase]);
              END ELSE
                IF Length(TempStr) = 1 THEN BEGIN
                  DrawLineChar := TempStr;
                  DrawLineInLogFileOnly := True;

                  LogStr := StringReplace(LogStr, LineEqualsLogStr + TempStr + '}', '', [rfIgnoreCase]);
                END ELSE BEGIN
                  Debug('Log file error: Line= must be followed by a single character or "BEFORE", "AFTER" OR "BEFOREANDAFTER" (Log string="' + LogStr + '")');
                  AddRichLine(LoggingWindow.LoggingWindowRichEdit, 'Log file error in "' + LogStr
                                                             + '" Line= must be followed by a single character or "BEFORE", "AFTER" OR "BEFOREANDAFTER" (Log string="'
                                                             + LogStr + '")');
                  WriteLn(LargeLogFile, 'Log file error in "' + LogStr
                                        + '" Line= must be followed by a single character or "BEFORE", "AFTER" OR "BEFOREANDAFTER" (Log string="' + LogStr + '")');
                END;
      END;
    END;

    NumberLines := False;
    IF Pos('{NUMBER}', UpperCase(LogStr)) > 0 THEN BEGIN
      NumberLines := True;
      LogStr := StringReplace(LogStr, '{NUMBER}', '', [rfIgnoreCase]);
    END;

    { Indents }
    IF Pos('{INDENT=', UpperCase(LogStr)) > 0 THEN BEGIN
      IndentStr := GetFollowingChars(LogStr, '{INDENT=', '}');
      IF NOT TryStrToInt(IndentStr, Indent) THEN BEGIN
        Debug('Log file error: INDENT= must be followed by a number (Log string="' + LogStr + ')"');
        AddRichLine(LoggingWindow.LoggingWindowRichEdit, 'Log file error in "' + LogStr + '": INDENT= must be followed by a number' + ' (Log string="' + LogStr + '")');
        WriteLn(LargeLogFile, 'Log file error in "' + LogStr + '": INDENT= must be followed by a number' + ' (Log string="' + LogStr + '")');
      END;

      LogStr := StringReplace(LogStr, '{INDENT=' + IndentStr + '}', '', [rfIgnoreCase]);
    END;

    { The calling unit name - but do not add the default if a substitute is specified. (This is used by the DrawLineInLogFile procedure to show where the line drawing
      originates from).
    }
    IF Pos('{UNIT=', UpperCase(LogStr)) > 0 THEN BEGIN
      UnitRef := GetFollowingChars(LogStr, '{UNIT=', '}');
      LogStr := StringReplace(LogStr, '{UNIT=' + UnitRef + '}', '', [rfIgnoreCase]);
    END;

    { See if we need to replace the unit with another - used, for instance, where a procedure to draw a line in the log is called, and would otherwise have the wrong
      unit, that is the unit where the line drawing procedure is located, recorded.
    }
    IF Pos('{UNITSUBSTITUTE=', UpperCase(LogStr)) > 0 THEN BEGIN
      UnitRef := GetFollowingChars(LogStr, '{UNITSUBSTITUTE=', '}');
      LogStr := StringReplace(LogStr, '{UNITSUBSTITUTE=' + UnitRef + '}', '', [rfIgnoreCase]);
    END;

    { Unit name not wanted }
    IF Pos('{NOUNITREF}', UpperCase(LogStr)) > 0 THEN BEGIN
      NoUnitRef := True;
      LogStr := StringReplace(LogStr, '{NOUNITREF}', '', [rfIgnoreCase]);
    END;

    { If there is anything else between curly brackets, there's presumably a typo somewhere }
    OpenCurlyPos := Pos('{', LogStr);
    CloseCurlyPos := Pos('}', LogStr);
    IF (OpenCurlyPos > 0) AND (CloseCurlyPos > 0) THEN BEGIN
      Debug('Log file error: invalid command "' + Copy(LogStr, OpenCurlyPos, CloseCurlyPos - OpenCurlyPos + 1) + '" found');
      AddRichLine(LoggingWindow.LoggingWindowRichEdit, 'Log file error: invalid command "' + Copy(LogStr, OpenCurlyPos, CloseCurlyPos - OpenCurlyPos + 1) + '" found');
      WriteLn(LargeLogFile, 'Log file error: invalid command "' + Copy(LogStr, OpenCurlyPos, CloseCurlyPos - OpenCurlyPos + 1) + '" found');
    END;

    { Finally remove any sundry spaces left, from, for example, between curly brackets }
    LogStr := StringReplace(LogStr, '  ', ' ', [rfIgnoreCase]);
    LogStr := StringReplace(LogStr, '  ', ' ', [rfIgnoreCase]);
  END; { RemoveGeneralInstructionsFromLogStr }

BEGIN { WriteToLogFile }
  TRY
    IF InLogsCurrentlyKeptMode THEN BEGIN
      { Set up the internal logging window }
      IF LoggingWindow = NIL THEN BEGIN
        LoggingWindow := TLoggingWindow.Create(Application);
        LoggingWindow.Update;
        AddRichLine(LoggingWindow.LoggingWindowRichEdit, GetProgramTitle);
        AddRichLine(LoggingWindow.LoggingWindowRichEdit, GetProgramVersion('Log File'));

        { When the cursor is inside the rich-edit window, store the text to write it to the window when the cursor leaves the window }
        SetLength(StoredRichEditLoggingTextArray, 0);
      END;

      { write out anything previous stored when the log file was not open }
      IF LogFileOpen AND (Length(LogArray) <> 0) THEN BEGIN
        AddRichLine(LoggingWindow.LoggingWindowRichEdit, '--------------------------------------------------');
        AddRichLine(LoggingWindow.LoggingWindowRichEdit, 'Log strings stored before the log file was opened:');
        AddRichLine(LoggingWindow.LoggingWindowRichEdit, '');

        WriteLn(LargeLogFile, '--------------------------------------------------');
        WriteLn(LargeLogFile, 'Log strings stored before the log file was opened:');
        WriteLn(LargeLogFile, '');
        LogArrayCount := 0;
        WHILE LogArrayCount <> Length(LogArray) DO BEGIN
          AddRichLine(LoggingWindow.LoggingWindowRichEdit, LogArray[LogArrayCount]);
          WriteLn(LargeLogFile, LogArray[LogArrayCount]);
          Inc(LogArrayCount);
        END; {WHILE}
        AddRichLine(LoggingWindow.LoggingWindowRichEdit, '--------------------------------------------------');
        WriteLn(LargeLogFile, '--------------------------------------------------');
        SetLength(LogArray, 0);
      END;

      IdenticalLinesFound := False;

      TypeOfLog := ' ';
      Indent := 0;
      WrapNum := 0;
      LocoChipStr := '';
      NoUnitRef := False;

      { See how the arguments are constituted }
      IF NOT ReplayMode THEN BEGIN
        { First look for locochip number }
        I := Pos(UnknownLocoChipStr, LogStr);
        IF I = 1 THEN BEGIN
          { replace the unknown loco chip string with four spaces in the log file so that it formats correctly }
          IF Copy(LogStr, 1, Length(UnknownLocoChipStr)) = UnknownLocoChipStr THEN BEGIN
            LogStr := Copy(LogStr, Length(UnknownLocoChipStr) + 2);
            LocoChipStr := '    ';
          END;
        END ELSE BEGIN
          IF TryStrToInt(Copy(LogStr, 1, 4), TempInt) OR (Copy(LogStr, 1, 4) = '----') THEN BEGIN
            IF Copy(LogStr, 5, 1) <> ' ' THEN BEGIN
              WriteOutError('No space after loco number in "' + LogStr + '"');
              Exit;
            END ELSE BEGIN
              IF Copy(LogStr, 1, 4) = '----' THEN
                LocoChipStr := '    '
              ELSE
                LocoChipStr := Copy(LogStr, 1, 4);
              LogStr := Copy(LogStr, 6);
            END;
          END;
        END;

        { look for log character after locochip number }
        IF Copy(LogStr, 1, 1) = ' ' THEN BEGIN
          { there isn't one - write out the whole line in case it's an error message }
          WriteOutError('Illegal space at position 1 in "' + LogStr + '"');
          Exit;
        END ELSE BEGIN
          CASE LogStr[1] OF
            'A', 'a', '$',
            'P', 'p', 'B', 'b', 'S', 's', 'T', 't',
            'L', 'l',
            'R', 'r',
            'D', 'd',
            'G', 'g',
            'W', 'w',
            '*', '+',
            'E', 'e', 'X', 'x':
              BEGIN
                { extract the log character }
                TypeOfLog := Copy(LogStr, 1, 1);
                IF Copy(LogStr, 2, 1) = ' ' THEN BEGIN
                  LogStr := Copy(LogStr, 3);
                  RemoveGeneralInstructionsFromLogStr(LogStr);
                END ELSE BEGIN
                  { we've found a debug character }
                  TempStr := LogStr[2];
                  CASE TempStr[1] OF
                    'G', 'g':
                      BEGIN
                        LogStr := Copy(LogStr, 4);
                        RemoveGeneralInstructionsFromLogStr(LogStr);
                        IF LocoChipStr = '' THEN
                          Debug(LogStr)
                        ELSE
                          Debug(LocoChipStr + ': ' + LogStr);
                      END;
                    '!', '+', '=', '&': { bold; italics; or bold and italics; and green! }
                      BEGIN
                        LogStr := Copy(LogStr, 4);
                        RemoveGeneralInstructionsFromLogStr(LogStr);
                        IF LocoChipStr = '' THEN
                          Debug(TempStr + LogStr)
                        ELSE
                          Debug(TempStr + LocoChipStr + ': ' + LogStr);
                      END;
                  ELSE {CASE}
                    BEGIN
                      { there isn't one - write out the whole line in case it's an error message }
                      WriteOutError('Illegal debug character "' + LogStr[2] + '" at position 2 in "' + LogStr + '"');
                      Exit;
                    END;
                  END; {CASE}
                END;
              END;
          ELSE
            { not a character we recognise }
            BEGIN
              WriteOutError('Illegal log character "' + LogStr[1] + '" at position 1 in "' + LogStr + '"');
              Exit;
            END;
          END; {CASE}
        END;

        { Check whether we're writing the same thing over and over - complain after three repeats }
        IF CheckForIdenticalLinesInLog THEN BEGIN
          IdenticalLinesFound := False;
          J := 1;
          I := 1;
          WHILE (I <= MaxSaveLogStrs) AND NOT IdenticalLinesFound DO BEGIN
            IF LogStr <> SaveLogStrArray[I] THEN
              { reset the threshold }
              J := 1
            ELSE BEGIN
              Inc(J);
              IF J > 1 THEN BEGIN
                IdenticalLinesFound := True;
                IF NOT IdenticalLineMsgWritten THEN BEGIN
                  IdenticalLineMsgWritten := True;
                  WriteToEachLogFile(TypeOfLog[1], '', CurrentRailwayTimeStr, NoLocoChipStr, '[' + IntToStr(J) + ' identical lines found]');
                END;
              END;
            END;
            Inc(I);
          END; {WHILE}
        END;

        SaveLogStrArray[1] := SaveLogStrArray[2];
        SaveLogStrArray[2] := SaveLogStrArray[3];
        SaveLogStrArray[3] := SaveLogStrArray[4];
        SaveLogStrArray[4] := SaveLogStrArray[5];
        SaveLogStrArray[5] := SaveLogStrArray[6];
        SaveLogStrArray[6] := SaveLogStrArray[7];
        SaveLogStrArray[7] := SaveLogStrArray[8];
        SaveLogStrArray[8] := SaveLogStrArray[9];
        SaveLogStrArray[9] := SaveLogStrArray[10];
        SaveLogStrArray[10] := LogStr;

        LastLogLineStr := LogStr;
        TryStrToInt(LocoChipStr, LocoChip);
        LineAfterJustDrawn := False;

        IF NOT IdenticalLinesFound OR NOT CheckForIdenticalLinesInLog THEN BEGIN
          { See if we want to draw a line either on its own or before the given output }
          IF DrawLineInLogFileBefore OR DrawLineInLogFileOnly THEN BEGIN
            WriteToEachLogFile(TypeOfLog[1],
                               IfThen(LogCurrentTimeMode,
                                      TimeToHMSZStr(Time),
                                      NoCurrentTimeStr),
                               CurrentRailwayTimeStr,
                               NoLocoChipStr,
                               NoLogStr);
            TempStr := '';
            FOR I := 0 TO 70 DO
              TempStr := TempStr + DrawLineChar + ' ';
            { Truncate the string just in case }
            Delete(TempStr, 140, Length(TempStr) - 140); { these shouldn't be magic numbers **** }

            WriteToEachLogFile(TypeOfLog[1],
                               IfThen(LogCurrentTimeMode,
                                      TimeToHMSZStr(Time),
                                      NoCurrentTimeStr),
                               CurrentRailwayTimeStr,
                               NoLocoChipStr,
                               TempStr + '[' + UnitRef + ']');

            WriteToEachLogFile(TypeOfLog[1],
                               IfThen(LogCurrentTimeMode,
                                      TimeToHMSZStr(Time),
                                      NoCurrentTimeStr),
                               CurrentRailwayTimeStr,
                               NoLocoChipStr,
                               NoLogStr);

            IF DrawLineInLogFileOnly THEN
              LineAfterJustDrawn := True;
          END;

          IF NOT DrawLineInLogFileOnly AND (LogStr <> ' ') THEN BEGIN
            CurrentRailwayTimeStr := TimeToHMSStr(CurrentRailwayTime);
            IdenticalLineMsgWritten := False;

            IF BlankLineBefore THEN BEGIN
              { a blank line is required }
              IF NOT LogCurrentTimeMode THEN
                WriteToEachLogFile(TypeOfLog[1], NoCurrentTimeStr, CurrentRailwayTimeStr, NoLocoChipStr, NoLogStr)
              ELSE
                WriteToEachLogFile(TypeOfLog[1], TimeToHMSZStr(Time), CurrentRailwayTimeStr, NoLocoChipStr, NoLogStr);

              IF TestingMode THEN
                Flush(LargeLogFile);
              IF WriteToLogFileAndTestFile THEN
                WriteLn(TestLogFile, '');
            END;

            { Add LocoChip, if any, or else spaces }
            IF Length(LocoChipStr) < 4 THEN BEGIN
              LocoChipStr := LocoChipStr + StringOfChar(' ', 4 - Length(LocoChipStr));
              IF (LocoChipStr = '    ') OR (LocoChipStr = '0000') THEN
                LocoChipStr := '';
            END;
            IF (Indent = 0) AND (WrapNum = 0) THEN BEGIN
              { Add the time, and a UnitRef if supplied }
              WriteToEachLogFile(TypeOfLog[1],
                                 IfThen(LogCurrentTimeMode,
                                        TimeToHMSZStr(Time),
                                        NoCurrentTimeStr),
                                 CurrentRailwayTimeStr,
                                 IfThen(LocoChipStr = '',
                                        NoLocoChipStr,
                                        LocoChipStr),
                                 LogStr
                                 + IfThen(NoUnitRef,
                                          '',
                                          ' [' + UnitRef + ']'));
            END ELSE BEGIN
              { Do the indenting and line wrapping - it is assumed that the line wrapping happens at the given number regardless of the indent }
              IF WrapNum <> 0 THEN BEGIN
                IF WrapNum > 16 THEN
                  WrapNum := WrapNum - 16;

                IF LogCurrentTimeMode THEN
                  IF WrapNum > 13 THEN
                    WrapNum := WrapNum - 13;

                IF WrapNum > Indent THEN
                  WrapNum := WrapNum - Indent - 4;

                { Insert CRLF at or near WrapNum }
                LogStr := WrapText(LogStr, WrapNum);
              END;

              Count := 1;

              PossibleEllipsisStr := '';
              IF Length(LogStr) > 1 THEN BEGIN
                I := 1;
                WHILE I < Length(LogStr) DO BEGIN
                  IF (LogStr[I] = #13) AND (LogStr[I + 1] = #10) THEN BEGIN
                    WriteToEachLogFile(TypeOfLog[1],
                                       IfThen(LogCurrentTimeMode,
                                              TimeToHMSZStr(Time),
                                              NoCurrentTimeStr),
                                       CurrentRailwayTimeStr,
                                       IfThen(LocoChipStr = '',
                                              NoLocoChipStr,
                                              LocoChipStr),
                                       IfThen(Indent > 0,
                                              StringOfChar(' ', Indent))
                                       + IfThen(NumberLines,
                                                IntToStr(Count) + ': ')
                                       + IfThen(Count > 1,
                                                EllipsisStr + ' ')
                                       + LeftStr(LogStr, I - 1)
                                       + EllipsisStr);
                    Inc(Count);
                    LogStr := Copy(LogStr, I + 2);
                    I := 0;
                    PossibleEllipsisStr := '... ';
                  END;
                  Inc(I);
                END;
                { Now write out the last bit }
                WriteToEachLogFile(TypeOfLog[1],
                                   IfThen(LogCurrentTimeMode,
                                          TimeToHMSZStr(Time),
                                          NoCurrentTimeStr),
                                   CurrentRailwayTimeStr,
                                   IfThen(LocoChipStr = '',
                                          NoLocoChipStr,
                                          LocoChipStr),
                                   IfThen(Indent > 0,
                                          StringOfChar(' ', Indent))
                                   + IfThen(NumberLines,
                                            IntToStr(Count) + ': ')
                                   + PossibleEllipsisStr
                                   + LogStr);
              END;
            END;

            { See if we want to draw a line after the given output }
            IF DrawLineInLogFileAfter THEN BEGIN
              WriteToEachLogFile(TypeOfLog[1], NoCurrentTimeStr, CurrentRailwayTimeStr, NoLocoChipStr, NoLogStr);
              TempStr := '';
              FOR I := 0 TO 70 DO
                TempStr := TempStr + DrawLineChar + ' ';
              WriteToEachLogFile(TypeOfLog[1], NoCurrentTimeStr, CurrentRailwayTimeStr, NoLocoChipStr, '[' + UnitRef + ']');
              WriteToEachLogFile(TypeOfLog[1], NoCurrentTimeStr, CurrentRailwayTimeStr, NoLocoChipStr, NoLogStr);

              LineAfterJustDrawn := True;
            END;
          END;

          { If we're in TestingMode, write things out immediately so they can be checked }
          IF TestingMode AND LogFileOpen THEN BEGIN
            IF TypeOfLog <> ' ' THEN
              Flush(LargeLogFile);
            CASE TypeOfLog[1] OF
              ' ':
                { ignore - this is for messages that we may at some stage want to log }
                ;
              'A', 'a', '$':
                { general happenings of relevance to all logs - includes '$' for quick debugging as it is easy to find in the log }
                IF MultipleLogFilesRequired THEN BEGIN
                  Flush(LocoLogFile);
                  Flush(RouteLogFile);
                  Flush(SignalPointAndTCLogFile);
                  Flush(DiagramsLogFile);
                  Flush(WorkingTimetableLogFile);
                END;
              'P', 'p', 'B', 'b', 'S', 's', 'T', 't' :
                IF MultipleLogFilesRequired THEN
                  Flush(SignalPointAndTCLogFile);
              'L', 'l':
                IF MultipleLogFilesRequired THEN
                  Flush(LocoLogFile);
              'R', 'r':
                IF MultipleLogFilesRequired THEN
                  Flush(RouteLogFile);
              'D', 'd':
                IF MultipleLogFilesRequired THEN
                  Flush(DiagramsLogFile);
              'W', 'w':
                begin Flush(TestLogFile);
                  IF MultipleLogFilesRequired THEN
                    Flush(WorkingTimetableLogFile);
                end;
              '*', '+':
                Flush(TestLogFile);
              'E', 'e', 'X', 'x': { unusual happenings of relevance to all the log files }
                IF MultipleLogFilesRequired THEN BEGIN
                  Flush(LocoLogFile);
                  Flush(RouteLogFile);
                  Flush(SignalPointAndTCLogFile);
                  Flush(DiagramsLogFile);
                  Flush(ErrorLogFile);
                  Flush(WorkingTimetableLogFile);
                END;
            END; {CASE}
          END;
        END;
      END;
    END;
  EXCEPT {TRY}
    ON E : Exception DO
      { Cannot call Log here as we are already in it }
      Debug('EG WriteToLogFile: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { WriteToLogFile }

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE WriteStringArrayToLogMainProcedure(LocoChipStr : String; TypeOfLogChar : Char; InitStr : String; StringArray : StringArrayType; Indent, WrapNum : Integer;
                                             NumberEachElement : Boolean; BreakOnStr : String);
{ Write the contents of an array to the log file, wrapping it at Wrapnum and indenting it by Indent }
CONST
  UnknownAsZeroes = True;

VAR
  BreakFound : Boolean;
  DebugStr : String;
  I : Integer;
  SaveBreak : Boolean;

BEGIN
  WrapNum := WrapNum - 25; { 20 is the amount of space taken up by the time and loco chip, etc. }

  { if there's an introductory string, write it first }
  IF InitStr <> '' THEN
    Log(UnknownLocoChipAsZeroesStr + ' ' + TypeOfLogChar + ' ' + InitStr);

  DebugStr := '';
  BreakFound := False;
  SaveBreak := False;
  FOR I := 0 TO High(StringArray) DO BEGIN
    IF Pos(BreakOnStr, StringArray[I]) > 0 THEN BEGIN
      { when a break is found, write out the previous line - indenting it if there was not a previous break }
      BreakFound := True;
      IF DebugStr <> '' THEN BEGIN
        IF NOT SaveBreak THEN
          Log(UnknownLocoChipAsZeroesStr + ' ' + TypeOfLogChar + StringOfChar(' ', Indent + 1) + ': ' + DebugStr + ' {NOUNITREF}')
        ELSE BEGIN
          Log(UnknownLocoChipAsZeroesStr + ' ' + TypeOfLogChar + StringOfChar(' ', Indent + 1) + ': ' + StringOfChar(' ', 5) + DebugStr + ' {NOUNITREF}');
          SaveBreak := False;
        END;
        DebugStr := '';
      END;
    END;

    IF (DebugStr <> '') AND (Length(DebugStr) > WrapNum) THEN BEGIN
      IF NOT BreakFound THEN
        Log(UnknownLocoChipAsZeroesStr + ' ' + TypeOfLogChar + StringOfChar(' ', Indent + 1) + ': ' + StringOfChar(' ', 5) + DebugStr + ' {NOUNITREF}')
      ELSE BEGIN
        Log(UnknownLocoChipAsZeroesStr + ' ' + TypeOfLogChar + StringOfChar(' ', Indent + 1) + ': ' + DebugStr + ' {NOUNITREF}');
        SaveBreak := True;
        BreakFound := False;
      END;
      DebugStr := '';
    END;

    DebugStr := DebugStr + StringArray[I] + ' ';

  END; {FOR}

  { and tidy up }
  IF DebugStr <> '' THEN BEGIN
    IF NOT BreakFound THEN
      Log(UnknownLocoChipAsZeroesStr + ' ' + TypeOfLogChar + StringOfChar(' ', Indent + 1) + ': ' + StringOfChar(' ', 5) + DebugStr + ' {NOUNITREF}')
    ELSE
      Log(UnknownLocoChipAsZeroesStr + ' ' + TypeOfLogChar + StringOfChar(' ', Indent + 1) + ': ' + DebugStr + ' {NOUNITREF}');
  END;
END; { WriteStringArrayToLogMainProcedure }

PROCEDURE WriteStringArrayToLog{1}(LocoChipStr : String; TypeOfLogChar : Char; StringArray : StringArrayType; Indent, WrapNum : Integer); Overload;
{ Write the contents of an array to the replay file, wrapping it at Wrapnum and indenting it by Indent: this version isn't preceded by an explanatory string }
CONST
  NoBreakOnStr = '';

BEGIN
  WriteStringArrayToLogMainProcedure(LocoChipStr, TypeOfLogChar, '', StringArray, Indent, WrapNum, NOT NumberElements, NoBreakOnStr);
END; { WriteStringArrayToLogFile-1}

PROCEDURE WriteStringArrayToLog{2}(LocoChipStr : String; TypeOfLogChar : Char; Str: String; StringArray : StringArrayType; Indent, WrapNum : Integer); Overload;
{ Write the contents of an array to the replay file, wrapping it at Wrapnum and indenting it by Indent: this version is preceded by an explanatory string }
CONST
  NoBreakOnStr = '';

BEGIN
  WriteStringArrayToLogMainProcedure(LocoChipStr, TypeOfLogChar, Str, StringArray, Indent, WrapNum, NOT NumberElements, NoBreakOnStr);
END; { WriteStringArrayToLogFile-2}

PROCEDURE WriteStringArrayToLog{3}(LocoChipStr : String; TypeOfLogChar : Char; Str: String; StringArray : StringArrayType; Indent, WrapNum : Integer; BreakOnStr : String);
                                   Overload;
{ Write the contents of an array to the replay file, wrapping it at Wrapnum and indenting it by Indent: this version is preceded by an explanatory string }
BEGIN
  WriteStringArrayToLogMainProcedure(LocoChipStr, TypeOfLogChar, Str, StringArray, Indent, WrapNum, NOT NumberElements, BreakOnStr);
END; { WriteStringArrayToLogFile-3}

PROCEDURE WriteStringArrayToLog{4}(LocoChipStr : String; TypeOfLogChar : Char; StringArray : StringArrayType; Indent, WrapNum : Integer; BreakOnStr : String); Overload;
{ Write the contents of an array to the replay file, wrapping it at Wrapnum and indenting it by Indent: this version is not preceded by an explanatory string. If the
  BreakOnStr string appears in the text, start a new line before it.
}
BEGIN
  WriteStringArrayToLogMainProcedure(LocoChipStr, TypeOfLogChar, '', StringArray, Indent, WrapNum, NOT NumberElements, BreakOnStr);
END; { WriteStringArrayToLogFile-4}

PROCEDURE WriteStringArrayToLog{5}(LocoChipStr : String; TypeOfLogChar : Char; StringArray : StringArrayType); Overload;
{ Write the contents of an array to the replay file, wrapping it at WrapNum and indenting it by two - this version is not preceded by an explanatory string }
CONST
  NoBreakOnStr = '';
  WrapNum = 190;

BEGIN
  WriteStringArrayToLogMainProcedure(LocoChipStr, TypeOfLogChar, '', StringArray, 2, WrapNum, NOT NumberElements, NoBreakOnStr);
END; { WriteStringArrayToLogFile-5}

PROCEDURE WriteStringArrayToLog{6}(LocoChipStr : String; TypeOfLogChar : Char; Str: String; StringArray : StringArrayType); Overload;
{ Write the contents of an array to the replay file, wrapping it by default at 140 and indenting it by two - this version is preceded by an explanatory string }
CONST
  NoBreakOnStr = '';
  WrapNum = 190;

BEGIN
  WriteStringArrayToLogMainProcedure(LocoChipStr, TypeOfLogChar, Str, StringArray, 2, WrapNum, NOT NumberElements, NoBreakOnStr);
END; { WriteStringArrayToLogFile-6}

PROCEDURE WriteStringArrayToLog{7}(LocoChipStr : String; TypeOfLogChar : Char; Str : String; StringArray : StringArrayType;  NumberElements : Boolean); Overload;
{ Write the contents of an array to the replay file, wrapping it at 140 and indenting it by two - this version is preceded by an explanatory string. If NumberElements is
  true, add number in array to each element
}
CONST
  NoBreakOnStr = '';
  WrapNum = 190;

BEGIN
  WriteStringArrayToLogMainProcedure(LocoChipStr, TypeOfLogChar, Str, StringArray, 2, WrapNum, NumberElements, NoBreakOnStr);
END; { WriteStringArrayToLogFile-7}

PROCEDURE WriteStringArrayToLog{8}(LocoChipStr : String; TypeOfLogChar : Char; Str : String; StringArray : StringArrayType; NumberElements : Boolean; BreakOnStr : String);
                                   Overload;
{ Write the contents of an array to the replay file, wrapping it at 140 and indenting it by two - this version is preceded by an explanatory string. If NumberElements is
  true, add number in array to each element. If the BreakOnStr string appears in the text, start a new line before it.
}
CONST
  WrapNum = 190;

BEGIN
  WriteStringArrayToLogMainProcedure(LocoChipStr, TypeOfLogChar, Str, StringArray, 2, WrapNum, NumberElements, BreakOnStr);
END; { WriteStringArrayToLogFile-8}

PROCEDURE WriteStringArrayToLog{9}(TypeOfLogChar : Char; Str: String; StringArray : StringArrayType; Indent, WrapNum : Integer; BreakOnStr : String); Overload;
{ Write the contents of an array to the replay file, wrapping it at Wrapnum and indenting it by Indent: this version is preceded by an explanatory string }
BEGIN
  WriteStringArrayToLogMainProcedure(UnknownLocoChipStr, TypeOfLogChar, Str, StringArray, Indent, WrapNum, NOT NumberElements, BreakOnStr);
END; { WriteStringArrayToLogFile-9}

PROCEDURE WriteTimeToLog(Num : Integer);
{ A debugging procedure whereby the current time is written to the log, together with the elapsed time since the previous call to the routine }
BEGIN
  IF PreviousLogTime = 0 THEN
    Log('#' + IntToStr(Num) + ': ' + TimeToHMSZStr(Time))
  ELSE
    Log('#' + IntToStr(Num) + ': ' + TimeToHMSZStr(Time) + ' (' + IntToStr(MilliSecondsBetween(Time, PreviousLogTime)) + 'ms)');
  PreviousLogTime := Time;
END; { WriteTimeToLog }

PROCEDURE UnknownLocoChipFound(RoutineName : String);
{ Called if an unknown loco chip is found at the beginning of a subroutine }
BEGIN
  Log('X! Unknown locochip found in routine ' + RoutineName);
  ASM
    Int 3
  END; {ASM}
END; { UnknownLocoChipFound }

PROCEDURE UnknownLocoRecordFound(RoutineName : String);
{ Called if an unknown loco record is found at the beginning of a subroutine }
BEGIN
  Log('X! Unknown loco record found in routine ' + RoutineName);
  ASM
    Int 3
  END; {ASM}
END; { UnknownLocoRecordFound }

PROCEDURE UnknownTrainRecordFound(RoutineName : String);
{ Called if an unknown train record is found at the beginning of a subroutine }
BEGIN
  Log('X! Unknown train record found in routine ' + RoutineName);
  ASM
    Int 3
  END; {ASM}
END; { UnknownTrainRecordFound }

FUNCTION AllJourneysComplete(T : TrainIndex) : Boolean;
{ Returns true if all a train's journeys are complete }
VAR
  JourneyCount : Integer;

BEGIN
  Result := True;

  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('AllJourneysComplete')
  ELSE BEGIN
    JourneyCount := 0;
    WHILE JourneyCount <= High(Trains[T].Train_JourneysArray) DO BEGIN
      IF NOT Trains[T].Train_JourneysArray[JourneyCount].TrainJourney_Cleared THEN
        Result := False;
      Inc(JourneyCount);
    END; {WHILE}
  END;
END; { AllJourneysComplete }

PROCEDURE AddLightsToLightsToBeSwitchedOnArray(T : TrainIndex; DesiredDirection1, DesiredDirection2 : DirectionType; MinSeconds, MaxSeconds : Integer; LightsOnTime : TDateTime);
{ Set up a train's lights to switch on at a random time ahead }
VAR
  DebugStr : String;
  Seconds : Integer;
  SwitchOnTime : TDateTime;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('AddLightsToLightsToBeSwitchedOnArray')
  ELSE BEGIN
    SetLength(LightsToBeSwitchedOnArray, Length(LightsToBeSwitchedOnArray) + 1);
    WITH LightsToBeSwitchedOnArray[High(LightsToBeSwitchedOnArray)] DO BEGIN
      LightsToBeSwitchedOn_Direction1 := DesiredDirection1;
      LightsToBeSwitchedOn_Direction2 := DesiredDirection2;
      IF DesiredDirection2 = UnknownDirection THEN
        DebugStr := 'L Switch on time for ' + DirectionToStr(DesiredDirection1) + ' lights'
      ELSE BEGIN
        IF DesiredDirection1 = Up THEN
          LightsToBeSwitchedOn_ColourStr1 := 'white'
        ELSE
          LightsToBeSwitchedOn_ColourStr1 := 'red';
        IF DesiredDirection2 = Up THEN
          LightsToBeSwitchedOn_ColourStr2 := 'red'
        ELSE
          LightsToBeSwitchedOn_ColourStr2 := 'white';

        DebugStr := 'L Switch on time for ' + LightsToBeSwitchedOn_ColourStr1 + ' lights at up' + ' and ' + LightsToBeSwitchedOn_ColourStr2 + ' lights at down';
      END;

      Seconds := MaxSeconds - MinSeconds;
      Seconds := Random(Seconds);
      Seconds := Seconds + MinSeconds;

      SwitchOnTime := IncSecond(LightsOnTime, Seconds);
      DebugStr := DebugStr + ' set to ' + TimeToHMSStr(SwitchOnTime) + ' (' + IntToStr(Seconds) + ' = between ' + IntToStr(MinSeconds)
                  + ' and ' + IntToStr(MaxSeconds) + ' seconds of ' + TimeToHMSStr(LightsOnTime) + ')';
      Log(Trains[T].Train_LocoChipStr + ' X ' + DebugStr);

      LightsToBeSwitchedOn_SwitchOnTime := SwitchOnTime;
      LightsToBeSwitchedOn_Train := T;
    END; {WITH}
  END;
END; { AddLightsToLightsToBeSwitchedOnArray }

PROCEDURE AddRichLine(RichEdit: TRichEdit; StrToAdd : String);
{ Taken from Delpi Pages (http://www.delphipages.com/tips/thread.cfm?ID=186) - by Slavikn, WebPage: http://www.organizermp3.com }
VAR
  RichTextFound : Boolean;
  StrLeft: String;
  TempStyle: TFontStyles;
  TempStr: String;

  FUNCTION FromLeftUntilStr(VAR OriginalStr : String; CONST UntilStr : String; CONST ToEndIfNotFound, Trim : Boolean): String;
  VAR
    TempPos: Integer;

  BEGIN
    TempPos := Pos(UntilStr, OriginalStr);
    IF TempPos > 0 THEN BEGIN
      Result := Copy(OriginalStr, 1, TempPos - 1);
      IF Trim THEN
        Delete(OriginalStr, 1, TempPos - 1);
    END ELSE BEGIN
      IF ToEndIfNotFound THEN BEGIN
        Result := OriginalStr;
        IF Trim THEN
          OriginalStr := '';
      END ELSE
        Result := '';
    END;
  END;

  FUNCTION StrStartsWith(VAR OriginalStr : String; CONST StartsWith : String; CONST IgnoreCase, Trim : Boolean): Boolean;
  VAR
    PartOfOriginalStr: String;
    NewStartsWith: String;

  BEGIN
    PartOfOriginalStr := Copy(OriginalStr, 1, Length(StartsWith));
    NewStartsWith := StartsWith;

    IF IgnoreCase THEN BEGIN
      PartOfOriginalStr := LowerCase(PartOfOriginalStr);
      NewStartsWith := LowerCase(NewStartsWith);
    END;

    Result := PartOfOriginalStr = NewStartsWith;

    IF (Result = True) And (Trim = True) THEN
      Delete(OriginalStr, 1, Length(NewStartsWith));
  END;

  PROCEDURE AddToStyle(VAR Style: TFontStyles; AStyle: TFontStyle);
  BEGIN
    IF Not (AStyle In Style) THEN
      Style := Style + [AStyle];
  END;

  PROCEDURE RemoveFromStyle(VAR Style: TFontStyles; AStyle: TFontStyle);
  BEGIN
    IF AStyle In Style THEN
      Style := Style - [AStyle];
  END;

BEGIN
  RichTextFound := False; { need to use this to allow lines with single < (not yet implemented) ***** }
  IF Pos('{R}', StrToAdd) > 0 THEN
    StrToAdd := Copy(StrToAdd, 4);
  TempStyle := RichEdit.Font.Style;
  StrLeft := StrToAdd;
  RichEdit.SelStart := Length(RichEdit.Text);
  WHILE StrLeft <> '' DO BEGIN
    IF StrStartsWith(StrLeft, '<', True, False) THEN BEGIN

      { Bold }
      IF StrStartsWith(StrLeft, '<b>', True, True) THEN BEGIN
        RichTextFound := True;
        AddToStyle(TempStyle, fsBold);
      END;
      IF StrStartsWith(StrLeft, '</b>', True, True) THEN BEGIN
        RichTextFound := True;
        RemoveFromStyle(TempStyle, fsBold);
      END;

      { Italics }
      IF StrStartsWith(StrLeft, '<i>', True, True) THEN BEGIN
        RichTextFound := True;
        AddToStyle(TempStyle, fsItalic);
      END;
      IF StrStartsWith(StrLeft, '</i>', True, True) THEN BEGIN
        RichTextFound := True;
        RemoveFromStyle(TempStyle, fsItalic);
      END;

      { Underline }
      IF StrStartsWith(StrLeft, '<u>', True, True) THEN BEGIN
        RichTextFound := True;
        AddToStyle(TempStyle, fsUnderLine);
      END;
      IF StrStartsWith(StrLeft, '</u>', True, True) THEN BEGIN
        RichTextFound := True;
        RemoveFromStyle(TempStyle, fsUnderLine);
      END;

      { Colour [Note: the background colour can only be set by changing the colour of the RichEdit control] }
      IF (StrStartsWith(StrLeft, '</color>', True, True)) OR (StrStartsWith(StrLeft, '</colour>', True, True)) THEN BEGIN
        RichTextFound := True;
        RichEdit.SelAttributes.Color := RichEdit.Font.Color;
      END;
      IF (StrStartsWith(StrLeft, '<color=', True, True)) OR (StrStartsWith(StrLeft, '<colour=', True, True)) THEN BEGIN
        RichTextFound := True;
        TempStr := FromLeftUntilStr(StrLeft, '>', False, True);
        TRY
          RichEdit.SelAttributes.Color := StrToColour(TempStr);
        EXCEPT
          RichEdit.SelAttributes.Color := RichEdit.Font.Color;
        END; {TRY}
        Delete(StrLeft, 1, 1);
      END;

      IF RichTextFound THEN
        RichTextFound := False
      ELSE
        Delete(StrLeft, 1, 1);
    END ELSE BEGIN
      RichEdit.SelAttributes.Style := TempStyle;
      RichEdit.SelText := FromLeftUntilStr(StrLeft, '<', True, True);
    END;

    RichEdit.SelStart := Length(RichEdit.Text);
  END;
  RichEdit.SelText := #13#10;
END; { AddRichLine }

PROCEDURE AddStoredRichEditLoggingTextToLoggingWindow;
{ Add any stored rich-edit data to the logging window }
VAR
  StoredRichEditLoggingTextArrayCount : Integer;

BEGIN
  StoredRichEditLoggingTextArrayCount := 0;
  WHILE StoredRichEditLoggingTextArrayCount <> Length(StoredRichEditLoggingTextArray) DO BEGIN
    AddRichLine(LoggingWindow.LoggingWindowRichEdit, StoredRichEditLoggingTextArray[StoredRichEditLoggingTextArrayCount]);
    Inc(StoredRichEditLoggingTextArrayCount);
  END; {WHILE}

  SetLength(StoredRichEditLoggingTextArray, 0);
END; { AddStoredRichEditLoggingTextToLoggingWindow }

PROCEDURE AppendIntegerArray2ToIntegerArray1(VAR IntegerArray1 : IntegerArrayType; IntegerArray2 : IntegerArrayType);
{ Join two integer arrays together }
VAR
  I : Integer;

BEGIN
  FOR I := 0 TO High(IntegerArray2) DO
    AppendToIntegerArray(IntegerArray1, IntegerArray2[I]);
END; { AppendIntegerArray2ToIntegerArray1 }

PROCEDURE AppendStringArray2ToStringArray1(VAR StringArray1 : StringArrayType; StringArray2 : StringArrayType);
{ Join two string arrays together }
VAR
  I : Integer;

BEGIN
  FOR I := 0 TO High(StringArray2) DO
    AppendToStringArray(StringArray1, StringArray2[I]);
END; { AppendStringArray2ToStringArray1 }

PROCEDURE AppendToAreaArray(VAR AreaArray : IntegerArrayType; NewElement : Integer);
{ Appends an area to the array }
BEGIN
  SetLength(AreaArray, Length(AreaArray) + 1);
  AreaArray[High(AreaArray)] := NewElement;
END; { AppendToAreaArray }

PROCEDURE AppendToBooleanArray(VAR BooleanArray : BooleanArrayType; NewElement : Boolean);
{ Appends a boolean value to the array }
BEGIN
  SetLength(BooleanArray, Length(BooleanArray) + 1);
  BooleanArray[High(BooleanArray)] := NewElement;
END; { AppendToBooleanArray }

PROCEDURE AppendToDateTimeArray(VAR DateTimeArray : DateTimeArrayType; NewElement : TDateTime);
{ Appends a TDateTime value to the array }
BEGIN
  { Append to the end of the array }
  SetLength(DateTimeArray, Length(DateTimeArray) + 1);
  DateTimeArray[High(DateTimeArray)] := NewElement;
END; { AppendToTDateTimeArray }

PROCEDURE AppendToDirectionArray(VAR DirectionArray : DirectionArrayType; NewElement : DirectionType);
{ Appends a direction value to the array }
BEGIN
  SetLength(DirectionArray, Length(DirectionArray) + 1);
  DirectionArray[High(DirectionArray)] := NewElement;
END; { AppendToDirectionArray }

PROCEDURE AppendToIntegerArray(VAR IntegerArray : IntegerArrayType; NewElement : Integer);
{ Appends an integer value to the array }
BEGIN
  SetLength(IntegerArray, Length(IntegerArray) + 1);
  IntegerArray[High(IntegerArray)] := NewElement;
END; { AppendToIntegerArray }

PROCEDURE AppendToLineArray(VAR LineArray : IntegerArrayType; NewElement : Integer);
{ Appends a line name to the array }
BEGIN
  SetLength(LineArray, Length(LineArray) + 1);
  LineArray[High(LineArray)] := NewElement;
END; { AppendToLineArray }

PROCEDURE AppendToLocationArray(VAR LocationArray : IntegerArrayType; NewElement : Integer);
{ Appends a location to the array }
BEGIN
  SetLength(LocationArray, Length(LocationArray) + 1);
  LocationArray[High(LocationArray)] := NewElement;
END; { AppendToLocationArray }

PROCEDURE AppendToNewLineArray(VAR NewLineArray : IntegerArrayType; NewElement : Integer);
{ Appends a newly-created line to the temporary new-lines array }
BEGIN
  SetLength(NewLineArray, Length(NewLineArray) + 1);
  NewLineArray[High(NewLineArray)] := NewElement;
END; { AppendToLineArray }

PROCEDURE AppendToStringArray(VAR StringArray : StringArrayType; NewElement : String);
{ Appends a string value to the array }
BEGIN
  { Append to the end of the array }
  SetLength(StringArray, Length(StringArray) + 1);
  StringArray[High(StringArray)] := NewElement;
END; { AppendToStringArray }

PROCEDURE AppendToTrainArray(VAR TrainArray : TrainArrayType; NewElement : TrainIndex);
{ Appends a given train array value to the array }
BEGIN
  SetLength(TrainArray, Length(TrainArray) + 1);
  TrainArray[High(TrainArray)] := NewElement;
END; { AppendToTrainArray }

PROCEDURE AppendToTrainTypeArray(VAR TrainTypes : TrainTypeArray; NewElement : TypeOfTrainType);
{ Appends a TrainType value to the array }
BEGIN
  SetLength(TrainTypes, Length(TrainTypes) + 1);
  TrainTypes[High(TrainTypes)] := NewElement;
END; { AppendToTrainTypeArray }

FUNCTION AspectToStr{1}(Aspect : AspectType): String; Overload;
{ Return the signal aspect as a long string }
BEGIN
  CASE Aspect OF
    FlashingDoubleYellowAspect:
      Result := FlashingDoubleYellowAspectStr;
    FlashingSingleYellowAspect:
      Result := FlashingSingleYellowAspectStr;
    DoubleYellowAspect:
      Result := DoubleYellowAspectStr;
    SingleYellowAspect:
      Result := SingleYellowAspectStr;
    GreenAspect:
      Result := GreenAspectStr;
    RedAspect:
      Result := RedAspectStr;
    NoAspect:
      Result := NoAspectStr;
  END; {CASE}
END; { AspectToStr-1 }

FUNCTION AspectToStr{2}(Aspect : AspectType; LongOrShortString : StringType) : String; Overload;
{ Return the signal aspect as either a short or a long string }
BEGIN
  IF LongOrShortString = LongStringType THEN BEGIN
    CASE Aspect OF
      FlashingDoubleYellowAspect:
        Result := FlashingDoubleYellowAspectStr;
      FlashingSingleYellowAspect:
        Result := FlashingSingleYellowAspectStr;
      DoubleYellowAspect:
        Result := DoubleYellowAspectStr;
      SingleYellowAspect:
        Result := SingleYellowAspectStr;
      GreenAspect:
        Result := GreenAspectStr;
      RedAspect:
        Result := RedAspectStr;
      NoAspect:
        Result := NoAspectStr;
    END; {CASE}
  END ELSE BEGIN
    CASE Aspect OF
      FlashingDoubleYellowAspect:
        Result := FlashingDoubleYellowAspectShortStr;
      FlashingSingleYellowAspect:
        Result := FlashingSingleYellowAspectShortStr;
      DoubleYellowAspect:
        Result := DoubleYellowAspectShortStr;
      SingleYellowAspect:
        Result := SingleYellowAspectShortStr;
      GreenAspect:
        Result := GreenAspectShortStr;
      RedAspect:
        Result := RedAspectShortStr;
      NoAspect:
        Result := NoAspectShortStr;
    END; { CASE }
  END;
END; { AspectToStr-2 }

FUNCTION CardinalMulDiv(A, B, C : Cardinal) : Cardinal;
{ Returns (A * B) / C; intermediate result is held double-length to avoid overflow }
VAR
  X : Cardinal;

BEGIN
  X := (A * B);
  Result := X DIV C;
END; { CardinalMulDiv }

PROCEDURE CheckSemaphoreDistantBeforeSemaphoreHomeCleared(S : Integer);
{ Sees if semaphore distants need automatically to be reset to allow a semaphore home to be put on }
VAR
  I : Integer;

BEGIN
  IF (Signals[S].Signal_Type = SemaphoreHome) AND (Signals[S].Signal_SemaphoreDistantLocking <> UnknownSignal) THEN BEGIN
    I := 0;
    WHILE I <= High(Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_SemaphoreDistantHomesArray) DO BEGIN
      { unlock the semaphore home signals }
      IF Signals[Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_SemaphoreDistantHomesArray[I]].Signal_Aspect = RedAspect THEN
        Signals[Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_SemaphoreDistantHomesArray[I]].Signal_LockedBySemaphoreDistant := False;
      Inc(I);
    END; {WHILE}

    { and set the distant on }
    Signals[Signals[S].Signal_SemaphoreDistantLocking].Signal_Aspect := RedAspect;
  END;
END; { CheckSemaphoreDistantBeforeSemaphoreHomeCleared }

FUNCTION ColourToStr(Colour : TColour) : String;
{ Checks if it's a Delphi colour or an FWP one }
BEGIN
  IF NOT ColorToIdent(Colour, Result) THEN BEGIN
    CASE Colour OF
      clFWPDkBlue:
        Result := clFWPDkBlueStr;
      clFWPOrange:
        Result := clFWPOrangeStr;
      clFWPLtBrown:
        Result := clFWPLtBrownStr;
      clFWPDkBrown:
        Result := clFWPDkBrownStr;
      clFWPPink:
        Result := clFWPPinkStr;
      clFWPDkGrey:
        Result := clFWPDkGreyStr;
      clFWPVeryDkGrey:
        Result := clFWPVeryDkGreyStr;
      clFWPPlatformColour:
        Result := clFWPPlatformColourStr;
    ELSE
      Result := UnknownColourStr + ': ' + IntToStr(Colour);
    END; {CASE}
  END;
END; { ColourToStr }

FUNCTION ColourToStrForUser(Colour : TColour) : String;
{ Checks if it's a Delphi colour or an FWP one, and removes the "cl" or "clFWP" }
BEGIN
  Result := ColourToStr(Colour);
  IF Copy(Result, 1, 5) = 'clFWP' THEN
    Result := Copy(Result, 6)
  ELSE
    IF Copy(Result, 1, 2) = 'cl' THEN
      Result := Copy(Result, 3);
  Result := LowerCase(Result);
END; { ColourToStrForUser }

FUNCTION ControlledByStateToStr(ControlledByState : LocoControlStateType) : String;
{ Return a string describing loco control }
BEGIN
  CASE ControlledByState OF
    ControlledByProgram:
      Result := 'by program';
    ControlledByUser:
      Result := 'by user';
    ControlledByRDC:
      Result := 'by RDC';
    ControlledByUnknownDevice:
      Result := 'by UnknownDevice';
  END; {CASE}
END; { ControlledByStateToStr }

FUNCTION CountSubRoutes(RouteArray : StringArrayType) : Integer;
{ Return how many subroutes there are in a given route array }
VAR
  I : Integer;

BEGIN
  Result := 0;
  FOR I := 0 TO High(RouteArray) DO BEGIN
    IF Pos('SR=', RouteArray[I]) > 0 THEN
      Inc(Result);
  END; {FOR}
END; { CountSubRoutes }

FUNCTION CursorToStr(Cursor : TCursor) : String;
{ Returns a string describing the cursor supplied }
BEGIN
  CASE Cursor OF
    crDefault:
      Result := 'Default';
    crAppStart:
      Result := 'AppStart';
    crArrow:
      Result := 'Arrow';
    crCross:
      Result := 'Cross';
    crDrag:
      Result := 'Drag';
    crHandPoint:
      Result := 'HandPoint';
    crHelp:
      Result := 'Help';
    crHourGlass:
      Result := 'HourGlass';
    crHSplit:
      Result := 'HSplit';
    crIBeam:
      Result := 'IBeam';
    crMultiDrag:
      Result := 'MultiDrag';
    crNo:
      Result := 'No';
    crNoDrop:
      Result := 'NoDrop';
    crNone:
      Result := 'None';
    crSize:
      Result := 'Size';
    crSizeNESW:
      Result := 'SizeNESW';
    crSizeNS:
      Result := 'SizeNS';
    crSizeNWSE:
      Result := 'SizeNWSE';
    crSizeWE:
      Result := 'SizeWE';
    crSQLWait:
      Result := 'SQLWait';
    crUpArrow:
      Result := 'UpArrow';
    crVSplit:
      Result := 'VSplit';
  ELSE {CASE}
    Result := 'Unknwon Cursor';
  END; {CASE}
END; { CursorToStr }

FUNCTION DayOfTheWeekToStr{1}(DayOfTheWeek : DayOfTheWeekType; LongOrShortString : StringType) : String; Overload;
{ Return a long or short string with the given day of the week }
BEGIN
  CASE DayOfTheWeek OF
    Monday:
      IF LongOrShortString = ShortStringType THEN
        Result := MondayShortStr
      ELSE
        Result := MondayStr;
    Tuesday:
      IF LongOrShortString = ShortStringType THEN
        Result := TuesdayShortStr
      ELSE
        Result := TuesdayStr;
    Wednesday:
      IF LongOrShortString = ShortStringType THEN
        Result := WednesdayShortStr
      ELSE
        Result := WednesdayStr;
    Thursday:
      IF LongOrShortString = ShortStringType THEN
        Result := ThursdayShortStr
      ELSE
        Result := ThursdayStr;
    Friday:
      IF LongOrShortString = ShortStringType THEN
        Result := FridayShortStr
      ELSE
        Result := FridayStr;
    Saturday:
      IF LongOrShortString = ShortStringType THEN
        Result := SaturdayShortStr
      ELSE
        Result := SaturdayStr;
    Sunday:
      IF LongOrShortString = ShortStringType THEN
        Result := SundayShortStr
      ELSE
        Result := SundayStr;
    UnknownDayofTheWeek:
      IF LongOrShortString = ShortStringType THEN
        Result := '?'
      ELSE
        Result := UnknownDayOfTheWeekStr;
  END; {CASE}
END; { DayOfTheWeekToStr-1 }

FUNCTION DayOfTheWeekToStr{2}(DayOfTheWeek : DayOfTheWeekType) : String; Overload;
{ Return a long string with the given day of the week }
BEGIN
  Result := DayOfTheWeekToStr(DayOfTheWeek, LongStringType);
END; { DayOfTheWeekToStr-2 }

FUNCTION DaysOfTheWeekSetToStr(DaysOfTheWeek : DaysOfTheWeekSetType) : String; Overload;
{ Return a string with given days of the week }
BEGIN
  Result := '';
  IF Monday IN DaysOfTheWeek THEN
    Result := Result + DayOfTheWeekToStr(Monday, ShortStringType);
  IF Tuesday IN DaysOfTheWeek THEN
    Result := Result + DayOfTheWeekToStr(Tuesday, ShortStringType);
  IF Wednesday IN DaysOfTheWeek THEN
    Result := Result + DayOfTheWeekToStr(Wednesday, ShortStringType);
  IF Thursday IN DaysOfTheWeek THEN
    Result := Result + DayOfTheWeekToStr(Thursday, ShortStringType);
  IF Friday IN DaysOfTheWeek THEN
    Result := Result + DayOfTheWeekToStr(Friday, ShortStringType);
  IF Saturday IN DaysOfTheWeek THEN
    Result := Result + DayOfTheWeekToStr(Saturday, ShortStringType);
  IF Sunday IN DaysOfTheWeek THEN
    Result := Result + DayOfTheWeekToStr(Sunday, ShortStringType);
END; { DaysOfTheWeekSetToStr }

FUNCTION DaysOfTheWeekSetToStr(DaysOfTheWeek : DaysOfTheWeekSetType; LongOrShortString : StringType) : String; Overload;
{ Return a string with given days of the week }
BEGIN
  Result := '';
  IF Monday IN DaysOfTheWeek THEN BEGIN
    IF LongOrShortString = ShortStringType THEN
      Result := Result + DayOfTheWeekToStr(Monday)
    ELSE
      Result := Result + DayOfTheWeekToStr(Monday, LongStringType);
  END;

  IF Tuesday IN DaysOfTheWeek THEN BEGIN
    IF LongOrShortString = ShortStringType THEN
      Result := Result + DayOfTheWeekToStr(Tuesday)
    ELSE
      Result := Result + DayOfTheWeekToStr(Tuesday, LongStringType);
  END;

  IF Wednesday IN DaysOfTheWeek THEN BEGIN
    IF LongOrShortString = ShortStringType THEN
      Result := Result + DayOfTheWeekToStr(Wednesday)
    ELSE
      Result := Result + DayOfTheWeekToStr(Wednesday, LongStringType);
  END;

  IF Thursday IN DaysOfTheWeek THEN BEGIN
    IF LongOrShortString = ShortStringType THEN
      Result := Result + DayOfTheWeekToStr(Thursday)
    ELSE
      Result := Result + DayOfTheWeekToStr(Thursday, LongStringType);
  END;

  IF Friday IN DaysOfTheWeek THEN BEGIN
    IF LongOrShortString = ShortStringType THEN
      Result := Result + DayOfTheWeekToStr(Friday)
    ELSE
      Result := Result + DayOfTheWeekToStr(Friday, LongStringType);
  END;

  IF Saturday IN DaysOfTheWeek THEN BEGIN
    IF LongOrShortString = ShortStringType THEN
      Result := Result + DayOfTheWeekToStr(Saturday)
    ELSE
      Result := Result + DayOfTheWeekToStr(Saturday, LongStringType);
  END;

  IF Sunday IN DaysOfTheWeek THEN BEGIN
    IF LongOrShortString = ShortStringType THEN
      Result := Result + DayOfTheWeekToStr(Sunday)
    ELSE
      Result := Result + DayOfTheWeekToStr(Sunday, LongStringType);
  END;
END; { DaysOfTheWeekSetToStr }

PROCEDURE Debug{1}; Overload;
{ Does nothing except stop locomotives if required by user - otherwise used only as a placeholder to set breakpoints on }
VAR
  OK : Boolean;

BEGIN
  IF StopAllLocomotivesWhenDebugCalled THEN
    StopAllLocomotives(OK);
END; { Debug-1 }

PROCEDURE Debug{2}(Str : String); Overload;
{ Write out debug text to the Debug Window }
VAR
  AlwaysMakeSound : Boolean;
  SaveStyle : TFontStyles;

BEGIN
  AlwaysMakeSound := False;
  IF Pos('!*', Str) > 0 THEN BEGIN
    AlwaysMakeSound := True;
    Str := StringReplace(Str, '*', '', []);
  END;

  { Needs this to avoid filling the window with identical lines of text }
  IF Str =  OldDebugStr THEN BEGIN
    { if there's an asterisk in position two, don't rewrite the message, but do repeat the sound }
    IF AlwaysMakeSound THEN
      IF MakeSoundWhenDebugWindowBoldTextAppears THEN
        MakeSound(1);
  END ELSE
    IF DebugWindow = NIL THEN BEGIN
      { store any messages written before the Debug Window is initialised, to be written out when it is }
      SetLength(DebugWindowLines, Length(DebugWindowLines) + 1);
      DebugWindowLines[High(DebugWindowLines)] := Str;
    END ELSE BEGIN
      { Note: without the following two statements, the RichEdit window doesn't initially scroll }
      IF DebugWindow.Visible AND NOT StationMonitorsWindow.Visible THEN
        DebugWindow.setFocus;
      DebugWindow.DebugRichEdit.Perform(EM_SCROLLCARET, 0, 0);

      SaveStyle := DebugWindow.DebugRichEdit.SelAttributes.Style;

      IF (Copy(Str, 1, 1) = '!') AND NOT (Copy(Str, 1, 2) = '!!') THEN BEGIN
        { urgent error messages }
        AddRichLine(DebugWindow.DebugRichEdit, '{R}<color=clRed><b>' + Copy(Str, 2) + '</b>');
        IF MakeSoundWhenDebugWindowBoldTextAppears THEN
          MakeSound(1);
      END ELSE
        IF (Copy(Str, 1, 1) = '&') AND NOT (Copy(Str, 1, 2) = '&&') THEN BEGIN
          { urgent error messages }
          AddRichLine(DebugWindow.DebugRichEdit, '{R}<color=clGreen><b>' + Copy(Str, 2) + '</b>');
          IF MakeSoundWhenDebugWindowBoldTextAppears THEN
            MakeSound(1);
        END ELSE
          IF (Copy(Str, 1, 1) = '+') AND NOT (Copy(Str, 1, 2) = '++') THEN BEGIN
            { warning messages }
            AddRichLine(DebugWindow.DebugRichEdit, '{R}<i>' + Copy(Str, 2) + '</i>');
            IF MakeSoundWhenDebugWindowBoldTextAppears THEN
              MakeSound(5);
          END ELSE
            IF (Copy(Str, 1, 1) = '=') AND NOT (Copy(Str, 1, 2) = '==') THEN BEGIN
              { instructions to the user }
              AddRichLine(DebugWindow.DebugRichEdit, '{R}<b><i>' + Copy(Str, 2) + '</i></b>');
              IF MakeSoundWhenDebugWindowBoldTextAppears THEN
                MakeSound(3);
            END ELSE
              IF (Pos('<', Str) > 0) AND ((Pos('>', Str) > 0)
              AND ((Pos('>', Str) > Pos('<', Str))))
              THEN
                AddRichLine(DebugWindow.DebugRichEdit, Str)
              ELSE BEGIN
                { no fancy text, though we may have to convert two !!s to one !, as otherwise '!' at the start of a line indicates we wish to highlight the line }
                IF Copy(Str, 1, 2) = '!!' THEN
                  Str := Copy(Str, 2);
                DebugWindow.DebugRichEdit.Lines.Add(Str);
              END;

      OldDebugStr := Str;
      DebugWindow.DebugRichEdit.SelAttributes.Style := SaveStyle;
    END;
END; { Debug-2 }

PROCEDURE Debug{3}(Str : String; WriteToStatusBar : Boolean); Overload;
{ Write out debug text to the Debug Window, and also write it to the Status Panel }
BEGIN
  IF Str <> OldDebugStr THEN BEGIN
    { needs this to avoid filling the window with identical lines of text }
    IF DebugWindow = NIL THEN BEGIN
      { store any messages written before the Debug Window is initialised, to be written out when it is }
      SetLength(DebugWindowLines, Length(DebugWindowLines) + 1);
      DebugWindowLines[High(DebugWindowLines)] := Str;
    END ELSE BEGIN
      DebugWindow.DebugRichEdit.Lines.Add(Str);
      OldDebugStr := Str;
      IF WriteToStatusBar THEN
        WriteToStatusBarPanel(StatusBarPanel2, Str);
    END;
  END;
END; { Debug-3 }

PROCEDURE DeleteElementFromBooleanArray(VAR BooleanArray : BooleanArrayType; Position : Integer);
{ Removes the selected element from a boolean array }
VAR
  I : Integer;

BEGIN
  { Move all existing elements down one }
  FOR I := Position TO (Length(BooleanArray) - 2) DO
    BooleanArray[I] := BooleanArray[I + 1];
  SetLength(BooleanArray, Length(BooleanArray) - 1);
END; { DeleteElementFromBooleanArray }

PROCEDURE DeleteElementFromDateTimeArray(VAR DateTimeArray : DateTimeArrayType; Position : Integer);
{ Removes the selected element from a TDateTime array }
VAR
  I : Integer;

BEGIN
  { Move all existing elements down one }
  FOR I := Position TO (Length(DateTimeArray) - 2) DO
    DateTimeArray[I] := DateTimeArray[I + 1];
  SetLength(DateTimeArray, Length(DateTimeArray) - 1);
END; { DeleteElementFromDateTimeArray }

PROCEDURE DeleteElementFromIntegerArray(VAR IntegerArray : IntegerArrayType; Position : Integer);
{ Removes the selected element from an integer array }
VAR
  I : Integer;

BEGIN
  { Move all existing elements down one }
  FOR I := Position TO (Length(IntegerArray) - 2) DO
    IntegerArray[I] := IntegerArray[I + 1];
  SetLength(IntegerArray, Length(IntegerArray) - 1);
END; { DeleteElementFromIntegerArray }

PROCEDURE DeleteElementFromLightsToBeSwitchedOnArray(Position : Integer);
{ Removes the given element from the lighting array }
VAR
  I : Integer;

BEGIN
  WITH LightsToBeSwitchedOnArray[Position] DO
    Log(Trains[LightsToBeSwitchedOn_Train].Train_LocoChipStr + ' L Lighting record removed from LightsToBeSwitchedOnArray');

  { Move all existing elements down one }
  FOR I := Position TO (Length(LightsToBeSwitchedOnArray) - 2) DO
    LightsToBeSwitchedOnArray[I] := LightsToBeSwitchedOnArray[I + 1];
  SetLength(LightsToBeSwitchedOnArray, Length(LightsToBeSwitchedOnArray) - 1);
END; { DeleteElementFromLightsToBeSwitchedOnArray }

PROCEDURE DeleteElementFromLocationArray(VAR LocationArray : IntegerArrayType; Position : Integer);
{ Removes the selected element from an array }
VAR
  I : Integer;

BEGIN
  { Move all existing elements down one }
  FOR I := Position TO (Length(LocationArray) - 2) DO
    LocationArray[I] := LocationArray[I + 1];
  SetLength(LocationArray, Length(LocationArray) - 1);
END; { DeleteElementFromLocationArray }

PROCEDURE DeleteElementFromStringArray(VAR StringArray : StringArrayType; Position : Integer);
{ Removes the selected element from an array }
VAR
  I : Integer;

BEGIN
  { Move all existing elements down one }
  FOR I := Position TO (Length(StringArray) - 2) DO
    StringArray[I] := StringArray[I + 1];
  SetLength(StringArray, Length(StringArray) - 1);
END; { DeleteElementFromStringArray }

FUNCTION DescribeIntegerArray(IntegerArray : IntegerArrayType) : String;
{ Return the contents of an integer array as a string }
VAR
  I : Integer;

BEGIN
  Result := '';
  FOR I := 0 TO High(IntegerArray) DO
    Result := Result + IntToStr(IntegerArray[I]) + ' ';
END; { DescribeIntegerArray }

FUNCTION DescribeLineNamesForTrackCircuit(TC : Integer) : String;
{ Return the line names for a track circuit }
VAR
  L : Integer;

BEGIN
  Result := '';
  FOR L := 0 TO High(Lines) DO
    IF (TC <> UnknownTrackCircuit) AND (TC = Lines[L].Line_TC) THEN
      Result := Result + LineToStr(L) + ', ';

  IF RightStr(Result, 2) = ', ' THEN
    Result := Copy(Result, 1, Length(Result) - 2);
END; { DescribeLineNamesForTrackCircuit }

FUNCTION DescribeStartAndEndOfRoute(Route : Integer) : String;
{ Return the signal at the start of the route, and the signal or buffer stop that ends it }
BEGIN
  { Ended by a signal }
  IF Routes_EndSignals[Route] <> UnknownSignal THEN
    Result := '(S' + IntToStr(Routes_StartSignals[Route]) + ' to S' + IntToStr(Routes_EndSignals[Route]) + ', '
              + LineToStr(Routes_StartLines[Route]) + ' to ' + LineToStr(Routes_EndLines[Route]) + ', '
              + 'TC' + IntToStr(Lines[Routes_StartLines[Route]].Line_TC) + ' to TC' + IntToStr(Lines[Routes_EndLines[Route]].Line_TC) + ')'

  ELSE
    IF Routes_EndBufferStops[Route] <> UnknownBufferStop THEN
      { or ended by a buffer stop }
     Result := '(S' + IntToStr(Routes_StartSignals[Route]) + ' to BS' + IntToStr(Routes_EndBufferStops[Route]) + ', '
               + LineToStr(Routes_StartLines[Route]) + ' to ' + LineToStr(Routes_EndLines[Route]) + ', '
               + 'TC' + IntToStr(Lines[Routes_StartLines[Route]].Line_TC) + ' to TC' + IntToStr(Lines[Routes_EndLines[Route]].Line_TC) + ')'
    ELSE
      Result := '';
END; { DescribeStartAndEndOfRoute }

FUNCTION DescribeSubRoute(Route, SubRoute : Integer) : String;
{ Return a description of the subroute }
VAR
  EndSignalOrBufferStopStr : String;

BEGIN
  TRY
    IF SubRoute < (Routes_TotalSubRoutes[Route] - 1) THEN
      EndSignalOrBufferStopStr := 'S' + IntToStr(Routes_SubRouteStartSignals[Route, SubRoute + 1])
    ELSE
      IF Routes_EndSignals[Route] <> UnknownSignal THEN
        EndSignalOrBufferStopStr := 'S' + IntToStr(Routes_EndSignals[Route])
      ELSE
        EndSignalOrBufferStopStr := 'BS' + IntToStr(Routes_EndBufferStops[Route]);

    Result :=  '(S' + IntToStr(Routes_SubRouteStartSignals[Route, SubRoute]) + ' to ' + EndSignalOrBufferStopStr + ', '
               + LineToStr(Routes_SubRouteStartLines[Route, SubRoute]) + ' to ' + LineToStr(Routes_SubRouteEndLines[Route, SubRoute]) + ', '
               + 'TC' + IntToStr(Lines[Routes_SubRouteStartLines[Route, SubRoute]].Line_TC)
               + ' to TC' + IntToStr(Lines[Routes_SubRouteEndLines[Route, SubRoute]].Line_TC) + ')';
  EXCEPT
    ON E : Exception DO
      Log('EG DescribeSubRoute: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DescribeSubRoute }

FUNCTION DescribeJourneyAndRoute(Args : ARRAY OF Integer) : String;
{ Return a description of the route (and subroute if requested) }
VAR
  Route, SubRoute : Integer;

BEGIN
  Route := Args[0];
  IF High(Args) = 0 THEN
    { 1 argument - just route }
    Result := IfThen(Routes_Journeys[Route] <> UnknownJourney,
                     'J' + IntToStr(Routes_Journeys[Route]) + ' ')
              + 'R' + IntToStr(Route) + ' ' + DescribeStartAndEndOfRoute(Route)
  ELSE BEGIN
    { 2 arguments - route and subroute }
    SubRoute := Args[1];
    Result := IfThen(Routes_Journeys[Route] <> UnknownJourney,
                     'J' + IntToStr(Routes_Journeys[Route]) + ' ')
              + 'R' + IntToStr(Route) + ' ' + DescribeStartAndEndOfRoute(Route)
              + ', SR' + IntToStr(SubRoute) + ' ' + DescribeSubRoute(Route, SubRoute);
  END;
END; { DescribeJourneyAndRoute }

FUNCTION DirectionWillChangeAfterGivenJourney(T : TrainIndex; CurrentJourney : Integer) : Boolean;
{ Returns true if the direction will change on the journey after the current journey }
BEGIN
  Result := False;

  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('DirectionWillChangeAfterGivenJourney')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF FinalJourney(T, CurrentJourney) THEN
        Result := False
      ELSE
        Result := Train_JourneysArray[CurrentJourney].TrainJourney_Direction <> Train_JourneysArray[CurrentJourney + 1].TrainJourney_Direction;
    END; {WITH}
  END;
END; { DirectionWillChangeAfterGivenJourney }

FUNCTION DirectionArrayToStr(DirectionsArray : DirectionArrayType) : String;
{ List the contents of an array }
VAR
  I : Integer;

BEGIN
  Result := '';

  IF Length(DirectionsArray) = 0 THEN
    Exit
  ELSE BEGIN
    FOR I := 0 TO High(DirectionsArray) - 1 DO
      Result := Result + DirectionToStr(DirectionsArray[I]) + ', ';
    Result := Result + DirectionToStr(DirectionsArray[High(DirectionsArray)]);
  END;
END; { DirectionArrayToStr }

FUNCTION DisplayJourneyNumber(Journey : Integer) : String;
{ Return the supplied journey number with an indent in a form that makes the debug output easier to read }
BEGIN
  IF Journey = UnknownJourney THEN
    Result := 'J-'
  ELSE
    Result := 'J=' + IntToStr(Journey);
  Result := Result + StringOfChar(' ', 8 - Length(Result)) + ': ';
END; { DisplayJourneyNumber }

FUNCTION DisplayJourneyNumbers(T : TrainIndex; FirstJourney, SecondJourney : Integer) : String; Overload;
{ Return the supplied journey numbers with an indent in a form that makes the debug output easier to read }
BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('DisplayJourneyNumbers')
  ELSE BEGIN
    IF FirstJourney = UnknownJourney THEN
      Result := 'J=-'
    ELSE BEGIN
      IF FirstJourney = SecondJourney THEN BEGIN
        IF FirstJourney = UnknownJourney THEN
          Result := 'J=- J=-'
        ELSE
          Result := 'J=' + IntToStr(FirstJourney)
      END ELSE
        IF (SecondJourney > High(Trains[T].Train_JourneysArray)) OR (SecondJourney = UnknownJourney) THEN
          Result := 'J=' + IntToStr(FirstJourney)
        ELSE
          Result := 'J=' + IntToStr(FirstJourney) + '/' + IntToStr(SecondJourney);
      Result := Result + StringOfChar(' ', 8 - Length(Result)) + ': ';
    END;
  END;
END; { DisplayJourneyNumbers }

FUNCTION DisplayTrackCircuitsForLocation(Location : Integer) : String;
{ Write out which track circuits the given location contains }
VAR
  I : Integer;
  TrackCircuitArray : IntegerArrayType;

BEGIN
  Result := '';
  TrackCircuitArray := GetTrackCircuitsForLocation(Location);
  IF Length(TrackCircuitArray) > 0 THEN BEGIN
    FOR I := 0 TO High(TrackCircuitArray) - 1 DO
      Result := Result + IntToStr(TrackCircuitArray[I]) + ', ';

    Result := Result + IntToStr(TrackCircuitArray[High(TrackCircuitArray)]);
  END;
END; { DisplayTrackCircuitsForLocation }

FUNCTION DistanceBetween(X1, Y1, X2, Y2 : Integer) : Extended;
{ Returns the Euclidean distance between (X1, Y1) and (X2, Y2) }
BEGIN
  Result := Sqrt(Power(X1 - X2, 2) + Power(Y1 - Y2, 2));
END; { DistanceBetween }

PROCEDURE DrawLineInLogFile(LocoChipStr : String; LogFileCh : Char; LineStr : String; OriginatingUnitRef : String);
{ Draw a line of a given character in the log file }
CONST
  UnknownAsZeroes = True;

BEGIN
  IF LineStr = '-' THEN
    Log(LocoChipStr + ' ' + LogFileCh + ' ' + ' {LINE} {UNITSUBSTITUTE=' + OriginatingUnitRef + '}')
  ELSE
    Log(LocoChipStr + ' ' + LogFileCh + ' ' + ' {LINE=' + LineStr + '} {UNITSUBSTITUTE=' + OriginatingUnitRef + '}');
END; { DrawLineInLogFile }

FUNCTION ExtractBufferStopFromString(Str : String): Integer;
{ Returns a buffer stop number from a given string }
BEGIN
  IF Pos('BS=', Str) = 0 THEN
    Result := UnknownBufferStop
  ELSE
    Result := StrToInt(Copy(Str, 4, 255));
END; { ExtractBufferStopFromString }

FUNCTION ExtractIntegerFromString(L : String) : Integer;
{ Returns a string with just the numbers, or -1 if there is no number }
VAR
  I : Integer;
  IntegerFound : Boolean;
  TempStr : String;

BEGIN
  Result := -1;
  TempStr := '';
  IntegerFound := False;
  I := 1;

  WHILE (I <= Length(L)) DO BEGIN
    IF IsNumeric(L[I]) THEN BEGIN
      TempStr := TempStr + L[I];
      IntegerFound := True;
    END ELSE
      IF IntegerFound THEN
        { we've now found a non-integer}
        Break;
    Inc(I);
  END;

  IF TempStr <> '' THEN
    Result := StrToInt(TempStr);
END; { ExtractIntegerFromString }

FUNCTION ExtractJourneyFromString(Str : String): Integer;
{ Returns a journey number from a given string }
BEGIN
  IF Pos('J=', Str) > 0 THEN
    Result := StrToInt(Copy(Str, 3, 255))
  ELSE
    Result := UnknownJourney;
END; { ExtractJourneyFromString }

FUNCTION ExtractLineFromString(Str : String): Integer;
{ Returns a linename from a given string }
BEGIN
  IF Pos('L=', Str) > 0 THEN BEGIN
    IF Pos('+', Str) > 0 THEN
      { remove any trailing "+" signs }
      Result := StrToLine(Copy(Str, 3, Length(Str) - 3))
    ELSE
      Result := StrToLine(Copy(Str, 3, 255))
  END ELSE
    Result := UnknownLine;
END; { ExtractLineFromString }

FUNCTION ExtractPointFromString(Str : String): Integer;
{ Returns a point number from a given array element }
VAR
  Str2 : String;

BEGIN
  IF (Pos('FP=', Str) > 0)
  OR (Pos('TP=', Str) > 0)
  OR (Pos('XP=', Str) > 0)
 THEN BEGIN
    Str2 := Copy(Str, 4, 255);
    { remove the trailing point direction if there }
    IF (Copy(Str2, Length(Str2), 1) = '/') OR (Copy(Str2, Length(Str2), 1) = '-') THEN
      Str2 := Copy(Str2, 1, Length(Str2) - 1);
    Result := StrToInt(Str2);
  END ELSE
    Result := UnknownPoint;
END; { ExtractPointFromString }

FUNCTION ExtractPointStateFromString(Str : String) : PointStateType;
{ Returns a point state from a given array element }
BEGIN
  IF Pos('-', Str) = Length(Str) THEN
    Result := Straight
  ELSE
    Result := Diverging;
END; { ExtractPointStateFromString }

FUNCTION ExtractRouteFromString(Str : String): Integer;
{ Returns a route number from a given string }
BEGIN
  IF Pos('R=', Str) > 0 THEN
    Result := StrToInt(Copy(Str, 3, 255))
  ELSE
    Result := UnknownRoute;
END; { ExtractRouteFromString }

FUNCTION ExtractSignalFromString(Str : String): Integer;
{ Extract a signal number from a string }
VAR
  JunctionIndicatorStrPos : Integer;

BEGIN
  Result := UnknownSignal;
  TRY
    { First remove any junction indicator abbreviations }
    JunctionIndicatorStrPos := Pos('UL', Str);
    IF JunctionIndicatorStrPos > 0 THEN
      Delete(Str, JunctionIndicatorStrPos, 2);
    JunctionIndicatorStrPos := Pos('ML', Str);
    IF JunctionIndicatorStrPos > 0 THEN
      Delete(Str, JunctionIndicatorStrPos, 2);
    JunctionIndicatorStrPos := Pos('LL', Str);
    IF JunctionIndicatorStrPos > 0 THEN
      Delete(Str, JunctionIndicatorStrPos, 2);
    JunctionIndicatorStrPos := Pos('UR', Str);
    IF JunctionIndicatorStrPos > 0 THEN
      Delete(Str, JunctionIndicatorStrPos, 2);
    JunctionIndicatorStrPos := Pos('MR', Str);
    IF JunctionIndicatorStrPos > 0 THEN
      Delete(Str, JunctionIndicatorStrPos, 2);
    JunctionIndicatorStrPos := Pos('LR', Str);
    IF JunctionIndicatorStrPos > 0 THEN
      Delete(Str, JunctionIndicatorStrPos, 2);

    IF (Pos('FS=', Str) = 0) AND (Pos('FR=', Str) = 0) AND (Pos('TS=', Str) = 0) AND (Pos('SS=', Str) = 0) THEN BEGIN
      IF Copy(Str, 1, 2) = 'S=' THEN
        Result := StrToInt(Copy(Str, 3, 255));
    END ELSE
      IF (Pos('\', Str) > 0)
      OR (Pos('|', Str) > 0)
      OR (Pos('.', Str) > 0)
      OR (Copy(Str, Length(Str), 1) = '=')
      THEN
        Result := StrToInt(Copy(Str, 4, Length(Str) - 4))
      ELSE
        IF (Pos('>', Str) > 0) THEN
          Result := StrToInt(Copy(Str, 4, Pos('>', Str) - 4))
        ELSE
          Result := StrToInt(Copy(Str, 4, 255));
  EXCEPT
    ON E : Exception DO
      Log('EG ExtractSignalFromString: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ExtractSignalFromString }

FUNCTION ExtractSignalStateFromString(Str : String) : SignalStateType;
{ Returns a signal state from a given array element }
BEGIN
  IF Pos('\', Str) > 0 THEN
    Result := SignalOff
  ELSE
    IF Pos('=', Str) > 0 THEN
      Result := SignalOn
    ELSE
      Result := UnknownSignalState;
END; { ExtractSignalStateFromString }

FUNCTION ExtractSubRouteFromString(Str : String): Integer;
{ Returns a subroute number from a given array element }
BEGIN
  IF Pos('SR=', Str) > 0 THEN
    Result := StrToInt(Copy(Str, 4, 255))
  ELSE
    Result := UnknownSubRoute;
END; { ExtractSubRouteFromString }

PROCEDURE ExtractSubStringsFromString(Str : String; DelimiterCh : Char; OUT StrArray : StringArrayType);
{ Goes through a given string, extracting substrings delimited by the given delimiter character }
VAR
  I : Integer;

BEGIN
  SetLength(StrArray, 0);

  I := 1;
  WHILE I <= Length(Str) DO BEGIN
    IF (Length(Str) > 0) AND (Str[I] = DelimiterCh) THEN BEGIN
      AppendToStringArray(StrArray, Copy(Str, 1, I - 1));
      StrArray[High(StrArray)] := Trim(StrArray[High(StrArray)]);
      Delete(Str, 1, I);
      I := 0;
    END ELSE
      IF I = Length(Str) THEN BEGIN
        AppendToStringArray(StrArray, Copy(Str, 1, I));
        StrArray[High(StrArray)] := Trim(StrArray[High(StrArray)]);
      END;
    Inc(I);
  END; {WHILE}
END; { ExtractSubStringsFromString }

FUNCTION ExtractTrackCircuitFromString(Str : String): Integer;
{ Returns a track-circuit number from a given string }
VAR
  Str2 : String;

BEGIN
  IF Pos('TC=', Str) > 0 THEN BEGIN
    Str2 := Copy(Str, 4, 255);
    { remove the trailing '*' if there }
    IF (Copy(Str2, Length(Str2), 1) = '*') THEN
      Str2 := Copy(Str2, 1, Length(Str2) - 1);
    Result := StrToInt(Str2);
  END ELSE
    Result := UnknownTrackCircuit;
END; { ExtractTrackCircuitFromString }

FUNCTION ExtractSubRouteFromStringArray(StringArray : StringArrayType; SubRoute : Integer): StringArrayType;
{ Returns a given subroute }
VAR
  NewStringArray : StringArrayType;
  StringArrayPos : Integer;
  SubRouteEndFound : Boolean;
  SubRouteStartFound : Boolean;

BEGIN
  SetLength(NewStringArray, 0);
  StringArrayPos := 0;
  SubRouteStartFound := False;
  SubRouteEndFound := False;
  WHILE (StringArrayPos <= High(StringArray)) AND NOT SubRouteEndFound DO BEGIN
    IF IsElementSubRouteMarker(StringArray, StringArrayPos) THEN BEGIN
      IF ExtractSubRouteFromString(StringArray[StringArrayPos]) = (SubRoute - 1) THEN
        { we've found the one we want }
        SubRouteStartFound := True;
    END;
    IF SubRouteStartFound THEN BEGIN
      { but work out when we reach the end of it }
      IF StringArrayPos = High(StringArray) THEN
        SubRouteEndFound := True
      ELSE
        { is the next element a marker? }
        IF IsElementSubRouteMarker(StringArray, StringArrayPos + 1) THEN
          SubRouteEndFound := True;
      AppendToStringArray(NewStringArray, StringArray[StringArrayPos]);
    END;

    Inc(StringArrayPos);
  END; {WHILE}
  Result := NewStringArray;
END; { ExtractSubRouteFromStringArray }

FUNCTION EndOfLineToStr(E : EndOfLineType) : String;
{ Return the end of line type as a string }

BEGIN
  CASE E OF
    BufferStopAtUp:
      Result := BufferStopAtUpStr;
    BufferStopAtDown:
      Result := BufferStopAtDownStr;
    ProjectedLineAtUp:
      Result := ProjectedLineAtUpStr;
    ProjectedLineAtDown:
      Result := ProjectedLineAtDownStr;
    NotEndOfLine:
      Result := NotEndOfLineStr;
  END; {CASE}
END; { EndOfLineToStr }

FUNCTION FinalJourney(T : TrainIndex; CurrentJourney : Integer) : Boolean;
{ Returns true if the current journey is the final one }
BEGIN
  Result := False;

  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('FinalJourney')
  ELSE
    IF CurrentJourney = High(Trains[T].Train_JourneysArray) THEN
      Result := True;
END; { FinalJourney }

PROCEDURE GetProjectVersionInfo(AVersionList: TStrings; AFileName: String = '');
{ This procedure returns ALL of the version information as separate string entries of a TString list. Each element can then be accessed by indexing the TString list thus:
  AVersionList[0], AVersionList[1] etc..
}
VAR
  I: Integer;
  InfoSize: DWORD;
  LastError: Integer;
  pTrans: PTransBuffer;
  TransStr: String;
  TypeStr: String;
  Value: PChar;
  VerBuf: pointer;
  VerSize: DWORD;
  Wnd: DWORD;

BEGIN
  InfoSize := 0;
  AVersionList.Clear;
  AFileName := ParamStr(0);
  IF AFileName = '' THEN
    InfoSize := GetFileVersionInfoSize(PChar(AFileName), Wnd);

  IF (InfoSize = 0) THEN BEGIN
    LastError := GetLastError;
    Log('A ' + Format('GetFileVersionInfo failed: (%d) %s', [LastError, SysErrorMessage(LastError)]));
    Exit;
  END ELSE BEGIN
    GetMem(VerBuf, InfoSize);
    TRY
      IF GetFileVersionInfo(PChar(AFileName), Wnd, InfoSize, VerBuf) THEN BEGIN
        VerQueryValue(VerBuf, PChar('\VarFileInfo\Translation'), Pointer(pTrans), VerSize);

        TransStr := IntToHex(pTrans^[1], 4) + IntToHex(pTrans^[2], 4);

        FOR I := Low(CInfoStr) to High(CInfoStr) DO BEGIN
          TypeStr := 'StringFileInfo\' + TransStr + '\' + CInfoStr[I];

          IF VerQueryvalue(VerBuf, PChar(TypeStr), Pointer(Value), VerSize) THEN
            AVersionList.Add(CInfoStr[I] + '=' + Value);
        END; {FOR}
      END;
    FINALLY
      FreeMem(VerBuf);
    END; {TRY}
  END;
END; { GetProjectVersionInfo }

FUNCTION GetBuildInfo(VAR V1, V2, V3, V4: Word; AFileName: String = ''): Boolean;
{ This procedure returns the individual Major/Minor/Release/Build values of the version information }
VAR
  Dummy: DWORD;
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  LastError: Integer;

BEGIN
  Result := True;
  IF AFileName = '' THEN
    AFileName := ParamStr(0);

  VerInfoSize := GetFileVersionInfoSize(PChar(AFileName), Dummy);
  IF VerInfoSize = 0 THEN BEGIN
    LastError := GetLastError;
    Log('A ' + Format('GetFileVersionInfo failed: (%d) %s', [LastError, SysErrorMessage(LastError)]));
    Exit;
  END;

  GetMem(VerInfo, VerInfoSize);
  TRY
    GetFileVersionInfo(PChar(AFileName), 0, VerInfoSize, VerInfo);
    VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);

    WITH VerValue^ DO BEGIN
      V1 := dwFileVersionMS SHR 16;
      V2 := dwFileVersionMS AND $FFFF;
      V3 := dwFileVersionLS SHR 16;
      V4 := dwFileVersionLS AND $FFFF;
    END; {WITH}
  FINALLY
    FreeMem(VerInfo, VerInfoSize);
  END; {TRY}
end; { GetBuildInfo }

FUNCTION GetVersionInfoAsString : String;
{ Return the program's version number }
VAR
  V1: Word;
  V2: Word;
  V3: Word;
  V4: Word;

BEGIN
  IF GetBuildInfo(V1, V2, V3, V4) THEN
    Result := Format('%d.%d.%d', [V1, V2, V3])
  ELSE
    Result := '';
END; { GetVersionInfoAsString }

FUNCTION GetBuildInfoAsString : String;
{ Return the program's build number - this is auto incremented on each build }
VAR
  V1: Word;
  V2: Word;
  V3: Word;
  V4: Word;

BEGIN
  IF GetBuildInfo(V1, V2, V3, V4) THEN
    Result := Format('%d', [V4])
  ELSE
    Result := '';
END; { GetBuildInfoAsString }

FUNCTION GetComputerNetName : String;
{ Return the local computer name (by Zarko Gajic, About.com) }
VAR
  Buffer: ARRAY[0..255] OF Char;
  Size: DWord;

BEGIN
  Size := 256;
  IF GetComputerName(Buffer, Size) THEN
    Result := Buffer
  ELSE
    Result := '';
END; { GetComputerNetName }

FUNCTION GetExtendedDataFromUser(Prompt, DefaultStr : String; LowerBound, UpperBound : Extended; BoundsErrorMsg : String; OUT NewValue : Extended) : Boolean;
{ Makes sure an extended valuer is returned by the user, and optionally that it is within the bounds set }
CONST
  Caption = '';

VAR
  OK : Boolean;
  ValueStr : String;

BEGIN
  REPEAT
    IF NOT InputQuery(Caption, Prompt, ValueStr) THEN BEGIN
      OK := True;
      Result := False;
    END ELSE BEGIN
      Result := True;
      OK := False;

      IF NOT TryStrToFloat(ValueStr, NewValue) THEN
        ShowMessage('Invalid number: "' + ValueStr + '" - please try again')
      ELSE
        IF (LowerBound = 0) AND (UpperBound = 0) THEN
          OK := True
        ELSE
          IF (NewValue >= LowerBound) AND (NewValue <= UpperBound) THEN
            OK := True
          ELSE
            ShowMessage(ValueStr + ' ' + BoundsErrorMsg);
    END;
  UNTIL OK;
END; { GetExtendedDataFromUser }

FUNCTION GetIntegerDataFromUser(Prompt, DefaultStr : String; LowerBound, UpperBound : Integer; BoundsErrorMsg : String; OUT NewValue : Integer) : Boolean;
{ Makes sure an integer is returned by the user, and optionally that it is within the bounds set }
CONST
  Caption = '';

VAR
  OK : Boolean;
  ValueStr : String;

BEGIN
  REPEAT
    IF NOT InputQuery(Caption, Prompt, ValueStr) THEN BEGIN
      OK := True;
      Result := False;
    END ELSE BEGIN
      Result := True;
      OK := False;

      IF NOT TryStrToInt(ValueStr, NewValue) THEN
        ShowMessage('Invalid number: "' + ValueStr + '" - please try again')
      ELSE
        IF (LowerBound = 0) AND (UpperBound = 0) THEN
          OK := True
        ELSE
          IF (NewValue >= LowerBound) AND (NewValue <= UpperBound) THEN
            OK := True
          ELSE
            ShowMessage(ValueStr + ' ' + BoundsErrorMsg);
    END;
  UNTIL OK;
END; { GetIntegerDataFromUser }

FUNCTION GetStringDataFromUser(Prompt, DefaultStr : String; PermissibleStrings : StringArrayType; OUT NewString : String) : Boolean;
{ Makes sure a non-empty string is returned by the user, and optionally that it is matches one of the strings supplied }
CONST
  Caption = '';

VAR
  I : Integer;
  OK : Boolean;
  PermissibleStringFound : Boolean;

BEGIN
  REPEAT
    IF NOT InputQuery(Caption, Prompt, NewString) THEN BEGIN
      OK := True;
      Result := False;
    END ELSE BEGIN
      Result := True;
      OK := False;

      IF NewString = '' THEN
        ShowMessage('Empty string - please try again')
      ELSE
        IF Length(PermissibleStrings) = 0 THEN
          OK := True
        ELSE BEGIN
          I := 0;
          PermissibleStringFound := False;

          WHILE (I <= High(PermissibleStrings)) AND NOT PermissibleStringFound DO BEGIN
            IF PermissibleStrings[I] = NewString THEN
              PermissibleStringFound := True
            ELSE
              Inc(I);
          END;

          IF NOT PermissibleStringFound THEN
            ShowMessage('"' + NewString + '" does not match the list of permissible strings')
          ELSE
            OK := True;
        END;
    END;
  UNTIL OK;
END; { GetStringDataFromUser }

FUNCTION GetUserFromWindows : String;
{ Return the local user name (by Zarko Gajic, About.com) }
VAR
  UserName : String;
  UserNameLen : DWord;

BEGIN
  UserNameLen := 255;
  SetLength(UserName, UserNameLen);
  IF GetUserName(PChar(UserName), UserNameLen) THEN
    Result := Copy(UserName, 1, UserNameLen - 1)
  ELSE
    Result := 'Unknown';
END;

FUNCTION GetLineTypeColour(T : TypeOfLine) : TColour;
{ Return the colour for a specific line type, unless the screen is set to be printed from, in whiuch case return black }
BEGIN
  IF ScreenColoursSetForPrinting THEN
    Result := clBlack
  ELSE BEGIN
    CASE T OF
      MainLine:
        Result := clRed;
      MainOrGoods:
        Result := clFWPOrange;
      GoodsLine:
        Result := clLime;
      SidingLine:
        Result := clAqua;
      SidingsApproach:
        Result := clFuchsia;
      BranchLineSingle, BranchLineDouble:
        Result := clCream;
      IslandStationLine:
        Result := clSkyBlue;
      MainStationLine:
        Result := clTeal;
      BranchStationLine:
        Result := clFWPPink;
      WindowStationLine:
        Result := clFWPOrange;
      FiddleyardLine:
        Result := clYellow;
      StationAvoiding:
        Result := clGreen;
      ProjectedLine:
        Result := clLtGray;
    ELSE
      Result := clDkGray;
    END; {CASE}
  END;
END; { GetLineTypeColour }

FUNCTION GetLinesForTrackCircuit(TC : Integer) : IntegerArrayType;
{ Return all the lines on a given track circuit }
VAR
  L : Integer;

BEGIN
  SetLength(Result, 0);

  FOR L := 0 TO High(Lines) DO
    IF (TC <> UnknownTrackCircuit) AND (TC = Lines[L].Line_TC) THEN
      AppendToLineArray(Result, L);
END; { GetLinesForTrackCircuit }

FUNCTION GetFollowingChars(LineStr : String; Str : String; EndStr : String) : String;
{ Return the characters after the given characters up to the delimiter supplied (or "CRLF" if it's at a line end - to force a search for a line end use '' as the second
  parameter). Do the test in upper case to avoid case errors. }
VAR
  EndStrPos : Integer;
  TestPos : Integer;

BEGIN
  TestPos := Pos(UpperCase(Str), UpperCase(LineStr));
  IF TestPos = 0 THEN
    Result := ''
  ELSE BEGIN
    EndStrPos := Pos(EndStr, Copy(LineStr, TestPos + Length(Str)));
    IF EndStrPos = 0 THEN
      EndStrPos := Length(LineStr) - 1;

    Result := Copy(LineStr, TestPos + Length(Str), EndStrPos - 1);
  END;
END; { GetFollowingChars }

FUNCTION GetLastLogLineStr : String;
{ Returns the last line written to the log }
BEGIN
  Result := LastLogLineStr;
END; { GetLastLogLine }

FUNCTION GetLocationFromTrackCircuit(TC : Integer) : Integer;
{ Return a location given a track-circuit number }
VAR
  Line : Integer;
  LocationFound : Boolean;

BEGIN
  Line := 0;
  Result := UnknownLocation;

  LocationFound := False;
  WHILE (Line <= High(Lines)) AND NOT LocationFound DO BEGIN
    IF Lines[Line].Line_TC = TC THEN BEGIN
      IF Lines[Line].Line_Location <> UnknownLocation THEN BEGIN
        LocationFound := True;
        Result := Lines[Line].Line_Location;
      END;
    END;
    Inc(Line);
  END; {WHILE}
END; { GetLocationFromTrackCircuit }

FUNCTION GetNextDayOfTheWeek(DayOfTheWeek : DayOfTheWeekType) : DayOfTheWeekType;
{ Return the next day of the week to the one given }
BEGIN
  Result := UnknownDayOfTheWeek;

  CASE CurrentRailwayDayOfTheWeek OF
    Monday:
      Result := Tuesday;
    Tuesday:
      Result := Wednesday;
    Wednesday:
      Result := Thursday;
    Thursday:
      Result := Friday;
    Friday:
      Result := Saturday;
    Saturday:
      Result := Sunday;
    Sunday:
      Result := Monday;
  END; {CASE}
END; { GetNextDayOfTheWeek }

FUNCTION GetProgramTitle : String;
{ Returns the program title }
BEGIN
  Result := ProgramTitle + ' - ' + CopyrightStatementForWritingToFile;
END; { GetProgramTitle }

FUNCTION GetProgramVersion(Str : String) : String;
{ Returns the program version preced by any additional text supplied }
BEGIN
  IF Str = '' THEN
    Result := 'Version ' + GetVersionInfoAsString + ' build ' + GetBuildInfoAsString + ' at ' + DescribeActualDateAndTime
  ELSE
    Result := Str + ' : version ' + GetVersionInfoAsString + ' build ' + GetBuildInfoAsString + ' at ' + DescribeActualDateAndTime;
END; { GetProgramVersion }

FUNCTION GetSignalAdjacentLine(S : Integer) : Integer;
{ Return the line adjacent to the given signal }
BEGIN
  IF S <> UnknownSignal THEN
    Result := Signals[S].Signal_AdjacentLine
  ELSE
    Result := UnknownLine;
END; { GetSignalAdjacentLine }

FUNCTION GetSignalAspect(S : Integer) : AspectType;
{ Return the state of a signal }
BEGIN
  IF S <> UnknownSignal THEN
    Result := Signals[S].Signal_Aspect
  ELSE BEGIN
    ShowMessage('Signal is zero');
    Result := NoAspect;
  END;
END; { GetSignalAspect }

FUNCTION GetTrackCircuitsForLocation(Location : Integer) : IntegerArrayType;
{ Return all the track circuits for a given location }
VAR
  L : Integer;

BEGIN
  SetLength(Result, 0);
  IF Location <> UnknownLocation THEN BEGIN
    FOR L := 0 TO High(Lines) DO BEGIN
      { only append a new TC if it's not there already }
      IF Lines[L].Line_Location = Location THEN BEGIN
        IF (Length(Result) = 0) OR (Result[High(Result)] <> Lines[L].Line_TC) THEN
          IF Lines[L].Line_TC <> UnknownTrackCircuit THEN
            AppendToIntegerArray(Result, Lines[L].Line_TC)
      END;
    END;
  END;
END; { GetTrackCircuitsForLocation }

FUNCTION GetTrackCircuitStateColour(TC : Integer) : TColour;
{ Return whether and how the track circuit is occupied }
BEGIN
  IF TC = UnknownTrackCircuit THEN
    GetTrackCircuitStateColour := TCUnoccupiedColour
  ELSE
    CASE GetTrackCircuitState(TC) OF
      TCFeedbackOccupation:
        Result := TCFeedbackOccupationColour;
      TCFeedbackOccupationButOutOfUse:
        Result := TCFeedbackOccupationButOutOfUseColour;
      TCLocoOutOfPlaceOccupation:
        Result := TCLocoOutOfPlaceOccupationColour;
      TCMissingOccupation:
        Result := TCMissingOccupationColour;
      TCOutOfUseSetByUser:
        Result := TCOutOfUseSetByUserColour;
      TCOutOfUseAsNoFeedbackReceived:
        Result := TCOutOfUseAsNoFeedbackReceivedColour;
      TCPermanentFeedbackOccupation:
        Result := TCPermanentFeedbackOccupationColour;
      TCPermanentOccupationSetByUser:
        Result := TCPermanentOccupationSetByUserColour;
      TCPermanentSystemOccupation:
        Result := TCPermanentSystemOccupationColour;
      TCSystemOccupation:
        Result := TCSystemOccupationColour;
      TCUnoccupied:
        Result := TCUnoccupiedColour;
    ELSE
      Result := TCUnoccupiedColour;
    END; {CASE}
END; { GetTrackCircuitStateColour }

FUNCTION GetTrackCircuitState(TC : Integer) : TrackCircuitStateType;
{ Return whether and how the track circuit is occupied }
BEGIN
  Result := TCUnoccupied;
  TRY
    IF TC <> UnknownTrackCircuit THEN BEGIN
      Result := TrackCircuits[TC].TC_OccupationState;
      IF DisplayFlashingTrackCircuits AND (TrackCircuits[TC].TC_Headcode = '?') THEN
        { only mystery occupation flashes }
        TrackCircuits[TC].TC_Flashing := True
      ELSE BEGIN
        TrackCircuits[TC].TC_Flashing := False;
        { and, in case it had been flashing, mark it as being in the lit-up state up so it will continue to be drawn }
        TrackCircuits[TC].TC_LitUp := True;
      END;
    END;
  EXCEPT
    ON E : Exception DO
      ShowMessage('GetTrackCircuitState: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { GetTrackCircuitState }

PROCEDURE HideMenus;
{ Make all the menus invisible }
BEGIN
  WITH FWPRailWindow DO BEGIN
    MenusVisible := False;

    MainClockMenu.Visible := False;
    MainDisplayMenu.Visible := False;
    MainFileMenu.Visible := False;
    MainHelpMenu.Visible := False;
    MainOperationsMenu.Visible := False;
    MainRunMenu.Visible := False;

    { this is a bit daft, as if the menus aren't visible, one is never going to see the tick, but I've no doubt that it is part of the AMS standard! }
    IF MainDisplayMenu.Visible THEN
      MainDisplayMenuShow.Checked := False;
  END; {WITH}
END; { HideMenus }

FUNCTION GetOrdinalFromCardinal(Ordinal : Integer) : String;
{ Return the ordinal version of a cardinal number }
BEGIN
  CASE Ordinal OF
    1:
      Result := IntToStr(Ordinal) + 'st';
    2:
      Result := IntToStr(Ordinal) + 'nd';
    3:
      Result := IntToStr(Ordinal) + 'rd';
    4..9:
      Result := IntToStr(Ordinal) + 'th';
  END; {CASE}
END; { GetOrdinalFromCardinal }

FUNCTION IndicatorToStr(I : IndicatorType; LongOrShortString : StringType) : String;
{ Return the type a route indicator is }
BEGIN
  IF LongOrShortString = LongStringType THEN BEGIN
    CASE I OF
      NoIndicator:
        Result := NoIndicatorStr;
      JunctionIndicator:
        Result := JunctionIndicatorStr;
      TheatreIndicator:
        Result := TheatreIndicatorStr;
      QueryIndicator:
        Result := QueryIndicatorStr;
    END; {CASE}
  END ELSE BEGIN
    CASE I OF
      NoIndicator:
        Result := '';
      JunctionIndicator:
        Result := 'Junction';
      TheatreIndicator:
        Result := 'Theatre';
      QueryIndicator:
        Result := 'Query';
    END; {CASE}
  END;
END; { IndicatorToStr }

FUNCTION IndicatorStateToStr(I : IndicatorStateType) : String;
{ Return the state of a route indicator }
BEGIN
  CASE I OF
    NoIndicatorLit:
      Result := 'N';
    LeftIndicatorLit:
      Result := 'L';
    UpperLeftIndicatorLit:
      Result := 'UL';
    MiddleLeftIndicatorLit:
      Result := 'ML';
    LowerLeftIndicatorLit:
      Result := 'LL';
    RightIndicatorLit:
      Result := 'R';
    UpperRightIndicatorLit:
      Result := 'UL';
    MiddleRightIndicatorLit:
      Result := 'ML';
    LowerRightIndicatorLit:
      Result := 'LL';
    TheatreIndicatorLit:
      Result := 'Theatre';
    QueryIndicatorLit:
      Result := '?';
  END; { CASE }
END; { IndicatorStateToStr }

FUNCTION IsProgramRunning(ProgramName : String) : Boolean;
{ Checks to see if a given program is running }
VAR
  ProcHandle : THandle;
  AProcEntry : TProcessEntry32;
  TempStr : String;

BEGIN
  Result := False;

  TRY
    TempStr := '';
    ProcHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS,0);
    IF ProcHandle = INVALID_HANDLE_VALUE THEN
      Exit;

    AprocEntry.dwSize :=SizeOf(TProcessEntry32);
    IF Process32First(ProcHandle, AProcEntry) THEN BEGIN
      TempStr := TempStr + (AProcEntry.szExeFile);

      WHILE Process32Next(ProcHandle,AProcEntry) DO
        TempStr := TempStr + (AProcEntry.szExeFile);
    END;

    IF Pos(UpperCase(ProgramName), UpperCase(TempStr)) > 0 THEN
      Result := True;

    CloseHandle(ProcHandle);
  EXCEPT
    ON E : Exception DO
      ShowMessage('IsProgramRunning: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { IsProgramRunning }

FUNCTION JunctionIndicatorLit(S : Integer) : Boolean;
{ Return true if a junction indicator is lit }
BEGIN
  Result := False;

  WITH Signals[S] DO BEGIN
    CASE Signals[S].Signal_IndicatorState OF
      UpperLeftIndicatorLit:
        Result := True;
      MiddleLeftIndicatorLit:
        Result := True;
      LowerLeftIndicatorLit:
        Result := True;
      UpperRightIndicatorLit:
        Result := True;
      MiddleRightIndicatorLit:
        Result := True;
      LowerRightIndicatorLit:
        Result := True;
    END; { CASE }
  END; {WITH}
END; { JunctionIndicatorLit }

FUNCTION JunctionIndicatorTypeToStr(J : JunctionIndicatorType) : String;
{ Returns a junction indicator type as a string }
BEGIN
  CASE J OF
    UpperLeftIndicator:
      Result := 'upper left indicator';
    MiddleLeftIndicator:
      Result := 'middle left indicator';
    LowerLeftIndicator:
      Result := 'lower left indicator';
    UpperRightIndicator:
      Result := 'upper right indicator';
    MiddleRightIndicator:
      Result := 'middle right indicator';
    LowerRightIndicator:
      Result := 'lower right indicator';
  END; {CASE}
END; { JunctionIndicatorTypeToStr }

FUNCTION IfThenTime{1}(Test : Boolean; Time1, Time2 : TDateTime) : TDateTime; Overload;
{ Returns the first supplied time if the test is true, the second time if not. (The system IfThen routine doesn't work with TDateTime values) }
BEGIN
  IF Test THEN
    Result := Time1
  ELSE
    Result := Time2;
END; { IfThenTime-1 }

FUNCTION IfThenTime{2}(Test : Boolean; Time : TDateTime) : TDateTime; Overload;
{ Returns the supplied time if the test is true. (The system IfThen routine doesn't work with TDateTime values) }
BEGIN
  IF Test THEN
    Result := Time
  ELSE
    Result := 0;
END; { IfThenTime-2 }

FUNCTION IntegerArrayToStr(IntegerArray : IntegerArrayType) : String;
{ Return the contents of an integer array }
VAR
  I : Integer;

BEGIN
  Result := '';

  FOR I := 0 TO High(IntegerArray) DO
    Result := Result + IntToStr(IntegerArray[I]) + ' ';
  Result := Trim(Result);
END; { IntegerArrayToStr }

FUNCTION GetElementPosInIntegerArray(IntegerArray : IntegerArrayType; Element : Integer) : Integer;
{ Returns where, if at all, the given array element is found in an integer array }
VAR
  I : Integer;

BEGIN
  Result := -1;

  I := 0;
  WHILE (I <= High(IntegerArray)) AND (Result = -1) DO BEGIN
    IF Element <> IntegerArray[I] THEN
      Inc(I)
    ELSE
      Result := I;
  END; {WHILE}
END; { GetElementPosInIntegerArray }

FUNCTION IsElementInIntegerArray{1}(IntegerArray : IntegerArrayType; Element : Integer) : Boolean; Overload;
{ Returns whether the given array element is found in an integer array }
VAR
  I : Integer;

BEGIN
  I := 0;
  Result := False;
  WHILE (I <= High(IntegerArray)) AND (Result = False) DO BEGIN
    IF Element <> IntegerArray[I] THEN
      Inc(I)
    ELSE
      Result := True;
  END; {WHILE}
END; { IsElementInIntegerArray-1 }

FUNCTION IsElementInIntegerArray{2}(IntegerArray : IntegerArrayType; Element : Integer; OUT ElementPos : Integer) : Boolean; Overload;
{ Returns whether the given array element is found in an integer array and where it is in the array }
VAR
  I : Integer;

BEGIN
  I := 0;
  Result := False;
  WHILE (I <= High(IntegerArray)) AND (Result = False) DO BEGIN
    IF Element <> IntegerArray[I] THEN
      Inc(I)
    ELSE BEGIN
      Result := True;
      ElementPos := I;
    END;
  END; {WHILE}
END; { IsElementInIntegerArray-2 }

FUNCTION IsElementInLineArray(LineArray : IntegerArrayType; Element : Integer) : Boolean;
{ Returns whether the given array element is in a line array }
VAR
  I : Integer;

BEGIN
  I := 0;
  Result := False;
  WHILE (I <= High(LineArray)) AND (Result = False) DO BEGIN
    IF Element = LineArray[I] THEN
      Result := True
    ELSE
      Inc(I);
  END; {WHILE}
END; { IsElementInLineArray }

FUNCTION IsElementInLocationArray(LocationArray : IntegerArrayType; Element : Integer; OUT ElementPos : Integer) : Boolean;
{ Returns whether the given array element is in an location array }
VAR
  I : Integer;

BEGIN
  I := 0;
  Result := False;
  WHILE (I <= High(LocationArray)) AND (Result = False) DO BEGIN
    IF Element <> LocationArray[I] THEN
      Inc(I)
    ELSE BEGIN
      Result := True;
      ElementPos := I;
    END;
  END; {WHILE}
END; { IsElementInLocationArray }

FUNCTION IsElementInStringArray{1}(StringArray : StringArrayType; Element : String) : Boolean; Overload;
{ Returns whether the given array element is in a string array }
VAR
  I : Integer;

BEGIN
  Result := False;

  I := 0;
  WHILE (I <= High(StringArray)) AND (Result = False) DO BEGIN
    IF Element = StringArray[I] THEN
      Result := True
    ELSE
      Inc(I);
  END; {WHILE}
END; { IsElementInStringArray-1 }

FUNCTION IsElementInStringArray{2}(StringArray : StringArrayType; Element : String; OUT ElementPos : Integer) : Boolean;  Overload;
{ Returns where the given array element is in a string array }
VAR
  I : Integer;

BEGIN
  ElementPos := -1;
  Result := False;

  I := 0;
  WHILE (I <= High(StringArray)) AND (Result = False) DO BEGIN
    IF Element = StringArray[I] THEN BEGIN
      Result := True;
      ElementPos := I;
    END ELSE
      Inc(I);
  END; {WHILE}
END; { IsElementInStringArray-2 }

FUNCTION IsSignalInStringArray(StringArray : StringArrayType; Signal : Integer) : Boolean;
{ Returns whether the given signal is found in a string array }
VAR
  I : Integer;

BEGIN
  I := 0;
  Result := False;
  WHILE (I <= High(StringArray)) AND (Result = False) DO BEGIN
    IF (Signal <> UnknownSignal) AND (Signal = ExtractSignalFromString(StringArray[I])) THEN
      Result := True
    ELSE
      Inc(I);
  END; {WHILE}
END; { IsSignalInStringArray }

FUNCTION IsPointInStringArray(StringArray : StringArrayType; Point : Integer) : Boolean;
{ Returns whether the given point is found in a string array }
VAR
  I : Integer;

BEGIN
  I := 0;
  Result := False;
  WHILE (I <= High(StringArray)) AND (Result = False) DO BEGIN
    IF (Point <> UnknownPoint) AND (Point = ExtractPointFromString(StringArray[I])) THEN
      Result := True
    ELSE
      Inc(I);
  END; {WHILE}
END; { IsPointInStringArray }

FUNCTION IsTrackCircuitInStringArray(StringArray : StringArrayType; TC : Integer; OUT Pos : Integer) : Boolean;
{ Returns whether and where the given track circuit is found in a string array }
BEGIN
  Pos := 0;
  Result := False;
  WHILE (Pos <= High(StringArray)) AND (Result = False) DO BEGIN
    IF (TC <> UnknownTrackCircuit) AND (TC = ExtractTrackCircuitFromString(StringArray[Pos])) THEN
      Result := True
    ELSE
      Inc(Pos);
  END; {WHILE}
END; { IsTrackCircuitInStringArray }

FUNCTION IsElementSubRouteMarker(StringArray : StringArrayType; StringArrayPos : Word) : Boolean;
{ Returns whether the given array element is a subroute marker }
BEGIN
  Result := False;
  IF Pos('SR=', StringArray[StringArrayPos]) > 0 THEN
    Result := True;
END; { IsElementSubRouteMarker }

FUNCTION IsElementIndicator(StringArray : StringArrayType; StringArrayPos : Word) : Boolean;
{ Returns whether the given array element is a route indicator }
BEGIN
  Result := False;
  IF (Pos('|', StringArray[StringArrayPos]) > 0)
  OR (Pos('.', StringArray[StringArrayPos]) > 0)
  OR (Pos('>', StringArray[StringArrayPos]) > 0)
  THEN
    Result := True;
END; { IsElementIndicator }

FUNCTION LightsTypeToStr(TypeOfLights : LightsType) : String;
{ Return the type of lights the train has }
BEGIN
  CASE TypeOfLights OF
    NoLights:
      Result := 'No Lights';
    HeadlightsAndTailLightsConnected:
      Result := 'Headlights and tail lights connected';
    HeadlightsAndTailLightsSeparatelySwitched:
      Result := 'Headlights and tail lights separately switched';
    ExpressModelsSeparateHeadlights:
      Result := 'Express Models separate headlights';
    LightsOperatedByTwoChips:
      Result := 'Lights operated by two chips';
    LightsShouldBeDimmed:
      Result := 'Lights should be dimmed';
    CustomLightingKit:
      Result := 'Custom lighting kit';
  ELSE
    Result := 'Unknown lights type';
  END; {CASE}
END; { LightsTypeToStr }

FUNCTION LocationOccupied(Location : Integer) : Boolean;
{ Returns true if the given location has a feedback occupation }
VAR
  I : Integer;
  LocationTCs : IntegerArrayType;

BEGIN
  LocationTCs := GetTrackCircuitsForLocation(Location);
  Result := False;
  I := 0;
  WHILE (I <= High(LocationTCs)) AND (Result <> True) DO BEGIN
    IF TrackCircuits[LocationTCs[I]].TC_OccupationState = TCFeedbackOccupation THEN
      Result := True
    ELSE
      Inc(I);
  END; {WHILE}
END; { LocationOccupied }

FUNCTION LinesAreAdjacent(L1, L2 : Integer; ErrorMsg : String) : Boolean;
{ Returns true if the two given lines are adjacent }
BEGIN
  Result := False;
  ErrorMsg := '';

  IF Lines[L1].Line_NextUpLine = L2 THEN BEGIN
    Result := True;
    ErrorMsg := 'Next line up to ' + LineToStr(L1) + ' is ' + LineToStr(L2);
  END ELSE
    IF Lines[L1].Line_NextDownLine = L2 THEN BEGIN
      Result := True;
      ErrorMsg := 'Next line down to ' + LineToStr(L1) + ' is ' + LineToStr(L2);
    END ELSE
      IF Lines[L2].Line_NextUpLine = L1 THEN BEGIN
        Result := True;
        ErrorMsg := 'Next line up to ' + LineToStr(L2) + ' is ' + LineToStr(L1);
      END ELSE
        IF Lines[L2].Line_NextDownLine = L1 THEN BEGIN
          Result := True;
          ErrorMsg := 'Next line down to ' + LineToStr(L2) + ' is ' + LineToStr(L1);
        END;
END; { LinesAreAdjacent }

FUNCTION MulDiv(A, B, C : Integer) : Integer;
{ Returns (A * B) / C; intermediate result is held double-length to avoid overflow }
VAR
  X : Integer;

BEGIN
  X := (A * B);
  Result := X DIV C;
END; { MulDiv }

FUNCTION LocationOutOfUse(Location : Integer; OUT OutOfUseTC : Integer; OUT OutOfUseStr : String) : Boolean;
{ Returns true if the given location is out of use because of an out-of-use or similar track-circuit occupation }
VAR
  I : Integer;
  LocationTCs : IntegerArrayType;

BEGIN
  LocationTCs := GetTrackCircuitsForLocation(Location);
  Result := False;
  I := 0;
  WHILE (I <= High(LocationTCs)) AND (Result <> True) DO BEGIN
    IF TrackCircuitStateIsPermanentlyOccupied(TrackCircuits[LocationTCs[I]].TC_OccupationState) THEN BEGIN
      Result := True;
      { note one of the out of use track circuits for diagnostic purposes }
      OutOfUseTC := LocationTCs[I];
      OutOfUseStr := TrackCircuitStateToStr(TrackCircuits[LocationTCs[I]].TC_OccupationState);
    END ELSE
      Inc(I);
  END; {WHILE}
END; { LocationOutOfUse }

FUNCTION GetLocoRecFromLocoChip(LocoChip : Integer; OUT Loco : LocoRec) : Boolean;
{ Look for a matching loco record given a loco chip number }
VAR
  L : LocoIndex;
  LocoFound : Boolean;

BEGIN
  Result := False;

  TRY
    IF LocoChip = UnknownLocoChip THEN
      Exit
    ELSE BEGIN
      L := 0;
      LocoFound := False;
      WHILE (L <= High(Locos)) AND NOT LocoFound DO BEGIN
        { run through the train list, to find our train }
        IF Locos[L].Loco_LocoChip = LocoChip THEN
          LocoFound := True
        ELSE
          Inc(L);
      END; {WHILE}

      IF LocoFound THEN BEGIN
        Loco := Locos[L];
        Result := True;
      END;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG GetLocoIndexFromLocoChip: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { GetLocoIndexFromLocoChip }

FUNCTION GetLocoIndexFromLocoChip(LocoChip : Integer) : LocoIndex;
{ Look for a matching loco record given a loco chip number }
VAR
  L : LocoIndex;
  LocoFound : Boolean;

BEGIN
  Result := UnknownLocoIndex;
  TRY
    IF LocoChip = UnknownLocoChip THEN
      Exit
    ELSE BEGIN
      L := 0;
      LocoFound := False;
      WHILE (L <= High(Locos)) AND NOT LocoFound DO BEGIN
        { run through the train list, to find our train }
        IF Locos[L].Loco_LocoChip = LocoChip THEN
          LocoFound := True
        ELSE
          Inc(L);
      END; {WHILE}

      IF LocoFound THEN
        Result := L
      ELSE
        Result := UnknownLocoIndex;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG GetLocoIndexFromLocoChip: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { GetLocoIndexFromLocoChip }

FUNCTION GetLocoIndexFromTrainIndex(T : TrainIndex) : LocoIndex;  { should this include DG chips? &&&& }
{ Look for a matching loco record given a train index }
VAR
  L : LocoIndex;
  LocoFound : Boolean;

BEGIN
  Result := UnknownLocoIndex;
  TRY
    IF T = UnknownTrainIndex THEN BEGIN
      UnknownTrainRecordFound('GetLocoIndexFromTrainIndex');
      Exit;
    END ELSE BEGIN
      L := 0;
      LocoFound := False;
      WHILE (L <= High(Locos)) AND NOT LocoFound DO BEGIN
        { run through the train list, to find our train }
        IF Locos[L].Loco_LocoChip = Trains[T].Train_LocoChip THEN
          LocoFound := True
        ELSE
          Inc(L);
      END; {WHILE}

      IF LocoFound THEN
        Result := L
      ELSE
        Result := UnknownLocoIndex;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG GetLocoIndexFromTrainIndex: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { GetLocoIndexFromTrainIndex }

FUNCTION GetTrainIndexFromLocoChip(LocoChip : Integer) : TrainIndex;
{ Look for a matching train index given a locochip }
VAR
  T : TrainIndex;
  TrainFound : Boolean;

BEGIN
  Result := UnknownTrainIndex;
  TRY
    IF LocoChip = UnknownLocoChip THEN
      Exit
    ELSE BEGIN
      T := 0;
      TrainFound := False;
      WHILE (T <= High(Trains)) AND NOT TrainFound DO BEGIN
        { run through the train list, to find our train }
        IF Trains[T].Train_LocoChip = LocoChip THEN
          TrainFound := True
        ELSE
          Inc(T);
      END; {WHILE}

      IF TrainFound THEN
        Result := T;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG GetTrainIndexFromLocoChip: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { GetTrainIndexFromLocoChip }

FUNCTION IntToMPH(Speed : Integer) : MPHType;
{ Returns the given integer as an MPH value }
BEGIN
  CASE Speed OF
    0:
      Result := MPH0;
    10:
      Result := MPH10;
    20:
      Result := MPH20;
    30:
      Result := MPH30;
    40:
      Result := MPH40;
    50:
      Result := MPH50;
    60:
      Result := MPH60;
    70:
      Result := MPH70;
    80:
      Result := MPH80;
    90:
      Result := MPH90;
    100:
      Result := MPH100;
    110:
      Result := MPH110;
    120:
      Result := MPH120;
  ELSE
    Result := NoSpecifiedSpeed;
  END; {CASE}
END; { IntToMPH }

FUNCTION IOError(Filename : String; SaveIOResult : Integer; OUT ErrorMsg : String) : Boolean;
{ Returns the IO error message }
BEGIN
  IF SaveIOResult = 0 THEN BEGIN
    Result := False;
    ErrorMsg := '';
  END ELSE BEGIN
    Result := True;
    CASE SaveIOResult OF
      2:
        ErrorMsg := 'File: ' + Filename + ' not found';
      3:
        ErrorMsg := 'Path not found';
      5:
        ErrorMsg := 'File access denied - file ' + Filename + ' is a directory, or is read-only';
      13:
        ErrorMsg := 'Permission denied';
      20:
        ErrorMsg := 'is not a directory';
      21:
        ErrorMsg := 'is a directory';
      32:
        ErrorMsg := 'Sharing violation';
      100:
        ErrorMsg := 'Disk read error';
      101:
        ErrorMsg := 'Disk write error - is disk full?';
      102:
        ErrorMsg := 'File not assigned';
      103:
        ErrorMsg := 'File not open';
      104:
        ErrorMsg := 'File not open for input';
      105:
        ErrorMsg := 'File not open for output';
    ELSE
      ErrorMsg := 'I/O Error no.' + IntToStr(SaveIOResult);
    END; { CASE}
  END;
END; { IOError }

FUNCTION GetTrainTypeFromLocoChip(LocoChip : Integer) : TypeOfTrainType;
{ Returns the train type given the loco number }
VAR
  LocoChipFound : Boolean;
  T : TrainIndex;

BEGIN
  Result := UnknownTrainType;
  T := 0;
  LocoChipFound := False;
  WHILE (T <= High(Trains)) AND NOT LocoChipFound DO BEGIN
    IF Trains[T].Train_LocoChip = LocoChip THEN BEGIN
      LocoChipFound := True;
      Result := Trains[T].Train_Type;
    END;
   Inc(T);
  END; {WHILE}
END; { GetTrainTypeFromLocoChip }

PROCEDURE ReadOut(SoundStr : String);
{ Uses system API SndPlaySound to read out the given text, by playing a .wav file. text is held in the system resource file, itself compiled by using "brcc32 -v rail.rc"
  from the command prompt. The file "rail.rc" is the resource script file.
}
  PROCEDURE ProcessSound(SoundStr : String);
  { Send the sound to the speaker }
  VAR
    hFind, hRes: THandle;
    SoundPChar : PChar;
    SoundPWideChar : PWideChar;

  BEGIN
    SoundPWideChar := StringToOleStr(SoundStr);

    hFind := FindResource(HInstance, SoundPWideChar, 'WAV');
    IF hFind = 0 THEN
      Log('X Unknown wave file ''' + SoundStr + ''' passed to ReadOut')
    ELSE BEGIN
      hRes := LoadResource(HInstance, hFind);
      IF hRes <> 0 THEN BEGIN
        SoundPChar := LockResource(hRes);
        IF Assigned(SoundPChar) THEN
          SndPlaySound(SoundPChar, snd_NoStop OR Snd_Memory);
        UnlockResource(hRes);
      END;
      FreeResource(hFind);
    END;
  END; { ProcessSound }

BEGIN
  TRY
    { just some text }
    IF UpperCase(SoundStr) = 'SHORTCIRCUIT' THEN
      ProcessSound(SoundStr)
    ELSE
      IF UpperCase(SoundStr) = 'OFF' THEN
        ProcessSound(SoundStr)
      ELSE
        IF UpperCase(SoundStr) = 'ON' THEN
          ProcessSound(SoundStr)
        ELSE
          IF UpperCase(SoundStr) = 'OK' THEN
            ProcessSound(SoundStr)
          ELSE BEGIN
            IF Length(SoundStr) = 3 THEN BEGIN
              CASE SoundStr[1] OF
                '1':
                  ProcessSound('Sound100');
                '2':
                  ProcessSound('Sound200');
                '3':
                  ProcessSound('Sound300');
                '4':
                  ProcessSound('Sound400');
                '5':
                  ProcessSound('Sound500');
                '6':
                  ProcessSound('Sound600');
                '7':
                  ProcessSound('Sound700');
                '8':
                  ProcessSound('Sound800');
                '9':
                  ProcessSound('Sound900');
              ELSE {CASE}
                BEGIN
                  { an unknown word to read out }
                  Debug('Unknown word to read out: ' + SoundStr);
                  ProcessSound('IsThisCorrect');
                END;
              END; {CASE}
              { Remove the hundreds }
              SoundStr := Copy(SoundStr, 2);
              IF SoundStr = '00' THEN
                Exit;
            END;

            IF Length(SoundStr) = 2 THEN BEGIN
              IF (SoundStr[1] = '0') AND (SoundStr[2] <> '0') THEN BEGIN
                { avoid the problem of, e.g., "09" }
                SoundStr := Copy(SoundStr, 2);
              END;
            END;

            IF Length(SoundStr) = 2 THEN BEGIN
              CASE SoundStr[1] OF
                '1':
                  BEGIN
                    CASE SoundStr[2] OF
                      '0':
                        ProcessSound('Sound10');
                      '1':
                        ProcessSound('Sound11');
                      '2':
                        ProcessSound('Sound12');
                      '3':
                        ProcessSound('Sound13');
                      '4':
                        ProcessSound('Sound14');
                      '5':
                        ProcessSound('Sound15');
                      '6':
                        ProcessSound('Sound16');
                      '7':
                        ProcessSound('Sound17');
                      '8':
                        ProcessSound('Sound18');
                      '9':
                        ProcessSound('Sound19');
                    END; {CASE}
                    Exit;
                  END;
                '2':
                  ProcessSound('Sound20');
                '3':
                  ProcessSound('Sound30');
                '4':
                  ProcessSound('Sound40');
                '5':
                  ProcessSound('Sound50');
                '6':
                  ProcessSound('Sound60');
                '7':
                  ProcessSound('Sound70');
                '8':
                  ProcessSound('Sound80');
                '9':
                  ProcessSound('Sound90');
              ELSE {CASE}
                BEGIN
                  { an unknown word to read out }
                  Debug('Unknown word to read out: ' + SoundStr);
                  ProcessSound('IsThisCorrect');
                END;
              END; {CASE}
              { Remove the tens }
              SoundStr := Copy(SoundStr, 2);
              IF SoundStr = '0' THEN
                Exit;
            END;

            { the length is 1 }
            CASE SoundStr[Length(SoundStr)] OF
              '0':
                ProcessSound('Sound0');
              '1':
                ProcessSound('Sound1');
              '2':
                ProcessSound('Sound2');
              '3':
                ProcessSound('Sound3');
              '4':
                ProcessSound('Sound4');
              '5':
                ProcessSound('Sound5');
              '6':
                ProcessSound('Sound6');
              '7':
                ProcessSound('Sound7');
              '8':
                ProcessSound('Sound8');
              '9':
                ProcessSound('Sound9');
            ELSE {CASE}
              BEGIN
                { an unknown word to read out }
                Debug('Unknown word to read out: ' + SoundStr);
                ProcessSound('IsThisCorrect');
              END;
            END; {CASE }
          END;
  EXCEPT
    ON E : Exception DO
      Log('EG NewReadOut: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ReadOut }

FUNCTION LocationOccupationStateToStr(OccupationState : LocationOccupationStateType) : String;
{ Return the state of the Location Occupation as a string }
BEGIN
  CASE OccupationState OF
    LocationOutOfUseOccupation:
      Result := 'out of use';
    LocationEndOfDayOccupation:
      Result := 'end of day';
    LocationPermanentOccupationWithFeedback:
      Result := 'permanent with feedback';
    LocationPermanentOccupationSetByUser:
      Result := 'permanent set by user';
    LocationPermanentOccupationSetBySystem:
      Result := 'permanent set by system';
    LocationStartOfDayOccupation:
      Result := 'start of day';
    LocationTemporaryOccupation:
      Result := 'temporary';
    LocationUnoccupied:
      Result := 'no occupation';
    LocationUnknownOccupation:
      Result := 'unknown occupation';
  ELSE
    Result := 'Error in LocationOccupationStateToStr routine: invalid occupation state';
  END; {CASE}
END; { LocationOccupationStateToStr }

PROCEDURE RenameLaterFiles(VAR SuppliedFileType : TextFile; SuppliedFileName, SuppliedFilenamePrefix : String);
{ This is used when a current log is thrown away: the older log file names revert to their previous names }
VAR
  BackupFileName, BackupFileName0, BackupFileName1, BackupFileName2, BackupFileName3, BackupFileName4, BackupFileName5, BackupFileName6, BackupFileName7, BackupFileName8,
    BackupFileName9 : String;
  DirInfo : TSearchRec;

BEGIN
  TRY
    IF SuppliedFilename = '' THEN
      ShowMessage('No filename passed to RenameLaterFiles routine')
    ELSE BEGIN
      BackupFileName := SuppliedFileName + '.' + SuppliedFilenamePrefix;
      BackupFileName0 := SuppliedFileName + '.0.' + SuppliedFilenamePrefix;
      BackupFileName1 := SuppliedFileName + '.1.' + SuppliedFilenamePrefix;
      BackupFileName2 := SuppliedFileName + '.2.' + SuppliedFilenamePrefix;
      BackupFileName3 := SuppliedFileName + '.3.' + SuppliedFilenamePrefix;
      BackupFileName4 := SuppliedFileName + '.4.' + SuppliedFilenamePrefix;
      BackupFileName5 := SuppliedFileName + '.5.' + SuppliedFilenamePrefix;
      BackupFileName6 := SuppliedFileName + '.6.' + SuppliedFilenamePrefix;
      BackupFileName7 := SuppliedFileName + '.7.' + SuppliedFilenamePrefix;
      BackupFileName8 := SuppliedFileName + '.8.' + SuppliedFilenamePrefix;
      BackupFileName9 := SuppliedFileName + '.9.' + SuppliedFilenamePrefix;

      IF FindFirst(BackupFileName, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename);
        Erase(SuppliedFileType);
        FindClose(DirInfo);
      END;
      IF FindFirst(BackupFileName0, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename0);
        Rename(SuppliedFileType, BackupFilename);
        FindClose(DirInfo);
      END;
      IF FindFirst(BackupFileName1, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename1);
        Rename(SuppliedFileType, BackupFilename0);
        FindClose(DirInfo);
      END;
      IF FindFirst(BackupFileName2, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename2);
        Rename(SuppliedFileType, BackupFilename1);
        FindClose(DirInfo);
      END;
      IF FindFirst(BackupFileName3, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename3);
        Rename(SuppliedFileType, BackupFilename2);
        FindClose(DirInfo);
      END;
      IF FindFirst(BackupFileName4, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename4);
        Rename(SuppliedFileType, BackupFilename3);
        FindClose(DirInfo);
      END;
      IF FindFirst(BackupFileName5, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename5);
        Rename(SuppliedFileType, BackupFilename4);
        FindClose(DirInfo);
      END;
      IF FindFirst(BackupFileName6, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename6);
        Rename(SuppliedFileType, BackupFilename5);
        FindClose(DirInfo);
      END;
      IF FindFirst(BackupFileName7, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename7);
        Rename(SuppliedFileType, BackupFilename6);
        FindClose(DirInfo);
      END;
      IF FindFirst(BackupFileName8, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename8);
        Rename(SuppliedFileType, BackupFilename7);
        FindClose(DirInfo);
      END;
      IF FindFirst(BackupFileName9, faAnyFile, DirInfo) = 0 THEN BEGIN
        AssignFile(SuppliedFileType, BackupFilename9);
        Rename(SuppliedFileType, BackupFilename8);
        FindClose(DirInfo);
      END;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG RenameLaterFiles: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { RenameLaterFiles }

PROCEDURE RenameEarlierFiles(VAR SuppliedFileType : TextFile; SuppliedFileName, SuppliedFilenamePrefix : String);
{ Renames any existing earlier files, to give three consecutive backups }
VAR
  BackupFileName0, BackupFileName1, BackupFileName2, BackupFileName3, BackupFileName4, BackupFileName5, BackupFileName6, BackupFileName7, BackupFileName8,
    BackupFileName9 : String;
  DirInfo : TSearchRec;
  ErrorMsg : String;

BEGIN
  IF SuppliedFilename = '' THEN
    ShowMessage('No filename passed to RenameEarlierFiles routine')
  ELSE BEGIN
    BackupFileName0 := SuppliedFileName + '.0.' + SuppliedFilenamePrefix;
    BackupFileName1 := SuppliedFileName + '.1.' + SuppliedFilenamePrefix;
    BackupFileName2 := SuppliedFileName + '.2.' + SuppliedFilenamePrefix;
    BackupFileName3 := SuppliedFileName + '.3.' + SuppliedFilenamePrefix;
    BackupFileName4 := SuppliedFileName + '.4.' + SuppliedFilenamePrefix;
    BackupFileName5 := SuppliedFileName + '.5.' + SuppliedFilenamePrefix;
    BackupFileName6 := SuppliedFileName + '.6.' + SuppliedFilenamePrefix;
    BackupFileName7 := SuppliedFileName + '.7.' + SuppliedFilenamePrefix;
    BackupFileName8 := SuppliedFileName + '.8.' + SuppliedFilenamePrefix;
    BackupFileName9 := SuppliedFileName + '.9.' + SuppliedFilenamePrefix;

    IF FindFirst(BackupFileName9, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, BackupFilename9);
      Erase(SuppliedFileType);
      FindClose(DirInfo);
    END;
    IF FindFirst(BackupFileName8, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, BackupFilename8);
      Rename(SuppliedFileType, BackupFilename9);
      FindClose(DirInfo);
    END;
    IF FindFirst(BackupFileName7, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, BackupFilename7);
      Rename(SuppliedFileType, BackupFilename8);
      FindClose(DirInfo);
    END;
    IF FindFirst(BackupFileName6, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, BackupFilename6);
      Rename(SuppliedFileType, BackupFilename7);
      FindClose(DirInfo);
    END;
    IF FindFirst(BackupFileName5, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, BackupFilename5);
      Rename(SuppliedFileType, BackupFilename6);
      FindClose(DirInfo);
    END;
    IF FindFirst(BackupFileName4, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, BackupFilename4);
      Rename(SuppliedFileType, BackupFilename5);
      FindClose(DirInfo);
    END;
    IF FindFirst(BackupFileName3, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, BackupFilename3);
      Rename(SuppliedFileType, BackupFilename4);
      FindClose(DirInfo);
    END;
    IF FindFirst(BackupFileName2, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, BackupFilename2);
      Rename(SuppliedFileType, BackupFilename3);
      FindClose(DirInfo);
    END;
    IF FindFirst(BackupFileName1, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, BackupFilename1);
      Rename(SuppliedFileType, BackupFilename2);
      FindClose(DirInfo);
    END;
    IF FindFirst(BackupFileName0, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, BackupFilename0);
      Rename(SuppliedFileType, BackupFilename1);
      FindClose(DirInfo);
    END;

    { needs more checking - if LocoLocations file is open when we start up, for instance, or when we exit **** }
    IF FindFirst(SuppliedFileName + '.' + SuppliedFilenamePrefix, faAnyFile, DirInfo) = 0 THEN BEGIN
      AssignFile(SuppliedFileType, SuppliedFilename + '.' + SuppliedFilenamePrefix);
      {$I-}
      Rename(SuppliedFileType, BackupFilename0);
      {$I-}
      IF IOError(SuppliedFileName + '.' + SuppliedFilenamePrefix, IOResult, ErrorMsg) THEN BEGIN
        Log('EG *** Warning: unable to rename "' + SuppliedFileName + '.' + SuppliedFilenamePrefix + '": ' + ErrorMsg + '***');
        ShowMessage('Warning: unable to rename "' + SuppliedFileName + '.' + SuppliedFilenamePrefix + '": ' + ErrorMsg);
      END;
      FindClose(DirInfo);
    END;
    AssignFile(SuppliedFileType, SuppliedFilename + '.' + SuppliedFilenamePrefix);
  END;
END; { RenameEarlierFiles }

PROCEDURE MakeSound(SoundNum : Integer);
{ Make a warning sound }
VAR
  mb: DWord;

BEGIN
  { Set up a breakpoint if required - sometimes cannot tell why sounds are being produced }
  IF BreakPointRequiredInMakeSoundRoutine THEN BEGIN
    BreakPointRequiredInMakeSoundRoutine := False;
    Log('X Breakpoint in MakeSound routine reached');
    ASM
      Int 3
    END; {ASM}
  END;

  { Suppress it if it's a part of a succession of beeps }
  IF NewMilliSecondsBetween(LastSoundTime, Now) < 250 THEN
    Exit;

  CASE SoundNum of
    1:
      BEGIN
        mb := MB_ICONASTERISK; //SystemAsterisk
        Log('X [Beep] (asterisk)');
      END;
    2:
      BEGIN
        mb := MB_ICONEXCLAMATION; //SystemExclamation
        Log('X [Beep] (exclamation)');
      END;
    3:
      BEGIN
        mb := MB_ICONHAND; //SystemHand
        Log('X [Beep] (hand)');
      END;
    4:
      BEGIN
        mb := MB_ICONQUESTION; //SystemQuestion
        Log('X [Beep] (question)');
      END;
    5:
      BEGIN
        mb := MB_OK; //SystemDefault
        Log('X [Beep] (default)');
      END;
  ELSE
    BEGIN
      mb:= $0FFFFFFFF; //Standard beep using the computer speaker
      Log('X [Beep]');
    END;
  END; {CASE}
  MessageBeep(mb);
  LastSoundTime := Now;
END; { MakeSound }

FUNCTION RemoveAllSpacesFromAString(Str : String) : String;
{ Removes all spaces from a given string }
VAR
  I : Integer;

BEGIN
  I := 1;
  WHILE I <= Length(Str) DO BEGIN
    IF (Length(Str) > 0) AND (Str[I] = ' ') THEN BEGIN
      { note: Delete may need "System" as because of a "WITH" statement it could become a DataSet command }
      System.Delete(Str, I, 1);
      I := I - 1;
    END;
    IF (Length(Str) > 3) AND (Copy(Str, I, 3) = '%20') THEN BEGIN
      { note: Delete may need "System" as because of a "WITH" statement it could become a DataSet command }
      System.Delete(Str, I, 3);
      I := I - 3;
    END;
    Inc(I);
  END; {WHILE}
  Result := Str;
END; { RemoveAllSpacesFromAString }

FUNCTION BufferStopToStr(BufferStop : Integer) : String;
{ Return the buffer stop number as a string }
BEGIN
  IF BufferStop = UnknownBufferStop THEN
    Result := UnknownBufferStopStr
  ELSE
    Result := IntToStr(BufferStop);
END; { BufferStopToStr }

FUNCTION JourneyToStr(Journey : Integer) : String;
{ Return a journey number as a string }
BEGIN
  IF Journey = UnknownJourney THEN
    Result := UnknownJourneyStr
  ELSE
    Result := IntToStr(Journey);
END; { JourneyToStr }

FUNCTION LineToStr(L : Integer) : String;
{ Return a line's name as a string }
BEGIN
  IF (L <> UnknownLine) AND (Length(Lines) > 0) THEN
    Result := Lines[L].Line_NameStr
  ELSE
    Result := UnknownLineStr;
END; { LineToStr }

FUNCTION LocoChipToStr{1}(LocoChip : Integer) : String; Overload;
{ Return the locomotive number as a string - if LocoChip less then 1000, add appropriate number of leading zeros }
CONST
  UnknownAsZeroes = True;

BEGIN
  Result := LocoChipToStr(LocoChip, UnknownAsZeroes);
END; { LocoChipToStr-1 }

FUNCTION LocoChipToStr{2}(LocoChip : Integer; UnknownAsZeroes : Boolean) : String; Overload;
{ Return the locomotive number as a string - if LocoChip less then 1000, add appropriate number of leading zeros }
BEGIN
  IF LocoChip = UnknownLocoChip THEN BEGIN
    IF UnknownAsZeroes THEN
      Result := '----'
    ELSE
      Result := UnknownLocoChipStr;
  END ELSE
    IF LocoChip < 10 THEN
      Result := '000' + IntToStr(LocoChip)
    ELSE
      IF LocoChip < 100 THEN
        Result := '00' + IntToStr(LocoChip)
      ELSE
        IF LocoChip < 1000 THEN
          Result := '0' + IntToStr(LocoChip)
        ELSE
          Result := IntToStr(LocoChip);
END; { LocoChipToStr-2 }

FUNCTION LocoIndexToStr(L : LocoIndex) : String;
{ Return the locomotive index as a string }
BEGIN
  Result := IntToStr(L);
END; { LocoIndexToStr }

FUNCTION PointToStr(Point : Integer) : String;
{ Return the point number as a string }
BEGIN
  IF Point = UnknownPoint THEN
    Result := UnknownPointStr
  ELSE
    Result := IntToStr(Point);
END; { PointToStr }

FUNCTION RouteToStr(Route : Integer) : String;
{ Return the Route number as a string }
BEGIN
  IF Route = UnknownRoute THEN
    Result := UnknownRouteStr
  ELSE
    Result := IntToStr(Route);
END; { RouteToStr }

FUNCTION SignalToStr(Signal : Integer) : String;
{ Return the signal number as a string }
BEGIN
  IF Signal = UnknownSignal THEN
    Result := UnknownSignalStr
  ELSE
    Result := IntToStr(Signal);
END; { SignalToStr }

FUNCTION TrackCircuitToStr(TrackCircuit : Integer) : String;
{ Return the track-circuit number as a string }
BEGIN
  IF TrackCircuit = UnknownTrackCircuit THEN
    Result := UnknownTrackCircuitStr
  ELSE
    Result := IntToStr(TrackCircuit);
END; { TrackCircuitToStr }

FUNCTION AddSeparatorToTimeString(Str : String) : String;
{ Add a separator to a string so that, e.g., 0630 becomes 06:30 }
BEGIN
  Result := LeftStr(Str, 2) + ':' + Copy(Str, 3, 2);
END; { AddSeparatorTimeToString }

FUNCTION GetMidPos(I1, I2 : Integer) : Integer;
{ Return the mid position between two given values }
BEGIN
  IF I1 < I2 THEN
    Result := I1 + ((I2 - I1) DIV 2)
  ELSE
    Result := I2 + ((I1 - I2) DIV 2);
END; { GetMidPos }

FUNCTION AreaArrayToStr(AreaArray : IntegerArrayType) : String;
{ Return an area array as a string }
VAR
  I : Integer;

BEGIN
  Result := '';
  IF Length(AreaArray) > 0 THEN BEGIN
    FOR I := 0 TO High(AreaArray) DO BEGIN
      IF I = 0 THEN
        Result := AreaToStr(AreaArray[I])
      ELSE
        Result := Result + ', ' + AreaToStr(AreaArray[I]);
    END;
  END;
END; { AreaArrayToStr }

FUNCTION AreaToStr{1}(Area : Integer) : String; Overload;
{ Return an area as a long string }
VAR
  FoundArea : Boolean;
  SearchArea : Integer;

BEGIN
  Result := UnknownAreaStr;
  SearchArea := 0;
  FoundArea := False;
  WHILE (SearchArea <= High(Areas)) AND NOT FoundArea DO BEGIN
    IF Area = SearchArea THEN BEGIN
      FoundArea := True;
      Result := Areas[Area].Area_LongStr
    END ELSE
      Inc(SearchArea);
  END; {WHILE}
END; { AreaToStr-1 }

FUNCTION AreaToStr{2}(Area : Integer; LongOrShortString : StringType) : String; Overload;
{ Return an area as either a short or a long string }
VAR
  FoundArea : Boolean;
  SearchArea : Integer;

BEGIN
  Result := UnknownAreaStr;
  SearchArea := 0;
  FoundArea := False;
  WHILE (SearchArea <= High(Areas)) AND NOT FoundArea DO BEGIN
    IF Area = SearchArea THEN BEGIN
      FoundArea := True;
      IF LongOrShortString = LongStringType THEN
        Result := Areas[Area].Area_LongStr
      ELSE
        Result := Areas[Area].Area_ShortStr;
    END ELSE
      Inc(SearchArea);
  END; {WHILE}
END; { AreaToStr-2 }

FUNCTION ATS(Area : Integer) : String;
{ Return an area as a string - routine designed only for use in debugging }
BEGIN
  Result := AreaToStr(Area);
END; { ATS }

FUNCTION ATSA(AreaArray : IntegerArrayType) : String;
{ Return an area array as a string - routine designed for use in debugging }
VAR
  I : Integer;

BEGIN
  Result := '';
  IF Length(AreaArray) > 0 THEN BEGIN
    FOR I := 0 TO High(AreaArray) DO BEGIN
      IF I = 0 THEN
        Result := AreaToStr(AreaArray[I])
      ELSE
        Result := Result + ', ' + AreaToStr(AreaArray[I]);
    END;
  END;
END; { ATSA }

FUNCTION GetAreaFromStationMonitorsDisplayOrderNum(OrderNum : Integer) : Integer;
{ Return the Area indicated by the station monitors' display order number }
VAR
  A : Integer;
  OrderNumFound : Boolean;

BEGIN
  Result := -1;

  A := 0;
  OrderNumFound := False;
  WHILE (A <= High(Areas)) AND NOT OrderNumFound DO BEGIN
    IF Areas[A].Area_StationMonitorsDisplayOrderNum = OrderNum THEN BEGIN
      Result := A;
      OrderNumFound := True;
    END ELSE
      Inc(A);
  END; {WHILE}
END; { GetStationMonitorsDisplayOrderStr }

FUNCTION GetStationMonitorsDisplayOrderStr(OrderNum : Integer) : String;
{ Return the long description of the Area indicated by the station monitors' display order number }
VAR
  A : Integer;
  OrderNumFound : Boolean;

BEGIN
  Result := '';

  A := 0;
  OrderNumFound := False;
  WHILE (A <= High(Areas)) AND NOT OrderNumFound DO BEGIN
    IF Areas[A].Area_StationMonitorsDisplayOrderNum = OrderNum THEN BEGIN
      Result := Areas[A].Area_LongStr;
      OrderNumFound := True;
    END ELSE
      Inc(A);
  END; {WHILE}
END; { GetStationMonitorsDisplayOrderStr }

FUNCTION GetStationNumFromStationMonitorsDisplayOrderNum(StationMonitorsDisplayOrderStr : String) : Integer;
{ returns the station monitors' display order number from the long or short station name }
VAR
  A : Integer;
  OrderNumFound : Boolean;

BEGIN
  Result := -1;

  A := 0;
  OrderNumFound := False;
  WHILE (A <= High(Areas)) AND NOT OrderNumFound DO BEGIN
    StationMonitorsDisplayOrderStr := RemoveAllSpacesFromAString(StationMonitorsDisplayOrderStr);
    IF (Pos(UpperCase(StationMonitorsDisplayOrderStr), UpperCase(RemoveAllSpacesFromAString(Areas[A].Area_LongStr))) > 0)
    OR (Pos(UpperCase(StationMonitorsDisplayOrderStr), UpperCase(RemoveAllSpacesFromAString(Areas[A].Area_ShortStr))) > 0)
    THEN BEGIN
      Result := A;
      OrderNumFound := True;
    END ELSE
      Inc(A);
  END; {WHILE}
END; { GetStationNumFromStationMonitorsDisplayOrderNum }

FUNCTION LenzConnectionToStr(LenzConnection : LenzConnectionType) : String;
{ Return the kind of connection }
BEGIN
  CASE LenzConnection OF
    USBConnection:
      Result := 'USB Connection';
    EthernetConnection:
      Result := 'Ethernet Connection';
    NoConnection:
      Result := 'No Connection';
  END; {CASE}
END; { LenzConnectionToStr }

FUNCTION LTS(L : Integer) : String;
{ Return a line as a string - routine designed for use in debugging }
BEGIN
  IF (Length(Lines) = 0) OR (L = UnknownLine) THEN
    Result := UnknownLineStr
  ELSE
    Result := Lines[L].Line_NameStr;
END; { LTS }

FUNCTION LATS(LineArray : IntegerArrayType) : String;
{ Return line array as a string - routine designed for use in debugging }
VAR
  I : Integer;

BEGIN
  Result := '';
  IF Length(LineArray) > 0 THEN BEGIN
    FOR I := 0 TO High(LineArray) DO BEGIN
      IF I = 0 THEN
        Result := LineToStr(LineArray[I])
      ELSE
        Result := Result + ', ' + LineToStr(LineArray[I]);
    END;
  END;
END; { LATS }

PROCEDURE LoadPreviousPointSettings;
{ Load the previous settings - generally used when starting up offline }
VAR
  P : Integer;

BEGIN
  IF SystemOnline THEN
    Debug('Cannot load previous point settings if system online')
  ELSE BEGIN
    FOR P := 0 TO High(Points) DO BEGIN
      IF Points[P].Point_ManualOperation THEN
        Points[P].Point_PresentState := Points[P].Point_LastManualStateAsReadIn
      ELSE
        Points[P].Point_PresentState := Points[P].Point_LastFeedbackStateAsReadIn;
    END; {FOR}
    Debug('Previous point settings loaded');

    InvalidateScreen(UnitRef, 'Load latest point settings in offline mode');
  END;
END; { LoadPreviousPointSettings }

FUNCTION LocationToStrMainProcedure(Location : Integer; LongOrShortString : StringType) : String;
{ Return a location as a string }
VAR
  FoundLocation : Boolean;
  SearchLocation : Integer;

BEGIN
  Result := UnknownLocationStr;
  IF Length(Locations) > 0 THEN BEGIN
    SearchLocation := 0;
    FoundLocation := False;
    WHILE (SearchLocation <= High(Locations)) AND NOT FoundLocation DO BEGIN
      IF Location = SearchLocation THEN BEGIN
        FoundLocation := True;
        IF LongOrShortString = LongStringType THEN
          Result := Locations[Location].Location_LongNameStr
        ELSE
          Result := Locations[Location].Location_ShortNameStr;
      END ELSE
        Inc(SearchLocation);
    END; {WHILE}
  END;
END; { LocationToStrMainProcedure }

FUNCTION LocTS(Location : Integer) : String;
{ Return a location as a string - routine designed for use in debugging }
CONST
  LongOrShortString = LongStringType;

BEGIN
  IF (Length(Locations) = 0) OR (Location = UnknownLocation) THEN
    Result := UnknownLocationStr
  ELSE
    Result := LocationToStrMainProcedure(Location, LongOrShortString);
END; { LocTS }

FUNCTION LocATS(LocationArray : IntegerArrayType) : String;
{ Return locations as a string - routine designed for use in debugging }
CONST
  LongOrShortString = LongStringType;

VAR
  I : Integer;

BEGIN
  Result := '';
  IF Length(LocationArray) > 0 THEN BEGIN
    FOR I := 0 TO High(LocationArray) DO BEGIN
      IF I = 0 THEN
        Result := LocationToStrMainProcedure(LocationArray[I], LongOrShortString)
      ELSE
        Result := Result + ', ' + LocationToStrMainProcedure(LocationArray[I], LongOrShortString);
    END;
  END;
END; { LocATS }

FUNCTION LocationToStr{1}(Location : Integer) : String; Overload;
{ Return a location as a long string }
CONST
  LongOrShortString = LongStringType;

BEGIN
  Result := LocationToStrMainProcedure(Location, LongOrShortString);
END; { LocationToStr-1 }

FUNCTION LocationToStr{2}(Location : Integer; LongOrShortString : StringType) : String; Overload;
{ Return a location as a string }
BEGIN
  Result := LocationToStrMainProcedure(Location, LongOrShortString);
END; { LocationToStr-2 }

FUNCTION MapGridYToRow(GridY : Integer) : Extended;
{ Map grid co-ordinate to row }
BEGIN
  Result := MapGridYToScreenY(GridY);
  Result := Result / GridInterLineSpacing;
  Result := Round(Result * 10) / 10;
END; { MapGridYToRow }

FUNCTION MapGridXToScreenX(GridX : Integer) : Integer;
{ Map grid co-ordinate to screen co-ordinate }
BEGIN
  Result := MulDiv(FWPRailWindow.ClientWidth, GridX, ZoomScaleFactor) - ScrollBarXAdjustment;
END; { MapGridXToScreenX }

FUNCTION MapGridYToScreenY(GridY : Integer) : Integer;
{ Map grid co-ordinate to screen co-ordinate }
BEGIN
  Result := MulDiv(FWPRailWindow.ClientHeight, GridY, ZoomScaleFactor) - ScrollBarYAdjustment;
END; { MapGridYToScreenY }

FUNCTION MapScreenXToGridX(ScreenX : Integer) : Integer;
{ Map screen co-ordinate to grid co-ordinate }
BEGIN
  Result := MulDiv(ZoomScaleFactor, ScreenX + ScrollBarXAdjustment, FWPRailWindow.ClientWidth);
END; { MapScreenXToGridX }

FUNCTION MapScreenYToGridY(ScreenY : Integer) : Integer;
{ Map screen co-ordinate to grid co-ordinate }
BEGIN
  Result := MulDiv(ZoomScaleFactor, ScreenY + ScrollBarYAdjustment, FWPRailWindow.ClientHeight);
END; { MapScreenYToGridY }

FUNCTION MPHToInt(MPH : MPHType) : Integer;
{ Returns the given MPH as an integer }
BEGIN
  CASE MPH OF
    MPH0:
      Result := 0;
    MPH10:
      Result := 10;
    MPH20:
      Result := 20;
    MPH30:
      Result := 30;
    MPH40:
      Result := 40;
    MPH50:
      Result := 50;
    MPH60:
      Result := 60;
    MPH70:
      Result := 70;
    MPH80:
      Result := 80;
    MPH90:
      Result := 90;
    MPH100:
      Result := 100;
    MPH110:
      Result := 110;
    MPH120:
      Result := 120;
  ELSE
    Result := 0;
  END; {CASE}
END; { MPHToInt }

FUNCTION MPHToStr(MPH : MPHType) : String;
{ Returns the given MPH as a string }
BEGIN
  CASE MPH OF
    MPH0:
      Result := '0';
    MPH10:
      Result := '10';
    MPH20:
      Result := '20';
    MPH30:
      Result := '30';
    MPH40:
      Result := '40';
    MPH50:
      Result := '50';
    MPH60:
      Result := '60';
    MPH70:
      Result := '70';
    MPH80:
      Result := '80';
    MPH90:
      Result := '90';
    MPH100:
      Result := '100';
    MPH110:
      Result := '110';
    MPH120:
      Result := '120';
    UnknownMPH:
      Result := '?';
    NoSpecifiedSpeed:
      Result := '';
  ELSE
    Result := '?';
  END; {CASE}
END; { MPHToStr }

FUNCTION StrToMPH(Str : String) : MPHType;
{ Returns the given string as a MPH }
BEGIN
  IF Str = '0' THEN
    Result := MPH0
  ELSE
    IF Str = '10' THEN
      Result := MPH10
    ELSE
      IF Str = '20' THEN
        Result := MPH20
      ELSE
        IF Str = '30' THEN
          Result := MPH30
        ELSE
          IF Str = '40' THEN
            Result := MPH40
          ELSE
            IF Str = '50' THEN
              Result := MPH50
            ELSE
              IF Str = '60' THEN
                Result := MPH60
              ELSE
                IF Str = '70' THEN
                  Result := MPH70
                ELSE
                  IF Str = '80' THEN
                    Result := MPH80
                  ELSE
                    IF Str = '90' THEN
                      Result := MPH90
                    ELSE
                      IF Str = '100' THEN
                        Result := MPH100
                      ELSE
                        IF Str = '110' THEN
                          Result := MPH110
                        ELSE
                          IF Str = '120' THEN
                            Result := MPH120
                          ELSE
                            Result := NospecifiedSpeed;
END; { MPHToStr }

FUNCTION OpenInputFileOK(VAR InputFilename : Text; Filename : String; OUT ErrorMsg : String) : Boolean;
{ Open a existing file to read from }
BEGIN
  AssignFile(InputFilename, Filename);
  {$I-}
  Reset(InputFilename);
  {$I+}
  Result := NOT IOError(Filename, IOResult, ErrorMsg);

  IF Result = False THEN BEGIN
   Log('EG *** Unable to open file: ' + Filename + ' for reading: ' + ErrorMsg + ' ***');
   ShowMessage('Warning! Unable to open file "' + Filename + '" for reading: ' + ErrorMsg);
  END;
END; { OpenInputFileOK }

{$O-}
FUNCTION OpenOutputFileOK(VAR OutputFilename : Text; Filename : String; OUT ErrorMsg : String; AppendToFile : Boolean) : Boolean;
{ Open (and create if necessary) a file }
BEGIN
  Result := False;

  { If file exists, append to it, else create it }
  {$I-}
  AssignFile(OutputFilename, Filename);
  IF AppendToFile THEN BEGIN
    Append(OutputFilename);
    {$I+}
    IF NOT IOError(Filename, IOResult, ErrorMsg) THEN
      Result := True;
  END;

  IF NOT AppendToFile OR (Result = False) THEN BEGIN
    {$I-}
    Rewrite(OutputFileName);
    {$I+}
    Result := NOT IOError(Filename, IOResult, ErrorMsg);
  END;

  IF Result = False THEN BEGIN
   Log('EG *** Unable to open file: ' + Filename + ' for writing: ' + ErrorMsg + ' ***');
   ShowMessage('Warning! Unable to open file "' + Filename + '" for writing: ' + ErrorMsg);
  END;
END; { OpenOutputFileOK }

PROCEDURE CloseInputOrOutputFile(VAR InputOrOutputFile : Text; Filename : String);
{ Close a file, capturing the error message if any }
VAR
  ErrorMsg : String;

BEGIN
  {$I-}
  CloseFile(InputOrOutputFile);
  {$I+}
  IF IOError(Filename, IOResult, ErrorMsg) THEN
    Log('AG Error in closing file ' + Filename + ': ' + ErrorMsg);
END; { CloseInputOrOutputFile }

FUNCTION DirectionToStr{1}(Dir : DirectionType) : String; Overload;
{ Return the long name of a direction }
BEGIN
  CASE Dir OF
    Up:
      Result := 'Up';
    Down:
      Result := 'Down';
    Bidirectional:
      Result := 'Bidirectional';
    UnknownDirection:
      Result := '?';
  ELSE
    Result := '?';
  END; {CASE}
END; { DirectionToStr-1 }

FUNCTION DirectionToStr{2}(Dir : DirectionType; LongOrShortString : StringType) : String; Overload;
{ Return the name of a direction }
BEGIN
  CASE Dir OF
    Up:
      CASE LongOrShortString OF
        LongStringType, ShortStringType:
          Result := 'Up';
        VeryShortStringType:
          Result := 'U';
      END; {CASE}
    Down:
      CASE LongOrShortString OF
        LongStringType:
          Result := 'Down';
        ShortStringType:
          Result := 'Dn';
        VeryShortStringType:
          Result := 'D';
      END; {CASE}
    Bidirectional:
      CASE LongOrShortString OF
        LongStringType:
          Result := 'Bidirectional';
        ShortStringType, VeryShortStringType:
          Result := 'B';
      END; {CASE}
    UnknownDirection:
      Result := '-';
  ELSE
    Result := '?';
  END; {CASE}
END; { DirectionToStr-2 }

FUNCTION OppositeDirection(Dir : DirectionType) : DirectionType;
{ Return the direction opposite to the one given }
BEGIN
  CASE Dir OF
    Up:
     Result := Down;
    Down:
      Result := Up;
    Bidirectional:
      Result := Bidirectional;
  ELSE
    Result := UnknownDirection;
  END; {CASE}
END; { OppositeDirection }

PROCEDURE DrawDebugWindow;
BEGIN
  DebugWindow.Height := DebugWindowHeight;
  DebugWindow.Width := DebugWindowWidth;
  DebugWindow.Top := DebugWindowTop;
  DebugWindow.Left := DebugWindowLeft;

  DebugWindow.Visible := True;
  DebugWindow.Invalidate;
END; { DrawDebugWindow }

PROCEDURE InsertElementInStringArray(VAR StringArray : StringArrayType; Position : Integer; NewElement : String);
{ Adds an element to a string array }
VAR
  I : Integer;

BEGIN
  SetLength(StringArray, Length(StringArray) + 1);
  { Move all existing elements up one }
  IF Length(StringArray) = 1 THEN
    StringArray[0] := NewElement
  ELSE BEGIN
    FOR I := (Length(StringArray) - 1) DOWNTO (Position + 1) DO
      StringArray[I] := StringArray[I - 1];
    { and insert the new element }
    StringArray[Position] := NewElement;
  END;
END; { InsertElementInStringArray }

PROCEDURE InsertElementInBooleanArray(VAR BooleanArray : BooleanArrayType; Position : Integer; NewElement : Boolean);
{ Adds an element to a boolean array }
VAR
  I : Integer;

BEGIN
  SetLength(BooleanArray, Length(BooleanArray) + 1);
  { Move all existing elements up one }
  IF Length(BooleanArray) = 1 THEN
    BooleanArray[0] := NewElement
  ELSE BEGIN
    FOR I := (Length(BooleanArray) - 1) DOWNTO (Position + 1) DO
      BooleanArray[I] := BooleanArray[I - 1];
    { and insert the new element }
    BooleanArray[Position] := NewElement;
  END;
END; { InsertElementInBooleanArray }

PROCEDURE InsertElementInDateTimeArray(VAR DateTimeArray : DateTimeArrayType; Position : Integer; NewElement : TDateTime);
{ Adds an element to a TDateTime array }
VAR
  I : Integer;

BEGIN
  SetLength(DateTimeArray, Length(DateTimeArray) + 1);
  { Move all existing elements up one }
  IF Length(DateTimeArray) = 1 THEN
    DateTimeArray[0] := NewElement
  ELSE BEGIN
    FOR I := (Length(DateTimeArray) - 1) DOWNTO (Position + 1) DO
      DateTimeArray[I] := DateTimeArray[I - 1];
    { and insert the new element }
    DateTimeArray[Position] := NewElement;
  END;
END; { InsertElementInDateTimeArray }

PROCEDURE InsertElementInTrainJourneyRecArray(VAR JourneyRecArray : TrainJourneyRecArrayType; Position : Integer;
                                              NewElement : TrainJourneyRec);
{ Adds an element to a TrainJourneyRec array }
VAR
  I : Integer;

BEGIN
  SetLength(JourneyRecArray, Length(JourneyRecArray) + 1);
  { Move all existing elements up one }
  IF Length(JourneyRecArray) = 1 THEN
    JourneyRecArray[0] := NewElement
  ELSE BEGIN
    FOR I := (Length(JourneyRecArray) - 1) DOWNTO (Position + 1) DO
      JourneyRecArray[I] := JourneyRecArray[I - 1];
    { and insert the new element }
    JourneyRecArray[Position] := NewElement;
  END;
END; { InsertElementInTrainJourneyRecArray }

PROCEDURE InsertElementInIntegerArray(VAR IntegerArray : IntegerArrayType; Position : Integer; NewElement : Integer);
{ Adds an element to an integer array }
VAR
  I : Integer;

BEGIN
  SetLength(IntegerArray, Length(IntegerArray) + 1);
  { Move all existing elements up one }
  IF Length(IntegerArray) = 1 THEN
    IntegerArray[0] := NewElement
  ELSE BEGIN
    FOR I := (Length(IntegerArray) - 1) DOWNTO (Position + 1) DO
      IntegerArray[I] := IntegerArray[I - 1];
    { and insert the new element }
    IntegerArray[Position] := NewElement;
  END;
END; { InsertElementInIntegerArray }

PROCEDURE InsertEntryInWorkingTimetable(VAR WorkingTimetableRecArray : WorkingTimetableRecArrayType; Position : Integer);
{ Adds a blank entry to the working timetable }
VAR
  I : Integer;
  NewEntry : WorkingTimetableRecType;

BEGIN
  SetLength(WorkingTimetableRecArray, Length(WorkingTimetableRecArray) + 1);
  { Move all existing elements up one }
  IF Length(WorkingTimetableRecArray) = 1 THEN
    WorkingTimetableRecArray[0] := NewEntry
  ELSE BEGIN
    FOR I := (Length(WorkingTimetableRecArray) - 1) DOWNTO (Position + 1) DO
      WorkingTimetableRecArray[I] := WorkingTimetableRecArray[I - 1];
    { and insert the new element }
    WorkingTimetableRecArray[Position] := NewEntry;
  END;
END; { InsertEntryInWorkingTimetable }

PROCEDURE RemoveDuplicateElementsFromStringArrayMainProcedure(VAR StringArray : StringArrayType);
{ Removes duplicate elements - the main procedure }
VAR
  I, J : Integer;
  TestElement1, TestElement2 : String;
  DuplicateElements : StringArrayType;
  SaveStringArray : StringArrayType;

BEGIN
  { note: copy is used here without parameters to copy the whole of a dynamic array }
  SaveStringArray := Copy(StringArray);
  SetLength(DuplicateElements, 0);

  { We do not want it if it's there already }
  I := 0;
  WHILE I < High(StringArray) DO BEGIN
    J := High(StringArray);
    WHILE J > 0 DO BEGIN
      IF I <> J THEN BEGIN
        TestElement1 := StringArray[I];
        TestElement2 := StringArray[J];

        { If it's a point, change FP=, TP= and XP== to P= or the comparison doesn't work properly }
        IF Copy(TestElement1, 1, 3) = 'FP=' THEN
          TestElement1 := Copy(TestElement1, 2, 255);
        IF Copy(TestElement1, 1, 3) = 'TP=' THEN
          TestElement1 := Copy(TestElement1, 2, 255);
        IF Copy(TestElement1, 1, 3) = 'XP=' THEN
          TestElement1 := Copy(TestElement1, 2, 255);

        IF Copy(TestElement2, 1, 3) = 'FP=' THEN
          TestElement2 := Copy(TestElement2, 2, 255);
        IF Copy(TestElement2, 1, 3) = 'TP=' THEN
          TestElement2 := Copy(TestElement2, 2, 255);
        IF Copy(TestElement2, 1, 3) = 'XP=' THEN
          TestElement2 := Copy(TestElement2, 2, 255);

        { Remove the suffix if it's a point }
        IF (Copy(TestElement1, Length(TestElement1), 1) = '/')
        OR (Copy(TestElement1, Length(TestElement1), 1) = '-')
        THEN
          TestElement1 := Copy(TestElement1, 1, Length(TestElement1) - 1);

        { Remove the suffix if it's a point }
        IF (Copy(TestElement2, Length(TestElement2), 1) = '/')
        OR (Copy(TestElement2, Length(TestElement2), 1) = '-')
        THEN
          TestElement2 := Copy(TestElement2, 1, Length(TestElement2) - 1);

        IF TestElement1 = TestElement2 THEN BEGIN
          { Only remove the second (and subsequent) occurrences }
          AppendToStringArray(DuplicateElements, TestElement1);
          DeleteElementFromStringArray(StringArray, J);
        END;
      END;
      Dec(J);
    END; {WHILE}
    Inc(I);
  END; {WHILE}

  IF InDebuggingMode AND (Length(DuplicateElements) > 0) THEN BEGIN
    Log('X Have removed the following duplicate elements');
    WriteStringArrayToLog(UnknownLocoChipStr, 'X', DuplicateElements);
    Log('X from the following string array');
    WriteStringArrayToLog(UnknownLocoChipStr, 'X', SaveStringArray);
    Log('X so it is now:');
    WriteStringArrayToLog(UnknownLocoChipStr, 'X', StringArray);
  END;
END; { RemoveDuplicateElementsFromStringArrayMainProcedure }

PROCEDURE RemoveDuplicateElementsFromIntegerArray(VAR IntegerArray : IntegerArrayType);
{ Removes duplicate elements }
VAR
  I, J : Integer;
  TestElement1, TestElement2 : Integer;
  DuplicateElements : IntegerArrayType;
  SaveIntegerArray : IntegerArrayType;

BEGIN
  { note: copy is used here without parameters to copy the whole of a dynamic array }
  SaveIntegerArray := Copy(IntegerArray);
  SetLength(DuplicateElements, 0);

  { We do not want it if it's there already }
  I := 0;
  WHILE I < High(IntegerArray) DO BEGIN
    J := High(IntegerArray);
    WHILE J > 0 DO BEGIN
      IF I <> J THEN BEGIN
        TestElement1 := IntegerArray[I];
        TestElement2 := IntegerArray[J];

        IF TestElement1 = TestElement2 THEN BEGIN
          { Only remove the second (and subsequent) occurrences }
          AppendToIntegerArray(DuplicateElements, TestElement1);
          DeleteElementFromIntegerArray(IntegerArray, J);
        END;
      END;
      Dec(J);
    END; {WHILE}
    Inc(I);
  END; {WHILE}

  IF InDebuggingMode AND (Length(DuplicateElements) > 0) THEN BEGIN
    Debug('Have removed the following duplicate elements');
    Debug(DescribeIntegerArray(DuplicateElements));
    Debug('from the following Integer array');
    Debug(DescribeIntegerArray(SaveIntegerArray));
    Debug('so it is now:');
    Debug(DescribeIntegerArray(IntegerArray));
  END;
END; { RemoveDuplicateElementsFromIntegerArray }

PROCEDURE RemoveDuplicateElementsFromStringArray{1}(VAR StringArray : StringArrayType); Overload
{ Removes duplicate elements }
BEGIN
  RemoveDuplicateElementsFromStringArrayMainProcedure(StringArray);
END; { RemoveDuplicateElementsFromStringArray-1 }

PROCEDURE RemoveDuplicateElementsFromStringArray{2}(VAR StringArray : StringArrayType; SubRouteCount : Integer); Overload
{ Removes duplicate elements - if SubRouteCount <> 0 do it for individual subroutes, not the whole array }
VAR
  I : Integer;
  NewStringArray : StringArrayType;
  SubRouteArray : StringArrayType;

BEGIN
  { Work through the supplied array, extracting and fixing each subroute in turn }
  FOR I := 1 TO SubRouteCount DO BEGIN
    SubRouteArray := ExtractSubRouteFromStringArray(StringArray, I);
    RemoveDuplicateElementsFromStringArray(SubRouteArray);
    AppendStringArray2ToStringArray1(NewStringArray, SubRouteArray);
  END;
  StringArray := NewStringArray;
END; { RemoveDuplicateElementsFromStringArray-2 }

FUNCTION DateTimeToMilliSeconds(DateTime: TDateTime) : Int64;
{ Converts a TDateTime variable to Int64 milliseconds from 0001-01-01 }
VAR
  TimeStamp : SysUtils.TTimeStamp;

BEGIN
  { Call DateTimeToTimeStamp to convert DateTime to TimeStamp }
  TimeStamp := SysUtils.DateTimeToTimeStamp(DateTime);

  { Multiply and add to complete the conversion }
  Result := Int64(TimeStamp.Date) * MSecsPerDay + TimeStamp.Time;
END; { DateTimeToMilliSeconds }

FUNCTION MilliSecondsToDateTime(MilliSeconds: Int64) : TDateTime;
{ Converts an Int64 milliseconds from 0001-01-01 to TDateTime variable }
VAR
  TimeStamp : SysUtils.TTimeStamp;

BEGIN
  { Divide and mod the milliseconds into the TimeStamp record }
  TimeStamp.Date := MilliSeconds DIV MSecsPerDay;
  TimeStamp.Time := MilliSeconds MOD MSecsPerDay;

  { Call TimeStampToDateTime to complete the conversion }
  Result := SysUtils.TimeStampToDateTime(TimeStamp);
END; { MilliSecondsToDateTime }

FUNCTION NewMinutesBetween(CONST AThen, ANow: TDateTime) : Int64;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
VAR
  nnNow, nnThen: Int64;

  FUNCTION TruncDateTimeToMinutes(DateTime: TDateTime) : LongInt;
  { Truncates a DateTime to minutes from 0001-01-01 }
  VAR
    ms: Int64;

  BEGIN
    ms := DateTimeToMilliSeconds(DateTime);
    Result := ms div (60*1000);
  END; { TruncDateTimeToMinutes }

BEGIN
  { Substitutes DateTimeToTimeStamp and TimeStampToMilliSeconds, TruncDateTimeToMinutes for use of Trunc function. }
  nnNow := TruncDateTimeToMinutes(ANow);
  nnThen := TruncDateTimeToMinutes(AThen);
  Result := nnNow - nnThen;
END; { NewMinutesBetween }

FUNCTION NewSecondsBetween(CONST AThen, ANow: TDateTime): Integer;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
VAR
  nnNow, nnThen: Int64;

  FUNCTION TruncDateTimeToSeconds(DateTime: TDateTime): Int64;
  { Truncates a DateTime to seconds from 0001-01-01 }
  VAR
    ms: Int64;

  BEGIN
    ms := DateTimeToMilliSeconds(DateTime);
    Result := ms div 1000;
  END; { TruncDateTimeToSeconds }

BEGIN
  { Substitutes DateTimeToTimeStamp and TimeStampToMilliSeconds, TruncDateTimeToMinutes for use of Trunc function. }
  nnNow := TruncDateTimeToSeconds(ANow);
  nnThen := TruncDateTimeToSeconds(AThen);
  Result := nnNow - nnThen;
END; { NewSecondsBetween }

FUNCTION NewMilliSecondsBetween(CONST AThen, ANow : TDateTime) : Int64;
{ A replacement for the system routine which is buggy - from http://qc.codegear.com/wc/qcmain.aspx?d=56957 and
  http://objectmix.com/delphi/401781-proposed-fix-dateutils-date-time-compare-functions.html
}
VAR
  nnNow, nnThen: Int64;

  FUNCTION TruncDateTimeToMilliSeconds(DateTime: TDateTime) : Int64;
  { Truncates a DateTime to seconds from 0001-01-01 }
  BEGIN
    Result := DateTimeToMilliSeconds(DateTime);
  END; { TruncDateTimeToSeconds }

BEGIN
  { Substitutes DateTimeToTimeStamp and TimeStampToMilliSeconds, TruncDateTimeToMinutes for use of Trunc function }
  nnNow := TruncDateTimeToMilliSeconds(ANow);
  nnThen := TruncDateTimeToMilliSeconds(AThen);
  Result := nnNow - nnThen;
END; { NewMilliSecondsBetween }

FUNCTION PointInPolygon(CONST Polygon: ARRAY OF TPoint; Point: TPoint) : Boolean;
{ Returns true if a point is in a defined region }
VAR
  Rgn : HRGN;

BEGIN
  Rgn := CreatePolygonRgn(Polygon[0], Length(Polygon), WINDING);
  Result := PtInRegion(Rgn, Point.X, Point.Y);
  DeleteObject(Rgn);
END; { PointInPolygon }

FUNCTION DescribePolygon(CONST Polygon: ARRAY OF TPoint) : String;
{ Returns the points comprising a polygon as a string }
VAR
  I : Integer;

BEGIN
  Result := 'X=' + IntToStr(Polygon[0].X) + ' Y=' + IntToStr(Polygon[0].Y);

  FOR I := 1 TO High(Polygon) DO
    Result := Result + ', X=' + IntToStr(Polygon[I].X) + ' Y=' + IntToStr(Polygon[I].Y);
END; { DescribePolygon }

FUNCTION PointIsCatchPoint(P : Integer) : Boolean;
{ Returns whether a given point is a catch point }
BEGIN
  IF (Points[P].Point_Type = CatchPointUp) OR (Points[P].Point_Type = CatchPointDown) THEN
    Result := True
  ELSE
    Result := False;
END; { PointIsCatchPoint }

FUNCTION PointStateToStr(PointState : PointStateType) : String;
{ Return the point state as a string }
BEGIN
  IF PointState = Straight THEN
    Result := StraightStr
  ELSE
    IF PointState = Diverging THEN
      Result := DivergingStr
    ELSE
      IF PointState = PointStateUnknown THEN
        Result := UnknownStr
      ELSE
        IF PointState = PointOutOfAction THEN
          Result := OutOfActionStr;
END; { PointStateToStr }

FUNCTION PointTypeToStr(PType : TypeOfPoint) : String;
{ Return the point type }
BEGIN
  CASE PType OF
    OrdinaryPoint:
      Result := OrdinaryPointStr;
    CrossOverPoint:
      Result := CrossOverPointStr;
    ThreeWayPointA:
      Result := ThreeWayPointAStr;
    ThreeWayPointB:
      Result := ThreeWayPointBStr;
    SingleSlip:
      Result := SingleSlipStr;
    DoubleSlip:
      Result := DoubleSlipStr;
    ProtectedPoint:
      Result := ProtectedPointStr;
    CatchPointUp:
      Result := CatchPointUpStr;
    CatchPointDown:
      Result := CatchPointDownStr;
  ELSE
    Result := UnknownPointTypeStr;
  END; {CASE}
END; { PointTypeToStr }

FUNCTION NextLineRouteingToStr(NextLineRouteing : NextLineRouteingType) : String;
{ Return a description of the next line routeing type }
BEGIN
  CASE NextLineRouteing OF
    EndOfLineIsNext:
      Result := 'End of line is next';
    LineIsNext:
      Result := 'Line is next';
    PointIsNext:
      Result := 'Point is next';
    UnknownNextLineRouteingType:
      Result := 'Unknown next line routeing type';
  ELSE
    Result := 'Unknown next line routeing type';
  END; {CASE}
END; { NextLineRouteingToStr }

FUNCTION GradientToStr(Gradient : GradientType) : String;
{ Return the gradient status of the line as a string }
BEGIN
  CASE Gradient OF
    Level:
      Result := LevelStr;
    RisingIfUp:
      Result := RisingIfUpStr;
    RisingIfDown:
      Result := RisingIfDownStr;
  ELSE
    Result := 'Error in DescribeGradientType routine: invalid gradient type';
  END; {CASE}
END; { GradientToStr }

FUNCTION FeedbackUnitTypeToStr(FeedbackType : TypeOfFeedbackDetector) : String;
{ Convert a feedback unit type to a string }
BEGIN
  CASE FeedbackType OF
    TrackCircuitFeedbackDetector:
      Result := TrackCircuitFeedbackDetectorStr;
    TRSPlungerFeedbackDetector:
      Result := TRSPlungerFeedbackDetectorStr;
    PointFeedbackDetector:
      Result := PointFeedbackDetectorStr;
    LineFeedbackDetector:
      Result := LineFeedbackDetectorStr;
    MixedFeedbackDetectors:
      Result := MixedFeedbackDetectorStr;
    FeedbackDetectorOutOfUse:
      Result := FeedbackDetectorOutOfUseStr;
  ELSE
    Result := UnknownFeedbackDetectorStr;
  END; {CASE}
END; { FeedbackUnitTypeToStr }

FUNCTION NextButton{1}(CONST Dlg: TForm; VAR Context: Integer): TButton;
{ The NextButton can be used to traverse the buttons in a Message Dialogue that has been created with the Dialogs.CreateMessageDialogue function. Initialize the Context
  variable to 0 to get the first button. The routine updates the Context variable to cause the next call to NextButton to return the next button. If there are no (more)
  buttons, the function returns NIL [from uDialogsExt from Borland website]
}
VAR
  Comp: TControl;

BEGIN
  WHILE Context < Dlg.ControlCount DO BEGIN
    Comp := Dlg.Controls[Context];
    Inc(Context);
    IF Comp IS TButton THEN BEGIN
      Result := TButton(Comp);
      Exit;
    END;
  END; {WHILE}
  Result := NIL;
END; { NextButton-1 }

FUNCTION NextButton{2}(CONST Dlg: TForm; VAR Context: Integer; OUT Button: TButton): Boolean;
{ This variant of NextButton returns the button in the Button variable. The function result indicates whether a button has been found [from uDialogsExt from Borland
  website]
}
BEGIN
  Button := NextButton(Dlg, Context);
  Result := Assigned(Button);
END; { NextButton-2 }

FUNCTION FindButton(CONST Dlg: TForm; CONST ModalResult: Integer): TButton;
VAR
  I: Integer;

BEGIN
  I := 0;
  WHILE NextButton(Dlg, I, Result) DO
    IF Result.ModalResult = ModalResult THEN
      Exit;
END; { FindButton }

PROCEDURE OutOfActionPointDialogue(DialogueText : String; VAR OK : Boolean);
{ Adapted from the Borland Delphi web site example - uses their uDialogsExt Unit }
VAR
  Dialog : TForm;

BEGIN
  Dialog := CreateMessageDialog(DialogueText, mtInformation, [mbYes, mbCancel]);
  TRY
    SetDefaultButton(Dialog, mrCancel);

    { change the button text }
    FindButton(Dialog, mrYes).Caption := 'Point set';

    { show the dialog }
    IF Dialog.ShowModal = mrCancel THEN
      OK := False
    ELSE
      OK := True;
  FINALLY
    Dialog.Free;
  END;
END; { OutOfActionPointDialogue }

FUNCTION MessageDialogueWithDefault{1}(DialogueText: String; StopTimer : Boolean; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons; DefaultButton : TMsgDlgBtn) : Word;
                                       Overload;
{ Adapted from the Borland Delphi web site example - uses procedures from their uDialogsExt Unit. This version has a default button. }
VAR
  Dialogue : TForm;
  DefaultButtonModalResult : TModalResult;
  DebugStr : String;

BEGIN
  IF StopTimer THEN
    StopSystemTimer;

  Dialogue := CreateMessageDialog(DialogueText, DlgType, Buttons);

  DefaultButtonModalResult := mrAbort;

  { Set up the default button }
  CASE DefaultButton OF
    mbYes:
      DefaultButtonModalResult := mrYes;
    mbNo:
      DefaultButtonModalResult := mrNo;
    mbOK:
      DefaultButtonModalResult := mrOK;
    mbCancel:
      DefaultButtonModalResult := mrCancel;
    mbAbort:
      DefaultButtonModalResult := mrAbort;
    mbRetry:
      DefaultButtonModalResult := mrRetry;
    mbIgnore:
      DefaultButtonModalResult := mrIgnore;
    mbAll:
      DefaultButtonModalResult := mrAll;
    mbNoToAll:
      DefaultButtonModalResult := mrNoToAll;
    mbYesToAll:
      DefaultButtonModalResult := mrYesToAll;
    mbHelp:
      BEGIN
        { to be implemented ***
        DefaultButtonModalResult := mrHelp;}
      END;
  END; {CASE}
  SetDefaultButton(Dialogue, DefaultButtonModalResult);

  { Log the text of the dialogue, converting CRLFs before we write out the string }
  DebugStr := 'MessageDialogueWithDefault: "' + DialogueText + '"';
  Log('A MessageDialogueWithDefault: "' + DebugStr + '"' + '{INDENT=0} {WRAP=SCREENWIDTH}');

  { show the dialogue and obtain the result }
  Result := Dialogue.ShowModal;

  DebugStr := 'MessageDialogueWithDefault: user pressed the ';
  CASE Dialogue.ModalResult OF
    idOK:
      DebugStr := DebugStr + 'OK button';
    idCancel:
      DebugStr := DebugStr + 'Cancel button';
    idAbort:
      DebugStr := DebugStr + 'Abort button';
    idRetry:
      DebugStr := DebugStr + 'Retry button';
    idIgnore:
      DebugStr := DebugStr + 'Ignore button';
    idYes:
      DebugStr := DebugStr + 'Yes button';
    idNo:
      DebugStr := DebugStr + 'No button';
    mrNo + 1:
      DebugStr := DebugStr + 'All button';
    mrAll + 1:
      DebugStr := DebugStr + 'No to All button';
    mrNoToAll + 1:
      DebugStr := DebugStr + 'Yes to All button';
  END; {CASE}
  Log('A ' + DebugStr + ' [' + IntToStr(Dialogue.ModalResult) + ']');
  Dialogue.Free;

  IF StopTimer THEN
    StartSystemTimer;

  { and redraw the screen, or we are left with a dialogue-sized hole }
  InvalidateScreen(UnitRef, 'MessageDialogueWithDefault');
END; { MessageDialogueWithDefault-1 }

FUNCTION MessageDialogueWithDefault{2}(DialogueText: String; StopTimer : Boolean; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons; ButtonText : ARRAY OF String;
                                       DefaultButton : TMsgDlgBtn) : Word; Overload;
{ Adapted from the Borland Delphi web site example - uses procedures from their uDialogsExt Unit. This version has replacement button text as well as a default button }
VAR
  ButtonCount : Integer;
  Dialogue : TForm;
  DefaultButtonModalResult : TModalResult;
  DebugStr : String;
  TestButtonElement : TMsgDlgBtn;

BEGIN
  IF StopTimer THEN
    StopSystemTimer;

  Dialogue := CreateMessageDialog(DialogueText, DlgType, Buttons);

  { Check that each button has button text supplied, and add it }
  ButtonCount := 0;
  FOR TestButtonElement := Low(TMsgDlgBtn) TO High(TMsgDlgBtn) DO BEGIN
    IF TestButtonElement IN Buttons THEN BEGIN
      CASE TestButtonElement OF
        mbYes:
          FindButton(Dialogue, mrYes).Caption := ButtonText[ButtonCount];
        mbNo:
          FindButton(Dialogue, mrNo).Caption := ButtonText[ButtonCount];
        mbOK:
          FindButton(Dialogue, mrOK).Caption := ButtonText[ButtonCount];
        mbCancel:
          FindButton(Dialogue, mrCancel).Caption := ButtonText[ButtonCount];
        mbAbort:
          FindButton(Dialogue, mrAbort).Caption := ButtonText[ButtonCount];
        mbRetry:
          FindButton(Dialogue, mrRetry).Caption := ButtonText[ButtonCount];
        mbIgnore:
          FindButton(Dialogue, mrIgnore).Caption := ButtonText[ButtonCount];
        mbAll:
          FindButton(Dialogue, mrAll).Caption := ButtonText[ButtonCount];
        mbNoToAll:
          FindButton(Dialogue, mrNoToAll).Caption := ButtonText[ButtonCount];
        mbYesToAll:
          FindButton(Dialogue, mrYesToAll).Caption := ButtonText[ButtonCount];
      END; {CASE}

      Inc(ButtonCount);
    END;
  END; {FOR}

  IF ButtonCount <> Length(ButtonText) THEN
    Log('A! ' + IntToStr(ButtonCount) + ' buttons are declared but there is text for ' + IntToStr(Length(ButtonText)) + ' buttons');

  DefaultButtonModalResult := mrAbort;

  { Set up the default button }
  CASE DefaultButton OF
    mbYes:
      DefaultButtonModalResult := mrYes;
    mbNo:
      DefaultButtonModalResult := mrNo;
    mbOK:
      DefaultButtonModalResult := mrOK;
    mbCancel:
      DefaultButtonModalResult := mrCancel;
    mbAbort:
      DefaultButtonModalResult := mrAbort;
    mbRetry:
      DefaultButtonModalResult := mrRetry;
    mbIgnore:
      DefaultButtonModalResult := mrIgnore;
    mbAll:
      DefaultButtonModalResult := mrAll;
    mbNoToAll:
      DefaultButtonModalResult := mrNoToAll;
    mbYesToAll:
      DefaultButtonModalResult := mrYesToAll;
    mbHelp:
      BEGIN
        { to be implemented ***
        DefaultButtonModalResult := mrHelp;}
      END;
  END; {CASE}
  SetDefaultButton(Dialogue, DefaultButtonModalResult);

  { Log the text of the dialogue, converting CRLFs before we write out the string }
  Log('A MessageDialogueWithDefault: "' + DialogueText + '"' + '{INDENT=0} {WRAP=SCREENWIDTH}');

  { show the dialogue and obtain the result }
  Result := Dialogue.ShowModal;

  DebugStr := 'MessageDialogueWithDefault: user pressed the ';
  CASE Dialogue.ModalResult OF
    idOK:
      DebugStr := DebugStr + 'OK button';
    idCancel:
      DebugStr := DebugStr + 'Cancel button';
    idAbort:
      DebugStr := DebugStr + 'Abort button';
    idRetry:
      DebugStr := DebugStr + 'Retry button';
    idIgnore:
      DebugStr := DebugStr + 'Ignore button';
    idYes:
      DebugStr := DebugStr + 'Yes button';
    idNo:
      DebugStr := DebugStr + 'No button';
    mrNo + 1:
      DebugStr := DebugStr + 'All button';
    mrAll + 1:
      DebugStr := DebugStr + 'No to All button';
    mrNoToAll + 1:
      DebugStr := DebugStr + 'Yes to All button';
  END; {CASE}
  Log('A ' + DebugStr + ' [' + IntToStr(Dialogue.ModalResult) + ']');
  Dialogue.Free;

  IF StopTimer THEN
    StartSystemTimer;

  { and redraw the screen, or we are left with a dialogue-sized hole }
  InvalidateScreen(UnitRef, 'MessageDialogueWithDefault');
END; { MessageDialogueWithDefault-2 }

FUNCTION PenStyleToStr(PenStyle : TPenStyle) : String;
{ Describes pen styles }
BEGIN
  CASE PenStyle OF
    psSolid:
      Result := 'Solid';
    psDash:
      Result := 'Dash';
    psDot:
      Result := 'Dot';
    psDashDot:
      Result := 'DashDot';
    psDashDotDot:
      Result := 'DashDotDot';
    psClear:
      Result := 'Clear';
    psInsideFrame:
      Result := 'InsideFrame';
  ELSE {CASE}
    Result := 'Unknown pen style';
  END; {CASE}
END; { PenStyleToStr }

FUNCTION RoundTimeToNextWholeMinute(Time : TDateTime) : TDateTime;
{ Round the time to the next whole minute, i.e. 06:30:00 becomes 06:31:00, but 06:30:20 becomes 06:32:00 }
VAR
  Hour, Min, Sec, mSec: Word;

BEGIN
  DecodeTime(Time, Hour, Min, Sec, mSec);
  IF Sec = 0 THEN BEGIN
    Time := IncMinute(Time, 1);
    DecodeTime(Time, Hour, Min, Sec, mSec);
    mSec := 0;
  END ELSE BEGIN
    Time := IncMinute(Time, 2);
    DecodeTime(Time, Hour, Min, Sec, mSec);
    Sec := 0;
    mSec := 0;
  END;

  Result := EncodeTime(Hour, Min, Sec, mSec);
END; { RoundTimeToNextWholeMinute }

PROCEDURE StopOrResumeAllOperations(Str : String);
{ Deal with emergency situations by stopping operations or restarting them }
VAR
  OK : Boolean;
  P : Integer;
  S : Integer;

BEGIN
  IF NOT SystemOnline THEN
    Debug('Cannot stop or resume operations - system offline')
  ELSE BEGIN
    IF OperationsStopped THEN BEGIN
      IF MessageDialogueWithDefault('Resume operations?', NOT StopTimer, mtConfirmation, [mbOK, mbAbort], mbAbort) = mrOK THEN BEGIN
        Log('A ' + Str + ' pressed : requesting resume all operations');
        ResumeOperations(OK);
        IF OK THEN BEGIN
          Log('AG Operations resumed');
          OperationsStopped := False;
        END ELSE
          Log('A! Operations not resumed');

        InvalidateScreen(UnitRef, 'StopOrResumeAllOperations');
      END;
    END ELSE BEGIN
      Log('A ' + Str + ' pressed : requesting stop all operations');
      StopOperations(OK);
      IF OK THEN BEGIN
        Log('A! All operations stopped');
        OperationsStopped := True;
      END;

      FOR P := 0 TO High(Points) DO
        EmergencyDeselectPoint(P, OK);
      Log('P! User has switched all points off');

      FOR S := 0 TO High(Signals) DO
        EmergencyDeselectSignal(S, OK);
      Log('S! User has switched all signals off');
    END;
  END;
END; { StopOrResumeAllOperations }

PROCEDURE ChangeTrainStatus(T : TrainIndex; NewStatus : TrainStatusType);
 { Change the current train status and record it }
VAR
  DebugStr : String;
  OK : Boolean;

BEGIN
  DebugStr := '';

  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('ChangeTrainStatus')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      { we need to make special provision for missing trains, as their status can switch back and fore continuously from Missing to MissingAndSuspended }
      IF (Train_CurrentStatus = Missing) OR (Train_CurrentStatus = MissingAndSuspended) THEN
        DebugStr := ' (immediate previous status was ' + TrainStatusToStr(Train_CurrentStatus) + ')'
      ELSE
        Train_PreviousStatus := Train_CurrentStatus;

      Train_CurrentStatus := NewStatus;
      Log(Train_LocoChipStr + ' L New status: ' + TrainStatusToStr(Train_CurrentStatus)
                            + '; previous status: ' + TrainStatusToStr(Train_PreviousStatus) + DebugStr);
      { update the diagrams window }
      DrawDiagramsStatusCell(T, Normalstyle);

      { And make any necessary changes to the train record, etc. }
      CASE Train_CurrentStatus OF
        ToBeRemovedFromDiagrams:
          BEGIN
            { Take it off the diagram grid }
            RemoveTrainFromDiagrams(T);
            ChangeTrainStatus(T, RemovedFromDiagrams);
            DrawDiagrams(UnitRef, 'Change Train Status');

            { Deal with the loco's lights (if any) }
            IF NOT Train_LightsRemainOnWhenJourneysComplete THEN
              TurnTrainLightsOff(T, OK);

            IF TrainHasCablights(T) AND Train_CabLightsAreOn THEN
              TurnTrainCabLightsOff(T, OK);
          END;
      END; {CASE}
    END; {WITH}
  END;
END; { ChangeTrainStatus }

FUNCTION DescribeTrainList{1} : String; Overload;
{ Describe the contents of the train list, the full list of locos and trains if DescribeFullTrainList is set }
VAR
  T : TrainIndex;

BEGIN
  Result := '';

  T := 0;
  WHILE T <= High(Trains) DO BEGIN
    WITH Trains[T] DO BEGIN
      Result := Result + IntToStr(Train_LocoChip) + ', ' ;
      Inc(T);
    END; {WITH}
  END; {WHILE}
END; { DescribeTrainList-1 }

FUNCTION DescribeTrainList{2}(Sort : SortOrder; DescribeFullTrainList : Boolean) : String; Overload;
{ Describe the contents of the train list, the full list of locos and trains if DescribeFullTrainList is set }
VAR
  T : TrainIndex;

BEGIN
  Result := '';

  T := 0;
  WHILE T <= High(Trains) DO BEGIN
    WITH Trains[T] DO BEGIN
      IF DescribeFullTrainList OR Train_DiagramFound THEN BEGIN
        CASE Sort OF
          Unsorted, LocoChipSort, ReverseLocoChipSort:
            Result := Result + IntToStr(Train_LocoChip) + ', ' ;
          TrainTypeSort:
            Result := Result + IntToStr(Train_LocoChip) + ' ' + IntToStr(Train_TypeNum) + ', ';
          LocoChipAndDepartureTimeSort:
            BEGIN
              IF Length(Train_JourneysArray) = 0 THEN
                Result := Result + IntToStr(Train_LocoChip) + ', '
              ELSE
                Result := Result + IntToStr(Train_LocoChip) + ' ' + TimeToHMStr(Train_JourneysArray[0].TrainJourney_CurrentDepartureTime) + ', ';
            END;
          DepartureTimeAndTrainTypeSort:
            BEGIN
              IF Length(Train_JourneysArray) = 0 THEN
                Result := Result + IntToStr(Train_TypeNum) + ', '
              ELSE
                Result := Result + TimeToHMStr(Train_JourneysArray[0].TrainJourney_CurrentDepartureTime) + ' ' + IntToStr(Train_TypeNum) + ', ';
            END;
        ELSE {CASE}
          Result := 'Unknown sort type';
        END; {CASE}
      END;
      Inc(T);
    END; {WITH}
  END; {WHILE}

  IF Result <> '' THEN BEGIN
    CASE Sort OF
      Unsorted:
        Result := 'TrainList unsorted = ' + Result;
      LocoChipSort:
        Result := 'TrainList sorted in LocoChip order = ' + Result;
      ReverseLocoChipSort:
        Result := 'TrainList reverse-sorted in LocoChip order = ' + Result;
      TrainTypeSort:
        Result := 'TrainList sorted in TrainType order = ' + Result;
      LocoChipAndDepartureTimeSort:
        Result := 'TrainList sorted in LocoChip and Departure Time order = ' + Result;
      DepartureTimeAndTrainTypeSort:
        Result := 'TrainList sorted in Departure Time and Train Type order = ' + Result;
    ELSE {CASE}
      Result := 'Unknown sort type';
    END; {CASE}
  END;
END; { DescribeTrainList-2 }

FUNCTION ListLocoChipsInIntegerArray(IntegerArray : IntegerArrayType) : String;
{ Lists loco chips from an integer array }
VAR
  I : Integer;

BEGIN
  Result := '';

  IF Length(IntegerArray) > 0 THEN BEGIN
    FOR I := 0 TO High(IntegerArray) - 1 DO
      Result := Result + LocoChipToStr(IntegerArray[I]) + ', ';
    Result := Result + LocoChipToStr(IntegerArray[High(IntegerArray)]);
  END;
END; { ListLocoChipsInIntegerArray }

PROCEDURE Pause(MilliSeconds : Cardinal; ProcessMessages : Boolean);
{ Pause for a given number of milliseconds }
VAR
  Start : Cardinal;

BEGIN
  Start := GetTickCount;

  { Debug('Pausing...'); }
  WHILE GetTickCount - Start < MilliSeconds DO BEGIN
    IF ProcessMessages THEN
      Application.ProcessMessages;

    IF InAutoMode THEN BEGIN
      DoCheckForUnexpectedData(UnitRef, 'Pause');
      MoveAllTrains;
    END;
  END;
END; { Pause }

PROCEDURE ReadInDataFromExcelFile;
{ Read in data from the Excel data file - works but is not needed - kept as an example of reading from Excel files }
//VAR
//  FieldCount : Integer;
//  I : Integer;
//  LocationDataOK : Boolean;
//  TableList : TStrings;

BEGIN
//  TRY
//    WITH LocationDataWindow DO BEGIN
//      TableList := TStringList.Create;
//      LocationDataOK := True;
//
//      LocationDataADOConnection.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source='
//                                                    + PathToRailDataFiles + LocationDataFilename + '.' + LocationDataFilenameSuffix
//                                                    + ';Extended Properties=Excel 8.0;';
//                                                 // + ';Extended Properties="Excel 8.0;IMEX=1"';
//
//      LocationDataADOConnection.Connected := True;
//
//      LocationDataADOConnection.GetTableNames(TableList, False);
//      LocationDataADOTable.TableName := '[Locations$]';
//
//      for I := 0 to (TableList.Count - 1) do begin
//        debug(Tablelist[I]);
//      end;
//
//
//      Log('D LocationData table and connection opened to initialise the location data');
//
//  //    LocationDataADOTable.Sort := '[LocoChip ASC], [DepartureTime0] ASC';
//
//      LocationDataADOTable.Open;
//      LocationDataADOTable.First;
//
//      Debug('fieldcount=' + IntToStr(LocationDataADOTable.FieldCount));
//      FieldCount := LocationDataADOTable.FieldCount;
//
//      WHILE LocationDataOK AND NOT LocationDataADOTable.EOF DO BEGIN
//        Debug('0: ' + LocationDataADOTable.Fields[0].asString);
//        FOR I := 0 TO FieldCount - 1 DO
//          Debug(IntToStr(I) + ': ' + LocationDataADOTable.Fields[I].asString);
//
//
//        LocationDataADOTable.Next;
//      END; {WHILE}
//
//      { Tidy up the database }
//      TableList.Free;
//      LocationDataADOTable.Close;
//      LocationDataADOConnection.Connected := False;
//      Log('D LocationData table and connection closed');
//    END; {WITH}
//  EXCEPT {TRY}
//    ON E : Exception DO BEGIN
//      Debug('LoadLocationData: ' + E.ClassName + ' error raised, with message: '+ E.Message);
//      Log(UnknownLocoChip, 'E LoadLocationData: ' + E.ClassName + ' error raised, with message: '+ E.Message);
//    END;
//  END; {TRY}
END; { ReadInDataFromExcelFile }

FUNCTION ReturnFixedLengthStr(Str : String; FixedLength : Integer) : String;
{ Return a short string of a fixed length }
BEGIN
  Result := Str + StringOfChar(' ', 4 - Length(Str))
END; { ReturnFixedLengthStr }

PROCEDURE ReturnTrainFromMissing(T : TrainIndex);
{ Set a train as being no longer missing }
VAR
  TC : Integer;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('ReturnTrainFromMissing')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF Train_CurrentStatus = MissingAndSuspended THEN
        ChangeTrainStatus(T, Suspended)
      ELSE
        IF Train_CurrentStatus = Missing THEN
          ChangeTrainStatus(T, Train_PreviousStatus);

      Train_MissingMessage := False;
      Train_LastMissingTC := Train_CurrentTC;
      Dec(MissingTrainCounter);

      FOR TC := 0 TO High(TrackCircuits) DO BEGIN
        IF (TrackCircuits[TC].TC_LocoChip = Train_LocoChip) AND (TrackCircuits[TC].TC_OccupationState = TCMissingOccupation) THEN BEGIN
          TrackCircuits[TC].TC_MissingTrainNoted := False;
          SetTrackCircuitState(Train_LocoChip, TC, TCFeedbackOccupation);
        END;
      END;

      Log(Train_LocoChipStr + ' LG Train has been restarted');
      DrawDiagramsStatusCell(T, NormalStyle);
    END; {WITH}
  END;
END; { ReturnTrainFromMissing }

FUNCTION SameTimeInHoursAndMinutesOnly(Time1, Time2 : TDateTime) : Boolean;
{ Compares two given times and returns true if they are the same in terms of hours and minutes (the system routine SameTime compares times down to the millisecond level,
  and comparing two times by means of Time1 = Time2 also fails if there is a fraction of a difference between them).
}
VAR
  Hour1, Min1, Sec1, MSec1: Word;
  Hour2, Min2, Sec2, MSec2: Word;

BEGIN
  Result := False;

  DecodeTime(Time1, Hour1, Min1, Sec1, MSec1);
  DecodeTime(Time2, Hour2, Min2, Sec2, MSec2);

  IF Hour1 = Hour2 THEN
    IF Min1 = Min2 THEN
      Result := True;
END; { SameTimeInHoursAndMinutesOnly }

PROCEDURE SetMode(TypeOfMode : ModeType; OnOrOff : Boolean);
{ Set up one of the various modes, updating the fourth status panel }

  PROCEDURE RemoveStringFromStatusPanel(Str : String);
  VAR
    TempStr : String;

  BEGIN
    { and remove from the status panel }
    TempStr := FWPRailWindow.FWPRailWindowStatusBar.Panels[StatusBarPanel3].Text;
    TempStr := StringReplace(TempStr, Str, '', [rfIgnoreCase]);
    WriteToStatusBarPanel(StatusBarPanel3, TempStr);
  END; { RemoveStringFromStatusPanel }

BEGIN
  IF OnOrOff = TurnOn THEN BEGIN
    CASE TypeOfMode OF
      AllRouteDebugging:
        IF NOT AllRouteDebuggingMode THEN BEGIN
          SetMode(AllRouteDebugging, True);
          Log('A All Route Debugging Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' ALLROUTE');
        END;
      AnonymousOccupation:
        IF NOT AnonymousOccupationMode THEN BEGIN
          AnonymousOccupationMode := True;
          Log('A Anonymous Occupation Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' ANON');
        END;
      FeedbackDebugging:
        IF NOT FeedbackDebuggingMode THEN BEGIN
          FeedbackDebuggingMode := True;
          Log('A Feedback Debugging Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' FBCK ');
        END;
      GeneralDebugging:
        IF NOT DebuggingMode THEN BEGIN
          DebuggingMode := True;
          Log('A Debugging Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' DEBUG ');
        END;
      LineDebugging:
        IF NOT LineDebuggingMode THEN BEGIN
          LineDebuggingMode := True;
          Log('A Line Debugging Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' LINE');
        END;
      LockDebugging:
        IF NOT LockDebuggingMode THEN BEGIN
          LockDebuggingMode := True;
          Log('A Lock Debugging Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' LOCK');
        END;
      Locking:
        IF NOT LockingMode THEN BEGIN
          LockingMode := True;
          Log('AG Locking Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' LDB');
        END;
      LocoSpeedTiming:
        IF NOT LocoSpeedTimingMode THEN BEGIN
          LocoSpeedTimingMode := True;
          Log('AG Loco Speed Timing Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' LST');
        END;
      LogsCurrentlyKept:
        IF NOT LogsCurrentlyKeptMode THEN BEGIN
          LogsCurrentlyKeptMode := True;
          Log('AG Logs Currently Kept Mde = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' LCK');
        END;
      PointDebugging:
        IF NOT PointDebuggingMode THEN BEGIN
          PointDebuggingMode := True;
          Log('A Point Debugging Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' POINT');
        END;
      PreviousPointSettings:
        { This probably shouldn't really be a mode, as there would be no point in turning it off once the previous point settings are loaded, but it is a way of recording
          that the startup parameter is set and then implementing it when the points are subsequently loaded
        }
        IF NOT PreviousPointSettingsMode THEN BEGIN
          PreviousPointSettingsMode := True;
          Log('A Previous Point Settings Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' PPS');
        END;
      RDC:
        IF NOT RDCMode THEN BEGIN
          RDCMode := True;
          Log('A RDC Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' RDC');
        END;
      RecordingMonitorScreens:
        IF NOT RecordingMonitorScreensMode THEN BEGIN
          RecordingMonitorScreensMode := True;
          Log('A Recording MonitorScreens Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' RECORDING');
        END;
      RecordLineDrawing:
        IF NOT RecordLineDrawingMode THEN BEGIN
          RecordLineDrawingMode := True;
          Log('A Record Line Drawing = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' LINEDRAWING');
        END;
      RouteDebugging:
        IF NOT RouteDebuggingMode THEN BEGIN
          RouteDebuggingMode := True;
          Log('A Route Debugging Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' ROUTE');
        END;
      RouteBacktrackDebugging:
        IF NOT RouteBacktrackDebuggingMode THEN BEGIN
          RouteBacktrackDebuggingMode := True;
          Log('A RouteBacktrack Debugging Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' BACKTRACK');
        END;
      RouteDrawing:
        IF NOT RouteDrawingMode THEN BEGIN
          RouteDrawingMode := True;
          Log('A Route Drawing Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' DRAWING');
        END;
      ShowAdjacentTrackCircuit:
        IF NOT ShowAdjacentTrackCircuitMode THEN BEGIN
          ShowAdjacentTrackCircuitMode := True;
          Log('A Show Adjacent Track Circuit Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' ADJ');
        END;
      StationStart:
        IF NOT StationStartMode THEN BEGIN
          StationStartMode := True;
          Log('A Station Start Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' SS');
        END;
      Testing:
        IF NOT TestingMode THEN BEGIN
          TestingMode := True;
          Log('A Test Mode = ON');
          WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' TEST');
        END;
    ELSE {CASE}
      Log('XG Unknown mode type found');
    END; {CASE}
  END ELSE BEGIN
    CASE TypeOfMode OF
      AllRouteDebugging:
        IF AllRouteDebuggingMode THEN BEGIN
          SetMode(AllRouteDebugging, False);
          Log('A All Route Debugging Mode = OFF');
          RemoveStringFromStatusPanel('ALLROUTE');
        END;
      AnonymousOccupation:
        IF AnonymousOccupationMode THEN BEGIN
          AnonymousOccupationMode := False;
          Log('A Anonymous Occupation Mode = OFF');
          RemoveStringFromStatusPanel('ANON');
        END;
      FeedbackDebugging:
        IF FeedbackDebuggingMode THEN BEGIN
          FeedbackDebuggingMode := False;
          Log('A Feedback Debugging Mode = OFF');
          RemoveStringFromStatusPanel('FBCK');
        END;
      GeneralDebugging:
        IF DebuggingMode THEN BEGIN
          DebuggingMode := False;
          Log('A Debugging Mode = OFF');
          RemoveStringFromStatusPanel('DEBUG');
        END;
      LineDebugging:
        IF LineDebuggingMode THEN BEGIN
          LineDebuggingMode := False;
          Log('A Line Debugging Mode = OFF');
          RemoveStringFromStatusPanel('LINE');
        END;
      LockDebugging:
        IF LockDebuggingMode THEN BEGIN
          LockDebuggingMode := False;
          Log('A Lock Debugging Mode = OFF');
          RemoveStringFromStatusPanel('LDB');
        END;
      Locking:
        IF LockingMode THEN BEGIN
          LockingMode := False;
          Log('AG Locking Mode = OFF');
          RemoveStringFromStatusPanel('LOCK');
        END;
      LocoSpeedTiming:
        IF LocoSpeedTimingMode THEN BEGIN
          LocoSpeedTimingMode := False;
          Log('AG Loco Speed Timing Mode = OFF');
          RemoveStringFromStatusPanel('LST');
        END;
      LogsCurrentlyKept:
        IF LogsCurrentlyKeptMode THEN BEGIN
          LogsCurrentlyKeptMode := False;
          Log('AG Logs Currently Kept = OFF');
          RemoveStringFromStatusPanel('LCK');
        END;
      PointDebugging:
        IF PointDebuggingMode THEN BEGIN
          PointDebuggingMode := False;
          Log('A Point Debugging Mode = OFF');
          RemoveStringFromStatusPanel('POINTDEBUG');
        END;
      RDC:
        IF RDCMode THEN BEGIN
          RDCMode := False;
          Log('A RDC Mode = OFF');
          RemoveStringFromStatusPanel('RDC');
        END;
      RecordingMonitorScreens:
        IF RecordingMonitorScreensMode THEN BEGIN
          RecordingMonitorScreensMode := False;
          Log('A Recording Monitor Screens Mode = OFF');
          RemoveStringFromStatusPanel('RECORDING');
        END;
      RecordLineDrawing:
        IF RecordLineDrawingMode THEN BEGIN
          RecordLineDrawingMode := False;
          Log('A Record Line Drawing Mode = OFF');
          RemoveStringFromStatusPanel('LINEDRAWING');
        END;
      RouteBacktrackDebugging:
        IF RouteBacktrackDebuggingMode THEN BEGIN
          RouteBacktrackDebuggingMode := False;
          Log('A RouteBacktrack Debugging Mode = OFF');
          RemoveStringFromStatusPanel('BACKTRACK');
        END;
      RouteDebugging:
        IF RouteDebuggingMode THEN BEGIN
          RouteDebuggingMode := False;
          Log('A Route Debugging Mode = OFF');
          RemoveStringFromStatusPanel('ROUTE');
        END;
      RouteDrawing:
        IF RouteDrawingMode THEN BEGIN
          RouteDrawingMode := False;
          Log('A Route Drawing Mode = OFF');
          RemoveStringFromStatusPanel('DRAWING');
        END;
      ShowAdjacentTrackCircuit:
        IF ShowAdjacentTrackCircuitMode THEN BEGIN
          ShowAdjacentTrackCircuitMode := False;
          Log('A Show Adjacent Trackj Circuit Mode Mode = OFF');
          RemoveStringFromStatusPanel('ADJ');
        END;
      StationStart:
        IF StationStartMode THEN BEGIN
          StationStartMode := False;
          Log('A Station Start Mode = OFF');
          RemoveStringFromStatusPanel('SS');
        END;
      Testing:
        IF TestingMode THEN BEGIN
          TestingMode := False;
          Log('A Test Mode = OFF');
          RemoveStringFromStatusPanel('TEST');
        END;
    ELSE {CASE}
      Log('XG Unknown mode type found');
    END; {CASE}
  END;
END; { SetMode }

PROCEDURE SetFeedbackDebuggingModeOn(DebugStr : String; AdjacentSignalNumber, DecoderNumber, TCInFull, TCOnce : Boolean);
{ Turn on feedback debugging mode, updating the fourth status panel }
BEGIN
  FeedbackDebuggingMode := True;
  Debug(DebugStr);
  WriteDataToFeedbackWindow(DebugStr);
  WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' FEEDBACK');

  ReadOutAdjacentSignalNumber := AdjacentSignalNumber;
  ReadOutTCInFull := TCInFull;
  ReadOutTCOnce := TCOnce;
  ReadOutDecoderNumber := DecoderNumber;
END; { SetFeedbackDebuggingModeOn }

PROCEDURE SetFeedbackDebuggingModeOff(DebugStr : String);
{ Turn off feedback debugging mode, updating the fourth status panel }
VAR
  TempStr : String;

BEGIN
  FeedbackDebuggingMode := False;
  Debug(DebugStr);
  WriteDataToFeedbackWindow(DebugStr);

  ReadOutAdjacentSignalNumber := False;
  ReadOutTCInFull := False;
  ReadOutTCOnce := False;
  ReadOutDecoderNumber := False;

  { The timer is used to clear the on-screen message and then switches itself off }
  FeedbackWindow.FeedbackWindowTimer.Enabled := True;

  { and remove from the status panel }
  TempStr := FWPRailWindow.FWPRailWindowStatusBar.Panels[StatusBarPanel3].Text;
  TempStr := StringReplace(TempStr, 'FEEDBACK', '', [rfIgnoreCase]);
  WriteToStatusBarPanel(StatusBarPanel3, TempStr);
END; { SetFeedbackDebuggingModeOff }

FUNCTION SetDefaultButton(CONST Dlg: TForm; CONST ModalResult: Integer): Boolean;
{ Sets the button to be the Active Control on the dialogue form - so gets the focus and is the default }
VAR
  Btn: TButton;
  DefButton: TButton;
  I: Integer;

BEGIN
  DefButton := NIL;
  I := 0;
  WHILE NextButton(Dlg, I, Btn) DO BEGIN
    Btn.Default := (Btn.ModalResult = ModalResult);
    IF Btn.Default AND NOT Assigned(DefButton) THEN
      DefButton := Btn;
  END; {WHILE}
  Result := Assigned(DefButton);
  IF Result THEN
    Dlg.ActiveControl := DefButton;
END; { SetDefaultButton }

PROCEDURE SetTwoLightingChips(L : LocoIndex; LightsAtUp, LightsAtDown : DirectionType; LightsOn : Boolean);
{ When appropriate switch two lighting chips }
VAR
  DebugStr : String;
  OK : Boolean;
  UserMsg : String;

BEGIN
  IF L = UnknownLocoIndex THEN
    UnknownLocoRecordFound('SetTwoLightingChips')
  ELSE BEGIN
    WITH Locos[L] DO BEGIN
      IF NOT LightsOn THEN BEGIN
        TurnLocoLightsOff(L, NOT UserMsgRequired, UserMsg, OK);
        Log(Loco_LocoChipStr + ' L Up and down lights turned off');
      END ELSE BEGIN
        IF NOT Loco_LightsOn THEN
          TurnLocoLightsOn(L, NOT NonMovingLoco, NOT LightLoco, NOT UserMsgRequired, UserMsg, OK);

          IF Loco_LightingChipUp = Loco_LocoChip THEN BEGIN
            IF Loco_CurrentDirection <> LightsAtUp THEN
              SetLocoDirection(Locos[L], LightsAtUp, OK)
          END ELSE
            IF Loco_LightingChipUpIndex <> UnknownLocoIndex THEN BEGIN
              IF Locos[Loco_LightingChipUpIndex].Loco_CurrentDirection <> LightsAtUp THEN
                SetLocoDirection(Locos[Loco_LightingChipUpIndex], LightsAtUp, OK);
            END;

          IF Loco_LightingChipDown = Loco_LocoChip THEN BEGIN
            IF Loco_CurrentDirection <> LightsAtDown THEN
              SetLocoDirection(Locos[L], LightsAtDown, OK)
          END ELSE
            IF Loco_LightingChipDownIndex <> UnknownLocoIndex THEN
              IF Locos[Loco_LightingChipDownIndex].Loco_CurrentDirection <> LightsAtDown THEN
                SetLocoDirection(Locos[Loco_LightingChipDownIndex], LightsAtDown, OK);

        DebugStr := 'Lights at Up set to ' + IfThen(LightsAtUp = Up,
                                                    'White',
                                                    'Red') + '; '
                    + 'Lights at Down set to ' + IfThen(LightsAtDown = Down,
                                                       'White',
                                                       'Red');
        IF DebugStr <> Loco_LightsMsg THEN BEGIN
          Log(Loco_LocoChipStr + ' L ' + DebugStr);
          Loco_LightsMsg := DebugStr;
        END;
      END;
    END; {WITH}
  END;
END; { SetTwoLightingChips }

PROCEDURE ShowMenus;
{ Make all the menus visible }
BEGIN
  WITH FWPRailWindow DO BEGIN
    MenusVisible := True;

    MainClockMenu.Visible := True;
    MainDisplayMenu.Visible := True;
    MainFileMenu.Visible := True;
    MainHelpMenu.Visible := True;
    MainOperationsMenu.Visible := True;
    MainRunMenu.Visible := True;

    { this is a bit daft, as if the menus aren't visible, one is never going to see the tick, but I've no doubt that it is part of the AMS standard! }
    IF MainDisplayMenu.Visible THEN
      MainDisplayMenuShow.Checked := True;
  END; {WITH}
END; { ShowMenus }

PROCEDURE ShutDownProgram(UnitRef : String; SubroutineStr : String);
{ Shut down the program neatly }

CONST
  Init = True;
  TrainListOnly = True;
  ReadWriteRegistry = True;

VAR
  OK : Boolean;
  T : TrainIndex;
  WindowsTaskBar : HWND;

BEGIN { ShutDownProgram }
  TRY
    { Write out the locations of the locos so we know where they are when we start up next time (locations are the last known location, added when a loco moves, or is
      purged)
    }
    Log('A! Shut down initiated');
    ProgramShuttingDown := True;

    IF ScreenColoursSetForPrinting THEN
      { we need to do this or the wrong colours are saved in the registry }
      ResetScreenColoursAfterPrinting;

    { Close the station monitor web page if it exists }
    CloseStationMonitorsWebPage(OK);
    IF OK THEN
      Log('A Station Monitors web page closed');

    { Restore the Windows taskbar if we're in full screen mode and it's been disabled }
    IF WindowsTaskbarDisabled THEN BEGIN
      { Find handle of TASKBAR }
      WindowsTaskBar := FindWindow('Shell_TrayWnd', NIL);
      { Enable the taskbar }
      EnableWindow(WindowsTaskBar, True);
      { Show the taskbar }
      ShowWindow(WindowsTaskbar, SW_SHOW);

      WindowsTaskbarDisabled := False;
    END;

    IF FWPRailWindowInitialised THEN BEGIN
      WriteOutLineDataToDatabase;
      WriteOutLocationDataToDatabase;
      WriteOutPointDataToDatabase;
      WriteOutSignalDataToDatabase;

      IF SystemOnline THEN
        WriteOutLocoDataToDatabase;

      { Stop any trains that are currently moving - better than leaving them running }
      IF StopAllLocosAtShutDown THEN
        StopLocos('shutdown');

      IF NOT AllSignalsSwitchedOff THEN
        SetAllSignalsToDanger;

      IF SwitchActiveLocoLightsOffAtShutDown THEN BEGIN
        T := 0;
        WHILE T <= High(Trains) DO BEGIN
          WITH Trains[T] DO BEGIN
            IF (Train_LocoIndex <> UnknownLocoIndex) AND (TrainFoundInDiagrams(Train_LocoIndex) <> 0) THEN BEGIN
              IF Train_HasLights THEN BEGIN
                TurnTrainLightsOff(T, OK);
                IF TrainHasCabLights(T) AND Train_CabLightsAreOn THEN
                  TurnTrainCabLightsOff(T, OK);
              END;
            END;
          END; {WITH}
          Inc(T);
        END; {WHILE}
      END;

      { Write then close the log file }
      WriteToLogFileAndTestFile := True;
      IF InRDCMode AND RailDriverInitialised THEN BEGIN
        WriteToRailDriverLEDs('');
        CloseRailDriver;
      END;

      StopSystemTimer;

      { Write things back to the .ini file }
      IF NOT ReplayMode AND FWPRailWindowInitialised THEN BEGIN
        Log('A Writing .ini file');
        FWPRailWindowInitialised := True;
        WriteIniFile;
      END;
    END;

    IF LenzConnection = USBConnection THEN
      StopLANUSBServer;

    Log('A Shut down initiated in ' + UnitRef + ' unit, ' + SubroutineStr + ' subroutine' + ' is now complete (' + DescribeActualDateAndTime + ')');
    IF InLogsCurrentlyKeptMode THEN BEGIN
      CloseFile(TestLogFile);
      CloseFile(LargeLogFile);
      IF MultipleLogFilesRequired THEN BEGIN
        CloseFile(ErrorLogFile);
        CloseFile(LocoLogFile);
        CloseFile(RouteLogFile);
        CloseFile(SignalPointAndTCLogFile);
        CloseFile(DiagramsLogFile);
        CloseFile(WorkingTimetableLogFile);
      END;
    END;

    IF RestoreLogsToPreviousState THEN BEGIN
      { Erase the newly created log file (if we're only doing a replay, not running a proper sequence, or else logging is off and rename the previous ones if they exist }
      RenameLaterFiles(LargeLogFile, PathToLogFiles + LogFileName, LogFileNameSuffix);
      RenameLaterFiles(TestLogFile, PathToLogFiles + LogFileName + '-Test', LogFileNameSuffix);
      IF MultipleLogFilesRequired THEN BEGIN
        RenameLaterFiles(ErrorLogFile, PathToLogFiles + LogFileName + '-Error', LogFileNameSuffix);
        RenameLaterFiles(LocoLogFile, PathToLogFiles + LogFileName + '-Loco', LogFileNameSuffix);
        RenameLaterFiles(RouteLogFile, PathToLogFiles + LogFileName + '-Route', LogFileNameSuffix);
        RenameLaterFiles(SignalPointAndTCLogFile, PathToLogFiles + LogFileName + '-SignalPointAndTC', LogFileNameSuffix);
        RenameLaterFiles(DiagramsLogFile, PathToLogFiles + LogFileName + '-Diagrams', LogFileNameSuffix);
        RenameLaterFiles(WorkingTimetableLogFile, PathToLogFiles + LogFileName + '-WorkingTimetable', LogFileNameSuffix);
      END;
    END;

    { and stop }
    IF NOT ReplayMode THEN BEGIN
      Application.Terminate;
      IF NOT Application.Terminated THEN
        Halt(12);
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG ShutDownProgram: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { ShutDownProgram }

FUNCTION SignalAdjacentLineOK(Line : Integer) : Boolean;
{ Returns true if a signal can be created next to the current line }
VAR
  S : Integer;
  SignalAdjacentLineFound : Boolean;

BEGIN
  Result := False;

  IF Line <> UnknownLine THEN BEGIN
    { only create a signal next to a line if there isn't one already attached to it }
    S := 0;
    SignalAdjacentLineFound := False;
    WHILE (S <= High(Signals)) AND NOT SignalAdjacentLineFound DO BEGIN
      IF Signals[S].Signal_AdjacentLine = Line THEN
        SignalAdjacentLineFound := True;
      Inc(S);
    END; {WHILE}

    IF NOT SignalAdjacentLineFound THEN
      { the line has to be horizontal }
      IF Lines[Line].Line_GridUpY = Lines[Line].Line_GridDownY THEN
        Result := True;
  END;
END; { SignalAdjacentLineOK }

FUNCTION SignalHasLeftJunctionIndicator(S : Integer; OUT Indicator : JunctionIndicatorType) : Boolean;
{ Returns true if the signal has a left junction indicator }
BEGIN
  Result := False;

  WITH Signals[S] DO BEGIN
    IF Signal_Indicator = JunctionIndicator THEN BEGIN
      Indicator := UnknownJunctionIndicator;
      IF Signal_JunctionIndicators[UpperLeftIndicator].JunctionIndicator_Exists THEN
        Indicator := UpperLeftIndicator
      ELSE
        IF Signal_JunctionIndicators[MiddleLeftIndicator].JunctionIndicator_Exists THEN
          Indicator := MiddleLeftIndicator
        ELSE
          IF Signal_JunctionIndicators[LowerLeftIndicator].JunctionIndicator_Exists THEN
            Indicator := LowerLeftIndicator;

      IF Indicator <> UnknownJunctionIndicator THEN
        Result := True;
    END; {WITH}
  END;
END; { SignalHasLeftJunctionIndicator }

FUNCTION SignalHasRightJunctionIndicator(S : Integer; OUT Indicator : JunctionIndicatorType) : Boolean;
{ Returns true if the signal has a right junction indicator }
BEGIN
  Result := False;

  WITH Signals[S] DO BEGIN
    IF Signal_Indicator = JunctionIndicator THEN BEGIN
      Indicator := UnknownJunctionIndicator;
      IF Signal_JunctionIndicators[UpperRightIndicator].JunctionIndicator_Exists THEN
        Indicator := UpperRightIndicator
      ELSE
        IF Signal_JunctionIndicators[MiddleRightIndicator].JunctionIndicator_Exists THEN
          Indicator := MiddleRightIndicator
        ELSE
          IF Signal_JunctionIndicators[LowerRightIndicator].JunctionIndicator_Exists THEN
            Indicator := LowerRightIndicator;

      IF Indicator <> UnknownJunctionIndicator THEN
        Result := True;
    END; {WITH}
  END;
END; { SignalHasRightJunctionIndicator }

FUNCTION SignalQuadrantToStr(Q : QuadrantType) : String;
{ Return a string with the supplied signal quadrant details }
BEGIN
  CASE Q OF
    LowerQuadrant:
      Result := 'Lower';
    UpperQuadrant:
      Result := 'Upper';
    NoQuadrant:
      Result := 'None';
  END; {CASE}
END; { SignalQuadrantToStr }

FUNCTION SignalTypeToStr(ST : TypeOfSignal; LongOrShortString : StringType) : String;
{ Return the type of a signal in words }
BEGIN
  IF LongOrShortString = LongStringType THEN BEGIN
    CASE ST OF
      CallingOn:
        Result := CallingOnStr;
      TwoAspect:
        Result := TwoAspectStr;
      ThreeAspect:
        Result := ThreeAspectStr;
      FourAspect:
        Result := FourAspectStr;
      SemaphoreHome:
        Result := SemaphoreHomeStr;
      SemaphoreDistant:
        Result := SemaphoreDistantStr;
    END; {CASE}
  END ELSE BEGIN
    CASE ST OF
      CallingOn:
        Result := 'Calling On';
      TwoAspect:
        Result := '2 Aspect';
      ThreeAspect:
        Result := '3 Aspect';
      FourAspect:
        Result := '4 Aspect';
      SemaphoreHome:
        Result := 'Semaphore Home';
      SemaphoreDistant:
        Result := 'Semaphore Distant';
    END; {CASE}
  END;
END; { SignalTypeToStr }

{$O-}
PROCEDURE StartLocos(Restart : Boolean);
{ Restart all the locos that were running before the enforced stop }
CONST
  ForceDraw = True;
  GoingForward = True;
  LightsOn = True;

VAR
  T : TrainIndex;
  OK : Boolean;
  TrainsRestarted : Boolean;

BEGIN
  TrainsRestarted := False;
  T := 0;
  WHILE T <= High(Trains) DO BEGIN
    WITH Trains[T] DO BEGIN
      IF (Train_LocoChip <> UnknownLocoChip)
      AND Train_DiagramFound
      AND (Train_CurrentStatus <> Suspended)
      AND (Train_CurrentStatus <> MissingAndSuspended)
      AND (Train_CurrentStatus <> Cancelled)
      THEN BEGIN
        IF SystemOnline THEN BEGIN
          IF Train_CurrentDirection = Up THEN BEGIN
            { may want to read in saved data before setting direction - ******* }
            IF NOT Train_UserDriving THEN BEGIN
              SetTrainDirection(T, Up, NOT ForceAWrite, OK);
              SetTwoLightingChips(Train_LocoIndex, Up, Up, LightsOn);
            END ELSE BEGIN
              IF Train_UserDriving AND Train_UserRequiresInstructions THEN
                Log(Train_LocoChipStr + ' L= User instructed to set direction to Up');
            END;
          END ELSE
            IF Train_CurrentDirection = Down THEN BEGIN
              IF NOT Train_UserDriving THEN BEGIN
                SetTrainDirection(T, Down, NOT ForceAWrite, OK);
                SetTwoLightingChips(Train_LocoIndex, Down, Down, LightsOn);
              END ELSE BEGIN
                IF Train_UserDriving AND Train_UserRequiresInstructions THEN
                  Log(Train_LocoChipStr + ' L= User instructed to set direction to Down');
              END;
            END;
        END;
//        Train_Accelerating := True;
//        Train_AccelerationTimeInSeconds := 5.0;
//        Train_AccelerationStartTime := 0; &&&&&

//        Train_Decelerating := False;
//        Train_DesiredLenzSpeed := Train_SaveDesiredLenzSpeed;
//        Train_SaveDesiredLenzSpeed := 0;
//        Train_CurrentLenzSpeed := 0; &&&&&

        SetDesiredLocoLenzSpeed(Train_LocoIndex, 0, Train_UserDriving, Train_UserRequiresInstructions);
        IF Train_DoubleHeaderLocoChip <> UnknownLocoChip THEN
          SetDesiredLocoLenzSpeed(Train_DoubleHeaderLocoIndex, 0, Train_UserDriving, Train_UserRequiresInstructions);
        TrainsRestarted := True;
      END; {WITH}
    END;
    Inc(T);
  END; {WHILE}

  IF Restart AND TrainsRestarted THEN
    Log('AG All locos restarted')
  ELSE
    IF Restart THEN
      Log('AG No locos to restart');
END; { StartLocos }

PROCEDURE StopAParticularTrain(T : TrainIndex);
{ Stops just one train }
VAR
  DebugStr : String;
  OK : Boolean;

BEGIN
  IF T = UnknownTrainIndex THEN
    UnknownTrainRecordFound('StopAParticularTrain')
  ELSE BEGIN
    WITH Trains[T] DO BEGIN
      IF SystemOnline THEN BEGIN
        DebugStr := 'Train stop requested';
        StopAParticularLocomotive(Locos[Train_LocoIndex], OK);
        IF Train_DoubleHeaderLocoChip <> UnknownLocoChip THEN BEGIN
          StopAParticularLocomotive(Locos[Train_DoubleHeaderLocoIndex], OK);
          DebugStr := DebugStr + '. DH Loco ' + LocoChipToStr(Train_DoubleHeaderLocoChip) + ' also stopped';
        END;

        Log(Train_LocoChipStr + ' L ' + DebugStr);
      END;
    END; {WITH}
  END;
END; { StopAParticularTrain }

PROCEDURE StopLocos(Msg : String);
{ Stop all the locos currently in the diagram list. This is a better approach than the brute force approach which is to send a "StopAllLocomotives" command, which produces
  an "emergency off" situation.
}
VAR
  T : TrainIndex;

BEGIN
  Log('AG Stopping any locos - initiated by ' + Msg);

  T := 0;
  WHILE T <= High(Trains) DO BEGIN
    WITH Trains[T] DO BEGIN
      IF (Train_LocoChip <> UnknownLocoChip)
      AND Train_DiagramFound
      AND (Train_CurrentStatus <> Cancelled)
      AND (Train_LocoChip <> UnknownLocoChip)
      THEN BEGIN
        StopAParticularTrain(T);
        LocosStopped := True;
        DrawDiagramsSpeedCell(T);
      END;
    END; {WITH}
    Inc(T);
  END; {WHILE}

  IF LocosStopped THEN
    Log('AG All locos stopped')
  ELSE
    Log('AG No locos to stop');
END; { StopLocos }

FUNCTION StringArraysCompareOK(FirstArray, SecondArray : StringArrayType; OUT ErrorMsg : String) : Boolean;
{ Does an element by element comparison of two string arrays }
VAR
  DifferenceFound : Boolean;
  I : Word;
  MaxArrayLen : Integer;

BEGIN
  Result := False;

  DifferenceFound := False;

  IF (Length(FirstArray) = 0) AND (Length(SecondArray) = 0) THEN
    ErrorMsg := 'Both arrays are empty'
  ELSE
    IF Length(FirstArray) = 0 THEN BEGIN
      DifferenceFound := True;
      ErrorMsg := 'First array is empty';
    END ELSE
      IF Length(SecondArray) = 0 THEN BEGIN
        DifferenceFound := True;
        ErrorMsg := 'Second array is empty';
      END;

  IF (Length(FirstArray) = Length(SecondArray)) OR (Length(FirstArray) < Length(SecondArray)) THEN
    MaxArrayLen := Length(FirstArray)
  ELSE
    MaxArrayLen := Length(SecondArray);

  IF NOT DifferenceFound THEN BEGIN
    I := 0;
    WHILE (I <= High(FirstArray)) AND (I < MaxArrayLen) AND NOT DifferenceFound DO BEGIN
      IF FirstArray[I] <> SecondArray[I] THEN BEGIN
        DifferenceFound := True;
        ErrorMsg := 'First array differs from second array at position ' + IntToStr(I)
                    + ' (' + FirstArray[I] + ', ' + SecondArray[I] + ')';
      END;
      Inc(I);
    END; {WHILE}
  END;

  IF NOT DifferenceFound AND (Length(FirstArray) = Length(SecondArray)) THEN BEGIN
    Result := True;
    ErrorMsg := 'Arrays compare ok';
  END ELSE
    IF Length(FirstArray) < Length(SecondArray) THEN BEGIN
      IF DifferenceFound THEN
        ErrorMsg := ErrorMsg + '. Also the first array is shorter.'
      ELSE
        ErrorMsg := 'Arrays compare ok, but the first array is shorter';
    END ELSE BEGIN
      { Length(FirstArray) > Length(SecondArray) }
      IF DifferenceFound THEN
        ErrorMsg := ErrorMsg + '. Also the first array is longer.'
      ELSE
        ErrorMsg := 'Arrays compare ok, but the first array is longer';
    END;
END; { StringArraysCompareOK }

FUNCTION StringArrayToStr(StringArray : StringArrayType) : String;
{ List the contents of an array }
VAR
  I : Integer;

BEGIN
  Result := '';

  IF Length(StringArray) = 0 THEN
    Exit
  ELSE BEGIN
    FOR I := 0 TO High(StringArray) - 1 DO
      Result := Result + StringArray[I] + ', ';
    Result := Result + StringArray[High(StringArray)];
  END;
END; { StringArrayToStr }

FUNCTION StrToArea(Str : String) : Integer;
{ Convert a string to an area }
VAR
  Area : Integer;

BEGIN
  Str := TrimRemoveSpacesAndMakeUpperCase(Str);

  Result := UnknownArea;
  Area := 0;
  WHILE (Area <= High(Areas)) AND (Result = UnknownArea) DO BEGIN
    IF (Str = Trim(Areas[Area].Area_ShortStr))
    OR (Str = RemoveAllSpacesFromAString(Trim(UpperCase(Areas[Area].Area_LongStr))))
    OR (Str = RemoveAllSpacesFromAString(Trim(UpperCase(Areas[Area].Area_LongStr) + 'AREA')))
    OR (Str + 'AREA' = RemoveAllSpacesFromAString(Trim(UpperCase(Areas[Area].Area_LongStr))))
    OR (Str + 'AREA' = RemoveAllSpacesFromAString(Trim(UpperCase(Areas[Area].Area_LongStr) + 'AREA')))
    THEN
      Result := Area
    ELSE
      Inc(Area);
  END; {WHILE}
END; { StrToArea }

FUNCTION StrToAspect(Str : String) : AspectType;
{ Return the signal aspect string as an aspect type }
BEGIN
  Str := TrimRemoveSpacesAndMakeUpperCase(Str);

  IF (Str = FlashingDoubleYellowAspectStr) OR (Str = FlashingDoubleYellowAspectShortStr) THEN
    Result := FlashingDoubleYellowAspect
  ELSE
    IF (Str = FlashingSingleYellowAspectStr) OR (Str = FlashingSingleYellowAspectShortStr) THEN
      Result := FlashingSingleYellowAspect
    ELSE
      IF (Str = DoubleYellowAspectStr) OR (Str = DoubleYellowAspectShortStr) THEN
        Result := DoubleYellowAspect
      ELSE
        IF (Str = SingleYellowAspectStr) OR (Str = SingleYellowAspectShortStr) THEN
          Result := SingleYellowAspect
        ELSE
          IF (Str = GreenAspectStr) OR (Str = GreenAspectShortStr) THEN
            Result := GreenAspect
          ELSE
            IF (Str = RedAspectStr) OR (Str = RedAspectShortStr) THEN
              Result := RedAspect
            ELSE
              IF (Str = NoAspectStr) OR (Str = NoAspectShortStr) THEN
                Result := NoAspect
              ELSE
                Result := UnknownAspectType;
END; { StrToAspect }

FUNCTION StrToColour(Str : String) : TColour;
{ Checks if it's a Delphi colour or an FWP one }
BEGIN
  IF Pos(UnknownColourStr, Str) > 0 THEN
    Result := StringToColor(Copy(Str, Length(UnknownColourStr) + 2))
  ELSE
    IF Str = clFWPDkBlueStr THEN
      Result := clFWPDkBlue
    ELSE
      IF Str = clFWPOrangeStr THEN
        Result := clFWPOrange
      ELSE
        IF Str = clFWPLtBrownStr THEN
          Result := clFWPLtBrown
        ELSE
          IF Str = clFWPDkBrownStr THEN
            Result := clFWPDkBrown
          ELSE
            IF Str = clFWPPinkStr THEN
              Result := clFWPPink
            ELSE
              IF Str = clFWPDkGreyStr THEN
                Result := clFWPDkGrey
              ELSE
                IF Str = clFWPVeryDkGreyStr THEN
                  Result := clFWPVeryDkGrey
                ELSE
                  IF Str = clFWPPlatformColourStr THEN
                    Result := clFWPPlatformColour
                  ELSE
                    Result := StringToColor(Str);
END;

FUNCTION StrToDayOfTheWeek(Str : String) : DayOfTheWeekType;
{ Return a day of the week from a given string }
BEGIN
  IF (Str = MondayStr) OR (Str = MondayShortStr) THEN
    Result := Monday
  ELSE
    IF (Str = TuesdayStr) OR (Str = TuesdayShortStr) THEN
      Result := Tuesday
    ELSE
      IF (Str = WednesdayStr) OR (Str = WednesdayShortStr) THEN
        Result := Wednesday
      ELSE
        IF (Str = ThursdayStr) OR (Str = ThursdayShortStr) THEN
          Result := Thursday
        ELSE
          IF (Str = FridayStr) OR (Str = FridayShortStr) THEN
            Result := Friday
          ELSE
            IF (Str = SaturdayStr) OR (STR = SaturdayShortStr) THEN
              Result := Saturday
            ELSE
              IF (Str = SundayStr) OR (STR = SundayShortStr) THEN
                Result := Sunday
              ELSE
                Result := UnknownDayOfTheWeek;
END; { StrToDayOfTheWeek }

FUNCTION StrToDirectionType(Str : String) : DirectionType;
{ Convert a string to a direction }
BEGIN
  Str := TrimRemoveSpacesAndMakeUpperCase(Str);

  IF (Str = 'U') OR (Str = 'UP') THEN
    Result := Up
  ELSE
    IF (Str = 'D') OR (Str = 'DN') OR (Str = 'DOWN') THEN
      Result := Down
    ELSE
      IF (Str = 'B') OR (Str = 'BIDIRECTIONAL') THEN
        Result := Bidirectional
      ELSE
        Result := UnknownDirection;
END; { StrToDirectionType }

FUNCTION StrToEndOfLine(Str : String) : EndOfLineType;
{ Convert a string to an end of line }
BEGIN
  IF Str = BufferStopAtUpStr THEN
    Result := BufferStopAtUp
  ELSE
    IF Str = BufferStopAtDownStr THEN
      Result := BufferStopAtDown
    ELSE
      IF Str = ProjectedLineAtUpStr THEN
        Result := ProjectedLineAtUp
      ELSE
        IF Str = ProjectedLineAtDownStr THEN
          Result := ProjectedLineAtDown
        ELSE
          IF Str = NotEndOfLineStr THEN
            Result := NotEndOfLine
          ELSE
            Result := UnknownEndOfLine;
END; { StrToEndOfLine }

FUNCTION StrToFeedbackUnitType(Str : String) : TypeOfFeedbackDetector;
{ Convert a string to a feedback unit type }
BEGIN
  IF Str = TrackCircuitFeedbackDetectorStr THEN
    Result := TrackCircuitFeedbackDetector
  ELSE
    IF Str = TRSPlungerFeedbackDetectorStr THEN
      Result := TRSPlungerFeedbackDetector
    ELSE
      IF Str = PointFeedbackDetectorStr THEN
        Result := PointFeedbackDetector
    ELSE
      IF Str = LineFeedbackDetectorStr THEN
        Result := LineFeedbackDetector
      ELSE
        IF Str = MixedFeedbackDetectorStr THEN
          Result := MixedFeedbackDetectors
        ELSE
          IF Str = FeedbackDetectorOutOfUseStr THEN
            Result := FeedbackDetectorOutOfUse
          ELSE
             Result := UnknownFeedbackDetectorType;
END; { StrToFeedbackUnitType }

FUNCTION StrToGradient(Str : String) : GradientType;
{ Convert a string to a gradient }
BEGIN
  IF Str = LevelStr THEN
    Result := Level
  ELSE
    IF Str = RisingIfUpStr THEN
      Result := RisingIfUp
    ELSE
      IF Str = RisingIfDownStr THEN
        Result := RisingIfDown
      ELSE
        Result := UnknownGradientType;
END; { StrToGradient }

FUNCTION StrToIndicatorType(Str : String) : IndicatorType;
{ Return the type a route indicator is }
BEGIN
  IF Str = NoIndicatorStr THEN
    Result := NoIndicator
  ELSE
    IF Str = JunctionIndicatorStr THEN
      Result := JunctionIndicator
    ELSE
      IF Str = TheatreIndicatorStr THEN
        Result := TheatreIndicator
      ELSE
        IF Str = QueryIndicatorStr THEN
          Result := QueryIndicator
        ELSE
          Result := UnknownIndicator;
END; { StrToIndicatorType }

FUNCTION StrToLine(Str : String) : Integer;
{ Convert a string to a line name }
VAR
  Line : Integer;
  LineNameFound : Boolean;

BEGIN
  Str := TrimRemoveSpacesAndMakeUpperCase(Str);
  Result := UnknownLine;

  LineNameFound := False;
  Line := 0;
  WHILE (Line <= High(Lines)) AND NOT LineNameFound And (Str <> '') DO BEGIN
    IF Str = UpperCase(LineToStr(Line)) THEN BEGIN
      LineNameFound := True;
      Result := Line;
    END ELSE
      Inc(Line);
  END; {WHILE}
  IF NOT LineNameFound THEN BEGIN
    { ShowMessage('LineName not found');
    RunError(1);}
  END;
END; { StrToLine }

FUNCTION StrToLocation(Str : String) : Integer;
{ Convert a string to a location }
VAR
  Location : Integer;
  SaveStr : String;
  TestStr1 : String;
  TestStr2 : String;

BEGIN
  SaveStr := Str;

  Str := TrimRemoveSpacesAndMakeUpperCase(Str);

  Result := UnknownLocation;
  Location := 0;
  WHILE (Location <= High(Locations)) AND (Result = UnknownLocation) DO BEGIN
    TestStr1 := UpperCase(Locations[Location].Location_ShortNameStr);
    TestStr2 := UpperCase(Locations[Location].Location_LongNameStr);
    TestStr2 := RemoveAllSpacesFromAString(TestStr2);

    IF (Str = TestStr1) OR (Str = TestStr2) THEN
      Result := Location
    ELSE
      Inc(Location);
  END; {WHILE}
//  IF Result = UnknownLocation THEN
//    Log('X Unknown location ' + SaveStr + ' in StrToLocation')
END; { StrToLocation }

FUNCTION StrToPenStyle(Str : String) : TPenStyle;
{ Converts strings to pen styles }
BEGIN
  IF Str = SolidStr THEN
    Result := psSolid
  ELSE
    IF Str = DashStr THEN
      Result := psDash
    ELSE
      IF Str = DotStr THEN
        Result := psDot
      ELSE
        IF Str = DashDotStr THEN
          Result := psDashDot
        ELSE
          IF Str = DashDotDotStr THEN
            Result := psDashDotDot
          ELSE
            IF Str = ClearStr THEN
              Result := psClear
            ELSE
              IF Str = InsideFrameStr THEN
                Result := psInsideFrame
              ELSE
                Result := psSolid; { the default }
END; { StrToPenStyle }

FUNCTION StrToPlatformNumberPosition(Str : String) : PlatformNumberPositionType;
{ Return the given platform number position value as a string }
BEGIN
  IF Str = LeftTopStr THEN
    Result := LeftTop
  ELSE
    IF Str = RightTopStr THEN
      Result := RightTop
    ELSE
      IF Str = CentreTopStr THEN
        Result := CentreTop
      ELSE
        IF Str = LeftBottomStr THEN
          Result := LeftBottom
        ELSE
          IF Str = RightBottomStr THEN
            Result := RightBottom
          ELSE
            IF Str = CentreBottomStr THEN
              Result := CentreBottom
            ELSE
              Result := UnknownPlatformNumberPosition;
END; { StrToPlatformNumberPosition }

FUNCTION StrToPointState(Str : String) : PointStateType;
{ Convert a string to a point state }
BEGIN
  Result := PointStateUnknown;

  TRY
    IF Str = StraightStr THEN
      Result := Straight
    ELSE
      IF Str = DivergingStr THEN
        Result := Diverging
      ELSE
        IF Str = OutOfActionStr THEN
          Result := PointOutOfAction;
  EXCEPT
    ON E : Exception DO
      Log('EG StrToPointState: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { StrToPointState }

FUNCTION StrToPointType(Str : String) : TypeOfPoint;
{ Convert a string to a point state }
BEGIN
  IF Str = OrdinaryPointStr THEN
    Result := OrdinaryPoint
  ELSE
    IF Str = CrossOverPointStr THEN
      Result := CrossOverPoint
    ELSE
      IF Str = ThreeWayPointAStr THEN
        Result := ThreeWayPointA
      ELSE
        IF Str = ThreeWayPointBStr THEN
          Result := ThreeWayPointB
        ELSE
          IF Str = SingleSlipStr THEN
            Result := SingleSlip
          ELSE
            IF Str = DoubleSlipSTr THEN
              Result := DoubleSlip
            ELSE
              IF Str = ProtectedPointStr THEN
                Result := ProtectedPoint
              ELSE
                IF Str = CatchPointUpStr THEN
                  Result := CatchPointUp
                ELSE
                  IF Str = CatchPointDownStr THEN
                    Result := CatchPointDown
                  ELSE
                    Result := PointTypeUnknown;
END; { StrToPointType }

FUNCTION StrToSignalType(Str : String) : TypeOfSignal;
{ Return the type of a signal }
BEGIN
  IF Str = CallingOnStr THEN
    Result := CallingOn
  ELSE
    IF Str =  TwoAspectStr THEN
      Result := TwoAspect
    ELSE
      IF Str = ThreeAspectStr THEN
        Result := ThreeAspect
      ELSE
        IF Str = FourAspectStr THEN
          Result := FourAspect
        ELSE
          IF Str = SemaphoreHomeStr THEN
            Result := SemaphoreHome
          ELSE
            IF Str = SemaphoreDistantStr THEN
              Result := SemaphoreDistant
            ELSE
              Result := UnknownSignalType;
END; { StrToSignalType }

FUNCTION StrToTypeOfLine(Str : String) : TypeOfLine;
{ Convert a string to a line type }
BEGIN
  IF Str = MainOrGoodsLineStr THEN
    Result := MainOrGoods
  ELSE
    IF Str =MainLineStr THEN
      Result := MainLine
    ELSE
      IF Str =MainStationLineStr THEN
        Result := MainStationLine
      ELSE
        IF Str =GoodsLineStr THEN
          Result := GoodsLine
        ELSE
          IF Str =BranchLineDoubleStr THEN
            Result := BranchLineDouble
          ELSE
            IF Str =BranchLineSingleStr THEN
              Result := BranchLineSingle
            ELSE
              IF Str =BranchStationLineStr THEN
                Result := BranchStationLine
              ELSE
                IF Str =FiddleyardLineStr THEN
                  Result := FiddleyardLine
                ELSE
                  IF Str =IslandStationLineStr THEN
                    Result := IslandStationLine
                  ELSE
                    IF Str =NewlyCreatedLineStr THEN
                      Result := NewlyCreatedLine
                    ELSE
                      IF Str =ProjectedLineStr THEN
                        Result := ProjectedLine
                      ELSE
                        IF Str =SidingLineStr THEN
                          Result := SidingLine
                        ELSE
                          IF Str =SidingsApproachLineStr THEN
                            Result := SidingsApproach
                          ELSE
                            IF Str =StationAvoidingLineStr THEN
                              Result := StationAvoiding
                            ELSE
                              IF Str =WindowStationLineStr THEN
                                Result := WindowStationLine
                              ELSE
                                Result := UnknownTypeOfLine;
END; { StrToTypeOfLine }

FUNCTION StrToThroughLocationState(Str : String) : ThroughLocationStateType;
{ Return the through state from a given string }
BEGIN
  IF Str = ThroughLocationStr THEN
    Result := ThroughLocation
  ELSE
    IF Str = NonThroughLocationStr THEN
      Result := NonThroughLocation
    ELSE
      Result := UnknownThroughLocationState;
END; { StrToThroughLocationState }

FUNCTION StrToTrainTypeNum(Str : String) : Integer;
{ Returns the train type as a number; an up-to-date list as of 5/10/05 }
BEGIN
  IF Str = LightLocoStr THEN
    Result := 0
  ELSE
    IF Str = 'EXPRESSPASSENGER' THEN
      Result := 1
    ELSE
      IF Str = 'ORDINARYPASSENGER' THEN
        Result := 2
      ELSE
        IF Str = 'EXPRESSFREIGHT' THEN
          Result := 3
        ELSE
          IF Str = '75MPHFREIGHT' THEN
            Result := 4
          ELSE
            IF Str = 'EMPTYCOACHINGSTOCK' THEN
              Result := 5
            ELSE
              IF Str = '60MPHFREIGHT' THEN
                Result := 6
              ELSE
                IF Str = '45MPHFREIGHT' THEN
                  Result := 7
                ELSE
                  IF Str = '35MPHFREIGHT' THEN
                    Result := 8
                  ELSE
                    IF Str = 'INTERNATIONAL' THEN
                      Result := 9
                    ELSE
                      Result := UnknownTrainTypeNum;
END; { StrToTrainTypeNum }

FUNCTION SubRouteStateToStr(SubRouteState : SubRouteStateType) : String;
{ Return the state of the subroute as a string }
BEGIN
  CASE SubRouteState OF
    SubRouteNotYetSetUp:
      Result := 'not yet set up';
    SubRouteSettingUpInProgress:
      Result := 'setting up in progress';
    SubRouteSettingUpStalled:
      Result := 'stalled';
    SubRouteSettingUpFailed:
      Result := 'setting up failed';
    SubRouteSetUp:
      Result := 'set up';
    SubRouteSettingUpCancelled:
      Result := 'setting up cancelled';
    SubRouteSettingUpHeld:
      Result := 'setting up held';
    SubRouteSettingUpHeldByStationSupervisor:
      Result := 'setting up held by station supervisor';
    SubRouteCleared:
      Result := 'cleared';
    SubRouteToBeCleared:
      Result := 'to be cleared';
  ELSE
    Result := 'Error in DescribeSubRouteState routine: invalid subroute state';
  END; {CASE}
END; { SubRouteStateToStr }

PROCEDURE ResetTestCount;
{ Resets the number which is incremented each time the TestCount or TestCountStr routines are called - used for debugging }
BEGIN
  TestCounter := 0;
  Debug('TestCounter has been reset to zero');
END; { ResetTestCount }

FUNCTION TestCount : Integer;
{ Returns a number which is incremented each time the routine is called - used for debugging }
BEGIN
  Inc(TestCounter);
  Result := TestCounter;
END; { TestCount }

FUNCTION TestCountStr : String;
{ Returns as a string a number which is incremented each time the routine is called - used for debugging }
BEGIN
  Inc(TestCounter);
  Result := 'Test=' + IntToStr(TestCounter);
END; { TestCount }

FUNCTION ThroughLocationStateToStr(ThroughLocationState : ThroughLocationStateType) : String;
{ Return the through state of the location as a string }
BEGIN
  CASE ThroughLocationState OF
    ThroughLocation:
      Result := ThroughLocationStr;
    NonThroughLocation:
      Result := NonThroughLocationStr;
  ELSE
    Result := 'Error in DescribeThroughLocationState routine: invalid through Location state';
  END; {CASE}
END; { ThroughLocationStateToStr }

FUNCTION TimeToHMStr(Time : TDateTime) : String;
{ Return a time string as hh:mm }
BEGIN
  TRY
    FormatSettings.LongTimeFormat := 'hh:mm';
    Result := TimeToStr(Time);
  EXCEPT
    ON E : Exception DO
      Log('EG TimeToHM_Str: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { TimeToHMStr }

FUNCTION TimeToHMSStr(Time : TDateTime) : String;
{ Return a time string as hh:mm:ss }
BEGIN
  TRY
    FormatSettings.LongTimeFormat := 'hh:mm:ss';
    Result := TimeToStr(Time);
  EXCEPT
    ON E : Exception DO
      Log('EG TimeToHMSStr: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { TimeToHMSStr }

FUNCTION TimeToHMSZStr(Time : TDateTime) : String;
{ Return a time string as hh:mm:ss:zzz }
BEGIN
  TRY
    FormatSettings.LongTimeFormat := 'hh:mm:ss:zzz';
    Result := TimeToStr(Time);
  EXCEPT
    ON E : Exception DO
      Log('EG TimeToHMSZStr: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { TimeToHMSZStr }

FUNCTION TrackCircuitStateIsPermanentlyOccupied(State : TrackCircuitStateType) : Boolean;
{ Returns true if a given track-circuit state is not set as permanently occupied }
BEGIN
  CASE State OF
    TCOutOfUseSetByUser, TCOutOfUseAsNoFeedbackReceived, TCLocoOutOfPlaceOccupation, TCPermanentFeedbackOccupation, TCPermanentOccupationSetByUser,
    TCPermanentSystemOccupation:
      Result := True;
  ELSE
    { all other occupation types }
    Result := False;
  END; {CASE}
END; { TrackCircuitStateIsPermanentlyOccupied }

FUNCTION TrackCircuitStateIsTemporarilyOccupied(State : TrackCircuitStateType) : Boolean;
{ Returns true if a given track-circuit state is set as temporarily occupied }

BEGIN
  CASE State OF
    TCFeedbackOccupation, TCFeedbackOccupationButOutOfUse, TCSystemOccupation:
      Result := True;
  ELSE
    { all other occupation types }
    Result := False;
  END; {CASE}
END; { TrackCircuitStateIsTemporarilyOccupied }

FUNCTION TrackCircuitStateToStr(State : TrackCircuitStateType) : String;
{ Describe the current state of a given track circuit }
BEGIN
  CASE State OF
    TCFeedbackOccupation:
      Result := 'feedback occupation';
    TCFeedbackOccupationButOutOfUse:
      Result := 'feedback occupation by out of use';
    TCLocoOutOfPlaceOccupation:
      Result := 'loco out of place';
    TCMissingOccupation:
      Result := 'missing occupation';
    TCOutOfUseSetByUser:
      Result := 'out of use set by user';
    TCOutOfUseAsNoFeedbackReceived:
      Result := 'out of use as no feedback received';
    TCPermanentFeedbackOccupation:
      Result := 'permanent feedback occupation';
    TCPermanentOccupationSetByUser:
      Result := 'permanent occupation set by user';
    TCPermanentSystemOccupation:
      Result := 'permanent system occupation';
    TCSystemOccupation:
      Result := 'system occupation';
    TCUnoccupied:
      Result := 'unoccupied';
  ELSE
    Result := 'unknown state'
  END; {CASE}
END; { TrackCircuitStateToStr }

FUNCTION SpeedInMPHToLocoLenzSpeed(L : LocoIndex; Speed : MPHType) : Integer;
{ Return the appropriate Lenz speed for the given loco }
BEGIN
  Result := 0;

  IF L = UnknownLocoIndex THEN
   UnknownLocoRecordFound('SpeedInMPHToLocoLenzSpeed')
  ELSE BEGIN
    WITH Locos[L] DO BEGIN
      IF L <> UnknownLocoIndex THEN BEGIN
         CASE Speed OF
          MPH0:
            Result := 0;
          MPH10:
            Result := Loco_Speed10;
          MPH20:
            Result := Loco_Speed20;
          MPH30:
            Result := Loco_Speed30;
          MPH40:
            Result := Loco_Speed40;
          MPH50:
            Result := Loco_Speed50;
          MPH60:
            Result := Loco_Speed60;
          MPH70:
            Result := Loco_Speed70;
          MPH80:
            Result := Loco_Speed80;
          MPH90:
            Result := Loco_Speed90;
          MPH100:
            Result := Loco_Speed100;
          MPH110:
            Result := Loco_Speed110;
          MPH120:
            Result := Loco_Speed120;
        ELSE
          Log('XG Unknown speed type found in SpeedInMPHToLocoLenzSpeed');
        END; {CASE}
      END; {WITH}
    END;
  END;
END; { SpeedInMPHToLocoLenzSpeed }

FUNCTION TrainStatusToStr(Status : TrainStatusType) : String;
{ Return the given train status as a string }
BEGIN
  CASE Status OF
    Cancelled:
      Result := 'cancelled';
    CommencedRouteing:
      Result := 'commenced routeing';
    Departed:
      Result := 'departed';
    InLightsOnTime:
      Result := 'in lights-on time';
    Missing:
      Result := 'missing';
    MissingAndSuspended:
      Result := 'missing and suspended';
    NonMoving:
      Result := 'non-moving';
    ReadyForCreation:
      Result := 'ready for creation';
    ReadyForRouteing:
      Result := 'ready for routeing';
    ReadyToDepart:
      Result := 'ready to depart';
    RemovedFromDiagrams:
      Result := 'removed from diagrams';
    RouteCompleted:
      Result := 'route completed';
    RouteingWhileDeparted:
      Result := 'routeing while departed';
    Suspended:
      Result := 'suspended';
    ToBeRemovedFromDiagrams:
      Result := 'to be removed from diagrams';
    UnknownTrainStatus:
      Result := UnknownTrainStatusStr;
    WaitingForLightsOn:
      Result := 'waiting for lights on';
    WaitingForRemovalFromDiagrams:
      Result := 'waiting for removal from diagrams';
    WaitingForRouteing:
      Result := 'waiting for routeing';
    WaitingForHiddenStationSignalAspectToClear:
      Result := 'waiting for hidden station signal aspect to clear';
  ELSE
    Result := UnknownTrainStatusStr;
  END; {CASE}
END; { TrainStatusToStr }

FUNCTION TrainTypeNumToStr(TrainTypeNum : Integer) : String;
{ Returns the train type number as a string; an up-to-date list as of 5/10/05 }
BEGIN
  CASE TrainTypeNum OF
    0:
      Result := LightLocoStr;
    1:
      Result := ExpressPassengerStr;
    2:
      Result := OrdinaryPassengerStr;
    3:
      Result := ExpressFreightStr;
    4:
      Result := Freight75mphStr;
    5:
      Result := EmptyCoachingStockStr;
    6:
      Result := Freight60mphStr;
    7:
      Result := Freight45mphStr;
    8:
      Result := Freight35mphStr;
    9:
      Result := InternationalStr;
  ELSE
    Result := UnknownTrainTypeStr;
  END; {CASE}
END; { TrainTypeNumToStr }

FUNCTION TrainTypeToTrainTypeNum(TrainType : TypeOfTrainType) : Integer;
{ Returns the number for the kind of train it is; an up-to-date list as of 5/10/05 }
BEGIN
  CASE TrainType OF
    LightLocoType:
      Result := 0;
    ExpressPassengerType:
      Result := 1;
    OrdinaryPassengerType:
      Result := 2;
    ExpressFreightType:
      Result := 3;
    Freight75mphType:
      Result := 4;
    EmptyCoachingStockType:
      Result := 5;
    Freight60mphType:
      Result := 6;
    Freight45mphType:
      Result := 7;
    Freight35mphType:
      Result := 8;
    InternationalType:
      Result := 9;
  ELSE
    Result := -1;
  END; {CASE}
END; { TrainTypeToTrainTypeNum }

FUNCTION TrainTypeNumToTrainType(TrainTypeNum : Integer) : TypeOfTrainType;
{ Returns the train type; an up-to-date list as of 5/10/05 }
BEGIN
  CASE TrainTypeNum OF
    0: { can also be supplied as 10 }
      Result := LightLocoType;
    1:
      Result := ExpressPassengerType;
    2:
      Result := OrdinaryPassengerType;
    3:
      Result := ExpressFreightType;
    4:
      Result := Freight75mphType;
    5:
      Result := EmptyCoachingStockType;
    6:
      Result := Freight60mphType;
    7:
      Result := Freight45mphType;
    8:
      Result := Freight35mphType;
    9:
      Result := InternationalType;
  ELSE
    Result := UnknownTrainType;
  END; {CASE}
END; { TrainTypeNumToTrainType }

FUNCTION TrimRemoveSpacesAndMakeUpperCase(Str : String) : String;
{ Tidy up a string to make comparisons more accurate }
BEGIN
  Result := Trim(Str);
  Result := RemoveAllSpacesFromAString(Result);
  Result := UpperCase(Result);
END; { TrimRemoveSpacesAndMakeUpperCase }

FUNCTION TTS(Time : TDateTime) : String;
{ Return a time string as hh:mm - abbreviated form of TimeToHMStr above with format 'hh:mm' for debugging }
BEGIN
  TRY
    FormatSettings.LongTimeFormat := 'hh:mm';
    Result := TimeToStr(Time);
  EXCEPT
    ON E : Exception DO
      Log('EG TTS: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TTS }

FUNCTION TTSS(Time : TDateTime) : String;
{ Return a time string as hh:mm:ss - abbreviated form of TimeToHMSStr for debugging }
BEGIN
  TRY
    FormatSettings.LongTimeFormat := 'hh:mm:ss';
    Result := TimeToStr(Time);
  EXCEPT
    ON E : Exception DO
      Log('EG TTSS: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TTSS }

FUNCTION TTSA(TimesArray : DateTimeArrayType) : String;
{ Return time strings as hh:mm - abbreviated form of TimeToHMStr above with format 'hh:mm' for debugging }
VAR
  I : Integer;

BEGIN
  TRY
    Result := '';
    FormatSettings.LongTimeFormat := 'hh:mm';
    IF Length(TimesArray) > 0 THEN BEGIN
      FOR I := 0 TO High(TimesArray) DO BEGIN
        IF I = 0 THEN
          Result := TimeToStr(TimesArray[I])
        ELSE
          Result := Result + ', ' + TimeToStr(TimesArray[I]);
      END;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG TTSA: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
END; { TTSA }

FUNCTION TTSZ(Time : TDateTime) : String;
{ Return a time string as hh:mm:ss:zzz }
BEGIN
  TRY
    FormatSettings.LongTimeFormat := 'hh:mm:ss:zzz';
    Result := TimeToStr(Time);
  EXCEPT
    ON E : Exception DO
      Log('EG TimeToHMSZStr: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { TimeToHMSZStr }

PROCEDURE TurnLocoHeadLightsOff(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turn off a loco's headlight }
CONST
  LightsOff = False;

BEGIN
  IF L = UnknownLocoIndex THEN
    UnknownLocoRecordFound('TurnLocoHeadLightsOff')
  ELSE BEGIN
    WITH Locos[L] DO BEGIN
      IF UserMsgRequired THEN
        UserMsg := 'User instructed to turn lights on by means of function 0'
      ELSE BEGIN
        Log(Locos[L].Loco_LocoChipStr + ' L Turning lights off');
        SetSingleLocoFunction(Locos[L], Function0, LightsOff, OK);
      END;
    END; {WITH}
  END;
END; { TurnLocoHeadLightsOff }

PROCEDURE TurnLocoHeadLightsOn(L : LocoIndex; Direction : DirectionType; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turn on a loco's headlight, and tail light if they are connected }
CONST
  ForceRead = True;
  LightsOn = True;
  LightsOff = False;

VAR
  DebugStr : String;

BEGIN
  OK := True;

  IF L = UnknownLocoIndex THEN
    UnknownLocoRecordFound('TurnLocoHeadLightsOn')
  ELSE BEGIN
    WITH Locos[L] DO BEGIN
      DebugStr := 'Turning lights on';
      IF L <> UnknownLocoIndex THEN BEGIN
        IF SingleLocoFunctionIsOn(Locos[L], Function0, NOT ForceRead, OK) THEN BEGIN
          IF UserMsgRequired THEN
            UserMsg := 'User: no action is required as lights are already on';
          DebugStr := DebugStr + ' is not necessary as lights are already on'
        END ELSE
          IF UserMsgRequired THEN
            UserMsg := 'User: turn loco ' + Loco_LocoChipStr + '''s lights on - direction ' + DirectionToStr(Direction) + ' - by means of function 0'
          ELSE BEGIN
            SetSingleLocoFunction(Locos[L], Function0, LightsOn, OK);
            DebugStr := DebugStr + ' - direction ' + DirectionToStr(Direction);
          END;

        Log(Loco_LocoChipStr + ' L ' + DebugStr);
      END;
    END; {WITH}
  END;
END; { TurnLocoHeadLightsOn }

PROCEDURE TurnLocoTailLightsOnAtOneEnd(L : LocoIndex; Direction : DirectionType; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turn on a loco's tail lights at one particular end }
CONST
  ForceRead = True;
  LightsOn = True;
  LightsOff = False;

BEGIN
  OK := True;
  IF UserMsgRequired THEN BEGIN
    IF Direction = Up THEN
      UserMsg := 'User: turn tail lights at up end on by setting function 1 on and function 2 off'
    ELSE
      UserMsg := 'User: turn tail lights at down end on by setting function 1 off and function 2 on';
  END ELSE BEGIN
    IF L = UnknownLocoIndex THEN
      UnknownLocoRecordFound('TurnLocoTailLightsOnAtOneEnd')
    ELSE BEGIN
      WITH Locos[L] DO BEGIN
        IF Direction = Up THEN BEGIN
          Log(Loco_LocoChipStr + ' L Turning up tail lights on');
          SetSingleLocoFunction(Locos[L], Function1, LightsOn, OK);
          SetSingleLocoFunction(Locos[L], Function2, LightsOff, OK);
        END ELSE BEGIN
          Log(Loco_LocoChipStr + ' L Turning down tail lights on');
          SetSingleLocoFunction(Locos[L], Function1, LightsOff, OK);
          SetSingleLocoFunction(Locos[L], Function2, LightsOn, OK);
        END;
      END; {WITH}
    END;
  END;
END; { TurnLocoTailLightsOnAtOneEnd }

PROCEDURE TurnLocoTailLightsOnAtBothEnds(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turn on a loco's tail lights at both ends - used for stationary locos }
CONST
  ForceRead = True;
  LightsOn = True;
  LightsOff = False;

BEGIN
  OK := True;

  IF UserMsgRequired THEN
    UserMsg := 'User: turn tail lights on at both ends by setting functions 1 and 2 on'
  ELSE BEGIN
    IF L = UnknownLocoIndex THEN
      UnknownLocoRecordFound('TurnLocoTailLightsOnAtBothEnds')
    ELSE BEGIN
      WITH Locos[L] DO BEGIN
        Log(Loco_LocoChipStr + ' L Turning tail lights on at both ends');
        SetSingleLocoFunction(Locos[L], Function1, LightsOn, OK);
        SetSingleLocoFunction(Locos[L], Function2, LightsOn, OK);
      END; {WITH}
    END;
  END;
END; { TurnLocoTailLightsOnAtBothEnds }

PROCEDURE TurnLocoLightsOn(L : LocoIndex; NonMoving, LightLoco : Boolean; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turns the lights on by changing functions on the loco chip and optional second chip }
CONST
  ForceRead = True;
  LightsOn = True;
  LightsOff = False;

  PROCEDURE TurnExpressModelsLocoHeadlightsOn(L : LocoIndex; Direction : DirectionType; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
  { Turn on a loco's head lights in day and night mode }
  BEGIN
    IF L <> UnknownLocoIndex THEN BEGIN
      WITH Locos[L] DO BEGIN
        IF Direction = Up THEN BEGIN
          { Is it daytime or night time? }
          IF DayTime THEN BEGIN
            IF UserMsgRequired THEN
              UserMsg := 'User: turn day-time up lights on by setting function 6 off and 7 on'
            ELSE BEGIN
              Log(Loco_LocoChipStr + ' L Turning day-time up lights on');
              SetSingleLocoFunction(Locos[L], Function6, LightsOff, OK);
              SetSingleLocoFunction(Locos[L], Function7, LightsOn, OK);
            END;
          END ELSE BEGIN
            IF UserMsgRequired THEN
              UserMsg := 'User: turn night-time up lights on by setting function 6 on and 7 off'
            ELSE BEGIN
              Log(Loco_LocoChipStr + ' L Turning night-time up lights on');
              SetSingleLocoFunction(Locos[L], Function6, LightsOn, OK);
              SetSingleLocoFunction(Locos[L], Function7, LightsOff, OK);
            END;
          END;
        END ELSE BEGIN
          { Direction = Down }
          { Is it daytime or night time? }
          IF DayTime THEN BEGIN
            IF UserMsgRequired THEN
              UserMsg := 'User: turn day-time down lights on by setting function 4 off and 5 on'
            ELSE BEGIN
              Log(Loco_LocoChipStr + ' L Turning day-time down lights on');
              SetSingleLocoFunction(Locos[L], Function4, LightsOff, OK);
              SetSingleLocoFunction(Locos[L], Function5, LightsOn, OK);
            END;
          END ELSE BEGIN
            IF UserMsgRequired THEN
              UserMsg := 'User instructed to turn night-time down lights on by setting function 4 on and 5 off'
            ELSE BEGIN
              Log(Loco_LocoChipStr + ' L Turning night-time down lights on');
              SetSingleLocoFunction(Locos[L], Function4, LightsOn, OK);
              SetSingleLocoFunction(Locos[L], Function5, LightsOff, OK);
            END;
          END;
        END;
      END; {WITH}
    END;
  END; { TurnExpressModelsLocoHeadlightsOn }

BEGIN
  OK := False;
  IF SystemOnline THEN BEGIN
    IF L = UnknownLocoIndex THEN
      UnknownLocoRecordFound('TurnLocoLightsOn')
    ELSE BEGIN
      WITH Locos[L] DO BEGIN
        IF Loco_LightsType <> NoLights THEN BEGIN
          IF NonMoving THEN BEGIN
            IF Loco_LightsType = HeadlightsAndTailLightsConnected THEN
              TurnLocoHeadLightsOn(L, Loco_CurrentDirection, UserMsgRequired, UserMsg, OK)
            ELSE
              IF Loco_LightsType = LightsOperatedByTwoChips THEN BEGIN
                TurnLocoHeadLightsOn(L, Down, UserMsgRequired, UserMsg, OK);
                TurnLocoHeadLightsOn(L, Up, UserMsgRequired, UserMsg, OK);
              END ELSE
                IF LightLoco THEN
                  { If train isn't moving, and is a light loco, needs red lights at both ends }
                  TurnLocoTailLightsOnAtBothEnds(L, UserMsgRequired, UserMsg, OK)
                ELSE BEGIN
                  { If train isn't moving, and is not a light loco, needs a red light at the front }
                  IF Loco_CurrentDirection = Up THEN
                    TurnLocoTailLightsOnAtOneEnd(L, Up, UserMsgRequired, UserMsg, OK)
                  ELSE
                    TurnLocoTailLightsOnAtOneEnd(L, Down, UserMsgRequired, UserMsg, OK);
                END;
          END ELSE BEGIN
            { a moving Loco }
            IF (Loco_LightsType = HeadlightsAndTailLightsConnected) THEN
              TurnLocoHeadLightsOn(L, Loco_CurrentDirection, UserMsgRequired, UserMsg, OK)
            ELSE
              IF Loco_LightsType = HeadlightsAndTailLightsSeparatelySwitched THEN BEGIN
                TurnLocoHeadLightsOn(L, Loco_CurrentDirection, UserMsgRequired, UserMsg, OK);
                { If train is a light loco, needs a red light at the rear }
                IF LightLoco THEN BEGIN
                  IF Loco_CurrentDirection = Up THEN
                    TurnLocoTailLightsOnAtOneEnd(L, Down, UserMsgRequired, UserMsg, OK)
                  ELSE
                    TurnLocoTailLightsOnAtOneEnd(L, Up, UserMsgRequired, UserMsg, OK);
                END;
              END ELSE
                IF Loco_LightsType = ExpressModelsSeparateHeadlights THEN
                  TurnExpressModelsLocoHeadlightsOn(L, Loco_CurrentDirection, UserMsgRequired, UserMsg, OK)
                ELSE
                  IF Loco_LightsType = LightsOperatedByTwoChips THEN BEGIN
                    TurnLocoHeadLightsOn(L, Loco_CurrentDirection, UserMsgRequired, UserMsg, OK);
                    TurnLocoHeadLightsOn(L, Loco_CurrentDirection, UserMsgRequired, UserMsg, OK);
                  END ELSE
                    IF Loco_LightsType = CustomLightingKit THEN BEGIN
                     { **** }
                    END;
          END;
        END;
      END; {WITH}
    END;
  END;
END; { TurnLocoLightsOn }

PROCEDURE TurnLocoTailLightsOff(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turn off a loco's tail lights }
CONST
  LightsOff = True;

BEGIN
  IF UserMsgRequired THEN
    UserMsg := 'User: turn up and down tail lights off by setting functions 1 and 2 off'
  ELSE BEGIN
    IF L = UnknownLocoIndex THEN
      UnknownLocoRecordFound('TurnTailLightsOff')
    ELSE BEGIN
      Log(Locos[L].Loco_LocoChipStr + ' L Turning up and down tail lights off');
      SetSingleLocoFunction(Locos[L], Function1, LightsOff, OK);
      SetSingleLocoFunction(Locos[L], Function2, LightsOff, OK);
    END;
  END;
END; { TurnTailLightsOff }

PROCEDURE TurnLocoLightsOff(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turns the lights off by changing functions on the loco chip and optional second chip }
CONST
  LightsOn = True;
  LightsOff = True;

  PROCEDURE TurnExpressModelsLocoHeadlightsOff(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
  { Turn off a loco's head lights in day and night mode }
  BEGIN
    IF UserMsgRequired THEN
      UserMsg := 'User: turn day-time and night-time lights off by setting functions 4-7 off'
    ELSE BEGIN
      IF L <> UnknownLocoIndex THEN BEGIN
        WITH Locos[L] DO BEGIN
          Log(Loco_LocoChipStr + ' L Turning day-time and night-time down and up lights off');
          SetSingleLocoFunction(Locos[L], Function4, LightsOff, OK);
          SetSingleLocoFunction(Locos[L], Function5, LightsOff, OK);
          SetSingleLocoFunction(Locos[L], Function6, LightsOff, OK);
          SetSingleLocoFunction(Locos[L], Function7, LightsOff, OK);
        END; {WITH}
      END;
    END;
  END; { TurnExpressModelsLocoHeadlightsOff }

BEGIN
  IF SystemOnline THEN BEGIN
    IF L = UnknownLocoIndex THEN
      UnknownLocoRecordFound('TurnLocoLightsOff')
    ELSE BEGIN
      WITH Locos[L] DO BEGIN
        IF Loco_LightsType <> NoLights THEN BEGIN
          IF Loco_LightsType = HeadlightsAndTailLightsConnected THEN
            TurnLocoHeadLightsOff(L, UserMsgRequired, UserMsg, OK)
          ELSE
            IF Loco_LightsType = HeadlightsAndTailLightsSeparatelySwitched THEN BEGIN
              TurnLocoHeadLightsOff(L, UserMsgRequired, UserMsg, OK);
              TurnLocoTailLightsOff(L, UserMsgRequired, UserMsg, OK);
            END ELSE
              IF Loco_LightsType = ExpressModelsSeparateHeadlights THEN BEGIN
                TurnLocoHeadLightsOff(L, UserMsgRequired, UserMsg, OK);
                TurnLocoTailLightsOff(L, UserMsgRequired, UserMsg, OK);
                TurnExpressModelsLocoHeadlightsOff(L, UserMsgRequired, UserMsg, OK);
              END ELSE
                IF Loco_LightsType = LightsOperatedByTwoChips THEN BEGIN
                  TurnLocoHeadLightsOff(L, UserMsgRequired, UserMsg, OK);
                  TurnLocoHeadLightsOff(L, UserMsgRequired, UserMsg, OK);
                END ELSE
                  IF Loco_LightsType = CustomLightingKit THEN BEGIN
                  END;

          IF OK THEN BEGIN
            Loco_LightsOn := False;
            Log(Loco_LocoChipStr + ' L Lights turned off');
          END ELSE
            Log(Loco_LocoChipStr + ' L Lights did not turn off');
        END;
      END; {WITH}
    END;
  END;
END; { TurnLocoLightsOff }

PROCEDURE TurnLocoCabLightsOn(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turns cab lights on or off. Assumes that the cab lights are operated by function one }
CONST
  LightsOn = True;

BEGIN
  OK := False;

  IF L = UnknownLocoIndex THEN
    UnknownLocoRecordFound('TurnLocoCabLightsOn')
  ELSE BEGIN
    WITH Locos[L] DO BEGIN
      IF Loco_HasCabLights THEN BEGIN
        IF UserMsgRequired THEN
          UserMsg := 'User: turn cab lights on by setting functions 1 on'
        ELSE BEGIN
          SetSingleLocoFunction(Locos[L], Function1, LightsOn, OK);
          Log(Loco_LocoChipStr + ' L Cab lights turned on');
        END;
      END;
    END; {WITH}
  END;
END; { TurnLocoCabLightsOn }

PROCEDURE TurnLocoCabLightsOff(L : LocoIndex; UserMsgRequired : Boolean; OUT UserMsg : String; OUT OK : Boolean);
{ Turns cab lights off. Assumes that the cab lights are operated by function one }
CONST
  LightsOn = True;

BEGIN
  OK := False;

  IF L = UnknownLocoIndex THEN
    UnknownLocoRecordFound('TurnLocoCabLightsOff')
  ELSE BEGIN
    WITH Locos[L] DO BEGIN
      IF Loco_HasCabLights THEN BEGIN
        IF UserMsgRequired THEN
          UserMsg := 'User: turn cab lights off by setting functions 1 off'
        ELSE BEGIN
          SetSingleLocoFunction(Locos[L], Function1, NOT LightsOn, OK);
          Log(Loco_LocoChipStr + ' L Cab lights turned off');
        END;
      END;
    END; {WITH}
  END;
END; { TurnLocoCabLightsOff }

FUNCTION TypeOfLineToStr(T : TypeOfLine) : String;
{ Describe a line type }
BEGIN
  CASE T OF
    MainOrGoods:
      Result := MainOrGoodsLineStr;
    MainLine:
      Result := MainLineStr;
    MainStationLine:
      Result := MainStationLineStr;
    GoodsLine:
      Result := GoodsLineStr;
    BranchLineDouble:
      Result := BranchLineDoubleStr;
    BranchLineSingle:
      Result := BranchLineSingleStr;
    BranchStationLine:
      Result := BranchStationLineStr;
    FiddleyardLine:
      Result := FiddleyardLineStr;
    IslandStationLine:
      Result := IslandStationLineStr;
    NewlyCreatedLine:
      Result := NewlyCreatedLineStr;
    ProjectedLine:
      Result := ProjectedLineStr;
    SidingLine:
      Result := SidingLineStr;
    SidingsApproach:
      Result := SidingsApproachLineStr;
    StationAvoiding:
      Result := StationAvoidingLineStr;
    WindowStationLine:
      Result := WindowStationLineStr;
  ELSE {CASE}
    Result := UnknownLineTypeStr;
  END; {CASE}
END; { TypeOfLineToStr }

FUNCTION WorkingTimetableStatusToStr(Status : WorkingTimetableStatusType) : String;
{ Return the given working timetable status as a string }
BEGIN
  CASE Status OF
    EntryCreatedFromWorkingTimetable:
      Result := 'Entry Created From Working Timetable';
    AdditionalEntryCreated:
      Result := 'Additional Entry Created';
    EntryCancelled:
      Result := 'Entry Cancelled';
    UnknownEntryStatus:
      Result := 'Unknown Entry Status';
  ELSE
    Result := 'Unknown Entry Status';
  END; {CASE}
END; { TrainStatusToStr }

PROCEDURE WriteVariableDataToFile;
{ List some variable data to a given file }
VAR
  AllLocos : Boolean;
  AllActiveLocos : Boolean;
  ErrorMsg : String;
  I : Integer;
  L : LocoIndex;
  OnlyLocosInDiagram : Boolean;
  T : TrainIndex;
  TempOutputFile : Text;
                                                     { needs working on post change ********** &&&&& }
BEGIN
  TRY
    AllLocos := False;
    AllActiveLocos := False;
    OnlyLocosInDiagram := False;

    IF OpenOutputFileOK(TempOutputFile, PathToRailDataFiles + 'Rail Variable Data', ErrorMsg, NOT AppendToFile) THEN BEGIN
      Log('XG Beginning write of all variable data to file');

      { Train record first }
      CASE MessageDialogueWithDefault('All locos, all locos marked as active, or only locos in the diagram?',
                                      StopTimer, mtWarning, [mbYes, mbNo, mbAbort], ['&All', '&AllActive', '&Diagram'], mbAbort)
      OF
        mrYes:
          AllLocos := True;
        mrNo:
          AllActiveLocos := True;
        mrAbort:
          OnlyLocosInDiagram := True;
      END; {CASE}

//      IF MessageDialogueWithDefault('All active trains or just trains in the diagram?',
//                                    StopTimer, mtWarning, [mbOK, mbAbort], ['&Active', '&Diagram'], mbAbort) = mrOK
//        THEN BEGIN
//          ActiveTrains := True;
//          OnlyTrainsInDiagram := False;
//        END ELSE BEGIN
//          ActiveTrains := False;
//          OnlyTrainsInDiagram := True;
//        END;
//      END;

      WriteLn(TempOutputFile, 'Loco Records: ' + IfThen(AllLocos,
                                                         '(All Locos)',
                                                         IfThen(AllActiveLocos,
                                                                '(only active locos',
                                                                IfThen(OnlyLocosInDiagram,
                                                                      '(only locos in the current diagram)'))));

      L := 0;
      WHILE L <= High(Locos) DO BEGIN
        WITH Locos[L] DO BEGIN
          IF AllLocos OR (AllActiveLocos AND Loco_Active) OR (OnlyLocosInDiagram AND Loco_DiagramFound) THEN BEGIN
            WriteLn(TempOutputFile);
            WriteLn(TempOutputFile, 'Loco_LocoChip = ' + IntToStr(Loco_LocoChip));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Accelerating = ' + BoolToStr(Loco_Accelerating, True));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_AccelerationAdjustRange = ' + IntToStr(Loco_AccelerationAdjustRange));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_AccelerationStartTime = ' + TimeToHMSStr(Loco_AccelerationStartTime));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_AccelerationStr = ' + Loco_AccelerationStr);
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_AccelerationTimeInterval = ' + FloatToStr(Loco_AccelerationTimeInterval));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Active = ' + BoolToStr(Loco_Active, True));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_ActualNumStr = ' + Loco_ActualNumStr);
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Decelerating = ' + BoolToStr(Loco_Decelerating, True));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Description = ' + Loco_Description);
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_DesiredLenzSpeed = ' + IntToStr(Loco_DesiredLenzSpeed));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_DiagramFound = ' + BoolToStr(Loco_DiagramFound, True));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_CurrentDirection = ' + DirectionToStr(Loco_CurrentDirection));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_FixedDirection = ' + DirectionToStr(Loco_FixedDirection));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_FixedLengthInInches = ' + IntToStr(Loco_FixedLengthInInches));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_HasCabLights = ' + BoolToStr(Loco_HasCabLights, True));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_HomeArea = ' + AreaToStr(Loco_HomeArea));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LastLengthInInches = ' + IntToStr(Loco_LastLengthInInches));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LastLocation = ' + LocationToStr(Loco_LastLocation));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LastTC = ' + TrackCircuitToStr(Loco_LastTC));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LightingChipDown = ' + LocoChipToStr(Loco_LightingChipDown));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LightingChipDownIndex = ' + LocoChipToStr(Loco_LightingChipDownIndex));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LightingChipUp = ' + LocoChipToStr(Loco_LightingChipUp));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LightingChipUpIndex = ' + LocoChipToStr(Loco_LightingChipUpIndex));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LightingChipRecordForChip = ' + IntToStr(Loco_LightingChipRecordForChip));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LightsMsg = ' + Loco_LightsMsg);
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LightsOn = ' + BoolToStr(Loco_LightsOn, True));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LightsType = ' + LIghtsTypeToStr(Loco_LightsType));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LocoChipStr = ' + Loco_LocoChipStr);
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LocoClassStr = ' + Loco_LocoClassStr);
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LocoName = ' + Loco_LocoName);
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_LocoTypeStr = ' + Loco_LocoTypeStr);
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_MaximumSpeedInMPH = ' + MPHToStr(Loco_MaximumSpeedInMPH));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_NumberOfCarriages = ' + IntToStr(Loco_NumberOfCarriages));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_SaveDesiredLenzSpeed = ' + IntToStr(Loco_SaveDesiredLenzSpeed));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_SavedDirection = ' + DirectionToStr(Loco_SavedDirection));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_SpeedArray:');
            FOR I := 1 TO 12 DO
              WriteLn(TempOutputFile, Loco_LocoChipStr + ':   (' + IntToStr(I) + ') = ' + IntToStr(Loco_SpeedArray[I]));

            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed10 = ' + IntToStr(Loco_Speed10));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed20 = ' + IntToStr(Loco_Speed20));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed30 = ' + IntToStr(Loco_Speed30));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed40 = ' + IntToStr(Loco_Speed40));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed50 = ' + IntToStr(Loco_Speed50));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed60 = ' + IntToStr(Loco_Speed60));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed70 = ' + IntToStr(Loco_Speed70));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed80 = ' + IntToStr(Loco_Speed80));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed90 = ' + IntToStr(Loco_Speed90));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed100 = ' + IntToStr(Loco_Speed100));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed110 = ' + IntToStr(Loco_Speed110));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_Speed120 = ' + IntToStr(Loco_Speed120));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_SpeedByte = ' + IntToStr(Loco_SpeedByte));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_SpeedByteReadIn = ' + BoolToStr(Loco_SpeedByteReadIn, True));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_SpeedSettingsMissing = ' + BoolToStr(Loco_SpeedSettingsMissing, True));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_SpeedStepMode = ' + IntToStr(Loco_SpeedStepMode));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_SpeedString = ' + Loco_SpeedString);
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_TrainIndex = ' + IntToStr(Loco_TrainIndex));
            WriteLn(TempOutputFile, Loco_LocoChipStr + ': Loco_UseTrailingTrackCircuits = ' + BoolToStr(Loco_UseTrailingTrackCircuits, True));
          END;
        END; {WITH}

        Inc(L);
      END; {WHILE}

//      WriteLn(TempOutputFile, 'Train Records: ' + IfThen(AllTrains,
//                                                         '(All Trains)',
//                                                         IfThen(ActiveTrains,
//                                                                '(only active trains',
//                                                                IfThen(OnlyTrainsInDiagram,
//                                                                      '(only trains in the current diagram)'))));

      WriteLn(TempOutputFile, 'Train Records');

      T := 0;
      WHILE T <= High(Trains) DO BEGIN
        WITH Trains[T] DO BEGIN
          //IF AllTrains OR (OnlyTrainsInDiagram AND Train_DiagramFound) THEN BEGIN
            WriteLn(TempOutputFile);
            WriteLn(TempOutputFile, 'Train_LocoChip = ' + IntToStr(Train_LocoChip));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_DoubleHeaderLocoChip = ' + LocoChipToStr(Train_DoubleHeaderLocoChip));
            WriteLn(TempOutputFile, 'Train_LocoChipStr = ' + Train_LocoChipStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_DoubleHeaderLocoChipStr = ' + Train_DoubleHeaderLocoChipStr);
            IF Train_LocoIndex <> unknownTrainIndex THEN
              WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LocoIndex = ' + LocoIndexToStr(Train_LocoIndex))
            ELSE
              WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LocoIndex = Unknown Train Index');
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_DoubleHeaderLocoIndex = ' + LocoIndexToStr(Train_DoubleHeaderLocoIndex));

            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_AccelerationTimeInSeconds = ' + FloatToStr(Train_AccelerationTimeInSeconds));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_ActualNumStr (from loco record) = ' + Train_ActualNumStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_AtCurrentBufferStop = ' + BufferStopToStr(Train_AtCurrentBufferStop));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_AtCurrentSignal = ' + SignalToStr(Train_AtCurrentSignal));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_AtHiddenStationSignalAspectSignal = ' + SignalToStr(Train_AtHiddenStationSignalAspectSignal));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_BeingAdvanced = ' + BoolToStr(Train_BeingAdvanced, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_BeingAdvancedTC = ' + TrackCircuitToStr(Train_BeingAdvancedTC));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CabLightsAreOn = ' + BoolToStr(Train_CabLightsAreOn, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CabLightsHaveBeenOn = ' + BoolToStr(Train_CabLightsHaveBeenOn, True));
//            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_ControlledByState = ' + ControlledByStateToStr(Train_ControlledByState));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentArrivalTime = ' + TimeToHMSStr(Train_CurrentArrivalTime));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentBufferStop = ' + BufferStopToStr(Train_CurrentBufferStop));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentDirection (initially from loco record) = ' + DirectionToStr(Train_CurrentDirection));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentJourney = ' + IntToStr(Train_CurrentJourney));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentLengthInInches = ' + IntToStr(Train_CurrentLengthInInches));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentRoute = ' + RouteToStr(Train_CurrentRoute));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentSignal = ' + SignalToStr(Train_CurrentSignal));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentSourceLocation = ' + LocationToStr(Train_CurrentSourceLocation));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentSpeedInMPH = ' + MPHToStr(Train_CurrentSpeedInMPH));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentStatus = ' + TrainStatusToStr(Train_CurrentStatus));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_CurrentTC = ' + TrackCircuitToStr(Train_CurrentTC));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_Description (from loco record) = ' + Train_Description);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_DesiredSpeedInMPH = ' + MPHToStr(Train_DesiredSpeedInMPH));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_DiagramFound = ' + BoolToStr(Train_DiagramFound, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_DiagramsGridRowNums = ' + IntegerArrayToStr(Train_DiagramsGridRowNums));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_DistanceToCurrentSignalOrBufferStop = '
                                                                                                                   + FloatToStr(Train_DistanceToCurrentSignalOrBufferStop));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_DistanceToNextSignalButOneOrBufferStop = '
                                                                                                                + FloatToStr(Train_DistanceToNextSignalButOneOrBufferStop));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_DistanceToNextSignalOrBufferStop = ' + FloatToStr(Train_DistanceToNextSignalOrBufferStop));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_EmergencyRouteing = ' + BoolToStr(Train_EmergencyRouteing, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_ExtraPowerAdjustment = ' + IntToStr(Train_ExtraPowerAdjustment));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_FirstStationSpecifiedStartTime = ' + TimeToHMSStr(Train_FirstStationSpecifiedStartTime));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_FixedDirection (from loco record) = ' + DirectionToStr(Train_FixedDirection));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_FixedLengthInInches (from loco record) = ' + IntToStr(Train_FixedLengthInInches));

//            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_Functions:');
//            FOR I := 0 TO 12 DO
//              WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') = ' + BoolToStr(Train_Functions[I], True));
//
//            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_Functions0To4Byte = ' + IntToStr(Train_Functions0To4Byte));
//            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_Functions5To12Byte = ' + IntToStr(Train_Functions5To12Byte));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_GradientSpeedAdjustment = ' + IntToStr(Train_GradientSpeedAdjustment));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_GradientSpeedAdjustmentMsgWritten = '
                                                                                                                + BoolToStr(Train_GradientSpeedAdjustmentMsgWritten, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_HasLights = ' + BoolToStr(Train_HasLights, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_Headcode = ' + Train_Headcode);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_InitialTrackCircuits:');
            FOR I := 1 TO 5 DO
              WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') = ' + TrackCircuitToStr(Train_InitialTrackCircuits[I]));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_InLightsOnTime = ' + BoolToStr(Train_InLightsOnTime, True));

            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LastLengthInInches (from loco record) = ' + IntToStr(Train_LastLengthInInches));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LastLocation = ' + LocationToStr(Train_LastLocation));

            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LastMissingTC = ' + TrackCircuitToStr(Train_LastMissingTC));

            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LastRouteLockedMsgStr = ' + Train_LastRouteLockedMsgStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LastSignal = ' + SignalToStr(Train_LastSignal));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LastTC (from loco record) = ' + TrackCircuitToStr(Train_LastTC));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LightsOnTime = ' + TimeToHMSStr(Train_LightsOnTime));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LightsOn = ' + BoolToStr(Train_LightsOn, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LightsRemainOnWhenJourneysComplete = '
                                                                                                               + BoolToStr(Train_LightsRemainOnWhenJourneysComplete, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LocatedAtStartup = ' + BoolToStr(Train_LocatedAtStartup, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_Locations:');
            FOR I := 0 TO High(Train_Locations) DO
              WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') = ' + LocationToStr(Train_Locations[I]));

            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LocoChipStr (from loco record) = ' + Train_LocoChipStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LocoClassStr (from loco record) = ' + Train_LocoClassStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LocoName (from loco record) = ' + Train_LocoName);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_LocoTypeStr (from loco record) = ' + Train_LocoTypeStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_MaximumSpeedInMPH (from loco records) = ' + MPHToStr(Train_MaximumSpeedInMPH));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_MinimumAccelerationTimeInSeconds = ' + IntToStr(Train_MinimumAccelerationTimeInSeconds));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_MissingMessage = ' + BoolToStr(Train_MissingMessage, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_MissingNum = ' + IntToStr(Train_MissingNum));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_NextTC = ' + TrackCircuitToStr(Train_NextTC));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_NextButOneTC = ' + TrackCircuitToStr(Train_NextButOneTC));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_NotInPlaceMsgWritten = ' + BoolToStr(Train_NotInPlaceMsgWritten, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_NotLocatedAtStartupMsgWritten = ' + BoolToStr(Train_NotLocatedAtStartupMsgWritten, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_NumberOfCarriages (from loco record) = ' + IntToStr(Train_NumberOfCarriages));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_PossibleRerouteTime = ' + TimeToHMSStr(Train_PossibleRerouteTime));
//            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_PreviouslyControlledByState = ' + ControlledByStateToStr(Train_PreviouslyControlledByState));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_PreviousStatus = ' + TrainStatusToStr(Train_PreviousStatus));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_PreviousTC = ' + TrackCircuitToStr(Train_PreviousTC));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_Reversing = ' + BoolToStr(Train_Reversing, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_ReversingDelayInSeconds = ' + IntToStr(Train_ReversingDelayInSeconds));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_ReversingStartTime = ' + TimeToHMSStr(Train_ReversingStartTime));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_ReversingWaitStarted = ' + BoolToStr(Train_ReversingWaitStarted, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_RouteCheckedTime = ' + TimeToHMSStr(Train_RouteCheckedTime));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_RouteCreationHeldJourney = ' + JourneyToStr(Train_RouteCreationHeldJourney));

            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_RouteCreationHeldMsgWrittenArray:');
            FOR I := FirstRouteCreationHeldMsgNumber TO LastRouteCreationHeldMsgNumber DO
              WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') = ' + BoolToStr(Train_RouteCreationHeldMsgWrittenArray[I], True));

            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_RouteCreationHoldNum = ' + IntToStr(Train_RouteCreationHoldNum));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_RouteCreationHoldMsg = ' + Train_RouteCreationHoldMsg);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_RouteCreationPlatformHeldStr = ' + Train_RouteCreationPlatformHeldStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_RouteCreationReleasedMsg = ' + Train_RouteCreationReleasedMsg);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_RouteingHeldAtSignal = ' + SignalToStr(Train_RouteingHeldAtSignal));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SaveCurrentTC = ' + TrackCircuitToStr(Train_SaveCurrentTC));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SavedLocation = ' + LocationToStr(Train_SavedLocation));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SavedRoute = ' + RouteToStr(Train_SavedRoute));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SaveSpeedInFiddleyardMsg = ' + Train_SaveSpeedInFiddleyardMsg);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SaveTCsClearedStr = ' + Train_SaveTCsClearedStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SaveTCsForReleaseStr = ' + Train_SaveTCsForReleaseStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SaveTCsOccupiedStr = ' + Train_SaveTCsOccupiedStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SectionStartTime = ' + TimeToHMSStr(Train_SectionStartTime));

//            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SpeedByte = ' + IntToStr(Train_SpeedByte));
//            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SpeedByteReadIn = ' + BoolToStr(Train_SpeedByteReadIn, True));
//            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SpeedSettingsMissing = ' + BoolToStr(Train_SpeedSettingsMissing, True));
//            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SpeedStepMode = ' + IntToStr(Train_SpeedStepMode));
//            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SpeedString = ' + Train_SpeedString);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_StalledMsgWritten = ' + BoolToStr(Train_StalledMsgWritten, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_SubRouteAheadCheckedTime = ' + TimeToHMSStr(Train_SubRouteAheadCheckedTime));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TakenOverByUserMsgWritten = ' + BoolToStr(Train_TakenOverByUserMsgWritten, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TCsAndSignalsNotClearedArray = ' + StringArrayToStr(Train_TCsAndSignalsNotClearedArray));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TCsAndSignalsNotClearedStr = ' + Train_TCsAndSignalsNotClearedStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TCsNotClearedArray = ' + StringArrayToStr(Train_TCsNotClearedArray));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TCsNotClearedStr = ' + Train_TCsNotClearedStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TCsOccupiedOrClearedArray = ' + StringArrayToStr(Train_TCsOccupiedOrClearedArray));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TCsOccupiedOrClearedStr = ' + Train_TCsOccupiedOrClearedStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TCsReleasedArray = ' + StringArrayToStr(Train_TCsReleasedArray));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TCsReleasedStr = ' + Train_TCsReleasedStr);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TempDraftRouteArray = ' + StringArrayToStr(Train_TempDraftRouteArray));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TempLockingArray = ' + StringArrayToStr(Train_TempLockingArray));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TerminatingSpeedReductionMsgWritten = '
                                                                                                               + BoolToStr(Train_TerminatingSpeedReductionMsgWritten, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TotalJourneys = ' + IntToStr(Train_TotalJourneys));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_Type = ' + TrainTypeNumToStr(TrainTypeToTrainTypeNum(Train_Type)));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_TypeNum = ' + IntToStr(Train_TypeNum));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_UserDriving = ' + BoolToStr(Train_UserDriving, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_UserPowerAdjustment = ' + IntToStr(Train_UserPowerAdjustment));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_UserRequiresInstructions = ' + BoolToStr(Train_UserRequiresInstructions, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_UserSpeedInstructionMsg = ' + Train_UserSpeedInstructionMsg);
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_UseTrailingTrackCircuits (from loco record) = '
                                                                                                                         + BoolToStr(Train_UseTrailingTrackCircuits, True));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_WaitingForHiddenStationSignalAspectStartTime = '
                                                                                                              + TimeToHMSStr(Train_WaitingForHiddenStationSignalAspectStartTime));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_WorkingTimetableLastArrivalArea = ' + AreaToStr(Train_WorkingTimetableLastArrivalArea));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_WorkingTimetableLastArrivalTime = ' + TimeToHMSStr(Train_WorkingTimetableLastArrivalTime));
            WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_WorkingTimetableLastEntryNumStr = ' + Train_WorkingTimetableLastEntryNumStr);

            { Deal with train journeys separately as they are held in a separate record }
            IF Length(Train_JourneysArray) = 0 THEN BEGIN
              WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_JourneysArray (no data)');
              WriteLn(TempOutputFile);
            END ELSE BEGIN
              WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ': Train_JourneysArray:');
              FOR I := 0 TO High(Train_JourneysArray) DO BEGIN
                WITH Train_JourneysArray[I] DO BEGIN
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_ActualArrivalTime = '
                                                                                                                            + TimeToHMSStr(TrainJourney_ActualArrivalTime));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_ActualDepartureTime = '
                                                                                                                          + TimeToHMSStr(TrainJourney_ActualDepartureTime));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_AdditionalRequiredStationWaitInMinutes = '
                                                                                                           + IntToStr(TrainJourney_AdditionalRequiredStationWaitInMinutes));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_Cleared = ' + BoolToStr(TrainJourney_Cleared));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_Created = ' + BoolToStr(TrainJourney_Created));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_CurrentArrivalTime = '
                                                                                                                           + TimeToHMSStr(TrainJourney_CurrentArrivalTime));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_CurrentDepartureTime = '
                                                                                                                         + TimeToHMSStr(TrainJourney_CurrentDepartureTime));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_DiagrammedArrivalTime = '
                                                                                                                        + TimeToHMSStr(TrainJourney_DiagrammedArrivalTime));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_DiagrammedDepartureTime = '
                                                                                                                      + TimeToHMSStr(TrainJourney_DiagrammedDepartureTime));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_DiagrammedStartLocation = '
                                                                                                                     + LocationToStr(TrainJourney_DiagrammedStartLocation));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_DiagrammedEndLocation = '
                                                                                                                       + LocationToStr(TrainJourney_DiagrammedEndLocation));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_Direction = ' + DirectionToStr(TrainJourney_Direction));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_DurationInMinutes = '
                                                                                                                                + IntToStr(TrainJourney_DurationInMinutes));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_EndArea = ' + AreaToStr(TrainJourney_EndArea));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_EndBufferStop = '
                                                                                                                             + BufferStopToStr(TrainJourney_EndBufferStop));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_EndLine = ' + LineToStr(TrainJourney_EndLine));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_EndLocation = '
                                                                                                                                 + LocationToStr(TrainJourney_EndLocation));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_EndStationName = ' + TrainJourney_EndStationName);
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_EndSignal = ' + SignalToStr(TrainJourney_EndSignal));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_FirstTC = ' + IntToStr(TrainJourney_FirstTC));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_LengthInInches = '
                                                                                                                                 + FloatToStr(TrainJourney_LengthInInches));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_LocationsPending = '
                                                                                                                                + BoolToStr(TrainJourney_LocationsPending));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_LockingArray = '
                                                                                                                             + StringArrayToStr(TrainJourney_LockingArray));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_NotForPublicUse = '
                                                                                                                           + BoolToStr(TrainJourney_NotForPublicUse, True));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_Route = ' + RouteToStr(TrainJourney_Route));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_RouteArray = '
                                                                                                                               + StringArrayToStr(TrainJourney_RouteArray));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_SetUp = ' + BoolToStr(TrainJourney_SetUp, True));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_StartArea = ' + AreaToStr(TrainJourney_StartArea));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_StartLine = ' + LineToStr(TrainJourney_StartLine));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_StartLocation = '
                                                                                                                               + LocationToStr(TrainJourney_StartLocation));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_StartStationName = ' + TrainJourney_StartStationName);
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_StartOfRepeatJourney ='
                                                                                                                      + BoolToStr(TrainJourney_StartOfRepeatJourney, True));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_StartSignal = ' + SignalToStr(TrainJourney_StartSignal));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_StoppingOnArrival = '
                                                                                                                         + BoolToStr(TrainJourney_StoppingOnArrival, True));
                  WriteLn(TempOutputFile, LocoChipToStr(Train_LocoChip) + ':   (' + IntToStr(I) + ') TrainJourney_UserToDrive = '
                                                                                                                               + BoolToStr(TrainJourney_UserToDrive, True));
                  WriteLn(TempOutputFile);
                END; {WITH}
              END; {FOR}
            END;
          //END;
        END; {WITH}

        Inc(T);
      END; {WHILE}

      { Lights To Be Switched On Array }
      WriteLn(TempOutputFile, 'LightsToBeSwitchedOnArray : ' + IfThen(Length(LightsToBeSwitchedOnArray) = 0,
                                                                      '(no data)',
                                                                      '(' + IntToStr(Length(LightsToBeSwitchedOnArray)) + ' records)'));
      FOR I := 0 TO High(LightsToBeSwitchedOnArray) DO BEGIN
        WITH LightsToBeSwitchedOnArray[I] DO BEGIN
          WriteLn(TempOutputFile, IntToStr(I) + ' LightsToBeSwitchedOn_Train = ' + LocoChipToStr(Trains[LightsToBeSwitchedOn_Train].Train_LocoChip));
          WriteLn(TempOutputFile, IntToStr(I) + ' LightsToBeSwitchedOn_ColourStr1 = ' + LightsToBeSwitchedOn_ColourStr1);
          WriteLn(TempOutputFile, IntToStr(I) + ' LightsToBeSwitchedOn_ColourStr2 = ' + LightsToBeSwitchedOn_ColourStr2);
          WriteLn(TempOutputFile, IntToStr(I) + ' LightsToBeSwitchedOn_Direction1 = ' + DirectionToStr(LightsToBeSwitchedOn_Direction1));
          WriteLn(TempOutputFile, IntToStr(I) + ' LightsToBeSwitchedOn_Direction2 = ' + DirectionToStr(LightsToBeSwitchedOn_Direction2));
          WriteLn(TempOutputFile, IntToStr(I) + ' LightsToBeSwitchedOn_SwitchOnTime = ' + TimeToHMSStr(LightsToBeSwitchedOn_SwitchOnTime));
          WriteLn(TempOutputFile);
        END; {WITH}
      END; {FOR}
      WriteLn(TempOutputFile);




    END;
    Log('XG Concluding Write of all variable data to file');
    CloseFile(TempOutputFile);
  EXCEPT
    ON E : Exception DO
      Log('EG WriteVariableDataToFile:' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { WriteLnVariableDataToFile }

PROCEDURE WriteNextLineDetailToDebugWindow(L : Integer; HelpRequired : Boolean);
{ Indicate what the next line status is for the given line }
BEGIN
  IF HelpRequired THEN BEGIN
    AddRichLine(HelpWindow.HelpRichEdit, '');
    AddRichLine(HelpWindow.HelpRichEdit, '<B>Debugging</B>');
    AddRichLine(HelpWindow.HelpRichEdit, '  <B>Shift + Right Mouse over Line</B> - Write line detail to the Debug Window');
  END ELSE BEGIN
    WITH Lines[L] DO BEGIN
      Debug('L=' + LineToStr(L));
      Debug('NextUpType=' + NextLineRouteingToStr(Line_NextUpType));
      Debug('NextDownType=' + NextLineRouteingToStr(Line_NextDownType));
      Debug('NextUpIsEndOfLine=' + EndOfLineToStr(Line_NextUpIsEndOfLine));
      Debug('NextDownIsEndOfLine=' + EndOfLineToStr(Line_NextDownIsEndOfLine));
      Debug('EndOfLineMarker=' + EndOfLineToStr(Line_EndOfLineMarker));
      Debug('NextUpLine=' + LineToStr(Line_NextUpLine) + ' (' + IntToStr(Line_NextUpLine) + ')');
      Debug('NextDownLine=' + LineToStr(Line_NextDownLine) + ' (' + IntToStr(Line_NextDownLine) + ')');
      Debug('NextUpPoint=' + IntToStr(Line_NextUpPoint));
      Debug('NextDownPoint=' + IntToStr(Line_NextDownPoint));
    END; {WITH}
  END;
END; { WriteNextLineDetailToDebugWindow }

(*
PROCEDURE TfrmMain.WMQueryEndSession(var Message : TWMQueryEndSession);
begin

 // Let the inherited message handler respond first
 inherited;

 if DataHasChanged then begin
    MessageMakeSound(1)(MB_ICONQUESTION);
    CASE MessageDlg('The current Windows
           session is ending.  Save league changes?',
           mtConfirmation, [mbYes,mbNo,mbCancel],0)
    OF
      mrYes : begin
                //Your data-saving code or method
                //call goes here
                Message.Result := 1;
               end;
      mrNo : Message.Result := 1;
      mrCancel : Message.Result := 0;
    end; {case}
  end else
      Message.Result := 1;
end;
*)

(*
Borland Developer Support


Different colored characters in a string grid - by Borland Developer Support Staff

Ratings: be the first! Not Rated Rate It

 Technical Information Database

TI1045D.txt  Different colored characters in a string grid
Category :General Programming
Platform :All
Product :Delphi All

Description:

This unit will show how to have text in a string grid where the
characters are different colors.

unit Strgr;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Grids, StdCtrls, DB;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    PROCEDURE StringGrid1DrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

PROCEDURE TForm1.StringGrid1DrawCell(Sender: TObject; Col, Row: Longint;
  Rect: TRect; State: TGridDrawState);
const
  CharOffset = 3;
begin
  with StringGrid1.canvas do
  begin
    font.color := clMaroon;
    textout(rect.left + CharOffset, rect.top + CharOffset, 'L');
      font.color := clNavy;
    textout(rect.left + CharOffset + TextWidth('L'),
      rect.top + CharOffset, 'loyd');
  end;
*)
(*
Text Alignment in StringGrid

 USES
    Classes;    // TAlignment

// Use half-width of 'X' as left margin
FUNCTION XLeft (rect: TRect; canvas: TCanvas; s: String) : INTEGER;
BEGIN
  RESULT := rect.Left + canvas.TextWidth('X') DIV 2
END {XRight};

FUNCTION XCenter (rect: TRect; canvas: TCanvas; s: String) : INTEGER;
BEGIN
  RESULT := ((rect.Left + rect.Right) - canvas.TextWidth(s)) DIV 2
END {XCenter};

// Use half-width of 'X' as right margin
FUNCTION XRight (rect: TRect; canvas: TCanvas; s: String) : INTEGER;
BEGIN
  RESULT := rect.Right - canvas.TextWidth(s) - canvas.TextWidth('X') DIV 2
END {XRight};

// Top of text is its origin, so adjust by half-height of text to center
FUNCTION YCenter (rect: TRect; canvas: TCanvas; s: String) : INTEGER;
BEGIN
  RESULT := ((rect.Top + rect.Bottom) - canvas.TextHeight(s)) DIV 2
END { YCenter };

PROCEDURE AlignText(CONST Canvas: TCanvas; CONST Rect: TRect;
                    CONST alignment: TAlignment; CONST s: String);
BEGIN
  CASE alignment OF
    taLeftJustify: Canvas.TextRect(Rect,
                                   XLeft(Rect, Canvas, s),
                                   YCenter(Rect, Canvas, s),
                                   s);
    taCenter: Canvas.TextRect(Rect,
                              XCenter(Rect, Canvas, s),
                              YCenter(Rect, Canvas, s),
                              s);

    taRightJustify: Canvas.TextRect(Rect,
                                    XRight(Rect, Canvas, s),
                                    YCenter(Rect, Canvas, s),
                                    s);
  END;
END { AlignText };

...

CONST
  ColumnAlignment: ARRAY [0..7] OF TAlignment =
    (taLeftJustify, taLeftJustify, taLeftJustify, taLeftJustify,
     taLeftJustify, taCenter, taCenter, taCenter);

...

procedure TFormList.StringGridListDrawCell(Sender: TObject;
  Col, Row: Longint; Rect: TRect; State: TGridDrawState);

  VAR
    s : String;
begin
  s := StringGridList.Cells[Col, Row];

  StringGridList.Canvas.Font.Color := clBlack;

  IF Col < StringGridList.FixedCols
  THEN StringGridList.Canvas.Brush.Color := clBtnFace
  ELSE
    IF Row < StringGridList.FixedRows
    THEN StringGridPatientList.Canvas.Brush.Color := clBtnFace
    ELSE
      IF (Row >= StringGridList.Selection.Top) AND
         (Row <= StringGridList.Selection.Bottom)
      THEN StringGridList.Canvas.Brush.Color := clYellow
      ELSE StringGridList.Canvas.Brush.Color := clWindow;

  StringGridList.Canvas.FillRect(Rect);

  IF Row = 0
  THEN AlignText(StringGridList.Canvas, Rect, taCenter, s)
  ELSE AlignText(StringGridList.Canvas, Rect, ColumnAlignment[Col], s)
end;
*)
(*
var
myArray: TStringArray;
...//delete fifth element
DeleteArrayItem(myArray, 5);

---------------------------
type
   TStringArray = array of String;

PROCEDURE DeleteArrayItem(var X: TStringArray; const Index: Integer);
begin
   if Index > High(X) then Exit;
   if Index < Low(X) then Exit;
   if Index = High(X) then
   begin
     SetLength(X, Length(X) - 1);
     Exit;
   end;
   Finalize(X[Index]);
   System.Move(X[Index +1], X[Index],(Length(X) - Index -1) * SizeOf(String) + 1);
   SetLength(X, Length(X) - 1);
end;
*)
PROCEDURE TDebugWindow.DebugWindowHide(Sender: TObject);
{ Un-check the window menu item }
BEGIN
  FWPRailWindow.MainOperationsMenuDebugOptions.Checked := False;
END; { DebugWindowHide }

PROCEDURE TDebugWindow.DebugWindowShow(Sender: TObject);
{ Check the window menu item }
BEGIN
  FWPRailWindow.MainOperationsMenuDebugOptions.Checked := True;
END; { TDebugWindow }

FUNCTION TimeIsValid(TimeStr : String) : Boolean;
{ Checking a given time string - an hour/min separator is permissible. Also ignore a trailing asterisk }
VAR
  ErrorCode : Integer;
  Hour, Min, Sec, MSec : Word;
  OK : Boolean;

BEGIN
  OK := True;

  Hour := 0;
  Min := 0;
  Sec := 0;
  MSec := 0;

  { Remove any trailing asterisks - there to advise that time string is system created }
  IF RightStr(TimeStr, 1) = '*' THEN
    TimeStr := Copy(TimeStr, 1, Length(TimeStr) - 1);

  IF (Length(TimeStr) <> 5) AND (Length(TimeStr) <> 8) THEN
    OK := False;

  IF OK THEN BEGIN
    Val(Copy(TimeStr, 1, 2), Hour, ErrorCode);
    IF ErrorCode <> 0 THEN
      OK := False;
  END;

  IF OK THEN BEGIN
    Val(Copy(TimeStr, 4, 2), Min, ErrorCode);
    IF ErrorCode <> 0 THEN
      OK := False;
  END;

  IF OK THEN BEGIN
    IF Length(TimeStr) = 8 THEN BEGIN
      Val(Copy(TimeStr, 7, 2), Sec, ErrorCode);
      IF ErrorCode <> 0 THEN
        OK := False;
    END;
  END;

  IF OK THEN
    OK := IsValidTime(Hour, Min, Sec, MSec);

  IF NOT OK THEN
    Debug('Time string "' + TimeStr + '" invalid');

  Result := OK;
END; { TimeIsValid }

PROCEDURE InitialiseMiscUtilsUnit;
{ Initialises the unit }
VAR
  I : Integer;

BEGIN
  { Now see if there are any messages waiting for the window to be created before they can be written on it }
  IF Length(DebugWindowLines) > 0 THEN
    FOR I := 0 TO High(DebugWindowLines) DO
      Debug(DebugWindowLines[I]);

  DrawDebugWindow;
END; { InitialiseMiscUtilsUnit }

PROCEDURE TDebugWindow.DebugWindowClose(Sender: TObject; VAR Action: TCloseAction);
{ Hides but does not close the window }
BEGIN
  Action := caHide;
END; { DebugWindowClose }

PROCEDURE TDebugWindow.PopupDebugWindowCopyClick(Sender: TObject);
BEGIN
  DebugRichEdit.CopyToClipboard;

  { and hide the selection }
  DebugRichEdit.SelStart := 0;
END; { PopupDebugWindowCopyClick }

PROCEDURE ResetDebugWindowSizeAndPosition;
{ Reset the window's size and position }
BEGIN
  DebugWindowHeight := DefaultDebugWindowHeight;
  DebugWindowWidth := DefaultDebugWindowWidth;
  DebugWindowTop := DefaultDebugWindowTop;
  DebugWindowLeft := DefaultDebugWindowLeft;

  DrawDebugWindow;
END; { ResetDebugWindowSizeAndPosition }

PROCEDURE TDebugWindow.PopupDebugWindowResetSizeAndPositionClick(Sender: TObject);
BEGIN
  ResetDebugWindowSizeAndPosition;
END; { PopupDebugWindowResetSizeAndPositionClick }

PROCEDURE TDebugWindow.DebugWindowResize(Sender: TObject);
BEGIN
  { Enable or disable the popup menu item allowing us to return the window to its default size }
  IF (DebugWindow.Height <> DefaultDebugWindowHeight)
  OR (DebugWindow.Width <> DefaultDebugWindowWidth)
  OR (DebugWindow.Top <> DefaultDebugWindowTop)
  OR (DebugWindow.Left <> DefaultDebugWindowLeft)
  THEN
    PopupDebugWindowResetSizeAndPosition.Enabled := True
  ELSE
    PopupDebugWindowResetSizeAndPosition.Enabled := False;
END; { DebugWindowResize }

PROCEDURE TDebugWindow.DebugRichEditKeyDown(Sender: TObject; VAR Key: Word; ShiftState: TShiftState);
BEGIN
  CASE Key OF
    { Exclude most non-alphanumeric keys }
    vk_Shift, vk_Control, vk_Menu { Alt }, vk_Pause, vk_SnapShot { PrtSc }, vk_LWin, vk_RWin, vk_Apps { Windows Applications }, vk_Numlock, vk_Scroll,
    vk_Cancel { Ctrl-Break}, vk_Capital { Caps Lock }, vk_Back { Backspace }:
      { do nothing };
  ELSE
    IF (Key = Ord('C')) AND (ssCtrl IN ShiftState) THEN BEGIN
      DebugRichEdit.CopyToClipboard;

      { and hide the selection }
      DebugRichEdit.SelStart := 0;
    END ELSE
      KeyPressedDown(Key, ShiftState);
  END; {CASE}
END; { DebugRichEditKeyDown }

PROCEDURE TDebugWindow.DebugRichEditMouseDown(Sender: TObject; Button: TMouseButton; ShiftState: TShiftState; X, Y: Integer);
{ Used to change the memo's colours }
BEGIN
  IF LocoDialogueWindow.Visible AND (GetLocoDialogueLocoSpeed > 0) AND (Button = mbRight) THEN BEGIN
    CheckEmergencyStop(Button, ShiftState);
    Exit;
  END ELSE
    IF (Button = mbRight) AND (ssShift IN ShiftState) THEN BEGIN
      { Show the default }
      DebugRichEditColourDialogue.Color := DebugRichEdit.Color;
      { Allow the user to change it }
      IF DebugRichEditColourDialogue.Execute THEN
        DebugRichEdit.Color := DebugRichEditColourDialogue.Color;
    END;
END; { DebugRichEditMouseDown }

PROCEDURE TDebugWindow.DebugRichEditMouseMove(Sender: TObject; ShiftState: TShiftState; X, Y: Integer);
{ If the mouse moves into the debug window, move the focus there }
BEGIN
  IF AutomaticallySetFocusWhenInDebugWindow AND NOT KeyboardAndMouseLocked AND (DebugWindow <> NIL) THEN BEGIN
    IF NOT DebugWindow.Active THEN BEGIN
      DebugWindow.SetFocus;
      ChangeCursor(crIBeam);
    END;
  END;
END; { DebugRichEditMouseMove }

PROCEDURE TDebugWindow.DebugRichEditPopupMenuOnPopup(Sender: TObject);
BEGIN
  IF DebugWindow.Top <> DefaultDebugWindowTop THEN
    PopupDebugWindowResetSizeAndPosition.Enabled := True
  ELSE
    PopupDebugWindowResetSizeAndPosition.Enabled := False;
END; { DebugRichEditPopupMenuOnPopup }

VAR
  TempIntegerArray : IntegerArrayType;
  TempTimeArray : DateTimeArrayType;

INITIALIZATION

  FOR InitialisationCount := 1 TO MaxSaveLogStrs DO
    SaveLogStrArray[InitialisationCount] := '';
  SetLength(LogArray, 0);

  { These are here to stop the linker eliminating these functions, which I usually only use when debugging using Watches }
  TTS(Time);
  TTSS(Time);
  TTSZ(Time);
  ATS(1);
  LTS(1);
  LocTS(1);

  AppendToIntegerArray(TempIntegerArray, 1);
  ATSA(TempIntegerArray);
  LATS(TempIntegerArray);
  LocATS(TempIntegerArray);

  AppendToDateTimeArray(TempTimeArray, StrToTime('08:00'));
  TTSA(TempTimeArray);
END { MiscUtils }.
