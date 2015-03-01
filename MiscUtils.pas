UNIT MiscUtils;

INTERFACE

USES Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, InitVars, Input, Menus, ComCtrls, System.UITypes, TLHelp32, Logging,
     PointsUnit, TrackCircuitsUnit, LinesUnit, Train, Feedback, LocationsUnit, SignalsUnit;

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
    PROCEDURE DebugWindowClose(Sender: TObject; VAR Action: TCloseAction);
    PROCEDURE DebugWindowResize(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  DebugWindow: TDebugWindow;

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

FUNCTION CardinalMulDiv(A, B, C : Cardinal) : Cardinal;
{ Returns (A * B) / C; intermediate result is held double-length to avoid overflow. FWP version }

PROCEDURE CloseInputOrOutputFile(VAR InputOrOutputFile : Text; Filename : String);
{ Close a file, capturing the error message if any }

FUNCTION CountSubRoutes(RouteArray : StringArrayType) : Integer;
{ Return how many subroutes there are in a given route array }

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

FUNCTION DistanceBetween(X1, Y1, X2, Y2 : Integer) : Extended;
{ Returns the Euclidean distance between (X1, Y1) and (X2, Y2) }

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

FUNCTION FindButton(CONST Dlg: TForm; CONST ModalResult: Integer): TButton;
{ FindButton finds a button in a Message Dialog that has been created with the Dialogs.CreateMessageDialog function, based on its ModalResult. The function returns the
  button, or NIL, if the button has not been found [from uDialogsExt from Borland website]
}
FUNCTION GetElementPosInIntegerArray(IntegerArray : IntegerArrayType; Element : Integer) : Integer;
{ Returns where, if at all, the given array element is found in an integer array }

FUNCTION GetLineTypeColour(T : TypeOfLine) : TColour;
{ Return the colour for a specific line type }

FUNCTION GetMidPos(I1, I2 : Integer) : Integer;
{ Return the mid position between two given values }

FUNCTION GetNextDayOfTheWeek(DayOfTheWeek : DayOfTheWeekType) : DayOfTheWeekType;
{ Return the next day of the week to the one given }

FUNCTION GetOrdinalFromCardinal(Ordinal : Integer) : String;
{ Return the ordinal version of a cardinal number }

FUNCTION GetExtendedDataFromUser(Prompt, DefaultStr : String; LowerBound, UpperBound : Extended; BoundsErrorMsg : String; OUT NewValue : Extended) : Boolean;
{ Makes sure an extended value is returned by the user, and optionally that it is within the bounds set }

FUNCTION GetIntegerDataFromUser(Prompt, DefaultStr : String; LowerBound, UpperBound : Integer; BoundsErrorMsg : String; OUT NewValue : Integer) : Boolean;
{ Makes sure an integer is returned by the user, and optionally that it is within the bounds set }

FUNCTION GetFollowingChars(LineStr : String; Str : String; EndStr : String) : String;
{ Return the characters after the given characters up to the delimiter supplied (or "CRLF" if it's at a line end) }

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

FUNCTION IsProgramRunning(ProgramName : String) : Boolean;
{ Checks to see if a given program is running }

PROCEDURE MakeSound(SoundNum : Integer);
{ Make a warning sound }

FUNCTION MessageDialogueWithDefault{1}(DialogueText: String; StopTimer : Boolean; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons; DefaultButton : TMsgDlgBtn) : Word;
                                       Overload;
{ Adapted from the Borland Delphi web site example - uses procedures from their uDialogsExt Unit. This version has a default button. }

FUNCTION MessageDialogueWithDefault{2}(DialogueText: String; StopTimer : Boolean; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons; ButtonText : ARRAY OF String;
                                       DefaultButton : TMsgDlgBtn) : Word; Overload;
{ Adapted from the Borland Delphi web site example - uses procedures from their uDialogsExt Unit. This version has replacement button text as well as a default button }

FUNCTION MulDiv(A, B, C : Integer) : Integer;
{ Returns (A * B) / C; intermediate result is held double-length to avoid overflow }

FUNCTION NextButton{1}(CONST Dlg: TForm; VAR Context: Integer): TButton; Overload;
{ The NextButton can be used to traverse the buttons in a Message Dialogue that has been created with the Dialogs.CreateMessageDialogue function. Initialize the Context
  variable to 0 to get the first button. The routine updates the Context variable to cause the next call to NextButton to return the next button. If there are no (more)
  buttons, the function returns NIL [from uDialogsExt from Borland website]
}
FUNCTION NextButton{2}(CONST Dlg: TForm; VAR Context: Integer; OUT Button: TButton): Boolean; Overload;
{ This variant of NextButton returns the button in the Button variable. The function result indicates whether a button has been found [from uDialogsExt from Borland
  website]
}
FUNCTION OpenInputFileOK(VAR InputFilename : Text; Filename : String; OUT ErrorMsg : String) : Boolean;
{ Open a existing file to read from }

FUNCTION OpenOutputFileOK(VAR OutputFilename : Text; Filename : String; OUT ErrorMsg : String; AppendToFile : Boolean) : Boolean;
{ Open (and create if necessary) a file }

FUNCTION OppositeDirection(Dir : DirectionType) : DirectionType;
{ Return the direction opposite to the one given }

PROCEDURE Pause(MilliSeconds : Cardinal; ProcessMessages : Boolean);
{ Pause for a given number of milliseconds }

FUNCTION PointInPolygon(CONST Polygon: ARRAY OF TPoint; Point: TPoint): Boolean;
{ Returns true if a point is in a defined region }

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

FUNCTION StringArraysCompareOK(FirstArray, SecondArray : StringArrayType; OUT ErrorMsg : String) : Boolean;
{ Does an element by element comparison of two string arrays }

FUNCTION TestCount : Integer;
{ Returns a number which is incremented each time the routine is called - used for debugging }

FUNCTION TestCountStr : String;
{ Returns as a string a number which is incremented each time the routine is called - used for debugging }

FUNCTION TrainTypeToTrainTypeNum(TrainType : TypeOfTrainType) : Integer;
{ Returns the number for the kind of train it is; an up-to-date list as of 5/10/05 }

FUNCTION TrainTypeNumToTrainType(TrainTypeNum : Integer) : TypeOfTrainType;
{ Returns the train type; an up-to-date list as of 5/10/05 }

PROCEDURE WriteVariableDataToFile;
{ List some variable data to a given file }

VAR
  TestingMode : Boolean = False;

IMPLEMENTATION

{$R *.dfm}

USES GetTime, Lenz, Diagrams, RailDraw, Types, LocoUtils, Math {sic}, IDGlobal, StrUtils, RDCUnit, CreateRoute, IniFiles, DateUtils, Startup, Cuneo, Movement, LocoDialogue,
     FWPShowMessageUnit, Options, Help, MMSystem, TCPIP, Main, StationMonitors, Edit, Locks, Route;

CONST
  UnitRef = 'MiscUtils';
  DebugWindowCaption = '&Debug Window';

VAR
  AllRouteDebuggingMode : Boolean = False;
  AnonymousOccupationMode : Boolean = True;
  DebuggingMode : Boolean = False;
  FeedbackDebuggingMode : Boolean = False;
  LastSoundTime : TDateTime = 0;
  LineDebuggingMode : Boolean = False;
  LockDebuggingMode : Boolean = False;
  LockingMode : Boolean = True;
  LocoSpeedTimingMode : Boolean = False;
  LogsCurrentlyKeptMode : Boolean = True;
  PointDebuggingMode : Boolean = False;
  RDCMode : Boolean = False;
  RecordingMonitorScreensMode : Boolean = False;
  RecordLineDrawingMode : Boolean = False;
  RouteBacktrackDebuggingMode : Boolean = False;
  RouteDebuggingMode : Boolean = False;
  RouteDrawingMode : Boolean = False;
  ShowAdjacentTrackCircuitMode : Boolean = False;
  StationStartMode : Boolean;
  TempSaveLogStr : String = '';
  TestCounter : Integer = 0; { used to test iterations in debugging }
  UserDebugText : String = '';

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION GetMidPos(I1, I2 : Integer) : Integer;
{ Return the mid position between two given values }
BEGIN
  IF I1 < I2 THEN
    Result := I1 + ((I2 - I1) DIV 2)
  ELSE
    Result := I2 + ((I1 - I2) DIV 2);
END; { GetMidPos }

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

FUNCTION CardinalMulDiv(A, B, C : Cardinal) : Cardinal;
{ Returns (A * B) / C; intermediate result is held double-length to avoid overflow }
VAR
  X : Cardinal;

BEGIN
  X := (A * B);
  Result := X DIV C;
END; { CardinalMulDiv }

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

FUNCTION DistanceBetween(X1, Y1, X2, Y2 : Integer) : Extended;
{ Returns the Euclidean distance between (X1, Y1) and (X2, Y2) }
BEGIN
  Result := Sqrt(Power(X1 - X2, 2) + Power(Y1 - Y2, 2));
END; { DistanceBetween }

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

FUNCTION GetExtendedDataFromUser(Prompt, DefaultStr : String; LowerBound, UpperBound : Extended; BoundsErrorMsg : String; OUT NewValue : Extended) : Boolean;
{ Makes sure an extended value is returned by the user, and optionally that it is within the bounds set }
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

FUNCTION GetLineTypeColour(T : TypeOfLine) : TColour;
{ Return the colour for a specific line type, unless the screen is set to be printed from, in which case return black }
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

FUNCTION MulDiv(A, B, C : Integer) : Integer;
{ Returns (A * B) / C; intermediate result is held double-length to avoid overflow }
VAR
  X : Integer;

BEGIN
  X := (A * B);
  Result := X DIV C;
END; { MulDiv }

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

FUNCTION PointInPolygon(CONST Polygon: ARRAY OF TPoint; Point: TPoint) : Boolean;
{ Returns true if a point is in a defined region }
VAR
  Rgn : HRGN;

BEGIN
  Rgn := CreatePolygonRgn(Polygon[0], Length(Polygon), WINDING);
  Result := PtInRegion(Rgn, Point.X, Point.Y);
  DeleteObject(Rgn);
END; { PointInPolygon }

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

PROCEDURE SetMode(TypeOfMode : ModeType; OnOrOff : Boolean);
{ Set up one of the various modes, updating the fourth status panel }

  PROCEDURE AddStringToStatusPanel(Str : String);
  BEGIN
    IF FWPRailWindow.FWPRailWindowStatusBar.Panels[StatusBarPanel3].Text = '' THEN
      WriteToStatusBarPanel(StatusBarPanel3, Str)
    ELSE
      WriteToStatusBarPanel(StatusBarPanel3, Str + ' ' + FWPRailWindow.FWPRailWindowStatusBar.Panels[StatusBarPanel3].Text);
  END; { AddStringToStatusPanel }

  PROCEDURE RemoveStringFromStatusPanel(Str : String);
  VAR
    TempStr : String;

  BEGIN
    TempStr := FWPRailWindow.FWPRailWindowStatusBar.Panels[StatusBarPanel3].Text;
    IF Pos(Str + ' ', FWPRailWindow.FWPRailWindowStatusBar.Panels[StatusBarPanel3].Text) > 0 THEN
      TempStr := StringReplace(TempStr, Str + ' ', '', [rfIgnoreCase])
    ELSE
      TempStr := StringReplace(TempStr, Str, '', [rfIgnoreCase]);
    WriteToStatusBarPanel(StatusBarPanel3, TempStr);
  END; { RemoveStringFromStatusPanel }

BEGIN
  IF OnOrOff = TurnOn THEN BEGIN
    CASE TypeOfMode OF
      AllRouteDebuggingModeType:
        IF NOT AllRouteDebuggingMode THEN BEGIN
          SetMode(AllRouteDebuggingModeType, True);
          Log('A All Route Debugging Mode = ON');
          AddStringToStatusPanel('ALLROUTE');
        END;
      AnonymousOccupationModeType:
        IF NOT AnonymousOccupationMode THEN BEGIN
          AnonymousOccupationMode := True;
          Log('A Anonymous Occupation Mode = ON');
          AddStringToStatusPanel('ANON');
        END;
      FeedbackDebuggingModeType:
        IF NOT FeedbackDebuggingMode THEN BEGIN
          FeedbackDebuggingMode := True;
          Log('A Feedback Debugging Mode = ON');
          AddStringToStatusPanel('FBCK ');
        END;
      GeneralDebuggingModeType:
        IF NOT DebuggingMode THEN BEGIN
          DebuggingMode := True;
          Log('A Debugging Mode = ON');
          AddStringToStatusPanel('DEBUG');
        END;
      LineDebuggingModeType:
        IF NOT LineDebuggingMode THEN BEGIN
          LineDebuggingMode := True;
          Log('A Line Debugging Mode = ON');
          AddStringToStatusPanel('LINE');
        END;
      LockDebuggingModeType:
        IF NOT LockDebuggingMode THEN BEGIN
          LockDebuggingMode := True;
          Log('A Lock Debugging Mode = ON');
          AddStringToStatusPanel('LOCK');
        END;
      LockingModeType:
        IF NOT LockingMode THEN BEGIN
          LockingMode := True;
          Log('AG Locking Mode = ON');
          AddStringToStatusPanel('LDB');
        END;
      LocoSpeedTimingModeType:
        IF NOT LocoSpeedTimingMode THEN BEGIN
          LocoSpeedTimingMode := True;
          Log('AG Loco Speed Timing Mode = ON');
          AddStringToStatusPanel('LST');
        END;
      LogsCurrentlyKeptModeType:
        IF NOT LogsCurrentlyKeptMode THEN BEGIN
          LogsCurrentlyKeptMode := True;
          Log('AG Logs Currently Kept Mde = ON');
          AddStringToStatusPanel('LCK');
        END;
      PointDebuggingModeType:
        IF NOT PointDebuggingMode THEN BEGIN
          PointDebuggingMode := True;
          Log('A Point Debugging Mode = ON');
          AddStringToStatusPanel('POINT');
        END;
      PreviousPointSettingsModeType:
        { This probably shouldn't really be a mode, as there would be no point in turning it off once the previous point settings are loaded, but it is a way of recording
          that the startup parameter is set and then implementing it when the points are subsequently loaded
        }
        IF NOT PreviousPointSettingsMode THEN BEGIN
          PreviousPointSettingsMode := True;
          Log('A Previous Point Settings Mode = ON');
          AddStringToStatusPanel('PPS');
        END;
      RDCModeType:
        IF NOT RDCMode THEN BEGIN
          RDCMode := True;
          Log('A RDC Mode = ON');
          AddStringToStatusPanel('RDC');
        END;
      RecordingMonitorScreensModeType:
        IF NOT RecordingMonitorScreensMode THEN BEGIN
          RecordingMonitorScreensMode := True;
          Log('A Recording MonitorScreens Mode = ON');
          AddStringToStatusPanel('RECORDING');
        END;
      RecordLineDrawingModeType:
        IF NOT RecordLineDrawingMode THEN BEGIN
          RecordLineDrawingMode := True;
          Log('A Record Line Drawing = ON');
          AddStringToStatusPanel('LINEDRAWING');
        END;
      RouteDebuggingModeType:
        IF NOT RouteDebuggingMode THEN BEGIN
          RouteDebuggingMode := True;
          Log('A Route Debugging Mode = ON');
          AddStringToStatusPanel('ROUTE');
        END;
      RouteBacktrackDebuggingModeType:
        IF NOT RouteBacktrackDebuggingMode THEN BEGIN
          RouteBacktrackDebuggingMode := True;
          Log('A RouteBacktrack Debugging Mode = ON');
          AddStringToStatusPanel('BACKTRACK');
        END;
      RouteDrawingModeType:
        IF NOT RouteDrawingMode THEN BEGIN
          RouteDrawingMode := True;
          Log('A Route Drawing Mode = ON');
          AddStringToStatusPanel('DRAWING');
        END;
      ShowAdjacentTrackCircuitModeType:
        IF NOT ShowAdjacentTrackCircuitMode THEN BEGIN
          ShowAdjacentTrackCircuitMode := True;
          Log('A Show Adjacent Track Circuit Mode = ON');
          AddStringToStatusPanel('ADJ');
        END;
      StationStartModeType:
        IF NOT StationStartMode THEN BEGIN
          StationStartMode := True;
          Log('A Station Start Mode = ON');
          AddStringToStatusPanel('SS');
        END;
      TestingModeType:
        IF NOT TestingMode THEN BEGIN
          TestingMode := True;
          Log('A Test Mode = ON');
          AddStringToStatusPanel('TEST');
        END;
    ELSE {CASE}
      Log('XG Unknown mode type found');
    END; {CASE}
  END ELSE BEGIN
    CASE TypeOfMode OF
      AllRouteDebuggingModeType:
        IF AllRouteDebuggingMode THEN BEGIN
          SetMode(AllRouteDebuggingModeType, False);
          Log('A All Route Debugging Mode = OFF');
          RemoveStringFromStatusPanel('ALLROUTE');
        END;
      AnonymousOccupationModeType:
        IF AnonymousOccupationMode THEN BEGIN
          AnonymousOccupationMode := False;
          Log('A Anonymous Occupation Mode = OFF');
          RemoveStringFromStatusPanel('ANON');
        END;
      FeedbackDebuggingModeType:
        IF FeedbackDebuggingMode THEN BEGIN
          FeedbackDebuggingMode := False;
          Log('A Feedback Debugging Mode = OFF');
          RemoveStringFromStatusPanel('FBCK');
        END;
      GeneralDebuggingModeType:
        IF DebuggingMode THEN BEGIN
          DebuggingMode := False;
          Log('A Debugging Mode = OFF');
          RemoveStringFromStatusPanel('DEBUG');
        END;
      LineDebuggingModeType:
        IF LineDebuggingMode THEN BEGIN
          LineDebuggingMode := False;
          Log('A Line Debugging Mode = OFF');
          RemoveStringFromStatusPanel('LINE');
        END;
      LockDebuggingModeType:
        IF LockDebuggingMode THEN BEGIN
          LockDebuggingMode := False;
          Log('A Lock Debugging Mode = OFF');
          RemoveStringFromStatusPanel('LDB');
        END;
      LockingModeType:
        IF LockingMode THEN BEGIN
          LockingMode := False;
          Log('AG Locking Mode = OFF');
          RemoveStringFromStatusPanel('LOCK');
        END;
      LocoSpeedTimingModeType:
        IF LocoSpeedTimingMode THEN BEGIN
          LocoSpeedTimingMode := False;
          Log('AG Loco Speed Timing Mode = OFF');
          RemoveStringFromStatusPanel('LST');
        END;
      LogsCurrentlyKeptModeType:
        IF LogsCurrentlyKeptMode THEN BEGIN
          LogsCurrentlyKeptMode := False;
          Log('AG Logs Currently Kept = OFF');
          RemoveStringFromStatusPanel('LCK');
        END;
      PointDebuggingModeType:
        IF PointDebuggingMode THEN BEGIN
          PointDebuggingMode := False;
          Log('A Point Debugging Mode = OFF');
          RemoveStringFromStatusPanel('POINTDEBUG');
        END;
      RDCModeType:
        IF RDCMode THEN BEGIN
          RDCMode := False;
          Log('A RDC Mode = OFF');
          RemoveStringFromStatusPanel('RDC');
        END;
      RecordingMonitorScreensModeType:
        IF RecordingMonitorScreensMode THEN BEGIN
          RecordingMonitorScreensMode := False;
          Log('A Recording Monitor Screens Mode = OFF');
          RemoveStringFromStatusPanel('RECORDING');
        END;
      RecordLineDrawingModeType:
        IF RecordLineDrawingMode THEN BEGIN
          RecordLineDrawingMode := False;
          Log('A Record Line Drawing Mode = OFF');
          RemoveStringFromStatusPanel('LINEDRAWING');
        END;
      RouteBacktrackDebuggingModeType:
        IF RouteBacktrackDebuggingMode THEN BEGIN
          RouteBacktrackDebuggingMode := False;
          Log('A RouteBacktrack Debugging Mode = OFF');
          RemoveStringFromStatusPanel('BACKTRACK');
        END;
      RouteDebuggingModeType:
        IF RouteDebuggingMode THEN BEGIN
          RouteDebuggingMode := False;
          Log('A Route Debugging Mode = OFF');
          RemoveStringFromStatusPanel('ROUTE');
        END;
      RouteDrawingModeType:
        IF RouteDrawingMode THEN BEGIN
          RouteDrawingMode := False;
          Log('A Route Drawing Mode = OFF');
          RemoveStringFromStatusPanel('DRAWING');
        END;
      ShowAdjacentTrackCircuitModeType:
        IF ShowAdjacentTrackCircuitMode THEN BEGIN
          ShowAdjacentTrackCircuitMode := False;
          Log('A Show Adjacent Trackj Circuit Mode Mode = OFF');
          RemoveStringFromStatusPanel('ADJ');
        END;
      StationStartModeType:
        IF StationStartMode THEN BEGIN
          StationStartMode := False;
          Log('A Station Start Mode = OFF');
          RemoveStringFromStatusPanel('SS');
        END;
      TestingModeType:
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

PROCEDURE SetFeedbackDebuggingModeOn;
{ Turn on feedback debugging mode, updating the fourth status panel }
BEGIN
  FeedbackDebuggingMode := True;
  Debug('Feedback debugging = ON');
  WriteStringToFeedbackWindow('Feedback debugging = ON');
  WriteToStatusBarPanel(StatusBarPanel3, SaveStatusPanel3Str + ' FEEDBACK');
END; { SetFeedbackDebuggingModeOn }

PROCEDURE SetFeedbackDebuggingModeOff;
{ Turn off feedback debugging mode, updating the fourth status panel }
VAR
  TempStr : String;

BEGIN
  FeedbackDebuggingMode := False;
  Debug('Feedback debugging = OFF');
  WriteStringToFeedbackWindow('Feedback debugging = OFF');

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
END; { WriteVariableDataToFile }

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

END { MiscUtils }.
