UNIT Logging;
{ Contains logging and string-related routines

  Copyright © F.W. Pritchard 2014-15. All Rights Reserved.

  v0.1  17/03/14 First written
  v0.2  09/02/15 Additional code extracted mainly from MiscUtils
}

INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
     Options, Vcl.Menus, InitVars, SignalsUnit, Train, LinesUnit, Feedback, LocationsUnit, PointsUnit, TrackCircuitsUnit;

TYPE
  TLoggingWindow = CLASS(TForm)
    LoggingWindowFindDialogue: TFindDialog;
    LoggingWindowFontDialogue : TFontDialog;
    LoggingWindowPopupChangeFontSize : TMenuItem;
    LoggingWindowPopupFontSize : TMenuItem;
    LoggingWindowPopupFontSizeRestoreDefault : TMenuItem;
    LoggingWindowPopupMenu : TPopupMenu;
    LoggingWindowRichEdit : TRichEdit;
    PROCEDURE LoggingWindowClose(Sender: TObject; var Action: TCloseAction);
    PROCEDURE LoggingWindowFindDialogueClose(Sender: TObject);
    PROCEDURE LoggingWindowFindDialogueFind(Sender: TObject);
    PROCEDURE LoggingWindowFindDialogueShow(Sender: TObject);
    PROCEDURE LoggingWindowPopupChangeFontSizeClick(Sender : TObject);
    PROCEDURE LoggingWindowPopupFontSizeRestoreDefaultClick(Sender : TObject);
    PROCEDURE LoggingWindowRichEditKeyDown(Sender : TObject; VAR Key : Word; ShiftState : TShiftState);
    PROCEDURE LoggingWindowRichEditMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; MouseX, MouseY : Integer);
  PRIVATE
    { Private declarations }

  PUBLIC
    { Public declarations }
  END;

PROCEDURE AddRichLine(RichEdit: TRichEdit; StrToAdd: String);
{ Taken from Delpi Pages (http://www.delphipages.com/tips/thread.cfm?ID=186) - by Slavikn, WebPage: http://www.organizermp3.com }

FUNCTION AddSeparatorToTimeString(Str : String) : String;
{ Add a separator to a string so that, e.g., 0630 becomes 06:30 }

PROCEDURE AddStoredRichEditLoggingTextToLoggingWindow;
{ Add any stored rich-edit data to the logging window }

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

FUNCTION ColourToStr(Colour : TColour) : String;
{ UK English version of Delphi subroutine }

FUNCTION ColourToStrForUser(Colour : TColour) : String;
{ Checks if it's a Delphi colour or an FWP one, and removes the "cl" or "clFWP" }

FUNCTION ControlledByStateToStr(ControlledByState : LocoControlStateType) : String;
{ Return a string describing loco control }

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

FUNCTION DirectionArrayToStr(DirectionsArray : DirectionArrayType) : String;
{ List the contents of an array }

PROCEDURE DrawLineInLogFile(LocoChipStr : String; LogFileCh : Char; LineStr : String; OriginatingUnitRef : String);
{ Draw a line of a given character in the log file }

FUNCTION EndOfLineToStr(E : EndOfLineType) : String;
{ Return the end of line type as a string }

FUNCTION FeedbackDetectorTypeToStr(FeedbackDetectorType : TypeOfFeedbackDetector) : String;
{ Convert a feedback detector type to a string }

FUNCTION FeedbackTypeToStr(FeedbackType : TypeOfFeedback) : String;
{ Convert a feedback type to a string }

FUNCTION GetBuildInfo(VAR V1, V2, V3, V4: Word; AFileName: String= ''): Boolean;

FUNCTION GetBuildInfoAsString : String;
{ Return the program's build number - this is auto incremented on each build }

FUNCTION GetComputerNetName : String;
{ Return the local computer name (by Zarko Gajic, About.com) }

FUNCTION GetProgramTitle : String;
{ Returns the program title }

FUNCTION GetProgramVersion(Str : String) : String;
{ Returns the program version }

PROCEDURE GetProjectVersionInfo(AVersionList: TStrings; AFileName: String= '');

FUNCTION GetUserFromWindows : String;
{ Return the local user name (by Zarko Gajic, About.com) }

FUNCTION GetVersionInfoAsString : String;
{ Return the program's version number }

FUNCTION GradientToStr(Gradient : GradientType) : String;
{ Return the gradient status of the line as a string }

PROCEDURE InitialiseLoggingWindow;
{ Such routines as this allow us to initialises the units in the order we wish }

FUNCTION IndicatorToStr(I : IndicatorType; LongOrShortString : StringType) : String;
{ Return the type a route indicator is }

FUNCTION IndicatorStateToStr(I : IndicatorStateType) : String;
{ Return the state of a route indicator }

FUNCTION IntegerArrayToStr(IntegerArray : IntegerArrayType) : String;
{ Return the contents of an integer array }

FUNCTION JourneyToStr(Journey : Integer) : String;
{ Return a journey number as a string }

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

FUNCTION ListLocoChipsInIntegerArray(IntegerArray : IntegerArrayType) : String;
{ Lists loco chips from an integer array }

FUNCTION LocationOccupationStateToStr(OccupationState : LocationOccupationStateType) : String;
{ Return the state of the Location Occupation as a string }

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

FUNCTION MPHToInt(MPH : MPHType) : Integer;
{ Returns the given MPH as a integer }

FUNCTION MPHToStr(MPH : MPHType) : String;
{ Returns the given MPH as a string }

FUNCTION NextLineRouteingToStr(NextLineRouteing : NextLineRouteingType) : String;
{ Return a description of the next line routeing type }

FUNCTION PenStyleToStr(PenStyle : TPenStyle) : String;
{ Describes pen styles }

FUNCTION PointStateToStr(PointState : PointStateType) : String;
{ Return the point state as a string }

FUNCTION PointToStr(Point : Integer) : String;
{ Return the point number as a string }

FUNCTION PointTypeToStr(PType : TypeOfPoint) : String;
{ Return the point type }

FUNCTION ReturnFixedLengthStr(Str : String; FixedLength : Integer) : String;
{ Return a short string of a fixed length }

FUNCTION RouteToStr(Route : Integer) : String;
{ Return the Route number as a string }

FUNCTION SemaphoreAspectToStr(Aspect : AspectType): String; Overload;
{ Return a semaphore signal aspect as a long string }

FUNCTION SignalQuadrantToStr(Q : QuadrantType) : String;
{ Return a string with the supplied signal quadrant details }

FUNCTION SignalToStr(Signal : Integer) : String;
{ Return the signal number as a string }

FUNCTION SignalTypeToStr(ST : TypeOfSignal; LongOrShortString : StringType) : String;
{ Return the type of a signal in words }

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

FUNCTION StrToFeedbackDetectorType(Str : String) : TypeOfFeedbackDetector;
{ Convert a string to a feedback detector type }

FUNCTION StrToFeedbackType(Str : String) : TypeOfFeedback;
{ Convert a striing to feedback type }

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

FUNCTION ThroughLocationStateToStr(ThroughLocationState : ThroughLocationStateType) : String;
{ Return the through state of the location as a string }

FUNCTION TimeToHMStr(Time : TDateTime) : String;
{ Return a time string as hh:mm }

FUNCTION TimeToHMSStr(Time : TDateTime) : String;
{ Return a time string as hh:mm:ss }

FUNCTION TimeToHMSZStr(Time : TDateTime) : String;
{ Return a time string as hh:mm:ss:zzz }

FUNCTION TrackCircuitStateToStr(State : TrackCircuitStateType) : String;
{ Describe the current state of a given track circuit }

FUNCTION TrackCircuitToStr(TrackCircuit : Integer) : String;
{ Return the track circuit number as a string }

FUNCTION TrainStatusToStr(Status : TrainStatusType) : String;
{ Return the given train status as a string }

FUNCTION TrainTypeNumToStr(TrainTypeNum : Integer) : String;
{ Returns the train type number as a string; an up-to-date list as of 5/10/05 }

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

FUNCTION TypeOfLineToStr(T : TypeOfLine) : String;
{ Describe a line type }

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
VAR
  LoggingWindow: TLoggingWindow;
  LoggingWindowFindDialogueActive : Boolean = False;
  StoreRichEditLoggingText : Boolean = False;

IMPLEMENTATION

{$R *.dfm}

USES MiscUtils, System.Types, RailDraw, RichEdit, ClipBrd, StrUtils, GetTime, StationMonitors, DateUtils, Lenz, Route, Help, System.UITypes;

CONST
  MaxSaveLogStrs = 10;
  RichEditChars = True;
  UnitRef = 'Logging';

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

VAR
  DrawLineInLogFileAfter : Boolean = False;
  DrawLineInLogFileBefore : Boolean = False;
  DrawLineInLogFileOnly : Boolean = False;
  IdenticalLineMsgWritten : Boolean = False;
  InitialisationCount : Integer;
  LastLogLineStr : String = '';
  LineAfterJustDrawn : Boolean = False;
  LogArray : StringArrayType;
  LoggingPausedByFindDialogue : Boolean;
  OldDebugStr : String = '';
  PreviousLogTime : TDateTime = 0;
  SaveLogStrArray : ARRAY [1..MaxSaveLogStrs] OF String;
  StoredRichEditLoggingTextArray : StringArrayType;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

FUNCTION IsLastRichEditLineVisible(RichEdit : TRichEdit) : Boolean;

  FUNCTION GetFirstVisibleRichEditLine(RichEdit : TRichEdit) : Integer;
  BEGIN
    WITH LoggingWindow DO
      Result := RichEdit.Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
  END; { GetFirstVisibleRichEditLine }

  FUNCTION GetLineCountForRichEditLine(RichEdit : TRichEdit) : Integer;
  BEGIN
    WITH LoggingWindow DO
      Result := RichEdit.Perform(EM_GETLINECOUNT, 0, 0);
  END; { GetLineCountForRichEditLine }

  FUNCTION GetLastVisibleRichEditLine(RichEdit: TRichEdit): Integer;
  CONST
    EM_EXLINEFROMCHAR = WM_USER + 54;

  VAR
    I : Integer;
    Rect : TRect;

  BEGIN
    { The EM_GETRECT message retrieves the formatting rectangle of an edit control }
    RichEdit.Perform(EM_GETRECT, 0, @Rect);
    Rect.Left := Rect.Left + 1;
    Rect.Top  := Rect.Bottom - 2;

    { The EM_CHARFROMPOS message retrieves information about the character closest to a specified point in the client area of an edit control }
    I := RichEdit.Perform(EM_CHARFROMPOS, 0, @Rect);

    { The EM_EXLINEFROMCHAR message determines which line contains the specified character in a rich edit control }
    Result := RichEdit.Perform(EM_EXLINEFROMCHAR, 0, I);
  END; { GetLastVisibleRichEditLine }

VAR
  LastVisibleLine : Integer;
  TotalLineCount : Integer;

BEGIN { IsLastRichEditLineVisible }
  WITH LoggingWindow DO BEGIN
    LastVisibleLine := GetLastVisibleRichEditLine(LoggingWindowRichEdit);
    TotalLineCount := GetLineCountForRichEditLine(LoggingWindowRichEdit);
    IF LastVisibleLine + 1 < TotalLineCount THEN
      Result := False
    ELSE
      Result := True;
  END; {WITH}
END; { IsLastRichEditLineVisible }

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

    PROCEDURE AddToGeneralLog(RichEditLogStr : String);
    { Either write to the log immediately or temporarily stores the text }
    CONST
      EM_EXLINEFROMCHAR = WM_USER + 54;

    VAR
      Rect : TRect;
      VisibleStart : Integer;

    BEGIN
      IF NOT IsLastRichEditLineVisible(LoggingWindow.LoggingWindowRichEdit) OR LoggingPausedByFindDialogue THEN BEGIN
        WITH LoggingWindow DO BEGIN
          { The EM_GETRECT message retrieves the formatting rectangle of an edit control }
          LoggingWindowRichEdit.Perform(EM_GETRECT, 0, LongInt(@Rect));

          { The EM_CHARFROMPOS message retrieves information about the character closest to a specified point in the client area of an edit control }
          VisibleStart := LoggingWindowRichEdit.Perform(EM_CHARFROMPOS, 0, @Rect);
          AddRichLine(LoggingWindow.LoggingWindowRichEdit, RichEditLogStr);
          LoggingWindowRichEdit.SelStart := VisibleStart;
        END; {WITH}
      END ELSE
        AddRichLine(LoggingWindow.LoggingWindowRichEdit, RichEditLogStr);
    END; { AddToGeneralLog }

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
            AddToGeneralLog(LogStrWithRichEditCommands);

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
            AddToGeneralLog(LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            IF MultipleLogFilesRequired THEN
              WriteLn(SignalPointAndTCLogFile, LogStrWithoutRichEditCommands);
          END;

        'D', 'd':
          BEGIN
            AddToGeneralLog(LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            IF MultipleLogFilesRequired THEN
              WriteLn(DiagramsLogFile, LogStrWithoutRichEditCommands);
          END;

        'W', 'w':
          BEGIN
            AddToGeneralLog(LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            WriteLn(TestLogFile, LogStrWithoutRichEditCommands);
            IF MultipleLogFilesRequired THEN
              WriteLn(WorkingTimetableLogFile, LogStrWithoutRichEditCommands);
          END;

        'L', 'l':
          BEGIN
            AddToGeneralLog(LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            IF MultipleLogFilesRequired THEN
              WriteLn(LocoLogFile, LogStrWithoutRichEditCommands);
          END;

        'R', 'r':
          BEGIN
            AddToGeneralLog(LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            IF MultipleLogFilesRequired THEN
              WriteLn(RouteLogFile, LogStrWithoutRichEditCommands);
          END;

        '*':
          BEGIN
            AddToGeneralLog(LogStrWithRichEditCommands);

            WriteLn(LargeLogFile, LogStrWithoutRichEditCommands);
            WriteLn(TestLogFile, LogStrWithoutRichEditCommands);
          END;
        '+':

          { Only write to the test log file }
          WriteLn(TestLogFile, LogStrWithoutRichEditCommands);

        'E', 'e', 'X', 'x': { unusual happenings of relevance to all the log files }
          BEGIN
            AddToGeneralLog(LogStrWithRichEditCommands);

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

FUNCTION GetLastLogLineStr : String;
{ Returns the last line written to the log }
BEGIN
  Result := LastLogLineStr;
END; { GetLastLogLine }

FUNCTION SemaphoreAspectToStr(Aspect : AspectType): String; Overload;
{ Return a semaphore signal aspect as a long string }
BEGIN
  CASE Aspect OF
    DoubleYellowAspect:
      Result := SemaphoreAspectOff;
    SingleYellowAspect:
      Result := SemaphoreAspectOff;
    GreenAspect:
      Result := SemaphoreAspectOff;
    RedAspect:
      Result := SemaphoreAspectOn;
    NoAspect:
      Result := NoAspectStr;
  ELSE {CASE}
    Result := UnknownSemaphoreAspect;
  END; {CASE}
END; { SemaphoreAspectToStr }

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

FUNCTION DescribePolygon(CONST Polygon: ARRAY OF TPoint) : String;
{ Returns the points comprising a polygon as a string }
VAR
  I : Integer;

BEGIN
  Result := 'X=' + IntToStr(Polygon[0].X) + ' Y=' + IntToStr(Polygon[0].Y);

  FOR I := 1 TO High(Polygon) DO
    Result := Result + ', X=' + IntToStr(Polygon[I].X) + ' Y=' + IntToStr(Polygon[I].Y);
END; { DescribePolygon }

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

FUNCTION FeedbackDetectorTypeToStr(FeedbackDetectorType : TypeOfFeedbackDetector) : String;
{ Convert a feedback detector type to a string }
BEGIN
  CASE FeedbackDetectorType OF
    FeedbackDetectorOutOfUse:
      Result := FeedbackDetectorOutOfUseStr;
    LineFeedbackDetector:
      Result := LineFeedbackDetectorStr;
    MixedFeedbackDetectors:
      Result := MixedFeedbackDetectorStr;
    PointFeedbackDetector:
      Result := PointFeedbackDetectorStr;
    TrackCircuitFeedbackDetector:
      Result := TrackCircuitFeedbackDetectorStr;
    TRSPlungerFeedbackDetector:
      Result := TRSPlungerFeedbackDetectorStr;
  ELSE
    Result := UnknownFeedbackDetectorStr;
  END; {CASE}
END; { FeedbackDetectorTypeToStr }

FUNCTION FeedbackTypeToStr(FeedbackType : TypeOfFeedback) : String;
{ Convert a feedback type to a string }
BEGIN
  CASE FeedbackType OF
    LineFeedback:
      Result := LineFeedbackStr;
    PointFeedback:
      Result := PointFeedbackStr;
    TrackCircuitFeedback:
      Result := TrackCircuitFeedbackStr;
    TRSPlungerFeedback:
      Result := TRSPlungerFeedbackStr;
  ELSE
    Result := UnknownFeedbackStr;
  END; {CASE}
END; { FeedbackTypeToStr }

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

FUNCTION ReturnFixedLengthStr(Str : String; FixedLength : Integer) : String;
{ Return a short string of a fixed length }
BEGIN
  Result := Str + StringOfChar(' ', 4 - Length(Str))
END; { ReturnFixedLengthStr }

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

FUNCTION StrToFeedbackDetectorType(Str : String) : TypeOfFeedbackDetector;
{ Convert a string to a feedback detector type }
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
END; { StrToFeedbackDetectorType }

FUNCTION StrToFeedbackType(Str : String) : TypeOfFeedback;
{ Convert a striing to feedback type }
BEGIN
  IF Str = LineFeedbackStr THEN
    Result := LineFeedback
  ELSE
    IF Str = LineFeedbackStr THEN
      Result := LineFeedback
    ELSE
      IF Str = PointFeedbackStr THEN
        Result := PointFeedback
      ELSE
        IF Str = TrackCircuitFeedbackStr THEN
          Result := TrackCircuitFeedback
        ELSE
          IF Str = TRSPlungerFeedbackStr THEN
            Result := TRSPlungerFeedback
          ELSE
            Result := UnknownFeedbackType;
END; { StrToFeedbackType }

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
    Result := 'Unknown Cursor';
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
  FWPRailWindowWasFocused : Boolean;
  LoggingWindowWasFocused : Boolean;
  SaveStyle : TFontStyles;

BEGIN
  AlwaysMakeSound := False;
  FWPRailWindowWasFocused := False;
  LoggingWindowWasFocused := False;

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
      IF DebugWindow.Visible AND NOT StationMonitorsWindow.Visible THEN BEGIN
        IF FWPRailWindow.Focused THEN
          FWPRailWindowWasFocused := True
        ELSE
          IF LoggingWindow.Visible THEN
            LoggingWindowWasFocused := True;

        DebugWindow.SetFocus;
      END;
      DebugWindow.DebugRichEdit.Perform(EM_SCROLLCARET, 0, 0);

      IF FWPRailWindowWasFocused THEN
        FWPRailWindow.SetFocus
      ELSE
        IF LoggingWindowWasFocused THEN
          LoggingWindow.SetFocus;

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
  TRY
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
  EXCEPT {TRY}
    ON E : Exception DO
      { Cannot call Log here as we are already in it }
      Debug('EG AddRichLine: ' + E.ClassName + ' error raised, with message: '+ E.Message);
  END; {TRY}
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

PROCEDURE TLoggingWindow.LoggingWindowPopupChangeFontSizeClick(Sender : TObject);
BEGIN
  { Show the default }
  LoggingWindowFontDialogue.Font.Name := LoggingWindowFontName;
  LoggingWindowFontDialogue.Font.Size := LoggingWindowFontSize;

  { Allow the user to change it }
  IF LoggingWindowFontDialogue.Execute THEN BEGIN
    LoggingWindowFontName := LoggingWindowFontDialogue.Font.Name;
    LoggingWindowFontSize := LoggingWindowFontDialogue.Font.Size;
    LoggingWindowRichEdit.Font.Style := [];

    LoggingWindowRichEdit.Font := LoggingWindowFontDialogue.Font;
  END;
END; { LoggingWindowPopupChangeFontSizeClick }

PROCEDURE TLoggingWindow.LoggingWindowPopupFontSizeRestoreDefaultClick(Sender : TObject);
BEGIN
  LoggingWindowRichEdit.Font.Name := DefaultLoggingWindowFontName;
  LoggingWindowFontName := DefaultLoggingWindowFontName;
  LoggingWindowRichEdit.Font.Size := DefaultLoggingWindowFontSize;
  LoggingWindowFontSize := DefaultLoggingWindowFontSize;
  LoggingWindowRichEdit.Font.Style := [];
END; { LoggingWindowPopupFontSizeRestoreDefaultClick }

PROCEDURE TLoggingWindow.LoggingWindowClose(Sender: TObject; VAR Action: TCloseAction);
BEGIN
  { Store where we want the window to be in case we move it, close it then want to reopen it }
  LoggingWindowHeight := LoggingWindow.Height;
  LoggingWindowWidth := LoggingWindow.Width;
  LoggingWindowTop := LoggingWindow.Top;
  LoggingWindowLeft := LoggingWindow.Left;
END; { LoggingWindowClose }

PROCEDURE TLoggingWindow.LoggingWindowFindDialogueClose(Sender: TObject);
BEGIN
  LoggingWindowFindDialogueActive := False;
END; { LoggingWindowFindDialogClose }

PROCEDURE TLoggingWindow.LoggingWindowFindDialogueShow(Sender: TObject);
BEGIN
  { There is no FindDialog.Width so the positioning here is a fudge to get it to the right of the Help window }
  LoggingWindowFindDialogue.Left := LoggingWindow.Left + (LoggingWindow.Width DIV 2);

  LoggingWindowFindDialogue.Top := LoggingWindow.Top;
  LoggingWindowFindDialogueActive := True;

  LoggingPausedByFindDialogue := True;
END; { LoggingWindowFindDialogShow }

PROCEDURE TLoggingWindow.LoggingWindowFindDialogueFind(Sender: TObject);
{ From "http://docwiki.embarcadero.com/CodeExamples/XE3/en/FindText_%28Delphi%29" }
VAR
  FoundAt : LongInt;
  mySearchTypes : TSearchTypes;
  StartPos : Integer;
  ToEnd : Integer;

BEGIN
  mySearchTypes := [];

  WITH LoggingWindowRichEdit DO BEGIN
    IF frMatchCase IN LoggingWindowFindDialogue.Options THEN
      mySearchTypes := mySearchTypes + [stMatchCase];
    IF frWholeWord IN LoggingWindowFindDialogue.Options THEN
      mySearchTypes := mySearchTypes + [stWholeWord];

    { Begin the search after the current selection, if there is one, otherwise, begin at the start of the text }
    IF SelLength <> 0 THEN
      StartPos := SelStart + SelLength
    ELSE
      StartPos := 0;

    { ToEnd is the length from StartPos through the end of the text in the rich edit control }
    ToEnd := Length(Text) - StartPos;
    FoundAt := FindText(LoggingWindowFindDialogue.FindText, StartPos, ToEnd, mySearchTypes);
    IF FoundAt = -1 THEN
      MakeSound(1)
    ELSE BEGIN
      SetFocus;
      SelStart := FoundAt;
      SelLength := Length(LoggingWindowFindDialogue.FindText);
    END;
  END; {WITH}
END; { LoggingWindowFindDialogFind }

PROCEDURE TLoggingWindow.LoggingWindowRichEditMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; MouseX, MouseY : Integer);
BEGIN
  IF Button = mbRight THEN
    LoggingWindowPopupMenu.Popup(LoggingWindow.Left + MouseX, LoggingWindow.Top + MouseY);
END; { LoggingWindowRichEditMouseDown }

PROCEDURE InitialiseLoggingWindow;
{ Such routines as this allow us to initialises the units in the order we wish }
BEGIN
  LoggingWindow.Height := LoggingWindowHeight;
  LoggingWindow.Width := LoggingWindowWidth;
  LoggingWindow.Top := LoggingWindowTop;
  LoggingWindow.Left := LoggingWindowLeft;

  LoggingWindow.LoggingWindowRichEdit.Font.Name := LoggingWindowFontName;
  LoggingWindow.LoggingWindowRichEdit.Font.Size := LoggingWindowFontSize;
  LoggingWindow.LoggingWindowRichEdit.Font.Style := [];

  LoggingPausedByFindDialogue := False;
END; { InitialiseLoggingWindow }

PROCEDURE TLoggingWindow.LoggingWindowRichEditKeyDown(Sender : TObject; VAR Key : Word; ShiftState : TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      IF LoggingWindowFindDialogueActive THEN
        LoggingWindowFindDialogue.CloseDialog
      ELSE
        LoggingWindow.Hide;
    Ord('F'), Ord('f'):
      IF ssCtrl IN ShiftState THEN BEGIN
        LoggingWindowFindDialogue.Position := Point(LoggingWindowRichEdit.Left + LoggingWindowRichEdit.Width, LoggingWindowRichEdit.Top);
        LoggingWindowFindDialogue.Execute;
      END;
    Ord('G'), Ord('g'):
      LoggingWindow.Visible := False;

    Ord('X'):
      IF IsLastRichEditLineVisible(LoggingWindowRichEdit) THEN
        Debug('Not scrolled ' + TestCountStr)
      ELSE
        Debug('Scrolled ' + TestCountStr);

  END; {CASE}
END; { LoggingWindowRichEditKeyDown }

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

END { Logging }.
