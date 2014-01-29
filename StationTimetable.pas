UNIT StationTimetable;
{ Writes the Station Timetable to screen independently of the FWP Railway Program

  v.0.0 22/03/09 first written
  v.1.0 23/03/09 perfected!
}

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls;

TYPE
  TStationTimetableWindow = CLASS(TForm)
    StationTimetableTimer: TTimer;
    PROCEDURE StationTimetableTimerTick(Sender: TObject);
    PROCEDURE StationTimetableWindowCreate(Sender: TObject);
    PROCEDURE StationTimetableWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftShift: TShiftState);
    PROCEDURE StationTimetableWindowPaint(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  StationTimetableWindow: TStationTimetableWindow;

IMPLEMENTATION

{$R *.dfm}

USES DateUtils;

VAR
  GameOver : Boolean = False;
  MaxX : Integer;
  MaxY : Integer;
  InvalidFileStr : String = '';
  NoFile : Boolean = False;
  SaveStationTimetableTimerInterval : Integer = 1000;
  StationTimetableDataFile : TextFile;
  StationTimetableDataFilename : String;
  StationTimeTableDataFileLastSize : TDateTime;
  StationTimeTableDataFileLastUpdateTime : TDateTime;

PROCEDURE Debug;
{ Used for setting informal breakpoints }
BEGIN
END; { Debug }

FUNCTION OpenInputFile(VAR InputFilename : Text; Filename : String) : Boolean;
{ Open a existing file to read from }
VAR
  SaveIOResult : Integer;

BEGIN
  AssignFile(InputFilename, Filename);
  {$I-}
  Reset(InputFilename);
  {$I+}
  SaveIOResult := IOResult;
  IF SaveIOResult = 0 THEN
    Result := True
  ELSE BEGIN
    Result := False;
    InvalidFileStr := Filename;
  END;
END; { OpenInputFile }

FUNCTION StrToColour(ColourStr : String) : TColor;
{ Checks if it's a Delphi colour or an FWP one }
BEGIN
  IF ColourStr = '' THEN BEGIN
    { a weird default! }
//    Debug('!No colour string supplied to StrToColour');
//    Log('X No colour string supplied to StrToColour');
    Result := clAqua
  END ELSE
    IF Pos(UpperCase('Unknown Colour'), UpperCase(ColourStr)) > 0 THEN
      Result := StringToColor(Copy(ColourStr, Length('Unknown Colour') + 2))
    ELSE
      IF ColourStr = 'clFWPDkBlue' THEN
        Result := $0802040
      ELSE
        IF ColourStr = 'clFWPOrange' THEN
          Result := $000080FF
        ELSE
          IF ColourStr = 'clFWPLtBrown' THEN
            Result := $004080FF
          ELSE
            IF ColourStr = 'clFWPDkBrown' THEN
              Result := $00004080
            ELSE
              IF ColourStr = 'clFWPPink' THEN
                Result := $008080FF
              ELSE
                IF ColourStr = 'clFWPVeryDkGrey' THEN
                  Result := $3355443
                ELSE
                  IF ColourStr = 'clFWPPhysicalPlatformColour' THEN
                    Result := $0038ABB1
                  ELSE
                    Result := StringToColor(ColourStr);
END; { StrToColour }

PROCEDURE TStationTimetableWindow.StationTimetableWindowKeyDown(Sender: TObject; VAR Key: Word; ShiftShift: TShiftState);
BEGIN
  CASE Key OF
    vk_Escape:
      Application.Terminate;
    Ord('B'):
      IF StationTimetableWindow.Borderstyle = bsNone THEN BEGIN
        StationTimetableWindow.Borderstyle := bsSingle;
        WindowState := wsNormal;
        ShowCursor(True);
        beep;
      END ELSE BEGIN
        StationTimetableWindow.Borderstyle := bsNone;
        WindowState := wsMaximized;
        ShowCursor(False);
      END;
  END; {CASE}
END; { StationTimetableWindowKeyDown }

PROCEDURE TStationTimetableWindow.StationTimetableWindowCreate(Sender: TObject);
VAR
  Ch : Char;

BEGIN
  ShowCursor(False);
  WITH StationTimetableWindow DO BEGIN
    StationTimetableWindow.Borderstyle := bsNone;
    WindowState := wsMaximized;
  END; {WITH}
  StationTimetableTimer.Interval := 1000;
  SaveStationTimetableTimerInterval := StationTimetableTimer.Interval;
  StationTimetableWindow.Width := Screen.DeskTopWidth;
  StationTimetableWindow.Height := Screen.DeskTopHeight;

//  StationTimetableWindow.Width := 1024;
//  StationTimetableWindow.Height := 768;

  MaxX := StationTimetableWindow.ClientWidth;
  MaxY := StationTimetableWindow.ClientHeight;

  Ch := ' ';
  IF ParamCount = 1 THEN
    Ch := ParamStr(1)[1];

    IF (Ch = '/') OR (Ch = '-') AND (Length(ParamStr(1)) > 1) THEN BEGIN
      { advance beyond the '/' or '-' }
      Ch := UpperCase(ParamStr(1))[2];
      CASE Ch OF
        'F':
          { List all the options here **** }
           IF Pos('FILE=', UpperCase(ParamStr(1))) > 0 THEN BEGIN
             StationTimetableDataFilename := Copy(ParamStr(1), 7);
      END; {CASE}
    END;
  END;
  IF StationTimetableDataFilename = '' THEN
    NoFile := True;
END; { StationTimetableWindowCreate }

PROCEDURE DrawStationTimetableWindow;
{ Collate, sort and display timetable entries }
VAR
  CommaPos : Integer;
  ErrorMsg : String;
  Line : String;
  TempReal : Real;
  TempStr : String;
  Text : String;
  XPos : Integer;
  YPos : Integer;

BEGIN
  TRY
    WITH StationTimetableWindow.Canvas DO BEGIN
      IF NOT NoFile THEN BEGIN
        FillRect(Rect(0, 0, MaxX, MaxY));

        IF OpenInputFile(StationTimetableDataFile, StationTimetableDataFilename) THEN BEGIN
          REPEAT
            ReadLn(StationTimetableDataFile, Line);
            IF Line = 'Game Over!' THEN
              GameOver := True
            ELSE BEGIN
              GameOver := False;
              StationTimetableWindow.StationTimetableTimer.Interval := SaveStationTimetableTimerInterval;

              { XPos }
              CommaPos := Pos(',', Line);
              TempStr := Copy(Line, 1, CommaPos - 1);
              XPos := StrToInt(TempStr);
              XPos := MulDiv(MaxX, XPos, 100);

              { YPos }
              Line := Copy(Line, CommaPos + 1);
              CommaPos := Pos(',', Line);
              TempStr := Copy(Line, 1, CommaPos - 1);
              TempReal := StrToFloat(TempStr);
              YPos := Round((MaxY * TempReal) / 100);

              { FontSize }
              Line := Copy(Line, CommaPos + 1);
              CommaPos := Pos(',', Line);
              TempStr := Copy(Line, 1, CommaPos - 1);
  //            Font.Size := StrToInt(TempStr);
              TempReal := StrToFloat(TempStr);
              Font.Size := MulDiv(MaxY, Round(TempReal), 100);

              { FontColour }
              Line := Copy(Line, CommaPos + 1);
              CommaPos := Pos(',', Line);
              TempStr := Copy(Line, 1, CommaPos - 1);
              Font.Color := StrToColour(TempStr);

              { FontStyle }
              Line := Copy(Line, CommaPos + 1);
              CommaPos := Pos(',', Line);
              TempStr := Copy(Line, 1, CommaPos - 1);
              IF TempStr = 'Bold' THEN
                Font.Style := [fsBold]
              ELSE
                IF TempStr = 'Italic' THEN
                  Font.Style := [fsItalic]
                ELSE
                  IF TempStr = 'Underline' THEN
                    Font.Style := [fsUnderline]
                  ELSE
                    IF TempStr = 'Strikeout' THEN
                      Font.Style := [fsStrikeout]
                    ELSE
                      Font.Style := [];

              { And the text }
              Text := Copy(Line, CommaPos + 1);

              IF Text <> '[Line]' THEN
                TextOut(Round(XPos), Round(YPos), Text)
              ELSE BEGIN
                { draw a separator line }
                MoveTo(0, Round(YPos));
                Pen.Color := Font.Color;
                LineTo(MaxX, Round(YPos));
              END;
            END;

          UNTIL EoF(StationTimetableDataFile);
          Close(StationTimetableDataFile);
        END;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO BEGIN
      Debug;
    END;
  END; {TRY}
END; { DrawStationTimetableWindow }

PROCEDURE TStationTimetableWindow.StationTimetableWindowPaint(Sender: TObject);
BEGIN
  DrawStationTimetableWindow;
END; { StationTimetableWindowPaint }

PROCEDURE TStationTimetableWindow.StationTimetableTimerTick(Sender: TObject);
{ Each time the clock ticks... }
VAR
  ErrorMsg : String;
  TempFileAge : Integer;
  TempFileSize : Integer;
  TempUpdateTime : TDateTime;

  FUNCTION NewFileSize(fileName : wideString) : Int64;
  { Returns file size in bytes or -1 if not found (by Zarko Gajic) }
  VAR
    sr : TSearchRec;

  BEGIN
    IF FindFirst(fileName, faAnyFile, sr ) = 0 THEN
      Result := Int64(sr.FindData.nFileSizeHigh) SHL Int64(32) + Int64(sr.FindData.nFileSizeLow)
    ELSE
      Result := -1;

    FindClose(sr) ;
  END; { NewFileSize }

BEGIN
  TRY
    WITH StationTimetableWindow.Canvas DO BEGIN
      IF NoFile OR (InvalidFileStr <> '') THEN BEGIN
        FillRect(Rect(0, 0, MaxX, MaxY));
        StationTimetableTimer.Interval := 3000;
        Font.Size := 20;
        Font.Color := clWhite;
        Randomize;
        IF NoFile THEN BEGIN
          ErrorMsg := 'No Station Timetable input filename supplied - press Escape to exit';
          TextOut(Random(MaxX - TextWidth(ErrorMsg)), Random(MaxY - TextHeight(ErrorMsg)), ErrorMsg);
        END ELSE
          IF InvalidFileStr <> '' THEN BEGIN
            ErrorMsg := 'Invalid Station Timetable input filename "' + InvalidFileStr + '" supplied - press Escape to exit';
            TextOut(Random(MaxX - TextWidth(ErrorMsg)), Random(MaxY - TextHeight(ErrorMsg)), ErrorMsg);
          END;
      END ELSE BEGIN
        TempFileAge := FileAge(StationTimetableDataFilename);
        IF TempFileAge <> -1 THEN BEGIN
          TempUpdateTime := FileDateToDateTime(TempFileAge);
          IF CompareTime(TempUpdateTime, StationTimeTableDataFileLastUpdateTime) > 0 THEN BEGIN
            DrawStationTimetableWindow;
            StationTimeTableDataFileLastUpdateTime := TempUpdateTime;
          END ELSE BEGIN
            TempFileSize := NewFileSize(StationTimeTableDataFilename);
            IF TempFileSize <> StationTimeTableDataFileLastSize THEN BEGIN
              DrawStationTimetableWindow;
              StationTimeTableDataFileLastSize := TempFileSize;
            END ELSE
              IF GameOver THEN BEGIN
                FillRect(Rect(0, 0, MaxX, MaxY));
                StationTimetableTimer.Interval := 3000;
                ErrorMsg := 'Game Over!';
                Font.Size := MulDiv(MaxY, 5, 100);
                Font.Color := clWhite;
                TextOut(Random(MaxX - TextWidth(ErrorMsg)), Random(MaxY - TextHeight(ErrorMsg)), ErrorMsg);
              END;
          END;
        END;
      END;
    END; {WITH}
  EXCEPT
    ON E : Exception DO BEGIN
      Debug;
    END;
  END; {TRY}
END; { StationTimetableTimerTick }

INITIALIZATION
  BEGIN
    TimeToStr(0);
    StationTimeTableDataFileLastUpdateTime := 0;
    StationTimeTableDataFileLastSize := 0;
  END;
END { StationTimetable }.

