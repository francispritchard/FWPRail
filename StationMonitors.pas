UNIT StationMonitors;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Grids, InitVars, System.UITypes, Web.Win.Sockets;

TYPE
  TStationMonitorsWindow = CLASS(TForm)
    StationMonitorsTcpServer: TTcpServer;
    PROCEDURE StationMonitorsFormPaint(Sender: TObject);
    PROCEDURE StationMonitorsTcpServerAccept(Sender: TObject; ClientSocket: TCustomIpClient);
    procedure StationMonitorsFormCreate(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  StationMonitorsWindow: TStationMonitorsWindow;

PROCEDURE CloseStationMonitorsWebPage(OUT OK : Boolean);
{ Close the station nonitor web page if it exists }

PROCEDURE DrawStationMonitorsWindow(Area : Integer);
{ Collate, sort and display station monitors entries }

{$R *.dfm}

IMPLEMENTATION

USES MiscUtils, StrUtils, DateUtils, Input, GetTime, Diagrams, Options, Raildraw, Types, SyncObjs;

CONST
  UnitRef = 'StationMonitors';

VAR
  ExpectedColumnStr : String;
  LocationColumnStr : String;
  LocoColumnStr : String;
  PlatformColumnStr : String;
  StationMonitorsCriticalSection : TCriticalSection;
  StationMonitorsWebPage : TStringList;
  TimeColumnStr : String;
  WebIncludeLocoChipInStationMonitors : Boolean = False;

PROCEDURE Log(Str : String);
{ For ease of debugging, adds the unit name }
BEGIN
  WriteToLogFile(Str + ' {UNIT=' + UnitRef + '}');
END; { Log }

PROCEDURE DrawStationMonitorsWindow(Area : Integer);
{ Collate, sort and display timetable entries }
TYPE
  StationMonitorsRec = RECORD
                          ActualPlatformNumStr : String;
                          ActualTimeStr : String;
                          DiagrammedPlatformNumStr : String;
                          DiagrammedTimeStr : String;
                          LocationStr : String;
                          LocoChipStr: String;
                        END;
CONST
  ArrivedStr = 'arrived';
  OnTimeStr = 'on time';
  DepartedStr = 'departed';
  DelayedStr = 'delayed';
  CancelledStr = 'cancelled';

VAR
  CurrentTimeStr : String;
  DiagramsArrayPos : Integer;
//  ErrorMsg : String;
  T : Integer;

  FUNCTION EntriesAreInOrder(Timetable1Rec, Timetable2Rec : StationMonitorsRec) : Boolean;
  { Check the departure times. If less, return true. If greater, return false. If they are equal, check the platform numbers }
  BEGIN
    IF Timetable1Rec.DiagrammedTimeStr < Timetable2Rec.DiagrammedTimeStr THEN
      Result := True
    ELSE
      IF Timetable1Rec.DiagrammedTimeStr > Timetable2Rec.DiagrammedTimeStr THEN
        Result := False
      ELSE
        { the start locations are equal, so check the end locations }
        IF Timetable1Rec.ActualPlatformNumStr < Timetable2Rec.ActualPlatformNumStr THEN
          Result := True
        ELSE
          IF Timetable1Rec.ActualPlatformNumStr > Timetable2Rec.ActualPlatformNumStr THEN
            Result := False
          ELSE
            Result := True;
  END; { EntriesAreInOrder }

  PROCEDURE SortAndDisplayTimetableEntries(TimetableArray : ARRAY OF StationMonitorsRec);
  { Sort and display the entries we're given - sort rewritten after discussion with AMS on Trafalgar Day 2007 at Battersea Park }
  VAR
    DebugStr : String;
    EarliestTimeStr : String;
    I, J, K : Integer;
    OldTimetableArray, NewTimetableArray : ARRAY OF StationMonitorsRec;
    TimePos : Integer;

  BEGIN
    WITH StationMonitorsWindow.Canvas DO BEGIN
      { First we sort the array into order - find the earliest time and put it at the front of the new array }
      SetLength(OldTimetableArray, Length(TimetableArray));

      { Copy the old array across or can't alter the length of it }
      FOR I := 0 TO High(TimetableArray) DO
        OldTimetableArray[I] := TimetableArray[I];
      SetLength(NewTimetableArray, 0);

      { Now do the sort }
      FOR I := 0 TO High(OldTimetableArray) DO BEGIN
        TimePos := -1;
        EarliestTimeStr := '23:59';
        FOR J := 0 TO High(OldTimetableArray) DO BEGIN
          IF OldTimetableArray[J].DiagrammedTimeStr < EarliestTimeStr THEN BEGIN
            EarliestTimeStr := OldTimetableArray[J].DiagrammedTimeStr;
            TimePos := J;
          END;
        END; {FOR}
        SetLength(NewTimetableArray, Length(NewTimetableArray) + 1);
        NewTimetableArray[High(NewTimetableArray)] := OldTimetableArray[TimePos];

        { and remove the element from the original array }
        FOR K := TimePos TO (Length(OldTimetableArray) - 2) DO
          OldTimetableArray[K] := OldTimetableArray[K + 1];
        SetLength(OldTimetableArray, Length(OldTimetableArray) -1);
      END; {FOR}

      { And now display it }
      I := 0;
      WHILE I <= High(NewTimetableArray) DO BEGIN
        WITH NewTimetableArray[I] DO BEGIN
          IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
            TimeColumnStr := TimeColumnStr + '<span class="ttentry">' + DiagrammedTimeStr + '</span>';
          DebugStr := DiagrammedTimeStr;

          IF WebIncludeLocoChipInStationMonitors THEN BEGIN
            IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
              LocoColumnStr := LocoColumnStr + '<span class="ttlocoentry">' + LocoChipStr + '</span>';
          END;

          { Deal with trains that are not for public use }
          IF LocationStr <> '' THEN BEGIN
            IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
              LocationColumnStr := LocationColumnStr + '<span class="ttentry">' + LocationStr + '</span>';

            DebugStr := DebugStr + StringOfChar(' ', 2) + LocationStr;
          END;

          IF DiagrammedPlatformNumStr = ActualPlatformNumStr THEN BEGIN
            IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
              PlatformColumnStr := PlatformColumnStr + '<span class="ttentry">' + DiagrammedPlatformNumStr + '</span>';
            DebugStr := DebugStr + StringOfChar(' ', 30 - Length(DebugStr)) + ActualPlatformNumStr;
          END ELSE BEGIN
            IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
              PlatformColumnStr := PlatformColumnStr + '<span class="ttentry">' + ActualPlatformNumStr + '</span>';
            DebugStr := DebugStr + StringOfChar(' ', 29 - Length(DebugStr)) + '*' + ActualPlatformNumStr + '*';
          END;

          IF Copy(ActualTimeStr, 1, 3) <> '<B>' THEN
            DebugStr := DebugStr + StringOfChar(' ', 38 - Length(DebugStr)) + ActualTimeStr
          ELSE BEGIN
            ActualTimeStr := Copy(ActualTimeStr, 4);
            DebugStr := DebugStr + StringOfChar(' ', 38 - Length(DebugStr)) + '*' + ActualTimeStr + '*';
          END;

          IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
            ExpectedColumnStr := ExpectedColumnStr + '<span class="ttentry">' + ActualTimeStr + '</span>';

          IF RecordingMonitorScreens THEN
            Log('T ' + DebugStr + ' {NOUNITREF}');
        END; {WITH}
        Inc(I);
      END; {WHILE}
    END; {WITH}
  END; { SortAndDisplayTimetableEntries }

  PROCEDURE ShowArrivals(Area : Integer);
  { Write out the arrivals screen }
  VAR
    Hour, Min, Sec, MSec : Word;
    JourneyCount : Integer;
    TempCurrentArrivalTime : TDateTime;
    TempDiagrammedArrivalTime : TDateTime;
    TimetableArray : ARRAY OF StationMonitorsRec;

  BEGIN
    TRY
      WITH StationMonitorsWindow.Canvas DO BEGIN
        IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN BEGIN
          TimeColumnStr :=       '    <div style="width:20%;float:left;">' + CRLF
                               + '    <span class="ttheading">Arrives</span>';
          IF WebIncludeLocoChipInStationMonitors THEN BEGIN
            LocoColumnStr :=     '    <div style="width:10%;float:left;">' + CRLF
                               + '    <span class="ttheading">Loco</span>';
            LocationColumnStr := '    <div style="width:30%;float:left;">' + CRLF
                               + '    <span class="ttheading">From</span>';
          END ELSE BEGIN
            LocoColumnStr := '';
            LocationColumnStr := '    <div style="width:40%;float:left;">' + CRLF
                               + '    <span class="ttheading">From</span>';
          END;
          PlatformColumnStr :=   '    <div style="width:20%;float:left;">' + CRLF
                               + '    <span class="ttheading">Platform</span>';
          ExpectedColumnStr :=   '    <div style="width:20%;float:left;">' + CRLF
                               + '    <span class="ttheading">Expected</span>';
        END;

        IF RecordingMonitorScreens THEN
          Log('T Arrivals' + ' {NOUNITREF}');

        SetLength(TimetableArray, 0);

        { Compile a list for the timetable - this is unsorted, though }
        DiagramsArrayPos := 0;
        WHILE DiagramsArrayPos <= High(DiagramsArray) DO BEGIN
          T := DiagramsArray[DiagramsArrayPos];
          WITH Trains[T] DO BEGIN
            { Build a list of trains using the station }
            FOR JourneyCount := 0 TO High(Train_JourneysArray) DO BEGIN
              WITH Train_JourneysArray[JourneyCount] DO BEGIN
                IF (Train_TotalJourneys = -1)
                OR (JourneyCount = Train_TotalJourneys)
                OR ((JourneyCount < Train_TotalJourneys)
                     AND (Train_JourneysArray[JourneyCount + 1].TrainJourney_ActualDepartureTime = 0))
                THEN BEGIN
                  IF (TrainJourney_EndArea = Area) AND TrainJourney_StoppingOnArrival THEN BEGIN
                    IF DisplayNotForPublicUseTrainsInStationMonitors OR NOT TrainJourney_NotForPublicUse THEN BEGIN
                      IF ((Train_CurrentStatus = Cancelled)
                          AND (CompareTime(CurrentRailwayTime, TrainJourney_DiagrammedArrivalTime) <= 0))
                      OR ((Train_CurrentStatus <> Cancelled)
                          AND ((TrainJourney_ActualArrivalTime = 0)
                                OR (CompareTime(CurrentRailwayTime, IncMinute(TrainJourney_ActualArrivalTime, 2)) < 0)))
                      THEN BEGIN
                        SetLength(TimetableArray, Length(TimetableArray) + 1);
                        WITH TimetableArray[High(TimetableArray)] DO BEGIN
                          LocationStr := TrainJourney_StartStationName;
                          IF TrainJourney_NotForPublicUse THEN
                            { show the location in a different font }
                            LocationStr := '<i>' + LocationStr + '</i>';

                          IF WebIncludeLocoChipInStationMonitors THEN
                            LocoChipStr := IntToStr(Train_LocoChip);

                          IF TrainJourney_DiagrammedEndLocation <> UnknownLocation THEN BEGIN
                            DiagrammedPlatformNumStr := Locations[TrainJourney_DiagrammedEndLocation].Location_PlatformOrFiddleyardNumStr;
                            IF TrainJourney_DiagrammedEndLocation = TrainJourney_EndLocation THEN
                              ActualPlatformNumStr := DiagrammedPlatformNumStr
                            ELSE
                              ActualPlatformNumStr := Locations[TrainJourney_EndLocation].Location_PlatformOrFiddleyardNumStr;

                            DiagrammedTimeStr := TimeToHMStr(TrainJourney_DiagrammedArrivalTime);

                            IF (Train_CurrentStatus = Suspended) OR (Train_CurrentStatus = MissingAndSuspended) THEN
                              { keep it on the board as cancelled until its departure time has passed }
                              ActualTimeStr := '<b>' + DelayedStr + '</b>'
                            ELSE
                              IF Train_CurrentStatus = Cancelled THEN
                                ActualTimeStr := '<b>' + CancelledStr + '</b>'
                              ELSE
                                IF Train_JourneysArray[JourneyCount].TrainJourney_ActualArrivalTime <> 0 THEN
                                  ActualTimeStr := ArrivedStr
                                ELSE BEGIN
                                  { See if it's on time, early or delayed. This routine uses temporary variables as we're removing the seconds from them to make the
                                    comparison based only on hours and minutes
                                  }
                                  TempCurrentArrivalTime := TrainJourney_CurrentArrivalTime;
                                  TempDiagrammedArrivalTime := TrainJourney_DiagrammedArrivalTime;

                                  DecodeTime(TempCurrentArrivalTime, Hour, Min, Sec, MSec);
                                  Sec := 0;
                                  TempCurrentArrivalTime:= EncodeTime(Hour, Min, Sec, MSec);

                                  DecodeTime(TempDiagrammedArrivalTime, Hour, Min, Sec, MSec);
                                  Sec := 0;
                                  TempDiagrammedArrivalTime := EncodeTime(Hour, Min, Sec, MSec);

                                  IF SameTimeInHoursAndMinutesOnly(TempDiagrammedArrivalTime, TempCurrentArrivalTime) THEN
                                    ActualTimeStr := OnTimeStr
                                  ELSE
                                    IF CompareTime(TrainJourney_CurrentArrivalTime, TrainJourney_DiagrammedArrivalTime) < 0 THEN
                                      ActualTimeStr := TimeToHMStr(TrainJourney_CurrentArrivalTime)
                                    ELSE
                                      ActualTimeStr := '<b>' + TimeToHMStr(TrainJourney_CurrentArrivalTime) + '</b>';
                                END;
                          END;
                        END; {WITH}
                      END;
                    END;
                  END;
                END;
              END; {WITH}
            END;
          END; {WITH}
          Inc(DiagramsArrayPos);
        END; {WHILE}

        SortAndDisplayTimetableEntries(TimetableArray);

        IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN BEGIN
          StationMonitorsWebPage.Add('<div class="tttable">');
          StationMonitorsWebPage.Add(TimeColumnStr + '</div>');
          IF WebIncludeLocoChipInStationMonitors THEN
            StationMonitorsWebPage.Add(LocoColumnStr + '</div>');
          StationMonitorsWebPage.Add(LocationColumnStr + '</div>');
          StationMonitorsWebPage.Add(PlatformColumnStr + '</div>');
          StationMonitorsWebPage.Add(ExpectedColumnStr + '</div>');
          StationMonitorsWebPage.Add('</div>');
        END;
      END; {WITH}
      SetLength(TimetableArray, 0);
    EXCEPT
      ON E : Exception DO
        Log('EG ShowArrivals: ' + E.ClassName + ' error raised, with message: ' + E.Message);
    END; {TRY}
  END; { ShowArrivals }

  PROCEDURE ShowDepartures(Area : Integer);
  { Write out the departures screen }
  VAR
    JourneyCount : Integer;
    PreviouslyStopping : Boolean;
    T : Integer;
    TimetableArray : ARRAY OF StationMonitorsRec;

  BEGIN
    WITH StationMonitorsWindow.Canvas DO BEGIN
      IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN BEGIN
        TimeColumnStr :=       '    <div style="width:20%;float:left;">' + CRLF
                             + '    <span class="ttheading">Departs</span>';
        IF WebIncludeLocoChipInStationMonitors THEN BEGIN
          LocoColumnStr :=     '    <div style="width:10%;float:left;">' + CRLF
                             + '    <span class="ttheading">Loco</span>';
          LocationColumnStr := '    <div style="width:30%;float:left;">' + CRLF
                             + '    <span class="ttheading">To</span>';
        END ELSE BEGIN
          LocoColumnStr := '';
          LocationColumnStr := '    <div style="width:40%;float:left;">' + CRLF
                             + '    <span class="ttheading">To</span>';
        END;
        PlatformColumnStr :=   '    <div style="width:20%;float:left;">' + CRLF
                             + '    <span class="ttheading">Platform</span>';
        ExpectedColumnStr :=   '    <div style="width:20%;float:left;">' + CRLF
                             + '    <span class="ttheading">Expected</span>';
      END;

      IF RecordingMonitorScreens THEN
        Log('T Departures' + ' {NOUNITREF}');

      SetLength(TimetableArray, 0);

      { Compile a list for the timetable - this is unsorted, though }
      DiagramsArrayPos := 0;
      WHILE DiagramsArrayPos <= High(DiagramsArray) DO BEGIN
        T := DiagramsArray[DiagramsArrayPos];
        WITH Trains[T] DO BEGIN
          { Build a list of trains using the station }
          FOR JourneyCount := 0 TO High(Train_JourneysArray) DO BEGIN
            IF (JourneyCount = 0)
            OR ((JourneyCount > 0) AND Train_JourneysArray[JourneyCount - 1].TrainJourney_StoppingOnArrival)
            THEN
              PreviouslyStopping := True
            ELSE
              PreviouslyStopping := False;

            WITH Train_JourneysArray[JourneyCount] DO BEGIN
              IF NOT TrainJourney_Cleared AND PreviouslyStopping
              AND ((TrainJourney_ActualDepartureTime = 0)
                   OR (CompareTime(CurrentRailwayTime, IncMinute(TrainJourney_ActualDepartureTime, 1)) < 0))
              THEN BEGIN
                IF TrainJourney_StartArea = Area THEN BEGIN
                  IF DisplayNotForPublicUseTrainsInStationMonitors OR NOT TrainJourney_NotForPublicUse THEN BEGIN
                    IF ((Train_CurrentStatus = Cancelled)
                        AND (CompareTime(CurrentRailwayTime, TrainJourney_CurrentDepartureTime) <= 0))
                    OR (Train_CurrentStatus <> Cancelled)
                    THEN BEGIN
                      SetLength(TimetableArray, Length(TimetableArray) + 1);
                      WITH TimetableArray[High(TimetableArray)] DO BEGIN
                        LocationStr := TrainJourney_EndStationName;
                        IF TrainJourney_NotForPublicUse THEN
                          { show the location in a different font }
                          LocationStr := '<i>' + LocationStr + '</i>';

                        IF WebIncludeLocoChipInStationMonitors THEN
                          LocoChipStr := IntToStr(Train_LocoChip);

                        IF TrainJourney_DiagrammedStartLocation <> UnknownLocation THEN
                          DiagrammedPlatformNumStr := Locations[TrainJourney_DiagrammedStartLocation].Location_PlatformOrFiddleyardNumStr;

                        IF TrainJourney_DiagrammedStartLocation = TrainJourney_StartLocation THEN
                          ActualPlatformNumStr := DiagrammedPlatformNumStr
                        ELSE
                          ActualPlatformNumStr := Locations[TrainJourney_StartLocation].Location_PlatformOrFiddleyardNumStr;

                        DiagrammedTimeStr := TimeToHMStr(TrainJourney_DiagrammedDepartureTime);
                        IF (Train_CurrentStatus = Suspended) OR (Train_CurrentStatus = MissingAndSuspended) THEN
                          { keep it on the board as cancelled until its departure time has passed }
                          ActualTimeStr := DelayedStr
                        ELSE
                          IF Train_CurrentStatus = Cancelled THEN
                            ActualTimeStr := '<b>' + CancelledStr + '</b>'
                          ELSE BEGIN
                            IF TrainJourney_ActualDepartureTime <> 0 THEN
                              ActualTimeStr := '<b>' + DepartedStr + '</b>'
                            ELSE
                              IF SameTimeInHoursAndMinutesOnly(TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime)
                              THEN
                                ActualTimeStr := OnTimeStr
                              ELSE
                                IF CompareTime(TrainJourney_CurrentDepartureTime, TrainJourney_DiagrammedDepartureTime) < 0 THEN
                                  ActualTimeStr := TimeToHMStr(TrainJourney_CurrentDepartureTime)
                                ELSE
                                  ActualTimeStr := '<B>' + TimeToHMStr(TrainJourney_CurrentDepartureTime) + '</b>';
                          END;
                      END; {WITH}
                    END;
                  END;
                END;
              END;
            END; {WITH}
          END; {FOR}
        END; {WITH}
        Inc(DiagramsArrayPos);
      END; {WHILE}

      SortAndDisplayTimetableEntries(TimetableArray);

      IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN BEGIN
        StationMonitorsWebPage.Add('<div class="tttable">');
        StationMonitorsWebPage.Add(TimeColumnStr + '</div>');
        IF WebIncludeLocoChipInStationMonitors THEN
          StationMonitorsWebPage.Add(LocoColumnStr + '</div>');
        StationMonitorsWebPage.Add(LocationColumnStr + '</div>');
        StationMonitorsWebPage.Add(PlatformColumnStr + '</div>');
        StationMonitorsWebPage.Add(ExpectedColumnStr + '</div>');
        StationMonitorsWebPage.Add('</div>');
      END;
    END; {WITH}
    SetLength(TimetableArray, 0);
  END; { ShowDepartures }

BEGIN
  TRY
    IF StationMonitorsWebPageRequired THEN BEGIN
      IF StationMonitorsWebPage <> NIL THEN
        StationMonitorsWebPage.Free;

      StationMonitorsWebPage := TStringList.Create;
      StationMonitorsWebPage.Add('<!DOCTYPE html>');
      StationMonitorsWebPage.Add('<html>');
      StationMonitorsWebPage.Add('<head>');
      StationMonitorsWebPage.Add('<meta http-equiv="refresh" content="1">');
      StationMonitorsWebPage.Add('<link rel="stylesheet" type="text/css" href="style.css">');
      StationMonitorsWebPage.Add('<title>FWP Rail</title>');
      StationMonitorsWebPage.Add('</head>');
      StationMonitorsWebPage.Add('<body class="tt">');
      StationMonitorsWebPage.Add('<span class="ttstationname">' + GetStationNameFromArea(Area) + '</span>');
      StationMonitorsWebPage.Add('<span class="tttime">' + TimeToHMStr(CurrentRailwayTime) + '</span>');
      StationMonitorsWebPage.Add('<br />');
    END;

    StationMonitorsWindow.Borderstyle := bsNone;
    StationMonitorsWindow.Width := Screen.DeskTopWidth;
    StationMonitorsWindow.Height := Screen.DeskTopHeight;

    IF StationMonitorsWindow <> NIL THEN BEGIN
      WITH StationMonitorsWindow.Canvas DO BEGIN
        { Now draw it to the timetable screen }
        Font.Color := clWhite;

        IF StationMonitorDisplay = StationClockDisplay THEN BEGIN

        END ELSE BEGIN
          IF RecordingMonitorScreens THEN BEGIN
            DrawLineinLogFile(UnknownLocoChipAsZeroesStr, 'D', 'T', UnitRef);
            Log('T ' + GetStationNameFromArea(Area) + ' {NOUNITREF}');
          END;

          CurrentTimeStr := TimeToHMStr(CurrentRailwayTime);

          CASE StationMonitorDisplay OF
            StationArrivalsDisplay:
              BEGIN
                IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
                  StationMonitorsWebPage.Add('<div class="container" style="top:5%">');
                ShowArrivals(Area);
                IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
                  StationMonitorsWebPage.Add('</div>');
              END;
            StationDeparturesDisplay:
              BEGIN
                IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
                  StationMonitorsWebPage.Add('<div class="container" style="top:5%">');
                ShowDepartures(Area);
                IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
                  StationMonitorsWebPage.Add('</div>');
              END;
            StationArrivalsAndDeparturesDisplay:
              BEGIN
                IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
                  StationMonitorsWebPage.Add('<div class="container" style="top:5%">');
                ShowArrivals(Area);
                IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN BEGIN
                  StationMonitorsWebPage.Add('</div>');
                  StationMonitorsWebPage.Add('<div class="container" style="top:50%">');
                END;
                ShowDepartures(Area);
                IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN
                  StationMonitorsWebPage.Add('</div>');
              END;
          END; {CASE}

          IF RecordingMonitorScreens THEN
            DrawLineinLogFile(UnknownLocoChipAsZeroesStr, 'D', 'T', UnitRef);
        END;
      END;
    END; {WITH}

    IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) THEN BEGIN
      StationMonitorsWebPage.Add('</body>');
      StationMonitorsWebPage.Add('</html>');
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG DrawStationMonitorsWindow:' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { DrawStationMonitorsWindow }

PROCEDURE TStationMonitorsWindow.StationMonitorsFormCreate(Sender: TObject);
BEGIN
  StationMonitorsTCPServer.Open;
  AddLineToStationMonitorsWebDiagnosticsMemo(DateTimeToStr(Now) + ': server started');
END; { StationMonitorsFormCreate }

PROCEDURE TStationMonitorsWindow.StationMonitorsFormPaint(Sender: TObject);
BEGIN
  DrawStationMonitorsWindow(StationMonitorsCurrentArea);
END; { StationMonitorsWindowPaint }

PROCEDURE TStationMonitorsWindow.StationMonitorsTcpServerAccept(Sender: TObject; ClientSocket: TCustomIpClient);
TYPE
  ServedPageType = (StationServedPage, StyleSheetServedPage, ErrorServedPage);

VAR
  HTTPPos : Integer;
  HTTPStatusNumStr : String;
  HTTPStatusText : String;
  I : Integer;
  Line : String;
  Msg : String;
  ParametersArray : TStringDynArray;
  Path : String;
  ServedPage : ServedPageType;
  StationMonitorsDisplayOrderNum : Integer;
  StationStr : String;
  StyleSheetStream : TFileStream;
  TempStr : String;

BEGIN
  TRY
    { TCriticalSection allows a thread in a multithreaded application to temporarily block other threads from accessing a block of code }
    StationMonitorsCriticalSection.Enter;
    TRY
      IF StationMonitorsWebPageRequired AND (StationMonitorsWebPage <> NIL) AND NOT ProgramShuttingDown THEN BEGIN
        Line := ' ';
        WHILE ClientSocket.Connected AND (Line <> '') DO BEGIN
          Line := String(ClientSocket.ReceiveLn);

          IF InitVarsWindow <> NIL THEN
            AddLineToStationMonitorsWebDiagnosticsMemo('Rec''d: ' + Line);

          IF Copy(Line, 1, 3) = 'GET' THEN BEGIN
            HTTPPos := Pos('HTTP', Line);
            Path := UpperCase(Copy(Line, 5, HTTPPos - 6));
            AddLineToStationMonitorsWebDiagnosticsMemo('Path: ' + Path);
          END;
        END;

        Msg := '';
        ServedPage := ErrorServedPage;
        WebIncludeLocoChipInStationMonitors := False;
        StationMonitorsDisplayOrderNum := 0;

        IF Path = '/' THEN BEGIN
          { the default is the first listed station in the Areas database with arrivals and departures }
          ServedPage := StationServedPage;
          StationMonitorsDisplayOrderNum := 0;
          StationStr := GetStationMonitorsDisplayOrderStr(1);
          StationMonitorDisplay := StationArrivalsAndDeparturesDisplay;
        END ELSE
          IF Pos('/STYLE.CSS', Path) > 0 THEN
            ServedPage := StyleSheetServedPage
          ELSE BEGIN
            { Parse the path string }
            ParametersArray := SplitString(Path, '/');
            { Note: the first element of the array is a null string because the first character in the Path variable is an oblique, which is the delimiter }
            IF Length(ParametersArray) > 1 THEN BEGIN
              IF ParametersArray[1] <> '' THEN BEGIN
                { The second area is the name of the station }
                TempStr := ParametersArray[1];
                StationMonitorsDisplayOrderNum := GetStationNumFromStationMonitorsDisplayOrderNum(TempStr);
                StationStr := GetStationMonitorsDisplayOrderStr(StationMonitorsDisplayOrderNum);
                IF StationMonitorsDisplayOrderNum = -1 THEN BEGIN
                  Msg := 'Unknown station name: ' + ParametersArray[1];
                  ServedPage := ErrorServedPage;
                END ELSE BEGIN
                  StationStr := GetStationMonitorsDisplayOrderStr(StationMonitorsDisplayOrderNum);
                  ServedPage := StationServedPage;

                  IF Length(ParametersArray) > 2 THEN BEGIN
                    IF ParametersArray[2] <> '' THEN BEGIN
                      { The third area is whether we are providing arrivals, departures or both }
                      TempStr := ParametersArray[2];

                      IF TempStr = 'ARRIVALS' THEN
                        StationMonitorDisplay := StationArrivalsDisplay
                      ELSE
                        IF TempStr = 'DEPARTURES' THEN
                          StationMonitorDisplay := StationDeparturesDisplay
                        ELSE
                          IF (TempStr = 'ARRIVALSANDDEPARTURES')
                          OR (TempStr = 'ARRIVALS&DEPARTURES')
                          OR (TempStr = 'DEPARTURESANDARRIVALS')
                          OR (TempStr = 'DEPARTURES&ARRIVALS')
                          OR (TempStr = 'D&A')
                          OR (TempStr = 'A&D')
                          THEN
                            StationMonitorDisplay := StationArrivalsAndDeparturesDisplay
                          ELSE BEGIN
                            Msg := 'Unknown second parameter: ' + ParametersArray[2];
                            ServedPage := ErrorServedPage;
                          END;

                      IF Length(ParametersArray) > 3 THEN BEGIN
                        IF ParametersArray[3] <> '' THEN BEGIN
                          { The third area is whether we are providing arrivals, departures or both }
                          TempStr := ParametersArray[3];

                          IF TempStr = 'LOCOS' THEN
                            WebIncludeLocoChipInStationMonitors := True
                          ELSE BEGIN
                            Msg := 'Unknown third parameter: ' + ParametersArray[3];
                            ServedPage := ErrorServedPage;
                          END;
                        END;
                      END;
                    END;
                  END;
                END;
              END;
            END;
          END;

        CASE ServedPage OF
          StationServedPage:
            BEGIN
              HTTPStatusNumStr := '200';
              HTTPStatusText := 'OK';
              ClientSocket.SendLn(AnsiString('HTTP/1.0 ' + HTTPStatusNumStr + ' ' + HTTPStatusText));
              ClientSocket.SendLn('');

              AddLineToStationMonitorsWebDiagnosticsMemo(HTTPStatusNumStr + ' ' + HTTPStatusText);
              AddLineToStationMonitorsWebDiagnosticsMemo('Returning data for ' + StationStr);

              DrawStationMonitorsWindow(GetAreaFromStationMonitorsDisplayOrderNum(StationMonitorsDisplayOrderNum));
              IF (StationMonitorsWebPage.Count > 0) AND NOT ProgramShuttingDown THEN
                FOR I := 0 TO StationMonitorsWebPage.Count - 1 DO
                  ClientSocket.SendLn(AnsiString(StationMonitorsWebPage[I]));
            END;
          StyleSheetServedPage:
            BEGIN
              TRY
                StyleSheetStream := TFileStream.Create(PathToRailDataFiles + 'style.css', fmOpenRead);
                HTTPStatusNumStr := '200';
                HTTPStatusText := 'OK';
                ClientSocket.SendLn(AnsiString('HTTP/1.0 ' + HTTPStatusNumStr + ' ' + HTTPStatusText));
                ClientSocket.SendLn('');
                ClientSocket.SendStream(StyleSheetStream);
                StyleSheetStream.Free;
              EXCEPT
                ON E : Exception DO
                  Log('EG StyleSheetServedPage:' + E.ClassName + ' error raised, with message: '+ E.Message);
              END; {TRY}
            END;
          ErrorServedPage:
            BEGIN
              HTTPStatusNumStr := '404';
              HTTPStatusText := 'Not Found';
              ClientSocket.SendLn('');
              ClientSocket.SendLn(AnsiString('<H1>HTTP/1.0 ' + HTTPStatusNumStr + ' ' + HTTPStatusText + '</H1><H3>' + Msg + '</H3>'));
              AddLineToStationMonitorsWebDiagnosticsMemo('Area ' + Path + ' not known');
            END;
        END; {CASE}

        ClientSocket.Close;
      END;
    FINALLY
      StationMonitorsCriticalSection.Leave;
    END;
  EXCEPT
    ON E : Exception DO
      Log('EG StationMonitorsTcpServerAccept: ' + E.ClassName + ' error raised, with message: ' + E.Message);
  END; {TRY}
END; { StationMonitorsTcpServerAccept }

PROCEDURE CloseStationMonitorsWebPage(OUT OK : Boolean);
{ Close the station monitor web page if it exists }
BEGIN
  OK := False;
  IF StationMonitorsWebPage <> NIL THEN BEGIN
    StationMonitorsWebPage.Free;
    OK := True;
  END;
END; { CloseStationMonitorsWebPage }

INITIALIZATION

BEGIN
  StationMonitorsCriticalSection := TCriticalSection.Create;
END;

END { StationMonitors }.

