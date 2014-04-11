PROGRAM Rail;
(* FWP's Railway Program

  Copyright © F.W. Pritchard & A.M. Stokes 1998-2014. All Rights Reserved.

  v5.0  10/04/09 Now using Turbo Delphi (free version of Borland Developer Studio 2006)
  V6.0  27/03/11 And now using Embarcadero Delphi XE Professional
  v7.0  16/01/14 Now using XE5

  Note the lines below should appear before BEGIN in the body of the code - this has the extra two icons in it - but sometimes Delphi removed it when they appeared beneath
  this comment

{$R RailResource.res}
{$R Rail.res}

*)
uses
  Forms,
  Raildraw in 'Raildraw.pas' {FWPRailWindow},
  Diagrams in 'Diagrams.pas' {DiagramsWindow},
  MiscUtils in 'MiscUtils.pas' {DebugWindow},
  RDC in 'RDC.pas',
  Input in 'Input.pas' {InputDialogueBox},
  Feedback in 'Feedback.pas' {FeedbackWindow},
  Startup in 'Startup.pas' {DebuggingOptionsWindow},
  Cuneo in 'Cuneo.pas' {CuneoWindow},
  Locks in 'Locks.pas' {LockListWindow},
  TCPIP in 'TCPIP.pas' {TCPIPForm},
  Lenz in 'Lenz.pas' {LenzWindow},
  LocoUtils in 'LocoUtils.pas' {LocoUtilsWindow},
  InitVars in 'InitVars.pas' {InitVarsWindow},
  GetTime in 'GetTime.pas' {ClockWindow},
  Movement in 'Movement.pas' {MovementWindow},
  Route in 'Route.pas' {RouteWindow},
  Splash in 'Splash.pas' {Splash},
  CreateRoute in 'CreateRoute.pas' {CreateRouteDisplayColoursWindow},
  TestUnit in 'TestUnit.pas' {TestUnitForm},
  StationMonitors in 'StationMonitors.pas' {StationMonitorsWindow},
  ProgressBar in 'ProgressBar.pas' {RoutesWritingProgressBarWindow},
  LocoDialogue in 'LocoDialogue.pas' {LocoDialogueWindow},
  Help in 'Help.pas' {HelpWindow},
  IniFiles in 'IniFiles.pas',
  LocationData in 'LocationData.pas' {LocationDataWindow},
  FWPShowMessageUnit in 'FWPShowMessageUnit.pas' {FWPShowMessageWindow},
  Replay in 'Replay.pas' {ReplayForm},
  Options in 'Options.pas' {OptionsWindow},
  Edit in 'Edit.pas' {EditWindow},
  WorkingTimetable in 'WorkingTimetable.pas' {WorkingTimetableWindow},
  Logging in 'Logging.pas' {LoggingWindow},
  Main in 'Main.pas' {MainWindow};

VAR
  I : Integer;
  WantSplash : Boolean;

{$R RailResource.res}
{$R Rail.res}

BEGIN
  WantSplash := True;

  Application.Initialize;
  Application.Title := 'FWP''s Rail';
  Application.HelpFile := '';
  FOR I := 1 TO ParamCount DO
    IF ParamStr(I) = '/nosplash' THEN
      WantSplash := False;

  IF WantSplash THEN BEGIN
    SplashForm := TSplashForm.Create(Application);
    SplashForm.Show;
    SplashForm.Update;
  END;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TFWPRailWindow, FWPRailWindow);
  Application.CreateForm(TRailDriverWindow, RailDriverWindow);
  Application.CreateForm(TDebugWindow, DebugWindow);
  Application.CreateForm(TDiagramsWindow, DiagramsWindow);
  Application.CreateForm(TInputDialogueBox, InputDialogueBox);
  Application.CreateForm(TFeedbackWindow, FeedbackWindow);
  Application.CreateForm(TDebuggingOptionsWindow, DebuggingOptionsWindow);
  Application.CreateForm(TCuneoWindow, CuneoWindow);
  Application.CreateForm(TLockListWindow, LockListWindow);
  Application.CreateForm(TLenzWindow, LenzWindow);
  Application.CreateForm(TLocoUtilsWindow, LocoUtilsWindow);
  Application.CreateForm(TInitVarsWindow, InitVarsWindow);
  Application.CreateForm(TClockWindow, ClockWindow);
  Application.CreateForm(TMovementWindow, MovementWindow);
  Application.CreateForm(TRouteWindow, RouteWindow);
  Application.CreateForm(TCreateRouteDisplayColoursWindow, CreateRouteDisplayColoursWindow);
  Application.CreateForm(TTestUnitForm, TestUnitForm);
  Application.CreateForm(TStationMonitorsWindow, StationMonitorsWindow);
  Application.CreateForm(TRoutesWritingProgressBarWindow, RoutesWritingProgressBarWindow);
  Application.CreateForm(TLocoDialogueWindow, LocoDialogueWindow);
  Application.CreateForm(THelpWindow, HelpWindow);
  Application.CreateForm(TLocationDataWindow, LocationDataWindow);
  Application.CreateForm(TFWPShowMessageWindow, FWPShowMessageWindow);
  Application.CreateForm(TReplayForm, ReplayForm);
  Application.CreateForm(TWorkingTimetableWindow, WorkingTimetableWindow);
  Application.CreateForm(TOptionsWindow, OptionsWindow);
  Application.CreateForm(TEditWindow, EditWindow);
  Application.Run;
END { Rail }.

