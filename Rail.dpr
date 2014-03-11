PROGRAM Rail;
(* FWP's Railway Program

  Copyright © F.W. Pritchard & A.M. Stokes 1998-2014. All Rights Reserved.

  v5.0  10/04/09 Now using Turbo Delphi (free version of Borland Developer Studio 2006)
  V6.0  27/03/11 And now using Embarcadero Delphi XE Professional
  v7.0  16/01/14 Now using XE5

 {$R RailResource.res} should be below this comment - this has the extra two icons in it - but sometimes Delphi removes it.
 {$R Rail.res}         should be below this comment - it has the version numbers in it - but sometimes Delphi removes it too.
*)

{$R RailResource.res} // {$R RailResource.res} should be to the left of this comment - this has the extra two icons in it - but sometimes Delphi removes it }
{$R Rail.res}         // {$R Rail.res} should be to the left of this comment - it has the version numbers in it - but sometimes Delphi removes it too }

USES
  Forms,
  Raildraw IN 'Raildraw.pas' {FWPRailMainWindow},
  Diagrams IN 'Diagrams.pas' {DiagramsWindow},
  MiscUtils IN 'MiscUtils.pas' {DebugWindow},
  RDC IN 'RDC.pas',
  Input IN 'Input.pas' {InputDialogueBox},
  Feedback IN 'Feedback.pas' {FeedbackWindow},
  Startup IN 'Startup.pas' {DebuggingOptionsWindow},
  Cuneo IN 'Cuneo.pas' {CuneoWindow},
  Locks IN 'Locks.pas' {LockListWindow},
  TCPIP IN 'TCPIP.pas' {TCPIPForm},
  Lenz IN 'Lenz.pas' {LenzWindow},
  LocoUtils IN 'LocoUtils.pas' {LocoUtilsWindow},
  InitVars IN 'InitVars.pas' {InitVarsWindow},
  GetTime IN 'GetTime.pas' {ClockWindow},
  Movement IN 'Movement.pas' {MovementWindow},
  Route IN 'Route.pas' {RouteWindow},
  Splash IN 'Splash.pas' {Splash},
  CreateRoute IN 'CreateRoute.pas' {CreateRouteDisplayColoursWindow},
  TestUnit IN 'TestUnit.pas' {TestUnitForm},
  StationMonitors IN 'StationMonitors.pas' {StationMonitorsWindow},
  ProgressBar IN 'ProgressBar.pas' {RoutesWritingProgressBarWindow},
  LocoDialogue IN 'LocoDialogue.pas' {LocoDialogueWindow},
  Help IN 'Help.pas' {HelpWindow},
  IniFiles IN 'IniFiles.pas',
  LocationData IN 'LocationData.pas' {LocationDataWindow},
  FWPShowMessageUnit IN 'FWPShowMessageUnit.pas' {FWPShowMessageWindow},
  Replay IN 'Replay.pas' {ReplayForm},
  Options IN 'Options.pas' {OptionsWindow},
  Edit IN 'Edit.pas' {EditWindow},
  WorkingTimetable IN 'WorkingTimetable.pas' {WorkingTimetableWindow};

VAR
  I : Integer;
  WantSplash : Boolean;

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
  Application.CreateForm(TFWPRailMainWindow, FWPRailMainWindow);
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

