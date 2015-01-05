PROGRAM Rail;
(* FWP's Railway Program

  Copyright © F.W. Pritchard & A.M. Stokes 1998-2015. All Rights Reserved.

  v5.0  10/04/09 Now using Turbo Delphi (free version of Borland Developer Studio 2006)
  V6.0  27/03/11 And now using Embarcadero Delphi XE Professional
  v7.0  16/01/14 Now using XE5

  Note the lines below should appear before BEGIN in the body of the code - this has the extra two icons in it - but sometimes Delphi removed it when it appeared
  immediately beneath this comment

{$R RailResource.res}
{$R Rail.res}

*)
USES
  Forms,
  Windows,
  Raildraw IN 'Raildraw.pas' {FWPRailWindow},
  Diagrams IN 'Diagrams.pas' {DiagramsWindow},
  MiscUtils IN 'MiscUtils.pas' {DebugWindow},
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
  CreateRoute IN 'CreateRoute.pas' {DisplayLineColoursWindow},
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
  WorkingTimetable IN 'WorkingTimetable.pas' {WorkingTimetableWindow},
  Logging IN 'Logging.pas' {LoggingWindow},
  Main IN 'Main.pas' {MainWindow},
  Train IN 'Train.pas' {TrainForm},
  DataCheck IN 'DataCheck.pas' {DataCheckForm},
  Install IN 'Install.pas' {InstallForm};

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
  Application.ShowMainForm := False;
  Application.MainFormOnTaskbar := False;

  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TTrainForm, TrainForm);
  Application.CreateForm(TDataCheckForm, DataCheckForm);
  Application.CreateForm(TInstallForm, InstallForm);
  Application.Run;
END { Rail }.

