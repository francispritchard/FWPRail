PROGRAM RailWatchdog;

USES
  Vcl.Forms,
  Watchdog IN 'Watchdog.pas' {WatchdogWindow};

{$R *.res}

BEGIN
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWatchdogWindow, WatchdogWindow);
  Application.Run;
END {RailWatchdog}.
