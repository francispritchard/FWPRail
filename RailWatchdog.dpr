PROGRAM RailWatchdog;

uses
  Vcl.Forms,
  Watchdog in 'Watchdog.pas' {WatchdogWindow};

{$R *.res}

BEGIN
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWatchdogWindow, WatchdogWindow);
  Application.Run;
END { RailWatchdog }.
