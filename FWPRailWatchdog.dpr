PROGRAM FWPRailWatchdog;

uses
  Vcl.Forms,
  WatchdogUnit in 'WatchdogUnit.pas' {WatchdogUnitWindow};

{$R RailResource.res}
{$R *.res}

BEGIN
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWatchdogUnitWindow, WatchdogUnitWindow);
  Application.Run;
END { RailWatchdog }.
