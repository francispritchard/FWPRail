PROGRAM FWPRailWatchdog;

uses
  Vcl.Forms,
  WatchdogUnit in 'WatchdogUnit.pas' {FWPRailWatchdogWindow};

{$R RailResource.res}
{$R *.res}

BEGIN
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFWPRailWatchdogWindow, FWPRailWatchdogWindow);
  Application.Run;
END { RailWatchdog }.
