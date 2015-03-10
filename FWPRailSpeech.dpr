PROGRAM FWPRailSpeech;

uses
  Vcl.Forms,
  SpeechUnit in 'SpeechUnit.pas' {FWPRailSpeechWindow};

{$R RailResource.res}
{$R *.res}

BEGIN
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFWPRailSpeechWindow, FWPRailSpeechWindow);
  Application.Run;
END { RailWatchdog }.
