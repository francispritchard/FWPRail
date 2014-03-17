UNIT Logging;

INTERFACE

USES Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

TYPE
  TLoggingWindow = CLASS(TForm)
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  LoggingWindow: TLoggingWindow;

IMPLEMENTATION

{$R *.dfm}

END { Logging } .
