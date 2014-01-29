UNIT ProgressBar;

INTERFACE

USES
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls;
TYPE
  TRoutesWritingProgressBarWindow = CLASS(TForm)
    RoutesWritingProgressBar: TProgressBar;
    RoutesWritingCancelButton: TButton;
    PROCEDURE RoutesWritingCancelButtonClick(Sender: TObject);
    PROCEDURE RoutesWritingProgressBarDblClick(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    { Public declarations }
  END;

VAR
  RoutesWritingProgressBarWindow: TRoutesWritingProgressBarWindow;

IMPLEMENTATION

{$R *.dfm}

USES InitVars;

PROCEDURE TRoutesWritingProgressBarWindow.RoutesWritingCancelButtonClick(Sender: TObject);
BEGIN
  RouteWritingCancel := True;
END; { RoutesWritingCancelButtonClick }

PROCEDURE TRoutesWritingProgressBarWindow.RoutesWritingProgressBarDblClick(Sender: TObject);
BEGIN
  WindowState := wsMinimized;
END; { RoutesWritingProgressBarDblClick }

END { ProgressBar}.
