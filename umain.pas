{
Resource Files Made Easy
http://delphi.about.com/library/weekly/aa113099a.htm
How Delphi uses standard Windows-format resource files.

********************************************
Zarko Gajic
About.com Guide to Delphi Programming
http://delphi.about.com
email: delphi.guide@about.com
********************************************
}


unit umain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TfrMain = class(TForm)
    Image1: TImage;
    btnCanvasPic: TButton;
    btnUseCursor: TButton;
    btnChangeIcon: TButton;
    btnLoadPic: TButton;
    Timer1: TTimer;
    procedure btnCanvasPicClick(Sender: TObject);
    procedure btnLoadPicClick(Sender: TObject);
    procedure btnUseCursorClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    nrIco : Integer;
    MinIcon : array[0..1] of TIcon;
  end;

var
  frMain: TfrMain;

implementation

{$R *.DFM}
{$R DPABOUT.RES}



procedure TfrMain.btnCanvasPicClick(Sender: TObject);
var bBitmap : TBitmap;
begin
 bBitmap := TBitmap.Create;
 try
  bBitmap.Handle := LoadBitmap(hInstance, 'ATHENA');
  Image1.Width := bBitmap.Width;
  Image1.Height := bBitmap.Height;
  Image1.Canvas.Draw(0,0,bBitmap);
 finally
  bBitmap.Free;
 end;
end;


procedure TfrMain.btnLoadPicClick(Sender: TObject);
begin
Image1.Picture.Bitmap.LoadFromResourceName(hInstance,'EARTH');
end;

procedure TfrMain.btnUseCursorClick(Sender: TObject);
const NewCursor = 1;
begin
Screen.Cursors[NewCursor] := LoadCursor(hInstance,'CURHAND');
Image1.Cursor := NewCursor;
end;

procedure TfrMain.Timer1Timer(Sender: TObject);
begin
 if IsIconic(Application.Handle) then begin
  NrIco:=(NrIco+1) mod 2;
  Application.Icon:=MinIcon[NrIco];
 end;
end;

procedure TfrMain.FormCreate(Sender: TObject);
begin
 MinIcon[0]:=TIcon.Create;
 MinIcon[1]:=TIcon.Create;
 MinIcon[0].Handle:=LoadIcon(hInstance,'ICOOK');
 MinIcon[1].Handle:=LoadIcon(hInstance,'ICOFOLD');
 NrIco:=0;
 Timer1.Interval:=200;
end;

procedure TfrMain.FormDestroy(Sender: TObject);
begin
 MinIcon[0].Free;
 MinIcon[1].Free;
end;

end.
