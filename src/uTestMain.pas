unit uTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uVideoMain, Vcl.Menus, Vcl.ComCtrls,  OpenGL,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.DateUtils, uMediaDisplay;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    N1: TMenuItem;
    DecodePak1: TMenuItem;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Memo1: TMemo;
    Memo2: TMemo;
    Splitter1: TSplitter;
    Memo3: TMemo;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;
    TabSheet2: TTabSheet;
    TrayIcon1: TTrayIcon;
    TaskDialog1: TTaskDialog;
    Play1: TMenuItem;
    EST1: TMenuItem;
    TrackBar1: TTrackBar;
    Stop1: TMenuItem;
    OpenDialog2: TOpenDialog;
    TabSheet3: TTabSheet;
    MediaDisplay: TMediaDisplay;
    Media: TMediaDecoder;
    procedure Open1Click(Sender: TObject);
    procedure EST1Click(Sender: TObject);
    procedure Play1Click(Sender: TObject);
    procedure Stop1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  BMP:TBitmap;

implementation

{$R *.dfm}

procedure TForm1.EST1Click(Sender: TObject);
var
  Texture:Cardinal;
begin
  glClear(GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT or GL_COLOR_BUFFER_BIT);//GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  if OpenDialog2.Execute then begin
   BMP:=TBitmap.Create;
   try
    BMP.LoadFromFile(OpenDialog2.FileName);
    MediaDisplay.SetBitmap(BMP,0,0);
   finally
     FreeAndNil(BMP);
   end;
  end;
  //SwapBuffers(MediaDisplay.HDC);
end;

procedure TForm1.Open1Click(Sender: TObject);
var i:Integer;
begin
 if not OpenDialog1.Execute then Exit;
 Media.CloseFile;
 Media.OpenFile(OpenDialog1.FileName);
end;

procedure TForm1.Play1Click(Sender: TObject);
begin
  if not Assigned(Media) then exit;
  //MediaDisplay.InitSDL;
  Media.SeekVideo(0);
  Media.Start;
end;

procedure TForm1.Stop1Click(Sender: TObject);
begin
 if Assigned(Media) then Media.Stop;
end;

end.
