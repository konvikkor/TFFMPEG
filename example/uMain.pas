unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, uFFMPG, Vcl.Menus,
  Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Close1: TMenuItem;
    Actions1: TMenuItem;
    Play1: TMenuItem;
    Stop1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    procedure Open1Click(Sender: TObject);
    procedure Play1Click(Sender: TObject);
    procedure Stop1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Open1Click(Sender: TObject);
begin
 if OpenDialog1.Execute then begin
  MyFFMpeg1.OpenFile(OpenDialog1.FileName);
  Self.Caption:=string(
  MyFFMpeg1.AVStream[MyFFMpeg1.VideoStreamIndex].stream.codec.codec.long_name)+' - ('+
  inttostr(MyFFMpeg1.AVStream[MyFFMpeg1.VideoStreamIndex].stream.codec.width)+'x'+
  inttostr(MyFFMpeg1.AVStream[MyFFMpeg1.VideoStreamIndex].stream.codec.height)+')';
 end;
end;

procedure TForm1.Play1Click(Sender: TObject);
begin
  MyFFMpeg1.Play;
end;

procedure TForm1.Stop1Click(Sender: TObject);
begin
  MyFFMpeg1.Stop;
end;

end.
