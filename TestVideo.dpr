program TestVideo;

uses
  Vcl.Forms,
  uTestMain in 'uTestMain.pas' {Form1},
  uVideoMain in 'uVideoMain.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
