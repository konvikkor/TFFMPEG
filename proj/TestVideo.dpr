program TestVideo;

uses
  Vcl.Forms,
  uTestMain in '../src/uTestMain.pas' {Form1},
  uVideoMain in '../src/uVideoMain.pas',
  uMediaDisplay in '../src/uMediaDisplay.pas',
  uMediaConstant in '../src/uMediaConstant.pas',
  uVideoThread in '../src/uVideoThread.pas',
  uMediaReader in '../src/uMediaReader.pas',
  uMediaTimeLine in '../src/uMediaTimeLine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
