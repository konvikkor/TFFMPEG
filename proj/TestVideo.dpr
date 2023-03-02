program testVideo;

uses
  Vcl.Forms,
  u3DCamera in '..\src\u3DCamera.pas',
  uFFMpegThead in '..\src\uFFMpegThead.pas',
  uFFMPG in '..\src\uFFMPG.pas',
  uMediaConstant in '..\src\uMediaConstant.pas',
  uMediaDisplay in '..\src\uMediaDisplay.pas',
  uMediaReader in '..\src\uMediaReader.pas',
  uMediaTimeLine in '..\src\uMediaTimeLine.pas',
  uVideoMain in '..\src\uVideoMain.pas',
  uVideoThread in '..\src\uVideoThread.pas',
  uTestMain in '..\src\uTestMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
