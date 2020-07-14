program MainTest;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form1};
  //uFFMPG in '..\uFFMPG.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
