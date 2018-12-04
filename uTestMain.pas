unit uTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uVideoMain, Vcl.Menus, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.DateUtils;

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
    Timer1: TTimer;
    TabSheet2: TTabSheet;
    TrayIcon1: TTrayIcon;
    TaskDialog1: TTaskDialog;
    procedure Open1Click(Sender: TObject);
    procedure DecodePak1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    var MediaDecoder:TMediaDecoder;
    var MediaMainCore:TMediaMainCore;
    var MediaVideo:TMediaVideo;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DecodePak1Click(Sender: TObject);
var i,j:integer;
begin
  if not Assigned(MediaDecoder) then exit;
  if not Assigned(MediaMainCore) then exit;
  if not Assigned(MediaVideo) then exit;
  {i:=1;
  try
   repeat
    Inc(i);
   until (not tmp.ReadPaked)OR(i > 10000);
  except

  end;
  Memo2.Clear;
  tmp.DecodePakedFromFrame;
  Memo2.Lines.BeginUpdate;
  for I := 0 to TMP.VideoPacket.Count-1 do begin
    Memo2.Lines.Add('AVPacket.Items['+IntToStr(i)+'].stream_index='+IntToStr(TMP.VideoPacket.Items[i].stream_index));
    Memo2.Lines.Add('AVPacket.Items['+IntToStr(i)+'].size='+IntToStr(TMP.VideoPacket.Items[i].size));
    Memo2.Lines.Add('AVPacket.Items['+IntToStr(i)+'].flags='+IntToStr(TMP.VideoPacket.Items[i].flags));
    Memo2.Lines.Add('AVPacket.Items['+IntToStr(i)+'].pts='+IntToStr(TMP.VideoPacket.Items[i].pts));
    Memo2.Lines.Add('AVPacket.Items['+IntToStr(i)+'].dts='+IntToStr(TMP.VideoPacket.Items[i].dts));
    Memo2.Lines.Add('AVPacket.Items['+IntToStr(i)+'].duration='+IntToStr(TMP.VideoPacket.Items[i].duration));
    Memo2.Lines.Add('AVPacket.Items['+IntToStr(i)+'].pos='+IntToStr(TMP.VideoPacket.Items[i].pos));
    Memo2.Lines.Add('===========');
  end;
  Memo2.Lines.EndUpdate;
  Memo3.Clear;
  Memo3.Lines.BeginUpdate;
  for I := 0 to tmp.VideoFrame.Count-1 do begin
   for j := 0 to TMP.AVStream.Count-1 do begin
    if TMP.AVStream.Items[j].AVStream.index = TMP.CurrentVideoStream then begin
    Memo3.Lines.Add('Time: '+floatToStr(//UnixToDateTime(
      (
        TMP.VideoFrame.Items[i].pts * (
          TMP.AVStream.Items[j].AVStream^.time_base.num / TMP.AVStream.Items[j].AVStream^.time_base.den
        )
      ) (* Return Second *)
      *1000 (* return Milisecond *)
    )//)
    );
    Break;
    end;
   end;
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].key_frame='+IntToStr(TMP.VideoFrame.Items[i].key_frame));
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].width='+IntToStr(TMP.VideoFrame.Items[i].width));
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].height='+IntToStr(TMP.VideoFrame.Items[i].height));
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].pts='+IntToStr(TMP.VideoFrame.Items[i].pts));
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].pkt_pts='+IntToStr(TMP.VideoFrame.Items[i].pkt_pts));
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].pkt_dts='+IntToStr(TMP.VideoFrame.Items[i].pkt_dts));
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].pkt_pos='+IntToStr(TMP.VideoFrame.Items[i].pkt_pos));
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].pkt_duration='+IntToStr(TMP.VideoFrame.Items[i].pkt_duration));
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].coded_picture_number='+IntToStr(TMP.VideoFrame.Items[i].coded_picture_number));
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].display_picture_number='+IntToStr(TMP.VideoFrame.Items[i].display_picture_number));
    Memo3.Lines.Add('AVFrame.Items['+IntToStr(i)+'].best_effort_timestamp='+IntToStr(TMP.VideoFrame.Items[i].best_effort_timestamp));
    Memo3.Lines.Add('===========');
  end;
  Memo3.Lines.EndUpdate;   *}
end;

procedure TForm1.Open1Click(Sender: TObject);
var i:Integer;
begin
 if not OpenDialog1.Execute then Exit;
 if not Assigned(MediaDecoder) then MediaDecoder:=TMediaDecoder.Create(self);
 if not Assigned(MediaMainCore) then begin
  MediaMainCore:=TMediaMainCore.Create;
  MediaDecoder.MediaCore:=MediaMainCore;
 end;
 if Assigned(MediaMainCore) then MediaMainCore.CloseFile;
 MediaMainCore.OpenFile(OpenDialog1.FileName);
 Memo1.Lines.Add(OpenDialog1.FileName);
 Memo1.Lines.Add(MediaDecoder.GetStatus);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not Assigned(MediaDecoder) then exit;
  {StatusBar1.Panels[0].Text:='AVStream:'+IntToStr(TMP.AVStream.Count);
  StatusBar1.Panels[1].Text:='VPacket:'+IntToStr(TMP.VideoPacket.Count);
  StatusBar1.Panels[2].Text:='VFrame:'+IntToStr(TMP.VideoFrame.Count);
  StatusBar1.Panels[3].Text:='APacket:'+IntToStr(TMP.AudioPacket.Count);
  StatusBar1.Panels[4].Text:='GlobalTime:'+IntToStr(TMP.GetGlobalTime);}
end;

end.
