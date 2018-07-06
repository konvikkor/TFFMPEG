unit uFFMpegThead;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,

  Vcl.ExtCtrls,Math,System.SyncObjs,

  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FFTypes, FFUtils, System.DateUtils,
  libavcodec, libavcodec_avfft, libavdevice, libavfilter, libavfilter_avcodec,
  libavfilter_buffersink, libavfilter_buffersrc, libavfilter_formats,
  libavformat, libavformat_avio, libavformat_url, libavutil,
  libavutil_audio_fifo, libavutil_avstring, libavutil_bprint, libavutil_buffer,
  libavutil_channel_layout, libavutil_common, libavutil_cpu, libavutil_dict,
  libavutil_display, libavutil_error, libavutil_eval, libavutil_fifo,
  libavutil_file, libavutil_frame, libavutil_imgutils, libavutil_log,
  libavutil_mathematics, libavutil_md5, libavutil_mem, libavutil_motion_vector,
  libavutil_opt, libavutil_parseutils, libavutil_pixdesc, libavutil_pixfmt,
  libavutil_rational, libavutil_samplefmt, libavutil_time, libavutil_timestamp,
  libswresample, libswscale, sdl2, {sdl, {uResourcePaths,} System.Threading,
  uFFmpg;

type
  TOnStatusThead = procedure (var PlayStop:Boolean; var Close:Boolean) of object;
  TOnReadPaket   = procedure (var Deley:Cardinal) of object;
  TOnSeek        = procedure (var PlayStop:Boolean; var Close:Boolean) of object;
  TOnDecodeVIDEO = procedure (var Deley:Cardinal) of object;
  TOnDecodeAUDIO = procedure (var Deley:Cardinal) of object;
  TOnRunVIDEO    = procedure (var Deley:Cardinal) of object;
  TOnRunAUDIO    = procedure (var Deley:Cardinal) of object;
  TOnDelay       = procedure (var TotalDelay:Cardinal)of object;
  TOnHookFrame = procedure (Frame:TAVFrame; Paket:TAVPacket; Steam:TAVStream)of object;

  TMyDecodeThead = class (TThread)
  private
    FMyFFMpeg: TMyFFMpeg;
    FStop:Boolean;
    FOnStatus: TOnStatusThead;
    FDelay: TOnDelay;
    FDecodeAUDIO: TOnDecodeAUDIO;
    FDecodeVIDEO: TOnDecodeVIDEO;
    FReadPaket: TOnReadPaket;
    FRunAUDIO: TOnRunAUDIO;
    FRunVIDEO: TOnRunVIDEO;
    FSeek: TOnSeek;
    FOnHookFrame: TOnHookFrame;
  protected
    procedure Execute; override;
    procedure SyncStatus;
  public
    property MyFFMpeg:TMyFFMpeg Read FMyFFMpeg write FMyFFMpeg;
  published
    property OnStatusThead:TOnStatusThead read FOnStatus Write FOnStatus;
    property OnReadPaket:TOnReadPaket read FReadPaket Write FReadPaket;
    property OnSeek:TOnSeek read FSeek write FSeek;
    property OnDecodeVIDEO:TOnDecodeVIDEO read FDecodeVIDEO Write FDecodeVIDEO;
    property OnDecodeAUDIO:TOnDecodeAUDIO read FDecodeAUDIO Write FDecodeAUDIO;
    property OnRunVIDEO:TOnRunVIDEO read FRunVIDEO Write FRunVIDEO;
    property OnRunAUDIO:TOnRunAUDIO read FRunAUDIO Write FRunAUDIO;
    property OnDelay:TOnDelay read FDelay Write FDelay;
    property OnHookFrame:TOnHookFrame read FOnHookFrame Write FOnHookFrame;
  end;

implementation

{ TMyDecodeThead }

procedure TMyDecodeThead.Execute;
var Delay:cardinal;
    FClose:Boolean;
begin FClose:=False;
 //if not Assigned(FMyFFMpeg) then Exit;
 while (not Self.Terminated)do begin
  try
   Synchronize(SyncStatus);
   if (FStop) or (FClose) then begin
    if Assigned(FMyFFMpeg.Display) then FMyFFMpeg.Display.UpdateRender;
   if Assigned(FReadPaket) then begin
    av_packet_unref(@FMyFFMpeg.AVPacket);
    av_frame_unref(@FMyFFMpeg.AVFrame);
    if Assigned(FSeek) then FSeek(FStop,FClose);
    av_packet_unref(@FMyFFMpeg.AVPacket);
    av_frame_unref(@FMyFFMpeg.AVFrame);
   end;
    Sleep(100);
    Continue;
   end;
   Delay:=0;
   if Assigned(FSeek) then begin
    FSeek(FStop,FClose);
    av_packet_unref(@FMyFFMpeg.AVPacket);
    av_frame_unref(@FMyFFMpeg.AVFrame);
   end;
   if Assigned(FReadPaket) then begin
    FReadPaket(Delay);
    if FMyFFMpeg.AVPacket.size <=0 then begin
     FMyFFMpeg.Stop;
     Continue; // if end file then stop
    end;
    if Assigned(FDecodeAUDIO) then begin
     FDecodeAUDIO(Delay);
     if Delay = $FFFFFFFF then begin
      //Delay:=3;
      //Sleep(Delay);
     end else
     if Assigned(FRunAUDIO) then FRunAUDIO(Delay);
    end;
    if Assigned(FDecodeVIDEO) then begin
     FDecodeVIDEO(Delay);
     if Delay = $FFFFFFFF then begin
      //Delay:=3;
      //Sleep(Delay);
     end else
     if Assigned(FRunVIDEO) then FRunVIDEO(Delay);
    end;
   end;
   if Assigned(FDelay) then FDelay(Delay);
   Sleep(Delay);
  except
  end;
 end;
end;

procedure TMyDecodeThead.SyncStatus;
begin
  FStop:=FMyFFMpeg.GetStatusPlay;
end;

end.
