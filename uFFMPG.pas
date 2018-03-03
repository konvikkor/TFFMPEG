unit uFFMPG;

interface

uses
  FFTypes, libavcodec, libavcodec_avfft, libavdevice, libavfilter,
  libavfilter_avcodec, libavfilter_buffersink, libavfilter_buffersrc,
  libavfilter_formats, libavformat, libavformat_avio, libavformat_url, libavutil,
  libavutil_audio_fifo, libavutil_avstring, libavutil_bprint, libavutil_buffer,
  libavutil_channel_layout, libavutil_common, libavutil_cpu, libavutil_dict,
  libavutil_display, libavutil_error, libavutil_eval, libavutil_fifo,
  libavutil_file, libavutil_frame, libavutil_hwcontext,
  libavutil_imgutils, libavutil_log, libavutil_mathematics, libavutil_md5,
  libavutil_mem, libavutil_motion_vector, libavutil_opt, libavutil_parseutils,
  libavutil_pixdesc, libavutil_pixfmt, libavutil_rational, libavutil_samplefmt,
  libavutil_time, libavutil_timestamp, libswresample, libswscale, sdl2, sdl2_gfx,
  sdl2_image, sdl2_mixer, sdl2_net, sdl2_ttf,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, FFUtils;

Type
  TFMPEG_ERROR = procedure (ErrorCode:Integer; ErrorMessage:string) of object;
  TFMPEGLoader = class
  private
    FlastError: Integer;
    FonError: TFMPEG_ERROR;
  protected
   FileInfo:PAVFormatContext;
   Video:packed record
    StreamIndex:Integer;
    Stream:PAVStream;
   end;
   Audio:packed record
    StreamIndex:Integer;
    Stream:PAVStream;
   end;
   Function OpenFile(FileName:TFileName):Boolean;
   function FinsStreams:Boolean;
   function LoadStreams:Boolean;
   //function FindStream():Boolean;
  public
    constructor Create;
    Property LastError:Integer read FlastError default -1;
    Property OnError:TFMPEG_ERROR read FonError Write FonError;
  end;
  TFMPEGDisplay = class
  private
  protected
  public
  end;
  TFFMPEG = class (TPanel)
  public
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property VerticalAlignment;
    property Visible;
    property StyleElements;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

procedure Register;
begin
  RegisterComponents('FFMPEG', [TFFMPEG]);
end;

{ TFMPEGLoader }

constructor TFMPEGLoader.Create;
begin
  FileInfo:=nil;
  Video.Stream:=nil;
end;

function TFMPEGLoader.FinsStreams: Boolean;
begin Result:=True;
  FlastError:= avformat_find_stream_info(FileInfo , nil);
  if  FlastError < 0 then
  begin
    if Assigned(FonError) then FonError(FlastError,format('Could not find stream information', []));
    Result:=False;
  end else av_dump_format(FileInfo, 0, PAnsiChar(@FileInfo.filename[0]), 0);
end;

function TFMPEGLoader.LoadStreams: Boolean;
var tmpstream:PAVStream;
  i:Integer;
begin
  tmpstream:=FileInfo.streams;
   for i:=0 to FileInfo.nb_streams-1 do
   begin
      if ((tmpstream^.codec.codec_type =  AVMEDIA_TYPE_VIDEO) and (Video.StreamIndex<0)) then
      begin
        Video.StreamIndex := i;
      end else
      if ((tmpstream^.codec.codec_type =  AVMEDIA_TYPE_AUDIO) and (Audio.StreamIndex < 0)) then
      begin
        Audio.StreamIndex := i;
      end;
      inc(tmpstream);
   end;


  if (Audio.StreamIndex >= 0) then
    form1.stream_component_open(is_, audio_index);

  if(Video.StreamIndex >= 0) then
    form1.stream_component_open(is_, video_index);

  if ((is_.videoStream < 0) or (is_.audioStream < 0)) then
  begin
    showmessage(format('%s: could not open codecs', [ansistring(is_.filename)]));
    goto fail;
  end;
end;

function TFMPEGLoader.OpenFile(FileName: TFileName): Boolean;
begin  Result:=True;
  // Open video file
  FlastError:=avformat_open_input(@FileInfo, PAnsiChar(AnsiString(FileName)), nil, nil);
  if LastError<>0 then
  begin
    if Assigned(FonError) then FonError(LastError,format('Could not open source file %s', [ansistring(filename)]));
    Result:=False;
  end;
end;

end.
