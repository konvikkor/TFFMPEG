unit uVideoMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,

  Vcl.ExtCtrls,Math,System.SyncObjs,System.Generics.Collections,

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
  libswresample, libswscale, sdl2, {sdl, {uResourcePaths,} System.Threading;

const
  MAX_AUDIO_FRAME_SIZE = 192000; // 1 second of 48khz 32bit audio
  MODULE_VERSION = '2.0';
  WM_DecodeFrame = WM_USER+0;
  WM_PlayFrame = WM_USER+1;
  WM_StopFrame = WM_USER+2;
  WM_FreeFrame = WM_USER+3;
  //WM_PlayFrame = WM_USER+4;
  //WM_PlayFrame = WM_USER+5;
  //WM_PlayFrame = WM_USER+6;

type
  TOnError = procedure (Sender:TObject; ErrorCode:Integer; MSG:string) of object;

  ///function UnixToDateTime(const AValue: Int64): TDateTime;

  TStreamInfo = Packed record
   AVStream: PAVStream;
   data: array [0 .. 3] of PByte;
   linesize: array [0 .. 3] of Integer;
   bufsize: Integer;
   //AVPacket : PAVPacket;
   GotFrame:Boolean;
   au_convert_ctx: PSwrContext;
  end;

  TMediaBufferInfo = packed Record
    AVStream  : TAVStream;
    AVPacket  : TAVPacket;
    AVFrame   : TAVFrame;
    GotFrame  : PInteger;
  End;
  PMediaBufferInfo = ^TMediaBufferInfo;

  (* sync event *)
  TOnDelay = procedure (Need:Cardinal;var resDelay:Cardinal)of object;

  (* Video Event *)
  TOnRenderVideo = procedure (Data:PMediaBufferInfo) of object;
  TOnHookVideo = procedure (Sender:TObject; Data:PMediaBufferInfo)of object;

  (* Audio Event *)
  TOnRenderAudio = procedure (Data:PMediaBufferInfo) of object;
  TOnHookAudio = procedure (Sender:TObject; Data:PMediaBufferInfo)of object;

  TMediaBuffer = class(TThread) (* Base Class Media thead *)
  private
    function GetBufferSize: Cardinal;
  protected
    CS:TCriticalSection;
    CSTime:TCriticalSection;
    GlobalTime:Cardinal;
    Buffer: TArray<PMediaBufferInfo>;
    MaxBuff:Cardinal;
    AddIndex, ReadIndex:Cardinal; (* Navigate index in buffer *)
  public
    constructor Create(CreateSuspended: Boolean); overload;
    destructor Destroy; overload;
    procedure SyncTime(Time:Cardinal);
    function GetBufCount: Cardinal;
    function AddBufItem(Item: PMediaBufferInfo): Boolean;virtual;abstract;
    function GetBuffItem(Index: Cardinal): PMediaBufferInfo;virtual;abstract;
    function ClearBuffer: Boolean;virtual;abstract;
    Property BufferSize:Cardinal Read GetBufferSize;
  end;

  TMediaVideo = Class(TMediaBuffer)(* Video Play *)
  private
    TmpInfo:PMediaBufferInfo;
    FOnRenderVideo: TOnRenderVideo;
    Function GetData:Boolean;
    Function DelData:Boolean;
    procedure DecodePakedFromFrame;
  public
    function ClearBuffer: Boolean; override;
    function AddBufItem(Item: PMediaBufferInfo): Boolean; override;
    function GetBuffItem(Index: Cardinal): PMediaBufferInfo; override;
    Property OnRenderVideo:TOnRenderVideo Read FOnRenderVideo Write FOnRenderVideo;
  end;

  TMediaAudio = Class(TMediaBuffer)(* Audio Play *);

  TMediaMainCore = class (TComponent)
  private
    FAudioStream: Integer;
    FVideoStream: Integer;
    FOnError: TOnError;
  protected
    AVFormatContext:PAVFormatContext;
    AVStream:Tlist<TStreamInfo>;
  public
    Constructor Create;
    Destructor Destroy;
    procedure CloseFile;
    function GetVersion: string;
    procedure OpenFile(FileName: TFileName);
    function GetGlobalTime: Int64;
    property BestVideoStream :Integer read FVideoStream;
    Property BestAudioStream :Integer read FAudioStream;
    Property OnError:TOnError read FOnError Write FOnError;
  end;

  TMediaDisplay = class (TGraphicControl)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  (*TVideoTimeLine = class(TGraphicControl)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;*)

  TMediaDecoder = class(TComponent)
  private
    FAudio: TMediaAudio;
    FVideo: TMediaVideo;
    FCore: TMediaMainCore;
    FOnError: TOnError;
    MemPointTime:Int64;
    GlobalTime:Cardinal;
    Timer:TTimer;
    FStartTime: TTime;
    FEndTime: TTime;
    FBufferTime: TTime;
    FCursorTime:TTime;
    function ReadPaked: Boolean;
    procedure OnTimer(Sender: TObject);
    function GetCurrentTime: TTime;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Function GetStatus:string;
    Property MediaCore:TMediaMainCore Read FCore Write FCore default nil;
    property StartTime:TTime Read FStartTime;
    Property EndTime:TTime read FEndTime;
    Property CursorTime:TTime read GetCurrentTime;
    property BufferedTime:TTime read FBufferTime;
    property Video:TMediaVideo Read FVideo Write FVideo default nil;
    property Audio:TMediaAudio Read FAudio Write FAudio default nil;
    Property OnError:TOnError read FOnError Write FOnError;
    Function Play:Boolean;
    Function Stop:Boolean;
  end;

function DurationToStr(ADuration: Int64): string;

implementation

function DurationToStr(ADuration: Int64): string;
var
  hours, mins, secs, us: Integer;
begin
  if ADuration <> AV_NOPTS_VALUE then
  begin
    secs := (ADuration (*+ 5000*)) div AV_TIME_BASE;
    us := (ADuration (*+ 5000*)) mod AV_TIME_BASE;
    mins := secs div 60;
    secs := secs mod 60;
    hours := mins div 60;
    mins := mins mod 60;
    Result := String(Format('%.2d:%.2d:%.2d.%.3d', [hours, mins, secs,
      (1000 * us) div 1000000]));
  end
  else
    Result := 'N/A';
end;

{ TMainCore }
procedure TMediaMainCore.CloseFile;
var
  i: Integer;
begin
 if Assigned(AVFormatContext) then begin
    avformat_close_input(@AVFormatContext);
    AVFormatContext:=nil;
    for i := 0 to AVStream.Count-1 do begin
     avcodec_close(AVStream[i].AVStream.codec);
     AVStream[i].AVStream.codec.codec := nil;
    end;
    AVStream.Clear;
 end;
end;

constructor TMediaMainCore.Create;
begin
  inherited Create(nil);
  av_register_all();
  avcodec_register_all();
  AVStream:=TList<TStreamInfo>.Create;
  FVideoStream:=-1;
  FAudioStream:=-1;
end;

constructor TMediaDecoder.Create(AOwner: TComponent);
begin
  inherited;
  FStartTime:=MinDateTime;
  FEndTime:=MinDateTime;
  FBufferTime:=MinDateTime;
  FCursorTime:=MinDateTime;
  Timer:=TTimer.Create(Self);
  Timer.Enabled:=False;
  Timer.Interval:=15;
  Timer.OnTimer:=OnTimer;
end;

destructor TMediaDecoder.Destroy;
begin
  FreeAndNil(Timer);
  inherited;
end;

function TMediaDecoder.GetCurrentTime: TTime;
begin
  Result:=FCursorTime;
end;

function TMediaDecoder.GetStatus: string;
var i:Integer;
begin
 if not(Assigned(FCore) and Assigned(FCore.AVFormatContext)) then exit;
 Result:='TMP.CurrentAudioStream='+IntToStr(FCore.BestAudioStream)+sLineBreak+
    'AVFormatContext.start_time='+IntToStr(FCore.AVFormatContext.start_time)+sLineBreak+
    'AVFormatContext.duration='+IntToStr(FCore.AVFormatContext.duration)+' as string '+DurationToStr(FCore.AVFormatContext.duration)+sLineBreak+
    'AVFormatContext.bit_rate='+IntToStr(FCore.AVFormatContext.bit_rate)+sLineBreak+
    'AVFormatContext.max_delay='+IntToStr(FCore.AVFormatContext.max_delay)+sLineBreak+
    'AVFormatContext.nb_streams='+IntToStr(FCore.AVFormatContext.nb_streams)+sLineBreak+
    '==========================================================================='+sLineBreak;
 for i:= 0 to FCore.AVStream.Count-1 do begin
  Result:=Result+
  'Time Start?F '+FloatToStr(FCore.AVFormatContext.start_time *(FCore.AVStream.Items[i].AVStream.time_base.num / FCore.AVStream.Items[i].AVStream.time_base.den))+sLineBreak+
  'Time Start?R '+FormatDateTime('hh:mm:ss.zzz',IncMilliSecond(round(FCore.AVFormatContext.start_time *(FCore.AVStream.Items[i].AVStream.time_base.num / FCore.AVStream.Items[i].AVStream.time_base.den))))+sLineBreak+
  'Time End?R '+FormatDateTime('hh:mm:ss.zzz',IncMilliSecond(MinDateTime,FCore.AVFormatContext.duration div 1000))+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.index='+IntToStr(FCore.AVStream.Items[i].AVStream.index)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.id='+IntToStr(FCore.AVStream.Items[i].AVStream.id)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.start_time='+IntToStr(FCore.AVStream.Items[i].AVStream.start_time)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.pts.den='+IntToStr(FCore.AVStream.Items[i].AVStream.pts.den)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.pts.num='+IntToStr(FCore.AVStream.Items[i].AVStream.pts.num)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.pts.val='+IntToStr(FCore.AVStream.Items[i].AVStream.pts.val)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.time_base.num='+IntToStr(FCore.AVStream.Items[i].AVStream.time_base.num)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.time_base.den='+IntToStr(FCore.AVStream.Items[i].AVStream.time_base.den)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.nb_frames='+IntToStr(FCore.AVStream.Items[i].AVStream.nb_frames)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.r_frame_rate.num='+IntToStr(FCore.AVStream.Items[i].AVStream.r_frame_rate.num)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.r_frame_rate.den='+IntToStr(FCore.AVStream.Items[i].AVStream.r_frame_rate.den)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.avg_frame_rate.den='+IntToStr(FCore.AVStream.Items[i].AVStream.avg_frame_rate.den)+sLineBreak+
  'TMP.AVStream.Items['+IntToStr(i)+'].AVStream.avg_frame_rate.den='+IntToStr(FCore.AVStream.Items[i].AVStream.avg_frame_rate.den)+sLineBreak+
  '==========================================================================='+sLineBreak;
 end;

end;

procedure TMediaDecoder.OnTimer(Sender: TObject);
var neddpacked:Boolean;
begin neddpacked:=False;
  if Assigned(FVideo) then begin
    FVideo.SyncTime(GlobalTime);
    neddpacked:= 10 > FVideo.GetBufferSize;
  end;
  if Assigned(FAudio) then begin
    FAudio.SyncTime(GlobalTime);
    neddpacked:= neddpacked and (10 > FAudio.GetBufferSize);
  end;
  if neddpacked then begin
    if not ReadPaked then begin
      Timer.Enabled:=False;
    end;
  end;
  GlobalTime:=av_gettime - MemPointTime;
end;

function TMediaDecoder.Play: Boolean;
begin
  Result:=True;
  try
   Timer.Enabled:=True;
   MemPointTime:=av_gettime;
  except
    Result:=False;
    Timer.Enabled:=Result;
  end;
end;

Function TMediaDecoder.ReadPaked:Boolean;
var ret:Integer;
    MBI:PMediaBufferInfo;
  I: Integer;
begin
  Result:=False;
  if not Assigned(FCore.AVFormatContext) then Exit;
  GetMem(MBI,SizeOf(TMediaBufferInfo));
  FillChar(MBI.AVPacket,SizeOf(MBI.AVPacket),0);
  ret := av_read_frame(FCore.AVFormatContext, @MBI.AVPacket);
  if MBI.AVPacket.size <= 0 then Result:=False;
  if ret < 0 then begin
   try
    av_packet_unref(@MBI.AVPacket);
    (* If finaly paked then return False *)
    if Assigned(FOnError) then FOnError(Self,ret,'Can not read Frame ' + sLineBreak + av_err2str(ret));
    //else raise Exception.Create('Can not read Frame ' + sLineBreak + av_err2str(ret));
   finally
   end;
  end else begin
   (* Write Video Paked *)
   if (MBI.AVPacket.stream_index = FCore.BestVideoStream)and(Assigned(FVideo)) then begin
    for I := 0 to FCore.AVStream.Count -1 do begin
     if FCore.AVStream[i].AVStream.index = FCore.BestVideoStream then
          MBI.AVStream:=FCore.AVStream[i].AVStream^;
    end;
    FVideo.AddBufItem(MBI);// VideoPacket.Add(tmpAVPacket)
    FBufferTime:=IncMilliSecond(MinDateTime,
            (MBI.AVPacket.pts*(MBI.AVStream.time_base.num div MBI.AVStream.time_base.den))
            );
   end else begin
    (* Write Audio Paked *)
    if (MBI.AVPacket.stream_index = FCore.BestAudioStream)and(Assigned(FAudio)) then begin
      for I := 0 to FCore.AVStream.Count -1 do begin
       if FCore.AVStream[i].AVStream.index = FCore.BestAudioStream then
            MBI.AVStream:=FCore.AVStream[i].AVStream^;
      end;
      FAudio.AddBufItem(MBI); //AudioPacket.Add(tmpAVPacket)
      FBufferTime:=IncMilliSecond(MinDateTime,
            (MBI.AVPacket.pts*(MBI.AVStream.time_base.num div MBI.AVStream.time_base.den))
            );
    end else begin
      (* If not filtered then free paked *)
      av_packet_unref(@MBI.AVPacket);
      FreeMem(MBI,SizeOf(TMediaBufferInfo));
    end;
   end;
   Result:=True;
  end;
end;

function TMediaDecoder.Stop: Boolean;
begin
  Timer.Enabled:=False;
  MemPointTime:=0;
end;

function TMediaVideo.AddBufItem(Item: PMediaBufferInfo): Boolean;
var tmppos:Cardinal;
begin
  CS.Enter;
  try
    if Buffer[AddIndex] = nil then begin
      Buffer[AddIndex]:=Item;
    end else begin
     if Assigned(Buffer[AddIndex]) then av_packet_unref(@Buffer[AddIndex].AVPacket);
     if Assigned(Buffer[AddIndex]) then av_frame_unref(@Buffer[AddIndex].AVFrame);
     if Assigned(Buffer[AddIndex].GotFrame) then Dispose(Buffer[AddIndex].GotFrame);
     FreeMem(Buffer[AddIndex],Sizeof(TMediaBufferInfo));
     Buffer[AddIndex]:=nil;
     Buffer[AddIndex]:=Item;
    end;
    Inc(AddIndex);
    if AddIndex > MaxBuff then AddIndex:=0;
  finally
    CS.Leave;
  end;
end;

function TMediaVideo.ClearBuffer: Boolean;
var
  I: Integer;
begin
  CS.Enter;
  try
   for I := 0 to High(Buffer)-1 do begin
    if Buffer[i] = nil then Continue;
    if Assigned(Buffer[i]) then av_packet_unref(@Buffer[i].AVPacket);
    if Assigned(Buffer[i]) then av_frame_unref(@Buffer[i].AVFrame);
    if Assigned(Buffer[i].GotFrame) then Dispose(Buffer[i].GotFrame);
    FreeMem(Buffer[i],Sizeof(TMediaBufferInfo));
    Buffer[i]:=nil;
   end;
   ReadIndex:=0;
   AddIndex:=0;
  finally
    CS.Leave;
  end;
end;

procedure TMediaVideo.DecodePakedFromFrame;
var i,j,ret:Integer;
    pts:Int64;
begin
   (* Video Decode *)
  try
  if not GetData then raise Exception.Create('Cannot read Buffer Data');
  except
    DelData;
    exit;
  end;
    if not Assigned(TmpInfo.GotFrame) then New(TmpInfo.GotFrame);
    try
     FillChar(TmpInfo.AVFrame,SizeOf(TmpInfo.AVFrame),0);
     ret := avcodec_decode_video2(TmpInfo.AVStream.codec, @TmpInfo.AVFrame{@frame}, TmpInfo.GotFrame, @TmpInfo.AVPacket);
     if (ret < 0) then begin
      (*if Assigned(FOnError) then FOnError(Self,ret,'Error decoding video frame (' + string(av_err2str(ret)) + ')')
      else raise Exception.Create('Error decoding video frame (' + string(av_err2str(ret)) + ')');*)
     end;
    finally
      //VideoFrame.Add(tmpAVFrame); //av_frame_unref(tmpAVFrame)
      //FillChar(tmpAVFrame,SizeOf(tmpAVFrame),0);
      //FillChar(tmpAVPacket,SizeOf(tmpAVPacket),0);
      //Dispose(got_frame);
    end;
   (* Video Decode END *)
 (* Audio Decode *)
  //if AVPacket.Items[i].stream_index = CurrentAudioStream then begin
  //end;
 (* Audio Decode END*)
end;

destructor TMediaMainCore.Destroy;
begin
 try
  FreeAndNil(AVStream);
 finally
  inherited Destroy;
 end;
end;

function TMediaMainCore.GetGlobalTime: Int64;
var i:Int64;
begin
  //i:=av_gettime_relative;
  i:=av_gettime;
  Result:= i;//Round(i/AV_TIME_BASE)
end;

function TMediaMainCore.GetVersion: string;
begin
  Result:=MODULE_VERSION;
end;

procedure TMediaMainCore.OpenFile(FileName: TFileName);
var VideoFile:PAnsiChar;
    AVideoFile:AnsiString;
    ret,i:integer;
    TMPStreamInfo:TStreamInfo;
    avdec: PAVCodec;
    opts: PAVDictionary;
begin
 AVideoFile:=AnsiString(FileName);
 VideoFile:=PAnsiChar(AVideoFile);
  { * open input file, and allocate format context * }
  ret := avformat_open_input(@AVFormatContext, VideoFile, nil, nil);
  if ret < 0 then begin
    if Assigned(FOnError) then FOnError(Self,ret,'Could not open source file "' + FileName+'"'+sLineBreak+av_err2str(ret))
    else raise Exception.Create('Could not open source file "' + FileName+'"'+sLineBreak+av_err2str(ret));
  end;
  (* retrieve stream information *)
  ret := avformat_find_stream_info(AVFormatContext, nil);
  if ret < 0 then begin
    if Assigned(FOnError) then FOnError(Self,ret,'Could not find stream information' + sLineBreak + av_err2str(ret))
    else raise Exception.Create('Could not find stream information' + sLineBreak + av_err2str(ret));
  end;
  // Dump information about file onto standard error
  av_dump_format(AVFormatContext, 0, VideoFile, 0);

  {* Поиск основного потока видео *}
  ret := av_find_best_stream(AVFormatContext, AVMEDIA_TYPE_VIDEO, -1, -1, nil, 0);
  if ret < 0 then begin
    if Assigned(FOnError) then FOnError(Self,ret,'Could not find '+string(av_get_media_type_string(AVMEDIA_TYPE_VIDEO))+' stream in input file ''' +String(FileName) + '''')
    else raise Exception.Create('Could not find '+string(av_get_media_type_string(AVMEDIA_TYPE_VIDEO))+' stream in input file ''' +String(FileName) + '''');
  end else FVideoStream:=ret;
  {* Поиск основного потока аудио *}
  FVideoStream := av_find_best_stream(AVFormatContext, AVMEDIA_TYPE_AUDIO, -1, -1, nil, 0);
  (*ret*)
  (* if ret < 0 then
    raise Exception.Create('Error Message:Could not find ' +
      string(av_get_media_type_string(AVMEDIA_TYPE_AUDIO)) + ' stream in input file ''' +
      String(FileName) + '''')
  else audio_stream_idx:=ret;*)
  ///SetLength(Steams,FormatContext.nb_streams);------------
  for I := 0 to AVFormatContext.nb_streams-1 do begin
    if PAVstream(PPtrIdx(AVFormatContext.streams, I))^.codec.codec_type = AVMEDIA_TYPE_VIDEO then begin
     try
      TMPStreamInfo.AVStream:=PPtrIdx(AVFormatContext.streams, I);
      avdec := avcodec_find_decoder(TMPStreamInfo.AVStream.codec.codec_id);
      if not assigned(avdec) then begin
        if Assigned(FOnError) then FOnError(Self,-1,'Failed to find ' + string(av_get_media_type_string(TMPStreamInfo.AVStream.codec.codec_type)) + ' codec')
        else raise Exception.Create('Failed to find ' + string(av_get_media_type_string(TMPStreamInfo.AVStream.codec.codec_type)) + ' codec');
      end;
      (* Init the decoders, with or without reference counting *)
      //av_dict_set(@opts, 'refcounted_frames', '1', 0);  /// Test Function
      ret := avcodec_open2(TMPStreamInfo.AVStream.codec, avdec, @opts);
      if ret < 0 then
        if Assigned(FOnError) then FOnError(Self,ret,'Failed to open ' + string(av_get_media_type_string(TMPStreamInfo.AVStream.codec.codec_type)) + ' codec')
        else raise Exception.Create('Failed to open ' + string(av_get_media_type_string(TMPStreamInfo.AVStream.codec.codec_type)) + ' codec');
      ret := av_image_alloc(@TMPStreamInfo.data[0],@TMPStreamInfo.linesize[0],
                             TMPStreamInfo.AVStream.codec.width,
                             TMPStreamInfo.AVStream.codec.height,
                             TMPStreamInfo.AVStream.codec.pix_fmt,1);
      if ret < 0 then begin
        if Assigned(FOnError) then FOnError(Self,ret,'Could not allocate raw video buffer')
        else raise Exception.Create ('Could not allocate raw video buffer');
      end;
      TMPStreamInfo.bufsize := ret;
     finally
       AVStream.Add(TMPStreamInfo);
       FillChar(TMPStreamInfo,sizeOf(TStreamInfo),#0);
     end;
    end;
    {if PAVstream(PPtrIdx(AVFormatContext.streams, I))^.codec.codec_type = AVMEDIA_TYPE_AUDIO then begin
     TMPStreamInfo.AVStream := PPtrIdx(AVFormatContext.streams, I);
     avdec := avcodec_find_decoder(TMPStreamInfo.AVStream.codec.codec_id);
     if not assigned(avdec) then
       raise Exception.Create('Error Message:Failed to find ' + string(av_get_media_type_string(TMPStreamInfo.AVStream.codec.codec_type)) + ' codec');
     (* Init the decoders, with or without reference counting *)
     //av_dict_set(@opts, 'refcounted_frames', '1', 0);  /// Test Function
     ret := avcodec_open2(TMPStreamInfo.AVStream.codec, avdec, @opts);
     if ret < 0 then
       raise Exception.Create('Error Message:Failed to open ' + string(av_get_media_type_string(TMPStreamInfo.AVStream.codec.codec_type)) + ' codec');
     //Out Audio Param
     out_channel_layout:=AV_CH_LAYOUT_STEREO;
     //nb_samples: AAC-1024 MP3-1152
     out_nb_samples:=TMPStreamInfo.AVStream.codec.frame_size;
     out_sample_fmt:=AV_SAMPLE_FMT_S16;
     out_sample_rate:=44100;
     out_channels:=av_get_channel_layout_nb_channels(out_channel_layout);
     //Out Buffer Size
     out_buffer_size:=av_samples_get_buffer_size(nil,out_channels ,out_nb_samples,out_sample_fmt, 1);
     out_buffer:=av_malloc(MAX_AUDIO_FRAME_SIZE*2);
    end;}
  end;
end;

{ TMediaDisplay }

constructor TMediaDisplay.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TMediaDisplay.Destroy;
begin

  inherited;
end;

procedure TMediaDisplay.Paint;
begin
  //inherited; //?
  (* Message Pain frame *)
end;

{ TMediaBuffer }

constructor TMediaBuffer.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  CSTime:=TCriticalSection.Create;
  CS:=TCriticalSection.Create;
  AddIndex:=0;
  ReadIndex:=0;
  GlobalTime:=0;
end;

destructor TMediaBuffer.Destroy;
begin
  FreeAndNil(CS);
  FreeAndNil(CSTime);
  inherited;
end;

function TMediaBuffer.GetBufCount: Cardinal;
var I:Cardinal;
begin
  CS.Enter;
  Result:=0;
  try
   for I := 0 to High(Buffer)-1 do
    if Buffer[i] <> nil then Inc(Result);
  finally
    CS.Leave;
  end;
end;

function TMediaBuffer.GetBufferSize: Cardinal;
begin
  Result:=High(Buffer);
end;

procedure TMediaBuffer.SyncTime(Time: Cardinal);
begin
  CSTime.Enter;
  try
    GlobalTime:=Time;
  finally
    CSTime.Leave;
  end;
end;

function TMediaVideo.DelData: Boolean;
begin
  if Assigned(TmpInfo) then av_packet_unref(@TmpInfo.AVPacket);
  if Assigned(TmpInfo) then av_frame_unref(@TmpInfo.AVFrame);
  FreeMem(TmpInfo,Sizeof(TMediaBufferInfo));
  TmpInfo:=nil;
end;

function TMediaVideo.GetBuffItem(Index: Cardinal): PMediaBufferInfo;
begin
  if Index > MaxBuff then raise Exception.Create('Out of range');
  Result:=Buffer[Index];
end;

function TMediaVideo.GetData: Boolean;
begin
  TmpInfo:=Buffer[ReadIndex];
  Inc(ReadIndex);
  if ReadIndex > MaxBuff then ReadIndex:=0;
end;

end.
