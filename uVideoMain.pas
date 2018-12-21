unit uVideoMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  (* Media modules *)
  uMediaDisplay,uMediaConstant,

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
  libswresample, libswscale, sdl2, {SDL2_ttf,{sdl, {uResourcePaths,} System.Threading,
  uVideoThread;

type
  TMediaCore = class(TComponent)
  private
    FDisplay: TMediaDisplay;
    FOnError: TOnError;
  protected
    CS:TCriticalSection;
    (*Global File*)
    FAVFormatContext:PAVFormatContext;
    FAVPacket:PAVPacket;
    CurrentTime:Ttime;
    BufferTime:Ttime;
    (*Video*)
    FVideoIdx:Integer;
    FVideoStrem:PAVStream;
    FVideoFrame:PAVFrame;
    FVideoGotFrame:PInteger;
    LastVideoThreadRead:Integer;
    FVideoBuffer:TMediaBuffer;
  public
    Constructor Create;
    Destructor Destroy; override;
    Function ReadPacked:Integer;overload;
    Function ReadPacked(var Pack:PAVPacket):Integer;overload;
    Function ReadPacked(Sender:TObject;var Pack:PAVPacket):Integer;overload;
    function ReadVideoPacked(Sender:TObject;var Pack:Pointer):Integer;
    Function DecodeVideo:Integer;overload;
    Function DecodeVideo(Sender:TObject;var Pack:PAVPacket; var Frame:PAVFrame; Var Gotframe:PInteger):Integer;overload;
    Function SeekTS(StreamID:Integer;TS:UInt64):Integer;
    Function OpenFile(FileName:TFileName):Integer;
    Function CloseFile:Boolean;
  published
    Property Display:TMediaDisplay Read FDisplay Write FDisplay;
    Property OnError:TOnError read FOnError Write FOnError;
  end;

  TMediaDecoder = class;

  TMediaReader = class (TThread)
  private
    FOnIsPlayed: TOnIsPlayed;
    FOnError: TOnError;
    FOnReadVideoPacked: TOnReadVideoPacked;
    FOnDecodeFrame: TOnDecodeVideo;
    FSyncTime: TOnSyncTime;
    FOnRenderVideoFrame: TOnRenderVideoFrame;
  protected
    FVideo:Array [0..1] of TVideoThread;
    FMediaDecoder:TMediaDecoder;
    procedure Execute; override;
  published
    constructor Create(MediaDecoder:TMediaDecoder);
    Property OnSyncTime:TOnSyncTime read FSyncTime Write FSyncTime;
    Property OnDecodeFrame:TOnDecodeVideo read FOnDecodeFrame Write FOnDecodeFrame;
    property OnReadVideoPacked:TOnReadVideoPacked read FOnReadVideoPacked Write FOnReadVideoPacked;
    property OnRenderVideoFrame:TOnRenderVideoFrame read FOnRenderVideoFrame Write FOnRenderVideoFrame;
    property OnIsPlayed:TOnIsPlayed read FOnIsPlayed write FOnIsPlayed;
    property OnError:TOnError Read FOnError Write FOnError;
  end;

  TMediaDecoder = class(TMediaCore)
  private
    FPlay: Boolean;
    GlobalTime:UInt64;
    StartTime:UInt64;
    StartT:TTime;
    Timer:TTimer;
    Last_pts:Int64;
  protected
    //FVideo:Array [0..0] of TVideoThread;
    FMediaReader:TMediaReader;
    Procedure OnTimer(sender:TObject);
    procedure OnIsPlayed(Sender:TObject;var Play:Boolean);
    procedure OnSyncTime(Sender:TObject;var GT:UInt64; Pack:PAVPacket; var Delay:Int64);
  public
    constructor Create;
    destructor Destroy;
    function Start:Boolean;
    property Play:Boolean Read FPlay default false;
    function GetCurrentTime:string;
    Function GetBufferTime:string;
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

{ TMediaCore }

function TMediaCore.CloseFile: Boolean;
begin
  {for I := 0 to High(Steams)-1 do begin
   if Assigned(Steams[i].stream) then
    if Assigned(Steams[i].stream.codec) then begin
       avcodec_close(Steams[i].stream.codec);
       Steams[i].stream.codec := nil;
    end;
  end;}
  if Assigned(FAVFormatContext) then begin
      avformat_close_input(@FAVFormatContext);
      FAVFormatContext := nil;
  end;
end;

constructor TMediaCore.Create;
begin
  av_register_all();
  avcodec_register_all();
  FAVPacket:=av_packet_alloc;
  FVideoFrame:=av_frame_alloc;
  New(FVideoGotFrame);
  CS:=TCriticalSection.Create;
end;

function TMediaCore.DecodeVideo: Integer;
begin
  Result := avcodec_decode_video2(FVideoStrem^.codec, FVideoFrame, FVideoGotFrame, FAVPacket);
  if (Result < 0) then
  begin
    if Assigned(FOnError) then FOnError(Self,Result,'DecodeVideo : '+av_err2str(Result));
  end;
end;

function TMediaCore.DecodeVideo(Sender:TObject;var Pack: PAVPacket; var Frame: PAVFrame;
  var Gotframe: PInteger): Integer;
begin
 if Assigned(CS) then CS.Enter;
 try
  Result := avcodec_decode_video2(FVideoStrem^.codec, Frame, Gotframe, Pack);
  if (Result < 0) then
  begin
    if Assigned(FOnError) then FOnError(Self,Result,'DecodeVideo : '+av_err2str(Result));
  end;
 finally
   if Assigned(CS) then CS.Leave;
 end;
end;

destructor TMediaCore.Destroy;
begin
  FreeAndNil(CS);
  inherited;
end;

function TMediaCore.OpenFile(FileName: TFileName): Integer;
var i:Integer;
  avdec: PAVCodec;
  opts: PAVDictionary;
  tmp:AnsiString;
  VideoFile:PAnsiChar;
begin opts:=nil;
 tmp:=AnsiString(FileName);
 VideoFile:=PAnsiChar(tmp);
 try
  { * open input file, and allocate format context * }
  Result := avformat_open_input(@FAVFormatContext, VideoFile, nil, nil);
  if Result < 0 then begin
    if Assigned(FOnError) then FOnError(self,Result,'Could not open source file "' + FileName+'" : '+av_err2str(Result));
    Exit;
  end;
  (* retrieve stream information *)
  Result := avformat_find_stream_info(FAVFormatContext, nil);
  if Result < 0 then begin
    if Assigned(FOnError)then FOnError(self,result,'Error Message:Could not find stream information' + sLineBreak + av_err2str(result));
    exit;
  end;
  // Dump information about file onto standard error
  //av_dump_format(FormatContext, 0, VideoFile, 0);
  {* Поиск основного потока видео *}
  Result := av_find_best_stream(FAVFormatContext, AVMEDIA_TYPE_VIDEO, -1, -1, nil, 0);
  if Result < 0 then begin
    if Assigned(FOnError) then FOnError(self,result,'Error Message:Could not find ' +
      string(av_get_media_type_string(AVMEDIA_TYPE_VIDEO)) + ' stream in input file ''' +
      String(FileName) + '''');
    exit;
  end else FVideoIdx:=Result;
  {* Поиск основного потока аудио *}
  (*ret*)//audio_stream_idx := av_find_best_stream(FormatContext, AVMEDIA_TYPE_AUDIO, -1, -1, nil, 0);
  (* if ret < 0 then
    raise Exception.Create('Error Message:Could not find ' +
      string(av_get_media_type_string(AVMEDIA_TYPE_AUDIO)) + ' stream in input file ''' +
      String(FileName) + '''')
  else audio_stream_idx:=ret;*)
  {* Открываем потоки файла в массив для быстрого доступа *}
  for I := 0 to FAVFormatContext.nb_streams-1 do begin
    if PAVstream(PPtrIdx(FAVFormatContext.streams, I))^.codec.codec_type = AVMEDIA_TYPE_VIDEO then begin
     FVideoStrem := PPtrIdx(FAVFormatContext.streams, I);
     avdec := avcodec_find_decoder(FVideoStrem.codec.codec_id);
     if not assigned(avdec) then begin
       if Assigned(FOnError) then FOnError(self,result,'Error Message:Failed to find ' + string(av_get_media_type_string(FVideoStrem.codec.codec_type)) + ' codec');
     end;
     (* Init the decoders, with or without reference counting *)
     //av_dict_set(@opts, 'refcounted_frames', '1', 0);  /// Test Function
     Result := avcodec_open2(FVideoStrem.codec, avdec, @opts);
     if Result < 0 then begin
       if Assigned(FOnError) then FOnError(self,result,'Error Message:Failed to open ' + string(av_get_media_type_string(FVideoStrem.codec.codec_type)) + ' codec');
     end;
     (*ret := av_image_alloc(@Steams[i].data[0],@Steams[i].linesize[0],FVideoStrem.codec.width,FVideoStrem.codec.height,FVideoStrem.codec.pix_fmt,1);
     if ret < 0 then
       raise Exception.Create ('Error Message:Could not allocate raw video buffer');
     Steams[i].bufsize := ret;*)
    end;
  end;
  FVideoStrem:=PPtrIdx(FAVFormatContext.streams, FVideoIdx);
  {* инициализация глобальных переменных *}
  (*FEndposition.TimeInt:=FormatContext.duration;
  FEndposition.ts := Round(FEndposition.TimeInt * av_q2d(Steams[video_stream_idx].stream.time_base));
  FEndposition.Time := UnixToDateTime(FEndposition.TimeInt div AV_TIME_BASE);//FEndposition.ts);*)
 finally
  {* Освобождение и закрытие*}
 end;
end;

function TMediaCore.ReadPacked(Sender:TObject;var Pack: PAVPacket): Integer;
begin
 //if Assigned(CS) then CS.Enter;
 try
  Result := av_read_frame(FAVFormatContext, Pack);
  if Result < 0 then begin
   av_packet_unref(Pack);
   if Assigned(FOnError) then FOnError(Self,Result,'ReadPacked : '+av_err2str(Result));
  end else begin
    if (Sender is TVideoThread) then begin
      LastVideoThreadRead:=(Sender as TVideoThread).Tag;
    end;
  end;
 finally
  //if Assigned(CS) then CS.Leave;
 end;
end;

function TMediaCore.ReadPacked(var Pack: PAVPacket): Integer;
begin
 //if Assigned(CS) then CS.Enter;
 try
  if Pack = nil then Pack:=av_packet_alloc;
  Result := av_read_frame(FAVFormatContext, Pack);
  //OutputDebugString(PWideChar('ReadPacked:'+inttoStr(Result)));
  if Result < 0 then begin
   av_packet_unref(Pack);
   if Assigned(FOnError) then FOnError(Self,Result,'ReadPacked : '+av_err2str(Result));
  end else begin
   if Pack.pts <> AV_NOPTS_VALUE then begin
     if Pack.dts > 0 then BufferTime:=IncMilliSecond(MinDateTime,(Round((Pack.dts * av_q2d(FVideoStrem.time_base)) * 1000)))
     else BufferTime:= EncodeTime(0,0,0,0);
   end;
  end;
 finally
  //if Assigned(CS) then CS.Leave;
 end;
end;

function TMediaCore.ReadVideoPacked(Sender: TObject;
  var Pack: Pointer): Integer;
begin
 if Assigned(CS) then CS.Enter;
 try
  Result:=FVideoBuffer.ReadData(Pack);
  if Pack <> nil then begin
   if PAVPacket(Pack).pts <> AV_NOPTS_VALUE then begin
     if PAVPacket(Pack).dts > 0 then CurrentTime:=IncMilliSecond(MinDateTime,(Round((PAVPacket(Pack).dts * av_q2d(FVideoStrem.time_base)) * 1000)))
     else CurrentTime:= EncodeTime(0,0,0,0);
   end;
  end;
 finally
   if Assigned(CS) then CS.Leave;
 end;
end;

function TMediaCore.SeekTS(StreamID: Integer; TS: UInt64): Integer;
var
  iSeekFlag, ret: Integer;
  iSeekTarget: Int64;
  iSuccess: Integer;
begin
 iSeekTarget := ts*1000;
 try
  avcodec_flush_buffers(
    PAVstream(PPtrIdx(FAVFormatContext.streams, StreamID)).codec
  );
  iSeekTarget := av_rescale_q(iSeekTarget, AV_TIME_BASE_Q, PAVstream(PPtrIdx(FAVFormatContext.streams, StreamID)).time_base);
  iSeekFlag := 0;//AVSEEK_FLAG_BACKWARD;// or AVSEEK_FLAG_ANY or AVSEEK_FLAG_FRAME;
  if iSeekTarget > FAVFormatContext.duration then iSeekTarget := ts;
  iSuccess := avformat_seek_file(FAVFormatContext,
                                 PAVstream(PPtrIdx(FAVFormatContext.streams, StreamID)).index,//video_stream_idx,
                                 PAVstream(PPtrIdx(FAVFormatContext.streams, StreamID)).start_time,//FStartPosition.ts,//0,
                                 iSeekTarget,
                                 FAVFormatContext.duration,//FEndposition.ts,//Round(FormatContext.duration * av_q2d(Steams[SteamID].stream.time_base)),
                                 iSeekFlag);
  if iSuccess < 0 then begin
    if Assigned(FOnError) then FOnError(self,iSuccess,'Error [av_seek_frame] Message:'+av_err2str(iSuccess))
    else raise Exception.Create('Error [av_seek_frame] Message:'+av_err2str(iSuccess));
  end;
 except

 end;
end;

function TMediaCore.ReadPacked: Integer;
begin
  Result := av_read_frame(FAVFormatContext, FAVPacket);
  if Result < 0 then
  begin
   av_packet_unref(FAVPacket);
   if Assigned(FOnError) then FOnError(Self,Result,'ReadPacked : '+av_err2str(Result));
  end;
end;

{ TMediaDecoder }

constructor TMediaDecoder.Create;
begin
  inherited;
  FVideoBuffer:=TMediaBuffer.Create();
  Timer:=TTimer.Create(self);
  Timer.Enabled:=False;
  Timer.OnTimer:=OnTimer;
end;

destructor TMediaDecoder.Destroy;
begin
  FreeAndNil(Timer);
  inherited;
end;

function TMediaDecoder.GetBufferTime: string;
begin
  Result:='';
  //Result:=FormatDateTime('hh.mm.ss.zzz',IncMilliSecond(MinDateTime,GlobalTime));
  //Result:=Result+'-';
  Result:=Result+FormatDateTime('hh.mm.ss.zzz',BufferTime);
end;

function TMediaDecoder.GetCurrentTime: string;
begin
  Result:='';
  //Result:=FormatDateTime('hh.mm.ss.zzz',IncMilliSecond(MinDateTime,GlobalTime));
  //Result:=Result+'-';
  Result:=Result+FormatDateTime('hh.mm.ss.zzz',CurrentTime);
end;

procedure TMediaDecoder.OnIsPlayed(Sender:TObject;var Play: Boolean);
begin
 //CS.Enter;
 try
  (*if (Sender is TVideoThread) then begin
    Play := (LastVideoThreadRead <> TVideoThread(Sender).Tag) and Self.FPlay;
  end else begin *)
   Play:=Self.FPlay;
  //end;
 finally
   //CS.Leave;
 end;
end;

procedure TMediaDecoder.OnSyncTime(Sender:TObject;var GT: UInt64; Pack:PAVPacket; var Delay:Int64);
begin
 if Assigned(CS) then CS.Enter;
 try
  GT:=GlobalTime;
  (* Test Delay Frame to Frame *)
  //Delay:= pack.pts - Last_pts; (* Fail *)
  (* Test Delay Duration *)
  if Pack.stream_index = FVideoIdx then begin
   //FVideoStrem.r_frame_rate;
   Delay:= Ceil(
                  (pack.duration * av_q2d(FVideoStrem.time_base))*1000
                );
   //Delay:=int64(FVideoStrem.r_frame_rate.den);
  end else Delay:=0;
  if Delay < 0 then Delay:=0;
  (* Test Result ??? *)
  Last_pts:=pack.pts;
 finally
   if Assigned(CS) then CS.Leave;
 end;
end;

procedure TMediaDecoder.OnTimer(sender: TObject);
begin
 CS.Enter;
 try
  try
   GlobalTime:=MilliSecondsBetween(Time,StartT);
  except
    on E:Exception do begin
      if Assigned(FOnError) then begin
        FOnError(self,-1,'[Timer:TMediaDecoder] '+e.Message);
      end;
    end;
  end;
 finally
   CS.Leave;
 end;
end;

function TMediaDecoder.Start: Boolean;
var i:Integer;
begin Result:=False;
  FPlay:=False;
  if not Assigned(FDisplay) then Exit;
  if Assigned(CS) then CS.Enter;
  try
   //FMediaReader.Start;
   GlobalTime:=0;
   Timer.Interval:=5;
   LastVideoThreadRead:=1;
   {for I := 0 to High(FVideo) do begin
    if not Assigned(FVideo[i]) then FVideo[i]:=TVideoThread.Create(FVideoStrem);
    FVideo[i].OnDecodeFrame:=DecodeVideo;
    FVideo[i].OnRenderVideoFrame:=FDisplay.RenderVideoFrame;
    FVideo[i].OnReadVideoPacked:=ReadVideoPacked;
    FVideo[i].OnIsPlayed:=OnIsPlayed;
    FVideo[i].OnSyncTime:=OnSyncTime;
    FVideo[i].tag := i;
   end;}
   if NOT Assigned(FMediaReader) then begin
    FMediaReader:=TMediaReader.Create(Self);
    FMediaReader.OnDecodeFrame:=DecodeVideo;
    FMediaReader.OnRenderVideoFrame:=FDisplay.RenderVideoFrame;
    FMediaReader.OnReadVideoPacked:=ReadVideoPacked;
    FMediaReader.OnIsPlayed:=OnIsPlayed;
    FMediaReader.OnSyncTime:=OnSyncTime;
    FMediaReader.Start;
   end;
   StartTime:=Round(av_gettime / 1000);
   StartT:=Time;
   FPlay:=True;
   Timer.Enabled:=FPlay;
  finally
   if Assigned(CS) then CS.Leave;
  end;
end;

{ TMediaReader }

constructor TMediaReader.Create(MediaDecoder: TMediaDecoder);
begin
  inherited Create(true);
  FMediaDecoder:=MediaDecoder;
end;

procedure TMediaReader.Execute;
var Play:Boolean;
  pack:PAVPacket;
  V,i:Integer;
begin
  v:=0;
  for I := 0 to High(FVideo) do begin
   if not Assigned(FVideo[i]) then FVideo[i]:=TVideoThread.Create(FMediaDecoder.FVideoStrem);
   FVideo[i].SetSDL(FMediaDecoder.Display.GetSDLInfo);
   FVideo[i].OnDecodeFrame:=FOnDecodeFrame;
   FVideo[i].OnRenderVideoFrame:=FOnRenderVideoFrame;
   FVideo[i].OnReadVideoPacked:=FOnReadVideoPacked;
   FVideo[i].OnIsPlayed:=FOnIsPlayed;
   FVideo[i].OnSyncTime:=FSyncTime;
   FVideo[i].tag := i;
   repeat
     Sleep(1);
   until (Terminated) or (FVideo[i].FBuffer <> nil);
  end;
  NameThreadForDebugging('TMediaReader');
  try
   repeat
    //Sleep(1);
    //if Assigned(Application) then Application.ProcessMessages;
    try
    if (Assigned(FOnIsPlayed))and(Assigned(FMediaDecoder)) then begin
      if Assigned(FOnIsPlayed) then FOnIsPlayed(self,Play) else Play:=False;
      if Play then begin
        //if FMediaDecoder.FVideoBuffer.GetSize < High(FMediaDecoder.FVideoBuffer.FBuffer)-1 then begin
          if FMediaDecoder.ReadPacked(Pack) = 0 then begin
            if pack.stream_index = FMediaDecoder.FVideoIdx then begin
              repeat
               Sleep(1);
               //Application.ProcessMessages;
               if not Assigned(FVideo[v]) then Break;
               if (Terminated) or (not Play) then Break;
              until (FVideo[v].FBuffer.GetCount < FVideo[v].FBuffer.GetSize-1);
              if Assigned(FVideo[v]) then
              //if FVideo[v].FBuffer.GetSize < High(FVideo[v].FBuffer.FBuffer)-1 then begin
               FVideo[v].FBuffer.WriteData(Pointer(pack));
               //Break;
              //end;
              Inc(V);
              if v >= High(FVideo) then v:=0;
              //FMediaDecoder.FVideoBuffer.WriteData(Pointer(pack));
              pack:=nil;
            end else begin
              av_packet_unref(pack);
              av_packet_free(@pack);
              pack:=nil;
            end;
          end;
      end;
    end;
    except
     on E:Exception do begin
      OutputDebugString(PWideChar('ERROR [TMediaReader]:'+E.Message));
     end;
    end;
   until (Terminated);
  finally
    for I := 0 to High(FVideo) do begin
      FreeAndNil(FVideo[i]);
    end;
  end;
end;

initialization
  (*av_register_all();
  avcodec_register_all();*)

end.
