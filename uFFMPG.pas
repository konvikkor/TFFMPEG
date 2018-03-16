unit uFFMPG;

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
  libswresample, libswscale, sdl2, {sdl, {uResourcePaths,} System.Threading;
// , SDL2_Frame;

resourcestring
  SListIndexError = 'List index out of bounds (%d)';

type
  TPosition = packed record
    TimeInt: Int64;
    ts: Int64;
    Time: TTime;
  end;


  TOnFrameHook = procedure(frame: PAVFrame; Buffer: Pointer;
    bufferSize: Integer) of object;

  TMyFFMpegDisplay = class(TCustomPanel) // class helper for TPanel
  private
    FSDLPantalla: PSDLPantalla;
    FFlags: UInt32;
    FRenderInfo: PSDL_RendererInfo;
    MooseTexture: PSDL_Texture;
    FProportionally: Boolean;
    Timer: TTimer;
    FDeley: Byte;
    procedure OnTimer(sendder: TObject);
    procedure SetDeley(const Value: Byte);
    procedure Resize(Sender:TObject);
  protected
    video_clock:Double;
    frame_last_pts:int64;
    frame_last_delay:Double;
    CS:TCriticalSection;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Function DisplayFrame(var AVFrame: TAVFrame; AVStream: PAVStream; DeleyTime:Cardinal = 0): Boolean;
    Function DisplayInit(AVStream: PAVStream): Boolean;
    Function DisplayFree: Boolean;
    property DeleyTime:Byte read FDeley write SetDeley default 25;
  published
    property OnContextPopup;
    property PopupMenu;
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnKeyUp;
    property OnKeyPress;
    property OnKeyDown;
    property Align;
    Property Proportionally: Boolean read FProportionally Write FProportionally
      default True;
  end;

  function DurationToStr(ADuration: Int64): string;

Type
  TStream = Packed record
   stream: PAVStream;
   data: array [0 .. 3] of PByte;
   linesize: array [0 .. 3] of Integer;
   bufsize: Integer;
  end;

  TMyFFMpegCore = class (TComponent)
  private
    function GetStream(Index: Integer): TStream;
  protected
    FStartPosition: TPosition;
    FEndposition: TPosition;
    FCurrentPosition: TPosition;

    FFMpedDisplay: TMyFFMpegDisplay;
    FormatContext: PAVFormatContext;
    PixelFormat: TAVPixelFormat;
    Steams: Array of TStream;
    video_stream_idx: Integer;
    //[Volatile]
    //frame: PAVFrame;//TAVFrame;
    video_frame_count: Integer;
    audio_frame_count: Integer;

    //Tasks: Array of ITask;
    FStop:Boolean;
    CS:TCriticalSection;
  public
    [Volatile]
    pkt: TAVPacket;
    AVFrame:TAVFrame;
    function OpenFile(FileName:string):Boolean;
    Function CloseFile:Boolean;
    constructor Create(AOwner: TComponent); override;
    Function ReadFrame:Cardinal;
    Function ReadFrameTry:Cardinal;
    Function DecodePaket(SteamID:Integer;VAR got_Frame:PInteger):Cardinal;
    Procedure GoToFrameToTS(SteamID:Integer;TS:Int64);
    property AVFormatContext:PAVFormatContext read FormatContext;
    property AVStream[Index: Integer]: TStream read GetStream; default;
    destructor Destroy; override;
  published
    property Display:TMyFFMpegDisplay read FFMpedDisplay Write FFMpedDisplay;
    property StartPosition: TPosition read FStartPosition;
    Property EndPosition: TPosition read FEndposition;
    Property CurrentPosition: TPosition read FCurrentPosition;
    Property VideoStreamIndex:integer Read video_stream_idx default -1;
  end;

  TMyFFMpeg = class(TMyFFMpegCore)
  private
   FStop:Boolean;
   FSeekTarget: Integer;
  protected
   Task:ITask;
  public
   Procedure Play;
   Procedure Stop;
   destructor Destroy; override;
  published
   Property Seek:Integer read FSeekTarget Write FSeekTarget default -1;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('My_FFMPEG',[TMyFFMpegDisplay]);
  RegisterComponents('My_FFMPEG',[TMyFFMpeg]);
end;

function DurationToStr(ADuration: Int64): string;
var
  hours, mins, secs, us: Integer;
begin
  if ADuration <> AV_NOPTS_VALUE then
  begin
    secs := (ADuration + 5000) div AV_TIME_BASE;
    us := (ADuration + 5000) mod AV_TIME_BASE;
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

{ TFFMpedDisplay }

constructor TMyFFMpegDisplay.Create(AOwner: TComponent);
begin
  inherited;
  CS:=TCriticalSection.Create;
  FDeley:=20;
  frame_last_pts:=0;
  FProportionally:=True;
end;

procedure TMyFFMpegDisplay.CreateWnd;
begin
  inherited;
  if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
    SDL_InitSubSystem(SDL_INIT_VIDEO);
  New(FSDLPantalla);
  FSDLPantalla.Window := SDL_CreateWindowFrom(Pointer(Self.Handle));
  if FSDLPantalla.Window <> nil then
  begin
    FSDLPantalla.Renderer := SDL_CreateRenderer(FSDLPantalla^.Window, -1, 0);
    // no forzamos ningъn tipo de render (0) para que el sistema coja el que pueda Hard-Soft
    if FSDLPantalla.Renderer <> nil then
    begin
      New(FRenderInfo);
      if SDL_GetRendererInfo(FSDLPantalla^.Renderer, FRenderInfo) = 0 then
      begin
        FSDLPantalla.max_texture_width := FRenderInfo^.max_texture_width;
        FSDLPantalla.max_texture_height := FRenderInfo^.max_texture_height;
        FSDLPantalla.hardware :=
          ((FRenderInfo.Flags and SDL_RENDERER_ACCELERATED) > 0);
        FSDLPantalla.render_name := FRenderInfo^.name;
        // PAnsiChar(FRenderInfo.name);
        SDL_ShowWindow(FSDLPantalla.Window);
        if SDL_SetRenderDrawColor(FSDLPantalla^.Renderer, 0, 0, 0,
          SDL_ALPHA_OPAQUE) = 0 then
        begin
          if SDL_RenderFillRect(FSDLPantalla^.Renderer, nil) = 0 then
            FFlags := SDL_GetWindowFlags(FSDLPantalla.Window)
          else
            ShowMessage('Error clearing render context');
        end
        else
          ShowMessage('Error setting render draw color');
      end
      else
        ShowMessage('Error getting information about rendering context');

    end
    else
      ShowMessage('Error crearting SDL2 Render');

  end
  else
    ShowMessage('Error creating SDL2 Window.');
  Self.OnResize:=Resize;
  Timer := TTimer.Create(Self);
  Timer.OnTimer := OnTimer;
  Timer.Interval := 20; // обновление полотна
  Timer.Enabled := True;
end;

destructor TMyFFMpegDisplay.Destroy;
begin
  Timer.Enabled := False;
  inherited;
end;

procedure TMyFFMpegDisplay.DestroyWnd;
begin
  FreeAndNil(Timer);
  if FSDLPantalla.Renderer <> nil then
  begin
    SDL_DestroyRenderer(FSDLPantalla.Renderer);
    FSDLPantalla.Renderer := nil;
  end;
  if FSDLPantalla.Window <> nil then
  begin
    SDL_DestroyWindow(FSDLPantalla.Window);
    FSDLPantalla.Window := nil;
  end;
  Dispose(FSDLPantalla);
  inherited;
end;

function TMyFFMpegDisplay.DisplayFrame(VAR AVFrame: TAVFrame;
  AVStream: PAVStream; DeleyTime:Cardinal): Boolean;
var
  res: SInt32;
  ImgConvContext: PSwsContext;
  Img: PAVFrame;
  Ibmp_Size: Integer;
  ibmp_Buff: PByte;
  pix_F: TAVPixelFormat;
  rect2, rect3: TSDL_Rect;
  p: Real;

  TicB,TicE:Cardinal;

  rect: TSDL_Rect;
begin
 try
  pix_F := AV_PIX_FMT_BGR0;
  Result := True;
  // проверяем была ли инициализация
  if (not assigned(FSDLPantalla.Window.surface)) OR (not assigned(MooseTexture))
  then
  begin
    Result := False;
    Exit;
  end;
  // Очистка от предыдущий ошибок
  Img := av_frame_alloc(); // Создаём пространство для конвертированного кадра
  ticB:=GetTickCount;
  // получаем контекст для преобразования в RGBы
  ImgConvContext := nil;
  ImgConvContext := sws_getContext(AVStream.codec.width, AVStream.codec.height,
    AVStream.codec.pix_fmt, AVStream.codec.width, AVStream.codec.height, pix_F, SWS_BILINEAR, nil,
    nil, nil);
  // Формируем буфер для картинки RGB
  Ibmp_Size := avpicture_get_size(pix_F, AVStream.codec.width, AVStream.codec.height);
  ibmp_Buff := nil;
  ibmp_Buff := av_malloc(Ibmp_Size * SizeOf(Byte));
  res := avpicture_fill(PAVPicture(Img), ibmp_Buff, pix_F, AVStream.codec.width,
    AVFrame.height);
  // Задаём размеры прямоугольника в дальнейшем по мену будем считать преобразование размера кадра
  rect.x := 0;
  rect.y := 0;
  rect.w := AVStream.codec.width; // шырена
  rect.h := AVStream.codec.height; // высота
  // конвертируем формат пикселя в RGB
  if Assigned(ImgConvContext) then
   res := sws_scale(ImgConvContext, @AVFrame.Data, @AVFrame.linesize, 0,
      AVFrame.height, @Img.Data, @Img.linesize);
  TicE:=GetTickCount;
  try
    // рисуем картинку на текстуре
    res := SDL_UpdateTexture(MooseTexture, @rect, @Img.Data[0]^,
      Img.linesize[0]);
    if res < 0 then
      Result := False;
    // копируем картинку с текстуры в рендер
    if FProportionally then begin
      SDL_RenderGetViewport(FSDLPantalla.Renderer,@rect3);
      rect2.x := 0;
      rect2.y := 0;
      p := (rect3.w * 100) / rect.w;
      rect2.w := round((rect.w * p) / 100);
      rect2.h := round((rect.h * p) / 100);
      if rect2.h > Self.height then
      begin
       p := (rect3.h * 100) / rect2.h;
       rect2.w := round((rect2.w * p) / 100);
       rect2.h := round((rect2.h * p) / 100);
      end;
      res := SDL_RenderCopy(FSDLPantalla^.Renderer, MooseTexture, nil,
        // С какой области скопировать кадр
        @rect2 // На какой размер растянуть кадр
        );
    end else begin
      res := SDL_RenderCopy(FSDLPantalla^.Renderer, MooseTexture, nil,
        // С какой области скопировать кадр
        nil // На какой размер растянуть кадр
        );
    end;
    if res < 0 then
      Result := False;
    // Отображаем картинку в рендере
    if AVStream.avg_frame_rate.den > 0 then
     res:=Trunc(av_q2d(AVStream.avg_frame_rate))-((TicE-TicB)+DeleyTime);
    if res < 0 then res:=0;
    sleep(res);
    frame_last_pts:=AVFrame.pts;
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;
 finally
  // очистка памяти от временного мусора
  av_free(ibmp_Buff);
  sws_freeContext(ImgConvContext);
  av_frame_free(@Img);
  Application.ProcessMessages;
 end;
end;

function TMyFFMpegDisplay.DisplayFree: Boolean;
begin Result:=True;
 try
  if assigned(MooseTexture) then begin
    SDL_DestroyTexture(MooseTexture);
    MooseTexture:=nil;
  end;
  if assigned(FSDLPantalla.Window.surface) then begin
    SDL_FreeSurface(FSDLPantalla.Window.surface);
    FSDLPantalla.Window.surface:=nil;
  end;
 except
  Result:=False;
 end;
end;

function TMyFFMpegDisplay.DisplayInit(AVStream: PAVStream): Boolean;
begin  Result:=True;
 try
  if assigned(FSDLPantalla.Window.surface) then begin
   if
    (FSDLPantalla.Window.surface.w <> AVStream.codec.width) or
    (FSDLPantalla.Window.surface.h <> AVStream.codec.height) then
     DisplayFree;
  end;
  // Создаём текстуру для отображения кадра
  if not assigned(FSDLPantalla.Window.surface) then
    FSDLPantalla.Window.surface := SDL_CreateRGBSurface(0, AVStream.codec.width,
      AVStream.codec.height, 24, $000000FF, $0000FF00, $00FF0000, $00000000);
  if not assigned(MooseTexture) then
    MooseTexture := SDL_CreateTextureFromSurface(FSDLPantalla.Renderer,
      FSDLPantalla.Window.surface);
 except
   Result:=False;
 end;
end;

procedure TMyFFMpegDisplay.OnTimer(sendder: TObject);
begin
  SDL_RenderPresent(FSDLPantalla^.Renderer);
end;

procedure TMyFFMpegDisplay.Resize(Sender: TObject);
begin
end;

procedure TMyFFMpegDisplay.SetDeley(const Value: Byte);
begin
  if Value < 20 then begin
   FDeley := 20;
   Exit;
  end;
  if Value > 30 then begin
   FDeley := 30;
   Exit;
  end;
  FDeley := Value;
end;

{ TMyFFMpegCore }

function TMyFFMpegCore.CloseFile: Boolean;
begin
 avformat_close_input(@FormatContext);
end;

constructor TMyFFMpegCore.Create(AOwner: TComponent);
begin
  inherited;
  av_register_all();
  avcodec_register_all();
  av_init_packet(@pkt);
  //av_frame_unref(frame);
  pkt.Data := nil;
  pkt.size := 0;
end;

function TMyFFMpegCore.DecodePaket(SteamID: Integer;
  var got_Frame: PInteger): Cardinal;
var TicB,TicE:Cardinal;
    ret:Integer;
    pts: Int64;
begin
  //if not Assigned(frame) then Frame := av_frame_alloc();
  (* decode video frame *)
  TicB:=GetTickCount;
  ret := avcodec_decode_video2(Steams[SteamID].stream^.codec, @AVFrame{@frame}, got_frame, @pkt);
  if ret < 0 then
  begin
    Result := ret;
    raise Exception.Create('Error decoding video frame (' + string(av_err2str(ret)) + ')');
  end;
  TicE:=GetTickCount;
  if got_frame^ <> 0 then begin
   if ((pkt.dts = AV_NOPTS_VALUE) and assigned(AVFrame.opaque) and (Int64(AVFrame.opaque^) <> AV_NOPTS_VALUE)) then
    begin
     pts := Int64(AVFrame.opaque^);
    end
   else if (pkt.dts <> AV_NOPTS_VALUE) then
    begin
     pts := pkt.dts
    end else pts := 0;
   FCurrentPosition.TimeInt := round(pts * av_q2d(Steams[SteamID].stream^.codec.time_base));
   FCurrentPosition.ts := pts;
   FCurrentPosition.Time := UnixToDateTime(FCurrentPosition.TimeInt);
  end;
  Result:=TicE-TicB;
end;

destructor TMyFFMpegCore.Destroy;
begin
  //if Assigned(frame) then av_free(frame);
  inherited;
end;

function TMyFFMpegCore.GetStream(Index: Integer): TStream;
begin
  if Cardinal(Index) >= Cardinal(High(Steams)) then
    raise EListError.CreateFmt(LoadResString(@SListIndexError), [Index]) at ReturnAddress;
  Result := Steams[Index];
end;

procedure TMyFFMpegCore.GoToFrameToTS(SteamID:Integer;TS:Int64);
var
  iSeekFlag, ret: Integer;
  iSeekTarget: Int64;
  iSuccess: Integer;
begin
 iSeekTarget := ts;
 try
  iSeekTarget := av_rescale_q(iSeekTarget*1000, AV_TIME_BASE_Q, Steams[SteamID].stream.time_base);
  iSeekFlag := {AVSEEK_FLAG_ANY or }AVSEEK_FLAG_FRAME;
  iSuccess := avformat_seek_file(FormatContext,
                                 video_stream_idx,
                                 0,
                                 iSeekTarget,
                                 Round(FormatContext.duration * av_q2d(Steams[SteamID].stream.time_base)),
                                 iSeekFlag);
  if iSuccess < 0 then raise Exception.Create('Error [av_seek_frame] Message:'+av_err2str(iSuccess));
 except

 end;
end;

function TMyFFMpegCore.OpenFile(FileName: String): Boolean;
var ret,i:Integer;
  avdec: PAVCodec;
  opts: PAVDictionary;
  tmp:AnsiString;
  VideoFile:PAnsiChar;
begin Result:=False; opts:=nil;
 tmp:=AnsiString(FileName);
 VideoFile:=PAnsiChar(tmp);
 try
  { * open input file, and allocate format context * }
  ret := avformat_open_input(@FormatContext, VideoFile, nil, nil);
  if ret < 0 then
    raise Exception.Create('Error Message:Could not open source file "' + FileName+'"'+sLineBreak+av_err2str(ret));
  (* retrieve stream information *)
  ret := avformat_find_stream_info(FormatContext, nil);
  if ret < 0 then
    raise Exception.Create('Error Message:Could not find stream information' + sLineBreak + av_err2str(ret));
  // Dump information about file onto standard error
  av_dump_format(FormatContext, 0, VideoFile, 0);
  {* Поиск основного потока видео *}
  ret := av_find_best_stream(FormatContext, AVMEDIA_TYPE_VIDEO, -1, -1, nil, 0);
  if ret < 0 then
    raise Exception.Create('Error Message:Could not find ' +
      string(av_get_media_type_string(AVMEDIA_TYPE_VIDEO)) + ' stream in input file ''' +
      String(FileName) + '''')
  else video_stream_idx:=ret;
  {* Открываем потоки файла в массив для быстрого доступа *}
  SetLength(Steams,FormatContext.nb_streams);
  for I := 0 to FormatContext.nb_streams-1 do begin
    if PAVstream(PPtrIdx(FormatContext.streams, I))^.codec.codec_type = AVMEDIA_TYPE_VIDEO then begin
     Steams[i].stream := PPtrIdx(FormatContext.streams, I);
     avdec := avcodec_find_decoder(Steams[i].stream.codec.codec_id);
     if not assigned(avdec) then
       raise Exception.Create('Error Message:Failed to find ' + string(av_get_media_type_string(Steams[i].stream.codec.codec_type)) + ' codec');
     (* Init the decoders, with or without reference counting *)
     //av_dict_set(@opts, 'refcounted_frames', '1', 0);  /// Test Function
     ret := avcodec_open2(Steams[i].stream.codec, avdec, @opts);
     if ret < 0 then
       raise Exception.Create('Error Message:Failed to open ' + string(av_get_media_type_string(Steams[i].stream.codec.codec_type)) + ' codec');
     ret := av_image_alloc(@Steams[i].data[0],@Steams[i].linesize[0],Steams[i].stream.codec.width,Steams[i].stream.codec.height,Steams[i].stream.codec.pix_fmt,1);
     if ret < 0 then
       raise Exception.Create ('Error Message:Could not allocate raw video buffer');
     Steams[i].bufsize := ret;
    end;
  end;
  {* инициализация глобальных переменных *}
  FStartPosition.TimeInt :=
    round(Steams[video_stream_idx].stream.start_time * av_q2d(Steams[video_stream_idx].stream.time_base));
  FStartPosition.ts := Steams[video_stream_idx].stream.start_time;
  FStartPosition.Time := UnixToDateTime(FStartPosition.TimeInt);

  FCurrentPosition := FStartPosition;
  if Steams[video_stream_idx].stream.nb_frames > 0 then begin
   FEndposition.TimeInt :=
    round(((Steams[video_stream_idx].stream.nb_frames * Steams[video_stream_idx].stream.r_frame_rate.den *
    Steams[video_stream_idx].stream.time_base.den) / (Int64(Steams[video_stream_idx].stream.r_frame_rate.num) *
    Steams[video_stream_idx].stream.time_base.num)) * av_q2d(Steams[video_stream_idx].stream.time_base));
   FEndposition.ts :=
    (((Steams[video_stream_idx].stream.nb_frames * Steams[video_stream_idx].stream.r_frame_rate.den *
    Steams[video_stream_idx].stream.time_base.den) div (Int64(Steams[video_stream_idx].stream.r_frame_rate.num) *
    Steams[video_stream_idx].stream.time_base.num)));
   FEndposition.Time := UnixToDateTime(FEndposition.TimeInt);
  end else begin
   FEndposition.TimeInt:=FormatContext.duration;
   FEndposition.ts := Round(FEndposition.TimeInt * av_q2d(Steams[video_stream_idx].stream.time_base));
   FEndposition.Time := UnixToDateTime(FEndposition.TimeInt div AV_TIME_BASE);//FEndposition.ts);
  end;

  Result:=True;
 finally
  {* Освобождение и закрытие*}
 end;
end;

function TMyFFMpegCore.ReadFrame: Cardinal;
var TicB1,TicE1:Cardinal;
  ret:Integer;
begin
  TicB1:=GetTickCount;
  ret := av_read_frame(FormatContext, @pkt);
  TicE1:=GetTickCount;
  if ret < 0 then
  begin
   av_packet_unref(@pkt);
   raise Exception.Create('Error Message:Can not read Frame ' + sLineBreak + av_err2str(ret));
  end;
  Result:=TicE1-TicB1;
end;

function TMyFFMpegCore.ReadFrameTry: Cardinal;
var TicB1,TicE1:Cardinal;
  ret:Integer;
begin
  TicB1:=GetTickCount;
  ret := av_read_frame(FormatContext, @pkt);
  TicE1:=GetTickCount;
  if ret < 0 then
  begin
   av_packet_unref(@pkt);
   //raise Exception.Create('Error Message:Can not read Frame ' + sLineBreak + av_err2str(ret));
  end;
  Result:=TicE1-TicB1;
end;

{ TMyFFMpeg }

destructor TMyFFMpeg.Destroy;
begin
  FStop:=True;
  Application.ProcessMessages;
  inherited;
end;

procedure TMyFFMpeg.Play;
var got_frame:PInteger;
  Delay:Cardinal;
  Size:Int64;
begin
  if not Assigned(FormatContext) then Exit;
  Task:=TTask.Run(procedure
  begin
  FStop:=False;
  new(got_frame);
  try
   got_frame^:=0;
   while not FStop do begin
    if Application.Terminated then Break;
    Application.ProcessMessages;
    if FSeekTarget > 0 then begin
      GoToFrameToTS(video_stream_idx,FSeekTarget);
      FSeekTarget:=-1;
    end;
    Delay:=ReadFrameTry;
    Size:=pkt.size;
    if Size > 0 then
    if pkt.stream_index = video_stream_idx then begin
      Delay:=Delay + DecodePaket(pkt.stream_index,got_frame);
      if Assigned(FFMpedDisplay) then begin
        FFMpedDisplay.DisplayInit(Steams[pkt.stream_index].stream);
        FFMpedDisplay.DisplayFrame(AVFrame,Steams[pkt.stream_index].stream,Delay);
      end;
      if (got_frame^ <> 0) then begin
       av_frame_unref(@AVFrame);//av_frame_unref(@frame);
      end;
    end;
    if Size <= 0 then Break;
   end;
  finally
   Dispose(got_frame);
  end;
  end);
  //Task.start;
end;

procedure TMyFFMpeg.Stop;
begin
  FStop:=True;
end;

end.
