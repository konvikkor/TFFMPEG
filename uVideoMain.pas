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
  libswresample, libswscale, sdl2, {SDL2_ttf,{sdl, {uResourcePaths,} System.Threading;

const
  MAX_AUDIO_FRAME_SIZE = 192000; // 1 second of 48khz 32bit audio
  MODULE_VERSION = '2.0';
  //WM_DecodeFrame = WM_USER+0;
  //WM_PlayFrame = WM_USER+1;
  //WM_StopFrame = WM_USER+2;
  //WM_FreeFrame = WM_USER+3;
  //WM_PlayFrame = WM_USER+4;
  //WM_PlayFrame = WM_USER+5;
  //WM_PlayFrame = WM_USER+6;

type
  TOnError = procedure (Sender:TObject; ErrorCode:Integer; MSG:string) of object;

  TMediaBufferInfo = packed Record
    AVStream  : PAVStream;
    AVPacket  : PAVPacket;//TAVPacket;
    AVFrame   : PAVFrame;//TAVFrame;
    GotFrame  : PInteger;
  End;
  PMediaBufferInfo = ^TMediaBufferInfo;

  TMediaDisplay = class (TCustomPanel)
  private
    FSDLPantalla: PSDLPantalla;
    FFlags: UInt32;
    FRenderInfo: PSDL_RendererInfo;
    MooseTexture: PSDL_Texture;
    FProportionally: Boolean;
    Timer: TTimer;
    CS:TCriticalSection;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure OnTimer(Sender:TObject);
    procedure CanResize (Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    Function Ini3DCanvas(Rect:TSDL_Rect):Boolean;
    Function Free3DCanvas:Boolean;
    procedure DrawStatusInDisplay;
  protected
    procedure Paint; override;
    Function CaslcSize(Sourse:TSDL_Rect):TSDL_Rect;
    procedure OnRenderVideo(var Data:PMediaBufferInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateRender;
    procedure RenderVideoFrame(w,h:SInt32;Data:Array of PByte;linesize: Array of Integer; pix_fmt:TAVPixelFormat);
  published
    property Anchors;
    property OnContextPopup;
    property PopupMenu;
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnKeyUp;
    property OnKeyPress;
    property OnKeyDown;
    property Align;
  end;

  TMediaCore = class(TComponent)
  private
    FDisplay: TMediaDisplay;
    FOnError: TOnError;
  protected
    CS:TCriticalSection;
    (*Global File*)
    FAVFormatContext:PAVFormatContext;
    FAVPacket:PAVPacket;
    (*Video*)
    FVideoIdx:Integer;
    FVideoStrem:PAVStream;
    FVideoFrame:PAVFrame;
    FVideoGotFrame:PInteger;
  public
    Constructor Create;
    Destructor Destroy; override;
    Function ReadPacked:Integer;overload;
    Function ReadPacked(var Pack:PAVPacket):Integer;overload;
    Function DecodeVideo:Integer;overload;
    Function DecodeVideo(var Pack:PAVPacket; var Frame:PAVFrame; Var Gotframe:PInteger):Integer;overload;
    Function SeekTS(StreamID:Integer;TS:UInt64):Integer;
    Function OpenFile(FileName:TFileName):Integer;
    Function CloseFile:Boolean;
  published
    Property Display:TMediaDisplay Read FDisplay Write FDisplay;
    Property OnError:TOnError read FOnError Write FOnError;
  end;

  TOnSyncTime = procedure (var GlobalTime:UInt64) of object;
  TOnDecodeVideo = function (var Pack:PAVPacket; var Frame:PAVFrame; Var Gotframe:PInteger):Integer of object;
  TOnReadPacked = Function (var Pack:PAVPacket):Integer of object;
  TOnRenderVideoFrame = procedure (w,h:SInt32;Data:Array of PByte;linesize: Array of Integer; pix_fmt:TAVPixelFormat) of object;
  TOnIsPlayed = Procedure (var Play:Boolean)of object;

  TVideoThread = class(TThread)
  private
    FSyncTime: TOnSyncTime;
    FOnDecodeFrame: TOnDecodeVideo;
    FOnReadPacked: TOnReadPacked;
    FOnRenderVideoFrame: TOnRenderVideoFrame;
    FOnIsPlayed: TOnIsPlayed;
  protected
    FVideoStrem:PAVStream;
    FAVPacked:PAVPacket;
    FAVFrame:PAVFrame;
    FGotFrame:PInteger;
    procedure Execute; override;
  public
    constructor Create(FVideoStrem:PAVStream);
    Property OnSyncTime:TOnSyncTime read FSyncTime Write FSyncTime;
    Property OnDecodeFrame:TOnDecodeVideo read FOnDecodeFrame Write FOnDecodeFrame;
    property OnReadPacked:TOnReadPacked read FOnReadPacked Write FOnReadPacked;
    property OnRenderVideoFrame:TOnRenderVideoFrame read FOnRenderVideoFrame Write FOnRenderVideoFrame;
    property OnIsPlayed:TOnIsPlayed read FOnIsPlayed write FOnIsPlayed;
  end;

  TMediaDecoder = class(TMediaCore)
  private
    FPlay: Boolean;
    GlobalTime:UInt64;
    StartTime:UInt64;
    Timer:TTimer;
  protected
    FVideo:TVideoThread;
    Procedure OnTimer(sender:TObject);
    procedure OnIsPlayed(var Play:Boolean);
    procedure OnSyncTime(var GT:UInt64);
  public
    constructor Create;
    destructor Destroy;
    function Start:Boolean;
    property Play:Boolean Read FPlay default false;
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

{ TMediaDisplay }

procedure TMediaDisplay.CanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
var Rect:TSDL_Rect;
begin
  CS.Enter;
  try
    Rect.x:=0; Rect.y:=0; Rect.w:=NewWidth; Rect.h:=NewHeight;
    Resize:=Ini3DCanvas(Rect);
  finally
    CS.Leave;
  end;
end;

function TMediaDisplay.CaslcSize(Sourse: TSDL_Rect): TSDL_Rect;
var rect:TSDL_Rect;
    p:Double;
begin
  if not Assigned(FSDLPantalla.Renderer) then Exit;
  SDL_RenderGetViewport(FSDLPantalla.Renderer,@rect);
  Result.x := 0;
  Result.y := 0;
  p := (rect.w * 100) / Sourse.w;
  Result.w := round((Sourse.w * p) / 100);
  Result.h := round((Sourse.h * p) / 100);
  if Result.h > rect.h then
  begin
   p := (rect.h * 100) / Result.h;
   Result.w := round((Result.w * p) / 100);
   Result.h := round((Result.h * p) / 100);
  end;
end;

constructor TMediaDisplay.Create(AOwner: TComponent);
begin
  inherited;
  CS:=TCriticalSection.Create;
  Self.Height:=200;
  Self.Width:=200;
  Self.Align:=alNone;
  Self.OnCanResize:=CanResize;
  Timer:=TTimer.Create(nil);
  Timer.Interval:=20;
  Timer.OnTimer:=OnTimer;
  Timer.Enabled:=True;
  FProportionally:=True;
end;

procedure TMediaDisplay.CreateWnd;
begin
  inherited;
  if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
    SDL_InitSubSystem(SDL_INIT_VIDEO);
  (*if TTF_Init = 0 then begin
   if not TTF_WasInit() then begin
     raise Exception.Create('Error Message [TTF_WasInit]:'+TTF_GetError());
   end;
  end else begin
    raise Exception.Create('Error Message [TTF_Init]:'+TTF_GetError());
  end;*)
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
            raise Exception.Create('Error clearing render context');
        end
        else
          raise Exception.Create('Error setting render draw color');
      end
      else
        raise Exception.Create('Error getting information about rendering context');

    end
    else
      raise Exception.Create('Error crearting SDL2 Render');

  end
  else
    raise Exception.Create('Error creating SDL2 Window.');
  //Self.OnResize:=Resize;
  //Timer := TTimer.Create(Self);
  //Timer.OnTimer := OnTimer;
  //Timer.Interval := 20; // обновление полотна
  //Timer.Enabled := True;
end;

destructor TMediaDisplay.Destroy;
begin
  FreeAndNil(CS);
  inherited;
end;

procedure TMediaDisplay.DestroyWnd;
begin
  //FreeAndNil(Timer);
  Timer.Enabled := False;
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
  //TTF_Quit;
  Dispose(FSDLPantalla);
  inherited;
end;

procedure TMediaDisplay.DrawStatusInDisplay;
const fontFile = 'F:\VIDEO_COMPONENT\Win32\Debug\arial.ttf'+#0+#0;
var surface:PSDL_Surface;
    //font:PTTF_Font;
    Color:TSDL_Color;
    texture:PSDL_Texture;
begin
 (* Default render screen *) 
 if FSDLPantalla = nil then Exit; 
 if FSDLPantalla.Renderer = nil then exit;
 CS.Enter;
 try
  {font:=SDL2_ttf.TTF_OpenFont(fontFile,25);
  if font = nil then begin
    raise Exception.Create('Error Message:'+(TTF_GetError()));
    Exit;
  end;
  Color.r:=255;
  Color.g:=0;
  Color.b:=0;
  Color.unused:=150;
  surface:=TTF_RenderText_Blended(font,PWideChar('TEST'),color);
  texture:=SDL_CreateTextureFromSurface(FSDLPantalla.Renderer,surface);
  SDL_RenderCopy(FSDLPantalla.Renderer,texture,nil,nil);
  UpdateRender;
  SDL_DestroyTexture(texture);}
 finally
   CS.Leave;
 end;
end;

function TMediaDisplay.Free3DCanvas: Boolean;
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

function TMediaDisplay.Ini3DCanvas(Rect: TSDL_Rect): Boolean;
var
 rect2:TSDL_Rect;
begin  Result:=True;
 try
  //rect2:=CaslcSize(rect);
  rect2:=rect;
  if assigned(FSDLPantalla.Window.surface) then begin
   if
    (FSDLPantalla.Window.surface.w <> rect2.w{AVStream.codec.width}) or
    (FSDLPantalla.Window.surface.h <> rect2.h{AVStream.codec.height}) then
     Free3DCanvas;
  end;
  // Создаём текстуру для отображения кадра
  if not assigned(FSDLPantalla.Window.surface) then
    FSDLPantalla.Window.surface := SDL_CreateRGBSurface(0, rect2.w{AVStream.codec.width},
      rect2.h{AVStream.codec.height}, 24, $000000FF, $0000FF00, $00FF0000, $00000000);
  if not assigned(MooseTexture) then
    MooseTexture := SDL_CreateTextureFromSurface(FSDLPantalla.Renderer,
      FSDLPantalla.Window.surface);
 except
   Result:=False;
 end;
end;

procedure TMediaDisplay.OnRenderVideo(var Data: PMediaBufferInfo);
begin
 if Data^.AVFrame = nil then Exit;
 RenderVideoFrame(
    Data^.AVFrame.width,
    Data^.AVFrame.height,
    Data^.AVFrame.data,
    Data^.AVFrame.linesize,
    Data^.AVStream.codec.pix_fmt
 );
end;

procedure TMediaDisplay.OnTimer(Sender: TObject);
var Rect:TSDL_Rect;  
  R:TRect;
begin
 if FSDLPantalla <> nil then begin 
   if FSDLPantalla.Window.surface = nil then begin   
     r:=GetClientRect;
     Ini3DCanvas(TSDL_Rect(R));
   end;
 end;
 (* Default render screen *)
 DrawStatusInDisplay;
 UpdateRender;
end;

procedure TMediaDisplay.Paint;
var Rect: TRect;
begin
  //inherited; //?
  (* Message Pain frame *)
  Rect := GetClientRect;
  with Canvas do begin
    Brush.Color:=clBlack;
    FillRect(Rect);
  end;
end;

procedure TMediaDisplay.RenderVideoFrame(w, h: SInt32; Data: array of PByte;
  linesize: array of Integer; pix_fmt:TAVPixelFormat);
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

  //MyRect:TRect;

  rect: TSDL_Rect;
begin
  // проверяем была ли инициализация
 if (not assigned(FSDLPantalla.Window.surface)) OR (not assigned(MooseTexture)) then begin
   Exit;
 end;
 CS.Enter;
 try
  pix_F := AV_PIX_FMT_BGR0;
  // Задаём размеры прямоугольника в дальнейшем по мену будем считать преобразование размера кадра
  rect.x := 0; rect.y := 0;
  //MyRect:=GetClientRect;
  rect.w := w; // шырена
  rect.h := h; // высота
  rect2:=CaslcSize(rect);
  // Очистка от предыдущий ошибок
  Img := av_frame_alloc(); // Создаём пространство для конвертированного кадра
  // получаем контекст для преобразования в RGBы
  ImgConvContext := nil;
  ImgConvContext := sws_getContext(w, h, pix_fmt, rect2.W, rect2.h, pix_F, SWS_BILINEAR, nil, nil, nil);
  // Формируем буфер для картинки RGB
  Ibmp_Size := avpicture_get_size(pix_F, rect2.w, rect2.H);
  ibmp_Buff := nil;
  ibmp_Buff := av_malloc(Ibmp_Size * SizeOf(Byte));
  res := avpicture_fill(PAVPicture(Img), ibmp_Buff, pix_F, rect2.w,rect2.h);
  // конвертируем формат пикселя в RGB
  if Assigned(ImgConvContext) then res := sws_scale(ImgConvContext, @Data, @linesize, 0, h, @Img.Data, @Img.linesize);
  try
    // рисуем картинку на текстуре
    res := SDL_UpdateTexture(MooseTexture, @rect2, @Img.Data[0]^, Img.linesize[0]);
    SDL_RenderClear(FSDLPantalla^.Renderer);
    // копируем картинку с текстуры в рендер
    if FProportionally then begin
      res := SDL_RenderCopy(FSDLPantalla^.Renderer, MooseTexture,
        nil, // С какой области скопировать кадр
        @rect2 // На какой размер растянуть кадр
        );
    end else begin
      res := SDL_RenderCopy(FSDLPantalla^.Renderer, MooseTexture,
        @rect,// С какой области скопировать кадр
        nil   // На какой размер растянуть кадр
        );
    end;
    // Отображаем картинку в рендере
    //if AVStream.avg_frame_rate.den > 0 then res:=Trunc(av_q2d(AVStream.avg_frame_rate))-((TicE-TicB)+DeleyTime);
  except
    on E: Exception do begin
      //Result := False;
    end;
  end;
 finally
  // очистка памяти от временного мусора
  av_free(ibmp_Buff);
  //av_freep(ibmp_Buff);
  sws_freeContext(ImgConvContext);
  //av_frame_unref(@Img);
  av_frame_free(@Img);
  Application.ProcessMessages;
  SDL_RenderPresent(FSDLPantalla^.Renderer);
  CS.Leave;
 end;
end;

procedure TMediaDisplay.UpdateRender;
begin
 if Assigned(FSDLPantalla) then begin
  if Assigned(FSDLPantalla^.Renderer) then begin
   CS.Enter;
   try
    SDL_RenderPresent(FSDLPantalla^.Renderer);
   finally
     CS.Leave;
   end;
  end;
 end;
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

function TMediaCore.DecodeVideo(var Pack: PAVPacket; var Frame: PAVFrame;
  var Gotframe: PInteger): Integer;
begin
 CS.Enter;
 try
  Result := avcodec_decode_video2(FVideoStrem^.codec, Frame, Gotframe, Pack);
  if (Result < 0) then
  begin
    if Assigned(FOnError) then FOnError(Self,Result,'DecodeVideo : '+av_err2str(Result));
  end;
 finally
   CS.Leave;
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
  {* инициализация глобальных переменных *}
  (*FEndposition.TimeInt:=FormatContext.duration;
  FEndposition.ts := Round(FEndposition.TimeInt * av_q2d(Steams[video_stream_idx].stream.time_base));
  FEndposition.Time := UnixToDateTime(FEndposition.TimeInt div AV_TIME_BASE);//FEndposition.ts);*)
 finally
  {* Освобождение и закрытие*}
 end;
end;

function TMediaCore.ReadPacked(var Pack: PAVPacket): Integer;
begin
 cs.Enter;
 try
  Result := av_read_frame(FAVFormatContext, Pack);
  if Result < 0 then
  begin
   av_packet_unref(Pack);
   if Assigned(FOnError) then FOnError(Self,Result,'ReadPacked : '+av_err2str(Result));
  end;
 finally
   CS.Leave;
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
  Timer:=TTimer.Create(self);
  Timer.Enabled:=False;
  Timer.OnTimer:=OnTimer;
end;

destructor TMediaDecoder.Destroy;
begin
  FreeAndNil(Timer);
  inherited;
end;

procedure TMediaDecoder.OnIsPlayed(var Play: Boolean);
begin
 CS.Enter;
 try
  Play:=Self.FPlay;
 finally
   CS.Leave;
 end;
end;

procedure TMediaDecoder.OnSyncTime(var GT: UInt64);
begin
 CS.Enter;
 try
  GT:=GlobalTime;
 finally
   CS.Leave;
 end;
end;

procedure TMediaDecoder.OnTimer(sender: TObject);
begin
 CS.Enter;
 try
  GlobalTime:=av_gettime - StartTime;
  //Inc(GlobalTime,Timer.Interval);
 finally
   CS.Leave;
 end;
end;

function TMediaDecoder.Start: Boolean;
begin Result:=False;
  FPlay:=False;
  if not Assigned(FDisplay) then Exit;
  if not Assigned(FVideo) then FVideo:=TVideoThread.Create(FVideoStrem);
  GlobalTime:=0;
  Timer.Interval:=10;
  FVideo.FOnDecodeFrame:=DecodeVideo;
  FVideo.FOnRenderVideoFrame:=FDisplay.RenderVideoFrame;
  FVideo.FOnReadPacked:=ReadPacked;
  FVideo.FOnIsPlayed:=OnIsPlayed;
  FVideo.FSyncTime:=OnSyncTime;
  StartTime:=av_gettime;
  FPlay:=True;
  Timer.Enabled:=FPlay;
end;

{ TVideoThread }

constructor TVideoThread.Create(FVideoStrem:PAVStream);
begin
  inherited Create(False);
  Self.FVideoStrem:=FVideoStrem;
end;

procedure TVideoThread.Execute;
var Play:Boolean;
  GT:UInt64;
  Decoded,Render:Boolean;
  tic1, tic2:UInt64;
  Delay:Int64;

  function isTimePak:Boolean;
  var tmp:Int64;
  begin
   tmp:=Round((FAVPacked.pts * FVideoStrem.time_base.num / FVideoStrem.time_base.den) * 1000);
   Result:= GT >= tmp
  end;

begin
  FAVPacked:=av_packet_alloc;
  FAVFrame:=av_frame_alloc;
  New(FGotFrame);
  Decoded:=False;
  Render:=False;
  try
    while not Self.Terminated do begin
      //Sleep(15);
      if Assigned(FOnIsPlayed) and
         Assigned(FSyncTime) and
         Assigned(FOnReadPacked) and
         Assigned(FOnDecodeFrame) and
         Assigned(FOnRenderVideoFrame)
      then
      begin
        FOnIsPlayed(Play);
        if Play then begin
          FSyncTime(GT);
          if (not Decoded) then begin
            tic1:=GetTickCount;
            Decoded := FOnReadPacked(FAVPacked) = 0;
            if (FAVPacked.stream_index) <> FVideoStrem.index then begin //nedd test
              Decoded:=False;
              av_packet_unref(FAVPacked);
            end;
          end;
          if Decoded then begin
            if isTimePak then begin Decoded:=False;
              Render:= FOnDecodeFrame(FAVPacked,FAVFrame,FGotFrame) > 0;
              if Render then begin
                FOnRenderVideoFrame(
                    FVideoStrem.codec.width,
                    FVideoStrem.codec.height,
                    FAVFrame.data,
                    FAVFrame.linesize,
                    FVideoStrem.codec.pix_fmt
                );
                Decoded:=False; Render:=False;
              end;
              tic2:=GetTickCount;
              Delay:=round((FAVPacked.duration * (FVideoStrem.time_base.num / FVideoStrem.time_base.den)* 1000))-(tic2-tic1);
              if Delay < 0 then Delay := 0;              
              Sleep(Delay);
            end; //is time end
          end;
        end;
      end;
    end;
  finally
    av_frame_free(@FAVFrame);
    av_packet_free(@FAVFrame);
    Dispose(FGotFrame);
  end;
end;

initialization
  (*av_register_all();
  avcodec_register_all();*)

end.
