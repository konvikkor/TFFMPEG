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
  libswresample, libswscale, sdl2, {sdl, {uResourcePaths,} System.Threading,
  FMX.Graphics, FMX.Types, FMX.Canvas.D2D;
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
    Function CaslcSize(Sourse:TSDL_Rect):TSDL_Rect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Function DisplayFrame(var AVFrame: TAVFrame; AVStream: PAVStream; DeleyTime:Cardinal = 0): Boolean;
    Function DisplayInit(AVStream: PAVStream): Boolean;
    Function DisplayFree: Boolean;
    Procedure UpdateRender;
    property DeleyTime:Byte read FDeley write SetDeley default 25;
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

  TBitmapInfoHeader = Packed Record
    Size:Int32;
    w,h:Int32;
    Color_Planes:Int16;
    Bit_Per_Pixel:Int16;
    Compression:Int32;
    ImageSize:Int32;
    ResolutionHorizontal:Int32;
    ResolutionVertical:Int32;
    ColorPlate:Int32;
    ColorUsed:Int32;
    RedMask,
    GreenMask,
    BlueMask,
    AlphaMask:Int32;
    LCS_WINDOWS_COLOR_SPACE:Int32;
    Color_Space_endpoints: Array [0..35] of byte;
    RedGamma:Int32;
    GreenGamma:Int32;
    BlueGamma:Int32;
  End;

  TBmpHead = packed record
    Name:Array [0..1] of AnsiChar;
    Size:Int32;
    Reserved: Array [0..3] of byte;
    StartAdress:Int32;
    FormatHead:TBitmapInfoHeader;
    Class Function Init:TBmpHead; Static;
    Class Function InitSize(w,h, DataSize:Int32):TBmpHead; Static;
  end;

  TOnHookFrame = procedure (Frame:TAVFrame; Paket:TAVPacket; Steam:TAVStream)of object;

  TMyFFMpegCore = class (TComponent)
  private
    FOnHookFrame: TOnHookFrame;
    function GetStream(Index: Integer): TStream;
  protected
    FSeekTarget: Integer;
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

    got_Frame:PInteger;

    //Tasks: Array of ITask;
    FStop:Boolean;
    CS:TCriticalSection;
  public
    [Volatile]
    AVPacket: TAVPacket;
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
    property OnHookFrame:TOnHookFrame read FOnHookFrame Write FOnHookFrame;
  end;

  TMyFFMpeg = class(TMyFFMpegCore)
  private
   procedure OnStatusThead(var PlayStop:Boolean; var Close:Boolean);
   procedure OnReadPaket(var Deley:Cardinal);
   procedure OnSeek(var PlayStop:Boolean; var Close:Boolean);
   procedure OnDecodeVIDEO(var Deley:Cardinal);
   procedure OnDecodeAUDIO(var Deley:Cardinal);
   procedure OnRunVIDEO(var Deley:Cardinal);
   procedure OnRunAUDIO(var Deley:Cardinal);
   procedure OnDelay(var TotalDelay:Cardinal);
  protected
   Task:ITask;
  public
   class var MyDecodeThead:Pointer;//TThread;
   Class Function SaveFrameAsBitmap(Frame:TAVFrame; Steam:TAVStream; var Pict: Vcl.Graphics.TBitmap):Boolean;
   Procedure Play;
   Procedure Stop;
   function GetStatusPlay:Boolean;
   destructor Destroy; override;
   constructor Create(AOwner: TComponent); override;
  published
   Property Seek:Integer read FSeekTarget Write FSeekTarget default -1;
  end;

procedure Register;

implementation

uses uFFMpegThead;

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

function TMyFFMpegDisplay.CaslcSize(Sourse: TSDL_Rect): TSDL_Rect;
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
  // Задаём размеры прямоугольника в дальнейшем по мену будем считать преобразование размера кадра
  rect.x := 0;
  rect.y := 0;
  rect.w := AVStream.codec.width; // шырена
  rect.h := AVStream.codec.height; // высота
  rect2:=CaslcSize(rect);
  // Очистка от предыдущий ошибок
  Img := av_frame_alloc(); // Создаём пространство для конвертированного кадра
  ticB:=GetTickCount;
  // получаем контекст для преобразования в RGBы
  ImgConvContext := nil;
  ImgConvContext := sws_getContext(AVStream.codec.width, AVStream.codec.height,
    AVStream.codec.pix_fmt, rect2.W{AVStream.codec.width}, rect2.h{AVStream.codec.height}, pix_F, SWS_BILINEAR, nil,
    nil, nil);
  // Формируем буфер для картинки RGB
  Ibmp_Size := avpicture_get_size(pix_F, rect2.w{AVStream.codec.width}, rect2.H{AVStream.codec.height});
  ibmp_Buff := nil;
  ibmp_Buff := av_malloc(Ibmp_Size * SizeOf(Byte));
  res := avpicture_fill(PAVPicture(Img), ibmp_Buff, pix_F, rect2.w{AVStream.codec.width},
    rect2.h{AVFrame.height});
  // конвертируем формат пикселя в RGB
  if Assigned(ImgConvContext) then
   res := sws_scale(ImgConvContext, @AVFrame.Data, @AVFrame.linesize, 0,
      AVStream.codec.height, @Img.Data, @Img.linesize);
  TicE:=GetTickCount;
  try
    // рисуем картинку на текстуре
    res := SDL_UpdateTexture(MooseTexture, @rect2, @Img.Data[0]^,
      Img.linesize[0]);
    if res < 0 then
      Result := False;
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
  //av_freep(ibmp_Buff);
  sws_freeContext(ImgConvContext);
  //av_frame_unref(@Img);
  av_frame_free(@Img);
  Application.ProcessMessages;
  SDL_RenderPresent(FSDLPantalla^.Renderer);
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
var
  rect, rect2:TSDL_Rect;
begin  Result:=True;
 try
  rect.x := 0;
  rect.y := 0;
  rect.w := AVStream.codec.width; // шырена
  rect.h := AVStream.codec.height; // высота
  rect2:=CaslcSize(rect);
  if assigned(FSDLPantalla.Window.surface) then begin
   if
    (FSDLPantalla.Window.surface.w <> rect2.w{AVStream.codec.width}) or
    (FSDLPantalla.Window.surface.h <> rect2.h{AVStream.codec.height}) then
     DisplayFree;
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

procedure TMyFFMpegDisplay.OnTimer(sendder: TObject);
begin
  //SDL_RenderPresent(FSDLPantalla^.Renderer);
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

procedure TMyFFMpegDisplay.UpdateRender;
begin
 if Assigned(FSDLPantalla^.Renderer) then
  SDL_RenderPresent(FSDLPantalla^.Renderer);
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
  av_init_packet(@AVPacket);
  //av_frame_unref(frame);
  AVPacket.Data := nil;
  AVPacket.size := 0;
  New(got_Frame);
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
  ret := avcodec_decode_video2(Steams[SteamID].stream^.codec, @AVFrame{@frame}, got_frame, @AVPacket);
  if ret < 0 then
  begin
    Result := ret;
    raise Exception.Create('Error decoding video frame (' + string(av_err2str(ret)) + ')');
  end;
  TicE:=GetTickCount;
  if got_frame^ <> 0 then begin
   if ((AVPacket.dts = AV_NOPTS_VALUE) and assigned(AVFrame.opaque) and (Int64(AVFrame.opaque^) <> AV_NOPTS_VALUE)) then
    begin
     pts := Int64(AVFrame.opaque^);
    end
   else if (AVPacket.dts <> AV_NOPTS_VALUE) then
    begin
     pts := AVPacket.dts
    end else pts := 0;
   //FCurrentPosition.TimeInt := round(AVFrame.pts{pts} * av_q2d(Steams[SteamID].stream^.time_base));
   FCurrentPosition.TimeInt := AVFrame.pts{pts} * Steams[SteamID].stream^.time_base.den;
   FCurrentPosition.ts := AVFrame.pts;{pts};
   FCurrentPosition.Time := UnixToDateTime(FCurrentPosition.TimeInt);
  end;
  Result:=TicE-TicB;
end;

destructor TMyFFMpegCore.Destroy;
begin
  //if Assigned(frame) then av_free(frame);
  Dispose(got_Frame);
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
 iSeekTarget := ts*1000;
 try
  avcodec_flush_buffers(Steams[SteamID].stream.codec);
  iSeekTarget := av_rescale_q(iSeekTarget, AV_TIME_BASE_Q, Steams[SteamID].stream.time_base);
  iSeekFlag := 0;//AVSEEK_FLAG_BACKWARD;// or AVSEEK_FLAG_ANY or AVSEEK_FLAG_FRAME;
  if iSeekTarget > FEndposition.ts then iSeekTarget := ts;
  iSuccess := avformat_seek_file(FormatContext,
                                 video_stream_idx,
                                 FStartPosition.ts,//0,
                                 iSeekTarget,
                                 FEndposition.ts,//Round(FormatContext.duration * av_q2d(Steams[SteamID].stream.time_base)),
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
  ret := av_read_frame(FormatContext, @AVPacket);
  TicE1:=GetTickCount;
  if ret < 0 then
  begin
   av_packet_unref(@AVPacket);
   raise Exception.Create('Error Message:Can not read Frame ' + sLineBreak + av_err2str(ret));
  end;
  Result:=TicE1-TicB1;
end;

function TMyFFMpegCore.ReadFrameTry: Cardinal;
var TicB1,TicE1:Cardinal;
  ret:Integer;
begin
  TicB1:=GetTickCount;
  ret := av_read_frame(FormatContext, @AVPacket);
  TicE1:=GetTickCount;
  if ret < 0 then
  begin
   av_packet_unref(@AVPacket);
   //raise Exception.Create('Error Message:Can not read Frame ' + sLineBreak + av_err2str(ret));
  end;
  Result:=TicE1-TicB1;
end;

{ TMyFFMpeg }

constructor TMyFFMpeg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStop:=True;
  Self.MyDecodeThead:=TMyDecodeThead.Create(False);
  TMyDecodeThead(Self.MyDecodeThead).FreeOnTerminate:=False;
  TMyDecodeThead(Self.MyDecodeThead).MyFFMpeg:=Self;
  TMyDecodeThead(Self.MyDecodeThead).Priority:=tpNormal;
  TMyDecodeThead(Self.MyDecodeThead).OnStatusThead:=OnStatusThead;
  TMyDecodeThead(Self.MyDecodeThead).OnReadPaket:=OnReadPaket;
  TMyDecodeThead(Self.MyDecodeThead).OnSeek:=OnSeek;
  TMyDecodeThead(Self.MyDecodeThead).OnDecodeVIDEO:=OnDecodeVIDEO;
  TMyDecodeThead(Self.MyDecodeThead).OnDecodeAUDIO:=OnDecodeAUDIO;
  TMyDecodeThead(Self.MyDecodeThead).OnRunVIDEO:=OnRunVIDEO;
  TMyDecodeThead(Self.MyDecodeThead).OnRunAUDIO:=OnRunAUDIO;
  TMyDecodeThead(Self.MyDecodeThead).OnDelay:=OnDelay;
  TMyDecodeThead(Self.MyDecodeThead).OnHookFrame:=FOnHookFrame;
end;

destructor TMyFFMpeg.Destroy;
begin
  FStop:=True;
  if Assigned(MyDecodeThead) then begin
    TMyDecodeThead(Self.MyDecodeThead).Terminate;
  end;
  FreeAndNil(TMyDecodeThead(Self.MyDecodeThead));
  Application.ProcessMessages;
  inherited;
end;

function TMyFFMpeg.GetStatusPlay: Boolean;
begin
  Result:=FStop;
end;

procedure TMyFFMpeg.OnDecodeAUDIO(var Deley: Cardinal);
begin
 //not use
end;

procedure TMyFFMpeg.OnDecodeVIDEO(var Deley: Cardinal);
var TicB,TicE:Cardinal;
    ret:Integer;
    pts: Int64;
begin
  //if not Assigned(frame) then Frame := av_frame_alloc();
  (* decode video frame *)
  if AVPacket.stream_index <> video_stream_idx then begin
   Deley:=$FFFFFFFF;
   exit;
  end;
  TicB:=GetTickCount;
  ret := avcodec_decode_video2(Steams[AVPacket.stream_index].stream^.codec, @AVFrame{@frame}, got_frame, @AVPacket);
  if ret < 0 then
  begin
    Deley := ret;
    raise Exception.Create('Error decoding video frame (' + string(av_err2str(ret)) + ')');
  end;
  TicE:=GetTickCount;
  if got_frame^ <> 0 then begin
   if ((AVPacket.dts = AV_NOPTS_VALUE) and assigned(AVFrame.opaque) and (Int64(AVFrame.opaque^) <> AV_NOPTS_VALUE)) then
    begin
     pts := Int64(AVFrame.opaque^);
    end
   else if (AVPacket.dts <> AV_NOPTS_VALUE) then
    begin
     pts := AVPacket.dts
    end else pts := 0;
   //FCurrentPosition.TimeInt := round(AVFrame.pts{pts} * av_q2d(Steams[SteamID].stream^.time_base));
   FCurrentPosition.TimeInt := AVFrame.pts{pts} * (Steams[AVPacket.stream_index].stream^.time_base.num div Steams[AVPacket.stream_index].stream^.time_base.den);
   FCurrentPosition.ts := AVFrame.pts;{pts};
   FCurrentPosition.Time := UnixToDateTime(FCurrentPosition.TimeInt);
   if Assigned(Self.FOnHookFrame) then Self.FOnHookFrame(AVFrame,AVPacket,Steams[AVPacket.stream_index].stream^);
  end;
  Deley:=Deley+(TicE-TicB);
end;

procedure TMyFFMpeg.OnDelay(var TotalDelay: Cardinal);
begin
  av_packet_unref(@AVPacket);
  av_frame_unref(@AVFrame);
  TotalDelay:=0;
end;

procedure TMyFFMpeg.OnReadPaket(var Deley:Cardinal);
var TicB1,TicE1:Cardinal;
  ret:Integer;
begin
  TicB1:=GetTickCount;
  ret := av_read_frame(FormatContext, @AVPacket);
  TicE1:=GetTickCount;
  if ret < 0 then
  begin
   av_packet_unref(@AVPacket);
   //raise Exception.Create('Error Message:Can not read Frame ' + sLineBreak + av_err2str(ret));
  end;
  Deley:=Deley+(TicE1-TicB1);
end;

procedure TMyFFMpeg.OnRunAUDIO(var Deley: Cardinal);
begin
  //not use
end;

procedure TMyFFMpeg.OnRunVIDEO(var Deley: Cardinal);
var d:Int64;
begin
 d:=Deley;
 //d:=round(av_q2d(Steams[video_stream_idx].stream.avg_frame_rate)-Deley);
 if d < 0 then Deley:=0
 else Deley:=d;
 if AVPacket.stream_index <> video_stream_idx then Exit;
 if Assigned(FFMpedDisplay) then begin
  FFMpedDisplay.DisplayInit(Steams[AVPacket.stream_index].stream);
  FFMpedDisplay.DisplayFrame(AVFrame,Steams[AVPacket.stream_index].stream,Deley);
 end;
 av_frame_unref(@AVFrame);
end;

procedure TMyFFMpeg.OnSeek(var PlayStop, Close: Boolean);
begin
 PlayStop:=FStop;
 Close:=False;
 if FSeekTarget > 0 then begin
  GoToFrameToTS(video_stream_idx,FSeekTarget);
  FSeekTarget:=-1;
 end;
end;

procedure TMyFFMpeg.OnStatusThead(var PlayStop, Close: Boolean);
begin
 PlayStop:=FStop;
 Close:=False;
end;

procedure TMyFFMpeg.Play;
var got_frame:PInteger;
  Delay:Cardinal;
  Size:Int64;
begin
  if not Assigned(FormatContext) then Exit;
  if not Assigned(MyDecodeThead) then exit;
  FStop:=False;
  if not TMyDecodeThead(Self.MyDecodeThead).Started then TMyDecodeThead(Self.MyDecodeThead).Start;
end;

class function TMyFFMpeg.SaveFrameAsBitmap(Frame: TAVFrame;  Steam:TAVStream;
  var Pict: Vcl.Graphics.TBitmap): Boolean;
var
  res: SInt32;
  ImgConvContext: PSwsContext;
  Img: PAVFrame;
  Ibmp_Size: Integer;
  ibmp_Buff: PByte;
  pix_F: TAVPixelFormat;

  TmpData:Pointer;
  FileLine:TArray<Byte>;

  Color: Array [0 .. 3] of Byte;
  c1,c2,c3,c4:Byte;
  RGB:TColor;
  i, j, linesize: Integer;

  BMPH:TBmpHead;

  mem:TMemoryStream;
begin Result := False;
 if Steam.codec.codec_type <> AVMEDIA_TYPE_VIDEO then Exit;
 try
  pix_F := AV_PIX_FMT_BGR0;
  mem:=TMemoryStream.Create;
  Img := av_frame_alloc(); // Создаём пространство для конвертированного кадра
  ImgConvContext := nil;
  ImgConvContext := sws_getContext(Steam.codec.width, Steam.codec.height,
    Steam.codec.pix_fmt, Steam.codec.width, Steam.codec.height, pix_F, SWS_BILINEAR, nil,
    nil, nil);
  // Формируем буфер для картинки RGB
  Ibmp_Size := avpicture_get_size(pix_F, Steam.codec.width, Steam.codec.height);
  ibmp_Buff := nil;
  ibmp_Buff := av_malloc(Ibmp_Size * SizeOf(Byte));
  res := avpicture_fill(PAVPicture(Img), ibmp_Buff, pix_F, Steam.codec.width, Steam.codec.height);
  // конвертируем формат пикселя в RGB
  if Assigned(ImgConvContext) then
   res := sws_scale(ImgConvContext, @Frame.Data, @Frame.linesize, 0, Steam.codec.height, @Img.Data, @Img.linesize);
  TmpData:=@Img.Data[0]^;//Pointer(integer(@Img.Data[0]^)+(Img.linesize[0]*(Steam.codec.height)));
  pict.PixelFormat := pf24bit;
  Pict.Create;
  Pict.width := Steam.codec.width;
  Pict.height := Steam.codec.height;
  SetLength(FileLine,Img.linesize[0]);
  BMPH:=BMPH.InitSize(Steam.codec.width, Steam.codec.height,Ibmp_Size);
  mem.Write(BMPH,SizeOf(BMPH));
  try
      for i := Steam.codec.height downto 1 do begin
       FillChar(FileLine[0],Img.linesize[0],0);
       linesize:=0;
       TmpData:= Pointer(Integer(@Img.Data[0]^)+((Img.linesize[0]*i)-Img.linesize[0]));
       repeat
         c1:=Byte(PByte(Pointer(Integer(TmpData)+linesize))^);inc(linesize); //R
         c2:=Byte(PByte(Pointer(Integer(TmpData)+linesize))^);inc(linesize); //G
         c3:=Byte(PByte(Pointer(Integer(TmpData)+linesize))^);inc(linesize); //B
         c4:=Byte(PByte(Pointer(Integer(TmpData)+linesize))^);inc(linesize); //A
         FileLine[linesize-4]:=c1;
         FileLine[linesize-3]:=c2;
         FileLine[linesize-2]:=c3;
         FileLine[linesize-1]:=c4;
       until linesize >= Img.linesize[0];
       mem.WriteData(FileLine,linesize);
      end;
  finally
   mem.Position:=0;
   //mem.SaveToFile('E:\Vidio\fr\'+IntToStr(Frame.pts)+'.bmp');
   //mem.Position:=0;
   Pict.LoadFromStream(mem);
   FreeAndNil(mem);
  end;
 finally
  // очистка памяти от временного мусора
  av_free(ibmp_Buff);
  sws_freeContext(ImgConvContext);
  av_frame_free(@Img);
  Application.ProcessMessages;
 end;
end;

procedure TMyFFMpeg.Stop;
begin
  FStop:=True;
end;

{ TBmpHead }

class function TBmpHead.Init: TBmpHead;
begin
 Result.Name[0]:='B';
 Result.Name[1]:='M';
 Result.Reserved[0]:=0;
 Result.Reserved[1]:=0;
 Result.Reserved[2]:=0;
 Result.Reserved[3]:=0;
 Result.StartAdress:=14+SizeOf(TBitmapInfoHeader);
 Result.FormatHead.Size:=SizeOf(TBitmapInfoHeader);
 Result.FormatHead.Compression:=3;
 Result.FormatHead.ResolutionHorizontal:=2835;
 Result.FormatHead.ResolutionVertical:=2835;
 Result.FormatHead.Bit_Per_Pixel:=32;
 Result.FormatHead.ColorPlate:=0;
 Result.FormatHead.ColorUsed:=0;
 Result.FormatHead.Color_Planes:=1;
 Result.FormatHead.RedMask   :=$00FF0000;
 Result.FormatHead.GreenMask :=$0000FF00;
 Result.FormatHead.BlueMask  :=$000000FF;
 Result.FormatHead.AlphaMask :=$FF000000;
 Result.FormatHead.LCS_WINDOWS_COLOR_SPACE:=$206E6957;
 //Result.FormatHead.Color_Space_endpoints: Array [0..35] of byte;
 Result.FormatHead.RedGamma:=0;
 Result.FormatHead.GreenGamma:=0;
 Result.FormatHead.BlueGamma:=0;
end;

class function TBmpHead.InitSize(w, h, DataSize: Int32): TBmpHead;
begin
  Result:=TBmpHead.Init;
  Result.FormatHead.w:=w;
  Result.FormatHead.h:=h;
  Result.FormatHead.ImageSize:=DataSize;
  Result.Size:=Result.FormatHead.ImageSize+SizeOf(TBmpHead);
end;

end.
