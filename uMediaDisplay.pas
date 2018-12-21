unit uMediaDisplay;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,

  uMediaConstant,

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

type
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
  protected
    procedure Paint; override;
    Function CaslcSize(Sourse:TSDL_Rect):TSDL_Rect;
    procedure OnRenderVideo(var Data:PMediaBufferInfo);
  public
    Function GetSDLInfo:PSDLPantalla;
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

implementation

{ TMediaDisplay }

procedure TMediaDisplay.CanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
var Rect:TSDL_Rect;
begin
  {CS.Enter;
  try
    Rect.x:=0; Rect.y:=0; Rect.w:=NewWidth; Rect.h:=NewHeight;
    Resize:=Ini3DCanvas(Rect);
  finally
    CS.Leave;
  end;}
end;

function TMediaDisplay.CaslcSize(Sourse: TSDL_Rect): TSDL_Rect;
var rect:TSDL_Rect;
    WinRect:TRect;
    pw,ph,p:Double;
begin
  if not Assigned(FSDLPantalla.Renderer) then Exit;
  SDL_RenderGetViewport(FSDLPantalla.Renderer,@rect);
  WinRect:=GetClientRect;
  Result.x := 0;
  Result.y := 0;
  Result.w := Sourse.w;
  Result.h := Sourse.h;
  p := (WinRect.Width * 100) / Result.w;
  Result.w := round((Result.w * p) / 100);
  Result.h := round((Result.h * p) / 100);
  if Result.h > rect.h then begin
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
  FProportionally:=true;//False;
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

function TMediaDisplay.GetSDLInfo: PSDLPantalla;
begin
  Result:=FSDLPantalla;
end;

function TMediaDisplay.Ini3DCanvas(Rect: TSDL_Rect): Boolean;
var
 rect2:TSDL_Rect;
begin  Result:=True;
 try
  //rect2:=CaslcSize(rect);
  rect2:=rect;
  //if assigned(FSDLPantalla.Window.surface) then begin
   (*if
    (FSDLPantalla.Window.surface.w <> rect2.w{AVStream.codec.width}) or
    (FSDLPantalla.Window.surface.h <> rect2.h{AVStream.codec.height}) then*)
     Free3DCanvas;
  //end;
  // Создаём текстуру для отображения кадра
  (*if not assigned(FSDLPantalla.Window.surface) then begin
    FSDLPantalla.Window.surface := SDL_CreateRGBSurface(0, rect2.w{AVStream.codec.width},
      rect2.h{AVStream.codec.height}, 24, $000000FF, $0000FF00, $00FF0000, $00000000);
    if FSDLPantalla.Window.surface = nil then
      raise Exception.Create('CreateRGBSurface failed:'+SDL_GetError());
  end;*)
  if not assigned(MooseTexture) then begin
    MooseTexture := SDL_CreateTexture(FSDLPantalla.Renderer,
      SDL_PIXELFORMAT_ARGB8888,
      1,//SDL_TEXTUREACCESS_STREAMING
      rect2.w,rect2.h);
    (*MooseTexture := SDL_CreateTextureFromSurface(FSDLPantalla.Renderer,
      FSDLPantalla.Window.surface);*)
    if MooseTexture = nil then
     raise Exception.Create('CreateTextureFromSurface failed:'+SDL_GetError());
  end;

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
 {if FSDLPantalla <> nil then begin
   //if FSDLPantalla.Window.surface = nil then begin
   if MooseTexture = nil then begin
    CS.Enter;
    try
     r:=GetClientRect;
     Ini3DCanvas(TSDL_Rect(R));
    finally
      CS.Leave;
    end;
   end;
 end;}
 (* Default render screen *)
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
 (*if {(not assigned(FSDLPantalla.Window.surface)) OR }(not assigned(MooseTexture)) then begin
   Exit;
 end;*)
 if Assigned(CS) then CS.Enter;
 try
  pix_F := AV_PIX_FMT_BGR0;
  // Задаём размеры прямоугольника в дальнейшем по мену будем считать преобразование размера кадра
  rect.x := 0; rect.y := 0;
  //MyRect:=GetClientRect;
  rect.w := w; // шырена
  rect.h := h; // высота
  rect2:=CaslcSize(rect);
  Ini3DCanvas(rect2);
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
    //SDL_RenderClear(FSDLPantalla^.Renderer);
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
     //Free3DCanvas;
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
  if Assigned(CS) then CS.Leave;
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



end.
