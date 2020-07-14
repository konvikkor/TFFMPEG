unit uMediaDisplay;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,

  uMediaConstant,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, OpenGL, System.TimeSpan,
  //Winapi.D3D11Shader, Winapi.D3D11,

  Vcl.ExtCtrls,Math,System.SyncObjs,System.Generics.Collections,

  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.DateUtils,
  libavcodec, libavdevice, libavfilter, libswresample, libswscale,
  libavutil, libavformat,
  sdl2, {SDL2_ttf,{sdl, {uResourcePaths,} System.Threading;

//const
  //GL_BGR = $80E0;

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
    FAutoInitSDL: Boolean;
    RenderBMP:TBitmap;
    FASCIICharList: GLuint;
    GlobalTime,PackTime:Cardinal;
    FDrawInfo: Boolean;
    CalcFPS:Double;
  protected
    RenderBitmap:BITMAP;
    procedure Paint; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    //Function CaslcSize(Sourse:TSDL_Rect):TSDL_Rect;
    //procedure OnRenderVideo(var Data:PMediaBufferInfo);
    function GetSizeProporcional(Source_x,Source_y:Integer):TPoint;
    Procedure InitSDL;
    Procedure DeInitSDL;
    procedure setupPixelFormat(DC:HDC);
    Procedure OnTimer(Sender:TObject);
    Procedure WMUpdateRender(var Message: TWMEraseBkgnd);message WM_PAINT_FRAME;
  public
    HDC:HDC;
    RC:HGLRC;
    Function GetSDLInfo:PSDLPantalla;
    function GetCanvas:TCanvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure UpdateRender;
    //procedure RenderVideoFrame(w,h:SInt32;Data:Array of PByte;linesize: Array of Integer; pix_fmt:TAVPixelFormat);
    procedure EraseBackground(var Message: TWMEraseBkgnd);message WM_ERASEBKGND;
    procedure Render(w,h:SInt32;Data:Array of PByte;linesize: Array of Integer);
    Function DrawBitmap(bmp: TBitmap; x, y: Integer; xZoom: Single = 1.0; yZoom: Single = 1.0):TMediaDisplay;
    function BuildTexture(bmp: TBitmap; var texId: GLuint):TMediaDisplay;
    function DeleteTexture(texId: GLuint): TMediaDisplay;
    function DrawBitmapTex(texId: GLuint; x, y, w, h: Integer): TMediaDisplay; overload;
    function DrawBitmapTex(bmp: TBitmap; x, y, w, h: Integer): TMediaDisplay; overload;
    Procedure BeginRender;
    Procedure UpdateRender;
    Procedure SetBitmap(BMP:TBitmap;GlobalTime,PackTime:Cardinal);
    function TextOut(const text: WideString; x, y: Integer; red, green, blue, alpha: Single): TMediaDisplay;
    Procedure EndRender;
    function Bitmap2PixelArray(Source:TBitmap):BITMAP;
  published
    //Property AutoInitSDL:Boolean Read FAutoInitSDL Write FAutoInitSDL default true;
    Property DrawInfo:Boolean Read FDrawInfo Write FDrawInfo default false;
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

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MY Media',[TMediaDisplay]);
end;

{ TMediaDisplay }

(*function TMediaDisplay.CaslcSize(Sourse: TSDL_Rect): TSDL_Rect;
var rect:TSDL_Rect;
    WinRect:TRect;
    pw,ph,p:Double;
begin
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
end;*)

procedure TMediaDisplay.BeginRender;
begin
 CS.Enter;
end;

function TMediaDisplay.Bitmap2PixelArray(Source: TBitmap): BITMAP;
begin
  GetObject(Source.Handle, SizeOf(Result), @Result);
end;

function TMediaDisplay.BuildTexture(bmp: TBitmap;
  var texId: GLuint): TMediaDisplay;
var
   bmpInfo: BITMAP;
begin
   GetObject(bmp.Handle, SizeOf(bmpInfo), @bmpInfo);
   glGenTextures(1, @texId);          // Create The Texture
   glPixelStorei(GL_PACK_ALIGNMENT, 1);
   // Typical Texture Generation Using Data From The Bitmap
   glBindTexture(GL_TEXTURE_2D, texId);        // Bind To The Texture ID
   (* TEST ADD > *)
   //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
   //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
   (* TEST ADD < *)
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); // Linear Min Filter
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); // Linear Mag Filter
   if bmpInfo.bmBitsPixel = 32 then
    glTexImage2D(GL_TEXTURE_2D, 0, GL_BGRA_EXT, bmpInfo.bmWidth, bmpInfo.bmHeight, 0, GL_BGRA_EXT, GL_UNSIGNED_BYTE, bmpInfo.bmBits)
   else
    glTexImage2D(GL_TEXTURE_2D, 0, GL_BGR_EXT, bmpInfo.bmWidth, bmpInfo.bmHeight, 0, GL_BGR_EXT, GL_UNSIGNED_BYTE, bmpInfo.bmBits);
   (* TEST > *)
   //glGenerateMipmap(GL_TEXTURE_2D);
   (* TEST < *)
   Result := Self;
end;

constructor TMediaDisplay.Create(AOwner: TComponent);
begin
  inherited;
  CS:=TCriticalSection.Create;
  Self.Height:=200;
  Self.Width:=200;
  Self.Align:=alNone;
  //Self.OnCanResize:=CanResize;
  FSDLPantalla := nil;
  //if FSDLPantalla = nil then New(FSDLPantalla);
  FProportionally:=true;//False;
  //SDL_Init(SDL_INIT_VIDEO);
  //InitSDL;
  RenderBMP:=TBitmap.Create;
end;

procedure TMediaDisplay.CreateWnd;
begin
  inherited CreateWnd;
  hdc:=GetDC(Self.Handle);
  SetupPixelFormat(hdc);
  RC:=wglCreateContext(hdc); //makes OpenGL window out of DC
  wglMakeCurrent(hdc, RC);   //makes OpenGL window active
  //glMatrixMode(GL_PROJECTION);
  //glFrustum(-0.1, 0.1, -0.1, 0.1, 0.3, 25.0);
  //glFrustum ( -50 , 50 , -50 , 50 , 0.3{1.25} , 100.0 ); //Область видимости
  //glMatrixMode(GL_MODELVIEW);
  //glLoadIdentity;
  //gluLookAt(0,0,5,0,0,0,0,0,1); //Позиция наблюдения
  glEnable(GL_DEPTH_TEST); // включаем проверку разрешения фигур (впереди стоящая закрывает фигуру за ней)
  //glEnable(GL_ALPHA_TEST); //разрешаем альфа-тест
  //glAlphaFunc(GL_GREATER,0.0);  // устанавливаем параметры
  //glEnable (GL_BLEND);     //Включаем режим смешивания цветов
  //glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA) ; //параметры смешивания
  glDepthFunc(GL_LEQUAL); //тип проверки
  //glEnable(GL_ALPHA_TEST);
  //glShadeModel(GL_FLAT); //Режим без сглаживания
  //или
  glShadeModel(GL_SMOOTH); //Сглаживание. По умолчанию установлен режим GL_SMOOTH.
  glEnable(GL_TEXTURE_2D); //разрешить режим наложения текстуры
  (* Освещение *)
  glEnable(GL_LIGHTING);
  {Timer:=TTimer.Create(self);
  Timer.OnTimer:=OnTimer;
  Timer.Interval:=20;}
  (*glClearColor (0.5, 0.5, 0.75, 1.0); //Цвет фона
  glClear (GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT or GL_COLOR_BUFFER_BIT); //Очистка буфера цвета и глубины
  glLoadIdentity;
  glViewport(0,0,ClientWidth,ClientHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(0,ClientWidth,0,ClientHeight);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;*)
end;

procedure TMediaDisplay.DeInitSDL;
begin
  if FSDLPantalla = nil then exit;
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
  FSDLPantalla:=nil;
  SDL_Quit;
end;

function TMediaDisplay.DeleteTexture(texId: GLuint): TMediaDisplay;
begin
   glDeleteTextures(1, @texId);
   Result := Self;
end;

destructor TMediaDisplay.Destroy;
begin
  FreeAndNil(RenderBMP);
  FreeAndNil(CS);
  DeInitSDL;
  inherited;
end;

procedure TMediaDisplay.DestroyWnd;
begin
  FreeAndNil(Timer);
  DeInitSDL;
  (* Deinit OpenGL > *)
  wglMakeCurrent(HDC, 0);
  wglDeleteContext(RC);
  ReleaseDC(Self.Handle, HDC);
  (* Deinit OpenGL < *)
  inherited;
end;

function TMediaDisplay.DrawBitmap(bmp: TBitmap; x, y: Integer; xZoom,
  yZoom: Single): TMediaDisplay;
var
   bmpInfo: BITMAP;
   res:TPoint;
   texId:GLuint;
begin
   GetObject(bmp.Handle, SizeOf(bmpInfo), @bmpInfo);
   glPixelZoom(xZoom, yZoom);
   glPushMatrix;
   glLoadIdentity;
   glRasterPos2i(x, y);
   if bmpInfo.bmBitsPixel = 32 then
     glDrawPixels(bmp.Width, bmp.Height, GL_BGRA_EXT, GL_UNSIGNED_BYTE, bmpInfo.bmBits)
   else
     glDrawPixels(bmp.Width, bmp.Height, GL_BGR_EXT, GL_UNSIGNED_BYTE, bmpInfo.bmBits);
   glPopMatrix;
   Result := Self;
   exit;

   GetObject(bmp.Handle, SizeOf(bmpInfo), @bmpInfo);
   res:=GetSizeProporcional(bmp.Width, bmp.Height);
   glPixelZoom(xZoom, yZoom);
   glPushMatrix;
   glLoadIdentity;
   glRasterPos2i(x, y);
   Self.RenderBMP.Assign(bmp);
   Self.RenderBMP.Dormant;
   Self.RenderBitmap:=Bitmap2PixelArray(Self.RenderBMP);
   glPixelStorei(GL_PACK_ALIGNMENT, 1);
   // Typical Texture Generation Using Data From The Bitmap
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); // Linear Min Filter
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); // Linear Mag Filter
   if bmpInfo.bmBitsPixel = 32 then begin
     //glDrawPixels(bmp.Width, bmp.Height, GL_BGRA_EXT, GL_UNSIGNED_BYTE, bmpInfo.bmBits)
     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, bmp.Width, bmp.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, Self.RenderBitmap.bmBits);
   end else begin
     //glDrawPixels(bmp.Width, bmp.Height, GL_BGR_EXT, GL_UNSIGNED_BYTE, bmpInfo.bmBits);
     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, bmp.Width, bmp.Height, 0, GL_RGB, GL_UNSIGNED_BYTE, Self.RenderBitmap.bmBits);
   end;
   {glGenTextures(1, @texId);
   glBindTexture(GL_TEXTURE_2D, texId);        // Bind To The Texture ID
   //glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
   glBegin(GL_QUADS);
    (*glNormal3f( 0.0, 0.0, 1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);*)
    glNormal3f( 0.0, 0.0, 1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-(res.X-5), -(res.Y-5),  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( (res.X-5), -(res.Y-5),  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( (res.X-5),  (res.Y-5),  1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-(res.X-5),  (res.Y-5),  1.0);
   glEnd;}
   glPopMatrix;
   Result := Self;
end;

function TMediaDisplay.DrawBitmapTex(bmp: TBitmap; x, y, w,
  h: Integer): TMediaDisplay;
var
   tex: GLuint;
begin
   glColor3f(1.0, 1.0, 1.0);
   //glDisable(GL_BLEND);
   //glEnable(GL_TEXTURE_2D);
   BuildTexture(bmp, tex);

   glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0); glVertex3i(x, y, 0);
    glTexCoord2f(1.0, 0.0); glVertex3f(x + w, y, 0);
    glTexCoord2f(1.0, 1.0); glVertex3f(x + w, y + h, 0);
    glTexCoord2f(0.0, 1.0); glVertex3f(x, y + h, 0);
   glEnd;

   glDeleteTextures(1, @tex);
   //glDisable(GL_TEXTURE_2D);
   //glEnable(GL_BLEND);
   //SetBlendState(FBlend);
   //glColor4fv(@FPenColor); // Restore color
   Result := Self;
end;

function TMediaDisplay.DrawBitmapTex(texId: GLuint; x, y, w,
  h: Integer): TMediaDisplay;
begin
   glColor3f(1.0, 1.0, 1.0);
   glDisable(GL_BLEND);
   glEnable(GL_TEXTURE_2D);

   glBindTexture(GL_TEXTURE_2D, texid);        // Bind To The Texture ID
   glBegin(GL_QUADS);
   glTexCoord2f(0.0, 0.0); glVertex3i(x, y, 0);
   glTexCoord2f(1.0, 0.0); glVertex3f(x + w, y, 0);
   glTexCoord2f(1.0, 1.0); glVertex3f(x + w, y + h, 0);
   glTexCoord2f(0.0, 1.0); glVertex3f(x, y + h, 0);
   glEnd;

   glDisable(GL_TEXTURE_2D);
   glEnable(GL_BLEND);
   //glColor4fv(@FPenColor); // Restore color
   Result := Self;
end;

procedure TMediaDisplay.EndRender;
begin
 CS.Leave;
end;

procedure TMediaDisplay.EraseBackground(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

function TMediaDisplay.GetCanvas: TCanvas;
begin
  Result:=Self.Canvas;
end;

function TMediaDisplay.GetSDLInfo: PSDLPantalla;
begin
 Result:=FSDLPantalla;
end;

function TMediaDisplay.GetSizeProporcional(Source_x, Source_y: Integer): TPoint;
var p:Double;
begin
  Result.x := Source_x;
  Result.y := Source_y;
  p := (Self.Width * 100) / Result.x;
  Result.x := round((Result.x * p) / 100);
  Result.y := round((Result.y * p) / 100);
  if Result.y > Self.Height then begin
    p := (Self.Height * 100) / Result.y;
    Result.x := round((Result.x * p) / 100);
    Result.y := round((Result.y * p) / 100);
  end;
end;

procedure TMediaDisplay.InitSDL;
begin
  //if not Self.Showing then exit;
  if SDL_WasInit(SDL_INIT_VIDEO) <> SDL_INIT_VIDEO then
    SDL_InitSubSystem(SDL_INIT_VIDEO);
  FSDLPantalla.Window := SDL_CreateWindowFrom(Pointer(Self.Handle));
  if FSDLPantalla.Window <> nil then
  begin
    FSDLPantalla.Renderer := SDL_CreateRenderer(FSDLPantalla.Window, -1, 0{SDL_RENDERER_PRESENTVSYNC});
    // no forzamos ningъn tipo de render (0) para que el sistema coja el que pueda Hard-Soft
    if FSDLPantalla.Renderer <> nil then
    begin
      New(FRenderInfo);
      if SDL_GetRendererInfo(FSDLPantalla.Renderer, FRenderInfo) = 0 then
      begin
        FSDLPantalla.max_texture_width := FRenderInfo.max_texture_width;
        FSDLPantalla.max_texture_height := FRenderInfo.max_texture_height;
        FSDLPantalla.hardware :=
          ((FRenderInfo.Flags and SDL_RENDERER_ACCELERATED) > 0);
        FSDLPantalla.render_name := FRenderInfo.name;
        // PAnsiChar(FRenderInfo.name);
        SDL_ShowWindow(FSDLPantalla.Window);
        if SDL_SetRenderDrawColor(FSDLPantalla.Renderer, 0, 0, 0,
          SDL_ALPHA_OPAQUE) = 0 then
        begin
          if SDL_RenderFillRect(FSDLPantalla.Renderer, nil) = 0 then
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
end;

procedure TMediaDisplay.OnTimer(Sender: TObject);
begin
  Self.UpdateRender;
end;

procedure TMediaDisplay.Paint;
var Rect: TRect;
  Text:string;
begin
  //glClearColor (0.5, 0.5, 0.75, 1.0); //Цвет фона
  glClearColor (0, 0, 0, 1.0); //Цвет фона
  glClear (GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT or GL_COLOR_BUFFER_BIT); //Очистка буфера цвета и глубины
  glLoadIdentity; //Сбрасываем текущую матрицу
  glViewport(0,0,ClientWidth,ClientHeight); // размеры экрана что будем показывать
  glMatrixMode(GL_PROJECTION); //переходим в матрицу проекции
  glLoadIdentity; //Сбрасываем текущую матрицу
  glOrtho(-ClientWidth div 2,ClientWidth div 2,-ClientHeight div 2,ClientHeight div 2,-800,800);   //центровка в ноль по центру экрана*)
  //gluOrtho2D(-ClientWidth div 2,ClientWidth div 2,-ClientHeight div 2,ClientHeight div 2);   //центровка в ноль по центру экрана*)
  glMatrixMode(GL_MODELVIEW);  // переходим в модельную матрицу
  glLoadIdentity; //Сбрасываем текущую матрицу
  //gluLookAt(5,5,5,0,0,0,0,0,1); //позиция наблюдателя
  (* Перемещение *)
  //glTranslatef(x, y, z) где x, y, z –вектор смещения всей системы координат. сдвинем в сторону.
  //запоминания текущей системы координат glPushMatrix и восстановления glPopMatrix
  //glRotatef(угол, x, y, z) где x, y, z – координаты оси поворота
  (* Освещение *)
  glEnable(GL_LIGHT0); //включаем источник света №0

  (*
    glpushMatrix; //Запомнили
    glTranslatef(0,-5,0); //Сместили
      Рисуем додекаэдр
    glPopmatrix; //Восстановили
  *)

  //glTranslatef(0.0,0.0,-10.0);
  (* Test OpenGL > *)
  (*glBegin(GL_QUADS);
    glColor3f(1.0, 1.0, 1.0);
    glVertex2i(0,0);
    glColor3f(0.0, 0.0, 1.0);
    glVertex2i(0,ClientWidth);
    glColor3f(0.0, 1.0, 0.0);
    glVertex2i(ClientWidth,ClientHeight);
    glColor3f(1.0, 0.0, 0.0);
    glVertex2i(ClientWidth,0);
  glEnd;*)
  (* Test OpenGL < *)
  SwapBuffers(wglGetCurrentDC); //---------------------------
 (*Canvas.Lock;
 try
  (* Message Pain frame *
  Text:=' Video Canvas ';
  Rect := GetClientRect;
  with Canvas do begin
    Brush.Color:=clBlack;
    FillRect(Rect);
    Brush.Color:=clRed;
    Font.Name := 'arial';
    Font.Color := clBlack;
    Font.Style := [];
    Font.Height := 24;
    TextOut((Self.Width div 2)-(TextWidth(Text) div 2),(Self.Height div 2)-(TextHeight(Text) div 2),Text);
    Text:=' Display ['+IntToStr(Self.Tag)+'] '+Self.Caption+' ';//+FormatDateTime('hh.mm.ss.zzz',Time);
    Brush.Color:=clBlack;
    Font.Name := 'arial';
    Font.Color := clGreen;
    Font.Style := [];
    Font.Height := 16;
    TextOut((Self.Width div 2)-(TextWidth(Text) div 2),2,Text);
  end;
 finally
   Canvas.Unlock;
 end; *)
end;

procedure TMediaDisplay.Render(w, h: SInt32; Data: array of PByte;
  linesize: array of Integer);
var
  BMPFile:TMemoryStream;//TFileStream;
  BMPHeader:BITMAPFILEHEADER;
  BMPInfo:TBitmapV4Header;
  ret:Integer;

  bmp:TBitmap;
  Graphics : TGPGraphics;
  Img:TGPBitmap;
  Text:string;

  TextureID:LongInt;
  Buffer:BITMAP;

begin
 if Assigned(CS) then CS.Enter;
 try
  bmpheader.bfReserved1 := 0;
  bmpheader.bfReserved2 := 0;
  bmpheader.bfType := $4d42;
  bmpheader.bfOffBits := sizeof(BITMAPFILEHEADER) + sizeof(BITMAPV4HEADER);
  bmpheader.bfSize := bmpheader.bfOffBits + w*h*32 div 8;

  bmpinfo.bV4Size := sizeof(BITMAPV4HEADER);
  bmpinfo.bV4Width := w;
  bmpinfo.bV4Height := -h;
  bmpinfo.bV4Planes := 1;
  bmpinfo.bV4BitCount := 32;//24;
  bmpinfo.bV4V4Compression := BI_BITFIELDS;
  bmpinfo.bV4SizeImage := 0;
  bmpinfo.bV4XPelsPerMeter := 100;//2835; // ResolutionHorizontal
  bmpinfo.bV4YPelsPerMeter := 100;//2835; //ResolutionVertical
  bmpinfo.bV4ClrUsed := 0;
  bmpinfo.bV4ClrImportant := 0;
  BMPInfo.bV4RedMask:=$00FF0000;
  BMPInfo.bV4GreenMask:=$0000FF00;
  BMPInfo.bV4BlueMask:=$000000FF;
  BMPInfo.bV4AlphaMask:=$FF000000;
  BMPInfo.bV4CSType:=$206E6957;
  BMPInfo.bV4GammaRed:=0;
  BMPInfo.bV4GammaGreen:=0;
  BMPInfo.bV4GammaBlue:=0;

  //BMPFile:=TFileStream.Create(ExtractFilePath(GetCurrentDir)+'TEST'+IntToStr(Random(500))+'.jpg',fmCreate);
  BMPFile:=TMemoryStream.Create;
  bmp:=TBitmap.Create;

  //FBitMap.Lock;
  //Graphics := TGPGraphics.Create(FBitMap.Handle);
  try
    BMPFile.WriteBuffer(bmpheader,SizeOf(bmpheader));
    BMPFile.WriteBuffer(bmpinfo,SizeOf(bmpinfo));
    BMPFile.WriteBuffer(data[0]^,w*h*32 div 8);
    BMPFile.Position:=0;
    BMP.LoadFromStream(BMPFile);
    glClear(GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT or GL_COLOR_BUFFER_BIT);//GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    Self.DrawBitmap(BMP,0,0);
    (*img:=TGPBitmap.Create(TStreamAdapter.Create(BMPFile));
    try
      //FMediaDisplay.RenderBitMap(bmp);
     Graphics.DrawImage(Img,0,0,w,h);
     {$ifdef SHOW_MEDIA}
     Text:=FormatDateTime('hh.mm.ss.zzz',IncMilliSecond(MinDateTime,Ceil(FAVPacked.dts * av_q2d(FVideoStrem.time_base)*1000)))+' [FRAME ]';
     GPEasyTextout(Graphics, Text, MakeRect(1, 1, w*1.0, h*1.0), MakeColor(0, 255, 0), StringAlignmentNear, StringAlignmentNear, 15);
     Text:=FormatDateTime('hh.mm.ss.zzz',IncMilliSecond(MinDateTime,GT))+' [GLOBAL]';
     GPEasyTextout(Graphics, Text, MakeRect(1, 15, w*1.0, h*1.0), MakeColor(0, 255, 255), StringAlignmentNear, StringAlignmentNear, 15);
     //GPEasyTextout(Graphics, 'TVideoThread:'+IntToStr(Self.Tag), MakeRect(1, 20, w*1.0, h*1.0), MakeColor(0, 0, 255), StringAlignmentNear, StringAlignmentNear, 10);
     GPEasyTextout(Graphics, 'Media Component v2.0', MakeRect(1, 1, w*1.0, h*1.0), MakeColor(255, 0, 0), StringAlignmentCenter, StringAlignmentNear, 10);
     {$endif}
    finally
     FreeAndNil(Img);
    end;*)
  finally
    //FBitMap.Unlock;
    FreeAndNil(BMPFile);
    FreeAndNil(bmp);
    FreeAndNil(Graphics);
  end;
 finally
  if Assigned(CS) then CS.Leave;
 end;
end;

procedure TMediaDisplay.SetBitmap(BMP: TBitmap;GlobalTime,PackTime:Cardinal);
var Stream:TMemoryStream;
begin
  if not Assigned(RenderBMP) then Exit;
  CS.Enter;
  Self.GlobalTime:=GlobalTime;
  Self.PackTime:=PackTime;
  Stream:=TMemoryStream.Create;
  try
    BMP.SaveToStream(Stream);
    Stream.Position:=0;
    RenderBMP.FreeImage;
    RenderBMP.LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
    CS.Leave;
  end;
end;

procedure TMediaDisplay.setupPixelFormat(DC: HDC);
const
   pfd:TPIXELFORMATDESCRIPTOR = (
        nSize:sizeof(TPIXELFORMATDESCRIPTOR);	// size
        nVersion:1;			// version
        dwFlags:PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW or
                PFD_DOUBLEBUFFER;	// support double-buffering
        iPixelType:PFD_TYPE_RGBA;	// color type
        cColorBits:32;			// preferred color depth
        cRedBits:0; cRedShift:0;	// color bits (ignored)
        cGreenBits:0;  cGreenShift:0;
        cBlueBits:0; cBlueShift:0;
        cAlphaBits:0;  cAlphaShift:0;   // no alpha buffer
        cAccumBits: 0;
        cAccumRedBits: 0;  		// no accumulation buffer,
        cAccumGreenBits: 0;     	// accum bits (ignored)
        cAccumBlueBits: 0;
        cAccumAlphaBits: 0;
        cDepthBits:24;			// depth buffer
        cStencilBits:0;			// no stencil buffer
        cAuxBuffers:0;			// no auxiliary buffers
        iLayerType:PFD_MAIN_PLANE;  	// main layer
        bReserved: 0;
        dwLayerMask: 0;
        dwVisibleMask: 0;
        dwDamageMask: 0;                    // no layer, visible, damage masks
   );
var pixelFormat:integer;
begin
   pixelFormat := ChoosePixelFormat(DC, @pfd);
   if (pixelFormat = 0) then
        exit;
   if (SetPixelFormat(DC, pixelFormat, @pfd) <> TRUE) then
        exit;
end;

function TMediaDisplay.TextOut(const text: WideString; x,
  y: Integer; red, green, blue, alpha: Single): TMediaDisplay;
var
   i: Integer;
   GLList: GLuint;
   Font:TFont;
begin
 Font:=TFont.Create;
 try
  Font.Style:=[fsBold];
  Font.Size:=10;
  Font.Color:=clGreen;
  SelectObject(HDC, Font.Handle);
  GLList := glGenLists(MaxChar);
  wglUseFontBitmaps(HDC, 0, MaxChar, GLList);

  glPushMatrix;
  glLoadIdentity;
  glColor4f(red, green, blue, alpha);
  glRasterPos2i(x, y);
  for i := 1 to Length(text) do
    glCallList(GLList + Ord(text[i]));
  glPopMatrix;

  glDeleteLists(GLList, MaxChar);
  //glColor4fv(@FPenColor);
  Result := Self;
 finally
   FreeAndNil(Font);
 end;
end;

procedure TMediaDisplay.UpdateRender;
var  x,y:Integer;
  Text:AnsiString;
  Texture:Cardinal;
  FPS:Double;
  bmpInfo: BITMAP;
begin x:=0; y:=0; Texture:=0;
  //glColor3f(1.0,0.0,0.0);
  glClear (GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT or GL_COLOR_BUFFER_BIT); //Очистка буфера цвета и глубины

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  FPS:=Round(TTimeSpan.Subtract(GetTime,Self.CalcFPS).Milliseconds);
  Self.CalcFPS:=GetTime;
  TextOut('Media Display ['+FormatDateTime('hh.mm.ss.zzz',Time)+'] FPS:'+FloatToStr(FPS),-ClientWidth div 2 + 5,ClientHeight div 2 - 15,-128,11,0,0);
  if FDrawInfo then begin
    TextOut('['+FormatDateTime('hh.mm.ss.zzz',IncMilliSecond(MinDateTime,GlobalTime))+'] Global',-ClientWidth div 2 + 5,ClientHeight div 2 - 30,1,0,1,0);
    TextOut('['+FormatDateTime('hh.mm.ss.zzz',IncMilliSecond(MinDateTime,PackTime))+'] Packed',-ClientWidth div 2 + 5,ClientHeight div 2 - 45,1,1,0,0);
  end;
  if Assigned(RenderBMP) then begin
    BeginRender;
    try
     if not RenderBMP.Empty then begin
      (* Draw IMG > *)
      x:=-RenderBMP.Width div 2;
      y:=-RenderBMP.Height div 2;
      DrawBitmap(RenderBMP,x,y);
      {x:=RenderBMP.Width;
      y:=RenderBMP.Height;
      glColor3f(0.7,0.7,0.7);
      glEnable(GL_TEXTURE_2D);
      glDisable(GL_TEXTURE_2D);
      glGenTextures(1,@Texture);
      glBindTexture(GL_TEXTURE_2D,Texture);
      GetObject(RenderBMP.Handle, SizeOf(bmpInfo), @bmpInfo);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, x, y, 0,GL_RGBA, GL_UNSIGNED_BYTE, bmpInfo.bmBits);
      glBegin(GL_QUADS);
        glTexCoord2f(0, 0);
        glVertex3f(-x/2, -y/2, 0);
        glTexCoord2f(1, 0);
        glVertex3f(x/2, -y/2, 0);
        glTexCoord2f(1, 1);
        glVertex3f(x/2, y/2, 0);
        glTexCoord2f(0, 1);
        glVertex3f(-x/2, y/2, 0);
      glEnd;}
      //DrawBitmapTex(RenderBMP,-x div 2,-y div 2,x,y);
      //gluLookAt(0,0,-10,0,0,0,0,0,0);
      (* Draw IMG < *)
      (*BuildTexture(RenderBMP,Texture);
      DrawBitmapTex(Texture,-x div 2,-y div 2,x,y);
      DeleteTexture(Texture);*)
     end;
    finally
      EndRender;
    end;
  end;
  //SwapBuffers(wglGetCurrentDC);
  //glFlush;
  //Application.ProcessMessages;
  SwapBuffers(HDC);
end;

procedure TMediaDisplay.WMUpdateRender(var Message: TWMEraseBkgnd);
begin
  UpdateRender;
end;

end.
