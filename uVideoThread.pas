unit uVideoThread;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  (* Media modules *)
  uMediaDisplay, uMediaConstant, //uMediaReader,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, OpenGL,

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
  TVideoThread = class(TThread)
  private
    FSyncTime: TOnSyncTime;
    FOnDecodeFrame: TOnDecodeVideo;
    FOnReadVideoPacked: TOnReadVideoPacked;
    FOnRenderVideoFrame: TOnRenderVideoFrame;
    FOnIsPlayed: TOnIsPlayed;
    FOnError: TOnError;
    (* SDL > *)
    FSDLPantalla: PSDLPantalla;
    MooseTexture: PSDL_Texture;
    (* SDL < *)
    CS:TCriticalSection;
  protected
    GT:UInt64;
    FEvent:TEvent;
    FVideoStrem:PAVStream;
    FAVPacked:PAVPacket;
    FAVFrame:PAVFrame;
    FGotFrame:PInteger;
    FBitMap:TCanvas;
    FMediaDisplay:TMediaDisplay;
    procedure FreeBuff (Sender:TObject; var Item:Pointer);
    procedure Execute; override;
    Function Ini3DCanvas(Rect:TSDL_Rect):Boolean;
    Function Free3DCanvas:Boolean;
    Function CaslcSize(Sourse:TSDL_Rect):TSDL_Rect;
    Function SaveFrameAsJPEG(w,h:SInt32;Data:Array of PByte;linesize: Array of Integer):Integer;
    procedure RenderVideoFrame(w,h:SInt32;Data:Array of PByte;linesize: Array of Integer; pix_fmt:TAVPixelFormat);
    Procedure GPEasyTextout(Graphics: TGPGraphics; Const TheText: String; Rect: TGPRectF; Color: TGPColor; HAlign, VAlign: TStringAlignment; Size: Integer = 10; FontName: String = 'Arial');
  public
    FBuffer:TMediaBuffer;
    Tag:Integer;
    constructor Create(FVideoStrem:PAVStream;Event:TEvent;CS:TCriticalSection = nil);
    Procedure SetSDL(SDLPantalla: PSDLPantalla);
    Procedure SetBitmap(BitMap:TCanvas);
    Procedure SetMediaDisplay(Display:TMediaDisplay);
    Property OnSyncTime:TOnSyncTime read FSyncTime Write FSyncTime;
    Property OnDecodeFrame:TOnDecodeVideo read FOnDecodeFrame Write FOnDecodeFrame;
    property OnReadVideoPacked:TOnReadVideoPacked read FOnReadVideoPacked Write FOnReadVideoPacked;
    property OnRenderVideoFrame:TOnRenderVideoFrame read FOnRenderVideoFrame Write FOnRenderVideoFrame;
    property OnIsPlayed:TOnIsPlayed read FOnIsPlayed write FOnIsPlayed;
    property OnError:TOnError Read FOnError Write FOnError;
  end;

implementation

{ 
  Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);  

  and UpdateCaption could look like,

    procedure TMediaRender.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; 
    
    or 
    
    Synchronize( 
      procedure 
      begin
        Form1.Caption := 'Updated in thread via an anonymous method' 
      end
      )
    );
    
  where an anonymous method is passed.
  
  Similarly, the developer can call the Queue method with similar parameters as 
  above, instead passing another TThread class as the first parameter, putting
  the calling thread in a queue with the other thread.
    
}

{ TVideoThread }

function TVideoThread.CaslcSize(Sourse: TSDL_Rect): TSDL_Rect;
var rect:TSDL_Rect;
    pw,ph,p:Double;
begin
  //if not Assigned(FSDLPantalla.Renderer) then Exit;
  //SDL_RenderGetViewport(FSDLPantalla.Renderer,@rect);
  //WinRect:=GetClientRect;
  Result.x := 0;
  Result.y := 0;
  Result.w := Sourse.w;
  Result.h := Sourse.h;
  //p := (WinRect.Width * 100) / Result.w;
  p := (FMediaDisplay.Width * 100) / Result.w;
  //p := (rect.w * 100) / Result.w;
  Result.w := round((Result.w * p) / 100);
  Result.h := round((Result.h * p) / 100);
  if Result.h > FMediaDisplay.Height then begin
    p := (FMediaDisplay.Height * 100) / Result.h;
    //p := (rect.h * 100) / Result.h;
    Result.w := round((Result.w * p) / 100);
    Result.h := round((Result.h * p) / 100);
  end;
end;

constructor TVideoThread.Create(FVideoStrem: PAVStream; Event:TEvent; CS:TCriticalSection);
begin
  inherited Create(False);
  Self.FVideoStrem:=FVideoStrem;
  FEvent:=Event;
  if CS <> nil then Self.CS:=CS;
end;

procedure TVideoThread.Execute;
var Play:Boolean;
  Decoded,Render:Boolean;
  tic1, tic2:UInt64;
  Delay,Delay2:Int64;
  Last_Delay:Int64;
  ErrorCount:Int64;

  function isTimePak:Boolean;
  var tmp:Int64;
  begin  Result:=False;
   if GT = 0 then begin
     Result:=True;
     exit;
   end;
   if FAVPacked = nil then exit;
   if FAVPacked.pts <> AV_NOPTS_VALUE then
    tmp:=Round((FAVPacked.pts * (FVideoStrem.time_base.num / FVideoStrem.time_base.den)) * 1000)
   else begin
     if FAVPacked.dts <> AV_NOPTS_VALUE then
      tmp:=Round((FAVPacked.dts * (FVideoStrem.time_base.num / FVideoStrem.time_base.den)) * 1000)
     else tmp:=0;
   end;
   Result:= GT >= tmp
  end;

  function isTimeFail:boolean;
  var tmp:Int64;tmp1:Int64;
    tic1, tic2:Int64;
  begin Result:=True;
    tic1:=GetTickCount;
   if GT = 0 then begin
     Result:=True;
     exit;
   end;
   if FAVPacked.dts <> AV_NOPTS_VALUE then
    tmp:=Round((FAVPacked.dts * (FVideoStrem.time_base.num / FVideoStrem.time_base.den)) * 1000)
   else tmp:=0;
   if FAVPacked.duration <> AV_NOPTS_VALUE then
    tmp1:=Round((FAVPacked.duration * (FVideoStrem.time_base.num / FVideoStrem.time_base.den)) * 1000)
   else tmp1:=0;
   tic2:=GetTickCount;
   Result:= GT - tmp > tmp1 {500};//((GT div 1000)+tmp1-(tic2 - tic1)) >= tmp
  end;

begin
  NameThreadForDebugging('TVideoThread:'+IntToStr(Self.Tag));
  {$Ifdef DEBUG}
  OutputDebugString(PChar('TVideoThread:'+IntToStr(Self.Tag)+' START'));
  {$endif}
  if Self.FBuffer = nil then begin
   Self.FBuffer:=TMediaBuffer.Create(1000);
  end;
  Self.FBuffer.OnFlushBuffer:=FreeBuff;
  //FAVPacked:=av_packet_alloc;
  FAVFrame:=av_frame_alloc;
  New(FGotFrame);
  Delay2:=0;
  Decoded:=False;
  Render:=False;
  ErrorCount:=0;
  try
    while not Self.Terminated do begin
     FEvent.WaitFor(INFINITE);
     FEvent.ResetEvent;
     try
      if Assigned(FOnIsPlayed) and Assigned(FSyncTime) then
      begin
       if Assigned(FOnIsPlayed) then FOnIsPlayed(Self,Play);
		   if Play then begin tic1:=GetTickCount;
          try
           repeat Sleep(1);
            if Assigned(FOnIsPlayed) then FOnIsPlayed(Self,Play);
            CS.Enter;
            try
             FBuffer.ReadData(Pointer(FAVPacked));
            finally
              CS.Leave;
            end;
           until (FAVPacked <> nil)or(Terminated)or(not Play);
           if FAVPacked <> nil then begin
            if FAVPacked.stream_index <> Self.FVideoStrem.index then begin
             try av_packet_free(@FAVPacked);
             finally FAVPacked:=nil;
             end;
             Continue;
            end;
           end;
           if not Play then Break;
          except
           av_frame_unref(FAVFrame);
           av_packet_unref(FAVPacked);
           av_packet_free(@FAVPacked);
           Continue;
          end;
          //Render := avcodec_decode_video2(FVideoStrem^.codec, FAVFrame, FGotFrame, FAVPacked) > 0;
          (*if not isTimeFail then Render := FOnDecodeFrame(self,FAVPacked,FAVFrame,FGotFrame) > 0
          else Render:=False;*)
          Render := FOnDecodeFrame(self,FAVPacked,FAVFrame,FGotFrame) > 0;
			(*IF Assigned(CS) then CS.Enter;
			try
				//Render := FOnDecodeFrame(self,FAVPacked,FAVFrame,FGotFrame) > 0;
				Render := avcodec_decode_video2(FVideoStrem^.codec, FAVFrame, FGotFrame, FAVPacked) > 0;
			finally
				If Assigned(CS) then CS.Leave;
			end;*)
          While (not Terminated) do begin
            if Assigned(FOnIsPlayed) then FOnIsPlayed(Self,Play);
            if not Play then Break;
            if Assigned(FSyncTime) then FSyncTime(Self,GT,FAVPacked,Delay2);
            if isTimePak then Break;
            //if isTimeFail then Break;
            sleep(1);
          end;
          FEvent.SetEvent;
          tic2:=GetTickCount;
          Delay2:=Delay2-(tic2-tic1);
          if Delay2 < 0 then Delay2:=1;
          //Sleep(Delay2);
          //Sleep(1);
          Decoded:=False;
          if Render then begin
            try
              //if not isTimeFail then
              RenderVideoFrame(
                  FVideoStrem.codec.width,
                  FVideoStrem.codec.height,
                  FAVFrame.data,
                  FAVFrame.linesize,
                  FVideoStrem.codec.pix_fmt
              );
            finally

            end;
            Decoded:=False; Render:=False;
          end;
          if FAVFrame <> nil then av_frame_unref(FAVFrame);
          if FAVPacked <> nil then av_packet_unref(FAVPacked);
          av_packet_free(@FAVPacked);
        end;
      end;
     except
      on E:Exception Do begin
       if Assigned(FOnError) then FOnError(self,-1,'ERROR [TVideoThread:'+IntToStr(Self.Tag)+'] '+e.Message);
       {$Ifdef DEBUG}
       OutputDebugString(PWideChar('ERROR [TVideoThread:'+IntToStr(Self.Tag)+'] '+e.Message));
       {$endif}
       if Assigned(FAVFrame) then av_frame_unref(FAVFrame);
       if Assigned(FAVPacked) then av_packet_unref(FAVPacked);
       if Assigned(FAVPacked) then av_packet_free(@FAVPacked);
	     if Errorcount > 10 then Break;
	     Inc(ErrorCount);
      end;
     end;
    end;
  finally
    FEvent.SetEvent;
    if Self.FBuffer <> nil then begin
     while Self.FBuffer.GetCount > 0 do begin
       FBuffer.ReadData(Pointer(FAVPacked));
       if Assigned(FAVPacked) then av_packet_unref(FAVPacked);
       if Assigned(FAVPacked) then av_packet_free(@FAVPacked);
     end;
     Self.FBuffer.FlushBuffer;
     FreeAndNil(Self.FBuffer);
    end;
    {$Ifdef DEBUG}
    OutputDebugString(PChar('TVideoThread:'+IntToStr(Self.Tag)+' END'));
    {$endif}
    if Assigned(FAVFrame) then av_frame_free(@FAVFrame);
    if Assigned(FAVPacked) then av_packet_free(@FAVPacked);
    Dispose(FGotFrame);
  end;
end;

function TVideoThread.Free3DCanvas: Boolean;
begin Result:=True;
 try
  if assigned(MooseTexture) then begin
    SDL_DestroyTexture(MooseTexture);
    MooseTexture:=nil;
  end;
  (*if assigned(FSDLPantalla.Window.surface) then begin
    SDL_FreeSurface(FSDLPantalla.Window.surface);
    FSDLPantalla.Window.surface:=nil;
  end;*)
 except
  Result:=False;
 end;
end;

procedure TVideoThread.FreeBuff(Sender: TObject; var Item: Pointer);
//var AVPacked:PAVPacket;
begin
  if Assigned(PAVPacket(Item)) then av_packet_unref(PAVPacket(Item));
  if Assigned(PAVPacket(Item)) then av_packet_free(@PAVPacket(Item));
end;

procedure TVideoThread.GPEasyTextout(Graphics: TGPGraphics;
  const TheText: String; Rect: TGPRectF; Color: TGPColor; HAlign,
  VAlign: TStringAlignment; Size: Integer; FontName: String);
var
  StringFormat: TGPStringFormat;
  FontFamily: TGPFontFamily;
  Font: TGPFont;
  Pen: TGPPen;
  Brush: TGPSolidBrush;
begin
  StringFormat := TGPStringFormat.Create;
  FontFamily := TGPFontFamily.Create(FontName);
  Font := TGPFont.Create(FontFamily, Size, FontStyleRegular, UnitPixel);
  Pen := TGPPen.Create(Color);
  Brush := TGPSolidBrush.Create(Color);
  StringFormat.SetAlignment(HAlign);
  StringFormat.SetLineAlignment(VAlign);
  Graphics.DrawString(TheText, -1, Font, Rect, StringFormat, Brush);
  Pen.Free;
  Brush.Free;
  StringFormat.Free;
  FontFamily.Free;
  Font.Free;
end;

function TVideoThread.Ini3DCanvas(Rect: TSDL_Rect): Boolean;
{var
 rect2:TSDL_Rect;}
begin  Result:=True;
 try
  //rect2:=rect;
  //Free3DCanvas;
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
      1,//SDL_TEXTUREACCESS_STREAMING,
      rect.w,rect.h);
    if MooseTexture = nil then
     raise Exception.Create('CreateTextureFromSurface failed:'+SDL_GetError());
  end;
 except
   Result:=False;
 end;
end;

procedure TVideoThread.RenderVideoFrame(w, h: SInt32; Data: array of PByte;
  linesize: array of Integer; pix_fmt: TAVPixelFormat);
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
 //if Assigned(CS) then CS.Enter;
 //if not Assigned(FSDLPantalla.Window) then Exit;
 try
  pix_F := AV_PIX_FMT_BGR0;{WORK}
  // Задаём размеры прямоугольника в дальнейшем по мену будем считать преобразование размера кадра
  rect.x := 0; rect.y := 0;
  //MyRect:=GetClientRect;
  rect.w := w; // шырена
  rect.h := h; // высота
  rect2:=CaslcSize(rect);
  //Ini3DCanvas(rect2{rect});
  // проверяем была ли инициализация
  (*if {(not assigned(FSDLPantalla.Window.surface)) OR }(not assigned(MooseTexture)) then begin
    Exit;
  end;*)
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
    //Ini3DCanvas(rect2);
    if FAVFrame.pict_type <> AV_PICTURE_TYPE_NONE then begin
     //Self.FMediaDisplay.Render(rect2.w, rect2.H,Img.Data[0], Img.linesize[0]);
     SaveFrameAsJPEG(rect2.w, rect2.H,Img.Data[0], Img.linesize[0]);
     (*Ini3DCanvas(rect2);
     try
     res := SDL_UpdateTexture(MooseTexture, @rect2, @Img.Data[0]^, Img.linesize[0]);
     if res = 0 then begin
       res := SDL_RenderCopy(FSDLPantalla^.Renderer, MooseTexture,
         @rect,// С какой области скопировать кадр
         nil   // На какой размер растянуть кадр
       );
     end;
     finally
       Free3DCanvas;
     end;*)
    end;
    //res := SDL_UpdateTexture(MooseTexture, @rect2, @Img.Data[0]^, Img.linesize[0]);
    //if res = 0 then begin
    //SDL_RenderClear(FSDLPantalla^.Renderer);
    // копируем картинку с текстуры в рендер
    //if (*FProportionally*) true then begin
    //TicB:=GetTickCount;
      (*res := SDL_RenderCopy(FSDLPantalla^.Renderer, MooseTexture,
        nil,// С какой области скопировать кадр
        nil//@rect // На какой размер растянуть кадр
        );*)
    //TicE:=GetTickCount;
    //OutputDebugString(PWideChar('SDL_RenderCopy ms:'+IntToStr(TicE - TicB)));
    //end;
    {if res <> 0 then
      raise Exception.Create('SDL Error Message : '+SDL_GetError());}
    (*end else begin
      res := SDL_RenderCopy(FSDLPantalla^.Renderer, MooseTexture,
        @rect,// С какой области скопировать кадр
        nil   // На какой размер растянуть кадр
        );
    end;*)
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
  //Application.ProcessMessages;
  //SDL_RenderPresent(FSDLPantalla^.Renderer);
  //if Assigned(CS) then CS.Leave;
 end;
end;

function TVideoThread.SaveFrameAsJPEG(w,h:SInt32;Data:Array of PByte;linesize: Array of Integer): Integer;
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
  res: Integer;

  LineByte:TArray<TArray<Byte>>;
  I: Integer;

begin Result:=0;
  if not Assigned(FBitMap) then Exit;
  bmpheader.bfReserved1 := 0;
  bmpheader.bfReserved2 := 0;
  bmpheader.bfType := $4d42;
  bmpheader.bfOffBits := sizeof(BITMAPFILEHEADER) + sizeof(BITMAPV4HEADER);
  bmpheader.bfSize := bmpheader.bfOffBits + w*h*32 div 8;

  bmpinfo.bV4Size := sizeof(BITMAPV4HEADER);
  bmpinfo.bV4Width := w;
  bmpinfo.bV4Height := h;
  bmpinfo.bV4Planes := 1;
  bmpinfo.bV4BitCount := 32;//24;
  bmpinfo.bV4V4Compression := BI_BITFIELDS;
  bmpinfo.bV4SizeImage := 0;
  bmpinfo.bV4XPelsPerMeter := 2835; // ResolutionHorizontal
  bmpinfo.bV4YPelsPerMeter := 2835; //ResolutionVertical
  bmpinfo.bV4ClrUsed := 0;
  bmpinfo.bV4ClrImportant := 0;
  BMPInfo.bV4RedMask:=  $00FF0000;{WORK}
  BMPInfo.bV4GreenMask:=$0000FF00;
  BMPInfo.bV4BlueMask:= $000000FF;{WORK}
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
  //FMediaDisplay.BeginRender;
  try
    BMPFile.WriteBuffer(bmpheader,SizeOf(bmpheader));
    BMPFile.WriteBuffer(bmpinfo,SizeOf(bmpinfo));
    SetLength(LineByte,h);
    for I := 0 to h-1 do begin
      SetLength(LineByte[i],w*32 div 8);
      CopyMemory(LineByte[i],data[0]+((w*32 div 8)*i),w*32 div 8);
    end;
    for I := High(LineByte) downto 0 do begin
      BMPFile.WriteData(LineByte[i],w*32 div 8);
    end;
    //BMPFile.WriteBuffer(data[0]^,w*h*32 div 8);
    BMPFile.Position:=0;
    BMP.LoadFromStream(BMPFile);
    (* OpenGL > *)
    //glTranslatef(0.0,0.0,-10.0);
    //glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    //wglMakeCurrent(FMediaDisplay.HDC, FMediaDisplay.RC);
    //glClear(GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT or GL_COLOR_BUFFER_BIT);//GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    FMediaDisplay.SetBitmap(bmp,GT,Round((FAVPacked.dts * (FVideoStrem.time_base.num / FVideoStrem.time_base.den)) * 1000));
    //SwapBuffers(Self.FMediaDisplay.HDC);
    (* OpenGL < *)
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
    //FMediaDisplay.EndRender;
    FreeAndNil(BMPFile);
    FreeAndNil(bmp);
    FreeAndNil(Graphics);
  end;
end;

procedure TVideoThread.SetBitmap(BitMap: TCanvas);
begin
  FBitMap:=BitMap;
end;

procedure TVideoThread.SetMediaDisplay(Display: TMediaDisplay);
begin
  FMediaDisplay:=Display;
end;

procedure TVideoThread.SetSDL(SDLPantalla: PSDLPantalla);
begin
  FSDLPantalla:=SDLPantalla;
end;

end.
