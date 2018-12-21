unit uVideoThread;

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
  protected
    FVideoStrem:PAVStream;
    FAVPacked:PAVPacket;
    FAVFrame:PAVFrame;
    FGotFrame:PInteger;
    procedure Execute; override;
    Function Ini3DCanvas(Rect:TSDL_Rect):Boolean;
    Function Free3DCanvas:Boolean;
    Function CaslcSize(Sourse:TSDL_Rect):TSDL_Rect;
    Function SaveFrameAsJPEG(w,h:SInt32;Data:Array of PByte;linesize: Array of Integer):Integer;
    procedure RenderVideoFrame(w,h:SInt32;Data:Array of PByte;linesize: Array of Integer; pix_fmt:TAVPixelFormat);
  public
    FBuffer:TMediaBuffer;
    Tag:Integer;
    constructor Create(FVideoStrem:PAVStream);
    Procedure SetSDL(SDLPantalla: PSDLPantalla);
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
  if not Assigned(FSDLPantalla.Renderer) then Exit;
  SDL_RenderGetViewport(FSDLPantalla.Renderer,@rect);
  //WinRect:=GetClientRect;
  Result.x := 0;
  Result.y := 0;
  Result.w := Sourse.w;
  Result.h := Sourse.h;
  //p := (WinRect.Width * 100) / Result.w;
  p := (rect.w * 100) / Result.w;
  Result.w := round((Result.w * p) / 100);
  Result.h := round((Result.h * p) / 100);
  if Result.h > rect.h then begin
    p := (rect.h * 100) / Result.h;
    Result.w := round((Result.w * p) / 100);
    Result.h := round((Result.h * p) / 100);
  end;
end;

constructor TVideoThread.Create(FVideoStrem: PAVStream);
begin
  inherited Create(False);
  Self.FVideoStrem:=FVideoStrem;
end;

procedure TVideoThread.Execute;
var Play:Boolean;
  GT:UInt64;
  Decoded,Render:Boolean;
  tic1, tic2:UInt64;
  Delay,Delay2:Int64;
  Last_Delay:Int64;

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
   else tmp:=0;
   Result:= GT >= tmp
  end;

  function isTimeFrame:boolean;
  var tmp:Int64;
  begin
   if GT = 0 then begin
     Result:=True;
     exit;
   end;
   if FAVPacked.dts <> AV_NOPTS_VALUE then
    tmp:=Round((FAVPacked.dts * (FVideoStrem.time_base.num / FVideoStrem.time_base.den)) * 1000)
   else tmp:=0;
   Result:= (GT div 1000) >= tmp
  end;

begin
  NameThreadForDebugging('TVideoThread:'+IntToStr(Self.Tag));
  {$Ifdef DEBUG}
  OutputDebugString(PChar('TVideoThread:'+IntToStr(Self.Tag)+' START'));
  {$endif}
  Self.FBuffer:=TMediaBuffer.Create(1000);
  //FAVPacked:=av_packet_alloc;
  FAVFrame:=av_frame_alloc;
  New(FGotFrame);
  Delay2:=0;
  Decoded:=False;
  Render:=False;
  try
   try
    while not Self.Terminated do begin
      //Sleep(1);
      if Assigned(FOnIsPlayed) and
         Assigned(FSyncTime) and
         //Assigned(FOnReadVideoPacked) and
         //Assigned(FOnDecodeFrame) and
         Assigned(FOnRenderVideoFrame)
      then
      begin
        if Assigned(FOnIsPlayed) then FOnIsPlayed(Self,Play);
        if Play then begin
          //(FAVPacked.stream_index) <> FVideoStrem.index
          tic1:=GetTickCount;
          try
           repeat
            Sleep(1);
            if Assigned(FOnIsPlayed) then FOnIsPlayed(Self,Play);
            FBuffer.ReadData(Pointer(FAVPacked));
            //FOnReadVideoPacked(Self,Pointer(FAVPacked));
           until (FAVPacked <> nil)or(Terminated)or(not Play);
           if FAVPacked.stream_index <> Self.FVideoStrem.index then begin
             try av_packet_free(@FAVPacked);
             finally FAVPacked:=nil;
             end;
             Continue;
           end;
           if not Play then Break;
          except
           av_frame_unref(FAVFrame);
           av_packet_unref(FAVPacked);
           av_packet_free(@FAVPacked);
           Continue;
          end;
          //Render := FOnDecodeFrame(self,FAVPacked,FAVFrame,FGotFrame) > 0;
          Render := avcodec_decode_video2(FVideoStrem^.codec, FAVFrame, FGotFrame, FAVPacked) > 0;
          While (not Terminated) do begin
            //Sleep(1);
            //Application.ProcessMessages;
            if Assigned(FSyncTime) then FSyncTime(Self,GT,FAVPacked,Delay2);
            if isTimePak then Break;
            if Assigned(FOnIsPlayed) then FOnIsPlayed(Self,Play);
            if not Play then Break;
          end;
          //if isTimePak then begin
          Decoded:=False;
          if Render then begin
            try
              RenderVideoFrame(
                  FVideoStrem.codec.width,
                  FVideoStrem.codec.height,
                  FAVFrame.data,
                  FAVFrame.linesize,
                  FVideoStrem.codec.pix_fmt
              );
              (*FOnRenderVideoFrame(
                  FVideoStrem.codec.width,
                  FVideoStrem.codec.height,
                  FAVFrame.data,
                  FAVFrame.linesize,
                  FVideoStrem.codec.pix_fmt
              );*)
            finally

            end;
            Decoded:=False; Render:=False;
          end;
          tic2:=GetTickCount;
          //FAVFrame.repeat_pict
          //Delay:=round((FAVPacked.duration * av_q2d(FVideoStrem.time_base)* 1000))-(tic2-tic1);//+Self.Tag;
          (* Test Calc Delay *)
          //Delay:=round(((FAVPacked.duration * (FVideoStrem.time_base.num / FVideoStrem.time_base.den))* 1000))-(tic2-tic1);//+Self.Tag;
          //Delay:=((FAVPacked.pts - Last_Delay))-(tic2-tic1);
          //Last_Delay:=FAVPacked.pts;
          //if Delay < 0 then Delay := 0;
          Delay2:=Delay2-(tic2-tic1);
          if Delay2 < 0 then Delay2:=0;

          {if FAVPacked.dts > 0 then
            Delay2:=Round((FAVPacked.dts * (FVideoStrem.time_base.num / FVideoStrem.time_base.den)) * 1000);}
          av_frame_unref(FAVFrame);
          av_packet_unref(FAVPacked);
          av_packet_free(@FAVPacked);
          //Sleep(Delay);
          Sleep(Delay2);
          //end; //is time end
          //Sleep(3);
        end;
      end;
    end;
   except
     on E:Exception Do begin
       if Assigned(FOnError) then FOnError(self,-1,'ERROR [TVideoThread:'+IntToStr(Self.Tag)+'] '+e.Message);
       {$Ifdef DEBUG}
       OutputDebugString(PWideChar('ERROR [TVideoThread:'+IntToStr(Self.Tag)+'] '+e.Message));
       {$endif}
     end;
   end;
  finally
    Free3DCanvas;
    FreeAndNil(Self.FBuffer);
    {$Ifdef DEBUG}
    OutputDebugString(PChar('TVideoThread:'+IntToStr(Self.Tag)+' END'));
    {$endif}
    av_frame_free(@FAVFrame);
    av_packet_free(@FAVFrame);
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

function TVideoThread.Ini3DCanvas(Rect: TSDL_Rect): Boolean;
var
 rect2:TSDL_Rect;
begin  Result:=True;
 try
  rect2:=rect;
  Free3DCanvas;
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
      rect2.w,rect2.h);
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
 if not Assigned(FSDLPantalla.Window) then Exit;
 try
  pix_F := AV_PIX_FMT_BGR0;
  // Задаём размеры прямоугольника в дальнейшем по мену будем считать преобразование размера кадра
  rect.x := 0; rect.y := 0;
  //MyRect:=GetClientRect;
  rect.w := w; // шырена
  rect.h := h; // высота
  rect2:=CaslcSize(rect);
  Ini3DCanvas(rect2{rect});
  // проверяем была ли инициализация
  if {(not assigned(FSDLPantalla.Window.surface)) OR }(not assigned(MooseTexture)) then begin
    Exit;
  end;
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
     SaveFrameAsJPEG(rect2.w, rect2.H,Img.Data[0], Img.linesize[0]);
    end;
    res := SDL_UpdateTexture(MooseTexture, @rect2, @Img.Data[0]^, Img.linesize[0]);
    if res = 0 then begin
    //SDL_RenderClear(FSDLPantalla^.Renderer);
    // копируем картинку с текстуры в рендер
    //if (*FProportionally*) true then begin
    //TicB:=GetTickCount;
      res := SDL_RenderCopy(FSDLPantalla^.Renderer, MooseTexture,
        nil,// С какой области скопировать кадр
        nil//@rect // На какой размер растянуть кадр
        );
    //TicE:=GetTickCount;
    //OutputDebugString(PWideChar('SDL_RenderCopy ms:'+IntToStr(TicE - TicB)));
    end;
    if res <> 0 then
      raise Exception.Create('SDL Error Message : '+SDL_GetError());
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
  SDL_RenderPresent(FSDLPantalla^.Renderer);
  //if Assigned(CS) then CS.Leave;
 end;
end;

function TVideoThread.SaveFrameAsJPEG(w,h:SInt32;Data:Array of PByte;linesize: Array of Integer): Integer;
var
  BMPFile:TFileStream;
  BMPHeader:BITMAPFILEHEADER;
  BMPInfo:TBitmapV4Header;
  ret:Integer;
begin Result:=0;
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

  BMPFile:=TFileStream.Create(ExtractFilePath(GetCurrentDir)+'TEST'+IntToStr(Random(500))+'.jpg',fmCreate);
  try
    BMPFile.WriteBuffer(bmpheader,SizeOf(bmpheader));
    BMPFile.WriteBuffer(bmpinfo,SizeOf(bmpinfo));
    BMPFile.WriteBuffer(data[0]^,w*h*32 div 8);
  finally
    FreeAndNil(BMPFile);
  end;
end;

procedure TVideoThread.SetSDL(SDLPantalla: PSDLPantalla);
begin
  FSDLPantalla:=SDLPantalla;
end;

end.
