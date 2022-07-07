unit uMediaReader;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  (* Media modules *)
  uMediaDisplay, uMediaConstant, uVideoThread, uVideoMain,
  Winapi.GDIPOBJ, Winapi.GDIPAPI, OpenGL,

  Vcl.ExtCtrls,Math,System.SyncObjs,System.Generics.Collections,

  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.DateUtils,
  libavcodec, libavdevice, libavfilter, libswresample, libswscale,
  libavutil, libavformat,
  sdl2, {SDL2_ttf,{sdl, {uResourcePaths,} System.Threading;

type

  TMediaReader = class (TThread)
  private
    FOnIsPlayed: TOnIsPlayed;
    FOnError: TOnError;
    FOnReadVideoPacked: TOnReadVideoPacked;
    FOnDecodeFrame: TOnDecodeVideo;
    FSyncTime: TOnSyncTime;
    FOnRenderVideoFrame: TOnRenderVideoFrame;
  protected
    FVideo:Array [0..0] of TVideoThread;
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

implementation

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
  VideoBuffer:TMediaBuffer;
  GlobalCS:TCriticalSection;
  BRepeat:Boolean;
  VideoEvent:TEvent;
begin
  NameThreadForDebugging('TMediaReader');
  v:=0;
  VideoBuffer:=TMediaBuffer.Create; //VideoBuffer
  VideoEvent:=TEvent.Create(nil,True,False,'');
  GlobalCS:=TCriticalSection.Create;
  for I := 0 to High(FVideo) do begin
   if not Assigned(FVideo[i]) then FVideo[i]:=TVideoThread.Create(FMediaDecoder.GetVideoStrem,VideoEvent,GlobalCS);
   FVideo[i].SetSDL(FMediaDecoder.Display.GetSDLInfo);
   FVideo[i].FBuffer:=VideoBuffer;
   FVideo[i].SetBitmap(FMediaDecoder.Display.GetCanvas);
   FVideo[i].SetMediaDisplay(FMediaDecoder.Display);
   FVideo[i].OnDecodeFrame:=FOnDecodeFrame;
   //FVideo[i].OnRenderVideoFrame:=FOnRenderVideoFrame;
   FVideo[i].OnReadVideoPacked:=FOnReadVideoPacked;
   FVideo[i].OnIsPlayed:=FOnIsPlayed;
   FVideo[i].OnSyncTime:=FSyncTime;
   FVideo[i].tag := i;
   repeat
     Sleep(1);
   until (Terminated) or (FVideo[i].FBuffer <> nil);
  end;
  VideoEvent.SetEvent;
  try
   repeat
    Sleep(1);
    //if Assigned(Application) then Application.ProcessMessages;
    try
    if (Assigned(FOnIsPlayed))and(Assigned(FMediaDecoder)) then begin
      if Assigned(FOnIsPlayed) then FOnIsPlayed(self,Play) else Play:=False;
      if Play then begin
        //if FMediaDecoder.FVideoBuffer.GetSize < High(FMediaDecoder.FVideoBuffer.FBuffer)-1 then begin
          if FMediaDecoder.ReadPacked(Pack) = 0 then begin
            if pack.stream_index = FMediaDecoder.VideoIdx then begin
              repeat
               Sleep(1);
               //Application.ProcessMessages;
               if not Assigned(FVideo[v]) then Break;
               if (Terminated) or (not Play) then Break;
               if (FVideo[v].Suspended) then FVideo[v].Start;
               GlobalCS.Enter;
               try
                BRepeat := (FVideo[v].FBuffer.GetCount < FVideo[v].FBuffer.GetSize);
               finally
                 GlobalCS.Leave;
               end;
              until BRepeat;
              GlobalCS.Enter;
              try
               if Assigned(FVideo[v]) then FVideo[v].FBuffer.WriteData(Pointer(pack));
              finally
               GlobalCS.Leave;
              end;
              Inc(V);
              if v > High(FVideo) then v:=0;
              //FMediaDecoder.FVideoBuffer.WriteData(Pointer(pack));
              pack:=nil;
            end else begin
              av_packet_unref(pack);
              av_packet_free(pack);
              pack:=nil;
            end;
          end else begin
            Break;
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
      try
       if FVideo[i] <> nil then begin
        FVideo[i].Terminate;
        //FVideo[i].WaitFor;
        //FVideo[i].Destroy;
       end;
      except
        FVideo[i]:=nil;
      end;
      FreeAndNil(FVideo[i]);
    end;
  end;
end;


end.
