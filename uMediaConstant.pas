unit uMediaConstant;

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

  TOnSyncTime = procedure (Sender:TObject;var GlobalTime:UInt64; Pack:PAVPacket; var Delay:Int64) of object;
  TOnDecodeVideo = function (Sender:TObject;var Pack:PAVPacket; var Frame:PAVFrame; Var Gotframe:PInteger):Integer of object;
  TOnReadPacked = Function (Sender:TObject;var Pack:PAVPacket):Integer of object;
  TOnReadVideoPacked = Function (Sender:TObject;var Pack:Pointer):Integer of object;
  //TOnReadAudioPacked = Function (Sender:TObject;var Pack:Pointer):Integer of object;
  TOnRenderVideoFrame = procedure (w,h:SInt32;Data:Array of PByte;linesize: Array of Integer; pix_fmt:TAVPixelFormat) of object;
  TOnIsPlayed = Procedure (Sender:TObject;var Play:Boolean)of object;

  TMediaBufferInfo = packed Record
    AVStream  : PAVStream;
    AVPacket  : PAVPacket;//TAVPacket;
    AVFrame   : PAVFrame;//TAVFrame;
    GotFrame  : PInteger;
  End;
  PMediaBufferInfo = ^TMediaBufferInfo;

  TMediaBuffer = class
  private
    CS:TCriticalSection;
    FLastReadItem, FLastWriteItem:Cardinal;
  protected
    FBuffer:TArray<Pointer>;
  public
    constructor Create(Size:Cardinal = 500);
    destructor Destroy;
    Function WriteData(var Item:Pointer):Cardinal;
    Function ReadData(var Item:Pointer):Cardinal;
    Function GetCount:Cardinal;
    Function GetSize:Cardinal;
    Property LastReadItem:Cardinal read FLastReadItem;
    property LastWriteItem:Cardinal read FLastWriteItem;
  end;

implementation

{ TMediaBuffer }

constructor TMediaBuffer.Create(Size: Cardinal);
begin
  if Size = 0 then begin
    raise Exception.Create('Размер не может быть нулевым');
  end;
  CS:=TCriticalSection.Create;
  SetLength(FBuffer,Size);
  FLastReadItem:=0;
  FLastWriteItem:=0;
end;

destructor TMediaBuffer.Destroy;
begin
  FreeAndNil(CS);
end;

function TMediaBuffer.GetCount: Cardinal;
begin
  //if Assigned(CS) then CS.Enter;
  try
   if FLastWriteItem >= FLastReadItem then begin
    Result:=FLastWriteItem - FLastReadItem;
   end else begin
    Result:=(High(FBuffer)-FLastReadItem)+FLastWriteItem;
   end;
  finally
    //if Assigned(CS) then CS.Leave;
  end;
end;

function TMediaBuffer.GetSize: Cardinal;
begin
 //if Assigned(CS) then CS.Enter;
 try
  Result:=High(FBuffer);
 finally
  //if Assigned(CS) then CS.Leave;
 end;
end;

function TMediaBuffer.ReadData(var Item: Pointer): Cardinal;
begin
  if Assigned(CS) then CS.Enter;
  try
   Result:=FLastReadItem;
   Item:=FBuffer[FLastReadItem];
   if Item = nil then Exit;
   FBuffer[FLastReadItem]:=nil;
   inc(FLastReadItem);
   if FLastReadItem > High(FBuffer) then FLastReadItem:=Low(FBuffer);
  finally
    if Assigned(CS) then CS.Leave;
  end;
end;

function TMediaBuffer.WriteData(var Item: Pointer): Cardinal;
begin
  if Assigned(CS) then CS.Enter;
  try
   Result:=FLastWriteItem;
   if FBuffer[FLastWriteItem] <> nil then begin
     Result:=FLastWriteItem;
     raise Exception.Create('Error write Buffer is not Free');
   end;
   FBuffer[FLastWriteItem]:=Item;
   inc(FLastWriteItem);
   if FLastWriteItem > High(FBuffer) then FLastWriteItem:=Low(FBuffer);
  finally
    if Assigned(CS) then CS.Leave;
  end;
end;


end.
