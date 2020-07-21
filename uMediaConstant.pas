unit uMediaConstant;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,

  OpenGl,

  Vcl.ExtCtrls,Math,System.SyncObjs,System.Generics.Collections,

  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.DateUtils,
  libavcodec, libavdevice, libavfilter, libswresample, libswscale,
  libavutil, libavformat,
  sdl2,
  {SDL2_ttf,{sdl, {uResourcePaths,} System.Threading;


const

  WM_PAINT_FRAME = WM_USER + 0;
  WM_3D_CAMERA_RENDER = WM_USER + 1;

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
  TOnRenderVideoFrame = procedure (w,h:SInt32;Data:Array of PByte;linesize: Array of Integer; pix_fmt:AVPixelFormat) of object;
  TOnIsPlayed = Procedure (Sender:TObject;var Play:Boolean)of object;
  TOnFlushBuffer = procedure (Sender:TObject; var Item:Pointer) of object;

  T3DPoint = packed record
    x, y, z:GLfloat;
  end;
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
    FOnFlushBuffer: TOnFlushBuffer;
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
    Property OnFlushBuffer:TOnFlushBuffer read FOnFlushBuffer write FOnFlushBuffer;
    Procedure FlushBuffer;
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

procedure TMediaBuffer.FlushBuffer;
var i:Integer;
begin
  for I := 0 to High(FBuffer) do begin
   if FBuffer[i] <> nil then begin
    FOnFlushBuffer(Self,FBuffer[i]);
    if FBuffer[i] <> nil then FBuffer[i]:=nil;
   end;
  end;
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
  Result:=High(FBuffer)-1;
 finally
  //if Assigned(CS) then CS.Leave;
 end;
end;

function TMediaBuffer.ReadData(var Item: Pointer): Cardinal;
begin
  //if Assigned(CS) then CS.Enter;
  try
   Result:=FLastReadItem;
   Item:=FBuffer[FLastReadItem];
   if Item = nil then Exit;
   FBuffer[FLastReadItem]:=nil;
   inc(FLastReadItem);
   if FLastReadItem > High(FBuffer) then FLastReadItem:=Low(FBuffer);
  finally
    //if Assigned(CS) then CS.Leave;
  end;
end;

function TMediaBuffer.WriteData(var Item: Pointer): Cardinal;
begin
  //if Assigned(CS) then CS.Enter;
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
    //if Assigned(CS) then CS.Leave;
  end;
end;


end.
