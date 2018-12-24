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
    FAutoInitSDL: Boolean;
    //procedure CreateWnd; override;
    //procedure DestroyWnd; override;
  protected
    procedure Paint; override;
    //Function CaslcSize(Sourse:TSDL_Rect):TSDL_Rect;
    //procedure OnRenderVideo(var Data:PMediaBufferInfo);
  public
    //Function GetSDLInfo:PSDLPantalla;
    function GetCanvas:TCanvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure UpdateRender;
    //procedure RenderVideoFrame(w,h:SInt32;Data:Array of PByte;linesize: Array of Integer; pix_fmt:TAVPixelFormat);
    //Procedure InitSDL;
    //Procedure DeInitSDL;
  published
    Property AutoInitSDL:Boolean Read FAutoInitSDL Write FAutoInitSDL default true;
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

constructor TMediaDisplay.Create(AOwner: TComponent);
begin
  inherited;
  CS:=TCriticalSection.Create;
  Self.Height:=200;
  Self.Width:=200;
  Self.Align:=alNone;
  //Self.OnCanResize:=CanResize;
  FProportionally:=true;//False;
end;

destructor TMediaDisplay.Destroy;
begin
  FreeAndNil(CS);
  inherited;
end;

function TMediaDisplay.GetCanvas: TCanvas;
begin
  Result:=Self.Canvas;
end;

procedure TMediaDisplay.Paint;
var Rect: TRect;
  Text:string;
begin
  //inherited; //?
  (* Message Pain frame *)
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
    Text:=' Display ['+IntToStr(Self.Tag)+'] '+Self.Caption+' ';
    Brush.Color:=clBlack;
    Font.Name := 'arial';
    Font.Color := clGreen;
    Font.Style := [];
    Font.Height := 16;
    TextOut((Self.Width div 2)-(TextWidth(Text) div 2),2,Text);
  end;
end;

end.
