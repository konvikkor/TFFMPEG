unit uFFMPEG.context;

interface

uses
  libavcodec, libavdevice, libavfilter, libswresample, libswscale,
  libavutil, libavformat, ArrayHelper, DateUtils, Classes;

type
  TMediaContext = class;
  TMediaStream = class;

  TMediaUtils = class
    class function ErrorToText(ErrorCode:integer):String;
  end;

  TMediaContext = class (TObject)
  private
    localSrcMedia: AnsiString;
    function decodeDuration: TTime;
    function getDuration: UInt64;
  protected
    FormatContext: PAVFormatContext;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property duration:UInt64 Read getDuration;
    property durationAsTime:TTime Read decodeDuration;
    property SRCMedia:AnsiString Read localSrcMedia;
  published
  end;

  TMediaStream = class(TObject)
    private
    protected
    public
  end;

implementation

{ TMediaContext }

procedure TMediaContext.AfterConstruction;
begin
  inherited;
  Self.FormatContext:=avformat_alloc_context();
end;

procedure TMediaContext.BeforeDestruction;
begin
  if Assigned(Self.FormatContext) then avformat_free_context(Self.FormatContext);
  inherited;
end;

function TMediaContext.decodeDuration: TTime;
var durationSec, _fH,_fM,_fS, _H,_M,_S:Double;
begin
  Result:=EncodeDateTime(0,0,0,0,0,0,0);
  durationSec:= FormatContext.duration * av_q2d(AV_TIME_BASE_Q);
  (*_fH := durationSec / 3600.0;
  _H  := int(_fH);
  _fM := (_fH - _H) * 60.0;
  _M  := int(_fM);
  _fS := (_fM - _M) * 60.0;
  _S  := _fS;*)
  IncSecond(Result,Round(durationSec));
end;

function TMediaContext.getDuration: UInt64;
begin
  try
    Result:= Self.FormatContext.duration;
  except
    Result:=0;
  end;
end;

{ TMediaUtils }

class function TMediaUtils.ErrorToText(ErrorCode: integer): String;
var
  localAnsiString:AnsiString;
  localPAnsiChar:PAnsiChar;
begin
  SetLength(localAnsiString,AV_ERROR_MAX_STRING_SIZE);
  localPAnsiChar:=av_make_error_string(@localAnsiString,AV_ERROR_MAX_STRING_SIZE,ErrorCode);
  Result:=localPAnsiChar;
end;

end.
