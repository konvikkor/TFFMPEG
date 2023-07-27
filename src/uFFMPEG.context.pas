unit uFFMPEG.context;

interface

uses
  System.SysUtils, Classes, ArrayHelper, Winapi.Windows, DateUtils,
  libavcodec, libavdevice, libavfilter, libswresample, libswscale, libavutil, libavformat;

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
    procedure detectStreams;
    property duration:UInt64 Read getDuration;
    property durationAsTime:TTime Read decodeDuration;
    property SRCMedia:AnsiString Read localSrcMedia;
  published
    procedure open(Src:string);
  end;

  TMediaStream = class(TObject)
    private
    protected
    public
  end;

implementation

{ TMediaContext }

function PPtrIdx(P: PPAVStream; I: Integer): PAVStream;
begin
  Inc(P, I);
  Result := P^;
end;

procedure TMediaContext.AfterConstruction;
begin
  inherited;
  FormatContext:=avformat_alloc_context();
end;

procedure TMediaContext.BeforeDestruction;
begin
  if Assigned(FormatContext) then begin avformat_free_context(FormatContext); FormatContext:=nil; end;
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

procedure TMediaContext.detectStreams;
var
  LocalAnsiString,ErrorFFMPEGString:PAnsiChar;
  tmpAnsiString:AnsiString;
  localString:string;
  retApi, i:integer;

  tmpLocalStream:PAVStream;
  opts: pAVDictionary;
begin
 retApi := avformat_find_stream_info(FormatContext, nil);
 if retApi < 0 then begin
   SetLength(tmpAnsiString,AV_ERROR_MAX_STRING_SIZE);
   ErrorFFMPEGString:=av_make_error_string(PAnsiChar(tmpAnsiString),AV_ERROR_MAX_STRING_SIZE,retApi);
   raise Exception.Create('Could not find stream information : ' + ErrorFFMPEGString);
 end else begin
   for I := 0 to FormatContext.nb_streams - 1 do begin
     tmpLocalStream := PPtrIdx(Self.FormatContext.streams, I);
     if tmpLocalStream.codec.codec_type = AVMEDIA_TYPE_VIDEO then begin
       Self.VideoCodec := avcodec_find_decoder(tmpLocalStream.codec.codec_id);
       if not Assigned(Self.VideoCodec) then begin
         ErrorFFMPEGString:=av_get_media_type_string(tmpLocalStream.codec.codec_type);
         localString:= localString +sLineBreak+ 'Error Message:Failed to find ' + string(ErrorFFMPEGString) + ' codec';//av_err2str(retApi);
       end;
       opts := nil;
       retApi := avcodec_open2(tmpLocalStream.codec, Self.VideoCodec, @opts);
       if retApi < 0 then begin
         ErrorFFMPEGString:=av_get_media_type_string(tmpLocalStream.codec.codec_type);
         localString := localString +sLineBreak+'Error Message:Failed to open ' + string(ErrorFFMPEGString) + ' codec';
       end;
     end;
   end;
   if localString <> '' then begin
     raise Exception.Create(localString);
   end;
 end;
end;

function TMediaContext.getDuration: UInt64;
begin
  try
    Result:= Self.FormatContext.duration;
  except
    Result:=0;
  end;
end;

procedure TMediaContext.open(Src: string);
var
  LocalAnsiString,ErrorFFMPEGString:PAnsiChar;
  tmpAnsiString:AnsiString;
  retApi:integer;
begin
  LocalAnsiString:=PAnsiChar(Src);
  retApi:=avformat_open_input(FormatContext,LocalAnsiString, nil, nil);
  if retApi < 0 then begin
    SetLength(tmpAnsiString,AV_ERROR_MAX_STRING_SIZE);
    ErrorFFMPEGString:=av_make_error_string(PAnsiChar(tmpAnsiString),AV_ERROR_MAX_STRING_SIZE,retApi);
    raise Exception.Create('Could not open source "' + Src + '" : ' + ErrorFFMPEGString);
  end
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

initialization

finalization

end.
