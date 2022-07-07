(*
  * Copyright (c) 2001 Fabrice Bellard
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy
  * of this software and associated documentation files (the "Software"), to deal
  * in the Software without restriction, including without limitation the rights
  * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  * copies of the Software, and to permit persons to whom the Software is
  * furnished to do so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in
  * all copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  * THE SOFTWARE.
*)

program decode_audio;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}
{$MINENUMSIZE 4} (* use 4-byte enums *)

uses
  Winapi.Windows,
  System.SysUtils,
  System.Math,
  ffmpeg_types,
  libavcodec,
  libavdevice,
  libavfilter,
  libavformat,
  libavutil,
  libpostproc,
  libswresample,
  libswscale;

const
  AUDIO_INBUF_SIZE = 20480;
  AUDIO_REFILL_THRESH = 4096;

function decode(dec_ctx: PAVCodecContext; pkt: PAVPacket; frame: PAVFrame; outfile: THandle): Boolean;
var
  i, ch: Integer;
  ret, data_size: Integer;
begin
  (* send the packet with the compressed data to the decoder *)
  ret := avcodec_send_packet(dec_ctx, pkt);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Error submitting the packet to the decoder');
    Result := False;
    Exit;
  end;

  (* read all the output frames (in general there may be any number of them *)
  while ret >= 0 do
  begin
    ret := avcodec_receive_frame(dec_ctx, frame);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
    begin
      Result := True;
      Exit;
    end
    else if ret < 0 then
    begin
      Writeln(ErrOutput, 'Error during decoding');
      Result := False;
      Exit;
    end;
    data_size := av_get_bytes_per_sample(dec_ctx.sample_fmt);
    if data_size < 0 then
    begin
      (* This should not occur, checking just for paranoia *)
      Writeln(ErrOutput, 'Failed to calculate data size');
      Result := False;
      Exit;
    end;
    for i := 0 to frame.nb_samples - 1 do
      for ch := 0 to dec_ctx.channels - 1 do
        FileWrite(outfile, PByte(Integer(frame.data[ch]) + data_size * i)^, data_size);
  end;
  Result := True;
end;

function main(): Integer;
var
  outfilename, filename: string;
  codec: PAVCodec;
  c: PAVCodecContext;
  parser: PAVCodecParserContext;
  len, ret: Integer;
  f, outfile: THandle;
  inbuf: array [0 .. AUDIO_INBUF_SIZE + AV_INPUT_BUFFER_PADDING_SIZE - 1] of Byte;
  data: puint8_t;
  pkt: PAVPacket;
  decoded_frame: PAVFrame;
  data_size: Integer;
label
  fail;
begin
  parser := nil;
  decoded_frame := nil;

  if ParamCount < 2 then
  begin
    Writeln(Format('Usage: %s <input file> <output file>', [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;
  filename := ParamStr(1);
  outfilename := ParamStr(2);

  (* register all the codecs *)
  avcodec_register_all();

  pkt := av_packet_alloc();

  (* find the MPEG audio decoder *)
  codec := avcodec_find_decoder(AV_CODEC_ID_MP2);
  if not Assigned(codec) then
  begin
    Writeln(ErrOutput, 'Codec not found');
    Result := 1;
    Exit;
  end;

  parser := av_parser_init(Integer(codec.id));
  if not Assigned(parser) then
  begin
    Writeln(ErrOutput, 'Parser not found');
    Result := 1;
    Exit;
  end;

  c := avcodec_alloc_context3(codec);
  if not Assigned(c) then
  begin
    Writeln(ErrOutput, 'Could not allocate audio codec context');
    Result := 1;
    Exit;
  end;

  (* open it *)
  if avcodec_open2(c, codec, nil) < 0 then
  begin
    Writeln(ErrOutput, 'Could not open codec');
    Result := 1;
    Exit;
  end;

  f := FileOpen(filename, fmOpenRead);
  if f = INVALID_HANDLE_VALUE then
  begin
    Writeln(ErrOutput, Format('Could not open %s', [filename]));
    Result := 1;
    Exit;
  end;
  outfile := FileCreate(outfilename);
  if outfile = INVALID_HANDLE_VALUE then
  begin
    Writeln(ErrOutput, Format('Could not open %s', [outfilename]));
    av_free(c);
    Result := 1;
    Exit;
  end;

  (* decode until eof *)
  data := @inbuf[0];
  data_size := FileRead(f, inbuf[0], AUDIO_INBUF_SIZE);

  while data_size > 0 do
  begin
    if not Assigned(decoded_frame) then
    begin
      decoded_frame := av_frame_alloc();
      if not Assigned(decoded_frame) then
      begin
        Writeln(ErrOutput, 'Could not allocate audio frame');
        Result := 1;
        Exit;
      end;
    end;

    ret := av_parser_parse2(parser, c, pkt.data, pkt.size, data, data_size, AV_NOPTS_VALUE, AV_NOPTS_VALUE, 0);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Error while parsing');
      Result := 1;
      Exit;
    end;
    Inc(data, ret);
    Dec(data_size, ret);

    if pkt.size > 0 then
      if not decode(c, pkt, decoded_frame, outfile) then
      begin
        Result := 1;
        goto fail;
      end;

    if data_size < AUDIO_REFILL_THRESH then
    begin
      Move(data^, inbuf[0], data_size);
      data := @inbuf[0];
      len := FileRead(f, PByte(PAnsiChar(data) + data_size)^, AUDIO_INBUF_SIZE - data_size);
      if len > 0 then
        Inc(data_size, len);
    end;
  end;
  Result := 0;

  (* flush the decoder *)
  pkt.data := nil;
  pkt.size := 0;
  if not decode(c, pkt, decoded_frame, outfile) then
    Result := 1;

fail:
  FileClose(outfile);
  FileClose(f);

  avcodec_free_context(c);
  av_parser_close(parser);
  av_frame_free(decoded_frame);
  av_packet_free(pkt);
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;

end.
