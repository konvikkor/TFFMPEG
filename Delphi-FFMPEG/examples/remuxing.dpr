(*
  * Copyright (c) 2013 Stefano Sabatini
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

(* *
  * @file
  * libavformat/libavcodec demuxing and muxing API example.
  *
  * Remux streams from one container format to another.
  * @example remuxing.c
*)

program remuxing;

{$APPTYPE CONSOLE}
{$POINTERMATH ON}

uses
  Winapi.Windows,
  System.SysUtils,
  ffmpeg_types,
  libavcodec,
  libavdevice,
  libavfilter,
  libavformat,
  libavutil,
  libpostproc,
  libswresample,
  libswscale;

procedure log_packet(const fmt_ctx: PAVFormatContext; const pkt: PAVPacket; const tag: PAnsiChar);
var
  time_base: PAVRational;
begin
  time_base := @PAVStream(fmt_ctx.streams[pkt.stream_index]).time_base;

  Writeln(Format('%s: pts:%s pts_time:%s dts:%s dts_time:%s duration:%s duration_time:%s stream_index:%d', [tag, av_ts2str(pkt.pts), av_ts2timestr(pkt.pts, time_base),
    av_ts2str(pkt.dts), av_ts2timestr(pkt.dts, time_base), av_ts2str(pkt.duration), av_ts2timestr(pkt.duration, time_base), pkt.stream_index]));
end;

function main(): Integer;
var
  ofmt: PAVOutputFormat;
  ifmt_ctx, ofmt_ctx: PAVFormatContext;
  pkt: AVPacket;
  in_filename, out_filename: PAnsiChar;
  ret, i: Integer;
  stream_index: Integer;
  stream_mapping: PInteger;
  stream_mapping_size: Integer;
  out_stream: PAVStream;
  in_stream: PAVStream;
  in_codecpar: PAVCodecParameters;
label
  the_end;
begin
  ofmt := nil;
  ifmt_ctx := nil;
  ofmt_ctx := nil;
  stream_index := 0;
  stream_mapping := nil;

  if ParamCount < 2 then
  begin
    Writeln(ErrOutput, Format('usage: %s input output' + sLineBreak + 'API example program to remux a media file with libavformat and libavcodec.' + sLineBreak +
      'The output format is guessed according to the file extension.', [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;

  in_filename := PAnsiChar(AnsiString(ParamStr(1)));
  out_filename := PAnsiChar(AnsiString(ParamStr(2)));

  av_register_all();

  ret := avformat_open_input(ifmt_ctx, in_filename, nil, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Could not open input file ''%s''', [in_filename]));
    goto the_end;
  end;

  ret := avformat_find_stream_info(ifmt_ctx, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Failed to retrieve input stream information');
    goto the_end;
  end;

  av_dump_format(ifmt_ctx, 0, in_filename, 0);

  avformat_alloc_output_context2(ofmt_ctx, nil, nil, out_filename);
  if not Assigned(ofmt_ctx) then
  begin
    Writeln(ErrOutput, 'Could not create output context');
    ret := AVERROR_UNKNOWN;
    goto the_end;
  end;

  stream_mapping_size := ifmt_ctx.nb_streams;
  stream_mapping := av_mallocz_array(stream_mapping_size, SizeOf(stream_mapping^));
  if not Assigned(stream_mapping) then
  begin
    ret := AVERROR_ENOMEM;
    goto the_end;
  end;

  ofmt := ofmt_ctx.oformat;

  for i := 0 to ifmt_ctx.nb_streams - 1 do
  begin
    in_stream := ifmt_ctx.streams[i];
    in_codecpar := in_stream.codecpar;

    if (in_codecpar.codec_type <> AVMEDIA_TYPE_AUDIO) and (in_codecpar.codec_type <> AVMEDIA_TYPE_VIDEO) and (in_codecpar.codec_type <> AVMEDIA_TYPE_SUBTITLE) then
    begin
      stream_mapping[i] := -1;
      Continue;
    end;

    stream_mapping[i] := stream_index;
    Inc(stream_index);

    out_stream := avformat_new_stream(ofmt_ctx, nil);
    if not Assigned(out_stream) then
    begin
      Writeln(ErrOutput, 'Failed allocating output stream');
      ret := AVERROR_UNKNOWN;
      goto the_end;
    end;

    ret := avcodec_parameters_copy(out_stream.codecpar, in_codecpar);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Failed to copy codec parameters');
      goto the_end;
    end;
    out_stream.codecpar.codec_tag.tag := 0;
  end;
  av_dump_format(ofmt_ctx, 0, out_filename, 1);

  if (ofmt.flags and AVFMT_NOFILE) = 0 then
  begin
    ret := avio_open(ofmt_ctx.pb, out_filename, AVIO_FLAG_WRITE);
    if ret < 0 then
    begin
      Writeln(ErrOutput, Format('Could not open output file ''%s''', [out_filename]));
      goto the_end;
    end;
  end;

  ret := avformat_write_header(ofmt_ctx, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Error occurred when opening output file');
    goto the_end;
  end;

  while True do
  begin
    ret := av_read_frame(ifmt_ctx, @pkt);
    if ret < 0 then
      Break;

    in_stream := ifmt_ctx.streams[pkt.stream_index];
    if (pkt.stream_index >= stream_mapping_size) or (stream_mapping[pkt.stream_index] < 0) then
    begin
      av_packet_unref(pkt);
      Continue;
    end;

    pkt.stream_index := stream_mapping[pkt.stream_index];
    out_stream := ofmt_ctx.streams[pkt.stream_index];
    log_packet(ifmt_ctx, @pkt, 'in');

    (* copy packet *)
    pkt.pts := av_rescale_q_rnd(pkt.pts, in_stream.time_base, out_stream.time_base, Ord(AV_ROUND_NEAR_INF) or Ord(AV_ROUND_PASS_MINMAX));
    pkt.dts := av_rescale_q_rnd(pkt.dts, in_stream.time_base, out_stream.time_base, Ord(AV_ROUND_NEAR_INF) or Ord(AV_ROUND_PASS_MINMAX));
    pkt.duration := av_rescale_q(pkt.duration, in_stream.time_base, out_stream.time_base);
    pkt.pos := -1;
    log_packet(ofmt_ctx, @pkt, 'out');

    ret := av_interleaved_write_frame(ofmt_ctx, @pkt);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Error muxing packet');
      Break;
    end;
    av_packet_unref(@pkt);
  end;

  av_write_trailer(ofmt_ctx);
the_end:

  avformat_close_input(ifmt_ctx);

  (* close output *)
  if Assigned(ofmt_ctx) and ((ofmt.flags and AVFMT_NOFILE) = 0) then
    avio_closep(ofmt_ctx.pb);
  avformat_free_context(ofmt_ctx);

  av_freep(stream_mapping);

  if (ret < 0) and (ret <> AVERROR_EOF) then
  begin
    Writeln(ErrOutput, Format('Error occurred: %s', [av_err2str(ret)]));
    Result := 1;
    Exit;
  end;

  Result := 0;
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      Writeln(ErrOutput, E.ClassName, ': ', E.Message);
  end;

end.
