(*
 * Copyright (c) 2017 Jun Zhao
 * Copyright (c) 2017 Kaixuan Liu
 *
 * HW Acceleration API (video decoding) decode sample
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

(**
 * @file
 * HW-Accelerated decoding example.
 *
 * @example hw_decode.c
 * This example shows how to do HW-accelerated decoding with output
 * frames from the HW video surfaces.
 *)

program hw_decode;

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

var
  hw_device_ctx: PAVBufferRef = nil;
  hw_pix_fmt: AVPixelFormat;
  output_file: THandle = INVALID_HANDLE_VALUE;

function find_fmt_by_hw_type(const type_: AVHWDeviceType): AVPixelFormat;
var
  fmt: AVPixelFormat;
begin
  case type_ of
    AV_HWDEVICE_TYPE_VAAPI: fmt := AV_PIX_FMT_VAAPI;
    AV_HWDEVICE_TYPE_DXVA2: fmt := AV_PIX_FMT_DXVA2_VLD;
    AV_HWDEVICE_TYPE_D3D11VA: fmt := AV_PIX_FMT_D3D11;
    AV_HWDEVICE_TYPE_VDPAU: fmt := AV_PIX_FMT_VDPAU;
    AV_HWDEVICE_TYPE_VIDEOTOOLBOX: fmt := AV_PIX_FMT_VIDEOTOOLBOX;
  else
    fmt := AV_PIX_FMT_NONE;
  end;

  Result := fmt;
end;

function hw_decoder_init(ctx: PAVCodecContext; const type_: AVHWDeviceType): Integer;
var
  err: Integer;
begin
  err := av_hwdevice_ctx_create(hw_device_ctx, type_, nil, nil, 0);
  if err < 0 then
  begin
    Writeln(ErrOutput, 'Failed to create specified HW device.');
    Result := err;
    Exit;
  end;
  ctx.hw_device_ctx := av_buffer_ref(hw_device_ctx);

  Result := err;
end;

function get_hw_format(ctx: PAVCodecContext; const pix_fmts: PAVPixelFormat): AVPixelFormat; cdecl;
var
  p: PAVPixelFormat;
begin
  p := pix_fmts;
  while p^ <> AV_PIX_FMT_NONE do
  begin
    if p^ = hw_pix_fmt then
    begin
      Result := p^;
      EXit;
    end;
    Inc(p);
  end;

  Writeln(ErrOutput, 'Failed to get HW surface format.');
  Result := AV_PIX_FMT_NONE;
end;

function decode_write(avctx: PAVCodecContext; packet: PAVPacket): Integer;
var
  frame, sw_frame: PAVFrame;
  tmp_frame: PAVFrame;
  buffer: PByte;
  size: Integer;
  ret: Integer;
label
  fail;
begin
  frame := nil;
  sw_frame := nil;
  tmp_frame := nil;
  buffer := nil;

  ret := avcodec_send_packet(avctx, packet);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Error during decoding');
    Result := ret;
    Exit;
  end;

  while ret >= 0 do
  begin
    frame := av_frame_alloc();
    if not Assigned(frame) then
    begin
      Writeln(ErrOutput, 'Can not alloc frame');
      ret := AVERROR_ENOMEM;
      goto fail;
    end;
    sw_frame := av_frame_alloc();
    if not Assigned(sw_frame) then
    begin
      Writeln(ErrOutput, 'Can not alloc frame');
      ret := AVERROR_ENOMEM;
      goto fail;
    end;

    ret := avcodec_receive_frame(avctx, frame);
    if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
    begin
      av_frame_free(frame);
      av_frame_free(sw_frame);
      Result := 0;
      Exit;
    end
    else if ret < 0 then
    begin
      Writeln(ErrOutput, 'Error while decoding');
      goto fail;
    end;

    if AVPixelFormat(frame.format) = hw_pix_fmt then
    begin
      (* retrieve data from GPU to CPU *)
      ret := av_hwframe_transfer_data(sw_frame, frame, 0);
      if ret < 0 then
      begin
        Writeln(ErrOutput, 'Error transferring the data to system memory');
        goto fail;
      end;
      tmp_frame := sw_frame;
    end
    else
      tmp_frame := frame;

    size := av_image_get_buffer_size(AVPixelFormat(tmp_frame.format), tmp_frame.width,
                                     tmp_frame.height, 1);
    buffer := av_malloc(size);
    if not Assigned(buffer) then
    begin
      Writeln(ErrOutput, 'Can not alloc buffer');
      ret := AVERROR_ENOMEM;
      goto fail;
    end;
    ret := av_image_copy_to_buffer(buffer, size,
                                  @tmp_frame.data[0],
                                  @tmp_frame.linesize[0], AVPixelFormat(tmp_frame.format),
                                   tmp_frame.width, tmp_frame.height, 1);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Can not copy image to buffer');
      goto fail;
    end;

    ret := FileWrite(output_file, buffer^, size);
    if ret < 0 then
    begin
      Writeln(ErrOutput, 'Failed to dump raw data.');
      goto fail;
    end;

fail:
    av_frame_free(frame);
    av_frame_free(sw_frame);
    if Assigned(buffer) then
      av_freep(buffer);
    if ret < 0 then
    begin
      Result := ret;
      Exit;
    end;
  end;

  Result := 0;
end;

function main(): Integer;
var
  input_ctx: PAVFormatContext;
  video_stream, ret: Integer;
  video: PAVStream;
  decoder_ctx: PAVCodecContext;
  decoder: PAVCodec;
  packet: AVPacket;
  type_: AVHWDeviceType;
begin
  input_ctx := nil;
  video := nil;
  decoder_ctx := nil;
  decoder := nil;

  if ParamCount < 3 then
  begin
    Writeln(ErrOutput, Format('Usage: %s <vaapi|vdpau|dxva2|d3d11va> <input file> <output file>', [ExtractFileName(ParamStr(0))]));
    Result := 1;
    Exit;
  end;

  av_register_all();

  type_ := av_hwdevice_find_type_by_name(PAnsiChar(AnsiString(ParamStr(1))));
  hw_pix_fmt := find_fmt_by_hw_type(type_);
  if hw_pix_fmt = AV_PIX_FMT_NONE then
  begin
    Writeln(ErrOutput, Format('Cannot support "%s" in this example.', [ParamStr(1)]));
    Result := 1;
    Exit;
  end;

  (* open the input file *)
  if avformat_open_input(input_ctx, PAnsiChar(AnsiString(ParamStr(2))), nil, nil) <> 0 then
  begin
    Writeln(ErrOutput, Format('Cannot open input file "%s"', [ParamStr(2)]));
    Result := 1;
    Exit;
  end;

  if avformat_find_stream_info(input_ctx, nil) < 0 then
  begin
    Writeln(ErrOutput, 'Cannot find input stream information.');
    Result := 1;
    Exit;
  end;

  (* find the video stream information *)
  ret := av_find_best_stream(input_ctx, AVMEDIA_TYPE_VIDEO, -1, -1, @decoder, 0);
  if ret < 0 then
  begin
    Writeln(ErrOutput, 'Cannot find a video stream in the input file');
    Result := 1;
    Exit;
  end;
  video_stream := ret;

  decoder_ctx := avcodec_alloc_context3(decoder);
  if not Assigned(decoder_ctx) then
  begin
    Result := AVERROR_ENOMEM;
    Exit;
  end;

  video := input_ctx.streams[video_stream];
  if avcodec_parameters_to_context(decoder_ctx, video.codecpar) < 0 then
  begin
    Result := 1;
    Exit;
  end;

  decoder_ctx.get_format := @get_hw_format;
  av_opt_set_int(decoder_ctx, 'refcounted_frames', 1, 0);

  if hw_decoder_init(decoder_ctx, type_) < 0 then
  begin
    Result := 1;
    Exit;
  end;

  ret := avcodec_open2(decoder_ctx, decoder, nil);
  if ret < 0 then
  begin
    Writeln(ErrOutput, Format('Failed to open codec for stream #%u', [video_stream]));
    Result := 1;
    Exit;
  end;

  (* open the file to dump raw data *)
  output_file := FileCreate(ParamStr(3));
  if output_file = INVALID_HANDLE_VALUE then
  begin
    Writeln(ErrOutput, Format('Could not open %s', [ParamStr(3)]));
    Result := 1;
    Exit;
  end;

  (* actual decoding and dump the raw data *)
  while ret >= 0 do
  begin
    ret := av_read_frame(input_ctx, @packet);
    if ret < 0 then
      Break;

    if video_stream = packet.stream_index then
      ret := decode_write(decoder_ctx, @packet);

    av_packet_unref(@packet);
  end;

  (* flush the decoder *)
  packet.data := nil;
  packet.size := 0;
  {ret := }decode_write(decoder_ctx, @packet);
  av_packet_unref(@packet);

  if output_file <> INVALID_HANDLE_VALUE then
    FileClose(output_file);
  avcodec_free_context(decoder_ctx);
  avformat_close_input(input_ctx);
  av_buffer_unref(hw_device_ctx);

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
