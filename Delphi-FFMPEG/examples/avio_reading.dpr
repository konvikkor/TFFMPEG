(*
  * Copyright (c) 2014 Stefano Sabatini
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
  * libavformat AVIOContext API example.
  *
  * Make libavformat demuxer access media content through a custom
  * AVIOContext read callback.
  * @example avio_reading.c
*)

program avio_reading;

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

type
  pbuffer_data = ^buffer_data;

  buffer_data = record
    ptr: puint8_t;
    size: size_t;
    /// < size left in the buffer
  end;

function read_packet(opaque: Pointer; buf: puint8_t; buf_size: int): int; cdecl;
var
  bd: pbuffer_data;
begin
  bd := pbuffer_data(opaque);
  buf_size := MIN(buf_size, bd^.size);

  if (buf_size = 0) then
    Exit(AVERROR_EOF);

  WriteLn('ptr:', Integer(bd^.ptr), ' size:', bd^.size);

  (* copy internal buffer data to buf *)
  Move(bd^.ptr^, buf^, buf_size);
  bd^.ptr := bd^.ptr + buf_size;
  bd^.size := bd^.size - buf_size;
  Result := buf_size;
end;

function main(): int;
var
  fmt_ctx: pAVFormatContext;
  avio_ctx: pAVIOContext;
  buffer: puint8_t;
  avio_ctx_buffer: punsigned_char;
  buffer_size, avio_ctx_buffer_size: size_t;
  input_filename: pAnsiChar;
  ret: int;
  bd: buffer_data;
begin
  fmt_ctx := nil;
  avio_ctx := nil;
  buffer := nil;
  avio_ctx_buffer := nil;
  avio_ctx_buffer_size := 4096;
  input_filename := nil;
  ret := 0;
  FillChar(bd, SizeOf(bd), 0);

  if ParamCount <> 1 then
  begin
    WriteLn('usage: ' + ExtractFileName(ParamStr(0)) + ' input_file'#13#10'API example program to show how to read from a custom buffer ' + 'accessed through AVIOContext.');
    Exit(1);
  end;
  input_filename := pAnsiChar(AnsiString(ParamStr(1)));

  (* slurp file content into buffer *)
  ret := av_file_map(input_filename, buffer, buffer_size, 0, nil);
  if (ret >= 0) then
    try

      (* fill opaque structure used by the AVIOContext read callback *)
      bd.ptr := buffer;
      bd.size := buffer_size;

      fmt_ctx := avformat_alloc_context();
      if not Assigned(fmt_ctx) then
        Exit(AVERROR_ENOMEM);

      avio_ctx_buffer := av_malloc(avio_ctx_buffer_size);
      if (avio_ctx_buffer = nil) then
        Exit(AVERROR_ENOMEM);

      avio_ctx := avio_alloc_context(avio_ctx_buffer, avio_ctx_buffer_size, 0, @bd, read_packet, nil, nil);
      if (avio_ctx = nil) then
        Exit(AVERROR_ENOMEM);

      fmt_ctx^.pb := avio_ctx;

      ret := avformat_open_input(fmt_ctx, nil, nil, nil);
      if (ret < 0) then
      begin
        WriteLn('Could not open input');
        Exit;
      end;

      ret := avformat_find_stream_info(fmt_ctx, nil);
      if (ret < 0) then
      begin
        WriteLn('Could not find stream information');
        Exit;
      end;

      av_dump_format(fmt_ctx, 0, input_filename, 0);

      Result := 0;
    finally
      avformat_close_input(fmt_ctx);
      (* note: the internal buffer could have changed, and be != avio_ctx_buffer *)
      if (avio_ctx <> nil) then
      begin
        if Pointer(avio_ctx^.buffer) = Pointer(avio_ctx_buffer) then
          av_freep(avio_ctx^.buffer);
        av_freep(avio_ctx);
      end;
      av_file_unmap(buffer, buffer_size);

      if (ret < 0) then
      begin
        WriteLn('Error occurred: ', av_err2str(ret));
        Result := 1;
      end;

    end;
end;

begin
  try
    ExitCode := main();
  except
    on E: Exception do
      WriteLn(ErrOutput, E.ClassName, ': ', E.Message);
  end;

end.
