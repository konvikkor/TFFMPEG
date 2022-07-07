(*
  * Copyright (c) 2014 Lukasz Marek
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

program avio_dir_cmd;

{$APPTYPE CONSOLE}

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

function print_error(const filename: string; err: Integer): string;
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to High(CErrorList) do
    if CErrorList[I].err = err then
    begin
      S := CErrorList[I].msg;
      Break;
    end;
  if S = '' then
    S := Format('Error number %d occurred', [err]);
  if filename <> '' then
    Result := filename + ': ' + S
  else
    Result := S;
end;

function type_string(type_: Integer): PAnsiChar;
begin
  case AVIODirEntryType(type_) of
    AVIO_ENTRY_DIRECTORY:
      Result := '<DIR>';
    AVIO_ENTRY_FILE:
      Result := '<FILE>';
    AVIO_ENTRY_BLOCK_DEVICE:
      Result := '<BLOCK DEVICE>';
    AVIO_ENTRY_CHARACTER_DEVICE:
      Result := '<CHARACTER DEVICE>';
    AVIO_ENTRY_NAMED_PIPE:
      Result := '<PIPE>';
    AVIO_ENTRY_SYMBOLIC_LINK:
      Result := '<LINK>';
    AVIO_ENTRY_SOCKET:
      Result := '<SOCKET>';
    AVIO_ENTRY_SERVER:
      Result := '<SERVER>';
    AVIO_ENTRY_SHARE:
      Result := '<SHARE>';
    AVIO_ENTRY_WORKGROUP:
      Result := '<WORKGROUP>';
    AVIO_ENTRY_UNKNOWN:
      Result := '<UNKNOWN>';
  else
    Result := '<UNKNOWN>';
  end;
end;

function list_op(const input_dir: string): Integer;
var
  entry: PAVIODirEntry;
  ctx: PAVIODirContext;
  cnt, ret: Integer;
  filemode: string;
  uid_and_gid: string;
  s1, s2, s3, s4: string;
label
  fail;
begin
  entry := nil;
  ctx := nil;

  ret := avio_open_dir(ctx, PAnsiChar(AnsiString(input_dir)), nil);
  if ret < 0 then
  begin
    // av_log(nil, AV_LOG_ERROR, 'Cannot open directory: %s.'#10, av_err2str(ret));
    av_log(nil, AV_LOG_ERROR, 'Cannot open directory: %s.'#10, PAnsiChar(AnsiString(print_error('', ret))));
    goto fail;
  end;

  cnt := 0;
  while True do
  begin
    ret := avio_read_dir(ctx, entry);
    if ret < 0 then
    begin
      // av_log(nil, AV_LOG_ERROR, 'Cannot list directory: %s.'#10, av_err2str(ret));
      av_log(nil, AV_LOG_ERROR, 'Cannot list directory: %s.'#10, PAnsiChar(AnsiString(print_error('', ret))));
      goto fail;
    end;
    if not Assigned(entry) then
      Break;
    if entry.filemode = -1 then
      filemode := '???'
    else
      filemode := StringReplace(Format('%3d', [entry.filemode]), ' ', '0', [rfReplaceAll]);
    uid_and_gid := Format('%d(%d)', [entry.user_id, entry.group_id]);
    s1 := StringReplace(Format('%12d', [entry.size]), ' ', '0', [rfReplaceAll]);
    s2 := StringReplace(Format('%16d', [entry.modification_timestamp]), ' ', '0', [rfReplaceAll]);
    s3 := StringReplace(Format('%16d', [entry.access_timestamp]), ' ', '0', [rfReplaceAll]);
    s4 := StringReplace(Format('%16d', [entry.status_change_timestamp]), ' ', '0', [rfReplaceAll]);
    if cnt = 0 then
      av_log(nil, AV_LOG_INFO, '%-9s %12s %30s %10s %s %16s %16s %16s'#10, 'TYPE', 'SIZE', 'NAME', 'UID(GID)', 'UGO', 'MODIFIED',
        'ACCESSED', 'STATUS_CHANGED');
    av_log(nil, AV_LOG_INFO, '%-9s %s %30s %10s %s %16"PRId64" %16"PRId64" %16"PRId64"'#10, type_string(entry._type),
      PAnsiChar(AnsiString(s1)), entry.name, PAnsiChar(AnsiString(uid_and_gid)), PAnsiChar(AnsiString(filemode)), PAnsiChar(AnsiString(s2)),
      PAnsiChar(AnsiString(s3)), PAnsiChar(AnsiString(s4)));
    avio_free_directory_entry(entry);
    Inc(cnt);
  end;

fail:
  avio_close_dir(ctx);
  Result := ret;
end;

function del_op(const url: PAnsiChar): Integer;
var
  ret: Integer;
begin
  ret := avpriv_io_delete(url);
  if ret < 0 then
    // av_log(nil, AV_LOG_ERROR, 'Cannot delete "%s": %s.'#10, url, av_err2str(ret));
    av_log(nil, AV_LOG_ERROR, 'Cannot delete "%s": %s.'#10, url, PAnsiChar(AnsiString(print_error('', ret))));
  Result := ret;
end;

function move_op(const src, dst: PAnsiChar): Integer;
var
  ret: Integer;
begin
  ret := avpriv_io_move(src, dst);
  if ret < 0 then
    // av_log(nil, AV_LOG_ERROR, 'Cannot move "%s" into "%s": %s.'#10, src, dst, av_err2str(ret));
    av_log(nil, AV_LOG_ERROR, 'Cannot move "%s" into "%s": %s.'#10, src, dst, PAnsiChar(AnsiString(print_error('', ret))));
  Result := ret;
end;

procedure usage(const program_name: string);
begin
  Writeln(ErrOutput, Format('usage: %s OPERATION entry1 [entry2]' + sLineBreak + 'API example program to show how to manipulate resources '
    + 'accessed through AVIOContext.' + sLineBreak + 'OPERATIONS:' + sLineBreak + 'list      list content of the directory' + sLineBreak +
    'move      rename content in directory' + sLineBreak + 'del       delete content in directory' + sLineBreak, [program_name]));
end;

function main(): Integer;
var
  op: string;
  ret: Integer;
begin
  av_log_set_level(AV_LOG_DEBUG);

  if ParamCount < 1 then
  begin
    usage(ExtractFileName(ParamStr(0)));
    Result := 1;
    Exit;
  end;

  (* register codecs and formats and other lavf/lavc components *)
  av_register_all();
  avformat_network_init();

  op := ParamStr(1);
  if op = 'list' then
  begin
    if ParamCount < 2 then
    begin
      av_log(nil, AV_LOG_INFO, 'Missing argument for list operation.'#10);
      ret := AVERROR_EINVAL;
    end
    else
      ret := list_op(ParamStr(2));
  end
  else if op = 'del' then
  begin
    if ParamCount < 2 then
    begin
      av_log(nil, AV_LOG_INFO, 'Missing argument for del operation.'#10);
      ret := AVERROR_EINVAL;
    end
    else
      ret := del_op(PAnsiChar(AnsiString(ParamStr(2))));
  end
  else if op = 'move' then
  begin
    if ParamCount < 3 then
    begin
      av_log(nil, AV_LOG_INFO, 'Missing argument for move operation.'#10);
      ret := AVERROR_EINVAL;
    end
    else
      ret := move_op(PAnsiChar(AnsiString(ParamStr(2))), PAnsiChar(AnsiString(ParamStr(3))));
  end
  else
  begin
    av_log(nil, AV_LOG_INFO, 'Invalid operation %s'#10, PAnsiChar(AnsiString(op)));
    ret := AVERROR_EINVAL;
  end;

  avformat_network_deinit();

  if ret < 0 then
    Result := 1
  else
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
