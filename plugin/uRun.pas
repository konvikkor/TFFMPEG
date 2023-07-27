unit uRun;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, shellApi, ExtCtrls, tlhelp32;

type
  DosString = type AnsiString(866);
  TRunStatus = (rsClose, rsRun, rsError);

  TRun = class
  private
    SA: TSecurityAttributes;
    StartInf: TStartupInfo;
    ProcInf: TProcessInformation;
    FStatus: TRunStatus;
    FHide_Proc: Boolean;
    FPipe: Boolean;
    function CreateChildProcess(ExeName, CommadLine: string): Boolean;
    procedure SetHide_Proc(const Value: Boolean);
    function GetStatus: TRunStatus;
  public
    InputPipeRead, InputPipeWrite, OutputPipeRead, OutputPipeWrite,
      ErrorPipeRead, ErrorPipeWrite: THandle;
    Procedure CreateChildProcessAndWait(ExeName, CommadLine: string);
    property Hide_Proc: Boolean read FHide_Proc write SetHide_Proc
      Default False;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Run(FileName: TFileName; Param: string): Boolean;
    procedure stop;
    Procedure WritePipeOut(OutputPipe: THandle; InString: AnsiString);
    function ReadPipeInput(InputPipe: THandle; var BytesRem: Integer): String;
    property Status: TRunStatus Read GetStatus default rsClose;
    property Pipes: Boolean Read FPipe Write FPipe default False;
    Class function IsRunning(sName: string): Boolean;
  end;

implementation

{ TRun }

constructor TRun.Create(AOwner: TComponent);
begin
  SA.nLength := sizeof(SA);
  SA.bInheritHandle := True;
  SA.lpSecurityDescriptor := nil;
  try
    if not CreatePipe(InputPipeRead, InputPipeWrite, @SA, 0) then
      raise Exception.Create('Error:Pipe IN');
    if not CreatePipe(OutputPipeRead, OutputPipeWrite, @SA, 0) then
      raise Exception.Create('Error:Pipe OUT');
    if not CreatePipe(ErrorPipeRead, ErrorPipeWrite, @SA, 0) then
      raise Exception.Create('Error:Pipe ERR');
  except
    on e: Exception do
      raise Exception.Create('{Create class Trun}:' + e.message);
  end;
end;

function TRun.CreateChildProcess(ExeName, CommadLine: string): Boolean;
begin
  try
    ZeroMemory(@StartInf, sizeof(TStartupInfo));
    StartInf.cb := sizeof(TStartupInfo);
    if FPipe then
    begin
      StartInf.hStdInput := InputPipeRead;
      StartInf.hStdOutput := OutputPipeWrite;
      StartInf.hStdError := ErrorPipeWrite;
    end;
    // StartInf.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartInf.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
    if Hide_Proc then
      StartInf.wShowWindow := SW_HIDE
    else
      StartInf.wShowWindow := SW_SHOWNORMAL;
  except
    on e: Exception do
      Exception.Create('{CreateChildProcess}' + e.message);
  end;
  // Create the child process.
  try
    Application.ProcessMessages;
    Result := CreateProcess(nil, PChar(ExeName + ' ' + CommadLine),
      // command line
      @SA, // process security attributes
      @SA, // primary thread security attributes
      True, // handles are inherited
      // CREATE_NEW_CONSOLE or SYNCHRONIZE, // creation flags
      $00010000, // creation flags
      nil, // use parent's environment
      nil, // use parent's current directory
      StartInf, // STARTUPINFO pointer
      ProcInf); // receives PROCESS_INFORMATION
  except
    on e: Exception do
      Exception.Create('{CreateProcess}:' + e.message);
  end;
  FStatus := rsRun;
  // WaitForInputIdle(ProcInf.hThread, INFINITE);
  /// //////////////////////////////////////////////////////////////////////////
  /// Add wait finish process
  {
    with ProcessInfo do
    begin
    //Ждем завершения инициализации.
    WaitForInputIdle(hProcess, INFINITE);
    //Ждем завершения процесса.
    WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
    //Получаем код завершения.
    GetExitCodeProcess(ProcessInfo.hProcess, ExitCode);
    //Закрываем дескриптор процесса.
    CloseHandle(hThread);
    //Закрываем дескриптор потока.
    CloseHandle(hProcess);
  }
end;

procedure TRun.CreateChildProcessAndWait(ExeName, CommadLine: string);
var
  ExitCode: DWORD;
begin
  try
    ZeroMemory(@StartInf, sizeof(TStartupInfo));
    StartInf.cb := sizeof(TStartupInfo);
    StartInf.hStdInput := InputPipeRead;
    StartInf.hStdOutput := OutputPipeWrite;
    StartInf.hStdError := ErrorPipeWrite;
    // StartInf.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartInf.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
    if Hide_Proc then
      StartInf.wShowWindow := SW_HIDE
    else
      StartInf.wShowWindow := SW_SHOWNORMAL;
  except
    on e: Exception do
      Exception.Create('{CreateChildProcess}' + e.message);
  end;
  // Create the child process.
  try
    CreateProcess(nil, PChar(ExeName + ' ' + CommadLine),
      // command line
      @SA, // process security attributes
      @SA, // primary thread security attributes
      True, // handles are inherited
      // CREATE_NEW_CONSOLE or SYNCHRONIZE, // creation flags
      $00010000, // creation flags
      nil, // use parent's environment
      nil, // use parent's current directory
      StartInf, // STARTUPINFO pointer
      ProcInf); // receives PROCESS_INFORMATION
  except
    on e: Exception do
      Exception.Create('{CreateProcess}:' + e.message);
  end;
  FStatus := rsRun;
  /// //////////////////////////////////////////////////////////////////////////
  /// Add wait finish process
  with ProcInf do
  begin
    // Ждем завершения инициализации.
    WaitForInputIdle(hProcess, INFINITE);
    // Ждем завершения процесса.
    WaitforSingleObject(ProcInf.hProcess, INFINITE);
    // Получаем код завершения.
    GetExitCodeProcess(ProcInf.hProcess, ExitCode);
    // Закрываем дескриптор процесса.
    CloseHandle(hThread);
    // Закрываем дескриптор потока.
    CloseHandle(hProcess);
  end;

end;

destructor TRun.Destroy;
var
  i: Integer;
begin
  if ProcInf.hProcess <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(ProcInf.hThread);
    i := WaitforSingleObject(ProcInf.hProcess, 1000);
    CloseHandle(ProcInf.hProcess);
    if i <> WAIT_OBJECT_0 then
    begin
      ProcInf.hProcess := OpenProcess(PROCESS_TERMINATE, False,
        ProcInf.dwProcessId);
      if ProcInf.hProcess <> 0 then
      begin
        TerminateProcess(ProcInf.hProcess, 0);
        CloseHandle(ProcInf.hProcess);
      end;
    end;
  end;
end;

function TRun.GetStatus: TRunStatus;
begin
  try
    case WaitforSingleObject(ProcInf.hProcess, 1000) of
      WAIT_FAILED:
        FStatus := rsError; // Ошибка, завершаем поток
      WAIT_OBJECT_0:
        FStatus := rsRun; // Сообщаем об изменении
    end;
  except

  end;
  Result := FStatus;
end;

class function TRun.IsRunning(sName: string): Boolean;
var
  han: THandle;
  ProcStruct: PROCESSENTRY32;
  sID: string;
begin
  Result := False;

  han := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
  if han = 0 then
    exit;

  ProcStruct.dwSize := sizeof(PROCESSENTRY32);
  if Process32First(han, ProcStruct) then
  begin
    repeat
      sID := ExtractFileName(ProcStruct.szExeFile);

      if uppercase(copy(sID, 1, length(sName))) = uppercase(sName) then
      begin

        Result := True;
        Break;
      end;
    until not Process32Next(han, ProcStruct);
  end;

  CloseHandle(han);
end;

function TRun.ReadPipeInput(InputPipe: THandle; var BytesRem: Integer): String;
var
  TextBuffer: array [1 .. 32767] of byte; // AnsiChar;// char;
  tmp: Tbytes;
  BytesRead: Cardinal;
  PipeSize: Integer;
begin
  Result := '';
  PipeSize := length(TextBuffer);
  PeekNamedPipe(InputPipe, nil, PipeSize, @BytesRead, @PipeSize, @BytesRem);
  if BytesRead > 0 then
  begin
    FillChar(TextBuffer, sizeof(TextBuffer), #0);
    ReadFile(InputPipe, TextBuffer, PipeSize, BytesRead, nil);
    try
      tmp := TEncoding.Convert(TEncoding.GetEncoding(866),
        TEncoding.GetEncoding(1251), @TextBuffer);
    except
      on e: Exception do
        Exception.Create('{TEncoding:[ReadPipeInput]}' + e.message);
    end;
    Result := StringOf(tmp);
  end;
end;

function TRun.Run(FileName: TFileName; Param: string): Boolean;
begin
  if FStatus = rsRun then
    Self.stop;
  try
    if not CreateChildProcess(FileName, Param) then
      RaiseLastOSError;
  finally
    Result := True;
  end;
end;

procedure TRun.SetHide_Proc(const Value: Boolean);
begin
  FHide_Proc := Value;
end;

procedure TRun.stop;
var
  i: Integer;
begin
  if FStatus = rsClose then
    exit;
  try
    if ProcInf.hProcess <> INVALID_HANDLE_VALUE then
    begin
      CloseHandle(ProcInf.hThread);
      i := WaitforSingleObject(ProcInf.hProcess, 1000);
      CloseHandle(ProcInf.hProcess);
      if i <> WAIT_OBJECT_0 then
      begin
        ProcInf.hProcess := OpenProcess(PROCESS_TERMINATE, False,
          ProcInf.dwProcessId);
        if ProcInf.hProcess <> 0 then
        begin
          TerminateProcess(ProcInf.hProcess, 0);
          CloseHandle(ProcInf.hProcess);
          FStatus := rsClose;
        end;
      end;
    end;
  except
    FStatus := rsError;
  end;
end;

procedure TRun.WritePipeOut(OutputPipe: THandle; InString: AnsiString);
var
  byteswritten: DWORD;
begin // most console programs require CR/LF after their input.
  InString := InString + sLineBreak;
  WriteFile(InputPipeWrite, InString[1], length(InString), byteswritten, nil);
end;

end.
