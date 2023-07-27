unit uWebSocketMain;

interface

uses
  Windows, Messages, SysUtils, System.Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, OverbyteIcsIniFiles, VCL.StdCtrls, VCL.ExtCtrls, OverbyteIcsWSocket,
  OverbyteIcsWSocketS, OverbyteIcsWndControl;
{$M+}

type
  TDisplayProc = procedure(const Msg: String) of object;

  TClientThread = class(TThread)
  private
    FWSocket: TWSocket; { Reference to client socket }
    FOnDisplay: TDisplayProc; { Event variable }
    FThreadAttached: Boolean; { TRUE once socket attached }
  public
    procedure Execute; override; { Main method }
    procedure Display(const Msg: String); { Takes care of synchroniz. }
  published
    property WSocket: TWSocket read FWSocket write FWSocket;
    property ThreadAttached: Boolean read FThreadAttached write FThreadAttached;
    property OnDisplay: TDisplayProc read FOnDisplay write FOnDisplay;
  end;

  TThrdSrvClient = class(TWSocketClient)
  public
    ClientThread: TClientThread;
    RcvdLine: String;
    ConnectTime: TDateTime;
  end;

  TwebSocketServer = class(TObject)
  private
    procedure wssOnBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure wssOnClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure wssOnServer1ClientDisconnect(Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure wssOnServer1BgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure wssOnServer1ClientCreate(Sender: TObject; Client: TWSocketClient);

    procedure ClientDataAvailable(Sender: TObject; Error: Word);
    procedure ProcessData(Client: TThrdSrvClient);
    procedure ClientBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure ClientLineLimitExceeded(Sender: TObject; Cnt: LongInt; var ClearData: Boolean);
  public
    WSS: TWSocketServer;
    constructor Create; overload;
  end;

procedure initWebSocketServer;
procedure destWebSocketServer;

var
  WSocketServer: TwebSocketServer;

implementation

uses ArrayHelper, logger, uRFC6455;

procedure initWebSocketServer;
begin
  try WSocketServer := TwebSocketServer.Create;
  except
    on E: Exception do begin
      __log__.Error(E.Message);
    end;
  end;
end;

procedure destWebSocketServer;
begin

end;

{ TClientThread }

procedure TClientThread.Display(const Msg: String);
begin

end;

procedure TClientThread.Execute;
begin
  inherited;

end;

{ TwebSocketServer }

procedure TwebSocketServer.ClientBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
begin
  with Sender as TThrdSrvClient do begin
    ClientThread.Display('Client exception occured: ' + E.ClassName + ': ' + E.Message);
  end;
  CanClose := TRUE; { Goodbye client ! }
end;

procedure TwebSocketServer.ClientDataAvailable(Sender: TObject; Error: Word);
begin
  with Sender as TThrdSrvClient do begin
    { We use line mode. We will receive complete lines }
    RcvdLine := ReceiveStr;
    { Remove trailing CR/LF }
    // while (Length(RcvdLine) > 0) and IsCharInSysCharset(RcvdLine[Length(RcvdLine)], [#13, #10]) do RcvdLine := Copy(RcvdLine, 1, Length(RcvdLine) - 1);
    // ClientThread.Display('Received from '+GetPeerAddr+': '''+RcvdLine + ''' '+'ThreadID: $'+IntToHex(GetCurrentThreadID, 8));
    // ProcessData(Sender as TThrdSrvClient);
  end;
end;

procedure TwebSocketServer.ClientLineLimitExceeded(Sender: TObject; Cnt: Integer; var ClearData: Boolean);
begin
  with Sender as TThrdSrvClient do begin
    ClientThread.Display('Line limit exceeded from ' + GetPeerAddr + '. Closing.');
    ClearData := TRUE;
    Close;
  end;
end;

constructor TwebSocketServer.Create;
begin
  inherited Create;
  WSS := TWSocketServer.Create(nil);
  with WSS do begin
    Proto := 'tcp'; { Use TCP protocol }
    Port := '55000'; { Use telnet port }
    Addr := '127.0.0.1'; // '0.0.0.0';      { Use any interface       }
    SendFlags := wsSendUrgent;
    LingerOnOff := wsLingerNoSet;
    LineMode := false;
    ClientClass := TThrdSrvClient; { Use our component }
    OnBgException := wssOnBgException;
    Listen;
  end;
end;

procedure TwebSocketServer.ProcessData(Client: TThrdSrvClient);
begin

end;

procedure TwebSocketServer.wssOnBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
begin
  __log__.Error(E.Message);
end;

procedure TwebSocketServer.wssOnClientConnect(Sender: TObject; Client: TWSocketClient; Error: Word);
begin
  with Client as TThrdSrvClient do begin
    __log__.info('Client connected.' + ' Remote: ' + PeerAddr + '/' + PeerPort + ' Local: ' + GetXAddr + '/' + GetXPort + ' ThreadID : $' +
      IntToHex(ClientThread.ThreadID, 8));
    OnDataAvailable := ClientDataAvailable;
    OnLineLimitExceeded := ClientLineLimitExceeded;
    OnBgException := ClientBgException;
    ConnectTime := Now;
  end;
end;

procedure TwebSocketServer.wssOnServer1BgException(Sender: TObject; E: Exception; var CanClose: Boolean);
begin

end;

procedure TwebSocketServer.wssOnServer1ClientCreate(Sender: TObject; Client: TWSocketClient);
begin
  with Client as TThrdSrvClient do begin
    Client.ThreadDetach;
    Client.MultiThreaded := TRUE;
    ClientThread := TClientThread.Create(TRUE);
    ClientThread.FreeOnTerminate := TRUE;
    ClientThread.WSocket := Client;
    // ClientThread.OnDisplay       := Display;
    ClientThread.Suspended := false;
    { Wait until thread is started and has attached client socket to }
    { his own context. }
    while not ClientThread.ThreadAttached do
      Sleep(0);
  end;
end;

procedure TwebSocketServer.wssOnServer1ClientDisconnect(Sender: TObject; Client: TWSocketClient; Error: Word);
begin
  with Client as TThrdSrvClient do begin
    __log__.info('Client disconnecting: ' + PeerAddr + '   ' + 'Duration: ' + FormatDateTime('hh:nn:ss', Now - ConnectTime) + ' ThreadID : $' +
      IntToHex(GetCurrentThreadID, 8));
    { Clear WSocket reference in worker thread }
    { ClientThread.WSocket := nil; }
    { Break message pump within worker thread }
    PostThreadMessage(ClientThread.ThreadID, WM_QUIT, 0, 0);
    { Allow up to 10 second for thread termination }
    WaitForSingleObject(ClientThread.Handle, 10000);
  end;
end;

end.
