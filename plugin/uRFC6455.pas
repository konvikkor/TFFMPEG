unit uRFC6455;

interface

uses SysUtils, Windows, Winsock, Classes, SyncObjs, ActiveX, VCL.Forms,
  DateUtils, System.Hash, ArrayHelper;

const
  RFC6455_SpecGUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

type
  TRFC6455OpCode = (ocText, ocBin, ocReservData, ocClose, ocPing, ocPong, ocReservControl, ocContinued);
  TRFC6455EventCode = (ecNormal, ecGone, ecError, ecData);
  PRFC6455Data = ^TRFC6455Data;

  TRFC6455Data = packed record
    PayloadDataLen: UInt64;
    Fin: Boolean;
    OpCode: TRFC6455OpCode;
    PayloadData: TArray<byte>;
    RAWData: TArray<byte>;
    Function MSGAssemble(mask: Boolean = false): TArray<byte>;
    Function MSGDisassemble(Msg: TArray<byte>): UInt64;
    Function CreateMessageRAW(SEC_MSG: TArray<byte>; mask:Boolean = false):TArray<byte>;
  public
    class Function MSGDisassembles(Msg: TArray<byte>): TArray<TRFC6455Data>; static;
    procedure AfterConstruction;
    procedure BeforeDestruction;
  end;

  PRFC6455 = ^TRFC6455;

  TRFC6455 = class
  private
    FPayloadDataLen: Int64;
    FPayloadData: TStringStream;
    FFin: Boolean;
    FOpCode: TRFC6455OpCode;
    FMask: Array [0 .. 3] of byte;
    FEventCode: TRFC6455EventCode;
    procedure GenMask;
  protected
    Function SHA1Encode(Input: AnsiString): AnsiString;
    function Base64Encode(Input: AnsiString): AnsiString;
  public
    Constructor Create;
    Destructor Destroy; override;
    Function RequestHeader(Host: String; Path: string): String;
    Function ResponseHeader(Header: String): String;
    Function MSGAssemble(mask: Boolean = false): TArray<byte>; overload;
    Function MSGAssemble(var Msg: TMemoryStream; mask: Boolean = false): Boolean; overload;
    Function MSGDisassemble(Msg: TArray<byte>): TArray<byte>; overload;
    Function MSGDisassemble(Msg: TStream): Boolean; overload;
    property PayloadData: TStringStream Read FPayloadData Write FPayloadData;
    property PayloadDataLen: Int64 Read FPayloadDataLen Write FPayloadDataLen default 0;
    property Fin: Boolean Read FFin Write FFin default True;
    property OpCode: TRFC6455OpCode Read FOpCode Write FOpCode default ocText;
    Property EventCode: TRFC6455EventCode Read FEventCode Write FEventCode default ecNormal;
  end;

implementation

uses Math;

function BSwap32(I: DWORD): DWORD; { inline; }
asm
  BSWAP   EAX
end;

// byte swaps 16 bit values
function BSwap16(I: WORD): WORD; { inline; }
asm
  XCHG    AL,AH
end;

// byte swaps 64 bit values
function BSwap64(I: UInt64): UInt64; { inline; }
asm
  MOV     EDX,[EAX]
  MOV     EAX,[EAX+4]
  BSWAP   EAX
  BSWAP   EDX
end;

{ TRFC6455 }

function TRFC6455.Base64Encode(Input: AnsiString): AnsiString;
var Final: AnsiString; Count, Len: Integer;
const
  Base64Out: array [0 .. 64] of AnsiChar = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U',
    'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
    'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '=');
begin Final := '';
  Count := 1;
  Len := Length(Input);
  while Count <= Len do begin
    Final := Final + Base64Out[(byte(Input[Count]) and $FC) shr 2];
    if (Count + 1) <= Len then begin
      Final := Final + Base64Out[((byte(Input[Count]) and $03) shl 4) + ((byte(Input[Count + 1]) and $F0) shr 4)];
      if (Count + 2) <= Len then begin
        Final := Final + Base64Out[((byte(Input[Count + 1]) and $0F) shl 2) + ((byte(Input[Count + 2]) and $C0) shr 6)];
        Final := Final + Base64Out[(byte(Input[Count + 2]) and $3F)];
      end else begin
        Final := Final + Base64Out[(byte(Input[Count + 1]) and $0F) shl 2];
        Final := Final + '=';
      end
    end else begin
      Final := Final + Base64Out[(byte(Input[Count]) and $03) shl 4];
      Final := Final + '==';
    end;
    Count := Count + 3;
  end;
  Result := Final;
end;

constructor TRFC6455.Create;
begin
  FPayloadData := TStringStream.Create('');
  FillChar(FMask, SizeOf(FMask), #0);
end;

destructor TRFC6455.Destroy;
begin
  FreeAndNil(FPayloadData);
end;

procedure TRFC6455.GenMask;
var I, b: byte;
begin
  Randomize;
  for I := 0 to 3 do begin
    repeat b := RandomRange($01, $FF);
    until b <> $0;
    FMask[I] := b;
  end;
end;

Function MSGAssemble(mask: Boolean = false): TArray<byte>;
begin
  Result := Result + [$00, $00];
end;

function TRFC6455.MSGAssemble(var Msg: TMemoryStream; mask: Boolean): Boolean;
var b, I: byte; l, m: Int64; s: string;
begin
  try
    try
      { need test functiob } b := IfThen(FFin, 1, 0) shl 7; { this }
      case FOpCode of
        ocText: b := b or $1;
        ocBin: b := b or $2;
        ocReservData: b := b or $3; // ..$7
        ocClose: b := b or $8;
        ocPing: b := b or $9;
        ocPong: b := b or $A;
        ocReservControl: b := b or $B; // ..$F
        ocContinued: b := b or $0;
      end;
      Msg.Write(b, 1);
      b := 0;
      b := IfThen(mask, 1, 0) shl 7; { this }
      l := FPayloadData.Size; { Load data }
      if l > 125 then begin I := 0;
        if l > $FFFF then begin { 64 }
          b := b or 127;
          Msg.Write(b, 1);
          b := 0;
          s := IntToHex(l, 16); { HEX $FFFFFFFFffffffff }
          I := 0;
          while I <= 16 do begin b := StrToInt('0x' + s[I] + s[I + 1]);
            Msg.Write(b, 1);
            Inc(I, 2);
          end;
        end else begin { 16 }
          b := b or 126;
          Msg.Write(b, 1);
          b := 0;
          s := IntToHex(l, 4); { HEX $FFFF }
          while I <= 4 do begin b := StrToInt('0x' + s[I] + s[I + 1]);
            Msg.Write(b, 1);
            Inc(I, 2);
          end;
        end;
      end else begin { normal }
        b := b or l;
        Msg.Write(b, 1);
        b := 0;
      end; { end add length in pack }
      { Mask and Message }
      if mask then begin GenMask; { Generate mask packet Random values $01..$ff }
        m := 0;
        for I := 0 to 3 do
          Msg.Write(FMask[I], 1); { Write mask }
        FPayloadData.Position := 0; { reset position }
        while m <= FPayloadData.Size do begin { werite message }
          FPayloadData.Read(b, 1);
          b := b xor FMask[m mod 4];
          Msg.Write(b, 1);
          Inc(m);
        end;
      end else begin { not mask }
        FPayloadData.Position := 0;
        m := 0;
        while m < FPayloadData.Size do begin { werite message }
          FPayloadData.Read(b, 1);
          Msg.Write(b, 1);
          Inc(m);
        end;
      end;
    finally Result := True;
    end;
  except Result := false;
  end;
end;

function TRFC6455.MSGDisassemble(Msg: TArray<byte>): TArray<byte>;
var
  x: byte;
  index, indexMessage, PayloadDataLen: UInt64;
  mask: Array [0 .. 3] of byte;
  Masked: Boolean;
  ReturnMessage: TArray<byte>;
  j: Int64;
begin
  if High(Msg) <= 0 then begin Exit;
  end;
  index := 0;
  PayloadDataLen := 0;
  try
    repeat
      FFin := Boolean(Msg[index] and $80); { Read bit FIN }
      FillChar(mask, SizeOf(mask), $0);
      case Msg[index] and $0F of { Read Opcode }
        $1: FOpCode := ocText;
        $2: FOpCode := ocBin;
        $3 .. $7: FOpCode := ocReservData;
        $8: FOpCode := ocClose;
        $9: FOpCode := ocPing;
        $A: FOpCode := ocPong;
        $B .. $F: FOpCode := ocReservControl;
        $0: FOpCode := ocContinued;
      end;
      Inc(index);
      Masked := Boolean(Msg[index] and $80); { Read Mask bit }
      x := Msg[index] and $7F; { Read Payload len }
      Inc(index);
      if x = 126 then begin
        PayloadDataLen := UInt64(BSwap16(PWord(Integer(@Msg[0]) + index)^)); { 16Bit Length 2Byte DWORD }
        Inc(index);
      end;
      if x = 127 then begin
        PayloadDataLen := UInt64(BSwap64(PUint64(Integer(@Msg[0]) + index)^)); { 64bit length 8Byte INT64 }
        Inc(index, 6);
      end;
      if x <= 125 then begin
        PayloadDataLen := x; { default length }
      end;
      if Masked then begin { Read INT32 mask KEY }
        mask[0] := Msg[index];
        Inc(index);
        mask[1] := Msg[index];
        Inc(index);
        mask[2] := Msg[index];
        Inc(index);
        mask[3] := Msg[index];
      end;
      Inc(index);
      { Chek position data fro write thi's }
      if FOpCode <> ocContinued then
        FPayloadData.Position := 0;
      { Length Message } j := High(ReturnMessage);
      if j < 0 then
        j := 0;
      SetLength(ReturnMessage, j + PayloadDataLen);
      while j <= PayloadDataLen - 1 do begin
        if j > High(ReturnMessage) then
          Break;
        if Masked then begin ReturnMessage[j] := Msg[index] xor FMask[j mod 4];
        end else begin ReturnMessage[j] := Msg[index];
        end;
        Inc(j);
        Inc(index);
      end;
    until (FFin or (index = High(Msg))) and (FOpCode <> ocContinued);
    Result := ReturnMessage;
  except

  end;
  { Neded Test ins function }
end;

function TRFC6455.MSGAssemble(mask: Boolean): TArray<byte>;
var l: UInt64;
  AMask: TArray<byte>;
  ADataPak: TArray<byte>;
  I: Integer;
begin
  Result := Result + [$00]; { (FIN, 0 , 0 , 0) 4/4 (opcode) }
  Result := Result + [$00]; { (MASK) 1/7 (Payload len) }
  case FOpCode of
    ocText: Result[0] := $1;
    ocBin: Result[0] := $2;
    ocReservData: Result[0] := $3; // ..$7
    ocClose: Result[0] := $8;
    ocPing: Result[0] := $9;
    ocPong: Result[0] := $A;
    ocReservControl: Result[0] := $B; // ..$F
    ocContinued: Result[0] := $0;
  end;
  if FFin then
    Result[0] := Result[0] or $80;
  l := FPayloadData.Size;
  if ((l > 125) and (l > $FFFF)) then begin
    Result[1] := 127;
    Result := Result + [$00, $00, $00, $00, $00, $00, $00, $00]; { UINT 64 }
    CopyMemory(@Result[2], @l, SizeOf(l));
    Result[0] := Result[0] or $80; { fin }
  end else begin
    if ((l > 125) and (l <= $FFFF)) then begin
      Result[1] := 126;
      Result := Result + [$00, $00]; { UINT 16 }
      CopyMemory(@Result[2], @l, 2);
      Result[0] := Result[0] or $80; { fin }
    end else begin
      CopyMemory(@Result[1], @l, 1);
      Result[0] := Result[0] or $80; { fin }
    end;
  end;
  ADataPak := FPayloadData.Bytes;
  SetLength(ADataPak, FPayloadData.Size);
  if mask then begin
    Randomize;
    AMask := [Random(255), Random(255), Random(255), Random(255)];
    for I := Low(ADataPak) to High(ADataPak) do begin
      ADataPak[I] := ADataPak[I] xor AMask[I mod 4]
    end;
    Result[1] := Result[1] OR $80;
    Result := Result + AMask;
  end;
  Result := Result + ADataPak;
end;

function TRFC6455.MSGDisassemble(Msg: TStream): Boolean;
var x: byte; mask: Boolean; j: Int64;
begin
  if Msg.Position > 0 then
    Msg.Position := 0;
  Result := false;
  try
    repeat
      Msg.Read(x, 1);
      FFin := Boolean(x and $80); { Read bit FIN }
      x := x and $0F; { Read Opcode }
      case x of
        $1: FOpCode := ocText;
        $2: FOpCode := ocBin;
        $3 .. $7: FOpCode := ocReservData;
        $8: FOpCode := ocClose;
        $9: FOpCode := ocPing;
        $A: FOpCode := ocPong;
        $B .. $F: FOpCode := ocReservControl;
        $0: FOpCode := ocContinued;
      end;
      Msg.Read(x, 1);
      mask := Boolean(x and $80); { Read Mask bit }
      x := x and $7F; { Read Payload len }
      if x = 126 then
        Msg.Read(FPayloadDataLen, 2); { 16Bit Length 2Byte DWORD }
      if x = 127 then
        Msg.Read(FPayloadDataLen, 8); { 64bit length 8Byte INT64 }
      if x <= 125 then
        FPayloadDataLen := x; { default length }
      if mask then
        Msg.Read(FMask[0], 4); { Read INT32 mask KEY }
      { Chek position data fro write thi's }
      if FOpCode <> ocContinued then
        FPayloadData.Position := 0;
      { Length Message } j := 0;
      while j <= FPayloadDataLen - 1 do begin
        if j > Msg.Size then
          Break;
        Msg.Read(x, 1);
        x := x xor FMask[j mod 4];
        FPayloadData.Write(x, 1);
        Inc(j);
      end;
    until (FFin or (Msg.Position = Msg.Size)) and (FOpCode <> ocContinued);
  except Result := false;
  end;
  { Neded Test ins function }
end;

function TRFC6455.RequestHeader(Host, Path: string): String;
var Header: TStringList;
  tmpKey: Array [1 .. 16] of AnsiChar;
  I: Integer;
begin
  Header := TStringList.Create;
  try
    // header.Delimiter:=#13;
    Header.NameValueSeparator := ':';
    Header.Add('GET ' + Path + ' HTTP/1.1');
    Header.Values['Host'] := Host;
    Header.Values['Connection'] := 'Upgrade';
    Header.Values['Upgrade'] := 'websocket';
    Header.Values['Sec-WebSocket-Version'] := '13';
    for I := Low(tmpKey) to High(tmpKey) do begin
      tmpKey[I] := AnsiChar(64 + Random(113));
    end;
    Header.Values['Sec-WebSocket-Key'] := Base64Encode(tmpKey);
    Result := Header.Text + sLineBreak;
  finally FreeAndNil(Header);
  end;
end;

function TRFC6455.ResponseHeader(Header: String): String;
const Response = 'HTTP/1.1 101 Switching Protocols' + sLineBreak + 'Upgrade: websocket' + sLineBreak + 'Connection: Upgrade' + sLineBreak +
    'Sec-WebSocket-Accept: %s' + sLineBreak + sLineBreak;
var Head: TStringList;
begin Result := ''; { This function need to DEVELOPER }
  Head := TStringList.Create;
  try
    Head.NameValueSeparator := ':';
    Head.Text := Header;
    if (LowerCase(Trim(Head.Values['Upgrade'])) = 'websocket') then begin
      if (Trim(Head.Values['Sec-WebSocket-Version']) = '13') then begin
        Result := Base64Encode(SHA1Encode(Trim(Head.Values['Sec-WebSocket-Key']) + RFC6455_SpecGUID));
        Result := Format(Response, [Result]);
      end;
    end;
  finally FreeAndNil(Head);
  end;
end;

function TRFC6455.SHA1Encode(Input: AnsiString): AnsiString;
var I: Integer;
  localSHA1: TArray<byte>;
begin Result := '';
  localSHA1 := THashSHA1.GetHashBytes(Input);
  for I := 0 to 19 do
    Result := Result + AnsiChar(localSHA1[I]);
end;

{ TRFC6455Data }

procedure TRFC6455Data.AfterConstruction;
begin
  Self.PayloadDataLen:=0;
  Self.Fin:=true;
  Self.OpCode:=ocBin;
end;

procedure TRFC6455Data.BeforeDestruction;
begin

end;

function TRFC6455Data.CreateMessageRAW(SEC_MSG: TArray<byte>; mask:Boolean): TArray<byte>;
begin
 Self.PayloadData:=SEC_MSG;
 Result:= Self.MSGAssemble(mask);
end;

function TRFC6455Data.MSGAssemble(mask: Boolean): TArray<byte>;
var l      : UInt64;
  AMask    : TArray<byte>;
  ADataPak : TArray<byte>;
  I        : Integer;
begin
  Result := Result + [$00]; { (FIN, 0 , 0 , 0) 4/4 (opcode) }
  Result := Result + [$00]; { (MASK) 1/7 (Payload len) }
  case Self.OpCode of
    ocText: Result[0] := $1;
    ocBin: Result[0] := $2;
    ocReservData: Result[0] := $3; // ..$7
    ocClose: Result[0] := $8;
    ocPing: Result[0] := $9;
    ocPong: Result[0] := $A;
    ocReservControl: Result[0] := $B; // ..$F
    ocContinued: Result[0] := $0;
  end;
  if Self.Fin then Result[0] := Result[0] or $80;
  Self.PayloadDataLen := Self.PayloadData.Length;
  if ((Self.PayloadDataLen > 125) and (Self.PayloadDataLen > $FFFF)) then begin
    Result[1] := 127;
    Result := Result + [$00, $00, $00, $00, $00, $00, $00, $00]; { UINT 64 }
    Result.setInt64(2,Self.PayloadDataLen);
    Result[0] := Result[0] or $80; { fin }
  end else if ((Self.PayloadDataLen > 125) and (Self.PayloadDataLen <= $FFFF)) then begin
    Result[1] := 126;
    Result := Result + [$00, $00]; { UINT 16 }
    Result.setInt16(2,Word(Self.PayloadDataLen));
    Result[0] := Result[0] or $80; { fin }
  end else begin
    CopyMemory(@Result[1], @Self.PayloadDataLen, 1);
    Result[0] := Result[0] or $80; { fin }
  end;
  ADataPak := Self.PayloadData;
  if mask then begin
    Randomize;
    AMask := [Random(255), Random(255), Random(255), Random(255)];
    for I := Low(ADataPak) to High(ADataPak) do begin
      ADataPak[I] := ADataPak[I] xor AMask[I mod 4]
    end;
    Result[1] := Result[1] OR $80;
    Result := Result + AMask;
  end;
  Result := Result + ADataPak;
end;

function TRFC6455Data.MSGDisassemble(Msg: TArray<byte>): UInt64;
var
  x: byte;
  index, indexMessage, PayloadDataLen: UInt64;
  mask: Array [0 .. 3] of byte;
  Masked: Boolean;
  j: Int64;
begin
  if High(Msg) <= 0 then begin Exit;
  end;
  index := 0;
  Self.PayloadDataLen := 0;
  Self.RAWData := Msg;
  try
    // repeat
    Self.Fin := Boolean(Msg[index] and $80); { Read bit FIN }
    FillChar(mask, SizeOf(mask), $0);
    case Msg[index] and $0F of { Read Opcode }
      $1: Self.OpCode := ocText;
      $2: Self.OpCode := ocBin;
      $3 .. $7: Self.OpCode := ocReservData;
      $8: Self.OpCode := ocClose;
      $9: Self.OpCode := ocPing;
      $A: Self.OpCode := ocPong;
      $B .. $F: Self.OpCode := ocReservControl;
      $0: Self.OpCode := ocContinued;
    end;
    Inc(index);
    Masked := Boolean(Msg[index] and $80); { Read Mask bit }
    x := Msg[index] and $7F; { Read Payload len }
    Inc(index);
    if x = 126 then begin
      Self.PayloadDataLen := UInt64(BSwap16(PWord(Integer(@Msg[0]) + index)^)); { 16Bit Length 2Byte DWORD }
      Inc(index, 2);
    end;
    if x = 127 then begin
      Self.PayloadDataLen := UInt64(BSwap64(PUint64(Integer(@Msg[0]) + index)^)); { 64bit length 8Byte INT64 }
      Inc(index, 8);
    end;
    if x <= 125 then begin
      Self.PayloadDataLen := x; { default length }
    end;
    if Masked then begin { Read INT32 mask KEY }
      mask[0] := Msg[index];
      Inc(index);
      mask[1] := Msg[index];
      Inc(index);
      mask[2] := Msg[index];
      Inc(index);
      mask[3] := Msg[index];
      Inc(index);
    end;
    { Length Message } j := 0; // High(Self.PayloadData);
    if j < 0 then
      j := 0;
    SetLength(Self.PayloadData, j + Self.PayloadDataLen);
    FillChar(Self.PayloadData[0], HIgh(Self.PayloadData), 0);
    while j <= Self.PayloadDataLen do begin
      if j > High(Self.PayloadData) then
        Break;
      if Masked then begin Self.PayloadData[j] := Msg[index] xor mask[j mod 4];
      end else begin Self.PayloadData[j] := Msg[index];
      end;
      Inc(j);
      Inc(index);
    end;
    // until (Self.Fin or (index > High(Msg))) and (Self.OpCode <> ocContinued);
    Result := index;
  except Result := 0;
  end;
  { Neded Test ins function }
end;

class function TRFC6455Data.MSGDisassembles(Msg: TArray<byte>): TArray<TRFC6455Data>;
var
  x: byte;
  index, indexMessage, PayloadDataLen: UInt64;
  mask: Array [0 .. 3] of byte;
  Masked: Boolean;
  j, b, e: Int64;
  item: TRFC6455Data;
begin
  if High(Msg) <= 0 then begin Exit;
  end;
  index := 0;
  try
    repeat
      b := index;
      item.PayloadDataLen := 0;
      item.Fin := Boolean(Msg[index] and $80); { Read bit FIN }
      FillChar(mask, SizeOf(mask), $0);
      case Msg[index] and $0F of { Read Opcode }
        $1: item.OpCode := ocText;
        $2: item.OpCode := ocBin;
        $3 .. $7: item.OpCode := ocReservData;
        $8: item.OpCode := ocClose;
        $9: item.OpCode := ocPing;
        $A: item.OpCode := ocPong;
        $B .. $F: item.OpCode := ocReservControl;
        $0: item.OpCode := ocContinued;
      end;
      Inc(index);
      Masked := Boolean(Msg[index] and $80); { Read Mask bit }
      x := Msg[index] and $7F; { Read Payload len }
      Inc(index);
      if x = 126 then begin
        item.PayloadDataLen := UInt64(BSwap16(PWord(Integer(@Msg[0]) + index)^)); { 16Bit Length 2Byte DWORD }
        Inc(index, 2);
      end;
      if x = 127 then begin
        item.PayloadDataLen := UInt64(BSwap64(PUint64(Integer(@Msg[0]) + index)^)); { 64bit length 8Byte INT64 }
        Inc(index, 8);
      end;
      if x <= 125 then begin
        item.PayloadDataLen := x; { default length }
      end;
      if Masked then begin { Read INT32 mask KEY }
        Inc(index, 4);
      end;
      Inc(index, item.PayloadDataLen);
      e := index;
      if index > Msg.Length then
        Break;
      j := item.MSGDisassemble(Msg.Slice(b, e));
      if j > 0 then
        Result := Result + [item]
      else
        Break;
    until (index > High(Msg));
  except

  end;
end;

end.
