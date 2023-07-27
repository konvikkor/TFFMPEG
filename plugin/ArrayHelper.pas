unit ArrayHelper;

interface

uses Winapi.Windows, NetEncoding, System.SysUtils, System.Generics.Collections;

type
  TUint8Array = TArray<byte>;

  Uint8Array = record helper for TUint8Array
  private
    function LocalGetLength: UInt64;
    procedure LocalSetLength(const Value: UInt64);
  public
    property length: UInt64 Read LocalGetLength Write LocalSetLength;
    // function length: UInt64;
    procedure appendData(data:pointer;size:UInt64);overload;
    procedure appendData(offset:UInt64;data:pointer;size:UInt64);overload;
    function fill(src: byte): TArray<byte>;
    function toString: String; overload;
    function toString(offset: UInt64): String; overload;
    function toString(offset, length: UInt64): AnsiString; overload;
    function toHex: string;
    function Slice(offset, length: UInt64): TArray<byte>;
    procedure setInt64(offset: UInt64; val: Int64; LitleEndian: Boolean = false);
    procedure setInt32(offset: UInt64; val: Int32; LitleEndian: Boolean = false);
    procedure setInt16(offset: UInt64; val: Int16; LitleEndian: Boolean = false);
    procedure loadBase64(b64: string);
    function saveBase64: String;
  end;

implementation

{ Uint8Array }

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

procedure Uint8Array.appendData(data: pointer; size: UInt64);
begin
  Self.length := size;
  CopyMemory(@Self[0],data,size);
end;

procedure Uint8Array.appendData(offset: UInt64; data: pointer; size: UInt64);
var
  savePosition:UInt64;
  tmpData:TUint8Array;
begin
  if offset > Self.length then Exit;
  if offset < low(Self) then Exit;
  savePosition:=Self.length;
  tmpData:=Self;
  Self.length := savePosition + size;
  CopyMemory(@Self[0],tmpData,tmpData.length);
  CopyMemory(@Self[offset],data,size);
end;

function Uint8Array.fill(src: byte): TArray<byte>;
begin
  FillMemory(@Self[0], Self.length, src);
  Result := Self;
end;

function Uint8Array.toString: String;
begin
  Result := Self.toString(low(Self), Self.length);
end;

function Uint8Array.toString(offset: UInt64): String;
begin
  Result := Self.toString(offset, Self.length);
end;

procedure Uint8Array.loadBase64(b64: string);
var
  tB64: TBase64Encoding;
begin
  tB64 := TBase64Encoding.Create(10, '');
  try
    Self := tB64.DecodeStringToBytes(b64);
  finally
    FreeAndNil(tB64);
  end;
end;

function Uint8Array.LocalGetLength: UInt64;
begin
  Result := High(Self) + 1;
end;

procedure Uint8Array.LocalSetLength(const Value: UInt64);
begin
  SetLength(Self, Value);
end;

function Uint8Array.saveBase64: String;
var
  tB64: TBase64Encoding;
begin
  tB64 := TBase64Encoding.Create(10, '');
  try
    Result := tB64.EncodeBytesToString(Self);
  finally
    FreeAndNil(tB64);
  end;
end;

procedure Uint8Array.setInt16(offset: UInt64; val: Int16; LitleEndian: Boolean);
var
  local16:WordRec;
begin
  if (offset > Self.length) then Exit;
  if (offset < low(Self)) then Exit;
  if ((offset + SizeOf(val)) > Self.length) then Exit;
  CopyMemory(@local16,@val,2);
  if LitleEndian then begin
    Self[offset+0]:=local16.Lo;
    Self[offset+1]:=local16.Hi;
  end else begin
    Self[offset+0]:=local16.Hi;
    Self[offset+1]:=local16.Lo;
  end;
end;

procedure Uint8Array.setInt32(offset: UInt64; val: Int32; LitleEndian: Boolean);
var
  local32:LongRec;
begin
  if (offset > Self.length) then Exit;
  if (offset < low(Self)) then Exit;
  if ((offset + SizeOf(val)) > Self.length) then Exit;
  CopyMemory(@local32,@val,4);
  if LitleEndian then begin
    Self[offset+0]:=local32.Bytes[0];
    Self[offset+1]:=local32.Bytes[1];
    Self[offset+2]:=local32.Bytes[2];
    Self[offset+3]:=local32.Bytes[3];
  end else begin
    Self[offset+0]:=local32.Bytes[3];
    Self[offset+1]:=local32.Bytes[2];
    Self[offset+2]:=local32.Bytes[1];
    Self[offset+3]:=local32.Bytes[0];
  end;
end;

procedure Uint8Array.setInt64(offset: UInt64; val: Int64; LitleEndian: Boolean);
var
  local64:Int64Rec;
begin
  if (offset > Self.length) then Exit;
  if (offset < low(Self)) then Exit;
  if ((offset + SizeOf(val)) > Self.length) then Exit;
  CopyMemory(@local64,@val,8);
  if LitleEndian then begin
    Self[offset+0]:=local64.Bytes[0];
    Self[offset+1]:=local64.Bytes[1];
    Self[offset+2]:=local64.Bytes[2];
    Self[offset+3]:=local64.Bytes[3];
    Self[offset+4]:=local64.Bytes[4];
    Self[offset+5]:=local64.Bytes[5];
    Self[offset+6]:=local64.Bytes[6];
    Self[offset+7]:=local64.Bytes[7];
  end else begin
    Self[offset+0]:=local64.Bytes[7];
    Self[offset+1]:=local64.Bytes[6];
    Self[offset+2]:=local64.Bytes[5];
    Self[offset+3]:=local64.Bytes[4];
    Self[offset+4]:=local64.Bytes[3];
    Self[offset+5]:=local64.Bytes[2];
    Self[offset+6]:=local64.Bytes[1];
    Self[offset+7]:=local64.Bytes[0];
  end;
end;

function Uint8Array.Slice(offset, length: UInt64): TArray<byte>;
begin
  Result := [];
  if (offset > Self.length)or(offset < low(Self)) or (offset > length) or (length > Self.length) or (length < low(Self)) then begin
    Exit;
  end;
  SetLength(Result, length - offset);
  {$ifdef WIN64}
    CopyMemory(@Result[0], Pointer(UInt64(@Self[0]) + offset), length - offset);
  {$else}
    CopyMemory(@Result[0], Pointer(UInt32(@Self[0]) + offset), length - offset);
  {$endif}
end;

function Uint8Array.toHex: string;
const
  B2HConvert: array [0 .. 15] of byte = ($30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $41, $42, $43, $44, $45, $46);
var
  I, j, count, BufOffset, TextOffset: Int64;
  Text: AnsiString;
begin
  Result := '';
  if High(Self) <= 0 then Exit;
  BufOffset := 0;
  SetLength(Text, (Self.length) * 2);
  TextOffset := 0;
  try
    I := 0;
    j := 1;
    repeat
      Text[(TextOffset + j * 2) - 1] := AnsiChar(B2HConvert[Self[BufOffset + I] shr 4]);
      Text[(TextOffset + j * 2 + 1) - 1] := AnsiChar(B2HConvert[Self[BufOffset + I] and $0F]);
      Inc(I);
      Inc(j);
    until (I > Self.length) finally Result := Text;
    SetLength(Text, 0);
  end;
end;

function Uint8Array.toString(offset, length: UInt64): AnsiString;
var l:UInt64;
begin
  Result := '';
  l:=Self.length;
  if (offset > l) then Exit;
  if (offset < low(Self)) then Exit;
  if (offset > length) then Exit;
  if (length > l) then Exit;
  if (length < low(Self)) then Exit;
  SetLength(Result, length - offset);
  {$ifdef WIN64}
    CopyMemory(@Result[1], Pointer(uint64(@Self[0]) + offset), length - offset);
  {$else}
    CopyMemory(@Result[1], Pointer(UInt32(@Self[0]) + offset), length - offset);
  {$endif}
end;

end.
