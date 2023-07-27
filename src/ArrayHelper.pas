unit ArrayHelper;

interface

uses Winapi.Windows;

type
  TUint8Array = TArray<byte>;

  Uint8Array = record helper for TUint8Array
    function length: UInt64;
    function toString: String; overload;
    function toString(offset: UInt64): String; overload;
    function toString(offset, length: UInt64): AnsiString; overload;
    function toHex: string;
    function Slice(offset, length: UInt64): TArray<byte>;
    procedure setInt64(offset: UInt64; val: Int64; LitleEndian: Boolean = false);
    procedure setInt32(offset: UInt64; val: Int32; LitleEndian: Boolean = false);
    procedure setInt16(offset: UInt64; val: Int16; LitleEndian: Boolean = false);
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

function Uint8Array.length: UInt64;
begin
  Result := High(Self) + 1;
end;

function Uint8Array.toString: String;
begin
  Result := Self.toString(low(Self), Self.length);
end;

function Uint8Array.toString(offset: UInt64): String;
begin
  Result := Self.toString(offset, Self.length);
end;

procedure Uint8Array.setInt16(offset: UInt64; val: Int16; LitleEndian: Boolean);
var tmp: UInt64;
begin
  if (offset > Self.length) then
    Exit;
  if (offset < low(Self)) then
    Exit;
  if ((offset + SizeOf(val)) > Self.length) then
    Exit;
end;

procedure Uint8Array.setInt32(offset: UInt64; val: Int32; LitleEndian: Boolean);
begin
  if (offset > Self.length) then
    Exit;
  if (offset < low(Self)) then
    Exit;
  if ((offset + SizeOf(val)) > Self.length) then
    Exit;
end;

procedure Uint8Array.setInt64(offset: UInt64; val: Int64; LitleEndian: Boolean);
begin
  if (offset > Self.length) then
    Exit;
  if (offset < low(Self)) then
    Exit;
  if ((offset + SizeOf(val)) > Self.length) then
    Exit;
end;

function Uint8Array.Slice(offset, length: UInt64): TArray<byte>;
begin Result := [];
  if (offset > Self.length) then
    Exit;
  if (offset < low(Self)) then
    Exit;
  if (offset > length) then
    Exit;
  if (length > Self.length) then
    Exit;
  if (length < low(Self)) then
    Exit;
  SetLength(Result, length - offset);
  CopyMemory(@Result[0], Pointer(integer(@Self[0]) + offset), length - offset);
end;

function Uint8Array.toHex: string;
const
  B2HConvert: array [0 .. 15] of byte = ($30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $41, $42, $43, $44, $45, $46);
var
  I, j, count, BufOffset, TextOffset: Int64;
  Text: AnsiString;
begin Result := '';
  if High(Self) <= 0 then
    Exit;
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
begin Result := '';
  if (offset > Self.length) then
    Exit;
  if (offset < low(Self)) then
    Exit;
  if (offset > length) then
    Exit;
  if (length > Self.length) then
    Exit;
  if (length < low(Self)) then
    Exit;
  SetLength(Result, length - offset);
  // Result:=PAnsiChar(@self[0]);
  CopyMemory(@Result[1], Pointer(integer(@Self[0]) + offset), length - offset);
end;

end.
