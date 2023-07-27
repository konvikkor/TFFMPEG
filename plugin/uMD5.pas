////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Модуль       : uMD5
//  * Назначение   : Класс для вычисления MD5 хэшей
//  * Автор        : Дмитрий Муратов
//  * Версия       : 1.2
//  * Дата         : 07.12.2012
//  * Примечания   : -
//  ****************************************************************************
//

//{$I DEFINES.INC}
{$DEFINE PUREPASCAL}

unit uMD5;

{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
  {$IFDEF DELPHIXE2_UP}
  System.SysUtils, System.Classes;
  {$ELSE}
  SysUtils, Classes;
  {$ENDIF}

resourcestring

  rsBadMD5StrLen = 'Can''t cast string of length %d to TMD5Digest';
  rsBadMD5StrChars = 'Can''t cast string %s to TMD5Digest: invalid hex digits';

type

  UINT4 = LongWord;

  PArray4UINT4 = ^TArray4UINT4;
  TArray4UINT4 = array[0..3] of UINT4;
  PArray2UINT4 = ^TArray2UINT4;
  TArray2UINT4 = array[0..1] of UINT4;
  PArray16Byte = ^TArray16Byte;
  TArray16Byte = array[0..15] of Byte;
  PArray64Byte = ^TArray64Byte;
  TArray64Byte = array[0..63] of Byte;

  PByteArray = ^TByteArray;
  TByteArray = array[0..0] of Byte;

  PUINT4Array = ^TUINT4Array;
  TUINT4Array = array[0..0] of UINT4;

  PMD5Context = ^TMD5Context;
  TMD5Context = record
    state: TArray4UINT4;
    count: TArray2UINT4;
    buffer: TArray64Byte;
  end;

  EMD5 = class(Exception);

  TMD5Digest = record
    case Integer of
      0: (Bytes: packed array[0..15] of Byte);
      1: (LongWords: packed array[0..3] of LongWord);
      2: (A, B, C, D: LongWord);
  end;

  TMD5 = class(TObject)
  public
    {$IFDEF UNICODE}
    class function Calculate(const X: TBytes): TMD5Digest; overload;
    class function Calculate(const S: RawByteString): TMD5Digest; overload;
    class function Calculate(const S: UnicodeString; const Encoding: TEncoding): TMD5Digest; overload;
    class function Calculate(const S: UnicodeString): TMD5Digest; overload;
    {$ELSE}
    class function Calculate(const S: string): TMD5Digest; overload;
    {$ENDIF}
    class function Calculate(const Stream: TStream): TMD5Digest; overload;
    class function Calculate(const Buffer; Size: LongWord): TMD5Digest; overload;
    class function CalculateFile(const FileName: string): TMD5Digest;
    class function DigestToStr(const Digest: TMD5Digest): string;
    class function StrToDigest(const S: string): TMD5Digest;
    class function DigestCompare(const Digest1, Digest2: TMD5Digest): Boolean;
  end;

procedure MD5Init(var AContext: TMD5Context);
procedure MD5Update(var AContext: TMD5Context; ABuffer: PByteArray; ABufferLen: LongWord);
procedure MD5Final(var AContext: TMD5Context; var ADigest: TMD5Digest);

implementation

const
  S11 = 7;
  S12 = 12;
  S13 = 17;
  S14 = 22;
  S21 = 5;
  S22 = 9;
  S23 = 14;
  S24 = 20;
  S31 = 4;
  S32 = 11;
  S33 = 16;
  S34 = 23;
  S41 = 6;
  S42 = 10;
  S43 = 15;
  S44 = 21;

var
  Padding: TArray64Byte =
  ($80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

//------------------------------------------------------------------------------

function _F(const x, y, z: UINT4): UINT4; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Result := (((x) and (y)) or ((not x) and (z)));
end;

//------------------------------------------------------------------------------

function _G(const x, y, z: UINT4): UINT4; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Result := (((x) and (z)) or ((y) and (not z)));
end;

//------------------------------------------------------------------------------

function _H(const x, y, z: UINT4): UINT4; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Result := ((x) xor (y) xor (z));
end;

//------------------------------------------------------------------------------

function _I(const x, y, z: UINT4): UINT4; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  Result := ((y) xor ((x) or (not z)));
end;

//------------------------------------------------------------------------------

function ROTATE_LEFT(const x, n: UINT4): UINT4;{$IFDEF PUREPASCAL}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}{$ENDIF}
{$IFDEF PUREPASCAL}
begin
  Result := (x shl n) or x shr (32 - n);
end;
{$ELSE ~PUREPASCAL}
asm
  {$IFDEF CPU32}
  // --> EAX x
  //     DL  n
  // <-- EAX Result
  MOV    CL,  DL
  ROL    EAX, CL
  {$ENDIF CPU32}
  {$IFDEF CPU64}
  // --> ECX x
  //     DL  n
  // <-- EAX Result
  MOV    EAX, ECX
  MOV    CL,  DL
  ROL    EAX, CL
  {$ENDIF CPU64}
end;
{$ENDIF ~PUREPASCAL}

//------------------------------------------------------------------------------

procedure FF(var a: UINT4; const b, c, d, x, s, ac: UINT4); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  a := a + _F(b, c, d) + x + ac;
  a := ROTATE_LEFT(a, s);
  a := a + b;
end;

//------------------------------------------------------------------------------

procedure GG(var a: UINT4; const b, c, d, x, s, ac: UINT4); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  a := a + _G(b, c, d) + x + ac;
  a := ROTATE_LEFT(a, s);
  a := a + b;
end;

//------------------------------------------------------------------------------

procedure HH(var a: UINT4; const b, c, d, x, s, ac: UINT4); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  a := a + _H(b, c, d) + x + ac;
  a := ROTATE_LEFT(a, s);
  a := a + b;
end;

//------------------------------------------------------------------------------

procedure II(var a: UINT4; const b, c, d, x, s, ac: UINT4); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  a := a + _I(b, c, d) + x + ac;
  a := ROTATE_LEFT(a, s);
  a := a + b;
end;

//------------------------------------------------------------------------------

procedure MD5Encode(Output: PByteArray; Input: PUINT4Array; Len: LongWord);
var
  i, j: LongWord;
begin
  j := 0;
  i := 0;
  while j < Len do
  begin
    Output^[j] := Byte(input^[i] and $FF);
    Output^[j + 1] := Byte((input^[i] shr 8) and $FF);
    Output^[j + 2] := Byte((input^[i] shr 16) and $FF);
    Output^[j + 3] := Byte((input^[i] shr 24) and $FF);
    Inc(j, 4);
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

procedure MD5Decode(Output: PUINT4Array; Input: PByteArray; Len: LongWord);
var
  i, j: LongWord;
begin
  j := 0;
  i := 0;
  while j < Len do
  begin
    Output^[i] := UINT4(input^[j]) or (UINT4(input^[j + 1]) shl 8) or
      (UINT4(input^[j + 2]) shl 16) or (UINT4(input^[j + 3]) shl 24);
    Inc(j, 4);
    Inc(i);
  end;
end;

//------------------------------------------------------------------------------

procedure MD5Transform(State: PArray4UINT4; Buffer: PArray64Byte);
var
  a, b, c, d: UINT4;
  x: array[0..15] of UINT4;
begin
  a := State^[0];
  b := State^[1];
  c := State^[2];
  d := State^[3];
  MD5Decode(PUINT4Array(@x), PByteArray(Buffer), 64);

  FF(a, b, c, d, x[0], S11, $D76AA478);
  FF(d, a, b, c, x[1], S12, $E8C7B756);
  FF(c, d, a, b, x[2], S13, $242070DB);
  FF(b, c, d, a, x[3], S14, $C1BDCEEE);
  FF(a, b, c, d, x[4], S11, $F57C0FAF);
  FF(d, a, b, c, x[5], S12, $4787C62A);
  FF(c, d, a, b, x[6], S13, $A8304613);
  FF(b, c, d, a, x[7], S14, $FD469501);
  FF(a, b, c, d, x[8], S11, $698098D8);
  FF(d, a, b, c, x[9], S12, $8B44F7AF);
  FF(c, d, a, b, x[10], S13, $FFFF5BB1);
  FF(b, c, d, a, x[11], S14, $895CD7BE);
  FF(a, b, c, d, x[12], S11, $6B901122);
  FF(d, a, b, c, x[13], S12, $FD987193);
  FF(c, d, a, b, x[14], S13, $A679438E);
  FF(b, c, d, a, x[15], S14, $49B40821);

  GG(a, b, c, d, x[1], S21, $F61E2562);
  GG(d, a, b, c, x[6], S22, $C040B340);
  GG(c, d, a, b, x[11], S23, $265E5A51);
  GG(b, c, d, a, x[0], S24, $E9B6C7AA);
  GG(a, b, c, d, x[5], S21, $D62F105D);
  GG(d, a, b, c, x[10], S22, $2441453);
  GG(c, d, a, b, x[15], S23, $D8A1E681);
  GG(b, c, d, a, x[4], S24, $E7D3FBC8);
  GG(a, b, c, d, x[9], S21, $21E1CDE6);
  GG(d, a, b, c, x[14], S22, $C33707D6);
  GG(c, d, a, b, x[3], S23, $F4D50D87);

  GG(b, c, d, a, x[8], S24, $455A14ED);
  GG(a, b, c, d, x[13], S21, $A9E3E905);
  GG(d, a, b, c, x[2], S22, $FCEFA3F8);
  GG(c, d, a, b, x[7], S23, $676F02D9);
  GG(b, c, d, a, x[12], S24, $8D2A4C8A);

  HH(a, b, c, d, x[5], S31, $FFFA3942);
  HH(d, a, b, c, x[8], S32, $8771F681);
  HH(c, d, a, b, x[11], S33, $6D9D6122);
  HH(b, c, d, a, x[14], S34, $FDE5380C);
  HH(a, b, c, d, x[1], S31, $A4BEEA44);
  HH(d, a, b, c, x[4], S32, $4BDECFA9);
  HH(c, d, a, b, x[7], S33, $F6BB4B60);
  HH(b, c, d, a, x[10], S34, $BEBFBC70);
  HH(a, b, c, d, x[13], S31, $289B7EC6);
  HH(d, a, b, c, x[0], S32, $EAA127FA);
  HH(c, d, a, b, x[3], S33, $D4EF3085);
  HH(b, c, d, a, x[6], S34, $4881D05);
  HH(a, b, c, d, x[9], S31, $D9D4D039);
  HH(d, a, b, c, x[12], S32, $E6DB99E5);
  HH(c, d, a, b, x[15], S33, $1FA27CF8);
  HH(b, c, d, a, x[2], S34, $C4AC5665);

  II(a, b, c, d, x[0], S41, $F4292244);
  II(d, a, b, c, x[7], S42, $432AFF97);
  II(c, d, a, b, x[14], S43, $AB9423A7);
  II(b, c, d, a, x[5], S44, $FC93A039);
  II(a, b, c, d, x[12], S41, $655B59C3);
  II(d, a, b, c, x[3], S42, $8F0CCC92);
  II(c, d, a, b, x[10], S43, $FFEFF47D);
  II(b, c, d, a, x[1], S44, $85845DD1);
  II(a, b, c, d, x[8], S41, $6FA87E4F);
  II(d, a, b, c, x[15], S42, $FE2CE6E0);
  II(c, d, a, b, x[6], S43, $A3014314);
  II(b, c, d, a, x[13], S44, $4E0811A1);
  II(a, b, c, d, x[4], S41, $F7537E82);
  II(d, a, b, c, x[11], S42, $BD3AF235);
  II(c, d, a, b, x[2], S43, $2AD7D2BB);
  II(b, c, d, a, x[9], S44, $EB86D391);

  Inc(State^[0], a);
  Inc(State^[1], b);
  Inc(State^[2], c);
  Inc(State^[3], d);
end;

//------------------------------------------------------------------------------

procedure MD5Init(var AContext: TMD5Context);
begin
  FillChar(AContext, SizeOf(AContext), 0);
  AContext.State[0] := $67452301;
  AContext.State[1] := $EFCDAB89;
  AContext.State[2] := $98BADCFE;
  AContext.State[3] := $10325476;
end;

//------------------------------------------------------------------------------

procedure MD5Update(var AContext: TMD5Context; ABuffer: PByteArray; ABufferLen: LongWord);
var
  i, Index, PartLen: LongWord;
begin
  Index := LongWord((AContext.Count[0] shr 3) and $3F);
  Inc(AContext.Count[0], UINT4(ABufferLen) shl 3);
  if AContext.Count[0] < UINT4(ABufferLen) shl 3 then
    Inc(AContext.Count[1]);
  Inc(AContext.Count[1], UINT4(ABufferLen) shr 29);
  PartLen := 64 - Index;
  if ABufferLen >= partLen then
  begin
    Move(ABuffer^, AContext.Buffer[index], PartLen);
    MD5Transform(@AContext.State, @AContext.Buffer);
    i := PartLen;
    while i + 63 < ABufferLen do
    begin
      MD5Transform(@AContext.State, PArray64Byte(@ABuffer[i]));
      Inc(i, 64);
    end;
    index := 0;
  end
  else
    i := 0;
  Move(ABuffer[i], AContext.Buffer[index], ABufferLen - i);
end;

//------------------------------------------------------------------------------

procedure MD5Final(var AContext: TMD5Context; var ADigest: TMD5Digest);
var
  Bits: array[0..7] of Byte;
  Index, PadLen: LongWord;
begin
  MD5Encode(PByteArray(@bits), PUINT4Array(@AContext.Count), 8);
  Index := LongWord((AContext.Count[0] shr 3) and $3F);
  if Index < 56 then
    PadLen := 56 - index
  else
    PadLen := 120 - index;
  MD5Update(AContext, PByteArray(@PADDING), padLen);
  MD5Update(AContext, PByteArray(@Bits), 8);
  MD5Encode(PByteArray(@ADigest), PUINT4Array(@AContext.state), 16);
  FillChar(AContext, SizeOf(AContext), 0);
end;

////////////////////////////////////////////////////////////////////////////////
// TMD5
////////////////////////////////////////////////////////////////////////////////

class function TMD5.DigestToStr(const Digest: TMD5Digest): string;
var
  i: Integer;
const
  Digits: array[0..15] of AnsiChar = (
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  );
begin
  Result := '';
  for I := 0 to 15 do
    Result := Result + string(Digits[(Digest.Bytes[i] shr 4) and $0f] +
      Digits[Digest.Bytes[i] and $0f]);
end;

//------------------------------------------------------------------------------

class function TMD5.StrToDigest(const S: string): TMD5Digest;
var
  Idx: Integer;
  B: Integer;
  ByteStr: string;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(S) <> 2 * Length(Result.Bytes) then
    raise EMD5.CreateFmt(rsBadMD5StrLen, [Length(S)]);
  for Idx := 0 to Pred(Length(Result.Bytes)) do
  begin
    ByteStr := S[2 * Idx + 1] + S[2 * Idx + 2];
    if not TryStrToInt('$' + ByteStr, B) then
      raise EMD5.CreateFmt(rsBadMD5StrChars, [S]);
    Result.Bytes[Idx] := Byte(B);
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF UNICODE}

class function TMD5.Calculate(const X: TBytes): TMD5Digest;
begin
  Result := Calculate(Pointer(X)^, Length(X));
end;

//------------------------------------------------------------------------------

class function TMD5.Calculate(const S: RawByteString): TMD5Digest;
begin
  Result := Calculate(Pointer(S)^, Length(S) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

class function TMD5.Calculate(const S: UnicodeString; const Encoding: TEncoding): TMD5Digest;
begin
  Result := Calculate(Encoding.GetBytes(S));
end;

//------------------------------------------------------------------------------

class function TMD5.Calculate(const S: UnicodeString): TMD5Digest;
begin
  Result := Calculate(S, TEncoding.Default);
end;

{$ELSE}

class function TMD5.Calculate(const S: string): TMD5Digest;
begin
  Result := Calculate(Pointer(S)^, Length(S));
end;

{$ENDIF}

//------------------------------------------------------------------------------

class function TMD5.CalculateFile(const FileName: string): TMD5Digest;
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := Calculate(F);
  finally
    F.Free;
  end;
end;

//------------------------------------------------------------------------------

class function TMD5.Calculate(const Stream: TStream): TMD5Digest;
var
  Context: TMD5Context;
  Buffer: array[0..4095] of Byte;
  Size: Int64;
  ReadBytes: Integer;
  TotalBytes: Int64;
  SavePos: Int64;
begin
  MD5Init(Context);
  Size := Stream.Size;
  SavePos := Stream.Position;
  TotalBytes := 0;
  try
    Stream.Seek(0, soBeginning);
    repeat
      ReadBytes := Stream.Read(Buffer, SizeOf(Buffer));
      Inc(TotalBytes, ReadBytes);
      MD5Update(Context, @Buffer, ReadBytes);
    until (ReadBytes = 0) or (TotalBytes = Size);
  finally
    Stream.Seek(SavePos, soBeginning);
  end;
  MD5Final(Context, Result);
end;

//------------------------------------------------------------------------------

class function TMD5.Calculate(const Buffer; Size: LongWord): TMD5Digest;
var
  Context: TMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PByteArray(@Buffer), Size);
  MD5Final(Context, Result);
end;

//------------------------------------------------------------------------------

class function TMD5.DigestCompare(const Digest1, Digest2: TMD5Digest): Boolean;
begin
  Result := False;
  if Digest1.A <> Digest2.A then
    Exit;
  if Digest1.B <> Digest2.B then
    Exit;
  if Digest1.C <> Digest2.C then
    Exit;
  if Digest1.D <> Digest2.D then
    Exit;
  Result := True;
end;

//------------------------------------------------------------------------------

{$IFDEF DEBUG}
procedure SelfTestModule;
begin
  Assert(TMD5.DigestToStr(TMD5.Calculate('')) = 'd41d8cd98f00b204e9800998ecf8427e', 'TMD5 test 1 failed');
  Assert(TMD5.DigestToStr(TMD5.Calculate('a')) = '0cc175b9c0f1b6a831c399e269772661', 'TMD5 test 2 failed');
  Assert(TMD5.DigestToStr(TMD5.Calculate('abc')) = '900150983cd24fb0d6963f7d28e17f72', 'TMD5 test 3 failed');
  Assert(TMD5.DigestToStr(TMD5.Calculate('message digest')) = 'f96b697d7cb7938d525a2f31aaf161d0', 'TMD5 test 4 failed');
  Assert(TMD5.DigestToStr(TMD5.Calculate('abcdefghijklmnopqrstuvwxyz')) = 'c3fcd3d76192e4007dfb496cca67e13b', 'TMD5 test 5 failed');
  Assert(TMD5.DigestToStr(TMD5.Calculate('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789')) = 'd174ab98d277d9f5a5611c2c9f419d9f', 'TMD5 test 6 failed');
  Assert(TMD5.DigestToStr(TMD5.Calculate('12345678901234567890123456789012345678901234567890123456789012345678901234567890')) = '57edf4a22be3c955ac49da2e2107b67a', 'TMD5 test 7 failed');
end;
{$ENDIF}

initialization

{$IFDEF DEBUG}
  SelfTestModule;
{$ENDIF}

end.
