unit ffmpeg_types;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

Type
  Bool = WordBool;
  float = Single;
  ppDouble = ^pDouble;

  size_t = NativeUInt;
  psize_t = ^size_t;
  ptrdiff_t = UInt32;
  uint32_t = Cardinal;
  unsigned = uint32_t;
  unsignedint = UInt32;
  UINT = unsigned;
  unsigned_int = UInt32;
  punsigned_int = ^unsigned_int;
  unsigned_long = Cardinal;

  unsignedchar = Byte;
  unsigned_char = unsignedchar;
  punsignedchar = PByte; // ^unsignedchar;
  punsigned_char = punsignedchar;

  Int = Integer;
  pint = ^Int;
  ppint = ^pint;

  int8_t = Int8;
  pint8_t = ^int8_t;

  uint8_t = Byte;
  puint8_t = PByte; // ^uint8_t;
  ppuint8_t = ^puint8_t;
  PPByte = ppuint8_t;

  int16_t = int16;
  pint16_t = ^int16_t;
  uint16_t = UInt16;
  puint16_t = ^uint16_t;

  int32_t = Int32;
  pint32_t = ^int32_t;
  ppint32_t = ^pint32_t;

  int64_t = Int64;
  pint64_t = ^int64_t;
  uint64_t = UInt64;
  puint64_t = ^uint64_t;

  array_uint8_t = array [0 .. 0] of uint8_t;
  parray_uint8_t = ^array_uint8_t;

  array_int = array [0 .. 0] of Int;
  parray_int = ^array_int;

  array4_int = array [0 .. 3] of Int;
  parray4_int = ^array4_int;

  array4_puint8_t = array [0 .. 3] of puint8_t;
  parray4_puint8_t = ^array4_puint8_t;

  array4_ptrdiff_t = array [0 .. 3] of ptrdiff_t;
  parray4_ptrdiff_t = ^array4_ptrdiff_t;

  time_t = LongInt;

  AnsiCharArray = array [0 .. 0] of pAnsiChar;
  pAnsiCharArray = ^AnsiCharArray;

  (* MICROSOFT VC++ STDIO'S FILE DEFINITION *)
  _iobuf = record
    _ptr: pAnsiChar;
    _cnt: Integer;
    _base: pAnsiChar;
    _flag: Integer;
    _file: Integer;
    _charbuf: Integer;
    _bufsiz: Integer;
    _tmpfname: pAnsiChar;
  end;

  PFile = ^TFile;
  TFile = _iobuf;

  pAVHWAccel = Pointer;
  ppAVCodecHWConfigInternal = Pointer;

const
  max_unsigned = $FFFF;

implementation

end.
