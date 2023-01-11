unit libavutil;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

Uses
  ffmpeg_types;

{$I ffmpeg.inc}
{$REGION 'avconfig.h'}

const
  AV_HAVE_BIGENDIAN      = 0;
  AV_HAVE_FAST_UNALIGNED = 1;
{$ENDREGION}
{$REGION 'common.h'}
  // rounded division & shift
  // #define RSHIFT(a,b) ((a) > 0 ? ((a) + ((1<<(b))>>1))>>(b) : ((a) + ((1<<(b))>>1)-1)>>(b))
function RSHIFT(a, b: int): int; inline;
/// * assume b>0 */
// #define ROUNDED_DIV(a,b) (((a)>0 ? (a) + ((b)>>1) : (a) - ((b)>>1))/(b))
function ROUNDED_DIV(a, b: int): int; inline;
// (* Fast a/(1<<b) rounded toward +inf. Assume a>=0 and b>=0 *)
// #define AV_CEIL_RSHIFT(a,b) (!av_builtin_constant_p(b) ? -((-(a)) >> (b)) : ((a) + (1<<(b)) - 1) >> (b))
/// * Backwards compat. */
// #define FF_CEIL_RSHIFT AV_CEIL_RSHIFT

// #define FFUDIV(a,b) (((a)>0 ?(a):(a)-(b)+1) / (b))
function FFUDIV(a, b: int): int; inline;
// #define FFUMOD(a,b) ((a)-(b)*FFUDIV(a,b))
function FFUMOD(a, b: int): int; inline;

(* *
  * Absolute value, Note, INT_MIN / INT64_MIN result in undefined behavior as they
  * are not representable as absolute values of their type. This is the same
  * as with *abs()
  * @see FFNABS()
*)
// #define FFABS(a) ((a) >= 0 ? (a) : (-(a)))
function FFABS(a: int): int; inline;
// #define FFSIGN(a) ((a) > 0 ? 1 : -1)
function FFSIGN(a: int): int; inline;

(* *
  * Negative Absolute value.
  * this works for all integers of all types.
  * As with many macros, this evaluates its argument twice, it thus must not have
  * a sideeffect, that is FFNABS(x++) has undefined behavior.
*)
// #define FFNABS(a) ((a) <= 0 ? (a) : (-(a)))
function FFNABS(a: int): int; inline;

(* *
  * Comparator.
  * For two numerical expressions x and y, gives 1 if x > y, -1 if x < y, and 0
  * if x == y. This is useful for instance in a qsort comparator callback.
  * Furthermore, compilers are able to optimize this to branchless code, and
  * there is no risk of overflow with signed types.
  * As with many macros, this evaluates its argument multiple times, it thus
  * must not have a side-effect.
*)
// #define FFDIFFSIGN(x,y) (((x)>(y)) - ((x)<(y)))
function FFDIFFSIGN(x, y: int): Boolean; inline;

// #define FFMAX(a,b) ((a) > (b) ? (a) : (b))
function FFMAX(a, b: int): int; inline;

// #define FFMAX3(a,b,c) FFMAX(FFMAX(a,b),c)
// #define FFMIN(a,b) ((a) > (b) ? (b) : (a))
// #define FFMIN3(a,b,c) FFMIN(FFMIN(a,b),c)

// #define FFSWAP(type,a,b) do{type SWAP_tmp= b; b= a; a= SWAP_tmp;}while(0)
// #define FF_ARRAY_ELEMS(a) (sizeof(a) / sizeof((a)[0]))

(* misc math functions *)

// #ifndef av_log2
// av_const int av_log2(unsigned v);
// #endif
function av_log2(v: unsigned): int; cdecl; external avutil_dll;

// #ifndef av_log2_16bit
// av_const int av_log2_16bit(unsigned v);
// #endif
function av_log2_16bit(v: unsigned): int; cdecl; external avutil_dll;

(* *
  * Clip a signed integer value into the amin-amax range.
  * @param a value to clip
  * @param amin minimum value of the clip range
  * @param amax maximum value of the clip range
  * @return clipped value
*)
// static av_always_inline av_const int av_clip_c(int a, int amin, int amax)
function av_clip_c(a: int; amin: int; amax: int): int; inline;

(* *
  * Clip a signed 64bit integer value into the amin-amax range.
  * @param a value to clip
  * @param amin minimum value of the clip range
  * @param amax maximum value of the clip range
  * @return clipped value
*)
// static av_always_inline av_const int64_t av_clip64_c(int64_t a, int64_t amin, int64_t amax)
function av_clip64_c(a: int64_t; amin: int64_t; amax: int64_t): int64_t; inline;

(* *
  * Clip a signed integer value into the 0-255 range.
  * @param a value to clip
  * @return clipped value
*)
// static av_always_inline av_const uint8_t av_clip_uint8_c(int a)
function av_clip_uint8_c(a: int): uint8_t; inline;

(* *
  * Clip a signed integer value into the -128,127 range.
  * @param a value to clip
  * @return clipped value
*)
// static av_always_inline av_const int8_t av_clip_int8_c(int a)
function av_clip_int8_c(a: int): int8_t; inline;

(* *
  * Clip a signed integer value into the 0-65535 range.
  * @param a value to clip
  * @return clipped value
*)
// static av_always_inline av_const uint16_t av_clip_uint16_c(int a)
function av_clip_uint16_c(a: int): uint16_t; inline;

(* *
  * Clip a signed integer value into the -32768,32767 range.
  * @param a value to clip
  * @return clipped value
*)
// static av_always_inline av_const int16_t av_clip_int16_c(int a)
function av_clip_int16_c(a: int): int16_t; inline;

(* *
  * Clip a signed 64-bit integer value into the -2147483648,2147483647 range.
  * @param a value to clip
  * @return clipped value
*)
// static av_always_inline av_const int32_t av_clipl_int32_c(int64_t a)
function av_clipl_int32_c(a: int64_t): int32_t; inline;

(* *
  * Clip a signed integer into the -(2^p),(2^p-1) range.
  * @param  a value to clip
  * @param  p bit position to clip at
  * @return clipped value
*)
// static av_always_inline av_const int av_clip_intp2_c(int a, int p)
function av_clip_intp2_c(a: int; p: int): int; inline;

(* *
  * Clip a signed integer to an unsigned power of two range.
  * @param  a value to clip
  * @param  p bit position to clip at
  * @return clipped value
*)
// static av_always_inline av_const unsigned av_clip_uintp2_c(int a, int p)
function av_clip_uintp2_c(a, p: int): unsigned; inline;

(* *
  * Clear high bits from an unsigned integer starting with specific bit position
  * @param  a value to clip
  * @param  p bit position to clip at
  * @return clipped value
*)
// static av_always_inline av_const unsigned av_mod_uintp2_c(unsigned a, unsigned p)
function av_mod_uintp2_c(a, p: unsigned): unsigned; inline;

(* *
  * Add two signed 32-bit values with saturation.
  *
  * @param  a one value
  * @param  b another value
  * @return sum with signed saturation
*)
// static av_always_inline int av_sat_add32_c(int a, int b)
function av_sat_add32_c(a, b: int): int; inline;

(* *
  * Add a doubled value to another value with saturation at both stages.
  *
  * @param  a first value
  * @param  b value doubled and added to a
  * @return sum sat(a + sat(2*b)) with signed saturation
*)
// static av_always_inline int av_sat_dadd32_c(int a, int b)
function av_sat_dadd32_c(a, b: int): int; inline;

(* *
  * Subtract two signed 32-bit values with saturation.
  *
  * @param  a one value
  * @param  b another value
  * @return difference with signed saturation
*)
// static av_always_inline int av_sat_sub32_c(int a, int b)
function av_sat_sub32_c(a, b: int): int; inline;

(* *
  * Subtract a doubled value from another value with saturation at both stages.
  *
  * @param  a first value
  * @param  b value doubled and subtracted from a
  * @return difference sat(a - sat(2*b)) with signed saturation
*)
// static av_always_inline int av_sat_dsub32_c(int a, int b)
function av_sat_dsub32_c(a, b: int): int; inline;

(* *
  * Clip a float value into the amin-amax range.
  * @param a value to clip
  * @param amin minimum value of the clip range
  * @param amax maximum value of the clip range
  * @return clipped value
*)
// static av_always_inline av_const float av_clipf_c(float a, float amin, float amax)
function av_clipf_c(a, amin, amax: float): float; inline;

(* *
  * Clip a double value into the amin-amax range.
  * @param a value to clip
  * @param amin minimum value of the clip range
  * @param amax maximum value of the clip range
  * @return clipped value
*)
// static av_always_inline av_const double av_clipd_c(double a, double amin, double amax)
function av_clipd_c(a, amin, amax: double): double; inline;

(* * Compute ceil(log2(x)).
  * @param x value used to compute ceil(log2(x))
  * @return computed ceiling of log2(x)
*)
// static av_always_inline av_const int av_ceil_log2_c(int x)
function av_ceil_log2_c(x: int): int; inline;

(* *
  * Count number of bits set to one in x
  * @param x value to count bits of
  * @return the number of bits set to one in x
*)
// static av_always_inline av_const int av_popcount_c(uint32_t x)
function av_popcount_c(x: uint32_t): int; inline;

// static av_always_inline av_const int av_parity_c(uint32_t v)
function av_parity_c(v: uint32_t): int; inline;

(* *
  * Count number of bits set to one in x
  * @param x value to count bits of
  * @return the number of bits set to one in x
*)
// static av_always_inline av_const int av_popcount64_c(uint64_t x)
function av_popcount64_c(x: uint64_t): int; inline;

{$ENDREGION}
{$REGION 'bprint.h'}

(* *
  * Buffer to print data progressively
  *
  * The string buffer grows as necessary and is always 0-terminated.
  * The content of the string is never accessed, and thus is
  * encoding-agnostic and can even hold binary data.
  *
  * Small buffers are kept in the structure itself, and thus require no
  * memory allocation at all (unless the contents of the buffer is needed
  * after the structure goes out of scope). This is almost as lightweight as
  * declaring a local "char buf[512]".
  *
  * The length of the string can go beyond the allocated size: the buffer is
  * then truncated, but the functions still keep account of the actual total
  * length.
  *
  * In other words, buf->len can be greater than buf->size and records the
  * total length of what would have been to the buffer if there had been
  * enough memory.
  *
  * Append operations do not need to be tested for failure: if a memory
  * allocation fails, data stop being appended to the buffer, but the length
  * is still updated. This situation can be tested with
  * av_bprint_is_complete().
  *
  * The size_max field determines several possible behaviours:
  *
  * size_max = -1 (= UINT_MAX) or any large value will let the buffer be
  * reallocated as necessary, with an amortized linear cost.
  *
  * size_max = 0 prevents writing anything to the buffer: only the total
  * length is computed. The write operations can then possibly be repeated in
  * a buffer with exactly the necessary size
  * (using size_init = size_max = len + 1).
  *
  * size_max = 1 is automatically replaced by the exact size available in the
  * structure itself, thus ensuring no dynamic memory allocation. The
  * internal buffer is large enough to hold a reasonable paragraph of text,
  * such as the current paragraph.
*)
type
  // FF_PAD_STRUCTURE(AVBPrint, 1024,
  // char *str;         (**< string so far *)
  // unsigned len;      (**< length so far *)
  // unsigned size;     (**< allocated memory *)
  // unsigned size_max; (**< maximum allocated memory *)
  // char reserved_internal_buffer[1];
  // )

  FF_PAD_STRUCTURE_AVBPrint = record
    str: PAnsiChar;     (* *< string so far *)
    len: Cardinal;      (* *< length so far *)
    size: Cardinal;     (* *< allocated memory *)
    size_max: Cardinal; (* *< maximum allocated memory *)
    reserved_internal_buffer: array [0 .. 0] of AnsiChar;
  end;

  pAVBPrint = ^AVBPrint;

  AVBPrint = record
    str: PAnsiChar;     (* *< string so far *)
    len: Cardinal;      (* *< length so far *)
    size: Cardinal;     (* *< allocated memory *)
    size_max: Cardinal; (* *< maximum allocated memory *)
    reserved_internal_buffer: array [0 .. 0] of AnsiChar;
    reserved_padding: array [0 .. 1024 - SizeOf(FF_PAD_STRUCTURE_AVBPrint) - 1] of AnsiChar;
  end;
{$ENDREGION}
{$REGION 'channel_layout.h'}

const
  (* *
    * @defgroup channel_masks Audio channel masks
    *
    * A channel layout is a 64-bits integer with a bit set for every channel.
    * The number of bits set must be equal to the number of channels.
    * The value 0 means that the channel layout is not known.
    * @note this data structure is not powerful enough to handle channels
    * combinations that have the same channel multiple times, such as
    * dual-mono.
    *
    * @{
  *)
  AV_CH_FRONT_LEFT            = $00000001;
  AV_CH_FRONT_RIGHT           = $00000002;
  AV_CH_FRONT_CENTER          = $00000004;
  AV_CH_LOW_FREQUENCY         = $00000008;
  AV_CH_BACK_LEFT             = $00000010;
  AV_CH_BACK_RIGHT            = $00000020;
  AV_CH_FRONT_LEFT_OF_CENTER  = $00000040;
  AV_CH_FRONT_RIGHT_OF_CENTER = $00000080;
  AV_CH_BACK_CENTER           = $00000100;
  AV_CH_SIDE_LEFT             = $00000200;
  AV_CH_SIDE_RIGHT            = $00000400;
  AV_CH_TOP_CENTER            = $00000800;
  AV_CH_TOP_FRONT_LEFT        = $00001000;
  AV_CH_TOP_FRONT_CENTER      = $00002000;
  AV_CH_TOP_FRONT_RIGHT       = $00004000;
  AV_CH_TOP_BACK_LEFT         = $00008000;
  AV_CH_TOP_BACK_CENTER       = $00010000;
  AV_CH_TOP_BACK_RIGHT        = $00020000;
  AV_CH_STEREO_LEFT           = $20000000;
  /// < Stereo downmix.
  AV_CH_STEREO_RIGHT = $40000000;
  /// < See AV_CH_STEREO_LEFT.
  AV_CH_WIDE_LEFT             = $0000000080000000;
  AV_CH_WIDE_RIGHT            = $0000000100000000;
  AV_CH_SURROUND_DIRECT_LEFT  = $0000000200000000;
  AV_CH_SURROUND_DIRECT_RIGHT = $0000000400000000;
  AV_CH_LOW_FREQUENCY_2       = $0000000800000000;

  (* * Channel mask value used for AVCodecContext.request_channel_layout
    to indicate that the user requests the channel order of the decoder output
    to be the native codec channel order. *)
  AV_CH_LAYOUT_NATIVE = $8000000000000000;

  (* *
    * @}
    * @defgroup channel_mask_c Audio channel layouts
    * @{
    * *)
  AV_CH_LAYOUT_MONO              = (AV_CH_FRONT_CENTER);
  AV_CH_LAYOUT_STEREO            = (AV_CH_FRONT_LEFT or AV_CH_FRONT_RIGHT);
  AV_CH_LAYOUT_2POINT1           = (AV_CH_LAYOUT_STEREO or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_2_1               = (AV_CH_LAYOUT_STEREO or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_SURROUND          = (AV_CH_LAYOUT_STEREO or AV_CH_FRONT_CENTER);
  AV_CH_LAYOUT_3POINT1           = (AV_CH_LAYOUT_SURROUND or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_4POINT0           = (AV_CH_LAYOUT_SURROUND or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_4POINT1           = (AV_CH_LAYOUT_4POINT0 or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_2_2               = (AV_CH_LAYOUT_STEREO or AV_CH_SIDE_LEFT or AV_CH_SIDE_RIGHT);
  AV_CH_LAYOUT_QUAD              = (AV_CH_LAYOUT_STEREO or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_5POINT0           = (AV_CH_LAYOUT_SURROUND or AV_CH_SIDE_LEFT or AV_CH_SIDE_RIGHT);
  AV_CH_LAYOUT_5POINT1           = (AV_CH_LAYOUT_5POINT0 or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_5POINT0_BACK      = (AV_CH_LAYOUT_SURROUND or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_5POINT1_BACK      = (AV_CH_LAYOUT_5POINT0_BACK or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_6POINT0           = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT0_FRONT     = (AV_CH_LAYOUT_2_2 or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_HEXAGONAL         = (AV_CH_LAYOUT_5POINT0_BACK or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1           = (AV_CH_LAYOUT_5POINT1 or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1_BACK      = (AV_CH_LAYOUT_5POINT1_BACK or AV_CH_BACK_CENTER);
  AV_CH_LAYOUT_6POINT1_FRONT     = (AV_CH_LAYOUT_6POINT0_FRONT or AV_CH_LOW_FREQUENCY);
  AV_CH_LAYOUT_7POINT0           = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_7POINT0_FRONT     = (AV_CH_LAYOUT_5POINT0 or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_7POINT1           = (AV_CH_LAYOUT_5POINT1 or AV_CH_BACK_LEFT or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_7POINT1_WIDE      = (AV_CH_LAYOUT_5POINT1 or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_7POINT1_WIDE_BACK = (AV_CH_LAYOUT_5POINT1_BACK or AV_CH_FRONT_LEFT_OF_CENTER or AV_CH_FRONT_RIGHT_OF_CENTER);
  AV_CH_LAYOUT_OCTAGONAL         = (AV_CH_LAYOUT_5POINT0 or AV_CH_BACK_LEFT or AV_CH_BACK_CENTER or AV_CH_BACK_RIGHT);
  AV_CH_LAYOUT_HEXADECAGONAL     = (AV_CH_LAYOUT_OCTAGONAL or AV_CH_WIDE_LEFT or AV_CH_WIDE_RIGHT or AV_CH_TOP_BACK_LEFT or AV_CH_TOP_BACK_RIGHT or
    AV_CH_TOP_BACK_CENTER or AV_CH_TOP_FRONT_CENTER or AV_CH_TOP_FRONT_LEFT or AV_CH_TOP_FRONT_RIGHT);
  AV_CH_LAYOUT_STEREO_DOWNMIX = (AV_CH_STEREO_LEFT or AV_CH_STEREO_RIGHT);

type
  AVMatrixEncoding = ( //
    AV_MATRIX_ENCODING_NONE, AV_MATRIX_ENCODING_DOLBY, AV_MATRIX_ENCODING_DPLII, AV_MATRIX_ENCODING_DPLIIX, AV_MATRIX_ENCODING_DPLIIZ,
    AV_MATRIX_ENCODING_DOLBYEX, AV_MATRIX_ENCODING_DOLBYHEADPHONE, AV_MATRIX_ENCODING_NB);

  (* *
    * Return a channel layout id that matches name, or 0 if no match is found.
    *
    * name can be one or several of the following notations,
    * separated by '+' or '|':
    * - the name of an usual channel layout (mono, stereo, 4.0, quad, 5.0,
    *   5.0(side), 5.1, 5.1(side), 7.1, 7.1(wide), downmix);
    * - the name of a single channel (FL, FR, FC, LFE, BL, BR, FLC, FRC, BC,
    *   SL, SR, TC, TFL, TFC, TFR, TBL, TBC, TBR, DL, DR);
    * - a number of channels, in decimal, followed by 'c', yielding
    *   the default channel layout for that number of channels (@see
    *   av_get_default_channel_layout);
    * - a channel layout mask, in hexadecimal starting with "0x" (see the
    *   AV_CH_* macros).
    *
    * Example: "stereo+FC" = "2c+FC" = "2c+1c" = "0x7"
  *)
  // uint64_t av_get_channel_layout(const char *name);
function av_get_channel_layout(const name: PAnsiChar): uint64_t; cdecl; external avutil_dll;
(* *
  * Return a channel layout and the number of channels based on the specified name.
  *
  * This function is similar to (@see av_get_channel_layout), but can also parse
  * unknown channel layout specifications.
  *
  * @param[in]  name             channel layout specification string
  * @param[out] channel_layout   parsed channel layout (0 if unknown)
  * @param[out] nb_channels      number of channels
  *
  * @return 0 on success, AVERROR(EINVAL) if the parsing fails.
*)
// int av_get_extended_channel_layout(const char *name, uint64_t* channel_layout, int* nb_channels);
function av_get_extended_channel_layout(const name: PAnsiChar; var channel_layout: uint64_t; var nb_channels: int): int; cdecl; external avutil_dll;
(* *
  * Return a description of a channel layout.
  * If nb_channels is <= 0, it is guessed from the channel_layout.
  *
  * @param buf put here the string containing the channel layout
  * @param buf_size size in bytes of the buffer
*)
// void av_get_channel_layout_string(char *buf, int buf_size, int nb_channels, uint64_t channel_layout);
procedure av_get_channel_layout_string(buf: PAnsiChar; buf_size: int; nb_channels: int; channel_layout: uint64_t); cdecl; external avutil_dll;

(* *
  * Append a description of a channel layout to a bprint buffer.
*)
// void av_bprint_channel_layout(struct AVBPrint *bp, int nb_channels, uint64_t channel_layout);
procedure av_bprint_channel_layout(bp: pAVBPrint; nb_channels: int; channel_layout: uint64_t); cdecl; external avutil_dll;
(* *
  * Return the number of channels in the channel layout.
*)
// int av_get_channel_layout_nb_channels(uint64_t channel_layout);
function av_get_channel_layout_nb_channels(channel_layout: uint64_t): int; cdecl; external avutil_dll;
(* *
  * Return default channel layout for a given number of channels.
*)
// int64_t av_get_default_channel_layout(int nb_channels);
function av_get_default_channel_layout(nb_channels: int): int64_t; cdecl; external avutil_dll;
(* *
  * Get the index of a channel in channel_layout.
  *
  * @param channel a channel layout describing exactly one channel which must be
  *                present in channel_layout.
  *
  * @return index of channel in channel_layout on success, a negative AVERROR
  *         on error.
*)
// int av_get_channel_layout_channel_index(uint64_t channel_layout, uint64_t channel);
function av_get_channel_layout_channel_index(channel_layout: uint64_t; channel: uint64_t): int; cdecl; external avutil_dll;
(* *
  * Get the channel with the given index in channel_layout.
*)
// uint64_t av_channel_layout_extract_channel(uint64_t channel_layout, int index);
function av_channel_layout_extract_channel(channel_layout: uint64_t; index: int): uint64_t; cdecl; external avutil_dll;
(* *
  * Get the name of a given channel.
  *
  * @return channel name on success, NULL on error.
*)
// const char *av_get_channel_name(uint64_t channel);
function av_get_channel_name(channel: uint64_t): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Get the description of a given channel.
  *
  * @param channel  a channel layout with a single channel
  * @return  channel description on success, NULL on error
*)
// const char *av_get_channel_description(uint64_t channel);
function av_get_channel_description(channel: uint64_t): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Get the value and name of a standard channel layout.
  *
  * @param[in]  index   index in an internal list, starting at 0
  * @param[out] layout  channel layout mask
  * @param[out] name    name of the layout
  * @return  0  if the layout exists,
  *          <0 if index is beyond the limits
*)
// int av_get_standard_channel_layout(unsigned index, uint64_t *layout, const char **name);
function av_get_standard_channel_layout(index: unsigned; var layout: uint64_t; const name: ppAnsiChar): int; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'dict.h'}

const
  AV_DICT_MATCH_CASE    = 1; (* *< Only get an entry with exact-case key match. Only relevant in av_dict_get(). *)
  AV_DICT_IGNORE_SUFFIX = 2; (* *< Return first entry in a dictionary whose first part corresponds to the search key,
    ignoring the suffix of the found key string. Only relevant in av_dict_get(). *)
  AV_DICT_DONT_STRDUP_KEY = 4; (* *< Take ownership of a key that's been
    allocated with av_malloc() or another memory allocation function. *)
  AV_DICT_DONT_STRDUP_VAL = 8; (* *< Take ownership of a value that's been
    allocated with av_malloc() or another memory allocation function. *)
  AV_DICT_DONT_OVERWRITE = 16;
  // < Don't overwrite existing entries.
  AV_DICT_APPEND = 32; (* *< If the entry already exists, append to it.  Note that no
    delimiter is added, the strings are simply concatenated. *)
  AV_DICT_MULTIKEY = 64; (* *< Allow to store several equal keys in the dictionary *)

Type
  AVDictionaryEntry = record
    key: PAnsiChar;
    value: PAnsiChar;
  end;

  pAVDictionaryEntry = ^AVDictionaryEntry;

  AVDictionary = record
  end;

  pAVDictionary = ^AVDictionary;
  ppAVDictionary = ^pAVDictionary;

  (* *
    * Get a dictionary entry with matching key.
    *
    * The returned entry key or value must not be changed, or it will
    * cause undefined behavior.
    *
    * To iterate through all the dictionary entries, you can set the matching key
    * to the null string "" and set the AV_DICT_IGNORE_SUFFIX flag.
    *
    * @param prev Set to the previous matching element to find the next.
    *             If set to NULL the first matching element is returned.
    * @param key matching key
    * @param flags a collection of AV_DICT_* flags controlling how the entry is retrieved
    * @return found entry or NULL in case no matching entry was found in the dictionary
  *)
  // AVDictionaryEntry *av_dict_get(const AVDictionary *m, const char *key,
  // const AVDictionaryEntry *prev, int flags);
function av_dict_get(const m: pAVDictionary; const key: PAnsiChar; const prev: pAVDictionaryEntry; flags: int): pAVDictionaryEntry; cdecl; external avutil_dll;

(* *
  * Get number of entries in dictionary.
  *
  * @param m dictionary
  * @return  number of entries in dictionary
*)
// int av_dict_count(const AVDictionary *m);
function av_dict_count(const m: pAVDictionary): int; cdecl; external avutil_dll;

(* *
  * Set the given entry in *pm, overwriting an existing entry.
  *
  * Note: If AV_DICT_DONT_STRDUP_KEY or AV_DICT_DONT_STRDUP_VAL is set,
  * these arguments will be freed on error.
  *
  * Warning: Adding a new entry to a dictionary invalidates all existing entries
  * previously returned with av_dict_get.
  *
  * @param pm pointer to a pointer to a dictionary struct. If *pm is NULL
  * a dictionary struct is allocated and put in *pm.
  * @param key entry key to add to *pm (will either be av_strduped or added as a new key depending on flags)
  * @param value entry value to add to *pm (will be av_strduped or added as a new key depending on flags).
  *        Passing a NULL value will cause an existing entry to be deleted.
  * @return >= 0 on success otherwise an error code <0
*)
// int av_dict_set(AVDictionary **pm, const char *key, const char *value, int flags);
function av_dict_set(Var pm: pAVDictionary; const key: PAnsiChar; const value: PAnsiChar; flags: int): int; cdecl; external avutil_dll;

(* *
  * Convenience wrapper for av_dict_set that converts the value to a string
  * and stores it.
  *
  * Note: If AV_DICT_DONT_STRDUP_KEY is set, key will be freed on error.
*)
// int av_dict_set_int(AVDictionary **pm, const char *key, int64_t value, int flags);
function av_dict_set_int(var pm: pAVDictionary; const key: PAnsiChar; value: int64_t; flags: int): int; cdecl; external avutil_dll;

(* *
  * Parse the key/value pairs list and add the parsed entries to a dictionary.
  *
  * In case of failure, all the successfully set entries are stored in
  * *pm. You may need to manually free the created dictionary.
  *
  * @param key_val_sep  a 0-terminated list of characters used to separate
  *                     key from value
  * @param pairs_sep    a 0-terminated list of characters used to separate
  *                     two pairs from each other
  * @param flags        flags to use when adding to dictionary.
  *                     AV_DICT_DONT_STRDUP_KEY and AV_DICT_DONT_STRDUP_VAL
  *                     are ignored since the key/value tokens will always
  *                     be duplicated.
  * @return             0 on success, negative AVERROR code on failure
*)
// int av_dict_parse_string(AVDictionary **pm, const char *str,
// const char *key_val_sep, const char *pairs_sep,
// int flags);
function av_dict_parse_string(Var pm: pAVDictionary; const str: PAnsiChar; const key_val_sep: PAnsiChar; const pairs_sep: PAnsiChar; flags: int): int; cdecl;
  external avutil_dll;

(* *
  * Copy entries from one AVDictionary struct into another.
  * @param dst pointer to a pointer to a AVDictionary struct. If *dst is NULL,
  *            this function will allocate a struct for you and put it in *dst
  * @param src pointer to source AVDictionary struct
  * @param flags flags to use when setting entries in *dst
  * @note metadata is read using the AV_DICT_IGNORE_SUFFIX flag
  * @return 0 on success, negative AVERROR code on failure. If dst was allocated
  *           by this function, callers should free the associated memory.
*)
// int av_dict_copy(AVDictionary **dst, const AVDictionary *src, int flags);
function av_dict_copy(var dst: pAVDictionary; const src: pAVDictionary; flags: int): int; cdecl; external avutil_dll;

(* *
  * Free all the memory allocated for an AVDictionary struct
  * and all keys and values.
*)
// void av_dict_free(AVDictionary **m);
procedure av_dict_free(Var m: pAVDictionary); cdecl; external avutil_dll;

(* *
  * Get dictionary entries as a string.
  *
  * Create a string containing dictionary's entries.
  * Such string may be passed back to av_dict_parse_string().
  * @note String is escaped with backslashes ('\').
  *
  * @param[in]  m             dictionary
  * @param[out] buffer        Pointer to buffer that will be allocated with string containg entries.
  *                           Buffer must be freed by the caller when is no longer needed.
  * @param[in]  key_val_sep   character used to separate key from value
  * @param[in]  pairs_sep     character used to separate two pairs from each other
  * @return                   >= 0 on success, negative on error
  * @warning Separators cannot be neither '\\' nor '\0'. They also cannot be the same.
*)
// int av_dict_get_string(const AVDictionary *m, char **buffer,
// const char key_val_sep, const char pairs_sep);
function av_dict_get_string(const m: pAVDictionary; Var buffer: PAnsiChar; const key_val_sep: AnsiChar; const pairs_sep: AnsiChar): int; cdecl;
  external avutil_dll;

{$ENDREGION}
{$REGION 'buffer.h'}

type
  (* *
    * A reference counted buffer type. It is opaque and is meant to be used through
    * references (AVBufferRef).
  *)
  AVBuffer = record
  end;

  pAVBuffer = ^AVBuffer;

  (* *
    * A reference to a data buffer.
    *
    * The size of this struct is not a part of the public ABI and it is not meant
    * to be allocated directly.
  *)
  AVBufferRef = record
    buffer: pAVBuffer;

    (* *
      * The data buffer. It is considered writable if and only if
      * this is the only reference to the buffer, in which case
      * av_buffer_is_writable() returns 1.
    *)
    data: puint8_t;
    (* *
      * Size of data in bytes.
    *)
    size: int;
  end;

  pAVBufferRef = ^AVBufferRef;
  ppAVBufferRef = ^pAVBufferRef;

  (* *
    * Allocate an AVBuffer of the given size using av_malloc().
    *
    * @return an AVBufferRef of given size or NULL when out of memory
  *)
  // AVBufferRef *av_buffer_alloc(int size);
function av_buffer_alloc(size: int): pAVBufferRef; cdecl; external avutil_dll;

(* *
  * Same as av_buffer_alloc(), except the returned buffer will be initialized
  * to zero.
*)
// AVBufferRef *av_buffer_allocz(int size);
function av_buffer_allocz(size: int): pAVBufferRef; cdecl; external avutil_dll;

const
  (* *
    * Always treat the buffer as read-only, even when it has only one
    * reference.
  *)
  AV_BUFFER_FLAG_READONLY = (1 shl 0);

  (* *
    * Create an AVBuffer from an existing array.
    *
    * If this function is successful, data is owned by the AVBuffer. The caller may
    * only access data through the returned AVBufferRef and references derived from
    * it.
    * If this function fails, data is left untouched.
    * @param data   data array
    * @param size   size of data in bytes
    * @param free   a callback for freeing this buffer's data
    * @param opaque parameter to be got for processing or passed to free
    * @param flags  a combination of AV_BUFFER_FLAG_*
    *
    * @return an AVBufferRef referring to data on success, NULL on failure.
  *)
  // AVBufferRef *av_buffer_create(uint8_t *data, int size,
  // void (*free)(void *opaque, uint8_t *data),
  // void *opaque, int flags);

type
  TFreeProc = procedure(opaque: Pointer; data: puint8_t); cdecl;

function av_buffer_create(data: puint8_t; size: int; freeproc: TFreeProc; opaque: Pointer; flags: int): AVBufferRef; cdecl; external avutil_dll;

(* *
  * Default free callback, which calls av_free() on the buffer data.
  * This function is meant to be passed to av_buffer_create(), not called
  * directly.
*)
// void av_buffer_default_free(void *opaque, uint8_t *data);
procedure av_buffer_default_free(opaque: Pointer; data: puint8_t); cdecl; external avutil_dll;

(* *
  * Create a new reference to an AVBuffer.
  *
  * @return a new AVBufferRef referring to the same AVBuffer as buf or NULL on
  * failure.
*)
// AVBufferRef *av_buffer_ref(AVBufferRef *buf);
function av_buffer_ref(buf: pAVBufferRef): pAVBufferRef; cdecl; external avutil_dll;
(* *
  * Free a given reference and automatically free the buffer if there are no more
  * references to it.
  *
  * @param buf the reference to be freed. The pointer is set to NULL on return.
*)
// void av_buffer_unref(AVBufferRef **buf);
procedure av_buffer_unref(var buf: pAVBufferRef); cdecl; external avutil_dll;

(* *
  * @return 1 if the caller may write to the data referred to by buf (which is
  * true if and only if buf is the only reference to the underlying AVBuffer).
  * Return 0 otherwise.
  * A positive answer is valid until av_buffer_ref() is called on buf.
*)
// int av_buffer_is_writable(const AVBufferRef *buf);
function av_buffer_is_writable(const buf: pAVBufferRef): int; cdecl; external avutil_dll;

(* *
  * @return the opaque parameter set by av_buffer_create.
*)
// void *av_buffer_get_opaque(const AVBufferRef *buf);
function av_buffer_get_opaque(const buf: pAVBufferRef): Pointer; cdecl; external avutil_dll;

// int av_buffer_get_ref_count(const AVBufferRef *buf);
function av_buffer_get_ref_count(const buf: pAVBufferRef): int; cdecl; external avutil_dll;

(* *
  * Create a writable reference from a given buffer reference, avoiding data copy
  * if possible.
  *
  * @param buf buffer reference to make writable. On success, buf is either left
  *            untouched, or it is unreferenced and a new writable AVBufferRef is
  *            written in its place. On failure, buf is left untouched.
  * @return 0 on success, a negative AVERROR on failure.
*)
// int av_buffer_make_writable(AVBufferRef **buf);
function av_buffer_make_writable(var buf: pAVBufferRef): int; cdecl; external avutil_dll;

(* *
  * Reallocate a given buffer.
  *
  * @param buf  a buffer reference to reallocate. On success, buf will be
  *             unreferenced and a new reference with the required size will be
  *             written in its place. On failure buf will be left untouched. *buf
  *             may be NULL, then a new buffer is allocated.
  * @param size required new buffer size.
  * @return 0 on success, a negative AVERROR on failure.
  *
  * @note the buffer is actually reallocated with av_realloc() only if it was
  * initially allocated through av_buffer_realloc(NULL) and there is only one
  * reference to it (i.e. the one passed to this function). In all other cases
  * a new buffer is allocated and the data is copied.
*)
// int av_buffer_realloc(AVBufferRef **buf, int size);
function av_buffer_realloc(var buf: pAVBufferRef; size: int): int; cdecl; external avutil_dll;

(* *
  * @defgroup lavu_bufferpool AVBufferPool
  * @ingroup lavu_data
  *
  * @{
  * AVBufferPool is an API for a lock-free thread-safe pool of AVBuffers.
  *
  * Frequently allocating and freeing large buffers may be slow. AVBufferPool is
  * meant to solve this in cases when the caller needs a set of buffers of the
  * same size (the most obvious use case being buffers for raw video or audio
  * frames).
  *
  * At the beginning, the user must call av_buffer_pool_init() to create the
  * buffer pool. Then whenever a buffer is needed, call av_buffer_pool_get() to
  * get a reference to a new buffer, similar to av_buffer_alloc(). This new
  * reference works in all aspects the same way as the one created by
  * av_buffer_alloc(). However, when the last reference to this buffer is
  * unreferenced, it is returned to the pool instead of being freed and will be
  * reused for subsequent av_buffer_pool_get() calls.
  *
  * When the caller is done with the pool and no longer needs to allocate any new
  * buffers, av_buffer_pool_uninit() must be called to mark the pool as freeable.
  * Once all the buffers are released, it will automatically be freed.
  *
  * Allocating and releasing buffers with this API is thread-safe as long as
  * either the default alloc callback is used, or the user-supplied one is
  * thread-safe.
*)

type
  (* *
    * The buffer pool. This structure is opaque and not meant to be accessed
    * directly. It is allocated with av_buffer_pool_init() and freed with
    * av_buffer_pool_uninit().
  *)
  AVBufferPool = record
  end;

  pAVBufferPool = ^AVBufferPool;

  (* *
    * Allocate and initialize a buffer pool.
    *
    * @param size size of each buffer in this pool
    * @param alloc a function that will be used to allocate new buffers when the
    * pool is empty. May be NULL, then the default allocator will be used
    * (av_buffer_alloc()).
    * @return newly created buffer pool on success, NULL on error.
  *)
  // AVBufferPool *av_buffer_pool_init(int size, AVBufferRef* (*alloc)(int size));
type
  Tbuffer_pool_init_proc = function(size: int): pAVBufferRef; cdecl;

function av_buffer_pool_init(size: int; alloc: Tbuffer_pool_init_proc): pAVBufferPool; cdecl; external avutil_dll;

(* *
  * Allocate and initialize a buffer pool with a more complex allocator.
  *
  * @param size size of each buffer in this pool
  * @param opaque arbitrary user data used by the allocator
  * @param alloc a function that will be used to allocate new buffers when the
  *              pool is empty.
  * @param pool_free a function that will be called immediately before the pool
  *                  is freed. I.e. after av_buffer_pool_uninit() is called
  *                  by the caller and all the frames are returned to the pool
  *                  and freed. It is intended to uninitialize the user opaque
  *                  data.
  * @return newly created buffer pool on success, NULL on error.
*)
type
  Tav_buffer_pool_init2_alloc_proc = function(opaque: Pointer; size: int): pAVBufferRef; cdecl;
  Tav_buffer_pool_init2_pool_free_proc = procedure(opaque: Pointer); cdecl;

  // AVBufferPool *av_buffer_pool_init2(int size, void *opaque,
  // AVBufferRef* (*alloc)(void *opaque, int size),
  // void (*pool_free)(void *opaque));
function av_buffer_pool_init2(size: int; opaque: Pointer; alloc: Tav_buffer_pool_init2_alloc_proc; pool_free: Tav_buffer_pool_init2_pool_free_proc)
  : pAVBufferPool; cdecl; external avutil_dll;

(* *
  * Mark the pool as being available for freeing. It will actually be freed only
  * once all the allocated buffers associated with the pool are released. Thus it
  * is safe to call this function while some of the allocated buffers are still
  * in use.
  *
  * @param pool pointer to the pool to be freed. It will be set to NULL.
*)
// void av_buffer_pool_uninit(AVBufferPool **pool);
procedure av_buffer_pool_uninit(var pool: pAVBufferPool); cdecl; external avutil_dll;

(* *
  * Allocate a new AVBuffer, reusing an old buffer from the pool when available.
  * This function may be called simultaneously from multiple threads.
  *
  * @return a reference to the new buffer on success, NULL on error.
*)
// AVBufferRef *av_buffer_pool_get(AVBufferPool *pool);
function av_buffer_pool_get(pool: pAVBufferPool): pAVBufferRef; cdecl; external avutil_dll;

{$ENDREGION}
{$REGION 'rational.h'}

Type
  (* *
    * Rational number (pair of numerator and denominator).
  *)
  AVRational = record
    num: int; // < Numerator
    den: int; // < Denominator
  end;

  pAVRational = ^AVRational;

  (* *
    * Create an AVRational.
    *
    * Useful for compilers that do not support compound literals.
    *
    * @note The return value is not reduced.
    * @see av_reduce()
  *)

  // static inline AVRational av_make_q(int num, int den)
function av_make_q(_num: int; _den: int): AVRational; inline;

(* *
  * Compare two rationals.
  *
  * @param a First rational
  * @param b Second rational
  *
  * @return One of the following values:
  *         - 0 if `a == b`
  *         - 1 if `a > b`
  *         - -1 if `a < b`
  *         - `INT_MIN` if one of the values is of the form `0 / 0`
*)
// static inline int av_cmp_q(AVRational a, AVRational b)
function av_cmp_q(a, b: AVRational): int; inline;

(* *
  * Convert an AVRational to a `double`.
  * @param a AVRational to convert
  * @return `a` in floating-point form
  * @see av_d2q()
*)
// static inline double av_q2d(AVRational a)
function av_q2d(a: AVRational): double; inline;

(* *
  * Reduce a fraction.
  *
  * This is useful for framerate calculations.
  *
  * @param[out] dst_num Destination numerator
  * @param[out] dst_den Destination denominator
  * @param[in]      num Source numerator
  * @param[in]      den Source denominator
  * @param[in]      max Maximum allowed values for `dst_num` & `dst_den`
  * @return 1 if the operation is exact, 0 otherwise
*)
// int av_reduce(int *dst_num, int *dst_den, int64_t num, int64_t den, int64_t max);
function av_reduce(Var dst_num: int; var dst_den: int; num: int64_t; den: int64_t; max: int64_t): int; cdecl; external avutil_dll;

(* *
  * Multiply two rationals.
  * @param b First rational
  * @param c Second rational
  * @return b*c
*)
// AVRational av_mul_q(AVRational b, AVRational c) av_const;
function av_mul_q(b, c: AVRational): AVRational; cdecl; external avutil_dll;

(* *
  * Divide one rational by another.
  * @param b First rational
  * @param c Second rational
  * @return b/c
*)
// AVRational av_div_q(AVRational b, AVRational c) av_const;
function av_div_q(b, c: AVRational): AVRational; cdecl; external avutil_dll;

(* *
  * Add two rationals.
  * @param b First rational
  * @param c Second rational
  * @return b+c
*)
// AVRational av_add_q(AVRational b, AVRational c) av_const;
function av_add_q(b, c: AVRational): AVRational; cdecl; external avutil_dll;

(* *
  * Subtract one rational from another.
  * @param b First rational
  * @param c Second rational
  * @return b-c
*)
// AVRational av_sub_q(AVRational b, AVRational c) av_const;
function av_sub_q(b, c: AVRational): AVRational; cdecl; external avutil_dll;

(* *
  * Invert a rational.
  * @param q value
  * @return 1 / q
*)
// static av_always_inline AVRational av_inv_q(AVRational q)
function av_inv_q(q: AVRational): AVRational; inline;

(* *
  * Convert a double precision floating point number to a rational.
  *
  * In case of infinity, the returned value is expressed as `{1, 0}` or
  * `{-1, 0}` depending on the sign.
  *
  * @param d   `double` to convert
  * @param max Maximum allowed numerator and denominator
  * @return `d` in AVRational form
  * @see av_q2d()
*)
// AVRational av_d2q(double d, int max) av_const;
function av_d2q(d: double; max: int): AVRational; cdecl; external avutil_dll;

(* *
  * Find which of the two rationals is closer to another rational.
  *
  * @param q     Rational to be compared against
  * @param q1,q2 Rationals to be tested
  * @return One of the following values:
  *         - 1 if `q1` is nearer to `q` than `q2`
  *         - -1 if `q2` is nearer to `q` than `q1`
  *         - 0 if they have the same distance
*)
// int av_nearer_q(AVRational q, AVRational q1, AVRational q2);
function av_nearer_q(q: AVRational; q1: AVRational; q2: AVRational): int; cdecl; external avutil_dll;

(* *
  * Find the value in a list of rationals nearest a given reference rational.
  *
  * @param q      Reference rational
  * @param q_list Array of rationals terminated by `{0, 0}`
  * @return Index of the nearest value found in the array
*)
// int av_find_nearest_q_idx(AVRational q, const AVRational* q_list);
function av_find_nearest_q_idx(q: AVRational; const q_list: pAVRational): int; cdecl; external avutil_dll;

(* *
  * Convert an AVRational to a IEEE 32-bit `float` expressed in fixed-point
  * format.
  *
  * @param q Rational to be converted
  * @return Equivalent floating-point value, expressed as an unsigned 32-bit
  *         integer.
  * @note The returned value is platform-indepedant.
*)
// uint32_t av_q2intfloat(AVRational q);
function av_q2intfloat(q: AVRational): uint32_t; cdecl; external avutil_dll;

{$ENDREGION}
{$REGION 'avutil'}
(* *
  * @}
*)

(* *
  * @addtogroup lavu_media Media Type
  * @brief Media Type
*)
type
  AVMediaType = (                //
    AVMEDIA_TYPE_UNKNOWN = -1,   // < Usually treated as AVMEDIA_TYPE_DATA
    AVMEDIA_TYPE_VIDEO = 0,      //
    AVMEDIA_TYPE_AUDIO = 1,      //
    AVMEDIA_TYPE_DATA = 2,       // < Opaque data information usually continuous
    AVMEDIA_TYPE_SUBTITLE = 3,   //
    AVMEDIA_TYPE_ATTACHMENT = 4, // < Opaque data information usually sparse
    AVMEDIA_TYPE_NB = 5          //
    );

  (* *
    * @defgroup lavu_const Constants
    * @{
    *
    * @defgroup lavu_enc Encoding specific
    *
    * @note those definition should move to avcodec
    * @{
  *)
const
  FF_LAMBDA_SHIFT = 7;
  FF_LAMBDA_SCALE = (1 shl FF_LAMBDA_SHIFT);
  FF_QP2LAMBDA    = 118;
  // < factor to convert from H.263 QP to lambda
  FF_LAMBDA_MAX = (256 * 128 - 1);

  FF_QUALITY_SCALE = FF_LAMBDA_SCALE; // FIXME maybe remove

  (* *
    * @}
    * @defgroup lavu_time Timestamp specific
    *
    * FFmpeg internal timebase and timestamp definitions
    *
    * @{
  *)

  (* *
    * @brief Undefined timestamp value
    *
    * Usually reported by demuxer that work on containers that do not provide
    * either pts or dts.
  *)

  AV_NOPTS_VALUE = int64_t($8000000000000000);

  (* *
    * Internal time base represented as integer
  *)

  AV_TIME_BASE = 1000000;

  (* *
    * Internal time base represented as fractional value
  *)

  AV_TIME_BASE_Q: AVRational = (num: 1; den: AV_TIME_BASE);

  (* *
    * @}
    * @}
    * @defgroup lavu_picture Image related
    *
    * AVPicture types, pixel formats and basic image planes manipulation.
    *
    * @{
  *)
type
  AVPictureType = (           //
    AV_PICTURE_TYPE_NONE = 0, // < Undefined
    AV_PICTURE_TYPE_I = 1,    // < Intra
    AV_PICTURE_TYPE_P = 2,    // < Predicted
    AV_PICTURE_TYPE_B = 3,    // < Bi-dir predicted
    AV_PICTURE_TYPE_S = 4,    // < S(GMC)-VOP MPEG-4
    AV_PICTURE_TYPE_SI = 5,   // < Switching Intra
    AV_PICTURE_TYPE_SP = 6,   // < Switching Predicted
    AV_PICTURE_TYPE_BI = 7    //
    );
  // < BI type
{$ENDREGION}
{$REGION 'pixfmt.h'}

type
  (* *
    * Pixel format.
    *
    * @note
    * AV_PIX_FMT_RGB32 is handled in an endian-specific manner. An RGBA
    * color is put together as:
    *  (A  shl  24) | (R  shl  16) | (G  shl  8) | B
    * This is stored as BGRA on little-endian CPU architectures and ARGB on
    * big-endian CPUs.
    *
    * @par
    * When the pixel format is palettized RGB32 (AV_PIX_FMT_PAL8), the palettized
    * image data is stored in AVFrame.data[0]. The palette is transported in
    * AVFrame.data[1], is 1024 bytes long (256 4-byte entries) and is
    * formatted the same as in AV_PIX_FMT_RGB32 described above (i.e., it is
    * also endian-specific). Note also that the individual RGB32 palette
    * components stored in AVFrame.data[1] should be in the range 0..255.
    * This is important as many custom PAL8 video codecs that were designed
    * to run on the IBM VGA graphics adapter use 6-bit palette components.
    *
    * @par
    * For all the 8 bits per pixel formats, an RGB32 palette is in data[1] like
    * for pal8. This palette is filled in automatically by the function
    * allocating the picture.
  *)
  pAVPixelFormat = ^AVPixelFormat;
  AVPixelFormat = (       //
    AV_PIX_FMT_NONE = -1, //
    AV_PIX_FMT_YUV420P,   // < planar YUV 4:2:0, 12bpp, (1 Cr & Cb sample per 2x2 Y samples)
    AV_PIX_FMT_YUYV422,   // < packed YUV 4:2:2, 16bpp, Y0 Cb Y1 Cr
    AV_PIX_FMT_RGB24,     // < packed RGB 8:8:8, 24bpp, RGBRGB...
    AV_PIX_FMT_BGR24,     // < packed RGB 8:8:8, 24bpp, BGRBGR...
    AV_PIX_FMT_YUV422P,   // < planar YUV 4:2:2, 16bpp, (1 Cr & Cb sample per 2x1 Y samples)
    AV_PIX_FMT_YUV444P,   // < planar YUV 4:4:4, 24bpp, (1 Cr & Cb sample per 1x1 Y samples)
    AV_PIX_FMT_YUV410P,   // < planar YUV 4:1:0,  9bpp, (1 Cr & Cb sample per 4x4 Y samples)
    AV_PIX_FMT_YUV411P,   // < planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples)
    AV_PIX_FMT_GRAY8,     // <        Y        ,  8bpp
    AV_PIX_FMT_MONOWHITE, // <        Y        ,  1bpp, 0 is white, 1 is black, in each byte pixels are ordered from the msb to the lsb
    AV_PIX_FMT_MONOBLACK, // <        Y        ,  1bpp, 0 is black, 1 is white, in each byte pixels are ordered from the msb to the lsb
    AV_PIX_FMT_PAL8,      // < 8 bits with AV_PIX_FMT_RGB32 palette
    AV_PIX_FMT_YUVJ420P,  // < planar YUV 4:2:0, 12bpp, full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV420P and setting color_range
    AV_PIX_FMT_YUVJ422P,  // < planar YUV 4:2:2, 16bpp, full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV422P and setting color_range
    AV_PIX_FMT_YUVJ444P,  // < planar YUV 4:4:4, 24bpp, full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV444P and setting color_range
{$IFDEF FF_API_XVMC}
    AV_PIX_FMT_XVMC_MPEG2_MC,                     // < XVideo Motion Acceleration via common packet passing
    AV_PIX_FMT_XVMC_MPEG2_IDCT,                   //
    AV_PIX_FMT_XVMC = AV_PIX_FMT_XVMC_MPEG2_IDCT, //
{$ENDIF}                                          (* FF_API_XVMC *)
    AV_PIX_FMT_UYVY422,                           // < packed YUV 4:2:2, 16bpp, Cb Y0 Cr Y1
    AV_PIX_FMT_UYYVYY411,                         // < packed YUV 4:1:1, 12bpp, Cb Y0 Y1 Cr Y2 Y3
    AV_PIX_FMT_BGR8,                              // < packed RGB 3:3:2,  8bpp, (msb)2B 3G 3R(lsb)
    AV_PIX_FMT_BGR4,
    // < packed RGB 1:2:1 bitstream,  4bpp, (msb)1B 2G 1R(lsb), a byte contains two pixels, the first pixel in the byte is the one composed by the 4 msb bits
    AV_PIX_FMT_BGR4_BYTE, // < packed RGB 1:2:1,  8bpp, (msb)1B 2G 1R(lsb)
    AV_PIX_FMT_RGB8,      // < packed RGB 3:3:2,  8bpp, (msb)2R 3G 3B(lsb)
    AV_PIX_FMT_RGB4,
    // < packed RGB 1:2:1 bitstream,  4bpp, (msb)1R 2G 1B(lsb), a byte contains two pixels, the first pixel in the byte is the one composed by the 4 msb bits
    AV_PIX_FMT_RGB4_BYTE, // < packed RGB 1:2:1,  8bpp, (msb)1R 2G 1B(lsb)
    AV_PIX_FMT_NV12,
    // < planar YUV 4:2:0, 12bpp, 1 plane for Y and 1 plane for the UV components, which are interleaved (first byte U and the following byte V)
    AV_PIX_FMT_NV21, // < as above, but U and V bytes are swapped

    AV_PIX_FMT_ARGB, // < packed ARGB 8:8:8:8, 32bpp, ARGBARGB...
    AV_PIX_FMT_RGBA,
    // < packed RGBA 8:8:8:8, 32bpp, RGBARGBA...
    AV_PIX_FMT_ABGR,
    // < packed ABGR 8:8:8:8, 32bpp, ABGRABGR...
    AV_PIX_FMT_BGRA,
    // < packed BGRA 8:8:8:8, 32bpp, BGRABGRA...

    AV_PIX_FMT_GRAY16BE,
    // <        Y        , 16bpp, big-endian
    AV_PIX_FMT_GRAY16LE,
    // <        Y        , 16bpp, little-endian
    AV_PIX_FMT_YUV440P,
    // < planar YUV 4:4:0 (1 Cr & Cb sample per 1x2 Y samples)
    AV_PIX_FMT_YUVJ440P,
    // < planar YUV 4:4:0 full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV440P and setting color_range
    AV_PIX_FMT_YUVA420P,
    // < planar YUV 4:2:0, 20bpp, (1 Cr & Cb sample per 2x2 Y & A samples)
{$IFDEF FF_API_VDPAU}
    AV_PIX_FMT_VDPAU_H264,
    // < H.264 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    AV_PIX_FMT_VDPAU_MPEG1,
    // < MPEG-1 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    AV_PIX_FMT_VDPAU_MPEG2,
    // < MPEG-2 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    AV_PIX_FMT_VDPAU_WMV3,
    // < WMV3 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
    AV_PIX_FMT_VDPAU_VC1,
    // < VC-1 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
{$ENDIF}
    AV_PIX_FMT_RGB48BE,
    // < packed RGB 16:16:16, 48bpp, 16R, 16G, 16B, the 2-byte value for each R/G/B component is stored as big-endian
    AV_PIX_FMT_RGB48LE,
    // < packed RGB 16:16:16, 48bpp, 16R, 16G, 16B, the 2-byte value for each R/G/B component is stored as little-endian

    AV_PIX_FMT_RGB565BE,
    // < packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), big-endian
    AV_PIX_FMT_RGB565LE,
    // < packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), little-endian
    AV_PIX_FMT_RGB555BE,
    // < packed RGB 5:5:5, 16bpp, (msb)1X 5R 5G 5B(lsb), big-endian   , X=unused/undefined
    AV_PIX_FMT_RGB555LE,
    // < packed RGB 5:5:5, 16bpp, (msb)1X 5R 5G 5B(lsb), little-endian, X=unused/undefined

    AV_PIX_FMT_BGR565BE,
    // < packed BGR 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), big-endian
    AV_PIX_FMT_BGR565LE,
    // < packed BGR 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), little-endian
    AV_PIX_FMT_BGR555BE,
    // < packed BGR 5:5:5, 16bpp, (msb)1X 5B 5G 5R(lsb), big-endian   , X=unused/undefined
    AV_PIX_FMT_BGR555LE,
    // < packed BGR 5:5:5, 16bpp, (msb)1X 5B 5G 5R(lsb), little-endian, X=unused/undefined

{$IFDEF FF_API_VAAPI}
    (* * @name Deprecated pixel formats *)
    (* *@{ *)
    AV_PIX_FMT_VAAPI_MOCO,
    // < HW acceleration through VA API at motion compensation entry-point, Picture.data[3] contains a vaapi_render_state struct which contains macroblocks as well as various fields extracted from headers
    AV_PIX_FMT_VAAPI_IDCT,
    // < HW acceleration through VA API at IDCT entry-point, Picture.data[3] contains a vaapi_render_state struct which contains fields extracted from headers
    AV_PIX_FMT_VAAPI_VLD,
    // < HW decoding through VA API, Picture.data[3] contains a VASurfaceID
    (* *@} *)
    AV_PIX_FMT_VAAPI = AV_PIX_FMT_VAAPI_VLD,
{$ELSE}
    (* *
      *  Hardware acceleration through VA-API, data[3] contains a
      *  VASurfaceID.
    *)
    AV_PIX_FMT_VAAPI,
{$ENDIF}
    AV_PIX_FMT_YUV420P16LE,
    // < planar YUV 4:2:0, 24bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    AV_PIX_FMT_YUV420P16BE,
    // < planar YUV 4:2:0, 24bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    AV_PIX_FMT_YUV422P16LE,
    // < planar YUV 4:2:2, 32bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_YUV422P16BE,
    // < planar YUV 4:2:2, 32bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    AV_PIX_FMT_YUV444P16LE,
    // < planar YUV 4:4:4, 48bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    AV_PIX_FMT_YUV444P16BE,
    // < planar YUV 4:4:4, 48bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
{$IFDEF FF_API_VDPAU}
    AV_PIX_FMT_VDPAU_MPEG4,
    // < MPEG-4 HW decoding with VDPAU, data[0] contains a vdpau_render_state struct which contains the bitstream of the slices as well as various fields extracted from headers
{$ENDIF}
    AV_PIX_FMT_DXVA2_VLD,
    // < HW decoding through DXVA2, Picture.data[3] contains a LPDIRECT3DSURFACE9 pointer

    AV_PIX_FMT_RGB444LE,
    // < packed RGB 4:4:4, 16bpp, (msb)4X 4R 4G 4B(lsb), little-endian, X=unused/undefined
    AV_PIX_FMT_RGB444BE,
    // < packed RGB 4:4:4, 16bpp, (msb)4X 4R 4G 4B(lsb), big-endian,    X=unused/undefined
    AV_PIX_FMT_BGR444LE,
    // < packed BGR 4:4:4, 16bpp, (msb)4X 4B 4G 4R(lsb), little-endian, X=unused/undefined
    AV_PIX_FMT_BGR444BE,
    // < packed BGR 4:4:4, 16bpp, (msb)4X 4B 4G 4R(lsb), big-endian,    X=unused/undefined
    AV_PIX_FMT_YA8,
    // < 8 bits gray, 8 bits alpha

    AV_PIX_FMT_Y400A = AV_PIX_FMT_YA8,
    // < alias for AV_PIX_FMT_YA8
    AV_PIX_FMT_GRAY8A = AV_PIX_FMT_YA8,
    // < alias for AV_PIX_FMT_YA8

    AV_PIX_FMT_BGR48BE,
    // < packed RGB 16:16:16, 48bpp, 16B, 16G, 16R, the 2-byte value for each R/G/B component is stored as big-endian
    AV_PIX_FMT_BGR48LE,
    // < packed RGB 16:16:16, 48bpp, 16B, 16G, 16R, the 2-byte value for each R/G/B component is stored as little-endian

    (* *
      * The following 12 formats have the disadvantage of needing 1 format for each bit depth.
      * Notice that each 9/10 bits sample is stored in 16 bits with extra padding.
      * If you want to support multiple bit depths, then using AV_PIX_FMT_YUV420P16* with the bpp stored separately is better.
    *)
    AV_PIX_FMT_YUV420P9BE,
    // < planar YUV 4:2:0, 13.5bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    AV_PIX_FMT_YUV420P9LE,
    // < planar YUV 4:2:0, 13.5bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    AV_PIX_FMT_YUV420P10BE,
    // < planar YUV 4:2:0, 15bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    AV_PIX_FMT_YUV420P10LE,
    // < planar YUV 4:2:0, 15bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    AV_PIX_FMT_YUV422P10BE,
    // < planar YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    AV_PIX_FMT_YUV422P10LE,
    // < planar YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_YUV444P9BE,
    // < planar YUV 4:4:4, 27bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    AV_PIX_FMT_YUV444P9LE,
    // < planar YUV 4:4:4, 27bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    AV_PIX_FMT_YUV444P10BE,
    // < planar YUV 4:4:4, 30bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    AV_PIX_FMT_YUV444P10LE,
    // < planar YUV 4:4:4, 30bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    AV_PIX_FMT_YUV422P9BE,
    // < planar YUV 4:2:2, 18bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    AV_PIX_FMT_YUV422P9LE,
    // < planar YUV 4:2:2, 18bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_VDA_VLD,
    // < hardware decoding through VDA
    AV_PIX_FMT_GBRP,
    // < planar GBR 4:4:4 24bpp
    AV_PIX_FMT_GBR24P = AV_PIX_FMT_GBRP, // alias for #AV_PIX_FMT_GBRP
    AV_PIX_FMT_GBRP9BE,
    // < planar GBR 4:4:4 27bpp, big-endian
    AV_PIX_FMT_GBRP9LE,
    // < planar GBR 4:4:4 27bpp, little-endian
    AV_PIX_FMT_GBRP10BE,
    // < planar GBR 4:4:4 30bpp, big-endian
    AV_PIX_FMT_GBRP10LE,
    // < planar GBR 4:4:4 30bpp, little-endian
    AV_PIX_FMT_GBRP16BE,
    // < planar GBR 4:4:4 48bpp, big-endian
    AV_PIX_FMT_GBRP16LE,
    // < planar GBR 4:4:4 48bpp, little-endian
    AV_PIX_FMT_YUVA422P,
    // < planar YUV 4:2:2 24bpp, (1 Cr & Cb sample per 2x1 Y & A samples)
    AV_PIX_FMT_YUVA444P,
    // < planar YUV 4:4:4 32bpp, (1 Cr & Cb sample per 1x1 Y & A samples)
    AV_PIX_FMT_YUVA420P9BE,
    // < planar YUV 4:2:0 22.5bpp, (1 Cr & Cb sample per 2x2 Y & A samples), big-endian
    AV_PIX_FMT_YUVA420P9LE,
    // < planar YUV 4:2:0 22.5bpp, (1 Cr & Cb sample per 2x2 Y & A samples), little-endian
    AV_PIX_FMT_YUVA422P9BE,
    // < planar YUV 4:2:2 27bpp, (1 Cr & Cb sample per 2x1 Y & A samples), big-endian
    AV_PIX_FMT_YUVA422P9LE,
    // < planar YUV 4:2:2 27bpp, (1 Cr & Cb sample per 2x1 Y & A samples), little-endian
    AV_PIX_FMT_YUVA444P9BE,
    // < planar YUV 4:4:4 36bpp, (1 Cr & Cb sample per 1x1 Y & A samples), big-endian
    AV_PIX_FMT_YUVA444P9LE,
    // < planar YUV 4:4:4 36bpp, (1 Cr & Cb sample per 1x1 Y & A samples), little-endian
    AV_PIX_FMT_YUVA420P10BE,
    // < planar YUV 4:2:0 25bpp, (1 Cr & Cb sample per 2x2 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA420P10LE,
    // < planar YUV 4:2:0 25bpp, (1 Cr & Cb sample per 2x2 Y & A samples, little-endian)
    AV_PIX_FMT_YUVA422P10BE,
    // < planar YUV 4:2:2 30bpp, (1 Cr & Cb sample per 2x1 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA422P10LE,
    // < planar YUV 4:2:2 30bpp, (1 Cr & Cb sample per 2x1 Y & A samples, little-endian)
    AV_PIX_FMT_YUVA444P10BE,
    // < planar YUV 4:4:4 40bpp, (1 Cr & Cb sample per 1x1 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA444P10LE,
    // < planar YUV 4:4:4 40bpp, (1 Cr & Cb sample per 1x1 Y & A samples, little-endian)
    AV_PIX_FMT_YUVA420P16BE,
    // < planar YUV 4:2:0 40bpp, (1 Cr & Cb sample per 2x2 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA420P16LE,
    // < planar YUV 4:2:0 40bpp, (1 Cr & Cb sample per 2x2 Y & A samples, little-endian)
    AV_PIX_FMT_YUVA422P16BE,
    // < planar YUV 4:2:2 48bpp, (1 Cr & Cb sample per 2x1 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA422P16LE,
    // < planar YUV 4:2:2 48bpp, (1 Cr & Cb sample per 2x1 Y & A samples, little-endian)
    AV_PIX_FMT_YUVA444P16BE,
    // < planar YUV 4:4:4 64bpp, (1 Cr & Cb sample per 1x1 Y & A samples, big-endian)
    AV_PIX_FMT_YUVA444P16LE,
    // < planar YUV 4:4:4 64bpp, (1 Cr & Cb sample per 1x1 Y & A samples, little-endian)

    AV_PIX_FMT_VDPAU,
    // < HW acceleration through VDPAU, Picture.data[3] contains a VdpVideoSurface

    AV_PIX_FMT_XYZ12LE,
    // < packed XYZ 4:4:4, 36 bpp, (msb) 12X, 12Y, 12Z (lsb), the 2-byte value for each X/Y/Z is stored as little-endian, the 4 lower bits are set to 0
    AV_PIX_FMT_XYZ12BE,
    // < packed XYZ 4:4:4, 36 bpp, (msb) 12X, 12Y, 12Z (lsb), the 2-byte value for each X/Y/Z is stored as big-endian, the 4 lower bits are set to 0
    AV_PIX_FMT_NV16,
    // < interleaved chroma YUV 4:2:2, 16bpp, (1 Cr & Cb sample per 2x1 Y samples)
    AV_PIX_FMT_NV20LE,
    // < interleaved chroma YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_NV20BE,
    // < interleaved chroma YUV 4:2:2, 20bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian

    AV_PIX_FMT_RGBA64BE,
    // < packed RGBA 16:16:16:16, 64bpp, 16R, 16G, 16B, 16A, the 2-byte value for each R/G/B/A component is stored as big-endian
    AV_PIX_FMT_RGBA64LE,
    // < packed RGBA 16:16:16:16, 64bpp, 16R, 16G, 16B, 16A, the 2-byte value for each R/G/B/A component is stored as little-endian
    AV_PIX_FMT_BGRA64BE,
    // < packed RGBA 16:16:16:16, 64bpp, 16B, 16G, 16R, 16A, the 2-byte value for each R/G/B/A component is stored as big-endian
    AV_PIX_FMT_BGRA64LE,
    // < packed RGBA 16:16:16:16, 64bpp, 16B, 16G, 16R, 16A, the 2-byte value for each R/G/B/A component is stored as little-endian

    AV_PIX_FMT_YVYU422,
    // < packed YUV 4:2:2, 16bpp, Y0 Cr Y1 Cb

    AV_PIX_FMT_VDA,
    // < HW acceleration through VDA, data[3] contains a CVPixelBufferRef

    AV_PIX_FMT_YA16BE,
    // < 16 bits gray, 16 bits alpha (big-endian)
    AV_PIX_FMT_YA16LE,
    // < 16 bits gray, 16 bits alpha (little-endian)

    AV_PIX_FMT_GBRAP,
    // < planar GBRA 4:4:4:4 32bpp
    AV_PIX_FMT_GBRAP16BE,
    // < planar GBRA 4:4:4:4 64bpp, big-endian
    AV_PIX_FMT_GBRAP16LE,
    // < planar GBRA 4:4:4:4 64bpp, little-endian
    (* *
      *  HW acceleration through QSV, data[3] contains a pointer to the
      *  mfxFrameSurface1 structure.
    *)
    AV_PIX_FMT_QSV,
    (* *
      * HW acceleration though MMAL, data[3] contains a pointer to the
      * MMAL_BUFFER_HEADER_T structure.
    *)
    AV_PIX_FMT_MMAL,

    AV_PIX_FMT_D3D11VA_VLD,
    // < HW decoding through Direct3D11, Picture.data[3] contains a ID3D11VideoDecoderOutputView pointer

    (* *
      * HW acceleration through CUDA. data[i] contain CUdeviceptr pointers
      * exactly as for system memory frames.
    *)
    AV_PIX_FMT_CUDA, AV_PIX_FMT_0RGB = $123 + 4,
    // < packed RGB 8:8:8, 32bpp, XRGBXRGB...   X=unused/undefined
    AV_PIX_FMT_RGB0,
    // < packed RGB 8:8:8, 32bpp, RGBXRGBX...   X=unused/undefined
    AV_PIX_FMT_0BGR,
    // < packed BGR 8:8:8, 32bpp, XBGRXBGR...   X=unused/undefined
    AV_PIX_FMT_BGR0,
    // < packed BGR 8:8:8, 32bpp, BGRXBGRX...   X=unused/undefined

    AV_PIX_FMT_YUV420P12BE,
    // < planar YUV 4:2:0,18bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    AV_PIX_FMT_YUV420P12LE,
    // < planar YUV 4:2:0,18bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    AV_PIX_FMT_YUV420P14BE,
    // < planar YUV 4:2:0,21bpp, (1 Cr & Cb sample per 2x2 Y samples), big-endian
    AV_PIX_FMT_YUV420P14LE,
    // < planar YUV 4:2:0,21bpp, (1 Cr & Cb sample per 2x2 Y samples), little-endian
    AV_PIX_FMT_YUV422P12BE,
    // < planar YUV 4:2:2,24bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    AV_PIX_FMT_YUV422P12LE,
    // < planar YUV 4:2:2,24bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_YUV422P14BE,
    // < planar YUV 4:2:2,28bpp, (1 Cr & Cb sample per 2x1 Y samples), big-endian
    AV_PIX_FMT_YUV422P14LE,
    // < planar YUV 4:2:2,28bpp, (1 Cr & Cb sample per 2x1 Y samples), little-endian
    AV_PIX_FMT_YUV444P12BE,
    // < planar YUV 4:4:4,36bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    AV_PIX_FMT_YUV444P12LE,
    // < planar YUV 4:4:4,36bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    AV_PIX_FMT_YUV444P14BE,
    // < planar YUV 4:4:4,42bpp, (1 Cr & Cb sample per 1x1 Y samples), big-endian
    AV_PIX_FMT_YUV444P14LE,
    // < planar YUV 4:4:4,42bpp, (1 Cr & Cb sample per 1x1 Y samples), little-endian
    AV_PIX_FMT_GBRP12BE,
    // < planar GBR 4:4:4 36bpp, big-endian
    AV_PIX_FMT_GBRP12LE,
    // < planar GBR 4:4:4 36bpp, little-endian
    AV_PIX_FMT_GBRP14BE,
    // < planar GBR 4:4:4 42bpp, big-endian
    AV_PIX_FMT_GBRP14LE,
    // < planar GBR 4:4:4 42bpp, little-endian
    AV_PIX_FMT_YUVJ411P,
    // < planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples) full scale (JPEG), deprecated in favor of AV_PIX_FMT_YUV411P and setting color_range

    AV_PIX_FMT_BAYER_BGGR8,
    // < bayer, BGBG..(odd line), GRGR..(even line), 8-bit samples *)
    AV_PIX_FMT_BAYER_RGGB8,
    // < bayer, RGRG..(odd line), GBGB..(even line), 8-bit samples *)
    AV_PIX_FMT_BAYER_GBRG8,
    // < bayer, GBGB..(odd line), RGRG..(even line), 8-bit samples *)
    AV_PIX_FMT_BAYER_GRBG8,
    // < bayer, GRGR..(odd line), BGBG..(even line), 8-bit samples *)
    AV_PIX_FMT_BAYER_BGGR16LE,
    // < bayer, BGBG..(odd line), GRGR..(even line), 16-bit samples, little-endian *)
    AV_PIX_FMT_BAYER_BGGR16BE,
    // < bayer, BGBG..(odd line), GRGR..(even line), 16-bit samples, big-endian *)
    AV_PIX_FMT_BAYER_RGGB16LE,
    // < bayer, RGRG..(odd line), GBGB..(even line), 16-bit samples, little-endian *)
    AV_PIX_FMT_BAYER_RGGB16BE,
    // < bayer, RGRG..(odd line), GBGB..(even line), 16-bit samples, big-endian *)
    AV_PIX_FMT_BAYER_GBRG16LE,
    // < bayer, GBGB..(odd line), RGRG..(even line), 16-bit samples, little-endian *)
    AV_PIX_FMT_BAYER_GBRG16BE,
    // < bayer, GBGB..(odd line), RGRG..(even line), 16-bit samples, big-endian *)
    AV_PIX_FMT_BAYER_GRBG16LE,
    // < bayer, GRGR..(odd line), BGBG..(even line), 16-bit samples, little-endian *)
    AV_PIX_FMT_BAYER_GRBG16BE,
    // < bayer, GRGR..(odd line), BGBG..(even line), 16-bit samples, big-endian *)
{$IFNDEF FF_API_XVMC}
    AV_PIX_FMT_XVMC,
    // < XVideo Motion Acceleration via common packet passing
{$ENDIF} (* !FF_API_XVMC *)
    AV_PIX_FMT_YUV440P10LE,
    // < planar YUV 4:4:0,20bpp, (1 Cr & Cb sample per 1x2 Y samples), little-endian
    AV_PIX_FMT_YUV440P10BE,
    // < planar YUV 4:4:0,20bpp, (1 Cr & Cb sample per 1x2 Y samples), big-endian
    AV_PIX_FMT_YUV440P12LE,
    // < planar YUV 4:4:0,24bpp, (1 Cr & Cb sample per 1x2 Y samples), little-endian
    AV_PIX_FMT_YUV440P12BE,
    // < planar YUV 4:4:0,24bpp, (1 Cr & Cb sample per 1x2 Y samples), big-endian
    AV_PIX_FMT_AYUV64LE,
    // < packed AYUV 4:4:4,64bpp (1 Cr & Cb sample per 1x1 Y & A samples), little-endian
    AV_PIX_FMT_AYUV64BE,
    // < packed AYUV 4:4:4,64bpp (1 Cr & Cb sample per 1x1 Y & A samples), big-endian

    AV_PIX_FMT_VIDEOTOOLBOX,
    // < hardware decoding through Videotoolbox

    AV_PIX_FMT_P010LE,
    // < like NV12, with 10bpp per component, data in the high bits, zeros in the low bits, little-endian
    AV_PIX_FMT_P010BE,
    // < like NV12, with 10bpp per component, data in the high bits, zeros in the low bits, big-endian

    AV_PIX_FMT_GBRAP12BE,
    // < planar GBR 4:4:4:4 48bpp, big-endian
    AV_PIX_FMT_GBRAP12LE,
    // < planar GBR 4:4:4:4 48bpp, little-endian

    AV_PIX_FMT_GBRAP10BE,
    // < planar GBR 4:4:4:4 40bpp, big-endian
    AV_PIX_FMT_GBRAP10LE,
    // < planar GBR 4:4:4:4 40bpp, little-endian

    AV_PIX_FMT_MEDIACODEC,
    // < hardware decoding through MediaCodec

    AV_PIX_FMT_GRAY12BE,
    // <        Y        , 12bpp, big-endian
    AV_PIX_FMT_GRAY12LE,
    // <        Y        , 12bpp, little-endian
    AV_PIX_FMT_GRAY10BE,
    // <        Y        , 10bpp, big-endian
    AV_PIX_FMT_GRAY10LE,
    // <        Y        , 10bpp, little-endian

    AV_PIX_FMT_P016LE,
    // < like NV12, with 16bpp per component, little-endian
    AV_PIX_FMT_P016BE,
    // < like NV12, with 16bpp per component, big-endian
    (* *
      * Hardware surfaces for Direct3D11.
      *
      * This is preferred over the legacy AV_PIX_FMT_D3D11VA_VLD. The new D3D11
      * hwaccel API and filtering support AV_PIX_FMT_D3D11 only.
      *
      * data[0] contains a ID3D11Texture2D pointer, and data[1] contains the
      * texture array index of the frame as intptr_t if the ID3D11Texture2D is
      * an array texture (or always 0 if it's a normal texture).
    *)
    AV_PIX_FMT_D3D11,

    AV_PIX_FMT_GRAY9BE,
    /// <        Y        , 9bpp, big-endian
    AV_PIX_FMT_GRAY9LE,
    /// <        Y        , 9bpp, little-endian

    AV_PIX_FMT_GBRPF32BE,
    /// < IEEE-754 single precision planar GBR 4:4:4,     96bpp, big-endian
    AV_PIX_FMT_GBRPF32LE,
    /// < IEEE-754 single precision planar GBR 4:4:4,     96bpp, little-endian
    AV_PIX_FMT_GBRAPF32BE,
    /// < IEEE-754 single precision planar GBRA 4:4:4:4, 128bpp, big-endian
    AV_PIX_FMT_GBRAPF32LE,
    /// < IEEE-754 single precision planar GBRA 4:4:4:4, 128bpp, little-endian

    (* *
      * DRM-managed buffers exposed through PRIME buffer sharing.
      *
      * data[0] points to an AVDRMFrameDescriptor.
    *)
    AV_PIX_FMT_DRM_PRIME,
    (* *
      * Hardware surfaces for OpenCL.
      *
      * data[i] contain 2D image objects (typed in C as cl_mem, used
      * in OpenCL as image2d_t) for each plane of the surface.
    *)
    AV_PIX_FMT_OPENCL, //
    AV_PIX_FMT_GRAY14BE,
    /// <        Y        , 14bpp, big-endian
    AV_PIX_FMT_GRAY14LE,
    /// <        Y        , 14bpp, little-endian

    AV_PIX_FMT_GRAYF32BE,
    /// < IEEE-754 single precision Y, 32bpp, big-endian
    AV_PIX_FMT_GRAYF32LE,
    /// < IEEE-754 single precision Y, 32bpp, little-endian

    AV_PIX_FMT_YUVA422P12BE,
    /// < planar YUV 4:2:2,24bpp, (1 Cr & Cb sample per 2x1 Y samples), 12b alpha, big-endian
    AV_PIX_FMT_YUVA422P12LE,
    /// < planar YUV 4:2:2,24bpp, (1 Cr & Cb sample per 2x1 Y samples), 12b alpha, little-endian
    AV_PIX_FMT_YUVA444P12BE,
    /// < planar YUV 4:4:4,36bpp, (1 Cr & Cb sample per 1x1 Y samples), 12b alpha, big-endian
    AV_PIX_FMT_YUVA444P12LE,
    /// < planar YUV 4:4:4,36bpp, (1 Cr & Cb sample per 1x1 Y samples), 12b alpha, little-endian

    AV_PIX_FMT_NV24,
    /// < planar YUV 4:4:4, 24bpp, 1 plane for Y and 1 plane for the UV components, which are interleaved (first byte U and the following byte V)
    AV_PIX_FMT_NV42,
    /// < as above, but U and V bytes are swapped
    AV_PIX_FMT_NB
    // < number of pixel formats, DO NOT USE THIS if you want to link with shared libav* because the number of formats might differ between versions
    );

const
  AVPALETTE_SIZE  = 1024;
  AVPALETTE_COUNT = 256;

  AV_PIX_FMT_RGB32: AVPixelFormat   = AV_PIX_FMT_BGRA; // AV_PIX_FMT_NE(ARGB, BGRA)
  AV_PIX_FMT_RGB32_1: AVPixelFormat = AV_PIX_FMT_ABGR; // AV_PIX_FMT_NE(RGBA, ABGR)
  AV_PIX_FMT_BGR32: AVPixelFormat   = AV_PIX_FMT_RGBA; // AV_PIX_FMT_NE(ABGR, RGBA)
  AV_PIX_FMT_BGR32_1: AVPixelFormat = AV_PIX_FMT_ARGB; // AV_PIX_FMT_NE(BGRA, ARGB)
  AV_PIX_FMT_0RGB32: AVPixelFormat  = AV_PIX_FMT_BGR0; // AV_PIX_FMT_NE(0RGB, BGR0)
  AV_PIX_FMT_0BGR32: AVPixelFormat  = AV_PIX_FMT_RGB0; // AV_PIX_FMT_NE(0BGR, RGB0)

  AV_PIX_FMT_GRAY10: AVPixelFormat = AV_PIX_FMT_GRAY10LE; // AV_PIX_FMT_NE(GRAY10BE, GRAY10LE)
  AV_PIX_FMT_GRAY12: AVPixelFormat = AV_PIX_FMT_GRAY12LE; // AV_PIX_FMT_NE(GRAY12BE, GRAY12LE)
  AV_PIX_FMT_GRAY16: AVPixelFormat = AV_PIX_FMT_GRAY16LE; // AV_PIX_FMT_NE(GRAY16BE, GRAY16LE)
  AV_PIX_FMT_YA16: AVPixelFormat   = AV_PIX_FMT_YA16LE;   // AV_PIX_FMT_NE(YA16BE,   YA16LE  )
  AV_PIX_FMT_RGB48: AVPixelFormat  = AV_PIX_FMT_RGB48LE;  // AV_PIX_FMT_NE(RGB48BE,  RGB48LE )
  AV_PIX_FMT_RGB565: AVPixelFormat = AV_PIX_FMT_RGB565LE; // AV_PIX_FMT_NE(RGB565BE, RGB565LE)
  AV_PIX_FMT_RGB555: AVPixelFormat = AV_PIX_FMT_RGB555LE; // AV_PIX_FMT_NE(RGB555BE, RGB555LE)
  AV_PIX_FMT_RGB444: AVPixelFormat = AV_PIX_FMT_RGB444LE; // AV_PIX_FMT_NE(RGB444BE, RGB444LE)
  AV_PIX_FMT_RGBA64: AVPixelFormat = AV_PIX_FMT_RGBA64LE; // AV_PIX_FMT_NE(RGBA64BE, RGBA64LE)
  AV_PIX_FMT_BGR48: AVPixelFormat  = AV_PIX_FMT_BGR48LE;  // AV_PIX_FMT_NE(BGR48BE,  BGR48LE )
  AV_PIX_FMT_BGR565: AVPixelFormat = AV_PIX_FMT_BGR565LE; // AV_PIX_FMT_NE(BGR565BE, BGR565LE)
  AV_PIX_FMT_BGR555: AVPixelFormat = AV_PIX_FMT_BGR555LE; // AV_PIX_FMT_NE(BGR555BE, BGR555LE)
  AV_PIX_FMT_BGR444: AVPixelFormat = AV_PIX_FMT_BGR444LE; // AV_PIX_FMT_NE(BGR444BE, BGR444LE)
  AV_PIX_FMT_BGRA64: AVPixelFormat = AV_PIX_FMT_BGRA64LE; // AV_PIX_FMT_NE(BGRA64BE, BGRA64LE)

  AV_PIX_FMT_YUV420P9: AVPixelFormat  = AV_PIX_FMT_YUV420P9LE; // AV_PIX_FMT_NE(YUV420P9BE , YUV420P9LE )
  AV_PIX_FMT_YUV422P9: AVPixelFormat  = AV_PIX_FMT_YUV422P9LE; // AV_PIX_FMT_NE(YUV422P9BE , YUV422P9LE )
  AV_PIX_FMT_YUV444P9: AVPixelFormat  = AV_PIX_FMT_YUV444P9LE; // AV_PIX_FMT_NE(YUV444P9BE , YUV444P9LE )
  AV_PIX_FMT_YUV420P10: AVPixelFormat = AV_PIX_FMT_YUV420P10LE; // AV_PIX_FMT_NE(YUV420P10BE, YUV420P10LE)
  AV_PIX_FMT_YUV422P10: AVPixelFormat = AV_PIX_FMT_YUV422P10LE; // AV_PIX_FMT_NE(YUV422P10BE, YUV422P10LE)
  AV_PIX_FMT_YUV440P10: AVPixelFormat = AV_PIX_FMT_YUV440P10LE; // AV_PIX_FMT_NE(YUV440P10BE, YUV440P10LE)
  AV_PIX_FMT_YUV444P10: AVPixelFormat = AV_PIX_FMT_YUV444P10LE; // AV_PIX_FMT_NE(YUV444P10BE, YUV444P10LE)
  AV_PIX_FMT_YUV420P12: AVPixelFormat = AV_PIX_FMT_YUV420P12LE; // AV_PIX_FMT_NE(YUV420P12BE, YUV420P12LE)
  AV_PIX_FMT_YUV422P12: AVPixelFormat = AV_PIX_FMT_YUV422P12LE; // AV_PIX_FMT_NE(YUV422P12BE, YUV422P12LE)
  AV_PIX_FMT_YUV440P12: AVPixelFormat = AV_PIX_FMT_YUV440P12LE; // AV_PIX_FMT_NE(YUV440P12BE, YUV440P12LE)
  AV_PIX_FMT_YUV444P12: AVPixelFormat = AV_PIX_FMT_YUV444P12LE; // AV_PIX_FMT_NE(YUV444P12BE, YUV444P12LE)
  AV_PIX_FMT_YUV420P14: AVPixelFormat = AV_PIX_FMT_YUV420P14LE; // AV_PIX_FMT_NE(YUV420P14BE, YUV420P14LE)
  AV_PIX_FMT_YUV422P14: AVPixelFormat = AV_PIX_FMT_YUV422P14LE; // AV_PIX_FMT_NE(YUV422P14BE, YUV422P14LE)
  AV_PIX_FMT_YUV444P14: AVPixelFormat = AV_PIX_FMT_YUV444P14LE; // AV_PIX_FMT_NE(YUV444P14BE, YUV444P14LE)
  AV_PIX_FMT_YUV420P16: AVPixelFormat = AV_PIX_FMT_YUV420P16LE; // AV_PIX_FMT_NE(YUV420P16BE, YUV420P16LE)
  AV_PIX_FMT_YUV422P16: AVPixelFormat = AV_PIX_FMT_YUV422P16LE; // AV_PIX_FMT_NE(YUV422P16BE, YUV422P16LE)
  AV_PIX_FMT_YUV444P16: AVPixelFormat = AV_PIX_FMT_YUV444P16LE; // AV_PIX_FMT_NE(YUV444P16BE, YUV444P16LE)

  AV_PIX_FMT_GBRP9: AVPixelFormat   = AV_PIX_FMT_GBRP9LE;  // AV_PIX_FMT_NE(GBRP9BE ,    GBRP9LE  )
  AV_PIX_FMT_GBRP10: AVPixelFormat  = AV_PIX_FMT_GBRP10LE; // AV_PIX_FMT_NE(GBRP10BE,    GBRP10LE )
  AV_PIX_FMT_GBRP12: AVPixelFormat  = AV_PIX_FMT_GBRP12LE; // AV_PIX_FMT_NE(GBRP12BE,    GBRP12LE )
  AV_PIX_FMT_GBRP14: AVPixelFormat  = AV_PIX_FMT_GBRP14LE; // AV_PIX_FMT_NE(GBRP14BE,    GBRP14LE )
  AV_PIX_FMT_GBRP16: AVPixelFormat  = AV_PIX_FMT_GBRP16LE; // AV_PIX_FMT_NE(GBRP16BE,    GBRP16LE )
  AV_PIX_FMT_GBRAP10: AVPixelFormat = AV_PIX_FMT_GBRAP10LE; // AV_PIX_FMT_NE(GBRAP10BE,   GBRAP10LE)
  AV_PIX_FMT_GBRAP12: AVPixelFormat = AV_PIX_FMT_GBRAP12LE; // AV_PIX_FMT_NE(GBRAP12BE,   GBRAP12LE)
  AV_PIX_FMT_GBRAP16: AVPixelFormat = AV_PIX_FMT_GBRAP16LE; // AV_PIX_FMT_NE(GBRAP16BE,   GBRAP16LE)

  AV_PIX_FMT_BAYER_BGGR16: AVPixelFormat = AV_PIX_FMT_BAYER_BGGR16LE; // AV_PIX_FMT_NE(BAYER_BGGR16BE,    BAYER_BGGR16LE)
  AV_PIX_FMT_BAYER_RGGB16: AVPixelFormat = AV_PIX_FMT_BAYER_RGGB16LE; // AV_PIX_FMT_NE(BAYER_RGGB16BE,    BAYER_RGGB16LE)
  AV_PIX_FMT_BAYER_GBRG16: AVPixelFormat = AV_PIX_FMT_BAYER_GBRG16LE; // AV_PIX_FMT_NE(BAYER_GBRG16BE,    BAYER_GBRG16LE)
  AV_PIX_FMT_BAYER_GRBG16: AVPixelFormat = AV_PIX_FMT_BAYER_GRBG16LE; // AV_PIX_FMT_NE(BAYER_GRBG16BE,    BAYER_GRBG16LE)

  AV_PIX_FMT_YUVA420P9: AVPixelFormat  = AV_PIX_FMT_YUVA420P9LE; // AV_PIX_FMT_NE(YUVA420P9BE , YUVA420P9LE )
  AV_PIX_FMT_YUVA422P9: AVPixelFormat  = AV_PIX_FMT_YUVA422P9LE; // AV_PIX_FMT_NE(YUVA422P9BE , YUVA422P9LE )
  AV_PIX_FMT_YUVA444P9: AVPixelFormat  = AV_PIX_FMT_YUVA444P9LE; // AV_PIX_FMT_NE(YUVA444P9BE , YUVA444P9LE )
  AV_PIX_FMT_YUVA420P10: AVPixelFormat = AV_PIX_FMT_YUVA420P10LE; // AV_PIX_FMT_NE(YUVA420P10BE, YUVA420P10LE)
  AV_PIX_FMT_YUVA422P10: AVPixelFormat = AV_PIX_FMT_YUVA422P10LE; // AV_PIX_FMT_NE(YUVA422P10BE, YUVA422P10LE)
  AV_PIX_FMT_YUVA444P10: AVPixelFormat = AV_PIX_FMT_YUVA444P10LE; // AV_PIX_FMT_NE(YUVA444P10BE, YUVA444P10LE)
  AV_PIX_FMT_YUVA422P12: AVPixelFormat = AV_PIX_FMT_YUVA422P12LE; // AV_PIX_FMT_NE(YUVA422P12BE, YUVA422P12LE);
  AV_PIX_FMT_YUVA444P12: AVPixelFormat = AV_PIX_FMT_YUVA444P12LE; // AV_PIX_FMT_NE(YUVA444P12BE, YUVA444P12LE);
  AV_PIX_FMT_YUVA420P16: AVPixelFormat = AV_PIX_FMT_YUVA420P16LE; // AV_PIX_FMT_NE(YUVA420P16BE, YUVA420P16LE)
  AV_PIX_FMT_YUVA422P16: AVPixelFormat = AV_PIX_FMT_YUVA422P16LE; // AV_PIX_FMT_NE(YUVA422P16BE, YUVA422P16LE)
  AV_PIX_FMT_YUVA444P16: AVPixelFormat = AV_PIX_FMT_YUVA444P16LE; // AV_PIX_FMT_NE(YUVA444P16BE, YUVA444P16LE)

  AV_PIX_FMT_XYZ12: AVPixelFormat  = AV_PIX_FMT_XYZ12LE;  // AV_PIX_FMT_NE(XYZ12BE, XYZ12LE )
  AV_PIX_FMT_NV20: AVPixelFormat   = AV_PIX_FMT_NV20LE;   // AV_PIX_FMT_NE(NV20BE,  NV20LE  )
  AV_PIX_FMT_AYUV64: AVPixelFormat = AV_PIX_FMT_AYUV64LE; // AV_PIX_FMT_NE(AYUV64BE,AYUV64LE)
  AV_PIX_FMT_P010: AVPixelFormat   = AV_PIX_FMT_P010LE;   // AV_PIX_FMT_NE(P010BE,  P010LE  )
  AV_PIX_FMT_P016: AVPixelFormat   = AV_PIX_FMT_P016LE;   // AV_PIX_FMT_NE(P016BE,  P016LE  )

  (* *
    * Chromaticity coordinates of the source primaries.
  *)
Type
  AVColorPrimaries = ( //
    AVCOL_PRI_RESERVED0 = 0, AVCOL_PRI_BT709 = 1,
    // < also ITU-R BT1361 / IEC 61966-2-4 / SMPTE RP177 Annex B
    AVCOL_PRI_UNSPECIFIED = 2, AVCOL_PRI_RESERVED = 3, AVCOL_PRI_BT470M = 4,
    // < also FCC Title 47 Code of Federal Regulations 73.682 (a)(20)

    AVCOL_PRI_BT470BG = 5,
    // < also ITU-R BT601-6 625 / ITU-R BT1358 625 / ITU-R BT1700 625 PAL & SECAM
    AVCOL_PRI_SMPTE170M = 6,
    // < also ITU-R BT601-6 525 / ITU-R BT1358 525 / ITU-R BT1700 NTSC
    AVCOL_PRI_SMPTE240M = 7,
    // < functionally identical to above
    AVCOL_PRI_FILM = 8,
    // < colour filters using Illuminant C
    AVCOL_PRI_BT2020 = 9,
    // < ITU-R BT2020
    AVCOL_PRI_SMPTE428 = 10,
    // < SMPTE ST 428-1 (CIE 1931 XYZ)
    AVCOL_PRI_SMPTEST428_1 = AVCOL_PRI_SMPTE428, //
    AVCOL_PRI_SMPTE431 = 11,                     // < SMPTE ST 431-2 (2011) / DCI P3
    AVCOL_PRI_SMPTE432 = 12,                     // < SMPTE ST 432-1 (2010) / P3 D65 / Display P3
    AVCOL_PRI_JEDEC_P22 = 22,
    /// < JEDEC P22 phosphors
    AVCOL_PRI_NB
    /// < Not part of ABI
    );

  (* *
    * Color Transfer Characteristic.
  *)
  AVColorTransferCharacteristic = ( //
    AVCOL_TRC_RESERVED0 = 0, AVCOL_TRC_BT709 = 1,
    // < also ITU-R BT1361
    AVCOL_TRC_UNSPECIFIED = 2,                   //
    AVCOL_TRC_RESERVED = 3,                      //
    AVCOL_TRC_GAMMA22 = 4,                       // < also ITU-R BT470M / ITU-R BT1700 625 PAL & SECAM
    AVCOL_TRC_GAMMA28 = 5,                       // < also ITU-R BT470BG
    AVCOL_TRC_SMPTE170M = 6,                     // < also ITU-R BT601-6 525 or 625 / ITU-R BT1358 525 or 625 / ITU-R BT1700 NTSC
    AVCOL_TRC_SMPTE240M = 7,                     //
    AVCOL_TRC_LINEAR = 8,                        // < "Linear transfer characteristics"
    AVCOL_TRC_LOG = 9,                           // < "Logarithmic transfer characteristic (100:1 range)"
    AVCOL_TRC_LOG_SQRT = 10,                     // < "Logarithmic transfer characteristic (100 * Sqrt(10) : 1 range)"
    AVCOL_TRC_IEC61966_2_4 = 11,                 // < IEC 61966-2-4
    AVCOL_TRC_BT1361_ECG = 12,                   // < ITU-R BT1361 Extended Colour Gamut
    AVCOL_TRC_IEC61966_2_1 = 13,                 // < IEC 61966-2-1 (sRGB or sYCC)
    AVCOL_TRC_BT2020_10 = 14,                    // < ITU-R BT2020 for 10-bit system
    AVCOL_TRC_BT2020_12 = 15,                    // < ITU-R BT2020 for 12-bit system
    AVCOL_TRC_SMPTE2084 = 16,                    // < SMPTE ST 2084 for 10-, 12-, 14- and 16-bit systems
    AVCOL_TRC_SMPTEST2084 = AVCOL_TRC_SMPTE2084, //
    AVCOL_TRC_SMPTE428 = 17,                     // < SMPTE ST 428-1
    AVCOL_TRC_SMPTEST428_1 = AVCOL_TRC_SMPTE428, //
    AVCOL_TRC_ARIB_STD_B67 = 18,                 // < ARIB STD-B67, known as "Hybrid log-gamma"
    AVCOL_TRC_NB                                 // < Not part of ABI
    );

  (* *
    * YUV colorspace type.
  *)
  AVColorSpace = ( //
    AVCOL_SPC_RGB = 0,
    // < order of coefficients is actually GBR, also IEC 61966-2-1 (sRGB)
    AVCOL_SPC_BT709 = 1,
    // < also ITU-R BT1361 / IEC 61966-2-4 xvYCC709 / SMPTE RP177 Annex B
    AVCOL_SPC_UNSPECIFIED = 2, AVCOL_SPC_RESERVED = 3, AVCOL_SPC_FCC = 4,
    // < FCC Title 47 Code of Federal Regulations 73.682 (a)(20)
    AVCOL_SPC_BT470BG = 5,
    // < also ITU-R BT601-6 625 / ITU-R BT1358 625 / ITU-R BT1700 625 PAL & SECAM / IEC 61966-2-4 xvYCC601
    AVCOL_SPC_SMPTE170M = 6,
    // < also ITU-R BT601-6 525 / ITU-R BT1358 525 / ITU-R BT1700 NTSC
    AVCOL_SPC_SMPTE240M = 7,
    // < functionally identical to above
    AVCOL_SPC_YCGCO = 8,
    // < Used by Dirac / VC-2 and H.264 FRext, see ITU-T SG16
    AVCOL_SPC_YCOCG = AVCOL_SPC_YCGCO, AVCOL_SPC_BT2020_NCL = 9,
    // < ITU-R BT2020 non-constant luminance system
    AVCOL_SPC_BT2020_CL = 10,
    // < ITU-R BT2020 constant luminance system
    AVCOL_SPC_SMPTE2085 = 11,
    // < SMPTE 2085, Y'D'zD'x
    AVCOL_SPC_NB
    // < Not part of ABI
    );

  // #define AVCOL_SPC_YCGCO AVCOL_SPC_YCOCG

  (* *
    * MPEG vs JPEG YUV range.
  *)
  AVColorRange = ( //
    AVCOL_RANGE_UNSPECIFIED = 0, AVCOL_RANGE_MPEG = 1,
    // < the normal 219*2^(n-8) "MPEG" YUV ranges
    AVCOL_RANGE_JPEG = 2,
    // < the normal     2^n-1   "JPEG" YUV ranges
    AVCOL_RANGE_NB
    // < Not part of ABI
    );

  (* *
    * Location of chroma samples.
    *
    * Illustration showing the location of the first (top left) chroma sample of the
    * image, the left shows only luma, the right
    * shows the location of the chroma sample, the 2 could be imagined to overlay
    * each other but are drawn separately due to limitations of ASCII
    *
    *----------------1st 2nd       1st 2nd horizontal luma sample positions
    *-----------------v   v         v   v
    *                 ______        ______
    *1st luma line > |X   X ...    |3 4 X ...     X are luma samples,
    *----------------|             |1 2           1-6 are possible chroma positions
    *2nd luma line > |X   X ...    |5 6 X ...     0 is undefined/unknown position
  *)
  AVChromaLocation = ( //
    AVCHROMA_LOC_UNSPECIFIED = 0, AVCHROMA_LOC_LEFT = 1,
    // < MPEG-2/4 4:2:0, H.264 default for 4:2:0
    AVCHROMA_LOC_CENTER = 2,
    // < MPEG-1 4:2:0, JPEG 4:2:0, H.263 4:2:0
    AVCHROMA_LOC_TOPLEFT = 3,
    // < ITU-R 601, SMPTE 274M 296M S314M(DV 4:1:1), mpeg2 4:2:2
    AVCHROMA_LOC_TOP = 4, AVCHROMA_LOC_BOTTOMLEFT = 5, AVCHROMA_LOC_BOTTOM = 6, AVCHROMA_LOC_NB
    // < Not part of ABI
    );
{$ENDREGION}
{$REGION 'frame.h'}

const
  AV_NUM_DATA_POINTERS = 8;

Type
  TAVNDPArray = array [0 .. AV_NUM_DATA_POINTERS - 1] of int;
  pAVNDPArray = ^TAVNDPArray;
  TAVNDPArray_int = TAVNDPArray;
  pAVNDPArray_int = ^TAVNDPArray_int;

  TAVNDPArray_puint8_t = array [0 .. AV_NUM_DATA_POINTERS - 1] of puint8_t;
  pAVNDPArray_puint8_t = ^TAVNDPArray_puint8_t;

  TAVNDPArray_uint64_t = array [0 .. AV_NUM_DATA_POINTERS - 1] of uint64_t;

  TAVNDPArray_pAVBufferRef = array [0 .. AV_NUM_DATA_POINTERS - 1] of pAVBufferRef;
  pAVNDPArray_pAVBufferRef = ^TAVNDPArray_pAVBufferRef;

  // uint8_t * data[4];
  Tuint8_t_array_4 = array [0 .. 3] of uint8_t;
  puint8_t_array_4 = ^Tuint8_t_array_4;
  // int linesize[4];
  Tint_array_4 = array [0 .. 3] of int;

  AVFrameSideDataType = (
    (* *
      * The data is the AVPanScan struct defined in libavcodec.
    *)
    AV_FRAME_DATA_PANSCAN,
    (* *
      * ATSC A53 Part 4 Closed Captions.
      * A53 CC bitstream is stored as uint8_t in AVFrameSideData.data.
      * The number of bytes of CC data is AVFrameSideData.size.
    *)
    AV_FRAME_DATA_A53_CC,
    (* *
      * Stereoscopic 3d metadata.
      * The data is the AVStereo3D struct defined in libavutil/stereo3d.h.
    *)
    AV_FRAME_DATA_STEREO3D,
    (* *
      * The data is the AVMatrixEncoding enum defined in libavutil/channel_layout.h.
    *)
    AV_FRAME_DATA_MATRIXENCODING,
    (* *
      * Metadata relevant to a downmix procedure.
      * The data is the AVDownmixInfo struct defined in libavutil/downmix_info.h.
    *)
    AV_FRAME_DATA_DOWNMIX_INFO,
    (* *
      * ReplayGain information in the form of the AVReplayGain struct.
    *)
    AV_FRAME_DATA_REPLAYGAIN,
    (* *
      * This side data contains a 3x3 transformation matrix describing an affine
      * transformation that needs to be applied to the frame for correct
      * presentation.
      *
      * See libavutil/display.h for a detailed description of the data.
    *)
    AV_FRAME_DATA_DISPLAYMATRIX,
    (* *
      * Active Format Description data consisting of a single byte as specified
      * in ETSI TS 101 154 using AVActiveFormatDescription enum.
    *)
    AV_FRAME_DATA_AFD,
    (* *
      * Motion vectors exported by some codecs (on demand through the export_mvs
      * flag set in the libavcodec AVCodecContext flags2 option).
      * The data is the AVMotionVector struct defined in
      * libavutil/motion_vector.h.
    *)
    AV_FRAME_DATA_MOTION_VECTORS,
    (* *
      * Recommmends skipping the specified number of samples. This is exported
      * only if the "skip_manual" AVOption is set in libavcodec.
      * This has the same format as AV_PKT_DATA_SKIP_SAMPLES.
      * @code
      * u32le number of samples to skip from start of this packet
      * u32le number of samples to skip from end of this packet
      * u8    reason for start skip
      * u8    reason for end   skip (0=padding silence, 1=convergence)
      * @endcode
    *)
    AV_FRAME_DATA_SKIP_SAMPLES,
    (* *
      * This side data must be associated with an audio frame and corresponds to
      * enum AVAudioServiceType defined in avcodec.h.
    *)
    AV_FRAME_DATA_AUDIO_SERVICE_TYPE,
    (* *
      * Mastering display metadata associated with a video frame. The payload is
      * an AVMasteringDisplayMetadata type and contains information about the
      * mastering display color volume.
    *)
    AV_FRAME_DATA_MASTERING_DISPLAY_METADATA,
    (* *
      * The GOP timecode in 25 bit timecode format. Data format is 64-bit integer.
      * This is set on the first frame of a GOP that has a temporal reference of 0.
    *)
    AV_FRAME_DATA_GOP_TIMECODE,

    (* *
      * The data represents the AVSphericalMapping structure defined in
      * libavutil/spherical.h.
    *)
    AV_FRAME_DATA_SPHERICAL,

    (* *
      * Content light level (based on CTA-861.3). This payload contains data in
      * the form of the AVContentLightMetadata struct.
    *)
    AV_FRAME_DATA_CONTENT_LIGHT_LEVEL,

    (* *
      * The data contains an ICC profile as an opaque octet buffer following the
      * format described by ISO 15076-1 with an optional name defined in the
      * metadata key entry "name".
    *)
    AV_FRAME_DATA_ICC_PROFILE,

{$IFDEF FF_API_FRAME_QP}
    (* *
      * Implementation-specific description of the format of AV_FRAME_QP_TABLE_DATA.
      * The contents of this side data are undocumented and internal; use
      * av_frame_set_qp_table() and av_frame_get_qp_table() to access this in a
      * meaningful way instead.
    *)
    AV_FRAME_DATA_QP_TABLE_PROPERTIES,

    (* *
      * Raw QP table data. Its format is described by
      * AV_FRAME_DATA_QP_TABLE_PROPERTIES. Use av_frame_set_qp_table() and
      * av_frame_get_qp_table() to access this instead.
    *)
    AV_FRAME_DATA_QP_TABLE_DATA,
{$ENDIF}
    (*
      * Timecode which conforms to SMPTE ST 12-1. The data is an array of 4 uint32_t
      * where the first uint32_t describes how many (1-3) of the other timecodes are used.
      * The timecode format is described in the av_timecode_get_smpte_from_framenum()
      * function in libavutil/timecode.c.
    *)
    AV_FRAME_DATA_S12M_TIMECODE, //
    (*
      * HDR dynamic metadata associated with a video frame. The payload is
      * an AVDynamicHDRPlus type and contains information for color
      * volume transform - application 4 of SMPTE 2094-40:2016 standard.
    *)
    AV_FRAME_DATA_DYNAMIC_HDR_PLUS,

    (*
      * Regions Of Interest, the data is an array of AVRegionOfInterest type, the number of
      * array element is implied by AVFrameSideData.size / AVRegionOfInterest.self_size.
    *)
    AV_FRAME_DATA_REGIONS_OF_INTEREST //
    );

  AVActiveFormatDescription = ( //
    AV_AFD_SAME = 8, AV_AFD_4_3 = 9, AV_AFD_16_9 = 10, AV_AFD_14_9 = 11, AV_AFD_4_3_SP_14_9 = 13, AV_AFD_16_9_SP_14_9 = 14, AV_AFD_SP_4_3 = 15);

  (* *
    * Structure to hold side data for an AVFrame.
    *
    * sizeof(AVFrameSideData) is not a part of the public ABI, so new fields may be added
    * to the end with a minor bump.
  *)
  AVFrameSideData = record
    _type: AVFrameSideDataType;
    data: puint8_t;
    size: int;
    metadata: pAVDictionary;
    buf: pAVBufferRef;
  end;

  pAVFrameSideData = ^AVFrameSideData;
  ppAVFrameSideData = ^pAVFrameSideData;

  (*
    * Structure describing a single Region Of Interest.
    *
    * When multiple regions are defined in a single side-data block, they
    * should be ordered from most to least important - some encoders are only
    * capable of supporting a limited number of distinct regions, so will have
    * to truncate the list.
    *
    * When overlapping regions are defined, the first region containing a given
    * area of the frame applies.
  *)
  AVRegionOfInterest = record
    (*
      * Must be set to the size of this data structure (that is,
      * sizeof(AVRegionOfInterest)).
    *)
    self_size: uint32_t;
    (*
      * Distance in pixels from the top edge of the frame to the top and
      * bottom edges and from the left edge of the frame to the left and
      * right edges of the rectangle defining this region of interest.
      *
      * The constraints on a region are encoder dependent, so the region
      * actually affected may be slightly larger for alignment or other
      * reasons.
    *)
    top: int;
    bottom: int;
    left: int;
    right: int;
    (*
      * Quantisation offset.
      *
      * Must be in the range -1 to +1.  A value of zero indicates no quality
      * change.  A negative value asks for better quality (less quantisation),
      * while a positive value asks for worse quality (greater quantisation).
      *
      * The range is calibrated so that the extreme values indicate the
      * largest possible offset - if the rest of the frame is encoded with the
      * worst possible quality, an offset of -1 indicates that this region
      * should be encoded with the best possible quality anyway.  Intermediate
      * values are then interpolated in some codec-dependent way.
      *
      * For example, in 10-bit H.264 the quantisation parameter varies between
      * -12 and 51.  A typical qoffset value of -1/10 therefore indicates that
      * this region should be encoded with a QP around one-tenth of the full
      * range better than the rest of the frame.  So, if most of the frame
      * were to be encoded with a QP of around 30, this region would get a QP
      * of around 24 (an offset of approximately -1/10 * (51 - -12) = -6.3).
      * An extreme value of -1 would indicate that this region should be
      * encoded with the best possible quality regardless of the treatment of
      * the rest of the frame - that is, should be encoded at a QP of -12.
    *)
    qoffset: AVRational;
  end;

  pAVRegionOfInterest = ^AVRegionOfInterest;

  (* *
    * This structure describes decoded (raw) audio or video data.
    *
    * AVFrame must be allocated using av_frame_alloc(). Note that this only
    * allocates the AVFrame itself, the buffers for the data must be managed
    * through other means (see below).
    * AVFrame must be freed with av_frame_free().
    *
    * AVFrame is typically allocated once and then reused multiple times to hold
    * different data (e.g. a single AVFrame to hold frames received from a
    * decoder). In such a case, av_frame_unref() will free any references held by
    * the frame and reset it to its original clean state before it
    * is reused again.
    *
    * The data described by an AVFrame is usually reference counted through the
    * AVBuffer API. The underlying buffer references are stored in AVFrame.buf /
    * AVFrame.extended_buf. An AVFrame is considered to be reference counted if at
    * least one reference is set, i.e. if AVFrame.buf[0] != NULL. In such a case,
    * every single data plane must be contained in one of the buffers in
    * AVFrame.buf or AVFrame.extended_buf.
    * There may be a single buffer for all the data, or one separate buffer for
    * each plane, or anything in between.
    *
    * sizeof(AVFrame) is not a part of the public ABI, so new fields may be added
    * to the end with a minor bump.
    *
    * Fields can be accessed through AVOptions, the name string used, matches the
    * C structure field name for fields accessible through AVOptions. The AVClass
    * for AVFrame can be obtained from avcodec_get_frame_class()
  *)
  (* *
    * @defgroup lavu_frame_flags AV_FRAME_FLAGS
    * @ingroup lavu_frame
    * Flags describing additional frame properties.
  *)
const
  (* *
    * The frame data may be corrupted, e.g. due to decoding errors.
  *)
  AV_FRAME_FLAG_CORRUPT = (1 shl 0);
  (* *
    * A flag to mark the frames which need to be decoded, but shouldn't be output.
  *)
  AV_FRAME_FLAG_DISCARD = (1 shl 2);

  // AVFrame ->   decode_error_flags:int;
  FF_DECODE_ERROR_INVALID_BITSTREAM  = 1;
  FF_DECODE_ERROR_MISSING_REFERENCE  = 2;
  FF_DECODE_ERROR_CONCEALMENT_ACTIVE = 4;
  FF_DECODE_ERROR_DECODE_SLICES      = 8;

type

  pAVFrame = ^AVFrame;

  AVFrame = record
    (* *
      * pointer to the picture/channel planes.
      * This might be different from the first allocated byte
      *
      * Some decoders access areas outside 0,0 - width,height, please
      * see avcodec_align_dimensions2(). Some filters and swscale can read
      * up to 16 bytes beyond the planes, if these filters are to be used,
      * then 16 extra bytes must be allocated.
      *
      * NOTE: Except for hwaccel formats, pointers not needed by the format
      * MUST be set to NULL.
    *)
    data: TAVNDPArray_puint8_t;

    (* *
      * For video, size in bytes of each picture line.
      * For audio, size in bytes of each plane.
      *
      * For audio, only linesize[0] may be set. For planar audio, each channel
      * plane must be the same size.
      *
      * For video the linesizes should be multiples of the CPUs alignment
      * preference, this is 16 or 32 for modern desktop CPUs.
      * Some code requires such alignment other code can be slower without
      * correct alignment, for yet other it makes no difference.
      *
      * @note The linesize may be larger than the size of usable data -- there
      * may be extra padding present for performance reasons.
    *)
    linesize: TAVNDPArray_int;

    (* *
      * pointers to the data planes/channels.
      *
      * For video, this should simply point to data[].
      *
      * For planar audio, each channel has a separate data pointer, and
      * linesize[0] contains the size of each channel buffer.
      * For packed audio, there is just one data pointer, and linesize[0]
      * contains the total size of the buffer for all channels.
      *
      * Note: Both data and extended_data should always be set in a valid frame,
      * but for planar audio with more channels that can fit in data,
      * extended_data must be used in order to access all channels.
    *)
    extended_data: ppuint8_t;

    (* *
      * @name Video dimensions
      * Video frames only. The coded dimensions (in pixels) of the video frame,
      * i.e. the size of the rectangle that contains some well-defined values.
      *
      * @note The part of the frame intended for display/presentation is further
      * restricted by the @ref cropping "Cropping rectangle".
      * @{
    *)
    width, height: int;
    (* *
      * @}
    *)

    (* *
      * number of audio samples (per channel) described by this frame
    *)
    nb_samples: int;

    (* *
      * format of the frame, -1 if unknown or unset
      * Values correspond to enum AVPixelFormat for video frames,
      * enum AVSampleFormat for audio)
    *)
    format: int;

    (* *
      * 1 -> keyframe, 0-> not
    *)
    key_frame: int;

    (* *
      * Picture type of the frame.
    *)
    pict_type: AVPictureType;

    (* *
      * Sample aspect ratio for the video frame, 0/1 if unknown/unspecified.
    *)
    sample_aspect_ratio: AVRational;

    (* *
      * Presentation timestamp in time_base units (time when frame should be shown to user).
    *)
    pts: int64_t;

{$IFDEF FF_API_PKT_PTS}
    (* *
      * PTS copied from the AVPacket that was decoded to produce this frame.
      * @deprecated use the pts field instead
    *)
    // attribute_deprecated
    pkt_pts: int64_t;
{$ENDIF}
    (* *
      * DTS copied from the AVPacket that triggered returning this frame. (if frame threading isn't used)
      * This is also the Presentation time of this AVFrame calculated from
      * only AVPacket.dts values without pts values.
    *)
    pkt_dts: int64_t;

    (* *
      * picture number in bitstream order
    *)
    coded_picture_number: int;
    (* *
      * picture number in display order
    *)
    display_picture_number: int;

    (* *
      * quality (between 1 (good) and FF_LAMBDA_MAX (bad))
    *)
    quality: int;

    (* *
      * for some private data of the user
    *)
    opaque: Pointer;

{$IFDEF FF_API_ERROR_FRAME}
    (* *
      * @deprecated unused
    *)
    // attribute_deprecated
    error: TAVNDPArray_puint8_t;
{$ENDIF}
    (* *
      * When decoding, this signals how much the picture must be delayed.
      * extra_delay = repeat_pict / (2*fps)
    *)
    repeat_pict: int;

    (* *
      * The content of the picture is interlaced.
    *)
    interlaced_frame: int;

    (* *
      * If the content is interlaced, is top field displayed first.
    *)
    top_field_first: int;

    (* *
      * Tell user application that palette has changed from previous frame.
    *)
    palette_has_changed: int;

    (* *
      * reordered opaque 64 bits (generally an integer or a double precision float
      * PTS but can be anything).
      * The user sets AVCodecContext.reordered_opaque to represent the input at
      * that time,
      * the decoder reorders values as needed and sets AVFrame.reordered_opaque
      * to exactly one of the values provided by the user through AVCodecContext.reordered_opaque
    *)
    reordered_opaque: int64_t;

    (* *
      * Sample rate of the audio data.
    *)
    sample_rate: int;

    (* *
      * Channel layout of the audio data.
    *)
    channel_layout: uint64_t;

    (* *
      * AVBuffer references backing the data for this frame. If all elements of
      * this array are NULL, then this frame is not reference counted. This array
      * must be filled contiguously -- if buf[i] is non-NULL then buf[j] must
      * also be non-NULL for all j < i.
      *
      * There may be at most one AVBuffer per data plane, so for video this array
      * always contains all the references. For planar audio with more than
      * AV_NUM_DATA_POINTERS channels, there may be more buffers than can fit in
      * this array. Then the extra AVBufferRef pointers are stored in the
      * extended_buf array.
    *)
    buf: TAVNDPArray_pAVBufferRef;

    (* *
      * For planar audio which requires more than AV_NUM_DATA_POINTERS
      * AVBufferRef pointers, this array will hold all the references which
      * cannot fit into AVFrame.buf.
      *
      * Note that this is different from AVFrame.extended_data, which always
      * contains all the pointers. This array only contains the extra pointers,
      * which cannot fit into AVFrame.buf.
      *
      * This array is always allocated using av_malloc() by whoever constructs
      * the frame. It is freed in av_frame_unref().
    *)
    extended_buf: ppAVBufferRef;
    (* *
      * Number of elements in extended_buf.
    *)
    nb_extended_buf: int;

    side_data: ppAVFrameSideData;
    nb_side_data: int;

    (* *
      * Frame flags, a combination of @ref lavu_frame_flags
    *)
    flags: int;

    (* *
      * MPEG vs JPEG YUV range.
      * - encoding: Set by user
      * - decoding: Set by libavcodec
    *)
    color_range: AVColorRange;

    color_primaries: AVColorPrimaries;

    color_trc: AVColorTransferCharacteristic;

    (* *
      * YUV colorspace type.
      * - encoding: Set by user
      * - decoding: Set by libavcodec
    *)
    colorspace: AVColorSpace;

    chroma_location: AVChromaLocation;

    (* *
      * frame timestamp estimated using various heuristics, in stream time base
      * - encoding: unused
      * - decoding: set by libavcodec, read by user.
    *)
    best_effort_timestamp: int64_t;

    (* *
      * reordered pos from the last AVPacket that has been input into the decoder
      * - encoding: unused
      * - decoding: Read by user.
    *)
    pkt_pos: int64_t;

    (* *
      * duration of the corresponding packet, expressed in
      * AVStream->time_base units, 0 if unknown.
      * - encoding: unused
      * - decoding: Read by user.
    *)
    pkt_duration: int64_t;

    (* *
      * metadata.
      * - encoding: Set by user.
      * - decoding: Set by libavcodec.
    *)
    metadata: pAVDictionary;

    (* *
      * decode error flags of the frame, set to a combination of
      * FF_DECODE_ERROR_xxx flags if the decoder produced a frame, but there
      * were errors during the decoding.
      * - encoding: unused
      * - decoding: set by libavcodec, read by user.
    *)
    decode_error_flags: int;

    (* *
      * number of audio channels, only used for audio.
      * - encoding: unused
      * - decoding: Read by user.
    *)
    channels: int;

    (* *
      * size of the corresponding packet containing the compressed
      * frame.
      * It is set to a negative value if unknown.
      * - encoding: unused
      * - decoding: set by libavcodec, read by user.
    *)
    pkt_size: int;

{$IFDEF FF_API_FRAME_QP}
    (* *
      * QP table
    *)
    // attribute_deprecated
    qscale_table: pint8_t;
    (* *
      * QP store stride
    *)
    // attribute_deprecated
    qstride: int;

    // attribute_deprecated
    qscale_type: int;

    // attribute_deprecated
    qp_table_buf: pAVBufferRef;
{$ENDIF}
    (* *
      * For hwaccel-format frames, this should be a reference to the
      * AVHWFramesContext describing the frame.
    *)
    hw_frames_ctx: pAVBufferRef;

    (* *
      * AVBufferRef for free use by the API user. FFmpeg will never check the
      * contents of the buffer ref. FFmpeg calls av_buffer_unref() on it when
      * the frame is unreferenced. av_frame_copy_props() calls create a new
      * reference with av_buffer_ref() for the target frame's opaque_ref field.
      *
      * This is unrelated to the opaque field, although it serves a similar
      * purpose.
    *)
    opaque_ref: pAVBufferRef;

    (* *
      * @anchor cropping
      * @name Cropping
      * Video frames only. The number of pixels to discard from the the
      * top/bottom/left/right border of the frame to obtain the sub-rectangle of
      * the frame intended for presentation.
      * @{
    *)
    crop_top: size_t;
    crop_bottom: size_t;
    crop_left: size_t;
    crop_right: size_t;
    (* *
      * @}
    *)

    (* *
      * AVBufferRef for internal use by a single libav* library.
      * Must not be used to transfer data between libraries.
      * Has to be NULL when ownership of the frame leaves the respective library.
      *
      * Code outside the FFmpeg libs should never check or change the contents of the buffer ref.
      *
      * FFmpeg calls av_buffer_unref() on it when the frame is unreferenced.
      * av_frame_copy_props() calls create a new reference with av_buffer_ref()
      * for the target frame's private_ref field.
    *)
    private_ref: pAVBufferRef;
  end;

{$IFDEF FF_API_FRAME_GET_SET}

  (* *
    * Accessors for some AVFrame fields. These used to be provided for ABI
    * compatibility, and do not need to be used anymore.
  *)
  // attribute_deprecated
  // int64_t av_frame_get_best_effort_timestamp(const AVFrame *frame);
function av_frame_get_best_effort_timestamp(const frame: pAVFrame): int64_t; cdecl; external avutil_dll;
// attribute_deprecated
// void    av_frame_set_best_effort_timestamp(AVFrame *frame, int64_t val);
procedure av_frame_set_best_effort_timestamp(frame: pAVFrame; val: int64_t); cdecl; external avutil_dll;
// attribute_deprecated
// int64_t av_frame_get_pkt_duration         (const AVFrame *frame);
function av_frame_get_pkt_duration(const frame: pAVFrame): int64_t; cdecl; external avutil_dll;
// attribute_deprecated
// void    av_frame_set_pkt_duration         (AVFrame *frame, int64_t val);
procedure av_frame_set_pkt_duration(frame: pAVFrame; val: int64_t); cdecl; external avutil_dll;
// attribute_deprecated
// int64_t av_frame_get_pkt_pos              (const AVFrame *frame);
function av_frame_get_pkt_pos(const frame: pAVFrame): int64_t; cdecl; external avutil_dll;
// attribute_deprecated
// void    av_frame_set_pkt_pos              (AVFrame *frame, int64_t val);
procedure av_frame_set_pkt_pos(frame: pAVFrame; val: int64_t); cdecl; external avutil_dll;
// attribute_deprecated
// int64_t av_frame_get_channel_layout       (const AVFrame *frame);
function av_frame_get_channel_layout(const frame: pAVFrame): int64_t; cdecl; external avutil_dll;
// attribute_deprecated
// void    av_frame_set_channel_layout       (AVFrame *frame, int64_t val);
procedure av_frame_set_channel_layout(frame: pAVFrame; val: int64_t); cdecl; external avutil_dll;
// attribute_deprecated
// int     av_frame_get_channels             (const AVFrame *frame);
function av_frame_get_channels(const frame: pAVFrame): int; cdecl; external avutil_dll;
// attribute_deprecated
// void    av_frame_set_channels             (AVFrame *frame, int     val);
procedure av_frame_set_channels(frame: pAVFrame; val: int); cdecl; external avutil_dll;
// attribute_deprecated
// int     av_frame_get_sample_rate          (const AVFrame *frame);
function av_frame_get_sample_rate(const frame: pAVFrame): int; cdecl; external avutil_dll;
// attribute_deprecated
// void    av_frame_set_sample_rate          (AVFrame *frame, int     val);
procedure av_frame_set_sample_rate(frame: pAVFrame; val: int); cdecl; external avutil_dll;
// attribute_deprecated
// AVDictionary *av_frame_get_metadata       (const AVFrame *frame);
function av_frame_get_metadata(const frame: AVFrame): pAVDictionary; cdecl; external avutil_dll;
// attribute_deprecated
// void          av_frame_set_metadata       (AVFrame *frame, AVDictionary *val);
procedure av_frame_set_metadata(frame: pAVFrame; val: pAVDictionary); cdecl; external avutil_dll;
// attribute_deprecated
// int     av_frame_get_decode_error_flags   (const AVFrame *frame);
function av_frame_get_decode_error_flags(const frame: pAVFrame): int; cdecl; external avutil_dll;
// attribute_deprecated
// void    av_frame_set_decode_error_flags   (AVFrame *frame, int     val);
procedure av_frame_set_decode_error_flags(frame: pAVFrame; val: int); cdecl; external avutil_dll;
// attribute_deprecated
// int     av_frame_get_pkt_size(const AVFrame *frame);
function av_frame_get_pkt_size(const frame: pAVFrame): int; cdecl; external avutil_dll;
// attribute_deprecated
// void    av_frame_set_pkt_size(AVFrame *frame, int val);
procedure av_frame_set_pkt_size(frame: AVFrame; val: int); cdecl; external avutil_dll;
{$IFDEF FF_API_FRAME_QP}
// attribute_deprecated
// int8_t *av_frame_get_qp_table(AVFrame *f, int *stride, int *type);
function av_frame_get_qp_table(f: pAVFrame; stride: pint; _type: pint): pint8_t; cdecl; external avutil_dll;
// attribute_deprecated
// int av_frame_set_qp_table(AVFrame *f, AVBufferRef *buf, int stride, int type);
function av_frame_set_qp_table(f: pAVFrame; buf: pAVBufferRef; stride: int; _type: int): int; cdecl; external avutil_dll;
{$ENDIF}
// attribute_deprecated
// enum AVColorSpace av_frame_get_colorspace(const AVFrame *frame);
function av_frame_get_colorspace(const frame: pAVFrame): AVColorSpace; cdecl; external avutil_dll;
// attribute_deprecated
// void    av_frame_set_colorspace(AVFrame *frame, enum AVColorSpace val);
procedure av_frame_set_colorspace(frame: pAVFrame; val: AVColorSpace); cdecl; external avutil_dll;
// attribute_deprecated
// enum AVColorRange av_frame_get_color_range(const AVFrame *frame);
function av_frame_get_color_range(const frame: pAVFrame): AVColorRange; cdecl; external avutil_dll;
// attribute_deprecated
// void    av_frame_set_color_range(AVFrame *frame, enum AVColorRange val);
procedure av_frame_set_color_range(frame: pAVFrame; val: AVColorRange); cdecl; external avutil_dll;
{$ENDIF}
(* *
  * Get the name of a colorspace.
  * @return a static string identifying the colorspace; can be NULL.
*)
// const char *av_get_colorspace_name(enum AVColorSpace val);
function av_get_colorspace_name(val: AVColorSpace): PAnsiChar; cdecl; external avutil_dll;

(* *
  * Allocate an AVFrame and set its fields to default values.  The resulting
  * struct must be freed using av_frame_free().
  *
  * @return An AVFrame filled with default values or NULL on failure.
  *
  * @note this only allocates the AVFrame itself, not the data buffers. Those
  * must be allocated through other means, e.g. with av_frame_get_buffer() or
  * manually.
*)
// AVFrame *av_frame_alloc(void);
function av_frame_alloc(): pAVFrame; cdecl; external avutil_dll;

(* *
  * Free the frame and any dynamically allocated objects in it,
  * e.g. extended_data. If the frame is reference counted, it will be
  * unreferenced first.
  *
  * @param frame frame to be freed. The pointer will be set to NULL.
*)
// void av_frame_free(AVFrame **frame);
procedure av_frame_free(Var frame: pAVFrame); cdecl; external avutil_dll;

(* *
  * Set up a new reference to the data described by the source frame.
  *
  * Copy frame properties from src to dst and create a new reference for each
  * AVBufferRef from src.
  *
  * If src is not reference counted, new buffers are allocated and the data is
  * copied.
  *
  * @warning: dst MUST have been either unreferenced with av_frame_unref(dst),
  *           or newly allocated with av_frame_alloc() before calling this
  *           function, or undefined behavior will occur.
  *
  * @return 0 on success, a negative AVERROR on error
*)
// int av_frame_ref(AVFrame *dst, const AVFrame *src);
function av_frame_ref(dst: pAVFrame; const src: pAVFrame): int; cdecl; external avutil_dll;

(* *
  * Create a new frame that references the same data as src.
  *
  * This is a shortcut for av_frame_alloc()+av_frame_ref().
  *
  * @return newly created AVFrame on success, NULL on error.
*)
// AVFrame *av_frame_clone(const AVFrame *src);
function av_frame_clone(const src: pAVFrame): pAVFrame; cdecl; external avutil_dll;

(* *
  * Unreference all the buffers referenced by frame and reset the frame fields.
*)
// void av_frame_unref(AVFrame *frame);
procedure av_frame_unref(frame: pAVFrame); cdecl; external avutil_dll;

(* *
  * Move everything contained in src to dst and reset src.
  *
  * @warning: dst is not unreferenced, but directly overwritten without reading
  *           or deallocating its contents. Call av_frame_unref(dst) manually
  *           before calling this function to ensure that no memory is leaked.
*)
// void av_frame_move_ref(AVFrame *dst, AVFrame *src);
procedure av_frame_move_ref(dst: pAVFrame; src: pAVFrame); cdecl; external avutil_dll;

(* *
  * Allocate new buffer(s) for audio or video data.
  *
  * The following fields must be set on frame before calling this function:
  * - format (pixel format for video, sample format for audio)
  * - width and height for video
  * - nb_samples and channel_layout for audio
  *
  * This function will fill AVFrame.data and AVFrame.buf arrays and, if
  * necessary, allocate and fill AVFrame.extended_data and AVFrame.extended_buf.
  * For planar formats, one buffer will be allocated for each plane.
  *
  * @warning: if frame already has been allocated, calling this function will
  *           leak memory. In addition, undefined behavior can occur in certain
  *           cases.
  *
  * @param frame frame in which to store the new buffers.
  * @param align Required buffer size alignment. If equal to 0, alignment will be
  *              chosen automatically for the current CPU. It is highly
  *              recommended to pass 0 here unless you know what you are doing.
  *
  * @return 0 on success, a negative AVERROR on error.
*)
// int av_frame_get_buffer(AVFrame *frame, int align);
function av_frame_get_buffer(frame: pAVFrame; align: int): int; cdecl; external avutil_dll;

(* *
  * Check if the frame data is writable.
  *
  * @return A positive value if the frame data is writable (which is true if and
  * only if each of the underlying buffers has only one reference, namely the one
  * stored in this frame). Return 0 otherwise.
  *
  * If 1 is returned the answer is valid until av_buffer_ref() is called on any
  * of the underlying AVBufferRefs (e.g. through av_frame_ref() or directly).
  *
  * @see av_frame_make_writable(), av_buffer_is_writable()
*)
// int av_frame_is_writable(AVFrame *frame);
function av_frame_is_writable(frame: pAVFrame): int; cdecl; external avutil_dll;

(* *
  * Ensure that the frame data is writable, avoiding data copy if possible.
  *
  * Do nothing if the frame is writable, allocate new buffers and copy the data
  * if it is not.
  *
  * @return 0 on success, a negative AVERROR on error.
  *
  * @see av_frame_is_writable(), av_buffer_is_writable(),
  * av_buffer_make_writable()
*)
// int av_frame_make_writable(AVFrame *frame);
function av_frame_make_writable(frame: pAVFrame): int; cdecl; external avutil_dll;

(* *
  * Copy the frame data from src to dst.
  *
  * This function does not allocate anything, dst must be already initialized and
  * allocated with the same parameters as src.
  *
  * This function only copies the frame data (i.e. the contents of the data /
  * extended data arrays), not any other properties.
  *
  * @return >= 0 on success, a negative AVERROR on error.
*)
// int av_frame_copy(AVFrame *dst, const AVFrame *src);
function av_frame_copy(dst: pAVFrame; const src: pAVFrame): int; cdecl; external avutil_dll;

(* *
  * Copy only "metadata" fields from src to dst.
  *
  * Metadata for the purpose of this function are those fields that do not affect
  * the data layout in the buffers.  E.g. pts, sample rate (for audio) or sample
  * aspect ratio (for video), but not width/height or channel layout.
  * Side data is also copied.
*)
// int av_frame_copy_props(AVFrame *dst, const AVFrame *src);
function av_frame_copy_props(dst: pAVFrame; const src: pAVFrame): int; cdecl; external avutil_dll;

(* *
  * Get the buffer reference a given data plane is stored in.
  *
  * @param plane index of the data plane of interest in frame->extended_data.
  *
  * @return the buffer reference that contains the plane or NULL if the input
  * frame is not valid.
*)
// AVBufferRef *av_frame_get_plane_buffer(AVFrame *frame, int plane);
function av_frame_get_plane_buffer(frame: pAVFrame; plane: int): pAVBufferRef; cdecl; external avutil_dll;

(* *
  * Add a new side data to a frame.
  *
  * @param frame a frame to which the side data should be added
  * @param type type of the added side data
  * @param size size of the side data
  *
  * @return newly added side data on success, NULL on error
*)
// AVFrameSideData *av_frame_new_side_data(AVFrame *frame,
// enum AVFrameSideDataType type,
// int size);
function av_frame_new_side_data(frame: AVFrame; _type: AVFrameSideDataType; size: int): pAVFrameSideData; cdecl; external avutil_dll;

(* *
  * Add a new side data to a frame from an existing AVBufferRef
  *
  * @param frame a frame to which the side data should be added
  * @param type  the type of the added side data
  * @param buf   an AVBufferRef to add as side data. The ownership of
  *              the reference is transferred to the frame.
  *
  * @return newly added side data on success, NULL on error. On failure
  *         the frame is unchanged and the AVBufferRef remains owned by
  *         the caller.
*)
// AVFrameSideData *av_frame_new_side_data_from_buf(AVFrame *frame,
// enum AVFrameSideDataType type,
// AVBufferRef *buf);
function av_frame_new_side_data_from_buf(frame: pAVFrame; _type: AVFrameSideDataType; buf: pAVBufferRef): pAVFrameSideData; cdecl; external avutil_dll;

(* *
  * @return a pointer to the side data of a given type on success, NULL if there
  * is no side data with such type in this frame.
*)
// AVFrameSideData *av_frame_get_side_data(const AVFrame *frame,
// enum AVFrameSideDataType type);
function av_frame_get_side_data(const frame: pAVFrame; _type: AVFrameSideDataType): pAVFrameSideData; cdecl; external avutil_dll;

(* *
  * If side data of the supplied type exists in the frame, free it and remove it
  * from the frame.
*)
// void av_frame_remove_side_data(AVFrame *frame, enum AVFrameSideDataType type);
procedure av_frame_remove_side_data(frame: pAVFrame; _type: AVFrameSideDataType); cdecl; external avutil_dll;

const
  (* *
    * Flags for frame cropping.
  *)

  (* *
    * Apply the maximum possible cropping, even if it requires setting the
    * AVFrame.data[] entries to unaligned pointers. Passing unaligned data
    * to FFmpeg API is generally not allowed, and causes undefined behavior
    * (such as crashes). You can pass unaligned data only to FFmpeg APIs that
    * are explicitly documented to accept it. Use this flag only if you
    * absolutely know what you are doing.
  *)
  AV_FRAME_CROP_UNALIGNED = 1 shl 0;

  (* *
    * Crop the given video AVFrame according to its crop_left/crop_top/crop_right/
    * crop_bottom fields. If cropping is successful, the function will adjust the
    * data pointers and the width/height fields, and set the crop fields to 0.
    *
    * In all cases, the cropping boundaries will be rounded to the inherent
    * alignment of the pixel format. In some cases, such as for opaque hwaccel
    * formats, the left/top cropping is ignored. The crop fields are set to 0 even
    * if the cropping was rounded or ignored.
    *
    * @param frame the frame which should be cropped
    * @param flags Some combination of AV_FRAME_CROP_* flags, or 0.
    *
    * @return >= 0 on success, a negative AVERROR on error. If the cropping fields
    * were invalid, AVERROR(ERANGE) is returned, and nothing is changed.
  *)
  // int av_frame_apply_cropping(AVFrame *frame, int flags);
function av_frame_apply_cropping(frame: pAVFrame; flags: int): int; cdecl; external avutil_dll;

(* *
  * @return a string identifying the side data type
*)
// const char *av_frame_side_data_name(enum AVFrameSideDataType type);
function av_frame_side_data_name(_type: AVFrameSideDataType): PAnsiChar; cdecl; external avutil_dll;

{$ENDREGION}
{$REGION 'framequeue.h'}

type
  pFFFrameBucket = ^FFFrameBucket;

  FFFrameBucket = record
    frame: pAVFrame;
  end;

  (* *
    * Structure to hold global options and statistics for frame queues.
    *
    * This structure is intended to allow implementing global control of the
    * frame queues, including memory consumption caps.
    *
    * It is currently empty.
  *)
  pFFFrameQueueGlobal = ^FFFrameQueueGlobal;

  FFFrameQueueGlobal = record
    dummy: AnsiChar; (* C does not allow empty structs *)
  end;

  (* *
    * Queue of AVFrame pointers.
  *)
  pFFFrameQueue = ^FFFrameQueue;

  FFFrameQueue = record

    (* *
      * Array of allocated buckets, used as a circular buffer.
    *)
    queue: pFFFrameBucket;

    (* *
      * Size of the array of buckets.
    *)
    allocated: size_t;

    (* *
      * Tail of the queue.
      * It is the index in the array of the next frame to take.
    *)
    tail: size_t;

    (* *
      * Number of currently queued frames.
    *)
    queued: size_t;

    (* *
      * Pre-allocated bucket for queues of size 1.
    *)
    first_bucket: FFFrameBucket;

    (* *
      * Total number of frames entered in the queue.
    *)
    total_frames_head: uint64_t;

    (* *
      * Total number of frames dequeued from the queue.
      * queued = total_frames_head - total_frames_tail
    *)
    total_frames_tail: uint64_t;

    (* *
      * Total number of samples entered in the queue.
    *)
    total_samples_head: uint64_t;

    (* *
      * Total number of samples dequeued from the queue.
      * queued_samples = total_samples_head - total_samples_tail
    *)
    total_samples_tail: uint64_t;

    (* *
      * Indicate that samples are skipped
    *)
    samples_skipped: int;
  end;

{$ENDREGION}
{$REGION 'opt.h'}

Type
  AVOptionType = ( //
    AV_OPT_TYPE_FLAGS, AV_OPT_TYPE_INT, AV_OPT_TYPE_INT64, AV_OPT_TYPE_DOUBLE, AV_OPT_TYPE_FLOAT, AV_OPT_TYPE_STRING, AV_OPT_TYPE_RATIONAL, AV_OPT_TYPE_BINARY,
    // < offset must point to a pointer immediately followed by an int for the length
    AV_OPT_TYPE_DICT, AV_OPT_TYPE_UINT64, AV_OPT_TYPE_CONST, AV_OPT_TYPE_IMAGE_SIZE,
    // < offset must point to two consecutive integers
    AV_OPT_TYPE_PIXEL_FMT, AV_OPT_TYPE_SAMPLE_FMT, AV_OPT_TYPE_VIDEO_RATE,
    // < offset must point to AVRational
    AV_OPT_TYPE_DURATION, AV_OPT_TYPE_COLOR, AV_OPT_TYPE_CHANNEL_LAYOUT, AV_OPT_TYPE_BOOL);

  (* *
    * AVOption
  *)

  Tdefault_val = record
    case int of
      0:
        (i64: int64_t);
      1:
        (dbl: double);
      2:
        (str: PAnsiChar);
      (* TODO those are unused now *)
      3:
        (q: AVRational);
  end;

  AVOption = record
    // const char *name;
    name: PAnsiChar;

    (* *
      * short English help text
      * @todo What about other languages?
    *)
    // const char *help;
    help: PAnsiChar;

    (* *
      * The offset relative to the context structure where the option
      * value is stored. It should be 0 for named constants.
    *)
    // int offset;
    offset: int;
    // enum AVOptionType type;
    _type: AVOptionType;

    (* *
      * the default value for scalar options
    *)
    default_val: Tdefault_val;

    min: double;
    // < minimum valid value for the option
    max: double;
    // < maximum valid value for the option

    flags: int;

    (* *
      * The logical unit to which the option belongs. Non-constant
      * options and corresponding named constants share the same
      * unit. May be NULL.
    *)
    // const char *unit;
    _unit: PAnsiChar;
  end;

  pAVOption = ^AVOption;

  (* *
    * A single allowed range of values, or a single allowed value.
  *)
  AVOptionRange = record
    // const char *str;
    str: PAnsiChar;
    (* *
      * Value range.
      * For string ranges this represents the min/max length.
      * For dimensions this represents the min/max pixel count or width/height in multi-component case.
    *)
    value_min, value_max: double;
    (* *
      * Value's component range.
      * For string this represents the unicode range for chars, 0-127 limits to ASCII.
    *)
    component_min, component_max: double;
    (* *
      * Range flag.
      * If set to 1 the struct encodes a range, if set to 0 a single value.
    *)
    is_range: int;
  end;

  pAVOptionRange = ^AVOptionRange;
  ppAVOptionRange = ^pAVOptionRange;

  (* *
    * List of AVOptionRange structs.
  *)
  AVOptionRanges = record
    (* *
      * Array of option ranges.
      *
      * Most of option types use just one component.
      * Following describes multi-component option types:
      *
      * AV_OPT_TYPE_IMAGE_SIZE:
      * component index 0: range of pixel count (width * height).
      * component index 1: range of width.
      * component index 2: range of height.
      *
      * @note To obtain multi-component version of this structure, user must
      *       provide AV_OPT_MULTI_COMPONENT_RANGE to av_opt_query_ranges or
      *       av_opt_query_ranges_default function.
      *
      * Multi-component range can be read as in following example:
      *
      * @code
      * int range_index, component_index;
      * AVOptionRanges *ranges;
      * AVOptionRange *range[3]; //may require more than 3 in the future.
      * av_opt_query_ranges(&ranges, obj, key, AV_OPT_MULTI_COMPONENT_RANGE);
      * for (range_index = 0; range_index < ranges->nb_ranges; range_index++) {
      *     for (component_index = 0; component_index < ranges->nb_components; component_index++)
      *         range[component_index] = ranges->range[ranges->nb_ranges * component_index + range_index];
      *     //do something with range here.
      * }
      * av_opt_freep_ranges(&ranges);
      * @endcode
    *)
    // AVOptionRange **range;
    range: ppAVOptionRange;
    (* *
      * Number of ranges per component.
    *)
    nb_ranges: int;
    (* *
      * Number of componentes.
    *)
    nb_components: int;
  end;

  pAVOptionRanges = ^AVOptionRanges;

const
  AV_OPT_FLAG_ENCODING_PARAM = 1;
  // < a generic parameter which can be set by the user for muxing or encoding
  AV_OPT_FLAG_DECODING_PARAM = 2;
  // < a generic parameter which can be set by the user for demuxing or decoding
  AV_OPT_FLAG_AUDIO_PARAM    = 8;
  AV_OPT_FLAG_VIDEO_PARAM    = 16;
  AV_OPT_FLAG_SUBTITLE_PARAM = 32;
  (* *
    * The option is intended for exporting values to the caller.
  *)
  AV_OPT_FLAG_EXPORT = 64;
  (* *
    * The option may not be set through the AVOptions API, only read.
    * This flag only makes sense when AV_OPT_FLAG_EXPORT is also set.
  *)
  AV_OPT_FLAG_READONLY  = 128;
  AV_OPT_FLAG_BSF_PARAM = (1 shl 8);
  // < a generic parameter which can be set by the user for bit stream filtering
  AV_OPT_FLAG_FILTERING_PARAM = (1 shl 16);

{$ENDREGION}
{$REGION 'log.h'}

type
  AVClassCategory = (                           //
    AV_CLASS_CATEGORY_NA = 0,                   //
    AV_CLASS_CATEGORY_INPUT,                    //
    AV_CLASS_CATEGORY_OUTPUT,                   //
    AV_CLASS_CATEGORY_MUXER,                    //
    AV_CLASS_CATEGORY_DEMUXER,                  //
    AV_CLASS_CATEGORY_ENCODER,                  //
    AV_CLASS_CATEGORY_DECODER,                  //
    AV_CLASS_CATEGORY_FILTER,                   //
    AV_CLASS_CATEGORY_BITSTREAM_FILTER,         //
    AV_CLASS_CATEGORY_SWSCALER,                 //
    AV_CLASS_CATEGORY_SWRESAMPLER,              //
    AV_CLASS_CATEGORY_DEVICE_VIDEO_OUTPUT = 40, //
    AV_CLASS_CATEGORY_DEVICE_VIDEO_INPUT,       //
    AV_CLASS_CATEGORY_DEVICE_AUDIO_OUTPUT,      //
    AV_CLASS_CATEGORY_DEVICE_AUDIO_INPUT,       //
    AV_CLASS_CATEGORY_DEVICE_OUTPUT,            //
    AV_CLASS_CATEGORY_DEVICE_INPUT,             //
    AV_CLASS_CATEGORY_NB
    // < not part of ABI/API
    );
  pAVClassCategory = ^AVClassCategory;

  // #define AV_IS_INPUT_DEVICE(category) \
  // (((category) == AV_CLASS_CATEGORY_DEVICE_VIDEO_INPUT) || \
  // ((category) == AV_CLASS_CATEGORY_DEVICE_AUDIO_INPUT) || \
  // ((category) == AV_CLASS_CATEGORY_DEVICE_INPUT))
  //
  // #define AV_IS_OUTPUT_DEVICE(category) \
  // (((category) == AV_CLASS_CATEGORY_DEVICE_VIDEO_OUTPUT) || \
  // ((category) == AV_CLASS_CATEGORY_DEVICE_AUDIO_OUTPUT) || \
  // ((category) == AV_CLASS_CATEGORY_DEVICE_OUTPUT))

  // struct AVOptionRanges;
  // AVOptionRanges = record
  // end;
  // pAVOptionRanges = ^AVOptionRanges;

  (* *
    * Describe the class of an AVClass context structure. That is an
    * arbitrary struct of which the first field is a pointer to an
    * AVClass struct (e.g. AVCodecContext, AVFormatContext etc.).
  *)
  pAVClass = ^avclass;

  avclass = record
    (* *
      * The name of the class; usually it is the same name as the
      * context structure type to which the AVClass is associated.
    *)
    // const char* class_name;
    class_name: PAnsiChar;

    (* *
      * A pointer to a function which returns the name of a context
      * instance ctx associated with the class.
    *)
    // const char* (*item_name)(void* ctx);
    item_name: function(ctx: Pointer): PAnsiChar; cdecl;

    (* *
      * a pointer to the first option specified in the class if any or NULL
      *
      * @see av_set_default_options()
    *)
    // const struct AVOption *option;
    option: pAVOption;

    (* *
      * LIBAVUTIL_VERSION with which this structure was created.
      * This is used to allow fields to be added without requiring major
      * version bumps everywhere.
    *)

    version: int;

    (* *
      * Offset in the structure where log_level_offset is stored.
      * 0 means there is no such variable
    *)
    log_level_offset_offset: int;

    (* *
      * Offset in the structure where a pointer to the parent context for
      * logging is stored. For example a decoder could pass its AVCodecContext
      * to eval as such a parent context, which an av_log() implementation
      * could then leverage to display the parent context.
      * The offset can be NULL.
    *)
    parent_log_context_offset: int;

    (* *
      * Return next AVOptions-enabled child or NULL
    *)
    // void * (* child_next)(void *obj, void *prev);
    child_next: function(obj: Pointer; prev: Pointer): Pointer; cdecl;

    (* *
      * Return an AVClass corresponding to the next potential
      * AVOptions-enabled child.
      *
      * The difference between child_next and this is that
      * child_next iterates over _already existing_ objects, while
      * child_class_next iterates over _all possible_ children.
    *)
    // const struct AVClass* (*child_class_next)(const struct AVClass *prev);
    child_class_next: function(const prev: pAVClass): pAVClass; cdecl;

    (* *
      * Category used for visualization (like color)
      * This is only set if the category is equal for all objects using this class.
      * available since version (51  shl  16 | 56  shl  8 | 100)
    *)
    category: AVClassCategory;

    (* *
      * Callback to return the category.
      * available since version (51  shl  16 | 59  shl  8 | 100)
    *)
    // AVClassCategory (*get_category)(void* ctx);
    get_category: function(ctx: Pointer): pAVClassCategory; cdecl;
    (* *
      * Callback to return the supported/allowed ranges.
      * available since version (52.12)
    *)
    // int (*query_ranges)(struct AVOptionRanges **, void *obj, const char *key, int flags);
    query_ranges: function(var ranges: pAVOptionRanges; obj: Pointer; const key: PAnsiChar; flags: int): int; cdecl;
  end;

  PVA_LIST = ^VA_LIST;
  VA_LIST = array [0 .. 0] of Pointer;

const
  (* *
    * Print no output.
  *)

  AV_LOG_QUIET = -8;

  (* *
    * Something went really wrong and we will crash now.
  *)
  AV_LOG_PANIC = 0;

  (* *
    * Something went wrong and recovery is not possible.
    * For example, no header was found for a format which depends
    * on headers or an illegal combination of parameters is used.
  *)
  AV_LOG_FATAL = 8;

  (* *
    * Something went wrong and cannot losslessly be recovered.
    * However, not all future data is affected.
  *)
  AV_LOG_ERROR = 16;

  (* *
    * Something somehow does not look correct. This may or may not
    * lead to problems. An example would be the use of '-vstrict -2'.
  *)
  AV_LOG_WARNING = 24;

  (* *
    * Standard information.
  *)
  AV_LOG_INFO = 32;

  (* *
    * Detailed information.
  *)
  AV_LOG_VERBOSE = 40;

  (* *
    * Stuff which is only useful for libav* developers.
  *)
  AV_LOG_DEBUG = 48;

  (* *
    * Extremely verbose debugging, useful for libav* development.
  *)
  AV_LOG_TRACE = 56;

  AV_LOG_MAX_OFFSET = (AV_LOG_TRACE - AV_LOG_QUIET);
{$ENDREGION}
{$REGION 'samplefmt.h'}

type
  pAVSampleFormat = ^AVSampleFormat;
  AVSampleFormat = (         //
    AV_SAMPLE_FMT_NONE = -1, //
    AV_SAMPLE_FMT_U8,
    // < unsigned 8 bits
    AV_SAMPLE_FMT_S16,
    // < signed 16 bits
    AV_SAMPLE_FMT_S32,
    // < signed 32 bits
    AV_SAMPLE_FMT_FLT,
    // < float
    AV_SAMPLE_FMT_DBL,
    // < double

    AV_SAMPLE_FMT_U8P,
    // < unsigned 8 bits, planar
    AV_SAMPLE_FMT_S16P,
    // < signed 16 bits, planar
    AV_SAMPLE_FMT_S32P,
    // < signed 32 bits, planar
    AV_SAMPLE_FMT_FLTP,
    // < float, planar
    AV_SAMPLE_FMT_DBLP,
    // < double, planar
    AV_SAMPLE_FMT_S64,
    // < signed 64 bits
    AV_SAMPLE_FMT_S64P,
    // < signed 64 bits, planar

    AV_SAMPLE_FMT_NB
    // < Number of sample formats. DO NOT USE if linking dynamically
    );

  (* *
    * Return the name of sample_fmt, or NULL if sample_fmt is not
    * recognized.
  *)
  // const char *av_get_sample_fmt_name(enum AVSampleFormat sample_fmt);
function av_get_sample_fmt_name(sample_fmt: AVSampleFormat): PAnsiChar; cdecl; external avutil_dll;

(* *
  * Return a sample format corresponding to name, or AV_SAMPLE_FMT_NONE
  * on error.
*)
// enum AVSampleFormat av_get_sample_fmt(const char *name);
function av_get_sample_fmt(const name: PAnsiChar): AVSampleFormat; cdecl; external avutil_dll;

(* *
  * Return the planar<->packed alternative form of the given sample format, or
  * AV_SAMPLE_FMT_NONE on error. If the passed sample_fmt is already in the
  * requested planar/packed format, the format returned is the same as the
  * input.
*)
// enum AVSampleFormat av_get_alt_sample_fmt(enum AVSampleFormat sample_fmt, int planar);
function av_get_alt_sample_fmt(sample_fmt: AVSampleFormat; planar: int): AVSampleFormat; cdecl; external avutil_dll;

(* *
  * Get the packed alternative form of the given sample format.
  *
  * If the passed sample_fmt is already in packed format, the format returned is
  * the same as the input.
  *
  * @return  the packed alternative form of the given sample format or
  AV_SAMPLE_FMT_NONE on error.
*)
// enum AVSampleFormat av_get_packed_sample_fmt(enum AVSampleFormat sample_fmt);
function av_get_packed_sample_fmt(sample_fmt: AVSampleFormat): AVSampleFormat; cdecl; external avutil_dll;

(* *
  * Get the planar alternative form of the given sample format.
  *
  * If the passed sample_fmt is already in planar format, the format returned is
  * the same as the input.
  *
  * @return  the planar alternative form of the given sample format or
  AV_SAMPLE_FMT_NONE on error.
*)
// enum AVSampleFormat av_get_planar_sample_fmt(enum AVSampleFormat sample_fmt);
function av_get_planar_sample_fmt(sample_fmt: AVSampleFormat): AVSampleFormat; cdecl; external avutil_dll;

(* *
  * Generate a string corresponding to the sample format with
  * sample_fmt, or a header if sample_fmt is negative.
  *
  * @param buf the buffer where to write the string
  * @param buf_size the size of buf
  * @param sample_fmt the number of the sample format to print the
  * corresponding info string, or a negative value to print the
  * corresponding header.
  * @return the pointer to the filled buffer or NULL if sample_fmt is
  * unknown or in case of other errors
*)
// char *av_get_sample_fmt_string(char *buf, int buf_size, enum AVSampleFormat sample_fmt);
function av_get_sample_fmt_string(buf: PAnsiChar; buf_size: int; sample_fmt: AVSampleFormat): PAnsiChar; cdecl; external avutil_dll;

(* *
  * Return number of bytes per sample.
  *
  * @param sample_fmt the sample format
  * @return number of bytes per sample or zero if unknown for the given
  * sample format
*)
// int av_get_bytes_per_sample(enum AVSampleFormat sample_fmt);
function av_get_bytes_per_sample(sample_fmt: AVSampleFormat): int; cdecl; external avutil_dll;

(* *
  * Check if the sample format is planar.
  *
  * @param sample_fmt the sample format to inspect
  * @return 1 if the sample format is planar, 0 if it is interleaved
*)
// int av_sample_fmt_is_planar(enum AVSampleFormat sample_fmt);
function av_sample_fmt_is_planar(sample_fmt: AVSampleFormat): int; cdecl; external avutil_dll;

(* *
  * Get the required buffer size for the given audio parameters.
  *
  * @param[out] linesize calculated linesize, may be NULL
  * @param nb_channels   the number of channels
  * @param nb_samples    the number of samples in a single channel
  * @param sample_fmt    the sample format
  * @param align         buffer size alignment (0 = default, 1 = no alignment)
  * @return              required buffer size, or negative error code on failure
*)
// int av_samples_get_buffer_size(int *linesize, int nb_channels, int nb_samples,
// enum AVSampleFormat sample_fmt, int align);
function av_samples_get_buffer_size(var linesize: int; nb_channels: int; nb_samples: int; sample_fmt: AVSampleFormat; align: int): int; cdecl; overload;
  external avutil_dll;
function av_samples_get_buffer_size(linesize: pint; nb_channels: int; nb_samples: int; sample_fmt: AVSampleFormat; align: int): int; cdecl; overload;
  external avutil_dll;
(* *
  * @}
  *
  * @defgroup lavu_sampmanip Samples manipulation
  *
  * Functions that manipulate audio samples
  * @{
*)

(* *
  * Fill plane data pointers and linesize for samples with sample
  * format sample_fmt.
  *
  * The audio_data array is filled with the pointers to the samples data planes:
  * for planar, set the start point of each channel's data within the buffer,
  * for packed, set the start point of the entire buffer only.
  *
  * The value pointed to by linesize is set to the aligned size of each
  * channel's data buffer for planar layout, or to the aligned size of the
  * buffer for all channels for packed layout.
  *
  * The buffer in buf must be big enough to contain all the samples
  * (use av_samples_get_buffer_size() to compute its minimum size),
  * otherwise the audio_data pointers will point to invalid data.
  *
  * @see enum AVSampleFormat
  * The documentation for AVSampleFormat describes the data layout.
  *
  * @param[out] audio_data  array to be filled with the pointer for each channel
  * @param[out] linesize    calculated linesize, may be NULL
  * @param buf              the pointer to a buffer containing the samples
  * @param nb_channels      the number of channels
  * @param nb_samples       the number of samples in a single channel
  * @param sample_fmt       the sample format
  * @param align            buffer size alignment (0 = default, 1 = no alignment)
  * @return                 >=0 on success or a negative error code on failure
  * @todo return minimum size in bytes required for the buffer in case
  * of success at the next bump
*)
// int av_samples_fill_arrays(uint8_t **audio_data, int *linesize,
// const uint8_t *buf,
// int nb_channels, int nb_samples,
// enum AVSampleFormat sample_fmt, int align);
function av_samples_fill_arrays(var audio_data: puint8_t; var linesize: int; const buf: puint8_t; nb_channels: int; nb_samples: int; sample_fmt: AVSampleFormat;
  align: int): int; cdecl; external avutil_dll;

(* *
  * Allocate a samples buffer for nb_samples samples, and fill data pointers and
  * linesize accordingly.
  * The allocated samples buffer can be freed by using av_freep(&audio_data[0])
  * Allocated data will be initialized to silence.
  *
  * @see enum AVSampleFormat
  * The documentation for AVSampleFormat describes the data layout.
  *
  * @param[out] audio_data  array to be filled with the pointer for each channel
  * @param[out] linesize    aligned size for audio buffer(s), may be NULL
  * @param nb_channels      number of audio channels
  * @param nb_samples       number of samples per channel
  * @param align            buffer size alignment (0 = default, 1 = no alignment)
  * @return                 >=0 on success or a negative error code on failure
  * @todo return the size of the allocated buffer in case of success at the next bump
  * @see av_samples_fill_arrays()
  * @see av_samples_alloc_array_and_samples()
*)
// int av_samples_alloc(uint8_t **audio_data, int *linesize, int nb_channels,
// int nb_samples, enum AVSampleFormat sample_fmt, int align);
function av_samples_alloc(var audio_data: puint8_t; linesize: pint; nb_channels: int; nb_samples: int; sample_fmt: AVSampleFormat; align: int): int; cdecl;
  external avutil_dll;

(* *
  * Allocate a data pointers array, samples buffer for nb_samples
  * samples, and fill data pointers and linesize accordingly.
  *
  * This is the same as av_samples_alloc(), but also allocates the data
  * pointers array.
  *
  * @see av_samples_alloc()
*)
// int av_samples_alloc_array_and_samples(uint8_t ***audio_data, int *linesize, int nb_channels,
// int nb_samples, enum AVSampleFormat sample_fmt, int align);
function av_samples_alloc_array_and_samples(Var audio_data: ppuint8_t; var linesize: int; nb_channels: int; nb_samples: int; sample_fmt: AVSampleFormat;
  align: int): int; cdecl; external avutil_dll;

(* *
  * Copy samples from src to dst.
  *
  * @param dst destination array of pointers to data planes
  * @param src source array of pointers to data planes
  * @param dst_offset offset in samples at which the data will be written to dst
  * @param src_offset offset in samples at which the data will be read from src
  * @param nb_samples number of samples to be copied
  * @param nb_channels number of audio channels
  * @param sample_fmt audio sample format
*)
// int av_samples_copy(uint8_t **dst, uint8_t * const *src, int dst_offset,
// int src_offset, int nb_samples, int nb_channels,
// enum AVSampleFormat sample_fmt);
function av_samples_copy(var dst: puint8_t; const src: ppuint8_t; dst_offset: int; src_offset: int; nb_samples: int; nb_channels: int;
  sample_fmt: AVSampleFormat): int; cdecl; external avutil_dll;

(* *
  * Fill an audio buffer with silence.
  *
  * @param audio_data  array of pointers to data planes
  * @param offset      offset in samples at which to start filling
  * @param nb_samples  number of samples to fill
  * @param nb_channels number of audio channels
  * @param sample_fmt  audio sample format
*)
// int av_samples_set_silence(uint8_t **audio_data, int offset, int nb_samples,
// int nb_channels, enum AVSampleFormat sample_fmt);
function av_samples_set_silence(var audio_data: puint8_t; offset: int; nb_samples: int; nb_channels: int; sample_fmt: AVSampleFormat): int; cdecl;
  external avutil_dll;

{$ENDREGION}
{$REGION 'opt.h'}
(* *
  * Show the obj options.
  *
  * @param req_flags requested flags for the options to show. Show only the
  * options for which it is opt->flags & req_flags.
  * @param rej_flags rejected flags for the options to show. Show only the
  * options for which it is !(opt->flags & req_flags).
  * @param av_log_obj log context to use for showing the options
*)
// int av_opt_show2(void *obj, void *av_log_obj, int req_flags, int rej_flags);
function av_opt_show2(obj, av_log_obj: Pointer; req_flags, rej_flags: int): int; cdecl; external avutil_dll;

(* *
  * Set the values of all AVOption fields to their default values.
  *
  * @param s an AVOption-enabled struct (its first member must be a pointer to AVClass)
*)
// void av_opt_set_defaults(void *s);
procedure av_opt_set_defaults(s: Pointer); cdecl; external avutil_dll;

(* *
  * Set the values of all AVOption fields to their default values. Only these
  * AVOption fields for which (opt->flags & mask) == flags will have their
  * default applied to s.
  *
  * @param s an AVOption-enabled struct (its first member must be a pointer to AVClass)
  * @param mask combination of AV_OPT_FLAG_*
  * @param flags combination of AV_OPT_FLAG_*
*)
// void av_opt_set_defaults2(void *s, int mask, int flags);
procedure av_opt_set_defaults2(s: Pointer; mask, flags: int); cdecl; external avutil_dll;

(* *
  * Parse the key/value pairs list in opts. For each key/value pair
  * found, stores the value in the field in ctx that is named like the
  * key. ctx must be an AVClass context, storing is done using
  * AVOptions.
  *
  * @param opts options string to parse, may be NULL
  * @param key_val_sep a 0-terminated list of characters used to
  * separate key from value
  * @param pairs_sep a 0-terminated list of characters used to separate
  * two pairs from each other
  * @return the number of successfully set key/value pairs, or a negative
  * value corresponding to an AVERROR code in case of error:
  * AVERROR(EINVAL) if opts cannot be parsed,
  * the error code issued by av_opt_set() if a key/value pair
  * cannot be set
*)
// int av_set_options_string(void *ctx, const char *opts,
// const char *key_val_sep, const char *pairs_sep);
function av_set_options_string(ctx: Pointer; const opts: PAnsiChar; const key_val_sep: PAnsiChar; const pairs_sep: PAnsiChar): int; cdecl; external avutil_dll;

(* *
  * Parse the key-value pairs list in opts. For each key=value pair found,
  * set the value of the corresponding option in ctx.
  *
  * @param ctx          the AVClass object to set options on
  * @param opts         the options string, key-value pairs separated by a
  *                     delimiter
  * @param shorthand    a NULL-terminated array of options names for shorthand
  *                     notation: if the first field in opts has no key part,
  *                     the key is taken from the first element of shorthand;
  *                     then again for the second, etc., until either opts is
  *                     finished, shorthand is finished or a named option is
  *                     found; after that, all options must be named
  * @param key_val_sep  a 0-terminated list of characters used to separate
  *                     key from value, for example '='
  * @param pairs_sep    a 0-terminated list of characters used to separate
  *                     two pairs from each other, for example ':' or ','
  * @return  the number of successfully set key=value pairs, or a negative
  *          value corresponding to an AVERROR code in case of error:
  *          AVERROR(EINVAL) if opts cannot be parsed,
  *          the error code issued by av_set_string3() if a key/value pair
  *          cannot be set
  *
  * Options names must use only the following characters: a-z A-Z 0-9 - . / _
  * Separators must use characters distinct from option names and from each
  * other.
*)
// int av_opt_set_from_string(void *ctx, const char *opts,
// const char *const *shorthand,
// const char *key_val_sep, const char *pairs_sep);
function av_opt_set_from_string(ctx: Pointer; const opts: PAnsiChar; const shorthand: ppAnsiChar; const key_val_sep: PAnsiChar; const pairs_sep: PAnsiChar)
  : int; cdecl; external avutil_dll;
(* *
  * Free all allocated objects in obj.
*)
// void av_opt_free(void *obj);
procedure av_opt_free(obj: Pointer); cdecl; external avutil_dll;

(* *
  * Check whether a particular flag is set in a flags field.
  *
  * @param field_name the name of the flag field option
  * @param flag_name the name of the flag to check
  * @return non-zero if the flag is set, zero if the flag isn't set,
  *         isn't of the right type, or the flags field doesn't exist.
*)
// int av_opt_flag_is_set(void *obj, const char *field_name, const char *flag_name);
function av_opt_flag_is_set(obj: Pointer; const field_name: PAnsiChar; const flag_name: PAnsiChar): int; cdecl; external avutil_dll;

(* *
  * Set all the options from a given dictionary on an object.
  *
  * @param obj a struct whose first element is a pointer to AVClass
  * @param options options to process. This dictionary will be freed and replaced
  *                by a new one containing all options not found in obj.
  *                Of course this new dictionary needs to be freed by caller
  *                with av_dict_free().
  *
  * @return 0 on success, a negative AVERROR if some option was found in obj,
  *         but could not be set.
  *
  * @see av_dict_copy()
*)
// int av_opt_set_dict(void *obj, struct AVDictionary **options);
function av_opt_set_dict(obj: Pointer; var options: pAVDictionary): int; cdecl; external avutil_dll;

(* *
  * Set all the options from a given dictionary on an object.
  *
  * @param obj a struct whose first element is a pointer to AVClass
  * @param options options to process. This dictionary will be freed and replaced
  *                by a new one containing all options not found in obj.
  *                Of course this new dictionary needs to be freed by caller
  *                with av_dict_free().
  * @param search_flags A combination of AV_OPT_SEARCH_*.
  *
  * @return 0 on success, a negative AVERROR if some option was found in obj,
  *         but could not be set.
  *
  * @see av_dict_copy()
*)
// int av_opt_set_dict2(void *obj, struct AVDictionary **options, int search_flags);
function av_opt_set_dict2(obj: Pointer; Var options: pAVDictionary; search_flags: int): int; cdecl; external avutil_dll;

(* *
  * Extract a key-value pair from the beginning of a string.
  *
  * @param ropts        pointer to the options string, will be updated to
  *                     point to the rest of the string (one of the pairs_sep
  *                     or the final NUL)
  * @param key_val_sep  a 0-terminated list of characters used to separate
  *                     key from value, for example '='
  * @param pairs_sep    a 0-terminated list of characters used to separate
  *                     two pairs from each other, for example ':' or ','
  * @param flags        flags; see the AV_OPT_FLAG_* values below
  * @param rkey         parsed key; must be freed using av_free()
  * @param rval         parsed value; must be freed using av_free()
  *
  * @return  >=0 for success, or a negative value corresponding to an
  *          AVERROR code in case of error; in particular:
  *          AVERROR(EINVAL) if no key is present
  *
*)
// int av_opt_get_key_value(const char **ropts,
// const char *key_val_sep, const char *pairs_sep,
// unsigned flags,
// char **rkey, char **rval);
function av_opt_get_key_value(const ropts: ppAnsiChar; const key_val_sep: PAnsiChar; const pairs_sep: PAnsiChar; flags: unsigned; rkey: ppAnsiChar;
  rval: ppAnsiChar): int; cdecl; external avutil_dll;

(* *
  * Accept to parse a value without a key; the key will then be returned
  * as NULL.
*)
const
  AV_OPT_FLAG_IMPLICIT_KEY = 1;

  (* *
    * @defgroup opt_eval_funcs Evaluating option strings
    * @{
    * This group of functions can be used to evaluate option strings
    * and get numbers out of them. They do the same thing as av_opt_set(),
    * except the result is written into the caller-supplied pointer.
    *
    * @param obj a struct whose first element is a pointer to AVClass.
    * @param o an option for which the string is to be evaluated.
    * @param val string to be evaluated.
    * @param *_out value of the string will be written here.
    *
    * @return 0 on success, a negative number on failure.
  *)
  // int av_opt_eval_flags (void *obj, const AVOption *o, const char *val, int        *flags_out);
function av_opt_eval_flags(obj: Pointer; const o: pAVOption; const val: PAnsiChar; var flags_out: int): int; cdecl; external avutil_dll;

// int av_opt_eval_int   (void *obj, const AVOption *o, const char *val, int        *int_out);
// int av_opt_eval_int64 (void *obj, const AVOption *o, const char *val, int64_t    *int64_out);
// int av_opt_eval_float (void *obj, const AVOption *o, const char *val, float      *float_out);
// int av_opt_eval_double(void *obj, const AVOption *o, const char *val, double     *double_out);
// int av_opt_eval_q     (void *obj, const AVOption *o, const char *val, AVRational *q_out);
(* *
  * @}
*)
const
  AV_OPT_SEARCH_CHILDREN = (1 shl 0); (* *< Search in possible children of the
    given object first. *)
  (* *
    *  The obj passed to av_opt_find() is fake -- only a double pointer to AVClass
    *  instead of a required pointer to a struct containing AVClass. This is
    *  useful for searching for options without needing to allocate the corresponding
    *  object.
  *)
  AV_OPT_SEARCH_FAKE_OBJ = (1 shl 1);

  (* *
    *  In av_opt_get, return NULL if the option has a pointer type and is set to NULL,
    *  rather than returning an empty string.
  *)
  AV_OPT_ALLOW_NULL = (1 shl 2);

  (* *
    *  Allows av_opt_query_ranges and av_opt_query_ranges_default to return more than
    *  one component for certain option types.
    *  @see AVOptionRanges for details.
  *)
  AV_OPT_MULTI_COMPONENT_RANGE = (1 shl 12);

  (* *
    * Look for an option in an object. Consider only options which
    * have all the specified flags set.
    *
    * @param[in] obj A pointer to a struct whose first element is a
    *                pointer to an AVClass.
    *                Alternatively a double pointer to an AVClass, if
    *                AV_OPT_SEARCH_FAKE_OBJ search flag is set.
    * @param[in] name The name of the option to look for.
    * @param[in] unit When searching for named constants, name of the unit
    *                 it belongs to.
    * @param opt_flags Find only options with all the specified flags set (AV_OPT_FLAG).
    * @param search_flags A combination of AV_OPT_SEARCH_*.
    *
    * @return A pointer to the option found, or NULL if no option
    *         was found.
    *
    * @note Options found with AV_OPT_SEARCH_CHILDREN flag may not be settable
    * directly with av_opt_set(). Use special calls which take an options
    * AVDictionary (e.g. avformat_open_input()) to set options found with this
    * flag.
  *)
  // const AVOption *av_opt_find(void *obj, const char *name, const char *unit,
  // int opt_flags, int search_flags);
function av_opt_find(obj: Pointer; const name: PAnsiChar; const _unit: PAnsiChar; opt_flags: int; search_flags: int): pAVOption; cdecl; external avutil_dll;

(* *
  * Look for an option in an object. Consider only options which
  * have all the specified flags set.
  *
  * @param[in] obj A pointer to a struct whose first element is a
  *                pointer to an AVClass.
  *                Alternatively a double pointer to an AVClass, if
  *                AV_OPT_SEARCH_FAKE_OBJ search flag is set.
  * @param[in] name The name of the option to look for.
  * @param[in] unit When searching for named constants, name of the unit
  *                 it belongs to.
  * @param opt_flags Find only options with all the specified flags set (AV_OPT_FLAG).
  * @param search_flags A combination of AV_OPT_SEARCH_*.
  * @param[out] target_obj if non-NULL, an object to which the option belongs will be
  * written here. It may be different from obj if AV_OPT_SEARCH_CHILDREN is present
  * in search_flags. This parameter is ignored if search_flags contain
  * AV_OPT_SEARCH_FAKE_OBJ.
  *
  * @return A pointer to the option found, or NULL if no option
  *         was found.
*)
// const AVOption *av_opt_find2(void *obj, const char *name, const char *unit,
// int opt_flags, int search_flags, void **target_obj);
function av_opt_find2(obj: Pointer; const name: PAnsiChar; const _unit: PAnsiChar; opt_flags: int; search_flags: int; Var target_obj: Pointer): pAVOption;
  cdecl; external avutil_dll;

(* *
  * Iterate over all AVOptions belonging to obj.
  *
  * @param obj an AVOptions-enabled struct or a double pointer to an
  *            AVClass describing it.
  * @param prev result of the previous call to av_opt_next() on this object
  *             or NULL
  * @return next AVOption or NULL
*)
// const AVOption *av_opt_next(const void *obj, const AVOption *prev);
function av_opt_next(const obj: Pointer; const prev: pAVOption): pAVOption; cdecl; external avutil_dll;

(* *
  * Iterate over AVOptions-enabled children of obj.
  *
  * @param prev result of a previous call to this function or NULL
  * @return next AVOptions-enabled child or NULL
*)
// void *av_opt_child_next(void *obj, void *prev);
function av_opt_child_next(obj: Pointer; prev: Pointer): Pointer; cdecl; external avutil_dll;

(* *
  * Iterate over potential AVOptions-enabled children of parent.
  *
  * @param prev result of a previous call to this function or NULL
  * @return AVClass corresponding to next potential child or NULL
*)
// const AVClass *av_opt_child_class_next(const AVClass *parent, const AVClass *prev);
function av_opt_child_class_next(const parent: pAVClass; const prev: pAVClass): pAVClass; cdecl; external avutil_dll;

(* *
  * @defgroup opt_set_funcs Option setting functions
  * @{
  * Those functions set the field of obj with the given name to value.
  *
  * @param[in] obj A struct whose first element is a pointer to an AVClass.
  * @param[in] name the name of the field to set
  * @param[in] val The value to set. In case of av_opt_set() if the field is not
  * of a string type, then the given string is parsed.
  * SI postfixes and some named scalars are supported.
  * If the field is of a numeric type, it has to be a numeric or named
  * scalar. Behavior with more than one scalar and +- infix operators
  * is undefined.
  * If the field is of a flags type, it has to be a sequence of numeric
  * scalars or named flags separated by '+' or '-'. Prefixing a flag
  * with '+' causes it to be set without affecting the other flags;
  * similarly, '-' unsets a flag.
  * @param search_flags flags passed to av_opt_find2. I.e. if AV_OPT_SEARCH_CHILDREN
  * is passed here, then the option may be set on a child of obj.
  *
  * @return 0 if the value has been set, or an AVERROR code in case of
  * error:
  * AVERROR_OPTION_NOT_FOUND if no matching option exists
  * AVERROR(ERANGE) if the value is out of range
  * AVERROR(EINVAL) if the value is not valid
*)

// int av_opt_set         (void *obj, const char *name, const char *val, int search_flags);
function av_opt_set(obj: Pointer; const name: PAnsiChar; const val: PAnsiChar; search_flags: int): int; cdecl; external avutil_dll;
// int av_opt_set_int     (void *obj, const char *name, int64_t     val, int search_flags);
function av_opt_set_int(obj: Pointer; const name: PAnsiChar; val: int64_t; search_flags: int): int; cdecl; external avutil_dll;
// int av_opt_set_double  (void *obj, const char *name, double      val, int search_flags);
function av_opt_set_double(obj: Pointer; const name: PAnsiChar; val: double; search_flags: int): int; cdecl; external avutil_dll;
// int av_opt_set_q       (void *obj, const char *name, AVRational  val, int search_flags);
function av_opt_set_q(obj: Pointer; const name: PAnsiChar; val: AVRational; search_flags: int): int; cdecl; external avutil_dll;
// int av_opt_set_bin     (void *obj, const char *name, const uint8_t *val, int size, int search_flags);
function av_opt_set_bin(obj: Pointer; const name: PAnsiChar; const val: puint8_t; size: int; search_flags: int): int; cdecl; external avutil_dll;
// int av_opt_set_image_size(void *obj, const char *name, int w, int h, int search_flags);
function av_opt_set_image_size(obj: Pointer; const name: PAnsiChar; w, h, search_flags: int): int; cdecl; external avutil_dll;
// int av_opt_set_pixel_fmt (void *obj, const char *name, enum AVPixelFormat fmt, int search_flags);
function av_opt_set_pixel_fmt(obj: Pointer; const name: PAnsiChar; fmt: AVPixelFormat; search_flags: int): int; cdecl; external avutil_dll;
// int av_opt_set_sample_fmt(void *obj, const char *name, enum AVSampleFormat fmt, int search_flags);
function av_opt_set_sample_fmt(obj: Pointer; const name: PAnsiChar; fmt: AVSampleFormat; search_flags: int): int; cdecl; external avutil_dll;
// int av_opt_set_video_rate(void *obj, const char *name, AVRational val, int search_flags);
function av_opt_set_video_rate(obj: Pointer; const name: PAnsiChar; val: AVRational; search_flags: int): int; cdecl; external avutil_dll;
// int av_opt_set_channel_layout(void *obj, const char *name, int64_t ch_layout, int search_flags);
function av_opt_set_channel_layout(obj: Pointer; const name: PAnsiChar; ch_layout: int64_t; search_flags: int): int; cdecl; external avutil_dll;
(* *
  * @note Any old dictionary present is discarded and replaced with a copy of the new one. The
  * caller still owns val is and responsible for freeing it.
*)
// int av_opt_set_dict_val(void *obj, const char *name, const AVDictionary *val, int search_flags);
function av_opt_set_dict_val(obj: Pointer; const name: PAnsiChar; const val: pAVDictionary; search_flags: int): int; cdecl; external avutil_dll;

(* *
  * Set a binary option to an integer list.
  *
  * @param obj    AVClass object to set options on
  * @param name   name of the binary option
  * @param val    pointer to an integer list (must have the correct type with
  *               regard to the contents of the list)
  * @param term   list terminator (usually 0 or -1)
  * @param flags  search flags
*)
// #define av_opt_set_int_list(obj, name, val, term, flags) \
// (av_int_list_length(val, term) > INT_MAX / sizeof(*(val)) ? \
// AVERROR(EINVAL) : \
// av_opt_set_bin(obj, name, (const uint8_t *)(val), \
// av_int_list_length(val, term) * sizeof(*(val)), flags))
function av_opt_set_int_list(obj: Pointer; name: PAnsiChar; list: Pointer; item_size: int; term: int64_t; flags: int): Integer; inline;

(* *
  * @}
*)

(* *
  * @defgroup opt_get_funcs Option getting functions
  * @{
  * Those functions get a value of the option with the given name from an object.
  *
  * @param[in] obj a struct whose first element is a pointer to an AVClass.
  * @param[in] name name of the option to get.
  * @param[in] search_flags flags passed to av_opt_find2. I.e. if AV_OPT_SEARCH_CHILDREN
  * is passed here, then the option may be found in a child of obj.
  * @param[out] out_val value of the option will be written here
  * @return >=0 on success, a negative error code otherwise
*)
(* *
  * @note the returned string will be av_malloc()ed and must be av_free()ed by the caller
  *
  * @note if AV_OPT_ALLOW_NULL is set in search_flags in av_opt_get, and the option has
  * AV_OPT_TYPE_STRING or AV_OPT_TYPE_BINARY and is set to NULL, *out_val will be set
  * to NULL instead of an allocated empty string.
*)
// int av_opt_get         (void *obj, const char *name, int search_flags, uint8_t   **out_val);
function av_opt_get(obj: Pointer; const name: PAnsiChar; search_flags: int; Var out_val: puint8_t): int; cdecl; external avutil_dll;
// int av_opt_get_int     (void *obj, const char *name, int search_flags, int64_t    *out_val);
function av_opt_get_int(obj: Pointer; const name: PAnsiChar; search_flags: int; var out_val: int64_t): int; cdecl; external avutil_dll;
// int av_opt_get_double  (void *obj, const char *name, int search_flags, double     *out_val);
function av_opt_get_double(obj: Pointer; const name: PAnsiChar; search_flags: int; out_val: double): int; cdecl; external avutil_dll;
// int av_opt_get_q       (void *obj, const char *name, int search_flags, AVRational *out_val);
function av_opt_get_q(obj: Pointer; const name: PAnsiChar; search_flags: int; var out_val: AVRational): int; cdecl; external avutil_dll;
// int av_opt_get_image_size(void *obj, const char *name, int search_flags, int *w_out, int *h_out);
function av_opt_get_image_size(obj: Pointer; const name: PAnsiChar; search_flags: int; var w_out, h_out: int): int; cdecl; external avutil_dll;
// int av_opt_get_pixel_fmt (void *obj, const char *name, int search_flags, enum AVPixelFormat *out_fmt);
function av_opt_get_pixel_fmt(obj: Pointer; const name: PAnsiChar; search_flags: int; var out_fmt: AVPixelFormat): int; cdecl; external avutil_dll;
// int av_opt_get_sample_fmt(void *obj, const char *name, int search_flags, enum AVSampleFormat *out_fmt);
function av_opt_get_sample_fmt(obj: Pointer; const name: PAnsiChar; search_flags: int; var out_fmt: AVSampleFormat): int; cdecl; external avutil_dll;
// int av_opt_get_video_rate(void *obj, const char *name, int search_flags, AVRational *out_val);
function av_opt_get_video_rate(obj: Pointer; const name: PAnsiChar; search_flags: int; var out_val: AVRational): int; cdecl; external avutil_dll;
// int av_opt_get_channel_layout(void *obj, const char *name, int search_flags, int64_t *ch_layout);
function av_opt_get_channel_layout(obj: Pointer; const name: PAnsiChar; search_flags: int; var ch_layout: int64_t): int; cdecl; external avutil_dll;
(* *
  * @param[out] out_val The returned dictionary is a copy of the actual value and must
  * be freed with av_dict_free() by the caller
*)
// int av_opt_get_dict_val(void *obj, const char *name, int search_flags, AVDictionary **out_val);
function av_opt_get_dict_val(obj: Pointer; const name: PAnsiChar; search_flags: int; var out_val: pAVDictionary): int; cdecl; external avutil_dll;
(* *
  * @}
*)
(* *
  * Gets a pointer to the requested field in a struct.
  * This function allows accessing a struct even when its fields are moved or
  * renamed since the application making the access has been compiled,
  *
  * @returns a pointer to the field, it can be cast to the correct type and read
  *          or written to.
*)
// void *av_opt_ptr(const AVClass *avclass, void *obj, const char *name);
function av_opt_ptr(const avclass: pAVClass; obj: Pointer; const name: PAnsiChar): Pointer; cdecl; external avutil_dll;

(* *
  * Free an AVOptionRanges struct and set it to NULL.
*)
// void av_opt_freep_ranges(AVOptionRanges **ranges);
procedure av_opt_freep_ranges(var ranges: pAVOptionRanges); cdecl; external avutil_dll;

(* *
  * Get a list of allowed ranges for the given option.
  *
  * The returned list may depend on other fields in obj like for example profile.
  *
  * @param flags is a bitmask of flags, undefined flags should not be set and should be ignored
  *              AV_OPT_SEARCH_FAKE_OBJ indicates that the obj is a double pointer to a AVClass instead of a full instance
  *              AV_OPT_MULTI_COMPONENT_RANGE indicates that function may return more than one component, @see AVOptionRanges
  *
  * The result must be freed with av_opt_freep_ranges.
  *
  * @return number of compontents returned on success, a negative errro code otherwise
*)
// int av_opt_query_ranges(AVOptionRanges **, void *obj, const char *key, int flags);
function av_opt_query_ranges(Var ranges: pAVOptionRanges; obj: Pointer; const key: PAnsiChar; flags: int): int; cdecl; external avutil_dll;

(* *
  * Copy options from src object into dest object.
  *
  * Options that require memory allocation (e.g. string or binary) are malloc'ed in dest object.
  * Original memory allocated for such options is freed unless both src and dest options points to the same memory.
  *
  * @param dest Object to copy from
  * @param src  Object to copy into
  * @return 0 on success, negative on error
*)
// int av_opt_copy(void *dest, const void *src);
function av_opt_copy(dest: Pointer; const src: Pointer): int; cdecl; external avutil_dll;

(* *
  * Get a default list of allowed ranges for the given option.
  *
  * This list is constructed without using the AVClass.query_ranges() callback
  * and can be used as fallback from within the callback.
  *
  * @param flags is a bitmask of flags, undefined flags should not be set and should be ignored
  *              AV_OPT_SEARCH_FAKE_OBJ indicates that the obj is a double pointer to a AVClass instead of a full instance
  *              AV_OPT_MULTI_COMPONENT_RANGE indicates that function may return more than one component, @see AVOptionRanges
  *
  * The result must be freed with av_opt_free_ranges.
  *
  * @return number of compontents returned on success, a negative errro code otherwise
*)
// int av_opt_query_ranges_default(AVOptionRanges **, void *obj, const char *key, int flags);
function av_opt_query_ranges_default(var ranges: pAVOptionRanges; obj: Pointer; const key: PAnsiChar; flags: int): int; cdecl; external avutil_dll;

(* *
  * Check if given option is set to its default value.
  *
  * Options o must belong to the obj. This function must not be called to check child's options state.
  * @see av_opt_is_set_to_default_by_name().
  *
  * @param obj  AVClass object to check option on
  * @param o    option to be checked
  * @return     >0 when option is set to its default,
  *              0 when option is not set its default,
  *             <0 on error
*)
// int av_opt_is_set_to_default(void *obj, const AVOption *o);
function av_opt_is_set_to_default(obj: Pointer; const o: pAVOption): int; cdecl; external avutil_dll;

(* *
  * Check if given option is set to its default value.
  *
  * @param obj          AVClass object to check option on
  * @param name         option name
  * @param search_flags combination of AV_OPT_SEARCH_*
  * @return             >0 when option is set to its default,
  *                     0 when option is not set its default,
  *                     <0 on error
*)
// int av_opt_is_set_to_default_by_name(void *obj, const char *name, int search_flags);
function av_opt_is_set_to_default_by_name(obj: Pointer; const name: PAnsiChar; search_flags: int): int; cdecl; external avutil_dll;

const
  AV_OPT_SERIALIZE_SKIP_DEFAULTS = $00000001;
  // < Serialize options that are not set to default values only.
  AV_OPT_SERIALIZE_OPT_FLAGS_EXACT = $00000002;
  // < Serialize options that exactly match opt_flags only.

  (* *
    * Serialize object's options.
    *
    * Create a string containing object's serialized options.
    * Such string may be passed back to av_opt_set_from_string() in order to restore option values.
    * A key/value or pairs separator occurring in the serialized value or
    * name string are escaped through the av_escape() function.
    *
    * @param[in]  obj           AVClass object to serialize
    * @param[in]  opt_flags     serialize options with all the specified flags set (AV_OPT_FLAG)
    * @param[in]  flags         combination of AV_OPT_SERIALIZE_* flags
    * @param[out] buffer        Pointer to buffer that will be allocated with string containg serialized options.
    *                           Buffer must be freed by the caller when is no longer needed.
    * @param[in]  key_val_sep   character used to separate key from value
    * @param[in]  pairs_sep     character used to separate two pairs from each other
    * @return                   >= 0 on success, negative on error
    * @warning Separators cannot be neither '\\' nor '\0'. They also cannot be the same.
  *)
  // int av_opt_serialize(void *obj, int opt_flags, int flags, char **buffer,
  // const char key_val_sep, const char pairs_sep);

function av_opt_serialize(obj: Pointer; opt_flags: int; flags: int; Var buffer: PAnsiChar; const key_val_sep: AnsiChar; const pairs_sep: AnsiChar): int; cdecl;
  external avutil_dll;

{$ENDREGION}
{$REGION 'log.h'}
(* *
  * Sets additional colors for extended debugging sessions.
  * @code
  av_log(ctx, AV_LOG_DEBUG|AV_LOG_C(134), "Message in purple\n");
  @endcode
  * Requires 256color terminal support. Uses outside debugging is not
  * recommended.
*)
// #define AV_LOG_C(x) ((x)  shl  8)

(* *
  * Send the specified message to the log if the level is less than or equal
  * to the current av_log_level. By default, all logging messages are sent to
  * stderr. This behavior can be altered by setting a different logging callback
  * function.
  * @see av_log_set_callback
  *
  * @param avcl A pointer to an arbitrary struct of which the first field is a
  *        pointer to an AVClass struct or NULL if general log.
  * @param level The importance level of the message expressed using a @ref
  *        lavu_log_constants "Logging Constant".
  * @param fmt The format string (printf-compatible) that specifies how
  *        subsequent arguments are converted to output.
*)
// void av_log(void *avcl, int level, const char *fmt, ...) av_printf_format(3, 4);
procedure av_log(avcl: Pointer; level: int; const fmt: PAnsiChar);
cdecl varargs;
external avutil_dll;

(* *
  * Send the specified message to the log if the level is less than or equal
  * to the current av_log_level. By default, all logging messages are sent to
  * stderr. This behavior can be altered by setting a different logging callback
  * function.
  * @see av_log_set_callback
  *
  * @param avcl A pointer to an arbitrary struct of which the first field is a
  *        pointer to an AVClass struct.
  * @param level The importance level of the message expressed using a @ref
  *        lavu_log_constants "Logging Constant".
  * @param fmt The format string (printf-compatible) that specifies how
  *        subsequent arguments are converted to output.
  * @param vl The arguments referenced by the format string.
*)
// void av_vlog(void *avcl, int level, const char *fmt, va_list vl);
procedure av_vlog(avcl: Pointer; level: int; const fmt: PAnsiChar; vl: PVA_LIST); cdecl; external avutil_dll;

(* *
  * Get the current log level
  *
  * @see lavu_log_constants
  *
  * @return Current log level
*)
// int av_log_get_level(void);
function av_log_get_level(): int; cdecl; external avutil_dll;

(* *
  * Set the log level
  *
  * @see lavu_log_constants
  *
  * @param level Logging level
*)
// void av_log_set_level(int level);
procedure av_log_set_level(level: int); cdecl; external avutil_dll;

(* *
  * Set the logging callback
  *
  * @note The callback must be thread safe, even if the application does not use
  *       threads itself as some codecs are multithreaded.
  *
  * @see av_log_default_callback
  *
  * @param callback A logging function with a compatible signature.
*)
// void av_log_set_callback(void (*callback)(void*, int, const char*, va_list));
Type
  Tav_log_callback = procedure(p: Pointer; lvl: Integer; fmt: PAnsiChar; vl: PVA_LIST);
cdecl varargs;

procedure av_log_set_callback(callbackproc: Tav_log_callback); cdecl; external avutil_dll;

(* *
  * Default logging callback
  *
  * It prints the message to stderr, optionally colorizing it.
  *
  * @param avcl A pointer to an arbitrary struct of which the first field is a
  *        pointer to an AVClass struct.
  * @param level The importance level of the message expressed using a @ref
  *        lavu_log_constants "Logging Constant".
  * @param fmt The format string (printf-compatible) that specifies how
  *        subsequent arguments are converted to output.
  * @param vl The arguments referenced by the format string.
*)
// void av_log_default_callback(void *avcl, int level, const char *fmt, va_list vl);
procedure av_log_default_callback(avcl: Pointer; level: int; const fmt: PAnsiChar; vl: PVA_LIST); cdecl; external avutil_dll;

(* *
  * Return the context name
  *
  * @param  ctx The AVClass context
  *
  * @return The AVClass class_name
*)
// const char* av_default_item_name(void* ctx);
function av_default_item_name(ctx: Pointer): PAnsiChar; cdecl; external avutil_dll;

// AVClassCategory av_default_get_category(void *ptr);
function av_default_get_category(ptr: Pointer): AVClassCategory; cdecl; external avutil_dll;

(* *
  * Format a line of log the same way as the default callback.
  * @param line          buffer to receive the formatted line
  * @param line_size     size of the buffer
  * @param print_prefix  used to store whether the prefix must be printed;
  *                      must point to a persistent integer initially set to 1
*)
// void av_log_format_line(void *ptr, int level, const char *fmt, va_list vl,
// char *line, int line_size, int *print_prefix);
procedure av_log_format_line(ptr: Pointer; level: int; const fmt: PAnsiChar; vl: PVA_LIST; line: PAnsiChar; line_size: int; Var print_prefix: int); cdecl;
  external avutil_dll;

(* *
  * Format a line of log the same way as the default callback.
  * @param line          buffer to receive the formatted line;
  *                      may be NULL if line_size is 0
  * @param line_size     size of the buffer; at most line_size-1 characters will
  *                      be written to the buffer, plus one null terminator
  * @param print_prefix  used to store whether the prefix must be printed;
  *                      must point to a persistent integer initially set to 1
  * @return Returns a negative value if an error occurred, otherwise returns
  *         the number of characters that would have been written for a
  *         sufficiently large buffer, not including the terminating null
  *         character. If the return value is not less than line_size, it means
  *         that the log message was truncated to fit the buffer.
*)
// int av_log_format_line2(void *ptr, int level, const char *fmt, va_list vl,
// char *line, int line_size, int *print_prefix);
function av_log_format_line2(ptr: Pointer; level: int; const fmt: PAnsiChar; vl: PVA_LIST; line: PAnsiChar; line_size: int; Var print_prefix: int): int; cdecl;
  external avutil_dll;

const
  (* *
    * Skip repeated messages, this requires the user app to use av_log() instead of
    * (f)printf as the 2 would otherwise interfere and lead to
    * "Last message repeated x times" messages below (f)printf messages with some
    * bad luck.
    * Also to receive the last, "last repeated" line if any, the user app must
    * call av_log(NULL, AV_LOG_QUIET, "%s", ""); at the end
  *)
  AV_LOG_SKIP_REPEATED = 1;

  (* *
    * Include the log severity in messages originating from codecs.
    *
    * Results in messages such as:
    * [rawvideo @ $DEADBEEF] [error] encode did not produce valid pts
  *)
  AV_LOG_PRINT_LEVEL = 2;

  // void av_log_set_flags(int arg);
procedure av_log_set_flags(arg: int); cdecl; external avutil_dll;

// int av_log_get_flags(void);
function av_log_get_flags(): int; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'avutil.h'}
(* *
  * Return the LIBAVUTIL_VERSION_INT constant.
*)
// unsigned avutil_version(void);
function avutil_version(): unsigned; cdecl; external avutil_dll;

(* *
  * Return an informative version string. This usually is the actual release
  * version number or a git commit description. This string has no fixed format
  * and can change any time. It should never be parsed by code.
*)
// const char *av_version_info(void);
function av_version_info(): PAnsiChar; cdecl; external avutil_dll;

(* *
  * Return the libavutil build-time configuration.
*)
// const char *avutil_configuration(void);
function avutil_configuration(): PAnsiChar; cdecl; external avutil_dll;

(* *
  * Return the libavutil license.
*)
// const char *avutil_license(void);
function avutil_license(): PAnsiChar; cdecl; external avutil_dll;

(* *
  * Return a string describing the media_type enum, NULL if media_type
  * is unknown.
*)
// const char *av_get_media_type_string(enum AVMediaType media_type);
function av_get_media_type_string(media_type: AVMediaType): PAnsiChar; cdecl; external avutil_dll;

(* *
  * Return a single letter to describe the given picture type
  * pict_type.
  *
  * @param[in] pict_type the picture type @return a single character
  * representing the picture type, '?' if pict_type is unknown
*)
// char av_get_picture_type_char(enum AVPictureType pict_type);
function av_get_picture_type_char(pict_type: AVPictureType): AnsiChar; cdecl; external avutil_dll;

(* *
  * Return x default pointer in case p is NULL.
*)
// static inline void *av_x_if_null(const void *p, const void *x)
// {
// return (void *)(intptr_t)(p ? p : x);
// }
function av_x_if_null(const p: Pointer; const x: Pointer): Pointer; inline;

(* *
  * Compute the length of an integer list.
  *
  * @param elsize  size in bytes of each list element (only 1, 2, 4 or 8)
  * @param term    list terminator (usually 0 or -1)
  * @param list    pointer to the list
  * @return  length of the list, in elements, not counting the terminator
*)
// unsigned av_int_list_length_for_size(unsigned elsize,
// const void *list, uint64_t term) av_pure;
function av_int_list_length_for_size(elsize: unsigned; const list: Pointer; term: uint64_t): unsigned; cdecl; external avutil_dll;

(* *
  * Compute the length of an integer list.
  *
  * @param term  list terminator (usually 0 or -1)
  * @param list  pointer to the list
  * @return  length of the list, in elements, not counting the terminator
*)
// #define av_int_list_length(list, term) \
// av_int_list_length_for_size(sizeof(*(list)), list, term)
function av_int_list_length(list: Pointer; item_size: int; term: int64_t): int; inline;

(* *
  * Open a file using a UTF-8 filename.
  * The API of this function matches POSIX fopen(), errors are returned through
  * errno.
*)
// FILE *av_fopen_utf8(const char *path, const char *mode);
function av_fopen_utf8(const path: PAnsiChar; const mode: PAnsiChar): pFile; cdecl; external avutil_dll;

(* *
  * Return the fractional representation of the internal time base.
*)
// AVRational av_get_time_base_q(void);
function av_get_time_base_q(): AVRational; cdecl; external avutil_dll;

const
  AV_FOURCC_MAX_STRING_SIZE = 32;

  // #define av_fourcc2str(fourcc) av_fourcc_make_string((char[AV_FOURCC_MAX_STRING_SIZE]){0}, fourcc)

  (* *
    * Fill the provided buffer with a string containing a FourCC (four-character
    * code) representation.
    *
    * @param buf    a buffer with size in bytes of at least AV_FOURCC_MAX_STRING_SIZE
    * @param fourcc the fourcc to represent
    * @return the buffer in input
  *)
  // char *av_fourcc_make_string(char *buf, uint32_t fourcc);
function av_fourcc_make_string(buf: PAnsiChar; fourcc: uint32_t): PAnsiChar; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'file.h'}
(* *
  * Read the file with name filename, and put its content in a newly
  * allocated buffer or map it with mmap() when available.
  * In case of success set *bufptr to the read or mmapped buffer, and
  * *size to the size in bytes of the buffer in *bufptr.
  * The returned buffer must be released with av_file_unmap().
  *
  * @param log_offset loglevel offset used for logging
  * @param log_ctx context used for logging
  * @return a non negative number in case of success, a negative value
  * corresponding to an AVERROR error code in case of failure
*)
// av_warn_unused_result
// int av_file_map(const char *filename, uint8_t **bufptr, size_t *size,
// int log_offset, void *log_ctx);
function av_file_map(const filename: PAnsiChar; var bufptr: puint8_t; var size: size_t; log_offset: int; log_ctx: Pointer): int; cdecl; external avutil_dll;
(* *
  * Unmap or free the buffer bufptr created by av_file_map().
  *
  * @param size size in bytes of bufptr, must be the same as returned
  * by av_file_map()
*)
// void av_file_unmap(uint8_t *bufptr, size_t size);
procedure av_file_unmap(bufptr: puint8_t; size: size_t); cdecl; external avutil_dll;
(* *
  * Wrapper to work around the lack of mkstemp() on mingw.
  * Also, tries to create file in /tmp first, if possible.
  * *prefix can be a character constant; *filename will be allocated internally.
  * @return file descriptor of opened file (or negative value corresponding to an
  * AVERROR code on error)
  * and opened file name in **filename.
  * @note On very old libcs it is necessary to set a secure umask before
  *       calling this, av_tempfile() can't call umask itself as it is used in
  *       libraries and could interfere with the calling application.
  * @deprecated as fd numbers cannot be passed saftely between libs on some platforms
*)
// int av_tempfile(const char *prefix, char **filename, int log_offset, void *log_ctx);
function av_tempfile(const prefix: PAnsiChar; var filename: PAnsiChar; log_offset: int; log_ctx: Pointer): int; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'error.h'}

const
  // #define AVERROR_BSF_NOT_FOUND      FFERRTAG(0xF8,'B','S','F') //< Bitstream filter not found
  AVERROR_BSF_NOT_FOUND = -($F8 or (Ord('B') shl 8) or (Ord('S') shl 16) or (Ord('F') shl 24));
  // #define AVERROR_BUG                FFERRTAG( 'B','U','G','!') //< Internal bug, also see AVERROR_BUG2
  AVERROR_BUG = -(Ord('B') or (Ord('U') shl 8) or (Ord('G') shl 16) or (Ord('!') shl 24));
  // #define AVERROR_BUFFER_TOO_SMALL   FFERRTAG( 'B','U','F','S') //< Buffer too small
  AVERROR_BUFFER_TOO_SMALL = -(Ord('B') or (Ord('U') shl 8) or (Ord('F') shl 16) or (Ord('S') shl 24));
  // #define AVERROR_DECODER_NOT_FOUND  FFERRTAG(0xF8,'D','E','C') //< Decoder not found
  AVERROR_DECODER_NOT_FOUND = -($F8 or (Ord('D') shl 8) or (Ord('E') shl 16) or (Ord('C') shl 24));
  // #define AVERROR_DEMUXER_NOT_FOUND  FFERRTAG(0xF8,'D','E','M') //< Demuxer not found
  AVERROR_DEMUXER_NOT_FOUND = -($F8 or (Ord('D') shl 8) or (Ord('E') shl 16) or (Ord('M') shl 24));
  // #define AVERROR_ENCODER_NOT_FOUND  FFERRTAG(0xF8,'E','N','C') //< Encoder not found
  AVERROR_ENCODER_NOT_FOUND = -($F8 or (Ord('E') shl 8) or (Ord('N') shl 16) or (Ord('C') shl 24));
  // #define AVERROR_EOF                FFERRTAG( 'E','O','F',' ') //< End of file
  AVERROR_EOF = -(Ord('E') or (Ord('O') shl 8) or (Ord('F') shl 16) or (Ord(' ') shl 24));
  // #define AVERROR_EXIT               FFERRTAG( 'E','X','I','T') //< Immediate exit was requested; the called function should not be restarted
  AVERROR_EXIT = -(Ord('E') or (Ord('X') shl 8) or (Ord('I') shl 16) or (Ord('T') shl 24));
  // #define AVERROR_EXTERNAL           FFERRTAG( 'E','X','T',' ') //< Generic error in an external library
  AVERROR_EXTERNAL = -(Ord('E') or (Ord('X') shl 8) or (Ord('T') shl 16) or (Ord(' ') shl 24));
  // #define AVERROR_FILTER_NOT_FOUND   FFERRTAG(0xF8,'F','I','L') //< Filter not found
  AVERROR_FILTER_NOT_FOUND = -($F8 or (Ord('F') shl 8) or (Ord('I') shl 16) or (Ord('L') shl 24));
  // #define AVERROR_INVALIDDATA        FFERRTAG( 'I','N','D','A') //< Invalid data found when processing input
  AVERROR_INVALIDDATA = -(Ord('I') or (Ord('N') shl 8) or (Ord('D') shl 16) or (Ord('A') shl 24));
  // #define AVERROR_MUXER_NOT_FOUND    FFERRTAG(0xF8,'M','U','X') //< Muxer not found
  AVERROR_MUXER_NOT_FOUND = -($F8 or (Ord('M') shl 8) or (Ord('U') shl 16) or (Ord('X') shl 24));
  // #define AVERROR_OPTION_NOT_FOUND   FFERRTAG(0xF8,'O','P','T') //< Option not found
  AVERROR_OPTION_NOT_FOUND = -($F8 or (Ord('O') shl 8) or (Ord('P') shl 16) or (Ord('T') shl 24));
  // #define AVERROR_PATCHWELCOME       FFERRTAG( 'P','A','W','E') //< Not yet implemented in FFmpeg, patches welcome
  AVERROR_PATCHWELCOME = -(Ord('P') or (Ord('A') shl 8) or (Ord('W') shl 16) or (Ord('E') shl 24));
  // #define AVERROR_PROTOCOL_NOT_FOUND FFERRTAG(0xF8,'P','R','O') //< Protocol not found
  AVERROR_PROTOCOL_NOT_FOUND = -($F8 or (Ord('P') shl 8) or (Ord('R') shl 16) or (Ord('O') shl 24));

  // #define AVERROR_STREAM_NOT_FOUND   FFERRTAG(0xF8,'S','T','R') //< Stream not found
  AVERROR_STREAM_NOT_FOUND = -($F8 or (Ord('S') shl 8) or (Ord('T') shl 16) or (Ord('R') shl 24));

  (* *
    * This is semantically identical to AVERROR_BUG
    * it has been introduced in Libav after our AVERROR_BUG and with a modified value.
  *)
  // #define AVERROR_BUG2               FFERRTAG( 'B','U','G',' ')
  AVERROR_BUG2 = -(Ord('B') or (Ord('U') shl 8) or (Ord('G') shl 16) or (Ord(' ') shl 24));
  // #define AVERROR_UNKNOWN            FFERRTAG( 'U','N','K','N') //< Unknown error, typically from an external library
  AVERROR_UNKNOWN = -(Ord('U') or (Ord('N') shl 8) or (Ord('K') shl 16) or (Ord('N') shl 24));
  // #define AVERROR_EXPERIMENTAL       (-0x2bb2afa8) //< Requested feature is flagged experimental. Set strict_std_compliance if you really want to use it.
  AVERROR_EXPERIMENTAL = -$2BB2AFA8;
  // #define AVERROR_INPUT_CHANGED      (-0x636e6701) //< Input changed between calls. Reconfiguration is required. (can be OR-ed with AVERROR_OUTPUT_CHANGED)
  AVERROR_INPUT_CHANGED = -$636E6701;
  // #define AVERROR_OUTPUT_CHANGED     (-0x636e6702) //< Output changed between calls. Reconfiguration is required. (can be OR-ed with AVERROR_INPUT_CHANGED)
  AVERROR_OUTPUT_CHANGED = -$636E6702;

  // * HTTP & RTSP errors */
  // #define AVERROR_HTTP_BAD_REQUEST   FFERRTAG(0xF8,'4','0','0')
  AVERROR_HTTP_BAD_REQUEST = -($F8 or (Ord('4') shl 8) or (Ord('0') shl 16) or (Ord('0') shl 24));
  // #define AVERROR_HTTP_UNAUTHORIZED  FFERRTAG(0xF8,'4','0','1')
  AVERROR_HTTP_UNAUTHORIZED = -($F8 or (Ord('4') shl 8) or (Ord('0') shl 16) or (Ord('1') shl 24));
  // #define AVERROR_HTTP_FORBIDDEN     FFERRTAG(0xF8,'4','0','3')
  AVERROR_HTTP_FORBIDDEN = -($F8 or (Ord('4') shl 8) or (Ord('0') shl 16) or (Ord('3') shl 24));
  // #define AVERROR_HTTP_NOT_FOUND     FFERRTAG(0xF8,'4','0','4')
  AVERROR_HTTP_NOT_FOUND = -($F8 or (Ord('4') shl 8) or (Ord('0') shl 16) or (Ord('4') shl 24));
  // #define AVERROR_HTTP_OTHER_4XX     FFERRTAG(0xF8,'4','X','X')
  AVERROR_HTTP_OTHER_4XX = -($F8 or (Ord('4') shl 8) or (Ord('X') shl 16) or (Ord('X') shl 24));
  // #define AVERROR_HTTP_SERVER_ERROR  FFERRTAG(0xF8,'5','X','X')
  AVERROR_HTTP_SERVER_ERROR = -($F8 or (Ord('5') shl 8) or (Ord('X') shl 16) or (Ord('X') shl 24));

  AV_ERROR_MAX_STRING_SIZE = 64;

  // errno.h
  AVERROR_EPERM           = -1;   // < Operation not permitted
  AVERROR_ENOENT          = -2;   // < No such file or directory
  AVERROR_ESRCH           = -3;   // < No such process
  AVERROR_EINTR           = -4;   // < Interrupted function call
  AVERROR_EIO             = -5;   // < I/O error
  AVERROR_ENXIO           = -6;   // < No such device or address
  AVERROR_E2BIG           = -7;   // < Argument list too long
  AVERROR_ENOEXEC         = -8;   // < Exec format error
  AVERROR_EBADF           = -9;   // < Bad file number
  AVERROR_ECHILD          = -10;  // < No child processes
  AVERROR_EAGAIN          = -11;  // < Resource temporarily unavailable / Try again
  AVERROR_ENOMEM          = -12;  // < Not enough space / Out of memory
  AVERROR_EACCES          = -13;  // < Permission denied
  AVERROR_EFAULT          = -14;  // < Bad address
  AVERROR_ENOTBLK         = -15;  // < Block device required (WIN: Unknown error)
  AVERROR_EBUSY           = -16;  // < Device or resource busy
  AVERROR_EEXIST          = -17;  // < File exists
  AVERROR_EXDEV           = -18;  // < Cross-device link
  AVERROR_ENODEV          = -19;  // < No such device
  AVERROR_ENOTDIR         = -20;  // < Not a directory
  AVERROR_EISDIR          = -21;  // < Is a directory
  AVERROR_EINVAL          = -22;  // < Invalid argument
  AVERROR_ENFILE          = -23;  // < Too many open files in system / File table overflow
  AVERROR_EMFILE          = -24;  // < Too many open files
  AVERROR_ENOTTY          = -25;  // < Inappropriate I/O control operation / Not a typewriter
  AVERROR_ETXTBSY         = -26;  // < Text file busy (WIN: Unknown error)
  AVERROR_EFBIG           = -27;  // < File too large
  AVERROR_ENOSPC          = -28;  // < No space left on device
  AVERROR_ESPIPE          = -29;  // < Illegal seek
  AVERROR_EROFS           = -30;  // < Read-only file system
  AVERROR_EMLINK          = -31;  // < Too many links
  AVERROR_EPIPE           = -32;  // < Broken pipe
  AVERROR_EDOM            = -33;  // < Math argument out of domain of func
  AVERROR_ERANGE          = -34;  // < Math result not representable
  AVERROR_EDEADLK         = -36;  // < Resource deadlock avoided
  AVERROR_ENAMETOOLONG    = -38;  // < File name too long
  AVERROR_ENOLCK          = -39;  // < No locks available
  AVERROR_ENOSYS          = -40;  // < Function not implemented
  AVERROR_ENOTEMPTY       = -41;  // < Directory not empty
  AVERROR_ELOOP           = -114; // < Too many symbolic links encountered
  AVERROR_ENOMSG          = -91;  // < No message of desired type (WIN: Unknown error)
  AVERROR_EIDRM           = -90;  // < Identifier removed (WIN: Unknown error)
  AVERROR_ENOSTR          = -99;  // < Device not a stream
  AVERROR_ENODATA         = -96;  // < No data available
  AVERROR_ETIME           = -101; // < Timer expired
  AVERROR_ENOSR           = -98;  // < Out of streams resources
  AVERROR_EREMOTE         = -71;  // < Too many levels of remote in path
  AVERROR_ENOLINK         = -97;  // < Link has been severed
  AVERROR_EMULTIHOP       = -95;  // < Multihop attempted
  AVERROR_EBADMSG         = -94;  // < Not a data message
  AVERROR_EPROTO          = -134; // < Protocol error
  AVERROR_EOVERFLOW       = -132; // < Value too large for defined data type
  AVERROR_EILSEQ          = -42;  // < Illegal byte sequence
  AVERROR_EUSERS          = -68;  // < Too many users
  AVERROR_ENOTSOCK        = -128; // < Socket operation on non-socket
  AVERROR_EDESTADDRREQ    = -109; // < Destination address required
  AVERROR_EMSGSIZE        = -115; // < Message too long
  AVERROR_EPROTOTYPE      = -136; // < Protocol wrong type for socket
  AVERROR_ENOPROTOOPT     = -123; // < Protocol not available
  AVERROR_EPROTONOSUPPORT = -135; // < Protocol not supported
  AVERROR_ESOCKTNOSUPPORT = -44;  // < Socket type not supported
  AVERROR_EOPNOTSUPP      = -130; // < Operation not supported on transport endpoint
  AVERROR_EPFNOSUPPORT    = -46;  // < Protocol family not supported
  AVERROR_EAFNOSUPPORT    = -102; // < Address family not supported by protocol
  AVERROR_EADDRINUSE      = -100; // < Address already in use
  AVERROR_EADDRNOTAVAIL   = -101; // < Cannot assign requested address
  AVERROR_ENETDOWN        = -116; // < Network is down
  AVERROR_ENETUNREACH     = -118; // < Network is unreachable
  AVERROR_ENETRESET       = -117; // < Network dropped connection because of reset
  AVERROR_ECONNABORTED    = -106; // < Software caused connection abort
  AVERROR_ECONNRESET      = -108; // < Connection reset by peer
  AVERROR_ENOBUFS         = -119; // < No buffer space available
  AVERROR_EISCONN         = -113; // < Transport endpoint is already connected
  AVERROR_ENOTCONN        = -126; // < Transport endpoint is not connected
  AVERROR_ESHUTDOWN       = -58;  // < Cannot send after transport endpoint shutdown
  AVERROR_ETOOMANYREFS    = -59;  // < Too many references: cannot splice
  AVERROR_ETIMEDOUT       = -138; // < Connection timed out
  AVERROR_ECONNREFUSED    = -107; // < Connection refused
  AVERROR_EHOSTDOWN       = -64;  // < Host is down
  AVERROR_EHOSTUNREACH    = -110; // < No route to host
  AVERROR_EALREADY        = -103; // < Operation already in progress
  AVERROR_EINPROGRESS     = -112; // < Operation now in progress
  AVERROR_ESTALE          = -70;  // < Stale NFS file handle
  AVERROR_ECANCELED       = -105; // < Operation Canceled
  AVERROR_EOWNERDEAD      = -133; // < Owner died
  AVERROR_ENOTRECOVERABLE = -44;  // < State not recoverable

  WSABASEERR = -10000;
{$EXTERNALSYM WSABASEERR}
  WSAEINTR = WSABASEERR - 4;
{$EXTERNALSYM WSAEINTR}
  WSAEBADF = WSABASEERR - 9;
{$EXTERNALSYM WSAEBADF}
  WSAEACCES = WSABASEERR - 13;
{$EXTERNALSYM WSAEACCES}
  WSAEFAULT = WSABASEERR - 14;
{$EXTERNALSYM WSAEFAULT}
  WSAEINVAL = WSABASEERR - 22;
{$EXTERNALSYM WSAEINVAL}
  WSAEMFILE = WSABASEERR - 24;
{$EXTERNALSYM WSAEMFILE}
  WSAEWOULDBLOCK = WSABASEERR - 35;
{$EXTERNALSYM WSAEWOULDBLOCK}
  WSAEINPROGRESS = WSABASEERR - 36; (* deprecated on WinSock2 *)
{$EXTERNALSYM WSAEINPROGRESS}
  WSAEALREADY = WSABASEERR - 37;
{$EXTERNALSYM WSAEALREADY}
  WSAENOTSOCK = WSABASEERR - 38;
{$EXTERNALSYM WSAENOTSOCK}
  WSAEDESTADDRREQ = WSABASEERR - 39;
{$EXTERNALSYM WSAEDESTADDRREQ}
  WSAEMSGSIZE = WSABASEERR - 40;
{$EXTERNALSYM WSAEMSGSIZE}
  WSAEPROTOTYPE = WSABASEERR - 41;
{$EXTERNALSYM WSAEPROTOTYPE}
  WSAENOPROTOOPT = WSABASEERR - 42;
{$EXTERNALSYM WSAENOPROTOOPT}
  WSAEPROTONOSUPPORT = WSABASEERR - 43;
{$EXTERNALSYM WSAEPROTONOSUPPORT}
  WSAESOCKTNOSUPPORT = WSABASEERR - 44;
{$EXTERNALSYM WSAESOCKTNOSUPPORT}
  WSAEOPNOTSUPP = WSABASEERR - 45;
{$EXTERNALSYM WSAEOPNOTSUPP}
  WSAEPFNOSUPPORT = WSABASEERR - 46;
{$EXTERNALSYM WSAEPFNOSUPPORT}
  WSAEAFNOSUPPORT = WSABASEERR - 47;
{$EXTERNALSYM WSAEAFNOSUPPORT}
  WSAEADDRINUSE = WSABASEERR - 48;
{$EXTERNALSYM WSAEADDRINUSE}
  WSAEADDRNOTAVAIL = WSABASEERR - 49;
{$EXTERNALSYM WSAEADDRNOTAVAIL}
  WSAENETDOWN = WSABASEERR - 50;
{$EXTERNALSYM WSAENETDOWN}
  WSAENETUNREACH = WSABASEERR - 51;
{$EXTERNALSYM WSAENETUNREACH}
  WSAENETRESET = WSABASEERR - 52;
{$EXTERNALSYM WSAENETRESET}
  WSAECONNABORTED = WSABASEERR - 53;
{$EXTERNALSYM WSAECONNABORTED}
  WSAECONNRESET = WSABASEERR - 54;
{$EXTERNALSYM WSAECONNRESET}
  WSAENOBUFS = WSABASEERR - 55;
{$EXTERNALSYM WSAENOBUFS}
  WSAEISCONN = WSABASEERR - 56;
{$EXTERNALSYM WSAEISCONN}
  WSAENOTCONN = WSABASEERR - 57;
{$EXTERNALSYM WSAENOTCONN}
  WSAESHUTDOWN = WSABASEERR - 58;
{$EXTERNALSYM WSAESHUTDOWN}
  WSAETOOMANYREFS = WSABASEERR - 59;
{$EXTERNALSYM WSAETOOMANYREFS}
  WSAETIMEDOUT = WSABASEERR - 60;
{$EXTERNALSYM WSAETIMEDOUT}
  WSAECONNREFUSED = WSABASEERR - 61;
{$EXTERNALSYM WSAECONNREFUSED}
  WSAELOOP = WSABASEERR - 62;
{$EXTERNALSYM WSAELOOP}
  WSAENAMETOOLONG = WSABASEERR - 63;
{$EXTERNALSYM WSAENAMETOOLONG}
  WSAEHOSTDOWN = WSABASEERR - 64;
{$EXTERNALSYM WSAEHOSTDOWN}
  WSAEHOSTUNREACH = WSABASEERR - 65;
{$EXTERNALSYM WSAEHOSTUNREACH}
  WSAENOTEMPTY = WSABASEERR - 66;
{$EXTERNALSYM WSAENOTEMPTY}
  WSAEPROCLIM = WSABASEERR - 67;
{$EXTERNALSYM WSAEPROCLIM}
  WSAEUSERS = WSABASEERR - 68;
{$EXTERNALSYM WSAEUSERS}
  WSAEDQUOT = WSABASEERR - 69;
{$EXTERNALSYM WSAEDQUOT}
  WSAESTALE = WSABASEERR - 70;
{$EXTERNALSYM WSAESTALE}
  WSAEREMOTE = WSABASEERR - 71;
{$EXTERNALSYM WSAEREMOTE}
  WSAEDISCON = WSABASEERR - 101;
{$EXTERNALSYM WSAEDISCON}
  WSASYSNOTREADY = WSABASEERR - 91;
{$EXTERNALSYM WSASYSNOTREADY}
  WSAVERNOTSUPPORTED = WSABASEERR - 92;
{$EXTERNALSYM WSAVERNOTSUPPORTED}
  WSANOTINITIALISED = WSABASEERR - 93;
{$EXTERNALSYM WSANOTINITIALISED}
  WSAHOST_NOT_FOUND = WSABASEERR - 1001;
{$EXTERNALSYM WSAHOST_NOT_FOUND}
  WSATRY_AGAIN = WSABASEERR - 1002;
{$EXTERNALSYM WSATRY_AGAIN}
  WSANO_RECOVERY = WSABASEERR - 1003;
{$EXTERNALSYM WSANO_RECOVERY}
  WSANO_DATA = WSABASEERR - 1004;
{$EXTERNALSYM WSANO_DATA}
  (* WinSock2 specific error codes *)
  WSAENOMORE = WSABASEERR - 102;
{$EXTERNALSYM WSAENOMORE}
  WSAECANCELLED = WSABASEERR - 103;
{$EXTERNALSYM WSAECANCELLED}
  WSAEINVALIDPROCTABLE = WSABASEERR - 104;
{$EXTERNALSYM WSAEINVALIDPROCTABLE}
  WSAEINVALIDPROVIDER = WSABASEERR - 105;
{$EXTERNALSYM WSAEINVALIDPROVIDER}
  WSAEPROVIDERFAILEDINIT = WSABASEERR - 106;
{$EXTERNALSYM WSAEPROVIDERFAILEDINIT}
  WSASYSCALLFAILURE = WSABASEERR - 107;
{$EXTERNALSYM WSASYSCALLFAILURE}
  WSASERVICE_NOT_FOUND = WSABASEERR - 108;
{$EXTERNALSYM WSASERVICE_NOT_FOUND}
  WSATYPE_NOT_FOUND = WSABASEERR - 109;
{$EXTERNALSYM WSATYPE_NOT_FOUND}
  WSA_E_NO_MORE = WSABASEERR - 110;
{$EXTERNALSYM WSA_E_NO_MORE}
  WSA_E_CANCELLED = WSABASEERR - 111;
{$EXTERNALSYM WSA_E_CANCELLED}
  WSAEREFUSED = WSABASEERR - 112;
{$EXTERNALSYM WSAEREFUSED}
  (* WS QualityofService errors *)
  WSA_QOS_RECEIVERS = WSABASEERR - 1005;
{$EXTERNALSYM WSA_QOS_RECEIVERS}
  WSA_QOS_SENDERS = WSABASEERR - 1006;
{$EXTERNALSYM WSA_QOS_SENDERS}
  WSA_QOS_NO_SENDERS = WSABASEERR - 1007;
{$EXTERNALSYM WSA_QOS_NO_SENDERS}
  WSA_QOS_NO_RECEIVERS = WSABASEERR - 1008;
{$EXTERNALSYM WSA_QOS_NO_RECEIVERS}
  WSA_QOS_REQUEST_CONFIRMED = WSABASEERR - 1009;
{$EXTERNALSYM WSA_QOS_REQUEST_CONFIRMED}
  WSA_QOS_ADMISSION_FAILURE = WSABASEERR - 1010;
{$EXTERNALSYM WSA_QOS_ADMISSION_FAILURE}
  WSA_QOS_POLICY_FAILURE = WSABASEERR - 1011;
{$EXTERNALSYM WSA_QOS_POLICY_FAILURE}
  WSA_QOS_BAD_STYLE = WSABASEERR - 1012;
{$EXTERNALSYM WSA_QOS_BAD_STYLE}
  WSA_QOS_BAD_OBJECT = WSABASEERR - 1013;
{$EXTERNALSYM WSA_QOS_BAD_OBJECT}
  WSA_QOS_TRAFFIC_CTRL_ERROR = WSABASEERR - 1014;
{$EXTERNALSYM WSA_QOS_TRAFFIC_CTRL_ERROR}
  WSA_QOS_GENERIC_ERROR = WSABASEERR - 1015;
{$EXTERNALSYM WSA_QOS_GENERIC_ERROR}
  WSA_QOS_ESERVICETYPE = WSABASEERR - 1016;
{$EXTERNALSYM WSA_QOS_ESERVICETYPE}
  WSA_QOS_EFLOWSPEC = WSABASEERR - 1017;
{$EXTERNALSYM WSA_QOS_EFLOWSPEC}
  WSA_QOS_EPROVSPECBUF = WSABASEERR - 1018;
{$EXTERNALSYM WSA_QOS_EPROVSPECBUF}
  WSA_QOS_EFILTERSTYLE = WSABASEERR - 1019;
{$EXTERNALSYM WSA_QOS_EFILTERSTYLE}
  WSA_QOS_EFILTERTYPE = WSABASEERR - 1020;
{$EXTERNALSYM WSA_QOS_EFILTERTYPE}
  WSA_QOS_EFILTERCOUNT = WSABASEERR - 1021;
{$EXTERNALSYM WSA_QOS_EFILTERCOUNT}
  WSA_QOS_EOBJLENGTH = WSABASEERR - 1022;
{$EXTERNALSYM WSA_QOS_EOBJLENGTH}
  WSA_QOS_EFLOWCOUNT = WSABASEERR - 1023;
{$EXTERNALSYM WSA_QOS_EFLOWCOUNT}
  WSA_QOS_EUNKOWNPSOBJ = WSABASEERR - 1024;
{$EXTERNALSYM WSA_QOS_EUNKOWNPSOBJ}
  WSA_QOS_EPOLICYOBJ = WSABASEERR - 1025;
{$EXTERNALSYM WSA_QOS_EPOLICYOBJ}
  WSA_QOS_EFLOWDESC = WSABASEERR - 1026;
{$EXTERNALSYM WSA_QOS_EFLOWDESC}
  WSA_QOS_EPSFLOWSPEC = WSABASEERR - 1027;
{$EXTERNALSYM WSA_QOS_EPSFLOWSPEC}
  WSA_QOS_EPSFILTERSPEC = WSABASEERR - 1028;
{$EXTERNALSYM WSA_QOS_EPSFILTERSPEC}
  WSA_QOS_ESDMODEOBJ = WSABASEERR - 1029;
{$EXTERNALSYM WSA_QOS_ESDMODEOBJ}
  WSA_QOS_ESHAPERATEOBJ = WSABASEERR - 1030;
{$EXTERNALSYM WSA_QOS_ESHAPERATEOBJ}
  WSA_QOS_RESERVED_PETYPE = WSABASEERR - 1031;
{$EXTERNALSYM WSA_QOS_RESERVED_PETYPE}

type
  TErrorItem = record
    err: Integer;
    msg: string;
  end;

const
  CErrorList: array [0 .. 173] of TErrorItem = ((err: WSAEINTR; msg: 'Interrupted function call'), (err: WSAEBADF; msg: 'Bad file number'), (err: WSAEACCES;
    msg: 'Permission denied'), (err: WSAEFAULT; msg: 'Bad address'), (err: WSAEINVAL; msg: 'Invalid argument / Invalid data found when processing input'),
    (err: WSAEMFILE; msg: 'Too many open files'), (err: WSAENAMETOOLONG; msg: 'File name too long'), (err: WSAENOTEMPTY; msg: 'Directory not empty'),
    (err: WSAELOOP; msg: 'Too many symbolic links encountered'), (err: WSAEREMOTE; msg: 'Too many levels of remote in path'), (err: WSAEUSERS;
    msg: 'Too many users'), (err: WSAENOTSOCK; msg: 'Socket operation on non-socket'), (err: WSAEDESTADDRREQ; msg: 'Destination address required'),
    (err: WSAEMSGSIZE; msg: 'Message too long'), (err: WSAEPROTOTYPE; msg: 'Protocol wrong type for socket'), (err: WSAENOPROTOOPT;
    msg: 'Protocol not available'), (err: WSAEPROTONOSUPPORT; msg: 'Protocol not supported'), (err: WSAESOCKTNOSUPPORT; msg: 'Socket type not supported'),
    (err: WSAEOPNOTSUPP; msg: 'Operation not supported on transport endpoint'), (err: WSAEPFNOSUPPORT; msg: 'Protocol family not supported'),
    (err: WSAEAFNOSUPPORT; msg: 'Address family not supported by protocol'), (err: WSAEADDRINUSE; msg: 'Address already in use'), (err: WSAEADDRNOTAVAIL;
    msg: 'Cannot assign requested address'), (err: WSAENETDOWN; msg: 'Network is down'), (err: WSAENETUNREACH; msg: 'Network is unreachable'),
    (err: WSAENETRESET; msg: 'Network dropped connection because of reset'), (err: WSAECONNABORTED; msg: 'Software caused connection abort'),
    (err: WSAECONNRESET; msg: 'Connection reset by peer'), (err: WSAENOBUFS; msg: 'No buffer space available'), (err: WSAEISCONN;
    msg: 'Transport endpoint is already connected'), (err: WSAENOTCONN; msg: 'Transport endpoint is not connected'), (err: WSAESHUTDOWN;
    msg: 'Cannot send after transport endpoint shutdown'), (err: WSAETOOMANYREFS; msg: 'Too many references: cannot splice'), (err: WSAETIMEDOUT;
    msg: 'Connection timed out'), (err: WSAECONNREFUSED; msg: 'Connection refused'), (err: WSAEHOSTDOWN; msg: 'Host is down'), (err: WSAEHOSTUNREACH;
    msg: 'No route to host'), (err: WSAEALREADY; msg: 'Operation already in progress'), (err: WSAEINPROGRESS; msg: 'Operation now in progress'),
    (err: WSAESTALE; msg: 'Stale NFS file handle'), (err: WSAEDQUOT; msg: 'Quota exceeded'), (err: WSAEWOULDBLOCK; msg: 'WSAEWOULDBLOCK'), (err: WSAEPROCLIM;
    msg: 'WSAEPROCLIM'), (err: WSAEDISCON; msg: 'WSAEDISCON'), (err: WSASYSNOTREADY; msg: 'WSASYSNOTREADY'), (err: WSAVERNOTSUPPORTED;
    msg: 'WSAVERNOTSUPPORTED'), (err: WSANOTINITIALISED; msg: 'WSANOTINITIALISED'), (err: WSAHOST_NOT_FOUND; msg: 'WSAHOST_NOT_FOUND'), (err: WSATRY_AGAIN;
    msg: 'WSATRY_AGAIN'), (err: WSANO_RECOVERY; msg: 'WSANO_RECOVERY'), (err: WSANO_DATA; msg: 'WSANO_DATA'), (err: WSAENOMORE; msg: 'WSAENOMORE'),
    (err: WSAECANCELLED; msg: 'WSAECANCELLED'), (err: WSAEINVALIDPROCTABLE; msg: 'WSAEINVALIDPROCTABLE'), (err: WSAEINVALIDPROVIDER;
    msg: 'WSAEINVALIDPROVIDER'), (err: WSAEPROVIDERFAILEDINIT; msg: 'WSAEPROVIDERFAILEDINIT'), (err: WSASYSCALLFAILURE; msg: 'WSASYSCALLFAILURE'),
    (err: WSASERVICE_NOT_FOUND; msg: 'WSASERVICE_NOT_FOUND'), (err: WSATYPE_NOT_FOUND; msg: 'WSATYPE_NOT_FOUND'), (err: WSA_E_NO_MORE; msg: 'WSA_E_NO_MORE'),
    (err: WSA_E_CANCELLED; msg: 'WSA_E_CANCELLED'), (err: WSAEREFUSED; msg: 'WSAEREFUSED'), //
    (err: AVERROR_BSF_NOT_FOUND; msg: 'Bitstream filter not found'), (err: AVERROR_BUG; msg: 'Internal bug, should not have happened'), (err: AVERROR_BUG2;
    msg: 'Internal bug, should not have happened'), (err: AVERROR_BUFFER_TOO_SMALL; msg: 'Buffer too small'), (err: AVERROR_DECODER_NOT_FOUND;
    msg: 'Decoder not found'), (err: AVERROR_DEMUXER_NOT_FOUND; msg: 'Demuxer not found'), (err: AVERROR_ENCODER_NOT_FOUND; msg: 'Encoder not found'),
    (err: AVERROR_EOF; msg: 'End of file'), (err: AVERROR_EXIT; msg: 'Immediate exit requested'), (err: AVERROR_EXTERNAL;
    msg: 'Generic error in an external library'), (err: AVERROR_FILTER_NOT_FOUND; msg: 'Filter not found'), (err: AVERROR_INVALIDDATA;
    msg: 'Invalid data found when processing input'), (err: AVERROR_MUXER_NOT_FOUND; msg: 'Muxer not found'), (err: AVERROR_OPTION_NOT_FOUND;
    msg: 'Option not found'), (err: AVERROR_PATCHWELCOME; msg: 'Not yet implemented in FFmpeg, patches welcome'), (err: AVERROR_PROTOCOL_NOT_FOUND;
    msg: 'Protocol not found'), (err: AVERROR_STREAM_NOT_FOUND; msg: 'Stream not found'), (err: AVERROR_UNKNOWN; msg: 'Unknown error occurred'),
    (err: AVERROR_EXPERIMENTAL; msg: 'Requested feature is flagged experimental. Set strict_std_compliance if you really want to use it.'),
    (err: AVERROR_INPUT_CHANGED; msg: 'Input changed between calls. Reconfiguration is required. (can be OR-ed with AVERROR_OUTPUT_CHANGED)'),
    (err: AVERROR_OUTPUT_CHANGED; msg: 'Output changed between calls. Reconfiguration is required. (can be OR-ed with AVERROR_INPUT_CHANGED)'),
    (err: AVERROR_HTTP_BAD_REQUEST; msg: 'HTTP or RTSP error: bad request(400)'), (err: AVERROR_HTTP_UNAUTHORIZED;
    msg: 'HTTP or RTSP error: unauthorized(401)'), (err: AVERROR_HTTP_FORBIDDEN; msg: 'HTTP or RTSP error: forbidden(403)'), (err: AVERROR_HTTP_NOT_FOUND;
    msg: 'HTTP or RTSP error: not found(404)'), (err: AVERROR_HTTP_OTHER_4XX; msg: 'HTTP or RTSP error: other error(4xx)'), (err: AVERROR_HTTP_SERVER_ERROR;
    msg: 'HTTP or RTSP error: server error(5xx)'), (err: AVERROR_ENOENT; msg: 'No such file or directory'), (err: AVERROR_ESRCH; msg: 'No such process'),
    (err: AVERROR_EINTR; msg: 'Interrupted function call'), (err: AVERROR_EIO; msg: 'I/O error'), (err: AVERROR_ENXIO; msg: 'No such device or address'),
    (err: AVERROR_E2BIG; msg: 'Argument list too long'), (err: AVERROR_ENOEXEC; msg: 'Exec format error'), (err: AVERROR_EBADF; msg: 'Bad file number'),
    (err: AVERROR_ECHILD; msg: 'No child processes'), (err: AVERROR_EAGAIN; msg: 'Resource temporarily unavailable / Try again'), (err: AVERROR_ENOMEM;
    msg: 'Not enough space / Out of memory'), (err: AVERROR_EACCES; msg: 'Permission denied'), (err: AVERROR_EFAULT; msg: 'Bad address'), (err: AVERROR_ENOTBLK;
    msg: 'Unknown error'), (err: AVERROR_EBUSY; msg: 'Device or resource busy'), (err: AVERROR_EEXIST; msg: 'File exists'), (err: AVERROR_EXDEV;
    msg: 'Cross-device link'), (err: AVERROR_ENODEV; msg: 'No such device'), (err: AVERROR_ENOTDIR; msg: 'Not a directory'), (err: AVERROR_EISDIR;
    msg: 'Is a directory'), (err: AVERROR_EINVAL; msg: 'Invalid argument / Invalid data found when processing input'), (err: AVERROR_ENFILE;
    msg: 'Too many open files in system / File table overflow'), (err: AVERROR_EMFILE; msg: 'Too many open files'), (err: AVERROR_ENOTTY;
    msg: 'Inappropriate I/O control operation / Not a typewriter'), (err: AVERROR_ETXTBSY; msg: 'Unknown error'), (err: AVERROR_EFBIG; msg: 'File too large'),
    (err: AVERROR_ENOSPC; msg: 'No space left on device'), (err: AVERROR_ESPIPE; msg: 'Illegal seek'), (err: AVERROR_EROFS; msg: 'Read-only file system'),
    (err: AVERROR_EMLINK; msg: 'Too many links'), (err: AVERROR_EPIPE; msg: 'Broken pipe'), (err: AVERROR_EDOM; msg: 'Math argument out of domain of func'),
    (err: AVERROR_ERANGE; msg: 'Math result not representable'), (err: AVERROR_EDEADLK; msg: 'Resource deadlock avoided'), (err: AVERROR_ENAMETOOLONG;
    msg: 'File name too long'), (err: AVERROR_ENOLCK; msg: 'No locks available'), (err: AVERROR_ENOSYS; msg: 'Function not implemented'),
    (err: AVERROR_ENOTEMPTY; msg: 'Directory not empty'), (err: AVERROR_ELOOP; msg: 'Too many symbolic links encountered'), (err: AVERROR_ENOMSG;
    msg: 'Unknown error'), (err: AVERROR_EIDRM; msg: 'Unknown error'), (err: AVERROR_ENOSTR; msg: 'Unknown error'), (err: AVERROR_ENODATA;
    msg: 'Unknown error'), (err: AVERROR_ETIME; msg: 'Unknown error'), (err: AVERROR_ENOSR; msg: 'Unknown error'), (err: AVERROR_EREMOTE; msg: 'Unknown error'),
    (err: AVERROR_ENOLINK; msg: 'Unknown error'), (err: AVERROR_EPROTO; msg: 'Protocol error'), (err: AVERROR_EMULTIHOP; msg: 'Unknown error'),
    (err: AVERROR_EBADMSG; msg: 'Unknown error'), (err: AVERROR_EOVERFLOW; msg: 'Value too large for defined data type'), (err: AVERROR_EILSEQ;
    msg: 'Illegal byte sequence'), (err: AVERROR_EUSERS; msg: 'Unknown error'), (err: AVERROR_ENOTSOCK; msg: 'Socket operation on non-socket'),
    (err: AVERROR_EDESTADDRREQ; msg: 'Destination address required'), (err: AVERROR_EMSGSIZE; msg: 'Message too long'), (err: AVERROR_EPROTOTYPE;
    msg: 'Protocol wrong type for socket'), (err: AVERROR_ENOPROTOOPT; msg: 'Protocol not available'), (err: AVERROR_EPROTONOSUPPORT;
    msg: 'Protocol not supported'), (err: AVERROR_ESOCKTNOSUPPORT; msg: 'Unknown error'), (err: AVERROR_EOPNOTSUPP;
    msg: 'Operation not supported on transport endpoint'), (err: AVERROR_EPFNOSUPPORT; msg: 'Unknown error'), (err: AVERROR_EAFNOSUPPORT;
    msg: 'Address family not supported by protocol'), (err: AVERROR_EADDRINUSE; msg: 'Address already in use'), (err: AVERROR_EADDRNOTAVAIL;
    msg: 'Cannot assign requested address'), (err: AVERROR_ENETDOWN; msg: 'Network is down'), (err: AVERROR_ENETUNREACH; msg: 'Network is unreachable'),
    (err: AVERROR_ENETRESET; msg: 'Network dropped connection because of reset'), (err: AVERROR_ECONNABORTED; msg: 'Software caused connection abort'),
    (err: AVERROR_ECONNRESET; msg: 'Connection reset by peer'), (err: AVERROR_ENOBUFS; msg: 'No buffer space available'), (err: AVERROR_EISCONN;
    msg: 'Transport endpoint is already connected'), (err: AVERROR_ENOTCONN; msg: 'Transport endpoint is not connected'), (err: AVERROR_ESHUTDOWN;
    msg: 'Unknown error'), (err: AVERROR_ETOOMANYREFS; msg: 'Unknown error'), (err: AVERROR_ETIMEDOUT; msg: 'Connection timed out'), (err: AVERROR_ECONNREFUSED;
    msg: 'Connection refused'), (err: AVERROR_EHOSTDOWN; msg: 'Unknown error'), (err: AVERROR_EHOSTUNREACH; msg: 'No route to host'), (err: AVERROR_EALREADY;
    msg: 'Operation already in progress'), (err: AVERROR_EINPROGRESS; msg: 'Operation now in progress'), (err: AVERROR_ESTALE; msg: 'Unknown error'),
    (err: AVERROR_ECANCELED; msg: 'Operation Canceled'), (err: AVERROR_EOWNERDEAD; msg: 'Owner died'), (err: AVERROR_ENOTRECOVERABLE;
    msg: 'State not recoverable'));

  (* *
    * Put a description of the AVERROR code errnum in errbuf.
    * In case of failure the global variable errno is set to indicate the
    * error. Even in case of failure av_strerror() will print a generic
    * error message indicating the errnum provided to errbuf.
    *
    * @param errnum      error code to describe
    * @param errbuf      buffer to which description is written
    * @param errbuf_size the size in bytes of errbuf
    * @return 0 on success, a negative value if a description for errnum
    * cannot be found
  *)
  // int av_strerror(int errnum, char *errbuf, size_t errbuf_size);
function av_strerror(errnum: int; errbuf: PAnsiChar; errbuf_size: size_t): int; cdecl; external avutil_dll;
(* *
  * Fill the provided buffer with a string containing an error string
  * corresponding to the AVERROR code errnum.
  *
  * @param errbuf         a buffer
  * @param errbuf_size    size in bytes of errbuf
  * @param errnum         error code to describe
  * @return the buffer in input, filled with the error description
  * @see av_strerror()
*)
// static inline char *av_make_error_string(char *errbuf, size_t errbuf_size, int errnum)
function av_make_error_string(errbuf: PAnsiChar; errbuf_size: size_t; errnum: int): PAnsiChar; inline;

(* *
  * Convenience macro, the return value should be used only directly in
  * function arguments but never stand-alone.
*)
function av_err2str(errnum: int): PAnsiChar;

{$ENDREGION}
{$REGION 'cpu.h'}

const
  AV_CPU_FLAG_FORCE = $80000000; (* force usage of selected flags (OR) *)

  (* lower 16 bits - CPU features *)
  AV_CPU_FLAG_MMX      = $0001;     // < standard MMX
  AV_CPU_FLAG_MMXEXT   = $0002;     // < SSE integer functions or AMD MMX ext
  AV_CPU_FLAG_MMX2     = $0002;     // < SSE integer functions or AMD MMX ext
  AV_CPU_FLAG_3DNOW    = $0004;     // < AMD 3DNOW
  AV_CPU_FLAG_SSE      = $0008;     // < SSE functions
  AV_CPU_FLAG_SSE2     = $0010;     // < PIV SSE2 functions
  AV_CPU_FLAG_SSE2SLOW = $40000000; // < SSE2 supported, but usually not faster
  // < than regular MMX/SSE (e.g. Core1)
  AV_CPU_FLAG_3DNOWEXT = $0020;     // < AMD 3DNowExt
  AV_CPU_FLAG_SSE3     = $0040;     // < Prescott SSE3 functions
  AV_CPU_FLAG_SSE3SLOW = $20000000; // < SSE3 supported, but usually not faster
  // < than regular MMX/SSE (e.g. Core1)
  AV_CPU_FLAG_SSSE3     = $0080;     // < Conroe SSSE3 functions
  AV_CPU_FLAG_SSSE3SLOW = $4000000;  // < SSSE3 supported, but usually not faster
  AV_CPU_FLAG_ATOM      = $10000000; // < Atom processor, some SSSE3 instructions are slower
  AV_CPU_FLAG_SSE4      = $0100;     // < Penryn SSE4.1 functions
  AV_CPU_FLAG_SSE42     = $0200;     // < Nehalem SSE4.2 functions
  AV_CPU_FLAG_AESNI     = $80000;    // < Advanced Encryption Standard functions
  AV_CPU_FLAG_AVX       = $4000;     // < AVX functions: requires OS support even if YMM registers aren't used
  AV_CPU_FLAG_AVXSLOW   = $8000000;  // < AVX supported, but slow when using YMM registers (e.g. Bulldozer)
  AV_CPU_FLAG_XOP       = $0400;     // < Bulldozer XOP functions
  AV_CPU_FLAG_FMA4      = $0800;     // < Bulldozer FMA4 functions
  AV_CPU_FLAG_CMOV      = $1000;     // < supports cmov instruction
  AV_CPU_FLAG_AVX2      = $8000;     // < AVX2 functions: requires OS support even if YMM registers aren't used
  AV_CPU_FLAG_FMA3      = $10000;    // < Haswell FMA3 functions
  AV_CPU_FLAG_BMI1      = $20000;    // < Bit Manipulation Instruction Set 1
  AV_CPU_FLAG_BMI2      = $40000;    // < Bit Manipulation Instruction Set 2
  AV_CPU_FLAG_AVX512    = $100000;   // < AVX-512 functions: requires OS support even if YMM/ZMM registers aren't used

  AV_CPU_FLAG_ALTIVEC = $0001; // < standard
  AV_CPU_FLAG_VSX     = $0002; // < ISA 2.06
  AV_CPU_FLAG_POWER8  = $0004; // < ISA 2.07

  AV_CPU_FLAG_ARMV5TE = (1 shl 0);
  AV_CPU_FLAG_ARMV6   = (1 shl 1);
  AV_CPU_FLAG_ARMV6T2 = (1 shl 2);
  AV_CPU_FLAG_VFP     = (1 shl 3);
  AV_CPU_FLAG_VFPV3   = (1 shl 4);
  AV_CPU_FLAG_NEON    = (1 shl 5);
  AV_CPU_FLAG_ARMV8   = (1 shl 6);
  AV_CPU_FLAG_VFP_VM  = (1 shl 7); // < VFPv2 vector mode, deprecated in ARMv7-A and unavailable in various CPUs implementations
  AV_CPU_FLAG_SETEND  = (1 shl 16);

  (* *
    * Return the flags which specify extensions supported by the CPU.
    * The returned value is affected by av_force_cpu_flags() if that was used
    * before. So av_get_cpu_flags() can easily be used in an application to
    * detect the enabled cpu flags.
  *)
  // int av_get_cpu_flags(void);
function av_get_cpu_flags(): int; cdecl; external avutil_dll;
(* *
  * Disables cpu detection and forces the specified flags.
  * -1 is a special case that disables forcing of specific flags.
*)
// void av_force_cpu_flags(int flags);
procedure av_force_cpu_flags(flags: int); cdecl; external avutil_dll;
(* *
  * Set a mask on flags returned by av_get_cpu_flags().
  * This function is mainly useful for testing.
  * Please use av_force_cpu_flags() and av_get_cpu_flags() instead which are more flexible
*)
// attribute_deprecated void av_set_cpu_flags_mask(int mask);
procedure av_set_cpu_flags_mask(mask: int); cdecl; external avutil_dll;
  deprecated 'Please use av_force_cpu_flags() and av_get_cpu_flags() instead which are more flexible';
(* *
  * Parse CPU flags from a string.
  *
  * The returned flags contain the specified flags as well as related unspecified flags.
  *
  * This function exists only for compatibility with libav.
  * Please use av_parse_cpu_caps() when possible.
  * @return a combination of AV_CPU_* flags, negative on error.
*)
// attribute_deprecated int av_parse_cpu_flags(const char *s);
function av_parse_cpu_flags(const s: PAnsiChar): int; cdecl; external avutil_dll; deprecated 'Please use av_parse_cpu_caps() when possible';
(* *
  * Parse CPU caps from a string and update the given AV_CPU_* flags based on that.
  *
  * @return negative on error.
*)
// int av_parse_cpu_caps(unsigned *flags, const char *s);
function av_parse_cpu_caps(var flags: unsigned; const s: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * @return the number of logical CPU cores present.
*)
// int av_cpu_count(void);
function av_cpu_count(): int; cdecl; external avutil_dll;
(* *
  * Get the maximum data alignment that may be required by FFmpeg.
  *
  * Note that this is affected by the build configuration and the CPU flags mask,
  * so e.g. if the CPU supports AVX, but libavutil has been built with
  * --disable-avx or the AV_CPU_FLAG_AVX flag has been disabled through
  *  av_set_cpu_flags_mask(), then this function will behave as if AVX is not
  *  present.
*)
// size_t av_cpu_max_align(void);
function av_cpu_max_align(): size_t; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'audio_fifo.h'}

(* *
  * Context for an Audio FIFO Buffer.
  *
  * - Operates at the sample level rather than the byte level.
  * - Supports multiple channels with either planar or packed sample format.
  * - Automatic reallocation when writing to a full buffer.
*)
type
  pAVAudioFifo = ^AVAudioFifo;

  AVAudioFifo = record
  end;

  (* *
    * Free an AVAudioFifo.
    *
    * @param af  AVAudioFifo to free
  *)
  // void av_audio_fifo_free(AVAudioFifo *af);
procedure av_audio_fifo_free(af: pAVAudioFifo); cdecl; external avutil_dll;
(* *
  * Allocate an AVAudioFifo.
  *
  * @param sample_fmt  sample format
  * @param channels    number of channels
  * @param nb_samples  initial allocation size, in samples
  * @return            newly allocated AVAudioFifo, or NULL on error
*)
// AVAudioFifo *av_audio_fifo_alloc(enum AVSampleFormat sample_fmt, int channels,
// int nb_samples);
function av_audio_fifo_alloc(sample_fmt: AVSampleFormat; channels: int; nb_samples: int): pAVAudioFifo; cdecl; external avutil_dll;
(* *
  * Reallocate an AVAudioFifo.
  *
  * @param af          AVAudioFifo to reallocate
  * @param nb_samples  new allocation size, in samples
  * @return            0 if OK, or negative AVERROR code on failure
*)
// av_warn_unused_result
// int av_audio_fifo_realloc(AVAudioFifo *af, int nb_samples);
function av_audio_fifo_realloc(af: pAVAudioFifo; nb_samples: int): int; cdecl; external avutil_dll;
(* *
  * Write data to an AVAudioFifo.
  *
  * The AVAudioFifo will be reallocated automatically if the available space
  * is less than nb_samples.
  *
  * @see enum AVSampleFormat
  * The documentation for AVSampleFormat describes the data layout.
  *
  * @param af          AVAudioFifo to write to
  * @param data        audio data plane pointers
  * @param nb_samples  number of samples to write
  * @return            number of samples actually written, or negative AVERROR
  *                    code on failure. If successful, the number of samples
  *                    actually written will always be nb_samples.
*)
// int av_audio_fifo_write(AVAudioFifo *af, void **data, int nb_samples);
function av_audio_fifo_write(af: pAVAudioFifo; var data: puint8_t; nb_samples: int): int; cdecl; external avutil_dll;
(* *
  * Peek data from an AVAudioFifo.
  *
  * @see enum AVSampleFormat
  * The documentation for AVSampleFormat describes the data layout.
  *
  * @param af          AVAudioFifo to read from
  * @param data        audio data plane pointers
  * @param nb_samples  number of samples to peek
  * @return            number of samples actually peek, or negative AVERROR code
  *                    on failure. The number of samples actually peek will not
  *                    be greater than nb_samples, and will only be less than
  *                    nb_samples if av_audio_fifo_size is less than nb_samples.
*)
// int av_audio_fifo_peek(AVAudioFifo *af, void **data, int nb_samples);
function av_audio_fifo_peek(af: pAVAudioFifo; var data: puint8_t; nb_samples: int): int; cdecl; external avutil_dll;
(* *
  * Peek data from an AVAudioFifo.
  *
  * @see enum AVSampleFormat
  * The documentation for AVSampleFormat describes the data layout.
  *
  * @param af          AVAudioFifo to read from
  * @param data        audio data plane pointers
  * @param nb_samples  number of samples to peek
  * @param offset      offset from current read position
  * @return            number of samples actually peek, or negative AVERROR code
  *                    on failure. The number of samples actually peek will not
  *                    be greater than nb_samples, and will only be less than
  *                    nb_samples if av_audio_fifo_size is less than nb_samples.
*)
// int av_audio_fifo_peek_at(AVAudioFifo *af, void **data, int nb_samples, int offset);
function av_audio_fifo_peek_at(af: pAVAudioFifo; var data: Pointer; nb_samples: int; offset: int): int; cdecl; external avutil_dll;
(* *
  * Read data from an AVAudioFifo.
  *
  * @see enum AVSampleFormat
  * The documentation for AVSampleFormat describes the data layout.
  *
  * @param af          AVAudioFifo to read from
  * @param data        audio data plane pointers
  * @param nb_samples  number of samples to read
  * @return            number of samples actually read, or negative AVERROR code
  *                    on failure. The number of samples actually read will not
  *                    be greater than nb_samples, and will only be less than
  *                    nb_samples if av_audio_fifo_size is less than nb_samples.
*)
// int av_audio_fifo_read(AVAudioFifo *af, void **data, int nb_samples);
function av_audio_fifo_read(af: pAVAudioFifo; var data: Pointer; nb_samples: int): int; cdecl; external avutil_dll;
(* *
  * Drain data from an AVAudioFifo.
  *
  * Removes the data without reading it.
  *
  * @param af          AVAudioFifo to drain
  * @param nb_samples  number of samples to drain
  * @return            0 if OK, or negative AVERROR code on failure
*)
// int av_audio_fifo_drain(AVAudioFifo *af, int nb_samples);
function av_audio_fifo_drain(af: pAVAudioFifo; nb_samples: int): int; cdecl; external avutil_dll;
(* *
  * Reset the AVAudioFifo buffer.
  *
  * This empties all data in the buffer.
  *
  * @param af  AVAudioFifo to reset
*)
// void av_audio_fifo_reset(AVAudioFifo *af);
procedure av_audio_fifo_reset(af: pAVAudioFifo); cdecl; external avutil_dll;
(* *
  * Get the current number of samples in the AVAudioFifo available for reading.
  *
  * @param af  the AVAudioFifo to query
  * @return    number of samples available for reading
*)
// int av_audio_fifo_size(AVAudioFifo *af);
function av_audio_fifo_size(af: pAVAudioFifo): int; cdecl; external avutil_dll;
(* *
  * Get the current number of samples in the AVAudioFifo available for writing.
  *
  * @param af  the AVAudioFifo to query
  * @return    number of samples available for writing
*)
// int av_audio_fifo_space(AVAudioFifo *af);
function av_audio_fifo_space(af: pAVAudioFifo): int; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'avstring.h'}
(* *
  * Return non-zero if pfx is a prefix of str. If it is, *ptr is set to
  * the address of the first character in str after the prefix.
  *
  * @param str input string
  * @param pfx prefix to test
  * @param ptr updated if the prefix is matched inside str
  * @return non-zero if the prefix matches, zero otherwise
*)
// int av_strstart(const char *str, const char *pfx, const char **ptr);
function av_strstart(const str: PAnsiChar; const pfx: PAnsiChar; const ptr: ppAnsiChar): int; cdecl; external avutil_dll;
(* *
  * Return non-zero if pfx is a prefix of str independent of case. If
  * it is, *ptr is set to the address of the first character in str
  * after the prefix.
  *
  * @param str input string
  * @param pfx prefix to test
  * @param ptr updated if the prefix is matched inside str
  * @return non-zero if the prefix matches, zero otherwise
*)
// int av_stristart(const char *str, const char *pfx, const char **ptr);
function av_stristart(const str: PAnsiChar; const pfx: PAnsiChar; const ptr: ppAnsiChar): int; cdecl; external avutil_dll;
(* *
  * Locate the first case-independent occurrence in the string haystack
  * of the string needle.  A zero-length string needle is considered to
  * match at the start of haystack.
  *
  * This function is a case-insensitive version of the standard strstr().
  *
  * @param haystack string to search in
  * @param needle   string to search for
  * @return         pointer to the located match within haystack
  *                 or a null pointer if no match
*)
// char *av_stristr(const char *haystack, const char *needle);
function av_stristr(const haystack: PAnsiChar; const needle: PAnsiChar): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Locate the first occurrence of the string needle in the string haystack
  * where not more than hay_length characters are searched. A zero-length
  * string needle is considered to match at the start of haystack.
  *
  * This function is a length-limited version of the standard strstr().
  *
  * @param haystack   string to search in
  * @param needle     string to search for
  * @param hay_length length of string to search in
  * @return           pointer to the located match within haystack
  *                   or a null pointer if no match
*)
// char *av_strnstr(const char *haystack, const char *needle, size_t hay_length);
function av_strnstr(const haystack: PAnsiChar; const needle: PAnsiChar; hay_length: size_t): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Copy the string src to dst, but no more than size - 1 bytes, and
  * null-terminate dst.
  *
  * This function is the same as BSD strlcpy().
  *
  * @param dst destination buffer
  * @param src source string
  * @param size size of destination buffer
  * @return the length of src
  *
  * @warning since the return value is the length of src, src absolutely
  * _must_ be a properly 0-terminated string, otherwise this will read beyond
  * the end of the buffer and possibly crash.
*)
// size_t av_strlcpy(char *dst, const char *src, size_t size);
function av_strlcpy(dst: PAnsiChar; const src: PAnsiChar; size: size_t): size_t; cdecl; external avutil_dll;
(* *
  * Append the string src to the string dst, but to a total length of
  * no more than size - 1 bytes, and null-terminate dst.
  *
  * This function is similar to BSD strlcat(), but differs when
  * size <= strlen(dst).
  *
  * @param dst destination buffer
  * @param src source string
  * @param size size of destination buffer
  * @return the total length of src and dst
  *
  * @warning since the return value use the length of src and dst, these
  * absolutely _must_ be a properly 0-terminated strings, otherwise this
  * will read beyond the end of the buffer and possibly crash.
*)
// size_t av_strlcat(char *dst, const char *src, size_t size);
function av_strlcat(dst: PAnsiChar; const src: PAnsiChar; size: size_t): size_t; cdecl; external avutil_dll;
(* *
  * Append output to a string, according to a format. Never write out of
  * the destination buffer, and always put a terminating 0 within
  * the buffer.
  * @param dst destination buffer (string to which the output is
  *  appended)
  * @param size total size of the destination buffer
  * @param fmt printf-compatible format string, specifying how the
  *  following parameters are used
  * @return the length of the string that would have been generated
  *  if enough space had been available
*)
// size_t av_strlcatf(char *dst, size_t size, const char *fmt, ...) av_printf_format(3, 4);

(* *
  * Get the count of continuous non zero chars starting from the beginning.
  *
  * @param len maximum number of characters to check in the string, that
  *            is the maximum value which is returned by the function
*)
// static inline size_t av_strnlen(const char *s, size_t len)
function av_strnlen(const s: PAnsiChar; len: size_t): size_t; inline;

(* *
  * Print arguments following specified format into a large enough auto
  * allocated buffer. It is similar to GNU asprintf().
  * @param fmt printf-compatible format string, specifying how the
  *            following parameters are used.
  * @return the allocated string
  * @note You have to free the string yourself with av_free().
*)
// char *av_asprintf(const char *fmt, ...) av_printf_format(1, 2);

(* *
  * Convert a number to an av_malloced string.
*)
// char *av_d2str(double d);
function av_d2str(d: double): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Unescape the given string until a non escaped terminating char,
  * and return the token corresponding to the unescaped string.
  *
  * The normal \ and ' escaping is supported. Leading and trailing
  * whitespaces are removed, unless they are escaped with '\' or are
  * enclosed between ''.
  *
  * @param buf the buffer to parse, buf will be updated to point to the
  * terminating char
  * @param term a 0-terminated list of terminating chars
  * @return the malloced unescaped string, which must be av_freed by
  * the user, NULL in case of allocation failure
*)
// char *av_get_token(const char **buf, const char *term);
function av_get_token(const buf: ppAnsiChar; const term: PAnsiChar): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Split the string into several tokens which can be accessed by
  * successive calls to av_strtok().
  *
  * A token is defined as a sequence of characters not belonging to the
  * set specified in delim.
  *
  * On the first call to av_strtok(), s should point to the string to
  * parse, and the value of saveptr is ignored. In subsequent calls, s
  * should be NULL, and saveptr should be unchanged since the previous
  * call.
  *
  * This function is similar to strtok_r() defined in POSIX.1.
  *
  * @param s the string to parse, may be NULL
  * @param delim 0-terminated list of token delimiters, must be non-NULL
  * @param saveptr user-provided pointer which points to stored
  * information necessary for av_strtok() to continue scanning the same
  * string. saveptr is updated to point to the next character after the
  * first delimiter found, or to NULL if the string was terminated
  * @return the found token, or NULL when no token is found
*)
// char *av_strtok(char *s, const char *delim, char **saveptr);
function av_strtok(s: PAnsiChar; const delim: PAnsiChar; saveptr: ppAnsiChar): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Locale-independent conversion of ASCII isdigit.
*)
// static inline av_const int av_isdigit(int c)
function av_isdigit(c: int): Boolean; inline;

(* *
  * Locale-independent conversion of ASCII isgraph.
*)
// static inline av_const int av_isgraph(int c)
function av_isgraph(c: int): Boolean; inline;

(* *
  * Locale-independent conversion of ASCII isspace.
*)
// static inline av_const int av_isspace(int c)
function av_isspace(c1: int): Boolean; inline;

(* *
  * Locale-independent conversion of ASCII characters to uppercase.
*)
// static inline av_const int av_toupper(int c)
function av_toupper(c1: int): int; inline;

(* *
  * Locale-independent conversion of ASCII characters to lowercase.
*)
// static inline av_const int av_tolower(int c)
function av_tolower(c1: int): int; inline;

(* *
  * Locale-independent conversion of ASCII isxdigit.
*)
// static inline av_const int av_isxdigit(int c)
function av_isxdigit(c1: int): Boolean; inline;

(* *
  * Locale-independent case-insensitive compare.
  * @note This means only ASCII-range characters are case-insensitive
*)
// int av_strcasecmp(const char *a, const char *b);
function av_strcasecmp(const a: PAnsiChar; const b: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * Locale-independent case-insensitive compare.
  * @note This means only ASCII-range characters are case-insensitive
*)
// int av_strncasecmp(const char *a, const char *b, size_t n);
function av_strncasecmp(const a: PAnsiChar; const b: PAnsiChar; n: size_t): int; cdecl; external avutil_dll;
(* *
  * Locale-independent strings replace.
  * @note This means only ASCII-range characters are replace
*)
// char *av_strireplace(const char *str, const char *from, const char *to);
function av_strireplace(const str: PAnsiChar; const from: PAnsiChar; const _to: PAnsiChar): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Thread safe basename.
  * @param path the path, on DOS both \ and / are considered separators.
  * @return pointer to the basename substring.
*)
// const char *av_basename(const char *path);
function av_basename(const path: PAnsiChar): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Thread safe dirname.
  * @param path the path, on DOS both \ and / are considered separators.
  * @return the path with the separator replaced by the string terminator or ".".
  * @note the function may change the input string.
*)
// const char *av_dirname(char *path);
function av_dirname(path: PAnsiChar): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Match instances of a name in a comma-separated list of names.
  * List entries are checked from the start to the end of the names list,
  * the first match ends further processing. If an entry prefixed with '-'
  * matches, then 0 is returned. The "ALL" list entry is considered to
  * match all names.
  *
  * @param name  Name to look for.
  * @param names List of names.
  * @return 1 on match, 0 otherwise.
*)
// int av_match_name(const char *name, const char *names);
function av_match_name(const name: PAnsiChar; const names: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * Append path component to the existing path.
  * Path separator '/' is placed between when needed.
  * Resulting string have to be freed with av_free().
  * @param path      base path
  * @param component component to be appended
  * @return new path or NULL on error.
*)
// char *av_append_path_component(const char *path, const char *component);
function av_append_path_component(const path: PAnsiChar; const component: PAnsiChar): PAnsiChar; cdecl; external avutil_dll;

type
  AVEscapeMode = ( //
    AV_ESCAPE_MODE_AUTO,
    // < Use auto-selected escaping mode.
    AV_ESCAPE_MODE_BACKSLASH,
    // < Use backslash escaping.
    AV_ESCAPE_MODE_QUOTE
    // < Use single-quote escaping.
    );

const
  (* *
    * Consider spaces special and escape them even in the middle of the
    * string.
    *
    * This is equivalent to adding the whitespace characters to the special
    * characters lists, except it is guaranteed to use the exact same list
    * of whitespace characters as the rest of libavutil.
  *)
  AV_ESCAPE_FLAG_WHITESPACE = (1 shl 0);

  (* *
    * Escape only specified special characters.
    * Without this flag, escape also any characters that may be considered
    * special by av_get_token(), such as the single quote.
  *)
  AV_ESCAPE_FLAG_STRICT = (1 shl 1);

  (* *
    * Escape string in src, and put the escaped string in an allocated
    * string in *dst, which must be freed with av_free().
    *
    * @param dst           pointer where an allocated string is put
    * @param src           string to escape, must be non-NULL
    * @param special_chars string containing the special characters which
    *                      need to be escaped, can be NULL
    * @param mode          escape mode to employ, see AV_ESCAPE_MODE_* macros.
    *                      Any unknown value for mode will be considered equivalent to
    *                      AV_ESCAPE_MODE_BACKSLASH, but this behaviour can change without
    *                      notice.
    * @param flags         flags which control how to escape, see AV_ESCAPE_FLAG_ macros
    * @return the length of the allocated string, or a negative error code in case of error
    * @see av_bprint_escape()
  *)
  // av_warn_unused_result
  // int av_escape(char **dst, const char *src, const char *special_chars,
  // enum AVEscapeMode mode, int flags);
function av_escape(var dst: PAnsiChar; const src: PAnsiChar; const special_chars: PAnsiChar; mode: AVEscapeMode; flags: int): int; cdecl; external avutil_dll;

const
  AV_UTF8_FLAG_ACCEPT_INVALID_BIG_CODES = 1;
  // < accept codepoints over 0x10FFFF
  AV_UTF8_FLAG_ACCEPT_NON_CHARACTERS = 2;
  // < accept non-characters - 0xFFFE and 0xFFFF
  AV_UTF8_FLAG_ACCEPT_SURROGATES = 4;
  // < accept UTF-16 surrogates codes
  AV_UTF8_FLAG_EXCLUDE_XML_INVALID_CONTROL_CODES = 8;
  // < exclude control codes not accepted by XML

  AV_UTF8_FLAG_ACCEPT_ALL = AV_UTF8_FLAG_ACCEPT_INVALID_BIG_CODES or AV_UTF8_FLAG_ACCEPT_NON_CHARACTERS or AV_UTF8_FLAG_ACCEPT_SURROGATES;

  (* *
    * Read and decode a single UTF-8 code point (character) from the
    * buffer in *buf, and update *buf to point to the next byte to
    * decode.
    *
    * In case of an invalid byte sequence, the pointer will be updated to
    * the next byte after the invalid sequence and the function will
    * return an error code.
    *
    * Depending on the specified flags, the function will also fail in
    * case the decoded code point does not belong to a valid range.
    *
    * @note For speed-relevant code a carefully implemented use of
    * GET_UTF8() may be preferred.
    *
    * @param codep   pointer used to return the parsed code in case of success.
    *                The value in *codep is set even in case the range check fails.
    * @param bufp    pointer to the address the first byte of the sequence
    *                to decode, updated by the function to point to the
    *                byte next after the decoded sequence
    * @param buf_end pointer to the end of the buffer, points to the next
    *                byte past the last in the buffer. This is used to
    *                avoid buffer overreads (in case of an unfinished
    *                UTF-8 sequence towards the end of the buffer).
    * @param flags   a collection of AV_UTF8_FLAG_* flags
    * @return >= 0 in case a sequence was successfully read, a negative
    * value in case of invalid sequence
  *)
  // av_warn_unused_result
  // int av_utf8_decode(int32_t *codep, const uint8_t **bufp, const uint8_t *buf_end,
  // unsigned int flags);
function av_utf8_decode(var codep: int32_t; const bufp: ppuint8_t; const buf_end: puint8_t; flags: unsigned_int): int; cdecl; external avutil_dll;
(* *
  * Check if a name is in a list.
  * @returns 0 if not found, or the 1 based index where it has been found in the
  *            list.
*)
// int av_match_list(const char *name, const char *list, char separator);
function av_match_list(const name: PAnsiChar; const list: PAnsiChar; separator: AnsiChar): int; cdecl; external avutil_dll;

(*
  * See libc sscanf manual for more information.
  * Locale-independent sscanf implementation.
*)
// int av_sscanf(const char *string, const char *format, ...);

{$ENDREGION}
{$REGION 'bprint.h'}

const
  (* *
    * Convenience macros for special values for av_bprint_init() size_max
    * parameter.
  *)
  AV_BPRINT_SIZE_UNLIMITED  = ((max_unsigned) - 1);
  AV_BPRINT_SIZE_AUTOMATIC  = 1;
  AV_BPRINT_SIZE_COUNT_ONLY = 0;

  (* *
    * Init a print buffer.
    *
    * @param buf        buffer to init
    * @param size_init  initial size (including the final 0)
    * @param size_max   maximum size;
    *                   0 means do not write anything, just count the length;
    *                   1 is replaced by the maximum value for automatic storage;
    *                   any large value means that the internal buffer will be
    *                   reallocated as needed up to that limit; -1 is converted to
    *                   UINT_MAX, the largest limit possible.
    *                   Check also AV_BPRINT_SIZE_* macros.
  *)
  // void av_bprint_init(AVBPrint *buf, unsigned size_init, unsigned size_max);
procedure av_bprint_init(buf: pAVBPrint; size_init: unsigned; size_max: unsigned); cdecl; external avutil_dll;
(* *
  * Init a print buffer using a pre-existing buffer.
  *
  * The buffer will not be reallocated.
  *
  * @param buf     buffer structure to init
  * @param buffer  byte buffer to use for the string data
  * @param size    size of buffer
*)
// void av_bprint_init_for_buffer(AVBPrint *buf, char *buffer, unsigned size);
procedure av_bprint_init_for_buffer(buf: pAVBPrint; buffer: PAnsiChar; size: unsigned); cdecl; external avutil_dll;
(* *
  * Append a formatted string to a print buffer.
*)
// void av_bprintf(AVBPrint *buf, const char *fmt, ...) av_printf_format(2, 3);

(* *
  * Append a formatted string to a print buffer.
*)
// void av_vbprintf(AVBPrint *buf, const char *fmt, va_list vl_arg);
procedure av_vbprintf(buf: pAVBPrint; const fmt: PAnsiChar; vl_arg: PVA_LIST); cdecl; external avutil_dll;
(* *
  * Append char c n times to a print buffer.
*)
// void av_bprint_chars(AVBPrint *buf, char c, unsigned n);
procedure av_bprint_chars(buf: pAVBPrint; c: AnsiChar; n: unsigned); cdecl; external avutil_dll;
(* *
  * Append data to a print buffer.
  *
  * param buf  bprint buffer to use
  * param data pointer to data
  * param size size of data
*)
// void av_bprint_append_data(AVBPrint *buf, const char *data, unsigned size);
procedure av_bprint_append_data(buf: pAVBPrint; const data: PAnsiChar; size: unsigned); cdecl; external avutil_dll;

type
  ptm = ^tm;

  tm = record
  end;

  (* *
    * Append a formatted date and time to a print buffer.
    *
    * param buf  bprint buffer to use
    * param fmt  date and time format string, see strftime()
    * param tm   broken-down time structure to translate
    *
    * @note due to poor design of the standard strftime function, it may
    * produce poor results if the format string expands to a very long text and
    * the bprint buffer is near the limit stated by the size_max option.
  *)
  // void av_bprint_strftime(AVBPrint *buf, const char *fmt, const struct tm *tm);
procedure av_bprint_strftime(buf: pAVBPrint; const fmt: PAnsiChar; const tm: ptm); cdecl; external avutil_dll;
(* *
  * Allocate bytes in the buffer for external use.
  *
  * @param[in]  buf          buffer structure
  * @param[in]  size         required size
  * @param[out] mem          pointer to the memory area
  * @param[out] actual_size  size of the memory area after allocation;
  *                          can be larger or smaller than size
*)
// void av_bprint_get_buffer(AVBPrint *buf, unsigned size,
// unsigned char **mem, unsigned *actual_size);
procedure av_bprint_get_buffer(buf: pAVBPrint; size: unsigned; var mem: punsigned_char; var actual_size: unsigned); cdecl; external avutil_dll;
(* *
  * Reset the string to "" but keep internal allocated data.
*)
// void av_bprint_clear(AVBPrint *buf);
procedure av_bprint_clear(buf: pAVBPrint); cdecl; external avutil_dll;
(* *
  * Test if the print buffer is complete (not truncated).
  *
  * It may have been truncated due to a memory allocation failure
  * or the size_max limit (compare size and size_max if necessary).
*)
// static inline int av_bprint_is_complete(const AVBPrint *buf)
function av_bprint_is_complete(const buf: pAVBPrint): Boolean; inline;

(* *
  * Finalize a print buffer.
  *
  * The print buffer can no longer be used afterwards,
  * but the len and size fields are still valid.
  *
  * @arg[out] ret_str  if not NULL, used to return a permanent copy of the
  *                    buffer contents, or NULL if memory allocation fails;
  *                    if NULL, the buffer is discarded and freed
  * @return  0 for success or error code (probably AVERROR(ENOMEM))
*)
// int av_bprint_finalize(AVBPrint *buf, char **ret_str);
function av_bprint_finalize(buf: pAVBPrint; var ret_str: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * Escape the content in src and append it to dstbuf.
  *
  * @param dstbuf        already inited destination bprint buffer
  * @param src           string containing the text to escape
  * @param special_chars string containing the special characters which
  *                      need to be escaped, can be NULL
  * @param mode          escape mode to employ, see AV_ESCAPE_MODE_* macros.
  *                      Any unknown value for mode will be considered equivalent to
  *                      AV_ESCAPE_MODE_BACKSLASH, but this behaviour can change without
  *                      notice.
  * @param flags         flags which control how to escape, see AV_ESCAPE_FLAG_* macros
*)
// void av_bprint_escape(AVBPrint *dstbuf, const char *src, const char *special_chars,
// enum AVEscapeMode mode, int flags);
procedure av_bprint_escape(dstbuf: pAVBPrint; const src: PAnsiChar; const special_chars: PAnsiChar; mode: AVEscapeMode; flags: int); cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'display.h'}
(* *
  * @addtogroup lavu_video_display
  * The display transformation matrix specifies an affine transformation that
  * should be applied to video frames for correct presentation. It is compatible
  * with the matrices stored in the ISO/IEC 14496-12 container format.
  *
  * The data is a 3x3 matrix represented as a 9-element array:
  *
  * @code{.unparsed}
  *                                  | a b u |
  *   (a, b, u, c, d, v, x, y, w) -> | c d v |
  *                                  | x y w |
  * @endcode
  *
  * All numbers are stored in native endianness, as 16.16 fixed-point values,
  * except for u, v and w, which are stored as 2.30 fixed-point values.
  *
  * The transformation maps a point (p, q) in the source (pre-transformation)
  * frame to the point (p', q') in the destination (post-transformation) frame as
  * follows:
  *
  * @code{.unparsed}
  *               | a b u |
  *   (p, q, 1) . | c d v | = z * (p', q', 1)
  *               | x y w |
  * @endcode
  *
  * The transformation can also be more explicitly written in components as
  * follows:
  *
  * @code{.unparsed}
  *   p' = (a * p + c * q + x) / z;
  *   q' = (b * p + d * q + y) / z;
  *   z  =  u * p + v * q + w
  * @endcode
*)

type
  Tav_display_matrix = array [0 .. 8] of int32_t;

  (* *
    * Extract the rotation component of the transformation matrix.
    *
    * @param matrix the transformation matrix
    * @return the angle (in degrees) by which the transformation rotates the frame
    *         counterclockwise. The angle will be in range [-180.0, 180.0],
    *         or NaN if the matrix is singular.
    *
    * @note floating point numbers are inherently inexact, so callers are
    *       recommended to round the return value to nearest integer before use.
  *)
  // double av_display_rotation_get(const int32_t matrix[9]);
function av_display_rotation_get(const matrix: Tav_display_matrix): double; cdecl; external avutil_dll;
(* *
  * Initialize a transformation matrix describing a pure counterclockwise
  * rotation by the specified angle (in degrees).
  *
  * @param matrix an allocated transformation matrix (will be fully overwritten
  *               by this function)
  * @param angle rotation angle in degrees.
*)
// void av_display_rotation_set(int32_t matrix[9], double angle);
procedure av_display_rotation_set(matrix: Tav_display_matrix; angle: double); cdecl; external avutil_dll;
(* *
  * Flip the input matrix horizontally and/or vertically.
  *
  * @param matrix an allocated transformation matrix
  * @param hflip whether the matrix should be flipped horizontally
  * @param vflip whether the matrix should be flipped vertically
*)
// void av_display_matrix_flip(int32_t matrix[9], int hflip, int vflip);
procedure av_display_matrix_flip(matrix: Tav_display_matrix; hflip: int; vflip: int); cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'eval.h'}

type
  pAVExpr = ^AVExpr;

  AVExpr = record
  end;

  // double (* const *funcs1)(void *, double)
  Tav_expr_funcs1 = function(p1: Pointer; p2: double): ppDouble; cdecl;
  // double (* const *funcs2)(void *, double, double)
  Tav_expr_funcs2 = function(p1: Pointer; p2: double; p3: double): ppDouble; cdecl;

  (* *
    * Parse and evaluate an expression.
    * Note, this is significantly slower than av_expr_eval().
    *
    * @param res a pointer to a double where is put the result value of
    * the expression, or NAN in case of error
    * @param s expression as a zero terminated string, for example "1+2^3+5*5+sin(2/3)"
    * @param const_names NULL terminated array of zero terminated strings of constant identifiers, for example {"PI", "E", 0}
    * @param const_values a zero terminated array of values for the identifiers from const_names
    * @param func1_names NULL terminated array of zero terminated strings of funcs1 identifiers
    * @param funcs1 NULL terminated array of function pointers for functions which take 1 argument
    * @param func2_names NULL terminated array of zero terminated strings of funcs2 identifiers
    * @param funcs2 NULL terminated array of function pointers for functions which take 2 arguments
    * @param opaque a pointer which will be passed to all functions from funcs1 and funcs2
    * @param log_ctx parent logging context
    * @return >= 0 in case of success, a negative value corresponding to an
    * AVERROR code otherwise
  *)
  // int av_expr_parse_and_eval(double *res, const char *s,
  // const char * const *const_names, const double *const_values,
  // const char * const *func1_names, double (* const *funcs1)(void *, double),
  // const char * const *func2_names, double (* const *funcs2)(void *, double, double),
  // void *opaque, int log_offset, void *log_ctx);

function av_expr_parse_and_eval(var res: double; const s: PAnsiChar; const_names: ppAnsiChar; const const_values: pdouble; func1_names: ppAnsiChar;
  funcs1: Tav_expr_funcs1; func2_names: ppAnsiChar; funcs2: Tav_expr_funcs2; opaque: Pointer; log_offset: int; log_ctx: Pointer): int; cdecl;
  external avutil_dll;
(* *
  * Parse an expression.
  *
  * @param expr a pointer where is put an AVExpr containing the parsed
  * value in case of successful parsing, or NULL otherwise.
  * The pointed to AVExpr must be freed with av_expr_free() by the user
  * when it is not needed anymore.
  * @param s expression as a zero terminated string, for example "1+2^3+5*5+sin(2/3)"
  * @param const_names NULL terminated array of zero terminated strings of constant identifiers, for example {"PI", "E", 0}
  * @param func1_names NULL terminated array of zero terminated strings of funcs1 identifiers
  * @param funcs1 NULL terminated array of function pointers for functions which take 1 argument
  * @param func2_names NULL terminated array of zero terminated strings of funcs2 identifiers
  * @param funcs2 NULL terminated array of function pointers for functions which take 2 arguments
  * @param log_ctx parent logging context
  * @return >= 0 in case of success, a negative value corresponding to an
  * AVERROR code otherwise
*)
// int av_expr_parse(AVExpr **expr, const char *s,
// const char * const *const_names,
// const char * const *func1_names, double (* const *funcs1)(void *, double),
// const char * const *func2_names, double (* const *funcs2)(void *, double, double),
// int log_offset, void *log_ctx);

function av_expr_parse(var expr: pAVExpr; const s: PAnsiChar; const_names: ppAnsiChar; func1_names: ppAnsiChar; funcs1: Tav_expr_funcs1;
  func2_names: ppAnsiChar; funcs2: Tav_expr_funcs2; log_offset: int; log_ctx: Pointer): int; cdecl; external avutil_dll;
(* *
  * Evaluate a previously parsed expression.
  *
  * @param const_values a zero terminated array of values for the identifiers from av_expr_parse() const_names
  * @param opaque a pointer which will be passed to all functions from funcs1 and funcs2
  * @return the value of the expression
*)
// double av_expr_eval(AVExpr *e, const double *const_values, void *opaque);
function av_expr_eval(e: pAVExpr; const const_values: pdouble; opaque: Pointer): double; cdecl; external avutil_dll;
(* *
  * Free a parsed expression previously created with av_expr_parse().
*)
// void av_expr_free(AVExpr *e);
procedure av_expr_free(e: pAVExpr); cdecl; external avutil_dll;
(* *
  * Parse the string in numstr and return its value as a double. If
  * the string is empty, contains only whitespaces, or does not contain
  * an initial substring that has the expected syntax for a
  * floating-point number, no conversion is performed. In this case,
  * returns a value of zero and the value returned in tail is the value
  * of numstr.
  *
  * @param numstr a string representing a number, may contain one of
  * the International System number postfixes, for example 'K', 'M',
  * 'G'. If 'i' is appended after the postfix, powers of 2 are used
  * instead of powers of 10. The 'B' postfix multiplies the value by
  * 8, and can be appended after another postfix or used alone. This
  * allows using for example 'KB', 'MiB', 'G' and 'B' as postfix.
  * @param tail if non-NULL puts here the pointer to the char next
  * after the last parsed character
*)
// double av_strtod(const char *numstr, char **tail);
function av_strtod(const numstr: PAnsiChar; var tail: PAnsiChar): double; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'fifo.h'}

type
  pAVFifoBuffer = ^AVFifoBuffer;

  AVFifoBuffer = record
    buffer: puint8_t;
    rptr, wptr, _end: puint8_t;
    rndx, wndx: uint32_t;
  end;

  (* *
    * Initialize an AVFifoBuffer.
    * @param size of FIFO
    * @return AVFifoBuffer or NULL in case of memory allocation failure
  *)
  // AVFifoBuffer *av_fifo_alloc(unsigned int size);
function av_fifo_alloc(size: unsigned_int): pAVFifoBuffer; cdecl; external avutil_dll;
(* *
  * Initialize an AVFifoBuffer.
  * @param nmemb number of elements
  * @param size  size of the single element
  * @return AVFifoBuffer or NULL in case of memory allocation failure
*)
// AVFifoBuffer *av_fifo_alloc_array(size_t nmemb, size_t size);
function av_fifo_alloc_array(nmemb: size_t; size: size_t): pAVFifoBuffer; cdecl; external avutil_dll;
(* *
  * Free an AVFifoBuffer.
  * @param f AVFifoBuffer to free
*)
// void av_fifo_free(AVFifoBuffer *f);
procedure av_fifo_free(f: pAVFifoBuffer); cdecl; external avutil_dll;
(* *
  * Free an AVFifoBuffer and reset pointer to NULL.
  * @param f AVFifoBuffer to free
*)
// void av_fifo_freep(AVFifoBuffer **f);
procedure av_fifo_freep(var f: pAVFifoBuffer); cdecl; external avutil_dll;
(* *
  * Reset the AVFifoBuffer to the state right after av_fifo_alloc, in particular it is emptied.
  * @param f AVFifoBuffer to reset
*)
// void av_fifo_reset(AVFifoBuffer *f);
procedure av_fifo_reset(f: pAVFifoBuffer); cdecl; external avutil_dll;
(* *
  * Return the amount of data in bytes in the AVFifoBuffer, that is the
  * amount of data you can read from it.
  * @param f AVFifoBuffer to read from
  * @return size
*)
// int av_fifo_size(const AVFifoBuffer *f);
function av_fifo_size(const f: pAVFifoBuffer): int; cdecl; external avutil_dll;
(* *
  * Return the amount of space in bytes in the AVFifoBuffer, that is the
  * amount of data you can write into it.
  * @param f AVFifoBuffer to write into
  * @return size
*)
// int av_fifo_space(const AVFifoBuffer *f);
function av_fifo_space(const f: pAVFifoBuffer): int; cdecl; external avutil_dll;

(* *
  * Feed data at specific position from an AVFifoBuffer to a user-supplied callback.
  * Similar as av_fifo_gereric_read but without discarding data.
  * @param f AVFifoBuffer to read from
  * @param offset offset from current read position
  * @param buf_size number of bytes to read
  * @param func generic read function
  * @param dest data destination
*)
// int av_fifo_generic_peek_at(AVFifoBuffer *f, void *dest, int offset, int buf_size, void (*func)(void*, void*, int));
type
  Tav_fifo_proc = procedure(p1: Pointer; p2: Pointer; p3: int); cdecl;
  Tav_fifo_func = function(p1: Pointer; p2: Pointer; p3: int): int; cdecl;

function av_fifo_generic_peek_at(f: pAVFifoBuffer; dest: Pointer; offset: int; buf_size: int; func: Tav_fifo_proc): int; cdecl; external avutil_dll;
(* *
  * Feed data from an AVFifoBuffer to a user-supplied callback.
  * Similar as av_fifo_gereric_read but without discarding data.
  * @param f AVFifoBuffer to read from
  * @param buf_size number of bytes to read
  * @param func generic read function
  * @param dest data destination
*)
// int av_fifo_generic_peek(AVFifoBuffer *f, void *dest, int buf_size, void (*func)(void*, void*, int));
function av_fifo_generic_peek(f: pAVFifoBuffer; dest: Pointer; buf_size: int; func: Tav_fifo_proc): int; cdecl; external avutil_dll;
(* *
  * Feed data from an AVFifoBuffer to a user-supplied callback.
  * @param f AVFifoBuffer to read from
  * @param buf_size number of bytes to read
  * @param func generic read function
  * @param dest data destination
*)
// int av_fifo_generic_read(AVFifoBuffer *f, void *dest, int buf_size, void (*func)(void*, void*, int));
function av_fifo_generic_read(f: pAVFifoBuffer; dest: Pointer; buf_size: int; func: Tav_fifo_proc): int; cdecl; external avutil_dll;
(* *
  * Feed data from a user-supplied callback to an AVFifoBuffer.
  * @param f AVFifoBuffer to write to
  * @param src data source; non-const since it may be used as a
  * modifiable context by the function defined in func
  * @param size number of bytes to write
  * @param func generic write function; the first parameter is src,
  * the second is dest_buf, the third is dest_buf_size.
  * func must return the number of bytes written to dest_buf, or <= 0 to
  * indicate no more data available to write.
  * If func is NULL, src is interpreted as a simple byte array for source data.
  * @return the number of bytes written to the FIFO
*)
// int av_fifo_generic_write(AVFifoBuffer *f, void *src, int size, int (*func)(void*, void*, int));
function av_fifo_generic_write(f: pAVFifoBuffer; src: Pointer; size: int; func: Tav_fifo_func): int; cdecl; external avutil_dll;
(* *
  * Resize an AVFifoBuffer.
  * In case of reallocation failure, the old FIFO is kept unchanged.
  *
  * @param f AVFifoBuffer to resize
  * @param size new AVFifoBuffer size in bytes
  * @return <0 for failure, >=0 otherwise
*)
// int av_fifo_realloc2(AVFifoBuffer *f, unsigned int size);
function av_fifo_realloc2(f: pAVFifoBuffer; size: unsigned_int): int; cdecl; external avutil_dll;
(* *
  * Enlarge an AVFifoBuffer.
  * In case of reallocation failure, the old FIFO is kept unchanged.
  * The new fifo size may be larger than the requested size.
  *
  * @param f AVFifoBuffer to resize
  * @param additional_space the amount of space in bytes to allocate in addition to av_fifo_size()
  * @return <0 for failure, >=0 otherwise
*)
// int av_fifo_grow(AVFifoBuffer *f, unsigned int additional_space);
function av_fifo_grow(f: pAVFifoBuffer; additional_space: unsigned_int): int; cdecl; external avutil_dll;
(* *
  * Read and discard the specified amount of data from an AVFifoBuffer.
  * @param f AVFifoBuffer to read from
  * @param size amount of data to read in bytes
*)
// void av_fifo_drain(AVFifoBuffer *f, int size);
procedure av_fifo_drain(f: pAVFifoBuffer; size: int); cdecl; external avutil_dll;
(* *
  * Return a pointer to the data stored in a FIFO buffer at a certain offset.
  * The FIFO buffer is not modified.
  *
  * @param f    AVFifoBuffer to peek at, f must be non-NULL
  * @param offs an offset in bytes, its absolute value must be less
  *             than the used buffer size or the returned pointer will
  *             point outside to the buffer data.
  *             The used buffer size can be checked with av_fifo_size().
*)
// static inline uint8_t *av_fifo_peek2(const AVFifoBuffer *f, int offs)
function av_fifo_peek2(const f: pAVFifoBuffer; offs: int): puint8_t; inline;

{$ENDREGION}
{$REGION 'hwcontext.h'}

type
  AVHWDeviceType = (AV_HWDEVICE_TYPE_NONE, AV_HWDEVICE_TYPE_VDPAU, AV_HWDEVICE_TYPE_CUDA, AV_HWDEVICE_TYPE_VAAPI, AV_HWDEVICE_TYPE_DXVA2, AV_HWDEVICE_TYPE_QSV,
    AV_HWDEVICE_TYPE_VIDEOTOOLBOX, AV_HWDEVICE_TYPE_D3D11VA, AV_HWDEVICE_TYPE_DRM, AV_HWDEVICE_TYPE_OPENCL, AV_HWDEVICE_TYPE_MEDIACODEC);

  pAVHWDeviceInternal = ^AVHWDeviceInternal;

  AVHWDeviceInternal = record
  end;

  (* *
    * This struct aggregates all the (hardware/vendor-specific) "high-level" state,
    * i.e. state that is not tied to a concrete processing configuration.
    * E.g., in an API that supports hardware-accelerated encoding and decoding,
    * this struct will (if possible) wrap the state that is common to both encoding
    * and decoding and from which specific instances of encoders or decoders can be
    * derived.
    *
    * This struct is reference-counted with the AVBuffer mechanism. The
    * av_hwdevice_ctx_alloc() constructor yields a reference, whose data field
    * points to the actual AVHWDeviceContext. Further objects derived from
    * AVHWDeviceContext (such as AVHWFramesContext, describing a frame pool with
    * specific properties) will hold an internal reference to it. After all the
    * references are released, the AVHWDeviceContext itself will be freed,
    * optionally invoking a user-specified callback for uninitializing the hardware
    * state.
  *)
  pAVHWDeviceContext = ^AVHWDeviceContext;

  AVHWDeviceContext = record
    (* *
      * A class for logging. Set by av_hwdevice_ctx_alloc().
    *)
    av_class: pAVClass;

    (* *
      * Private data used internally by libavutil. Must not be accessed in any
      * way by the caller.
    *)
    internal: pAVHWDeviceInternal;

    (* *
      * This field identifies the underlying API used for hardware access.
      *
      * This field is set when this struct is allocated and never changed
      * afterwards.
    *)
    _type: AVHWDeviceType;

    (* *
      * The format-specific data, allocated and freed by libavutil along with
      * this context.
      *
      * Should be cast by the user to the format-specific context defined in the
      * corresponding header (hwcontext_*.h) and filled as described in the
      * documentation before calling av_hwdevice_ctx_init().
      *
      * After calling av_hwdevice_ctx_init() this struct should not be modified
      * by the caller.
    *)
    hwctx: Pointer;

    (* *
      * This field may be set by the caller before calling av_hwdevice_ctx_init().
      *
      * If non-NULL, this callback will be called when the last reference to
      * this context is unreferenced, immediately before it is freed.
      *
      * @note when other objects (e.g an AVHWFramesContext) are derived from this
      *       struct, this callback will be invoked after all such child objects
      *       are fully uninitialized and their respective destructors invoked.
    *)
    // void (*free)(struct AVHWDeviceContext *ctx);
    free: procedure(ctx: pAVHWDeviceContext); cdecl;
    (* *
      * Arbitrary user data, to be used e.g. by the free() callback.
    *)
    user_opaque: Pointer;
  end;

  pAVHWFramesInternal = ^AVHWFramesInternal;

  AVHWFramesInternal = record
  end;

  (* *
    * This struct describes a set or pool of "hardware" frames (i.e. those with
    * data not located in normal system memory). All the frames in the pool are
    * assumed to be allocated in the same way and interchangeable.
    *
    * This struct is reference-counted with the AVBuffer mechanism and tied to a
    * given AVHWDeviceContext instance. The av_hwframe_ctx_alloc() constructor
    * yields a reference, whose data field points to the actual AVHWFramesContext
    * struct.
  *)
  pAVHWFramesContext = ^AVHWFramesContext;

  AVHWFramesContext = record
    (* *
      * A class for logging.
    *)
    av_class: pAVClass;

    (* *
      * Private data used internally by libavutil. Must not be accessed in any
      * way by the caller.
    *)
    internal: pAVHWFramesInternal;

    (* *
      * A reference to the parent AVHWDeviceContext. This reference is owned and
      * managed by the enclosing AVHWFramesContext, but the caller may derive
      * additional references from it.
    *)
    device_ref: pAVBufferRef;

    (* *
      * The parent AVHWDeviceContext. This is simply a pointer to
      * device_ref->data provided for convenience.
      *
      * Set by libavutil in av_hwframe_ctx_init().
    *)
    device_ctx: pAVHWDeviceContext;

    (* *
      * The format-specific data, allocated and freed automatically along with
      * this context.
      *
      * Should be cast by the user to the format-specific context defined in the
      * corresponding header (hwframe_*.h) and filled as described in the
      * documentation before calling av_hwframe_ctx_init().
      *
      * After any frames using this context are created, the contents of this
      * struct should not be modified by the caller.
    *)
    hwctx: Pointer;

    (* *
      * This field may be set by the caller before calling av_hwframe_ctx_init().
      *
      * If non-NULL, this callback will be called when the last reference to
      * this context is unreferenced, immediately before it is freed.
    *)
    // void (*free)(struct AVHWFramesContext *ctx);
    free: procedure(ctx: pAVHWFramesContext); cdecl;
    (* *
      * Arbitrary user data, to be used e.g. by the free() callback.
    *)
    user_opaque: Pointer;

    (* *
      * A pool from which the frames are allocated by av_hwframe_get_buffer().
      * This field may be set by the caller before calling av_hwframe_ctx_init().
      * The buffers returned by calling av_buffer_pool_get() on this pool must
      * have the properties described in the documentation in the corresponding hw
      * type's header (hwcontext_*.h). The pool will be freed strictly before
      * this struct's free() callback is invoked.
      *
      * This field may be NULL, then libavutil will attempt to allocate a pool
      * internally. Note that certain device types enforce pools allocated at
      * fixed size (frame count), which cannot be extended dynamically. In such a
      * case, initial_pool_size must be set appropriately.
    *)
    pool: pAVBufferPool;

    (* *
      * Initial size of the frame pool. If a device type does not support
      * dynamically resizing the pool, then this is also the maximum pool size.
      *
      * May be set by the caller before calling av_hwframe_ctx_init(). Must be
      * set if pool is NULL and the device type does not support dynamic pools.
    *)
    initial_pool_size: int;

    (* *
      * The pixel format identifying the underlying HW surface type.
      *
      * Must be a hwaccel format, i.e. the corresponding descriptor must have the
      * AV_PIX_FMT_FLAG_HWACCEL flag set.
      *
      * Must be set by the user before calling av_hwframe_ctx_init().
    *)
    format: AVPixelFormat;

    (* *
      * The pixel format identifying the actual data layout of the hardware
      * frames.
      *
      * Must be set by the caller before calling av_hwframe_ctx_init().
      *
      * @note when the underlying API does not provide the exact data layout, but
      * only the colorspace/bit depth, this field should be set to the fully
      * planar version of that format (e.g. for 8-bit 420 YUV it should be
      * AV_PIX_FMT_YUV420P, not AV_PIX_FMT_NV12 or anything else).
    *)
    sw_format: AVPixelFormat;

    (* *
      * The allocated dimensions of the frames in this pool.
      *
      * Must be set by the user before calling av_hwframe_ctx_init().
    *)
    width, height: int;
  end;

  (* *
    * Look up an AVHWDeviceType by name.
    *
    * @param name String name of the device type (case-insensitive).
    * @return The type from enum AVHWDeviceType, or AV_HWDEVICE_TYPE_NONE if
    *         not found.
  *)
  // enum AVHWDeviceType av_hwdevice_find_type_by_name(const char *name);
function av_hwdevice_find_type_by_name(const name: PAnsiChar): AVHWDeviceType; cdecl; external avutil_dll;
(* * Get the string name of an AVHWDeviceType.
  *
  * @param type Type from enum AVHWDeviceType.
  * @return Pointer to a static string containing the name, or NULL if the type
  *         is not valid.
*)
// const char *av_hwdevice_get_type_name(enum AVHWDeviceType type);
function av_hwdevice_get_type_name(_type: AVHWDeviceType): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Iterate over supported device types.
  *
  * @param type AV_HWDEVICE_TYPE_NONE initially, then the previous type
  *             returned by this function in subsequent iterations.
  * @return The next usable device type from enum AVHWDeviceType, or
  *         AV_HWDEVICE_TYPE_NONE if there are no more.
*)
// enum AVHWDeviceType av_hwdevice_iterate_types(enum AVHWDeviceType prev);
function av_hwdevice_iterate_types(prev: AVHWDeviceType): AVHWDeviceType; cdecl; external avutil_dll;
(* *
  * Allocate an AVHWDeviceContext for a given hardware type.
  *
  * @param type the type of the hardware device to allocate.
  * @return a reference to the newly created AVHWDeviceContext on success or NULL
  *         on failure.
*)
// AVBufferRef *av_hwdevice_ctx_alloc(enum AVHWDeviceType type);
function av_hwdevice_ctx_alloc(_type: AVHWDeviceType): pAVBufferRef; cdecl; external avutil_dll;
(* *
  * Finalize the device context before use. This function must be called after
  * the context is filled with all the required information and before it is
  * used in any way.
  *
  * @param ref a reference to the AVHWDeviceContext
  * @return 0 on success, a negative AVERROR code on failure
*)
// int av_hwdevice_ctx_init(AVBufferRef *ref);
function av_hwdevice_ctx_init(ref: pAVBufferRef): int; cdecl; external avutil_dll;
(* *
  * Open a device of the specified type and create an AVHWDeviceContext for it.
  *
  * This is a convenience function intended to cover the simple cases. Callers
  * who need to fine-tune device creation/management should open the device
  * manually and then wrap it in an AVHWDeviceContext using
  * av_hwdevice_ctx_alloc()/av_hwdevice_ctx_init().
  *
  * The returned context is already initialized and ready for use, the caller
  * should not call av_hwdevice_ctx_init() on it. The user_opaque/free fields of
  * the created AVHWDeviceContext are set by this function and should not be
  * touched by the caller.
  *
  * @param device_ctx On success, a reference to the newly-created device context
  *                   will be written here. The reference is owned by the caller
  *                   and must be released with av_buffer_unref() when no longer
  *                   needed. On failure, NULL will be written to this pointer.
  * @param type The type of the device to create.
  * @param device A type-specific string identifying the device to open.
  * @param opts A dictionary of additional (type-specific) options to use in
  *             opening the device. The dictionary remains owned by the caller.
  * @param flags currently unused
  *
  * @return 0 on success, a negative AVERROR code on failure.
*)
// int av_hwdevice_ctx_create(AVBufferRef **device_ctx, enum AVHWDeviceType type,
// const char *device, AVDictionary *opts, int flags);
function av_hwdevice_ctx_create(var device_ctx: pAVBufferRef; _type: AVHWDeviceType; const device: PAnsiChar; opts: pAVDictionary; flags: int): int; cdecl;
  external avutil_dll;
(* *
  * Create a new device of the specified type from an existing device.
  *
  * If the source device is a device of the target type or was originally
  * derived from such a device (possibly through one or more intermediate
  * devices of other types), then this will return a reference to the
  * existing device of the same type as is requested.
  *
  * Otherwise, it will attempt to derive a new device from the given source
  * device.  If direct derivation to the new type is not implemented, it will
  * attempt the same derivation from each ancestor of the source device in
  * turn looking for an implemented derivation method.
  *
  * @param dst_ctx On success, a reference to the newly-created
  *                AVHWDeviceContext.
  * @param type    The type of the new device to create.
  * @param src_ctx A reference to an existing AVHWDeviceContext which will be
  *                used to create the new device.
  * @param flags   Currently unused; should be set to zero.
  * @return        Zero on success, a negative AVERROR code on failure.
*)
// int av_hwdevice_ctx_create_derived(AVBufferRef **dst_ctx,
// enum AVHWDeviceType type,
// AVBufferRef *src_ctx, int flags);
function av_hwdevice_ctx_create_derived(var dst_ctx: pAVBufferRef; _type: AVHWDeviceType; src_ctx: pAVBufferRef; flags: int): int; cdecl; external avutil_dll;
(* *
  * Allocate an AVHWFramesContext tied to a given device context.
  *
  * @param device_ctx a reference to a AVHWDeviceContext. This function will make
  *                   a new reference for internal use, the one passed to the
  *                   function remains owned by the caller.
  * @return a reference to the newly created AVHWFramesContext on success or NULL
  *         on failure.
*)
// AVBufferRef *av_hwframe_ctx_alloc(AVBufferRef *device_ctx);
function av_hwframe_ctx_alloc(device_ctx: pAVBufferRef): pAVBufferRef; cdecl; external avutil_dll;
(* *
  * Finalize the context before use. This function must be called after the
  * context is filled with all the required information and before it is attached
  * to any frames.
  *
  * @param ref a reference to the AVHWFramesContext
  * @return 0 on success, a negative AVERROR code on failure
*)
// int av_hwframe_ctx_init(AVBufferRef *ref);
function av_hwframe_ctx_init(ref: pAVBufferRef): int; cdecl; external avutil_dll;
(* *
  * Allocate a new frame attached to the given AVHWFramesContext.
  *
  * @param hwframe_ctx a reference to an AVHWFramesContext
  * @param frame an empty (freshly allocated or unreffed) frame to be filled with
  *              newly allocated buffers.
  * @param flags currently unused, should be set to zero
  * @return 0 on success, a negative AVERROR code on failure
*)
// int av_hwframe_get_buffer(AVBufferRef *hwframe_ctx, AVFrame *frame, int flags);
function av_hwframe_get_buffer(hwframe_ctx: pAVBufferRef; frame: pAVFrame; flags: int): int; cdecl; external avutil_dll;
(* *
  * Copy data to or from a hw surface. At least one of dst/src must have an
  * AVHWFramesContext attached.
  *
  * If src has an AVHWFramesContext attached, then the format of dst (if set)
  * must use one of the formats returned by av_hwframe_transfer_get_formats(src,
  * AV_HWFRAME_TRANSFER_DIRECTION_FROM).
  * If dst has an AVHWFramesContext attached, then the format of src must use one
  * of the formats returned by av_hwframe_transfer_get_formats(dst,
  * AV_HWFRAME_TRANSFER_DIRECTION_TO)
  *
  * dst may be "clean" (i.e. with data/buf pointers unset), in which case the
  * data buffers will be allocated by this function using av_frame_get_buffer().
  * If dst->format is set, then this format will be used, otherwise (when
  * dst->format is AV_PIX_FMT_NONE) the first acceptable format will be chosen.
  *
  * The two frames must have matching allocated dimensions (i.e. equal to
  * AVHWFramesContext.width/height), since not all device types support
  * transferring a sub-rectangle of the whole surface. The display dimensions
  * (i.e. AVFrame.width/height) may be smaller than the allocated dimensions, but
  * also have to be equal for both frames. When the display dimensions are
  * smaller than the allocated dimensions, the content of the padding in the
  * destination frame is unspecified.
  *
  * @param dst the destination frame. dst is not touched on failure.
  * @param src the source frame.
  * @param flags currently unused, should be set to zero
  * @return 0 on success, a negative AVERROR error code on failure.
*)
// int av_hwframe_transfer_data(AVFrame *dst, const AVFrame *src, int flags);
function av_hwframe_transfer_data(dst: pAVFrame; const src: pAVFrame; flags: int): int; cdecl; external avutil_dll;

type
  AVHWFrameTransferDirection = (
    (* *
      * Transfer the data from the queried hw frame.
    *)
    AV_HWFRAME_TRANSFER_DIRECTION_FROM,
    (* *
      * Transfer the data to the queried hw frame.
    *)
    AV_HWFRAME_TRANSFER_DIRECTION_TO);

  (* *
    * Get a list of possible source or target formats usable in
    * av_hwframe_transfer_data().
    *
    * @param hwframe_ctx the frame context to obtain the information for
    * @param dir the direction of the transfer
    * @param formats the pointer to the output format list will be written here.
    *                The list is terminated with AV_PIX_FMT_NONE and must be freed
    *                by the caller when no longer needed using av_free().
    *                If this function returns successfully, the format list will
    *                have at least one item (not counting the terminator).
    *                On failure, the contents of this pointer are unspecified.
    * @param flags currently unused, should be set to zero
    * @return 0 on success, a negative AVERROR code on failure.
  *)
  // int av_hwframe_transfer_get_formats(AVBufferRef *hwframe_ctx,
  // enum AVHWFrameTransferDirection dir,
  // enum AVPixelFormat **formats, int flags);
function av_hwframe_transfer_get_formats(hwframe_ctx: pAVBufferRef; dir: AVHWFrameTransferDirection; var formats: pAVPixelFormat; flags: int): int; cdecl;
  external avutil_dll;

type
  (* *
    * This struct describes the constraints on hardware frames attached to
    * a given device with a hardware-specific configuration.  This is returned
    * by av_hwdevice_get_hwframe_constraints() and must be freed by
    * av_hwframe_constraints_free() after use.
  *)
  pAVHWFramesConstraints = ^AVHWFramesConstraints;

  AVHWFramesConstraints = record
    (* *
      * A list of possible values for format in the hw_frames_ctx,
      * terminated by AV_PIX_FMT_NONE.  This member will always be filled.
    *)
    valid_hw_formats: pAVPixelFormat;

    (* *
      * A list of possible values for sw_format in the hw_frames_ctx,
      * terminated by AV_PIX_FMT_NONE.  Can be NULL if this information is
      * not known.
    *)
    valid_sw_formats: pAVPixelFormat;

    (* *
      * The minimum size of frames in this hw_frames_ctx.
      * (Zero if not known.)
    *)
    min_width: int;
    min_height: int;

    (* *
      * The maximum size of frames in this hw_frames_ctx.
      * (INT_MAX if not known / no limit.)
    *)
    max_width: int;
    max_height: int;
  end;

  (* *
    * Allocate a HW-specific configuration structure for a given HW device.
    * After use, the user must free all members as required by the specific
    * hardware structure being used, then free the structure itself with
    * av_free().
    *
    * @param device_ctx a reference to the associated AVHWDeviceContext.
    * @return The newly created HW-specific configuration structure on
    *         success or NULL on failure.
  *)
  // void *av_hwdevice_hwconfig_alloc(AVBufferRef *device_ctx);
function av_hwdevice_hwconfig_alloc(device_ctx: pAVBufferRef): Pointer; cdecl; external avutil_dll;
(* *
  * Get the constraints on HW frames given a device and the HW-specific
  * configuration to be used with that device.  If no HW-specific
  * configuration is provided, returns the maximum possible capabilities
  * of the device.
  *
  * @param ref a reference to the associated AVHWDeviceContext.
  * @param hwconfig a filled HW-specific configuration structure, or NULL
  *        to return the maximum possible capabilities of the device.
  * @return AVHWFramesConstraints structure describing the constraints
  *         on the device, or NULL if not available.
*)
// AVHWFramesConstraints *av_hwdevice_get_hwframe_constraints(AVBufferRef *ref,const void *hwconfig);
function av_hwdevice_get_hwframe_constraints(ref: pAVBufferRef; const hwconfig: Pointer): pAVHWFramesConstraints; cdecl; external avutil_dll;
(* *
  * Free an AVHWFrameConstraints structure.
  *
  * @param constraints The (filled or unfilled) AVHWFrameConstraints structure.
*)
// void av_hwframe_constraints_free(AVHWFramesConstraints **constraints);
procedure av_hwframe_constraints_free(var constraints: pAVHWFramesConstraints); cdecl; external avutil_dll;

const
  (* *
    * Flags to apply to frame mappings.
  *)

  (* *
    * The mapping must be readable.
  *)
  AV_HWFRAME_MAP_READ = 1 shl 0;
  (* *
    * The mapping must be writeable.
  *)
  AV_HWFRAME_MAP_WRITE = 1 shl 1;
  (* *
    * The mapped frame will be overwritten completely in subsequent
    * operations, so the current frame data need not be loaded.  Any values
    * which are not overwritten are unspecified.
  *)
  AV_HWFRAME_MAP_OVERWRITE = 1 shl 2;
  (* *
    * The mapping must be direct.  That is, there must not be any copying in
    * the map or unmap steps.  Note that performance of direct mappings may
    * be much lower than normal memory.
  *)
  AV_HWFRAME_MAP_DIRECT = 1 shl 3;

  (* *
    * Map a hardware frame.
    *
    * This has a number of different possible effects, depending on the format
    * and origin of the src and dst frames.  On input, src should be a usable
    * frame with valid buffers and dst should be blank (typically as just created
    * by av_frame_alloc()).  src should have an associated hwframe context, and
    * dst may optionally have a format and associated hwframe context.
    *
    * If src was created by mapping a frame from the hwframe context of dst,
    * then this function undoes the mapping - dst is replaced by a reference to
    * the frame that src was originally mapped from.
    *
    * If both src and dst have an associated hwframe context, then this function
    * attempts to map the src frame from its hardware context to that of dst and
    * then fill dst with appropriate data to be usable there.  This will only be
    * possible if the hwframe contexts and associated devices are compatible -
    * given compatible devices, av_hwframe_ctx_create_derived() can be used to
    * create a hwframe context for dst in which mapping should be possible.
    *
    * If src has a hwframe context but dst does not, then the src frame is
    * mapped to normal memory and should thereafter be usable as a normal frame.
    * If the format is set on dst, then the mapping will attempt to create dst
    * with that format and fail if it is not possible.  If format is unset (is
    * AV_PIX_FMT_NONE) then dst will be mapped with whatever the most appropriate
    * format to use is (probably the sw_format of the src hwframe context).
    *
    * A return value of AVERROR(ENOSYS) indicates that the mapping is not
    * possible with the given arguments and hwframe setup, while other return
    * values indicate that it failed somehow.
    *
    * @param dst Destination frame, to contain the mapping.
    * @param src Source frame, to be mapped.
    * @param flags Some combination of AV_HWFRAME_MAP_* flags.
    * @return Zero on success, negative AVERROR code on failure.
  *)
  // int av_hwframe_map(AVFrame *dst, const AVFrame *src, int flags);
function av_hwframe_map(dst: pAVFrame; const src: pAVFrame; flags: int): int; cdecl; external avutil_dll;
(* *
  * Create and initialise an AVHWFramesContext as a mapping of another existing
  * AVHWFramesContext on a different device.
  *
  * av_hwframe_ctx_init() should not be called after this.
  *
  * @param derived_frame_ctx  On success, a reference to the newly created
  *                           AVHWFramesContext.
  * @param derived_device_ctx A reference to the device to create the new
  *                           AVHWFramesContext on.
  * @param source_frame_ctx   A reference to an existing AVHWFramesContext
  *                           which will be mapped to the derived context.
  * @param flags  Some combination of AV_HWFRAME_MAP_* flags, defining the
  *               mapping parameters to apply to frames which are allocated
  *               in the derived device.
  * @return       Zero on success, negative AVERROR code on failure.
*)
// int av_hwframe_ctx_create_derived(AVBufferRef **derived_frame_ctx,
// enum AVPixelFormat format,
// AVBufferRef *derived_device_ctx,
// AVBufferRef *source_frame_ctx,
// int flags);
function av_hwframe_ctx_create_derived(var derived_frame_ctx: pAVBufferRef; format: AVPixelFormat; derived_device_ctx: pAVBufferRef;
  source_frame_ctx: pAVBufferRef; flags: int): int; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'hwcontext_mediacodec.h'}

(* *
  * MediaCodec details.
  *
  * Allocated as AVHWDeviceContext.hwctx
*)
type
  pAVMediaCodecDeviceContext = ^AVMediaCodecDeviceContext;

  AVMediaCodecDeviceContext = record
    (* *
      * android/view/Surface handle, to be filled by the user.
      *
      * This is the default surface used by decoders on this device.
    *)
    surface: Pointer;
  end;
{$ENDREGION}
{$REGION 'hwcontext_drm.h'}
  (* *
    * @file
    * API-specific header for AV_HWDEVICE_TYPE_DRM.
    *
    * Internal frame allocation is not currently supported - all frames
    * must be allocated by the user.  Thus AVHWFramesContext is always
    * NULL, though this may change if support for frame allocation is
    * added in future.
  *)

const
  (* *
    * The maximum number of layers/planes in a DRM frame.
  *)
  AV_DRM_MAX_PLANES = 4;

  (* *
    * DRM object descriptor.
    *
    * Describes a single DRM object, addressing it as a PRIME file
    * descriptor.
  *)
type
  pAVDRMObjectDescriptor = ^AVDRMObjectDescriptor;

  AVDRMObjectDescriptor = record
    (* *
      * DRM PRIME fd for the object.
    *)
    fd: int;
    (* *
      * Total size of the object.
      *
      * (This includes any parts not which do not contain image data.)
    *)
    size: size_t;
    (* *
      * Format modifier applied to the object (DRM_FORMAT_MOD_* ).
      *
      * If the format modifier is unknown then this should be set to
      * DRM_FORMAT_MOD_INVALID.
    *)
    format_modifier: uint64_t;
  end;

  (* *
    * DRM plane descriptor.
    *
    * Describes a single plane of a layer, which is contained within
    * a single object.
  *)
  pAVDRMPlaneDescriptor = ^AVDRMPlaneDescriptor;

  AVDRMPlaneDescriptor = record
    (* *
      * Index of the object containing this plane in the objects
      * array of the enclosing frame descriptor.
    *)
    object_index: int;
    (* *
      * Offset within that object of this plane.
    *)
    offset: ptrdiff_t;
    (* *
      * Pitch (linesize) of this plane.
    *)
    pitch: ptrdiff_t;
  end;

  (* *
    * DRM layer descriptor.
    *
    * Describes a single layer within a frame.  This has the structure
    * defined by its format, and will contain one or more planes.
  *)
  pAVDRMLayerDescriptor = ^AVDRMLayerDescriptor;

  AVDRMLayerDescriptor = record
    (* *
      * Format of the layer (DRM_FORMAT_* ).
    *)
    format: uint32_t;
    (* *
      * Number of planes in the layer.
      *
      * This must match the number of planes required by format.
    *)
    nb_planes: int;
    (* *
      * Array of planes in this layer.
    *)
    planes: array [0 .. AV_DRM_MAX_PLANES - 1] of AVDRMPlaneDescriptor;
  end;

  (* *
    * DRM frame descriptor.
    *
    * This is used as the data pointer for AV_PIX_FMT_DRM_PRIME frames.
    * It is also used by user-allocated frame pools - allocating in
    * AVHWFramesContext.pool must return AVBufferRefs which contain
    * an object of this type.
    *
    * The fields of this structure should be set such it can be
    * imported directly by EGL using the EGL_EXT_image_dma_buf_import
    * and EGL_EXT_image_dma_buf_import_modifiers extensions.
    * (Note that the exact layout of a particular format may vary between
    * platforms - we only specify that the same platform should be able
    * to import it.)
    *
    * The total number of planes must not exceed AV_DRM_MAX_PLANES, and
    * the order of the planes by increasing layer index followed by
    * increasing plane index must be the same as the order which would
    * be used for the data pointers in the equivalent software format.
  *)
  pAVDRMFrameDescriptor = ^AVDRMFrameDescriptor;

  AVDRMFrameDescriptor = record
    (* *
      * Number of DRM objects making up this frame.
    *)
    nb_objects: int;
    (* *
      * Array of objects making up the frame.
    *)
    objects: array [0 .. AV_DRM_MAX_PLANES - 1] of AVDRMObjectDescriptor;
    (* *
      * Number of layers in the frame.
    *)
    nb_layers: int;
    (* *
      * Array of layers in the frame.
    *)
    layers: array [0 .. AV_DRM_MAX_PLANES - 1] of AVDRMLayerDescriptor;
  end;

  (* *
    * DRM device.
    *
    * Allocated as AVHWDeviceContext.hwctx.
  *)
  pAVDRMDeviceContext = ^AVDRMDeviceContext;

  AVDRMDeviceContext = record
    (* *
      * File descriptor of DRM device.
      *
      * This is used as the device to create frames on, and may also be
      * used in some derivation and mapping operations.
      *
      * If no device is required, set to -1.
    *)
    fd: int;
  end;

{$ENDREGION}
{$REGION 'pixdesc.h'}

type
  pAVComponentDescriptor = ^AVComponentDescriptor;

  AVComponentDescriptor = record
    (* *
      * Which of the 4 planes contains the component.
    *)
    plane: int;

    (* *
      * Number of elements between 2 horizontally consecutive pixels.
      * Elements are bits for bitstream formats, bytes otherwise.
    *)
    step: int;

    (* *
      * Number of elements before the component of the first pixel.
      * Elements are bits for bitstream formats, bytes otherwise.
    *)
    offset: int;

    (* *
      * Number of least significant bits that must be shifted away
      * to get the value.
    *)
    shift: int;

    (* *
      * Number of bits in the component.
    *)
    depth: int;

{$IFDEF FF_API_PLUS1_MINUS1}
    (* * deprecated, use step instead *)
    // attribute_deprecated int step_minus1;
    step_minus1: int deprecated;

    (* * deprecated, use depth instead *)
    // attribute_deprecated int depth_minus1;
    depth_minus1: int deprecated;
    (* * deprecated, use offset instead *)
    // attribute_deprecated int offset_plus1;
    offset_plus1: int deprecated;
{$ENDIF}
  end;

  (* *
    * Descriptor that unambiguously describes how the bits of a pixel are
    * stored in the up to 4 data planes of an image. It also stores the
    * subsampling factors and number of components.
    *
    * @note This is separate of the colorspace (RGB, YCbCr, YPbPr, JPEG-style YUV
    *       and all the YUV variants) AVPixFmtDescriptor just stores how values
    *       are stored not what these values represent.
  *)
  pAVPixFmtDescriptor = ^AVPixFmtDescriptor;

  AVPixFmtDescriptor = record
    name: PAnsiChar;
    nb_components: uint8_t;
    /// < The number of components each pixel has, (1-4)

    (* *
      * Amount to shift the luma width right to find the chroma width.
      * For YV12 this is 1 for example.
      * chroma_width = AV_CEIL_RSHIFT(luma_width, log2_chroma_w)
      * The note above is needed to ensure rounding up.
      * This value only refers to the chroma components.
    *)
    log2_chroma_w: uint8_t;

    (* *
      * Amount to shift the luma height right to find the chroma height.
      * For YV12 this is 1 for example.
      * chroma_height= AV_CEIL_RSHIFT(luma_height, log2_chroma_h)
      * The note above is needed to ensure rounding up.
      * This value only refers to the chroma components.
    *)
    log2_chroma_h: uint8_t;

    (* *
      * Combination of AV_PIX_FMT_FLAG_... flags.
    *)
    flags: uint64_t;

    (* *
      * Parameters that describe how pixels are packed.
      * If the format has 1 or 2 components, then luma is 0.
      * If the format has 3 or 4 components:
      *   if the RGB flag is set then 0 is red, 1 is green and 2 is blue;
      *   otherwise 0 is luma, 1 is chroma-U and 2 is chroma-V.
      *
      * If present, the Alpha channel is always the last component.
    *)
    comp: array [0 .. 3] of AVComponentDescriptor;

    (* *
      * Alternative comma-separated names.
    *)
    alias: PAnsiChar;
  end;

const
  (* *
    * Pixel format is big-endian.
  *)
  AV_PIX_FMT_FLAG_BE = (1 shl 0);
  (* *
    * Pixel format has a palette in data[1], values are indexes in this palette.
  *)
  AV_PIX_FMT_FLAG_PAL = (1 shl 1);
  (* *
    * All values of a component are bit-wise packed end to end.
  *)
  AV_PIX_FMT_FLAG_BITSTREAM = (1 shl 2);
  (* *
    * Pixel format is an HW accelerated format.
  *)
  AV_PIX_FMT_FLAG_HWACCEL = (1 shl 3);
  (* *
    * At least one pixel component is not in the first data plane.
  *)
  AV_PIX_FMT_FLAG_PLANAR = (1 shl 4);
  (* *
    * The pixel format contains RGB-like data (as opposed to YUV/grayscale).
  *)
  AV_PIX_FMT_FLAG_RGB = (1 shl 5);

  (* *
    * The pixel format is "pseudo-paletted". This means that it contains a
    * fixed palette in the 2nd plane but the palette is fixed/constant for each
    * PIX_FMT. This allows interpreting the data as if it was PAL8, which can
    * in some cases be simpler. Or the data can be interpreted purely based on
    * the pixel format without using the palette.
    * An example of a pseudo-paletted format is AV_PIX_FMT_GRAY8
    *
    * @deprecated This flag is deprecated, and will be removed. When it is removed,
    * the extra palette allocation in AVFrame.data[1] is removed as well. Only
    * actual paletted formats (as indicated by AV_PIX_FMT_FLAG_PAL) will have a
    * palette. Starting with FFmpeg versions which have this flag deprecated, the
    * extra "pseudo" palette is already ignored, and API users are not required to
    * allocate a palette for AV_PIX_FMT_FLAG_PSEUDOPAL formats (it was required
    * before the deprecation, though).
  *)
  AV_PIX_FMT_FLAG_PSEUDOPAL = (1 shl 6);

  (* *
    * The pixel format has an alpha channel. This is set on all formats that
    * support alpha in some way. The exception is AV_PIX_FMT_PAL8, which can
    * carry alpha as part of the palette. Details are explained in the
    * AVPixelFormat enum, and are also encoded in the corresponding
    * AVPixFmtDescriptor.
    *
    * The alpha is always straight, never pre-multiplied.
    *
    * If a codec or a filter does not support alpha, it should set all alpha to
    * opaque, or use the equivalent pixel formats without alpha component, e.g.
    * AV_PIX_FMT_RGB0 (or AV_PIX_FMT_RGB24 etc.) instead of AV_PIX_FMT_RGBA.
  *)
  AV_PIX_FMT_FLAG_ALPHA = (1 shl 7);

  (* *
    * The pixel format is following a Bayer pattern
  *)
  AV_PIX_FMT_FLAG_BAYER = (1 shl 8);

  (* *
    * The pixel format contains IEEE-754 floating point values. Precision (double,
    * single, or half) should be determined by the pixel size (64, 32, or 16 bits).
  *)
  AV_PIX_FMT_FLAG_FLOAT = (1 shl 9);

  (* *
    * Return the number of bits per pixel used by the pixel format
    * described by pixdesc. Note that this is not the same as the number
    * of bits per sample.
    *
    * The returned number of bits refers to the number of bits actually
    * used for storing the pixel information, that is padding bits are
    * not counted.
  *)
  // int av_get_bits_per_pixel(const AVPixFmtDescriptor *pixdesc);
function av_get_bits_per_pixel(const pixdesc: pAVPixFmtDescriptor): int; cdecl; external avutil_dll;
(* *
  * Return the number of bits per pixel for the pixel format
  * described by pixdesc, including any padding or unused bits.
*)
// int av_get_padded_bits_per_pixel(const AVPixFmtDescriptor *pixdesc);
function av_get_padded_bits_per_pixel(const pixdesc: pAVPixFmtDescriptor): int; cdecl; external avutil_dll;
(* *
  * @return a pixel format descriptor for provided pixel format or NULL if
  * this pixel format is unknown.
*)
// const AVPixFmtDescriptor *av_pix_fmt_desc_get(enum AVPixelFormat pix_fmt);
function av_pix_fmt_desc_get(pix_fmt: AVPixelFormat): pAVPixFmtDescriptor; cdecl; external avutil_dll;
(* *
  * Iterate over all pixel format descriptors known to libavutil.
  *
  * @param prev previous descriptor. NULL to get the first descriptor.
  *
  * @return next descriptor or NULL after the last descriptor
*)
// const AVPixFmtDescriptor *av_pix_fmt_desc_next(const AVPixFmtDescriptor *prev);
function av_pix_fmt_desc_next(const prev: pAVPixFmtDescriptor): pAVPixFmtDescriptor; cdecl; external avutil_dll;
(* *
  * @return an AVPixelFormat id described by desc, or AV_PIX_FMT_NONE if desc
  * is not a valid pointer to a pixel format descriptor.
*)
// enum AVPixelFormat av_pix_fmt_desc_get_id(const AVPixFmtDescriptor *desc);
function av_pix_fmt_desc_get_id(const desc: pAVPixFmtDescriptor): AVPixelFormat; cdecl; external avutil_dll;
(* *
  * Utility function to access log2_chroma_w log2_chroma_h from
  * the pixel format AVPixFmtDescriptor.
  *
  * @param[in]  pix_fmt the pixel format
  * @param[out] h_shift store log2_chroma_w (horizontal/width shift)
  * @param[out] v_shift store log2_chroma_h (vertical/height shift)
  *
  * @return 0 on success, AVERROR(ENOSYS) on invalid or unknown pixel format
*)
// int av_pix_fmt_get_chroma_sub_sample(enum AVPixelFormat pix_fmt,int *h_shift, int *v_shift);
function av_pix_fmt_get_chroma_sub_sample(pix_fmt: AVPixelFormat; var h_shift: int; var v_shift: int): int; cdecl; external avutil_dll;
(* *
  * @return number of planes in pix_fmt, a negative AVERROR if pix_fmt is not a
  * valid pixel format.
*)
// int av_pix_fmt_count_planes(enum AVPixelFormat pix_fmt);
function av_pix_fmt_count_planes(pix_fmt: AVPixelFormat): int; cdecl; external avutil_dll;
(* *
  * @return the name for provided color range or NULL if unknown.
*)
// const char *av_color_range_name(enum AVColorRange range);
function av_color_range_name(range: AVColorRange): PAnsiChar; cdecl; external avutil_dll;
(* *
  * @return the AVColorRange value for name or an AVError if not found.
*)
// int av_color_range_from_name(const char *name);
function av_color_range_from_name(const name: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * @return the name for provided color primaries or NULL if unknown.
*)
// const char *av_color_primaries_name(enum AVColorPrimaries primaries);
function av_color_primaries_name(primaries: AVColorPrimaries): PAnsiChar; cdecl; external avutil_dll;
(* *
  * @return the AVColorPrimaries value for name or an AVError if not found.
*)
// int av_color_primaries_from_name(const char *name);
function av_color_primaries_from_name(const name: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * @return the name for provided color transfer or NULL if unknown.
*)
// const char *av_color_transfer_name(enum AVColorTransferCharacteristic transfer);
function av_color_transfer_name(transfer: AVColorTransferCharacteristic): PAnsiChar; cdecl; external avutil_dll;
(* *
  * @return the AVColorTransferCharacteristic value for name or an AVError if not found.
*)
// int av_color_transfer_from_name(const char *name);
function av_color_transfer_from_name(const name: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * @return the name for provided color space or NULL if unknown.
*)
// const char *av_color_space_name(enum AVColorSpace space);
function av_color_space_name(space: AVColorSpace): PAnsiChar; cdecl; external avutil_dll;
(* *
  * @return the AVColorSpace value for name or an AVError if not found.
*)
// int av_color_space_from_name(const char *name);
function av_color_space_from_name(const name: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * @return the name for provided chroma location or NULL if unknown.
*)
// const char *av_chroma_location_name(enum AVChromaLocation location);
function av_chroma_location_name(location: AVChromaLocation): PAnsiChar; cdecl; external avutil_dll;
(* *
  * @return the AVChromaLocation value for name or an AVError if not found.
*)
// int av_chroma_location_from_name(const char *name);
function av_chroma_location_from_name(const name: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * Return the pixel format corresponding to name.
  *
  * If there is no pixel format with name name, then looks for a
  * pixel format with the name corresponding to the native endian
  * format of name.
  * For example in a little-endian system, first looks for "gray16",
  * then for "gray16le".
  *
  * Finally if no pixel format has been found, returns AV_PIX_FMT_NONE.
*)
// enum AVPixelFormat av_get_pix_fmt(const char *name);
function av_get_pix_fmt(const name: PAnsiChar): AVPixelFormat; cdecl; external avutil_dll;
(* *
  * Return the short name for a pixel format, NULL in case pix_fmt is
  * unknown.
  *
  * @see av_get_pix_fmt(), av_get_pix_fmt_string()
*)
// const char *av_get_pix_fmt_name(enum AVPixelFormat pix_fmt);
function av_get_pix_fmt_name(pix_fmt: AVPixelFormat): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Print in buf the string corresponding to the pixel format with
  * number pix_fmt, or a header if pix_fmt is negative.
  *
  * @param buf the buffer where to write the string
  * @param buf_size the size of buf
  * @param pix_fmt the number of the pixel format to print the
  * corresponding info string, or a negative value to print the
  * corresponding header.
*)

// char *av_get_pix_fmt_string(char *buf, int buf_size, enum AVPixelFormat pix_fmt);
function av_get_pix_fmt_string(buf: PAnsiChar; buf_size: int; pix_fmt: AVPixelFormat): PAnsiChar; cdecl; external avutil_dll;

(* *
  * Read a line from an image, and write the values of the
  * pixel format component c to dst.
  *
  * @param data the array containing the pointers to the planes of the image
  * @param linesize the array containing the linesizes of the image
  * @param desc the pixel format descriptor for the image
  * @param x the horizontal coordinate of the first pixel to read
  * @param y the vertical coordinate of the first pixel to read
  * @param w the width of the line to read, that is the number of
  * values to write to dst
  * @param read_pal_component if not zero and the format is a paletted
  * format writes the values corresponding to the palette
  * component c in data[1] to dst, rather than the palette indexes in
  * data[0]. The behavior is undefined if the format is not paletted.
  * @param dst_element_size size of elements in dst array (2 or 4 byte)
*)

Type
  Tav_read_array4_puint8_t = record
{$IFDEF  REALISE}
      array4_puint8_t;
{$ENDIF}
  end;

  pav_read_array4_puint8_t = ^Tav_read_array4_puint8_t;

  Tav_read_array4_int = record
{$IFDEF  REALISE}
      array4_int;
{$ENDIF}
  end;

  pav_read_array4_int = ^Tav_read_array4_int;

  // void av_read_image_line2(void *dst, const uint8_t *data[4],
  // const int linesize[4], const AVPixFmtDescriptor *desc,
  // int x, int y, int c, int w, int read_pal_component,
  // int dst_element_size);

procedure av_read_image_line2(dst: Pointer; const data: pav_read_array4_puint8_t; const linesize: pav_read_array4_int; const desc: pAVPixFmtDescriptor;
  x, y, c, w, read_pal_component, dst_element_size: int); cdecl; external avutil_dll;

// void av_read_image_line(uint16_t *dst, const uint8_t *data[4],
// const int linesize[4], const AVPixFmtDescriptor *desc,
// int x, int y, int c, int w, int read_pal_component);
procedure av_read_image_line(dst: puint16_t; const data: pav_read_array4_puint8_t; const linesize: pav_read_array4_int; const desc: pAVPixFmtDescriptor;
  x, y, c, w: int; read_pal_component: int); cdecl; external avutil_dll;
(* *
  * Write the values from src to the pixel format component c of an
  * image line.
  *
  * @param src array containing the values to write
  * @param data the array containing the pointers to the planes of the
  * image to write into. It is supposed to be zeroed.
  * @param linesize the array containing the linesizes of the image
  * @param desc the pixel format descriptor for the image
  * @param x the horizontal coordinate of the first pixel to write
  * @param y the vertical coordinate of the first pixel to write
  * @param w the width of the line to write, that is the number of
  * values to write to the image line
  * @param src_element_size size of elements in src array (2 or 4 byte)
*)

// void av_write_image_line2(const void *src, uint8_t *data[4],
// const int linesize[4], const AVPixFmtDescriptor *desc,
// int x, int y, int c, int w, int src_element_size);
procedure av_write_image_line2(const src: puint16_t; data: pav_read_array4_puint8_t; const linesize: pav_read_array4_int; const desc: pAVPixFmtDescriptor;
  x: int; y: int; c: int; w: int; src_element_size: int); cdecl; external avutil_dll;

// void av_write_image_line(const uint16_t *src, uint8_t *data[4],
// const int linesize[4], const AVPixFmtDescriptor *desc,
// int x, int y, int c, int w);
procedure av_write_image_line(const src: puint16_t; data: pav_read_array4_puint8_t; const linesize: pav_read_array4_int; const desc: pAVPixFmtDescriptor;
  x: int; y: int; c: int; w: int); cdecl; external avutil_dll;
(* *
  * Utility function to swap the endianness of a pixel format.
  *
  * @param[in]  pix_fmt the pixel format
  *
  * @return pixel format with swapped endianness if it exists,
  * otherwise AV_PIX_FMT_NONE
*)
// enum AVPixelFormat av_pix_fmt_swap_endianness(enum AVPixelFormat pix_fmt);
function av_pix_fmt_swap_endianness(pix_fmt: AVPixelFormat): AVPixelFormat; cdecl; external avutil_dll;

const
  FF_LOSS_RESOLUTION = $0001; (* *< loss due to resolution change *)
  FF_LOSS_DEPTH      = $0002; (* *< loss due to color depth change *)
  FF_LOSS_COLORSPACE = $0004; (* *< loss due to color space conversion *)
  FF_LOSS_ALPHA      = $0008; (* *< loss of alpha bits *)
  FF_LOSS_COLORQUANT = $0010; (* *< loss due to color quantization *)
  FF_LOSS_CHROMA     = $0020; (* *< loss of chroma (e.g. RGB to gray conversion) *)

  (* *
    * Compute what kind of losses will occur when converting from one specific
    * pixel format to another.
    * When converting from one pixel format to another, information loss may occur.
    * For example, when converting from RGB24 to GRAY, the color information will
    * be lost. Similarly, other losses occur when converting from some formats to
    * other formats. These losses can involve loss of chroma, but also loss of
    * resolution, loss of color depth, loss due to the color space conversion, loss
    * of the alpha bits or loss due to color quantization.
    * av_get_fix_fmt_loss() informs you about the various types of losses
    * which will occur when converting from one pixel format to another.
    *
    * @param[in] dst_pix_fmt destination pixel format
    * @param[in] src_pix_fmt source pixel format
    * @param[in] has_alpha Whether the source pixel format alpha channel is used.
    * @return Combination of flags informing you what kind of losses will occur
    * (maximum loss for an invalid dst_pix_fmt).
  *)
  // int av_get_pix_fmt_loss(enum AVPixelFormat dst_pix_fmt,
  // enum AVPixelFormat src_pix_fmt,
  // int has_alpha);
function av_get_pix_fmt_loss(dst_pix_fmt: AVPixelFormat; src_pix_fmt: AVPixelFormat; has_alpha: int): int; cdecl; external avutil_dll;
(* *
  * Compute what kind of losses will occur when converting from one specific
  * pixel format to another.
  * When converting from one pixel format to another, information loss may occur.
  * For example, when converting from RGB24 to GRAY, the color information will
  * be lost. Similarly, other losses occur when converting from some formats to
  * other formats. These losses can involve loss of chroma, but also loss of
  * resolution, loss of color depth, loss due to the color space conversion, loss
  * of the alpha bits or loss due to color quantization.
  * av_get_fix_fmt_loss() informs you about the various types of losses
  * which will occur when converting from one pixel format to another.
  *
  * @param[in] dst_pix_fmt destination pixel format
  * @param[in] src_pix_fmt source pixel format
  * @param[in] has_alpha Whether the source pixel format alpha channel is used.
  * @return Combination of flags informing you what kind of losses will occur
  * (maximum loss for an invalid dst_pix_fmt).
*)
// enum AVPixelFormat av_find_best_pix_fmt_of_2(enum AVPixelFormat dst_pix_fmt1, enum AVPixelFormat dst_pix_fmt2,
// enum AVPixelFormat src_pix_fmt, int has_alpha, int *loss_ptr);
function av_find_best_pix_fmt_of_2(dst_pix_fmt1: AVPixelFormat; dst_pix_fmt2: AVPixelFormat; src_pix_fmt: AVPixelFormat; has_alpha: int; var loss_ptr: int)
  : AVPixelFormat; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'imgutils.h'}

type
  Tav_image_array4_int = array4_int;
  pav_image_array4_int = ^Tav_image_array4_int;

  Tav_image_array4_puint8_t = array4_puint8_t;
  pav_image_array4_puint8_t = ^Tav_image_array4_puint8_t;

  Tav_image_array4_ptrdiff_t = array4_ptrdiff_t;
  pav_image_array4_ptrdiff_t = ^Tav_image_array4_ptrdiff_t;

  (* *
    * Compute the max pixel step for each plane of an image with a
    * format described by pixdesc.
    *
    * The pixel step is the distance in bytes between the first byte of
    * the group of bytes which describe a pixel component and the first
    * byte of the successive group in the same plane for the same
    * component.
    *
    * @param max_pixsteps an array which is filled with the max pixel step
    * for each plane. Since a plane may contain different pixel
    * components, the computed max_pixsteps[plane] is relative to the
    * component in the plane with the max pixel step.
    * @param max_pixstep_comps an array which is filled with the component
    * for each plane which has the max pixel step. May be NULL.
  *)
  // void av_image_fill_max_pixsteps(int max_pixsteps[4], int max_pixstep_comps[4],
  // const AVPixFmtDescriptor *pixdesc);
procedure av_image_fill_max_pixsteps(max_pixsteps: pav_image_array4_int; max_pixstep_comps: pav_image_array4_int; const pixdesc: pAVPixFmtDescriptor); cdecl;
  external avutil_dll;
(* *
  * Compute the size of an image line with format pix_fmt and width
  * width for the plane plane.
  *
  * @return the computed size in bytes
*)
// int av_image_get_linesize(enum AVPixelFormat pix_fmt, int width, int plane);
function av_image_get_linesize(pix_fmt: AVPixelFormat; width: int; plane: int): int; cdecl; external avutil_dll;
(* *
  * Fill plane linesizes for an image with pixel format pix_fmt and
  * width width.
  *
  * @param linesizes array to be filled with the linesize for each plane
  * @return >= 0 in case of success, a negative error code otherwise
*)
// int av_image_fill_linesizes(int linesizes[4], enum AVPixelFormat pix_fmt, int width);
function av_image_fill_linesizes(linesizes: pav_image_array4_int; pix_fmt: AVPixelFormat; width: int): int; cdecl; external avutil_dll;
(* *
  * Fill plane data pointers for an image with pixel format pix_fmt and
  * height height.
  *
  * @param data pointers array to be filled with the pointer for each image plane
  * @param ptr the pointer to a buffer which will contain the image
  * @param linesizes the array containing the linesize for each
  * plane, should be filled by av_image_fill_linesizes()
  * @return the size in bytes required for the image buffer, a negative
  * error code in case of failure
*)
// int av_image_fill_pointers(uint8_t *data[4], enum AVPixelFormat pix_fmt, int height,
// uint8_t *ptr, const int linesizes[4]);
function av_image_fill_pointers(data: pav_image_array4_puint8_t; pix_fmt: AVPixelFormat; height: int; ptr: puint8_t; const linesizes: pav_image_array4_int)
  : int; cdecl; external avutil_dll;
(* *
  * Allocate an image with size w and h and pixel format pix_fmt, and
  * fill pointers and linesizes accordingly.
  * The allocated image buffer has to be freed by using
  * av_freep(&pointers[0]).
  *
  * @param align the value to use for buffer size alignment
  * @return the size in bytes required for the image buffer, a negative
  * error code in case of failure
*)
// int av_image_alloc(uint8_t *pointers[4], int linesizes[4],
// int w, int h, enum AVPixelFormat pix_fmt, int align);
function av_image_alloc(               //
  pointers: pav_image_array4_puint8_t; //
  linesizes: pav_image_array4_int;     //
  w: int;                              //
  h: int;                              //
  pix_fmt: AVPixelFormat;              //
  align: int):                         //
  int; cdecl; overload; external avutil_dll;

function av_image_alloc(  //
  pointers: Pointer;      //
  linesizes: Pointer;     //
  w: int;                 //
  h: int;                 //
  pix_fmt: AVPixelFormat; //
  align: int):            //
  int; cdecl; overload; external avutil_dll;

(* *
  * Copy image plane from src to dst.
  * That is, copy "height" number of lines of "bytewidth" bytes each.
  * The first byte of each successive line is separated by *_linesize
  * bytes.
  *
  * bytewidth must be contained by both absolute values of dst_linesize
  * and src_linesize, otherwise the function behavior is undefined.
  *
  * @param dst_linesize linesize for the image plane in dst
  * @param src_linesize linesize for the image plane in src
*)
// void av_image_copy_plane(uint8_t       *dst, int dst_linesize,
// const uint8_t *src, int src_linesize,
// int bytewidth, int height);
procedure av_image_copy_plane(dst: puint8_t; dst_linesize: int; const src: puint8_t; src_linesize: int; bytewidth: int; height: int); cdecl;
  external avutil_dll;
(* *
  * Copy image in src_data to dst_data.
  *
  * @param dst_linesizes linesizes for the image in dst_data
  * @param src_linesizes linesizes for the image in src_data
*)
// void av_image_copy(uint8_t *dst_data[4], int dst_linesizes[4],
// const uint8_t *src_data[4], const int src_linesizes[4],
// enum AVPixelFormat pix_fmt, int width, int height);
procedure av_image_copy(dst_data: pav_image_array4_puint8_t; dst_linesizes: pav_image_array4_int; const src_data: pav_image_array4_puint8_t;
  const src_linesizes: pav_image_array4_int; pix_fmt: AVPixelFormat; width: int; height: int); cdecl; external avutil_dll;
(* *
  * Copy image data located in uncacheable (e.g. GPU mapped) memory. Where
  * available, this function will use special functionality for reading from such
  * memory, which may result in greatly improved performance compared to plain
  * av_image_copy().
  *
  * The data pointers and the linesizes must be aligned to the maximum required
  * by the CPU architecture.
  *
  * @note The linesize parameters have the type ptrdiff_t here, while they are
  *       int for av_image_copy().
  * @note On x86, the linesizes currently need to be aligned to the cacheline
  *       size (i.e. 64) to get improved performance.
*)
// void av_image_copy_uc_from(uint8_t *dst_data[4],       const ptrdiff_t dst_linesizes[4],
// const uint8_t *src_data[4], const ptrdiff_t src_linesizes[4],
// enum AVPixelFormat pix_fmt, int width, int height);
procedure av_image_copy_uc_from(dst_data: pav_image_array4_puint8_t; const dst_linesizes: pav_image_array4_ptrdiff_t; const src_data: pav_image_array4_puint8_t;
  const src_linesizes: pav_image_array4_ptrdiff_t; pix_fmt: AVPixelFormat; width: int; height: int); cdecl; external avutil_dll;
(* *
  * Setup the data pointers and linesizes based on the specified image
  * parameters and the provided array.
  *
  * The fields of the given image are filled in by using the src
  * address which points to the image data buffer. Depending on the
  * specified pixel format, one or multiple image data pointers and
  * line sizes will be set.  If a planar format is specified, several
  * pointers will be set pointing to the different picture planes and
  * the line sizes of the different planes will be stored in the
  * lines_sizes array. Call with src == NULL to get the required
  * size for the src buffer.
  *
  * To allocate the buffer and fill in the dst_data and dst_linesize in
  * one call, use av_image_alloc().
  *
  * @param dst_data      data pointers to be filled in
  * @param dst_linesize  linesizes for the image in dst_data to be filled in
  * @param src           buffer which will contain or contains the actual image data, can be NULL
  * @param pix_fmt       the pixel format of the image
  * @param width         the width of the image in pixels
  * @param height        the height of the image in pixels
  * @param align         the value used in src for linesize alignment
  * @return the size in bytes required for src, a negative error code
  * in case of failure
*)
// int av_image_fill_arrays(uint8_t *dst_data[4], int dst_linesize[4],
// const uint8_t *src,
// enum AVPixelFormat pix_fmt, int width, int height, int align);
function av_image_fill_arrays(dst_data: pav_image_array4_puint8_t; dst_linesize: pav_image_array4_int; const src: puint8_t; pix_fmt: AVPixelFormat; width: int;
  height: int; align: int): int; cdecl; external avutil_dll;
(* *
  * Return the size in bytes of the amount of data required to store an
  * image with the given parameters.
  *
  * @param pix_fmt  the pixel format of the image
  * @param width    the width of the image in pixels
  * @param height   the height of the image in pixels
  * @param align    the assumed linesize alignment
  * @return the buffer size in bytes, a negative error code in case of failure
*)
// int av_image_get_buffer_size(enum AVPixelFormat pix_fmt, int width, int height, int align);
function av_image_get_buffer_size(pix_fmt: AVPixelFormat; width: int; height: int; align: int): int; cdecl; external avutil_dll;
(* *
  * Copy image data from an image into a buffer.
  *
  * av_image_get_buffer_size() can be used to compute the required size
  * for the buffer to fill.
  *
  * @param dst           a buffer into which picture data will be copied
  * @param dst_size      the size in bytes of dst
  * @param src_data      pointers containing the source image data
  * @param src_linesize  linesizes for the image in src_data
  * @param pix_fmt       the pixel format of the source image
  * @param width         the width of the source image in pixels
  * @param height        the height of the source image in pixels
  * @param align         the assumed linesize alignment for dst
  * @return the number of bytes written to dst, or a negative value
  * (error code) on error
*)
// int av_image_copy_to_buffer(uint8_t *dst, int dst_size,
// const uint8_t * const src_data[4], const int src_linesize[4],
// enum AVPixelFormat pix_fmt, int width, int height, int align);
function av_image_copy_to_buffer(dst: puint8_t; dst_size: int; const src_data: pav_image_array4_puint8_t; const src_linesize: pav_image_array4_int;
  pix_fmt: AVPixelFormat; width: int; height: int; align: int): int; cdecl; external avutil_dll;
(* *
  * Check if the given dimension of an image is valid, meaning that all
  * bytes of the image can be addressed with a signed int.
  *
  * @param w the width of the picture
  * @param h the height of the picture
  * @param log_offset the offset to sum to the log level for logging with log_ctx
  * @param log_ctx the parent logging context, it may be NULL
  * @return >= 0 if valid, a negative error code otherwise
*)
// int av_image_check_size(unsigned int w, unsigned int h, int log_offset, void *log_ctx);
function av_image_check_size(w: unsigned_int; h: unsigned_int; log_offset: int; log_ctx: Pointer): int; cdecl; external avutil_dll;
(* *
  * Check if the given dimension of an image is valid, meaning that all
  * bytes of a plane of an image with the specified pix_fmt can be addressed
  * with a signed int.
  *
  * @param w the width of the picture
  * @param h the height of the picture
  * @param max_pixels the maximum number of pixels the user wants to accept
  * @param pix_fmt the pixel format, can be AV_PIX_FMT_NONE if unknown.
  * @param log_offset the offset to sum to the log level for logging with log_ctx
  * @param log_ctx the parent logging context, it may be NULL
  * @return >= 0 if valid, a negative error code otherwise
*)
// int av_image_check_size2(unsigned int w, unsigned int h, int64_t max_pixels, enum AVPixelFormat pix_fmt, int log_offset, void *log_ctx);
function av_image_check_size2(w: unsigned_int; h: unsigned_int; max_pixels: int64_t; pix_fmt: AVPixelFormat; log_offset: int; log_ctx: Pointer): int; cdecl;
  external avutil_dll;
(* *
  * Check if the given sample aspect ratio of an image is valid.
  *
  * It is considered invalid if the denominator is 0 or if applying the ratio
  * to the image size would make the smaller dimension less than 1. If the
  * sar numerator is 0, it is considered unknown and will return as valid.
  *
  * @param w width of the image
  * @param h height of the image
  * @param sar sample aspect ratio of the image
  * @return 0 if valid, a negative AVERROR code otherwise
*)
// int av_image_check_sar(unsigned int w, unsigned int h, AVRational sar);
function av_image_check_sar(w: unsigned_int; h: unsigned_int; sar: AVRational): int; cdecl; external avutil_dll;
(* *
  * Overwrite the image data with black. This is suitable for filling a
  * sub-rectangle of an image, meaning the padding between the right most pixel
  * and the left most pixel on the next line will not be overwritten. For some
  * formats, the image size might be rounded up due to inherent alignment.
  *
  * If the pixel format has alpha, the alpha is cleared to opaque.
  *
  * This can return an error if the pixel format is not supported. Normally, all
  * non-hwaccel pixel formats should be supported.
  *
  * Passing NULL for dst_data is allowed. Then the function returns whether the
  * operation would have succeeded. (It can return an error if the pix_fmt is
  * not supported.)
  *
  * @param dst_data      data pointers to destination image
  * @param dst_linesize  linesizes for the destination image
  * @param pix_fmt       the pixel format of the image
  * @param range         the color range of the image (important for colorspaces such as YUV)
  * @param width         the width of the image in pixels
  * @param height        the height of the image in pixels
  * @return 0 if the image data was cleared, a negative AVERROR code otherwise
*)
// int av_image_fill_black(uint8_t *dst_data[4], const ptrdiff_t dst_linesize[4],
// enum AVPixelFormat pix_fmt, enum AVColorRange range,
// int width, int height);
function av_image_fill_black(dst_data: pav_image_array4_puint8_t; const dst_linesize: pav_image_array4_ptrdiff_t; pix_fmt: AVPixelFormat; range: AVColorRange;
  width: int; height: int): int; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'time.h'}
(* *
  * Get the current time in microseconds.
*)
// int64_t av_gettime(void);
function av_gettime(): int64_t; cdecl; external avutil_dll;
(* *
  * Get the current time in microseconds since some unspecified starting point.
  * On platforms that support it, the time comes from a monotonic clock
  * This property makes this time source ideal for measuring relative time.
  * The returned values may not be monotonic on platforms where a monotonic
  * clock is not available.
*)
// int64_t av_gettime_relative(void);
function av_gettime_relative(): int64_t; cdecl; external avutil_dll;
(* *
  * Indicates with a boolean result if the av_gettime_relative() time source
  * is monotonic.
*)
// int av_gettime_relative_is_monotonic(void);
function av_gettime_relative_is_monotonic(): int; cdecl; external avutil_dll;
(* *
  * Sleep for a period of time.  Although the duration is expressed in
  * microseconds, the actual delay may be rounded to the precision of the
  * system timer.
  *
  * @param  usec Number of microseconds to sleep.
  * @return zero on success or (negative) error code.
*)
// int av_usleep(unsigned usec);
function av_usleep(usec: unsigned): int; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'timestamp.h'}

const
  AV_TS_MAX_STRING_SIZE = 32;

  (* *
    * Fill the provided buffer with a string containing a timestamp
    * representation.
    *
    * @param buf a buffer with size in bytes of at least AV_TS_MAX_STRING_SIZE
    * @param ts the timestamp to represent
    * @return the buffer in input
  *)
  // static inline char *av_ts_make_string(char *buf, int64_t ts)
function av_ts_make_string(buf: PAnsiChar; ts: int64_t): PAnsiChar;

(* *
  * Convenience macro, the return value should be used only directly in
  * function arguments but never stand-alone.
*)
// #define av_ts2str(ts) av_ts_make_string((char[AV_TS_MAX_STRING_SIZE]){0}, ts)
function av_ts2str(ts: int64_t): PAnsiChar;

(* *
  * Fill the provided buffer with a string containing a timestamp time
  * representation.
  *
  * @param buf a buffer with size in bytes of at least AV_TS_MAX_STRING_SIZE
  * @param ts the timestamp to represent
  * @param tb the timebase of the timestamp
  * @return the buffer in input
*)
// static inline char *av_ts_make_time_string(char *buf, int64_t ts, AVRational *tb)
function av_ts_make_time_string(buf: PAnsiChar; ts: int64_t; tb: pAVRational): PAnsiChar;

(* *
  * Convenience macro, the return value should be used only directly in
  * function arguments but never stand-alone.
*)
// #define av_ts2timestr(ts, tb) av_ts_make_time_string((char[AV_TS_MAX_STRING_SIZE]){0}, ts, tb)
function av_ts2timestr(ts: int64_t; tb: pAVRational): PAnsiChar;

{$ENDREGION}
{$REGION 'mem.h'}
(* *
  * Allocate a memory block with alignment suitable for all memory accesses
  * (including vectors if available on the CPU).
  *
  * @param size Size in bytes for the memory block to be allocated
  * @return Pointer to the allocated block, or `NULL` if the block cannot
  *         be allocated
  * @see av_mallocz()
*)
// void *av_malloc(size_t size) av_malloc_attrib av_alloc_size(1);
function av_malloc(size: size_t): Pointer; cdecl; external avutil_dll;

(* *
  * Allocate a memory block with alignment suitable for all memory accesses
  * (including vectors if available on the CPU) and zero all the bytes of the
  * block.
  *
  * @param size Size in bytes for the memory block to be allocated
  * @return Pointer to the allocated block, or `NULL` if it cannot be allocated
  * @see av_malloc()
*)
// void *av_mallocz(size_t size) av_malloc_attrib av_alloc_size(1);
function av_mallocz(size: size_t): Pointer; cdecl; external avutil_dll;
(* *
  * Allocate a memory block for an array with av_malloc().
  *
  * The allocated memory will have size `size * nmemb` bytes.
  *
  * @param nmemb Number of element
  * @param size  Size of a single element
  * @return Pointer to the allocated block, or `NULL` if the block cannot
  *         be allocated
  * @see av_malloc()
*)
// av_alloc_size(1, 2) void *av_malloc_array(size_t nmemb, size_t size);
function av_malloc_array(nmemb: size_t; size: size_t): Pointer; cdecl; external avutil_dll;
(* *
  * Allocate a memory block for an array with av_mallocz().
  *
  * The allocated memory will have size `size * nmemb` bytes.
  *
  * @param nmemb Number of elements
  * @param size  Size of the single element
  * @return Pointer to the allocated block, or `NULL` if the block cannot
  *         be allocated
  *
  * @see av_mallocz()
  * @see av_malloc_array()
*)
// av_alloc_size(1, 2) void *av_mallocz_array(size_t nmemb, size_t size);
function av_mallocz_array(nmemb: size_t; size: size_t): Pointer; cdecl; external avutil_dll;
(* *
  * Non-inlined equivalent of av_mallocz_array().
  *
  * Created for symmetry with the calloc() C function.
*)
// void *av_calloc(size_t nmemb, size_t size) av_malloc_attrib;
function av_calloc(nmemb: size_t; size: size_t): Pointer; cdecl; external avutil_dll;
(* *
  * Allocate, reallocate, or free a block of memory.
  *
  * If `ptr` is `NULL` and `size` > 0, allocate a new block. If `size` is
  * zero, free the memory block pointed to by `ptr`. Otherwise, expand or
  * shrink that block of memory according to `size`.
  *
  * @param ptr  Pointer to a memory block already allocated with
  *             av_realloc() or `NULL`
  * @param size Size in bytes of the memory block to be allocated or
  *             reallocated
  *
  * @return Pointer to a newly-reallocated block or `NULL` if the block
  *         cannot be reallocated or the function is used to free the memory block
  *
  * @warning Unlike av_malloc(), the returned pointer is not guaranteed to be
  *          correctly aligned.
  * @see av_fast_realloc()
  * @see av_reallocp()
*)
// void *av_realloc(void *ptr, size_t size) av_alloc_size(2);
function av_realloc(ptr: Pointer; size: size_t): Pointer; cdecl; external avutil_dll;
(* *
  * Allocate, reallocate, or free a block of memory through a pointer to a
  * pointer.
  *
  * If `*ptr` is `NULL` and `size` > 0, allocate a new block. If `size` is
  * zero, free the memory block pointed to by `*ptr`. Otherwise, expand or
  * shrink that block of memory according to `size`.
  *
  * @param[in,out] ptr  Pointer to a pointer to a memory block already allocated
  *                     with av_realloc(), or a pointer to `NULL`. The pointer
  *                     is updated on success, or freed on failure.
  * @param[in]     size Size in bytes for the memory block to be allocated or
  *                     reallocated
  *
  * @return Zero on success, an AVERROR error code on failure
  *
  * @warning Unlike av_malloc(), the allocated memory is not guaranteed to be
  *          correctly aligned.
*)
// av_warn_unused_result
// int av_reallocp(void *ptr, size_t size);
function av_reallocp(ptr: Pointer; size: size_t): int; cdecl; external avutil_dll;
(* *
  * Allocate, reallocate, or free a block of memory.
  *
  * This function does the same thing as av_realloc(), except:
  * - It takes two size arguments and allocates `nelem * elsize` bytes,
  *   after checking the result of the multiplication for integer overflow.
  * - It frees the input block in case of failure, thus avoiding the memory
  *   leak with the classic
  *   @code{.c}
  *   buf = realloc(buf);
  *   if (!buf)
  *       return -1;
  *   @endcode
  *   pattern.
*)
// void *av_realloc_f(void *ptr, size_t nelem, size_t elsize);
function av_realloc_f(ptr: Pointer; nelem: size_t; elsize: size_t): Pointer; cdecl; external avutil_dll;
(* *
  * Allocate, reallocate, or free an array.
  *
  * If `ptr` is `NULL` and `nmemb` > 0, allocate a new block. If
  * `nmemb` is zero, free the memory block pointed to by `ptr`.
  *
  * @param ptr   Pointer to a memory block already allocated with
  *              av_realloc() or `NULL`
  * @param nmemb Number of elements in the array
  * @param size  Size of the single element of the array
  *
  * @return Pointer to a newly-reallocated block or NULL if the block
  *         cannot be reallocated or the function is used to free the memory block
  *
  * @warning Unlike av_malloc(), the allocated memory is not guaranteed to be
  *          correctly aligned.
  * @see av_reallocp_array()
*)
// av_alloc_size(2, 3) void *av_realloc_array(void *ptr, size_t nmemb, size_t size);
function av_realloc_array(ptr: Pointer; nmemb: size_t; size: size_t): Pointer; cdecl; external avutil_dll;
(* *
  * Allocate, reallocate, or free an array through a pointer to a pointer.
  *
  * If `*ptr` is `NULL` and `nmemb` > 0, allocate a new block. If `nmemb` is
  * zero, free the memory block pointed to by `*ptr`.
  *
  * @param[in,out] ptr   Pointer to a pointer to a memory block already
  *                      allocated with av_realloc(), or a pointer to `NULL`.
  *                      The pointer is updated on success, or freed on failure.
  * @param[in]     nmemb Number of elements
  * @param[in]     size  Size of the single element
  *
  * @return Zero on success, an AVERROR error code on failure
  *
  * @warning Unlike av_malloc(), the allocated memory is not guaranteed to be
  *          correctly aligned.
*)
// av_alloc_size(2, 3) int av_reallocp_array(void *ptr, size_t nmemb, size_t size);
function av_reallocp_array(ptr: Pointer; nmemb: size_t; size: size_t): int; cdecl; external avutil_dll;
(* *
  * Reallocate the given buffer if it is not large enough, otherwise do nothing.
  *
  * If the given buffer is `NULL`, then a new uninitialized buffer is allocated.
  *
  * If the given buffer is not large enough, and reallocation fails, `NULL` is
  * returned and `*size` is set to 0, but the original buffer is not changed or
  * freed.
  *
  * A typical use pattern follows:
  *
  * @code{.c}
  * uint8_t *buf = ...;
  * uint8_t *new_buf = av_fast_realloc(buf, &current_size, size_needed);
  * if (!new_buf) {
  *     // Allocation failed; clean up original buffer
  *     av_freep(&buf);
  *     return AVERROR(ENOMEM);
  * }
  * @endcode
  *
  * @param[in,out] ptr      Already allocated buffer, or `NULL`
  * @param[in,out] size     Pointer to the size of buffer `ptr`. `*size` is
  *                         updated to the new allocated size, in particular 0
  *                         in case of failure.
  * @param[in]     min_size Desired minimal size of buffer `ptr`
  * @return `ptr` if the buffer is large enough, a pointer to newly reallocated
  *         buffer if the buffer was not large enough, or `NULL` in case of
  *         error
  * @see av_realloc()
  * @see av_fast_malloc()
*)
// void *av_fast_realloc(void *ptr, unsigned int *size, size_t min_size);
function av_fast_realloc(ptr: Pointer; var size: unsigned_int; min_size: size_t): Pointer; cdecl; external avutil_dll;
(* *
  * Allocate a buffer, reusing the given one if large enough.
  *
  * Contrary to av_fast_realloc(), the current buffer contents might not be
  * preserved and on error the old buffer is freed, thus no special handling to
  * avoid memleaks is necessary.
  *
  * `*ptr` is allowed to be `NULL`, in which case allocation always happens if
  * `size_needed` is greater than 0.
  *
  * @code{.c}
  * uint8_t *buf = ...;
  * av_fast_malloc(&buf, &current_size, size_needed);
  * if (!buf) {
  *     // Allocation failed; buf already freed
  *     return AVERROR(ENOMEM);
  * }
  * @endcode
  *
  * @param[in,out] ptr      Pointer to pointer to an already allocated buffer.
  *                         `*ptr` will be overwritten with pointer to new
  *                         buffer on success or `NULL` on failure
  * @param[in,out] size     Pointer to the size of buffer `*ptr`. `*size` is
  *                         updated to the new allocated size, in particular 0
  *                         in case of failure.
  * @param[in]     min_size Desired minimal size of buffer `*ptr`
  * @see av_realloc()
  * @see av_fast_mallocz()
*)
// void av_fast_malloc(void *ptr, unsigned int *size, size_t min_size);
procedure av_fast_malloc(ptr: Pointer; var size: unsigned_int; min_size: size_t); cdecl; external avutil_dll;
(* *
  * Allocate and clear a buffer, reusing the given one if large enough.
  *
  * Like av_fast_malloc(), but all newly allocated space is initially cleared.
  * Reused buffer is not cleared.
  *
  * `*ptr` is allowed to be `NULL`, in which case allocation always happens if
  * `size_needed` is greater than 0.
  *
  * @param[in,out] ptr      Pointer to pointer to an already allocated buffer.
  *                         `*ptr` will be overwritten with pointer to new
  *                         buffer on success or `NULL` on failure
  * @param[in,out] size     Pointer to the size of buffer `*ptr`. `*size` is
  *                         updated to the new allocated size, in particular 0
  *                         in case of failure.
  * @param[in]     min_size Desired minimal size of buffer `*ptr`
  * @see av_fast_malloc()
*)
// void av_fast_mallocz(void *ptr, unsigned int *size, size_t min_size);
procedure av_fast_mallocz(ptr: Pointer; var size: unsigned_int; min_size: size_t); cdecl; external avutil_dll;
(* *
  * Free a memory block which has been allocated with a function of av_malloc()
  * or av_realloc() family.
  *
  * @param ptr Pointer to the memory block which should be freed.
  *
  * @note `ptr = NULL` is explicitly allowed.
  * @note It is recommended that you use av_freep() instead, to prevent leaving
  *       behind dangling pointers.
  * @see av_freep()
*)
// void av_free(void *ptr);
procedure av_free(ptr: Pointer); cdecl; external avutil_dll;
(* *
  * Free a memory block which has been allocated with a function of av_malloc()
  * or av_realloc() family, and set the pointer pointing to it to `NULL`.
  *
  * @code{.c}
  * uint8_t *buf = av_malloc(16);
  * av_free(buf);
  * // buf now contains a dangling pointer to freed memory, and accidental
  * // dereference of buf will result in a use-after-free, which may be a
  * // security risk.
  *
  * uint8_t *buf = av_malloc(16);
  * av_freep(&buf);
  * // buf is now NULL, and accidental dereference will only result in a
  * // NULL-pointer dereference.
  * @endcode
  *
  * @param ptr Pointer to the pointer to the memory block which should be freed
  * @note `*ptr = NULL` is safe and leads to no action.
  * @see av_free()
*)
// void av_freep(void *ptr);
procedure av_freep(ptr: Pointer); cdecl; external avutil_dll;
(* *
  * Duplicate a string.
  *
  * @param s String to be duplicated
  * @return Pointer to a newly-allocated string containing a
  *         copy of `s` or `NULL` if the string cannot be allocated
  * @see av_strndup()
*)
// char *av_strdup(const char *s) av_malloc_attrib;
function av_strdup(const s: PAnsiChar): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Duplicate a substring of a string.
  *
  * @param s   String to be duplicated
  * @param len Maximum length of the resulting string (not counting the
  *            terminating byte)
  * @return Pointer to a newly-allocated string containing a
  *         substring of `s` or `NULL` if the string cannot be allocated
*)
// char *av_strndup(const char *s, size_t len) av_malloc_attrib;
function av_strndup(const s: PAnsiChar; len: size_t): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Duplicate a buffer with av_malloc().
  *
  * @param p    Buffer to be duplicated
  * @param size Size in bytes of the buffer copied
  * @return Pointer to a newly allocated buffer containing a
  *         copy of `p` or `NULL` if the buffer cannot be allocated
*)
// void *av_memdup(const void *p, size_t size);
function av_memdup(const p: Pointer; size: size_t): Pointer; cdecl; external avutil_dll;
(* *
  * Overlapping memcpy() implementation.
  *
  * @param dst  Destination buffer
  * @param back Number of bytes back to start copying (i.e. the initial size of
  *             the overlapping window); must be > 0
  * @param cnt  Number of bytes to copy; must be >= 0
  *
  * @note `cnt > back` is valid, this will copy the bytes we just copied,
  *       thus creating a repeating pattern with a period length of `back`.
*)
// void av_memcpy_backptr(uint8_t *dst, int back, int cnt);
procedure av_memcpy_backptr(dst: puint8_t; back: int; cnt: int); cdecl; external avutil_dll;

(* *
  * @defgroup lavu_mem_dynarray Dynamic Array
  *
  * Utilities to make an array grow when needed.
  *
  * Sometimes, the programmer would want to have an array that can grow when
  * needed. The libavutil dynamic array utilities fill that need.
  *
  * libavutil supports two systems of appending elements onto a dynamically
  * allocated array, the first one storing the pointer to the value in the
  * array, and the second storing the value directly. In both systems, the
  * caller is responsible for maintaining a variable containing the length of
  * the array, as well as freeing of the array after use.
  *
  * The first system stores pointers to values in a block of dynamically
  * allocated memory. Since only pointers are stored, the function does not need
  * to know the size of the type. Both av_dynarray_add() and
  * av_dynarray_add_nofree() implement this system.
  *
  * @code
  * type **array = NULL; //< an array of pointers to values
  * int    nb    = 0;    //< a variable to keep track of the length of the array
  *
  * type to_be_added  = ...;
  * type to_be_added2 = ...;
  *
  * av_dynarray_add(&array, &nb, &to_be_added);
  * if (nb == 0)
  *     return AVERROR(ENOMEM);
  *
  * av_dynarray_add(&array, &nb, &to_be_added2);
  * if (nb == 0)
  *     return AVERROR(ENOMEM);
  *
  * // Now:
  * //  nb           == 2
  * // &to_be_added  == array[0]
  * // &to_be_added2 == array[1]
  *
  * av_freep(&array);
  * @endcode
  *
  * The second system stores the value directly in a block of memory. As a
  * result, the function has to know the size of the type. av_dynarray2_add()
  * implements this mechanism.
  *
  * @code
  * type *array = NULL; //< an array of values
  * int   nb    = 0;    //< a variable to keep track of the length of the array
  *
  * type to_be_added  = ...;
  * type to_be_added2 = ...;
  *
  * type *addr = av_dynarray2_add((void ** )&array, &nb, sizeof(*array), NULL);
  * if (!addr)
  *     return AVERROR(ENOMEM);
  * memcpy(addr, &to_be_added, sizeof(to_be_added));
  *
  * // Shortcut of the above.
  * type *addr = av_dynarray2_add((void ** )&array, &nb, sizeof( *array),
  *                               (const void * )&to_be_added2);
  * if (!addr)
  *     return AVERROR(ENOMEM);
  *
  * // Now:
  * //  nb           == 2
  * //  to_be_added  == array[0]
  * //  to_be_added2 == array[1]
  *
  * av_freep(&array);
  * @endcode
  *
  * @{
*)

(* *
  * Add the pointer to an element to a dynamic array.
  *
  * The array to grow is supposed to be an array of pointers to
  * structures, and the element to add must be a pointer to an already
  * allocated structure.
  *
  * The array is reallocated when its size reaches powers of 2.
  * Therefore, the amortized cost of adding an element is constant.
  *
  * In case of success, the pointer to the array is updated in order to
  * point to the new grown array, and the number pointed to by `nb_ptr`
  * is incremented.
  * In case of failure, the array is freed, `*tab_ptr` is set to `NULL` and
  * `*nb_ptr` is set to 0.
  *
  * @param[in,out] tab_ptr Pointer to the array to grow
  * @param[in,out] nb_ptr  Pointer to the number of elements in the array
  * @param[in]     elem    Element to add
  * @see av_dynarray_add_nofree(), av_dynarray2_add()
*)
// void av_dynarray_add(void *tab_ptr, int *nb_ptr, void *elem);
procedure av_dynarray_add(tab_ptr: Pointer; var nb_ptr: int; elem: Pointer); cdecl; external avutil_dll;
(* *
  * Add an element to a dynamic array.
  *
  * Function has the same functionality as av_dynarray_add(),
  * but it doesn't free memory on fails. It returns error code
  * instead and leave current buffer untouched.
  *
  * @return >=0 on success, negative otherwise
  * @see av_dynarray_add(), av_dynarray2_add()
*)
// av_warn_unused_result
// int av_dynarray_add_nofree(void *tab_ptr, int *nb_ptr, void *elem);
function av_dynarray_add_nofree(tab_ptr: Pointer; var nb_ptr: int; elem: Pointer): int; cdecl; external avutil_dll;
(* *
  * Add an element of size `elem_size` to a dynamic array.
  *
  * The array is reallocated when its number of elements reaches powers of 2.
  * Therefore, the amortized cost of adding an element is constant.
  *
  * In case of success, the pointer to the array is updated in order to
  * point to the new grown array, and the number pointed to by `nb_ptr`
  * is incremented.
  * In case of failure, the array is freed, `*tab_ptr` is set to `NULL` and
  * `*nb_ptr` is set to 0.
  *
  * @param[in,out] tab_ptr   Pointer to the array to grow
  * @param[in,out] nb_ptr    Pointer to the number of elements in the array
  * @param[in]     elem_size Size in bytes of an element in the array
  * @param[in]     elem_data Pointer to the data of the element to add. If
  *                          `NULL`, the space of the newly added element is
  *                          allocated but left uninitialized.
  *
  * @return Pointer to the data of the element to copy in the newly allocated
  *         space
  * @see av_dynarray_add(), av_dynarray_add_nofree()
*)
// void *av_dynarray2_add(void **tab_ptr, int *nb_ptr, size_t elem_size,
// const uint8_t *elem_data);
function av_dynarray2_add(var tab_ptr: Pointer; var nb_ptr: int; elem_size: size_t; const elem_data: puint8_t): Pointer; cdecl; external avutil_dll;

(* *
  * @defgroup lavu_mem_misc Miscellaneous Functions
  *
  * Other functions related to memory allocation.
  *
  * @{
*)

(* *
  * Multiply two `size_t` values checking for overflow.
  *
  * @param[in]  a,b Operands of multiplication
  * @param[out] r   Pointer to the result of the operation
  * @return 0 on success, AVERROR(EINVAL) on overflow
*)
// static inline int av_size_mult(size_t a, size_t b, size_t *r)
function av_size_mult(a: size_t; b: size_t; var r: size_t): int; inline;

(* *
  * Set the maximum size that may be allocated in one block.
  *
  * The value specified with this function is effective for all libavutil's @ref
  * lavu_mem_funcs "heap management functions."
  *
  * By default, the max value is defined as `INT_MAX`.
  *
  * @param max Value to be set as the new maximum size
  *
  * @warning Exercise extreme caution when using this function. Don't touch
  *          this if you do not understand the full consequence of doing so.
*)

// void av_max_alloc(size_t max);
procedure av_max_alloc(max: size_t); cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'timecode.h'}

const
  AV_TIMECODE_STR_SIZE = 23;

type
  AVTimecodeFlag = ( //
    AV_TIMECODE_FLAG_DROPFRAME = 1 shl 0,
    /// < timecode is drop frame
    AV_TIMECODE_FLAG_24HOURSMAX = 1 shl 1,
    /// < timecode wraps after 24 hours
    AV_TIMECODE_FLAG_ALLOWNEGATIVE = 1 shl 2
    /// < negative time values are allowed
    );

  pAVTimecode = ^AVTimecode;

  AVTimecode = record
    start: int;
    /// < timecode frame start (first base frame number)
    flags: uint32_t;
    /// < flags such as drop frame, +24 hours support, ...
    rate: AVRational;
    /// < frame rate in rational form
    fps: unsigned;
    /// < frame per second; must be consistent with the rate field
  end;

  (* *
    * Adjust frame number for NTSC drop frame time code.
    *
    * @param framenum frame number to adjust
    * @param fps      frame per second, 30 or 60
    * @return         adjusted frame number
    * @warning        adjustment is only valid in NTSC 29.97 and 59.94
  *)
  // int av_timecode_adjust_ntsc_framenum2(int framenum, int fps);
function av_timecode_adjust_ntsc_framenum2(framenum: int; fps: int): int; cdecl; external avutil_dll;
(* *
  * Convert frame number to SMPTE 12M binary representation.
  *
  * @param tc       timecode data correctly initialized
  * @param framenum frame number
  * @return         the SMPTE binary representation
  *
  * @note Frame number adjustment is automatically done in case of drop timecode,
  *       you do NOT have to call av_timecode_adjust_ntsc_framenum2().
  * @note The frame number is relative to tc->start.
  * @note Color frame (CF), binary group flags (BGF) and biphase mark polarity
  *       correction (PC) bits are set to zero.
*)
// uint32_t av_timecode_get_smpte_from_framenum(const AVTimecode *tc, int framenum);
function av_timecode_get_smpte_from_framenum(const tc: pAVTimecode; framenum: int): uint32_t; cdecl; external avutil_dll;
(* *
  * Load timecode string in buf.
  *
  * @param buf      destination buffer, must be at least AV_TIMECODE_STR_SIZE long
  * @param tc       timecode data correctly initialized
  * @param framenum frame number
  * @return         the buf parameter
  *
  * @note Timecode representation can be a negative timecode and have more than
  *       24 hours, but will only be honored if the flags are correctly set.
  * @note The frame number is relative to tc->start.
*)
// char *av_timecode_make_string(const AVTimecode *tc, char *buf, int framenum);
function av_timecode_make_string(const tc: pAVTimecode; buf: PAnsiChar; framenum: int): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Get the timecode string from the SMPTE timecode format.
  *
  * @param buf        destination buffer, must be at least AV_TIMECODE_STR_SIZE long
  * @param tcsmpte    the 32-bit SMPTE timecode
  * @param prevent_df prevent the use of a drop flag when it is known the DF bit
  *                   is arbitrary
  * @return           the buf parameter
*)
// char *av_timecode_make_smpte_tc_string(char *buf, uint32_t tcsmpte, int prevent_df);
function av_timecode_make_smpte_tc_string(buf: PAnsiChar; tcsmpte: uint32_t; prevent_df: int): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Get the timecode string from the 25-bit timecode format (MPEG GOP format).
  *
  * @param buf     destination buffer, must be at least AV_TIMECODE_STR_SIZE long
  * @param tc25bit the 25-bits timecode
  * @return        the buf parameter
*)
// char *av_timecode_make_mpeg_tc_string(char *buf, uint32_t tc25bit);
function av_timecode_make_mpeg_tc_string(buf: PAnsiChar; tc25bit: uint32_t): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Init a timecode struct with the passed parameters.
  *
  * @param log_ctx     a pointer to an arbitrary struct of which the first field
  *                    is a pointer to an AVClass struct (used for av_log)
  * @param tc          pointer to an allocated AVTimecode
  * @param rate        frame rate in rational form
  * @param flags       miscellaneous flags such as drop frame, +24 hours, ...
  *                    (see AVTimecodeFlag)
  * @param frame_start the first frame number
  * @return            0 on success, AVERROR otherwise
*)
// int av_timecode_init(AVTimecode *tc, AVRational rate, int flags, int frame_start, void *log_ctx);
function av_timecode_init(tc: pAVTimecode; rate: AVRational; flags: int; rame_start: int; log_ctx: Pointer): int; cdecl; external avutil_dll;
(* *
  * Parse timecode representation (hh:mm:ss[:;.]ff).
  *
  * @param log_ctx a pointer to an arbitrary struct of which the first field is a
  *                pointer to an AVClass struct (used for av_log).
  * @param tc      pointer to an allocated AVTimecode
  * @param rate    frame rate in rational form
  * @param str     timecode string which will determine the frame start
  * @return        0 on success, AVERROR otherwise
*)
// int av_timecode_init_from_string(AVTimecode *tc, AVRational rate, const char *str, void *log_ctx);
function av_timecode_init_from_string(tc: pAVTimecode; rate: AVRational; const str: PAnsiChar; log_ctx: Pointer): int; cdecl; external avutil_dll;
(* *
  * Check if the timecode feature is available for the given frame rate
  *
  * @return 0 if supported, <0 otherwise
*)
// int av_timecode_check_frame_rate(AVRational rate);
function av_timecode_check_frame_rate(rate: AVRational): int; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'mathematics.h'}

const

  M_E       = 2.7182818284590452354;  (* e *)
  M_LN2     = 0.69314718055994530942; (* log_e 2 *)
  M_LN10    = 2.30258509299404568402; (* log_e 10 *)
  M_LOG2_10 = 3.32192809488736234787; (* log_2 10 *)
  M_PHI     = 1.61803398874989484820; (* phi / golden ratio *)
  M_PI      = 3.14159265358979323846; (* pi *)
  M_PI_2    = 1.57079632679489661923; (* pi/2 *)
  M_SQRT1_2 = 0.70710678118654752440; (* 1/sqrt(2) *)
  M_SQRT2   = 1.41421356237309504880; (* sqrt(2) *)
  // NAN          =  av_int2float(0x7fc00000);
  // INFINITY     =  av_int2float(0x7f800000);

type
  AVRounding = int;

  (* *
    * Rounding methods.
  *)
const
  // AVRounding = ( //
  AV_ROUND_ZERO = 0;
  /// < Round toward zero.
  AV_ROUND_INF = 1;
  /// < Round away from zero.
  AV_ROUND_DOWN = 2;
  /// < Round toward -infinity.
  AV_ROUND_UP = 3;
  /// < Round toward +infinity.
  AV_ROUND_NEAR_INF = 5;
  /// < Round to nearest and halfway cases away from zero.
  (* *
    * Flag telling rescaling functions to pass `INT64_MIN`/`MAX` through
    * unchanged, avoiding special cases for #AV_NOPTS_VALUE.
    *
    * Unlike other values of the enumeration AVRounding, this value is a
    * bitmask that must be used in conjunction with another value of the
    * enumeration through a bitwise OR, in order to set behavior for normal
    * cases.
    *
    * @code{.c}
    * av_rescale_rnd(3, 1, 2, AV_ROUND_UP | AV_ROUND_PASS_MINMAX);
    * // Rescaling 3:
    * //     Calculating 3 * 1 / 2
    * //     3 / 2 is rounded up to 2
    * //     => 2
    *
    * av_rescale_rnd(AV_NOPTS_VALUE, 1, 2, AV_ROUND_UP | AV_ROUND_PASS_MINMAX);
    * // Rescaling AV_NOPTS_VALUE:
    * //     AV_NOPTS_VALUE == INT64_MIN
    * //     AV_NOPTS_VALUE is passed through
    * //     => AV_NOPTS_VALUE
    * @endcode
  *)
  AV_ROUND_PASS_MINMAX = 8192;
  // );

  (* *
    * Compute the greatest common divisor of two integer operands.
    *
    * @param a,b Operands
    * @return GCD of a and b up to sign; if a >= 0 and b >= 0, return value is >= 0;
    * if a == 0 and b == 0, returns 0.
  *)
  // int64_t av_const av_gcd(int64_t a, int64_t b);
function av_gcd(a, b: int64_t): int64_t; cdecl; external avutil_dll;
(* *
  * Rescale a 64-bit integer with rounding to nearest.
  *
  * The operation is mathematically equivalent to `a * b / c`, but writing that
  * directly can overflow.
  *
  * This function is equivalent to av_rescale_rnd() with #AV_ROUND_NEAR_INF.
  *
  * @see av_rescale_rnd(), av_rescale_q(), av_rescale_q_rnd()
*)
// int64_t av_rescale(int64_t a, int64_t b, int64_t c) av_const;
function av_rescale(a, b, c: int64_t): int64_t; cdecl; external avutil_dll;
(* *
  * Rescale a 64-bit integer with specified rounding.
  *
  * The operation is mathematically equivalent to `a * b / c`, but writing that
  * directly can overflow, and does not support different rounding methods.
  *
  * @see av_rescale(), av_rescale_q(), av_rescale_q_rnd()
*)
// int64_t av_rescale_rnd(int64_t a, int64_t b, int64_t c, enum AVRounding rnd) av_const;
function av_rescale_rnd(a, b, c: int64_t; rnd: AVRounding): int64_t; cdecl; external avutil_dll;
(* *
  * Rescale a 64-bit integer by 2 rational numbers.
  *
  * The operation is mathematically equivalent to `a * bq / cq`.
  *
  * This function is equivalent to av_rescale_q_rnd() with #AV_ROUND_NEAR_INF.
  *
  * @see av_rescale(), av_rescale_rnd(), av_rescale_q_rnd()
*)
// int64_t av_rescale_q(int64_t a, AVRational bq, AVRational cq) av_const;
function av_rescale_q(a: int64_t; bq: AVRational; cq: AVRational): int64_t; cdecl; external avutil_dll;
(* *
  * Rescale a 64-bit integer by 2 rational numbers with specified rounding.
  *
  * The operation is mathematically equivalent to `a * bq / cq`.
  *
  * @see av_rescale(), av_rescale_rnd(), av_rescale_q()
*)
// int64_t av_rescale_q_rnd(int64_t a, AVRational bq, AVRational cq, enum AVRounding rnd) av_const;
function av_rescale_q_rnd(a: int64_t; bq: AVRational; cq: AVRational; rnd: AVRounding): int64_t; cdecl; external avutil_dll;
(* *
  * Compare two timestamps each in its own time base.
  *
  * @return One of the following values:
  *         - -1 if `ts_a` is before `ts_b`
  *         - 1 if `ts_a` is after `ts_b`
  *         - 0 if they represent the same position
  *
  * @warning
  * The result of the function is undefined if one of the timestamps is outside
  * the `int64_t` range when represented in the other's timebase.
*)
// int av_compare_ts(int64_t ts_a, AVRational tb_a, int64_t ts_b, AVRational tb_b);
function av_compare_ts(ts_a: int64_t; tb_a: AVRational; ts_b: int64_t; tb_b: AVRational): int; cdecl; external avutil_dll;
(* *
  * Compare the remainders of two integer operands divided by a common divisor.
  *
  * In other words, compare the least significant `log2(mod)` bits of integers
  * `a` and `b`.
  *
  * @code{.c}
  * av_compare_mod(0x11, 0x02, 0x10) < 0 // since 0x11 % 0x10  (0x1) < 0x02 % 0x10  (0x2)
  * av_compare_mod(0x11, 0x02, 0x20) > 0 // since 0x11 % 0x20 (0x11) > 0x02 % 0x20 (0x02)
  * @endcode
  *
  * @param a,b Operands
  * @param mod Divisor; must be a power of 2
  * @return
  *         - a negative value if `a % mod < b % mod`
  *         - a positive value if `a % mod > b % mod`
  *         - zero             if `a % mod == b % mod`
*)
// int64_t av_compare_mod(uint64_t a, uint64_t b, uint64_t mod);
function av_compare_mod(a: uint64_t; b: uint64_t; _mod: uint64_t): int64_t; cdecl; external avutil_dll;
(* *
  * Rescale a timestamp while preserving known durations.
  *
  * This function is designed to be called per audio packet to scale the input
  * timestamp to a different time base. Compared to a simple av_rescale_q()
  * call, this function is robust against possible inconsistent frame durations.
  *
  * The `last` parameter is a state variable that must be preserved for all
  * subsequent calls for the same stream. For the first call, `*last` should be
  * initialized to #AV_NOPTS_VALUE.
  *
  * @param[in]     in_tb    Input time base
  * @param[in]     in_ts    Input timestamp
  * @param[in]     fs_tb    Duration time base; typically this is finer-grained
  *                         (greater) than `in_tb` and `out_tb`
  * @param[in]     duration Duration till the next call to this function (i.e.
  *                         duration of the current packet/frame)
  * @param[in,out] last     Pointer to a timestamp expressed in terms of
  *                         `fs_tb`, acting as a state variable
  * @param[in]     out_tb   Output timebase
  * @return        Timestamp expressed in terms of `out_tb`
  *
  * @note In the context of this function, "duration" is in term of samples, not
  *       seconds.
*)
// int64_t av_rescale_delta(AVRational in_tb, int64_t in_ts,  AVRational fs_tb, int duration, int64_t *last, AVRational out_tb);
function av_rescale_delta(in_tb: AVRational; in_ts: int64_t; fs_tb: AVRational; duration: int; var last: int64_t; out_tb: AVRational): int64_t; cdecl;
  external avutil_dll;
(* *
  * Add a value to a timestamp.
  *
  * This function guarantees that when the same value is repeatly added that
  * no accumulation of rounding errors occurs.
  *
  * @param[in] ts     Input timestamp
  * @param[in] ts_tb  Input timestamp time base
  * @param[in] inc    Value to be added
  * @param[in] inc_tb Time base of `inc`
*)
// int64_t av_add_stable(AVRational ts_tb, int64_t ts, AVRational inc_tb, int64_t inc);
function av_add_stable(ts_tb: AVRational; ts: int64_t; inc_tb: AVRational; inc: int64_t): int64_t; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'parseutils.h'}
(* *
  * Parse str and store the parsed ratio in q.
  *
  * Note that a ratio with infinite (1/0) or negative value is
  * considered valid, so you should check on the returned value if you
  * want to exclude those values.
  *
  * The undefined value can be expressed using the "0:0" string.
  *
  * @param[in,out] q pointer to the AVRational which will contain the ratio
  * @param[in] str the string to parse: it has to be a string in the format
  * num:den, a float number or an expression
  * @param[in] max the maximum allowed numerator and denominator
  * @param[in] log_offset log level offset which is applied to the log
  * level of log_ctx
  * @param[in] log_ctx parent logging context
  * @return >= 0 on success, a negative error code otherwise
*)
// int av_parse_ratio(AVRational *q, const char *str, int max, int log_offset, void *log_ctx);
function av_parse_ratio(q: pAVRational; const str: PAnsiChar; max, log_offset: int; log_ctx: Pointer): int; cdecl; external avutil_dll;

// #define av_parse_ratio_quiet(rate, str, max) av_parse_ratio(rate, str, max, AV_LOG_MAX_OFFSET, NULL)
function av_parse_ratio_quiet(q: pAVRational; const str: PAnsiChar; max: int): int; inline;

(* *
  * Parse str and put in width_ptr and height_ptr the detected values.
  *
  * @param[in,out] width_ptr pointer to the variable which will contain the detected
  * width value
  * @param[in,out] height_ptr pointer to the variable which will contain the detected
  * height value
  * @param[in] str the string to parse: it has to be a string in the format
  * width x height or a valid video size abbreviation.
  * @return >= 0 on success, a negative error code otherwise
*)
// int av_parse_video_size(int *width_ptr, int *height_ptr, const char *str);
function av_parse_video_size(var width_ptr: int; var height_ptr: int; const str: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * Parse str and store the detected values in *rate.
  *
  * @param[in,out] rate pointer to the AVRational which will contain the detected
  * frame rate
  * @param[in] str the string to parse: it has to be a string in the format
  * rate_num / rate_den, a float number or a valid video rate abbreviation
  * @return >= 0 on success, a negative error code otherwise
*)
// int av_parse_video_rate(AVRational *rate, const char *str);
function av_parse_video_rate(rate: pAVRational; const str: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * Put the RGBA values that correspond to color_string in rgba_color.
  *
  * @param color_string a string specifying a color. It can be the name of
  * a color (case insensitive match) or a [0x|#]RRGGBB[AA] sequence,
  * possibly followed by "@" and a string representing the alpha
  * component.
  * The alpha component may be a string composed by "0x" followed by an
  * hexadecimal number or a decimal number between 0.0 and 1.0, which
  * represents the opacity value (0x00/0.0 means completely transparent,
  * 0xff/1.0 completely opaque).
  * If the alpha component is not specified then 0xff is assumed.
  * The string "random" will result in a random color.
  * @param slen length of the initial part of color_string containing the
  * color. It can be set to -1 if color_string is a null terminated string
  * containing nothing else than the color.
  * @return >= 0 in case of success, a negative value in case of
  * failure (for example if color_string cannot be parsed).
*)
// int av_parse_color(uint8_t *rgba_color, const char *color_string, int slen, void *log_ctx);
function av_parse_color(rgba_color: puint8_t; const color_string: PAnsiChar; slen: int; log_ctx: Pointer): int; cdecl; external avutil_dll;
(* *
  * Get the name of a color from the internal table of hard-coded named
  * colors.
  *
  * This function is meant to enumerate the color names recognized by
  * av_parse_color().
  *
  * @param color_idx index of the requested color, starting from 0
  * @param rgbp      if not NULL, will point to a 3-elements array with the color value in RGB
  * @return the color name string or NULL if color_idx is not in the array
*)
// const char *av_get_known_color_name(int color_idx, const uint8_t **rgb);
function av_get_known_color_name(color_idx: int; const rgb: ppuint8_t): PAnsiChar; cdecl; external avutil_dll;
(* *
  * Parse timestr and return in *time a corresponding number of
  * microseconds.
  *
  * @param timeval puts here the number of microseconds corresponding
  * to the string in timestr. If the string represents a duration, it
  * is the number of microseconds contained in the time interval.  If
  * the string is a date, is the number of microseconds since 1st of
  * January, 1970 up to the time of the parsed date.  If timestr cannot
  * be successfully parsed, set *time to INT64_MIN.

  * @param timestr a string representing a date or a duration.
  * - If a date the syntax is:
  * @code
  * [{YYYY-MM-DD|YYYYMMDD}[T|t| ]]{{HH:MM:SS[.m...]]]}|{HHMMSS[.m...]]]}}[Z]
  * now
  * @endcode
  * If the value is "now" it takes the current time.
  * Time is local time unless Z is appended, in which case it is
  * interpreted as UTC.
  * If the year-month-day part is not specified it takes the current
  * year-month-day.
  * - If a duration the syntax is:
  * @code
  * [-][HH:]MM:SS[.m...]
  * [-]S+[.m...]
  * @endcode
  * @param duration flag which tells how to interpret timestr, if not
  * zero timestr is interpreted as a duration, otherwise as a date
  * @return >= 0 in case of success, a negative value corresponding to an
  * AVERROR code otherwise
*)
// int av_parse_time(int64_t *timeval, const char *timestr, int duration);
function av_parse_time(timeval: pint64_t; const timestr: PAnsiChar; duration: int): int; cdecl; external avutil_dll;
(* *
  * Attempt to find a specific tag in a URL.
  *
  * syntax: '?tag1=val1&tag2=val2...'. Little URL decoding is done.
  * Return 1 if found.
*)
// int av_find_info_tag(char *arg, int arg_size, const char *tag1, const char *info);
function av_find_info_tag(arg: PAnsiChar; arg_size: int; const tag1: PAnsiChar; const info: PAnsiChar): int; cdecl; external avutil_dll;
(* *
  * Simplified version of strptime
  *
  * Parse the input string p according to the format string fmt and
  * store its results in the structure dt.
  * This implementation supports only a subset of the formats supported
  * by the standard strptime().
  *
  * The supported input field descriptors are listed below.
  * - %H: the hour as a decimal number, using a 24-hour clock, in the
  *   range '00' through '23'
  * - %J: hours as a decimal number, in the range '0' through INT_MAX
  * - %M: the minute as a decimal number, using a 24-hour clock, in the
  *   range '00' through '59'
  * - %S: the second as a decimal number, using a 24-hour clock, in the
  *   range '00' through '59'
  * - %Y: the year as a decimal number, using the Gregorian calendar
  * - %m: the month as a decimal number, in the range '1' through '12'
  * - %d: the day of the month as a decimal number, in the range '1'
  *   through '31'
  * - %T: alias for '%H:%M:%S'
  * - %%: a literal '%'
  *
  * @return a pointer to the first character not processed in this function
  *         call. In case the input string contains more characters than
  *         required by the format string the return value points right after
  *         the last consumed input character. In case the whole input string
  *         is consumed the return value points to the null byte at the end of
  *         the string. On failure NULL is returned.
*)
// char *av_small_strptime(const char *p, const char *fmt, struct tm *dt);
function av_small_strptime(const p: PAnsiChar; const fmt: PAnsiChar; dt: ptm): PAnsiChar; cdecl; external avutil_dll;

(* *
  * Convert the decomposed UTC time in tm to a time_t value.
*)
// time_t av_timegm(struct tm *tm);
function av_timegm(tm: ptm): time_t; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'motion_vector.h'}

type
  pAVMotionVector = ^AVMotionVector;

  AVMotionVector = record
    (* *
      * Where the current macroblock comes from; negative value when it comes
      * from the past, positive value when it comes from the future.
      * XXX: set exact relative ref frame reference instead of a +/- 1 "direction".
    *)
    source: int32_t;
    (* *
      * Width and height of the block.
    *)
    w, h: uint8_t;
    (* *
      * Absolute source position. Can be outside the frame area.
    *)
    src_x, src_y: int16_t;
    (* *
      * Absolute destination position. Can be outside the frame area.
    *)
    dst_x, dst_y: int16_t;
    (* *
      * Extra flag information.
      * Currently unused.
    *)
    flags: uint64_t;
    (* *
      * Motion vector
      * src_x = dst_x + motion_x / motion_scale
      * src_y = dst_y + motion_y / motion_scale
    *)
    motion_x, motion_y: int32_t;
    motion_scale: uint16_t;
  end;
{$ENDREGION}
{$REGION 'md5.h'}

type
  pAVMD5 = ^AVMD5;

  AVMD5 = record

  end;

  (* *
    * Allocate an AVMD5 context.
  *)
  // struct AVMD5 *av_md5_alloc(void);
function av_md5_alloc(): pAVMD5; cdecl; external avutil_dll;

(* *
  * Initialize MD5 hashing.
  *
  * @param ctx pointer to the function context (of size av_md5_size)
*)
// void av_md5_init(struct AVMD5 *ctx);
procedure av_md5_init(ctx: pAVMD5); cdecl; external avutil_dll;

(* *
  * Update hash value.
  *
  * @param ctx hash function context
  * @param src input data to update hash with
  * @param len input data length
*)

// #if FF_API_CRYPTO_SIZE_T
// void av_md5_update(struct AVMD5 *ctx, const uint8_t *src, int len);
// #else
// void av_md5_update(struct AVMD5 *ctx, const uint8_t *src, size_t len);
// #endif
procedure av_md5_update(ctx: pAVMD5; const src: puint8_t; len:
{$IFDEF FF_API_CRYPTO_SIZE_T}
  int
{$ELSE}
  size_t
{$ENDIF}
  ); cdecl; external avutil_dll;

(* *
  * Finish hashing and output digest value.
  *
  * @param ctx hash function context
  * @param dst buffer where output digest value is stored
*)
// void av_md5_final(struct AVMD5 *ctx, uint8_t *dst);
procedure av_md5_final(ctx: pAVMD5; dst: puint8_t); cdecl; external avutil_dll;

(* *
  * Hash an array of data.
  *
  * @param dst The output buffer to write the digest into
  * @param src The data to hash
  * @param len The length of the data, in bytes
*)

// #if FF_API_CRYPTO_SIZE_T
// void av_md5_sum(uint8_t *dst, const uint8_t *src, const int len);
// #else
// void av_md5_sum(uint8_t *dst, const uint8_t *src, size_t len);
// #endif
procedure av_md5_sum(dst: puint8_t; const src: puint8_t; const len:
{$IFDEF FF_API_CRYPTO_SIZE_T}
  int
{$ELSE}
  size_t
{$ENDIF}
  ); cdecl; external avutil_dll;

{$ENDREGION}
{$REGION 'avassert.h'}
(* *
  * Assert that floating point opperations can be executed.
  *
  * This will av_assert0() that the cpu is not in MMX state on X86
*)
// void av_assert0_fpu(void);
procedure av_assert0_fpu(); cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'intfloat.h'}

type
  av_intfloat32 = record
    case Integer of
      0:
        (i: uint32_t);
      1:
        (f: float);
  end;

  av_intfloat64 = record
    case Integer of
      0:
        (i: uint64_t);
      1:
        (f: double);
  end;

  (* *
    * Reinterpret a 32-bit integer as a float.
  *)
  // static av_always_inline float av_int2float(uint32_t i)
function av_int2float(i: uint32_t): float; inline;

(* *
  * Reinterpret a float as a 32-bit integer.
*)
// static av_always_inline uint32_t av_float2int(float f)
function av_float2int(f: float): uint32_t; inline;

(* *
  * Reinterpret a 64-bit integer as a double.
*)
// static av_always_inline double av_int2double(uint64_t i)
function av_int2double(i: uint64_t): double; inline;

(* *
  * Reinterpret a double as a 64-bit integer.
*)
// static av_always_inline uint64_t av_double2int(double f)
function av_double2int(f: double): uint64_t; inline;

{$ENDREGION}
{$REGION 'mastering_display_metadata.h'}

type
  (* *
    * Mastering display metadata capable of representing the color volume of
    * the display used to master the content (SMPTE 2086:2014).
    *
    * To be used as payload of a AVFrameSideData or AVPacketSideData with the
    * appropriate type.
    *
    * @note The struct should be allocated with av_mastering_display_metadata_alloc()
    *       and its size is not a part of the public ABI.
  *)
  pAVMasteringDisplayMetadata = ^AVMasteringDisplayMetadata;

  AVMasteringDisplayMetadata = record
    (* *
      * CIE 1931 xy chromaticity coords of color primaries (r, g, b order).
    *)
    display_primaries: array [0 .. 2, 0 .. 1] of AVRational;

    (* *
      * CIE 1931 xy chromaticity coords of white point.
    *)
    white_point: array [0 .. 1] of AVRational;

    (* *
      * Min luminance of mastering display (cd/m^2).
    *)
    min_luminance: AVRational;

    (* *
      * Max luminance of mastering display (cd/m^2).
    *)
    max_luminance: AVRational;

    (* *
      * Flag indicating whether the display primaries (and white point) are set.
    *)
    has_primaries: int;

    (* *
      * Flag indicating whether the luminance (min_ and max_) have been set.
    *)
    has_luminance: int;
  end;

  (* *
    * Allocate an AVMasteringDisplayMetadata structure and set its fields to
    * default values. The resulting struct can be freed using av_freep().
    *
    * @return An AVMasteringDisplayMetadata filled with default values or NULL
    *         on failure.
  *)
  // AVMasteringDisplayMetadata *av_mastering_display_metadata_alloc(void);
function av_mastering_display_metadata_alloc(): pAVMasteringDisplayMetadata; cdecl; external avutil_dll;
(* *
  * Allocate a complete AVMasteringDisplayMetadata and add it to the frame.
  *
  * @param frame The frame which side data is added to.
  *
  * @return The AVMasteringDisplayMetadata structure to be filled by caller.
*)
// AVMasteringDisplayMetadata *av_mastering_display_metadata_create_side_data(AVFrame *frame);
function av_mastering_display_metadata_create_side_data(frame: pAVFrame): pAVMasteringDisplayMetadata; cdecl; external avutil_dll;

type
  (* *
    * Content light level needed by to transmit HDR over HDMI (CTA-861.3).
    *
    * To be used as payload of a AVFrameSideData or AVPacketSideData with the
    * appropriate type.
    *
    * @note The struct should be allocated with av_content_light_metadata_alloc()
    *       and its size is not a part of the public ABI.
  *)
  pAVContentLightMetadata = ^AVContentLightMetadata;

  AVContentLightMetadata = record
    (* *
      * Max content light level (cd/m^2).
    *)
    MaxCLL: unsigned;
    (* *
      * Max average light level per frame (cd/m^2).
    *)
    MaxFALL: unsigned;
  end;

  (* *
    * Allocate an AVContentLightMetadata structure and set its fields to
    * default values. The resulting struct can be freed using av_freep().
    *
    * @return An AVContentLightMetadata filled with default values or NULL
    *         on failure.
  *)
  // AVContentLightMetadata *av_content_light_metadata_alloc(size_t *size);
function av_content_light_metadata_alloc(var size: size_t): pAVContentLightMetadata; cdecl; external avutil_dll;
(* *
  * Allocate a complete AVContentLightMetadata and add it to the frame.
  *
  * @param frame The frame which side data is added to.
  *
  * @return The AVContentLightMetadata structure to be filled by caller.
*)
// AVContentLightMetadata *av_content_light_metadata_create_side_data(AVFrame *frame);
function av_content_light_metadata_create_side_data(frame: pAVFrame): pAVContentLightMetadata; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'pixelutils.h'}

(* *
  * Sum of abs(src1[x] - src2[x])
*)
type
  // int (*av_pixelutils_sad_fn)(const uint8_t *src1, ptrdiff_t stride1,
  // const uint8_t *src2, ptrdiff_t stride2);
  av_pixelutils_sad_fn = function(const src1: puint8_t; stride1: ptrdiff_t; const src2: puint8_t; stride2: ptrdiff_t): int; cdecl;

  (* *
    * Get a potentially optimized pointer to a Sum-of-absolute-differences
    * function (see the av_pixelutils_sad_fn prototype).
    *
    * @param w_bits  1<<w_bits is the requested width of the block size
    * @param h_bits  1<<h_bits is the requested height of the block size
    * @param aligned If set to 2, the returned sad function will assume src1 and
    *                src2 addresses are aligned on the block size.
    *                If set to 1, the returned sad function will assume src1 is
    *                aligned on the block size.
    *                If set to 0, the returned sad function assume no particular
    *                alignment.
    * @param log_ctx context used for logging, can be NULL
    *
    * @return a pointer to the SAD function or NULL in case of error (because of
    *         invalid parameters)
  *)
  // av_pixelutils_sad_fn av_pixelutils_get_sad_fn(int w_bits, int h_bits, int aligned, void *log_ctx);
function av_pixelutils_get_sad_fn(w_bits: int; h_bits: int; aligned: int; log_ctx: Pointer): av_pixelutils_sad_fn; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'random_seed.h'}
(* *
  * Get a seed to use in conjunction with random functions.
  * This function tries to provide a good seed at a best effort bases.
  * Its possible to call this function multiple times if more bits are needed.
  * It can be quite slow, which is why it should only be used as seed for a faster
  * PRNG. The quality of the seed depends on the platform.
*)
// uint32_t av_get_random_seed(void);
function av_get_random_seed(): uint32_t; cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'aes.h'}

Type
  pAVAES = ^AVAES;

  AVAES = record
  end;

  (* *
    * Allocate an AVAES context.
  *)
  // struct AVAES *av_aes_alloc(void);
function av_aes_alloc: pAVAES; cdecl; external avutil_dll;
(* *
  * Initialize an AVAES context.
  * @param key_bits 128, 192 or 256
  * @param decrypt 0 for encryption, 1 for decryption
*)
// int av_aes_init(struct AVAES *a, const uint8_t *key, int key_bits, int decrypt);
function av_aes_init(a: pAVAES; const key: puint8_t; key_bits: int; decrypt: int): int; cdecl; external avutil_dll;
(* *
  * Encrypt or decrypt a buffer using a previously initialized context.
  * @param count number of 16 byte blocks
  * @param dst destination array, can be equal to src
  * @param src source array, can be equal to dst
  * @param iv initialization vector for CBC mode, if NULL then ECB will be used
  * @param decrypt 0 for encryption, 1 for decryption
*)
// void av_aes_crypt(struct AVAES *a, uint8_t *dst, const uint8_t *src, int count, uint8_t *iv, int decrypt);
procedure av_aes_crypt(a: pAVAES; dst: puint8_t; const src: puint8_t; count: int; iv: puint8_t; decrypt: int); cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'twofish.h'}

type
  pAVTWOFISH = ^AVTWOFISH;

  AVTWOFISH = record

  end;

  (* *
    * Allocate an AVTWOFISH context
    * To free the struct: av_free(ptr)
  *)
  // struct AVTWOFISH *av_twofish_alloc(void);
function av_twofish_alloc: pAVTWOFISH; cdecl; external avutil_dll;

(* *
  * Initialize an AVTWOFISH context.
  *
  * @param ctx an AVTWOFISH context
  * @param key a key of size ranging from 1 to 32 bytes used for encryption/decryption
  * @param key_bits number of keybits: 128, 192, 256 If less than the required, padded with zeroes to nearest valid value; return value is 0 if key_bits is 128/192/256, -1 if less than 0, 1 otherwise
*)
// int av_twofish_init(struct AVTWOFISH *ctx, const uint8_t *key, int key_bits);
function av_twofish_init(ctx: pAVTWOFISH; const key: puint8_t; key_bits: int): int; cdecl; external avutil_dll;

(* *
  * Encrypt or decrypt a buffer using a previously initialized context
  *
  * @param ctx an AVTWOFISH context
  * @param dst destination array, can be equal to src
  * @param src source array, can be equal to dst
  * @param count number of 16 byte blocks
  * @paran iv initialization vector for CBC mode, NULL for ECB mode
  * @param decrypt 0 for encryption, 1 for decryption
*)
// void av_twofish_crypt(struct AVTWOFISH *ctx, uint8_t *dst, const uint8_t *src, int count, uint8_t* iv, int decrypt);
procedure av_twofish_crypt(ctx: pAVTWOFISH; dst: puint8_t; const src: puint8_t; count: int; iv: puint8_t; decrypt: int); cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'tea.h'}

type
  pAVTEA = ^AVTEA;

  AVTEA = record
  end;

  (* *
    * Allocate an AVTEA context
    * To free the struct: av_free(ptr)
  *)
  // struct AVTEA *av_tea_alloc(void);
function av_tea_alloc: pAVTEA; cdecl; external avutil_dll;

(* *
  * Initialize an AVTEA context.
  *
  * @param ctx an AVTEA context
  * @param key a key of 16 bytes used for encryption/decryption
  * @param rounds the number of rounds in TEA (64 is the "standard")
*)
// void av_tea_init(struct AVTEA *ctx, const uint8_t key[16], int rounds);
procedure av_tea_init(ctx: pAVTEA; const key: puint8_t; rounds: int); cdecl; external avutil_dll;

(* *
  * Encrypt or decrypt a buffer using a previously initialized context.
  *
  * @param ctx an AVTEA context
  * @param dst destination array, can be equal to src
  * @param src source array, can be equal to dst
  * @param count number of 8 byte blocks
  * @param iv initialization vector for CBC mode, if NULL then ECB will be used
  * @param decrypt 0 for encryption, 1 for decryption
*)
// void av_tea_crypt(struct AVTEA *ctx, uint8_t *dst, const uint8_t *src,
// int count, uint8_t *iv, int decrypt);
procedure av_tea_crypt(ctx: pAVTEA; dst: puint8_t; const src: puint8_t; count: int; iv: puint8_t; decrypt: int); cdecl; external avutil_dll;

{$ENDREGION}
{$REGION 'hwcontext_videotoolbox.h'}
(*
  * Convert a VideoToolbox (actually CoreVideo) format to AVPixelFormat.
  * Returns AV_PIX_FMT_NONE if no known equivalent was found.
*)
// enum AVPixelFormat av_map_videotoolbox_format_to_pixfmt(uint32_t cv_fmt);
function av_map_videotoolbox_format_to_pixfmt(cv_fmt: uint32_t): AVPixelFormat; cdecl; external avutil_dll;

(*
  * Convert an AVPixelFormat to a VideoToolbox (actually CoreVideo) format.
  * Returns 0 if no known equivalent was found.
*)
// uint32_t av_map_videotoolbox_format_from_pixfmt(enum AVPixelFormat pix_fmt);
function av_map_videotoolbox_format_from_pixfmt(pix_fmt: AVPixelFormat): uint32_t; cdecl; external avutil_dll;

(*
  * Same as av_map_videotoolbox_format_from_pixfmt function, but can map and
  * return full range pixel formats via a flag.
*)
// uint32_t av_map_videotoolbox_format_from_pixfmt2(enum AVPixelFormat pix_fmt, bool full_range);
// function av_map_videotoolbox_format_from_pixfmt2(pix_fmt: AVPixelFormat; full_range: bool): uint32_t; cdecl; external avutil_dll; //4.2.2

{$ENDREGION}
{$REGION 'tx.h'}

// typedef struct AVTXContext AVTXContext;
Type
  AVTXContext = record
  end;

  pAVTXContext = ^AVTXContext;

  AVComplexFloat = record
    re, im: float;
  end;

  // 4.2.2
  // AVComplexDouble = record
  // re, im: double;
  // end;

  AVTXType = (
    (*
      * Standard complex to complex FFT with sample data type AVComplexFloat.
      * Scaling currently unsupported
    *)
    AV_TX_FLOAT_FFT = 0,
    (*
      * Standard MDCT with sample data type of float and a scale type of
      * float. Length is the frame size, not the window size (which is 2x frame)
    *)
    AV_TX_FLOAT_MDCT = 1

    // 4.2.2
    // ,
    // (*
    // * Same as AV_TX_FLOAT_FFT with a data type of AVComplexDouble.
    // *)
    // AV_TX_DOUBLE_FFT = 2,
    // (*
    // * Same as AV_TX_FLOAT_MDCT with data and scale type of double.
    // *)
    // AV_TX_DOUBLE_MDCT = 3 //
    );

  (*
    * Function pointer to a function to perform the transform.
    *
    * @note Using a different context than the one allocated during av_tx_init()
    * is not allowed.
    *
    * @param s the transform context
    * @param out the output array
    * @param in the input array
    * @param stride the input or output stride (depending on transform direction)
    * in bytes, currently implemented for all MDCT transforms
  *)
  // typedef void (*av_tx_fn)(AVTXContext *s, void *out, void *in, ptrdiff_t stride);
  av_tx_fn = procedure(s: pAVTXContext; &out, &in: Pointer; stride: ptrdiff_t); cdecl;

  (*
    * Initialize a transform context with the given configuration
    * Currently power of two lengths from 4 to 131072 are supported, along with
    * any length decomposable to a power of two and either 3, 5 or 15.
    *
    * @param ctx the context to allocate, will be NULL on error
    * @param tx pointer to the transform function pointer to set
    * @param type type the type of transform
    * @param inv whether to do an inverse or a forward transform
    * @param len the size of the transform in samples
    * @param scale pointer to the value to scale the output if supported by type
    * @param flags currently unused
    *
    * @return 0 on success, negative error code on failure
  *)
  // int av_tx_init(AVTXContext **ctx, av_tx_fn *tx, enum AVTXType type, int inv, int len, const void *scale, uint64_t flags);
function av_tx_init(Var ctx: pAVTXContext; tx: av_tx_fn; &type: AVTXType; inv, len: int; const scale: Pointer; flags: uint64_t): int; cdecl;
  external avutil_dll;

(*
  * Frees a context and sets ctx to NULL, does nothing when ctx == NULL
*)
// void av_tx_uninit(AVTXContext **ctx);
procedure av_tx_uninit(Var ctx: pAVTXContext); cdecl; external avutil_dll;
{$ENDREGION}
{$REGION 'lfg.h'}

(* *
  * Context structure for the Lagged Fibonacci PRNG.
  * The exact layout, types and content of this struct may change and should
  * not be accessed directly. Only its sizeof() is guranteed to stay the same
  * to allow easy instanciation.
*)
type
  pAVLFG = ^AVLFG;

  AVLFG = record
    state: array [0 .. 63] of uint;
    index: int;
  end;

  // void av_lfg_init(AVLFG *c, unsigned int seed);
procedure av_lfg_init(c: pAVLFG; seed: uint); cdecl; external avutil_dll;

(* *
  * Seed the state of the ALFG using binary data.
  *
  * Return value: 0 on success, negative value (AVERROR) on failure.
*)
// int av_lfg_init_from_data(AVLFG *c, const uint8_t *data, unsigned int length);
function av_lfg_init_from_data(c: pAVLFG; const data: puint8_t; length: uint): int; cdecl; external avutil_dll;

(* *
  * Get the next random unsigned 32-bit number using an ALFG.
  *
  * Please also consider a simple LCG like state= state*1664525+1013904223,
  * it may be good enough and faster for your specific use case.
*)
// static inline unsigned int av_lfg_get(AVLFG *c){
// unsigned a = c->state[c->index & 63] = c->state[(c->index-24) & 63] + c->state[(c->index-55) & 63];
// c->index += 1U;
// return a;
// }
function av_lfg_get(c: pAVLFG): uint; inline;

(* *
  * Get the next random unsigned 32-bit number using a MLFG.
  *
  * Please also consider av_lfg_get() above, it is faster.
*)
// static inline unsigned int av_mlfg_get(AVLFG *c){
// unsigned int a= c->state[(c->index-55) & 63];
// unsigned int b= c->state[(c->index-24) & 63];
// a = c->state[c->index & 63] = 2*a*b+a+b;
// c->index += 1U;
// return a;
// }

function av_mlfg_get(c: pAVLFG): uint; inline;

(* *
  * Get the next two numbers generated by a Box-Muller Gaussian
  * generator using the random numbers issued by lfg.
  *
  * @param out array where the two generated numbers are placed
*)
// void av_bmg_get(AVLFG *lfg, double out[2]);
Type
  Tav_bmg_get_arrayofdouble = array [0 .. 1] of double;

procedure av_bmg_get(lfg: pAVLFG; &out: Tav_bmg_get_arrayofdouble); cdecl; external avutil_dll;
{$ENDREGION}

implementation

{$REGION 'common.h'}

function RSHIFT(a, b: int): int; inline;
begin
  if a > 0 then
    Result := ((a) + ((1 shl (b)) shr 1)) shr (b)
  else
    Result := ((a) + ((1 shl (b)) shr 1) - 1) shr (b);
end;

function ROUNDED_DIV(a, b: int): int; inline;
begin
  if a > 0 then
    Result := a + (b shr 1)
  else
    Result := a - (b shr 1) div b;
end;

function FFUDIV(a, b: int): int; inline;
begin
  if a > 0 then
    Result := a
  else
    Result := a - b + 1;
  Result := Result div b;

end;

function FFUMOD(a, b: int): int; inline;
begin
  Result := a - b * FFUDIV(a, b);
end;

function FFABS(a: int): int; inline;
begin
  if a >= 0 then
    Result := a
  else
    Result := -a;
end;

function FFSIGN(a: int): int; inline;
begin
  if a > 0 then
    Result := 1
  else
    Result := -1;
end;

function FFNABS(a: int): int; inline;
begin
  if a <= 0 then
    Result := a
  else
    Result := -a;
end;

function FFDIFFSIGN(x, y: int): Boolean; inline;
begin
  Result := FFSIGN(x) <> FFSIGN(y);
end;

function FFMAX(a, b: int): int; inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function av_clip_c(a: int; amin: int; amax: int): int; inline;
begin
  // #if defined(HAVE_AV_CONFIG_H) && defined(ASSERT_LEVEL) && ASSERT_LEVEL >= 2
  // if (amin > amax) abort();
  // #endif
  if (a < amin) then
    Result := amin
  else if (a > amax) then
    Result := amax
  else
    Result := a;
end;

function av_clip64_c(a: int64_t; amin: int64_t; amax: int64_t): int64_t; inline;
begin
  // #if defined(HAVE_AV_CONFIG_H) && defined(ASSERT_LEVEL) && ASSERT_LEVEL >= 2
  // if (amin > amax) abort();
  // #endif
  if (a < amin) then
    Result := amin
  else if (a > amax) then
    Result := amax
  else
    Result := a;
end;

function av_clip_uint8_c(a: int): uint8_t; inline;
begin
  // if (a&(~0xFF)) return (~a)>>31;
  // else           return a;
  if (a and (not $FF)) <> 0 then
    Result := (not a) shr 31
  else
    Result := a;
end;

function av_clip_int8_c(a: int): int8_t; inline;
begin
  if ((a + $80) and (not $FF)) <> 0 then
    Result := (a shr 31) xor $7F
  else
    Result := a;
end;

function av_clip_uint16_c(a: int): uint16_t; inline;
begin
  if (a and (not $FFFF)) <> 0 then
    Result := (not a) shr 31
  else
    Result := a;
end;

function av_clip_int16_c(a: int): int16_t; inline;
begin
  if ((a + $8000) and (not $FFFF)) <> 0 then
    Result := (a shr 31) xor $7FFF
  else
    Result := a;
end;

function av_clipl_int32_c(a: int64_t): int32_t; inline;
begin
  if ((a + $80000000) and (not $FFFFFFFF)) <> 0 then
    Result := ((a shr 63) xor $7FFFFFFF)
  else
    Result := a;
end;

function av_clip_intp2_c(a: int; p: int): int; inline;
begin
  if ((a + (1 shl p)) and (not((2 shl p) - 1))) <> 0 then
    Result := (a shr 31) xor ((1 shl p) - 1)
  else
    Result := a;
end;

function av_clip_uintp2_c(a: int; p: int): uint; inline;
begin
  if (a and (not((1 shl p) - 1))) <> 0 then
    Result := (not a) shr 31 and ((1 shl p) - 1)
  else
    Result := a;
end;

function av_mod_uintp2_c(a: uint; p: uint): uint; inline;
begin
  Result := a and ((uint(1) shl p) - 1);
end;

function av_sat_add32_c(a: int; b: int): int; inline;
begin
  Result := av_clipl_int32_c(a + b);
end;

function av_sat_dadd32_c(a: int; b: int): int; inline;
begin
  Result := av_sat_add32_c(a, av_sat_add32_c(b, b));
end;

function av_sat_sub32_c(a: int; b: int): int; inline;
begin
  Result := av_clipl_int32_c(a - b);
end;

function av_sat_dsub32_c(a: int; b: int): int; inline;
begin
  Result := av_sat_sub32_c(a, av_sat_add32_c(b, b));
end;

function av_clipf_c(a: float; amin: float; amax: float): float; inline;
begin
  if a < amin then
    Result := amin
  else if a > amax then
    Result := amax
  else
    Result := a;
end;

function av_clipd_c(a: double; amin: double; amax: double): double; inline;
begin
  if a < amin then
    Result := amin
  else if a > amax then
    Result := amax
  else
    Result := a;
end;

function av_ceil_log2_c(x: int): int; inline;
begin
  Result := av_log2((x - 1) shl 1);
end;

function av_popcount_c(x: uint32_t): int; inline;
begin
  x := x - ((x shr 1) and $55555555);
  x := (x and $33333333) + ((x shr 2) and $33333333);
  x := (x + (x shr 4)) and $0F0F0F0F;
  x := x + (x shr 8);
  Result := (x + (x shr 16)) and $3F;
end;

function av_popcount64_c(x: uint64_t): int; inline;
begin
  Result := av_popcount_c(x) + av_popcount_c(x shr 32);
end;

function av_parity_c(v: uint32_t): int; inline;
begin
  Result := av_popcount_c(v) and 1;
end;

{$ENDREGION}
{$REGION 'rational.h'}

function av_make_q(_num: int; _den: int): AVRational; inline;
begin
  Result.num := _num;
  Result.den := _den;
end;

function av_cmp_q(a, b: AVRational): int; inline;
Var
  tmp: int64_t;
begin
  tmp := a.num * b.den - b.num * a.den;
  if (tmp <> 0) then
    Result := ((tmp xor a.den xor b.den) shr 63) or 1
  else if (b.den and a.den) <> 0 then
    Result := 0
  else if (a.num and b.num) <> 0 then
    Result := (a.num shr 31) - (b.num shr 31)
  else
    Result := -MaxInt;
end;

function av_q2d(a: AVRational): double; inline;
begin
  Result := a.num / a.den;
end;

function av_inv_q(q: AVRational): AVRational; inline;
begin
  Result.den := q.den;
  Result.num := q.num;
end;

function av_x_if_null(const p: Pointer; const x: Pointer): Pointer; inline;
begin
  // return (void *)(intptr_t)(p ? p : x);
  if Assigned(p) then
    Result := p
  else
    Result := x;
end;
{$ENDREGION}
{$REGION 'opt.h'}

function av_opt_set_int_list(obj: Pointer; name: PAnsiChar; list: Pointer; item_size: int; term: int64_t; flags: int): Integer; inline;
begin
  if av_int_list_length(list, item_size, term) > MaxInt / item_size then
    Result := AVERROR_EINVAL
  else
    Result := av_opt_set_bin(obj, name, puint8_t(list), av_int_list_length(list, item_size, term) * item_size, flags);
end;

function av_int_list_length(list: Pointer; item_size: int; term: int64_t): int; inline;
begin
  Result := av_int_list_length_for_size(item_size, list, term);
end;
{$ENDREGION}
{$REGION 'error.h'}

function av_make_error_string(errbuf: PAnsiChar; errbuf_size: size_t; errnum: int): PAnsiChar;
begin
  av_strerror(errnum, @errbuf, errbuf_size);
  Result := @errbuf;
end;

var
  error_str: array [0 .. AV_ERROR_MAX_STRING_SIZE - 1] of AnsiChar;

function av_err2str(errnum: int): PAnsiChar;
begin
  FillChar(error_str, SizeOf(error_str), 0);
  av_make_error_string(@error_str, AV_ERROR_MAX_STRING_SIZE, errnum);
  Result := @error_str;
end;

{$ENDREGION}
{$REGION 'avstring.h'}

function av_strnlen(const s: PAnsiChar; len: size_t): size_t; inline;
begin
  Result := 0;
  While s[Result] <> #0 do
    inc(Result);
end;

function av_isdigit(c: int): Boolean; inline;
begin
  Result := (AnsiChar(c) >= '0') and (AnsiChar(c) <= '9');
end;

function av_isgraph(c: int): Boolean; inline;
begin
  Result := (c > 32) and (c < 127);
end;

function av_isspace(c1: int): Boolean; inline;
var
  c: AnsiChar;
begin
  c := AnsiChar(c1);
  Result :=       //
    (c = ' ') or  //
    (c = #$0C) or //
    (c = #$0A) or //
    (c = #$0D) or //
    (c = #$09) or //
    (c = #$0B);
end;

function av_toupper(c1: int): int; inline;
var
  c: AnsiChar;
begin
  c := AnsiChar(c1);
  Result := Ord(c);
  if (c >= 'a') and (c <= 'z') then
    Result := Result xor $20;
end;

function av_tolower(c1: int): int; inline;
var
  c: AnsiChar;
begin
  c := AnsiChar(c1);
  Result := Ord(c);
  if (c >= 'A') and (c <= 'Z') then
    Result := Result xor $20;
end;

function av_isxdigit(c1: int): Boolean; inline;
var
  c: AnsiChar;
begin
  c1 := av_tolower(c1);
  c := AnsiChar(AnsiChar(c1));
  Result := av_isdigit(c1) or ((c >= 'a') and (c <= 'f'));
end;
{$ENDREGION}
{$REGION 'bprint.h'}

function av_bprint_is_complete(const buf: pAVBPrint): Boolean; inline;
begin
  Result := buf^.len < buf^.size;
end;
{$ENDREGION}
{$REGION 'fifo.h'}

function av_fifo_peek2(const f: pAVFifoBuffer; offs: int): puint8_t; inline;
var
  ptr: puint8_t;
begin
  ptr := f^.rptr + offs;
  if (ptr >= f^._end) then
    ptr := f^.buffer + (ptr - f^._end)
  else if (ptr < f^.buffer) then
    ptr := f^._end - (f^.buffer - ptr);
  Result := ptr;
end;
{$ENDREGION}
{$REGION 'timestamp.h'}

function av_ts_make_string(buf: PAnsiChar; ts: int64_t): PAnsiChar;
Var
  p: AnsiString;
  m: size_t;
begin
  {
    if (ts == AV_NOPTS_VALUE)
    snprintf(buf, AV_TS_MAX_STRING_SIZE, "NOPTS");
    else
    snprintf(buf, AV_TS_MAX_STRING_SIZE, "%" PRId64, ts);
    return buf;
  }
  if (ts = AV_NOPTS_VALUE) then
    p := 'NOPTS'
  else
    str(ts, p);
  m := length(p);
  if m > AV_TS_MAX_STRING_SIZE then
    m := AV_TS_MAX_STRING_SIZE;
  move(p[1], buf^, m);
  Result := buf;
end;

var
  av_ts_buf: array [0 .. AV_TS_MAX_STRING_SIZE] of AnsiChar;

function av_ts2str(ts: int64_t): PAnsiChar;
begin
  FillChar(av_ts_buf, SizeOf(av_ts_buf), 0);
  Result := av_ts_make_string(@av_ts_buf[0], ts);
end;

function av_ts_make_time_string(buf: PAnsiChar; ts: int64_t; tb: pAVRational): PAnsiChar;
Var
  p: AnsiString;
  m: size_t;
begin
  {
    if (ts == AV_NOPTS_VALUE) snprintf(buf, AV_TS_MAX_STRING_SIZE, "NOPTS");
    else                      snprintf(buf, AV_TS_MAX_STRING_SIZE, "%.6g", av_q2d(*tb) * ts);
    return buf;
  }
  if (ts = AV_NOPTS_VALUE) then
    p := 'NOPTS'
  else
    str((av_q2d(tb^) * ts): 1: 6, p);
  m := length(p);
  if m > AV_TS_MAX_STRING_SIZE then
    m := AV_TS_MAX_STRING_SIZE;
  move(p[1], buf^, m);
  Result := buf;
end;

function av_ts2timestr(ts: int64_t; tb: pAVRational): PAnsiChar;
begin
  FillChar(av_ts_buf, SizeOf(av_ts_buf), 0);
  Result := av_ts_make_time_string(@av_ts_buf[0], ts, tb);
end;

{$ENDREGION}
{$REGION 'mem.h'}

function av_size_mult(a: size_t; b: size_t; var r: size_t): int; inline;
var
  t: size_t;
begin
  t := a * b;
  (* Hack inspired from glibc: don't try the division if nelem and elsize
    * are both less than sqrt(SIZE_MAX). *)
  if ((a or b) >= (size_t(1) shl (SizeOf(size_t) * 4))) and (a <> 0) and ((t div a) <> b) then
    Exit(AVERROR_EINVAL);
  r := t;
  Result := 0;
end;
{$ENDREGION}
{$REGION 'parseutils.h'}

function av_parse_ratio_quiet(q: pAVRational; const str: PAnsiChar; max: int): int; inline;
begin
  Result := av_parse_ratio(q, str, max, AV_LOG_MAX_OFFSET, nil);
end;
{$ENDREGION}
{$REGION 'intfloat.h'}

function av_int2float(i: uint32_t): float; inline;
begin
  Result := av_intfloat32(i).f;
end;

function av_float2int(f: float): uint32_t; inline;
begin
  Result := av_intfloat32(f).i;
end;

function av_int2double(i: uint64_t): double; inline;
begin
  Result := av_intfloat64(i).f;
end;

function av_double2int(f: double): uint64_t; inline;
begin
  Result := av_intfloat64(f).i;
end;

{$ENDREGION}
{$REGION 'lfg.h'}

function av_lfg_get(c: pAVLFG): uint; inline;
begin
  // unsigned a = c->state[c->index & 63] = c->state[(c->index-24) & 63] + c->state[(c->index-55) & 63];
  Result := c^.state[(c^.index - 24) and 63] + c^.state[(c^.index - 55) and 63];
  c^.state[c^.index and 63] := Result;
  // c->index += 1U;
  c^.index := c^.index + 1;
  // return a;
end;

function av_mlfg_get(c: pAVLFG): uint; inline;
var
  a, b: uint;
begin
  // unsigned int a= c->state[(c->index-55) & 63];
  a := c^.state[(c^.index - 55) and 63];
  // unsigned int b= c->state[(c->index-24) & 63];
  b := c^.state[(c^.index - 24) and 63];
  // a = c->state[c->index & 63] = 2*a*b+a+b;
  Result := 2 * a * b + a + b;
  c^.state[c^.index and 63] := Result;
  // c->index += 1U;
  // return a;
end;

{$ENDREGION}

end.
