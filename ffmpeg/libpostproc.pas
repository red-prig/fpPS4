unit libpostproc;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

Uses
  ffmpeg_types;

{$I ffmpeg.inc}

(* *
  * Return the LIBPOSTPROC_VERSION_INT constant.
*)
// unsigned postproc_version(void);
function postproc_version(): unsigned; cdecl; external postproc_dll;

(* *
  * Return the libpostproc build-time configuration.
*)
// const char *postproc_configuration(void);
function postproc_configuration(): pAnsiChar; cdecl; external postproc_dll;
(* *
  * Return the libpostproc license.
*)
// const char *postproc_license(void);
function postproc_license(): pAnsiChar; cdecl; external postproc_dll;

const
  PP_QUALITY_MAX = 6;

  // #include <inttypes.h>

type
  ppp_context = ^pp_context;

  pp_context = record
  end;

  ppp_mode = ^pp_mode;

  pp_mode = record
  end;

  Tpp_src_puint8_t = array [0 .. 2] of puint8_t;
  Tpp_dst_puint8_t = Tpp_src_puint8_t;
  Tpp_srcStride_int = array [0 .. 2] of int;
  Tpp_dstStride_int = Tpp_srcStride_int;

  (*
    #if LIBPOSTPROC_VERSION_INT < (52<<16)
    typedef pp_context pp_context_t;
    typedef pp_mode pp_mode_t;
    extern const char *const pp_help; ///< a simple help text
    #else
    extern const char pp_help[]; ///< a simple help text
    #endif
  *)

  // void  pp_postprocess(const uint8_t * src[3], const int srcStride[3],
  // uint8_t * dst[3], const int dstStride[3],
  // int horizontalSize, int verticalSize,
  // const int8_t *QP_store,  int QP_stride,
  // pp_mode *mode, pp_context *ppContext, int pict_type);

procedure pp_postprocess(const src: Tpp_src_puint8_t; const srcStride: Tpp_srcStride_int; dst: Tpp_dst_puint8_t;
  const dstStride: Tpp_dstStride_int; horizontalSize: int; verticalSize: int; const QP_store: pint8_t; QP_stride: int; mode: ppp_mode;
  ppContext: ppp_context; pict_type: int); cdecl; external postproc_dll;
(* *
  * Return a pp_mode or NULL if an error occurred.
  *
  * @param name    the string after "-pp" on the command line
  * @param quality a number from 0 to PP_QUALITY_MAX
*)
// pp_mode *pp_get_mode_by_name_and_quality(const char *name, int quality);
function pp_get_mode_by_name_and_quality(const name: pAnsiChar; quality: int): ppp_mode; cdecl; external postproc_dll;

// void pp_free_mode(pp_mode *mode);
procedure pp_free_mode(mode: ppp_mode); cdecl; external postproc_dll;

// pp_context *pp_get_context(int width, int height, int flags);
function pp_get_context(width: int; height: int; flags: int): ppp_context; cdecl; external postproc_dll;

// void pp_free_context(pp_context *ppContext);
procedure pp_free_context(ppContext: ppp_context); cdecl; external postproc_dll;

const
  PP_CPU_CAPS_MMX = $80000000;
  PP_CPU_CAPS_MMX2 = $20000000;
  PP_CPU_CAPS_3DNOW = $40000000;
  PP_CPU_CAPS_ALTIVEC = $10000000;
  PP_CPU_CAPS_AUTO = $00080000;

  PP_FORMAT = $00000008;
  PP_FORMAT_420 = ($00000011 or PP_FORMAT);
  PP_FORMAT_422 = ($00000001 or PP_FORMAT);
  PP_FORMAT_411 = ($00000002 or PP_FORMAT);
  PP_FORMAT_444 = ($00000000 or PP_FORMAT);
  PP_FORMAT_440 = ($00000010 or PP_FORMAT);

  PP_PICT_TYPE_QP2 = $00000010;
  /// < MPEG2 style QScale

implementation

end.
