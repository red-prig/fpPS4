unit libswscale;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

Uses
  ffmpeg_types, libavutil;

{$I ffmpeg.inc}
(*
  * @defgroup libsws libswscale
  * Color conversion and scaling library.
  *
  * @{
  *
  * Return the LIBSWSCALE_VERSION_INT constant.
*)
// unsigned swscale_version(void);
function swscale_version(): unsigned; cdecl; external swscale_dll;

(*
  * Return the libswscale build-time configuration.
*)
// const char *swscale_configuration(void);
function swscale_configuration(): pAnsiChar; cdecl; external swscale_dll;

(*
  * Return the libswscale license.
*)
// const char *swscale_license(void);
function swscale_license(): pAnsiChar; cdecl; external swscale_dll;

const
  (* values for the flags, the stuff on the command line is different *)
  SWS_FAST_BILINEAR = 1;
  SWS_BILINEAR      = 2;
  SWS_BICUBIC       = 4;
  SWS_X             = 8;
  SWS_POINT         = $10;
  SWS_AREA          = $20;
  SWS_BICUBLIN      = $40;
  SWS_GAUSS         = $80;
  SWS_SINC          = $100;
  SWS_LANCZOS       = $200;
  SWS_SPLINE        = $400;

  SWS_SRC_V_CHR_DROP_MASK  = $30000;
  SWS_SRC_V_CHR_DROP_SHIFT = 16;

  SWS_PARAM_DEFAULT = 123456;

  SWS_PRINT_INFO = $1000;

  // the following 3 flags are not completely implemented
  // internal chrominance subsampling info
  SWS_FULL_CHR_H_INT = $2000;
  // input subsampling info
  SWS_FULL_CHR_H_INP  = $4000;
  SWS_DIRECT_BGR      = $8000;
  SWS_ACCURATE_RND    = $40000;
  SWS_BITEXACT        = $80000;
  SWS_ERROR_DIFFUSION = $800000;

  SWS_MAX_REDUCE_CUTOFF = 0.002;

  SWS_CS_ITU709    = 1;
  SWS_CS_FCC       = 4;
  SWS_CS_ITU601    = 5;
  SWS_CS_ITU624    = 5;
  SWS_CS_SMPTE170M = 5;
  SWS_CS_SMPTE240M = 7;
  SWS_CS_DEFAULT   = 5;
  SWS_CS_BT2020    = 9;

  (*
    * Return a pointer to yuv<->rgb coefficients for the given colorspace
    * suitable for sws_setColorspaceDetails().
    *
    * @param colorspace One of the SWS_CS_* macros. If invalid,
    * SWS_CS_DEFAULT is used.
  *)
  // const int *sws_getCoefficients(int colorspace);
function sws_getCoefficients(colorspace: int): pInt; cdecl; external swscale_dll;

// when used for filters they must have an odd number of elements
// coeffs cannot be shared between vectors

type
  SwsVector = record
    coeff: pdouble;
    /// < pointer to the list of coefficients
    length: int;
    /// < number of coefficients in the vector
  end;

  pSwsVector = ^SwsVector;

  // vectors can be shared
  SwsFilter = record
    lumH: pSwsVector;
    lumV: pSwsVector;
    chrH: pSwsVector;
    chrV: pSwsVector;
  End;

  pSwsFilter = ^SwsFilter;

  SwsContext = record
  end;

  pSwsContext = ^SwsContext;

  Tsws_array_uint8_t = array_uint8_t;
  psws_array_uint8_t = ^Tsws_array_uint8_t;

  Tsws_array_int = array_int;
  psws_array_int = ^Tsws_array_int;

  Tsws_array4_int = array4_int;
  psws_array4_int = ^Tsws_array4_int;

  (*
    * Return a positive value if pix_fmt is a supported input format, 0
    * otherwise.
  *)
  // int sws_isSupportedInput(enum AVPixelFormat pix_fmt);
function sws_isSupportedInput(pix_fmt: AVPixelFormat): int; cdecl; external swscale_dll;

(*
  * Return a positive value if pix_fmt is a supported output format, 0
  * otherwise.
*)
// int sws_isSupportedOutput(enum AVPixelFormat pix_fmt);
function sws_isSupportedOutput(pix_fmt: AVPixelFormat): int; cdecl; external swscale_dll;

(*
  * @param[in]  pix_fmt the pixel format
  * @return a positive value if an endianness conversion for pix_fmt is
  * supported, 0 otherwise.
*)
// int sws_isSupportedEndiannessConversion(enum AVPixelFormat pix_fmt);
function sws_isSupportedEndiannessConversion(pix_fmt: AVPixelFormat): int; cdecl; external swscale_dll;

(*
  * Allocate an empty SwsContext. This must be filled and passed to
  * sws_init_context(). For filling see AVOptions, options.c and
  * sws_setColorspaceDetails().
*)
// struct SwsContext *sws_alloc_context(void);
function sws_alloc_context(): pSwsContext; cdecl; external swscale_dll;

(*
  * Initialize the swscaler context sws_context.
  *
  * @return zero or positive value on success, a negative value on
  * error
*)
// av_warn_unused_result
// int sws_init_context(struct SwsContext *sws_context, SwsFilter *srcFilter, SwsFilter *dstFilter);
function sws_init_context(sws_context: pSwsContext; srcFilter: pSwsFilter; dstFilter: pSwsFilter): int; cdecl; external swscale_dll;

(*
  * Free the swscaler context swsContext.
  * If swsContext is NULL, then does nothing.
*)
// void sws_freeContext(struct SwsContext *swsContext);
procedure sws_freeContext(SwsContext: pSwsContext); cdecl; external swscale_dll;

(*
  * Allocate and return an SwsContext. You need it to perform
  * scaling/conversion operations using sws_scale().
  *
  * @param srcW the width of the source image
  * @param srcH the height of the source image
  * @param srcFormat the source image format
  * @param dstW the width of the destination image
  * @param dstH the height of the destination image
  * @param dstFormat the destination image format
  * @param flags specify which algorithm and options to use for rescaling
  * @param param extra parameters to tune the used scaler
  *              For SWS_BICUBIC param[0] and [1] tune the shape of the basis
  *              function, param[0] tunes f(1) and param[1] f´(1)
  *              For SWS_GAUSS param[0] tunes the exponent and thus cutoff
  *              frequency
  *              For SWS_LANCZOS param[0] tunes the width of the window function
  * @return a pointer to an allocated context, or NULL in case of error
  * @note this function is to be removed after a saner alternative is
  *       written
*)
// struct SwsContext *sws_getContext(int srcW, int srcH, enum AVPixelFormat srcFormat,
// int dstW, int dstH, enum AVPixelFormat dstFormat,
// int flags, SwsFilter *srcFilter,
// SwsFilter *dstFilter, const double *param);

function sws_getContext(srcW: int; srcH: int; srcFormat: AVPixelFormat; dstW: int; dstH: int; dstFormat: AVPixelFormat; flags: int; srcFilter: pSwsFilter;
  dstFilter: pSwsFilter; const param: pdouble): pSwsContext; cdecl; external swscale_dll;

(*
  * Scale the image slice in srcSlice and put the resulting scaled
  * slice in the image in dst. A slice is a sequence of consecutive
  * rows in an image.
  *
  * Slices have to be provided in sequential order, either in
  * top-bottom or bottom-top order. If slices are provided in
  * non-sequential order the behavior of the function is undefined.
  *
  * @param c         the scaling context previously created with
  *                  sws_getContext()
  * @param srcSlice  the array containing the pointers to the planes of
  *                  the source slice
  * @param srcStride the array containing the strides for each plane of
  *                  the source image
  * @param srcSliceY the position in the source image of the slice to
  *                  process, that is the number (counted starting from
  *                  zero) in the image of the first row of the slice
  * @param srcSliceH the height of the source slice, that is the number
  *                  of rows in the slice
  * @param dst       the array containing the pointers to the planes of
  *                  the destination image
  * @param dstStride the array containing the strides for each plane of
  *                  the destination image
  * @return          the height of the output slice
*)

// int sws_scale(struct SwsContext *c, const uint8_t *const srcSlice[],
// const int srcStride[], int srcSliceY, int srcSliceH,
// uint8_t *const dst[], const int dstStride[]);
function sws_scale(c: pSwsContext; const srcSlice: psws_array_uint8_t; const srcStride: psws_array_int; srcSliceY: int; srcSliceH: int; dst: psws_array_uint8_t;
  const dstStride: psws_array_int): int; cdecl; overload; external swscale_dll;

(*
  * @param dstRange flag indicating the while-black range of the output (1=jpeg / 0=mpeg)
  * @param srcRange flag indicating the while-black range of the input (1=jpeg / 0=mpeg)
  * @param table the yuv2rgb coefficients describing the output yuv space, normally ff_yuv2rgb_coeffs[x]
  * @param inv_table the yuv2rgb coefficients describing the input yuv space, normally ff_yuv2rgb_coeffs[x]
  * @param brightness 16.16 fixed point brightness correction
  * @param contrast 16.16 fixed point contrast correction
  * @param saturation 16.16 fixed point saturation correction
  * @return -1 if not supported
*)
// int sws_setColorspaceDetails(struct SwsContext *c, const int inv_table[4],
// int srcRange, const int table[4], int dstRange,
// int brightness, int contrast, int saturation);
function sws_setColorspaceDetails(c: pSwsContext; const inv_table: psws_array4_int; srcRange: int; const table: psws_array4_int; dstRange: int; brightness: int;
  contrast: int; saturation: int): int; cdecl; external swscale_dll;

(*
  * @return -1 if not supported
*)
// int sws_getColorspaceDetails(struct SwsContext *c, int **inv_table,
// int *srcRange, int **table, int *dstRange,
// int *brightness, int *contrast, int *saturation);
function sws_getColorspaceDetails(c: pSwsContext; var inv_table: pInt; var srcRange: int; var table: pInt; var dstRange: int; var brightness: int;
  var contrast: int; var saturation: int): int; cdecl; external swscale_dll;

(*
  * Allocate and return an uninitialized vector with length coefficients.
*)
// SwsVector *sws_allocVec(int length);
function sws_allocVec(length: int): pSwsVector; cdecl; external swscale_dll;

(*
  * Return a normalized Gaussian curve used to filter stuff
  * quality = 3 is high quality, lower is lower quality.
*)
// SwsVector *sws_getGaussianVec(double variance, double quality);
function sws_getGaussianVec(variance: double; quality: double): pSwsVector; cdecl; external swscale_dll;

(*
  * Scale all the coefficients of a by the scalar value.
*)
// void sws_scaleVec(SwsVector *a, double scalar);
procedure sws_scaleVec(a: pSwsVector; scalar: double); cdecl; external swscale_dll;

(*
  * Scale all the coefficients of a so that their sum equals height.
*)
// void sws_normalizeVec(SwsVector *a, double height);
procedure sws_normalizeVec(a: pSwsVector; height: double); cdecl; external swscale_dll;

{$IFDEF FF_API_SWS_VECTOR}
// attribute_deprecated SwsVector *sws_getConstVec(double c, int length);
function sws_getConstVec(c: double; length: int): pSwsVector; cdecl; external swscale_dll;
// attribute_deprecated SwsVector *sws_getIdentityVec(void);
function sws_getIdentityVec(): pSwsVector; cdecl; external swscale_dll;
// attribute_deprecated void sws_convVec(SwsVector *a, SwsVector *b);
procedure sws_convVec(a: pSwsVector; b: pSwsVector); cdecl; external swscale_dll;
// attribute_deprecated void sws_addVec(SwsVector *a, SwsVector *b);
procedure sws_addVec(a: pSwsVector; b: pSwsVector); cdecl; external swscale_dll;
// attribute_deprecated void sws_subVec(SwsVector *a, SwsVector *b);
procedure sws_subVec(a: pSwsVector; b: pSwsVector); cdecl; external swscale_dll;
// attribute_deprecated void sws_shiftVec(SwsVector *a, int shift);
procedure sws_shiftVec(a: pSwsVector; shift: int); cdecl; external swscale_dll;
// attribute_deprecated SwsVector *sws_cloneVec(SwsVector *a);
function sws_cloneVec(a: pSwsVector): pSwsVector; cdecl; external swscale_dll;
// attribute_deprecated void sws_printVec2(SwsVector *a, AVClass *log_ctx, int log_level);
procedure sws_printVec2(a: pSwsVector; log_ctx: pAVClass; log_level: int); cdecl; external swscale_dll;
{$ENDIF}
// void sws_freeVec(SwsVector *a);
procedure sws_freeVec(a: pSwsVector); cdecl; external swscale_dll;

// SwsFilter *sws_getDefaultFilter(float lumaGBlur, float chromaGBlur,
// float lumaSharpen, float chromaSharpen,
// float chromaHShift, float chromaVShift,
// int verbose);
function sws_getDefaultFilter(lumaGBlur: float; chromaGBlur: float; lumaSharpen: float; chromaSharpen: float; chromaHShift: float; chromaVShift: float;
  verbose: int): pSwsFilter; cdecl; external swscale_dll;

// void sws_freeFilter(SwsFilter *filter);
procedure sws_freeFilter(filter: pSwsFilter); cdecl; external swscale_dll;

(*
  * Check if context can be reused, otherwise reallocate a new one.
  *
  * If context is NULL, just calls sws_getContext() to get a new
  * context. Otherwise, checks if the parameters are the ones already
  * saved in context. If that is the case, returns the current
  * context. Otherwise, frees context and gets a new context with
  * the new parameters.
  *
  * Be warned that srcFilter and dstFilter are not checked, they
  * are assumed to remain the same.
*)
// struct SwsContext *sws_getCachedContext(struct SwsContext *context,
// int srcW, int srcH, enum AVPixelFormat srcFormat,
// int dstW, int dstH, enum AVPixelFormat dstFormat,
// int flags, SwsFilter *srcFilter,
// SwsFilter *dstFilter, const double *param);
function sws_getCachedContext(context: pSwsContext; srcW: int; srcH: int; srcFormat: AVPixelFormat; dstW: int; dstH: int; dstFormat: AVPixelFormat; flags: int;
  srcFilter: pSwsFilter; dstFilter: pSwsFilter; const param: pdouble): pSwsContext; cdecl; external swscale_dll;

(*
  * Convert an 8-bit paletted frame into a frame with a color depth of 32 bits.
  *
  * The output frame will have the same packed format as the palette.
  *
  * @param src        source frame buffer
  * @param dst        destination frame buffer
  * @param num_pixels number of pixels to convert
  * @param palette    array with [256] entries, which must match color arrangement (RGB or BGR) of src
*)
// void sws_convertPalette8ToPacked32(const uint8_t *src, uint8_t *dst, int num_pixels, const uint8_t *palette);
procedure sws_convertPalette8ToPacked32(const src: puint8_t; var dst: uint8_t; num_pixels: int; const palette: puint8_t); cdecl; external swscale_dll;

(*
  * Convert an 8-bit paletted frame into a frame with a color depth of 24 bits.
  *
  * With the palette format "ABCD", the destination frame ends up with the format "ABC".
  *
  * @param src        source frame buffer
  * @param dst        destination frame buffer
  * @param num_pixels number of pixels to convert
  * @param palette    array with [256] entries, which must match color arrangement (RGB or BGR) of src
*)
// void sws_convertPalette8ToPacked24(const uint8_t *src, uint8_t *dst, int num_pixels, const uint8_t *palette);
procedure sws_convertPalette8ToPacked24(const src: puint8_t; var dst: uint8_t; num_pixels: int; const palette: puint8_t); cdecl; external swscale_dll;

(*
  * Get the AVClass for swsContext. It can be used in combination with
  * AV_OPT_SEARCH_FAKE_OBJ for examining options.
  *
  * @see av_opt_find().
*)
// const AVClass *sws_get_class(void);
function sws_get_class(): pAVClass; cdecl; external swscale_dll;

implementation

end.
