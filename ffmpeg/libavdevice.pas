unit libavdevice;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

Uses
  ffmpeg_types, libavutil, libavcodec, libavformat;

{$I ffmpeg.inc}

(* *
  * @defgroup lavd libavdevice
  * Special devices muxing/demuxing library.
  *
  * Libavdevice is a complementary library to @ref libavf "libavformat". It
  * provides various "special" platform-specific muxers and demuxers, e.g. for
  * grabbing devices, audio capture and playback etc. As a consequence, the
  * (de)muxers in libavdevice are of the AVFMT_NOFILE type (they use their own
  * I/O functions). The filename passed to avformat_open_input() often does not
  * refer to an actually existing file, but has some special device-specific
  * meaning - e.g. for xcbgrab it is the display name.
  *
  * To use libavdevice, simply call avdevice_register_all() to register all
  * compiled muxers and demuxers. They all use standard libavformat API.
  *
*)

(* *
  * Return the LIBAVDEVICE_VERSION_INT constant.
*)
// unsigned avdevice_version(void);
function avdevice_version(): unsigned; cdecl; external avdevice_dll;

(* *
  * Return the libavdevice build-time configuration.
*)
// const char *avdevice_configuration(void);
function avdevice_configuration(): pAnsiChar; cdecl; external avdevice_dll;

(* *
  * Return the libavdevice license.
*)
// const char *avdevice_license(void);
function avdevice_license(): pAnsiChar; cdecl; external avdevice_dll;

(* *
  * Initialize libavdevice and register all the input and output devices.
*)
// void avdevice_register_all(void);
procedure avdevice_register_all(); cdecl; external avdevice_dll;

(* *
  * Audio input devices iterator.
  *
  * If d is NULL, returns the first registered input audio/video device,
  * if d is non-NULL, returns the next registered input audio/video device after d
  * or NULL if d is the last one.
*)
// AVInputFormat *av_input_audio_device_next(AVInputFormat  *d);
function av_input_audio_device_next(d: pAVInputFormat): pAVInputFormat; cdecl; external avdevice_dll;

(* *
  * Video input devices iterator.
  *
  * If d is NULL, returns the first registered input audio/video device,
  * if d is non-NULL, returns the next registered input audio/video device after d
  * or NULL if d is the last one.
*)
// AVInputFormat *av_input_video_device_next(AVInputFormat  *d);
function av_input_video_device_next(d: pAVInputFormat): pAVInputFormat; cdecl; external avdevice_dll;

(* *
  * Audio output devices iterator.
  *
  * If d is NULL, returns the first registered output audio/video device,
  * if d is non-NULL, returns the next registered output audio/video device after d
  * or NULL if d is the last one.
*)
// AVOutputFormat *av_output_audio_device_next(AVOutputFormat *d);
function av_output_audio_device_next(d: pAVOutputFormat): pAVOutputFormat; cdecl; external avdevice_dll;

(* *
  * Video output devices iterator.
  *
  * If d is NULL, returns the first registered output audio/video device,
  * if d is non-NULL, returns the next registered output audio/video device after d
  * or NULL if d is the last one.
*)
// AVOutputFormat *av_output_video_device_next(AVOutputFormat *d);
function av_output_video_device_next(d: pAVOutputFormat): pAVOutputFormat; cdecl; external avdevice_dll;

type
  pAVDeviceRect = ^AVDeviceRect;

  AVDeviceRect = record
    x: int; (* *< x coordinate of top left corner *)
    y: int; (* *< y coordinate of top left corner *)
    width: int; (* *< width *)
    height: int; (* *< height *)
  end;

  (* *
    * Message types used by avdevice_app_to_dev_control_message().
  *)
  AVAppToDevMessageType = array [0 .. 3] of AnsiChar;

const
  (* *
    * Dummy message.
  *)
  AV_APP_TO_DEV_NONE: AVAppToDevMessageType = ('N', 'O', 'N', 'E');

  (* *
    * Window size change message.
    *
    * Message is sent to the device every time the application changes the size
    * of the window device renders to.
    * Message should also be sent right after window is created.
    *
    * data: AVDeviceRect: new window size.
  *)
  AV_APP_TO_DEV_WINDOW_SIZE: AVAppToDevMessageType = ('G', 'E', 'O', 'M');

  (* *
    * Repaint request message.
    *
    * Message is sent to the device when window has to be repainted.
    *
    * data: AVDeviceRect: area required to be repainted.
    *       NULL: whole area is required to be repainted.
  *)
  AV_APP_TO_DEV_WINDOW_REPAINT: AVAppToDevMessageType = ('R', 'E', 'P', 'A');

  (* *
    * Request pause/play.
    *
    * Application requests pause/unpause playback.
    * Mostly usable with devices that have internal buffer.
    * By default devices are not paused.
    *
    * data: NULL
  *)
  AV_APP_TO_DEV_PAUSE: AVAppToDevMessageType = ('P', 'A', 'U', ' ');
  AV_APP_TO_DEV_PLAY: AVAppToDevMessageType = ('P', 'L', 'A', 'Y');
  AV_APP_TO_DEV_TOGGLE_PAUSE: AVAppToDevMessageType = ('P', 'A', 'U', 'T');

  (* *
    * Volume control message.
    *
    * Set volume level. It may be device-dependent if volume
    * is changed per stream or system wide. Per stream volume
    * change is expected when possible.
    *
    * data: double: new volume with range of 0.0 - 1.0.
  *)
  AV_APP_TO_DEV_SET_VOLUME: AVAppToDevMessageType = ('S', 'V', 'O', 'L');

  (* *
    * Mute control messages.
    *
    * Change mute state. It may be device-dependent if mute status
    * is changed per stream or system wide. Per stream mute status
    * change is expected when possible.
    *
    * data: NULL.
  *)
  AV_APP_TO_DEV_MUTE: AVAppToDevMessageType = (' ', 'M', 'U', 'T');
  AV_APP_TO_DEV_UNMUTE: AVAppToDevMessageType = ('U', 'M', 'U', 'T');
  AV_APP_TO_DEV_TOGGLE_MUTE: AVAppToDevMessageType = ('T', 'M', 'U', 'T');

  (* *
    * Get volume/mute messages.
    *
    * Force the device to send AV_DEV_TO_APP_VOLUME_LEVEL_CHANGED or
    * AV_DEV_TO_APP_MUTE_STATE_CHANGED command respectively.
    *
    * data: NULL.
  *)
  AV_APP_TO_DEV_GET_VOLUME: AVAppToDevMessageType = ('G', 'V', 'O', 'L');
  AV_APP_TO_DEV_GET_MUTE: AVAppToDevMessageType = ('G', 'M', 'U', 'T');

type
  AVDevToAppMessageType = array [0 .. 3] of AnsiChar;

const
  (* *
    * Message types used by avdevice_dev_to_app_control_message().
  *)
  (* *
    * Dummy message.
  *)
  AV_DEV_TO_APP_NONE: AVDevToAppMessageType = ('N', 'O', 'N', 'E');

  (* *
    * Create window buffer message.
    *
    * Device requests to create a window buffer. Exact meaning is device-
    * and application-dependent. Message is sent before rendering first
    * frame and all one-shot initializations should be done here.
    * Application is allowed to ignore preferred window buffer size.
    *
    * @note: Application is obligated to inform about window buffer size
    *        with AV_APP_TO_DEV_WINDOW_SIZE message.
    *
    * data: AVDeviceRect: preferred size of the window buffer.
    *       NULL: no preferred size of the window buffer.
  *)
  AV_DEV_TO_APP_CREATE_WINDOW_BUFFER: AVDevToAppMessageType = ('B', 'C', 'R', 'E');

  (* *
    * Prepare window buffer message.
    *
    * Device requests to prepare a window buffer for rendering.
    * Exact meaning is device- and application-dependent.
    * Message is sent before rendering of each frame.
    *
    * data: NULL.
  *)
  AV_DEV_TO_APP_PREPARE_WINDOW_BUFFER: AVDevToAppMessageType = ('B', 'P', 'R', 'E');

  (* *
    * Display window buffer message.
    *
    * Device requests to display a window buffer.
    * Message is sent when new frame is ready to be displayed.
    * Usually buffers need to be swapped in handler of this message.
    *
    * data: NULL.
  *)
  AV_DEV_TO_APP_DISPLAY_WINDOW_BUFFER: AVDevToAppMessageType = ('B', 'D', 'I', 'S');

  (* *
    * Destroy window buffer message.
    *
    * Device requests to destroy a window buffer.
    * Message is sent when device is about to be destroyed and window
    * buffer is not required anymore.
    *
    * data: NULL.
  *)
  AV_DEV_TO_APP_DESTROY_WINDOW_BUFFER: AVDevToAppMessageType = ('B', 'D', 'E', 'S');

  (* *
    * Buffer fullness status messages.
    *
    * Device signals buffer overflow/underflow.
    *
    * data: NULL.
  *)
  AV_DEV_TO_APP_BUFFER_OVERFLOW: AVDevToAppMessageType = ('B', 'O', 'F', 'L');
  AV_DEV_TO_APP_BUFFER_UNDERFLOW: AVDevToAppMessageType = ('B', 'U', 'F', 'L');

  (* *
    * Buffer readable/writable.
    *
    * Device informs that buffer is readable/writable.
    * When possible, device informs how many bytes can be read/write.
    *
    * @warning Device may not inform when number of bytes than can be read/write changes.
    *
    * data: int64_t: amount of bytes available to read/write.
    *       NULL: amount of bytes available to read/write is not known.
  *)
  AV_DEV_TO_APP_BUFFER_READABLE: AVDevToAppMessageType = ('B', 'R', 'D', ' ');
  AV_DEV_TO_APP_BUFFER_WRITABLE: AVDevToAppMessageType = ('B', 'W', 'R', ' ');

  (* *
    * Mute state change message.
    *
    * Device informs that mute state has changed.
    *
    * data: int: 0 for not muted state, non-zero for muted state.
  *)
  AV_DEV_TO_APP_MUTE_STATE_CHANGED: AVDevToAppMessageType = ('C', 'M', 'U', 'T');

  (* *
    * Volume level change message.
    *
    * Device informs that volume level has changed.
    *
    * data: double: new volume with range of 0.0 - 1.0.
  *)
  AV_DEV_TO_APP_VOLUME_LEVEL_CHANGED: AVDevToAppMessageType = ('C', 'V', 'O', 'L');

  (* *
    * Send control message from application to device.
    *
    * @param s         device context.
    * @param type      message type.
    * @param data      message data. Exact type depends on message type.
    * @param data_size size of message data.
    * @return >= 0 on success, negative on error.
    *         AVERROR(ENOSYS) when device doesn't implement handler of the message.
  *)
  // int avdevice_app_to_dev_control_message(struct AVFormatContext *s,
  // enum AVAppToDevMessageType type,
  // void *data, size_t data_size);
function avdevice_app_to_dev_control_message(s: pAVFormatContext; _type: AVAppToDevMessageType; data: Pointer; data_size: size_t): int;
  cdecl; external avdevice_dll;
(* *
  * Send control message from device to application.
  *
  * @param s         device context.
  * @param type      message type.
  * @param data      message data. Can be NULL.
  * @param data_size size of message data.
  * @return >= 0 on success, negative on error.
  *         AVERROR(ENOSYS) when application doesn't implement handler of the message.
*)
// int avdevice_dev_to_app_control_message(struct AVFormatContext *s,
// enum AVDevToAppMessageType type,
// void *data, size_t data_size);
function avdevice_dev_to_app_control_message(s: pAVFormatContext; _type: AVDevToAppMessageType; data: Pointer; data_size: size_t): int;
  cdecl; external avdevice_dll;

(* *
  * Following API allows user to probe device capabilities (supported codecs,
  * pixel formats, sample formats, resolutions, channel counts, etc).
  * It is build on top op AVOption API.
  * Queried capabilities make it possible to set up converters of video or audio
  * parameters that fit to the device.
  *
  * List of capabilities that can be queried:
  *  - Capabilities valid for both audio and video devices:
  *    - codec:          supported audio/video codecs.
  *                      type: AV_OPT_TYPE_INT (AVCodecID value)
  *  - Capabilities valid for audio devices:
  *    - sample_format:  supported sample formats.
  *                      type: AV_OPT_TYPE_INT (AVSampleFormat value)
  *    - sample_rate:    supported sample rates.
  *                      type: AV_OPT_TYPE_INT
  *    - channels:       supported number of channels.
  *                      type: AV_OPT_TYPE_INT
  *    - channel_layout: supported channel layouts.
  *                      type: AV_OPT_TYPE_INT64
  *  - Capabilities valid for video devices:
  *    - pixel_format:   supported pixel formats.
  *                      type: AV_OPT_TYPE_INT (AVPixelFormat value)
  *    - window_size:    supported window sizes (describes size of the window size presented to the user).
  *                      type: AV_OPT_TYPE_IMAGE_SIZE
  *    - frame_size:     supported frame sizes (describes size of provided video frames).
  *                      type: AV_OPT_TYPE_IMAGE_SIZE
  *    - fps:            supported fps values
  *                      type: AV_OPT_TYPE_RATIONAL
  *
  * Value of the capability may be set by user using av_opt_set() function
  * and AVDeviceCapabilitiesQuery object. Following queries will
  * limit results to the values matching already set capabilities.
  * For example, setting a codec may impact number of formats or fps values
  * returned during next query. Setting invalid value may limit results to zero.
  *
  * Example of the usage basing on opengl output device:
  *
  * @code
  *  AVFormatContext *oc = NULL;
  *  AVDeviceCapabilitiesQuery *caps = NULL;
  *  AVOptionRanges *ranges;
  *  int ret;
  *
  *  if ((ret = avformat_alloc_output_context2(&oc, NULL, "opengl", NULL)) < 0)
  *      goto fail;
  *  if (avdevice_capabilities_create(&caps, oc, NULL) < 0)
  *      goto fail;
  *
  *  //query codecs
  *  if (av_opt_query_ranges(&ranges, caps, "codec", AV_OPT_MULTI_COMPONENT_RANGE)) < 0)
  *      goto fail;
  *  //pick codec here and set it
  *  av_opt_set(caps, "codec", AV_CODEC_ID_RAWVIDEO, 0);
  *
  *  //query format
  *  if (av_opt_query_ranges(&ranges, caps, "pixel_format", AV_OPT_MULTI_COMPONENT_RANGE)) < 0)
  *      goto fail;
  *  //pick format here and set it
  *  av_opt_set(caps, "pixel_format", AV_PIX_FMT_YUV420P, 0);
  *
  *  //query and set more capabilities
  *
  * fail:
  *  //clean up code
  *  avdevice_capabilities_free(&query, oc);
  *  avformat_free_context(oc);
  * @endcode
*)
type
  (* *
    * Structure describes device capabilities.
    *
    * It is used by devices in conjunction with av_device_capabilities AVOption table
    * to implement capabilities probing API based on AVOption API. Should not be used directly.
  *)
  pAVDeviceCapabilitiesQuery = ^AVDeviceCapabilitiesQuery;

  AVDeviceCapabilitiesQuery = record
    av_class: pAVClass;
    device_context: pAVFormatContext;
    codec: AVCodecID;
    sample_format: AVSampleFormat;
    pixel_format: AVPixelFormat;
    sample_rate: int;
    channels: int;
    channel_layout: int64_t;
    window_width: int;
    window_height: int;
    frame_width: int;
    frame_height: int;
    fps: AVRational;
  end;

  (* *
    * AVOption table used by devices to implement device capabilities API. Should not be used by a user.
  *)
  // extern const AVOption av_device_capabilities[];

  (* *
    * Initialize capabilities probing API based on AVOption API.
    *
    * avdevice_capabilities_free() must be called when query capabilities API is
    * not used anymore.
    *
    * @param[out] caps      Device capabilities data. Pointer to a NULL pointer must be passed.
    * @param s              Context of the device.
    * @param device_options An AVDictionary filled with device-private options.
    *                       On return this parameter will be destroyed and replaced with a dict
    *                       containing options that were not found. May be NULL.
    *                       The same options must be passed later to avformat_write_header() for output
    *                       devices or avformat_open_input() for input devices, or at any other place
    *                       that affects device-private options.
    *
    * @return >= 0 on success, negative otherwise.
  *)
  // int avdevice_capabilities_create(AVDeviceCapabilitiesQuery **caps, AVFormatContext *s,
  // AVDictionary **device_options);
function avdevice_capabilities_create(var caps: pAVDeviceCapabilitiesQuery; s: pAVFormatContext; var device_options: pAVDictionary): int;
  cdecl; external avdevice_dll;
(* *
  * Free resources created by avdevice_capabilities_create()
  *
  * @param caps Device capabilities data to be freed.
  * @param s    Context of the device.
*)
// void avdevice_capabilities_free(AVDeviceCapabilitiesQuery **caps, AVFormatContext *s);
procedure avdevice_capabilities_free(var caps: pAVDeviceCapabilitiesQuery; s: pAVFormatContext); cdecl; external avdevice_dll;

type
  (* *
    * Structure describes basic parameters of the device.
  *)
  pAVDeviceInfo = ^AVDeviceInfo;
  ppAVDeviceInfo = ^pAVDeviceInfo;

  AVDeviceInfo = record
    device_name: pAnsiChar; (* *< device name, format depends on device *)
    device_description: pAnsiChar; (* *< human friendly name *)
  end;

  (* *
    * List of devices.
  *)
  pAVDeviceInfoList = ^AVDeviceInfoList;

  AVDeviceInfoList = record
    devices: ppAVDeviceInfo; (* *< list of autodetected devices *)
    nb_devices: int; (* *< number of autodetected devices *)
    default_device: int; (* *< index of default device or -1 if no default *)
  end;

  (* *
    * List devices.
    *
    * Returns available device names and their parameters.
    *
    * @note: Some devices may accept system-dependent device names that cannot be
    *        autodetected. The list returned by this function cannot be assumed to
    *        be always completed.
    *
    * @param s                device context.
    * @param[out] device_list list of autodetected devices.
    * @return count of autodetected devices, negative on error.
  *)
  // int avdevice_list_devices(struct AVFormatContext *s, AVDeviceInfoList **device_list);
function avdevice_list_devices(s: pAVFormatContext; var device_list: pAVDeviceInfoList): int; cdecl; external avdevice_dll;
(* *
  * Convenient function to free result of avdevice_list_devices().
  *
  * @param devices device list to be freed.
*)
// void avdevice_free_list_devices(AVDeviceInfoList **device_list);
procedure avdevice_free_list_devices(var device_list: pAVDeviceInfoList); cdecl; external avdevice_dll;
(* *
  * List devices.
  *
  * Returns available device names and their parameters.
  * These are convinient wrappers for avdevice_list_devices().
  * Device context is allocated and deallocated internally.
  *
  * @param device           device format. May be NULL if device name is set.
  * @param device_name      device name. May be NULL if device format is set.
  * @param device_options   An AVDictionary filled with device-private options. May be NULL.
  *                         The same options must be passed later to avformat_write_header() for output
  *                         devices or avformat_open_input() for input devices, or at any other place
  *                         that affects device-private options.
  * @param[out] device_list list of autodetected devices
  * @return count of autodetected devices, negative on error.
  * @note device argument takes precedence over device_name when both are set.
*)
// int avdevice_list_input_sources(struct AVInputFormat *device, const char *device_name,
// AVDictionary *device_options, AVDeviceInfoList **device_list);
function avdevice_list_input_sources(device: pAVInputFormat; const device_name: pAnsiChar; device_options: pAVDictionary;
  var device_list: pAVDeviceInfoList): int; cdecl; external avdevice_dll;

// int avdevice_list_output_sinks(struct AVOutputFormat *device, const char *device_name,
// AVDictionary *device_options, AVDeviceInfoList **device_list);
function avdevice_list_output_sinks(device: pAVOutputFormat; const device_name: pAnsiChar; device_options: pAVDictionary;
  var device_list: pAVDeviceInfoList): int; cdecl; external avdevice_dll;

implementation

end.
