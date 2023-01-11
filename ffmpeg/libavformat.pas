unit libavformat;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

Uses
  ffmpeg_types, libavutil, libavcodec;

{$I ffmpeg.inc}
{$REGION 'formats.h'}

(* *
  * A list of supported formats for one end of a filter link. This is used
  * during the format negotiation process to try to pick the best format to
  * use to minimize the number of necessary conversions. Each filter gives a
  * list of the formats supported by each input and output pad. The list
  * given for each pad need not be distinct - they may be references to the
  * same list of formats, as is often the case when a filter supports multiple
  * formats, but will always output the same format as it is given in input.
  *
  * In this way, a list of possible input formats and a list of possible
  * output formats are associated with each link. When a set of formats is
  * negotiated over a link, the input and output lists are merged to form a
  * new list containing only the common elements of each list. In the case
  * that there were no common elements, a format conversion is necessary.
  * Otherwise, the lists are merged, and all other links which reference
  * either of the format lists involved in the merge are also affected.
  *
  * For example, consider the filter chain:
  * filter (a) --> (b) filter (b) --> (c) filter
  *
  * where the letters in parenthesis indicate a list of formats supported on
  * the input or output of the link. Suppose the lists are as follows:
  * (a) = {A, B}
  * (b) = {A, B, C}
  * (c) = {B, C}
  *
  * First, the first link's lists are merged, yielding:
  * filter (a) --> (a) filter (a) --> (c) filter
  *
  * Notice that format list (b) now refers to the same list as filter list (a).
  * Next, the lists for the second link are merged, yielding:
  * filter (a) --> (a) filter (a) --> (a) filter
  *
  * where (a) = {B}.
  *
  * Unfortunately, when the format lists at the two ends of a link are merged,
  * we must ensure that all links which reference either pre-merge format list
  * get updated as well. Therefore, we have the format list structure store a
  * pointer to each of the pointers to itself.
*)
type
  pAVFilterFormats = ^AVFilterFormats;
  ppAVFilterFormats = ^pAVFilterFormats;
  pppAVFilterFormats = ^ppAVFilterFormats;

  AVFilterFormats = record
    nb_formats: unsigned;
    /// < number of formats
    formats: pint;
    /// < list of media formats

    refcount: unsigned;
    /// < number of references to this list
    refs: pppAVFilterFormats;
    /// < references to this list
  end;

  (* *
    * A list of supported channel layouts.
    *
    * The list works the same as AVFilterFormats, except for the following
    * differences:
    * - A list with all_layouts = 1 means all channel layouts with a known
    *   disposition; nb_channel_layouts must then be 0.
    * - A list with all_counts = 1 means all channel counts, with a known or
    *   unknown disposition; nb_channel_layouts must then be 0 and all_layouts 1.
    * - The list must not contain a layout with a known disposition and a
    *   channel count with unknown disposition with the same number of channels
    *   (e.g. AV_CH_LAYOUT_STEREO and FF_COUNT2LAYOUT(2).
  *)
  pAVFilterChannelLayouts = ^AVFilterChannelLayouts;
  ppAVFilterChannelLayouts = ^pAVFilterChannelLayouts;
  pppAVFilterChannelLayouts = ^ppAVFilterChannelLayouts;

  AVFilterChannelLayouts = record
    channel_layouts: puint64_t;
    /// < list of channel layouts
    nb_channel_layouts: int;
    /// < number of channel layouts
    all_layouts: AnsiChar;
    /// < accept any known channel layout
    all_counts: AnsiChar;
    /// < accept any channel layout or count
    refcount: unsigned;
    /// < number of references to this list
    refs: pppAVFilterChannelLayouts;
    /// < references to this list
  end;

{$ENDREGION}
{$REGION 'avformat.h'}
  (* *
    * @defgroup libavf libavformat
    * I/O and Muxing/Demuxing Library
    *
    * Libavformat (lavf) is a library for dealing with various media container
    * formats. Its main two purposes are demuxing - i.e. splitting a media file
    * into component streams, and the reverse process of muxing - writing supplied
    * data in a specified container format. It also has an @ref lavf_io
    * "I/O module" which supports a number of protocols for accessing the data (e.g.
    * file, tcp, http and others).
    * Unless you are absolutely sure you won't use libavformat's network
    * capabilities, you should also call avformat_network_init().
    *
    * A supported input format is described by an AVInputFormat struct, conversely
    * an output format is described by AVOutputFormat. You can iterate over all
    * input/output formats using the  av_demuxer_iterate / av_muxer_iterate() functions.
    * The protocols layer is not part of the public API, so you can only get the names
    * of supported protocols with the avio_enum_protocols() function.
    *
    * Main lavf structure used for both muxing and demuxing is AVFormatContext,
    * which exports all information about the file being read or written. As with
    * most Libavformat structures, its size is not part of public ABI, so it cannot be
    * allocated on stack or directly with av_malloc(). To create an
    * AVFormatContext, use avformat_alloc_context() (some functions, like
    * avformat_open_input() might do that for you).
    *
    * Most importantly an AVFormatContext contains:
    * @li the @ref AVFormatContext.iformat "input" or @ref AVFormatContext.oformat
    * "output" format. It is either autodetected or set by user for input;
    * always set by user for output.
    * @li an @ref AVFormatContext.streams "array" of AVStreams, which describe all
    * elementary streams stored in the file. AVStreams are typically referred to
    * using their index in this array.
    * @li an @ref AVFormatContext.pb "I/O context". It is either opened by lavf or
    * set by user for input, always set by user for output (unless you are dealing
    * with an AVFMT_NOFILE format).
    *
    * @section lavf_options Passing options to (de)muxers
    * It is possible to configure lavf muxers and demuxers using the @ref avoptions
    * mechanism. Generic (format-independent) libavformat options are provided by
    * AVFormatContext, they can be examined from a user program by calling
    * av_opt_next() / av_opt_find() on an allocated AVFormatContext (or its AVClass
    * from avformat_get_class()). Private (format-specific) options are provided by
    * AVFormatContext.priv_data if and only if AVInputFormat.priv_class /
    * AVOutputFormat.priv_class of the corresponding format struct is non-NULL.
    * Further options may be provided by the @ref AVFormatContext.pb "I/O context",
    * if its AVClass is non-NULL, and the protocols layer. See the discussion on
    * nesting in @ref avoptions documentation to learn how to access those.
    *
    * @section urls
    * URL strings in libavformat are made of a scheme/protocol, a ':', and a
    * scheme specific string. URLs without a scheme and ':' used for local files
    * are supported but deprecated. "file:" should be used for local files.
    *
    * It is important that the scheme string is not taken from untrusted
    * sources without checks.
    *
    * Note that some schemes/protocols are quite powerful, allowing access to
    * both local and remote files, parts of them, concatenations of them, local
    * audio and video devices and so on.
    *
    * @{
    *
    * @defgroup lavf_decoding Demuxing
    * @{
    * Demuxers read a media file and split it into chunks of data (@em packets). A
    * @ref AVPacket "packet" contains one or more encoded frames which belongs to a
    * single elementary stream. In the lavf API this process is represented by the
    * avformat_open_input() function for opening a file, av_read_frame() for
    * reading a single packet and finally avformat_close_input(), which does the
    * cleanup.
    *
    * @section lavf_decoding_open Opening a media file
    * The minimum information required to open a file is its URL, which
    * is passed to avformat_open_input(), as in the following code:
    * @code
    * const char    *url = "file:in.mp3";
    * AVFormatContext *s = NULL;
    * int ret = avformat_open_input(&s, url, NULL, NULL);
    * if (ret < 0)
    *     abort();
    * @endcode
    * The above code attempts to allocate an AVFormatContext, open the
    * specified file (autodetecting the format) and read the header, exporting the
    * information stored there into s. Some formats do not have a header or do not
    * store enough information there, so it is recommended that you call the
    * avformat_find_stream_info() function which tries to read and decode a few
    * frames to find missing information.
    *
    * In some cases you might want to preallocate an AVFormatContext yourself with
    * avformat_alloc_context() and do some tweaking on it before passing it to
    * avformat_open_input(). One such case is when you want to use custom functions
    * for reading input data instead of lavf internal I/O layer.
    * To do that, create your own AVIOContext with avio_alloc_context(), passing
    * your reading callbacks to it. Then set the @em pb field of your
    * AVFormatContext to newly created AVIOContext.
    *
    * Since the format of the opened file is in general not known until after
    * avformat_open_input() has returned, it is not possible to set demuxer private
    * options on a preallocated context. Instead, the options should be passed to
    * avformat_open_input() wrapped in an AVDictionary:
    * @code
    * AVDictionary *options = NULL;
    * av_dict_set(&options, "video_size", "640x480", 0);
    * av_dict_set(&options, "pixel_format", "rgb24", 0);
    *
    * if (avformat_open_input(&s, url, NULL, &options) < 0)
    *     abort();
    * av_dict_free(&options);
    * @endcode
    * This code passes the private options 'video_size' and 'pixel_format' to the
    * demuxer. They would be necessary for e.g. the rawvideo demuxer, since it
    * cannot know how to interpret raw video data otherwise. If the format turns
    * out to be something different than raw video, those options will not be
    * recognized by the demuxer and therefore will not be applied. Such unrecognized
    * options are then returned in the options dictionary (recognized options are
    * consumed). The calling program can handle such unrecognized options as it
    * wishes, e.g.
    * @code
    * AVDictionaryEntry *e;
    * if (e = av_dict_get(options, "", NULL, AV_DICT_IGNORE_SUFFIX)) {
    *     fprintf(stderr, "Option %s not recognized by the demuxer.\n", e->key);
    *     abort();
    * }
    * @endcode
    *
    * After you have finished reading the file, you must close it with
    * avformat_close_input(). It will free everything associated with the file.
    *
    * @section lavf_decoding_read Reading from an opened file
    * Reading data from an opened AVFormatContext is done by repeatedly calling
    * av_read_frame() on it. Each call, if successful, will return an AVPacket
    * containing encoded data for one AVStream, identified by
    * AVPacket.stream_index. This packet may be passed straight into the libavcodec
    * decoding functions avcodec_send_packet() or avcodec_decode_subtitle2() if the
    * caller wishes to decode the data.
    *
    * AVPacket.pts, AVPacket.dts and AVPacket.duration timing information will be
    * set if known. They may also be unset (i.e. AV_NOPTS_VALUE for
    * pts/dts, 0 for duration) if the stream does not provide them. The timing
    * information will be in AVStream.time_base units, i.e. it has to be
    * multiplied by the timebase to convert them to seconds.
    *
    * If AVPacket.buf is set on the returned packet, then the packet is
    * allocated dynamically and the user may keep it indefinitely.
    * Otherwise, if AVPacket.buf is NULL, the packet data is backed by a
    * static storage somewhere inside the demuxer and the packet is only valid
    * until the next av_read_frame() call or closing the file. If the caller
    * requires a longer lifetime, av_packet_make_refcounted() will ensure that
    * the data is reference counted, copying the data if necessary.
    * In both cases, the packet must be freed with av_packet_unref() when it is no
    * longer needed.
    *
    * @section lavf_decoding_seek Seeking
    * @}
    *
    * @defgroup lavf_encoding Muxing
    * @{
    * Muxers take encoded data in the form of @ref AVPacket "AVPackets" and write
    * it into files or other output bytestreams in the specified container format.
    *
    * The main API functions for muxing are avformat_write_header() for writing the
    * file header, av_write_frame() / av_interleaved_write_frame() for writing the
    * packets and av_write_trailer() for finalizing the file.
    *
    * At the beginning of the muxing process, the caller must first call
    * avformat_alloc_context() to create a muxing context. The caller then sets up
    * the muxer by filling the various fields in this context:
    *
    * - The @ref AVFormatContext.oformat "oformat" field must be set to select the
    *   muxer that will be used.
    * - Unless the format is of the AVFMT_NOFILE type, the @ref AVFormatContext.pb
    *   "pb" field must be set to an opened IO context, either returned from
    *   avio_open2() or a custom one.
    * - Unless the format is of the AVFMT_NOSTREAMS type, at least one stream must
    *   be created with the avformat_new_stream() function. The caller should fill
    *   the @ref AVStream.codecpar "stream codec parameters" information, such as the
    *   codec @ref AVCodecParameters.codec_type "type", @ref AVCodecParameters.codec_id
    *   "id" and other parameters (e.g. width / height, the pixel or sample format,
    *   etc.) as known. The @ref AVStream.time_base "stream timebase" should
    *   be set to the timebase that the caller desires to use for this stream (note
    *   that the timebase actually used by the muxer can be different, as will be
    *   described later).
    * - It is advised to manually initialize only the relevant fields in
    *   AVCodecParameters, rather than using @ref avcodec_parameters_copy() during
    *   remuxing: there is no guarantee that the codec context values remain valid
    *   for both input and output format contexts.
    * - The caller may fill in additional information, such as @ref
    *   AVFormatContext.metadata "global" or @ref AVStream.metadata "per-stream"
    *   metadata, @ref AVFormatContext.chapters "chapters", @ref
    *   AVFormatContext.programs "programs", etc. as described in the
    *   AVFormatContext documentation. Whether such information will actually be
    *   stored in the output depends on what the container format and the muxer
    *   support.
    *
    * When the muxing context is fully set up, the caller must call
    * avformat_write_header() to initialize the muxer internals and write the file
    * header. Whether anything actually is written to the IO context at this step
    * depends on the muxer, but this function must always be called. Any muxer
    * private options must be passed in the options parameter to this function.
    *
    * The data is then sent to the muxer by repeatedly calling av_write_frame() or
    * av_interleaved_write_frame() (consult those functions' documentation for
    * discussion on the difference between them; only one of them may be used with
    * a single muxing context, they should not be mixed). Do note that the timing
    * information on the packets sent to the muxer must be in the corresponding
    * AVStream's timebase. That timebase is set by the muxer (in the
    * avformat_write_header() step) and may be different from the timebase
    * requested by the caller.
    *
    * Once all the data has been written, the caller must call av_write_trailer()
    * to flush any buffered packets and finalize the output file, then close the IO
    * context (if any) and finally free the muxing context with
    * avformat_free_context().
    * @}
    *
    * @defgroup lavf_io I/O Read/Write
    * @{
    * @section lavf_io_dirlist Directory listing
    * The directory listing API makes it possible to list files on remote servers.
    *
    * Some of possible use cases:
    * - an "open file" dialog to choose files from a remote location,
    * - a recursive media finder providing a player with an ability to play all
    * files from a given directory.
    *
    * @subsection lavf_io_dirlist_open Opening a directory
    * At first, a directory needs to be opened by calling avio_open_dir()
    * supplied with a URL and, optionally, ::AVDictionary containing
    * protocol-specific parameters. The function returns zero or positive
    * integer and allocates AVIODirContext on success.
    *
    * @code
    * AVIODirContext *ctx = NULL;
    * if (avio_open_dir(&ctx, "smb://example.com/some_dir", NULL) < 0) {
    *     fprintf(stderr, "Cannot open directory.\n");
    *     abort();
    * }
    * @endcode
    *
    * This code tries to open a sample directory using smb protocol without
    * any additional parameters.
    *
    * @subsection lavf_io_dirlist_read Reading entries
    * Each directory's entry (i.e. file, another directory, anything else
    * within ::AVIODirEntryType) is represented by AVIODirEntry.
    * Reading consecutive entries from an opened AVIODirContext is done by
    * repeatedly calling avio_read_dir() on it. Each call returns zero or
    * positive integer if successful. Reading can be stopped right after the
    * NULL entry has been read -- it means there are no entries left to be
    * read. The following code reads all entries from a directory associated
    * with ctx and prints their names to standard output.
    * @code
    * AVIODirEntry *entry = NULL;
    * for (;;) {
    *     if (avio_read_dir(ctx, &entry) < 0) {
    *         fprintf(stderr, "Cannot list directory.\n");
    *         abort();
    *     }
    *     if (!entry)
    *         break;
    *     printf("%s\n", entry->name);
    *     avio_free_directory_entry(&entry);
    * }
    * @endcode
    * @}
    *
    * @defgroup lavf_codec Demuxers
    * @{
    * @defgroup lavf_codec_native Native Demuxers
    * @{
    * @}
    * @defgroup lavf_codec_wrappers External library wrappers
    * @{
    * @}
    * @}
    * @defgroup lavf_protos I/O Protocols
    * @{
    * @}
    * @defgroup lavf_internal Internal
    * @{
    * @}
    * @}
  *)

  // #include <time.h>
  // #include <stdio.h>  (* FILE *)
  // #include "libavcodec/avcodec.h"
  // #include "libavutil/dict.h"
  // #include "libavutil/log.h"
  // #include "avio.h"
  // #include "libavformat/version.h"

{$REGION 'avio.h'}

const
  (* *
    * Seeking works like for a local file.
  *)
  AVIO_SEEKABLE_NORMAL = (1 shl 0);

  (* *
    * Seeking by timestamp with avio_seek_time() is possible.
  *)
  AVIO_SEEKABLE_TIME = (1 shl 1);

type
  (* *
    * Callback for checking whether to abort blocking functions.
    * AVERROR_EXIT is returned in this case by the interrupted
    * function. During blocking operations, callback is called with
    * opaque as parameter. If the callback returns 1, the
    * blocking operation will be aborted.
    *
    * No members can be added to this struct without a major bump, if
    * new elements have been added after this struct in AVFormatContext
    * or AVIOContext.
  *)
  pAVIOInterruptCB = ^AVIOInterruptCB;

  AVIOInterruptCB = record
    // int (*callback)(void*);
    callback: function(p: pointer): int; cdecl;
    opaque: pointer;
  end;

  (* *
    * Directory entry types.
  *)
  AVIODirEntryType = ( //
    AVIO_ENTRY_UNKNOWN, AVIO_ENTRY_BLOCK_DEVICE, AVIO_ENTRY_CHARACTER_DEVICE, AVIO_ENTRY_DIRECTORY, AVIO_ENTRY_NAMED_PIPE, AVIO_ENTRY_SYMBOLIC_LINK,
    AVIO_ENTRY_SOCKET, AVIO_ENTRY_FILE, AVIO_ENTRY_SERVER, AVIO_ENTRY_SHARE, AVIO_ENTRY_WORKGROUP);

  (* *
    * Describes single entry of the directory.
    *
    * Only name and type fields are guaranteed be set.
    * Rest of fields are protocol or/and platform dependent and might be unknown.
  *)
  pAVIODirEntry = ^AVIODirEntry;

  AVIODirEntry = record
    name: PAnsiChar; (* *< Filename *)
    _type: int;      (* *< Type of the entry *)
    utf8: int; (* *< Set to 1 when name is encoded with UTF-8, 0 otherwise.
      Name can be encoded with UTF-8 even though 0 is set. *)
    size: int64_t; (* *< File size in bytes, -1 if unknown. *)
    modification_timestamp: int64_t; (* *< Time of last modification in microseconds since unix
      epoch, -1 if unknown. *)
    access_timestamp: int64_t; (* *< Time of last access in microseconds since unix epoch,
      -1 if unknown. *)
    status_change_timestamp: int64_t; (* *< Time of last status change in microseconds since unix
      epoch, -1 if unknown. *)
    user_id: int64_t;  (* *< User ID of owner, -1 if unknown. *)
    group_id: int64_t; (* *< Group ID of owner, -1 if unknown. *)
    filemode: int64_t; (* *< Unix file mode, -1 if unknown. *)
  end;

  pURLContext = ^URLContext;

  URLContext = record
  end;

  pAVIODirContext = ^AVIODirContext;

  AVIODirContext = record
    url_context: pURLContext;
  end;

  (* *
    * Different data types that can be returned via the AVIO
    * write_data_type callback.
  *)
  AVIODataMarkerType = (
    (* *
      * Header data; this needs to be present for the stream to be decodeable.
    *)
    AVIO_DATA_MARKER_HEADER,
    (* *
      * A point in the output bytestream where a decoder can start decoding
      * (i.e. a keyframe). A demuxer/decoder given the data flagged with
      * AVIO_DATA_MARKER_HEADER, followed by any AVIO_DATA_MARKER_SYNC_POINT,
      * should give decodeable results.
    *)
    AVIO_DATA_MARKER_SYNC_POINT,
    (* *
      * A point in the output bytestream where a demuxer can start parsing
      * (for non self synchronizing bytestream formats). That is, any
      * non-keyframe packet start point.
    *)
    AVIO_DATA_MARKER_BOUNDARY_POINT,
    (* *
      * This is any, unlabelled data. It can either be a muxer not marking
      * any positions at all, it can be an actual boundary/sync point
      * that the muxer chooses not to mark, or a later part of a packet/fragment
      * that is cut into multiple write callbacks due to limited IO buffer size.
    *)
    AVIO_DATA_MARKER_UNKNOWN,
    (* *
      * Trailer data, which doesn't contain actual content, but only for
      * finalizing the output file.
    *)
    AVIO_DATA_MARKER_TRAILER,
    (* *
      * A point in the output bytestream where the underlying AVIOContext might
      * flush the buffer depending on latency or buffering requirements. Typically
      * means the end of a packet.
    *)
    AVIO_DATA_MARKER_FLUSH_POINT);

  (* *
    * Bytestream IO Context.
    * New fields can be added to the end with minor version bumps.
    * Removal, reordering and changes to existing fields require a major
    * version bump.
    * sizeof(AVIOContext) must not be used outside libav*.
    *
    * @note None of the function pointers in AVIOContext should be called
    *       directly, they should only be set by the client application
    *       when implementing custom I/O. Normally these are set to the
    *       function pointers specified in avio_alloc_context()
  *)
  pAVIOContext = ^AVIOContext;

  AVIOContext = record
    (* *
      * A class for private options.
      *
      * If this AVIOContext is created by avio_open2(), av_class is set and
      * passes the options down to protocols.
      *
      * If this AVIOContext is manually allocated, then av_class may be set by
      * the caller.
      *
      * warning -- this field can be NULL, be sure to not pass this AVIOContext
      * to any av_opt_* functions in that case.
    *)
    av_class: pAVClass;

    (*
      * The following shows the relationship between buffer, buf_ptr,
      * buf_ptr_max, buf_end, buf_size, and pos, when reading and when writing
      * (since AVIOContext is used for both):
      *
      **********************************************************************************
      *                                   READING
      **********************************************************************************
      *
      *                            |              buffer_size              |
      *                            |---------------------------------------|
      *                            |                                       |
      *
      *                         buffer          buf_ptr       buf_end
      *                            +---------------+-----------------------+
      *                            |/ / / / / / / /|/ / / / / / /|         |
      *  read buffer:              |/ / consumed / | to be read /|         |
      *                            |/ / / / / / / /|/ / / / / / /|         |
      *                            +---------------+-----------------------+
      *
      *                                                         pos
      *              +-------------------------------------------+-----------------+
      *  input file: |                                           |                 |
      *              +-------------------------------------------+-----------------+
      *
      *
      **********************************************************************************
      *                                   WRITING
      **********************************************************************************
      *
      *                             |          buffer_size                 |
      *                             |--------------------------------------|
      *                             |                                      |
      *
      *                                                buf_ptr_max
      *                          buffer                 (buf_ptr)       buf_end
      *                             +-----------------------+--------------+
      *                             |/ / / / / / / / / / / /|              |
      *  write buffer:              | / / to be flushed / / |              |
      *                             |/ / / / / / / / / / / /|              |
      *                             +-----------------------+--------------+
      *                               buf_ptr can be in this
      *                               due to a backward seek
      *
      *                            pos
      *               +-------------+----------------------------------------------+
      *  output file: |             |                                              |
      *               +-------------+----------------------------------------------+
      *
    *)
    buffer: punsignedchar;  (* *< Start of the buffer. *)
    buffer_size: int;       (* *< Maximum buffer size *)
    buf_ptr: punsignedchar; (* *< Current position in the buffer *)
    buf_end: punsignedchar; (* *< End of the data, may be less than
      buffer+buffer_size if the read function returned
      less data than requested, e.g. for streams where
      no more data has been received yet. *)
    opaque: pointer; (* *< A private pointer, passed to the read/write/seek/...
      functions. *)
    // int (*read_packet)(void *opaque, uint8_t *buf, int buf_size);
    read_packet: function(opaque: pointer; buf: puint8_t; buf_size: int): int; cdecl;
    // int (*write_packet)(void *opaque, uint8_t *buf, int buf_size);
    write_packet: function(opaque: pointer; buf: puint8_t; buf_size: int): int; cdecl;
    // int64_t (*seek)(void *opaque, int64_t offset, int whence);
    seek: function(opaque: pointer; offset: int64_t; whence: int): int64_t; cdecl;
    pos: int64_t;     (* *< position in the file of the current buffer *)
    eof_reached: int; (* *< true if was unable to read due to error or eof *)
    write_flag: int;  (* *< true if open for writing *)
    max_packet_size: int;
    checksum: unsigned_long;
    checksum_ptr: punsignedchar;
    // unsigned long (*update_checksum)(unsigned long checksum, const uint8_t *buf, unsigned int size);
    update_checksum: function(checksum: unsigned_long; const buf: puint8_t; size: unsigned_int): unsigned_long; cdecl;
    error: int; (* *< contains the error code or 0 if no error happened *)
    (* *
      * Pause or resume playback for network streaming protocols - e.g. MMS.
    *)
    // int (*read_pause)(void *opaque, int pause);
    read_pause: function(opaque: pointer; pause: int): int; cdecl;
    (* *
      * Seek to a given timestamp in stream with the specified stream_index.
      * Needed for some network streaming protocols which don't support seeking
      * to byte position.
    *)
    // int64_t (*read_seek)(void *opaque, int stream_index, int64_t timestamp, int flags);
    read_seek: function(opaque: pointer; stream_index: int; timestamp: int64_t; flags: int): int64_t; cdecl;
    (* *
      * A combination of AVIO_SEEKABLE_ flags or 0 when the stream is not seekable.
    *)
    seekable: int;

    (* *
      * max filesize, used to limit allocations
      * This field is internal to libavformat and access from outside is not allowed.
    *)
    maxsize: int64_t;

    (* *
      * avio_read and avio_write should if possible be satisfied directly
      * instead of going through a buffer, and avio_seek will always
      * call the underlying seek function directly.
    *)
    direct: int;

    (* *
      * Bytes read statistic
      * This field is internal to libavformat and access from outside is not allowed.
    *)
    bytes_read: int64_t;

    (* *
      * seek statistic
      * This field is internal to libavformat and access from outside is not allowed.
    *)
    seek_count: int;

    (* *
      * writeout statistic
      * This field is internal to libavformat and access from outside is not allowed.
    *)
    writeout_count: int;

    (* *
      * Original buffer size
      * used internally after probing and ensure seekback to reset the buffer size
      * This field is internal to libavformat and access from outside is not allowed.
    *)
    orig_buffer_size: int;

    (* *
      * Threshold to favor readahead over seek.
      * This is current internal only, do not use from outside.
    *)
    short_seek_threshold: int;

    (* *
      * ',' separated list of allowed protocols.
    *)
    protocol_whitelist: PAnsiChar;

    (* *
      * ',' separated list of disallowed protocols.
    *)
    protocol_blacklist: PAnsiChar;

    (* *
      * A callback that is used instead of write_packet.
    *)
    // int (*write_data_type)(void *opaque, uint8_t *buf, int buf_size, enum AVIODataMarkerType type, int64_t time);
    write_data_type: function(opaque: pointer; buf: puint8_t; buf_size: int; _type: AVIODataMarkerType; time: int64_t): int; cdecl;
    (* *
      * If set, don't call write_data_type separately for AVIO_DATA_MARKER_BOUNDARY_POINT,
      * but ignore them and treat them as AVIO_DATA_MARKER_UNKNOWN (to avoid needlessly
      * small chunks of data returned from the callback).
    *)
    ignore_boundary_point: int;

    (* *
      * Internal, not meant to be used from outside of AVIOContext.
    *)
    current_type: AVIODataMarkerType;
    last_time: int64_t;

    (* *
      * A callback that is used instead of short_seek_threshold.
      * This is current internal only, do not use from outside.
    *)
    // int (*short_seek_get)(void *opaque);
    short_seek_get: function(opaque: pointer): int; cdecl;

    written: int64_t;

    (* *
      * Maximum reached position before a backward seek in the write buffer,
      * used keeping track of already written data for a later flush.
    *)
    buf_ptr_max: punsigned_char;

    (* *
      * Try to buffer at least this amount of data before flushing it
    *)
    min_packet_size: int;
  end;

  (* *
    * Return the name of the protocol that will handle the passed URL.
    *
    * NULL is returned if no protocol could be found for the given URL.
    *
    * @return Name of the protocol or NULL.
  *)
  // const char *avio_find_protocol_name(const char *url);
function avio_find_protocol_name(const url: PAnsiChar): PAnsiChar; cdecl; external avformat_dll;

(* *
  * Return AVIO_FLAG_* access flags corresponding to the access permissions
  * of the resource in url, or a negative value corresponding to an
  * AVERROR code in case of failure. The returned access flags are
  * masked by the value in flags.
  *
  * @note This function is intrinsically unsafe, in the sense that the
  * checked resource may change its existence or permission status from
  * one call to another. Thus you should not trust the returned value,
  * unless you are sure that no other processes are accessing the
  * checked resource.
*)
// int avio_check(const char *url, int flags);
function avio_check(const url: PAnsiChar; flags: int): int; cdecl; external avformat_dll;
(* *
  * Move or rename a resource.
  *
  * @note url_src and url_dst should share the same protocol and authority.
  *
  * @param url_src url to resource to be moved
  * @param url_dst new url to resource if the operation succeeded
  * @return >=0 on success or negative on error.
*)
// int avpriv_io_move(const char *url_src, const char *url_dst);
function avpriv_io_move(const url_src: PAnsiChar; const url_dst: PAnsiChar): int; cdecl; external avformat_dll;
(* *
  * Delete a resource.
  *
  * @param url resource to be deleted.
  * @return >=0 on success or negative on error.
*)
// int avpriv_io_delete(const char *url);
function avpriv_io_delete(const url: PAnsiChar): int; cdecl; external avformat_dll;
(* *
  * Open directory for reading.
  *
  * @param s       directory read context. Pointer to a NULL pointer must be passed.
  * @param url     directory to be listed.
  * @param options A dictionary filled with protocol-private options. On return
  *                this parameter will be destroyed and replaced with a dictionary
  *                containing options that were not found. May be NULL.
  * @return >=0 on success or negative on error.
*)
// int avio_open_dir(AVIODirContext **s, const char *url, AVDictionary **options);
function avio_open_dir(var s: pAVIODirContext; const url: PAnsiChar; options: ppAVDictionary): int; cdecl; external avformat_dll;
(* *
  * Get next directory entry.
  *
  * Returned entry must be freed with avio_free_directory_entry(). In particular
  * it may outlive AVIODirContext.
  *
  * @param s         directory read context.
  * @param[out] next next entry or NULL when no more entries.
  * @return >=0 on success or negative on error. End of list is not considered an
  *             error.
*)
// int avio_read_dir(AVIODirContext *s, AVIODirEntry **next);
function avio_read_dir(s: pAVIODirContext; var next: pAVIODirEntry): int; cdecl; external avformat_dll;
(* *
  * Close directory.
  *
  * @note Entries created using avio_read_dir() are not deleted and must be
  * freeded with avio_free_directory_entry().
  *
  * @param s         directory read context.
  * @return >=0 on success or negative on error.
*)
// int avio_close_dir(AVIODirContext **s);
function avio_close_dir(var s: pAVIODirContext): int; cdecl; external avformat_dll;
(* *
  * Free entry allocated by avio_read_dir().
  *
  * @param entry entry to be freed.
*)
// void avio_free_directory_entry(AVIODirEntry **entry);
procedure avio_free_directory_entry(var entry: pAVIODirEntry); cdecl; external avformat_dll;
(* *
  * Allocate and initialize an AVIOContext for buffered I/O. It must be later
  * freed with avio_context_free().
  *
  * @param buffer Memory block for input/output operations via AVIOContext.
  *        The buffer must be allocated with av_malloc() and friends.
  *        It may be freed and replaced with a new buffer by libavformat.
  *        AVIOContext.buffer holds the buffer currently in use,
  *        which must be later freed with av_free().
  * @param buffer_size The buffer size is very important for performance.
  *        For protocols with fixed blocksize it should be set to this blocksize.
  *        For others a typical size is a cache page, e.g. 4kb.
  * @param write_flag Set to 1 if the buffer should be writable, 0 otherwise.
  * @param opaque An opaque pointer to user-specific data.
  * @param read_packet  A function for refilling the buffer, may be NULL.
  *                     For stream protocols, must never return 0 but rather
  *                     a proper AVERROR code.
  * @param write_packet A function for writing the buffer contents, may be NULL.
  *        The function may not change the input buffers content.
  * @param seek A function for seeking to specified byte position, may be NULL.
  *
  * @return Allocated AVIOContext or NULL on failure.
*)

// AVIOContext *avio_alloc_context(
// unsigned char *buffer,
// int buffer_size,
// int write_flag,
// void *opaque,
// int (*read_packet)(void *opaque, uint8_t *buf, int buf_size),
// int (*write_packet)(void *opaque, uint8_t *buf, int buf_size),
// int64_t (*seek)(void *opaque, int64_t offset, int whence));

type
  // int (*read_packet)(void *opaque, uint8_t *buf, int buf_size),
  Tavio_alloc_context_read_packet = function(opaque: pointer; buf: puint8_t; buf_size: int): int; cdecl;
  // int (*write_packet)(void *opaque, uint8_t *buf, int buf_size),
  Tavio_alloc_context_write_packet = Tavio_alloc_context_read_packet;
  // int64_t (*seek)(void *opaque, int64_t offset, int whence)
  Tavio_alloc_context_seek = function(opaque: pointer; offset: int64_t; whence: int): int64_t; cdecl;

function avio_alloc_context(buffer: punsigned_char; buffer_size: int; write_flag: int; opaque: pointer; read_packet: Tavio_alloc_context_read_packet;
  write_packet: Tavio_alloc_context_write_packet; seek: Tavio_alloc_context_seek): pAVIOContext; cdecl; external avformat_dll;
(* *
  * Free the supplied IO context and everything associated with it.
  *
  * @param s Double pointer to the IO context. This function will write NULL
  * into s.
*)
// void avio_context_free(AVIOContext **s);
procedure avio_context_free(var s: pAVIOContext); cdecl; external avformat_dll;

// void avio_w8(AVIOContext *s, int b);
procedure avio_w8(s: pAVIOContext; b: int); cdecl; external avformat_dll;

// void avio_write(AVIOContext *s, const unsigned char *buf, int size);
procedure avio_write(s: pAVIOContext; const buf: punsigned_char; size: int); cdecl; external avformat_dll;

// void avio_wl64(AVIOContext *s, uint64_t val);
procedure avio_wl64(s: pAVIOContext; val: uint64_t); cdecl; external avformat_dll;

// void avio_wb64(AVIOContext *s, uint64_t val);
procedure avio_wb64(s: pAVIOContext; val: uint64_t); cdecl; external avformat_dll;

// void avio_wl32(AVIOContext *s, unsigned int val);
procedure avio_wl32(s: pAVIOContext; val: unsigned_int); cdecl; external avformat_dll;

// void avio_wb32(AVIOContext *s, unsigned int val);
procedure avio_wb32(s: pAVIOContext; val: unsigned_int); cdecl; external avformat_dll;

// void avio_wl24(AVIOContext *s, unsigned int val);
procedure avio_wl24(s: pAVIOContext; val: unsigned_int); cdecl; external avformat_dll;

// void avio_wb24(AVIOContext *s, unsigned int val);
procedure avio_wb24(s: pAVIOContext; val: unsigned_int); cdecl; external avformat_dll;

// void avio_wl16(AVIOContext *s, unsigned int val);
procedure avio_wl16(s: pAVIOContext; val: unsigned_int); cdecl; external avformat_dll;

// void avio_wb16(AVIOContext *s, unsigned int val);
procedure avio_wb16(s: pAVIOContext; val: unsigned_int); cdecl; external avformat_dll;
(* *
  * Write a NULL-terminated string.
  * @return number of bytes written.
*)
// int avio_put_str(AVIOContext *s, const char *str);
function avio_put_str(s: pAVIOContext; const str: PAnsiChar): int; cdecl; external avformat_dll;
(* *
  * Convert an UTF-8 string to UTF-16LE and write it.
  * @param s the AVIOContext
  * @param str NULL-terminated UTF-8 string
  *
  * @return number of bytes written.
*)
// int avio_put_str16le(AVIOContext *s, const char *str);
function avio_put_str16le(s: pAVIOContext; const str: PAnsiChar): int; cdecl; external avformat_dll;
(* *
  * Convert an UTF-8 string to UTF-16BE and write it.
  * @param s the AVIOContext
  * @param str NULL-terminated UTF-8 string
  *
  * @return number of bytes written.
*)
// int avio_put_str16be(AVIOContext *s, const char *str);
function avio_put_str16be(s: pAVIOContext; const str: PAnsiChar): int; cdecl; external avformat_dll;
(* *
  * Mark the written bytestream as a specific type.
  *
  * Zero-length ranges are omitted from the output.
  *
  * @param time the stream time the current bytestream pos corresponds to
  *             (in AV_TIME_BASE units), or AV_NOPTS_VALUE if unknown or not
  *             applicable
  * @param type the kind of data written starting at the current pos
*)
// void avio_write_marker(AVIOContext *s, int64_t time, enum AVIODataMarkerType type);
procedure avio_write_marker(s: pAVIOContext; time: int64_t; _type: AVIODataMarkerType); cdecl; external avformat_dll;

const
  (* *
    * ORing this as the "whence" parameter to a seek function causes it to
    * return the filesize without seeking anywhere. Supporting this is optional.
    * If it is not supported then the seek function will return <0.
  *)
  AVSEEK_SIZE = $10000;

  (* *
    * Passing this flag as the "whence" parameter to a seek function causes it to
    * seek by any means (like reopening and linear reading) or other normally unreasonable
    * means that can be extremely slow.
    * This may be ignored by the seek code.
  *)
  AVSEEK_FORCE = $20000;

  (* *
    * fseek() equivalent for AVIOContext.
    * @return new position or AVERROR.
  *)
  // int64_t avio_seek(AVIOContext *s, int64_t offset, int whence);
function avio_seek(s: pAVIOContext; offset: int64_t; whence: int): int64_t; cdecl; external avformat_dll;
(* *
  * Skip given number of bytes forward
  * @return new position or AVERROR.
*)
// int64_t avio_skip(AVIOContext *s, int64_t offset);
function avio_skip(s: pAVIOContext; offset: int64_t): int64_t; cdecl; external avformat_dll;
(* *
  * ftell() equivalent for AVIOContext.
  * @return position or AVERROR.
*)
// static av_always_inline int64_t avio_tell(AVIOContext *s)
// {
// return avio_seek(s, 0, SEEK_CUR);
// }
function avio_tell(s: pAVIOContext): int64_t; inline;

(* *
  * Get the filesize.
  * @return filesize or AVERROR
*)
// int64_t avio_size(AVIOContext *s);
function avio_size(s: pAVIOContext): int64_t; cdecl; external avformat_dll;
(* *
  * Similar to feof() but also returns nonzero on read errors.
  * @return non zero if and only if at end of file or a read error happened when reading.
*)
// int avio_feof(AVIOContext *s);
function avio_feof(s: pAVIOContext): int; cdecl; external avformat_dll;

(*
  * Writes a formatted string to the context.
  * @return number of bytes written, < 0 on error.
*)
// int avio_printf(AVIOContext *s, const char *fmt, ...) av_printf_format(2, 3);

// --> 4.2.2
(* *
  * Write a NULL terminated array of strings to the context.
  * Usually you don't need to use this function directly but its macro wrapper,
  * avio_print.
*)
// void avio_print_string_array(AVIOContext *s, const char *strings[]);
//procedure avio_print_string_array(s: pAVIOContext; const strings: pAnsiCharArray); cdecl; external avformat_dll; //4.2.2

(*
  * Write strings (const char* ) to the context.
  * This is a convenience macro around avio_print_string_array and it
  * automatically creates the string array from the variable argument list.
  * For simple string concatenations this function is more performant than using
  * avio_printf since it does not need a temporary buffer.
*)
// #define avio_print(s, ...) avio_print_string_array(s, (const char*[]){__VA_ARGS__, NULL})
// <-- 4.2.2

(* *
  * Force flushing of buffered data.
  *
  * For write streams, force the buffered data to be immediately written to the output,
  * without to wait to fill the internal buffer.
  *
  * For read streams, discard all currently buffered data, and advance the
  * reported file position to that of the underlying stream. This does not
  * read new data, and does not perform any seeks.
*)
// void avio_flush(AVIOContext *s);
procedure avio_flush(s: pAVIOContext); cdecl; external avformat_dll;
(* *
  * Read size bytes from AVIOContext into buf.
  * @return number of bytes read or AVERROR
*)
// int avio_read(AVIOContext *s, unsigned char *buf, int size);
function avio_read(s: pAVIOContext; buf: punsigned_char; size: int): int; cdecl; external avformat_dll;
(* *
  * Read size bytes from AVIOContext into buf. Unlike avio_read(), this is allowed
  * to read fewer bytes than requested. The missing bytes can be read in the next
  * call. This always tries to read at least 1 byte.
  * Useful to reduce latency in certain cases.
  * @return number of bytes read or AVERROR
*)
// int avio_read_partial(AVIOContext *s, unsigned char *buf, int size);
function avio_read_partial(s: pAVIOContext; buf: punsigned_char; size: int): int; cdecl; external avformat_dll;
(* *
  * @name Functions for reading from AVIOContext
  * @{
  *
  * @note return 0 if EOF, so you cannot use it if EOF handling is
  *       necessary
*)
// int avio_r8  (AVIOContext *s);
function avio_r8(s: pAVIOContext): int; cdecl; external avformat_dll;
// unsigned int avio_rl16(AVIOContext *s);
function avio_rl16(s: pAVIOContext): unsigned_int; cdecl; external avformat_dll;

// unsigned int avio_rl24(AVIOContext *s);
function avio_rl24(s: pAVIOContext): unsigned_int; cdecl; external avformat_dll;

// unsigned int avio_rl32(AVIOContext *s);
function avio_rl32(s: pAVIOContext): unsigned_int; cdecl; external avformat_dll;

// uint64_t avio_rl64(AVIOContext *s);
function avio_rl64(s: pAVIOContext): uint64_t; cdecl; external avformat_dll;

// unsigned int avio_rb16(AVIOContext *s);
function avio_rb16(s: pAVIOContext): unsigned_int; cdecl; external avformat_dll;

// unsigned int avio_rb24(AVIOContext *s);
function avio_rb24(s: pAVIOContext): unsigned_int; cdecl; external avformat_dll;

// unsigned int avio_rb32(AVIOContext *s);
function avio_rb32(s: pAVIOContext): unsigned_int; cdecl; external avformat_dll;

// uint64_t avio_rb64(AVIOContext *s);
function avio_rb64(s: pAVIOContext): uint64_t; cdecl; external avformat_dll;
(* *
  * @}
*)

(* *
  * Read a string from pb into buf. The reading will terminate when either
  * a NULL character was encountered, maxlen bytes have been read, or nothing
  * more can be read from pb. The result is guaranteed to be NULL-terminated, it
  * will be truncated if buf is too small.
  * Note that the string is not interpreted or validated in any way, it
  * might get truncated in the middle of a sequence for multi-byte encodings.
  *
  * @return number of bytes read (is always <= maxlen).
  * If reading ends on EOF or error, the return value will be one more than
  * bytes actually read.
*)
// int avio_get_str(AVIOContext *pb, int maxlen, char *buf, int buflen);
function avio_get_str(pb: pAVIOContext; maxlen: int; buf: PAnsiChar; buflen: int): int; cdecl; external avformat_dll;
(* *
  * Read a UTF-16 string from pb and convert it to UTF-8.
  * The reading will terminate when either a null or invalid character was
  * encountered or maxlen bytes have been read.
  * @return number of bytes read (is always <= maxlen)
*)
// int avio_get_str16le(AVIOContext *pb, int maxlen, char *buf, int buflen);
function avio_get_str16le(pb: pAVIOContext; maxlen: int; buf: PAnsiChar; buflen: int): int; cdecl; external avformat_dll;

// int avio_get_str16be(AVIOContext *pb, int maxlen, char *buf, int buflen);
function avio_get_str16be(pb: pAVIOContext; maxlen: int; buf: PAnsiChar; buflen: int): int; cdecl; external avformat_dll;

const
  (* *
    * @name URL open modes
    * The flags argument to avio_open must be one of the following
    * constants, optionally ORed with other flags.
    *
  *)
  AVIO_FLAG_READ       = 1; (* *< read-only *)
  AVIO_FLAG_WRITE      = 2; (* *< write-only *)
  AVIO_FLAG_READ_WRITE = (AVIO_FLAG_READ or AVIO_FLAG_WRITE); (* *< read-write pseudo flag *)

  (* *
    * Use non-blocking mode.
    * If this flag is set, operations on the context will return
    * AVERROR(EAGAIN) if they can not be performed immediately.
    * If this flag is not set, operations on the context will never return
    * AVERROR(EAGAIN).
    * Note that this flag does not affect the opening/connecting of the
    * context. Connecting a protocol will always block if necessary (e.g. on
    * network protocols) but never hang (e.g. on busy devices).
    * Warning: non-blocking protocols is work-in-progress; this flag may be
    * silently ignored.
  *)
  AVIO_FLAG_NONBLOCK = 8;

  (* *
    * Use direct mode.
    * avio_read and avio_write should if possible be satisfied directly
    * instead of going through a buffer, and avio_seek will always
    * call the underlying seek function directly.
  *)
  AVIO_FLAG_DIRECT = $8000;

  (* *
    * Create and initialize a AVIOContext for accessing the
    * resource indicated by url.
    * @note When the resource indicated by url has been opened in
    * read+write mode, the AVIOContext can be used only for writing.
    *
    * @param s Used to return the pointer to the created AVIOContext.
    * In case of failure the pointed to value is set to NULL.
    * @param url resource to access
    * @param flags flags which control how the resource indicated by url
    * is to be opened
    * @return >= 0 in case of success, a negative value corresponding to an
    * AVERROR code in case of failure
  *)
  // int avio_open(AVIOContext **s, const char *url, int flags);
function avio_open(var s: pAVIOContext; const url: PAnsiChar; flags: int): int; cdecl; external avformat_dll;
(* *
  * Create and initialize a AVIOContext for accessing the
  * resource indicated by url.
  * @note When the resource indicated by url has been opened in
  * read+write mode, the AVIOContext can be used only for writing.
  *
  * @param s Used to return the pointer to the created AVIOContext.
  * In case of failure the pointed to value is set to NULL.
  * @param url resource to access
  * @param flags flags which control how the resource indicated by url
  * is to be opened
  * @param int_cb an interrupt callback to be used at the protocols level
  * @param options  A dictionary filled with protocol-private options. On return
  * this parameter will be destroyed and replaced with a dict containing options
  * that were not found. May be NULL.
  * @return >= 0 in case of success, a negative value corresponding to an
  * AVERROR code in case of failure
*)
// int avio_open2(AVIOContext **s, const char *url, int flags, const AVIOInterruptCB *int_cb, AVDictionary **options);
function avio_open2(var s: pAVIOContext; const url: PAnsiChar; flags: int; const int_cb: pAVIOInterruptCB; var options: pAVDictionary): int; cdecl;
  external avformat_dll;
(* *
  * Close the resource accessed by the AVIOContext s and free it.
  * This function can only be used if s was opened by avio_open().
  *
  * The internal buffer is automatically flushed before closing the
  * resource.
  *
  * @return 0 on success, an AVERROR < 0 on error.
  * @see avio_closep
*)
// int avio_close(AVIOContext *s);
function avio_close(s: pAVIOContext): int; cdecl; external avformat_dll;
(* *
  * Close the resource accessed by the AVIOContext *s, free it
  * and set the pointer pointing to it to NULL.
  * This function can only be used if s was opened by avio_open().
  *
  * The internal buffer is automatically flushed before closing the
  * resource.
  *
  * @return 0 on success, an AVERROR < 0 on error.
  * @see avio_close
*)
// int avio_closep(AVIOContext **s);
function avio_closep(var s: pAVIOContext): int; cdecl; external avformat_dll;
(* *
  * Open a write only memory stream.
  *
  * @param s new IO context
  * @return zero if no error.
*)
// int avio_open_dyn_buf(AVIOContext **s);
function avio_open_dyn_buf(var s: pAVIOContext): int; cdecl; external avformat_dll;
(* *
  * Return the written size and a pointer to the buffer.
  * The AVIOContext stream is left intact.
  * The buffer must NOT be freed.
  * No padding is added to the buffer.
  *
  * @param s IO context
  * @param pbuffer pointer to a byte buffer
  * @return the length of the byte buffer
*)
// int avio_get_dyn_buf(AVIOContext *s, uint8_t **pbuffer);
function avio_get_dyn_buf(s: pAVIOContext; var pbuffer: puint8_t): int; cdecl; external avformat_dll;
(* *
  * Return the written size and a pointer to the buffer. The buffer
  * must be freed with av_free().
  * Padding of AV_INPUT_BUFFER_PADDING_SIZE is added to the buffer.
  *
  * @param s IO context
  * @param pbuffer pointer to a byte buffer
  * @return the length of the byte buffer
*)
// int avio_close_dyn_buf(AVIOContext *s, uint8_t **pbuffer);
function avio_close_dyn_buf(s: pAVIOContext; var pbuffer: puint8_t): int; cdecl; external avformat_dll;
(* *
  * Iterate through names of available protocols.
  *
  * @param opaque A private pointer representing current protocol.
  *        It must be a pointer to NULL on first iteration and will
  *        be updated by successive calls to avio_enum_protocols.
  * @param output If set to 1, iterate over output protocols,
  *               otherwise over input protocols.
  *
  * @return A static string containing the name of current protocol or NULL
*)
// const char *avio_enum_protocols(void **opaque, int output);
function avio_enum_protocols(var opaque: pointer; output: int): PAnsiChar; cdecl; external avformat_dll;
(* *
  * Pause and resume playing - only meaningful if using a network streaming
  * protocol (e.g. MMS).
  *
  * @param h     IO context from which to call the read_pause function pointer
  * @param pause 1 for pause, 0 for resume
*)
// int avio_pause(AVIOContext *h, int pause);
function avio_pause(h: pAVIOContext; pause: int): int; cdecl; external avformat_dll;
(* *
  * Seek to a given timestamp relative to some component stream.
  * Only meaningful if using a network streaming protocol (e.g. MMS.).
  *
  * @param h IO context from which to call the seek function pointers
  * @param stream_index The stream index that the timestamp is relative to.
  *        If stream_index is (-1) the timestamp should be in AV_TIME_BASE
  *        units from the beginning of the presentation.
  *        If a stream_index >= 0 is used and the protocol does not support
  *        seeking based on component streams, the call will fail.
  * @param timestamp timestamp in AVStream.time_base units
  *        or if there is no stream specified then in AV_TIME_BASE units.
  * @param flags Optional combination of AVSEEK_FLAG_BACKWARD, AVSEEK_FLAG_BYTE
  *        and AVSEEK_FLAG_ANY. The protocol may silently ignore
  *        AVSEEK_FLAG_BACKWARD and AVSEEK_FLAG_ANY, but AVSEEK_FLAG_BYTE will
  *        fail if used and not supported.
  * @return >= 0 on success
  * @see AVInputFormat::read_seek
*)
// int64_t avio_seek_time(AVIOContext *h, int stream_index, int64_t timestamp, int flags);
function avio_seek_time(h: pAVIOContext; stream_index: int; timestamp: int64_t; flags: int): int64_t; cdecl; external avformat_dll;

(* *
  * Read contents of h into print buffer, up to max_size bytes, or up to EOF.
  *
  * @return 0 for success (max_size bytes read or EOF reached), negative error
  * code otherwise
*)
// int avio_read_to_bprint(AVIOContext *h, struct AVBPrint *pb, size_t max_size);
function avio_read_to_bprint(h: pAVIOContext; pb: pAVBPrint; max_size: size_t): int; cdecl; external avformat_dll;
(* *
  * Accept and allocate a client context on a server context.
  * @param  s the server context
  * @param  c the client context, must be unallocated
  * @return   >= 0 on success or a negative value corresponding
  *           to an AVERROR on failure
*)
// int avio_accept(AVIOContext *s, AVIOContext **c);
function avio_accept(s: pAVIOContext; var c: pAVIOContext): int; cdecl; external avformat_dll;
(* *
  * Perform one step of the protocol handshake to accept a new client.
  * This function must be called on a client returned by avio_accept() before
  * using it as a read/write context.
  * It is separate from avio_accept() because it may block.
  * A step of the handshake is defined by places where the application may
  * decide to change the proceedings.
  * For example, on a protocol with a request header and a reply header, each
  * one can constitute a step because the application may use the parameters
  * from the request to change parameters in the reply; or each individual
  * chunk of the request can constitute a step.
  * If the handshake is already finished, avio_handshake() does nothing and
  * returns 0 immediately.
  *
  * @param  c the client context to perform the handshake on
  * @return   0   on a complete and successful handshake
  *           > 0 if the handshake progressed, but is not complete
  *           < 0 for an AVERROR code
*)
// int avio_handshake(AVIOContext *c);
function avio_handshake(c: pAVIOContext): int; cdecl; external avformat_dll;
{$ENDREGION}

const
  // AVFormatContext -> int flags;
  AVFMT_FLAG_GENPTS   = $0001; // < Generate missing pts even if it requires parsing future frames.
  AVFMT_FLAG_IGNIDX   = $0002; // < Ignore index.
  AVFMT_FLAG_NONBLOCK = $0004; // < Do not block when reading packets from input.
  AVFMT_FLAG_IGNDTS   = $0008; // < Ignore DTS on frames that contain both DTS & PTS
  AVFMT_FLAG_NOFILLIN = $0010; // < Do not infer any values from other values, just return what is stored in the container
  AVFMT_FLAG_NOPARSE  = $0020;
  // < Do not use AVParsers, you also must set AVFMT_FLAG_NOFILLIN as the fillin code works on frames and no parsing -> no frames. Also seeking to frames can not work if parsing to find frame boundaries has been disabled
  AVFMT_FLAG_NOBUFFER        = $0040; // < Do not buffer frames when possible
  AVFMT_FLAG_CUSTOM_IO       = $0080; // < The caller has supplied a custom AVIOContext, don't avio_close() it.
  AVFMT_FLAG_DISCARD_CORRUPT = $0100; // < Discard frames marked corrupted
  AVFMT_FLAG_FLUSH_PACKETS   = $0200; // < Flush the AVIOContext every packet.
  (* *
    * When muxing, try to avoid writing any random/volatile data to the output.
    * This includes any random IDs, real-time timestamps/dates, muxer version, etc.
    *
    * This flag is mainly intended for testing.
  *)
  AVFMT_FLAG_BITEXACT = $0400;
{$IFDEF FF_API_LAVF_MP4A_LATM}
  // < Enable RTP MP4A-LATM payload
  AVFMT_FLAG_MP4A_LATM = $8000;
  /// < Deprecated, does nothing.
{$ENDIF}
  AVFMT_FLAG_SORT_DTS = $10000; // < try to interleave outputted packets by dts (using this flag can slow demuxing down)
  AVFMT_FLAG_PRIV_OPT = $20000;
  // < Enable use of private options by delaying codec open (this could be made default once all code is converted)
{$IFDEF FF_API_LAVF_KEEPSIDE_FLAG}
  AVFMT_FLAG_KEEP_SIDE_DATA = $40000; // < Deprecated, does nothing.
{$ENDIF}
  AVFMT_FLAG_FAST_SEEK = $80000;  // < Enable fast, but inaccurate seeks for some formats
  AVFMT_FLAG_SHORTEST  = $100000; // < Stop muxing when the shortest stream stops.
  AVFMT_FLAG_AUTO_BSF  = $200000; // < Add bitstream filters as requested by the muxer

  // AVFormatContext ->int debug;
  FF_FDEBUG_TS = $0001;
  // AVFormatContext ->int event_flags;
  AVFMT_EVENT_FLAG_METADATA_UPDATED = $0001; // < The call resulted in updated metadata.
  // AVFormatContext ->int avoid_negative_ts;
  AVFMT_AVOID_NEG_TS_AUTO              = -1; // < Enabled when required by target format
  AVFMT_AVOID_NEG_TS_MAKE_NON_NEGATIVE = 1;  // < Shift timestamps so they are non negative
  AVFMT_AVOID_NEG_TS_MAKE_ZERO         = 2;  // < Shift timestamps so that they start at 0

  // AVProgram
  AV_PROGRAM_RUNNING = 1;
  //
  AVFMTCTX_NOHEADER = $0001; (* *< signal that no header is present
    (streams are added dynamically) *)
  AVFMTCTX_UNSEEKABLE = $0002; (* *< signal that the stream is definitely
    not seekable, and attempts to call the
    seek function will fail. For some
    network protocols (e.g. HLS), this can
    change dynamically at runtime. *)

const
  AVPROBE_SCORE_EXTENSION = 50;  // < score for file extension
  AVPROBE_SCORE_MIME      = 75;  // < score for file mime type
  AVPROBE_SCORE_MAX       = 100; // < maximum score

  AVPROBE_PADDING_SIZE = 32; // < extra allocated bytes at the end of the probe buffer

  AVPROBE_SCORE_RETRY        = (AVPROBE_SCORE_MAX div 4);
  AVPROBE_SCORE_STREAM_RETRY = (AVPROBE_SCORE_MAX div 4 - 1);

  // Demuxer will use avio_open, no opened file should be provided by the caller.
  AVFMT_NOFILE        = $0001;
  AVFMT_NEEDNUMBER    = $0002; (* *< Needs '%d' in filename. *)
  AVFMT_SHOW_IDS      = $0008; (* *< Show format stream IDs numbers. *)
  AVFMT_GLOBALHEADER  = $0040; (* *< Format wants global header. *)
  AVFMT_NOTIMESTAMPS  = $0080; (* *< Format does not need / have any timestamps. *)
  AVFMT_GENERIC_INDEX = $0100; (* *< Use generic index building code. *)
  AVFMT_TS_DISCONT    = $0200; (* *< Format allows timestamp discontinuities. Note, muxers always require valid (monotone) timestamps *)
  AVFMT_VARIABLE_FPS  = $0400; (* *< Format allows variable fps. *)
  AVFMT_NODIMENSIONS  = $0800; (* *< Format does not need width/height *)
  AVFMT_NOSTREAMS     = $1000; (* *< Format does not require any streams *)
  AVFMT_NOBINSEARCH   = $2000; (* *< Format does not allow to fall back on binary search via read_timestamp *)
  AVFMT_NOGENSEARCH   = $4000; (* *< Format does not allow to fall back on generic search *)
  AVFMT_NO_BYTE_SEEK  = $8000; (* *< Format does not allow seeking by bytes *)
  AVFMT_ALLOW_FLUSH   = $10000;
  (* *< Format allows flushing. If not set, the muxer will not receive a NULL packet in the write_packet function. *)
  AVFMT_TS_NONSTRICT = $20000; (* *< Format does not require strictly
    increasing timestamps, but they must
    still be monotonic *)
  AVFMT_TS_NEGATIVE = $40000; (* *< Format allows muxing negative
    timestamps. If not set the timestamp
    will be shifted in av_write_frame and
    av_interleaved_write_frame so they
    start from 0.
    The user or muxer can override this through
    AVFormatContext.avoid_negative_ts
  *)

  AVFMT_SEEK_TO_PTS = $4000000; (* *< Seeking is based on PTS *)

  AVINDEX_KEYFRAME      = $0001;
  AVINDEX_DISCARD_FRAME = $0002;

  AV_DISPOSITION_DEFAULT  = $0001;
  AV_DISPOSITION_DUB      = $0002;
  AV_DISPOSITION_ORIGINAL = $0004;
  AV_DISPOSITION_COMMENT  = $0008;
  AV_DISPOSITION_LYRICS   = $0010;
  AV_DISPOSITION_KARAOKE  = $0020;

  (* *
    * Track should be used during playback by default.
    * Useful for subtitle track that should be displayed
    * even when user did not explicitly ask for subtitles.
  *)
  AV_DISPOSITION_FORCED           = $0040;
  AV_DISPOSITION_HEARING_IMPAIRED = $0080; (* *< stream for hearing impaired audiences *)
  AV_DISPOSITION_VISUAL_IMPAIRED  = $0100; (* *< stream for visual impaired audiences *)
  AV_DISPOSITION_CLEAN_EFFECTS    = $0200; (* *< stream without voice *)
  (* *
    * The stream is stored in the file as an attached picture/"cover art" (e.g.
    * APIC frame in ID3v2). The first (usually only) packet associated with it
    * will be returned among the first few packets read from the file unless
    * seeking takes place. It can also be accessed at any time in
    * AVStream.attached_pic.
  *)
  AV_DISPOSITION_ATTACHED_PIC = $0400;
  (* *
    * The stream is sparse, and contains thumbnail images, often corresponding
    * to chapter markers. Only ever used with AV_DISPOSITION_ATTACHED_PIC.
  *)
  AV_DISPOSITION_TIMED_THUMBNAILS = $0800;

  (* *
    * To specify text track kind (different from subtitles default).
  *)
  AV_DISPOSITION_CAPTIONS     = $10000;
  AV_DISPOSITION_DESCRIPTIONS = $20000;
  AV_DISPOSITION_METADATA     = $40000;
  AV_DISPOSITION_DEPENDENT    = $80000; // < dependent audio stream (mix_type=0 in mpegts)
  AV_DISPOSITION_STILL_IMAGE  = $100000;
  /// < still images in video stream (still_picture_flag=1 in mpegts)

  (* *
    * Options for behavior on timestamp wrap detection.
  *)
  AV_PTS_WRAP_IGNORE     = 0;  // < ignore the wrap
  AV_PTS_WRAP_ADD_OFFSET = 1;  // < add the format specific offset on wrap detection
  AV_PTS_WRAP_SUB_OFFSET = -1; // < subtract the format specific offset on wrap detection

  AVSTREAM_EVENT_FLAG_METADATA_UPDATED = $0001; // < The call resulted in updated metadata.
  (* ****************************************************************
    * All fields below this line are not part of the public API. They
    * may not be used outside of libavformat and can be changed and
    * removed at will.
    * Internal note: be aware that physically removing these fields
    * will break ABI. Replace removed fields with dummy fields, and
    * add new fields to AVStreamInternal.
    *****************************************************************
  *)

  MAX_STD_TIMEBASES = (30 * 12 + 30 + 3 + 6);

  MAX_REORDER_DELAY = 16;

type

  pAVFormatContext = ^AVFormatContext;
  ppAVFormatContext = ^pAVFormatContext;
  pAVFormatInternal = ^AVFormatInternal;
  pAVInputFormat = ^AVInputFormat;
  pAVCodecTag = ^AVCodecTag;
  ppAVCodecTag = ^pAVCodecTag;
  pAVProbeData = ^AVProbeData;
  pAVDeviceInfoList = ^AVDeviceInfoList;
  pAVDeviceCapabilitiesQuery = ^AVDeviceCapabilitiesQuery;
  pAVOutputFormat = ^AVOutputFormat;
  pAVChapter = ^AVChapter;
  ppAVChapter = ^pAVChapter;
  pAVStream = ^AVStream;
  ppAVStream = ^pAVStream;
  pAVProgram = ^AVProgram;
  ppAVProgram = ^pAVProgram;

  AVChapter = record
    id: int;               // < unique ID to identify the chapter
    time_base: AVRational; // < time base in which the start/end timestamps are specified
    start, _end: int64_t;  // < chapter start/end time in time_base units
    metadata: pAVDictionary;
  end;

  (* *
    * Callback used by devices to communicate with application.
  *)
  // typedef int (*av_format_control_message)(struct AVFormatContext *s, int type,
  // void *data, size_t data_size);
  Tav_format_control_message = function(s: pAVFormatContext; _type: int; data: pointer; data_size: size_t): int; cdecl;

  // typedef int (*AVOpenCallback)(struct AVFormatContext *s, AVIOContext **pb, const char *url, int flags,
  // const AVIOInterruptCB *int_cb, AVDictionary **options);
  TAVOpenCallback = function(s: pAVFormatContext; var pb: pAVIOContext; const url: PAnsiChar; flags: int; const int_cb: pAVIOInterruptCB;
    var options: pAVDictionary): int; cdecl;

  (* *
    * The duration of a video can be estimated through various ways, and this enum can be used
    * to know how the duration was estimated.
  *)
  AVDurationEstimationMethod = ( //
    AVFMT_DURATION_FROM_PTS,     // < Duration accurately estimated from PTSes
    AVFMT_DURATION_FROM_STREAM,  // < Duration estimated from a stream with a known duration
    AVFMT_DURATION_FROM_BITRATE  // < Duration estimated from bitrate (less accurate)
    );

  AVFormatInternal = record
  end;

  AVInputFormat = record
    (* *
      * A comma separated list of short names for the format. New names
      * may be appended with a minor bump.
    *)
    name: PAnsiChar;

    (* *
      * Descriptive name for the format, meant to be more human-readable
      * than name. You should use the NULL_IF_CONFIG_SMALL() macro
      * to define it.
    *)
    long_name: PAnsiChar;

    (* *
      * Can use flags: AVFMT_NOFILE, AVFMT_NEEDNUMBER, AVFMT_SHOW_IDS,
      * AVFMT_NOTIMESTAMPS, AVFMT_GENERIC_INDEX, AVFMT_TS_DISCONT, AVFMT_NOBINSEARCH,
      * AVFMT_NOGENSEARCH, AVFMT_NO_BYTE_SEEK, AVFMT_SEEK_TO_PTS.
    *)
    flags: int;

    (* *
      * If extensions are defined, then no probe is done. You should
      * usually not use extension format guessing because it is not
      * reliable enough
    *)
    extensions: PAnsiChar;

    codec_tag: ppAVCodecTag;

    priv_class: pAVClass; // < AVClass for the private context

    (* *
      * Comma-separated list of mime types.
      * It is used check for matching mime types while probing.
      * @see av_probe_input_format2
    *)
    mime_type: PAnsiChar;

    (* ****************************************************************
      * No fields below this line are part of the public API. They
      * may not be used outside of libavformat and can be changed and
      * removed at will.
      * New public fields should be added right above.
      *****************************************************************
    *)
    next: pAVInputFormat;

    (* *
      * Raw demuxers store their codec ID here.
    *)
    raw_codec_id: int;

    (* *
      * Size of private data so that it can be allocated in the wrapper.
    *)
    priv_data_size: int;

    (* *
      * Tell if a given file has a chance of being parsed as this format.
      * The buffer provided is guaranteed to be AVPROBE_PADDING_SIZE bytes
      * big so you do not have to check for that unless you need more.
    *)
    // int (*read_probe)(AVProbeData *);
    read_probe: function(const p: pAVProbeData): int; cdecl;

    (* *
      * Read the format header and initialize the AVFormatContext
      * structure. Return 0 if OK. 'avformat_new_stream' should be
      * called to create new streams.
    *)
    // int (*read_header)(struct AVFormatContext *);
    read_header: function(p: pAVFormatContext): int; cdecl;

    (* *
      * Read one packet and put it in 'pkt'. pts and flags are also
      * set. 'avformat_new_stream' can be called only if the flag
      * AVFMTCTX_NOHEADER is used and only in the calling thread (not in a
      * background thread).
      * @return 0 on success, < 0 on error.
      *         When returning an error, pkt must not have been allocated
      *         or must be freed before returning
    *)
    // int (*read_packet)(struct AVFormatContext *, AVPacket *pkt);
    read_packet: function(p: pAVFormatContext; pkt: pAVPacket): int; cdecl;

    (* *
      * Close the stream. The AVFormatContext and AVStreams are not
      * freed by this function
    *)
    // int (*read_close)(struct AVFormatContext *);
    read_close: function(p: pAVFormatContext): int; cdecl;
    (* *
      * Seek to a given timestamp relative to the frames in
      * stream component stream_index.
      * @param stream_index Must not be -1.
      * @param flags Selects which direction should be preferred if no exact
      *              match is available.
      * @return >= 0 on success (but not necessarily the new offset)
    *)
    // int (*read_seek)(struct AVFormatContext *, int stream_index, int64_t timestamp, int flags);
    read_seek: function(p: pAVFormatContext; stream_index: int; timestamp: int64_t; flags: int): int; cdecl;

    (* *
      * Get the next timestamp in stream[stream_index].time_base units.
      * @return the timestamp or AV_NOPTS_VALUE if an error occurred
    *)
    // int64_t (*read_timestamp)(struct AVFormatContext *s, int stream_index, int64_t *pos, int64_t pos_limit);
    read_timestamp: function(s: pAVFormatContext; stream_index: int; var pos: int64_t; pos_limit: int64_t): int64_t; cdecl;

    (* *
      * Start/resume playing - only meaningful if using a network-based format
      * (RTSP).
    *)
    // int (*read_play)(struct AVFormatContext *);
    read_play: function(p: pAVFormatContext): int; cdecl;
    (* *
      * Pause playing - only meaningful if using a network-based format
      * (RTSP).
    *)
    // int (*read_pause)(struct AVFormatContext *);
    read_pause: function(p: pAVFormatContext): int; cdecl;
    (* *
      * Seek to timestamp ts.
      * Seeking will be done so that the point from which all active streams
      * can be presented successfully will be closest to ts and within min/max_ts.
      * Active streams are all streams that have AVStream.discard < AVDISCARD_ALL.
    *)
    // int (*read_seek2)(struct AVFormatContext *s, int stream_index, int64_t min_ts, int64_t ts, int64_t max_ts, int flags);
    read_seek2: function(s: pAVFormatContext; stream_index: int; min_ts: int64_t; ts: int64_t; max_ts: int64_t; flags: int): int; cdecl;

    (* *
      * Returns device list with it properties.
      * @see avdevice_list_devices() for more details.
    *)
    // int (*get_device_list)(struct AVFormatContext *s, struct AVDeviceInfoList *device_list);
    get_device_list: function(s: pAVFormatContext; device_list: pAVDeviceInfoList): int; cdecl;
    (* *
      * Initialize device capabilities submodule.
      * @see avdevice_capabilities_create() for more details.
    *)
    // int (*create_device_capabilities)(struct AVFormatContext *s, struct AVDeviceCapabilitiesQuery *caps);
    create_device_capabilities: function(s: pAVFormatContext; caps: pAVDeviceCapabilitiesQuery): int; cdecl;
    (* *
      * Free device capabilities submodule.
      * @see avdevice_capabilities_free() for more details.
    *)
    // int (*free_device_capabilities)(struct AVFormatContext *s, struct AVDeviceCapabilitiesQuery *caps);
    free_device_capabilities: function(s: pAVFormatContext; caps: pAVDeviceCapabilitiesQuery): int; cdecl;
  end;

  (* *
    * Format I/O context.
    * New fields can be added to the end with minor version bumps.
    * Removal, reordering and changes to existing fields require a major
    * version bump.
    * sizeof(AVFormatContext) must not be used outside libav*, use
    * avformat_alloc_context() to create an AVFormatContext.
    *
    * Fields can be accessed through AVOptions (av_opt* ),
    * the name string used matches the associated command line parameter name and
    * can be found in libavformat/options_table.h.
    * The AVOption/command line parameter names differ in some cases from the C
    * structure field names for historic reasons or brevity.
  *)

  AVFormatContext = record
    (* *
      * A class for logging and @ref avoptions. Set by avformat_alloc_context().
      * Exports (de)muxer private options if they exist.
    *)
    av_class: pAVClass;

    (* *
      * The input container format.
      *
      * Demuxing only, set by avformat_open_input().
    *)
    iformat: pAVInputFormat;

    (* *
      * The output container format.
      *
      * Muxing only, must be set by the caller before avformat_write_header().
    *)
    oformat: pAVOutputFormat;

    (* *
      * Format private data. This is an AVOptions-enabled struct
      * if and only if iformat/oformat.priv_class is not NULL.
      *
      * - muxing: set by avformat_write_header()
      * - demuxing: set by avformat_open_input()
    *)
    priv_data: pointer;

    (* *
      * I/O context.
      *
      * - demuxing: either set by the user before avformat_open_input() (then
      *             the user must close it manually) or set by avformat_open_input().
      * - muxing: set by the user before avformat_write_header(). The caller must
      *           take care of closing / freeing the IO context.
      *
      * Do NOT set this field if AVFMT_NOFILE flag is set in
      * iformat/oformat.flags. In such a case, the (de)muxer will handle
      * I/O in some other way and this field will be NULL.
    *)
    pb: pAVIOContext;

    (* stream info *)
    (* *
      * Flags signalling stream properties. A combination of AVFMTCTX_*.
      * Set by libavformat.
    *)
    ctx_flags: int;

    (* *
      * Number of elements in AVFormatContext.streams.
      *
      * Set by avformat_new_stream(), must not be modified by any other code.
    *)
    nb_streams: unsigned_int;
    (* *
      * A list of all streams in the file. New streams are created with
      * avformat_new_stream().
      *
      * - demuxing: streams are created by libavformat in avformat_open_input().
      *             If AVFMTCTX_NOHEADER is set in ctx_flags, then new streams may also
      *             appear in av_read_frame().
      * - muxing: streams are created by the user before avformat_write_header().
      *
      * Freed by libavformat in avformat_free_context().
    *)
    streams: ppAVStream;

{$IFDEF FF_API_FORMAT_FILENAME}
    (* *
      * input or output filename
      *
      * - demuxing: set by avformat_open_input()
      * - muxing: may be set by the caller before avformat_write_header()
      *
      * @deprecated Use url instead.
    *)
    // attribute_deprecated
    filename: array [0 .. 1024 - 1] of AnsiChar;
{$ENDIF}
    (* *
      * input or output URL. Unlike the old filename field, this field has no
      * length restriction.
      *
      * - demuxing: set by avformat_open_input(), initialized to an empty
      *             string if url parameter was NULL in avformat_open_input().
      * - muxing: may be set by the caller before calling avformat_write_header()
      *           (or avformat_init_output() if that is called first) to a string
      *           which is freeable by av_free(). Set to an empty string if it
      *           was NULL in avformat_init_output().
      *
      * Freed by libavformat in avformat_free_context().
    *)
    url: PAnsiChar;

    (* *
      * Position of the first frame of the component, in
      * AV_TIME_BASE fractional seconds. NEVER set this value directly:
      * It is deduced from the AVStream values.
      *
      * Demuxing only, set by libavformat.
    *)
    start_time: int64_t;

    (* *
      * Duration of the stream, in AV_TIME_BASE fractional
      * seconds. Only set this value if you know none of the individual stream
      * durations and also do not set any of them. This is deduced from the
      * AVStream values if not set.
      *
      * Demuxing only, set by libavformat.
    *)
    duration: int64_t;

    (* *
      * Total stream bitrate in bit/s, 0 if not
      * available. Never set it directly if the file_size and the
      * duration are known as FFmpeg can compute it automatically.
    *)
    bit_rate: int64_t;

    packet_size: unsigned_int;
    max_delay: int;

    (* *
      * Flags modifying the (de)muxer behaviour. A combination of AVFMT_FLAG_*.
      * Set by the user before avformat_open_input() / avformat_write_header().
    *)
    flags: int;

    (* *
      * Maximum size of the data read from input for determining
      * the input container format.
      * Demuxing only, set by the caller before avformat_open_input().
    *)
    robesize: int64_t;

    (* *
      * Maximum duration (in AV_TIME_BASE units) of the data read
      * from input in avformat_find_stream_info().
      * Demuxing only, set by the caller before avformat_find_stream_info().
      * Can be set to 0 to let avformat choose using a heuristic.
    *)
    ax_analyze_duration: int64_t;

    key: puint8_t;
    keylen: int;

    nb_programs: unsigned_int;
    programs: ppAVProgram;

    (* *
      * Forced video codec_id.
      * Demuxing: Set by user.
    *)
    video_codec_id: AVCodecID;

    (* *
      * Forced audio codec_id.
      * Demuxing: Set by user.
    *)
    audio_codec_id: AVCodecID;

    (* *
      * Forced subtitle codec_id.
      * Demuxing: Set by user.
    *)
    subtitle_codec_id: AVCodecID;

    (* *
      * Maximum amount of memory in bytes to use for the index of each stream.
      * If the index exceeds this size, entries will be discarded as
      * needed to maintain a smaller size. This can lead to slower or less
      * accurate seeking (depends on demuxer).
      * Demuxers for which a full in-memory index is mandatory will ignore
      * this.
      * - muxing: unused
      * - demuxing: set by user
    *)
    max_index_size: unsigned_int;

    (* *
      * Maximum amount of memory in bytes to use for buffering frames
      * obtained from realtime capture devices.
    *)
    max_picture_buffer: unsigned_int;

    (* *
      * Number of chapters in AVChapter array.
      * When muxing, chapters are normally written in the file header,
      * so nb_chapters should normally be initialized before write_header
      * is called. Some muxers (e.g. mov and mkv) can also write chapters
      * in the trailer.  To write chapters in the trailer, nb_chapters
      * must be zero when write_header is called and non-zero when
      * write_trailer is called.
      * - muxing: set by user
      * - demuxing: set by libavformat
    *)
    nb_chapters: unsigned_int;
    chapters: ppAVChapter;

    (* *
      * Metadata that applies to the whole file.
      *
      * - demuxing: set by libavformat in avformat_open_input()
      * - muxing: may be set by the caller before avformat_write_header()
      *
      * Freed by libavformat in avformat_free_context().
    *)
    metadata: pAVDictionary;

    (* *
      * Start time of the stream in real world time, in microseconds
      * since the Unix epoch (00:00 1st January 1970). That is, pts=0 in the
      * stream was captured at this real world time.
      * - muxing: Set by the caller before avformat_write_header(). If set to
      *           either 0 or AV_NOPTS_VALUE, then the current wall-time will
      *           be used.
      * - demuxing: Set by libavformat. AV_NOPTS_VALUE if unknown. Note that
      *             the value may become known after some number of frames
      *             have been received.
    *)
    start_time_realtime: int64_t;

    (* *
      * The number of frames used for determining the framerate in
      * avformat_find_stream_info().
      * Demuxing only, set by the caller before avformat_find_stream_info().
    *)
    fps_probe_size: int;

    (* *
      * Error recognition; higher values will detect more errors but may
      * misdetect some more or less valid parts as errors.
      * Demuxing only, set by the caller before avformat_open_input().
    *)
    error_recognition: int;

    (* *
      * Custom interrupt callbacks for the I/O layer.
      *
      * demuxing: set by the user before avformat_open_input().
      * muxing: set by the user before avformat_write_header()
      * (mainly useful for AVFMT_NOFILE formats). The callback
      * should also be passed to avio_open2() if it's used to
      * open the file.
    *)
    interrupt_callback: AVIOInterruptCB;

    (* *
      * Flags to enable debugging.
    *)
    debug: int;

    (* *
      * Maximum buffering duration for interleaving.
      *
      * To ensure all the streams are interleaved correctly,
      * av_interleaved_write_frame() will wait until it has at least one packet
      * for each stream before actually writing any packets to the output file.
      * When some streams are "sparse" (i.e. there are large gaps between
      * successive packets), this can result in excessive buffering.
      *
      * This field specifies the maximum difference between the timestamps of the
      * first and the last packet in the muxing queue, above which libavformat
      * will output a packet regardless of whether it has queued a packet for all
      * the streams.
      *
      * Muxing only, set by the caller before avformat_write_header().
    *)
    max_interleave_delta: int64_t;

    (* *
      * Allow non-standard and experimental extension
      * @see AVCodecContext.strict_std_compliance
    *)
    strict_std_compliance: int;

    (* *
      * Flags for the user to detect events happening on the file. Flags must
      * be cleared by the user once the event has been handled.
      * A combination of AVFMT_EVENT_FLAG_*.
    *)
    event_flags: int;

    (* *
      * Maximum number of packets to read while waiting for the first timestamp.
      * Decoding only.
    *)
    max_ts_probe: int;

    (* *
      * Avoid negative timestamps during muxing.
      * Any value of the AVFMT_AVOID_NEG_TS_* constants.
      * Note, this only works when using av_interleaved_write_frame. (interleave_packet_per_dts is in use)
      * - muxing: Set by user
      * - demuxing: unused
    *)
    avoid_negative_ts: int;

    (* *
      * Transport stream id.
      * This will be moved into demuxer private options. Thus no API/ABI compatibility
    *)
    ts_id: int;

    (* *
      * Audio preload in microseconds.
      * Note, not all formats support this and unpredictable things may happen if it is used when not supported.
      * - encoding: Set by user
      * - decoding: unused
    *)
    audio_preload: int;

    (* *
      * Max chunk time in microseconds.
      * Note, not all formats support this and unpredictable things may happen if it is used when not supported.
      * - encoding: Set by user
      * - decoding: unused
    *)
    max_chunk_duration: int;

    (* *
      * Max chunk size in bytes
      * Note, not all formats support this and unpredictable things may happen if it is used when not supported.
      * - encoding: Set by user
      * - decoding: unused
    *)
    max_chunk_size: int;

    (* *
      * forces the use of wallclock timestamps as pts/dts of packets
      * This has undefined results in the presence of B frames.
      * - encoding: unused
      * - decoding: Set by user
    *)
    use_wallclock_as_timestamps: int;

    (* *
      * avio flags, used to force AVIO_FLAG_DIRECT.
      * - encoding: unused
      * - decoding: Set by user
    *)
    avio_flags: int;

    (* *
      * The duration field can be estimated through various ways, and this field can be used
      * to know how the duration was estimated.
      * - encoding: unused
      * - decoding: Read by user
    *)
    duration_estimation_method: AVDurationEstimationMethod;

    (* *
      * Skip initial bytes when opening stream
      * - encoding: unused
      * - decoding: Set by user
    *)
    skip_initial_bytes: int64_t;

    (* *
      * Correct single timestamp overflows
      * - encoding: unused
      * - decoding: Set by user
    *)
    correct_ts_overflow: unsigned_int;

    (* *
      * Force seeking to any (also non key) frames.
      * - encoding: unused
      * - decoding: Set by user
    *)
    seek2any: int;

    (* *
      * Flush the I/O context after each packet.
      * - encoding: Set by user
      * - decoding: unused
    *)
    flush_packets: int;

    (* *
      * format probing score.
      * The maximal score is AVPROBE_SCORE_MAX, its set when the demuxer probes
      * the format.
      * - encoding: unused
      * - decoding: set by avformat, read by user
    *)
    probe_score: int;

    (* *
      * number of bytes to read maximally to identify format.
      * - encoding: unused
      * - decoding: set by user
    *)
    format_probesize: int;

    (* *
      * ',' separated list of allowed decoders.
      * If NULL then all are allowed
      * - encoding: unused
      * - decoding: set by user
    *)
    codec_whitelist: PAnsiChar;

    (* *
      * ',' separated list of allowed demuxers.
      * If NULL then all are allowed
      * - encoding: unused
      * - decoding: set by user
    *)
    format_whitelist: PAnsiChar;

    (* *
      * An opaque field for libavformat internal usage.
      * Must not be accessed in any way by callers.
    *)
    internal: pAVFormatInternal;

    (* *
      * IO repositioned flag.
      * This is set by avformat when the underlaying IO context read pointer
      * is repositioned, for example when doing byte based seeking.
      * Demuxers can use the flag to detect such changes.
    *)
    io_repositioned: int;

    (* *
      * Forced video codec.
      * This allows forcing a specific decoder, even when there are multiple with
      * the same codec_id.
      * Demuxing: Set by user
    *)
    video_codec: pAVCodec;

    (* *
      * Forced audio codec.
      * This allows forcing a specific decoder, even when there are multiple with
      * the same codec_id.
      * Demuxing: Set by user
    *)
    audio_codec: pAVCodec;

    (* *
      * Forced subtitle codec.
      * This allows forcing a specific decoder, even when there are multiple with
      * the same codec_id.
      * Demuxing: Set by user
    *)
    subtitle_codec: pAVCodec;

    (* *
      * Forced data codec.
      * This allows forcing a specific decoder, even when there are multiple with
      * the same codec_id.
      * Demuxing: Set by user
    *)
    data_codec: pAVCodec;

    (* *
      * Number of bytes to be written as padding in a metadata header.
      * Demuxing: Unused.
      * Muxing: Set by user via av_format_set_metadata_header_padding.
    *)
    metadata_header_padding: int;

    (* *
      * User data.
      * This is a place for some private data of the user.
    *)
    opaque: pointer;

    (* *
      * Callback used by devices to communicate with application.
    *)
    control_message_cb: Tav_format_control_message;

    (* *
      * Output timestamp offset, in microseconds.
      * Muxing: set by user
    *)
    output_ts_offset: int64_t;

    (* *
      * dump format separator.
      * can be ", " or "\n      " or anything else
      * - muxing: Set by user.
      * - demuxing: Set by user.
    *)
    dump_separator: puint8_t;

    (* *
      * Forced Data codec_id.
      * Demuxing: Set by user.
    *)
    data_codec_id: AVCodecID;

{$IFDEF FF_API_OLD_OPEN_CALLBACKS}
    (* *
      * Called to open further IO contexts when needed for demuxing.
      *
      * This can be set by the user application to perform security checks on
      * the URLs before opening them.
      * The function should behave like avio_open2(), AVFormatContext is provided
      * as contextual information and to reach AVFormatContext.opaque.
      *
      * If NULL then some simple checks are used together with avio_open2().
      *
      * Must not be accessed directly from outside avformat.
      * @See av_format_set_open_cb()
      *
      * Demuxing: Set by user.
      *
      * @deprecated Use io_open and io_close.
    *)
    // attribute_deprecated
    // int (*open_cb)(struct AVFormatContext *s, AVIOContext **p, const char *url, int flags, const AVIOInterruptCB *int_cb, AVDictionary **options);
    open_cb: function(s: pAVFormatContext; var p: pAVIOContext; const url: PAnsiChar; flags: int; const int_cb: pAVIOInterruptCB; var options: pAVDictionary)
      : int; cdecl;
{$ENDIF}
    (* *
      * ',' separated list of allowed protocols.
      * - encoding: unused
      * - decoding: set by user
    *)
    protocol_whitelist: PAnsiChar;

    (* *
      * A callback for opening new IO streams.
      *
      * Whenever a muxer or a demuxer needs to open an IO stream (typically from
      * avformat_open_input() for demuxers, but for certain formats can happen at
      * other times as well), it will call this callback to obtain an IO context.
      *
      * @param s the format context
      * @param pb on success, the newly opened IO context should be returned here
      * @param url the url to open
      * @param flags a combination of AVIO_FLAG_*
      * @param options a dictionary of additional options, with the same
      *                semantics as in avio_open2()
      * @return 0 on success, a negative AVERROR code on failure
      *
      * @note Certain muxers and demuxers do nesting, i.e. they open one or more
      * additional internal format contexts. Thus the AVFormatContext pointer
      * passed to this callback may be different from the one facing the caller.
      * It will, however, have the same 'opaque' field.
    *)
    // int (*io_open)(struct AVFormatContext *s, AVIOContext **pb, const char *url, int flags, AVDictionary **options);
    io_open: function(s: pAVFormatContext; var pb: pAVIOContext; const url: PAnsiChar; flags: int; var options: pAVDictionary): int; cdecl;

    (* *
      * A callback for closing the streams opened with AVFormatContext.io_open().
    *)
    // void (*io_close)(struct AVFormatContext *s, AVIOContext *pb);
    io_close: procedure(s: pAVFormatContext; pb: pAVIOContext); cdecl;
    (* *
      * ',' separated list of disallowed protocols.
      * - encoding: unused
      * - decoding: set by user
    *)
    protocol_blacklist: PAnsiChar;

    (* *
      * The maximum number of streams.
      * - encoding: unused
      * - decoding: set by user
    *)
    max_streams: int;
    (*
      * Skip duration calcuation in estimate_timings_from_pts.
      * - encoding: unused
      * - decoding: set by user
    *)
    skip_estimate_duration_from_pts: int;
  end;

  (* input/output formats *)

  AVCodecTag = record
  end;

  (* *
    * This structure contains the data a format has to probe a file.
  *)

  AVProbeData = record
    filename: PAnsiChar;
    buf: punsigned_char;  (* *< Buffer must have AVPROBE_PADDING_SIZE of extra allocated bytes filled with zero. *)
    buf_size: int;        (* *< Size of buf except extra allocated bytes *)
    mime_type: PAnsiChar; (* *< mime_type, when known. *)
  end;

  AVDeviceInfoList = record

  end;

  AVDeviceCapabilitiesQuery = record

  end;

  AVOutputFormat = record
    name: PAnsiChar;
    (* *
      * Descriptive name for the format, meant to be more human-readable
      * than name. You should use the NULL_IF_CONFIG_SMALL() macro
      * to define it.
    *)
    long_name: PAnsiChar;
    mime_type: PAnsiChar;
    extensions: PAnsiChar; (* *< comma-separated filename extensions *)
    (* output support *)
    audio_codec: AVCodecID;    (* *< default audio codec *)
    video_codec: AVCodecID;    (* *< default video codec *)
    subtitle_codec: AVCodecID; (* *< default subtitle codec *)
    (* *
      * can use flags: AVFMT_NOFILE, AVFMT_NEEDNUMBER,
      * AVFMT_GLOBALHEADER, AVFMT_NOTIMESTAMPS, AVFMT_VARIABLE_FPS,
      * AVFMT_NODIMENSIONS, AVFMT_NOSTREAMS, AVFMT_ALLOW_FLUSH,
      * AVFMT_TS_NONSTRICT, AVFMT_TS_NEGATIVE
    *)
    flags: int;

    (* *
      * List of supported codec_id-codec_tag pairs, ordered by "better
      * choice first". The arrays are all terminated by AV_CODEC_ID_NONE.
    *)
    codec_tag: ppAVCodecTag;

    priv_class: pAVClass; // < AVClass for the private context

    (* ****************************************************************
      * No fields below this line are part of the public API. They
      * may not be used outside of libavformat and can be changed and
      * removed at will.
      * New public fields should be added right above.
      *****************************************************************
    *)
    next: pAVOutputFormat;
    (* *
      * size of private data so that it can be allocated in the wrapper
    *)
    priv_data_size: int;

    // int (*write_header)(struct AVFormatContext *);
    write_header: function(p: pAVFormatContext): int; cdecl;
    (* *
      * Write a packet. If AVFMT_ALLOW_FLUSH is set in flags,
      * pkt can be NULL in order to flush data buffered in the muxer.
      * When flushing, return 0 if there still is more data to flush,
      * or 1 if everything was flushed and there is no more buffered
      * data.
    *)
    // int (*write_packet)(struct AVFormatContext *, AVPacket *pkt);
    write_packet: function(fc: pAVFormatContext; pkt: pAVPacket): int; cdecl;

    // int (*write_trailer)(struct AVFormatContext *);
    write_trailer: function(p: pAVFormatContext): int; cdecl;
    (* *
      * Currently only used to set pixel format if not YUV420P.
    *)
    // int (*interleave_packet)(struct AVFormatContext *, AVPacket *out, AVPacket *in, int flush);
    interleave_packet: function(p: pAVFormatContext; _out: pAVPacket; _in: pAVPacket; flush: int): int; cdecl;
    (* *
      * Test if the given codec can be stored in this container.
      *
      * @return 1 if the codec is supported, 0 if it is not.
      *         A negative number if unknown.
      *         MKTAG('A', 'P', 'I', 'C') if the codec is only supported as AV_DISPOSITION_ATTACHED_PIC
    *)
    // int (*query_codec)(enum AVCodecID id, int std_compliance);
    query_codec: function(id: AVCodecID; std_compliance: int): int; cdecl;

    // void (*get_output_timestamp)(struct AVFormatContext *s, int stream, int64_t *dts, int64_t *wall);
    get_output_timestamp: procedure(s: pAVFormatContext; stream: int; var dts: int64_t; var wall: int64_t); cdecl;
    (* *
      * Allows sending messages from application to device.
    *)
    // int (*control_message)(struct AVFormatContext *s, int type, void *data, size_t data_size);
    control_message: function(s: pAVFormatContext; _type: int; data: pointer; data_size: size_t): int; cdecl;
    (* *
      * Write an uncoded AVFrame.
      *
      * See av_write_uncoded_frame() for details.
      *
      * The library will free *frame afterwards, but the muxer can prevent it
      * by setting the pointer to NULL.
    *)
    // int (*write_uncoded_frame)(struct AVFormatContext *, int stream_index, AVFrame **frame, unsigned flags);
    write_uncoded_frame: function(p: pAVFormatContext; stream_index: int; var frame: pAVFrame; flags: unsigned): int; cdecl;
    (* *
      * Returns device list with it properties.
      * @see avdevice_list_devices() for more details.
    *)
    // int (*get_device_list)(struct AVFormatContext *s, struct AVDeviceInfoList *device_list);
    get_device_list: function(s: pAVFormatContext; device_list: pAVDeviceInfoList): int; cdecl;
    (* *
      * Initialize device capabilities submodule.
      * @see avdevice_capabilities_create() for more details.
    *)
    // int (*create_device_capabilities)(struct AVFormatContext *s, struct AVDeviceCapabilitiesQuery *caps);
    create_device_capabilities: function(s: pAVFormatContext; caps: pAVDeviceCapabilitiesQuery): int; cdecl;
    (* *
      * Free device capabilities submodule.
      * @see avdevice_capabilities_free() for more details.
    *)
    // int (*free_device_capabilities)(struct AVFormatContext *s, struct AVDeviceCapabilitiesQuery *caps);
    free_device_capabilities: function(s: pAVFormatContext; caps: pAVDeviceCapabilitiesQuery): int; cdecl;

    data_codec: AVCodecID; (* *< default data codec *)
    (* *
      * Initialize format. May allocate data here, and set any AVFormatContext or
      * AVStream parameters that need to be set before packets are sent.
      * This method must not write output.
      *
      * Return 0 if streams were fully configured, 1 if not, negative AVERROR on failure
      *
      * Any allocations made here must be freed in deinit().
    *)
    // int (*init)(struct AVFormatContext *);
    init: function(p: pAVFormatContext): int; cdecl;
    (* *
      * Deinitialize format. If present, this is called whenever the muxer is being
      * destroyed, regardless of whether or not the header has been written.
      *
      * If a trailer is being written, this is called after write_trailer().
      *
      * This is called if init() fails as well.
    *)
    // void (*deinit)(struct AVFormatContext *);
    deinit: procedure(p: pAVFormatContext); cdecl;
    (* *
      * Set up any necessary bitstream filtering and extract any extra data needed
      * for the global header.
      * Return 0 if more packets from this stream must be checked; 1 if not.
    *)
    // int (*check_bitstream)(struct AVFormatContext *, const AVPacket *pkt);
    check_bitstream: function(p: pAVFormatContext; const pkt: pAVPacket): int; cdecl;
  end;

  AVStreamParseType = (                       //
    AVSTREAM_PARSE_NONE, AVSTREAM_PARSE_FULL, (* *< full parsing and repack *)
    AVSTREAM_PARSE_HEADERS,                   (* *< Only parse headers, do not repack. *)
    AVSTREAM_PARSE_TIMESTAMPS,                (* *< full parsing and interpolation of timestamps for frames not starting on a packet boundary *)
    AVSTREAM_PARSE_FULL_ONCE,                 (* *< full parsing and repack of the first frame only, only implemented for H.264 currently *)
    AVSTREAM_PARSE_FULL_RAW (* *< full parsing and repack with timestamp and position generation by parser for raw
      this assumes that each packet in the file contains no demuxer level headers and
      just codec level data, otherwise position generation would fail *)
    );

  pAVIndexEntry = ^AVIndexEntry;

  AVIndexEntry = record
    pos: int64_t;
    timestamp: int64_t; (* *<
      * Timestamp in AVStream.time_base units, preferably the time from which on correctly decoded frames are available
      * when seeking to this entry. That means preferable PTS on keyframe based formats.
      * But demuxers can choose to store a different timestamp, if it is more convenient for the implementation or nothing better
      * is known
    *)
    (* *
      * Flag is used to indicate which frame should be discarded after decoding.
    *)
    // int flags:2;
    // int size:30; //Yeah, trying to keep the size of this small to reduce memory requirements (it is 24 vs. 32 bytes due to possible 8-byte alignment).
    flag_size: int32;
    min_distance: int; (* *< Minimum distance between this and the previous keyframe, used to avoid unneeded searching. *)
  end;

  pAVStreamInternal = ^AVStreamInternal;

  AVStreamInternal = record

  end;

  Tduration_error = array [0 .. 1, 0 .. MAX_STD_TIMEBASES] of Double;
  pduration_error = ^Tduration_error;

  pAVStream_info = ^AVStream_info;

  AVStream_info = record
    last_dts: int64_t;
    duration_gcd: int64_t;
    duration_count: int;
    rfps_duration_sum: int64_t;
    duration_error: pduration_error;
    codec_info_duration: int64_t;
    codec_info_duration_fields: int64_t;
    frame_delay_evidence: int;

    (* *
      * 0  -> decoder has not been searched for yet.
      * >0 -> decoder found
      * <0 -> decoder with codec_id == -found_decoder has not been found
    *)
    found_decoder: int;

    last_duration: int64_t;

    (* *
      * Those are used for average framerate estimation.
    *)
    fps_first_dts: int64_t;
    fps_first_dts_idx: int;
    fps_last_dts: int64_t;
    fps_last_dts_idx: int;
  end;

  Tpts_buffer_int64_t = array [0 .. MAX_REORDER_DELAY] of int64_t;
  Tpts_reorder_error_count_uint8_t = array [0 .. MAX_REORDER_DELAY] of uint8_t;

  pAVPacketList = ^AVPacketList;

  AVPacketList = record
    pkt: AVPacket;
    next: pAVPacketList;
  end;

  (* *
    * Stream structure.
    * New fields can be added to the end with minor version bumps.
    * Removal, reordering and changes to existing fields require a major
    * version bump.
    * sizeof(AVStream) must not be used outside libav*.
  *)

  AVStream = record
    index: int; (* *< stream index in AVFormatContext *)
    (* *
      * Format-specific stream ID.
      * decoding: set by libavformat
      * encoding: set by the user, replaced by libavformat if left unset
    *)
    id: int;
{$IFDEF FF_API_LAVF_AVCTX}
    (* *
      * @deprecated use the codecpar struct instead
    *)
    // attribute_deprecated
    codec: pAVCodecContext;
{$ENDIF}
    priv_data: pointer;

    (* *
      * This is the fundamental unit of time (in seconds) in terms
      * of which frame timestamps are represented.
      *
      * decoding: set by libavformat
      * encoding: May be set by the caller before avformat_write_header() to
      *           provide a hint to the muxer about the desired timebase. In
      *           avformat_write_header(), the muxer will overwrite this field
      *           with the timebase that will actually be used for the timestamps
      *           written into the file (which may or may not be related to the
      *           user-provided one, depending on the format).
    *)
    time_base: AVRational;

    (* *
      * Decoding: pts of the first frame of the stream in presentation order, in stream time base.
      * Only set this if you are absolutely 100% sure that the value you set
      * it to really is the pts of the first frame.
      * This may be undefined (AV_NOPTS_VALUE).
      * @note The ASF header does NOT contain a correct start_time the ASF
      * demuxer must NOT set this.
    *)
    start_time: int64_t;

    (* *
      * Decoding: duration of the stream, in stream time base.
      * If a source file does not specify a duration, but does specify
      * a bitrate, this value will be estimated from bitrate and file size.
      *
      * Encoding: May be set by the caller before avformat_write_header() to
      * provide a hint to the muxer about the estimated duration.
    *)
    duration: int64_t;

    nb_frames: int64_t; // < number of frames in this stream if known or 0

    disposition: int; (* *< AV_DISPOSITION_* bit field *)

    discard: AVDiscard; // < Selects which packets can be discarded at will and do not need to be demuxed.

    (* *
      * sample aspect ratio (0 if unknown)
      * - encoding: Set by user.
      * - decoding: Set by libavformat.
    *)
    sample_aspect_ratio: AVRational;

    metadata: pAVDictionary;

    (* *
      * Average framerate
      *
      * - demuxing: May be set by libavformat when creating the stream or in
      *             avformat_find_stream_info().
      * - muxing: May be set by the caller before avformat_write_header().
    *)
    avg_frame_rate: AVRational;

    (* *
      * For streams with AV_DISPOSITION_ATTACHED_PIC disposition, this packet
      * will contain the attached picture.
      *
      * decoding: set by libavformat, must not be modified by the caller.
      * encoding: unused
    *)
    attached_pic: AVPacket;

    (* *
      * An array of side data that applies to the whole stream (i.e. the
      * container does not allow it to change between packets).
      *
      * There may be no overlap between the side data in this array and side data
      * in the packets. I.e. a given side data is either exported by the muxer
      * (demuxing) / set by the caller (muxing) in this array, then it never
      * appears in the packets, or the side data is exported / sent through
      * the packets (always in the first packet where the value becomes known or
      * changes), then it does not appear in this array.
      *
      * - demuxing: Set by libavformat when the stream is created.
      * - muxing: May be set by the caller before avformat_write_header().
      *
      * Freed by libavformat in avformat_free_context().
      *
      * @see av_format_inject_global_side_data()
    *)
    side_data: pAVPacketSideData;
    (* *
      * The number of elements in the AVStream.side_data array.
    *)
    nb_side_data: int;

    (* *
      * Flags for the user to detect events happening on the stream. Flags must
      * be cleared by the user once the event has been handled.
      * A combination of AVSTREAM_EVENT_FLAG_*.
    *)
    event_flags: int;
    (* *
      * Real base framerate of the stream.
      * This is the lowest framerate with which all timestamps can be
      * represented accurately (it is the least common multiple of all
      * framerates in the stream). Note, this value is just a guess!
      * For example, if the time base is 1/90000 and all frames have either
      * approximately 3600 or 1800 timer ticks, then r_frame_rate will be 50/1.
    *)
    r_frame_rate: AVRational;

{$IFDEF FF_API_LAVF_FFSERVER}
    (* *
      * String containing pairs of key and values describing recommended encoder configuration.
      * Pairs are separated by ','.
      * Keys are separated from values by '='.
      *
      * @deprecated unused
    *)
    // attribute_deprecated
    recommended_encoder_configuration: PAnsiChar;
{$ENDIF}
    (* *
      * Codec parameters associated with this stream. Allocated and freed by
      * libavformat in avformat_new_stream() and avformat_free_context()
      * respectively.
      *
      * - demuxing: filled by libavformat on stream creation or in
      *             avformat_find_stream_info()
      * - muxing: filled by the caller before avformat_write_header()
    *)
    codecpar: pAVCodecParameters;
    (* *
      * Stream information used internally by avformat_find_stream_info()
    *)
    info: pAVStream_info;

    pts_wrap_bits: int; (* *< number of bits in pts (used for wrapping control) *)

    // Timestamp generation support:
    (* *
      * Timestamp corresponding to the last dts sync point.
      *
      * Initialized when AVCodecParserContext.dts_sync_point >= 0 and
      * a DTS is received from the underlying container. Otherwise set to
      * AV_NOPTS_VALUE by default.
    *)
    first_dts: int64_t;
    cur_dts: int64_t;
    last_IP_pts: int64_t;
    last_IP_duration: int;

    (* *
      * Number of packets to buffer for codec probing
    *)
    probe_packets: int;

    (* *
      * Number of frames that have been demuxed during avformat_find_stream_info()
    *)
    codec_info_nb_frames: int;

    (* av_read_frame() support *)
    need_parsing: AVStreamParseType;
    parser: pAVCodecParserContext;

    (* *
      * last packet in packet_buffer for this stream when muxing.
    *)
    last_in_packet_buffer: pAVPacketList;
    probe_data: AVProbeData;

    pts_buffer: Tpts_buffer_int64_t;

    index_entries: pAVIndexEntry; (* *< Only used if the format does not
      support seeking natively. *)
    nb_index_entries: int;
    index_entries_allocated_size: unsigned_int;

    (* *
      * Stream Identifier
      * This is the MPEG-TS stream identifier +1
      * 0 means unknown
    *)
    stream_identifier: int;

    (*
      * Details of the MPEG-TS program which created this stream.
    *)
    program_num: int;
    pmt_version: int;
    pmt_stream_idx: int;

    interleaver_chunk_size: int64_t;
    interleaver_chunk_duration: int64_t;

    (* *
      * stream probing state
      * -1   -> probing finished
      *  0   -> no probing requested
      * rest -> perform probing with request_probe being the minimum score to accept.
      * NOT PART OF PUBLIC API
    *)
    request_probe: int;
    (* *
      * Indicates that everything up to the next keyframe
      * should be discarded.
    *)
    skip_to_keyframe: int;

    (* *
      * Number of samples to skip at the start of the frame decoded from the next packet.
    *)
    skip_samples: int;

    (* *
      * If not 0, the number of samples that should be skipped from the start of
      * the stream (the samples are removed from packets with pts==0, which also
      * assumes negative timestamps do not happen).
      * Intended for use with formats such as mp3 with ad-hoc gapless audio
      * support.
    *)
    start_skip_samples: int64_t;

    (* *
      * If not 0, the first audio sample that should be discarded from the stream.
      * This is broken by design (needs global sample count), but can't be
      * avoided for broken by design formats such as mp3 with ad-hoc gapless
      * audio support.
    *)
    first_discard_sample: int64_t;

    (* *
      * The sample after last sample that is intended to be discarded after
      * first_discard_sample. Works on frame boundaries only. Used to prevent
      * early EOF if the gapless info is broken (considered concatenated mp3s).
    *)
    last_discard_sample: int64_t;

    (* *
      * Number of internally decoded frames, used internally in libavformat, do not access
      * its lifetime differs from info which is why it is not in that structure.
    *)
    nb_decoded_frames: int;

    (* *
      * Timestamp offset added to timestamps before muxing
      * NOT PART OF PUBLIC API
    *)
    mux_ts_offset: int64_t;

    (* *
      * Internal data to check for wrapping of the time stamp
    *)
    pts_wrap_reference: int64_t;

    (* *
      * Options for behavior, when a wrap is detected.
      *
      * Defined by AV_PTS_WRAP_ values.
      *
      * If correction is enabled, there are two possibilities:
      * If the first time stamp is near the wrap point, the wrap offset
      * will be subtracted, which will create negative time stamps.
      * Otherwise the offset will be added.
    *)
    pts_wrap_behavior: int;

    (* *
      * Internal data to prevent doing update_initial_durations() twice
    *)
    update_initial_durations_done: int;

    (* *
      * Internal data to generate dts from pts
    *)
    pts_reorder_error: Tpts_buffer_int64_t;
    pts_reorder_error_count: Tpts_reorder_error_count_uint8_t;

    (* *
      * Internal data to analyze DTS and detect faulty mpeg streams
    *)
    last_dts_for_order_check: int64_t;
    dts_ordered: uint8_t;
    dts_misordered: uint8_t;

    (* *
      * Internal data to inject global side data
    *)
    inject_global_side_data: int;

    (* *
      * display aspect ratio (0 if unknown)
      * - encoding: unused
      * - decoding: Set by libavformat to calculate sample_aspect_ratio internally
    *)
    display_aspect_ratio: AVRational;

    (* *
      * An opaque field for libavformat internal usage.
      * Must not be accessed in any way by callers.
    *)
    internal: pAVStreamInternal;
  end;

  (* *
    * New fields can be added to the end with minor version bumps.
    * Removal, reordering and changes to existing fields require a major
    * version bump.
    * sizeof(AVProgram) must not be used outside libav*.
  *)

  AVProgram = record
    id: int;
    flags: int;
    discard: AVDiscard; // < selects which program to discard and which to feed to the caller
    stream_index: punsigned_int;
    nb_stream_indexes: unsigned_int;
    metadata: pAVDictionary;

    program_num: int;
    pmt_pid: int;
    pcr_pid: int;
    pmt_version: int;

    (* ****************************************************************
      * All fields below this line are not part of the public API. They
      * may not be used outside of libavformat and can be changed and
      * removed at will.
      * New public fields should be added right above.
      *****************************************************************
    *)
    start_time: int64_t;
    end_time: int64_t;

    pts_wrap_reference: int64_t; // < reference dts for wrap detection
    pts_wrap_behavior: int;      // < behavior on wrap detection
  end;

  (* *
    * @defgroup metadata_api Public Metadata API
    * @{
    * @ingroup libavf
    * The metadata API allows libavformat to export metadata tags to a client
    * application when demuxing. Conversely it allows a client application to
    * set metadata when muxing.
    *
    * Metadata is exported or set as pairs of key/value strings in the 'metadata'
    * fields of the AVFormatContext, AVStream, AVChapter and AVProgram structs
    * using the @ref lavu_dict "AVDictionary" API. Like all strings in FFmpeg,
    * metadata is assumed to be UTF-8 encoded Unicode. Note that metadata
    * exported by demuxers isn't checked to be valid UTF-8 in most cases.
    *
    * Important concepts to keep in mind:
    * -  Keys are unique; there can never be 2 tags with the same key. This is
    *    also meant semantically, i.e., a demuxer should not knowingly produce
    *    several keys that are literally different but semantically identical.
    *    E.g., key=Author5, key=Author6. In this example, all authors must be
    *    placed in the same tag.
    * -  Metadata is flat, not hierarchical; there are no subtags. If you
    *    want to store, e.g., the email address of the child of producer Alice
    *    and actor Bob, that could have key=alice_and_bobs_childs_email_address.
    * -  Several modifiers can be applied to the tag name. This is done by
    *    appending a dash character ('-') and the modifier name in the order
    *    they appear in the list below -- e.g. foo-eng-sort, not foo-sort-eng.
    *    -  language -- a tag whose value is localized for a particular language
    *       is appended with the ISO 639-2/B 3-letter language code.
    *       For example: Author-ger=Michael, Author-eng=Mike
    *       The original/default language is in the unqualified "Author" tag.
    *       A demuxer should set a default if it sets any translated tag.
    *    -  sorting  -- a modified version of a tag that should be used for
    *       sorting will have '-sort' appended. E.g. artist="The Beatles",
    *       artist-sort="Beatles, The".
    * - Some protocols and demuxers support metadata updates. After a successful
    *   call to av_read_packet(), AVFormatContext.event_flags or AVStream.event_flags
    *   will be updated to indicate if metadata changed. In order to detect metadata
    *   changes on a stream, you need to loop through all streams in the AVFormatContext
    *   and check their individual event_flags.
    *
    * -  Demuxers attempt to export metadata in a generic format, however tags
    *    with no generic equivalents are left as they are stored in the container.
    *    Follows a list of generic tag names:
    *
    @verbatim
    album        -- name of the set this work belongs to
    album_artist -- main creator of the set/album, if different from artist.
    e.g. "Various Artists" for compilation albums.
    artist       -- main creator of the work
    comment      -- any additional description of the file.
    composer     -- who composed the work, if different from artist.
    copyright    -- name of copyright holder.
    creation_time-- date when the file was created, preferably in ISO 8601.
    date         -- date when the work was created, preferably in ISO 8601.
    disc         -- number of a subset, e.g. disc in a multi-disc collection.
    encoder      -- name/settings of the software/hardware that produced the file.
    encoded_by   -- person/group who created the file.
    filename     -- original name of the file.
    genre        -- <self-evident>.
    language     -- main language in which the work is performed, preferably
    in ISO 639-2 format. Multiple languages can be specified by
    separating them with commas.
    performer    -- artist who performed the work, if different from artist.
    E.g for "Also sprach Zarathustra", artist would be "Richard
    Strauss" and performer "London Philharmonic Orchestra".
    publisher    -- name of the label/publisher.
    service_name     -- name of the service in broadcasting (channel name).
    service_provider -- name of the service provider in broadcasting.
    title        -- name of the work.
    track        -- number of this work in the set, can be in form current/total.
    variant_bitrate -- the total bitrate of the bitrate variant that the current stream is part of
    @endverbatim
    *
    * Look in the examples section for an application example how to use the Metadata API.
    *
    * @}
  *)

  (* packet functions *)

  (* *
    * Allocate and read the payload of a packet and initialize its
    * fields with default values.
    *
    * @param s    associated IO context
    * @param pkt packet
    * @param size desired payload size
    * @return >0 (read size) if OK, AVERROR_xxx otherwise
  *)
  // int av_get_packet(AVIOContext *s, AVPacket *pkt, int size);
function av_get_packet(s: pAVIOContext; pkt: pAVPacket; size: int): int; cdecl; external avformat_dll;
(* *
  * Read data and append it to the current content of the AVPacket.
  * If pkt->size is 0 this is identical to av_get_packet.
  * Note that this uses av_grow_packet and thus involves a realloc
  * which is inefficient. Thus this function should only be used
  * when there is no reasonable way to know (an upper bound of)
  * the final size.
  *
  * @param s    associated IO context
  * @param pkt packet
  * @param size amount of data to read
  * @return >0 (read size) if OK, AVERROR_xxx otherwise, previous data
  *         will not be lost even if an error occurs.
*)
// int av_append_packet(AVIOContext *s, AVPacket *pkt, int size);
function av_append_packet(s: pAVIOContext; pkt: pAVPacket; size: int): int; cdecl; external avformat_dll;
(* *********************************************** *)

{$IFDEF FF_API_FORMAT_GET_SET}
(* *
  * Accessors for some AVStream fields. These used to be provided for ABI
  * compatibility, and do not need to be used anymore.
*)
// attribute_deprecated
// AVRational av_stream_get_r_frame_rate(const AVStream *s);
function av_stream_get_r_frame_rate(const s: pAVStream): AVRational; cdecl; external avformat_dll;

// attribute_deprecated
// void av_stream_set_r_frame_rate(AVStream *s, AVRational r);
procedure av_stream_set_r_frame_rate(s: pAVStream; r: AVRational); cdecl; external avformat_dll;
{$IFDEF FF_API_LAVF_FFSERVER}
// attribute_deprecated
// char* av_stream_get_recommended_encoder_configuration(const AVStream *s);
function av_stream_get_recommended_encoder_configuration(const s: pAVStream): PAnsiChar; cdecl; external avformat_dll;

// attribute_deprecated
// void  av_stream_set_recommended_encoder_configuration(AVStream *s, char *configuration);
procedure av_stream_set_recommended_encoder_configuration(s: pAVStream; configuration: PAnsiChar); cdecl; external avformat_dll;
{$ENDIF}
{$ENDIF}
// struct AVCodecParserContext *av_stream_get_parser(const AVStream *s);
function av_stream_get_parser(const s: pAVStream): pAVCodecParserContext; cdecl; external avformat_dll;
(* *
  * Returns the pts of the last muxed packet + its duration
  *
  * the retuned value is undefined when used with a demuxer.
*)
// int64_t av_stream_get_end_pts(const AVStream *st);
function av_stream_get_end_pts(const st: pAVStream): int64_t; cdecl; external avformat_dll;

{$IFDEF FF_API_FORMAT_GET_SET}
(* *
  * Accessors for some AVFormatContext fields. These used to be provided for ABI
  * compatibility, and do not need to be used anymore.
*)
// attribute_deprecated
// int av_format_get_probe_score(const AVFormatContext *s);
function av_format_get_probe_score(const s: pAVFormatContext): int; cdecl; external avformat_dll;

// attribute_deprecated
// AVCodec * av_format_get_video_codec(const AVFormatContext *s);
function av_format_get_video_codec(const s: pAVFormatContext): pAVCodec; cdecl; external avformat_dll;

// attribute_deprecated
// void av_format_set_video_codec(AVFormatContext *s, AVCodec *c);
procedure av_format_set_video_codec(s: pAVFormatContext; c: pAVCodec); cdecl; external avformat_dll;

// attribute_deprecated
// AVCodec * av_format_get_audio_codec(const AVFormatContext *s);
function av_format_get_audio_codec(const s: pAVFormatContext): pAVCodec; cdecl; external avformat_dll;

// attribute_deprecated
// void av_format_set_audio_codec(AVFormatContext *s, AVCodec *c);
procedure av_format_set_audio_codec(s: pAVFormatContext; c: pAVCodec); cdecl; external avformat_dll;

// attribute_deprecated
// AVCodec *av_format_get_subtitle_codec(const AVFormatContext *s);
function av_format_get_subtitle_codec(const s: pAVFormatContext): pAVCodec; cdecl; external avformat_dll;

// attribute_deprecated
// void av_format_set_subtitle_codec(AVFormatContext *s, AVCodec *c);
procedure av_format_set_subtitle_codec(s: pAVFormatContext; c: pAVCodec); cdecl; external avformat_dll;

// attribute_deprecated
// AVCodec *av_format_get_data_codec(const AVFormatContext *s);
function av_format_get_data_codec(const s: pAVFormatContext): pAVCodec; cdecl; external avformat_dll;

// attribute_deprecated
// void av_format_set_data_codec(AVFormatContext *s, AVCodec *c);
procedure av_format_set_data_codec(s: pAVFormatContext; c: pAVCodec); cdecl; external avformat_dll;

// attribute_deprecated
// int av_format_get_metadata_header_padding(const AVFormatContext *s);
function av_format_get_metadata_header_padding(const s: pAVFormatContext): int; cdecl; external avformat_dll;

// attribute_deprecated
// void av_format_set_metadata_header_padding(AVFormatContext *s, int c);
procedure av_format_set_metadata_header_padding(s: pAVFormatContext; c: int); cdecl; external avformat_dll;

// attribute_deprecated
// void *av_format_get_opaque(const AVFormatContext *s);
function av_format_get_opaque(const s: pAVFormatContext): pointer; cdecl; external avformat_dll;

// attribute_deprecated
// void av_format_set_opaque(AVFormatContext *s, void *opaque);
procedure av_format_set_opaque(s: pAVFormatContext; opaque: pointer); cdecl; external avformat_dll;

// attribute_deprecated
// av_format_control_message av_format_get_control_message_cb(const AVFormatContext *s);
function av_format_get_control_message_cb(const s: pAVFormatContext): Tav_format_control_message; cdecl; external avformat_dll;

// attribute_deprecated
// void av_format_set_control_message_cb(AVFormatContext *s, av_format_control_message callback);
procedure av_format_set_control_message_cb(s: pAVFormatContext; callback: Tav_format_control_message); cdecl; external avformat_dll;
{$IFDEF FF_API_OLD_OPEN_CALLBACKS}
// attribute_deprecated AVOpenCallback av_format_get_open_cb(const AVFormatContext *s);
function av_format_get_open_cb(const s: pAVFormatContext): TAVOpenCallback; cdecl; external avformat_dll;

// attribute_deprecated void av_format_set_open_cb(AVFormatContext *s, AVOpenCallback callback);
procedure av_format_set_open_cb(s: pAVFormatContext; callback: TAVOpenCallback); cdecl; external avformat_dll;
{$ENDIF}
{$ENDIF}
(* *
  * This function will cause global side data to be injected in the next packet
  * of each stream as well as after any subsequent seek.
*)
// void av_format_inject_global_side_data(AVFormatContext *s);
procedure av_format_inject_global_side_data(s: pAVFormatContext); cdecl; external avformat_dll;

(* *
  * Returns the method used to set ctx->duration.
  *
  * @return AVFMT_DURATION_FROM_PTS, AVFMT_DURATION_FROM_STREAM, or AVFMT_DURATION_FROM_BITRATE.
*)
// enum AVDurationEstimationMethod av_fmt_ctx_get_duration_estimation_method(const AVFormatContext* ctx);
function av_fmt_ctx_get_duration_estimation_method(const ctx: pAVFormatContext): AVDurationEstimationMethod; cdecl; external avformat_dll;

(* *
  * @defgroup lavf_core Core functions
  * @ingroup libavf
  *
  * Functions for querying libavformat capabilities, allocating core structures,
  * etc.
  * @{
*)

(* *
  * Return the LIBAVFORMAT_VERSION_INT constant.
*)
// unsigned avformat_version(void);
function avformat_version(): unsigned; cdecl; external avformat_dll;

(* *
  * Return the libavformat build-time configuration.
*)
// const char *avformat_configuration(void);
function avformat_configuration(): PAnsiChar; cdecl; external avformat_dll;

(* *
  * Return the libavformat license.
*)
// const char *avformat_license(void);
function avformat_license(): PAnsiChar; cdecl; external avformat_dll;

{$IFDEF FF_API_NEXT}
(* *
  * Initialize libavformat and register all the muxers, demuxers and
  * protocols. If you do not call this function, then you can select
  * exactly which formats you want to support.
  *
  * @see av_register_input_format()
  * @see av_register_output_format()
*)
// attribute_deprecated void av_register_all(void);
procedure av_register_all(); cdecl; external avformat_dll;

// attribute_deprecated void av_register_input_format(AVInputFormat *format);
procedure av_register_input_format(format: pAVInputFormat); cdecl; external avformat_dll;
// attribute_deprecated void av_register_output_format(AVOutputFormat *format);
procedure av_register_output_format(format: pAVOutputFormat); cdecl; external avformat_dll;
{$ENDIF}
(* *
  * Do global initialization of network libraries. This is optional,
  * and not recommended anymore.
  *
  * This functions only exists to work around thread-safety issues
  * with older GnuTLS or OpenSSL libraries. If libavformat is linked
  * to newer versions of those libraries, or if you do not use them,
  * calling this function is unnecessary. Otherwise, you need to call
  * this function before any other threads using them are started.
  *
  * This function will be deprecated once support for older GnuTLS and
  * OpenSSL libraries is removed, and this function has no purpose
  * anymore.
*)
// int avformat_network_init(void);
function avformat_network_init(): int; cdecl; external avformat_dll;

(* *
  * Undo the initialization done by avformat_network_init. Call it only
  * once for each time you called avformat_network_init.
*)
// int avformat_network_deinit(void);
function avformat_network_deinit(): int; cdecl; external avformat_dll;

{$IFDEF FF_API_NEXT}
(* *
  * If f is NULL, returns the first registered input format,
  * if f is non-NULL, returns the next registered input format after f
  * or NULL if f is the last one.
*)
// attribute_deprecated
// AVInputFormat  *av_iformat_next(const AVInputFormat  *f);
function av_iformat_next(const f: pAVInputFormat): pAVInputFormat; cdecl; external avformat_dll;

(* *
  * If f is NULL, returns the first registered output format,
  * if f is non-NULL, returns the next registered output format after f
  * or NULL if f is the last one.
*)
// attribute_deprecated
// AVOutputFormat *av_oformat_next(const AVOutputFormat *f);
function av_oformat_next(const f: pAVOutputFormat): pAVOutputFormat; cdecl; external avformat_dll;
{$ENDIF}
(* *
  * Iterate over all registered muxers.
  *
  * @param opaque a pointer where libavformat will store the iteration state. Must
  *               point to NULL to start the iteration.
  *
  * @return the next registered muxer or NULL when the iteration is
  *         finished
*)
// const AVOutputFormat *av_muxer_iterate(void **opaque);
function av_muxer_iterate(var opaque: pointer): pAVOutputFormat; cdecl; external avformat_dll;

(* *
  * Iterate over all registered demuxers.
  *
  * @param opaque a pointer where libavformat will store the iteration state. Must
  *               point to NULL to start the iteration.
  *
  * @return the next registered demuxer or NULL when the iteration is
  *         finished
*)
// const AVInputFormat *av_demuxer_iterate(void **opaque);
function av_demuxer_iterate(var opaque: pointer): pAVInputFormat; cdecl; external avformat_dll;

(* *
  * Allocate an AVFormatContext.
  * avformat_free_context() can be used to free the context and everything
  * allocated by the framework within it.
*)
// AVFormatContext *avformat_alloc_context(void);
function avformat_alloc_context(): pAVFormatContext; cdecl; external avformat_dll;

(* *
  * Free an AVFormatContext and all its streams.
  * @param s context to free
*)
// void avformat_free_context(AVFormatContext *s);
procedure avformat_free_context(s: pAVFormatContext); cdecl; external avformat_dll;

(* *
  * Get the AVClass for AVFormatContext. It can be used in combination with
  * AV_OPT_SEARCH_FAKE_OBJ for examining options.
  *
  * @see av_opt_find().
*)
// const AVClass *avformat_get_class(void);
function avformat_get_class(): pAVClass; cdecl; external avformat_dll;

(* *
  * Add a new stream to a media file.
  *
  * When demuxing, it is called by the demuxer in read_header(). If the
  * flag AVFMTCTX_NOHEADER is set in s.ctx_flags, then it may also
  * be called in read_packet().
  *
  * When muxing, should be called by the user before avformat_write_header().
  *
  * User is required to call avcodec_close() and avformat_free_context() to
  * clean up the allocation by avformat_new_stream().
  *
  * @param s media file handle
  * @param c If non-NULL, the AVCodecContext corresponding to the new stream
  * will be initialized to use this codec. This is needed for e.g. codec-specific
  * defaults to be set, so codec should be provided if it is known.
  *
  * @return newly created stream or NULL on error.
*)
// AVStream *avformat_new_stream(AVFormatContext *s, const AVCodec *c);
function avformat_new_stream(s: pAVFormatContext; const c: pAVCodec): pAVStream; cdecl; external avformat_dll;

(* *
  * Wrap an existing array as stream side data.
  *
  * @param st stream
  * @param type side information type
  * @param data the side data array. It must be allocated with the av_malloc()
  *             family of functions. The ownership of the data is transferred to
  *             st.
  * @param size side information size
  * @return zero on success, a negative AVERROR code on failure. On failure,
  *         the stream is unchanged and the data remains owned by the caller.
*)
// int av_stream_add_side_data(AVStream *st, enum AVPacketSideDataType type,
// uint8_t *data, size_t size);
function av_stream_add_side_data(st: pAVStream; _type: AVPacketSideDataType; data: puint8_t; size: size_t): int; cdecl; external avformat_dll;

(* *
  * Allocate new information from stream.
  *
  * @param stream stream
  * @param type desired side information type
  * @param size side information size
  * @return pointer to fresh allocated data or NULL otherwise
*)
// uint8_t *av_stream_new_side_data(AVStream *stream,
// enum AVPacketSideDataType type, int size);
function av_stream_new_side_data(stream: pAVStream; _type: AVPacketSideDataType; size: int): puint8_t; cdecl; external avformat_dll;
(* *
  * Get side information from stream.
  *
  * @param stream stream
  * @param type desired side information type
  * @param size pointer for side information size to store (optional)
  * @return pointer to data if present or NULL otherwise
*)
// uint8_t *av_stream_get_side_data(const AVStream *stream,
// enum AVPacketSideDataType type, int *size);
function av_stream_get_side_data(const stream: pAVStream; _type: AVPacketSideDataType; var size: int): puint8_t; cdecl; external avformat_dll;

// AVProgram *av_new_program(AVFormatContext *s, int id);
function av_new_program(s: pAVFormatContext; id: int): pAVProgram; cdecl; external avformat_dll;

(* *
  * Allocate an AVFormatContext for an output format.
  * avformat_free_context() can be used to free the context and
  * everything allocated by the framework within it.
  *
  * @param *ctx is set to the created format context, or to NULL in
  * case of failure
  * @param oformat format to use for allocating the context, if NULL
  * format_name and filename are used instead
  * @param format_name the name of output format to use for allocating the
  * context, if NULL filename is used instead
  * @param filename the name of the filename to use for allocating the
  * context, may be NULL
  * @return >= 0 in case of success, a negative AVERROR code in case of
  * failure
*)
// int avformat_alloc_output_context2(AVFormatContext **ctx, AVOutputFormat *oformat,
// const char *format_name, const char *filename);
function avformat_alloc_output_context2(var ctx: pAVFormatContext; oformat: pAVOutputFormat; const format_name: PAnsiChar; const filename: PAnsiChar): int;
  cdecl; external avformat_dll;

(* *
  * @addtogroup lavf_decoding
  * @{
*)

(* *
  * Find AVInputFormat based on the short name of the input format.
*)
// AVInputFormat *av_find_input_format(const char *short_name);
function av_find_input_format(const short_name: PAnsiChar): pAVInputFormat; cdecl; external avformat_dll;

(* *
  * Guess the file format.
  *
  * @param pd        data to be probed
  * @param is_opened Whether the file is already opened; determines whether
  *                  demuxers with or without AVFMT_NOFILE are probed.
*)
// AVInputFormat *av_probe_input_format(AVProbeData *pd, int is_opened);
function av_probe_input_format(pd: pAVProbeData; is_opened: int): pAVInputFormat; cdecl; external avformat_dll;

(* *
  * Guess the file format.
  *
  * @param pd        data to be probed
  * @param is_opened Whether the file is already opened; determines whether
  *                  demuxers with or without AVFMT_NOFILE are probed.
  * @param score_max A probe score larger that this is required to accept a
  *                  detection, the variable is set to the actual detection
  *                  score afterwards.
  *                  If the score is <= AVPROBE_SCORE_MAX / 4 it is recommended
  *                  to retry with a larger probe buffer.
*)
// AVInputFormat *av_probe_input_format2(AVProbeData *pd, int is_opened, int *score_max);
function av_probe_input_format2(pd: pAVProbeData; is_opened: int; var score_max: int): pAVInputFormat; cdecl; external avformat_dll;

(* *
  * Guess the file format.
  *
  * @param is_opened Whether the file is already opened; determines whether
  *                  demuxers with or without AVFMT_NOFILE are probed.
  * @param score_ret The score of the best detection.
*)
// AVInputFormat *av_probe_input_format3(AVProbeData *pd, int is_opened, int *score_ret);
function av_probe_input_format3(pd: pAVProbeData; is_opened: int; var score_ret: int): pAVInputFormat; cdecl; external avformat_dll;

(* *
  * Probe a bytestream to determine the input format. Each time a probe returns
  * with a score that is too low, the probe buffer size is increased and another
  * attempt is made. When the maximum probe size is reached, the input format
  * with the highest score is returned.
  *
  * @param pb the bytestream to probe
  * @param fmt the input format is put here
  * @param url the url of the stream
  * @param logctx the log context
  * @param offset the offset within the bytestream to probe from
  * @param max_probe_size the maximum probe buffer size (zero for default)
  * @return the score in case of success, a negative value corresponding to an
  *         the maximal score is AVPROBE_SCORE_MAX
  * AVERROR code otherwise
*)
// int av_probe_input_buffer2(AVIOContext *pb, AVInputFormat **fmt,
// const char *url, void *logctx,
// unsigned int offset, unsigned int max_probe_size);
function av_probe_input_buffer2(pb: pAVIOContext; var fmt: pAVInputFormat; const url: PAnsiChar; logctx: pointer; offset: unsigned_int;
  max_probe_size: unsigned_int): int; cdecl; external avformat_dll;

(* *
  * Like av_probe_input_buffer2() but returns 0 on success
*)
// int av_probe_input_buffer(AVIOContext *pb, AVInputFormat **fmt,
// const char *url, void *logctx,
// unsigned int offset, unsigned int max_probe_size);
function av_probe_input_buffer(pb: pAVIOContext; var fmt: pAVInputFormat; const url: PAnsiChar; logctx: pointer; offset: unsigned_int;
  max_probe_size: unsigned_int): int; cdecl; external avformat_dll;

(* *
  * Open an input stream and read the header. The codecs are not opened.
  * The stream must be closed with avformat_close_input().
  *
  * @param ps Pointer to user-supplied AVFormatContext (allocated by avformat_alloc_context).
  *           May be a pointer to NULL, in which case an AVFormatContext is allocated by this
  *           function and written into ps.
  *           Note that a user-supplied AVFormatContext will be freed on failure.
  * @param url URL of the stream to open.
  * @param fmt If non-NULL, this parameter forces a specific input format.
  *            Otherwise the format is autodetected.
  * @param options  A dictionary filled with AVFormatContext and demuxer-private options.
  *                 On return this parameter will be destroyed and replaced with a dict containing
  *                 options that were not found. May be NULL.
  *
  * @return 0 on success, a negative AVERROR on failure.
  *
  * @note If you want to use custom IO, preallocate the format context and set its pb field.
*)
// int avformat_open_input(AVFormatContext **ps, const char *url, AVInputFormat *fmt, AVDictionary **options);
function avformat_open_input(var ps: pAVFormatContext; const url: PAnsiChar; fmt: pAVInputFormat; options: ppAVDictionary): int; cdecl; overload; external avformat_dll;
function avformat_open_input(var ps: pAVFormatContext; const url: PAnsiChar; fmt: pAVInputFormat; var options: pAVDictionary): int; cdecl; overload; external avformat_dll;

// attribute_deprecated
// int av_demuxer_open(AVFormatContext *ic);
function av_demuxer_open(ic: pAVFormatContext): int; cdecl; external avformat_dll;

(* *
  * Read packets of a media file to get stream information. This
  * is useful for file formats with no headers such as MPEG. This
  * function also computes the real framerate in case of MPEG-2 repeat
  * frame mode.
  * The logical file position is not changed by this function;
  * examined packets may be buffered for later processing.
  *
  * @param ic media file handle
  * @param options  If non-NULL, an ic.nb_streams long array of pointers to
  *                 dictionaries, where i-th member contains options for
  *                 codec corresponding to i-th stream.
  *                 On return each dictionary will be filled with options that were not found.
  * @return >=0 if OK, AVERROR_xxx on error
  *
  * @note this function isn't guaranteed to open all the codecs, so
  *       options being non-empty at return is a perfectly normal behavior.
  *
  * @todo Let the user decide somehow what information is needed so that
  *       we do not waste time getting stuff the user does not need.
*)
// int avformat_find_stream_info(AVFormatContext *ic, AVDictionary **options);
function avformat_find_stream_info(ic: pAVFormatContext; options: ppAVDictionary): int; cdecl; overload; external avformat_dll;
function avformat_find_stream_info(ic: pAVFormatContext; Var options: pAVDictionary): int; cdecl; overload; external avformat_dll;

(* *
  * Find the programs which belong to a given stream.
  *
  * @param ic    media file handle
  * @param last  the last found program, the search will start after this
  *              program, or from the beginning if it is NULL
  * @param s     stream index
  * @return the next program which belongs to s, NULL if no program is found or
  *         the last program is not among the programs of ic.
*)
// AVProgram *av_find_program_from_stream(AVFormatContext *ic, AVProgram *last, int s);
function av_find_program_from_stream(ic: pAVFormatContext; last: pAVProgram; s: int): pAVProgram; cdecl; external avformat_dll;

// void av_program_add_stream_index(AVFormatContext *ac, int progid, unsigned int idx);
procedure av_program_add_stream_index(ac: pAVFormatContext; progid: int; idx: unsigned_int); cdecl; external avformat_dll;

(* *
  * Find the "best" stream in the file.
  * The best stream is determined according to various heuristics as the most
  * likely to be what the user expects.
  * If the decoder parameter is non-NULL, av_find_best_stream will find the
  * default decoder for the stream's codec; streams for which no decoder can
  * be found are ignored.
  *
  * @param ic                media file handle
  * @param type              stream type: video, audio, subtitles, etc.
  * @param wanted_stream_nb  user-requested stream number,
  *                          or -1 for automatic selection
  * @param related_stream    try to find a stream related (eg. in the same
  *                          program) to this one, or -1 if none
  * @param decoder_ret       if non-NULL, returns the decoder for the
  *                          selected stream
  * @param flags             flags; none are currently defined
  * @return  the non-negative stream number in case of success,
  *          AVERROR_STREAM_NOT_FOUND if no stream with the requested type
  *          could be found,
  *          AVERROR_DECODER_NOT_FOUND if streams were found but no decoder
  * @note  If av_find_best_stream returns successfully and decoder_ret is not
  *        NULL, then *decoder_ret is guaranteed to be set to a valid AVCodec.
*)
// int av_find_best_stream(AVFormatContext *ic,
// enum AVMediaType type,
// int wanted_stream_nb,
// int related_stream,
// AVCodec **decoder_ret,
// int flags);
function av_find_best_stream(ic: pAVFormatContext; _type: AVMediaType; wanted_stream_nb: int; related_stream: int; var decoder_ret: pAVCodec; flags: int): int;
  cdecl; external avformat_dll;

(* *
  * Return the next frame of a stream.
  * This function returns what is stored in the file, and does not validate
  * that what is there are valid frames for the decoder. It will split what is
  * stored in the file into frames and return one for each call. It will not
  * omit invalid data between valid frames so as to give the decoder the maximum
  * information possible for decoding.
  *
  * If pkt->buf is NULL, then the packet is valid until the next
  * av_read_frame() or until avformat_close_input(). Otherwise the packet
  * is valid indefinitely. In both cases the packet must be freed with
  * av_packet_unref when it is no longer needed. For video, the packet contains
  * exactly one frame. For audio, it contains an integer number of frames if each
  * frame has a known fixed size (e.g. PCM or ADPCM data). If the audio frames
  * have a variable size (e.g. MPEG audio), then it contains one frame.
  *
  * pkt->pts, pkt->dts and pkt->duration are always set to correct
  * values in AVStream.time_base units (and guessed if the format cannot
  * provide them). pkt->pts can be AV_NOPTS_VALUE if the video format
  * has B-frames, so it is better to rely on pkt->dts if you do not
  * decompress the payload.
  *
  * @return 0 if OK, < 0 on error or end of file
*)
// int av_read_frame(AVFormatContext *s, AVPacket *pkt);
function av_read_frame(s: pAVFormatContext; pkt: pAVPacket): int; cdecl; overload; external avformat_dll;
function av_read_frame(s: pAVFormatContext; var pkt: AVPacket): int; cdecl; overload; external avformat_dll;

(* *
  * Seek to the keyframe at timestamp.
  * 'timestamp' in 'stream_index'.
  *
  * @param s media file handle
  * @param stream_index If stream_index is (-1), a default
  * stream is selected, and timestamp is automatically converted
  * from AV_TIME_BASE units to the stream specific time_base.
  * @param timestamp Timestamp in AVStream.time_base units
  *        or, if no stream is specified, in AV_TIME_BASE units.
  * @param flags flags which select direction and seeking mode
  * @return >= 0 on success
*)
// int av_seek_frame(AVFormatContext *s, int stream_index, int64_t timestamp, int flags);
function av_seek_frame(s: pAVFormatContext; stream_index: int; timestamp: int64_t; flags: int): int; cdecl; external avformat_dll;

(* *
  * Seek to timestamp ts.
  * Seeking will be done so that the point from which all active streams
  * can be presented successfully will be closest to ts and within min/max_ts.
  * Active streams are all streams that have AVStream.discard < AVDISCARD_ALL.
  *
  * If flags contain AVSEEK_FLAG_BYTE, then all timestamps are in bytes and
  * are the file position (this may not be supported by all demuxers).
  * If flags contain AVSEEK_FLAG_FRAME, then all timestamps are in frames
  * in the stream with stream_index (this may not be supported by all demuxers).
  * Otherwise all timestamps are in units of the stream selected by stream_index
  * or if stream_index is -1, in AV_TIME_BASE units.
  * If flags contain AVSEEK_FLAG_ANY, then non-keyframes are treated as
  * keyframes (this may not be supported by all demuxers).
  * If flags contain AVSEEK_FLAG_BACKWARD, it is ignored.
  *
  * @param s media file handle
  * @param stream_index index of the stream which is used as time base reference
  * @param min_ts smallest acceptable timestamp
  * @param ts target timestamp
  * @param max_ts largest acceptable timestamp
  * @param flags flags
  * @return >=0 on success, error code otherwise
  *
  * @note This is part of the new seek API which is still under construction.
  *       Thus do not use this yet. It may change at any time, do not expect
  *       ABI compatibility yet!
*)
// int avformat_seek_file(AVFormatContext *s, int stream_index, int64_t min_ts, int64_t ts, int64_t max_ts, int flags);
function avformat_seek_file(s: pAVFormatContext; stream_index: int; min_ts: int64_t; ts: int64_t; max_ts: int64_t; flags: int): int; cdecl;
  external avformat_dll;

(* *
  * Discard all internally buffered data. This can be useful when dealing with
  * discontinuities in the byte stream. Generally works only with formats that
  * can resync. This includes headerless formats like MPEG-TS/TS but should also
  * work with NUT, Ogg and in a limited way AVI for example.
  *
  * The set of streams, the detected duration, stream parameters and codecs do
  * not change when calling this function. If you want a complete reset, it's
  * better to open a new AVFormatContext.
  *
  * This does not flush the AVIOContext (s->pb). If necessary, call
  * avio_flush(s->pb) before calling this function.
  *
  * @param s media file handle
  * @return >=0 on success, error code otherwise
*)
// int avformat_flush(AVFormatContext *s);
function avformat_flush(s: pAVFormatContext): int; cdecl; external avformat_dll;

(* *
  * Start playing a network-based stream (e.g. RTSP stream) at the
  * current position.
*)
// int av_read_play(AVFormatContext *s);
function av_read_play(s: pAVFormatContext): int; cdecl; external avformat_dll;

(* *
  * Pause a network-based stream (e.g. RTSP stream).
  *
  * Use av_read_play() to resume it.
*)
// int av_read_pause(AVFormatContext *s);
function av_read_pause(s: pAVFormatContext): int; cdecl; external avformat_dll;

(* *
  * Close an opened input AVFormatContext. Free it and all its contents
  * and set *s to NULL.
*)
// void avformat_close_input(AVFormatContext **s);
procedure avformat_close_input(var s: pAVFormatContext); cdecl; external avformat_dll;

(* *
  * @}
*)
const
  AVSEEK_FLAG_BACKWARD = 1; // < seek backward
  AVSEEK_FLAG_BYTE     = 2; // < seeking based on position in bytes
  AVSEEK_FLAG_ANY      = 4; // < seek to any frame, even non-keyframes
  AVSEEK_FLAG_FRAME    = 8; // < seeking based on frame number

  (* *
    * @addtogroup lavf_encoding
    * @{
  *)

  AVSTREAM_INIT_IN_WRITE_HEADER = 0; // < stream parameters initialized in avformat_write_header
  AVSTREAM_INIT_IN_INIT_OUTPUT  = 1; // < stream parameters initialized in avformat_init_output

  (* *
    * Allocate the stream private data and write the stream header to
    * an output media file.
    *
    * @param s Media file handle, must be allocated with avformat_alloc_context().
    *          Its oformat field must be set to the desired output format;
    *          Its pb field must be set to an already opened AVIOContext.
    * @param options  An AVDictionary filled with AVFormatContext and muxer-private options.
    *                 On return this parameter will be destroyed and replaced with a dict containing
    *                 options that were not found. May be NULL.
    *
    * @return AVSTREAM_INIT_IN_WRITE_HEADER on success if the codec had not already been fully initialized in avformat_init,
    *         AVSTREAM_INIT_IN_INIT_OUTPUT  on success if the codec had already been fully initialized in avformat_init,
    *         negative AVERROR on failure.
    *
    * @see av_opt_find, av_dict_set, avio_open, av_oformat_next, avformat_init_output.
  *)
  // av_warn_unused_result
  // int avformat_write_header(AVFormatContext *s, AVDictionary **options);
function avformat_write_header(s: pAVFormatContext; options: ppAVDictionary): int; cdecl; external avformat_dll;

(* *
  * Allocate the stream private data and initialize the codec, but do not write the header.
  * May optionally be used before avformat_write_header to initialize stream parameters
  * before actually writing the header.
  * If using this function, do not pass the same options to avformat_write_header.
  *
  * @param s Media file handle, must be allocated with avformat_alloc_context().
  *          Its oformat field must be set to the desired output format;
  *          Its pb field must be set to an already opened AVIOContext.
  * @param options  An AVDictionary filled with AVFormatContext and muxer-private options.
  *                 On return this parameter will be destroyed and replaced with a dict containing
  *                 options that were not found. May be NULL.
  *
  * @return AVSTREAM_INIT_IN_WRITE_HEADER on success if the codec requires avformat_write_header to fully initialize,
  *         AVSTREAM_INIT_IN_INIT_OUTPUT  on success if the codec has been fully initialized,
  *         negative AVERROR on failure.
  *
  * @see av_opt_find, av_dict_set, avio_open, av_oformat_next, avformat_write_header.
*)
// av_warn_unused_result
// int avformat_init_output(AVFormatContext *s, AVDictionary **options);
function avformat_init_output(s: pAVFormatContext; var options: pAVDictionary): int; cdecl; external avformat_dll;

(* *
  * Write a packet to an output media file.
  *
  * This function passes the packet directly to the muxer, without any buffering
  * or reordering. The caller is responsible for correctly interleaving the
  * packets if the format requires it. Callers that want libavformat to handle
  * the interleaving should call av_interleaved_write_frame() instead of this
  * function.
  *
  * @param s media file handle
  * @param pkt The packet containing the data to be written. Note that unlike
  *            av_interleaved_write_frame(), this function does not take
  *            ownership of the packet passed to it (though some muxers may make
  *            an internal reference to the input packet).
  *            <br>
  *            This parameter can be NULL (at any time, not just at the end), in
  *            order to immediately flush data buffered within the muxer, for
  *            muxers that buffer up data internally before writing it to the
  *            output.
  *            <br>
  *            Packet's @ref AVPacket.stream_index "stream_index" field must be
  *            set to the index of the corresponding stream in @ref
  *            AVFormatContext.streams "s->streams".
  *            <br>
  *            The timestamps (@ref AVPacket.pts "pts", @ref AVPacket.dts "dts")
  *            must be set to correct values in the stream's timebase (unless the
  *            output format is flagged with the AVFMT_NOTIMESTAMPS flag, then
  *            they can be set to AV_NOPTS_VALUE).
  *            The dts for subsequent packets passed to this function must be strictly
  *            increasing when compared in their respective timebases (unless the
  *            output format is flagged with the AVFMT_TS_NONSTRICT, then they
  *            merely have to be nondecreasing).  @ref AVPacket.duration
  *            "duration") should also be set if known.
  * @return < 0 on error, = 0 if OK, 1 if flushed and there is no more data to flush
  *
  * @see av_interleaved_write_frame()
*)
// int av_write_frame(AVFormatContext *s, AVPacket *pkt);
function av_write_frame(s: pAVFormatContext; pkt: pAVPacket): int; cdecl; external avformat_dll;
(* *
  * Write a packet to an output media file ensuring correct interleaving.
  *
  * This function will buffer the packets internally as needed to make sure the
  * packets in the output file are properly interleaved in the order of
  * increasing dts. Callers doing their own interleaving should call
  * av_write_frame() instead of this function.
  *
  * Using this function instead of av_write_frame() can give muxers advance
  * knowledge of future packets, improving e.g. the behaviour of the mp4
  * muxer for VFR content in fragmenting mode.
  *
  * @param s media file handle
  * @param pkt The packet containing the data to be written.
  *            <br>
  *            If the packet is reference-counted, this function will take
  *            ownership of this reference and unreference it later when it sees
  *            fit.
  *            The caller must not access the data through this reference after
  *            this function returns. If the packet is not reference-counted,
  *            libavformat will make a copy.
  *            <br>
  *            This parameter can be NULL (at any time, not just at the end), to
  *            flush the interleaving queues.
  *            <br>
  *            Packet's @ref AVPacket.stream_index "stream_index" field must be
  *            set to the index of the corresponding stream in @ref
  *            AVFormatContext.streams "s->streams".
  *            <br>
  *            The timestamps (@ref AVPacket.pts "pts", @ref AVPacket.dts "dts")
  *            must be set to correct values in the stream's timebase (unless the
  *            output format is flagged with the AVFMT_NOTIMESTAMPS flag, then
  *            they can be set to AV_NOPTS_VALUE).
  *            The dts for subsequent packets in one stream must be strictly
  *            increasing (unless the output format is flagged with the
  *            AVFMT_TS_NONSTRICT, then they merely have to be nondecreasing).
  *            @ref AVPacket.duration "duration") should also be set if known.
  *
  * @return 0 on success, a negative AVERROR on error. Libavformat will always
  *         take care of freeing the packet, even if this function fails.
  *
  * @see av_write_frame(), AVFormatContext.max_interleave_delta
*)
// int av_interleaved_write_frame(AVFormatContext *s, AVPacket *pkt);
function av_interleaved_write_frame(s: pAVFormatContext; pkt: pAVPacket): int; cdecl; external avformat_dll;
(* *
  * Write an uncoded frame to an output media file.
  *
  * The frame must be correctly interleaved according to the container
  * specification; if not, then av_interleaved_write_frame() must be used.
  *
  * See av_interleaved_write_frame() for details.
*)
// int av_write_uncoded_frame(AVFormatContext *s, int stream_index, AVFrame *frame);
function av_write_uncoded_frame(s: pAVFormatContext; stream_index: int; frame: pAVFrame): int; cdecl; external avformat_dll;
(* *
  * Write an uncoded frame to an output media file.
  *
  * If the muxer supports it, this function makes it possible to write an AVFrame
  * structure directly, without encoding it into a packet.
  * It is mostly useful for devices and similar special muxers that use raw
  * video or PCM data and will not serialize it into a byte stream.
  *
  * To test whether it is possible to use it with a given muxer and stream,
  * use av_write_uncoded_frame_query().
  *
  * The caller gives up ownership of the frame and must not access it
  * afterwards.
  *
  * @return  >=0 for success, a negative code on error
*)
// int av_interleaved_write_uncoded_frame(AVFormatContext *s, int stream_index, AVFrame *frame);
function av_interleaved_write_uncoded_frame(s: pAVFormatContext; stream_index: int; frame: pAVFrame): int; cdecl; external avformat_dll;
(* *
  * Test whether a muxer supports uncoded frame.
  *
  * @return  >=0 if an uncoded frame can be written to that muxer and stream,
  *          <0 if not
*)
// int av_write_uncoded_frame_query(AVFormatContext *s, int stream_index);
function av_write_uncoded_frame_query(s: pAVFormatContext; stream_index: int): int; cdecl; external avformat_dll;
(* *
  * Write the stream trailer to an output media file and free the
  * file private data.
  *
  * May only be called after a successful call to avformat_write_header.
  *
  * @param s media file handle
  * @return 0 if OK, AVERROR_xxx on error
*)
// int av_write_trailer(AVFormatContext *s);
function av_write_trailer(s: pAVFormatContext): int; cdecl; external avformat_dll;
(* *
  * Return the output format in the list of registered output formats
  * which best matches the provided parameters, or return NULL if
  * there is no match.
  *
  * @param short_name if non-NULL checks if short_name matches with the
  * names of the registered formats
  * @param filename if non-NULL checks if filename terminates with the
  * extensions of the registered formats
  * @param mime_type if non-NULL checks if mime_type matches with the
  * MIME type of the registered formats
*)
// AVOutputFormat *av_guess_format(const char *short_name,
// const char *filename,
// const char *mime_type);
function av_guess_format(const short_name: PAnsiChar; const filename: PAnsiChar; const mime_type: PAnsiChar): pAVOutputFormat; cdecl; external avformat_dll;
(* *
  * Guess the codec ID based upon muxer and filename.
*)
// enum AVCodecID av_guess_codec(AVOutputFormat *fmt, const char *short_name,
// const char *filename, const char *mime_type,
// enum AVMediaType type);
function av_guess_codec(fmt: pAVOutputFormat; const short_name: PAnsiChar; const filename: PAnsiChar; const mime_type: PAnsiChar; _type: AVMediaType)
  : AVCodecID; cdecl; external avformat_dll;
(* *
  * Get timing information for the data currently output.
  * The exact meaning of "currently output" depends on the format.
  * It is mostly relevant for devices that have an internal buffer and/or
  * work in real time.
  * @param s          media file handle
  * @param stream     stream in the media file
  * @param[out] dts   DTS of the last packet output for the stream, in stream
  *                   time_base units
  * @param[out] wall  absolute time when that packet whas output,
  *                   in microsecond
  * @return  0 if OK, AVERROR(ENOSYS) if the format does not support it
  * Note: some formats or devices may not allow to measure dts and wall
  * atomically.
*)
// int av_get_output_timestamp(struct AVFormatContext *s, int stream,
// int64_t *dts, int64_t *wall);
function av_get_output_timestamp(s: pAVFormatContext; stream: int; var dts: int64_t; var wall: int64_t): int; cdecl; external avformat_dll;
(* *
  * @}
*)

(* *
  * @defgroup lavf_misc Utility functions
  * @ingroup libavf
  * @{
  *
  * Miscellaneous utility functions related to both muxing and demuxing
  * (or neither).
*)

(* *
  * Send a nice hexadecimal dump of a buffer to the specified file stream.
  *
  * @param f The file stream pointer where the dump should be sent to.
  * @param buf buffer
  * @param size buffer size
  *
  * @see av_hex_dump_log, av_pkt_dump2, av_pkt_dump_log2
*)
// void av_hex_dump(FILE *f, const uint8_t *buf, int size);
procedure av_hex_dump(f: pFILE; const buf: puint8_t; size: int); cdecl; external avformat_dll;
(* *
  * Send a nice hexadecimal dump of a buffer to the log.
  *
  * @param avcl A pointer to an arbitrary struct of which the first field is a
  * pointer to an AVClass struct.
  * @param level The importance level of the message, lower values signifying
  * higher importance.
  * @param buf buffer
  * @param size buffer size
  *
  * @see av_hex_dump, av_pkt_dump2, av_pkt_dump_log2
*)
// void av_hex_dump_log(void *avcl, int level, const uint8_t *buf, int size);
procedure av_hex_dump_log(avcl: pointer; level: int; const buf: puint8_t; size: int); cdecl; external avformat_dll;
(* *
  * Send a nice dump of a packet to the specified file stream.
  *
  * @param f The file stream pointer where the dump should be sent to.
  * @param pkt packet to dump
  * @param dump_payload True if the payload must be displayed, too.
  * @param st AVStream that the packet belongs to
*)
// void av_pkt_dump2(FILE *f, const AVPacket *pkt, int dump_payload, const AVStream *st);
procedure av_pkt_dump2(f: pFILE; const pkt: pAVPacket; dump_payload: int; const st: pAVStream); cdecl; external avformat_dll;
(* *
  * Send a nice dump of a packet to the log.
  *
  * @param avcl A pointer to an arbitrary struct of which the first field is a
  * pointer to an AVClass struct.
  * @param level The importance level of the message, lower values signifying
  * higher importance.
  * @param pkt packet to dump
  * @param dump_payload True if the payload must be displayed, too.
  * @param st AVStream that the packet belongs to
*)
// void av_pkt_dump_log2(void *avcl, int level, const AVPacket *pkt, int dump_payload,
// const AVStream *st);
procedure av_pkt_dump_log2(avcl: pointer; level: int; const pkt: pAVPacket; dump_payload: int; const st: pAVStream); cdecl; external avformat_dll;
(* *
  * Get the AVCodecID for the given codec tag tag.
  * If no codec id is found returns AV_CODEC_ID_NONE.
  *
  * @param tags list of supported codec_id-codec_tag pairs, as stored
  * in AVInputFormat.codec_tag and AVOutputFormat.codec_tag
  * @param tag  codec tag to match to a codec ID
*)
// enum AVCodecID av_codec_get_id(const struct AVCodecTag * const *tags, unsigned int tag);
function av_codec_get_id(const tags: ppAVCodecTag; tag: unsigned_int): AVCodecID; cdecl; external avformat_dll;
(* *
  * Get the codec tag for the given codec id id.
  * If no codec tag is found returns 0.
  *
  * @param tags list of supported codec_id-codec_tag pairs, as stored
  * in AVInputFormat.codec_tag and AVOutputFormat.codec_tag
  * @param id   codec ID to match to a codec tag
*)
// unsigned int av_codec_get_tag(const struct AVCodecTag * const *tags, enum AVCodecID id);
function av_codec_get_tag(const tags: ppAVCodecTag; id: AVCodecID): unsigned_int; cdecl; external avformat_dll;
(* *
  * Get the codec tag for the given codec id.
  *
  * @param tags list of supported codec_id - codec_tag pairs, as stored
  * in AVInputFormat.codec_tag and AVOutputFormat.codec_tag
  * @param id codec id that should be searched for in the list
  * @param tag A pointer to the found tag
  * @return 0 if id was not found in tags, > 0 if it was found
*)
// int av_codec_get_tag2(const struct AVCodecTag * const *tags, enum AVCodecID id,
// unsigned int *tag);
function av_codec_get_tag2(const tags: ppAVCodecTag; id: AVCodecID; tag: punsigned_int): int; cdecl; external avformat_dll;

// int av_find_default_stream_index(AVFormatContext *s);
function av_find_default_stream_index(s: pAVFormatContext): int; cdecl; external avformat_dll;
(* *
  * Get the index for a specific timestamp.
  *
  * @param st        stream that the timestamp belongs to
  * @param timestamp timestamp to retrieve the index for
  * @param flags if AVSEEK_FLAG_BACKWARD then the returned index will correspond
  *                 to the timestamp which is <= the requested one, if backward
  *                 is 0, then it will be >=
  *              if AVSEEK_FLAG_ANY seek to any frame, only keyframes otherwise
  * @return < 0 if no such timestamp could be found
*)
// int av_index_search_timestamp(AVStream *st, int64_t timestamp, int flags);
function av_index_search_timestamp(st: pAVStream; timestamp: int64_t; flags: int): int; cdecl; external avformat_dll;
(* *
  * Add an index entry into a sorted list. Update the entry if the list
  * already contains it.
  *
  * @param timestamp timestamp in the time base of the given stream
*)
// int av_add_index_entry(AVStream *st, int64_t pos, int64_t timestamp,
// int size, int distance, int flags);
function av_add_index_entry(st: pAVStream; pos: int64_t; timestamp: int64_t; size: int; distance: int; flags: int): int; cdecl; external avformat_dll;
(* *
  * Split a URL string into components.
  *
  * The pointers to buffers for storing individual components may be null,
  * in order to ignore that component. Buffers for components not found are
  * set to empty strings. If the port is not found, it is set to a negative
  * value.
  *
  * @param proto the buffer for the protocol
  * @param proto_size the size of the proto buffer
  * @param authorization the buffer for the authorization
  * @param authorization_size the size of the authorization buffer
  * @param hostname the buffer for the host name
  * @param hostname_size the size of the hostname buffer
  * @param port_ptr a pointer to store the port number in
  * @param path the buffer for the path
  * @param path_size the size of the path buffer
  * @param url the URL to split
*)
// void av_url_split(char *proto,         int proto_size,
// char *authorization, int authorization_size,
// char *hostname,      int hostname_size,
// int *port_ptr,
// char *path,          int path_size,
// const char *url);
procedure av_url_split(proto: PAnsiChar; proto_size: int; authorization: PAnsiChar; authorization_size: int; hostname: PAnsiChar; hostname_size: int;
  var port_ptr: int; path: PAnsiChar; path_size: int; const url: PAnsiChar); cdecl; external avformat_dll;
(* *
  * Print detailed information about the input or output format, such as
  * duration, bitrate, streams, container, programs, metadata, side data,
  * codec and time base.
  *
  * @param ic        the context to analyze
  * @param index     index of the stream to dump information about
  * @param url       the URL to print, such as source or destination file
  * @param is_output Select whether the specified context is an input(0) or output(1)
*)
// void av_dump_format(AVFormatContext *ic,
// int index,
// const char *url,
// int is_output);
procedure av_dump_format(ic: pAVFormatContext; index: int; const url: PAnsiChar; is_output: int); cdecl; external avformat_dll;

const
  AV_FRAME_FILENAME_FLAGS_MULTIPLE = 1; // < Allow multiple %d

  (* *
    * Return in 'buf' the path with '%d' replaced by a number.
    *
    * Also handles the '%0nd' format where 'n' is the total number
    * of digits and '%%'.
    *
    * @param buf destination buffer
    * @param buf_size destination buffer size
    * @param path numbered sequence string
    * @param number frame number
    * @param flags AV_FRAME_FILENAME_FLAGS_*
    * @return 0 if OK, -1 on format error
  *)
  // int av_get_frame_filename2(char *buf, int buf_size,
  // const char *path, int number, int flags);
function av_get_frame_filename2(buf: PAnsiChar; buf_size: int; const path: PAnsiChar; number: int; flags: int): int; cdecl; external avformat_dll;

// int av_get_frame_filename(char *buf, int buf_size,
// const char *path, int number);
function av_get_frame_filename(buf: PAnsiChar; buf_size: int; const path: PAnsiChar; number: int): int; cdecl; external avformat_dll;
(* *
  * Check whether filename actually is a numbered sequence generator.
  *
  * @param filename possible numbered sequence string
  * @return 1 if a valid numbered sequence string, 0 otherwise
*)
// int av_filename_number_test(const char *filename);
function av_filename_number_test(const filename: PAnsiChar): int; cdecl; external avformat_dll;
(* *
  * Generate an SDP for an RTP session.
  *
  * Note, this overwrites the id values of AVStreams in the muxer contexts
  * for getting unique dynamic payload types.
  *
  * @param ac array of AVFormatContexts describing the RTP streams. If the
  *           array is composed by only one context, such context can contain
  *           multiple AVStreams (one AVStream per RTP stream). Otherwise,
  *           all the contexts in the array (an AVCodecContext per RTP stream)
  *           must contain only one AVStream.
  * @param n_files number of AVCodecContexts contained in ac
  * @param buf buffer where the SDP will be stored (must be allocated by
  *            the caller)
  * @param size the size of the buffer
  * @return 0 if OK, AVERROR_xxx on error
*)
// int av_sdp_create(AVFormatContext *ac[], int n_files, char *buf, int size);
function av_sdp_create(ac: ppAVFormatContext; n_files: int; buf: PAnsiChar; size: int): int; cdecl; external avformat_dll;
(* *
  * Return a positive value if the given filename has one of the given
  * extensions, 0 otherwise.
  *
  * @param filename   file name to check against the given extensions
  * @param extensions a comma-separated list of filename extensions
*)
// int av_match_ext(const char *filename, const char *extensions);
function av_match_ext(const filename: PAnsiChar; const extensions: PAnsiChar): int; cdecl; external avformat_dll;
(* *
  * Test if the given container can store a codec.
  *
  * @param ofmt           container to check for compatibility
  * @param codec_id       codec to potentially store in container
  * @param std_compliance standards compliance level, one of FF_COMPLIANCE_*
  *
  * @return 1 if codec with ID codec_id can be stored in ofmt, 0 if it cannot.
  *         A negative number if this information is not available.
*)
// int avformat_query_codec(const AVOutputFormat *ofmt, enum AVCodecID codec_id,
// int std_compliance);
function avformat_query_codec(const ofmt: pAVOutputFormat; codec_id: AVCodecID; std_compliance: int): int; cdecl; external avformat_dll;
(* *
  * @defgroup riff_fourcc RIFF FourCCs
  * @{
  * Get the tables mapping RIFF FourCCs to libavcodec AVCodecIDs. The tables are
  * meant to be passed to av_codec_get_id()/av_codec_get_tag() as in the
  * following code:
  * @code
  * uint32_t tag = MKTAG('H', '2', '6', '4');
  * const struct AVCodecTag *table[] = { avformat_get_riff_video_tags(), 0 };
  * enum AVCodecID id = av_codec_get_id(table, tag);
  * @endcode
*)
(* *
  * @return the table mapping RIFF FourCCs for video to libavcodec AVCodecID.
*)
// const struct AVCodecTag *avformat_get_riff_video_tags(void);
function avformat_get_riff_video_tags(): pAVCodecTag; cdecl; external avformat_dll;
(* *
  * @return the table mapping RIFF FourCCs for audio to AVCodecID.
*)
// const struct AVCodecTag *avformat_get_riff_audio_tags(void);
function avformat_get_riff_audio_tags(): pAVCodecTag; cdecl; external avformat_dll;
(* *
  * @return the table mapping MOV FourCCs for video to libavcodec AVCodecID.
*)
// const struct AVCodecTag *avformat_get_mov_video_tags(void);
function avformat_get_mov_video_tags(): pAVCodecTag; cdecl; external avformat_dll;
(* *
  * @return the table mapping MOV FourCCs for audio to AVCodecID.
*)
// const struct AVCodecTag *avformat_get_mov_audio_tags(void);
function avformat_get_mov_audio_tags(): pAVCodecTag; cdecl; external avformat_dll;
(* *
  * @}
*)

(* *
  * Guess the sample aspect ratio of a frame, based on both the stream and the
  * frame aspect ratio.
  *
  * Since the frame aspect ratio is set by the codec but the stream aspect ratio
  * is set by the demuxer, these two may not be equal. This function tries to
  * return the value that you should use if you would like to display the frame.
  *
  * Basic logic is to use the stream aspect ratio if it is set to something sane
  * otherwise use the frame aspect ratio. This way a container setting, which is
  * usually easy to modify can override the coded value in the frames.
  *
  * @param format the format context which the stream is part of
  * @param stream the stream which the frame is part of
  * @param frame the frame with the aspect ratio to be determined
  * @return the guessed (valid) sample_aspect_ratio, 0/1 if no idea
*)
// AVRational av_guess_sample_aspect_ratio(AVFormatContext *format, AVStream *stream, AVFrame *frame);
function av_guess_sample_aspect_ratio(format: pAVFormatContext; stream: pAVStream; frame: pAVFrame): AVRational; cdecl; external avformat_dll;
(* *
  * Guess the frame rate, based on both the container and codec information.
  *
  * @param ctx the format context which the stream is part of
  * @param stream the stream which the frame is part of
  * @param frame the frame for which the frame rate should be determined, may be NULL
  * @return the guessed (valid) frame rate, 0/1 if no idea
*)
// AVRational av_guess_frame_rate(AVFormatContext *ctx, AVStream *stream, AVFrame *frame);
function av_guess_frame_rate(ctx: pAVFormatContext; stream: pAVStream; frame: pAVFrame): AVRational; cdecl; external avformat_dll;
(* *
  * Check if the stream st contained in s is matched by the stream specifier
  * spec.
  *
  * See the "stream specifiers" chapter in the documentation for the syntax
  * of spec.
  *
  * @return  >0 if st is matched by spec;
  *          0  if st is not matched by spec;
  *          AVERROR code if spec is invalid
  *
  * @note  A stream specifier can match several streams in the format.
*)
// int avformat_match_stream_specifier(AVFormatContext *s, AVStream *st,
// const char *spec);
function avformat_match_stream_specifier(s: pAVFormatContext; st: pAVStream; const spec: PAnsiChar): int; cdecl; external avformat_dll;

// int avformat_queue_attached_pictures(AVFormatContext *s);
function avformat_queue_attached_pictures(s: pAVFormatContext): int; cdecl; external avformat_dll;
{$IFDEF FF_API_OLD_BSF}
(* *
  * Apply a list of bitstream filters to a packet.
  *
  * @param codec AVCodecContext, usually from an AVStream
  * @param pkt the packet to apply filters to. If, on success, the returned
  *        packet has size == 0 and side_data_elems == 0, it indicates that
  *        the packet should be dropped
  * @param bsfc a NULL-terminated list of filters to apply
  * @return  >=0 on success;
  *          AVERROR code on failure
*)
// attribute_deprecated
// int av_apply_bitstream_filters(AVCodecContext *codec, AVPacket *pkt,
// AVBitStreamFilterContext *bsfc);
function av_apply_bitstream_filters(codec: pAVCodecContext; pkt: pAVPacket; bsfc: pAVBitStreamFilterContext): int; cdecl; external avformat_dll;
{$ENDIF}

type
  AVTimebaseSource = (    //
    AVFMT_TBCF_AUTO = -1, //
    AVFMT_TBCF_DECODER,   //
    AVFMT_TBCF_DEMUXER    //
{$IFDEF FF_API_R_FRAME_RATE}
    , AVFMT_TBCF_R_FRAMERATE
{$ENDIF}
    );

  (* *
    * Transfer internal timing information from one stream to another.
    *
    * This function is useful when doing stream copy.
    *
    * @param ofmt     target output format for ost
    * @param ost      output stream which needs timings copy and adjustments
    * @param ist      reference input stream to copy timings from
    * @param copy_tb  define from where the stream codec timebase needs to be imported
  *)
  // int avformat_transfer_internal_stream_timing_info(const AVOutputFormat *ofmt,
  // AVStream *ost, const AVStream *ist,
  // enum AVTimebaseSource copy_tb);
function avformat_transfer_internal_stream_timing_info(const ofmt: pAVOutputFormat; ost: pAVStream; const ist: pAVStream; copy_tb: AVTimebaseSource): int;
  cdecl; external avformat_dll;
(* *
  * Get the internal codec timebase from a stream.
  *
  * @param st  input stream to extract the timebase from
*)
// AVRational av_stream_get_codec_timebase(const AVStream *st);
function av_stream_get_codec_timebase(const st: AVStream): AVRational; cdecl; external avformat_dll;
{$ENDREGION}

implementation

function avio_tell(s: pAVIOContext): int64_t; inline;
begin
  Result := avio_seek(s, 0, 1 { SEEK_CUR } );
end;

end.
