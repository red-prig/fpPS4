unit libavfilter;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

Uses
  ffmpeg_types, libavutil, libavformat;

{$I ffmpeg.inc}
{$REGION 'avfilter.h'}
(* *
  * Return the LIBAVFILTER_VERSION_INT constant.
*)
// unsigned avfilter_version(void);
function avfilter_version(): unsigned; cdecl; external avfilter_dll;

(* *
  * Return the libavfilter build-time configuration.
*)
// const char *avfilter_configuration(void);
function avfilter_configuration(): pAnsiChar; cdecl; external avfilter_dll;
(* *
  * Return the libavfilter license.
*)
// const char *avfilter_license(void);
function avfilter_license(): pAnsiChar; cdecl; external avfilter_dll;

const
  (* *
    * Process multiple parts of the frame concurrently.
  *)
  AVFILTER_THREAD_SLICE = (1 shl 0);

const
  (* *
    * The number of the filter inputs is not determined just by AVFilter.inputs.
    * The filter might add additional inputs during initialization depending on the
    * options supplied to it.
  *)
  AVFILTER_FLAG_DYNAMIC_INPUTS = (1 shl 0);
  (* *
    * The number of the filter outputs is not determined just by AVFilter.outputs.
    * The filter might add additional outputs during initialization depending on
    * the options supplied to it.
  *)
  AVFILTER_FLAG_DYNAMIC_OUTPUTS = (1 shl 1);
  (* *
    * The filter supports multithreading by splitting frames into multiple parts
    * and processing them concurrently.
  *)
  AVFILTER_FLAG_SLICE_THREADS = (1 shl 2);
  (* *
    * Some filters support a generic "enable" expression option that can be used
    * to enable or disable a filter in the timeline. Filters supporting this
    * option have this flag set. When the enable expression is false, the default
    * no-op filter_frame() function is called in place of the filter_frame()
    * callback defined on each input pad, thus the frame is passed unchanged to
    * the next filters.
  *)
  AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC = (1 shl 16);
  (* *
    * Same as AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC, except that the filter will
    * have its filter_frame() callback(s) called as usual even when the enable
    * expression is false. The filter will disable filtering within the
    * filter_frame() callback(s) itself, for example executing code depending on
    * the AVFilterContext->is_disabled value.
  *)
  AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL = (1 shl 17);
  (* *
    * Handy mask to test whether the filter supports or no the timeline feature
    * (internally or generically).
  *)
  AVFILTER_FLAG_SUPPORT_TIMELINE = (AVFILTER_FLAG_SUPPORT_TIMELINE_GENERIC or AVFILTER_FLAG_SUPPORT_TIMELINE_INTERNAL);

type
  pAVFilterContext = ^AVFilterContext;
  ppAVFilterContext = ^pAVFilterContext;
  pAVFilterLink = ^AVFilterLink;
  ppAVFilterLink = ^pAVFilterLink;

  pAVFilterPad = ^AVFilterPad;

  AVFilterPad = record
  end;
  // pAVFilterFormats=^AVFilterFormats;
  // AVFilterFormats = record
  // end;

  pAVFilterGraph = ^AVFilterGraph;

  pAVFilterCommand = ^AVFilterCommand;

  AVFilterCommand = record
  end;

  (* *
    * Filter definition. This defines the pads a filter contains, and all the
    * callback functions used to interact with the filter.
  *)
  pAVFilter = ^AVFilter;

  AVFilter = record
    (* *
      * Filter name. Must be non-NULL and unique among filters.
    *)
    name: pAnsiChar;

    (* *
      * A description of the filter. May be NULL.
      *
      * You should use the NULL_IF_CONFIG_SMALL() macro to define it.
    *)
    description: pAnsiChar;

    (* *
      * List of inputs, terminated by a zeroed element.
      *
      * NULL if there are no (static) inputs. Instances of filters with
      * AVFILTER_FLAG_DYNAMIC_INPUTS set may have more inputs than present in
      * this list.
    *)
    inputs: pAVFilterPad;
    (* *
      * List of outputs, terminated by a zeroed element.
      *
      * NULL if there are no (static) outputs. Instances of filters with
      * AVFILTER_FLAG_DYNAMIC_OUTPUTS set may have more outputs than present in
      * this list.
    *)
    outputs: pAVFilterPad;

    (* *
      * A class for the private data, used to declare filter private AVOptions.
      * This field is NULL for filters that do not declare any options.
      *
      * If this field is non-NULL, the first member of the filter private data
      * must be a pointer to AVClass, which will be set by libavfilter generic
      * code to this class.
    *)
    priv_class: pAVClass;

    (* *
      * A combination of AVFILTER_FLAG_*
    *)
    flags: int;

    (* ****************************************************************
      * All fields below this line are not part of the public API. They
      * may not be used outside of libavfilter and can be changed and
      * removed at will.
      * New public fields should be added right above.
      *****************************************************************
    *)

    (* *
      * Filter pre-initialization function
      *
      * This callback will be called immediately after the filter context is
      * allocated, to allow allocating and initing sub-objects.
      *
      * If this callback is not NULL, the uninit callback will be called on
      * allocation failure.
      *
      * @return 0 on success,
      *         AVERROR code on failure (but the code will be
      *           dropped and treated as ENOMEM by the calling code)
    *)
    // int (*preinit)(AVFilterContext *ctx);
    preinit: function(ctx: pAVFilterContext): int; cdecl;
    (* *
      * Filter initialization function.
      *
      * This callback will be called only once during the filter lifetime, after
      * all the options have been set, but before links between filters are
      * established and format negotiation is done.
      *
      * Basic filter initialization should be done here. Filters with dynamic
      * inputs and/or outputs should create those inputs/outputs here based on
      * provided options. No more changes to this filter's inputs/outputs can be
      * done after this callback.
      *
      * This callback must not assume that the filter links exist or frame
      * parameters are known.
      *
      * @ref AVFilter.uninit "uninit" is guaranteed to be called even if
      * initialization fails, so this callback does not have to clean up on
      * failure.
      *
      * @return 0 on success, a negative AVERROR on failure
    *)
    // int (*init)(AVFilterContext *ctx);
    init: function(ctx: pAVFilterContext): int; cdecl;
    (* *
      * Should be set instead of @ref AVFilter.init "init" by the filters that
      * want to pass a dictionary of AVOptions to nested contexts that are
      * allocated during init.
      *
      * On return, the options dict should be freed and replaced with one that
      * contains all the options which could not be processed by this filter (or
      * with NULL if all the options were processed).
      *
      * Otherwise the semantics is the same as for @ref AVFilter.init "init".
    *)
    // int (*init_dict)(AVFilterContext *ctx, AVDictionary **options);
    init_dict: function(ctx: pAVFilterContext; var options: pAVDictionary): int; cdecl;
    (* *
      * Filter uninitialization function.
      *
      * Called only once right before the filter is freed. Should deallocate any
      * memory held by the filter, release any buffer references, etc. It does
      * not need to deallocate the AVFilterContext.priv memory itself.
      *
      * This callback may be called even if @ref AVFilter.init "init" was not
      * called or failed, so it must be prepared to handle such a situation.
    *)
    // void (*uninit)(AVFilterContext *ctx);
    uninit: procedure(ctx: pAVFilterContext); cdecl;
    (* *
      * Query formats supported by the filter on its inputs and outputs.
      *
      * This callback is called after the filter is initialized (so the inputs
      * and outputs are fixed), shortly before the format negotiation. This
      * callback may be called more than once.
      *
      * This callback must set AVFilterLink.out_formats on every input link and
      * AVFilterLink.in_formats on every output link to a list of pixel/sample
      * formats that the filter supports on that link. For audio links, this
      * filter must also set @ref AVFilterLink.in_samplerates "in_samplerates" /
      * @ref AVFilterLink.out_samplerates "out_samplerates" and
      * @ref AVFilterLink.in_channel_layouts "in_channel_layouts" /
      * @ref AVFilterLink.out_channel_layouts "out_channel_layouts" analogously.
      *
      * This callback may be NULL for filters with one input, in which case
      * libavfilter assumes that it supports all input formats and preserves
      * them on output.
      *
      * @return zero on success, a negative value corresponding to an
      * AVERROR code otherwise
    *)
    // int (*query_formats)(AVFilterContext *);
    query_formats: function(p: pAVFilterContext): int; cdecl;
    priv_size: int; // < size of private data to allocate for the filter

    flags_internal: int; // < Additional flags for avfilter internal use only.

    (* *
      * Used by the filter registration system. Must not be touched by any other
      * code.
    *)
    next: pAVFilter;

    (* *
      * Make the filter instance process a command.
      *
      * @param cmd    the command to process, for handling simplicity all commands must be alphanumeric only
      * @param arg    the argument for the command
      * @param res    a buffer with size res_size where the filter(s) can return a response. This must not change when the command is not supported.
      * @param flags  if AVFILTER_CMD_FLAG_FAST is set and the command would be
      *               time consuming then a filter should treat it like an unsupported command
      *
      * @returns >=0 on success otherwise an error code.
      *          AVERROR(ENOSYS) on unsupported commands
    *)
    // int (*process_command)(AVFilterContext *, const char *cmd, const char *arg, char *res, int res_len, int flags);
    process_command: function(ctx: pAVFilterContext; const cmd: pAnsiChar; const arg: pAnsiChar; res: pAnsiChar; res_len: int; flags: int): int; cdecl;
    (* *
      * Filter initialization function, alternative to the init()
      * callback. Args contains the user-supplied parameters, opaque is
      * used for providing binary data.
    *)
    // int (*init_opaque)(AVFilterContext *ctx, void *opaque);
    init_opaque: function(ctx: pAVFilterContext; opaque: pointer): int; cdecl;
    (* *
      * Filter activation function.
      *
      * Called when any processing is needed from the filter, instead of any
      * filter_frame and request_frame on pads.
      *
      * The function must examine inlinks and outlinks and perform a single
      * step of processing. If there is nothing to do, the function must do
      * nothing and not return an error. If more steps are or may be
      * possible, it must use ff_filter_set_ready() to schedule another
      * activation.
    *)
    // int (*activate)(AVFilterContext *ctx);
    activate: function(ctx: pAVFilterContext): int; cdecl;
  end;

  pAVFilterInternal = ^AVFilterInternal;

  AVFilterInternal = record
  end;
  (* * An instance of a filter *)

  AVFilterContext = record
    av_class: pAVClass; // < needed for av_log() and filters common options

    filter: pAVFilter; // < the AVFilter of which this is an instance

    name: pAnsiChar; // < name of this filter instance

    input_pads: pAVFilterPad; // < array of input pads
    inputs: ppAVFilterLink;   // < array of pointers to input links
    nb_inputs: unsigned;      // < number of input pads

    output_pads: pAVFilterPad; // < array of output pads
    outputs: ppAVFilterLink;   // < array of pointers to output links
    nb_outputs: unsigned;      // < number of output pads

    priv: pointer; // < private data for use by the filter

    graph: pAVFilterGraph; // < filtergraph this filter belongs to

    (* *
      * Type of multithreading being allowed/used. A combination of
      * AVFILTER_THREAD_* flags.
      *
      * May be set by the caller before initializing the filter to forbid some
      * or all kinds of multithreading for this filter. The default is allowing
      * everything.
      *
      * When the filter is initialized, this field is combined using bit AND with
      * AVFilterGraph.thread_type to get the final mask used for determining
      * allowed threading types. I.e. a threading type needs to be set in both
      * to be allowed.
      *
      * After the filter is initialized, libavfilter sets this field to the
      * threading type that is actually used (0 for no multithreading).
    *)
    thread_type: int;

    (* *
      * An opaque struct for libavfilter internal use.
    *)
    internal: pAVFilterInternal;

    command_queue: pAVFilterCommand;

    enable_str: pAnsiChar; // < enable expression string
    enable: pointer;       // < parsed expression (AVExpr*)
    var_values: pdouble;   // < variable values for the enable expression
    is_disabled: int;      // < the enabled state from the last expression evaluation

    (* *
      * For filters which will create hardware frames, sets the device the
      * filter should create them in.  All other filters will ignore this field:
      * in particular, a filter which consumes or processes hardware frames will
      * instead use the hw_frames_ctx field in AVFilterLink to carry the
      * hardware context information.
    *)
    hw_device_ctx: pAVBufferRef;

    (* *
      * Max number of threads allowed in this filter instance.
      * If <= 0, its value is ignored.
      * Overrides global number of threads set per filter graph.
    *)
    nb_threads: int;

    (* *
      * Ready status of the filter.
      * A non-0 value means that the filter needs activating;
      * a higher value suggests a more urgent activation.
    *)
    ready: unsigned;

    (* *
      * Sets the number of extra hardware frames which the filter will
      * allocate on its output links for use in following filters or by
      * the caller.
      *
      * Some hardware filters require all frames that they will use for
      * output to be defined in advance before filtering starts.  For such
      * filters, any hardware frame pools used for output must therefore be
      * of fixed size.  The extra frames set here are on top of any number
      * that the filter needs internally in order to operate normally.
      *
      * This field must be set before the graph containing this filter is
      * configured.
    *)
    extra_hw_frames: int;
  end;

  pAVFilterGraphInternal = ^AVFilterGraphInternal;

  AVFilterGraphInternal = record
  end;

  (* *
    * A function pointer passed to the @ref AVFilterGraph.execute callback to be
    * executed multiple times, possibly in parallel.
    *
    * @param ctx the filter context the job belongs to
    * @param arg an opaque parameter passed through from @ref
    *            AVFilterGraph.execute
    * @param jobnr the index of the job being executed
    * @param nb_jobs the total number of jobs
    *
    * @return 0 on success, a negative AVERROR on error
  *)
  // typedef int (avfilter_action_func)(AVFilterContext *ctx, void *arg, int jobnr, int nb_jobs);
  Tavfilter_action_func = function(ctx: pAVFilterContext; arg: pointer; jobnr: int; b_jobs: int): int; cdecl;

  (* *
    * A function executing multiple jobs, possibly in parallel.
    *
    * @param ctx the filter context to which the jobs belong
    * @param func the function to be called multiple times
    * @param arg the argument to be passed to func
    * @param ret a nb_jobs-sized array to be filled with return values from each
    *            invocation of func
    * @param nb_jobs the number of jobs to execute
    *
    * @return 0 on success, a negative AVERROR on error
  *)
  // typedef int (avfilter_execute_func)(AVFilterContext *ctx, avfilter_action_func *func,
  // void *arg, int *ret, int nb_jobs);
  Tavfilter_execute_func = function(ctx: pAVFilterContext; func: Tavfilter_action_func; arg: pointer; var ret: int; nb_jobs: int): int; cdecl;

  AVFilterGraph = record
    av_class: pAVClass;
    filters: ppAVFilterContext;
    nb_filters: unsigned;

    scale_sws_opts: pAnsiChar; // < sws options to use for the auto-inserted scale filters
{$IFDEF FF_API_LAVR_OPTS}
    // attribute_deprecated
    resample_lavr_opts: pAnsiChar deprecated; // < libavresample options to use for the auto-inserted resample filters
{$ENDIF}
    (* *
      * Type of multithreading allowed for filters in this graph. A combination
      * of AVFILTER_THREAD_* flags.
      *
      * May be set by the caller at any point, the setting will apply to all
      * filters initialized after that. The default is allowing everything.
      *
      * When a filter in this graph is initialized, this field is combined using
      * bit AND with AVFilterContext.thread_type to get the final mask used for
      * determining allowed threading types. I.e. a threading type needs to be
      * set in both to be allowed.
    *)
    thread_type: int;

    (* *
      * Maximum number of threads used by filters in this graph. May be set by
      * the caller before adding any filters to the filtergraph. Zero (the
      * default) means that the number of threads is determined automatically.
    *)
    nb_threads: int;

    (* *
      * Opaque object for libavfilter internal use.
    *)
    internal: pAVFilterGraphInternal;

    (* *
      * Opaque user data. May be set by the caller to an arbitrary value, e.g. to
      * be used from callbacks like @ref AVFilterGraph.execute.
      * Libavfilter will not touch this field in any way.
    *)
    opaque: pointer;

    (* *
      * This callback may be set by the caller immediately after allocating the
      * graph and before adding any filters to it, to provide a custom
      * multithreading implementation.
      *
      * If set, filters with slice threading capability will call this callback
      * to execute multiple jobs in parallel.
      *
      * If this field is left unset, libavfilter will use its internal
      * implementation, which may or may not be multithreaded depending on the
      * platform and build options.
    *)
    execute: Tavfilter_execute_func;

    aresample_swr_opts: pAnsiChar; // < swr options to use for the auto-inserted aresample filters, Access ONLY through AVOptions

    (* *
      * Private fields
      *
      * The following fields are for internal use only.
      * Their type, offset, number and semantic can change without notice.
    *)

    sink_links: ppAVFilterLink;
    sink_links_count: int;

    disable_auto_convert: unsigned;
  end;

  (* *
    * A link between two filters. This contains pointers to the source and
    * destination filters between which this link exists, and the indexes of
    * the pads involved. In addition, this link also contains the parameters
    * which have been negotiated and agreed upon between the filter, such as
    * image dimensions, format, etc.
    *
    * Applications must not normally access the link structure directly.
    * Use the buffersrc and buffersink API instead.
    * In the future, access to the header may be reserved for filters
    * implementation.
  *)
  Tinit_state = (      //
    AVLINK_UNINIT = 0, // < not started
    AVLINK_STARTINIT,  // < started, but incomplete
    AVLINK_INIT        // < complete
    );

  AVFilterLink = record
    src: pAVFilterContext; // < source filter
    srcpad: pAVFilterPad;  // < output pad on the source filter

    dst: pAVFilterContext; // < dest filter
    dstpad: pAVFilterPad;  // < input pad on the dest filter

    _type: AVMediaType; // < filter media type

    (* These parameters apply only to video *)
    w: int;                          // < agreed upon image width
    h: int;                          // < agreed upon image height
    sample_aspect_ratio: AVRational; // < agreed upon sample aspect ratio
    (* These parameters apply only to audio *)
    channel_layout: uint64_t; // < channel layout of current buffer (see libavutil/channel_layout.h)
    sample_rate: int;         // < samples per second

    format: int; // < agreed upon media format

    (* *
      * Define the time base used by the PTS of the frames/samples
      * which will pass through this link.
      * During the configuration stage, each filter is supposed to
      * change only the output timebase, while the timebase of the
      * input link is assumed to be an unchangeable property.
    *)
    time_base: AVRational;

    (* ****************************************************************
      * All fields below this line are not part of the public API. They
      * may not be used outside of libavfilter and can be changed and
      * removed at will.
      * New public fields should be added right above.
      *****************************************************************
    *)
    (* *
      * Lists of formats and channel layouts supported by the input and output
      * filters respectively. These lists are used for negotiating the format
      * to actually be used, which will be loaded into the format and
      * channel_layout members, above, when chosen.
      *
    *)
    in_formats: pAVFilterFormats;
    out_formats: pAVFilterFormats;

    (* *
      * Lists of channel layouts and sample rates used for automatic
      * negotiation.
    *)
    in_samplerates: pAVFilterFormats;
    out_samplerates: pAVFilterFormats;
    in_channel_layouts: pAVFilterChannelLayouts;
    out_channel_layouts: pAVFilterChannelLayouts;

    (* *
      * Audio only, the destination filter sets this to a non-zero value to
      * request that buffers with the given number of samples should be sent to
      * it. AVFilterPad.needs_fifo must also be set on the corresponding input
      * pad.
      * Last buffer before EOF will be padded with silence.
    *)
    request_samples: int;

    (* * stage of the initialization of the link properties (dimensions, etc) *)
    init_state: Tinit_state;

    (* *
      * Graph the filter belongs to.
    *)
    graph: pAVFilterGraph;

    (* *
      * Current timestamp of the link, as defined by the most recent
      * frame(s), in link time_base units.
    *)
    current_pts: int64_t;

    (* *
      * Current timestamp of the link, as defined by the most recent
      * frame(s), in AV_TIME_BASE units.
    *)
    current_pts_us: int64_t;

    (* *
      * Index in the age array.
    *)
    age_index: int;

    (* *
      * Frame rate of the stream on the link, or 1/0 if unknown or variable;
      * if left to 0/0, will be automatically copied from the first input
      * of the source filter if it exists.
      *
      * Sources should set it to the best estimation of the real frame rate.
      * If the source frame rate is unknown or variable, set this to 1/0.
      * Filters should update it if necessary depending on their function.
      * Sinks can use it to set a default output frame rate.
      * It is similar to the r_frame_rate field in AVStream.
    *)
    frame_rate: AVRational;

    (* *
      * Buffer partially filled with samples to achieve a fixed/minimum size.
    *)
    partial_buf: pAVFrame;

    (* *
      * Size of the partial buffer to allocate.
      * Must be between min_samples and max_samples.
    *)
    partial_buf_size: int;

    (* *
      * Minimum number of samples to filter at once. If filter_frame() is
      * called with fewer samples, it will accumulate them in partial_buf.
      * This field and the related ones must not be changed after filtering
      * has started.
      * If 0, all related fields are ignored.
    *)
    min_samples: int;

    (* *
      * Maximum number of samples to filter at once. If filter_frame() is
      * called with more samples, it will split them.
    *)
    max_samples: int;

    (* *
      * Number of channels.
    *)
    channels: int;

    (* *
      * Link processing flags.
    *)
    flags: unsigned;

    (* *
      * Number of past frames sent through the link.
    *)
    frame_count_in, frame_count_out: int64_t;

    (* *
      * A pointer to a FFFramePool struct.
    *)
    frame_pool: pointer;

    (* *
      * True if a frame is currently wanted on the output of this filter.
      * Set when ff_request_frame() is called by the output,
      * cleared when a frame is filtered.
    *)
    frame_wanted_out: int;

    (* *
      * For hwaccel pixel formats, this should be a reference to the
      * AVHWFramesContext describing the frames.
    *)
    hw_frames_ctx: pAVBufferRef;

{$IFNDEF FF_INTERNAL_FIELDS}
    (* *
      * Internal structure members.
      * The fields below this limit are internal for libavfilter's use
      * and must in no way be accessed by applications.
    *)
    reserved: array [0 .. $F000 - 1] of AnsiChar;

{$ELSE} (* FF_INTERNAL_FIELDS *)

    (* *
      * Queue of frames waiting to be filtered.
    *)
    fifo: FFFrameQueue;

    (* *
      * If set, the source filter can not generate a frame as is.
      * The goal is to avoid repeatedly calling the request_frame() method on
      * the same link.
    *)
    frame_blocked_in: int;

    (* *
      * Link input status.
      * If not zero, all attempts of filter_frame will fail with the
      * corresponding code.
    *)
    status_in: int;

    (* *
      * Timestamp of the input status change.
    *)
    status_in_pts: int64_t;

    (* *
      * Link output status.
      * If not zero, all attempts of request_frame will fail with the
      * corresponding code.
    *)
    status_out: int;

{$ENDIF} (* FF_INTERNAL_FIELDS *)
  end;

  (* *
    * Link two filters together.
    *
    * @param src    the source filter
    * @param srcpad index of the output pad on the source filter
    * @param dst    the destination filter
    * @param dstpad index of the input pad on the destination filter
    * @return       zero on success
  *)
  // int avfilter_link(AVFilterContext *src, unsigned srcpad, AVFilterContext *dst, unsigned dstpad);
function avfilter_link(src: pAVFilterContext; srcpad: unsigned; dst: pAVFilterContext; dstpad: unsigned): int; cdecl; external avfilter_dll;
(* *
  * Free the link in *link, and set its pointer to NULL.
*)
// void avfilter_link_free(AVFilterLink **link);
procedure avfilter_link_free(var link: pAVFilterLink); cdecl; external avfilter_dll;

{$IFDEF FF_API_FILTER_GET_SET}
(* *
  * Get the number of channels of a link.
  * @deprecated Use av_buffersink_get_channels()
*)
// attribute_deprecated
// int avfilter_link_get_channels(AVFilterLink *link);
function avfilter_link_get_channels(link: pAVFilterLink): int; deprecated 'Use av_buffersink_get_channels()'; cdecl; external avfilter_dll;
{$ENDIF}
(* *
  * Set the closed field of a link.
  * @deprecated applications are not supposed to mess with links, they should
  * close the sinks.
*)
// attribute_deprecated
// void avfilter_link_set_closed(AVFilterLink *link, int closed);
procedure avfilter_link_set_closed(link: pAVFilterLink; closed: int);
  deprecated 'applications are not supposed to mess with links, they should close the sinks.'; cdecl; external avfilter_dll;
(* *
  * Negotiate the media format, dimensions, etc of all inputs to a filter.
  *
  * @param filter the filter to negotiate the properties for its inputs
  * @return       zero on successful negotiation
*)
// int avfilter_config_links(AVFilterContext *filter);
function avfilter_config_links(filter: pAVFilterContext): int; cdecl; external avfilter_dll;

const
  AVFILTER_CMD_FLAG_ONE = 1;
  // < Stop once a filter understood the command (for target=all for example), fast filters are favored automatically
  AVFILTER_CMD_FLAG_FAST = 2; // < Only execute command when its fast (like a video out that supports contrast adjustment in hw)

  (* *
    * Make the filter instance process a command.
    * It is recommended to use avfilter_graph_send_command().
  *)
  // int avfilter_process_command(AVFilterContext *filter, const char *cmd, const char *arg, char *res, int res_len, int flags);
function avfilter_process_command(filter: pAVFilterContext; const cmd: pAnsiChar; const arg: pAnsiChar; res: pAnsiChar; res_len: int; flags: int): int; cdecl;
  external avfilter_dll;
(* *
  * Iterate over all registered filters.
  *
  * @param opaque a pointer where libavfilter will store the iteration state. Must
  *               point to NULL to start the iteration.
  *
  * @return the next registered filter or NULL when the iteration is
  *         finished
*)
// const AVFilter *av_filter_iterate(void **opaque);
function av_filter_iterate(var opaque: pointer): pAVFilter; cdecl; external avfilter_dll;

{$IFDEF FF_API_NEXT}
(* * Initialize the filter system. Register all builtin filters. *)
// attribute_deprecated
// void avfilter_register_all(void);
procedure avfilter_register_all(); deprecated; cdecl; external avfilter_dll;
(* *
  * Register a filter. This is only needed if you plan to use
  * avfilter_get_by_name later to lookup the AVFilter structure by name. A
  * filter can still by instantiated with avfilter_graph_alloc_filter even if it
  * is not registered.
  *
  * @param filter the filter to register
  * @return 0 if the registration was successful, a negative value
  * otherwise
*)
// attribute_deprecated
// int avfilter_register(AVFilter *filter);
function avfilter_register(filter: pAVFilter): int; deprecated; cdecl; external avfilter_dll;
(* *
  * Iterate over all registered filters.
  * @return If prev is non-NULL, next registered filter after prev or NULL if
  * prev is the last filter. If prev is NULL, return the first registered filter.
*)
// attribute_deprecated
// const AVFilter *avfilter_next(const AVFilter *prev);
function avfilter_next(const prev: pAVFilter): pAVFilter; deprecated; cdecl; external avfilter_dll;
{$ENDIF}
(* *
  * Get a filter definition matching the given name.
  *
  * @param name the filter name to find
  * @return     the filter definition, if any matching one is registered.
  *             NULL if none found.
*)
// const AVFilter *avfilter_get_by_name(const char *name);
function avfilter_get_by_name(const name: pAnsiChar): pAVFilter; cdecl; external avfilter_dll;
(* *
  * Initialize a filter with the supplied parameters.
  *
  * @param ctx  uninitialized filter context to initialize
  * @param args Options to initialize the filter with. This must be a
  *             ':'-separated list of options in the 'key=value' form.
  *             May be NULL if the options have been set directly using the
  *             AVOptions API or there are no options that need to be set.
  * @return 0 on success, a negative AVERROR on failure
*)
// int avfilter_init_str(AVFilterContext *ctx, const char *args);
function avfilter_init_str(ctx: pAVFilterContext; const args: pAnsiChar): int; cdecl; external avfilter_dll;
(* *
  * Initialize a filter with the supplied dictionary of options.
  *
  * @param ctx     uninitialized filter context to initialize
  * @param options An AVDictionary filled with options for this filter. On
  *                return this parameter will be destroyed and replaced with
  *                a dict containing options that were not found. This dictionary
  *                must be freed by the caller.
  *                May be NULL, then this function is equivalent to
  *                avfilter_init_str() with the second parameter set to NULL.
  * @return 0 on success, a negative AVERROR on failure
  *
  * @note This function and avfilter_init_str() do essentially the same thing,
  * the difference is in manner in which the options are passed. It is up to the
  * calling code to choose whichever is more preferable. The two functions also
  * behave differently when some of the provided options are not declared as
  * supported by the filter. In such a case, avfilter_init_str() will fail, but
  * this function will leave those extra options in the options AVDictionary and
  * continue as usual.
*)
// int avfilter_init_dict(AVFilterContext *ctx, AVDictionary **options);
function avfilter_init_dict(ctx: pAVFilterContext; var options: pAVDictionary): int; cdecl; external avfilter_dll;
(* *
  * Free a filter context. This will also remove the filter from its
  * filtergraph's list of filters.
  *
  * @param filter the filter to free
*)
// void avfilter_free(AVFilterContext *filter);
procedure avfilter_free(filter: pAVFilterContext); cdecl; external avfilter_dll;
(* *
  * Insert a filter in the middle of an existing link.
  *
  * @param link the link into which the filter should be inserted
  * @param filt the filter to be inserted
  * @param filt_srcpad_idx the input pad on the filter to connect
  * @param filt_dstpad_idx the output pad on the filter to connect
  * @return     zero on success
*)
// int avfilter_insert_filter(AVFilterLink *link, AVFilterContext *filt,
// unsigned filt_srcpad_idx, unsigned filt_dstpad_idx);
function avfilter_insert_filter(link: pAVFilterLink; filt: pAVFilterContext; filt_srcpad_idx: unsigned; filt_dstpad_idx: unsigned): int; cdecl;
  external avfilter_dll;
(* *
  * @return AVClass for AVFilterContext.
  *
  * @see av_opt_find().
*)
// const AVClass *avfilter_get_class(void);
function avfilter_get_class(): pAVClass; cdecl; external avfilter_dll;

(* *
  * Allocate a filter graph.
  *
  * @return the allocated filter graph on success or NULL.
*)
// AVFilterGraph *avfilter_graph_alloc(void);
function avfilter_graph_alloc(): pAVFilterGraph; cdecl; external avfilter_dll;
(* *
  * Create a new filter instance in a filter graph.
  *
  * @param graph graph in which the new filter will be used
  * @param filter the filter to create an instance of
  * @param name Name to give to the new instance (will be copied to
  *             AVFilterContext.name). This may be used by the caller to identify
  *             different filters, libavfilter itself assigns no semantics to
  *             this parameter. May be NULL.
  *
  * @return the context of the newly created filter instance (note that it is
  *         also retrievable directly through AVFilterGraph.filters or with
  *         avfilter_graph_get_filter()) on success or NULL on failure.
*)
// AVFilterContext *avfilter_graph_alloc_filter(AVFilterGraph *graph,
// const AVFilter *filter,
// const char *name);
function avfilter_graph_alloc_filter(graph: pAVFilterGraph; const filter: pAVFilter; const name: pAnsiChar): pAVFilterContext; cdecl; external avfilter_dll;
(* *
  * Get a filter instance identified by instance name from graph.
  *
  * @param graph filter graph to search through.
  * @param name filter instance name (should be unique in the graph).
  * @return the pointer to the found filter instance or NULL if it
  * cannot be found.
*)
// AVFilterContext *avfilter_graph_get_filter(AVFilterGraph *graph, const char *name);
function avfilter_graph_get_filter(graph: pAVFilterGraph; const name: pAnsiChar): pAVFilterContext; cdecl; external avfilter_dll;
(* *
  * Create and add a filter instance into an existing graph.
  * The filter instance is created from the filter filt and inited
  * with the parameters args and opaque.
  *
  * In case of success put in *filt_ctx the pointer to the created
  * filter instance, otherwise set *filt_ctx to NULL.
  *
  * @param name the instance name to give to the created filter instance
  * @param graph_ctx the filter graph
  * @return a negative AVERROR error code in case of failure, a non
  * negative value otherwise
*)
// int avfilter_graph_create_filter(AVFilterContext **filt_ctx, const AVFilter *filt,
// const char *name, const char *args, void *opaque,
// AVFilterGraph *graph_ctx);
function avfilter_graph_create_filter(var filt_ctx: pAVFilterContext; const filt: pAVFilter; const name: pAnsiChar; const args: pAnsiChar; opaque: pointer;
  graph_ctx: pAVFilterGraph): int; cdecl; external avfilter_dll;
(* *
  * Enable or disable automatic format conversion inside the graph.
  *
  * Note that format conversion can still happen inside explicitly inserted
  * scale and aresample filters.
  *
  * @param flags  any of the AVFILTER_AUTO_CONVERT_* constants
*)
// void avfilter_graph_set_auto_convert(AVFilterGraph *graph, unsigned flags);
procedure avfilter_graph_set_auto_convert(graph: pAVFilterGraph; flags: unsigned); cdecl; external avfilter_dll;

const

  AVFILTER_AUTO_CONVERT_ALL  = 0;  (* *< all automatic conversions enabled *)
  AVFILTER_AUTO_CONVERT_NONE = -1; (* *< all automatic conversions disabled *)

  (* *
    * Check validity and configure all the links and formats in the graph.
    *
    * @param graphctx the filter graph
    * @param log_ctx context used for logging
    * @return >= 0 in case of success, a negative AVERROR code otherwise
  *)
  // int avfilter_graph_config(AVFilterGraph *graphctx, void *log_ctx);
function avfilter_graph_config(graphctx: pAVFilterGraph; log_ctx: pointer): int; cdecl; external avfilter_dll;
(* *
  * Free a graph, destroy its links, and set *graph to NULL.
  * If *graph is NULL, do nothing.
*)
// void avfilter_graph_free(AVFilterGraph **graph);
procedure avfilter_graph_free(var graph: pAVFilterGraph); cdecl; external avfilter_dll;

type
  (* *
    * A linked-list of the inputs/outputs of the filter chain.
    *
    * This is mainly useful for avfilter_graph_parse() / avfilter_graph_parse2(),
    * where it is used to communicate open (unlinked) inputs and outputs from and
    * to the caller.
    * This struct specifies, per each not connected pad contained in the graph, the
    * filter context and the pad index required for establishing a link.
  *)
  pAVFilterInOut = ^AVFilterInOut;

  AVFilterInOut = record
    (* * unique name for this input/output in the list *)
    name: pAnsiChar;

    (* * filter context associated to this input/output *)
    filter_ctx: pAVFilterContext;

    (* * index of the filt_ctx pad to use for linking *)
    pad_idx: int;

    (* * next input/input in the list, NULL if this is the last *)
    next: pAVFilterInOut;
  end;

  (* *
    * Allocate a single AVFilterInOut entry.
    * Must be freed with avfilter_inout_free().
    * @return allocated AVFilterInOut on success, NULL on failure.
  *)
  // AVFilterInOut *avfilter_inout_alloc(void);
function avfilter_inout_alloc(): pAVFilterInOut; cdecl; external avfilter_dll;
(* *
  * Free the supplied list of AVFilterInOut and set *inout to NULL.
  * If *inout is NULL, do nothing.
*)
// void avfilter_inout_free(AVFilterInOut **inout);
procedure avfilter_inout_free(var inout: pAVFilterInOut); cdecl; external avfilter_dll;
(* *
  * Add a graph described by a string to a graph.
  *
  * @note The caller must provide the lists of inputs and outputs,
  * which therefore must be known before calling the function.
  *
  * @note The inputs parameter describes inputs of the already existing
  * part of the graph; i.e. from the point of view of the newly created
  * part, they are outputs. Similarly the outputs parameter describes
  * outputs of the already existing filters, which are provided as
  * inputs to the parsed filters.
  *
  * @param graph   the filter graph where to link the parsed graph context
  * @param filters string to be parsed
  * @param inputs  linked list to the inputs of the graph
  * @param outputs linked list to the outputs of the graph
  * @return zero on success, a negative AVERROR code on error
*)
// int avfilter_graph_parse(AVFilterGraph *graph, const char *filters,
// AVFilterInOut *inputs, AVFilterInOut *outputs,
// void *log_ctx);
function avfilter_graph_parse(graph: pAVFilterGraph; const filters: pAnsiChar; inputs: pAVFilterInOut; outputs: pAVFilterInOut; log_ctx: pointer): int; cdecl;
  external avfilter_dll;
(* *
  * Add a graph described by a string to a graph.
  *
  * In the graph filters description, if the input label of the first
  * filter is not specified, "in" is assumed; if the output label of
  * the last filter is not specified, "out" is assumed.
  *
  * @param graph   the filter graph where to link the parsed graph context
  * @param filters string to be parsed
  * @param inputs  pointer to a linked list to the inputs of the graph, may be NULL.
  *                If non-NULL, *inputs is updated to contain the list of open inputs
  *                after the parsing, should be freed with avfilter_inout_free().
  * @param outputs pointer to a linked list to the outputs of the graph, may be NULL.
  *                If non-NULL, *outputs is updated to contain the list of open outputs
  *                after the parsing, should be freed with avfilter_inout_free().
  * @return non negative on success, a negative AVERROR code on error
*)
// int avfilter_graph_parse_ptr(AVFilterGraph *graph, const char *filters,
// AVFilterInOut **inputs, AVFilterInOut **outputs,
// void *log_ctx);
function avfilter_graph_parse_ptr(graph: pAVFilterGraph; const filters: pAnsiChar; var inputs: pAVFilterInOut; var outputs: pAVFilterInOut; log_ctx: pointer)
  : int; cdecl; external avfilter_dll;
(* *
  * Add a graph described by a string to a graph.
  *
  * @param[in]  graph   the filter graph where to link the parsed graph context
  * @param[in]  filters string to be parsed
  * @param[out] inputs  a linked list of all free (unlinked) inputs of the
  *                     parsed graph will be returned here. It is to be freed
  *                     by the caller using avfilter_inout_free().
  * @param[out] outputs a linked list of all free (unlinked) outputs of the
  *                     parsed graph will be returned here. It is to be freed by the
  *                     caller using avfilter_inout_free().
  * @return zero on success, a negative AVERROR code on error
  *
  * @note This function returns the inputs and outputs that are left
  * unlinked after parsing the graph and the caller then deals with
  * them.
  * @note This function makes no reference whatsoever to already
  * existing parts of the graph and the inputs parameter will on return
  * contain inputs of the newly parsed part of the graph.  Analogously
  * the outputs parameter will contain outputs of the newly created
  * filters.
*)
// int avfilter_graph_parse2(AVFilterGraph *graph, const char *filters,
// AVFilterInOut **inputs,
// AVFilterInOut **outputs);
function avfilter_graph_parse2(graph: pAVFilterGraph; const filters: pAnsiChar; var inputs: pAVFilterInOut; var outputs: pAVFilterInOut): int; cdecl;
  external avfilter_dll;
(* *
  * Send a command to one or more filter instances.
  *
  * @param graph  the filter graph
  * @param target the filter(s) to which the command should be sent
  *               "all" sends to all filters
  *               otherwise it can be a filter or filter instance name
  *               which will send the command to all matching filters.
  * @param cmd    the command to send, for handling simplicity all commands must be alphanumeric only
  * @param arg    the argument for the command
  * @param res    a buffer with size res_size where the filter(s) can return a response.
  *
  * @returns >=0 on success otherwise an error code.
  *              AVERROR(ENOSYS) on unsupported commands
*)
// int avfilter_graph_send_command(AVFilterGraph *graph, const char *target, const char *cmd, const char *arg, char *res, int res_len, int flags);
function avfilter_graph_send_command(graph: pAVFilterGraph; const target: pAnsiChar; const cmd: pAnsiChar; const arg: pAnsiChar; res: pAnsiChar; res_len: int;
  flags: int): int; cdecl; external avfilter_dll;
(* *
  * Queue a command for one or more filter instances.
  *
  * @param graph  the filter graph
  * @param target the filter(s) to which the command should be sent
  *               "all" sends to all filters
  *               otherwise it can be a filter or filter instance name
  *               which will send the command to all matching filters.
  * @param cmd    the command to sent, for handling simplicity all commands must be alphanumeric only
  * @param arg    the argument for the command
  * @param ts     time at which the command should be sent to the filter
  *
  * @note As this executes commands after this function returns, no return code
  *       from the filter is provided, also AVFILTER_CMD_FLAG_ONE is not supported.
*)
// int avfilter_graph_queue_command(AVFilterGraph *graph, const char *target, const char *cmd, const char *arg, int flags, double ts);
function avfilter_graph_queue_command(graph: pAVFilterGraph; const target: pAnsiChar; const cmd: pAnsiChar; const arg: pAnsiChar; flags: int; ts: double): int;
  cdecl; external avfilter_dll;
(* *
  * Dump a graph into a human-readable string representation.
  *
  * @param graph    the graph to dump
  * @param options  formatting options; currently ignored
  * @return  a string, or NULL in case of memory allocation failure;
  *          the string must be freed using av_free
*)
// char *avfilter_graph_dump(AVFilterGraph *graph, const char *options);
function avfilter_graph_dump(graph: pAVFilterGraph; const options: pAnsiChar): pAnsiChar; cdecl; external avfilter_dll;
(* *
  * Request a frame on the oldest sink link.
  *
  * If the request returns AVERROR_EOF, try the next.
  *
  * Note that this function is not meant to be the sole scheduling mechanism
  * of a filtergraph, only a convenience function to help drain a filtergraph
  * in a balanced way under normal circumstances.
  *
  * Also note that AVERROR_EOF does not mean that frames did not arrive on
  * some of the sinks during the process.
  * When there are multiple sink links, in case the requested link
  * returns an EOF, this may cause a filter to flush pending frames
  * which are sent to another sink link, although unrequested.
  *
  * @return  the return value of ff_request_frame(),
  *          or AVERROR_EOF if all links returned AVERROR_EOF
*)
// int avfilter_graph_request_oldest(AVFilterGraph *graph);
function avfilter_graph_request_oldest(graph: pAVFilterGraph): int; cdecl; external avfilter_dll;
{$ENDREGION}
{$REGION 'buffersink.h'}
(* *
  * Get a frame with filtered data from sink and put it in frame.
  *
  * @param ctx    pointer to a buffersink or abuffersink filter context.
  * @param frame  pointer to an allocated frame that will be filled with data.
  *               The data must be freed using av_frame_unref() / av_frame_free()
  * @param flags  a combination of AV_BUFFERSINK_FLAG_* flags
  *
  * @return  >= 0 in for success, a negative AVERROR code for failure.
*)
// int av_buffersink_get_frame_flags(AVFilterContext *ctx, AVFrame *frame, int flags);
function av_buffersink_get_frame_flags(ctx: pAVFilterContext; frame: pAVFrame; flags: int): int; cdecl; external avfilter_dll;

const
  (* *
    * Tell av_buffersink_get_buffer_ref() to read video/samples buffer
    * reference, but not remove it from the buffer. This is useful if you
    * need only to read a video/samples buffer, without to fetch it.
  *)
  AV_BUFFERSINK_FLAG_PEEK = 1;

  (* *
    * Tell av_buffersink_get_buffer_ref() not to request a frame from its input.
    * If a frame is already buffered, it is read (and removed from the buffer),
    * but if no frame is present, return AVERROR(EAGAIN).
  *)
  AV_BUFFERSINK_FLAG_NO_REQUEST = 2;

type
  (* *
    * Struct to use for initializing a buffersink context.
  *)
  pAVBufferSinkParams = ^AVBufferSinkParams;

  AVBufferSinkParams = record
    pixel_fmts: pAVPixelFormat;
    /// < list of allowed pixel formats, terminated by AV_PIX_FMT_NONE
  end;

  (* *
    * Create an AVBufferSinkParams structure.
    *
    * Must be freed with av_free().
  *)
  // AVBufferSinkParams *av_buffersink_params_alloc(void);
function av_buffersink_params_alloc(): pAVBufferSinkParams; cdecl; external avfilter_dll;

type
  (* *
    * Struct to use for initializing an abuffersink context.
  *)
  pAVABufferSinkParams = ^AVABufferSinkParams;

  AVABufferSinkParams = record
    sample_fmts: pAVSampleFormat;
    /// < list of allowed sample formats, terminated by AV_SAMPLE_FMT_NONE
    channel_layouts: pint64_t;
    /// < list of allowed channel layouts, terminated by -1
    channel_counts: pint;
    /// < list of allowed channel counts, terminated by -1
    all_channel_counts: int;
    /// < if not 0, accept any channel count or layout
    sample_rates: pint;
    /// < list of allowed sample rates, terminated by -1
  end;

  (* *
    * Create an AVABufferSinkParams structure.
    *
    * Must be freed with av_free().
  *)
  // AVABufferSinkParams *av_abuffersink_params_alloc(void);
function av_abuffersink_params_alloc(): pAVABufferSinkParams; cdecl; external avfilter_dll;
(* *
  * Set the frame size for an audio buffer sink.
  *
  * All calls to av_buffersink_get_buffer_ref will return a buffer with
  * exactly the specified number of samples, or AVERROR(EAGAIN) if there is
  * not enough. The last buffer at EOF will be padded with 0.
*)
// void av_buffersink_set_frame_size(AVFilterContext *ctx, unsigned frame_size);
procedure av_buffersink_set_frame_size(ctx: pAVFilterContext; frame_size: unsigned); cdecl; external avfilter_dll;
(* *
  * @defgroup lavfi_buffersink_accessors Buffer sink accessors
  * Get the properties of the stream
  * @{
*)

// enum AVMediaType av_buffersink_get_type                (const AVFilterContext *ctx);
function av_buffersink_get_type(const ctx: pAVFilterContext): AVMediaType; cdecl; external avfilter_dll;

// AVRational       av_buffersink_get_time_base           (const AVFilterContext *ctx);
function av_buffersink_get_time_base(const ctx: pAVFilterContext): AVRational; cdecl; external avfilter_dll;

// int av_buffersink_get_format              (const AVFilterContext *ctx);
function av_buffersink_get_format(const ctx: pAVFilterContext): int; cdecl; external avfilter_dll;
// AVRational       av_buffersink_get_frame_rate          (const AVFilterContext *ctx);
function av_buffersink_get_frame_rate(const ctx: pAVFilterContext): AVRational; cdecl; external avfilter_dll;

// int              av_buffersink_get_w                   (const AVFilterContext *ctx);
function av_buffersink_get_w(const ctx: pAVFilterContext): int; cdecl; external avfilter_dll;

// int              av_buffersink_get_h                   (const AVFilterContext *ctx);
function av_buffersink_get_h(const ctx: pAVFilterContext): int; cdecl; external avfilter_dll;

// AVRational       av_buffersink_get_sample_aspect_ratio (const AVFilterContext *ctx);
function av_buffersink_get_sample_aspect_ratio(const ctx: pAVFilterContext): AVRational; cdecl; external avfilter_dll;

// int              av_buffersink_get_channels            (const AVFilterContext *ctx);
function av_buffersink_get_channels(const ctx: pAVFilterContext): int; cdecl; external avfilter_dll;

// uint64_t         av_buffersink_get_channel_layout      (const AVFilterContext *ctx);
function av_buffersink_get_channel_layout(const ctx: pAVFilterContext): uint64_t; cdecl; external avfilter_dll;

// int              av_buffersink_get_sample_rate         (const AVFilterContext *ctx);
function av_buffersink_get_sample_rate(const ctx: pAVFilterContext): int; cdecl; external avfilter_dll;

// AVBufferRef *    av_buffersink_get_hw_frames_ctx       (const AVFilterContext *ctx);
function av_buffersink_get_hw_frames_ctx(const ctx: pAVFilterContext): pAVBufferRef; cdecl; external avfilter_dll;
(* * @} *)

(* *
  * Get a frame with filtered data from sink and put it in frame.
  *
  * @param ctx pointer to a context of a buffersink or abuffersink AVFilter.
  * @param frame pointer to an allocated frame that will be filled with data.
  *              The data must be freed using av_frame_unref() / av_frame_free()
  *
  * @return
  *         - >= 0 if a frame was successfully returned.
  *         - AVERROR(EAGAIN) if no frames are available at this point; more
  *           input frames must be added to the filtergraph to get more output.
  *         - AVERROR_EOF if there will be no more output frames on this sink.
  *         - A different negative AVERROR code in other failure cases.
*)
// int av_buffersink_get_frame(AVFilterContext *ctx, AVFrame *frame);
function av_buffersink_get_frame(ctx: pAVFilterContext; frame: pAVFrame): int; cdecl; external avfilter_dll;
(* *
  * Same as av_buffersink_get_frame(), but with the ability to specify the number
  * of samples read. This function is less efficient than
  * av_buffersink_get_frame(), because it copies the data around.
  *
  * @param ctx pointer to a context of the abuffersink AVFilter.
  * @param frame pointer to an allocated frame that will be filled with data.
  *              The data must be freed using av_frame_unref() / av_frame_free()
  *              frame will contain exactly nb_samples audio samples, except at
  *              the end of stream, when it can contain less than nb_samples.
  *
  * @return The return codes have the same meaning as for
  *         av_buffersink_get_samples().
  *
  * @warning do not mix this function with av_buffersink_get_frame(). Use only one or
  * the other with a single sink, not both.
*)
// int av_buffersink_get_samples(AVFilterContext *ctx, AVFrame *frame, int nb_samples);
function av_buffersink_get_samples(ctx: pAVFilterContext; frame: pAVFrame; nb_samples: int): int; cdecl; external avfilter_dll;
{$ENDREGION}
{$REGION 'buffersrc.h'}

const

  (* *
    * Do not check for format changes.
  *)
  AV_BUFFERSRC_FLAG_NO_CHECK_FORMAT = 1;

  (* *
    * Immediately push the frame to the output.
  *)
  AV_BUFFERSRC_FLAG_PUSH = 4;

  (* *
    * Keep a reference to the frame.
    * If the frame if reference-counted, create a new reference; otherwise
    * copy the frame data.
  *)
  AV_BUFFERSRC_FLAG_KEEP_REF = 8;

  (* *
    * Get the number of failed requests.
    *
    * A failed request is when the request_frame method is called while no
    * frame is present in the buffer.
    * The number is reset when a frame is added.
  *)
  // unsigned av_buffersrc_get_nb_failed_requests(AVFilterContext *buffer_src);
function av_buffersrc_get_nb_failed_requests(buffer_src: pAVFilterContext): unsigned; cdecl; external avfilter_dll;

type
  (* *
    * This structure contains the parameters describing the frames that will be
    * passed to this filter.
    *
    * It should be allocated with av_buffersrc_parameters_alloc() and freed with
    * av_free(). All the allocated fields in it remain owned by the caller.
  *)
  pAVBufferSrcParameters = ^AVBufferSrcParameters;

  AVBufferSrcParameters = record
    (* *
      * video: the pixel format, value corresponds to enum AVPixelFormat
      * audio: the sample format, value corresponds to enum AVSampleFormat
    *)
    format: int;
    (* *
      * The timebase to be used for the timestamps on the input frames.
    *)
    time_base: AVRational;

    (* *
      * Video only, the display dimensions of the input frames.
    *)
    width, height: int;

    (* *
      * Video only, the sample (pixel) aspect ratio.
    *)
    sample_aspect_ratio: AVRational;

    (* *
      * Video only, the frame rate of the input video. This field must only be
      * set to a non-zero value if input stream has a known constant framerate
      * and should be left at its initial value if the framerate is variable or
      * unknown.
    *)
    frame_rate: AVRational;

    (* *
      * Video with a hwaccel pixel format only. This should be a reference to an
      * AVHWFramesContext instance describing the input frames.
    *)
    hw_frames_ctx: pAVBufferRef;

    (* *
      * Audio only, the audio sampling rate in samples per secon.
    *)
    sample_rate: int;

    (* *
      * Audio only, the audio channel layout
    *)
    channel_layout: uint64_t;
  end;

  (* *
    * Allocate a new AVBufferSrcParameters instance. It should be freed by the
    * caller with av_free().
  *)
  // AVBufferSrcParameters *av_buffersrc_parameters_alloc(void);
function av_buffersrc_parameters_alloc(): pAVBufferSrcParameters; cdecl; external avfilter_dll;
(* *
  * Initialize the buffersrc or abuffersrc filter with the provided parameters.
  * This function may be called multiple times, the later calls override the
  * previous ones. Some of the parameters may also be set through AVOptions, then
  * whatever method is used last takes precedence.
  *
  * @param ctx an instance of the buffersrc or abuffersrc filter
  * @param param the stream parameters. The frames later passed to this filter
  *              must conform to those parameters. All the allocated fields in
  *              param remain owned by the caller, libavfilter will make internal
  *              copies or references when necessary.
  * @return 0 on success, a negative AVERROR code on failure.
*)
// int av_buffersrc_parameters_set(AVFilterContext *ctx, AVBufferSrcParameters *param);
function av_buffersrc_parameters_set(ctx: pAVFilterContext; param: pAVBufferSrcParameters): int; cdecl; external avfilter_dll;
(* *
  * Add a frame to the buffer source.
  *
  * @param ctx   an instance of the buffersrc filter
  * @param frame frame to be added. If the frame is reference counted, this
  * function will make a new reference to it. Otherwise the frame data will be
  * copied.
  *
  * @return 0 on success, a negative AVERROR on error
  *
  * This function is equivalent to av_buffersrc_add_frame_flags() with the
  * AV_BUFFERSRC_FLAG_KEEP_REF flag.
*)
// av_warn_unused_result
// int av_buffersrc_write_frame(AVFilterContext *ctx, const AVFrame *frame);
function av_buffersrc_write_frame(ctx: pAVFilterContext; const frame: pAVFrame): int; cdecl; external avfilter_dll;
(* *
  * Add a frame to the buffer source.
  *
  * @param ctx   an instance of the buffersrc filter
  * @param frame frame to be added. If the frame is reference counted, this
  * function will take ownership of the reference(s) and reset the frame.
  * Otherwise the frame data will be copied. If this function returns an error,
  * the input frame is not touched.
  *
  * @return 0 on success, a negative AVERROR on error.
  *
  * @note the difference between this function and av_buffersrc_write_frame() is
  * that av_buffersrc_write_frame() creates a new reference to the input frame,
  * while this function takes ownership of the reference passed to it.
  *
  * This function is equivalent to av_buffersrc_add_frame_flags() without the
  * AV_BUFFERSRC_FLAG_KEEP_REF flag.
*)
// av_warn_unused_result
// int av_buffersrc_add_frame(AVFilterContext *ctx, AVFrame *frame);
function av_buffersrc_add_frame(ctx: pAVFilterContext; frame: pAVFrame): int; cdecl; external avfilter_dll;
(* *
  * Add a frame to the buffer source.
  *
  * By default, if the frame is reference-counted, this function will take
  * ownership of the reference(s) and reset the frame. This can be controlled
  * using the flags.
  *
  * If this function returns an error, the input frame is not touched.
  *
  * @param buffer_src  pointer to a buffer source context
  * @param frame       a frame, or NULL to mark EOF
  * @param flags       a combination of AV_BUFFERSRC_FLAG_*
  * @return            >= 0 in case of success, a negative AVERROR code
  *                    in case of failure
*)
// av_warn_unused_result
// int av_buffersrc_add_frame_flags(AVFilterContext *buffer_src,
// AVFrame *frame, int flags);
function av_buffersrc_add_frame_flags(buffer_src: pAVFilterContext; frame: pAVFrame; flags: int): int; cdecl; external avfilter_dll;
(* *
  * Close the buffer source after EOF.
  *
  * This is similar to passing NULL to av_buffersrc_add_frame_flags()
  * except it takes the timestamp of the EOF, i.e. the timestamp of the end
  * of the last frame.
*)
// int av_buffersrc_close(AVFilterContext *ctx, int64_t pts, unsigned flags);
function av_buffersrc_close(ctx: pAVFilterContext; pts: int64_t; flags: unsigned): int; cdecl; external avfilter_dll;
{$ENDREGION}

implementation

end.
