unit libavcodec;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

Uses
  ffmpeg_types, libavutil;

{$I ffmpeg.inc}
{$REGION 'avcodec.h'}

(*
  * Identify the syntax and semantics of the bitstream.
  * The principle is roughly:
  * Two decoders with the same ID can decode the same streams.
  * Two encoders with the same ID can encode compatible streams.
  * There may be slight deviations from the principle due to implementation
  * details.
  *
  * If you add a codec ID to this list, add it so that
  * 1. no value of an existing codec ID changes (that would break ABI),
  * 2. it is as close as possible to similar codecs
  *
  * After adding new codec IDs, do not forget to add an entry to the codec
  * descriptor list and bump libavcodec minor version.
*)
type
  pAVCodecID = ^AVCodecID;
  AVCodecID = ( //
    AV_CODEC_ID_NONE,
    (* video codecs *)
    AV_CODEC_ID_MPEG1VIDEO,      //
    AV_CODEC_ID_MPEG2VIDEO,      // < preferred ID for MPEG-1/2 video decoding
    AV_CODEC_ID_H261,            //
    AV_CODEC_ID_H263,            //
    AV_CODEC_ID_RV10,            //
    AV_CODEC_ID_RV20,            //
    AV_CODEC_ID_MJPEG,           //
    AV_CODEC_ID_MJPEGB,          //
    AV_CODEC_ID_LJPEG,           //
    AV_CODEC_ID_SP5X,            //
    AV_CODEC_ID_JPEGLS,          //
    AV_CODEC_ID_MPEG4,           //
    AV_CODEC_ID_RAWVIDEO,        //
    AV_CODEC_ID_MSMPEG4V1,       //
    AV_CODEC_ID_MSMPEG4V2,       //
    AV_CODEC_ID_MSMPEG4V3,       //
    AV_CODEC_ID_WMV1,            //
    AV_CODEC_ID_WMV2,            //
    AV_CODEC_ID_H263P,           //
    AV_CODEC_ID_H263I,           //
    AV_CODEC_ID_FLV1,            //
    AV_CODEC_ID_SVQ1,            //
    AV_CODEC_ID_SVQ3,            //
    AV_CODEC_ID_DVVIDEO,         //
    AV_CODEC_ID_HUFFYUV,         //
    AV_CODEC_ID_CYUV,            //
    AV_CODEC_ID_H264,            //
    AV_CODEC_ID_INDEO3,          //
    AV_CODEC_ID_VP3,             //
    AV_CODEC_ID_THEORA,          //
    AV_CODEC_ID_ASV1,            //
    AV_CODEC_ID_ASV2,            //
    AV_CODEC_ID_FFV1,            //
    AV_CODEC_ID_4XM,             //
    AV_CODEC_ID_VCR1,            //
    AV_CODEC_ID_CLJR,            //
    AV_CODEC_ID_MDEC,            //
    AV_CODEC_ID_ROQ,             //
    AV_CODEC_ID_INTERPLAY_VIDEO, //
    AV_CODEC_ID_XAN_WC3,         //
    AV_CODEC_ID_XAN_WC4,         //
    AV_CODEC_ID_RPZA,            //
    AV_CODEC_ID_CINEPAK,         //
    AV_CODEC_ID_WS_VQA,          //
    AV_CODEC_ID_MSRLE,           //
    AV_CODEC_ID_MSVIDEO1,        //
    AV_CODEC_ID_IDCIN,           //
    AV_CODEC_ID_8BPS,            //
    AV_CODEC_ID_SMC,             //
    AV_CODEC_ID_FLIC,            //
    AV_CODEC_ID_TRUEMOTION1,     //
    AV_CODEC_ID_VMDVIDEO,        //
    AV_CODEC_ID_MSZH,            //
    AV_CODEC_ID_ZLIB,            //
    AV_CODEC_ID_QTRLE,           //
    AV_CODEC_ID_TSCC,            //
    AV_CODEC_ID_ULTI,            //
    AV_CODEC_ID_QDRAW,           //
    AV_CODEC_ID_VIXL,            //
    AV_CODEC_ID_QPEG,            //
    AV_CODEC_ID_PNG,             //
    AV_CODEC_ID_PPM,             //
    AV_CODEC_ID_PBM,             //
    AV_CODEC_ID_PGM,             //
    AV_CODEC_ID_PGMYUV,          //
    AV_CODEC_ID_PAM,             //
    AV_CODEC_ID_FFVHUFF,         //
    AV_CODEC_ID_RV30,            //
    AV_CODEC_ID_RV40,            //
    AV_CODEC_ID_VC1,             //
    AV_CODEC_ID_WMV3,            //
    AV_CODEC_ID_LOCO,            //
    AV_CODEC_ID_WNV1,            //
    AV_CODEC_ID_AASC,            //
    AV_CODEC_ID_INDEO2,          //
    AV_CODEC_ID_FRAPS,           //
    AV_CODEC_ID_TRUEMOTION2,     //
    AV_CODEC_ID_BMP,             //
    AV_CODEC_ID_CSCD,            //
    AV_CODEC_ID_MMVIDEO,         //
    AV_CODEC_ID_ZMBV,            //
    AV_CODEC_ID_AVS,             //
    AV_CODEC_ID_SMACKVIDEO,      //
    AV_CODEC_ID_NUV,             //
    AV_CODEC_ID_KMVC,            //
    AV_CODEC_ID_FLASHSV,         //
    AV_CODEC_ID_CAVS,            //
    AV_CODEC_ID_JPEG2000,        //
    AV_CODEC_ID_VMNC,            //
    AV_CODEC_ID_VP5,             //
    AV_CODEC_ID_VP6,             //
    AV_CODEC_ID_VP6F,            //
    AV_CODEC_ID_TARGA,           //
    AV_CODEC_ID_DSICINVIDEO,     //
    AV_CODEC_ID_TIERTEXSEQVIDEO, //
    AV_CODEC_ID_TIFF,            //
    AV_CODEC_ID_GIF,             //
    AV_CODEC_ID_DXA,             //
    AV_CODEC_ID_DNXHD,           //
    AV_CODEC_ID_THP,             //
    AV_CODEC_ID_SGI,             //
    AV_CODEC_ID_C93,             //
    AV_CODEC_ID_BETHSOFTVID,     //
    AV_CODEC_ID_PTX,             //
    AV_CODEC_ID_TXD,             //
    AV_CODEC_ID_VP6A,            //
    AV_CODEC_ID_AMV,             //
    AV_CODEC_ID_VB,              //
    AV_CODEC_ID_PCX,             //
    AV_CODEC_ID_SUNRAST,         //
    AV_CODEC_ID_INDEO4,          //
    AV_CODEC_ID_INDEO5,          //
    AV_CODEC_ID_MIMIC,           //
    AV_CODEC_ID_RL2,             //
    AV_CODEC_ID_ESCAPE124,       //
    AV_CODEC_ID_DIRAC,           //
    AV_CODEC_ID_BFI,             //
    AV_CODEC_ID_CMV,             //
    AV_CODEC_ID_MOTIONPIXELS,    //
    AV_CODEC_ID_TGV,             //
    AV_CODEC_ID_TGQ,             //
    AV_CODEC_ID_TQI,             //
    AV_CODEC_ID_AURA,            //
    AV_CODEC_ID_AURA2,           //
    AV_CODEC_ID_V210X,           //
    AV_CODEC_ID_TMV,             //
    AV_CODEC_ID_V210,            //
    AV_CODEC_ID_DPX,             //
    AV_CODEC_ID_MAD,             //
    AV_CODEC_ID_FRWU,            //
    AV_CODEC_ID_FLASHSV2,        //
    AV_CODEC_ID_CDGRAPHICS,      //
    AV_CODEC_ID_R210,            //
    AV_CODEC_ID_ANM,             //
    AV_CODEC_ID_BINKVIDEO,       //
    AV_CODEC_ID_IFF_ILBM,        //
    // AV_CODEC_ID_IFF_BYTERUN1 = AV_CODEC_ID_IFF_ILBM, //
    AV_CODEC_ID_KGV1,       //
    AV_CODEC_ID_YOP,        //
    AV_CODEC_ID_VP8,        //
    AV_CODEC_ID_PICTOR,     //
    AV_CODEC_ID_ANSI,       //
    AV_CODEC_ID_A64_MULTI,  //
    AV_CODEC_ID_A64_MULTI5, //
    AV_CODEC_ID_R10K,       //
    AV_CODEC_ID_MXPEG,      //
    AV_CODEC_ID_LAGARITH,   //
    AV_CODEC_ID_PRORES,     //
    AV_CODEC_ID_JV,         //
    AV_CODEC_ID_DFA,        //
    AV_CODEC_ID_WMV3IMAGE,  //
    AV_CODEC_ID_VC1IMAGE,   //
    AV_CODEC_ID_UTVIDEO,    //
    AV_CODEC_ID_BMV_VIDEO,  //
    AV_CODEC_ID_VBLE,       //
    AV_CODEC_ID_DXTORY,     //
    AV_CODEC_ID_V410,       //
    AV_CODEC_ID_XWD,        //
    AV_CODEC_ID_CDXL,       //
    AV_CODEC_ID_XBM,        //
    AV_CODEC_ID_ZEROCODEC,  //
    AV_CODEC_ID_MSS1,       //
    AV_CODEC_ID_MSA1,       //
    AV_CODEC_ID_TSCC2,      //
    AV_CODEC_ID_MTS2,       //
    AV_CODEC_ID_CLLC,       //
    AV_CODEC_ID_MSS2,       //
    AV_CODEC_ID_VP9,        //
    AV_CODEC_ID_AIC,        //
    AV_CODEC_ID_ESCAPE130,  //
    AV_CODEC_ID_G2M,        //
    AV_CODEC_ID_WEBP,       //
    AV_CODEC_ID_HNM4_VIDEO, //
    AV_CODEC_ID_HEVC,       //
    // AV_CODEC_ID_H265 = AV_CODEC_ID_HEVC, //
    AV_CODEC_ID_FIC,          //
    AV_CODEC_ID_ALIAS_PIX,    //
    AV_CODEC_ID_BRENDER_PIX,  //
    AV_CODEC_ID_PAF_VIDEO,    //
    AV_CODEC_ID_EXR,          //
    AV_CODEC_ID_VP7,          //
    AV_CODEC_ID_SANM,         //
    AV_CODEC_ID_SGIRLE,       //
    AV_CODEC_ID_MVC1,         //
    AV_CODEC_ID_MVC2,         //
    AV_CODEC_ID_HQX,          //
    AV_CODEC_ID_TDSC,         //
    AV_CODEC_ID_HQ_HQA,       //
    AV_CODEC_ID_HAP,          //
    AV_CODEC_ID_DDS,          //
    AV_CODEC_ID_DXV,          //
    AV_CODEC_ID_SCREENPRESSO, //
    AV_CODEC_ID_RSCC,         //
    AV_CODEC_ID_AVS2,         //

    AV_CODEC_ID_Y41P = $8000,  //
    AV_CODEC_ID_AVRP,          //
    AV_CODEC_ID_012V,          //
    AV_CODEC_ID_AVUI,          //
    AV_CODEC_ID_AYUV,          //
    AV_CODEC_ID_TARGA_Y216,    //
    AV_CODEC_ID_V308,          //
    AV_CODEC_ID_V408,          //
    AV_CODEC_ID_YUV4,          //
    AV_CODEC_ID_AVRN,          //
    AV_CODEC_ID_CPIA,          //
    AV_CODEC_ID_XFACE,         //
    AV_CODEC_ID_SNOW,          //
    AV_CODEC_ID_SMVJPEG,       //
    AV_CODEC_ID_APNG,          //
    AV_CODEC_ID_DAALA,         //
    AV_CODEC_ID_CFHD,          //
    AV_CODEC_ID_TRUEMOTION2RT, //
    AV_CODEC_ID_M101,          //
    AV_CODEC_ID_MAGICYUV,      //
    AV_CODEC_ID_SHEERVIDEO,    //
    AV_CODEC_ID_YLC,           //
    AV_CODEC_ID_PSD,           //
    AV_CODEC_ID_PIXLET,        //
    AV_CODEC_ID_SPEEDHQ,       //
    AV_CODEC_ID_FMVC,          //
    AV_CODEC_ID_SCPR,          //
    AV_CODEC_ID_CLEARVIDEO,    //
    AV_CODEC_ID_XPM,           //
    AV_CODEC_ID_AV1,           //
    AV_CODEC_ID_BITPACKED,     //
    AV_CODEC_ID_MSCC,          //
    AV_CODEC_ID_SRGC,          //
    AV_CODEC_ID_SVG,           //
    AV_CODEC_ID_GDV,           //
    AV_CODEC_ID_FITS,          //
    AV_CODEC_ID_IMM4,          //
    AV_CODEC_ID_PROSUMER,      //
    AV_CODEC_ID_MWSC,          //
    AV_CODEC_ID_WCMV,          //
    AV_CODEC_ID_RASC,          //
    AV_CODEC_ID_HYMT,          //
    AV_CODEC_ID_ARBC,          //
    AV_CODEC_ID_AGM,           //
    AV_CODEC_ID_LSCR,          //
    AV_CODEC_ID_VP4,           //
//    AV_CODEC_ID_IMM5,          // 4.2.2

    (* various PCM "codecs" *)
    AV_CODEC_ID_FIRST_AUDIO = $10000, // < A dummy id pointing at the start of audio codecs
    AV_CODEC_ID_PCM_S16LE = $10000,   //
    AV_CODEC_ID_PCM_S16BE,            //
    AV_CODEC_ID_PCM_U16LE,            //
    AV_CODEC_ID_PCM_U16BE,            //
    AV_CODEC_ID_PCM_S8,               //
    AV_CODEC_ID_PCM_U8,               //
    AV_CODEC_ID_PCM_MULAW,            //
    AV_CODEC_ID_PCM_ALAW,             //
    AV_CODEC_ID_PCM_S32LE,            //
    AV_CODEC_ID_PCM_S32BE,            //
    AV_CODEC_ID_PCM_U32LE,            //
    AV_CODEC_ID_PCM_U32BE,            //
    AV_CODEC_ID_PCM_S24LE,            //
    AV_CODEC_ID_PCM_S24BE,            //
    AV_CODEC_ID_PCM_U24LE,            //
    AV_CODEC_ID_PCM_U24BE,            //
    AV_CODEC_ID_PCM_S24DAUD,          //
    AV_CODEC_ID_PCM_ZORK,             //
    AV_CODEC_ID_PCM_S16LE_PLANAR,     //
    AV_CODEC_ID_PCM_DVD,              //
    AV_CODEC_ID_PCM_F32BE,            //
    AV_CODEC_ID_PCM_F32LE,            //
    AV_CODEC_ID_PCM_F64BE,            //
    AV_CODEC_ID_PCM_F64LE,            //
    AV_CODEC_ID_PCM_BLURAY,           //
    AV_CODEC_ID_PCM_LXF,              //
    AV_CODEC_ID_S302M,                //
    AV_CODEC_ID_PCM_S8_PLANAR,        //
    AV_CODEC_ID_PCM_S24LE_PLANAR,     //
    AV_CODEC_ID_PCM_S32LE_PLANAR,     //
    AV_CODEC_ID_PCM_S16BE_PLANAR,     //

    AV_CODEC_ID_PCM_S64LE = $10800, //
    AV_CODEC_ID_PCM_S64BE,          //
    AV_CODEC_ID_PCM_F16LE,          //
    AV_CODEC_ID_PCM_F24LE,          //
    AV_CODEC_ID_PCM_VIDC,           //

    (* various ADPCM codecs *)
    AV_CODEC_ID_ADPCM_IMA_QT = $11000, //
    AV_CODEC_ID_ADPCM_IMA_WAV,         //
    AV_CODEC_ID_ADPCM_IMA_DK3,         //
    AV_CODEC_ID_ADPCM_IMA_DK4,         //
    AV_CODEC_ID_ADPCM_IMA_WS,          //
    AV_CODEC_ID_ADPCM_IMA_SMJPEG,      //
    AV_CODEC_ID_ADPCM_MS,              //
    AV_CODEC_ID_ADPCM_4XM,             //
    AV_CODEC_ID_ADPCM_XA,              //
    AV_CODEC_ID_ADPCM_ADX,             //
    AV_CODEC_ID_ADPCM_EA,              //
    AV_CODEC_ID_ADPCM_G726,            //
    AV_CODEC_ID_ADPCM_CT,              //
    AV_CODEC_ID_ADPCM_SWF,             //
    AV_CODEC_ID_ADPCM_YAMAHA,          //
    AV_CODEC_ID_ADPCM_SBPRO_4,         //
    AV_CODEC_ID_ADPCM_SBPRO_3,         //
    AV_CODEC_ID_ADPCM_SBPRO_2,         //
    AV_CODEC_ID_ADPCM_THP,             //
    AV_CODEC_ID_ADPCM_IMA_AMV,         //
    AV_CODEC_ID_ADPCM_EA_R1,           //
    AV_CODEC_ID_ADPCM_EA_R3,           //
    AV_CODEC_ID_ADPCM_EA_R2,           //
    AV_CODEC_ID_ADPCM_IMA_EA_SEAD,     //
    AV_CODEC_ID_ADPCM_IMA_EA_EACS,     //
    AV_CODEC_ID_ADPCM_EA_XAS,          //
    AV_CODEC_ID_ADPCM_EA_MAXIS_XA,     //
    AV_CODEC_ID_ADPCM_IMA_ISS,         //
    AV_CODEC_ID_ADPCM_G722,            //
    AV_CODEC_ID_ADPCM_IMA_APC,         //
    AV_CODEC_ID_ADPCM_VIMA,            //
    //
    AV_CODEC_ID_ADPCM_AFC = $11800, //
    AV_CODEC_ID_ADPCM_IMA_OKI,      //
    AV_CODEC_ID_ADPCM_DTK,          //
    AV_CODEC_ID_ADPCM_IMA_RAD,      //
    AV_CODEC_ID_ADPCM_G726LE,       //
    AV_CODEC_ID_ADPCM_THP_LE,       //
    AV_CODEC_ID_ADPCM_PSX,          //
    AV_CODEC_ID_ADPCM_AICA,         //
    AV_CODEC_ID_ADPCM_IMA_DAT4,     //
    AV_CODEC_ID_ADPCM_MTAF,         //
    AV_CODEC_ID_ADPCM_AGM,          //

    (* AMR *)                                                              //
    AV_CODEC_ID_AMR_NB = $12000, //
    AV_CODEC_ID_AMR_WB, //

    (* RealAudio codecs *)                                                  //
    AV_CODEC_ID_RA_144 = $13000, //
    AV_CODEC_ID_RA_288,

    (* various DPCM codecs *)
    AV_CODEC_ID_ROQ_DPCM = $14000, //
    AV_CODEC_ID_INTERPLAY_DPCM,    //
    AV_CODEC_ID_XAN_DPCM,          //
    AV_CODEC_ID_SOL_DPCM,          //

    AV_CODEC_ID_SDX2_DPCM = $14800, //
    AV_CODEC_ID_GREMLIN_DPCM,       //

    (* audio codecs *)
    AV_CODEC_ID_MP2 = $15000, //
    AV_CODEC_ID_MP3,
    /// < preferred ID for decoding MPEG audio layer 1, 2 or 3
    AV_CODEC_ID_AAC,           //
    AV_CODEC_ID_AC3,           //
    AV_CODEC_ID_DTS,           //
    AV_CODEC_ID_VORBIS,        //
    AV_CODEC_ID_DVAUDIO,       //
    AV_CODEC_ID_WMAV1,         //
    AV_CODEC_ID_WMAV2,         //
    AV_CODEC_ID_MACE3,         //
    AV_CODEC_ID_MACE6,         //
    AV_CODEC_ID_VMDAUDIO,      //
    AV_CODEC_ID_FLAC,          //
    AV_CODEC_ID_MP3ADU,        //
    AV_CODEC_ID_MP3ON4,        //
    AV_CODEC_ID_SHORTEN,       //
    AV_CODEC_ID_ALAC,          //
    AV_CODEC_ID_WESTWOOD_SND1, //
    AV_CODEC_ID_GSM,
    /// < as in Berlin toast format
    AV_CODEC_ID_QDM2,           //
    AV_CODEC_ID_COOK,           //
    AV_CODEC_ID_TRUESPEECH,     //
    AV_CODEC_ID_TTA,            //
    AV_CODEC_ID_SMACKAUDIO,     //
    AV_CODEC_ID_QCELP,          //
    AV_CODEC_ID_WAVPACK,        //
    AV_CODEC_ID_DSICINAUDIO,    //
    AV_CODEC_ID_IMC,            //
    AV_CODEC_ID_MUSEPACK7,      //
    AV_CODEC_ID_MLP,            //
    AV_CODEC_ID_GSM_MS,         (* as found in WAV *)
    AV_CODEC_ID_ATRAC3,         //
    AV_CODEC_ID_APE,            //
    AV_CODEC_ID_NELLYMOSER,     //
    AV_CODEC_ID_MUSEPACK8,      //
    AV_CODEC_ID_SPEEX,          //
    AV_CODEC_ID_WMAVOICE,       //
    AV_CODEC_ID_WMAPRO,         //
    AV_CODEC_ID_WMALOSSLESS,    //
    AV_CODEC_ID_ATRAC3P,        //
    AV_CODEC_ID_EAC3,           //
    AV_CODEC_ID_SIPR,           //
    AV_CODEC_ID_MP1,            //
    AV_CODEC_ID_TWINVQ,         //
    AV_CODEC_ID_TRUEHD,         //
    AV_CODEC_ID_MP4ALS,         //
    AV_CODEC_ID_ATRAC1,         //
    AV_CODEC_ID_BINKAUDIO_RDFT, //
    AV_CODEC_ID_BINKAUDIO_DCT,  //
    AV_CODEC_ID_AAC_LATM,       //
    AV_CODEC_ID_QDMC,           //
    AV_CODEC_ID_CELT,           //
    AV_CODEC_ID_G723_1,         //
    AV_CODEC_ID_G729,           //
    AV_CODEC_ID_8SVX_EXP,       //
    AV_CODEC_ID_8SVX_FIB,       //
    AV_CODEC_ID_BMV_AUDIO,      //
    AV_CODEC_ID_RALF,           //
    AV_CODEC_ID_IAC,            //
    AV_CODEC_ID_ILBC,           //
    AV_CODEC_ID_OPUS,           //
    AV_CODEC_ID_COMFORT_NOISE,  //
    AV_CODEC_ID_TAK,            //
    AV_CODEC_ID_METASOUND,      //
    AV_CODEC_ID_PAF_AUDIO,      //
    AV_CODEC_ID_ON2AVC,         //
    AV_CODEC_ID_DSS_SP,         //
    AV_CODEC_ID_CODEC2,         //

    AV_CODEC_ID_FFWAVESYNTH = $15800, //
    AV_CODEC_ID_SONIC,                //
    AV_CODEC_ID_SONIC_LS,             //
    AV_CODEC_ID_EVRC,                 //
    AV_CODEC_ID_SMV,                  //
    AV_CODEC_ID_DSD_LSBF,             //
    AV_CODEC_ID_DSD_MSBF,             //
    AV_CODEC_ID_DSD_LSBF_PLANAR,      //
    AV_CODEC_ID_DSD_MSBF_PLANAR,      //
    AV_CODEC_ID_4GV,                  //
    AV_CODEC_ID_INTERPLAY_ACM,        //
    AV_CODEC_ID_XMA1,                 //
    AV_CODEC_ID_XMA2,                 //
    AV_CODEC_ID_DST,                  //
    AV_CODEC_ID_ATRAC3AL,             //
    AV_CODEC_ID_ATRAC3PAL,            //
    AV_CODEC_ID_DOLBY_E,              //
    AV_CODEC_ID_APTX,                 //
    AV_CODEC_ID_APTX_HD,              //
    AV_CODEC_ID_SBC,                  //
    AV_CODEC_ID_ATRAC9,               //
    AV_CODEC_ID_HCOM,                 //
//    AV_CODEC_ID_ACELP_KELVIN,         // 4.2.2

    (* subtitle codecs *)                                                                                           //
    AV_CODEC_ID_FIRST_SUBTITLE = $17000,
    /// < A dummy ID pointing at the start of subtitle codecs.
    AV_CODEC_ID_DVD_SUBTITLE = $17000, //
    AV_CODEC_ID_DVB_SUBTITLE,          //
    AV_CODEC_ID_TEXT,
    /// < raw UTF-8 text
    AV_CODEC_ID_XSUB,              //
    AV_CODEC_ID_SSA,               //
    AV_CODEC_ID_MOV_TEXT,          //
    AV_CODEC_ID_HDMV_PGS_SUBTITLE, //
    AV_CODEC_ID_DVB_TELETEXT,      //
    AV_CODEC_ID_SRT,               //

    AV_CODEC_ID_MICRODVD = $17800,  //
    AV_CODEC_ID_EIA_608,            //
    AV_CODEC_ID_JACOSUB,            //
    AV_CODEC_ID_SAMI,               //
    AV_CODEC_ID_REALTEXT,           //
    AV_CODEC_ID_STL,                //
    AV_CODEC_ID_SUBVIEWER1,         //
    AV_CODEC_ID_SUBVIEWER,          //
    AV_CODEC_ID_SUBRIP,             //
    AV_CODEC_ID_WEBVTT,             //
    AV_CODEC_ID_MPL2,               //
    AV_CODEC_ID_VPLAYER,            //
    AV_CODEC_ID_PJS,                //
    AV_CODEC_ID_ASS,                //
    AV_CODEC_ID_HDMV_TEXT_SUBTITLE, //
    AV_CODEC_ID_TTML,               //
    AV_CODEC_ID_ARIB_CAPTION,       //

    (* other specific kind of codecs (generally used for attachments) *)
    AV_CODEC_ID_FIRST_UNKNOWN = $18000, // < A dummy ID pointing at the start of various fake codecs.
    AV_CODEC_ID_TTF = $18000,           //

    AV_CODEC_ID_SCTE_35,          // < Contain timestamp estimated through PCR of program stream.
//    AV_CODEC_ID_EPG,              // 4.2.2
    AV_CODEC_ID_BINTEXT = $18800, //
    AV_CODEC_ID_XBIN,             //
    AV_CODEC_ID_IDF,              //
    AV_CODEC_ID_OTF,              //
    AV_CODEC_ID_SMPTE_KLV,        //
    AV_CODEC_ID_DVD_NAV,          //
    AV_CODEC_ID_TIMED_ID3,        //
    AV_CODEC_ID_BIN_DATA,         //

    AV_CODEC_ID_MPEG2TS = $20000, (* < _FAKE_ codec to indicate a raw MPEG-2 TS
      * stream (only used by libavformat) *)
    AV_CODEC_ID_MPEG4SYSTEMS = $20001, (* < _FAKE_ codec to indicate a MPEG-4 Systems
      * stream (only used by libavformat) *)
    AV_CODEC_ID_FFMETADATA = $21000,     // < Dummy codec for streams containing only metadata information.
    AV_CODEC_ID_WRAPPED_AVFRAME = $21001 // < Passthrough codec, AVFrames wrapped in AVPacket
    );

const
  AV_CODEC_ID_IFF_BYTERUN1: AVCodecID = AV_CODEC_ID_IFF_ILBM;
  AV_CODEC_ID_H265: AVCodecID         = AV_CODEC_ID_HEVC;

  (*
    * The codec supports this format via the hw_device_ctx interface.
    *
    * When selecting this format, AVCodecContext.hw_device_ctx should
    * have been set to a device of the specified type before calling
    * avcodec_open2().
  *)
  AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX = $01;
  (*
    * The codec supports this format via the hw_frames_ctx interface.
    *
    * When selecting this format for a decoder,
    * AVCodecContext.hw_frames_ctx should be set to a suitable frames
    * context inside the get_format() callback.  The frames context
    * must have been created on a device of the specified type.
  *)
  AV_CODEC_HW_CONFIG_METHOD_HW_FRAMES_CTX = $02;
  (*
    * The codec supports this format by some internal method.
    *
    * This format can be selected without any additional configuration -
    * no device or frames context is required.
  *)
  AV_CODEC_HW_CONFIG_METHOD_INTERNAL = $04;
  (*
    * The codec supports this format by some ad-hoc method.
    *
    * Additional settings and/or function calls are required.  See the
    * codec-specific documentation for details.  (Methods requiring
    * this sort of configuration are deprecated and others should be
    * used in preference.)
  *)
  AV_CODEC_HW_CONFIG_METHOD_AD_HOC = $08;

  (*
    * Codec uses only intra compression.
    * Video and audio codecs only.
  *)
  AV_CODEC_PROP_INTRA_ONLY = (1 shl 0);
  (*
    * Codec supports lossy compression. Audio and video codecs only.
    * @note a codec may support both lossy and lossless
    * compression modes
  *)
  AV_CODEC_PROP_LOSSY = (1 shl 1);
  (*
    * Codec supports lossless compression. Audio and video codecs only.
  *)
  AV_CODEC_PROP_LOSSLESS = (1 shl 2);
  (*
    * Codec supports frame reordering. That is, the coded order (the order in which
    * the encoded packets are output by the encoders / stored / input to the
    * decoders) may be different from the presentation order of the corresponding
    * frames.
    *
    * For codecs that do not have this property set, PTS and DTS should always be
    * equal.
  *)
  AV_CODEC_PROP_REORDER = (1 shl 3);
  (*
    * Subtitle codec is bitmap based
    * Decoded AVSubtitle data can be read from the AVSubtitleRect->pict field.
  *)
  AV_CODEC_PROP_BITMAP_SUB = (1 shl 16);
  (*
    * Subtitle codec is text based.
    * Decoded AVSubtitle data can be read from the AVSubtitleRect->ass field.
  *)
  AV_CODEC_PROP_TEXT_SUB = (1 shl 17);

  (*
    * @ingroup lavc_decoding
    * Required number of additionally allocated bytes at the end of the input bitstream for decoding.
    * This is mainly needed because some optimized bitstream readers read
    * 32 or 64 bit at once and could read over the end.<br>
    * Note: If the first 23 bits of the additional bytes are not 0, then damaged
    * MPEG bitstreams could cause overread and segfault.
  *)
  AV_INPUT_BUFFER_PADDING_SIZE = 64;

  (*
    * @ingroup lavc_encoding
    * minimum encoding buffer size
    * Used to avoid some checks during header writing.
  *)
  AV_INPUT_BUFFER_MIN_SIZE = 16384;

  (* encoding support
    These flags can be passed in AVCodecContext.flags before initialization.
    Note: Not everything is supported yet.
  *)

  (*
    * Allow decoders to produce frames with data planes that are not aligned
    * to CPU requirements (e.g. due to cropping).
  *)
  AV_CODEC_FLAG_UNALIGNED = (1 shl 0);
  (*
    * Use fixed qscale.
  *)
  AV_CODEC_FLAG_QSCALE = (1 shl 1);
  (*
    * 4 MV per MB allowed / advanced prediction for H.263.
  *)
  AV_CODEC_FLAG_4MV = (1 shl 2);
  (*
    * Output even those frames that might be corrupted.
  *)
  AV_CODEC_FLAG_OUTPUT_CORRUPT = (1 shl 3);
  (*
    * Use qpel MC.
  *)
  AV_CODEC_FLAG_QPEL = (1 shl 4);
  (*
    * Don't output frames whose parameters differ from first
    * decoded frame in stream.
  *)
  AV_CODEC_FLAG_DROPCHANGED = (1 shl 5);
  (*
    * Use internal 2pass ratecontrol in first pass mode.
  *)
  AV_CODEC_FLAG_PASS1 = (1 shl 9);
  (*
    * Use internal 2pass ratecontrol in second pass mode.
  *)
  AV_CODEC_FLAG_PASS2 = (1 shl 10);
  (*
    * loop filter.
  *)
  AV_CODEC_FLAG_LOOP_FILTER = (1 shl 11);
  (*
    * Only decode/encode grayscale.
  *)
  AV_CODEC_FLAG_GRAY = (1 shl 13);
  (*
    * error[?] variables will be set during encoding.
  *)
  AV_CODEC_FLAG_PSNR = (1 shl 15);
  (*
    * Input bitstream might be truncated at a random location
    * instead of only at frame boundaries.
  *)
  AV_CODEC_FLAG_TRUNCATED = (1 shl 16);
  (*
    * Use interlaced DCT.
  *)
  AV_CODEC_FLAG_INTERLACED_DCT = (1 shl 18);
  (*
    * Force low delay.
  *)
  AV_CODEC_FLAG_LOW_DELAY = (1 shl 19);
  (*
    * Place global headers in extradata instead of every keyframe.
  *)
  AV_CODEC_FLAG_GLOBAL_HEADER = (1 shl 22);
  (*
    * Use only bitexact stuff =(except =(I);DCT);.
  *)
  AV_CODEC_FLAG_BITEXACT = (1 shl 23);
  (* Fx : Flag for H.263+ extra options *)
  (*
    * H.263 advanced intra coding / MPEG-4 AC prediction
  *)
  AV_CODEC_FLAG_AC_PRED = (1 shl 24);
  (*
    * interlaced motion estimation
  *)
  AV_CODEC_FLAG_INTERLACED_ME = (1 shl 29);
  AV_CODEC_FLAG_CLOSED_GOP    = (1 shl 31);

  (*
    * Allow non spec compliant speedup tricks.
  *)
  AV_CODEC_FLAG2_FAST = (1 shl 0);
  (*
    * Skip bitstream encoding.
  *)
  AV_CODEC_FLAG2_NO_OUTPUT = (1 shl 2);
  (*
    * Place global headers at every keyframe instead of in extradata.
  *)
  AV_CODEC_FLAG2_LOCAL_HEADER = (1 shl 3);

  (*
    * timecode is in drop frame format. DEPRECATED!!!!
  *)
  AV_CODEC_FLAG2_DROP_FRAME_TIMECODE = (1 shl 13)deprecated;

  (*
    * Input bitstream might be truncated at a packet boundaries
    * instead of only at frame boundaries.
  *)
  AV_CODEC_FLAG2_CHUNKS = (1 shl 15);
  (*
    * Discard cropping information from SPS.
  *)
  AV_CODEC_FLAG2_IGNORE_CROP = (1 shl 16);

  (*
    * Show all frames before the first keyframe
  *)
  AV_CODEC_FLAG2_SHOW_ALL = (1 shl 22);
  (*
    * Export motion vectors through frame side data
  *)
  AV_CODEC_FLAG2_EXPORT_MVS = (1 shl 28);
  (*
    * Do not skip samples and export skip information as frame side data
  *)
  AV_CODEC_FLAG2_SKIP_MANUAL = (1 shl 29);
  (*
    * Do not reset ASS ReadOrder field on flush =(subtitles decoding);
  *)
  AV_CODEC_FLAG2_RO_FLUSH_NOOP = (1 shl 30);

  (* Unsupported options :
    *              Syntax Arithmetic coding =(SAC);
    *              Reference Picture Selection
    *              Independent Segment Decoding *)
  (* /Fx *)
  (* codec capabilities *)

  (*
    * Decoder can use draw_horiz_band callback.
  *)
  AV_CODEC_CAP_DRAW_HORIZ_BAND = (1 shl 0);
  (*
    * Codec uses get_buffer=(); for allocating buffers and supports custom allocators.
    * If not set, it might not use get_buffer=(); at all or use operations that
    * assume the buffer was allocated by avcodec_default_get_buffer.
  *)
  AV_CODEC_CAP_DR1       = (1 shl 1);
  AV_CODEC_CAP_TRUNCATED = (1 shl 3);
  (*
    * Encoder or decoder requires flushing with NULL input at the end in order to
    * give the complete and correct output.
    *
    * NOTE: If this flag is not set, the codec is guaranteed to never be fed with
    *       with NULL data. The user can still send NULL data to the public encode
    *       or decode function, but libavcodec will not pass it along to the codec
    *       unless this flag is set.
    *
    * Decoders:
    * The decoder has a non-zero delay and needs to be fed with avpkt->data=NULL,
    * avpkt->size=0 at the end to get the delayed data until the decoder no longer
    * returns frames.
    *
    * Encoders:
    * The encoder needs to be fed with NULL data at the end of encoding until the
    * encoder no longer returns data.
    *
    * NOTE: For encoders implementing the AVCodec.encode2=(); function, setting this
    *       flag also means that the encoder must set the pts and duration for
    *       each output packet. If this flag is not set, the pts and duration will
    *       be determined by libavcodec from the input frame.
  *)
  AV_CODEC_CAP_DELAY = (1 shl 5);
  (*
    * Codec can be fed a final frame with a smaller size.
    * This can be used to prevent truncation of the last audio samples.
  *)
  AV_CODEC_CAP_SMALL_LAST_FRAME = (1 shl 6);

  (*
    * Codec can output multiple frames per AVPacket
    * Normally demuxers return one frame at a time, demuxers which do not do
    * are connected to a parser to split what they return into proper frames.
    * This flag is reserved to the very rare category of codecs which have a
    * bitstream that cannot be split into frames without timeconsuming
    * operations like full decoding. Demuxers carrying such bitstreams thus
    * may return multiple frames in a packet. This has many disadvantages like
    * prohibiting stream copy in many cases thus it should only be considered
    * as a last resort.
  *)
  AV_CODEC_CAP_SUBFRAMES = (1 shl 8);
  (*
    * Codec is experimental and is thus avoided in favor of non experimental
    * encoders
  *)
  AV_CODEC_CAP_EXPERIMENTAL = (1 shl 9);
  (*
    * Codec should fill in channel configuration and samplerate instead of container
  *)
  AV_CODEC_CAP_CHANNEL_CONF = (1 shl 10);
  (*
    * Codec supports frame-level multithreading.
  *)
  AV_CODEC_CAP_FRAME_THREADS = (1 shl 12);
  (*
    * Codec supports slice-based =(or partition-based); multithreading.
  *)
  AV_CODEC_CAP_SLICE_THREADS = (1 shl 13);
  (*
    * Codec supports changed parameters at any point.
  *)
  AV_CODEC_CAP_PARAM_CHANGE = (1 shl 14);
  (*
    * Codec supports avctx->thread_count == 0 =(auto);.
  *)
  AV_CODEC_CAP_AUTO_THREADS = (1 shl 15);
  (*
    * Audio encoder supports receiving a different number of samples in each call.
  *)
  AV_CODEC_CAP_VARIABLE_FRAME_SIZE = (1 shl 16);
  (*
    * Decoder is not a preferred choice for probing.
    * This indicates that the decoder is not a good choice for probing.
    * It could for example be an expensive to spin up hardware decoder,
    * or it could simply not provide a lot of useful information about
    * the stream.
    * A decoder marked with this flag should only be used as last resort
    * choice for probing.
  *)
  AV_CODEC_CAP_AVOID_PROBING = (1 shl 17);
  (*
    * Codec is intra only.
  *)
  AV_CODEC_CAP_INTRA_ONLY = $40000000;
  (*
    * Codec is lossless.
  *)
  AV_CODEC_CAP_LOSSLESS = $80000000;

  (*
    * Codec is backed by a hardware implementation. Typically used to
    * identify a non-hwaccel hardware decoder. For information about hwaccels, use
    * avcodec_get_hw_config=(); instead.
  *)
  AV_CODEC_CAP_HARDWARE = (1 shl 18);

  (*
    * Codec is potentially backed by a hardware implementation, but not
    * necessarily. This is used instead of AV_CODEC_CAP_HARDWARE, if the
    * implementation provides some sort of internal fallback.
  *)
  AV_CODEC_CAP_HYBRID = (1 shl 19);
  (*
    * This codec takes the reordered_opaque field from input AVFrames
    * and returns it in the corresponding field in AVCodecContext after
    * encoding.
  *)
  AV_CODEC_CAP_ENCODER_REORDERED_OPAQUE = (1 shl 20);

const
  (*
    * HWAccel is experimental and is thus avoided in favor of non experimental
    * codecs
  *)
  AV_HWACCEL_CODEC_CAP_EXPERIMENTAL = $0200;

  (*
    * Hardware acceleration should be used for decoding even if the codec level
    * used is unknown or higher than the maximum supported level reported by the
    * hardware driver.
    *
    * It's generally a good idea to pass this flag unless you have a specific
    * reason not to, as hardware tends to under-report supported levels.
  *)
  AV_HWACCEL_FLAG_IGNORE_LEVEL = (1 shl 0);

  (*
    * Hardware acceleration can output YUV pixel formats with a different chroma
    * sampling than 4:2:0 and/or other than 8 bits per component.
  *)
  AV_HWACCEL_FLAG_ALLOW_HIGH_DEPTH = (1 shl 1);

  (*
    * Hardware acceleration should still be attempted for decoding when the
    * codec profile does not match the reported capabilities of the hardware.
    *
    * For example, this can be used to try to decode baseline profile H.264
    * streams in hardware - it will often succeed, because many streams marked
    * as baseline profile actually conform to constrained baseline profile.
    *
    * @warning If the stream is actually not supported then the behaviour is
    *          undefined, and may include returning entirely incorrect output
    *          while indicating success.
  *)
  AV_HWACCEL_FLAG_ALLOW_PROFILE_MISMATCH = (1 shl 2);

type

  (*
    * AVProfile.
  *)
  AVProfile = record
    profile: int;
    name: pAnsiChar; // < short name for the profile
  end;

  pAVProfile = ^AVProfile;

  (*
    * This struct describes the properties of a single codec described by an
    * AVCodecID.
    * @see avcodec_descriptor_get()
  *)
  pAVCodecDescriptor = ^AVCodecDescriptor;

  AVCodecDescriptor = record
    id: AVCodecID;
    _type: AVMediaType;
    (*
      * Name of the codec described by this descriptor. It is non-empty and
      * unique for each codec descriptor. It should contain alphanumeric
      * characters and '_' only.
    *)
    name: pAnsiChar;
    (*
      * A more descriptive name for this codec. May be NULL.
    *)
    long_name: pAnsiChar;
    (*
      * Codec properties, a combination of AV_CODEC_PROP_* flags.
    *)
    props: int;
    (*
      * MIME type(s) associated with the codec.
      * May be NULL; if not, a NULL-terminated array of MIME types.
      * The first item is always non-NULL and is the preferred MIME type.
    *)
    // const char *const *mime_types;
    mime_types: PPAnsiChar;
    (*
      * If non-NULL, an array of profiles recognized for this codec.
      * Terminated with FF_PROFILE_UNKNOWN.
    *)
    // const struct AVProfile *profiles;
    profiles: pAVProfile;
  end;

  // AVHWDeviceType = (
  // AV_HWDEVICE_TYPE_VDPAU,
  // AV_HWDEVICE_TYPE_CUDA,
  // AV_HWDEVICE_TYPE_VAAPI,
  // AV_HWDEVICE_TYPE_DXVA2,
  // AV_HWDEVICE_TYPE_QSV,
  // AV_HWDEVICE_TYPE_VIDEOTOOLBOX,
  // AV_HWDEVICE_TYPE_NONE,
  // AV_HWDEVICE_TYPE_D3D11VA,
  // AV_HWDEVICE_TYPE_DRM
  // );

  pAVCodecHWConfig = ^AVCodecHWConfig;

  AVCodecHWConfig = record
    (*
      * A hardware pixel format which the codec can use.
    *)
    pix_fmt: AVPixelFormat;
    (*
      * Bit set of AV_CODEC_HW_CONFIG_METHOD_* flags, describing the possible
      * setup methods which can be used with this configuration.
    *)
    methods: int;
    (*
      * The device type associated with the configuration.
      *
      * Must be set for AV_CODEC_HW_CONFIG_METHOD_HW_DEVICE_CTX and
      * AV_CODEC_HW_CONFIG_METHOD_HW_FRAMES_CTX, otherwise unused.
    *)
    device_type: AVHWDeviceType;
  end;

  (*
    * @ingroup lavc_decoding
  *)
  AVDiscard = ( //
    (* We leave some space between them for extensions (drop some
      * keyframes for intra-only or drop just some bidir frames). *)
    AVDISCARD_NONE = -16,    // < discard nothing
    AVDISCARD_DEFAULT = 0,   // < discard useless packets like 0 size packets in avi
    AVDISCARD_NONREF = 8,    // < discard all non reference
    AVDISCARD_BIDIR = 16,    // < discard all bidirectional frames
    AVDISCARD_NONINTRA = 24, // < discard all non intra frames
    AVDISCARD_NONKEY = 32,   // < discard all frames except keyframes
    AVDISCARD_ALL = 48       // < discard all
    );

  AVAudioServiceType = ( //
    AV_AUDIO_SERVICE_TYPE_MAIN = 0, AV_AUDIO_SERVICE_TYPE_EFFECTS = 1, AV_AUDIO_SERVICE_TYPE_VISUALLY_IMPAIRED = 2, AV_AUDIO_SERVICE_TYPE_HEARING_IMPAIRED = 3,
    AV_AUDIO_SERVICE_TYPE_DIALOGUE = 4, AV_AUDIO_SERVICE_TYPE_COMMENTARY = 5, AV_AUDIO_SERVICE_TYPE_EMERGENCY = 6, AV_AUDIO_SERVICE_TYPE_VOICE_OVER = 7,
    AV_AUDIO_SERVICE_TYPE_KARAOKE = 8, AV_AUDIO_SERVICE_TYPE_NB
    // < Not part of ABI
    );

  (*
    * @ingroup lavc_encoding
  *)
  RcOverride = record
    start_frame: int;
    end_frame: int;
    qscale: int; // If this is 0 then quality_factor will be used instead.
    quality_factor: float;
  end;

  pRcOverride = ^RcOverride;

  (*
    * Pan Scan area.
    * This specifies the area which should be displayed.
    * Note there may be multiple such areas for one frame.
  *)
  AVPanScan = record
    (*
      * id
      * - encoding: Set by user.
      * - decoding: Set by libavcodec.
    *)
    id: int;

    (*
      * width and height in 1/16 pel
      * - encoding: Set by user.
      * - decoding: Set by libavcodec.
    *)
    width: int;
    height: int;

    (*
      * position of the top left corner in 1/16 pel for up to 3 fields/frames
      * - encoding: Set by user.
      * - decoding: Set by libavcodec.
    *)
    position: array [0 .. 2, 0 .. 1] of int16_t;
  end;

  (*
    * This structure describes the bitrate properties of an encoded bitstream. It
    * roughly corresponds to a subset the VBV parameters for MPEG-2 or HRD
    * parameters for H.264/HEVC.
  *)
  pAVCPBProperties = ^AVCPBProperties;

  AVCPBProperties = record
    (*
      * Maximum bitrate of the stream, in bits per second.
      * Zero if unknown or unspecified.
    *)
{$IFDEF FF_API_UNSANITIZED_BITRATES}
    max_bitrate: int;
{$ELSE}
    max_bitrate: int64_t;
{$ENDIF}
    (*
      * Minimum bitrate of the stream, in bits per second.
      * Zero if unknown or unspecified.
    *)
{$IFDEF FF_API_UNSANITIZED_BITRATES}
    min_bitrate: int;
{$ELSE}
    min_bitrate: int64_t;
{$ENDIF}
    (*
      * Average bitrate of the stream, in bits per second.
      * Zero if unknown or unspecified.
    *)
{$IFDEF FF_API_UNSANITIZED_BITRATES}
    avg_bitrate: int;
{$ELSE}
    avg_bitrate: int64_t;
{$ENDIF}
    (*
      * The size of the buffer to which the ratecontrol is applied, in bits.
      * Zero if unknown or unspecified.
    *)
    buffer_size: int;

    (*
      * The delay between the time the packet this structure is associated with
      * is received and the time when it should be decoded, in periods of a 27MHz
      * clock.
      *
      * UINT64_MAX when unknown or unspecified.
    *)
    vbv_delay: uint64_t;
  end;

const
  (*
    * The decoder will keep a reference to the frame and may reuse it later.
  *)
  AV_GET_BUFFER_FLAG_REF = (1 shl 0);

  AV_PKT_FLAG_KEY     = $0001; // < The packet contains a keyframe
  AV_PKT_FLAG_CORRUPT = $0002; // < The packet content is corrupted
  (*
    * Flag is used to discard packets which are required to maintain valid
    * decoder state but are not required for output and should be dropped
    * after decoding.
    * *)
  AV_PKT_FLAG_DISCARD = $0004;
  (*
    * The packet comes from a trusted source.
    *
    * Otherwise-unsafe constructs such as arbitrary pointers to data
    * outside the packet may be followed.
  *)
  AV_PKT_FLAG_TRUSTED = $0008;
  (*
    * Flag is used to indicate packets that contain frames that can
    * be discarded by the decoder.  I.e. Non-reference frames.
  *)
  AV_PKT_FLAG_DISPOSABLE = $0010;

  // AVCodecContext -> int compression_level;
  FF_COMPRESSION_DEFAULT = -1;
{$IFDEF FF_API_PRIVATE_OPT}
  // AVCodecContext -> int prediction_method;
  FF_PRED_LEFT   = 0;
  FF_PRED_PLANE  = 1;
  FF_PRED_MEDIAN = 2;
{$ENDIF}
  // AVCodecContext -> int ildct_cmp;
  FF_CMP_SAD        = 0;
  FF_CMP_SSE        = 1;
  FF_CMP_SATD       = 2;
  FF_CMP_DCT        = 3;
  FF_CMP_PSNR       = 4;
  FF_CMP_BIT        = 5;
  FF_CMP_RD         = 6;
  FF_CMP_ZERO       = 7;
  FF_CMP_VSAD       = 8;
  FF_CMP_VSSE       = 9;
  FF_CMP_NSSE       = 10;
  FF_CMP_W53        = 11;
  FF_CMP_W97        = 12;
  FF_CMP_DCTMAX     = 13;
  FF_CMP_DCT264     = 14;
  FF_CMP_MEDIAN_SAD = 15;
  FF_CMP_CHROMA     = 256;

  // AVCodecContext -> slice_flags:int;
  SLICE_FLAG_CODED_ORDER = $0001; // < draw_horiz_band() is called in coded order instead of display
  SLICE_FLAG_ALLOW_FIELD = $0002; // < allow draw_horiz_band() with field slices (MPEG-2 field pics)
  SLICE_FLAG_ALLOW_PLANE = $0004; // < allow draw_horiz_band() with 1 component at a time (SVQ1)

  // AVCodecContext -> int mb_decision;
  FF_MB_DECISION_SIMPLE = 0; // < uses mb_cmp
  FF_MB_DECISION_BITS   = 1; // < chooses the one which needs the fewest bits
  FF_MB_DECISION_RD     = 2; // < rate distortion

{$IFDEF FF_API_CODER_TYPE}
  // AVCodecContext -> int coder_type;
  FF_CODER_TYPE_VLC = 0;
  FF_CODER_TYPE_AC  = 1;
  FF_CODER_TYPE_RAW = 2;
  FF_CODER_TYPE_RLE = 3;
{$ENDIF}
  // AVCodecContext -> int workaround_bugs;
  FF_BUG_AUTODETECT       = 1; // < autodetection
  FF_BUG_XVID_ILACE       = 4;
  FF_BUG_UMP4             = 8;
  FF_BUG_NO_PADDING       = 16;
  FF_BUG_AMV              = 32;
  FF_BUG_QPEL_CHROMA      = 64;
  FF_BUG_STD_QPEL         = 128;
  FF_BUG_QPEL_CHROMA2     = 256;
  FF_BUG_DIRECT_BLOCKSIZE = 512;
  FF_BUG_EDGE             = 1024;
  FF_BUG_HPEL_CHROMA      = 2048;
  FF_BUG_DC_CLIP          = 4096;
  FF_BUG_MS               = 8192; // < Work around various bugs in Microsoft's broken decoders.
  FF_BUG_TRUNCATED        = 16384;
  FF_BUG_IEDGE            = 32768;

  // AVCodecContext -> int strict_std_compliance;
  FF_COMPLIANCE_VERY_STRICT  = 2; // < Strictly conform to an older more strict version of the spec or reference software.
  FF_COMPLIANCE_STRICT       = 1; // < Strictly conform to all the things in the spec no matter what consequences.
  FF_COMPLIANCE_NORMAL       = 0;
  FF_COMPLIANCE_UNOFFICIAL   = -1; // < Allow unofficial extensions
  FF_COMPLIANCE_EXPERIMENTAL = -2; // < Allow nonstandardized experimental things.

  // AVCodecContext -> error_concealment:int;
  FF_EC_GUESS_MVS   = 1;
  FF_EC_DEBLOCK     = 2;
  FF_EC_FAVOR_INTER = 256;

  // AVCodecContext -> debug:int;
  FF_DEBUG_PICT_INFO = 1;
  FF_DEBUG_RC        = 2;
  FF_DEBUG_BITSTREAM = 4;
  FF_DEBUG_MB_TYPE   = 8;
  FF_DEBUG_QP        = 16;
{$IFDEF FF_API_DEBUG_MV}
  (*
    * @deprecated this option does nothing
  *)
  FF_DEBUG_MV = 32 deprecated;
{$ENDIF}
  FF_DEBUG_DCT_COEFF = $00000040;
  FF_DEBUG_SKIP      = $00000080;
  FF_DEBUG_STARTCODE = $00000100;
  FF_DEBUG_ER        = $00000400;
  FF_DEBUG_MMCO      = $00000800;
  FF_DEBUG_BUGS      = $00001000;
{$IFDEF FF_API_DEBUG_MV}
  FF_DEBUG_VIS_QP      = $00002000;
  FF_DEBUG_VIS_MB_TYPE = $00004000;
{$ENDIF}
  FF_DEBUG_BUFFERS  = $00008000;
  FF_DEBUG_THREADS  = $00010000;
  FF_DEBUG_GREEN_MD = $00800000;
  FF_DEBUG_NOMC     = $01000000;
{$IFDEF FF_API_DEBUG_MV}
  (*
    * debug
    * - encoding: Set by user.
    * - decoding: Set by user.
  *)
  // AVCodecContext -> int debug_mv;
  FF_DEBUG_VIS_MV_P_FOR  = $00000001; // visualize forward predicted MVs of P-frames
  FF_DEBUG_VIS_MV_B_FOR  = $00000002; // visualize forward predicted MVs of B-frames
  FF_DEBUG_VIS_MV_B_BACK = $00000004; // visualize backward predicted MVs of B-frames
{$ENDIF}
  // AVCodecContext ->err_recognition:int;
  (*
    * Verify checksums embedded in the bitstream (could be of either encoded or
    * decoded data, depending on the codec) and print an error message on mismatch.
    * If AV_EF_EXPLODE is also set, a mismatching checksum will result in the
    * decoder returning an error.
  *)
  AV_EF_CRCCHECK  = (1 shl 0);
  AV_EF_BITSTREAM = (1 shl 1); // < detect bitstream specification deviations
  AV_EF_BUFFER    = (1 shl 2); // < detect improper bitstream length
  AV_EF_EXPLODE   = (1 shl 3); // < abort decoding on minor error detection

  AV_EF_IGNORE_ERR = (1 shl 15); // < ignore errors and continue
  AV_EF_CAREFUL    = (1 shl 16);
  // < consider things that violate the spec, are fast to calculate and have not been seen in the wild as errors
  AV_EF_COMPLIANT  = (1 shl 17); // < consider all spec non compliances as errors
  AV_EF_AGGRESSIVE = (1 shl 18); // < consider things that a sane encoder should not do as an error

  // AVCodecContext ->int dct_algo;
  FF_DCT_AUTO    = 0;
  FF_DCT_FASTINT = 1;
  FF_DCT_INT     = 2;
  FF_DCT_MMX     = 3;
  FF_DCT_ALTIVEC = 5;
  FF_DCT_FAAN    = 6;

  // AVCodecContext -> int idct_algo;
  FF_IDCT_AUTO          = 0;
  FF_IDCT_INT           = 1;
  FF_IDCT_SIMPLE        = 2;
  FF_IDCT_SIMPLEMMX     = 3;
  FF_IDCT_ARM           = 7;
  FF_IDCT_ALTIVEC       = 8;
  FF_IDCT_SIMPLEARM     = 10;
  FF_IDCT_XVID          = 14;
  FF_IDCT_SIMPLEARMV5TE = 16;
  FF_IDCT_SIMPLEARMV6   = 17;
  FF_IDCT_FAAN          = 20;
  FF_IDCT_SIMPLENEON    = 22;
  FF_IDCT_NONE          = 24; (* Used by XvMC to extract IDCT coefficients with FF_IDCT_PERM_NONE *)
  FF_IDCT_SIMPLEAUTO    = 128;

  // AVCodecContext -> int thread_type;
  FF_THREAD_FRAME = 1; // < Decode more than one frame at once
  FF_THREAD_SLICE = 2; // < Decode more than one part of a single frame at once

  // AVCodecContext -> int profile;
  FF_PROFILE_UNKNOWN  = -99;
  FF_PROFILE_RESERVED = -100;

  FF_PROFILE_AAC_MAIN      = 0;
  FF_PROFILE_AAC_LOW       = 1;
  FF_PROFILE_AAC_SSR       = 2;
  FF_PROFILE_AAC_LTP       = 3;
  FF_PROFILE_AAC_HE        = 4;
  FF_PROFILE_AAC_HE_V2     = 28;
  FF_PROFILE_AAC_LD        = 22;
  FF_PROFILE_AAC_ELD       = 38;
  FF_PROFILE_MPEG2_AAC_LOW = 128;
  FF_PROFILE_MPEG2_AAC_HE  = 131;

  FF_PROFILE_DNXHD     = 0;
  FF_PROFILE_DNXHR_LB  = 1;
  FF_PROFILE_DNXHR_SQ  = 2;
  FF_PROFILE_DNXHR_HQ  = 3;
  FF_PROFILE_DNXHR_HQX = 4;
  FF_PROFILE_DNXHR_444 = 5;

  FF_PROFILE_DTS         = 20;
  FF_PROFILE_DTS_ES      = 30;
  FF_PROFILE_DTS_96_24   = 40;
  FF_PROFILE_DTS_HD_HRA  = 50;
  FF_PROFILE_DTS_HD_MA   = 60;
  FF_PROFILE_DTS_EXPRESS = 70;

  FF_PROFILE_MPEG2_422          = 0;
  FF_PROFILE_MPEG2_HIGH         = 1;
  FF_PROFILE_MPEG2_SS           = 2;
  FF_PROFILE_MPEG2_SNR_SCALABLE = 3;
  FF_PROFILE_MPEG2_MAIN         = 4;
  FF_PROFILE_MPEG2_SIMPLE       = 5;

  FF_PROFILE_H264_CONSTRAINED = (1 shl 9);  // 8+1; constraint_set1_flag
  FF_PROFILE_H264_INTRA       = (1 shl 11); // 8+3; constraint_set3_flag

  FF_PROFILE_H264_BASELINE             = 66;
  FF_PROFILE_H264_CONSTRAINED_BASELINE = (66 or FF_PROFILE_H264_CONSTRAINED);
  FF_PROFILE_H264_MAIN                 = 77;
  FF_PROFILE_H264_EXTENDED             = 88;
  FF_PROFILE_H264_HIGH                 = 100;
  FF_PROFILE_H264_HIGH_10              = 110;
  FF_PROFILE_H264_HIGH_10_INTRA        = (110 or FF_PROFILE_H264_INTRA);
  FF_PROFILE_H264_MULTIVIEW_HIGH       = 118;
  FF_PROFILE_H264_HIGH_422             = 122;
  FF_PROFILE_H264_HIGH_422_INTRA       = (122 or FF_PROFILE_H264_INTRA);
  FF_PROFILE_H264_STEREO_HIGH          = 128;
  FF_PROFILE_H264_HIGH_444             = 144;
  FF_PROFILE_H264_HIGH_444_PREDICTIVE  = 244;
  FF_PROFILE_H264_HIGH_444_INTRA       = (244 or FF_PROFILE_H264_INTRA);
  FF_PROFILE_H264_CAVLC_444            = 44;

  FF_PROFILE_VC1_SIMPLE   = 0;
  FF_PROFILE_VC1_MAIN     = 1;
  FF_PROFILE_VC1_COMPLEX  = 2;
  FF_PROFILE_VC1_ADVANCED = 3;

  FF_PROFILE_MPEG4_SIMPLE                    = 0;
  FF_PROFILE_MPEG4_SIMPLE_SCALABLE           = 1;
  FF_PROFILE_MPEG4_CORE                      = 2;
  FF_PROFILE_MPEG4_MAIN                      = 3;
  FF_PROFILE_MPEG4_N_BIT                     = 4;
  FF_PROFILE_MPEG4_SCALABLE_TEXTURE          = 5;
  FF_PROFILE_MPEG4_SIMPLE_FACE_ANIMATION     = 6;
  FF_PROFILE_MPEG4_BASIC_ANIMATED_TEXTURE    = 7;
  FF_PROFILE_MPEG4_HYBRID                    = 8;
  FF_PROFILE_MPEG4_ADVANCED_REAL_TIME        = 9;
  FF_PROFILE_MPEG4_CORE_SCALABLE             = 10;
  FF_PROFILE_MPEG4_ADVANCED_CODING           = 11;
  FF_PROFILE_MPEG4_ADVANCED_CORE             = 12;
  FF_PROFILE_MPEG4_ADVANCED_SCALABLE_TEXTURE = 13;
  FF_PROFILE_MPEG4_SIMPLE_STUDIO             = 14;
  FF_PROFILE_MPEG4_ADVANCED_SIMPLE           = 15;

  FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_0  = 1;
  FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_1  = 2;
  FF_PROFILE_JPEG2000_CSTREAM_NO_RESTRICTION = 32768;
  FF_PROFILE_JPEG2000_DCINEMA_2K             = 3;
  FF_PROFILE_JPEG2000_DCINEMA_4K             = 4;

  FF_PROFILE_VP9_0 = 0;
  FF_PROFILE_VP9_1 = 1;
  FF_PROFILE_VP9_2 = 2;
  FF_PROFILE_VP9_3 = 3;

  FF_PROFILE_HEVC_MAIN               = 1;
  FF_PROFILE_HEVC_MAIN_10            = 2;
  FF_PROFILE_HEVC_MAIN_STILL_PICTURE = 3;
  FF_PROFILE_HEVC_REXT               = 4;

  FF_PROFILE_AV1_MAIN         = 0;
  FF_PROFILE_AV1_HIGH         = 1;
  FF_PROFILE_AV1_PROFESSIONAL = 2;

  FF_PROFILE_MJPEG_HUFFMAN_BASELINE_DCT            = $C0;
  FF_PROFILE_MJPEG_HUFFMAN_EXTENDED_SEQUENTIAL_DCT = $C1;
  FF_PROFILE_MJPEG_HUFFMAN_PROGRESSIVE_DCT         = $C2;
  FF_PROFILE_MJPEG_HUFFMAN_LOSSLESS                = $C3;
  FF_PROFILE_MJPEG_JPEG_LS                         = $F7;

  FF_PROFILE_SBC_MSBC = 1;

  FF_PROFILE_PRORES_PROXY    = 0;
  FF_PROFILE_PRORES_LT       = 1;
  FF_PROFILE_PRORES_STANDARD = 2;
  FF_PROFILE_PRORES_HQ       = 3;
  FF_PROFILE_PRORES_4444     = 4;
  FF_PROFILE_PRORES_XQ       = 5;

  FF_PROFILE_ARIB_PROFILE_A = 0;
  FF_PROFILE_ARIB_PROFILE_C = 1;

  (*
    * level
    * - encoding: Set by user.
    * - decoding: Set by libavcodec.
  *)
  // AVCodecContext ->int level;
  FF_LEVEL_UNKNOWN = -99;

  // AVCodecContext -> int sub_charenc_mode;
  FF_SUB_CHARENC_MODE_DO_NOTHING = -1;
  // < do nothing (demuxer outputs a stream supposed to be already in UTF-8, or the codec is bitmap for instance)
  FF_SUB_CHARENC_MODE_AUTOMATIC   = 0; // < libavcodec will select the mode itself
  FF_SUB_CHARENC_MODE_PRE_DECODER = 1;
  // < the AVPacket data needs to be recoded to UTF-8 before being fed to the decoder, requires iconv
  FF_SUB_CHARENC_MODE_IGNORE = 2; // < neither convert the subtitles, nor check them for valid UTF-8

  // AVCodecContext -> unsigned properties;
  FF_CODEC_PROPERTY_LOSSLESS        = $00000001;
  FF_CODEC_PROPERTY_CLOSED_CAPTIONS = $00000002;

  // AVCodecContext -> int sub_text_format;
  FF_SUB_TEXT_FMT_ASS = 0;
{$IFDEF FF_API_ASS_TIMING}
  FF_SUB_TEXT_FMT_ASS_WITH_TIMINGS = 1;
{$ENDIF}
  AV_SUBTITLE_FLAG_FORCED = $00000001;

type
  (*
    * @defgroup lavc_packet AVPacket
    *
    * Types and functions for working with AVPacket.
    * @{
  *)
  AVPacketSideDataType = ( //
    (*
      * An AV_PKT_DATA_PALETTE side data packet contains exactly AVPALETTE_SIZE
      * bytes worth of palette. This side data signals that a new palette is
      * present.
    *)
    AV_PKT_DATA_PALETTE,

    (*
      * The AV_PKT_DATA_NEW_EXTRADATA is used to notify the codec or the format
      * that the extradata buffer was changed and the receiving side should
      * act upon it appropriately. The new extradata is embedded in the side
      * data buffer and should be immediately used for processing the current
      * frame or packet.
    *)
    AV_PKT_DATA_NEW_EXTRADATA,

    (*
      * An AV_PKT_DATA_PARAM_CHANGE side data packet is laid out as follows:
      * @code
      * u32le param_flags
      * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT)
      *     s32le channel_count
      * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT)
      *     u64le channel_layout
      * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE)
      *     s32le sample_rate
      * if (param_flags & AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS)
      *     s32le width
      *     s32le height
      * @endcode
    *)
    AV_PKT_DATA_PARAM_CHANGE,

    (*
      * An AV_PKT_DATA_H263_MB_INFO side data packet contains a number of
      * structures with info about macroblocks relevant to splitting the
      * packet into smaller packets on macroblock edges (e.g. as for RFC 2190).
      * That is, it does not necessarily contain info about all macroblocks,
      * as long as the distance between macroblocks in the info is smaller
      * than the target payload size.
      * Each MB info structure is 12 bytes, and is laid out as follows:
      * @code
      * u32le bit offset from the start of the packet
      * u8    current quantizer at the start of the macroblock
      * u8    GOB number
      * u16le macroblock address within the GOB
      * u8    horizontal MV predictor
      * u8    vertical MV predictor
      * u8    horizontal MV predictor for block number 3
      * u8    vertical MV predictor for block number 3
      * @endcode
    *)
    AV_PKT_DATA_H263_MB_INFO,

    (*
      * This side data should be associated with an audio stream and contains
      * ReplayGain information in form of the AVReplayGain struct.
    *)
    AV_PKT_DATA_REPLAYGAIN,

    (*
      * This side data contains a 3x3 transformation matrix describing an affine
      * transformation that needs to be applied to the decoded video frames for
      * correct presentation.
      *
      * See libavutil/display.h for a detailed description of the data.
    *)
    AV_PKT_DATA_DISPLAYMATRIX,

    (*
      * This side data should be associated with a video stream and contains
      * Stereoscopic 3D information in form of the AVStereo3D struct.
    *)
    AV_PKT_DATA_STEREO3D,

    (*
      * This side data should be associated with an audio stream and corresponds
      * to enum AVAudioServiceType.
    *)
    AV_PKT_DATA_AUDIO_SERVICE_TYPE,

    (*
      * This side data contains quality related information from the encoder.
      * @code
      * u32le quality factor of the compressed frame. Allowed range is between 1 (good) and FF_LAMBDA_MAX (bad).
      * u8    picture type
      * u8    error count
      * u16   reserved
      * u64le[error count] sum of squared differences between encoder in and output
      * @endcode
    *)
    AV_PKT_DATA_QUALITY_STATS,

    (*
      * This side data contains an integer value representing the stream index
      * of a "fallback" track.  A fallback track indicates an alternate
      * track to use when the current track can not be decoded for some reason.
      * e.g. no decoder available for codec.
    *)
    AV_PKT_DATA_FALLBACK_TRACK,

    (*
      * This side data corresponds to the AVCPBProperties struct.
    *)
    AV_PKT_DATA_CPB_PROPERTIES,

    (*
      * Recommmends skipping the specified number of samples
      * @code
      * u32le number of samples to skip from start of this packet
      * u32le number of samples to skip from end of this packet
      * u8    reason for start skip
      * u8    reason for end   skip (0=padding silence, 1=convergence)
      * @endcode
    *)
    AV_PKT_DATA_SKIP_SAMPLES,

    (*
      * An AV_PKT_DATA_JP_DUALMONO side data packet indicates that
      * the packet may contain "dual mono" audio specific to Japanese DTV
      * and if it is true, recommends only the selected channel to be used.
      * @code
      * u8    selected channels (0=mail/left, 1=sub/right, 2=both)
      * @endcode
    *)
    AV_PKT_DATA_JP_DUALMONO,

    (*
      * A list of zero terminated key/value strings. There is no end marker for
      * the list, so it is required to rely on the side data size to stop.
    *)
    AV_PKT_DATA_STRINGS_METADATA,

    (*
      * Subtitle event position
      * @code
      * u32le x1
      * u32le y1
      * u32le x2
      * u32le y2
      * @endcode
    *)
    AV_PKT_DATA_SUBTITLE_POSITION,

    (*
      * Data found in BlockAdditional element of matroska container. There is
      * no end marker for the data, so it is required to rely on the side data
      * size to recognize the end. 8 byte id (as found in BlockAddId) followed
      * by data.
    *)
    AV_PKT_DATA_MATROSKA_BLOCKADDITIONAL,

    (*
      * The optional first identifier line of a WebVTT cue.
    *)
    AV_PKT_DATA_WEBVTT_IDENTIFIER,

    (*
      * The optional settings (rendering instructions) that immediately
      * follow the timestamp specifier of a WebVTT cue.
    *)
    AV_PKT_DATA_WEBVTT_SETTINGS,

    (*
      * A list of zero terminated key/value strings. There is no end marker for
      * the list, so it is required to rely on the side data size to stop. This
      * side data includes updated metadata which appeared in the stream.
    *)
    AV_PKT_DATA_METADATA_UPDATE,

    (*
      * MPEGTS stream ID as uint8_t, this is required to pass the stream ID
      * information from the demuxer to the corresponding muxer.
    *)
    AV_PKT_DATA_MPEGTS_STREAM_ID,

    (*
      * Mastering display metadata (based on SMPTE-2086:2014). This metadata
      * should be associated with a video stream and contains data in the form
      * of the AVMasteringDisplayMetadata struct.
    *)
    AV_PKT_DATA_MASTERING_DISPLAY_METADATA,

    (*
      * This side data should be associated with a video stream and corresponds
      * to the AVSphericalMapping structure.
    *)
    AV_PKT_DATA_SPHERICAL,

    (*
      * Content light level (based on CTA-861.3). This metadata should be
      * associated with a video stream and contains data in the form of the
      * AVContentLightMetadata struct.
    *)
    AV_PKT_DATA_CONTENT_LIGHT_LEVEL,

    (*
      * ATSC A53 Part 4 Closed Captions. This metadata should be associated with
      * a video stream. A53 CC bitstream is stored as uint8_t in AVPacketSideData.data.
      * The number of bytes of CC data is AVPacketSideData.size.
    *)
    AV_PKT_DATA_A53_CC,

    (*
      * This side data is encryption initialization data.
      * The format is not part of ABI, use av_encryption_init_info_* methods to
      * access.
    *)
    AV_PKT_DATA_ENCRYPTION_INIT_INFO,

    (*
      * This side data contains encryption info for how to decrypt the packet.
      * The format is not part of ABI, use av_encryption_info_* methods to access.
    *)
    AV_PKT_DATA_ENCRYPTION_INFO,

    (*
      * Active Format Description data consisting of a single byte as specified
      * in ETSI TS 101 154 using AVActiveFormatDescription enum.
    *)
    AV_PKT_DATA_AFD,

    (*
      * The number of side data types.
      * This is not part of the public API/ABI in the sense that it may
      * change when new side data types are added.
      * This must stay the last enum value.
      * If its value becomes huge, some code using it
      * needs to be updated as it assumes it to be smaller than other limits.
    *)
    AV_PKT_DATA_NB);

  AVPacketSideData = record
    data: puint8_t;
    size: int;
    _type: AVPacketSideDataType;
  end;

  pAVPacketSideData = ^AVPacketSideData;

  (*
    * This structure stores compressed data. It is typically exported by demuxers
    * and then passed as input to decoders, or received as output from encoders and
    * then passed to muxers.
    *
    * For video, it should typically contain one compressed frame. For audio it may
    * contain several compressed frames. Encoders are allowed to output empty
    * packets, with no compressed data, containing only side data
    * (e.g. to update some stream parameters at the end of encoding).
    *
    * AVPacket is one of the few structs in FFmpeg, whose size is a part of public
    * ABI. Thus it may be allocated on stack and no new fields can be added to it
    * without libavcodec and libavformat major bump.
    *
    * The semantics of data ownership depends on the buf field.
    * If it is set, the packet data is dynamically allocated and is
    * valid indefinitely until a call to av_packet_unref() reduces the
    * reference count to 0.
    *
    * If the buf field is not set av_packet_ref() would make a copy instead
    * of increasing the reference count.
    *
    * The side data is always allocated with av_malloc(), copied by
    * av_packet_ref() and freed by av_packet_unref().
    *
    * @see av_packet_ref
    * @see av_packet_unref
  *)
  pAVPacket = ^AVPacket;

  AVPacket = record
    (*
      * A reference to the reference-counted buffer where the packet data is
      * stored.
      * May be NULL, then the packet data is not reference-counted.
    *)
    buf: pAVBufferRef;
    (*
      * Presentation timestamp in AVStream->time_base units; the time at which
      * the decompressed packet will be presented to the user.
      * Can be AV_NOPTS_VALUE if it is not stored in the file.
      * pts MUST be larger or equal to dts as presentation cannot happen before
      * decompression, unless one wants to view hex dumps. Some formats misuse
      * the terms dts and pts/cts to mean something different. Such timestamps
      * must be converted to true pts/dts before they are stored in AVPacket.
    *)
    pts: int64_t;
    (*
      * Decompression timestamp in AVStream->time_base units; the time at which
      * the packet is decompressed.
      * Can be AV_NOPTS_VALUE if it is not stored in the file.
    *)
    dts: int64_t;
    data: puint8_t;
    size: int;
    stream_index: int;
    (*
      * A combination of AV_PKT_FLAG values
    *)
    flags: int;
    (*
      * Additional packet data that can be provided by the container.
      * Packet can contain several types of side information.
    *)
    side_data: pAVPacketSideData;
    side_data_elems: int;

    (*
      * Duration of this packet in AVStream->time_base units, 0 if unknown.
      * Equals next_pts - this_pts in presentation order.
    *)
    duration: int64_t;

    pos: int64_t; // < byte position in stream, -1 if unknown

{$IFDEF FF_API_CONVERGENCE_DURATION}
    (*
      * @deprecated Same as the duration field, but as int64_t. This was required
      * for Matroska subtitles, whose duration values could overflow when the
      * duration field was still an int.
    *)
    // attribute_deprecated
    convergence_duration: int64_t deprecated;
{$ENDIF}
  end;

  AVSideDataParamChangeFlags = (                      //
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT = $0001,  //
    AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT = $0002, //
    AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE = $0004,    //
    AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS = $0008      //
    );
  (*
    * @}
  *)

  AVCodecInternal = record

  end;

  pAVCodecInternal = ^AVCodecInternal;

  AVFieldOrder = (                                       //
    AV_FIELD_UNKNOWN, AV_FIELD_PROGRESSIVE, AV_FIELD_TT, // < Top coded_first, top displayed first
    AV_FIELD_BB,                                         // < Bottom coded first, bottom displayed first
    AV_FIELD_TB,                                         // < Top coded first, bottom displayed first
    AV_FIELD_BT                                          // < Bottom coded first, top displayed first
    );

  AVCodecDefault = record
  end;

  (*
    * AVCodec.
  *)
  pAVCodec = ^avcodec;
  ppAVCodec = ^pAVCodec;
  pAVCodecContext = ^AVCodecContext;
  ppAVCodecContext = ^pAVCodecContext;
  pAVCodecDefault = ^AVCodecDefault;
  pAVSubtitle = ^AVSubtitle;

  avcodec = record
    (*
      * Name of the codec implementation.
      * The name is globally unique among encoders and among decoders (but an
      * encoder and a decoder can share the same name).
      * This is the primary way to find a codec from the user perspective.
    *)
    name: pAnsiChar;
    (*
      * Descriptive name for the codec, meant to be more human readable than name.
      * You should use the NULL_IF_CONFIG_SMALL() macro to define it.
    *)
    long_name: pAnsiChar;
    _type: AVMediaType;
    id: AVCodecID;
    (*
      * Codec capabilities.
      * see AV_CODEC_CAP_*
    *)
    capabilities: int;
    supported_framerates: pAVRational; // < array of supported framerates, or NULL if any, array is terminated by {0,0}
    pix_fmts: pAVPixelFormat;          // < array of supported pixel formats, or NULL if unknown, array is terminated by -1
    supported_samplerates: pInt;       // < array of supported audio samplerates, or NULL if unknown, array is terminated by 0
    sample_fmts: pAVSampleFormat;      // < array of supported sample formats, or NULL if unknown, array is terminated by -1
    channel_layouts: puint64_t;        // < array of support channel layouts, or NULL if unknown. array is terminated by 0
    max_lowres: uint8_t;               // < maximum value for lowres supported by the decoder
    priv_class: pAVClass;              // < AVClass for the private context
    profiles: pAVProfile;              // < array of recognized profiles, or NULL if unknown, array is terminated by {FF_PROFILE_UNKNOWN}

    (*
      * Group name of the codec implementation.
      * This is a short symbolic name of the wrapper backing this codec. A
      * wrapper uses some kind of external implementation for the codec, such
      * as an external library, or a codec implementation provided by the OS or
      * the hardware.
      * If this field is NULL, this is a builtin, libavcodec native codec.
      * If non-NULL, this will be the suffix in AVCodec.name in most cases
      * (usually AVCodec.name will be of the form "<codec_name>_<wrapper_name>").
    *)
    wrapper_name: pAnsiChar;

    (* ***************************************************************
      * No fields below this line are part of the public API. They
      * may not be used outside of libavcodec and can be changed and
      * removed at will.
      * New public fields should be added right above.
      *****************************************************************
    *)
    priv_data_size: int;
    next: pAVCodec;
    (*
      * @name Frame-level threading support functions
      * @{
    *)
    (*
      * If defined, called on thread contexts when they are created.
      * If the codec allocates writable tables in init(), re-allocate them here.
      * priv_data will be set to a copy of the original.
    *)
    // int (*init_thread_copy)(AVCodecContext *);
    init_thread_copy: function(ctx: pAVCodecContext): int; cdecl;
    (*
      * Copy necessary context variables from a previous thread context to the current one.
      * If not defined, the next thread will start automatically; otherwise, the codec
      * must call ff_thread_finish_setup().
      *
      * dst and src will (rarely) point to the same context, in which case memcpy should be skipped.
    *)
    // int (*update_thread_context)(AVCodecContext *dst, const AVCodecContext *src);
    update_thread_context: function(dst: pAVCodecContext; const src: pAVCodecContext): int; cdecl;

    (*
      * Private codec-specific defaults.
    *)
    defaults: pAVCodecDefault;

    (*
      * Initialize codec static data, called from avcodec_register().
      *
      * This is not intended for time consuming operations as it is
      * run for every codec regardless of that codec being used.
    *)
    // void (*init_static_data)(struct AVCodec *codec);
    init_static_data: procedure(codec: pAVCodec); cdecl;

    // int (*init)(AVCodecContext *);
    init: function(ctx: pAVCodecContext): int; cdecl;
    // int (*encode_sub)(AVCodecContext *, uint8_t *buf, int buf_size, const struct AVSubtitle *sub);
    encode_sub: function(ctx: pAVCodecContext; buf: puint8_t; buf_size: int; const sub: pAVSubtitle): int; cdecl;
    (*
      * Encode data to an AVPacket.
      *
      * @param      avctx          codec context
      * @param      avpkt          output AVPacket (may contain a user-provided buffer)
      * @param[in]  frame          AVFrame containing the raw data to be encoded
      * @param[out] got_packet_ptr encoder sets to 0 or 1 to indicate that a
      *                            non-empty packet was returned in avpkt.
      * @return 0 on success, negative error code on failure
    *)
    // int (*encode2)(AVCodecContext *avctx, AVPacket *avpkt, const AVFrame *frame,int *got_packet_ptr);
    encode2: function(avctx: pAVCodecContext; avpkt: pAVPacket; const frame: pAVFrame; got_packet_ptr: pInt): int; cdecl;
    // int (*decode)(AVCodecContext *, void *outdata, int *outdata_size, AVPacket *avpkt);
    decode: function(cnt: pAVCodecContext; outdata: Pointer; outdata_size: pInt; avpkt: pAVPacket): int; cdecl;
    // int (*close)(AVCodecContext *);
    close: function(ctx: pAVCodecContext): int; cdecl;
    (*
      * Encode API with decoupled packet/frame dataflow. The API is the
      * same as the avcodec_ prefixed APIs (avcodec_send_frame() etc.), except
      * that:
      * - never called if the codec is closed or the wrong type,
      * - if AV_CODEC_CAP_DELAY is not set, drain frames are never sent,
      * - only one drain frame is ever passed down,
    *)
    // int (*send_frame)(AVCodecContext *avctx, const AVFrame *frame);
    send_frame: function(avctx: pAVCodecContext; const frame: pAVFrame): int; cdecl;
    // int (*receive_packet)(AVCodecContext *avctx, AVPacket *avpkt);
    receive_packet: function(avctx: pAVCodecContext; avpkt: pAVPacket): int; cdecl;

    (*
      * Decode API with decoupled packet/frame dataflow. This function is called
      * to get one output frame. It should call ff_decode_get_packet() to obtain
      * input data.
    *)
    // int (*receive_frame)(AVCodecContext *avctx, AVFrame *frame);
    receive_frame: function(avctx: pAVCodecContext; frame: pAVFrame): int; cdecl;
    (*
      * Flush buffers.
      * Will be called when seeking
    *)
    // void (*flush)(AVCodecContext *);
    flush: procedure(ctx: pAVCodecContext); cdecl;
    (*
      * Internal codec capabilities.
      * See FF_CODEC_CAP_* in internal.h
    *)
    caps_internal: int;

    (*
      * Decoding only, a comma-separated list of bitstream filters to apply to
      * packets before decoding.
    *)
    bsfs: pAnsiChar;

    (*
      * Array of pointers to hardware configurations supported by the codec,
      * or NULL if no hardware supported.  The array is terminated by a NULL
      * pointer.
      *
      * The user can only access this field via avcodec_get_hw_config().
    *)
    hw_configs: ppAVCodecHWConfigInternal;
  end;

  TAVCodecContext_execute = function(c2: pAVCodecContext; arg: Pointer): int; cdecl;

  TAVCodecContext_execute2 = function(c2: pAVCodecContext; arg: Pointer; jobnr: int; threadnr: int): int; cdecl;

  (*
    * main external API structure.
    * New fields can be added to the end with minor version bumps.
    * Removal, reordering and changes to existing fields require a major
    * version bump.
    * You can use AVOptions (av_opt* / av_set/get*()) to access these fields from user
    * applications.
    * The name string for AVOptions options matches the associated command line
    * parameter name and can be found in libavcodec/options_table.h
    * The AVOption/command line parameter names differ in some cases from the C
    * structure field names for historic reasons or brevity.
    * sizeof(AVCodecContext) must not be used outside libav*.
  *)
  AVCodecContext = record
    (*
      * information on struct for av_log
      * - set by avcodec_alloc_context3
    *)
    av_class: pAVClass;
    log_level_offset: int;

    codec_type: AVMediaType; (* see AVMEDIA_TYPE_xxx *)
    codec: pAVCodec;
    codec_id: AVCodecID; (* see AV_CODEC_ID_xxx *)

    (*
      * fourcc (LSB first, so "ABCD" -> ('D'shl24) + ('C'shl16) + ('B'shl8) + 'A').
      * This is used to work around some encoder bugs.
      * A demuxer should set this to what is stored in the field used to identify the codec.
      * If there are multiple such fields in a container then the demuxer should choose the one
      * which maximizes the information about the used codec.
      * If the codec tag field in a container is larger than 32 bits then the demuxer should
      * remap the longer ID to 32 bits with a table or other structure. Alternatively a new
      * extra_codec_tag + size could be added but for this a clear advantage must be demonstrated
      * first.
      * - encoding: Set by user, if not then the default based on codec_id will be used.
      * - decoding: Set by user, will be converted to uppercase by libavcodec during init.
    *)
    codec_tag: unsigned;

    priv_data: Pointer;

    (*
      * Private context used for internal data.
      *
      * Unlike priv_data, this is not codec-specific. It is used in general
      * libavcodec functions.
    *)
    internal: pAVCodecInternal;

    (*
      * Private data of the user, can be used to carry app specific stuff.
      * - encoding: Set by user.
      * - decoding: Set by user.
    *)
    opaque: Pointer;

    (*
      * the average bitrate
      * - encoding: Set by user; unused for constant quantizer encoding.
      * - decoding: Set by user, may be overwritten by libavcodec
      *             if this info is available in the stream
    *)
    bit_rate: int64_t;

    (*
      * number of bits the bitstream is allowed to diverge from the reference.
      *           the reference can be CBR (for CBR pass1) or VBR (for pass2)
      * - encoding: Set by user; unused for constant quantizer encoding.
      * - decoding: unused
    *)
    bit_rate_tolerance: int;

    (*
      * Global quality for codecs which cannot change it per frame.
      * This should be proportional to MPEG-1/2/4 qscale.
      * - encoding: Set by user.
      * - decoding: unused
    *)
    global_quality: int;

    (*
      * - encoding: Set by user.
      * - decoding: unused
    *)
    compression_level: int;

    (*
      * AV_CODEC_FLAG_*.
      * - encoding: Set by user.
      * - decoding: Set by user.
    *)
    flags: int;

    (*
      * AV_CODEC_FLAG2_*
      * - encoding: Set by user.
      * - decoding: Set by user.
    *)
    flags2: int;

    (*
      * some codecs need / can use extradata like Huffman tables.
      * MJPEG: Huffman tables
      * rv10: additional flags
      * MPEG-4: global headers (they can be in the bitstream or here)
      * The allocated memory should be AV_INPUT_BUFFER_PADDING_SIZE bytes larger
      * than extradata_size to avoid problems if it is read with the bitstream reader.
      * The bytewise contents of extradata must not depend on the architecture or CPU endianness.
      * Must be allocated with the av_malloc() family of functions.
      * - encoding: Set/allocated/freed by libavcodec.
      * - decoding: Set/allocated/freed by user.
    *)
    extradata: puint8_t;
    extradata_size: int;

    (*
      * This is the fundamental unit of time (in seconds) in terms
      * of which frame timestamps are represented. For fixed-fps content,
      * timebase should be 1/framerate and timestamp increments should be
      * identically 1.
      * This often, but not always is the inverse of the frame rate or field rate
      * for video. 1/time_base is not the average frame rate if the frame rate is not
      * constant.
      *
      * Like containers, elementary streams also can store timestamps, 1/time_base
      * is the unit in which these timestamps are specified.
      * As example of such codec time base see ISO/IEC 14496-2:2001(E)
      * vop_time_increment_resolution and fixed_vop_rate
      * (fixed_vop_rate == 0 implies that it is different from the framerate)
      *
      * - encoding: MUST be set by user.
      * - decoding: the use of this field for decoding is deprecated.
      *             Use framerate instead.
    *)
    time_base: AVRational;

    (*
      * For some codecs, the time base is closer to the field rate than the frame rate.
      * Most notably, H.264 and MPEG-2 specify time_base as half of frame duration
      * if no telecine is used ...
      *
      * Set to time_base ticks per frame. Default 1, e.g., H.264/MPEG-2 set it to 2.
    *)
    ticks_per_frame: int;

    (*
      * Codec delay.
      *
      * Encoding: Number of frames delay there will be from the encoder input to
      *           the decoder output. (we assume the decoder matches the spec)
      * Decoding: Number of frames delay in addition to what a standard decoder
      *           as specified in the spec would produce.
      *
      * Video:
      *   Number of frames the decoded output will be delayed relative to the
      *   encoded input.
      *
      * Audio:
      *   For encoding, this field is unused (see initial_padding).
      *
      *   For decoding, this is the number of samples the decoder needs to
      *   output before the decoder's output is valid. When seeking, you should
      *   start decoding this many samples prior to your desired seek point.
      *
      * - encoding: Set by libavcodec.
      * - decoding: Set by libavcodec.
    *)
    delay: int;

    (* video only *)
    (*
      * picture width / height.
      *
      * @note Those fields may not match the values of the last
      * AVFrame output by avcodec_decode_video2 due frame
      * reordering.
      *
      * - encoding: MUST be set by user.
      * - decoding: May be set by the user before opening the decoder if known e.g.
      *             from the container. Some decoders will require the dimensions
      *             to be set by the caller. During decoding, the decoder may
      *             overwrite those values as required while parsing the data.
    *)
    width, height: int;

    (*
      * Bitstream width / height, may be different from width/height e.g. when
      * the decoded frame is cropped before being output or lowres is enabled.
      *
      * @note Those field may not match the value of the last
      * AVFrame output by avcodec_receive_frame() due frame
      * reordering.
      *
      * - encoding: unused
      * - decoding: May be set by the user before opening the decoder if known
      *             e.g. from the container. During decoding, the decoder may
      *             overwrite those values as required while parsing the data.
    *)
    coded_width, coded_height: int;

    (*
      * the number of pictures in a group of pictures, or 0 for intra_only
      * - encoding: Set by user.
      * - decoding: unused
    *)
    gop_size: int;

    (*
      * Pixel format, see AV_PIX_FMT_xxx.
      * May be set by the demuxer if known from headers.
      * May be overridden by the decoder if it knows better.
      *
      * @note This field may not match the value of the last
      * AVFrame output by avcodec_receive_frame() due frame
      * reordering.
      *
      * - encoding: Set by user.
      * - decoding: Set by user if known, overridden by libavcodec while
      *             parsing the data.
    *)
    pix_fmt: AVPixelFormat;

    (*
      * If non NULL, 'draw_horiz_band' is called by the libavcodec
      * decoder to draw a horizontal band. It improves cache usage. Not
      * all codecs can do that. You must check the codec capabilities
      * beforehand.
      * When multithreading is used, it may be called from multiple threads
      * at the same time; threads might draw different parts of the same AVFrame,
      * or multiple AVFrames, and there is no guarantee that slices will be drawn
      * in order.
      * The function is also used by hardware acceleration APIs.
      * It is called at least once during frame decoding to pass
      * the data needed for hardware render.
      * In that mode instead of pixel data, AVFrame points to
      * a structure specific to the acceleration API. The application
      * reads the structure and can change some fields to indicate progress
      * or mark state.
      * - encoding: unused
      * - decoding: Set by user.
      * @param height the height of the slice
      * @param y the y position of the slice
      * @param type 1->top field, 2->bottom field, 3->frame
      * @param offset offset into the AVFrame.data from which the slice should be read
    *)
    // void (*draw_horiz_band)(struct AVCodecContext *s,
    // const AVFrame *src, int offset[AV_NUM_DATA_POINTERS],
    // int y, int type, int height);
    draw_horiz_band: procedure(s: pAVCodecContext; const src: pAVFrame; offset: pAVNDPArray; y, _type, height: int); cdecl;

    (*
      * callback to negotiate the pixelFormat
      * @param fmt is the list of formats which are supported by the codec,
      * it is terminated by -1 as 0 is a valid format, the formats are ordered by quality.
      * The first is always the native one.
      * @note The callback may be called again immediately if initialization for
      * the selected (hardware-accelerated) pixel format failed.
      * @warning Behavior is undefined if the callback returns a value not
      * in the fmt list of formats.
      * @return the chosen format
      * - encoding: unused
      * - decoding: Set by user, if not set the native format will be chosen.
    *)
    // enum AVPixelFormat (*get_format)(struct AVCodecContext *s, const enum AVPixelFormat * fmt);
    get_format: procedure(s: pAVCodecContext; const fmt: pAVPixelFormat); cdecl;

    (*
      * maximum number of B-frames between non-B-frames
      * Note: The output will be delayed by max_b_frames+1 relative to the input.
      * - encoding: Set by user.
      * - decoding: unused
    *)
    max_b_frames: int;

    (*
      * qscale factor between IP and B-frames
      * If > 0 then the last P-frame quantizer will be used (q= lastp_q*factor+offset).
      * If < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset).
      * - encoding: Set by user.
      * - decoding: unused
    *)
    b_quant_factor: float;

{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    b_frame_strategy: int deprecated;
{$ENDIF}
    (*
      * qscale offset between IP and B-frames
      * - encoding: Set by user.
      * - decoding: unused
    *)
    b_quant_offset: float;

    (*
      * Size of the frame reordering buffer in the decoder.
      * For MPEG-2 it is 1 IPB or 0 low delay IP.
      * - encoding: Set by libavcodec.
      * - decoding: Set by libavcodec.
    *)
    has_b_frames: int;

{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    mpeg_quant: int deprecated;
{$ENDIF}
    (*
      * qscale factor between P- and I-frames
      * If > 0 then the last P-frame quantizer will be used (q = lastp_q * factor + offset).
      * If < 0 then normal ratecontrol will be done (q= -normal_q*factor+offset).
      * - encoding: Set by user.
      * - decoding: unused
    *)
    i_quant_factor: float;

    (*
      * qscale offset between P and I-frames
      * - encoding: Set by user.
      * - decoding: unused
    *)
    i_quant_offset: float;

    (*
      * luminance masking (0-> disabled)
      * - encoding: Set by user.
      * - decoding: unused
    *)
    lumi_masking: float;

    (*
      * temporary complexity masking (0-> disabled)
      * - encoding: Set by user.
      * - decoding: unused
    *)
    temporal_cplx_masking: float;

    (*
      * spatial complexity masking (0-> disabled)
      * - encoding: Set by user.
      * - decoding: unused
    *)
    spatial_cplx_masking: float;

    (*
      * p block masking (0-> disabled)
      * - encoding: Set by user.
      * - decoding: unused
    *)
    p_masking: float;

    (*
      * darkness masking (0-> disabled)
      * - encoding: Set by user.
      * - decoding: unused
    *)
    dark_masking: float;

    (*
      * slice count
      * - encoding: Set by libavcodec.
      * - decoding: Set by user (or 0).
    *)
    slice_count: int;

{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    prediction_method: int deprecated;
{$ENDIF}
    (*
      * slice offsets in the frame in bytes
      * - encoding: Set/allocated by libavcodec.
      * - decoding: Set/allocated by user (or NULL).
    *)
    slice_offset: pInt;

    (*
      * sample aspect ratio (0 if unknown)
      * That is the width of a pixel divided by the height of the pixel.
      * Numerator and denominator must be relatively prime and smaller than 256 for some video standards.
      * - encoding: Set by user.
      * - decoding: Set by libavcodec.
    *)
    sample_aspect_ratio: AVRational;

    (*
      * motion estimation comparison function
      * - encoding: Set by user.
      * - decoding: unused
    *)
    me_cmp: int;
    (*
      * subpixel motion estimation comparison function
      * - encoding: Set by user.
      * - decoding: unused
    *)
    me_sub_cmp: int;
    (*
      * macroblock comparison function (not supported yet)
      * - encoding: Set by user.
      * - decoding: unused
    *)
    mb_cmp: int;
    (*
      * interlaced DCT comparison function
      * - encoding: Set by user.
      * - decoding: unused
    *)
    ildct_cmp: int;

    (*
      * ME diamond size & shape
      * - encoding: Set by user.
      * - decoding: unused
    *)
    dia_size: int;

    (*
      * amount of previous MV predictors (2a+1 x 2a+1 square)
      * - encoding: Set by user.
      * - decoding: unused
    *)
    last_predictor_count: int;

{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    pre_me: int deprecated;
{$ENDIF}
    (*
      * motion estimation prepass comparison function
      * - encoding: Set by user.
      * - decoding: unused
    *)
    me_pre_cmp: int;

    (*
      * ME prepass diamond size & shape
      * - encoding: Set by user.
      * - decoding: unused
    *)
    pre_dia_size: int;

    (*
      * subpel ME quality
      * - encoding: Set by user.
      * - decoding: unused
    *)
    me_subpel_quality: int;

    (*
      * maximum motion estimation search range in subpel units
      * If 0 then no limit.
      *
      * - encoding: Set by user.
      * - decoding: unused
    *)
    me_range: int;

    (*
      * slice flags
      * - encoding: unused
      * - decoding: Set by user.
    *)
    slice_flags: int;

    (*
      * macroblock decision mode
      * - encoding: Set by user.
      * - decoding: unused
    *)
    mb_decision: int;

    (*
      * custom intra quantization matrix
      * Must be allocated with the av_malloc() family of functions, and will be freed in
      * avcodec_free_context().
      * - encoding: Set/allocated by user, freed by libavcodec. Can be NULL.
      * - decoding: Set/allocated/freed by libavcodec.
    *)
    intra_matrix: puint16_t;

    (*
      * custom inter quantization matrix
      * Must be allocated with the av_malloc() family of functions, and will be freed in
      * avcodec_free_context().
      * - encoding: Set/allocated by user, freed by libavcodec. Can be NULL.
      * - decoding: Set/allocated/freed by libavcodec.
    *)
    inter_matrix: puint16_t;

{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    scenechange_threshold: int deprecated;

    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    noise_reduction: int deprecated;
{$ENDIF}
    (*
      * precision of the intra DC coefficient - 8
      * - encoding: Set by user.
      * - decoding: Set by libavcodec
    *)
    intra_dc_precision: int;

    (*
      * Number of macroblock rows at the top which are skipped.
      * - encoding: unused
      * - decoding: Set by user.
    *)
    skip_top: int;

    (*
      * Number of macroblock rows at the bottom which are skipped.
      * - encoding: unused
      * - decoding: Set by user.
    *)
    skip_bottom: int;

    (*
      * minimum MB Lagrange multiplier
      * - encoding: Set by user.
      * - decoding: unused
    *)
    mb_lmin: int;

    (*
      * maximum MB Lagrange multiplier
      * - encoding: Set by user.
      * - decoding: unused
    *)
    mb_lmax: int;

{$IFDEF FF_API_PRIVATE_OPT}
    (*
      * @deprecated use encoder private options instead
    *)
    // attribute_deprecated
    me_penalty_compensation: int deprecated;
{$ENDIF}
    (*
      * - encoding: Set by user.
      * - decoding: unused
    *)
    bidir_refine: int;

{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    brd_scale: int deprecated;
{$ENDIF}
    (*
      * minimum GOP size
      * - encoding: Set by user.
      * - decoding: unused
    *)
    keyint_min: int;

    (*
      * number of reference frames
      * - encoding: Set by user.
      * - decoding: Set by lavc.
    *)
    refs: int;

{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    chromaoffset: int;
{$ENDIF}
    (*
      * Note: Value depends upon the compare function used for fullpel ME.
      * - encoding: Set by user.
      * - decoding: unused
    *)
    mv0_threshold: int;

{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    b_sensitivity: int deprecated;
{$ENDIF}
    (*
      * Chromaticity coordinates of the source primaries.
      * - encoding: Set by user
      * - decoding: Set by libavcodec
    *)
    color_primaries: AVColorPrimaries;

    (*
      * Color Transfer Characteristic.
      * - encoding: Set by user
      * - decoding: Set by libavcodec
    *)
    color_trc: AVColorTransferCharacteristic;

    (*
      * YUV colorspace type.
      * - encoding: Set by user
      * - decoding: Set by libavcodec
    *)
    colorspace: AVColorSpace;

    (*
      * MPEG vs JPEG YUV range.
      * - encoding: Set by user
      * - decoding: Set by libavcodec
    *)
    color_range: AVColorRange;

    (*
      * This defines the location of chroma samples.
      * - encoding: Set by user
      * - decoding: Set by libavcodec
    *)
    chroma_sample_location: AVChromaLocation;

    (*
      * Number of slices.
      * Indicates number of picture subdivisions. Used for parallelized
      * decoding.
      * - encoding: Set by user
      * - decoding: unused
    *)
    slices: int;

    (* Field order
      * - encoding: set by libavcodec
      * - decoding: Set by user.
    *)
    field_order: AVFieldOrder;

    (* audio only *)
    sample_rate: int; // < samples per second
    channels: int;    // < number of audio channels

    (*
      * audio sample format
      * - encoding: Set by user.
      * - decoding: Set by libavcodec.
    *)
    sample_fmt: AVSampleFormat; // < sample format

    (* The following data should not be initialized. *)
    (*
      * Number of samples per channel in an audio frame.
      *
      * - encoding: set by libavcodec in avcodec_open2(). Each submitted frame
      *   except the last must contain exactly frame_size samples per channel.
      *   May be 0 when the codec has AV_CODEC_CAP_VARIABLE_FRAME_SIZE set, then the
      *   frame size is not restricted.
      * - decoding: may be set by some decoders to indicate constant frame size
    *)
    frame_size: int;

    (*
      * Frame counter, set by libavcodec.
      *
      * - decoding: total number of frames returned from the decoder so far.
      * - encoding: total number of frames passed to the encoder so far.
      *
      *   @note the counter is not incremented if encoding/decoding resulted in
      *   an error.
    *)
    frame_number: int;

    (*
      * number of bytes per packet if constant and known or 0
      * Used by some WAV based audio codecs.
    *)
    block_align: int;

    (*
      * Audio cutoff bandwidth (0 means "automatic")
      * - encoding: Set by user.
      * - decoding: unused
    *)
    cutoff: int;

    (*
      * Audio channel layout.
      * - encoding: set by user.
      * - decoding: set by user, may be overwritten by libavcodec.
    *)
    channel_layout: uint64_t;

    (*
      * Request decoder to use this channel layout if it can (0 for default)
      * - encoding: unused
      * - decoding: Set by user.
    *)
    request_channel_layout: uint64_t;

    (*
      * Type of service that the audio stream conveys.
      * - encoding: Set by user.
      * - decoding: Set by libavcodec.
    *)
    audio_service_type: AVAudioServiceType;

    (*
      * desired sample format
      * - encoding: Not used.
      * - decoding: Set by user.
      * Decoder will decode to this format if it can.
    *)
    request_sample_fmt: AVSampleFormat;

    (*
      * This callback is called at the beginning of each frame to get data
      * buffer(s) for it. There may be one contiguous buffer for all the data or
      * there may be a buffer per each data plane or anything in between. What
      * this means is, you may set however many entries in buf[] you feel necessary.
      * Each buffer must be reference-counted using the AVBuffer API (see description
      * of buf[] below).
      *
      * The following fields will be set in the frame before this callback is
      * called:
      * - format
      * - width, height (video only)
      * - sample_rate, channel_layout, nb_samples (audio only)
      * Their values may differ from the corresponding values in
      * AVCodecContext. This callback must use the frame values, not the codec
      * context values, to calculate the required buffer size.
      *
      * This callback must fill the following fields in the frame:
      * - data[]
      * - linesize[]
      * - extended_data:
      *   * if the data is planar audio with more than 8 channels, then this
      *     callback must allocate and fill extended_data to contain all pointers
      *     to all data planes. data[] must hold as many pointers as it can.
      *     extended_data must be allocated with av_malloc() and will be freed in
      *     av_frame_unref().
      *   * otherwise extended_data must point to data
      * - buf[] must contain one or more pointers to AVBufferRef structures. Each of
      *   the frame's data and extended_data pointers must be contained in these. That
      *   is, one AVBufferRef for each allocated chunk of memory, not necessarily one
      *   AVBufferRef per data[] entry. See: av_buffer_create(), av_buffer_alloc(),
      *   and av_buffer_ref().
      * - extended_buf and nb_extended_buf must be allocated with av_malloc() by
      *   this callback and filled with the extra buffers if there are more
      *   buffers than buf[] can hold. extended_buf will be freed in
      *   av_frame_unref().
      *
      * If AV_CODEC_CAP_DR1 is not set then get_buffer2() must call
      * avcodec_default_get_buffer2() instead of providing buffers allocated by
      * some other means.
      *
      * Each data plane must be aligned to the maximum required by the target
      * CPU.
      *
      * @see avcodec_default_get_buffer2()
      *
      * Video:
      *
      * If AV_GET_BUFFER_FLAG_REF is set in flags then the frame may be reused
      * (read and/or written to if it is writable) later by libavcodec.
      *
      * avcodec_align_dimensions2() should be used to find the required width and
      * height, as they normally need to be rounded up to the next multiple of 16.
      *
      * Some decoders do not support linesizes changing between frames.
      *
      * If frame multithreading is used and thread_safe_callbacks is set,
      * this callback may be called from a different thread, but not from more
      * than one at once. Does not need to be reentrant.
      *
      * @see avcodec_align_dimensions2()
      *
      * Audio:
      *
      * Decoders request a buffer of a particular size by setting
      * AVFrame.nb_samples prior to calling get_buffer2(). The decoder may,
      * however, utilize only part of the buffer by setting AVFrame.nb_samples
      * to a smaller value in the output frame.
      *
      * As a convenience, av_samples_get_buffer_size() and
      * av_samples_fill_arrays() in libavutil may be used by custom get_buffer2()
      * functions to find the required data size and to fill data pointers and
      * linesize. In AVFrame.linesize, only linesize[0] may be set for audio
      * since all planes must be the same size.
      *
      * @see av_samples_get_buffer_size(), av_samples_fill_arrays()
      *
      * - encoding: unused
      * - decoding: Set by libavcodec, user can override.
    *)
    // int (*get_buffer2)(struct AVCodecContext *s, AVFrame *frame, int flags);
    get_buffer2: function(s: pAVCodecContext; frame: pAVFrame; flags: int): int; cdecl;

    (*
      * If non-zero, the decoded audio and video frames returned from
      * avcodec_decode_video2() and avcodec_decode_audio4() are reference-counted
      * and are valid indefinitely. The caller must free them with
      * av_frame_unref() when they are not needed anymore.
      * Otherwise, the decoded frames must not be freed by the caller and are
      * only valid until the next decode call.
      *
      * This is always automatically enabled if avcodec_receive_frame() is used.
      *
      * - encoding: unused
      * - decoding: set by the caller before avcodec_open2().
    *)
    // attribute_deprecated
    refcounted_frames: int deprecated;

    (* - encoding parameters *)
    qcompress: float; // < amount of qscale change between easy & hard scenes (0.0-1.0)
    qblur: float;     // < amount of qscale smoothing over time (0.0-1.0)

    (*
      * minimum quantizer
      * - encoding: Set by user.
      * - decoding: unused
    *)
    qmin: int;

    (*
      * maximum quantizer
      * - encoding: Set by user.
      * - decoding: unused
    *)
    qmax: int;

    (*
      * maximum quantizer difference between frames
      * - encoding: Set by user.
      * - decoding: unused
    *)
    max_qdiff: int;

    (*
      * decoder bitstream buffer size
      * - encoding: Set by user.
      * - decoding: unused
    *)
    rc_buffer_size: int;

    (*
      * ratecontrol override, see RcOverride
      * - encoding: Allocated/set/freed by user.
      * - decoding: unused
    *)
    rc_override_count: int;
    rc_override: pRcOverride;

    (*
      * maximum bitrate
      * - encoding: Set by user.
      * - decoding: Set by user, may be overwritten by libavcodec.
    *)
    rc_max_rate: int64_t;

    (*
      * minimum bitrate
      * - encoding: Set by user.
      * - decoding: unused
    *)
    rc_min_rate: int64_t;

    (*
      * Ratecontrol attempt to use, at maximum, <value> of what can be used without an underflow.
      * - encoding: Set by user.
      * - decoding: unused.
    *)
    rc_max_available_vbv_use: float;

    (*
      * Ratecontrol attempt to use, at least, <value> times the amount needed to prevent a vbv overflow.
      * - encoding: Set by user.
      * - decoding: unused.
    *)
    rc_min_vbv_overflow_use: float;

    (*
      * Number of bits which should be loaded into the rc buffer before decoding starts.
      * - encoding: Set by user.
      * - decoding: unused
    *)
    rc_initial_buffer_occupancy: int;

{$IFDEF FF_API_CODER_TYPE}
    (*
      * @deprecated use encoder private options instead
    *)
    // attribute_deprecated
    coder_type: int deprecated;
{$ENDIF} (* FF_API_CODER_TYPE *)
{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    context_model: int deprecated;
{$ENDIF}
{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    frame_skip_threshold: int deprecated;

    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    frame_skip_factor: int;

    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    frame_skip_exp: int deprecated;

    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    frame_skip_cmp: int deprecated;
{$ENDIF} (* FF_API_PRIVATE_OPT *)

    (*
      * trellis RD quantization
      * - encoding: Set by user.
      * - decoding: unused
    *)
    trellis: int;

{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    min_prediction_order: int deprecated;

    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    max_prediction_order: int deprecated;

    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    timecode_frame_start: int64_t deprecated;
{$ENDIF}
{$IFDEF FF_API_RTP_CALLBACK}
    (*
      * @deprecated unused
    *)
    (* The RTP callback: This function is called *)
    (* every time the encoder has a packet to send. *)
    (* It depends on the encoder if the data starts *)
    (* with a Start Code (it should). H.263 does. *)
    (* mb_nb contains the number of macroblocks *)
    (* encoded in the RTP payload. *)
    // attribute_deprecated
    // void (*rtp_callback)(struct AVCodecContext *avctx, void *data, int size, int mb_nb);
    rtp_callback: procedure(avctx: pAVCodecContext; data: Pointer; size: int; mb_nb: int); cdecl;
{$ENDIF}
{$IFDEF FF_API_PRIVATE_OPT}
    (* @deprecated use encoder private options instead *)
    // attribute_deprecated
    rtp_payload_size: int deprecated; (* The size of the RTP payload: the coder will *)
    (* do its best to deliver a chunk with size *)
    (* below rtp_payload_size, the chunk will start *)
    (* with a start code on some codecs like H.263. *)
    (* This doesn't take account of any particular *)
    (* headers inside the transmitted RTP payload. *)
{$ENDIF}
{$IFDEF FF_API_STAT_BITS}
    (* statistics, used for 2-pass encoding *)
    // attribute_deprecated
    mv_bits: int deprecated;
    // attribute_deprecated
    header_bits: int deprecated;
    // attribute_deprecated
    i_tex_bits: int deprecated;
    // attribute_deprecated
    p_tex_bits: int deprecated;
    // attribute_deprecated
    i_count: int deprecated;
    // attribute_deprecated
    p_count: int deprecated;
    // attribute_deprecated
    skip_count: int deprecated;
    // attribute_deprecated
    misc_bits: int deprecated;

    (* @deprecated this field is unused *)
    // attribute_deprecated
    frame_bits: int deprecated;
{$ENDIF}
    (*
      * pass1 encoding statistics output buffer
      * - encoding: Set by libavcodec.
      * - decoding: unused
    *)
    stats_out: pAnsiChar;

    (*
      * pass2 encoding statistics input buffer
      * Concatenated stuff from stats_out of pass1 should be placed here.
      * - encoding: Allocated/set/freed by user.
      * - decoding: unused
    *)
    stats_in: pAnsiChar;

    (*
      * Work around bugs in encoders which sometimes cannot be detected automatically.
      * - encoding: Set by user
      * - decoding: Set by user
    *)
    workaround_bugs: int;

    (*
      * strictly follow the standard (MPEG-4, ...).
      * - encoding: Set by user.
      * - decoding: Set by user.
      * Setting this to STRICT or higher means the encoder and decoder will
      * generally do stupid things, whereas setting it to unofficial or lower
      * will mean the encoder might produce output that is not supported by all
      * spec-compliant decoders. Decoders don't differentiate between normal,
      * unofficial and experimental (that is, they always try to decode things
      * when they can) unless they are explicitly asked to behave stupidly
      * (=strictly conform to the specs)
    *)
    strict_std_compliance: int;

    (*
      * error concealment flags
      * - encoding: unused
      * - decoding: Set by user.
    *)
    error_concealment: int;

    (*
      * debug
      * - encoding: Set by user.
      * - decoding: Set by user.
    *)
    debug: int;

{$IFDEF FF_API_DEBUG_MV}
    (*
      * debug
      * - encoding: Set by user.
      * - decoding: Set by user.
    *)
    debug_mv: int;
{$ENDIF}
    (*
      * Error recognition; may misdetect some more or less valid parts as errors.
      * - encoding: unused
      * - decoding: Set by user.
    *)
    err_recognition: int;
    (*
      * opaque 64-bit number (generally a PTS) that will be reordered and
      * output in AVFrame.reordered_opaque
      * - encoding: Set by libavcodec to the reordered_opaque of the input
      *             frame corresponding to the last returned packet. Only
      *             supported by encoders with the
      *             AV_CODEC_CAP_ENCODER_REORDERED_OPAQUE capability.
      * - decoding: Set by user.
    *)
    reordered_opaque: int64_t;
    (*
      * Hardware accelerator in use
      * - encoding: unused.
      * - decoding: Set by libavcodec
    *)
    hwaccel: pAVHWAccel;

    (*
      * Hardware accelerator context.
      * For some hardware accelerators, a global context needs to be
      * provided by the user. In that case, this holds display-dependent
      * data FFmpeg cannot instantiate itself. Please refer to the
      * FFmpeg HW accelerator documentation to know how to fill this
      * is. e.g. for VA API, this is a struct vaapi_context.
      * - encoding: unused
      * - decoding: Set by user
    *)
    hwaccel_context: Pointer;

    (*
      * error
      * - encoding: Set by libavcodec if flags & AV_CODEC_FLAG_PSNR.
      * - decoding: unused
    *)
    // uint64_t error[AV_NUM_DATA_POINTERS];
    error: TAVNDPArray_uint64_t;

    (*
      * DCT algorithm, see FF_DCT_* below
      * - encoding: Set by user.
      * - decoding: unused
    *)
    dct_algo: int;

    (*
      * IDCT algorithm, see FF_IDCT_* below.
      * - encoding: Set by user.
      * - decoding: Set by user.
    *)
    idct_algo: int;

    (*
      * bits per sample/pixel from the demuxer (needed for huffyuv).
      * - encoding: Set by libavcodec.
      * - decoding: Set by user.
    *)
    bits_per_coded_sample: int;

    (*
      * Bits per sample/pixel of internal libavcodec pixel/sample format.
      * - encoding: set by user.
      * - decoding: set by libavcodec.
    *)
    bits_per_raw_sample: int;

{$IFDEF FF_API_LOWRES}
    (*
      * low resolution decoding, 1-> 1/2 size, 2->1/4 size
      * - encoding: unused
      * - decoding: Set by user.
    *)
    lowres: int;
{$ENDIF}
{$IFDEF FF_API_CODED_FRAME}
    (*
      * the picture in the bitstream
      * - encoding: Set by libavcodec.
      * - decoding: unused
      *
      * @deprecated use the quality factor packet side data instead
    *)
    // attribute_deprecated
    coded_frame: pAVFrame deprecated;
{$ENDIF}
    (*
      * thread count
      * is used to decide how many independent tasks should be passed to execute()
      * - encoding: Set by user.
      * - decoding: Set by user.
    *)
    thread_count: int;

    (*
      * Which multithreading methods to use.
      * Use of FF_THREAD_FRAME will increase decoding delay by one frame per thread,
      * so clients which cannot provide future frames should not use it.
      *
      * - encoding: Set by user, otherwise the default is used.
      * - decoding: Set by user, otherwise the default is used.
    *)
    thread_type: int;

    (*
      * Which multithreading methods are in use by the codec.
      * - encoding: Set by libavcodec.
      * - decoding: Set by libavcodec.
    *)
    active_thread_type: int;

    (*
      * Set by the client if its custom get_buffer() callback can be called
      * synchronously from another thread, which allows faster multithreaded decoding.
      * draw_horiz_band() will be called from other threads regardless of this setting.
      * Ignored if the default get_buffer() is used.
      * - encoding: Set by user.
      * - decoding: Set by user.
    *)
    thread_safe_callbacks: int;

    (*
      * The codec may call this to execute several independent things.
      * It will return only after finishing all tasks.
      * The user may replace this with some multithreaded implementation,
      * the default implementation will execute the parts serially.
      * @param count the number of things to execute
      * - encoding: Set by libavcodec, user can override.
      * - decoding: Set by libavcodec, user can override.
    *)
    // int (* execute)(struct AVCodecContext *c, int (*func)(struct AVCodecContext *c2, void *arg), void *arg2, int *ret, int count, int size);
    execute: function(c: pAVCodecContext; func: TAVCodecContext_execute; arg2: Pointer; ret: pInt; count: int; size: int): int; cdecl;

    (*
      * The codec may call this to execute several independent things.
      * It will return only after finishing all tasks.
      * The user may replace this with some multithreaded implementation,
      * the default implementation will execute the parts serially.
      * Also see avcodec_thread_init and e.g. the --enable-pthread configure option.
      * @param c context passed also to func
      * @param count the number of things to execute
      * @param arg2 argument passed unchanged to func
      * @param ret return values of executed functions, must have space for "count" values. May be NULL.
      * @param func function that will be called count times, with jobnr from 0 to count-1.
      *             threadnr will be in the range 0 to c->thread_count-1 < MAX_THREADS and so that no
      *             two instances of func executing at the same time will have the same threadnr.
      * @return always 0 currently, but code should handle a future improvement where when any call to func
      *         returns < 0 no further calls to func may be done and < 0 is returned.
      * - encoding: Set by libavcodec, user can override.
      * - decoding: Set by libavcodec, user can override.
    *)
    // int (* execute2)(struct AVCodecContext *c, int (*func)(struct AVCodecContext *c2, void *arg, int jobnr, int threadnr), void *arg2, int *ret, int count);
    execute2: function(c: pAVCodecContext; func: TAVCodecContext_execute2; arg2: Pointer; ret: pInt; count: int): int; cdecl;

    (*
      * noise vs. sse weight for the nsse comparison function
      * - encoding: Set by user.
      * - decoding: unused
    *)
    nsse_weight: int;

    (*
      * profile
      * - encoding: Set by user.
      * - decoding: Set by libavcodec.
    *)
    profile: int;

    (*
      * level
      * - encoding: Set by user.
      * - decoding: Set by libavcodec.
    *)
    level: int;

    (*
      * Skip loop filtering for selected frames.
      * - encoding: unused
      * - decoding: Set by user.
    *)
    skip_loop_filter: AVDiscard;

    (*
      * Skip IDCT/dequantization for selected frames.
      * - encoding: unused
      * - decoding: Set by user.
    *)
    skip_idct: AVDiscard;

    (*
      * Skip decoding for selected frames.
      * - encoding: unused
      * - decoding: Set by user.
    *)
    skip_frame: AVDiscard;

    (*
      * Header containing style information for text subtitles.
      * For SUBTITLE_ASS subtitle type, it should contain the whole ASS
      * [Script Info] and [V4+ Styles] section, plus the [Events] line and
      * the Format line following. It shouldn't include any Dialogue line.
      * - encoding: Set/allocated/freed by user (before avcodec_open2())
      * - decoding: Set/allocated/freed by libavcodec (by avcodec_open2())
    *)
    subtitle_header: puint8_t;
    subtitle_header_size: int;

{$IFDEF FF_API_VBV_DELAY}
    (*
      * VBV delay coded in the last frame (in periods of a 27 MHz clock).
      * Used for compliant TS muxing.
      * - encoding: Set by libavcodec.
      * - decoding: unused.
      * @deprecated this value is now exported as a part of
      * AV_PKT_DATA_CPB_PROPERTIES packet side data
    *)
    // attribute_deprecated
    vbv_delay: uint64_t deprecated;
{$ENDIF}
{$IFDEF FF_API_SIDEDATA_ONLY_PKT}
    (*
      * Encoding only and set by default. Allow encoders to output packets
      * that do not contain any encoded data, only side data.
      *
      * Some encoders need to output such packets, e.g. to update some stream
      * parameters at the end of encoding.
      *
      * @deprecated this field disables the default behaviour and
      *             it is kept only for compatibility.
    *)
    // attribute_deprecated
    side_data_only_packets: int deprecated;
{$ENDIF}
    (*
      * Audio only. The number of "priming" samples (padding) inserted by the
      * encoder at the beginning of the audio. I.e. this number of leading
      * decoded samples must be discarded by the caller to get the original audio
      * without leading padding.
      *
      * - decoding: unused
      * - encoding: Set by libavcodec. The timestamps on the output packets are
      *             adjusted by the encoder so that they always refer to the
      *             first sample of the data actually contained in the packet,
      *             including any added padding.  E.g. if the timebase is
      *             1/samplerate and the timestamp of the first input sample is
      *             0, the timestamp of the first output packet will be
      *             -initial_padding.
    *)
    initial_padding: int;

    (*
      * - decoding: For codecs that store a framerate value in the compressed
      *             bitstream, the decoder may export it here. { 0, 1} when
      *             unknown.
      * - encoding: May be used to signal the framerate of CFR content to an
      *             encoder.
    *)
    framerate: AVRational;

    (*
      * Nominal unaccelerated pixel format, see AV_PIX_FMT_xxx.
      * - encoding: unused.
      * - decoding: Set by libavcodec before calling get_format()
    *)
    sw_pix_fmt: AVPixelFormat;

    (*
      * Timebase in which pkt_dts/pts and AVPacket.dts/pts are.
      * - encoding unused.
      * - decoding set by user.
    *)
    pkt_timebase: AVRational;

    (*
      * AVCodecDescriptor
      * - encoding: unused.
      * - decoding: set by libavcodec.
    *)
    codec_descriptor: pAVCodecDescriptor;

{$IFNDEF FF_API_LOWRES}
    (*
      * low resolution decoding, 1-> 1/2 size, 2->1/4 size
      * - encoding: unused
      * - decoding: Set by user.
    *)
    lowres: int;
{$ENDIF}
    (*
      * Current statistics for PTS correction.
      * - decoding: maintained and used by libavcodec, not intended to be used by user apps
      * - encoding: unused
    *)
    pts_correction_num_faulty_pts: int64_t; // Number of incorrect PTS values so far
    pts_correction_num_faulty_dts: int64_t; // Number of incorrect DTS values so far
    pts_correction_last_pts: int64_t;       // PTS of the last frame
    pts_correction_last_dts: int64_t;       // DTS of the last frame

    (*
      * Character encoding of the input subtitles file.
      * - decoding: set by user
      * - encoding: unused
    *)
    sub_charenc: pAnsiChar;

    (*
      * Subtitles character encoding mode. Formats or codecs might be adjusting
      * this setting (if they are doing the conversion themselves for instance).
      * - decoding: set by libavcodec
      * - encoding: unused
    *)
    sub_charenc_mode: int;

    (*
      * Skip processing alpha if supported by codec.
      * Note that if the format uses pre-multiplied alpha (common with VP6,
      * and recommended due to better video quality/compression)
      * the image will look as if alpha-blended onto a black background.
      * However for formats that do not use pre-multiplied alpha
      * there might be serious artefacts (though e.g. libswscale currently
      * assumes pre-multiplied alpha anyway).
      *
      * - decoding: set by user
      * - encoding: unused
    *)
    skip_alpha: int;

    (*
      * Number of samples to skip after a discontinuity
      * - decoding: unused
      * - encoding: set by libavcodec
    *)
    seek_preroll: int;

{$IFNDEF FF_API_DEBUG_MV}
    (*
      * debug motion vectors
      * - encoding: Set by user.
      * - decoding: Set by user.
    *)
    debug_mv: int;
{$ENDIF}
    (*
      * custom intra quantization matrix
      * - encoding: Set by user, can be NULL.
      * - decoding: unused.
    *)
    chroma_intra_matrix: puint16_t;

    (*
      * dump format separator.
      * can be ", " or "\n      " or anything else
      * - encoding: Set by user.
      * - decoding: Set by user.
    *)
    dump_separator: puint8_t;

    (*
      * ',' separated list of allowed decoders.
      * If NULL then all are allowed
      * - encoding: unused
      * - decoding: set by user
    *)
    codec_whitelist: pAnsiChar;

    (*
      * Properties of the stream that gets decoded
      * - encoding: unused
      * - decoding: set by libavcodec
    *)
    properties: unsigned;

    (*
      * Additional data associated with the entire coded stream.
      *
      * - decoding: unused
      * - encoding: may be set by libavcodec after avcodec_open2().
    *)
    coded_side_data: pAVPacketSideData;
    nb_coded_side_data: int;

    (*
      * A reference to the AVHWFramesContext describing the input (for encoding)
      * or output (decoding) frames. The reference is set by the caller and
      * afterwards owned (and freed) by libavcodec - it should never be read by
      * the caller after being set.
      *
      * - decoding: This field should be set by the caller from the get_format()
      *             callback. The previous reference (if any) will always be
      *             unreffed by libavcodec before the get_format() call.
      *
      *             If the default get_buffer2() is used with a hwaccel pixel
      *             format, then this AVHWFramesContext will be used for
      *             allocating the frame buffers.
      *
      * - encoding: For hardware encoders configured to use a hwaccel pixel
      *             format, this field should be set by the caller to a reference
      *             to the AVHWFramesContext describing input frames.
      *             AVHWFramesContext.format must be equal to
      *             AVCodecContext.pix_fmt.
      *
      *             This field should be set before avcodec_open2() is called.
    *)
    hw_frames_ctx: pAVBufferRef;

    (*
      * Control the form of AVSubtitle.rects[N]->ass
      * - decoding: set by user
      * - encoding: unused
    *)
    sub_text_format: int;
    (*
      * Audio only. The amount of padding (in samples) appended by the encoder to
      * the end of the audio. I.e. this number of decoded samples must be
      * discarded by the caller from the end of the stream to get the original
      * audio without any trailing padding.
      *
      * - decoding: unused
      * - encoding: unused
    *)
    trailing_padding: int;

    (*
      * The number of pixels per image to maximally accept.
      *
      * - decoding: set by user
      * - encoding: set by user
    *)
    max_pixels: int64_t;

    (*
      * A reference to the AVHWDeviceContext describing the device which will
      * be used by a hardware encoder/decoder.  The reference is set by the
      * caller and afterwards owned (and freed) by libavcodec.
      *
      * This should be used if either the codec device does not require
      * hardware frames or any that are used are to be allocated internally by
      * libavcodec.  If the user wishes to supply any of the frames used as
      * encoder input or decoder output then hw_frames_ctx should be used
      * instead.  When hw_frames_ctx is set in get_format() for a decoder, this
      * field will be ignored while decoding the associated stream segment, but
      * may again be used on a following one after another get_format() call.
      *
      * For both encoders and decoders this field should be set before
      * avcodec_open2() is called and must not be written to thereafter.
      *
      * Note that some decoders may require this field to be set initially in
      * order to support hw_frames_ctx at all - in that case, all frames
      * contexts used must be created on the same device.
    *)
    hw_device_ctx: pAVBufferRef;

    (*
      * Bit set of AV_HWACCEL_FLAG_* flags, which affect hardware accelerated
      * decoding (if active).
      * - encoding: unused
      * - decoding: Set by user (either before avcodec_open2(), or in the
      *             AVCodecContext.get_format callback)
    *)
    hwaccel_flags: int;

    (*
      * Video decoding only. Certain video codecs support cropping, meaning that
      * only a sub-rectangle of the decoded frame is intended for display.  This
      * option controls how cropping is handled by libavcodec.
      *
      * When set to 1 (the default), libavcodec will apply cropping internally.
      * I.e. it will modify the output frame width/height fields and offset the
      * data pointers (only by as much as possible while preserving alignment, or
      * by the full amount if the AV_CODEC_FLAG_UNALIGNED flag is set) so that
      * the frames output by the decoder refer only to the cropped area. The
      * crop_* fields of the output frames will be zero.
      *
      * When set to 0, the width/height fields of the output frames will be set
      * to the coded dimensions and the crop_* fields will describe the cropping
      * rectangle. Applying the cropping is left to the caller.
      *
      * @warning When hardware acceleration with opaque output frames is used,
      * libavcodec is unable to apply cropping from the top/left border.
      *
      * @note when this option is set to zero, the width/height fields of the
      * AVCodecContext and output AVFrames have different meanings. The codec
      * context fields store display dimensions (with the coded dimensions in
      * coded_width/height), while the frame fields store the coded dimensions
      * (with the display dimensions being determined by the crop_* fields).
    *)
    apply_cropping: int;

    (*
      * Video decoding only.  Sets the number of extra hardware frames which
      * the decoder will allocate for use by the caller.  This must be set
      * before avcodec_open2() is called.
      *
      * Some hardware decoders require all frames that they will use for
      * output to be defined in advance before decoding starts.  For such
      * decoders, the hardware frame pool must therefore be of a fixed size.
      * The extra frames set here are on top of any number that the decoder
      * needs internally in order to operate normally (for example, frames
      * used as reference pictures).
    *)
    extra_hw_frames: int;
    (*
      * The percentage of damaged samples to discard a frame.
      *
      * - decoding: set by user
      * - encoding: unused
    *)
    discard_damaged_percentage: int;
  end;

  MpegEncContext = record
  end;

  pMpegEncContext = ^MpegEncContext;

  (*
    * @defgroup lavc_hwaccel AVHWAccel
    *
    * @note  Nothing in this structure should be accessed by the user.  At some
    *        point in future it will not be externally visible at all.
    *
    * @{
  *)
  AVHWAccel = record
    (*
      * Name of the hardware accelerated codec.
      * The name is globally unique among encoders and among decoders (but an
      * encoder and a decoder can share the same name).
    *)
    name: pAnsiChar;

    (*
      * Type of codec implemented by the hardware accelerator.
      *
      * See AVMEDIA_TYPE_xxx
    *)
    _type: AVMediaType;

    (*
      * Codec implemented by the hardware accelerator.
      *
      * See AV_CODEC_ID_xxx
    *)
    id: AVCodecID;

    (*
      * Supported pixel format.
      *
      * Only hardware accelerated formats are supported here.
    *)
    pix_fmt: AVPixelFormat;

    (*
      * Hardware accelerated codec capabilities.
      * see AV_HWACCEL_CODEC_CAP_*
    *)
    capabilities: int;

    (* ***************************************************************
      * No fields below this line are part of the public API. They
      * may not be used outside of libavcodec and can be changed and
      * removed at will.
      * New public fields should be added right above.
      *****************************************************************
    *)

    (*
      * Allocate a custom buffer
    *)
    // int (*alloc_frame)(AVCodecContext *avctx, AVFrame *frame);
    alloc_frame: function(avctx: pAVCodecContext; frame: pAVFrame): int; cdecl;

    (*
      * Called at the beginning of each frame or field picture.
      *
      * Meaningful frame information (codec specific) is guaranteed to
      * be parsed at this point. This function is mandatory.
      *
      * Note that buf can be NULL along with buf_size set to 0.
      * Otherwise, this means the whole frame is available at this point.
      *
      * @param avctx the codec context
      * @param buf the frame data buffer base
      * @param buf_size the size of the frame in bytes
      * @return zero if successful, a negative value otherwise
    *)
    // int (*start_frame)(AVCodecContext *avctx, const uint8_t *buf, uint32_t buf_size);
    start_frame: function(avctx: pAVCodecContext; const buf: puint8_t; buf_size: uint32_t): int; cdecl;

    (*
      * Callback for parameter data (SPS/PPS/VPS etc).
      *
      * Useful for hardware decoders which keep persistent state about the
      * video parameters, and need to receive any changes to update that state.
      *
      * @param avctx the codec context
      * @param type the nal unit type
      * @param buf the nal unit data buffer
      * @param buf_size the size of the nal unit in bytes
      * @return zero if successful, a negative value otherwise
    *)
    // int (*decode_params)(AVCodecContext *avctx, int type, const uint8_t *buf, uint32_t buf_size);
    decode_params: function(avctx: pAVCodecContext; _type: int; const buf: puint8_t; buf_size: uint32_t): int; cdecl;

    (*
      * Callback for each slice.
      *
      * Meaningful slice information (codec specific) is guaranteed to
      * be parsed at this point. This function is mandatory.
      * The only exception is XvMC, that works on MB level.
      *
      * @param avctx the codec context
      * @param buf the slice data buffer base
      * @param buf_size the size of the slice in bytes
      * @return zero if successful, a negative value otherwise
    *)
    // int (*decode_slice)(AVCodecContext *avctx, const uint8_t *buf, uint32_t buf_size);
    decode_slice: function(avctx: pAVCodecContext; const buf: puint8_t; buf_size: uint32_t): int; cdecl;

    (*
      * Called at the end of each frame or field picture.
      *
      * The whole picture is parsed at this point and can now be sent
      * to the hardware accelerator. This function is mandatory.
      *
      * @param avctx the codec context
      * @return zero if successful, a negative value otherwise
    *)
    // int (*end_frame)(AVCodecContext *avctx);
    end_frame: function(avctx: pAVCodecContext): int; cdecl;

    (*
      * Size of per-frame hardware accelerator private data.
      *
      * Private data is allocated with av_mallocz() before
      * AVCodecContext.get_buffer() and deallocated after
      * AVCodecContext.release_buffer().
    *)
    frame_priv_data_size: int;

    (*
      * Called for every Macroblock in a slice.
      *
      * XvMC uses it to replace the ff_mpv_reconstruct_mb().
      * Instead of decoding to raw picture, MB parameters are
      * stored in an array provided by the video driver.
      *
      * @param s the mpeg context
    *)
    // void (*decode_mb)(struct MpegEncContext *s);
    decode_mb: procedure(s: pMpegEncContext); cdecl;

    (*
      * Initialize the hwaccel private data.
      *
      * This will be called from ff_get_format(), after hwaccel and
      * hwaccel_context are set and the hwaccel private data in AVCodecInternal
      * is allocated.
    *)
    // int (*init)(AVCodecContext *avctx);
    init: function(avctx: pAVCodecContext): int; cdecl;

    (*
      * Uninitialize the hwaccel private data.
      *
      * This will be called from get_format() or avcodec_close(), after hwaccel
      * and hwaccel_context are already uninitialized.
    *)
    // int (*uninit)(AVCodecContext *avctx);
    uninit: function(avctx: pAVCodecContext): int; cdecl;

    (*
      * Size of the private data to allocate in
      * AVCodecInternal.hwaccel_priv_data.
    *)
    priv_data_size: int;

    (*
      * Internal hwaccel capabilities.
    *)
    caps_internal: int;

    (*
      * Fill the given hw_frames context with current codec parameters. Called
      * from get_format. Refer to avcodec_get_hw_frames_parameters() for
      * details.
      *
      * This CAN be called before AVHWAccel.init is called, and you must assume
      * that avctx->hwaccel_priv_data is invalid.
    *)
    // int (*frame_params)(AVCodecContext *avctx, AVBufferRef *hw_frames_ctx);
    frame_params: function(avctx: pAVCodecContext; hw_frames_ctx: pAVBufferRef): int; cdecl;
  end;

{$IFDEF FF_API_AVPICTURE}

  (*
    * Picture data structure.
    *
    * Up to four components can be stored into it, the last component is
    * alpha.
    * @deprecated use AVFrame or imgutils functions instead
  *)
  pAVPicture = ^AVPicture;

  AVPicture = record
    // attribute_deprecated
    data: TAVNDPArray_puint8_t deprecated; // < pointers to the image data planes

    // attribute_deprecated
    linesize: TAVNDPArray_int deprecated; // < number of bytes per line
  end deprecated 'use AVFrame or imgutils functions instead';

{$ENDIF}

  AVSubtitleType = (                //
    SUBTITLE_NONE, SUBTITLE_BITMAP, // < A bitmap, pict will be set
    (*
      * Plain text, the text field must be set by the decoder and is
      * authoritative. ass and pict fields may contain approximations.
    *)
    SUBTITLE_TEXT,
    (*
      * Formatted text, the ass field must be set by the decoder and is
      * authoritative. pict and text fields may contain approximations.
    *)
    SUBTITLE_ASS);

  AVSubtitleRect = record
    x: int;         // < top left corner  of pict, undefined when pict is not set
    y: int;         // < top left corner  of pict, undefined when pict is not set
    w: int;         // < width            of pict, undefined when pict is not set
    h: int;         // < height           of pict, undefined when pict is not set
    nb_colors: int; // < number of colors in pict, undefined when pict is not set

{$IFDEF FF_API_AVPICTURE}
    (*
      * @deprecated unused
    *)
    // attribute_deprecated
    pict: AVPicture deprecated;
{$ENDIF}
    (*
      * data+linesize for the bitmap of this subtitle.
      * Can be set for text/ass as well once they are rendered.
    *)
    data: puint8_t_array_4;
    linesize: Tint_array_4;

    _type: AVSubtitleType;

    text: pAnsiChar; // < 0 terminated plain UTF-8 text

    (*
      * 0 terminated ASS/SSA compatible event line.
      * The presentation of this is unaffected by the other values in this
      * struct.
    *)
    ass: pAnsiChar;

    flags: int;
  end;

  pAVSubtitleRect = ^AVSubtitleRect;
  ppAVSubtitleRect = ^pAVSubtitleRect;

  AVSubtitle = record
    format: uint16_t;             (* 0 = graphics *)
    start_display_time: uint32_t; (* relative to packet pts, in ms *)
    end_display_time: uint32_t;   (* relative to packet pts, in ms *)
    num_rects: unsigned;
    rects: ppAVSubtitleRect;
    pts: int64_t; // < Same as packet pts, in AV_TIME_BASE
  end;

  (*
    * This struct describes the properties of an encoded stream.
    *
    * sizeof(AVCodecParameters) is not a part of the public ABI, this struct must
    * be allocated with avcodec_parameters_alloc() and freed with
    * avcodec_parameters_free().
  *)
  pAVCodecParameters = ^AVCodecParameters;

  AVCodecParameters = record
    (*
      * General type of the encoded data.
    *)
    codec_type: AVMediaType;
    (*
      * Specific type of the encoded data (the codec used).
    *)
    codec_id: AVCodecID;

    (*
      * Additional information about the codec (corresponds to the AVI FOURCC).
    *)
    codec_tag: // uint32_t;
      packed record
      case Integer of
        0:
          (tag: Cardinal);
        1:
          (fourcc: array [0 .. 3] of AnsiChar);
        2:
          (fourbb: array [0 .. 3] of Byte);
    end;

    (*
      * Extra binary data needed for initializing the decoder, codec-dependent.
      *
      * Must be allocated with av_malloc() and will be freed by
      * avcodec_parameters_free(). The allocated size of extradata must be at
      * least extradata_size + AV_INPUT_BUFFER_PADDING_SIZE, with the padding
      * bytes zeroed.
    *)
    extradata: puint8_t;
    (*
      * Size of the extradata content in bytes.
    *)
    extradata_size: int;

    (*
      * - video: the pixel format, the value corresponds to enum AVPixelFormat.
      * - audio: the sample format, the value corresponds to enum AVSampleFormat.
    *)
    format: int;

    (*
      * The average bitrate of the encoded data (in bits per second).
    *)
    bit_rate: int64_t;

    (*
      * The number of bits per sample in the codedwords.
      *
      * This is basically the bitrate per sample. It is mandatory for a bunch of
      * formats to actually decode them. It's the number of bits for one sample in
      * the actual coded bitstream.
      *
      * This could be for example 4 for ADPCM
      * For PCM formats this matches bits_per_raw_sample
      * Can be 0
    *)
    bits_per_coded_sample: int;

    (*
      * This is the number of valid bits in each output sample. If the
      * sample format has more bits, the least significant bits are additional
      * padding bits, which are always 0. Use right shifts to reduce the sample
      * to its actual size. For example, audio formats with 24 bit samples will
      * have bits_per_raw_sample set to 24, and format set to AV_SAMPLE_FMT_S32.
      * To get the original sample use "(int32_t)sample >> 8"."
      *
      * For ADPCM this might be 12 or 16 or similar
      * Can be 0
    *)
    bits_per_raw_sample: int;

    (*
      * Codec-specific bitstream restrictions that the stream conforms to.
    *)
    profile: int;
    level: int;

    (*
      * Video only. The dimensions of the video frame in pixels.
    *)
    width: int;
    height: int;

    (*
      * Video only. The aspect ratio (width / height) which a single pixel
      * should have when displayed.
      *
      * When the aspect ratio is unknown / undefined, the numerator should be
      * set to 0 (the denominator may have any value).
    *)
    sample_aspect_ratio: AVRational;

    (*
      * Video only. The order of the fields in interlaced video.
    *)
    field_order: AVFieldOrder;

    (*
      * Video only. Additional colorspace characteristics.
    *)
    color_range: AVColorRange;
    color_primaries: AVColorPrimaries;
    color_trc: AVColorTransferCharacteristic;
    color_space: AVColorSpace;
    chroma_location: AVChromaLocation;

    (*
      * Video only. Number of delayed frames.
    *)
    video_delay: int;

    (*
      * Audio only. The channel layout bitmask. May be 0 if the channel layout is
      * unknown or unspecified, otherwise the number of bits set must be equal to
      * the channels field.
    *)
    channel_layout: uint64_t;
    (*
      * Audio only. The number of audio channels.
    *)
    channels: int;
    (*
      * Audio only. The number of audio samples per second.
    *)
    sample_rate: int;
    (*
      * Audio only. The number of bytes per coded audio frame, required by some
      * formats.
      *
      * Corresponds to nBlockAlign in WAVEFORMATEX.
    *)
    block_align: int;
    (*
      * Audio only. Audio frame size, if known. Required by some formats to be static.
    *)
    frame_size: int;

    (*
      * Audio only. The amount of padding (in samples) inserted by the encoder at
      * the beginning of the audio. I.e. this number of leading decoded samples
      * must be discarded by the caller to get the original audio without leading
      * padding.
    *)
    initial_padding: int;
    (*
      * Audio only. The amount of padding (in samples) appended by the encoder to
      * the end of the audio. I.e. this number of decoded samples must be
      * discarded by the caller from the end of the stream to get the original
      * audio without any trailing padding.
    *)
    trailing_padding: int;
    (*
      * Audio only. Number of samples to skip after a discontinuity.
    *)
    seek_preroll: int;
  end;

{$IFDEF FF_API_CODEC_GET_SET}

  (*
    * Accessors for some AVCodecContext fields. These used to be provided for ABI
    * compatibility, and do not need to be used anymore.
  *)
  // attribute_deprecated
  // AVRational av_codec_get_pkt_timebase(const AVCodecContext * avctx);
function av_codec_get_pkt_timebase(const avctx: pAVCodecContext): AVRational; cdecl; external avcodec_dll; deprecated;
// attribute_deprecated
// void av_codec_set_pkt_timebase(AVCodecContext * avctx, AVRational val);
procedure av_codec_set_pkt_timebase(avctx: pAVCodecContext; val: AVRational); cdecl; external avcodec_dll; deprecated;

// attribute_deprecated
// const AVCodecDescriptor * av_codec_get_codec_descriptor(const AVCodecContext * avctx);
function av_codec_get_codec_descriptor(const avctx: pAVCodecContext): pAVCodecDescriptor; cdecl; external avcodec_dll;

// attribute_deprecated
// void av_codec_set_codec_descriptor(AVCodecContext * avctx, const AVCodecDescriptor * desc);
procedure av_codec_set_codec_descriptor(avctx: pAVCodecContext; const desc: pAVCodecDescriptor); cdecl; external avcodec_dll;

// attribute_deprecated
// unsigned av_codec_get_codec_properties(const AVCodecContext * avctx);
function av_codec_get_codec_properties(const avctx: pAVCodecContext): unsigned; cdecl; external avcodec_dll; deprecated;

{$IFDEF FF_API_LOWRES}
// attribute_deprecated
// int  av_codec_get_lowres(const AVCodecContext *avctx);
function av_codec_get_lowres(const avctx: pAVCodecContext): int; cdecl; external avcodec_dll; deprecated;

// attribute_deprecated
// void av_codec_set_lowres(AVCodecContext *avctx, int val);
procedure av_codec_set_lowres(avctx: pAVCodecContext; val: int); cdecl; external avcodec_dll; deprecated;

{$ENDIF}
// attribute_deprecated
// int av_codec_get_seek_preroll(const AVCodecContext * avctx);
function av_codec_get_seek_preroll(const avctx: pAVCodecContext): int; cdecl; external avcodec_dll; deprecated;

// attribute_deprecated
// void av_codec_set_seek_preroll(AVCodecContext * avctx, int val);
procedure av_codec_set_seek_preroll(avctx: pAVCodecContext; val: int); cdecl; external avcodec_dll; deprecated;

// attribute_deprecated
// uint16_t * av_codec_get_chroma_intra_matrix(const AVCodecContext * avctx);
function av_codec_get_chroma_intra_matrix(const avctx: pAVCodecContext): puint16_t; cdecl; external avcodec_dll; deprecated;

// attribute_deprecated
// void av_codec_set_chroma_intra_matrix(AVCodecContext * avctx, uint16_t * val);
procedure av_codec_set_chroma_intra_matrix(avctx: pAVCodecContext; val: puint16_t); cdecl; external avcodec_dll; deprecated;
{$ENDIF}
{$IFDEF FF_API_CODEC_GET_SET}
// attribute_deprecated
// int av_codec_get_max_lowres(const avcodec * codec);
function av_codec_get_max_lowres(const codec: pAVCodec): int; cdecl; external avcodec_dll; deprecated;
{$ENDIF}
(*
  * Retrieve supported hardware configurations for a codec.
  *
  * Values of index from zero to some maximum return the indexed configuration
  * descriptor; all other values return NULL.  If the codec does not support
  * any hardware configurations then it will always return NULL.
*)
// const AVCodecHWConfig * avcodec_get_hw_config(const avcodec * codec, int index);
function avcodec_get_hw_config(const codec: pAVCodec; index: int): pAVCodecHWConfig; cdecl; external avcodec_dll;

const
  AV_PKT_DATA_QUALITY_FACTOR: AVPacketSideDataType = AV_PKT_DATA_QUALITY_STATS; // DEPRECATED

  (*
    * Iterate over all registered codecs.
    *
    * @param opaque a pointer where libavcodec will store the iteration state. Must
    *               point to NULL to start the iteration.
    *
    * @return the next registered codec or NULL when the iteration is
    *         finished
  *)
  // const avcodec * av_codec_iterate(void * * opaque);
function av_codec_iterate(var opaque: Pointer): pAVCodec; cdecl; external avcodec_dll;

{$IFDEF FF_API_NEXT}
(*
  * If c is NULL, returns the first registered codec,
  * if c is non-NULL, returns the next registered codec after c,
  * or NULL if c is the last one.
*)
// attribute_deprecated avcodec * av_codec_next(const avcodec * c);
function av_codec_next(const c: pAVCodec): pAVCodec; cdecl; external avcodec_dll;
{$ENDIF}
(*
  * Return the LIBAVCODEC_VERSION_INT constant.
*)
// unsigned avcodec_version(void);
function avcodec_version(): unsigned; cdecl; external avcodec_dll;

(*
  * Return the libavcodec build-time configuration.
*)
// const char * avcodec_configuration(void);
function avcodec_configuration(): pAnsiChar; cdecl; external avcodec_dll;

(*
  * Return the libavcodec license.
*)
// const char * avcodec_license(void);
function avcodec_license(): pAnsiChar; cdecl; external avcodec_dll;

{$IFDEF FF_API_NEXT}
(*
  * Register the codec codec and initialize libavcodec.
  *
  * @warning either this function or avcodec_register_all() must be called
  * before any other libavcodec functions.
  *
  * @see avcodec_register_all()
*)
// attribute_deprecated void avcodec_register(avcodec * codec);
procedure avcodec_register(codec: pAVCodec); cdecl; external avcodec_dll; deprecated;

(*
  * Register all the codecs, parsers and bitstream filters which were enabled at
  * configuration time. If you do not call this function you can select exactly
  * which formats you want to support, by using the individual registration
  * functions.
  *
  * @see avcodec_register
  * @see av_register_codec_parser
  * @see av_register_bitstream_filter
*)
// attribute_deprecated void avcodec_register_all(void);
procedure avcodec_register_all(); cdecl; external avcodec_dll; deprecated;
{$ENDIF}
(*
  * Allocate an AVCodecContext and set its fields to default values. The
  * resulting struct should be freed with avcodec_free_context().
  *
  * @param codec if non-NULL, allocate private data and initialize defaults
  *              for the given codec. It is illegal to then call avcodec_open2()
  *              with a different codec.
  *              If NULL, then the codec-specific defaults won't be initialized,
  *              which may result in suboptimal default settings (this is
  *              important mainly for encoders, e.g. libx264).
  *
  * @return An AVCodecContext filled with default values or NULL on failure.
*)
// AVCodecContext * avcodec_alloc_context3(const avcodec * codec);
function avcodec_alloc_context3(const codec: pAVCodec): pAVCodecContext; cdecl; external avcodec_dll;

(*
  * Free the codec context and everything associated with it and write NULL to
  * the provided pointer.
*)
// void avcodec_free_context(AVCodecContext * * avctx);
procedure avcodec_free_context(var avctx: pAVCodecContext); cdecl; external avcodec_dll;

{$IFDEF FF_API_GET_CONTEXT_DEFAULTS}
(*
  * @deprecated This function should not be used, as closing and opening a codec
  * context multiple time is not supported. A new codec context should be
  * allocated for each new use.
*)
// int avcodec_get_context_defaults3(AVCodecContext * s, const avcodec * codec);
function avcodec_get_context_defaults3(s: pAVCodecContext; const codec: pAVCodec): int; cdecl; external avcodec_dll;
{$ENDIF}
(*
  * Get the AVClass for AVCodecContext. It can be used in combination with
  * AV_OPT_SEARCH_FAKE_OBJ for examining options.
  *
  * @see av_opt_find().
*)
// const AVClass * avcodec_get_class(void);
function avcodec_get_class(): pAVClass; cdecl; external avcodec_dll;

{$IFDEF FF_API_COPY_CONTEXT}
(*
  * Get the AVClass for AVFrame. It can be used in combination with
  * AV_OPT_SEARCH_FAKE_OBJ for examining options.
  *
  * @see av_opt_find().
*)
// const AVClass * avcodec_get_frame_class(void);
function avcodec_get_frame_class(): pAVClass; cdecl; external avcodec_dll;

(*
  * Get the AVClass for AVSubtitleRect. It can be used in combination with
  * AV_OPT_SEARCH_FAKE_OBJ for examining options.
  *
  * @see av_opt_find().
*)
// const AVClass * avcodec_get_subtitle_rect_class(void);
function avcodec_get_subtitle_rect_class(): pAVClass; cdecl; external avcodec_dll;

(*
  * Copy the settings of the source AVCodecContext into the destination
  * AVCodecContext. The resulting destination codec context will be
  * unopened, i.e. you are required to call avcodec_open2() before you
  * can use this AVCodecContext to decode/encode video/audio data.
  *
  * @param dest target codec context, should be initialized with
  *             avcodec_alloc_context3(NULL), but otherwise uninitialized
  * @param src source codec context
  * @return AVERROR() on error (e.g. memory allocation error), 0 on success
  *
  * @deprecated The semantics of this function are ill-defined and it should not
  * be used. If you need to transfer the stream parameters from one codec context
  * to another, use an intermediate AVCodecParameters instance and the
  * avcodec_parameters_from_context() / avcodec_parameters_to_context()
  * functions.
*)
// attribute_deprecated int avcodec_copy_context(AVCodecContext * dest, const AVCodecContext * src);
function avcodec_copy_context(dest: pAVCodecContext; const src: pAVCodecContext): int; cdecl; external avcodec_dll; deprecated;
{$ENDIF}
(*
  * Allocate a new AVCodecParameters and set its fields to default values
  * (unknown/invalid/0). The returned struct must be freed with
  * avcodec_parameters_free().
*)
// AVCodecParameters * avcodec_parameters_alloc(void);
function avcodec_parameters_alloc(): pAVCodecParameters; cdecl; external avcodec_dll;

(*
  * Free an AVCodecParameters instance and everything associated with it and
  * write NULL to the supplied pointer.
*)
// void avcodec_parameters_free(AVCodecParameters * * par);
procedure avcodec_parameters_free(var par: pAVCodecParameters); cdecl; external avcodec_dll;

(*
  * Copy the contents of src to dst. Any allocated fields in dst are freed and
  * replaced with newly allocated duplicates of the corresponding fields in src.
  *
  * @return >= 0 on success, a negative AVERROR code on failure.
*)
// int avcodec_parameters_copy(AVCodecParameters * dst, const AVCodecParameters * src);
function avcodec_parameters_copy(dst: pAVCodecParameters; const src: pAVCodecParameters): int; cdecl; external avcodec_dll;

(*
  * Fill the parameters struct based on the values from the supplied codec
  * context. Any allocated fields in par are freed and replaced with duplicates
  * of the corresponding fields in codec.
  *
  * @return >= 0 on success, a negative AVERROR code on failure
*)
// int avcodec_parameters_from_context(AVCodecParameters * par, const AVCodecContext * codec);
function avcodec_parameters_from_context(par: pAVCodecParameters; const codec: pAVCodecContext): int; cdecl; external avcodec_dll;

(*
  * Fill the codec context based on the values from the supplied codec
  * parameters. Any allocated fields in codec that have a corresponding field in
  * par are freed and replaced with duplicates of the corresponding field in par.
  * Fields in codec that do not have a counterpart in par are not touched.
  *
  * @return >= 0 on success, a negative AVERROR code on failure.
*)
// int avcodec_parameters_to_context(AVCodecContext * codec, const AVCodecParameters * par);
function avcodec_parameters_to_context(codec: pAVCodecContext; const par: pAVCodecParameters): int; cdecl; external avcodec_dll;

(*
  * Initialize the AVCodecContext to use the given AVCodec. Prior to using this
  * function the context has to be allocated with avcodec_alloc_context3().
  *
  * The functions avcodec_find_decoder_by_name(), avcodec_find_encoder_by_name(),
  * avcodec_find_decoder() and avcodec_find_encoder() provide an easy way for
  * retrieving a codec.
  *
  * @warning This function is not thread safe!
  *
  * @note Always call this function before using decoding routines (such as
  * @ref avcodec_receive_frame()).
  *
  * @code
  * avcodec_register_all();
  * av_dict_set(&opts, "b", "2.5M", 0);
  * codec = avcodec_find_decoder(AV_CODEC_ID_H264);
  * if (!codec)
  *     exit(1);
  *
  * context = avcodec_alloc_context3(codec);
  *
  * if (avcodec_open2(context, codec, opts) < 0)
  *     exit(1);
  * @endcode
  *
  * @param avctx The context to initialize.
  * @param codec The codec to open this context for. If a non-NULL codec has been
  *              previously passed to avcodec_alloc_context3() or
  *              for this context, then this parameter MUST be either NULL or
  *              equal to the previously passed codec.
  * @param options A dictionary filled with AVCodecContext and codec-private options.
  *                On return this object will be filled with options that were not found.
  *
  * @return zero on success, a negative value on error
  * @see avcodec_alloc_context3(), avcodec_find_decoder(), avcodec_find_encoder(),
  *      av_dict_set(), av_opt_find().
*)
// int avcodec_open2(AVCodecContext * avctx, const avcodec *codec, AVDictionary **options);
function avcodec_open2(avctx: pAVCodecContext; const codec: pAVCodec; options: ppAVDictionary): int; cdecl; external avcodec_dll;

(*
  * Close a given AVCodecContext and free all the data associated with it
  * (but not the AVCodecContext itself).
  *
  * Calling this function on an AVCodecContext that hasn't been opened will free
  * the codec-specific data allocated in avcodec_alloc_context3() with a non-NULL
  * codec. Subsequent calls will do nothing.
  *
  * @note Do not use this function. Use avcodec_free_context() to destroy a
  * codec context (either open or closed). Opening and closing a codec context
  * multiple times is not supported anymore -- use multiple codec contexts
  * instead.
*)
// int avcodec_close(AVCodecContext * avctx);
function avcodec_close(avctx: pAVCodecContext): int; cdecl; external avcodec_dll;

(*
  * Free all allocated data in the given subtitle struct.
  *
  * @param sub AVSubtitle to free.
*)
// void avsubtitle_free(AVSubtitle * sub);
procedure avsubtitle_free(sub: pAVSubtitle); cdecl; external avcodec_dll;

(*
  * Allocate an AVPacket and set its fields to default values.  The resulting
  * struct must be freed using av_packet_free().
  *
  * @return An AVPacket filled with default values or NULL on failure.
  *
  * @note this only allocates the AVPacket itself, not the data buffers. Those
  * must be allocated through other means such as av_new_packet.
  *
  * @see av_new_packet
*)
// AVPacket *av_packet_alloc(void);
function av_packet_alloc(): pAVPacket; cdecl; external avcodec_dll;

(*
  * Create a new packet that references the same data as src.
  *
  * This is a shortcut for av_packet_alloc()+av_packet_ref().
  *
  * @return newly created AVPacket on success, NULL on error.
  *
  * @see av_packet_alloc
  * @see av_packet_ref
*)
// AVPacket * av_packet_clone(const AVPacket * src);
function av_packet_clone(const src: pAVPacket): pAVPacket; cdecl; external avcodec_dll;

(*
  * Free the packet, if the packet is reference counted, it will be
  * unreferenced first.
  *
  * @param pkt packet to be freed. The pointer will be set to NULL.
  * @note passing NULL is a no-op.
*)
// void av_packet_free(AVPacket * * pkt);
procedure av_packet_free(var pkt: pAVPacket); cdecl; external avcodec_dll;

(*
  * Initialize optional fields of a packet with default values.
  *
  * Note, this does not touch the data and size members, which have to be
  * initialized separately.
  *
  * @param pkt packet
*)
// void av_init_packet(AVPacket * pkt);
procedure av_init_packet(pkt: pAVPacket); cdecl; overload; external avcodec_dll;
procedure av_init_packet(var pkt: AVPacket); cdecl; overload; external avcodec_dll;

(*
  * Allocate the payload of a packet and initialize its fields with
  * default values.
  *
  * @param pkt packet
  * @param size wanted payload size
  * @return 0 if OK, AVERROR_xxx otherwise
*)
// int av_new_packet(AVPacket * pkt, int size);
function av_new_packet(pkt: pAVPacket; size: int): int; cdecl; external avcodec_dll;

(*
  * Reduce packet size, correctly zeroing padding
  *
  * @param pkt packet
  * @param size new size
*)
// void av_shrink_packet(AVPacket * pkt, int size);
procedure av_shrink_packet(pkt: pAVPacket; size: int); cdecl; external avcodec_dll;

(*
  * Increase packet size, correctly zeroing padding
  *
  * @param pkt packet
  * @param grow_by number of bytes by which to increase the size of the packet
*)
// int av_grow_packet(AVPacket * pkt, int grow_by);
function av_grow_packet(pkt: pAVPacket; grow_by: int): int; cdecl; external avcodec_dll;

(*
  * Initialize a reference-counted packet from av_malloc()ed data.
  *
  * @param pkt packet to be initialized. This function will set the data, size,
  *        and buf fields, all others are left untouched.
  * @param data Data allocated by av_malloc() to be used as packet data. If this
  *        function returns successfully, the data is owned by the underlying AVBuffer.
  *        The caller may not access the data through other means.
  * @param size size of data in bytes, without the padding. I.e. the full buffer
  *        size is assumed to be size + AV_INPUT_BUFFER_PADDING_SIZE.
  *
  * @return 0 on success, a negative AVERROR on error
*)
// int av_packet_from_data(AVPacket * pkt, uint8_t * data, int size);
function av_packet_from_data(pkt: pAVPacket; data: puint8_t; size: int): int; cdecl; external avcodec_dll;

{$IFDEF FF_API_AVPACKET_OLD_API}
(*
  * @warning This is a hack - the packet memory allocation stuff is broken. The
  * packet is allocated if it was not really allocated.
  *
  * @deprecated Use av_packet_ref or av_packet_make_refcounted
*)
// attribute_deprecated int av_dup_packet(AVPacket * pkt);
function av_dup_packet(pkt: pAVPacket): int; cdecl; external avcodec_dll; deprecated;
(*
  * Copy packet, including contents
  *
  * @return 0 on success, negative AVERROR on fail
  *
  * @deprecated Use av_packet_ref
*)
// attribute_deprecated int av_copy_packet(AVPacket * dst, const AVPacket * src);
function av_copy_packet(dst: pAVPacket; const src: pAVPacket): int; cdecl; external avcodec_dll; deprecated;

(*
  * Copy packet side data
  *
  * @return 0 on success, negative AVERROR on fail
  *
  * @deprecated Use av_packet_copy_props
*)
// attribute_deprecated int av_copy_packet_side_data(AVPacket * dst, const AVPacket * src);
function av_copy_packet_side_data(dst: pAVPacket; const src: pAVPacket): int; cdecl; external avcodec_dll; deprecated;

(*
  * Free a packet.
  *
  * @deprecated Use av_packet_unref
  *
  * @param pkt packet to free
*)
// attribute_deprecated void av_free_packet(AVPacket * pkt);
procedure av_free_packet(pkt: pAVPacket); cdecl; overload; external avcodec_dll; deprecated;
procedure av_free_packet(Var pkt: AVPacket); cdecl; overload; external avcodec_dll; deprecated;
{$ENDIF}
(*
  * Allocate new information of a packet.
  *
  * @param pkt packet
  * @param type side information type
  * @param size side information size
  * @return pointer to fresh allocated data or NULL otherwise
*)
// uint8_t * av_packet_new_side_data(AVPacket * pkt, enum AVPacketSideDataType type , int size);
function av_packet_new_side_data(pkt: pAVPacket; _type: AVPacketSideDataType; size: int): puint8_t; cdecl; external avcodec_dll;

(*
  * Wrap an existing array as a packet side data.
  *
  * @param pkt packet
  * @param type side information type
  * @param data the side data array. It must be allocated with the av_malloc()
  *             family of functions. The ownership of the data is transferred to
  *             pkt.
  * @param size side information size
  * @return a non-negative number on success, a negative AVERROR code on
  *         failure. On failure, the packet is unchanged and the data remains
  *         owned by the caller.
*)
// int av_packet_add_side_data(AVPacket * pkt, enum AVPacketSideDataType type , uint8_t * data, size_t size);
function av_packet_add_side_data(pkt: pAVPacket; _type: AVPacketSideDataType; data: puint8_t; size: size_t): int; cdecl; external avcodec_dll;

(*
  * Shrink the already allocated side data buffer
  *
  * @param pkt packet
  * @param type side information type
  * @param size new side information size
  * @return 0 on success, < 0 on failure
*)
// int av_packet_shrink_side_data(AVPacket * pkt, enum AVPacketSideDataType type , int size);
function av_packet_shrink_side_data(pkt: pAVPacket; _type: AVPacketSideDataType; size: int): int; cdecl; external avcodec_dll;

(*
  * Get side information from packet.
  *
  * @param pkt packet
  * @param type desired side information type
  * @param size pointer for side information size to store (optional)
  * @return pointer to data if present or NULL otherwise
*)
// uint8_t * av_packet_get_side_data(const AVPacket * pkt, enum AVPacketSideDataType type , int * size);
function av_packet_get_side_data(const pkt: pAVPacket; _type: AVPacketSideDataType; size: pInt): puint8_t; cdecl; external avcodec_dll;

{$IFDEF FF_API_MERGE_SD_API}
// attribute_deprecated int av_packet_merge_side_data(AVPacket * pkt);
function av_packet_merge_side_data(pkt: pAVPacket): int; cdecl; external avcodec_dll; deprecated;

// attribute_deprecated int av_packet_split_side_data(AVPacket * pkt);
function av_packet_split_side_data(pkt: pAVPacket): int; cdecl; external avcodec_dll; deprecated;
{$ENDIF}
// const char * av_packet_side_data_name(enum AVPacketSideDataType type);
function av_packet_side_data_name(_type: AVPacketSideDataType): pAnsiChar; cdecl; external avcodec_dll;

(*
  * Pack a dictionary for use in side_data.
  *
  * @param dict The dictionary to pack.
  * @param size pointer to store the size of the returned data
  * @return pointer to data if successful, NULL otherwise
*)
// uint8_t * av_packet_pack_dictionary(AVDictionary * dict, int * size);
function av_packet_pack_dictionary(dict: pAVDictionary; size: pInt): puint8_t; cdecl; external avcodec_dll;
(*
  * Unpack a dictionary from side_data.
  *
  * @param data data from side_data
  * @param size size of the data
  * @param dict the metadata storage dictionary
  * @return 0 on success, < 0 on failure
*)
// int av_packet_unpack_dictionary(const uint8_t * data, int size, AVDictionary **dict);
function av_packet_unpack_dictionary(const data: puint8_t; size: int; var dict: pAVDictionary): int; cdecl; external avcodec_dll;

(*
  * Convenience function to free all the side data stored.
  * All the other fields stay untouched.
  *
  * @param pkt packet
*)
// void av_packet_free_side_data(AVPacket * pkt);
procedure av_packet_free_side_data(pkt: pAVPacket); cdecl; external avcodec_dll;

(*
  * Setup a new reference to the data described by a given packet
  *
  * If src is reference-counted, setup dst as a new reference to the
  * buffer in src. Otherwise allocate a new buffer in dst and copy the
  * data from src into it.
  *
  * All the other fields are copied from src.
  *
  * @see av_packet_unref
  *
  * @param dst Destination packet
  * @param src Source packet
  *
  * @return 0 on success, a negative AVERROR on error.
*)
// int av_packet_ref(AVPacket * dst, const AVPacket * src);
function av_packet_ref(dst: pAVPacket; const src: pAVPacket): int; cdecl; external avcodec_dll;

(*
  * Wipe the packet.
  *
  * Unreference the buffer referenced by the packet and reset the
  * remaining packet fields to their default values.
  *
  * @param pkt The packet to be unreferenced.
*)
// void av_packet_unref(AVPacket * pkt);
procedure av_packet_unref(pkt: pAVPacket); cdecl; external avcodec_dll; overload;
procedure av_packet_unref(var pkt: AVPacket); cdecl; external avcodec_dll; overload;

(*
  * Move every field in src to dst and reset src.
  *
  * @see av_packet_unref
  *
  * @param src Source packet, will be reset
  * @param dst Destination packet
*)
// void av_packet_move_ref(AVPacket * dst, AVPacket * src);
procedure av_packet_move_ref(dst: pAVPacket; src: pAVPacket); cdecl; external avcodec_dll; overload;
procedure av_packet_move_ref(dst: pAVPacket; var src: AVPacket); cdecl; external avcodec_dll; overload;

(*
  * Copy only "properties" fields from src to dst.
  *
  * Properties for the purpose of this function are all the fields
  * beside those related to the packet data (buf, data, size)
  *
  * @param dst Destination packet
  * @param src Source packet
  *
  * @return 0 on success AVERROR on failure.
*)
// int av_packet_copy_props(AVPacket * dst, const AVPacket * src);
function av_packet_copy_props(dst: pAVPacket; const src: pAVPacket): int; cdecl; external avcodec_dll;

(*
  * Ensure the data described by a given packet is reference counted.
  *
  * @note This function does not ensure that the reference will be writable.
  *       Use av_packet_make_writable instead for that purpose.
  *
  * @see av_packet_ref
  * @see av_packet_make_writable
  *
  * @param pkt packet whose data should be made reference counted.
  *
  * @return 0 on success, a negative AVERROR on error. On failure, the
  *         packet is unchanged.
*)
// int av_packet_make_refcounted(AVPacket * pkt);
function av_packet_make_refcounted(pkt: pAVPacket): int; cdecl; external avcodec_dll;

(*
  * Create a writable reference for the data described by a given packet,
  * avoiding data copy if possible.
  *
  * @param pkt Packet whose data should be made writable.
  *
  * @return 0 on success, a negative AVERROR on failure. On failure, the
  *         packet is unchanged.
*)
// int av_packet_make_writable(AVPacket * pkt);
function av_packet_make_writable(pkt: pAVPacket): int; cdecl; external avcodec_dll;

(*
  * Convert valid timing fields (timestamps / durations) in a packet from one
  * timebase to another. Timestamps with unknown values (AV_NOPTS_VALUE) will be
  * ignored.
  *
  * @param pkt packet on which the conversion will be performed
  * @param tb_src source timebase, in which the timing fields in pkt are
  *               expressed
  * @param tb_dst destination timebase, to which the timing fields will be
  *               converted
*)
// void av_packet_rescale_ts(AVPacket * pkt, AVRational tb_src, AVRational tb_dst);
procedure av_packet_rescale_ts(pkt: pAVPacket; tb_src: AVRational; tb_dst: AVRational); cdecl; external avcodec_dll;

(*
  * Find a registered decoder with a matching codec ID.
  *
  * @param id AVCodecID of the requested decoder
  * @return A decoder if one was found, NULL otherwise.
*)
// avcodec * avcodec_find_decoder(enum AVCodecID id);
function avcodec_find_decoder(id: AVCodecID): pAVCodec; cdecl; external avcodec_dll;

(*
  * Find a registered decoder with the specified name.
  *
  * @param name name of the requested decoder
  * @return A decoder if one was found, NULL otherwise.
*)
// avcodec * avcodec_find_decoder_by_name(const char * name);
function avcodec_find_decoder_by_name(const name: pAnsiChar): pAVCodec; cdecl; external avcodec_dll;

(*
  * The default callback for AVCodecContext.get_buffer2(). It is made public so
  * it can be called by custom get_buffer2() implementations for decoders without
  * AV_CODEC_CAP_DR1 set.
*)
// int avcodec_default_get_buffer2(AVCodecContext * s, AVFrame * frame, int flags);
function avcodec_default_get_buffer2(s: pAVCodecContext; frame: pAVFrame; flags: int): int; cdecl; external avcodec_dll;

(*
  * Modify width and height values so that they will result in a memory
  * buffer that is acceptable for the codec if you do not use any horizontal
  * padding.
  *
  * May only be used if a codec with AV_CODEC_CAP_DR1 has been opened.
*)
// void avcodec_align_dimensions(AVCodecContext * s, int * width, int * height);
procedure avcodec_align_dimensions(s: pAVCodecContext; width: pInt; height: pInt); cdecl; external avcodec_dll;

(*
  * Modify width and height values so that they will result in a memory
  * buffer that is acceptable for the codec if you also ensure that all
  * line sizes are a multiple of the respective linesize_align[i].
  *
  * May only be used if a codec with AV_CODEC_CAP_DR1 has been opened.
*)
// void avcodec_align_dimensions2(AVCodecContext * s, int * width, int * height, int linesize_align[AV_NUM_DATA_POINTERS]);
procedure avcodec_align_dimensions2(s: pAVCodecContext; var width: int; var height: int; linesize_align: TAVNDPArray_int); cdecl; external avcodec_dll;

(*
  * Converts AVChromaLocation to swscale x/y chroma position.
  *
  * The positions represent the chroma (0,0) position in a coordinates system
  * with luma (0,0) representing the origin and luma(1,1) representing 256,256
  *
  * @param xpos  horizontal chroma sample position
  * @param ypos  vertical   chroma sample position
*)
// int avcodec_enum_to_chroma_pos(int * xpos, int * ypos, enum AVChromaLocation pos);
function avcodec_enum_to_chroma_pos(var xpos: int; ypos: int; pos: AVChromaLocation): int; cdecl; external avcodec_dll;

(*
  * Converts swscale x/y chroma position to AVChromaLocation.
  *
  * The positions represent the chroma (0,0) position in a coordinates system
  * with luma (0,0) representing the origin and luma(1,1) representing 256,256
  *
  * @param xpos  horizontal chroma sample position
  * @param ypos  vertical   chroma sample position
*)
// enum AVChromaLocation avcodec_chroma_pos_to_enum(int xpos, int ypos);
function avcodec_chroma_pos_to_enum(xpos, ypos: int): AVChromaLocation; cdecl; external avcodec_dll;

(*
  * Decode the audio frame of size avpkt->size from avpkt->data into frame.
  *
  * Some decoders may support multiple frames in a single AVPacket. Such
  * decoders would then just decode the first frame and the return value would be
  * less than the packet size. In this case, avcodec_decode_audio4 has to be
  * called again with an AVPacket containing the remaining data in order to
  * decode the second frame, etc...  Even if no frames are returned, the packet
  * needs to be fed to the decoder with remaining data until it is completely
  * consumed or an error occurs.
  *
  * Some decoders (those marked with AV_CODEC_CAP_DELAY) have a delay between input
  * and output. This means that for some packets they will not immediately
  * produce decoded output and need to be flushed at the end of decoding to get
  * all the decoded data. Flushing is done by calling this function with packets
  * with avpkt->data set to NULL and avpkt->size set to 0 until it stops
  * returning samples. It is safe to flush even those decoders that are not
  * marked with AV_CODEC_CAP_DELAY, then no samples will be returned.
  *
  * @warning The input buffer, avpkt->data must be AV_INPUT_BUFFER_PADDING_SIZE
  *          larger than the actual read bytes because some optimized bitstream
  *          readers read 32 or 64 bits at once and could read over the end.
  *
  * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
  * before packets may be fed to the decoder.
  *
  * @param      avctx the codec context
  * @param[out] frame The AVFrame in which to store decoded audio samples.
  *                   The decoder will allocate a buffer for the decoded frame by
  *                   calling the AVCodecContext.get_buffer2() callback.
  *                   When AVCodecContext.refcounted_frames is set to 1, the frame is
  *                   reference counted and the returned reference belongs to the
  *                   caller. The caller must release the frame using av_frame_unref()
  *                   when the frame is no longer needed. The caller may safely write
  *                   to the frame if av_frame_is_writable() returns 1.
  *                   When AVCodecContext.refcounted_frames is set to 0, the returned
  *                   reference belongs to the decoder and is valid only until the
  *                   next call to this function or until closing or flushing the
  *                   decoder. The caller may not write to it.
  * @param[out] got_frame_ptr Zero if no frame could be decoded, otherwise it is
  *                           non-zero. Note that this field being set to zero
  *                           does not mean that an error has occurred. For
  *                           decoders with AV_CODEC_CAP_DELAY set, no given decode
  *                           call is guaranteed to produce a frame.
  * @param[in]  avpkt The input AVPacket containing the input buffer.
  *                   At least avpkt->data and avpkt->size should be set. Some
  *                   decoders might also require additional fields to be set.
  * @return A negative error code is returned if an error occurred during
  *         decoding, otherwise the number of bytes consumed from the input
  *         AVPacket is returned.
  *
  * @deprecated Use avcodec_send_packet() and avcodec_receive_frame().
*)
// attribute_deprecated int avcodec_decode_audio4(AVCodecContext * avctx, AVFrame * frame, int * got_frame_ptr, const AVPacket * avpkt);
function avcodec_decode_audio4(avctx: pAVCodecContext; frame: pAVFrame; var got_frame_ptr: int; const avpkt: pAVPacket): int; cdecl; external avcodec_dll;
  deprecated;

(*
  * Decode the video frame of size avpkt->size from avpkt->data into picture.
  * Some decoders may support multiple frames in a single AVPacket, such
  * decoders would then just decode the first frame.
  *
  * @warning The input buffer must be AV_INPUT_BUFFER_PADDING_SIZE larger than
  * the actual read bytes because some optimized bitstream readers read 32 or 64
  * bits at once and could read over the end.
  *
  * @warning The end of the input buffer buf should be set to 0 to ensure that
  * no overreading happens for damaged MPEG streams.
  *
  * @note Codecs which have the AV_CODEC_CAP_DELAY capability set have a delay
  * between input and output, these need to be fed with avpkt->data=NULL,
  * avpkt->size=0 at the end to return the remaining frames.
  *
  * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
  * before packets may be fed to the decoder.
  *
  * @param avctx the codec context
  * @param[out] picture The AVFrame in which the decoded video frame will be stored.
  *             Use av_frame_alloc() to get an AVFrame. The codec will
  *             allocate memory for the actual bitmap by calling the
  *             AVCodecContext.get_buffer2() callback.
  *             When AVCodecContext.refcounted_frames is set to 1, the frame is
  *             reference counted and the returned reference belongs to the
  *             caller. The caller must release the frame using av_frame_unref()
  *             when the frame is no longer needed. The caller may safely write
  *             to the frame if av_frame_is_writable() returns 1.
  *             When AVCodecContext.refcounted_frames is set to 0, the returned
  *             reference belongs to the decoder and is valid only until the
  *             next call to this function or until closing or flushing the
  *             decoder. The caller may not write to it.
  *
  * @param[in] avpkt The input AVPacket containing the input buffer.
  *            You can create such packet with av_init_packet() and by then setting
  *            data and size, some decoders might in addition need other fields like
  *            flags&AV_PKT_FLAG_KEY. All decoders are designed to use the least
  *            fields possible.
  * @param[in,out] got_picture_ptr Zero if no frame could be decompressed, otherwise, it is nonzero.
  * @return On error a negative value is returned, otherwise the number of bytes
  * used or zero if no frame could be decompressed.
  *
  * @deprecated Use avcodec_send_packet() and avcodec_receive_frame().
*)
// attribute_deprecated int avcodec_decode_video2(AVCodecContext * avctx, AVFrame * picture, int * got_picture_ptr, const AVPacket * avpkt);
function avcodec_decode_video2(avctx: pAVCodecContext; picture: pAVFrame; var got_picture_ptr: int; const avpkt: pAVPacket): int; cdecl; external avcodec_dll;
  deprecated;

(*
  * Decode a subtitle message.
  * Return a negative value on error, otherwise return the number of bytes used.
  * If no subtitle could be decompressed, got_sub_ptr is zero.
  * Otherwise, the subtitle is stored in *sub.
  * Note that AV_CODEC_CAP_DR1 is not available for subtitle codecs. This is for
  * simplicity, because the performance difference is expect to be negligible
  * and reusing a get_buffer written for video codecs would probably perform badly
  * due to a potentially very different allocation pattern.
  *
  * Some decoders (those marked with AV_CODEC_CAP_DELAY) have a delay between input
  * and output. This means that for some packets they will not immediately
  * produce decoded output and need to be flushed at the end of decoding to get
  * all the decoded data. Flushing is done by calling this function with packets
  * with avpkt->data set to NULL and avpkt->size set to 0 until it stops
  * returning subtitles. It is safe to flush even those decoders that are not
  * marked with AV_CODEC_CAP_DELAY, then no subtitles will be returned.
  *
  * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
  * before packets may be fed to the decoder.
  *
  * @param avctx the codec context
  * @param[out] sub The Preallocated AVSubtitle in which the decoded subtitle will be stored,
  *                 must be freed with avsubtitle_free if *got_sub_ptr is set.
  * @param[in,out] got_sub_ptr Zero if no subtitle could be decompressed, otherwise, it is nonzero.
  * @param[in] avpkt The input AVPacket containing the input buffer.
*)
// int avcodec_decode_subtitle2(AVCodecContext * avctx, AVSubtitle * sub, int * got_sub_ptr, AVPacket * avpkt);
function avcodec_decode_subtitle2(avctx: pAVCodecContext; var sub: AVSubtitle; var got_sub_ptr: int; avpkt: pAVPacket): int; cdecl; external avcodec_dll;

(*
  * Supply raw packet data as input to a decoder.
  *
  * Internally, this call will copy relevant AVCodecContext fields, which can
  * influence decoding per-packet, and apply them when the packet is actually
  * decoded. (For example AVCodecContext.skip_frame, which might direct the
  * decoder to drop the frame contained by the packet sent with this function.)
  *
  * @warning The input buffer, avpkt->data must be AV_INPUT_BUFFER_PADDING_SIZE
  *          larger than the actual read bytes because some optimized bitstream
  *          readers read 32 or 64 bits at once and could read over the end.
  *
  * @warning Do not mix this API with the legacy API (like avcodec_decode_video2())
  *          on the same AVCodecContext. It will return unexpected results now
  *          or in future libavcodec versions.
  *
  * @note The AVCodecContext MUST have been opened with @ref avcodec_open2()
  *       before packets may be fed to the decoder.
  *
  * @param avctx codec context
  * @param[in] avpkt The input AVPacket. Usually, this will be a single video
  *                  frame, or several complete audio frames.
  *                  Ownership of the packet remains with the caller, and the
  *                  decoder will not write to the packet. The decoder may create
  *                  a reference to the packet data (or copy it if the packet is
  *                  not reference-counted).
  *                  Unlike with older APIs, the packet is always fully consumed,
  *                  and if it contains multiple frames (e.g. some audio codecs),
  *                  will require you to call avcodec_receive_frame() multiple
  *                  times afterwards before you can send a new packet.
  *                  It can be NULL (or an AVPacket with data set to NULL and
  *                  size set to 0); in this case, it is considered a flush
  *                  packet, which signals the end of the stream. Sending the
  *                  first flush packet will return success. Subsequent ones are
  *                  unnecessary and will return AVERROR_EOF. If the decoder
  *                  still has frames buffered, it will return them after sending
  *                  a flush packet.
  *
  * @return 0 on success, otherwise negative error code:
  *      AVERROR(EAGAIN):   input is not accepted in the current state - user
  *                         must read output with avcodec_receive_frame() (once
  *                         all output is read, the packet should be resent, and
  *                         the call will not fail with EAGAIN).
  *      AVERROR_EOF:       the decoder has been flushed, and no new packets can
  *                         be sent to it (also returned if more than 1 flush
  *                         packet is sent)
  *      AVERROR(EINVAL):   codec not opened, it is an encoder, or requires flush
  *      AVERROR(ENOMEM):   failed to add packet to internal queue, or similar
  *      other errors: legitimate decoding errors
*)
// int avcodec_send_packet(AVCodecContext * avctx, const AVPacket * avpkt);
function avcodec_send_packet(avctx: pAVCodecContext; const avpkt: pAVPacket): int; cdecl; overload; external avcodec_dll;
function avcodec_send_packet(avctx: pAVCodecContext; var avpkt: AVPacket): int; cdecl; overload; external avcodec_dll;

(*
  * Return decoded output data from a decoder.
  *
  * @param avctx codec context
  * @param frame This will be set to a reference-counted video or audio
  *              frame (depending on the decoder type) allocated by the
  *              decoder. Note that the function will always call
  *              av_frame_unref(frame) before doing anything else.
  *
  * @return
  *      0:                 success, a frame was returned
  *      AVERROR(EAGAIN):   output is not available in this state - user must try
  *                         to send new input
  *      AVERROR_EOF:       the decoder has been fully flushed, and there will be
  *                         no more output frames
  *      AVERROR(EINVAL):   codec not opened, or it is an encoder
  *      AVERROR_INPUT_CHANGED:   current decoded frame has changed parameters
  *                               with respect to first decoded frame. Applicable
  *                               when flag AV_CODEC_FLAG_DROPCHANGED is set.
  *      other negative values: legitimate decoding errors
*)
// int avcodec_receive_frame(AVCodecContext * avctx, AVFrame * frame);
function avcodec_receive_frame(avctx: pAVCodecContext; frame: pAVFrame): int; cdecl; external avcodec_dll;

(*
  * Supply a raw video or audio frame to the encoder. Use avcodec_receive_packet()
  * to retrieve buffered output packets.
  *
  * @param avctx     codec context
  * @param[in] frame AVFrame containing the raw audio or video frame to be encoded.
  *                  Ownership of the frame remains with the caller, and the
  *                  encoder will not write to the frame. The encoder may create
  *                  a reference to the frame data (or copy it if the frame is
  *                  not reference-counted).
  *                  It can be NULL, in which case it is considered a flush
  *                  packet.  This signals the end of the stream. If the encoder
  *                  still has packets buffered, it will return them after this
  *                  call. Once flushing mode has been entered, additional flush
  *                  packets are ignored, and sending frames will return
  *                  AVERROR_EOF.
  *
  *                  For audio:
  *                  If AV_CODEC_CAP_VARIABLE_FRAME_SIZE is set, then each frame
  *                  can have any number of samples.
  *                  If it is not set, frame->nb_samples must be equal to
  *                  avctx->frame_size for all frames except the last.
  *                  The final frame may be smaller than avctx->frame_size.
  * @return 0 on success, otherwise negative error code:
  *      AVERROR(EAGAIN):   input is not accepted in the current state - user
  *                         must read output with avcodec_receive_packet() (once
  *                         all output is read, the packet should be resent, and
  *                         the call will not fail with EAGAIN).
  *      AVERROR_EOF:       the encoder has been flushed, and no new frames can
  *                         be sent to it
  *      AVERROR(EINVAL):   codec not opened, refcounted_frames not set, it is a
  *                         decoder, or requires flush
  *      AVERROR(ENOMEM):   failed to add packet to internal queue, or similar
  *      other errors: legitimate decoding errors
*)
// int avcodec_send_frame(AVCodecContext * avctx, const AVFrame * frame);
function avcodec_send_frame(avctx: pAVCodecContext; const frame: pAVFrame): int; cdecl; external avcodec_dll;

(*
  * Read encoded data from the encoder.
  *
  * @param avctx codec context
  * @param avpkt This will be set to a reference-counted packet allocated by the
  *              encoder. Note that the function will always call
  *              av_frame_unref(frame) before doing anything else.
  * @return 0 on success, otherwise negative error code:
  *      AVERROR(EAGAIN):   output is not available in the current state - user
  *                         must try to send input
  *      AVERROR_EOF:       the encoder has been fully flushed, and there will be
  *                         no more output packets
  *      AVERROR(EINVAL):   codec not opened, or it is an encoder
  *      other errors: legitimate decoding errors
*)
// int avcodec_receive_packet(AVCodecContext * avctx, AVPacket * avpkt);
function avcodec_receive_packet(avctx: pAVCodecContext; avpkt: pAVPacket): int; cdecl; overload; external avcodec_dll;
function avcodec_receive_packet(avctx: pAVCodecContext; var avpkt: AVPacket): int; cdecl; overload; external avcodec_dll;

(*
  * Create and return a AVHWFramesContext with values adequate for hardware
  * decoding. This is meant to get called from the get_format callback, and is
  * a helper for preparing a AVHWFramesContext for AVCodecContext.hw_frames_ctx.
  * This API is for decoding with certain hardware acceleration modes/APIs only.
  *
  * The returned AVHWFramesContext is not initialized. The caller must do this
  * with av_hwframe_ctx_init().
  *
  * Calling this function is not a requirement, but makes it simpler to avoid
  * codec or hardware API specific details when manually allocating frames.
  *
  * Alternatively to this, an API user can set AVCodecContext.hw_device_ctx,
  * which sets up AVCodecContext.hw_frames_ctx fully automatically, and makes
  * it unnecessary to call this function or having to care about
  * AVHWFramesContext initialization at all.
  *
  * There are a number of requirements for calling this function:
  *
  * - It must be called from get_format with the same avctx parameter that was
  *   passed to get_format. Calling it outside of get_format is not allowed, and
  *   can trigger undefined behavior.
  * - The function is not always supported (see description of return values).
  *   Even if this function returns successfully, hwaccel initialization could
  *   fail later. (The degree to which implementations check whether the stream
  *   is actually supported varies. Some do this check only after the user's
  *   get_format callback returns.)
  * - The hw_pix_fmt must be one of the choices suggested by get_format. If the
  *   user decides to use a AVHWFramesContext prepared with this API function,
  *   the user must return the same hw_pix_fmt from get_format.
  * - The device_ref passed to this function must support the given hw_pix_fmt.
  * - After calling this API function, it is the user's responsibility to
  *   initialize the AVHWFramesContext (returned by the out_frames_ref parameter),
  *   and to set AVCodecContext.hw_frames_ctx to it. If done, this must be done
  *   before returning from get_format (this is implied by the normal
  *   AVCodecContext.hw_frames_ctx API rules).
  * - The AVHWFramesContext parameters may change every time time get_format is
  *   called. Also, AVCodecContext.hw_frames_ctx is reset before get_format. So
  *   you are inherently required to go through this process again on every
  *   get_format call.
  * - It is perfectly possible to call this function without actually using
  *   the resulting AVHWFramesContext. One use-case might be trying to reuse a
  *   previously initialized AVHWFramesContext, and calling this API function
  *   only to test whether the required frame parameters have changed.
  * - Fields that use dynamically allocated values of any kind must not be set
  *   by the user unless setting them is explicitly allowed by the documentation.
  *   If the user sets AVHWFramesContext.free and AVHWFramesContext.user_opaque,
  *   the new free callback must call the potentially set previous free callback.
  *   This API call may set any dynamically allocated fields, including the free
  *   callback.
  *
  * The function will set at least the following fields on AVHWFramesContext
  * (potentially more, depending on hwaccel API):
  *
  * - All fields set by av_hwframe_ctx_alloc().
  * - Set the format field to hw_pix_fmt.
  * - Set the sw_format field to the most suited and most versatile format. (An
  *   implication is that this will prefer generic formats over opaque formats
  *   with arbitrary restrictions, if possible.)
  * - Set the width/height fields to the coded frame size, rounded up to the
  *   API-specific minimum alignment.
  * - Only _if_ the hwaccel requires a pre-allocated pool: set the initial_pool_size
  *   field to the number of maximum reference surfaces possible with the codec,
  *   plus 1 surface for the user to work (meaning the user can safely reference
  *   at most 1 decoded surface at a time), plus additional buffering introduced
  *   by frame threading. If the hwaccel does not require pre-allocation, the
  *   field is left to 0, and the decoder will allocate new surfaces on demand
  *   during decoding.
  * - Possibly AVHWFramesContext.hwctx fields, depending on the underlying
  *   hardware API.
  *
  * Essentially, out_frames_ref returns the same as av_hwframe_ctx_alloc(), but
  * with basic frame parameters set.
  *
  * The function is stateless, and does not change the AVCodecContext or the
  * device_ref AVHWDeviceContext.
  *
  * @param avctx The context which is currently calling get_format, and which
  *              implicitly contains all state needed for filling the returned
  *              AVHWFramesContext properly.
  * @param device_ref A reference to the AVHWDeviceContext describing the device
  *                   which will be used by the hardware decoder.
  * @param hw_pix_fmt The hwaccel format you are going to return from get_format.
  * @param out_frames_ref On success, set to a reference to an _uninitialized_
  *                       AVHWFramesContext, created from the given device_ref.
  *                       Fields will be set to values required for decoding.
  *                       Not changed if an error is returned.
  * @return zero on success, a negative value on error. The following error codes
  *         have special semantics:
  *      AVERROR(ENOENT): the decoder does not support this functionality. Setup
  *                       is always manual, or it is a decoder which does not
  *                       support setting AVCodecContext.hw_frames_ctx at all,
  *                       or it is a software format.
  *      AVERROR(EINVAL): it is known that hardware decoding is not supported for
  *                       this configuration, or the device_ref is not supported
  *                       for the hwaccel referenced by hw_pix_fmt.
*)
// int avcodec_get_hw_frames_parameters(AVCodecContext * avctx, AVBufferRef * device_ref, enum AVPixelFormat hw_pix_fmt,
// AVBufferRef * * out_frames_ref);
function avcodec_get_hw_frames_parameters(avctx: pAVCodecContext; device_ref: pAVBufferRef; hw_pix_fmt: AVPixelFormat; var out_frames_ref: pAVBufferRef): int;
  cdecl; external avcodec_dll;

const
  AV_PARSER_PTS_NB = 4;

  PARSER_FLAG_COMPLETE_FRAMES = $0001;
  PARSER_FLAG_ONCE            = $0002;
  // Set if the parser has a valid file offset
  PARSER_FLAG_FETCHED_OFFSET = $0004;
  PARSER_FLAG_USE_CODEC_TS   = $1000;

type

  TAPPNArray_int64_t = array [0 .. AV_PARSER_PTS_NB - 1] of int64_t;

  AVPictureStructure = (               //
    AV_PICTURE_STRUCTURE_UNKNOWN,      // < unknown
    AV_PICTURE_STRUCTURE_TOP_FIELD,    // < coded as top field
    AV_PICTURE_STRUCTURE_BOTTOM_FIELD, // < coded as bottom field
    AV_PICTURE_STRUCTURE_FRAME         // < coded as frame
    );

  pAVCodecParserContext = ^AVCodecParserContext;
  pAVCodecParser = ^AVCodecParser;

  AVCodecParserContext = record
    priv_data: Pointer;
    parser: pAVCodecParser;
    frame_offset: int64_t; (* offset of the current frame *)
    cur_offset: int64_t; (* current offset
      (incremented by each av_parser_parse()) *)
    next_frame_offset: int64_t; (* offset of the next frame *)
    (* video info *)
    pict_type: int; (* XXX: Put it back in AVCodecContext. *)
    (*
      * This field is used for proper frame duration computation in lavf.
      * It signals, how much longer the frame duration of the current frame
      * is compared to normal frame duration.
      *
      * frame_duration = (1 + repeat_pict) * time_base
      *
      * It is used by codecs like H.264 to display telecined material.
    *)
    repeat_pict: int; (* XXX: Put it back in AVCodecContext. *)
    pts: int64_t;     (* pts of the current frame *)
    dts: int64_t;     (* dts of the current frame *)

    (* private data *)
    last_pts: int64_t;
    last_dts: int64_t;
    fetch_timestamp: int;

    cur_frame_start_index: int;
    cur_frame_offset: TAPPNArray_int64_t;
    cur_frame_pts: TAPPNArray_int64_t;
    cur_frame_dts: TAPPNArray_int64_t;

    flags: int;

    offset: int64_t; // < byte offset from starting packet start
    cur_frame_end: TAPPNArray_int64_t;

    (*
      * Set by parser to 1 for key frames and 0 for non-key frames.
      * It is initialized to -1, so if the parser doesn't set this flag,
      * old-style fallback using AV_PICTURE_TYPE_I picture type as key frames
      * will be used.
    *)
    key_frame: int;

{$IFDEF FF_API_CONVERGENCE_DURATION}
    (*
      * @deprecated unused
    *)
    // attribute_deprecated
    convergence_duration: int64_t deprecated;
{$ENDIF}
    // Timestamp generation support:
    (*
      * Synchronization point for start of timestamp generation.
      *
      * Set to >0 for sync point, 0 for no sync point and <0 for undefined
      * (default).
      *
      * For example, this corresponds to presence of H.264 buffering period
      * SEI message.
    *)
    dts_sync_point: int;

    (*
      * Offset of the current timestamp against last timestamp sync point in
      * units of AVCodecContext.time_base.
      *
      * Set to INT_MIN when dts_sync_point unused. Otherwise, it must
      * contain a valid timestamp offset.
      *
      * Note that the timestamp of sync point has usually a nonzero
      * dts_ref_dts_delta, which refers to the previous sync point. Offset of
      * the next frame after timestamp sync point will be usually 1.
      *
      * For example, this corresponds to H.264 cpb_removal_delay.
    *)
    dts_ref_dts_delta: int;

    (*
      * Presentation delay of current frame in units of AVCodecContext.time_base.
      *
      * Set to INT_MIN when dts_sync_point unused. Otherwise, it must
      * contain valid non-negative timestamp delta (presentation time of a frame
      * must not lie in the past).
      *
      * This delay represents the difference between decoding and presentation
      * time of the frame.
      *
      * For example, this corresponds to H.264 dpb_output_delay.
    *)
    pts_dts_delta: int;

    (*
      * Position of the packet in file.
      *
      * Analogous to cur_frame_pts/dts
    *)
    cur_frame_pos: TAPPNArray_int64_t;

    (*
      * Byte position of currently parsed frame in stream.
    *)
    pos: int64_t;

    (*
      * Previous frame byte position.
    *)
    last_pos: int64_t;

    (*
      * Duration of the current frame.
      * For audio, this is in units of 1 / AVCodecContext.sample_rate.
      * For all other types, this is in units of AVCodecContext.time_base.
    *)
    duration: int;

    field_order: AVFieldOrder;

    (*
      * Indicate whether a picture is coded as a frame, top field or bottom field.
      *
      * For example, H.264 field_pic_flag equal to 0 corresponds to
      * AV_PICTURE_STRUCTURE_FRAME. An H.264 picture with field_pic_flag
      * equal to 1 and bottom_field_flag equal to 0 corresponds to
      * AV_PICTURE_STRUCTURE_TOP_FIELD.
    *)
    picture_structure: AVPictureStructure;

    (*
      * Picture number incremented in presentation or output order.
      * This field may be reinitialized at the first picture of a new sequence.
      *
      * For example, this corresponds to H.264 PicOrderCnt.
    *)
    output_picture_number: int;

    (*
      * Dimensions of the decoded video intended for presentation.
    *)
    width: int;
    height: int;

    (*
      * Dimensions of the coded video.
    *)
    coded_width: int;
    coded_height: int;

    (*
      * The format of the coded data, corresponds to enum AVPixelFormat for video
      * and for enum AVSampleFormat for audio.
      *
      * Note that a decoder can have considerable freedom in how exactly it
      * decodes the data, so the format reported here might be different from the
      * one returned by a decoder.
    *)
    format: int;
  end;

  Tcodec_ids_array_5_int = array [0 .. 4] of int;

  AVCodecParser = record
    codec_ids: Tcodec_ids_array_5_int; (* several codec IDs are permitted *)
    priv_data_size: int;
    // int (*parser_init)(AVCodecParserContext *s);
    parser_init: function(s: pAVCodecParserContext): int; cdecl;
    (* This callback never returns an error, a negative value means that
      * the frame start was in a previous packet. *)
    // int (*parser_parse)(AVCodecParserContext *s, AVCodecContext *avctx, const uint8_t **poutbuf, int *poutbuf_size, const uint8_t *buf, int buf_size);
    parser_parse: function(s: pAVCodecParserContext; avctx: pAVCodecContext; const poutbuf: puint8_t; poutbuf_size: pInt; const buf: puint8_t; buf_size: int)
      : int; cdecl;

    // void (*parser_close)(AVCodecParserContext *s);
    parser_close: procedure(s: pAVCodecParserContext); cdecl;
    // int (*split)(AVCodecContext *avctx, const uint8_t *buf, int buf_size);
    split: function(avctx: pAVCodecContext; const buf: puint8_t; buf_size: int): int; cdecl;
    next: pAVCodecParser;
  end;

  (*
    * Iterate over all registered codec parsers.
    *
    * @param opaque a pointer where libavcodec will store the iteration state. Must
    *               point to NULL to start the iteration.
    *
    * @return the next registered codec parser or NULL when the iteration is
    *         finished
  *)
  // const AVCodecParser * av_parser_iterate(void * * opaque);
function av_parser_iterate(var opaque: Pointer): pAVCodecParser; cdecl; external avcodec_dll;

// attribute_deprecated AVCodecParser * av_parser_next(const AVCodecParser * c);
function av_parser_next(const c: pAVCodecParser): pAVCodecParser; cdecl; external avcodec_dll; deprecated;

// attribute_deprecated void av_register_codec_parser(AVCodecParser * parser);
procedure av_register_codec_parser(parser: pAVCodecParser); cdecl; external avcodec_dll; deprecated;

// AVCodecParserContext * av_parser_init(int codec_id);
function av_parser_init(codec_id: int): pAVCodecParserContext; cdecl; external avcodec_dll; deprecated;

(*
  * Parse a packet.
  *
  * @param s             parser context.
  * @param avctx         codec context.
  * @param poutbuf       set to pointer to parsed buffer or NULL if not yet finished.
  * @param poutbuf_size  set to size of parsed buffer or zero if not yet finished.
  * @param buf           input buffer.
  * @param buf_size      buffer size in bytes without the padding. I.e. the full buffer
  size is assumed to be buf_size + AV_INPUT_BUFFER_PADDING_SIZE.
  To signal EOF, this should be 0 (so that the last frame
  can be output).
  * @param pts           input presentation timestamp.
  * @param dts           input decoding timestamp.
  * @param pos           input byte position in stream.
  * @return the number of bytes of the input bitstream used.
  *
  * Example:
  * @code
  *   while(in_len){
  *       len = av_parser_parse2(myparser, AVCodecContext, &data, &size,
  *                                        in_data, in_len,
  *                                        pts, dts, pos);
  *       in_data += len;
  *       in_len  -= len;
  *
  *       if(size)
  *          decode_frame(data, size);
  *   }
  * @endcode
*)
// int av_parser_parse2(AVCodecParserContext * s, AVCodecContext * avctx, uint8_t * * poutbuf, int * poutbuf_size, const uint8_t * buf,
// int buf_size, int64_t pts, int64_t dts, int64_t pos);
function av_parser_parse2(s: pAVCodecParserContext; avctx: pAVCodecContext; var poutbuf: puint8_t; var poutbuf_size: int; const buf: puint8_t; buf_size: int;
  pts: int64_t; dts: int64_t; pos: int64_t): int; cdecl; external avcodec_dll;

(*
  * @return 0 if the output buffer is a subset of the input, 1 if it is allocated and must be freed
  * @deprecated use AVBitStreamFilter
*)
// int av_parser_change(AVCodecParserContext * s, AVCodecContext * avctx, uint8_t **poutbuf, int * poutbuf_size, const uint8_t *buf,
// int buf_size, int keyframe);
function av_parser_change(s: pAVCodecParserContext; avctx: pAVCodecContext; var poutbuf: puint8_t; var poutbuf_size: int; const buf: puint8_t; buf_size: int;
  keyframe: int): int; cdecl; external avcodec_dll; deprecated 'deprecated use AVBitStreamFilter';

// void av_parser_close(AVCodecParserContext * s);
procedure av_parser_close(s: pAVCodecParserContext); cdecl; external avcodec_dll; deprecated;

(*
  * Find a registered encoder with a matching codec ID.
  *
  * @param id AVCodecID of the requested encoder
  * @return An encoder if one was found, NULL otherwise.
*)
// avcodec * avcodec_find_encoder(enum AVCodecID id);
function avcodec_find_encoder(id: AVCodecID): pAVCodec; cdecl; external avcodec_dll;

(*
  * Find a registered encoder with the specified name.
  *
  * @param name name of the requested encoder
  * @return An encoder if one was found, NULL otherwise.
*)
// avcodec * avcodec_find_encoder_by_name(const char * name);
function avcodec_find_encoder_by_name(const name: pAnsiChar): pAVCodec; cdecl; external avcodec_dll;

(*
  * Encode a frame of audio.
  *
  * Takes input samples from frame and writes the next output packet, if
  * available, to avpkt. The output packet does not necessarily contain data for
  * the most recent frame, as encoders can delay, split, and combine input frames
  * internally as needed.
  *
  * @param avctx     codec context
  * @param avpkt     output AVPacket.
  *                  The user can supply an output buffer by setting
  *                  avpkt->data and avpkt->size prior to calling the
  *                  function, but if the size of the user-provided data is not
  *                  large enough, encoding will fail. If avpkt->data and
  *                  avpkt->size are set, avpkt->destruct must also be set. All
  *                  other AVPacket fields will be reset by the encoder using
  *                  av_init_packet(). If avpkt->data is NULL, the encoder will
  *                  allocate it. The encoder will set avpkt->size to the size
  *                  of the output packet.
  *
  *                  If this function fails or produces no output, avpkt will be
  *                  freed using av_packet_unref().
  * @param[in] frame AVFrame containing the raw audio data to be encoded.
  *                  May be NULL when flushing an encoder that has the
  *                  AV_CODEC_CAP_DELAY capability set.
  *                  If AV_CODEC_CAP_VARIABLE_FRAME_SIZE is set, then each frame
  *                  can have any number of samples.
  *                  If it is not set, frame->nb_samples must be equal to
  *                  avctx->frame_size for all frames except the last.
  *                  The final frame may be smaller than avctx->frame_size.
  * @param[out] got_packet_ptr This field is set to 1 by libavcodec if the
  *                            output packet is non-empty, and to 0 if it is
  *                            empty. If the function returns an error, the
  *                            packet can be assumed to be invalid, and the
  *                            value of got_packet_ptr is undefined and should
  *                            not be used.
  * @return          0 on success, negative error code on failure
  *
  * @deprecated use avcodec_send_frame()/avcodec_receive_packet() instead
*)
// attribute_deprecated
// int avcodec_encode_audio2(AVCodecContext * avctx, AVPacket * avpkt, const AVFrame * frame, int * got_packet_ptr);
function avcodec_encode_audio2(avctx: pAVCodecContext; avpkt: pAVPacket; const frame: pAVFrame; var got_packet_ptr: int): int; cdecl; external avcodec_dll;
  deprecated;

(*
  * Encode a frame of video.
  *
  * Takes input raw video data from frame and writes the next output packet, if
  * available, to avpkt. The output packet does not necessarily contain data for
  * the most recent frame, as encoders can delay and reorder input frames
  * internally as needed.
  *
  * @param avctx     codec context
  * @param avpkt     output AVPacket.
  *                  The user can supply an output buffer by setting
  *                  avpkt->data and avpkt->size prior to calling the
  *                  function, but if the size of the user-provided data is not
  *                  large enough, encoding will fail. All other AVPacket fields
  *                  will be reset by the encoder using av_init_packet(). If
  *                  avpkt->data is NULL, the encoder will allocate it.
  *                  The encoder will set avpkt->size to the size of the
  *                  output packet. The returned data (if any) belongs to the
  *                  caller, he is responsible for freeing it.
  *
  *                  If this function fails or produces no output, avpkt will be
  *                  freed using av_packet_unref().
  * @param[in] frame AVFrame containing the raw video data to be encoded.
  *                  May be NULL when flushing an encoder that has the
  *                  AV_CODEC_CAP_DELAY capability set.
  * @param[out] got_packet_ptr This field is set to 1 by libavcodec if the
  *                            output packet is non-empty, and to 0 if it is
  *                            empty. If the function returns an error, the
  *                            packet can be assumed to be invalid, and the
  *                            value of got_packet_ptr is undefined and should
  *                            not be used.
  * @return          0 on success, negative error code on failure
  *
  * @deprecated use avcodec_send_frame()/avcodec_receive_packet() instead
*)
// attribute_deprecated
// int avcodec_encode_video2(AVCodecContext * avctx, AVPacket * avpkt, const AVFrame * frame, int * got_packet_ptr);
function avcodec_encode_video2(avctx: pAVCodecContext; avpkt: pAVPacket; const frame: pAVFrame; var got_packet_ptr: int): int; cdecl; overload;
  external avcodec_dll; deprecated;
function avcodec_encode_video2(avctx: pAVCodecContext; var avpkt: AVPacket; const frame: pAVFrame; var got_packet_ptr: int): int; cdecl; overload;
  external avcodec_dll; deprecated;

// int avcodec_encode_subtitle(AVCodecContext * avctx, uint8_t * buf, int buf_size, const AVSubtitle * sub);
function avcodec_encode_subtitle(avctx: pAVCodecContext; buf: puint8_t; buf_size: int; const sub: pAVSubtitle): int; cdecl; external avcodec_dll;

{$IFDEF FF_API_AVPICTURE}
(*
  * @deprecated unused
*)
// attribute_deprecated
// int avpicture_alloc(AVPicture * picture, enum AVPixelFormat pix_fmt, int width, int height);
function avpicture_alloc(picture: pAVPicture; pix_fmt: AVPixelFormat; width: int; height: int): int; cdecl; overload; external avcodec_dll; deprecated;
function avpicture_alloc(Var picture: AVPicture; pix_fmt: AVPixelFormat; width: int; height: int): int; cdecl; overload; external avcodec_dll; deprecated;

(*
  * @deprecated unused
*)
// attribute_deprecated
// void avpicture_free(AVPicture * picture);
procedure avpicture_free(picture: pAVPicture); cdecl; external avcodec_dll; deprecated;

(*
  * @deprecated use av_image_fill_arrays() instead.
*)
// attribute_deprecated
// int avpicture_fill(AVPicture * picture, const uint8_t * ptr, enum AVPixelFormat pix_fmt, int width, int height);
function avpicture_fill(picture: pAVPicture; const ptr: puint8_t; pix_fmt: AVPixelFormat; width: int; height: int): int; cdecl; external avcodec_dll;

(*
  * @deprecated use av_image_copy_to_buffer() instead.
*)
// attribute_deprecated
// int avpicture_layout(const AVPicture * src, enum AVPixelFormat pix_fmt, int width, int height, unsigned char * dest, int dest_size);
function avpicture_layout(const src: pAVPicture; pix_fmt: AVPixelFormat; width: int; height: int; dest: punsignedchar; dest_size: int): int; cdecl;
  external avcodec_dll; deprecated;

(*
  * @deprecated use av_image_get_buffer_size() instead.
*)
// attribute_deprecated
// int avpicture_get_size(enum AVPixelFormat pix_fmt, int width, int height);
function avpicture_get_size(pix_fmt: AVPixelFormat; width: int; height: int): int; cdecl; external avcodec_dll; deprecated;

(*
  * @deprecated av_image_copy() instead.
*)
// attribute_deprecated
// void av_picture_copy(AVPicture * dst, const AVPicture * src, enum AVPixelFormat pix_fmt, int width, int height);
procedure av_picture_copy(dst: pAVPicture; const src: pAVPicture; pix_fmt: AVPixelFormat; width: int; height: int); cdecl; external avcodec_dll; deprecated;

(*
  * @deprecated unused
*)
// attribute_deprecated
// int av_picture_crop(AVPicture * dst, const AVPicture * src, enum AVPixelFormat pix_fmt, int top_band, int left_band);
function av_picture_crop(dst: pAVPicture; const src: pAVPicture; pix_fmt: AVPixelFormat; top_band: int; left_band: int): int; cdecl; external avcodec_dll;

(*
  * @deprecated unused
*)
// attribute_deprecated
// int av_picture_pad(AVPicture * dst, const AVPicture * src, int height, int width, enum AVPixelFormat pix_fmt, int padtop, int padbottom, int padleft, int padright, int * color);
function av_picture_pad(dst: pAVPicture; const src: pAVPicture; height: int; width: int; pix_fmt: AVPixelFormat; padtop: int; padbottom: int; padleft: int;
  padright: int; var color: int): int; cdecl; external avcodec_dll; deprecated;

{$ENDIF}
(*
  * @defgroup lavc_misc Utility functions
  * @ingroup libavc
  *
  * Miscellaneous utility functions related to both encoding and decoding
  * (or neither).
  * @{
*)

(*
  * @defgroup lavc_misc_pixfmt Pixel formats
  *
  * Functions for working with pixel formats.
  * @{
*)

{$IFDEF FF_API_GETCHROMA}
(*
  * @deprecated Use av_pix_fmt_get_chroma_sub_sample
*)

// attribute_deprecated
// void avcodec_get_chroma_sub_sample(enum AVPixelFormat pix_fmt, int * h_shift, int * v_shift);
procedure avcodec_get_chroma_sub_sample(pix_fmt: AVPixelFormat; var h_shift, v_shift: int); cdecl; external avcodec_dll; deprecated;
{$ENDIF}
(*
  * Return a value representing the fourCC code associated to the
  * pixel format pix_fmt, or 0 if no associated fourCC code can be
  * found.
*)
// unsigned int avcodec_pix_fmt_to_codec_tag(enum AVPixelFormat pix_fmt);
function avcodec_pix_fmt_to_codec_tag(pix_fmt: AVPixelFormat): unsignedint; cdecl; external avcodec_dll;

(*
  * @deprecated see av_get_pix_fmt_loss()
*)
// int avcodec_get_pix_fmt_loss(enum AVPixelFormat dst_pix_fmt, enum AVPixelFormat src_pix_fmt, int has_alpha);
function avcodec_get_pix_fmt_loss(dst_pix_fmt: AVPixelFormat; src_pix_fmt: AVPixelFormat; has_alpha: int): int; cdecl; external avcodec_dll;

(*
  * Find the best pixel format to convert to given a certain source pixel
  * format.  When converting from one pixel format to another, information loss
  * may occur.  For example, when converting from RGB24 to GRAY, the color
  * information will be lost. Similarly, other losses occur when converting from
  * some formats to other formats. avcodec_find_best_pix_fmt_of_2() searches which of
  * the given pixel formats should be used to suffer the least amount of loss.
  * The pixel formats from which it chooses one, are determined by the
  * pix_fmt_list parameter.
  *
  *
  * @param[in] pix_fmt_list AV_PIX_FMT_NONE terminated array of pixel formats to choose from
  * @param[in] src_pix_fmt source pixel format
  * @param[in] has_alpha Whether the source pixel format alpha channel is used.
  * @param[out] loss_ptr Combination of flags informing you what kind of losses will occur.
  * @return The best pixel format to convert to or -1 if none was found.
*)
// enum AVPixelFormat avcodec_find_best_pix_fmt_of_list(const enum AVPixelFormat * pix_fmt_list, enum AVPixelFormat src_pix_fmt, int has_alpha,
// int * loss_ptr);
function avcodec_find_best_pix_fmt_of_list(const pix_fmt_list: pAVPixelFormat; src_pix_fmt: AVPixelFormat; has_alpha: int; var loss_ptr: int): AVPixelFormat;
  cdecl; external avcodec_dll;

(*
  * @deprecated see av_find_best_pix_fmt_of_2()
*)
// enum AVPixelFormat avcodec_find_best_pix_fmt_of_2(enum AVPixelFormat dst_pix_fmt1, enum AVPixelFormat dst_pix_fmt2,
// enum AVPixelFormat src_pix_fmt, int has_alpha, int * loss_ptr);
function avcodec_find_best_pix_fmt_of_2(dst_pix_fmt1: AVPixelFormat; dst_pix_fmt2: AVPixelFormat; src_pix_fmt: AVPixelFormat; has_alpha: int; var loss_ptr: int)
  : AVPixelFormat; cdecl; external avcodec_dll;

// attribute_deprecated
// enum AVPixelFormat avcodec_find_best_pix_fmt2(enum AVPixelFormat dst_pix_fmt1, enum AVPixelFormat dst_pix_fmt2,
// enum AVPixelFormat src_pix_fmt, int has_alpha, int * loss_ptr);
function avcodec_find_best_pix_fmt2(dst_pix_fmt1: AVPixelFormat; dst_pix_fmt2: AVPixelFormat; src_pix_fmt: AVPixelFormat; has_alpha: int; var loss_ptr: int)
  : AVPixelFormat; cdecl; external avcodec_dll; deprecated;

// enum AVPixelFormat avcodec_default_get_format(struct AVCodecContext * s, const enum AVPixelFormat * fmt);
function avcodec_default_get_format(s: pAVCodecContext; const fmt: pAVPixelFormat): AVPixelFormat; cdecl; external avcodec_dll;

{$IFDEF FF_API_TAG_STRING}
(*
  * Put a string representing the codec tag codec_tag in buf.
  *
  * @param buf       buffer to place codec tag in
  * @param buf_size size in bytes of buf
  * @param codec_tag codec tag to assign
  * @return the length of the string that would have been generated if
  * enough space had been available, excluding the trailing null
  *
  * @deprecated see av_fourcc_make_string() and av_fourcc2str().
*)
// attribute_deprecated
// size_t av_get_codec_tag_string(char * buf, size_t buf_size, unsigned int codec_tag);
function av_get_codec_tag_string(buf: pAnsiChar; buf_size: size_t; codec_tag: unsignedint): size_t; cdecl; external avcodec_dll; deprecated;
{$ENDIF}
// void avcodec_string(char * buf, int buf_size, AVCodecContext * enc, int encode);
procedure avcodec_string(buf: pAnsiChar; buf_size: int; enc: pAVCodecContext; encode: int); cdecl; external avcodec_dll;

(*
  * Return a name for the specified profile, if available.
  *
  * @param codec the codec that is searched for the given profile
  * @param profile the profile value for which a name is requested
  * @return A name for the profile if found, NULL otherwise.
*)
// const char * av_get_profile_name(const avcodec * codec, int profile);
function av_get_profile_name(const codec: pAVCodec; profile: int): pAnsiChar; cdecl; external avcodec_dll;

(*
  * Return a name for the specified profile, if available.
  *
  * @param codec_id the ID of the codec to which the requested profile belongs
  * @param profile the profile value for which a name is requested
  * @return A name for the profile if found, NULL otherwise.
  *
  * @note unlike av_get_profile_name(), which searches a list of profiles
  *       supported by a specific decoder or encoder implementation, this
  *       function searches the list of profiles from the AVCodecDescriptor
*)
// const char * avcodec_profile_name(enum AVCodecID codec_id, int profile);
function avcodec_profile_name(codec_id: AVCodecID; profile: int): pAnsiChar; cdecl; external avcodec_dll;

// int avcodec_default_execute(AVCodecContext * c, int (* func)(AVCodecContext *c2, void *arg2),void *arg, int *ret, int count, int size);
// int avcodec_default_execute2(AVCodecContext *c, int (*func)(AVCodecContext *c2, void *arg2, int, int),void *arg, int *ret, int count);
type
  Tavcodec_default_execute_func = function(c2: pAVCodecContext; arg2: Pointer): int; cdecl;

function avcodec_default_execute(c: pAVCodecContext; func: Tavcodec_default_execute_func; arg: Pointer; var ret: int; count: int; size: int): int; cdecl;
  external avcodec_dll;

// int avcodec_default_execute2(AVCodecContext *c, int (*func)(AVCodecContext *c2, void *arg2, int, int),void *arg, int *ret, int count);
type
  Tavcodec_default_execute2_func = function(c2: pAVCodecContext; arg2: Pointer; p1, p2: int): int; cdecl;

function avcodec_default_execute2(c: pAVCodecContext; func: Tavcodec_default_execute2_func; arg: Pointer; var ret: int; count: int): int; cdecl;
  external avcodec_dll;

// FIXME func typedef

(*
  * Fill AVFrame audio data and linesize pointers.
  *
  * The buffer buf must be a preallocated buffer with a size big enough
  * to contain the specified samples amount. The filled AVFrame data
  * pointers will point to this buffer.
  *
  * AVFrame extended_data channel pointers are allocated if necessary for
  * planar audio.
  *
  * @param frame       the AVFrame
  *                    frame->nb_samples must be set prior to calling the
  *                    function. This function fills in frame->data,
  *                    frame->extended_data, frame->linesize[0].
  * @param nb_channels channel count
  * @param sample_fmt  sample format
  * @param buf         buffer to use for frame data
  * @param buf_size    size of buffer
  * @param align       plane size sample alignment (0 = default)
  * @return            >=0 on success, negative error code on failure
  * @todo return the size in bytes required to store the samples in
  * case of success, at the next libavutil bump
*)
// int avcodec_fill_audio_frame(AVFrame * frame, int nb_channels, enum AVSampleFormat sample_fmt, const uint8_t * buf, int buf_size, int align);
function avcodec_fill_audio_frame(frame: pAVFrame; nb_channels: int; sample_fmt: AVSampleFormat; const buf: puint8_t; buf_size: int; align: int): int; cdecl;
  external avcodec_dll;

(*
  * Reset the internal decoder state / flush internal buffers. Should be called
  * e.g. when seeking or when switching to a different stream.
  *
  * @note when refcounted frames are not used (i.e. avctx->refcounted_frames is 0),
  * this invalidates the frames previously returned from the decoder. When
  * refcounted frames are used, the decoder just releases any references it might
  * keep internally, but the caller's reference remains valid.
*)
// void avcodec_flush_buffers(AVCodecContext * avctx);
procedure avcodec_flush_buffers(avctx: pAVCodecContext); cdecl; external avcodec_dll;

(*
  * Return codec bits per sample.
  *
  * @param[in] codec_id the codec
  * @return Number of bits per sample or zero if unknown for the given codec.
*)
// int av_get_bits_per_sample(enum AVCodecID codec_id);
function av_get_bits_per_sample(codec_id: AVCodecID): int; cdecl; external avcodec_dll;

(*
  * Return the PCM codec associated with a sample format.
  * @param be  endianness, 0 for little, 1 for big,
  *            -1 (or anything else) for native
  * @return  AV_CODEC_ID_PCM_* or AV_CODEC_ID_NONE
*)
// enum AVCodecID av_get_pcm_codec(enum AVSampleFormat fmt, int be);
function av_get_pcm_codec(fmt: AVSampleFormat; be: int): AVCodecID; cdecl; external avcodec_dll;

(*
  * Return codec bits per sample.
  * Only return non-zero if the bits per sample is exactly correct, not an
  * approximation.
  *
  * @param[in] codec_id the codec
  * @return Number of bits per sample or zero if unknown for the given codec.
*)
// int av_get_exact_bits_per_sample(enum AVCodecID codec_id);
function av_get_exact_bits_per_sample(codec_id: AVCodecID): int; cdecl; external avcodec_dll;

(*
  * Return audio frame duration.
  *
  * @param avctx        codec context
  * @param frame_bytes  size of the frame, or 0 if unknown
  * @return             frame duration, in samples, if known. 0 if not able to
  *                     determine.
*)
// int av_get_audio_frame_duration(AVCodecContext * avctx, int frame_bytes);
function av_get_audio_frame_duration(avctx: pAVCodecContext; frame_bytes: int): int; cdecl; external avcodec_dll;

(*
  * This function is the same as av_get_audio_frame_duration(), except it works
  * with AVCodecParameters instead of an AVCodecContext.
*)
// int av_get_audio_frame_duration2(AVCodecParameters * par, int frame_bytes);
function av_get_audio_frame_duration2(par: pAVCodecParameters; frame_bytes: int): int; cdecl; external avcodec_dll;

Type
  pAVBitStreamFilter = ^AVBitStreamFilter;

{$IFDEF FF_API_OLD_BSF}
  pAVBitStreamFilterContext = ^AVBitStreamFilterContext;

  AVBitStreamFilterContext = record
    priv_data: Pointer;
    filter: pAVBitStreamFilter;
    parser: pAVCodecParserContext;
    next: pAVBitStreamFilterContext;
    (*
      * Internal default arguments, used if NULL is passed to av_bitstream_filter_filter().
      * Not for access by library users.
    *)
    args: pAnsiChar;
  end;
{$ENDIF}

  pAVBSFInternal = ^AVBSFInternal;

  AVBSFInternal = record
  end;

  (*
    * The bitstream filter state.
    *
    * This struct must be allocated with av_bsf_alloc() and freed with
    * av_bsf_free().
    *
    * The fields in the struct will only be changed (by the caller or by the
    * filter) as described in their documentation, and are to be considered
    * immutable otherwise.
  *)
  pAVBSFContext = ^AVBSFContext;

  AVBSFContext = record
    (*
      * A class for logging and AVOptions
    *)
    av_class: pAVClass;

    (*
      * The bitstream filter this context is an instance of.
    *)
    filter: pAVBitStreamFilter;

    (*
      * Opaque libavcodec internal data. Must not be touched by the caller in any
      * way.
    *)
    internal: pAVBSFInternal;

    (*
      * Opaque filter-specific private data. If filter->priv_class is non-NULL,
      * this is an AVOptions-enabled struct.
    *)
    priv_data: Pointer;

    (*
      * Parameters of the input stream. This field is allocated in
      * av_bsf_alloc(), it needs to be filled by the caller before
      * av_bsf_init().
    *)
    par_in: pAVCodecParameters;

    (*
      * Parameters of the output stream. This field is allocated in
      * av_bsf_alloc(), it is set by the filter in av_bsf_init().
    *)
    par_out: pAVCodecParameters;

    (*
      * The timebase used for the timestamps of the input packets. Set by the
      * caller before av_bsf_init().
    *)
    time_base_in: AVRational;

    (*
      * The timebase used for the timestamps of the output packets. Set by the
      * filter in av_bsf_init().
    *)
    time_base_out: AVRational;
  end;

  AVBitStreamFilter = record
    name: pAnsiChar;

    (*
      * A list of codec ids supported by the filter, terminated by
      * AV_CODEC_ID_NONE.
      * May be NULL, in that case the bitstream filter works with any codec id.
    *)
    codec_ids: pAVCodecID;

    (*
      * A class for the private data, used to declare bitstream filter private
      * AVOptions. This field is NULL for bitstream filters that do not declare
      * any options.
      *
      * If this field is non-NULL, the first member of the filter private data
      * must be a pointer to AVClass, which will be set by libavcodec generic
      * code to this class.
    *)
    priv_class: pAVClass;

    (* ***************************************************************
      * No fields below this line are part of the public API. They
      * may not be used outside of libavcodec and can be changed and
      * removed at will.
      * New public fields should be added right above.
      *****************************************************************
    *)
    priv_data_size: int;
    // int (*init)(AVBSFContext *ctx);
    init: function(ctx: pAVBSFContext): int; cdecl;
    // int (*filter)(AVBSFContext *ctx, AVPacket *pkt);
    filter: function(ctx: pAVBSFContext; pkt: pAVPacket): int; cdecl;
    // void (*close)(AVBSFContext *ctx);
    close: procedure(ctx: pAVBSFContext); cdecl;
    // void (*flush)(AVBSFContext *ctx);
    flush: procedure(ctx: pAVBSFContext); cdecl;
  end;

{$IFDEF FF_API_OLD_BSF}

  (*
    * @deprecated the old bitstream filtering API (using AVBitStreamFilterContext)
    * is deprecated. Use the new bitstream filtering API (using AVBSFContext).
  *)
  // attribute_deprecated void av_register_bitstream_filter(AVBitStreamFilter * bsf);
procedure av_register_bitstream_filter(bsf: pAVBitStreamFilter); cdecl; external avcodec_dll; deprecated;
(*
  * @deprecated the old bitstream filtering API (using AVBitStreamFilterContext)
  * is deprecated. Use av_bsf_get_by_name(), av_bsf_alloc(), and av_bsf_init()
  * from the new bitstream filtering API (using AVBSFContext).
*)
// attribute_deprecated AVBitStreamFilterContext * av_bitstream_filter_init(const char * name);
function av_bitstream_filter_init(const name: pAnsiChar): pAVBitStreamFilterContext; cdecl; external avcodec_dll; deprecated;
(*
  * @deprecated the old bitstream filtering API (using AVBitStreamFilterContext)
  * is deprecated. Use av_bsf_send_packet() and av_bsf_receive_packet() from the
  * new bitstream filtering API (using AVBSFContext).
*)
// attribute_deprecated int av_bitstream_filter_filter(AVBitStreamFilterContext * bsfc, AVCodecContext * avctx, const char * args,
// uint8_t * * poutbuf, int * poutbuf_size, const uint8_t * buf, int buf_size, int keyframe);
function av_bitstream_filter_filter(bsfc: pAVBitStreamFilterContext; avctx: pAVCodecContext; const args: pAnsiChar; var poutbuf: puint8_t;
  var poutbuf_size: int; const buf: puint8_t; buf_size: int; keyframe: int): int; cdecl; external avcodec_dll; deprecated;
(*
  * @deprecated the old bitstream filtering API (using AVBitStreamFilterContext)
  * is deprecated. Use av_bsf_free() from the new bitstream filtering API (using
  * AVBSFContext).
*)
// attribute_deprecated void av_bitstream_filter_close(AVBitStreamFilterContext * bsf);
procedure av_bitstream_filter_close(bsf: pAVBitStreamFilterContext); cdecl; external avcodec_dll; deprecated;
(*
  * @deprecated the old bitstream filtering API (using AVBitStreamFilterContext)
  * is deprecated. Use av_bsf_iterate() from the new bitstream filtering API (using
  * AVBSFContext).
*)
// attribute_deprecated const AVBitStreamFilter * av_bitstream_filter_next(const AVBitStreamFilter * f);
function av_bitstream_filter_next(const f: pAVBitStreamFilter): pAVBitStreamFilter; cdecl; external avcodec_dll; deprecated;
{$ENDIF}
(*
  * @return a bitstream filter with the specified name or NULL if no such
  *         bitstream filter exists.
*)
// const AVBitStreamFilter * av_bsf_get_by_name(const char * name);
function av_bsf_get_by_name(const name: pAnsiChar): pAVBitStreamFilter; cdecl; external avcodec_dll;

(*
  * Iterate over all registered bitstream filters.
  *
  * @param opaque a pointer where libavcodec will store the iteration state. Must
  *               point to NULL to start the iteration.
  *
  * @return the next registered bitstream filter or NULL when the iteration is
  *         finished
*)
// const AVBitStreamFilter * av_bsf_iterate(void * * opaque);
function av_bsf_iterate(var opaque: Pointer): pAVBitStreamFilter; cdecl; external avcodec_dll;
{$IFDEF FF_API_NEXT}
// attribute_deprecated const AVBitStreamFilter * av_bsf_next(void * * opaque);
function av_bsf_next(var opaque: Pointer): pAVBitStreamFilter; cdecl; external avcodec_dll; deprecated;
{$ENDIF}
(*
  * Allocate a context for a given bitstream filter. The caller must fill in the
  * context parameters as described in the documentation and then call
  * av_bsf_init() before sending any data to the filter.
  *
  * @param filter the filter for which to allocate an instance.
  * @param ctx a pointer into which the pointer to the newly-allocated context
  *            will be written. It must be freed with av_bsf_free() after the
  *            filtering is done.
  *
  * @return 0 on success, a negative AVERROR code on failure
*)
// int av_bsf_alloc(const AVBitStreamFilter * filter, AVBSFContext * * ctx);
function av_bsf_alloc(const filter: pAVBitStreamFilter; var ctx: pAVBSFContext): int; cdecl; external avcodec_dll;

(*
  * Prepare the filter for use, after all the parameters and options have been
  * set.
*)
// int av_bsf_init(AVBSFContext * ctx);
function av_bsf_init(ctx: pAVBSFContext): int; cdecl; external avcodec_dll;

(*
  * Submit a packet for filtering.
  *
  * After sending each packet, the filter must be completely drained by calling
  * av_bsf_receive_packet() repeatedly until it returns AVERROR(EAGAIN) or
  * AVERROR_EOF.
  *
  * @param pkt the packet to filter. The bitstream filter will take ownership of
  * the packet and reset the contents of pkt. pkt is not touched if an error occurs.
  * This parameter may be NULL, which signals the end of the stream (i.e. no more
  * packets will be sent). That will cause the filter to output any packets it
  * may have buffered internally.
  *
  * @return 0 on success, a negative AVERROR on error.
*)
// int av_bsf_send_packet(AVBSFContext * ctx, AVPacket * pkt);
function av_bsf_send_packet(ctx: pAVBSFContext; pkt: pAVPacket): int; cdecl; external avcodec_dll;

(*
  * Retrieve a filtered packet.
  *
  * @param[out] pkt this struct will be filled with the contents of the filtered
  *                 packet. It is owned by the caller and must be freed using
  *                 av_packet_unref() when it is no longer needed.
  *                 This parameter should be "clean" (i.e. freshly allocated
  *                 with av_packet_alloc() or unreffed with av_packet_unref())
  *                 when this function is called. If this function returns
  *                 successfully, the contents of pkt will be completely
  *                 overwritten by the returned data. On failure, pkt is not
  *                 touched.
  *
  * @return 0 on success. AVERROR(EAGAIN) if more packets need to be sent to the
  * filter (using av_bsf_send_packet()) to get more output. AVERROR_EOF if there
  * will be no further output from the filter. Another negative AVERROR value if
  * an error occurs.
  *
  * @note one input packet may result in several output packets, so after sending
  * a packet with av_bsf_send_packet(), this function needs to be called
  * repeatedly until it stops returning 0. It is also possible for a filter to
  * output fewer packets than were sent to it, so this function may return
  * AVERROR(EAGAIN) immediately after a successful av_bsf_send_packet() call.
*)
// int av_bsf_receive_packet(AVBSFContext * ctx, AVPacket * pkt);
function av_bsf_receive_packet(ctx: pAVBSFContext; pkt: pAVPacket): int; cdecl; external avcodec_dll;

(* *
  * Reset the internal bitstream filter state / flush internal buffers.
*)
// void av_bsf_flush(AVBSFContext *ctx);
procedure av_bsf_flush(var ctx: AVBSFContext); cdecl; external avcodec_dll;

(*
  * Free a bitstream filter context and everything associated with it; write NULL
  * into the supplied pointer.
*)
// void av_bsf_free(AVBSFContext **ctx);
procedure av_bsf_free(var ctx: pAVBSFContext); cdecl; external avcodec_dll;

(*
  * Get the AVClass for AVBSFContext. It can be used in combination with
  * AV_OPT_SEARCH_FAKE_OBJ for examining options.
  *
  * @see av_opt_find().
*)
// const AVClass * av_bsf_get_class(void);
function av_bsf_get_class(): pAVClass; cdecl; external avcodec_dll;

type
  (*
    * Structure for chain/list of bitstream filters.
    * Empty list can be allocated by av_bsf_list_alloc().
  *)
  pAVBSFList = ^AVBSFList;

  AVBSFList = record
  end;

  (*
    * Allocate empty list of bitstream filters.
    * The list must be later freed by av_bsf_list_free()
    * or finalized by av_bsf_list_finalize().
    *
    * @return Pointer to @ref AVBSFList on success, NULL in case of failure
  *)
  // AVBSFList * av_bsf_list_alloc(void);
function av_bsf_list_alloc(): pAVBSFList; cdecl; external avcodec_dll;

(*
  * Free list of bitstream filters.
  *
  * @param lst Pointer to pointer returned by av_bsf_list_alloc()
*)
// void av_bsf_list_free(AVBSFList * * lst);
procedure av_bsf_list_free(var lst: pAVBSFList); cdecl; external avcodec_dll;
(*
  * Append bitstream filter to the list of bitstream filters.
  *
  * @param lst List to append to
  * @param bsf Filter context to be appended
  *
  * @return >=0 on success, negative AVERROR in case of failure
*)
// int av_bsf_list_append(AVBSFList * lst, AVBSFContext * bsf);
function av_bsf_list_append(lst: pAVBSFList; bsf: pAVBSFContext): int; cdecl; external avcodec_dll;
(*
  * Construct new bitstream filter context given it's name and options
  * and append it to the list of bitstream filters.
  *
  * @param lst      List to append to
  * @param bsf_name Name of the bitstream filter
  * @param options  Options for the bitstream filter, can be set to NULL
  *
  * @return >=0 on success, negative AVERROR in case of failure
*)
// int av_bsf_list_append2(AVBSFList * lst, const char * bsf_name, AVDictionary * * options);
function av_bsf_list_append2(lst: pAVBSFList; const bsf_name: pAnsiChar; var options: pAVDictionary): int; cdecl; external avcodec_dll;
(*
  * Finalize list of bitstream filters.
  *
  * This function will transform @ref AVBSFList to single @ref AVBSFContext,
  * so the whole chain of bitstream filters can be treated as single filter
  * freshly allocated by av_bsf_alloc().
  * If the call is successful, @ref AVBSFList structure is freed and lst
  * will be set to NULL. In case of failure, caller is responsible for
  * freeing the structure by av_bsf_list_free()
  *
  * @param      lst Filter list structure to be transformed
  * @param[out] bsf Pointer to be set to newly created @ref AVBSFContext structure
  *                 representing the chain of bitstream filters
  *
  * @return >=0 on success, negative AVERROR in case of failure
*)
// int av_bsf_list_finalize(AVBSFList * * lst, AVBSFContext * * bsf);
function av_bsf_list_finalize(var lst: pAVBSFList; var bsf: pAVBSFContext): int; cdecl; external avcodec_dll;
(*
  * Parse string describing list of bitstream filters and create single
  * @ref AVBSFContext describing the whole chain of bitstream filters.
  * Resulting @ref AVBSFContext can be treated as any other @ref AVBSFContext freshly
  * allocated by av_bsf_alloc().
  *
  * @param      str String describing chain of bitstream filters in format
  *                 `bsf1[=opt1=val1:opt2=val2][,bsf2]`
  * @param[out] bsf Pointer to be set to newly created @ref AVBSFContext structure
  *                 representing the chain of bitstream filters
  *
  * @return >=0 on success, negative AVERROR in case of failure
*)
// int av_bsf_list_parse_str(const char * str, AVBSFContext * * bsf);
function av_bsf_list_parse_str(const str: pAnsiChar; var bsf: pAVBSFContext): int; cdecl; external avcodec_dll;
(*
  * Get null/pass-through bitstream filter.
  *
  * @param[out] bsf Pointer to be set to new instance of pass-through bitstream filter
  *
  * @return
*)
// int av_bsf_get_null_filter(AVBSFContext * * bsf);
function av_bsf_get_null_filter(var bsf: pAVBSFContext): int; cdecl; external avcodec_dll;
(* memory *)

(*
  * Same behaviour av_fast_malloc but the buffer has additional
  * AV_INPUT_BUFFER_PADDING_SIZE at the end which will always be 0.
  *
  * In addition the whole buffer will initially and after resizes
  * be 0-initialized so that no uninitialized data will ever appear.
*)
// void av_fast_padded_malloc(void * ptr, unsigned int * size, size_t min_size);
procedure av_fast_padded_malloc(ptr: Pointer; var size: unsignedint; min_size: size_t); cdecl; external avcodec_dll;
(*
  * Same behaviour av_fast_padded_malloc except that buffer will always
  * be 0-initialized after call.
*)
// void av_fast_padded_mallocz(void * ptr, unsigned int * size, size_t min_size);
procedure av_fast_padded_mallocz(ptr: Pointer; var size: unsignedint; min_size: size_t); cdecl; external avcodec_dll;
(*
  * Encode extradata length to a buffer. Used by xiph codecs.
  *
  * @param s buffer to write to; must be at least (v/255+1) bytes long
  * @param v size of extradata in bytes
  * @return number of bytes written to the buffer.
*)
// unsigned int av_xiphlacing(unsigned char * s, unsigned int v);
function av_xiphlacing(s: punsignedchar; v: unsignedint): unsignedint; cdecl; external avcodec_dll;
{$IFDEF FF_API_USER_VISIBLE_AVHWACCEL}
(*
  * Register the hardware accelerator hwaccel.
  *
  * @deprecated  This function doesn't do anything.
*)
// attribute_deprecated void av_register_hwaccel(AVHWAccel * hwaccel);
procedure av_register_hwaccel(hwaccel: pAVHWAccel); cdecl; external avcodec_dll; deprecated 'This function doesn''t do anything';
(*
  * If hwaccel is NULL, returns the first registered hardware accelerator,
  * if hwaccel is non-NULL, returns the next registered hardware accelerator
  * after hwaccel, or NULL if hwaccel is the last one.
  *
  * @deprecated  AVHWaccel structures contain no user-serviceable parts, so
  *              this function should not be used.
*)
// attribute_deprecated AVHWAccel * av_hwaccel_next(const AVHWAccel * hwaccel);
function av_hwaccel_next(const hwaccel: pAVHWAccel): pAVHWAccel; cdecl; external avcodec_dll;
  deprecated 'AVHWaccel structures contain no user-serviceable parts, so this function should not be used';

{$ENDIF}
{$IFDEF FF_API_LOCKMGR}

type
  (*
    * Lock operation used by lockmgr
    *
    * @deprecated Deprecated together with av_lockmgr_register().
  *)
  AVLockOp = (       //
    AV_LOCK_CREATE,  // < Create a mutex
    AV_LOCK_OBTAIN,  // < Lock the mutex
    AV_LOCK_RELEASE, // < Unlock the mutex
    AV_LOCK_DESTROY  // < Free mutex resources
    )deprecated 'Deprecated together with av_lockmgr_register()';

  (*
    * Register a user provided lock manager supporting the operations
    * specified by AVLockOp. The "mutex" argument to the function points
    * to a (void * ) where the lockmgr should store/get a pointer to a user
    * allocated mutex. It is NULL upon AV_LOCK_CREATE and equal to the
    * value left by the last call for all other ops. If the lock manager is
    * unable to perform the op then it should leave the mutex in the same
    * state as when it was called and return a non-zero value. However,
    * when called with AV_LOCK_DESTROY the mutex will always be assumed to
    * have been successfully destroyed. If av_lockmgr_register succeeds
    * it will return a non-negative value, if it fails it will return a
    * negative value and destroy all mutex and unregister all callbacks.
    * av_lockmgr_register is not thread-safe, it must be called from a
    * single thread before any calls which make use of locking are used.
    *
    * @param cb User defined callback. av_lockmgr_register invokes calls
    *           to this callback and the previously registered callback.
    *           The callback will be used to create more than one mutex
    *           each of which must be backed by its own underlying locking
    *           mechanism (i.e. do not use a single static object to
    *           implement your lock manager). If cb is set to NULL the
    *           lockmgr will be unregistered.
    *
    * @deprecated This function does nothing, and always returns 0. Be sure to
    *             build with thread support to get basic thread safety.
  *)
  // attribute_deprecated int av_lockmgr_register(int (* cb)(void **mutex, enum AVLockOp op));
type
  Tav_lockmgr_register_cb_func = function(var mutex: Pointer; op: AVLockOp): int; cdecl;

function av_lockmgr_register(cb: Tav_lockmgr_register_cb_func): int; cdecl; external avcodec_dll;
{$ENDIF}
(*
  * Get the type of the given codec.
*)
// enum AVMediaType avcodec_get_type(enum AVCodecID codec_id);
function avcodec_get_type(codec_id: AVCodecID): AVMediaType; cdecl; external avcodec_dll;
(*
  * Get the name of a codec.
  * @return  a static string identifying the codec; never NULL
*)
// const char * avcodec_get_name(enum AVCodecID id);
function avcodec_get_name(id: AVCodecID): pAnsiChar; cdecl; external avcodec_dll;
(*
  * @return a positive value if s is open (i.e. avcodec_open2() was called on it
  * with no corresponding avcodec_close()), 0 otherwise.
*)
// int avcodec_is_open(AVCodecContext * s);
function avcodec_is_open(s: pAVCodecContext): int; cdecl; external avcodec_dll;
(*
  * @return a non-zero number if codec is an encoder, zero otherwise
*)
// int av_codec_is_encoder(const avcodec * codec);
function av_codec_is_encoder(const codec: pAVCodec): int; cdecl; external avcodec_dll;

(*
  * @return a non-zero number if codec is a decoder, zero otherwise
*)
// int av_codec_is_decoder(const avcodec * codec);
function av_codec_is_decoder(const codec: pAVCodec): int; cdecl; external avcodec_dll;

(*
  * @return descriptor for given codec ID or NULL if no descriptor exists.
*)
// const AVCodecDescriptor * avcodec_descriptor_get(enum AVCodecID id);
function avcodec_descriptor_get(id: pAVCodecID): pAVCodecDescriptor; cdecl; external avcodec_dll;

(*
  * Iterate over all codec descriptors known to libavcodec.
  *
  * @param prev previous descriptor. NULL to get the first descriptor.
  *
  * @return next descriptor or NULL after the last descriptor
*)
// const AVCodecDescriptor * avcodec_descriptor_next(const AVCodecDescriptor * prev);
function avcodec_descriptor_next(const prev: pAVCodecDescriptor): pAVCodecDescriptor; cdecl; external avcodec_dll;

(*
  * @return codec descriptor with the given name or NULL if no such descriptor
  *         exists.
*)
// const AVCodecDescriptor * avcodec_descriptor_get_by_name(const char * name);
function avcodec_descriptor_get_by_name(const name): pAVCodecDescriptor; cdecl; external avcodec_dll;

(*
  * Allocate a CPB properties structure and initialize its fields to default
  * values.
  *
  * @param size if non-NULL, the size of the allocated struct will be written
  *             here. This is useful for embedding it in side data.
  *
  * @return the newly allocated struct or NULL on failure
*)
// AVCPBProperties * av_cpb_properties_alloc(size_t * size);
function av_cpb_properties_alloc(size: psize_t): pAVCPBProperties; cdecl; external avcodec_dll;

{$ENDREGION}
{$REGION 'ac3_parser.h'}
(* *
  * Extract the bitstream ID and the frame size from AC-3 data.
*)
// int av_ac3_parse_header(const uint8_t *buf, size_t size,
// uint8_t *bitstream_id, uint16_t *frame_size);
function av_ac3_parse_header(const buf: puint8_t; size: size_t; var bitstream_id: uint8_t; var frame_size: uint16_t): int; cdecl; external avcodec_dll;
{$ENDREGION}
{$REGION 'adts_parser.h'}
(* *
  * Extract the number of samples and frames from AAC data.
  * @param[in]  buf     pointer to AAC data buffer
  * @param[out] samples Pointer to where number of samples is written
  * @param[out] frames  Pointer to where number of frames is written
  * @return Returns 0 on success, error code on failure.
*)
// int av_adts_header_parse(const uint8_t *buf, uint32_t *samples,
// uint8_t *frames);
function av_adts_header_parse(const buf: puint8_t; var samples: uint32_t; var frames: uint8_t): int; cdecl; external avcodec_dll;
{$ENDREGION}
{$REGION 'jni.h'}
(*
  * Manually set a Java virtual machine which will be used to retrieve the JNI
  * environment. Once a Java VM is set it cannot be changed afterwards, meaning
  * you can call multiple times av_jni_set_java_vm with the same Java VM pointer
  * however it will error out if you try to set a different Java VM.
  *
  * @param vm Java virtual machine
  * @param log_ctx context used for logging, can be NULL
  * @return 0 on success, < 0 otherwise
*)
// int av_jni_set_java_vm(void *vm, void *log_ctx);
function av_jni_set_java_vm(vm: Pointer; log_ctx: Pointer): int; cdecl; external avcodec_dll;
(*
  * Get the Java virtual machine which has been set with av_jni_set_java_vm.
  *
  * @param vm Java virtual machine
  * @return a pointer to the Java virtual machine
*)
// void *av_jni_get_java_vm(void *log_ctx);
function av_jni_get_java_vm(log_ctx: Pointer): Pointer; cdecl; external avcodec_dll;
{$ENDREGION}
{$REGION 'vorbis_parser.h'}

type
  pAVVorbisParseContext = ^AVVorbisParseContext;

  AVVorbisParseContext = record
  end;

  (* *
    * Allocate and initialize the Vorbis parser using headers in the extradata.
  *)
  // AVVorbisParseContext *av_vorbis_parse_init(const uint8_t *extradata, int extradata_size);
function av_vorbis_parse_init(const extradata: puint8_t; extradata_size: int): pAVVorbisParseContext; cdecl; external avcodec_dll;
(* *
  * Free the parser and everything associated with it.
*)
// void av_vorbis_parse_free(AVVorbisParseContext **s);
procedure av_vorbis_parse_free(var s: pAVVorbisParseContext); cdecl; external avcodec_dll;

const
  VORBIS_FLAG_HEADER  = $00000001;
  VORBIS_FLAG_COMMENT = $00000002;
  VORBIS_FLAG_SETUP   = $00000004;

  (* *
    * Get the duration for a Vorbis packet.
    *
    * If @p flags is @c NULL,
    * special frames are considered invalid.
    *
    * @param s        Vorbis parser context
    * @param buf      buffer containing a Vorbis frame
    * @param buf_size size of the buffer
    * @param flags    flags for special frames
  *)
  // int av_vorbis_parse_frame_flags(AVVorbisParseContext *s, const uint8_t *buf, int buf_size, int *flags);
function av_vorbis_parse_frame_flags(s: pAVVorbisParseContext; const buf: puint8_t; buf_size: int; var flags: int): int; cdecl; external avcodec_dll;
(* *
  * Get the duration for a Vorbis packet.
  *
  * @param s        Vorbis parser context
  * @param buf      buffer containing a Vorbis frame
  * @param buf_size size of the buffer
*)
// int av_vorbis_parse_frame(AVVorbisParseContext *s, const uint8_t *buf, int buf_size);
function av_vorbis_parse_frame(s: pAVVorbisParseContext; const buf: puint8_t; buf_size: int): int; cdecl; external avcodec_dll;

// void av_vorbis_parse_reset(AVVorbisParseContext *s);
procedure av_vorbis_parse_reset(s: pAVVorbisParseContext); cdecl; external avcodec_dll;
{$ENDREGION}
{$REGION 'vaapi.h'}
{$IFDEF FF_API_STRUCT_VAAPI_CONTEXT}

type
  (* *
    * This structure is used to share data between the FFmpeg library and
    * the client video application.
    * This shall be zero-allocated and available as
    * AVCodecContext.hwaccel_context. All user members can be set once
    * during initialization or through each AVCodecContext.get_buffer()
    * function call. In any case, they must be valid prior to calling
    * decoding functions.
    *
    * Deprecated: use AVCodecContext.hw_frames_ctx instead.
  *)
  // struct attribute_deprecated
  vaapi_context = record
    (* *
      * Window system dependent data
      *
      * - encoding: unused
      * - decoding: Set by user
    *)
    display: Pointer;

    (* *
      * Configuration ID
      *
      * - encoding: unused
      * - decoding: Set by user
    *)
    config_id: uint32_t;

    (* *
      * Context ID (video decode pipeline)
      *
      * - encoding: unused
      * - decoding: Set by user
    *)
    context_id: uint32_t;
  end deprecated;

{$ENDIF}
{$ENDREGION}
{$REGION 'avdct.h'}

type
  (* *
    * AVDCT context.
    * @note function pointers can be NULL if the specific features have been
    *       disabled at build time.
  *)
  pAVDCT = ^AVDCT;

  AVDCT = record
    av_class: pAVClass;
    // void (*idct)(int16_t *block /* align 16 */);
    idct: procedure(block: pint16_t (* align 16 *) ); cdecl;
    (* *
      * IDCT input permutation.
      * Several optimized IDCTs need a permutated input (relative to the
      * normal order of the reference IDCT).
      * This permutation must be performed before the idct_put/add.
      * Note, normally this can be merged with the zigzag/alternate scan<br>
      * An example to avoid confusion:
      * - (->decode coeffs -> zigzag reorder -> dequant -> reference IDCT -> ...)
      * - (x -> reference DCT -> reference IDCT -> x)
      * - (x -> reference DCT -> simple_mmx_perm = idct_permutation
      *    -> simple_idct_mmx -> x)
      * - (-> decode coeffs -> zigzag reorder -> simple_mmx_perm -> dequant
      *    -> simple_idct_mmx -> ...)
    *)
    idct_permutation: array [0 .. 64 - 1] of uint8_t;
    // void (*fdct)(int16_t *block /* align 16 */);
    fdct: procedure(block: pint16_t (* align 16 *) ); cdecl;
    (* *
      * DCT algorithm.
      * must use AVOptions to set this field.
    *)
    dct_algo: int;
    (* *
      * IDCT algorithm.
      * must use AVOptions to set this field.
    *)
    idct_algo: int;
    // void (*get_pixels)(int16_t *block /* align 16 */,
    // const uint8_t *pixels /* align 8 */,
    // ptrdiff_t line_size);
    get_pixels: procedure(block: int16_t (* align 16 *); const pixels: puint8_t (* align 8 *); line_size: ptrdiff_t); cdecl;
    bits_per_sample: int;
  end;

  (* *
    * Allocates a AVDCT context.
    * This needs to be initialized with avcodec_dct_init() after optionally
    * configuring it with AVOptions.
    *
    * To free it use av_free()
  *)
  // AVDCT *avcodec_dct_alloc(void);
function avcodec_dct_alloc(): pAVDCT; cdecl; external avcodec_dll;

// int avcodec_dct_init(AVDCT *);
function avcodec_dct_init(p: pAVDCT): int; cdecl; external avcodec_dll;

// const AVClass *avcodec_dct_get_class(void);
function avcodec_dct_get_class(): pAVClass; cdecl; external avcodec_dll;
{$ENDREGION}
{$REGION 'mediacodec.h'}

(* *
  * This structure holds a reference to a android/view/Surface object that will
  * be used as output by the decoder.
  *
*)
type
  pAVMediaCodecContext = ^AVMediaCodecContext;

  AVMediaCodecContext = record
    (* *
      * android/view/Surface object reference.
    *)
    surface: Pointer;
  end;

  (* *
    * Allocate and initialize a MediaCodec context.
    *
    * When decoding with MediaCodec is finished, the caller must free the
    * MediaCodec context with av_mediacodec_default_free.
    *
    * @return a pointer to a newly allocated AVMediaCodecContext on success, NULL otherwise
  *)
  // AVMediaCodecContext *av_mediacodec_alloc_context(void);
function av_mediacodec_alloc_context(): pAVMediaCodecContext; cdecl; external avcodec_dll;
(* *
  * Convenience function that sets up the MediaCodec context.
  *
  * @param avctx codec context
  * @param ctx MediaCodec context to initialize
  * @param surface reference to an android/view/Surface
  * @return 0 on success, < 0 otherwise
*)
// int av_mediacodec_default_init(AVCodecContext * avctx, AVMediaCodecContext * ctx, void * surface);
function av_mediacodec_default_init(avctx: pAVCodecContext; ctx: pAVMediaCodecContext; surface: Pointer): int; cdecl; external avcodec_dll;
(* *
  * This function must be called to free the MediaCodec context initialized with
  * av_mediacodec_default_init().
  *
  * @param avctx codec context
*)
// void av_mediacodec_default_free(AVCodecContext * avctx);
procedure av_mediacodec_default_free(avctx: pAVCodecContext); cdecl; external avcodec_dll;

(* *
  * Opaque structure representing a MediaCodec buffer to render.
*)
type
  pAVMediaCodecBuffer = ^AVMediaCodecBuffer;

  AVMediaCodecBuffer = record
  end;

  (* *
    * Release a MediaCodec buffer and render it to the surface that is associated
    * with the decoder. This function should only be called once on a given
    * buffer, once released the underlying buffer returns to the codec, thus
    * subsequent calls to this function will have no effect.
    *
    * @param buffer the buffer to render
    * @param render 1 to release and render the buffer to the surface or 0 to
    * discard the buffer
    * @return 0 on success, < 0 otherwise
  *)
  // int av_mediacodec_release_buffer(AVMediaCodecBuffer * buffer, int render);
function av_mediacodec_release_buffer(buffer: pAVMediaCodecBuffer; render: int): int; cdecl; external avcodec_dll;

(*
  * Release a MediaCodec buffer and render it at the given time to the surface
  * that is associated with the decoder. The timestamp must be within one second
  * of the current java/lang/System#nanoTime() (which is implemented using
  * CLOCK_MONOTONIC on Android). See the Android MediaCodec documentation
  * of android/media/MediaCodec#releaseOutputBuffer(int,long) for more details.
  *
  * @param buffer the buffer to render
  * @param time timestamp in nanoseconds of when to render the buffer
  * @return 0 on success, < 0 otherwise
*)
// int av_mediacodec_render_buffer_at_time(AVMediaCodecBuffer *buffer, int64_t time);
function av_mediacodec_render_buffer_at_time(buffer: pAVMediaCodecBuffer; time: int64_t): int; cdecl; external avcodec_dll;
{$ENDREGION}
{$REGION 'avfft.h'}

type
  FFTSample = float;
  pFFTSample = ^FFTSample;

  pFFTComplex = ^FFTComplex;

  FFTComplex = record
    re, im: FFTSample;
  end;

  pFFTContext = ^FFTContext;

  FFTContext = record

  end;

  (* *
    * Set up a complex FFT.
    * @param nbits           log2 of the length of the input array
    * @param inverse         if 0 perform the forward transform, if 1 perform the inverse
  *)
  // FFTContext *av_fft_init(int nbits, int inverse);
function av_fft_init(nbits: int; inverse: int): pFFTContext; cdecl; external avcodec_dll;
(* *
  * Do the permutation needed BEFORE calling ff_fft_calc().
*)
// void av_fft_permute(FFTContext *s, FFTComplex *z);
procedure av_fft_permute(s: pFFTContext; z: pFFTContext); cdecl; external avcodec_dll;
(* *
  * Do a complex FFT with the parameters defined in av_fft_init(). The
  * input data must be permuted before. No 1.0/sqrt(n) normalization is done.
*)
// void av_fft_calc(FFTContext *s, FFTComplex *z);
procedure av_fft_calc(s: pFFTContext; z: pFFTComplex); cdecl; external avcodec_dll;

// void av_fft_end(FFTContext *s);
procedure av_fft_end(s: pFFTContext); cdecl; external avcodec_dll;

// FFTContext *av_mdct_init(int nbits, int inverse, double scale);
function av_mdct_init(nbits: int; inverse: int; scale: double): pFFTContext; cdecl; external avcodec_dll;

// void av_imdct_calc(FFTContext *s, FFTSample *output, const FFTSample *input);
procedure av_imdct_calc(s: pFFTContext; output: pFFTSample; const input: pFFTSample); cdecl; external avcodec_dll;

// void av_imdct_half(FFTContext *s, FFTSample *output, const FFTSample *input);
procedure av_imdct_half(s: pFFTContext; output: pFFTSample; const input: pFFTSample); cdecl; external avcodec_dll;

// void av_mdct_calc(FFTContext *s, FFTSample *output, const FFTSample *input);
procedure av_mdct_calc(s: pFFTContext; output: pFFTSample; const input: pFFTSample); cdecl; external avcodec_dll;

// void av_mdct_end(FFTContext *s);
procedure av_mdct_end(s: pFFTContext); cdecl; external avcodec_dll;

Type
  (* Real Discrete Fourier Transform *)

  RDFTransformType = (DFT_R2C, IDFT_C2R, IDFT_R2C, DFT_C2R);

  pRDFTContext = ^RDFTContext;

  RDFTContext = record

  end;

  (* *
    * Set up a real FFT.
    * @param nbits           log2 of the length of the input array
    * @param trans           the type of transform
  *)
  // RDFTContext *av_rdft_init(int nbits, enum RDFTransformType trans);
function av_rdft_init(nbits: int; trans: RDFTransformType): pRDFTContext; cdecl; external avcodec_dll;

// void av_rdft_calc(RDFTContext *s, FFTSample *data);
procedure av_rdft_calc(s: pRDFTContext; data: pFFTSample); cdecl; external avcodec_dll;

// void av_rdft_end(RDFTContext *s);
procedure av_rdft_end(s: pRDFTContext); cdecl; external avcodec_dll;
(* Discrete Cosine Transform *)

type
  pDCTContext = ^DCTContext;

  DCTContext = record
  end;

  DCTTransformType = ( //
    DCT_II = 0, DCT_III, DCT_I, DST_I);

  (* *
    * Set up DCT.
    *
    * @param nbits           size of the input array:
    *                        (1 << nbits)     for DCT-II, DCT-III and DST-I
    *                        (1 << nbits) + 1 for DCT-I
    * @param type            the type of transform
    *
    * @note the first element of the input of DST-I is ignored
  *)
  // DCTContext *av_dct_init(int nbits, enum DCTTransformType type);
function av_dct_init(nbits: int; _type: DCTTransformType): pDCTContext; cdecl; external avcodec_dll;

// void av_dct_calc(DCTContext *s, FFTSample *data);
procedure av_dct_calc(s: pDCTContext; data: pFFTSample); cdecl; external avcodec_dll;

// void av_dct_end (DCTContext *s);
procedure av_dct_end(s: pDCTContext); cdecl; external avcodec_dll;
{$ENDREGION}
{$REGION 'dv_profile.h'}

const
  (* minimum number of bytes to read from a DV stream in order to
    * determine the profile *)
  DV_PROFILE_BYTES = (6 * 80); (* 6 DIF blocks *)

type
  (*
    * AVDVProfile is used to express the differences between various
    * DV flavors. For now it's primarily used for differentiating
    * 525/60 and 625/50, but the plans are to use it for various
    * DV specs as well (e.g. SMPTE314M vs. IEC 61834).
  *)
  Taudio_shuffle = array [0 .. 8] of uint8_t;
  paudio_shuffle = ^Taudio_shuffle;
  pAVDVProfile = ^AVDVProfile;

  AVDVProfile = record
    dsf: int;                                 (* value of the dsf in the DV header *)
    video_stype: int;                         (* stype for VAUX source pack *)
    frame_size: int;                          (* total size of one frame in bytes *)
    difseg_size: int;                         (* number of DIF segments per DIF channel *)
    n_difchan: int;                           (* number of DIF channels per frame *)
    time_base: AVRational;                    (* 1/framerate *)
    ltc_divisor: int;                         (* FPS from the LTS standpoint *)
    height: int;                              (* picture height in pixels *)
    width: int;                               (* picture width in pixels *)
    sar: array [0 .. 1] of AVRational;        (* sample aspect ratios for 4:3 and 16:9 *)
    pix_fmt: AVPixelFormat;                   (* picture pixel format *)
    bpm: int;                                 (* blocks per macroblock *)
    block_sizes: puint8_t;                    (* AC block sizes, in bits *)
    audio_stride: int;                        (* size of audio_shuffle table *)
    audio_min_samples: array [0 .. 2] of int; (* min amount of audio samples *)
    (* for 48kHz, 44.1kHz and 32kHz *)
    audio_samples_dist: array [0 .. 4] of int; (* how many samples are supposed to be *)
    (* in each frame in a 5 frames window *)
    audio_shuffle: paudio_shuffle; (* PCM shuffling table *)
  end;

  (* *
    * Get a DV profile for the provided compressed frame.
    *
    * @param sys the profile used for the previous frame, may be NULL
    * @param frame the compressed data buffer
    * @param buf_size size of the buffer in bytes
    * @return the DV profile for the supplied data or NULL on failure
  *)
  // const AVDVProfile *av_dv_frame_profile(const AVDVProfile *sys,
  // const uint8_t *frame, unsigned buf_size);
function av_dv_frame_profile(const sys: pAVDVProfile; const frame: puint8_t; buf_size: unsigned): pAVDVProfile; cdecl; external avcodec_dll;
(* *
  * Get a DV profile for the provided stream parameters.
*)
// const AVDVProfile *av_dv_codec_profile(int width, int height, enum AVPixelFormat pix_fmt);
function av_dv_codec_profile(width: int; height: int; pix_fmt: AVPixelFormat): pAVDVProfile; cdecl; external avcodec_dll;
(* *
  * Get a DV profile for the provided stream parameters.
  * The frame rate is used as a best-effort parameter.
*)
// const AVDVProfile *av_dv_codec_profile2(int width, int height, enum AVPixelFormat pix_fmt, AVRational frame_rate);
function av_dv_codec_profile2(width: int; height: int; pix_fmt: AVPixelFormat; frame_rate: AVRational): pAVDVProfile; cdecl; external avcodec_dll;
{$ENDREGION}
{$REGION 'dirac.h'}

const
  (* *
    * The spec limits the number of wavelet decompositions to 4 for both
    * level 1 (VC-2) and 128 (long-gop default).
    * 5 decompositions is the maximum before >16-bit buffers are needed.
    * Schroedinger allows this for DD 9,7 and 13,7 wavelets only, limiting
    * the others to 4 decompositions (or 3 for the fidelity filter).
    *
    * We use this instead of MAX_DECOMPOSITIONS to save some memory.
  *)
  MAX_DWT_LEVELS = 5;

type
  (* *
    * Parse code values:
    *
    * Dirac Specification ->
    * 9.6.1  Table 9.1
    *
    * VC-2 Specification  ->
    * 10.4.1 Table 10.1
  *)

  DiracParseCodes = (DIRAC_PCODE_SEQ_HEADER = $00, DIRAC_PCODE_END_SEQ = $10, DIRAC_PCODE_AUX = $20, DIRAC_PCODE_PAD = $30, DIRAC_PCODE_PICTURE_CODED = $08,
    DIRAC_PCODE_PICTURE_RAW = $48, DIRAC_PCODE_PICTURE_LOW_DEL = $C8, DIRAC_PCODE_PICTURE_HQ = $E8, DIRAC_PCODE_INTER_NOREF_CO1 = $0A,
    DIRAC_PCODE_INTER_NOREF_CO2 = $09, DIRAC_PCODE_INTER_REF_CO1 = $0D, DIRAC_PCODE_INTER_REF_CO2 = $0E, DIRAC_PCODE_INTRA_REF_CO = $0C,
    DIRAC_PCODE_INTRA_REF_RAW = $4C, DIRAC_PCODE_INTRA_REF_PICT = $CC, DIRAC_PCODE_MAGIC = $42424344);

  DiracVersionInfo = record
    major: int;
    minor: int;
  end;

  pAVDiracSeqHeader = ^AVDiracSeqHeader;

  AVDiracSeqHeader = record
    width: unsigned;
    height: unsigned;
    chroma_format: uint8_t;
    // < 0: 444  1: 422  2: 420

    interlaced: uint8_t;
    top_field_first: uint8_t;

    frame_rate_index: uint8_t;
    // < index into dirac_frame_rate[]
    aspect_ratio_index: uint8_t;
    // < index into dirac_aspect_ratio[]

    clean_width: uint16_t;
    clean_height: uint16_t;
    clean_left_offset: uint16_t;
    clean_right_offset: uint16_t;

    pixel_range_index: uint8_t;
    // < index into dirac_pixel_range_presets[]
    color_spec_index: uint8_t;
    // < index into dirac_color_spec_presets[]

    profile: int;
    level: int;

    framerate: AVRational;
    sample_aspect_ratio: AVRational;

    pix_fmt: AVPixelFormat;
    color_range: AVColorRange;
    color_primaries: AVColorPrimaries;
    color_trc: AVColorTransferCharacteristic;
    colorspace: AVColorSpace;

    version: DiracVersionInfo;
    bit_depth: int;
  end;

  (* *
    * Parse a Dirac sequence header.
    *
    * @param dsh this function will allocate and fill an AVDiracSeqHeader struct
    *            and write it into this pointer. The caller must free it with
    *            av_free().
    * @param buf the data buffer
    * @param buf_size the size of the data buffer in bytes
    * @param log_ctx if non-NULL, this function will log errors here
    * @return 0 on success, a negative AVERROR code on failure
  *)
  // int av_dirac_parse_sequence_header(AVDiracSeqHeader **dsh,
  // const uint8_t *buf, size_t buf_size,
  // void *log_ctx);
function av_dirac_parse_sequence_header(var dsh: pAVDiracSeqHeader; const buf: puint8_t; buf_size: size_t; log_ctx: Pointer): int; cdecl; external avcodec_dll;
{$ENDREGION}

implementation

end.
