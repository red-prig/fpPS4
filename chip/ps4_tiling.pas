unit ps4_Tiling;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,

  bittype,

  si_ci_vi_merged_offset,
  si_ci_vi_merged_enum,
  si_ci_vi_merged_registers
  ;

const
 // Depth modes (for depth buffers)
 kTileModeDepth_2dThin_64                   = $00000000; ///< Recommended for depth targets with one fragment per pixel.
 kTileModeDepth_2dThin_128                  = $00000001; ///< Recommended for depth targets with two or four fragments per pixel, or texture-readable.
 kTileModeDepth_2dThin_256                  = $00000002; ///< Recommended for depth targets with eight fragments per pixel.
 kTileModeDepth_2dThin_512                  = $00000003; ///< Recommended for depth targets with 512-byte tiles.
 kTileModeDepth_2dThin_1K                   = $00000004; ///< Recommended for depth targets with 1024-byte tiled.
 kTileModeDepth_1dThin                      = $00000005; ///< Not used; included only for completeness.
 kTileModeDepth_2dThinPrt_256               = $00000006; ///< Recommended for partially-resident depth surfaces. Does not support aliasing multiple virtual texture pages to the same physical page.
 kTileModeDepth_2dThinPrt_1K                = $00000007; ///< Not used; included only for completeness.
 // Display modes
 kTileModeDisplay_LinearAligned             = $00000008; ///< Recommended for any surface to be easily accessed on the CPU.
 kTileModeDisplay_1dThin                    = $00000009; ///< Not used; included only for completeness.
 kTileModeDisplay_2dThin                    = $0000000A; ///< Recommended mode for displayable render targets.
 kTileModeDisplay_ThinPrt                   = $0000000B; ///< Supports aliasing multiple virtual texture pages to the same physical page.
 kTileModeDisplay_2dThinPrt                 = $0000000C; ///< Does not support aliasing multiple virtual texture pages to the same physical page.
 // Thin modes (for non-displayable 1D/2D/3D surfaces)
 kTileModeThin_1dThin                       = $0000000D; ///< Recommended for read-only non-volume textures.
 kTileModeThin_2dThin                       = $0000000E; ///< Recommended for non-displayable intermediate render targets and read/write non-volume textures.
 kTileModeThin_3dThin                       = $0000000F; ///< Not used; included only for completeness.
 kTileModeThin_ThinPrt                      = $00000010; ///< Recommended for partially-resident textures (PRTs). Supports aliasing multiple virtual texture pages to the same physical page.
 kTileModeThin_2dThinPrt                    = $00000011; ///< Does not support aliasing multiple virtual texture pages to the same physical page.
 kTileModeThin_3dThinPrt                    = $00000012; ///< Does not support aliasing multiple virtual texture pages to the same physical page.
 // Thick modes (for 3D textures)
 kTileModeThick_1dThick                     = $00000013; ///< Recommended for read-only volume textures.
 kTileModeThick_2dThick                     = $00000014; ///< Recommended for volume textures to which pixel shaders will write.
 kTileModeThick_3dThick                     = $00000015; ///< Not used; included only for completeness.
 kTileModeThick_ThickPrt                    = $00000016; ///< Supports aliasing multiple virtual texture pages to the same physical page.
 kTileModeThick_2dThickPrt                  = $00000017; ///< Does not support aliasing multiple virtual texture pages to the same physical page.
 kTileModeThick_3dThickPrt                  = $00000018; ///< Does not support aliasing multiple virtual texture pages to the same physical page.
 kTileModeThick_2dXThick                    = $00000019; ///< Recommended for volume textures to which pixel shaders will write.
 kTileModeThick_3dXThick                    = $0000001A; ///< Not used; included only for completeness.
 // Hugely inefficient linear display mode -- do not use!
 kTileModeDisplay_LinearGeneral             = $0000001F; ///< Unsupported; do not use!

 kGpuModeBase = 0; ///< GPU mode that the original PlayStation 4 uses.
 kGpuModeNeo  = 1; ///< GPU mode that NEO uses.

 kNumSamples1   = $0; ///< 1 sample per pixel.
 kNumSamples2   = $1; ///< 2 samples per pixel.
 kNumSamples4   = $2; ///< 4 samples per pixel.
 kNumSamples8   = $3; ///< 8 samples per pixel.
 kNumSamples16  = $4; ///< 16 samples per pixel.

 kNumFragments1 = $0; ///< 1 fragment per pixel.
 kNumFragments2 = $1; ///< 2 fragments per pixel.
 kNumFragments4 = $2; ///< 4 fragments per pixel.
 kNumFragments8 = $3; ///< 8 fragments per pixel.

 kMicroTileModeDisplay = $00000000; ///< Only for 64 bpp and below.
 kMicroTileModeThin    = $00000001; ///< Non-displayable. Can be used for thin, thick, or X thick.
 kMicroTileModeDepth   = $00000002; ///< Only mode supported by DB.
 kMicroTileModeRotated = $00000003; ///< Rotated. Not supported by Gnm.
 kMicroTileModeThick   = $00000004; ///< Thick and X thick, non-AA only.

 kArrayModeLinearGeneral   = $00000000; ///< Linear pixel storage; no alignment or padding restrictions. DEPRECATED -- Do not use!
 kArrayModeLinearAligned   = $00000001; ///< Linear pixel storage with some minor alignment requirements and internal padding.
 kArrayMode1dTiledThin     = $00000002; ///< Micro-tile-only tiling for non-volume surfaces. Not valid for AA modes.
 kArrayMode1dTiledThick    = $00000003; ///< Micro-tile-only tiling for volume surfaces (8x8x4 pixel micro-tiles). Not valid for AA modes.
 kArrayMode2dTiledThin     = $00000004; ///< Macro-tile tiling for non-volume surfaces.
 kArrayModeTiledThinPrt    = $00000005; ///< Macro-tile tiling for non-volume partially-resident texture (PRT) surfaces. Supports aliasing multiple virtual texture pages to the same physical page.
 kArrayMode2dTiledThinPrt  = $00000006; ///< Macro-tile tiling for non-volume partially-resident texture (PRT) surfaces. Does not support aliasing multiple virtual texture pages to the same physical page.
 kArrayMode2dTiledThick    = $00000007; ///< Macro-tile tiling for volume surfaces (8x8x4 pixel micro-tiles).
 kArrayMode2dTiledXThick   = $00000008; ///< Macro-tile tiling for volume surfaces (8x8x8 pixel micro-tiles).
 kArrayModeTiledThickPrt   = $00000009; ///< Micro-tile-only tiling for partially-resident texture (PRT) volume surfaces (8x8x4 pixel micro-tiles). Supports aliasing multiple virtual texture pages to the same physical page.
 kArrayMode2dTiledThickPrt = $0000000a; ///< Macro-tile tiling for partially-resident texture (PRT) volume surfaces (8x8x4 pixel micro-tiles). Does not support aliasing multiple virtual texture pages to the same physical page.
 kArrayMode3dTiledThinPrt  = $0000000b; ///< Macro-tile tiling for partially-resident texture (PRT) non-volume surfaces. Z slices are rotated by pipe. Does not support aliasing multiple virtual texture pages to the same physical page.
 kArrayMode3dTiledThin     = $0000000c; ///< Macro-tile tiling for non-volume surfaces. Z slices are rotated by pipe.
 kArrayMode3dTiledThick    = $0000000d; ///< Macro-tile tiling for volume surfaces (8x8x4 pixel micro-tiles). Z slices are rotated by pipe.
 kArrayMode3dTiledXThick   = $0000000e; ///< Macro-tile tiling for volume surfaces (8x8x8 pixel micro-tiles). Z slices are rotated by pipe.
 kArrayMode3dTiledThickPrt = $0000000f; ///< Macro-tile tiling for partially-resident texture (PRT) volume surfaces (8x8x4 pixel micro-tiles). Z slices are rotated by pipe. Does not support aliasing multiple virtual texture pages to the same physical page.

 kPipeConfigP8_32x32_8x16  = $0000000a;
 kPipeConfigP8_32x32_16x16 = $0000000c;
 kPipeConfigP16            = $00000012;

 kDramRowSize = $400;
 kNumLogicalBanks = 16;
 kPipeInterleaveBytes = 256;
 kBankInterleave = 1;
 kMicroTileWidth = 8;
 kMicroTileHeight = 8;
 kNumMicroTilePixels = kMicroTileWidth*kMicroTileHeight;
 kCmaskCacheBits = $400;
 kHtileCacheBits = $4000;

 kSurfaceFormatInvalid                               = $00000000; ///< Invalid surface format.
 kSurfaceFormat8                                     = $00000001; ///< One 8-bit channel. X=0xFF
 kSurfaceFormat16                                    = $00000002; ///< One 16-bit channel. X=0xFFFF
 kSurfaceFormat8_8                                   = $00000003; ///< Two 8-bit channels. X=0x00FF, Y=0xFF00
 kSurfaceFormat32                                    = $00000004; ///< One 32-bit channel. X=0xFFFFFFFF
 kSurfaceFormat16_16                                 = $00000005; ///< Two 16-bit channels. X=0x0000FFFF, Y=0xFFFF0000
 kSurfaceFormat10_11_11                              = $00000006; ///< One 10-bit channel (Z) and two 11-bit channels (Y,X). X=0x000007FF, Y=0x003FF800, Z=0xFFC00000 Interpreted only as floating-point by texture unit, but also as integer by rasterizer.
 kSurfaceFormat11_11_10                              = $00000007; ///< Two 11-bit channels (Z,Y) and one 10-bit channel (X). X=0x000003FF, Y=0x001FFC00, Z=0xFFE00000 Interpreted only as floating-point by texture unit, but also as integer by rasterizer.
 kSurfaceFormat10_10_10_2                            = $00000008; ///< Three 10-bit channels (W,Z,Y) and one 2-bit channel (X). X=0x00000003, Y=0x00000FFC, Z=0x003FF000, W=0xFFC00000 X is never negative, even when YZW are.
 kSurfaceFormat2_10_10_10                            = $00000009; ///< One 2-bit channel (W) and three 10-bit channels (Z,Y,X). X=0x000003FF, Y=0x000FFC00, Z=0x3FF00000, W=0xC0000000 W is never negative, even when XYZ are.
 kSurfaceFormat8_8_8_8                               = $0000000a; ///< Four 8-bit channels. X=0x000000FF, Y=0x0000FF00, Z=0x00FF0000, W=0xFF000000
 kSurfaceFormat32_32                                 = $0000000b; ///< Two 32-bit channels.
 kSurfaceFormat16_16_16_16                           = $0000000c; ///< Four 16-bit channels.
 kSurfaceFormat32_32_32                              = $0000000d; ///< Three 32-bit channels.
 kSurfaceFormat32_32_32_32                           = $0000000e; ///< Four 32-bit channels.
 kSurfaceFormat5_6_5                                 = $00000010; ///< One 5-bit channel (Z), one 6-bit channel (Y), and a second 5-bit channel (X). X=0x001F, Y=0x07E0, Z=0xF800
 kSurfaceFormat1_5_5_5                               = $00000011; ///< One 1-bit channel (W) and three 5-bit channels (Z,Y,X). X=0x001F, Y=0x03E0, Z=0x7C00, W=0x8000
 kSurfaceFormat5_5_5_1                               = $00000012; ///< Three 5-bit channels (W,Z,Y) and one 1-bit channel (X). X=0x0001, Y=0x003E, Z=0x07C0, W=0xF800
 kSurfaceFormat4_4_4_4                               = $00000013; ///< Four 4-bit channels. X=0x000F, Y=0x00F0, Z=0x0F00, W=0xF000
 kSurfaceFormat8_24                                  = $00000014; ///< One 8-bit channel and one 24-bit channel.
 kSurfaceFormat24_8                                  = $00000015; ///< One 24-bit channel and one 8-bit channel.
 kSurfaceFormatX24_8_32                              = $00000016; ///< One 24-bit channel, one 8-bit channel, and one 32-bit channel.
 kSurfaceFormatGB_GR                                 = $00000020; ///< To be documented.
 kSurfaceFormatBG_RG                                 = $00000021; ///< To be documented.
 kSurfaceFormat5_9_9_9                               = $00000022; ///< One 5-bit channel (W) and three 9-bit channels (Z,Y,X). X=0x000001FF, Y=0x0003FE00, Z=0x07FC0000, W=0xF8000000. Interpreted only as three 9-bit denormalized mantissas, and one shared 5-bit exponent.
 kSurfaceFormatBc1                                   = $00000023; ///< BC1 block-compressed surface.
 kSurfaceFormatBc2                                   = $00000024; ///< BC2 block-compressed surface.
 kSurfaceFormatBc3                                   = $00000025; ///< BC3 block-compressed surface.
 kSurfaceFormatBc4                                   = $00000026; ///< BC4 block-compressed surface.
 kSurfaceFormatBc5                                   = $00000027; ///< BC5 block-compressed surface.
 kSurfaceFormatBc6                                   = $00000028; ///< BC6 block-compressed surface.
 kSurfaceFormatBc7                                   = $00000029; ///< BC7 block-compressed surface.
 kSurfaceFormatFmask8_S2_F1                          = $0000002C; ///< 8 bits-per-element FMASK surface (2 samples, 1 fragment).
 kSurfaceFormatFmask8_S4_F1                          = $0000002D; ///< 8 bits-per-element FMASK surface (4 samples, 1 fragment).
 kSurfaceFormatFmask8_S8_F1                          = $0000002E; ///< 8 bits-per-element FMASK surface (8 samples, 1 fragment).
 kSurfaceFormatFmask8_S2_F2                          = $0000002F; ///< 8 bits-per-element FMASK surface (2 samples, 2 fragments).
 kSurfaceFormatFmask8_S4_F2                          = $00000030; ///< 8 bits-per-element FMASK surface (8 samples, 2 fragments).
 kSurfaceFormatFmask8_S4_F4                          = $00000031; ///< 8 bits-per-element FMASK surface (4 samples, 4 fragments).
 kSurfaceFormatFmask16_S16_F1                        = $00000032; ///< 16 bits-per-element FMASK surface (16 samples, 1 fragment).
 kSurfaceFormatFmask16_S8_F2                         = $00000033; ///< 16 bits-per-element FMASK surface (8 samples, 2 fragments).
 kSurfaceFormatFmask32_S16_F2                        = $00000034; ///< 32 bits-per-element FMASK surface (16 samples, 2 fragments).
 kSurfaceFormatFmask32_S8_F4                         = $00000035; ///< 32 bits-per-element FMASK surface (8 samples, 4 fragments).
 kSurfaceFormatFmask32_S8_F8                         = $00000036; ///< 32 bits-per-element FMASK surface (8 samples, 8 fragments).
 kSurfaceFormatFmask64_S16_F4                        = $00000037; ///< 64 bits-per-element FMASK surface (16 samples, 4 fragments).
 kSurfaceFormatFmask64_S16_F8                        = $00000038; ///< 64 bits-per-element FMASK surface (16 samples, 8 fragments).
 kSurfaceFormat4_4                                   = $00000039; ///< Two 4-bit channels (Y,X). X=0x0F, Y=0xF0
 kSurfaceFormat6_5_5                                 = $0000003A; ///< One 6-bit channel (Z) and two 5-bit channels (Y,X). X=0x001F, Y=0x03E0, Z=0xFC00
 kSurfaceFormat1				     = $0000003B; ///< One 1-bit channel. 8 pixels per byte, with pixel index increasing from LSB to MSB.
 kSurfaceFormat1Reversed			     = $0000003C; ///< One 1-bit channel. 8 pixels per byte, with pixel index increasing from MSB to LSB.

 kSurfaceTypeColorTargetDisplayable = 1 ; ///< A render target that will be sent to scan-out. This may have different restrictions than a general-purpose color buffer.
 kSurfaceTypeColorTarget            = 2 ; ///< An intermediate render target.
 kSurfaceTypeDepthTarget            = 3 ; ///< A depth target that supports a depth buffer and/or a stencil buffer.
 kSurfaceTypeDepthOnlyTarget        = 4 ; ///< A depth target that supports a depth buffer, but may not also support a stencil buffer.
 kSurfaceTypeStencilOnlyTarget      = 5 ; ///< A depth target that supports a stencil buffer, but may not also support a depth buffer.
 kSurfaceTypeFmaskBuffer            = 6 ; ///< An FMASK surface.
 kSurfaceTypeTextureFlat            = 7 ; ///< A read-only 1D or 2D texture (or texture array).
 kSurfaceTypeTextureVolume          = 8 ; ///< A read-only 3D texture.
 kSurfaceTypeTextureCubemap         = 9 ; ///< A read-only cubic environment map (or cubemap array).
 kSurfaceTypeRwTextureFlat          = 10; ///< A read/write 1D or 2D texture (or texture array).
 kSurfaceTypeRwTextureVolume        = 11; ///< A read/write 3D texture.
 kSurfaceTypeRwTextureCubemap       = 12; ///< A read/write cubic environment map (or cubemap array).

 kSurfaceMipmapDisable = 0;	///< Surface has no mipmaps beyond the base level.
 kSurfaceMipmapEnable  = 1;	///< Surface has one or more mipmaps beyond the base level.

 kDccBlockSize64  = 0; ///<  64-byte blocks.
 kDccBlockSize128 = 1; ///< 128-byte blocks.
 kDccBlockSize256 = 2; ///< 256-byte blocks.

 kNumBanks2  = $0;
 kNumBanks4  = $1;
 kNumBanks8  = $2;
 kNumBanks16 = $3;

type
 TDATA_FORMAT=bitpacked record
  m_surfaceFormat :bit8; //0  < Gnm::SurfaceFormat.
  m_channelType   :bit4; //8  < Gnm::TextureChannelType. Can be easily converted to BufferChannelType or RenderTargetChannelType.
  m_channelX      :bit3; //12 < Gnm::TextureChannel.
  m_channelY      :bit3; //15 < Gnm::TextureChannel.
  m_channelZ      :bit3; //18 < Gnm::TextureChannel.
  m_channelW      :bit3; //21 < Gnm::TextureChannel.
  m_unused        :bit8; //29 < Unused.
 end;

 RenderTargetInitFlags=bitpacked record
  enableCmaskFastClear                :0..1; ///< Set to 1 to enable CMASK fast clears for this target.
  enableFmaskCompression              :0..1; ///< Set to 1 to enable FMASK compression for this target. Has no effect for non-MSAA surfaces. Ignored if <c>enableCmaskFastClear=0</c>.
  enableColorTextureWithoutDecompress :0..1; ///< Set to 1 to allow the compressed color buffer to be sampled without an explicit DCC decompression pass. Ignored if <c>enableDccCompression=0</c>.
  enableFmaskTextureWithoutDecompress :0..1; ///< Set to 1 to allow the FMASK surface to be sampled without an explicit FMASK decompression pass. Ignored if <c>enableFmaskCompression=0</c> or <c>enableDccCompression=0</c>.
  enableDccCompression                :0..1; ///< Set to 1 to enable DCC color compression for this target. NEO mode only.
  reserved                            :0..134217727; ///< This field must be set to zero.
 end;

 RenderTarget=packed object
  BASE       :TCB_COLOR0_BASE       ; //0  mmCB_COLOR0_BASE_DEFAULT
  PITCH      :TCB_COLOR0_PITCH      ; //1  mmCB_COLOR0_PITCH_DEFAULT
  SLICE      :TCB_COLOR0_SLICE      ; //2  mmCB_COLOR0_SLICE_DEFAULT
  VIEW       :TCB_COLOR0_VIEW       ; //3  mmCB_COLOR0_VIEW_DEFAULT
  INFO       :TCB_COLOR0_INFO       ; //4  mmCB_COLOR0_INFO_DEFAULT
  ATTRIB     :TCB_COLOR0_ATTRIB     ; //5  mmCB_COLOR0_ATTRIB_DEFAULT
  DCC_CONTROL:TCB_COLOR0_DCC_CONTROL; //6  mmCB_COLOR0_DCC_CONTROL_DEFAULT
  CMASK      :TCB_COLOR0_CMASK      ; //7  mmCB_COLOR0_CMASK_DEFAULT
  CMASK_SLICE:TCB_COLOR0_CMASK_SLICE; //8  mmCB_COLOR0_CMASK_SLICE_DEFAULT
  FMASK      :TCB_COLOR0_FMASK      ; //9  mmCB_COLOR0_FMASK_DEFAULT
  FMASK_SLICE:TCB_COLOR0_FMASK_SLICE; //10 mmCB_COLOR0_FMASK_SLICE_DEFAULT
  CLEAR_WORD :QWORD;                  //11 mmCB_COLOR0_CLEAR_WORD0_DEFAULT
                                      //12 mmCB_COLOR0_CLEAR_WORD1_DEFAULT
  DCC_BASE   :TCB_COLOR0_DCC_BASE   ; //13 mmCB_COLOR0_DCC_BASE_DEFAULT

  Width,Height:WORD;                  //14not a reg

  function getTileMode:Byte; inline;
  function getWidth:WORD;  inline;
  function getHeight:WORD; inline;
  function getMinimumGpuMode:Byte; inline;
  function getNumFragments:Byte; inline;
  function getPitchDiv8Minus1:Word; inline;
  function getPitch:DWORD; inline;
  function getDccCompressionEnable:Boolean; inline;
  function getDataFormat:TDATA_FORMAT;
  function getTileSwizzleMask:Byte;
 end;

 RenderTargetSpec=object
  m_width:DWORD; ///< The requested width, in pixels. The actual surface width may be padded to accommodate hardware restrictions. Valid range is <c>[1..16384]</c>.
  m_height:DWORD; ///< The requested height, in pixels. The actual surface width may be padded to accommodate hardware restrictions. Valid range is <c>[1..16384]</c>.
  m_pitch:DWORD; ///< The requested pitch in pixels. If this value is zero, the library will compute the minimum valid pitch for the surface given the restrictions
                    ///< imposed by other surface parameters; otherwise the provided pitch will be used, provided it also conforms to hardware restrictions. A non-zero
                    ///< pitch that does not conform to hardware restrictions will cause initialization to fail. The valid range is [0..16384] subject to hardware restrictions.
  m_numSlices:DWORD; ///< The requested number of array slices. The actual number of slices may be padded to accommodate hardware restrictions. Valid range is [1..2048].
  m_colorFormat:TDATA_FORMAT; ///< The requested format for each color fragment. This format must be one that is supported for render targets (see DataFormat::supportsRenderTarget()).
  m_colorTileModeHint:DWORD; ///< The requested tiling mode. The actual tiling mode by be different to accommodate hardware restrictions; use RenderTarget::getTileMode() to determine the object's final tiling mode.
  m_minGpuMode:DWORD; ///< The minimum GPU mode on which this surface should be supported. This setting may affect surface sizes, memory layout, available features, and so on.
  m_numSamples:DWORD; ///< The number of samples per pixel. This must not be less than <c><i>numFragments</i></c>.
  m_numFragments:DWORD; ///< The number of fragments per pixel. This must not be greater than <c><i>numSamples</i></c>.
  m_flags:RenderTargetInitFlags; ///< Used to enable additional RenderTarget features.

  m_regs:RenderTarget;
 end;

 PSurfaceFlags=^SurfaceFlags;
 SurfaceFlags=bitpacked record
  m_colorTarget   :bit1;  //< DEPRECATED -- Unused.
  m_depthTarget   :bit1;  //< Flag indicates whether the surface is a depth-only buffer.
  m_stencilTarget :bit1;  //< Flag indicates whether the surface is a stencil-only buffer.
  m_texture       :bit1;  //< DEPRECATED -- Unused.
  m_cube          :bit1;  //< Flag indicates whether the surface is a cubemap.
  m_volume        :bit1;  //< Flag indicates whether the surface is a volume texture.
  m_fmask         :bit1;  //< Flag indicates whether the surface is an FMASK surface.
  m_cubeAsArray   :bit1;  //< DEPRECATED -- Unused.
  m_overlay       :bit1;  //< DEPRECATED -- Unused.
  m_noStencil     :bit1;  //< DEPRECATED -- Unused.
  m_display       :bit1;  //< DEPRECATED -- Unused.
  m_prt           :bit1;  //< DEPRECATED -- Unused.
  m_pow2Pad       :bit1;  //< If set, all dimensions will be padded to powers of 2. <i>Must</i> be set for any texture with mipmaps, including the base level.
  m_texCompatible :bit1;  //< Flag indicates whether the surface must be configured such that it can be used/aliased as a Texture. NEO ONLY.
  m_reserved      :bit18; //< Reserved bits.
 end;

 TilingParameters=object
  m_tileMode            :DWORD;
  m_minGpuMode          :DWORD;

  m_linearWidth         :DWORD;
  m_linearHeight        :DWORD;
  m_linearDepth         :DWORD;
  m_numFragmentsPerPixel:DWORD;
  m_baseTiledPitch      :DWORD;

  m_mipLevel            :DWORD;
  m_arraySlice          :DWORD;
  m_surfaceFlags        :SurfaceFlags;
  m_bitsPerFragment     :DWORD;
  m_isBlockCompressed   :Boolean;
  m_tileSwizzleMask     :Byte;

  function initFromRenderTarget(var target:RenderTarget;arraySlice:DWORD):Integer;
  function initFromRenderTargetSpec(var target:RenderTargetSpec;arraySlice:DWORD):Integer;
 end;

 p_element_table_xyz=^t_element_table_xyz;
 //                           z    y    x
 t_element_table_xyz=array[0..7,0..7,0..7] of WORD;

 Tiler2d=object
  m_minGpuMode          :DWORD;
  m_tileMode            :DWORD;
  m_arrayMode           :DWORD;
  m_linearWidth         :DWORD;
  m_linearHeight        :DWORD;
  m_linearDepth         :DWORD;
  m_paddedWidth         :DWORD;
  m_paddedHeight        :DWORD;
  m_paddedDepth         :DWORD;
  m_bitsPerElement      :DWORD;
  m_linearSizeBytes     :DWORD;
  m_tiledSizeBytes      :DWORD;

  m_microTileMode       :DWORD;
  m_pipeConfig          :DWORD;
  m_arraySlice          :DWORD;
  m_numFragmentsPerPixel:DWORD;
  m_bankWidth           :DWORD;
  m_bankHeight          :DWORD;
  m_numBanks            :DWORD;
  m_macroTileAspect     :DWORD;
  m_tileSplitBytes      :DWORD;
  m_numPipes            :DWORD;
  m_tileThickness       :DWORD;
  m_macroTileWidth      :DWORD;
  m_macroTileHeight     :DWORD;
  m_pipeInterleaveBytes :DWORD;
  m_pipeInterleaveBits  :DWORD;
  m_pipeInterleaveMask  :DWORD;
  m_pipeBits            :DWORD;
  m_bankBits            :DWORD;
  m_pipeMask            :DWORD;
  m_bankSwizzleMask     :DWORD;
  m_pipeSwizzleMask     :DWORD;

  function  init(var tp:TilingParameters):integer;
  function  getTiledElementBitOffset(var outTiledBitOffset:QWORD;x,y,z,fragmentIndex:DWORD):integer;
  procedure getTiledElementByteOffset_2d_32(var outTiledByteOffset:QWORD;x,y,z:DWORD);
 end;

 Tiler1d=object
  m_minGpuMode     :DWORD;
  m_tileMode       :DWORD;
  m_arrayMode      :DWORD;
  m_linearWidth    :DWORD;
  m_linearHeight   :DWORD;
  m_linearDepth    :DWORD;
  m_paddedWidth    :DWORD;
  m_paddedHeight   :DWORD;
  m_paddedDepth    :DWORD;
  m_bitsPerElement :DWORD;
  m_bytePerElement :DWORD;
  m_linearSizeBytes:DWORD;
  m_tiledSizeBytes :DWORD;

  m_microTileMode  :DWORD;
  m_tileThickness  :DWORD;
  m_tileBytes      :DWORD;
  m_tilesPerRow    :DWORD;
  m_tilesPerSlice  :DWORD;

  m_isBlockCompressed:DWORD;

  m_element_table  :p_element_table_xyz;

  procedure init_surface(bytePerElement,isBlockCompressed,tile_idx,tile_alt:DWORD);
  procedure init_size_2d(width,height:DWORD);
  function  getTiledElementByteOffset(var outTiledByteOffset:QWORD;x,y,z:DWORD):integer;
  function  getTiledElementBitOffset (var outTiledBitOffset :QWORD;x,y,z:DWORD):integer;
 end;

{
 m_minGpuMode:1
 m_tileMode:10
 m_arrayMode:4
 m_linearWidth:1920
 m_linearHeight:1080
 m_linearDepth:1
 m_paddedWidth:1920
 m_paddedHeight:1152
 m_paddedDepth:1
 m_bitsPerElement:32
 m_linearSizeBytes:8294400
 m_tiledSizeBytes:8847360
 m_microTileMode:0
 m_pipeConfig:18
 m_arraySlice:0
 m_numFragmentsPerPixel:1
 m_bankWidth:1
 m_bankHeight:2
 m_numBanks:8
 m_macroTileAspect:1
 m_tileSplitBytes:512
 m_numPipes:16
 m_tileThickness:1
 m_macroTileWidth:128
 m_macroTileHeight:128
 m_pipeInterleaveBytes:256
 m_pipeInterleaveBits:8
 m_pipeInterleaveMask:255
 m_pipeBits:4
 m_bankBits:3
 m_pipeMask:3840
 m_bankSwizzleMask:0
 m_pipeSwizzleMask:0
}

const
 Texture2d_32:Tiler1d=(
  m_minGpuMode:0       ;
  m_tileMode:13        ;
  m_arrayMode:2        ;
  m_linearWidth:8      ;
  m_linearHeight:8     ;
  m_linearDepth:1      ;
  m_paddedWidth:8      ;
  m_paddedHeight:8     ;
  m_paddedDepth:1      ;
  m_bitsPerElement:32  ;
  m_linearSizeBytes:256;
  m_tiledSizeBytes:256 ;
  m_microTileMode:1    ;
  m_tileThickness:1    ;
  m_tileBytes:256      ;
  m_tilesPerRow:1      ;
  m_tilesPerSlice:1    ;
 );

 Texture2d_8:Tiler1d=(
  m_minGpuMode:0      ;
  m_tileMode:13       ;
  m_arrayMode:2       ;
  m_linearWidth:8     ;
  m_linearHeight:8    ;
  m_linearDepth:1     ;
  m_paddedWidth:32    ;
  m_paddedHeight:8    ;
  m_paddedDepth:1     ;
  m_bitsPerElement:8  ;
  m_linearSizeBytes:64;
  m_tiledSizeBytes:256;
  m_microTileMode:1   ;
  m_tileThickness:1   ;
  m_tileBytes:64      ;
  m_tilesPerRow:4     ;
  m_tilesPerSlice:4   ;
 );

const
 Tiler2d_1280_720_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1280          ;
  m_linearHeight:720          ;
  m_linearDepth:1             ;
  m_paddedWidth:1280          ;
  m_paddedHeight:768          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:3686400   ;
  m_tiledSizeBytes:3932160    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1312_738_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1312          ;
  m_linearHeight:738          ;
  m_linearDepth:1             ;
  m_paddedWidth:1408          ;
  m_paddedHeight:768          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:3873024   ;
  m_tiledSizeBytes:4325376    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1344_756_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1344          ;
  m_linearHeight:756          ;
  m_linearDepth:1             ;
  m_paddedWidth:1408          ;
  m_paddedHeight:768          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:4064256   ;
  m_tiledSizeBytes:4325376    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1376_774_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1376          ;
  m_linearHeight:774          ;
  m_linearDepth:1             ;
  m_paddedWidth:1408          ;
  m_paddedHeight:832          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:4260096   ;
  m_tiledSizeBytes:4685824    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1408_792_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1408          ;
  m_linearHeight:792          ;
  m_linearDepth:1             ;
  m_paddedWidth:1408          ;
  m_paddedHeight:832          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:4460544   ;
  m_tiledSizeBytes:4685824    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1440_810_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1440          ;
  m_linearHeight:810          ;
  m_linearDepth:1             ;
  m_paddedWidth:1536          ;
  m_paddedHeight:832          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:4665600   ;
  m_tiledSizeBytes:5111808    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1472_828_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1472          ;
  m_linearHeight:828          ;
  m_linearDepth:1             ;
  m_paddedWidth:1536          ;
  m_paddedHeight:832          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:4875264   ;
  m_tiledSizeBytes:5111808    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1504_846_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1504          ;
  m_linearHeight:846          ;
  m_linearDepth:1             ;
  m_paddedWidth:1536          ;
  m_paddedHeight:896          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:5089536   ;
  m_tiledSizeBytes:5505024    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1536_864_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1536          ;
  m_linearHeight:864          ;
  m_linearDepth:1             ;
  m_paddedWidth:1536          ;
  m_paddedHeight:896          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:5308416   ;
  m_tiledSizeBytes:5505024    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1568_882_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1568          ;
  m_linearHeight:882          ;
  m_linearDepth:1             ;
  m_paddedWidth:1664          ;
  m_paddedHeight:896          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:5531904   ;
  m_tiledSizeBytes:5963776    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1600_900_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1600          ;
  m_linearHeight:900          ;
  m_linearDepth:1             ;
  m_paddedWidth:1664          ;
  m_paddedHeight:960          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:5760000   ;
  m_tiledSizeBytes:6389760    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1632_918_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1632          ;
  m_linearHeight:918          ;
  m_linearDepth:1             ;
  m_paddedWidth:1664          ;
  m_paddedHeight:960          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:5992704   ;
  m_tiledSizeBytes:6389760    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1664_936_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1664          ;
  m_linearHeight:936          ;
  m_linearDepth:1             ;
  m_paddedWidth:1664          ;
  m_paddedHeight:960          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:6230016   ;
  m_tiledSizeBytes:6389760    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1696_954_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1696          ;
  m_linearHeight:954          ;
  m_linearDepth:1             ;
  m_paddedWidth:1792          ;
  m_paddedHeight:960          ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:6471936   ;
  m_tiledSizeBytes:6881280    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1728_972_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1728          ;
  m_linearHeight:972          ;
  m_linearDepth:1             ;
  m_paddedWidth:1792          ;
  m_paddedHeight:1024         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:6718464   ;
  m_tiledSizeBytes:7340032    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1760_990_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1760          ;
  m_linearHeight:990          ;
  m_linearDepth:1             ;
  m_paddedWidth:1792          ;
  m_paddedHeight:1024         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:6969600   ;
  m_tiledSizeBytes:7340032    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1792_1008_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1792          ;
  m_linearHeight:1008         ;
  m_linearDepth:1             ;
  m_paddedWidth:1792          ;
  m_paddedHeight:1024         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:7225344   ;
  m_tiledSizeBytes:7340032    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1824_1026_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1824          ;
  m_linearHeight:1026         ;
  m_linearDepth:1             ;
  m_paddedWidth:1920          ;
  m_paddedHeight:1088         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:7485696   ;
  m_tiledSizeBytes:8355840    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1856_1044_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1856          ;
  m_linearHeight:1044         ;
  m_linearDepth:1             ;
  m_paddedWidth:1920          ;
  m_paddedHeight:1088         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:7750656   ;
  m_tiledSizeBytes:8355840    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1888_1062_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1888          ;
  m_linearHeight:1062         ;
  m_linearDepth:1             ;
  m_paddedWidth:1920          ;
  m_paddedHeight:1088         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:8020224   ;
  m_tiledSizeBytes:8355840    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_1920_1080_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:1920          ;
  m_linearHeight:1080         ;
  m_linearDepth:1             ;
  m_paddedWidth:1920          ;
  m_paddedHeight:1088         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:8294400   ;
  m_tiledSizeBytes:8355840    ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_2240_1260_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:2240          ;
  m_linearHeight:1260         ;
  m_linearDepth:1             ;
  m_paddedWidth:2304          ;
  m_paddedHeight:1280         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:11289600  ;
  m_tiledSizeBytes:11796480   ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_2560_1440_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:2560          ;
  m_linearHeight:1440         ;
  m_linearDepth:1             ;
  m_paddedWidth:2560          ;
  m_paddedHeight:1472         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:14745600  ;
  m_tiledSizeBytes:15073280   ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_2880_1620_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:2880          ;
  m_linearHeight:1620         ;
  m_linearDepth:1             ;
  m_paddedWidth:2944          ;
  m_paddedHeight:1664         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:18662400  ;
  m_tiledSizeBytes:19595264   ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_3200_1800_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:3200          ;
  m_linearHeight:1800         ;
  m_linearDepth:1             ;
  m_paddedWidth:3200          ;
  m_paddedHeight:1856         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:23040000  ;
  m_tiledSizeBytes:23756800   ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_3360_1890_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:3360          ;
  m_linearHeight:1890         ;
  m_linearDepth:1             ;
  m_paddedWidth:3456          ;
  m_paddedHeight:1920         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:25401600  ;
  m_tiledSizeBytes:26542080   ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_3520_1980_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:3520          ;
  m_linearHeight:1980         ;
  m_linearDepth:1             ;
  m_paddedWidth:3584          ;
  m_paddedHeight:1984         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:27878400  ;
  m_tiledSizeBytes:28442624   ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_3680_2070_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:3680          ;
  m_linearHeight:2070         ;
  m_linearDepth:1             ;
  m_paddedWidth:3712          ;
  m_paddedHeight:2112         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:30470400  ;
  m_tiledSizeBytes:31358976   ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 Tiler2d_3840_2160_32:Tiler2d=(
  m_minGpuMode:0              ;
  m_tileMode:10               ;
  m_arrayMode:4               ;
  m_linearWidth:3840          ;
  m_linearHeight:2160         ;
  m_linearDepth:1             ;
  m_paddedWidth:3840          ;
  m_paddedHeight:2176         ;
  m_paddedDepth:1             ;
  m_bitsPerElement:32         ;
  m_linearSizeBytes:33177600  ;
  m_tiledSizeBytes:33423360   ;
  m_microTileMode:0           ;
  m_pipeConfig:12             ;
  m_arraySlice:0              ;
  m_numFragmentsPerPixel:1    ;
  m_bankWidth:1               ;
  m_bankHeight:1              ;
  m_numBanks:16               ;
  m_macroTileAspect:2         ;
  m_tileSplitBytes:512        ;
  m_numPipes:8                ;
  m_tileThickness:1           ;
  m_macroTileWidth:128        ;
  m_macroTileHeight:64        ;
  m_pipeInterleaveBytes:256   ;
  m_pipeInterleaveBits:8      ;
  m_pipeInterleaveMask:255    ;
  m_pipeBits:3                ;
  m_bankBits:4                ;
  m_pipeMask:1792             ;
  m_bankSwizzleMask:0         ;
  m_pipeSwizzleMask:0         ;
 );

 function  GetTiler2d(Width,m_bitsPerElement:DWORD):Tiler2d;
 procedure detile32bppDisplaySse2(dst,src:Pointer;destPitch:DWORD); assembler; MS_ABI_CDecl;
 procedure detile32bppBuf(var T:Tiler2d;src,dst:Pointer);

 function getMicroTileMode(outMicroTileMode:PByte;tmode:Byte):Integer;
 Function computeSurfaceMacroTileMode(outMacroTileMode:PByte;tileMode,bitsPerElement,numFragmentsPerPixel:Byte):Integer;

 procedure computeHtileInfo(outHtileSizeBytes:PPtruint;
                            outHtileAlign    :PPtruint;
                            outHtilePitch    :PWord;
                            outHtileHeight   :PWord;
                            //
                            Pitch :DWORD;
                            Height:DWORD;
                            Slice :DWORD;
                            //
                            isHtileLinear :Boolean;
                            isTcCompatible:Boolean;
                            tileMode      :Byte
                           );

 function getArrayMode(outArrayMode:PByte;tmode:Byte):Integer;

 function getMicroTileThickness(arrayMode:Byte):Byte;

function IsTileModeDepth(tiling_idx:Byte):Boolean;

implementation

function IsTileModeDepth(tiling_idx:Byte):Boolean;
begin
 case tiling_idx of
   kTileModeDepth_2dThin_64,
   kTileModeDepth_2dThin_128,
   kTileModeDepth_2dThin_256,
   kTileModeDepth_2dThin_512,
   kTileModeDepth_2dThin_1K,
   kTileModeDepth_1dThin,
   kTileModeDepth_2dThinPrt_256,
   kTileModeDepth_2dThinPrt_1K:
    Result:=True;
  else
    Result:=False;
 end;
end;

function GetTiler2d(Width,m_bitsPerElement:DWORD):Tiler2d;
begin
 Result:=Default(Tiler2d);
 if (m_bitsPerElement<>32) then Exit;
 Case Width of
  1280:Result:=Tiler2d_1280_720_32;
  1312:Result:=Tiler2d_1312_738_32;
  1344:Result:=Tiler2d_1344_756_32;
  1376:Result:=Tiler2d_1376_774_32;
  1408:Result:=Tiler2d_1408_792_32;
  1440:Result:=Tiler2d_1440_810_32;
  1472:Result:=Tiler2d_1472_828_32;
  1504:Result:=Tiler2d_1504_846_32;
  1536:Result:=Tiler2d_1536_864_32;
  1568:Result:=Tiler2d_1568_882_32;
  1600:Result:=Tiler2d_1600_900_32;
  1632:Result:=Tiler2d_1632_918_32;
  1664:Result:=Tiler2d_1664_936_32;
  1696:Result:=Tiler2d_1696_954_32;
  1728:Result:=Tiler2d_1728_972_32;
  1760:Result:=Tiler2d_1760_990_32;
  1792:Result:=Tiler2d_1792_1008_32;
  1824:Result:=Tiler2d_1824_1026_32;
  1856:Result:=Tiler2d_1856_1044_32;
  1888:Result:=Tiler2d_1888_1062_32;
  1920:Result:=Tiler2d_1920_1080_32;
  2240:Result:=Tiler2d_2240_1260_32;
  2560:Result:=Tiler2d_2560_1440_32;
  2880:Result:=Tiler2d_2880_1620_32;
  3200:Result:=Tiler2d_3200_1800_32;
  3360:Result:=Tiler2d_3360_1890_32;
  3520:Result:=Tiler2d_3520_1980_32;
  3680:Result:=Tiler2d_3680_2070_32;
  3840:Result:=Tiler2d_3840_2160_32;
 end;

end;

//Resolution (width x height)
//1280x720
//1312x738
//1344x756
//1376x774
//1408x792
//1440x810
//1472x828
//1504x846
//1536x864
//1568x882
//1600x900
//1632x918
//1664x936
//1696x954
//1728x972
//1760x990
//1792x1008
//1824x1026
//1856x1044
//1888x1062
//1920x1080
//2240x1260
//2560x1440
//2880x1620
//3200x1800
//3360x1890
//3520x1980
//3680x2070
//3840x2160

const
 kCbColorBase       = mmCB_COLOR0_BASE        - mmCB_COLOR0_BASE;
 kCbColorPitch      = mmCB_COLOR0_PITCH       - mmCB_COLOR0_BASE;
 kCbColorSlice      = mmCB_COLOR0_SLICE       - mmCB_COLOR0_BASE;
 kCbColorView       = mmCB_COLOR0_VIEW        - mmCB_COLOR0_BASE;
 kCbColorInfo       = mmCB_COLOR0_INFO        - mmCB_COLOR0_BASE;
 kCbColorAttrib     = mmCB_COLOR0_ATTRIB      - mmCB_COLOR0_BASE;
 kCbColorDccControl = mmCB_COLOR0_DCC_CONTROL - mmCB_COLOR0_BASE;
 kCbColorCmask      = mmCB_COLOR0_CMASK       - mmCB_COLOR0_BASE;
 kCbColorCmaskSlice = mmCB_COLOR0_CMASK_SLICE - mmCB_COLOR0_BASE;
 kCbColorFmask      = mmCB_COLOR0_FMASK       - mmCB_COLOR0_BASE;
 kCbColorFmaskSlice = mmCB_COLOR0_FMASK_SLICE - mmCB_COLOR0_BASE;
 kCbColorClearWord0 = mmCB_COLOR0_CLEAR_WORD0 - mmCB_COLOR0_BASE;
 kCbColorClearWord1 = mmCB_COLOR0_CLEAR_WORD1 - mmCB_COLOR0_BASE;
 kCbColorDccBase    = mmCB_COLOR0_DCC_BASE    - mmCB_COLOR0_BASE;
 // 14: unused
 kCbWidthHeight     = 15; // not a GPU register. width in [15:0], height in [31:16].

const
 g_bitsPerElement:array[0..60] of Integer=(
    $0,           $8,          $10,          $10,
   $20,          $20,          $20,          $20,
   $20,          $20,          $20,          $40,
   $40,          $60,          $80,           -1,
   $10,          $10,          $10,          $10,
   $20,          $20,          $40,           -1,
    -1,           -1,           -1,           -1,
    -1,           -1,           -1,           -1,
   $10,          $10,          $20,           $4,
    $8,           $8,           $4,           $8,
    $8,           $8,           -1,           -1,
    $8,           $8,           $8,           $8,
    $8,           $8,          $10,          $10,
   $20,          $20,          $20,          $40,
   $40,           $8,          $10,           $1,
    $1);


function getTotalBitsPerElement(this:TDATA_FORMAT):Integer;
var
 ret:Integer;
 m_surfaceFormat:Byte;
begin
 m_surfaceFormat:=this.m_surfaceFormat;
 ret:=0;
 if (m_surfaceFormat < $3d) then
 begin
  ret := $10;
  if (6 < Byte(m_surfaceFormat - $23)) then
  begin
    if (Byte(m_surfaceFormat - $3b) < 2) then
    begin
      ret := 8;
    end else
    begin
      ret := 1;
    end;
  end;
  ret := ret * g_bitsPerElement[m_surfaceFormat];
 end;
 Result:=ret;
end;

function getTexelsPerElement(this:TDATA_FORMAT):DWORD;
var
 ret:DWORD;
begin
 ret := $10;
 if (6 < byte(this.m_surfaceFormat - $23)) then
 begin
  if (byte(this.m_surfaceFormat - $3b) < 2) then
  begin
    Exit(8);
  end;
  ret := 1;
 end;
 Result:=ret;
end;

type
 TTILE_MODE_REG=bitpacked record
  RESERVED0          :bit2;
  ARRAY_MODE         :bit4; ///< Gnm::ArrayMode
  PIPE_CONFIG        :bit5; ///< Gnm::PipeConfig
  TILE_SPLIT         :bit3; ///< Gnm::TileSplit
  RESERVED1          :bit8;
  MICRO_TILE_MODE_NEW:bit3; ///< Gnm::MicroTileMode
  SAMPLE_SPLIT       :bit2; ///< Gnm::SampleSplit
  ALT_PIPE_CONFIG    :bit5; ///< NEO ONLY
 end;

 TMACRO_TILE_MODE_REG=bitpacked record
  BANK_WIDTH           :bit2; ///< Gnm::BankWidth
  BANK_HEIGHT          :bit2; ///< Gnm::BankHeight
  MACRO_TILE_ASPECT    :bit2; ///< Gnm::MacroTileAspect
  NUM_BANKS            :bit2; ///< Gnm::NumBanks
  ALT_BANK_HEIGHT      :bit2; ///< NEO ONLY
  ALT_MACRO_TILE_ASPECT:bit2; ///< NEO ONLY
  ALT_NUM_BANKS        :bit2; ///< NEO ONLY
  RESERVED0            :bit18;
 end;

 TTILE_MODE=packed record
  Case Byte of
   0:(B:TTILE_MODE_REG);
   1:(D:DWORD);
 end;

 TMACRO_TILE_MODE=packed record
  Case Byte of
   0:(B:TMACRO_TILE_MODE_REG);
   1:(D:DWORD);
 end;

const
 GB_TILE_MODE:array[0..31] of TTILE_MODE=(
  (D:$90800310), // GB_TILE_MODE0  0x00 kTileModeDepth_2dThin_64       am=2dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Depth    ss=1
  (D:$90800B10), // GB_TILE_MODE1  0x01 kTileModeDepth_2dThin_128      am=2dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts= 128  mtm=Depth    ss=1
  (D:$90801310), // GB_TILE_MODE2  0x02 kTileModeDepth_2dThin_256      am=2dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts= 256  mtm=Depth    ss=1
  (D:$90801B10), // GB_TILE_MODE3  0x03 kTileModeDepth_2dThin_512      am=2dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts= 512  mtm=Depth    ss=1
  (D:$90802310), // GB_TILE_MODE4  0x04 kTileModeDepth_2dThin_1K       am=2dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts=1024  mtm=Depth    ss=1
  (D:$90800308), // GB_TILE_MODE5  0x05 kTileModeDepth_1dThin          am=1dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Depth    ss=1
  (D:$90801318), // GB_TILE_MODE6  0x06 kTileModeDepth_2dThinPrt_256   am=2dTiledThinPrt   pipe/alt=P8_32x32_16x16/P16  ts= 256  mtm=Depth    ss=1
  (D:$90802318), // GB_TILE_MODE7  0x07 kTileModeDepth_2dThinPrt_1K    am=2dTiledThinPrt   pipe/alt=P8_32x32_16x16/P16  ts=1024  mtm=Depth    ss=1
  (D:$90000304), // GB_TILE_MODE8  0x08 kTileModeDisplay_LinearAligned am=LinearAligned    pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Display  ss=1
  (D:$90000308), // GB_TILE_MODE9  0x09 kTileModeDisplay_1dThin        am=1dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Display  ss=1
  (D:$92000310), // GB_TILE_MODE10 0x0A kTileModeDisplay_2dThin        am=2dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Display  ss=2
  (D:$92000294), // GB_TILE_MODE11 0x0B kTileModeDisplay_ThinPrt       am=TiledThinPrt     pipe/alt=P8_32x32_8x16 /P16  ts=  64  mtm=Display  ss=2
  (D:$92000318), // GB_TILE_MODE12 0x0C kTileModeDisplay_2dThinPrt     am=2dTiledThinPrt   pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Display  ss=2
  (D:$90400308), // GB_TILE_MODE13 0x0D kTileModeThin_1dThin           am=1dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Thin     ss=1
  (D:$92400310), // GB_TILE_MODE14 0x0E kTileModeThin_2dThin           am=2dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Thin     ss=2
  (D:$924002B0), // GB_TILE_MODE15 0x0F kTileModeThin_3dThin           am=3dTiledThin      pipe/alt=P8_32x32_8x16 /P16  ts=  64  mtm=Thin     ss=2
  (D:$92400294), // GB_TILE_MODE16 0x10 kTileModeThin_ThinPrt          am=TiledThinPrt     pipe/alt=P8_32x32_8x16 /P16  ts=  64  mtm=Thin     ss=2
  (D:$92400318), // GB_TILE_MODE17 0x11 kTileModeThin_2dThinPrt        am=2dTiledThinPrt   pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Thin     ss=2
  (D:$9240032C), // GB_TILE_MODE18 0x12 kTileModeThin_3dThinPrt        am=3dTiledThinPrt   pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Thin     ss=2
  (D:$9100030C), // GB_TILE_MODE19 0x13 kTileModeThick_1dThick         am=1dTiledThick     pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Thick    ss=1
  (D:$9100031C), // GB_TILE_MODE20 0x14 kTileModeThick_2dThick         am=2dTiledThick     pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Thick    ss=1
  (D:$910002B4), // GB_TILE_MODE21 0x15 kTileModeThick_3dThick         am=3dTiledThick     pipe/alt=P8_32x32_8x16 /P16  ts=  64  mtm=Thick    ss=1
  (D:$910002A4), // GB_TILE_MODE22 0x16 kTileModeThick_ThickPrt        am=TiledThickPrt    pipe/alt=P8_32x32_8x16 /P16  ts=  64  mtm=Thick    ss=1
  (D:$91000328), // GB_TILE_MODE23 0x17 kTileModeThick_2dThickPrt      am=2dTiledThickPrt  pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Thick    ss=1
  (D:$910002BC), // GB_TILE_MODE24 0x18 kTileModeThick_3dThickPrt      am=3dTiledThickPrt  pipe/alt=P8_32x32_8x16 /P16  ts=  64  mtm=Thick    ss=1
  (D:$91000320), // GB_TILE_MODE25 0x19 kTileModeThick_2dXThick        am=2dTiledXThick    pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Thick    ss=1
  (D:$910002B8), // GB_TILE_MODE26 0x1A kTileModeThick_3dXThick        am=3dTiledXThick    pipe/alt=P8_32x32_8x16 /P16  ts=  64  mtm=Thick    ss=1
  (D:$90C00308), // GB_TILE_MODE27 0x1B kTileModeRotated_1dThin        am=1dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Rotated  ss=1
  (D:$92C00310), // GB_TILE_MODE28 0x1C kTileModeRotated_2dThin        am=2dTiledThin      pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Rotated  ss=2
  (D:$92C00294), // GB_TILE_MODE29 0x1D kTileModeRotated_ThinPrt       am=TiledThinPrt     pipe/alt=P8_32x32_8x16 /P16  ts=  64  mtm=Rotated  ss=2
  (D:$92C00318), // GB_TILE_MODE30 0x1E kTileModeRotated_2dThinPrt     am=2dTiledThinPrt   pipe/alt=P8_32x32_16x16/P16  ts=  64  mtm=Rotated  ss=2
  (D:$00000000)  // GB_TILE_MODE31 0x1F kTileModeDisplay_LinearGeneral am=LinearGeneral    pipe/alt=P2            / P2  ts=  64  mtm=Display  ss=1
  );

 GB_MACROTILE_MODE:array[0..15] of TMACRO_TILE_MODE=(
  (D:$26E8), // GB_MACROTILE_MODE0  0x00 kMacroTileMode_1x4_16      bankWidth=1 bankHeight=4 macroTileAspect=4 numBanks=16 altBankHeight=4 altNumBanks= 8 altMacroTileAspect=2
  (D:$26D4), // GB_MACROTILE_MODE1  0x01 kMacroTileMode_1x2_16      bankWidth=1 bankHeight=2 macroTileAspect=2 numBanks=16 altBankHeight=4 altNumBanks= 8 altMacroTileAspect=2
  (D:$21D0), // GB_MACROTILE_MODE2  0x02 kMacroTileMode_1x1_16      bankWidth=1 bankHeight=1 macroTileAspect=2 numBanks=16 altBankHeight=2 altNumBanks= 8 altMacroTileAspect=1
  (D:$21D0), // GB_MACROTILE_MODE3  0x03 kMacroTileMode_1x1_16_dup  bankWidth=1 bankHeight=1 macroTileAspect=2 numBanks=16 altBankHeight=2 altNumBanks= 8 altMacroTileAspect=1
  (D:$2080), // GB_MACROTILE_MODE4  0x04 kMacroTileMode_1x1_8       bankWidth=1 bankHeight=1 macroTileAspect=1 numBanks= 8 altBankHeight=1 altNumBanks= 8 altMacroTileAspect=1
  (D:$2040), // GB_MACROTILE_MODE5  0x05 kMacroTileMode_1x1_4       bankWidth=1 bankHeight=1 macroTileAspect=1 numBanks= 4 altBankHeight=1 altNumBanks= 8 altMacroTileAspect=1
  (D:$1000), // GB_MACROTILE_MODE6  0x06 kMacroTileMode_1x1_2       bankWidth=1 bankHeight=1 macroTileAspect=1 numBanks= 2 altBankHeight=1 altNumBanks= 4 altMacroTileAspect=1
  (D:$0000), // GB_MACROTILE_MODE7  0x07 kMacroTileMode_1x1_2_dup   bankWidth=1 bankHeight=1 macroTileAspect=1 numBanks= 2 altBankHeight=1 altNumBanks= 2 altMacroTileAspect=1
  (D:$36EC), // GB_MACROTILE_MODE8  0x08 kMacroTileMode_1x8_16      bankWidth=1 bankHeight=8 macroTileAspect=4 numBanks=16 altBankHeight=4 altNumBanks=16 altMacroTileAspect=2
  (D:$26E8), // GB_MACROTILE_MODE9  0x09 kMacroTileMode_1x4_16_dup  bankWidth=1 bankHeight=4 macroTileAspect=4 numBanks=16 altBankHeight=4 altNumBanks= 8 altMacroTileAspect=2
  (D:$21D4), // GB_MACROTILE_MODE10 0x0A kMacroTileMode_1x2_16_dup  bankWidth=1 bankHeight=2 macroTileAspect=2 numBanks=16 altBankHeight=2 altNumBanks= 8 altMacroTileAspect=1
  (D:$20D0), // GB_MACROTILE_MODE11 0x0B kMacroTileMode_1x1_16_dup2 bankWidth=1 bankHeight=1 macroTileAspect=2 numBanks=16 altBankHeight=1 altNumBanks= 8 altMacroTileAspect=1
  (D:$1080), // GB_MACROTILE_MODE12 0x0C kMacroTileMode_1x1_8_dup   bankWidth=1 bankHeight=1 macroTileAspect=1 numBanks= 8 altBankHeight=1 altNumBanks= 4 altMacroTileAspect=1
  (D:$1040), // GB_MACROTILE_MODE13 0x0D kMacroTileMode_1x1_4_dup   bankWidth=1 bankHeight=1 macroTileAspect=1 numBanks= 4 altBankHeight=1 altNumBanks= 4 altMacroTileAspect=1
  (D:$0000), // GB_MACROTILE_MODE14 0x0E kMacroTileMode_1x1_2_dup2  bankWidth=1 bankHeight=1 macroTileAspect=1 numBanks= 2 altBankHeight=1 altNumBanks= 2 altMacroTileAspect=1
  (D:$0000)  // GB_MACROTILE_MODE15 0x0F kMacroTileMode_1x1_2_dup3  bankWidth=1 bankHeight=1 macroTileAspect=1 numBanks= 2 altBankHeight=1 altNumBanks= 2 altMacroTileAspect=1
 );

function getArrayMode(outArrayMode:PByte;tmode:Byte):Integer;
begin
 Result:=-$7f2d0000;
 if ((outArrayMode<>nil) and (tmode<$20)) then
 begin
  outArrayMode^:=GB_TILE_MODE[tmode].B.ARRAY_MODE;
  Result:=0;
 end;
end;

function getMicroTileMode(outMicroTileMode:PByte;tmode:Byte):Integer;
begin
 Result:=-$7f2d0000;
 if ((outMicroTileMode<>nil) and (tmode<$20)) then
 begin
  outMicroTileMode^:=GB_TILE_MODE[tmode].B.MICRO_TILE_MODE_NEW;
  Result:=0;
 end;
end;

function getSampleSplit(outSampleSplit:PByte;tmode:Byte):Integer;
begin
 Result:=-$7f2d0000;
 if ((outSampleSplit<>nil) and (tmode<$20)) then
 begin
  outSampleSplit^:=GB_TILE_MODE[tmode].B.SAMPLE_SPLIT;
  Result:=0;
 end;
end;

function getTileSplit(outTileSplit:PByte;tmode:Byte):Integer;
begin
 Result:=-$7f2d0000;
 if ((outTileSplit<>nil) and (tmode<$20)) then
 begin
  outTileSplit^:=GB_TILE_MODE[tmode].B.TILE_SPLIT;
  Result:=0;
 end;
end;

function getPipeConfig(outPipeConfig:PDWORD;tileMode:Byte):Integer;
begin
 outPipeConfig^:=GB_TILE_MODE[tileMode].B.PIPE_CONFIG;
 Result:=0;
end;

function getAltPipeConfig(outAltPipeConfig:PDWORD;tileMode:Byte):Integer;
begin
 outAltPipeConfig^:=GB_TILE_MODE[tileMode].B.ALT_PIPE_CONFIG;
 Result:=0;
end;

function RenderTarget.getTileMode:Byte; inline;
begin
 Result:=ATTRIB.TILE_MODE_INDEX;
end;

function RenderTarget.getWidth:WORD; inline;
begin
 Result:=Width;
end;

function RenderTarget.getHeight:WORD; inline;
begin
 Result:=Height;
end;

function RenderTarget.getMinimumGpuMode:Byte; inline;
begin
 Result:=INFO.ALT_TILE_MODE;
end;

function RenderTarget.getNumFragments:Byte; inline;
begin
 Result:=ATTRIB.NUM_FRAGMENTS;
end;

function RenderTarget.getPitchDiv8Minus1:Word; inline;
begin
 Result:=PITCH.TILE_MAX;
end;

function RenderTarget.getPitch:DWORD; inline;
begin
 Result:=(getPitchDiv8Minus1+1)*8;
end;

function RenderTarget.getDccCompressionEnable:Boolean; inline;
begin
 Result:=INFO.DCC_ENABLE<>0;
end;

const
 chY_1:array[0..2] of Integer=(5,0,4);
 chX_1:array[0..2] of Integer=(4,4,5);
 chW_1:array[0..2] of Integer=(1,5,1);

 chY_2:array[0..2] of Integer=(5,5,6);
 chZ_2:array[0..2] of Integer=(6,4,5);
 chX_2:array[0..2] of Integer=(4,6,7);
 chW_2:array[0..2] of Integer=(7,7,4);

 chZ_3:array[0..2] of Integer=(6,0,4);
 chX_3:array[0..2] of Integer=(4,4,6);
 chW_3:array[0..2] of Integer=(1,6,1);

 chZ_4:array[0..2] of Integer=(0,0,4);
 chY_4:array[0..2] of Integer=(0,4,0);
 chX_4:array[0..2] of Integer=(4,0,0);

function sce_Gnm_DataFormat_build(FORMAT,NUMBER_TYPE,COMP_SWAP:Byte):TDATA_FORMAT;
var
 m_channelX:Integer;
 m_channelY:Integer;
 m_channelZ:Integer;
 m_channelW:Integer;
 m_channelType:Integer;
 IS_SWAP_ALT_REV:Boolean;
label
 _end,_zero;
begin

 if (($1800004000000016 shr (FORMAT and $3f) and 1)=0) then
 begin

  if (($3fff08000700828 shr (FORMAT and $3f) and 1)<>0) then
  begin
   IS_SWAP_ALT_REV:=COMP_SWAP=3;
   if (COMP_SWAP<3) then
   begin
    m_channelY:=chY_1[COMP_SWAP];
    m_channelX:=chX_1[COMP_SWAP];
    m_channelW:=chW_1[COMP_SWAP];
    m_channelZ:=0;
   end else
   begin
    m_channelY:=0;
    m_channelX:=ord(IS_SWAP_ALT_REV)+ord(IS_SWAP_ALT_REV)*4;
    m_channelW:=ord(IS_SWAP_ALT_REV)*3+1;
    m_channelZ:=0;
   end;
   goto _end;
  end;

  if (($4000107000120c0 shr (FORMAT and $3f) and 1)=0) then
  begin

   if (($238000e5700 shr (FORMAT and $3f) and 1)=0) then
   begin
    m_channelW:=1;
    goto _zero;
   end;

   if (COMP_SWAP<3) then
   begin
    m_channelY:=chY_2[COMP_SWAP];
    m_channelZ:=chZ_2[COMP_SWAP];
    m_channelX:=chX_2[COMP_SWAP];
    m_channelW:=chW_2[COMP_SWAP];
    goto _end;
   end;

   IS_SWAP_ALT_REV:=COMP_SWAP=3;
   m_channelX:=ord(IS_SWAP_ALT_REV)*5;
   m_channelY:=6;
   m_channelZ:=7;
   if (not IS_SWAP_ALT_REV) then
   begin
    m_channelZ:=0;
    m_channelY:=0;
   end;


  end else
  begin

   if (COMP_SWAP<3) then
   begin
    m_channelZ:=chZ_3[COMP_SWAP];
    m_channelX:=chX_3[COMP_SWAP];
    m_channelW:=chW_3[COMP_SWAP];
    m_channelY:=5;
    goto _end;
   end;

   m_channelX:=6;
   m_channelY:=ord(COMP_SWAP=3)*5;
   m_channelZ:=0;
   if (COMP_SWAP<>3) then
   begin
    m_channelX:=0;
   end;

  end;

  m_channelW:=ord(COMP_SWAP=3)*3+1;

 end else
 begin

  if (COMP_SWAP<3) then
  begin
   m_channelW:=1;
   m_channelZ:=chZ_4[COMP_SWAP];
   m_channelY:=chY_4[COMP_SWAP];
   m_channelX:=chX_4[COMP_SWAP];
   goto _end;
  end;

  m_channelW:=ord(COMP_SWAP=3)*3+1;

  _zero:

  m_channelZ:=0;
  m_channelY:=0;
  m_channelX:=0;

 end;

 _end:

 m_channelType:=$9;

 if (NUMBER_TYPE<>6) then
 begin
  m_channelType:=(NUMBER_TYPE and $F);
 end;

 Result.m_surfaceFormat:=FORMAT;
 Result.m_channelType  :=m_channelType;
 Result.m_channelX     :=m_channelX;
 Result.m_channelY     :=m_channelY;
 Result.m_channelZ     :=m_channelZ;
 Result.m_channelW     :=m_channelW;
 Result.m_unused       :=0;
end;

function RenderTarget.getDataFormat:TDATA_FORMAT;
begin
 Result:=sce_Gnm_DataFormat_build(INFO.FORMAT,INFO.NUMBER_TYPE,INFO.COMP_SWAP);
end;

function isMacroTiled(tileMode:Byte):Boolean; inline;
begin
 Result:=($7f7dcdf shr (tileMode and $3f) and 1)<>0;
end;

function isPartiallyResidentTexture(arrayMode:Byte):Boolean; inline;
begin
 Result:=($8e60 shr (arrayMode and $3f) and 1)<>0;
end;

function isPowerOfTwo(x:DWORD):Boolean;
begin
 if (x<>0) then
 begin
  Result:=((x-1) and x)=0;
 end else
 begin
  Result:=False;
 end;
end;

function getMicroTileThickness(arrayMode:Byte):Byte;
begin
 Case arrayMode of
  kArrayMode1dTiledThick   ,
  kArrayMode2dTiledThick   ,
  kArrayMode3dTiledThick   ,
  kArrayModeTiledThickPrt  ,
  kArrayMode2dTiledThickPrt,
  kArrayMode3dTiledThickPrt:Result:=4;
  kArrayMode2dTiledXThick  ,
  kArrayMode3dTiledXThick  :Result:=8;
  kArrayModeLinearGeneral  ,
  kArrayModeLinearAligned  ,
  kArrayMode1dTiledThin    ,
  kArrayMode2dTiledThin    ,
  kArrayModeTiledThinPrt   ,
  kArrayMode2dTiledThinPrt ,
  kArrayMode3dTiledThinPrt ,
  kArrayMode3dTiledThin    :Result:=1;
  else
   Result:=0;
 end;
end;

function fastIntLog2(i:DWORD):DWORD; inline;
begin
 Result:=BsrDWord(i or 1);
end;

Function computeSurfaceMacroTileMode(outMacroTileMode:PByte;tileMode,bitsPerElement,numFragmentsPerPixel:Byte):Integer;
var
 colorTileSplit:DWORD;
 tileSplit:DWORD;
 tileSplitC:DWORD;
 tileBytes1x:DWORD;
 tileBytes:DWORD;
 mtmIndex:DWORD;
 tileSplitHw:Byte;
 sampleSplitHw:Byte;
 microTileMode:Byte;
 tileThickness:Byte;
 arrayMode:Byte;
begin
 Result:=-$7f2d0000;
 if (outMacroTileMode <> nil) then Exit;

 if (numFragmentsPerPixel > 8) or (not isPowerOfTwo(numFragmentsPerPixel)) then Exit;

 Result := getArrayMode(@arrayMode,tileMode);
 if (Result <> 0) then Exit;

 Result := -$7f2d0000;

 if (bitsPerElement < 1) or (bitsPerElement > 128) or (not isMacroTiled(arrayMode)) then Exit;

 if (numFragmentsPerPixel < 1) or (numFragmentsPerPixel > 16) or (not isPowerOfTwo(numFragmentsPerPixel)) then Exit;

 Result := getMicroTileMode(@microTileMode,tileMode);
 if (Result <> 0) then Exit;

 Result := getSampleSplit(@sampleSplitHw,tileMode);
 if (Result <> 0) then Exit;

 Result := getTileSplit(@tileSplitHw,tileMode);
 if (Result <> 0) then Exit;

 tileThickness := getMicroTileThickness(arrayMode);
 tileBytes1x := bitsPerElement * tileThickness * 8 {64 shr 3};

 colorTileSplit:=max(256, tileBytes1x shl (sampleSplitHw and $1f));

 if (microTileMode=kMicroTileModeDepth) then
  tileSplit:=(64 shl tileSplitHw)
 else
  tileSplit:=colorTileSplit;

 tileSplitC:=min(kDramRowSize, tileSplit);

 tileBytes:=min(tileSplitC, numFragmentsPerPixel*tileBytes1x);

 Result := -$7f2d0000;

 if (not isPowerOfTwo(tileBytes)) or (tileBytes < 64) or (tileBytes > 4096) then Exit;

 mtmIndex:=fastIntLog2(tileBytes shr 6); //div 64

 if isPartiallyResidentTexture(arrayMode) then
  outMacroTileMode^:=mtmIndex+8
 else
  outMacroTileMode^:=mtmIndex;

end;

function getPipeCount(pipeConfig:Byte):DWORD; forward;

procedure computeHtileInfo(outHtileSizeBytes:PPtruint;
                           outHtileAlign    :PPtruint;
                           outHtilePitch    :PWord;
                           outHtileHeight   :PWord;
                           //
                           Pitch :DWORD;
                           Height:DWORD;
                           Slice :DWORD;
                           //
                           isHtileLinear :Boolean;
                           isTcCompatible:Boolean;
                           tileMode      :Byte
                          );
const
 bitsPerElement    =32;
 cacheBits         =kHtileCacheBits;
 htileCacheLineSize=kHtileCacheBits div 8;
 numTiles          =8;
var
 NumSlices   :DWORD;
 pipeConfig  :DWORD;
 numPipes    :DWORD;
 macroWidth  :DWORD;
 macroHeight :DWORD;
 htilePitch  :DWORD;
 htileHeight :DWORD;
 htileAlign  :DWORD;
 surfaceBytes:Ptruint;
 cacheAlign  :Ptruint;
 htileBytes  :Ptruint;
begin
 PipeConfig:=0;
 getPipeConfig(@pipeConfig,tileMode);
 numPipes:=getPipeCount(pipeConfig);

 //Pitch  = getPitch  -> (DB_DEPTH_SIZE.PITCH_TILE_MAX +1)*8;
 //Height = getHeight -> (DB_DEPTH_SIZE.HEIGHT_TILE_MAX+1)*8;
 //Slice  -> DB_DEPTH_VIEW.SLICE_MAX
 NumSlices:=1+Slice;

 if isHtileLinear then
 begin
  macroWidth :=numTiles*kMicroTileWidth;
  macroHeight:=numTiles*kMicroTileHeight;
 end else
 begin
  case numPipes of
   8:
    begin
     macroWidth :=512; //8*64
     macroHeight:=512; //8*8*8
    end;
   16:
    begin
     macroWidth :=1024; //8*128
     macroHeight:=512;  //8*4*16
    end;
   else
    begin
     macroWidth :=0;
     macroHeight:=0;
    end;
  end;
 end;

 htilePitch :=(Pitch +(macroWidth -1)) and (not (macroWidth -1));
 htileHeight:=(Height+(macroHeight-1)) and (not (macroHeight-1));

 //surfaceBytes:=htilePitch*htileHeight*(bitsPerElement div 8)*NumSlices div 64;
 surfaceBytes:=htilePitch*htileHeight*NumSlices div 16;

 cacheAlign:=htileCacheLineSize*numPipes;

 if (outHtileSizeBytes<>nil) then
 begin
  htileBytes:=(surfaceBytes+(cacheAlign-1)) and (not (cacheAlign-1));
  outHtileSizeBytes^:=htileBytes;
 end;

 if (outHtileAlign<>nil) then
 begin
  htileAlign:=kPipeInterleaveBytes*numPipes;

  if isTcCompatible then
  begin
   htileAlign:=htileAlign*(2 shl kNumBanks8);
  end;

  outHtileAlign^:=htileAlign;
 end;

 if (outHtilePitch<>nil) then
 begin
  outHtilePitch^:=htilePitch;
 end;

 if (outHtileHeight<>nil) then
 begin
  outHtileHeight^:=htileHeight;
 end;

end;

Function getAltNumBanks(outAltNumBanks:PByte;tileMode,bitsPerElement,numFragmentsPerPixel:Byte):Integer;
var
 _MacroTileMode:Byte;
begin
 Result:=-$7f2d0000;
 if (outAltNumBanks<>nil) then
 begin
   Result := computeSurfaceMacroTileMode(@_MacroTileMode,tileMode,bitsPerElement,numFragmentsPerPixel);
   if (Result = 0) then
   begin
    outAltNumBanks^:=GB_MACROTILE_MODE[_MacroTileMode].B.ALT_NUM_BANKS;
   end;
 end;
end;

Function getNumBanks(outNumBanks:PByte;tileMode,bitsPerElement,numFragmentsPerPixel:Byte):Integer;
var
 _MacroTileMode:Byte;
begin
 Result:=-$7f2d0000;
 if (outNumBanks<>nil)  then
 begin
   Result := computeSurfaceMacroTileMode(@_MacroTileMode,tileMode,bitsPerElement,numFragmentsPerPixel);
   if (Result = 0)  then
   begin
    outNumBanks^:=GB_MACROTILE_MODE[_MacroTileMode].B.NUM_BANKS
   end;
 end;
end;

function getAllMacroTileData(tileMode,
                             bitsPerElement,
                             numFragmentsPerPixel:Byte;
                             outBankWidth,
                             outBankHeight,
                             outMacroTileAspect,
                             outNumBanks:PDWORD):Integer;
var
 mtmReg:TMACRO_TILE_MODE_REG;
 macroTileMode:Byte;
begin
 Result:=computeSurfaceMacroTileMode(@macroTileMode,tileMode,bitsPerElement,numFragmentsPerPixel);

 if (Result=0) then
 begin
  mtmReg:=GB_MACROTILE_MODE[macroTileMode].B;

  if (outBankWidth<>nil) then
   outBankWidth^:=mtmReg.BANK_WIDTH;

  if (outBankHeight<>nil) then
   outBankHeight^:=mtmReg.BANK_HEIGHT;

  if (outMacroTileAspect<>nil) then
   outMacroTileAspect^:=mtmReg.MACRO_TILE_ASPECT;

  if (outNumBanks<>nil) then
   outNumBanks^:=mtmReg.NUM_BANKS;

 end;
end;

function getAllAltMacroTileData(tileMode,
                                bitsPerElement,
                                numFragmentsPerPixel:Byte;
                                outBankWidth,
                                outAltBankHeight,
                                outAltMacroTileAspect,
                                outAltNumBanks:PDWORD):Integer;
var
 mtmReg:TMACRO_TILE_MODE_REG;
 macroTileMode:Byte;
begin
 Result:=computeSurfaceMacroTileMode(@macroTileMode,tileMode,bitsPerElement,numFragmentsPerPixel);

 if (Result=0) then
 begin
  mtmReg:=GB_MACROTILE_MODE[macroTileMode].B;

  if (outBankWidth<>nil) then
   outBankWidth^:=mtmReg.BANK_WIDTH;

  if (outAltBankHeight<>nil) then
   outAltBankHeight^:=mtmReg.ALT_BANK_HEIGHT;

  if (outAltMacroTileAspect<>nil) then
   outAltMacroTileAspect^:=mtmReg.ALT_MACRO_TILE_ASPECT;

  if (outAltNumBanks<>nil) then
   outAltNumBanks^:=mtmReg.ALT_NUM_BANKS;

 end;
end;


function RenderTarget.getTileSwizzleMask:Byte;
var
 _isMacroTiled:Boolean;
 dataFormat:TDATA_FORMAT;
 m_bitsPerFragment:Byte;
 sVar1:Integer;
 _NumBanks:Byte;
begin
 if (Integer(INFO) < 0) then
 begin
   dataFormat:=getDataFormat;
   if (dataFormat.m_surfaceFormat <> 0) then
   begin
     _isMacroTiled:=isMacroTiled(ATTRIB.TILE_MODE_INDEX);
     if (_isMacroTiled <> false) then
     begin
       m_bitsPerFragment:=getTotalBitsPerElement(dataFormat);
       if (Integer(INFO) < 0) then
       begin
         getAltNumBanks(@_NumBanks,ATTRIB.TILE_MODE_INDEX,m_bitsPerFragment,ATTRIB.NUM_FRAGMENTS);
         sVar1:=4;
       end else
       begin
         getNumBanks(@_NumBanks,ATTRIB.TILE_MODE_INDEX,m_bitsPerFragment,ATTRIB.NUM_FRAGMENTS);
         sVar1:=3;
       end;
       Result := ((((1 shl ((_NumBanks + 1) and $1f)) -1) shl sVar1) and BASE) shr 4;
       Exit;
     end;
   end;
 end;
 Result:=0;
end;

function getFlagsForSurfaceType(minGpuMode:Byte;outFlags:PSurfaceFlags;surfaceType:Byte;mipmapMode:Byte):Integer;
begin
 if (outFlags=nil) then Exit(-$7f2d0000);
 DWORD(outFlags^):=0;
 case surfaceType of
  kSurfaceTypeColorTargetDisplayable:;
  kSurfaceTypeColorTarget:;
  kSurfaceTypeDepthTarget:
  begin
   outFlags^.m_depthTarget  :=1;
   outFlags^.m_stencilTarget:=1;
  end;
  kSurfaceTypeDepthOnlyTarget:
  begin
   outFlags^.m_depthTarget:=1;
  end;
  kSurfaceTypeStencilOnlyTarget:
  begin
   outFlags^.m_stencilTarget:=1;
  end;
  kSurfaceTypeFmaskBuffer:
  begin
   outFlags^.m_fmask:=1;
  end;
  kSurfaceTypeTextureFlat,
  kSurfaceTypeRwTextureFlat:
  begin
   outFlags^.m_pow2Pad      := (mipmapMode and 1); // Must be set for textures w/mipmaps.
   outFlags^.m_texCompatible:= (minGpuMode and 1);
  end;
  kSurfaceTypeTextureVolume,
  kSurfaceTypeRwTextureVolume:
  begin
   outFlags^.m_volume:= 1;
   outFlags^.m_pow2Pad      := (mipmapMode and 1); // Must be set for textures w/mipmaps.
   outFlags^.m_texCompatible:= (minGpuMode and 1);
  end;
  kSurfaceTypeTextureCubemap,
  kSurfaceTypeRwTextureCubemap:
  begin
   outFlags^.m_cube:= 1;
   outFlags^.m_pow2Pad      := (mipmapMode and 1); // Must be set for textures w/mipmaps.
   outFlags^.m_texCompatible:= (minGpuMode and 1);
  end;
  else
   Exit(-$7f2d0000);
 end;
 Result:=0;
end;

{
int32_t sce::GpuAddress::TilingParameters::initFromTexture(const Gnm::Texture *texture, uint32_t mipLevel, uint32_t arraySlice)

 SCE_GNM_ASSERT_MSG_RETURN(texture != 0, kStatusInvalidArgument, "texture must not be NULL.");
 SCE_GNM_ASSERT_MSG_RETURN(mipLevel <= texture->getLastMipLevel(), kStatusInvalidArgument, "mipLevel (%u) is out of range for texture; last level is %u", mipLevel, texture->getLastMipLevel());
 bool isCubemap = (texture->getTextureType() == Gnm::kTextureTypeCubemap);
 bool isVolume = (texture->getTextureType() == Gnm::kTextureType3d);
 // Building surface flags manually is error-prone, but we don't know exactly what type of texture this is.
 m_surfaceFlags.m_value = 0;
 Gnm::MicroTileMode microTileMode;
 int32_t status = getMicroTileMode(&microTileMode, texture->getTileMode());
 if (status != kStatusSuccess)
  return status;
 m_surfaceFlags.m_depthTarget   = (!isVolume && (microTileMode == Gnm::kMicroTileModeDepth) && (texture->getDataFormat().getZFormat()       != Gnm::kZFormatInvalid)) ? 1 : 0;
 m_surfaceFlags.m_stencilTarget = (!isVolume && (microTileMode == Gnm::kMicroTileModeDepth) && (texture->getDataFormat().getStencilFormat() != Gnm::kStencilInvalid)) ? 1 : 0;
 m_surfaceFlags.m_cube = isCubemap ? 1 : 0;
 m_surfaceFlags.m_volume = isVolume ? 1 : 0;
 m_surfaceFlags.m_pow2Pad = texture->isPaddedToPow2() ? 1 : 0;
 if (texture->getMinimumGpuMode() == Gnm::kGpuModeNeo)
 {
  m_surfaceFlags.m_texCompatible = 1;
 }
 m_tileMode = texture->getTileMode(); // see below, though
 m_minGpuMode = texture->getMinimumGpuMode();
 Gnm::DataFormat dataFormat = texture->getDataFormat();
 m_bitsPerFragment = dataFormat.getTotalBitsPerElement() / dataFormat.getTexelsPerElement();
 m_isBlockCompressed = (dataFormat.getTexelsPerElement() > 1);
 m_tileSwizzleMask = texture->getTileSwizzleMask();
 m_linearWidth = std::max(texture->getWidth() >> mipLevel, 1U);
 m_linearHeight = std::max(texture->getHeight() >> mipLevel, 1U);
 m_linearDepth = m_surfaceFlags.m_volume ? std::max(texture->getDepth() >> mipLevel, 1U) : 1;
 m_numFragmentsPerPixel = 1 << texture->getNumFragments();
 m_baseTiledPitch = texture->getPitch();
 m_mipLevel = mipLevel;
 SCE_GNM_ASSERT_MSG_RETURN(arraySlice == 0 || !m_surfaceFlags.m_volume, kStatusInvalidArgument, "for volume textures, arraySlice must be 0."); // volume textures can't be arrays
 uint32_t arraySliceCount = texture->getTotalArraySliceCount();
 if (isCubemap)
  arraySliceCount *= 6; // Cube maps store 6 faces per array slice
 else if (isVolume)
  arraySliceCount = 1;
 if (texture->isPaddedToPow2())
  arraySliceCount = nextPowerOfTwo(arraySliceCount); // array slice counts are padded to a power of two as well
 SCE_GNM_ASSERT_MSG_RETURN(arraySlice < arraySliceCount, kStatusInvalidArgument, "arraySlice (%u) is out of range for texture (0x%p) with %u slices.", arraySlice, texture, arraySliceCount);
 m_arraySlice = arraySlice;
 // Use computeSurfaceInfo() to determine what array mode we REALLY need to use, since it's occasionally not the one the Texture uses.
 // (e.g. for a 2D-tiled texture, the smaller mip levels will implicitly use a 1D array mode to cut down on wasted padding space)
 SurfaceInfo surfInfoOut = {0};
 status = computeSurfaceInfo(&surfInfoOut, this);
 if (status != kStatusSuccess)
  return status;
 status = adjustTileMode(m_minGpuMode, &m_tileMode, m_tileMode, surfInfoOut.m_arrayMode);
 if (status != kStatusSuccess)
  return status;
 return kStatusSuccess;
}


function TilingParameters.initFromRenderTarget(var target:RenderTarget;arraySlice:DWORD):Integer;
var
 dataFormat:TDATA_FORMAT;
 status:Integer;
 maxUncompressedBlockSize:Integer;
 st:Byte;
 microTileMode:Byte;
 independentDccBlocks:Boolean;
 isDccEnabled:Boolean;
begin
 m_tileMode:=target.getTileMode(); // see below, though
 m_minGpuMode:=target.getMinimumGpuMode();

 dataFormat:=target.getDataFormat;

 m_bitsPerFragment:=getTotalBitsPerElement(dataFormat) div getTexelsPerElement(dataFormat);
 m_isBlockCompressed:=(getTexelsPerElement(dataFormat)>1);

 m_tileSwizzleMask:= target.getTileSwizzleMask();
 m_linearWidth :=target.getWidth();
 m_linearHeight:=target.getHeight();
 m_linearDepth :=1;
 m_numFragmentsPerPixel:=1 shl target.getNumFragments();
 m_baseTiledPitch:=target.getPitch();
 m_mipLevel :=0; // unused for render targets
 m_arraySlice:=arraySlice;

 status:=getMicroTileMode(@microTileMode,target.getTileMode);
 if (status<>0) then Exit(status);

 if (microTileMode=kMicroTileModeDisplay) then
  st:=kSurfaceTypeColorTargetDisplayable
 else
  st:=kSurfaceTypeColorTarget;

 status:=getFlagsForSurfaceType(m_minGpuMode,@m_surfaceFlags,st,kSurfaceMipmapDisable);
 if (status<>0) then Exit(status);

 if (m_minGpuMode=kGpuModeNeo) then
 begin
  independentDccBlocks    :=target.DCC_CONTROL.INDEPENDENT_64B_BLOCKS<>0;
  maxUncompressedBlockSize:=target.DCC_CONTROL.MAX_COMPRESSED_BLOCK_SIZE;

  isDccEnabled:=target.getDccCompressionEnable();

  if (isDccEnabled and (not independentDccBlocks or (maxUncompressedBlockSize>kDccBlockSize64))) then
   m_surfaceFlags.m_texCompatible:=0
  else
   m_surfaceFlags.m_texCompatible:=1;
 end;

 //

 Result:=0;
end;

//int32_t sce::GpuAddress::TilingParameters::initFromRenderTarget(const Gnm::RenderTarget *target, uint32_t arraySlice)
//{

// SurfaceInfo surfInfoOut = {0};
// status = computeSurfaceInfo(&surfInfoOut, this);
// if (status != kStatusSuccess)
//  return status;
// status = adjustTileMode(m_minGpuMode, &m_tileMode, m_tileMode, surfInfoOut.m_arrayMode);
// if (status != kStatusSuccess)
//  return status;
// return kStatusSuccess;
//}

function TilingParameters.initFromRenderTargetSpec(var target:RenderTargetSpec;arraySlice:DWORD):Integer;
var
 status:Integer;
 maxUncompressedBlockSize:Integer;
 dFormat:TDATA_FORMAT;
 microTileMode:Byte;
 st:Byte;
 independentDccBlocks:Boolean;
 isDccEnabled:Boolean;
begin
  m_tileMode := target.m_colorTileModeHint; // see below, though
  m_minGpuMode := target.m_minGpuMode;
  dFormat := target.m_colorFormat;
  m_bitsPerFragment := getTotalBitsPerElement(dFormat) div getTexelsPerElement(dFormat);
  m_isBlockCompressed := (getTexelsPerElement(dFormat) > 1);
  m_tileSwizzleMask := target.m_regs.getTileSwizzleMask();
  m_linearWidth := target.m_width;
  m_linearHeight := target.m_height;
  m_linearDepth := 1;
  m_numFragmentsPerPixel := 1 shl target.m_numFragments;
  m_baseTiledPitch := target.m_pitch;
  m_mipLevel := 0; // unused for render targets
  m_arraySlice := arraySlice;

  status:=getMicroTileMode(@microTileMode,target.m_regs.getTileMode);
  if (status<>0) then Exit(status);

  if (microTileMode=kMicroTileModeDisplay) then
   st:=kSurfaceTypeColorTargetDisplayable
  else
   st:=kSurfaceTypeColorTarget;

  status:=getFlagsForSurfaceType(m_minGpuMode,@m_surfaceFlags,st,kSurfaceMipmapDisable);
  if (status<>0) then Exit(status);

  if (m_minGpuMode=kGpuModeNeo) then
  begin
   independentDccBlocks    :=target.m_regs.DCC_CONTROL.INDEPENDENT_64B_BLOCKS<>0;
   maxUncompressedBlockSize:=target.m_regs.DCC_CONTROL.MAX_COMPRESSED_BLOCK_SIZE;

   isDccEnabled:=target.m_regs.getDccCompressionEnable();

   if (isDccEnabled and (not independentDccBlocks or (maxUncompressedBlockSize>kDccBlockSize64))) then
    m_surfaceFlags.m_texCompatible:=0
   else
    m_surfaceFlags.m_texCompatible:=1;
  end;

  {

  // Use computeSurfaceInfo() to determine what array mode we REALLY need to use, since it's occasionally not the one the Texture uses.
  // (e.g. for a 2D-tiled texture, the smaller mip levels will implicitly use a 1D array mode to cut down on wasted padding space)
  SurfaceInfo surfInfoOut = {0};
  status = computeSurfaceInfo(&surfInfoOut, this);
  if (status != kStatusSuccess)
   return status;
  status = adjustTileMode(m_minGpuMode, &m_tileMode, m_tileMode, surfInfoOut.m_arrayMode);
  if (status != kStatusSuccess)
   return status;
  return kStatusSuccess;}

  Result:=0;
end;

function _getElementIndex(x,y,z,bitsPerElement,microTileMode,arrayMode:DWORD):DWORD;
var
 elem:DWORD;
begin
 elem:=0;
 case microTileMode of
  kMicroTileModeDisplay:
    begin
     case bitsPerElement of
      8:
        begin
         elem:=elem or ( (x shr 0) and $1 ) shl 0;
         elem:=elem or ( (x shr 1) and $1 ) shl 1;
         elem:=elem or ( (x shr 2) and $1 ) shl 2;
         elem:=elem or ( (y shr 1) and $1 ) shl 3;
         elem:=elem or ( (y shr 0) and $1 ) shl 4;
         elem:=elem or ( (y shr 2) and $1 ) shl 5;
        end;
      16:
        begin
         elem:=elem or ( (x shr 0) and $1 ) shl 0;
         elem:=elem or ( (x shr 1) and $1 ) shl 1;
         elem:=elem or ( (x shr 2) and $1 ) shl 2;
         elem:=elem or ( (y shr 0) and $1 ) shl 3;
         elem:=elem or ( (y shr 1) and $1 ) shl 4;
         elem:=elem or ( (y shr 2) and $1 ) shl 5;
        end;
      32:
        begin
   elem:=elem or ( (x shr 0) and $1 ) shl 0;
   elem:=elem or ( (x shr 1) and $1 ) shl 1;
   elem:=elem or ( (y shr 0) and $1 ) shl 2;
   elem:=elem or ( (x shr 2) and $1 ) shl 3;
   elem:=elem or ( (y shr 1) and $1 ) shl 4;
   elem:=elem or ( (y shr 2) and $1 ) shl 5;
        end;
      64:
        begin
   elem:=elem or ( (x shr 0) and $1 ) shl 0;
   elem:=elem or ( (y shr 0) and $1 ) shl 1;
   elem:=elem or ( (x shr 1) and $1 ) shl 2;
   elem:=elem or ( (x shr 2) and $1 ) shl 3;
   elem:=elem or ( (y shr 1) and $1 ) shl 4;
   elem:=elem or ( (y shr 2) and $1 ) shl 5;
        end;
      else;
       //Assert(false,'Unsupported bitsPerElement (%u) for displayable surface.');
     end;
    end;
   kMicroTileModeThin,
   kMicroTileModeDepth:
     begin
      elem:=elem or ( (x shr 0) and $1 ) shl 0;
      elem:=elem or ( (y shr 0) and $1 ) shl 1;
      elem:=elem or ( (x shr 1) and $1 ) shl 2;
      elem:=elem or ( (y shr 1) and $1 ) shl 3;
      elem:=elem or ( (x shr 2) and $1 ) shl 4;
      elem:=elem or ( (y shr 2) and $1 ) shl 5;
      case arrayMode of
       kArrayMode2dTiledXThick,
       kArrayMode3dTiledXThick:
         begin
          elem:=elem or ( (z shr 2) and $1 ) shl 8;
         end;
       kArrayMode1dTiledThick,
       kArrayMode2dTiledThick,
       kArrayMode3dTiledThick,
       kArrayModeTiledThickPrt,
       kArrayMode2dTiledThickPrt,
       kArrayMode3dTiledThickPrt:
         begin
          elem:=elem or ( (z shr 0) and $1 ) shl 6;
          elem:=elem or ( (z shr 1) and $1 ) shl 7;
         end;
       else;
      end;
     end;
   kMicroTileModeThick:
     begin
      case arrayMode of
       kArrayMode2dTiledXThick,
       kArrayMode3dTiledXThick:
         begin
   elem:=elem or ( (z shr 2) and $1 ) shl 8;
         end;
       kArrayMode1dTiledThick,
       kArrayMode2dTiledThick,
       kArrayMode3dTiledThick,
       kArrayModeTiledThickPrt,
       kArrayMode2dTiledThickPrt,
       kArrayMode3dTiledThickPrt:
        case bitsPerElement of
         8,16:
           begin
         elem:=elem or ( (x shr 0) and $1 ) shl 0;
         elem:=elem or ( (y shr 0) and $1 ) shl 1;
         elem:=elem or ( (x shr 1) and $1 ) shl 2;
         elem:=elem or ( (y shr 1) and $1 ) shl 3;
         elem:=elem or ( (z shr 0) and $1 ) shl 4;
         elem:=elem or ( (z shr 1) and $1 ) shl 5;
         elem:=elem or ( (x shr 2) and $1 ) shl 6;
         elem:=elem or ( (y shr 2) and $1 ) shl 7;
           end;
         32:
           begin
         elem:=elem or ( (x shr 0) and $1 ) shl 0;
         elem:=elem or ( (y shr 0) and $1 ) shl 1;
         elem:=elem or ( (x shr 1) and $1 ) shl 2;
         elem:=elem or ( (z shr 0) and $1 ) shl 3;
         elem:=elem or ( (y shr 1) and $1 ) shl 4;
         elem:=elem or ( (z shr 1) and $1 ) shl 5;
         elem:=elem or ( (x shr 2) and $1 ) shl 6;
         elem:=elem or ( (y shr 2) and $1 ) shl 7;
           end;
         64,128:
           begin
         elem:=elem or ( (x shr 0) and $1 ) shl 0;
         elem:=elem or ( (y shr 0) and $1 ) shl 1;
         elem:=elem or ( (z shr 0) and $1 ) shl 2;
         elem:=elem or ( (x shr 1) and $1 ) shl 3;
         elem:=elem or ( (y shr 1) and $1 ) shl 4;
         elem:=elem or ( (z shr 1) and $1 ) shl 5;
         elem:=elem or ( (x shr 2) and $1 ) shl 6;
         elem:=elem or ( (y shr 2) and $1 ) shl 7;
           end;
          else;
           //Assert(false,'Invalid bitsPerElement (%u) for microTileMode=kMicroTileModeThick.');
        end;
      else;
       //Assert(false,'Invalid arrayMode (0x%02X) for thick/xthick microTileMode=kMicroTileModeThick.');
      end;
     end;
 end;

 Result:=elem;
end;

{
type
 p_element_table_xyz=^t_element_table_xyz;
 //                           z    y    x
 t_element_table_xyz=array[0..7,0..7,0..7] of WORD;
}

var
 //                         bpe  mtm    am
 element_index_table:array[0..4,0..4,0..15] of t_element_table_xyz;

procedure init_element_index_table;
var
 x,y,z,bpe,mtm,am:Byte;
 bitsPerElement:DWORD;
 elem:DWORD;
begin
 For x:=0 to 7 do
 For y:=0 to 7 do
 For z:=0 to 7 do
 For bpe:=0 to 4 do
 For mtm:=0 to 4 do
 For am:=0 to 15 do
 begin
  bitsPerElement:=(1 shl bpe) shl 3;

  elem:=_getElementIndex(x,y,z,bitsPerElement,mtm,am);

  element_index_table[bpe,mtm,am][z,y,x]:=elem;
 end;
end;

function getElementIndex(x,y,z,bitsPerElement,microTileMode,arrayMode:DWORD):DWORD; inline;
var
 bpe:Byte;
begin
 bpe:=fastIntLog2(bitsPerElement shr 3);

 Result:=element_index_table[bpe,microTileMode,arrayMode][z and 7,y and 7,x and 7];
end;

function getElementTableXYZ(bitsPerElement,microTileMode,arrayMode:DWORD):p_element_table_xyz; inline;
var
 bpe:Byte;
begin
 bpe:=fastIntLog2(bitsPerElement shr 3);

 Result:=@element_index_table[bpe,microTileMode,arrayMode];
end;

function getPipeIndex(x,y,pipeCfg:DWORD):DWORD;
var
 pipe:DWORD;
begin
 pipe:= 0;
 case pipeCfg of
  kPipeConfigP8_32x32_8x16:
   begin
    pipe:=pipe or ( ((x shr 4) xor (y shr 3) xor (x shr 5)) and $1 )  shl  0;
    pipe:=pipe or ( ((x shr 3) xor (y shr 4))               and $1 )  shl  1;
    pipe:=pipe or ( ((x shr 5) xor (y shr 5))               and $1 )  shl  2;
   end;
  kPipeConfigP8_32x32_16x16:
   begin
    pipe:=pipe or ( ((x shr 3) xor (y shr 3) xor (x shr 4)) and $1 )  shl  0;
    pipe:=pipe or ( ((x shr 4) xor (y shr 4))               and $1 )  shl  1;
    pipe:=pipe or ( ((x shr 5) xor (y shr 5))               and $1 )  shl  2;
   end;
  kPipeConfigP16:
   begin
    pipe:=pipe or ( ((x shr 3) xor (y shr 3) xor (x shr 4)) and $1 )  shl  0;
    pipe:=pipe or ( ((x shr 4) xor (y shr 4))               and $1 )  shl  1;
    pipe:=pipe or ( ((x shr 5) xor (y shr 5))               and $1 )  shl  2;
    pipe:=pipe or ( ((x shr 6) xor (y shr 5))               and $1 )  shl  3;
   end;
  else
   Assert(false,'Unsupported pipeCfg (0x%02X).');
 end;
 Result:=pipe;
end;

function getBankIndex(x,y,bank_width,bank_height,num_banks,num_pipes:DWORD):DWORD;
var
 x_shift_offset,
 y_shift_offset,
 xs,ys:DWORD;
 bank:DWORD;
begin
 x_shift_offset := fastIntLog2(bank_width * num_pipes);
 y_shift_offset := fastIntLog2(bank_height);
 xs := x shr x_shift_offset;
 ys := y shr y_shift_offset;
 bank:= 0;
 case num_banks of
  2:
   begin
    bank :=bank or ( ((xs shr 3) xor (ys shr 3))   and $1 )  shl  0;
   end;
  4:
   begin
    bank :=bank or ( ((xs shr 3) xor (ys shr 4))   and $1 )  shl  0;
    bank :=bank or ( ((xs shr 4) xor (ys shr 3))   and $1 )  shl  1;
   end;
  8:
   begin
    bank :=bank or ( ((xs shr 3) xor (ys shr 5))   and $1 )  shl  0;
    bank :=bank or ( ((xs shr 4) xor (ys shr 4) xor (ys shr 5)) and $1 )  shl  1;
    bank :=bank or ( ((xs shr 5) xor (ys shr 3))   and $1 )  shl  2;
   end;
  16:
   begin
    bank :=bank or ( ((xs shr 3) xor (ys shr 6))   and $1 )  shl  0;
    bank :=bank or ( ((xs shr 4) xor (ys shr 5)    xor (ys shr 6)) and $1 )  shl  1;
    bank :=bank or ( ((xs shr 5) xor (ys shr 4))   and $1 )  shl  2;
    bank :=bank or ( ((xs shr 6) xor (ys shr 3))   and $1 )  shl  3;
   end;
  else
   Assert(false,'invalid num_banks (%u) -- must be 2, 4, 8, or 16.');
 end;
 Result:=bank;
end;

procedure Tiler1d.init_surface(bytePerElement,isBlockCompressed,tile_idx,tile_alt:DWORD);
begin
 m_minGpuMode    :=tile_alt;
 m_tileMode      :=tile_idx;

 m_arrayMode     :=0;
 getArrayMode(@m_arrayMode,m_tileMode);

 m_bitsPerElement:=bytePerElement*8;
 m_bytePerElement:=bytePerElement;

 m_microTileMode :=0;
 getMicroTileMode(@m_microTileMode,m_tileMode);

 m_tileThickness :=getMicroTileThickness(m_arrayMode);

 m_tileBytes     := kMicroTileWidth * kMicroTileHeight * m_tileThickness * m_bytePerElement;

 m_isBlockCompressed := isBlockCompressed;

 m_element_table :=getElementTableXYZ(m_bitsPerElement,m_microTileMode,m_arrayMode);
end;

Function Get1dThinAlignWidth(bpp,width:Ptruint):Ptruint; inline;
var
 align_m:Ptruint;
begin
 align_m:=(32 div bpp)-1;
 Result:=(width+align_m) and (not align_m);
end;

procedure Tiler1d.init_size_2d(width,height:DWORD);
begin
 m_paddedWidth :=Get1dThinAlignWidth(m_bytePerElement,width);
 m_paddedHeight:=(height+7) and (not 7);
 m_paddedDepth :=1;

 m_linearWidth :=width;
 m_linearHeight:=height;
 m_linearDepth :=1;

 if (m_isBlockCompressed<>0) then
 begin
  m_paddedWidth :=(m_paddedWidth +3) shr 2;
  m_paddedHeight:=(m_paddedHeight+3) shr 2;
  //
  m_linearWidth :=(m_linearWidth +3) shr 2;
  m_linearHeight:=(m_linearHeight+3) shr 2;
 end;

 m_linearSizeBytes:=m_linearWidth*m_linearHeight*m_linearDepth*m_bytePerElement;
 m_tiledSizeBytes :=m_paddedWidth*m_paddedHeight*m_paddedDepth*m_bytePerElement;

 //tiler.m_tiledSizeBytes:=(tiler.m_tiledSizeBytes+255) and (not Ptruint(255));

 m_tilesPerRow    :=m_paddedWidth div kMicroTileWidth;
 m_tilesPerSlice  :=m_tilesPerRow * (m_paddedHeight div kMicroTileHeight);
end;

function Tiler1d.getTiledElementBitOffset(var outTiledBitOffset:QWORD;x,y,z:DWORD):integer;
var
 element_index:QWORD;
 slice_offset:QWORD;
 tile_row_index:QWORD;
 tile_column_index:QWORD;
 tile_offset:QWORD;
 element_offset:QWORD;
 final_offset:QWORD;
begin
 element_index := m_element_table^[z and 7,y and 7,x and 7];

 slice_offset := (z div m_tileThickness) * m_tilesPerSlice * m_tileBytes;

 tile_row_index    := y shr 3;// div kMicroTileHeight;
 tile_column_index := x shr 3;// div kMicroTileWidth;
 tile_offset       := ((tile_row_index * m_tilesPerRow) + tile_column_index) * m_tileBytes;

 element_offset    := element_index * m_bitsPerElement;

 final_offset      := (slice_offset + tile_offset)*8 + element_offset;

 outTiledBitOffset := final_offset;

 Result:=0;
end;

function Tiler1d.getTiledElementByteOffset(var outTiledByteOffset:QWORD;x,y,z:DWORD):integer;
var
 element_index:QWORD;
 slice_offset:QWORD;
 tile_row_index:QWORD;
 tile_column_index:QWORD;
 tile_offset:QWORD;
 element_offset:QWORD;
 final_offset:QWORD;
begin
 element_index := m_element_table^[z and 7,y and 7,x and 7];

 slice_offset := (z div m_tileThickness) * m_tilesPerSlice * m_tileBytes;

 tile_row_index    := y shr 3;// div kMicroTileHeight;
 tile_column_index := x shr 3;// div kMicroTileWidth;
 tile_offset       := ((tile_row_index * m_tilesPerRow) + tile_column_index) * m_tileBytes;

 element_offset    := element_index * m_bytePerElement;

 final_offset      := (slice_offset + tile_offset) + (element_offset);

 outTiledByteOffset:= final_offset;

 Result:=0;
end;

function max(a,b:DWORD):DWORD; inline;
begin
 if (a>b) then Result:=a else Result:=b;
end;

function min(a,b:DWORD):DWORD; inline;
begin
 if (a<b) then Result:=a else Result:=b;
end;

function getPipeCount(pipeConfig:Byte):DWORD;
begin
 case pipeConfig of
  kPipeConfigP8_32x32_8x16 :Result:=8;
  kPipeConfigP8_32x32_16x16:Result:=8;
  kPipeConfigP16           :Result:=16;
  else
                            Result:=0;
 end;
end;

function Tiler2d.init(var tp:TilingParameters):integer;
var
 bankWidthHW:DWORD;
 bankHeightHW:DWORD;
 macroAspectHW:DWORD;
 numBanksHW:DWORD;
 tileBytes1x:DWORD;
 sampleSplitHw:Byte;
 tileSplitHw:Byte;
 sampleSplit:DWORD;
 tileSplitC:DWORD;
begin
 if @tp=nil then Exit(-$7f2d0000);

 //// Use gpu_addr to come up with actual legal/padded surface parameters
 //SurfaceInfo surfInfoOut = {0};
 //int32_t status = computeSurfaceInfo(&surfInfoOut, tp);
 //SCE_GNM_ASSERT_MSG_RETURN(status == kStatusSuccess, status, "computeSurfaceInfo() failed: %d", status);

 // derived inputs
 m_minGpuMode:=tp.m_minGpuMode;
 m_tileMode  :=tp.m_tileMode;

 //m_arrayMode :=surfInfoOut.m_arrayMode;
 getArrayMode(@m_arrayMode,m_tileMode);

 getMicroTileMode(@m_microTileMode,m_tileMode);

 //// other constants
 m_tileThickness:=getMicroTileThickness(m_arrayMode);


 m_linearWidth   :=tp.m_linearWidth; // unpadded
 m_linearHeight  :=tp.m_linearHeight; // unpadded
 m_linearDepth   :=tp.m_linearDepth; // unpadded
 m_bitsPerElement:=tp.m_bitsPerFragment;
 //m_paddedWidth   :=surfInfoOut.m_pitch; // padded
 //m_paddedHeight  :=surfInfoOut.m_height;
 //m_paddedDepth   :=surfInfoOut.m_depth;
 m_numFragmentsPerPixel:=tp.m_numFragmentsPerPixel;

 if tp.m_isBlockCompressed then
 begin
  m_linearWidth :=max((m_linearWidth +3) shr 2, 1);
  m_linearHeight:=max((m_linearHeight+3) shr 2, 1);
  m_paddedWidth :=max((m_paddedWidth +3) shr 2, 1);
  m_paddedHeight:=max((m_paddedHeight+3) shr 2, 1);
  //
  m_bitsPerElement:=m_bitsPerElement*8;
 end;

 m_linearSizeBytes:=(m_linearWidth * m_linearHeight * m_linearDepth * m_bitsPerElement * m_numFragmentsPerPixel + 7) div 8;
 //m_tiledSizeBytes :=surfInfoOut.m_surfaceSize;

 if (tp.m_minGpuMode=kGpuModeNeo) then
 begin
  getAltPipeConfig(@m_pipeConfig, m_tileMode);
  getAllAltMacroTileData(m_tileMode, m_bitsPerElement, m_numFragmentsPerPixel, @bankWidthHW, @bankHeightHW, @macroAspectHW, @numBanksHW);
 end else
 begin
  getPipeConfig(@m_pipeConfig, m_tileMode);
  getAllMacroTileData(m_tileMode, m_bitsPerElement, m_numFragmentsPerPixel, @bankWidthHW, @bankHeightHW, @macroAspectHW, @numBanksHW);
 end;

 m_bankWidth      :=1 shl bankWidthHW;
 m_bankHeight     :=1 shl bankHeightHW;
 m_numBanks       :=2 shl numBanksHW;
 m_macroTileAspect:=1 shl macroAspectHW;

 tileBytes1x:=(m_tileThickness*m_bitsPerElement*kMicroTileWidth*kMicroTileHeight + 7) div 8;

 getSampleSplit(@sampleSplitHw, tp.m_tileMode);
 getTileSplit  (@tileSplitHw  , tp.m_tileMode);

 sampleSplit:=1 shl sampleSplitHw;

 if (m_microTileMode=kMicroTileModeDepth) then
  tileSplitC:=(64 shl tileSplitHw) // depth modes store tile split directly
 else
  tileSplitC:=max(256, tileBytes1x*sampleSplit); // other modes store a sample split multiplier

 m_tileSplitBytes:=min(kDramRowSize, tileSplitC);

 m_pipeInterleaveBytes:=kPipeInterleaveBytes;
 m_numPipes:=getPipeCount(m_pipeConfig);
 m_pipeInterleaveBits:=fastIntLog2(m_pipeInterleaveBytes);
 m_pipeInterleaveMask:=(1 shl m_pipeInterleaveBits) - 1;
 m_pipeBits:=fastIntLog2(m_numPipes);
 m_bankBits:=fastIntLog2(m_numBanks);
 m_pipeMask:=(m_numPipes-1) shl m_pipeInterleaveBits;
 m_bankSwizzleMask:=tp.m_tileSwizzleMask;
 m_pipeSwizzleMask:=0; // not currently used
 m_macroTileWidth :=(kMicroTileWidth  * m_bankWidth  * m_numPipes) *   m_macroTileAspect;
 m_macroTileHeight:=(kMicroTileHeight * m_bankHeight * m_numBanks) div m_macroTileAspect;

 m_arraySlice:=tp.m_arraySlice;

 Result:=0;
end;

function Tiler2d.getTiledElementBitOffset(var outTiledBitOffset:QWORD;x,y,z,fragmentIndex:DWORD):integer;
var
 element_index,xh,yh:DWORD;
 tile_bytes:DWORD;
 slice:DWORD;
 pipe,bank:QWORD;
 element_offset:QWORD;
 pixel_offset:QWORD;
 fragment_offset:QWORD;
 slices_per_tile:QWORD;
 tile_split_slice:QWORD;
 macro_tile_bytes:QWORD;
 macro_tiles_per_row:QWORD;
 macro_tile_row_index:QWORD;
 macro_tile_column_index:QWORD;
 macro_tile_index:QWORD;
 macro_tile_offset:QWORD;
 macro_tiles_per_slice:QWORD;
 slice_bytes:QWORD;
 slice_offset:QWORD;
 tile_row_index:QWORD;
 tile_column_index:QWORD;
 tile_index:QWORD;
 tile_offset:QWORD;
 bank_swizzle:QWORD;
 pipe_swizzle:QWORD;
 pipe_slice_rotation:QWORD;
 slice_rotation:QWORD;
 tile_split_slice_rotation:QWORD;
 total_offset:QWORD;
 bitOffset:QWORD;
 pipe_interleave_offset:QWORD;
 offset:QWORD;
 finalByteOffset:QWORD;
begin

 element_index:=getElementIndex(x, y, z, m_bitsPerElement, m_microTileMode, m_arrayMode);

 xh := x;
 yh := y;

 if (m_arrayMode=kArrayModeTiledThinPrt) or (m_arrayMode = kArrayModeTiledThickPrt) then
 begin
  xh := xh mod m_macroTileWidth;
  yh := yh mod m_macroTileHeight;
 end;

 pipe := getPipeIndex(xh, yh, m_pipeConfig);
 bank := getBankIndex(xh, yh, m_bankWidth, m_bankHeight, m_numBanks, m_numPipes);

 tile_bytes := (kMicroTileWidth * kMicroTileHeight * m_tileThickness * m_bitsPerElement * m_numFragmentsPerPixel + 7) div 8;

 element_offset:=0;

 if (m_microTileMode=kMicroTileModeDepth) then
 begin
  pixel_offset := element_index * m_bitsPerElement * m_numFragmentsPerPixel;
  element_offset := pixel_offset + (fragmentIndex * m_bitsPerElement);
 end else
 begin
  fragment_offset := fragmentIndex * (tile_bytes div m_numFragmentsPerPixel) * 8;
  element_offset := fragment_offset + (element_index * m_bitsPerElement);
 end;

 slices_per_tile := 1;
 tile_split_slice := 0;
 if (tile_bytes > m_tileSplitBytes) and (m_tileThickness = 1) then
 begin
  slices_per_tile := tile_bytes div m_tileSplitBytes;
  tile_split_slice := element_offset div (m_tileSplitBytes*8);
  element_offset:=element_offset mod (m_tileSplitBytes*8);
  tile_bytes := m_tileSplitBytes;
 end;

 macro_tile_bytes := (m_macroTileWidth div kMicroTileWidth) * (m_macroTileHeight div kMicroTileHeight) * tile_bytes div (m_numPipes * m_numBanks);
 macro_tiles_per_row := m_paddedWidth div m_macroTileWidth;
 macro_tile_row_index := y div m_macroTileHeight;
 macro_tile_column_index := x div m_macroTileWidth;
 macro_tile_index := (macro_tile_row_index * macro_tiles_per_row) + macro_tile_column_index;
 macro_tile_offset := macro_tile_index * macro_tile_bytes;
 macro_tiles_per_slice := macro_tiles_per_row * (m_paddedHeight div m_macroTileHeight);
 slice_bytes := macro_tiles_per_slice * macro_tile_bytes;

 slice := z;
 slice_offset := (tile_split_slice + slices_per_tile * slice div m_tileThickness) * slice_bytes;
 if (m_arraySlice<>0) then slice := m_arraySlice;

 tile_row_index := (y div kMicroTileHeight) mod m_bankHeight;
 tile_column_index := ((x div kMicroTileWidth) div m_numPipes) mod m_bankWidth;
 tile_index := (tile_row_index * m_bankWidth) + tile_column_index;
 tile_offset := tile_index * tile_bytes;

 bank_swizzle := m_bankSwizzleMask;
 pipe_swizzle := m_pipeSwizzleMask;

 pipe_slice_rotation:=0;
 case m_arrayMode of
  kArrayMode3dTiledThin,
  kArrayMode3dTiledThick,
  kArrayMode3dTiledXThick:
   begin
    pipe_slice_rotation := max(1, (m_numPipes div 2)-1) * (slice div m_tileThickness);
   end;
 end;

 pipe_swizzle:=pipe_swizzle+pipe_slice_rotation;
 pipe_swizzle:=pipe_swizzle and (m_numPipes - 1);
 pipe := pipe xor pipe_swizzle;

 slice_rotation:=0;

 case m_arrayMode of
  kArrayMode2dTiledThin,
  kArrayMode2dTiledThick,
  kArrayMode2dTiledXThick:
   begin
    slice_rotation := ((m_numBanks div 2)-1) * (slice div m_tileThickness);
   end;
  kArrayMode3dTiledThin,
  kArrayMode3dTiledThick,
  kArrayMode3dTiledXThick:
   begin
    slice_rotation := max(1, (m_numPipes div 2)-1) * (slice div m_tileThickness) div m_numPipes;
   end;

 end;

 tile_split_slice_rotation:= 0;
 case m_arrayMode of
  kArrayMode2dTiledThin,
  kArrayMode3dTiledThin,
  kArrayMode2dTiledThinPrt,
  kArrayMode3dTiledThinPrt:
   begin
    tile_split_slice_rotation := ((m_numBanks div 2)+1) * tile_split_slice;
   end;

 end;

 bank:=bank xor bank_swizzle + slice_rotation;
 bank:=bank xor tile_split_slice_rotation;
 bank:=bank and (m_numBanks - 1);

 total_offset := (slice_offset + macro_tile_offset + tile_offset)*8 + element_offset;
 bitOffset := total_offset and $7;
 total_offset:=total_offset div 8;

 pipe_interleave_offset := total_offset and m_pipeInterleaveMask;
 offset := total_offset shr m_pipeInterleaveBits;

 finalByteOffset := pipe_interleave_offset or
  (pipe   shl (m_pipeInterleaveBits)) or
  (bank   shl (m_pipeInterleaveBits + m_pipeBits)) or
  (offset shl (m_pipeInterleaveBits + m_pipeBits + m_bankBits));

 outTiledBitOffset := (finalByteOffset shl 3) or bitOffset;

 Result:=0;
end;

////////

procedure Tiler2d.getTiledElementByteOffset_2d_32(var outTiledByteOffset:QWORD;x,y,z:DWORD);
var
 element_index:DWORD;
 pipe,bank:QWORD;
 macro_tiles_per_row:QWORD;
 macro_tile_row_index:QWORD;
 macro_tile_column_index:QWORD;
 macro_tile_index:QWORD;
 macro_tiles_per_slice:QWORD;
 tile_row_index:QWORD;
 tile_column_index:QWORD;
 tile_index:QWORD;
 total_offset:QWORD;
 offset:QWORD;

 function getBankIndex16(x,y:DWORD):DWORD; inline;
 begin
  x := x shr 3;
  Result:= 0;
  Result:=Result or ( ((x shr 3) xor (y shr 6))  and $1 )  shl  0;
  Result:=Result or ( ((x shr 4) xor (y shr 5) xor (y shr 6)) and $1 )  shl  1;
  Result:=Result or ( ((x shr 5) xor (y shr 4))  and $1 )  shl  2;
  Result:=Result or ( ((x shr 6) xor (y shr 3))  and $1 )  shl  3;
 end;

begin
 element_index:=0;
 element_index:=element_index or ( (x shr 0) and $1 ) shl 0;
 element_index:=element_index or ( (x shr 1) and $1 ) shl 1;
 element_index:=element_index or ( (y shr 0) and $1 ) shl 2;
 element_index:=element_index or ( (x shr 2) and $1 ) shl 3;
 element_index:=element_index or ( (y shr 1) and $1 ) shl 4;
 element_index:=element_index or ( (y shr 2) and $1 ) shl 5;

 pipe:=0;
 pipe:=pipe or ( ((x shr 3) xor (y shr 3) xor (x shr 4)) and $1 )  shl  0;
 pipe:=pipe or ( ((x shr 4) xor (y shr 4))               and $1 )  shl  1;
 pipe:=pipe or ( ((x shr 5) xor (y shr 5))               and $1 )  shl  2;

 bank := getBankIndex16(x,y);

 macro_tiles_per_row     := m_paddedWidth div 128;
 macro_tile_row_index    := y div 64;
 macro_tile_column_index := x div 128;
 macro_tile_index        := (macro_tile_row_index * macro_tiles_per_row) + macro_tile_column_index;
 macro_tiles_per_slice   := macro_tiles_per_row * (m_paddedHeight div 64);

 tile_row_index    := (y div 8) mod 1;
 tile_column_index := ((x div 8) div 8) mod 1;
 tile_index        := tile_row_index + tile_column_index;

 bank:=(bank+7*z) and 15;

 total_offset:=((z*macro_tiles_per_slice)+macro_tile_index+tile_index)*256+(element_index*4);

 offset := total_offset shr 8;

 outTiledByteOffset := (total_offset and 255) or
  (pipe   shl (8)) or
  (bank   shl (11)) or
  (offset shl (15));
end;

{
function fastIntLog2(i:DWORD):DWORD; inline;
begin
 Result:=BsrDWord(i or 1);
end;

function getElementIndex(x,y:DWORD):DWORD;
var
elem:DWORD;
begin
elem:=0;

elem:=elem or ( (x shr 0) and $1 ) shl 0;
elem:=elem or ( (y shr 0) and $1 ) shl 2;

elem:=elem or ( (x shr 1) and $1 ) shl 1;
elem:=elem or ( (x shr 2) and $1 ) shl 3;
elem:=elem or ( (y shr 1) and $1 ) shl 4;
elem:=elem or ( (y shr 2) and $1 ) shl 5;

Result:=elem;
end;

function getPipeIndex(x,y:DWORD):DWORD;
var
pipe:DWORD;
begin
pipe:= 0;

pipe:=pipe or ( ((x shr 3) xor (y shr 3) xor (x shr 4)) and $1 )  shl  0;
pipe:=pipe or ( ((x shr 4) xor (y shr 4))               and $1 )  shl  1;
pipe:=pipe or ( ((x shr 5) xor (y shr 5))               and $1 )  shl  2;
pipe:=pipe or ( ((x shr 6) xor (y shr 5))               and $1 )  shl  3;

Result:=pipe;
end;

function getBankIndex(x,y:DWORD):DWORD;
var
xs,ys:DWORD;
bank:DWORD;
begin

xs := (x shr 4);
ys := (y shr 1);

bank:= 0;

   bank :=bank or ( ((xs shr 3) xor (ys shr 5))   and $1 )  shl  0;
   bank :=bank or ( ((xs shr 4) xor (ys shr 4) xor (ys shr 5)) and $1 )  shl  1;
   bank :=bank or ( ((xs shr 5) xor (ys shr 3))   and $1 )  shl  2;

Result:=bank;
end;

const
m_paddedWidth=1920;
m_paddedHeight=1152;

function getTiledElementBitOffset(var outTiledBitOffset:QWORD;x,y:DWORD):integer;
var
element_index,xh,yh:DWORD;
pipe,bank:QWORD;
//element_offset:QWORD;
//macro_tiles_per_row:QWORD;
//macro_tile_row_index:QWORD;
//macro_tile_column_index:QWORD;
//macro_tile_index:QWORD;
macro_tile_offset:QWORD;
//macro_tiles_per_slice:QWORD;
//slice_offset:QWORD;
tile_row_index:QWORD;
tile_column_index:QWORD;
tile_index:QWORD;
//tile_offset:QWORD;
total_offset:QWORD;
bitOffset:QWORD;
pipe_interleave_offset:QWORD;
offset:QWORD;
finalByteOffset:QWORD;
begin

element_index:=getElementIndex(x, y);

xh := x;
yh := y;

pipe := getPipeIndex(xh, yh);
bank := getBankIndex(xh, yh);

//element_offset := (element_index * 32);

//macro_tiles_per_row := (m_paddedWidth div 128);
//macro_tile_row_index    := (y div 128);
//macro_tile_column_index := (x div 128);
//macro_tile_index := ((y div 128) * (m_paddedWidth div 128)) + (x div 128);
macro_tile_offset := (((y div 128) * (m_paddedWidth div 128)) + (x div 128)) shl 9;
//macro_tiles_per_slice := (m_paddedWidth div 128) * (m_paddedHeight div 128);

tile_row_index := (y div 8) mod 2;
tile_column_index := (x div 128) mod 1;

tile_index := tile_row_index + tile_column_index;
//tile_offset := (tile_index * 256);

bank:=0;

total_offset := (((((y div 128) * (m_paddedWidth div 128)) + (x div 128)) shl 9) + (tile_index shl 8)) + (element_index shl 2);

//bitOffset := total_offset and $7;
//total_offset:=total_offset div 8;

pipe_interleave_offset := total_offset and 255;
offset := total_offset shr 8;

finalByteOffset := pipe_interleave_offset or
 (pipe   shl (8)) or
 (bank   shl (12)) or
 (offset shl (15));

outTiledBitOffset := (finalByteOffset shl 3) or bitOffset;

Result:=0;
end;
}

//xorl %r8d, %r8d 3
//xorl %edx, %edx 2
//xorl %ecx, %ecx 1

procedure detile32bppDisplaySse2(dst,src:Pointer;destPitch:DWORD); assembler; nostackframe; MS_ABI_CDecl;
asm
 //subq $40, %rsp               //unsafe
 //movaps %xmm6, (%rsp)
 //movaps %xmm7, 16(%rsp)
 movdqa 16(%rdx), %xmm5
 movdqa 32(%rdx), %xmm6
 movdqa 48(%rdx), %xmm4
 movdqa 64(%rdx), %xmm3
 movdqa 80(%rdx), %xmm1
 leal (%r8,%r8,2), %eax
 movdqa 96(%rdx), %xmm2
 leal 0(,%r8,8), %r9d
 sall $2, %eax
 movdqa 112(%rdx), %xmm0
 leal 0(,%r8,4), %r10d
 sall $4, %r8d
 movdqa (%rdx), %xmm7
 movups %xmm6, 16(%rcx)
 movups %xmm7, (%rcx)
 movups %xmm5, (%rcx,%r10)
 movups %xmm4, 16(%rcx,%r10)
 movups %xmm3, (%rcx,%r9)
 movups %xmm2, 16(%rcx,%r9)
 movups %xmm1, (%rcx,%rax)
 movups %xmm0, 16(%rcx,%rax)
 movdqa 128(%rdx), %xmm7
 addq %r8, %rcx
 movdqa 144(%rdx), %xmm5
 movdqa 160(%rdx), %xmm6
 movdqa 176(%rdx), %xmm4
 movdqa 192(%rdx), %xmm3
 movdqa 208(%rdx), %xmm1
 movdqa 224(%rdx), %xmm2
 movdqa 240(%rdx), %xmm0
 movups %xmm7, (%rcx)
 movups %xmm6, 16(%rcx)
 movups %xmm5, (%rcx,%r10)
 movups %xmm4, 16(%rcx,%r10)
 movups %xmm3, (%rcx,%r9)
 movups %xmm2, 16(%rcx,%r9)
 movups %xmm1, (%rcx,%rax)
 movups %xmm0, 16(%rcx,%rax)
 //movaps (%rsp), %xmm6
 //movaps 16(%rsp), %xmm7
 //addq $40, %rsp
end;

procedure detile32bppDisplayAvx(dst,src:Pointer;destPitch:DWORD); assembler; nostackframe; MS_ABI_CDecl;
asm
 vmovdqa 32(%rdx), %ymm2
 vmovdqa 64(%rdx), %ymm1
 vmovdqa 96(%rdx), %ymm0
 vmovaps (%rdx), %ymm3
 leal         (%r8,%r8,2), %eax
 leal         0(,%r8,8), %r9d
 sall         $2, %eax
 vmovups %xmm3, (%rcx)
 leal         0(,%r8,4), %r10d
 sall         $4, %r8d
 vextractf128 $0x1, %ymm3, (%rcx,%r10)
 vmovups %xmm2, 16(%rcx)
 vextractf128 $0x1, %ymm2, 16(%rcx,%r10)
 vmovups %xmm1, (%rcx,%r9)
 vextractf128 $0x1, %ymm1, (%rcx,%rax)
 vmovups %xmm0, 16(%rcx,%r9)
 vextractf128 $0x1, %ymm0, 16(%rcx,%rax)

 vmovdqa 160(%rdx), %ymm2
 addq         %r8, %rcx
 vmovdqa 192(%rdx), %ymm1
 vmovdqa 224(%rdx), %ymm0
 vmovaps 128(%rdx), %ymm3
 vmovups %xmm3, (%rcx)
 vextractf128 $0x1, %ymm3, (%rcx,%r10)
 vmovups %xmm2, 16(%rcx)
 vextractf128 $0x1, %ymm2, 16(%rcx,%r10)
 vmovups %xmm1, (%rcx,%r9)
 vextractf128 $0x1, %ymm1, (%rcx,%rax)
 vmovups %xmm0, 16(%rcx,%r9)
 vextractf128 $0x1, %ymm0, 16(%rcx,%rax)
 //vzeroupper
end;

//xorl %r8 , %r8  3 destPitch
//xorl %rdx, %rdx 2 src
//xorl %rcx, %rcx 1 dst

//[3] ymm0 = ymm4 [7]
//[2] ymm1 = ymm5 [6]
//[1] ymm2 = ymm6 [5]
//[0] ymm3 = ymm7 [4]

procedure detile32bppDisplayAvx_cached(dst,src:Pointer;destPitch:DWORD); assembler; nostackframe; MS_ABI_CDecl;
asm
 leal         (%r8,%r8,2), %eax
 leal         0(,%r8,8), %r9d
 sall         $2, %eax
 vmovups %xmm3, (%rcx)
 leal         0(,%r8,4), %r10d
 sall         $4, %r8d
 vextractf128 $0x1, %ymm3, (%rcx,%r10)
 vmovups %xmm2, 16(%rcx)
 vextractf128 $0x1, %ymm2, 16(%rcx,%r10)
 vmovups %xmm1, (%rcx,%r9)
 vextractf128 $0x1, %ymm1, (%rcx,%rax)
 vmovups %xmm0, 16(%rcx,%r9)
 vextractf128 $0x1, %ymm0, 16(%rcx,%rax)

 addq         %r8, %rcx
 vmovups %xmm7, (%rcx)
 vextractf128 $0x1, %ymm7, (%rcx,%r10)
 vmovups %xmm6, 16(%rcx)
 vextractf128 $0x1, %ymm6, 16(%rcx,%r10)
 vmovups %xmm5, (%rcx,%r9)
 vextractf128 $0x1, %ymm5, (%rcx,%rax)
 vmovups %xmm4, 16(%rcx,%r9)
 vextractf128 $0x1, %ymm4, 16(%rcx,%rax)
end;

procedure move64_sse(dst,src:Pointer); assembler; nostackframe; MS_ABI_CDecl;
asm
 movdqa  0(%rdx), %xmm0
 movdqa 16(%rdx), %xmm1
 movdqa 32(%rdx), %xmm2
 movdqa 48(%rdx), %xmm3
 movdqa    %xmm0,  0(%rcx)
 movdqa    %xmm1, 16(%rcx)
 movdqa    %xmm2, 32(%rcx)
 movdqa    %xmm3, 48(%rcx)
end;

procedure move64_avx(dst,src:Pointer); assembler; nostackframe; MS_ABI_CDecl;
asm
 vmovdqa  0(%rdx), %ymm0
 vmovdqa 32(%rdx), %ymm1
 vmovdqa    %ymm0,  0(%rcx)
 vmovdqa    %ymm1, 32(%rcx)
end;

//[3] ymm0 = ymm4 [7]  11  15
//[2] ymm1 = ymm5 [6]  10  14
//[1] ymm2 = ymm6 [5]  9   13
//[0] ymm3 = ymm7 [4]  8   12

//xorl %r8 , %r8  3 destPitch
//xorl %rdx, %rdx 2 src
//xorl %rcx, %rcx 1 dst

procedure move64_avx_cached(dst,src:Pointer;id:Byte);  MS_ABI_CDecl;
begin
 Case id of
  0:asm
     vmovdqa  0(%rdx), %ymm3
     vmovdqa 32(%rdx), %ymm2
    end;
  1:asm
     vmovdqa  0(%rdx), %ymm1
     vmovdqa 32(%rdx), %ymm0
    end;
  2:asm
     vmovdqa  0(%rdx), %ymm7
     vmovdqa 32(%rdx), %ymm6
    end;
  3:asm
     vmovdqa  0(%rdx), %ymm5
     vmovdqa 32(%rdx), %ymm4
    end;
  4:asm
     vmovdqa  0(%rdx), %ymm11
     vmovdqa 32(%rdx), %ymm10
    end;
  5:asm
     vmovdqa  0(%rdx), %ymm9
     vmovdqa 32(%rdx), %ymm8
    end;
  6:asm
     vmovdqa  0(%rdx), %ymm15
     vmovdqa 32(%rdx), %ymm14
    end;
  7..15:
  begin
   src:=src+(id-7)*64;
   asm
    vmovdqa  0(%rdx), %ymm13
    vmovdqa 32(%rdx), %ymm12
    vmovdqa    %ymm13,  0(%rcx)
    vmovdqa    %ymm12, 32(%rcx)
   end;
  end;
 end;
end;

//vmovaps 0(%rdx),  %ymm3
//vmovdqa 32(%rdx), %ymm2
//vmovdqa 64(%rdx), %ymm1
//vmovdqa 96(%rdx), %ymm0
//
//
//vmovaps 128(%rdx), %ymm3
//vmovdqa 160(%rdx), %ymm2
//vmovdqa 192(%rdx), %ymm1
//vmovdqa 224(%rdx), %ymm0


type
 TOffset=packed record
  x:Byte;
  y:Byte;
  //m_z:Byte;
  //m_w:Byte;
 end;

 TOffsets=packed record
  cl:Byte;
  off:array[0..15] of TOffset;
 end;

Const
 g_offsetOfCacheLine:array[0..2,0..4] of TOffsets=(
  ( // DISPLAY
      (cl: 1; off:((x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), //  8bpp
      (cl: 2; off:((x:0;y:0),(x:0;y:4),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), // 16bpp
      (cl: 4; off:((x:0;y:0),(x:0;y:2),(x:0;y:4),(x:0;y:6),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), // 32bpp
      (cl: 8; off:((x:0;y:0),(x:4;y:0),(x:0;y:2),(x:4;y:2),
                   (x:0;y:4),(x:4;y:4),(x:0;y:6),(x:4;y:6),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ),  // 64bpp

      (cl: 0; off:((x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) )

  ),
  ( // THIN
      (cl: 1; off:((x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), //  8bpp
      (cl: 2; off:((x:0;y:0),(x:0;y:4),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), //  16bpp
      (cl: 4; off:((x:0;y:0),(x:4;y:0),(x:0;y:4),(x:4;y:4),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), //  32bpp
      (cl: 8; off:((x:0;y:0),(x:0;y:2),(x:4;y:0),(x:4;y:2),
                   (x:0;y:4),(x:0;y:6),(x:4;y:4),(x:4;y:6),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), //  64bpp
      (cl:16; off:((x:0;y:0),(x:2;y:0),(x:0;y:2),(x:2;y:2),
                   (x:4;y:0),(x:6;y:0),(x:4;y:2),(x:6;y:2),
                   (x:0;y:4),(x:2;y:4),(x:0;y:6),(x:2;y:6),
                   (x:4;y:4),(x:6;y:4),(x:4;y:6),(x:6;y:6)) )  // 128bpp
  ),
  ( // DEPTH
      (cl: 1; off:((x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), //  8bpp
      (cl: 2; off:((x:0;y:0),(x:0;y:4),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), //  16bpp
      (cl: 4; off:((x:0;y:0),(x:4;y:0),(x:0;y:4),(x:4;y:4),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), //  32bpp
      (cl: 8; off:((x:0;y:0),(x:0;y:2),(x:4;y:0),(x:4;y:2),
                   (x:0;y:4),(x:0;y:6),(x:4;y:4),(x:4;y:6),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0),
                   (x:0;y:0),(x:0;y:0),(x:0;y:0),(x:0;y:0)) ), //  64bpp
      (cl:16; off:((x:0;y:0),(x:2;y:0),(x:0;y:2),(x:2;y:2),
                   (x:4;y:0),(x:6;y:0),(x:4;y:2),(x:6;y:2),
                   (x:0;y:4),(x:2;y:4),(x:0;y:6),(x:2;y:6),
                   (x:4;y:4),(x:6;y:4),(x:4;y:6),(x:6;y:6)) )  // 128bpp
  )
 );

procedure detile32bppBuf(var T:Tiler2d;src,dst:Pointer);
type
 PcontiguousCache=^TcontiguousCache;
 TcontiguousCache=array[0..15,0..63] of Byte;

var
 x,y,z,bytesPerElement,cacheLine:Ptrint;
 tiled_offset,linear_offset:Ptrint;
 offsetOfCacheLine:TOffsets;

 contiguous_data:packed record
  data:TcontiguousCache;
  align:array[0..{15}31] of Byte;
 end;

 contiguous:PcontiguousCache;

 cacheLineX:Ptrint;
 cacheLineY:Ptrint;
 cacheLineZ:Ptrint;

begin
 linear_offset:=0;
 bytesPerElement:=T.m_bitsPerElement div 8;
 offsetOfCacheLine:=g_offsetOfCacheLine[T.m_microTileMode][fastIntLog2(bytesPerElement)];

 //Writeln(HexStr(@contiguous_data));
 //Writeln(HexStr(Align(@contiguous_data,16)));

 contiguous:=Align(@contiguous_data,{16}32);
 //FillChar(contiguous^,SizeOf(TcontiguousCache),0);

 z:=0;
 While (z+T.m_tileThickness<=T.m_linearDepth) do
 begin
  y:=0;
  While (y+kMicroTileHeight<=T.m_linearHeight) do
  begin

   x:=0;
   While (x+kMicroTileWidth<=T.m_linearWidth) do
   begin

    For cacheLine:=0 to offsetOfCacheLine.cl-1 do  //4*64/32=8
    begin
     cacheLineX:=x + offsetOfCacheLine.off[cacheLine].x;
     cacheLineY:=y + offsetOfCacheLine.off[cacheLine].y;
     cacheLineZ:=z + 0;

     tiled_offset:=0;
     T.getTiledElementByteOffset_2d_32(qword(tiled_offset),cacheLineX,cacheLineY,cacheLineZ);

     move64_avx_cached(@contiguous^[0][0],(src+tiled_offset),cacheLine);
     //move64_avx(@contiguous^[cacheLine][0],(src + tiled_offset));
     //Move((src + tiled_offset)^,contiguous^[cacheLine][0], 64);
    end;

    //Writeln(HexStr(dst + linear_offset));
    //Writeln(HexStr(contiguous));
    //Writeln(T.m_linearWidth);

    linear_offset:=(x*bytesPerElement)+(y*bytesPerElement*T.m_linearWidth)+(z*bytesPerElement*T.m_linearWidth*T.m_linearHeight);

    detile32bppDisplayAvx_cached(dst + linear_offset,contiguous, T.m_linearWidth);
    //detile32bppDisplayAvx(dst + linear_offset, contiguous, T.m_linearWidth);
    //detile32bppDisplaySse2(dst + linear_offset, contiguous, T.m_linearWidth);

    //linear_offset:=linear_offset+bytesPerElement*kMicroTileWidth;
    x:=x+kMicroTileWidth;
   end;

   {if (x<T.m_linearWidth) then //slow
   begin
    For x:=x to T.m_linearWidth-1 do
    begin
     tiled_offset:=0;
     T.getTiledElementBitOffset(qword(tiled_offset),x,y,z,0);
     tiled_offset:=tiled_offset div 8;
     linear_offset:=(x*bytesPerElement)+(y*bytesPerElement*T.m_linearWidth)+(z*bytesPerElement*T.m_linearWidth*T.m_linearHeight);
     PDWORD(dst + linear_offset)^:=PDWORD(src+tiled_offset)^;
    end;
   end;}

   y:=y+kMicroTileHeight;
  end;

  if (y<T.m_linearHeight) then //slow
  begin
   For y:=y to T.m_linearHeight-1 do
   begin
    For x:=0 to T.m_linearWidth-1 do
    begin
     tiled_offset:=0;
     T.getTiledElementByteOffset_2d_32(qword(tiled_offset),x,y,z);
     linear_offset:=(x*bytesPerElement)+(y*bytesPerElement*T.m_linearWidth)+(z*bytesPerElement*T.m_linearWidth*T.m_linearHeight);
     PDWORD(dst + linear_offset)^:=PDWORD(src+tiled_offset)^;
    end;
   end;
  end;

  z:=z+T.m_tileThickness;
 end;

end;

//int32_t sce::GpuAddress::Tiler2d::detileSurfaceRegionOneFragment(
//void *outUntiledPixels,
//const void *inTiledPixels,
//const SurfaceRegion *srcRegion,
// uint32_t destPitch,
// uint32_t destSlicePitch,
// uint32_t fragment)
{
        const auto region = *srcRegion;

 const auto in_bytes = static_cast<const uint8_t*>(inTiledPixels);
    const auto out_bytes = static_cast<uint8_t*>(outUntiledPixels);
 const auto bytesPerElement = m_bitsPerElement / 8;

 if(m_microTileMode == Gnm::kMicroTileModeDepth && m_numFragmentsPerPixel > 1)
 {
  for(auto z = 0; z < depth(region); ++z)
   for(auto y = 0; y < height(region); ++y)
   {
    uint64_t linear_offset;
    computeLinearElementByteOffset(&linear_offset, 0, y, z, 0, destPitch, destSlicePitch, m_bitsPerElement, 1);
    for(auto x = 0; x < width(region); ++x)
    {
     uint64_t tiled_offset;
     getTiledElementByteOffset(&tiled_offset, region.m_left + x, region.m_top + y, region.m_front + z, fragment);
     small_memcpy(out_bytes + linear_offset, in_bytes + tiled_offset, bytesPerElement);
     linear_offset += bytesPerElement;
    }
   }
  return kStatusSuccess;
 }

    bool canTakeFastPath = true;
    if(m_microTileMode >= sizeof(g_offsetOfCacheLine)/sizeof(g_offsetOfCacheLine[0]))
        canTakeFastPath = false;
    if(canTakeFastPath)
    {
        Regions regions;
        regions.Init(region, m_tileThickness);
        if(hasTexels(regions.m_aligned))
        {
            const auto microTileFunc = getDetileFuncSse2(m_microTileMode, m_bitsPerElement);
            SCE_GNM_ASSERT_MSG_RETURN(nullptr != microTileFunc, kStatusInvalidArgument, "Can't find SSE2 detiling function for micro tilemode %d.", m_microTileMode);
            const auto offsetOfCacheLine = &g_offsetOfCacheLine[m_microTileMode][fastIntLog2(bytesPerElement)];
            const int dx = regions.m_aligned.m_left   - region.m_left;
            const int dy = regions.m_aligned.m_top    - region.m_top;
            const int dz = regions.m_aligned.m_front  - region.m_front;
         for(auto z = 0; z < depth(regions.m_aligned); z += m_tileThickness)
          for(auto y = 0; y < height(regions.m_aligned); y += kMicroTileHeight)
           for(auto x = 0; x < width(regions.m_aligned); x += kMicroTileWidth)
                    {
                        // Due to tile split, the cache lines of a microtile may be stored non-contiguously.
                        // But to use the optimized microtile detiler, all cache lines of a microtile must be stored contiguously.
                        // Therefore, here we gather all the cache lines together into a temporary buffer before proceeding...
                        uint8_t contiguous[16][64];
                        for(auto cacheLine = 0U; cacheLine < offsetOfCacheLine->m_cacheLinesPerFragment; ++cacheLine)
                        {
                            const auto cacheLineX = regions.m_aligned.m_left  + x + offsetOfCacheLine->m_offset[cacheLine].m_x;
                            const auto cacheLineY = regions.m_aligned.m_top   + y + offsetOfCacheLine->m_offset[cacheLine].m_y;
                            const auto cacheLineZ = regions.m_aligned.m_front + z + offsetOfCacheLine->m_offset[cacheLine].m_z;
             uint64_t tiled_offset;
             getTiledElementByteOffset(&tiled_offset, cacheLineX, cacheLineY, cacheLineZ, fragment);
                            memcpy(contiguous[cacheLine], in_bytes + tiled_offset, 64);
                        }
                        // Now that we have one contiguous microtile, we can pass it to the optimized microtile detiler...
               uint64_t linear_offset;
            computeLinearElementByteOffset(&linear_offset, dx + x, dy + y, dz + z, 0, destPitch, destSlicePitch, m_bitsPerElement, 1);
                        microTileFunc(out_bytes + linear_offset, contiguous, destPitch, destSlicePitch);
           }
            for(auto i = 0; i < regions.m_unaligneds; ++i)
                slowDetileOneFragment<Tiler2d>(this, region, regions.m_unaligned[i], fragment, destPitch, destSlicePitch, out_bytes, in_bytes, bytesPerElement);
            return kStatusSuccess;
        }
    }
    slowDetileOneFragment<Tiler2d>(this, region, region, fragment, destPitch, destSlicePitch, out_bytes, in_bytes, bytesPerElement);
    return kStatusSuccess;
}

initialization
 init_element_index_table;

end.

