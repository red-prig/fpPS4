
uses
 bittype,
 sysutils;

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

const
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
      //
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
      //
     end;

   kMicroTileModeThick:
     begin
      //
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

type
 t_bits_per_element=(b8,b16,b32,b64,b128);

 t_micro_tile_modes =Set of 0..4;
 t_bits_per_elements=Set of t_bits_per_element;
 t_array_modes      =Set of 0..$f;

var
 g_microTileMode :t_micro_tile_modes;
 g_bitsPerElement:t_bits_per_elements;
 g_arrayMode     :t_array_modes;

procedure set_mtm(microTileMode:t_micro_tile_modes);
begin
 g_microTileMode:=microTileMode;
end;

procedure set_bpe(bitsPerElement:t_bits_per_elements);
begin
 g_bitsPerElement:=bitsPerElement;
end;

procedure set_arm(arrayMode:t_array_modes);
begin
 g_arrayMode:=arrayMode;
end;

type
 t_axis=(m__,m_x,m_y,m_z,m_i);

const
 axis_str:array[t_axis] of Char=('_','x','y','z','i');
 bits_str:array[t_bits_per_element] of String=('8','16','32','64','128');
 mtm_str :array[0..4] of String=('Display','Thin','Depth','Rotated','Thick');

type
 t_biti=record
  xyz:t_axis;
  bit:Byte; //0..2
 end;

 t_bits=object
  num:Byte;
  bit:array[0..8] of t_biti;
  procedure set_elm(xyz:t_axis;m_shr,m_and,m_shl:Byte);
 end;

var
 g_bits:t_bits;

procedure t_bits.set_elm(xyz:t_axis;m_shr,m_and,m_shl:Byte);
begin
 Assert(m_and=1);

 if (num<m_shl) then num:=m_shl;

 bit[m_shl].xyz:=xyz;
 bit[m_shl].bit:=m_shr;
end;

//elem:=elem or ( (x shr 0) and $1 ) shl 0;
//                       0..2            0..8
procedure set_elm(xyz:t_axis;m_shr,m_and,m_shl:Byte);
begin
 g_bits.set_elm(xyz,m_shr,m_and,m_shl);
end;

function Is_1d_Thin(tiling:Byte):Boolean;
var
 ArrayMode:Byte;
 ra:Integer;
begin
 ra:=getArrayMode(@ArrayMode,tiling);
 Result:=(ra=0) and (ArrayMode=kArrayMode1dTiledThin);
end;

type
 t_tilings=Set of 0..$1F;

function get_tilings():t_tilings;
var
 i:Byte;
 ra,rm:Integer;

 ArrayMode    :Byte;
 MicroTileMode:Byte;

begin
 Result:=[];

 For i:=0 to High(t_tilings) do
 begin
  ra:=getArrayMode    (@ArrayMode    ,i);
  rm:=getMicroTileMode(@MicroTileMode,i);

  if (ra=0) and (rm=0) then
  if (g_arrayMode=[])     or (ArrayMode     in g_arrayMode    ) then
  //if (g_microTileMode=[]) or (MicroTileMode in g_microTileMode) then
  begin
   Result:=Result+[i];
  end;

 end;

end;

function filter_1d_Thin(TS:t_tilings):t_tilings;
var
 i:Byte;
begin
 Result:=TS;
 For i:=0 to High(t_tilings) do
 if not Is_1d_Thin(i) then
 begin
  Result:=Result-[i];
 end;
end;

procedure mark_end;
var
 i,g:Byte;
 A,B,M,T:RawByteString;
 TS:t_tilings;

 AXISS:array[t_axis] of t_bits;
begin
 M:='';
 For i:=0 to High(t_micro_tile_modes) do
 if i in g_microTileMode then
 begin
  if M<>'' then M:=M+',';
  M:=M+mtm_str[i];
 end;

 B:='';
 For i:=ord(Low(t_bits_per_elements)) to ord(High(t_bits_per_elements)) do
 if t_bits_per_element(i) in g_bitsPerElement then
 begin
  if B<>'' then B:=B+',';
  B:=B+bits_str[t_bits_per_element(i)];
 end;

 A:='';
 For i:=0 to High(t_array_modes) do
 if i in g_arrayMode then
 begin
  if A<>'' then A:=A+',';
  A:=A+IntToStr(i);
 end;

 TS:=filter_1d_Thin(get_tilings());

 if (TS=[]) then Exit;

 T:='';
 For i:=0 to High(t_tilings) do
 if i in TS then
 begin
  if T<>'' then T:=T+',';
  T:=T+IntToStr(i);
 end;

 Writeln('M:[',M,'] B:[',B,'] A:[',A,'] Tilings:',T);

 Write('i=');
 For i:=0 to g_bits.num do
 begin
  Write('[',axis_str[g_bits.bit[i].xyz],':',g_bits.bit[i].bit,']');
 end;
 Writeln;

 FillChar(AXISS,sizeof(AXISS),0);

 For i:=0 to g_bits.num do
 begin
  AXISS[g_bits.bit[i].xyz].set_elm(m_i,g_bits.bit[i].bit,1,i);
 end;


 For g:=ord(m_x) to ord(m_z) do
 begin
  A:='';

  For i:=0 to g_bits.num do
  begin
   //Write('[',axis_str[AXISS[t_axis(g)].bit[i].xyz],':',AXISS[t_axis(g)].bit[i].bit,']');

   if AXISS[t_axis(g)].bit[i].xyz=m_i then
   begin
    if (A<>'') then A:=A+' or ';

    A:=A+'(((i shr '+IntToStr(i)+') and 1) shl '+IntToStr(AXISS[t_axis(g)].bit[i].bit)+')';
   end;

  end;

  Writeln(axis_str[t_axis(g)],':=',A,';');
 end;

 //elem:=elem or ( (x shr 0) and $1 ) shl 0;

 Writeln('---');
 //
end;

procedure _getAxis_1d_thin(i,bitsPerElement:Byte;var x,y:Byte);
begin
 case bitsPerElement of
  8:
   begin
    x:=(((i shr 0) and 1) shl 0) or (((i shr 1) and 1) shl 1) or (((i shr 2) and 1) shl 2);
    y:=(((i shr 3) and 1) shl 1) or (((i shr 4) and 1) shl 0) or (((i shr 5) and 1) shl 2);
   end;

  16:
   begin
    x:=(((i shr 0) and 1) shl 0) or (((i shr 1) and 1) shl 1) or (((i shr 2) and 1) shl 2);
    y:=(((i shr 3) and 1) shl 0) or (((i shr 4) and 1) shl 1) or (((i shr 5) and 1) shl 2);
   end;

  32:
   begin
    x:=(((i shr 0) and 1) shl 0) or (((i shr 1) and 1) shl 1) or (((i shr 3) and 1) shl 2);
    y:=(((i shr 2) and 1) shl 0) or (((i shr 4) and 1) shl 1) or (((i shr 5) and 1) shl 2);
   end;

  64:
   begin
    x:=(((i shr 0) and 1) shl 0) or (((i shr 2) and 1) shl 1) or (((i shr 3) and 1) shl 2);
    y:=(((i shr 1) and 1) shl 0) or (((i shr 4) and 1) shl 1) or (((i shr 5) and 1) shl 2);
   end;

  else;
 end;
end;

function fastIntLog2(i:DWORD):DWORD; inline;
begin
 Result:=BsrDWord(i or 1);
end;

type
 tbit_interval=record
  pos_i:Word;
  srt_x:Byte;
  end_x:Byte;
      y:Byte;
  bitcn:Byte;
 end;

 tbit_interval_array=object
  num_i:Byte;
  pos_i:Word;
  intervals:array[0..63] of tbit_interval;
  Procedure Add(srt_x,end_x,y,bitcn:Byte);
  procedure Sort_xy;
 end;

Procedure tbit_interval_array.Add(srt_x,end_x,y,bitcn:Byte);
begin
 intervals[num_i].pos_i:=pos_i;
 intervals[num_i].srt_x:=srt_x;
 intervals[num_i].end_x:=end_x;
 intervals[num_i].    y:=    y;
 intervals[num_i].bitcn:=bitcn;
 //
 pos_i:=pos_i+bitcn;
 num_i:=num_i+1;
end;

Procedure tbit_interval_array.Sort_xy;
var
 i,k:Byte;
 val1,val2:Word;
 tmp:tbit_interval;
begin
 For k:=0 to num_i-1 do
 For i:=0 to num_i-2 do
 begin
  with intervals[i+0] do
   val1:=srt_x+y*64;

  with intervals[i+1] do
   val2:=srt_x+y*64;

  if (val1>val2) then
  begin
   tmp:=intervals[i+1];
   intervals[i+1]:=intervals[i+0];
   intervals[i+0]:=tmp;
  end;

 end;
end;

var
 g_axis_intervals:array[b8..b64] of tbit_interval_array;

Procedure Iterate_Axiss;
var
 x,y:Byte;
 b:Byte;
 bit:Byte;
 i:Byte;
 out_x,out_y:Byte;
 prv_x,prv_y:Byte;
 str_x:Byte;
begin

 For b:=ord(b8) to ord(b64) do
 begin
  bit:=(1 shl b) shl 3;
  Writeln('[',bit,']:all=',(bit div 8)*64,'bytes');
  //For i:=0 to (1 shl 6)-1 do
  For y:=0 to 7 do
  begin
   For x:=0 to 7 do
   begin
    i:=x+y*8;
    _getAxis_1d_thin(i,bit,out_x,out_y);

    //Write(out_x,',',out_y,' ');

    if (x=0) then
    begin
     //start
     str_x:=out_x;
     prv_x:=out_x;
     prv_y:=out_y;
    end else
    begin

     if (prv_x+1=out_x) and
        (prv_y  =out_y) then
     begin
      //
     end else
     begin
      g_axis_intervals[t_bits_per_element(b)].Add(str_x,prv_x,prv_y,(bit)*(prv_x-str_x+1));

      Write(str_x,'..',prv_x,':',prv_y,':',(bit)*(prv_x-str_x+1),'bit ');

      //reset
      str_x:=out_x;
     end;

     prv_x:=out_x;
     prv_y:=out_y;
    end;

   end;

   g_axis_intervals[t_bits_per_element(b)].Add(str_x,prv_x,prv_y,(bit)*(prv_x-str_x+1));

   Write(str_x,'..',prv_x,':',prv_y,':',(bit)*(prv_x-str_x+1),'bit ');

   Writeln;
  end;
 end;


end;

//leal           2(%rdx,%rdx,2), %eax
//vmovups %xmm3, (%rcx)
//vextractf128 $1, %ymm2, 16(%rcx,%r9)

//rdi(dst),rsi(src),rdx(pitch)

type
 t_lea_used_y=object
  flags:Byte;
  a_lea:array[0..3] of RawByteString;
  a_ofs:array[0..7] of RawByteString;
  procedure _set(y:Byte);
  procedure _build_str(const reg_dst:RawByteString);
 end;

procedure t_lea_used_y._set(y:Byte);
begin
 Assert(y<8);
 flags:=flags or (1 shl y)
end;

procedure t_lea_used_y._build_str(const reg_dst:RawByteString);
const
 r_pitch='%rdx';
 rtmp:array[0..2] of pchar=(
  '%r8 ',
  '%r9 ',
  '%r10'
 );
begin
 //
 if ((flags and (1 shl 2))<>0) or ((flags and (1 shl 3))<>0) then
 begin
  a_lea[0]:='('+reg_dst+','+r_pitch+',2), '+rtmp[0]+' //+2'; //+2 +3
 end;
 if ((flags and (1 shl 4))<>0) or ((flags and (1 shl 5))<>0) then
 begin
  a_lea[1]:='('+reg_dst+','+r_pitch+',4), '+rtmp[1]+' //+4'; //+4 +5
 end;
 if ((flags and (1 shl 6))<>0) or ((flags and (1 shl 7))<>0) then
 begin
  a_lea[2]:='('+r_pitch+','+r_pitch+',2), '+rtmp[2]+' //+3'; //+3
  a_lea[3]:='('+reg_dst+','+rtmp[2]+',2), '+rtmp[2]+' //+6'; //+6 +7
 end;
 //
 a_ofs[0]:='('+reg_dst+')'            ;  //     +0
 a_ofs[1]:='('+reg_dst+','+r_pitch+')';  //     +1
 a_ofs[2]:='('+rtmp[0]+')'            ;  //     +2
 a_ofs[3]:='('+rtmp[0]+','+r_pitch+')';  //+2+1 +3
 a_ofs[4]:='('+rtmp[1]+')'            ;  //     +4
 a_ofs[5]:='('+rtmp[1]+','+r_pitch+')';  //+4+1 +5
 a_ofs[6]:='('+rtmp[2]+')'            ;  //     +6
 a_ofs[7]:='('+rtmp[2]+','+r_pitch+')';  //+6+1 +7
end;

procedure on_gen_1dThin_detile(const interval:tbit_interval;
                               bytes    :Byte;
                               reg_count:Byte;
                               var flags:QWORD;
                               var lea_used_y:t_lea_used_y;
                               mode:Byte
                              );

var
 bytes_pos:Word;
 reg_num:Byte;
 reg_mod:Byte;
 reg_ext:Byte;
 dlt_x:Integer;
 dlt_x_str:RawByteString;
 dlt_y_str:RawByteString;
begin
 Assert((interval.pos_i mod 8)=0);

 bytes_pos:=interval.pos_i div 8;

 reg_num:=bytes_pos div 32;
 reg_mod:=bytes_pos mod 32;

 dlt_x:=(interval.srt_x)*bytes;

 dlt_x_str:='';
 if (dlt_x<>0) then
 begin
  dlt_x_str:=IntToStr(dlt_x);
 end;

 dlt_y_str:=lea_used_y.a_ofs[interval.y];

 case interval.bitcn of
   64:
    begin

     case reg_mod of
       0:
        begin
         if mode=1 then
          Writeln('vmovq ':18,('%xmm'+IntToStr(reg_num)):6,', ',dlt_x_str:2,dlt_y_str);
        end;
       8:
        begin
         if mode=2 then
          Writeln('vpextrq $1, ':18,('%xmm'+IntToStr(reg_num)):6,', ',dlt_x_str:2,dlt_y_str);
        end;
      16:
        begin
         reg_ext:=reg_count+reg_num;

         if (flags and (1 shl reg_ext))=0 then
         begin
          Writeln('vextractf128 $1, ':18,('%ymm'+IntToStr(reg_num)):6,', %xmm',reg_ext);
          flags:=flags or (1 shl reg_ext);
         end;

         if mode=1 then
          Writeln('vmovq ':18,('%xmm'+IntToStr(reg_ext)):6,', ',dlt_x_str:2,dlt_y_str);
        end;
      24:
        begin
         reg_ext:=reg_count+reg_num;

         if (flags and (1 shl reg_ext))=0 then
         begin
          Writeln('vextractf128 $1, ':18,('%ymm'+IntToStr(reg_num)):6,', %xmm',reg_ext);
          flags:=flags or (1 shl reg_ext);
         end;

         if mode=2 then
          Writeln('vpextrq $1, ':18,('%xmm'+IntToStr(reg_ext)):6,', ',dlt_x_str:2,dlt_y_str);
        end;
      else
        Assert(False);
     end;

    end;
  128:
    begin

     case reg_mod of
      0:
       begin
        if mode=1 then
         Writeln('vmovups ':18,('%xmm'+IntToStr(reg_num)):6,', ',dlt_x_str:2,dlt_y_str);
       end;
     16:
       begin
        if mode=2 then
         Writeln('vextractf128 $1, ':18,('%ymm'+IntToStr(reg_num)):6,', ',dlt_x_str:2,dlt_y_str);
       end;
     else
       Assert(False);
     end;

    end;
  else
    Assert(False);
 end;


end;

procedure on_gen_1dThin_tiling(const interval:tbit_interval;
                               bytes    :Byte;
                               reg_count:Byte;
                               var flags:QWORD;
                               var lea_used_y:t_lea_used_y;
                               mode:Byte
                              );

var
 bytes_pos:Word;
 reg_num:Byte;
 reg_mod:Byte;
 reg_ext:Byte;
 dlt_x:Integer;
 dlt_x_str:RawByteString;
 dlt_y_str:RawByteString;
begin
 Assert((interval.pos_i mod 8)=0);

 bytes_pos:=interval.pos_i div 8;

 reg_num:=bytes_pos div 32;
 reg_mod:=bytes_pos mod 32;

 dlt_x:=(interval.srt_x)*bytes;

 dlt_x_str:='';
 if (dlt_x<>0) then
 begin
  dlt_x_str:=IntToStr(dlt_x);
 end;

 dlt_y_str:=lea_used_y.a_ofs[interval.y];

 case interval.bitcn of
   64:
    begin

     case reg_mod of
       0:
        begin

         if (flags and (1 shl reg_num))=0 then
         begin
          //only if first
          Writeln('vmovq ':18,dlt_x_str:2,dlt_y_str,', ','%xmm',reg_num); //
         end else
         begin
          //ignore
          //Writeln('vpinsrq $0, ':18,dlt_x_str:2,dlt_y_str,', ','%xmm',reg_num,', ','%xmm',reg_num); //
         end;

         flags:=flags or (1 shl reg_num);
        end;
       8:
        begin
         if mode=1 then
          Writeln('vpinsrq $1, ':18,dlt_x_str:2,dlt_y_str,', ','%xmm',reg_num,', ','%xmm',reg_num); //

         flags:=flags or (1 shl reg_num);
        end;
      16:
        begin
         reg_ext:=reg_count+reg_num;

         if (flags and (1 shl reg_ext))=0 then
         begin
          //only if first
          Writeln('vmovq ':18,dlt_x_str:2,dlt_y_str,', ','%xmm',reg_ext); //
         end else
         begin
          //ignore
          //Writeln('vpinsrq $0, ':18,dlt_x_str:2,dlt_y_str,', ','%xmm',reg_ext,', ','%xmm',reg_ext); //
         end;

         flags:=flags or (1 shl reg_ext);
        end;
      24:
        begin
         reg_ext:=reg_count+reg_num;

         if mode=1 then
          Writeln('vpinsrq $1, ':18,dlt_x_str:2,dlt_y_str,', ','%xmm',reg_ext,', ','%xmm',reg_ext); //

         flags:=flags or (1 shl reg_ext);
        end;
      else
        Assert(False);
     end;

    end;
  128:
    begin

     case reg_mod of
      0:
       begin

        if (flags and (1 shl reg_num))=0 then
        begin
         //only if first
         Writeln('vmovups ':18,dlt_x_str:2,dlt_y_str,', ','%xmm',reg_num); //
        end else
        begin
         //ignore
         //Writeln('vinsertf128 $0, ':18,dlt_x_str:2,dlt_y_str,', ','%ymm',reg_num,', ','%ymm',reg_num); //
        end;

        flags:=flags or (1 shl reg_num);
       end;
     16:
       begin
        if mode=1 then
         Writeln('vinsertf128 $1, ':18,dlt_x_str:2,dlt_y_str,', ','%ymm',reg_num,', ','%ymm',reg_num); //

        flags:=flags or (1 shl reg_num);
       end;
     else
       Assert(False);
     end;

    end;
  else
    Assert(False);
 end;

end;

Procedure generate_1dThin_asm;
const
 reg_dst  ='%rdi';
 reg_src  ='%rsi';
 reg_pitch='%rdx';

var
 b,bits:Byte;
 bytes:Byte;
 all:Word;
 i,r,reg_count:Byte;
 bytes_pos:Word;
 reg_num:Byte;
 reg_mod:Byte;
 reg_ext:Byte;
 dlt_x  :Integer;
 dlt_y  :Integer;
 flags  :QWORD;
 dlt_x_str:RawByteString;

 lea_used_y:t_lea_used_y;
begin
 //sort all
 For b:=ord(b8) to ord(b64) do
 begin
  with g_axis_intervals[t_bits_per_element(b)] do
  begin
   Sort_xy;
  end;
 end;
 //sort all

 For b:=ord(b8) to ord(b64) do
 begin
  bytes:=(1 shl b);
  bits:=bytes shl 3;
  all:=bytes*64;
  Writeln('//[',bits,']:all=',all,'bytes');

  //rdi(dst),rsi(src),rdx(dst_pitch)

  Writeln('procedure detile_1dThin_',bits,'(dst,src:Pointer;pitch:QWORD); assembler; nostackframe; SysV_ABI_CDecl;');
  Writeln('asm');

  //build lea
  lea_used_y:=Default(t_lea_used_y);
  with g_axis_intervals[t_bits_per_element(b)] do
   For i:=0 to num_i-1 do
   begin
    lea_used_y._set(intervals[i].y);
   end;
  //build lea
  lea_used_y._build_str(reg_dst);

  //print lea
  For i:=0 to High(lea_used_y.a_lea) do
  if (lea_used_y.a_lea[i]<>'') then
  begin
   Writeln('lea ':18,lea_used_y.a_lea[i]);
  end;
  //print lea

  //load to regs
  reg_count:=all div 32;
  for r:=0 to reg_count-1 do
  begin
   Writeln('vmovups ':18,r*32:3,'(',reg_src,')',', ',('%ymm'+IntToStr(r)):0);
  end;
  //load to regs

  with g_axis_intervals[t_bits_per_element(b)] do
  begin

   flags:=0;

   For i:=0 to num_i-1 do
   begin
    on_gen_1dThin_detile(intervals[i],
                         bytes,
                         reg_count,
                         flags,
                         lea_used_y,
                         0
                        );

    //Writeln(intervals[i].pos_i div 8,'->',intervals[i].srt_x,'..',intervals[i].end_x,':',intervals[i].y,':',intervals[i].bitcn,'bit ');

   end;

   For i:=0 to num_i-1 do
   begin
    on_gen_1dThin_detile(intervals[i],
                         bytes,
                         reg_count,
                         flags,
                         lea_used_y,
                         1
                        );
   end;

   For i:=0 to num_i-1 do
   begin
    on_gen_1dThin_detile(intervals[i],
                         bytes,
                         reg_count,
                         flags,
                         lea_used_y,
                         2
                        );
   end;


   //
  end; //with

  Writeln('end;');
  //<-detiling

  //tiling->

  //rdi(src),rsi(dst),rdx(dst_pitch)

  Writeln('procedure tile_1dThin_',bits,'(dst,src:Pointer;pitch:QWORD); assembler; nostackframe; SysV_ABI_CDecl;');
  Writeln('asm');

  //build lea
  lea_used_y:=Default(t_lea_used_y);
  with g_axis_intervals[t_bits_per_element(b)] do
   For i:=0 to num_i-1 do
   begin
    lea_used_y._set(intervals[i].y);
   end;
  //build lea
  lea_used_y._build_str(reg_src);

  //print lea
  For i:=0 to High(lea_used_y.a_lea) do
  if (lea_used_y.a_lea[i]<>'') then
  begin
   Writeln('lea ':18,lea_used_y.a_lea[i]);
  end;
  //print lea

  with g_axis_intervals[t_bits_per_element(b)] do
  begin

   flags:=0;

   For i:=0 to num_i-1 do
   begin
    on_gen_1dThin_tiling(intervals[i],
                         bytes,
                         reg_count,
                         flags,
                         lea_used_y,
                         0
                        );
   end;

   For i:=0 to num_i-1 do
   begin
    on_gen_1dThin_tiling(intervals[i],
                         bytes,
                         reg_count,
                         flags,
                         lea_used_y,
                         1
                        );
   end;

   //combine
   if (flags<>0) then
   for r:=reg_count to (reg_count*2)-1 do
    if (flags and (1 shl r))<>0 then
    begin
     Writeln('vinsertf128 $1, ':18,'%xmm',r,', ','%ymm',r-reg_count,', ','%ymm',r-reg_count); //
    end;
   //combine

  end; //with

  //write to mem
  reg_count:=all div 32;
  for r:=0 to reg_count-1 do
  begin
   Writeln('vmovups ':18,('%ymm'+IntToStr(r)):6,', ',r*32:3,'(',reg_dst,')');
  end;
  //write to mem


  Writeln('end;');
  //<-tiling

  Writeln;
 end; //For b

end;

var
 g_bits_pos  :Byte=0;
 g_bits_stack:array[0..5] of t_bits;

procedure push_bits;
begin
 g_bits_stack[g_bits_pos]:=g_bits;
 g_bits_pos:=g_bits_pos+1;
end;

procedure pop_bits;
begin
 g_bits_pos:=g_bits_pos-1;
 g_bits:=g_bits_stack[g_bits_pos];
end;

begin

 set_arm([]);
 set_mtm([kMicroTileModeDisplay]);
 set_bpe([b8]);
  begin
   push_bits;

    set_elm(m_x,0,$1,0);
    set_elm(m_x,1,$1,1);
    set_elm(m_x,2,$1,2);
    set_elm(m_y,1,$1,3);
    set_elm(m_y,0,$1,4);
    set_elm(m_y,2,$1,5);

   mark_end;
   pop_bits;
  end;
 set_bpe([b16]);
  begin
   push_bits;

    set_elm(m_x,0,$1,0);
    set_elm(m_x,1,$1,1);
    set_elm(m_x,2,$1,2);
    set_elm(m_y,0,$1,3);
    set_elm(m_y,1,$1,4);
    set_elm(m_y,2,$1,5);

   mark_end;
   pop_bits;
  end;
 set_bpe([b32]);
  begin
   push_bits;

    set_elm(m_x,0,$1,0);
    set_elm(m_x,1,$1,1);
    set_elm(m_y,0,$1,2);
    set_elm(m_x,2,$1,3);
    set_elm(m_y,1,$1,4);
    set_elm(m_y,2,$1,5);

   mark_end;
   pop_bits;
  end;
 set_bpe([b64]);
  begin
   push_bits;

    set_elm(m_x,0,$1,0);
    set_elm(m_y,0,$1,1);
    set_elm(m_x,1,$1,2);
    set_elm(m_x,2,$1,3);
    set_elm(m_y,1,$1,4);
    set_elm(m_y,2,$1,5);

   mark_end;
   pop_bits;
  end;

  set_arm([]);
  set_bpe([]);

  set_mtm([kMicroTileModeThin,kMicroTileModeDepth]);
    begin
     push_bits;

      set_elm(m_x,0,$1,0);
      set_elm(m_y,0,$1,1);
      set_elm(m_x,1,$1,2);
      set_elm(m_y,1,$1,3);
      set_elm(m_x,2,$1,4);
      set_elm(m_y,2,$1,5);
      //
      set_arm([kArrayMode2dTiledXThick,kArrayMode3dTiledXThick]);
      begin
       push_bits;

        set_elm(m_z,2,$1,8);

       mark_end;
       pop_bits;
      end;
      //

      set_arm([
       kArrayMode1dTiledThick,
       kArrayMode2dTiledThick,
       kArrayMode3dTiledThick,
       kArrayModeTiledThickPrt,
       kArrayMode2dTiledThickPrt,
       kArrayMode3dTiledThickPrt
      ]);

      begin
       push_bits;

        set_elm(m_z,0,$1,6);
        set_elm(m_z,1,$1,7);

       mark_end;
       pop_bits;
      end;

     //
     pop_bits;
    end;

  set_arm([]);
  set_bpe([]);

  set_mtm([kMicroTileModeThick]);
  begin
   //
    set_arm([
     kArrayMode2dTiledXThick,
     kArrayMode3dTiledXThick
    ]);
    begin
     push_bits;

      set_elm(m_z,2,$1,8);

     mark_end;
     pop_bits;
    end;

    set_arm([
     kArrayMode1dTiledThick,
     kArrayMode2dTiledThick,
     kArrayMode3dTiledThick,
     kArrayModeTiledThickPrt,
     kArrayMode2dTiledThickPrt,
     kArrayMode3dTiledThickPrt
    ]);

    set_bpe([b8,b16]);
      begin
       push_bits;

        set_elm(m_x,0,$1,0);
        set_elm(m_y,0,$1,1);
        set_elm(m_x,1,$1,2);
        set_elm(m_y,1,$1,3);
        set_elm(m_z,0,$1,4);
        set_elm(m_z,1,$1,5);
        set_elm(m_x,2,$1,6);
        set_elm(m_y,2,$1,7);

       mark_end;
       pop_bits;
      end;
    set_bpe([b32]);
      begin
       push_bits;

        set_elm(m_x,0,$1,0);
        set_elm(m_y,0,$1,1);
        set_elm(m_x,1,$1,2);
        set_elm(m_z,0,$1,3);
        set_elm(m_y,1,$1,4);
        set_elm(m_z,1,$1,5);
        set_elm(m_x,2,$1,6);
        set_elm(m_y,2,$1,7);

       mark_end;
       pop_bits;
      end;
    set_bpe([b64,b128]);
      begin
       push_bits;

        set_elm(m_x,0,$1,0);
        set_elm(m_y,0,$1,1);
        set_elm(m_z,0,$1,2);
        set_elm(m_x,1,$1,3);
        set_elm(m_y,1,$1,4);
        set_elm(m_z,1,$1,5);
        set_elm(m_x,2,$1,6);
        set_elm(m_y,2,$1,7);

       mark_end;
       pop_bits;
      end;

  end;

  Iterate_Axiss;
  generate_1dThin_asm;

 readln;
end.

