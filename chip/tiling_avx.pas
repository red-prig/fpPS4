unit tiling_avx;

{$mode objfpc}{$H+}
{$OPTIMIZATION REGVAR,PEEPHOLE,CSE,NODEADSTORE}

interface

//linear dimensions must be aligned 8x8 pixels

type
 t_copy_cbs=procedure(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_tile2linear_Display_1dThin_8(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_tile2linear_Display_1dThin_16(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_tile2linear_Display_1dThin_32(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_tile2linear_Display_1dThin_64(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_linear2tile_Display_1dThin_8(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_linear2tile_Display_1dThin_16(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_linear2tile_Display_1dThin_32(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_linear2tile_Display_1dThin_64(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_tile2linear_Thin_1dThin_8(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_tile2linear_Thin_1dThin_16(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_tile2linear_Thin_1dThin_32(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_tile2linear_Thin_1dThin_64(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_tile2linear_Thin_1dThin_128(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_linear2tile_Thin_1dThin_8(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_linear2tile_Thin_1dThin_16(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_linear2tile_Thin_1dThin_32(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_linear2tile_Thin_1dThin_64(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

procedure copy_linear2tile_Thin_1dThin_128(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;

const
 copy_array_tile2linear_Display_1dThin:array[0..4] of t_copy_cbs=(
  @copy_tile2linear_Display_1dThin_8,
  @copy_tile2linear_Display_1dThin_16,
  @copy_tile2linear_Display_1dThin_32,
  @copy_tile2linear_Display_1dThin_64,
  nil
 );

const
 copy_array_linear2tile_Display_1dThin:array[0..4] of t_copy_cbs=(
  @copy_linear2tile_Display_1dThin_8,
  @copy_linear2tile_Display_1dThin_16,
  @copy_linear2tile_Display_1dThin_32,
  @copy_linear2tile_Display_1dThin_64,
  nil
 );

const
 copy_array_tile2linear_Thin_1dThin:array[0..4] of t_copy_cbs=(
  @copy_tile2linear_Thin_1dThin_8,
  @copy_tile2linear_Thin_1dThin_16,
  @copy_tile2linear_Thin_1dThin_32,
  @copy_tile2linear_Thin_1dThin_64,
  @copy_tile2linear_Thin_1dThin_128
 );

const
 copy_array_linear2tile_Thin_1dThin:array[0..4] of t_copy_cbs=(
  @copy_linear2tile_Thin_1dThin_8,
  @copy_linear2tile_Thin_1dThin_16,
  @copy_linear2tile_Thin_1dThin_32,
  @copy_linear2tile_Thin_1dThin_64,
  @copy_linear2tile_Thin_1dThin_128
 );

implementation


procedure tile2linear_Display_1dThin_8(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rdi,%rdx,2), %r8  //+2
              lea (%rdi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rdi,%r10,2), %r10 //+6
          vmovups   0(%rsi), %ymm0
          vmovups  32(%rsi), %ymm1
 vextractf128 $1,  %ymm0, %xmm2
 vextractf128 $1,  %ymm1, %xmm3
            vmovq  %xmm0,   (%rdi)
            vmovq  %xmm2,   (%rdi,%rdx)
            vmovq  %xmm1,   (%r9 )
            vmovq  %xmm3,   (%r9 ,%rdx)
      vpextrq $1,  %xmm0,   (%r8 )
      vpextrq $1,  %xmm2,   (%r8 ,%rdx)
      vpextrq $1,  %xmm1,   (%r10)
      vpextrq $1,  %xmm3,   (%r10,%rdx)
end;

procedure linear2tile_Display_1dThin_8(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rsi,%rdx,2), %r8  //+2
              lea (%rsi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rsi,%r10,2), %r10 //+6
            vmovq   (%rsi), %xmm0
            vmovq   (%rsi,%rdx), %xmm2
            vmovq   (%r9 ), %xmm1
            vmovq   (%r9 ,%rdx), %xmm3
      vpinsrq $1,   (%r8 ), %xmm0, %xmm0
      vpinsrq $1,   (%r8 ,%rdx), %xmm2, %xmm2
      vpinsrq $1,   (%r10), %xmm1, %xmm1
      vpinsrq $1,   (%r10,%rdx), %xmm3, %xmm3
  vinsertf128 $1, %xmm2, %ymm0, %ymm0
  vinsertf128 $1, %xmm3, %ymm1, %ymm1
          vmovups  %ymm0,   0(%rdi)
          vmovups  %ymm1,  32(%rdi)
end;

procedure tile2linear_Display_1dThin_16(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rdi,%rdx,2), %r8  //+2
              lea (%rdi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rdi,%r10,2), %r10 //+6
          vmovups   0(%rsi), %ymm0
          vmovups  32(%rsi), %ymm1
          vmovups  64(%rsi), %ymm2
          vmovups  96(%rsi), %ymm3
          vmovups  %xmm0,   (%rdi)
          vmovups  %xmm1,   (%r8 )
          vmovups  %xmm2,   (%r9 )
          vmovups  %xmm3,   (%r10)
 vextractf128 $1,  %ymm0,   (%rdi,%rdx)
 vextractf128 $1,  %ymm1,   (%r8 ,%rdx)
 vextractf128 $1,  %ymm2,   (%r9 ,%rdx)
 vextractf128 $1,  %ymm3,   (%r10,%rdx)
end;

procedure linear2tile_Display_1dThin_16(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rsi,%rdx,2), %r8  //+2
              lea (%rsi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rsi,%r10,2), %r10 //+6
          vmovups   (%rsi), %xmm0
          vmovups   (%r8 ), %xmm1
          vmovups   (%r9 ), %xmm2
          vmovups   (%r10), %xmm3
  vinsertf128 $1,   (%rsi,%rdx), %ymm0, %ymm0
  vinsertf128 $1,   (%r8 ,%rdx), %ymm1, %ymm1
  vinsertf128 $1,   (%r9 ,%rdx), %ymm2, %ymm2
  vinsertf128 $1,   (%r10,%rdx), %ymm3, %ymm3
          vmovups  %ymm0,   0(%rdi)
          vmovups  %ymm1,  32(%rdi)
          vmovups  %ymm2,  64(%rdi)
          vmovups  %ymm3,  96(%rdi)
end;

procedure tile2linear_Display_1dThin_32(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rdi,%rdx,2), %r8  //+2
              lea (%rdi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rdi,%r10,2), %r10 //+6
          vmovups   0(%rsi), %ymm0
          vmovups  32(%rsi), %ymm1
          vmovups  64(%rsi), %ymm2
          vmovups  96(%rsi), %ymm3
          vmovups 128(%rsi), %ymm4
          vmovups 160(%rsi), %ymm5
          vmovups 192(%rsi), %ymm6
          vmovups 224(%rsi), %ymm7
          vmovups  %xmm0,   (%rdi)
          vmovups  %xmm1, 16(%rdi)
          vmovups  %xmm2,   (%r8 )
          vmovups  %xmm3, 16(%r8 )
          vmovups  %xmm4,   (%r9 )
          vmovups  %xmm5, 16(%r9 )
          vmovups  %xmm6,   (%r10)
          vmovups  %xmm7, 16(%r10)
 vextractf128 $1,  %ymm0,   (%rdi,%rdx)
 vextractf128 $1,  %ymm1, 16(%rdi,%rdx)
 vextractf128 $1,  %ymm2,   (%r8 ,%rdx)
 vextractf128 $1,  %ymm3, 16(%r8 ,%rdx)
 vextractf128 $1,  %ymm4,   (%r9 ,%rdx)
 vextractf128 $1,  %ymm5, 16(%r9 ,%rdx)
 vextractf128 $1,  %ymm6,   (%r10,%rdx)
 vextractf128 $1,  %ymm7, 16(%r10,%rdx)
end;

procedure linear2tile_Display_1dThin_32(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rsi,%rdx,2), %r8  //+2
              lea (%rsi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rsi,%r10,2), %r10 //+6
          vmovups   (%rsi), %xmm0
          vmovups 16(%rsi), %xmm1
          vmovups   (%r8 ), %xmm2
          vmovups 16(%r8 ), %xmm3
          vmovups   (%r9 ), %xmm4
          vmovups 16(%r9 ), %xmm5
          vmovups   (%r10), %xmm6
          vmovups 16(%r10), %xmm7
  vinsertf128 $1,   (%rsi,%rdx), %ymm0, %ymm0
  vinsertf128 $1, 16(%rsi,%rdx), %ymm1, %ymm1
  vinsertf128 $1,   (%r8 ,%rdx), %ymm2, %ymm2
  vinsertf128 $1, 16(%r8 ,%rdx), %ymm3, %ymm3
  vinsertf128 $1,   (%r9 ,%rdx), %ymm4, %ymm4
  vinsertf128 $1, 16(%r9 ,%rdx), %ymm5, %ymm5
  vinsertf128 $1,   (%r10,%rdx), %ymm6, %ymm6
  vinsertf128 $1, 16(%r10,%rdx), %ymm7, %ymm7
          vmovups  %ymm0,   0(%rdi)
          vmovups  %ymm1,  32(%rdi)
          vmovups  %ymm2,  64(%rdi)
          vmovups  %ymm3,  96(%rdi)
          vmovups  %ymm4, 128(%rdi)
          vmovups  %ymm5, 160(%rdi)
          vmovups  %ymm6, 192(%rdi)
          vmovups  %ymm7, 224(%rdi)
end;

procedure tile2linear_Display_1dThin_64(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rdi,%rdx,2), %r8  //+2
              lea (%rdi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rdi,%r10,2), %r10 //+6
          vmovups   0(%rsi), %ymm0
          vmovups  32(%rsi), %ymm1
          vmovups  64(%rsi), %ymm2
          vmovups  96(%rsi), %ymm3
          vmovups 128(%rsi), %ymm4
          vmovups 160(%rsi), %ymm5
          vmovups 192(%rsi), %ymm6
          vmovups 224(%rsi), %ymm7
          vmovups 256(%rsi), %ymm8
          vmovups 288(%rsi), %ymm9
          vmovups 320(%rsi), %ymm10
          vmovups 352(%rsi), %ymm11
          vmovups 384(%rsi), %ymm12
          vmovups 416(%rsi), %ymm13
          vmovups 448(%rsi), %ymm14
          vmovups 480(%rsi), %ymm15
          vmovups  %xmm0,   (%rdi)
          vmovups  %xmm1, 16(%rdi)
          vmovups  %xmm2, 32(%rdi)
          vmovups  %xmm3, 48(%rdi)
          vmovups  %xmm4,   (%r8 )
          vmovups  %xmm5, 16(%r8 )
          vmovups  %xmm6, 32(%r8 )
          vmovups  %xmm7, 48(%r8 )
          vmovups  %xmm8,   (%r9 )
          vmovups  %xmm9, 16(%r9 )
          vmovups %xmm10, 32(%r9 )
          vmovups %xmm11, 48(%r9 )
          vmovups %xmm12,   (%r10)
          vmovups %xmm13, 16(%r10)
          vmovups %xmm14, 32(%r10)
          vmovups %xmm15, 48(%r10)
 vextractf128 $1,  %ymm0,   (%rdi,%rdx)
 vextractf128 $1,  %ymm1, 16(%rdi,%rdx)
 vextractf128 $1,  %ymm2, 32(%rdi,%rdx)
 vextractf128 $1,  %ymm3, 48(%rdi,%rdx)
 vextractf128 $1,  %ymm4,   (%r8 ,%rdx)
 vextractf128 $1,  %ymm5, 16(%r8 ,%rdx)
 vextractf128 $1,  %ymm6, 32(%r8 ,%rdx)
 vextractf128 $1,  %ymm7, 48(%r8 ,%rdx)
 vextractf128 $1,  %ymm8,   (%r9 ,%rdx)
 vextractf128 $1,  %ymm9, 16(%r9 ,%rdx)
 vextractf128 $1, %ymm10, 32(%r9 ,%rdx)
 vextractf128 $1, %ymm11, 48(%r9 ,%rdx)
 vextractf128 $1, %ymm12,   (%r10,%rdx)
 vextractf128 $1, %ymm13, 16(%r10,%rdx)
 vextractf128 $1, %ymm14, 32(%r10,%rdx)
 vextractf128 $1, %ymm15, 48(%r10,%rdx)
end;

procedure linear2tile_Display_1dThin_64(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rsi,%rdx,2), %r8  //+2
              lea (%rsi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rsi,%r10,2), %r10 //+6
          vmovups   (%rsi), %xmm0
          vmovups 16(%rsi), %xmm1
          vmovups 32(%rsi), %xmm2
          vmovups 48(%rsi), %xmm3
          vmovups   (%r8 ), %xmm4
          vmovups 16(%r8 ), %xmm5
          vmovups 32(%r8 ), %xmm6
          vmovups 48(%r8 ), %xmm7
          vmovups   (%r9 ), %xmm8
          vmovups 16(%r9 ), %xmm9
          vmovups 32(%r9 ), %xmm10
          vmovups 48(%r9 ), %xmm11
          vmovups   (%r10), %xmm12
          vmovups 16(%r10), %xmm13
          vmovups 32(%r10), %xmm14
          vmovups 48(%r10), %xmm15
  vinsertf128 $1,   (%rsi,%rdx), %ymm0, %ymm0
  vinsertf128 $1, 16(%rsi,%rdx), %ymm1, %ymm1
  vinsertf128 $1, 32(%rsi,%rdx), %ymm2, %ymm2
  vinsertf128 $1, 48(%rsi,%rdx), %ymm3, %ymm3
  vinsertf128 $1,   (%r8 ,%rdx), %ymm4, %ymm4
  vinsertf128 $1, 16(%r8 ,%rdx), %ymm5, %ymm5
  vinsertf128 $1, 32(%r8 ,%rdx), %ymm6, %ymm6
  vinsertf128 $1, 48(%r8 ,%rdx), %ymm7, %ymm7
  vinsertf128 $1,   (%r9 ,%rdx), %ymm8, %ymm8
  vinsertf128 $1, 16(%r9 ,%rdx), %ymm9, %ymm9
  vinsertf128 $1, 32(%r9 ,%rdx), %ymm10, %ymm10
  vinsertf128 $1, 48(%r9 ,%rdx), %ymm11, %ymm11
  vinsertf128 $1,   (%r10,%rdx), %ymm12, %ymm12
  vinsertf128 $1, 16(%r10,%rdx), %ymm13, %ymm13
  vinsertf128 $1, 32(%r10,%rdx), %ymm14, %ymm14
  vinsertf128 $1, 48(%r10,%rdx), %ymm15, %ymm15
          vmovups  %ymm0,   0(%rdi)
          vmovups  %ymm1,  32(%rdi)
          vmovups  %ymm2,  64(%rdi)
          vmovups  %ymm3,  96(%rdi)
          vmovups  %ymm4, 128(%rdi)
          vmovups  %ymm5, 160(%rdi)
          vmovups  %ymm6, 192(%rdi)
          vmovups  %ymm7, 224(%rdi)
          vmovups  %ymm8, 256(%rdi)
          vmovups  %ymm9, 288(%rdi)
          vmovups %ymm10, 320(%rdi)
          vmovups %ymm11, 352(%rdi)
          vmovups %ymm12, 384(%rdi)
          vmovups %ymm13, 416(%rdi)
          vmovups %ymm14, 448(%rdi)
          vmovups %ymm15, 480(%rdi)
end;

procedure tile2linear_Thin_1dThin_8(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rdi,%rdx,2), %r8  //+2
              lea (%rdi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rdi,%r10,2), %r10 //+6
          vmovups   0(%rsi), %ymm0
          vmovups  32(%rsi), %ymm1
 vextractf128 $1,  %ymm0, %xmm2
 vextractf128 $1,  %ymm1, %xmm3
     vpextrw  $0,  %xmm0,   (%rdi)
     vpextrw  $2,  %xmm0,  2(%rdi)
     vpextrw  $0,  %xmm2,  4(%rdi)
     vpextrw  $2,  %xmm2,  6(%rdi)
     vpextrw  $1,  %xmm0,   (%rdi,%rdx)
     vpextrw  $3,  %xmm0,  2(%rdi,%rdx)
     vpextrw  $1,  %xmm2,  4(%rdi,%rdx)
     vpextrw  $3,  %xmm2,  6(%rdi,%rdx)
     vpextrw  $4,  %xmm0,   (%r8 )
     vpextrw  $6,  %xmm0,  2(%r8 )
     vpextrw  $4,  %xmm2,  4(%r8 )
     vpextrw  $6,  %xmm2,  6(%r8 )
     vpextrw  $5,  %xmm0,   (%r8 ,%rdx)
     vpextrw  $7,  %xmm0,  2(%r8 ,%rdx)
     vpextrw  $5,  %xmm2,  4(%r8 ,%rdx)
     vpextrw  $7,  %xmm2,  6(%r8 ,%rdx)
     vpextrw  $0,  %xmm1,   (%r9 )
     vpextrw  $2,  %xmm1,  2(%r9 )
     vpextrw  $0,  %xmm3,  4(%r9 )
     vpextrw  $2,  %xmm3,  6(%r9 )
     vpextrw  $1,  %xmm1,   (%r9 ,%rdx)
     vpextrw  $3,  %xmm1,  2(%r9 ,%rdx)
     vpextrw  $1,  %xmm3,  4(%r9 ,%rdx)
     vpextrw  $3,  %xmm3,  6(%r9 ,%rdx)
     vpextrw  $4,  %xmm1,   (%r10)
     vpextrw  $6,  %xmm1,  2(%r10)
     vpextrw  $4,  %xmm3,  4(%r10)
     vpextrw  $6,  %xmm3,  6(%r10)
     vpextrw  $5,  %xmm1,   (%r10,%rdx)
     vpextrw  $7,  %xmm1,  2(%r10,%rdx)
     vpextrw  $5,  %xmm3,  4(%r10,%rdx)
     vpextrw  $7,  %xmm3,  6(%r10,%rdx)
end;

procedure linear2tile_Thin_1dThin_8(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rsi,%rdx,2), %r8  //+2
              lea (%rsi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rsi,%r10,2), %r10 //+6
     vpinsrw  $0,   (%rsi), %xmm0, %xmm0
     vpinsrw  $2,  2(%rsi), %xmm0, %xmm0
     vpinsrw  $0,  4(%rsi), %xmm2, %xmm2
     vpinsrw  $2,  6(%rsi), %xmm2, %xmm2
     vpinsrw  $1,   (%rsi,%rdx), %xmm0, %xmm0
     vpinsrw  $3,  2(%rsi,%rdx), %xmm0, %xmm0
     vpinsrw  $1,  4(%rsi,%rdx), %xmm2, %xmm2
     vpinsrw  $3,  6(%rsi,%rdx), %xmm2, %xmm2
     vpinsrw  $4,   (%r8 ), %xmm0, %xmm0
     vpinsrw  $6,  2(%r8 ), %xmm0, %xmm0
     vpinsrw  $4,  4(%r8 ), %xmm2, %xmm2
     vpinsrw  $6,  6(%r8 ), %xmm2, %xmm2
     vpinsrw  $5,   (%r8 ,%rdx), %xmm0, %xmm0
     vpinsrw  $7,  2(%r8 ,%rdx), %xmm0, %xmm0
     vpinsrw  $5,  4(%r8 ,%rdx), %xmm2, %xmm2
     vpinsrw  $7,  6(%r8 ,%rdx), %xmm2, %xmm2
     vpinsrw  $0,   (%r9 ), %xmm1, %xmm1
     vpinsrw  $2,  2(%r9 ), %xmm1, %xmm1
     vpinsrw  $0,  4(%r9 ), %xmm3, %xmm3
     vpinsrw  $2,  6(%r9 ), %xmm3, %xmm3
     vpinsrw  $1,   (%r9 ,%rdx), %xmm1, %xmm1
     vpinsrw  $3,  2(%r9 ,%rdx), %xmm1, %xmm1
     vpinsrw  $1,  4(%r9 ,%rdx), %xmm3, %xmm3
     vpinsrw  $3,  6(%r9 ,%rdx), %xmm3, %xmm3
     vpinsrw  $4,   (%r10), %xmm1, %xmm1
     vpinsrw  $6,  2(%r10), %xmm1, %xmm1
     vpinsrw  $4,  4(%r10), %xmm3, %xmm3
     vpinsrw  $6,  6(%r10), %xmm3, %xmm3
     vpinsrw  $5,   (%r10,%rdx), %xmm1, %xmm1
     vpinsrw  $7,  2(%r10,%rdx), %xmm1, %xmm1
     vpinsrw  $5,  4(%r10,%rdx), %xmm3, %xmm3
     vpinsrw  $7,  6(%r10,%rdx), %xmm3, %xmm3
  vinsertf128 $1, %xmm2, %ymm0, %ymm0
  vinsertf128 $1, %xmm3, %ymm1, %ymm1
          vmovups  %ymm0,   0(%rdi)
          vmovups  %ymm1,  32(%rdi)
end;

procedure tile2linear_Thin_1dThin_16(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rdi,%rdx,2), %r8  //+2
              lea (%rdi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rdi,%r10,2), %r10 //+6
          vmovups   0(%rsi), %ymm0
          vmovups  32(%rsi), %ymm1
          vmovups  64(%rsi), %ymm2
          vmovups  96(%rsi), %ymm3
 vextractf128 $1,  %ymm0, %xmm4
 vextractf128 $1,  %ymm1, %xmm5
 vextractf128 $1,  %ymm2, %xmm6
 vextractf128 $1,  %ymm3, %xmm7
     vpextrd  $0,  %xmm0,   (%rdi)
     vpextrd  $2,  %xmm0,  4(%rdi)
     vpextrd  $0,  %xmm1,  8(%rdi)
     vpextrd  $2,  %xmm1, 12(%rdi)
     vpextrd  $1,  %xmm0,   (%rdi,%rdx)
     vpextrd  $3,  %xmm0,  4(%rdi,%rdx)
     vpextrd  $1,  %xmm1,  8(%rdi,%rdx)
     vpextrd  $3,  %xmm1, 12(%rdi,%rdx)
     vpextrd  $0,  %xmm4,   (%r8 )
     vpextrd  $2,  %xmm4,  4(%r8 )
     vpextrd  $0,  %xmm5,  8(%r8 )
     vpextrd  $2,  %xmm5, 12(%r8 )
     vpextrd  $1,  %xmm4,   (%r8 ,%rdx)
     vpextrd  $3,  %xmm4,  4(%r8 ,%rdx)
     vpextrd  $1,  %xmm5,  8(%r8 ,%rdx)
     vpextrd  $3,  %xmm5, 12(%r8 ,%rdx)
     vpextrd  $0,  %xmm2,   (%r9 )
     vpextrd  $2,  %xmm2,  4(%r9 )
     vpextrd  $0,  %xmm3,  8(%r9 )
     vpextrd  $2,  %xmm3, 12(%r9 )
     vpextrd  $1,  %xmm2,   (%r9 ,%rdx)
     vpextrd  $3,  %xmm2,  4(%r9 ,%rdx)
     vpextrd  $1,  %xmm3,  8(%r9 ,%rdx)
     vpextrd  $3,  %xmm3, 12(%r9 ,%rdx)
     vpextrd  $0,  %xmm6,   (%r10)
     vpextrd  $2,  %xmm6,  4(%r10)
     vpextrd  $0,  %xmm7,  8(%r10)
     vpextrd  $2,  %xmm7, 12(%r10)
     vpextrd  $1,  %xmm6,   (%r10,%rdx)
     vpextrd  $3,  %xmm6,  4(%r10,%rdx)
     vpextrd  $1,  %xmm7,  8(%r10,%rdx)
     vpextrd  $3,  %xmm7, 12(%r10,%rdx)
end;

procedure linear2tile_Thin_1dThin_16(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rsi,%rdx,2), %r8  //+2
              lea (%rsi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rsi,%r10,2), %r10 //+6
     vpinsrd  $0,   (%rsi), %xmm0, %xmm0
     vpinsrd  $2,  4(%rsi), %xmm0, %xmm0
     vpinsrd  $0,  8(%rsi), %xmm1, %xmm1
     vpinsrd  $2, 12(%rsi), %xmm1, %xmm1
     vpinsrd  $1,   (%rsi,%rdx), %xmm0, %xmm0
     vpinsrd  $3,  4(%rsi,%rdx), %xmm0, %xmm0
     vpinsrd  $1,  8(%rsi,%rdx), %xmm1, %xmm1
     vpinsrd  $3, 12(%rsi,%rdx), %xmm1, %xmm1
     vpinsrd  $0,   (%r8 ), %xmm4, %xmm4
     vpinsrd  $2,  4(%r8 ), %xmm4, %xmm4
     vpinsrd  $0,  8(%r8 ), %xmm5, %xmm5
     vpinsrd  $2, 12(%r8 ), %xmm5, %xmm5
     vpinsrd  $1,   (%r8 ,%rdx), %xmm4, %xmm4
     vpinsrd  $3,  4(%r8 ,%rdx), %xmm4, %xmm4
     vpinsrd  $1,  8(%r8 ,%rdx), %xmm5, %xmm5
     vpinsrd  $3, 12(%r8 ,%rdx), %xmm5, %xmm5
     vpinsrd  $0,   (%r9 ), %xmm2, %xmm2
     vpinsrd  $2,  4(%r9 ), %xmm2, %xmm2
     vpinsrd  $0,  8(%r9 ), %xmm3, %xmm3
     vpinsrd  $2, 12(%r9 ), %xmm3, %xmm3
     vpinsrd  $1,   (%r9 ,%rdx), %xmm2, %xmm2
     vpinsrd  $3,  4(%r9 ,%rdx), %xmm2, %xmm2
     vpinsrd  $1,  8(%r9 ,%rdx), %xmm3, %xmm3
     vpinsrd  $3, 12(%r9 ,%rdx), %xmm3, %xmm3
     vpinsrd  $0,   (%r10), %xmm6, %xmm6
     vpinsrd  $2,  4(%r10), %xmm6, %xmm6
     vpinsrd  $0,  8(%r10), %xmm7, %xmm7
     vpinsrd  $2, 12(%r10), %xmm7, %xmm7
     vpinsrd  $1,   (%r10,%rdx), %xmm6, %xmm6
     vpinsrd  $3,  4(%r10,%rdx), %xmm6, %xmm6
     vpinsrd  $1,  8(%r10,%rdx), %xmm7, %xmm7
     vpinsrd  $3, 12(%r10,%rdx), %xmm7, %xmm7
  vinsertf128 $1, %xmm4, %ymm0, %ymm0
  vinsertf128 $1, %xmm5, %ymm1, %ymm1
  vinsertf128 $1, %xmm6, %ymm2, %ymm2
  vinsertf128 $1, %xmm7, %ymm3, %ymm3
          vmovups  %ymm0,   0(%rdi)
          vmovups  %ymm1,  32(%rdi)
          vmovups  %ymm2,  64(%rdi)
          vmovups  %ymm3,  96(%rdi)
end;

procedure tile2linear_Thin_1dThin_32(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rdi,%rdx,2), %r8  //+2
              lea (%rdi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rdi,%r10,2), %r10 //+6
          vmovups   0(%rsi), %ymm0
          vmovups  32(%rsi), %ymm1
          vmovups  64(%rsi), %ymm2
          vmovups  96(%rsi), %ymm3
          vmovups 128(%rsi), %ymm4
          vmovups 160(%rsi), %ymm5
          vmovups 192(%rsi), %ymm6
          vmovups 224(%rsi), %ymm7
 vextractf128 $1,  %ymm0, %xmm8
 vextractf128 $1,  %ymm2, %xmm10
 vextractf128 $1,  %ymm1, %xmm9
 vextractf128 $1,  %ymm3, %xmm11
 vextractf128 $1,  %ymm4, %xmm12
 vextractf128 $1,  %ymm6, %xmm14
 vextractf128 $1,  %ymm5, %xmm13
 vextractf128 $1,  %ymm7, %xmm15
            vmovq  %xmm0,   (%rdi)
            vmovq  %xmm8,  8(%rdi)
            vmovq  %xmm2, 16(%rdi)
            vmovq %xmm10, 24(%rdi)
            vmovq  %xmm1,   (%r8 )
            vmovq  %xmm9,  8(%r8 )
            vmovq  %xmm3, 16(%r8 )
            vmovq %xmm11, 24(%r8 )
            vmovq  %xmm4,   (%r9 )
            vmovq %xmm12,  8(%r9 )
            vmovq  %xmm6, 16(%r9 )
            vmovq %xmm14, 24(%r9 )
            vmovq  %xmm5,   (%r10)
            vmovq %xmm13,  8(%r10)
            vmovq  %xmm7, 16(%r10)
            vmovq %xmm15, 24(%r10)
      vpextrq $1,  %xmm0,   (%rdi,%rdx)
      vpextrq $1,  %xmm8,  8(%rdi,%rdx)
      vpextrq $1,  %xmm2, 16(%rdi,%rdx)
      vpextrq $1, %xmm10, 24(%rdi,%rdx)
      vpextrq $1,  %xmm1,   (%r8 ,%rdx)
      vpextrq $1,  %xmm9,  8(%r8 ,%rdx)
      vpextrq $1,  %xmm3, 16(%r8 ,%rdx)
      vpextrq $1, %xmm11, 24(%r8 ,%rdx)
      vpextrq $1,  %xmm4,   (%r9 ,%rdx)
      vpextrq $1, %xmm12,  8(%r9 ,%rdx)
      vpextrq $1,  %xmm6, 16(%r9 ,%rdx)
      vpextrq $1, %xmm14, 24(%r9 ,%rdx)
      vpextrq $1,  %xmm5,   (%r10,%rdx)
      vpextrq $1, %xmm13,  8(%r10,%rdx)
      vpextrq $1,  %xmm7, 16(%r10,%rdx)
      vpextrq $1, %xmm15, 24(%r10,%rdx)
end;

procedure linear2tile_Thin_1dThin_32(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rsi,%rdx,2), %r8  //+2
              lea (%rsi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rsi,%r10,2), %r10 //+6
            vmovq   (%rsi), %xmm0
            vmovq  8(%rsi), %xmm8
            vmovq 16(%rsi), %xmm2
            vmovq 24(%rsi), %xmm10
            vmovq   (%r8 ), %xmm1
            vmovq  8(%r8 ), %xmm9
            vmovq 16(%r8 ), %xmm3
            vmovq 24(%r8 ), %xmm11
            vmovq   (%r9 ), %xmm4
            vmovq  8(%r9 ), %xmm12
            vmovq 16(%r9 ), %xmm6
            vmovq 24(%r9 ), %xmm14
            vmovq   (%r10), %xmm5
            vmovq  8(%r10), %xmm13
            vmovq 16(%r10), %xmm7
            vmovq 24(%r10), %xmm15
      vpinsrq $1,   (%rsi,%rdx), %xmm0, %xmm0
      vpinsrq $1,  8(%rsi,%rdx), %xmm8, %xmm8
      vpinsrq $1, 16(%rsi,%rdx), %xmm2, %xmm2
      vpinsrq $1, 24(%rsi,%rdx), %xmm10, %xmm10
      vpinsrq $1,   (%r8 ,%rdx), %xmm1, %xmm1
      vpinsrq $1,  8(%r8 ,%rdx), %xmm9, %xmm9
      vpinsrq $1, 16(%r8 ,%rdx), %xmm3, %xmm3
      vpinsrq $1, 24(%r8 ,%rdx), %xmm11, %xmm11
      vpinsrq $1,   (%r9 ,%rdx), %xmm4, %xmm4
      vpinsrq $1,  8(%r9 ,%rdx), %xmm12, %xmm12
      vpinsrq $1, 16(%r9 ,%rdx), %xmm6, %xmm6
      vpinsrq $1, 24(%r9 ,%rdx), %xmm14, %xmm14
      vpinsrq $1,   (%r10,%rdx), %xmm5, %xmm5
      vpinsrq $1,  8(%r10,%rdx), %xmm13, %xmm13
      vpinsrq $1, 16(%r10,%rdx), %xmm7, %xmm7
      vpinsrq $1, 24(%r10,%rdx), %xmm15, %xmm15
  vinsertf128 $1, %xmm8, %ymm0, %ymm0
  vinsertf128 $1, %xmm9, %ymm1, %ymm1
  vinsertf128 $1, %xmm10, %ymm2, %ymm2
  vinsertf128 $1, %xmm11, %ymm3, %ymm3
  vinsertf128 $1, %xmm12, %ymm4, %ymm4
  vinsertf128 $1, %xmm13, %ymm5, %ymm5
  vinsertf128 $1, %xmm14, %ymm6, %ymm6
  vinsertf128 $1, %xmm15, %ymm7, %ymm7
          vmovups  %ymm0,   0(%rdi)
          vmovups  %ymm1,  32(%rdi)
          vmovups  %ymm2,  64(%rdi)
          vmovups  %ymm3,  96(%rdi)
          vmovups  %ymm4, 128(%rdi)
          vmovups  %ymm5, 160(%rdi)
          vmovups  %ymm6, 192(%rdi)
          vmovups  %ymm7, 224(%rdi)
end;

procedure tile2linear_Thin_1dThin_64(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rdi,%rdx,2), %r8  //+2
              lea (%rdi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rdi,%r10,2), %r10 //+6
          vmovups   0(%rsi), %ymm0
          vmovups  32(%rsi), %ymm1
          vmovups  64(%rsi), %ymm2
          vmovups  96(%rsi), %ymm3
          vmovups 128(%rsi), %ymm4
          vmovups 160(%rsi), %ymm5
          vmovups 192(%rsi), %ymm6
          vmovups 224(%rsi), %ymm7
          vmovups 256(%rsi), %ymm8
          vmovups 288(%rsi), %ymm9
          vmovups 320(%rsi), %ymm10
          vmovups 352(%rsi), %ymm11
          vmovups 384(%rsi), %ymm12
          vmovups 416(%rsi), %ymm13
          vmovups 448(%rsi), %ymm14
          vmovups 480(%rsi), %ymm15
          vmovups  %xmm0,   (%rdi)
          vmovups  %xmm1, 16(%rdi)
          vmovups  %xmm4, 32(%rdi)
          vmovups  %xmm5, 48(%rdi)
          vmovups  %xmm2,   (%r8 )
          vmovups  %xmm3, 16(%r8 )
          vmovups  %xmm6, 32(%r8 )
          vmovups  %xmm7, 48(%r8 )
          vmovups  %xmm8,   (%r9 )
          vmovups  %xmm9, 16(%r9 )
          vmovups %xmm12, 32(%r9 )
          vmovups %xmm13, 48(%r9 )
          vmovups %xmm10,   (%r10)
          vmovups %xmm11, 16(%r10)
          vmovups %xmm14, 32(%r10)
          vmovups %xmm15, 48(%r10)
 vextractf128 $1,  %ymm0,   (%rdi,%rdx)
 vextractf128 $1,  %ymm1, 16(%rdi,%rdx)
 vextractf128 $1,  %ymm4, 32(%rdi,%rdx)
 vextractf128 $1,  %ymm5, 48(%rdi,%rdx)
 vextractf128 $1,  %ymm2,   (%r8 ,%rdx)
 vextractf128 $1,  %ymm3, 16(%r8 ,%rdx)
 vextractf128 $1,  %ymm6, 32(%r8 ,%rdx)
 vextractf128 $1,  %ymm7, 48(%r8 ,%rdx)
 vextractf128 $1,  %ymm8,   (%r9 ,%rdx)
 vextractf128 $1,  %ymm9, 16(%r9 ,%rdx)
 vextractf128 $1, %ymm12, 32(%r9 ,%rdx)
 vextractf128 $1, %ymm13, 48(%r9 ,%rdx)
 vextractf128 $1, %ymm10,   (%r10,%rdx)
 vextractf128 $1, %ymm11, 16(%r10,%rdx)
 vextractf128 $1, %ymm14, 32(%r10,%rdx)
 vextractf128 $1, %ymm15, 48(%r10,%rdx)
end;

procedure linear2tile_Thin_1dThin_64(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rsi,%rdx,2), %r8  //+2
              lea (%rsi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rsi,%r10,2), %r10 //+6
          vmovups   (%rsi), %xmm0
          vmovups 16(%rsi), %xmm1
          vmovups 32(%rsi), %xmm4
          vmovups 48(%rsi), %xmm5
          vmovups   (%r8 ), %xmm2
          vmovups 16(%r8 ), %xmm3
          vmovups 32(%r8 ), %xmm6
          vmovups 48(%r8 ), %xmm7
          vmovups   (%r9 ), %xmm8
          vmovups 16(%r9 ), %xmm9
          vmovups 32(%r9 ), %xmm12
          vmovups 48(%r9 ), %xmm13
          vmovups   (%r10), %xmm10
          vmovups 16(%r10), %xmm11
          vmovups 32(%r10), %xmm14
          vmovups 48(%r10), %xmm15
  vinsertf128 $1,   (%rsi,%rdx), %ymm0, %ymm0
  vinsertf128 $1, 16(%rsi,%rdx), %ymm1, %ymm1
  vinsertf128 $1, 32(%rsi,%rdx), %ymm4, %ymm4
  vinsertf128 $1, 48(%rsi,%rdx), %ymm5, %ymm5
  vinsertf128 $1,   (%r8 ,%rdx), %ymm2, %ymm2
  vinsertf128 $1, 16(%r8 ,%rdx), %ymm3, %ymm3
  vinsertf128 $1, 32(%r8 ,%rdx), %ymm6, %ymm6
  vinsertf128 $1, 48(%r8 ,%rdx), %ymm7, %ymm7
  vinsertf128 $1,   (%r9 ,%rdx), %ymm8, %ymm8
  vinsertf128 $1, 16(%r9 ,%rdx), %ymm9, %ymm9
  vinsertf128 $1, 32(%r9 ,%rdx), %ymm12, %ymm12
  vinsertf128 $1, 48(%r9 ,%rdx), %ymm13, %ymm13
  vinsertf128 $1,   (%r10,%rdx), %ymm10, %ymm10
  vinsertf128 $1, 16(%r10,%rdx), %ymm11, %ymm11
  vinsertf128 $1, 32(%r10,%rdx), %ymm14, %ymm14
  vinsertf128 $1, 48(%r10,%rdx), %ymm15, %ymm15
          vmovups  %ymm0,   0(%rdi)
          vmovups  %ymm1,  32(%rdi)
          vmovups  %ymm2,  64(%rdi)
          vmovups  %ymm3,  96(%rdi)
          vmovups  %ymm4, 128(%rdi)
          vmovups  %ymm5, 160(%rdi)
          vmovups  %ymm6, 192(%rdi)
          vmovups  %ymm7, 224(%rdi)
          vmovups  %ymm8, 256(%rdi)
          vmovups  %ymm9, 288(%rdi)
          vmovups %ymm10, 320(%rdi)
          vmovups %ymm11, 352(%rdi)
          vmovups %ymm12, 384(%rdi)
          vmovups %ymm13, 416(%rdi)
          vmovups %ymm14, 448(%rdi)
          vmovups %ymm15, 480(%rdi)
end;

procedure tile2linear_Thin_1dThin_128(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rdi,%rdx,2), %r8  //+2
              lea (%rdi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rdi,%r10,2), %r10 //+6
          vmovups   0(%rsi), %ymm0
          vmovups  32(%rsi), %ymm1
          vmovups  64(%rsi), %ymm2
          vmovups  96(%rsi), %ymm3
          vmovups 128(%rsi), %ymm4
          vmovups 160(%rsi), %ymm5
          vmovups 192(%rsi), %ymm6
          vmovups 224(%rsi), %ymm7
          vmovups 256(%rsi), %ymm8
          vmovups 288(%rsi), %ymm9
          vmovups 320(%rsi), %ymm10
          vmovups 352(%rsi), %ymm11
          vmovups 384(%rsi), %ymm12
          vmovups 416(%rsi), %ymm13
          vmovups 448(%rsi), %ymm14
          vmovups 480(%rsi), %ymm15
          vmovups  %ymm0,   (%rdi)
          vmovups  %ymm2, 32(%rdi)
          vmovups  %ymm8, 64(%rdi)
          vmovups %ymm10, 96(%rdi)
          vmovups  %ymm1,   (%rdi,%rdx)
          vmovups  %ymm3, 32(%rdi,%rdx)
          vmovups  %ymm9, 64(%rdi,%rdx)
          vmovups %ymm11, 96(%rdi,%rdx)
          vmovups  %ymm4,   (%r8 )
          vmovups  %ymm6, 32(%r8 )
          vmovups %ymm12, 64(%r8 )
          vmovups %ymm14, 96(%r8 )
          vmovups  %ymm5,   (%r8 ,%rdx)
          vmovups  %ymm7, 32(%r8 ,%rdx)
          vmovups %ymm13, 64(%r8 ,%rdx)
          vmovups %ymm15, 96(%r8 ,%rdx)
          vmovups 512(%rsi), %ymm0
          vmovups 544(%rsi), %ymm1
          vmovups 576(%rsi), %ymm2
          vmovups 608(%rsi), %ymm3
          vmovups 640(%rsi), %ymm4
          vmovups 672(%rsi), %ymm5
          vmovups 704(%rsi), %ymm6
          vmovups 736(%rsi), %ymm7
          vmovups 768(%rsi), %ymm8
          vmovups 800(%rsi), %ymm9
          vmovups 832(%rsi), %ymm10
          vmovups 864(%rsi), %ymm11
          vmovups 896(%rsi), %ymm12
          vmovups 928(%rsi), %ymm13
          vmovups 960(%rsi), %ymm14
          vmovups 992(%rsi), %ymm15
          vmovups  %ymm0,   (%r9 )
          vmovups  %ymm2, 32(%r9 )
          vmovups  %ymm8, 64(%r9 )
          vmovups %ymm10, 96(%r9 )
          vmovups  %ymm1,   (%r9 ,%rdx)
          vmovups  %ymm3, 32(%r9 ,%rdx)
          vmovups  %ymm9, 64(%r9 ,%rdx)
          vmovups %ymm11, 96(%r9 ,%rdx)
          vmovups  %ymm4,   (%r10)
          vmovups  %ymm6, 32(%r10)
          vmovups %ymm12, 64(%r10)
          vmovups %ymm14, 96(%r10)
          vmovups  %ymm5,   (%r10,%rdx)
          vmovups  %ymm7, 32(%r10,%rdx)
          vmovups %ymm13, 64(%r10,%rdx)
          vmovups %ymm15, 96(%r10,%rdx)
end;

procedure linear2tile_Thin_1dThin_128(dst,src:Pointer;pitch:QWORD); SysV_ABI_CDecl; assembler; nostackframe;
asm
              lea (%rsi,%rdx,2), %r8  //+2
              lea (%rsi,%rdx,4), %r9  //+4
              lea (%rdx,%rdx,2), %r10 //+3
              lea (%rsi,%r10,2), %r10 //+6
          vmovups   (%rsi), %ymm0
          vmovups 32(%rsi), %ymm2
          vmovups 64(%rsi), %ymm8
          vmovups 96(%rsi), %ymm10
          vmovups   (%rsi,%rdx), %ymm1
          vmovups 32(%rsi,%rdx), %ymm3
          vmovups 64(%rsi,%rdx), %ymm9
          vmovups 96(%rsi,%rdx), %ymm11
          vmovups   (%r8 ), %ymm4
          vmovups 32(%r8 ), %ymm6
          vmovups 64(%r8 ), %ymm12
          vmovups 96(%r8 ), %ymm14
          vmovups   (%r8 ,%rdx), %ymm5
          vmovups 32(%r8 ,%rdx), %ymm7
          vmovups 64(%r8 ,%rdx), %ymm13
          vmovups 96(%r8 ,%rdx), %ymm15
          vmovups  %ymm0,   0(%rdi)
          vmovups  %ymm1,  32(%rdi)
          vmovups  %ymm2,  64(%rdi)
          vmovups  %ymm3,  96(%rdi)
          vmovups  %ymm4, 128(%rdi)
          vmovups  %ymm5, 160(%rdi)
          vmovups  %ymm6, 192(%rdi)
          vmovups  %ymm7, 224(%rdi)
          vmovups  %ymm8, 256(%rdi)
          vmovups  %ymm9, 288(%rdi)
          vmovups %ymm10, 320(%rdi)
          vmovups %ymm11, 352(%rdi)
          vmovups %ymm12, 384(%rdi)
          vmovups %ymm13, 416(%rdi)
          vmovups %ymm14, 448(%rdi)
          vmovups %ymm15, 480(%rdi)
          vmovups   (%r9 ), %ymm0
          vmovups 32(%r9 ), %ymm2
          vmovups 64(%r9 ), %ymm8
          vmovups 96(%r9 ), %ymm10
          vmovups   (%r9 ,%rdx), %ymm1
          vmovups 32(%r9 ,%rdx), %ymm3
          vmovups 64(%r9 ,%rdx), %ymm9
          vmovups 96(%r9 ,%rdx), %ymm11
          vmovups   (%r10), %ymm4
          vmovups 32(%r10), %ymm6
          vmovups 64(%r10), %ymm12
          vmovups 96(%r10), %ymm14
          vmovups   (%r10,%rdx), %ymm5
          vmovups 32(%r10,%rdx), %ymm7
          vmovups 64(%r10,%rdx), %ymm13
          vmovups 96(%r10,%rdx), %ymm15
          vmovups  %ymm0, 512(%rdi)
          vmovups  %ymm1, 544(%rdi)
          vmovups  %ymm2, 576(%rdi)
          vmovups  %ymm3, 608(%rdi)
          vmovups  %ymm4, 640(%rdi)
          vmovups  %ymm5, 672(%rdi)
          vmovups  %ymm6, 704(%rdi)
          vmovups  %ymm7, 736(%rdi)
          vmovups  %ymm8, 768(%rdi)
          vmovups  %ymm9, 800(%rdi)
          vmovups %ymm10, 832(%rdi)
          vmovups %ymm11, 864(%rdi)
          vmovups %ymm12, 896(%rdi)
          vmovups %ymm13, 928(%rdi)
          vmovups %ymm14, 960(%rdi)
          vmovups %ymm15, 992(%rdi)
end;

procedure copy_tile2linear_Display_1dThin_8(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=1;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    tile2linear_Display_1dThin_8(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_paddedWidth;
   dst:=dst+m_pitch_bytes*7;

   Dec(y);
  until (y=0);

  src:=src+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_tile2linear_Display_1dThin_16(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=2;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    tile2linear_Display_1dThin_16(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_paddedWidth;
   dst:=dst+m_pitch_bytes*7;

   Dec(y);
  until (y=0);

  src:=src+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_tile2linear_Display_1dThin_32(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=4;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    tile2linear_Display_1dThin_32(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_paddedWidth;
   dst:=dst+m_pitch_bytes*7;

   Dec(y);
  until (y=0);

  src:=src+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_tile2linear_Display_1dThin_64(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=8;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    tile2linear_Display_1dThin_64(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_paddedWidth;
   dst:=dst+m_pitch_bytes*7;

   Dec(y);
  until (y=0);

  src:=src+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_linear2tile_Display_1dThin_8(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=1;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    linear2tile_Display_1dThin_8(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_pitch_bytes*7;
   dst:=dst+m_paddedWidth;

   Dec(y);
  until (y=0);

  dst:=dst+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_linear2tile_Display_1dThin_16(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=2;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    linear2tile_Display_1dThin_16(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_pitch_bytes*7;
   dst:=dst+m_paddedWidth;

   Dec(y);
  until (y=0);

  dst:=dst+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_linear2tile_Display_1dThin_32(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=4;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    linear2tile_Display_1dThin_32(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_pitch_bytes*7;
   dst:=dst+m_paddedWidth;

   Dec(y);
  until (y=0);

  dst:=dst+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_linear2tile_Display_1dThin_64(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=8;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    linear2tile_Display_1dThin_64(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_pitch_bytes*7;
   dst:=dst+m_paddedWidth;

   Dec(y);
  until (y=0);

  dst:=dst+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_tile2linear_Thin_1dThin_8(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=1;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    tile2linear_Thin_1dThin_8(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_paddedWidth;
   dst:=dst+m_pitch_bytes*7;

   Dec(y);
  until (y=0);

  src:=src+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_tile2linear_Thin_1dThin_16(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=2;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    tile2linear_Thin_1dThin_16(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_paddedWidth;
   dst:=dst+m_pitch_bytes*7;

   Dec(y);
  until (y=0);

  src:=src+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_tile2linear_Thin_1dThin_32(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=4;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    tile2linear_Thin_1dThin_32(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_paddedWidth;
   dst:=dst+m_pitch_bytes*7;

   Dec(y);
  until (y=0);

  src:=src+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_tile2linear_Thin_1dThin_64(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=8;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    tile2linear_Thin_1dThin_64(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_paddedWidth;
   dst:=dst+m_pitch_bytes*7;

   Dec(y);
  until (y=0);

  src:=src+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_tile2linear_Thin_1dThin_128(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=16;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    tile2linear_Thin_1dThin_128(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_paddedWidth;
   dst:=dst+m_pitch_bytes*7;

   Dec(y);
  until (y=0);

  src:=src+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_linear2tile_Thin_1dThin_8(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=1;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    linear2tile_Thin_1dThin_8(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_pitch_bytes*7;
   dst:=dst+m_paddedWidth;

   Dec(y);
  until (y=0);

  dst:=dst+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_linear2tile_Thin_1dThin_16(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=2;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    linear2tile_Thin_1dThin_16(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_pitch_bytes*7;
   dst:=dst+m_paddedWidth;

   Dec(y);
  until (y=0);

  dst:=dst+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_linear2tile_Thin_1dThin_32(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=4;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    linear2tile_Thin_1dThin_32(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_pitch_bytes*7;
   dst:=dst+m_paddedWidth;

   Dec(y);
  until (y=0);

  dst:=dst+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_linear2tile_Thin_1dThin_64(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=8;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    linear2tile_Thin_1dThin_64(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_pitch_bytes*7;
   dst:=dst+m_paddedWidth;

   Dec(y);
  until (y=0);

  dst:=dst+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

procedure copy_linear2tile_Thin_1dThin_128(
  dst,src:Pointer;
  m_linearWidth,
  m_linearHeight,
  m_linearDepth,
  m_paddedWidth,
  m_paddedHeight:DWORD); SysV_ABI_CDecl;
const
 kMicroTileWidth =8;
 kMicroTileHeight=8;
 m_bytePerElement=16;
var
 m_pitch_bytes:QWORD;
 x,y,z        :DWORD;
begin

 m_pitch_bytes :=(m_linearWidth)*m_bytePerElement;

 m_paddedWidth :=(m_paddedWidth  - m_linearWidth )*(kMicroTileHeight * m_bytePerElement);
 m_paddedHeight:=(m_paddedHeight - m_linearHeight)*m_pitch_bytes;

 m_linearWidth :=(m_linearWidth  div kMicroTileWidth );
 m_linearHeight:=(m_linearHeight div kMicroTileHeight);

 z:=m_linearDepth;
 repeat

  y:=m_linearHeight;
  repeat

   x:=m_linearWidth;
   repeat
    linear2tile_Thin_1dThin_128(dst,src,m_pitch_bytes);

    src:=src+(kMicroTileWidth * m_bytePerElement);
    dst:=dst+(kMicroTileWidth * kMicroTileHeight * m_bytePerElement);

    Dec(x);
   until (x=0);

   src:=src+m_pitch_bytes*7;
   dst:=dst+m_paddedWidth;

   Dec(y);
  until (y=0);

  dst:=dst+m_paddedHeight;

  Dec(z);
 until (z=0);
end;

end.

