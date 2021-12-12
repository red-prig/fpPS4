/*
 ***********************************************************************************************************************
 *
 *  Copyright (c) 2017-2021 Advanced Micro Devices, Inc. All Rights Reserved.
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in all
 *  copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *  SOFTWARE.
 *
 **********************************************************************************************************************/

#ifndef __SI_CI_VI_MERGED_PM4DEFS_HEADER
#define __SI_CI_VI_MERGED_PM4DEFS_HEADER

#include "si_ci_vi_merged_typedef.h"

/******************************************************************************
*
*  si_ci_vi_merged_pm4defs.h
*
*  SI/CI/VI PM4 definitions, typedefs, and enumerations.
*
******************************************************************************/

namespace Pal
{
namespace Gfx6
{
inline namespace Chip
{

// File version information
#define SI_CI_VI_PM4DEFS_VERSION_MAJOR  3
#define SI_CI_VI_PM4DEFS_VERSION_MINOR  0

// PM4 command shifts
#define PM4_PREDICATE_SHIFT              0
#define PM4_SHADERTYPE_SHIFT             1
#define PM4_OP_SHIFT                     8
#define PM4_COUNT_SHIFT                  16
#define PM4_TYPE_SHIFT                   30
#define PM4_T0_ONE_REG_WR_SHIFT          15
#define PM4_T0_INDX_SHIFT                0

// PM4 command control settings
#define PM4_T0_NO_INCR                  (1 << PM4_T0_ONE_REG_WR_SHIFT)

// ROLL_CONTEXT defines
#define PM4_SEL_8_CP_STATE               0
#define PM4_SEL_BLOCK_STATE              1

/**
***************************************************************************************************
* @brief This enum defines the Shader types supported in PM4 type 3 header
***************************************************************************************************
*/
enum PM4ShaderType
{
    ShaderGraphics  = 0,    ///< Graphics shader
    ShaderCompute   = 1     ///< Compute shader
};

/**
***************************************************************************************************
* @brief This enum defines the predicate value supported in PM4 type 3 header
***************************************************************************************************
*/
enum PM4Predicate
{
    PredDisable = 0,    ///< Predicate disabled
    PredEnable  = 1     ///< Predicate enabled
};

// PM4 type 3 header macro for creating a PM4 type 3 header
#define PM4_TYPE_3_HDR(opCode, count, shaderType, predicate)             \
      ((unsigned int)(predicate      <<  PM4_PREDICATE_SHIFT)    |       \
                     (shaderType     <<  PM4_SHADERTYPE_SHIFT)   |       \
                     (PM4_TYPE_3     <<  PM4_TYPE_SHIFT)         |       \
                     ((count - 2)    <<  PM4_COUNT_SHIFT)        |       \
                     (opCode         <<  PM4_OP_SHIFT))

// PM4 type 0 header macros
#define PM4_TYPE_0_HDR(Reg0, nWrites)                                    \
    ((((unsigned int)(nWrites)-1) << PM4_COUNT_SHIFT)       |            \
                     ((Reg0)      << PM4_T0_INDX_SHIFT))

// RJVR: This macro needs to be modified to use Type 3 ONE_REG_WRITE.
#define PM4_TYPE_0_HDR_NO_INCR(Reg0, nWrites)                            \
    ((((unsigned int)(nWrites)-1) << PM4_COUNT_SHIFT) |                  \
     ((Reg0) << PM4_T0_INDX_SHIFT)                    |                  \
     PM4_T0_NO_INCR)

// PM4 type 2 NOP
#define PM4_TYPE_2_NOP                   (PM4_TYPE_2 << PM4_TYPE_SHIFT)

//-------------------------------------------------------------------------------------------------

typedef union _PM4_TYPE_0_HEADER
{
    struct
    {
        unsigned int base  : 16;///< the DWORD Memory-mapped address
        unsigned int count : 14;///< count of DWORDs in the *information* body (N - 1 for N dwords)
        unsigned int type  :  2;///< packet identifier. It should be 0 for type 0 packets.
    };
    unsigned int u32All;

} PM4_TYPE_0_HEADER;

//-------------------------------------------------------------------------------------------------

typedef union PM4_TYPE_3_HEADER
{
    struct
    {
        unsigned int predicate : 1; ///< predicated version of packet when set
        unsigned int shaderType: 1; ///< 0: Graphics, 1: Compute Shader
        unsigned int reserved1 : 6; ///< reserved
        unsigned int opcode    : 8; ///< IT opcode
        unsigned int count     : 14;///< number of DWORDs - 1 in the information body.
        unsigned int type      : 2; ///< packet identifier. It should be 3 for type 3 packets
    };
    unsigned int u32All;
} PM4_TYPE_3_HEADER;

//-------------------------------------------------------------------------------------------------

typedef union _CONTEXT_CONTROL_ENABLE
{
    struct
    {
        unsigned int enableSingleCntxConfigReg      :  1;   ///< single context config reg
        unsigned int enableMultiCntxRenderReg       :  1;   ///< multi context render state reg
        unsigned int reserved1                      : 13;   ///< reserved
        unsigned int enableUserConfigReg__CI        :  1;   ///< User Config Reg on CI(reserved for SI)
        unsigned int enableGfxSHReg                 :  1;   ///< Gfx SH Registers
        unsigned int reserved2                      :  7;   ///< reserved
        unsigned int enableCSSHReg                  :  1;   ///< CS SH Registers
        unsigned int reserved3                      :  6;   ///< reserved
        unsigned int enableDw                       :  1;   ///< DW enable
    };
    unsigned int u32All;
} CONTEXT_CONTROL_ENABLE;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDCONTEXTCONTROL
{
    union
    {
        PM4_TYPE_3_HEADER       header;      ///< header
        unsigned int            ordinal1;
    };
    union
    {
        CONTEXT_CONTROL_ENABLE  loadControl; ///< enable bits for loading
        unsigned int            ordinal2;
    };
    union
    {
        CONTEXT_CONTROL_ENABLE  shadowEnable;///< enable bits for shadowing
        unsigned int            ordinal3;
    };

} PM4CMDCONTEXTCONTROL, *PPM4CMDCONTEXTCONTROL;

//-------------------------------------------------------------------------------------------------
typedef union _LOAD_ADDRESS_HIGH
{
    struct
    {
        unsigned int ADDR_HI     : 16;   ///< bits (47:32) for the block in Memory from where
                                         ///< the CP will fetch the state
        unsigned int reserved1   : 15;   ///< reserved
        unsigned int WAIT_IDLE   : 1;    ///< if set the CP will wait for the graphics pipe to
                                         ///< be idle by writing to the GRBM Wait Until register
                                         ///< with "Wait for 3D idle"
    };
    unsigned int    u32All;
} LOAD_ADDRESS_HIGH;

//-------------------------------------------------------------------------------------------------

// PM4CMDLOADDATA can be used with the following opcodes
// - IT_LOAD_CONFIG_REG
// - IT_LOAD_CONTEXT_REG
// - IT_LOAD_SH_REG
typedef struct _PM4CMDLOADDATA
{
    union
    {
        PM4_TYPE_3_HEADER   header;     ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        addrLo;     ///< low 32 address bits for the block in memory from where
                                        ///< the CP will fetch the state
        unsigned int        ordinal2;
    };
    union
    {
        LOAD_ADDRESS_HIGH   addrHi;
        unsigned int        ordinal3;
    };
    union
    {
        unsigned int        regOffset;  ///< offset in DWords from the register base address
        unsigned int        ordinal4;
    };
    union
    {
        unsigned int        numDwords;  ///< number of DWords that the CP will fetch and write
                                        ///< into the chip. A value of zero will fetch nothing
        unsigned int        ordinal5;
    };
    // This is a variable length packet. So, based on size in header, the layout following this
    // looks as follows (offser/numDwords pairs).
    // unsigned int offset1;
    // unsigned int numDwords1;
    // ...
    // unsigned int offsetN;
    // unsigned int numDwordsN;

} PM4CMDLOADDATA, *PPM4CMDLOADDATA;

//-------------------------------------------------------------------------------------------------
typedef union _LOAD_ADDRESS_LOW
{
    struct
    {
        unsigned int index     :  1; ///< 0 : ADDR_LO is direct address
                                     ///< 1 : ARRD_LO is ignored and memory offset is in ordinal 3
        unsigned int reserved  :  1; ///< reserved
        unsigned int ADDR_LO   : 30; ///< bits (31:2) for the block in Memory from where
                                     ///< the CP will fetch the state. DWORD aligned
    };
    unsigned int    u32All;
} LOAD_ADDRESS_LOW;

//-------------------------------------------------------------------------------------------------

// PM4CMDLOADDATAINDEX can be used with the following opcodes (VI+)
// - IT_LOAD_CONTEXT_REG_INDEX
// - IT_LOAD_SH_REG_INDEX

// Index values (VI+)
#define LOAD_DATA_INDEX_DIRECT_ADDR         0  // Direct load from memory address
#define LOAD_DATA_INDEX_OFFSET              1  // Load from indirect memory offset (_INDEX packets)

#define LOAD_DATA_FORMAT_OFFSET_AND_SIZE    0  // Data is consecutive DWORDs
#define LOAD_DATA_FORMAT_OFFSET_AND_DATA    1  // Register offset and data is interleaved

typedef struct _PM4CMDLOADDATAINDEX
{
    union
    {
        PM4_TYPE_3_HEADER   header;     ///< header
        unsigned int        ordinal1;
    };
    union
    {
        LOAD_ADDRESS_LOW    addrLo;     ///< low 32 address bits for the block in memory from where
                                        ///< the CP will fetch the state
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        addrOffset; ///< addrLo.index = 1 Indexed mode
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int    regOffset  : 16;  ///< offset in DWords from the register base address
            unsigned int    reserved   : 15;
            unsigned int    dataFormat :  1;  ///< LOAD_DATA_FORMAT_*
        };
        unsigned int        ordinal4;
    };
    union
    {
        unsigned int        numDwords;  ///< number of DWords that the CP will fetch and write
                                        ///< into the chip. A value of zero will fetch nothing
        unsigned int        ordinal5;
    };
    // This is a variable length packet. So, based on size in header, the layout following this
    // looks as follows (offser/numDwords pairs).
    // unsigned int offset1;
    // unsigned int numDwords1;
    // ...
    // unsigned int offsetN;
    // unsigned int numDwordsN;

} PM4CMDLOADDATAINDEX, *PPM4CMDLOADDATAINDEX;

//-------------------------------------------------------------------------------------------------

// PM4CMDSETDATA can be used with the following opcodes:
//
// - IT_SET_CONFIG_REG
// - IT_SET_CONTEXT_REG
// - IT_SET_CONTEXT_REG_INDIRECT
// - IT_SET_SH_REG
// - IT_SET_SH_REG_INDEX
// - IT_SET_UCONFIG_REG

// SET_CONTEXT_REG index values (CI+)
#define SET_CONTEXT_INDEX_DEFAULT             0  // Use this for all registers except the following...
#define SET_CONTEXT_INDEX_MULTI_VGT_PARAM     1  // Use this when writing IA_MULTI_VGT_PARAM
#define SET_CONTEXT_INDEX_VGT_LS_HS_CONFIG    2  // Use this when writing VGT_LS_HS_CONFIG
#define SET_CONTEXT_INDEX_PA_SC_RASTER_CONFIG 3  // Use this when writing PA_SC_RASTER_CONFIG

#define SET_CONTEXT_INDEX_SHIFT 28 // Offset in ordinal2 of the index field.

// SET_UCONFIG_REG index values (CI+)
#define SET_UCONFIG_INDEX_DEFAULT       0  // Use this for all registers except the following...
#define SET_UCONFIG_INDEX_PRIM_TYPE     1  // Use this when writing VGT_PRIMITIVE_TYPE
#define SET_UCONFIG_INDEX_INDEX_TYPE    2  // Use this when writing VGT_INDEX_TYPE
#define SET_UCONFIG_INDEX_NUM_INSTANCES 3  // Use this when writing VGT_NUM_INSTANCES

// SET_SH_REG_INDEX index values (Hawaii, VI+)
// Index (0-2): reserved
#define SET_SH_REG_INDEX_CP_MODIFY_CU_MASK 3 // Use this to modify CU_EN for COMPUTE_STATIC* and SPI_SHADER_PGM_RSRC3*
                                             // CP performs AND operation on KMD and UMD CU masks to write registers.

typedef struct _PM4CMDSETDATA
{
    union
    {
        PM4_TYPE_3_HEADER   header;     ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    regOffset : 16;  ///< offset in DWords from the register base address
            unsigned int    reserved1 : 12;  ///< Program to zero
            unsigned int    index     : 4;   ///< Index for UCONFIG/CONTEXT on CI+
                                             ///< Program to zero for other opcodes and on SI
        };
        unsigned int        ordinal2;
    };
    // This is a variable length packet. So, based on size in header, the layout following this
    // looks as follows:
    // Data for SET_CONTEXT_REG
    // DW Offset into Patch table for SET_CONTEXT_REG_INDIRECT
    // unsigned int data0;
    // ...
    // unsigned int dataN;

} PM4CMDSETDATA, *PPM4CMDSETDATA;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDNOP
{
    union
    {
        PM4_TYPE_3_HEADER   header;     ///< header
        unsigned int        ordinal1;
    };
} PM4CMDNOP, *PPM4CMDNOP;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWINDEXOFFSET2
{
    union
    {
        PM4_TYPE_3_HEADER       header;         ///<header
        unsigned int            ordinal1;
    };
    union
    {
        unsigned int            maxSize;        ///< maximum number of indices
        unsigned int            ordinal2;
    };
    union
    {
        unsigned int            indexOffset;    ///< zero based starting index number
                                                ///< in the index buffer
        unsigned int            ordinal3;
    };
    union
    {
        regVGT_DMA_SIZE         indexCount;     ///< number of indices in the Index Buffer
        unsigned int            ordinal4;
    };
    union
    {
        regVGT_DRAW_INITIATOR   drawInitiator;  ///< draw Initiator Register
        unsigned int            ordinal5;
    };
} PM4CMDDRAWINDEXOFFSET2, *PPM4CMDDRAWINDEXOFFSET2;

//-------------------------------------------------------------------------------------------------

typedef struct PM4CMDDRAWPREAMBLE
{
    union
    {
        PM4_TYPE_3_HEADER       header;         ///< header
        unsigned int            ordinal1;
    };
    union
    {
        regVGT_PRIMITIVE_TYPE   control1;       ///< writes to VGT_PRIMITIVE_TYPE reg
        unsigned int            ordinal2;
    };
    union
    {
        regIA_MULTI_VGT_PARAM   control2;       ///< writes to IA_MULTI_VGT_PARAM reg
        unsigned int            ordinal3;
    };
    union
    {
        regVGT_LS_HS_CONFIG     control3;       ///< writes to VGT_LS_HS_CONFIG reg
        unsigned int            ordinal4;
    };

} PM4CMDDRAWPREAMBLE, *PPM4CMDDRAWPREAMBLE;

//-------------------------------------------------------------------------------------------------

typedef struct PM4CMDDRAWINDEX2
{
    union
    {
        PM4_TYPE_3_HEADER     header;     ///< header
        unsigned int          ordinal1;
    };
    union
    {
        unsigned int          maxSize;    ///< maximum number of indices
        unsigned int          ordinal2;
    };
    union
    {
        unsigned int          indexBaseLo;///< base Address Lo [31:1] of Index Buffer
                                          ///< (Word-Aligned). Written to the VGT_DMA_BASE register.
        unsigned int          ordinal3;
    };
    union
    {
        unsigned int          indexBaseHi;///< base Address Hi [39:32] of Index Buffer.
                                          ///< Written to the VGT_DMA_BASE_HI register
        unsigned int          ordinal4;
    };
    union
    {
        unsigned int          indexCount; ///< number of indices in the Index Buffer.
                                          ///< Written to the VGT_NUM_INDICES register.
        unsigned int          ordinal5;
    };
    union
    {
        regVGT_DRAW_INITIATOR drawInitiator;  ///< written to the VGT_DRAW_INITIATOR register
        unsigned int          ordinal6;
    };

} PM4CMDDRAWINDEX2, *PPM4CMDDRAWINDEX2;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWINDEXAUTO
{
    union
    {
        PM4_TYPE_3_HEADER     header;         ///< header
        unsigned int          ordinal1;
    };
    union
    {
        unsigned int          indexCount;     ///< max index count
        unsigned int          ordinal2;
    };
    union
    {
        regVGT_DRAW_INITIATOR drawInitiator;  ///< written to the VGT_DRAW_INITIATOR register
        unsigned int          ordinal3;
    };

} PM4CMDDRAWINDEXAUTO, *PPM4CMDDRAWINDEXAUTO;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWINDEXIMMD
{
    union
    {
        PM4_TYPE_3_HEADER     header;         ///< header
        unsigned int          ordinal1;
    };
    union
    {
        unsigned int          indexCount;     ///< max index count
        unsigned int          ordinal2;
    };
    union
    {
        regVGT_DRAW_INITIATOR drawInitiator;  ///< written to the VGT_DRAW_INITIATOR register
        unsigned int          ordinal3;
    };
    // Indices (16-bit or 32-bit) follow this structure, based on size in

} PM4CMDDRAWINDEXIMMD, *PPM4CMDDRAWINDEXIMMD;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWINDEXTYPE
{
    union
    {
        PM4_TYPE_3_HEADER   header;             ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    indexType   :  2;   ///< select 16 Vs 32bit index
            unsigned int    swapMode    :  2;   ///< DMA swap mode
        };
        unsigned int        ordinal2;
    };

} PM4CMDDRAWINDEXTYPE, *PPM4CMDDRAWINDEXTYPE;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWINDEXBASE
{
    union
    {
        PM4_TYPE_3_HEADER   header;             ///< PM4 type-3 header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        addrLo;             ///< Base Address Lo of index buffer, must be 2 byte aligned
        unsigned int        ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    addrHi      : 16;   ///< Base Address Hi of index buffer
            unsigned int    reserved1   : 14;
            unsigned int    baseSelect  :  2;   ///< Base Address select mode
        };
        unsigned int        ordinal3;
    };

} PM4CMDDRAWINDEXBASE, *PPM4CMDDRAWINDEXBASE;

#define BASE_SELECT_OFFSET  0x00    // address includes startIndexOffset
#define BASE_SELECT_BASE    0x01    // address doesn't include startIndexOffset

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDINDEXATTRIBUTESINDIRECT
{
    union
    {
        PM4_TYPE_3_HEADER   header;
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    reserved1 :  4;
            unsigned int    addressLo : 28;
        };
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        addressHi;
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int    attributeIndex : 16;
            unsigned int    reserved2      : 16;
        };
        unsigned int        ordinal4;
    };
} PM4CMDINDEXATTRIBUTESINDIRECT, *PPM4CMDINDEXATTRIBUTESINDIRECT;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWINDEXBUFFERSIZE
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< PM4 Type-3 Header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        numIndices;     ///< Number of indices contained in the index buffer
        unsigned int        ordinal2;
    };

} PM4CMDDRAWINDEXBUFFERSIZE, *PPM4CMDDRAWINDEXBUFFERSIZE;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWNUMINSTANCES
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        numInstances;   ///< number of Instances
        unsigned int        ordinal2;
    };

} PM4CMDDRAWNUMINSTANCES, *PPM4CMDDRAWNUMINSTANCES;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWSETBASE
{
    union
    {
        PM4_TYPE_3_HEADER   header;             ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    baseIndex   :  4;   ///< base index selector
            unsigned int    reserved1   : 28;
        };
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        addressLo;          ///< base address Lo of buffer, must be 8 byte aligned
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int    addressHi   : 16;   ///< base address Hi of buffer
            unsigned int    reserved2   : 16;
        };
        unsigned int        ordinal4;
    };
} PM4CMDDRAWSETBASE, *PPM4CMDDRAWSETBASE;

// Valid values for PM4CMDDRAWSETBASE::baseIndex
#define BASE_INDEX_DISPLAY_LIST           0x0000
#define BASE_INDEX_DRAW_INDIRECT          0x0001
#define BASE_INDEX_DISPATCH_INDIRECT      0x0001
#define BASE_INDEX_LOAD_REG               0x0004 // Used by LOAD_SH/CONTEXT_REG_INDEX
#define BASE_INDEX_INDIRECT_DATA          0x0005 // Used by SET_SH_REG_OFFSET index = 1

// Valid values for PM4CMDDRAWSETBASE::baseIndex constant engine packet
#define BASE_INDEX_CE_DST_BASE_ADDR       0x0002

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWINDIRECT
{
    union
    {
        PM4_TYPE_3_HEADER       header;         ///< header
        unsigned int            ordinal1;
    };
    union
    {
        unsigned int            dataOffset;     ///< DWORD aligned offset
        unsigned int            ordinal2;
    };
    union
    {
        struct
        {
            unsigned int        baseVtxLoc  :   16;     ///< base vertex location
            unsigned int        reserved1   :   16;
        };
        unsigned int            ordinal3;
    };
    union
    {
        struct
        {
            unsigned int        startInstLoc    :   16;   ///< start instance location
            unsigned int        reserved2       :   16;
        };
        unsigned int            ordinal4;
    };
    union
    {
        regVGT_DRAW_INITIATOR   drawInitiator;  ///< Draw Initiator Register
        unsigned int            ordinal5;
    };
} PM4CMDDRAWINDIRECT, *PPM4CMDDRAWINDIRECT;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWINDIRECTMULTI
{
    union
    {
        PM4_TYPE_3_HEADER       header;         ///< header
        unsigned int            ordinal1;
    };
    union
    {
        unsigned int            dataOffset;     ///< DWORD aligned offset
        unsigned int            ordinal2;
    };
    union
    {
        struct
        {
            unsigned int        baseVtxLoc  :   16;     ///< base vertex location
            unsigned int        reserved1   :   16;
        };
        unsigned int            ordinal3;
    };
    union
    {
        struct
        {
            unsigned int        startInstLoc    :   16;   ///< start instance location
            unsigned int        reserved2       :   16;
        };
        unsigned int            ordinal4;
    };
    union
    {
        struct
        {
            unsigned int        drawIndexLoc        :   16;   ///< register offset to write the Draw Index count
            unsigned int        reserved3           :   14;
            unsigned int        countIndirectEnable :    1;   ///< Indicates the data structure count is in memory
            unsigned int        drawIndexEnable     :    1;   ///< Enables writing of Draw Index count to DRAW_INDEX_LOC
        };
        unsigned int            ordinal5;
    };
    union
    {
        unsigned int            count;      ///< Count of data structures to loop through before going to next packet
        unsigned int            ordinal6;
    };
    union
    {
        unsigned int            countAddrLo;      ///< Lower bits of DWord aligned Address[31:2]; Valid if
                                                  ///  countIndirectEnable is set
        unsigned int            ordinal7;
    };
    union
    {
        unsigned int            countAddrHi;      ///< Upper bits of Address[63:32]; Valid if countIndirectEnable is set
        unsigned int            ordinal8;
    };
    union
    {
        unsigned int            stride;      ///< Stride in memory from one data structure to the next
        unsigned int            ordinal9;
    };
    union
    {
        regVGT_DRAW_INITIATOR   drawInitiator;  ///< Draw Initiator Register
        unsigned int            ordinal10;
    };
} PM4CMDDRAWINDIRECTMULTI, *PPM4CMDDRAWINDIRECTMULTI;

//-------------------------------------------------------------------------------------------------

// WAIT_REG_MEM space and function definitions
#define WAIT_REG_MEM_SPACE_REGISTER       0
#define WAIT_REG_MEM_SPACE_MEMORY         1
#define WAIT_REG_MEM_SPACE_TCL2__CI       2

#define WAIT_REG_MEM_FUNC_ALWAYS          0
#define WAIT_REG_MEM_FUNC_LESS            1
#define WAIT_REG_MEM_FUNC_LESS_EQUAL      2
#define WAIT_REG_MEM_FUNC_EQUAL           3
#define WAIT_REG_MEM_FUNC_NOT_EQUAL       4
#define WAIT_REG_MEM_FUNC_GREATER_EQUAL   5
#define WAIT_REG_MEM_FUNC_GREATER         6

#define WAIT_REG_MEM_ENGINE_ME            0
#define WAIT_REG_MEM_ENGINE_PFP           1
#define WAIT_REG_MEM_ENGINE_CE            2

typedef struct _PM4CMDWAITREGMEM
{
    union
    {
        PM4_TYPE_3_HEADER   header;          ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    function        : 3;  ///< function. WAIT_REG_MEM_FUNC_XXXX
            unsigned int    reserved1       : 1;  ///< reserved
            unsigned int    memSpace        : 2;  ///< memory space (0 = register, 1 = memory, 2=TC/L2, 3 = reserved)
            unsigned int    operation__CI   : 2;  ///< operation:
                                                  ///<    00: WAIT_REG_MEM - Wait on Masked Register/Memory value to equal reference value.
                                                  ///<    01: WR_WAIT_WR_REG (PFP only)
                                                  ///<            Writes REFERENCE value to POLL_ADDRESS_LO
                                                  ///<            Waits for REFERENCE = POLL_ADDRESS_HI
                                                  ///<            Write REFERENCE to POLL_ADDRESS_HI.
            unsigned int    engine          : 2;  ///< 0 = ME, 1 = PFP, 2 = CE
            unsigned int    uncached__VI    : 1;  ///< When set the memory read will always use MTYPE 3 (uncached)
                                                  ///  Only applies when executed on MEC (ACE).
                                                  ///  WAIT_REG_MEM on PFP or ME are always uncached.
            unsigned int    reserved2       : 13; ///< reserved
            unsigned int    atc__CI         : 1;  ///< ATC steting for MC read transactions
            unsigned int    cachePolicy__CI : 2;  ///< Reserved for future use of CACHE_POLICY
            unsigned int    volatile__CI    : 1;  ///< Reserved for future use of VOLATILE
            unsigned int    reserved3       : 4;  ///< reserved
        };
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        pollAddressLo;  ///< lower portion of Address to poll or register offset
        unsigned int        ordinal3;
    };
    union
    {
        unsigned int        pollAddressHi;  ///< high portion of Address to poll, dont care for regs
        unsigned int        ordinal4;
    };
    union
    {
        unsigned int        reference;      ///< reference value
        unsigned int        ordinal5;
    };
    union
    {
        unsigned int        mask;           ///< mask for comparison
        unsigned int        ordinal6;
    };
    union
    {
        unsigned int        pollInterval;   ///< interval to wait when issuing new poll requests
        unsigned int        ordinal7;
    };

} PM4CMDWAITREGMEM, *PPM4CMDWAITREGMEM;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDWAITREGMEM64
{
    union
    {
        PM4_TYPE_3_HEADER   header;          ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    function        : 3;  ///< function. WAIT_REG_MEM_FUNC_XXXX
            unsigned int    reserved1       : 1;  ///< reserved
            unsigned int    memSpace        : 2;  ///< memory space (0 = register, 1 = memory, 2=TC/L2, 3 = reserved)
            unsigned int    operation       : 2;  ///< operation:
                                                  ///<    00: WAIT_REG_MEM - Wait on Masked Register/Memory value to equal reference value.
                                                  ///<    01: WR_WAIT_WR_REG (PFP only)
                                                  ///<            Writes REFERENCE value to POLL_ADDRESS_LO
                                                  ///<            Waits for REFERENCE = POLL_ADDRESS_HI
                                                  ///<            Write REFERENCE to POLL_ADDRESS_HI.
            unsigned int    engineSel       : 2;  ///< 0 = ME, 1 = PFP, 2 = CE
            unsigned int    uncached__VI    : 1;  ///< When set the memory read will always use MTYPE 3 (uncached).
                                                  ///  Only applies when executed on MEC (ACE).
                                                  ///  WAIT_REG_MEM on PFP or ME are always uncached.
            unsigned int    reserved2       : 21;
        };
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        pollAddressLo;  ///< lower portion of Address to poll or register offset
        unsigned int        ordinal3;
    };
    union
    {
        unsigned int        pollAddressHi;  ///< high portion of Address to poll, dont care for regs
        unsigned int        ordinal4;
    };
    union
    {
        unsigned int        reference;      ///< reference value (low 32 bits)
        unsigned int        ordinal5;
    };
    union
    {
        unsigned int        referenceHi;    ///< reference value (upper 32 bits)
        unsigned int        ordinal6;
    };
    union
    {
        unsigned int        mask;           ///< mask for comparison (low 32 bits)
        unsigned int        ordinal7;
    };
    union
    {
        unsigned int        maskHi;           ///< mask for comparison (upper 32 bits)
        unsigned int        ordinal8;
    };
    union
    {
        unsigned int        pollInterval;   ///< interval to wait when issuing new poll requests
        unsigned int        ordinal9;
    };

} PM4CMDWAITREGMEM64, *PPM4CMDWAITREGMEM64;

//-------------------------------------------------------------------------------------------------

// EVENTWRITE and EVENTWRITEQUERY are essentially the same packet; the difference is that the CP
// only expects the "addressLo" and "addressHi" fields for the following events:
//     Sample_PipelineStats, Sample_StreamoutStats, and Zpass (Occlusion).
//
// Use EVENTWRITE for the events that don't require an address; use EVENTWRITEQUERY for the above-
// listed events only.
typedef struct _PM4CMDEVENTWRITE
{
    union
    {
        PM4_TYPE_3_HEADER   header;            ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    eventType        : 6;    ///< event type written to VGT_EVENT_INITIATOR
            unsigned int    reserved1        : 2;    ///< reserved
            unsigned int    eventIndex       : 4;    ///< event index
                                                     ///<      0000: Any non-Time Stamp/non-Fence/non-Trap EVENT_TYPE not listed.
                                                     ///<      0001: ZPASS_DONE
                                                     ///<      0010: SAMPLE_PIPELINESTATS
                                                     ///<      0011: SAMPLE_STREAMOUTSTAT[S|S1|S2|S3]
                                                     ///<      0100: [CS|VS|PS]_PARTIAL_FLUSH
                                                     ///<      0101: Reserved for EVENT_WRITE_EOP time stamp/fence event types
                                                     ///<      0110: Reserved for EVENT_WRITE_EOS packet
                                                     ///<      0111: CACHE_FLUSH, CACHE_FLUSH_AND_INV_EVENT
                                                     ///<      1000 - 1111: Reserved for future use.
            unsigned int    reserved2        : 8;    ///< reserved
            unsigned int    invalidateL2__SI : 1;    ///< Send WBINVL2 op to the TC L2 cache when eventIndex = 0111.
            unsigned int    reserved3        : 3;
            unsigned int    ATC__CI          : 1;   ///< needed by Sample_PipelineStats (compute engine)
            unsigned int    reserved4        : 6;   ///< reserved
            unsigned int    offload_enable   : 1;     ///< Offload queue until EOP queue goes empty, only works for MEC.
                                                    ///< Setting this bit on graphics/ME will do nothing/be masked out.
        };
        unsigned int        ordinal2;
    };

} PM4CMDEVENTWRITE, *PPM4CMDEVENTWRITE;

//-------------------------------------------------------------------------------------------------

// EVENTWRITE and EVENTWRITEQUERY are essentially the same packet; the difference is that the CP
// only expects the "addressLo" and "addressHi" fields for the following events:
//     Sample_PipelineStats, Sample_StreamoutStats, and Zpass (Occlusion).
//
// Use EVENTWRITE for the events that don't require an address; use EVENTWRITEQUERY for the above-
// listed events only.
typedef struct _PM4CMDEVENTWRITEQUERY
{
    union
    {
        PM4_TYPE_3_HEADER   header;            ///< header
        unsigned int        ordinal1;
    };
    union
    {
        // While this is basically the EVENT_WRITE packet, the "invalidateL2" bit is not present
        // in this structure because that bit should only be set for cache_flush events, and the
        // query packet (i.e., this one) should only be used for the sample_* and zpass events.
        struct
        {
            unsigned int    eventType  : 6;    ///< event type written to VGT_EVENT_INITIATOR
            unsigned int    reserved1  : 2;    ///< reserved
            unsigned int    eventIndex : 4;    ///< event index
            unsigned int    reserved2  : 20;   ///< reserved
        };
        unsigned int        ordinal2;
    };
    // ordinal3 and ordinal4 should be supplied only for the following events:
    // Sample_PipelineStats, Sample_StreamoutStats, and Zpass (Occlusion).
    union
    {
        struct
        {
            unsigned int    reserved4           : 3;
            unsigned int    counterId__CI       : 6;
            unsigned int    stride__CI          : 2;
            unsigned int    instanceEnable__CI  : 16;
            unsigned int    reserved5           : 5;
        };
        unsigned int        addressLo;   ///< low address bits, must be 8 byte aligned
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int    addressHi   : 16;   ///< hi address bits
            unsigned int    reserved3   : 16;   ///< reserved
        };
        unsigned int        addressHi32;        ///< hi address bits
        unsigned int        ordinal4;
    };

} PM4CMDEVENTWRITEQUERY, *PPM4CMDEVENTWRITEQUERY;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDEVENTWRITEEOP
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    eventType             : 6;    ///< event type written to VGT_EVENT_INITIATOR
            unsigned int    reserved1             : 2;    ///< reserved
            unsigned int    eventIndex            : 4;    ///< event index
            unsigned int    tcl1VolActionEna__CI  : 1;    ///<
            unsigned int    tcVolActionEna__CI    : 1;    ///<
            unsigned int    reserved2             : 1;
            unsigned int    tcWbActionEna__CI     : 1;    ///<
            unsigned int    tcl1ActionEna__CI     : 1;    ///<
            unsigned int    tcActionEna__CI       : 1;
            unsigned int    reserved3             : 2;
            unsigned int    invalidateL2__SI      : 1;
            unsigned int    reserved4             : 3;
            unsigned int    atc__CI               : 1;
            unsigned int    cachePolicy__CI       : 2;    ///< Cache Policy setting used for writing fences and timestamps to the TCL2
            unsigned int    volatile__CI          : 1;    ///< Volatile setting used for writing fences and timestamps to the TCL2.
            unsigned int    reserved5             : 4;
        };
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        addressLo; ///< low bits of address
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int    addressHi  : 16;    ///< high bits of address
            unsigned int    reserved6  : 8;   ///< reserved
            unsigned int    intSel     : 2;    ///< selects interrupt action for end-of-pipe
            unsigned int    reserved7  : 3;    ///< reserved
            unsigned int    dataSel    : 3;    ///< selects source of data
        };
        unsigned int        ordinal4;
    };
    union
    {
        unsigned int        dataLo;        ///< value that will be written to memory when event occurs
        unsigned int        ordinal5;
    };
    union
    {
        unsigned int        dataHi;        ///< value that will be written to memory when event occurs
        unsigned int        ordinal6;
    };
} PM4CMDEVENTWRITEEOP, *PPM4CMDEVENTWRITEEOP;

//-------------------------------------------------------------------------------------------------

// EVENT_WRITE_EOP packet definitions
#define EVENTWRITEEOP_DATA_SEL_DISCARD              0
#define EVENTWRITEEOP_DATA_SEL_SEND_DATA32          1
#define EVENTWRITEEOP_DATA_SEL_SEND_DATA64          2
#define EVENTWRITEEOP_DATA_SEL_SEND_GPU_CLOCK       3

#define EVENTWRITEEOP_INT_SEL_NONE                  0
#define EVENTWRITEEOP_INT_SEL_SEND_INT              1
#define EVENTWRITEEOP_INT_SEL_SEND_INT_ON_CONFIRM   2
#define EVENTWRITEEOP_INT_SEL_SEND_DATA_ON_CONFIRM  3

#define EVENT_WRITE_INDEX_ANY_NON_TIMESTAMP         0
#define EVENT_WRITE_INDEX_ZPASS_DONE                1
#define EVENT_WRITE_INDEX_SAMPLE_PIPELINESTAT       2
#define EVENT_WRITE_INDEX_SAMPLE_STREAMOUTSTATS     3
#define EVENT_WRITE_INDEX_VS_PS_PARTIAL_FLUSH       4
#define EVENT_WRITE_INDEX_ANY_EOP_TIMESTAMP         5
#define EVENT_WRITE_INDEX_ANY_EOS_TIMESTAMP         6
#define EVENT_WRITE_INDEX_CACHE_FLUSH_EVENT         7
#define EVENT_WRITE_INDEX_INVALID                   0xffffffff

//-------------------------------------------------------------------------------------------------
#define STRMOUT_CNTL_OFFSET_SEL_EXPLICT_OFFSET               0
#define STRMOUT_CNTL_OFFSET_SEL_READ_VGT_BUFFER_FILLED_SIZE  1
#define STRMOUT_CNTL_OFFSET_SEL_READ_SRC_ADDRESS             2
#define STRMOUT_CNTL_OFFSET_SEL_NONE                         3

typedef struct _PM4CMDSTRMOUTBUFFERUPDATE
{
    union
    {
        PM4_TYPE_3_HEADER   header;                      ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    storeBufferFilledSize  : 1;  ///< store BufferFilledSize to memory
            unsigned int    offsetSourceSelect     : 2;  ///< offset source select to write into
                                                         ///< VGT_STRMOUT_BUFFER_OFFSET
            unsigned int    reserved1              : 4;  ///< reserved
            unsigned int    dataType               : 1;  ///< Data type of the value in memory
                                                         ///< 0 = DWORD's, 1 = bytes
            unsigned int    bufferSelect           : 2;  ///< buffer select
            unsigned int    reserved2              : 22; ///< reserved
        };
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        dstAddressLo;          ///< low address bits, must be 4 bytes aligned
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int    dstAddressHi   : 16;    ///< high bits of dst address
            unsigned int    reserved3      : 16;   ///< reserved
        };
        unsigned int        ordinal4;
    };
    union
    {
        unsigned int        bufferOffset;          ///< if offsetSourceSelect is 0x0 contains buffer offset to
                                                   ///< write to VGT_STRMOUT_BUFFER_OFFSET
        unsigned int        srcAddressLo;          ///< contains low bits of source address when
                                                   ///< offsetSourceSelect is 0x1
        unsigned int        ordinal5;
    };
    union
    {
        // Ordinal valid only if Source Select is "10".
        struct
        {
            unsigned int    srcAddressHi   : 16;    ///< hi bits of source address
            unsigned int    reserved4      : 16;   ///< reserved
        };
        unsigned int        ordinal6;
    };

} PM4CMDSTRMOUTBUFFERUPDATE, *PPM4CMDSTRMOUTBUFFERUPDATE;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDSETPREDICATION
{
    union
    {
        PM4_TYPE_3_HEADER   header;                 ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        startAddressLo;         ///< start address low
        unsigned int        ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    startAddrHi        : 8;  ///< start address hi
            unsigned int    predicationBoolean : 1;  ///< predication boolean
            unsigned int    reserved1          : 3;  ///< reserved
            unsigned int    hint               : 1;  ///< hint
                                                     ///< (only valid for Zpass/Occlusion Predicate)
            unsigned int    reserved2          : 3;  ///< reserved
            unsigned int    predOp             : 3;  ///< predicate operation
            unsigned int    reserved3          : 12; ///< reserved
            unsigned int    continueBit        : 1;  ///< continue set predication
        };
        unsigned int        ordinal3;
        unsigned int        predProperties;
    };

} PM4CMDSETPREDICATION, *PPM4CMDSETPREDICATION;

#define SET_PRED_CLEAR               0
#define SET_PRED_ZPASS               1
#define SET_PRED_PRIMCOUNT           2
#define SET_PRED_MEM                 3

//-------------------------------------------------------------------------------------------------

// PM4CMDMEMSEMAPHORE - values for fields
#define MEM_SEMA_SIGNAL 0x6
#define MEM_SEMA_WAIT   0x7

#define MEM_SEMA_CP     0x00
#define MEM_SEMA_CB     0x01
#define MEM_SEMA_DB     0x10

typedef struct _PM4CMDMEMSEMAPHORE
{
    union
    {
        PM4_TYPE_3_HEADER   header;              ///< header
        unsigned int        ordinal1;
    };

    union
    {
        unsigned int        addrLo;              ///< low bits of the address
        unsigned int        ordinal2;
    };

    union
    {
        struct
        {
            unsigned int addrHi             : 8; ///< upper bits of the address
            unsigned int reserved1          : 4;
            unsigned int waitOnSignal       : 1; ///< deprecated on SI?
            unsigned int reserved2          : 3;
            unsigned int useMailbox         : 1; ///< 0 -  Signal Semaphore will not wait for
                                                 ///<      mailbox to be written
                                                 ///< 1 -  Signal Semaphore will wait for mailbox
                                                 ///<      to be written
            unsigned int reserved3          : 3; ///< reserved
            unsigned int signalType         : 1; ///< signal Type
                                                 ///< 0 - SEM_SEL = Signal Semaphore and signal type
                                                 ///< is increment, or the SEM_SEL = Wait Semaphore.
                                                 ///< 1 - SEM_SEL = Signal Semaphore and signal
                                                 ///< type is write '1'.
            unsigned int reserved4          : 3; ///< reserved
            unsigned int clientCode         : 2; ///< values can be MEM_SEMA_[CP|CB|DB]
            unsigned int reserved5          : 3; ///< reserved
            unsigned int semSel             : 3; ///< this is a multi-bit field to be DW
                                                 ///< compatible with EVENT_WRITE_EOP
                                                 ///< values can be MEM_SEMA_[SIGNAL|WAIT]
        } SI;
        struct
        {
            unsigned int addrHi             : 16; ///< upper bits of the address
            unsigned int useMailbox         : 1; ///< 0 -  Signal Semaphore will not wait for
                                                 ///<      mailbox to be written
                                                 ///< 1 -  Signal Semaphore will wait for mailbox
                                                 ///<      to be written
            unsigned int reserved1          : 3; ///< reserved
            unsigned int signalType         : 1; ///< signal Type
                                                 ///< 0 - SEM_SEL = Signal Semaphore and signal type
                                                 ///< is increment, or the SEM_SEL = Wait Semaphore.
                                                 ///< 1 - SEM_SEL = Signal Semaphore and signal
                                                 ///< type is write '1'.
            unsigned int reserved2          : 3; ///< reserved
            unsigned int clientCode         : 2; ///< values can be MEM_SEMA_[CP|CB|DB]
            unsigned int reserved3          : 3; ///< reserved
            unsigned int semSel             : 3; ///< this is a multi-bit field to be DW
                                                 ///< compatible with EVENT_WRITE_EOP
                                                 ///< values can be MEM_SEMA_[SIGNAL|WAIT]
        } CI;
        unsigned int     ordinal3;
    };
} PM4CMDMEMSEMAPHORE, *PPM4CMDMEMSEMAPHORE;

//-------------------------------------------------------------------------------------------------
#define SURFACE_SYNC_ENGINE_PFP           0
#define SURFACE_SYNC_ENGINE_ME            1

typedef struct _PM4CMDSURFACESYNC
{
    union
    {
        PM4_TYPE_3_HEADER   header;               ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    coherCntl : 31;
            unsigned int    engine    :  1;
        };
        unsigned int        ordinal2;
    };
    union
    {
        regCP_COHER_SIZE    cpCoherSize;
        unsigned int        ordinal3;
    };
    union
    {
        regCP_COHER_BASE    cpCoherBase;
        unsigned int        ordinal4;
    };
    union
    {
        unsigned int        pollInterval;
        unsigned int        ordinal5;
    };
} PM4CMDSURFACESYNC, *PPM4CMDSURFACESYNC;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDISPATCHDIRECT
{
    union
    {
        PM4_TYPE_3_HEADER   header;     ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int dimX;      ///< X dimensions of the array of thread groups to be dispatched
        unsigned int ordinal2;
    };
    union
    {
        unsigned int dimY;      ///< Y dimensions of the array of thread groups to be dispatched
        unsigned int ordinal3;
    };
    union
    {
        unsigned int dimZ;      ///< Z dimensions of the array of thread groups to be dispatched
        unsigned int ordinal4;
    };
    union
    {
        regCOMPUTE_DISPATCH_INITIATOR dispatchInitiator; ///< Dispatch Initiator Register
        unsigned int ordinal5;
    };
} PM4CMDDISPATCHDIRECT, *PPM4CMDDISPATCHDIRECT;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDISPATCHINDIRECT
{
    union
    {
        PM4_TYPE_3_HEADER   header;     ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int dataOffset;  ///< DWORD aligned offset
        unsigned int ordinal2;
    };
    union
    {
        regCOMPUTE_DISPATCH_INITIATOR dispatchInitiator; ///< Dispatch Initiator Register
        unsigned int ordinal3;
    };
} PM4CMDDISPATCHINDIRECT, *PPM4CMDDISPATCHINDIRECT;

//-------------------------------------------------------------------------------------------------

/// @note ASICs with a MEC engine (CI+) use a different version of the DISPATCH_INDIRECT packet on
///       their async compute queues. This MEC-only version does not require a SET_BASE packet;
///       instead the DISPATCH_INDIRECT packet contains the full address of the args buffer.
typedef struct _PM4CMDDISPATCHINDIRECTMEC
{
    union
    {
        PM4_TYPE_3_HEADER header; ///< header
        unsigned int ordinal1;
    };
    union
    {
        unsigned int addressLo;   ///< DWORD aligned address
        unsigned int ordinal2;
    };
    union
    {
        unsigned int addressHi;
        unsigned int ordinal3;
    };
    union
    {
        regCOMPUTE_DISPATCH_INITIATOR dispatchInitiator; ///< Dispatch Initiator Register
        unsigned int ordinal4;
    };
} PM4CMDDISPATCHINDIRECTMEC, *PPM4CMDDISPATCHINDIRECTMEC;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDCLEARSTATE
{
    union
    {
        PM4_TYPE_3_HEADER   header;     ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int dummyData;  ///< Dummy data
        unsigned int ordinal2;
    };
} PM4CMDCLEARSTATE, *PPM4CMDCLEARSTATE;

//-------------------------------------------------------------------------------------------------

#define EVENT_WRITE_EOS_INDEX_CSDONE_PSDONE 6

#define EVENT_WRITE_EOS_CMD_STORE_APPEND_COUNT_TO_MEMORY  0
#define EVENT_WRITE_EOS_CMD_STORE_GDS_DATA_TO_MEMORY      1
#define EVENT_WRITE_EOS_CMD_STORE_32BIT_DATA_TO_MEMORY    2

typedef struct _PM4CMDEVENTWRITEEOS
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int eventType  : 6;    ///< event type written to VGT_EVENT_INITIATOR
            unsigned int reserved1  : 2;    ///< reserved
            unsigned int eventIndex : 4;    ///< event index
            unsigned int reserved2  : 20;   ///< reserved
        };
        unsigned int     ordinal2;
    };
    union
    {
        unsigned int addressLo;             ///< low bits of address, must be 4 byte aligned
        unsigned int ordinal3;
    };
    union
    {
        struct
        {
            unsigned int addressHi  : 16;   ///< high bits of address
            unsigned int reserved3  : 13;   ///< reserved
            unsigned int command    : 3;    ///< command
        };
        unsigned int     ordinal4;
    };
    union
    {
        struct
        {
            unsigned int gdsIndex    :16;   ///< indexed offset into GDS partition
            unsigned int size        :16;   ///< number of DWs to read from the GDS
        };
        unsigned int     data;              ///< fence value that will be written to memory when event occurs
        unsigned int     ordinal5;
    };
} PM4CMDEVENTWRITEEOS, *PPM4CMDEVENTWRITEEOS;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDSCRATCHRAMWRITE
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    scratchAddr     :   8;    ///< address in scratch RAM to write to
            unsigned int    reserved1       :  22;    ///< reserved for future expansion
                                                      ///  of scratched RAM address field
            unsigned int    engSel          :   2;
        };
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        scratchData;    ///< data to write to scratch RAM
        unsigned int        ordinal3;
    };
} PM4CMDSCRATCHRAMWRITE, *PPM4CMDSCRATCHRAMWRITE;

//-------------------------------------------------------------------------------------------------
// Use for WRITE_CONST_RAM
//         WRITE_CONST_RAM_OFFSET
//         WRITE_CONST_RAM_INDIRECT
typedef struct _PM4CMDCONSTRAMWRITE
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    offset    : 16;    ///< DWord offset in the constant RAM, must
                                               ///< be 4 byte aligned
            unsigned int    reserved1 : 16;
        };
        unsigned int        ordinal2;
    };
    // This is a variable length packet. So, based on size in header, the layout following this
    // looks as follows:
    // unsigned int data0;
    // ...
    // unsigned int dataN;
} PM4CMDCONSTRAMWRITE, *PPM4CMDCONSTRAMWRITE;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDCONSTRAMDUMP
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    offset          : 16; ///< Byte offset in the RAM (bits 1-0 must be 0)
            unsigned int    reserved1       :  9;
            unsigned int    cachePolicy__CI :  2; ///< Cache policy: 0 : LRU, 1: Stream, 2: Bypass
            unsigned int    reserved2       :  3;
            unsigned int    incrementCs__VI :  1; ///< Increment CS counter at end of packet
            unsigned int    incrementCe__VI :  1; ///< Increment CE counter at end of packet
        };
        unsigned int        ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    numDwords : 15; ///< number of DWords to dump
            unsigned int    reserved3 : 17;
        };
        unsigned int        ordinal3;
    };
    union
    {
        unsigned int        addrLo;         ///< Low address bits, must be 4 byte aligned
        unsigned int        ordinal4;
    };
    union
    {
        unsigned int        addrHi;         ///< High address bits
        unsigned int        ordinal5;
    };
} PM4CMDCONSTRAMDUMP, *PPM4CMDCONSTRAMDUMP;

//-------------------------------------------------------------------------------------------------
// The following packet is only supported on VI and later ASICs
typedef struct _PM4CMDCONSTRAMDUMPOFFSET
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
           unsigned int    offset      : 16; ///< Byte offset in the RAM (bits 1-0 must be 0)
           unsigned int    reserved1   :  9;
           unsigned int    cachePolicy :  2; ///< Cache policy: 0 : LRU, 1: Stream, 2: Bypass
           unsigned int    reserved2   :  3;
           unsigned int    incrementCs :  1; ///< Increment CS counter at end of packet
           unsigned int    incrementCe :  1; ///< Increment CE counter at end of packet
        };
        unsigned int        ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    numDwords : 15; ///< number of DWords to dump
            unsigned int    reserved3 : 17;
        };
        unsigned int        ordinal3;
    };
    union
    {
        unsigned int        addrOffset;     ///< Indirect offset
        unsigned int        ordinal4;
    };
} PM4CMDCONSTRAMDUMPOFFSET, *PPM4CMDCONSTRAMDUMPOFFSET;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDCONSTRAMLOAD
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        addrLo;         ///< Low address bits, must be 32 byte aligned
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        addrHi;         ///< High address bits
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int    numDwords : 15; ///< number of DWords to load (bits 2-0 must be 0)
            unsigned int    reserved1 : 17;
        };
        unsigned int        ordinal4;
    };
    union
    {
        struct
        {
            unsigned int    offset    : 16; ///< Byte offset in the RAM, must ve 32 byte aligned
            unsigned int    reserved2 : 16;
        };
        unsigned int        ordinal5;
    };
} PM4CMDCONSTRAMLOAD, *PPM4CMDCONSTRAMLOAD;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDINCCECOUNTER
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    incCEcounter :  1;
            unsigned int    incCScounter :  1;
            unsigned int    reserved1    : 30;
        };
        unsigned int        dummyData;      ///< dummy DWORD, kept for compatability reasons
        unsigned int        ordinal2;
    };
} PM4CMDINCCECOUNTER, *PPM4CMDINCCECOUNTER;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDINCDECOUNTER
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        dummyData;      ///< dummy DWORD
        unsigned int        ordinal2;
    };

} PM4CMDINCDECOUNTER, *PPM4CMDINCDECOUNTER;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDSETCEDECOUNTERS
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        counterLo;      ///< low 32bits of the counter
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        counterHi;      ///< high 32bits of the counter
        unsigned int        ordinal3;
    };
} PM4CMDSETCEDECOUNTERS, *PPM4CMDSETCEDECOUNTERS;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDWAITONAVAILBUFFER
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        dummyData;      ///< dummy DWORD
        unsigned int        ordinal2;
    };
} PM4CMDWAITONAVAILBUFFER, *PPM4CMDWAITONAVAILBUFFER;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDWAITONCECOUNTER
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    control      :  1; ///< Conditional Surface Sync for wrapping CE buffers
            unsigned int    forceSync    :  1;
            unsigned int    reserved1    : 25;
            unsigned int    volatile__CI :  1;
            unsigned int    reserved2    :  4;
        };
        unsigned int        ordinal2;
    };

} PM4CMDWAITONCECOUNTER, *PPM4CMDWAITONCECOUNTER;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDWAITONDECOUNTERDIFF
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        counterDiff;    ///< diff counter value
        unsigned int        ordinal2;
    };
} PM4CMDWAITONDECOUNTERDIFF, *PPM4CMDWAITONDECOUNTERDIFF;

//-------------------------------------------------------------------------------------------------

// Packet description for
//  INDIRECT_BUFFER_CONST
//  INDIRECT_BUFFER

#define INDIRECT_BUFFER_CACHE_POLICY_LRU      0
#define INDIRECT_BUFFER_CACHE_POLICY_STREAM   1
#define INDIRECT_BUFFER_CACHE_POLICY_BYPASS   2

typedef struct _PM4CMDINDIRECTBUFFER
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        ibBaseLo;       ///< Indirect buffer base address, must be 4 byte aligned
        unsigned int        ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    ibBaseHi  : 16;       ///< Indirect buffer base address
            unsigned int    reserved1 : 16;
        };
        unsigned int        ibBaseHi32;           ///< Indirect buffer base address
        unsigned int        ordinal3;
    };
    union
    {
        union
        {
            struct {
                unsigned int    ibSize      : 20;    ///< Indirect buffer size
                unsigned int    chain       :  1;    ///< set to chain to IB allocations
                unsigned int    reserved1   :  3;    ///< reserved
                unsigned int    vmid        :  8;    ///< Virtual memory domain ID for command buffer
            } SI;

            struct {
                unsigned int    ibSize         : 20;    ///< Indirect buffer size
                unsigned int    chain          : 1;
                unsigned int    offLoadPolling : 1;
                unsigned int    volatile__CI   : 1;
                unsigned int    valid          : 1;
                unsigned int    vmid           : 4;
                unsigned int    cachePolicy    : 2;
                unsigned int    reserved1      : 2;
            } CI;

            struct {
                unsigned int    ibSize       : 20;      ///< Indirect buffer size
                unsigned int    chain        : 1;
                unsigned int    preEna       : 1;
                unsigned int    reserved     : 1;
                unsigned int    valid        : 1;
                unsigned int    vmid         : 4;
                unsigned int    cachePolicy  : 2;
                unsigned int    preResume    : 1;
                unsigned int    reserved1    : 1;
            } VI;
        };
        unsigned int        ordinal4;
    };
} PM4CMDINDIRECTBUFFER, *PPM4CMDINDIRECTBUFFER;

//-------------------------------------------------------------------------------------------------

// Packet description for COND_INDIRECT_BUFFER.

#define COND_INDIRECT_BUFFER_MODE_IF        1  ///< if(cmp func ref){IB1}
#define COND_INDIRECT_BUFFER_MODE_IF_ELSE   2  ///< if(cmp func ref){IB1} else{IB2}

#define COND_INDIRECT_BUFFER_FUNC_ALWAYS          0
#define COND_INDIRECT_BUFFER_FUNC_LESS            1
#define COND_INDIRECT_BUFFER_FUNC_LESS_EQUAL      2
#define COND_INDIRECT_BUFFER_FUNC_EQUAL           3
#define COND_INDIRECT_BUFFER_FUNC_NOT_EQUAL       4
#define COND_INDIRECT_BUFFER_FUNC_GREATER_EQUAL   5
#define COND_INDIRECT_BUFFER_FUNC_GREATER         6

#define COND_INDIRECT_BUFFER_CACHE_POLICY_LRU      0
#define COND_INDIRECT_BUFFER_CACHE_POLICY_STREAM   1

typedef struct _PM4CMDCONDINDIRECTBUFFER
{
    union
    {
        PM4_TYPE_3_HEADER   header;          ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    mode      :  2;  ///< jump mode
            unsigned int    reserved1 :  6;  ///< reserved
            unsigned int    function  :  3;  ///< compare function
            unsigned int    reserved2 : 21;  ///< reserved
        };
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int        compareAddrLo;   ///< cmp data addr low bits, must be 8 byte aligned
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int    compareAddrHi : 16; ///< cmp data address high bits
            unsigned int    reserved3     : 16; ///< reserved
        };
        unsigned int        ordinal4;
    };
    union
    {
        unsigned int        maskLo;          ///< mask low bits
        unsigned int        ordinal5;
    };
    union
    {
        unsigned int        maskHi;          ///< mask high bits
        unsigned int        ordinal6;
    };
    union
    {
        unsigned int        referenceLo;     ///< reference value low bits
        unsigned int        ordinal7;
    };
    union
    {
        unsigned int        referenceHi;     ///< reference value high bits
        unsigned int        ordinal8;
    };
    union
    {
        unsigned int        ibBase1Lo;       ///< IB1 base address low bits, must be 4 byte aligned
        unsigned int        ordinal9;
    };
    union
    {
        struct
        {
            unsigned int    ibBase1Hi : 16;  ///< IB1 base address high bits
            unsigned int    reserved4 : 16;  ///< reserved
        };
        unsigned int        ordinal10;
    };
    union
    {
        struct
        {
            unsigned int    ibSize1      : 20; ///< size of IB1 in DWORDs
            unsigned int    reserved5    :  8; ///< reserved, must be zero
            unsigned int    cachePolicy1 :  2; ///< cache policy for KMD initiated IB1
            unsigned int    reserved6    :  2; ///< reserved, must be zero
        };
        unsigned int        ordinal11;
    };
    union
    {
        unsigned int        ibBase2Lo;       ///< IB2 base address low bits, must be 4 byte aligned
        unsigned int        ordinal12;
    };
    union
    {
        struct
        {
            unsigned int    ibBase2Hi : 16;  ///< IB2 base address high bits
            unsigned int    reserved7 : 16;  ///< reserved
        };
        unsigned int        ordinal13;
    };
    union
    {
        struct
        {
            unsigned int    ibSize2      : 20; ///< size of IB2 in DWORDs
            unsigned int    reserved8    :  8; ///< reserved, must be zero
            unsigned int    cachePolicy2 :  2; ///< cache policy for KMD initiated IB2
            unsigned int    reserved9    :  2; ///< reserved, must be zero
        };
        unsigned int        ordinal14;
    };
} PM4CMDCONDINDIRECTBUFFER, *PPM4CMDCONDINDIRECTBUFFER;

//-------------------------------------------------------------------------------------------------
#define SET_SH_REG_OFFSET_INDEX_NORMAL        0
#define SET_SH_REG_OFFSET_INDEX_DATA_INDIRECT 1
typedef struct _PM4CMDSETSHREGOFFSET
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    regOffset   :   16;
            unsigned int    reserved1   :   15;
            unsigned int    index__VI   :    1; ///< 0: normal operation, 1: indirect data
        };
        unsigned int        ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    dwIbOffset  :   22; ///< offset in DWs from indirect command buffer
            unsigned int    reserved2   :   10;
        };
        unsigned int        dataOffset__VI;     ///< Data offset for index__VI = 1
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int    reserved3   :   16;
            unsigned int    driverData  :   16;     ///< data supplied by driver
        };
        unsigned int        ordinal4;
    };

} PM4CMDSETSHREGOFFSET, *PPM4CMDSETSHREGOFFSET;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWINDEXINDIRECT
{
    union
    {
        PM4_TYPE_3_HEADER       header;             ///< header
        unsigned int            ordinal1;
    };
    union
    {
        unsigned int            dataOffset;         ///< DWORD aligned offset
        unsigned int            ordinal2;
    };
    union
    {
        struct
        {
            unsigned int        baseVtxLoc   : 16;  ///< base vertex location
            unsigned int        reserved1    : 16;
        };
        unsigned int            ordinal3;
    };
    union
    {
        struct
        {
            unsigned int        startInstLoc : 16;  ///< start instance location
            unsigned int        reserved2    : 16;
        };
        unsigned int            ordinal4;
    };
    union
    {
        regVGT_DRAW_INITIATOR   drawInitiator;      ///< Draw Initiator Register
        unsigned int            ordinal5;
    };
} PM4CMDDRAWINDEXINDIRECT, *PPM4CMDDRAWINDEXINDIRECT;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWINDEXINDIRECTMULTI
{
    union
    {
        PM4_TYPE_3_HEADER       header;             ///< header
        unsigned int            ordinal1;
    };
    union
    {
        unsigned int            dataOffset;         ///< DWORD aligned offset
        unsigned int            ordinal2;
    };
    union
    {
        struct
        {
            unsigned int        baseVtxLoc   : 16;  ///< base vertex location
            unsigned int        reserved1    : 16;
        };
        unsigned int            ordinal3;
    };
    union
    {
        struct
        {
            unsigned int        startInstLoc : 16;  ///< start instance location
            unsigned int        reserved2    : 16;
        };
        unsigned int            ordinal4;
    };
    union
    {
        struct
        {
            unsigned int        drawIndexLoc        :   16;   ///< register offset to write the Draw Index count
            unsigned int        reserved3           :   14;
            unsigned int        countIndirectEnable :    1;   ///< Indicates the data structure count is in memory
            unsigned int        drawIndexEnable     :    1;   ///< Enables writing of Draw Index count to DRAW_INDEX_LOC
        };
        unsigned int            ordinal5;
    };
    union
    {
        unsigned int            count;      ///< Count of data structures to loop through before going to next packet
        unsigned int            ordinal6;
    };
    union
    {
        unsigned int            countAddrLo;      ///< Lower bits of DWord aligned Address[31:2]; Valid if
                                                  ///  countIndirectEnable is set
        unsigned int            ordinal7;
    };
    union
    {
        unsigned int            countAddrHi;      ///< Upper bits of Address[63:32]; Valid if countIndirectEnable is set
        unsigned int            ordinal8;
    };
    union
    {
        unsigned int            stride;      ///< Stride in memory from one data structure to the next
        unsigned int            ordinal9;
    };
    union
    {
        regVGT_DRAW_INITIATOR   drawInitiator;  ///< Draw Initiator Register
        unsigned int            ordinal10;
    };
} PM4CMDDRAWINDEXINDIRECTMULTI, *PPM4CMDDRAWINDEXINDIRECTMULTI;

//-------------------------------------------------------------------------------------------------
typedef union _PM4_DRAW_CONTROL
{
    struct
    {
        unsigned int indexOffset  : 16; ///< starting index for this primitive
        unsigned int primType     :  5; ///< primitive type
        unsigned int indexCount   : 11; ///< number of indices to generate for this prim
    };
    unsigned int u32All;

} PM4_DRAW_CONTROL;

//-------------------------------------------------------------------------------------------------
typedef struct _PM4CMDDRAWINDEXMULTIAUTO
{
    union
    {
        PM4_TYPE_3_HEADER     header;         ///< header
        unsigned int          ordinal1;
    };
    union
    {
        unsigned int          primCount;      ///< primitive count
        unsigned int          ordinal2;
    };
    union
    {
        regVGT_DRAW_INITIATOR drawInitiator;  ///< written to the VGT_DRAW_INITIATOR register
        unsigned int          ordinal3;
    };
    union
    {
        PM4_DRAW_CONTROL      control;
        unsigned int          ordinal4;
    };

} PM4CMDDRAWINDEXMULTIAUTO, *PPM4CMDDRAWINDEXMULTIAUTO;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDDRAWMPEGINDEX
{
    union
    {
        PM4_TYPE_3_HEADER       header;         ///< header
        unsigned int            ordinal1;
    };
    union
    {
        unsigned int            numIndices;     ///< number of indices
        unsigned int            ordinal2;
    };
    union
    {
        regVGT_DRAW_INITIATOR   drawInitiator;  ///< Draw Initiator Register
        unsigned int            ordinal3;
    };
    union
    {
        unsigned int            rectIndex;      ///< first index of rect.
        unsigned int            ordinal4;
    };
} PM4CMDDRAWMPEGINDEX, *PPM4CMDDRAWMPEGINDEX;

//-------------------------------------------------------------------------------------------------
typedef struct _PM4CMDCONDEXEC
{
    union
    {
        PM4_TYPE_3_HEADER header;       ///< header
        unsigned int    ordinal1;
    };
    union
    {
        unsigned int    boolAddrLo;     ///< boolean address bits, must be 4 byte aligned
        unsigned int    ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    boolAddrHi  : 16;
            unsigned int    reserved1   : 12;
            unsigned int    command     :  4;   ///< command
        };
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int    execCount  : 14; ///< total number of DWs of subsequent pred packet
            unsigned int    reserved2  : 18;
        };
        unsigned int    ordinal4;
    };

} PM4CMDCONDEXEC, *PPM4CMDCONDEXEC;

//-------------------------------------------------------------------------------------------------
typedef struct _PM4CMDCONDEXEC_CI
{
    union
    {
        PM4_TYPE_3_HEADER header;       ///< header
        unsigned int    ordinal1;
    };
    union
    {
        unsigned int    boolAddrLo;     ///< boolean address bits, must be 4 byte aligned
        unsigned int    ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    boolAddrHi  : 16;
            unsigned int    reserved1   : 12;
            unsigned int    command     :  4; ///< This is actually part of "reserved1" but has been
                                              ///  kept for backwards compatibility.
        };
        unsigned int        boolAddrHi32;
        unsigned int        ordinal3;
    };
    union
    {
        struct
        {
            unsigned int   reserved2   : 28;
            unsigned int   control     :  4; ///< 0=discard, 1 = stall (proposed), others = reserved
        };
        unsigned int       ordinal4;
    };
    union
    {
        struct
        {
            unsigned int   execCount  : 14; ///< total number of DWs of subsequent pred packet
            unsigned int   reserved3  : 18;
        };
        unsigned int       ordinal5;
    };

} PM4CMDCONDEXEC_CI, *PPM4CMDCONDEXEC_CI;

//-------------------------------------------------------------------------------------------------
// COND_WRITE write_space, poll_space, and function definitions
#define COND_WRITE_SPACE_REGISTER       0
#define COND_WRITE_SPACE_MEMORY         1

#define COND_WRITE_POLL_SPACE_REGISTER  0
#define COND_WRITE_POLL_SPACE_MEMORY    1

#define COND_WRITE_FUNC_ALWAYS          0
#define COND_WRITE_FUNC_LESS            1
#define COND_WRITE_FUNC_LESS_EQUAL      2
#define COND_WRITE_FUNC_EQUAL           3
#define COND_WRITE_FUNC_NOT_EQUAL       4
#define COND_WRITE_FUNC_GREATER_EQUAL   5
#define COND_WRITE_FUNC_GREATER         6

typedef struct _PM4CMDCONDWRITE
{
    union
    {
        PM4_TYPE_3_HEADER header;   ///< header
        unsigned int    ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    function    :   3;  ///< compare function
            unsigned int    reserved1   :   1;
            unsigned int    pollSpace   :   1;  ///< poll space
            unsigned int    reserved2   :   3;
            unsigned int    writeSpace  :   2;  ///< write space
            unsigned int    reserved3   :  22;
        };
        unsigned int    ordinal2;
    };
    union
    {
        unsigned int    pollAddrLo;     ///< lower portion of poll address
        unsigned int    ordinal3;
    };
    union
    {
        unsigned int    pollAddrHi;
        unsigned int    ordinal4;
    };
    union
    {
        unsigned int    referenceVal;   ///< reference value
        unsigned int    ordinal5;
    };
    union
    {
        unsigned int    mask;           ///< comparison mask
        unsigned int    ordinal6;
    };
    union
    {
        unsigned int    writeAddrLo;    ///< write address low
        unsigned int    ordinal7;
    };
    union
    {
        unsigned int    writeAddrHi;    ///< write address low
        unsigned int    ordinal8;
    };
    union
    {
        unsigned int    writeData;      ///< write data that would be conditionally written to address
        unsigned int    ordinal9;
    };

} PM4CMDCONDWRITE, *PPM4CMDCONDWRITE;

//-------------------------------------------------------------------------------------------------
typedef union _ATOM_CNTL
{
    struct
    {
        unsigned int autoIncr     :  6; ///< auto increment in bytes
        unsigned int reserved1    :  2; ///< reserved
        unsigned int DMODE        :  1; ///< controls flushing of denorms
        unsigned int reserved2    : 23;
    };
    unsigned int u32All;

} ATOM_CNTL;
typedef struct _PM4CMDATOMICGDS
{
    union
    {
        PM4_TYPE_3_HEADER   header;               ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    atomOp        :   7;  ///< atomic operation
            unsigned int    reserved1     :   9;  ///< reserved
            unsigned int    atomCmpSwap   :   1;  ///< atomic compare swap
            unsigned int    atomComplete  :   1;  ///< atomic complete
            unsigned int    atomRead      :   1;  ///< atomic read
            unsigned int    atomRdCntl    :   2;  ///< read atomic pre-op source0 data
            unsigned int    reserved2     :  11;  ///< reserved
        };
        unsigned int        ordinal2;
    };
    union
    {
        ATOM_CNTL       atomCntl;
        unsigned int    ordinal3;
    };
    union
    {
        struct
        {
            unsigned int atomBase    : 16;   ///< Base adress for Atomic operation
                                             ///  relative to the GDS partition base
            unsigned int reserved3   : 16;
        };
        unsigned int     ordinal4;
    };
    union
    {
        struct
        {
            unsigned int atomSize    : 16;   ///< atomic size in bytes of the DS memory
                                             ///  determines when clamping begins
            unsigned int reserved4   : 16;
        };
        unsigned int     ordinal5;
    };
    union
    {
        struct
        {
            unsigned int atomOffset0 :   8;  ///< used to calculate address of
                                             ///  corresponding source operation
            unsigned int reserved5   :   8;  ///< reserved
            unsigned int atomOffset1 :   8;  ///< offset 1
            unsigned int reserved6   :   8;  ///< reserved
        };
        unsigned int     ordinal6;
    };
    union
    {
        unsigned int    atomDst;        ///< DS memory address to perform atomic op
        unsigned int    ordinal7;
    };
    union
    {
        unsigned int    atomSrc0Lo;      ///< Lower bits of atomic source0 data
        unsigned int    ordinal8;
    };
     union
    {
        unsigned int    atomSrc0Hi;      ///< Upper bits of atomic source0 data
        unsigned int    ordinal9;
    };
    union
    {
        unsigned int    atomSrc1Lo;      ///< Lower bits of atomic source1 data
        unsigned int    ordinal10;
    };
     union
    {
        unsigned int    atomSrc1Hi;      ///< Upper bits of atomic source1 data
        unsigned int    ordinal11;
    };
} PM4CMDATOMICGDS, *PPM4CMDATOMICGDS;

//-------------------------------------------------------------------------------------------------
typedef struct _PM4CMDATOMIC
{
    union
    {
        PM4_TYPE_3_HEADER header;   ///< header
        unsigned int      ordinal1;
    };
    union
    {
        struct
        {
            unsigned int atomOp      :   7;  ///< atomic operation
            unsigned int reserved1   :   1;
            unsigned int command     :   4;  ///< command
            unsigned int reserved2   :  20;
        };
        unsigned int     ordinal2;
    };
    union
    {
        unsigned int    addressLo;      ///< 4 byte aligned for 32 bit, 8 byte aligned for 64 bit
        unsigned int    ordinal3;
    };
    union
    {
        unsigned int    addressHi;
        unsigned int    ordinal4;
    };
    union
    {
        unsigned int    srcDataLo;      ///< atomic source data lower bits
        unsigned int    ordinal5;
    };
    union
    {
        unsigned int    srcDataHi;      ///< atomic source data upper bits
        unsigned int    ordinal6;
    };
    union
    {
        unsigned int    cmpDataLo;      ///< atomic compare data lower bits
        unsigned int    ordinal7;
    };
    union
    {
        unsigned int    cmpDataHi;      ///< atomic compare data upper bits
        unsigned int    ordinal8;
    };
    union
    {
        struct
        {
            unsigned int loopInterval    :   13;   ///< loop interval
            unsigned int reserved3       :   19;
        };
        unsigned int     ordinal9;
    };
} PM4CMDATOMIC, *PPM4CMDATOMIC;
//-------------------------------------------------------------------------------------------------
typedef struct _PM4CMDOCCLUSIONQUERY
{
    union
    {
        PM4_TYPE_3_HEADER header;       ///< header
        unsigned int      ordinal1;
    };
    union
    {
        unsigned int    startAddrLo;    ///< DB0 start address bits, 16 byte aligned
        unsigned int    ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    startAddrHi     : 16;   ///< DB0 start address bits
            unsigned int    reserved1       : 16;   ///< reserved
        };
        unsigned int        ordinal3;
    };
    union
    {
        unsigned int        queryAddrLo; ///< DW aligned accumalated query address bits, 4 byte aligned
        unsigned int        ordinal4;
    };
     union
    {
        struct
        {
            unsigned int    queryAddrHi     :  16;   ///< DW aligned accumalated query address bits
            unsigned int    reserved2       :  16;   ///< reserved
        };
        unsigned int        ordinal5;
    };

} PM4CMDOCCLUSIONQUERY, *PPM4CMDOCCLUSIONQUERY;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDPFPSYNCME
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        dummyData;
        unsigned int        ordinal2;
    };
} PM4CMDPFPSYNCME, *PPM4CMDPFPSYNCME;

//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDALLOCGDS
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    segmentSize     :  16;   ///< size of segment to allocate within
                                                     ///  corresponding partition
            unsigned int    reserved1       :  16;
        };
        unsigned int        ordinal2;
    };
} PM4CMDALLOCGDS, *PPM4CMDALLOCGDS;

//-------------------------------------------------------------------------------------------------

typedef enum _CPDMA_SRC_SEL
{
    CPDMA_SRC_SEL_SRC_ADDR          = 0,
    CPDMA_SRC_SEL_GDS               = 1,
    CPDMA_SRC_SEL_DATA              = 2,
    CPDMA_SRC_SEL_SRC_ADDR_USING_L2 = 3
} CPDMA_SRC_SEL;

typedef enum _CPDMA_DST_SEL
{
    CPDMA_DST_SEL_DST_ADDR          = 0,
    CPDMA_DST_SEL_GDS               = 1,
    CPDMA_DST_SEL_DST_ADDR_USING_L2 = 3
} CPDMA_DST_SEL;

typedef enum _CPDMA_ADDR_SPACE
{
    CPDMA_ADDR_SPACE_MEM   = 0,
    CPDMA_ADDR_SPACE_REG   = 1
} CPDMA_ADDR_SPACE;

typedef union _CP_DMA_COMMAND
{
    struct
    {
        unsigned int    byteCount    : 21; ///< byte count of transfer
        unsigned int    disWc        :  1; ///< disable write confirm
        unsigned int    srcSwap      :  2; ///< source endian swap control
        unsigned int    dstSwap      :  2; ///< destination endian swap contrl
        unsigned int    srcAddrSpace :  1; ///< source address space
        unsigned int    dstAddrSpace :  1; ///< destination address space
        unsigned int    srcAddrInc   :  1; ///< source address increment control
        unsigned int    dstAddrInc   :  1; ///< destination address increment control
        unsigned int    rawWait      :  1; ///< force DMA to wait to read surce data
        unsigned int    reserved1    :  1; ///< reserved
    };
    unsigned int u32All;

} CP_DMA_COMMAND;

//-------------------------------------------------------------------------------------------------
#define CP_DMA_DST_SEL_VIDMEM CPDMA_DST_SEL_DST_ADDR
#define CP_DMA_DST_SEL_GDS    CPDMA_DST_SEL_GDS

#define CP_DMA_SRC_SEL_VIDMEM CPDMA_SRC_SEL_SRC_ADDR
#define CP_DMA_SRC_SEL_GDS    CPDMA_SRC_SEL_GDS
#define CP_DMA_SRC_SEL_DATA   CPDMA_SRC_SEL_DATA

#define CP_DMA_ENGINE_ME      0
#define CP_DMA_ENGINE_PFP     1

typedef struct _PM4CMDCPDMA
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int    srcAddrLo;
        unsigned int    ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    srcAddrHi   :  16;
            unsigned int    reserved1   :   4;
            unsigned int    dstSel      :   2;  ///< destination select
            unsigned int    reserved2   :   2;
            unsigned int    m2mOpt      :   1;  ///< valid for r6xx and r7xx
            unsigned int    reserved3   :   2;
            unsigned int    engine      :   1;  ///< 0=ME, 1=PFP
            unsigned int    reserved4   :   1;
            unsigned int    srcSel      :   2;  ///< source select
            unsigned int    cpSync      :   1;  ///< lock out PFP or ME
        };
        unsigned int        ordinal3;
    };
    union
    {
        unsigned int    dstAddrLo;      ///<destination address lo
        unsigned int    ordinal4;
    };
    union
    {
        unsigned int    dstAddrHi;      ///<destination address upper
        unsigned int    ordinal5;
    };
    union
    {
        CP_DMA_COMMAND  command;
        unsigned int    ordinal6;
    };

} PM4CMDCPDMA, *PPM4CMDCPDMA;
//-------------------------------------------------------------------------------------------------

typedef struct _PM4CMDREGRMW
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    modAdrs         :  14;  ///< register DW address
            unsigned int    reserved1       :  16;
            unsigned int    orMaskSrc       :   1;  ///< OR Mask source
            unsigned int    andMaskSrc      :   1;  ///< AND mask source
        };
        unsigned int    ordinal2;
    };
    union
    {
        unsigned int    andMask;    ///< value logically ANDed with register contents
        unsigned int    andAdrs;    ///< register DW address for AND mask
        unsigned int    ordinal3;
    };
    union
    {
        unsigned int    orMask;     ///< value logically ORed with register contents
        unsigned int    orAdrs;     ///< register DW address for OR mask
        unsigned int    ordinal4;
    };
} PM4CMDREGRMW, *PPM4CMDREGRMW;
//-------------------------------------------------------------------------------------------------
typedef struct _PM4CONTEXTREGRMW
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    regOffset       :  16;  ///< register DW address
            unsigned int    reserved1       :  16;
        };
        unsigned int    ordinal2;
    };
    union
    {
        unsigned int    regMask;    ///< value logically ANDed with register contents
        unsigned int    ordinal3;
    };
    union
    {
        unsigned int    regData;    ///< Data to be used in the read/modify/write
        unsigned int    ordinal4;
    };
} PM4CONTEXTREGRMW, *PPM4CONTEXTREGRMW;
//-------------------------------------------------------------------------------------------------
typedef struct _PM4CMDWRITEGDSRAM
{
    union
    {
        PM4_TYPE_3_HEADER   header;      ///< header
        unsigned int        ordinal1;
    };
    union
    {
        unsigned int        gdsIndex;   ///< indexed offset from start of segment
                                        ///  within partition where immediate data is written
        unsigned int        ordinal2;
    };
    // This is a variable length packet. So, based on size in header, the layout following this
    // looks as follows
    // unsigned int data1;
    // ...
    // unsigned int dataN;

} PM4CMDWRITEGDSRAM, *PPM4CMDWRITEGDSRAM;

//-------------------------------------------------------------------------------------------------
// WRITE_DATA DST_SEL and ENGINE definitions
#define WRITE_DATA_DST_SEL_REGISTER      0
#define WRITE_DATA_DST_SEL_MEMORY_SYNC   1
#define WRITE_DATA_DST_SEL_TCL2          2
#define WRITE_DATA_DST_SEL_GDS           3
#define WRITE_DATA_DST_SEL_MEMORY_ASYNC  5

#define WRITE_DATA_CACHE_POLICY_LRU      0
#define WRITE_DATA_CACHE_POLICY_STREAM   1
#define WRITE_DATA_CACHE_POLICY_BYPASS   2

#define WRITE_DATA_ENGINE_ME             0
#define WRITE_DATA_ENGINE_PFP            1
#define WRITE_DATA_ENGINE_CE             2

typedef struct _PM4CMDWRITEDATA
{
    union
    {
        PM4_TYPE_3_HEADER   header;      ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    reserved1       :   8;
            unsigned int    dstSel          :   4;  ///< destination select
            unsigned int    reserved2       :   4;
            unsigned int    wrOneAddr       :   1;  ///< Increment or not increment address
            unsigned int    reserved3       :   3;
            unsigned int    wrConfirm       :   1;  ///< Wait or not wait for confirmation
            unsigned int    reserved4       :   3;
            unsigned int    atc__CI         :   1;
            unsigned int    cachePolicy__CI :   2; ///< Cache olicy settings for write requests to the TCL2
            unsigned int    volatile__CI    :   1; ///< Volatile setting for write requests to the TCL2
            unsigned int    reserved5       :   2;
            unsigned int    engineSel       :   2;  ///< engine select
        };
        unsigned int        ordinal2;
    };
    union
    {
        unsigned int    dstAddrLo;
        unsigned int    ordinal3;
    };
    union
    {
        unsigned int    dstAddrHi;
        unsigned int    ordinal4;
    };

    // This is a variable length packet. So, based on size in header, the layout following this
    // looks as follows
    // unsigned int data1;
    // ...
    // unsigned int dataN;
} PM4CMDWRITEDATA, *PPM4CMDWRITEDATA;

//-------------------------------------------------------------------------------------------------
typedef union _PRED_EXEC_CONTROL
{
    struct
    {
        unsigned int exec_count    : 14;
        unsigned int reserved1     : 10;
        unsigned int device_select :  8;
    };
    unsigned int u32All;
} PRED_EXEC_CONTROL;

//-------------------------------------------------------------------------------------------------
typedef struct _PM4CMDPREDEXEC
{
    union
    {
        PM4_TYPE_3_HEADER header;
        unsigned int      ordinal1;
    };
    union
    {
        PRED_EXEC_CONTROL control;
        unsigned int      ordinal2;
    };
} PM4CMDPREDEXEC, *PPM4CMDPREDEXEC;

// PREAMBLE_CNTL command definitions
#define PREAMBLE_CNTL_PREAMBLE_BEGIN        0
#define PREAMBLE_CNTL_PREAMBLE_END          1
#define PREAMBLE_CNTL_CLEAR_STATE_BEGIN     2
#define PREAMBLE_CNTL_CLEAR_STATE_END       3

//-------------------------------------------------------------------------------------------------
typedef struct _PM4CMDPREAMBLECNTL
{
    union
    {
        PM4_TYPE_3_HEADER header;
        unsigned int      ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    ib_offset   :   20; ///< dword offset in IB of the Preamble Begin or End Header
            unsigned int    reserved2   :    8; ///< reserved
            unsigned int    command     :    4; ///< command
                                                ///< 0000 : Begin Preamble
                                                ///< 0001 : End Preamble
                                                ///< 0010 : Begin of clear state init
                                                ///< 0011 : End of clear state init
        };
        unsigned int    ordinal2;
    };
} PM4CMDPREAMBLECNTL, *PPM4CMDPREAMBLECNTL;

//-------------------------------------------------------------------------------------------------
// ME_COPY_DATA
#define COPY_DATA_SEL_REG                         0    ///< Mem-mapped register for src/ dst

#define COPY_DATA_SEL_SRC_MEMORY                  1    ///< Memory
#define COPY_DATA_SEL_SRC_TC_L2                   2    ///< TC_L2 cache
#define COPY_DATA_SEL_SRC_GDS                     3    ///< GDS
#define COPY_DATA_SEL_SRC_SYS_PERF_COUNTER        4    ///< Privileged memory performance counter
#define COPY_DATA_SEL_SRC_IMME_DATA               5    ///< Immediate data
#define COPY_DATA_SEL_SRC_ATOMIC_RETURN_DATA      6    ///< ATOMIC_RETURN_DATA
#define COPY_DATA_SEL_SRC_GDS_ATOMIC_RETURN_DATA0 7    ///< GDS_ATOMIC_RETURN_DATA0
#define COPY_DATA_SEL_SRC_GDS_ATOMIC_RETURN_DATA1 8    ///< GDS_ATOMIC_RETURN_DATA1
#define COPY_DATA_SEL_SRC_GPU_CLOCK_COUNT         9    ///< Gpu clock count

#define COPY_DATA_SEL_DST_SYNC_MEMORY             1    ///< Memory (sync across GRBM)
#define COPY_DATA_SEL_DST_TC_L2                   2    ///< TC_L2 cache
#define COPY_DATA_SEL_DST_GDS                     3    ///< GDS
#define COPY_DATA_SEL_DST_SYS_PERF_COUNTER        4    ///< Privileged memory performance counter
#define COPY_DATA_SEL_DST_ASYNC_MEMORY            5    ///< Memory (async - direct)
#define COPY_DATA_SEL_DST_MEM_MAPPED_REG_DC       6    ///< MEM_MAPPED_REG_DC

#define COPY_DATA_SEL_COUNT_1DW                   0    ///< Copy 1 word (32 bits)
#define COPY_DATA_SEL_COUNT_2DW                   1    ///< Copy 2 words (64 bits)

#define COPY_DATA_SRC_CACHE_POLICY_LRU            0
#define COPY_DATA_SRC_CACHE_POLICY_STREAM         1
#define COPY_DATA_SRC_CACHE_POLICY_BYPASS         2

#define COPY_DATA_DST_CACHE_POLICY_LRU            0
#define COPY_DATA_DST_CACHE_POLICY_STREAM         1
#define COPY_DATA_DST_CACHE_POLICY_BYPASS         2

#define COPY_DATA_ENGINE_ME                       0
#define COPY_DATA_ENGINE_PFP                      1
#define COPY_DATA_ENGINE_CE                       2

#define COPY_DATA_WR_CONFIRM_NO_WAIT              0
#define COPY_DATA_WR_CONFIRM_WAIT                 1

typedef struct _PM4CMDCOPYDATA //only supported on NI and later asics
{
    union
    {
        PM4_TYPE_3_HEADER   header; ///< header
        unsigned int        ordinal1;
    };

    union
    {
        struct
        {
            unsigned int srcSel             : 4;  ///< 0 = register, 1 = memory, 5 = immediate data, others are reserved for future use
            unsigned int reserved1          : 4;  ///< reserved
            unsigned int dstSel             : 4;  ///< 0 = register, 1 = memory (sync - across GRBM), 5 = memory (async - direct), others are reserved for future use
            unsigned int srcAtc__CI         : 1;  ///< ATC setting for read requests to the MC and TCL2
            unsigned int srcCachePolicy__CI : 2;  ///< Cache policy settings for read requests to the TCL2
            unsigned int srcVolatile__CI    : 1;  ///< Volatile setting for read requests to the TCL2
            unsigned int countSel           : 1;  ///< 0 = 32-bits (1 DW), 1 = 64-bits (2 DW)
            unsigned int reserved3          : 3;  ///< reserved
            unsigned int wrConfirm          : 1;  ///< 0 = do not wait for write confirm, 1  wait for confirmation that the write has completed
            unsigned int reserved4          : 3;  ///< reserved
            unsigned int dstAtc__CI         : 1;  ///< ATC setting for read requests to the MC and TCL2
            unsigned int dstCachePolicy__CI : 2;  ///< Cache policy settings for read requests to the TCL2
            unsigned int dstVolatile__CI    : 1;  ///< Volatile setting for read requests to the TCL2
            unsigned int reserved5          : 2;  ///< reserved
            unsigned int engineSel          : 2;  ///< 0 = ME, 1 = PFP, 2 = CE (SI only)
        };
        unsigned int    ordinal2;
    };
    union
    {
        unsigned int srcAddressLo;  ///< low bits of source address, memory-mapped register, or immediate data
        unsigned int ordinal3;
    };
    union
    {
        unsigned int srcAddressHi;  ///< high bits of source address, or immediate data
        unsigned int ordinal4;
    };
    union
    {
        unsigned int dstAddressLo;  ///< low bits of dst address, or memory-mapped register
        unsigned int ordinal5;
    };
    union
    {
        unsigned int dstAddressHi; ///< high dst address
        unsigned int ordinal6;
    };

} PM4CMDCOPYDATA, *PPM4CMDCOPYDATA;

//-------------------------------------------------------------------------------------------------
typedef struct _PM4ACQUIREMEM {
    union
    {
        PM4_TYPE_3_HEADER   header; ///< header
        unsigned int        ordinal1;
    };

    union
    {
        struct
        {
            unsigned int        coherCntl : 31;
            unsigned int        engine    :  1;
        };

        unsigned int  ordinal2;
    };

    union
    {
        unsigned int  coherSize;
        unsigned int  ordinal3;
    };

    union
    {
        struct
        {
            unsigned int        coherSizeHi     :  8;
            unsigned int        coherSizeHiRsvd : 16;
            unsigned int        reserved1       :  8;
        };

        unsigned int  ordinal4;
    };

    union
    {
        unsigned int  coherBaseLo;
        unsigned int  ordinal5;
    };

    union
    {
        struct
        {
            unsigned int        coherBaseHi     : 24;
            unsigned int        reserved2       :  8;
        };

        unsigned int  ordinal6;
    };

    union
    {
        struct
        {
            unsigned int        pollInterval    : 16;
            unsigned int        reserved3       : 16;
        };

        unsigned int  ordinal7;
    };
} PM4ACQUIREMEM, *PPM4ACQUIREMEM;

//-------------------------------------------------------------------------------------------------
typedef struct _PM4ATOMICMEM_CI {
    union
    {
        PM4_TYPE_3_HEADER   header; ///< header
        unsigned int        ordinal1;
    };

    union
    {
        struct
        {
            unsigned int  atomicOp     :  7;  ///< This field can only be used with TC_OP_ATOMIC_* ops.
            unsigned int  reserved1    :  1;  ///< 0 = single pass atomic, 1 = loop until compare is satisfied
            unsigned int  command      :  4;
            unsigned int  reserved2    : 12;
            unsigned int  atc          :  1;
            unsigned int  cachePolicy  :  2;  ///< 0 = LRU, 1 = stream, 2 = bypass, 3 = reserved
            unsigned int  volatile__CI :  1;
            unsigned int  reserved3    :  2;
            unsigned int  engineSel    :  2;  ///< 0 = ME, 1 = PFP, 2, 3 = reserved
        };
        unsigned int  ordinal2;
    };

    union
    {
        unsigned int  addrLo;
        unsigned int  ordinal3;
    };

    union
    {
        unsigned int  addrHi;
        unsigned int  ordinal4;
    };

    union
    {
        unsigned int  srcDataLo;
        unsigned int  ordinal5;
    };

    union
    {
        unsigned int  srcDataHi;
        unsigned int  ordinal6;
    };

    union
    {
        unsigned int  cmpDataLo;
        unsigned int  ordinal7;
    };

    union
    {
        unsigned int  cmpDataHi;
        unsigned int  ordinal8;
    };

    union
    {
        struct
        {
            unsigned int  loopInterval : 13;
            unsigned int  reserved4    : 19;
        };
        unsigned int  ordinal9;
    };
} PM4ATOMICMEM_CI, *PPM4ATOMICMEM_CI;

//-------------------------------------------------------------------------------------------------
typedef struct _PM4WRITEDATACONFIRM {
    union
    {
        PM4_TYPE_3_HEADER   header; ///< header
        unsigned int        ordinal1;
    };

    union
    {
        struct
        {
            unsigned int  function  :  3; ///< function:
                                          ///<     000 = Always (Compare Passes). Still does read operation and waits for read results to come back.
                                          ///<     001 = Less Than (<) the Reference Value.
                                          ///<     010 = Less Than or Equal () to the Reference Value.
                                          ///<     011 = Equal (= ) to the Reference Value.
                                          ///<     100 = Not Equal (! = ) to the Reference Value.
                                          ///<     101 = Greater Than or Equal () to the Reference Value.
                                          ///<     110 = Greater Than (>) the Reference Value.
                                          ///<     111 = Reserved. If ENGINE=PFP, only 101/Greater Than or Equal is valid, since that is all the ucode implemented in the PFP.
            unsigned int  operation :  2; ///< operation:
                                          ///<     0 = Write Reference to Address0 -> Poll Address1 -> Write Reference to Address 1
            unsigned int  reserved1 :  3;
            unsigned int  engine    :  1; ///< engine, 0 = ME, 1 = PFP
            unsigned int  reserved2 : 23;
        };

        unsigned int  ordinal2;
    };

    union
    {
        unsigned int  addrLo;
        unsigned int  ordinal3;
    };

    union
    {
        unsigned int  addrHi;
        unsigned int  ordinal4;
    };

    union
    {
        unsigned int  reference;
        unsigned int  ordinal5;
    };

    union
    {
        unsigned int  mask;
        unsigned int  ordinal6;
    };

    union
    {
        struct
        {
            unsigned int  pollInterval : 16;
            unsigned int  reserved4    : 16;
        };
        unsigned int  ordinal7;
    };
} PM4WRITEDATACONFIRM, *PPM4WRITEDATACONFIRM;

//-------------------------------------------------------------------------------------------------
typedef struct _PM4DMADATA {
    union
    {
        PM4_TYPE_3_HEADER   header; ///< header
        unsigned int        ordinal1;
    };

    union
    {
        struct
        {
            unsigned int  engine         :  1;
            unsigned int  reserved1      : 11;
            unsigned int  srcATC         :  1;
            unsigned int  srcCachePolicy :  2;
            unsigned int  srcVolatile    :  1;
            unsigned int  reserved2      :  4;
            unsigned int  dstSel         :  2;
            unsigned int  reserved3      :  2;
            unsigned int  dstATC         :  1;
            unsigned int  dstCachePolicy :  2;
            unsigned int  dstVolatile    :  1;
            unsigned int  reserved4      :  1;
            unsigned int  srcSel         :  2;
            unsigned int  cpSync         :  1;
        };
        unsigned int  ordinal2;
    };

    union
    {
        unsigned int  srcAddrLo;
        unsigned int  data;
        unsigned int  ordinal3;
    };

    union
    {
        unsigned int  srcAddrHi;
        unsigned int  ordinal4;
    };

    union
    {
        unsigned int  dstAddrLo;
        unsigned int  ordinal5;
    };

    union
    {
        unsigned int  dstAddrHi;
        unsigned int  ordinal6;
    };

    union
    {
        struct
        {
            unsigned int  byteCount : 21;
            unsigned int  disWC     :  1;
            unsigned int  srcSwap   :  2;
            unsigned int  dstSwap   :  2;
            unsigned int  sas       :  1;
            unsigned int  das       :  1;
            unsigned int  saic      :  1;
            unsigned int  daic      :  1;
            unsigned int  rawWait   :  1;
            unsigned int  reserved5 :  1;
        };

        unsigned int  command;
        unsigned int  ordinal7;
    };
} PM4DMADATA, *PPM4DMADATA;

//-------------------------------------------------------------------------------------------------
typedef struct _PM4CMDRELEASEMEM
{
    union
    {
        PM4_TYPE_3_HEADER   header;         ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    eventType             : 6;    ///< event type written to VGT_EVENT_INITIATOR
            unsigned int    reserved1             : 2;    ///< reserved
            unsigned int    eventIndex            : 4;    ///< event index
            unsigned int    tcl1VolActionEna      : 1;    ///<
            unsigned int    tcVolActionEna        : 1;    ///<
            unsigned int    reserved2             : 1;
            unsigned int    tcWbActionEna         : 1;    ///<
            unsigned int    tcl1ActionEna         : 1;    ///<
            unsigned int    tcActionEna           : 1;
            unsigned int    reserved3             : 7;
            unsigned int    cachePolicy           : 2;    ///< Cache Policy setting used for writing fences and timestamps to the TCL2
            unsigned int    _volatile__CI         : 1;    ///< Volatile setting used for writing fences and timestamps to the TCL2.
            unsigned int    reserved5             : 4;
        };
        unsigned int        ordinal2;
    };
    union
    {
        struct
        {
            unsigned int    reserved6  : 16;   ///< reserved
            unsigned int    dstSel     :  2;   ///< destination select
            unsigned int    reserved7  :  6;   ///< reserved
            unsigned int    intSel     :  3;   ///< selects interrupt action for end-of-pipe
            unsigned int    reserved8  :  2;   ///< reserved
            unsigned int    dataSel    :  3;   ///< selects source of data
        };
        unsigned int        ordinal3;
    };
    union
    {
        unsigned int        addressLo; ///< low bits of address
        unsigned int        ordinal4;
    };
    union
    {
        unsigned int        addressHi;    ///< high bits of address
        unsigned int        ordinal5;
    };
    union
    {
        struct
        {
            unsigned int    gdsIndex  : 16; ///< Byte offset into GDS to copy from
            unsigned int    numDwords : 16; ///< Number of DWORDS of GDS to copy
        };
        unsigned int        dataLo;        ///< value that will be written to memory when event occurs
        unsigned int        ordinal6;
    };
    union
    {
        unsigned int        dataHi;        ///< value that will be written to memory when event occurs
        unsigned int        ordinal7;
    };
} PM4CMDRELEASEMEM, *PPM4CMDRELEASEMEM;

// EVENT_WRITE_EOP packet definitions
#define RELEASEMEM_DST_SEL_MEMORY                0
#define RELEASEMEM_DST_SEL_L2                    1

#define RELEASEMEM_DATA_SEL_DISCARD              0
#define RELEASEMEM_DATA_SEL_SEND_DATA32          1
#define RELEASEMEM_DATA_SEL_SEND_DATA64          2
#define RELEASEMEM_DATA_SEL_SEND_GPU_CLOCK       3
#define RELEASEMEM_DATA_SEL_SEND_CP_PERFCOUNTER  4
#define RELEASEMEM_DATA_SEL_STORE_GDS_DATA       5

#define RELEASEMEM_INT_SEL_NONE                  0
#define RELEASEMEM_INT_SEL_SEND_INT              1
#define RELEASEMEM_INT_SEL_SEND_INT_ON_CONFIRM   2
#define RELEASEMEM_INT_SEL_SEND_DATA_ON_CONFIRM  3

//-------------------------------------------------------------------------------------------------
typedef struct _PM4CMDREWIND
{
    union
    {
        PM4_TYPE_3_HEADER   header;             ///< header
        unsigned int        ordinal1;
    };
    union
    {
        struct
        {
            unsigned int    reserved0     : 24;   ///< Reserved
            unsigned int    offloadEnable : 1;    ///< Enable offload polling valid bit to IQ
            unsigned int    reserved1     : 6;    ///< Reserved
            unsigned int    valid         : 1;    ///< Set when subsequent packets are valid
        };
        unsigned int        ordinal2;
    };
} PM4CMDREWIND, *PPM4CMDREWIND;

// Rewind packet valid bit mask
#define REWIND_MASK_VALID 0x800000000

//-------------------------------------------------------------------------------------------------
// PM4 command template sizes
#define PM4_CMD_NOP_DWORDS                                  \
    (sizeof(PM4CMDNOP) / sizeof(unsigned int))

#define PM4_CMD_DRAW_PREAMBLE_DWORDS                        \
    (sizeof(PM4CMDDRAWPREAMBLE) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_2_DWORDS                         \
    (sizeof(PM4CMDDRAWINDEX2) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_OFFSET_2_DWORDS                  \
    (sizeof(PM4CMDDRAWINDEXOFFSET2) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_AUTO_DWORDS                      \
    (sizeof(PM4CMDDRAWINDEXAUTO) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_IMMD_DWORDS                      \
    (sizeof(PM4CMDDRAWINDEXIMMD) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_TYPE_DWORDS                      \
    (sizeof(PM4CMDDRAWINDEXTYPE) / sizeof(unsigned int))

#define PM4_CMD_INDEX_ATTRIBUTES_INDIRECT_DWORDS            \
    (sizeof(PM4CMDINDEXATTRIBUTESINDIRECT) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_BUFFER_SIZE_DWORDS               \
    (sizeof(PM4CMDDRAWINDEXBUFFERSIZE) / sizeof(unsigned int))

#define PM4_CMD_DRAW_NUM_INSTANCES_DWORDS                   \
    (sizeof(PM4CMDDRAWNUMINSTANCES) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_BASE_DWORDS                      \
    (sizeof(PM4CMDDRAWINDEXBASE) / sizeof(unsigned int))

#define PM4_CMD_DRAW_SET_BASE_DWORDS                        \
    (sizeof(PM4CMDDRAWSETBASE) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDIRECT_DWORDS                        \
    (sizeof(PM4CMDDRAWINDIRECT) / sizeof(unsigned int))

#define PM4_CMD_LOAD_DATA_DWORDS                            \
    (sizeof(PM4CMDLOADDATA) / sizeof(unsigned int))

#define PM4_CMD_LOAD_DATA_INDEX_DWORDS                      \
    (sizeof(PM4CMDLOADDATAINDEX) / sizeof(unsigned int))

#define PM4_CMD_SET_DATA_DWORDS                             \
    (sizeof(PM4CMDSETDATA) / sizeof(unsigned int))

#define PM4_CMD_WAIT_REG_MEM_DWORDS                         \
    (sizeof(PM4CMDWAITREGMEM) / sizeof(unsigned int))

#define PM4_CMD_WAIT_REG_MEM64_DWORDS                       \
    (sizeof(PM4CMDWAITREGMEM64) / sizeof(unsigned int))

#define PM4_CMD_WAIT_EVENT_WRITE_DWORDS                     \
    (sizeof(PM4CMDEVENTWRITE) / sizeof(unsigned int))

#define PM4_CMD_WAIT_EVENT_WRITE_QUERY_DWORDS               \
    (sizeof(PM4CMDEVENTWRITEQUERY) / sizeof(unsigned int))

#define PM4_CMD_WAIT_EVENT_WRITE_EOP_DWORDS                 \
    (sizeof(PM4CMDEVENTWRITEEOP) / sizeof(unsigned int))

#define PM4_CMD_STRMOUT_BUFFER_UPDATE_DWORDS                \
    (sizeof(PM4CMDSTRMOUTBUFFERUPDATE) / sizeof(unsigned int))

#define PM4_CMD_CONTEXT_CTL_DWORDS                          \
    (sizeof(PM4CMDCONTEXTCONTROL) / sizeof(unsigned int))

#define PM4_CMD_SET_PREDICATION_DWORDS                      \
    (sizeof(PM4CMDSETPREDICATION) / sizeof(unsigned int))

#define PM4_CMD_SURFACE_SYNC_DWORDS                         \
    (sizeof(PM4CMDSURFACESYNC) / sizeof(unsigned int))

#define PM4_CMD_DISPATCH_DIRECT_DWORDS                      \
    (sizeof(PM4CMDDISPATCHDIRECT) / sizeof(unsigned int))

#define PM4_CMD_DISPATCH_INDIRECT_DWORDS                    \
    (sizeof(PM4CMDDISPATCHINDIRECT) / sizeof(unsigned int))

#define PM4_CMD_DISPATCH_INDIRECT_MEC_DWORDS                \
    (sizeof(PM4CMDDISPATCHINDIRECTMEC) / sizeof(unsigned int))

#define PM4_CMD_CLEAR_STATE_DWORDS                          \
    (sizeof(PM4CMDCLEARSTATE) / sizeof(unsigned int))

#define PM4_CMD_EVENT_WRITE_EOS_DWORDS                      \
    (sizeof(PM4CMDEVENTWRITEEOS) / sizeof(unsigned int))

#define PM4_CMD_SCRATCH_RAM_WRITE_DWORDS                    \
    (sizeof(PM4CMDSCRATCHRAMWRITE) / sizeof(unsigned int))

#define PM4_CMD_WRITE_CONST_RAM_DWORDS                    \
    (sizeof(PM4CMDCONSTRAMWRITE) / sizeof(unsigned int))

#define PM4_CMD_DUMP_CONST_RAM_DWORDS                     \
    (sizeof(PM4CMDCONSTRAMDUMP) / sizeof(unsigned int))

#define PM4_CMD_DUMP_CONST_RAM_OFFSET_DWORDS              \
    (sizeof(PM4CMDCONSTRAMDUMPOFFSET) / sizeof(unsigned int))

#define PM4_CMD_LOAD_CONST_RAM_DWORDS                     \
    (sizeof(PM4CMDCONSTRAMLOAD) / sizeof(unsigned int))

#define PM4_CMD_INC_CE_COUNTER_DWORDS                      \
    (sizeof(PM4CMDINCCECOUNTER) / sizeof(unsigned int))

#define PM4_CMD_INC_DE_COUNTER_DWORDS                      \
    (sizeof(PM4CMDINCDECOUNTER) / sizeof(unsigned int))

#define PM4_CMD_SET_CE_DE_COUNTERS_DWORDS                  \
    (sizeof(PM4CMDSETCEDECOUNTERS) / sizeof(unsigned int))

#define PM4_CMD_WAIT_ON_AVAIL_BUFFER_DWORDS                 \
    (sizeof(PM4CMDWAITONAVAILBUFFER) / sizeof(unsigned int))

#define PM4_CMD_WAIT_ON_CE_COUNTER_DWORDS                   \
    (sizeof(PM4CMDWAITONCECOUNTER) / sizeof(unsigned int))

#define PM4_CMD_WAIT_ON_DE_COUNTER_DIFF_DWORDS               \
    (sizeof(PM4CMDWAITONDECOUNTERDIFF) / sizeof(unsigned int))

// Miscellaneous defines
#define PM4_CMD_MAX_SIZE_DWORDS      (1 << 14)

#define PM4_SETTING_PRED_EXEC(mask, count)  ((mask << 24) | count)

//-------------------------------------------------------------------------------------------------

#define PM4_CMD_INDIRECT_BUFFER_CONST_DWORDS                            \
    (sizeof(PM4CMDINDIRECTBUFFER) / sizeof(unsigned int))

#define PM4_CMD_INDIRECT_BUFFER_DWORDS                                  \
    (sizeof(PM4CMDINDIRECTBUFFER) / sizeof(unsigned int))

#define PM4_CMD_COND_INDIRECT_BUFFER_DWORDS                             \
    (sizeof(PM4CMDCONDINDIRECTBUFFER) / sizeof(unsigned int))

#define PM4_CMD_SET_SH_REG_OFFSET_DWORDS                                \
    (sizeof(PM4CMDSETSHREGOFFSET) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_INDIRECT_DWORDS                              \
    (sizeof(PM4CMDDRAWINDEXINDIRECT) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_INDIRECT_MULTI_DWORDS                        \
    (sizeof(PM4CMDDRAWINDEXINDIRECTMULTI) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_MULTI_AUTO_DWORDS                            \
    (sizeof(PM4CMDDRAWINDEXMULTIAUTO) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDIRECT_DWORDS                                    \
    (sizeof(PM4CMDDRAWINDIRECT) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDIRECT_MULTI_DWORDS                              \
    (sizeof(PM4CMDDRAWINDIRECTMULTI) / sizeof(unsigned int))

#define PM4_CMD_INCREMENT_DE_COUNTER_DWORDS                             \
    (sizeof(PM4CMDINCREMENTCOUNTER) / sizeof(unsigned int))

#define PM4_CMD_DRAW_INDEX_BASE_DWORDS                                  \
    (sizeof(PM4CMDDRAWINDEXBASE) / sizeof(unsigned int))

#define PM4_CMD_DRAW_MPEG_INDEX_DWORDS                                  \
    (sizeof(PM4CMDDRAWMPEGINDEX) / sizeof(unsigned int))

#define PM4_CMD_COND_EXEC_DWORDS                                        \
    (sizeof(PM4CMDCONDEXEC) / sizeof(unsigned int))

#define PM4_CMD_COND_EXEC_CI_DWORDS                                     \
    (sizeof(PM4CMDCONDEXEC_CI) / sizeof(unsigned int))

#define PM4_CMD_COND_WRITE_DWORDS                                       \
    (sizeof(PM4CMDCONDWRITE) / sizeof(unsigned int))

#define PM4_CMD_PRED_EXEC_DWORDS                                        \
    (sizeof(PM4CMDPREDEXEC) / sizeof(unsigned int))

#define PM4_CMD_ATOMIC_DWORDS                                           \
    (sizeof(PM4CMDATOMIC) / sizeof(unsigned int))

#define PM4_CMD_ATOMIC_GDS_DWORDS                                       \
    (sizeof(PPM4CMDATOMICGDS) / sizeof(unsigned int))

#define PM4_CMD_OCCLUSION_QUERY_DWORDS                                  \
    (sizeof(PM4CMDOCCLUSIONQUERY) / sizeof(unsigned int))

#define PM4_CMD_ALLOC_GDS_DWORDS                                        \
    (sizeof(PM4CMDALLOCGDS) / sizeof(unsigned int))

#define PM4_CMD_CP_DMA_DWORDS                                           \
    (sizeof(PM4CMDCPDMA) / sizeof(unsigned int))

#define PM4_CMD_REG_RMW_DWORDS                                          \
    (sizeof(PM4CMDREGRMW) / sizeof(unsigned int))

#define PM4_CONTEXT_REG_RMW_DWORDS                                      \
    (sizeof(PM4CONTEXTREGRMW) / sizeof(unsigned int))

#define PM4_CMD_WRITE_GDS_RAM_DWORDS                                    \
    (sizeof(PM4CMDWRITEGDSRAM) / sizeof(unsigned int))

#define PM4_CMD_WRITE_DATA_DWORDS                                       \
    (sizeof(PM4CMDWRITEDATA) / sizeof(unsigned int))

#define PM4_CMD_WRITE_CONST_RAM_OFFSET_DWORDS                           \
    (sizeof(PM4CMDCONSTRAMWRITE) / sizeof(unsigned int))

#define PM4_CMD_LOAD_CONFIG_REG_DWORDS                                  \
    (sizeof(PM4CMDLOADDATA) / sizeof(unsigned int))

#define PM4_CMD_SET_CONFIG_REG_DWORDS                                   \
    (sizeof(PM4CMDSETDATA) / sizeof(unsigned int))

#define PM4_CMD_PREAMBLE_CNTL_REG_DWORDS                                \
    (sizeof(PM4CMDPREAMBLECNTL) / sizeof(unsigned int))

#define PM4_CMD_SET_CONTEXT_REG_DWORDS                                  \
    (sizeof(PM4CMDSETDATA) / sizeof(unsigned int))

#define PM4_CMD_SET_CONTEXT_REG_INDIRECT_DWORDS                         \
    (sizeof(PM4CMDSETDATA) / sizeof(unsigned int))

#define PM4_CMD_LOAD_CONTEXT_REG_DWORDS                                 \
    (sizeof(PM4CMDLOADDATA) / sizeof(unsigned int))

#define PM4_CMD_LOAD_SH_REG_DWORDS                                      \
    (sizeof(PM4CMDLOADDATA) / sizeof(unsigned int))

#define PM4_CMD_SET_SH_REG_DWORDS                                       \
    (sizeof(PM4CMDSETDATA) / sizeof(unsigned int))

#define PM4_CMD_WRITE_CONST_RAM_INDIRECT_DWORDS                         \
    (sizeof(PM4CMDCONSTRAMWRITE) / sizeof(unsigned int))

#define PM4_CMD_MEM_SEMAPHORE_DWORDS                                    \
    (sizeof(PM4CMDMEMSEMAPHORE) / sizeof(unsigned int))

#define PM4_CMD_PFP_SYNC_ME_DWORDS                                      \
    (sizeof(PM4CMDPFPSYNCME) / sizeof(unsigned int))

#define PM4_CMD_COPY_DATA_DWORDS                                        \
    (sizeof(PM4CMDCOPYDATA) / sizeof(unsigned int))

#define PM4_CMD_ACQUIRE_MEM_DWORDS                                      \
    (sizeof(PM4ACQUIREMEM) / sizeof (unsigned int))

#define PM4_CMD_ATOMIC_MEM_DWORDS                                       \
    (sizeof(PM4ATOMICMEM_CI) / sizeof (unsigned int))

#define PM4_CMD_WRITE_DATA_CONFIRM_DWORDS                               \
    (sizeof(PM4WRITEDATACONFIRM) / sizeof (unsigned int))

#define PM4_CMD_DMA_DATA_DWORDS                                         \
    (sizeof(PM4DMADATA) / sizeof (unsigned int))

#define PM4_CMD_RELEASE_MEM_DWORDS                                      \
    (sizeof(PM4CMDRELEASEMEM) / sizeof (unsigned int))

#define PM4_CMD_REWIND_DWORDS                                           \
    (sizeof(PM4CMDREWIND) / sizeof(unsigned int))

} // inline namespace Chip
} // namespace Gfx6
} // namespace Pal

#endif

