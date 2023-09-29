unit kern_psl;

{$mode ObjFPC}{$H+}

interface

//80 40   10   04   01
//SF:ZF:0:AF:0:PF:1:CF

const
 PSL_C   =$00000001; // carry bit
 PSL_PF  =$00000004; // parity bit
 PSL_AF  =$00000010; // bcd carry bit
 PSL_Z   =$00000040; // zero bit
 PSL_N   =$00000080; // negative bit
 PSL_T   =$00000100; // trace enable bit
 PSL_I   =$00000200; // interrupt enable bit
 PSL_D   =$00000400; // string instruction direction bit
 PSL_V   =$00000800; // overflow bit
 PSL_IOPL=$00003000; // i/o privilege level
 PSL_NT  =$00004000; // nested task bit
 PSL_RF  =$00010000; // resume flag bit
 PSL_AC  =$00040000; // alignment checking
 PSL_ID  =$00200000; // identification bit

 PSL_RESERVED_DEFAULT=$00000002;

 PSL_KERNEL=PSL_RESERVED_DEFAULT;
 PSL_USER  =(PSL_RESERVED_DEFAULT or PSL_I);

 PSL_USERCHANGE=(PSL_C or PSL_PF or PSL_AF or PSL_Z or PSL_N or PSL_T or PSL_D or PSL_V or PSL_NT or PSL_RF or PSL_AC or PSL_ID);

implementation

end.

