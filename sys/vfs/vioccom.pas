unit vioccom;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

{
 * Ioctl's have the command encoded in the lower word, and the size of
 * any in or out parameters in the upper word.  The high 3 bits of the
 * upper word are used to encode the in/out status of the parameter.
 }
const
 IOCPARM_SHIFT=13;  { number of bits for ioctl size }
 IOCPARM_MASK =((1 shl IOCPARM_SHIFT) - 1); { parameter length mask }

 IOCPARM_MAX =(1 shl IOCPARM_SHIFT); { max size of ioctl }
 IOC_VOID    =$20000000; { no parameters }
 IOC_OUT     =$40000000; { copy out parameters }
 IOC_IN      =$80000000; { copy in parameters }
 IOC_INOUT   =(IOC_IN or IOC_OUT);
 IOC_DIRMASK =(IOC_VOID or IOC_OUT or IOC_IN);

function IOCPARM_LEN(x:QWORD):QWORD; inline;
function IOCBASECMD (x:QWORD):QWORD; inline;
function IOCGROUP   (x:QWORD):QWORD; inline;

implementation

function IOCPARM_LEN(x:QWORD):QWORD; inline;
begin
 Result:=(x shr 16) and IOCPARM_MASK;
end;

function IOCBASECMD(x:QWORD):QWORD; inline;
begin
 Result:=x and (not (IOCPARM_MASK shl 16));
end;

function IOCGROUP(x:QWORD):QWORD; inline;
begin
 Result:=(x shr 8) and $ff;
end;


end.

