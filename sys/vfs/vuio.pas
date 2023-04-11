unit vuio;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 p_iovec=^iovec;
 iovec=packed record
  iov_base:Pointer; //Base address.
  iov_len :Ptruint; //Length.
 end;

 uio_rw=(UIO_READ,UIO_WRITE);

 // Segment flag values.
 uio_seg=(
  UIO_USERSPACE, // from user data space
  UIO_SYSSPACE,  // from system space
  UIO_NOCOPY     // don't copy, already in object
 );

 p_uio=^t_uio;
 t_uio=packed record
  uio_iov   :p_iovec; // scatter/gather list
  uio_iovcnt:Integer; // length of scatter/gather list
  _align    :Integer;
  uio_offset:Int64;   // offset in target object
  uio_resid :Int64;   // remaining bytes to process
  uio_segflg:uio_seg; // address space
  uio_rw    :uio_rw;  // operation
  uio_td    :Pointer; // owner
 end;

const
 UIO_MAXIOV=1024; // max 1K of iov's

implementation

end.

