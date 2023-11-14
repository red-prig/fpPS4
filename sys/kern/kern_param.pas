unit kern_param;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 vmparam;

const
 NAME_MAX  =255;  // max bytes in a file name
 PATH_MAX  =1024; // max bytes in pathname
 PIPE_BUF  =512;  // max bytes for atomic pipe writes
 IOV_MAX   =1024; // max elements in i/o vector
 MAXNAMLEN =255;

 LINK_MAX   =32767; // max file link count
 MAX_CANON  =255;   // max bytes in term canon input line
 MAX_INPUT  =255;   // max bytes in terminal input
 NGROUPS_MAX=1023;  // max supplemental group id's

 MAXPATHLEN =PATH_MAX;
 MAXSYMLINKS=32;

 MAXCOMLEN  =31;

 SPECNAMELEN=63;    { max length of devicename }

 MAXFIDSZ   =16;

 MFSNAMELEN =16; // length of type name including null
 MNAMELEN   =88; // size of on/from name bufs

 DEV_BSHIFT=9;
 DEV_BSIZE =(1 shl DEV_BSHIFT);

 BLKDEV_IOSIZE=PAGE_SIZE;
 DFLTPHYS     =(64 * 1024);
 MAXPHYS      =(128 * 1024);

 PUSER=700;
 PRI_MIN_KERN=64;

 PSWP  =(PRI_MIN_KERN+ 0);
 PVM   =(PRI_MIN_KERN+ 4);
 PINOD =(PRI_MIN_KERN+ 8);
 PRIBIO=(PRI_MIN_KERN+12);
 PVFS  =(PRI_MIN_KERN+16);
 PZERO =(PRI_MIN_KERN+20);
 PSOCK =(PRI_MIN_KERN+24);
 PWAIT =(PRI_MIN_KERN+28);
 PLOCK =(PRI_MIN_KERN+32);
 PPAUSE=(PRI_MIN_KERN+36);

 PRIMASK=$0fff;
 PCATCH =$1000; //OR'd with pri for tsleep to check signals
 PDROP  =$2000; //OR'd with pri to stop re-entry of interlock mutex
 PBDRY  =$4000; //for PCATCH stop is done on the user boundary

 maxfilesperproc = 44236;

 IOSIZE_MAX      =High(Integer);
 DEVFS_IOSIZE_MAX=High(Integer);

implementation

end.

