unit dev_gc;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_conf;

procedure gc_initialize();

implementation

uses
 errno,
 kern_authinfo,
 vm,
 vmparam,
 trap;

var
 gc_mmap_ptr:Pointer=nil;
 gc_AreSubmitsAllowed:Integer=0; //0=true,1=false (0xfe0100000)

type
 p_SetGsRingSizes=^t_SetGsRingSizes;
 t_SetGsRingSizes=packed record
  esgsRingSize:DWORD;
  gsvsRingSize:DWORD;
  zero        :DWORD;
 end;

 p_SetMipStatsReport=^t_SetMipStatsReport;
 t_SetMipStatsReport=packed record
  p_type:DWORD;
  param1:DWORD;
  param2:DWORD;
  param3:DWORD;
 end;

Function gc_ioctl(dev:p_cdev;cmd:QWORD;data:Pointer;fflag:Integer):Integer;
begin
 Result:=0;

 Writeln('gc_ioctl(0x',HexStr(cmd,8),')');

 case cmd of
  $C004811F: //sceGnmGetNumTcaUnits
            begin
             Exit(19);
            end;

  $C00C8110: //sceGnmSetGsRingSizes
            begin
             Writeln('SetGsRingSizes(0x',HexStr(p_SetGsRingSizes(data)^.esgsRingSize,8),',0x'
                                        ,HexStr(p_SetGsRingSizes(data)^.gsvsRingSize,8),')');
            end;

  $C0848119: //*MipStatsReport
            begin
             case PInteger(data)^ of
              $10001:
                     begin
                      Writeln('MipStatsReport(0x',HexStr(p_SetMipStatsReport(data)^.param1,8),',0x'
                                                 ,HexStr(p_SetMipStatsReport(data)^.param2,8),',0x'
                                                 ,HexStr(p_SetMipStatsReport(data)^.param3,8),')');
                     end;

              $18001:; //diag?

              else
               Exit(EINVAL);
             end;
            end;
  $C008811B: //sceGnmAreSubmitsAllowed
            begin
             PPointer(data)^:=@gc_AreSubmitsAllowed;
            end;

  $C010810B: //get cu mask
            begin
             PInteger(data)[0]:=$10; //& 0x3ff   GC SE0 Redundant CU: 0x10
             PInteger(data)[1]:=$10; //& 0x3ff   GC SE1 Redundant CU: 0x10
             PInteger(data)[2]:=$00; //& 0x3ff   GC SE2 Redundant CU: 0x00
             PInteger(data)[3]:=$00; //& 0x3ff   GC SE3 Redundant CU: 0x00
            end;

  $C0048116: //sceGnmSubmitDone
            begin
             Writeln('sceGnmSubmitDone');
            end;

  $C0048114: //sceGnmFlushGarlic
            begin
             Writeln('sceGnmFlushGarlic');
            end


  else
   begin
    print_backtrace_td(stderr);
    Assert(False);
    Result:=EINVAL;
   end;
 end;

end;

Function gc_mmap(dev:p_cdev;offset:vm_ooffset_t;paddr:p_vm_paddr_t;nprot:Integer;memattr:p_vm_memattr_t):Integer;
begin
 if sceSblACMgrHasUseHp3dPipeCapability(@g_authinfo) then
 begin
  Exit(EINVAL);
 end;

 if (offset>=PAGE_SIZE) then
 begin
  Exit(EPERM);
 end;

 paddr^:=offset + QWORD(gc_mmap_ptr);
 memattr^:=0;

 Result:=0;
end;

const
 gc_cdevsw:t_cdevsw=(
  d_version    :D_VERSION;
  d_flags      :0;
  d_name       :'gc';
  d_open       :nil;
  d_fdopen     :nil;
  d_close      :nil;
  d_read       :nil;
  d_write      :nil;
  d_ioctl      :@gc_ioctl;
  d_poll       :nil;
  d_mmap       :@gc_mmap;
  d_strategy   :nil;
  d_dump       :nil;
  d_kqfilter   :nil;
  d_purge      :nil;
  d_mmap_single:nil;
 );

procedure gc_initialize();
begin
 gc_mmap_ptr:=Pointer($fe0200000);

 make_dev(@gc_cdevsw,0,0,0,&666,'gc',[]);
end;


end.

