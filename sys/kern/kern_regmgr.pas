unit kern_regmgr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 SCE_REGMGR_ENT_KEY_SYSTEM_LIBC_intmem_peak_size           = $02C30100;
 SCE_REGMGR_ENT_KEY_SYSTEM_LIBC_intmem_shortage_count      = $02C30200;
 SCE_REGMGR_ENT_KEY_VIDEOOUT_enable_supersampling_mode     = $0A170000;
 SCE_REGMGR_ENT_KEY_NP_env                                 = $19800000;
 SCE_REGMGR_ENT_KEY_NP_debug                               = $19810000;
 SCE_REGMGR_ENT_KEY_BROWSER_DEBUG_notification             = $3CC80700;
 SCE_REGMGR_ENT_KEY_MORPHEUS_DEBUG_vr_capture              = $58800C00;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_gpi00                      = $78020400;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_trc_notify                 = $78026400;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_sys_prx_preload            = $78028A00;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_use_default_lib            = $78028300;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_game_intmem_dbg            = $7802BF00;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_sce_module_dbg             = $7802C000;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_preload_chk_off            = $78020500;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_disable_sce_module_ver_chk = $78020B00;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_game_heap_trace            = $7802B700;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_expose_under_2k            = $7802B900;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_COMMONDIALOG_watch_dog     = $7802CD01;

function sys_regmgr_call(op,key:DWORD;presult,pvalue:Pointer;vlen:QWORD):Integer;
function sys_workaround8849(key:DWORD):Integer;

implementation

uses
 errno,
 systm,
 kern_thr,
 subr_backtrace;

const
 s_ScrambNSsdk:array[0..15] of Byte=(
  $6B, $E8, $98, $03,
  $9A, $70, $23, $5A,
  $63, $EE, $F5, $7B,
  $FF, $A4, $4C, $8C
 );

 s_ScrambNSlib:array[0..15] of Byte=(
  $14, $EE, $DE, $E1,
  $80, $AC, $F3, $78,
  $47, $43, $DB, $40,
  $93, $DD, $B1, $34
 );

type
 t_encoded_id=packed record
  data    :array[0..3] of Byte;
  table   :Byte;
  index   :Byte;
  checksum:Word;
 end;

function regMgrCnvRegId(enc:QWORD;enc2:DWORD):Integer;
var
 a,b,c,d,e,f,x:Byte;
 g:DWORD;
 h:DWORD;
 y:DWORD;
 p_Scramb:PByte;
 data:array[0..3] of Byte;
begin
 Result:=0;

 a:=enc and $ff;            //data[0]
 b:=(enc shr  8) and $ff;   //data[1]
 c:=(enc shr 16) and $ff;   //data[2]
 d:=(enc shr 24) and $ff;   //data[3]
 e:=(enc shr 32) and $ff;   //table
 f:=(enc shr 40) and $ff;   //index
 g:=(enc shr 48) and $ffff; //checksum
 h:=(a + b + c + d + e * f) and $ffff;

 if (g<>h) then Exit(Integer($800d0204));

 f:=f xor $6b;

 if (f > 12) then Exit(Integer($800d0203));

 x:=e xor s_ScrambNSsdk[$f - f];

 if (x=$19) then
 begin
  p_Scramb:=@s_ScrambNSlib;
 end else
 begin
  p_Scramb:=@s_ScrambNSsdk;
  if (x<>$72) then Exit(Integer($800d0205));
 end;

 x:=p_Scramb[f];
 y:=p_Scramb[f + 1];

 data[0]:=(a xor p_Scramb[f + 3]);
 data[1]:=(c xor p_Scramb[f + 2]);
 data[2]:=(d xor y);
 data[3]:=(b xor x);

 Result:=(data[0]       ) or
         (data[1] shl  8) or
         (data[2] shl 16) or
         (data[3] shl 24)
end;

function sceRegMgrGetInt(key:DWORD;p_out:PInteger):Integer;
begin
 Result:=0;

 case key of
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_game_intmem_dbg           :p_out^:=0; //_malloc_init_lv2
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_sce_module_dbg            :p_out^:=0; //libSceSysmodule  (bit 1,2 -> load debug lib)
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_preload_chk_off           :p_out^:=0; //libSceSysmodule  (print errors?)
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_disable_sce_module_ver_chk:p_out^:=0; //libSceSysmodule  (preload module?)
  SCE_REGMGR_ENT_KEY_MORPHEUS_DEBUG_vr_capture             :p_out^:=0; //libkernel
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_game_heap_trace           :p_out^:=0; //libSceLibcInternal (sceLibcHeapGetTraceInfo -> get_segment_info)
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_expose_under_2k           :p_out^:=0; //sceVideoOutOpen  debug video modes?
  SCE_REGMGR_ENT_KEY_VIDEOOUT_enable_supersampling_mode    :p_out^:=0; //sceVideoOutOpen

  SCE_REGMGR_ENT_KEY_SYSTEM_LIBC_intmem_peak_size          :p_out^:=0; //libSceLibcInternal
  SCE_REGMGR_ENT_KEY_SYSTEM_LIBC_intmem_shortage_count     :p_out^:=0; //libSceLibcInternal

  SCE_REGMGR_ENT_KEY_NP_debug                              :p_out^:=0; //sys_workaround8849, libSceSysUtil
  SCE_REGMGR_ENT_KEY_BROWSER_DEBUG_notification            :p_out^:=0; //sys_workaround8849
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_trc_notify                :p_out^:=0; //sys_workaround8849
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_sys_prx_preload           :p_out^:=0; //sys_workaround8849
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_use_default_lib           :p_out^:=0; //sys_workaround8849

  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_COMMONDIALOG_watch_dog    :p_out^:=0; //sceCommonDialog

  else
   Exit(-1);
 end;

end;

function sys_regmgr_call(op,key:DWORD;presult,pvalue:Pointer;vlen:QWORD):Integer;
label
 _copyout_value,
 _err;
var
 kret:DWORD;
 skey:Integer;

 data:packed record
     enc1:QWORD;
     enc2:DWORD;
  int_val:DWORD;
     size:QWORD;
     data:array[0..2047] of AnsiChar;
 end;

begin
 kret:=0;

 if (presult=nil) then
 begin
  kret:=$800d0202;
  goto _err;
 end;

 case op of

  $18: Assert(false,'Unhandled regmgr op:sceRegMgrNonSysSetInt');

  $19: //sceRegMgrNonSysGetInt
      begin
       Result:=copyin(pvalue,@data,16);
       if (Result<>0) then
       begin
        kret:=$800d020f;
        goto _err;
       end;

       skey:=regMgrCnvRegId(data.enc1,data.enc2);

       if (skey<0) then
       begin
        kret:=skey;
        goto _err;
       end;

       Result:=sceRegMgrGetInt(skey,@data.int_val);
       if (Result<>0) then
       begin
        print_error_td(' enc:0x'+HexStr(data.enc1,16)+'->key:0x'+HexStr(skey,8));
        Assert(False);
       end;

       _copyout_value:
       Result:=copyout(@data,pvalue,vlen);
       if (Result<>0) then
       begin
        kret:=$800d0210;
        goto _err;
       end;
      end;

  $1B: //sceRegMgrNonSysGetStr
      begin
       Result:=copyin(pvalue,@data,$818);
       if (Result<>0) then
       begin
        kret:=$800d020f;
        goto _err;
       end;

       skey:=regMgrCnvRegId(data.enc1,data.enc2);

       if (skey<0) then
       begin
        kret:=skey;
        goto _err;
       end;

       case skey of
        SCE_REGMGR_ENT_KEY_NP_env: //sceNpUtilGetEnv
          begin
           data.data:='np';
          end;
        else
         begin
          print_error_td(' enc:0x'+HexStr(data.enc1,16)+'->key:0x'+HexStr(skey,8));
          Assert(False);
         end;
       end;

       vlen:=$818;
       goto _copyout_value;
      end;

    $1D: //sceRegMgrNonSysGetBin
      begin
       Result:=copyin(pvalue,@data,$818);
       if (Result<>0) then
       begin
        kret:=$800d020f;
        goto _err;
       end;

       skey:=regMgrCnvRegId(data.enc1,data.enc2);

       if (skey<0) then
       begin
        kret:=skey;
        goto _err;
       end;

       case skey of
        SCE_REGMGR_ENT_KEY_DEVENV_TOOL_gpi00:PQWORD(@data.data)^:=0; //sceKernelGetGPI

        else
         begin
          print_error_td(' enc:0x'+HexStr(data.enc1,16)+'->key:0x'+HexStr(skey,8));
          Assert(False);
         end;
       end;

       vlen:=$818;
       goto _copyout_value;
      end;

  else
      begin
       if (op=$27) or (op>=$40) then
       begin
        kret:=$800d0219;
        goto _err;
       end;

       print_error_td('Unhandled regmgr op:0x'+HexStr(op,4));
       Assert(False);
      end;
 end;

_err:
 Result:=copyout(@kret,presult,SizeOf(DWORD));
end;

function sys_workaround8849(key:DWORD):Integer;
var
 td:p_kthread;
 int_val:Integer;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 Result:=0;
 case key of
  SCE_REGMGR_ENT_KEY_NP_debug                   ,
  SCE_REGMGR_ENT_KEY_BROWSER_DEBUG_notification ,
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_trc_notify     ,
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_sys_prx_preload,
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_use_default_lib:
    begin
     Result:=sceRegMgrGetInt(key,@int_val);
     td^.td_retval[0]:=int_val;
    end;
  else
   Result:=EINVAL;
 end;

end;



end.

