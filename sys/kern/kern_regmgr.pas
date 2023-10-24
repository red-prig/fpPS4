unit kern_regmgr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 SCE_REGMGR_ENT_KEY_NP_env                      = $19800000;
 SCE_REGMGR_ENT_KEY_NP_debug                    = $19810000;
 SCE_REGMGR_ENT_KEY_BROWSER_DEBUG_notification  = $3CC80700;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_trc_notify      = $78026400;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_sys_prx_preload = $78028A00;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_use_default_lib = $78028300;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_game_intmem_dbg = $7802BF00;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_sce_module_dbg  = $7802C000;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_preload_chk_off = $78020500;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_020B00          = $78020B00;
 SCE_REGMGR_ENT_KEY_MORPHEUS_DEBUG_vr_capture   = $58800C00;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_game_heap_trace = $7802B700;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_expose_under_2k = $7802B900;

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

function regMgrCnvRegId(enc:QWORD):Integer;
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

function sys_regmgr_call(op,key:DWORD;presult,pvalue:Pointer;vlen:QWORD):Integer;
label
 _copyout_value,
 _err;
var
 kret:DWORD;
 skey:Integer;

 data:packed record
  enc :QWORD;
  val1:DWORD;
  val2:DWORD;
  slen:DWORD;
  dstr:array[0..2055] of AnsiChar;
 end;

begin
 kret:=0;

 if (presult=nil) then
 begin
  kret:=$800d0202;
  goto _err;
 end;

 case op of

  $19: //sceRegMgrNonSysGetInt
      begin
       Result:=copyin(pvalue,@data,16);
       if (Result<>0) then
       begin
        kret:=$800d020f;
        goto _err;
       end;

       skey:=regMgrCnvRegId(data.enc);

       if (skey<0) then
       begin
        kret:=skey;
        goto _err;
       end;

       case skey of
        SCE_REGMGR_ENT_KEY_DEVENV_TOOL_game_intmem_dbg:data.val2:=0; //_malloc_init_lv2
        SCE_REGMGR_ENT_KEY_DEVENV_TOOL_sce_module_dbg :data.val2:=0; //libSceSysmodule  (bit 1,2 -> load debug lib)
        SCE_REGMGR_ENT_KEY_DEVENV_TOOL_preload_chk_off:data.val2:=0; //libSceSysmodule  (print errors?)
        SCE_REGMGR_ENT_KEY_DEVENV_TOOL_020B00         :data.val2:=0; //libSceSysmodule  (preload module?)
        SCE_REGMGR_ENT_KEY_MORPHEUS_DEBUG_vr_capture  :data.val2:=0; //libkernel
        SCE_REGMGR_ENT_KEY_DEVENV_TOOL_game_heap_trace:data.val2:=0; //libSceLibcInternal (sceLibcHeapGetTraceInfo -> get_segment_info)
        SCE_REGMGR_ENT_KEY_DEVENV_TOOL_expose_under_2k:data.val2:=0; //sceVideoOutOpen  debug video modes?

        else
         begin
          Writeln(' enc:0x',HexStr(data.enc,16),'->key:0x',HexStr(skey,8));
          print_backtrace_td(stderr);
          Assert(False);
         end;
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

       skey:=regMgrCnvRegId(data.enc);

       if (skey<0) then
       begin
        kret:=skey;
        goto _err;
       end;

       case skey of
        SCE_REGMGR_ENT_KEY_NP_env: //sceNpUtilGetEnv
          begin
           data.dstr:='np';
          end;
        else
         begin
          Writeln(' enc:0x',HexStr(data.enc,16),'->key:0x',HexStr(skey,8));
          print_backtrace_td(stderr);
          Assert(False);
         end;
       end;

       vlen:=$818;
       goto _copyout_value;
      end


  else
      begin
       if (op=$27) or (op>=$40) then
       begin
        kret:=$800d0219;
        goto _err;
       end;

       Writeln('Unhandled regmgr op:0x',HexStr(op,4));
       print_backtrace_td(stderr);
       Assert(False);
      end;
 end;

_err:
 Result:=copyout(@kret,presult,SizeOf(DWORD));
end;

function sys_workaround8849(key:DWORD):Integer;
var
 td:p_kthread;
begin
 td:=curkthread;
 if (td=nil) then Exit(-1);

 Result:=0;
 case key of
  SCE_REGMGR_ENT_KEY_NP_debug                   :td^.td_retval[0]:=0; //nop
  SCE_REGMGR_ENT_KEY_BROWSER_DEBUG_notification :td^.td_retval[0]:=0; //nop
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_trc_notify     :td^.td_retval[0]:=0; //nop
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_sys_prx_preload:td^.td_retval[0]:=0; //nop
  SCE_REGMGR_ENT_KEY_DEVENV_TOOL_use_default_lib:td^.td_retval[0]:=0; //nop
  else
   Result:=EINVAL;
 end;
end;



end.

