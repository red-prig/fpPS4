unit kern_regmgr;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 SCE_REGMGR_ENT_KEY_NP_debug                   =$19810000;
 SCE_REGMGR_ENT_KEY_BROWSER_DEBUG_notification =$3CC80700;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_trc_notify     =$78026400;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_sys_prx_preload=$78028A00;
 SCE_REGMGR_ENT_KEY_DEVENV_TOOL_use_default_lib=$78028300;

function sys_regmgr_call(op,key:DWORD;presult,pvalue:Pointer;vlen:QWORD):Integer;
function sys_workaround8849(key:DWORD):Integer;

implementation

uses
 errno,
 systm,
 kern_thr;

type
 t_encoded_id=packed record
  data    :array[0..3] of Byte;
  table   :Byte;
  index   :Byte;
  checksum:Word;
 end;

function sys_regmgr_call(op,key:DWORD;presult,pvalue:Pointer;vlen:QWORD):Integer;
label
 _err;
var
 kret:DWORD;

 data:packed record
  enc :t_encoded_id;
  val1:DWORD;
  val2:DWORD;
 end;

begin
 kret:=0;

 if (presult=nil) then
 begin
  kret:=$800d0202;
  goto _err;
 end;

 case op of
  $19:begin //sceRegMgrNonSysGetInt
       Result:=copyin(pvalue,@data,16);
       if (Result<>0) then
       begin
        kret:=$800d020f;
        goto _err;
       end;

       Writeln(' enc:0x',HexStr(qword(data.enc),16),' val1:0x',HexStr(data.val1,8),' val12:0x',HexStr(data.val2,8));

       data.val2:=0;

       Result:=copyout(@data,pvalue,vlen);
       if (Result<>0) then
       begin
        kret:=$800d0210;
        goto _err;
       end;
      end;
  else
      begin
       Writeln('Unhandled regmgr op:0x',HexStr(op,4));
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

