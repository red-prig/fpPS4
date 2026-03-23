unit ps4_libSceImeDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_mtx,
 subr_dynlib,
 kern_proc,
 sys_bootparam,
 ime_types,
 host_ipc_interface;

{$CALLING default}

type
 PImeDialogOpen=^TImeDialogOpen;
 TImeDialogOpen=record
  //
 end;

implementation

var
 g_ImeDialog_mtx:mtx;
 g_state        :Integer=0;

{$CALLING SysV_ABI_CDecl}

const
 //SceImeDialogStatus
 SCE_IME_DIALOG_STATUS_NONE    =0;
 SCE_IME_DIALOG_STATUS_RUNNING =1;
 SCE_IME_DIALOG_STATUS_FINISHED=2;

var
 status_ime_dialog:Integer=SCE_IME_DIALOG_STATUS_NONE;

function ps4_sceImeDialogInit(param   :pSceImeDialogParam;
                              extended:pSceImeParamExtended):Integer;
begin
 writeln;
 Result:=0;
end;

function ps4_sceImeDialogTerm():Integer;
begin
 Result:=0;
end;

//sceImeDialogAbort
//sceImeDialogForceClose

function ps4_sceImeDialogGetStatus():Integer;
begin
 Result:=status_ime_dialog;
end;

Function CheckOption(option:DWORD):Boolean; inline;
var
 filter:DWORD;
begin

 if (p_proc.p_sdk_version > $14fffff) then
 begin
  filter:=$69ff;
 end else
 begin
  filter:=$80068ff;
 end;

 if (p_proc.p_sdk_version > $174ffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $80061ff;
 end;

 if (p_proc.p_sdk_version > $34fffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $80049ff;
 end;

 if (p_proc.p_sdk_version > $3ffffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $80029ff;
 end;

 if ((option and (filter xor $fffffdff))<>0) then
 begin
  Result:=False;
 end else
 begin
  Result:=True;
 end;

end;

Function CheckLang(supportedLanguages:QWORD):Boolean; inline;
var
 filter:QWORD;
begin

 filter:=ord($1ffffff < p_proc.p_sdk_version) * $1000000 + $3fe1fffff;

 if ($24fffff < p_proc.p_sdk_version) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $3fd1fffff;
 end;

 if ($4ffffff < p_proc.p_sdk_version) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $2031fffff;
 end;

 if ($fffffff < p_proc.p_sdk_version) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $1ff1fffff;
 end;

 if (((not filter) and supportedLanguages)<>0) then
 begin
  Result:=False;
 end else
 begin
  Result:=True;
 end;

end;

Function CheckExtendedOption(option:DWORD):Boolean; inline;
var
 filter:DWORD;
begin

 if (p_proc.p_sdk_version < $1560000) then
 begin
  filter:=$71df;
 end else
 begin
  filter:=$7fdf;
 end;

 if (p_proc.p_sdk_version > $24fffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $4fdf;
 end;

 Result:=false;
 if ((option and $3000)<>$2000) and ((option and $4080)<>$4000) then
 begin
  if (p_proc.p_sdk_version > $5ffffff) then
  begin
   filter:=filter;
  end else
  begin
   filter:=filter and $3fdf;
  end;
  Result:=((not filter) and option)=0;
 end;

end;


function ps4_sceImeDialogGetPanelSize(param   :pSceImeDialogParam;
                                      p_width :PDWORD;
                                      p_height:PDWORD):Integer;
var
 width :DWORD;
 height:DWORD;
begin

 if (param=nil) or
    (p_width=nil) or
    (p_height=nil) then
 begin
  Exit(SCE_IME_ERROR_INVALID_ADDRESS);
 end;

 if (SCE_IME_TYPE_NUMBER < param^.ImeType) then
 begin
  Exit(SCE_IME_ERROR_INVALID_TYPE);
 end;

 if not CheckOption(param^.option) then
 begin
  Exit(SCE_IME_ERROR_INVALID_OPTION);
 end;

 if not CheckLang(param^.supportedLanguages) then
 begin
  Exit(SCE_IME_ERROR_INVALID_SUPPORTED_LANGUAGES);
 end;

 if (param^.ImeType=SCE_IME_TYPE_NUMBER) then
 begin
  width :=370;
  height:=470;
  if (p_proc.p_sdk_version > $16fffff) then
  begin
   height:=522 - ord(p_proc.p_sdk_version < $2000000);
  end;
 end else
 begin
  if (param^.ImeType<>SCE_IME_TYPE_BASIC_LATIN) and
     ((param^.option and $c0000004)<>SCE_IME_OPTION_PASSWORD) then
  begin
   width :=793;
   height:=628;
   if ((param^.option and SCE_IME_OPTION_MULTILINE)=0) then
   begin
    height:=528;
   end;
  end else
  begin
   width:=793;
   if ((param^.option and SCE_IME_OPTION_MULTILINE)=0) then
   begin
    height:=476;
    if (p_proc.p_sdk_version > $16fffff) then
    begin
     height:=528;
    end;
   end else
   begin
    height:=576;
    if (p_proc.p_sdk_version > $16fffff) then
    begin
     height:=628;
    end;
   end;
  end;
 end;

 if ((param^.option and SCE_IME_OPTION_USE_OVER_2K_COORDINATES)<>0) then
 begin
  width :=width  shl 1;
  height:=height shl 1;
 end;

 p_width^ :=width;
 p_height^:=height;
 Result:=0;
end;

function ps4_sceImeDialogGetPanelSizeExtended(param   :pSceImeDialogParam;
                                              extended:pSceImeParamExtended;
                                              p_width :PDWORD;
                                              p_height:PDWORD):Integer;
var
 width :DWORD;
 height:DWORD;
begin

 if (param=nil) or
    (p_width=nil) or
    (p_height=nil) then
 begin
  Exit(SCE_IME_ERROR_INVALID_ADDRESS);
 end;

 if (SCE_IME_TYPE_NUMBER < param^.ImeType) then
 begin
  Exit(SCE_IME_ERROR_INVALID_TYPE);
 end;

 if not CheckOption(param^.option) then
 begin
  Exit(SCE_IME_ERROR_INVALID_OPTION);
 end;

 if not CheckLang(param^.supportedLanguages) then
 begin
  Exit(SCE_IME_ERROR_INVALID_SUPPORTED_LANGUAGES);
 end;

 if (extended=nil) then
 begin
  Exit(ps4_sceImeDialogGetPanelSize(param,p_width,p_height));
 end;

 if (p_proc.p_sdk_version > $16fffff) then
 begin
  if not CheckExtendedOption(extended^.option) then
  begin
   Exit(SCE_IME_ERROR_INVALID_EXTENDED);
  end;
 end;

 //IPMI

 if (param^.ImeType=SCE_IME_TYPE_NUMBER) then
 begin
  width:=370;
  if ((param^.option and SCE_IME_OPTION_EXT_KEYBOARD)=0) or
     ((extended^.option and SCE_IME_EXT_OPTION_HIDE_KEYPANEL_IF_EXT_KEYBOARD)=0) then
  begin
   height:=470;
   if (p_proc.p_sdk_version > $16fffff) then
   begin
    height:=522 - ord(p_proc.p_sdk_version < $2000000);
   end;
  end else
  if (p_proc.p_sdk_version < $1700000) then
  begin
   height:=470;
  end else
  begin
   height:=102 - ord(p_proc.p_sdk_version < $2000000);
  end;
 end else
 begin

  if (param^.ImeType<>SCE_IME_TYPE_BASIC_LATIN) and
     ((param^.option and $c0000004)<>SCE_IME_OPTION_PASSWORD) then
  begin
   width:=793;

   if ((param^.option and SCE_IME_OPTION_MULTILINE)=0) then
   begin

    if ((param^.option and SCE_IME_OPTION_EXT_KEYBOARD)<>0) and
       ((extended^.option and SCE_IME_EXT_OPTION_HIDE_KEYPANEL_IF_EXT_KEYBOARD)<>0) then
    begin
     if ((param^.ImeType and $fffffffe)=2) then
     begin
      height:=102 - ord(p_proc.p_sdk_version < $2000000);
     end else
     begin
      height:=168 - ord(p_proc.p_sdk_version < $1700000) * 2;
     end;
    end else
    begin
     height:=528;
    end;

   end else
   begin

    if ((param^.option and SCE_IME_OPTION_EXT_KEYBOARD)<>0) and
       ((extended^.option and SCE_IME_EXT_OPTION_HIDE_KEYPANEL_IF_EXT_KEYBOARD)<>0) then
    begin
     if ((param^.ImeType and $fffffffe)=2) then
     begin
      height:=203 - ord(p_proc.p_sdk_version < $1700000);
     end else
     begin
      height:=268 - ord(p_proc.p_sdk_version < $1700000) * 2;
     end;
    end else
    begin
     height:=628;
    end;

   end;

  end else
  begin

   width:=793;
   if ((param^.option and SCE_IME_OPTION_MULTILINE)=0) then
   begin
    if ((param^.option and SCE_IME_OPTION_EXT_KEYBOARD)=0) or
       ((extended^.option and SCE_IME_EXT_OPTION_HIDE_KEYPANEL_IF_EXT_KEYBOARD)=0) then
    begin
     if (p_proc.p_sdk_version > $16fffff) then
     begin
      height:=528;
     end else
     begin
      height:=476;
     end;
    end else
    begin
     height:=103 - ord(p_proc.p_sdk_version < $2000000);
    end;
   end else
   if ((param^.option and SCE_IME_OPTION_EXT_KEYBOARD)=0) or
      ((extended^.option and SCE_IME_EXT_OPTION_HIDE_KEYPANEL_IF_EXT_KEYBOARD)=0) then
   begin
    if (p_proc.p_sdk_version > $16fffff) then
    begin
     height:=628;
    end else
    begin
     height:=576;
    end;
   end else
   begin
    if (p_proc.p_sdk_version < $1700000) then
    begin
     height:=266;
    end else
    begin
     height:=203;
    end;
   end;

  end;

 end;

 if ((param^.option and SCE_IME_OPTION_USE_OVER_2K_COORDINATES)<>0) then
 begin
  width :=width  shl 1;
  height:=height shl 1;
 end;

 p_width^ :=width;
 p_height^:=height;
 Result:=0;
end;

function dt_fini(args:QWORD;argp,addr:Pointer):Integer;
begin
 ps4_sceImeDialogTerm();
 Result:=0;
end;

//

function Load_libSceImeDialog(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceImeDialog');

 Result^.fini_proc_addr.native:=@dt_fini;

 lib:=Result^.add_lib('libSceImeDialog');
 lib.set_proc($354781ACDEE1CDFD,@ps4_sceImeDialogInit);
 lib.set_proc($8324F2567F9B5CCC,@ps4_sceImeDialogTerm);
 lib.set_proc($2000E60F8B527016,@ps4_sceImeDialogGetStatus);
 lib.set_proc($C2AB09BD15F0979F,@ps4_sceImeDialogGetPanelSize);
 lib.set_proc($0910FE8D212B1094,@ps4_sceImeDialogGetPanelSizeExtended);

 mtx_init(g_ImeDialog_mtx,'g_ImeDialog_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceImeDialog.prx',@Load_libSceImeDialog);

end.

