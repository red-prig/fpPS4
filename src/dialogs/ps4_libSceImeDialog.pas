unit ps4_libSceImeDialog;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 sysutils,
 kern_mtx,
 subr_dynlib,
 kern_proc,
 sys_bootparam,
 ime_types,
 host_ipc;

{$CALLING default}

type
 TImeDialogResult=packed record
  result   :Integer;
  endstatus:SceImeDialogEndStatus;
  inputText:array[0..2047] of WideChar;
 end;

 TImeDialogTextFilter=packed record
  result    :Integer;
  Text      :array[0..120] of WideChar;
  TextLength:Integer;
 end;

 TImeDialogPosAndForm=record
  PanelType          :Byte; //SceImePanelType
  horizontalAlignment:Byte; //SceImeHorizontalAlignment
  verticalAlignment  :Byte; //SceImeVerticalAlignment
  posx               :Single;
  posy               :Single;
  width              :DWORD;
  height             :DWORD;
 end;

 TImeDialogOpen=record
  ImeType                 :Byte;  // SceImeType
  enterLabel              :Byte;  // SceImeEnterLabel
  option                  :WORD;  // SCE_IME_OPTION
  ExtOption               :WORD;  // SCE_IME_EXT_OPTION
  priority                :Byte;  // SceImePanelPriority
  disableDevice           :Byte;  // SCE_IME_DISABLE_DEVICE
  extKeyboardMode         :DWORD; // SCE_IME_INIT_EXT_KEYBOARD_MODE
  //
  userId                  :Integer;
  supportedLanguages      :QWORD;
  maxTextLength           :DWORD;
  PosAndForm              :TImeDialogPosAndForm;
  result                  :TImeDialogResult;
  placeholder             :array[0..127] of WideChar;
  title                   :array[0.. 63] of WideChar;
  //
  colorBase               :SceImeColor;
  colorLine               :SceImeColor;
  colorTextField          :SceImeColor;
  colorPreedit            :SceImeColor;
  colorButtonDefault      :SceImeColor;
  colorButtonFunction     :SceImeColor;
  colorButtonSymbol       :SceImeColor;
  colorText               :SceImeColor;
  colorSpecial            :SceImeColor;
  //
  additionalDictionaryPath:array[0..1023] of AnsiChar;
 end;

 TImeDialogStatus=(dRUNNING,sFINISHED,dABORTED);

 TImeDialogClient=class
  data             :TImeDialogOpen;
  output           :PWideChar;
  filter:record
   addr:SceImeTextFilter;
   src :TImeDialogTextFilter;
   dst :TImeDialogTextFilter;
  end;
  extKeyboardFilter:SceImeExtKeyboardFilter;
  state            :TImeDialogStatus;
 end;

implementation

var
 g_ImeDialog_mtx:mtx;
 g_dialog       :TImeDialogClient=nil;

{$CALLING SysV_ABI_CDecl}

const
 //SceImeDialogStatus
 SCE_IME_DIALOG_STATUS_NONE    =0;
 SCE_IME_DIALOG_STATUS_RUNNING =1;
 SCE_IME_DIALOG_STATUS_FINISHED=2;

 //SceImeDialogEndStatus
 SCE_IME_DIALOG_END_STATUS_OK           =0;
 SCE_IME_DIALOG_END_STATUS_USER_CANCELED=1;
 SCE_IME_DIALOG_END_STATUS_ABORTED      =2;

function strncpy_s(dst,src:PChar;maxlen:ptrint):PChar;
begin
 if (dst=nil) or (src=nil) then Exit(nil);
 Result:=StrLCopy(dst,src,maxlen);
end;

function wcsncpy_s(dst,src:PWideChar;maxlen:ptrint):PWideChar;
begin
 if (dst=nil) or (src=nil) then Exit(nil);
 Result:=StrLCopy(dst,src,maxlen);
end;

Function CheckOption_old(option:DWORD):Boolean; inline;
var
 filter:DWORD;
begin

 if (p_proc.p_sdk_version > $14fffff) then
 begin
 filter:=$f7f06fff;
 end else
 begin
  filter:=$ff706eff;
 end;

 if (p_proc.p_sdk_version < $1700000) then
 begin
  filter:=filter and $ffe06bff;
 end else
 begin
  filter:=filter or $8600000;
 end;

 if (p_proc.p_sdk_version > $174ffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $fff067ff;
 end;

 if (p_proc.p_sdk_version > $34fffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $fff04fff;
 end;

 if (p_proc.p_sdk_version > $3ffffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $fff02fff;
 end;

 if (((not filter) and option)<>0) then
 begin
  Result:=False;
 end else
 begin
  Result:=True;
 end;

end;

Function CheckOption_new(option:DWORD):Boolean; inline;
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

Function CheckLang_old(supportedLanguages:QWORD):Boolean; inline;
var
 filter:QWORD;
begin

 filter:=ord(p_proc.p_sdk_version > $1ffffff) * $1000000 + $303fe1fffff;

 if (p_proc.p_sdk_version > $24fffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $303fd1fffff;
 end;

 if (p_proc.p_sdk_version > $4ffffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $302031fffff;
 end;

 if (p_proc.p_sdk_version > $fffffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $301ff1fffff;
 end;

 if (((not filter) and supportedLanguages)<>0) then
 begin
  Result:=False;
 end else
 begin
  Result:=True;
 end;

end;

Function CheckLang_new(supportedLanguages:QWORD):Boolean; inline;
var
 filter:QWORD;
begin

 filter:=ord(p_proc.p_sdk_version > $1ffffff) * $1000000 + $3fe1fffff;

 if (p_proc.p_sdk_version > $24fffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $3fd1fffff;
 end;

 if (p_proc.p_sdk_version > $4ffffff) then
 begin
  filter:=filter;
 end else
 begin
  filter:=filter and $2031fffff;
 end;

 if (p_proc.p_sdk_version > $fffffff) then
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

Function CheckExtendedOption_old(option:DWORD):Boolean; inline;
var
 filter:DWORD;
begin

 if (p_proc.p_sdk_version < $1560000) then
 begin
  filter:=$41df;
 end else
 begin
  filter:=$4fdf;
 end;

 if ((option and $4080)=$4000) then
 begin
  Result:=false;
 end else
 begin
  if (p_proc.p_sdk_version > $5ffffff) then
  begin
   filter:=filter;
  end else
  begin
   filter:=filter and $fdf;
  end;
  Result:=((not filter) and option)=0;
 end;

end;

Function CheckExtendedOption_new(option:DWORD):Boolean; inline;
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

function CheckReserved(var buf;len:DWORD):Boolean;
var
 i:DWORD;
begin
 for i:=0 to len-1 do
 if (PByte(@buf)[i]<>0) then
 begin
  Exit(False);
 end;
 Result:=True;
end;

function IsRegistered(userId:Integer):Boolean; inline;
begin
 //sceUserServiceGetRegisteredUserIdList
 Result:=True;
end;

const
 posx_per2k:array[0..1] of Single=(3840.0,1920.0);
 posy_per2k:array[0..1] of Single=(2160.0,1080.0);

function imeDialogInitParamCheck(param   :pSceImeDialogParam;
                                 extended:pSceImeParamExtended;
                                 below_15:Boolean):Integer;
var
 extKeyboardMode:DWORD;
begin

 if (param=nil) then
 begin
  Exit(SCE_IME_ERROR_INVALID_ADDRESS);
 end;

 if (below_15) then
 begin

  if (SCE_IME_TYPE_NUMBER < DWORD(param^.ImeType)) and
     (1 < DWORD(param^.ImeType - 256)) then
  begin
   Exit(SCE_IME_ERROR_INVALID_TYPE);
  end;

  if not CheckOption_old(param^.option) then
  begin
   Exit(SCE_IME_ERROR_INVALID_OPTION);
  end;

  if not CheckLang_old(param^.supportedLanguages) then
  begin
   Exit(SCE_IME_ERROR_INVALID_SUPPORTED_LANGUAGES);
  end;

 end else
 begin

  if (SCE_IME_TYPE_NUMBER < DWORD(param^.ImeType)) then
  begin
   Exit(SCE_IME_ERROR_INVALID_TYPE);
  end;

  if not CheckOption_new(param^.option) then
  begin
   Exit(SCE_IME_ERROR_INVALID_OPTION);
  end;

  if not CheckLang_new(param^.supportedLanguages) then
  begin
   Exit(SCE_IME_ERROR_INVALID_SUPPORTED_LANGUAGES);
  end;

 end;

 //

 if (p_proc.p_sdk_version < $1500000) then
 begin
  if (1919 < param^.posx) then
  begin
   Exit(SCE_IME_ERROR_INVALID_POSX);
  end;
  if (1079 < param^.posy) then
  begin
   Exit(SCE_IME_ERROR_INVALID_POSY);
  end;
 end else
 begin
  if (param^.posx < 0.0) then
  begin
   Exit(SCE_IME_ERROR_INVALID_POSX);
  end;
  if (posx_per2k[ord((param^.option and SCE_IME_OPTION_USE_OVER_2K_COORDINATES)=0)] <= param^.posx) then
  begin
   Exit(SCE_IME_ERROR_INVALID_POSX);
  end;
  if (param^.posy < 0.0) then
  begin
   Exit(SCE_IME_ERROR_INVALID_POSY);
  end;
  if (posy_per2k[ord((param^.option and SCE_IME_OPTION_USE_OVER_2K_COORDINATES)=0)] <= param^.posy) then
  begin
   Exit(SCE_IME_ERROR_INVALID_POSY);
  end;
 end;

 //

 if (DWORD(param^.horizontalAlignment) > 2) then
 begin
  Exit(SCE_IME_ERROR_INVALID_HORIZONTAL_ALIGNMENT);
 end;
 if (DWORD(param^.verticalAlignment) > 2) then
 begin
  Exit(SCE_IME_ERROR_INVALID_VERTICAL_ALIGNMENT);
 end;

 //

 if (((not param^.option) and 5)<>0) and
    (
     ((param^.option and SCE_IME_OPTION_PASSWORD)=0) or
     (SCE_IME_TYPE_MAIL < DWORD(param^.ImeType)) or
     (param^.ImeType=SCE_IME_TYPE_BASIC_LATIN)
    ) and
    (
     ((param^.option and SCE_IME_OPTION_MULTILINE)=0) or
     (2 < DWORD(param^.ImeType - 2))
    ) then
 begin
  //
 end else
 begin
  Exit(SCE_IME_ERROR_INVALID_PARAM);
 end;

 //

 if (below_15) then
 begin
  if (1 < DWORD(param^.userId +   1)) and
     (1 < DWORD(param^.userId - $fe)) and
     (not IsRegistered(param^.userId)) then
  begin
   Exit(SCE_IME_ERROR_INVALID_USER_ID);
  end;
 end else
 begin
  if (not IsRegistered(param^.userId)) then
  begin
   Exit(SCE_IME_ERROR_INVALID_USER_ID);
  end;
  if (p_proc.p_sdk_version < $1500000) and
     (param^.userId=-1) then
  begin
   Exit(SCE_IME_ERROR_INVALID_USER_ID);
  end;
 end;

 //

 if not CheckReserved(param^.reserved,sizeof(param^.reserved)) then
 begin
  Exit(SCE_IME_ERROR_INVALID_RESERVED);
 end;

 if (param^.inputTextBuffer=nil) then
 begin
  Exit(SCE_IME_ERROR_INVALID_INPUT_TEXT_BUFFER);
 end;

 //

 if (extended=nil) then
 begin
  Exit(0);
 end;

 if (SCE_IME_PANEL_PRIORITY_ACCENT < DWORD(extended^.priority)) then
 begin
  Exit(SCE_IME_ERROR_INVALID_EXTENDED);
 end;

 if (p_proc.p_sdk_version < $1500000) then
 begin
  if ((extended^.option and $ffffff20)<>0) then
  begin
   Exit(SCE_IME_ERROR_INVALID_EXTENDED);
  end;
 end else
 begin
  if (p_proc.p_sdk_version < $2500000) then
  begin
   if not CheckExtendedOption_old(extended^.option) then
   begin
    Exit(SCE_IME_ERROR_INVALID_EXTENDED);
   end;
  end else
  begin
   if not CheckExtendedOption_new(extended^.option) then
   begin
    Exit(SCE_IME_ERROR_INVALID_EXTENDED);
   end;
  end;
 end;

 if not CheckReserved(extended^.reserved,sizeof(extended^.reserved)) then
 begin
  Exit(SCE_IME_ERROR_INVALID_EXTENDED);
 end;

 if (p_proc.p_sdk_version < $1560000) then
 begin
  if (extended^.extKeyboardFilter<>nil) then
  begin
   Exit(SCE_IME_ERROR_INVALID_EXTENDED);
  end;
  if (extended^.disableDevice<>0) then
  begin
   Exit(SCE_IME_ERROR_INVALID_EXTENDED);
  end;
  extKeyboardMode:=extended^.extKeyboardMode;
 end else
 begin
  extKeyboardMode:=extended^.extKeyboardMode and $e3fffffc;
 end;

 if (extKeyboardMode<>0) then
 begin
  Exit(SCE_IME_ERROR_INVALID_EXTENDED);
 end;

 if (7 < DWORD(extended^.disableDevice)) then
 begin
  Exit(SCE_IME_ERROR_INVALID_EXTENDED);
 end;

 Result:=0;
end;

function ps4_sceImeDialogGetPanelSizeExtended(param   :pSceImeDialogParam;
                                              extended:pSceImeParamExtended;
                                              p_width :PDWORD;
                                              p_height:PDWORD):Integer; forward;

Procedure CopyParams(g_dialog:TImeDialogClient;
                     param   :pSceImeDialogParam;
                     extended:pSceImeParamExtended);
begin
 g_dialog.data.userId             :=param^.userId             ;
 g_dialog.data.ImeType            :=param^.ImeType            ;
 g_dialog.data.supportedLanguages :=param^.supportedLanguages ;
 g_dialog.data.enterLabel         :=param^.enterLabel         ;
 g_dialog.filter.addr             :=param^.filter             ;
 g_dialog.data.option             :=param^.option             ;
 g_dialog.data.maxTextLength      :=param^.maxTextLength      ;
 //
 g_dialog.data.PosAndForm.PanelType          :=SCE_IME_PANEL_TYPE_DIALOG;
 g_dialog.data.PosAndForm.posx               :=param^.posx               ;
 g_dialog.data.PosAndForm.posy               :=param^.posy               ;
 g_dialog.data.PosAndForm.horizontalAlignment:=param^.horizontalAlignment;
 g_dialog.data.PosAndForm.verticalAlignment  :=param^.verticalAlignment  ;
 //
 ps4_sceImeDialogGetPanelSizeExtended(param,extended,
                                      @g_dialog.data.PosAndForm.width,
                                      @g_dialog.data.PosAndForm.height);
 //
 wcsncpy_s(@g_dialog.data.result.inputText,param^.inputTextBuffer,g_dialog.data.maxTextLength);
 wcsncpy_s(@g_dialog.data.placeholder     ,param^.placeholder    ,Length(g_dialog.data.placeholder));
 wcsncpy_s(@g_dialog.data.title           ,param^.title          ,Length(g_dialog.data.title));
 //
 g_dialog.output:=param^.inputTextBuffer;
 //
 if (extended<>nil) then
 begin
  g_dialog.data.ExtOption               :=extended^.option             ;
  g_dialog.data.colorBase               :=extended^.colorBase          ;
  g_dialog.data.colorLine               :=extended^.colorLine          ;
  g_dialog.data.colorTextField          :=extended^.colorTextField     ;
  g_dialog.data.colorPreedit            :=extended^.colorPreedit       ;
  g_dialog.data.colorButtonDefault      :=extended^.colorButtonDefault ;
  g_dialog.data.colorButtonFunction     :=extended^.colorButtonFunction;
  g_dialog.data.colorButtonSymbol       :=extended^.colorButtonSymbol  ;
  g_dialog.data.colorText               :=extended^.colorText          ;
  g_dialog.data.colorSpecial            :=extended^.colorSpecial       ;
  g_dialog.data.priority                :=extended^.priority           ;
  g_dialog.extKeyboardFilter            :=extended^.extKeyboardFilter  ;
  g_dialog.data.disableDevice           :=extended^.disableDevice      ;
  g_dialog.data.extKeyboardMode         :=extended^.extKeyboardMode    ;
  //
  strncpy_s(@g_dialog.data.additionalDictionaryPath,extended^.additionalDictionaryPath,Length(g_dialog.data.additionalDictionaryPath));
 end;
end;

function InvokeSync2(const msg:RawByteString;buf:Pointer;len:DWORD):Integer;
begin
 Result:=p_host_ipc.InvokeSync2(msg,buf,len);
 if (Result=-1) then
 begin
  Result:=SCE_IME_ERROR_CONNECTION_FAILED;
 end else
 if (Result<0) then
 begin
  Result:=SCE_IME_ERROR_NOT_ACTIVE;
 end;
end;

function InvokeSync(const msg:RawByteString;var Output:TIpcValue):Integer;
begin
 Output:=p_host_ipc.InvokeSync(msg);
 Result:=Output.GetQWORD;
 if (Result=-1) then
 begin
  Result:=SCE_IME_ERROR_CONNECTION_FAILED;
 end else
 if (Result<0) then
 begin
  Result:=SCE_IME_ERROR_NOT_ACTIVE;
 end;
end;

function ps4_sceImeDialogInit(param   :pSceImeDialogParam;
                              extended:pSceImeParamExtended):Integer;
begin

 if (g_dialog<>nil) then
 begin
  Exit(SCE_IME_ERROR_BUSY);
 end;

 if (param=nil) then
 begin
  Exit(SCE_IME_ERROR_INVALID_ADDRESS);
 end;

 Result:=imeDialogInitParamCheck(param,extended,p_proc.p_sdk_version < $1500000);
 if (Result<>0) then Exit;

 mtx_lock(g_ImeDialog_mtx);

  if (g_dialog<>nil) then
  begin
   Result:=SCE_IME_ERROR_BUSY;
  end else
  begin
   g_dialog:=TImeDialogClient.Create;
   CopyParams(g_dialog,param,extended);

   Assert(g_dialog.extKeyboardFilter=nil,'TODO:extKeyboardFilter');

   Result:=InvokeSync2('IME_DIALOG_OPEN',@g_dialog.data,sizeof(g_dialog.data));
   if (Result=0) then
   begin
    g_dialog.state:=dRUNNING;
   end else
   begin
    FreeAndNil(g_dialog);
   end;

  end;

 mtx_unlock(g_ImeDialog_mtx);
end;

function ps4_sceImeDialogTerm():Integer;
begin
 Result:=SCE_IME_DIALOG_ERROR_NOT_IN_USE;
 if (g_dialog=nil) then Exit;

 mtx_lock(g_ImeDialog_mtx);

  if (g_dialog<>nil) then
  begin
   Result:=InvokeSync2('IME_DIALOG_TERM',nil,0);

   if (Result=0) then
   begin
    FreeAndNil(g_dialog);
   end;
  end;

 mtx_unlock(g_ImeDialog_mtx);
end;

function ps4_sceImeDialogForceClose():Integer;
begin
 Result:=ps4_sceImeDialogTerm();
end;

function ps4_sceImeDialogAbort():Integer;
begin
 Result:=SCE_IME_DIALOG_ERROR_NOT_IN_USE;
 if (g_dialog=nil) then Exit;

 mtx_lock(g_ImeDialog_mtx);

  if (g_dialog<>nil) then
  begin

   if (g_dialog.state<>sFINISHED) then
   begin
    if (g_dialog.state<>dABORTED) then
    begin
     Result:=InvokeSync2('IME_DIALOG_ABORT',nil,0);
    end;
    g_dialog.state:=dABORTED;
   end;

  end;

 mtx_unlock(g_ImeDialog_mtx);
end;

//

function ExecuteTextFilter(
          addr:Pointer;
          outText      :PWideChar;
          outTextLength:PDWORD;
          srcText      :PWideChar;
          srcTextLength:DWORD
         ):Integer; external name 'ExecuteGuest';

function ps4_sceImeDialogGetStatus():Integer;
var
 Output:TIpcValue;
begin
 Result:=SCE_IME_DIALOG_STATUS_NONE;
 if (g_dialog=nil) then Exit;

 mtx_lock(g_ImeDialog_mtx);

  if (g_dialog<>nil) then
  begin
   if (g_dialog.state=sFINISHED) or
      (g_dialog.state=dABORTED) then
   begin
    Result:=SCE_IME_DIALOG_STATUS_FINISHED;
   end else
   begin

    Result:=SCE_IME_DIALOG_STATUS_RUNNING;
    if (InvokeSync2('IME_DIALOG_UPDATE',nil,0)=2) then
    begin
     g_dialog.state:=sFINISHED;
     Result:=SCE_IME_DIALOG_STATUS_FINISHED;
    end;

    if (g_dialog.filter.addr<>nil) then
    begin
     FillChar(g_dialog.filter.src,sizeof(g_dialog.filter.src),0);
     FillChar(g_dialog.filter.dst,sizeof(g_dialog.filter.dst),0);

     g_dialog.filter.src.result:=InvokeSync('IME_DIALOG_GETTEXT',Output);
     if (g_dialog.filter.src.result=0) then
     begin
      Output.MoveTo(@g_dialog.filter.src,sizeof(g_dialog.filter.src));
      Output.Free;

      g_dialog.filter.dst.result:=ExecuteTextFilter(
        g_dialog.filter.addr,
       @g_dialog.filter.dst.Text,
       @g_dialog.filter.dst.TextLength,
       @g_dialog.filter.src.Text,
        g_dialog.filter.src.TextLength);

      if (g_dialog.filter.dst.result=0) then
      begin
       InvokeSync2('IME_DIALOG_SETTEXT',@g_dialog.filter.dst,sizeof(g_dialog.filter.dst))
      end;

     end;

    end; //filter

   end; //state

  end;

 mtx_unlock(g_ImeDialog_mtx);
end;

function ps4_sceImeDialogGetResult(pResult:pSceImeDialogResult):Integer;
var
 Output:TIpcValue;
begin
 Result:=SCE_IME_DIALOG_ERROR_NOT_IN_USE;
 if (g_dialog=nil) then Exit;

 mtx_lock(g_ImeDialog_mtx);

  if (g_dialog<>nil) then
  begin
   Result:=SCE_IME_ERROR_INVALID_ADDRESS;
   if (pResult<>nil) then
   begin
    Result:=SCE_IME_ERROR_INVALID_RESERVED;
    if CheckReserved(pResult^.reserved,sizeof(pResult^.reserved)) then
    begin

     case g_dialog.state of
      dRUNNING:Result:=SCE_IME_DIALOG_ERROR_NOT_FINISHED;
      dABORTED:
               begin;
                pResult^.endstatus:=SCE_IME_DIALOG_END_STATUS_ABORTED;
                wcsncpy_s(g_dialog.output,g_dialog.data.result.inputText,g_dialog.data.maxTextLength);
                Result:=0;
               end;
      sFINISHED:
               begin
                Result:=InvokeSync('IME_DIALOG_RESULT',Output);
                if (Result=0) then
                begin
                 FillChar(g_dialog.data.result,sizeof(g_dialog.data.result),0);
                 Output.MoveTo(@g_dialog.data.result,sizeof(g_dialog.data.result));
                 //
                 pResult^.endstatus:=g_dialog.data.result.endstatus;
                 wcsncpy_s(g_dialog.output,g_dialog.data.result.inputText,g_dialog.data.maxTextLength);
                end;
                Output.Free;
               end;
      else;
     end;

    end;
   end;
  end;

 mtx_unlock(g_ImeDialog_mtx);
end;

function ps4_sceImeDialogGetPanelPositionAndForm(posForm:pSceImePositionAndForm):Integer;
var
 Output:TIpcValue;
begin
 Result:=SCE_IME_DIALOG_ERROR_NOT_IN_USE;
 if (g_dialog=nil) then Exit;

 mtx_lock(g_ImeDialog_mtx);

  if (g_dialog<>nil) then
  begin
   Result:=SCE_IME_ERROR_INVALID_ADDRESS;
   if (posForm<>nil) then
   begin
    //
    if (g_dialog.state=dRUNNING) then
    begin
     Result:=InvokeSync('IME_DIALOG_GETPOS',Output);
     if (Result>=0) then
     begin
      Output.MoveTo(@g_dialog.data.PosAndForm,sizeof(g_dialog.data.PosAndForm));
     end;
     Output.Free;
    end;
    //
    posForm^.PanelType          :=g_dialog.data.PosAndForm.PanelType;
    posForm^.posx               :=g_dialog.data.PosAndForm.posx;
    posForm^.posy               :=g_dialog.data.PosAndForm.posy;
    posForm^.horizontalAlignment:=g_dialog.data.PosAndForm.horizontalAlignment;
    posForm^.verticalAlignment  :=g_dialog.data.PosAndForm.verticalAlignment;
    posForm^.width              :=g_dialog.data.PosAndForm.width;
    posForm^.height             :=g_dialog.data.PosAndForm.height;
    //
    Result:=0;
   end;
  end;

 mtx_unlock(g_ImeDialog_mtx);
end;

//

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

 if (SCE_IME_TYPE_NUMBER < DWORD(param^.ImeType)) then
 begin
  Exit(SCE_IME_ERROR_INVALID_TYPE);
 end;

 if not CheckOption_new(param^.option) then
 begin
  Exit(SCE_IME_ERROR_INVALID_OPTION);
 end;

 if not CheckLang_new(param^.supportedLanguages) then
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

 if (SCE_IME_TYPE_NUMBER < DWORD(param^.ImeType)) then
 begin
  Exit(SCE_IME_ERROR_INVALID_TYPE);
 end;

 if not CheckOption_new(param^.option) then
 begin
  Exit(SCE_IME_ERROR_INVALID_OPTION);
 end;

 if not CheckLang_new(param^.supportedLanguages) then
 begin
  Exit(SCE_IME_ERROR_INVALID_SUPPORTED_LANGUAGES);
 end;

 if (extended=nil) then
 begin
  Exit(ps4_sceImeDialogGetPanelSize(param,p_width,p_height));
 end;

 if (p_proc.p_sdk_version > $16fffff) then
 begin
  if not CheckExtendedOption_new(extended^.option) then
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
 lib.set_proc($6D7E07FACC4F23FA,@ps4_sceImeDialogForceClose);
 lib.set_proc($A019B0E31AE67CAB,@ps4_sceImeDialogAbort);
 lib.set_proc($2000E60F8B527016,@ps4_sceImeDialogGetStatus);
 lib.set_proc($C74D63C6EFAFC657,@ps4_sceImeDialogGetResult);
 lib.set_proc($F23AB3CCF8A8625F,@ps4_sceImeDialogGetPanelPositionAndForm);
 lib.set_proc($C2AB09BD15F0979F,@ps4_sceImeDialogGetPanelSize);
 lib.set_proc($0910FE8D212B1094,@ps4_sceImeDialogGetPanelSizeExtended);

 mtx_init(g_ImeDialog_mtx,'g_ImeDialog_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceImeDialog.prx',@Load_libSceImeDialog);

end.

