unit ps4_libSceNpCommerce;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

uses
 kern_mtx,
 subr_dynlib,
 kern_proc,
 ps4_libSceUserService,
 ps4_libSceCommonDialog,
 libkern;

Const
 SCE_NP_COMMERCE_DIALOG_NUM_TARGETS_MAX  =10;
 SCE_NP_COMMERCE_DIALOG_TARGET_MAX_LENGTH=41;

 //SceNpCommerceDialogMode
 SCE_NP_COMMERCE_DIALOG_MODE_CATEGORY    =0;
 SCE_NP_COMMERCE_DIALOG_MODE_PRODUCT     =1;
 SCE_NP_COMMERCE_DIALOG_MODE_PRODUCT_CODE=2;
 SCE_NP_COMMERCE_DIALOG_MODE_CHECKOUT    =3;
 SCE_NP_COMMERCE_DIALOG_MODE_DOWNLOADLIST=4;
 SCE_NP_COMMERCE_DIALOG_MODE_PLUS        =5;

 //result
 //SCE_COMMON_DIALOG_RESULT_OK
 //SCE_COMMON_DIALOG_RESULT_USER_CANCELED
 SCE_NP_COMMERCE_DIALOG_RESULT_PURCHASED=2;

 //SceNpCommercePsStoreIconPos
 SCE_NP_COMMERCE_PS_STORE_ICON_CENTER=0;
 SCE_NP_COMMERCE_PS_STORE_ICON_LEFT  =1;
 SCE_NP_COMMERCE_PS_STORE_ICON_RIGHT =2;

 //SceNpCommercePsStoreIconLayout
 SCE_NP_COMMERCE_PS_STORE_ICON_LAYOUT_DEFAULT                      =0;
 SCE_NP_COMMERCE_PS_STORE_ICON_LAYOUT_FOLLOW_DISPLAY_SAFE_AREA_INFO=1;
 SCE_NP_COMMERCE_PS_STORE_ICON_LAYOUT_FIXED_SCALE_90PERCENT        =2;

type
 SceNpCommerceDialogMode=Integer;
 SceNpCommercePsStoreIconPos=Integer;
 SceNpCommercePsStoreIconLayout=Integer;

{$CALLING default}

type
 TNpCommerceTarget=array[0..41] of Char;

 TNpCommerceDialogOpen=record
  userId      :Integer;
  mode        :Byte; //SceNpCommerceDialogMode
  serviceLabel:Byte; //SceNpServiceLabel
  numTargets  :Byte;
  targets     :array[0..9] of TNpCommerceTarget;
  features    :QWORD;
  userData    :Pointer;
 end;

 TNpCommerceDialogResult=record
  resultId  :Integer;
  authorized:Boolean;
 end;

 TNpCommerceDialogClient=class(TCommonDialogClient)
  data:TNpCommerceDialogOpen;
 end;

implementation

{$I log.inc}{$DEFINE LOG_FILE:={$I %FILE%}}

var
 g_NpCommerce_mtx:mtx;
 g_client        :TNpCommerceDialogClient=nil;

{$CALLING SysV_ABI_CDecl}

type
 pSceNpCommerceDialogParam=^SceNpCommerceDialogParam;
 SceNpCommerceDialogParam=packed record
  baseParam   :SceCommonDialogBaseParam;
  size        :Integer;
  userId      :Integer;
  mode        :SceNpCommerceDialogMode;
  serviceLabel:DWORD;   //SceNpServiceLabel
  targets     :PPChar;
  numTargets  :DWORD;
  align       :Integer;
  features    :QWORD;
  userData    :Pointer;
  reserved    :array[0..31] of Byte;
 end;

 pSceNpCommerceDialogResult=^SceNpCommerceDialogResult;
 SceNpCommerceDialogResult=packed record
  result    :Integer;
  authorized:Boolean;
  align1    :Byte;
  align2    :Word;
  userData  :Pointer;
  reserved  :array[0..31] of Byte;
 end;

function ps4_sceNpCommerceDialogInitialize():Integer;
var
 client:TNpCommerceDialogClient;
begin
 LOG_INFO('sceNpCommerceDialogInitialize');

 mtx_lock(g_NpCommerce_mtx);

  Result:=SCE_COMMON_DIALOG_ERROR_ALREADY_INITIALIZED;
  if (g_client=nil) then
  begin

   Result:=SCE_COMMON_DIALOG_ERROR_BUSY;
   if (not ps4_sceCommonDialogIsUsed) then
   begin
    client:=TNpCommerceDialogClient.Create;

    Result:=client.launchCmnDialog();

    if (Result<>0) then
    begin
     client.Free;
    end else
    begin
     g_client:=client;
    end;

   end;

  end;

 mtx_unlock(g_NpCommerce_mtx);
end;

function IsLoggedIn(userId:Integer):Boolean; inline;
begin
 Result:=(ps4_sceUserServiceIsLoggedIn(userId)=1);
end;

function ps4_sceNpCommerceDialogOpen(param:pSceNpCommerceDialogParam):Integer;
label
 _unlock;
var
 i:Integer;
begin
 Result:=0;

 if (param=nil) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_ARG_NULL);
 end;

 if CheckBaseParam(@param^.baseParam)<>0 then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);
 end;

 LOG_INFO('sceNpCommerceDialogOpen');

 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_NpCommerce_mtx);

  if (g_client<>nil) then
  begin

   Result:=(SCE_COMMON_DIALOG_ERROR_PARAM_INVALID);

   if (p_proc.p_sdk_version > $16fffff) then
   begin
    if (param^.size<>$80) then
    begin
     goto _unlock;
    end;
    //
    if CheckReserved(param^.reserved,SizeOf(param^.reserved))<>0 then
    begin
     goto _unlock;
    end;
    //
    if not IsLoggedIn(param^.userId) then
    begin
     goto _unlock;
    end;
    //
    if (DWORD(param^.serviceLabel)>7) then
    begin
     goto _unlock;
    end;
   end;

   case param^.mode of
    SCE_NP_COMMERCE_DIALOG_MODE_CATEGORY:
      begin
       if (DWORD(param^.numTargets)>1) then goto _unlock;
       //
       if (param^.numTargets<>0) then
       begin
        if (param^.targets=nil) then goto _unlock;
       end else
       begin
        if (param^.targets<>nil) then goto _unlock;
       end;
      end;
    SCE_NP_COMMERCE_DIALOG_MODE_PRODUCT:
      begin
       if (param^.numTargets<>1) then goto _unlock;
       if (param^.targets=nil) then goto _unlock;
      end;
    SCE_NP_COMMERCE_DIALOG_MODE_PRODUCT_CODE:
      begin
       if (DWORD(param^.numTargets)>1) then goto _unlock;
       //
       if (param^.numTargets<>1) then
       begin
        if (p_proc.p_sdk_version > $16fffff) then
        begin
         if (param^.targets<>nil) then goto _unlock;
        end;
       end else
       begin
        if (param^.targets=nil) then goto _unlock;
       end;
      end;
    SCE_NP_COMMERCE_DIALOG_MODE_CHECKOUT:
      begin
       if (param^.numTargets=0) then goto _unlock;
       if (param^.targets=nil) then goto _unlock;
       if (DWORD(param^.numTargets)>10) then goto _unlock;
      end;
    SCE_NP_COMMERCE_DIALOG_MODE_DOWNLOADLIST:
      begin
       if (param^.numTargets<>0) then
       begin
        if (param^.targets=nil) or (DWORD(param^.numTargets)>10) then goto _unlock;
       end else
       begin
        if (p_proc.p_sdk_version > $16fffff) then
        begin
         if (param^.targets<>nil) then goto _unlock;
        end;
       end;
      end;
    SCE_NP_COMMERCE_DIALOG_MODE_PLUS:
      begin
       if (param^.numTargets<>0) then goto _unlock;
       //
       if (p_proc.p_sdk_version < $3500000) then
       begin
        if ((param^.features and 3)=0) or
           ( (p_proc.p_sdk_version > $16fffff) and (param^.features>3) ) then
        begin
         goto _unlock;
        end;
       end else
       begin
        if (param^.features<>1) then goto _unlock;
       end;
      end;
    else
      begin
       goto _unlock;
      end;
   end;

   FillChar(g_client.data,SizeOf(g_client.data),0);

   g_client.data.userId      :=param^.userId;
   g_client.data.mode        :=param^.mode;
   g_client.data.serviceLabel:=param^.serviceLabel;
   g_client.data.numTargets  :=param^.numTargets;

   if (param^.numTargets<>0) then
   for i:=0 to param^.numTargets-1 do
   begin
    strncpy_s(@g_client.data.targets[i],param^.targets[i],42);
   end;

   g_client.data.features:=param^.features;
   g_client.data.userData:=param^.userData;

   Result:=g_client.Open('NPCOMMERCE_DIALOG_OPEN',@g_client.data,SizeOf(g_client.data));
  end;

 _unlock:
 mtx_unlock(g_NpCommerce_mtx);
end;

function ps4_sceNpCommerceDialogClose():Integer;
begin
 LOG_INFO('sceNpCommerceDialogClose');

 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_NpCommerce_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_RUNNING;
   if (not g_client.isInitializedStatus) then
   if (not g_client.isFinish) then
   begin
    Result:=g_client.Close(nil,0);
   end;
  end;

 mtx_unlock(g_NpCommerce_mtx);
 //
end;

function ps4_sceNpCommerceDialogTerminate():Integer;
begin
 LOG_INFO('sceNpCommerceDialogTerminate');

 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_NpCommerce_mtx);

  if (g_client<>nil) then
  begin
   g_client.Terminate;
   g_client:=nil;
   Result:=0;
  end;

 mtx_unlock(g_NpCommerce_mtx);
 //
end;

function ps4_sceNpCommerceDialogUpdateStatus():Integer;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;

 mtx_lock(g_NpCommerce_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
   if (not g_client.isInitializedStatus) then
   begin
    Result:=SCE_COMMON_DIALOG_STATUS_FINISHED;
    if (not g_client.isFinish) then
    begin
     g_client.updateState;
     if (g_client.isFinish) then
     begin
      Result:=SCE_COMMON_DIALOG_STATUS_FINISHED;
     end else
     begin
      Result:=SCE_COMMON_DIALOG_STATUS_RUNNING;
     end;
    end;
   end;
  end;

 mtx_unlock(g_NpCommerce_mtx);
 //
end;

function ps4_sceNpCommerceDialogGetStatus():Integer;
begin
 Result:=SCE_COMMON_DIALOG_STATUS_NONE;

 mtx_lock(g_NpCommerce_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_STATUS_INITIALIZED;
   if (not g_client.isInitializedStatus) then
   begin
    if (g_client.isFinish) then
    begin
     Result:=SCE_COMMON_DIALOG_STATUS_FINISHED;
    end else
    begin
     Result:=SCE_COMMON_DIALOG_STATUS_RUNNING;
    end;
   end;
  end;

 mtx_unlock(g_NpCommerce_mtx);
 //
end;

function ps4_sceNpCommerceDialogGetResult(pResult:pSceNpCommerceDialogResult):Integer;
var
 rzdata:TNpCommerceDialogResult;
begin
 if (pResult=nil) then
 begin
  Exit(SCE_COMMON_DIALOG_ERROR_ARG_NULL);
 end;

 Result:=SCE_COMMON_DIALOG_ERROR_NOT_INITIALIZED;
 mtx_lock(g_NpCommerce_mtx);

  if (g_client<>nil) then
  begin
   Result:=SCE_COMMON_DIALOG_ERROR_NOT_FINISHED;
   if (g_client.isFinish) then
   begin
    rzdata:=Default(TNpCommerceDialogResult);
    g_client.getFinishData(@rzdata,sizeof(rzdata));

    pResult^.result    :=rzdata.resultId;
    pResult^.authorized:=rzdata.authorized;
    pResult^.userData  :=g_client.data.userData;

    if (p_proc.p_sdk_version > $16fffff) then
    begin
     FillChar(pResult^.reserved,SizeOf(pResult^.reserved),0);
    end;

    Result:=rzdata.resultId;
   end;
  end;

 mtx_unlock(g_NpCommerce_mtx);
 //
end;

function ps4_sceNpCommerceShowPsStoreIcon(pos:SceNpCommercePsStoreIconPos):Integer;
begin
 LOG_INFO('sceNpCommerceShowPsStoreIcon:',pos);
 Result:=0;
end;

function ps4_sceNpCommerceHidePsStoreIcon():Integer;
begin
 LOG_INFO('sceNpCommerceHidePsStoreIcon');
 Result:=0;
end;

function ps4_sceNpCommerceSetPsStoreIconLayout(layout:SceNpCommercePsStoreIconLayout):Integer;
begin
 LOG_INFO('sceNpCommerceSetPsStoreIconLayout:',layout);
 Result:=0;
end;

//

function dt_fini(args:QWORD;argp,addr:Pointer):Integer;
begin
 ps4_sceNpCommerceDialogTerminate();
 Result:=0;
end;

//

{$WARN 4110 off}
function Load_libSceNpCommerce(name:pchar):p_lib_info;
var
 lib:TLIBRARY;
begin
 Result:=obj_new_int('libSceNpCommerce');

 Result^.fini_proc_addr.native:=@dt_fini;

 lib:=Result^.add_lib('libSceNpCommerce');
 lib.set_proc($D1A4766969906A5E,@ps4_sceNpCommerceDialogInitialize);
 lib.set_proc($0DF4820D10371236,@ps4_sceNpCommerceDialogOpen);
 lib.set_proc($354DDC9061CC157A,@ps4_sceNpCommerceDialogClose);
 lib.set_proc($9BF23DD806F9D16F,@ps4_sceNpCommerceDialogTerminate);
 lib.set_proc($2D1E5CC0530C0951,@ps4_sceNpCommerceDialogUpdateStatus);
 lib.set_proc($0826C2FA5AAABC5D,@ps4_sceNpCommerceDialogGetStatus);
 lib.set_proc($AF8D9B59C41BB596,@ps4_sceNpCommerceDialogGetResult);
 lib.set_proc($0C79B0B1AE92F137,@ps4_sceNpCommerceShowPsStoreIcon);
 lib.set_proc($76CA8256C34CD198,@ps4_sceNpCommerceHidePsStoreIcon);
 lib.set_proc($B8A4C35BC864FEDB,@ps4_sceNpCommerceSetPsStoreIconLayout);

 mtx_init(g_NpCommerce_mtx,'g_NpCommerce_mtx');
end;

var
 stub:t_int_file;

initialization
 RegisteredInternalFile(stub,'libSceNpCommerce.prx',@Load_libSceNpCommerce);

end.

