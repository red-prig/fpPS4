
type
 _t_log_filter_cb=function(self:Pointer;fname:Pchar):Boolean; register;

 _p_log_filter_node=^_t_log_filter_node;
 _t_log_filter_node=packed object
  next:Pointer;
  fcbs:_t_log_filter_cb;
 end;

{$IFNDEF LOG_DECLARE}

function _log_filter_default_Trace   (self:Pointer;fname:Pchar):Boolean; register; external;
function _log_filter_default_Debug   (self:Pointer;fname:Pchar):Boolean; register; external;
function _log_filter_default_Info    (self:Pointer;fname:Pchar):Boolean; register; external;
function _log_filter_default_Warning (self:Pointer;fname:Pchar):Boolean; register; external;
function _log_filter_default_Error   (self:Pointer;fname:Pchar):Boolean; register; external;
function _log_filter_default_Critical(self:Pointer;fname:Pchar):Boolean; register; external;

var
 _var_log_filter_Trace   :_t_log_filter_node=(fcbs:@_log_filter_default_Trace   );
 _var_log_filter_Debug   :_t_log_filter_node=(fcbs:@_log_filter_default_Debug   );
 _var_log_filter_Info    :_t_log_filter_node=(fcbs:@_log_filter_default_Info    );
 _var_log_filter_Warning :_t_log_filter_node=(fcbs:@_log_filter_default_Warning );
 _var_log_filter_Error   :_t_log_filter_node=(fcbs:@_log_filter_default_Error   );
 _var_log_filter_Critical:_t_log_filter_node=(fcbs:@_log_filter_default_Critical);

{$MACRO ON}

{$DEFINE LOG_TRACE   :=if _var_log_filter_Trace   .fcbs(@_var_log_filter_Trace   ,LOG_FILE) then Writeln}
{$DEFINE LOG_DEBUG   :=if _var_log_filter_Debug   .fcbs(@_var_log_filter_Debug   ,LOG_FILE) then Writeln}
{$DEFINE LOG_INFO    :=if _var_log_filter_Info    .fcbs(@_var_log_filter_Info    ,LOG_FILE) then Writeln}
{$DEFINE LOG_WARNING :=if _var_log_filter_Warning .fcbs(@_var_log_filter_Warning ,LOG_FILE) then Writeln}
{$DEFINE LOG_ERROR   :=if _var_log_filter_Error   .fcbs(@_var_log_filter_Error   ,LOG_FILE) then Writeln}
{$DEFINE LOG_CRITICAL:=if _var_log_filter_Critical.fcbs(@_var_log_filter_Critical,LOG_FILE) then Writeln}

{$ENDIF}
