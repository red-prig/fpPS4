
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

{$DEFINE IS_LOG_TRACE   :=(_var_log_filter_Trace   .fcbs(@_var_log_filter_Trace   ,LOG_FILE))}
{$DEFINE IS_LOG_DEBUG   :=(_var_log_filter_Debug   .fcbs(@_var_log_filter_Debug   ,LOG_FILE))}
{$DEFINE IS_LOG_INFO    :=(_var_log_filter_Info    .fcbs(@_var_log_filter_Info    ,LOG_FILE))}
{$DEFINE IS_LOG_WARNING :=(_var_log_filter_Warning .fcbs(@_var_log_filter_Warning ,LOG_FILE))}
{$DEFINE IS_LOG_ERROR   :=(_var_log_filter_Error   .fcbs(@_var_log_filter_Error   ,LOG_FILE))}
{$DEFINE IS_LOG_CRITICAL:=(_var_log_filter_Critical.fcbs(@_var_log_filter_Critical,LOG_FILE))}

{$DEFINE LOG_TRACE   :=if IS_LOG_TRACE    then Writeln}
{$DEFINE LOG_DEBUG   :=if IS_LOG_DEBUG    then Writeln}
{$DEFINE LOG_INFO    :=if IS_LOG_INFO     then Writeln}
{$DEFINE LOG_WARNING :=if IS_LOG_WARNING  then Writeln}
{$DEFINE LOG_ERROR   :=if IS_LOG_ERROR    then Writeln}
{$DEFINE LOG_CRITICAL:=if IS_LOG_CRITICAL then Writeln}

{$ENDIF}
