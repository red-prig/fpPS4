unit logging;

{$mode ObjFPC}{$H+}

interface

type
 t_log_level=(Trace,Debug,Info,Warning,Error,Critical,Off);

procedure set_log_filter(const log_filter:RawByteString);

//"*:Off" "lib:Info"
//"class:level"
//LOG_TRACE
//LOG_DEBUG
//LOG_INFO
//LOG_WARNING
//LOG_ERROR
//LOG_CRITICAL

implementation

uses
 sysutils;

{$OPTIMIZATION LEVEL3}
{$OPTIMIZATION REGVAR}

{$DEFINE LOG_DECLARE}
{$I log.inc}

type
 p_wildcards_blob=^t_wildcards_blob;
 t_wildcards_blob=packed record
  lastw:Pointer;
  level:Byte;       //t_log_level
  dataw:record end; //p_log_wildcard
 end;

 p_log_wildcard=^t_log_wildcard;
 t_log_wildcard=record
  level:Byte;       //t_log_level
  wildw:record end; //Pchar
 end;

const
 default_rule:t_wildcards_blob=(lastw:@default_rule.dataw;level:Byte(Info);dataw:());

var
 wild_rules:p_wildcards_blob=@default_rule;

 global_epoch:Integer=0;
 epoch_count :array[0..1] of Integer=(0,0);

type
 t_filtres=object
  tail_:_p_log_filter_node;
  stub_:_t_log_filter_node;
  head_:_p_log_filter_node;
  procedure Push(Node:_p_log_filter_node);
  procedure Reset(cbs:_t_log_filter_cb);
 end;

function XCHG(Var addr:Pointer;New:Pointer):Pointer; inline;
begin
 Result:=System.InterLockedExchange(addr,New);
end;

Procedure store_release(Var addr:Pointer;v:Pointer); inline;
begin
 WriteBarrier;
 addr:=v;
end;

function load_consume(Var addr:Pointer):Pointer; inline;
begin
 ReadDependencyBarrier;
 Result:=addr;
end;

Procedure store_release(Var addr:Integer;v:Integer); inline;
begin
 WriteBarrier;
 addr:=v;
end;

function load_consume(Var addr:Integer):Integer; inline;
begin
 ReadDependencyBarrier;
 Result:=addr;
end;

procedure t_filtres.Push(Node:_p_log_filter_node);
Var
 prev:_p_log_filter_node;
begin
 if not Assigned(Node) then Exit;
 store_release(Node^.next,nil);
 prev:=XCHG(head_,Node);
 store_release(prev^.next,Node);
end;

procedure t_filtres.Reset(cbs:_t_log_filter_cb);
Var
 tail,n:_p_log_filter_node;
begin
 tail:=tail_;
 if not Assigned(tail) then Exit;

 n:=load_consume(tail^.next);
 if (tail=@stub_) then
 begin
  if not Assigned(n) then Exit;
  tail:=n;
 end;

 n:=tail;
 while (n<>nil) and (n<>@stub_) do
 begin
  WriteBarrier;
  n^.fcbs:=cbs;
  n:=load_consume(n^.next);
 end;

end;

var
 filtres:array[t_log_level] of t_filtres=(
  (tail_:@filtres[Trace   ].stub_;head_:@filtres[Trace   ].stub_),
  (tail_:@filtres[Debug   ].stub_;head_:@filtres[Debug   ].stub_),
  (tail_:@filtres[Info    ].stub_;head_:@filtres[Info    ].stub_),
  (tail_:@filtres[Warning ].stub_;head_:@filtres[Warning ].stub_),
  (tail_:@filtres[Error   ].stub_;head_:@filtres[Error   ].stub_),
  (tail_:@filtres[Critical].stub_;head_:@filtres[Critical].stub_),
  (tail_:@filtres[Off     ].stub_;head_:@filtres[Off     ].stub_)
 );

const
 level_string:array[t_log_level] of PChar=('Trace', 'Debug',  'Info',  'Warning',  'Error', 'Critical', 'Off');

function lowercase(c:Char):Char; inline;
begin
 if (c in ['A'..'Z']) then
 begin
  Exit(char(ord(c)+32));
 end else
 begin
  Exit(c);
 end;
end;

function CharMatch(s,p:Char): Boolean; inline;
begin
 Result:=(lowercase(s)=lowercase(p));
end;

function IsWild(i,w:PChar;max:DWORD):Boolean;
var
 star  :PChar;
 s_star:PChar;
 i_last:PChar;
begin
 if (w^=#0) then
 begin
  Exit(i^=#0);
 end;

 star  :=nil;
 s_star:=nil;
 i_last:=i+max;

 while (i<>i_last) and (i^<>#0) do
 begin
  if (w^='*') then
  begin
   star  :=w;
   s_star:=i;
   Inc(w);
  end else
  if (w^='?') or CharMatch(i^,w^) then
  begin
   Inc(i);
   Inc(w);
  end else
  if (star<>nil) then
  begin
   Inc(s_star);
   i:=s_star;
   w:=star+1;
  end else
  begin
   Exit(False);
  end;
 end;

 while (w^='*') do Inc(w);

 Result:=(w^=#0);
end;

function _get_log_level(const param:pchar):Integer;
var
 i:t_log_level;
begin
 Result:=-1;
 for i:=Low(t_log_level) to High(t_log_level) do
 if IsWild(level_string[i],param,8) then
 begin
  Exit(ord(i));
 end;
end;


type
 t_pchar_pos=packed record
  curr:Pchar;
  last:Pchar;
 end;

 t_params_concat=packed object
  _file :array[0..62] of Char;
  _level:array[0..8] of Char;
  params:array[0..1] of t_pchar_pos;
  procedure Init; inline;
  procedure Add(b:boolean;c:Char); inline;
  procedure Final; inline;
  function  get_file_len:Byte; inline;
  procedure Reinit; inline;
 end;

procedure t_params_concat.Init; inline;
begin
 params[0].curr:=@_file;
 params[1].curr:=@_level;
 //
 params[0].last:=@_file [High(_file )];
 params[1].last:=@_level[High(_level)];
end;

procedure t_params_concat.Add(b:boolean;c:Char); inline;
begin
 with params[ord(b)] do
 if (curr<>last) then
 begin
  curr^:=c;
  Inc(curr);
 end;
end;

procedure t_params_concat.Final; inline;
begin
 params[0].curr^:=#0;
 params[1].curr^:=#0;
end;

function t_params_concat.get_file_len:Byte; inline;
begin
 Result:=PChar(params[0].curr)-PChar(@_file)+1;
end;

procedure t_params_concat.Reinit; inline;
begin
 params[0].curr:=@_file;
 params[1].curr:=@_level;
end;

Procedure ResetAll; forward;

procedure set_log_filter(const log_filter:RawByteString);
var
 p:pchar;
 c,e,d:char;

 _output:p_wildcards_blob;
 _params:t_params_concat;

 old_epoch:Integer;
 new_epoch:Integer;

 Function Alloc(s:Integer):Pointer;
 var
  len:QWORD;
 begin
  len:=QWORD(_output^.lastw)-QWORD(_output);
  ReAllocMem(_output,len+s);
  _output^.lastw:=Pointer(_output)+(len+s);
  Result:=Pointer(_output)+len;
 end;

 procedure Add;
 var
  i,s:Integer;
  w:p_log_wildcard;
 begin
  i:=_get_log_level(@_params._level);

  if (i<>-1) then
  begin
   if IsWild('',@_params._file,1) then
   begin
    _output^.level:=i;
   end else
   begin
    s:=_params.get_file_len;
    w:=Alloc(sizeof(t_log_wildcard)+s);

    with w^ do
    begin
     level:=i;
     Move(_params._file,wildw,s);
    end;

   end;
  end;

 end;

begin
 _output:=AllocMem(sizeof(t_log_wildcard));
 _output^:=default_rule;
 _output^.lastw:=_output+1;

 _params.Init;

 p:=@log_filter[1];
 e:=#0;
 d:=#0;

 while True do
 begin
  c:=p^;

  if (c=#0) then Break;

  if (c=e) then
  begin
   e:=#0;
  end else
  case c of
   '"',
   '''':e:=c;
   ':' :d:=c;
   ' ' :if (e=#0) then
        begin
         _params.Final;

         //Writeln(_params._file,'|',_params._level,' ',_get_log_level(@_params._level));

         Add;

         _params.Reinit;
         d:=#0;
        end;
   else
    _params.Add(d<>#0,c);
  end;

  inc(p);
 end;

 _params.Final;

 //Writeln(_params._file,'|',_params._level,' ',_get_log_level(@_params._level));

 Add;

 //
 _output:=XCHG(Pointer(wild_rules),_output);

 old_epoch:=load_consume(global_epoch);
 new_epoch:=old_epoch xor 1;
 store_release(global_epoch,new_epoch);

 if (_output<>nil) and (_output<>@default_rule) then
 begin
  while (load_consume(epoch_count[old_epoch])<>0) do sleep(100);

  FreeMem(_output);
 end;

 //
 ResetAll;
end;

function DetectExt(i:Pchar):DWORD;
var
 p,e:Pchar;
begin
 p:=i;
 e:=nil;

 while (p^<>#0) do
 begin
  if (p^='.') then e:=p;
  Inc(p);
 end;

 if (e=nil) then e:=p;

 Result:=(e-i);
end;

function iterate_wildcards(min:t_log_level;fname:Pchar):Boolean;
var
 rules:p_wildcards_blob;
 w:p_log_wildcard;
 max:DWORD;
begin
 rules:=wild_rules;

 max:=DetectExt(fname);

 Result:=(min>=t_log_level(rules^.level));

 w:=p_log_wildcard(@wild_rules^.dataw);
 while (Pointer(w)<rules^.lastw) do
 begin

  if IsWild(fname,@w^.wildw,max) then
  begin
   Result:=(min>=t_log_level(w^.level));
  end;

  w:=p_log_wildcard(@PChar(@w^.wildw)[strlen(PChar(@w^.wildw))+1]);
 end;

end;

function _log_filter_n(self:Pointer;fname:Pchar):Boolean; register; assembler; nostackframe;
asm
 xor %eax,%eax
end;

function _log_filter_y(self:Pointer;fname:Pchar):Boolean; register; assembler; nostackframe;
asm
 mov $1,%eax
end;

function _log_filter_default(min:t_log_level;self:_p_log_filter_node;fname:Pchar):Boolean; inline;
var
 epoch:Integer;
begin
 epoch:=load_consume(global_epoch);
 System.InterlockedIncrement(epoch_count[epoch]);

  Result:=iterate_wildcards(min,fname);

 System.InterlockedDecrement(epoch_count[epoch]);

 if Result then
 begin
  self^.fcbs:=@_log_filter_y;
 end else
 begin
  self^.fcbs:=@_log_filter_n;
 end;

 filtres[min].Push(self);
end;

function _log_filter_default_Trace(self:Pointer;fname:Pchar):Boolean; register; [public, alias:'_log_filter_default_Trace'];
begin
 Result:=_log_filter_default(Trace,self,fname);
end;

function _log_filter_default_Debug(self:Pointer;fname:Pchar):Boolean; register; [public, alias:'_log_filter_default_Debug'];
begin
 Result:=_log_filter_default(Debug,self,fname);
end;

function _log_filter_default_Info(self:Pointer;fname:Pchar):Boolean; register; [public, alias:'_log_filter_default_Info'];
begin
 Result:=_log_filter_default(Info,self,fname);
end;

function _log_filter_default_Warning(self:Pointer;fname:Pchar):Boolean; register; [public, alias:'_log_filter_default_Warning'];
begin
 Result:=_log_filter_default(Warning,self,fname);
end;

function _log_filter_default_Error(self:Pointer;fname:Pchar):Boolean; register; [public, alias:'_log_filter_default_Error'];
begin
 Result:=_log_filter_default(Error,self,fname);
end;

function _log_filter_default_Critical(self:Pointer;fname:Pchar):Boolean; register; [public, alias:'_log_filter_default_Critical'];
begin
 Result:=_log_filter_default(Critical,self,fname);
end;

const
 _log_filter_default_:array[t_log_level] of _t_log_filter_cb=(
  @_log_filter_default_Trace,
  @_log_filter_default_Debug,
  @_log_filter_default_Info,
  @_log_filter_default_Warning,
  @_log_filter_default_Error,
  @_log_filter_default_Critical,
  nil
 );

Procedure ResetAll;
var
 i:t_log_level;
begin
 for i:=Low(t_log_level) to High(t_log_level) do
 begin
  filtres[i].Reset(_log_filter_default_[i]);
 end;
end;

end.

