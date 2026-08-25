unit sys_tty;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface


uses
 sysutils,
 vselinfo,
 kern_mtx,
 subr_msgbuf,
 placeholder_fmt;

type
 p_tty_target=^t_tty_target;
 t_tty_target=record
  next  :Pointer;
  refs  :DWORD;
  target:PChar;
  priv  :Pointer;
 end;

 p_tty=^t_tty;
 t_tty=record
  t_name   :PChar;
  t_nlen   :DWORD;

  t_mtx    :p_mtx;      // TTY lock.
  t_mtxobj :mtx;        // Per-TTY lock (when not borrowing).

  // Polling mechanisms.
  t_inpoll :t_selinfo;  // (t) Input  poll queue.
  t_outpoll:t_selinfo;  // (t) Output poll queue.

  t_target:p_tty_target;

  t_priv:Pointer;

  t_update    :TProcedure;
 end;

procedure tty_lock  (tp:p_tty);
procedure tty_unlock(tp:p_tty);

procedure tty_init(tp:p_tty;name:PChar;mutex:p_mtx);
procedure tty_fini(tp:p_tty);

var
 std_tty  :array[0..2 ] of t_tty;
 deci_tty :array[0..11] of t_tty;
 debug_tty:t_tty;

type
 t_tty_init_param=record
  tp  :p_tty;
  name:Pchar;
 end;

const
 tty_init_array:array[0..15] of t_tty_init_param=(
  (tp: @std_tty[ 0];name:'Input' ),
  (tp: @std_tty[ 1];name:'Output'),
  (tp: @std_tty[ 2];name:'Error' ),

  (tp:@deci_tty[ 0];name:'stdin' ),
  (tp:@deci_tty[ 1];name:'stdout'),
  (tp:@deci_tty[ 2];name:'stderr'),
  (tp:@deci_tty[ 3];name:'tty2'  ),
  (tp:@deci_tty[ 4];name:'tty3'  ),
  (tp:@deci_tty[ 5];name:'tty4'  ),
  (tp:@deci_tty[ 6];name:'tty5'  ),
  (tp:@deci_tty[ 7];name:'tty6'  ),
  (tp:@deci_tty[ 8];name:'tty7'  ),
  (tp:@deci_tty[ 9];name:'ttya0' ),
  (tp:@deci_tty[10];name:'ttyb0' ),
  (tp:@deci_tty[11];name:'ttyc0' ),

  (tp:@debug_tty   ;name:'Debug' )
 );

var
 tty_prefix:t_fmt_builder;

const
 tty_prefix_values:array[0..3] of t_placeholder_value=(
  (id:0;maxsize: 9;name:'tty_name';fmt:'%0:s'),
  (id:1;maxsize:31;name:'td_name' ;fmt:'%1:s'),
  (id:2;maxsize: 7;name:'td_tid'  ;fmt:'%2:d'),
  (id:3;maxsize:10;name:'fib_addr';fmt:'%3:10.10x')
 );

procedure sys_tty_init(const Prefix,Redirect:RawByteString);

implementation

uses
 logging,
 vsys_generic,
 sys_event;

procedure tty_lock(tp:p_tty);
begin
 mtx_lock(tp^.t_mtx^)
end;

procedure tty_unlock(tp:p_tty);
begin
 mtx_unlock(tp^.t_mtx^)
end;

procedure tty_init(tp:p_tty;name:PChar;mutex:p_mtx);
begin
 if (tp=nil) then Exit;

 tp^.t_name:=name;

 if (name<>nil) then
 begin
  tp^.t_nlen:=strlen(name);
 end;

 if (mutex<>nil) then
 begin
  tp^.t_mtx:=mutex;
 end else
 begin
  tp^.t_mtx:=@tp^.t_mtxobj;
  mtx_init(tp^.t_mtxobj, 'ttymtx');
 end;

 knlist_init_mtx(@tp^.t_inpoll .si_note, tp^.t_mtx);
 knlist_init_mtx(@tp^.t_outpoll.si_note, tp^.t_mtx);

end;

procedure tty_fini(tp:p_tty);
begin

 seldrain(@tp^.t_inpoll);
 seldrain(@tp^.t_outpoll);

 knlist_destroy(@tp^.t_inpoll .si_note);
 knlist_destroy(@tp^.t_outpoll.si_note);

 if (tp^.t_mtx=@tp^.t_mtxobj) then
 begin
  mtx_destroy(tp^.t_mtxobj);
 end;

end;

type
 t_tty_redirect_builder=object
  list:p_tty_target;
  //
  procedure Free;
  function  FetchTarget(const target:RawByteString):p_tty_target;
  procedure AddMask    (const mask,target:RawByteString);
  procedure Parse      (const redirect:RawByteString);
 end;

procedure t_tty_redirect_builder.Free;
var
 node,next,prev:p_tty_target;
begin
 node:=list;
 prev:=nil;
 while (node<>nil) do
 begin
  next:=node^.next;
  //
  if (node^.refs=0) then
  begin
   //unlink
   if (prev<>nil) then
   begin
    prev^.next:=next;
   end;
   //free
   Finalize(node^);
   FreeMem(node);
  end;
  //
  prev:=node;
  node:=next;
 end;
 //
 list:=nil;
end;

function t_tty_redirect_builder.FetchTarget(const target:RawByteString):p_tty_target;
var
 node:p_tty_target;
begin
 node:=list;
 while (node<>nil) do
 begin
  if SameFileName(node^.target,target) then
  begin
   Exit(node);
  end;
  //
  node:=node^.next;
 end;

 node:=AllocMem(SizeOf(t_tty_target));
 node^.target:=StrNew(PChar(target));

 node^.next:=list;
 list:=node;

 Exit(node);
end;

procedure _set_tty_target(var prev:p_tty_target;new:p_tty_target);inline;
begin
 if (prev<>nil) then
 begin
  Dec(prev^.refs);
 end;
 prev:=new;
 Inc(new^.refs);
end;

procedure t_tty_redirect_builder.AddMask(const mask,target:RawByteString);
var
 i:Integer;
 new:p_tty_target;
begin
 new:=nil;

 For i:=0 to High(tty_init_array) do
 begin
  if IsWild(tty_init_array[i].name,PChar(mask),8) then
  begin
   if (new=nil) then
   begin
    new:=FetchTarget(target);
   end;
   //
   _set_tty_target(tty_init_array[i].tp^.t_target,new);
  end;
 end;

end;

type
 t_params=record
  curr:RawByteString;
 end;

 t_params_concat=packed object
  params:array[0..1] of t_params;
  procedure Init; inline;
  procedure Add(b:boolean;c:Char); inline;
 end;

procedure t_params_concat.Init; inline;
begin
 params[0].curr:='';
 params[1].curr:='';
end;

procedure t_params_concat.Add(b:boolean;c:Char); inline;
begin
 with params[ord(b)] do
 begin
  curr:=curr+c;
 end;
end;

procedure t_tty_redirect_builder.Parse(const redirect:RawByteString);
var
 p:pchar;
 c,e,d:char;

 _params:t_params_concat;

 procedure Add;
 begin
  //normalize
  _params.params[0].curr:=Trim(_params.params[0].curr);
  _params.params[1].curr:=Trim(_params.params[1].curr);

  case LowerCase(_params.params[1].curr) of
   'nul':_params.params[1].curr:='null';
   'in' :_params.params[1].curr:='stdin';
   'out':_params.params[1].curr:='stdout';
   'err':_params.params[1].curr:='stderr';
  end;

  AddMask(_params.params[0].curr,_params.params[1].curr);
 end;

begin
 _params.Init;

 p:=@redirect[1];
 if (p=nil) then p:='';

 e:=#0;
 d:=#0;

 while (p^<=' ') do Inc(p);

 while True do
 begin
  c:=p^;

  if (c=#0) then Break;

  if (c=e) then
  begin
   e:=#0;
  end else
  if (c<=' ') then
  begin

   if (e=#0) then
   begin
    Add;

    _params.Init;
    d:=#0;

    inc(p);
    while (p^<=' ') do Inc(p);
    Continue;
   end;

   _params.Add(d<>#0,c);
  end else
  case c of
   '"',
   '''':e:=c;
   ':' :d:=c;
   else
    _params.Add(d<>#0,c);
  end;

  inc(p);
 end;

 Add;
end;

procedure sys_tty_init(const Prefix,Redirect:RawByteString);
var
 i:Integer;
 builder:t_tty_redirect_builder;
begin
 tty_prefix.build(Prefix,@tty_prefix_values,Length(tty_prefix_values));
 //
 For i:=0 to High(tty_init_array) do
 begin
  tty_init(tty_init_array[i].tp,tty_init_array[i].name,nil);
 end;
 //
 builder:=Default(t_tty_redirect_builder);
 //
 builder.Parse('*:stdout'); //default
 //
 builder.Parse(Redirect);
 //
 builder.Free;
end;


end.



