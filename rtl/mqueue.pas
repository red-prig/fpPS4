unit mqueue;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 P_TAILQ_HEAD=^TAILQ_HEAD;
 TAILQ_HEAD=packed record
  tqh_first:Pointer;
  tqh_last :PPointer;
 end;

 P_TAILQ_ENTRY=^TAILQ_ENTRY;
 TAILQ_ENTRY=packed record
  tqe_next:Pointer;
  tqe_prev:PPointer;
 end;

 P_LIST_ENTRY=^LIST_ENTRY;
 LIST_ENTRY=packed record
  le_next:Pointer;
  le_prev:PPointer;
 end;

 P_STAILQ_HEAD=^STAILQ_HEAD;
 STAILQ_HEAD=packed record
  stqh_first:Pointer;
  stqh_last :PPointer;
 end;

 P_STAILQ_ENTRY=^STAILQ_ENTRY;
 STAILQ_ENTRY=packed record
  stqe_next:Pointer;
 end;


 P_SLIST_ENTRY=^SLIST_ENTRY;
 SLIST_ENTRY=packed record
  sle_next:Pointer;
 end;

procedure TAILQ_INIT         (head:Pointer); inline;
function  TAILQ_EMPTY        (head:Pointer):Boolean; inline;
function  TAILQ_FIRST        (head:Pointer):Pointer; inline;
function  TAILQ_LAST         (head,headname:Pointer):Pointer; inline;
function  TAILQ_NEXT         (elm,field:Pointer):Pointer; inline;
function  TAILQ_PREV         (elm,headname,field:Pointer):Pointer; inline;
procedure TAILQ_INSERT_HEAD  (head,elm,field:Pointer); inline;
procedure TAILQ_INSERT_TAIL  (head,elm,field:Pointer); inline;
procedure TAILQ_INSERT_AFTER (head,listelm,elm,field:Pointer); inline;
procedure TAILQ_INSERT_BEFORE(listelm,elm,field:Pointer); inline;
procedure TAILQ_REMOVE       (head,elm,field:Pointer); inline;

procedure LIST_INIT        (head:Pointer); inline;
function  LIST_EMPTY       (head:Pointer):Boolean; inline;
function  LIST_FIRST       (head:Pointer):Pointer; inline;
function  LIST_NEXT        (elm,field:Pointer):Pointer; inline;
procedure LIST_INSERT_HEAD (head,elm,field:Pointer); inline;
procedure LIST_REMOVE      (elm,field:Pointer); inline;

procedure STAILQ_INIT(head:Pointer); inline;
function  STAILQ_EMPTY(head:Pointer):Boolean; inline;
function  STAILQ_FIRST(head:Pointer):Pointer; inline;
function  STAILQ_NEXT(elm,field:Pointer):Pointer; inline;
procedure STAILQ_INSERT_AFTER(head,tqelm,elm,field:Pointer); inline;
procedure STAILQ_INSERT_HEAD(head,elm,field:Pointer); inline;
procedure STAILQ_INSERT_TAIL(head,elm,field:Pointer); inline;
procedure STAILQ_REMOVE_AFTER(head,elm,field:Pointer); inline;
procedure STAILQ_REMOVE_HEAD(head,elm,field:Pointer); inline;
procedure STAILQ_REMOVE(head,elm,field:Pointer); inline;

implementation

procedure TAILQ_INIT(head:Pointer); inline;
begin
 P_TAILQ_HEAD(head)^.tqh_first:=nil;
 P_TAILQ_HEAD(head)^.tqh_last :=@P_TAILQ_HEAD(head)^.tqh_first;
end;

function TAILQ_EMPTY(head:Pointer):Boolean; inline;
begin
 Result:=P_TAILQ_HEAD(head)^.tqh_first=nil;
end;

function TAILQ_FIRST(head:Pointer):Pointer; inline;
begin
 Result:=P_TAILQ_HEAD(head)^.tqh_first;
end;

function TAILQ_LAST(head,headname:Pointer):Pointer; inline;
begin
 Result:=P_TAILQ_HEAD(P_TAILQ_HEAD(head)^.tqh_last+ptruint(headname))^.tqh_last^;
end;

function TAILQ_NEXT(elm,field:Pointer):Pointer; inline;
begin
 Result:=P_TAILQ_ENTRY(field)^.tqe_next;
end;

function TAILQ_PREV(elm,headname,field:Pointer):Pointer; inline;
begin
 Result:=P_TAILQ_HEAD(P_TAILQ_ENTRY(field)^.tqe_prev+ptruint(headname))^.tqh_last^;
end;

procedure TAILQ_INSERT_HEAD(head,elm,field:Pointer); inline;
var
 offset:ptruint;
begin
 if (P_TAILQ_ENTRY(field)^.tqe_next=P_TAILQ_HEAD(head)^.tqh_first) and
    (P_TAILQ_HEAD(head)^.tqh_first<>nil) then
 begin
  offset:=ptruint(field-elm);
  P_TAILQ_ENTRY(P_TAILQ_HEAD(head)^.tqh_first+offset)^.tqe_prev:=@P_TAILQ_ENTRY(field)^.tqe_next;
 end else
 begin
  P_TAILQ_HEAD(head)^.tqh_last:=@P_TAILQ_ENTRY(field)^.tqe_next;
 end;
 P_TAILQ_HEAD(head)^.tqh_first:=elm;
 P_TAILQ_ENTRY(field)^.tqe_prev:=@P_TAILQ_HEAD(head)^.tqh_first;
end;

procedure TAILQ_INSERT_TAIL(head,elm,field:Pointer); inline;
begin
 P_TAILQ_ENTRY(field)^.tqe_next:=nil;
 P_TAILQ_ENTRY(field)^.tqe_prev:=P_TAILQ_HEAD(head)^.tqh_last;
 P_TAILQ_HEAD(head)^.tqh_last^:=elm;
 P_TAILQ_HEAD(head)^.tqh_last:=@P_TAILQ_ENTRY(field)^.tqe_next;
end;

procedure TAILQ_INSERT_AFTER(head,listelm,elm,field:Pointer); inline;
var
 offset:ptruint;
begin
 offset:=ptruint(field-elm);
 P_TAILQ_ENTRY(field)^.tqe_next:=P_TAILQ_ENTRY(listelm+offset)^.tqe_next;
 if (P_TAILQ_ENTRY(field)^.tqe_next<>nil) then
 begin
  P_TAILQ_ENTRY(P_TAILQ_ENTRY(field)^.tqe_next+offset)^.tqe_prev:=@P_TAILQ_ENTRY(field)^.tqe_next;
 end else
 begin
  P_TAILQ_HEAD(head)^.tqh_last:=@P_TAILQ_ENTRY(field)^.tqe_next;
 end;
 P_TAILQ_ENTRY(listelm+offset)^.tqe_next:=(elm);
 P_TAILQ_ENTRY(field)^.tqe_prev:=@P_TAILQ_ENTRY(listelm+offset)^.tqe_next;
end;

procedure TAILQ_INSERT_BEFORE(listelm,elm,field:Pointer); inline;
var
 offset:ptruint;
begin
 offset:=ptruint(field-elm);
 P_TAILQ_ENTRY(field)^.tqe_prev:=P_TAILQ_ENTRY(listelm+offset)^.tqe_prev;
 P_TAILQ_ENTRY(field)^.tqe_next:=listelm;
 P_TAILQ_ENTRY(listelm+offset)^.tqe_prev^:=elm;
 P_TAILQ_ENTRY(listelm+offset)^.tqe_prev:=@P_TAILQ_ENTRY(field)^.tqe_next;
end;

procedure TAILQ_REMOVE(head,elm,field:Pointer); inline;
var
 offset:ptruint;
begin
 if (P_TAILQ_ENTRY(field)^.tqe_next<>nil) then
 begin
  offset:=ptruint(field-elm);
  P_TAILQ_ENTRY(P_TAILQ_ENTRY(field)^.tqe_next+offset)^.tqe_prev:=P_TAILQ_ENTRY(field)^.tqe_prev;
 end else
 begin
  P_TAILQ_HEAD(head)^.tqh_last:=P_TAILQ_ENTRY(field)^.tqe_prev;
 end;
 P_TAILQ_ENTRY(field)^.tqe_prev^:=P_TAILQ_ENTRY(field)^.tqe_next;
end;

//

procedure LIST_INIT(head:Pointer); inline;
begin
 PPointer(head)^:=nil;
end;

function LIST_EMPTY(head:Pointer):Boolean; inline;
begin
 Result:=PPointer(head)^=nil;
end;

function LIST_FIRST(head:Pointer):Pointer; inline;
begin
 Result:=PPointer(head)^;
end;

function LIST_NEXT(elm,field:Pointer):Pointer; inline;
begin
 Result:=P_LIST_ENTRY(field)^.le_next;
end;

procedure LIST_INSERT_HEAD(head,elm,field:Pointer); inline;
var
 offset:ptruint;
begin
 if (P_LIST_ENTRY(field)^.le_next=PPointer(head)^) and (PPointer(head)^<>nil) then
 begin
  offset:=ptruint(field-elm);
  P_LIST_ENTRY(PPointer(head)^+offset)^.le_prev:=@P_LIST_ENTRY(field)^.le_next;
 end;
 PPointer(head)^:=elm;
 P_LIST_ENTRY(field)^.le_prev:=head;
end;

procedure LIST_REMOVE(elm,field:Pointer); inline;
var
 offset:ptruint;
begin
 if (P_LIST_ENTRY(field)^.le_next<>nil) then
 begin
  offset:=ptruint(field-elm);
  P_LIST_ENTRY(P_LIST_ENTRY(field)^.le_next+offset)^.le_prev:=P_LIST_ENTRY(field)^.le_prev;
 end;
 P_LIST_ENTRY(field)^.le_prev:=@P_LIST_ENTRY(field)^.le_next;
end;

//

procedure STAILQ_INIT(head:Pointer); inline;
begin
 P_STAILQ_HEAD(head)^.stqh_first:=nil;
 P_STAILQ_HEAD(head)^.stqh_last :=@P_STAILQ_HEAD(head)^.stqh_first;
end;

function STAILQ_EMPTY(head:Pointer):Boolean; inline;
begin
 Result:=P_STAILQ_HEAD(head)^.stqh_first=nil;
end;

function STAILQ_FIRST(head:Pointer):Pointer; inline;
begin
 Result:=P_STAILQ_HEAD(head)^.stqh_first;
end;

function STAILQ_NEXT(elm,field:Pointer):Pointer; inline;
begin
 Result:=P_STAILQ_ENTRY(field)^.stqe_next;
end;

procedure STAILQ_INSERT_AFTER(head,tqelm,elm,field:Pointer); inline;
var
 offset:ptruint;
begin
 offset:=ptruint(field-elm);
 P_STAILQ_ENTRY(field)^.stqe_next:=P_STAILQ_ENTRY(tqelm+offset)^.stqe_next;
 if (P_STAILQ_ENTRY(field)^.stqe_next=nil) then
 begin
  P_STAILQ_HEAD(head)^.stqh_last:=@P_STAILQ_ENTRY(field)^.stqe_next;
 end;
 P_STAILQ_ENTRY(tqelm+offset)^.stqe_next:=elm;
end;

procedure STAILQ_INSERT_HEAD(head,elm,field:Pointer); inline;
begin
 P_STAILQ_ENTRY(field)^.stqe_next:=P_STAILQ_HEAD(head)^.stqh_first;
 if (P_STAILQ_HEAD(head)^.stqh_first=nil) then
 begin
  P_STAILQ_HEAD(head)^.stqh_last:=@P_STAILQ_ENTRY(field)^.stqe_next;
 end;
 P_STAILQ_HEAD(head)^.stqh_first:=elm;
end;

procedure STAILQ_INSERT_TAIL(head,elm,field:Pointer); inline;
begin
 P_STAILQ_ENTRY(field)^.stqe_next:=nil;
 P_STAILQ_HEAD(head)^.stqh_last^:=elm;
 P_STAILQ_HEAD(head)^.stqh_last:=@P_STAILQ_ENTRY(field)^.stqe_next;
end;

procedure STAILQ_REMOVE_AFTER(head,elm,field:Pointer); inline;
var
 offset:ptruint;
begin
 offset:=ptruint(field-elm);
 P_STAILQ_ENTRY(field)^.stqe_next:=P_STAILQ_ENTRY(P_STAILQ_ENTRY(field)^.stqe_next+offset)^.stqe_next;
 if (P_STAILQ_ENTRY(field)^.stqe_next=nil) then
 begin
  P_STAILQ_HEAD(head)^.stqh_last:=@P_STAILQ_ENTRY(field)^.stqe_next;
 end;
end;

procedure STAILQ_REMOVE_HEAD(head,elm,field:Pointer); inline;
var
 offset:ptruint;
begin
 offset:=ptruint(field-elm);
 P_STAILQ_HEAD(head)^.stqh_first:=P_STAILQ_ENTRY(P_STAILQ_HEAD(head)^.stqh_first+offset)^.stqe_next;
 if (P_STAILQ_HEAD(head)^.stqh_first=nil) then
 begin
  P_STAILQ_HEAD(head)^.stqh_last:=@P_STAILQ_HEAD(head)^.stqh_first;
 end;
end;

procedure STAILQ_REMOVE(head,elm,field:Pointer); inline;
var
 offset:ptruint;
 curelm:Pointer;
begin
 if (P_STAILQ_HEAD(head)^.stqh_first=elm) then
 begin
  STAILQ_REMOVE_HEAD(head,elm,field);
 end else
 begin
  offset:=ptruint(field-elm);
  curelm:=P_STAILQ_HEAD(head)^.stqh_first;
  while (P_STAILQ_ENTRY(curelm+offset)^.stqe_next<>elm) do
  begin
   curelm:=P_STAILQ_ENTRY(curelm+offset)^.stqe_next;
  end;
  STAILQ_REMOVE_AFTER(head,curelm,curelm+offset);
 end;
end;


end.

