unit gtailq;

{$mode ObjFPC}{$H+}

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

procedure TAILQ_INIT       (head:Pointer); inline;
function  TAILQ_EMPTY      (head:Pointer):Boolean; inline;
function  TAILQ_FIRST      (head:Pointer):Pointer; inline;
function  TAILQ_NEXT       (elm,field:Pointer):Pointer; inline;
procedure TAILQ_INSERT_HEAD(head,elm,field:Pointer); inline;
procedure TAILQ_INSERT_TAIL(head,elm,field:Pointer); inline;
procedure TAILQ_REMOVE     (head,elm,field:Pointer); inline;

procedure LIST_INIT        (head:Pointer); inline;
function  LIST_EMPTY       (head:Pointer):Boolean; inline;
function  LIST_FIRST       (head:Pointer):Pointer; inline;
function  LIST_NEXT        (elm,field:Pointer):Pointer; inline;
procedure LIST_INSERT_HEAD (head,elm,field:Pointer); inline;
procedure LIST_REMOVE      (elm,field:Pointer); inline;

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

function TAILQ_NEXT(elm,field:Pointer):Pointer; inline;
begin
 Result:=P_TAILQ_ENTRY(field)^.tqe_next;
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


end.

