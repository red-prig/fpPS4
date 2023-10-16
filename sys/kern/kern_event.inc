

procedure knote(list:p_knlist;hint:QWORD;lockflags:Integer); external;

procedure KNOTE_LOCKED(list:p_knlist;hint:QWORD); inline;
begin
 knote(list, hint, KNF_LISTLOCKED);
end;

procedure KNOTE_UNLOCKED(list:p_knlist;hint:QWORD); inline;
begin
 knote(list, hint, 0);
end;

procedure knlist_add   (knl:p_knlist;kn:p_knote;islocked:Integer); external;
procedure knlist_remove(knl:p_knlist;kn:p_knote;islocked:Integer); external;

procedure knlist_destroy (knl:p_knlist); external;
procedure knlist_cleardel(knl:p_knlist;islocked,killkn:Integer); external;

procedure knlist_clear(knl:p_knlist;islocked:Integer); inline;
begin
 knlist_cleardel(knl, islocked, 0)
end;

procedure knlist_init(knl:p_knlist;lock,kl_lock,kl_unlock,kl_assert_locked,kl_assert_unlocked:Pointer); external;
procedure knlist_init_mtx(knl:p_knlist;lock:p_mtx); external;

function M_KNLIST_EMPTY(list:p_knlist):Boolean; inline;
begin
 Result:=SLIST_EMPTY(@list^.kl_list);
end;


