unit sockopt;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

type
 t_sopt_dir=(SOPT_GET,SOPT_SET);

 t_sockopt=record
  sopt_dir    :t_sopt_dir;
  sopt_level  :Integer;
  sopt_name   :Integer;
  sopt_val    :Pointer;
  sopt_valsize:QWORD;
  sopt_td     :Pointer; //p_kthread;
 end;

implementation

end.

