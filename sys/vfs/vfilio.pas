unit vfilio;

{$mode ObjFPC}{$H+}
{$CALLING SysV_ABI_CDecl}

interface

const
 FIOCLEX    =$20006601;
 FIONCLEX   =$20006602;
 FIONREAD   =$4004667F;
 FIONBIO    =$8004667E;
 FIOASYNC   =$8004667D;
 FIOSETOWN  =$8004667C;
 FIOGETOWN  =$4004667B;
 FIODTYPE   =$4004667A;
 FIOGETLBA  =$40046679;
 FIODGNAME  =$80106678;
 FIONWRITE  =$40046677;
 FIONSPACE  =$40046676;
 FIOSEEKDATA=$C0046661;
 FIOSEEKHOLE=$C0046662;

type
 p_fiodgname_arg=^t_fiodgname_arg;
 t_fiodgname_arg=packed record
  len:Integer;
  _align:Integer;
  buf:Pointer;
 end;

implementation

end.

