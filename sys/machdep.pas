unit machdep;

{$mode ObjFPC}{$H+}

interface

uses
 ntapi,
 signal,
 signalvar;

procedure sendsig(catcher:sig_t;ksi:p_ksiginfo;mask:p_sigset_t);

implementation

procedure sendsig(catcher:sig_t;ksi:p_ksiginfo;mask:p_sigset_t);
begin
 //TODO
end;



end.

