program cpu_probe;
{$mode ObjFPC}{$H+}
{$ASMMODE INTEL}

{ fpPS4 CPU probe

  Pins the current thread to every logical CPU and prints:
    - dense Windows processor number (group:number)
    - CPUID.1.EBX[31:24]   initial APIC ID (what fpPS4 uses for cpuid_h2g now)
    - CPUID.1F/0B.EDX      x2APIC ID
    - IA32_TSC_AUX         via RDPID/RDTSCP (what GetCurrentProcessorNumber returns)

  Then emulates fpPS4's current cpuset_init/cpuid_h2g lookup for group 0 and
  shows where it would return a wrong CPU or loop forever.

  Build: fpc -MObjFPC -O1 cpu_probe.pas
}

uses
 Windows, sysutils;

type
 PROCESSOR_NUMBER=packed record
  Group:Word;
  Number:Byte;
  Reserved:Byte;
 end;

 GROUP_AFFINITY=packed record
  Mask:PtrUInt;
  Group:Word;
  Reserved:array[0..2] of Word;
 end;

 PGROUP_AFFINITY=^GROUP_AFFINITY;

 t_cpuid=packed record
  a,b,c,d:DWORD;
 end;

 t_cpu=record
  group :Word;
  num   :DWORD;   // dense number inside the group
  pn    :DWORD;   // GetCurrentProcessorNumber (valid for group 0)
  apic8 :DWORD;   // CPUID.1.EBX[31:24]
  x2apic:DWORD;   // CPUID.1F/0B.EDX or $FFFFFFFF
  aux   :DWORD;   // TSC_AUX via RDPID/RDTSCP or $FFFFFFFF
 end;

function GetActiveProcessorCount(g:WORD):DWORD; stdcall; external 'kernel32';
function GetActiveProcessorGroupCount:DWORD; stdcall; external 'kernel32';
function GetCurrentProcessorNumber:DWORD; stdcall; external 'kernel32';
function SwitchToThread:BOOL; stdcall; external 'kernel32';
procedure GetCurrentProcessorNumberEx(out pn:PROCESSOR_NUMBER); stdcall; external 'kernel32';
function SetThreadGroupAffinity(h:THandle;const ga:GROUP_AFFINITY;prev:PGROUP_AFFINITY):BOOL; stdcall; external 'kernel32';

var
 maxleaf   :DWORD;
 maxext    :DWORD;
 vendor    :string;
 brand     :string;
 has_x2apic:Boolean;
 has_rdpid :Boolean;
 has_rdtscp:Boolean;
 has_hybrid:Boolean;

function cpuid_leaf(leaf,sub:DWORD;out r:t_cpuid):Boolean; assembler; nostackframe;
asm
 push rbx
 mov eax,leaf
 mov ecx,sub
 cpuid
 mov [r],eax
 mov [r+4],ebx
 mov [r+8],ecx
 mov [r+12],edx
 pop rbx
 mov eax,1
 ret
end;

function rdpid_value:DWORD; assembler; nostackframe;
asm
 db $F3,$0F,$C7,$F9   // rdpid eax
 ret
end;

function rdtscp_value:DWORD; assembler; nostackframe;
asm
 db $0F,$01,$F9       // rdtscp
 mov eax,ecx
 ret
end;

function dword_str(d:DWORD):string;
var
 i:Integer;
begin
 Result:='';
 for i:=0 to 3 do
  Result:=Result+Chr((d shr (i*8)) and $FF);
end;

function bsf(d:DWORD):Integer;
begin
 if (d=0) then Exit(-1);
 Result:=0;
 while ((d and 1)=0) do
 begin
  d:=d shr 1;
  Inc(Result);
 end;
end;

procedure detect_features;
var
 r:t_cpuid;
begin
 cpuid_leaf(0,0,r);
 maxleaf :=r.a;
 vendor  :=dword_str(r.b)+dword_str(r.d)+dword_str(r.c);

 cpuid_leaf(1,0,r);
 has_x2apic:=(r.c and (1 shl 21))<>0;

 maxext:=0;
 has_rdtscp:=False;
 has_rdpid :=False;
 has_hybrid:=False;

 cpuid_leaf($80000000,0,r);
 maxext:=r.a;

 if (maxext>=$80000001) then
 begin
  cpuid_leaf($80000001,0,r);
  has_rdtscp:=(r.d and (1 shl 27))<>0;
 end;

 if (maxleaf>=7) then
 begin
  cpuid_leaf(7,0,r);
  has_rdpid :=(r.c and (1 shl 22))<>0;
  has_hybrid:=(r.d and (1 shl 15))<>0;
 end;

 brand:='';
 if (maxext>=$80000004) then
 begin
  cpuid_leaf($80000002,0,r); brand:=brand+dword_str(r.a)+dword_str(r.b)+dword_str(r.c)+dword_str(r.d);
  cpuid_leaf($80000003,0,r); brand:=brand+dword_str(r.a)+dword_str(r.b)+dword_str(r.c)+dword_str(r.d);
  cpuid_leaf($80000004,0,r); brand:=brand+dword_str(r.a)+dword_str(r.b)+dword_str(r.c)+dword_str(r.d);
 end;
 brand:=Trim(brand);
end;

function get_x2apic:DWORD;
var
 r:t_cpuid;
begin
 Result:=$FFFFFFFF;
 if (maxleaf>=$1F) then
 begin
  cpuid_leaf($1F,0,r);
  Result:=r.d;
 end else
 if (maxleaf>=$0B) then
 begin
  cpuid_leaf($0B,0,r);
  Result:=r.d;
 end;
end;

function pin_cpu(group:Word;num:DWORD):Boolean;
var
 ga,old:GROUP_AFFINITY;
 pn:PROCESSOR_NUMBER;
 tries:Integer;
begin
 Result:=False;

 FillChar(ga,SizeOf(ga),0);
 ga.Mask :=PtrUInt(1) shl num;
 ga.Group:=group;

 if (SetThreadGroupAffinity(GetCurrentThread,ga,@old)=False) then Exit;

 tries:=0;
 repeat
  if not SwitchToThread then Sleep(0);
  GetCurrentProcessorNumberEx(pn);
  Inc(tries);
 until ((pn.Group=group) and (pn.Number=num)) or (tries>200000);

 Result:=(pn.Group=group) and (pn.Number=num);

 SetThreadGroupAffinity(GetCurrentThread,old,nil);
end;

var
 cpus:array of t_cpu;
 cpus_n:Integer;
 groups:DWORD;

procedure probe_all;
var
 g,n,i:Integer;
 cnt:DWORD;
 c:t_cpu;
 r:t_cpuid;
begin
 groups:=GetActiveProcessorGroupCount and $FFFF;

 cnt:=0;
 for g:=0 to groups-1 do
  Inc(cnt,GetActiveProcessorCount(g));

 SetLength(cpus,cnt);
 cpus_n:=0;

 for g:=0 to groups-1 do
 begin
  n:=GetActiveProcessorCount(g);
  for i:=0 to n-1 do
  begin
   if not pin_cpu(g,i) then
   begin
    writeln('WARN: cannot pin to ',g,':',i);
    Continue;
   end;

   c.group:=g;
   c.num  :=i;
   c.pn   :=$FFFFFFFF;

   if (g=0) then c.pn:=GetCurrentProcessorNumber;

   if has_x2apic then c.x2apic:=get_x2apic else c.x2apic:=$FFFFFFFF;

   if has_rdpid then c.aux:=rdpid_value
   else
   if has_rdtscp then c.aux:=rdtscp_value
   else c.aux:=$FFFFFFFF;

   cpuid_leaf(1,0,r);
   c.apic8:=r.b shr 24;

   cpus[cpus_n]:=c;
   Inc(cpus_n);
  end;
 end;
end;

procedure print_report;
var
 i,j:Integer;
 c:t_cpu;

 cpu_num:DWORD;
 apic_of:array of DWORD;
 h2g:array[0..63] of Byte;
 proposed:array[0..255] of Byte;

 host,guest:DWORD;
 mask:Byte;
 gidx,rep:Integer;

 ok_cnt  :Integer;
 bad_cnt :Integer;
 oob_cnt :Integer;
 zero_cnt:Integer;
 dup_cnt :Integer;
 prop_bad:Integer;

 s,s2,s3:string;
begin
 writeln('fpPS4 CPU probe');
 writeln('vendor  : ',vendor);
 writeln('brand   : ',brand);
 writeln('maxleaf : $',IntToHex(maxleaf,8),'  maxext: $',IntToHex(maxext,8));
 writeln('x2apic  : ',has_x2apic,'  hybrid: ',has_hybrid);
 writeln('rdpid   : ',has_rdpid ,'  rdtscp: ',has_rdtscp);
 writeln('groups  : ',groups);
 writeln('group0  : ',GetActiveProcessorCount(0),' processors');
 writeln('cpus    : ',cpus_n);
 writeln;
 writeln('group:num  pn    apic8  x2apic  tscaux');
 writeln('--------------------------------------------------');

 for i:=0 to cpus_n-1 do
 begin
  c:=cpus[i];

  if (c.pn=$FFFFFFFF) then s:='  -'
  else s:=Format('%-3d',[c.pn]);

  s2:='-';
  if (c.x2apic<>$FFFFFFFF) then s2:=IntToStr(c.x2apic);

  s3:='-';
  if (c.aux<>$FFFFFFFF) then s3:=IntToStr(c.aux);

  writeln(Format('%d:%-3d     %s   %-3d     %-5s   %s',
                 [c.group,c.num,s,c.apic8,s2,s3]));
 end;

 //emulate fpPS4 cpuset_init (current, dense indexed)
 cpu_num:=GetActiveProcessorCount(0);
 if (cpu_num>64) then cpu_num:=64;

 FillChar(h2g,SizeOf(h2g),0);
 host:=0;
 for guest:=7 downto 0 do
 begin
  if (host<cpu_num) then
   h2g[host]:=h2g[host] or Byte(1 shl guest);
  host:=(host+1) mod cpu_num;
 end;

 SetLength(apic_of,cpu_num);
 for i:=0 to cpu_num-1 do apic_of[i]:=$FFFFFFFF;
 for i:=0 to cpus_n-1 do
  if (cpus[i].group=0) and (cpus[i].num<cpu_num) then
   apic_of[cpus[i].num]:=cpus[i].apic8;

 //emulate proposed cpuset_init (apic indexed)
 FillChar(proposed,SizeOf(proposed),0);
 host:=0;
 for guest:=7 downto 0 do
 begin
  if (host<cpu_num) and (apic_of[host]<>$FFFFFFFF) then
   proposed[apic_of[host]]:=proposed[apic_of[host]] or Byte(1 shl guest);
  host:=(host+1) mod cpu_num;
 end;

 ok_cnt  :=0;
 bad_cnt :=0;
 oob_cnt :=0;
 zero_cnt:=0;
 dup_cnt :=0;
 prop_bad:=0;

 writeln;
 writeln('--- current fpPS4 lookup emulation (group 0) ---');
 writeln('dense  apic8  h2g[apic8]  guest  status');

 for i:=0 to cpus_n-1 do
 begin
  c:=cpus[i];
  if (c.group<>0) or (c.num>=cpu_num) then Continue;

  s:='';

  if (c.apic8>=64) then
  begin
   Inc(oob_cnt);
   s:='OOB (table 0..63)';
  end else
  begin
   mask:=h2g[c.apic8];
   if (mask=0) then
   begin
    Inc(zero_cnt);
    s:='ZERO -> jz _cpuid_1 forever (hang)';
   end else
   begin
    gidx:=bsf(mask);
    rep:=7-gidx;
    if (c.apic8=c.num) then
    begin
     Inc(ok_cnt);
     s:='ok (reports cpu '+IntToStr(rep)+')';
    end else
    begin
     Inc(bad_cnt);
     s:='WRONG lookup: apic8<>dense (reports cpu '+IntToStr(rep)+')';
    end;
   end;
  end;

  writeln(Format('%-5d  %-5d  0x%s        %-5s  %s',
                 [c.num,c.apic8,IntToHex(mask,2),IntToStr(bsf(mask)),s]));
 end;

 //duplicates
 for i:=0 to cpus_n-1 do
 begin
  if (cpus[i].group<>0) or (cpus[i].num>=cpu_num) then Continue;
  for j:=i+1 to cpus_n-1 do
  begin
   if (cpus[j].group<>0) or (cpus[j].num>=cpu_num) then Continue;
   if (cpus[i].apic8=cpus[j].apic8) then
   begin
    Inc(dup_cnt);
    writeln('DUPLICATE apic8=',cpus[i].apic8,' for cpus ',
            i,':',cpus[i].num,' and ',j,':',cpus[j].num);
   end;
  end;
 end;

 //proposed
 for i:=0 to cpus_n-1 do
 begin
  c:=cpus[i];
  if (c.group<>0) or (c.num>=cpu_num) then Continue;

  if (c.apic8>255) then Begin Inc(prop_bad); Continue; end;

  mask:=proposed[c.apic8];
  if (mask=0) or (mask<>h2g[c.num]) then Inc(prop_bad);
 end;

 writeln;
 writeln('--- summary ---');
 writeln('dense==apic8 && correct : ',ok_cnt);
 writeln('wrong mapping           : ',bad_cnt);
 writeln('apic8 >= 64 (OOB)       : ',oob_cnt);
 writeln('zero mapping (hang)     : ',zero_cnt);
 writeln('duplicate apic8         : ',dup_cnt);
 writeln('proposed apic8 table bad: ',prop_bad);

 if (bad_cnt=0) and (oob_cnt=0) and (zero_cnt=0) then
  writeln('verdict: current fpPS4 cpuid_h2g indexing works on this machine')
 else
  writeln('verdict: current fpPS4 cpuid_h2g indexing is BROKEN on this machine');
end;

procedure restore_affinity;
var
 ga:GROUP_AFFINITY;
begin
 //put the thread on cpu 0 of group 0
 FillChar(ga,SizeOf(ga),0);
 ga.Mask:=1;
 ga.Group:=0;
 SetThreadGroupAffinity(GetCurrentThread,ga,nil);
end;

begin
 detect_features;
 probe_all;
 print_report;
 restore_affinity;
end.
