unit uarch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
 Parch_sig=^Tarch_sig;
 Tarch_sig=array[0..7] of AnsiChar;

 Parch_header=^Tarch_header;
 Tarch_header=packed record
  FileName :array[0..15] of AnsiChar;
  timestamp:array[0..11] of AnsiChar;
  OwnerID  :array[0..5]  of AnsiChar;
  GroupID  :array[0..5]  of AnsiChar;
  FileMode :array[0..7]  of AnsiChar;
  FileSize :array[0..9]  of AnsiChar;
  Ending   :array[0..1]  of AnsiChar;
 end;

const
 arch_sig:Tarch_sig='!<arch>'#$0A;

implementation

end.

