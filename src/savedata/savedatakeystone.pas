unit SaveDataKeystone;

{$mode ObjFPC}{$H+}

interface

uses
 sha256;

type
 p_keystone_file=^t_keystone_file;
 t_keystone_file=packed record
  kMagic        :array[0..7] of AnsiChar; //"keystone"
  kType         :Word; //2
  kVersion      :Word; //1
  Padding       :array[0..19] of Byte;
  PasscodeDigest:array[0..31] of Byte;
  KeystoneDigest:array[0..31] of Byte;
 end;

const
 fake_pkg_keystone:t_keystone_file=(
  kMagic  :'keystone';
  kType   :2;
  kVersion:1;
  Padding :(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  PasscodeDigest:(
    $29,$4a,$5e,$d0,$6d,$b1,$70,$61,$8f,$2e,$ed,$8c,$42,$4b,$9d,$82,
    $88,$79,$c0,$80,$cc,$66,$fb,$c4,$86,$4f,$69,$e9,$74,$de,$b8,$56
  );
  KeystoneDigest:(
    $fa,$0d,$0c,$2e,$bd,$6a,$00,$80,$63,$71,$3d,$e8,$81,$0d,$7e,$10,
    $b7,$32,$14,$3b,$91,$cd,$2e,$4f,$ea,$2d,$20,$53,$10,$6e,$b7,$5d
  )
 );

function sceSblSsCheckKeystone (keystone:p_keystone_file):Integer;
function sceSblSsVerifyKeystone(keystone:p_keystone_file;fingerprint:pchar):Integer;

implementation

const
 keystone_ks_secret:array[0..31] of Byte=(
  $78, $3D, $6F, $3A,
  $E9, $1C, $0E, $07,
  $12, $FC, $AA, $B7,
  $95, $0B, $DE, $06,
  $85, $5C, $F7, $A2,
  $2D, $CD, $BD, $E1,
  $27, $E9, $BF, $CB,
  $AD, $0F, $F0, $FE
 );

function sceSblSsCheckKeystone(keystone:p_keystone_file):Integer;
var
 digest:Tsha256Digest;
begin
 Result:=Integer($800f0434);

 if (PQWORD(@keystone^.kMagic)^<>$656E6F747379656B) then Exit;
 if (keystone^.kType   <>2) then Exit;
 if (keystone^.kVersion<>1) then Exit;

 Sha256Hmac(digest,keystone,64,@keystone_ks_secret,32);

 if (CompareByte(digest,keystone^.KeystoneDigest,32)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=Integer($800f0435);
 end;

end;

function to9(b:DWORD):DWORD; inline;
begin
 Result:=b + (b shl 3);
end;

function hex_to_val_i(i:DWORD):DWORD; inline;
begin
 Result:=(i and Byte($F)) + to9(i shr 6);
end;

function hex2_to_val(i:DWORD):DWORD; inline;
begin
 i:=RorDWord(i,8);

 Result:=hex_to_val_i(i) or (hex_to_val_i(i shr 24) shl 4);
end;

function sceSblSsVerifyKeystone(keystone:p_keystone_file;fingerprint:pchar):Integer;
var
 i:Integer;
 PasscodeDigest:array[0..31] of Byte;
begin
 Result:=Integer($800f0416);
 if (keystone=nil) or (fingerprint=nil) then Exit;

 Result:=sceSblSsCheckKeystone(keystone);
 if (Result<>0) then Exit;

 For i:=0 to 31 do
 begin
  PasscodeDigest[i]:=hex2_to_val(PWORD(fingerprint)[i]);
 end;

 if (CompareByte(PasscodeDigest,keystone^.PasscodeDigest,32)=0) then
 begin
  Result:=0;
 end else
 begin
  Result:=Integer($800f0435);
 end;

end;



end.

