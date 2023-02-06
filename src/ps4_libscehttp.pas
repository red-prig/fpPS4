unit ps4_libSceHttp;

{$mode objfpc}{$H+}

interface

uses
  ps4_program,
  Classes,
  SysUtils,
  Generics.Collections;

implementation

uses
  sys_kernel,
  sys_types;

const
 SCE_HTTP_NB_EVENT_SOCK_ERR = 8;

 SCE_HTTP_ERROR_OUT_OF_MEMORY=$80431022;
 SCE_HTTP_ERROR_INVALID_VALUE=$804311fe;
 SCE_HTTP_ERROR_INVALID_URL  =$80433060;

type
 SceHttpUriElement=packed record
  opaque  :LongBool;
  _align  :DWord;
  scheme  :PChar;
  username:PChar;
  password:PChar;
  hostname:PChar;
  path    :PChar;
  query   :PChar;
  fragment:PChar;
  port    :Word;
  reserved:array[0..9] of Byte;
 end;
 PSceHttpUriElement=^SceHttpUriElement;

function ps4_sceHttpInit(libnetMemId,libsslCtxId:Integer;poolSize:size_t):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceHttpInit poolSize=',poolSize);
 Result:=4; // libhttpCtxId
end;

function ps4_sceHttpTerm(libhttpCtxId:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceHttpTerm:',libhttpCtxId);
 Result:=0;
end;

function ps4_sceHttpCreateTemplate(
	  libhttpCtxId:Integer;
	  userAgent:PChar;
	  httpVer:Integer;
	  autoProxyConf:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceHttpCreateTemplate userAgent=',userAgent,' httpVer=',httpVer);
 Result:=1; // templateId
end;

function ps4_sceHttpDeleteTemplate(templateId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpsLoadCert(
	  libhttpCtxId:Integer;
	  caCertNum:Integer;
          caList :Pointer; //SceSslData **caList
          cert   :Pointer; //SceSslData *cert
          privKey:Pointer  //SceSslData *privKey
	  ):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetRedirectCallback(id:Integer;
                                        cbfunc:Pointer; //SceHttpRedirectCallback
                                        userArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetAuthInfoCallback(id:Integer;
                                        cbfunc:Pointer; //SceHttpAuthInfoCallback
                                        userArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetNonblock(id:Integer;enable:Boolean):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetAutoRedirect(id:Integer;enable:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpsEnableOption(id:Integer;sslFlags:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpsDisableOption(id:Integer;sslFlags:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpsSetSslVersion(id:Integer;version:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetCookieEnabled(id:Integer;enable:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetInflateGZIPEnabled(id:Integer;isEnable:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetAuthEnabled(id:Integer;enable:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetRecvTimeOut(id:Integer;usec:DWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

type
 PSceHttpEpollHandle=^SceHttpEpollHandle;
 SceHttpEpollHandle=Pointer;

function ps4_sceHttpCreateEpoll(libhttpCtxId:Integer;eh:PSceHttpEpollHandle):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 eh^:=Pointer($BADF);
end;

function ps4_sceHttpDestroyEpoll(libhttpCtxId:Integer;eh:SceHttpEpollHandle):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSetEpoll(id:Integer;eh:SceHttpEpollHandle;userArg:Pointer):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix, 'sceHttpSetEpoll');
 Result:=0;
end;

function ps4_sceHttpUnsetEpoll(id:Integer):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix, 'sceHttpUnsetEpoll');
 Result:=0;
end;

type
 PSceHttpNBEvent=^SceHttpNBEvent;
 SceHttpNBEvent=packed record
  events:DWORD;
  eventDetail:DWORD;
  id:Integer;
  userArg:Pointer;
 end;

function ps4_sceHttpWaitRequest(eh:SceHttpEpollHandle;
                                nbev:PSceHttpNBEvent;
                                maxevents:Integer;
                                timeout_us:Integer):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpWaitRequest handle=', HexStr(eh),' event.id=', nbev^.id,' maxevents=',maxevents);

 nbev^.events:=SCE_HTTP_NB_EVENT_SOCK_ERR;
 nbev^.id:=3;

 Result:=1;
end;

function ps4_sceHttpAddRequestHeader(id:Integer;name:PChar;value:PChar;mode:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
 WriteLn(SysLogPrefix, 'sceHttpAddRequestHeader ',name,'=',value,' mode=',mode);
end;

type
 SceHttpsCallback=function(
                   libsslCtxId:Integer;
                   verifyErr:Integer;
                   sslCert:Pointer; //SceSslCert
                   certNum:Integer;
                   userArg:Pointer):Integer; SysV_ABI_CDecl;

function ps4_sceHttpsSetSslCallback(id:Integer;cbfunc:SceHttpsCallback;userArg:Pointer):Integer; SysV_ABI_CDecl;
begin      
 WriteLn(SysLogPrefix, 'sceHttpsSetSslCallback id=',id,' callback=',HexStr(@cbfunc));
 Result:=0;
end;

function ps4_sceHttpCreateConnectionWithURL(tmplId:Integer;
                                            url:PChar;
                                            enableKeepalive:Boolean):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpCreateConnectionWithURL:' + url);
 Result:=2;
end;

function ps4_sceHttpDeleteConnection(connId:Integer):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpCreateRequestWithURL(connId:Integer;
                                         method:Integer;
                                         url:PChar;
                                         contentLength:QWORD):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpCreateRequestWithURL: method=', method, 'url=',url);
 Result:=3;
end;

function ps4_sceHttpCreateRequestWithURL2(connId:Integer;
                                          method:PChar;
                                          url:PChar;
                                          contentLength:QWORD):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpCreateRequestWithURL2: method=', method, 'url=',url);
 Result:=3;
end;

function ps4_sceHttpDeleteRequest(reqId:Integer):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpDeleteRequest');
 Result:=0;
end;

function ps4_sceHttpSetRequestContentLength(reqId:Integer;contentLength:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpSendRequest(reqId:Integer;
                                postData:Pointer;
                                size:QWORD):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpSendRequest reqId=',reqId,' postData=',HexStr(postData));
 Result:=0;
end;

function ps4_sceHttpGetStatusCode(reqId:Integer;
                                  statusCode:PInteger):Integer; SysV_ABI_CDecl;
begin
 if (statusCode<>nil) then
 begin
  statusCode^:=404;
 end;
 Result:=0;
 Result:=Integer($80431082);
end;

const
 nullp:Pchar='';

function ps4_sceHttpGetAllResponseHeaders(reqId:Integer;
                                          header:PPchar;
                                          headerSize:PQWORD):Integer; SysV_ABI_CDecl;
begin
 header^:=@nullp;
 headerSize^:=0;
 Result:=0;
end;

function ps4_sceHttpGetResponseContentLength(reqId:Integer;
                                             presult:PInteger;
                                             contentLength:PQWORD):Integer; SysV_ABI_CDecl;
begin
 presult^:=0;
 contentLength^:=0;
 Result:=0;
end;

function ps4_sceHttpReadData(reqId:Integer;
                             data:Pointer;
                             size:QWORD):Integer; SysV_ABI_CDecl;
begin
 Result:=0;
end;

function ps4_sceHttpUriParse(output:PSceHttpUriElement;
                             uri:PChar;
                             pool:Pointer;
                             require:psize_t;
                             prepare:size_t):Integer; SysV_ABI_CDecl;
const
 PARSE_TYPE_SCHEME  =0;
 PARSE_TYPE_HOSTNAME=1;
 PARSE_TYPE_PATH    =2;
 PARSE_TYPE_QUERY   =3;
 PARSE_TYPE_PORT    =4;
type
 TTokenKind=(tkString,
             tkNumber,
             tkSlash,
             tkColon,
             tkDoubleSlashes,
             tkQuestion,
             tkUnknown,
             tkEOL);
type
 TToken=record
  kind :TTokenKind;
  value:RawByteString;
  pos  :Integer;
 end;
 TTokenList=specialize TList<TToken>;
var
 tokenKindNames:array[tkString..tkEOL] of RawByteString=('tkString',
                                                         'tkNumber',
                                                         'tkSlash',
                                                         'tkColon',
                                                         'tkDoubleSlashes',
                                                         'tkQuestion',
                                                         'tkUnknown',
                                                         'tkEOL');
 scheme        :RawByteString;
 hostname      :RawByteString;
 path          :RawByteString;
 query         :RawByteString;
 port          :Word=0;
 pos           :Integer;
 parseType     :Integer=PARSE_TYPE_SCHEME;
 tokenList     :TTokenList;

 function _nextChar:Char;
 begin
  Inc(pos);
  Result:=uri[pos];
 end;

 function _peekAtNextChar:Char;
 begin
  if uri[pos]<>#0 then
   Result:=uri[pos+1]
  else
   Result:=#0;
 end;

 function _nextToken:TToken;
 begin
  if pos+1<tokenList.Count then
  begin
   Inc(pos);
   Result:=tokenList[pos];
  end else
   Result.kind:=tkEOL;
 end;

 function _peekAtNextToken:TToken;
 begin
  if pos+1<tokenList.Count then
   Result:=tokenList[pos+1]
  else
   Result.kind:=tkEOL;
 end;

 function _tokenKindNames(const kinds:array of TTokenKind):RawByteString;
 var
  kind:TTokenKind;
 begin
  for kind in kinds do
   Result:=Result+tokenKindNames[kind] + ' ';
 end;

 procedure _tokenExpected(const token: TToken;const expected:array of TTokenKind);
 var
  kind:TTokenKind;
 begin
  for kind in expected do
   if kind=token.kind then
    exit;
  raise Exception.Create(IntToStr(token.pos) + ': Expected '+_tokenKindNames(expected)+' but found '+tokenKindNames[token.kind]);
 end;

 function _nextTokenExpected(const expected:array of TTokenKind):TToken;
 begin
  Result:=_nextToken;
  _tokenExpected(Result, expected);
 end;

 function _peekAtNextTokenExpected(const expected:array of TTokenKind):TToken;
 begin
  Result:=_peekAtNextToken;
  _tokenExpected(Result, expected);
 end;

 procedure _lex;
 var
  c    :Char;
  token:TToken;
 begin
  Result:=0;
  pos   :=-1;
  while True do
  begin
   c:=_nextChar;
   case c of
    '\':
     begin
      token.kind :=tkSlash;
      token.pos  :=pos;
      token.value:=c;
     end;
    '/':
     begin
      if _peekAtNextChar='/' then
      begin
       token.kind :=tkDoubleSlashes;
       token.pos  :=pos;
       token.value:='//';
       _nextChar;
      end else
      begin
       token.kind :=tkSlash;
       token.value:=c;
      end;
     end;
    ':':
     begin
      token.kind :=tkColon;
      token.value:=c;
     end;
    '?':
     begin
      token.kind :=tkQuestion;
      token.value:=c;
     end;
    '0'..'9':
     begin
      token.kind :=tkNumber;
      token.value:='';
      token.pos  :=pos;
      Dec(pos);
      repeat
       token.value:=token.value + _nextChar;
      until not (_peekAtNextChar in ['0'..'9']);
     end;
    #0:
     break;
    else
     begin
      token.kind :=tkString;
      token.value:='';
      token.pos  :=pos;
      Dec(pos);
      repeat
       token.value:=token.value + _nextChar;
      until _peekAtNextChar in [#0,':','/','\','?'];
     end;
   end;
   tokenList.Add(token);
  end;
 end;

 procedure _parse;
 var
  token:TToken;
 begin
  pos:=-1;
  while True do
  begin
   token:=_nextToken;
   if token.kind=tkEOL then
    break;
   case parseType of
    PARSE_TYPE_SCHEME:
     begin
      _tokenExpected(token,[tkString]);
      scheme:=token.value;
      if (output<>nil) and (not output^.opaque) then
       scheme:=scheme+'//'
      else
      if output=nil then
       scheme:=scheme+'//';
      _nextTokenExpected([tkColon]);
      while _peekAtNextToken.kind in [tkSlash,tkDoubleSlashes] do
       _nextToken;
      parseType:=PARSE_TYPE_HOSTNAME;
     end;
    PARSE_TYPE_HOSTNAME:
     begin
      _tokenExpected(token,[tkString]);
      hostname:=token.value;
      if _peekAtNextToken.kind=tkColon then
      begin
       _nextToken;
       parseType:=PARSE_TYPE_PORT;
      end else
       parseType:=PARSE_TYPE_PATH;
     end;
    PARSE_TYPE_PORT:
     begin
      _tokenExpected(token,[tkNumber]);
      port:=StrToInt(token.value);
      parseType:=PARSE_TYPE_PATH;
     end;
    PARSE_TYPE_PATH:
     begin
      path:=path+token.value;
      if _peekAtNextToken.kind=tkQuestion then
       parseType:=PARSE_TYPE_QUERY;
     end;
    PARSE_TYPE_QUERY:
     begin
      query:=query+token.value;
     end;
   end;
  end;
 end;

 function _assemble:Integer;
 var
  sizeNeeded:Integer;
  p         :PChar;

  procedure _writeStringToPool(const pStartPos:PPChar;const src:RawByteString);
  var
   c:Char;
  begin
   if src<>'' then
   begin
    pStartPos^:=p;
    for c in src do
    begin
     p^:=c;
     Inc(p);
    end;
    p^:=#0;
    Inc(p);
   end;
  end;

 begin
  Writeln('scheme  : ',scheme);
  Writeln('hostname: ',hostname);
  Writeln('path    : ',path);
  Writeln('query   : ',query);
  Writeln('port    : ',port);
  // Calculate size needed
  sizeNeeded:=Length(scheme)+Length(hostname)+Length(path)+Length(query)+4;
  Writeln('require : ',sizeNeeded);
  if require<>nil then
   require^:=sizeNeeded;
  if sizeNeeded>prepare then
   Exit(SCE_HTTP_ERROR_OUT_OF_MEMORY);
  if output=nil then
   Exit(0);
  p:=pool;
  // Write strings to pool
  _writeStringToPool(@output^.scheme  ,scheme);
  _writeStringToPool(@output^.hostname,hostname);
  _writeStringToPool(@output^.path    ,path);
  _writeStringToPool(@output^.query   ,query);
  output^.port:=port;
  Result:=0;
 end;

begin
 Writeln(SysLogPrefix,'sceHttpUriParse,uri=',uri,',prepare=',prepare);
 if ((output=nil) or (pool=nil)) and (require=nil) then
  Exit(SCE_HTTP_ERROR_INVALID_VALUE);
 tokenList:=TTokenList.Create;
 _lex;
 _parse;
 Result:=_assemble;
 tokenList.Free;
end;

function ps4_sceHttpCreateConnection(tmplId:Integer;
                                     server:PChar;
                                     scheme:PChar;
                                     port:Word;
                                     keepAlive:LongBool):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceHttpCreateConnection,server=',server,',scheme=',scheme,',port=',port,',keepAlive=',keepAlive);
 Result:=2;
end;

function ps4_sceHttpCreateRequest(connId:Integer;
                                  method:Integer;
                                  url:PChar;
                                  contentLength:QWORD):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpCreateRequest,method=', method,',url=',url);
 Result:=3;
end;

function ps4_sceHttpCreateRequest2(connId:Integer;
                                   method:PChar;
                                   path:PChar;
                                   contentLength:QWORD):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttpCreateRequest2,method=', method,',path=',path);
 Result:=3;
end;

function ps4_sceHttpSetConnectTimeOut(id:Integer;usec:DWord):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceHttpSetConnectTimeOut,id=',id,',usec=',usec);
 Result:=0;
end;

function ps4_sceHttpSetSendTimeOut(id:Integer;usec:DWord):Integer; SysV_ABI_CDecl;
begin
 Writeln(SysLogPrefix,'sceHttpSetSendTimeOut,id=',id,',usec=',usec);
 Result:=0;
end;

function ps4_sceHttpSetChunkedTransferEnabled(id,isEnabled:Integer):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix,'sceHttpSetChunkedTransferEnabled,id=', id,',isEnabled=',isEnabled);
 Result:=0;
end;

function Load_libSceHttp(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceHttp');
 lib^.set_proc($03D715314B44A786,@ps4_sceHttpInit);
 lib^.set_proc($224FCAA4B4E57FB4,@ps4_sceHttpTerm);
 lib^.set_proc($D206233D347FE9C6,@ps4_sceHttpCreateTemplate);
 lib^.set_proc($E08F2F129B84859F,@ps4_sceHttpDeleteTemplate);
 lib^.set_proc($0CAF86A1708D4F4E,@ps4_sceHttpsLoadCert);
 lib^.set_proc($87DC261595F88BFE,@ps4_sceHttpSetRedirectCallback);
 lib^.set_proc($8DFE130769D43B8D,@ps4_sceHttpSetAuthInfoCallback);
 lib^.set_proc($B36FCD3C8BF3FA20,@ps4_sceHttpSetNonblock);
 lib^.set_proc($4FF986A3D7F73EEE,@ps4_sceHttpSetAutoRedirect);
 lib^.set_proc($7F8D8ADFB9A6E513,@ps4_sceHttpsEnableOption);
 lib^.set_proc($992402C73593C152,@ps4_sceHttpsDisableOption);
 lib^.set_proc($539131406CB2C7DB,@ps4_sceHttpsSetSslVersion);
 lib^.set_proc($5CD5280F607D6BA0,@ps4_sceHttpSetCookieEnabled);
 lib^.set_proc($8BD9A169FCE4122F,@ps4_sceHttpSetInflateGZIPEnabled);
 lib^.set_proc($A858364AEC932496,@ps4_sceHttpSetAuthEnabled);
 lib^.set_proc($CA282BE15D3F1D33,@ps4_sceHttpSetRecvTimeOut);
 lib^.set_proc($EB7F3575617EC6C4,@ps4_sceHttpCreateEpoll);
 lib^.set_proc($C1885755F4B612DE,@ps4_sceHttpDestroyEpoll);
 lib^.set_proc($FF19BB91940DA472,@ps4_sceHttpSetEpoll);
 lib^.set_proc($E7DB4BD404016FC5,@ps4_sceHttpUnsetEpoll);
 lib^.set_proc($A884A30C7AF138D7,@ps4_sceHttpWaitRequest);
 lib^.set_proc($118DBC4F66E437B9,@ps4_sceHttpAddRequestHeader);
 lib^.set_proc($86DC813A859E4B9F,@ps4_sceHttpsSetSslCallback);
 lib^.set_proc($AA0C43063A2B531B,@ps4_sceHttpCreateConnectionWithURL);
 lib^.set_proc($3FA037CADA6C8987,@ps4_sceHttpDeleteConnection);
 lib^.set_proc($01EBB9C152A417DC,@ps4_sceHttpCreateRequestWithURL);
 lib^.set_proc($0A7A7BEE9A1D9025,@ps4_sceHttpCreateRequestWithURL2);
 lib^.set_proc($A9EEE867EBF83D60,@ps4_sceHttpDeleteRequest);
 lib^.set_proc($3D3885214C42A497,@ps4_sceHttpSetRequestContentLength);
 lib^.set_proc($D5ED8137023F5F31,@ps4_sceHttpSendRequest);
 lib^.set_proc($D1AD9304D7C4DC15,@ps4_sceHttpGetStatusCode);
 lib^.set_proc($68260F31250868FF,@ps4_sceHttpGetAllResponseHeaders);
 lib^.set_proc($CAE3B61F652F9E8B,@ps4_sceHttpGetResponseContentLength);
 lib^.set_proc($3F9A5DA3290F6139,@ps4_sceHttpReadData);
 lib^.set_proc($2166A5027FE0B85B,@ps4_sceHttpUriParse);
 lib^.set_proc($2A2C2FF6BE086427,@ps4_sceHttpCreateConnection);
 lib^.set_proc($B6C195AEEDE109EF,@ps4_sceHttpCreateRequest);
 lib^.set_proc($AC6366F858C85CA9,@ps4_sceHttpCreateRequest2);
 lib^.set_proc($D12F6D4C7D2EA935,@ps4_sceHttpSetConnectTimeOut);
 lib^.set_proc($C5E8057D9281565C,@ps4_sceHttpSetSendTimeOut);
 lib^.set_proc($3C3C52E3CC4640BB,@ps4_sceHttpSetChunkedTransferEnabled);
end;

function ps4_sceHttp2Init(libnetMemId,libsslCtxId:Integer;
                          poolSize:size_t;
                          maxConcurrentRequest:Integer):Integer; SysV_ABI_CDecl;
begin
 WriteLn(SysLogPrefix, 'sceHttp2Init poolSize=',poolSize);
 Result:=3;
end;

function Load_libSceHttp2(Const name:RawByteString):TElf_node;
var
 lib:PLIBRARY;
begin
 Result:=TElf_node.Create;
 Result.pFileName:=name;
 lib:=Result._add_lib('libSceHttp2');
 lib^.set_proc($DC909EDE509B43C0,@ps4_sceHttp2Init);
end;

initialization
 ps4_app.RegistredPreLoad('libSceHttp.prx' ,@Load_libSceHttp);
 ps4_app.RegistredPreLoad('libSceHttp2.prx',@Load_libSceHttp2);

end.
