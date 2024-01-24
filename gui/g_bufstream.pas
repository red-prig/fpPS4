{
    This file is part of the Free Component Library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implement a buffered stream.
    TBufferedFileStream contributed by José Mejuto, bug ID 30549.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$H+}
{$IFNDEF FPC_DOTTEDUNITS}
unit g_bufstream;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

Const
  DefaultBufferCapacity : Integer = 16; // Default buffer capacity in Kb.

Type
{ ---------------------------------------------------------------------
  TBufferedFileStream -
  Multiple pages buffer for random access reading/writing in file.
  ---------------------------------------------------------------------}

  TBufferedFileStream = class(THandleStream)
  private
    const
      TSTREAMCACHEPAGE_SIZE_DEFAULT=4*1024;
      TSTREAMCACHEPAGE_MAXCOUNT_DEFAULT=8;
    type
      TStreamCacheEntry=record
        IsDirty: Boolean;
        LastTick: NativeUInt;
        PageBegin: int64;
        PageRealSize: integer;
        Buffer: Pointer;
      end;
      PStreamCacheEntry=^TStreamCacheEntry;
  private
    FCachePages: array of PStreamCacheEntry;
    FCacheLastUsedPage: integer;
    FCacheStreamPosition: int64;
    FCacheStreamSize: int64;
    FOpCounter: NativeUInt;
    FStreamCachePageSize: integer;
    FStreamCachePageMaxCount: integer;
    FEmergencyFlag: Boolean;
    procedure ClearCache;
    procedure WriteDirtyPage(const aPage: PStreamCacheEntry);
    procedure WriteDirtyPage(const aIndex: integer);
    procedure WriteDirtyPages;
    procedure EmergencyWriteDirtyPages;
    procedure FreePage(const aPage: PStreamCacheEntry; const aFreeBuffer: Boolean); inline;
    function  LookForPositionInPages: Boolean;
    function  ReadPageForPosition: Boolean;
    function  ReadPageBeforeWrite: Boolean;
    function  FreeOlderInUsePage(const aFreeBuffer: Boolean=false): PStreamCacheEntry;
    function  GetOpCounter: NativeUInt; inline;
    function  DoCacheRead(var Buffer; Count: Longint): Longint;
    function  DoCacheWrite(const Buffer; Count: Longint): Longint;
  protected
    function  GetPosition: Int64; override;
    procedure SetPosition(const Pos: Int64); override;
    function  GetSize: Int64; override;
    procedure SetSize64(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;overload;
    procedure SetSize(const NewSize: Int64); override;overload;
  public
    // Warning using Mode=fmOpenWrite because the write buffer
    // needs to read, as this class is a cache system not a dumb buffer.
    constructor Create(AHandle: THandle);
    destructor  Destroy; override;
    function  Seek(Offset: Longint; Origin: Word): Longint; override; overload;
    function  Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override; overload;
    function  Read(var Buffer; Count: Longint): Longint; override;
    function  Write(const Buffer; Count: Longint): Longint; override;
    // Flush write-cache content to disk
    procedure Flush;
    // re-initialize the cache with aCacheBlockCount block
    // of aCacheBlockSize bytes in each block.
    procedure InitializeCache(const aCacheBlockSize: integer; const aCacheBlockCount: integer);
  end;


implementation

Resourcestring
  SErrCacheUnexpectedPageDiscard ='CACHE: Unexpected behaviour. Discarded page.';
  SErrCacheUnableToReadExpected = 'CACHE: Unable to read expected bytes (Open for write only ?). Expected: %d, effective read: %d';
  SErrCacheUnableToWriteExpected ='CACHE: Unable to write expected bytes (Open for read only ?). Expected: %d, effective write: %d';
  SErrCacheInternal = 'CACHE: Internal error.';

{ ---------------------------------------------------------------------
  TBufferedFileStream
  ---------------------------------------------------------------------}

procedure TBufferedFileStream.ClearCache;
var
  j: integer;
  pStream: PStreamCacheEntry;
begin
  try
    WriteDirtyPages;
  finally
    for j := 0 to Pred(FStreamCachePageMaxCount) do begin
      pStream:=FCachePages[j];
      if Assigned(pStream) then begin
        if Assigned(pStream^.Buffer) then Freemem(pStream^.Buffer);
        Dispose(pStream);
        FCachePages[j]:=nil;
      end;
    end;
  end;
end;

procedure TBufferedFileStream.WriteDirtyPage(const aPage: PStreamCacheEntry);
var
  lEffectiveBytesWrite: integer;
begin
  inherited Seek(aPage^.PageBegin,soBeginning);
  lEffectiveBytesWrite:=inherited Write(aPage^.Buffer^,aPage^.PageRealSize);
  if lEffectiveBytesWrite<>aPage^.PageRealSize then begin
    EmergencyWriteDirtyPages;
    Raise EStreamError.CreateFmt(SErrCacheUnableToWriteExpected,[aPage^.PageRealSize,lEffectiveBytesWrite,IntToStr(aPage^.PageBegin)]);
  end;
  aPage^.IsDirty:=False;
  aPage^.LastTick:=GetOpCounter;
end;

procedure TBufferedFileStream.WriteDirtyPage(const aIndex: integer);
var
  pCache: PStreamCacheEntry;
begin
  pCache:=FCachePages[aIndex];
  if Assigned(pCache) then begin
    WriteDirtyPage(pCache);
  end;
end;

procedure TBufferedFileStream.WriteDirtyPages;
var
  j: integer;
  pCache: PStreamCacheEntry;
begin
  for j := 0 to Pred(FStreamCachePageMaxCount) do begin
    pCache:=FCachePages[j];
    if Assigned(pCache) then begin
      if pCache^.IsDirty then begin
        WriteDirtyPage(pCache);
      end;
    end;
  end;
end;

procedure TBufferedFileStream.EmergencyWriteDirtyPages;
var
  j: integer;
  pCache: PStreamCacheEntry;
begin
  // Are we already in a emergency write dirty pages ??
  if FEmergencyFlag then exit;
  FEmergencyFlag:=true;
  // This procedure tries to save all dirty pages inconditional
  // because a write fail happens, so everything in cache will
  // be dumped to stream if possible, trying to save as much
  // information as possible.
  for j := 0 to Pred(FStreamCachePageMaxCount) do begin
    pCache:=FCachePages[j];
    if Assigned(pCache) then begin
      if pCache^.IsDirty then begin
        try
          WriteDirtyPage(pCache);
        except on e: Exception do begin
          // Do nothing, eat exception if happen.
          // This way the cache still holds data to be
          // written (that fails) and can be written later
          // if write fail conditions change.
          end;
        end;
      end;
    end;
  end;
  FEmergencyFlag:=False;
end;

procedure TBufferedFileStream.FreePage(const aPage: PStreamCacheEntry;
  const aFreeBuffer: Boolean);
begin
  aPage^.PageBegin:=0;
  aPage^.PageRealSize:=0;
  aPage^.LastTick:=0;
  aPage^.IsDirty:=false;
  if aFreeBuffer then begin
    FreeMem(aPage^.Buffer);
    aPage^.Buffer:=nil;
  end;
end;

function TBufferedFileStream.LookForPositionInPages: Boolean;
var
  j: integer;
  pCache: PStreamCacheEntry;
begin
  Result:=false;
  for j := 0 to Pred(FStreamCachePageMaxCount) do begin
    pCache:=FCachePages[j];
    if Assigned(pCache^.Buffer) then begin
      if (FCacheStreamPosition>=pCache^.PageBegin) and (FCacheStreamPosition<pCache^.PageBegin+pCache^.PageRealSize) then begin
        FCacheLastUsedPage:=j;
        Result:=true;
        exit;
      end;
    end;
  end;
end;

function TBufferedFileStream.ReadPageForPosition: Boolean;
var
  j: integer;
  pCache: PStreamCacheEntry=nil;
  lStreamPosition: int64;
begin
  // Find free page entry
  for j := 0 to Pred(FStreamCachePageMaxCount) do begin
    if not Assigned(FCachePages[j]^.Buffer) then begin
      pCache:=FCachePages[j];
      FCacheLastUsedPage:=j;
      break;
    end;
  end;
  if not Assigned(pCache) then begin
    // Free last used page
    pCache:=FreeOlderInUsePage(false);
  end;
  if not Assigned(pCache^.Buffer) then begin
    Getmem(pCache^.Buffer,FStreamCachePageSize);
  end;
  lStreamPosition:=(FCacheStreamPosition div FStreamCachePageSize)*FStreamCachePageSize;
  inherited Seek(lStreamPosition,soBeginning);
  pCache^.PageBegin:=lStreamPosition;
  pCache^.PageRealSize:=inherited Read(pCache^.Buffer^,FStreamCachePageSize);
  if pCache^.PageRealSize=FStreamCachePageSize then begin
    pCache^.LastTick:=GetOpCounter;
    Result:=true;
  end else begin
    if FCacheStreamPosition<lStreamPosition+pCache^.PageRealSize then begin
      pCache^.LastTick:=GetOpCounter;
      Result:=true;
    end else begin
      Result:=false;
    end;
  end;
end;

function TBufferedFileStream.ReadPageBeforeWrite: Boolean;
var
  j: integer;
  pCache: PStreamCacheEntry=nil;
  lStreamPosition: int64;
  lExpectedBytesToRead: integer;
  lEffectiveRead: integer;
begin
  // Find free page entry
  for j := 0 to Pred(FStreamCachePageMaxCount) do begin
    if not Assigned(FCachePages[j]^.Buffer) then begin
      pCache:=FCachePages[j];
      FCacheLastUsedPage:=j;
      break;
    end;
  end;
  if not Assigned(pCache) then begin
    // Free last used page
    pCache:=FreeOlderInUsePage(false);
  end;
  if not Assigned(pCache^.Buffer) then begin
    Getmem(pCache^.Buffer,FStreamCachePageSize);
  end;
  lStreamPosition:=(FCacheStreamPosition div FStreamCachePageSize)*FStreamCachePageSize;
  inherited Seek(lStreamPosition,soBeginning);
  if (lStreamPosition+FStreamCachePageSize) > FCacheStreamSize then begin
    lExpectedBytesToRead:=FCacheStreamSize-lStreamPosition;
  end else begin
    lExpectedBytesToRead:=FStreamCachePageSize;
  end;
  pCache^.PageBegin:=lStreamPosition;
  pCache^.PageRealSize:=inherited Read(pCache^.Buffer^,FStreamCachePageSize);
  if pCache^.PageRealSize<>lExpectedBytesToRead then begin
    lEffectiveRead:=pCache^.PageRealSize;
    pCache^.IsDirty:=false;
    pCache^.LastTick:=0;
    pCache^.PageBegin:=0;
    pCache^.PageRealSize:=0;
    Freemem(pCache^.Buffer);
    pCache^.Buffer:=nil;
    Raise EStreamError.CreateFmt(SErrCacheUnableToReadExpected,[lExpectedBytesToRead,lEffectiveRead]);
  end;
  pCache^.LastTick:=GetOpCounter;
  Result:=true;
end;

function TBufferedFileStream.FreeOlderInUsePage(const aFreeBuffer: Boolean
  ): PStreamCacheEntry;
var
  j: integer;
  lOlderTick: int64=High(int64);
  lOlderEntry: integer=-1;
begin
  for j := 0 to Pred(FStreamCachePageMaxCount) do begin
    Result:=FCachePages[j];
    if Assigned(Result^.Buffer) then begin
      if Result^.LastTick<lOlderTick then begin
        lOlderTick:=Result^.LastTick;
        lOlderEntry:=j;
      end;
    end;
  end;
  if lOlderEntry=-1 then begin
    Raise Exception.Create(SErrCacheInternal);
  end;
  Result:=FCachePages[lOlderEntry];
  FCacheLastUsedPage:=lOlderEntry;
  if Result^.IsDirty then begin
    WriteDirtyPage(Result);
  end;
  FreePage(Result,aFreeBuffer);
end;

function TBufferedFileStream.GetOpCounter: NativeUInt;
begin
  Result:=FOpCounter;
  {$PUSH}
  {$Q-}
  inc(FOpCounter);
  {$POP}
end;

function TBufferedFileStream.DoCacheRead(var Buffer; Count: Longint): Longint;
var
  pCache: PStreamCacheEntry;
  lAvailableInThisPage: integer;
  lPositionInPage: integer;
  lNewBuffer: PBYTE;
begin
  pCache:=FCachePages[FCacheLastUsedPage];
  if Assigned(pCache) then begin
    // Check if FCacheStreamPosition is in range
    if Assigned(pCache^.Buffer) then begin
      if (FCacheStreamPosition>=pCache^.PageBegin) and (FCacheStreamPosition<pCache^.PageBegin+pCache^.PageRealSize) then begin
        // Position is in range, so read available data from this page up to count or page end
        lPositionInPage:=(FCacheStreamPosition-pCache^.PageBegin);
        lAvailableInThisPage:=pCache^.PageRealSize - lPositionInPage;
        if lAvailableInThisPage>=Count then begin
          move((PBYTE(pCache^.Buffer)+lPositionInPage)^,Buffer,Count);
          inc(FCacheStreamPosition,Count);
          Result:=Count;
          pCache^.LastTick:=GetOpCounter;
          exit;
        end else begin
          move((PBYTE(pCache^.Buffer)+lPositionInPage)^,Buffer,lAvailableInThisPage);
          inc(FCacheStreamPosition,lAvailableInThisPage);
          if pCache^.PageRealSize=FStreamCachePageSize then begin
            lNewBuffer:=PBYTE(@Buffer)+lAvailableInThisPage;
            Result:=lAvailableInThisPage+DoCacheRead(lNewBuffer^,Count-lAvailableInThisPage);
          end else begin
            // This cache page is not filled, so it is the last one
            // in the file, nothing more to read...
            pCache^.LastTick:=GetOpCounter;
            Result:=lAvailableInThisPage;
          end;
          exit;
        end;
      end else begin
        // The position is in other cache page or not in cache at all, so look for
        // position in cached pages or allocate a new page.
        if LookForPositionInPages then begin
          Result:=DoCacheRead(Buffer,Count);
          exit;
        end else begin
          if ReadPageForPosition then begin
            Result:=DoCacheRead(Buffer,Count);
          end else begin
            Result:=0;
          end;
          exit;
        end;
      end;
    end else begin
      if ReadPageForPosition then begin
        Result:=DoCacheRead(Buffer,Count);
      end else begin
        Result:=0;
      end;
      exit;
    end;
  end else begin
    // The page has been discarded for some unknown reason
    Raise EStreamError.Create(SErrCacheUnexpectedPageDiscard);
  end;
end;

function TBufferedFileStream.DoCacheWrite(const Buffer; Count: Longint): Longint;
var
  pCache: PStreamCacheEntry;
  lAvailableInThisPage: integer;
  lPositionInPage: integer;
  lNewBuffer: PBYTE;
begin
  pCache:=FCachePages[FCacheLastUsedPage];
  if Assigned(pCache) then begin
    // Check if FCacheStreamPosition is in range
    if Assigned(pCache^.Buffer) then begin
      if (FCacheStreamPosition>=pCache^.PageBegin) and (FCacheStreamPosition<pCache^.PageBegin+FStreamCachePageSize) then begin
        // Position is in range, so write data up to end of page
        lPositionInPage:=(FCacheStreamPosition-pCache^.PageBegin);
        lAvailableInThisPage:=FStreamCachePageSize - lPositionInPage;
        if lAvailableInThisPage>=Count then begin
          move(Buffer,(PBYTE(pCache^.Buffer)+lPositionInPage)^,Count);
          if not pCache^.IsDirty then pCache^.IsDirty:=true;
          inc(FCacheStreamPosition,Count);
          // Update page size
          if lPositionInPage+Count > pCache^.PageRealSize then pCache^.PageRealSize:=lPositionInPage+Count;
          // Update file size
          if FCacheStreamPosition>FCacheStreamSize then FCacheStreamSize:=FCacheStreamPosition;
          Result:=Count;
          pCache^.LastTick:=GetOpCounter;
          exit;
        end else begin
          move(Buffer,(PBYTE(pCache^.Buffer)+lPositionInPage)^,lAvailableInThisPage);
          if not pCache^.IsDirty then pCache^.IsDirty:=true;
          inc(FCacheStreamPosition,lAvailableInThisPage);
          // Update page size
          if lPositionInPage+Count > pCache^.PageRealSize then pCache^.PageRealSize:=lPositionInPage+lAvailableInThisPage;
          // Update file size
          if FCacheStreamPosition>FCacheStreamSize then FCacheStreamSize:=FCacheStreamPosition;

          Assert(pCache^.PageRealSize=FStreamCachePageSize,'This must not happend');
          lNewBuffer:=PBYTE(@Buffer)+lAvailableInThisPage;
          Result:=lAvailableInThisPage+DoCacheWrite(lNewBuffer^,Count-lAvailableInThisPage);
          exit;
        end;
      end else begin
        // The position is in other cache page or not in cache at all, so look for
        // position in cached pages or allocate a new page.
        if LookForPositionInPages then begin
          Result:=DoCacheWrite(Buffer,Count);
          exit;
        end else begin
          if ReadPageBeforeWrite then begin
            Result:=DoCacheWrite(Buffer,Count);
          end else begin
            Result:=0;
          end;
          exit;
        end;
      end;
    end else begin
      if ReadPageBeforeWrite then begin
        Result:=DoCacheWrite(Buffer,Count);
      end else begin
        Result:=0;
      end;
      exit;
    end;
  end else begin
    // The page has been discarded for some unknown reason
    Raise EStreamError.Create(SErrCacheUnexpectedPageDiscard);
  end;
end;

function TBufferedFileStream.GetPosition: Int64;
begin
  Result:=FCacheStreamPosition;
end;

procedure TBufferedFileStream.SetPosition(const Pos: Int64);
begin
  if Pos<0 then begin
    FCacheStreamPosition:=0;
  end else begin
    FCacheStreamPosition:=Pos;
  end;
end;

function TBufferedFileStream.GetSize: Int64;
begin
  Result:=FCacheStreamSize;
end;

procedure TBufferedFileStream.SetSize64(const NewSize: Int64);
var
  j: integer;
  pCache: PStreamCacheEntry;
begin
  WriteDirtyPages;
  inherited SetSize64(NewSize);
  FCacheStreamSize:=inherited Seek(0,soFromEnd);
  for j := 0 to Pred(FStreamCachePageMaxCount) do begin
    pCache:=FCachePages[j];
    if Assigned(pCache^.Buffer) and (pCache^.PageRealSize+pCache^.PageBegin>FCacheStreamSize) then begin
      // This page is out of bounds the new file size
      // so discard it.
      FreePage(pCache,True);
      break;
    end;
  end;
end;

procedure TBufferedFileStream.SetSize(NewSize: Longint);
begin
  SetSize64(NewSize);
end;

procedure TBufferedFileStream.SetSize(const NewSize: Int64);
begin
  SetSize64(NewSize);
end;

constructor TBufferedFileStream.Create(AHandle: THandle);
begin
  // Initialize with 8 blocks of 4096 bytes
  InitializeCache(TSTREAMCACHEPAGE_SIZE_DEFAULT,TSTREAMCACHEPAGE_MAXCOUNT_DEFAULT);
  inherited Create(AHandle);
  FCacheStreamSize:=inherited Seek(int64(0),soEnd);
end;

function TBufferedFileStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:=DoCacheRead(Buffer,Count);
end;

function TBufferedFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:=DoCacheWrite(Buffer,Count);
end;

procedure TBufferedFileStream.Flush;
begin
  WriteDirtyPages;
end;

function TBufferedFileStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result:=Seek(int64(OffSet),TSeekOrigin(Origin));
end;

function TBufferedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  lNewOffset: int64;
begin
  Case Origin of
    soEnd:
      begin
        lNewOffset:=FCacheStreamSize+Offset;
      end;
    soBeginning:
      begin
        lNewOffset:=0+Offset;
      end;
    soCurrent:
      begin
        lNewOffset:=FCacheStreamPosition+Offset;
      end;
  end;
  if lNewOffset>=0 then begin
    FCacheStreamPosition:=lNewOffset;
    Result:=lNewOffset;
  end else begin
    // This is compatible with FPC stream
    // as it returns the negative value :-?
    // but in fact does not move the read pointer.
    Result:=-1;
  end;
end;

procedure TBufferedFileStream.InitializeCache(const aCacheBlockSize: integer;
  const aCacheBlockCount: integer);
var
  j: integer;
begin
  ClearCache;
  FStreamCachePageSize:=aCacheBlockSize;
  FStreamCachePageMaxCount:=aCacheBlockCount;
  FCacheStreamSize:=inherited Seek(0,soEnd);
  SetLength(FCachePages,FStreamCachePageMaxCount);
  for j := 0 to Pred(FStreamCachePageMaxCount) do begin
    FCachePages[j]:=New(PStreamCacheEntry);
    FillByte(FCachePages[j]^,Sizeof(PStreamCacheEntry^),0);
  end;
end;

destructor TBufferedFileStream.Destroy;
begin
  ClearCache;
  inherited Destroy;
end;

end.
