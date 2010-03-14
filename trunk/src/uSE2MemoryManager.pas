unit uSE2MemoryManager;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2BaseTypes;

type
  TSE2MemoryManager = class
  private
    {$IFDEF SEII_MM_USE_CACHE}
    FCache    : Pointer;
    FCacheEnd : integer;
    FEntryC   : integer;
    FEntries  : TSE2List;
    {$ENDIF}
  protected
    {$IFDEF SEII_MM_USE_CACHE}
    procedure CreateCache;
    procedure FreeCache;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function  GetMem(iSize: integer): Pointer; overload;
    procedure GetMem(var ptr: Pointer; iSize: integer); overload;
    procedure FreeMem(Data: Pointer); overload;
    procedure FreeMem(Data: Pointer; iSize: integer); overload;
  end;

implementation

{$IFDEF SEII_FPC}     
    {$HINTS OFF} 
  {$ENDIF}

{ TSE2CacheMemory }

{$IFDEF SEII_MM_CACHE_BIG}
const CacheSize = 1572864; // 1,5 MB
const MaxDataSize = 128; // 128 byte  -> 12288 elements
{$ELSE}
const CacheSize = 32768; // 32 KB
const MaxDataSize = 8; // 8 byte -> 4096  elements
{$ENDIF}

constructor TSE2MemoryManager.Create;
begin
  inherited;
  {$IFDEF SEII_MM_USE_CACHE}
  FEntries  := TSE2List.Create;
  CreateCache;
  {$ENDIF}
end;

destructor TSE2MemoryManager.Destroy;
begin
  {$IFDEF SEII_MM_USE_CACHE}
  FreeCache;
  FEntries.Free;
  {$ENDIF}
  inherited;
end;                

{$IFDEF SEII_MM_USE_CACHE}
procedure TSE2MemoryManager.CreateCache;
var i: cardinal;
begin
  FreeCache;

  System.GetMem(FCache, CacheSize);
  FillChar(FCache^, CacheSize, 0);

  FCacheEnd := cardinal(FCache) + CacheSize - 1;
  FEntries.Count := (CacheSize div MaxDataSize);
  for i:=0 to (CacheSize div MaxDataSize) - 1 do
    FEntries.Data[i] := (Pointer(  cardinal(FCache) + i*MaxDataSize  ));
  FEntryC := FEntries.Count - 1;
end;
{$ENDIF}           

{$IFDEF SEII_MM_USE_CACHE}
procedure TSE2MemoryManager.FreeCache;
begin
  if FCache <> nil then
     System.FreeMem(FCache);
  FCache := nil;
  FEntries.Clear;
  FCacheEnd := -1;
  FEntryC   := -1;
end;
{$ENDIF}

procedure TSE2MemoryManager.FreeMem(Data: Pointer);
begin
  {$IFDEF SEII_MM_USE_CACHE}
  if (cardinal(Data) >= cardinal(FCache)) and
     (cardinal(Data) <= cardinal(FCacheEnd)) then
  begin
    FEntryC := FEntryC + 1;
    FEntries.Data[FEntryC] := (Data);
  end else
  {$ENDIF}
    System.FreeMem(Data);
end;              

procedure TSE2MemoryManager.FreeMem(Data: Pointer; iSize: integer);
begin
  {$IFDEF SEII_MM_USE_CACHE}
  if (cardinal(Data) >= cardinal(FCache)) and
     (cardinal(Data) <= cardinal(FCacheEnd)) then
  begin
    FEntryC := FEntryC + 1;
    FEntries.Data[FEntryC] := (Data);
  end else
  {$ENDIF}
    System.FreeMem(Data, iSize);
end;

function TSE2MemoryManager.GetMem(iSize: integer): Pointer;
begin
  {$IFDEF SEII_MM_USE_CACHE}
  if (iSize <= MaxDataSize) and (FEntryC >= 0) then
  begin
    result := FEntries.Data[FEntryC];
    FEntryC := FEntryC - 1;
  end else
  {$ENDIF}
    System.GetMem(result, iSize);
end;

procedure TSE2MemoryManager.GetMem(var ptr: Pointer; iSize: integer);
begin
  {$IFDEF SEII_MM_USE_CACHE}
  if (iSize <= MaxDataSize) and (FEntryC >= 0) then
  begin
    ptr     := FEntries.Data[FEntryC];
    FEntryC := FEntryC - 1;
  end else
  {$ENDIF}
    System.GetMem(ptr, iSize);
end;

end.
