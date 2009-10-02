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

    function  GetMem(iSize: integer): Pointer;
    procedure FreeMem(Data: Pointer);
  end;

implementation

{$IFDEF FPC}     
    {$HINTS OFF} 
  {$ENDIF}

{ TSE2CacheMemory }

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
const CacheSize = 32768;
var i: cardinal;
begin
  FreeCache;

  System.GetMem(FCache, CacheSize);
  FillChar(FCache^, CacheSize, 0);

  FCacheEnd := cardinal(FCache) + CacheSize - 1;
  FEntries.Count := (CacheSize div 8);
  for i:=0 to (CacheSize div 8) - 1 do
    FEntries[i] := (Pointer(  cardinal(FCache) + i*8  ));
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
    FEntries[FEntryC] := (Data);
  end else
  {$ENDIF}
    System.FreeMem(Data);
end;

function TSE2MemoryManager.GetMem(iSize: integer): Pointer;
begin
  {$IFDEF SEII_MM_USE_CACHE}
  if (iSize <= 8) and (FEntryC >= 0) then
  begin
    result := FEntries[FEntryC];
    FEntryC := FEntryC - 1;
  end else
  {$ENDIF}
    System.GetMem(result, iSize);
end;

end.
