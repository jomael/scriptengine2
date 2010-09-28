unit uSE2UnitCacheMngr;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2BaseTypes, uSE2Types, uSE2UnitCache, uSE2NameWeaver;

type
  TSE2GetCacheStream = procedure(Sender: TObject; const aUnitName: string; var Cache: TStream) of object;
  TSE2GetChangedUnits = procedure(Sender: TObject; const Target: TStrings) of object;

  TSE2UnitCacheMngr = class(TSE2Object)
  private
    FList : TList;
    FOnGetCacheStream  : TSE2GetCacheStream;
    FOnGetChangedUnits : TSE2GetChangedUnits;
  protected
    procedure PublishEntry(Entry: TSE2BaseType; Weaver: TSE2NameWeaver);
    procedure PublishUnit(AUnit: TSE2Unit; Weaver: TSE2NameWeaver);
    procedure PublishUnits(UnitList: TSE2BaseTypeList; Weaver: TSE2NameWeaver);
    function  IndexOf(const Name: string): integer;

    function  DoGetCacheStream(const aUnitName: string): TStream;
    function  InternGetUnit(Cache: TSE2UnitCache; UnitList: TSE2BaseTypeList; Filter: TStrings): TSE2Unit;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure CheckForChanges;

    procedure Clear;
    procedure Add(aUnit: TSE2Unit);
    function  GetCache(const Name: string): TSE2UnitCache;
    procedure ClearCache(const Name: string);
    procedure ClearCacheForRecompile(const Name: string);
    function  HasUnit(const Name: string): boolean;
    function  GetUnit(const Name: string; UnitList: TSE2BaseTypeList): TSE2Unit; overload;
    function  GetUnit(Cache: TSE2UnitCache; UnitList: TSE2BaseTypeList): TSE2Unit; overload;

    property  OnGetChangedUnits : TSE2GetChangedUnits read FOnGetChangedUnits write FOnGetChangedUnits;
    property  OnGetCacheStream  : TSE2GetCacheStream read FOnGetCacheStream write FOnGetCacheStream;
  end;

implementation

{ TSE2UnitCacheMngr }

procedure TSE2UnitCacheMngr.Add(aUnit: TSE2Unit);
var Cache : TSE2UnitCache;
begin
  if aUnit = nil then
     exit;

  if IndexOf(aUnit.AUnitName) > -1 then
     ClearCache(aUnit.Name);

  Cache := TSE2UnitCache.Create( DoGetCacheStream(aUnit.Name) );
  Cache.StoreUnit(aUnit);
  Cache.AUnitName := aUnit.Name;
  FList.Add(Cache);
end;

function TSE2UnitCacheMngr.DoGetCacheStream(
  const aUnitName: string): TStream;
begin
  result := nil;
  if Assigned(FOnGetCacheStream) then
     FOnGetCacheStream(Self, aUnitName, result);

  if result = nil then
     result := TMemoryStream.Create;
end;

procedure TSE2UnitCacheMngr.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    TSE2UnitCache(FList[i]).Free;
  FList.Clear;
end;

procedure TSE2UnitCacheMngr.ClearCache(const Name: string);
var i: integer;
begin
  i := IndexOf(Name);
  if i = -1 then
     exit;

  TSE2UnitCache(FList[i]).Free;
  FList.Delete(i);
end;

procedure TSE2UnitCacheMngr.ClearCacheForRecompile(const Name: string);
var i: integer;
    p: TSE2UnitCache;
    c: integer;
begin
  ClearCache(Name);
  if FList.Count > 0 then
  begin
    repeat
      i := FList.Count-1;
      c := 0;
      repeat
        p := FList[i];
        if p.DependsOn.IndexOf(Name) >= 0 then
        begin
          ClearCacheForRecompile(p.AUnitName);
          c := c + 1;
        end;
        i := i - 1;
      until i < 0;
    until (c = 0) or (FList.Count = 0);
  end;
end;

constructor TSE2UnitCacheMngr.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TSE2UnitCacheMngr.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2UnitCacheMngr.GetCache(const Name: string): TSE2UnitCache;
var i     : integer;
begin
  result := nil;
  i := IndexOf(Name);
  if i = -1 then
     exit;


  result := TSE2UnitCache(FList[i]);
end;

function TSE2UnitCacheMngr.GetUnit(const Name: string; UnitList: TSE2BaseTypeList): TSE2Unit;
begin
  result := GetUnit(GetCache(Name), UnitList);
end;

function TSE2UnitCacheMngr.InternGetUnit(Cache: TSE2UnitCache;
  UnitList: TSE2BaseTypeList; Filter: TStrings): TSE2Unit;
var i: integer;
begin
  result := nil;
  if Cache = nil then
     exit;

  Filter.Add(Cache.AUnitName);
  for i:=0 to Cache.DependsOn.Count-1 do
    if UnitList.FindItem(Cache.DependsOn[i]) = nil then
    begin
      if Filter.IndexOf(Cache.DependsOn[i]) >= 0 then
         raise Exception.Create('Fatal: recursive dependency detected: ' + Cache.AUnitName + ' and ' + Cache.DependsOn[i]);
      result := InternGetUnit(GetCache(Cache.DependsOn[i]), UnitList, Filter);
      if result = nil then
         exit;
      UnitList.Add(result);
    end;

  result := nil;
  Cache.RestoreUnit(result);

  // Publish self
  PublishUnit(result, Cache.Weaver);

  // Publish every dependend unit
  PublishUnits(UnitList, Cache.Weaver);
end;

function TSE2UnitCacheMngr.GetUnit(Cache: TSE2UnitCache;
  UnitList: TSE2BaseTypeList): TSE2Unit;
var list: TStringList;
begin
  list := TStringList.Create;
  try
    result := InternGetUnit(Cache, UnitList, list);
  finally
    list.Free;
  end;
end;

function TSE2UnitCacheMngr.HasUnit(const Name: string): boolean;
begin
  result := IndexOf(Name) > -1;
end;

function TSE2UnitCacheMngr.IndexOf(const Name: string): integer;
var nameHash: integer;
begin
  nameHash := MakeHash(name);
  for result:=FList.Count-1 downto 0 do
    if TSE2UnitCache(FList[result]).NameHash = nameHash then
      if StringIdentical(TSE2UnitCache(FList[result]).AUnitName, Name) then
         exit;

  result := -1;
end;

procedure TSE2UnitCacheMngr.PublishEntry(Entry: TSE2BaseType;
  Weaver: TSE2NameWeaver);
var i: integer;
begin
  Weaver.SetValues( Entry.GetUniqueName(), Entry);

  if Entry is TSE2Method then
  begin
    //for i:=0 to TSE2Method(Entry).Params.Count-1 do
    //  PublishEntry(TSE2Method(Entry).Params[i], Weaver);

    //for i:=0 to TSE2Method(Entry).Variables.Count-1 do
    //  PublishEntry(TSE2Method(Entry).Variables[i], Weaver);

    for i:=0 to TSE2Method(Entry).Types.Count-1 do
      PublishEntry(TSE2Method(Entry).Types.List.List[i], Weaver);

    //if TSE2Method(Entry).ReturnValue <> nil then
    //   PublishEntry(TSE2Method(Entry).ReturnValue, Weaver);
  end;
end;

procedure TSE2UnitCacheMngr.PublishUnit(AUnit: TSE2Unit;
  Weaver: TSE2NameWeaver);
var i: integer;
begin
  PublishEntry(AUnit, Weaver);

  for i:=0 to aUnit.Strings.Count-1 do
    PublishEntry(aUnit.Strings[i], Weaver);

  for i:=0 to aUnit.TypeList.Count-1 do
    PublishEntry(aUnit.TypeList.List.List[i], Weaver);

  for i:=0 to aUnit.ElemList.Count-1 do
    PublishEntry(aUnit.ElemList.List.List[i], Weaver);

  //for i:=0 to aUnit.AInitialization.Count-1 do
  //  PublishEntry(aUnit.AInitialization[i], Weaver);

  //for i:=0 to aUnit.AInitialization.Count-1 do
  //  PublishEntry(aUnit.AFinalization[i], Weaver);

  //if aUnit.Main <> nil then
  //   PublishEntry(aUnit.Main, Weaver);
end;

procedure TSE2UnitCacheMngr.PublishUnits(UnitList: TSE2BaseTypeList;
  Weaver: TSE2NameWeaver);
var i: integer;
begin
  for i:=0 to UnitList.Count-1 do
    PublishUnit(TSE2Unit(UnitList.List.List[i]), Weaver);
end;

procedure TSE2UnitCacheMngr.CheckForChanges;
var list: TStringList;
    i   : integer;
begin
  if Assigned(FOnGetChangedUnits) then
  begin
    list := TStringList.Create;
    try
      list.Sorted := True;
      list.Duplicates := dupIgnore;
      list.CaseSensitive := False;

      FOnGetChangedUnits(Self, list);
      for i:=0 to list.Count-1 do
        ClearCacheForRecompile(list[i]);
        
    finally
      list.Free;
    end;
  end;
end;

end.
