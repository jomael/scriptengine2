unit uSE2UnitCacheMngr;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2BaseTypes, uSE2Types, uSE2UnitCache, uSE2NameWeaver;

type
  TSE2UnitCacheMngr = class(TSE2Object)
  private
    FList : TList;
  protected
    procedure PublishEntry(Entry: TSE2BaseType; Weaver: TSE2NameWeaver);
    procedure PublishUnit(AUnit: TSE2Unit; Weaver: TSE2NameWeaver);
    procedure PublishUnits(UnitList: TSE2BaseTypeList; Weaver: TSE2NameWeaver);
    function  IndexOf(const Name: string): integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(aUnit: TSE2Unit);
    function  GetCache(const Name: string): TSE2UnitCache;
    procedure ClearCache(const Name: string);
    function  HasUnit(const Name: string): boolean;
    function  GetUnit(const Name: string; UnitList: TSE2BaseTypeList): TSE2Unit; overload;
    function  GetUnit(Cache: TSE2UnitCache; UnitList: TSE2BaseTypeList): TSE2Unit; overload;
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

  Cache := TSE2UnitCache.Create(TMemoryStream.Create);
  Cache.StoreUnit(aUnit);
  Cache.AUnitName := aUnit.Name;
  FList.Add(Cache);
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

function TSE2UnitCacheMngr.GetUnit(Cache: TSE2UnitCache;
  UnitList: TSE2BaseTypeList): TSE2Unit;
var i: integer;
begin
  result := nil;
  if Cache = nil then
     exit;

  for i:=0 to Cache.DependsOn.Count-1 do
    if UnitList.FindItem(Cache.DependsOn[i]) = nil then
    begin
      result := GetUnit(Cache.DependsOn[i], UnitList);
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

function TSE2UnitCacheMngr.HasUnit(const Name: string): boolean;
begin
  result := IndexOf(Name) > -1;
end;

function TSE2UnitCacheMngr.IndexOf(const Name: string): integer;
begin
  for result:=FList.Count-1 downto 0 do
    if StringIdentical(TSE2UnitCache(FList[result]).AUnitName, Name) then
       exit;

  result := -1;
end;

procedure TSE2UnitCacheMngr.PublishEntry(Entry: TSE2BaseType;
  Weaver: TSE2NameWeaver);
var i: integer;
begin
  Weaver.SetValues(Entry.GetUniqueName(), Entry);

  if Entry is TSE2Method then
  begin
    //for i:=0 to TSE2Method(Entry).Params.Count-1 do
    //  PublishEntry(TSE2Method(Entry).Params[i], Weaver);

    //for i:=0 to TSE2Method(Entry).Variables.Count-1 do
    //  PublishEntry(TSE2Method(Entry).Variables[i], Weaver);

    for i:=0 to TSE2Method(Entry).Types.Count-1 do
      PublishEntry(TSE2Method(Entry).Types[i], Weaver);

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
    PublishEntry(aUnit.TypeList[i], Weaver);

  for i:=0 to aUnit.ElemList.Count-1 do
    PublishEntry(aUnit.ElemList[i], Weaver);

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
    PublishUnit(TSE2Unit(UnitList[i]), Weaver);
end;

end.
