unit uSE2UnitCache;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2Types, uSE2BaseTypes, uSE2Consts, uSE2NameWeaver;

type
  TSE2UnitCache = class(TSE2Object)
  private
    FUnitName  : string;
    FNameHash  : integer;
    FDependsOn : TStringList;
    FStream    : TStream;
    FWeaver    : TSE2NameWeaver;
    FCacheTime : TDateTime;
  protected
    procedure SetUnitName(const value: string);
  public
    constructor Create(aStream: TStream); reintroduce;
    destructor Destroy; override;

    procedure StoreUnit(aUnit: TSE2Unit);
    procedure RestoreUnit(var Target: TSE2Unit);

    property CacheTime : TDateTime      read FCacheTime   write FCacheTime;
    property DependsOn : TStringList    read FDependsOn;
    property Stream    : TStream        read FStream;
    property Weaver    : TSE2NameWeaver read FWeaver;
    property NameHash  : integer        read FNameHash;
    property AUnitName : string         read FUnitName    write SetUnitName;
  end;

procedure RegisterComponent(aClass: TClass);
function  GetClassByName(const Name: string): TSE2BaseTypeClass;
function  GetNameByClass(aClass: TClass): string;

implementation

type
  PSE2BaseTypeClassEntry = ^TSE2BaseTypeClassEntry;
  TSE2BaseTypeClassEntry = record
    Name     : string;
    NameHash : integer;
    AClass   : TClass;
  end;

  TSE2BaseTypeClassList = class
  private
    FList : TList;
  protected
    function IndexOf(const Name: string): integer; overload;
    function IndexOf(AClass: TClass): integer; overload;
    function GetItem(const Name: string): TClass;
    function GetName(AClass: TClass): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure RegisterClass(aClass: TClass);
    property Names[index: TClass]: string read GetName;

    property Items[const index: string]: TClass read GetItem;
  end;

var
  SE2BaseTypeClassList : TSE2BaseTypeClassList;


procedure RegisterComponent(aClass: TClass);
begin
  if SE2BaseTypeClassList = nil then
     SE2BaseTypeClassList := TSE2BaseTypeClassList.Create;
  SE2BaseTypeClassList.RegisterClass(aClass);
end;

procedure RegisterClasses;
begin
  FreeAndNil(SE2BaseTypeClassList);

  RegisterComponent(TSE2Array);
  RegisterComponent(TSE2BaseType);
  RegisterComponent(TSE2Class);
  RegisterComponent(TSE2CodeElement);
  RegisterComponent(TSE2Constant);
  RegisterComponent(TSE2Enum);
  RegisterComponent(TSE2LinkString);
  RegisterComponent(TSE2Method);
  RegisterComponent(TSE2Parameter);
  RegisterComponent(TSE2ParamExpression);
  RegisterComponent(TSE2Record);
  RegisterComponent(TSE2SetOf);
  RegisterComponent(TSE2Type);
  RegisterComponent(TSE2Unit);
  RegisterComponent(TSE2Variable);
  RegisterComponent(TSE2Property);
  RegisterComponent(TSE2ClassRTTI);
  RegisterComponent(TSE2UsedByList);
  RegisterComponent(TSE2MethodVariable);
end;

function  GetClassByName(const Name: string): TSE2BaseTypeClass;
begin
  result := TSE2BaseTypeClass(SE2BaseTypeClassList.Items[Name]);
end;

function  GetNameByClass(aClass: TClass): string;
begin
  result := SE2BaseTypeClassList.Names[aClass];
end;

procedure ClearRegisteredClasses;
begin
  FreeAndNil(SE2BaseTypeClassList);
end;


{ TSE2BaseTypeClassList }

procedure TSE2BaseTypeClassList.Clear;
var i: integer;
    p: PSE2BaseTypeClassEntry;
begin
  for i:=FList.Count-1 downto 0 do
  begin
    p := FList[i];
    p^.Name     := '';
    p^.NameHash := 0;
    p^.AClass   := nil;
    Dispose(p);
  end;
  FList.Clear;
end;

constructor TSE2BaseTypeClassList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TSE2BaseTypeClassList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2BaseTypeClassList.GetItem(const Name: string): TClass;
var i: integer;
begin
  result := nil;
  i := IndexOf(Name);
  if i < 0 then
     exit;

  result := PSE2BaseTypeClassEntry(FList[i])^.AClass;
end;

function TSE2BaseTypeClassList.GetName(AClass: TClass): string;
var i: integer;
begin
  result := '';
  i := IndexOf(AClass);
  if i < 0 then
     exit;

  result := PSE2BaseTypeClassEntry(FList[i])^.Name;
end;

function TSE2BaseTypeClassList.IndexOf(AClass: TClass): integer;
begin
  for result:=FList.Count-1 downto 0 do
    if PSE2BaseTypeClassEntry(FList[result])^.AClass = AClass then
      exit;
  result := -1;
end;

function TSE2BaseTypeClassList.IndexOf(const Name: string): integer;
var nameHash: integer;
begin
  nameHash := MakeHash(Name);
  for result:=FList.Count-1 downto 0 do
    if PSe2BaseTypeClassEntry(FList[result])^.NameHash = nameHash then
      if StringIdentical(PSE2BaseTypeClassEntry(FList[result])^.Name, Name) then
        exit;
  result := -1;
end;

procedure TSE2BaseTypeClassList.RegisterClass(aClass: TClass);
var p: PSE2BaseTypeClassEntry;
begin
  if IndexOf(aClass) > -1 then
     exit;

  New(p);
  p^.Name   := aClass.ClassName;
  p^.NameHash := MakeHash(p^.Name);
  p^.AClass := aClass;
  FList.Add(p);
end;

{ TSE2UnitCache }

constructor TSE2UnitCache.Create(aStream: TStream);
begin
  inherited Create;
  FStream    := aStream;
  FWeaver    := TSE2NameWeaver.Create;
  FDependsOn := TStringList.Create;
  FDependsOn.CaseSensitive := False;
  FDependsOn.Sorted := True;
  FDependsOn.Duplicates := dupIgnore;
end;

destructor TSE2UnitCache.Destroy;
begin
  FDependsOn.Free;
  FWeaver.Free;
  FStream.Free;
  inherited;
end;

procedure TSE2UnitCache.RestoreUnit(var Target: TSE2Unit);
begin
  FWeaver.Clear;
  FStream.Position := 0;

  TSE2StreamHelper.ReadString(Stream);
  Target := TSE2Unit.Create;
  Target.LoadFromStream(Stream, FWeaver);
end;

procedure TSE2UnitCache.SetUnitName(const value: string);
begin
  FUnitName := value;
  FNameHash := MakeHash(value);
end;

procedure TSE2UnitCache.StoreUnit(aUnit: TSE2Unit);
begin
  FWeaver.Clear;
  FStream.Size := 0;
  FDependsOn.Assign(aUnit.RequiredUnits);
  FCacheTime   := Now;

  aUnit.SaveToStream(FStream);
end;

initialization
  RegisterClasses;

finalization
  ClearRegisteredClasses;

end.
