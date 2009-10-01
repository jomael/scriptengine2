unit uSE2UnitManager;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2Types, uSE2BaseTypes, uSE2RunAccess;

type
  TSE2ManagedUnit = class;

  {$Hints off}
  TSE2UnitManager = class(TSE2Object)
  private
    FList : TList;
  protected
    constructor Create; override;

    function GetUnit(index: integer): TSE2ManagedUnit;
    function GetCount: integer;

    function IndexOf(aUnit: TSE2ManagedUnit): integer;
    function DoRegister(aUnit: TSE2ManagedUnit): boolean;
    function DoDelete(aUnit: TSE2ManagedUnit): boolean;
    procedure DoSort;
  public
    destructor Destroy; override;
    class function Instance: TSE2UnitManager;
    class function RegisterUnit(aUnit: TSE2ManagedUnit): boolean;
    class function UnRegisterUnit(aUnit: TSE2ManagedUnit): boolean;
    class function FindUnit(const Name: string): TSE2ManagedUnit;

    class procedure RegisterMethods(Target: TSE2RunAccess);
    class procedure RegisterVariables(Target: TSE2RunAccess);

    property Units[index: integer]: TSE2ManagedUnit read GetUnit; default;
    property Count                : integer         read GetCount;
  end;
  {$Hints on}

  TSE2ManagedUnit = class(TSE2Object)
  private
    FUnitName   : string;
    FNameHash   : integer;
    FPriority   : integer;
  protected
    procedure SetUnitName(const value: string);
  public
    constructor Create; override;
    destructor Destroy; override;

    function  LastChangeTime: TDateTime; virtual; abstract;
    function  CanCacheSource: boolean; virtual; abstract;

    procedure GetUnitSource(var Target: string); virtual; abstract;
    procedure RegisterVariables(const Target: TSE2RunAccess); virtual; abstract;
    procedure RegisterMethods(const Target: TSE2RunAccess); virtual; abstract;

    property  Priority: integer  read FPriority    write FPriority;
    {$Warnings off}
    property  UnitName: string   read FUnitName    write SetUnitName;
    {$Warnings on}
    property  NameHash: integer  read FNameHash;
  end;

  TSE2UnitLastChangeTime     = function: TDateTime;
  TSE2UnitCanCacheSource     = function: boolean;
  TSE2UnitGetUnitSource      = procedure(var Target: string);
  TSE2UnitRegisterVariables  = procedure(const Target: TSE2RunAccess);
  TSE2UnitRegisterMethods    = procedure(const Target: TSE2RunAccess);


  TSE2MethodUnit = class(TSE2ManagedUnit)
  public
    DoLastChangeTime    : TSE2UnitLastChangeTime;
    DoCanCacheSource    : TSE2UnitCanCacheSource;
    DoGetUnitSource     : TSE2UnitGetUnitSource;
    DoRegisterVariables : TSE2UnitRegisterVariables;
    DoRegisterMethods   : TSE2UnitRegisterMethods;
  public
    constructor Create; override;

    function LastChangeTime: TDateTime; override;
    function CanCacheSource: boolean; override;

    procedure GetUnitSource(var Target: string); override;
    procedure RegisterVariables(const Target: TSE2RunAccess); override;
    procedure RegisterMethods(const Target: TSE2RunAccess); override;
  end;

implementation

var
  unitMngr : TSE2UnitManager;

{ TSE2UnitManager }

constructor TSE2UnitManager.Create;
begin                       
  if Assigned(unitMngr) then
     raise Exception.Create('You can not create several instances of the unit manager');

  inherited;
  FList := TList.Create;
end;

destructor TSE2UnitManager.Destroy;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    TSE2ManagedUnit(FList[i]).Free;
    
  FList.Free;
  inherited;
  unitMngr := nil;
end;

function TSE2UnitManager.DoDelete(aUnit: TSE2ManagedUnit): boolean;
var i: integer;
begin
  i      := IndexOf(aUnit);
  result := i > -1;

  if result then
    FList.Delete(i);
end;

function TSE2UnitManager.DoRegister(aUnit: TSE2ManagedUnit): boolean;
begin
  result := False;
  if IndexOf(aUnit) > -1 then
     exit;

  FList.Add(aUnit);
  DoSort;
end;

class function TSE2UnitManager.FindUnit(
  const Name: string): TSE2ManagedUnit;
var i        : integer;
    NameHash : integer;
begin
  NameHash := MakeHash(Name);
  for i:=Instance.FList.Count-1 downto 0 do
    if Instance[i].NameHash = NameHash then
      if StringIdentical(Instance[i].UnitName, Name) then
      begin
        result := Instance[i];
        exit;
      end;
  result := nil;
end;

function TSE2UnitManager.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2UnitManager.GetUnit(index: integer): TSE2ManagedUnit;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := FList[index];
end;

function TSE2UnitManager.IndexOf(aUnit: TSE2ManagedUnit): integer;
begin
  for result:=FList.Count-1 downto 0 do
    if FList[result] = aUnit then
       exit;
  result := -1;
end;

class function TSE2UnitManager.Instance: TSE2UnitManager;
begin
  if unitMngr = nil then
     unitMngr := TSE2UnitManager.Create;

  result := unitMngr;
end;

class procedure TSE2UnitManager.RegisterMethods(Target: TSE2RunAccess);
var i: integer;
begin
  for i:=0 to Instance.FList.Count-1 do
    TSE2ManagedUnit(Instance.FList[i]).RegisterMethods(Target); 
end;

class procedure TSE2UnitManager.RegisterVariables(Target: TSE2RunAccess);
var i: integer;
begin
  for i:=0 to Instance.FList.Count-1 do
    TSE2ManagedUnit(Instance.FList[i]).RegisterVariables(Target);
end;

class function TSE2UnitManager.RegisterUnit(
  aUnit: TSE2ManagedUnit): boolean;
begin
  result := Instance.DoRegister(aUnit);
end;

class function TSE2UnitManager.UnRegisterUnit(
  aUnit: TSE2ManagedUnit): boolean;
begin
  result := Instance.DoDelete(aUnit);
end;

function ListSortMethod(Item1, Item2: TSE2ManagedUnit): integer;
begin
  if Item1.Priority < Item2.Priority then
     result := -1
  else
  if Item1.Priority > Item2.Priority then
     result := 1
  else
     result := 0;
end;

procedure TSE2UnitManager.DoSort;
begin
  FList.Sort(@ListSortMethod);
end;

{ TSE2ManagedUnit }

constructor TSE2ManagedUnit.Create;
begin
  inherited;
  FPriority := MaxInt;
end;

destructor TSE2ManagedUnit.Destroy;
begin
  inherited;
end;

procedure TSE2ManagedUnit.SetUnitName(const value: string);
begin
  FUnitName := value;
  FNameHash := MakeHash(value);
end;

function MethodUnit_LastChangeTime: TDateTime;
begin
  result := 0;
end;

function MethodUnit_CanCacheSource: boolean;
begin
  result := True;
end;

procedure MethodUnit_RegisterVariables(const Target: TSE2RunAccess);
begin

end;

{ TSE2MethodUnit }

function TSE2MethodUnit.CanCacheSource: boolean;
begin
  result := DoCanCacheSource;
end;

constructor TSE2MethodUnit.Create;
begin
  inherited;
  DoLastChangeTime    := MethodUnit_LastChangeTime;
  DoCanCacheSource    := MethodUnit_CanCacheSource;
  DoRegisterVariables := MethodUnit_RegisterVariables;         
end;

procedure TSE2MethodUnit.GetUnitSource(var Target: string);
begin
  DoGetUnitSource(Target);
end;

function TSE2MethodUnit.LastChangeTime: TDateTime;
begin
  result := DoLastChangeTime;
end;

procedure TSE2MethodUnit.RegisterMethods(const Target: TSE2RunAccess);
begin
  DoRegisterMethods(Target);
end;

procedure TSE2MethodUnit.RegisterVariables(const Target: TSE2RunAccess);
begin
  DoRegisterVariables(Target);
end;

initialization

finalization
  TSE2UnitManager.Instance.Free;

end.
