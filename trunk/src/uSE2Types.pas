unit uSE2Types;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Dialogs,


  Classes, uSE2Tokenizer, uSE2Reader, uSE2BaseTypes, uSE2Consts, uSE2OpCode,
  uSE2NameWeaver;

{$IFDEF SEII_FPC}
  {$HINTS OFF}
{$ENDIF}

type
  TSE2Visibility    = (visPrivate, visProtected, visPublic);
  TSE2Visibilities  = set of TSE2Visibility;

const
  CAllVisibilities     : TSE2Visibilities = [visPrivate, visProtected, visPublic];
  CPublicVisibilities  : TSE2Visibilities = [visPublic];

type
  TSE2IntegerList = class(TSE2Object)
  private
    FList : TList;
  protected
    function  GetCount: integer;
    function  GetItem(index: integer): integer;
    procedure SetItem(index, value: integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure Clear;
    procedure Add(value: integer);
    function  Delete(index: integer): boolean;
    function  IndexOf(value: integer): integer;

    property  Items[index: integer]: integer read GetItem  write SetItem; default;
    property  Count                : integer read GetCount; 
  end;

  //TSE2CompatibleList = class;

  TSE2BaseType = class(TSE2Object)
  private
    FName            : string;
    FNameHash        : integer;
    FUnitName        : string;
    FUnitHash        : integer;
    FVisibility      : TSE2Visibility;
    FID              : integer;

    FInheritsFrom    : TSE2BaseType;
    FParent          : TSE2BaseType;
    FDeclLine        : integer;
    FDeclPos         : integer;

    FBaseCompability : TSE2TokenTypes;
    FIsDeprecated    : boolean;
    FDeprecatedValue : string;

    //FCompability     : TSE2CompatibleList;
  protected
    procedure SetName(const value: string);
    procedure SetUnitName(const value: string);

    function  GetBaseCompability : TSE2TokenTypes;
    procedure SetBaseCompability(value: TSE2TokenTypes);

    function  GetRootType: TSE2BaseType;
  public
    constructor Create; override;
    destructor Destroy; override;
    
    procedure  LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); virtual;
    procedure  SaveToStream(Stream: TStream); virtual;

    function   GenLinkerName(i: integer = -1): string; virtual;
    function   GetUniqueName(i: integer = -1): string;

    function   IsName(const value: string): boolean; overload;
    function   IsName(const value: string; const NameHash: integer): boolean; overload;
    function   IsUnit(const value: string): boolean; overload;
    function   IsUnit(const value: string; const NameHash: integer): boolean; overload;
    function   IsChildOf(const Parent: TSE2BaseType): boolean;
    function   IsTypeOf(const BaseType: TSE2BaseType): boolean;
    //function   IsCompatible(Action: TSE2TokenType; const ToObj: TSE2BaseType): boolean; overload;
    //function   IsCompatible(Action: TSE2TokenType; ToObj: array of TSE2BaseType): boolean; overload;

    property   Name            : string             read FName           write SetName;
    property   NameHash        : integer            read FNameHash;
    property   AUnitName       : string             read FUnitName       write SetUnitName;
    property   UnitHash        : integer            read FUnitHash;   
    property   ID              : integer            read FID             write FID;
    property   DeprecatedValue : string             read FDeprecatedValue write FDeprecatedValue;
    property   IsDeprecated    : boolean            read FIsDeprecated   write FIsDeprecated;


    property   Visibility      : TSE2Visibility     read FVisibility     write FVisibility;
    property   DeclLine        : integer            read FDeclLine       write FDeclLine;
    property   DeclPos         : integer            read FDeclPos        write FDeclPos;
    property   InheritFrom     : TSE2BaseType       read FInheritsFrom   write FInheritsFrom;
    property   InheritRoot     : TSE2BaseType       read GetRootType;
    property   Parent          : TSE2BaseType       read FParent         write FParent;

    property   BaseCompatibles : TSE2TokenTypes     read GetBaseCompability write SetBaseCompability;
    //property   Compatibles     : TSE2CompatibleList read FCompability;
  end;
  TSE2BaseTypeClass   = class of TSE2BaseType;
  TSE2BaseTypeClasses = array of TSE2BaseTypeClass;

  TSE2BaseTypeFilter = class(TSE2Object)
  public
    Filters : TSE2BaseTypeClasses;
    constructor Create(Filter: array of TSE2BaseTypeClass); reintroduce;

    function Duplicate: TSE2BaseTypeFilter;
    function TypeIsIn(aType: TSE2BaseTypeClass): boolean;
  end;

  TSE2BaseTypeList = class(TSE2Object)
  private
    FList      : TList;
    FOwnsObjs  : boolean;
    FSetID     : boolean;
  protected
    function  GetItem(i: integer): TSE2BaseType;
    procedure SetItem(i: integer; const obj: TSE2BaseType);
    function  GetCount: integer;
    procedure SetEntryID(Entry: TSE2BaseType);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver);
    procedure SaveToStream(Stream: TStream);

    procedure Clear;
    function  IndexOf(obj: TSE2BaseType): integer;
    procedure CopyFrom(List: TSE2BaseTypeList);

    procedure Insert(const Index: integer; Item: TSE2BaseType);
    procedure Add(const Item: TSE2BaseType);
    function  Delete(index: integer): boolean; overload;
    function  Delete(Item: TSE2BaseType): boolean; overload;

    function  FindItem(const Name: string;
                       const DeclUnit: string = '';
                       const Parent: TSE2BaseType = nil;
                       const InheritedFrom: TSE2BaseType = nil;
                       const ClassTypes: TSE2BaseTypeFilter = nil;
                       Visibility: TSE2Visibilities = [visPublic];
                       AcceptSetType: boolean = False): TSE2BaseType;

    property  Items[index: integer]: TSE2BaseType   read GetItem       write SetItem; default;
    property  Count                : integer        read GetCount;
    property  OwnsObjs             : boolean        read FOwnsObjs     write FOwnsObjs;
    property  SetID                : boolean        read FSetID        write FSetID;
  end;

(*  TSE2Compatibility = class(TSE2Object)
  private
    FEntry     : TSE2BaseType;
    FAllowed   : TSE2TokenTypes;
    FRecursive : boolean;
  public
    constructor Create(Entry: TSE2BaseType; Allowed: TSE2TokenTypes; Recursive: boolean); reintroduce;
    destructor Destroy; override;

    property Entry     : TSE2BaseType    read FEntry       write FEntry;
    property Allowed   : TSE2TokenTypes  read FAllowed     write FAllowed;
    property Recursive : boolean         read FRecursive   write FRecursive;
  end;

  TSE2CompatibleList = class(TSE2Object)
  private
    FList      : TList;
  protected
    function IndexOf(const obj: TSE2BaseType): integer;
    function Delete(index: integer): boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure AddCompatbile(const obj: TSE2BaseType; Allowed: TSE2TokenTypes; Recursive: boolean);
    function  DeleteCompatible(const obj: TSE2BaseType): boolean;
    function  IsCompatible(obj: TSE2BaseType; Action: TSE2TokenType): boolean;
  end;            *)

  TSE2UsedByList = class(TSE2BaseType)
  private
    FList : TList;
  protected
    function GetCount: integer;
    function GetItem(index: integer): TSE2BaseType;
    function IndexOf(Item: TSE2BaseType): integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    procedure Clear;
    procedure Add(Item: TSE2BaseType);

    property  Items[index: integer]: TSE2BaseType read GetItem; default;
    property  Count                : integer      read GetCount;
  end;

  PSE2ClassRTTIEntry = ^TSE2ClassRTTIEntry;
  TSE2ClassRTTIEntry = record
    Offset : integer;
    Size   : integer;
    aType  : TSE2TypeIdent;
  end;

  TSE2ClassRTTI = class(TSE2BaseType)
  private
    FList : TList;
  protected
    function GetCount: integer;
    function GetItem(index: integer): PSE2ClassRTTIEntry;
  public          
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(aType: TSE2TypeIdent; Offset, Size: integer); overload;
    procedure Add(Item: PSE2ClassRTTIEntry); overload;


    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;    
    procedure SaveToStream(Stream: TStream); override;
    function  Duplicate(index, deltaOffset: integer): PSE2ClassRTTIEntry;

    property  Items[index: integer]: PSE2ClassRTTIEntry read GetItem; default;
    property  Count                : integer            read GetCount;
  end;

  TSE2Type = class(TSE2BaseType)
  private
    FType     : TSE2TypeIdent;
    FDataSize : integer;
    FStrict   : boolean;
    FMetaIndex: integer;
  public
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    property MetaIndex: integer         read FMetaIndex       write FMetaIndex;
    property AType    : TSE2TypeIdent   read FType            write FType;
    property Strict   : boolean         read FStrict          write FStrict;
    property DataSize : integer         read FDataSize        write FDataSize;
  end;

  TSE2Constant = class(TSE2BaseType)
  private
    FType   : TSE2Type;
    FValue  : string;
  protected
    function  GetAsInteger: int64;
    function  GetAsFloat  : double;
    procedure SetAsInteger(value: int64);
    procedure SetAsFloat(value: double);
  public
    property AType     : TSE2Type        read FType            write FType;

    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    function IsInteger: boolean;
    function IsFloat: boolean;

    property Value     : string          read FValue           write FValue;
    property AsInteger : int64           read GetAsInteger     write SetAsInteger;
    property AsFloat   : double          read GetAsFloat       write SetAsFloat;
  end;

  TSE2Enum = class(TSE2Type)
  end;

  TSE2SetOf = class(TSE2Type)
  end;

  TSE2Record = class(TSE2Type)
  private
    FVariables  : TSE2BaseTypeList;
    FRecordSize : integer;   
    FRTTI       : TSE2ClassRTTI;
  public
    constructor Create; override;
    destructor Destroy; override;


    function GenLinkerName(i: Integer = -1): String; override;
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    property RTTI       : TSE2ClassRTTI       read FRTTI;
    property Variables  : TSE2BaseTypeList    read FVariables;
    property RecordSize : integer             read FRecordSize  write FRecordSize;
  end;

  TSE2Array = class(TSE2Type)
  private
    FArrayType  : TSE2Type;
    FArrayCount : integer;
    FStartIndex : integer;
  public
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    property ArrayType  : TSE2Type   read FArrayType    write FArrayType;
    property ArrayCount : integer    read FArrayCount   write FArrayCount;
    property StartIndex : integer    read FStartIndex   write FStartIndex;
  end;

  TSE2CodeElement = class(TSE2BaseType)
  private
    FCodePos  : integer;
    FDebugPos : integer;
    FUsed     : boolean;
  public
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    property CodePos  : integer         read FCodePos         write FCodePos;
    property DebugPos : integer         read FDebugPos        write FDebugPos;
    property Used     : boolean         read FUsed            write FUsed;
  end;

  TSE2Variable = class(TSE2CodeElement)
  private
    FType       : TSE2Type;
    FIsStatic   : boolean;   // Fixed Memory Position
    FIsExternal : boolean;   // Can be written by the host program
    FIsPublic   : boolean;   // Host program can read the variable
  public
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    property AType       : TSE2Type         read FType            write FType;
    property IsStatic    : boolean          read FIsStatic        write FIsStatic;
    property IsExternal  : boolean          read FIsExternal      write FIsExternal;
    property IsPublic    : boolean          read FIsPublic        write FIsPublic;
  end;

  TSE2ParameterType = (ptDefault, ptConst, ptVar);

  TSE2Parameter = class(TSE2Variable)
  private
    FParameterType : TSE2ParameterType;
  public
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    property ParameterType : TSE2ParameterType read FParameterType write FParameterType;
  end;

  TSE2ParamExpression = class(TSE2BaseType)
  private
    FType     : TSE2Type;
    FCodePos  : integer;
    FVariable : TSE2Variable;
  public
    constructor Create; overload; override;
    constructor Create(AType: TSE2Type; Variable: TSE2Variable; CodePos: integer); reintroduce; overload;
    
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    property  AType       : TSE2Type     read FType     write FType;
    property  Variable    : TSE2Variable read FVariable write FVariable;
    property  CodePos     : integer      read FCodePos  write FCodePos;
  end;

  TSE2Class = class(TSE2Type)
  private
    FMethods     : TSE2BaseTypeList;
    FIsForwarded : boolean;
    FClassSize   : integer;
    FVariables   : integer;
    FDynMethods  : integer;
    FIsPartial   : boolean;
    FIsHelper    : boolean;
    FRTTI        : TSE2ClassRTTI;
  protected
    function  GetParentClass : TSE2Class;
    procedure SetParentClass(value: TSE2Class);
  public
    constructor Create; override;
    destructor Destroy; override;

    function GenLinkerName(i: Integer = -1): String; override;   

    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    //property Methods      : TSE2BaseTypeList       read FMethods;
    //property ParentClass  : TSE2Class              read GetParentClass write SetParentClass;
    property RTTI         : TSE2ClassRTTI          read FRTTI;
    property DynMethods   : integer                read FDynMethods    write FDynMethods;
    property ClassSize    : integer                read FClassSize     write FClassSize;
    property Variables    : integer                read FVariables     write FVariables;
    property IsForwarded  : boolean                read FIsForwarded   write FIsForwarded;
    property IsPartial    : boolean                read FIsPartial     write FIsPartial;
    property IsHelper     : boolean                read FIsHelper      write FIsHelper;
  end;

  TSE2MethodType = (mtProcedure, mtFunction, mtConstructor, mtDestructor);

  TSE2MethodLists = class(TSE2Object)
  private
    FContinueList   : TSE2IntegerList;
    FBreakList      : TSE2IntegerList;
    FExitList       : TSE2IntegerList;
    FExitTableList  : TSE2IntegerList;    
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property ContinueList      : TSE2IntegerList    read FContinueList;
    property BreakList         : TSE2IntegerList    read FBreakList;
    property ExitList          : TSE2IntegerList    read FExitList;
    property ExitTableList     : TSE2IntegerList    read FExitTableList;
  end;

  TSE2Method = class(TSE2CodeElement)
  private
    FParams         : TSE2BaseTypeList;
    FIsAbstract     : boolean;
    FIsForwarded    : boolean;    // forward or interface declaration
    FIsExternal     : boolean;    // External method
    FIsOverload     : boolean;    // overloaded
    FIsVirtual      : boolean;
    FIsOverride     : boolean;
    FIsStatic       : boolean;
    FIsExport       : boolean;
    FIsMethodType   : boolean;
    FAddHasSelfParam: boolean;
    FCallConvention : TSE2CallType;
    FReturnValue    : TSE2Variable;
    FStackSize      : integer;
    FDynamicIndex   : integer;
    //FOwnerClass     : TSE2Class;
    FMethodType     : TSE2MethodType;
    FLists          : TSE2MethodLists;
    FUsedMethods    : TSE2UsedByList;
    FUsageProcessed : boolean;


    FVariables      : TSE2BaseTypeList;
    FTypes          : TSE2BaseTypeList;

    FOpCodes        : TSE2LinkOpCodeList;
  protected
    function GetHasSelfParam: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    property Params            : TSE2BaseTypeList   read FParams;
    property OpCodes           : TSE2LinkOpCodeList read FOpCodes;

    property Variables         : TSE2BaseTypeList   read FVariables;
    property Types             : TSE2BaseTypeList   read FTypes;
    property Lists             : TSE2MethodLists    read FLists;

    property UsageProcessed    : boolean            read FUsageProcessed  write FUsageProcessed;
    property UsedMethods       : TSE2UsedByList     read FUsedMethods;
    property AddHasSelfParam   : boolean            read FAddHasSelfParam write FAddHasSelfParam;
    property HasSelfParam      : boolean            read GetHasSelfParam;
    property DynamicIndex      : integer            read FDynamicIndex    write FDynamicIndex;
    property IsStatic          : boolean            read FIsStatic        write FIsStatic;
    property IsVirtual         : boolean            read FIsVirtual       write FIsVirtual;
    property IsAbstract        : boolean            read FIsAbstract      write FIsAbstract;
    property IsOverride        : boolean            read FIsOverride      write FIsOverride;
    property IsExternal        : boolean            read FIsExternal      write FIsExternal;
    property IsOverload        : boolean            read FIsOverload      write FIsOverload; 
    property IsExport          : boolean            read FIsExport        write FIsExport;
    property IsMethodType      : boolean            read FIsMethodType    write FIsMethodType;
    property CallConvention    : TSE2CallType       read FCallConvention  write FCallConvention;
    property ReturnValue       : TSE2Variable       read FReturnValue     write FReturnValue;
    property StackSize         : integer            read FStackSize       write FStackSize;
    //property OwnerClass        : TSE2Class          read FOwnerClass      write FOwnerClass;
    property IsForwarded       : boolean            read FIsForwarded     write FIsForwarded;
    property MethodType        : TSE2MethodType     read FMethodType      write FMethodType;
  end;

  TSE2Property = class(TSE2BaseType)
  private
    FGetter   : TSE2BaseType;
    FSetter   : TSE2BaseType;
    FParams   : TSE2BaseTypeList;
    FType     : TSE2Type;
    FIsStatic : boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    procedure AddParam(const Name: string; const aType: TSE2Type);
    procedure FillEmptyParams(newType: TSE2Type);

    property Params   : TSE2BaseTypeList read FParams;
    property IsStatic : boolean      read FIsStatic  write FIsStatic;
    property Getter   : TSE2BaseType read FGetter    write FGetter;
    property Setter   : TSE2BaseType read FSetter    write FSetter;
    property AType    : TSE2Type     read FType      write FType;
  end;

  TSE2LinkStringList = class;

  TSE2Unit = class(TSE2BaseType)
  private
    FTypeList         : TSE2BaseTypeList;
    FElemList         : TSE2BaseTypeList;
    FStrings          : TSE2LinkStringList;
    FInitializations  : TSE2BaseTypeList;
    FFinalizations    : TSE2BaseTypeList;
    FMain             : TSE2Method;
    FIsProgram        : boolean;
    FRequiredUnits    : TStringList;
  protected
    procedure SetMain(value: TSE2Method);
  public
    constructor Create; override;
    destructor Destroy; override;
    
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    property RequiredUnits   : TStringList        read FRequiredUnits;
    property Strings         : TSE2LinkStringList read FStrings;
    property TypeList        : TSE2BaseTypeList   read FTypeList;
    property ElemList        : TSE2BaseTypeList   read FElemList;
    property AInitialization : TSE2BaseTypeList   read FInitializations;
    property AFinalization   : TSE2BaseTypeList   read FFinalizations;
    property Main            : TSE2Method         read FMain                   write SetMain;
    property IsProgram       : boolean            read FIsProgram              write FIsProgram;
  end;

  TSE2LinkString = class(TSE2BaseType)
  private
    FValue : string;
    FID    : integer;
    FOwner : TSE2Unit;
  public
    constructor Create(const Owner: TSE2Unit; const Value: string; ID: integer); reintroduce;
    destructor Destroy; override;
    
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;

    function GenLinkerName(i: integer = -1): string; override;

    property Owner   : TSE2Unit read FOwner   write FOwner;
    property Value   : string   read FValue   write FValue;
    property ID      : integer  read FID      write FID;
  end;

  TSE2LinkStringList = class(TSE2Object)
  private
    FList   : TList;
    FNextID : integer;
  protected
    function   GetItem(index: integer): TSE2LinkString;
    function   GetCount: integer;
    function   GetNextID: integer;
    procedure  ResetNextID;
  public
    constructor Create; override;
    destructor Destroy; override;
    
    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver);
    procedure SaveToStream(Stream: TStream);

    procedure Clear;
    function Add(Owner: TSE2Unit; const value: string): TSE2LinkString;
    function IndexOf(const ID: integer): integer; overload;
    function IndexOf(const value: string): integer; overload;
    function Delete(index: integer): boolean;

    property Items[index: integer]: TSE2LinkString read GetItem; default;
    property Count: integer                        read GetCount;
  end;

  TSE2MethodVariable = class(TSE2Type)
  private
    FMethod: TSE2Method;
  protected
    procedure SetMethod(value: TSE2Method);
  public                 
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver); override;
    procedure SaveToStream(Stream: TStream); override;         

    property Method: TSE2Method read FMethod write SetMethod;
  end;

implementation

uses SysUtils, uSE2UnitCache, StrUtils;

const
  TSE2IntegerList_CurrentVersion  = 1;
  TSE2BaseType_CurrentVersion     = 1;
  TSE2BaseTypeList_CurrentVersion = 1;
  TSE2Constant_CurrentVersion     = 1;
  TSE2Type_CurrentVersion         = 1;

procedure SaveWeaverData(Stream: TStream; obj: TSE2BaseType);
var s: string;
begin
  s := '';
  if obj <> nil then
  begin
    if obj.ID > 0 then
      s := obj.GetUniqueName()
    else
      s := obj.GetUniqueName();
  end;

  if (Pos('integer', s) > 0) and (Pos('[1]', s) > 0) then
  begin
    TSE2StreamHelper.WriteString(Stream, s);
    if obj <> nil then
  end
  else
    TSE2StreamHelper.WriteString(Stream, s);
end;

procedure RaiseIncompatibleStream;
begin
  raise ESE2InvalidDataStream.Create('Incompatible stream version');
end;

{ TSE2BaseType }

constructor TSE2BaseType.Create;
begin
  inherited;
  Name := '';
  FVisibility  := visPublic;
  //FCompability := TSE2CompatibleList.Create;
end;

destructor TSE2BaseType.Destroy;
begin
  //FCompability.Free;
  inherited;
end;

function TSE2BaseType.IsName(const value: string): boolean;
begin
  result := StringIdentical(FName, value);
end;

function TSE2BaseType.IsChildOf(const Parent: TSE2BaseType): boolean;
var Obj: TSE2BaseType;
begin
  if (Parent = nil) and (FParent = nil) then
  begin
    result := True;
    exit;
  end;
  Obj    := FParent;
  result := False;
  while (Obj <> nil) and (not result) do
  begin
    if Obj = Parent then
    begin
      result := True;
      exit;
    end;
    Obj := Obj.Parent;
  end;
end;

function TSE2BaseType.IsName(const value: string;
  const NameHash: integer): boolean;
begin
  if NameHash = FNameHash then
     result := IsName(value)
  else
     result := False;
end;

function TSE2BaseType.IsTypeOf(const BaseType: TSE2BaseType): boolean;
var Obj: TSE2BaseType;
begin
  Obj    := FInheritsFrom;
  result := False;
  while (Obj <> nil) and (not result) do
  begin
    if Obj = BaseType then
    begin
      result := True;
      exit;
    end;
    Obj := Obj.InheritFrom;
  end;
end;

procedure TSE2BaseType.SetName(const value: string);
begin
  FName     := value;
  FNameHash := MakeHash(value);
end;

procedure TSE2BaseType.SetUnitName(const value: string);
begin
  FUnitName := value;
  FUnitHash := MakeHash(value);
end;

function TSE2BaseType.IsUnit(const value: string): boolean;
begin
  result := StringIdentical(value, FUnitName);
end;

function TSE2BaseType.IsUnit(const value: string;
  const NameHash: integer): boolean;
begin
  if FUnitHash = NameHash then
     result := IsUnit(value)
  else
     result := False;
end;

(*function TSE2BaseType.IsCompatible(Action: TSE2TokenType; const ToObj: TSE2BaseType): boolean;
begin
  result := True;
  if ToObj = nil then
     exit;

  result := FCompability.IsCompatible(ToObj, Action);
end;

function TSE2BaseType.IsCompatible(Action: TSE2TokenType; ToObj: array of TSE2BaseType): boolean;
var i: integer;
begin
  result := False;
  for i:=Low(ToObj) to High(ToObj) do
    if not IsCompatible(Action, ToObj[i]) then
      exit;
  result := True;
end;       *)

function TSE2BaseType.GetBaseCompability: TSE2TokenTypes;
begin
  if FInheritsFrom <> nil then
     result := FInheritsFrom.GetBaseCompability
  else
     result := FBaseCompability;
end;

procedure TSE2BaseType.SetBaseCompability(value: TSE2TokenTypes);
begin
  if FInheritsFrom <> nil then
     FInheritsFrom.SetBaseCompability(value)
  else
     FBaseCompability := value;
end;

function TSE2BaseType.GetRootType: TSE2BaseType;
begin
  if FInheritsFrom <> nil then
     result := FInheritsFrom.GetRootType
  else
     result := Self;
end;

function TSE2BaseType.GenLinkerName(i: integer = -1): string;
begin
  result := GetUniqueName(i);
end;

function TSE2BaseType.GetUniqueName(i: integer): string;
var TypeName : string;
    Parents  : string;
    obj      : TSE2BaseType;
begin
  if i = -1 then
     i := Self.ID;

  Parents := '';
  obj := Self.Parent;
  while obj <> nil do
  begin
    Parents := obj.Name + '.' + Parents;
    obj := obj.Parent;
  end;

  TypeName := Self.ClassName;
  result := '['+TypeName+']' +
            '['+Self.AUnitName+'].' +
            '['+Parents + Self.Name+'].' +
            '['+IntToStr(i)+']';
end;

procedure TSE2BaseType.LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver);
var version: byte;
begin
  // DO NOT READ THE CLASS TYPE NAME - IT WAS ALREADY READ BEFORE
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        TSE2StreamHelper.ReadString(Stream, FName);
        Stream.Read(FNameHash, SizeOf(FNameHash));
        TSE2StreamHelper.ReadString(Stream, FUnitName);
        Stream.Read(FUnitHash, SizeOf(integer));
        Stream.Read(FVisibility, SizeOf(TSE2Visibility));
        Stream.Read(FID, SizeOf(integer));
        Stream.Read(FIsDeprecated, SizeOf(boolean));
        TSE2StreamHelper.ReadString(Stream, FDeprecatedValue);

        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FInheritsFrom);  
        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FParent);

        Stream.Read(FDeclLine        , SizeOf(integer));
        Stream.Read(FDeclPos         , SizeOf(integer));

        Stream.Read(FBaseCompability , SizeOf(TSE2TokenTypes));


      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2BaseType.SaveToStream(Stream: TStream);
var version: byte;
begin
  // Save Class Name
  TSE2StreamHelper.WriteString(Stream, uSE2UnitCache.GetNameByClass(Self.ClassType));

  // now save the data
  version := TSE2BaseType_CurrentVersion;
  Stream.Write(version, SizeOf(version));


  TSE2StreamHelper.WriteString(Stream, FName);
  Stream.Write(FNameHash, SizeOf(FNameHash));
  TSE2StreamHelper.WriteString(Stream, FUnitName);
  Stream.Write(FUnitHash, SizeOf(integer));
  Stream.Write(FVisibility, SizeOf(TSE2Visibility));
  Stream.Write(FID, SizeOf(integer));
  Stream.Write(FIsDeprecated, SizeOf(boolean));   
  TSE2StreamHelper.WriteString(Stream, FDeprecatedValue);

  SaveWeaverData(Stream, FInheritsFrom);  
  SaveWeaverData(Stream, FParent);

  Stream.Write(FDeclLine        , SizeOf(integer));
  Stream.Write(FDeclPos         , SizeOf(integer));

  Stream.Write(FBaseCompability , SizeOf(TSE2TokenTypes));
end;

{ TSE2BaseTypeList }

procedure TSE2BaseTypeList.Add(const Item: TSE2BaseType);
begin
  if FSetID then
     SetEntryID(Item);
  FList.Add(Item);
end;

procedure TSE2BaseTypeList.Insert(const Index: integer;
  Item: TSE2BaseType);
begin
  if FSetID then
     SetEntryID(Item);
  FList.Insert(Index, Item);
end;

procedure TSE2BaseTypeList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Delete(i);
end;

procedure TSE2BaseTypeList.CopyFrom(List: TSE2BaseTypeList);
var i: integer;
begin
  for i:=0 to List.Count-1 do
    Add(List[i]);
  List.OwnsObjs := False;
  List.Clear;
end;

constructor TSE2BaseTypeList.Create;
begin
  inherited;
  FOwnsObjs   := True;
  FSetID      := True;
  FList       := TList.Create;
end;

function TSE2BaseTypeList.Delete(index: integer): boolean;
var obj: TSE2BaseType;
begin
  if (index < 0) or (index >= FList.Count) then
     result := False
  else
  begin

    obj := FList[index];
    FList.Delete(index);
    try
      if FOwnsObjs then
         obj.Free;
    except
      
    end;
    result := True;
  end;
end;

function TSE2BaseTypeList.Delete(Item: TSE2BaseType): boolean;
begin
  result := Delete(IndexOf(Item));
end;

destructor TSE2BaseTypeList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2BaseTypeList.FindItem(const Name, DeclUnit: string;
  const Parent, InheritedFrom: TSE2BaseType; const ClassTypes: TSE2BaseTypeFilter; Visibility: TSE2Visibilities;
  AcceptSetType: boolean): TSE2BaseType;
var NameHash : integer;
    UnitHash : integer;
    i, c     : integer;
    obj      : TSE2BaseType;
begin
  NameHash := MakeHash(Name);
  UnitHash := MakeHash(DeclUnit);

  for i:=FList.Count-1 downto 0 do
  begin
    obj := FList[i];

    if not (obj.Visibility in Visibility) then
       continue;

    if obj is TSE2Constant then
      if TSE2Constant(obj).AType is TSE2SetOf then
        if not AcceptSetType then
           continue;

    if not obj.IsName(Name, NameHash) then
       continue;

    if DeclUnit <> '' then
       if not obj.IsUnit(DeclUnit, UnitHash) then
          continue;

    //if Parent <> nil then
       if not obj.IsChildOf(Parent) then
          continue;

    if InheritedFrom <> nil then
       if not obj.IsTypeOf(InheritedFrom) then
          continue;

    if ClassTypes <> nil then
    begin
      for c:=Low(ClassTypes.Filters) to High(ClassTypes.Filters) do
      begin
        if not (obj is ClassTypes.Filters[c]) then
           continue
      end;
    end;

    result := obj;
    exit;
  end;
  result := nil;
end;

function TSE2BaseTypeList.GetCount: integer;
begin
  result :=FList.Count;
end;

function TSE2BaseTypeList.GetItem(i: integer): TSE2BaseType;
begin
  if (i < 0) or (i >= FList.Count) then
     result := nil
  else
     result := FList[i];
end;

function TSE2BaseTypeList.IndexOf(obj: TSE2BaseType): integer;
begin
  for result:=FList.Count-1 downto 0 do
    if FList[result] = obj then
      exit;
  result := -1;
end;

procedure TSE2BaseTypeList.SetItem(i: integer; const obj: TSE2BaseType);
begin
  if (i >= 0) and (i < FList.Count) then
     FList[i] := obj;
end;

procedure TSE2BaseTypeList.SetEntryID(Entry: TSE2BaseType);
var i: integer;

  function HasName(const value: string): boolean;
  var i: integer;
  begin
    result := True;
    for i:=FList.Count-1 downto 0 do
      if TSE2BaseType(FList[i]).GetUniqueName = value then
        exit;
    result := False;
  end;

  function MakeStr(i: integer): string;
  begin
    result := Entry.GetUniqueName(i);
  end;

begin
  i := 0;

  if IndexOf(Entry) > -1 then
     exit;

  while HasName(MakeStr(i)) do
    i := i + 1;
  Entry.ID := i;
end;

{ TSE2Compatibility }
(*
constructor TSE2Compatibility.Create(Entry: TSE2BaseType;
  Allowed: TSE2TokenTypes; Recursive: boolean);
begin
  inherited Create;
  FEntry     := Entry;
  FAllowed   := Allowed;
  FRecursive := Recursive;
end;

destructor TSE2Compatibility.Destroy;
begin
  inherited;
end;        *)

{ TSE2CompatibleList }

(*
procedure TSE2CompatibleList.AddCompatbile(const obj: TSE2BaseType;
  Allowed: TSE2TokenTypes; Recursive: boolean);
begin
  if IndexOf(obj) > -1 then
     exit;

  FList.Add(TSE2Compatibility.Create(obj, Allowed, Recursive));
end;

procedure TSE2CompatibleList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Delete(i);
end;

constructor TSE2CompatibleList.Create;
begin
  inherited;
  FList := TList.Create;
end;

function TSE2CompatibleList.Delete(index: integer): boolean;
var p: TSE2Compatibility;
begin
  if (index < 0) or (index >= FList.Count) then
     result := False
  else
  begin
    p := FList[index];
    FList.Delete(index);

    p.Free;
    result := True;
  end;
end;

function TSE2CompatibleList.DeleteCompatible(
  const obj: TSE2BaseType): boolean;
begin
  result := Delete(IndexOf(obj));
end;

destructor TSE2CompatibleList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2CompatibleList.IndexOf(const obj: TSE2BaseType): integer;
begin
  for result:=FList.Count-1 downto 0 do
    if TSE2Compatibility(FList[result]).Entry = obj then
      exit;
  result := -1;
end;

function TSE2CompatibleList.IsCompatible(obj: TSE2BaseType;
  Action: TSE2TokenType): boolean;
var p: TSE2Compatibility;
    i: integer;
begin
  result := False;
  try
    while (not result) and (obj <> nil) do
    begin
      i := IndexOf(obj);
      if i > -1 then
      begin
        p := FList[i];
        result := Action in p.Allowed;
        if not p.Recursive then
           exit;
      end;
      obj := obj.InheritFrom;
    end;
  finally

  end;
end;
*)
procedure TSE2BaseTypeList.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version  : byte;
    i, count : integer;
    obj      : TSE2BaseType;
begin
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Stream.Read(FOwnsObjs, SizeOf(FOwnsObjs));
        Stream.Read(FSetID, SizeOf(FSetID));

        Stream.Read(count, SizeOf(count));
        for i:=0 to count-1 do
        begin
          obj := uSE2UnitCache.GetClassByName(TSE2StreamHelper.ReadString(Stream)).Create;
          obj.LoadFromStream(Stream, Weaver);
          FList.Add(obj);
        end;
      end
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2BaseTypeList.SaveToStream(Stream: TStream);
var version : byte;
    i, count: integer;
    obj     : TSE2BaseType;
begin
  version := TSE2BaseTypeList_CurrentVersion;
  Stream.Write(version, SizeOf(Version));

  Stream.Write(FOwnsObjs, SizeOf(FOwnsObjs));
  Stream.Write(FSetID, SizeOf(FSetID));

  count := Self.Count;
  Stream.Write(count, SizeOf(count));
  for i:=0 to count-1 do
  begin
    obj := Items[i];
    obj.SaveToStream(Stream);
  end;                       
end;

{ TSE2Type }

procedure TSE2Type.LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Stream.Read(FType, SizeOf(FType));
        Stream.Read(FDataSize, SizeOf(FDataSize));
        Stream.Read(FStrict, SizeOf(FStrict));
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2Type.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := TSE2Type_CurrentVersion;
  Stream.Write(version, SizeOf(version));

  Stream.Write(FType, SizeOf(FType));
  Stream.Write(FDataSize, SizeOf(FDataSize));
  Stream.Write(FStrict, SizeOf(FStrict));
end;

{ TSE2Constant }

function TSE2Constant.GetAsFloat: double;
begin
  result := TSE2Converter.StrToFloat(FValue);
end;

function TSE2Constant.GetAsInteger: int64;
begin
  result := TSE2Converter.StrToInt(FValue);
end;

function TSE2Constant.IsFloat: boolean;
begin
  result := TSE2Converter.IsFloat(FValue);
end;

function TSE2Constant.IsInteger: boolean;
begin
  result := TSE2Converter.IsInteger(FValue);
end;

procedure TSE2Constant.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FType);
        TSE2StreamHelper.ReadString(Stream, FValue);
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2Constant.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := TSE2Constant_CurrentVersion;
  Stream.Write(version, SizeOf(Version));

  SaveWeaverData(Stream, FType);
  TSE2StreamHelper.WriteString(Stream, FValue);
end;

procedure TSE2Constant.SetAsFloat(value: double);
begin
  FValue := TSE2Converter.FloatToStr(value);
end;

procedure TSE2Constant.SetAsInteger(value: int64);
begin
  FValue := TSE2Converter.IntToStr(value);
end;

{ TSE2Class }

constructor TSE2Class.Create;
begin
  inherited;
  FMethods := TSE2BaseTypeList.Create;
  FRTTI    := TSE2ClassRTTI.Create;
end;

destructor TSE2Class.Destroy;
begin
  FMethods.Free;
  FRTTI.Free;
  inherited;
end;

function TSE2Class.GenLinkerName(i: Integer): String;
begin
  result := '[' + Self.Name + ']' +
            '[' + Self.AUnitName + ']';
end;

function TSE2Class.GetParentClass: TSE2Class;
begin
  result := TSE2Class(InheritFrom);
end;

procedure TSE2Class.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(Version) then
     exit;

  case version of
  1 :
      begin
        FMethods.LoadFromStream(Stream, Weaver);
        FRTTI.LoadFromStream(Stream, Weaver);
        STream.Read(FIsForwarded, SizeOf(FIsForwarded));
        Stream.Read(FClassSize, SizeOf(FClassSize));
        Stream.Read(FVariables, SizeOf(FVariables));
        Stream.Read(FDynMethods, SizeOf(FDynMethods));
        Stream.Read(FIsPartial, SizeOf(boolean));
        Stream.Read(FIsHelper, SizeOf(boolean));
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2Class.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(version));

  FMethods.SaveToStream(Stream);
  FRTTI.SaveToStream(Stream);
  Stream.Write(FIsForwarded, SizeOf(FIsForwarded));
  Stream.Write(FClassSize, SizeOf(FClassSize));
  Stream.Write(FVariables, SizeOf(FVariables));
  Stream.Write(FDynMethods, SizeOf(FDynMethods));
  Stream.Write(FIsPartial, SizeOf(boolean));
  Stream.Write(FIsHelper, SizeOf(boolean));
end;

procedure TSE2Class.SetParentClass(value: TSE2Class);
begin
  InheritFrom := value;
end;

{ TSE2Method }

constructor TSE2Method.Create;
begin
  inherited;
  FParams  := TSE2BaseTypeList.Create;
  FOpCodes := TSE2LinkOpCodeList.Create;
  FCallConvention := callRegister;
  FVariables := TSE2BaseTypeList.Create;
  FTypes     := TSE2BaseTypeList.Create;

  FLists     := TSE2MethodLists.Create;
  FUsedMethods := TSE2UsedByList.Create;
end;

destructor TSE2Method.Destroy;
begin
  FLists.Free;

  FVariables.Free;
  FTypes.Free;
  FReturnValue.Free;
  FOpCodes.Free;
  FParams.Free;
  FUsedMethods.Free;
  inherited;
end;

function TSE2Method.GetHasSelfParam: boolean;
begin
  result := (Parent <> nil) or (FAddHasSelfParam);
  if result then
  begin
    if IsStatic then
    begin
      if not IsExternal then
         result := True;
    end;
  end;
end;

procedure TSE2Method.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version))  < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        FParams.LoadFromStream(Stream, Weaver);
        Stream.Read(FIsAbstract     , SizeOf(boolean));
        Stream.Read(FIsForwarded    , SizeOf(boolean));    // forward or interface declaration
        Stream.Read(FIsExternal     , SizeOf(boolean));    // External method
        Stream.Read(FIsOverload     , SizeOf(boolean));    // overloaded
        Stream.Read(FIsVirtual      , SizeOf(boolean));
        Stream.Read(FIsOverride     , SizeOf(boolean));
        Stream.Read(FIsStatic       , SizeOf(boolean));
        Stream.Read(FIsExport       , SizeOf(boolean));
        Stream.Read(FIsMethodType   , SizeOf(boolean));
        Stream.Read(FAddHasSelfParam, SizeOf(boolean));

        Stream.Read(FCallConvention , SizeOf(TSE2CallType));
        Stream.Read(FDynamicIndex   , SizeOf(FDynamicIndex));


        if TSE2StreamHelper.ReadString(Stream) <> '' then
        begin
          if FReturnValue = nil then
             FReturnValue := TSE2Variable.Create;
          FReturnValue.LoadFromStream(Stream, Weaver);
        end;
        Stream.Read(FStackSize      , SizeOf(integer));

        Stream.Read(FMethodType     , SizeOf(TSE2MethodType));
        FLists.LoadFromStream(Stream);

        FVariables.LoadFromStream(Stream, Weaver);
        FTypes.LoadFromStream(Stream, Weaver);

        FOpCodes.LoadFromStream(Stream);
        FUsedMethods.LoadFromStream(Stream, Weaver);
      end
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2Method.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(version));

  FParams.SaveToStream(Stream);
  Stream.Write(FIsAbstract     , SizeOf(boolean));
  Stream.Write(FIsForwarded    , SizeOf(boolean));    // forward or interface declaration
  Stream.Write(FIsExternal     , SizeOf(boolean));    // External method
  Stream.Write(FIsOverload     , SizeOf(boolean));    // overloaded
  Stream.Write(FIsVirtual      , SizeOf(boolean));
  Stream.Write(FIsOverride     , SizeOf(boolean));
  Stream.Write(FIsStatic       , SizeOf(boolean));
  Stream.Write(FIsExport       , SizeOf(boolean));
  Stream.Write(FIsMethodType   , SizeOf(boolean));
  Stream.Write(FAddHasSelfParam, SizeOf(boolean));
  Stream.Write(FCallConvention , SizeOf(TSE2CallType));
  Stream.Write(FDynamicIndex   , SizeOf(FDynamicIndex));

  if ReturnValue <> nil then
     ReturnValue.SaveToStream(Stream)
  else
     TSE2StreamHelper.WriteString(Stream, '');
  Stream.Write(FStackSize      , SizeOf(integer));

  Stream.Write(FMethodType     , SizeOf(TSE2MethodType));
  FLists.SaveToStream(Stream);

  FVariables.SaveToStream(Stream);
  FTypes.SaveToStream(Stream);

  FOpCodes.SaveToStream(Stream);
  FUsedMethods.SaveToStream(Stream);
end;

{ TSE2Record }

constructor TSE2Record.Create;
begin
  inherited;
  FVariables := TSE2BaseTypeList.Create;
  FRTTI      := TSE2ClassRTTI.Create;
  AType      := btRecord;
  FDataSize  := 4;
end;

destructor TSE2Record.Destroy;
begin
  FVariables.Free;
  FRTTI.Free;
  inherited;
end;

function TSE2Record.GenLinkerName(i: Integer): String;
begin
  result := '[' + Self.Name + ']' +
            '[' + Self.AUnitName + ']';
end;

procedure TSE2Record.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: Byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        FVariables.LoadFromStream(Stream, Weaver);
        FRTTI.LoadFromStream(Stream, Weaver);

        Stream.Read(FRecordSize, SizeOf(integer));
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2Record.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(version));

  FVariables.SaveToStream(Stream);
  FRTTI.SaveToStream(Stream);

  Stream.Write(FRecordSize, SizeOf(integer));
end;

{ TSE2Unit }

constructor TSE2Unit.Create;
begin
  inherited;
  FTypeList := TSE2BaseTypeList.Create;
  FElemList := TSE2BaseTypeList.Create;
  FStrings  := TSE2LinkStringList.Create;
  FInitializations := TSE2BaseTypeList.Create;
  FFinalizations   := TSE2BaseTypeList.Create;
  FRequiredUnits   := TStringList.Create;
end;

destructor TSE2Unit.Destroy;
begin
  FRequiredUnits.Free;
  FStrings.Free;
  FMain.Free;
  FInitializations.Free;
  FFinalizations.Free;
  FTypeList.Free;
  FElemList.Free;
  inherited;
end;

procedure TSE2Unit.LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver);
var version: byte;
    i, count : integer;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Stream.Read(count, SizeOf(count));
        for i:=0 to count-1 do
          FRequiredUnits.Add(TSE2StreamHelper.ReadString(Stream));


        FTypeList.LoadFromStream(Stream, Weaver);
        FElemList.LoadFromStream(Stream, Weaver);
        FStrings.LoadFromStream(Stream, Weaver);
        FInitializations.LoadFromStream(Stream, Weaver);
        FFinalizations.LoadFromStream(Stream, Weaver);
        if TSE2StreamHelper.ReadString(Stream) <> '' then
        begin
          if FMain = nil then
             FMain := TSE2Method.Create;
          FMain.LoadFromStream(Stream, Weaver);
        end;
        Stream.Read(FIsProgram, SizeOf(FIsProgram));
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2Unit.SaveToStream(Stream: TStream);
var version: byte;
    i, count: integer;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(version));

  count := FRequiredUnits.Count;
  Stream.Write(count, SizeOf(count));
  for i:=0 to FRequiredUnits.Count-1 do
  begin
    TSE2StreamHelper.WriteString(Stream, FRequiredUnits[i]);
  end;
  FTypeList.SaveToStream(Stream);
  FElemList.SaveToStream(Stream);
  FStrings.SaveToStream(Stream);
  FInitializations.SaveToStream(Stream);
  FFinalizations.SaveToStream(Stream);
  if FMain = nil then
     TSE2StreamHelper.WriteString(Stream, '')
  else
     FMain.SaveToStream(Stream);
  Stream.Write(FIsProgram, SizeOf(FIsProgram));
end;

procedure TSE2Unit.SetMain(value: TSE2Method);
begin
  FreeAndNil(FMain);
  FMain := value;
end;

{ TSE2BaseTypeFilter }

constructor TSE2BaseTypeFilter.Create(Filter: array of TSE2BaseTypeClass);
var i: integer;
begin
  inherited Create;
  SetLength(Filters, length(Filter));
  for i:=Low(Filter) to High(Filter) do
    Filters[i] := Filter[i];
end;

function TSE2BaseTypeFilter.Duplicate: TSE2BaseTypeFilter;
begin
  result := nil;
  if Self <> nil then
     result := TSE2BaseTypeFilter.Create(Self.Filters);
end;

function TSE2BaseTypeFilter.TypeIsIn(aType: TSE2BaseTypeClass): boolean;
var i: integer;
begin            
  result := True;
  if Self <> nil then
  begin
    for i:=Low(Filters) to High(Filters) do
      if Filters[i] = aType then
        exit;
    result := False;
  end;
end;

{ TSE2LinkStringList }

function TSE2LinkStringList.Add(Owner: TSE2Unit; const value: string): TSE2LinkString;
var Item  : TSE2LinkString;
    index : integer;
begin
  index := IndexOf(value);
  if index > -1 then
  begin
    result := Items[index];
    exit;
  end;

  Item   := TSE2LinkString.Create(Owner, value, GetNextID);
  result := Item;
  FList.Add(Item);
end;

procedure TSE2LinkStringList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Delete(i);
  ResetNextID;
end;

constructor TSE2LinkStringList.Create;
begin
  inherited;
  FList := TList.Create;
  ResetNextID;
end;

function TSE2LinkStringList.Delete(index: integer): boolean;
var Item: TSE2LinkString;
begin
  if (index < 0) or (index >= FList.Count) then
     result := False
  else
  begin
    Item := FList[index];
    FList.Delete(index);
    Item.Free;
    result := True;
  end;
end;

destructor TSE2LinkStringList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2LinkStringList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2LinkStringList.GetItem(index: integer): TSE2LinkString;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := FList[index];
end;

function TSE2LinkStringList.GetNextID: integer;
begin
  result := FNextID;
  inc(FNextID);
end;

function TSE2LinkStringList.IndexOf(const ID: integer): integer;
begin
  for result:=FList.Count-1 downto 0 do
    if TSE2LinkString(FList[result]).ID = ID then
      exit;
  result := -1;
end;

function TSE2LinkStringList.IndexOf(const value: string): integer;
begin
  for result:=FList.Count-1 downto 0 do
    if AnsiSameStr(TSE2LinkString(FList[result]).Value, value) then
      exit;
  result := -1;
end;

procedure TSE2LinkStringList.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var i, count: integer;
    version : byte;
    obj     : TSE2LinkString;
begin
  if Stream.Read(version, SizeOf(Version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Stream.Read(count, SizeOf(count));
        for i:=0 to count-1 do
        begin
          TSE2StreamHelper.ReadString(Stream);
          obj := TSE2LinkString.Create(nil, '', 0);
          obj.LoadFromStream(Stream, Weaver);
          FList.Add(obj)
        end;
        Stream.Read(FNextID, SizeOf(FNextID));
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2LinkStringList.ResetNextID;
begin
  FNextID := 1;
end;

procedure TSE2LinkStringList.SaveToStream(Stream: TStream);
var i, count: integer;
    version : byte;
    obj     : TSE2LinkString;
begin
  version := 1;
  Stream.Write(version, SizeOf(Version));

  count := FList.Count;
  Stream.Write(count, SizeOf(count));
  for i:=0 to count-1 do
  begin
    obj := FList[i];
    obj.SaveToStream(Stream);
  end;
  Stream.Write(FNextID, SizeOf(FNextID));
end;

{ TSE2LinkString }

constructor TSE2LinkString.Create(const Owner: TSE2Unit; const Value: string; ID: integer);
begin
  inherited Create;
  FOwner := Owner;
  FValue := value;
  FID    := ID;
end;

destructor TSE2LinkString.Destroy;
begin
  inherited
end;

function TSE2LinkString.GenLinkerName(i: integer = -1): string;
begin
  if i = -1 then
     i := Self.ID;
  result := '[string]' +
            '['+Self.Owner.Name+']' +
            '['+IntToStr(i)+']';
end;

procedure TSE2LinkString.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version))  < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        TSE2StreamHelper.ReadString(Stream, FValue);
        Stream.Read(FID, SizeOf(FID));
        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FOwner);
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2LinkString.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(version));
                           
  TSE2StreamHelper.WriteString(Stream, FValue);
  Stream.Write(FID, SizeOf(FID));
  SaveWeaverData(Stream, FOwner);
end;

{ TSE2IntegerList }

procedure TSE2IntegerList.Add(value: integer);
var p: PInteger;
begin
  New(p);
  p^ := value;
  FList.Add(p);
end;

procedure TSE2IntegerList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Delete(i);
end;

constructor TSE2IntegerList.Create;
begin
  inherited;
  FList := TList.Create;
end;

function TSE2IntegerList.Delete(index: integer): boolean;
var p: PInteger;
begin
  if (index < 0) or (index >= FList.Count) then
     result := False
  else
  begin
    p := FList[index];
    FList.Delete(index);
    result := True;
    Dispose(p);
  end;
end;

destructor TSE2IntegerList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2IntegerList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2IntegerList.GetItem(index: integer): integer;
begin
  if (index < 0) or (index >= FList.Count) then
     result := 0
  else
     result := PInteger(FList[index])^;

end;

function TSE2IntegerList.IndexOf(value: integer): integer;
begin
  for result := FList.Count - 1 downto 0 do
    if PInteger(FList[result])^ = value then
      exit;
  result := -1;
end;

procedure TSE2IntegerList.LoadFromStream(Stream: TStream);
var i, count: integer;
    p       : PInteger;
    version : byte;
begin
  Clear;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;


  case version of
  1 :
      begin
        Stream.Read(count, SizeOf(count));
        FList.Count := count;
        for i:=0 to count-1 do
        begin
          New(p);
          Stream.Read(p^, SizeOf(integer));
          FList[i] := p;
        end;
      end;
  else
      raise ESE2InvalidDataStream.Create('Incompatible stream version');
  end;
end;

procedure TSE2IntegerList.SaveToStream(Stream: TStream);
var version : byte;
    i, count: integer;
    c       : integer;
begin
  version := TSE2IntegerList_CurrentVersion;
  Stream.Write(version, SizeOf(version));

  count := SElf.Count;
  Stream.Write(count, SizeOf(count));
  for i:=0 to count-1 do
  begin
    c := Items[i];
    Stream.Write(c, SizeOf(integer));
  end;
end;

procedure TSE2IntegerList.SetItem(index, value: integer);
begin
  if (index >= 0) and (index < FList.Count) then
     PInteger(FList[index])^ := value;
end;

{ TSE2MethodLists }

constructor TSE2MethodLists.Create;
begin
  inherited;
  FContinueList  := TSE2IntegerList.Create;
  FBreakList     := TSE2IntegerList.Create;
  FExitList      := TSE2IntegerList.Create;
  FExitTableList := TSE2IntegerList.Create;
end;

destructor TSE2MethodLists.Destroy;
begin
  FContinueList.Free;
  FBreakList.Free;
  FExitList.Free;
  FExitTableList.Free;
  inherited;
end;

procedure TSE2MethodLists.LoadFromStream(Stream: TStream);
var version: byte;
begin
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        FContinueList.LoadFromStream(Stream);
        FBreakList.LoadFromStream(Stream);
        FExitList.LoadFromStream(Stream);
        FExitTableList.LoadFromStream(Stream);
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2MethodLists.SaveToStream(Stream: TStream);
var version: byte;
begin
  version := 1;
  Stream.Write(version, SizeOf(version));
             
  FContinueList.SaveToStream(Stream);
  FBreakList.SaveToStream(Stream);
  FExitList.SaveToStream(Stream);
  FExitTableList.SaveToStream(Stream);
end;

{ TSE2ParamExpression }

constructor TSE2ParamExpression.Create;
begin
  inherited;

end;

constructor TSE2ParamExpression.Create(AType: TSE2Type;
  Variable: TSE2Variable; CodePos: integer);
begin
  inherited Create;
  FType     := AType;
  FVariable := Variable;
  FCodePos  := CodePos;
end;

procedure TSE2ParamExpression.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Stream.Read(FCodePos, SizeOf(FCodePos));
        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FType);
        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FVariable);
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2ParamExpression.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(version));

  Stream.Write(FCodePos, SizeOf(FCodePos));
  SaveWeaverData(Stream, FType);
  SaveWeaverData(Stream, FVariable);
end;

{ TSE2Array }

procedure TSE2Array.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FArrayType);
        Stream.Read(FArrayCount, SizeOf(FArrayCount));
        Stream.Read(FStartIndex, SizeOf(FStartIndex));
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2Array.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(version));

  SaveWeaverData(Stream, FArrayType);   
  Stream.Write(FArrayCount, SizeOf(FArrayCount));
  Stream.Write(FStartIndex, SizeOf(FStartIndex));
end;

{ TSE2CodeElement }

procedure TSE2CodeElement.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Stream.Read(FCodePos, SizeOf(FCodePos));
        Stream.Read(FDebugPos, SizeOf(FDebugPos));
        STream.Read(FUsed, SizeOf(FUsed));
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2CodeElement.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, Sizeof(version));
        
  Stream.Write(FCodePos, SizeOf(FCodePos));
  Stream.Write(FDebugPos, SizeOf(FDebugPos));
  STream.Write(FUsed, SizeOf(FUsed));
end;

{ TSE2Variable }

procedure TSE2Variable.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version))  < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FType);
        Stream.Read(FIsStatic, SizeOF(FIsStatic));
        Stream.Read(FIsPublic, SizeOf(FIsPublic));
        Stream.Read(FIsExternal, SizeOf(FIsExternal));
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2Variable.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(version));

  SaveWeaverData(Stream, FType);
  Stream.Write(FIsStatic, SizeOF(FIsStatic));
  Stream.Write(FIsPublic, SizeOf(FIsPublic));
  Stream.Write(FIsExternal, SizeOf(FIsExternal));
end;

{ TSE2Parameter }

procedure TSE2Parameter.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Stream.Read(FParameterType, SizeOf(FParameterType));
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2Parameter.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(version));
                              
  Stream.Write(FParameterType, SizeOf(FParameterType));
end;

{ TSE2Property }

procedure TSE2Property.AddParam(const Name: string; const aType: TSE2Type);
var v: TSE2Variable;
begin
  v := TSE2Variable.Create;
  v.Name := Name;
  v.AType := aType;
  FParams.Add(v);
end;

constructor TSE2Property.Create;
begin
  inherited;
  FParams := TSE2BaseTypeList.Create;
  FParams.OwnsObjs := True;
end;

destructor TSE2Property.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TSE2Property.FillEmptyParams(newType: TSE2Type);
var i: integer;
begin
  for i:=0 to FParams.Count-1 do
    if TSE2Variable(FParams[i]).AType = nil then
      TSE2Variable(FParams[i]).AType := newType;
end;

procedure TSE2Property.LoadFromStream(Stream: TStream; Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Stream.Read(FIsStatic, SizeOf(FIsStatic));
        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FType);
        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FGetter);
        Weaver.Add(TSE2StreamHelper.ReadString(Stream), @FSetter);

        FParams.LoadFromStream(Stream, Weaver);
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2Property.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(Version));
                               
  Stream.Write(FIsStatic, SizeOf(FIsStatic));
  SaveWeaverData(Stream, FType);     
  SaveWeaverData(Stream, FGetter);
  SaveWeaverData(Stream, FSetter);

  FParams.SaveToStream(Stream);
end;

{ TSE2ClassRTTI }

procedure TSE2ClassRTTI.Add(aType: TSE2TypeIdent; Offset, Size: integer);
var p: PSE2ClassRTTIEntry;
begin
  New(p);
  p^.Offset := Offset;
  p^.Size   := Size;
  p^.aType  := aType;
  FList.Add(p);
end;

procedure TSE2ClassRTTI.Add(Item: PSE2ClassRTTIEntry);
begin
  if FList.IndexOf(Item) < 0 then
     FList.Add(Item);
end;

procedure TSE2ClassRTTI.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Dispose(PSE2ClassRTTIEntry(Flist[i]));
  FList.Clear;
end;

constructor TSE2ClassRTTI.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TSE2ClassRTTI.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2ClassRTTI.Duplicate(index,
  deltaOffset: integer): PSE2ClassRTTIEntry;
var source: PSE2ClassRTTIEntry;
begin
  source := FList[index];
  New(result);
  result^.Offset := source^.Offset + deltaOffset;
  result^.Size   := source^.Size;
  result^.aType  := source^.aType;
end;

function TSE2ClassRTTI.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2ClassRTTI.GetItem(index: integer): PSE2ClassRTTIEntry;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := Flist[index];
end;

procedure TSE2ClassRTTI.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version  : byte;
    i, count : integer;
    p        : PSE2ClassRTTIEntry;
begin
  if Stream.Read(version, SizeOf(version)) < SizeOf(Version) then
     exit;

  Clear;
  case version of
  1 :
      begin     
        Stream.Read(count, SizeOf(count));
        Flist.Count := count;
        for i:=0 to count-1 do
        begin
          New(p);
          Stream.Read(p^.Offset, SizeOf(integer));
          Stream.Read(p^.Size, SizeOf(integer));
          Stream.Read(p^.aType, SizeOf(TSE2TypeIdent));
          Flist[i] := p;
        end;
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2ClassRTTI.SaveToStream(Stream: TStream);
var version: byte;
    i, count : integer;
    p        : PSE2ClassRTTIEntry;
begin
  version := 1;
  Stream.Write(version, SizeOf(version));

  count := FList.Count;
  Stream.Write(count, SizeOf(count));
  for i:=0 to count-1 do
  begin
    p := Flist[i];
    Stream.Write(p^.Offset, SizeOf(integer));
    Stream.Write(p^.Size, SizeOf(integer));
    Stream.Write(p^.aType, SizeOf(TSE2TypeIdent));
  end;
end;

{ TSE2UsedByList }

procedure TSE2UsedByList.Add(Item: TSE2BaseType);
var p: PPointer;
begin
  if IndexOf(Item) < 0 then
  begin
    New(p);
    p^ := Item;
    FList.Add(p);
  end;
end;

procedure TSE2UsedByList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Dispose(FList[i]);
  FList.Clear;
end;

constructor TSE2UsedByList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TSE2UsedByList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2UsedByList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2UsedByList.GetItem(index: integer): TSE2BaseType;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := PPointer(FList[index])^;
end;

function TSE2UsedByList.IndexOf(Item: TSE2BaseType): integer;
begin
  for result := FList.Count-1 downto 0 do
    if PPointer(FList[result])^ = Item then
      exit;

  result := -1;
end;

procedure TSE2UsedByList.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version  : byte;
    i, count : integer;
    p        : PPointer;
begin
  Clear;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        Stream.Read(count, SizeOf(count));
        FList.Count := count;

        for i:=0 to count-1 do
        begin
          New(p);
          FList[i] := p;
          Weaver.Add(TSE2StreamHelper.ReadString(Stream), p);
        end;
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2UsedByList.SaveToStream(Stream: TStream);
var version  : byte;
    i, count : integer;
begin
  version := 1;
  Stream.Write(version, SizeOf(version));

  count := FList.Count;
  Stream.Write(count, SizeOf(count));

  for i:=0 to count-1 do
  begin
    TSE2StreamHelper.WriteString(Stream, Items[i].GetUniqueName());
  end;
end;

{ TSE2MethodType }

constructor TSE2MethodVariable.Create;
begin
  inherited;
  FMethod  := TSE2Method.Create;
  AType    := btProcPtr;
  DataSize := SizeOf(Pointer) * 2;
end;

destructor TSE2MethodVariable.Destroy;
begin
  FMethod.Free;
  inherited;
end;

procedure TSE2MethodVariable.LoadFromStream(Stream: TStream;
  Weaver: TSE2NameWeaver);
var version: byte;
begin
  inherited;
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;

  case version of
  1 :
      begin
        // read class name - will not be readed by the
        TSE2StreamHelper.ReadString(Stream);

        FMethod.LoadFromStream(Stream, Weaver);
      end;
  else RaiseIncompatibleStream;
  end;
end;

procedure TSE2MethodVariable.SaveToStream(Stream: TStream);
var version: byte;
begin
  inherited;
  version := 1;
  Stream.Write(version, SizeOf(version));

  FMethod.SaveToStream(Stream);
end;

procedure TSE2MethodVariable.SetMethod(value: TSE2Method);
begin
  if value <> nil then
    if FMethod <> value then
    begin
      FreeAndNil(FMethod);
      FMethod := value;
    end;
end;

end.
