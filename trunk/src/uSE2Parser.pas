unit uSE2Parser;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2BaseTypes, uSE2Types, uSE2Consts, uSE2Tokenizer, uSE2OpCode,
  uSE2Errors;

type
  TSE2UnitRequest = procedure(Sender: TObject; const UnitName: string; var UnitData: TSE2Unit) of object;
  TSE2ErrorEvent  = procedure(Sender: TObject; ErrorType: TSE2ErrorType; ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: integer; UserData: TObject) of object;
  
  TSE2CompileCallBack = procedure(Sender: TObject; CodePos, ParamIndex: integer; CurrentMethod, ParamMethod: TSE2Method;
                                  Parent: TSE2BaseType; TargetType: TSE2Type; TokenType: TSE2TokenType; StaticOnly: boolean) of object;


type
  TSE2Parser = class;

  TSE2ParseState = class
  public
    StackSize      : integer;
    IsInInterface  : boolean;
    IsInLoop       : boolean;
    LoopStackSize  : integer;
    IsExpression   : boolean;
    IsAtStatement  : boolean;
    Visibility     : TSE2Visibility;
    LastVariable   : TSE2Variable;
    LastProperty   : TSE2Property;
    LastMethod     : TSE2Method;
    RootOffsetObj  : TSE2Type;
    LastTargetVar  : TSE2Variable;
    LastSetVariable: TSE2Variable;
    CurrentOwner   : TSE2Type;
    IsStatic       : boolean;
    WasFunction    : boolean;
    NoStaticPointer: boolean;
    RecordsCreated : integer;
    ArraysCreated  : integer;
    IsInExceptBlock: boolean; 
    IsInFinallyBlock: boolean;
    IsInUsesBlock  : boolean;
    IsInTryBlock   : boolean;

    TargetType     : TSE2Type;
    Method         : TSE2Method;
    ParamMethod    : TSE2Method;
    ParamIndex     : integer;
    AParent        : TSE2BaseType;
    Parser         : TSE2Parser;

    constructor Create(Parent: TSE2Parser);

    procedure IncStack;
    procedure DecStack;
  end;

{$IFDEF SEII_FPC}
    {$HINTS OFF}
{$ENDIF}

  PSE2Type = ^TSE2Type;
  TSE2SetterEvent = procedure(Sender: TObject; State: TSE2ParseState; Method: TSE2Method; Target: TSE2Type; NewValueType: PSE2Type) of object;

  TSE2Parser = class(TSE2Object)
  private
    FOwnsUnit    : boolean;
    FUnit        : TSE2Unit;
    FTokenizer   : TSE2Tokenizer;
    FHasError    : boolean;
    FUnitList    : TSE2BaseTypeList;
    FExpectedName: string;

    // Code Completion
    FCallBackPos : integer;
    FParserState : TSE2ParseState;

    // Events
    FBeforeParse : TSE2UnitRequest;
    FUnitRequest : TSE2UnitRequest;
    FErrorEvent  : TSE2ErrorEvent;
    FCompileCall : TSE2CompileCallBack;
  protected
    procedure SetUnit(AUnit: TSE2Unit);
    function  DoNeedUnit(const UnitName: string): TSE2Unit;
    procedure RaiseError(ErrorType: TSE2ErrorType; ErrorText: string; ErrorPos: integer = -1; ErrorLine: integer = -1);
    function  CompileUnitName: string;

    function  GenIncompatibleExpression(Current, Target: TSE2Type; Operation: TSE2TokenType = sesNone): string;
    procedure ArrayMethodCall(Sender: TObject; CallerMethod, CalledMethod: TSE2Method; CallOwner: TSE2Type; StackElem: TSE2Variable);
    procedure LinkArrayMethodEvents;

    procedure ShowUnusedVariables(OnlyPrivate: boolean);
    procedure ParseUnit(State: TSE2ParseState);
    procedure ParseProgram(State: TSE2ParseState);
    function  TypeIsNoRefType(aType: TSE2Type): boolean;

    procedure VariableDeclaration(State: TSE2ParseState; Method: TSE2Method);
    procedure TypeDeclaration(State: TSE2ParseState; Method: TSE2Method);
    procedure ConstDeclaration(State: TSE2ParseState; Method: TSE2Method);

    procedure ChangeArrayLenAndPopToVar(State: TSE2ParseState; Method: TSE2Method; ArrayType: TSE2Type; Variable: TSE2Variable);
    procedure ProcessDeprecatedExpression(State: TSE2ParseState; Elem: TSE2BaseType);
    procedure MethodTypeDeclaration(State: TSE2ParseState; MethodName: string);
    procedure TypeTypeDeclaration(State: TSE2ParseState; Method: TSE2Method; TypeName: string);
    procedure SetDeclaration(State: TSE2ParseState; Method: TSE2Method; TypeName: string);
    procedure ClassDeclaration(State: TSE2ParseState; ClassName: string);
    procedure RecordDeclaration(State: TSE2ParseState; RecordName: string);
    procedure InterfaceDeclaration(State: TSE2ParseState; InterfaceName: string);
    procedure ArrayTypeDeclaration(State: TSE2ParseState; ArrayName: string);
    procedure PushVarToStack(State: TSE2ParseState; Method: TSE2Method; Variable: TSE2Variable; MoveData: boolean);
    procedure PopStackToVar(State: TSE2ParseState; Method: TSE2Method; Variable: TSE2Variable; StackValue: TSE2Type = nil);

    procedure IncreaseStackPosition(State: TSE2ParseState; OpCode: PSE2OpDefault; Offset, MinStackDist: integer);
    procedure IncreaseMethodStackPositions(State: TSE2ParseState; Method: TSE2Method; Start, Stop, Offset, MinStackDist: integer);
    function  GetOverwrittenMethod(Method: TSE2Method): TSE2Method;
    function  MethodCall(State: TSE2ParseState; Method: TSE2Method; CallMethod: TSE2Method;
                         UseBrackets: boolean = False; LastParamSetter: TSE2SetterEvent = nil;
                         AllowDynamic: boolean = True; ParentType: TSE2BaseType = nil;
                         NoMagicMemory: boolean = False;
                         AlternativeMethod: TSE2Method = nil): TSE2Type;
    function  Expression(State: TSE2ParseState; Method: TSE2Method; TargetType: TSE2Type): TSE2Type;
    function  FindMatchingMethod(State: TSE2ParseState; Method: TSE2Method; MethodList, ParamExpression: TSE2BaseTypeList; IgnoreFirst: boolean): TSE2Method;

    procedure GenerateComparison(Method: TSE2Method; t1, t2: TSE2Type; Operation: TSE2TokenType);

    procedure UsesDeclaration(State: TSE2ParseState);
    procedure MainBodyDeclaration(State: TSE2ParseState);
    procedure Statement(State: TSE2ParseState; Method: TSE2Method);
    procedure StatementSquence(State: TSE2ParseState; Method: TSE2Method);
    function  MethodDeclaration(State: TSE2ParseState; IsTypeDeclaration: boolean): TSE2Method;
    procedure PropertyDeclaration(State: TSE2ParseState);
    procedure MethodBodyDeclaration(State: TSE2ParseState; Method: TSE2Method);
    procedure PushMethodVariables(State: TSE2ParseState; Method: TSE2Method);
    procedure PopMethodVariables(State: TSE2ParseState; Method: TSE2Method);
    function  ExceptionCreation(State: TSE2ParseState; Method: TSE2Method): boolean;

    procedure IdentifierSetterEvent(Sender: TObject; State: TSE2ParseState; Method: TSE2Method; Target: TSE2Type; newValueType: PSE2Type);
    function  IdentifierExpression(State: TSE2ParseState; Method: TSE2Method; SetterCallBack: TSE2SetterEvent; AccessSet: boolean = False; UseVarMove: boolean = False): TSE2Type;

    procedure AtStatement(State: TSE2ParseState; Method: TSE2Method);
    procedure IfStatement(State: TSE2ParseState; Method: TSE2Method);
    procedure CaseStatement(State: TSE2ParseState; Method: TSE2Method);
    procedure OnExceptStatement(State: TSE2ParseState; Method: TSE2Method);
    procedure RepeatStatement(State: TSE2ParseState; Method: TSE2Method);
    procedure WhileStatement(State: TSE2ParseState; Method: TSE2Method);
    procedure ForStatement(State: TSE2ParseState; Method: TSE2Method);
    procedure LoopFlowStatement(State: TSE2ParseState; Method: TSE2Method);
    procedure TryStatement(State: TSE2ParseState; Method: TSE2Method);
    function  InheritedExpression(State: TSE2ParseState; Method: TSE2Method): TSE2Type;
    procedure IdentifierStatement(State: TSE2ParseState; Method: TSE2Method);

    procedure ValidateForwards(State: TSE2ParseState);

    function  Factor(State: TSE2ParseState; Method: TSE2Method; TargetType: TSE2Type): TSE2Type;
    function  Operators(State: TSE2ParseState; Method: TSE2Method; TargetType: TSE2Type): TSE2Type;
    function  Term(State: TSE2ParseState; Method: TSE2Method; TargetType: TSE2Type): TSE2Type;
    function  MathExpression(State: TSE2ParseState; Method: TSE2Method; TargetType: TSE2Type): TSE2Type;

    procedure GenCode(Method: TSE2Method; OpCodeData: TSE2LinkOpCode; Index: integer = -1; Replace: boolean = False);
    function  MakeCallExOptionMask(Method: TSE2Method): integer;
    function  FindIdentifier(Method: TSE2Method; const IdentName: string; Parent: TSE2BaseType; const UnitName: string = ''; const ClassTypes: TSE2BaseTypeFilter = nil; AcceptSetType: boolean = False): TSE2BaseType;
    function  TypeExpression(Method: TSE2Method): TSE2Type;
    function  ConstExpression(Method: TSE2Method): TSE2Constant;
    procedure CheckForDeprecated(aType: TSE2BaseType);

    function  ConvertIntToSingle(State: TSE2ParseState; Method: TSE2Method; aType1, aType2: TSE2Type): TSE2Type;
    function  ConvertVariableCode(State: TSE2ParseState; Method: TSE2Method; aCurrentType, aNewType: TSE2Type; DoAdd: boolean = True): TSE2LinkOpCode;

    function  GetBooleanType: TSE2Type;
    function  GetIntegerType(use64Bit: boolean = True): TSE2Type;
    function  GetDoubleType: TSE2Type;
    function  GetStringType: TSE2Type;
    function  GetPointerType: TSE2Type;
    function  GetExternalObjectType: TSE2Type;
    function  GetBaseArrayType: TSE2Type;
    function  GetScriptObjectType: TSE2Type;
    function  GetScriptExceptionType: TSE2Type;
    function  IsScriptExceptionType(aType: TSE2Type): boolean;
    procedure DeclareType(State: TSE2ParseState; const aType: TSE2BaseType; const Name: string; UnitName: string = '');
    procedure ReadNextToken;
    function  ExpectToken(Symbol : TSE2TokenTypes): boolean;

    class function ClassesAreCompatible(TargetType, CurrentType: TSE2Class): boolean;
  public
    constructor Create(Tokenizer: TSE2Tokenizer); reintroduce;
    destructor Destroy; override;
                                       
    procedure GetOverloadedMethods(State: TSE2ParseState; Method: TSE2Method; out Target: TSE2BaseTypeList);
    class function IsCompatible(TargetType, CurrentType: TSE2Type; Operation: TSE2TokenType = sesNone; StrictInt: boolean = False; AInstance: TSE2Parser = nil): boolean;
    class function HasHelper(TargetType, CurrentType: TSE2Type; UnitList: TSE2BaseTypeList; Operation: TSE2TokenType = sesNone): boolean;

    procedure RegisterCallBack(TargetPos: integer; Event: TSE2CompileCallBack);
    function Compile: boolean;

    property ExpectedName : string           read FExpectedName write FExpectedName;
    property OwnsUnit     : boolean          read FOwnsUnit     write FOwnsUnit;
    property AUnit        : TSE2Unit         read FUnit         write SetUnit;
    property UnitList     : TSE2BaseTypeList read FUnitList;
    property Tokenizer    : TSE2Tokenizer    read FTokenizer;
    property HasError     : boolean          read FHasError;
    property State        : TSE2ParseState   read FParserState;

    // Events
    property OnCompileCall     : TSE2CompileCallBack  read FCompileCall       write FCompileCall;
    property OnBeforeParse     : TSE2UnitRequest      read FBeforeParse       write FBeforeParse;
    property OnUnitRequest     : TSE2UnitRequest      read FUnitRequest       write FUnitRequest;
    property OnError           : TSE2ErrorEvent       read FErrorEvent        write FErrorEvent;
  end;
                {
var
  DEBUG_TimeMeasure : int64;      }

implementation

uses SysUtils, Math, Dialogs, DateUtils
               {
     , Windows  }
     ;
               {
var
  TTimeMeasure_BeginTime    : array[0..1024] of int64;
  TTimeMeasure_CurrentIndex : integer = 0;

type
  TTimeMeasure = class
  public
    class procedure Start;
    class procedure Stop;
  end;

class procedure TTimeMeasure.Start;
begin
  QueryPerformanceCounter(TTimeMeasure_BeginTime[TTimeMeasure_CurrentIndex]);
  TTimeMeasure_CurrentIndex := TTimeMeasure_CurrentIndex + 1;
end;

class procedure TTimeMeasure.Stop;
var t: int64;
begin
  TTimeMeasure_CurrentIndex := TTimeMeasure_CurrentIndex - 1;
  QueryPerformanceCounter(t);
  DEBUG_TimeMeasure := DEBUG_TimeMeasure + (t - TTimeMeasure_BeginTime[TTimeMeasure_CurrentIndex]);
end;
                     }

{ TSE2TryBlock }

type
  TSE2TryBlock = class
  private
    { General }
    FMethod           : TSE2Method;
    FState            : TSE2ParseState;
    FParser           : TSE2Parser;

    { Old Jump Lists }
    FoldExitList      : integer;
    FoldContinueList  : integer;
    FoldBreakList     : integer;

    { try block positions }
    FOpCodePos1       : integer;
    FOpCodePos2       : integer;

    FIsExceptBlock    : boolean;

    { Jump-List positions }
    FgeneralRETPos    : integer;
    FexitRETPos       : integer;
    FcontinueRETPos   : integer;
    FbreakRETPos      : integer;

    FexitEntry        : boolean;
    FcontinueEntry    : boolean;
    FbreakEntry       : boolean;

    FwasInTryFinally  : boolean;
    FwasInTryExcept   : boolean;
  protected
  public
    constructor Create(AMethod: TSE2Method; Parser: TSE2Parser; State: TSE2ParseState);

    procedure Step1;
    procedure Step2_TryContent;
    procedure Step3(IsFinally: boolean);
    procedure Step4_SafeContent;
    procedure Step5;
  end;

{ TSE2ParseState }

constructor TSE2ParseState.Create(Parent: TSE2Parser);
begin
  inherited Create;
  Self.Parser := Parent;
end;

procedure TSE2ParseState.DecStack;
begin
  StackSize := StackSize - 1;
  if (StackSize < 0) then
  begin
     StackSize := 0;
     Parser.RaiseError(petError, 'Internal error: stack size calulation failed');
  end;
end;

procedure TSE2ParseState.IncStack;
begin
  StackSize := StackSize + 1;
end;

{ TSE2Parser }  

function TSE2Parser.CompileUnitName: string;
var fName: string;
begin
  result := '';
  ExpectToken([sesIdentifier]);
  while Tokenizer.Token.AType in [sesIdentifier] do
  begin
    fName := fName + Tokenizer.Token.Value;
    ReadNextToken;
    if Tokenizer.Token.AType in [sesDot] then
    begin
      fName := fName + '.';
      ReadNextToken;
      ExpectToken([sesIdentifier]);
    end;
  end;



  if FExpectedName <> '' then
    if not StringIdentical(FExpectedName, fName) then
      RaiseError(petError, 'Expected "'+FExpectedName+'" as unit name but found "' + Tokenizer.Token.Value + '" instead');

  result := fName;

  (*
  while not (Tokenizer.Token.AType in [sesSemiColon]) do
  begin
    ExpectToken([sesIdentifier]);
    result := result + Tokenizer.Token.Value;
    ReadNextToken;
    if Tokenizer.Token.AType in [sesDot] then
    begin
       result := result + '.';
       ReadNextToken;
       ExpectToken([sesIdentifier]);
    end;
  end;       *)
  ExpectToken([sesSemiColon]);
  ReadNextToken;
end;

function TSE2Parser.Compile: boolean;
var Token  : TSE2TokenType;
    pUnit  : TSE2Unit;
begin
  try
    FHasError := False;
    FParserState := TSE2ParseState.Create(Self);
    try
      ReadNextToken;
      if FExpectedName <> '' then
         ExpectToken([sesUnit])
      else
         ExpectToken([sesUnit, sesProgram]);
      Token := Tokenizer.Token.AType;
      ReadNextToken;
      ExpectToken([sesIdentifier]);
      FUnit.Name      := CompileUnitName;
      FUnit.AUnitName := FUnit.Name;

      if Token = sesUnit then
        if Assigned(FBeforeParse) then
        begin
           FBeforeParse(Self, FUnit.Name, pUnit);
           if pUnit <> nil then
              AUnit := pUnit;
        end;

      case Token of
      sesUnit    : ParseUnit(FParserState);
      sesProgram : ParseProgram(FParserState);
      else         RaiseError(petError, C_ERR_UnkownFileType);
      end;
    finally
      FParserState.Free;
      result := not FHasError;
    end;
  except
    on E: EAbort do
       result := False;
    on E: Exception do
    begin
      FErrorEvent(Self, petError, FUnit.AUnitName, 'Exception ['+E.ClassName+']: ' + E.Message, Tokenizer.Reader.Position, Tokenizer.Reader.Line, Tokenizer.Reader.Data);
      raise;
    end;
  end;
end;

constructor TSE2Parser.Create(Tokenizer: TSE2Tokenizer);
begin
  inherited Create;
  FTokenizer := Tokenizer;

  FUnitList    := TSE2BaseTypeList.Create;
  FUnitList.OwnsObjs := False;

  FUnit        := TSE2Unit.Create;
  FOwnsUnit    := True;
  FCallBackPos := -1;
end;

procedure TSE2Parser.DeclareType(State: TSE2ParseState; const aType: TSE2BaseType; const Name: string;
  UnitName: string = '');
begin
  aType.Name      := Name;
  if UnitName = '' then
     UnitName := FUnit.Name;
  aType.AUnitName  := UnitName;
  aType.DeclLine  := Tokenizer.Reader.Line;
  aType.DeclPos   := Tokenizer.Reader.Position;
  aType.Visibility := State.Visibility;

  if Tokenizer.InlineDoc <> '' then
  begin
    aType.InlineDoc  := aType.InlineDoc + Tokenizer.InlineDoc;
    Tokenizer.InlineDoc := '';
  end;
end;

destructor TSE2Parser.Destroy;
begin
  FTokenizer.Free;
  FUnitList.Free;
  if FOwnsUnit then
     FUnit.Free;
  inherited;
end;

function TSE2Parser.DoNeedUnit(const UnitName: string): TSE2Unit;
begin
  result := nil;
  if not Assigned(FUnitRequest) then
     exit;

  FUnitRequest(Self, UnitName, Result);
  if result <> nil then
  begin
    FUnit.RequiredUnits.Add(UnitName);

    if result.RequiredUnits.IndexOf(Self.AUnit.Name) >= 0 then
       RaiseError(petError, 'Recursive unit dependency detected');
  end;
end;

function TSE2Parser.ExpectToken(Symbol: TSE2TokenTypes): boolean;
var Expected : string;
    i        : TSE2TokenType;
begin
  result := True;
  if not (FTokenizer.Token.AType in Symbol) then
  begin
    if Symbol <> [] then
    begin
      Expected := '';
      for i:=sesNone to sesLastToken do
        if i in Symbol then
          Expected := Expected + '"' + TSE2TokenName[i] + '" or ';

      Delete(Expected, length(Expected) - 3, 4);

      RaiseError(petError, Format(C_ERR_ExpectedButFound, [Expected, TSE2TokenName[Tokenizer.Token.AType]]));
    end else
    begin
      RaiseError(petError, Format(C_ERR_Unexpected, [TSE2TokenName[Tokenizer.Token.AType]]));
    end;
    result := False;
  end;
end;

function TSE2Parser.FindIdentifier(Method: TSE2Method; const IdentName: string; Parent: TSE2BaseType;
  const UnitName: string = ''; const ClassTypes: TSE2BaseTypeFilter = nil; AcceptSetType: boolean = False): TSE2BaseType;
var pUnit : TSE2Unit;
    i     : integer;

  function MethodIsParentOfClass(Method: TSE2Method; aClass: TSE2Class): boolean;
  begin
    result := False;
    if Method.Parent = aClass then
      result := True
    else
    if aClass.InheritFrom is TSE2Class then
       result := MethodIsParentOfClass(Method, TSE2Class(aClass.InheritFrom));
  end;

  function ElementIsChildTypeOf(Elem: TSE2Type; RootType: TSE2Type): boolean;
  begin
    result := False;
    while (Elem <> nil) and (RootType <> nil) and (not result) do
    begin
      result := Elem = RootType;
      if result then
         exit;

      Elem := TSE2Type(Elem.InheritFrom);
    end;
  end;

  function FindInClassHelpers(aUnit: TSE2Unit): TSE2BaseType;
  var i: integer;
  begin
    result := nil;
    if not (Parent is TSE2Type) then
       exit;

    for i:=aUnit.TypeList.Count-1 downto 0 do
      if aUnit.TypeList[i] is TSE2Class then
        if TSE2Class(aUnit.TypeList[i]).IsHelper then
          if ElementIsChildTypeOf(TSE2Type(Parent), TSE2Type(TSE2Class(aUnit.TypeList[i]).InheritFrom)) then
          begin
            //result := FindIdentifier(Method, IdentName, TSE2Class(aUnit.TypeList[i]), '', ClassTypes.Duplicate, AcceptSetType);
            result := aUnit.ElemList.FindItem(IdentName, '', aUnit.TypeList[i], nil, ClassTypes, CAllVisibilities, AcceptSetType);
            if result <> nil then
            begin
              exit;
            end;
          end;
  end;

  function FindUnitName(const Name: string): TSE2Unit;
  var i: integer;
      p: TSE2Unit;
  begin
    result := TSE2Unit(FUnitList.FindItem(Name));
    if result = nil then
    begin
      for i:=FUnitList.Count-1 downto 0 do
      begin
        p := TSE2Unit(FUnitList.Items[i]);
        if Pos(LowerCase(Name) + '.', LowerCase(p.Name)) = 1 then
        begin
          result := p;
          exit;
        end;
      end;
    end;
  end;

  function IdentifierInUnit(aUnit: TSE2Unit): TSE2BaseType;
  var vis        : TSE2Visibilities;
      bCanBeUnit : boolean;
      p          : TSE2BaseType;
      bCanSearch : boolean;
  begin
    if aUnit = FUnit then
       vis := CAllVisibilities
    else
       vis := CPublicVisibilities;

    result := nil;
    if Parent is TSE2Class then
    begin
      if Method <> nil then
      begin
        if (aUnit <> FUnit) then
          if Method.Parent = Parent then
             vis := [visProtected, visPublic];
      end else
      begin
        if (aUnit <> FUnit) and (State.CurrentOwner is TSE2Class) and (Parent <> nil) then
          if TSE2Class(State.CurrentOwner).IsTypeOf(Parent) or (State.CurrentOwner = Parent) then
             vis := [visProtected, visPublic];
      end;

      p := Parent;
      while p <> nil do
      begin
        result := aUnit.ElemList.FindItem(IdentName, '', p, nil, ClassTypes, vis, AcceptSetType);
        if result = nil then
           result := aUnit.TypeList.FindItem(IdentName, '', p, nil, ClassTypes, vis, AcceptSetType);

        if result <> nil then
           break
        else
           p := p.InheritFrom;
      end;
    end else
    if Parent is TSE2Record then
    begin
      if Method <> nil then
        if aUnit <> FUnit then
          if Method.Parent = Parent then
            vis := [visProtected, visPublic];

      p := Parent;
      while p <> nil do
      begin
        result := aUnit.ElemList.FindItem(IdentName, '', p, nil, ClassTypes, vis, AcceptSetType);
        if result = nil then
           result := aUnit.TypeList.FindItem(IdentName, '', p, nil, ClassTypes, vis, AcceptSetType);

        if result <> nil then
           break
        else
           p := p.InheritFrom;
      end;
    end else
    if Parent is TSE2Array then
    begin
      if Method <> nil then
        if aUnit <> FUnit then
          if Method.Parent = Parent then
            vis := [visProtected, visPublic];

      p := Parent;
      while p <> nil do
      begin
        result := aUnit.ElemList.FindItem(IdentName, '', p, nil, ClassTypes, vis, AcceptSetType);
        if result = nil then
           result := aUnit.TypeList.FindItem(IdentName, '', p, nil, ClassTypes, vis, AcceptSetType);

        if result <> nil then
           break
        else
           p := p.InheritFrom;
      end;
    end else
    begin
      result := aUnit.ElemList.FindItem(IdentName, '', Parent, nil, ClassTypes, vis, AcceptSetType);
      if result = nil then
         result := aUnit.TypeList.FindItem(IdentName, '', Parent, nil, ClassTypes, vis, AcceptSetType);
    end;
    bCanBeUnit := False;
    if result = nil then
    begin
      bCanBeUnit := (ClassTypes = nil) and (UnitName = '');
      if (not bCanBeUnit) and (ClassTypes <> nil) then
      begin
        bCanBeUnit := ClassTypes.TypeIsIn(TSE2Unit);
      end;
      if bCanBeUnit then
         if aUnit.IsName(IdentName) then
           result := aUnit;
    end;

    if result = nil then
    begin
      bCanSearch := True;
      if Parent is TSE2Class then
        if TSE2Class(Parent).IsHelper then
          bCanSearch := False;

      if bCanSearch then       
         result := FindInClassHelpers(aUnit);
    end;

    if result = nil then
      if bCanBeUnit then
        Result := FindUnitName(IdentName);
      //if not (Parent is TSE2Class) then
  end;

{
program Project1;

uses
  SE2.Records.Externals;

var t: SE2.Records.Externals;
begin

end.
}

  function FindUnitEx(const UnitName, IdentName: string): TSE2Unit;
  var iPos: integer;
  begin
    result := nil;
    iPos := Pos('.' + AnsiLowerCase(IdentName), AnsiLowerCase(UnitName));
    if iPos = length(Copy(UnitName, 1, iPos)) { - length(IdentName) } then
    begin
      result := FindUnitName(UnitName);
      if not (result is TSE2Unit) then
         result := nil;
    end;
  end;

begin
  result := nil;

  try
    if (Method <> nil) and (Parent = nil) then
    begin
      if Method.ReturnValue <> nil then
        if Method.ReturnValue.IsName(IdentName) then
          result := Method.ReturnValue;
      if result = nil then
         result := Method.Types.FindItem(IdentName, '', nil, nil, ClassTypes, CAllVisibilities);
      if result = nil then
         result := Method.Variables.FindItem(IdentName, '', nil, nil, ClassTypes, CAllVisibilities);
      if result = nil then
         result := Method.Params.FindItem(IdentName, '', nil, nil, ClassTypes, CAllVisibilities);

      if result <> nil then
         exit;

      result := nil;
    end;

    if UnitName <> '' then
    begin
      if FUnit.IsName(UnitName) then
         pUnit := FUnit
      else
         pUnit := TSE2Unit(FUnitList.FindItem(UnitName));

      if pUnit = nil then
         exit;

      result := IdentifierInUnit(pUnit);

      if (result = nil) and (ClassTypes.TypeIsIn(TSE2Unit)) then
      begin
        pUnit := FindUnitName(UnitName + '.' + IdentName);
        if pUnit = nil then
        begin
          pUnit := FindUnitEx(UnitName, IdentName);
        end;
        if pUnit <> nil then
           result := pUnit;
      end;
    end else
    begin
      result := IdentifierInUnit(FUnit);
      if result = nil then
      begin
        for i:=FUnitList.Count-1 downto 0 do
        begin
          pUnit  := TSE2Unit(FUnitList[i]);
          result := IdentifierInUnit(pUnit);
          if result <> nil then
             exit;
        end;
      end;

    end;
  finally
    ClassTypes.Free;
  end;
end;

procedure TSE2Parser.GenCode(Method: TSE2Method;
  OpCodeData: TSE2LinkOpCode; Index: integer = -1; Replace: boolean = False);
begin
  Assert(Method <> nil);
  if Index = -1 then
     Method.OpCodes.Add(OpCodeData)
  else
  begin
    if Replace then
       Method.OpCodes.Delete(index);

    Method.OpCodes.Insert(Index, OpCodeData);
  end;
end;

procedure TSE2Parser.RaiseError(ErrorType: TSE2ErrorType;
  ErrorText: string; ErrorPos, ErrorLine: integer);
var parentValue : TSE2BaseType;
begin
  if ErrorType = petError then
     FHasError := True;

  if Assigned(FErrorEvent) then
  begin
    if ErrorPos = -1 then
       ErrorPos := Tokenizer.Reader.Position;
    if ErrorLine = -1 then
       ErrorLine := Tokenizer.Reader.Line;

    FErrorEvent(Self, ErrorType, FUnit.AUnitName, ErrorText, ErrorPos, ErrorLine, Tokenizer.Reader.Data);
  end;

  if FHasError then
  begin
    if FCallBackPos > -1 then
      if Tokenizer.Reader.Position >= FCallBackPos then
        if Assigned(FCompileCall) then
        begin
          parentValue := FParserState.AParent;
          //if parentValue = nil then
          //   parentValue := FParserState.CurrentOwner;

          FCompileCall(Self, Tokenizer.Reader.Position, FParserState.ParamIndex, FParserState.Method, FParserState.ParamMethod, parentValue, FParserState.TargetType, Tokenizer.Token.AType, FParserState.NoStaticPointer);
          FCompileCall := nil;
        end;
          
    raise ESE2ParserError.Create('');
  end;
end;

procedure TSE2Parser.ReadNextToken;
var parentValue : TSE2BaseType;
begin           
  Assert(Self <> nil);
  Assert(Tokenizer <> nil);

  if FCallBackPos > -1 then
    if Tokenizer.Reader.Position > FCallBackPos then
      if Assigned(FCompileCall) then
      begin
        parentValue := FParserState.AParent;
        //if parentValue = nil then
        //   parentValue := FParserState.CurrentOwner;
        FCompileCall(Self, Tokenizer.Reader.Position, FParserState.ParamIndex, FParserState.Method, FParserState.ParamMethod, parentValue, FParserState.TargetType, Tokenizer.Token.AType, FParserState.NoStaticPointer);
        FCompileCall := nil;
      end;

  if not Tokenizer.NextToken then
     RaiseError(petError, C_ERR_UnexpectedEndOfFile);

  if Tokenizer.Token.AType = sesUnknown then
     RaiseError(petError, 'Unkown symbol found: '+Tokenizer.Token.Value);

  if Tokenizer.Token.AType in [sesBegin, sesEnd, sesRepeat, sesDo, sesWhile, sesUntil, sesTry, sesExcept, sesFinally,
                              sesFor, sesIf, sesThen, sesElse, sesCase] then
     Tokenizer.InlineDoc := '';
end;

procedure TSE2Parser.ParseProgram(State: TSE2ParseState);
var oldExitList : integer;
    i           : integer;
begin
  FUnit.IsProgram := True;

  State.IsInInterface := True;
  State.Visibility    := visPublic;
  UsesDeclaration(State);    
  LinkArrayMethodEvents;

  State.IsInInterface := False;
  MainBodyDeclaration(State);


  FUnit.Main      := TSE2Method.Create;
  FUnit.Main.Used := True;
  ExpectToken([sesBegin]);
  DeclareType(State, FUnit.Main, C_SE2MainMethod);

  oldExitList := FUnit.Main.Lists.ExitList.Count;
  FUnit.Main.Lists.ExitTableList.Add(State.StackSize);

  ValidateForwards(State);

  GenCode(FUnit.Main, TSE2LinkOpCode.Create(TSE2OpCodeGen.DEBUG_META(0), FUnit.Main.GenLinkerName));
  StatementSquence(State, FUnit.Main);

  ShowUnusedVariables(False);

  FUnit.Main.Lists.ExitTableList.Delete(FUnit.Main.Lists.ExitTableList.Count-1);

  for i:=FUnit.Main.Lists.ExitList.Count-1 downto oldExitList do
  begin
    if PSE2OpFLOW_GOTO(FUnit.Main.OpCodes[FUnit.Main.Lists.ExitList[i]].OpCode)^.Position = 0 then
       PSE2OpFLOW_GOTO(FUnit.Main.OpCodes[FUnit.Main.Lists.ExitList[i]].OpCode)^.Position := FUnit.Main.OpCodes.Count;
    FUnit.Main.Lists.ExitList.Delete(FUnit.Main.Lists.ExitList.Count-1);
  end;

  GenCode(FUnit.Main, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));

  ExpectToken([sesDot]);


  // More Text ???
  if Tokenizer.NextToken then
     RaiseError(petHint, C_HINT_CodeAfterEndIgnored);
end;

procedure TSE2Parser.ParseUnit(State: TSE2ParseState);
var Method: TSE2Method;
begin
  FUnit.IsProgram := False;
  ExpectToken([sesInterface]);
  ReadNextToken;
  State.IsInInterface := True;
  State.Visibility    := visPublic;

  UsesDeclaration(State);
  LinkArrayMethodEvents;

  MainBodyDeclaration(State);

  ExpectToken([sesImplementation]);
  ReadNextToken;
  State.IsInInterface := False;
  State.Visibility    := visPrivate;

  UsesDeclaration(State);

  MainBodyDeclaration(State);

  ValidateForwards(State);
  if Tokenizer.Token.AType = sesInitialization then
  begin
    ReadNextToken;

    Method := TSE2Method.Create;
    Method.Used := True;
    DeclareType(State, Method, C_SE2InitializationMethod);
    FUnit.AInitialization.Add(Method);

    StatementSquence(State, Method);
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));

    if Tokenizer.Token.AType in [sesFinalization] then
    begin
      ExpectToken([sesFinalization]);

      Method := TSE2Method.Create;
      Method.Used := True;
      DeclareType(State, Method, C_SE2FinalizationMethod);
      FUnit.AFinalization.Insert(0, Method);

      ReadNextToken;
      StatementSquence(State, Method);

      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));
    end;
  end;

  ExpectToken([sesEnd]);
  ReadNextToken;
  ExpectToken([sesDot]);

  ShowUnusedVariables(True);

  // More Text ???
  if Tokenizer.NextToken then
     RaiseError(petHint, C_HINT_CodeAfterEndIgnored);
end;


procedure TSE2Parser.UsesDeclaration(State: TSE2ParseState);
var aNewUnit : TSE2Unit;
    fName    : string;

  function RecursiveUnitUsage(const AUnit: TSE2Unit; const UnitName: string): boolean;
  begin
    result := AUnit.RequiredUnits.IndexOf(UnitName) >= 0;
  end;

begin
  if (not StringIdentical(FUnit.AUnitName, C_SE2SystemUnitName) and State.IsInInterface) then
  begin
    aNewUnit := DoNeedUnit(C_SE2SystemUnitName);
    if aNewUnit = nil then
    begin
      RaiseError(petError, C_ERR_SystemUnitNotFound);
      exit;
    end;
    FUnitList.Add(aNewUnit);
  end;
  if Tokenizer.Token.AType in [sesUses] then
  begin
    State.IsInUsesBlock := True;
    ExpectToken([sesUses]);
    ReadNextToken;
    ExpectToken([sesIdentifier]);
    while (FTokenizer.Token.AType = sesIdentifier) do
    begin
      fName := '';
      while Tokenizer.Token.AType = sesIdentifier do
      begin
        fName := fName + Tokenizer.Token.Value;
        ReadNextToken;
        if Tokenizer.Token.AType = sesDot then
        begin
          ReadNextToken;
          fName := fName + '.';
          ExpectToken([sesIdentifier]);
        end;
      end;


      if FUnitList.FindItem(fName) <> nil then
      begin
        RaiseError(petError, Format(C_ERR_UnitAlreadyAdded, [fName]));
        exit;
      end;
      if FUnit.IsName(fName) then
      begin
        RaiseError(petError, Format(C_ERR_CanNotUseOwnUnit, [fName]));
        exit;
      end;
      aNewUnit := DoNeedUnit(fName);
      if aNewUnit = nil then
         RaiseError(petError, Format(C_ERR_CouldNotAddUnit, [fName]))
      else
      if RecursiveUnitUsage(aNewUnit, Self.FUnit.Name) then
         RaiseError(petError, 'Recursive unit usage not possible')
      else
         FUnitList.Add(aNewUnit);

      ExpectToken([sesColon, sesSemiColon]);
      if Tokenizer.Token.AType = sesSemiColon then
         break
      else
      begin
        ReadNextToken;
        ExpectToken([sesIdentifier]);
      end;
    end;
    ReadNextToken;
    State.IsInUsesBlock := False;
  end;
end;

procedure TSE2Parser.MainBodyDeclaration(State: TSE2ParseState);
var Method: TSE2Method;
begin
  while True do
  begin

    State.CurrentOwner := nil;
    State.IsStatic     := False;
    State.Method       := nil;

    case Tokenizer.Token.AType of
    sesType       :
        TypeDeclaration(State, nil);// TypeDeclaration
    sesConst      :
        ConstDeclaration(State, nil);
    sesVar        :
        VariableDeclaration(State, nil);// VarDeclaration
    sesClass :
        begin
          State.IsStatic := True;
          ReadNextToken;
          ExpectToken([sesProcedure, sesFunction]);

          Method := MethodDeclaration(State, False);
          if (not State.IsInInterface) and Assigned(Method) then
          begin
            if not Method.IsExternal then
              if not Method.IsForwarded then
                if not Method.IsAbstract then
                   MethodBodyDeclaration(State, Method);


          end else
          if not Assigned(Method) then
             break;

          State.IsStatic := False;
        end;
    sesConstructor,
    sesDestructor,
    sesProcedure,
    sesFunction   :
        begin
          Method := MethodDeclaration(State, False);
          if (not State.IsInInterface) and Assigned(Method) then
          begin
            if not Method.IsExternal then
              if not Method.IsForwarded then
                if not Method.IsAbstract then
                   MethodBodyDeclaration(State, Method);
          end else
          if not Assigned(Method) then
             break;
        end;
    else
        break;
    end;
  end;
end;

procedure TSE2Parser.VariableDeclaration(State: TSE2ParseState;
  Method: TSE2Method);
var NewVaribles: TSE2BaseTypeList;
    NewVar     : TSE2Variable;
    VarType    : TSE2BaseType;
    i, rt, y   : integer;
    IsPublic   : boolean;
    IsExternal : boolean;
    ASearchTmp : TSE2BaseType;
begin
  State.Method     := Method;
  State.AParent    := nil;
  State.TargetType := nil;
  if (State.CurrentOwner = nil) then
  begin
    if not ExpectToken([sesVar]) then
       exit;
    ReadNextToken;
  end else
  if (State.CurrentOwner <> nil) and (State.Method <> nil) then
  begin
    if not ExpectToken([sesVar]) then
       exit;
    ReadNextToken;
  end;

  if State.CurrentOwner <> nil then
    if TSE2Type(State.CurrentOwner.InheritRoot) = GetExternalObjectType then
      if State.Method = nil then
         RaiseError(petError, 'External classes can not have any variables');


  while True do
  begin
    NewVaribles := TSE2BaseTypeList.Create;
    try
      while (Tokenizer.Token.AType in [sesIdentifier]) do
      begin
        ASearchTmp := FUnit.ElemList.FindItem(Tokenizer.Token.Value, FUnit.AUnitName, State.CurrentOwner, nil, nil, CAllVisibilities);
        if ASearchTmp <> nil then
        begin
          if not ((ASearchTmp.Parent = State.CurrentOwner) and
                 (State.CurrentOwner <> nil) and
                 (Method <> nil)) then
          begin
            RaiseError(petError, 'Name already declared "'+Tokenizer.Token.Value+'"');
            exit;
          end;
        end;

        if State.CurrentOwner is TSE2Class then
          if TSE2Class(State.CurrentOwner).IsPartial then
            if FUnitList.FindItem(State.CurrentOwner.AUnitName) is TSE2Unit then
              if TSE2Unit(FUnitList.FindItem(State.CurrentOwner.AUnitName))
                  .ElemList.FindItem(Tokenizer.Token.Value, State.CurrentOwner.AUnitName, State.CurrentOwner, nil, nil, CAllVisibilities) <> nil then
              begin
                RaiseError(petError, 'Name already declared "'+Tokenizer.Token.Value+'"');
                exit;
              end;

        if NewVaribles.FindItem(Tokenizer.Token.Value) <> nil then
        begin
          RaiseError(petError, 'Name already declared "'+Tokenizer.Token.Value+'"');
          exit;
        end;
        if Method <> nil then
        begin
          if Method.Params.FindItem(Tokenizer.Token.Value) <> nil then
          begin
            RaiseError(petError, 'Name already declared "'+Tokenizer.Token.Value+'"');
            exit;
          end;
        end;

        NewVar := TSE2Variable.Create;
        DeclareType(State, NewVar, Tokenizer.Token.Value);
        if Method = nil then
           NewVar.Parent    := State.CurrentOwner;
        NewVar.IsStatic  := (Method = nil) and (State.CurrentOwner = nil);
        NewVaribles.Add(NewVar);

        ReadNextToken;
        if Tokenizer.Token.AType in [sesColon] then
           ReadNextToken
        else
           break;
      end;

      ExpectToken([sesDoublePoint]);
      ReadNextToken;
      if ExpectToken([sesIdentifier]) then
      begin
        VarType := TypeExpression(Method);
        //VarType := FindIdentifier(Method, Tokenizer.Token.Value, nil);
        if VarType = nil then
           RaiseError(petError, 'Unkown variable type: "'+Tokenizer.Token.Value+'"')
        else
        if not (VarType is TSE2Type) then
           RaiseError(petError, 'Identifier is not a type identifier: "'+Tokenizer.Token.Value+'"')
        else
        begin
          if (State.CurrentOwner = VarType) and (State.CurrentOwner is TSE2Record) and (Method = nil) then
          begin
            RaiseError(petError, 'recursive record declaration not possible');
            exit;
          end;

          CheckForDeprecated(VarType);

          IsPublic   := False;
          IsExternal := False;

          ReadNextToken;

          if Tokenizer.Token.AType in [sesPublic, sesExternal] then
          begin
            if (Method <> nil) or (State.CurrentOwner <> nil) then
            begin
              RaiseError(petError, 'Public or external is not allowed here');
            end;

            case Tokenizer.Token.AType of
            sesPublic   : IsPublic   := True;
            sesExternal : IsExternal := True;
            end;

            ReadNextToken;

            if Tokenizer.Token.AType in [sesPublic, sesExternal] then
            begin
              case Tokenizer.Token.AType of
              sesPublic :
                  begin
                    if IsPublic then
                       RaiseError(petError, 'This variable is already public')
                    else
                       IsPublic := True;
                  end;
              sesExternal :
                  begin
                    if IsExternal then
                       RaiseError(petError, 'This variable is already external')
                    else
                       IsExternal := True;
                  end;
              end;

              ReadNextToken;
            end;
          end;

          ExpectToken([sesSemiColon]);
          ReadNextToken;



          for i:=0 to NewVaribles.Count-1 do
          begin
            NewVar            := TSE2Variable(NewVaribles[i]);
            NewVar.AType      := TSE2Type(VarType);
            NewVar.Used       := IsPublic or IsExternal;
            NewVar.IsPublic   := IsPublic;
            NewVar.IsExternal := IsExternal;
            NewVar.IsStatic   := NewVar.IsStatic or (State.IsStatic and (Method = nil));


            if Method <> nil then
               Method.Variables.Add(NewVar)
            else
               FUnit.ElemList.Add(NewVar);

            if (State.CurrentOwner is TSE2Class) and (State.Method = nil) and (not State.IsStatic) then
            begin
              NewVar.CodePos := TSE2Class(State.CurrentOwner).ClassSize;

              if TSE2Type(NewVar.AType.InheritRoot).DataSize = 0 then
                 RaiseError(petError, 'Internal error: data size can not be 0');

              //Assert(NewVar.AType <> nil);
              //Assert(TSE2Type(NewVar.AType.InheritRoot).DataSize <> 0);


              TSE2Class(State.CurrentOwner).ClassSize := TSE2Class(State.CurrentOwner).ClassSize + TSE2Type(NewVar.AType.InheritRoot).DataSize;
              TSE2Class(State.CurrentOwner).Variables := TSE2Class(State.CurrentOwner).Variables + 1;

              case TSE2Type(NewVar.AType.InheritRoot).AType of
              btString,
              btUTF8String,
              btWideString,
              btPChar,
              btAnsiString,
              btPAnsiChar,
              btPWideChar  : TSE2Class(State.CurrentOwner).RTTI.Add(TSE2Type(NewVar.AType.InheritRoot).AType, NewVar.CodePos, SizeOf(Pointer));
              btArray      : TSE2Class(State.CurrentOwner).RTTI.Add(btArray, NewVar.CodePos, integer(TSE2Array(NewVar.AType)));
              btRecord     : TSE2Class(State.CurrentOwner).RTTI.Add(btRecord, NewVar.CodePos, integer(TSE2Record(NewVar.AType.InheritRoot)));
              end;
            end else
            if (State.CurrentOwner is TSE2Record) and (State.Method = nil) and (not State.IsStatic) then
            begin
              NewVar.CodePos := TSE2Record(State.CurrentOwner).RecordSize;

              if NewVar.AType is TSE2Record then
              begin
                TSE2Record(State.CurrentOwner).RecordSize := TSE2Record(State.CurrentOwner).RecordSize + TSE2Record(NewVar.AType).RecordSize;
                for rt:=0 to TSE2Record(NewVar.AType).RTTI.Count-1 do
                begin
                  TSE2Record(State.CurrentOwner).RTTI.Add( TSE2Record(NewVar.AType).RTTI.Duplicate(rt, NewVar.CodePos));
                end;

                //TSE2Record(State.CurrentOwner).RTTI.Add(btRecord, NewVar.CodePos, integer(NewVar.AType));
              end else
              if (NewVar.AType is TSE2Array) and (not TSE2Array(NewVar.AType).IsDynamic) then
              begin
                TSE2Record(State.CurrentOwner).RecordSize := TSE2Record(State.CurrentOwner).RecordSize + TSE2Array(NewVar.AType).ArraySize;
                for rt:=0 to TSE2Array(NewVar.AType).RTTI.Count-1 do
                begin
                  for y:=0 to TSE2Array(NewVar.AType).ArrayCount-1 do
                    TSE2Record(State.CurrentOwner).RTTI.Add(
                      TSE2Array(NewVar.AType).RTTI.Duplicate(rt, NewVar.CodePos + y * TSE2Array(NewVar.AType).ElemSize)
                    );
                end;
              end else
              begin
                if TSE2Type(NewVar.AType.InheritRoot).DataSize = 0 then
                   RaiseError(petError, 'Internal error: data size can not be 0');

                TSE2Record(State.CurrentOwner).RecordSize := TSE2Record(State.CurrentOwner).RecordSize + TSE2Type(NewVar.AType.InheritRoot).DataSize;
                case TSE2Type(NewVar.AType.InheritRoot).AType of
                btString,
                btUTF8String,
                btWideString,
                btPChar,
                btAnsiString,
                btPAnsiChar,
                btPWideChar  : TSE2Record(State.CurrentOwner).RTTI.Add(TSE2Type(NewVar.AType.InheritRoot).AType, NewVar.CodePos, SizeOf(Pointer));
                btArray      : TSE2Record(State.CurrentOwner).RTTI.Add(btArray, NewVar.CodePos, integer(NewVar.AType));
                btRecord     : TSE2Record(State.CurrentOwner).RTTI.Add(btRecord, NewVar.CodePos, integer(NewVar.AType));
                end;
              end;
            end;
          end;
          NewVaribles.OwnsObjs := False;
        end;

        if Tokenizer.Token.AType = sesDeprecated then
        begin
          ProcessDeprecatedExpression(State, NewVaribles.Items[NewVaribles.Count-1]);
          ExpectToken([sesSemiColon]);
          ReadNextToken;
        end;

      end;
    finally
      NewVaribles.Free;
    end;

    if ((State.CurrentOwner is TSE2Class) or (State.CurrentOwner is TSE2Record)) and (State.IsStatic) and (Method = nil) then
       break
    else
    if not (Tokenizer.Token.AType in [sesIdentifier]) then
       break;
  end;
end;

procedure TSE2Parser.TypeDeclaration(State: TSE2ParseState; Method: TSE2Method);
var TypeName : string;
begin
  State.Method     := Method;
  State.AParent    := nil;
  State.TargetType := nil;

  if State.CurrentOwner <> nil then
  begin
    RaiseError(petError, 'Type declaration not allowed here');
    exit;
  end;

  ExpectToken([sesType]);
  ReadNextToken;
  while True do
  begin
    ExpectToken([sesIdentifier]);

    TypeName := Tokenizer.Token.Value;
    ReadNextToken;

    ExpectToken([sesEqual]);
    ReadNextToken;
    case Tokenizer.Token.AType of
    sesIdentifier,
    sesType       : begin
                      TypeTypeDeclaration(State, Method, TypeName);
                    end;
    sesOpenRound,
    sesSet        : begin
                      SetDeclaration(State, Method, TypeName);
                    end;
    sesSealed,
    sesPartial,
    sesHelper,
    sesClass      : begin
                      ClassDeclaration(State, TypeName);
                    end;
    sesRecord     : begin
                      RecordDeclaration(State, TypeName);
                    end;
    sesInterface  : begin
                      InterfaceDeclaration(State, TypeName);
                    end;
    sesArray      : begin
                      ArrayTypeDeclaration(State, TypeName);
                    end;
    sesProcedure,
    sesFunction   :
                    begin
                      MethodTypeDeclaration(State, TypeName);
                    end;
    else ExpectToken([sesIdentifier, sesClass, sesHelper, sesSet, sesOpenRound, sesPartial, sesInterface, sesRecord, sesProcedure, sesFunction]);
    end;

    if not (Tokenizer.Token.AType in [sesIdentifier]) then
       break;
  end;
end;

procedure TSE2Parser.MethodTypeDeclaration(State: TSE2ParseState;
  MethodName: string);
var newMethod : TSE2MethodVariable;
begin
  State.Method     := nil;
  State.AParent    := nil;
  State.TargetType := nil;


  ExpectToken([sesProcedure, sesFunction]);
  newMethod := TSE2MethodVariable.Create;
  try
    DeclareType(State, newMethod, MethodName);
    FUnit.TypeList.Add(newMethod);
    newMethod.Method := MethodDeclaration(State, True);
    if newMethod.Method <> nil then
    begin
      newMethod.Method.Name      := newMethod.Name;
      newMethod.Method.AUnitName := newMethod.AUnitName;
      newMethod.Method.DeclLine  := newMethod.DeclLine;
      newMethod.Method.DeclPos   := newMethod.DeclPos;
      newMethod.Method.InlineDoc := newMethod.InlineDoc;
      newMethod.Method.IsMethodType := True;
    end;
  finally

  end;

end;

procedure TSE2Parser.SetDeclaration(State: TSE2ParseState;
  Method: TSE2Method; TypeName: string);
var isSet    : boolean;
    aType    : TSE2Enum;
    enumList : TSE2BaseTypeList;
    newEnum  : TSE2Constant;
    newSet   : TSE2SetOf;
    newType  : TSE2Type;
    i        : integer;
begin
  State.Method     := Method;
  State.AParent    := nil;
  State.TargetType := nil;

  isSet := Tokenizer.Token.AType in [sesSet];
  if isSet then
  begin
    ExpectToken([sesSet]);
    ReadNextToken;
    ExpectToken([sesOf]);
    ReadNextToken;
    if Tokenizer.Token.AType in [sesIdentifier] then
    begin
      aType := TSE2Enum(FindIdentifier(Method, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Enum])));
      if aType = nil then
         RaiseError(petError, 'Unknown identifier: "'+Tokenizer.Token.Value+'"');

      enumList := TSE2BaseTypeList.Create;
      enumList.OwnsObjs := False;
      enumList.SetID    := False;
      try
        for i:=0 to FUnit.ElemList.Count-1 do
        begin
          if FUnit.ElemList[i] is TSE2Constant then
            if TSE2Constant(FUnit.ElemList[i]).AType = aType then
               enumList.Add(FUnit.ElemList[i]);
        end;

        newSet := TSE2SetOf.Create;
        DeclareType(State, newSet, TypeName);
        newSet.Strict   := True;
        case enumList.Count of
        0..8 :
            begin
              newSet.AType    := btU8;
              newSet.DataSize := 1;
            end;
        9..16 :
            begin
              newSet.AType    := btU16;
              newSet.DataSize := 2;
            end;
        else
            begin
              newSet.AType    := btU32;
              newSet.DataSize := 4;
            end;
        end;
        FUnit.TypeList.Add(newSet);

        for i:=0 to enumList.Count-1 do
        begin
          newEnum := TSE2Constant.Create;
          newEnum.AType      := newSet;
          newEnum.AsInteger  := 1 shl i;
          newEnum.Name       := enumList[i].Name;
          newEnum.AUnitName   := enumList[i].AUnitName;
          newEnum.Visibility := enumList[i].Visibility;
          newEnum.Parent     := enumList[i].Parent;
          FUnit.ElemList.Add(newEnum);
        end;
      finally
        enumList.Free;
      end;

      ReadNextToken;
      ExpectToken([sesSemiColon]);
      ReadNextToken;

      exit;
    end;
  end;
  ExpectToken([sesOpenRound]);
  ReadNextToken;
  ExpectToken([sesIdentifier]);

  if isSet then
    newType := TSE2SetOf.Create
  else
    newType := TSE2Enum.Create;
  try
    DeclareType(State, newType, TypeName);
    newType.Strict  := True;

    enumList := TSE2BaseTypeList.Create;
    enumList.SetID := False;
    try
      while Tokenizer.Token.AType in [sesIdentifier] do
      begin
        ExpectToken([sesIdentifier]);
        if enumList.FindItem(Tokenizer.Token.Value, '', newType, nil, nil, CAllVisibilities) <> nil then
           RaiseError(petError, 'Identifier already in usage: "'+Tokenizer.Token.Value+'"');


        newEnum := TSE2Constant.Create;
        DeclareType(State, newEnum, Tokenizer.Token.Value);
        newEnum.AType     := newType;
        if isSet then
           newEnum.AsInteger := 1 shl enumList.Count
        else
           newEnum.AsInteger := enumList.Count;
        enumList.Add(newEnum);

        ReadNextToken;
        if Tokenizer.Token.AType = sesColon then
        begin
          ReadNextToken;
          ExpectToken([sesIdentifier]);
        end else
          ExpectToken([sesCloseRound]);
      end;

      if not FHasError then
      begin
        for i:=0 to enumList.Count-1 do
          FUnit.ElemList.Add(enumList[i]);
        enumList.OwnsObjs := False;

        if not isSet then
        begin
          if enumList.Count > 65535 then
            newType.AType := btU32
          else
          if enumList.Count > 255 then
            newType.AType := btU16
          else
            newType.AType := btU8;

        end else
        begin
          case enumList.Count of
          0..8  : newType.AType := btU8;
          9..16 : newType.AType := btU16;
          else    newType.AType := btU32;
          end;
        end;

        case newType.AType of
        btU32  : newType.DataSize := 4;
        btU16  : newType.DataSize := 2;
        btU8   : newType.DataSize := 1;
        end;
      end;
    finally
      enumList.Free;
    end;

    if not FHasError then
       FUnit.TypeList.Add(newType);
  finally
    if FHasError then
    begin
       newType.Free;
       newType := nil;
    end;
  end;

  ExpectToken([sesCloseRound]);
  ReadNextToken;
  ExpectToken([sesSemiColon]);
  ReadNextToken;
  if Tokenizer.Token.AType = sesDeprecated then
  begin
    ProcessDeprecatedExpression(State, newType);
    ExpectToken([sesSemiColon]);
    ReadNextToken;
  end;
end;

procedure TSE2Parser.ClassDeclaration(State: TSE2ParseState;
  ClassName: string);
var ClassType : TSE2Class;
    BaseClass : TSE2Type;
    IsPartial : boolean;
    IsHelper  : boolean;
    IsSealed  : boolean;
    IsFirstPartial : boolean;
    i, j      : integer;
begin
  State.Method     := nil;
  State.AParent    := nil;
  State.TargetType := nil;

  IsHelper  := False;
  IsPartial := False;
  IsSealed  := False;

  if Tokenizer.Token.AType in [sesSealed] then
  begin
    IsSealed := True;
    ReadNextToken;
  end;

  if Tokenizer.Token.AType in [sesPartial] then
  begin
    IsPartial := True;
    ReadNextToken;
  end;

  ClassType := TSE2Class(FindIdentifier(nil, ClassName, nil, FUnit.Name, TSE2BaseTypeFilter.Create([TSE2Class])));
  if ClassType <> nil then
  begin
    if (not ClassType.IsForwarded) and (not (IsPartial and ClassType.IsPartial))  then
    begin
      RaiseError(petError, 'This class is already defined: "'+ClassName+'"');
      exit;
    end;

    if (ClassType.IsForwarded and IsPartial) then
    begin
      RaiseError(petError, 'Forwarded classes can not be declared as partial class');
      exit;
    end;
  end;

  ExpectToken([sesClass, sesHelper]);
  if Tokenizer.Token.AType in [sesHelper] then
    if IsSealed then
       RaiseError(petError, 'Helper classes can not be sealed')
    else
       IsHelper := True;

  ReadNextToken;
  if (Tokenizer.Token.AType = sesSemiColon) then
  begin
    if IsPartial then
       RaiseError(petError, 'Partial classes can not be forwarded');
    if (ClassType = nil) then
    begin
      ClassType := TSE2Class.Create;
      DeclareType(State, ClassType, ClassName);
      if State.IsInInterface then
         ClassType.Visibility := visPublic
      else
         ClassType.Visibility := visPrivate;
      ClassType.IsPartial   := IsPartial;
      ClassType.IsForwarded := True;
      ClassType.IsSealed    := IsSealed;
      ClassType.AType       := btObject;
      FUnit.TypeList.Add(ClassType);

      BaseClass := GetScriptObjectType;
      if BaseClass = nil then
      begin
        RaiseError(petError, 'FATAL ERROR: internal '+C_SE2TObjectName+' not found!');
        exit;
      end;

      ClassType.DataSize := BaseClass.DataSize;
      ReadNextToken;
      exit;
    end else
    begin
      RaiseError(petError, 'Class "'+ClassName+'" is already forwarded');
      exit;
    end;
  end;

  if IsPartial then
  begin
    ClassType := TSE2Class(FindIdentifier(nil, ClassName, nil, '', TSE2BaseTypeFilter.Create([TSE2Class])));
    if ClassType <> nil then
       if IsSealed and not ClassType.IsSealed then
          RaiseError(petError, 'Partial part of the class can not be sealed, if the first part is not declared as sealed');
  end;

  if ClassType = nil then
  begin
    ClassType := TSE2Class.Create;
    DeclareType(State, ClassType, ClassName);
    if State.IsInInterface then
       ClassType.Visibility := visPublic
    else
       ClassType.Visibility := visPrivate;
    ClassType.IsPartial := IsPartial;
    ClassType.IsSealed  := IsSealed;
    FUnit.TypeList.Add(ClassType);
  end else
  begin
    ClassType.IsForwarded := False;
  end;

  try
    IsFirstPartial := False;
    if IsPartial then
      if ClassType.InheritFrom = nil then
        IsFirstPartial := True
      else
      begin
        if not ClassType.IsTypeOf(GetExternalObjectType) then
        begin
          for i:=0 to FUnitList.Count-1 do
            for j:=0 to TSE2Unit(FUnitList[i]).TypeList.Count-1 do
              if TSE2BaseType(TSE2Unit(FUnitList[i]).TypeList.List.List[j]) is TSE2Class then
                if TSE2Class(TSE2Unit(FUnitList[i]).TypeList.List.List[j]).IsTypeOf(ClassType) then
                   RaiseError(petError, 'Can not extend partial class because "'+
                         TSE2Class(TSE2Unit(FUnitList[i]).TypeList.List.List[j]).GetStrongName +
                         '" has already derived from the class');

          for j:=0 to TSE2Unit(FUnit).TypeList.Count-1 do
            if TSE2BaseType(TSE2Unit(FUnit).TypeList.List.List[j]) is TSE2Class then
              if TSE2Class(TSE2Unit(FUnit).TypeList.List.List[j]).IsTypeOf(ClassType) then
                 RaiseError(petError, 'Can not extend partial class because "'+
                       TSE2Class(TSE2Unit(FUnit).TypeList.List.List[j]).GetStrongName +
                       '" has already derived from the class');
        end;
      end;

    if (Tokenizer.Token.AType = sesHelper) or IsHelper then
    begin
      ClassType.IsHelper := True;

      if not IsHelper then
         ReadNextToken;
      IsHelper := True;
      ExpectToken([sesFor]);
      ReadNextToken;
      ExpectToken([sesIdentifier]);

      BaseClass := TypeExpression(nil);

      //BaseClass := TSE2Type(FindIdentifier(nil, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Type])));
      if BaseClass = nil then
         RaiseError(petError, 'Unknown identifier: "'+Tokenizer.Token.Value+'"');

      if BaseClass is TSE2Class then
        if TSE2Class(BaseClass).IsHelper then
          RaiseError(petError, 'Helper objects can not be a helper for another helper object');

      if BaseClass is TSE2MethodVariable then
        if TSE2MethodVariable(BaseClass).Method.ReturnValue <> nil then
           RaiseError(petError, 'Helper classes can not be made for method types with a return value');


      CheckForDeprecated(BaseClass);

      ReadNextToken;
    end else
    if Tokenizer.Token.AType = sesOpenRound then
    begin
      if IsPartial then
        if ClassType.InheritFrom <> nil then
          if ClassType.IsUnit(FUnit.Name) then
             RaiseError(petError, 'partial part of the class must not have a class parent');



      if ClassType.InheritFrom <> nil then
        if not ClassType.IsUnit(FUnit.Name) then
        begin
          ClassType := TSE2Class.Create;
          DeclareType(State, ClassType, ClassName);
          if State.IsInInterface then
             ClassType.Visibility := visPublic
          else
             ClassType.Visibility := visPrivate;
          ClassType.IsPartial := IsPartial;
          ClassType.IsSealed  := IsSealed;
          FUnit.TypeList.Add(ClassType);
        end;

      IsFirstPartial := True;
      ReadNextToken;
      ExpectToken([sesIdentifier]);

      BaseClass := TSE2Class( TypeExpression(nil) );
      if BaseClass = nil then
         RaiseError(petError, 'Unkown identifier: "'+Tokenizer.Token.Value+'"')
      else
      if not (BaseClass is TSE2Class) then
      begin
         RaiseError(petError, 'Base type is not a class: ' + BaseClass.GetStrongName);
         exit;
      end;

      if TSE2Class(BaseClass).IsForwarded then
         RaiseError(petError, 'Can not derive from incompleted class "' + TSE2Class(BaseClass).GetStrongName + '"');

      if TSE2Class(BaseClass).IsHelper then
      begin
         RaiseError(petError, 'Classes can not derive from helper classes');
         exit;
      end;

      if BaseClass = ClassType then
      begin
         RaiseError(petError, 'The class can not derive from itself');
         exit;
      end;

      if TSE2Class(BaseClass).IsSealed then
      begin
        RaiseError(petError, 'Cannot derive from sealed type ' + BaseClass.GetStrongName);
        exit;
      end;

      CheckForDeprecated(BaseClass);

      ClassType.InheritFrom := BaseClass;
      ReadNextToken;
      ExpectToken([sesCloseRound]);
      ReadNextToken;
    end else
    begin
      BaseClass := GetScriptObjectType;
      if BaseClass = nil then
      begin
        RaiseError(petError, 'FATAL ERROR: internal '+C_SE2TObjectName+' not found!');
        exit;
      end;
    end;

    if IsHelper then
    begin
      ClassType.InheritFrom := BaseClass;
    end else
    if (IsPartial and IsFirstPartial) or ((not IsPartial)) and (not IsHelper) then
    begin
      ClassType.InheritFrom := BaseClass;
      ClassType.ClassSize   := TSE2Class(BaseClass).ClassSize;
      ClassType.Variables   := TSE2Class(BaseClass).Variables;
      ClassType.DynMethods  := TSE2Class(BaseClass).DynMethods;
    end;

    State.CurrentOwner := ClassType;
    State.AParent      := ClassType;
    while (Tokenizer.Token.AType in [sesPublic, sesProtected, sesPrivate]) do
    begin
      State.AParent := ClassType;
      case Tokenizer.Token.AType of
      sesPublic     : State.Visibility := visPublic;
      sesProtected  : State.Visibility := visProtected;
      sesPrivate    : State.Visibility := visPrivate;
      end;

      ReadNextToken;
      while True do
      begin
        State.IsStatic := False;
        State.AParent  := ClassType;
        if Tokenizer.Token.AType = sesClass then
        begin
          if IsHelper then
             RaiseError(petError, 'Static elements are not allowed in class helpers');
          State.IsStatic := True;
          ReadNextToken;
          ExpectToken([sesProcedure, sesFunction, sesProperty, sesVar]);
        end;
        case Tokenizer.Token.AType of
        sesPrivate, sesPublic, sesProtected :
            begin
              State.AParent := ClassType;
              if State.IsStatic then
                 ExpectToken([sesIdentifier, sesProcedure, sesFunction, sesVar]);
              break;
            end;
        sesVar          :
            if not State.IsStatic then
               RaiseError(petError, 'var is not allowed here')
            else
            begin
              ReadNextToken;
              ExpectToken([sesIdentifier]);
              VariableDeclaration(State, nil);
            end;
        sesConst :
            if State.IsStatic then
               RaiseError(petError, 'constants are always static')
            else
            begin
               ConstDeclaration(State, nil);
            end;
        sesIdentifier   :
            begin
              if State.IsStatic then
                 ExpectToken([sesVar])
              else
              begin
                if IsHelper then
                   RaiseError(petError, 'Variables are not allowed in class helpers');
                 VariableDeclaration(State, nil);
              end;
            end;
        sesProcedure,
        sesFunction     :
            begin
              MethodDeclaration(State, False);
            end;
        sesConstructor,
        sesDestructor   :
            begin
              if IsHelper then
                 RaiseError(petError, 'Class helpers can not have a constructor/destructor');
              if State.IsStatic then
                 ExpectToken([sesIdentifier, sesProcedure, sesFunction])
              else
              begin
                State.IsStatic := Tokenizer.Token.AType = sesConstructor;
                MethodDeclaration(State, False);
                State.IsStatic := False;
              end;
            end;
        sesProperty    :
            begin
              PropertyDeclaration(State);
            end;
        end;
        if not (Tokenizer.Token.AType in [sesIdentifier, sesProcedure, sesFunction, sesConst, sesClass, sesConstructor, sesDestructor, sesProperty]) then
           break;
      end;
    end;
    ExpectToken([sesEnd]);
    ReadNextToken;
    State.AParent := nil;
    ExpectToken([sesSemiColon]);
    ReadNextToken;
    if Tokenizer.Token.AType = sesDeprecated then
    begin
      ProcessDeprecatedExpression(State, ClassType);
      ExpectToken([sesSemiColon]);
      ReadNextToken;
    end;
  finally
    State.CurrentOwner := nil;
  end;
end;

procedure TSE2Parser.InterfaceDeclaration(State: TSE2ParseState;
  InterfaceName: string);
begin
  State.Method     := nil;
  State.AParent    := nil;
  State.TargetType := nil;
  RaiseError(petError, 'interface declaration not implemented yet');    
end;

procedure TSE2Parser.ArrayTypeDeclaration(State: TSE2ParseState;
  ArrayName: string);
var minIndex, maxIndex: integer;
    newType           : TSE2Array;
    BaseType          : TSE2BaseType;
    constType         : TSE2Constant;
    i                 : integer;
    isDynamic         : boolean;
begin
  State.Method     := nil;
  State.AParent    := nil;
  State.TargetType := nil;

  if FindIdentifier(nil, ArrayName, nil, FUnit.Name, TSE2BaseTypeFilter.Create([TSE2Type])) <> nil then
     RaiseError(petError, 'The identifier "'+ArrayName+'" is already in usage');


  if ExpectToken([sesArray]) then
  begin
    RaiseError(petWarning, 'Array implementation is not finished yet and only experimental at the moment! Usage at own risk!');
    //RaiseError(petError, 'Arrays are not really supported yet');

    minIndex := 0;
    maxIndex := 0;
    ReadNextToken;
    if Tokenizer.Token.AType = sesOpenBracket then
    begin
      isDynamic := False;
      ExpectToken([sesOpenBracket]);
      ReadNextToken;
      ExpectToken([sesInteger, sesIdentifier]);
      if Tokenizer.Token.AType = sesInteger then
         minIndex := FTokenizer.Token.AsInteger
      else
      begin
        constType := ConstExpression(State.Method);
        if constType = nil then
           RaiseError(petError, 'Constant expression expected for min index');
        if not constType.IsInteger then
           RaiseError(petError, 'Ordinal constant expected for min index');

        minIndex := constType.AsInteger;
      end;

      ReadNextToken;
      ExpectToken([sesDotDot]);
      ReadNextToken;
      ExpectToken([sesInteger, sesIdentifier]);
      if Tokenizer.Token.AType = sesInteger then
         maxIndex := FTokenizer.Token.AsInteger
      else
      begin
        constType := ConstExpression(State.Method);
        if constType = nil then
           RaiseError(petError, 'Constant expression expected for max index');
        if not constType.IsInteger then
           RaiseError(petError, 'Ordinal constant expected for max index');

        maxIndex := constType.AsInteger;
      end;

      if minIndex > maxIndex then
         RaiseError(petError, 'Min index can not be greater than max index');

      ReadNextToken;
      ExpectToken([sesCloseBracket]);
      ReadNextToken;
    end else
      isDynamic := True;
    ExpectToken([sesOf]);
    ReadNextToken;
    ExpectToken([sesIdentifier]);

    BaseType := TypeExpression(State.Method);
    if BaseType = nil then
       RaiseError(petError, 'Unkown identifier: "'+Tokenizer.Token.Value+'"')
    else
    begin
      newType := TSE2Array.Create;
      newType.InheritFrom := GetBaseArrayType;
      DeclareType(State, newType, ArrayName);
      newType.Content.AType := TSE2Type(BaseType);
      if isDynamic then
      begin
        newType.IsDynamic  := True;
        newType.StartIndex := 0;
        newType.ArrayCount := 0;
      end else
      begin
        newType.IsDynamic  := False;
        newType.StartIndex := minIndex;
        newType.ArrayCount := maxIndex + 1 - minIndex;
      end;
      if newType.Content.AType is TSE2Record then
      begin
        newType.ArraySize  := newType.ArrayCount * TSE2Record(newType.Content.AType).RecordSize;
        for i:=0 to TSE2Record(newType.Content.AType).RTTI.Count-1 do
          newType.RTTI.Add(TSE2Record(newType.Content.AType).RTTI.Duplicate(i, 0));
      end else
      if newType.Content.AType is TSE2Array then
      begin
        if TSE2Array(newType.Content.AType).IsDynamic then
        begin
          newType.ArraySize := newType.ArrayCount * TSE2Type(newType.Content.AType).DataSize;
          newType.RTTI.Add(btArray, 0, integer(newType.Content.AType));
        end else
        begin
          newType.ArraySize  := newType.ArrayCount * TSE2Array(newType.Content.AType).ArraySize;
          for i:=0 to TSE2Array(newType.Content.AType).RTTI.Count-1 do
            newType.RTTI.Add(TSE2Array(newType.Content.AType).RTTI.Duplicate(i, 0));
        end;
      end else
      begin
        newType.ArraySize  := newType.ArrayCount * TSE2Type(newType.Content.AType.InheritRoot).DataSize;
        case TSE2Type(newType.Content.AType.InheritRoot).AType of
        btString,
        btUTF8String,
        btWideString,
        btPChar,
        btAnsiString,
        btPAnsiChar,
        btPWideChar  : newType.RTTI.Add(TSE2Type(newType.Content.AType.InheritRoot).AType, 0, SizeOf(Pointer));
        end;
      end;

      //if Method <> nil then
      //   Method.Types.Add(newType)
      //else
      FUnit.TypeList.Add(newType);
      ReadNextToken;
      ExpectToken([sesSemiColon]);
      ReadNextToken;
    end;
  end;

end;

procedure TSE2Parser.RecordDeclaration(State: TSE2ParseState;
  RecordName: string);
var RecordType : TSE2Record;
begin
  State.Method     := nil;
  State.AParent    := nil;
  State.TargetType := nil;

  RecordType := TSE2Record(FindIdentifier(nil, RecordName, nil, FUnit.Name, TSE2BaseTypeFilter.Create([TSE2Record])));
  if RecordType <> nil then
     RaiseError(petError, 'This record is already defined: "'+RecordName+'"');

  ExpectToken([sesRecord]);

  RecordType := TSE2Record.Create;
  DeclareType(State, RecordType, RecordName);
  if State.IsInInterface then
     RecordType.Visibility := visPublic
  else
     RecordType.Visibility := visPrivate;
  FUnit.TypeList.Add(RecordType);

  ReadNextToken;
  try
    RecordType.RecordSize := 0;
    State.CurrentOwner := RecordType;
    State.AParent      := RecordType;
    State.Visibility   := visPublic;
    while True do
    begin
      State.AParent := RecordType;
      if Tokenizer.Token.AType in [sesPublic, sesProtected, sesPrivate] then
      begin
        case Tokenizer.Token.AType of
        sesPublic    : State.Visibility := visPublic;
        sesProtected : State.Visibility := visProtected;
        sesPrivate   : State.Visibility := visPrivate;
        end;
        ReadNextToken;
      end;
      State.IsStatic := False;
      State.AParent  := RecordType;
      if Tokenizer.Token.AType = sesClass then
      begin
        State.IsStatic := True;
        ReadNextToken;
        ExpectToken([sesProcedure, sesFunction, sesProperty, sesVar]);
      end;
      case Tokenizer.Token.AType of
      sesPrivate, sesPublic, sesProtected :
          begin
            State.AParent := RecordType;
            if State.IsStatic then
               ExpectToken([sesIdentifier, sesProcedure, sesFunction, sesProperty, sesVar]);
            break;
          end;
      sesVar          :
          if not State.IsStatic then
             RaiseError(petError, 'var is not allowed here')
          else
          begin
            ReadNextToken;
            ExpectToken([sesIdentifier]);
            VariableDeclaration(State, nil);
          end;
      sesIdentifier   :
          begin
            if State.IsStatic then
               ExpectToken([sesVar])
            else
               VariableDeclaration(State, nil);
          end;
      sesConst :
          begin
            if State.IsStatic then
               RaiseError(petError, 'Record constants are always static')
            else
               ConstDeclaration(State, nil);
          end;
      sesProcedure,
      sesFunction     :
          begin
            MethodDeclaration(State, False);
          end;
      sesConstructor,
      sesDestructor   :
          begin
            if Tokenizer.Token.AType = sesConstructor then
               RaiseError(petError, 'Records can not have a constructor')
            else
               RaiseError(petError, 'Records can not have a destructor');
          end;
      sesProperty    :
          begin
            PropertyDeclaration(State);
          end
      end;
      if not (Tokenizer.Token.AType in [sesIdentifier, sesProcedure, sesConst, sesFunction, sesClass, sesProperty, sesPrivate, sesProtected, sesPublic]) then
         break;

    end;
    ExpectToken([sesEnd]);
    ReadNextToken;
    State.AParent := nil;
    ExpectToken([sesSemiColon]);
    ReadNextToken;

    if Tokenizer.Token.AType = sesDeprecated then
    begin
      ProcessDeprecatedExpression(State, RecordType);
      ExpectToken([sesSemiColon]);
      ReadNextToken;
    end;
  finally
    State.CurrentOwner := nil;
  end;

  (*
    while (Tokenizer.Token.AType in [sesPublic, sesProtected, sesPrivate]) and (not FHasError) do
    begin
      State.AParent := ClassType;
      case Tokenizer.Token.AType of
      sesPublic     : State.Visibility := visPublic;
      sesProtected  : State.Visibility := visProtected;
      sesPrivate    : State.Visibility := visPrivate;
      end;

      ReadNextToken;
      while not FHasError do
      begin
        State.IsStatic := False;
        State.AParent  := ClassType;
        if Tokenizer.Token.AType = sesClass then
        begin
          State.IsStatic := True;
          ReadNextToken;
        end;
        case Tokenizer.Token.AType of
        sesPrivate, sesPublic, sesProtected :
            begin
              State.AParent := ClassType;
              if State.IsStatic then
                 ExpectToken([sesIdentifier, sesProcedure, sesProperty, sesFunction, sesVar]);
              break;
            end;
        sesVar          :
            if not State.IsStatic then
               RaiseError(petError, 'var is not allowed here')
            else
            begin
              ReadNextToken;
              ExpectToken([sesIdentifier]);
              VariableDeclaration(State, nil);
            end;
        sesIdentifier   :
            begin
              if State.IsStatic then
                 ExpectToken([sesVar])
              else
                 VariableDeclaration(State, nil);
            end;
        sesProcedure,
        sesFunction     :
            begin
              MethodDeclaration(State);
            end;
        sesConstructor,
        sesDestructor   :
            begin
              if State.IsStatic then
                 ExpectToken([sesIdentifier, sesProcedure, sesFunction])
              else
              begin
                State.IsStatic := Tokenizer.Token.AType = sesConstructor;
                MethodDeclaration(State);
                State.IsStatic := False;
              end;
            end;
        sesProperty    :
            begin
              PropertyDeclaration(State);
            end;
        end;
        if not (Tokenizer.Token.AType in [sesIdentifier, sesProcedure, sesFunction, sesClass, sesConstructor, sesDestructor, sesProperty]) then
           break;
      end;
    end;
    ExpectToken([sesEnd]);
    ReadNextToken;
    ExpectToken([sesSemiColon]);
    State.AParent := nil;
    ReadNextToken;
  finally
    State.CurrentOwner := nil;
  end;

  *)


  //RaiseError(petError, 'record declaration not implemented yet');
end;

procedure TSE2Parser.TypeTypeDeclaration(State: TSE2ParseState; Method: TSE2Method;
  TypeName: string);
var newType  : TSE2Type;
    BaseType : TSE2BaseType;
    IsStrict : boolean;
begin
  State.Method     := Method;
  State.AParent    := nil;
  State.TargetType := nil;

  if ExpectToken([sesIdentifier, sesType]) then
  begin
    IsStrict := Tokenizer.Token.AType = sesType;
    if IsStrict then
    begin
      ReadNextToken;
      if not ExpectToken([sesIdentifier]) then
         exit;
    end;
    BaseType := TypeExpression(nil);
    //BaseType := FindIdentifier(nil, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Type]));
    if BaseType = nil then
       RaiseError(petError, 'Unkown identifier: "'+Tokenizer.Token.Value+'"')
    else
    begin
      CheckForDeprecated(BaseType);

      if BaseType is TSE2Class then
         newType := TSE2Class.Create
      else
      if BaseType is TSE2Record then
         newType := TSE2Record.Create
      else
      if BaseType is TSE2Enum then
         newType := TSE2Enum.Create
      else
      if BaseType is TSE2Array then
         newType := TSE2Array.Create
      else
      if BaseType is TSE2MethodVariable then
         newType := TSE2MethodVariable.Create
      else
         newType := TSE2Type.Create;

         
      DeclareType(State, newType, TypeName);
      NewType.InheritFrom := BaseType;
      newType.DataSize := TSE2Type(BaseType).DataSize;
      if newType is TSE2Record then
         TSE2Record(newType).RecordSize := TSE2Record(BaseType).RecordSize
      else
      if newType is TSE2Array then
      begin
        TSE2Array(newType).ArrayCount := TSE2Array(BaseType).ArrayCount;
        TSE2Array(newType).ArraySize  := TSE2Array(BaseType).ArraySize;
        TSE2Array(newType).IsDynamic  := TSE2Array(BaseType).IsDynamic;
        TSE2Array(newType).Content.AType := TSE2Array(BaseType).Content.AType;
        TSE2Array(newType).StartIndex := TSE2Array(BaseType).StartIndex;
        TSE2Array(newType).AType      := TSE2Array(BaseType).AType;
      end;


      //if IsStrict then
        //newType.Compatibles.AddCompatbile(BaseType.InheritRoot, BaseType.InheritRoot.BaseCompatibles, False)
      //else
        //NewType.Compatibles.AddCompatbile(BaseType, BaseType.BaseCompatibles, True);

      //newType.Compatibles.AddCompatbile(BaseType, [sesExplicitCast], True);
      //BaseType.Compatibles.AddCompatbile(newType, [sesExplicitCast], True);

      if Method <> nil then
         Method.Types.Add(newType)
      else
         FUnit.TypeList.Add(newType);
      ReadNextToken;
      ExpectToken([sesSemiColon]);
      ReadNextToken;
      if Tokenizer.Token.AType = sesDeprecated then
      begin
        ProcessDeprecatedExpression(State, newType);
        ExpectToken([sesSemiColon]);
        ReadNextToken;
      end;
    end;
  end;
end;

function TSE2Parser.MethodDeclaration(State: TSE2ParseState; IsTypeDeclaration: boolean): TSE2Method;
var Method       : TSE2Method;
    pMeth        : TSE2Method;
    Param        : TSE2Parameter;
    Name         : string;
    MinParam     : integer;
    ParamCount   : integer;
    WasForwarded : boolean;
    LibNameConst : TSE2BaseType;

  procedure ParamListDeclaration(Method: TSE2Method);  
  var Param     : TSE2Parameter;
      Params    : TSE2BaseTypeList;
      ParamType : TSE2ParameterType;
      aType     : TSE2Type;
      i         : integer;
  begin
    Params := TSE2BaseTypeList.Create;
    try
      if Tokenizer.Token.AType = sesConst then
      begin
         ParamType := ptConst;
         ReadNextToken;
      end else
      if Tokenizer.Token.AType in [sesVar, sesOut] then
      begin
         ParamType := ptVar;
         ReadNextToken;
      end else
         ParamType := ptDefault;

      ExpectToken([sesIdentifier]);
      while (Tokenizer.Token.AType = sesIdentifier) do
      begin
        Param := TSE2Parameter.Create;
        DeclareType(State, Param, Tokenizer.Token.Value);
        Param.Visibility    := visPublic;
        Param.IsStatic      := False;
        Param.ParameterType := ParamType;
        Params.Add(Param);
        ReadNextToken;

        ExpectToken([sesColon, sesDoublePoint]);

        if Tokenizer.Token.AType = sesColon then
           ReadNextToken;
      end;

      ExpectToken([sesDoublePoint]);
      ReadNextToken;
      if ExpectToken([sesIdentifier]) then
      begin
        aType := TypeExpression(nil);
        //aType := TSE2Type(FindIdentifier(nil, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Type])));
        if aType = nil then
        begin
          RaiseError(petError, 'Unkown identifier: "'+Tokenizer.Token.Value+'"');
          exit;
        end;
        CheckForDeprecated(aType);
        if not FHasError then
        begin
          for i:=0 to Params.Count-1 do
          begin
            Param := TSE2Parameter(Params[i]);
            Param.AType   := aType;
            Param.CodePos := Method.Params.Count;
            ParamCount := ParamCount + 1;
            if WasForwarded then
            begin
              if (Method.Params.Count < ParamCount) then
                 RaiseError(petError, 'The method declaration is different from the original declaration');
              if (Param.ParameterType <> TSE2Parameter(Method.Params[ParamCount-1]).ParameterType) or
                 (Param.AType <> TSE2Parameter(Method.Params[ParamCount-1]).AType) or
                 (not StringIdentical(Param.Name, TSE2Parameter(Method.Params[ParamCount-1]).Name)) then
                 RaiseError(petError, 'The method declaration is different from the original declaration');

            end
            else
              Method.Params.Add(Param);
          end;

          if not WasForwarded then
             Params.OwnsObjs := False;
        end;
      end;
      ReadNextToken;
    finally
      Params.Free;
    end;
  end;

  function MethodHeaderEqual(Meth1, Meth2: TSE2Method): boolean;
  var i      : integer;
      p1, p2 : TSE2Parameter;
      offset : integer;
  begin
    result := False;
    if Meth1 = Meth2 then
       exit;
    result := (
               (Meth1.Params.Count = Meth2.Params.Count) or
               (
                 (Meth1.Params.Count = Meth2.Params.Count - 1) and
                 (Meth1.Parent <> nil) and (Meth2.Parent <> nil)
               )
              )  and
              (Meth1.MethodType = Meth2.MethodType) and
              (Meth1.IsStatic = Meth2.IsStatic) and
              (
                (Meth1.IsVirtual = Meth2.IsVirtual) or
                (Meth1.IsOverload) or
                (Meth2.IsOverload)
              ) and
              (Meth1.IsOverride = Meth2.IsOverride) and
              (Meth1.IsOverload = Meth2.IsOverload);
    if not result then
       exit;
      (*
    result := ((Meth1.ReturnValue = nil) and (Meth2.ReturnValue <> nil)) or
              ((Meth1.ReturnValue <> nil) and (Meth2.ReturnValue = nil));
    if not result then
       exit; *)

    if Meth1.ReturnValue <> nil then
    begin
      result := Meth1.ReturnValue.AType = Meth2.ReturnValue.AType;
      if not result then
         exit;
    end;

    offset := 0;
    if Meth1.Params.Count = Meth2.Params.Count - 1 then
      if (Meth1.Parent <> nil) and (Meth2.Parent <> nil) then
        offset := 1;

    for i:=0 to Meth1.Params.Count-1 do
    begin
      p1 := TSE2Parameter(Meth1.Params[i]);
      p2 := TSE2Parameter(Meth2.Params[i + offset]);

      if (not StringIdentical(p1.Name, p2.Name)) or
         (p1.ParameterType <> p2.ParameterType) or
         (p1.AType <> p2.AType) then
      begin
        result := False;
        exit;
      end;
    end;
  end;

var aVisibility    : TSE2Visibility;
    MethodList     : TSE2BaseTypeList;
    i              : integer;
    DeclareForward : boolean;
    MethodClass    : TSE2Type;

    tmpType        : TSE2Type;
begin
  State.Method     := nil;
  State.TargetType := nil;

  result := nil;

  WasForwarded := False;
  Method := TSE2Method.Create;
  try
    try
      //if State.CurrentOwner <> nil then
      if IsTypeDeclaration then
        ExpectToken([sesFunction, sesProcedure])
      else
        ExpectToken([sesFunction, sesProcedure, sesConstructor, sesDestructor]);
      //else
      //  ExpectToken([sesFunction, sesProcedure]);

      Method.IsStatic    := State.IsStatic;
      Method.IsForwarded := (State.CurrentOwner <> nil) or State.IsInInterface;
      Method.Parent      := State.CurrentOwner;
    

      case Tokenizer.Token.AType of
      sesProcedure   : Method.MethodType := mtProcedure;
      sesFunction    : Method.MethodType := mtFunction;
      sesConstructor : Method.MethodType := mtConstructor;
      sesDestructor  : Method.MethodType := mtDestructor;
      end;

      ReadNextToken;
      if State.CurrentOwner = nil then
         State.AParent    := nil;
      if not IsTypeDeclaration then
      begin
         ExpectToken([sesIdentifier]);
         Name := Tokenizer.Token.Value;
         ReadNextToken;
      end;
      if (Tokenizer.Token.AType in [sesDot]) and (not IsTypeDeclaration) then
      begin
        if State.IsInInterface then
           RaiseError(petError, 'Class methods can not be implemented in the interface part');

        MethodClass := TSE2Class(FindIdentifier(nil, Name, nil, FUnit.Name, TSE2BaseTypeFilter.Create([TSE2Class, TSE2Record])));
        if MethodClass = nil then
        begin
          MethodClass := TSE2Class(FindIdentifier(nil, Name, nil, '', TSE2BaseTypeFilter.Create([TSE2Class, TSE2Record])));
          if MethodClass = nil then
             RaiseError(petError, 'Class name not found: "'+Name+'"');
          if MethodClass is TSE2Class then
            if not TSE2Class(MethodClass).IsPartial then
               RaiseError(petError, 'Class name not found: "'+Name+'"');
        end;

        State.AParent      := MethodClass;
        State.CurrentOwner := MethodClass;
        ReadNextToken;
        ExpectToken([sesIdentifier]);

        Name := Tokenizer.Token.Value;

        pMeth := TSE2Method(FindIdentifier(nil, Name, MethodClass, FUnit.AUnitName, TSE2BaseTypeFilter.Create([TSE2Method])));
        if pMeth = nil then
           RaiseError(petError, 'Class does not have a method named "'+Name+'"')
        else
        if pMeth.Parent <> State.CurrentOwner then
           RaiseError(petError, 'Class does not have a method named "'+Name+'"');

        if (not pMeth.IsForwarded) and (not pMeth.IsOverload) then
           RaiseError(petError, 'The method "'+pMeth.Name+'" has already been declared');

        ReadNextToken;
        State.AParent := nil;
      end;
      if State.IsStatic and (State.CurrentOwner = nil) then
      begin
        RaiseError(petError, 'Static class method does not have an owner');
        exit;
      end;

      if IsTypeDeclaration then
        aVisibility := visPublic
      else
        aVisibility := State.Visibility;
      DeclareForward := False;

      if not IsTypeDeclaration then
      begin
        pMeth := TSE2Method(FindIdentifier(nil, Name, State.CurrentOwner, FUnit.Name, TSE2BaseTypeFilter.Create([TSE2Method])));
        if (pMeth = nil) and (State.CurrentOwner is TSE2Class) then
          if TSE2Class(State.CurrentOwner).IsPartial and not FUnit.IsName(State.CurrentOwner.AUnitName) then
             pMeth := TSE2Method(FindIdentifier(nil, Name, State.CurrentOwner, State.CurrentOwner.AUnitName, TSE2BaseTypeFilter.Create([TSE2Method])));

        if pMeth <> nil then
        begin
          if not pMeth.IsOverload then
          begin
            if (not pMeth.IsForwarded) and (not (pMeth.IsVirtual or pMeth.IsOverride)) and (pMeth.Parent = State.CurrentOwner) then
               RaiseError(petError, 'A method named "'+Name+'" already exists!');

            if (((pMeth.IsStatic <> Method.IsStatic) and ((pMeth.MethodType <> mtConstructor) and (Method.MethodType <> mtConstructor))) or
               (pMeth.MethodType <> Method.MethodType)) and
               (not (pMeth.IsExternal or Method.IsExternal)) then
               RaiseError(petError, 'The method is not equal to the forwarded declaration: ' + pMeth.Name);

            if pMeth.IsForwarded and (pMeth.Parent = State.CurrentOwner) then

            //if pMeth.IsForwarded or (not (not (pMeth.IsVirtual or pMeth.IsOverride or pMeth.IsAbstract)) and (pMeth.Parent = State.CurrentOwner)) then
            begin
              WasForwarded := True;
              FreeAndNil(Method);
              Method := pMeth;
              Method.Parent      := State.CurrentOwner;
              Method.IsForwarded := False;
              aVisibility := Method.Visibility;

              if Method.IsAbstract then
                 RaiseError(petError, 'Abstract methods do not have a implementation');
            end;
          end else
          begin
            DeclareForward := True;
            Method.Parent  := State.CurrentOwner;
          end;
        end;
        DeclareType(State, Method, Name);
        Method.Visibility := aVisibility;
      end;

      if WasForwarded and Method.HasSelfParam then
         MinParam := 1
      else
         MinParam := 0;

      State.AParent := nil;

      if (Tokenizer.Token.AType = sesOpenRound) or (WasForwarded and (Method.Params.Count > MinParam)) then
      begin            
        ParamCount := MinParam;
        ExpectToken([sesOpenRound]);
        ReadNextToken;
        if Tokenizer.Token.AType <> sesCloseRound then
        begin
          while (True) do
          begin
            ParamListDeclaration(Method);
            if Tokenizer.Token.AType <> sesSemiColon then
               break
            else
               ReadNextToken;
          end;
        end;
        ExpectToken([sesCloseRound]);
        ReadNextToken;
        if ParamCount <> Method.Params.Count then
           RaiseError(petError, 'The method declaration is different from the original declaration');
      end;

      case Method.MethodType of
      mtProcedure,
      mtDestructor    :
          begin
            if not IsTypeDeclaration then
            begin
              ExpectToken([sesSemiColon]);
              ReadNextToken;
            end;
          end;
      mtConstructor   :
          begin
            ExpectToken([sesSemiColon]);
            ReadNextToken;
            if Method.ReturnValue = nil then
            begin
              Method.ReturnValue := TSE2Variable.Create;
              DeclareType(State, Method.ReturnValue, '!result');
              Method.ReturnValue.AType := State.CurrentOwner;
              Method.ReturnValue.Visibility := visPrivate;
              if Method.ReturnValue.AType = nil then
              begin
                RaiseError(petError, 'Internal error: constructor object not found');
                exit;
              end;
            end;
          end;
      mtFunction      :
          begin
            ExpectToken([sesDoublePoint]);
            ReadNextToken;
            if ExpectToken([sesIdentifier]) then
            begin
              if Method.ReturnValue = nil then
              begin
                Method.ReturnValue := TSE2Variable.Create;
                DeclareType(State, Method.ReturnValue, 'result');
                Method.ReturnValue.InlineDoc := 'The return value of ' + Method.GetStrongName;
                Method.ReturnValue.IsStatic := False;

                Method.ReturnValue.AType    := TypeExpression(nil);
                //Method.ReturnValue.AType    := TSE2Type(FindIdentifier(nil, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Type])));
                if Method.ReturnValue.AType = nil then
                begin
                  RaiseError(petError, 'Unkown type as return value: "'+Tokenizer.Token.Value+'"');
                  exit;
                end;
                CheckForDeprecated(Method.ReturnValue.AType);
              end else
              begin
                tmpType := TypeExpression(nil);
                if tmpType = nil then
                   RaiseError(petError, 'Type not found');
                if Method.ReturnValue.AType <> TypeExpression(nil) then
                   RaiseError(petError, Format('Return value type of "%s" not equal to the forwared declaration: found "%s", required "%s"', [Method.GetStrongName, tmpType.GetStrongName, Method.ReturnValue.AType.GetStrongName]));
              end;
            end;
            ReadNextToken;
            if not IsTypeDeclaration then
            begin
              ExpectToken([sesSemiColon]);
              ReadNextToken;
            end;
          end;
      end;

      if DeclareForward then
      begin
        Method.IsOverload := True;
        GetOverloadedMethods(State, Method, MethodList);
        try
          pMeth := nil;
          for i:=0 to MethodList.Count-1 do
          begin
            if MethodHeaderEqual(Method, TSE2Method(MethodList[i])) then
            begin
              pMeth := TSE2Method(MethodList[i]);
              break;
            end;
          end;

          if pMeth <> nil then
          begin
            if not pMeth.IsForwarded then
               RaiseError(petError, 'Method "'+Method.Name+'" is already fully declared');

            if pMeth.IsExternal then
               RaiseError(petError, 'The method header is already declared');

            WasForwarded := True;
            Method.Free;
            Method := pMeth;
            Method.IsForwarded := False;
          end;
        finally
          MethodList.Free;
        end;
      end;

      if IsTypeDeclaration then
      begin
        ExpectToken([sesSemiColon, sesOf]);
        if Tokenizer.Token.AType = sesOf then
        begin
          ReadNextToken;
          ExpectToken([sesObject]);
          ReadNextToken;
          Method.AddHasSelfParam := True;
        end;
        ExpectToken([sesSemiColon]);
        ReadNextToken;
      end;

      if Method.HasSelfParam and not WasForwarded then
      begin
        Param := TSE2Parameter.Create;

        Param.Name       := Name;
        Param.AUnitName  := FUnit.Name;
        Param.DeclLine   := Tokenizer.Reader.Line;
        Param.DeclPos    := Tokenizer.Reader.Position;
        Param.Visibility := visPublic;
        Param.AType      := State.CurrentOwner;
        Param.InlineDoc  := 'The current class instance, calling ' + Method.GetStrongName;

        if Method.IsStatic and (Method.MethodType <> mtConstructor) then
           Param.Name := '!Self'
        else
           Param.Name := 'Self';

        if State.CurrentOwner is TSE2Class then
          if TSE2Class(State.CurrentOwner).IsHelper then
             Param.AType := TSE2Type( TSE2Class(State.CurrentOwner).InheritFrom );
        Param.IsStatic    := False;
        Method.Params.Insert(0, Param);
      end;

      while (Tokenizer.Token.AType in [sesExternal, sesForward, sesVirtual, sesAbstract, sesOverride, sesOverload, sesExport]) and
            (True) do
      begin
        if IsTypeDeclaration then
           ExpectToken([sesExternal]);

        if WasForwarded and (not (Tokenizer.Token.AType in [sesExport])) then
           RaiseError(petError, 'Modificator not allowed here, because method modificators has already been declared in first declaration');
        case Tokenizer.Token.AType of
        sesExternal :
            begin
              if ((Method.IsForwarded and not State.IsInInterface)) and (State.CurrentOwner = nil) then
                 RaiseError(petError, 'Forwarded methods can not be external')
              else
              if Method.IsAbstract then
                 RaiseError(petError, 'Abstract methods can not be external')
              else
              if Method.IsOverride then
                 RaiseError(petError, '"'+Method.GetStrongName+'" external must be set before override')
              else
              if Method.AddHasSelfParam then
                 RaiseError(petError, 'External modificator not allowed for instanced method " ' + Method.GetStrongName + '"')
              else
              begin
                 Method.IsExternal := True;
                 Method.IsForwarded := False;
              end;
              ReadNextToken;
              if Tokenizer.Token.AType in [sesIdentifier, sesString] then
              begin
                if Method.Parent <> nil then
                begin
                  if not Method.IsStatic then
                     RaiseError(petError, 'External library methods must be static');
                  if Method.HasSelfParam then
                  begin
                    Method.Params.Delete(0);
                  end;
                end;
                if Tokenizer.Token.AType = sesString then
                begin
                  if Trim(Tokenizer.Token.Value) = '' then
                     RaiseError(petError, 'Library name can not be empty');
                  Method.ExternalLib := Tokenizer.Token.Value;
                end else
                begin
                  LibNameConst := ConstExpression(nil);

                  if not (TSE2Type(TSE2Constant(LibNameConst).AType.InheritRoot).AType in
                         [btString, btAnsiString, btUTF8String, btWideString, btPAnsiChar, btPWideChar, btPChar]) then
                     RaiseError(petError, 'Identifier "'+Tokenizer.Token.Value+'" must be a string constant');


                  if Trim(TSE2Constant(LibNameConst).Value) = '' then
                     RaiseError(petError, 'Library name can not be empty');
                  Method.ExternalLib := TSE2Constant(LibNameConst).Value;
                end;
                ReadNextToken;
                if Tokenizer.Token.AType = sesIdentifier then
                begin
                  if StringIdentical(Tokenizer.Token.Value, 'name') then
                  begin
                     ReadNextToken;
                     ExpectToken([sesIdentifier, sesString]);

                     if Tokenizer.Token.AType = sesString then
                        Method.ExternalName := Tokenizer.Token.Value
                     else
                     begin
                       LibNameConst := ConstExpression(nil);

                       if not (TSE2Type(TSE2Constant(LibNameConst).AType.InheritRoot).AType in
                              [btString, btAnsiString, btUTF8String, btWideString, btPAnsiChar, btPWideChar, btPChar]) then
                          RaiseError(petError, 'Identifier "'+Tokenizer.Token.Value+'" must be a string constant');

                       Method.ExternalName := TSE2Constant(LibNameConst).Value;
                     end;
                     Method.ExternalIndex := -1;
                  end else
                  if StringIdentical(Tokenizer.Token.Value, 'index') then
                  begin
                     ReadNextToken;
                     ExpectToken([sesInteger]);
                     if (Tokenizer.Token.AsInteger < 0) or (Tokenizer.Token.AsInteger > High(Word)) then
                        RaiseError(petError, 'Method index must be between 0 and '+IntToStr(High(Word)));

                     Method.ExternalIndex := Tokenizer.Token.AsInteger;
                  end else
                     RaiseError(petError, '"name" or "index" expected, but found "'+Tokenizer.Token.Value+'" instead');

                  ReadNextToken;
                end else
                begin
                  Method.ExternalName := Method.Name;
                  Method.ExternalIndex := -1;
                end;
              end else
              begin
                if State.CurrentOwner is TSE2Class then
                   if TSE2Type(State.CurrentOwner.InheritRoot) <> TSE2Class(FindIdentifier(nil, C_SE2TExternalObjectName, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Class]))) then
                      RaiseError(petError, 'External method "'+Method.Name+'" must be declared in classes which inherits from TExternalObject');
              end;
            end;
        sesForward :
            begin
              if Method.IsExternal then
                 RaiseError(petError, 'External methods can not be forwarded')
              else
              if State.CurrentOwner <> nil then
                 RaiseError(petError, 'Methods in classes are forwarded automatically')
              else
              if WasForwarded then
                 RaiseError(petError, 'This method is already forwarded')
              else
                 Method.IsForwarded := True;

              ReadNextToken;
            end;
        sesVirtual :
            begin
              if State.CurrentOwner = nil then
                 RaiseError(petError, 'Normal methods can not be virtual')
              else
              if Method.IsStatic and (Method.MethodType <> mtConstructor) then
                 RaiseError(petError, 'Static methods can not be virtual')
              else
              if State.CurrentOwner is TSE2Record then
                 RaiseError(petError, 'Record methods can not be virtual')
              else
              begin
                if State.CurrentOwner is TSE2Class then
                  if TSE2Class(State.CurrentOwner).IsHelper then
                    RaiseError(petError, 'Methods of helper classes can not be virtual');

                 Method.IsVirtual := True;
                 Method.Used := True;

                 Method.DynamicIndex := TSE2Class(State.CurrentOwner).DynMethods;
                 TSE2Class(State.CurrentOwner).DynMethods := TSE2Class(State.CurrentOwner).DynMethods + 1;
              end;

              ReadNextToken;
            end;
        sesAbstract :
            begin
              if not Method.IsVirtual then
                 RaiseError(petError, 'Abstract methods must be virtual')
              else
              if State.CurrentOwner = nil then
                 RaiseError(petError, 'Normal methods can not be abstract')
              else
              if Method.MethodType in [mtConstructor, mtDestructor] then
                 RaiseError(petError, 'Constructors/destructors can not be abstract')
              else
              begin
                if State.CurrentOwner is TSE2Class then
                  if TSE2Class(State.CurrentOwner).IsHelper then
                    RaiseError(petError, 'Methods of helper classes can not be abstract');

                 Method.IsAbstract := True;
                 Method.Used := True;

                 Method.DynamicIndex := TSE2Class(State.CurrentOwner).DynMethods;
                 TSE2Class(State.CurrentOwner).DynMethods := TSE2Class(State.CurrentOwner).DynMethods + 1;
              end;

              ReadNextToken;
            end;
        sesOverride :
            begin
              if Method.IsStatic and (not (Method.MethodType in [mtConstructor, mtDestructor])) then
                 RaiseError(petError, 'Static methods can not be overwritten')
              else
              begin
                if not (State.CurrentOwner is TSE2Class) then
                   RaiseError(petError, 'Only class methods can be declared as override')
                else
                begin
                  if State.CurrentOwner is TSE2Class then
                    if TSE2Class(State.CurrentOwner).IsHelper then
                      RaiseError(petError, 'Methods of helper classes can not be virtual');

                  Method.IsAbstract := False;
                  Method.IsOverride := True;

                  if not Method.IsExternal then
                  begin
                    Method.Used := True;

                    pMeth := GetOverwrittenMethod(Method);
                    if pMeth = nil then
                       RaiseError(petError, 'Method to override not found');

                    Method.DynamicIndex := pMeth.DynamicIndex;
                  end;
                end;
              end;

              ReadNextToken;
            end;
        sesOverload :
            begin
              Method.IsOverload := True;
              ReadNextToken;
            end;
        sesExport :
            begin
              Method.IsExport := True;
              ReadNextToken;
            end;
        else
            ReadNextToken;
        end;
        ExpectToken([sesSemiColon]);
        ReadNextToken;
      end;

      if Tokenizer.Token.AType in [sesCdecl, sesPascal, sesRegister, sesSafecall, sesStdCall] then
      begin
        if not Method.IsExternal then
           RaiseError(petError, 'Call convention can only be set for external methods')
        else
        begin
          case Tokenizer.Token.AType of
          sesCdecl     : Method.CallConvention := callCdecl;
          sesPascal    : Method.CallConvention := callPascal;
          sesRegister  : Method.CallConvention := callRegister;
          sesSafecall  : Method.CallConvention := callSafecall;
          sesStdCall   : Method.CallConvention := callStdcall;
          end;
        end;
        ReadNextToken;
        ExpectToken([sesSemiColon]);
        ReadNextToken;
      end;

      if Tokenizer.Token.AType = sesDeprecated then
      begin
        ProcessDeprecatedExpression(State, Method);
        ExpectToken([sesSemiColon]);
        ReadNextToken;
      end;

      if Method.MethodType = mtConstructor then
        if not Method.IsExternal then
          if State.CurrentOwner <> nil then
             if TSE2Type(State.CurrentOwner.InheritRoot) = TSE2Class(FindIdentifier(nil, C_SE2TExternalObjectName, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Class]))) then
               RaiseError(petError, 'Constructors of external classes must be external');


      if Method.IsExternal then
      begin
        if Method.ExternalLib <> '' then
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_LOADEX, ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALLEX(0, 0{MakeCallExOptionMask(Method)}), Method.GenLinkerName));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));
      end;       

      case Method.MethodType of
      mtConstructor : Method.Used := (not Method.IsExternal) or Method.Used;
      mtDestructor  : Method.Used := (not Method.IsExternal) or Method.Used;
      end;

      {
      if not FHasError then
        // if "WasForwarded" = True, Method is already in the list
        if not WasForwarded then
          if not IsTypeDeclaration then
            FUnit.ElemList.Add(Method);
      }
      if not WasForwarded then
        if not IsTypeDeclaration then
          FUnit.ElemList.Add(Method);
    except
      if not WasForwarded then
      begin
        FUnit.ElemList.Delete(Method);
        Method.Free;
        Method := nil;
      end;
    end
  finally               {
    if FHasError and (not WasForwarded) then
    begin
      FUnit.ElemList.Delete(Method);
      Method.Free;
      Method := nil;
    end;         }
      //if not WasForwarded then
       //FreeAndNil(Method);
    result := Method;
  end;
end;

procedure TSE2Parser.StatementSquence(State: TSE2ParseState; Method: TSE2Method);
var oldStackSize : integer;
    oldRecords   : integer;
    oldArrays    : integer;

  procedure DoStatement;
  var oldStack: integer;
  begin
    oldStack := State.StackSize;
    try
      Statement(State, Method);
    except
      on E: ESE2ParserError do
      begin
        State.StackSize    := oldStack;
        State.IsExpression := False;
        State.LastVariable := nil;
        State.LastProperty := nil;
        State.LastMethod   := nil;
        State.LastTargetVar := nil;
        State.LastSetVariable := nil;
        while not (Tokenizer.Token.AType in [sesSemiColon, sesEnd, sesNone, sesUnknown]) do
          ReadNextToken;
      end;
    end;
  end;

begin
  State.Method     := Method;
  State.AParent    := nil;
  State.TargetType := nil;
  State.LastSetVariable := nil;

  oldArrays    := State.ArraysCreated;
  oldRecords   := State.RecordsCreated;
  oldStackSize := State.StackSize;
  DoStatement;
  while (Tokenizer.Token.AType = sesSemiColon) {and (not FHasError)} do
  begin
    if oldRecords < State.RecordsCreated then
    begin
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_DEL_RECS(State.RecordsCreated - oldRecords), ''));
    end;
    if oldArrays < State.ArraysCreated then
    begin
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_DEL_ARRS(State.ArraysCreated - oldArrays), ''));
    end;
    State.ArraysCreated  := oldArrays;
    State.RecordsCreated := oldRecords;

    ReadNextToken;
    DoStatement;
  end;

  if oldRecords < State.RecordsCreated then
  begin
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_DEL_RECS(State.RecordsCreated - oldRecords), ''));
  end;
  if oldArrays < State.ArraysCreated then
  begin
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_DEL_ARRS(State.ArraysCreated - oldArrays), ''));
  end;
  State.ArraysCreated  := oldArrays;
  State.RecordsCreated := oldRecords;

  if not FHasError then
    if oldStackSize <> State.StackSize then
       RaiseError(petError, 'Internal error: calculated stack size is invalid. State: '+IntToStr(State.StackSize) + ' - expected: ' + IntToStr(oldStackSize));
end;

procedure TSE2Parser.Statement(State: TSE2ParseState; Method: TSE2Method);
begin
  State.Method     := Method;
  State.TargetType := nil;
  State.AParent    := nil;

  case Tokenizer.Token.AType of
  sesBegin :
      begin
        ReadNextToken;
        StatementSquence(State, Method);
        ExpectToken([sesEnd]);
        ReadNextToken;
      end;
  sesIf :
      begin
        IfStatement(State, Method);
      end;
  sesCase :
      begin
        CaseStatement(State, Method);
      end;
  sesRepeat :
      begin
        RepeatStatement(State, Method);
      end;
  sesWhile :
      begin
        WhileStatement(State, Method);
      end;
  sesFor :
      begin
        ForStatement(State, Method);
      end;
  sesContinue, sesBreak, sesExit :
      begin
        if State.IsInFinallyBlock then
           RaiseError(petError, '''continue'', ''break'' or ''exit'' is not allowed here');
        LoopFlowStatement(State, Method);
      end;
  sesTry :
      begin
        TryStatement(State, Method);
      end;
  sesUnit, sesIdentifier :
      begin
        IdentifierStatement(State, Method);
      end;
  sesInherited :
      begin
        if InheritedExpression(State, Method) <> nil then
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
          State.DecStack;
        end;
      end;
  sesRaise :
      begin
        if ExceptionCreation(State, Method) then
        begin
          // push call stack
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(0, False), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_INCP(4, btString, False), ''));

          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btString), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_STACK, ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_DECP(-1), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
        end;

        // raise
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_INTER, ''));

        State.DecStack;
      end;
  sesOn :
      begin
        if not State.IsInExceptBlock then
           RaiseError(petError, 'on not allowed here');

        OnExceptStatement(State, Method);
      end;
  end;
end;

function TSE2Parser.ExceptionCreation(State: TSE2ParseState;
  Method: TSE2Method): boolean;
var Ident: TSE2Type;
    Target: TSE2Type;
    iPos, iLine : integer;
begin
  ExpectToken([sesRaise]);
  State.IsExpression := True;
  ReadNextToken;

  Target := GetScriptObjectType;
  State.TargetType := Target;
  State.AParent    := nil;

  iPos := Tokenizer.Reader.Position;
  iLine := Tokenizer.Reader.Line;

  Ident := Expression(State, Method, Target);
  if not IsCompatible(Target, Ident, sesNone, False, Self) then
     RaiseError(petError, GenIncompatibleExpression(Ident, Target), iPos, iLine);

  result := IsScriptExceptionType(Ident);

  ConvertVariableCode(State, Method, Ident, Target);
  State.IsExpression := False;
end;

function TSE2Parser.MethodCall(State: TSE2ParseState; Method,
  CallMethod: TSE2Method; UseBrackets: boolean = False; LastParamSetter: TSE2SetterEvent = nil;
  AllowDynamic: boolean = True; ParentType: TSE2BaseType = nil;
  NoMagicMemory: boolean = False; AlternativeMethod: TSE2Method = nil): TSE2Type;
var i          : integer;
    iStart     : integer;
    iStop      : integer;
    aType      : TSE2BaseType;
    OldTarget  : TSE2Type;
    MethodList : TSE2BaseTypeList;

    tokOpen    : TSE2TokenType;
    tokClose   : TSE2TokenType;

    ParamDecl  : TSE2BaseTypeList;
    wasExpr    : boolean;
    //iPushPos   : integer;
{$IFDEF SEII_FPC}
  {$WARNINGS OFF}
{$ENDIF}
    iStackSize : integer;
{$IFDEF SEII_FPC}
  {$WARNINGS OFF}
{$ENDIF}

  procedure AddMethodPointerStack(CallMethod: TSE2Method; Index: integer);
  begin
    if CallMethod.HasSelfParam then
    begin
      if CallMethod.IsExternal and CallMethod.IsStatic then
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btPointer), ''), Index, Index > -1);
        if Index > -1 then
           IncreaseMethodStackPositions(State, Method, index + 1, Method.OpCodes.Count-1, -1, iStackSize);
        State.IncStack;
      end else
      if CallMethod.IsExternal and ((CallMethod.ReturnValue <> nil) or (CallMethod.IsOverload or (AlternativeMethod <> nil) )) then
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''), Index, Index > -1);
        if Index > -1 then
           IncreaseMethodStackPositions(State, Method, index + 1, Method.OpCodes.Count-1, -1, iStackSize);
        State.IncStack;
      end else
      if (CallMethod.ReturnValue <> nil) or (CallMethod.IsOverload or (AlternativeMethod <> nil)) then
      begin
        if CallMethod.IsStatic then
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(0, False), ''), Index, Index > -1)
        else
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''), Index, Index > -1);
          
        if Index > -1 then
           IncreaseMethodStackPositions(State, Method, index + 1, Method.OpCodes.Count-1, -1, iStackSize);
        State.IncStack;
      end else
      if (CallMethod.IsStatic) and (CallMethod.ReturnValue = nil) and (not CallMethod.IsExternal) then
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btPointer), ''), Index, Index > -1);
        if Index > -1 then
           IncreaseMethodStackPositions(State, Method, index + 1, Method.OpCodes.Count-1, -1, iStackSize);
        State.IncStack;
      end;
    end
  end;

var ResultCodeIndex  : integer;
    wasStaticPointer : boolean;
begin
  State.AParent     := nil;
  State.ParamMethod := CallMethod;
  if UseBrackets then
  begin
    tokOpen  := sesOpenBracket;
    tokClose := sesCloseBracket;
  end else
  begin
    tokOpen  := sesOpenRound;
    tokClose := sesCloseRound;
  end;

  //  Stack
  //  --------------------------------------------------
  //  Return Value  |  Parameters 0..n | Return Position
  //  --------------------------------------------------

  // if the Call Method is not overloaded, declare as used
  // Otherwise, the really called method will be set as used
  // The real called Method is determinated by the
  // Parameters.

  OldTarget := State.TargetType;
  State.TargetType := nil;

  case CallMethod.MethodType of
  mtProcedure   : result := nil;
  mtFunction    : result := CallMethod.ReturnValue.AType;
  mtConstructor : result := TSE2Type(CallMethod.Parent);
  mtDestructor  : result := nil;
  else begin
         result := nil;
         RaiseError(petError, 'Internal error: unknown method type');
         exit;
       end;
  end;

  if CallMethod.IsStatic then
    if not State.NoStaticPointer then
    begin
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      State.DecStack;
    end;

  ResultCodeIndex := Method.OpCodes.Count;
  // Add the return value as a new stack element
  if (result <> nil) or (CallMethod.IsOverload) or (AlternativeMethod <> nil) then
  begin
    if (result <> nil) then
       GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(TSE2Type(result.InheritRoot).AType), ''))
    else
       GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btS32), ''));
     
    if not CallMethod.IsOverload then
    begin
      if result is TSE2Record then
      begin
        State.RecordsCreated := State.RecordsCreated + 1;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_MAKE(0, 0), 'META_' + result.GenLinkerName));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_MARK_DEL, ''));
      end else
      if result is TSE2Array then
      begin
        State.ArraysCreated := State.ArraysCreated + 1;
        if TSE2Array(result).IsDynamic then
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_MAKE(0, 0), 'META_' + result.GenLinkerName));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_MARK_DEL, ''));
        end else
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_SFL(TSE2Array(result).ArrayCount, 0), 'META_' + result.GenLinkerName));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_MARK_DEL, ''));
        end;
      end;
    end else
    begin
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.NOOP, ''));
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.NOOP, ''));
    end;
    State.IncStack;
  end;


  iStop := CallMethod.Params.Count;
  if Assigned(LastParamSetter) then
     iStop := iStop - 1;

  iStart := 0;  
  if CallMethod.HasSelfParam then
     iStart := 1;
     
  // if method is a class method and the method points to an external method
  // we must push a temporary pointer to the stack to make the method
  // call compatible to delphi
  //if not CallMethod.IsOverload then
  begin
    AddMethodPointerStack(CallMethod, -1);
  end(* else
  begin
    iPushPos   := Method.OpCodes.Count;
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.NOOP, ''));
    iStackSize := State.StackSize;
  end*);


  MethodList := nil;
  try
    if CallMethod.IsOverload then
    begin
      GetOverloadedMethods(State, CallMethod, MethodList);
      ParamDecl := TSE2BaseTypeList.Create;
      try
        if CallMethod.HasSelfParam then
           ParamDecl.Add(TSE2ParamExpression.Create(TSE2Type(CallMethod.Parent), nil, -1));

        wasStaticPointer := State.NoStaticPointer;
        if Tokenizer.Token.AType = tokOpen then
        begin
          State.ParamIndex := 0;
          ExpectToken([tokOpen]);
          ReadNextToken;


          while not (Tokenizer.Token.AType in [tokClose]) do
          begin
            State.LastVariable := nil;
            State.ParamMethod := CallMethod;
            wasExpr := State.IsExpression;
            State.IsExpression := True;
            aType := Expression(State, Method, nil);
            State.IsExpression := wasExpr;

            if aType = nil then
               RaiseError(petError, 'Expression does not return a value');

            ParamDecl.Add(TSE2ParamExpression.Create( TSE2Type(aType), State.LastVariable, Method.OpCodes.Count));

            State.ParamIndex := ParamDecl.Count;
            State.ParamMethod := CallMethod;
            ExpectToken([tokClose, sesColon]);
            if Tokenizer.Token.AType = sesColon then
            begin
              ReadNextToken;
              if Tokenizer.Token.AType in [tokClose] then
                 RaiseError(petError, 'Unexpected symbol: "'+TSE2TokenName[tokClose]+'"');
            end;
          end;

          State.NoStaticPointer := wasStaticPointer;
          CallMethod := FindMatchingMethod(State, Method, MethodList, ParamDecl, CallMethod.HasSelfParam);
          if CallMethod = nil then
          begin
            RaiseError(petError, 'No method matches with the given parameter types');
            exit;
          end;



          ExpectToken([tokClose]);
          ReadNextToken;
        end else
        begin
          State.NoStaticPointer := wasStaticPointer;
          CallMethod := FindMatchingMethod(State, Method, MethodList, ParamDecl, CallMethod.HasSelfParam);
          if CallMethod = nil then
          begin
            RaiseError(petError, 'No method matches with the given parameter types');
            exit;
          end;
        end;
      finally
        ParamDecl.Free;
      end;

      case CallMethod.MethodType of
      mtProcedure   : result := nil;
      mtFunction    : result := CallMethod.ReturnValue.AType;
      mtConstructor : result := TSE2Type(CallMethod.Parent);
      mtDestructor  : result := nil;
      end;

      if result <> nil then
      begin
        if Method.OpCodes[ResultCodeIndex].OpCode.OpCode = soSTACK_INC then
        begin
          PSE2OpSTACK_INC(Method.OpCodes.Items[ResultCodeIndex].OpCode).AType := TSE2Type(result.InheritRoot).AType;

          if result is TSE2Record then
          begin
            State.RecordsCreated := State.RecordsCreated + 1;
            Method.OpCodes.Items[ResultCodeIndex + 1].ChangeOpCode(TSE2OpCodeGen.REC_MAKE(0, 0), 'META_' + result.GenLinkerName);
            Method.OpCodes.Items[ResultCodeIndex + 2].ChangeOpCode(TSE2OpCodeGen.REC_MARK_DEL, '');
          end else
          if result is TSE2Array then
          begin
            State.ArraysCreated := State.ArraysCreated + 1;
            if TSE2Array(result).IsDynamic then
            begin
              Method.OpCodes.Items[ResultCodeIndex + 1].ChangeOpCode(TSE2OpCodeGen.ARR_MAKE(0, 0), 'META_' + result.GenLinkerName);
              Method.OpCodes.Items[ResultCodeIndex + 2].ChangeOpCode(TSE2OpCodeGen.ARR_MARK_DEL, '');
            end else
            begin
              Method.OpCodes.Items[ResultCodeIndex + 1].ChangeOpCode(TSE2OpCodeGen.ARR_SFL(TSE2Array(result).ArrayCount, 0), 'META_' + result.GenLinkerName);
              Method.OpCodes.Items[ResultCodeIndex + 2].ChangeOpCode(TSE2OpCodeGen.ARR_MARK_DEL, '');
            end;
          end;
        end;
      end;

      //AddMethodPointerStack(CallMethod, iPushPos);


    end else
    begin
      wasStaticPointer := State.NoStaticPointer;
      // Add the parameters to the stack
      if (iStop > iStart) or (Tokenizer.Token.AType = tokOpen) then
      begin
        State.ParamMethod := CallMethod;
        State.ParamIndex  := 0;
        if CallMethod.Params[iStart] <> nil then
           State.TargetType  := TSE2Parameter(CallMethod.Params[iStart]).AType;
        ExpectToken([tokOpen]);
        ReadNextToken;

        for i:=iStart to iStop-1 do
        begin
          State.ParamMethod := CallMethod;
          State.ParamIndex  := i - iStart;
          State.TargetType  := TSE2Parameter(CallMethod.Params[i]).AType;

          case TSE2Parameter(CallMethod.Params[i]).ParameterType of
          ptDefault, ptConst :
              begin
                wasExpr := State.IsExpression;
                State.IsExpression := True;
                aType := Expression(State, Method, TSE2Parameter(CallMethod.Params[i]).AType);
                State.IsExpression := wasExpr;
                if aType = nil then
                   RaiseError(petError, 'Expression does not return a value');

                State.ParamIndex  := i - iStart + 1;
                State.ParamMethod := CallMethod;

                if State.LastVariable <> nil then
                  if State.LastVariable.Parent <> nil then
                     GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_UNREF, ''));

                if not IsCompatible(TSE2Parameter(CallMethod.Params[i]).AType, TSE2Type(aType), sesNone, False, Self) then
                begin
                  RaiseError(petError, 'Expression is not compatible to the parameter "'+CallMethod.Params[i].Name+'"');
                  exit;
                end;

                ConvertVariableCode(State, Method, TSE2Type(aType), TSE2Parameter(CallMethod.Params[i]).AType);
              end;
          ptVar :
              begin
                State.ParamMethod := CallMethod;
                State.ParamIndex  := i - iStart;    (*
                ExpectToken([sesIdentifier]);
                aType := FindIdentifier(Method, Tokenizer.Token.Value, nil, '',
                              TSE2BaseTypeFilter.Create([TSE2Parameter, TSE2Variable]));
                if aType = nil then
                begin
                  RaiseError(petError, 'Unkown identifier: "'+Tokenizer.Token.Value+'"');
                  exit;
                end;                                  *)

                ExpectToken([sesIdentifier]);
                State.LastTargetVar := nil;
                wasExpr := State.IsExpression;
                State.IsExpression  := False;
                aType := IdentifierExpression(State, Method, nil, False, True);
                State.IsExpression := wasExpr;
                if aType = nil then
                   RaiseError(petError, 'Unknown identifier: "'+Tokenizer.Token.Value+'"');

                if State.LastTargetVar = nil then
                   RaiseError(petError, 'Only variables can be var parameters');


                if State.LastTargetVar is TSE2Parameter then
                  if TSE2Parameter(State.LastVariable).ParameterType = ptConst then
                     RaiseError(petError, 'Constant parameters can not allowed as var parameters');

                if TSE2Variable(State.LastTargetVar).AType <> TSE2Parameter(CallMethod.Params[i]).AType then
                   RaiseError(petError, Format('var parameter type "%s" does not match with the given type "%s"',
                              [TSE2Parameter(CallMethod.Params[i]).AType.GetStrongName,
                               TSE2Variable(State.LastTargetVar).AType.GetStrongName]));

                //PushVarToStack(State, Method, TSE2Variable(State.LastTargetVar), True);


                State.ParamIndex  := i - iStart + 1;
                State.ParamMethod := CallMethod;
              end;
          end;
          State.TargetType := nil;
          if i < iStop-1 then
          begin
            ExpectToken([sesColon]);
            ReadNextToken;
          end;
        end;

        ExpectToken([tokClose]);
        ReadNextToken;

      end;
      State.NoStaticPointer := wasStaticPointer;
    end;

    if AlternativeMethod <> nil then
    begin
      if Tokenizer.Token.AType <> sesBecomes then
      begin
        LastParamSetter := nil;

        if AlternativeMethod.ReturnValue <> nil then
           result := AlternativeMethod.ReturnValue.AType
        else
           result := nil;


        if result <> nil then
        begin
          if Method.OpCodes[ResultCodeIndex].OpCode.OpCode = soSTACK_INC then
          begin
            PSE2OpSTACK_INC(Method.OpCodes.Items[ResultCodeIndex].OpCode).AType := TSE2Type(result.InheritRoot).AType;

            if result is TSE2Record then
            begin
              State.RecordsCreated := State.RecordsCreated + 1;
              Method.OpCodes.Items[ResultCodeIndex + 1].ChangeOpCode(TSE2OpCodeGen.REC_MAKE(0, 0), 'META_' + result.GenLinkerName);
              Method.OpCodes.Items[ResultCodeIndex + 2].ChangeOpCode(TSE2OpCodeGen.REC_MARK_DEL, '');
            end else
            if result is TSE2Array then
            begin
              State.ArraysCreated := State.ArraysCreated + 1;
              if TSE2Array(result).IsDynamic then
              begin
                Method.OpCodes.Items[ResultCodeIndex + 1].ChangeOpCode(TSE2OpCodeGen.ARR_MAKE(0, 0), 'META_' + result.GenLinkerName);
                Method.OpCodes.Items[ResultCodeIndex + 2].ChangeOpCode(TSE2OpCodeGen.ARR_MARK_DEL, '');
              end else
              begin
                Method.OpCodes.Items[ResultCodeIndex + 1].ChangeOpCode(TSE2OpCodeGen.ARR_SFL(TSE2Array(result).ArrayCount, 0), 'META_' + result.GenLinkerName);
                Method.OpCodes.Items[ResultCodeIndex + 2].ChangeOpCode(TSE2OpCodeGen.ARR_MARK_DEL, '');
              end;
            end;
          end;
        end;

        CallMethod := AlternativeMethod;

      end;
    end;

    wasStaticPointer := State.NoStaticPointer;
    if Assigned(LastParamSetter) then
       LastParamSetter(Self, State, Method, TSE2Parameter(CallMethod.Params[iStop]).AType, nil);
    State.NoStaticPointer := wasStaticPointer;

    if (CallMethod.AUnitName <> '') and (CallMethod.Name <> '') and (not (CallMethod.IsMethodType)) then
        {$IFDEF SEII_SMART_LINKING}
        Method.UsedMethods.Add(CallMethod);
        {$ELSE}
        CallMethod.Used := True;
        {$ENDIF}

    CheckForDeprecated(CallMethod);

    // Add the return position to the stack
    //=========== OLD CODE : BEGIN
    //GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btReturnAddress), ''));
    //State.IncStack;
    //GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetInt(Method.OpCodes.Count + 1), ''));
    //=========== OLD CODE : END


    if (CallMethod.MethodType = mtConstructor) then
    begin
      if not (CallMethod.Parent is TSE2Class) then
         RaiseError(petError, 'Internal error: constructor class not found');

      if (ParentType is TSE2Class) and (CallMethod.Parent <> ParentType) then
      begin
        if (not CallMethod.IsExternal) and (not NoMagicMemory)  then
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_CREATE(CallMethod.Params.Count - 1, 0), 'META_' + TSE2Class(ParentType).GenLinkerName));
        result := TSE2Class(ParentType);
      end else
      begin
        if (not CallMethod.IsExternal) and (not NoMagicMemory) then
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_CREATE(CallMethod.Params.Count - 1, 0), 'META_' + TSE2Class(CallMethod.Parent).GenLinkerName ));
        result := TSE2Class(CallMethod.Parent);
      end;

      if (ParentType is TSE2Class) and (ParentType.InheritRoot = GetExternalObjectType) then
         if CallMethod.IsExternal then
            result := TSE2Class(CallMethod.Parent);
    end;

    if (CallMethod.MethodType in [mtConstructor, mtDestructor]) then 
      if (CallMethod.Parent is TSE2Class) then
        if TSE2Class(CallMethod.Parent).IsHelper then
           RaiseError(petError, 'Helper classes can not be created directly');

    //=========== NEW CODE : BEGIN
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(Method.OpCodes.Count + 2, 0), ''));
    State.IncStack;
    //=========== NEW CODE : END

    if State.NoStaticPointer then
      if not CallMethod.IsStatic then
         RaiseError(petError, 'Method is not a static');

    if CallMethod.IsMethodType then
    begin
      PSE2OpFLOW_PUSHRET(Method.OpCodes[Method.OpCodes.Count-1].OpCode)^.Position :=
         PSE2OpFLOW_PUSHRET(Method.OpCodes[Method.OpCodes.Count-1].OpCode)^.Position + 1;
      i := CallMethod.Params.Count;
      if not CallMethod.HasSelfParam then
      begin
         i := i + 1;
         if CallMethod.ReturnValue <> nil then
            i := i + 1;
      end;
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-i, False), ''));
      State.IncStack;
      if CallMethod.IsExternal then
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.META_PUSH(0), CallMethod.GenLinkerName()));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALLPEX, ''));
        // Return value is still in the stack, because CALLPEX does not use it, so manually remove it
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      end else
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALLPTR, ''));
      end;
      // return value removed
      State.DecStack;
    end else
    begin
      if AllowDynamic and (CallMethod.IsOverride or CallMethod.IsVirtual or CallMethod.IsAbstract) and (not CallMethod.IsExternal) then
      begin
        PSE2OpFLOW_PUSHRET(Method.OpCodes[Method.OpCodes.Count - 1].OpCode)^.Position :=
          PSE2OpFLOW_PUSHRET(Method.OpCodes[Method.OpCodes.Count - 1].OpCode)^.Position + 1;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-(CallMethod.Params.Count - 1) - 1, False), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALLDYN(CallMethod.DynamicIndex), ''))
      end
      else
        // Call the method
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALL(0), CallMethod.GenLinkerName));
    end;

    if CallMethod.MethodType = mtDestructor then
      if not NoMagicMemory then
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_DESTROY, ''));
      end;

    // Return position already deleted by OP_RET
    State.DecStack;
    State.WasFunction := result <> nil;

    // Pop Parameters from stack
    for i:=CallMethod.Params.Count-1 downto 0 do
    begin                                              {
      case TSE2Parameter(CallMethod.Params[i]).ParameterType of
      ptConst,
      ptDefault : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      ptVar     : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC_NODEL, ''));
      end;     }
      State.DecStack;
    end;
    if CallMethod.Params.Count > 0 then
    begin
      if CallMethod.Params.Count > 1 then
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC_COUNT(CallMethod.Params.Count), ''))
      else
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
    end;

    if CallMethod.HasSelfParam then
    begin
      if CallMethod.IsExternal and (not CallMethod.IsStatic) then
      begin
        if CallMethod.ReturnValue <> nil then
        begin
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(-1, False), ''));
           State.DecStack;
        end;
      end else
      if (CallMethod.ReturnValue <> nil) and (not CallMethod.IsStatic) and (not CallMethod.IsExternal) then
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(-1, False), ''));
        State.DecStack;
      end;
      // Class must be already in stack - so no push
    end;

    if CallMethod.IsOverload or (AlternativeMethod <> nil) then
    begin
      if (CallMethod.ReturnValue = nil) then
      begin
        if CallMethod.HasSelfParam then
        begin
          if not CallMethod.IsStatic then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
            State.DecStack;
          end;
        end;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
        State.DecStack;
      end;
      //if (not CallMethod.IsStatic) and (CallMethod.Parent <> nil) then
      //   State.DecStack;
    end;
                       (*
    if CallMethod.IsOverload then
    begin
      State.DecStack;
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
    end;       *)
  finally
    MethodList.Free;
  end;
  State.TargetType  := OldTarget;
  State.ParamMethod := nil;
end;

procedure TSE2Parser.ChangeArrayLenAndPopToVar(State: TSE2ParseState;
  Method: TSE2Method; ArrayType: TSE2Type; Variable: TSE2Variable);
var CodeIndex: integer;
begin
  GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_FREE(-1), ''));
  GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_SLEN(0, 0), 'META_' + ArrayType.GenLinkerName));
  State.DecStack;

  if (Variable.Parent = nil) or (Variable.IsStatic) then
  begin
    if Variable.IsStatic then
       GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(0, True), Variable.GenLinkerName))
    else
    begin
      CodeIndex := -(State.StackSize) - Method.StackSize + Variable.CodePos + 1;
      if Variable is TSE2Parameter then
         CodeIndex := CodeIndex - 1;

      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(CodeIndex, False), ''));
    end;
  end else
     GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));

  State.DecStack;
end;

procedure TSE2Parser.ArrayMethodCall(Sender: TObject; CallerMethod, CalledMethod: TSE2Method;
  CallOwner: TSE2Type; StackElem: TSE2Variable);
var aType     : TSE2Type;
    wasExpr   : boolean;
begin
  if not (CallOwner is TSE2Array) then
     RaiseError(petError, 'Internal error: method expected array type');

  if CalledMethod.IsName('GetMinIndex') then
  begin
    GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
    GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(CallOwner).StartIndex), ''));
  end else
  if CalledMethod.IsName('GetMaxIndex') then
  begin
    if TSE2Array(CallOwner).IsDynamic then
    begin
       GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_GLEN, ''));
       GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(-1), ''));
       if TSE2Array(CallOwner).StartIndex <> 0 then
       begin
         GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(CallOwner).StartIndex), ''));
         GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(2), '')); // add
       end;
       GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(2), '')); // add
    end else
    begin
      GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(
                          TSE2Array(CallOwner).StartIndex +
                          TSE2Array(CallOwner).ArrayCount - 1), ''));
    end;
  end else
  if CalledMethod.IsName('GetLength') then
  begin
    if TSE2Array(CallOwner).IsDynamic then
       GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_GLEN, ''))
    else
    begin
      GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      GenCode(CallerMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(CallOwner).ArrayCount), ''));
    end;
  end else
  if CalledMethod.IsName('SetLength') then
  begin
    if TSE2Array(CallOwner).IsDynamic then
    begin
      if StackElem = nil then
         RaiseError(petError, 'Array length can not be changed here');

      if StackElem is TSE2Parameter then
        if TSE2Parameter(StackElem).ParameterType <> ptVar then
           RaiseError(petError, 'The size of array-parameters can only be changed, if they are declared as var-parameters');

      ExpectToken([sesBecomes]);
      ReadNextToken;

      wasExpr := State.IsExpression;
      State.IsExpression := True;
      aType := Expression(State, CallerMethod, GetIntegerType(False));
      if aType = nil then
         RaiseError(petError, 'Expression does not return any value');
      State.IsExpression := wasExpr;

      if not IsCompatible(GetIntegerType(False), aType, sesNone, False, Self) then
         RaiseError(petError, GenIncompatibleExpression(aType, GetIntegerType(False)));

      ChangeArrayLenAndPopToVar(State, CallerMethod, CallOwner, StackElem);
    end else
      RaiseError(petError, 'Array length can only changed for dynamic arrays');
  end;
end;

function TSE2Parser.IdentifierExpression(State: TSE2ParseState;
  Method: TSE2Method; SetterCallBack: TSE2SetterEvent; AccessSet: boolean = False; UseVarMove: boolean = False): TSE2Type;
var pSearchUnit   : TSE2Unit;
    SearchUnit    : string;
    pTempType     : TSE2Type;
    OldTarget     : TSE2Type;
    pSearchParent : TSE2BaseType;
    FindItem      : TSE2BaseType;
    LastItem      : TSE2BaseType;
    ParentItem    : TSE2BaseType;
    StringIndex   : string;
    wasExpression : boolean;
    sCheckUnitName: string;
    searchString  : string;

  function DoMethodCall(CallMethod: TSE2Method; UseBrackets: boolean = False; LastParamSetter: TSE2SetterEvent = nil;
             AllowDynamic: boolean = True; ParentType: TSE2BaseType = nil; AlternativeMethod: TSE2Method = nil): TSE2Type;
  var aType: TSE2Type;
      elem : TSE2Variable;
  begin
    if not Assigned(CallMethod.OnCallMethod) then
       result := MethodCall(State, Method, CallMethod, UseBrackets, LastParamSetter, AllowDynamic, ParentType, False, AlternativeMethod)
    else
    begin
      if ParentItem is TSE2Variable then
      begin
         aType := TSE2Variable(ParentItem).AType;
         elem  := TSE2Variable(ParentItem);
      end else
      if ParentItem is TSE2Type then
      begin
         aType := TSE2Type(ParentItem);
         elem  := nil;
      end else
      if ParentItem is TSE2Property then
      begin
         aType := TSE2Type(ParentItem.Parent);
         if TSE2Property(ParentItem).Getter is TSE2Variable then
            elem := TSE2Variable(TSE2Property(ParentItem).Getter)
         else
            elem := nil;
      end else
      if ParentItem is TSE2Method then
      begin
        if TSE2Method(ParentItem).ReturnValue <> nil then
           aType := TSE2Method(ParentItem).ReturnValue.AType
        else
           aType := nil;

        elem := nil;
      end else
      begin
         aType := nil;
         elem  := nil;
      end;
      CallMethod.OnCallMethod(Self, Method, CallMethod, aType, elem);
      if CallMethod.ReturnValue <> nil then
         result := CallMethod.ReturnValue.AType
      else
         result := nil;
    end;
    State.NoStaticPointer := False;
  end;

  procedure DoPushVariable(FindItem: TSE2Variable);
  begin
    if FindItem.Parent <> nil then
      if TSE2Variable(FindItem).IsStatic then
        if not State.NoStaticPointer then
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
          State.DecStack;
        end;
    State.NoStaticPointer := False;
    State.LastVariable := TSE2Variable(FindItem);
    if not State.IsExpression then
       State.LastTargetVar := State.LastVariable;
    PushVarToStack(State, Method, TSE2Variable(FindItem), UseVarMove);
    result        := TSE2Variable(FindItem).AType;
    LastItem      := FindItem;
    pSearchParent := result;

    if TSE2Variable(FindItem).Parent is TSE2Record then
    begin
      if not TSE2Variable(FindItem).IsStatic then
      begin
        if State.RootOffsetObj = nil then
          State.RootOffsetObj := TSE2Record(TSE2Variable(FindItem).Parent);
      end else
        State.RootOffsetObj := nil;
    end;

    if (TSE2Variable(FindItem).AType is TSE2Class) then
       State.RootOffsetObj := nil;

    if result is TSE2MethodVariable then
    begin
      if (not State.IsAtStatement) and (Tokenizer.Token.AType <> sesBecomes) then
      begin
        LastItem := result;
        if Tokenizer.Token.AType in [sesDot] then
        begin

        end else
        begin
          result := DoMethodCall(TSE2MethodVariable(result).Method, False, nil, True, nil);
          if result <> nil then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(-1, False), ''));
            State.DecStack;
          end else
          if not TSE2MethodVariable(LastItem).Method.HasSelfParam then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
            State.DecStack;
          end;
          pSearchParent := result;
          LastItem := nil;
        end;
      end;
    end;
  end;

var SetterCallbackType: TSE2Type;
begin
  State.RootOffsetObj := nil;
  State.LastMethod := nil;
  State.AParent    := nil;
  State.NoStaticPointer := False;

  ParentItem  := nil;
  FindItem    := nil;
  result      := nil;
  SearchUnit  := '';
  sCheckUnitName := '';
  if Tokenizer.Token.AType = sesUnit then
  begin
    ExpectToken([sesUnit]);
    ReadNextToken;
    ExpectToken([sesOpenRound]);
    ReadNextToken;
    ExpectToken([sesIdentifier]);

    pSearchUnit := TSE2Unit(FindIdentifier(nil, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Unit])));
    State.AParent := pSearchUnit;
    if pSearchUnit = nil then
       RaiseError(petError, 'Unit "'+Tokenizer.Token.Value+'" not found');

    SearchUnit := pSearchUnit.Name;
    ReadNextToken;
    ExpectToken([sesCloseRound]);
    ReadNextToken;
    ExpectToken([sesDot]);
    ReadNextToken;
  end;

  if not State.IsExpression then
     State.LastTargetVar := nil;

  pSearchParent := nil;       
  LastItem      := nil;  
  State.WasFunction := False;
  State.LastProperty := nil;
  while True do
  begin
    ParentItem := FindItem;
    ExpectToken([sesIdentifier]);

    searchString := Tokenizer.Token.Value;
    FindItem := FindIdentifier(Method, Tokenizer.Token.Value, pSearchParent, SearchUnit, nil, AccessSet);
    if FindItem = nil then
    begin
      if (pSearchParent = nil) and (SearchUnit = '') and (Method <> nil) then
         if (Method.Parent is TSE2Class) or (Method.Parent is TSE2Record) then
         begin
           if (not Method.IsStatic) or (Method.MethodType in [mtConstructor, mtDestructor]) then
           begin
             if Method.HasSelfParam and (Method.Params.Count > 0) then
             begin
               FindItem := FindIdentifier(Method, Tokenizer.Token.Value, TSE2Parameter(Method.Params[0]).AType, SearchUnit, nil, AccessSet);
               if FindItem <> nil then
               begin
                 DoPushVariable(TSE2Parameter(Method.Params[0]));
               end;
             end;
           end else
           if Method.IsStatic and (Method.Params.Count > 0) and ((Method.Parent is TSE2Record) or (Method.Parent is TSE2Class)) then
           begin
             FindItem := FindIdentifier(Method, Tokenizer.Token.Value, TSE2Parameter(Method.Params[0]).AType, SearchUnit, nil, AccessSet);
             if FindItem <> nil then
                State.NoStaticPointer := True;
           end;
         end;


      if FindItem = nil then
         RaiseError(petError, 'Unkown identifier: "'+Tokenizer.Token.Value+'"');

    end;

    // Method deprecated is used in "CallMethod" because of overloads
    if not (FindItem is TSE2Method) then
       CheckForDeprecated(FindItem);

    if not (FindItem is TSE2Unit) then
    begin
      if sCheckUnitName <> '' then
         if not StringIdentical(FindItem.AUnitName, sCheckUnitName) then
         begin
            RaiseError(petError, 'Unknown identifier: ' + sCheckUnitName + '.' + searchString);
            exit;
         end;
      sCheckUnitName := '';
      SearchUnit     := '';
    end;



    //LastItem      := nil;
    ReadNextToken;
    if FindItem is TSE2Variable then
    begin
      if (FindItem.Parent = pSearchParent) or (State.NoStaticPointer and (pSearchParent = nil)) then
        if State.NoStaticPointer then
           if not TSE2Variable(FindItem).IsStatic then
              RaiseError(petError, 'Variable is not accessable without an instance');
      DoPushVariable(TSE2Variable(FindItem));
    end else
    if FindItem is TSE2Method then
    begin
      if State.IsAtStatement then
      begin
        result := GetPointerType;
        State.LastMethod := TSE2Method(FindItem);

        if State.NoStaticPointer then
          if TSE2Method(FindItem).HasSelfParam then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btPointer), ''));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_CLEAR(False), ''));
          end;

        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_GetProcPtr(0, TSE2Method(FindItem).HasSelfParam), TSE2Method(FindItem).GenLinkerName()));
        TSE2Method(FindItem).Used := True;
        if not TSE2Method(FindItem).HasSelfParam then
           State.IncStack;
      end else
      begin
        result             := DoMethodCall(TSE2Method(FindItem), False, nil, True, pSearchParent);
        State.NoStaticPointer := False;
        LastItem           := nil;
        pSearchParent      := result;
        State.LastVariable := nil;
        if not State.IsExpression then
           State.LastTargetVar := nil;
      end;
    end else
    if FindItem is TSE2Property then
    begin
      result             := TSE2Property(FindItem).AType;
      State.LastProperty := TSE2Property(FindItem);
      LastItem           := FindItem;
      pSearchParent      := result;
      State.LastVariable := nil;
      if not State.IsExpression then
         State.LastTargetVar := nil;
    end else
    if FindItem is TSE2Constant then
    begin
      if (not State.NoStaticPointer) and (FindItem.Parent <> nil) then
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
        State.DecStack;
      end;
      result    := TSE2Constant(FindItem).AType;
      LastItem  := FindItem;
      pSearchParent := result;
      State.NoStaticPointer := False;
      State.LastVariable := nil;
      if not State.IsExpression then
         State.LastTargetVar := nil;

      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC( TSE2Type(TSE2Constant(FindItem).AType.InheritRoot).AType), ''));
      case TSE2Type(TSE2Constant(FindItem).AType.InheritRoot).AType of
      btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64 :
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetInt(TSE2Constant(FindItem).AsInteger), ''));
          end;
      btSingle :
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetFloat(TSE2Constant(FindItem).AsFloat), ''));
          end;
      btDouble :
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetFloat(TSE2Constant(FindItem).AsFloat), ''));
          end;
      btString :
          begin
            StringIndex := FUnit.Strings.Add(FUnit, TSE2Constant(FindItem).Value).GenLinkerName;
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_LOADRES(0), StringIndex));
          end;
      end;
      State.IncStack;
    end else
    if FindItem is TSE2Type then
    begin
      LastItem := nil;
      State.LastVariable := nil;
      if not State.IsExpression then
         State.LastTargetVar := nil;
      case Tokenizer.Token.AType of
      sesOpenRound :
          begin
            State.AParent    := nil;

            OldTarget := State.TargetType;
            State.TargetType := nil;
            ExpectToken([sesOpenRound]);
            ReadNextToken;
            wasExpression := State.IsExpression;
            State.IsExpression := True;
            pTempType := Expression(State, Method, TSE2Type(FindItem));
            State.IsExpression := wasExpression;
            if not IsCompatible(TSE2Type(FindItem), pTempType, sesImplicitCast, False, Self) then
               RaiseError(petError, GenIncompatibleExpression(pTempType, TSE2Type(FindItem)));

            ExpectToken([sesCloseRound]);
            ReadNextToken;

            ConvertVariableCode(State, Method, pTempType, TSE2Type(FindItem));

            State.TargetType := OldTarget;
            pSearchParent := FindItem;
            result        := TSE2Type(FindItem);
          end;
      sesDot :
          begin
            ExpectToken([sesDot]);
            if (not (FindItem is TSE2Class)) and (not (FindItem is TSE2Record)) then
               RaiseError(petError, 'Identifier must be a class or a record name');

            State.NoStaticPointer := True;
                      (*
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btPointer), ''));
            State.IncStack; *)

            result         := TSE2Type(FindItem);
            pSearchParent  := FindItem;
          end;
      else RaiseError(petError, 'Invalid expression: type "'+FindItem.GetStrongName +'" can not be used like a variable');
      end;
    end else
    if FindItem is TSE2Unit then
    begin
      if result <> nil then
         RaiseError(petError, 'Unit name can not be used here');

      if sCheckUnitName <> '' then
         sCheckUnitName := sCheckUnitName + '.';
      sCheckUnitName := sCheckUnitName + searchString;

      State.LastVariable := nil;
      if not State.IsExpression then
         State.LastTargetVar := nil;
      pSearchUnit := TSE2Unit(FindItem);
      SearchUnit  := pSearchUnit.Name;
      ExpectToken([sesDot]);

      State.NoStaticPointer := False;
      State.AParent := pSearchUnit;
      result        := nil;
      pSearchParent := nil;
    end else
    if FindItem <> nil then
      RaiseError(petError, 'Unexpected identifier type "'+Tokenizer.Token.Value+'"');


    if (Tokenizer.Token.AType = sesDot) then
    begin
      if not (FindItem is TSE2Unit) then
      begin
        State.AParent := pSearchParent;
      end;

      if FindItem is TSE2Property then
      begin
        if TSE2Property(FindItem).Getter = nil then
           RaiseError(petError, 'Property is write only');

        if TSE2Property(FindItem).Getter is TSE2Variable then
        begin
          if TSE2Variable(TSE2Property(FindItem).Getter).Parent <> nil then
            if TSE2Variable(TSE2Property(FindItem).Getter).IsStatic then
              if not State.NoStaticPointer then
              begin
                GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
                State.DecStack;
              end;
          State.NoStaticPointer := False;
          PushVarToStack(State, Method, TSE2Variable(TSE2Property(FindItem).Getter), UseVarMove);
        end else
        if TSE2Property(FindItem).Getter is TSE2Method then
        begin
          DoMethodCall(TSE2Method(TSE2Property(FindItem).Getter));
          State.NoStaticPointer := False;
          State.AParent := pSearchParent;
        end;
      end;

      State.LastVariable := nil;
      if not State.IsExpression then
         State.LastTargetVar := nil;
      //LastItem := nil;
      ReadNextToken;
      if (result = nil) and (not (FindItem is TSE2Unit)) then
         RaiseError(petError, '"." is not allowed here');
    end else // if (Tokenizer.Token.AType = sesDot) then
    begin
      if (Tokenizer.Token.AType = sesOpenBracket) then
      begin
        if FindItem is TSE2Property then
        begin
          if State.IsExpression then
          begin
            if not (TSE2Property(FindItem).Getter is TSE2Method) then
               RaiseError(petError, '"[" is not allowed here');
            if ((TSE2Method(TSE2Property(FindItem).Getter).Params.Count < 2) and (TSE2Method(TSE2Property(FindItem).Getter).HasSelfParam)) or
               ((TSE2Method(TSE2Property(FindItem).Getter).Params.Count < 1) and (not TSE2Method(TSE2Property(FindItem).Getter).HasSelfParam)) then
               RaiseError(petError, 'This property does not have any index');
          end else
          begin
            if TSE2Property(FindItem).Getter is TSE2Method then
            begin
              if not (TSE2Property(FindItem).Getter is TSE2Method) then
                 RaiseError(petError, '"[" is not allowed here');
              if ((TSE2Method(TSE2Property(FindItem).Getter).Params.Count < 2) and (TSE2Method(TSE2Property(FindItem).Getter).HasSelfParam)) or
                 ((TSE2Method(TSE2Property(FindItem).Getter).Params.Count < 1) and (not TSE2Method(TSE2Property(FindItem).Getter).HasSelfParam)) then
                 RaiseError(petError, 'This property does not have any index');
            end else
            begin
              if not (TSE2Property(FindItem).Setter is TSE2Method) then
                 RaiseError(petError, '"[" is not allowed here');
              if ((TSE2Method(TSE2Property(FindItem).Setter).Params.Count < 2) and (TSE2Method(TSE2Property(FindItem).Setter).HasSelfParam)) or
                 ((TSE2Method(TSE2Property(FindItem).Setter).Params.Count < 1) and (not TSE2Method(TSE2Property(FindItem).Setter).HasSelfParam)) then
                 RaiseError(petError, 'This property does not have any index');
            end;
          end;
        end else
        begin
          if not (FindItem is TSE2Variable) then
             RaiseError(petError, '"[" is not allowed here');

          if (TSE2Variable(FindItem).AType is TSE2Array) then
          begin
            if State.RootOffsetObj <> nil then
            begin
              // GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_GetRef(0, False, False), ''));
            end;

            ReadNextToken;
            while true do
            begin
              wasExpression      := State.IsExpression;
              State.IsExpression := True;
              pTempType := Expression(State, Method, GetIntegerType);
              if not (IsCompatible(GetIntegerType, pTempType, sesNone, False, Self)) then
                 RaiseError(petError, 'Expression is not valid');
              State.IsExpression := wasExpression;

              if ((TSE2Variable(FindItem).Parent is TSE2Array) or
                  (TSE2Variable(FindItem).Parent is TSE2Record)) and
                 (not TSE2Array(TSE2Variable(FindItem).AType).IsDynamic) then
                GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_CHECK_RANGE(TSE2Array(TSE2Variable(FindItem).AType).ArrayCount), ''))
              else
                GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_CHECK_RANGE(-1), ''));

              GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(TSE2Variable(FindItem).AType).ElemSize), ''));
              GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(4), '')); // multiply

              if TSE2Array(TSE2Variable(FindItem).AType).StartIndex <> 0 then
              begin
                GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(TSE2Variable(FindItem).AType).StartIndex * TSE2Array(TSE2Variable(FindItem).AType).ElemSize), ''));
                GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(3), '')); // substract
              end;
              GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_INCPD(
                                                    TSE2Type(TSE2Array(TSE2Variable(FindItem).AType).Content.AType.InheritRoot).AType,
                                                    TypeIsNoRefType(TSE2Array(TSE2Variable(FindItem).AType).Content.AType)), ''));

              State.DecStack;
              if TypeIsNoRefType(TSE2Array(TSE2Variable(FindItem).AType).Content.AType) then
                 State.RootOffsetObj := nil
              else
                 State.RootOffsetObj := TSE2Array(TSE2Variable(FindItem).AType);
              ExpectToken([sesCloseBracket, sesColon]);


              FindItem   := TSE2Array(TSE2Variable(FindItem).AType).Content;
              result     := TSE2Variable(FindItem).AType;     
              LastItem   := FindItem;
              State.LastVariable := TSE2Variable(FindItem);

              if Tokenizer.Token.AType = sesCloseBracket then
              begin
                ReadNextToken;
                if Tokenizer.Token.AType <> sesOpenBracket then
                   break;
              end;

              if not (TSE2Variable(FindItem).AType is TSE2Array) then
                 RaiseError(petError, 'Array type expected, but found "' + result.GetStrongName + '" instead');

              ReadNextToken;
            end;
          end else
            RaiseError(petError, '"[" is not allowed here');
        end;
      end;

      if Tokenizer.Token.AType <> sesBecomes then
      begin
        if FindItem is TSE2Property then
        begin
          if State.IsExpression then
          begin
            if TSE2Property(FindItem).Getter = nil then
               RaiseError(petError, 'Property is write only');

            if TSE2Property(FindItem).Getter is TSE2Variable then
            begin
              if TSE2Variable(TSE2Property(FindItem).Getter).Parent <> nil then
                if TSE2Variable(TSE2Property(FindItem).Getter).IsStatic then
                  if not State.NoStaticPointer then
                  begin
                    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
                    State.DecStack;
                  end;
              State.NoStaticPointer := False;
              PushVarToStack(State, Method, TSE2Variable(TSE2Property(FindItem).Getter), UseVarMove);
            end else
            if TSE2Property(FindItem).Getter is TSE2Method then
            begin
              result := DoMethodCall(TSE2Method(TSE2Property(FindItem).Getter), True);
              State.NoStaticPointer := False;

              pSearchParent := result;
              State.AParent := result;

              if Tokenizer.Token.AType = sesDot then
              begin
                ReadNextToken;
                continue;
              end;
            end;
          end else
          begin
            if Tokenizer.Token.AType = sesOpenBracket then
            begin
              if TSE2Property(FindItem).Setter = nil then
              begin
                if TSE2Property(FindItem).Getter is TSE2Method then
                begin
                   DoMethodCall(TSE2Method(TSE2Property(FindItem).Getter), True, nil);
                   State.NoStaticPointer := False;

                   result := TSE2Method(TSE2Property(FindItem).Getter).ReturnValue.AType;
                   LastItem := TSE2Method(TSE2Property(FindItem).Getter).ReturnValue;

                   State.LastVariable := nil;
                   if not State.IsExpression then
                      State.LastTargetVar := nil;
                   pSearchParent := TSE2Method(TSE2Property(FindItem).Getter).ReturnValue.AType;
                   State.AParent := pSearchParent;

                   if Tokenizer.Token.AType in [sesBecomes] then
                      RaiseError(petError, 'Property is read only')
                   else
                      ExpectToken([sesDot]);
                   ReadNextToken;
                   Continue;
                end else
                   RaiseError(petError, 'Internal error: property access type is not supported in read only property');
              end else
              begin
                if TSE2Property(FindItem).Setter is TSE2Method then
                begin
                   result := DoMethodCall(TSE2Method(TSE2Property(FindItem).Setter), True, SetterCallBack, True, nil,
                                          TSE2Method(TSE2Property(FindItem).Getter));
                   State.NoStaticPointer := False;

                   if result <> nil then
                   begin

                     State.LastVariable := nil;
                     if not State.IsExpression then
                        State.LastTargetVar := nil;
                     pSearchParent := TSE2Method(TSE2Property(FindItem).Getter).ReturnValue.AType;
                     State.AParent := pSearchParent;

                     if Tokenizer.Token.AType = sesDot then
                     begin
                       ReadNextToken;
                       Continue;
                     end;
                   end;

                end else
                   RaiseError(petError, 'Internal error: property access type is not supported');
              end;
            end else
            if TSE2Property(FindItem).Getter = nil then
               RaiseError(petError, 'Property is write only')
            else
            if TSE2Property(FindItem).Getter is TSE2Variable then
            begin
              if TSE2Variable(TSE2Property(FindItem).Getter).AType is TSE2MethodVariable then
              begin
                if (not State.IsAtStatement) then
                begin
                  PushVarToStack(State, Method, TSE2Variable(TSE2Property(FindItem).Getter), False);
                  result   := TSE2Variable(TSE2Property(FindItem).Getter).AType;
                  LastItem := result;
                  result := DoMethodCall(TSE2MethodVariable(result).Method, False, nil, True, nil);
                  if result <> nil then
                  begin
                    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(-1, False), ''));
                    State.DecStack;
                  end else
                  if not TSE2MethodVariable(LastItem).Method.HasSelfParam then
                  begin
                    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
                    State.DecStack;
                  end;
                  LastItem := nil;
                end else
                  RaiseError(petError, 'expression not allowed here');
              end else
                RaiseError(petError, 'Internal error: unkown state expression');
            end else
            if TSE2Property(FindItem).Getter is TSE2Method then
            begin
              if TSE2Method(TSE2Property(FindItem).Getter).ReturnValue.AType is TSE2MethodVariable then
              begin
                if (not State.IsAtStatement) then
                begin
                  DoMethodCall(TSE2Method(TSE2Property(FindItem).Getter), True);
                  result   := TSE2Method(TSE2Property(FindItem).Getter).ReturnValue.AType;
                  LastItem := result;
                  result := DoMethodCall(TSE2MethodVariable(result).Method, False, nil, True, nil);
                  if result <> nil then
                  begin
                    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(-1, False), ''));
                    State.DecStack;
                  end else
                  if not TSE2MethodVariable(LastItem).Method.HasSelfParam then
                  begin
                    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
                    State.DecStack;
                  end;
                  LastItem := nil;
                end else
                  RaiseError(petError, 'expression not allowed here');
              end else
              begin
                RaiseError(petError, 'Assignment expected')
                //RaiseError(petError, 'Internal error: unknown state method expression');
              end;
            end else
              RaiseError(petError, 'Internal error: lost parser state');
          end;
        end else // if FindItem is TSE2Property then
        if (LastItem is TSE2Variable) and
             ((TSE2Variable(LastItem).AType is TSE2Array) or
              (TSE2Variable(LastItem).IsDynamic)) then
        begin
          if Tokenizer.Token.AType in [sesDot, sesOpenBracket] then
          begin
            ExpectToken([sesDot, sesOpenBracket]);
            pSearchParent := TSE2Variable(LastItem).AType;
            State.AParent := TSE2Type(pSearchParent);
            ReadNextToken;
            continue;
          end;
        end;
      end; // if Tokenizer.Token.AType <> sesBecomes then
      break;
    end; // if (Tokenizer.Token.AType = sesDot) then else
  end; // while not FHasError do

  if LastItem <> nil then
  begin
    if Tokenizer.Token.AType = sesBecomes then
    begin
      result := nil;
      if not Assigned(SetterCallBack) then
         RaiseError(petError, TSE2TokenName[sesBecomes] + ' is not allowed here');

      if LastItem is TSE2Property then
      begin
        if TSE2Property(LastItem).Setter = nil then
           RaiseError(petError, 'Can not assign values to a read-only property');

        if TSE2Property(LastItem).Setter is TSE2Method then
           DoMethodCall(TSE2Method(TSE2Property(LastItem).Setter), True, SetterCallBack)
        else
        if TSE2Property(LastItem).Setter is TSE2Variable then
        begin
          if TSE2Variable(TSE2Property(LastItem).Setter).Parent <> nil then
            if TSE2Variable(TSE2Property(LastItem).Setter).IsStatic then
              if not State.NoStaticPointer then
              begin
                GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
                State.DecStack;
              end;
          State.NoStaticPointer := False;

          if not TSE2Variable(TSE2Property(LastItem).Setter).IsStatic then
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_INCP(
                               TSE2Variable(TSE2Property(LastItem).Setter).CodePos,
                               TSE2Type(TSE2Variable(TSE2Property(LastItem).Setter).AType.InheritRoot).AType,
                               False), '')
             );
          SetterCallBack(Self, State, Method, TSE2Property(LastItem).AType, nil);
          PopStackToVar(State, Method, TSE2Variable(TSE2Property(LastItem).Setter));
        end;
      end else
      if LastItem is TSE2Type then
      begin
        State.LastSetVariable := nil;
        RaiseError(petError, 'Internal error: multi dimensional array assignment not supported yet');

      end else
      begin
        State.LastSetVariable := State.LastVariable;

        if LastItem is TSE2Parameter then
          if TSE2Parameter(LastItem).ParameterType = ptConst then
          begin
             RaiseError(petError, 'Constant parameters can not be changed');
             exit;
          end;

        if TSE2Variable(LastItem).Parent = nil then
        begin
          if not (LastItem.Parent is TSE2Array) then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
            State.DecStack;
            SetterCallBack(Self, State, Method, TSE2Variable(LastItem).AType, nil);
            PopStackToVar(State, Method, TSE2Variable(LastItem));
          end else
          begin
            SetterCallBack(Self, State, Method, TSE2Variable(LastItem).AType, nil);
            PopStackToVar(State, Method, TSE2Variable(LastItem));
          end;
        end else
        if TSE2Variable(LastItem).Parent is TSE2Class then
        begin
          if TSE2Variable(LastItem).IsStatic then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
            State.DecStack;
          end;

          SetterCallBack(Self, State, Method, TSE2Variable(LastItem).AType, @SetterCallbackType);
          PopStackToVar(State, Method, TSE2Variable(LastItem), SetterCallbackType);
        end else
        if TSE2Variable(LastItem).Parent is TSE2Record then
        begin
          if TSE2Variable(LastItem).IsStatic then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
            State.DecStack;
          end;
          SetterCallBack(Self, State, Method, TSE2Variable(LastItem).AType, @SetterCallbackType);
          PopStackToVar(State, Method, TSE2Variable(LastItem), SetterCallbackType);
        end else
        if TSE2Variable(LastItem).Parent is TSE2Array then
        begin
          SetterCallBack(Self, State, Method, TSE2Variable(LastItem).AType, @SetterCallbackType);
          PopStackToVar(State, Method, TSE2Variable(LastItem), SetterCallbackType);
        end else
          RaiseError(petError, 'Internal error: variable parent not as expected');
      end;
    end;
  end;
  State.AParent := nil;
end;

function TSE2Parser.Expression(State: TSE2ParseState; Method: TSE2Method;
  TargetType: TSE2Type): TSE2Type;
var Token   : TSE2TokenType;
    FacType : TSE2Type;
    iPos, iLine : integer;
begin
  result := MathExpression(State, Method, TargetType);
  if result = nil then
     RaiseError(petError, 'Expression did not return a value');
  while Tokenizer.Token.AType in [sesPlus, sesMinus, sesIs, sesAs, sesEqual, sesSmaller, sesBigger, sesSmallerEqual, sesBiggerEqual, sesUnEqual, sesOr, sesXor] do
  begin
    Token   := Tokenizer.Token.AType;
    ReadNextToken;

    case Token of
    sesIs :
        begin
          if not (result is TSE2Class) then
             RaiseError(petError, 'Is-operator is only available for classes');
          if (TSE2Class(result).IsHelper) then
             RaiseError(petError, 'Is-operator can not be used for class helpers');

          if TSE2Type(result.InheritRoot) = GetExternalObjectType then
             RaiseError(petError, 'Internal Error: Is-Operator not yet implemented for external classes');


          ExpectToken([sesIdentifier]);
          FacType := TypeExpression(Method);
          if FacType = nil then
             RaiseError(petError, 'Unknown identifier');

          if not (FacType is TSE2Class) then
             RaiseError(petError, 'Type comparisons must be applied to classes');
          if TSE2Class(FacType).IsHelper then
             RaiseError(petError, 'Is-Operator can not be used for class helpers');
          if TSE2Type(FacType.InheritRoot) = GetExternalObjectType then
             RaiseError(petError, 'Internal Error: Is-Operator not yet implemented for external classes');

          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.META_SHARE(0), 'META_'+TSE2Class(FacType).GenLinkerName));

          ReadNextToken;
          result := GetBooleanType;
        end;
    sesAs :
        begin
          if not (result is TSE2Class) then
             RaiseError(petError, 'As-operator is only available for classes');
          if (TSE2Class(result).IsHelper) then
             RaiseError(petError, 'As-operator can not be used for class helpers');

          if TSE2Type(result.InheritRoot) = GetExternalObjectType then
             RaiseError(petError, 'Internal Error: As-Operator not yet implemented for external classes');


          ExpectToken([sesIdentifier]);
          FacType := TypeExpression(Method);
          if FacType = nil then
             RaiseError(petError, 'Unknown identifier');

          if not (FacType is TSE2Class) then
             RaiseError(petError, 'Type comparisons must be applied to classes');
          if TSE2Class(FacType).IsHelper then
             RaiseError(petError, 'As-Operator can not be used for class helpers');
          if TSE2Type(FacType.InheritRoot) = GetExternalObjectType then
             RaiseError(petError, 'Internal Error: As-Operator not yet implemented for external classes');

          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.META_CAST(0), 'META_'+TSE2Class(FacType).GenLinkerName));

          ReadNextToken;
          result := FacType;
        end;
    else
        begin
          iPos := Tokenizer.Reader.Position;
          iLine := Tokenizer.Reader.Line;

          FacType := MathExpression(State, Method, TargetType);
          if FacType = nil then
             RaiseError(petError, 'Expression did not return a value');

          if not IsCompatible(result, FacType, Token, False, Self) then
             RaiseError(petError, GenIncompatibleExpression(FacType, result, Token), iPos, iLine);

          result := ConvertIntToSingle(State, Method, result, FacType);
          //result := FacType;

          case Token of
          sesPlus   : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(2), ''));
          sesMinus  : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(3), ''));
          sesOr     : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(7), ''));
          sesXor    : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(8), ''));
          else
              begin
                GenerateComparison(Method, Result, FacType, Token);
              end;
          end;

          if Token in [sesEqual, sesSmaller, sesBigger, sesBiggerEqual, sesSmallerEqual, sesUnEqual] then
          begin
            result := GetBooleanType;
          end;

          State.DecStack;
        end;
    end;
  end;
end;

procedure TSE2Parser.PushVarToStack(State: TSE2ParseState;
  Method: TSE2Method; Variable: TSE2Variable; MoveData: boolean);
var CodeIndex: integer;
begin
  Variable.Used := True;
  if Variable is TSE2Parameter then
  begin
    CodeIndex := -(State.StackSize) - (Method.StackSize) + TSE2Parameter(Variable).CodePos;
    if MoveData then
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_MOVE_FROM(CodeIndex, False), ''))
    else
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(CodeIndex, False), ''));
  end else
  // Is TSE2Variable!
  begin
    if (Variable.IsStatic) then
    begin
      if MoveData then
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_MOVE_FROM(0, True), Variable.GenLinkerName))
      else
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(0, True), Variable.GenLinkerName));
    end else
    begin
      if (Variable.Parent = nil) or (Variable.IsStatic) then
      begin
        CodeIndex := -(State.StackSize) - Method.StackSize + 1 + Variable.CodePos;
        if MoveData then
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_MOVE_FROM(CodeIndex, False), ''))
        else
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(CodeIndex, False), ''));
      end else
      if Variable.Parent is TSE2Class then
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_INCP(Variable.CodePos, TSE2Type(Variable.AType.InheritRoot).AType, False), ''));
        // Decrease stack because last intruction of this method is IncStack and SPEC_INCP does not change stack size
        // --> StackSize = StackSize - 1 + 1 = StackSize;
        State.DecStack;
      end else
      if Variable.Parent is TSE2Record then
      begin
        //if State.WasFunction then
        //   GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_MARK_DEL, ''));

        if State.RootOffsetObj = nil then
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_INCP(Variable.CodePos, TSE2Type(Variable.AType.InheritRoot).AType, TypeIsNoRefType(Variable.AType)), ''))
        else
        begin
          if (State.RootOffsetObj is TSE2Array) or
             (Method.OpCodes.Last.OpCode.OpCode = soSPEC_INCPD) then
          begin
            // GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_GetRef(0, False, False), ''));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_INCP(Variable.CodePos, TSE2Type(Variable.AType.InheritRoot).AType, TypeIsNoRefType(Variable.AType)), ''));
          end else
          begin
            PSE2OpSPEC_INCP(Method.OpCodes.Last.OpCode).Offset  := PSE2OpSPEC_INCP(Method.OpCodes.Last.OpCode).Offset + Variable.CodePos;
            PSE2OpSPEC_INCP(Method.OpCodes.Last.OpCode).newType := TSE2Type(Variable.AType.InheritRoot).AType;
            PSE2OpSPEC_INCP(Method.OpCodes.Last.OpCode).NoRef   := TypeIsNoRefType(Variable.AType);
          end;
        end;
        // Decrease stack because last intruction of this method is IncStack and SPEC_INCP does not change stack size
        // --> StackSize = StackSize - 1 + 1 = StackSize;
        State.DecStack;
      end else
        RaiseError(petError, 'Internal error: variable owner not as expected');
    end;
  end;
  State.IncStack;
  State.WasFunction := False;
end;

procedure TSE2Parser.PopStackToVar(State: TSE2ParseState;
  Method: TSE2Method; Variable: TSE2Variable; StackValue: TSE2Type = nil);
var CodeIndex: integer;

  procedure MakeRecordPop;
  begin
    //if (State.WasFunction) then
    //    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_FREE(0), ''));
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
  end;

  procedure MakeArrayPop;
  begin
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
  end;

begin
  Variable.Used := True;
  if Variable is TSE2Parameter then
  begin
    CodeIndex := -(State.StackSize) - Method.StackSize + TSE2Parameter(Variable).CodePos;
    if (Variable.AType.InheritRoot is TSE2Record) and (not Variable.IsStatic) then
    begin
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_COPY_TO(CodeIndex, 0), 'META_' + Variable.AType.GenLinkerName));
      MakeRecordPop;
    end else
    if (Variable.AType.InheritRoot is TSE2Array) and (not Variable.IsStatic) then
    begin
      if TSE2Array(Variable.AType).IsDynamic then
      begin
        if TSE2Parameter(Variable).ParameterType <> ptVar then
           RaiseError(petError, 'Array parameters can only be changed for var-parameters');

        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(CodeIndex, False), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_GLEN, ''));
        State.IncStack;
        State.IncStack;
        ChangeArrayLenAndPopToVar(State, Method, Variable.AType, Variable);
      end;

      if TSE2Array(Variable.AType).IsDynamic then
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(-1), ''))
      else
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(Variable.AType).ArrayCount), ''));

      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_COPY_TO(CodeIndex - 1, 0), 'META_'+ Variable.AType.GenLinkerName));
      MakeArrayPop;
    end else
    if TSE2Parameter(Variable).ParameterType = ptVar then
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_DECP(CodeIndex), ''))
    else
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(CodeIndex, False), ''));
  end else
  // Is TSE2Variable!
  begin
    if (Variable.Parent = nil) then
    begin
      if Variable.AType.InheritRoot is TSE2Record then
      begin
        if Variable.IsStatic then
        begin
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_COPY_TO(0, 0), Variable.GenLinkerName));
           MakeRecordPop;
        end else
        begin
           CodeIndex := -(State.StackSize) - Method.StackSize + 1 + Variable.CodePos;
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_COPY_TO(CodeIndex, 0), 'META_' + Variable.AType.GenLinkerName));
           MakeRecordPop;
        end;
      end else
      if Variable.AType.InheritRoot is TSE2Array then
      begin
        if Variable.IsStatic then
        begin
          if TSE2Array(Variable.AType).IsDynamic then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(0, True), Variable.GenLinkerName));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_GLEN, ''));
            State.IncStack;
            State.IncStack;
            ChangeArrayLenAndPopToVar(State, Method, Variable.AType, Variable);
          end;

          if TSE2Array(Variable.AType).IsDynamic then
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(-1), ''))
          else
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(Variable.AType).ArrayCount), ''));

          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_COPY_TO(0, 0), Variable.GenLinkerName));
          MakeArrayPop;
        end else
        begin
          CodeIndex := -(State.StackSize) - Method.StackSize + 1 + Variable.CodePos;

          if TSE2Array(Variable.AType).IsDynamic then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(CodeIndex, False), ''));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_GLEN, ''));
            State.IncStack;
            State.IncStack;
            ChangeArrayLenAndPopToVar(State, Method, Variable.AType, Variable);
          end;

          if TSE2Array(Variable.AType).IsDynamic then
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(-1), ''))
          else
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(Variable.AType).ArrayCount), ''));

          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_COPY_TO(CodeIndex - 1, 0), 'META_' + Variable.AType.GenLinkerName));
          MakeArrayPop;
        end;
      end else
      begin
        if (Variable.IsStatic) then
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(0, True), Variable.GenLinkerName))
        else
        begin
          CodeIndex := -(State.StackSize) - Method.StackSize + 1 + Variable.CodePos;
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(CodeIndex, False), ''));
        end;
      end;
    end else
    if Variable.Parent is TSE2Class then
    begin
      if Variable.AType.InheritRoot is TSE2Record then
      begin
        if Variable.IsStatic then
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_COPY_TO(0, 0), Variable.GenLinkerName));
          MakeRecordPop;
        end else
        begin
          CodeIndex := -1;
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_COPY_TO(CodeIndex, 0), 'META_' + Variable.AType.GenLinkerName));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
          MakeRecordPop;
          State.DecStack;
        end;
      end else
      if Variable.AType.InheritRoot is TSE2Array then
      begin
        if (StackValue is TSE2Array) then
        begin
          if TSE2Array(StackValue).IsDynamic then
          begin
            if Variable.IsStatic then
               GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_MOVE_FROM(0, True), Variable.GenLinkerName))
            else
               GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_MOVE_FROM(-1, False), ''));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_GLEN, ''));
            State.IncStack;
            State.IncStack;
            ChangeArrayLenAndPopToVar(State, Method, StackValue, Variable);
          end;

          if TSE2Array(StackValue).IsDynamic then
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(-1), ''))
          else
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(Variable.AType).ArrayCount), ''));

          if Variable.IsStatic then
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_COPY_TO(0, 0), Variable.GenLinkerName))
          else
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_COPY_TO(-2, 0), 'META_' + StackValue.GenLinkerName));
          MakeArrayPop;

          if not Variable.IsStatic then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
            State.DecStack;
          end;
        end else
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_DECP(-1), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
          State.DecStack;
        end;
      end else
      begin
        if Variable.IsStatic then
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(0, True), Variable.GenLinkerName))
        else
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_DECP(-1), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
          State.DecStack;
        end;
      end;
    end else
    if Variable.Parent is TSE2Record then
    begin
      if Variable.AType.InheritRoot is TSE2Record then
      begin
        if Variable.IsStatic then
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_COPY_TO(0, 0), Variable.GenLinkerName));
          MakeRecordPop;
        end else
        begin
          CodeIndex := -1;
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_COPY_TO(CodeIndex, 0), 'META_' + Variable.AType.GenLinkerName));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
          MakeRecordPop;
          State.DecStack;
        end;
      end else
      if Variable.AType.InheritRoot is TSE2Array then
      begin
        if (StackValue is TSE2Array) then
        begin
          if Variable.IsStatic then
          begin
            if TSE2Array(Variable.AType).IsDynamic then
            begin
              GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_MOVE_FROM(0, True), Variable.GenLinkerName));
              GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
              GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_GLEN, ''));
              State.IncStack;
              State.IncStack;
              ChangeArrayLenAndPopToVar(State, Method, Variable.AType, Variable);
            end;

            if TSE2Array(Variable.AType).IsDynamic then
               GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(-1), ''))
            else
               GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(Variable.AType).ArrayCount), ''));

            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_COPY_TO(0, 0), Variable.GenLinkerName));
            MakeArrayPop;
          end else
          begin
            if TSE2Array(StackValue).IsDynamic then
            begin
              GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_MOVE_FROM(-1, False), ''));
              GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
              GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_GLEN, ''));
              State.IncStack;
              State.IncStack;
              ChangeArrayLenAndPopToVar(State, Method, StackValue, Variable);
            end;

            if TSE2Array(StackValue).IsDynamic then
               GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(-1), ''))
            else
               GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(Variable.AType).ArrayCount), ''));


            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_COPY_TO(-2, 0), 'META_' + StackValue.GenLinkerName));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
            MakeArrayPop;
            State.DecStack;
          end;
        end else
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_DECP(-1), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
          State.DecStack;
        end;
      end else
      begin                  
        if Variable.IsStatic then
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(0, True), Variable.GenLinkerName))
        else
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_DECP(-1), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
          State.DecStack;
        end;
      end;
    end else
    if Variable.Parent is TSE2Array then
    begin
      if Variable.AType.InheritRoot is TSE2Record then
      begin
        CodeIndex := -1;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_COPY_TO(CodeIndex, 0), 'META_' + Variable.AType.GenLinkerName));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC_COUNT(1), ''));
        MakeRecordPop;
        State.DecStack;
      end else
      if Variable.AType.InheritRoot is TSE2Array then
      begin
        if TSE2Array(Variable.AType).IsDynamic then
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_MOVE_FROM(-1, False), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_GLEN, ''));
          State.IncStack;
          State.IncStack;
          ChangeArrayLenAndPopToVar(State, Method, Variable.AType, Variable);
        end;

        if TSE2Array(Variable.AType).IsDynamic then
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(-1), ''))
        else
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(Variable.AType).ArrayCount), ''));

        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_COPY_TO(-2, 0), 'META_' + Variable.AType.GenLinkerName));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC_COUNT(2), ''));
        State.DecStack

      end else
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_DECP(-1), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
        State.DecStack;
      end;
    end else
       RaiseError(petError, 'Internal error: variable owner not as expected');

  end;
  State.DecStack;
end;

procedure TSE2Parser.IfStatement(State: TSE2ParseState;
  Method: TSE2Method);
var OpCodePos1,
    OpCodePos2   : integer;
    Expr       : TSE2Type;
    iPos, iLine : integer;
begin
  ExpectToken([sesIf]);
  ReadNextToken;

  State.IsExpression := True;
  try
    iPos := Tokenizer.Reader.Position;
    iLine := Tokenizer.Reader.Line;
    Expr := Expression(State, Method, GetBooleanType);
    if not IsCompatible(GetBooleanType, Expr, sesNone, False, Self) then
       RaiseError(petError, GenIncompatibleExpression(Expr, GetBooleanType), iPos, iLine);
  except
    on E: ESE2ParserError do
    begin
      State.IsExpression := False;
      State.LastVariable := nil;
      State.LastProperty := nil;
      State.LastMethod   := nil;
      State.LastTargetVar := nil;
      State.LastSetVariable := nil;
      while not (Tokenizer.Token.AType in [sesThen, sesElse, sesSemiColon, sesEnd, sesNone, sesUnknown]) do
        ReadNextToken;
    end;
  end;
  State.IsExpression := False;

  ExpectToken([sesThen]);
  ReadNextToken;

  OpCodePos1 := Method.OpCodes.Count;
  GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_JIZ(0), ''));
  State.DecStack;

  try
    Statement(State, Method);
  except
    on E: ESE2ParserError do
    begin
      State.IsExpression := False;
      State.LastVariable := nil;
      State.LastProperty := nil;
      State.LastMethod   := nil;
      State.LastTargetVar := nil;
      State.LastSetVariable := nil;
      while not (Tokenizer.Token.AType in [sesElse, sesSemiColon, sesEnd, sesNone, sesUnknown]) do
        ReadNextToken;
    end;
  end;

  OpCodePos2 := Method.OpCodes.Count;
  GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));

  PSE2OpFLOW_JIZ(Method.OpCodes[OpCodePos1].OpCode).Position := Method.OpCodes.Count;

  if Tokenizer.Token.AType in [sesElse] then
  begin
    ReadNextToken;
    try
      Statement(State, Method);
    except
    on E: ESE2ParserError do
    begin
      State.IsExpression := False;
      State.LastVariable := nil;
      State.LastProperty := nil;
      State.LastMethod   := nil;
      State.LastTargetVar := nil;
      State.LastSetVariable := nil;
      while not (Tokenizer.Token.AType in [sesSemiColon, sesEnd, sesNone, sesUnknown]) do
        ReadNextToken;
    end;
  end;
  end;

  PSE2OpFLOW_GOTO(Method.OpCodes[OpCodePos2].OpCode).Position := Method.OpCodes.Count;
end;

procedure TSE2Parser.CaseStatement(State: TSE2ParseState;
  Method: TSE2Method);
var Expr         : TSE2Type;
    CaseType     : TSE2Type;
    EqualJumps   : TSE2IntegerList;
    NextJumps    : TSE2IntegerList;
    FinalJumps   : TSE2IntegerList;
    i            : integer;
    iCaseCount   : integer;

    oldLoopSize  : integer;
begin
  oldLoopSize := State.LoopStackSize;

  ExpectToken([sesCase]);
  ReadNextToken;

  State.IsExpression := True;
  Expr := Expression(State, Method, nil);
  if Expr = nil then
     RaiseError(petError, 'Expression not allowed here');
  State.IsExpression := False;

  iCaseCount := 0;
  FinalJumps := TSE2IntegerList.Create;
  EqualJumps := TSE2IntegerList.Create;
  NextJumps  := TSE2IntegerList.Create;
  try
    ExpectToken([sesOf]);
    ReadNextToken;

    //OpCodePos1 := -1;
    while not (Tokenizer.Token.AType in [sesEnd, sesElse]) do
    begin
      //if OpCodePos1 > -1 then
      //   PSE2OpFLOW_JIZ(Method.OpCodes[OpCodePos1].OpCode).Position := Method.OpCodes.Count;

      while true do
      begin
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(0, False), ''));
        State.IncStack;

        State.TargetType := Expr;

        State.IsExpression := True;
        try
          CaseType := Expression(State, Method, Expr);
          if not IsCompatible(Expr, CaseType, sesNone, False, Self) then
             RaiseError(petError, 'Type dismatch');
        except
          on E: ESE2ParserError do
          begin
            State.IsExpression := False;
            State.LastVariable := nil;
            State.LastProperty := nil;
            State.LastMethod   := nil;
            State.LastTargetVar := nil;
            State.LastSetVariable := nil;
            while not (Tokenizer.Token.AType in [sesColon, sesElse, sesSemiColon, sesDoublePoint, sesEnd, sesNone, sesUnknown]) do
              ReadNextToken;
          end;
        end;
        State.IsExpression := False;

        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(1), ''));
        State.DecStack;

        NextJumps.Add(Method.OpCodes.Count);
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_JIZ(0), ''));
        State.DecStack;

        ExpectToken([sesDoublePoint, sesColon]);
        if Tokenizer.Token.AType = sesDoublePoint then
           break;

        ExpectToken([sesColon]);
        ReadNextToken;

        EqualJumps.Add(Method.OpCodes.Count);
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));

        PSE2OpFLOW_JIZ(Method.OpCodes.Items[NextJumps[0]].OpCode).Position := Method.OpCodes.Count;
        NextJumps.Clear;
      end;

      iCaseCount := iCaseCount + 1;
      ExpectToken([sesDoublePoint]);

      for i:=0 to EqualJumps.Count-1 do
        PSE2OpFLOW_GOTO(Method.OpCodes[EqualJumps[i]].OpCode).Position := Method.OpCodes.Count;
      EqualJumps.Clear;

      //State.LoopStackSize := State.StackSize;

      ReadNextToken;
      try
        Statement(State, Method);
      except
        on E: ESE2ParserError do
        begin
          State.IsExpression := False;
          State.LastVariable := nil;
          State.LastProperty := nil;
          State.LastMethod   := nil;
          State.LastTargetVar := nil;
          State.LastSetVariable := nil;
          while not (Tokenizer.Token.AType in [sesElse, sesDoublePoint, sesColon, sesSemiColon, sesEnd, sesNone, sesUnknown]) do
            ReadNextToken;
        end;
      end;

      ExpectToken([sesSemiColon]);
      ReadNextToken;

      FinalJumps.Add(Method.OpCodes.Count);
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));

      for i:=0 to NextJumps.Count-1 do
        PSE2OpFLOW_JIZ(Method.OpCodes[NextJumps[i]].OpCode).Position := Method.OpCodes.Count;
      NextJumps.Clear;
    end;            {
    if NextJumps.Count = 0 then
       RaiseError(petError, 'Expression expected');  }

    for i:=0 to NextJumps.Count-1 do
      PSE2OpFLOW_JIZ(Method.OpCodes[NextJumps[i]].OpCode).Position := Method.OpCodes.Count;


    if iCaseCount = 0 then
       RaiseError(petError, 'Expression expected');

    if Tokenizer.Token.AType in [sesElse] then
    begin
      ReadNextToken;
      Statement(State, Method);
      ExpectToken([sesSemiColon]);
      ReadNextToken;
    end;

    for i:=FinalJumps.Count-1 downto 0 do
      PSE2OpFLOW_GOTO(Method.OpCodes[FinalJumps[i]].OpCode).Position := Method.OpCodes.Count;


    ExpectToken([sesEnd]);
    ReadNextToken;
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
    State.DecStack;
  finally
    State.LoopStackSize := oldLoopSize;
    EqualJumps.Free;
    NextJumps.Free;
    FinalJumps.Free;
  end;
  State.TargetType := nil;
end;

procedure TSE2Parser.RepeatStatement(State: TSE2ParseState;
  Method: TSE2Method);
var oldBreakList    : integer;
    oldContinueList : integer;

    OpCodePos1      : integer;
    OpCodePos2      : integer;

    i               : integer;
    oldIsInLoop     : boolean;
    oldLoopStack    : integer;
    Expr            : TSE2Type;

    iPos, iLine     : integer;
begin
  ExpectToken([sesRepeat]);
  ReadNextToken;

  oldBreakList    := Method.Lists.BreakList.Count;
  oldContinueList := Method.Lists.ContinueList.Count;

  OpCodePos1      := Method.OpCodes.Count;

  { Push old }
  oldIsInLoop         := State.IsInLoop;
  oldLoopStack        := State.LoopStackSize;

  { New values }
  State.IsInLoop      := True;
  State.LoopStackSize := State.StackSize;

  StatementSquence(State, Method);

  { Pop old }
  State.IsInLoop      := oldIsInLoop;
  State.LoopStackSize := oldLoopStack;

  ExpectToken([sesUntil]);
  ReadNextToken;

  OpCodePos2      := Method.OpCodes.Count;
                              
  State.IsExpression := True;

  iPos := Tokenizer.Reader.Position;
  iLine := Tokenizer.Reader.Line;

  Expr := Expression(State, Method, GetBooleanType);
  if not IsCompatible(GetBooleanType, Expr, sesNone, False, Self) then
     RaiseError(petError, GenIncompatibleExpression(Expr, GetBooleanType), iPos, iLine);

  State.IsExpression := False;
  GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_JIZ(OpCodePos1), ''));
  State.DecStack;

  for i:=Method.Lists.ContinueList.Count-1 downto oldContinueList do
  begin
    PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.ContinueList[i]].OpCode).Position := OpCodePos2;
    Method.Lists.ContinueList.Delete(Method.Lists.ContinueList.Count-1);
  end;

  for i:=Method.Lists.BreakList.Count-1 downto oldBreakList do
  begin
    PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.BreakList[i]].OpCode).Position := Method.OpCodes.Count;
    Method.Lists.BreakList.Delete(Method.Lists.BreakList.Count-1);
  end;
end;

procedure TSE2Parser.WhileStatement(State: TSE2ParseState;
  Method: TSE2Method);
var oldBreakList    : integer;
    oldContinueList : integer;

    OpCodePos1      : integer;
    OpCodePos2      : integer;

    i               : integer;
    oldIsInLoop     : boolean;
    oldLoopStack    : integer;
    Expr            : TSE2Type;

    iPos, iLine     : integer;
begin
  ExpectToken([sesWhile]);
  ReadNextToken;

  oldBreakList     := Method.Lists.BreakList.Count;
  oldContinueList  := Method.Lists.ContinueList.Count;

  OpCodePos2       := Method.OpCodes.Count;  
  State.IsExpression := True;

  iPos := Tokenizer.Reader.Position;
  iLine := Tokenizer.Reader.Line;

  Expr := Expression(State, Method, GetBooleanType);
  if not IsCompatible(GetBooleanType, Expr, sesNone, False, Self) then
     RaiseError(petError, GenIncompatibleExpression(Expr, GetBooleanType), iPos, iLine);

  State.IsExpression := False;
  ExpectToken([sesDo]);
  ReadNextToken;

  OpCodePos1  := Method.OpCodes.Count;
  GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_JIZ(0), ''));
  State.DecStack;

  { Push old }
  oldIsInLoop    := State.IsInLoop;
  oldLoopStack   := State.LoopStackSize;

  { New values }
  State.IsInLoop      := True;
  State.LoopStackSize := State.StackSize;

  Statement(State, Method);

  { Pop old }
  State.IsInLoop      := oldIsInLoop;
  State.LoopStackSize := oldLoopStack;

  GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(OpCodePos2), ''));

  for i:=Method.Lists.ContinueList.Count-1 downto oldContinueList do
  begin
    PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.ContinueList[i]].OpCode).Position := OpCodePos2;
    Method.Lists.ContinueList.Delete(Method.Lists.ContinueList.Count-1);
  end;

  for i:=Method.Lists.BreakList.Count-1 downto oldBreakList do
  begin
    PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.BreakList[i]].OpCode).Position := Method.OpCodes.Count;
    Method.Lists.BreakList.Delete(Method.Lists.BreakList.Count-1);
  end;

  PSE2OpFLOW_JIZ(Method.OpCodes[OpCodePos1].OpCode).Position := Method.OpCodes.Count;
end;

procedure TSE2Parser.ForStatement(State: TSE2ParseState;
  Method: TSE2Method);
var oldBreakList    : integer;
    oldContinueList : integer;

    opCodePos1      : integer;
    opCodePos2      : integer;

    loopIncrease    : boolean;

    oldIsInLoop     : boolean;
    oldLoopStack    : integer;

    i               : integer;
    oldVar          : TSE2Variable;
    lastVar         : TSE2Variable;
    exprType        : TSE2Type;

    newVariable     : TSE2Variable;
    newVarStart     : TSE2Type;

    enumTryBlock    : TSE2TryBlock;
    enumerator      : TSE2BaseType;
    enumGetEnum     : TSE2BaseType;
    enumMoveNext    : TSE2BaseType;
    enumCurrent     : TSE2BaseType;
    enumAnonymPos   : integer;
begin
  oldBreakList     := Method.Lists.BreakList.Count;
  oldContinueList  := Method.Lists.ContinueList.Count;

  ExpectToken([sesFor]);
  ReadNextToken;

  enumAnonymPos := 0;
  enumTryBlock := nil;
  newVariable := nil;
  oldVar := State.LastVariable;
  try                      
    { Push old }
    oldIsInLoop      := State.IsInLoop;
    oldLoopStack     := State.StackSize;

    State.LastTargetVar := nil;

    ExpectToken([sesIdentifier, sesVar]);
    if Tokenizer.Token.AType = sesVar then
    begin
      ReadNextToken;
      ExpectToken([sesIdentifier]);

      newVariable := TSE2Variable.Create;
      DeclareType(State, newVariable, Tokenizer.Token.Value);
      State.IncStack;

      if Method.Variables.FindItem(Tokenizer.Token.Value) <> nil then
         RaiseError(petError, 'Variable name already in use: "'+Tokenizer.Token.Value+'"');
      if Method.Types.FindItem(Tokenizer.Token.Value) <> nil then
         RaiseError(petError, 'Variable name already in use: "'+Tokenizer.Token.Value+'"');
      if Method.Params.FindItem(Tokenizer.Token.Value) <> nil then
         RaiseError(petError, 'Variable name already in use: "'+Tokenizer.Token.Value+'"');

      ReadNextToken;
      ExpectToken([sesDoublePoint, sesIn]);
      if Tokenizer.Token.AType = sesIn then
      begin
        enumTryBlock := TSE2TryBlock.Create(Method, Self, State);
        newVariable.CodePos := State.StackSize - 1 + Method.StackSize;
        enumAnonymPos := Method.OpCodes.Count;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btS32), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.NOOP, ''));
        ReadNextToken;
      end else
      begin
        ReadNextToken;
        newVariable.AType := TypeExpression(Method);
        newVariable.Used  := True;

        ReadNextToken;
        ExpectToken([sesBecomes, sesIn]);
        if Tokenizer.Token.AType = sesIn then
        begin
          enumTryBlock := TSE2TryBlock.Create(Method, Self, State);
        end;
        ReadNextToken;

        newVariable.CodePos := State.StackSize - 1 + Method.StackSize;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(TSE2Type(newVariable.AType.InheritRoot).AType), ''));
        if newVariable.AType is TSE2Record then
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_MAKE(0, 0), 'META_' + TSE2Record(newVariable.AType).GenLinkerName));
        end else
        if newVariable.AType is TSE2Array then
        begin
          if TSE2Array(newVariable.AType).IsDynamic then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_MAKE(0, 0), 'META_' + newVariable.AType.GenLinkerName));
          end else
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_SFL(TSE2Array(newVariable.AType).ArrayCount, 0), 'META_' + newVariable.AType.GenLinkerName));
          end;
        end;


        if enumTryBlock = nil then
        begin
          State.IsExpression := True;
          newVarStart := Expression(State, Method, newVariable.AType);
          State.IsExpression := False;
          if not IsCompatible(newVariable.AType, newVarStart) then
             RaiseError(petError, GenIncompatibleExpression(newVarStart, newVariable.AType));

          Method.Variables.Add(newVariable);
          PopStackToVar(State, Method, newVariable);
        end else
          Method.Variables.Add(newVariable);
      end;
      State.LastTargetVar := newVariable;
      State.LastSetVariable := newVariable;
    end else
    begin
      ExpectToken([sesIdentifier]);
      newVariable := TSE2Variable(FindIdentifier(Method, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Variable])));
      if newVariable = nil then
         RaiseError(petError, 'Unknown identifier: "'+Tokenizer.Token.Value+'"');
      try
        ReadNextToken;
        ExpectToken([sesBecomes, sesIn]);
        if Tokenizer.Token.AType = sesIn then
        begin
          enumTryBlock := TSE2TryBlock.Create(Method, Self, State);
        end;
        ReadNextToken;

        if enumTryBlock = nil then
        begin
          State.IsExpression := True;
          newVarStart := Expression(State, Method, newVariable.AType);
          State.IsExpression := False;

          if not IsCompatible(newVariable.AType, newVarStart) then
             RaiseError(petError, GenIncompatibleExpression(newVarStart, newVariable.AType));

          PopStackToVar(State, Method, newVariable);
        end;
        State.LastTargetVar := newVariable;
        State.LastSetVariable := newVariable;
      finally
        newVariable := nil;
      end;
    end;

    if enumTryBlock = nil then
    begin
      if not IsCompatible(State.LastTargetVar.AType, GetIntegerType, sesNone, True, Self) then
         RaiseError(petError, 'For loop requires an ordinary variable type');

      ExpectToken([sesTo, sesDownTo]);
      loopIncrease := Tokenizer.Token.AType in [sesTo];
      ReadNextToken;

      State.TargetType := State.LastTargetVar.AType;
      lastVar := State.LastSetVariable;
                               
      State.IsExpression := True;
      exprType := Expression(State, Method, nil);
      if not IsCompatible(State.TargetType, exprType, sesPlus, True, Self) then
         RaiseError(petError, GenIncompatibleExpression(exprType, State.TargetType));
      State.IsExpression := False;

      ConvertVariableCode(State, Method, exprType, lastVar.AType);

      ExpectToken([sesDo]);
      ReadNextToken;

      opCodePos2 := Method.OpCodes.Count;
      PushVarToStack(State, Method, lastVar, False);
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
      State.IncStack;

      if loopIncrease then
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(5), ''))
      else
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(4), ''));
      State.DecStack;

      opCodePos1 := Method.OpCodes.Count;
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_JIZ(0), ''));
      State.DecStack;

      { New values }
      State.IsInLoop      := True;
      State.LoopStackSize := State.StackSize;

      Statement(State, Method);


      for i:=Method.Lists.ContinueList.Count-1 downto oldContinueList do
        PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.ContinueList[i]].OpCode)^.Position := Method.OpCodes.Count;


      PushVarToStack(State, Method, lastVar, False);
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btS32), ''));
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetInt(1), ''));
      if loopIncrease then
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(2), ''))
      else
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(3), '')); 
      PopStackToVar(State, Method, lastVar);

      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(opCodePos2), ''));


      for i:=Method.Lists.BreakList.Count-1 downto oldBreakList do
      begin
        PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.BreakList[i]].OpCode).Position := Method.OpCodes.Count;
        Method.Lists.BreakList.Delete(Method.Lists.BreakList.Count-1);
      end;

      for i:=Method.Lists.ContinueList.Count-1 downto oldContinueList do
        Method.Lists.ContinueList.Delete(Method.Lists.ContinueList.Count-1);

      PSE2OpFLOW_JIZ(Method.OpCodes[opCodePos1].OpCode).Position := Method.OpCodes.Count;
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      State.DecStack;

    end else
    begin
      State.IsExpression := True;
      enumerator := Expression(State, Method, nil);
      if enumerator = nil then
         RaiseError(petError, 'Expression did not return any value');
      State.IsExpression := False;

      //if not ((enumerator is TSE2Class) or (enumerator is TSE2Record)) then
      //   RaiseError(petError, 'for-each loop only works with classes or records');

      enumGetEnum := FindIdentifier(Method, C_SE2Enumerator_Getter, enumerator);
      if enumGetEnum = nil then
         RaiseError(petError, 'Required declaration "'+enumerator.GetStrongName+'.'+C_SE2Enumerator_Getter+'" not found');
      if not (enumGetEnum is TSE2Method) then
         RaiseError(petError, '"'+C_SE2Enumerator_Getter+'" must be a function');
      if (TSE2Method(enumGetEnum).MethodType <> mtFunction) or
         (not TSE2Method(enumGetEnum).HasSelfParam) or
         (TSE2Method(enumGetEnum).Params.Count <> 1) or
         (TSE2Method(enumGetEnum).IsOverload) or        
         (TSE2Method(enumGetEnum).IsStatic) or
         (TSE2Method(enumGetEnum).ReturnValue = nil) or
         (not (TSE2Method(enumGetEnum).ReturnValue.AType is TSE2Class)) then
         RaiseError(petError, '"'+C_SE2Enumerator_Getter+'" must be a function returning the enumerator instance');

      ExpectToken([sesDo]);
      enumerator := MethodCall(State, Method, TSE2Method(enumGetEnum));

      enumMoveNext := FindIdentifier(Method, C_SE2Enumerator_MoveNext, enumerator);
      if enumMoveNext = nil then
         RaiseError(petError, 'Declaration "'+C_SE2Enumerator_MoveNext+'" inside the returned enumerator not found');
      if not (enumMoveNext is TSE2Method) then
         RaiseError(petError, '"'+C_SE2Enumerator_MoveNext+'" must be a function');
      if (TSE2Method(enumMoveNext).MethodType <> mtFunction) or
         (not TSE2Method(enumMoveNext).HasSelfParam) or
         (TSE2Method(enumMoveNext).IsStatic) or       
         (TSE2Method(enumMoveNext).IsOverload) or
         (TSE2Method(enumMoveNext).Params.Count <> 1) or
         (TSE2Method(enumMoveNext).ReturnValue = nil) or
         (TSE2Method(enumMoveNext).ReturnValue.AType.InheritRoot <> GetBooleanType) then
         RaiseError(petError, '"'+C_SE2Enumerator_MoveNext+'" must be a function');


      enumCurrent := FindIdentifier(Method, C_SE2Enumerator_Current, enumerator);
      if enumCurrent = nil then
         RaiseError(petError, 'Declaration "'+C_SE2Enumerator_Current+'" inside the returned enumerator not found');
      if not (enumCurrent is TSE2Property) then
         RaiseError(petError, '"'+C_SE2Enumerator_Current+'" must be a property');
      if (TSE2Property(enumCurrent).Getter = nil) or
         (TSE2Property(enumCurrent).IsStatic) or
         (TSE2Property(enumCurrent).Params.Count <> 0) then
         RaiseError(petError, '"'+C_SE2Enumerator_Current+'" must be a none-indexed property');


      if Method.Variables.IndexOf(State.LastTargetVar) < 0 then
      begin
        State.LastTargetVar.AType   := TSE2Property(enumCurrent).AType;

        PSE2OpSTACK_INC(Method.OpCodes[enumAnonymPos].OpCode).AType := TSE2Type(State.LastTargetVar.AType.InheritRoot).AType;
        if State.LastTargetVar.AType is TSE2Record then
        begin
          PSE2OpSTACK_INC(Method.OpCodes[enumAnonymPos + 1].OpCode).OpCode := soREC_MAKE;
          Method.OpCodes[enumAnonymPos + 1].CodeIndex := 'META_' + TSE2Record(State.LastTargetVar.AType).GenLinkerName;
        end else
        if State.LastTargetVar.AType is TSE2Array then
        begin
          if TSE2Array(State.LastTargetVar.AType).IsDynamic then
          begin
            Method.OpCodes[enumAnonymPos + 1].ChangeOpCode(TSE2OpCodeGen.ARR_MAKE(0, 0), 'META_' + newVariable.AType.GenLinkerName);
          end else
          begin
            Method.OpCodes[enumAnonymPos + 1].ChangeOpCode(TSE2OpCodeGen.ARR_SFL(TSE2Array(newVariable.AType).ArrayCount, 0), 'META_' + newVariable.AType.GenLinkerName);
          end;
        end;
        Method.Variables.Add(State.LastTargetVar);
      end;

      if not IsCompatible(State.LastTargetVar.AType, TSE2Property(enumCurrent).AType) then
         RaiseError(petError, GenIncompatibleExpression(TSE2Property(enumCurrent).AType, State.LastTargetVar.AType));

      ReadNextToken;


      enumTryBlock.Step1;

      { while enum.MoveNext }
      {
      [START]
      [PUSH FROM [enumerator]]
      [PUSH U8]
      [PUSH FROM [-1 DYNAMIC]]
      [PUSH RET +2]
      [CALL [MoveNext]]
      [POP]
      [POP TO [-1 DYNAMIC]]
      [JIZ END]  }        
      opCodePos1 := Method.OpCodes.Count;
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(0, False), ''));
      MethodCall(State, Method, TSE2Method(enumMoveNext));

      opCodePos2 := Method.OpCodes.Count;
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_JIZ(0), ''));

      {   p := enum.Current; }
      {
      [PUSH FROM [enumerator]]
      [PUSH [value type]]
      [PUSH FROM [-1 DYNAMIC]]
      [PUSH RET +2]
      [CALL [GetCurrent]]
      [POP]
      [POP TO -1]
      [POP TO [variable position]]    }
      State.IncStack;
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(0, False), ''));
      if TSE2Property(enumCurrent).Getter is TSE2Method then
      begin
        MethodCall(State, Method, TSE2Method(TSE2Property(enumCurrent).Getter));
        PopStackToVar(State, Method, State.LastTargetVar);
      end else
      begin
        PushVarToStack(State, Method, TSE2Variable(TSE2Property(enumCurrent).Getter), False); 
        PopStackToVar(State, Method, State.LastTargetVar);
      end;

      
      State.IsInLoop      := True;
      State.LoopStackSize := State.StackSize;
      Statement(State, Method);

      { end }
      {[GOTO START]
      [END]  }

      { New values }

      for i:=Method.Lists.ContinueList.Count-1 downto oldContinueList do
        PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.ContinueList[i]].OpCode)^.Position := Method.OpCodes.Count;

      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(opCodePos1), ''));
      PSE2OpFLOW_GOTO(Method.OpCodes[opCodePos2].OpCode).Position := Method.OpCodes.Count;

      for i:=Method.Lists.BreakList.Count-1 downto oldBreakList do
      begin
        PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.BreakList[i]].OpCode).Position := Method.OpCodes.Count;
        Method.Lists.BreakList.Delete(Method.Lists.BreakList.Count-1);
      end;

      for i:=Method.Lists.ContinueList.Count-1 downto oldContinueList do
        Method.Lists.ContinueList.Delete(Method.Lists.ContinueList.Count-1);

      enumTryBlock.Step3(True);

      { Destroy Enumerator }
      if TSE2Type(enumerator).InheritRoot = GetExternalObjectType then
        enumCurrent := TSE2Method(FindIdentifier(nil, 'Free', GetExternalObjectType, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Method])))
      else
        enumCurrent := TSE2Method(FindIdentifier(nil, 'Free', GetScriptObjectType, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Method])));
      {$IFDEF SEII_SMART_LINKING}
      Method.UsedMethods.Add(TSE2Method(enumCurrent));
      {$ELSE}
      TSE2Method(enumCurrent).Used := True;
      {$ENDIF}
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(Method.OpCodes.Count + 2, 0), ''));
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALL(0), TSE2Method(enumCurrent).GenLinkerName));
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));

      enumTryBlock.Step5;

      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      State.DecStack;

              {
      enumGetEnum     : TSE2BaseType;
      enumMoveNext    : TSE2BaseType;
      enumCurrent     : TSE2BaseType;     }
    end;


    { Pop old }
    State.IsInLoop      := oldIsInLoop;
    State.LoopStackSize := oldLoopStack;
  finally
    if newVariable <> nil then
    begin

      if newVariable.AType is TSE2Record then
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_FREE(0), ''));

      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      State.DecStack;

      if Method.Variables.IndexOf(newVariable) >= 0 then
         Method.Variables.Delete(newVariable)
      else
         newVariable.Free;
    end;

    enumTryBlock.Free;
    State.LastVariable := oldVar;
  end;
end;

procedure TSE2Parser.LoopFlowStatement(State: TSE2ParseState;
  Method: TSE2Method);
var i: integer;
begin
  case Tokenizer.Token.AType of
  sesContinue :
      begin
        if not State.IsInLoop then
           RaiseError(petError, 'Continue not allowed here');

        for i:=State.StackSize-1 downto State.LoopStackSize do
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));

        if State.IsInTryBlock then
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.NOOP, ''));

        Method.Lists.ContinueList.Add(Method.OpCodes.Count);
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
        ReadNextToken;
      end;
  sesBreak :
      begin
        if not State.IsInLoop then
           RaiseError(petError, 'Break not allowed here');

        for i:=State.StackSize-1 downto State.LoopStackSize do
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));

        
        if State.IsInTryBlock then
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.NOOP, ''));

        Method.Lists.BreakList.Add(Method.OpCodes.Count);
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
        ReadNextToken;
      end;
  sesExit :
      begin
        if Method.Lists.ExitTableList.Count = 0 then
           RaiseError(petError, 'Exit not allowed here');

        {if State.IsInTryBlock then
        begin
          for i:=State.StackSize-1 downto State.LoopStackSize do
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
        end else  { }
        begin
          for i:=State.StackSize-1 downto Method.Lists.ExitTableList.Last do
             GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
        end;

        if State.IsInTryBlock then
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.NOOP, ''));

        Method.Lists.ExitList.Add(Method.OpCodes.Count);
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
        ReadNextToken;
      end;
  end;
end;

procedure TSE2Parser.TryStatement(State: TSE2ParseState;
  Method: TSE2Method);
(*var oldExitList     : integer;
    oldContinueList : integer;
    oldBreakList    : integer;

    OpCodePos1      : integer;
    OpCodePos2      : integer;

    i               : integer;
    bIsExceptBlock  : boolean;

  procedure CallExceptFree;
  var aMethod: TSE2Method;
  begin
    aMethod := TSE2Method(FindIdentifier(nil, 'Free', GetScriptObjectType, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Method])));

    {$IFDEF SEII_SMART_LINKING}
    Method.UsedMethods.Add(aMethod);
    {$ELSE}
    aMethod.Used := True;
    {$ENDIF}                       

    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_PEX, ''));
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(Method.OpCodes.Count + 2, 0), ''));
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALL(0), aMethod.GenLinkerName));   
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
  end;

var generalRETPos : integer;
    exitRETPos    : integer;
    continueRETPos: integer;
    breakRETPos   : integer;

    exitEntry     : boolean;
    continueEntry : boolean;
    breakEntry    : boolean;

    wasInTryFinally : boolean;
                               *)
var tryBlock: TSE2TryBlock;
begin          
  ExpectToken([sesTry]);
  ReadNextToken;
  tryBlock := TSE2TryBlock.Create(Method, Self, State);
  try
    tryBlock.Step1;
    tryBlock.Step2_TryContent;

    ExpectToken([sesFinally, sesExcept]);

    tryBlock.Step3(Tokenizer.Token.AType = sesFinally);
    
    ReadNextToken;
    tryBlock.Step4_SafeContent;

    tryBlock.Step5;

    
    State.IsInExceptBlock := False;
    ExpectToken([sesEnd]);
    ReadNextToken;
  finally
    tryBlock.Free;
  end;
  (*

  ExpectToken([sesTry]);
  ReadNextToken;

  oldExitList     := Method.Lists.ExitList.Count;
  oldContinueList := Method.Lists.ContinueList.Count;
  oldBreakList    := Method.Lists.BreakList.Count;

  OpCodePos2      := Method.OpCodes.Count;
  GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_TRYFIN(0, 0), ''));

  StatementSquence(State, Method);

  ExpectToken([sesFinally, sesExcept]);

  generalRETPos  := 0;
  exitRETPos     := 0;
  continueRETPos := 0;
  breakRETPos    := 0;

  // finally - flow
  //   [try]  1    2   3   4  [finally]   5    6   [end]  7
  //   ------------------------------------------------------
  //   Normal-Code:
  //     goto [1] until [2]; [2] prepares later jump to [7] and goto [5]; [5] returns to [7]
  //
  //   Exit-Code
  //     goto [1] skip [2]; [3] prepares later jump to [6] and goto [5]; [5] returns to [6] where exit is processed
  //
  //   Except-Code
  //     goto [1] skip [2,3]; [4] prepares later jump to [end] and goto [5]; [5] returns to [end]

  if Tokenizer.Token.AType = sesFinally then
  begin
    generalRETPos := Method.OpCodes.Count;
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(0, 0), ''));
    State.IncStack;
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
  end;

  PSE2OpSAFE_TRYFIN(Method.OpCodes[OpCodePos2].OpCode).SavePos := Method.OpCodes.Count;
  OpCodePos1 := Method.OpCodes.Count;

  bIsExceptBlock := False;
  case Tokenizer.Token.AType of
  sesFinally :
      begin
        if Method.Lists.ExitList.Count > oldExitList then
        begin
          for i:=Method.Lists.ExitList.Count-1 downto oldExitList do
            PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.ExitList[i]].OpCode).Position := Method.OpCodes.Count;
          exitRETPos := Method.OpCodes.Count;
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(0, 0), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
        end;

        if Method.Lists.ContinueList.Count > oldContinueList then
        begin
          for i:=Method.Lists.ContinueList.Count-1 downto oldContinueList do
            PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.ContinueList[i]].OpCode).Position := Method.OpCodes.Count;
          continueRETPos := Method.OpCodes.Count;
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(0, 0), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
        end;

        if Method.Lists.BreakList.Count > oldBreakList then
        begin
          for i:=Method.Lists.BreakList.Count-1 downto oldBreakList do
            PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.BreakList[i]].OpCode).Position := Method.OpCodes.Count;
          breakRETPos := Method.OpCodes.Count;
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(0, 0), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
        end;
                                                                                              
        PSE2OpFLOW_GOTO(Method.OpCodes[generalRETPos  + 1].OpCode).Position := Method.OpCodes.Count;

        if Method.Lists.ExitList.Count > oldExitList then
           PSE2OpFLOW_GOTO(Method.OpCodes[exitRETPos     + 1].OpCode).Position := Method.OpCodes.Count;
        if Method.Lists.ContinueList.Count > oldContinueList then
           PSE2OpFLOW_GOTO(Method.OpCodes[continueRETPos + 1].OpCode).Position := Method.OpCodes.Count;
        if Method.Lists.BreakList.Count > oldBreakList then
           PSE2OpFLOW_GOTO(Method.OpCodes[breakRETPos    + 1].OpCode).Position := Method.OpCodes.Count;

        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_BLOCK(0, False), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(Method.OpCodes.Count + 3), ''));

        OpCodePos1 := Method.OpCodes.Count;
        PSE2OpSAFE_TRYFIN(Method.OpCodes[OpCodePos2].OpCode).SavePos := Method.OpCodes.Count;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_BLOCK(0, True), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(0, 0), ''));

      end;
  sesExcept :
      begin
        bIsExceptBlock := True;
        PSE2OpSAFE_TRYEX(Method.OpCodes[OpCodePos2].OpCode).OpCode := soSAFE_TRYEX;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_BLOCK(0, True), ''));
      end;
  end;
  ReadNextToken;
  State.IsInExceptBlock := bIsExceptBlock;

  exitEntry     := Method.Lists.ExitList.Count > oldExitList;
  continueEntry := Method.Lists.ContinueList.Count > oldContinueList;
  breakEntry    := Method.Lists.BreakList.Count > oldBreakList;

  if not bIsExceptBlock then
  begin
    for i:=Method.Lists.ExitList.Count-1 downto oldExitList do
      Method.Lists.ExitList.Delete(Method.Lists.ExitList.Count-1);
    for i:=Method.Lists.ContinueList.Count-1 downto oldContinueList do
      Method.Lists.ContinueList.Delete(Method.Lists.ContinueList.Count-1);
    for i:=Method.Lists.BreakList.Count-1 downto oldBreakList do
      Method.Lists.BreakList.Delete(Method.Lists.BreakList.Count-1);
  end;

  wasInTryFinally := State.IsInFinallyBlock;
  if not bIsExceptBlock then
     State.IsInFinallyBlock := True;

  try
    StatementSquence(State, Method);
  finally
    State.IsInFinallyBlock := wasInTryFinally;
  end;

  if not bIsExceptBlock then
  begin
    State.DecStack;
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));

    if exitEntry then
    begin
      PSE2OpFLOW_PUSHRET(Method.OpCodes[exitRETPos].OpCode).Position  := Method.OpCodes.Count;
      Method.Lists.ExitList.Add(Method.OpCodes.Count);
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
    end;

    if continueEntry then
    begin
      PSE2OpFLOW_PUSHRET(Method.OpCodes[continueRETPos].OpCode).Position  := Method.OpCodes.Count;
      Method.Lists.ContinueList.Add(Method.OpCodes.Count);
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
    end;
    
    if breakEntry then
    begin
      PSE2OpFLOW_PUSHRET(Method.OpCodes[breakRETPos].OpCode).Position  := Method.OpCodes.Count;
      Method.Lists.BreakList.Add(Method.OpCodes.Count);
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
    end;
                                                                                
    PSE2OpFLOW_PUSHRET(Method.OpCodes[OpCodePos1 + 1].OpCode).Position := Method.OpCodes.Count;
    PSE2OpFLOW_PUSHRET(Method.OpCodes[generalRETPos].OpCode).Position  := Method.OpCodes.Count;

    
    PSE2OpSAFE_TRYFIN(Method.OpCodes[OpCodePos2].OpCode).LeavePos := Method.OpCodes.Count;
    PSE2OpSAFE_BLOCK(Method.OpCodes[OpCodePos1].OpCode).SkipPoint := Method.OpCodes.Count;
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_TRYEND, ''));
  end else
  begin
    CallExceptFree;

    PSE2OpSAFE_TRYFIN(Method.OpCodes[OpCodePos2].OpCode).LeavePos := Method.OpCodes.Count;
    PSE2OpSAFE_BLOCK(Method.OpCodes[OpCodePos1].OpCode).SkipPoint := Method.OpCodes.Count;

    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_TRYEND, ''));
  end;

  State.IsInExceptBlock := False;
  ExpectToken([sesEnd]);
  ReadNextToken; *)
end;

function TSE2Parser.Term(State: TSE2ParseState; Method: TSE2Method;
  TargetType: TSE2Type): TSE2Type;
var Token   : TSE2TokenType;
    FacType : TSE2Type;
    iPos, iLine : integer;
begin
  result := Operators(State, Method, TargetType);
  if result = nil then
     RaiseError(petError, 'Expression did not return a value');
  while Tokenizer.Token.AType in [sesStar, sesSlash, sesDiv, sesMod, sesAnd, sesShr, sesShl] do
  begin

    Token   := Tokenizer.Token.AType;
    ReadNextToken;

    iPos := Tokenizer.Reader.Position;
    iLine := Tokenizer.Reader.Line;
    FacType := Operators(State, Method, TargetType);
    if FacType = nil then
       RaiseError(petError, 'Expression did not return a value', iPos, iLine);


    if Token = sesSlash then
    begin
      ConvertVariableCode(State, Method, FacType, GetDoubleType);
      FacType := GetDoubleType;
    end;

    if not IsCompatible(FacType, result, Token, False, Self) then
       RaiseError(petError, GenIncompatibleExpression(Result, FacType, Token), iPos, iLine);

    result := ConvertIntToSingle(State, Method, result, FacType);
    ConvertVariableCode(State, Method, FacType, result);
    //result  := FacType;

    case Token of
    sesStar   : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(4), ''));
    sesDiv,
    sesSlash  : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(5), ''));
    sesAnd    : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(6), ''));
    sesMod    : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(9), ''));
    sesShr    : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(10), ''));
    sesShl    : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(11), ''));
    end;
    State.DecStack;
  end;
end;

function TSE2Parser.MathExpression(State: TSE2ParseState;
  Method: TSE2Method; TargetType: TSE2Type): TSE2Type;
var Token   : TSE2TokenType;
    FacType : TSE2Type;
    iPos, iLine : integer;
begin
  if Tokenizer.Token.AType in [sesPlus, sesMinus] then
  begin
    Token  := Tokenizer.Token.AType;
    ReadNextToken;
    result := Term(State, Method, TargetType);

    if Token = sesMinus then
       GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(1), ''))
    else
    if Token = sesNot then
    begin
      if result.InheritRoot = GetBooleanType then
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(21), ''))
      else
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(20), ''));
    end;
  end else
    result := Term(State, Method, TargetType);

  if result = nil then
     RaiseError(petError, 'Expression did not return a value');

  while Tokenizer.Token.AType in [sesPlus, sesMinus, sesOr, sesXor] do
  begin
                 
    Token := Tokenizer.Token.AType;
    ReadNextToken;

    iPos := Tokenizer.Reader.Position;
    iLine := Tokenizer.Reader.Line;
    FacType := Term(State, Method, TargetType);
    if FacType = nil then
       RaiseError(petError, 'Expression did not return a value');

    if not IsCompatible(FacType, Result, Token, False, Self) then
       RaiseError(petError, GenIncompatibleExpression(Result, FacType, Token), iPos, iLine);

    result := ConvertIntToSingle(State, Method, result, FacType);

    //if FacType.AType <> result.AType then
    //   ConvertVariableCode(State, Method, FacType, result);

    case Token of
    sesPlus    : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(2), ''));
    sesMinus   : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(3), ''));
    sesOr      : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(7), ''));
    sesXor     : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(8), ''));
    end;
    State.DecStack;
  end;
end;

function TSE2Parser.GetBooleanType: TSE2Type;
begin
  result := TSE2Type(FindIdentifier(nil, C_SE2Boolean, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Type])));
  if result = nil then
     RaiseError(petError, 'Internal error: boolean type not found');
end;       

function TSE2Parser.GetIntegerType(use64Bit: boolean = True): TSE2Type;
var s: string;
begin
  if use64Bit then
     s := C_SE2Int64
  else
     s := C_SE2Int32;

  result := TSE2Type(FindIdentifier(nil, s, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Type])));
  if result = nil then
     RaiseError(petError, 'Internal error: '+s+' type not found');
end;

function TSE2Parser.GetDoubleType: TSE2Type;
begin
  result := TSE2Type(FindIdentifier(nil, C_SE2Double, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Type])));
  if result = nil then
     RaiseError(petError, 'Internal error: double type not found');
end;

function TSE2Parser.GetStringType: TSE2Type;
begin
  result := TSE2Type(FindIdentifier(nil, C_SE2String, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Type])));
  if result = nil then
     RaiseError(petError, 'Internal error: string type not found');
end;   

function TSE2Parser.GetPointerType: TSE2Type;
begin
  result := TSE2Type(FindIdentifier(nil, C_SE2Pointer, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Type])));
  if result = nil then
     RaiseError(petError, 'Internal error: pointer type not found');
end;

function TSE2Parser.ConvertIntToSingle(State: TSE2ParseState;
  Method: TSE2Method; aType1, aType2: TSE2Type): TSE2Type;

  function TypeBaseType(aType: TSE2Type): TSE2TypeIdent;
  begin                
    if aType.InheritRoot <> nil then
       aType := TSE2Type(aType.InheritRoot);

    result := aType.AType;
  end;

  function TypeIsFloatingPoint(aType: TSE2Type): boolean;
  begin
    result := TypeBaseType(aType) in [btSingle, btDouble];
  end;

  function TypeIsInteger(aType: TSE2Type): boolean;
  begin
    result := TypeBaseType(aType) in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64];
  end;

begin
  result := aType1;
  if aType1 = aType2 then
     exit;

  if TypeIsFloatingPoint(aType1) or TypeIsFloatingPoint(aType2) then
    if TypeIsInteger(aType1) or TypeIsInteger(aType2) then
    begin
      if TypeIsInteger(aType1) then
      begin
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_CONVERT(TypeBaseType(aType2), -1), ''));
         result := aType2;
      end else
      if TypeIsInteger(aType2) then
      begin
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_CONVERT(TypeBaseType(aType1), 0), ''));
         result := aType1;
      end;
    end;
end;

function TSE2Parser.ConvertVariableCode(State: TSE2ParseState;
  Method: TSE2Method; aCurrentType, aNewType: TSE2Type; DoAdd: boolean = True): TSE2LinkOpCode;
var CanConvert  : boolean;
    CurrentType : TSE2TypeIdent;
    NewType     : TSE2TypeIdent;
begin
  result := nil;
  if aCurrentType.InheritRoot is TSE2Type then
     aCurrentType := TSE2Type(aCurrentType.InheritRoot);
  if aNewType.InheritRoot is TSE2Type then
     aNewType := TSE2Type(aNewType.InheritRoot);

  CurrentType := aCurrentType.AType;
  NewType     := aNewType.AType;

  if CurrentType = NewType then
     exit;

  CanConvert := False;
  case NewType of
  btU8, btS8, btU16, btS16, btU32, btS32 :
      CanConvert := CurrentType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64, btPointer];
  btS64, btU64 :
      CanConvert := CurrentType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64, btPointer];
  btSingle :                     
      CanConvert := CurrentType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64, btDouble];
  btDouble :
      CanConvert := CurrentType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64, btSingle];
  btString :
      CanConvert := CurrentType in [btPChar, btUTF8String, btWideString, btAnsiString, btPAnsiChar, btPWideChar];
  btUTF8String :
      CanConvert := CurrentType in [btString, btPChar, btWideString, btAnsiString, btPAnsiChar, btPWideChar];
  btWideString :
      CanConvert := CurrentType in [btString, btPChar, btUTF8String, btAnsiString, btPAnsiChar, btPWideChar];
  btPChar :
      CanConvert := CurrentType in [btString, btUTF8String, btWideString, btAnsiString, btPAnsiChar, btPWideChar];
  btAnsiString :
      CanConvert := CurrentType in [btString, btPChar, btUTF8String, btWideString, btPAnsiChar, btPWideChar];
  btPAnsiChar :
      CanConvert := CurrentType in [btString, btPChar, btUTF8String, btWideString, btAnsiString, btPWideChar];
  btPWideChar :
      CanConvert := CurrentType in [btString, btPChar, btUTF8String, btWideString, btAnsiString, btPAnsiChar];


  btProcPtr :
      CanConvert := CurrentType in [btPointer];
  btPointer,
  btObject :
      CanConvert := CurrentType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64];
  end;

  if CanConvert then
  begin
    result := TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_CONVERT(NewType, 0), '');
    if DoAdd then
      GenCode(Method, result );
  end;
end;       

class function TSE2Parser.ClassesAreCompatible(TargetType,
  CurrentType: TSE2Class): boolean;

  function CheckClassTree(Target, Current: TSE2Class): boolean;
  begin
    result := False;
    if Target = Current then
       result := True
    else
    begin
      if Current.InheritFrom is TSE2Class then
         result := CheckClassTree(Target, TSE2Class(Current.InheritFrom));
    end;
  end;

begin
  if TargetType <> CurrentType then
  begin
    result := CheckClassTree(TargetType, CurrentType);
  end else
    result := True;
end;

class function TSE2Parser.IsCompatible(TargetType, CurrentType: TSE2Type;
  Operation: TSE2TokenType; StrictInt: boolean; AInstance: TSE2Parser): boolean;

  function ImplicitCast: boolean;
  begin
    case TargetType.AType of
    btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64, btObject, btPointer :
        begin
          result := CurrentType.AType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64, btPointer, btObject, btRecord];
        end;
    btSingle, btDouble :
        begin
          result := CurrentType.AType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64, btPointer, btObject, btSingle, btDouble];
        end;
    btString, btUTF8String, btWideString, btPChar, btAnsiString, btPAnsiChar, btPWideChar :
        begin
          result := CurrentType.AType in [btString, btUTF8String, btWideString, btPChar, btAnsiString, btPAnsiChar, btPWideChar];
        end;
    btRecord :
        begin
          result := (CurrentType = TargetType) or (CurrentType.AType in [btPointer, btObject]);
        end;
    btProcPtr :
        begin
          result := (CurrentType = TargetType) or (CurrentType.AType in [btPointer]);
        end;
    else
        result := False;
    end;
  end;

  function CompareMethods(Meth1, Meth2: TSE2Method; StrictMode: boolean): boolean;
  var i: integer;
      p1, p2: TSE2Parameter;
  begin
    result := True;
    if (Meth1 = nil) or (Meth2 = nil) then
       exit;


    result := False;
    if Meth1.IsExternal then
       exit;
       
    if Meth1.Params.Count <> Meth2.Params.Count then
       exit;

    if ((Meth1.ReturnValue <> nil) and (Meth2.ReturnValue = nil)) or
       ((Meth1.ReturnValue = nil) and (Meth2.ReturnValue <> nil)) then
       exit;

    if StrictMode then
      if Meth1.ReturnValue <> nil then
        if Meth1.ReturnValue.AType <> Meth2.ReturnValue.AType then
          exit;

    for i:=0 to Meth1.Params.Count-1 do
    begin
      p1 := TSE2Parameter(Meth1.Params[i]);
      p2 := TSE2Parameter(Meth2.Params[i]);

      if (p1.ParameterType <> p2.ParameterType) then
         exit;

      if StrictMode then
      begin
        if (p1.AType <> p2.AType) then
          if not ((p1.AType = nil) or (p2.AType = nil)) then
             exit;
      end else
      begin
        if (p1.AType <> nil) and (p2.AType <> nil) then
          if (p1.AType.InheritRoot <> p2.AType.InheritRoot) then
             exit;
      end;
    end;

    result := True;
  end;

var orgTarget  : TSE2Type;
    orgCurrent : TSE2Type;
begin
  if (TargetType = nil) or (CurrentType = nil) then
  begin
    result := True;
    exit;
  end;
  if TargetType.Strict and (Operation = sesNone) then
  begin
    result := TargetType = CurrentType;
    exit;
  end;

  orgTarget  := nil;
  orgCurrent := nil;
  if (TargetType.InheritRoot is TSE2Type) and not (TargetType is TSE2Array) then
  begin
     orgTarget  := TargetType;
     TargetType := TSE2Type(TargetType.InheritRoot);
  end;
  if (CurrentType.InheritRoot is TSE2Type) and not (CurrentType is TSE2Array) then
  begin
     orgCurrent  := CurrentType;
     CurrentType := TSE2Type(CurrentType.InheritRoot);
  end;

  if Operation in [sesImplicitCast] then
  begin
    result := ImplicitCast;
    exit;
  end;

  case TargetType.AType of
  btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64 :
      begin
        result := CurrentType.AType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64];
      end;
  btSingle, btDouble :
      begin
        result := CurrentType.AType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64, btSingle, btDouble];
      end;
  btString, btUTF8String, btWideString, btPChar, btAnsiString, btPAnsiChar, btPWideChar :
      begin
        result := CurrentType.AType in [btString, btUTF8String, btWideString, btPChar, btAnsiString, btPAnsiChar, btPWideChar];
      end;
  btPointer :
      begin
        result := CurrentType.AType in [btObject, btPointer, btRecord, btProcPtr];
      end;
  btRecord  :
      begin
        result := (CurrentType = TargetType) or (CurrentType.AType in [btObject, btPointer]);
      end;
  btArray :
      begin
        result := (CurrentType = TargetType) or
                 IsCompatible(TSE2Array(TargetType).Content.AType, CurrentType, Operation, StrictInt, AInstance);
      end;
  btObject  :
      begin
        result := CurrentType.AType in [btObject, btPointer];
        if result then
          if orgCurrent is TSE2Class then
            if orgTarget is TSE2Class then
              if not ClassesAreCompatible(TSE2Class(orgTarget), TSE2Class(orgCurrent)) then
              begin
                result := False;
                if AInstance <> nil then
                   AInstance.RaiseError(petError,
                     Format('Classes are not compatible: "%s" can not be assigned to "%s"',
                       [
                         orgCurrent.GetStrongName,
                         orgTarget.GetStrongName
                       ]
                     )
                   );
              end;

      end;
  btProcPtr :
      begin
        result := CurrentType.AType in [btPointer, btProcPtr];
        if result and (AInstance <> nil) then
          if AInstance.FParserState.LastMethod <> nil then
             result := CompareMethods(TSE2MethodVariable(TargetType).Method, AInstance.FParserState.LastMethod,
                       CurrentType.AType = btPointer)
          else
             result := True;
      end;
  else
      result := False;
  end;

  if Operation in [sesSmaller, sesSmallerEqual, sesEqual, sesBigger, sesBiggerEqual, sesUnEqual] then
    if not result then
    begin
      if IsCompatible(orgCurrent, orgTarget, sesNone, StrictInt, AInstance) then
         result := True;
    end;

  if result and (Operation <> sesNone) then
  begin
    case TargetType.AType of
    btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64 :
        begin
          result := Operation in [sesPlus, sesMinus, sesStar, sesDiv, sesShl, sesShr, sesNot, sesAnd, sesOr, sesXor, sesMod,
                                  sesSmaller, sesSmallerEqual, sesEqual, sesBiggerEqual, sesBigger, sesUnEqual];
        end;
    btSingle, btDouble :
        begin
          result := Operation in [sesPlus, sesMinus, sesStar, sesSlash,
                                  sesSmaller, sesSmallerEqual, sesEqual, sesBiggerEqual, sesBigger, sesUnEqual];
        end;
    btString, btUTF8String, btWideString, btPChar, btAnsiString, btPAnsiChar, btPWideChar :
        begin
          result := Operation in [sesPlus, sesSmaller, sesSmallerEqual, sesEqual, sesBiggerEqual, sesBigger, sesUnEqual];
        end;
    btPointer, btObject :
        begin
          result := Operation in [sesEqual, sesUnEqual];
        end;
    btRecord :
        begin
          result := Operation in [sesEqual, sesUnEqual];
        end;
    else
          result := False;
    end;
  end;
end;

function TSE2Parser.Operators(State: TSE2ParseState; Method: TSE2Method;
  TargetType: TSE2Type): TSE2Type;
begin
  if Tokenizer.Token.AType in [sesNot] then
  begin
    ReadNextToken;
    result := Factor(State, Method, TargetType);
    if not IsCompatible(GetIntegerType, result, sesNot, True, Self) then
       RaiseError(petError, 'Ordinal expression expected');

    if result.InheritRoot = GetBooleanType then
       GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(21), ''))
    else
       GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(20), ''));
  end else
    result := Factor(State, Method, TargetType);
end;

function TSE2Parser.Factor(State: TSE2ParseState; Method: TSE2Method;
  TargetType: TSE2Type): TSE2Type;
var StringIndex: string;
    aType      : TSE2Type;
    mustBeEnum : boolean;
begin
  result := nil;
  case Tokenizer.Token.AType of
  sesOpenBracket :
      begin
        ReadNextToken;
        if Tokenizer.Token.AType = sesCloseBracket then
        begin
          if TargetType is TSE2SetOf then
          begin                    
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btS64), ''));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetInt(0), ''));
            State.IncStack;

            result := TargetType;
          end else
            RaiseError(petError, 'Expression not supported yet');
        end else
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btS64), ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetInt(0), ''));
          State.IncStack;

          mustBeEnum := False;

          repeat
            aType := IdentifierExpression(State, Method, nil, True);
            if aType = nil then
               RaiseError(petError, GenIncompatibleExpression(nil, nil));

            if (aType is TSE2SetOf) then
            begin
              result := aType;
              mustBeEnum := True;
              GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_OPERATION(7), ''));
              State.DecStack;
            end else
            if mustBeEnum then
               RaiseError(petError, GenIncompatibleExpression(nil, nil));

            if Tokenizer.Token.AType = sesColon then
            begin
              ReadNextToken;
              if Tokenizer.Token.AType = sesCloseBracket then
                 RaiseError(petError, 'Unexpected symbol: "]"');
            end else
              ExpectToken([sesCloseBracket]);
          until Tokenizer.Token.AType = sesCloseBracket;
          ExpectToken([sesCloseBracket]);
        end;
        ReadNextToken;
      end;
  sesOpenRound :
      begin
        ReadNextToken;
        result := Expression(State, Method, TargetType);
        ExpectToken([sesCloseRound]);
        ReadNextToken;
      end;
  sesInteger :
      begin
        if Tokenizer.Token.AsInteger and int64($FFFFFFFF00000000) <> 0 then
        begin
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btS64), ''));
           result := GetIntegerType(True);
        end else
        begin
           GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btS32), ''));
           result := GetIntegerType(False);
        end;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetInt(Tokenizer.Token.AsInteger), ''));
        State.IncStack;
        ReadNextToken;
      end;
  sesFloat :
      begin
        result := GetDoubleType;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btDouble), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetFloat(Tokenizer.Token.AsFloat), ''));
        State.IncStack;    
        ReadNextToken;
      end;
  sesNil :
      begin
        result := GetPointerType;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btPointer), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetPtr(nil), ''));
        State.IncStack;   
        ReadNextToken;
      end;
  sesString :
      begin
        result := GetStringType;
        StringIndex := FUnit.Strings.Add(FUnit, Tokenizer.Token.Value).GenLinkerName;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btString), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_LOADRES(0), StringIndex));
        State.IncStack;
        ReadNextToken;
      end;
  sesAt    :
      begin
        result := GetPointerType;
        AtStatement(State, Method);
      end;
  sesIdentifier, sesUnit :
      begin
        result := IdentifierExpression(State, Method, nil);
        if result = nil then
           RaiseError(petError, 'Expression does not return a value');

      end;
  sesInherited :
      begin
        result := InheritedExpression(State, Method);
        if result = nil then
           RaiseError(petError, 'Expression does not return a value');
      end;
  sesSizeOf :
      begin
        ExpectToken([sesSizeOf]);
        ReadNextToken;
        ExpectToken([sesOpenRound]);
        ReadNextToken;
        aType := TypeExpression(Method);
        if aType = nil then
           RaiseError(petError, 'Unknown identifier');
        ReadNextToken;
        ExpectToken([sesCloseRound]);
        ReadNextToken;

        result := GetIntegerType;

        if aType is TSE2Class then
          if TSE2Class(aType).IsHelper then
            RaiseError(petError, 'Helper classes does not have a size');
        
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btS64), ''));
        if aType is TSE2Record then
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetInt(TSE2Record(aType).RecordSize), ''))
        else
        if aType is TSE2Array then
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetInt(TSE2Array(aType).ArraySize), ''))
        else
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetInt(aType.DataSize), ''));
        State.IncStack;
      end;
  else
      begin
        RaiseError(petError, 'Expression expected but found "'+TSE2TokenName[Tokenizer.Token.AType]+'" instead');
      end;
  end;

  if (TargetType <> nil) and (result <> nil) then
  begin
    if (result.AType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64]) and
       (TargetType.AType in [btSingle, btDouble]) then
    begin
      ConvertVariableCode(State, Method, result, TargetType);
      result := TargetType;
    end;
  end;
end;

procedure TSE2Parser.MethodBodyDeclaration(State: TSE2ParseState;
  Method: TSE2Method);
var oldExitList : integer;
    i           : integer;
begin
  if Method.IsVirtual or Method.IsOverride then
     GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DEBUG_META(0), Method.GenLinkerName));

  while True do
  begin
    case Tokenizer.Token.AType of
    sesType       :
        TypeDeclaration(State, Method);// TypeDeclaration
    sesConst      :
        ConstDeclaration(State, Method);
    sesVar        :
        VariableDeclaration(State, Method);// VarDeclaration
    sesProcedure,
    sesFunction   :
        begin
          RaiseError(petError, 'Methods are not allowed here');
        end;
    else
        break;
    end;
  end;


  ExpectToken([sesBegin]);


  oldExitList := Method.Lists.ExitList.Count;
  Method.Lists.ExitTableList.Add(State.StackSize);
  ReadNextToken;

  // + 1 because of the return position
  Method.StackSize := Method.Params.Count + 1;
  if Method.ReturnValue <> nil then
     // + 1 because of the return variable
     Method.StackSize := Method.StackSize + 1;
  PushMethodVariables(State, Method);

  StatementSquence(State, Method);

  Method.Lists.ExitTableList.Delete(Method.Lists.ExitTableList.Count-1);

  for i:=Method.Lists.ExitList.Count-1 downto oldExitList do
  begin
    if PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.ExitList[i]].OpCode)^.Position = 0 then
       PSE2OpFLOW_GOTO(Method.OpCodes[Method.Lists.ExitList[i]].OpCode)^.Position := Method.OpCodes.Count;
    Method.Lists.ExitList.Delete(Method.Lists.ExitList.Count-1);
  end;

  PopMethodVariables(State, Method);
  ExpectToken([sesEnd]);
  ReadNextToken;
  ExpectToken([sesSemiColon]);
  ReadNextToken;

  if State.LoopStackSize <> 0 then
     RaiseError(petError, 'Internal error: stack size correction for loops failed');

  GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));

  for i:=0 to Method.Variables.Count-1 do
  begin
    if not TSE2Variable(Method.Variables[i]).Used then
       RaiseError(petHint, 'Variable "'+Method.Variables[i].Name+'" is declared but never used', Method.Variables[i].DeclPos,
                  Method.Variables[i].DeclLine);
  end;
end;

procedure TSE2Parser.PopMethodVariables(State: TSE2ParseState;
  Method: TSE2Method);
var i: integer;
begin
  for i:=Method.Variables.Count-1 downto 0 do
  begin
    if TSE2Type(TSE2Variable(Method.Variables[i]).AType.InheritRoot) is TSE2Record then
       GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_FREE(0), ''))
    else
    if TSE2Type(TSE2Variable(Method.Variables[i]).AType.InheritRoot) is TSE2Array then
       GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_FREE(0), ''));

    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
  end;
end;

procedure TSE2Parser.PushMethodVariables(State: TSE2ParseState;
  Method: TSE2Method);
var i      : integer;
    Offset : integer;
begin
  Offset := 0;
  if Method.ReturnValue <> nil then
  begin
    Method.ReturnValue.CodePos := -Method.StackSize;
    Offset := 1;
  end;

  for i:=0 to Method.Params.Count-1 do
  begin
    TSE2Parameter(Method.Params[i]).CodePos := - ( Method.StackSize - 1 - (i + Offset) ) ;
  end;

  for i:=0 to Method.Variables.Count-1 do
  begin
    TSE2Variable(Method.Variables[i]).CodePos := i;
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(TSE2Type(TSE2Variable(Method.Variables[i]).AType.InheritRoot).AType), ''));
    if TSE2Variable(Method.Variables[i]).AType is TSE2Record then
    begin
      State.RecordsCreated := State.RecordsCreated + 1;
      GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_MAKE(0, 0), 'META_' + TSE2Record(TSE2Variable(Method.Variables[i]).AType).GenLinkerName));
    end else
    if TSE2Variable(Method.Variables[i]).AType is TSE2Array then
    begin                                                                 
      State.ArraysCreated := State.ArraysCreated + 1;
      if TSE2Array(TSE2Variable(Method.Variables[i]).AType).IsDynamic then
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_MAKE(0, 0), 'META_' + TSE2Array(TSE2Variable(Method.Variables[i]).AType).GenLinkerName))
      else
         GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.ARR_SFL(TSE2Array(TSE2Variable(Method.Variables[i]).AType).ArrayCount, 0), 'META_' + TSE2Array(TSE2Variable(Method.Variables[i]).AType).GenLinkerName));
    end;
  end;

  Method.StackSize := Method.Variables.Count;
end;

procedure TSE2Parser.IdentifierStatement(State: TSE2ParseState;
  Method: TSE2Method);
var IdentifierType : TSE2Type;
    StackSize      : integer;
begin
  StackSize := State.StackSize;
  IdentifierType := IdentifierExpression(State, Method, IdentifierSetterEvent);


  // function call - but result is not used
  if (IdentifierType <> nil) and (State.StackSize > StackSize) then
  begin
    //if IdentifierType is TSE2Record then
    //   GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_FREE(0), ''));
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
    State.DecStack;
  end;
end;

procedure TSE2Parser.IdentifierSetterEvent(Sender: TObject;
  State: TSE2ParseState; Method: TSE2Method; Target: TSE2Type; newValueType: PSE2Type);
var Ident: TSE2Type;
    iPos, iLine : integer;
begin
  State.IsExpression := True;
  ExpectToken([sesBecomes]);
  ReadNextToken;
  
  iPos  := Tokenizer.Reader.Position;
  iLine := Tokenizer.Reader.Line;

  State.TargetType := Target;
  State.AParent    := nil;

  Ident := Expression(State, Method, Target);
  if not IsCompatible(Target, Ident, sesNone, False, Self) then
     RaiseError(petError, GenIncompatibleExpression(Ident, Target), iPos, iLine);

  ConvertVariableCode(State, Method, Ident, Target);
  State.IsExpression := False;

  if newValueType <> nil then
     newValueType^ := Ident;
end;

procedure TSE2Parser.ValidateForwards(State: TSE2ParseState);
var i: integer;
begin
  for i:=0 to FUnit.ElemList.Count-1 do
    if FUnit.ElemList[i] is TSE2Method then
    begin
      if TSE2Method(FUnit.ElemList[i]).IsForwarded and not TSE2Method(FUnit.ElemList[i]).IsAbstract then
        if TSE2Method(FUnit.ElemList[i]).OpCodes.Count = 0 then
        begin
          if TSE2Method(FUnit.ElemList[i]).Parent is TSE2Type then
            RaiseError(petError, 'Method '+FUnit.ElemList[i].GetStrongName+' is not fully declared', FUnit.ElemList[i].DeclPos, FUnit.ElemList[i].DeclLine)
          else
            RaiseError(petError, 'Method '+FUnit.ElemList[i].GetStrongName+' is not fully declared', FUnit.ElemList[i].DeclPos, FUnit.ElemList[i].DeclLine);
        end;
    end;

  for i:=0 to FUnit.TypeList.Count-1 do
    if FUnit.TypeList[i] is TSE2Class then
    begin
      if TSE2Class(FUnit.TypeList[i]).IsForwarded then
         RaiseError(petError, 'Class '+FUnit.TypeList[i].Name + ' is not fully declared', FUnit.TypeList[i].DeclPos, FUnit.TypeList[i].DeclLine);
    end;
end;

function TSE2Parser.MakeCallExOptionMask(Method: TSE2Method): integer;
begin
  // Bits
  //  31  30  29  28  27  26 ...
  //  --------------------------
  //  O   R   C   C   C   P  ... P
  //
  //  O: Has Parent (Self-Parameter)
  //  R: Has Return value
  //  C: Call Convention
  //  P: Parameter Count
  result := 0;
  if Method.HasSelfParam then
     result := result or (1 shl 31);
  if Method.ReturnValue <> nil then
     result := result or (1 shl 30);

  result := result or ( byte(Method.CallConvention) shl 27 ); 
  result := result or ( Method.Params.Count );
end;

procedure TSE2Parser.GetOverloadedMethods(State: TSE2ParseState;
  Method: TSE2Method; out Target: TSE2BaseTypeList);
var i: integer;

  procedure CheckUnit(AUnit: TSE2Unit);
  var i: integer;
      n: TSE2BaseType;
      m: TSE2Method;
  begin
    for i:=AUnit.ElemList.Count-1 downto 0 do
    begin
      n := AUnit.ElemList[i];
      if (n.Parent = Method.Parent) and (n.NameHash = Method.NameHash) then
      begin
        if n is TSE2Method then
        begin
          m := TSE2Method(n);
          if (m.IsOverload) and (m.IsStatic = Method.IsStatic) then
             if m.IsName(Method.Name) then
                Target.Add(m);
        end;
      end;
    end;
  end;

begin
  Target := TSE2BaseTypeList.Create;
  Target.OwnsObjs := False;
  Target.SetID    := False;

  if not Method.IsOverload then
  begin
    Target.Add(Method);
    exit;
  end;

  for i:=0 to FUnitList.Count-1 do
    CheckUnit(TSE2Unit(FUnitList[i]));
  CheckUnit(FUnit);
end;

function TSE2Parser.FindMatchingMethod(State: TSE2ParseState; Method: TSE2Method; MethodList,
  ParamExpression: TSE2BaseTypeList; IgnoreFirst: boolean): TSE2Method;

type
  TSearchFilter = (sfLoose, sfVarParam, sfNumberType, sfStrictNumberType, sfInheritRoot, sfExactParam);

  function MethodMatchesParam(MethodParam: TSE2Parameter; Param: TSE2ParamExpression; Filter: TSearchFilter): boolean;
  begin
    result := IsCompatible(MethodParam.AType, Param.AType, sesNone, False, nil);
    if (Filter = sfLoose) or (not result) then
       exit;

    if MethodParam.ParameterType = ptVar then
    begin
      if Param.Variable = nil then
         result := False
      else
      if MethodParam.AType <> Param.Variable.AType then
         result := False;
    end;

    if (Filter = sfVarParam) or (not result) then
       exit;

    case TSE2Type(MethodParam.AType.InheritRoot).AType of
    btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64 :
        result := TSE2Type(Param.AType.InheritRoot).AType in
                     [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64];
    btSingle, btDouble :
        result := TSE2Type(Param.AType.InheritRoot).AType in
                     [btSingle, btDouble];
    btString, btUTF8String, btWideString, btPChar, btAnsiString, btPAnsiChar, btPWideChar :
        result := TSE2Type(Param.AType.InheritRoot).AType in
                     [btString, btUTF8String, btWideString, btPChar, btAnsiString, btPAnsiChar, btPWideChar];
    btPointer :
        result := TSE2Type(Param.AType.InheritRoot).AType in
                     [btPointer];
    btObject  :
        result := TSE2Type(Param.AType.InheritRoot).AType in
                     [btObject];
    btRecord  :
        result := Param.AType = MethodParam.AType;
    else result := True;
    end;

    if (Filter = sfNumberType) or (not result) then
       exit;

    result := TSE2Type(MethodParam.AType.InheritRoot).AType = TSE2Type(Param.AType.InheritRoot).AType;

    if (Filter = sfStrictNumberType) or (not result) then
       exit;

    result := Param.AType.InheritRoot = MethodParam.AType.InheritRoot;

    if (Filter = sfInheritRoot) or (not result) then
       exit;

    result := Param.AType = MethodParam.AType;
  end;

  procedure AddOffsets(Method: TSE2Method; MinJumpLimit, Offset: integer);
  var i: integer;
  begin
    for i:=0 to Method.OpCodes.Count-1 do
    begin
      if Method.OpCodes[i].GetJumpPos > MinJumpLimit then
         Method.OpCodes[i].AddOffset(Offset, MinJumpLimit);
    end;
  end;

  procedure MakeParamsCompatible(Method, CallMethod: TSE2Method; ParamList: TSE2BaseTypeList; iParamStart: integer);
  var CodeOffset  : integer;
      param       : TSE2ParamExpression;
      i           : integer;
      OpCode      : TSE2LinkOpCode;
  begin
    CodeOffset := 0;
    for i:=iParamStart to CallMethod.Params.Count-1 do
    begin
      param := TSE2ParamExpression(ParamList[i]);
      if TSE2Parameter(CallMethod.Params[i]).ParameterType = ptVar then
      begin
        if Method.OpCodes[param.CodePos - 1 + CodeOffset].OpCode.OpCode = soDAT_COPY_FROM then
           Method.OpCodes[param.CodePos - 1 + CodeOffset].OpCode.OpCode := soDAT_MOVE_FROM;
      end else
      begin
        if param.Variable <> nil then
          if param.Parent <> nil then
          begin
            OpCode := TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_UNREF, '');
            Method.OpCodes.Insert(param.CodePos + CodeOffset, OpCode);
            AddOffsets(Method, param.CodePos + CodeOffset - 1, 1);
            CodeOffset := CodeOffset + 1;
          end;
          
        OpCode := ConvertVariableCode(State, Method, param.AType, TSE2Parameter(CallMethod.Params[i]).AType, False);
        if OpCode <> nil then
        begin
          Method.OpCodes.Insert(param.CodePos + CodeOffset, OpCode);
          AddOffsets(Method, param.CodePos + CodeOffset - 1, 1);
          CodeOffset := CodeOffset + 1;
        end;
      end;
    end;
  end;

var i, p        : integer;
    iParamStart : integer;
    m           : TSE2Method;
begin
  result := nil;
  // first of all - delete every method with different number of parameters
  for i:=MethodList.Count-1 downto 0 do
    if TSE2Method(MethodList[i]).Params.Count <> ParamExpression.Count then
       MethodList.Delete(i)
    else
    if State.NoStaticPointer and not TSE2Method(MethodList[i]).IsStatic then
       MethodList.Delete(i);

  if MethodList.Count = 0 then
     exit;

  iParamStart := 0;
  if ParamExpression.Count > 0 then
    if TSE2ParamExpression(ParamExpression[0]).CodePos = -1 then
       iParamStart := 1;
  for i:=MethodList.Count-1 downto 0 do
  begin
    for p:=iParamStart to TSE2Method(MethodList[i]).Params.Count-1 do
      if not (MethodMatchesParam(TSE2Parameter(TSE2Method(MethodList[i]).Params[p]), TSE2ParamExpression(ParamExpression[p]), sfLoose)) then
      begin
        MethodList.Delete(i);
        break;
      end;
  end;

  if MethodList.Count = 0 then
     exit;

  for i:=MethodList.Count-1 downto 0 do
  begin
    m := TSE2Method(MethodList[i]);
    for p:=iParamStart to m.Params.Count-1 do
    begin
      if not (MethodMatchesParam(TSE2Parameter(m.Params[p]), TSE2ParamExpression(ParamExpression[p]), sfVarParam)) then
      begin
        MethodList.Delete(i);
        break;
      end;
    end;
  end;

  if MethodList.Count = 0 then
     exit;

  if MethodList.Count > 1 then
  begin
    for i:=MethodList.Count-1 downto 0 do
    begin
      for p:=iParamStart to TSE2Method(MethodList[i]).Params.Count-1 do
        if not (MethodMatchesParam(TSE2Parameter(TSE2Method(MethodList[i]).Params[p]), TSE2ParamExpression(ParamExpression[p]), sfNumberType)) then
        begin
          MethodList.Delete(i);
          break;
        end;
      if MethodList.Count < 2 then
         break;
    end;

    if MethodList.Count = 0 then
       exit;

    if MethodList.Count > 1 then
    begin
      for i:=MethodList.Count-1 downto 0 do
      begin
        for p:=iParamStart to TSE2Method(MethodList[i]).Params.Count-1 do
          if not (MethodMatchesParam(TSE2Parameter(TSE2Method(MethodList[i]).Params[p]), TSE2ParamExpression(ParamExpression[p]), sfStrictNumberType)) then
          begin
            MethodList.Delete(i);
            break;
          end;

        if MethodList.Count < 2 then
           break;
      end;

      if MethodList.Count = 0 then
         exit;

      if MethodList.Count > 1 then
      begin
        for i:=MethodList.Count-1 downto 0 do
        begin
          for p:=iParamStart to TSE2Method(MethodList[i]).Params.Count-1 do
            if not (MethodMatchesParam(TSE2Parameter(TSE2Method(MethodList[i]).Params[p]), TSE2ParamExpression(ParamExpression[p]), sfInheritRoot)) then
            begin
              MethodList.Delete(i);
              break;
            end;

          if MethodList.Count < 2 then
             break;
        end;

        if MethodList.Count = 0 then
           exit;

        if MethodList.Count > 1 then
        begin
          for i:=MethodList.Count-1 downto 0 do
          begin
            for p:=iParamStart to TSE2Method(MethodList[i]).Params.Count-1 do
              if not (MethodMatchesParam(TSE2Parameter(TSE2Method(MethodList[i]).Params[p]), TSE2ParamExpression(ParamExpression[p]), sfExactParam)) then
              begin
                MethodList.Delete(i);
                break;
              end;

            if MethodList.Count < 2 then
               break;
          end;
          if MethodList.Count > 1 then
          begin
             RaiseError(petError, 'More than one method matches to the parameters');
             exit;
          end;
        end;
      end;
    end;
  end;

  
  if MethodList.Count = 0 then
     exit;

  result := TSE2Method(MethodList[0]);
  MakeParamsCompatible(Method, result, ParamExpression, iParamStart);
end;

procedure TSE2Parser.SetUnit(AUnit: TSE2Unit);
begin
  if AUnit = FUnit then
     exit;
     
  if FOwnsUnit then
     FreeAndNil(FUnit);
  FUnit := AUnit;
  FOwnsUnit := False;
end;

procedure TSE2Parser.ConstDeclaration(State: TSE2ParseState;
  Method: TSE2Method);
var aType     : TSE2Type;
    ConstName : string;
    oldC      : TSE2Constant;
    cnst      : TSE2Constant;
    neg       : boolean;
begin
  State.TargetType := nil;
  State.AParent    := nil;

  ExpectToken([sesConst]);
  ReadNextToken;

  while True do
  begin
    ExpectToken([sesIdentifier]);
    ConstName := Tokenizer.Token.Value;
    if FindIdentifier(Method, Tokenizer.Token.Value, State.CurrentOwner) <> nil then
    begin
      RaiseError(petError, 'Identifier is already in usage: "'+Tokenizer.Token.Value+'"');
      exit;
    end;
    ReadNextToken;

    ExpectToken([sesDoublePoint, sesEqual]);
    aType := nil;
    if Tokenizer.Token.AType = sesDoublePoint then
    begin
      ReadNextToken;
      ExpectToken([sesIdentifier]);
      aType := TypeExpression(Method);
      //aType := TSE2Type(FindIdentifier(Method, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Type])));
      if aType = nil then
         RaiseError(petError, 'Unkown identifier: "'+Tokenizer.Token.Value+'"');

      if aType is TSE2Class then
         RaiseError(petError, 'Classes can not be a constant type');

      if aType is TSE2Record then
         RaiseError(petError, 'Records can not be a constant type');

      ReadNextToken;
    end;
    ExpectToken([sesEqual]);
    ReadNextToken;

    neg := False;
    if aType = nil then
    begin
      ExpectToken([sesInteger, sesFloat, sesString, sesMinus, sesIdentifier]);
      if Tokenizer.Token.AType = sesIdentifier then
      begin
        oldC := TSE2Constant(FindIdentifier(Method, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Constant])));
        if oldC = nil then
           RaiseError(petError, 'Unknown identifier "'+Tokenizer.Token.Value+'"');

        cnst := TSE2Constant.Create;
        DeclareType(State, cnst, ConstName);
        cnst.AType := oldC.AType;
        cnst.Value := oldC.Value;
        if Method <> nil then
           Method.Types.Add(cnst)
        else
        begin
          cnst.Parent := State.CurrentOwner;
          FUnit.ElemList.Add(cnst);
        end;
      end else
      begin
        if Tokenizer.Token.AType = sesMinus then
        begin
          neg := True;
          ReadNextToken;
          ExpectToken([sesInteger, sesFloat]);
        end;

        cnst := TSE2Constant.Create;
        DeclareType(State, cnst, ConstName);
        case Tokenizer.Token.AType of
        sesInteger :
            begin
              cnst.AType := GetIntegerType;
              cnst.AsInteger := Tokenizer.Token.AsInteger;
              if neg then
                 cnst.AsInteger := -cnst.AsInteger;
            end;
        sesFloat :
            begin
              cnst.AType   := GetDoubleType;
              cnst.AsFloat := Tokenizer.Token.AsFloat;
              if neg then
                 cnst.AsFloat := -cnst.AsFloat;
            end;
        sesString :
            begin
              cnst.AType   := GetStringType;
              cnst.Value   := Tokenizer.Token.Value;
            end;
        end;
        if Method <> nil then
           Method.Types.Add(cnst)
        else
        begin
          cnst.Parent := State.CurrentOwner;
          FUnit.ElemList.Add(cnst);
        end;
      end;
    end else
    begin
      //if aType.InheritRoot is TSE2Type then
      //   aType := TSE2Type(aType.InheritRoot);
      case TSE2Type(aType.InheritRoot).AType of
      btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64 :
          begin
            ExpectToken([sesInteger, sesMinus]);
            if Tokenizer.Token.AType = sesMinus then
            begin
              neg := True;
              ReadNextToken;
              ExpectToken([sesInteger]);
            end;

            cnst := TSE2Constant.Create;
            cnst.AType := aType;
            DeclareType(State, cnst, ConstName);
            cnst.AsInteger := Tokenizer.Token.AsInteger;
            if neg then
               cnst.AsInteger := -cnst.AsInteger;

            if Method <> nil then
               Method.Types.Add(cnst)
            else
            begin
              cnst.Parent := State.CurrentOwner;
              FUnit.ElemList.Add(cnst);
            end;
          end;
      btSingle, btDouble :
          begin
            ExpectToken([sesInteger, sesFloat, sesMinus]);
            if Tokenizer.Token.AType = sesMinus then
            begin
              neg := True;
              ReadNextToken;
              ExpectToken([sesInteger, sesFloat]);
            end;

            cnst := TSE2Constant.Create;
            cnst.AType := aType;
            DeclareType(State, cnst, ConstName);
            cnst.AsFloat := Tokenizer.Token.AsFloat;
            if neg then
               cnst.AsFloat := -cnst.AsFloat;

            if Method <> nil then
               Method.Types.Add(cnst)
            else
            begin
              cnst.Parent := State.CurrentOwner;
              FUnit.ElemList.Add(cnst);
            end;
          end;
      btString, btWideString, btUTF8String, btPChar, btAnsiString, btPAnsiChar, btPWideChar :
          begin
            ExpectToken([sesString]);
            
            cnst := TSE2Constant.Create;  
            cnst.AType := GetStringType;
            DeclareType(State, cnst, ConstName);
            cnst.Value := Tokenizer.Token.Value;

            if Method <> nil then
               Method.Types.Add(cnst)
            else
            begin
              cnst.Parent := State.CurrentOwner;
              FUnit.ElemList.Add(cnst);
            end;
          end;
      else
          RaiseError(petError, 'Unsupported const type');
      end;
    end;
    ReadNextToken;
    ExpectToken([sesSemiColon]);
    ReadNextToken;

    if ((State.CurrentOwner is TSE2Class) or (State.CurrentOwner is TSE2Record)) and (State.IsStatic) and (Method = nil) then
       break
    else
    if not (Tokenizer.Token.AType in [sesIdentifier]) then
       break;

    //if (not (Tokenizer.Token.AType in [sesIdentifier])) or (State.CurrentOwner <> nil) then
    //   break;
  end;
  //
end;

procedure TSE2Parser.RegisterCallBack(TargetPos: integer;
  Event: TSE2CompileCallBack);
begin
  FCallBackPos := TargetPos;
  FCompileCall := Event;
end;

procedure TSE2Parser.PropertyDeclaration(State: TSE2ParseState);
var aProperty : TSE2Property;
    aSource   : TSE2BaseType;
    aParams   : TSE2BaseTypeList;
    iParams   : integer;
    aParamType: TSE2BaseType;
    i, start  : integer;
    Item      : TSE2BaseType;
    oldParent : TSE2BaseType;
    genParam  : TSE2Parameter;
begin
  if State.CurrentOwner = nil then
     RaiseError(petError, 'Internal error: property not in class');

  ExpectToken([sesProperty]);
  ReadNextToken;
  ExpectToken([sesIdentifier]);

  Item := FindIdentifier(nil, Tokenizer.Token.Value, State.CurrentOwner, FUnit.Name);
  if Item = nil then
    if State.CurrentOwner is TSE2Class then
      if TSE2Class(State.CurrentOwner).IsPartial and not FUnit.IsName(State.CurrentOwner.AUnitName) then
        Item := FindIdentifier(nil, Tokenizer.Token.Value, State.CurrentOwner, State.CurrentOwner.AUnitName);

  if Item <> nil then
    if Item.Parent = State.CurrentOwner then
    begin
      RaiseError(petError, 'Identifier already declared: "'+Tokenizer.Token.Value+'"');
    end;



  aProperty := TSE2Property.Create;
  DeclareType(State, aProperty, Tokenizer.Token.Value);
  aProperty.IsStatic := State.IsStatic;
  aProperty.Parent   := State.CurrentOwner;
  aParams := TSE2BaseTypeList.Create;
  aParams.OwnsObjs := False;
  aParams.OwnsObj  := False;
  try
    oldParent := State.AParent;
    State.AParent := nil;
    ReadNextToken;
    ExpectToken([sesDoublePoint, sesOpenBracket]);
    if Tokenizer.Token.AType = sesOpenBracket then
    begin
      ReadNextToken;
      repeat
        iParams := 1;
        ExpectToken([sesIdentifier]);
        if aProperty.Params.FindItem(Tokenizer.Token.Value) <> nil then
           RaiseError(petError, 'Name already declared');
        aProperty.AddParam(Tokenizer.Token.Value, nil);
        ReadNextToken;
        while (Tokenizer.Token.AType in [sesColon]) do
        begin
          ExpectToken([sesColon]);
          ReadNextToken;
          ExpectToken([sesIdentifier]);
          if aProperty.Params.FindItem(Tokenizer.Token.Value) <> nil then
             RaiseError(petError, 'Name already declared');
          aProperty.AddParam(Tokenizer.Token.Value, nil);
          ReadNextToken;
          iParams := iParams + 1;
        end;
        ExpectToken([sesDoublePoint]);
        ReadNextToken;
        ExpectToken([sesIdentifier]);

        aParamType := TypeExpression(nil);
        //aParamType := FindIdentifier(nil, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Type]));
        if aParamType = nil then
           RaiseError(petError, 'Unkown identifier: "'+Tokenizer.Token.Value+'"')
        else
        begin
          aProperty.FillEmptyParams(TSE2Type(aParamType));
          while iParams > 0 do
          begin
            aParams.Add(aParamType);
            iParams := iParams - 1;
          end;
        end;
        ReadNextToken;
        ExpectToken([sesSemiColon, sesCloseBracket]);
        if Tokenizer.Token.AType = sesSemiColon then
        begin
           ReadNextToken;
           ExpectToken([sesIdentifier]);
        end;
      until (Tokenizer.Token.AType = sesCloseBracket);
      ReadNextToken;
    end;
    ExpectToken([sesDoublePoint]);
    ReadNextToken;
    ExpectToken([sesIdentifier]);
    aProperty.AType := TypeExpression(nil);
    //aProperty.AType := TSE2Type(FindIdentifier(nil, Tokenizer.Token.Value, nil, '', TSE2BaseTypeFilter.Create([TSE2Type])));
    if aProperty.AType = nil then
       RaiseError(petError, 'Unknown type identifier: "'+Tokenizer.Token.Value+'"');

    State.AParent := oldParent;
    ReadNextToken;
    if (not StringIdentical(Tokenizer.Token.Value, 'read')) and
       (not StringIdentical(Tokenizer.Token.Value, 'write')) then
       RaiseError(petError, '"read" or "write" expected');

    if StringIdentical(Tokenizer.Token.Value, 'read') then
    begin
      ReadNextToken;
      if Tokenizer.Token.AType in [sesAnd, sesSemiColon] then
      begin
        if State.CurrentOwner is TSE2Record then
           RaiseError(petError, 'Auto generated properties are not supported inside records')
        else
        if State.CurrentOwner is TSE2Class then
        begin
          if not (TSE2Class(State.CurrentOwner).IsTypeOf(GetExternalObjectType)) then
          begin
            RaiseError(petError, 'Auto generated properties must be used in external classes');
          end;

          if TSE2Class(State.CurrentOwner).IsHelper then
             RaiseError(petError, 'Auto generated properties can not be used for helper classes');
        end
        else
           RaiseError(petError, 'Auto generated properties are not supported for the current context');
                                                             
        aSource := TSE2Method.Create;
        DeclareType(State, aSource, '|Get' + aProperty.Name);
        aSource.Parent := State.CurrentOwner;
        TSE2Method(aSource).IsStatic := aProperty.IsStatic;
        TSE2Method(aSource).IsExternal := True;
        TSE2Method(aSource).MethodType := mtFunction;
        TSE2Method(aSource).ReturnValue := TSE2Variable.Create;
        TSE2Method(aSource).ReturnValue.AType := aProperty.AType;

        genParam := TSE2Parameter.Create;
        DeclareType(State, genParam, 'Self');
        genParam.AType := State.CurrentOwner;
        genParam.IsStatic := False;
        if TSE2Method(aSource).IsStatic  then
           genParam.Name := '!Self';

        TSE2Method(aSource).Params.Add(genParam);

        for i:=0 to aProperty.Params.Count-1 do
        begin
          genParam := TSE2Parameter.Create;
          DeclareType(State, genParam, aProperty.Params[i].Name);
          genParam.AType := TSE2Variable(aProperty.Params[i]).AType;
          TSE2Method(aSource).Params.Add(genParam);
        end;

        FUnit.ElemList.Add(aSource);
        aProperty.Getter := aSource;

        if Tokenizer.Token.AType = sesAnd then
        begin
           ReadNextToken;
           ExpectToken([sesIdentifier]);
        end;

        

        
        if TSE2Method(aSource).ExternalLib <> '' then
           GenCode(TSE2Method(aSource), TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_LOADEX, ''));
        GenCode(TSE2Method(aSource), TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALLEX(0, 0), TSE2Method(aSource).GenLinkerName));
        GenCode(TSE2Method(aSource), TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));

        {
          - GENERATE read method for external classes
        }

      end else
      begin
        ExpectToken([sesIdentifier]);
        aSource := FindIdentifier(nil, Tokenizer.Token.Value, State.CurrentOwner, '',
                                  TSE2BaseTypeFilter.Create([TSE2Variable, TSE2Method]));

        if aSource = nil then
           RaiseError(petError, 'Unkown identifier: "'+Tokenizer.Token.Value+'"');

        CheckForDeprecated(aSource);

        if aSource is TSE2Method then
        begin
          if TSE2Method(aSource).IsStatic <> aProperty.IsStatic then
          begin
            if aProperty.IsStatic then
               RaiseError(petError, 'static properties can not access non-static elements')
            else
               RaiseError(petError, 'non-static properties can not access static elements');
          end;

          if TSE2Method(aSource).IsOverload then
             RaiseError(petError, 'Method can not be declared as overload');

          if TSE2Method(aSource).ReturnValue = nil then
             RaiseError(petError, 'Method does not return any value');

          if TSE2Method(aSource).ReturnValue.AType <> aProperty.AType then
             RaiseError(petError, 'Incompatible return value');

          if TSE2Method(aSource).HasSelfParam then
          begin
            start := 1;
            if TSE2Method(aSource).Params.Count <> aParams.Count + 1 then
               RaiseError(petError, 'Incompatible method: method parameter count is not compatible');
          end else
          begin
            start := 0;
            if TSE2Method(aSource).Params.Count <> aParams.Count then
               RaiseError(petError, 'Incompatible method: method parameter count is not compatible');
          end;

          for i:=start to aParams.Count-1 do
          begin
            if TSE2Parameter(TSE2Method(aSource).Params[i]).AType <> TSE2Type(aParams[i]) then
               RaiseError(petError, 'Parameter declaration of the getter function not compatible');
            if TSE2Parameter(TSE2Method(aSource).Params[i]).ParameterType in [ptConst, ptVar] then
               RaiseError(petError, 'Parameter modifier not allowed');
          end;

          aProperty.Getter := aSource;
        end else
        if aSource is TSE2Variable then
        begin
          if aProperty.Parent = nil then
             RaiseError(petError, 'Internal error: property parent is nil');

          if (not aProperty.Parent.IsTypeOf(TSE2Variable(aSource).Parent)) and
             (TSE2Variable(aSource).Parent <> aProperty.Parent)  then
             RaiseError(petError, 'Property variable must have the same owner');

          if TSE2Variable(aSource).IsStatic <> aProperty.IsStatic then
          begin
            if aProperty.IsStatic then
               RaiseError(petError, 'static properties can not access non-static elements')
            else
               RaiseError(petError, 'non-static properties can not access static elements');
          end;

          if aParams.Count > 0 then
             RaiseError(petError, 'Indexed properites can not link to a variable');

          if TSE2Variable(aSource).AType <> aProperty.AType then
             RaiseError(petError, 'Variable and property type must be the same');

          aProperty.Getter := aSource;
          TSE2Variable(aSource).Used := True;
          //RaiseError(petError, 'Variables are not supported yet');
        end else
          RaiseError(petError, 'Internal error: Unknown source');


        ReadNextToken;
      end;
    end;
    if StringIdentical(Tokenizer.Token.Value, 'write') then
    begin
      ReadNextToken;

      if Tokenizer.Token.AType in [sesSemiColon] then
      begin
        if State.CurrentOwner is TSE2Record then
           RaiseError(petError, 'Auto generated properties are not supported inside records')
        else
        if State.CurrentOwner is TSE2Class then
        begin
          if not (TSE2Class(State.CurrentOwner).IsTypeOf(GetExternalObjectType)) then
          begin
            RaiseError(petError, 'Auto generated properties must be used in external classes');
          end;

          if TSE2Class(State.CurrentOwner).IsHelper then
             RaiseError(petError, 'Auto generated properties can not be used for helper classes');
        end
        else
           RaiseError(petError, 'Auto generated properties are not supported for the current context');
                                                             
        aSource := TSE2Method.Create;
        DeclareType(State, aSource, '|Set' + aProperty.Name);
        aSource.Parent := State.CurrentOwner;
        TSE2Method(aSource).IsStatic := aProperty.IsStatic;
        TSE2Method(aSource).IsExternal := True;
        TSE2Method(aSource).MethodType := mtProcedure;

        genParam := TSE2Parameter.Create;
        DeclareType(State, genParam, 'Self');
        genParam.AType := State.CurrentOwner;
        genParam.IsStatic := False;
        if TSE2Method(aSource).IsStatic  then
           genParam.Name := '!Self';

        TSE2Method(aSource).Params.Add(genParam);

        for i:=0 to aProperty.Params.Count-1 do
        begin
          genParam := TSE2Parameter.Create;
          DeclareType(State, genParam, aProperty.Params[i].Name);
          genParam.AType := TSE2Variable(aProperty.Params[i]).AType;
          TSE2Method(aSource).Params.Add(genParam);
        end;

        genParam := TSE2Parameter.Create;
        DeclareType(State, genParam, 'newValue');
        genParam.AType := aProperty.AType;
        genParam.IsStatic := False;    
        TSE2Method(aSource).Params.Add(genParam);

        FUnit.ElemList.Add(aSource);
        aProperty.Setter := aSource;

        
        if TSE2Method(aSource).ExternalLib <> '' then
           GenCode(TSE2Method(aSource), TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_LOADEX, ''));
        GenCode(TSE2Method(aSource), TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALLEX(0, 0), TSE2Method(aSource).GenLinkerName));
        GenCode(TSE2Method(aSource), TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));

      end else
      begin
        ExpectToken([sesIdentifier]);
        aSource := FindIdentifier(nil, Tokenizer.Token.Value, State.CurrentOwner, '',
                                  TSE2BaseTypeFilter.Create([TSE2Variable, TSE2Method]));

        if aSource = nil then
           RaiseError(petError, 'Unkown identifier: "'+Tokenizer.Token.Value+'"');

        CheckForDeprecated(aSource);

        if aSource is TSE2Method then
        begin
          if TSE2Method(aSource).IsStatic <> aProperty.IsStatic then
          begin
            if aProperty.IsStatic then
               RaiseError(petError, 'static properties can not access non-static elements')
            else
               RaiseError(petError, 'non-static properties can not access static elements');
          end;

          if TSE2Method(aSource).ReturnValue <> nil then
             RaiseError(petError, 'Incompatible method');

          if TSE2Method(aSource).HasSelfParam then
          begin
            start := 1;
            if TSE2Method(aSource).Params.Count <> aParams.Count + 2 then
               RaiseError(petError, 'Incompatible method: method parameter count is not compatible');
            if TSE2Parameter(TSE2Method(aSource).Params[TSE2Method(aSource).Params.Count-1]).ParameterType = ptVar then
               RaiseError(petError, 'Incompatible method: parameter not compatible');
            if TSE2Parameter(TSE2Method(aSource).Params[TSE2Method(aSource).Params.Count-1]).AType <> aProperty.AType then
               RaiseError(petError, 'Incompatible method: parameter not compatible');
          end else
          begin
            start := 0;
            if TSE2Method(aSource).Params.Count <> aParams.Count + 1 then
               RaiseError(petError, 'Incompatible method: method parameter count is not compatible');
            if TSE2Parameter(TSE2Method(aSource).Params[TSE2Method(aSource).Params.Count-1]).ParameterType = ptVar then
               RaiseError(petError, 'Incompatible method: parameter not compatible');
            if TSE2Parameter(TSE2Method(aSource).Params[TSE2Method(aSource).Params.Count-1]).AType <> aProperty.AType then
               RaiseError(petError, 'Incompatible method: parameter not compatible');
          end;

          for i:=start to aParams.Count-1 do
          begin
            if TSE2Parameter(TSE2Method(aSource).Params[i]).AType <> TSE2Type(aParams[i]) then
               RaiseError(petError, 'Parameter declaration of the getter function not compatible');
            if TSE2Parameter(TSE2Method(aSource).Params[i]).ParameterType in [ptConst, ptVar] then
               RaiseError(petError, 'Parameter modifier not allowed');
          end;

          (*


          *)

          aProperty.Setter := aSource;
        end else
        if aSource is TSE2Variable then
        begin
          if aProperty.Parent = nil then
             RaiseError(petError, 'Internal error: property parent is nil');

          if (not aProperty.Parent.IsTypeOf(TSE2Variable(aSource).Parent)) and
             (TSE2Variable(aSource).Parent <> aProperty.Parent) then
             RaiseError(petError, 'Property variable must have the same owner');

          if TSE2Variable(aSource).IsStatic <> aProperty.IsStatic then
          begin
            if aProperty.IsStatic then
               RaiseError(petError, 'static properties can not access non-static elements')
            else
               RaiseError(petError, 'non-static properties can not access static elements');
          end;

          if aParams.Count > 0 then
             RaiseError(petError, 'Indexed properties can not link to a variable');

          if TSE2Variable(aSource).AType <> aProperty.AType then
             RaiseError(petError, 'Variable and property type must be the same');

          aProperty.Setter := aSource;
          TSE2Variable(aSource).Used := True;
        end else
          RaiseError(petError, 'Internal error: Unknown source');


        ReadNextToken;
      end;
    end;
    ExpectToken([sesSemiColon]);
    ReadNextToken;

    if not FHasError then
       FUnit.ElemList.Add(aProperty);
  finally
    aParams.Free;
    if FHasError then
       aProperty.Free;
  end;
end;

function TSE2Parser.InheritedExpression(State: TSE2ParseState;
  Method: TSE2Method): TSE2Type;
var pOverwrite : TSE2Method;
    i          : integer;
    diff       : integer;
    pFindType  : TSE2BaseType;
    Meth       : TSE2Method;
begin
  result := nil;
  ExpectToken([sesInherited]);

  if not (Method.Parent is TSE2Class) then
     RaiseError(petError, 'Inherited not allowed here');

  ReadNextToken;



  if Tokenizer.Token.AType = sesIdentifier then
  begin
    if TSE2Class(Method.Parent).InheritFrom = Method.Parent then
       RaiseError(petError, 'No overwritten method available in root classes');

    ExpectToken([sesIdentifier]);
    pFindType := FindIdentifier(Method, Tokenizer.Token.Value, TSE2Class(Method.Parent).InheritFrom, '');

    if pFindType = nil then
       RaiseError(petError, 'Unkown method: ' + Tokenizer.Token.Value);

    if not (pFindType is TSE2Method) then
       RaiseError(petError, 'Only methods are allowed here');

    ReadNextToken;
    Meth := TSE2Method(pFindType);

    if (Meth.AUnitName <> '') and (Meth.Name <> '') then
        {$IFDEF SEII_SMART_LINKING}
        Method.UsedMethods.Add(Meth);
        {$ELSE}
        Meth.Used := True;
        {$ENDIF}

    diff := 0;
    if Meth.HasSelfParam then
    begin
      case TSE2Parameter(Meth.Params[0]).ParameterType of
      ptConst,
      ptDefault : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-(Method.Params.Count + diff + Method.StackSize) - State.StackSize, False), ''));
      ptVar     : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_MOVE_FROM(-(Method.Params.Count + diff + Method.StackSize) - State.StackSize, False), ''));
      end;
    end;

    State.IncStack;
    result := MethodCall(State, Method, Meth, False, nil, False, nil, True);

    {if Meth.HasSelfParam then
    begin
      case TSE2Parameter(Meth.Params[0]).ParameterType of
      ptConst,
      ptDefault : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      ptVar     : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC_NODEL, ''));
      end;
    end;               }

    // Hm, ....
    //if Meth.HasSelfParam then
    //   State.DecStack;
    //if result <> nil then
    //   State.DecStack;
  end else
  begin
    if not Method.IsOverride then
       RaiseError(petError, 'Inherited not allowed here - method must override another method');

    pOverwrite := GetOverwrittenMethod(Method);
    if pOverwrite = nil then
       RaiseError(petError, 'Could not find the overwritten method');

    if (pOverwrite.AUnitName <> '') and (pOverwrite.Name <> '') then
        {$IFDEF SEII_SMART_LINKING}
        Method.UsedMethods.Add(pOverwrite);
        {$ELSE}
        pOverwrite.Used := True;
        {$ENDIF}

    if pOverwrite.IsAbstract then
       exit;
    if pOverwrite.OpCodes.Count = 1 then
      if pOverwrite.OpCodes[0].OpCode.OpCode = soFLOW_RET then
        exit;

    diff := 0;
    if Method.ReturnValue <> nil then
    begin
       //GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-(Method.Params.Count + 1), False), ''));
       diff := 1;
       PushVarToStack(State, Method, Method.ReturnValue, False)
    end else
    if pOverwrite.ReturnValue <> nil then
    begin
       GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(TSE2Type(pOverwrite.ReturnValue.InheritRoot).AType), ''));
       diff := 1;
    end;

    for i:=0 to Method.Params.Count-1 do
    begin
      case TSE2Parameter(Method.Params[i]).ParameterType of
      ptConst,
      ptDefault : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-(Method.Params.Count + diff + Method.StackSize), False), ''));
      ptVar     : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_MOVE_FROM(-(Method.Params.Count + diff + Method.StackSize), False), ''));
      end;
    end;


    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(Method.OpCodes.Count + 2, 0), ''));
    GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALL(0), pOverwrite.GenLinkerName));



    for i:=Method.Params.Count-1 downto 0 do
    begin
      case TSE2Parameter(Method.Params[i]).ParameterType of
      ptConst,
      ptDefault : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      ptVar     : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC_NODEL, ''));
      end;
    end;


    if Method.ReturnValue <> nil then
       PopStackToVar(State, Method, Method.ReturnValue)
    else
    if pOverwrite.ReturnValue <> nil then
       GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
  end;
end;

function TSE2Parser.GetOverwrittenMethod(Method: TSE2Method): TSE2Method;

  function MethodParamsMatches(m1, m2: TSE2Method): boolean;
  var i      : integer;
      p1, p2 : TSE2Parameter;
  begin
    result := False;
    if (m1.Params.Count <> m2.Params.Count) then
       exit;

    if m1.MethodType <> m2.MethodType then
       exit;

    case m1.MethodType of
    mtFunction :
        if m1.ReturnValue.AType <> m2.ReturnValue.AType then
          exit;
    end;

    for i:=1 to m2.Params.Count-1 do
    begin
      p1 := TSE2Parameter(m1.Params[i]);
      p2 := TSE2Parameter(m2.Params[i]);

      if (p1 = nil) or (p2 = nil) then
         exit;

      if p1.ParameterType <> p2.ParameterType then
         exit;

      if p1.AType <> p2.AType then
         exit;
    end;
    result := True;
  end;

  function GetMethodFromUnit(AUnit: TSE2Unit; Method: TSE2Method; aClass: TSE2Class): TSE2Method;
  var iElem: integer;
      p    : TSE2BaseType;
  begin
    for iElem := 0 to AUnit.ElemList.Count-1 do
    begin
      p := AUnit.ElemList[iElem];
      if p is TSE2Method then
        if TSE2Method(p).Parent = aClass then
          if TSE2Method(p).IsVirtual or TSE2Method(p).IsOverride or TSE2Method(p).IsAbstract then
            if p.IsName(Method.Name, Method.NameHash) then
              if MethodParamsMatches(Method, TSE2Method(p)) then
              begin
                result := TSE2Method(p);
                exit;
              end;
    end;
    result := nil;
  end;

  function GetMethod(Method: TSE2Method; aClass: TSE2Class): TSE2Method;
  var iUnit: integer;
  begin
    for iUnit := 0 to FUnitList.Count-1 do
    begin
      result := GetMethodFromUnit(TSE2Unit(FUnitList[iUnit]), Method, aClass);
      if result <> nil then
         exit;
    end;
    result := GetMethodFromUnit(FUnit, Method, aClass);
  end;

var aClass: TSE2Class;
begin
  result := nil;
  if not (Method.Parent is TSE2Class) then
     exit;

  aClass := TSE2Class(Method.Parent);
  if not (aClass.InheritFrom is TSE2Class) then
     exit;

  aClass := TSE2Class(aClass.InheritFrom);
  repeat
    result := GetMethod(Method, aClass);
    if result = nil then
       aClass := TSE2Class(aClass.InheritFrom);
  until (result <> nil) or (aClass = nil);



  //
end;

function TSE2Parser.GenIncompatibleExpression(Current,
  Target: TSE2Type; Operation: TSE2TokenType = sesNone): string;
var OpStr : string;
begin
  OpStr := TSE2TokenString[Operation];
  if OpStr = '' then
    case Operation of
    sesImplicitCast : OpStr := 'implicit cast';
    sesExplicitCast : OpStr := 'explicit cast';
    end;
  if ((Current = nil) or (Target = nil)) and (Operation <> sesNone) then
     result := Format('Operation "%s" for expression not compatible', [OpStr])
  else
  if (Current = nil) or (Target = nil) then
     result := 'Expression not compatible'
  else
  if (Current = Target) and (Operation <> sesNone) then
     result := Format('Operation "%s" is not available for "%s"', [OpStr, Current.GetStrongName])
  else
  if (Current <> Target) and (Operation <> sesNone) then
     result := Format('Operation "%s" can not be applied between "%s" and "%s"', [OpStr, Current.GetStrongName,
                                                            Target.GetStrongName])
  else
  begin
    if (Target is TSE2MethodVariable) and (Current = GetPointerType) then
       result := Format('Method is not compatible to "%s"', [Target.GetStrongName])
    else
       result := Format('Incompatible types: "%s" and "%s"', [Current.GetStrongName,
                                                            Target.GetStrongName]);
  end;
end;

procedure TSE2Parser.AtStatement(State: TSE2ParseState;
  Method: TSE2Method);
var variable  : TSE2Variable;
    CodeIndex : integer;
    aType     : TSE2Type;
begin
  ExpectToken([sesAt]);
  ReadNextToken;

  State.IsAtStatement := True;
  State.LastVariable := nil;
  State.LastProperty := nil;
  aType := IdentifierExpression(State, Method, nil);
  if aType = nil then
     RaiseError(petError, 'invalid expression for @ - operator');

  if (State.LastVariable = nil) and (State.LastMethod = nil) and (State.LastProperty = nil) then
     RaiseError(petError, '@ operator not allowed here');

  if State.LastProperty <> nil then
  begin
    if not (State.LastProperty.Getter is TSE2Variable) then
       RaiseError(petError, '@ not allowed for property getter type');
    State.LastVariable := TSE2Variable(State.LastProperty.Getter);
  end;
           
  if State.LastMethod <> nil then
  begin

  end else
  if State.LastVariable <> nil then
  begin
    variable := State.LastVariable;
    if (variable.AType is TSE2MethodVariable) then
    begin

    end else
    begin
      if Variable is TSE2Parameter then
      begin
        CodeIndex := -(State.StackSize) - (Method.StackSize) + TSE2Parameter(Variable).CodePos;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_GetRef(CodeIndex, False, True), ''));
      end else
      // Is TSE2Variable!
      begin
        if (Variable.IsStatic) then
        begin
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
          GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_GetRef(0, True, True), Variable.GenLinkerName));
        end else
        begin
          if (Variable.Parent = nil) or (Variable.IsStatic) then
          begin
            CodeIndex := -(State.StackSize) - Method.StackSize + 1 + Variable.CodePos;
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_GetRef(CodeIndex, False, True), ''));
          end else
          if Variable.Parent is TSE2Class then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_GetRef(0, False, False), ''));
          end else
          if variable.Parent is TSE2Record then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_GetRef(0, False, False), ''));
          end else
          if variable.Parent is TSE2Array then
          begin
            GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_GetRef(0, False, False), ''));
          end else
            RaiseError(petError, 'Internal error: variable owner not as expected');
        end;
      end;
    end;
  end else
    RaiseError(petError, 'Internal error: at-operator not specified for type');
  State.IsAtStatement := False;
end;

(*
program Project1;

type
  TRect = record
  public
    class procedure Test(i1, i2, i3, i4, i5: integer); overload;  
  end;
  
class procedure TRect.Test(i1, i2, i3, i4, i5: integer);
begin
  Console.WriteLine(i1);
  Console.WriteLine(i2);
  Console.WriteLine(i3);
  Console.WriteLine(i4);
  Console.WriteLine(i5);
end; 

procedure Testen;
var i1, i2, i3, i4, i5: integer;
begin
  i1 := 1;
  i2 := 2;
  i3 := 3;
  i4 := 4;
  i5 := 5;
  TRect.Test(i1, i2, i3, i4, i5);
end;

begin
  Testen;
  Console.ReadKey;
end.
*)

procedure TSE2Parser.IncreaseStackPosition(State: TSE2ParseState; OpCode: PSE2OpDefault;
  Offset, MinStackDist: integer);

  function DistanceOk(Data: integer): boolean;
  begin
    result := -Data >= State.StackSize;  //True; //(State.StackSize + Data - MinStackDist)  < 0;
  end;

begin
  case OpCode.OpCode of
  soDAT_COPY_TO    :
      begin
        if not PSE2OpDAT_COPY_TO(OpCode).Static then
           if DistanceOk(PSE2OpDAT_COPY_TO(OpCode).Target) then
              PSE2OpDAT_COPY_TO(OpCode).Target := PSE2OpDAT_COPY_TO(OpCode).Target + Offset;
      end;
  soDAT_COPY_FROM  :
      begin
        if not PSE2OpDAT_COPY_FROM(OpCode).Static then  
           if DistanceOk(PSE2OpDAT_COPY_FROM(OpCode).Source) then
              PSE2OpDAT_COPY_FROM(OpCode).Source := PSE2OpDAT_COPY_FROM(OpCode).Source + Offset;
      end;
  soDAT_MOVE_TO    :
      begin
        if not PSE2OpDAT_MOVE_TO(OpCode).Static then   
           if DistanceOk(PSE2OpDAT_MOVE_TO(OpCode).Target) then
              PSE2OpDAT_MOVE_TO(OpCode).Target := PSE2OpDAT_MOVE_TO(OpCode).Target + Offset;
      end;
  soDAT_MOVE_FROM  :
      begin
        if not PSE2OpDAT_MOVE_FROM(OpCode).Static then  
           if DistanceOk(PSE2OpDAT_MOVE_FROM(OpCode).Source) then
              PSE2OpDAT_MOVE_FROM(OpCode).Source := PSE2OpDAT_MOVE_FROM(OpCode).Source + Offset;
      end;
  soSPEC_GetRef    :
      begin
        if not PSE2OpSPEC_GetRef(OpCode).Static then   
           if DistanceOk(PSE2OpSPEC_GetRef(OpCode).Offset) then
              PSE2OpSPEC_GetRef(OpCode).Offset := PSE2OpSPEC_GetRef(OpCode).Offset + Offset;
      end;
  soREC_COPY_TO    :
      begin
        if PSE2OpREC_COPY_TO(OpCode).Target >= 0 then
           if DistanceOk(PSE2OpREC_COPY_TO(OpCode).Target) then
              PSE2OpREC_COPY_TO(OpCode).Target := PSE2OpREC_COPY_TO(OpCode).Target + Offset;
      end;
  soARR_COPY_TO :
      begin
        if PSE2OpARR_COPY_TO(OpCode).Target >= 0 then
           if DistanceOk(PSE2OpARR_COPY_TO(OpCode).Target) then
              PSE2OpARR_COPY_TO(OpCode).Target := PSE2OpARR_COPY_TO(OpCode).Target + Offset;
      end;
  end;
end;

procedure TSE2Parser.IncreaseMethodStackPositions(State: TSE2ParseState; Method: TSE2Method;
  Start, Stop, Offset, MinStackDist: integer);
var i: integer;
begin
  for i := Max(0, Start) to Min(Stop, Method.OpCodes.Count - 1) do
    IncreaseStackPosition(State, Method.OpCodes[i].OpCode, Offset, MinStackDist);
end;

procedure TSE2Parser.ProcessDeprecatedExpression(State: TSE2ParseState;
  Elem: TSE2BaseType);
begin
  if Elem <> nil then
    if Tokenizer.Token.AType = sesDeprecated then
    begin
      Elem.IsDeprecated := True;
      ReadNextToken;
      if Tokenizer.Token.AType = sesString then
      begin
         Elem.DeprecatedValue := Tokenizer.Token.Value;
         ReadNextToken;
      end;
    end;
end;

function TSE2Parser.TypeExpression(Method: TSE2Method): TSE2Type;
var aType     : TSE2BaseType;
    sUnitName : string;
    sCheckName: string;
begin
  result := nil;
  ExpectToken([sesIdentifier]);

  sUnitName  := '';
  sCheckName := '';
  while true do
  begin
    aType := FindIdentifier(Method, Tokenizer.Token.Value, nil, sUnitName, TSE2BaseTypeFilter.Create([TSE2Unit, TSE2Type]));
    if aType = nil then
    begin
       RaiseError(petError, 'Unkown identifier: ' + Tokenizer.Token.Value);
       exit;
    end;

    if aType is TSE2Unit then
    begin
      if sCheckName <> '' then
         sCheckName := sCheckName + '.';
      sCheckName := sCheckName + Tokenizer.Token.Value;

      sUnitName := aType.Name;
      ReadNextToken;
      ExpectToken([sesDot]);
      ReadNextToken;
      ExpectToken([sesIdentifier]);
    end else
    if aType is TSE2Type then
    begin
      if sCheckName <> '' then
         if not StringIdentical(sUnitName, sCheckName) then
            RaiseError(petError, 'Unknown identifier: ' + sCheckName + '.' + Tokenizer.Token.Value);

      result := TSE2Type(aType);
      break;
    end else
    begin
      RaiseError(petError, 'Identifier is not a declared type: ' + Tokenizer.Token.Value);
      exit;
    end;
  end;
end;

function TSE2Parser.ConstExpression(Method: TSE2Method): TSE2Constant;
var aType     : TSE2BaseType;
    sUnitName : string;
    sCheckName: string;
    aParent   : TSE2Type;
    FirstAttempt : boolean;
begin
  result  := nil;
  aParent := State.CurrentOwner;
  ExpectToken([sesIdentifier]);
  FirstAttempt := True;

  sUnitName  := '';
  sCheckName := '';
  while true do
  begin
    aType := FindIdentifier(Method, Tokenizer.Token.Value, aParent, sUnitName, TSE2BaseTypeFilter.Create([TSE2Unit, TSE2Type, TSE2Constant]));
    if (aType = nil) and (sCheckName = '') and (aParent = State.CurrentOwner) and (aParent <> nil) and FirstAttempt then
    begin
      aParent := nil;
      aType   := FindIdentifier(Method, Tokenizer.Token.Value, aParent, sUnitName, TSE2BaseTypeFilter.Create([TSE2Unit, TSE2Type, TSE2Constant]));
    end;

    FirstAttempt := False;
    if aType = nil then
    begin
       RaiseError(petError, 'Unkown identifier: ' + Tokenizer.Token.Value);
       exit;
    end;

    if aType is TSE2Unit then
    begin
      if sCheckName <> '' then
         sCheckName := sCheckName + '.';
      sCheckName := sCheckName + Tokenizer.Token.Value;

      sUnitName := aType.Name;
      ReadNextToken;
      ExpectToken([sesDot]);
      ReadNextToken;
      ExpectToken([sesIdentifier]);
    end else
    if aType is TSE2Type then
    begin
      if sCheckName <> '' then
         if not StringIdentical(sUnitName, sCheckName) then
            RaiseError(petError, 'Unknown identifier: ' + sCheckName + '.' + Tokenizer.Token.Value);

      ReadNextToken;
      ExpectToken([sesDot]);
      ReadNextToken;
      ExpectToken([sesIdentifier]);
      aParent := TSE2Type(aType);
    end else
    if aType is TSE2Constant then
    begin       
      if sCheckName <> '' then
         if not StringIdentical(sUnitName, sCheckName) then
            RaiseError(petError, 'Unknown identifier: ' + sCheckName + '.' + Tokenizer.Token.Value);

      result := TSE2Constant(aType);
      break;
    end else
    begin
      RaiseError(petError, 'Identifier is not a declared type: ' + Tokenizer.Token.Value);
      exit;
    end;
  end;
end;

{

}

class function TSE2Parser.HasHelper(TargetType, CurrentType: TSE2Type;
  UnitList: TSE2BaseTypeList; Operation: TSE2TokenType = sesNone): boolean;
var i, j, k  : integer;
    aUnit    : TSE2Unit;
    helper   : TSE2Class;
    pType    : TSE2Type;
    m        : TSE2Method;
begin
  result := False;
  pType  := CurrentType;
  while pType <> nil do
  begin
    for i:=0 to UnitList.Count-1 do
    begin
      aUnit := TSE2Unit(UnitList[i]);
      for j:=0 to aUnit.TypeList.Count-1 do
        if aUnit.TypeList[j] is TSE2Class then
        begin
          helper := TSE2Class(aUnit.TypeList[j]);
          if helper.IsHelper then
          begin
            for k:=0 to aUnit.ElemList.Count-1 do
            begin
              if aUnit.ElemList[k].Parent = helper then
                if aUnit.ElemList[k] is TSE2Method then
                begin
                  m := TSE2Method(aUnit.ElemList[k]);
                  if m.ReturnValue <> nil then
                     if TSE2Parser.IsCompatible(TargetType, m.ReturnValue.AType, Operation) then
                     begin
                       result := True;
                       exit;
                     end;
                end;
            end;
          end;
        end;
    end;
    pType := TSE2Type(pType.InheritFrom);
  end;
end;

function TSE2Parser.GetExternalObjectType: TSE2Type;
begin
  result := TSE2Class(FindIdentifier(nil, C_SE2TExternalObjectName, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Class])));
end;

function TSE2Parser.GetScriptObjectType: TSE2Type;
begin
  result := TSE2Class(FindIdentifier(nil, C_SE2TObjectName, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Class])));
  if result = nil then
     RaiseError(petError, 'FATAL ERROR: internal '+C_SE2TObjectName+' not found!');
end;

procedure TSE2Parser.OnExceptStatement(State: TSE2ParseState;
  Method: TSE2Method);
var aType    : TSE2Type;
    varName  : string;
    Variable : TSE2Variable;
    nextJump : TSE2IntegerList;
    finalJump: TSE2IntegerList;
    i        : integer;
begin
  nextJump  := TSE2IntegerList.Create;
  finalJump := TSE2IntegerList.Create;
  try
    repeat
      ExpectToken([sesOn]);
      ReadNextToken;
      ExpectToken([sesIdentifier]);

      varName := Tokenizer.Token.Value;
      if Method.Variables.FindItem(varName) <> nil then
         RaiseError(petError, varName + ' is already declared');

      ReadNextToken;
      ExpectToken([sesDoublePoint]);
      ReadNextToken;
      ExpectToken([sesIdentifier]);

      aType := TypeExpression(Method);

      if not (aType is TSE2Class) then
         RaiseError(petError, 'Class identifier expected');
      if TSE2Class(aType).IsHelper then
         RaiseError(petError, 'Helper classes can not be used for here');
      if TSE2Type(aType.InheritRoot) = GetExternalObjectType then
           RaiseError(petError, 'External classes not not supported here');

      Variable := TSE2Variable.Create;
      try
        Method.Variables.Add(Variable);
        DeclareType(State, Variable, varName);
        Variable.CodePos := State.StackSize + Method.StackSize; // Method.StackSize + State.LoopStackSize;
        Variable.AType := aType;

        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_PEX, ''));
        State.IncStack;

        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(0, False), ''));
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.META_SHARE(0), 'META_'+TSE2Class(aType).GenLinkerName));

        nextJump.Add(Method.OpCodes.Count);
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_JIZ(0), ''));

        ReadNextToken;
        ExpectToken([sesDo]);
        ReadNextToken;

        State.IsInExceptBlock := False;
        Statement(State, Method);
        State.IsInExceptBlock := True;


        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
        finalJump.Add(Method.OpCodes.Count);
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
        State.DecStack;

        PSE2OpFLOW_JIZ(Method.OpCodes.Items[nextJump[0]].OpCode).Position := Method.OpCodes.Count;
        GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
        nextJump.Clear;
      finally
        Method.Variables.Delete(Variable);
      end;

      ExpectToken([sesSemiColon]);
      ReadNextToken;
    until Tokenizer.Token.AType <> sesOn;

    for i:=0 to finalJump.Count-1 do 
      PSE2OpFLOW_GOTO(Method.OpCodes.Items[finalJump[i]].OpCode).Position := Method.OpCodes.Count;

  finally
    nextJump.Free;
    finalJump.Free;
  end;
end;

function TSE2Parser.IsScriptExceptionType(aType: TSE2Type): boolean;
var exType: TSE2Type;
begin
  result := False;
  if not (aType is TSE2Class) then
     exit;

  if TSE2Class(aType).IsHelper then
     exit;

  if TSE2Type(aType.InheritRoot) <> GetScriptObjectType then
     exit;

  exType := GetScriptExceptionType;
  while aType <> nil do
  begin
    if aType = exType then
    begin
      result := True;
      exit;
    end;
    aType := TSE2Type(aType.InheritFrom);
    if aType <> nil then
      if aType.InheritFrom = aType then
         exit;
  end;
end;

function TSE2Parser.GetScriptExceptionType: TSE2Type;
begin
  result := TSE2Class(FindIdentifier(nil, C_SE2ExceptionObject, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Class])));
end;

procedure TSE2Parser.ShowUnusedVariables(OnlyPrivate: boolean);
var i: integer;
begin
  for i:=0 to FUnit.ElemList.Count-1 do
    if FUnit.ElemList[i] is TSE2Variable then
      if ((FUnit.ElemList[i].Visibility = visPrivate) and OnlyPrivate) or
          not OnlyPrivate then
      begin
        if TSE2Variable(FUnit.ElemList[i]).IsStatic then
        begin
          if not TSE2Variable(FUnit.ElemList[i]).Used then
             RaiseError(petHint, 'Variable "'+TSE2Variable(FUnit.ElemList[i]).Name+'" is declared but never used', TSE2Variable(FUnit.ElemList[i]).DeclPos,
                        TSE2Variable(FUnit.ElemList[i]).DeclLine);
        end else
        begin
          if not TSE2Variable(FUnit.ElemList[i]).Used then
            if not (TSE2Variable(FUnit.ElemList[i]).Parent is TSE2Record) then
            begin
              if TSE2Variable(FUnit.ElemList[i]).Parent is TSE2Class then
                if not TSE2Class(TSE2Variable(FUnit.ElemList[i]).Parent).IsPartial then
                    RaiseError(petHint, 'Variable "'+TSE2Variable(FUnit.ElemList[i]).Name+'" is declared but never used', TSE2Variable(FUnit.ElemList[i]).DeclPos,
                               TSE2Variable(FUnit.ElemList[i]).DeclLine);
            end;
        end;
      end;
end;

procedure TSE2Parser.GenerateComparison(Method: TSE2Method; t1,
  t2: TSE2Type; Operation: TSE2TokenType);
begin
  if (t1 is TSE2Record) and (t2 is TSE2Record) then
  begin
    if TSE2Record(t1).RTTI.Count > 0 then
       RaiseError(petWarning, 'The comparison will always return false, because records with strings are not supported for comparisons');

    case Operation of
    sesEqual        : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_EQUAL(0), 'META_' + t1.GenLinkerName)); 
    sesUnEqual      : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.REC_UNEQUAL(0), 'META_' + t1.GenLinkerName));
    end;
  end else
  begin
    case Operation of
    sesEqual        : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(1), ''));
    sesSmaller      : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(2), ''));
    sesBigger       : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(3), ''));
    sesBiggerEqual  : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(4), ''));
    sesSmallerEqual : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(5), ''));
    sesUnEqual      : GenCode(Method, TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(6), ''));
    end;
  end;
end;

procedure TSE2Parser.CheckForDeprecated(aType: TSE2BaseType);
begin
  if aType <> nil then
    if aType.IsDeprecated then
    begin
      if aType.DeprecatedValue <> '' then
         RaiseError(petWarning, aType.GetStrongName + ' is deprecated. ' + aType.DeprecatedValue)
      else
         RaiseError(petWarning, aType.GetStrongName + ' is deprecated.');
    end;
end;

function TSE2Parser.GetBaseArrayType: TSE2Type;
begin
  result := TSE2Type(FindIdentifier(nil, C_SE2BaseSystemArray, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Type])));
  if result = nil then
     RaiseError(petError, 'Internal error: array type not found');
end;

procedure TSE2Parser.LinkArrayMethodEvents;
var ArrayType: TSE2Type;
    Method   : TSE2Method;
    aUnit    : TSE2Unit;
    Filter   : TSE2BaseTypeFilter;
begin
  ArrayType := TSE2Type(FindIdentifier(nil, C_SE2BaseSystemArray, nil, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Type])));
  if ArrayType = nil then
     exit;

  aUnit     := TSE2Unit(FUnitList.FindItem(C_SE2SystemUnitName));
  if aUnit = nil then
     exit;

  Filter := TSE2BaseTypeFilter.Create([TSE2Method]);
  try
    Method    := TSE2Method(aUnit.ElemList.FindItem('GetMinIndex', C_SE2SystemUnitName, ArrayType, nil, Filter, CAllVisibilities));
    if Method <> nil then
       Method.OnCallMethod := ArrayMethodCall;

    Method    := TSE2Method(aUnit.ElemList.FindItem('GetMaxIndex', C_SE2SystemUnitName, ArrayType, nil, Filter, CAllVisibilities));
    if Method <> nil then
       Method.OnCallMethod := ArrayMethodCall;

    Method    := TSE2Method(aUnit.ElemList.FindItem('GetLength', C_SE2SystemUnitName, ArrayType, nil, Filter, CAllVisibilities));
    if Method <> nil then
       Method.OnCallMethod := ArrayMethodCall;

    Method    := TSE2Method(aUnit.ElemList.FindItem('SetLength', C_SE2SystemUnitName, ArrayType, nil, Filter, CAllVisibilities));
    if Method <> nil then
       Method.OnCallMethod := ArrayMethodCall;
  finally
    Filter.Free;
  end;
end;

function TSE2Parser.TypeIsNoRefType(aType: TSE2Type): boolean;
begin
  result := False;
  if TSE2Type(aType.InheritRoot).AType in [btArray, btRecord] then
  begin
    if TSE2Type(aType.InheritRoot).AType = btRecord then
       result := True
    else
       result := not TSE2Array(aType).IsDynamic;
  end;
end;

{ TSE2TryBlock }

constructor TSE2TryBlock.Create(AMethod: TSE2Method;
  Parser: TSE2Parser; State: TSE2ParseState);
begin
  inherited Create;
  FParser := Parser;
  FMethod := AMethod;
  FState  := State;
end;

procedure TSE2TryBlock.Step1;
begin

  FgeneralRETPos  := 0;
  FexitRETPos     := 0;
  FcontinueRETPos := 0;
  FbreakRETPos    := 0;
  
  FoldExitList     := FMethod.Lists.ExitList.Count;
  FoldContinueList := FMethod.Lists.ContinueList.Count;
  FoldBreakList    := FMethod.Lists.BreakList.Count;

  FOpCodePos2      := FMethod.OpCodes.Count;
  FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_TRYFIN(0, 0), ''));
end;

procedure TSE2TryBlock.Step2_TryContent;
var wasInTryBlock: boolean;
begin
  wasInTryBlock := FState.IsInTryBlock;
  FState.IsInTryBlock := True;
  try
    FParser.StatementSquence(FState, FMethod);
  finally
    FState.IsInTryBlock := wasInTryBlock;
  end;
end;

procedure TSE2TryBlock.Step3(IsFinally: boolean);
var i, j: integer;
begin
  FIsExceptBlock := not IsFinally;

  if IsFinally then
  begin
    FgeneralRETPos := FMethod.OpCodes.Count;
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(0, 0), ''));
    FState.IncStack;
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
  end;

  PSE2OpSAFE_TRYFIN(FMethod.OpCodes[FOpCodePos2].OpCode).SavePos := FMethod.OpCodes.Count;
  FOpCodePos1 := FMethod.OpCodes.Count;

  if IsFinally then
  begin
    if FMethod.Lists.ExitList.Count > FoldExitList then
    begin
      for i:=FMethod.Lists.ExitList.Count-1 downto FoldExitList do
      begin
        for j:=2 to 1 + FState.LoopStackSize - FMethod.Lists.ExitTableList.Last do
        begin
          Assert(FMethod.OpCodes[FMethod.Lists.ExitList[i]-j].OpCode.OpCode = soSTACK_DEC);
          PSE2OpSTACK_DEC(FMethod.OpCodes[FMethod.Lists.ExitList[i]-j].OpCode).OpCode := soNOOP;
        end;
        PSE2OpFLOW_GOTO(FMethod.OpCodes[FMethod.Lists.ExitList[i]].OpCode).Position := FMethod.OpCodes.Count;
      end;
      FexitRETPos := FMethod.OpCodes.Count;
      FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(0, 0), ''));
      FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
    end;

    if FMethod.Lists.ContinueList.Count > FoldContinueList then
    begin
      for i:=FMethod.Lists.ContinueList.Count-1 downto FoldContinueList do
        PSE2OpFLOW_GOTO(FMethod.OpCodes[FMethod.Lists.ContinueList[i]].OpCode).Position := FMethod.OpCodes.Count;
      FcontinueRETPos := FMethod.OpCodes.Count;
      FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(0, 0), ''));
      FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
    end;

    if FMethod.Lists.BreakList.Count > FoldBreakList then
    begin
      for i:=FMethod.Lists.BreakList.Count-1 downto FoldBreakList do
        PSE2OpFLOW_GOTO(FMethod.OpCodes[FMethod.Lists.BreakList[i]].OpCode).Position := FMethod.OpCodes.Count;
      FbreakRETPos := FMethod.OpCodes.Count;
      FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(0, 0), ''));
      FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
    end;

    PSE2OpFLOW_GOTO(FMethod.OpCodes[FgeneralRETPos  + 1].OpCode).Position := FMethod.OpCodes.Count;

    if FMethod.Lists.ExitList.Count > FoldExitList then
       PSE2OpFLOW_GOTO(FMethod.OpCodes[FexitRETPos     + 1].OpCode).Position := FMethod.OpCodes.Count;
    if FMethod.Lists.ContinueList.Count > FoldContinueList then
       PSE2OpFLOW_GOTO(FMethod.OpCodes[FcontinueRETPos + 1].OpCode).Position := FMethod.OpCodes.Count;
    if FMethod.Lists.BreakList.Count > FoldBreakList then
       PSE2OpFLOW_GOTO(FMethod.OpCodes[FbreakRETPos    + 1].OpCode).Position := FMethod.OpCodes.Count;

    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_BLOCK(0, False), ''));
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(FMethod.OpCodes.Count + 3), ''));

    FOpCodePos1 := FMethod.OpCodes.Count;
    PSE2OpSAFE_TRYFIN(FMethod.OpCodes[FOpCodePos2].OpCode).SavePos := FMethod.OpCodes.Count;
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_BLOCK(0, True), ''));
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(0, 0), ''));

  end else
  begin
    if FMethod.Lists.ExitList.Count > FoldExitList then
    begin
      for i:=FMethod.Lists.ExitList.Count-1 downto FoldExitList do
      begin
        FMethod.OpCodes[FMethod.Lists.ExitList[i] - 1].OpCode.OpCode := soSAFE_TRYEND;
      end;
    end;
    if FMethod.Lists.ContinueList.Count > FoldContinueList then
    begin
      for i:=FMethod.Lists.ContinueList.Count-1 downto FoldContinueList do
      begin
        FMethod.OpCodes[FMethod.Lists.ContinueList[i] - 1].OpCode.OpCode := soSAFE_TRYEND;
      end;
    end;    
    if FMethod.Lists.BreakList.Count > FoldBreakList then
    begin
      for i:=FMethod.Lists.BreakList.Count-1 downto FoldBreakList do
      begin
        FMethod.OpCodes[FMethod.Lists.BreakList[i] - 1].OpCode.OpCode := soSAFE_TRYEND;
      end;
    end;

    PSE2OpSAFE_TRYEX(FMethod.OpCodes[FOpCodePos2].OpCode).OpCode := soSAFE_TRYEX;
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_BLOCK(0, True), ''));
  end;

  FState.IsInExceptBlock := not IsFinally;

  FexitEntry     := FMethod.Lists.ExitList.Count > FoldExitList;
  FcontinueEntry := FMethod.Lists.ContinueList.Count > FoldContinueList;
  FbreakEntry    := FMethod.Lists.BreakList.Count > FoldBreakList;

  if IsFinally then
  begin
    for i:=FMethod.Lists.ExitList.Count-1 downto FoldExitList do
      FMethod.Lists.ExitList.Delete(FMethod.Lists.ExitList.Count-1);
    for i:=FMethod.Lists.ContinueList.Count-1 downto FoldContinueList do
      FMethod.Lists.ContinueList.Delete(FMethod.Lists.ContinueList.Count-1);
    for i:=FMethod.Lists.BreakList.Count-1 downto FoldBreakList do
      FMethod.Lists.BreakList.Delete(FMethod.Lists.BreakList.Count-1);
  end;

  FwasInTryFinally := FState.IsInFinallyBlock;
  FwasInTryExcept  := FState.IsInExceptBlock;
  if IsFinally then
     FState.IsInFinallyBlock := True;
end;

procedure TSE2TryBlock.Step4_SafeContent;
begin
  FParser.StatementSquence(FState, FMethod);
end;

procedure TSE2TryBlock.Step5;

  procedure CallExceptFree;
  var aMethod: TSE2Method;
  begin
    aMethod := TSE2Method(FParser.FindIdentifier(nil, 'Free', FParser.GetScriptObjectType, C_SE2SystemUnitName, TSE2BaseTypeFilter.Create([TSE2Method])));

    {$IFDEF SEII_SMART_LINKING}
    FMethod.UsedMethods.Add(aMethod);
    {$ELSE}
    aMethod.Used := True;
    {$ENDIF}

    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_PEX, ''));
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(FMethod.OpCodes.Count + 2, 0), ''));
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALL(0), aMethod.GenLinkerName));
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
  end;

var i: integer;
begin
  FState.IsInFinallyBlock := FwasInTryFinally;

  if not FIsExceptBlock then
  begin
    FState.DecStack;
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));

    if FexitEntry then
    begin
      PSE2OpFLOW_PUSHRET(FMethod.OpCodes[FexitRETPos].OpCode).Position  := FMethod.OpCodes.Count;
                {
      if FState.IsInTryBlock then
      begin
        for i:=FState.StackSize-1 downto FState.LoopStackSize do
          FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      end else  { }
      begin
        for i:=FState.StackSize-1 downto FMethod.Lists.ExitTableList.Last do
          FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
      end;       { }
      
      if FState.IsInTryBlock then
         FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.NOOP, ''));

      FMethod.Lists.ExitList.Add(FMethod.OpCodes.Count);
      FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
    end;

    if FcontinueEntry then
    begin
      PSE2OpFLOW_PUSHRET(FMethod.OpCodes[FcontinueRETPos].OpCode).Position  := FMethod.OpCodes.Count;

      if FState.IsInTryBlock then
         FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.NOOP, ''));

      FMethod.Lists.ContinueList.Add(FMethod.OpCodes.Count);
      FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
    end;
    
    if FbreakEntry then
    begin
      PSE2OpFLOW_PUSHRET(FMethod.OpCodes[FbreakRETPos].OpCode).Position  := FMethod.OpCodes.Count;

      if FState.IsInTryBlock then
         FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.NOOP, ''));

      FMethod.Lists.BreakList.Add(FMethod.OpCodes.Count);
      FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(0), ''));
    end;
                                                                                
    PSE2OpFLOW_PUSHRET(FMethod.OpCodes[FOpCodePos1 + 1].OpCode).Position := FMethod.OpCodes.Count;
    PSE2OpFLOW_PUSHRET(FMethod.OpCodes[FgeneralRETPos].OpCode).Position  := FMethod.OpCodes.Count;

    
    PSE2OpSAFE_TRYFIN(FMethod.OpCodes[FOpCodePos2].OpCode).LeavePos := FMethod.OpCodes.Count;
    PSE2OpSAFE_BLOCK(FMethod.OpCodes[FOpCodePos1].OpCode).SkipPoint := FMethod.OpCodes.Count;
    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_TRYEND, ''));
  end else
  begin
    CallExceptFree;

    PSE2OpSAFE_TRYFIN(FMethod.OpCodes[FOpCodePos2].OpCode).LeavePos := FMethod.OpCodes.Count;
    PSE2OpSAFE_BLOCK(FMethod.OpCodes[FOpCodePos1].OpCode).SkipPoint := FMethod.OpCodes.Count;

    FParser.GenCode(FMethod, TSE2LinkOpCode.Create(TSE2OpCodeGen.SAFE_TRYEND, ''));
  end;

  FState.IsInExceptBlock := FwasInTryExcept;
end;

end.

