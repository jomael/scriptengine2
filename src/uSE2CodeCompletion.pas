unit uSE2CodeCompletion;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2Consts, uSE2Parser, uSE2Compiler, uSE2BaseTypes, uSE2Types,
  uSE2Reader, uSE2Tokenizer, uSE2UnitCacheMngr, uSE2OpCode, uSE2UnitManager;

type
  TSE2AddItemEvent   = procedure(Sender: TObject; const DisplayText, InsertText: string) of object;
  TSE2GetUnitMngr    = procedure(Sender: TObject; var Mngr: TSE2UnitCacheMngr) of object;
  TSE2GetCustomUnits = procedure(Sender: TObject; const Target: TStrings) of object;

  TSE2CodeCompletion = class(TSE2Object)
  private
    FCompiler         : TSE2Compiler;
    FOnAddItem        : TSE2AddItemEvent;
    FOnGetUnitMngr    : TSE2GetUnitMngr;
    FOnGetCustomUnits : TSE2GetCustomUnits;
    FEventRaised      : boolean;
    FCanExecute       : boolean;
  protected
    procedure CompileCallBack(Sender: TObject; CodePos, ParamIndex: integer; CurrentMethod, ParamMethod: TSE2Method;
                                  Parent: TSE2BaseType; TargetType: TSE2Type; TokenType: TSE2TokenType; StaticOnly: boolean);

    procedure DoGetCustomUnits(const Target: TStrings);
    procedure DoAddItem(const DispText, InsText: string); virtual;
    procedure DoProcessItem(Item: TSE2BaseType);
    function  ItemIsCompatible(Item: TSE2BaseType; ResType: TSE2Type; Opr: TSE2TokenType = sesNone): boolean;

    function  GetDisplayStrMethod(Entry: TSE2Method): string; virtual; abstract;
    function  GetDisplayStrProperty(Entry: TSE2Property): string; virtual; abstract;
    function  GetDisplayStrType(Entry: TSE2Type): string; virtual; abstract;
    function  GetDisplayStrConst(Entry: TSE2Constant): string; virtual; abstract;
    function  GetDisplayStrVar(Entry: TSE2Variable): string; virtual; abstract;
    function  GetDisplayStrUnit(const Name: string): string; virtual; abstract;
    function  GetDisplayStrKeyWord(const KeyWord: string): string; virtual; abstract;

    function  GetInsertStrMethod(Entry: TSE2Method): string; virtual; abstract;
    function  GetInsertStrProperty(Entry: TSE2Property): string; virtual; abstract;
    function  GetInsertStrType(Entry: TSE2Type): string; virtual; abstract;
    function  GetInsertStrConst(Entry: TSE2Constant): string; virtual; abstract;
    function  GetInsertStrVar(Entry: TSE2Variable): string; virtual; abstract;
    function  GetInsertStrUnit(const Name: string): string; virtual; abstract;

    function  GetDisplayStr(Entry: TSE2BaseType): string; virtual;
    function  GetInsertStr(Entry: TSE2BaseType): string; virtual;

    procedure ProcessUnit(AUnit: TSE2Unit; ParentObj: TSE2BaseType; Visibility: TSE2Visibilities; ResultType: TSE2Type; Method: TSE2Method; StaticOnly: boolean);
    procedure ProcessUnits(Compiler: TSE2Compiler; Parser: TSE2Parser; ParentObj: TSE2BaseType; Visibility: TSE2Visibilities; ResultType: TSE2Type; Method: TSE2Method; StaticOnly: boolean);
  public
    constructor Create(CallBack: TSE2AddItemEvent); reintroduce;
    destructor Destroy; override;

    function GetCodeCompletion(const Source: string; CursorPos: integer): boolean;

    property Compiler         : TSE2Compiler       read FCompiler;
    property OnAddItem        : TSE2AddItemEvent   read FOnAddItem        write FOnAddItem;
    property OnGetUnitMngr    : TSE2GetUnitMngr    read FOnGetUnitMngr    write FOnGetUnitMngr;
    property OnGetCustomUnits : TSE2GetCustomUnits read FOnGetCustomUnits write FOnGetCustomUnits;
  end;

  TSE2SynCodeCompletion = class(TSE2CodeCompletion)
  protected
    function  GetDisplayStrMethod(Entry: TSE2Method): string; override;
    function  GetDisplayStrProperty(Entry: TSE2Property): string; override;
    function  GetDisplayStrType(Entry: TSE2Type): string; override;
    function  GetDisplayStrConst(Entry: TSE2Constant): string; override;
    function  GetDisplayStrVar(Entry: TSE2Variable): string; override;  
    function  GetDisplayStrUnit(const Name: string): string; override;
    function  GetDisplayStrKeyWord(const KeyWord: String): string; override;

    function  GetInsertStrMethod(Entry: TSE2Method): string; override;   
    function  GetInsertStrProperty(Entry: TSE2Property): string; override;
    function  GetInsertStrType(Entry: TSE2Type): string; override;
    function  GetInsertStrConst(Entry: TSE2Constant): string; override;
    function  GetInsertStrVar(Entry: TSE2Variable): string; override;
    function  GetInsertStrUnit(const Name: string): string; override;
  end;

  TSE2ParamCompletion = class(TSE2Object)
  private
    FCompiler      : TSE2Compiler;
    FOnAddItem     : TSE2AddItemEvent;
    FOnGetUnitMngr : TSE2GetUnitMngr;
    FParamIndex    : integer;
    FEventRaised   : boolean;
    FCanExecute    : boolean;
  protected
    procedure CompileCallBack(Sender: TObject; CodePos, ParamIndex: integer; CurrentMethod, ParamMethod: TSE2Method;
                                  Parent: TSE2BaseType; TargetType: TSE2Type; TokenType: TSE2TokenType; StaticOnly: boolean);

    procedure DoAddItem(const DispText: string); virtual;
    function  FormatParamStr(Entry: TSE2Parameter; IsLastParam: boolean): string; virtual; abstract;
    
    procedure ProcessParam(Method: TSE2Method);
  public
    constructor Create(CallBack: TSE2AddItemEvent); reintroduce;
    destructor Destroy; override;

    function GetParamCompletion(const Source: string; CursorPos: integer; var ParamIndex: integer): boolean;

    property Compiler      : TSE2Compiler      read FCompiler;
    property OnAddItem     : TSE2AddItemEvent  read FOnAddItem      write FOnAddItem;
    property OnGetUnitMngr : TSE2GetUnitMngr   read FOnGetUnitMngr  write FOnGetUnitMngr;
  end;

  TSE2SynParamCompletion = class(TSE2ParamCompletion)
  protected
    function  FormatParamStr(Entry: TSE2Parameter; IsLastParam: boolean): string; override;
  end;

implementation

{ TSE2CodeCompletion }

constructor TSE2CodeCompletion.Create(CallBack: TSE2AddItemEvent);
begin
  inherited Create;
  FOnAddItem := CallBack;
  FCompiler  := TSE2Compiler.Create;
end;

destructor TSE2CodeCompletion.Destroy;
begin
  FreeAndNil(FCompiler);
  inherited;
end;

procedure TSE2CodeCompletion.DoAddItem(const DispText, InsText: string);
begin
  if Assigned(FOnAddItem) then
     FOnAddItem(Self, DispText, InsText);
end;     

function TSE2CodeCompletion.ItemIsCompatible(Item: TSE2BaseType; ResType: TSE2Type; Opr: TSE2TokenType = sesNone): boolean;
begin
  result := True;

  if Item = nil then
     exit;

  if ResType = nil then
     exit
  else
  begin
    if (Item is TSE2Type) then
      if (not (Item is TSE2Class)) and (not (Item is TSE2Record)) then
      begin
        result := False;
        exit;
      end;
  end;

  if Item is TSE2Class then
  begin
    result := True;
  end else
  if Item is TSE2Record then
  begin
    result := True;
  end else
  if Item is TSE2Type then
  begin
    result := ResType = nil;
    //result := TSE2Parser.IsCompatible(ResType, TSE2Type(Item), Opr);
  end else
  if Item is TSE2Property then
  begin
    result := TSE2Parser.IsCompatible(ResType, TSE2Property(Item).AType, Opr);
    if not result then
      if TSE2Type(TSE2Property(Item).AType.InheritRoot).AType = btObject then
         result := True;
  end else
  if Item is TSE2Variable then
  begin
    result := TSE2Parser.IsCompatible(ResType, TSE2Variable(Item).AType, Opr);
    if not result then
       if TSE2Type(TSE2Variable(Item).AType.InheritRoot).AType = btObject then
         result := True;
  end else
  if Item is TSE2Constant then
  begin
    result := TSE2Parser.IsCompatible(ResType, TSE2Constant(Item).AType, Opr);
  end else
  if Item is TSE2Method then
  begin
    result := False;
    if TSE2Method(Item).ReturnValue <> nil then
       result := TSE2Parser.IsCompatible(ResType, TSE2Method(Item).ReturnValue.AType, Opr);
  end;
end;

function MethodIsParentOfClass(Method: TSE2Method; aClass: TSE2Class): boolean;
begin
  result := False;
  if Method.Parent = aClass then
    result := True
  else
  if aClass.InheritFrom is TSE2Class then
     result := MethodIsParentOfClass(Method, TSE2Class(aClass.InheritFrom));
end;

function MethodIsParentOfRecord(Method: TSE2Method; aRecord: TSE2Record): boolean;
begin
  result := False;
  if Method.Parent = aRecord then
     result := True
  else
  if aRecord.InheritFrom is TSE2Record then
     result := MethodIsParentOfRecord(Method, TSE2Record(aRecord.InheritFrom));
end;

function  ObjIsClassHelperElement(Obj: TSE2BaseType; ParentObj: TSE2BaseType): boolean;
var child : TSE2Type;
begin
  result := False;
  child  := TSE2Type(obj.Parent);
  if child = nil then
     exit;

  if not (child is TSE2Class) then
     exit;

  if not TSE2Class(child).IsHelper then
     exit;

  while (ParentObj <> nil) and (not result) do
  begin
    result := TSE2Class(child).InheritFrom = ParentObj;
    if not result then
       ParentObj := ParentObj.InheritFrom;
  end;
end;

procedure TSE2CodeCompletion.ProcessUnit(AUnit: TSE2Unit; ParentObj: TSE2BaseType; Visibility: TSE2Visibilities; ResultType: TSE2Type; Method: TSE2Method; StaticOnly: boolean);
var i   : integer;
    obj : TSE2BaseType;
    aVis: TSE2Visibilities;
begin
  if (Method <> nil) and (ParentObj = nil) then
  begin
    for i:=0 to Method.Params.Count-1 do
    begin
      obj := Method.Params[i];
      if obj <> nil then
        if ItemIsCompatible(obj, ResultType) then
          DoProcessItem(obj);
    end;

    for i:=0 to Method.Variables.Count-1 do
    begin
      obj := Method.Variables[i];
      if obj <> nil then
        if ItemIsCompatible(obj, ResultType) then
          DoProcessItem(obj);
    end;

    for i:=0 to Method.Types.Count-1 do
    begin
      obj := Method.Types[i];
      if obj <> nil then        
        if ItemIsCompatible(obj, ResultType) then
          DoProcessItem(obj);
    end;
  end;

  if ParentObj = nil then
    for i:=0 to AUnit.TypeList.Count-1 do
    begin
      obj := AUnit.TypeList[i];
      if obj <> nil then
        if obj.IsChildOf(ParentObj) or obj.IsTypeOf(ParentObj) then
          if obj.Visibility in Visibility then  
             //if ItemIsCompatible(obj, ResultType) then
               DoProcessItem(obj);
    end;


  repeat
    for i:=0 to AUnit.ElemList.Count-1 do
    begin
      aVis := CPublicVisibilities;
      obj  := AUnit.ElemList[i];
      if obj <> nil then
      begin
        if ParentObj is TSE2Class then
        begin
          if Method <> nil then
          begin        
            if SameText(ParentObj.AUnitName, Method.AUnitName) then
               aVis := CAllVisibilities
            else
            if MethodIsParentOfClass(Method, TSE2Class(ParentObj)) then
               aVis := [visProtected, visPublic]
            else
               aVis := CPublicVisibilities;
          end else
          begin
            if SameText(ParentObj.AUnitName, obj.AUnitName) then
               aVis := CAllVisibilities;
          end;
        end else
        if ParentObj is TSE2Record then
        begin
          if Method <> nil then
          begin
            if SameText(ParentObj.AUnitName, Method.AUnitName) then
               aVis := CAllVisibilities
            else
            if MethodIsParentOfRecord(Method, TSE2Record(ParentObj)) then
               aVis := [visProtected, visPublic]
            else
               aVis := CPublicVisibilities;
          end else
          begin
            if SameText(ParentObj.AUnitName, obj.AUnitName) then
               aVis := CAllVisibilities;
          end;
        end;

        if not (obj.Visibility in aVis) then
           continue;


        if obj.IsChildOf(ParentObj) or (ObjIsClassHelperElement(obj, ParentObj)) then
        begin
          if StaticOnly then
          begin
            if obj is TSE2Method then
            begin
              if not TSE2Method(obj).IsStatic then
                 continue;
            end else
            if obj is TSE2Property then
            begin
              if not TSE2Property(obj).IsStatic then
                 continue;
            end else
            if obj is TSE2Variable then
            begin
              if not TSE2Variable(obj).IsStatic then
                 continue;
            end;
          end;
          //if (obj.Visibility in Visibility) or ((ParentObj is TSE2Class) and (obj.Visibility in [visProtected, visPublic])) then
            if ItemIsCompatible(obj, ResultType) then
              DoProcessItem(obj);
        end;
      end;
    end;

    if ParentObj <> nil then
       ParentObj := ParentObj.InheritFrom;
  until ParentObj = nil;
end;

procedure TSE2CodeCompletion.DoProcessItem(Item: TSE2BaseType);
begin
  if Pos('!', Item.Name) = 0 then
     DoAddItem(GetDisplayStr(Item), GetInsertStr(Item));
end;

function TSE2CodeCompletion.GetDisplayStr(Entry: TSE2BaseType): string;
begin
  if Entry is TSE2Method then
     result := GetDisplayStrMethod(TSE2Method(Entry))
  else
  if Entry is TSE2Type then
     result := GetDisplayStrType(TSE2Type(Entry))
  else
  if Entry is TSE2Constant then
     result := GetDisplayStrConst(TSE2Constant(Entry))
  else
  if Entry is TSE2Variable then
     result := GetDisplayStrVar(TSE2Variable(Entry))
  else
  if Entry is TSE2Property then
     result := GetDisplayStrProperty(TSE2Property(Entry))
  else
  if Entry is TSE2Unit then
     result := GetDisplayStrUnit(TSE2Unit(Entry).Name)
  else
     result := Entry.GenLinkerName();
end;     

function TSE2CodeCompletion.GetInsertStr(Entry: TSE2BaseType): string;
begin
  if Entry is TSE2Method then
     result := GetInsertStrMethod(TSE2Method(Entry))
  else
  if Entry is TSE2Type then
     result := GetInsertStrType(TSE2Type(Entry))
  else
  if Entry is TSE2Constant then
     result := GetInsertStrConst(TSE2Constant(Entry))
  else
  if Entry is TSE2Variable then
     result := GetInsertStrVar(TSE2Variable(Entry))
  else
  if Entry is TSE2Property then
     result := GetInsertStrProperty(TSE2Property(Entry))
  else                            
  if Entry is TSE2Unit then
     result := GetInsertStrUnit(TSE2Unit(Entry).Name)
  else
     result := Entry.GenLinkerName();
end;

function TSE2CodeCompletion.GetCodeCompletion(const Source: string;
  CursorPos: integer): boolean;
var UnitMngr : TSE2UnitCacheMngr;
begin
  FEventRaised := False;
  FCanExecute  := True;

  UnitMngr := nil;
  if Assigned(FOnGetUnitMngr) then
     FOnGetUnitMngr(Self, UnitMngr);

  FCompiler.UnitCache := UnitMngr;
  FCompiler.CodeComplete(Source, CompileCallBack, CursorPos);

  if not FEventRaised then
  begin
    ProcessUnits(FCompiler, nil, nil, CAllVisibilities, nil, nil, False);
  end;

  result := FCanExecute;
end;

procedure TSE2CodeCompletion.CompileCallBack(Sender: TObject;
  CodePos, ParamIndex: integer; CurrentMethod, ParamMethod: TSE2Method; Parent: TSE2BaseType;
  TargetType: TSE2Type; TokenType: TSE2TokenType; StaticOnly: boolean);
begin
  FEventRaised := True;
  FCanExecute  := not (TokenType in [sesString, sesInteger, sesFloat]);
  ProcessUnits(FCompiler, TSE2Parser(Sender), Parent, CAllVisibilities, TargetType, CurrentMethod, StaticOnly);
end;

procedure TSE2CodeCompletion.ProcessUnits(Compiler: TSE2Compiler;
  Parser: TSE2Parser; ParentObj: TSE2BaseType;
  Visibility: TSE2Visibilities; ResultType: TSE2Type; Method: TSE2Method; StaticOnly: boolean);
var i          : integer;
    IgnoreUnit : TSE2Unit;
    vis        : TSE2Visibilities;
    unitList   : TStringList;
begin
  if ParentObj <> nil then
     if ParentObj is TSE2Unit then
     begin
       vis := CPublicVisibilities;
       if Parser <> nil then
          if Parser.AUnit = ParentObj then
            vis := CAllVisibilities;

       ProcessUnit(TSE2Unit(ParentObj), nil, vis, nil, nil, StaticOnly);
       exit;
     end;

  IgnoreUnit := nil;
  if Parser <> nil then
  begin
    IgnoreUnit := Parser.AUnit;
    ProcessUnit(IgnoreUnit, ParentObj, CAllVisibilities, ResultType, Method, StaticOnly);
  end;

  for i:=0 to Compiler.UnitList.Count-1 do
    if Compiler.UnitList[i] <> IgnoreUnit then
      ProcessUnit(TSE2Unit(Compiler.UnitList[i]), ParentObj, CPublicVisibilities, ResultType, Method, StaticOnly);


  if ParentObj = nil then
  begin
    unitList := TStringList.Create;
    try
      unitList.Sorted := True;
      unitList.Duplicates := dupIgnore;
      unitList.CaseSensitive := False;

      DoGetCustomUnits(unitList);

      for i:=0 to Compiler.UnitList.Count-1 do
        if TSE2UnitManager.FindUnit(Compiler.UnitList[i].Name) = nil then
           unitList.Add(Compiler.UnitList[i].Name);

      for i:=0 to TSE2UnitManager.Instance.Count-1 do
        unitList.Add(TSE2UnitManager.Instance[i].UnitName);

      for i:=0 to unitList.Count-1 do
        DoAddItem(GetDisplayStrUnit(unitList[i]), GetInsertStrUnit(unitList[i])); 
    finally
      unitList.Free;
    end;
  end;



end;

procedure TSE2CodeCompletion.DoGetCustomUnits(const Target: TStrings);
begin
  if Assigned(FOnGetCustomUnits) then
     FOnGetCustomUnits(Self, Target);
end;

{ TSE2SynCodeCompletion }

function TSE2SynCodeCompletion.GetDisplayStrConst(
  Entry: TSE2Constant): string;
begin
  result := '\color{clGreen}const \color{clBlack} \column{}\style{+B}'+Entry.Name+'\style{-B} : '+Entry.AType.Name;
end;

function TSE2SynCodeCompletion.GetDisplayStrKeyWord(
  const KeyWord: String): string;
begin
  result := '';
end;

function TSE2SynCodeCompletion.GetDisplayStrMethod(
  Entry: TSE2Method): string;
var i, Start  : integer;
    LastParam : TSE2Parameter;
    Param     : TSE2Parameter;

  function IsNewParamType(LastParam, Param: TSE2Parameter): boolean;
  begin
    result := True;
    if (LastParam = nil) or (Param = nil) then
       exit;

    if LastParam.ParameterType <> Param.ParameterType then
       exit;

    if LastParam.AType <> Param.AType then
       exit;

    result := False;
  end;

begin
  case Entry.MethodType of
  mtProcedure   : result := '\color{clTeal}procedure \color{clBlack} \column{}';
  mtFunction    : result := '\color{clBlue}function \color{clBlack} \column{}';
  mtConstructor : result := '\color{clAqua}constructor \color{clBlack} \column{}';
  mtDestructor  : result := '\color{clBlue}destructor \color{clBlack} \column{}';
  end;
  result := result + '\style{+B}'+Entry.Name+'\style{-B}';


  Start  := 0;
  if Entry.HasSelfParam then
     Start := 1;

  if Entry.Params.Count > Start then
  begin
    result := result + '(';

    LastParam := nil;
    for i:=Start to Entry.Params.Count-1 do
    begin
      Param := TSE2Parameter(Entry.Params[i]);
      if IsNewParamType(LastParam, Param) then
      begin
        case Param.ParameterType of
        ptConst : result := result + '\style{+B}const\style{-B} ';
        ptVar   : result := result + '\style{+B}var\style{-B} ';
        end;
      end;
      result := result + Param.Name;
      if IsNewParamType(Param, TSE2Parameter(Entry.Params[i+1])) then
      begin
        result := result + ': '+Param.AType.Name;
        if i < Entry.Params.Count-1 then
           result := result + '; ';
      end else
      begin
        result := result + ', ';
      end;
      LastParam := Param;
    end;

    result := result + ')';
  end;

  if Entry.MethodType = mtFunction then
  begin
    if Entry.ReturnValue <> nil then
       result := result + ': ' + Entry.ReturnValue.AType.Name
    else
       result := result + ': ???';
  end;
  result := result + ';';
end;

function TSE2SynCodeCompletion.GetDisplayStrProperty(
  Entry: TSE2Property): string;
var i         : integer;
    LastParam : TSE2Variable;
    Param     : TSE2Variable;

  function IsNewParamType(LastParam, Param: TSE2Variable ): boolean;
  begin
    result := True;
    if (LastParam = nil) or (Param = nil) then
       exit;

    //if LastParam.ParameterType <> Param.ParameterType then
    //   exit;

    if LastParam.AType <> Param.AType then
       exit;

    result := False;
  end;

begin
  result := '\color{clTeal}property \color{clBlack}';

  result := result + ' \column{}\style{+B}'+Entry.Name+'\style{-B}';
  if Entry.Params.Count > 0 then
  begin
    result := result + '[';

    LastParam := nil;
    for i:=0 to Entry.Params.Count-1 do
    begin
      Param := TSE2Variable(Entry.Params[i]);
      if IsNewParamType(LastParam, Param) then
      begin
        (*
        case Param.ParameterType of
        ptConst : result := result + '\style{+B}const\style{-B} ';
        ptVar   : result := result + '\style{+B}var\style{-B} ';
        end;  *)
      end;
      result := result + Param.Name;
      if IsNewParamType(Param, TSE2Parameter(Entry.Params[i+1])) then
      begin
        result := result + ': '+Param.AType.Name;
        if i < Entry.Params.Count-1 then
           result := result + '; ';
      end else
      begin
        result := result + ', ';
      end;
      LastParam := Param;
    end;

    result := result + ']';

  end;
  result := result + ' : ' + Entry.AType.Name;
end;

function TSE2SynCodeCompletion.GetDisplayStrType(Entry: TSE2Type): string;
var s: string;
begin
  if Entry is TSE2Class then
    result := '\color{clGreen}class \color{clBlack} \column{}\style{+B}\color{clNavy}'+Entry.Name+'\style{-B}\color{clBlack}'
  else
    result := '\color{clOlive}type \color{clBlack} \column{}\style{+B}'+Entry.Name+'\style{-B}';

  s := ';';
  if Entry.InheritFrom <> nil then
  begin
    s := ': \color{clGreen}'+Entry.InheritFrom.Name+'\color{clBlack};';
  end;


  result := result + s;
end;

function TSE2SynCodeCompletion.GetDisplayStrUnit(const Name: string): string;
begin
  result := '\color{clGray}unit \color{clBlack}  \column{}\style{+B}'+Name+'\style{-B};';
end;

function TSE2SynCodeCompletion.GetDisplayStrVar(
  Entry: TSE2Variable): string;
begin
  result := '\color{clMaroon}var \color{clBlack}';
  if Entry is TSE2Parameter then
    if TSE2Parameter(Entry).ParameterType = ptConst then
      result := '\color{clGreen}const \color{clBlack}';

  result := result + ' \column{}\style{+B}'+Entry.Name+'\style{-B} : ';
  result := result + Entry.AType.Name;
end;

function TSE2SynCodeCompletion.GetInsertStrConst(
  Entry: TSE2Constant): string;
begin
  result := Entry.Name;
end;

function TSE2SynCodeCompletion.GetInsertStrMethod(
  Entry: TSE2Method): string;
begin
  result := '';
  if Entry.HasSelfParam then
  begin
    if Entry.Params.Count > 1 then
       //result := '(';
  end else
  begin
    if Entry.Params.Count > 0 then
       //result := '(';
  end;

  result := Entry.Name + result;
end;

function TSE2SynCodeCompletion.GetInsertStrProperty(
  Entry: TSE2Property): string;
begin
  result := Entry.Name;
end;

function TSE2SynCodeCompletion.GetInsertStrType(Entry: TSE2Type): string;
begin
  result := Entry.Name;
end;

function TSE2SynCodeCompletion.GetInsertStrUnit(const Name: string): string;
begin
  result := Name;
end;

function TSE2SynCodeCompletion.GetInsertStrVar(
  Entry: TSE2Variable): string;
begin
  result := Entry.Name;
end;

{ TSE2ParamCompletion }

procedure TSE2ParamCompletion.CompileCallBack(Sender: TObject;
  CodePos, ParamIndex: integer; CurrentMethod, ParamMethod: TSE2Method; Parent: TSE2BaseType;
  TargetType: TSE2Type; TokenType: TSE2TokenType; StaticOnly: boolean);
var i    : integer;
    List : TSE2BaseTypeList;
begin
  FEventRaised := ParamMethod <> nil;
  FParamIndex  := ParamIndex;
  FCanExecute  := True;
  if TokenType in [sesString, sesInteger, sesFloat] then
     FCanExecute := False;
     
  if FEventRaised then
  begin
    if ParamMethod.IsOverload then
    begin
      TSE2Parser(Sender).GetOverloadedMethods(nil, ParamMethod, List);
      try
        for i:=0 to List.Count-1 do
          ProcessParam(TSE2Method(List[i]));
      finally
        List.Free;
      end;
    end else
      ProcessParam(ParamMethod);
  end;
end;

constructor TSE2ParamCompletion.Create(CallBack: TSE2AddItemEvent);
begin
  inherited Create;
  FOnAddItem := CallBack;
  FCompiler  := TSE2Compiler.Create;
end;

destructor TSE2ParamCompletion.Destroy;
begin
  FreeAndNil(FCompiler);
  inherited;
end;

procedure TSE2ParamCompletion.DoAddItem(const DispText: string);
begin
  if Assigned(FOnAddItem) then
     FOnAddItem(Self, DispText, '');
end;

function TSE2ParamCompletion.GetParamCompletion(const Source: string;
  CursorPos: integer; var ParamIndex: integer): boolean;
var UnitMngr : TSE2UnitCacheMngr;
begin
  FEventRaised := False;

  UnitMngr := nil;
  if Assigned(FOnGetUnitMngr) then
     FOnGetUnitMngr(Self, UnitMngr);

  FCompiler.UnitCache := UnitMngr;
  FCompiler.CodeComplete(Source, CompileCallBack, CursorPos);

  ParamIndex := Self.FParamIndex;
  result     := FEventRaised and FCanExecute;
end;

procedure TSE2ParamCompletion.ProcessParam(Method: TSE2Method);
var iStart: integer;
    i     : integer;
    s     : string;
begin
  iStart := 0;
  if Method.HasSelfParam then
     iStart := 1;

  if Method.Params.Count <= iStart then
  begin
    DoAddItem(FormatParamStr(nil, True));
    exit;
  end;

  s := '';
  for i:=iStart to Method.Params.Count-1 do
  begin
    s := s + FormatParamStr(TSE2Parameter(Method.Params[i]), i = Method.Params.Count-1);
  end;
  DoAddItem(s);
end;

{ TSE2SynParamCompletion }

function TSE2SynParamCompletion.FormatParamStr(Entry: TSE2Parameter;
  IsLastParam: boolean): string;
begin
  if Entry = nil then
  begin
    result := '"* no parameter expected *"';
  end else
  begin
    result := '"';
    case Entry.ParameterType of
    ptConst : result := result + '\color{clGreen}const\color{clBlack} ';
    ptVar   : result := result + '\color{clMaroon}var\color{clBlack} ';
    end;

    result := result + '\color{clNavy}' + Entry.Name + ': \color{clBlack}'+Entry.AType.Name+';"';
    if not IsLastParam then
       result := result + ', ';

  end;
end;

end.
