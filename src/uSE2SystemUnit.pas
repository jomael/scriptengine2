unit uSE2SystemUnit;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2Types, uSE2BaseTypes, uSE2Consts, uSE2OpCode, uSE2PEData;

type
  TSE2SystemUnit = class(TSE2Object)
  protected
    class procedure FillBaseTypes(AUnit: TSE2Unit);
    class function  FillTObject(AUnit: TSE2Unit): TSE2Class;
    class procedure FillTExternalObject(AUnit: TSE2Unit; pTObject: TSE2Class);
  public
    class procedure FillSystemUnit(AUnit: TSE2Unit);
  end;

{ Classes }
function  CreateScriptClassObject(const Meta: TSE2MetaEntry; PE: TSE2PE): Pointer;
procedure DestroyScriptClassObject(AClass: Pointer);

{ Records }
function  CreateScriptRecord(const Meta: TSE2MetaEntry; PE: TSE2PE): Pointer;
procedure DestroyScriptRecord(ARecord: Pointer);
function  DuplicateScriptRecord(ARecord: Pointer): Pointer;
procedure CopyScriptRecord(ASource, ADest: Pointer);

{ Helper }
function  GetClassMethods(AClass: Pointer): TSE2DynMethodList;
function  GetClassMeta(AClass: Pointer): TSE2MetaEntry;

const
  vmtScriptInstanceSize = -4;
  vmtScriptMetaEntry    = -8;
  vmtScriptPointerList  = -12;
  vmtScriptMethodList   = -16;

  vmtScriptMetaTotalSize = 16;

implementation

uses
  uSE2UnitManager, uSE2RunAccess,

  { YOU ARE NOT ALLOWED TO CHANGE AND/OR TO REMOVE THIS COMMENT AND/OR THE FOLLOWING LINE }
  uSE2IncDateTime, uSE2IncInfo, uSE2IncConsole, uSE2IncConvert, uSE2IncMath, uSE2IncStrings,
  SysUtils;

{ TSE2SystemUnit }

class procedure TSE2SystemUnit.FillBaseTypes(AUnit: TSE2Unit);

  function  FindType(Name: string): TSE2BaseType;
  begin
    result := AUnit.TypeList.FindItem(Name, '');
  end;

  procedure DoAddType(const Name: string; BaseType: TSE2TypeIdent; Size: integer; Visibility: TSE2Visibility; Parent: TSE2BaseType);
  var aType : TSE2Type;
  begin
    aType := TSE2Type.Create;
    aType.Name        := Name;
    aType.AUnitName    := AUnit.Name;
    aType.DataSize    := Size;
    aType.AType       := BaseType;
    if Parent <> nil then
    begin
      aType.InheritFrom := Parent;
      //aType.Compatibles.AddCompatbile(Parent, Parent.BaseCompatibles, True);
    end;
    aType.Visibility  := Visibility;
    AUnit.TypeList.Add(aType);
  end;

  (*procedure SetCompatible(aTypeName: string; Compatible: array of string);
  var i     : integer;
      aType : TSE2BaseType;
      pType : TSE2BaseType;
  begin
    aType := AUnit.TypeList.FindItem(aTypeName, '', nil, nil, nil, CAllVisibilities);
    if aType = nil then
       exit;

    for i:=Low(Compatible) to High(Compatible) do
    begin
      pType := AUnit.TypeList.FindItem(Compatible[i], '', nil, nil, nil, CAllVisibilities);
      if pType <> nil then
      begin
        aType.Compatibles.AddCompatbile(pType, pType.BaseCompatibles, True);
        pType.Compatibles.AddCompatbile(aType, aType.BaseCompatibles, True);
      end;
    end;
  end;   *)

var c: TSE2Constant;
begin
  (*OrdType            := TSE2Type.Create;
  OrdType.Name       := 'ordinal';
  OrdType.UnitName   := AUnit.Name;
  OrdType.DataSize   := -1;
  OrdType.Visibility := visPrivate;
  OrdType.BaseCompatibles := [sesPlus, sesMinus, sesStar, sesDiv,
                               sesMod, sesAnd, sesOr, sesXor, sesNot,
                               sesSmaller, sesSmallerEqual, sesEqual, sesBiggerEqual, sesBigger, sesUnEqual,
                               sesImplicitCast, sesExplicitCast];
  AUnit.TypeList.Add(OrdType);        *)



  DoAddType('boolean', btBoolean, SizeOf(TbtU8), visPublic, nil);
  DoAddType('byte', btU8, SizeOf(TbtU8), visPublic, nil);
  DoAddType('shortint', btS8, SizeOf(TbtS8), visPublic, nil);
  DoAddType('word', btU16, SizeOf(TbtU16), visPublic, nil);
  DoAddType('smallint', btS16, SizeOf(TbtS16), visPublic, nil);
  DoAddType('cardinal', btU32, SizeOf(TbtU32), visPublic, nil);  
  DoAddType('integer', btS32, SizeOf(TbtS32), visPublic, nil);
  DoAddType('int64', btS64, SizeOf(TbtS64), visPublic, nil);
  
  DoAddType('LongWord', 0, 0, visPublic, FindType('cardinal'));
  DoAddType('Longint', 0, 0, visPublic, FindType('integer'));
  DoAddType('DWord', 0, 0, visPublic, FindType('cardinal'));
  DoAddType('THandle', 0, 0, visPublic, FindType('LongWord'));

  c := TSE2Constant.Create;
  c.AType      := TSE2Type(FindType('boolean'));
  c.Name       := 'True';
  c.AUnitName   := C_SE2SystemUnitName;
  c.AsInteger  := 1;
  c.Visibility := visPublic;
  AUnit.ElemList.Add(c);

  c := TSE2Constant.Create;
  c.AType      := TSE2Type(FindType('boolean'));
  c.Name       := 'False';
  c.AUnitName   := C_SE2SystemUnitName;
  c.AsInteger  := 0;
  c.Visibility := visPublic;
  AUnit.ElemList.Add(c);



  DoAddType('string', btString, SizeOf(Pointer), visPublic, nil);
  DoAddType('UTF8String', btUTF8String, SizeOf(Pointer), visPublic, nil);
  DoAddType('WideString', btWideString, SizeOf(Pointer), visPublic, nil);
  DoAddType('PChar', btPChar, SizeOf(Pointer), visPublic, nil);

  DoAddType('single', btSingle, SizeOf(Single), visPublic, nil);
  DoAddType('double', btDouble, SizeOf(Double), visPublic, nil);

  DoAddType('pointer', btPointer, SizeOf(Pointer), visPublic, nil);

  (*SetCompatible('byte'    , ['shortint', 'word', 'smallint', 'cardinal', 'integer', 'int64']);
  SetCompatible('shortint', ['byte', 'word', 'smallint', 'cardinal', 'integer', 'int64']);
  SetCompatible('word'    , ['byte', 'shortint', 'smallint', 'cardinal', 'integer', 'int64']);
  SetCompatible('smallint', ['byte', 'shortint', 'word', 'cardinal', 'integer', 'int64']);
  SetCompatible('cardinal', ['byte', 'shortint', 'word', 'smallint', 'integer', 'int64']);
  SetCompatible('integer' , ['byte', 'shortint', 'word', 'smallint', 'cardinal', 'int64']);
  SetCompatible('int64'   , ['byte', 'shortint', 'word', 'smallint', 'cardinal', 'integer']);         *)
end;

class function TSE2SystemUnit.FillTObject(AUnit: TSE2Unit): TSE2Class;
var aClass : TSE2Class;
    method : TSE2Method;
    param  : TSE2Parameter;
    destruct : TSE2Method;

  function FindType(const Name: string): TSE2Type;
  var i: integer;
  begin
    for i:=0 to AUnit.TypeList.Count-1 do
      if AUnit.TypeList[i].IsName(Name) then
        if AUnit.TypeList[i] is TSE2Type then
        begin
          result := TSE2Type(AUnit.TypeList[i]);
          exit;
        end;
    result := nil;
  end;

begin
  aClass             := TSE2Class.Create;
  aClass.Name        := C_SE2TObjectName;
  aClass.AUnitName    := C_SE2SystemUnitName;
  aClass.AType       := btObject;
  aClass.Visibility  := visPublic;
  aClass.DataSize    := SizeOf(Pointer);
  AUnit.TypeList.Add(aClass);

  // CONSTRUCTOR
  method := TSE2Method.Create;
  AUnit.ElemList.Add(method);
  method.Parent       := aClass;
  method.Name         := 'Create';
  method.AUnitName     := C_SE2SystemUnitName;
  method.IsStatic     := True;
  method.IsVirtual    := True;
  method.DynamicIndex := 0;
  method.MethodType   := mtConstructor;
  method.IsExternal   := False;
  method.Used         := True;
  method.Parent       := aClass;
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));
  method.CallConvention := callRegister;
  method.Visibility     := visPublic;

  method.ReturnValue          := TSE2Variable.Create;
  method.ReturnValue.Name     := '!result';
  method.ReturnValue.AUnitName := C_SE2SystemUnitName;
  method.ReturnValue.AType    := aClass;
  method.ReturnValue.Visibility := visPrivate;

  Param := TSE2Parameter.Create;
  param.Name := 'Self';
  param.AUnitName := C_SE2SystemUnitName;
  Param.Visibility  := visPublic;
  Param.Parent := Method;
  Param.AType  := aClass;
  Param.Visibility := visPrivate;
  Param.IsStatic   := False;
  Param.Visibility := visProtected;
  Method.Params.Insert(0, Param);

  // Destructor
  method := TSE2Method.Create;
  AUnit.ElemList.Add(method);
  method.Parent       := aClass;
  method.Name         := 'Destroy';
  method.AUnitName     := C_SE2SystemUnitName;
  method.IsStatic     := False;
  method.MethodType   := mtDestructor;
  method.IsVirtual    := True;
  method.Used         := True;
  method.DynamicIndex := 1;
  method.IsExternal   := False;
  method.Parent       := aClass;
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));
  method.CallConvention := callRegister;
  method.Visibility     := visPublic;

  Param := TSE2Parameter.Create;
  param.Name := 'Self';
  param.AUnitName := C_SE2SystemUnitName;
  Param.Visibility  := visPublic;
  Param.Parent := Method;
  Param.AType  := aClass;
  Param.IsStatic   := False;
  Method.Params.Insert(0, Param);

  destruct := method;

  // FREE
  method := TSE2Method.Create;
  AUnit.ElemList.Add(method);
  method.Parent       := aClass;
  method.Name         := 'Free';
  method.AUnitName     := C_SE2SystemUnitName;
  method.IsStatic     := False;
  method.MethodType   := mtProcedure;
  method.IsExternal   := False;


  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btPointer), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetPtr(nil), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(6), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_JIZ(12), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_PUSHRET(6 + 3, 0), ''));
                                                                               
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALLDYN(destruct.DynamicIndex), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.SPEC_DESTROY, ''));

  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_DEC, ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_GOTO(12), ''));

              (*
  PUSH FROM -1 Dynamic
  PUSH PTR
  CMP [<>]
  JIZ 80
  PUSH FROM -1 Dynamic
  PUSH RET [78]
  CALL System.TObject.Destroy
  POP
  GOTO 80
  RET       *)

  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));
  method.CallConvention := callRegister;
  method.Visibility     := visPublic;

  Param := TSE2Parameter.Create;
  param.Name := 'Self';
  param.AUnitName := C_SE2SystemUnitName;
  Param.Visibility  := visPublic;
  Param.Parent := Method;
  Param.AType  := aClass;
  Param.Visibility := visPrivate;
  Param.IsStatic   := False;
  Param.Visibility := visProtected;
  Method.Params.Insert(0, Param);

  aClass.DynMethods := 2;
  result := aClass;

  // Assigned
  method := TSE2Method.Create;
  AUnit.ElemList.Add(method);
  method.Parent       := aClass;
  method.Name         := 'Assigned';
  method.AUnitName     := C_SE2SystemUnitName;
  method.IsStatic     := False;
  method.MethodType   := mtFunction;
  method.IsExternal   := False;


  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btPointer), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetPtr(nil), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(6), ''));   
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(-3, False), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));

  (*
  PUSH FROM -1 Dynamic
  PUSH POINTER
  MAKE POINTER 0
  COMPARE <>
  POP TO -3 Dynamic
  RET
  *)

  method.CallConvention := callRegister;
  method.Visibility     := visPublic;

  Param := TSE2Parameter.Create;
  param.Name := 'Self';
  param.AUnitName := C_SE2SystemUnitName;
  Param.Visibility  := visPublic;
  Param.Parent := Method;
  Param.AType  := aClass;
  Param.Visibility := visPrivate;
  Param.IsStatic   := False;
  Param.Visibility := visProtected;
  Method.Params.Insert(0, Param);

  method.ReturnValue          := TSE2Variable.Create;
  method.ReturnValue.Name     := 'result';
  method.ReturnValue.AUnitName := C_SE2SystemUnitName;
  method.ReturnValue.AType    := FindType('boolean');
  method.ReturnValue.Visibility := visPublic;
end;

class procedure TSE2SystemUnit.FillTExternalObject(AUnit: TSE2Unit;
  pTObject: TSE2Class);
var aClass : TSE2Class;
    method : TSE2Method;
    param  : TSE2Parameter;

  function FindType(const Name: string): TSE2Type;
  var i: integer;
  begin
    for i:=0 to AUnit.TypeList.Count-1 do
      if AUnit.TypeList[i].IsName(Name) then
        if AUnit.TypeList[i] is TSE2Type then
        begin
          result := TSE2Type(AUnit.TypeList[i]);
          exit;
        end;
    result := nil;
  end;

begin
  aClass             := TSE2Class.Create;
  aClass.Name        := C_SE2TExternalObjectName;
  aClass.AUnitName    := C_SE2SystemUnitName;
  aClass.AType       := btObject;
  aClass.Visibility  := visPublic;
  aClass.DataSize    := SizeOf(Pointer);
  AUnit.TypeList.Add(aClass);

  // CONSTRUCTOR
  method := TSE2Method.Create;
  AUnit.ElemList.Add(method);
  method.Parent       := aClass;
  method.Name         := 'Create';
  method.AUnitName     := C_SE2SystemUnitName;
  method.IsStatic     := True;
  method.MethodType   := mtConstructor;
  method.IsExternal   := True;
  method.Parent       := aClass;
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALLEX(0, 0), Method.GenLinkerName));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));
  method.CallConvention := callRegister;
  method.Visibility     := visPublic;

  method.ReturnValue          := TSE2Variable.Create;
  method.ReturnValue.Name     := '!result';
  method.ReturnValue.AUnitName := C_SE2SystemUnitName;
  method.ReturnValue.AType    := aClass;
  method.ReturnValue.Visibility := visPrivate;

  Param := TSE2Parameter.Create;
  param.Name := 'Self';
  param.AUnitName := C_SE2SystemUnitName;
  Param.Visibility  := visPublic;
  Param.Parent := Method;
  Param.AType  := aClass;
  Param.Visibility := visPrivate;
  Param.IsStatic   := False;
  Param.Visibility := visProtected;
  Method.Params.Insert(0, Param);

  // FREE
  method := TSE2Method.Create;
  AUnit.ElemList.Add(method);
  method.Parent       := aClass;
  method.Name         := 'Free';
  method.AUnitName     := C_SE2SystemUnitName;
  method.IsStatic     := False;
  method.MethodType   := mtProcedure;
  method.IsExternal   := True;
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALLEX(0, 0), Method.GenLinkerName));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));
  method.CallConvention := callRegister;
  method.Visibility     := visPublic;

  Param := TSE2Parameter.Create;
  param.Name := 'Self';
  param.AUnitName := C_SE2SystemUnitName;
  Param.Visibility  := visPublic;
  Param.Parent := Method;
  Param.AType  := aClass;
  Param.Visibility := visPrivate;
  Param.IsStatic   := False;
  Param.Visibility := visProtected;
  Method.Params.Insert(0, Param);

  // CLASS NAME
  method := TSE2Method.Create;
  AUnit.ElemList.Add(method);
  method.Parent       := aClass;
  method.Name         := 'ClassName';
  method.AUnitName     := C_SE2SystemUnitName;
  method.IsStatic     := False;
  method.MethodType   := mtFunction;
  method.IsExternal   := True;
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_CALLEX(0, 0), Method.GenLinkerName));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));
  method.CallConvention := callRegister;
  method.Visibility     := visPublic;

  method.ReturnValue          := TSE2Variable.Create;
  method.ReturnValue.Name     := '!result';
  method.ReturnValue.AUnitName := C_SE2SystemUnitName;
  method.ReturnValue.AType    := FindType('string');
  method.ReturnValue.Visibility := visPublic;

  Param := TSE2Parameter.Create;
  param.Name := 'Self';
  param.AUnitName := C_SE2SystemUnitName;
  Param.Visibility  := visPublic;
  Param.Parent := Method;
  Param.AType  := aClass;
  Param.Visibility := visPrivate;
  Param.IsStatic   := False;
  Param.Visibility := visProtected;
  Method.Params.Insert(0, Param);

  // Assigned
  method := TSE2Method.Create;
  AUnit.ElemList.Add(method);
  method.Parent       := aClass;
  method.Name         := 'Assigned';
  method.AUnitName     := C_SE2SystemUnitName;
  method.IsStatic     := False;
  method.MethodType   := mtFunction;
  method.IsExternal   := False;


  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_FROM(-1, False), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.STACK_INC(btPointer), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_SetPtr(nil), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.OP_COMPARE(6), ''));   
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.DAT_COPY_TO(-3, False), ''));
  method.OpCodes.Add(TSE2LinkOpCode.Create(TSE2OpCodeGen.FLOW_RET, ''));

  (*
  PUSH FROM -1 Dynamic
  PUSH POINTER
  MAKE POINTER 0
  COMPARE <>
  POP TO -3 Dynamic
  RET
  *)

  method.CallConvention := callRegister;
  method.Visibility     := visPublic;

  Param := TSE2Parameter.Create;
  param.Name := 'Self';
  param.AUnitName := C_SE2SystemUnitName;
  Param.Visibility  := visPublic;
  Param.Parent := Method;
  Param.AType  := aClass;
  Param.Visibility := visPrivate;
  Param.IsStatic   := False;
  Param.Visibility := visProtected;
  Method.Params.Insert(0, Param);

  method.ReturnValue          := TSE2Variable.Create;
  method.ReturnValue.Name     := 'result';
  method.ReturnValue.AUnitName := C_SE2SystemUnitName;
  method.ReturnValue.AType    := FindType('boolean');
  method.ReturnValue.Visibility := visPublic;
end;

function TObject_Create(Sender: TObject): TObject;
begin
  result := TObject.Create;
end;

procedure TObject_Free(Obj: TObject);
begin
  Obj.Free;
end;

function TObject_ClassName(Obj: TObject): string;
var s: string;
begin
  if obj = nil then
     s := ''
  else
     s := Obj.ClassName;
  result := s;
end;

type
  PSE2ClassPointerEntry = ^TSE2ClassPointerEntry;
  TSE2ClassPointerEntry = record
    Offset : integer;
    Size   : integer;
    aType  : TSE2TypeIdent;
  end;

  TSE2ClassPointerList = class(TObject)
  private
    Flist : TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TSE2ClassPointerList);                                                               
    procedure Clear;
    procedure Add(aType: TSE2TypeIdent; Offset, Size: integer);

    procedure CopyData(AClass, ADataSource: Pointer);

    procedure CreateData(AClass: Pointer; PE: TSE2PE); overload;
    procedure CreateData(AClass, ADataSource: Pointer); overload;
    procedure FreeData(AClass: Pointer);
  end;

  PSE2PointerEntry = ^TSE2PointerEntry;
  TSE2PointerEntry = record
    Offset   : integer;
    Value    : Pointer;
  end;

  TSE2PointerList = class(TObject)
  private
    FList : TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure PushPointers(Data: Pointer; StoreList: TSE2ClassPointerList);
    procedure PopPointers(Data: Pointer);
  end;


function  GetClassMethods(AClass: Pointer): TSE2DynMethodList;
begin
  result := PPointer(integer(AClass) + vmtScriptMethodList)^;
end;

function  GetClassMeta(AClass: Pointer): TSE2MetaEntry;
begin                                           
  result := PPointer(integer(AClass) + vmtScriptMetaEntry)^;
end;
  
function  GetClassData(AClass: Pointer; Offset: integer): Pointer;
begin
  result := Pointer(integer(AClass) + Offset);
end;

function CreateScriptClassObject(const Meta: TSE2MetaEntry; PE: TSE2PE): Pointer;
var runInfo : TSE2ClassPointerList;
    runCalls: TSE2DynMethodList;
    i       : integer;
    p       : PSE2RTTIEntry;
begin
  result := nil;
  if Meta <> nil then
    if Meta.MetaType = mtClass then
    begin
      GetMem(result, Meta.ParamCount + vmtScriptMetaTotalSize);
      FillChar(result^, Meta.ParamCount + vmtScriptMetaTotalSize, 0);
      result := Pointer(cardinal(result) + vmtScriptMetaTotalSize);
      
      PInteger(integer(result) + vmtScriptInstanceSize)^ := Meta.ParamCount;
      PPointer(integer(result) + vmtScriptMetaEntry)^    := Meta;

      runInfo := TSE2ClassPointerList.Create;
      PPointer(integer(result) + vmtScriptPointerList)^  := runInfo;

      runCalls := TSE2DynMethodList.Create;             
      PPointer(integer(result) + vmtScriptMethodList)^   := runCalls;

      runCalls.Assign(Meta.DynMethods);

      for i:=0 to Meta.RTTI.Count-1 do
      begin
        p := Meta.RTTI[i];
        runInfo.Add(p^.AType, p^.Offset, p^.Size);
      end;
      runInfo.CreateData(result, PE);
    end;
end;

procedure DestroyScriptClassObject(AClass: Pointer);
var iSize   : integer;
    runInfo : TSE2ClassPointerList;
    runCalls: TSE2DynMethodList;

begin
  if AClass <> nil then
  begin
    iSize  := PInteger(Integer(AClass) + vmtScriptInstanceSize)^;
    runInfo := PPointer(integer(AClass) + vmtScriptPointerList)^;
    runInfo.FreeData(AClass);
    runInfo.Free;
    runCalls := PPointer(integer(AClass) + vmtScriptMethodList)^;
    runCalls.Free;

    AClass := Pointer(cardinal(AClass) - vmtScriptMetaTotalSize);
    FreeMem(AClass, iSize + vmtScriptMetaTotalSize);
  end;
end;

function  CreateScriptRecord(const Meta: TSE2MetaEntry; PE: TSE2PE): Pointer;
var runInfo : TSE2ClassPointerList;
    runCalls: TSE2DynMethodList;
    i       : integer;
    p       : PSE2RTTIEntry;
begin
  result := nil;
  if Meta <> nil then
    if Meta.MetaType = mtRecord then
    begin
      GetMem(result, Meta.ParamCount + vmtScriptMetaTotalSize);
      FillChar(result^, Meta.ParamCount + vmtScriptMetaTotalSize, 0);
      result := Pointer(cardinal(result) + vmtScriptMetaTotalSize);

      PInteger(integer(result) + vmtScriptInstanceSize)^ := Meta.ParamCount;
      PPointer(integer(result) + vmtScriptMetaEntry)^    := Meta;

      runInfo := TSE2ClassPointerList.Create;
      PPointer(integer(result) + vmtScriptPointerList)^  := runInfo;

      runCalls := TSE2DynMethodList.Create;             
      PPointer(integer(result) + vmtScriptMethodList)^   := runCalls;

      runCalls.Assign(Meta.DynMethods);

      for i:=0 to Meta.RTTI.Count-1 do
      begin
        p := Meta.RTTI[i];
        runInfo.Add(p^.AType, p^.Offset, p^.Size);
      end;
      runInfo.CreateData(result, PE);
    end;
end;

function  DuplicateScriptRecord(ARecord: Pointer): Pointer;
var iSize   : integer;
    pReal   : Pointer;
    runInfo : TSE2ClassPointerList;
    runCalls: TSE2DynMethodList;

begin
  result := nil;
  if ARecord <> nil then
  begin
    pReal  := Pointer(cardinal(ARecord) - vmtScriptMetaTotalSize);
    iSize  := PInteger(Integer(ARecord) + vmtScriptInstanceSize)^;
    GetMem(result, iSize + vmtScriptMetaTotalSize);
    Move(pReal^, result^, iSize + vmtScriptMetaTotalSize);
    result := Pointer(cardinal(result) + vmtScriptMetaTotalSize);
    
    runInfo := TSE2ClassPointerList.Create;
    runInfo.Assign(TSE2ClassPointerList(PPointer(integer(result) + vmtScriptPointerList)^));
    PPointer(integer(result) + vmtScriptPointerList)^  := runInfo;

    runCalls := TSE2DynMethodList.Create;
    runCalls.Assign(TSE2DynMethodList(PPointer(integer(result) + vmtScriptMethodList)^));
    PPointer(integer(result) + vmtScriptMethodList)^   := runCalls;

    runInfo.CreateData(result, ARecord);
  end;
end;

procedure CopyScriptRecord(ASource, ADest: Pointer);
var PtrSave : TSE2PointerList;
    runInfo : TSE2ClassPointerList;
    iSize   : integer;
begin
  if (ASource <> nil) and (ADest <> nil) then
  begin
    PtrSave := TSE2PointerList.Create;

    runInfo := PPointer(integer(ADest) + vmtScriptPointerList)^;
    PtrSave.PushPointers(ADest, runInfo);

    iSize := PInteger(Integer(ADest) + vmtScriptInstanceSize)^;
    Move(ASource^, ADest^, iSize);

    PtrSave.PopPointers(ADest);
    runInfo.CopyData(ADest, ASource);

    PtrSave.Free;
  end;
end;

procedure DestroyScriptRecord(ARecord: Pointer);
var iSize   : integer;
    runInfo : TSE2ClassPointerList;
    runCalls: TSE2DynMethodList;
begin
  if ARecord <> nil then
  begin
    iSize  := PInteger(Integer(ARecord) + vmtScriptInstanceSize)^;
    runInfo := PPointer(integer(ARecord) + vmtScriptPointerList)^;
    runInfo.FreeData(ARecord);
    runInfo.Free;
    runCalls := PPointer(integer(ARecord) + vmtScriptMethodList)^;
    runCalls.Free;

    ARecord := Pointer(cardinal(ARecord) - vmtScriptMetaTotalSize);
    FreeMem(ARecord, iSize + vmtScriptMetaTotalSize);
  end;
end;

class procedure TSE2SystemUnit.FillSystemUnit(AUnit: TSE2Unit);
begin
  AUnit.Name       := C_SE2SystemUnitName;
  AUnit.AUnitName   := C_SE2SystemUnitName;
  AUnit.IsProgram  := False;

  FillBaseTypes(AUnit);
  FillTExternalObject(AUnit, FillTObject(AUnit));
end;

function TPersistent_Create(Self: TPersistent): TPersistent;
begin
  result := TPersistent.Create;
end;

procedure TPersistent_Assign(Self, Source: TPersistent);
begin
  Self.Assign(Source);
end;

function TPersistent_GetNamePath(Self: TPersistent): string;
begin
  result := Self.GetNamePath;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_SE2SystemUnitName) then
  begin
    Target.Method[C_SE2TExternalObjectName + '.Create[0]', C_SE2SystemUnitName] := @TObject_Create;
    Target.Method[C_SE2TExternalObjectName + '.Free[0]', C_SE2SystemUnitName] := @TObject_Free;
    Target.Method[C_SE2TExternalObjectName + '.ClassName[0]', C_SE2SystemUnitName] := @TObject_ClassName;

    Target.Method['TPersistent.Create[0]', C_SE2SystemUnitName] := @TPersistent_Create;  
    Target.Method['TPersistent.Assign[0]', C_SE2SystemUnitName] := @TPersistent_Assign;
    Target.Method['TPersistent.GetNamePath[0]', C_SE2SystemUnitName] := @TPersistent_GetNamePath;
  end;
end;


procedure Unit_GetSource(var Target: string);
begin
  Target := 'unit '+C_SE2SystemUnitName + '; '+#13#10+
  #13#10 +
  'interface'+#13#10+
  'type'+#13#10+
  '  TNotifyEvent = procedure(Sender: TObject) of object;'+#13#10+
  '  TPersistent = class(TExternalObject)'+#13#10+
  '  public'+#13#10+
  '    constructor Create; external;'+#13#10+
  '    procedure Assign(Source: TPersistent);  external;'+#13#10+
  '    function  GetNamePath: string;  external;'+#13#10+
  '  end;'+#13#10+
  'implementation end.';
end;

procedure RegisterUnit;
var p : TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.Priority          := 0;
  p.DoGetUnitSource   := Unit_GetSource;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.UnitName          := C_SE2SystemUnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

{ TSE2ClassPointerList }

procedure TSE2ClassPointerList.Add(aType: TSE2TypeIdent; Offset,
  Size: integer);
var p: PSE2ClassPointerEntry;
begin
  New(p);
  p^.Offset := Offset;
  p^.Size   := Size;
  p^.aType  := aType;
  Flist.Add(p);
end;       

procedure TSE2ClassPointerList.Assign(Source: TSE2ClassPointerList);
var i    : integer;
    p, s : PSE2ClassPointerEntry;
begin
  Clear;
  Flist.Count := Source.Flist.Count;
  for i:=0 to Source.Flist.Count-1 do
  begin
    s := Source.Flist[i];
    New(p);
    p^.Offset := s^.Offset;
    p^.Size   := s^.Size;
    p^.aType  := s^.aType;
    FList[i]  := s;
  end;
end;

procedure TSE2ClassPointerList.Clear;
var i: integer;
begin
  for i:=Flist.Count-1 downto 0 do
    Dispose(PSE2ClassPointerEntry(FList[i]));
  Flist.Clear;
end;

constructor TSE2ClassPointerList.Create;
begin
  inherited;
  Flist := TList.Create;
end;

procedure TSE2ClassPointerList.CopyData(AClass, ADataSource: Pointer);
var i  : integer;
    p  : PSE2ClassPointerEntry;
begin
  for i:=0 to Flist.Count-1 do
  begin
    p := Flist[i];
    case p.aType of
    btString     :
        begin
          PbtString(PPointer(integer(AClass) + p.Offset)^)^ := PbtString(PPointer(integer(ADataSource) + p.Offset)^)^;
        end;
    btUTF8String :
        begin
          PbtUTF8String(PPointer(integer(AClass) + p.Offset)^)^ := PbtUTF8String(PPointer(integer(ADataSource) + p.Offset)^)^;
        end;
    btWideString :
        begin
          PbtWideString(PPointer(integer(AClass) + p.Offset)^)^ := PbtWideString(PPointer(integer(ADataSource) + p.Offset)^)^;
        end;
    btPChar      :
        begin
          PbtPChar(PPointer(integer(AClass) + p.Offset)^)^ := PbtPChar(PPointer(integer(ADataSource) + p.Offset)^)^;
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          CopyScriptRecord(PPointer(integer(ADataSource) + p.Offset)^, PPointer(integer(AClass) + p.Offset)^);
        end;
    end;
  end;
end;

procedure TSE2ClassPointerList.CreateData(AClass, ADataSource: Pointer);
var i  : integer;
    p  : PSE2ClassPointerEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pP : Pointer;
begin
  for i:=0 to Flist.Count-1 do
  begin
    p := Flist[i];
    case p.aType of
    btString     :
        begin
          New(pS);
          pS^ := PbtString(PPointer(integer(ADataSource) + p.Offset)^)^;
          PPointer(integer(AClass) + p.Offset)^ := pS;
        end;
    btUTF8String :
        begin
          New(pU);
          pU^ := PbtUTF8String(PPointer(integer(ADataSource) + p.Offset)^)^;
          PPointer(integer(AClass) + p.Offset)^ := pU;
        end;
    btWideString :
        begin
          New(pW);
          pW^ := PbtWideString(PPointer(integer(ADataSource) + p.Offset)^)^;
          PPointer(integer(AClass) + p.Offset)^ := pW;
        end;
    btPChar      :
        begin
          New(pC);
          pC^ := PbtPChar(PPointer(integer(ADataSource) + p.Offset)^)^;
          PPointer(integer(AClass) + p.Offset)^ := pC;
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          pP := DuplicateScriptRecord(PPointer(integer(ADataSource) + p.Offset)^);
          PPointer(integer(AClass) + p.Offset)^ := pP;
        end;
    end;
  end;
end;

procedure TSE2ClassPointerList.CreateData(AClass: Pointer; PE: TSE2PE);
var i  : integer;
    p  : PSE2ClassPointerEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pP : Pointer;
begin
  for i:=0 to Flist.Count-1 do
  begin
    p := Flist[i];
    case p.aType of
    btString     :
        begin
          New(pS);
          pS^ := '';
          PPointer(integer(AClass) + p.Offset)^ := pS;
        end;
    btUTF8String :
        begin
          New(pU);
          pU^ := '';
          PPointer(integer(AClass) + p.Offset)^ := pU;
        end;
    btWideString :
        begin
          New(pW);
          pW^ := '';
          PPointer(integer(AClass) + p.Offset)^ := pW;
        end;
    btPChar      :
        begin
          New(pC);
          pC^ := '';
          PPointer(integer(AClass) + p.Offset)^ := pC;
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          pP := CreateScriptRecord( PE.MetaData[p.Size], PE );
          PPointer(integer(AClass) + p.Offset)^ := pP;
        end;
    end;
  end;
end;

procedure TSE2ClassPointerList.FreeData(AClass: Pointer);
var i  : integer;
    p  : PSE2ClassPointerEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pP : Pointer;
begin
  for i:=0 to Flist.Count-1 do
  begin
    p := Flist[i];
    case p.aType of
    btString     :
        begin
          pS := PPointer(integer(AClass) + p.Offset)^;
          pS^ := '';
          Dispose(pS);
        end;
    btUTF8String :
        begin
          pU := PPointer(integer(AClass) + p.Offset)^;
          pU^ := '';
          Dispose(pU);
        end;
    btWideString :
        begin
          pW := PPointer(integer(AClass) + p.Offset)^;
          pW^ := '';
          Dispose(pW);
        end;
    btPChar      :
        begin
          pC := PPointer(integer(AClass) + p.Offset)^;
          pC^ := '';
          Dispose(pC);
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          pP := PPointer(Pointer(integer(AClass) + p^.Offset))^;
          DestroyScriptRecord(pP);
        end;
    end;
  end;
end;

destructor TSE2ClassPointerList.Destroy;
begin
  Clear;
  Flist.Free;
  inherited;
end;

{ TSE2PointerList }

procedure TSE2PointerList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Dispose(PSE2PointerEntry(Flist[i]));
  Flist.Clear;
end;

constructor TSE2PointerList.Create;
begin
  inherited;
  Flist := TList.Create;
end;

destructor TSE2PointerList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TSE2PointerList.PopPointers(Data: Pointer);
var i     : integer;
    Entry : PSE2PointerEntry;
begin
  for i:=0 to Flist.Count-1 do
  begin
    Entry := Flist[i];
    PPointer(integer(Data) + Entry^.Offset)^ := Entry^.Value;
  end;
end;

procedure TSE2PointerList.PushPointers(Data: Pointer;
  StoreList: TSE2ClassPointerList);
var i  : integer;
    p  : PSE2ClassPointerEntry;


    Entry : PSE2PointerEntry;
begin
  Clear;
  FList.Count := StoreList.Flist.Count;
  for i:=0 to StoreList.Flist.Count-1 do
  begin
    p := StoreList.Flist[i];
    case p.aType of
    btString,
    btUTF8String,
    btWideString,
    btPChar,
    btRecord :
        begin
          New(Entry);
          Entry^.Offset := p.Offset;
          Entry^.Value  := PPointer(integer(Data) + p.Offset)^;
          FList[i] := Entry;
        end;
    end;
  end;
end;

initialization
  RegisterUnit;

end.
