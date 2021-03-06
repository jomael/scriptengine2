unit uSE2Linker;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2BaseTypes, uSE2Types, uSE2OpCode, uSE2Consts, uSE2Parser, uSE2PEData, uSE2Optimizer;

type
  TSE2Linker = class(TSE2Object)
  private
    FUnitList  : TSE2BaseTypeList;
    FOptimizer : TSE2Optimizer;
  protected
    procedure SetCodePos(ElemName: string; NewPos: cardinal; DebugPos: integer; TypeMeta: TSE2BaseType = nil);
    procedure SetDebugPos(ElemName: string; DebugPos: cardinal);
    procedure AddCodeOffset(OpCode: PSE2OpDefault; Offset: integer);

    function  MakeMethodParamDecl(Method: TSE2Method): AnsiString;

    procedure OptimizeMethod(Method: TSE2Method);
    procedure OptimizeMethod2(Method: TSE2Method);
    procedure OptimizeMethods(AUnit: TSE2Unit);

    procedure LinkString(PE: TSE2PE; value: TSE2LinkString);
    procedure LinkResources(PE: TSE2PE);
    procedure LinkMethod(PE: TSE2PE; Method: TSE2Method);
    procedure LinkMethods(PE: TSE2PE; AUnit: TSE2Unit);
    procedure LinkClass(PE: TSE2PE; AClass: TSE2Class);
    procedure LinkClasses(PE: TSE2PE; AUnit: TSE2Unit);
    procedure LinkRecord(PE: TSE2PE; ARecord: TSE2Record);
    procedure LinkRecords(PE: TSE2PE; AUnit: TSE2Unit);
    procedure LinkArray(PE: TSE2PE; AArray: TSE2Array);
    procedure LinkArrays(PE: TSE2PE; AUnit: TSE2Unit);
    procedure LinkMethodVariable(PE: TSE2PE; Method: TSE2MethodVariable); 
    procedure LinkMethodVariables(PE: TSE2PE; AUnit: TSE2Unit);
    procedure LinkGlobalVars(PE: TSE2PE);
    procedure LinkUnit(PE: TSE2PE; AUnit: TSE2Unit);
    procedure LinkInitialization(PE: TSE2PE);
    procedure LinkFinalization(PE: TSE2PE);
    procedure LinkMain(PE: TSE2PE);
    {$IFDEF SEII_SMART_LINKING}
    procedure ProcessUsedMethods(BaseMethod: TSE2Method);
    procedure ProcessUsage;
    {$ENDIF}

    procedure UnlinkMethod(Method: TSE2Method);
    procedure UnlinkOpCodes;

    function  GetParentClass(PE: TSE2PE; aClass: TSE2MetaEntry): TSE2MetaEntry;
    function  GetParentRecord(PE: TSE2PE; aRecord: TSE2MetaEntry): TSE2MetaEntry;
    function  GetIndexOfRecord(PE: TSE2PE; aRecord: TSE2Record): integer;
    function  GetIndexOfArray(PE: TSE2PE; aArray: TSE2Array): integer;

    procedure PostLinkClass(PE: TSE2PE; aClass: TSE2MetaEntry);
    procedure PostLinkClasses(PE: TSE2PE);
    procedure PostLinkRecord(PE: TSE2PE; aRecord: TSE2MetaEntry; MetaIndex: integer);
    procedure PostLinkRecords(PE: TSE2PE);
    procedure PostLinkArray(PE: TSE2PE; aArray: TSE2MetaEntry; MetaIndex: integer);
    procedure PostLinkArrays(PE: TSE2PE);
    procedure GenCode(PE: TSE2PE; OpCode: PSE2OpDefault);
  public
    constructor Create(const UnitList: TSE2BaseTypeList); reintroduce;
    destructor Destroy; override;

    function LinkProgram: TSE2PE;

    property UnitList : TSE2BaseTypeList read FUnitList write FUnitList;
  end;

implementation

uses SysUtils;

{ TSE2Linker }

constructor TSE2Linker.Create(const UnitList: TSE2BaseTypeList);
begin
  inherited Create;
  FUnitList := UnitList;
  FOptimizer := TSE2Optimizer.Create;
end;

destructor TSE2Linker.Destroy;
begin
  FOptimizer.Free;
  inherited;
end;

function TSE2Linker.LinkProgram: TSE2PE;
var i: integer;
begin
  result := TSE2PE.Create;
  {$IFDEF SEII_SMART_LINKING}
  ProcessUsage;
  {$ENDIF}


  // 1st of all - optimize
  for i:=0 to FUnitList.Count-1 do
    OptimizeMethods(TSE2Unit(FUnitList[i]));



  // Make as the 1st OpCode
  // this is to make a return to position 0 is not possible
  // Return to 0 means that the execution will end
  GenCode(result, TSE2OpCodeGen.NOOP);
  LinkResources(result);


  for i:=0 to FUnitList.Count-1 do
    LinkRecords(result, TSE2Unit(FUnitList[i]));

  for i:=0 to FUnitList.Count-1 do
    LinkArrays(result, TSE2Unit(FUnitList[i]));

  for i:=0 to FUnitList.Count-1 do
    LinkClasses(result, TSE2Unit(FUnitList[i]));

  for i:=0 to FUnitList.Count-1 do
    LinkMethodVariables(result, TSE2Unit(FUnitList[i]));

  //TTimeMeasure.Start;
  for i:=0 to FUnitList.Count-1 do
    LinkUnit(result, TSE2Unit(FUnitList[i]));

  //TTimeMeasure.Stop;
  // UnlinkOpCodes;

  PostLinkClasses(Result);
  PostLinkRecords(Result);
  PostLinkArrays(Result);

  LinkInitialization(result);
  LinkFinalization(result);
  LinkMain(result);
end;


procedure TSE2Linker.LinkString(PE: TSE2PE; value: TSE2LinkString);
begin
  PE.Strings.Add(value.Value);
  SetCodePos(value.GenLinkerName, PE.Strings.Count-1, -1, nil);
end;

procedure TSE2Linker.LinkResources(PE: TSE2PE);
var i, j: integer;
begin
  for i:=0 to FUnitList.Count-1 do
    for j:=0 to TSE2Unit(FUnitList[i]).Strings.Count-1 do
      LinkString(PE, TSE2LinkString(TSE2Unit(FUnitList[i]).Strings[j]));
end;

procedure TSE2Linker.LinkUnit(PE: TSE2PE; AUnit: TSE2Unit);
begin
  LinkMethods(PE, AUnit);
end;

procedure TSE2Linker.LinkClass(PE: TSE2PE; AClass: TSE2Class);
var Meta   : TSE2MetaEntry;

  procedure LinkClassRTTI(aClass: TSE2Class);
  var p: PSE2ClassRTTIEntry;
      i: integer;
  begin
    for i:=0 to aClass.RTTI.Count-1 do
    begin
      p := aClass.RTTI[i];
      Meta.RTTI.Add(p^.aType, p^.Offset, p^.Size);
    end;

    if aClass.InheritFrom is TSE2Class then
       LinkClassRTTI(TSE2Class(aClass.InheritFrom));
  end;

begin
  Meta := TSE2MetaEntry.Create;
  Meta.MetaType := mtClass;
  Meta.Name     := AClass.Name;
  Meta.AUnitName := AClass.AUnitName;
  Meta.CodePos  := -1; //AClass.Variables;
  Meta.SourcePos  := AClass.DeclPos;
  Meta.SourceLine := AClass.DeclLine;
  Meta.ParamCount := AClass.ClassSize;
  Meta.DynMethods.Count := AClass.DynMethods;
  Meta.MetaLinkName := 'META_['+Meta.Name+']['+Meta.AUnitName+']';

  if AClass.InheritFrom is TSE2Class then
     Meta.ParamDecl := AnsiString(AClass.InheritFrom.AUnitName + '.' + AClass.InheritFrom.Name);

  LinkClassRTTI(AClass);

  PE.MetaData.Add(Meta);
end;

procedure TSE2Linker.LinkClasses(PE: TSE2PE; AUnit: TSE2Unit);
var i      : integer;
    AClass : TSE2Class;
begin
  for i:=0 to AUnit.TypeList.Count-1 do
  begin
    if AUnit.TypeList[i] is TSE2Class then
    begin
      AClass := TSE2Class(AUnit.TypeList[i]);
      LinkClass(PE, AClass);
    end;
  end;
end;

procedure TSE2Linker.LinkRecord(PE: TSE2PE; ARecord: TSE2Record);
var Meta   : TSE2MetaEntry;

  procedure LinkRecordRTTI(ARecord: TSE2Record);
  var p: PSE2ClassRTTIEntry;
      i: integer;
  begin
    for i:=0 to ARecord.RTTI.Count-1 do
    begin
      p := ARecord.RTTI[i];
      Meta.RTTI.Add(p^.aType, p^.Offset, p^.Size);
    end;

    if ARecord.InheritFrom is TSE2Record then
       LinkRecordRTTI(TSE2Record(ARecord.InheritFrom));
  end;

begin
  Meta := TSE2MetaEntry.Create;
  Meta.MetaType := mtRecord;
  Meta.Name     := ARecord.Name;
  Meta.AUnitName := ARecord.AUnitName;
  Meta.CodePos  := integer(ARecord); //AClass.Variables;
  Meta.SourcePos  := ARecord.DeclPos;
  Meta.SourceLine := ARecord.DeclLine;
  Meta.ParamCount := ARecord.RecordSize;
  Meta.DynMethods.Count := 0; //ARecord.DynMethods;
  Meta.MetaLinkName := 'META_['+Meta.Name+']['+Meta.AUnitName+']';

  if ARecord.InheritFrom is TSE2Record then
     Meta.ParamDecl := AnsiString(ARecord.InheritFrom.AUnitName + '.' + ARecord.InheritFrom.Name);

  LinkRecordRTTI(ARecord);

  ARecord.MetaIndex := PE.MetaData.Count;
  PE.MetaData.Add(Meta);
end;

procedure TSE2Linker.LinkRecords(PE: TSE2PE; AUnit: TSE2Unit);
var i       : integer;
    ARecord : TSE2Record;
begin
  for i:=0 to AUnit.TypeList.Count-1 do
  begin
    if AUnit.TypeList[i] is TSE2Record then
    begin
      ARecord := TSE2Record(AUnit.TypeList[i]);
      LinkRecord(PE, ARecord);
    end;
  end;
end;

procedure TSE2Linker.LinkGlobalVars(PE: TSE2PE);
var i, j  : integer;
    AUnit : TSE2Unit;
    Index : integer;
    pVar  : TSE2Variable;
    aType : TSE2TypeIdent;
    bFirst: boolean;
begin
  bFirst := True;
  Index := 0;
  for i:=0 to FUnitList.Count-1 do
  begin
    AUnit := TSE2Unit(FUnitList[i]);
    for j:=0 to AUnit.ElemList.Count-1 do
      if AUnit.ElemList[j] is TSE2Variable then
        if TSE2Variable(AUnit.ElemList[j]).IsStatic then
        begin
          pVar := TSE2Variable(AUnit.ElemList[j]);
          pVar.CodePos := Index;
          aType := TSE2Type(TSE2Variable(AUnit.ElemList[j]).AType.InheritRoot).AType;
          SetCodePos(pVar.GenLinkerName, Index, -1, TSE2Variable(AUnit.ElemList[j]).AType);
          GenCode(PE, TSE2OpCodeGen.MEM_MAKE(aType));
          if TSE2Variable(AUnit.ElemList[j]).AType is TSE2Record then
             GenCode(PE, TSE2OpCodeGen.MEM_REC_MAKE(0, GetIndexOfRecord(PE, TSE2Record(TSE2Variable(AUnit.ElemList[j]).AType))))
          else
          if TSE2Variable(AUnit.ElemList[j]).AType is TSE2Array then
          begin
            GenCode(PE, TSE2OpCodeGen.STACK_INC(btArray));
            if TSE2Array(TSE2Variable(AUnit.ElemList[j]).AType).IsDynamic then
               GenCode(PE, TSE2OpCodeGen.ARR_MAKE(0, GetIndexOfArray(PE, TSE2Array(TSE2Variable(AUnit.ElemList[j]).AType))))
            else
            begin
              GenCode(PE, TSE2OpCodeGen.DAT_PUSHInt32(TSE2Array(TSE2Variable(AUnit.ElemList[j]).AType).ArrayCount));
              GenCode(PE, TSE2OpCodeGen.ARR_SLEN(0, GetIndexOfArray(PE, TSE2Array(TSE2Variable(AUnit.ElemList[j]).AType))));
            end;
            GenCode(PE, TSE2OpCodeGen.DAT_COPY_TO(Index, True));
          end else
          if (i = 0) and bFirst then
            GenCode(PE, TSE2OpCodeGen.SAVE_INSTANCE)
          else
            GenCode(PE, TSE2OpCodeGen.DAT_CLEAR(True));

          bFirst := False;
          Index := Index + 1;
        end;
  end;
end;          

function TSE2Linker.MakeMethodParamDecl(Method: TSE2Method): AnsiString;
var i: integer;
    d: byte;
begin
  if Method.Params.Count = 0 then
  begin
    result := '';
    exit;
  end;
  SetLength(result, Method.Params.Count);
  for i:=0 to Method.Params.Count-1 do
  begin
    d := TSE2Type(TSE2Parameter(Method.Params[i]).AType.InheritRoot).AType;
    if d = 0 then
    begin
      if TSE2Parameter(Method.Params[i]).AType is TSE2Class then
         d := btObject;
    end;
    d := TSE2ParamHelper.MakeParamData(d, TSE2Parameter(Method.Params[i]).ParameterType = ptVar);
    result[i+1] := AnsiChar(d);
  end;
end;

function TSE2Linker.GetIndexOfRecord(PE: TSE2PE;
  aRecord: TSE2Record): integer;

  function FindRecordMeta(const MetaName: string): integer;
  var s: string;
  begin
    for result := 0 to PE.MetaData.Count-1 do
    begin
      s := 'META_['+PE.MetaData[result].Name+']['+PE.MetaData[result].AUnitName+']';
      if s = MetaName then
         exit;
    end;
    result := -1;
  end;

begin
  result := FindRecordMeta('META_' + aRecord.GenLinkerName);
end;

function TSE2Linker.GetIndexOfArray(PE: TSE2PE;
  aArray: TSE2Array): integer;

  function FindArrayMeta(const MetaName: string): integer;
  var s: string;
  begin
    for result := 0 to PE.MetaData.Count-1 do
    begin
      s := 'META_['+PE.MetaData[result].Name+']['+PE.MetaData[result].AUnitName+']';
      if s = MetaName then
         exit;
    end;
    result := -1;
  end;

begin
  result := FindArrayMeta('META_' + aArray.GenLinkerName);
end;

procedure TSE2Linker.LinkMethod(PE: TSE2PE; Method: TSE2Method);
var CodePos: integer;
    i      : integer;
    Meta   : TSE2MetaEntry;

  function FindClassMeta(const MetaName: string): integer;
  var //s: string;
      n: integer;
  begin
    n := MakeHash(MetaName);
    for result := 0 to PE.MetaData.Count-1 do
    begin
      if n = PE.MetaData[result].MetaLinkHash then
        if MetaName = PE.MetaData[result].MetaLinkName then
      //s := 'META_['+PE.MetaData[result].Name+']['+PE.MetaData[result].AUnitName+']';
      //if n = MakeHash(s) then
        //if s = MetaName then
           exit;
    end;
    result := -1;
  end;

  function GetRecordMeta(const Rec: TSE2Record): string;
  begin
    result := 'META_['+Rec.Name+']['+rec.AUnitName+']';
  end;

  function GetArrayMeta(const Arr: TSE2Array): string;
  begin
    result := 'META_['+Arr.Name+']['+Arr.AUnitName+']';
  end;

  function FindMeta(const MetaName: string; const default: integer = -1): integer;
  var n: integer;
  begin
    n := MakeHash(MetaName);
    for result := 0 to PE.MetaData.Count-1 do
    begin
      if n = PE.MetaData[result].MetaLinkHash then
        if MetaName = PE.MetaData[result].MetaLinkName then
           exit;
    end;
    result := default;
  end;

  procedure SetupDynamicMethod(Method: TSE2Method; aClass: TSE2Class);
  var meta: TSE2MetaEntry;
  begin
    meta := PE.MetaData[FindClassMeta('META_'+aClass.GenLinkerName)];
    if meta <> nil then
    begin
      if meta.DynMethods.Count <= Method.DynamicIndex then
         raise Exception.Create('Dyn Method assignment failed: ' + IntToStr(Method.DynamicIndex) + ' is not in range! Max: ' + IntToStr(meta.DynMethods.Count) +
               #13#10 + 'Class: ' + aClass.GetStrongName +
               #13#10 + 'Method: ' + Method.GetStrongName +
               #13#10 + 'Declaration: ' + IntToStr(Method.DeclLine));
      meta.DynMethods[Method.DynamicIndex] := (Method.CodePos);
    end;
  end;

var OpCode: TSE2LinkOpCode;
begin
  if (Method.OpCodes.Count = 0) and (not Method.IsExternal) and (not Method.IsAbstract) then
     exit;

  if (not Method.Used) and (not Method.IsExport) then
     exit;

  Meta := TSE2MetaEntry.Create;
  Meta.MetaType    := mtMethod;
  Meta.Name        := Method.Name;
  Meta.AUnitName    := Method.AUnitName;
  Meta.CodePos     := PE.OpCodes.Count;
  Meta.SourcePos   := Method.DeclPos;
  Meta.SourceLine  := Method.DeclLine;
  Meta.ParamCount  := Method.Params.Count;
  if Method.ReturnValue <> nil then
     Meta.ResultType := TSE2Type(Method.ReturnValue.AType.InheritRoot).AType;
  Meta.IsExternal  := Method.IsExternal;
  Meta.CallType    := Method.CallConvention;
  Meta.HasSelf     := Method.HasSelfParam;
  Meta.ParamDecl   := MakeMethodParamDecl(Method);

  Meta.LibraryName := Method.ExternalLib;
  Meta.LibPosition := Method.ExternalName;
  Meta.LibIndex    := Method.ExternalIndex;

  if Method.ReturnValue <> nil then
  begin
    if Method.ReturnValue.AType is TSE2Record then
    begin
      Meta.RTTI.Add(btRecord, -1, FindMeta(GetRecordMeta(TSE2Record(Method.ReturnValue.AType))));
    end else
    if Method.ReturnValue.AType is TSE2Array then
    begin
      Meta.RTTI.Add(btArray, -1, FindMeta(GetArrayMeta(TSE2Array(Method.ReturnValue.AType))));
    end;
  end;

  for i:=0 to Method.Params.Count-1 do
    if TSE2Parameter(Method.Params[i]).AType is TSE2Record then
      Meta.RTTI.Add(btRecord, i, FindMeta(GetRecordMeta(TSE2Record(TSE2Parameter(Method.Params[i]).AType))))
    else
    if TSE2Parameter(Method.Params[i]).AType is TSE2Array then
      Meta.RTTI.Add(btArray, i, FindMeta(GetArrayMeta(TSE2Array(TSE2Parameter(Method.Params[i]).AType))));

  PE.MetaData.Add(Meta);

  CodePos := PE.OpCodes.Count;
  Method.CodePos  := CodePos;
  Method.DebugPos := PE.MetaData.Count-1;

  SetCodePos(Method.GenLinkerName, CodePos, PE.MetaData.Count - 1);
  //  * DO NOT USE - Integrated in "SetCodePos" - if it does not work correctly, used it again *
  //  * SetDebugPos(Method.GenLinkerName, PE.MetaData.Count-1);                                *
  //  * END DO NOT USE                                                                         *

  if Method.Parent is TSE2Class then
    if Method.IsOverride or Method.IsVirtual or Method.IsAbstract then
    begin
      SetupDynamicMethod(Method, TSE2Class(Method.Parent));
    end;

  if (Method.IsVirtual or Method.IsAbstract or Method.IsOverride) and (not Method.IsExternal) then
     Meta.DynIndex := Method.DynamicIndex
  else
     Meta.DynIndex := -1;

  if Method.Parent <> nil then
     Meta.Name := Method.Parent.Name + '.' + Meta.Name;

  Meta.Name := Meta.Name + '['+TSE2Converter.IntToStr(Method.ID)+']';


  for i:=0 to Method.OpCodes.Count-1 do
  begin
    OpCode := Method.OpCodes[i];
    // Do not set .OpCode := nil here - later links will fail
    PE.OpCodes.Add(OpCode.OpCode);

    // If OpCode is not linked to a global entry,
    // add the CodeOffset to every OpCode
    // right variable will be choosen in "AddCodeOffset"
    if OpCode.CodeIndex = '' then
       AddCodeOffset(OpCode.OpCode, CodePos)
    else
    begin
      if OpCode.OpCode.OpCode = soFLOW_CALLEX then
         PSE2OpFLOW_CALLEX(OpCode.OpCode).MetaIndex := PE.MetaData.Count - 1;

      case OpCode.OpCode.OpCode of
      soSPEC_CREATE          : PSE2OpSPEC_CREATE(OpCode.OpCode).MetaIndex := FindClassMeta(OpCode.CodeIndex);
      soMETA_PUSH            : PSE2OpMETA_PUSH(OpCode.OpCode).MetaIndex := FindClassMeta(OpCode.CodeIndex);
      soMETA_SHARE           : PSE2OpMETA_SHARE(OpCode.OpCode).MetaIndex := FindClassMeta(OpCode.CodeIndex);
      soMETA_CAST            : PSE2OpMETA_CAST(OpCode.OpCode).MetaIndex := FindClassMeta(OpCode.CodeIndex);

      soREC_MAKE             : PSE2OpREC_MAKE(OpCode.OpCode).MetaIndex := FindMeta(OpCode.CodeIndex);
      soREC_EQUAL            : PSE2OpREC_EQUAL(OpCode.OpCode).MetaIndex := FindMeta(OpCode.CodeIndex);
      soREC_UNEQUAL          : PSE2OpREC_UNEQUAL(OpCode.OpCode).MetaIndex := FindMeta(OpCode.CodeIndex);
      soREC_COPY_TO          : PSE2OpREC_COPY_TO(OpCode.OpCode).MetaIndex := FindMeta(OpCode.CodeIndex, PSE2OpREC_COPY_TO(OpCode.OpCode).MetaIndex);

      soARR_COPY_TO          : PSE2OpARR_COPY_TO(OpCode.OpCode).MetaIndex := FindMeta(OpCode.CodeIndex, PSE2OpARR_COPY_TO(OpCode.OpCode).MetaIndex);
      soARR_MAKE             : PSE2OpARR_MAKE(OpCode.OpCode).MetaIndex := FindMeta(OpCode.CodeIndex);
      soARR_SLEN             : PSE2OpARR_SLEN(OpCode.OpCode).MetaIndex := FindMeta(OpCode.CodeIndex);
      soARR_SFL              : PSE2OpARR_SFL(OpCode.OpCode).MetaIndex := FindMeta(OpCode.CodeIndex);
      end;
    end;
  end;
end;

procedure TSE2Linker.LinkMethodVariable(PE: TSE2PE;
  Method: TSE2MethodVariable);
var i      : integer;
    Meta   : TSE2MetaEntry;

  function FindClassMeta(const MetaName: string): integer;
  var //s: string;
      n: integer;
  begin
    n := MakeHash(MetaName);
    for result := 0 to PE.MetaData.Count-1 do
    begin
      if n = PE.MetaData[result].MetaLinkHash then
        if MetaName = PE.MetaData[result].MetaLinkName then
      //s := 'META_['+PE.MetaData[result].Name+']['+PE.MetaData[result].AUnitName+']';
      //if n = MakeHash(s) then
        //if s = MetaName then
           exit;
    end;
    result := -1;
  end;

  function GetRecordMeta(const Rec: TSE2Record): string;
  begin
    result := 'META_['+Rec.Name+']['+rec.AUnitName+']';
  end;
  

  function FindRecordMeta(const MetaName: string; const default: integer = -1): integer;
  var //s: string;
      n: integer;
  begin
    n := MakeHash(MetaName);
    for result := 0 to PE.MetaData.Count-1 do
    begin
      if n = PE.MetaData[result].MetaLinkHash then
        if MetaName = PE.MetaData[result].MetaLinkName then

      //s := 'META_['+PE.MetaData[result].Name+']['+PE.MetaData[result].AUnitName+']';
      //if n = MakeHash(s) then
        //if s = MetaName then
           exit;
    end;
    result := default;
  end;

begin
  if (not Method.Method.IsExternal) then
     exit;

  Meta := TSE2MetaEntry.Create;
  Meta.MetaType    := mtMethod;
  Meta.Name        := Method.Name;
  Meta.AUnitName    := Method.AUnitName;
  Meta.CodePos     := -1;
  Meta.SourcePos   := Method.DeclPos;
  Meta.SourceLine  := Method.DeclLine;
  Meta.ParamCount  := Method.Method.Params.Count;
  if Method.Method.ReturnValue <> nil then
     Meta.ResultType := TSE2Type(Method.Method.ReturnValue.AType.InheritRoot).AType;
  Meta.IsExternal  := Method.Method.IsExternal;
  Meta.CallType    := Method.Method.CallConvention;
  Meta.HasSelf     := Method.Method.HasSelfParam;
  Meta.ParamDecl   := MakeMethodParamDecl(Method.Method);

  if Method.Method.ReturnValue <> nil then
    if Method.Method.ReturnValue.AType is TSE2Record then
    begin
      Meta.RTTI.Add(btRecord, -1, FindRecordMeta(GetRecordMeta(TSE2Record(Method.Method.ReturnValue.AType))));
    end;

  for i:=0 to Method.Method.Params.Count-1 do
    if TSE2Parameter(Method.Method.Params[i]).AType is TSE2Record then
      Meta.RTTI.Add(btRecord, i, FindRecordMeta(GetRecordMeta(TSE2Record(TSE2Parameter(Method.Method.Params[i]).AType))));

  PE.MetaData.Add(Meta);
  Method.Method.CodePos  := -1;
  Method.Method.DebugPos := PE.MetaData.Count-1;
  Meta.MetaLinkName := Method.Method.GenLinkerName();
  Meta.DynIndex := -1;

  Meta.Name := Meta.Name + '['+TSE2Converter.IntToStr(Method.ID)+']';
end;

procedure TSE2Linker.LinkMethodVariables(PE: TSE2PE; AUnit: TSE2Unit);
var i: integer;
begin
  for i:=0 to AUnit.TypeList.Count-1 do
    if TObject(AUnit.TypeList.List.List[i]) is TSE2MethodVariable then
      LinkMethodVariable(PE, TSE2MethodVariable(AUnit.TypeList.List.List[i]));
end;

procedure TSE2Linker.LinkMethods(PE: TSE2PE; AUnit: TSE2Unit);
var i: integer;
begin
  for i:=0 to AUnit.ElemList.Count-1 do
    if TObject(AUnit.ElemList.List.List[i]) is TSE2Method then
      LinkMethod(PE, TSE2Method(AUnit.ElemList.List.List[i]));

  if AUnit.AInitialization <> nil then
    for i:=0 to AUnit.AInitialization.Count-1 do
       LinkMethod(PE, TSE2Method(AUnit.AInitialization.List.List[i]));

  if AUnit.AFinalization <> nil then
    for i:=0 to AUnit.AFinalization.Count-1 do
       LinkMethod(PE, TSE2Method(AUnit.AFinalization.List.List[i]));

  if AUnit.Main <> nil then
     LinkMethod(PE, AUnit.Main);
end;

procedure TSE2Linker.SetDebugPos(ElemName: string; DebugPos: cardinal);
var i, j     : integer;
    Entry    : TSE2BaseType;
    NameHash : integer;

  procedure DoForMethod(Method: TSE2Method);
  var c: integer;
      p: PSE2OpDefault;
  begin
    if Method = nil then
       exit;
    if not Method.Used then
       exit;

    for c := 0 to Method.OpCodes.Count-1 do
      if Method.OpCodes[c].CodeHash = NameHash then
        if AnsiSameText(Method.OpCodes[c].CodeIndex, ElemName) then
          if Method.OpCodes[c].OpCode <> nil then
          begin
            if Method.OpCodes[c].OpCode.OpCode in [soFLOW_CALL, soFLOW_CALLEX] then
            begin
              if Method.OpCodes[c-1] <> nil then
              begin
                p := Method.OpCodes[c-1].OpCode;
                if p <> nil then
                  if p.OpCode = soFLOW_PUSHRET then
                  begin
                    PSE2OpFLOW_PUSHRET(p).DebugData := DebugPos + 1;
                  end;
              end;
            end else
            if Method.OpCodes[c].OpCode.OpCode in [soSPEC_GetProcPtr] then
            begin
              PSE2OpSPEC_GetProcPtr(Method.OpCodes[c].OpCode).MetaIndex := DebugPos;
            end else
            if Method.OpCodes[c].OpCode.OpCode in [soDEBUG_META] then
            begin
              PSE2OpDEBUG_META(Method.OpCodes[c].OpCode).MetaIndex := DebugPos + 1;
            end;
          end;
  end;

begin
  if ElemName = '' then
     exit;
  NameHash := MakeHash(ElemName);

  for i:=0 to FUnitList.Count-1 do
  begin
    for j:=0 to TSE2Unit(FUnitList[i]).ElemList.Count-1 do
    begin
      Entry := TSE2Unit(FUnitList[i]).ElemList[j];
      if Entry is TSE2Method then
         DoForMethod(TSE2Method(Entry));
    end;
    for j:=0 to TSE2Unit(FUnitList[i]).AInitialization.Count-1 do
      DoForMethod(TSE2Method(TSE2Unit(FUnitList[i]).AInitialization[j]));   
    for j:=0 to TSE2Unit(FUnitList[i]).AFinalization.Count-1 do
      DoForMethod(TSE2Method(TSE2Unit(FUnitList[i]).AFinalization[j]));
    DoForMethod(TSE2Unit(FUnitList[i]).Main);
  end;
end;

procedure TSE2Linker.SetCodePos(ElemName: string; NewPos: cardinal; DebugPos: integer; TypeMeta: TSE2BaseType = nil);
var i, j     : integer;
    NameHash : integer;

  procedure DoForMethod(Method: TSE2Method);
  var c: integer;
      p: PSE2OpDefault;
  begin
    if Method = nil then
       exit;
    if not Method.Used then
       exit;

    for c := 0 to Method.OpCodes.Count-1 do
      if Method.OpCodes[c].CodeHash = NameHash then
        if AnsiSameText(Method.OpCodes[c].CodeIndex, ElemName) then
        begin
          Assert(Method.OpCodes[c].OpCode <> nil);
          Method.OpCodes[c].SetPosition(NewPos);
          if Method.OpCodes[c].OpCode.OpCode = soREC_COPY_TO then
            if TypeMeta is TSE2Type then
               PSE2OpREC_COPY_TO(Method.OpCodes[c].OpCode).MetaIndex := TSE2Type(TypeMeta).MetaIndex;
          if Method.OpCodes[c].OpCode.OpCode = soARR_COPY_TO then
            if TypeMeta is TSE2Type then
               PSE2OpARR_COPY_TO(Method.OpCodes[c].OpCode).MetaIndex := TSE2Type(TypeMeta).MetaIndex;

          if DebugPos >= 0 then
          begin
            case Method.OpCodes[c].OpCode.OpCode of
            soFLOW_CALL, soFLOW_CALLEX :
               if Method.OpCodes[c - 1] <> nil then
               begin
                 p := Method.OpCodes[c - 1].OpCode;
                 if p <> nil then
                    if p.OpCode = soFLOW_PUSHRET then
                       PSE2OpFLOW_PUSHRET(p).DebugData := DebugPos + 1;
               end;
            soSPEC_GetProcPtr :
               PSE2OpSPEC_GetProcPtr(Method.OpCodes[c].OpCode).MetaIndex := DebugPos;
            soDEBUG_META :
               PSE2OpDEBUG_META(Method.OpCodes[c].OpCode).MetaIndex := DebugPos + 1;
            end;
          end;
        end;
  end;

var Entry    : TSE2BaseType;
begin
  if ElemName = '' then
     exit;
  NameHash := MakeHash(ElemName);

  for i:=0 to FUnitList.Count-1 do
  begin
    for j:=0 to TSE2Unit(FUnitList[i]).ElemList.Count-1 do
    begin
      Entry := TSE2Unit(FUnitList[i]).ElemList[j];
      if Entry is TSE2Method then
         DoForMethod(TSE2Method(Entry));
    end;
    for j:=0 to TSE2Unit(FUnitList[i]).AInitialization.Count-1 do
      DoForMethod(TSE2Method(TSE2Unit(FUnitList[i]).AInitialization[j]));
    for j:=0 to TSE2Unit(FUnitList[i]).AFinalization.Count-1 do
      DoForMethod(TSE2Method(TSE2Unit(FUnitList[i]).AFinalization[j]));
    DoForMethod(TSE2Unit(FUnitList[i]).Main);
  end;
end;

procedure TSE2Linker.UnlinkOpCodes;
var i, j     : integer;
    Entry    : TSE2BaseType;
begin
  for i:=0 to FUnitList.Count-1 do
  begin
    for j:=0 to TSE2Unit(FUnitList[i]).ElemList.Count-1 do
    begin
      Entry := TSE2Unit(FUnitList[i]).ElemList[j];
      if Entry is TSE2Method then
         UnlinkMethod(TSE2Method(Entry));
    end;
    for j:=0 to TSE2Unit(FUnitList[i]).AInitialization.Count-1 do
      UnlinkMethod(TSE2Method(TSE2Unit(FUnitList[i]).AInitialization[j]));
    for j:=0 to TSE2Unit(FUnitList[i]).AFinalization.Count-1 do
      UnlinkMethod(TSE2Method(TSE2Unit(FUnitList[i]).AFinalization[j]));
    UnlinkMethod(TSE2Unit(FUnitList[i]).Main);
  end;
end;

{$Warnings off}
procedure TSE2Linker.AddCodeOffset(OpCode: PSE2OpDefault; Offset: integer);
begin
  case OpCode.OpCode of
  soFLOW_GOTO       : PSE2OpFLOW_GOTO(OpCode)^.Position := PSE2OpFLOW_GOTO(OpCode)^.Position + Offset;
  soFLOW_JIZ        : PSE2OpFLOW_JIZ(OpCode)^.Position  := PSE2OpFLOW_JIZ(OpCode)^.Position + Offset;
  soFLOW_JNZ        : PSE2OpFLOW_JNZ(OpCode)^.Position  := PSE2OpFLOW_JNZ(OpCode)^.Position + Offset;   
  soFLOW_JGZ        : PSE2OpFLOW_JGZ(OpCode)^.Position  := PSE2OpFLOW_JGZ(OpCode)^.Position + Offset;
  soFLOW_JGEZ       : PSE2OpFLOW_JGEZ(OpCode)^.Position := PSE2OpFLOW_JGEZ(OpCode)^.Position + Offset;
  soFLOW_JLZ        : PSE2OpFLOW_JLZ(OpCode)^.Position  := PSE2OpFLOW_JLZ(OpCode)^.Position + Offset;
  soFLOW_JLEZ       : PSE2OpFLOW_JLEZ(OpCode)^.Position := PSE2OpFLOW_JLEZ(OpCode)^.Position + Offset;

  soFLOW_CALL       : PSE2OpFLOW_CALL(OpCode)^.Position := PSE2OpFLOW_CALL(OpCode)^.Position + Offset;
  soFLOW_PUSHRET    : PSE2OpFLOW_PUSHRET(OpCode)^.Position := PSE2OpFLOW_PUSHRET(OpCode)^.Position + Offset;
  soSAFE_BLOCK      : PSE2OpSAFE_BLOCK(OpCode)^.SkipPoint := PSE2OpSAFE_BLOCK(OpCode)^.SkipPoint + Offset;
  soSAFE_SJUMP      :
      begin
        PSE2OpSAFE_SJUMP(OpCode)^.Target    := PSE2OpSAFE_SJUMP(OpCode)^.Target  + Offset;
        PSE2OpSAFE_SJUMP(OpCode)^.ExitTo    := PSE2OpSAFE_SJUMP(OpCode)^.ExitTo  + Offset;
      end;
  soSAFE_TRYEX      :
      begin
        PSE2OpSAFE_TRYEX(OpCode)^.SavePos   := PSE2OpSAFE_TRYEX(OpCode)^.SavePos  + Offset;
        PSE2OpSAFE_TRYEX(OpCode)^.LeavePos  := PSE2OpSAFE_TRYEX(OpCode)^.LeavePos + Offset;
      end;
  soSAFE_TRYFIN     :
      begin
        PSE2OpSAFE_TRYFIN(OpCode)^.SavePos   := PSE2OpSAFE_TRYFIN(OpCode)^.SavePos  + Offset;
        PSE2OpSAFE_TRYFIN(OpCode)^.LeavePos  := PSE2OpSAFE_TRYFIN(OpCode)^.LeavePos + Offset;
      end;
  end;
end;
{$Warnings on}

procedure TSE2Linker.LinkFinalization(PE: TSE2PE);
var i, j: integer;
    numStatic : integer;
    iPos      : integer;
begin
  PE.FinalizationPoint := PE.OpCodes.Count;

  numStatic := 0;
  for i:=0 to FUnitList.Count-1 do
    for j:=0 to TSE2Unit(FUnitList[i]).ElemList.Count-1 do
      if TSE2Unit(FUnitList[i]).ElemList[j] is TSE2Variable then
        if TSE2Variable(TSE2Unit(FUnitList[i]).ElemList[j]).IsStatic then
          numStatic := numStatic + 1;

  GenCode(PE, TSE2OpCodeGen.FINIT_STACK(numStatic));

  for i:=FUnitList.Count-1 downto 0 do
    if TSE2Unit(FUnitList[i]).AFinalization <> nil then
      for j:=0 to TSE2Unit(FUnitList[i]).AFinalization.Count-1 do
        if TSE2Method(TSE2Unit(FUnitList[i]).AFinalization[j]).OpCodes.Count > 1 then
        begin
          GenCode(PE, TSE2OpCodeGen.FLOW_PUSHRET(PE.OpCodes.Count + 2, 0));
          GenCode(PE, TSE2OpCodeGen.FLOW_CALL(TSE2Method(TSE2Unit(FUnitList[i]).AFinalization[j]).CodePos));
        end;


  // unlink global records
  for i:=0 to FUnitList.Count-1 do
    for j:=0 to TSE2Unit(FUnitList[i]).ElemList.Count-1 do
      if TSE2Unit(FUnitList[i]).ElemList[j] is TSE2Variable then
        if TSE2Variable(TSE2Unit(FUnitList[i]).ElemList[j]).IsStatic then
        begin
          if TSE2Variable(TSE2Unit(FUnitList[i]).ElemList[j]).AType is TSE2Record then
          begin
            iPos := TSE2Variable(TSE2Unit(FUnitList[i]).ElemList[j]).CodePos;
            iPos := numStatic - iPos;
            GenCode(PE, TSE2OpCodeGen.MEM_REC_FREE( -iPos + 1 ));
          end else
          if TSE2Variable(TSE2Unit(FUnitList[i]).ElemList[j]).AType is TSE2Array then
          begin
            iPos := TSE2Variable(TSE2Unit(FUnitList[i]).ElemList[j]).CodePos;
            //iPos := numStatic - iPos;
            GenCode(PE, TSE2OpCodeGen.DAT_COPY_FROM( iPos, True));
            GenCode(PE, TSE2OpCodeGen.ARR_FREE( 0 ));
            GenCode(PE, TSE2OpCodeGen.STACK_DEC);
          end;
        end;

  // no unlink of global variables - this is done by the runtime automatically

  GenCode(PE, TSE2OpCodeGen.FLOW_PUSHRET(0, 0));
  GenCode(PE, TSE2OpCodeGen.FLOW_RET);
end;

procedure TSE2Linker.LinkInitialization(PE: TSE2PE);
var i, j: integer;
begin
  PE.InitializationPoint := PE.OpCodes.Count;

  LinkGlobalVars(PE);
  UnlinkOpCodes;

  for i:=0 to FUnitList.Count-1 do
    if TSE2Unit(FUnitList[i]).AInitialization <> nil then
      for j:=0 to TSE2Unit(FUnitList[i]).AInitialization.Count-1 do
        if TSE2Method(TSE2Unit(FUnitList[i]).AInitialization[j]).OpCodes.Count > 1 then
        begin
          GenCode(PE, TSE2OpCodeGen.FLOW_PUSHRET(PE.OpCodes.Count + 2, 0));
          GenCode(PE, TSE2OpCodeGen.FLOW_CALL(TSE2Method(TSE2Unit(FUnitList[i]).AInitialization[j]).CodePos));
        end;


  GenCode(PE, TSE2OpCodeGen.FLOW_PUSHRET(0, 0));
  GenCode(PE, TSE2OpCodeGen.FLOW_RET);
end;

procedure TSE2Linker.LinkMain(PE: TSE2PE);
var MainMethod : TSE2Method;
    i          : integer;
begin
  MainMethod := nil;
  for i:=0 to FUnitList.Count-1 do
  begin
    if TSE2Unit(FUnitList[i]).IsProgram then
    begin
      MainMethod := TSE2Unit(FUnitList[i]).Main;
      break;
    end;
  end;

  if MainMethod = nil then
     PE.MainMethodPoint := 0
  else
  begin
    PE.MainMethodPoint := PE.OpCodes.Count;
    GenCode(PE, TSE2OpCodeGen.FLOW_PUSHRET(0, 0));
    GenCode(PE, TSE2OpCodeGen.FLOW_CALL(MainMethod.CodePos));
  end;
end;

procedure TSE2Linker.GenCode(PE: TSE2PE; OpCode: PSE2OpDefault);
begin
  PE.OpCodes.Add(OpCode);
end;

procedure TSE2Linker.UnlinkMethod(Method: TSE2Method);
var c: integer;
begin
  if Method = nil then
     exit;

  if (not Method.Used) and (not Method.IsExport) then
     exit;

  for c := 0 to Method.OpCodes.Count-1 do
    Method.OpCodes[c].OpCode := nil;
end;

procedure TSE2Linker.OptimizeMethod(Method: TSE2Method);
var i, j    : integer;
    OpCodes : array[0..10] of TSE2LinkOpCode;
    Mode    : integer;

  procedure AddOffsetPos(Offset, CurIndex: integer);
  var i: integer;
  begin
    for i:=0 to Method.OpCodes.Count-1 do
    begin
      if Method.OpCodes[i].GetJumpPos > CurIndex then
         Method.OpCodes[i].AddOffset(Offset, CurIndex);
    end;
  end;

  procedure DeleteUselessPushPop();
  var n, c: integer;
  begin
    if OpCodes[0].OpCode.OpCode = soDAT_COPY_FROM then
    begin
      if OpCodes[1] <> nil then
      begin
        if OpCodes[1].OpCode.OpCode = soSTACK_DEC then
        begin
          // Delete this and the next opcode
          Method.OpCodes.Delete(i);
          Method.OpCodes.Delete(i);

          // Decrease every position
          AddOffsetPos(-2, i);

          i := i - 1;
        end else
        if OpCodes[1].OpCode.OpCode = soSPEC_INCP then
        begin
          c := 2;
          for n:=2 to 9 do
            if OpCodes[n] <> nil then
            begin
              if OpCodes[n].OpCode.OpCode = soSPEC_INCP then
                 c := c + 1
              else
                 break;
            end else
              break;

          if OpCodes[c] <> nil then
            if OpCodes[c].OpCode.OpCode = soSTACK_DEC then
            begin
              Method.OpCodes.Delete(i);
              for n:=2 to c do
                Method.OpCodes.Delete(i);
              Method.OpCodes.Delete(i);

                             
              AddOffsetPos(-c - 1, i);

              i := i - 1;
            end;
        end;
      end;
    end;
  end;

  procedure InsertFastIncrement();
  var bIsStatic   : boolean;
      bHasConvert : boolean;
  begin
    if OpCodes[0].OpCode.OpCode = soDAT_COPY_FROM then
    begin
      // Copy Variable = 0; Add New Variable = 1; Set Variable Value = 2; Add-Operation = 3; Copy Variable Back = 4;
      if (OpCodes[1] <> nil) and (OpCodes[2] <> nil) and (OpCodes[3] <> nil) and (OpCodes[4] <> nil) then
        if OpCodes[1].OpCode.OpCode = soSTACK_INC then
          if (OpCodes[2].OpCode.OpCode = soDAT_SetInt) and
             ((PSE2OpDAT_SetInt(OpCodes[2].OpCode).Value and $FFFFFFFF00000000) = 0) then
          begin
            bHasConvert := False;
            if OpCodes[3].OpCode.OpCode = soDAT_CONVERT then
            begin
              if PSE2OpDAT_CONVERT(OpCodes[3].OpCode).NewType in [btSingle, btDouble] then
                 exit;

              if OpCodes[5] = nil then
                 exit;

              OpCodes[3] := OpCodes[4];
              OpCodes[4] := OpCodes[5];
              bHasConvert := True;
            end;

            if OpCodes[3].OpCode.OpCode = soOP_OPERATION then
              if PSE2OpOP_OPERATION(OpCodes[3].OpCode).OpType in [2, 3] then
                if OpCodes[4].OpCode.OpCode = soDAT_COPY_TO then
                  if (PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Static = PSE2OpDAT_COPY_TO(OpCodes[4].OpCode).Static) and
                     (
                       (PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Static and (PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Source = PSE2OpDAT_COPY_TO(OpCodes[4].OpCode).Target)) or 
                       ((not PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Static) and (PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Source = PSE2OpDAT_COPY_TO(OpCodes[4].OpCode).Target + 1))
                     ) then
                  begin
                    bIsStatic := PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Static;
                    case PSE2OpOP_OPERATION(OpCodes[3].OpCode).OpType of
                    2 : begin
                          if bIsStatic then
                          begin
                            OpCodes[0].OpCode.OpCode := soINT_INCSTATIC;
                            OpCodes[0].CodeIndex     := OpCodes[4].CodeIndex;
                            //PSE2OpINT_INCSTATIC(OpCodes[0].OpCode).Offset := PSE2OpDAT_COPY_TO(OpCodes[4].OpCode).Target;
                          end else
                          begin
                            OpCodes[0].OpCode.OpCode := soINT_INCSTACK;
                            PSE2OpINT_INCSTATIC(OpCodes[0].OpCode).Offset := PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Source;
                          end;
                        end;
                    3 : begin
                          if bIsStatic then
                          begin
                            OpCodes[0].OpCode.OpCode := soINT_DECSTATIC;  
                            OpCodes[0].CodeIndex     := OpCodes[4].CodeIndex;
                            //PSE2OpINT_INCSTATIC(OpCodes[0].OpCode).Offset := PSE2OpDAT_COPY_TO(OpCodes[4].OpCode).Target;
                          end else
                          begin
                            OpCodes[0].OpCode.OpCode := soINT_DECSTACK;      
                            PSE2OpINT_INCSTATIC(OpCodes[0].OpCode).Offset := PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Source;
                          end;
                        end;
                    end;


                    PSE2OpINT_INCSTATIC(OpCodes[0].OpCode).Value  := PSE2OpDAT_SetInt(OpCodes[2].OpCode).Value;

                    // Delete the obsolte opcodes
                    Method.OpCodes.Delete(i + 1);  // OpCodes[1]
                    Method.OpCodes.Delete(i + 1);  // OpCodes[2]
                    Method.OpCodes.Delete(i + 1);  // OpCodes[3]
                    Method.OpCodes.Delete(i + 1);  // OpCodes[4]
                    if bHasConvert then
                       Method.OpCodes.Delete(i + 1);

                    // Decrease every position
                    if bHasConvert then
                      AddOffsetPos(-5, i)
                    else
                      AddOffsetPos(-4, i);

                    //i := i - 1;
                    exit;
                  end;
          end;
    end;
  end;

  procedure OptimizeIntValues;
  begin
    if OpCodes[0].OpCode.OpCode <> soSTACK_INC then
       exit;

    if PSE2OpSTACK_INC(OpCodes[0].OpCode).AType = btS64 then
    begin
      if OpCodes[1].OpCode.OpCode <> soDAT_SetInt then
         exit;

      if OpCodes[2].OpCode.OpCode <> soDAT_CONVERT then
         exit;

      if PSE2OpDAT_CONVERT(OpCodes[2].OpCode).NewType in [btU8, btS8, btU16, btS16, btU32, btS32] then
      begin
        PSE2OpSTACK_INC(OpCodes[0].OpCode).AType := PSE2OpDAT_CONVERT(OpCodes[2].OpCode).NewType;
        // Delete the convert opcode
        Method.OpCodes.Delete(i + 2);

        // Decrease every position
        AddOffsetPos(-1, i);

        i := i - 1;
      end else
      if PSE2OpDAT_CONVERT(OpCodes[2].OpCode).NewType in [btSingle, btDouble] then
      begin
        PSE2OpSTACK_INC(OpCodes[0].OpCode).AType    := PSE2OpDAT_CONVERT(OpCodes[2].OpCode).NewType;
        PSE2OpDAT_SetFloat(OpCodes[1].OpCode).Value := PSE2OpDAT_SetInt(OpCodes[1].OpCode).Value;
        OpCodes[1].OpCode.OpCode := soDAT_SetFloat;


        // Delete the convert opcode
        Method.OpCodes.Delete(i + 2);

        // Decrease every position
        AddOffsetPos(-1, i);

        i := i - 1;
      end;
    end else
    if PSE2OpSTACK_INC(OpCodes[0].OpCode).AType = btDouble then
    begin
      if OpCodes[1].OpCode.OpCode <> soDAT_SetFloat then
         exit;

      if OpCodes[2].OpCode.OpCode <> soDAT_CONVERT then
         exit;

      if PSE2OpDAT_CONVERT(OpCodes[2].OpCode).NewType in [btSingle, btDouble] then
      begin
        PSE2OpSTACK_INC(OpCodes[0].OpCode).AType := PSE2OpDAT_CONVERT(OpCodes[2].OpCode).NewType;
        // Delete the convert opcode
        Method.OpCodes.Delete(i + 2);

        // Decrease every position
        AddOffsetPos(-1, i);

        i := i - 1;
      end
    end;
  end;

  procedure InsertFastCompare;
  var src1, src2: integer;
  begin
    if OpCodes[0].OpCode.OpCode <> soDAT_COPY_FROM then
       exit;
    if OpCodes[1].OpCode.OpCode <> soDAT_COPY_FROM then
       exit;
    if OpCodes[2].OpCode.OpCode <> soOP_COMPARE then
       exit;
    if PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Static and PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Static then
        exit;

    if not PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Static and (PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Source = 0) then
       exit;
    if not PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Static and (PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Source = 0) then
       exit;

    if PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Static then
       OpCodes[0].CodeIndex := OpCodes[1].CodeIndex;

    if PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Static then
       src1 := 0
    else
    begin
      src1 := PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Source;
      {
      src1 := abs(PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Source);
      if PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Source < 0 then
         src1 := src1 or $8000000;
      }
    end;

    if PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Static then
       src2 := 0
    else
    begin
      src2 := PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Source;
      {
      src2 := abs(PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Source);
      if PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Source < 0 then
         src2 := src2 or $8000000;
      }
    end;

    OpCodes[0].OpCode.OpCode := soOP_FASTCOMPARE;
    PSE2OpOP_FASTCOMPARE(OpCodes[0].OpCode).CompType := PSE2OpOP_COMPARE(OpCodes[2].OpCode).CompType;
    PSE2OpOP_FASTCOMPARE(OpCodes[0].OpCode).Src1     := src2;
    PSE2OpOP_FASTCOMPARE(OpCodes[0].OpCode).Src2     := src1;
    {
    PSE2OpOP_FASTCOMPARE(OpCodes[0].OpCode).iSrc1 := src2 shl 4;
    PSE2OpOP_FASTCOMPARE(OpCodes[0].OpCode).iSrc2 := ((src2 and $F0) shl 24) or (src1 and $FFFFFFF);  //src1;
    }

    // Delete the convert opcode
    Method.OpCodes.Delete(i + 1);
    Method.OpCodes.Delete(i + 1);

    // Decrease every position
    AddOffsetPos(-2, i);
  end;

  procedure InsertFastOperation;
  var src1, src2: integer;
  begin
    if OpCodes[0].OpCode.OpCode <> soDAT_COPY_FROM then
       exit;
    if OpCodes[1].OpCode.OpCode <> soDAT_COPY_FROM then
       exit;
    if OpCodes[2].OpCode.OpCode <> soOP_OPERATION then
       exit;
    if PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Static and PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Static then
        exit;

    if not PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Static and (PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Source = 0) then
       exit;
    if not PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Static and (PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Source = 0) then
       exit;

    if PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Static then
       OpCodes[0].CodeIndex := OpCodes[1].CodeIndex;

    if PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Static then
       src1 := 0
    else
    begin
      src1 := PSE2OpDAT_COPY_FROM(OpCodes[0].OpCode).Source;
    end;

    if PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Static then
       src2 := 0
    else
    begin
      src2 := PSE2OpDAT_COPY_FROM(OpCodes[1].OpCode).Source;
    end;

    OpCodes[0].OpCode.OpCode := soOP_FASTOPERATION;
    PSE2OpOP_FASTOPERATION(OpCodes[0].OpCode).OpType   := PSE2OpOP_OPERATION(OpCodes[2].OpCode).OpType;
    PSE2OpOP_FASTOPERATION(OpCodes[0].OpCode).Src1     := src2;
    PSE2OpOP_FASTOPERATION(OpCodes[0].OpCode).Src2     := src1;

    // Delete the convert opcode
    Method.OpCodes.Delete(i + 1);
    Method.OpCodes.Delete(i + 1);

    // Decrease every position
    AddOffsetPos(-2, i);
  end;

  procedure ExchangePushConstant;
  begin
    if (OpCodes[0] = nil) or (OpCodes[1] = nil) then
       exit;

    if OpCodes[0].OpCode.OpCode <> soSTACK_INC then
       exit;

    case OpCodes[1].OpCode.OpCode of
    soDAT_SetInt   :
      begin
        if PSE2OpSTACK_INC(OpCodes[0].OpCode).AType = btS64 then
        begin
          OpCodes[0].OpCode.OpCode := soDAT_PUSHInt64;
          PSE2OpDAT_PUSHInt64(OpCodes[0].OpCode).Value := PSE2OpDAT_SetInt(OpCodes[1].OpCode).Value;
        end else
        if PSE2OpSTACK_INC(OpCodes[0].OpCode).AType = btU64 then
        begin
          OpCodes[0].OpCode.OpCode := soDAT_PUSHUInt64;
          PSE2OpDAT_PUSHUInt64(OpCodes[0].OpCode).Value := PSE2OpDAT_SetInt(OpCodes[1].OpCode).Value;
        end else
        begin
          OpCodes[0].OpCode.OpCode := soDAT_PUSHInt32;
          PSE2OpDAT_PUSHInt32(OpCodes[0].OpCode).Value := PSE2OpDAT_SetInt(OpCodes[1].OpCode).Value;
        end;
      end;
    soDAT_SetFloat :
      begin
        if PSE2OpSTACK_INC(OpCodes[0].OpCode).AType = btSingle then
        begin
          OpCodes[0].OpCode.OpCode := soDAT_PUSHFloat4;
          PSE2OpDAT_PUSHFloat4(OpCodes[0].OpCode).Value := PSE2OpDAT_SetFloat(OpCodes[1].OpCode).Value;
        end else
        begin
          OpCodes[0].OpCode.OpCode := soDAT_PUSHFloat8;
          PSE2OpDAT_PUSHFloat8(OpCodes[0].OpCode).Value := PSE2OpDAT_SetFloat(OpCodes[1].OpCode).Value;
        end;
      end;
    soDAT_SetPtr   :
      begin
        if PSE2OpSTACK_INC(OpCodes[0].OpCode).AType = btPointer then
        begin
          OpCodes[0].OpCode.OpCode := soDAT_PUSHPtr;
          PSE2OpDAT_PUSHPtr(OpCodes[0].OpCode).Value := PSE2OpDAT_SetPtr(OpCodes[1].OpCode).Value;
        end else
          exit;
      end;
    soDAT_LOADRES :
      begin
        if PSE2OpSTACK_INC(OpCodes[0].OpCode).AType in [btString, btPChar, btAnsiString, btPAnsiChar, btWideString, btPWideChar, btUTF8String] then
        begin
          OpCodes[0].OpCode.OpCode := soDAT_PUSHRES;
          OpCodes[0].CodeIndex := OpCodes[1].CodeIndex;
          PSE2OpDAT_PUSHRES(OpCodes[0].OpCode).Index := PSE2OpDAT_LoadRes(OpCodes[1].OpCode).Index;
        end else
          exit;
      end;
    else exit;
    end;
    
    Method.OpCodes.Delete(i + 1);
    AddOffsetPos(-1, i);
  end;

  procedure InsertJumpIfZero;
  begin
    if (OpCodes[0] = nil) or (OpCodes[1] = nil) or (OpCodes[2] = nil) then
       exit;

    if not ( OpCodes[0].OpCode.OpCode in
        [soDAT_PUSHInt32, soDAT_PUSHInt64, soDAT_PUSHUInt64, soDAT_PUSHFloat4,
         soDAT_PUSHFloat8, soDAT_PUSHPtr]) then
       exit;

    if OpCodes[1].OpCode.OpCode <> soOP_COMPARE then
       exit;

    if PSE2OpOP_COMPARE(OpCodes[1].OpCode).CompType <> 1 then
       exit;
       
    if OpCodes[2].OpCode.OpCode <> soFLOW_JIZ then
       exit;

    case OpCodes[0].OpCode.OpCode of
    soDAT_PUSHInt32  : if PSE2OpDAT_PUSHInt32(OpCodes[0].OpCode).Value <> 0 then exit;
    soDAT_PUSHInt64  : if PSE2OpDAT_PUSHInt64(OpCodes[0].OpCode).Value <> 0 then exit;
    soDAT_PUSHUInt64 : if PSE2OpDAT_PUSHUInt64(OpCodes[0].OpCode).Value <> 0 then exit;
    soDAT_PUSHFLOAT4 : if PSE2OpDAT_PUSHFloat4(OpCodes[0].OpCode).Value <> 0 then exit;
    soDAT_PUSHFLOAT8 : if PSE2OpDAT_PUSHFloat8(OpCodes[0].OpCode).Value <> 0 then exit;
    soDAT_PUSHPtr    : if PSE2OpDAT_PUSHPtr(OpCodes[0].OpCode).Value <> nil then exit;
    end;

    PSE2OpFLOW_JNZ(OpCodes[0].OpCode).OpCode := soFLOW_JNZ;
    PSE2OpFLOW_JNZ(OpCodes[0].OpCode).Position := PSE2OpFLOW_JIZ(OpCodes[2].OpCode).Position;
    OpCodes[0].CodeIndex := OpCodes[2].CodeIndex;

    Method.OpCodes.Delete(i + 1);  
    Method.OpCodes.Delete(i + 1);
    AddOffsetPos(-2, i);
  end;

  procedure CombineMultiplePops;
  var index, popCount : integer;

    function AnybodyJumpingToPosition(TargetPos: integer): boolean;
    var i: integer;
    begin
      result := True;
      for i:=0 to Method.OpCodes.Count-1 do
        if Method.OpCodes[i].GetJumpPos = TargetPos then
           exit;
      result := False;
    end;


  begin
    if OpCodes[0] = nil then
       exit
    else
    if OpCodes[0].OpCode^.OpCode <> soSTACK_DEC then
       exit;

    popCount := 0;
    for index:=0 to 10 do
    begin
        if AnybodyJumpingToPosition(i + index) then
        begin
          break;
        end;

      if OpCodes[index] = nil then
         break
      else
      if OpCodes[index].OpCode.OpCode = soSTACK_DEC then
         popCount := popCount + 1
      else
         break;
    end;

    if popCount < 2 then
       exit;

    OpCodes[0].OpCode.OpCode := soSTACK_DEC_COUNT;
    PSE2OpSTACK_DEC_COUNT(OpCodes[0].OpCode).Count := popCount;

    for index:=2 to popCount do
      Method.OpCodes.Delete(i + 1);

    AddOffsetPos(-(popCount - 1), i);
  end;

  procedure CombineMultiplePush;
  var index, pushCount : integer;
      aType            : integer;
  begin
    pushCount := 0;
    aType     := -1;
    for index:=0 to 10 do
    begin
      if OpCodes[index] = nil then
         break
      else
      if (OpCodes[index].OpCode.OpCode = soSTACK_INC) and
         (( PSE2OpSTACK_INC(OpCodes[index].OpCode).AType = aType ) or (aType = -1)) then
      begin
        pushCount := pushCount + 1;
        aType     := PSE2OpSTACK_INC(OpCodes[index].OpCode).AType;
      end else
        break;
    end;

    if pushCount < 2 then
       exit;

    OpCodes[0].OpCode.OpCode := soSTACK_INC_COUNT;
    PSE2OpSTACK_INC_COUNT(OpCodes[0].OpCode).Count := pushCount;
    PSE2OpSTACK_INC_COUNT(OpCodes[0].OpCode).AType := aType;

    for index:=2 to pushCount do
      Method.OpCodes.Delete(i + 1);

    AddOffsetPos(-(pushCount - 1), i);
  end;

  procedure RemoveNOOPs;
  begin
    if OpCodes[0].OpCode.OpCode = soNOOP then
    begin
      // Delete this opcode
      Method.OpCodes.Delete(i);

      // Decrease every position
      AddOffsetPos(-1, i);

      i := i - 1;
    end;
  end;

begin
  if Method = nil then
     exit;
  if (Method.OpCodes.Count = 0) or ((not Method.Used) and (not Method.IsExport)) then
     exit;

  if Method.IsExternal then
     exit;

  for Mode := 0 to 8 do
  begin
    i := 0;
    repeat
      for j:=0 to 10 do
        OpCodes[j] := Method.OpCodes[i + j];

      case Mode of
      0 : DeleteUselessPushPop;
      1 : InsertFastIncrement;
      2 : OptimizeIntValues;
      3 : InsertFastCompare;
      4 : InsertFastOperation;
      5 : RemoveNOOPs;
      6 : ExchangePushConstant;
      7 : InsertJumpIfZero;
      8 : CombineMultiplePops;
      //9 : CombineMultiplePush;
      end;

      inc(i);
    until i >= Method.OpCodes.Count;
  end;
end;

procedure TSE2Linker.OptimizeMethod2(Method: TSE2Method);
begin
  if Method = nil then
     exit;
  if (Method.OpCodes.Count = 0) or ((not Method.Used) and (not Method.IsExport)) or Method.IsExternal then
     exit;

  FOptimizer.Optimize(Method);
end;

procedure TSE2Linker.OptimizeMethods(AUnit: TSE2Unit);
var i: integer;
begin
  for i:=0 to AUnit.ElemList.Count-1 do
    if AUnit.ElemList[i] is TSE2Method then
       OptimizeMethod2(TSE2Method(AUnit.ElemList[i]));

  for i:=0 to AUnit.AInitialization.Count-1 do
    OptimizeMethod2(TSE2Method(AUnit.AInitialization[i]));
  for i:=0 to AUnit.AFinalization.Count-1 do
    OptimizeMethod2(TSE2Method(AUnit.AFinalization[i]));
  OptimizeMethod2(AUnit.Main);
end;

procedure TSE2Linker.PostLinkClass(PE: TSE2PE; aClass: TSE2MetaEntry);
var i: integer;
    p: TSE2MetaEntry;

  function ScanClass(aClass: TSE2MetaEntry; index: integer): integer;
  begin
    result := 0;
    if aClass = nil then
       exit;

    if aClass.DynMethods.Count <= index then
       result := 0
    else
    if aClass.DynMethods[index] <> 0 then
       result := aClass.DynMethods[index]
    else
    begin
      result := ScanClass(GetParentClass(PE, aClass), index);
      aClass.DynMethods[index] := index;
    end;
  end;

begin
  for i:=0 to aClass.DynMethods.Count-1 do
    if aClass.DynMethods[i] = 0 then
    begin
      p := GetParentClass(PE, aClass);
      aClass.DynMethods[i] := ScanClass(p, i);
    end;
end;

procedure TSE2Linker.PostLinkClasses(PE: TSE2PE);
var i: integer;
begin
  for i:=0 to PE.MetaData.Count-1 do
    if PE.MetaData[i].MetaType = mtClass then
      PostLinkClass(PE, PE.MetaData[i]);
end;

function TSE2Linker.GetParentClass(PE: TSE2PE;
  aClass: TSE2MetaEntry): TSE2MetaEntry;
var aInheritUnit, aInheritName : string;
    i    : integer;
begin
  result := nil;

  SE2SplitFullQualifiedName(string(aClass.ParamDecl), aInheritUnit, aInheritName);
  for i:=0 to PE.MetaData.Count-1 do
    if PE.MetaData[i].MetaType = mtClass then
      if PE.MetaData[i].AUnitName = aInheritUnit then
        if PE.MetaData[i].Name = aInheritName then
        begin
          result := PE.MetaData[i];
          exit;
        end;
end;

procedure TSE2Linker.PostLinkRecord(PE: TSE2PE; aRecord: TSE2MetaEntry; MetaIndex: integer);
var i: integer;
    p: TSE2MetaEntry;

  function ScanRecord(aRecord: TSE2MetaEntry; index: integer): integer;
  begin
    result := 0;
    if aRecord = nil then
       exit;

    if aRecord.DynMethods.Count <= index then
       result := 0
    else
    if aRecord.DynMethods[index] <> 0 then
       result := aRecord.DynMethods[index]
    else
    begin
      result := ScanRecord(GetParentRecord(PE, aRecord), index);
      aRecord.DynMethods[index] := index;
    end;
  end;

  procedure UpdateRecordRTTI(MetaIndex: integer; ARecord: integer);
  var i, m : integer;
      meta : TSE2MetaEntry;
      p    : PSE2RTTIEntry;
  begin
    for m:=0 to PE.MetaData.Count-1 do
    begin
      meta := PE.MetaData[m];
      if (meta.MetaType = mtRecord) or (meta.MetaType = mtClass) or (meta.MetaType = mtArray) then
      begin
        for i:=0 to meta.RTTI.Count-1 do
        begin
          p := meta.RTTI[i];
          if p^.AType = btRecord then
            if p^.Size = ARecord then
              meta.RTTI[i].Size := MetaIndex;
        end;
      end;
    end;
  end;

begin
  for i:=0 to aRecord.DynMethods.Count-1 do
    if aRecord.DynMethods[i] = 0 then
    begin
      p := GetParentRecord(PE, aRecord);
      aRecord.DynMethods[i] := ScanRecord(p, i);
    end;

  for i:=0 to PE.MetaData.Count-1 do
    UpdateRecordRTTI(MetaIndex, aRecord.CodePos);

  aRecord.CodePos := -1;
end;

procedure TSE2Linker.PostLinkRecords(PE: TSE2PE);
var i: integer;
begin
  for i:=0 to PE.MetaData.Count-1 do
    if PE.MetaData[i].MetaType = mtRecord then
      PostLinkRecord(PE, PE.MetaData[i], i);
end;

function TSE2Linker.GetParentRecord(PE: TSE2PE;
  aRecord: TSE2MetaEntry): TSE2MetaEntry;
var aInheritUnit, aInheritName : string;
    i    : integer;
begin
  result := nil;

  SE2SplitFullQualifiedName(string(aRecord.ParamDecl), aInheritUnit, aInheritName);

  for i:=0 to PE.MetaData.Count-1 do
    if PE.MetaData[i].MetaType = mtRecord then
      if PE.MetaData[i].AUnitName = aInheritUnit then
        if PE.MetaData[i].Name = aInheritName then
        begin
          result := PE.MetaData[i];
          exit;
        end;

end;

{$IFDEF SEII_SMART_LINKING}
procedure TSE2Linker.ProcessUsedMethods(BaseMethod: TSE2Method);
var i: integer;
begin
  if BaseMethod.UsageProcessed then
     exit;

  BaseMethod.Used := True;
  BaseMethod.UsageProcessed := True;

  for i:=0 to BaseMethod.UsedMethods.Count-1 do
    if BaseMethod.UsedMethods[i] is TSE2Method then
      ProcessUsedMethods(TSE2Method(BaseMethod.UsedMethods[i]));
end;

procedure TSE2Linker.ProcessUsage;

  procedure ProcessUnit(aUnit: TSE2Unit);
  var i: integer;
  begin
    for i:=aUnit.ElemList.Count-1 downto 0 do
      if aUnit.ElemList[i] is TSE2Method then
        if TSE2Method(aUnit.ElemList[i]).Used or TSE2Method(aUnit.ElemList[i]).IsExport then
          ProcessUsedMethods(TSE2Method(aUnit.ElemList[i]));

    if aUnit.AInitialization <> nil then
       for i:=0 to aUnit.AInitialization.Count-1 do
         if TSE2Method(aUnit.AInitialization[i]).OpCodes.Count > 1 then
          ProcessUsedMethods(TSE2Method(aUnit.AInitialization[i]));

    if aUnit.AFinalization <> nil then
       for i:=0 to aUnit.AFinalization.Count-1 do
         if TSE2Method(aUnit.AFinalization[i]).OpCodes.Count > 1 then
          ProcessUsedMethods(TSE2Method(aUnit.AFinalization[i]));

    if aUnit.Main <> nil then
       ProcessUsedMethods(aUnit.Main);
  end;

var i: integer;
begin
  for i:=0 to FUnitList.Count-1 do
    ProcessUnit(TSE2Unit(FUnitList[i]));
end;
{$ENDIF}

procedure TSE2Linker.LinkArray(PE: TSE2PE; AArray: TSE2Array);
var Meta   : TSE2MetaEntry;

  procedure LinkArrayRTTI(AArray: TSE2Array);
  var p: PSE2ClassRTTIEntry;
      i: integer;
  begin
    for i:=0 to AArray.RTTI.Count-1 do
    begin
      p := AArray.RTTI[i];
      Meta.RTTI.Add(p^.aType, p^.Offset, p^.Size);
    end;

    if AArray.InheritFrom is TSE2Array then
       LinkArrayRTTI(TSE2Array(AArray.InheritFrom));

  end;

begin
  Meta := TSE2MetaEntry.Create;
  Meta.MetaType := mtArray;
  Meta.Name     := AArray.Name;
  Meta.AUnitName := AArray.AUnitName;
  Meta.CodePos  := integer(AArray); //AClass.Variables;
  Meta.SourcePos  := AArray.DeclPos;
  Meta.SourceLine := AArray.DeclLine;
  if AArray.IsDynamic then
     Meta.ParamCount := -1
  else
     Meta.ParamCount := AArray.ArraySize;
  Meta.DynMethods.Count := 0; //ARecord.DynMethods;
  Meta.DynIndex := AArray.ElemSize;
  Meta.MetaLinkName := 'META_['+Meta.Name+']['+Meta.AUnitName+']';

  if AArray.InheritFrom is TSE2Array then
     Meta.ParamDecl := AnsiString(AArray.InheritFrom.AUnitName + '.' + AArray.InheritFrom.Name);

  LinkArrayRTTI(AArray);

  AArray.MetaIndex := PE.MetaData.Count;
  PE.MetaData.Add(Meta);
end;

procedure TSE2Linker.LinkArrays(PE: TSE2PE; AUnit: TSE2Unit);
var i       : integer;
    AArray  : TSE2Array;
begin
  for i:=0 to AUnit.TypeList.Count-1 do
  begin
    if AUnit.TypeList[i] is TSE2Array then
    begin
      AArray := TSE2Array(AUnit.TypeList[i]);
      LinkArray(PE, AArray);
    end;
  end;
end;

procedure TSE2Linker.PostLinkArray(PE: TSE2PE; aArray: TSE2MetaEntry;
  MetaIndex: integer);
var i: integer;

  procedure UpdateArrayRTTI(MetaIndex: integer; AArray: integer);
  var i, m : integer;
      meta : TSE2MetaEntry;
      p    : PSE2RTTIEntry;
  begin
    for m:=0 to PE.MetaData.Count-1 do
    begin
      meta := PE.MetaData[m];
      if (meta.MetaType = mtRecord) or (meta.MetaType = mtClass) or (meta.MetaType = mtArray) then
      begin
        for i:=0 to meta.RTTI.Count-1 do
        begin
          p := meta.RTTI[i];
          if p^.AType = btArray then
            if p^.Size = AArray then
              meta.RTTI[i].Size := MetaIndex;
        end;
      end;
    end;
  end;


begin
  for i:=0 to PE.MetaData.Count-1 do
    UpdateArrayRTTI(MetaIndex, AArray.CodePos);

  AArray.CodePos := -1;
end;

procedure TSE2Linker.PostLinkArrays(PE: TSE2PE);
var i: integer;
begin
  for i:=0 to PE.MetaData.Count-1 do
    if PE.MetaData[i].MetaType = mtArray then
      PostLinkArray(PE, PE.MetaData[i], i);
end;

end.
