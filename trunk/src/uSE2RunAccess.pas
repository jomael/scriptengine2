unit uSE2RunAccess;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2RunType, uSE2Consts, uSE2BaseTypes, uSE2OpCode, uSE2PEData;

type
  TSE2RunAccess = class(TSE2Object)
  private
    FPEData : TSE2PE;
    FTmpObj : TObject;
  protected
    function  GetMetaEntry(const Name, UnitName: string): TSE2MetaEntry; overload;
    function  GetMetaEntry(const Name, UnitName: string; MetaType: TSE2MetaType): TSE2MetaEntry; overload;

    function  GetMethod(MethodName, UnitName: string): Pointer;
    procedure SetMethod(MethodName, UnitName: string; const MethodPos: Pointer);

    function  GetMethodName(index: integer): string;
    function  GetCount: integer;
  public
    constructor Create(PEData: TSE2PE); reintroduce;
    destructor Destroy; override;

    property TmpObj : TObject read FTmpObj write FTmpObj;
    function MethodMatches(index: integer; 
                        const ParamModes: array of TSE2ParamMode;
                        const ParamTypes: array of TSE2TypeIdent): boolean;
    function FindMethod(const MethodName, UnitName: string;
                        const ParamModes: array of TSE2ParamMode;
                        const ParamTypes: array of TSE2TypeIdent): Pointer;
    function FindClass(const ClassName, UnitName: string): TSE2MetaEntry;

    function IsChildOfClass(BaseClass, Child: TSE2MetaEntry; CanBeEqualClasses: boolean): boolean;
    function FindParentClass(Child: TSE2MetaEntry): TSE2MetaEntry;

    function HasUnit(const UnitName: string): boolean;
    property Method[MethodName, UnitName: string]: Pointer read GetMethod  write SetMethod;

    property MethodNames[index: integer]: string  read GetMethodName;
    property Count                      : integer read GetCount;
  end;

implementation

uses SysUtils;

function NoIndex(const s: string): string;
begin
  result := s;
  if Pos('[', s) > 0 then
     result := Copy(result, 1, Pos('[', result) - 1);
end;

{ TSE2RunAccess }

constructor TSE2RunAccess.Create(PEData: TSE2PE);
begin
  if PEData = nil then
     raise ESE2NullReferenceError.Create('PEData must not be nil');

  inherited Create;
  FPEData := PEData;
end;

destructor TSE2RunAccess.Destroy;
begin
  inherited;
end;

function TSE2RunAccess.FindClass(const ClassName,
  UnitName: string): TSE2MetaEntry;
var i: integer;
begin
  result := nil;
  for i:=FPEData.MetaData.Count-1 downto 0 do
    if FPEData.MetaData[i].MetaType = mtClass then
      if (UnitName = '') or StringIdentical(FPEData.MetaData[i].AUnitName, UnitName) then
        if StringIdentical(NoIndex(FPEData.MetaData[i].Name), ClassName) then
        begin
           result := FPEData.MetaData[i];
           exit;
        end;
end;

function TSE2RunAccess.FindMethod(const MethodName, UnitName: string;
  const ParamModes: array of TSE2ParamMode;
  const ParamTypes: array of TSE2TypeIdent): Pointer;
var i: integer;
begin
  result := nil;
  if length(ParamModes) <> length(ParamTypes) then
     exit;
  for i:=FPEData.MetaData.Count-1 downto 0 do
    if FPEData.MetaData[i].MetaType = mtMethod then
      if (UnitName = '') or StringIdentical(FPEData.MetaData[i].AUnitName, UnitName) then
        if StringIdentical(NoIndex(FPEData.MetaData[i].Name), MethodName) then
          if MethodMatches(i, ParamModes, ParamTypes) then
          begin
            result := FPEData.MetaData[i];
            exit;
          end;
end;

function TSE2RunAccess.FindParentClass(
  Child: TSE2MetaEntry): TSE2MetaEntry;
var sClass, sUnit : string;
begin
  result := nil;
  if Child.MetaType <> mtClass then
     exit;

  SE2SplitFullQualifiedName(string(Child.ParamDecl), sUnit, sClass);
  if sUnit = '' then
     exit;

  result := FindClass(sClass, sUnit);
end;

function TSE2RunAccess.GetCount: integer;
begin
  result := FPEData.MetaData.Count;
end;

function TSE2RunAccess.GetMetaEntry(const Name,
  UnitName: string): TSE2MetaEntry;
var i: integer;
begin
  for i:=FPEData.MetaData.Count-1 downto 0 do
    if StringIdentical(FPEData.MetaData[i].Name, Name) then
      if StringIdentical(FPEData.MetaData[i].AUnitName, UnitName) then
      begin
        result := FPEData.MetaData[i];
        exit;
      end;
  result := nil;
end;

function TSE2RunAccess.GetMetaEntry(const Name, UnitName: string;
  MetaType: TSE2MetaType): TSE2MetaEntry;
var i: integer;
begin
  for i:=FPEData.MetaData.Count-1 downto 0 do     
    if FPEData.MetaData[i].MetaType = MetaType then
      if StringIdentical(FPEData.MetaData[i].Name, Name) then
        if StringIdentical(FPEData.MetaData[i].AUnitName, UnitName) then
        begin
          result := FPEData.MetaData[i];
          exit;
        end;
  result := nil;
end;

function TSE2RunAccess.GetMethod(MethodName, UnitName: string): Pointer;
var MetaEntry : TSE2MetaEntry;
    OpCode    : PSE2OpDefault;
begin
  result    := nil;
  MetaEntry := GetMetaEntry(MethodName, UnitName);
  if MetaEntry = nil then
     exit;

  if MetaEntry.IsExternal then
  begin
    OpCode := FPEData.OpCodes[MetaEntry.CodePos];
    if OpCode = nil then
       exit;

    if OpCode.OpCode <> soFLOW_CALLEX then
       exit;

    result := Pointer(PSE2OpFLOW_CALLEX(OpCode)^.Position);
  end;
end;

function TSE2RunAccess.GetMethodName(index: integer): string;
var Meta: TSE2MetaEntry;
begin
  result := '';
  Meta := FPEData.MetaData.Items[index];
  if Meta <> nil then
     result := Meta.AUnitName + '.' + NoIndex(Meta.Name);
end;

function TSE2RunAccess.HasUnit(const UnitName: string): boolean;
var i: integer;
begin
  for i:=FPEData.MetaData.Count-1 downto 0 do
  begin
    if AnsiSameText(FPEData.MetaData[i].AUnitName, UnitName) then
    begin
      result := True;
      exit;
    end;
  end;

  result := False; // True;
end;

function TSE2RunAccess.IsChildOfClass(BaseClass,
  Child: TSE2MetaEntry; CanBeEqualClasses: boolean): boolean;
var p             : TSE2MetaEntry;
begin
  result := False;
  if (BaseClass.MetaType <> mtClass) or (Child.MetaType <> mtClass) then
     exit;

  if BaseClass = Child then
  begin
    result := CanBeEqualClasses;
    exit;
  end;

  p := FindParentClass(Child);
  while p <> nil do
  begin
    if BaseClass = p then
    begin
      result := True;
      exit;
    end;
    p := FindParentClass(p);
  end;
end;

function TSE2RunAccess.MethodMatches(index: integer;
  const ParamModes: array of TSE2ParamMode;
  const ParamTypes: array of TSE2TypeIdent): boolean;

  function ParamsOk(const Data: TSE2MetaEntry): boolean;
  var givenParams  : integer;
      giveResult   : boolean;
      i            : integer;
      declareParam : byte;
  begin
    result := False;
    givenParams := length(ParamModes);
    giveResult  := False;
    if givenParams > 0 then
      if ParamModes[High(ParamModes)] = pmResult then
      begin
        giveResult := True;
        givenParams := givenParams - 1;
      end;

    if givenParams <> Data.ParamCount then
       exit;

    if giveResult <> Data.HasResult then
       exit;

    if Data.HasSelf then
      if givenParams < 1 then
        exit;

    if givenParams > 0 then
    begin
      for i:=0 to Data.ParamCount-1 do
      begin
        declareParam := Ord(Data.ParamDecl[i+1]);
        if TSE2ParamHelper.GetParamType(declareParam) <> ParamTypes[i] then
           exit;

        case ParamModes[i] of
        pmIn :
            if TSE2ParamHelper.IsVarParam(declareParam) then
               exit;
        pmInOut :
            if not TSE2ParamHelper.IsVarParam(declareParam) then
               exit;
        end;
      end;
    end;

    if giveResult then
       if TSE2ParamHelper.GetParamType(Data.ResultType) <> ParamTypes[High(ParamTypes)] then
         exit;

    result := True;
  end;

var Meta: TSE2MetaEntry;
begin
  result := False;

  Meta := FPEData.MetaData[index];
  if Meta <> nil then
     result := ParamsOk(Meta);



end;

procedure TSE2RunAccess.SetMethod(MethodName, UnitName: string;
  const MethodPos: Pointer);
var MetaEntry : TSE2MetaEntry;
    OpCode    : PSE2OpDefault;
begin
  MetaEntry := GetMetaEntry(MethodName, UnitName);
  if MetaEntry = nil then
     exit;

  if MetaEntry.IsExternal then
  begin
    OpCode := FPEData.OpCodes[MetaEntry.CodePos];
    if OpCode = nil then
       exit;

    if OpCode.OpCode <> soFLOW_CALLEX then
       exit;

    PSE2OpFLOW_CALLEX(OpCode)^.Position := cardinal(MethodPos);
  end;
end;

end.
