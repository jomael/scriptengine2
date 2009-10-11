unit uSE2ScriptImporter;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2Consts, uSE2BaseTypes, uSE2Types;

type
  TSE2ScriptImporter = class(TObject)
  private
    FUnitSource      : string;
    FImportSource    : boolean;

    FOutput          : TStrings;
    FImportedMethods : TStrings;
  protected
    procedure FormatMethod(AUnit: TSE2Unit; AMethod: TSE2Method; var NativeMethod, ImportName: string; Target: TStrings); virtual; abstract;
    procedure FormatImport(index: integer; Target: TStrings); virtual; abstract;
    procedure ProcessMethod(AUnit: TSE2Unit; AMethod: TSE2Method);
    procedure ProcessUnit(AUnit: TSE2Unit);
    procedure MethodIsPropertyMethod(AUnit: TSE2Unit; AMethod: TSE2Method; out IsGetter, IsSetter: boolean; out PropertyName: string);
    function  MethodIsPropertyRead(AUnit: TSE2Unit; AMethod: TSE2Method; out PropertyName: string): boolean;
    function  MethodIsPropertyWrite(AUnit: TSE2Unit; AMethod: TSE2Method; out PropertyName: string): boolean;

    procedure AddHeader(AUnit: TSE2Unit); virtual; abstract;
    procedure AddInterface(AUnit: TSE2Unit); virtual; abstract;
    procedure AddImplementation(AUnit: TSE2Unit); virtual; abstract;
    procedure AddUnitSource(AUnit: TSE2Unit); virtual; abstract;
    procedure AddUnitImporterGenerator(AUnit: TSE2Unit); virtual; abstract;
    procedure AddEnd(AUnit: TSE2Unit); virtual; abstract;
    procedure AddImportMethod(AUnit: TSE2Unit); virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Import(AUnitList: TSE2BaseTypeList; const UnitName: string): boolean; overload;
    function Import(AUnit: TSE2Unit): boolean; overload;

    property Output       : TStrings read FOutput;
    property ImportSource : boolean  read FImportSource write FImportSource;
    property UnitSource   : string   read FUnitSource   write FUnitSource;
  end;

  TSE2ScriptImporterPascal = class(TSE2ScriptImporter)
  protected
    procedure AddEnd(AUnit: TSE2Unit); override;
    procedure AddHeader(AUnit: TSE2Unit);  override;
    procedure AddUnitSource(AUnit: TSE2Unit); override;
    procedure FormatMethod(AUnit: TSE2Unit; AMethod: TSE2Method; var NativeMethod: String;
      var ImportName: String; Target: TStrings); override;
    procedure AddImplementation(AUnit: TSE2Unit); override;
  end;

  TSE2ScriptImporterAppPascal = class(TSE2ScriptImporterPascal)
  protected
    procedure AddInterface(AUnit: TSE2Unit); override;
    procedure AddUnitImporterGenerator(AUnit: TSE2Unit); override;
    procedure FormatImport(index: Integer; Target: TStrings); override;
    procedure AddImportMethod(AUnit: TSE2Unit); override;
    procedure AddEnd(AUnit: TSE2Unit); override;
    procedure AddImplementation(AUnit: TSE2Unit); override;
  end;

  TSE2ScriptImporterPackagePascal = class(TSE2ScriptImporterPascal)    
  protected
    procedure AddInterface(AUnit: TSE2Unit); override;
    procedure FormatImport(index: Integer; Target: TStrings); override;
    procedure AddImportMethod(AUnit: TSE2Unit); override;               
    procedure AddUnitImporterGenerator(AUnit: TSE2Unit); override;
  end;

implementation

uses SysUtils;

type
  TMyStringObj = class
    Value: string;
    constructor Create(const s: string);
  end;  

{ TMyStringObj }

constructor TMyStringObj.Create(const s: string);
begin
  inherited Create;
  Value := s;
end;

{ TSE2ScriptImporter }

constructor TSE2ScriptImporter.Create;
begin
  inherited;
  FImportSource := True;
  FOutput := TStringList.Create;
  FImportedMethods := TStringList.Create;
end;

destructor TSE2ScriptImporter.Destroy;
var i: integer;
begin
  for i:=FImportedMethods.Count-1 downto 0 do
    FImportedMethods.Objects[i].Free;
  FImportedMethods.Free;
  FOutput.Free;
  inherited;
end;

function TSE2ScriptImporter.Import(AUnit: TSE2Unit): boolean;
begin
  result := True;

  AddHeader(AUnit);
  AddInterface(AUnit);
  AddImplementation(AUnit);
  
  ProcessUnit(AUnit);
  AddImportMethod(AUnit);
  AddUnitImporterGenerator(AUnit);
  AddEnd(AUnit);
end;

function TSE2ScriptImporter.Import(AUnitList: TSE2BaseTypeList;
  const UnitName: string): boolean;
var i     : integer;
begin
  for i:=0 to AUnitList.Count-1 do
    if SameText(AUnitList[i].Name, UnitName) then
    begin
      result := Import(TSE2Unit(AUnitList[i]));
      exit;
    end;
  result := False;
end;

procedure TSE2ScriptImporter.MethodIsPropertyMethod(AUnit: TSE2Unit;
  AMethod: TSE2Method; out IsGetter, IsSetter: boolean; out PropertyName: string);
var i: integer;
begin
  IsGetter := False;
  IsSetter := False;
  PropertyName := '';

  for i:=AUnit.ElemList.Count-1 downto 0 do
    if AUnit.ElemList[i] is TSE2Property then
    begin
      if TSE2Property(AUnit.ElemList[i]).Getter = AMethod then
      begin
        IsGetter := True;
        PropertyName := AUnit.ElemList[i].Name;
      end else
      if TSE2Property(AUnit.ElemList[i]).Setter = AMethod then
      begin
         IsSetter := True;
         PropertyName := AUnit.ElemList[i].Name;
      end;

      if IsGetter and IsSetter then
         exit;
    end;

end;

function TSE2ScriptImporter.MethodIsPropertyRead(AUnit: TSE2Unit;
  AMethod: TSE2Method; out PropertyName: string): boolean;
var tmp: boolean;
begin
  MethodIsPropertyMethod(AUnit, AMethod, result, tmp, PropertyName);
end;

function TSE2ScriptImporter.MethodIsPropertyWrite(AUnit: TSE2Unit;
  AMethod: TSE2Method; out PropertyName: string): boolean;
var tmp: boolean;
begin
  MethodIsPropertyMethod(AUnit, AMethod, tmp, result, PropertyName);
end;

procedure TSE2ScriptImporter.ProcessMethod(AUnit: TSE2Unit; AMethod: TSE2Method);
var Native, Import: string;
begin
  FormatMethod(AUnit, AMethod, Native, Import, Output);
  FImportedMethods.AddObject(Import, TMyStringObj.Create(Native));
end;

procedure TSE2ScriptImporter.ProcessUnit(AUnit: TSE2Unit);
var i: integer;
begin
  for i:=0 to AUnit.ElemList.Count-1 do
    if AUnit.ElemList[i] is TSE2Method then
      if TSE2Method(AUnit.ElemList[i]).IsExternal then
        ProcessMethod(AUnit, TSE2Method(AUnit.ElemList[i]));
end;

{ TSE2ScriptImporterPascal }

procedure TSE2ScriptImporterPascal.AddEnd(AUnit: TSE2Unit);
begin
  inherited;
  FOutput.Add('end.');
end;

procedure TSE2ScriptImporterPascal.AddHeader(AUnit: TSE2Unit);
begin
  inherited;
  FOutput.Add('unit [ImportUnitName];');
  FOutput.Add('');
end;   

procedure TSE2ScriptImporterPascal.AddImplementation(AUnit: TSE2Unit);
begin
  inherited;
  Output.Add('implementation');
  Output.Add('');
end;

procedure TSE2ScriptImporterPascal.FormatMethod(AUnit: TSE2Unit; AMethod: TSE2Method;
  var NativeMethod, ImportName: String; Target: TStrings);
var s         : string;
    TypeName  : string;
    i         : integer;
    Start     : integer;
    Stop      : integer;
    PrevParam : TSE2Parameter;
    Param     : TSE2Parameter;
    NextParam : TSE2Parameter;
    bIsGetter : boolean;
    bIsSetter : boolean;
    PropertyName : string;

  function NextParamEqual: boolean;
  begin
    result := False;
    if NextParam = nil then
       exit;

    if NextParam.ParameterType = Param.ParameterType then
      if NextParam.AType = Param.AType then
        result := True;
  end;

  function PrevParamEqual: boolean;
  begin
    result := False;
    if PrevParam = nil then
       exit;

    if PrevParam.ParameterType = Param.ParameterType then
      if PrevParam.AType = Param.AType then
        result := True;
  end;

begin
  inherited;

  MethodIsPropertyMethod(AUnit, AMethod, bIsGetter, bIsSetter, PropertyName);


  ImportName := AMethod.Name;
  if AMethod.Parent <> nil then
     ImportName := AMethod.Parent.Name + '.' + ImportName;
  ImportName := ImportName + '['+TSE2Converter.IntToStr(AMethod.ID)+']';

  NativeMethod := '_' + AMethod.Name;
  if AMethod.Parent <> nil then
     NativeMethod := AMethod.Parent.Name + NativeMethod;

  if AMethod.ID > 0 then
     NativeMethod := NativeMethod + TSE2Converter.IntToStr(AMethod.ID);

  if AMethod.ReturnValue <> nil then
     s := 'function '
  else
     s := 'procedure ';

  s := s + NativeMethod;
  if AMethod.Params.Count > 0 then
  begin
    s := s + '(';

    PrevParam := nil;
    for i:=0 to AMethod.Params.Count-1 do
    begin
      Param     := TSE2Parameter(AMethod.Params[i]);
      NextParam := TSE2Parameter(AMethod.Params[i + 1]);

      if not PrevParamEqual then
      begin
        case Param.ParameterType of
        ptDefault : ;
        ptConst   : s := s + 'const ';
        ptVar     : s := s + 'var ';
        end;
      end;

      s := s + Param.Name;
      if NextParamEqual then
         s := s + ', '
      else
      begin
        TypeName := Param.AType.Name;
        s := s + ': ' + TypeName;
        if i < AMethod.Params.Count - 1 then
           s := s + '; ';
      end;

      PrevParam := Param;
    end;

    s := s + ')';
  end;

  if AMethod.ReturnValue <> nil then
     s := s + ': ' + AMethod.ReturnValue.AType.Name;
  s := s + ';';

  case AMethod.CallConvention of
  callRegister : ;
  callStdcall  : s := s + ' stdcall;';
  callCdecl    : s := s + ' cdecl;';
  callPascal   : s := s + ' pascal;';
  callSafecall : s := s + ' safecall;';
  end;

  Output.Add(s);
  Output.Add('begin');

  s := '  ';
  if AMethod.ReturnValue <> nil then
     s := s + 'result := ';

  if AMethod.Parent <> nil then
  begin
    if AMethod.MethodType = mtConstructor then
       s := s + AMethod.Parent.Name + '.'
    else
    if AMethod.IsStatic then
       s := s + AMethod.Parent.Name + '.'
    else
       s := s + 'Self.';
  end;

  if bIsGetter or bIsSetter then
    s := s + PropertyName
  else
    s := s + AMethod.Name;

  Start := 0;
  if AMethod.Parent <> nil then
  begin
    Start := 1;
  end;

  if bIsSetter then
     Stop := AMethod.Params.Count-2
  else
     Stop := AMethod.Params.Count-1;

  if (AMethod.Params.Count > Start) and (Start <= Stop) then
  begin
    if bIsGetter or bIsSetter then
      s := s + '['
    else
      s := s + '(';

    for i:=Start to Stop do
    begin
      TypeName := AMethod.Params[i].Name;
      s := s + TypeName;
      if i < Stop then
         s := s + ', ';
    end;

    if bIsGetter or bIsSetter then
    begin
      s := s + ']';
    end else
      s := s + ')';
  end;

  if bIsSetter then
  begin
    TypeName := AMethod.Params[AMethod.Params.Count-1].Name;
    s := s + ' := ' + TypeName;
  end;

  s := s + ';';
  Output.Add(s);
  Output.Add('end;');
  Output.Add('');
end;

procedure TSE2ScriptImporterPascal.AddUnitSource(AUnit: TSE2Unit);
var aSource : TStringList;
    i       : integer;
    s       : string;
begin
  inherited;

  aSource := TStringList.Create;
  try
    Output.Add('const');
    Output.Add('  C_UnitName   = ''' + AUnit.Name + ''';');
    Output.Add('  C_UnitSource = ');

    aSource.Text := FUnitSource;
    for i:=0 to aSource.Count-1 do
    begin
      if Trim(aSource[i]) = '' then
         s := '        '
      else
         s := '        '#39 + StringReplace(TrimRight(aSource[i]), #39, #39#39, [rfReplaceAll]) + #39;

      if i < aSource.Count - 1 then
      begin
        if Trim(s) <> '' then
           s := s + ' + #13#10 + '
        else
           s := s + '#13#10 + ';
      end else
         s := s + ';';

      Output.Add(s);
    end;
  finally
    aSource.Free;
  end;
  Output.Add('');
end;

{ TSE2ScriptImporterAppPascal }

procedure TSE2ScriptImporterAppPascal.FormatImport(index: Integer;
  Target: TStrings);
var s: string;
begin
  inherited;

  s := '    Target.Method['#39 + FImportedMethods[index] + #39', C_UnitName] := @' + TMyStringObj(FImportedMethods.Objects[index]).Value + ';';
  Output.Add(s);
end;

procedure TSE2ScriptImporterAppPascal.AddImportMethod(AUnit: TSE2Unit);
var i: integer;
begin
  inherited;

  Output.Add('procedure Unit_GetSource(var Target: string);');
  Output.Add('begin');
  Output.Add('  Target := C_UnitSource;');
  Output.Add('end;');
  Output.Add('');

  Output.Add('procedure Unit_RegisterMethods(const Target: TSE2RunAccess);');
  Output.Add('begin');
  Output.Add('  if Target.HasUnit(C_UnitName) then');
  Output.Add('  begin');

  for i:=0 to FImportedMethods.Count-1 do
    FormatImport(i, Output);

  Output.Add('  end');
  Output.Add('end;');
  Output.Add('');
end;

procedure TSE2ScriptImporterAppPascal.AddInterface(AUnit: TSE2Unit);
begin
  inherited;
  Output.Add('interface');
  Output.Add('');
  Output.Add('uses');
  Output.Add('  uSE2RunAccess, uSE2UnitManager, uSE2Consts;');
  Output.Add('');
end;

procedure TSE2ScriptImporterAppPascal.AddUnitImporterGenerator(
  AUnit: TSE2Unit);
begin
  inherited;

  Output.Add('procedure RegisterUnit();');
  Output.Add('var p: TSE2MethodUnit;');
  Output.Add('begin');
  Output.Add('  p := TSE2MethodUnit.Create;');
  Output.Add('  p.DoRegisterMethods := Unit_RegisterMethods;');
  Output.Add('  p.DoGetUnitSource   := Unit_GetSource;');
  Output.Add('  p.UnitName          := C_UnitName;');
  Output.Add('  TSE2UnitManager.RegisterUnit(p);');
  Output.Add('end;');
  Output.Add('');
end;

procedure TSE2ScriptImporterAppPascal.AddEnd(AUnit: TSE2Unit);
begin
  Output.Add('initialization');
  Output.Add('  RegisterUnit();');
  Output.Add('');
  inherited;
end;

procedure TSE2ScriptImporterAppPascal.AddImplementation(AUnit: TSE2Unit);
begin
  inherited;
  if FImportSource then
     AddUnitSource(AUnit);
end;

{ TSE2ScriptImporterPackagePascal }

procedure TSE2ScriptImporterPackagePascal.AddImportMethod(AUnit: TSE2Unit);
var i: integer;
begin
  inherited;

  Output.Add('procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);');
  Output.Add('begin');

  for i:=0 to FImportedMethods.Count-1 do
    FormatImport(i, Output);

  Output.Add('end;');
  Output.Add('');
end;

procedure TSE2ScriptImporterPackagePascal.AddInterface(AUnit: TSE2Unit);
begin
  inherited;

  Output.Add('interface');
  Output.Add('');
  Output.Add('uses');
  Output.Add('  uSE2PackageAPI;');
  Output.Add('');
  if FImportSource then
     AddUnitSource(AUnit);
  Output.Add('procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);');
  Output.Add('');
end;

procedure TSE2ScriptImporterPackagePascal.AddUnitImporterGenerator(
  AUnit: TSE2Unit);
begin
  inherited;
end;

procedure TSE2ScriptImporterPackagePascal.FormatImport(index: Integer;
  Target: TStrings);
var s: string;                        
begin
  inherited;

  s := '  CallBack(Module, Data, @'+TMyStringObj(FImportedMethods.Objects[index]).Value + ', '#39 + FImportedMethods[index] + #39');';
  Output.Add(s);
end;

end.
