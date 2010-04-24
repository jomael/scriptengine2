unit uSE2IncDiagnosticsDebug;

{$INCLUDE ScriptEngine.inc}

interface

uses
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;

implementation

const
  C_UnitName   = 'System.Diagnostics';
  C_UnitSource = 
        'unit System.Diagnostics;' + #13#10 + 
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  EAssertionFailed = class(EException)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  Debug = class' + #13#10 + 
        '  public' + #13#10 + 
        '    class procedure Assert(value: boolean); overload;' + #13#10 + 
        '    class procedure Assert(value: boolean; Message: string); overload;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'constructor EAssertionFailed.Create(const Message: string);' + #13#10 + 
        'begin' + #13#10 + 
        '  inherited;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class procedure Debug.Assert(value: boolean);' + #13#10 + 
        'begin' + #13#10 + 
        '  if not value then' + #13#10 + 
        '     raise EAssertionFailed.Create(''Assertion failed'');' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class procedure Debug.Assert(value: boolean; Message: string);' + #13#10 + 
        'begin' + #13#10 + 
        '  if not value then' + #13#10 + 
        '     raise EAssertionFailed.Create(Message);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'end.';

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
  end
end;

procedure RegisterUnit();
var p: TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

initialization
  RegisterUnit();

end.
