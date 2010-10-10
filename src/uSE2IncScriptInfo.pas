unit uSE2IncScriptInfo;

{$INCLUDE ScriptEngine.inc}

interface

uses
  SysUtils, uSE2BaseTypes, uSE2RunAccess, uSE2UnitManager;

implementation

uses
  Math, uSE2Consts;

const
  C_UnitName   = 'System';
  C_UnitSource =
     'unit System;'+#13#10+
     #13#10+
     'interface'+#13#10+
     #13#10+
     'type' + #13#10 +
     '  /// Stores some basic information about the compiler, the script was built with'+#13#10+
     '  ScriptInfo = class(TObject)'+#13#10+
     '  public' + #13#10 +
     '    /// The date and the time the current script was build'+#13#10+
     '    const BuiltDate       : TDateTime = {%SCRIPT_DATE%};'+#13#10+
     '    /// The used ScriptEngineII compiler version'+#13#10+
     '    const CompilerVersion : string = ''{%COMP_VERSION%}'';'+#13#10+
     '    /// The date the ScriptEngineII compiler was build'+#13#10+
     '    const CompilerDate    : TDateTime = {%COMP_DATE%};'+#13#10+
     '    /// Returns true if the compiler was an unicode compiler'+#13#10+
     '    const UnicodeCompiler = {%COMP_UNICODE%};'+#13#10+
     '  end; ' + #13#10 +
     #13#10 +
     'implementation'+#13#10+
     #13#10+
     'end.';

procedure Unit_GetSource(var Target: string);
var d: TDateTime;
    s1: string;
    decimalSep: Char;
begin
  Target := C_UnitSource;

  {$IFDEF DELPHIXEUP}
  decimalSep := TFormatSettings.Create.DecimalSeparator;
  {$ELSE}
  decimalSep := DecimalSeparator;
  {$ENDIF}

  d := Now();
  s1 := FloatToStrF(d, ffGeneral, 14, 7);
  s1 := StringReplace(s1, decimalSep, '.', []);
  Target := StringReplace(Target, '{%SCRIPT_DATE%}', s1, [rfReplaceAll]);

  d := TSE2ScriptEngineInfo.BuildDate;
  s1 := FloatToStrF(d, ffGeneral, 14, 7);
  s1 := StringReplace(s1, decimalSep, '.', []);
  Target := StringReplace(Target, '{%COMP_DATE%}', s1, [rfReplaceAll]);

  s1 := TSE2ScriptEngineInfo.Version;
  Target := StringReplace(Target, '{%COMP_VERSION%}', s1, [rfReplaceAll]);

  {$IFDEF Unicode}
  s1 := 'true';
  {$ELSE}
  s1 := 'false';
  {$ENDIF}
  Target := StringReplace(Target, '{%COMP_UNICODE%}', s1, [rfReplaceAll]);
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
  end;
end;

procedure RegisterUnit;
var p : TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;          
  p.Priority          := 1;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  p.Priority          := 20;
  TSE2UnitManager.RegisterUnit(p);
end;

initialization
  RegisterUnit;

end.
