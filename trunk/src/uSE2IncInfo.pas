unit uSE2IncInfo;

{$INCLUDE ScriptEngine.inc}

interface

uses
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;

implementation

{ YOU ARE NOT ALLOWED TO CHANGE AND/OR TO REMOVE THIS FILE }

const
  C_UnitName   = 'System';
  C_UnitSource =
      'unit System;'+#13#10+
      #13#10+
      'interface'+#13#10+
      #13#10+
      'type'+#13#10+
      '  /// Basic information about the current ScriptEngineII'+#13#10+
      '  ScriptEngineInfo = record'+#13#10+
      '  public'+#13#10+
      '    /// The current name of SEII'+#13#10+
      '    class function  Name      : string; external;'+#13#10+
      '    /// The author of SEII'+#13#10+
      '    class function  Author    : string; external;'+#13#10+
      '    /// The current running version of SEII'+#13#10+
      '    class function  Version   : string; external;'+#13#10+
      '    /// The build date of the current SEII'+#13#10+
      '    class function  BuildDate : TDateTime; external;'+#13#10+
      '    /// Additional information'+#13#10+
      '    class function  Copyright : string; external;'+#13#10+
      '  end;'+#13#10+
      #13#10+
      '  ScriptEngineAdditionalInfo = record'+#13#10+
      '  public'+#13#10+
      '    class function  Authors   : string; external;'+#13#10+
      '    class function  Revision  : string; external;'+#13#10+
      '    class function  BuildDate : TDateTime; external;'+#13#10+
      '    class function  Copyright : string; external;'+#13#10+
      '    class function  ChangeLog : string; external;'+#13#10+
      '  end;'+#13#10+
      #13#10+
      'implementation'+#13#10+
      #13#10+
      'end.';

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
    Target.Method['ScriptEngineInfo.Name[0]', C_UnitName] := @TSE2ScriptEngineInfo.Name;  
    Target.Method['ScriptEngineInfo.Author[0]', C_UnitName] := @TSE2ScriptEngineInfo.Author;
    Target.Method['ScriptEngineInfo.Version[0]', C_UnitName] := @TSE2ScriptEngineInfo.Version;
    Target.Method['ScriptEngineInfo.BuildDate[0]', C_UnitName] := @TSE2ScriptEngineInfo.BuildDate;
    Target.Method['ScriptEngineInfo.Copyright[0]', C_UnitName] := @TSE2ScriptEngineInfo.Copyright;

    Target.Method['ScriptEngineAdditionalInfo.Authors[0]', C_UnitName] := @TSE2ScriptEngineAdditionalInfo.Authors; 
    Target.Method['ScriptEngineAdditionalInfo.Revision[0]', C_UnitName] := @TSE2ScriptEngineAdditionalInfo.Revision;
    Target.Method['ScriptEngineAdditionalInfo.BuildDate[0]', C_UnitName] := @TSE2ScriptEngineAdditionalInfo.BuildDate;
    Target.Method['ScriptEngineAdditionalInfo.Copyright[0]', C_UnitName] := @TSE2ScriptEngineAdditionalInfo.Copyright;  
    Target.Method['ScriptEngineAdditionalInfo.ChangeLog[0]', C_UnitName] := @TSE2ScriptEngineAdditionalInfo.ChangeLog;
  end;
end;

procedure RegisterUnit;
var p : TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

initialization
  RegisterUnit;

end.
