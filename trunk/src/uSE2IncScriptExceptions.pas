unit uSE2IncScriptExceptions;

{$INCLUDE ScriptEngine.inc}

interface

uses
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;

implementation

uses
  SysUtils;

const
  C_UnitName   = 'System';
  C_UnitSource = 
        'unit System;' + #13#10 +
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 +
        '  /// The base class for every exception'+#13#10+
        '  EException = class(TObject)' + #13#10 +
        '  private' + #13#10 +
        '    FMessage    : string;' + #13#10 +
        '    FCallStack  : string;' + #13#10 +
        '  public' + #13#10 +
        '    /// Creates a new exception class instance'+#13#10+
        '    constructor Create(const Message: string); virtual;' + #13#10 +
        #13#10 +
        '    /// Returns a string with every information of the exception'+#13#10+
        '    function ToString: string; override;' + #13#10 +
        '    procedure Assign(Source: EException); virtual;'+#13#10+
        #13#10 +
        '    /// The message of the exception'+#13#10+
        '    property Message   : string read FMessage write FMessage;' + #13#10 +
        '    /// The call stack, the exception was thrown'+#13#10+
        '    property CallStack : string read FCallStack;' + #13#10 + 
        '  end;' + #13#10 +
        #13#10+
        '  /// The base exception for every external exceptions'+#13#10+
        '  '+C_SE2ExceptExternal+' = class(EException)' + #13#10 +
        '  public' + #13#10 +
        '    constructor Create(const Message: string); override;' + #13#10 +
        '  end;' + #13#10 +
        #13#10 +
        '  /// This exception is thrown if an unregistered exception occured'+#13#10+
        '  '+C_SE2ExceptUnknown+' = sealed class('+C_SE2ExceptExternal+')'+#13#10+
        '  private'+#13#10+
        '    FExceptionClassName : string;'+#13#10+
        '  public' + #13#10 +
        '    constructor Create(const Message: string); override;' + #13#10 +
        #13#10 +
        '    function ToString: string; override;'+#13#10+
        '    /// The class name of the unknown exception'+#13#10+
        '    property ExceptionClassName: string read FExceptionClassName;'+#13#10+
        '  end;' + #13#10 +
        #13#10 +
        'implementation' + #13#10 + 
        #13#10 + 
        'constructor EException.Create(const Message: string);' + #13#10 + 
        'begin' + #13#10 + 
        '  inherited Create;' + #13#10 + 
        '  Self.FMessage   := Message;' + #13#10 + 
        'end;' + #13#10 +
        #13#10 +
        'function EException.ToString: string;' + #13#10 +
        'begin' + #13#10 +
        '  result := ''['' + Self.ClassName + ''] '' + Self.FMessage + '' '' + #13#10 + Self.FCallStack;' + #13#10 +
        'end;' + #13#10 +
        #13#10 +
        'procedure EException.Assign(Source: EException);'+#13#10+
        'begin'+#13#10+
        '  if Source is EException then'+#13#10+
        '  begin'+#13#10+
        '    Self.FMessage   := Source.FMessage;'+#13#10+
        '    Self.FCallStack := Source.FCallStack;'+#13#10+
        '  end;'+#13#10+
        'end;'+#13#10+
        #13#10 +
        'constructor '+C_SE2ExceptExternal+'.Create(const Message: string);' + #13#10 +
        'begin' + #13#10 +
        '  inherited;' + #13#10 +
        'end;' + #13#10 +  
        #13#10 +
        'constructor '+C_SE2ExceptUnknown+'.Create(const Message: string);' + #13#10 +
        'begin' + #13#10 +
        '  inherited;' + #13#10 +
        'end;' + #13#10 +
        #13#10 +
        'function '+C_SE2ExceptUnknown+'.ToString: string;' + #13#10 +
        'begin' + #13#10 +
        '  result := ''['' + Self.ClassName + ''] '' + Self.ExceptionClassName + '': '' + Self.FMessage + '' '' + #13#10 + Self.FCallStack;' + #13#10 +
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

procedure Unit_RegisterExceptions(const Target: TSE2RunAccess);
begin
  Target.Exceptions.Add(TObject, Target.FindClass(C_SE2ExceptExternal, C_UnitName) );
  Target.Exceptions.Add(EExternalException, Target.FindClass(C_SE2ExceptExternal,  C_UnitName) );
  Target.Exceptions.Add(TObject, Target.FindClass(C_SE2ExceptUnknown, C_UnitName));
end;

procedure RegisterUnit();
var p: TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  p.DoRegExceptions   := Unit_RegisterExceptions;
  p.Priority          := 1;
  TSE2UnitManager.RegisterUnit(p);
end;

initialization
  RegisterUnit();

end.
