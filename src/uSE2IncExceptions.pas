unit uSE2IncExceptions;

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
        '  EOutOfMemory = class(EExternalException)' + #13#10 +
        '  public'+#13#10+
        '    constructor Create(const Message: string); override;' + #13#10+
        '  end;'+#13#10+
        #13#10+
        '  EIntError = class(EExternalException)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EDivisionByZero = class(EIntError)' + #13#10 +
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  ERangeError = class(EIntError)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EIntOverflow = class(EIntError)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EMathError = class(EExternalException)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EInvalidOp = class(EMathError)' + #13#10 +
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EZeroDivision = class(EMathError)' + #13#10 +
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EOverflow = class(EMathError)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EUnderflow = class(EMathError)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EAbort = class(EException)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EInvalidPointer = class(EExternalException)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EConvertError = class(EExternalException)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  EAccessViolation = class(EConvertError)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  ENullReference = class(EExternalException)' + #13#10 +
        '  public' + #13#10 +
        '    constructor Create(const Message: string); override;' + #13#10 +
        '  end;' + #13#10 +
        #13#10 +
        '  EMethodNotAssigned = class(ENullReference)' + #13#10 +
        '  public' + #13#10 +
        '    constructor Create(const Message: string); override;' + #13#10 +
        '  end;' + #13#10 +   
        #13#10 +
        '  EStackOverflow = class(EExternalException)' + #13#10 +
        '  public' + #13#10 +
        '    constructor Create(const Message: string); override;' + #13#10 +
        '  end;' + #13#10 +
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 +
        'constructor EOutOfMemory.Create(const Message: string);'+#13#10+
        'begin inherited; end;' + #13#10 +
        #13#10 +
        'constructor EIntError.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EDivisionByZero.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor ERangeError.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EIntOverflow.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EMathError.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EInvalidOp.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EZeroDivision.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EOverflow.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EUnderflow.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EAbort.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EInvalidPointer.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EConvertError.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 + 
        'constructor EAccessViolation.Create(const Message: string);' + #13#10 + 
        'begin inherited; end;' + #13#10 + 
        #13#10 +
        'constructor ENullReference.Create(const Message: string);' + #13#10 +
        'begin inherited; end;' + #13#10 +
        #13#10+
        'constructor EMethodNotAssigned.Create(const Message: string);' + #13#10 +
        'begin inherited; end;' + #13#10 +   
        #13#10+
        'constructor EStackOverflow.Create(const Message: string);' + #13#10 +
        'begin inherited; end;' + #13#10 +
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
const
  C_SE2ExceptEOutOfMemory        = 'EOutOfMemory';
  C_SE2ExceptEIntError           = 'EIntError';
  C_SE2ExceptEDivByZero          = 'EDivisionByZero';
  C_SE2ExceptERangeError         = 'ERangeError';
  C_SE2ExceptEIntOverflow        = 'EIntOverflow';
  C_SE2ExceptEMathError          = 'EMathError';
  C_SE2ExceptEInvalidOp          = 'EInvalidOp';

  C_SE2ExceptEZeroDivide         = 'EZeroDivision';
  C_SE2ExceptEOverflow           = 'EOverflow';
  C_SE2ExceptEUnderflow          = 'EUnderflow';
  C_SE2ExceptEAbort              = 'EAbort';
  C_SE2ExceptEInvalidPointer     = 'EInvalidPointer';
  C_SE2ExceptEConvertError       = 'EConvertError';
  C_SE2ExceptEAccessViolation    = 'EAccessViolation';
  C_SE2ExceptENullReference      = 'ENullReference';
  C_SE2ExceptEMethodNotAssigned  = 'EMethodNotAssigned';
  C_SE2ExceotEStackOverflow      = 'EStackOverflow';
begin
  Target.Exceptions.Add(EOutOfMemory, Target.FindClass(C_SE2ExceptEOutOfMemory, C_UnitName));

  Target.Exceptions.Add(EIntError, Target.FindClass(C_SE2ExceptEIntError, C_UnitName));
  Target.Exceptions.Add(EDivByZero, Target.FindClass(C_SE2ExceptEDivByZero, C_UnitName));
  Target.Exceptions.Add(ERangeError, Target.FindClass(C_SE2ExceptERangeError, C_UnitName));
  Target.Exceptions.Add(EIntOverflow, Target.FindClass(C_SE2ExceptEIntOverflow, C_UnitName));

  Target.Exceptions.Add(EMathError, Target.FindClass(C_SE2ExceptEMathError, C_UnitName));
  Target.Exceptions.Add(EInvalidOp, Target.FindClass(C_SE2ExceptEInvalidOp, C_UnitName));
  Target.Exceptions.Add(EZeroDivide, Target.FindClass(C_SE2ExceptEZeroDivide, C_UnitName));
  Target.Exceptions.Add(EOverflow, Target.FindClass(C_SE2ExceptEOverflow, C_UnitName));
  Target.Exceptions.Add(EUnderflow, Target.FindClass(C_SE2ExceptEUnderflow, C_UnitName));

  Target.Exceptions.Add(EAbort, Target.FindClass(C_SE2ExceptEAbort, C_UnitName));
  Target.Exceptions.Add(EInvalidPointer, Target.FindClass(C_SE2ExceptEInvalidPointer, C_UnitName));
  Target.Exceptions.Add(EConvertError, Target.FindClass(C_SE2ExceptEConvertError, C_UnitName));
  Target.Exceptions.Add(EAccessViolation, Target.FindClass(C_SE2ExceptEAccessViolation, C_UnitName));
  Target.Exceptions.Add(ESE2NullReferenceError, Target.FindClass(C_SE2ExceptENullReference, C_UnitName));
  Target.Exceptions.Add(ESE2UnassignedMethod, Target.FindClass(C_SE2ExceptEMethodNotAssigned, C_UnitName));

  Target.Exceptions.Add(ESE2StackOverflow, Target.FindClass(C_SE2ExceotEStackOverflow, C_UnitName));
end;

procedure RegisterUnit();
var p: TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.DoRegExceptions   := Unit_RegisterExceptions;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

initialization
  RegisterUnit();

end.
