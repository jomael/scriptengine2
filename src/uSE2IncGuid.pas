unit uSE2IncGuid;           

{$Include ScriptEngine.inc}

// To use the package mode, uncomment the following define
{.$DEFINE SE2_IMPORT_AS_PACKAGE}

// If you only want to register the method pointers to
// the script engine (e.g. for a release without
// the posibility to recompile scripts), uncomment the
// following define
{.$DEFINE SE2_ONLY_REGISTER_METHODS}

interface

uses
{$IFDEF SE2_IMPORT_AS_PACKAGE}
  uSE2PackageAPI;
{$ELSE}
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;
{$ENDIF}

{$IFDEF SE2_ONLY_REGISTER_METHODS}
const
  C_UnitName   = 'System';
{$ELSE}
const
  C_UnitName   = 'System';
  C_UnitSource = 
        'unit System;' + #13#10 +
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 +
        '  /// Represents a global unique identifier'+#13#10+ 
        '  TGuid = record' + #13#10 + 
        '  private' + #13#10 + 
        '    Fa  : LongWord;' + #13#10 + 
        '    Fb  : word;' + #13#10 + 
        '    Fc  : word;' + #13#10 + 
        '    Fd1 : byte;' + #13#10 + 
        '    Fd2 : byte;' + #13#10 + 
        '    Fd3 : byte;' + #13#10 + 
        '    Fd4 : byte;' + #13#10 + 
        '    Fd5 : byte;' + #13#10 + 
        '    Fd6 : byte;' + #13#10 + 
        '    Fd7 : byte;' + #13#10 + 
        '    Fd8 : byte;' + #13#10 + 
        '  protected' + #13#10 + 
        '    class function CreateNewGuid: TGuid; external;' + #13#10 + 
        '    class function GuidToString(aGuid: TGuid): string; external;' + #13#10 + 
        '    class function StringToGuid(value: string): TGuid; external;' + #13#10 + 
        '  public' + #13#10 +
        '    class function Guid(a : LongWord; b, c: word; d1, d2, d3, d4, d5, d6, d7, d8: byte): TGuid; overload;' + #13#10 + 
        '    /// Create a new guid'+#13#10+
        '    class function Guid(value: string): TGuid; overload;' + #13#10 +      
        '    /// Returns a new guid equals to {00000000-0000-0000-0000-000000000000}'+#13#10+
        '    class function Empty: TGuid;' + #13#10 +
        '    /// Generate a new unique guid'+#13#10+
        '    class function Generate: TGuid;' + #13#10 +
        #13#10 +
        '    /// Returns true, if the guid is equal to another guid'+#13#10+
        '    function Equals(aGuid: TGuid): boolean;' + #13#10 + 
        '    /// Checks, if the guid is equal to {00000000-0000-0000-0000-000000000000}'+#13#10+
        '    function IsZeroGuid: boolean;' + #13#10 + 
        #13#10 +
        '    /// Converts a guid to a string'+#13#10+
        '    /// Format: {xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}'+#13#10+
        '    function ToString: string;' + #13#10 + 
        '  end;' + #13#10 +
        #13#10 +
        '  Convert = partial class' + #13#10 + 
        '  public' + #13#10 + 
        '    class function ToString(value: TGuid): string; overload;' + #13#10 + 
        '    class function ToGuid(value: string): TGuid; overload;' + #13#10 + 
        '  end;' + #13#10 +
        #13#10 + 
        'implementation' + #13#10 +
        #13#10 +
        'class function Convert.ToString(value: TGuid): string;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value.ToString;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToGuid(value: string): TGuid;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := TGuid.Guid(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 +
        ' { TGuid }'+#13#10+
        #13#10 + 
        'class function TGuid.Guid(a : LongWord; b, c: word; d1, d2, d3, d4, d5, d6, d7, d8: byte): TGuid;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.Fa := a;' + #13#10 + 
        '  result.Fb := b;' + #13#10 + 
        '  result.Fc := c;' + #13#10 + 
        '  result.Fd1 := d1;' + #13#10 + 
        '  result.Fd2 := d2;' + #13#10 + 
        '  result.Fd3 := d3;' + #13#10 + 
        '  result.Fd4 := d4;' + #13#10 + 
        '  result.Fd5 := d5;' + #13#10 + 
        '  result.Fd6 := d6;' + #13#10 + 
        '  result.Fd7 := d7;' + #13#10 + 
        '  result.Fd8 := d8;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TGuid.Guid(value: string): TGuid;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := TGuid.StringToGuid(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TGuid.Empty: TGuid;' + #13#10 + 
        'begin' + #13#10 + 
        '  // do nothing - is ok' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TGuid.Generate: TGuid;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := TGuid.CreateNewGuid;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TGuid.Equals(aGuid: TGuid): boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := False;' + #13#10 + 
        '  if Self.Fa = aGuid.Fa then' + #13#10 + 
        '  if Self.Fb = aGuid.Fb then' + #13#10 + 
        '  if Self.Fc = aGuid.Fc then' + #13#10 + 
        '  if Self.Fd1 = aGuid.Fd1 then' + #13#10 + 
        '  if Self.Fd2 = aGuid.Fd2 then' + #13#10 + 
        '  if Self.Fd3 = aGuid.Fd3 then' + #13#10 + 
        '  if Self.Fd4 = aGuid.Fd4 then' + #13#10 + 
        '  if Self.Fd5 = aGuid.Fd5 then' + #13#10 + 
        '  if Self.Fd6 = aGuid.Fd6 then' + #13#10 + 
        '  if Self.Fd7 = aGuid.Fd7 then' + #13#10 + 
        '  if Self.Fd8 = aGuid.Fd8 then' + #13#10 + 
        '     result := True;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TGuid.IsZeroGuid: boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := False;' + #13#10 + 
        '  if Self.Fa = 0 then' + #13#10 + 
        '  if Self.Fb = 0 then' + #13#10 + 
        '  if Self.Fc = 0 then' + #13#10 + 
        '  if Self.Fd1 = 0 then' + #13#10 + 
        '  if Self.Fd2 = 0 then' + #13#10 + 
        '  if Self.Fd3 = 0 then' + #13#10 + 
        '  if Self.Fd4 = 0 then' + #13#10 + 
        '  if Self.Fd5 = 0 then' + #13#10 + 
        '  if Self.Fd6 = 0 then' + #13#10 + 
        '  if Self.Fd7 = 0 then' + #13#10 + 
        '  if Self.Fd8 = 0 then' + #13#10 + 
        '     result := True;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TGuid.ToString: string;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := TGuid.GuidToString(Self);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'end.';

{$ENDIF}

{$IFDEF SE2_IMPORT_AS_PACKAGE}
procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
{$ENDIF}

implementation

uses SysUtils;

function TGuid_CreateNewGuid(Self: Pointer): TGuid;
begin
  SysUtils.CreateGUID(result);
end;

function TGuid_GuidToString(Self: Pointer; aGuid: TGuid): string;
begin
  result := SysUtils.GUIDToString(aGuid);
end;

function TGuid_StringToGuid(Self: Pointer; value: string): TGuid;
begin
  result := SysUtils.StringToGUID(value);
end;

{$IFNDEF SE2_IMPORT_AS_PACKAGE}
procedure Unit_GetSource(var Target: string);
begin
  {$IFNDEF SE2_ONLY_REGISTER_METHODS}
  Target := C_UnitSource;
  {$ELSE}
  Target := '';
  {$ENDIF}
end;
{$ENDIF}

{$IFNDEF SE2_IMPORT_AS_PACKAGE}
procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
    Target.Method['TGuid.CreateNewGuid[0]', C_UnitName] := @TGuid_CreateNewGuid;
    Target.Method['TGuid.GuidToString[0]', C_UnitName] := @TGuid_GuidToString;
    Target.Method['TGuid.StringToGuid[0]', C_UnitName] := @TGuid_StringToGuid;
  end
end;
{$ELSE}
procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TGuid_CreateNewGuid, 'TGuid.CreateNewGuid[0]');
  CallBack(Module, Data, @TGuid_GuidToString, 'TGuid.GuidToString[0]');
  CallBack(Module, Data, @TGuid_StringToGuid, 'TGuid.StringToGuid[0]');
end;
{$ENDIF}


{$IFNDEF SE2_IMPORT_AS_PACKAGE}
procedure RegisterUnit();
var p: TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;
{$ENDIF}

{$IFNDEF SE2_IMPORT_AS_PACKAGE}
initialization
  RegisterUnit();
{$ENDIF}

end.
