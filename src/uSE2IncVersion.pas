unit uSE2IncVersion;     

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
        '  /// Stores version information and gives the possibility to compare them'+#13#10+
        '  TVersion = record' + #13#10 + 
        '  private' + #13#10 + 
        '    FMajor    : integer;' + #13#10 + 
        '    FMinor    : integer;' + #13#10 + 
        '    FBuild    : integer;' + #13#10 + 
        '    FRevision : integer;' + #13#10 + 
        '  public' + #13#10 +
        '    /// The major part of the version (x.0.0.0)'+#13#10+
        '    property Major    : integer read FMajor    write FMajor;' + #13#10 +
        '    /// the minor part of the version (0.x.0.0)'+#13#10+
        '    property Minor    : integer read FMinor    write FMinor;' + #13#10 +
        '    /// the build part of the version (0.0.x.0)'+#13#10+
        '    property Build    : integer read FBuild    write FBuild;' + #13#10 +
        '    /// the revision / patch part of the version (0.0.0.x)'+#13#10+
        '    property Revision : integer read FRevision write FRevision;' + #13#10 + 
        #13#10 +
        '    function ToString: string; overload;' + #13#10 +
        '    /// Convert the version to a string'+#13#10+
        '    /// Format: x.x.x.x'+#13#10+
        '    function ToString(FieldCount: integer): string; overload;' + #13#10 +
        '    /// Returns true, if this version is equals to another one'+#13#10+
        '    function Equals(const Other: TVersion): boolean;' + #13#10 +
        #13#10 +
        '    function Increase(Major: integer): TVersion; overload;' + #13#10 + 
        '    function Increase(Major, Minor: integer): TVersion; overload;' + #13#10 + 
        '    function Increase(Major, Minor, Build: integer): TVersion; overload;' + #13#10 + 
        '    function Increase(Major, Minor, Build, Revision: integer): TVersion; overload;' + #13#10 +
        '    /// Returns a new version, increased with the given parameters'+#13#10+
        '    function Increase(Version: TVersion): TVersion; overload;' + #13#10 +
        #13#10 +
        '    /// Compares two version and returns the comparison code'+#13#10+
        '    /// < 0: this version is older than "other"'+#13#10+
        '    /// > 0: this version is newer than "other"'+#13#10+
        '    /// = 0: both versions are equal'+#13#10+
        '    function CompareTo(const Other: TVersion): integer;' + #13#10 +
        '    function BiggerEqual(toVersion: TVersion): boolean;' + #13#10 +
        '    function Bigger(toVersion: TVersion): boolean;' + #13#10 +
        '    function SmallerEqual(toVersion: TVersion): boolean;' + #13#10 + 
        '    function Smaller(toVersion: TVersion): boolean;' + #13#10 + 
        #13#10 + 
        '    class function Version(versionString: string): TVersion; overload;' + #13#10 + 
        '    class function Version(Major, Minor: integer): TVersion; overload;' + #13#10 + 
        '    class function Version(Major, Minor, Build: integer): TVersion; overload;' + #13#10 +
        '    /// Create a new version specified by the given parameters'+#13#10+
        '    class function Version(Major, Minor, Build, Revision: integer): TVersion; overload;' + #13#10 + 
        #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'function TVersion.ToString: string;' + #13#10 + 
        'begin' + #13#10 + 
        '  if Self.FBuild = -1 then' + #13#10 + 
        '     result := Self.ToString(2)' + #13#10 + 
        '  else' + #13#10 + 
        '  if Self.Revision = -1 then' + #13#10 + 
        '     result := Self.ToString(3)' + #13#10 + 
        '  else' + #13#10 + 
        '     result := Self.ToString(4);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.ToString(FieldCount: integer): string;' + #13#10 + 
        'begin' + #13#10 + 
        '  case FieldCount of' + #13#10 + 
        '  0 : result := '''';' + #13#10 + 
        '  1 : result := Self.FMajor.ToString;' + #13#10 + 
        '  2 : result := Self.FMajor.ToString + ''.'' + Self.FMinor.ToString;' + #13#10 + 
        '  3 :' + #13#10 + 
        '      begin' + #13#10 + 
        '        result := Self.FMajor.ToString + ''.'' + Self.FMinor.ToString;' + #13#10 + 
        '        if Self.FBuild = -1 then' + #13#10 + 
        '           result := result + ''.0''' + #13#10 + 
        '        else' + #13#10 + 
        '           result := result + ''.'' + Self.FBuild.ToString;' + #13#10 + 
        '      end;' + #13#10 + 
        '  4 :' + #13#10 + 
        '      begin' + #13#10 + 
        '        result := Self.FMajor.ToString + ''.'' + Self.FMinor.ToString;' + #13#10 + 
        '        if Self.FBuild = -1 then' + #13#10 + 
        '           result := result + ''.0''' + #13#10 + 
        '        else' + #13#10 + 
        '           result := result + ''.'' + Self.FBuild.ToString;' + #13#10 + 
        '        if Self.FRevision = -1 then' + #13#10 + 
        '           result := result + ''.0''' + #13#10 + 
        '        else' + #13#10 + 
        '           result := result + ''.'' + Self.FRevision.ToString;' + #13#10 + 
        '      end;' + #13#10 + 
        '  end;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.Equals(const Other: TVersion): boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self = Other;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.Increase(Major: integer): TVersion;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self;' + #13#10 + 
        '  result.FMajor := result.FMajor + Major;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.Increase(Major, Minor: integer): TVersion;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self;' + #13#10 + 
        '  result.FMajor := result.FMajor + Major;' + #13#10 + 
        '  result.FMinor := result.FMinor + Minor;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.Increase(Major, Minor, Build: integer): TVersion;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FMajor := Self.FMajor + Major;' + #13#10 + 
        '  result.FMinor := Self.FMinor + Minor;' + #13#10 + 
        '  if (Build <> -1) then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if (Self.Build = -1) then' + #13#10 + 
        '        result.Build  := Build' + #13#10 + 
        '    else' + #13#10 + 
        '        result.Build  := Self.Build + Build;' + #13#10 + 
        '  end else' + #13#10 + 
        '    result.Build := Self.Build;' + #13#10 + 
        #13#10 + 
        '  result.FRevision := Self.FRevision;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.Increase(Major, Minor, Build, Revision: integer): TVersion;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FMajor := Self.FMajor + Major;' + #13#10 + 
        '  result.FMinor := Self.FMinor + Minor;' + #13#10 + 
        #13#10 + 
        '  if Build <> -1 then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if (Self.Build = -1) then' + #13#10 + 
        '        result.Build  := Build' + #13#10 + 
        '    else' + #13#10 + 
        '        result.Build  := Self.Build + Build;' + #13#10 + 
        '  end else' + #13#10 + 
        '    result.Build := Self.Build;' + #13#10 + 
        #13#10 + 
        '  if Revision <> -1 then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if (Self.Revision = -1) then' + #13#10 + 
        '        result.Revision  := Revision' + #13#10 + 
        '    else' + #13#10 + 
        '        result.Revision  := Self.Revision + Revision;' + #13#10 + 
        '  end else' + #13#10 + 
        '     result.Revision := Self.Revision;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.Increase(Version: TVersion): TVersion;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.Increase(Version.FMajor, Version.FMajor, Version.FBuild, Version.FRevision);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.CompareTo(const Other: TVersion): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  if Self.FMajor <> Other.Major then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if Self.Major > Other.Major then' + #13#10 + 
        '       result := 1' + #13#10 + 
        '    else' + #13#10 + 
        '       result := -1;' + #13#10 + 
        '    exit;' + #13#10 + 
        '  end;' + #13#10 + 
        '  if Self.FMinor <> Other.Minor then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if Self.Minor > Other.Minor then' + #13#10 + 
        '       result := 1' + #13#10 + 
        '    else' + #13#10 + 
        '       result := -1;' + #13#10 + 
        '    exit;' + #13#10 + 
        '  end;' + #13#10 + 
        '  if Self.FBuild <> Other.Build then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if Self.Build > Other.Build then' + #13#10 + 
        '       result := 1' + #13#10 + 
        '    else' + #13#10 + 
        '       result := -1;' + #13#10 + 
        '    exit;' + #13#10 + 
        '  end;' + #13#10 + 
        '  if Self.FRevision <> Other.Revision then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if Self.FRevision > Other.Revision then' + #13#10 + 
        '       result := 1' + #13#10 + 
        '    else' + #13#10 + 
        '       result := -1;' + #13#10 + 
        '    exit;' + #13#10 + 
        '  end;' + #13#10 + 
        '  result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.BiggerEqual(toVersion: TVersion): boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.CompareTo(toVersion) >= 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.Bigger(toVersion: TVersion): boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.CompareTo(toVersion) > 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.SmallerEqual(toVersion: TVersion): boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.CompareTo(toVersion) <= 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TVersion.Smaller(toVersion: TVersion): boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.CompareTo(toVersion) < 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TVersion.Version(versionString: string): TVersion;' + #13#10 + 
        'var v1, v2, v3, v4 : integer;' + #13#10 + 
        '    iPos: integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  iPos := versionString.IndexOf(''.'');' + #13#10 + 
        '  if iPos = 0 then' + #13#10 + 
        '     raise EConvertError.Create(''Expected at least one dot'');' + #13#10 + 
        '  if not Convert.TryStrToInt(versionString.SubString(1, iPos - 1), v1) then' + #13#10 + 
        '     raise EConvertError.Create(''Invalid number found'');' + #13#10 + 
        '  versionString := versionString.SubString(iPos + 1);' + #13#10 + 
        #13#10 + 
        '  iPos := versionString.IndexOf(''.'');' + #13#10 + 
        '  if iPos = 0 then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if not Convert.TryStrToInt(versionString, v2) then' + #13#10 + 
        '       raise EConvertError.Create(''Invalid number found'');' + #13#10 + 
        '    result := TVersion.Version(v1, v2);' + #13#10 + 
        '    exit;' + #13#10 + 
        '  end;' + #13#10 + 
        '  if not Convert.TryStrToInt(versionString.SubString(1, iPos - 1), v2) then' + #13#10 + 
        '     raise EConvertError.Create(''Invalid number found'');' + #13#10 + 
        '  versionString := versionString.SubString(iPos + 1);' + #13#10 + 
        #13#10 + 
        #13#10 + 
        '  iPos := versionString.IndexOf(''.'');' + #13#10 + 
        '  if iPos = 0 then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if not Convert.TryStrToInt(versionString, v3) then' + #13#10 + 
        '       raise EConvertError.Create(''Invalid number found'');' + #13#10 + 
        '    result := TVersion.Version(v1, v2, v3);' + #13#10 + 
        '    exit;' + #13#10 + 
        '  end;' + #13#10 + 
        '  if not Convert.TryStrToInt(versionString.SubString(1, iPos - 1), v3) then' + #13#10 + 
        '     raise EConvertError.Create(''Invalid number found'');' + #13#10 + 
        #13#10 + 
        #13#10 + 
        '  if not Convert.TryStrToInt(versionString.SubString(iPos + 1), v4) then' + #13#10 + 
        '     raise EConvertError.Create(''Invalid number found'');' + #13#10 + 
        #13#10 + 
        '  result := TVersion.Version(v1, v2, v3, v4);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TVersion.Version(Major, Minor: integer): TVersion;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.Major := Major;' + #13#10 + 
        '  result.Minor := Minor;' + #13#10 + 
        '  result.FBuild := -1;' + #13#10 + 
        '  result.FRevision := -1;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TVersion.Version(Major, Minor, Build: integer): TVersion;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.Major := Major;' + #13#10 + 
        '  result.Minor := Minor;' + #13#10 + 
        '  result.FBuild := Build;' + #13#10 + 
        '  result.FRevision := -1;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TVersion.Version(Major, Minor, Build, Revision: integer): TVersion;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.Major := Major;' + #13#10 + 
        '  result.Minor := Minor;' + #13#10 + 
        '  result.FBuild := Build;' + #13#10 + 
        '  result.FRevision := Revision;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'end.';

{$ENDIF}

{$IFDEF SE2_IMPORT_AS_PACKAGE}
procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
{$ENDIF}

implementation

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
  end
end;
{$ELSE}
procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
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
