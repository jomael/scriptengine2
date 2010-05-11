unit uSE2IncTimeZone;

{$Include ScriptEngine.inc}

// To use the package mode, uncomment the following define
{.$DEFINE SE2_IMPORT_AS_PACKAGE}

// If you only want to register the method pointers to
// the script engine (e.g. for a release without
// the posibility to recompile scripts), uncomment the
// following define
{.$DEFINE SE2_ONLY_REGISTER_METHODS}

interface

{$IFNDEF SEII_FPC}

uses
{$IFDEF SE2_IMPORT_AS_PACKAGE}
  uSE2PackageAPI;
{$ELSE}
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;
{$ENDIF}

type
  TTimeSpan = packed record
    Span : double;
  end;

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
        '  TimeZone = class(TExternalObject)' + #13#10 +
        '  public' + #13#10 + 
        '    class function Bias : TTimeSpan; external;' + #13#10 +
        #13#10 + 
        '    class function StandardBias : TTimeSpan; external;' + #13#10 +
        '    class function StandardName : WideString; external;' + #13#10 + 
        //'    class function StandardDate : TDateTime; external;' + #13#10 +
        #13#10 +
        '    class function DaylightBias : TTimeSpan; external;' + #13#10 +
        '    class function DaylightName : WideString; external;' + #13#10 +
        //'    class function DaylightDate : TDateTime; external;' + #13#10 +
        #13#10 +
        '    class function CurrentBias  : TTimeSpan; external;' + #13#10 +
        '    class function IsDaylight   : boolean; external;'+#13#10+
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

{$ENDIF}

{$IFDEF SE2_IMPORT_AS_PACKAGE}
procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
{$ENDIF}

{$ENDIF}

implementation

{$IFNDEF SEII_FPC}

uses
  Windows, SysUtils;

function TimeZone_Bias(Self: Pointer): TTimeSpan;
var Zone : TTimeZoneInformation;
begin
  GetTimeZoneInformation(Zone);
  result.Span := Zone.Bias / (60.0 * 24.0);
end;

function TimeZone_StandardBias(Self: Pointer): TTimeSpan;
var Zone : TTimeZoneInformation;
begin
  GetTimeZoneInformation(Zone);
  result.Span := Zone.StandardBias / (60.0 * 24.0);
end;

function TimeZone_StandardName(Self: Pointer): WideString;
var Zone : TTimeZoneInformation;
begin
  GetTimeZoneInformation(Zone);
  result := Zone.StandardName;
end;
          {
function TimeZone_StandardDate(Self: Pointer): TDateTime;
var Zone : TTimeZoneInformation;
begin
  DateTimeToSystemTime(Now, Zone.StandardDate);
  GetTimeZoneInformation(Zone);
  result := SystemTimeToDateTime(Zone.StandardDate);
end;       }

function TimeZone_DaylightBias(Self: Pointer): TTimeSpan;
var Zone : TTimeZoneInformation;
begin
  GetTimeZoneInformation(Zone);
  result.Span := Zone.DaylightBias / (60.0 * 24.0);
end;

function TimeZone_DaylightName(Self: Pointer): WideString;
var Zone : TTimeZoneInformation;
begin
  GetTimeZoneInformation(Zone);
  result := Zone.DaylightName;
end;
                       {
function TimeZone_DaylightDate(Self: Pointer): TDateTime;
var Zone : TTimeZoneInformation;
begin
  GetTimeZoneInformation(Zone);
  result := SystemTimeToDateTime(Zone.DaylightDate);
end;                }

function TimeZone_IsDaylight(Self: Pointer): boolean;
var Zone : TTimeZoneInformation;
begin
  case GetTimeZoneInformation(Zone) of
  TIME_ZONE_ID_STANDARD : result := False;
  TIME_ZONE_ID_DAYLIGHT : result := True;
  else                    result := False;
  end;
end;

function TimeZone_CurrentBias(Self: Pointer): TTimeSpan;
var Zone : TTimeZoneInformation;
begin
  case GetTimeZoneInformation(Zone) of
  TIME_ZONE_ID_STANDARD : result.Span := Zone.Bias / (60.0 * 24.0);
  TIME_ZONE_ID_DAYLIGHT : result.Span := (Zone.Bias + Zone.DaylightBias) / (60.0 * 24.0);
  else                    result.Span := Zone.Bias / (60.0 * 24.0);
  end;

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
    Target.Method['TimeZone.Bias[0]', C_UnitName] := @TimeZone_Bias;
    Target.Method['TimeZone.StandardBias[0]', C_UnitName] := @TimeZone_StandardBias;
    Target.Method['TimeZone.StandardName[0]', C_UnitName] := @TimeZone_StandardName;
    Target.Method['TimeZone.DaylightBias[0]', C_UnitName] := @TimeZone_DaylightBias;
    Target.Method['TimeZone.DaylightName[0]', C_UnitName] := @TimeZone_DaylightName;
    Target.Method['TimeZone.CurrentBias[0]', C_UnitName] := @TimeZone_CurrentBias;
    Target.Method['TimeZone.IsDaylight[0]', C_UnitName] := @TimeZone_IsDaylight;
  end
end;
{$ELSE}
procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TimeZone_Bias, 'TimeZone.Bias[0]');
  CallBack(Module, Data, @TimeZone_StandardBias, 'TimeZone.StandardBias[0]');
  CallBack(Module, Data, @TimeZone_StandardName, 'TimeZone.StandardName[0]');
  CallBack(Module, Data, @TimeZone_DaylightBias, 'TimeZone.DaylightBias[0]');
  CallBack(Module, Data, @TimeZone_DaylightName, 'TimeZone.DaylightName[0]');
  CallBack(Module, Data, @TimeZone_CurrentBias, 'TimeZone.CurrentBias[0]');
  CallBack(Module, Data, @TimeZone_IsDaylight, 'TimeZone.IsDaylight[0]');
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

{$ENDIF}

end.
