unit uSE2IncConvert;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2Types, uSE2BaseTypes, uSE2RunAccess, uSE2UnitManager;

implementation

const
  C_UnitName   = 'System';
  C_UnitSource =
     'unit System;'+#13#10+
     #13#10+
     'interface'+#13#10+
     #13#10+
     'type'+#13#10+
     '  TFloatFormat = (ffGeneral, ffExponent, ffFixed, ffNumber, ffCurrency);'+#13#10+
     #13#10+
     '  Convert = partial class(TExternalObject)'+#13#10+
     '  public'+#13#10+
     '    class function IntToStr(i: int64): string; overload; external;'+#13#10+
     '    class function IntToStr(i: int64; digits: integer): string; overload;'+#13#10+
     '    class function IntToStr(i: int64; digits: integer; PrefixStr: string): string; overload;'+#13#10+
     '    class function IntToHex(i: int64; Digits: int64): string; external;'+#13#10+
     '    class function IsInteger(s: string): boolean; external;'+#13#10+
     '    class function StrToInt(s: string): int64; external;'+#13#10+
     '    class function StrToIntDef(s: string; def: int64): int64; external;'+#13#10+

     '    class function TryStrToInt(s: string; out i: byte): boolean; overload; external;'+#13#10+
     '    class function TryStrToInt(s: string; out i: shortint): boolean; overload; external;'+#13#10+
     '    class function TryStrToInt(s: string; out i: word): boolean; overload; external;'+#13#10+
     '    class function TryStrToInt(s: string; out i: smallint): boolean; overload; external;'+#13#10+
     '    class function TryStrToInt(s: string; out i: cardinal): boolean; overload; external;'+#13#10+
     '    class function TryStrToInt(s: string; out i: integer): boolean; overload; external;'+#13#10+
     '    class function TryStrToInt(s: string; out i: int64): boolean; overload; external;'+#13#10+

     '    class function FloatToStr(d: double): string; overload; external;'+#13#10+
     '    class function FloatToStr(d: double; Format: TFloatFormat; Precision, Digits: Integer): string; overload; external;'+#13#10+
     '    class function IsFloat(s: string): boolean; external;'+#13#10+
     '    class function StrToFloat(s: string): double; external;'+#13#10+
     '    class function StrToFloatDef(s: string; def: double): double; external;'+#13#10+
     '    class function TryStrToFloat(s: string; out i: single): boolean; overload; external;'+#13#10+
     '    class function TryStrToFloat(s: string; out i: double): boolean; overload; external;'+#13#10+

     '    class function BoolToStr(b: boolean; UseBoolStr: boolean): string; overload; external;'+#13#10+
     '    class function BoolToStr(b: boolean): string; overload; external;'+#13#10+
     '    class function StrToBool(s: string): boolean; external;'+#13#10+
     '    class function StrToBoolDef(s: string; Def: Boolean): Boolean; external;'+#13#10+
     '    class function TryStrToBool(s: string; out value: boolean): boolean; external;'+#13#10+
     '  end;'+#13#10+
     #13#10+
     'implementation'+#13#10+
     #13#10+
     'class function Convert.IntToStr(i: int64; digits: integer): string;'+#13#10+
     'var isNeg: boolean;'+#13#10+
     'begin'+#13#10+
     '  isNeg  := i < 0;'+#13#10+
     '  if i < 0 then'+#13#10+
     '     i := -i;'+#13#10+
     '  result := Convert.IntToStr(i);'+#13#10+
     '  while Strings.Length(result) < digits do'+#13#10+
     '     result := ''0'' + result;'+#13#10+
     '  if isNeg then'+#13#10+
     '     result := ''-'' + result;'+#13#10+
     'end;'+#13#10+
     #13#10 +
     'class function Convert.IntToStr(i: int64; digits: integer; PrefixStr: string): string;'+#13#10+
     'var isNeg: boolean;'+#13#10+
     'begin'+#13#10+
     '  isNeg  := i < 0;'+#13#10+
     '  if i < 0 then'+#13#10+
     '     i := -i;'+#13#10+
     '  result := Convert.IntToStr(i);'+#13#10+
     '  while Strings.Length(result) < digits do'+#13#10+
     '     result := PrefixStr + result;'+#13#10+
     '  if isNeg then'+#13#10+
     '     result := ''-'' + result;'+#13#10+
     'end;'+#13#10+
     #13#10+
     'end.';

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

type
  TConvert = class
  public
    class function IntToStr(i: int64): string;
    class function IntToHex(i: int64; Digits: int64): string;
    class function IsInteger(s: string): boolean;
    class function StrToInt(s: string): int64;
    class function StrToIntDef(s: string; def: int64): int64;

    class function TryStrToIntU8(s: string; var i: byte): boolean;
    class function TryStrToIntS8(s: string; var i: Shortint): boolean;
    class function TryStrToIntU16(s: string; var i: Word): boolean;
    class function TryStrToIntS16(s: string; var i: Smallint): boolean;
    class function TryStrToIntU32(s: string; var i: cardinal): boolean;
    class function TryStrToIntS32(s: string; var i: integer): boolean;
    class function TryStrToIntS64(s: string; var i: int64): boolean;

    class function FloatToStr(d: double): string;
    class function FloatToStrF(d: double; Format: TFloatFormat; Precision, Digits: Integer): string;
    class function IsFloat(s: string): boolean;
    class function StrToFloat(s: string): double;
    class function StrToFloatDef(s: string; def: double): double;
    class function TryStrToFloatS(s: string; var d: single): boolean;
    class function TryStrToFloatD(s: string; var d: double): boolean;

    class function BoolToStr0(b: boolean; UseBoolStr: boolean): string;
    class function BoolToStr1(b: boolean): string;
    class function StrToBool(s: string): boolean;
    class function StrToBoolDef(s: string; Def: Boolean): Boolean;
    class function TryStrToBool(s: string; out value: boolean): boolean;
  end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
     Target.Method['Convert.IntToStr[0]', C_UnitName] := @TConvert.IntToStr;
     Target.Method['Convert.IntToHex[0]', c_UnitName] := @TConvert.IntToHex;
     Target.Method['Convert.IsInteger[0]', C_UnitName] := @TConvert.IsInteger;
     Target.Method['Convert.StrToInt[0]', C_UnitName] := @TConvert.StrToInt;
     Target.Method['Convert.StrToIntDef[0]', C_UnitName] := @TConvert.StrToIntDef;

     Target.Method['Convert.TryStrToInt[0]', C_UnitName] := @TConvert.TryStrToIntU8;
     Target.Method['Convert.TryStrToInt[1]', C_UnitName] := @TConvert.TryStrToIntS8;
     Target.Method['Convert.TryStrToInt[2]', C_UnitName] := @TConvert.TryStrToIntU16;
     Target.Method['Convert.TryStrToInt[3]', C_UnitName] := @TConvert.TryStrToIntS16;
     Target.Method['Convert.TryStrToInt[4]', C_UnitName] := @TConvert.TryStrToIntU32;
     Target.Method['Convert.TryStrToInt[5]', C_UnitName] := @TConvert.TryStrToIntS32;
     Target.Method['Convert.TryStrToInt[6]', C_UnitName] := @TConvert.TryStrToIntS64;

     Target.Method['Convert.FloatToStr[0]', C_UnitName] := @TConvert.FloatToStr;
     Target.Method['Convert.FloatToStr[1]', C_UnitName] := @TConvert.FloatToStrF;
     Target.Method['Convert.IsFloat[0]', C_UnitName] := @TConvert.IsFloat;
     Target.Method['Convert.StrToFloat[0]', C_UnitName] := @TConvert.StrToFloat;
     Target.Method['Convert.StrToFloatDef[0]', C_UnitName] := @TConvert.StrToFloatDef;
     Target.Method['Convert.TryStrToFloat[0]', C_UnitName] := @TConvert.TryStrToFloatS;
     Target.Method['Convert.TryStrToFloat[1]', C_UnitName] := @TConvert.TryStrToFloatD;

     Target.Method['Convert.BoolToStr[0]', C_UnitName] := @TConvert.BoolToStr0;
     Target.Method['Convert.BoolToStr[1]', C_UnitName] := @TConvert.BoolToStr1;
     Target.Method['Convert.StrToBool[0]', C_UnitName] := @TConvert.StrToBool;
     Target.Method['Convert.StrToBoolDef[0]', C_UnitName] := @TConvert.StrToBoolDef;
     Target.Method['Convert.TryStrToBool[0]', C_UnitName] := @TConvert.TryStrToBool;
  end;
end;

procedure RegisterUnit;
var p : TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;              
  p.Priority          := 4;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

{ TConvert }

class function TConvert.BoolToStr0(b, UseBoolStr: boolean): string;
begin
  result := SysUtils.BoolToStr(b, UseBoolStr);
end;

class function TConvert.BoolToStr1(b: boolean): string;
begin
  result := SysUtils.BoolToStr(b);
end;

class function TConvert.FloatToStr(d: double): string;
begin
  result := SysUtils.FloatToStr(d);
end;

class function TConvert.FloatToStrF(d: double; Format: TFloatFormat;
  Precision, Digits: Integer): string;
begin
  result := SysUtils.FloatToStrF(d, Format, Precision, Digits);
end;

class function TConvert.IntToHex(i: int64; Digits: int64): string;
begin
  result := SysUtils.IntToHex(i, Digits);
end;

class function TConvert.IntToStr(i: int64): string;
begin
  result := SysUtils.IntToStr(i);
end;

{$Hints off}
class function TConvert.IsFloat(s: string): boolean;
var i: double;
    c: integer;
begin
  Val(s, i, c);
  result := c = 0;
  if not result then
  begin
    result := SysUtils.TryStrToFloat(s, i);
  end;
end;

class function TConvert.IsInteger(s: string): boolean;
var i: int64;
    c: integer;
begin
  Val(s, i, c);
  result := c = 0;
end;
{$Hints on}

class function TConvert.StrToBool(s: string): boolean;
begin
  result := SysUtils.StrToBool(s);
end;

class function TConvert.StrToBoolDef(s: string; Def: Boolean): Boolean;
begin
  result := SysUtils.StrToBoolDef(s, def);
end;

class function TConvert.StrToFloat(s: string): double;
var i: double;
    c: integer;
begin
  Val(s, i, c);
  if c = 0 then
     result := i
  else
     result := SysUtils.StrToFloat(s);
end;

class function TConvert.StrToFloatDef(s: string; def: double): double;
var i: double;
    c: integer;
begin
  Val(s, i, c);
  if c = 0 then
     result := i
  else
     result := SysUtils.StrToFloatDef(s, def);
end;

class function TConvert.StrToInt(s: string): int64;
begin
  result := SysUtils.StrToInt64(s);
end;

class function TConvert.StrToIntDef(s: string; def: int64): int64;
begin
  result := SysUtils.StrToIntDef(s, def);
end;

class function TConvert.TryStrToBool(s: string;
  out value: boolean): boolean;
begin
  result := SysUtils.TryStrToBool(s, value);
end;

class function TConvert.TryStrToFloatD(s: string; var d: double): boolean;
var i: double;
    c: integer;
begin
  Val(s, i, c);
  if c = 0 then
  begin
     d := i;
     result := True;
  end else
     result := SysUtils.TryStrToFloat(s, d);
end;

class function TConvert.TryStrToFloatS(s: string; var d: single): boolean;
var i: single;
    c: integer;
begin
  Val(s, i, c);
  if c = 0 then
  begin
     d := i;
     result := True;
  end else
     result := SysUtils.TryStrToFloat(s, d);
end;

class function TConvert.TryStrToIntS16(s: string;
  var i: Smallint): boolean;
var v: integer;
begin
  result := SysUtils.TryStrToInt(s, v);
  i := v;
end;

class function TConvert.TryStrToIntS32(s: string; var i: integer): boolean;
begin
  result := SysUtils.TryStrToInt(s, i);
end;

class function TConvert.TryStrToIntS64(s: string; var i: int64): boolean;
begin
  result := SysUtils.TryStrToInt64(s, i);
end;

class function TConvert.TryStrToIntS8(s: string; var i: Shortint): boolean;
var v: integer;
begin
  result := SysUtils.TryStrToInt(s, v);
  i := v;
end;

class function TConvert.TryStrToIntU16(s: string; var i: Word): boolean;
var v: integer;
begin
  result := SysUtils.TryStrToInt(s, v);
  i := v;
end;

class function TConvert.TryStrToIntU32(s: string;
  var i: cardinal): boolean;
var v: integer;
begin
  result := SysUtils.TryStrToInt(s, v);
  i := v;
end;

class function TConvert.TryStrToIntU8(s: string; var i: byte): boolean;
var v: integer;
begin
  result := SysUtils.TryStrToInt(s, v);
  i := v;
end;

initialization
  RegisterUnit;

end.
