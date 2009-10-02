unit uSE2IncMath;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, Math, uSE2Types, uSE2BaseTypes, uSE2RunAccess, uSE2UnitManager;

implementation

const
  C_UnitName   = 'System';
  C_UnitSource =
      'unit System;'+#13#10+
      #13#10+
      'interface'+#13#10+
      #13#10+
      'const'+#13#10+
      '  PI            = 3.1415926535897932385;'+#13#10+
      #13#10+
      'type'+#13#10+
      '  TRoundToRange = shortint;'+#13#10+
      #13#10+
      '  Math = class(TExternalObject)'+#13#10+
      '  public'+#13#10+
      '    class function ArcCos(const X: double): double; external;'+#13#10+
      '    class function ArcCosh(const X: double): double; external;'+#13#10+
      {$IFNDEF SEII_NO_EXT_MATH}
      '    class function ArcCot(const X: double): double; external;'+#13#10+
      '    class function ArcCotH(const X: double): double; external;'+#13#10+
      '    class function ArcCsc(const X: double): double; external;'+#13#10+
      '    class function ArcCscH(const X: double): double; external;'+#13#10+
      '    class function ArcSec(const X: double): double; external;'+#13#10+
      '    class function ArcSecH(const X: double): double; external;'+#13#10+
      {$ENDIF}
      '    class function ArcSin(const X: double): double; external;'+#13#10+
      '    class function ArcSinh(X: double): double; external;'+#13#10+
      '    class function ArcTan2(const Y, X: double): double; external;'+#13#10+
      '    class function ArcTanh(X: double): double; external;'+#13#10+
      '    class function Ceil(const X: double): integer; external;'+#13#10+
      '    class function Cosecant(const X: double): double; external;'+#13#10+
      '    class function Cosh(const X: double): double; external;'+#13#10+
      '    class function Cot(const X: double): double; external;'+#13#10+   
      '    class function Cos(const X: double): double; external;'+#13#10+
      '    class function Cotan(const X: double): double; external;'+#13#10+
      {$IFNDEF SEII_NO_EXT_MATH}
      '    class function CotH(const X: double): double; external;'+#13#10+
      {$ENDIF}
      '    class function Csc(const X: double): double; external;'+#13#10+
      {$IFNDEF SEII_NO_EXT_MATH}
      '    class function CscH(const X: double): double; external;'+#13#10+
      {$ENDIF}
      {$IFNDEF SEII_NO_EXT_MATH}
      '    class function CycleToDeg(const Cycles: double): double; external;'+#13#10+
      {$ENDIF}
      '    class function CycleToGrad(const Cycles: double): double; external;'+#13#10+
      '    class function CycleToRad(const Cycles: double): double; external;'+#13#10+
      {$IFNDEF SEII_NO_EXT_MATH}
      '    class function DegToCycle(const Degrees: double): double; external;'+#13#10+
      {$ENDIF}
      '    class function DegToGrad(const Degrees: double): double; external;'+#13#10+
      '    class function DegToRad(const Degrees: double): double; external;'+#13#10+
      '    class procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: Word); external;'+#13#10+
      {$IFNDEF SEII_NO_EXT_MATH}
      '    class function DoubleDecliningBalance(const Cost, Salvage: double; Life, Period: Integer): double; external;'+#13#10+
      {$ENDIF}
      '    class function EnsureRange(const AValue, AMin, AMax: Int64): Int64; overload; external;'+#13#10+
      '    class function EnsureRange(const AValue, AMin, AMax: Double): Double; overload; external;'+#13#10+
      '    class function Floor(const X: double): Integer; external;'+#13#10+
      '    class procedure Frexp(const X: double; var Mantissa: double; var Exponent: Integer); external;'+#13#10+
      {$IFNDEF SEII_NO_EXT_MATH}
      '    class function GradToCycle(const Grads: double): double; external;'+#13#10+
      {$ENDIF}
      '    class function GradToDeg(const Grads: double): double; external;'+#13#10+
      '    class function GradToRad(const Grads: double): double; external;'+#13#10+
      '    class function Hypot(const X, Y: double): double; external;'+#13#10+
      '    class function InRange(const AValue, AMin, AMax: Int64): Boolean; overload; external;'+#13#10+
      '    class function InRange(const AValue, AMin, AMax: Double): Boolean; overload; external;'+#13#10+
      '    class function IntPower(const Base: double; const Exponent: Integer): double;  external;'+#13#10+
      '    class function IsInfinite(const AValue: Double): Boolean; external;'+#13#10+
      '    class function IsNan(const AValue: Single): Boolean; overload; external;'+#13#10+
      '    class function IsNan(const AValue: Double): Boolean; overload; external;'+#13#10+
      '    class function IsZero(const A: Single; Epsilon: Single): Boolean; overload; external;'+#13#10+ 
      '    class function IsZero(const A: Single): Boolean; overload; external;'+#13#10+
      '    class function IsZero(const A: Double; Epsilon: Double): Boolean; overload; external;'+#13#10+  
      '    class function IsZero(const A: Double): Boolean; overload; external;'+#13#10+
      '    class function Ldexp(const X: double; const P: Integer): double; external;'+#13#10+
      '    class function LnXP1(const X: double): double; external;'+#13#10+
      '    class function Log10(const X: double): double; external;'+#13#10+
      '    class function Log2(const X: double): double; external;'+#13#10+
      '    class function LogN(const Base, X: double): double; external;'+#13#10+
      '    class function Max(A,B: Int64): Int64; overload; external;'+#13#10+
      '    class function Max(A,B: Single): Single; overload; external;'+#13#10+
      '    class function Max(A,B: Double): Double; overload; external;'+#13#10+
      '    class function Min(A,B: Int64): Int64; overload; external;'+#13#10+
      '    class function Min(A,B: Single): Single; overload; external;'+#13#10+
      '    class function Min(A,B: Double): Double; overload; external;'+#13#10+
      '    class function Power(const Base, Exponent: double): double; external;'+#13#10+
      '    class function RadToCycle(const Radians: double): double; external;'+#13#10+
      '    class function RadToDeg(const Radians: double): double; external;'+#13#10+
      '    class function RadToGrad(const Radians: double): double; external;'+#13#10+
      '    class function RandG(Mean, StdDev: double): double; external;'+#13#10+
      '    class function RandomRange(const AFrom, ATo: Integer): Integer; external;'+#13#10+
      '    class function RoundTo(const AValue: Double; const ADigit: TRoundToRange): Double; external;'+#13#10+
      '    class function SameValue(const A, B: Single; Epsilon: Single): Boolean; overload; external;'+#13#10+   
      '    class function SameValue(const A, B: Single): Boolean; overload; external;'+#13#10+
      '    class function SameValue(const A, B: Double; Epsilon: Double): Boolean; overload; external;'+#13#10+ 
      '    class function SameValue(const A, B: Double): Boolean; overload; external;'+#13#10+
      '    class function Sec(const X: double): double; external;'+#13#10+
      {$IFNDEF SEII_NO_EXT_MATH}
      '    class function SecH(const X: double): double; external;'+#13#10+
      {$ENDIF}
      '    class function Sign(const AValue: Double): shortint; overload; external;'+#13#10+
      '    class function Sign(const AValue: Integer): shortint; overload; external;'+#13#10+
      '    class function Sign(const AValue: Int64): shortint; overload; external;'+#13#10+
      '    class procedure SinCos(const Theta: double; var Sin, Cos: double); external;'+#13#10+
      '    class function Sin(X: double): double; external;'+#13#10+
      '    class function Sinh(const X: double): double; external;'+#13#10+
      {$IFNDEF SEII_NO_EXT_MATH}
      '    class function SLNDepreciation(const Cost, Salvage: double; Life: Integer): double; external;'+#13#10+
      '    class function SYDDepreciation(const Cost, Salvage: double; Life, Period: Integer): double; external;'+#13#10+
      {$ENDIF}
      '    class function Tan(const X: double): double; external;'+#13#10+
      '    class function Tanh(const X: double): double; external;'+#13#10+
      '    class function Exp(X: double): double; external;'+#13#10+
      '    class function Abs(X: int64): int64; overload; external;'+#13#10+
      '    class function Abs(X: single): single; overload; external;'+#13#10+
      '    class function Abs(X: double): double; overload; external;'+#13#10+
      '    class function Frac(X: double): double; external;'+#13#10+
      '    class function Ln(X: double): double; external;'+#13#10+
      '    class function Round(X: double): Int64; external;'+#13#10+
      '    class function Sqr(X: double): double; overload; external;'+#13#10+
      '    class function Sqr(X: Integer): Integer; overload; external;'+#13#10+
      '    class function Sqrt(X: double): double; external;'+#13#10+
      '    class function Trunc(X: double): Int64; external;'+#13#10+
      
      '    class procedure Randomize; external;'+#13#10+
      '    class function Random: double; overload; external;'+#13#10+
      '    class function Random(max: integer): integer; overload; external;'+#13#10+
      '    class function RandSeed: integer; external;'+#13#10+
      '    class procedure SetRandSeed(value: integer); external;'+#13#10+
      '  end;'+#13#10+
      #13#10+
      'implementation'+#13#10+
      #13#10+
      'end.';

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

type
  TMath = class
  public
    class function ArcCos(const X: double): double;
    class function ArcCosh(const X: double): double;
    {$IFNDEF SEII_NO_EXT_MATH}
    class function ArcCot(const X: double): double;
    class function ArcCotH(const X: double): double;
    class function ArcCsc(const X: double): double;
    class function ArcCscH(const X: double): double;
    class function ArcSec(const X: double): double;
    class function ArcSecH(const X: double): double;
    {$ENDIF}
    class function ArcSin(const X: double): double;
    class function ArcSinh(X: double): double;
    class function ArcTan2(const Y, X: double): double;
    class function ArcTanh(X: double): double;
    class function Ceil(const X: double): integer;
    class function Cosecant(const X: double): double;
    class function Cosh(const X: double): double;
    class function Cot(const X: double): double;
    class function Cos(const X: double): double;
    class function Cotan(const X: double): double;
    {$IFNDEF SEII_NO_EXT_MATH}
    class function CotH(const X: double): double;
    {$ENDIF}
    class function Csc(const X: double): double;
    {$IFNDEF SEII_NO_EXT_MATH}
    class function CscH(const X: double): double;
    {$ENDIF}
    {$IFNDEF SEII_NO_EXT_MATH}
    class function CycleToDeg(const Cycles: double): double;
    {$ENDIF}
    class function CycleToGrad(const Cycles: double): double;
    class function CycleToRad(const Cycles: double): double;
    {$IFNDEF SEII_NO_EXT_MATH}
    class function DegToCycle(const Degrees: double): double;
    {$ENDIF}
    class function DegToGrad(const Degrees: double): double;
    class function DegToRad(const Degrees: double): double;
    class procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: Word);
    {$IFNDEF SEII_NO_EXT_MATH}
    class function DoubleDecliningBalance(const Cost, Salvage: double; Life, Period: Integer): double;
    {$ENDIF}
    class function EnsureRange0(const AValue, AMin, AMax: Int64): Int64;
    class function EnsureRange1(const AValue, AMin, AMax: Double): Double;
    class function Floor(const X: double): Integer;
    class procedure Frexp(const X: double; var Mantissa: double; var Exponent: Integer);
    {$IFNDEF SEII_NO_EXT_MATH}
    class function GradToCycle(const Grads: double): double;
    {$ENDIF}
    class function GradToDeg(const Grads: double): double;
    class function GradToRad(const Grads: double): double;
    class function Hypot(const X, Y: double): double;
    class function InRange0(const AValue, AMin, AMax: Int64): Boolean;
    class function InRange1(const AValue, AMin, AMax: Double): Boolean;
    class function IntPower(const Base: double; const Exponent: Integer): double; 
    class function IsInfinite(const AValue: Double): Boolean;
    class function IsNan0(const AValue: Single): Boolean;
    class function IsNan1(const AValue: Double): Boolean;
    class function IsZero0(const A: Single; Epsilon: Single): Boolean;
    class function IsZero1(const A: Single): Boolean;
    class function IsZero2(const A: Double; Epsilon: Double): Boolean;
    class function IsZero3(const A: Double): Boolean;
    class function Ldexp(const X: double; const P: Integer): double;
    class function LnXP1(const X: double): double;
    class function Log10(const X: double): double;
    class function Log2(const X: double): double;
    class function LogN(const Base, X: double): double;
    class function Max0(A,B: Int64): Int64;
    class function Max1(A,B: Single): Single;
    class function Max2(A,B: Double): Double;
    class function Min0(A,B: Int64): Int64;
    class function Min1(A,B: Single): Single;
    class function Min2(A,B: Double): Double;
    class function Power(const Base, Exponent: double): double;
    class function RadToCycle(const Radians: double): double;
    class function RadToDeg(const Radians: double): double;
    class function RadToGrad(const Radians: double): double;
    class function RandG(Mean, StdDev: double): double;
    class function RandomRange(const AFrom, ATo: Integer): Integer;
    class function RoundTo(const AValue: Double; const ADigit: TRoundToRange): Double;
    class function SameValue0(const A, B: Single; Epsilon: Single): Boolean;
    class function SameValue1(const A, B: Single): Boolean;
    class function SameValue2(const A, B: Double; Epsilon: Double): Boolean;
    class function SameValue3(const A, B: Double): Boolean;
    class function Sec(const X: double): double;
    {$IFNDEF SEII_NO_EXT_MATH}
    class function SecH(const X: double): double;
    {$ENDIF}
    class function Sign0(const AValue: Double): shortint;
    class function Sign1(const AValue: Integer): shortint;
    class function Sign2(const AValue: Int64): shortint;
    class procedure SinCos(const Theta: double; var Sin, Cos: double);
    class function Sin(X: double): double;
    class function Sinh(const X: double): double;
    {$IFNDEF SEII_NO_EXT_MATH}
    class function SLNDepreciation(const Cost, Salvage: double; Life: Integer): double;
    class function SYDDepreciation(const Cost, Salvage: double; Life, Period: Integer): double;
    {$ENDIF}
    class function Tan(const X: double): double;
    class function Tanh(const X: double): double;
    class function Exp(X: double): double;
    class function Abs0(X: int64): int64;
    class function Abs1(X: single): single;
    class function Abs2(X: double): double;
    class function Frac(X: double): double;
    class function Ln(X: double): double;
    class function Round(X: double): Int64;
    class function Sqr0(X: double): double;
    class function Sqr1(X: Integer): Integer;
    class function Sqrt(X: double): double;
    class function Trunc(X: double): Int64;

    class procedure Randomize;
    class function Random0: double;
    class function Random1(max: integer): integer;         
    class function RandSeed: integer;
    class procedure SetRandSeed(value: integer);
  end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
    Target.Method['Math.ArcCos[0]', C_UnitName] := @TMath.ArcCos;
    Target.Method['Math.ArcCosh[0]', C_UnitName] := @TMath.ArcCosh;
    {$IFNDEF SEII_NO_EXT_MATH}
    Target.Method['Math.ArcCot[0]', C_UnitName] := @TMath.ArcCot;
    Target.Method['Math.ArcCotH[0]', C_UnitName] := @TMath.ArcCotH;
    Target.Method['Math.ArcCsc[0]', C_UnitName] := @TMath.ArcCsc;
    Target.Method['Math.ArcCscH[0]', C_UnitName] := @TMath.ArcCscH;
    Target.Method['Math.ArcSec[0]', C_UnitName] := @TMath.ArcSec;
    Target.Method['Math.ArcSecH[0]', C_UnitName] := @TMath.ArcSecH;
    {$ENDIF}
    Target.Method['Math.ArcSin[0]', C_UnitName] := @TMath.ArcSin;
    Target.Method['Math.ArcSinh[0]', C_UnitName] := @TMath.ArcSinh;
    Target.Method['Math.ArcTan2[0]', C_UnitName] := @TMath.ArcTan2;
    Target.Method['Math.ArcTanh[0]', C_UnitName] := @TMath.ArcTanh;
    Target.Method['Math.Ceil[0]', C_UnitName] := @TMath.Ceil;
    Target.Method['Math.Cosecant[0]', C_UnitName] := @TMath.Cosecant;
    Target.Method['Math.Cosh[0]', C_UnitName] := @TMath.Cosh;
    Target.Method['Math.Cot[0]', C_UnitName] := @TMath.Cot;
    Target.Method['Math.Cos[0]', C_UnitName] := @TMath.Cos;
    Target.Method['Math.Cotan[0]', C_UnitName] := @TMath.Cotan;
    {$IFNDEF SEII_NO_EXT_MATH}
    Target.Method['Math.CotH[0]', C_UnitName] := @TMath.CotH;
    {$ENDIF}
    Target.Method['Math.Csc[0]', C_UnitName] := @TMath.Csc;
    {$IFNDEF SEII_NO_EXT_MATH}
    Target.Method['Math.CscH[0]', C_UnitName] := @TMath.CscH;
    {$ENDIF}
    {$IFNDEF SEII_NO_EXT_MATH}
    Target.Method['Math.CycleToDeg[0]', C_UnitName] := @TMath.CycleToDeg;
    {$ENDIF}
    Target.Method['Math.CycleToGrad[0]', C_UnitName] := @TMath.CycleToGrad;
    Target.Method['Math.CycleToRad[0]', C_UnitName] := @TMath.CycleToRad;
    {$IFNDEF SEII_NO_EXT_MATH}
    Target.Method['Math.DegToCycle[0]', C_UnitName] := @TMath.DegToCycle;
    {$ENDIF}
    Target.Method['Math.DegToGrad[0]', C_UnitName] := @TMath.DegToGrad;
    Target.Method['Math.DegToRad[0]', C_UnitName] := @TMath.DegToRad;
    Target.Method['Math.DivMod[0]', C_UnitName] := @TMath.DivMod;
    {$IFNDEF SEII_NO_EXT_MATH}
    Target.Method['Math.DoubleDecliningBalance[0]', C_UnitName] := @TMath.DoubleDecliningBalance;
    {$ENDIF}
    Target.Method['Math.EnsureRange[0]', C_UnitName] := @TMath.EnsureRange0;
    Target.Method['Math.EnsureRange[1]', C_UnitName] := @TMath.EnsureRange1;
    Target.Method['Math.Floor[0]', C_UnitName] := @TMath.Floor;
    Target.Method['Math.Frexp[0]', C_UnitName] := @TMath.Frexp;
    {$IFNDEF SEII_NO_EXT_MATH}
    Target.Method['Math.GradToCycle[0]', C_UnitName] := @TMath.GradToCycle;
    {$ENDIF}
    Target.Method['Math.GradToDeg[0]', C_UnitName] := @TMath.GradToDeg;
    Target.Method['Math.GradToRad[0]', C_UnitName] := @TMath.GradToRad;
    Target.Method['Math.Hypot[0]', C_UnitName] := @TMath.Hypot;
    Target.Method['Math.InRange[0]', C_UnitName] := @TMath.InRange0;
    Target.Method['Math.InRange[1]', C_UnitName] := @TMath.InRange1;
    Target.Method['Math.IntPower[0]', C_UnitName] := @TMath.IntPower;
    Target.Method['Math.IsInfinite[0]', C_UnitName] := @TMath.IsInfinite;
    Target.Method['Math.IsNan[0]', C_UnitName] := @TMath.IsNan0;
    Target.Method['Math.IsNan[1]', C_UnitName] := @TMath.IsNan1;
    Target.Method['Math.IsZero[0]', C_UnitName] := @TMath.IsZero0;
    Target.Method['Math.IsZero[1]', C_UnitName] := @TMath.IsZero1;
    Target.Method['Math.IsZero[2]', C_UnitName] := @TMath.IsZero2;
    Target.Method['Math.IsZero[3]', C_UnitName] := @TMath.IsZero3;
    Target.Method['Math.Ldexp[0]', C_UnitName] := @TMath.Ldexp;
    Target.Method['Math.LnXP1[0]', C_UnitName] := @TMath.LnXP1;
    Target.Method['Math.Log10[0]', C_UnitName] := @TMath.Log10;
    Target.Method['Math.Log2[0]', C_UnitName] := @TMath.Log2;
    Target.Method['Math.LogN[0]', C_UnitName] := @TMath.LogN;
    Target.Method['Math.Max[0]', C_UnitName] := @TMath.Max0;
    Target.Method['Math.Max[1]', C_UnitName] := @TMath.Max1;
    Target.Method['Math.Max[2]', C_UnitName] := @TMath.Max2;
    Target.Method['Math.Min[0]', C_UnitName] := @TMath.Min0;
    Target.Method['Math.Min[1]', C_UnitName] := @TMath.Min1;
    Target.Method['Math.Min[2]', C_UnitName] := @TMath.Min2;
    Target.Method['Math.Power[0]', C_UnitName] := @TMath.Power;
    Target.Method['Math.RadToCycle[0]', C_UnitName] := @TMath.RadToCycle;
    Target.Method['Math.RadToDeg[0]', C_UnitName] := @TMath.RadToDeg;
    Target.Method['Math.RadToGrad[0]', C_UnitName] := @TMath.RadToGrad;
    Target.Method['Math.RandG[0]', C_UnitName] := @TMath.RandG;
    Target.Method['Math.RandomRange[0]', C_UnitName] := @TMath.RandomRange;
    Target.Method['Math.RoundTo[0]', C_UnitName] := @TMath.RoundTo;
    Target.Method['Math.SameValue[0]', C_UnitName] := @TMath.SameValue0;
    Target.Method['Math.SameValue[1]', C_UnitName] := @TMath.SameValue1;
    Target.Method['Math.SameValue[2]', C_UnitName] := @TMath.SameValue2;
    Target.Method['Math.SameValue[3]', C_UnitName] := @TMath.SameValue3;
    Target.Method['Math.Sec[0]', C_UnitName] := @TMath.Sec;
    {$IFNDEF SEII_NO_EXT_MATH}
    Target.Method['Math.SecH[0]', C_UnitName] := @TMath.SecH;
    {$ENDIF}
    Target.Method['Math.Sign[0]', C_UnitName] := @TMath.Sign0;
    Target.Method['Math.Sign[1]', C_UnitName] := @TMath.Sign1;
    Target.Method['Math.Sign[2]', C_UnitName] := @TMath.Sign2;
    Target.Method['Math.SinCos[0]', C_UnitName] := @TMath.SinCos;
    Target.Method['Math.Sin[0]', C_UnitName] := @TMath.Sin;
    Target.Method['Math.Sinh[0]', C_UnitName] := @TMath.Sinh;
    {$IFNDEF SEII_NO_EXT_MATH}
    Target.Method['Math.SLNDepreciation[0]', C_UnitName] := @TMath.SLNDepreciation;
    Target.Method['Math.SYDDepreciation[0]', C_UnitName] := @TMath.SYDDepreciation;
    {$ENDIF}
    Target.Method['Math.Tan[0]', C_UnitName] := @TMath.Tan;
    Target.Method['Math.Tanh[0]', C_UnitName] := @TMath.Tanh;
    Target.Method['Math.Exp[0]', C_UnitName] := @TMath.Exp;
    Target.Method['Math.Abs[0]', C_UnitName] := @TMath.Abs0;
    Target.Method['Math.Abs[1]', C_UnitName] := @TMath.Abs1;
    Target.Method['Math.Abs[2]', C_UnitName] := @TMath.Abs2;
    Target.Method['Math.Frac[0]', C_UnitName] := @TMath.Frac;
    Target.Method['Math.Ln[0]', C_UnitName] := @TMath.Ln;
    Target.Method['Math.Round[0]', C_UnitName] := @TMath.Round;
    Target.Method['Math.Sqr[0]', C_UnitName] := @TMath.Sqr0;
    Target.Method['Math.Sqr[1]', C_UnitName] := @TMath.Sqr1;
    Target.Method['Math.Sqrt[0]', C_UnitName] := @TMath.Sqrt;
    Target.Method['Math.Trunc[0]', C_UnitName] := @TMath.Trunc;
    Target.Method['Math.Randomize[0]', C_UnitName] := @TMath.Randomize;
    Target.Method['Math.Random[0]', C_UnitName] := @TMath.Random0;
    Target.Method['Math.Random[1]', C_UnitName] := @TMath.Random1;
    Target.Method['Math.RandSeed[0]', C_UnitName] := @TMath.RandSeed;
    Target.Method['Math.SetRandSeed[0]', C_UnitName] := @TMath.SetRandSeed;
  end;
end;

procedure RegisterUnit;
var p : TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.Priority          := 11;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

{ TMath }

class function TMath.Abs0(X: int64): int64;
begin
  result := System.Abs(x);
end;

class function TMath.Abs1(X: single): single;
begin
  result := System.Abs(x);
end;

class function TMath.Abs2(X: double): double;
begin
  result := System.Abs(x);
end;

class function TMath.ArcCos(const X: double): double;
begin
  result := Math.ArcCos(x);
end;

class function TMath.ArcCosh(const X: double): double;
begin
  result := Math.ArcCosh(x)
end;

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.ArcCot(const X: double): double;
begin
  result := Math.ArcCot(x);
end;
{$ENDIF}

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.ArcCotH(const X: double): double;
begin
  result := Math.ArcCotH(x);
end;
{$ENDIF}

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.ArcCsc(const X: double): double;
begin

  result := Math.ArcCsc(x);
end;
{$ENDIF}

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.ArcCscH(const X: double): double;
begin
  result := Math.ArcCscH(x);
end;
{$ENDIF}

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.ArcSec(const X: double): double;
begin
  result := Math.ArcSec(x);
end;
{$ENDIF}

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.ArcSecH(const X: double): double;
begin
  result := Math.ArcSecH(x);
end;
{$ENDIF}

class function TMath.ArcSin(const X: double): double;
begin
  result := Math.ArcSin(x);
end;

class function TMath.ArcSinh(X: double): double;
begin
  result := Math.ArcSinH(x);
end;

class function TMath.ArcTan2(const Y, X: double): double;
begin
  result := Math.ArcTan2(x, y);
end;

class function TMath.ArcTanh(X: double): double;
begin
  result := Math.ArcTanh(x);
end;

class function TMath.Ceil(const X: double): integer;
begin
  result := Math.Ceil(x);
end;

class function TMath.Cos(const X: double): double;
begin
  result := System.Cos(x);
end;

class function TMath.Cosecant(const X: double): double;
begin
  result := Math.Cosecant(x);
end;

class function TMath.Cosh(const X: double): double;
begin
  result := Math.Cosh(x);
end;

class function TMath.Cot(const X: double): double;
begin
  result := Math.Cot(x);
end;

class function TMath.Cotan(const X: double): double;
begin
  result := Math.Cotan(x);
end;

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.CotH(const X: double): double;
begin
  result := Math.Coth(x);
end;
{$ENDIF}

class function TMath.Csc(const X: double): double;
begin
  result := Math.Csc(x);
end;

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.CscH(const X: double): double;
begin
  result := Math.CscH(x);
end;
{$ENDIF}

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.CycleToDeg(const Cycles: double): double;
begin
  result := Math.CycleToDeg(Cycles);
end;
{$ENDIF}

class function TMath.CycleToGrad(const Cycles: double): double;
begin
  result := Math.CycleToRad(Cycles);
end;

class function TMath.CycleToRad(const Cycles: double): double;
begin
  result := Math.CycleToRad(Cycles);
end;

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.DegToCycle(const Degrees: double): double;
begin
  result := Math.DegToCycle(Degrees);
end;
{$ENDIF}

class function TMath.DegToGrad(const Degrees: double): double;
begin
  result := Math.DegToGrad(Degrees);
end;

class function TMath.DegToRad(const Degrees: double): double;
begin
  result := Math.DegToRad(Degrees);
end;

class procedure TMath.DivMod(Dividend: Integer; Divisor: Word; var Result,
  Remainder: Word);
begin
  Math.DivMod(Dividend, Divisor, Result, Remainder);
end;

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.DoubleDecliningBalance(const Cost, Salvage: double;
  Life, Period: Integer): double;
begin
  result := Math.DoubleDecliningBalance(Cost, Salvage, Life, Period)
end;
{$ENDIF}

class function TMath.EnsureRange0(const AValue, AMin, AMax: Int64): Int64;
begin
  result := Math.EnsureRange(AValue, AMin, AMax);
end;

class function TMath.EnsureRange1(const AValue, AMin,
  AMax: Double): Double;
begin
  result := Math.EnsureRange(AValue, AMin, AMax);
end;

class function TMath.Exp(X: double): double;
begin
  result := System.Exp(x);
end;

class function TMath.Floor(const X: double): Integer;
begin
  result := Math.Floor(x);
end;

class function TMath.Frac(X: double): double;
begin
  result := System.Frac(x);
end;

class procedure TMath.Frexp(const X: double; var Mantissa: double;
  var Exponent: Integer);
var c : extended;
begin
  c := Mantissa;
  Math.Frexp(x, c, Exponent);
  Mantissa := c;
end;

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.GradToCycle(const Grads: double): double;
begin
  result := Math.GradToCycle(Grads);
end;
{$ENDIF}

class function TMath.GradToDeg(const Grads: double): double;
begin
  result := Math.GradToDeg(Grads);
end;

class function TMath.GradToRad(const Grads: double): double;
begin
  result := Math.GradToRad(Grads);
end;

class function TMath.Hypot(const X, Y: double): double;
begin
  result := Math.Hypot(x, y);
end;

class function TMath.InRange0(const AValue, AMin, AMax: Int64): Boolean;
begin
  result := Math.InRange(AValue, AMin, AMax);
end;

class function TMath.InRange1(const AValue, AMin, AMax: Double): Boolean;
begin
  result := Math.InRange(AValue, AMin, AMax)
end;

class function TMath.IntPower(const Base: double;
  const Exponent: Integer): double;
begin
  result := Math.IntPower(Base, Exponent);
end;

class function TMath.IsInfinite(const AValue: Double): Boolean;
begin
  result := Math.IsInfinite(AValue);
end;

class function TMath.IsNan0(const AValue: Single): Boolean;
begin
  result := Math.IsNan(AValue);
end;

class function TMath.IsNan1(const AValue: Double): Boolean;
begin
  result := Math.IsNan(AValue);
end;

class function TMath.IsZero0(const A: Single; Epsilon: Single): Boolean;
begin
  result := Math.IsZero(A, Epsilon);
end;

class function TMath.IsZero1(const A: Single): Boolean;
begin
  result := Math.IsZero(A)
end;

class function TMath.IsZero2(const A: Double; Epsilon: Double): Boolean;
begin
  result := Math.IsZero(A, Epsilon);
end;

class function TMath.IsZero3(const A: Double): Boolean;
begin
  result := Math.IsZero(A);
end;

class function TMath.Ldexp(const X: double; const P: Integer): double;
begin
  result := Math.Ldexp(x, P);
end;

class function TMath.Ln(X: double): double;
begin
  result := System.Ln(x);
end;

class function TMath.LnXP1(const X: double): double;
begin
  result := Math.LnXP1(x);
end;

class function TMath.Log10(const X: double): double;
begin
  result := Math.Log10(x);
end;

class function TMath.Log2(const X: double): double;
begin
  result := Math.Log2(x);
end;

class function TMath.LogN(const Base, X: double): double;
begin
  result := Math.LogN(Base, X);
end;

class function TMath.Max0(A, B: Int64): Int64;
begin
  result := Math.Max(A, b);
end;

class function TMath.Max1(A, B: Single): Single;
begin
  result := Math.Max(A, b);
end;

class function TMath.Max2(A, B: Double): Double;
begin
  result := Math.Max(A, b);
end;

class function TMath.Min0(A, B: Int64): Int64;
begin
  result := Math.Min(A, b);
end;

class function TMath.Min1(A, B: Single): Single;
begin
  result := Math.Min(A, b);
end;

class function TMath.Min2(A, B: Double): Double;
begin
  result := Math.Min(A, b);
end;

class function TMath.Power(const Base, Exponent: double): double;
begin      
  result := Math.Power(Base, Exponent);
end;

class function TMath.RadToCycle(const Radians: double): double;
begin
  result := Math.RadToCycle(Radians);
end;

class function TMath.RadToDeg(const Radians: double): double;
begin
  result := Math.RadToDeg(Radians);
end;

class function TMath.RadToGrad(const Radians: double): double;
begin
  result := Math.RadToGrad(Radians);
end;

class function TMath.RandG(Mean, StdDev: double): double;
begin
  result := Math.RandG(Mean, StdDev);
end;

class function TMath.Random0: double;
begin
  result := System.Random;
end;

class function TMath.Random1(max: integer): integer;
begin
  result := System.Random(max);
end;

class procedure TMath.Randomize;
begin
  System.Randomize;
end;

class function TMath.RandomRange(const AFrom, ATo: Integer): Integer;
begin
  {$IFDEF SEII_NO_EXT_MATH}
  result := System.Random(ATo - AFrom) + AFrom;
  {$ELSE}
  result := Math.RandomRange(AFrom, ATo);
  {$ENDIF}
end;

class function TMath.RandSeed: integer;
begin
  result := System.RandSeed;
end;

class function TMath.Round(X: double): Int64;
begin
  result := System.Round(x);
end;

class function TMath.RoundTo(const AValue: Double;
  const ADigit: TRoundToRange): Double;
begin
  result := Math.RoundTo(AValue, ADigit);
end;

class function TMath.SameValue0(const A, B: Single;
  Epsilon: Single): Boolean;
begin
  result := Math.SameValue(A, b, Epsilon);
end;

class function TMath.SameValue1(const A, B: Single): Boolean;
begin
  result := Math.SameValue(A, b);
end;

class function TMath.SameValue2(const A, B: Double;
  Epsilon: Double): Boolean;
begin
  result := Math.SameValue(A, b, Epsilon);
end;

class function TMath.SameValue3(const A, B: Double): Boolean;
begin
  result := Math.SameValue(A, b);
end;

class function TMath.Sec(const X: double): double;
begin
  result := Math.Sec(x);
end;

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.SecH(const X: double): double;
begin
  result := Math.SecH(x);
end;
{$ENDIF}

class procedure TMath.SetRandSeed(value: integer);
begin
  System.RandSeed := value;
end;

class function TMath.Sign0(const AValue: Double): shortint;
begin
  result := Math.Sign(AValue);
end;

class function TMath.Sign1(const AValue: Integer): shortint;
begin
  result := Math.Sign(AValue);
end;

class function TMath.Sign2(const AValue: Int64): shortint;
begin
  result := Math.Sign(AValue);
end;

class function TMath.Sin(X: double): double;
begin
  result := System.Sin(x);
end;

class procedure TMath.SinCos(const Theta: double; var Sin, Cos: double);
var x, y: extended;
begin
  Math.SinCos(Theta, x, y);
  sin := x;
  cos := y;
end;

class function TMath.Sinh(const X: double): double;
begin
  result := Math.Sinh(x);
end;

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.SLNDepreciation(const Cost, Salvage: double;
  Life: Integer): double;
begin
  result := Math.SLNDepreciation(Cost, Salvage, Life);
end;
{$ENDIF}

class function TMath.Sqr0(X: double): double;
begin
  result := System.Sqr(x);
end;

class function TMath.Sqr1(X: Integer): Integer;
begin
  result := System.Sqr(x);
end;

class function TMath.Sqrt(X: double): double;
begin
  result := System.Sqrt(x);
end;

{$IFNDEF SEII_NO_EXT_MATH}
class function TMath.SYDDepreciation(const Cost, Salvage: double; Life,
  Period: Integer): double;
begin
  result := Math.SYDDepreciation(Cost, Salvage, Life, Period);
end;
{$ENDIF}

class function TMath.Tan(const X: double): double;
begin
  result := Math.Tan(x);
end;

class function TMath.Tanh(const X: double): double;
begin
  result := Math.Tanh(x);
end;

class function TMath.Trunc(X: double): Int64;
begin
  result := System.Trunc(x);
end;

initialization
  RegisterUnit;

end.
