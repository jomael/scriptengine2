unit uSE2IncHelpers;

{$INCLUDE ScriptEngine.inc}

interface

uses
  uSE2Types, uSE2BaseTypes, uSE2RunAccess, uSE2UnitManager;

implementation

const
  C_UnitName   = 'System';
  C_UnitSource =
          'unit System;' + #13#10 +
          'interface' + #13#10 +
          'type' + #13#10 +
          '  TDateTimeHelper = helper for TDateTime' + #13#10 +
          '  public' + #13#10 +
          '    function AddDays(value: double): TDateTime;' + #13#10 +
          '    function AddHours(value: double): TDateTime;' + #13#10 +
          '    function AddMinutes(value: double): TDateTime;' + #13#10 +
          '    function AddSeconds(value: double): TDateTime;' + #13#10 +
          '    function AddMilliseconds(value: double): TDateTime;' + #13#10 +
          '    function DayOfWeek: integer;' + #13#10 +
          '    function DayOfYear: integer;' + #13#10 +
          '    function DateToStr: string;' + #13#10 +
          '    function TimeToStr: string;' + #13#10 +
          '    function ToString: string; overload;' + #13#10 +
          '    function ToString(Format: string): string; overload;' + #13#10 +
          '    function Year: integer;' + #13#10 +
          '    function Month: integer;' + #13#10 +
          '    function Day: integer;' + #13#10 +
          '    function Hours: integer;' + #13#10 +
          '    function Minutes: integer;' + #13#10 +
          '    function Seconds: integer;' + #13#10 +
          '    function Milliseconds: integer;  ' + #13#10 +
          {$IFNDEF FPC}
          '    function DateTimeToUTCTime: TDateTime;' +#13#10+
          '    function UTCTimeToDateTime: TDateTime;' +#13#10+
          {$ENDIF}
          '  end;' + #13#10 +
          '  TStringHelper = helper for string' + #13#10 +
          '  protected' + #13#10 +
          '    function GetChar(index: integer): string;' + #13#10 +
          '  public' + #13#10 +
          '    function Copy(Start, Count: integer): string;' + #13#10 +
          '    function Insert(Position: integer; value: string): string;' + #13#10 +
          '    function IndexOf(value: string): integer; overload;' + #13#10 +
          '    function IndexOf(value: string; SearchStart: integer): integer; overload;' + #13#10 +
          '    function Length: integer;' + #13#10 +
          '    function Replace(OldValue, NewValue: string): string; overload;' + #13#10 +
          '    function Replace(OldValue, NewValue: string; Flags: TReplaceFlags): string; overload; ' + #13#10 +
          '    function ToUpper: string;' + #13#10 +
          '    function ToLower: string;' + #13#10 +
          '    function Trim: string;' + #13#10 +
          '    function TrimLeft: string;' + #13#10 +
          '    function TrimRight: string; ' + #13#10 +
          '    function EndsWith(const value: string): boolean; overload;' + #13#10 +
          '    function EndsWith(const value: string; CaseSensitive: boolean): boolean; overload;' + #13#10 +
          '    function StartsWith(const value: string): boolean; overload;' + #13#10 +
          '    function StartsWith(const value: string; CaseSensitive: boolean): boolean; overload;' + #13#10 +
          '    property Chars[index: integer]: string read GetChar;' + #13#10 +
          '  end;' + #13#10 +
          '  TWideStringHelper = helper for WideString' + #13#10 +
          '  protected' + #13#10 +
          '    function GetChar(index: integer): WideString;' + #13#10 +
          '  public' + #13#10 +
          '    function Copy(Start, Count: integer): WideString;' + #13#10 +
          '    function Insert(Position: integer; value: WideString): WideString;' + #13#10 +
          '    function IndexOf(value: WideString): integer; ' + #13#10 +
          '    function Length: integer;' + #13#10 + 
          '    function ToUpper: WideString;' + #13#10 +
          '    function ToLower: WideString;' + #13#10 +
          '    function Trim: WideString;' + #13#10 +
          '    function TrimLeft: WideString;' + #13#10 +
          '    function TrimRight: WideString; ' + #13#10 +              
          '    function EndsWith(const value: WideString): boolean; overload;' + #13#10 +
          '    function EndsWith(const value: WideString; CaseSensitive: boolean): boolean; overload;' + #13#10 +
          '    function StartsWith(const value: WideString): boolean; overload;' + #13#10 +
          '    function StartsWith(const value: WideString; CaseSensitive: boolean): boolean; overload;' + #13#10 +
          '    property Chars[index: integer]: WideString read GetChar;   ' + #13#10 +
          '  end;' + #13#10 +
          '  TPointerHelper = helper for pointer' + #13#10 +
          '  public' + #13#10 +
          '    function Assigned: boolean;' + #13#10 +
          '  end;' + #13#10 +
          '  TDoubleHelper = helper for double' + #13#10 +
          '  public' + #13#10 +
          '    function IsNan: boolean;' + #13#10 +
          '    function IsInfinite: boolean;' + #13#10 +
          '    function Round: int64;' + #13#10 +
          '    function RoundTo(digits: integer): double;' + #13#10 +
          '    function Trunc: int64;' + #13#10 +
          '    function Floor: integer;' + #13#10 +
          '  end;' + #13#10 +
          '  TSingleHelper = helper for single' + #13#10 +
          '  public' + #13#10 +
          '    function IsNan: boolean;' + #13#10 +
          '    function IsInfinite: boolean;' + #13#10 +  
          '    function Round: int64;' + #13#10 +
          '    function RoundTo(digits: integer): double;' + #13#10 +
          '    function Trunc: int64;' + #13#10 +
          '    function Floor: integer;' + #13#10 +
          '  end;' + #13#10 +
          '  TColorHelper = helper for TColor' + #13#10 +
          '  public' + #13#10 +
          '    function Red: byte;' + #13#10 +
          '    function Green: byte;' + #13#10 +
          '    function Blue: byte;' + #13#10 +  
          '    function ToGrayscale: TColor;' + #13#10 +
          '  end;' + #13#10 +
          'implementation' + #13#10 +
          'function TDateTimeHelper.Year: integer;' + #13#10 +
          'var y, m, d: word;' + #13#10 +
          'begin' + #13#10 +
          '  DateTime.DecodeDate(Self, y, m, d);' + #13#10 +
          '  result := y;' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.Month: integer;' + #13#10 +
          'var y, m, d: word;' + #13#10 +
          'begin' + #13#10 +
          '  DateTime.DecodeDate(Self, y, m, d);' + #13#10 +
          '  result := m;' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.Day: integer;' + #13#10 +
          'var y, m, d: word;' + #13#10 +
          'begin' + #13#10 +
          '  DateTime.DecodeDate(Self, y, m, d);' + #13#10 +
          '  result := d;' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.Hours: integer;' + #13#10 +
          'var h, m, s, ms: word;' + #13#10 +
          'begin' + #13#10 +
          '  DateTime.DecodeTime(Self, h, m, s, ms);' + #13#10 +
          '  result := h;' + #13#10 +
          'end;    ' + #13#10 +
          'function TDateTimeHelper.Minutes: integer;' + #13#10 +
          'var h, m, s, ms: word;' + #13#10 +
          'begin' + #13#10 +
          '  DateTime.DecodeTime(Self, h, m, s, ms);' + #13#10 +
          '  result := m;' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.Seconds: integer;' + #13#10 +
          'var h, m, s, ms: word;' + #13#10 +
          'begin' + #13#10 +
          '  DateTime.DecodeTime(Self, h, m, s, ms);' + #13#10 +
          '  result := s;' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.Milliseconds: integer;' + #13#10 +
          'var h, m, s, ms: word;' + #13#10 +
          'begin' + #13#10 +
          '  DateTime.DecodeTime(Self, h, m, s, ms);' + #13#10 +
          '  result := ms;' + #13#10 +
          'end;' + #13#10 +

          {$IFNDEF FPC}
          'function TDateTimeHelper.DateTimeToUTCTime: TDateTime;' +#13#10+
          'begin'+#13#10+
          '  result := DateTime.DateTimeToUTCTime(Self);'+#13#10+
          'end;'+#13#10+
          'function TDateTimeHelper.UTCTimeToDateTime: TDateTime;' +#13#10+     
          'begin'+#13#10+
          '  result := DateTime.UTCTimeToDateTime(Self);'+#13#10+
          'end;'+#13#10+
          {$ENDIF}

          'function TDateTimeHelper.AddMilliseconds(value: double): TDateTime;' + #13#10 +
          'begin' + #13#10 +
          '  result := Self + (value / (24.0 * 60.0 * 60.0 * 1000.0));' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.AddSeconds(value: double): TDateTime;' + #13#10 +
          'begin' + #13#10 +
          '  result := Self + (value / (24.0 * 60.0 * 60.0));' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.AddMinutes(value: double): TDateTime;' + #13#10 +
          'begin' + #13#10 +
          '  result := Self + (value / (24.0 * 60.0));' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.AddHours(value: double): TDateTime;' + #13#10 +
          'begin' + #13#10 +
          '  result := Self + (value / (24.0));' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.AddDays(value: double): TDateTime;' + #13#10 +
          'begin' + #13#10 +
          '  result := Self + value;' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.DayOfWeek: integer;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.DayOfWeek(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.DayOfYear: integer;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.DayOfTheYear(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.DateToStr: string;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.DateToStr(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.TimeToStr: string;   ' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.TimeToStr(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.ToString: string;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.DateTimeToStr(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.ToString(Format: string): string; ' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.FormatDateTime(Format, Self);' + #13#10 +
          'end;' + #13#10 +
          'function TStringHelper.Insert(Position: integer; value: string): string;' + #13#10 +
          'begin' + #13#10 +
          '  Strings.Insert(value, Self, Position);' + #13#10 +
          '  result := Self;' + #13#10 +
          'end;' + #13#10 +
          'function TStringHelper.GetChar(index: integer): string;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.Copy(Self, index, 1);' + #13#10 +
          'end;' + #13#10 +
          'function TStringHelper.ToUpper: string;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.UpperCase(Self);' + #13#10 +
          'end;    ' + #13#10 +
          'function TStringHelper.ToLower: string;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.LowerCase(Self);' + #13#10 +
          'end;    ' + #13#10 +
          'function TStringHelper.Length: integer;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.Length(Self);' + #13#10 +
          'end;    ' + #13#10 +
          'function TStringHelper.Replace(OldValue, NewValue: string): string;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.Replace(Self, OldValue, NewValue, [rfReplaceAll, rfIgnoreCase]);' + #13#10 +
          'end;      ' + #13#10 +
          'function TStringHelper.Replace(OldValue, NewValue: string; Flags: TReplaceFlags): string;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.Replace(Self, OldValue, NewValue, Flags);' + #13#10 +
          'end;' + #13#10 +
          'function TStringHelper.Copy(Start, Count: integer): string;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.Copy(Self, Start, Count);' + #13#10 +
          'end;' + #13#10 +
          'function TStringHelper.IndexOf(value: string): integer;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.Pos(value, Self);' + #13#10 +
          'end;        ' + #13#10 +
          'function TStringHelper.IndexOf(value: string; SearchStart: integer): integer;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.PosEx(value, Self, SearchStart);' + #13#10 +
          'end;         ' + #13#10 +
          'function TStringHelper.TrimLeft: string;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.TrimLeft(Self);' + #13#10 +
          'end;       ' + #13#10 +
          'function TStringHelper.TrimRight: string;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.TrimRight(Self);' + #13#10 +
          'end;  ' + #13#10 +
          'function TStringHelper.Trim: string;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.Trim(Self);' + #13#10 +
          'end;' + #13#10 +

          'function TStringHelper.EndsWith(const value: string): boolean;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.CompareStr(Self.Copy(Self.Length - value.Length + 1, value.Length), value) = 0;' + #13#10 +
          'end;' + #13#10 +
          'function TStringHelper.EndsWith(const value: string; CaseSensitive: boolean): boolean;' + #13#10 +
          'var s: string;' + #13#10 +
          'begin' + #13#10 +
          '  s := Self.Copy(Self.Length - value.Length + 1, value.Length);' + #13#10 +
          '  if CaseSensitive then' + #13#10 +
          '     result := Strings.CompareStr(s, value) = 0' + #13#10 +
          '  else' + #13#10 +
          '     result := Strings.SameText(s, value);' + #13#10 +
          'end;' + #13#10 +
          'function TStringHelper.StartsWith(const value: string): boolean;' + #13#10 +
          'begin' + #13#10 +
          '  result := Strings.SameText(Self.Copy(1, value.Length), value);' + #13#10 +
          'end;' + #13#10 +
          'function TStringHelper.StartsWith(const value: string; CaseSensitive: boolean): boolean;' + #13#10 +
          'var s: string;' + #13#10 +
          'begin' + #13#10 +
          '  s := Self.Copy(1, value.Length);' + #13#10 +
          '  if CaseSensitive then' + #13#10 +
          '     result := Strings.CompareStr(s, value) = 0' + #13#10 +
          '  else' + #13#10 +
          '     result := Strings.SameText(s, value);' + #13#10 +
          'end;' + #13#10 +

          'function TWideStringHelper.Insert(Position: integer; value: WideString): WideString;' + #13#10 +
          'begin' + #13#10 +
          '  WideStrings.Insert(value, Self, Position);' + #13#10 +
          '  result := Self;' + #13#10 +
          'end;' + #13#10 +
          'function TWideStringHelper.GetChar(index: integer): WideString;' + #13#10 +
          'begin' + #13#10 +
          '  result := WideStrings.Copy(Self, index, 1);' + #13#10 +
          'end;' + #13#10 +
          'function TWideStringHelper.ToUpper: WideString;' + #13#10 +
          'begin' + #13#10 +
          '  result := WideStrings.UpperCase(Self);' + #13#10 +
          'end;    ' + #13#10 +
          'function TWideStringHelper.ToLower: WideString;' + #13#10 +
          'begin' + #13#10 +
          '  result := WideStrings.LowerCase(Self);' + #13#10 +
          'end;        ' + #13#10 +
          'function TWideStringHelper.Copy(Start, Count: integer): WideString;' + #13#10 +
          'begin' + #13#10 +
          '  result := WideStrings.Copy(Self, Start, Count);' + #13#10 +
          'end;' + #13#10 +
          'function TWideStringHelper.IndexOf(value: WideString): integer;' + #13#10 +
          'begin' + #13#10 +
          '  result := WideStrings.Pos(value, Self);' + #13#10 +
          'end;        ' + #13#10 +
          'function TWideStringHelper.TrimLeft: WideString;' + #13#10 +
          'begin' + #13#10 +
          '  result := WideStrings.TrimLeft(Self);' + #13#10 +
          'end;       ' + #13#10 +
          'function TWideStringHelper.TrimRight: WideString;' + #13#10 +
          'begin' + #13#10 +
          '  result := WideStrings.TrimRight(Self);' + #13#10 +
          'end;  ' + #13#10 +
          'function TWideStringHelper.Trim: WideString;' + #13#10 +
          'begin' + #13#10 +
          '  result := WideStrings.Trim(Self);' + #13#10 +
          'end;' + #13#10 +

         // 'function TWideStringHelper.EndsWith(const value: WideString): boolean;' + #13#10 +


          'function TWideStringHelper.EndsWith(const value: WideString): boolean;' + #13#10 +
          'begin' + #13#10 +
          '  result := WideStrings.SameText(Self.Copy(Self.Length - value.Length + 1, value.Length), value) = 0;' + #13#10 +
          'end;' + #13#10 +
          'function TWideStringHelper.EndsWith(const value: WideString; CaseSensitive: boolean): boolean;' + #13#10 +
          'var s: string;' + #13#10 +
          'begin' + #13#10 +
          '  s := Self.Copy(Self.Length - value.Length + 1, value.Length);' + #13#10 +
          '  if CaseSensitive then' + #13#10 +
          '     result := WideStrings.SameStr(s, value)' + #13#10 +
          '  else' + #13#10 +
          '     result := WideStrings.SameText(s, value);' + #13#10 +
          'end;' + #13#10 +
          'function TWideStringHelper.StartsWith(const value: WideString): boolean;' + #13#10 +
          'begin' + #13#10 +
          '  result := WideStrings.SameText(Self.Copy(1, value.Length), value);' + #13#10 +
          'end;' + #13#10 +
          'function TWideStringHelper.StartsWith(const value: WideString; CaseSensitive: boolean): boolean;' + #13#10 +
          'var s: string;' + #13#10 +
          'begin' + #13#10 +
          '  s := Self.Copy(1, value.Length);' + #13#10 +
          '  if CaseSensitive then' + #13#10 +
          '     result := WideStrings.SameStr(s, value)' + #13#10 +
          '  else' + #13#10 +
          '     result := WideStrings.SameText(s, value);' + #13#10 +
          'end;' + #13#10 +
          'function TWideStringHelper.Length: integer;'+#13#10+
          'begin'+#13#10+
          '  result := WideStrings.Length(Self);'+#13#10+
          'end;'+#13#10+    
          'function TPointerHelper.Assigned: boolean;' + #13#10 +
          'begin' + #13#10 +
          '  result := Self <> nil;' + #13#10 +
          'end;' + #13#10 +
          'function TDoubleHelper.IsNan: boolean;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.IsNan(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TDoubleHelper.IsInfinite: boolean;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.IsInfinite(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TDoubleHelper.Round: int64;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.Round(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TDoubleHelper.RoundTo(digits: integer): double;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.RoundTo(Self, Digits);' + #13#10 +
          'end;' + #13#10 +
          'function TDoubleHelper.Trunc: int64;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.Trunc(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TDoubleHelper.Floor: integer;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.Floor(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TSingleHelper.IsNan: boolean;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.IsNan(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TSingleHelper.IsInfinite: boolean;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.IsInfinite(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TSingleHelper.Round: int64;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.Round(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TSingleHelper.RoundTo(digits: integer): double;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.RoundTo(Self, Digits);' + #13#10 +
          'end;' + #13#10 +
          'function TSingleHelper.Trunc: int64;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.Trunc(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TSingleHelper.Floor: integer;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.Floor(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TColorHelper.Red: byte;' + #13#10 +
          'begin' + #13#10 +
          '  result := Colors.RedValue(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TColorHelper.Green: byte;' + #13#10 +
          'begin' + #13#10 +
          '  result := Colors.GreenValue(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TColorHelper.Blue: byte;' + #13#10 +
          'begin' + #13#10 +
          '  result := Colors.BlueValue(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TColorHelper.ToGrayscale: TColor;' + #13#10 +
          'begin' + #13#10 +
          '  result := Math.Round(Self.Red * 0.56 + Self.Green * 0.33 + Self.Blue * 0.11);' + #13#10 +
          '  result := Colors.RGB(result, result, result);' + #13#10 + 
          'end;' + #13#10 +
          'end.';



{ =================================== }

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
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
