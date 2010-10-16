unit uSE2IncTimeSpan;

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
        '  /// Represents a time difference'+#13#10+
        '  TTimeSpan = record' + #13#10 + 
        '  private' + #13#10 + 
        '    FSpan : double;' + #13#10 + 
        '  protected' + #13#10 + 
        '    function GetDays: integer;' + #13#10 + 
        '    function GetHours: integer;' + #13#10 + 
        '    function GetMinutes: integer;' + #13#10 + 
        '    function GetSeconds: integer;' + #13#10 + 
        '    function GetMilliseconds: integer;' + #13#10 + 
        #13#10 + 
        '    function GetTotalMilliseconds: double;' + #13#10 + 
        '    function GetTotalSeconds: double;' + #13#10 + 
        '    function GetTotalMinutes: double;' + #13#10 + 
        '    function GetTotalHours: double;' + #13#10 + 
        '    function GetTotalDays: double;' + #13#10 + 
        '  public' + #13#10 +
        '    /// Total number of hours in one day'+#13#10+
        '    const HoursPerDay   : integer = 24;' + #13#10 +
        '    /// Total number of minutes in one day'+#13#10+
        '    const MinutesPerDay : integer = 1440; // 24 * 60' + #13#10 +
        '    /// Total number of seconds in one day'+#13#10+
        '    const SecondsPerDay : integer = 86400; // 24 * 60 * 60' + #13#10 +
        '    /// Total number of milliseconds in one day'+#13#10+
        '    const MillSecPerDay : integer = 86400000; // 24 * 60 * 60 * 1000' + #13#10 + 
        #13#10 +
        '    function ToString: string; overload;' + #13#10 +
        '    /// Convert the current time span to a string ([days].[hours]:[minutes]:[seconds].[milliseconds]'+#13#10+
        '    function ToString(ShowMilliseconds: boolean): string; overload;' + #13#10 + 
        #13#10 +
        '    /// Add an offset to the current value'+#13#10+
        '    function Add(TimeSpan: TTimeSpan): TTimeSpan;' + #13#10 +
        '    /// Substract an offset from the current value'+#13#10+
        '    function Substract(TimeSpan: TTimeSpan): TTimeSpan;' + #13#10 +
        '    /// Negate the current time span'+#13#10+
        '    function Negate: TTimeSpan;' + #13#10 +
        '    /// Returns the absolute duration of the current time span'+#13#10+
        '    function Duration: TTimeSpan;' + #13#10 + 
        #13#10 +
        '    /// Add a millisecond offset to the current time span'+#13#10+
        '    function AddMilliseconds(value: double): TTimeSpan;' + #13#10 +
        '    /// Add a second offset to the current time span'+#13#10+
        '    function AddSeconds(value: double): TTimeSpan;' + #13#10 +
        '    /// Add a minute offset to the current time span'+#13#10+
        '    function AddMinutes(value: double): TTimeSpan;' + #13#10 +
        '    /// Add a hour offset to the curren time span'+#13#10+
        '    function AddHours(value: double): TTimeSpan;' + #13#10 +
        '    /// Add a day offset to the current time span'+#13#10+
        '    function AddDays(value: double): TTimeSpan;' + #13#10 + 
        #13#10 +
        '    /// Get the time span between to time stamps'+#13#10+
        '    class function TimeBetween(d1, d2: TDateTime): TTimeSpan;' + #13#10 +
        #13#10 +
        '    class function TimeSpan(hours, minutes, seconds: integer): TTimeSpan; overload;' + #13#10 +
        '    class function TimeSpan(days, hours, minutes, seconds: integer): TTimeSpan; overload;' + #13#10 +
        '    /// Create a new time span value'+#13#10+
        '    class function TimeSpan(days, hours, minutes, seconds, milliseconds: integer): TTimeSpan; overload;' + #13#10 + 
        #13#10 +
        '    /// Create a new time span and initiate it the the given number of days'+#13#10+
        '    class function FromDays(value: double): TTimeSpan;' + #13#10 +          
        '    /// Create a new time span and initiate it the the given number of hours'+#13#10+
        '    class function FromHours(value: double): TTimeSpan;' + #13#10 +        
        '    /// Create a new time span and initiate it the the given number of minutes'+#13#10+
        '    class function FromMinutes(value: double): TTimeSpan;' + #13#10 +     
        '    /// Create a new time span and initiate it the the given number of seconds'+#13#10+
        '    class function FromSeconds(value: double): TTimeSpan;' + #13#10 +   
        '    /// Create a new time span and initiate it the the given number of milliseconds'+#13#10+
        '    class function FromMilliseconds(value: double): TTimeSpan;' + #13#10 +  
        '    /// Create a new time span and initiate it the the given duration in the string'+#13#10+
        '    class function FromString(value: string): TTimeSpan;' + #13#10 + 
        #13#10 +

        '    /// Absolute days within the time span'+#13#10+
        '    property TotalDays    : double  read GetTotalDays;' + #13#10 +
        '    /// Absolute hours within the time span'+#13#10+
        '    property TotalHours   : double  read GetTotalHours;' + #13#10 +   
        '    /// Absolute hours minutes the time span'+#13#10+
        '    property TotalMinutes : double  read GetTotalMinutes;' + #13#10 +     
        '    /// Absolute hours seconds the time span'+#13#10+
        '    property TotalSeconds : double  read GetTotalSeconds;' + #13#10 +   
        '    /// Absolute hours milliseconds the time span'+#13#10+
        '    property TotalMilliseconds : double read GetTotalMilliseconds;' + #13#10 + 
        #13#10 +                             
        '    /// Completed days within the time span'+#13#10+
        '    property Days         : integer read GetDays;' + #13#10 +
        '    /// Completed hours within the time span'+#13#10+
        '    property Hours        : integer read GetHours;' + #13#10 +  
        '    /// Completed minutes within the time span'+#13#10+
        '    property Minutes      : integer read GetMinutes;' + #13#10 +    
        '    /// Completed seconds within the time span'+#13#10+
        '    property Seconds      : integer read GetSeconds;' + #13#10 +       
        '    /// Completed milliseconds within the time span'+#13#10+
        '    property Milliseconds : integer read GetMilliseconds;' + #13#10 + 
        #13#10 +                   
        '    /// Absolute time span ticks'+#13#10+
        '    property Value        : double  read FSpan write FSpan;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  TDateTimeTimeSpanHelper = helper for TDateTime' + #13#10 + 
        '  public' + #13#10 +
        '    function Substract(aTime: TDateTime): TTimeSpan;' + #13#10 + 
        '    function Add(aTime: TTimeSpan): TDateTime;' + #13#10 +
        '    function TimeBetween(aTime: TDateTime): TTimeSpan;' + #13#10 +
        '  end;' + #13#10 +
        #13#10 +
        '  Convert = partial class' + #13#10 + 
        '  public' + #13#10 + 
        '    class function ToTimeSpan(value: string): TTimeSpan; overload;' + #13#10 + 
        '    class function ToTimeSpan(value: TDateTime): TTimeSpan; overload;' + #13#10 + 
        '    class function ToString(value: TTimeSpan): string; overload;' + #13#10 + 
        '  end;' + #13#10 +
        #13#10 + 
        'implementation' + #13#10 +
        #13#10 +
        'class function Convert.ToTimeSpan(value: string): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := TTimeSpan.FromString(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToTimeSpan(value: TDateTime): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.Value := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: TTimeSpan): string;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value.ToString;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 +
        'function TDateTimeTimeSpanHelper.Substract(aTime: TDateTime): TTimeSpan;' + #13#10 +
        'begin' + #13#10 +
        '  result.FSpan := Self - aTime;' + #13#10 +
        'end;' + #13#10 +     
        #13#10 +
        'function TDateTimeTimeSpanHelper.TimeBetween(aTime: TDateTime): TTimeSpan;' + #13#10 +
        'begin' + #13#10 +
        '  result.FSpan := Self - aTime;' + #13#10 +
        'end;' + #13#10 +
        #13#10 + 
        'function TDateTimeTimeSpanHelper.Add(aTime: TTimeSpan): TDateTime;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self + aTime.FSpan;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TTimeSpan.TimeBetween(d1, d2: TDateTime): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := d1 - d2;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.Add(TimeSpan: TTimeSpan): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := Self.FSpan + TimeSpan.FSpan;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.Substract(TimeSpan: TTimeSpan): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := Self.FSpan - TimeSpan.FSpan;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.Negate: TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := -Self.FSpan;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.Duration: TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := Self.FSpan;' + #13#10 + 
        '  if result.FSpan < 0 then' + #13#10 + 
        '     result.FSpan := -result.FSpan;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.ToString: string;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.ToString(True);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.ToString(ShowMilliseconds: boolean): string;' + #13#10 + 
        'var d, h, m, s, ms: integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  d := Self.Days;' + #13#10 + 
        '  h := Self.Hours;' + #13#10 + 
        '  m := Self.Minutes;' + #13#10 + 
        '  s := Self.Seconds;' + #13#10 + 
        '  ms:= Self.Milliseconds;' + #13#10 + 
        #13#10 + 
        '  result := h.Abs.ToString(2) + '':'' +' + #13#10 +
        '            m.Abs.ToString(2) + '':'' +' + #13#10 +
        '            s.Abs.ToString(2);' + #13#10 +
        #13#10 + 
        '  if (ms <> 0) and ShowMilliseconds then' + #13#10 + 
        '     result := result + ''.'' + ms.Abs.ToString(3);' + #13#10 +
        #13#10 + 
        '  if d <> 0 then' + #13#10 + 
        '     result := Math.Abs(d).ToString + ''.'' + result;' + #13#10 + 
        #13#10 + 
        '  if Self.FSpan < 0 then' + #13#10 + 
        '     result := ''-'' + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.GetDays: integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Trunc(Math.Abs(Self.FSpan));' + #13#10 + 
        '  if Self.FSpan < 0 then' + #13#10 + 
        '     result := -result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.GetHours: integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := TDateTime(Math.Abs(Self.FSpan)).Hours;' + #13#10 + 
        '  if Self.FSpan < 0 then' + #13#10 + 
        '     result := -result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.GetMinutes: integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := TDateTime(Math.Abs(Self.FSpan)).Minutes;' + #13#10 + 
        '  if Self.FSpan < 0 then' + #13#10 + 
        '     result := -result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.GetSeconds: integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := TDateTime(Math.Abs(Self.FSpan)).Seconds;' + #13#10 + 
        '  if Self.FSpan < 0 then' + #13#10 + 
        '     result := -result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.GetMilliseconds: integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := TDateTime(Math.Abs(Self.FSpan)).Milliseconds;' + #13#10 + 
        '  if Self.FSpan < 0 then' + #13#10 + 
        '     result := -result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.GetTotalDays: double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.FSpan;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.GetTotalHours: double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.FSpan * TTimeSpan.HoursPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.GetTotalMinutes: double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.FSpan * TTimeSpan.MinutesPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.GetTotalSeconds: double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.FSpan * TTimeSpan.SecondsPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.GetTotalMilliseconds: double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.FSpan * TTimeSpan.MillSecPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TTimeSpan.TimeSpan(hours, minutes, seconds: integer): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := seconds / (TTimeSpan.SecondsPerDay) + minutes / (TTimeSpan.MinutesPerDay) + hours / TTimeSpan.HoursPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TTimeSpan.TimeSpan(days, hours, minutes, seconds: integer): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := seconds / (TTimeSpan.SecondsPerDay) + minutes / (TTimeSpan.MinutesPerDay) + hours / TTimeSpan.HoursPerDay + days;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TTimeSpan.TimeSpan(days, hours, minutes, seconds, milliseconds: integer): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := milliseconds / (TTimeSpan.MillSecPerDay) + seconds / (TTimeSpan.SecondsPerDay) + minutes / (TTimeSpan.MinutesPerDay) + hours / TTimeSpan.HoursPerDay + days;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TTimeSpan.FromDays(value: double): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TTimeSpan.FromHours(value: double): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := value / TTimeSpan.HoursPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TTimeSpan.FromMinutes(value: double): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := value / TTimeSpan.MinutesPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TTimeSpan.FromSeconds(value: double): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := value / TTimeSpan.SecondsPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TTimeSpan.FromMilliseconds(value: double): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := value / TTimeSpan.MillSecPerDay;' + #13#10 + 
        'end;' + #13#10 +
        #13#10 +
        'class function TTimeSpan.FromString(value: string): TTimeSpan;' + #13#10 + 
        'var d, h, m, s, ms: integer;' + #13#10 +
        '    iPos, sign : integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  //  [o]                       [o]' + #13#10 + 
        '  // [DAY].[HOUR]:[MIN]:[SEC].[MILLI]' + #13#10 + 
        #13#10 +
        '  sign := 1;' + #13#10 + 
        '  if value.StartsWith(''-'') then' + #13#10 + 
        '  begin' + #13#10 + 
        '    sign := -1;' + #13#10 + 
        '    value := value.SubString(2, value.Length);' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  // 1st value' + #13#10 + 
        '  iPos := value.IndexOf('':'');' + #13#10 + 
        '  if iPos = 0 then' + #13#10 + 
        '     exit;' + #13#10 + 
        #13#10 + 
        '  if (value.IndexOf(''.'') > 0) and' + #13#10 + 
        '     (value.IndexOf(''.'') < iPos) then' + #13#10 + 
        '  begin' + #13#10 + 
        '    d := Convert.ToInt32(value.SubString(1, value.IndexOf(''.'') - 1), 0);' + #13#10 +
        '    value := value.SubString(value.IndexOf(''.'') + 1, value.Length);' + #13#10 + 
        '    iPos  := value.IndexOf('':'');' + #13#10 + 
        '  end else' + #13#10 + 
        '    d := 0;' + #13#10 + 
        #13#10 + 
        '  h := Convert.ToInt32(value.SubString(1, iPos - 1), 0);' + #13#10 +
        '  value := value.SubString(iPos + 1, value.Length);' + #13#10 +
        #13#10 +
        '  // 2nd value' + #13#10 +
        '  iPos := value.IndexOf('':'');' + #13#10 + 
        '  if iPos = 0 then' + #13#10 + 
        '     exit;' + #13#10 +
        #13#10 + 
        '  m := Convert.ToInt32(value.SubString(1, iPos - 1), 0);' + #13#10 +
        '  value := value.SubString(iPos + 1, value.Length);' + #13#10 +
        #13#10 +
        '  // 3rd value' + #13#10 +
        '  iPos  := value.IndexOf(''.'');' + #13#10 + 
        '  if iPos > 0 then' + #13#10 + 
        '  begin' + #13#10 + 
        '    s := Convert.ToInt32(value.SubString(1, iPos - 1), 0);' + #13#10 + 
        '    value := value.SubString(iPos + 1, value.Length);' + #13#10 +
        #13#10 + 
        '    ms := Convert.ToInt32(value, 0);' + #13#10 + 
        '  end else' + #13#10 +
        '  begin'+#13#10+
        '    s  := Convert.ToInt32(value, 0);'+#13#10+
        '    ms := 0;' + #13#10 +
        '  end;'+#13#10+
        #13#10 + 
        '  result.FSpan := d +' + #13#10 + 
        '                  h / TTimeSpan.HoursPerDay +' + #13#10 + 
        '                  m / TTimeSpan.MinutesPerDay +' + #13#10 + 
        '                  s / TTimeSpan.SecondsPerDay +' + #13#10 + 
        '                  ms / TTimeSpan.MillSecPerDay;' + #13#10 + 
        #13#10 + 
        '  result.FSpan := result.FSpan * sign;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.AddMilliseconds(value: double): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := Self.FSpan + value / TTimeSpan.MillSecPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.AddSeconds(value: double): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := Self.FSpan + value / TTimeSpan.SecondsPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.AddMinutes(value: double): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := Self.FSpan + value / TTimeSpan.MinutesPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.AddHours(value: double): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := Self.FSpan + value / TTimeSpan.HoursPerDay;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TTimeSpan.AddDays(value: double): TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.FSpan := Self.FSpan + value;' + #13#10 + 
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
  p.Priority          := 1000;
  TSE2UnitManager.RegisterUnit(p);
end;
{$ENDIF}

{$IFNDEF SE2_IMPORT_AS_PACKAGE}
initialization
  RegisterUnit();
{$ENDIF}

end.
