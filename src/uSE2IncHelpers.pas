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
          #13#10 + 
          'interface' + #13#10 +
          #13#10 + 
          'type' + #13#10 + 
          '  TStringHelper = helper for string' + #13#10 + 
          '  protected' + #13#10 + 
          '    function GetChar(index: integer): string;' + #13#10 + 
          '  public' + #13#10 + 
          '    function Copy(Start, Count: integer): string; overload; // deprecated ''use .SubString instead'';' + #13#10 + 
          '    /// Copy a substring out of the string' + #13#10 + 
          '    function Copy(Start: integer): string; overload; // deprecated ''use .SubString instead'';' + #13#10 + 
          '    /// Insert the string "value" at "Position"' + #13#10 + 
          '    function Insert(Position: integer; value: string): string;' + #13#10 + 
          '    function IndexOf(value: string): integer; overload;' + #13#10 + 
          '    /// Get the first occurance of "value"' + #13#10 + 
          '    function IndexOf(value: string; SearchStart: integer): integer; overload;' + #13#10 + 
          '    /// Get the length of the string' + #13#10 + 
          '    function Length: integer;' + #13#10 + 
          '    function Replace(OldValue, NewValue: string): string; overload;' + #13#10 + 
          '    /// Replace a token with a new token' + #13#10 + 
          '    function Replace(OldValue, NewValue: string; Flags: TReplaceFlags): string; overload;' + #13#10 + 
          '    /// Convert every character to the corresponding upper case character if available' + #13#10 + 
          '    function ToUpper: string;' + #13#10 + 
          '    /// Convert every character to the corresponding lower case character if available' + #13#10 + 
          '    function ToLower: string;' + #13#10 + 
          '    function SubString(StartIndex: integer): string; overload;' + #13#10 + 
          '    /// Copy a substring out of the string' + #13#10 + 
          '    function SubString(StartIndex, Length: integer): string; overload;' + #13#10 + 
          '    /// Remove every control and white space character at the beginning and at the end of the string' + #13#10 + 
          '    function Trim: string;' + #13#10 + 
          '    /// Remove every control and white space character at the beginning of the string' + #13#10 + 
          '    function TrimLeft: string;' + #13#10 + 
          '    /// Remove every control and white space character at the end of the string' + #13#10 + 
          '    function TrimRight: string;' + #13#10 + 
          '    function EndsWith(const value: string): boolean; overload;' + #13#10 + 
          '    /// Returns true, if the last characters are equal to "value"' + #13#10 + 
          '    function EndsWith(const value: string; CaseSensitive: boolean): boolean; overload;' + #13#10 + 
          '    function StartsWith(const value: string): boolean; overload;' + #13#10 + 
          '    /// Returns true, if the first characters are equal to "value"' + #13#10 + 
          '    function StartsWith(const value: string; CaseSensitive: boolean): boolean; overload;' + #13#10 + 
          '    /// Returns the last occurance of "value" inside the string' + #13#10 + 
          '    function LastIndexOf(const value: string): integer;' + #13#10 + 
          '    /// Get the corresponding ascii-table index of the first character' + #13#10 + 
          '    function ToASCIIIndex: word;' + #13#10 + 
          '    property Chars[index: integer]: string read GetChar;' + #13#10 + 
          '  end;' + #13#10 + 
          '  TWideStringHelper = helper for WideString' + #13#10 + 
          '  protected' + #13#10 + 
          '    function GetChar(index: integer): WideString;' + #13#10 + 
          '  public' + #13#10 + 
          '    function Copy(Start, Count: integer): WideString; overload; // deprecated ''use .SubString instead'';' + #13#10 + 
          '    function Copy(Start: integer): WideString; overload; // deprecated ''use .SubString instead'';' + #13#10 + 
          '    function SubString(StartIndex: integer): WideString; overload;' + #13#10 + 
          '    function SubString(StartIndex, Length: integer): WideString; overload;' + #13#10 + 
          '    function Insert(Position: integer; value: WideString): WideString;' + #13#10 + 
          '    function IndexOf(value: WideString): integer;' + #13#10 + 
          '    function Length: integer;' + #13#10 + 
          '    function ToUpper: WideString;' + #13#10 + 
          '    function ToLower: WideString;' + #13#10 + 
          '    function Trim: WideString;' + #13#10 + 
          '    function TrimLeft: WideString;' + #13#10 + 
          '    function TrimRight: WideString;' + #13#10 + 
          '    function EndsWith(const value: WideString): boolean; overload;' + #13#10 + 
          '    function EndsWith(const value: WideString; CaseSensitive: boolean): boolean; overload;' + #13#10 + 
          '    function StartsWith(const value: WideString): boolean; overload;' + #13#10 + 
          '    function StartsWith(const value: WideString; CaseSensitive: boolean): boolean; overload;' + #13#10 + 
          '    property Chars[index: integer]: WideString read GetChar;' + #13#10 + 
          '  end;' + #13#10 + 
          '  TPointerHelper = helper for pointer' + #13#10 + 
          '  public' + #13#10 + 
          '    /// Check if the pointer points to any memory address' + #13#10 + 
          '    function Assigned: boolean;' + #13#10 + 
          '    /// Add an offset to the pointer' + #13#10 + 
          '    function AddOffset(const Offset: int64): Pointer;' + #13#10 + 
          '    /// Returns the offset between "Self" and "Target"' + #13#10 + 
          '    function OffsetTo(Target: Pointer): int64;' + #13#10 + 
          #13#10 + 
          '    function ToString: string; overload;' + #13#10 + 
          '    /// Convert the pointer to a string' + #13#10 + 
          '    function ToString(Prefix: string): string; overload;' + #13#10 + 
          '  end;' + #13#10 + 
          '  TDoubleHelper = helper for double' + #13#10 + 
          '  public' + #13#10 + 
          '    /// Returns true, if the value is not a number' + #13#10 + 
          '    function IsNan: boolean;' + #13#10 + 
          '    /// Returns true, if the value represents infinity' + #13#10 + 
          '    function IsInfinite: boolean;' + #13#10 + 
          '    /// Round mathematically' + #13#10 + 
          '    function Round: int64;' + #13#10 + 
          '    /// Round after "digits" fragment' + #13#10 + 
          '    function RoundTo(digits: integer): double;' + #13#10 + 
          '    /// Round to zero' + #13#10 + 
          '    function Trunc: int64;' + #13#10 + 
          '    /// Round to the next lower value' + #13#10 + 
          '    function Floor: integer;' + #13#10 + 
          '    function ToString: string; overload;' + #13#10 + 
          '    /// Convert the value to a string' + #13#10 + 
          '    function ToString(Digits: integer): string; overload;' + #13#10 + 
          '    /// Negate the value' + #13#10 + 
          '    function Negate: double;' + #13#10 + 
          '    /// Returns the absolte value' + #13#10 + 
          '    function Abs: double;' + #13#10 + 
          '    /// Returns true, if the value is >= min and <= max' + #13#10 + 
          '    function IsInRange(min, max: double): boolean;' + #13#10 + 
          '    /// Returns min if the value is smaller min, returns max if the value is greater max, otherwise returns the value' + #13#10 + 
          '    function ClampToRange(min, max: double): double;' + #13#10 + 
          '  end;' + #13#10 + 
          '  TSingleHelper = helper for single' + #13#10 + 
          '  public' + #13#10 + 
          '    /// Returns true, if the value is not a number' + #13#10 + 
          '    function IsNan: boolean;' + #13#10 + 
          '    /// Returns true, if the value represents infinity' + #13#10 + 
          '    function IsInfinite: boolean;' + #13#10 + 
          '    /// Round mathematically' + #13#10 + 
          '    function Round: int64;' + #13#10 + 
          '    /// Round after "digits" fragment' + #13#10 + 
          '    function RoundTo(digits: integer): double;' + #13#10 + 
          '    /// Round to zero' + #13#10 + 
          '    function Trunc: int64;' + #13#10 + 
          '    /// Round to the next lower value' + #13#10 + 
          '    function Floor: integer;' + #13#10 + 
          '    function ToString: string; overload;' + #13#10 + 
          '    /// Convert the value to a string' + #13#10 + 
          '    function ToString(Digits: integer): string; overload;' + #13#10 + 
          '    /// Returns the negative value' + #13#10 + 
          '    function Negate: single;' + #13#10 + 
          '    /// Returns the absolute value' + #13#10 + 
          '    function Abs: single;' + #13#10 + 
          '    /// Returns true, if the value is >= min and <= max' + #13#10 + 
          '    function IsInRange(min, max: single): boolean;' + #13#10 + 
          '    /// Returns min if the value is smaller min, returns max if the value is greater max, otherwise returns the value' + #13#10 + 
          '    function ClampToRange(min, max: single): single;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          '  TNotifyEventHelper = helper for TNotifyEvent' + #13#10 + 
          '  public' + #13#10 + 
          '    function Assigned: boolean;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          '  TByteHelper = helper for byte' + #13#10 + 
          '  public' + #13#10 + 
          '    function ToString: string; overload;' + #13#10 + 
          '    function ToString(Digits: integer): string; overload;' + #13#10 + 
          '    function ToString(Digits: integer; PrefixStr: string): string; overload;' + #13#10 + 
          '    /// Returns true, if the value is >= min and <= max' + #13#10 + 
          '    function IsInRange(min, max: byte): boolean;' + #13#10 + 
          '    /// Returns min if the value is smaller min, returns max if the value is greater max, otherwise returns the value' + #13#10 + 
          '    function ClampToRange(min, max: byte): byte;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          '  TShortIntHelper = helper for shortint' + #13#10 + 
          '  public' + #13#10 + 
          '    function ToString: string; overload;' + #13#10 + 
          '    function ToString(Digits: integer): string; overload;' + #13#10 + 
          '    function ToString(Digits: integer; PrefixStr: string): string; overload;' + #13#10 + 
          '    function Negate: shortint;' + #13#10 + 
          '    function Abs: shortint;' + #13#10 + 
          '    /// Returns true, if the value is >= min and <= max' + #13#10 + 
          '    function IsInRange(min, max: shortint): boolean;' + #13#10 + 
          '    /// Returns min if the value is smaller min, returns max if the value is greater max, otherwise returns the value' + #13#10 + 
          '    function ClampToRange(min, max: shortint): shortint;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          '  TWordHelper = helper for word' + #13#10 + 
          '  public' + #13#10 + 
          '    function ToString: string; overload;' + #13#10 + 
          '    function ToString(Digits: integer): string; overload;' + #13#10 + 
          '    function ToString(Digits: integer; PrefixStr: string): string; overload;' + #13#10 + 
          '    /// Returns true, if the value is >= min and <= max' + #13#10 + 
          '    function IsInRange(min, max: word): boolean;' + #13#10 + 
          '    /// Returns min if the value is smaller min, returns max if the value is greater max, otherwise returns the value' + #13#10 + 
          '    function ClampToRange(min, max: word): word;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          '  TSmallIntHelper = helper for smallint' + #13#10 + 
          '  public' + #13#10 + 
          '    function ToString: string; overload;' + #13#10 + 
          '    function ToString(Digits: integer): string; overload;' + #13#10 + 
          '    function ToString(Digits: integer; PrefixStr: string): string; overload;' + #13#10 + 
          '    function Negate: smallint;' + #13#10 + 
          '    function Abs: smallint;' + #13#10 + 
          '    /// Returns true, if the value is >= min and <= max' + #13#10 + 
          '    function IsInRange(min, max: smallint): boolean;' + #13#10 + 
          '    /// Returns min if the value is smaller min, returns max if the value is greater max, otherwise returns the value' + #13#10 + 
          '    function ClampToRange(min, max: smallint): smallint;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          '  TIntegerHelper = helper for integer' + #13#10 + 
          '  public' + #13#10 + 
          '    function ToString: string; overload;' + #13#10 + 
          '    function ToString(Digits: integer): string; overload;' + #13#10 + 
          '    function ToString(Digits: integer; PrefixStr: string): string; overload;' + #13#10 + 
          '    function Negate: integer;' + #13#10 + 
          '    function Abs: integer;' + #13#10 + 
          '    /// Returns true, if the value is >= min and <= max' + #13#10 + 
          '    function IsInRange(min, max: integer): boolean;' + #13#10 + 
          '    /// Returns min if the value is smaller min, returns max if the value is greater max, otherwise returns the value' + #13#10 + 
          '    function ClampToRange(min, max: integer): integer;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          '  TCardinalHelper = helper for cardinal' + #13#10 + 
          '  public' + #13#10 + 
          '    function ToString: string; overload;' + #13#10 + 
          '    function ToString(Digits: integer): string; overload;' + #13#10 + 
          '    function ToString(Digits: integer; PrefixStr: string): string; overload;' + #13#10 + 
          '    /// Returns true, if the value is >= min and <= max' + #13#10 + 
          '    function IsInRange(min, max: cardinal): boolean;' + #13#10 + 
          '    /// Returns min if the value is smaller min, returns max if the value is greater max, otherwise returns the value' + #13#10 + 
          '    function ClampToRange(min, max: cardinal): cardinal;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          '  TInt64Helper = helper for int64' + #13#10 + 
          '  public' + #13#10 + 
          '    function ToString: string; overload;' + #13#10 + 
          '    function ToString(Digits: integer): string; overload;' + #13#10 + 
          '    function ToString(Digits: integer; PrefixStr: string): string; overload;' + #13#10 + 
          '    function Negate: int64;' + #13#10 + 
          '    function Abs: int64;' + #13#10 + 
          '    /// Returns true, if the value is >= min and <= max' + #13#10 + 
          '    function IsInRange(min, max: int64): boolean;' + #13#10 + 
          '    /// Returns min if the value is smaller min, returns max if the value is greater max, otherwise returns the value' + #13#10 + 
          '    function ClampToRange(min, max: int64): int64;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          '  TBooleanHelper = helper for boolean' + #13#10 + 
          '  public' + #13#10 + 
          '    function ToString: string;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          '  TDateTimeHelper = helper for TDateTime' + #13#10 + 
          '  public' + #13#10 + 
          '    /// Add "days" to the date' + #13#10 + 
          '    function AddDays(value: double): TDateTime;' + #13#10 + 
          '    /// Add "hours" to the date' + #13#10 + 
          '    function AddHours(value: double): TDateTime;' + #13#10 + 
          '    /// Add "minutes" to the date' + #13#10 + 
          '    function AddMinutes(value: double): TDateTime;' + #13#10 + 
          '    /// Add "seconds" to the date' + #13#10 + 
          '    function AddSeconds(value: double): TDateTime;' + #13#10 + 
          '    /// Add "milliseconds" to the date' + #13#10 + 
          '    function AddMilliseconds(value: double): TDateTime;' + #13#10 + 
          '    /// Returns the day of the week (1 = Sunday, 7 = Saturday)' + #13#10 + 
          '    function DayOfWeek: integer;' + #13#10 + 
          '    /// Returns the absolute day index in the year' + #13#10 + 
          '    function DayOfYear: integer;' + #13#10 +
          '    /// Convert to the system date string' + #13#10 +
          '    function DateToStr: string; overload;' + #13#10 +
          '    /// Convert to the system date string' + #13#10 +
          '    function DateToStr(LanguageId: integer): string; overload;' + #13#10 +
          '    /// Convert to the system time string' + #13#10 +
          '    function TimeToStr: string; overload;' + #13#10 +    
          '    /// Convert to the system time string' + #13#10 +
          '    function TimeToStr(LanguageId: integer): string; overload;'+#13#10+
          '    function ToString: string; overload;' + #13#10 +
          '    function ToString(Format: string): string; overload;' + #13#10 +
          '    function ToString(LanguageId: integer): string; overload;' + #13#10 +
          '    function ToString(Format: string; LanguageId: integer): string; overload;' + #13#10 +
          '    /// Get the year' + #13#10 + 
          '    function Year: integer;' + #13#10 + 
          '    /// Get the month' + #13#10 + 
          '    function Month: integer;' + #13#10 + 
          '    /// Get the day' + #13#10 + 
          '    function Day: integer;' + #13#10 + 
          '    /// Get the hour' + #13#10 + 
          '    function Hours: integer;' + #13#10 + 
          '    function Minutes: integer;' + #13#10 + 
          '    function Seconds: integer;' + #13#10 + 
          '    function Milliseconds: integer;' + #13#10 + 
          #13#10 + 
           {$IFNDEF SEII_FPC}
          '    /// Convert the time to utc-time' + #13#10 + 
          '    function ToUTCTime: TDateTime;' + #13#10 + 
          '    /// Convert a utc-time to local time' + #13#10 + 
          '    function ToLocalTime: TDateTime;' + #13#10 + 
          {$ENDIF}
          '  end;' + #13#10 + 
          #13#10 + 
          '  TColorHelper = helper for TColor' + #13#10 + 
          '  public' + #13#10 + 
          '    /// The red part of the color' + #13#10 + 
          '    function Red: byte;' + #13#10 + 
          '    /// The green part of the color' + #13#10 + 
          '    function Green: byte;' + #13#10 + 
          '    /// The blue part of the color' + #13#10 + 
          '    function Blue: byte;' + #13#10 + 
          '    /// Convert the color to a grayscale value' + #13#10 + 
          '    function ToGrayscale: TColor;' + #13#10 + 
          '  end;' + #13#10 + 
          #13#10 + 
          'implementation' + #13#10 + 
          #13#10 + 
          'function TNotifyEventHelper.Assigned: boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := @Self <> nil;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
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
          'end;' + #13#10 + 
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
          {$IFNDEF SEII_FPC}
          'function TDateTimeHelper.ToUTCTime: TDateTime;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := DateTime.DateTimeToUTCTime(Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TDateTimeHelper.ToLocalTime: TDateTime;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := DateTime.UTCTimeToDateTime(Self);' + #13#10 + 
          'end;' + #13#10 + 
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
          'function TDateTimeHelper.DateToStr(LanguageId: integer): string;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.DateToStr(Self, LanguageId);' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.TimeToStr: string;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.TimeToStr(Self);' + #13#10 +
          'end;' + #13#10 +     
          'function TDateTimeHelper.TimeToStr(LanguageId: integer): string;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.TimeToStr(Self, LanguageId);' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.ToString: string;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.DateTimeToStr(Self);' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.ToString(Format: string): string;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.FormatDateTime(Format, Self);' + #13#10 +
          'end;' + #13#10 +    
          'function TDateTimeHelper.ToString(LanguageId: integer): string;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.DateTimeToStr(Self, LanguageId);' + #13#10 +
          'end;' + #13#10 +
          'function TDateTimeHelper.ToString(Format: string; LanguageId: integer): string;' + #13#10 +
          'begin' + #13#10 +
          '  result := DateTime.FormatDateTime(Format, Self, LanguageId);' + #13#10 +
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
          'end;' + #13#10 + 
          'function TStringHelper.ToLower: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.LowerCase(Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.Length: integer;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.Length(Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.Replace(OldValue, NewValue: string): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.Replace(Self, OldValue, NewValue, [rfReplaceAll, rfIgnoreCase]);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.Replace(OldValue, NewValue: string; Flags: TReplaceFlags): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.Replace(Self, OldValue, NewValue, Flags);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.Copy(Start, Count: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.Copy(Self, Start, Count);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.Copy(Start: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.Copy(Self, Start, $7FFFFFFF);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.SubString(StartIndex: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.Copy(Self, StartIndex, $7FFFFFFF);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.SubString(StartIndex, Length: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.Copy(Self, StartIndex, Length);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.IndexOf(value: string): integer;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.Pos(value, Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.IndexOf(value: string; SearchStart: integer): integer;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.PosEx(value, Self, SearchStart);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.TrimLeft: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.TrimLeft(Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.TrimRight: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.TrimRight(Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.Trim: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.Trim(Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.EndsWith(const value: string): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.CompareStr(Self.SubString(Self.Length - value.Length + 1, value.Length), value) = 0;' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.EndsWith(const value: string; CaseSensitive: boolean): boolean;' + #13#10 + 
          'var s: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  s := Self.SubString(Self.Length - value.Length + 1, value.Length);' + #13#10 + 
          '  if CaseSensitive then' + #13#10 + 
          '     result := Strings.CompareStr(s, value) = 0' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Strings.SameText(s, value);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.StartsWith(const value: string): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.SameText(Self.SubString(1, value.Length), value);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.StartsWith(const value: string; CaseSensitive: boolean): boolean;' + #13#10 + 
          'var s: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  s := Self.SubString(1, value.Length);' + #13#10 + 
          '  if CaseSensitive then' + #13#10 + 
          '     result := Strings.CompareStr(s, value) = 0' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Strings.SameText(s, value);' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.LastIndexOf(const value: string): integer;' + #13#10 + 
          'var iSearchStart, iPos: integer;' + #13#10 + 
          'begin' + #13#10 + 
          '  result       := 0;' + #13#10 + 
          '  iSearchStart := 0;' + #13#10 + 
          '  repeat' + #13#10 + 
          '    iPos := Strings.PosEx(value, Self, iSearchStart);' + #13#10 + 
          '    if iPos > 0 then' + #13#10 + 
          '       result := iPos;' + #13#10 + 
          '    iSearchStart := iPos + 1;' + #13#10 + 
          '  until iPos = 0;' + #13#10 + 
          'end;' + #13#10 + 
          'function TStringHelper.ToASCIIIndex: word;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Strings.ToASCIIIndex(Self);' + #13#10 + 
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
          'end;' + #13#10 + 
          'function TWideStringHelper.ToLower: WideString;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.LowerCase(Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.Copy(Start, Count: integer): WideString;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.Copy(Self, Start, Count);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.Copy(Start: integer): WideString;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.Copy(Self, Start, $7FFFFFFF);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.SubString(StartIndex: integer): WideString;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.Copy(Self, StartIndex, $7FFFFFFF);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.SubString(StartIndex, Length: integer): WideString;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.Copy(Self, StartIndex, Length);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.IndexOf(value: WideString): integer;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.Pos(value, Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.TrimLeft: WideString;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.TrimLeft(Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.TrimRight: WideString;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.TrimRight(Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.Trim: WideString;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.Trim(Self);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.EndsWith(const value: WideString): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.SameText(Self.SubString(Self.Length - value.Length + 1, value.Length), value) = 0;' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.EndsWith(const value: WideString; CaseSensitive: boolean): boolean;' + #13#10 + 
          'var s: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  s := Self.SubString(Self.Length - value.Length + 1, value.Length);' + #13#10 + 
          '  if CaseSensitive then' + #13#10 + 
          '     result := WideStrings.SameStr(s, value)' + #13#10 + 
          '  else' + #13#10 + 
          '     result := WideStrings.SameText(s, value);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.StartsWith(const value: WideString): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.SameText(Self.SubString(1, value.Length), value);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.StartsWith(const value: WideString; CaseSensitive: boolean): boolean;' + #13#10 + 
          'var s: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  s := Self.SubString(1, value.Length);' + #13#10 + 
          '  if CaseSensitive then' + #13#10 + 
          '     result := WideStrings.SameStr(s, value)' + #13#10 + 
          '  else' + #13#10 + 
          '     result := WideStrings.SameText(s, value);' + #13#10 + 
          'end;' + #13#10 + 
          'function TWideStringHelper.Length: integer;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := WideStrings.Length(Self);' + #13#10 + 
          'end;' + #13#10 + 
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
          'function TDoubleHelper.IsInRange(min, max: double): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := (Self >= min) and (Self <= max);' + #13#10 + 
          'end;' + #13#10 + 
          'function TDoubleHelper.ClampToRange(min, max: double): double;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self < min then' + #13#10 + 
          '     result := min' + #13#10 + 
          '  else' + #13#10 + 
          '  if Self > max then' + #13#10 + 
          '     result := max' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Self;' + #13#10 + 
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
          'function TSingleHelper.IsInRange(min, max: single): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := (Self >= min) and (Self <= max);' + #13#10 + 
          'end;' + #13#10 + 
          'function TSingleHelper.ClampToRange(min, max: single): single;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self < min then' + #13#10 + 
          '     result := min' + #13#10 + 
          '  else' + #13#10 + 
          '  if Self > max then' + #13#10 + 
          '     result := max' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Self;' + #13#10 + 
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
          #13#10 + 
          'function TByteHelper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TByteHelper.ToString(Digits: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TByteHelper.ToString(Digits: integer; PrefixStr: string): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits, PrefixStr);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TByteHelper.IsInRange(min, max: byte): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := (Self >= min) and (Self <= max);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TByteHelper.ClampToRange(min, max: byte): byte;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self < min then' + #13#10 + 
          '     result := min' + #13#10 + 
          '  else' + #13#10 + 
          '  if Self > max then' + #13#10 + 
          '     result := max' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TShortIntHelper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TShortIntHelper.ToString(Digits: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TShortIntHelper.ToString(Digits: integer; PrefixStr: string): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits, PrefixStr);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TShortIntHelper.Negate: shortint;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TShortIntHelper.Abs: shortint;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self > 0 then' + #13#10 + 
          '     result := Self' + #13#10 + 
          '  else' + #13#10 + 
          '     result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TShortIntHelper.IsInRange(min, max: shortint): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := (Self >= min) and (Self <= max);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TShortIntHelper.ClampToRange(min, max: shortint): shortint;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self < min then' + #13#10 + 
          '     result := min' + #13#10 + 
          '  else' + #13#10 + 
          '  if Self > max then' + #13#10 + 
          '     result := max' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TWordHelper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TWordHelper.ToString(Digits: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TWordHelper.ToString(Digits: integer; PrefixStr: string): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits, PrefixStr);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TWordHelper.IsInRange(min, max: word): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := (Self >= min) and (Self <= max);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TWordHelper.ClampToRange(min, max: word): word;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self < min then' + #13#10 + 
          '     result := min' + #13#10 + 
          '  else' + #13#10 + 
          '  if Self > max then' + #13#10 + 
          '     result := max' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSmallIntHelper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSmallIntHelper.ToString(Digits: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSmallIntHelper.ToString(Digits: integer; PrefixStr: string): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits, PrefixStr);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSmallIntHelper.Negate: smallint;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSmallIntHelper.Abs: smallint;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self > 0 then' + #13#10 + 
          '     result := Self' + #13#10 + 
          '  else' + #13#10 + 
          '     result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSmallIntHelper.IsInRange(min, max: smallint): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := (Self >= min) and (Self <= max);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSmallIntHelper.ClampToRange(min, max: smallint): smallint;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self < min then' + #13#10 + 
          '     result := min' + #13#10 + 
          '  else' + #13#10 + 
          '  if Self > max then' + #13#10 + 
          '     result := max' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TCardinalHelper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TCardinalHelper.ToString(Digits: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TCardinalHelper.ToString(Digits: integer; PrefixStr: string): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits, PrefixStr);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TCardinalHelper.IsInRange(min, max: cardinal): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := (Self >= min) and (Self <= max);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TCardinalHelper.ClampToRange(min, max: cardinal): cardinal;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self < min then' + #13#10 + 
          '     result := min' + #13#10 + 
          '  else' + #13#10 + 
          '  if Self > max then' + #13#10 + 
          '     result := max' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TIntegerHelper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TIntegerHelper.ToString(Digits: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TIntegerHelper.ToString(Digits: integer; PrefixStr: string): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits, PrefixStr);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TIntegerHelper.Negate: integer;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TIntegerHelper.Abs: integer;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self > 0 then' + #13#10 + 
          '     result := Self' + #13#10 + 
          '  else' + #13#10 + 
          '     result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TIntegerHelper.IsInRange(min, max: integer): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := (Self >= min) and (Self <= max);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TIntegerHelper.ClampToRange(min, max: integer): integer;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self < min then' + #13#10 + 
          '     result := min' + #13#10 + 
          '  else' + #13#10 + 
          '  if Self > max then' + #13#10 + 
          '     result := max' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TInt64helper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TInt64helper.ToString(Digits: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TInt64helper.ToString(Digits: integer; PrefixStr: string): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.IntToStr(Self, Digits, PrefixStr);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TInt64helper.Negate: int64;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TInt64helper.Abs: int64;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self > 0 then' + #13#10 + 
          '     result := Self' + #13#10 + 
          '  else' + #13#10 + 
          '     result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TInt64Helper.IsInRange(min, max: int64): boolean;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := (Self >= min) and (Self <= max);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TInt64Helper.ClampToRange(min, max: int64): int64;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self < min then' + #13#10 + 
          '     result := min' + #13#10 + 
          '  else' + #13#10 + 
          '  if Self > max then' + #13#10 + 
          '     result := max' + #13#10 + 
          '  else' + #13#10 + 
          '     result := Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TBooleanHelper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.BoolToStr(Self, True);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSingleHelper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.FloatToStr(Self);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TPointerHelper.AddOffset(const Offset: int64): Pointer;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Pointer(Int64(Self) + Offset);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TPointerHelper.OffsetTo(Target: pointer): int64;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := int64(Target) - Int64(Self);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TPointerHelper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Self.ToString(''$'');' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TPointerHelper.ToString(Prefix: string): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Prefix + Convert.IntToHex(int64(Self), 8);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSingleHelper.ToString(Digits: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.FloatToStr(Self, ffFixed, 7, Digits);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSingleHelper.Negate: single;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TSingleHelper.Abs: single;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self > 0 then' + #13#10 + 
          '     result := Self' + #13#10 + 
          '  else' + #13#10 + 
          '     result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TDoubleHelper.ToString: string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.FloatToStr(Self);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TDoubleHelper.ToString(Digits: integer): string;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := Convert.FloatToStr(Self, ffFixed, 15, Digits);' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TDoubleHelper.Negate: double;' + #13#10 + 
          'begin' + #13#10 + 
          '  result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
          'function TDoubleHelper.Abs: double;' + #13#10 + 
          'begin' + #13#10 + 
          '  if Self > 0 then' + #13#10 + 
          '     result := Self' + #13#10 + 
          '  else' + #13#10 + 
          '     result := -Self;' + #13#10 + 
          'end;' + #13#10 + 
          #13#10 + 
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
  p.Priority          := 500;
  TSE2UnitManager.RegisterUnit(p);
end;

initialization
  RegisterUnit;

end.
