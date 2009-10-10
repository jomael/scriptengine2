unit uSE2IncDateTime;

{$INCLUDE ScriptEngine.inc}

interface

uses
  {$IFNDEF FPC}
  Windows,
  {$ENDIF}
  Classes, SysUtils, DateUtils, uSE2Types, uSE2BaseTypes, uSE2RunAccess, uSE2UnitManager;

implementation

const
  C_UnitName   = 'System';
  C_UnitSource =
     'unit System;'+#13#10+
     #13#10+
     'interface'+#13#10+
     #13#10+
     '(*'+#13#10+
     'const'+#13#10+
     '   DayMonday    : byte = 1;'+#13#10+
     '   DayTuesday   : byte = 2;'+#13#10+
     '   DayWednesday : byte = 3;'+#13#10+
     '   DayThursday  : byte = 4;'+#13#10+
     '   DayFriday    : byte = 5;'+#13#10+
     '   DaySaturday  : byte = 6;'+#13#10+
     '   DaySunday    : byte = 7;'+#13#10+
     '*)'+#13#10+
     #13#10+
     'type'+#13#10+
     '  TDateTime = double;'+#13#10+
     '  LongWord  = cardinal;'+#13#10+
     #13#10+
     '  DateTime = class(TExternalObject)'+#13#10+
     '  public'+#13#10+
     '    class function CompareDate(const a, b: TDateTime): integer; external;'+#13#10+
     '    class function CompareDateTime(const a, b: TDateTime): integer; external;'+#13#10+
     '    class function CompareTime(const a, b: TDateTime): integer; external;'+#13#10+
     '    class function CurrentYear: word; external;'+#13#10+
     '    class function Date: TDateTime; external;'+#13#10+
     '    class function DateOf(const Date: TDateTime): TDateTime; external;'+#13#10+
     '    class function DateTimeToStr(const Date: TDateTime): string; external;'+#13#10+
     '    class function DateToStr(const Date: TDateTime): string; external;'+#13#10+
     '    class function DayOf(const Date: TDateTime): word; external;'+#13#10+
     '    class function DayOfTheMonth(const Date: TDateTime): word; external;'+#13#10+   
     '    class function DayOfTheWeek(const Date: TDateTime): word; external;'+#13#10+
     '    class function DayOfTheYear(const Date: TDateTime): word; external;'+#13#10+
     '    class function DayOfWeek(const Date: TDateTime): integer; external;'+#13#10+
     '    class function DaysBetween(const d1, d2: TDateTime): integer; external;'+#13#10+
     '    class function DaysInAMonth(const Year, Month: word): word; external;'+#13#10+   
     '    class function DaysInAYear(const Year: word): word; external;'+#13#10+
     '    class function DaysInMonth(const dt: TDateTime): word; external;'+#13#10+
     '    class function DaysInYear(const dt: TDateTime): word; external;'+#13#10+
     '    class function DaySpan(const d1, d2: TDateTime): double; external;'+#13#10+
     '    class procedure DecodeDate(dt: TDateTime; var Year, Month, Day: word); external;'+#13#10+
     '    class procedure DecodeDateDay(dt: TDateTime; var Year, DayOfYear: word); external;'+#13#10+
     '    class function DecodeDateFully(dt: TDateTime; var Year, Month, Day, DOW: word): boolean; external;'+#13#10+
     '    class procedure DecodeDateMonthWeek(const AValue: TDateTime; out Year, Month, WeekOfMonth, DayOfWeek: Word); external;'+#13#10+
     '    class procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word); external;'+#13#10+
     '    class procedure DecodeDateWeek(const AValue: TDateTime; out AYear, AWeekOfYear, ADayOfWeek: Word); external;'+#13#10+
     '    class procedure DecodeDayOfWeekInMonth(const AValue: TDateTime; out AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word); external;'+#13#10+
     '    class procedure DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word); external;'+#13#10+
     
     '    class function EncodeDate(Year, Month, Day: Word): TDateTime; external;'+#13#10+
     '    class function EncodeDateDay(const AYear, ADayOfYear: Word): TDateTime; external;'+#13#10+
     '    class function EncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth: Word; const ADayOfWeek: Word): TDateTime; overload; external;'+#13#10+
     '    class function EncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth: Word): TDateTime; overload; external;'+#13#10+
     '    class function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word):TDateTime; external;'+#13#10+
     '    class function EncodeDateWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime; overload; external;'+#13#10+
     '    class function EncodeDateWeek(const AYear, AWeekOfYear: Word): TDateTime; overload; external;'+#13#10+
     '    class function EncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word): TDateTime; external;'+#13#10+
     '    class function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime; external;'+#13#10+
     '    class function EndOfADay(const AYear, ADayOfYear: Word): TDateTime; overload; external;'+#13#10+
     '    class function EndOfADay(const AYear, AMonth, ADay: Word): TDateTime; overload; external;'+#13#10+
     '    class function EndOfAMonth(const AYear, AMonth: Word): TDateTime; external;'+#13#10+
     '    class function EndOfAWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime; overload; external;'+#13#10+
     '    class function EndOfAWeek(const AYear, AWeekOfYear: Word): TDateTime; overload; external;'+#13#10+
     '    class function EndOfAYear(const AYear: word): TDateTime; external;'+#13#10+
     '    class function EndOfTheDay(const AValue: TDateTime): TDateTime; external;'+#13#10+
     '    class function EndOfTheMonth(const AValue: TDateTime): TDateTime; external;'+#13#10+
     '    class function EndOfTheWeek(const AValue: TDateTime): TDateTime; external;'+#13#10+
     '    class function EndOfTheYear(const AValue: TDateTime): TDateTime; external;'+#13#10+
     '    class function FormatDateTime(const Format: string; DateTime: TDateTime): string; external;'+#13#10+
     '    class function HourOf(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function HourOfTheDay(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function HourOfTheMonth(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function HourOfTheWeek(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function HourOfTheYear(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function HoursBetween(const ANow, AThen: TDateTime): Int64; external;'+#13#10+
     '    class function HourSpan(const ANow, AThen: TDateTime): Double; external;'+#13#10+
     '    class procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer); overload; external;'+#13#10+
     '    class procedure IncAMonth(var Year, Month, Day: Word); overload; external;'+#13#10+
     '    class function IncDay(const AValue: TDateTime; const ANumberOfDays: Integer): TDateTime; overload; external;'+#13#10+
     '    class function IncDay(const AValue: TDateTime): TDateTime; overload; external;'+#13#10+
     '    class function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime; overload; external;'+#13#10+
     '    class function IncHour(const AValue: TDateTime): TDateTime; overload; external;'+#13#10+
     '    class function IncMilliSecond(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64): TDateTime; overload; external;'+#13#10+
     '    class function IncMilliSecond(const AValue: TDateTime): TDateTime; overload; external;'+#13#10+
     '    class function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime; overload; external;'+#13#10+
     '    class function IncMinute(const AValue: TDateTime): TDateTime; overload; external;'+#13#10+
     '    class function IncMonth(const Date: TDateTime; NumberOfMonths: Integer): TDateTime; overload; external;'+#13#10+
     '    class function IncMonth(const Date: TDateTime): TDateTime; overload; external;'+#13#10+
     '    class function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime; overload; external;'+#13#10+
     '    class function IncSecond(const AValue: TDateTime): TDateTime; overload; external;'+#13#10+
     '    class function IncWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer): TDateTime; overload; external;'+#13#10+
     '    class function IncWeek(const AValue: TDateTime): TDateTime; overload; external;'+#13#10+
     '    class function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer): TDateTime; overload; external;'+#13#10+
     '    class function IncYear(const AValue: TDateTime): TDateTime; overload; external;'+#13#10+
     '    class function IsInLeapYear(const AValue: TDateTime): Boolean; external;'+#13#10+
     '    class function IsLeapYear(Year: Word): Boolean; external;'+#13#10+
     '    class function IsPM(const AValue: TDateTime): Boolean; external;'+#13#10+
     '    class function IsSameDay(const AValue, ABasis: TDateTime): Boolean; external;'+#13#10+
     '    class function IsToday(const AValue: TDateTime): Boolean; external;'+#13#10+
     '    class function IsValidDate(const AYear, AMonth, ADay: Word): Boolean; external;'+#13#10+
     '    class function IsValidDateDay(const AYear, ADayOfYear: Word): Boolean; external;'+#13#10+
     '    class function IsValidDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): Boolean; external;'+#13#10+
     '    class function IsValidDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): Boolean; external;'+#13#10+
     '    class function IsValidDateWeek(const AYear, AWeekOfYear, ADayOfWeek: Word): Boolean; external;'+#13#10+
     '    class function IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean; external;'+#13#10+
     '    class function MilliSecondOf(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function MilliSecondOfTheDay(const AValue: TDateTime): LongWord; external;'+#13#10+
     '    class function MilliSecondOfTheHour(const AValue: TDateTime): LongWord; external;'+#13#10+
     '    class function MilliSecondOfTheMinute(const AValue: TDateTime): LongWord; external;'+#13#10+
     '    class function MilliSecondOfTheMonth(const AValue: TDateTime): LongWord; external;'+#13#10+
     '    class function MilliSecondOfTheSecond(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function MilliSecondOfTheWeek(const AValue: TDateTime): LongWord; external;'+#13#10+
     '    class function MilliSecondOfTheYear(const AValue: TDateTime): Int64; external;'+#13#10+
     '    class function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64; external;'+#13#10+
     '    class function MilliSecondSpan(const ANow, AThen: TDateTime): Double; external;'+#13#10+
     '    class function MinuteOf(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function MinuteOfTheHour(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function MinuteOfTheMonth(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function MinuteOfTheWeek(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function MinuteOfTheYear(const AValue: TDateTime): LongWord; external;'+#13#10+
     '    class function MinutesBetween(const ANow, AThen: TDateTime): Int64; external;'+#13#10+
     '    class function MinuteSpan(const ANow, AThen: TDateTime): Double; external;'+#13#10+
     '    class function MonthOf(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function MonthOfTheYear(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function MonthsBetween(const ANow, AThen: TDateTime): Integer; external;'+#13#10+
     '    class function MonthSpan(const ANow, AThen: TDateTime): Double; external;'+#13#10+
     '    class function Now: TDateTime; external;'+#13#10+
     '    class function RecodeDate(const AValue: TDateTime; const AYear, AMonth, ADay: Word): TDateTime; external;'+#13#10+
     '    class function RecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime; external;'+#13#10+
     '    class function RecodeDay(const AValue: TDateTime; const ADay: Word): TDateTime; external;'+#13#10+
     '    class function RecodeHour(const AValue: TDateTime; const AHour: Word): TDateTime; external;'+#13#10+
     '    class function RecodeMilliSecond(const AValue: TDateTime; const AMilliSecond: Word): TDateTime; external;'+#13#10+
     '    class function RecodeMinute(const AValue: TDateTime; const AMinute: Word): TDateTime; external;'+#13#10+
     '    class function RecodeMonth(const AValue: TDateTime; const AMonth: Word): TDateTime; external;'+#13#10+
     '    class function RecodeSecond(const AValue: TDateTime; const ASecond: Word): TDateTime; external;'+#13#10+
     '    class function RecodeTime(const AValue: TDateTime; const AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime; external;'+#13#10+
     '    class function RecodeYear(const AValue: TDateTime; const AYear: Word): TDateTime; external;'+#13#10+
     '    class procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime); external;'+#13#10+
     '    class procedure ReplaceTime(var DateTime: TDateTime; const NewTime: TDateTime); external;'+#13#10+
     '    class function SameDate(const A, B: TDateTime): Boolean; external;'+#13#10+
     '    class function SameDateTime(const A, B: TDateTime): Boolean; external;'+#13#10+
     '    class function SameTime(const A, B: TDateTime): Boolean; external;'+#13#10+

     '    class function SecondOf(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function SecondOfTheDay(const AValue: TDateTime): LongWord; external;'+#13#10+
     '    class function SecondOfTheHour(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function SecondOfTheMinute(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function SecondOfTheMonth(const AValue: TDateTime): LongWord; external;'+#13#10+
     '    class function SecondOfTheWeek(const AValue: TDateTime): LongWord; external;'+#13#10+
     '    class function SecondOfTheYear(const AValue: TDateTime): LongWord; external;'+#13#10+
     '    class function SecondsBetween(const ANow, AThen: TDateTime): Int64; external;'+#13#10+
     '    class function SecondSpan(const ANow, AThen: TDateTime): Double; external;'+#13#10+
     '    class function StartOfADay(const AYear, ADayOfYear: Word): TDateTime; overload; external;'+#13#10+
     '    class function StartOfADay(const AYear, AMonth, ADay: Word): TDateTime; overload; external;'+#13#10+
     '    class function StartOfAMonth(const AYear, AMonth: Word): TDateTime; external;'+#13#10+
     '    class function StartOfAWeek(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime; overload; external;'+#13#10+
     '    class function StartOfAWeek(const AYear, AWeekOfYear: Word): TDateTime; overload; external;'+#13#10+
     '    class function StartOfAYear(const AYear: word): TDateTime; external;'+#13#10+
     '    class function StartOfTheDay(const AValue: TDateTime): TDateTime; external;'+#13#10+
     '    class function StartOfTheMonth(const AValue: TDateTime): TDateTime; external;'+#13#10+
     '    class function StartOfTheWeek(const AValue: TDateTime): TDateTime; external;'+#13#10+
     '    class function StartOfTheYear(const AValue: TDateTime): TDateTime; external;'+#13#10+
     '    class function StrToDate(const S: string): TDateTime; external;'+#13#10+
     '    class function StrToDateDef(const S: string; const Default: TDateTime): TDateTime; external;'+#13#10+
     '    class function StrToDateTime(const S: string): TDateTime; external;'+#13#10+
     '    class function StrToDateTimeDef(const S: string; const Default: TDateTime): TDateTime; external;'+#13#10+
     '    class function StrToTime(const S: string): TDateTime; external;'+#13#10+
     '    class function StrToTimeDef(const S: string; const Default: TDateTime): TDateTime; external;'+#13#10+
     '    class function Time: TDateTime; external;'+#13#10+
     '    class function TimeOf(const AValue: TDateTime): TDateTime; external;'+#13#10+
     '    class function TimeToStr(Time: TDateTime): string; external;'+#13#10+
     '    class function Today: TDateTime; external;'+#13#10+
     '    class function Tomorrow: TDateTime; external;'+#13#10+
     '    class function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean; external;'+#13#10+
     '    class function TryEncodeDateDay(const AYear, ADayOfYear: Word; out AValue: TDateTime): Boolean; external;'+#13#10+
     '    class function TryEncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word; out AValue: TDateTime): Boolean; external;'+#13#10+
     '    class function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; out AValue: TDateTime): Boolean; external;'+#13#10+
     '    class function TryEncodeDateWeek(const AYear, AWeekOfYear: Word; out AValue: TDateTime; const ADayOfWeek: Word): Boolean; overload; external;'+#13#10+
     '    class function TryEncodeDateWeek(const AYear, AWeekOfYear: Word; out AValue: TDateTime): Boolean; overload; external;'+#13#10+
     '    class function TryEncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word; out AValue: TDateTime): Boolean; external;'+#13#10+
     '    class function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean; external;'+#13#10+
     '    class function TryRecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; out AResult: TDateTime): Boolean; external;'+#13#10+
     '    class function TryStrToDate(const S: string; out Value: TDateTime): Boolean; external;'+#13#10+
     '    class function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean; external;'+#13#10+
     '    class function TryStrToTime(const S: string; out Value: TDateTime): Boolean; external;'+#13#10+
     '    class function WeekOf(const AValue: TDateTime): Word; external;'+#13#10+

     '    class function WeekOfTheMonth(const AValue: TDateTime): Word; overload; external;'+#13#10+
     '    class function WeekOfTheMonth(const AValue: TDateTime; var AYear, AMonth: Word): Word; overload; external;'+#13#10+
     '    class function WeekOfTheYear(const AValue: TDateTime): Word; overload; external;'+#13#10+
     '    class function WeekOfTheYear(const AValue: TDateTime; var AYear: word): Word; overload; external;'+#13#10+
     '    class function WeeksBetween(const ANow, AThen: TDateTime): Integer; external;'+#13#10+
     '    class function WeeksInAYear(const AYear: Word): Word; external;'+#13#10+
     '    class function WeeksInYear(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function WeekSpan(const ANow, AThen: TDateTime): Double; external;'+#13#10+
     '    class function WithinPastDays(const ANow, AThen: TDateTime; const ADays: Integer): Boolean; external;'+#13#10+
     '    class function WithinPastHours(const ANow, AThen: TDateTime; const AHours: Int64): Boolean; external;'+#13#10+
     '    class function WithinPastMilliSeconds(const ANow, AThen: TDateTime; const AMilliSeconds: Int64): Boolean; external;'+#13#10+
     '    class function WithinPastMinutes(const ANow, AThen: TDateTime; const AMinutes: Int64): Boolean; external;'+#13#10+
     '    class function WithinPastMonths(const ANow, AThen: TDateTime; const AMonths: Integer): Boolean; external;'+#13#10+
     '    class function WithinPastSeconds(const ANow, AThen: TDateTime; const ASeconds: Int64): Boolean; external;'+#13#10+
     '    class function WithinPastWeeks(const ANow, AThen: TDateTime; const AWeeks: Integer): Boolean; external;'+#13#10+
     '    class function WithinPastYears(const ANow, AThen: TDateTime; const AYears: Integer): Boolean; external;'+#13#10+
     '    class function YearOf(const AValue: TDateTime): Word; external;'+#13#10+
     '    class function YearsBetween(const ANow, AThen: TDateTime): Integer; external;'+#13#10+
     '    class function YearSpan(const ANow, AThen: TDateTime): Double; external;'+#13#10+
     '    class function Yesterday: TDateTime; external;'+#13#10+

     {$IFNDEF FPC}
     '    class function DateTimeToUTCTime(DateTime: TDateTime): TDateTime; external;'+#13#10+
     '    class function UTCTimeToDateTime(UTC: TDateTime): TDateTime; external;'+#13#10+
     {$ENDIF}

     '  end;'+#13#10+

     '  Convert = partial class'+#13#10+
     '  public'+#13#10+
     '    class function DateTimeToStr(const Date: TDateTime): string; external;'+#13#10+
     '    class function DateToStr(const Date: TDateTime): string; external;'+#13#10+
     '    class function TimeToStr(Time: TDateTime): string; external;'+#13#10+

     '    class function StrToDate(const S: string): TDateTime; external;'+#13#10+
     '    class function StrToDateDef(const S: string; const Default: TDateTime): TDateTime; external;'+#13#10+
     '    class function StrToDateTime(const S: string): TDateTime; external;'+#13#10+
     '    class function StrToDateTimeDef(const S: string; const Default: TDateTime): TDateTime; external;'+#13#10+
     '    class function StrToTime(const S: string): TDateTime; external;'+#13#10+
     '    class function StrToTimeDef(const S: string; const Default: TDateTime): TDateTime; external;'+#13#10+
     
     '    class function TryStrToDate(const S: string; out Value: TDateTime): Boolean; external;'+#13#10+
     '    class function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean; external;'+#13#10+
     '    class function TryStrToTime(const S: string; out Value: TDateTime): Boolean; external;'+#13#10+
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
  DateTime = class
  public
    class function CompareDate(const a, b: TDateTime): integer;
    class function CompareDateTime(const a, b: TDateTime): integer;
    class function CompareTime(const a, b: TDateTime): integer;
    class function CurrentYear: word;
    class function Date: TDateTime;
    class function DateOf(const Date: TDateTime): TDateTime;
    class function DateTimeToStr(const Date: TDateTime): string;
    class function DateToStr(const Date: TDateTime): string;
    class function DayOf(const Date: TDateTime): word;
    class function DayOfTheMonth(const Date: TDateTime): word;
    class function DayOfTheWeek(const Date: TDateTime): word;
    class function DayOfTheYear(const Date: TDateTime): word;
    class function DayOfWeek(const Date: TDateTime): integer;
    class function DaysBetween(const d1, d2: TDateTime): integer;
    class function DaysInAMonth(const Year, Month: word): word;
    class function DaysInAYear(const Year: word): word;
    class function DaysInMonth(const dt: TDateTime): word;
    class function DaysInYear(const dt: TDateTime): word;
    class function DaySpan(const d1, d2: TDateTime): double;
    class procedure DecodeDate(dt: TDateTime; var Year, Month, Day: word);
    class procedure DecodeDateDay(dt: TDateTime; var Year, DayOfYear: word);
    class function DecodeDateFully(dt: TDateTime; var Year, Month, Day, DOW: word): boolean;
    class procedure DecodeDateMonthWeek(const AValue: TDateTime; out Year, Month, WeekOfMonth, DayOfWeek: Word);
    class procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word);
    class procedure DecodeDateWeek(const AValue: TDateTime; out AYear, AWeekOfYear, ADayOfWeek: Word);
    class procedure DecodeDayOfWeekInMonth(const AValue: TDateTime; out AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word);
    class procedure DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
     
    class function EncodeDate(Year, Month, Day: Word): TDateTime;
    class function EncodeDateDay(const AYear, ADayOfYear: Word): TDateTime;
    class function EncodeDateMonthWeek0(const AYear, AMonth, AWeekOfMonth: Word; const ADayOfWeek: Word): TDateTime;
    class function EncodeDateMonthWeek1(const AYear, AMonth, AWeekOfMonth: Word): TDateTime;
    class function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word):TDateTime;
    class function EncodeDateWeek0(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
    class function EncodeDateWeek1(const AYear, AWeekOfYear: Word): TDateTime;
    class function EncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word): TDateTime;
    class function EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
    class function EndOfADay0(const AYear, ADayOfYear: Word): TDateTime;
    class function EndOfADay1(const AYear, AMonth, ADay: Word): TDateTime;
    class function EndOfAMonth(const AYear, AMonth: Word): TDateTime;
    class function EndOfAWeek0(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word): TDateTime;
    class function EndOfAWeek1(const AYear, AWeekOfYear: Word): TDateTime;
    class function EndOfAYear(const AYear: word): TDateTime;
    class function EndOfTheDay(const AValue: TDateTime): TDateTime;
    class function EndOfTheMonth(const AValue: TDateTime): TDateTime;
    class function EndOfTheWeek(const AValue: TDateTime): TDateTime;
    class function EndOfTheYear(const AValue: TDateTime): TDateTime;
    class function FormatDateTime(const Format: string; DateTime: TDateTime): string;
    class function HourOf(const AValue: TDateTime): Word;
    class function HourOfTheDay(const AValue: TDateTime): Word;
    class function HourOfTheMonth(const AValue: TDateTime): Word;
    class function HourOfTheWeek(const AValue: TDateTime): Word;
    class function HourOfTheYear(const AValue: TDateTime): Word;
    class function HoursBetween(const ANow, AThen: TDateTime): Int64;
    class function HourSpan(const ANow, AThen: TDateTime): Double;
    class procedure IncAMonth0(var Year, Month, Day: Word; NumberOfMonths: Integer);
    class procedure IncAMonth1(var Year, Month, Day: Word);
    class function IncDay0(const AValue: TDateTime; const ANumberOfDays: Integer): TDateTime;
    class function IncDay1(const AValue: TDateTime): TDateTime; 
    class function IncHour0(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime;
    class function IncHour1(const AValue: TDateTime): TDateTime;
    class function IncMilliSecond0(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64): TDateTime;
    class function IncMilliSecond1(const AValue: TDateTime): TDateTime;
    class function IncMinute0(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;
    class function IncMinute1(const AValue: TDateTime): TDateTime;
    class function IncMonth0(const Date: TDateTime; NumberOfMonths: Integer): TDateTime;
    class function IncMonth1(const Date: TDateTime): TDateTime;
    class function IncSecond0(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime;
    class function IncSecond1(const AValue: TDateTime): TDateTime;
    class function IncWeek0(const AValue: TDateTime; const ANumberOfWeeks: Integer): TDateTime;
    class function IncWeek1(const AValue: TDateTime): TDateTime;
    class function IncYear0(const AValue: TDateTime; const ANumberOfYears: Integer): TDateTime;
    class function IncYear1(const AValue: TDateTime): TDateTime;
    class function IsInLeapYear(const AValue: TDateTime): Boolean;
    class function IsLeapYear(Year: Word): Boolean;
    class function IsPM(const AValue: TDateTime): Boolean;
    class function IsSameDay(const AValue, ABasis: TDateTime): Boolean;
    class function IsToday(const AValue: TDateTime): Boolean;
    class function IsValidDate(const AYear, AMonth, ADay: Word): Boolean;
    class function IsValidDateDay(const AYear, ADayOfYear: Word): Boolean;
    class function IsValidDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word): Boolean;
    class function IsValidDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
    class function IsValidDateWeek(const AYear, AWeekOfYear, ADayOfWeek: Word): Boolean;
    class function IsValidTime(const AHour, AMinute, ASecond, AMilliSecond: Word): Boolean;
    class function MilliSecondOf(const AValue: TDateTime): Word;
    class function MilliSecondOfTheDay(const AValue: TDateTime): LongWord;
    class function MilliSecondOfTheHour(const AValue: TDateTime): LongWord;
    class function MilliSecondOfTheMinute(const AValue: TDateTime): LongWord;
    class function MilliSecondOfTheMonth(const AValue: TDateTime): LongWord;
    class function MilliSecondOfTheSecond(const AValue: TDateTime): Word;
    class function MilliSecondOfTheWeek(const AValue: TDateTime): LongWord;
    class function MilliSecondOfTheYear(const AValue: TDateTime): Int64;
    class function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64;
    class function MilliSecondSpan(const ANow, AThen: TDateTime): Double;
    class function MinuteOf(const AValue: TDateTime): Word;
    class function MinuteOfTheHour(const AValue: TDateTime): Word;
    class function MinuteOfTheMonth(const AValue: TDateTime): Word;
    class function MinuteOfTheWeek(const AValue: TDateTime): Word;
    class function MinuteOfTheYear(const AValue: TDateTime): LongWord;
    class function MinutesBetween(const ANow, AThen: TDateTime): Int64;
    class function MinuteSpan(const ANow, AThen: TDateTime): Double;
    class function MonthOf(const AValue: TDateTime): Word;
    class function MonthOfTheYear(const AValue: TDateTime): Word;
    class function MonthsBetween(const ANow, AThen: TDateTime): Integer;
    class function MonthSpan(const ANow, AThen: TDateTime): Double;
    class function Now: TDateTime;
    class function RecodeDate(const AValue: TDateTime; const AYear, AMonth, ADay: Word): TDateTime;
    class function RecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
    class function RecodeDay(const AValue: TDateTime; const ADay: Word): TDateTime;
    class function RecodeHour(const AValue: TDateTime; const AHour: Word): TDateTime;
    class function RecodeMilliSecond(const AValue: TDateTime; const AMilliSecond: Word): TDateTime;
    class function RecodeMinute(const AValue: TDateTime; const AMinute: Word): TDateTime;
    class function RecodeMonth(const AValue: TDateTime; const AMonth: Word): TDateTime;
    class function RecodeSecond(const AValue: TDateTime; const ASecond: Word): TDateTime;
    class function RecodeTime(const AValue: TDateTime; const AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;
    class function RecodeYear(const AValue: TDateTime; const AYear: Word): TDateTime;
    class procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);
    class procedure ReplaceTime(var DateTime: TDateTime; const NewTime: TDateTime);
    class function SameDate(const A, B: TDateTime): Boolean;
    class function SameDateTime(const A, B: TDateTime): Boolean;
    class function SameTime(const A, B: TDateTime): Boolean;

    class function SecondOf(const AValue: TDateTime): Word;
    class function SecondOfTheDay(const AValue: TDateTime): LongWord;
    class function SecondOfTheHour(const AValue: TDateTime): Word;
    class function SecondOfTheMinute(const AValue: TDateTime): Word;
    class function SecondOfTheMonth(const AValue: TDateTime): LongWord;
    class function SecondOfTheWeek(const AValue: TDateTime): LongWord;
    class function SecondOfTheYear(const AValue: TDateTime): LongWord;
    class function SecondsBetween(const ANow, AThen: TDateTime): Int64;
    class function SecondSpan(const ANow, AThen: TDateTime): Double;
    class function StartOfADay0(const AYear, ADayOfYear: Word): TDateTime;
    class function StartOfADay1(const AYear, AMonth, ADay: Word): TDateTime;
    class function StartOfAMonth(const AYear, AMonth: Word): TDateTime;
    class function StartOfAWeek0(const AYear, AWeekOfYear: Word; const ADayOfWeek: Word = 1): TDateTime;
    class function StartOfAWeek1(const AYear, AWeekOfYear: Word): TDateTime;
    class function StartOfAYear(const AYear: word): TDateTime;
    class function StartOfTheDay(const AValue: TDateTime): TDateTime;
    class function StartOfTheMonth(const AValue: TDateTime): TDateTime;
    class function StartOfTheWeek(const AValue: TDateTime): TDateTime;
    class function StartOfTheYear(const AValue: TDateTime): TDateTime;
    class function StrToDate(const S: string): TDateTime; 
    class function StrToDateDef(const S: string; const Default: TDateTime): TDateTime; 
    class function StrToDateTime(const S: string): TDateTime;
    class function StrToDateTimeDef(const S: string; const Default: TDateTime): TDateTime; 
    class function StrToTime(const S: string): TDateTime;
    class function StrToTimeDef(const S: string; const Default: TDateTime): TDateTime; 
    class function Time: TDateTime;
    class function TimeOf(const AValue: TDateTime): TDateTime;
    class function TimeToStr(Time: TDateTime): string;
    class function Today: TDateTime;
    class function Tomorrow: TDateTime;
    class function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
    class function TryEncodeDateDay(const AYear, ADayOfYear: Word; out AValue: TDateTime): Boolean;
    class function TryEncodeDateMonthWeek(const AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word; out AValue: TDateTime): Boolean;
    class function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; out AValue: TDateTime): Boolean;
    class function TryEncodeDateWeek0(const AYear, AWeekOfYear: Word; out AValue: TDateTime; const ADayOfWeek: Word = 1): Boolean;
    class function TryEncodeDateWeek1(const AYear, AWeekOfYear: Word; out AValue: TDateTime): Boolean;
    class function TryEncodeDayOfWeekInMonth(const AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word; out AValue: TDateTime): Boolean;
    class function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
    class function TryRecodeDateTime(const AValue: TDateTime; const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word; out AResult: TDateTime): Boolean;
    class function TryStrToDate(const S: string; out Value: TDateTime): Boolean; 
    class function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean;
    class function TryStrToTime(const S: string; out Value: TDateTime): Boolean;
    class function WeekOf(const AValue: TDateTime): Word;

    class function WeekOfTheMonth0(const AValue: TDateTime): Word;
    class function WeekOfTheMonth1(const AValue: TDateTime; var AYear, AMonth: Word): Word;
    class function WeekOfTheYear0(const AValue: TDateTime): Word;
    class function WeekOfTheYear1(const AValue: TDateTime; var AYear: word): Word;
    class function WeeksBetween(const ANow, AThen: TDateTime): Integer;
    class function WeeksInAYear(const AYear: Word): Word;
    class function WeeksInYear(const AValue: TDateTime): Word;
    class function WeekSpan(const ANow, AThen: TDateTime): Double;
    class function WithinPastDays(const ANow, AThen: TDateTime; const ADays: Integer): Boolean;
    class function WithinPastHours(const ANow, AThen: TDateTime; const AHours: Int64): Boolean;
    class function WithinPastMilliSeconds(const ANow, AThen: TDateTime; const AMilliSeconds: Int64): Boolean;
    class function WithinPastMinutes(const ANow, AThen: TDateTime; const AMinutes: Int64): Boolean;
    class function WithinPastMonths(const ANow, AThen: TDateTime; const AMonths: Integer): Boolean;
    class function WithinPastSeconds(const ANow, AThen: TDateTime; const ASeconds: Int64): Boolean;
    class function WithinPastWeeks(const ANow, AThen: TDateTime; const AWeeks: Integer): Boolean;
    class function WithinPastYears(const ANow, AThen: TDateTime; const AYears: Integer): Boolean;
    class function YearOf(const AValue: TDateTime): Word;
    class function YearsBetween(const ANow, AThen: TDateTime): Integer;
    class function YearSpan(const ANow, AThen: TDateTime): Double;
    class function Yesterday: TDateTime;
    {$IFNDEF FPC}
    class function DateTimeToUTCTime(DateTime: TDateTime): TDateTime;
    class function UTCTimeToDateTime(UTCTime: TDateTime): TDateTime;
    {$ENDIF}
  end;




procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
    Target.Method['DateTime.CompareDate[0]', C_UnitName] := @              DateTime.CompareDate;
    Target.Method['DateTime.CompareDateTime[0]', C_UnitName] := @          DateTime.CompareDateTime;
    Target.Method['DateTime.CompareTime[0]', C_UnitName] := @              DateTime.CompareTime;
    Target.Method['DateTime.CurrentYear[0]', C_UnitName] := @              DateTime.CurrentYear;
    Target.Method['DateTime.Date[0]', C_UnitName] := @                     DateTime.Date;
    Target.Method['DateTime.DateOf[0]', C_UnitName] := @                   DateTime.DateOf;
    Target.Method['DateTime.DateTimeToStr[0]', C_UnitName] := @            DateTime.DateTimeToStr;
    Target.Method['DateTime.DateToStr[0]', C_UnitName] := @                DateTime.DateToStr;
    Target.Method['DateTime.DayOf[0]', C_UnitName] := @                    DateTime.DayOf;
    Target.Method['DateTime.DayOfTheMonth[0]', C_UnitName] := @            DateTime.DayOfTheMonth;
    Target.Method['DateTime.DayOfTheWeek[0]', C_UnitName] := @             DateTime.DayOfTheWeek;
    Target.Method['DateTime.DayOfTheYear[0]', C_UnitName] := @             DateTime.DayOfTheYear;
    Target.Method['DateTime.DayOfWeek[0]', C_UnitName] := @                DateTime.DayOfWeek;
    Target.Method['DateTime.DaysBetween[0]', C_UnitName] := @              DateTime.DaysBetween;
    Target.Method['DateTime.DaysInAMonth[0]', C_UnitName] := @             DateTime.DaysInAMonth;
    Target.Method['DateTime.DaysInAYear[0]', C_UnitName] := @              DateTime.DaysInAYear;
    Target.Method['DateTime.DaysInMonth[0]', C_UnitName] := @              DateTime.DaysInMonth;
    Target.Method['DateTime.DaysInYear[0]', C_UnitName] := @               DateTime.DaysInYear;
    Target.Method['DateTime.DaySpan[0]', C_UnitName] := @                  DateTime.DaySpan;
    Target.Method['DateTime.DecodeDate[0]', C_UnitName] := @               DateTime.DecodeDate;
    Target.Method['DateTime.DecodeDateDay[0]', C_UnitName] := @            DateTime.DecodeDateDay;
    Target.Method['DateTime.DecodeDateFully[0]', C_UnitName] := @          DateTime.DecodeDateFully;
    Target.Method['DateTime.DecodeDateMonthWeek[0]', C_UnitName] := @      DateTime.DecodeDateMonthWeek;
    Target.Method['DateTime.DecodeDateTime[0]', C_UnitName] := @           DateTime.DecodeDateTime;
    Target.Method['DateTime.DecodeDateWeek[0]', C_UnitName] := @           DateTime.DecodeDateWeek;
    Target.Method['DateTime.DecodeDayOfWeekInMonth[0]', C_UnitName] := @   DateTime.DecodeDayOfWeekInMonth;
    Target.Method['DateTime.DecodeTime[0]', C_UnitName] := @               DateTime.DecodeTime;

    Target.Method['DateTime.EncodeDate[0]', C_UnitName] := @               DateTime.EncodeDate;
    Target.Method['DateTime.EncodeDateDay[0]', C_UnitName] := @            DateTime.EncodeDateDay;
    Target.Method['DateTime.EncodeDateMonthWeek[0]', C_UnitName] := @      DateTime.EncodeDateMonthWeek0;
    Target.Method['DateTime.EncodeDateMonthWeek[1]', C_UnitName] := @      DateTime.EncodeDateMonthWeek1;
    Target.Method['DateTime.EncodeDateTime[0]', C_UnitName] := @           DateTime.EncodeDateTime;
    Target.Method['DateTime.EncodeDateWeek[0]', C_UnitName] := @           DateTime.EncodeDateWeek0;
    Target.Method['DateTime.EncodeDateWeek[1]', C_UnitName] := @           DateTime.EncodeDateWeek1;
    Target.Method['DateTime.EncodeDayOfWeekInMonth[0]', C_UnitName] := @   DateTime.EncodeDayOfWeekInMonth;
    Target.Method['DateTime.EncodeTime[0]', C_UnitName] := @               DateTime.EncodeTime;
    Target.Method['DateTime.EndOfADay[0]', C_UnitName] := @                DateTime.EndOfADay0;
    Target.Method['DateTime.EndOfADay[1]', C_UnitName] := @                DateTime.EndOfADay1;
    Target.Method['DateTime.EndOfAMonth[0]', C_UnitName] := @              DateTime.EndOfAMonth;
    Target.Method['DateTime.EndOfAWeek[0]', C_UnitName] := @               DateTime.EndOfAWeek0;
    Target.Method['DateTime.EndOfAWeek[1]', C_UnitName] := @               DateTime.EndOfAWeek1;
    Target.Method['DateTime.EndOfAYear[0]', C_UnitName] := @               DateTime.EndOfAYear;
    Target.Method['DateTime.EndOfTheDay[0]', C_UnitName] := @              DateTime.EndOfTheDay;
    Target.Method['DateTime.EndOfTheMonth[0]', C_UnitName] := @            DateTime.EndOfTheMonth;
    Target.Method['DateTime.EndOfTheWeek[0]', C_UnitName] := @             DateTime.EndOfTheWeek;
    Target.Method['DateTime.EndOfTheYear[0]', C_UnitName] := @             DateTime.EndOfTheYear;
    Target.Method['DateTime.FormatDateTime[0]', C_UnitName] := @           DateTime.FormatDateTime;
    Target.Method['DateTime.HourOf[0]', C_UnitName] := @                   DateTime.HourOf;
    Target.Method['DateTime.HourOfTheDay[0]', C_UnitName] := @             DateTime.HourOfTheDay;
    Target.Method['DateTime.HourOfTheMonth[0]', C_UnitName] := @           DateTime.HourOfTheMonth;
    Target.Method['DateTime.HourOfTheWeek[0]', C_UnitName] := @            DateTime.HourOfTheWeek;
    Target.Method['DateTime.HourOfTheYear[0]', C_UnitName] := @            DateTime.HourOfTheYear;
    Target.Method['DateTime.HoursBetween[0]', C_UnitName] := @             DateTime.HoursBetween;
    Target.Method['DateTime.HourSpan[0]', C_UnitName] := @                 DateTime.HourSpan;
    Target.Method['DateTime.IncAMonth[0]', C_UnitName] := @                DateTime.IncAMonth0;
    Target.Method['DateTime.IncAMonth[1]', C_UnitName] := @                DateTime.IncAMonth1;
    Target.Method['DateTime.IncDay[0]', C_UnitName] := @                   DateTime.IncDay0;
    Target.Method['DateTime.IncDay[1]', C_UnitName] := @                   DateTime.IncDay1;
    Target.Method['DateTime.IncHour[0]', C_UnitName] := @                  DateTime.IncHour0;
    Target.Method['DateTime.IncHour[1]', C_UnitName] := @                  DateTime.IncHour1;
    Target.Method['DateTime.IncMilliSecond[0]', C_UnitName] := @           DateTime.IncMilliSecond0;
    Target.Method['DateTime.IncMilliSecond[1]', C_UnitName] := @           DateTime.IncMilliSecond1;
    Target.Method['DateTime.IncMinute[0]', C_UnitName] := @                DateTime.IncMinute0;
    Target.Method['DateTime.IncMinute[1]', C_UnitName] := @                DateTime.IncMinute1;
    Target.Method['DateTime.IncMonth[0]', C_UnitName] := @                 DateTime.IncMonth0;
    Target.Method['DateTime.IncMonth[1]', C_UnitName] := @                 DateTime.IncMonth1;
    Target.Method['DateTime.IncSecond[0]', C_UnitName] := @                DateTime.IncSecond0;
    Target.Method['DateTime.IncSecond[1]', C_UnitName] := @                DateTime.IncSecond1;
    Target.Method['DateTime.IncWeek[0]', C_UnitName] := @                  DateTime.IncWeek0;
    Target.Method['DateTime.IncWeek[1]', C_UnitName] := @                  DateTime.IncWeek1;
    Target.Method['DateTime.IncYear[0]', C_UnitName] := @                  DateTime.IncYear0;
    Target.Method['DateTime.IncYear[1]', C_UnitName] := @                  DateTime.IncYear1;
    Target.Method['DateTime.IsInLeapYear[0]', C_UnitName] := @             DateTime.IsInLeapYear;
    Target.Method['DateTime.IsLeapYear[0]', C_UnitName] := @               DateTime.IsLeapYear;
    Target.Method['DateTime.IsPM[0]', C_UnitName] := @                     DateTime.IsPM;
    Target.Method['DateTime.IsSameDay[0]', C_UnitName] := @                DateTime.IsSameDay;
    Target.Method['DateTime.IsToday[0]', C_UnitName] := @                  DateTime.IsToday;
    Target.Method['DateTime.IsValidDate[0]', C_UnitName] := @              DateTime.IsValidDate;
    Target.Method['DateTime.IsValidDateDay[0]', C_UnitName] := @           DateTime.IsValidDateDay;
    Target.Method['DateTime.IsValidDateMonthWeek[0]', C_UnitName] := @     DateTime.IsValidDateMonthWeek;
    Target.Method['DateTime.IsValidDateTime[0]', C_UnitName] := @          DateTime.IsValidDateTime;
    Target.Method['DateTime.IsValidDateWeek[0]', C_UnitName] := @          DateTime.IsValidDateWeek;
    Target.Method['DateTime.IsValidTime[0]', C_UnitName] := @              DateTime.IsValidTime;
    Target.Method['DateTime.MilliSecondOf[0]', C_UnitName] := @            DateTime.MilliSecondOf;
    Target.Method['DateTime.MilliSecondOfTheDay[0]', C_UnitName] := @      DateTime.MilliSecondOfTheDay;
    Target.Method['DateTime.MilliSecondOfTheHour[0]', C_UnitName] := @     DateTime.MilliSecondOfTheHour;
    Target.Method['DateTime.MilliSecondOfTheMinute[0]', C_UnitName] := @   DateTime.MilliSecondOfTheMinute;
    Target.Method['DateTime.MilliSecondOfTheMonth[0]', C_UnitName] := @    DateTime.MilliSecondOfTheMonth;
    Target.Method['DateTime.MilliSecondOfTheSecond[0]', C_UnitName] := @   DateTime.MilliSecondOfTheSecond;
    Target.Method['DateTime.MilliSecondOfTheWeek[0]', C_UnitName] := @     DateTime.MilliSecondOfTheWeek;
    Target.Method['DateTime.MilliSecondOfTheYear[0]', C_UnitName] := @     DateTime.MilliSecondOfTheYear;
    Target.Method['DateTime.MilliSecondsBetween[0]', C_UnitName] := @      DateTime.MilliSecondsBetween;
    Target.Method['DateTime.MilliSecondSpan[0]', C_UnitName] := @          DateTime.MilliSecondSpan;
    Target.Method['DateTime.MinuteOf[0]', C_UnitName] := @                 DateTime.MinuteOf;
    Target.Method['DateTime.MinuteOfTheHour[0]', C_UnitName] := @          DateTime.MinuteOfTheHour;
    Target.Method['DateTime.MinuteOfTheMonth[0]', C_UnitName] := @         DateTime.MinuteOfTheMonth;
    Target.Method['DateTime.MinuteOfTheWeek[0]', C_UnitName] := @          DateTime.MinuteOfTheWeek;
    Target.Method['DateTime.MinuteOfTheYear[0]', C_UnitName] := @          DateTime.MinuteOfTheYear;
    Target.Method['DateTime.MinutesBetween[0]', C_UnitName] := @           DateTime.MinutesBetween;
    Target.Method['DateTime.MinuteSpan[0]', C_UnitName] := @               DateTime.MinuteSpan;
    Target.Method['DateTime.MonthOf[0]', C_UnitName] := @                  DateTime.MonthOf;
    Target.Method['DateTime.MonthOfTheYear[0]', C_UnitName] := @           DateTime.MonthOfTheYear;
    Target.Method['DateTime.MonthsBetween[0]', C_UnitName] := @            DateTime.MonthsBetween;
    Target.Method['DateTime.MonthSpan[0]', C_UnitName] := @                DateTime.MonthSpan;
    Target.Method['DateTime.Now[0]', C_UnitName] := @                      DateTime.Now;
    Target.Method['DateTime.RecodeDate[0]', C_UnitName] := @               DateTime.RecodeDate;
    Target.Method['DateTime.RecodeDateTime[0]', C_UnitName] := @           DateTime.RecodeDateTime;
    Target.Method['DateTime.RecodeDay[0]', C_UnitName] := @                DateTime.RecodeDay;
    Target.Method['DateTime.RecodeHour[0]', C_UnitName] := @               DateTime.RecodeHour;
    Target.Method['DateTime.RecodeMilliSecond[0]', C_UnitName] := @        DateTime.RecodeMilliSecond;
    Target.Method['DateTime.RecodeMinute[0]', C_UnitName] := @             DateTime.RecodeMinute;
    Target.Method['DateTime.RecodeMonth[0]', C_UnitName] := @              DateTime.RecodeMonth;
    Target.Method['DateTime.RecodeSecond[0]', C_UnitName] := @             DateTime.RecodeSecond;
    Target.Method['DateTime.RecodeTime[0]', C_UnitName] := @               DateTime.RecodeTime;
    Target.Method['DateTime.RecodeYear[0]', C_UnitName] := @               DateTime.RecodeYear;
    Target.Method['DateTime.ReplaceDate[0]', C_UnitName] := @              DateTime.ReplaceDate;
    Target.Method['DateTime.ReplaceTime[0]', C_UnitName] := @              DateTime.ReplaceTime;
    Target.Method['DateTime.SameDate[0]', C_UnitName] := @                 DateTime.SameDate;
    Target.Method['DateTime.SameDateTime[0]', C_UnitName] := @             DateTime.SameDateTime;
    Target.Method['DateTime.SameTime[0]', C_UnitName] := @                 DateTime.SameTime;

    Target.Method['DateTime.SecondOf[0]', C_UnitName] := @                 DateTime.SecondOf;
    Target.Method['DateTime.SecondOfTheDay[0]', C_UnitName] := @           DateTime.SecondOfTheDay;
    Target.Method['DateTime.SecondOfTheHour[0]', C_UnitName] := @          DateTime.SecondOfTheHour;
    Target.Method['DateTime.SecondOfTheMinute[0]', C_UnitName] := @        DateTime.SecondOfTheMinute;
    Target.Method['DateTime.SecondOfTheMonth[0]', C_UnitName] := @         DateTime.SecondOfTheMonth;
    Target.Method['DateTime.SecondOfTheWeek[0]', C_UnitName] := @          DateTime.SecondOfTheWeek;
    Target.Method['DateTime.SecondOfTheYear[0]', C_UnitName] := @          DateTime.SecondOfTheYear;
    Target.Method['DateTime.SecondsBetween[0]', C_UnitName] := @           DateTime.SecondsBetween;
    Target.Method['DateTime.SecondSpan[0]', C_UnitName] := @               DateTime.SecondSpan;
    Target.Method['DateTime.StartOfADay[0]', C_UnitName] := @              DateTime.StartOfADay0;
    Target.Method['DateTime.StartOfADay[1]', C_UnitName] := @              DateTime.StartOfADay1;
    Target.Method['DateTime.StartOfAMonth[0]', C_UnitName] := @            DateTime.StartOfAMonth;
    Target.Method['DateTime.StartOfAWeek[0]', C_UnitName] := @             DateTime.StartOfAWeek0;
    Target.Method['DateTime.StartOfAWeek[1]', C_UnitName] := @             DateTime.StartOfAWeek1;
    Target.Method['DateTime.StartOfAYear[0]', C_UnitName] := @             DateTime.StartOfAYear;
    Target.Method['DateTime.StartOfTheDay[0]', C_UnitName] := @            DateTime.StartOfTheDay;
    Target.Method['DateTime.StartOfTheMonth[0]', C_UnitName] := @          DateTime.StartOfTheMonth;
    Target.Method['DateTime.StartOfTheWeek[0]', C_UnitName] := @           DateTime.StartOfTheWeek;
    Target.Method['DateTime.StartOfTheYear[0]', C_UnitName] := @           DateTime.StartOfTheYear;
    Target.Method['DateTime.StrToDate[0]', C_UnitName] := @                DateTime.StrToDate;
    Target.Method['DateTime.StrToDateDef[0]', C_UnitName] := @             DateTime.StrToDateDef;
    Target.Method['DateTime.StrToDateTime[0]', C_UnitName] := @            DateTime.StrToDateTime;
    Target.Method['DateTime.StrToDateTimeDef[0]', C_UnitName] := @         DateTime.StrToDateTimeDef;
    Target.Method['DateTime.StrToTime[0]', C_UnitName] := @                DateTime.StrToTime;
    Target.Method['DateTime.StrToTimeDef[0]', C_UnitName] := @             DateTime.StrToTimeDef;
    Target.Method['DateTime.Time[0]', C_UnitName] := @                     DateTime.Time;
    Target.Method['DateTime.TimeOf[0]', C_UnitName] := @                   DateTime.TimeOf;
    Target.Method['DateTime.TimeToStr[0]', C_UnitName] := @                DateTime.TimeToStr;
    Target.Method['DateTime.Today[0]', C_UnitName] := @                    DateTime.Today;
    Target.Method['DateTime.Tomorrow[0]', C_UnitName] := @                 DateTime.Tomorrow;
    Target.Method['DateTime.TryEncodeDate[0]', C_UnitName] := @            DateTime.TryEncodeDate;
    Target.Method['DateTime.TryEncodeDateDay[0]', C_UnitName] := @         DateTime.TryEncodeDateDay;
    Target.Method['DateTime.TryEncodeDateMonthWeek[0]', C_UnitName] := @   DateTime.TryEncodeDateMonthWeek;
    Target.Method['DateTime.TryEncodeDateTime[0]', C_UnitName] := @        DateTime.TryEncodeDateTime;
    Target.Method['DateTime.TryEncodeDateWeek[0]', C_UnitName] := @        DateTime.TryEncodeDateWeek0;
    Target.Method['DateTime.TryEncodeDateWeek[1]', C_UnitName] := @        DateTime.TryEncodeDateWeek1;
    Target.Method['DateTime.TryEncodeDayOfWeekInMonth[0]', C_UnitName] := @DateTime.TryEncodeDayOfWeekInMonth;
    Target.Method['DateTime.TryEncodeTime[0]', C_UnitName] := @            DateTime.TryEncodeTime;
    Target.Method['DateTime.TryRecodeDateTime[0]', C_UnitName] := @        DateTime.TryRecodeDateTime;
    Target.Method['DateTime.TryStrToDate[0]', C_UnitName] := @             DateTime.TryStrToDate;
    Target.Method['DateTime.TryStrToDateTime[0]', C_UnitName] := @         DateTime.TryStrToDateTime;
    Target.Method['DateTime.TryStrToTime[0]', C_UnitName] := @             DateTime.TryStrToTime;
    Target.Method['DateTime.WeekOf[0]', C_UnitName] := @                   DateTime.WeekOf;

    Target.Method['DateTime.WeekOfTheMonth[0]', C_UnitName] := @           DateTime.WeekOfTheMonth0;
    Target.Method['DateTime.WeekOfTheMonth[1]', C_UnitName] := @           DateTime.WeekOfTheMonth1;
    Target.Method['DateTime.WeekOfTheYear[0]', C_UnitName] := @            DateTime.WeekOfTheYear0;
    Target.Method['DateTime.WeekOfTheYear[1]', C_UnitName] := @            DateTime.WeekOfTheYear1;
    Target.Method['DateTime.WeeksBetween[0]', C_UnitName] := @             DateTime.WeeksBetween;
    Target.Method['DateTime.WeeksInAYear[0]', C_UnitName] := @             DateTime.WeeksInAYear;
    Target.Method['DateTime.WeeksInYear[0]', C_UnitName] := @              DateTime.WeeksInYear;
    Target.Method['DateTime.WeekSpan[0]', C_UnitName] := @                 DateTime.WeekSpan;
    Target.Method['DateTime.WithinPastDays[0]', C_UnitName] := @           DateTime.WithinPastDays;
    Target.Method['DateTime.WithinPastHours[0]', C_UnitName] := @          DateTime.WithinPastHours;
    Target.Method['DateTime.WithinPastMilliSeconds[0]', C_UnitName] := @   DateTime.WithinPastMilliSeconds;
    Target.Method['DateTime.WithinPastMinutes[0]', C_UnitName] := @        DateTime.WithinPastMinutes;
    Target.Method['DateTime.WithinPastMonths[0]', C_UnitName] := @         DateTime.WithinPastMonths;
    Target.Method['DateTime.WithinPastSeconds[0]', C_UnitName] := @        DateTime.WithinPastSeconds;
    Target.Method['DateTime.WithinPastWeeks[0]', C_UnitName] := @          DateTime.WithinPastWeeks;
    Target.Method['DateTime.WithinPastYears[0]', C_UnitName] := @          DateTime.WithinPastYears;
    Target.Method['DateTime.YearOf[0]', C_UnitName] := @                   DateTime.YearOf;
    Target.Method['DateTime.YearsBetween[0]', C_UnitName] := @             DateTime.YearsBetween;
    Target.Method['DateTime.YearSpan[0]', C_UnitName] := @                 DateTime.YearSpan;
    Target.Method['DateTime.Yesterday[0]', C_UnitName] := @                DateTime.Yesterday;

    {$IFNDEF FPC}
    Target.Method['DateTime.DateTimeToUTCTime[0]', C_UnitName] := @        DateTime.DateTimeToUTCTime;
    Target.Method['DateTime.UTCTimeToDateTime[0]', C_UnitName] := @        DateTime.UTCTimeToDateTime;
    {$ENDIF}

    // Convert
    
    Target.Method['Convert.DateTimeToStr[0]', C_UnitName] := @            DateTime.DateTimeToStr;
    Target.Method['Convert.DateToStr[0]', C_UnitName] := @                DateTime.DateToStr;
    Target.Method['Convert.TimeToStr[0]', C_UnitName] := @                DateTime.TimeToStr;
    
    Target.Method['Convert.StrToDate[0]', C_UnitName] := @                DateTime.StrToDate;
    Target.Method['Convert.StrToDateDef[0]', C_UnitName] := @             DateTime.StrToDateDef;
    Target.Method['Convert.StrToDateTime[0]', C_UnitName] := @            DateTime.StrToDateTime;
    Target.Method['Convert.StrToDateTimeDef[0]', C_UnitName] := @         DateTime.StrToDateTimeDef;
    Target.Method['Convert.StrToTime[0]', C_UnitName] := @                DateTime.StrToTime;
    Target.Method['Convert.StrToTimeDef[0]', C_UnitName] := @             DateTime.StrToTimeDef;
    
    Target.Method['Convert.TryStrToDate[0]', C_UnitName] := @             DateTime.TryStrToDate;
    Target.Method['Convert.TryStrToDateTime[0]', C_UnitName] := @         DateTime.TryStrToDateTime;
    Target.Method['Convert.TryStrToTime[0]', C_UnitName] := @             DateTime.TryStrToTime;
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

{ DateTime }

class function DateTime.CompareDate(const a, b: TDateTime): integer;
begin
  result := DateUtils.CompareDate(a, b);
end;

class function DateTime.CompareDateTime(const a, b: TDateTime): integer;
begin
  result := DateUtils.CompareDateTime(a, b);
end;

class function DateTime.CompareTime(const a, b: TDateTime): integer;
begin
  result := DateUtils.CompareTime(a, b);
end;

class function DateTime.CurrentYear: word;
begin
  result := SysUtils.CurrentYear;
end;

class function DateTime.Date: TDateTime;
begin
  result := SysUtils.Date;
end;

class function DateTime.DateOf(const Date: TDateTime): TDateTime;
begin
  result := DateUtils.DateOf(Date);
end;

class function DateTime.DateTimeToStr(const Date: TDateTime): string;
begin
  result := SysUtils.DateTimeToStr(Date);
end;

{$IFNDEF FPC}
class function DateTime.DateTimeToUTCTime(DateTime: TDateTime): TDateTime;
var Zone : TTimeZoneInformation;
begin
  case GetTimeZoneInformation(Zone) of
  TIME_ZONE_ID_STANDARD :
      result := DateTime + ((Zone.Bias) / 60 / 24);
  TIME_ZONE_ID_DAYLIGHT :
      result := DateTime + ((Zone.Bias + Zone.DaylightBias) / 60 / 24)
  else
      result := DateTime;
  end;
end;  

class function DateTime.UTCTimeToDateTime(UTCTime: TDateTime): TDateTime;
var Zone : TTimeZoneInformation;
begin
  case GetTimeZoneInformation(Zone) of
  TIME_ZONE_ID_STANDARD :
      result := UTCTime - ((Zone.Bias) / 60 / 24);
  TIME_ZONE_ID_DAYLIGHT :
      result := UTCTime - ((Zone.Bias + Zone.DaylightBias) / 60 / 24)
  else
      result := UTCTime;
  end;
end;
{$ENDIF}

class function DateTime.DateToStr(const Date: TDateTime): string;
begin
  result := SysUtils.DateToStr(Date);
end;

class function DateTime.DayOf(const Date: TDateTime): word;
begin
  result := DateUtils.DayOf(Date);
end;

class function DateTime.DayOfTheMonth(const Date: TDateTime): word;
begin
  result := DateUtils.DayOfTheMonth(Date);
end;

class function DateTime.DayOfTheWeek(const Date: TDateTime): word;
begin
  result := DateUtils.DayOfTheWeek(Date);
end;

class function DateTime.DayOfTheYear(const Date: TDateTime): word;
begin
  result := DateUtils.DayOfTheYear(Date);
end;

class function DateTime.DayOfWeek(const Date: TDateTime): integer;
begin
  result := SysUtils.DayOfWeek(Date);
end;

class function DateTime.DaysBetween(const d1, d2: TDateTime): integer;
begin
  result := DateUtils.DaysBetween(d1, d2);
end;

class function DateTime.DaysInAMonth(const Year, Month: word): word;
begin
  result := DateUtils.DaysInAMonth(Year, Month);
end;

class function DateTime.DaysInAYear(const Year: word): word;
begin
  result := DateUtils.DaysInAYear(Year);
end;

class function DateTime.DaysInMonth(const dt: TDateTime): word;
begin
  result := DateUtils.DaysInMonth(dt);
end;

class function DateTime.DaysInYear(const dt: TDateTime): word;
begin
  result := DateUtils.DaysInYear(dt);
end;

class function DateTime.DaySpan(const d1, d2: TDateTime): double;
begin
  result := DateUtils.DaySpan(d1, d2);
end;

class procedure DateTime.DecodeDate(dt: TDateTime; var Year, Month,
  Day: word);
begin
  SysUtils.DecodeDate(dt, year, month, day);
end;

class procedure DateTime.DecodeDateDay(dt: TDateTime; var Year,
  DayOfYear: word);
begin
  DateUtils.DecodeDateDay(dt, Year, DayOfYear);
end;

class function DateTime.DecodeDateFully(dt: TDateTime; var Year, Month,
  Day, DOW: word): boolean;
begin
  result := SysUtils.DecodeDateFully(dt, Year, Month, Day, DOW);
end;

class procedure DateTime.DecodeDateMonthWeek(const AValue: TDateTime;
  out Year, Month, WeekOfMonth, DayOfWeek: Word);
begin
  DateUtils.DecodeDateMonthWeek(AValue, Year, Month, WeekOfMonth, DayOfWeek);
end;

class procedure DateTime.DecodeDateTime(const AValue: TDateTime; out AYear,
  AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word);
begin
  DateUtils.DecodeDateTime(AValue, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
end;

class procedure DateTime.DecodeDateWeek(const AValue: TDateTime; out AYear,
  AWeekOfYear, ADayOfWeek: Word);
begin
  DateUtils.DecodeDateWeek(AValue, AYear, AWeekOfYear, ADayOfWeek);
end;

class procedure DateTime.DecodeDayOfWeekInMonth(const AValue: TDateTime;
  out AYear, AMonth, ANthDayOfWeek, ADayOfWeek: Word);
begin
  DateUtils.DecodeDayOfWeekInMonth(AValue, AYear, AMonth, ANthDayOfWeek, ADayOfWeek);
end;

class procedure DateTime.DecodeTime(Time: TDateTime; var Hour, Min, Sec,
  MSec: Word);
begin
  SysUtils.DecodeTime(Time, Hour, Min, Sec, MSec);
end;

class function DateTime.EncodeDate(Year, Month, Day: Word): TDateTime;
begin
  result := SysUtils.EncodeDate(Year, Month, Day)
end;

class function DateTime.EncodeDateDay(const AYear,
  ADayOfYear: Word): TDateTime;
begin
  result := DateUtils.EncodeDateDay(AYear, ADayOfYear)
end;

class function DateTime.EncodeDateMonthWeek0(const AYear, AMonth,
  AWeekOfMonth, ADayOfWeek: Word): TDateTime;
begin
  result := DateUtils.EncodeDateMonthWeek(AYear, AMonth, AWeekOfMonth, ADayOfWeek);
end;

class function DateTime.EncodeDateMonthWeek1(const AYear, AMonth,
  AWeekOfMonth: Word): TDateTime;
begin
  result := DateUtils.EncodeDateMonthWeek(AYear, AMonth, AWeekOfMonth, 1);
end;

class function DateTime.EncodeDateTime(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMilliSecond: Word): TDateTime;
begin
  result := DateUtils.EncodeDateTime(AYear, AMonth, ADay, AHour, AMonth, ASecond, AMilliSecond);
end;

class function DateTime.EncodeDateWeek0(const AYear, AWeekOfYear,
  ADayOfWeek: Word): TDateTime;
begin
  result := DateUtils.EncodeDateWeek(AYear, AWeekOfYear, ADayOfWeek);
end;

class function DateTime.EncodeDateWeek1(const AYear,
  AWeekOfYear: Word): TDateTime;
begin
  result := DateUtils.EncodeDateWeek(AYear, AWeekOfYear);
end;

class function DateTime.EncodeDayOfWeekInMonth(const AYear, AMonth,
  ANthDayOfWeek, ADayOfWeek: Word): TDateTime;
begin
  result := DateUtils.EncodeDayOfWeekInMonth(AYear, AMonth, ANthDayOfWeek, ADayOfWeek);
end;

class function DateTime.EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
begin
  result := SysUtils.EncodeTime(Hour, Min, Sec, MSec);
end;

class function DateTime.EndOfADay0(const AYear,
  ADayOfYear: Word): TDateTime;
begin
  result := DateUtils.EndOfADay(AYear, ADayOfYear);
end;

class function DateTime.EndOfADay1(const AYear, AMonth,
  ADay: Word): TDateTime;
begin
  result := DateUtils.EndOfADay(AYear, AMonth, ADay);
end;

class function DateTime.EndOfAMonth(const AYear, AMonth: Word): TDateTime;
begin
  result := DateUtils.EndOfAMonth(AYear, AMonth);
end;

class function DateTime.EndOfAWeek0(const AYear, AWeekOfYear,
  ADayOfWeek: Word): TDateTime;
begin
  result := DateUtils.EndOfAWeek(AYear, AWeekOfYear, ADayOfWeek);
end;

class function DateTime.EndOfAWeek1(const AYear,
  AWeekOfYear: Word): TDateTime;
begin
  result := DateUtils.EndOfAWeek(AYear, AWeekOfYear)
end;

class function DateTime.EndOfAYear(const AYear: word): TDateTime;
begin
  result := DateUtils.EndOfAYear(AYear);
end;

class function DateTime.EndOfTheDay(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.EndoFTheDay(AValue);
end;

class function DateTime.EndOfTheMonth(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.EndOfTheMonth(AValue);
end;

class function DateTime.EndOfTheWeek(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.EndOfTheWeek(AValue);
end;

class function DateTime.EndOfTheYear(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.EndOfTheYear(AValue);
end;

class function DateTime.FormatDateTime(const Format: string;
  DateTime: TDateTime): string;
begin
  result := SysUtils.FormatDateTime(Format, DateTime);
end;

class function DateTime.HourOf(const AValue: TDateTime): Word;
begin
  result := DateUtils.HourOf(AValue);
end;

class function DateTime.HourOfTheDay(const AValue: TDateTime): Word;
begin
  result := DateUtils.HourOfTheYear(AValue);
end;

class function DateTime.HourOfTheMonth(const AValue: TDateTime): Word;
begin
  result := DateUtils.HourOfTheMonth(AValue);
end;

class function DateTime.HourOfTheWeek(const AValue: TDateTime): Word;
begin
  result := DateUtils.HourOfTheWeek(AValue);
end;

class function DateTime.HourOfTheYear(const AValue: TDateTime): Word;
begin
  result := DateUtils.HourOfTheYear(AValue);
end;

class function DateTime.HoursBetween(const ANow, AThen: TDateTime): Int64;
begin
  result := DateUtils.HoursBetween(ANow, AThen);
end;

class function DateTime.HourSpan(const ANow, AThen: TDateTime): Double;
begin
  result := DateUtils.HourSpan(ANow, AThen);
end;

class procedure DateTime.IncAMonth0(var Year, Month, Day: Word;
  NumberOfMonths: Integer);
begin
  SysUtils.IncAMonth(Year, Month, Day, NumberOfMonths);
end;

class procedure DateTime.IncAMonth1(var Year, Month, Day: Word);
begin
  SysUtils.IncAMonth(Year, Month, Day);
end;

class function DateTime.IncDay0(const AValue: TDateTime;
  const ANumberOfDays: Integer): TDateTime;
begin
  result := DateUtils.IncDay(AValue, ANumberOfDays);
end;

class function DateTime.IncDay1(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.IncDay(AValue);
end;

class function DateTime.IncHour0(const AValue: TDateTime;
  const ANumberOfHours: Int64): TDateTime;
begin
  result := DateUtils.IncHour(AValue, ANumberOfHours);
end;

class function DateTime.IncHour1(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.IncHour(AValue);
end;

class function DateTime.IncMilliSecond0(const AValue: TDateTime;
  const ANumberOfMilliSeconds: Int64): TDateTime;
begin
  result := DateUtils.IncMilliSecond(AValue, ANumberOfMilliSeconds)
end;

class function DateTime.IncMilliSecond1(
  const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.IncMilliSecond(AValue);
end;

class function DateTime.IncMinute0(const AValue: TDateTime;
  const ANumberOfMinutes: Int64): TDateTime;
begin
  result := DateUtils.IncMinute(AValue, ANumberOfMinutes);
end;

class function DateTime.IncMinute1(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.IncMinute(AValue);
end;

class function DateTime.IncMonth0(const Date: TDateTime;
  NumberOfMonths: Integer): TDateTime;
begin
  result := SysUtils.IncMonth(Date, NumberOfMonths);
end;

class function DateTime.IncMonth1(const Date: TDateTime): TDateTime;
begin
  result := SysUtils.IncMonth(Date)
end;

class function DateTime.IncSecond0(const AValue: TDateTime;
  const ANumberOfSeconds: Int64): TDateTime;
begin
  result := DateUtils.IncSecond(AValue, ANumberOfSeconds);
end;

class function DateTime.IncSecond1(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.IncSecond(AValue)
end;

class function DateTime.IncWeek0(const AValue: TDateTime;
  const ANumberOfWeeks: Integer): TDateTime;
begin
  result := DateUtils.IncWeek(AValue, ANumberOfWeeks);
end;

class function DateTime.IncWeek1(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.IncWeek(AValue);
end;

class function DateTime.IncYear0(const AValue: TDateTime;
  const ANumberOfYears: Integer): TDateTime;
begin
  result := DateUtils.IncYear(AValue, ANumberOfYears);
end;

class function DateTime.IncYear1(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.IncYear(AValue);
end;

class function DateTime.IsInLeapYear(const AValue: TDateTime): Boolean;
begin
  result := DateUtils.IsInLeapYear(AValue);
end;

class function DateTime.IsLeapYear(Year: Word): Boolean;
begin
  result := SysUtils.IsLeapYear(Year);
end;

class function DateTime.IsPM(const AValue: TDateTime): Boolean;
begin
  result := DateUtils.IsPM(AValue);
end;

class function DateTime.IsSameDay(const AValue,
  ABasis: TDateTime): Boolean;
begin
  result := DateUtils.IsSameDay(AValue, ABasis);
end;

class function DateTime.IsToday(const AValue: TDateTime): Boolean;
begin
  result := DateUtils.IsToday(AValue);
end;

class function DateTime.IsValidDate(const AYear, AMonth,
  ADay: Word): Boolean;
begin
  result := DateUtils.IsValidDate(AYear, AMonth, ADay);
end;

class function DateTime.IsValidDateDay(const AYear,
  ADayOfYear: Word): Boolean;
begin
  result := DateUtils.IsValidDateDay(YearsPerDecade, ADayOfYear);
end;

class function DateTime.IsValidDateMonthWeek(const AYear, AMonth,
  AWeekOfMonth, ADayOfWeek: Word): Boolean;
begin
  result := DateUtils.IsValidDateMonthWeek(AYear, AMonth, AWeekOfMonth, ADayOfWeek);
end;

class function DateTime.IsValidDateTime(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMilliSecond: Word): Boolean;
begin
  result := DateUtils.IsValidDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
end;

class function DateTime.IsValidDateWeek(const AYear, AWeekOfYear,
  ADayOfWeek: Word): Boolean;
begin
  result := DateUtils.IsValidDateWeek(AYear, AWeekOfYear, ADayOfWeek);
end;

class function DateTime.IsValidTime(const AHour, AMinute, ASecond,
  AMilliSecond: Word): Boolean;
begin
  result := DateUtils.IsValidTime(AHour, AMinute, ASecond, AMilliSecond);
end;

class function DateTime.MilliSecondOf(const AValue: TDateTime): Word;
begin
  result := DateUtils.MilliSecondOf(AValue);
end;

class function DateTime.MilliSecondOfTheDay(
  const AValue: TDateTime): LongWord;
begin
  result := DateUtils.MilliSecondOfTheDay(AValue);
end;

class function DateTime.MilliSecondOfTheHour(
  const AValue: TDateTime): LongWord;
begin
  result := DateUtils.MilliSecondOfTheHour(AValue);
end;

class function DateTime.MilliSecondOfTheMinute(
  const AValue: TDateTime): LongWord;
begin
  result := DateUtils.MilliSecondOfTheMinute(AValue);
end;

class function DateTime.MilliSecondOfTheMonth(
  const AValue: TDateTime): LongWord;
begin
  result := DateUtils.MilliSecondOfTheMonth(AValue)
end;

class function DateTime.MilliSecondOfTheSecond(
  const AValue: TDateTime): Word;
begin
  result := DateUtils.MilliSecondOfTheSecond(AValue)
end;

class function DateTime.MilliSecondOfTheWeek(
  const AValue: TDateTime): LongWord;
begin
  result := DateUtils.MilliSecondOfTheWeek(AValue)
end;

class function DateTime.MilliSecondOfTheYear(
  const AValue: TDateTime): Int64;
begin
  result := DateUtils.MilliSecondOfTheYear(AValue)
end;

class function DateTime.MilliSecondsBetween(const ANow,
  AThen: TDateTime): Int64;
begin
  result := DateUtils.MilliSecondsBetween(ANow, AThen);
end;

class function DateTime.MilliSecondSpan(const ANow,
  AThen: TDateTime): Double;
begin
  result := DateUtils.MilliSecondSpan(ANow, AThen);
end;

class function DateTime.MinuteOf(const AValue: TDateTime): Word;
begin
  result := DateUtils.MinuteOf(AValue);
end;

class function DateTime.MinuteOfTheHour(const AValue: TDateTime): Word;
begin
  result := DateUtils.MinuteOfTheHour(AValue)
end;

class function DateTime.MinuteOfTheMonth(const AValue: TDateTime): Word;
begin
  result := DateUtils.MinuteOfTheMonth(AValue)
end;

class function DateTime.MinuteOfTheWeek(const AValue: TDateTime): Word;
begin
  result := DateUtils.MinuteOfTheWeek(AValue)
end;

class function DateTime.MinuteOfTheYear(const AValue: TDateTime): LongWord;
begin
  result := DateUtils.MinuteOfTheYear(AValue)
end;

class function DateTime.MinutesBetween(const ANow,
  AThen: TDateTime): Int64;
begin
  result := DateUtils.MinutesBetween(ANow, AThen)
end;

class function DateTime.MinuteSpan(const ANow, AThen: TDateTime): Double;
begin
  result := DateUtils.MinuteSpan(ANow, AThen)
end;

class function DateTime.MonthOf(const AValue: TDateTime): Word;
begin
  result := DateUtils.MonthOf(AValue)
end;

class function DateTime.MonthOfTheYear(const AValue: TDateTime): Word;
begin
  result := DateUtils.MonthOfTheYear(AValue)
end;

class function DateTime.MonthsBetween(const ANow,
  AThen: TDateTime): Integer;
begin
  result := DateUtils.MonthsBetween(ANow, AThen)
end;

class function DateTime.MonthSpan(const ANow, AThen: TDateTime): Double;
begin
  result := DateUtils.MonthSpan(ANow, AThen)
end;

class function DateTime.Now: TDateTime;
begin
  result := SysUtils.Now;
  result := result;

end;

class function DateTime.RecodeDate(const AValue: TDateTime; const AYear,
  AMonth, ADay: Word): TDateTime;
begin
  result := DateUtils.RecodeDate(AValue, AYear, AMonth, ADay);
end;

class function DateTime.RecodeDateTime(const AValue: TDateTime;
  const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word): TDateTime;
begin
  result := DateUtils.RecodeDateTime(AValue, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond)
end;

class function DateTime.RecodeDay(const AValue: TDateTime;
  const ADay: Word): TDateTime;
begin
  result := DateUtils.RecodeDay(AValue, ADay)
end;

class function DateTime.RecodeHour(const AValue: TDateTime;
  const AHour: Word): TDateTime;
begin
  result := DateUtils.RecodeHour(AValue, AHour)
end;

class function DateTime.RecodeMilliSecond(const AValue: TDateTime;
  const AMilliSecond: Word): TDateTime;
begin
  result := DateUtils.RecodeMilliSecond(AValue, AMilliSecond)
end;

class function DateTime.RecodeMinute(const AValue: TDateTime;
  const AMinute: Word): TDateTime;
begin
  result := DateUtils.RecodeMinute(AValue, AMinute)
end;

class function DateTime.RecodeMonth(const AValue: TDateTime;
  const AMonth: Word): TDateTime;
begin
  result := DateUtils.RecodeMonth(AValue, AMonth)
end;

class function DateTime.RecodeSecond(const AValue: TDateTime;
  const ASecond: Word): TDateTime;
begin
  result := DateUtils.RecodeSecond(AValue, ASecond)
end;

class function DateTime.RecodeTime(const AValue: TDateTime; const AHour,
  AMinute, ASecond, AMilliSecond: Word): TDateTime;
begin
  result := DateUtils.RecodeTime(AValue, AHour, AMinute, ASecond, AMilliSecond)
end;

class function DateTime.RecodeYear(const AValue: TDateTime;
  const AYear: Word): TDateTime;
begin
  result := DateUtils.RecodeYear(AValue, AYear)
end;

class procedure DateTime.ReplaceDate(var DateTime: TDateTime;
  const NewDate: TDateTime);
begin
  SysUtils.ReplaceDate(DateTime, NewDate);
end;

class procedure DateTime.ReplaceTime(var DateTime: TDateTime;
  const NewTime: TDateTime);
begin
  SysUtils.ReplaceTime(DateTime, NewTime);
end;

class function DateTime.SameDate(const A, B: TDateTime): Boolean;
begin
  result := DateUtils.SameDate(A, B)
end;

class function DateTime.SameDateTime(const A, B: TDateTime): Boolean;
begin
  result := DateUtils.SameDateTime(A, B)
end;

class function DateTime.SameTime(const A, B: TDateTime): Boolean;
begin
  result := DateUtils.SameTime(A, B);
end;

class function DateTime.SecondOf(const AValue: TDateTime): Word;
begin
  result := DateUtils.SecondOf(AValue)
end;

class function DateTime.SecondOfTheDay(const AValue: TDateTime): LongWord;
begin
  result := DateUtils.SecondOfTheDay(AValue)
end;

class function DateTime.SecondOfTheHour(const AValue: TDateTime): Word;
begin
  result := DateUtils.SecondOfTheHour(AValue)
end;

class function DateTime.SecondOfTheMinute(const AValue: TDateTime): Word;
begin
  result := DateUtils.SecondOfTheMinute(AValue)
end;

class function DateTime.SecondOfTheMonth(
  const AValue: TDateTime): LongWord;
begin
  result := DateUtils.SecondOfTheMonth(AValue)
end;

class function DateTime.SecondOfTheWeek(const AValue: TDateTime): LongWord;
begin
  result := DateUtils.SecondOfTheWeek(AValue)
end;

class function DateTime.SecondOfTheYear(const AValue: TDateTime): LongWord;
begin
  result := DateUtils.SecondOfTheYear(AValue)
end;

class function DateTime.SecondsBetween(const ANow,
  AThen: TDateTime): Int64;
begin
  result := DateUtils.SecondsBetween(ANow, AThen)
end;

class function DateTime.SecondSpan(const ANow, AThen: TDateTime): Double;
begin
  result := DateUtils.SecondSpan(ANow, AThen)
end;

class function DateTime.StartOfADay0(const AYear,
  ADayOfYear: Word): TDateTime;
begin
  result := DateUtils.StartOfADay(AYear, ADayOfYear);
end;

class function DateTime.StartOfADay1(const AYear, AMonth,
  ADay: Word): TDateTime;
begin
  result := DateUtils.StartOfADay(AYear, AMonth, ADay)
end;

class function DateTime.StartOfAMonth(const AYear,
  AMonth: Word): TDateTime;
begin
  result := DateUtils.StartOfAMonth(AYear, AMonth)
end;

class function DateTime.StartOfAWeek0(const AYear, AWeekOfYear,
  ADayOfWeek: Word): TDateTime;
begin
  result := DateUtils.StartOfAWeek(AYear, AWeekOfYear, ADayOfWeek)
end;

class function DateTime.StartOfAWeek1(const AYear,
  AWeekOfYear: Word): TDateTime;
begin
  result := DateUtils.StartOfAWeek(AYear, AWeekOfYear)
end;

class function DateTime.StartOfAYear(const AYear: word): TDateTime;
begin
  result := DateUtils.StartOfAYear(AYear)
end;

class function DateTime.StartOfTheDay(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.StartOfTheDay(AValue)
end;

class function DateTime.StartOfTheMonth(
  const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.StartOfTheMonth(AValue)
end;

class function DateTime.StartOfTheWeek(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.StartOfTheWeek(AValue)
end;

class function DateTime.StartOfTheYear(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.StartOfTheYear(AValue)
end;

class function DateTime.StrToDate(const S: string): TDateTime;
begin
  result := SysUtils.StrToDate(s);
end;

class function DateTime.StrToDateDef(const S: string;
  const Default: TDateTime): TDateTime;
begin
  result := SysUtils.StrToDateDef(S, Default);
end;

class function DateTime.StrToDateTime(const S: string): TDateTime;
begin
  result := SysUtils.StrToDateTime(s);
end;

class function DateTime.StrToDateTimeDef(const S: string;
  const Default: TDateTime): TDateTime;
begin
  result := SysUtils.StrToDateTimeDef(s, Default);
end;

class function DateTime.StrToTime(const S: string): TDateTime;
begin
  result := SysUtils.StrToTime(s);
end;

class function DateTime.StrToTimeDef(const S: string;
  const Default: TDateTime): TDateTime;
begin
  result := SysUtils.StrToTimeDef(s, Default)
end;

class function DateTime.Time: TDateTime;
begin
  result := SysUtils.Time;
end;

class function DateTime.TimeOf(const AValue: TDateTime): TDateTime;
begin
  result := DateUtils.TimeOf(AValue)
end;

class function DateTime.TimeToStr(Time: TDateTime): string;
begin
  result := SysUtils.TimeToStr(Time);
end;

class function DateTime.Today: TDateTime;
begin
  result := DateUtils.Today;
end;

class function DateTime.Tomorrow: TDateTime;
begin
  result := DateUtils.Tomorrow;
end;

class function DateTime.TryEncodeDate(Year, Month, Day: Word;
  out Date: TDateTime): Boolean;
begin
  result := SysUtils.TryEncodeDate(Year, Month, Day, Date)
end;

class function DateTime.TryEncodeDateDay(const AYear, ADayOfYear: Word;
  out AValue: TDateTime): Boolean;
begin
  result := DateUtils.TryEncodeDateDay(AYear, ADayOfYear, AValue)
end;

class function DateTime.TryEncodeDateMonthWeek(const AYear, AMonth,
  AWeekOfMonth, ADayOfWeek: Word; out AValue: TDateTime): Boolean;
begin
  result := DateUtils.TryEncodeDateMonthWeek(AYear, AMonth, AWeekOfMonth, ADayOfWeek, AValue)
end;

class function DateTime.TryEncodeDateTime(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMilliSecond: Word; out AValue: TDateTime): Boolean;
begin
  result := DateUtils.TryEncodeDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, AValue)
end;

class function DateTime.TryEncodeDateWeek0(const AYear, AWeekOfYear: Word;
  out AValue: TDateTime; const ADayOfWeek: Word): Boolean;
begin
  result := DateUtils.TryEncodeDateWeek(AYear, AWeekOfYear, AValue, ADayOfWeek)
end;

class function DateTime.TryEncodeDateWeek1(const AYear, AWeekOfYear: Word;
  out AValue: TDateTime): Boolean;
begin
  result := DateUtils.TryEncodeDateWeek(AYear, AWeekOfYear, AValue)
end;

class function DateTime.TryEncodeDayOfWeekInMonth(const AYear, AMonth,
  ANthDayOfWeek, ADayOfWeek: Word; out AValue: TDateTime): Boolean;
begin
  result := DateUtils.TryEncodeDayOfWeekInMonth(AYear, AMonth, ANthDayOfWeek, ADayOfWeek, AValue);
end;

class function DateTime.TryEncodeTime(Hour, Min, Sec, MSec: Word;
  out Time: TDateTime): Boolean;
begin
  result := SysUtils.TryEncodeTime(Hour, Min, Sec, MSec, Time)
end;

class function DateTime.TryRecodeDateTime(const AValue: TDateTime;
  const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  out AResult: TDateTime): Boolean;
begin
  result := DateUtils.TryRecodeDateTime(AValue, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, AResult);
end;

class function DateTime.TryStrToDate(const S: string;
  out Value: TDateTime): Boolean;
begin
  result := SysUtils.TryStrToDate(s, Value)
end;

class function DateTime.TryStrToDateTime(const S: string;
  out Value: TDateTime): Boolean;
begin
  result := SysUtils.TryStrToDateTime(s, Value)
end;

class function DateTime.TryStrToTime(const S: string;
  out Value: TDateTime): Boolean;
begin
  result := SysUtils.TryStrToTime(s, Value)
end;

class function DateTime.WeekOf(const AValue: TDateTime): Word;
begin
  result := DateUtils.WeekOf(AValue)
end;

class function DateTime.WeekOfTheMonth0(const AValue: TDateTime): Word;
begin
  result := DateUtils.WeekOfTheMonth(AValue)
end;

class function DateTime.WeekOfTheMonth1(const AValue: TDateTime; var AYear,
  AMonth: Word): Word;
begin
  result := DateUtils.WeekOfTheMonth(AValue, AYear, AMonth)
end;

class function DateTime.WeekOfTheYear0(const AValue: TDateTime): Word;
begin
  result := DateUtils.WeekOfTheYear(AValue)
end;

class function DateTime.WeekOfTheYear1(const AValue: TDateTime;
  var AYear: word): Word;
begin
  result := DateUtils.WeekOfTheYear(AValue, AYear)
end;

class function DateTime.WeeksBetween(const ANow,
  AThen: TDateTime): Integer;
begin
  result := DateUtils.WeeksBetween(ANow, AThen)
end;

class function DateTime.WeeksInAYear(const AYear: Word): Word;
begin
  result := DateUtils.WeeksInYear(AYear)
end;

class function DateTime.WeeksInYear(const AValue: TDateTime): Word;
begin
  result := DateUtils.WeeksInYear(AValue)
end;

class function DateTime.WeekSpan(const ANow, AThen: TDateTime): Double;
begin
  result := DateUtils.WeekSpan(ANow, AThen)
end;

class function DateTime.WithinPastDays(const ANow, AThen: TDateTime;
  const ADays: Integer): Boolean;
begin
  result := DateUtils.WithinPastDays(ANow, AThen, ADays)
end;

class function DateTime.WithinPastHours(const ANow, AThen: TDateTime;
  const AHours: Int64): Boolean;
begin
  result := DateUtils.WithinPastHours(ANow, AThen, AHours)
end;

class function DateTime.WithinPastMilliSeconds(const ANow,
  AThen: TDateTime; const AMilliSeconds: Int64): Boolean;
begin
  result := DateUtils.WithinPastMilliSeconds(ANow, AThen, AMilliSeconds)
end;

class function DateTime.WithinPastMinutes(const ANow, AThen: TDateTime;
  const AMinutes: Int64): Boolean;
begin
  result := DateUtils.WithinPastMinutes(ANow, AThen, AMinutes)
end;

class function DateTime.WithinPastMonths(const ANow, AThen: TDateTime;
  const AMonths: Integer): Boolean;
begin
  result := DateUtils.WithinPastMonths(ANow, AThen, AMonths)
end;

class function DateTime.WithinPastSeconds(const ANow, AThen: TDateTime;
  const ASeconds: Int64): Boolean;
begin
  result := DateUtils.WithinPastSeconds(ANow, AThen, ASeconds)
end;

class function DateTime.WithinPastWeeks(const ANow, AThen: TDateTime;
  const AWeeks: Integer): Boolean;
begin
  result := DateTime.WithinPastWeeks(ANow, AThen, AWeeks)
end;

class function DateTime.WithinPastYears(const ANow, AThen: TDateTime;
  const AYears: Integer): Boolean;
begin
  result := DateUtils.WithinPastYears(ANow, AThen, AYears)
end;

class function DateTime.YearOf(const AValue: TDateTime): Word;
begin
  result := DateUtils.YearOf(AValue)
end;

class function DateTime.YearsBetween(const ANow,
  AThen: TDateTime): Integer;
begin
  result := DateUtils.YearsBetween(ANow, AThen)
end;

class function DateTime.YearSpan(const ANow, AThen: TDateTime): Double;
begin
  result := DateUtils.YearSpan(ANow, AThen)
end;

class function DateTime.Yesterday: TDateTime;
begin
  result := DateUtils.Yesterday;
end;

initialization
  RegisterUnit;

end.
