unit uSE2IncStrings;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, StrUtils, uSE2Types, uSE2BaseTypes, uSE2RunAccess, uSE2UnitManager;

implementation

uses
  uSE2RunType;

{$IFDEF SEII_FPC}
  {$HINTS OFF}
  {$WARNINGS OFF}
{$ENDIF}

const
  C_UnitName   = 'System';
  C_UnitSource =
      'unit System;'+#13#10+
      #13#10+
      'interface'+#13#10+
      #13#10+
      'type'+#13#10+
      '  TReplaceFlags       = set of (rfReplaceAll, rfIgnoreCase);'+#13#10+
      '  TTextLineBreakStyle = (tlbsLF, tlbsCRLF);'+#13#10+
      {$IFDEF SEII_USE_SOUND_EX}
      '  TSoundExLength      = cardinal;'+#13#10+
      {$ENDIF}
      #13#10+
      '  TStringEncoding = (encodingAnsi, encodingUTF8, encodingUTF16, encodingUTF32);'+#13#10+
      #13#10+
      '  /// Provides basic string routines'+#13#10+
      '  Strings = sealed partial class(TExternalObject)'+#13#10+
      '  public'+#13#10+
      '    /// Returns the encoding, "System.String" is encoded'+#13#10+
      '    class function DefaultEncoding: TStringEncoding; external;'+#13#10+
      '    /// Converts the first character of the string into the corresponding ascii-index'+#13#10+
      '    class function ToASCIIIndex(const s: string): word; external;'+#13#10+
      '    /// Converts a given ascii-index to the corresponding character'+#13#10+
      '    class function FromASCIIIndex(index: word): string; external;'+#13#10+
      '    class function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string; external;'+#13#10+
      '    /// Compares two strings case sensitive. Returns 0 if the strings are equal'+#13#10+
      '    class function CompareStr(const S1, S2: string): Integer; external;'+#13#10+
      '    /// Compares two strings case insensitive. Returns 0 if the strings are equal'+#13#10+
      '    class function CompareText(const S1, S2: string): Integer; external;'+#13#10+
      '    /// Copy a substring out of "s", starting at "index" and with "count" characters'+#13#10+
      '    class function Copy(const s: string; Index, Count: Integer): string; external;'+#13#10+
      {$IFDEF SEII_USE_SOUND_EX}
      '    class function DecodeSoundExWord(AValue: Word): string; external;'+#13#10+
      {$ENDIF}
      '    /// Delete a part of the string, starting at "index"'+#13#10+
      '    class procedure Delete(var S: string; Index, Count:Integer); external;'+#13#10+
      '    /// Repeats the input string "ACount" - Times'+#13#10+
      '    class function Duplicate(const AText: string; ACount: Integer): string; external;'+#13#10+
      '    class procedure Insert(SubStr: string; var S: string; Index: Integer); external;'+#13#10+
      '    class function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean; external;'+#13#10+
      '    class function LastDelimiter(const Delimiters, S: string): Integer; external;'+#13#10+
      '    class function LeftBStr(const AText: string; const AByteCount: Integer): string; external;'+#13#10+
      '    class function LeftStr(const AText: string; const ACount: Integer): string; external;'+#13#10+
      '    /// Returns the length of the given string'+#13#10+
      '    class function Length(const s: string): Integer; external;'+#13#10+
      '    /// Converts the given string to lower case characters'+#13#10+
      '    class function LowerCase(const S: string): string; external;'+#13#10+
      '    class function MidBStr(const AText: string; const AByteStart, AByteCount: Integer): string; external;'+#13#10+
      '    class function MidStr(const AText: string; const AStart, ACount: Integer): string; external;'+#13#10+
      '    /// Search for "substr" in "s". Returns the first start index if "substr" was found, otherwise 0'+#13#10+
      '    class function Pos(Substr: string; S: string): Integer; external;'+#13#10+
      '    class function PosEx(const SubStr, S: string; Offset: Cardinal): Integer; overload; external;'+#13#10+
      '    /// Search for "substr" in "s" starting at "offset"'+#13#10+
      '    class function PosEx(const SubStr, S: string): Integer; overload; external;'+#13#10+
      '    class function QuotedStr(const S: string): string; external;'+#13#10+
      '    class function ReverseString(const AText: string): string; external;'+#13#10+

      '    class procedure SetLength(var s: string; NewLength: integer; ClearContent: boolean); overload; external;'+#13#10+
      '    /// Change the length of a given string to "NewLength"'+#13#10+
      '    class procedure SetLength(var S: string; NewLength: Integer); overload; external;'+#13#10+

      '    /// Returns true, if the strings are equal (case sensitive)'+#13#10+
      '    class function SameStr(const s1, s2: string): boolean; external;'+#13#10+
      '    /// Returns true, if the strings are equal (case insensitive)'+#13#10+
      '    class function SameText(const S1, S2: string): Boolean; external;'+#13#10+
      '    class function RightStr(const AText: string; const ACount: Integer): string; external;'+#13#10+
      '    class function RightBStr(const AText: string; const AByteCount: Integer): string; external;'+#13#10+

      {$IFDEF SEII_USE_SOUND_EX}
      '    class function SoundEx(const AText: string; ALength: TSoundExLength): string; overload; external;'+#13#10+
      '    class function SoundEx(const AText: string): string; overload; external;'+#13#10+
      '    class function SoundExCompare(const AText, AOther: string; ALength: TSoundExLength): Integer; overload; external;'+#13#10+
      '    class function SoundExCompare(const AText, AOther: string): Integer; overload; external;'+#13#10+
      '    class function SoundExProc(const AText, AOther: string): Boolean; external;'+#13#10+
      '    class function SoundExSimilar(const AText, AOther: string; ALength: TSoundExLength): Boolean; overload; external;'+#13#10+
      '    class function SoundExSimilar(const AText, AOther: string): Boolean; overload; external;'+#13#10+
      '    class function SoundExWord(const AText: string): Word; external;'+#13#10+
      {$ENDIF}

      '    /// Replace "OldPattern" with "NewPattern" inside "s"'+#13#10+
      '    class function Replace(const s: string; OldPattern, NewPattern: string; Flags: TReplaceFlags): string; external;'+#13#10+
      '    /// Overwrites the content in "AText" with "ASubText", starting at "AStart"'+#13#10+
      '    class function StuffString(const AText: string; AStart, ALength: Cardinal; const ASubText: string): string; external;'+#13#10+
      '    /// Removes every white space and control characters at the beginning and at the end of the string'+#13#10+
      '    class function Trim(const Text: string): string; external;'+#13#10+
      '    /// Removes every white space and control characters at the beginning of the string'+#13#10+
      '    class function TrimLeft(const Text: string): string; external;'+#13#10+
      '    /// Removes every white space and control characters at the end of the string'+#13#10+
      '    class function TrimRight(const Text: string): string; external;'+#13#10+
      '    /// Convert the given string to upper case characters'+#13#10+
      '    class function UpperCase(const Text: string): string; external;'+#13#10+
      '    class function WrapText(const Text: string; MaxCol: Integer): string; external;'+#13#10+
      '  end;'+#13#10+
      #13#10+

      '  AnsiStrings = sealed partial class(TExternalObject)' + #13#10 + 
      '  public' + #13#10 +
      '    /// Converts the first character of the string into the corresponding ascii-index' + #13#10 + 
      '    class function ToASCIIIndex(const s: AnsiString): word; external;' + #13#10 + 
      '    /// Converts a given ascii-index to the corresponding character' + #13#10 + 
      '    class function FromASCIIIndex(index: word): AnsiString; external;' + #13#10 + 
      '    class function AdjustLineBreaks(const S: AnsiString; Style: TTextLineBreakStyle): AnsiString; external;' + #13#10 + 
      '    /// Compares two strings case sensitive. Returns 0 if the strings are equal' + #13#10 + 
      '    class function CompareStr(const S1, S2: AnsiString): Integer; external;' + #13#10 + 
      '    /// Compares two strings case insensitive. Returns 0 if the strings are equal' + #13#10 + 
      '    class function CompareText(const S1, S2: AnsiString): Integer; external;' + #13#10 + 
      '    /// Copy a substring out of "s", starting at "index" and with "count" characters' + #13#10 + 
      '    class function Copy(const s: AnsiString; Index, Count: Integer): AnsiString; external;' + #13#10 + 
      '    /// Delete a part of the string, starting at "index"' + #13#10 + 
      '    class procedure Delete(var S: AnsiString; Index, Count:Integer); external;' + #13#10 + 
      '    /// Repeats the input string "ACount" - Times' + #13#10 + 
      '    class function Duplicate(const AText: AnsiString; ACount: Integer): AnsiString; external;' + #13#10 + 
      '    class procedure Insert(SubStr: AnsiString; var S: AnsiString; Index: Integer); external;' + #13#10 + 
      '    class function IsDelimiter(const Delimiters, S: AnsiString; Index: Integer): Boolean; external;' + #13#10 + 
      '    class function LastDelimiter(const Delimiters, S: AnsiString): Integer; external;' + #13#10 + 
      '    class function LeftBStr(const AText: AnsiString; const AByteCount: Integer): AnsiString; external;' + #13#10 + 
      '    class function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString; external;' + #13#10 + 
      '    /// Returns the length of the given string' + #13#10 + 
      '    class function Length(const s: AnsiString): Integer; external;' + #13#10 + 
      '    /// Converts the given string to lower case characters' + #13#10 + 
      '    class function LowerCase(const S: AnsiString): AnsiString; external;' + #13#10 + 
      '    class function MidBStr(const AText: AnsiString; const AByteStart, AByteCount: Integer): AnsiString; external;' + #13#10 + 
      '    class function MidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString; external;' + #13#10 + 
      '    /// Search for "substr" in "s". Returns the first start index if "substr" was found, otherwise 0' + #13#10 + 
      '    class function Pos(Substr: AnsiString; S: AnsiString): Integer; external;' + #13#10 + 
      '    class function PosEx(SubStr, S: AnsiString; Offset: Cardinal): Integer; overload; external;' + #13#10 + 
      '    /// Search for "substr" in "s" starting at "offset"' + #13#10 + 
      '    class function PosEx(const SubStr, S: AnsiString): Integer; overload; external;' + #13#10 + 
      '    class function QuotedStr(const S: AnsiString): AnsiString; external;' + #13#10 + 
      '    class function ReverseString(const AText: AnsiString): AnsiString; external;' + #13#10 + 
      '    class procedure SetLength(var s: AnsiString; NewLength: integer; ClearContent: boolean); overload; external;' + #13#10 + 
      '    /// Change the length of a given string to "NewLength"' + #13#10 + 
      '    class procedure SetLength(var S: AnsiString; NewLength: Integer); overload; external;' + #13#10 + 
      '    /// Returns true, if the strings are equal (case sensitive)' + #13#10 + 
      '    class function SameStr(const s1, s2: AnsiString): boolean; external;' + #13#10 + 
      '    /// Returns true, if the strings are equal (case insensitive)' + #13#10 + 
      '    class function SameText(const S1, S2: AnsiString): Boolean; external;' + #13#10 + 
      '    class function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString; external;' + #13#10 + 
      '    class function RightBStr(const AText: AnsiString; const AByteCount: Integer): AnsiString; external;' + #13#10 + 
      '    /// Replace "OldPattern" with "NewPattern" inside "s"' + #13#10 + 
      '    class function Replace(const s: AnsiString; OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString; external;' + #13#10 + 
      '    /// Overwrites the content in "AText" with "ASubText", starting at "AStart"' + #13#10 + 
      '    class function StuffString(const AText: AnsiString; AStart, ALength: Cardinal; const ASubText: AnsiString): AnsiString; external;' + #13#10 + 
      '    /// Removes every white space and control characters at the beginning and at the end of the string' + #13#10 + 
      '    class function Trim(const Text: AnsiString): AnsiString; external;' + #13#10 + 
      '    /// Removes every white space and control characters at the beginning of the string' + #13#10 + 
      '    class function TrimLeft(const Text: AnsiString): AnsiString; external;' + #13#10 + 
      '    /// Removes every white space and control characters at the end of the string' + #13#10 + 
      '    class function TrimRight(const Text: AnsiString): AnsiString; external;' + #13#10 + 
      '    /// Convert the given string to upper case characters' + #13#10 + 
      '    class function UpperCase(const Text: AnsiString): AnsiString; external;' + #13#10 + 
      '    class function WrapText(const Text: AnsiString; MaxCol: Integer): AnsiString; external;' + #13#10 + 
      '  end;' + #13#10 +

      #13#10+
      '  WideStrings = sealed partial class(TExternalObject)'+#13#10+
      '  public'+#13#10+
      '    class function Copy(const s: WideString; Index, Count: Integer): WideString; external;'+#13#10+
      '    class procedure Delete(var S: WideString; Index, Count: Integer); external;'+#13#10+
      '    class procedure Insert(SubStr: WideString; var S: WideString; Index: Integer); external;'+#13#10+
      '    class function Pos(Substr: WideString; S: WideString): Integer; external;'+#13#10+
      '    class function SameStr(const s1, s2: WideString): boolean; external;'+#13#10+
      '    class function SameText(const s1, s2: WideString): boolean; external;'+#13#10+

      '    class function Length(const s: WideString): integer; external;'+#13#10+
      '    class function Trim(const Text: WideString): WideString; external;'+#13#10+
      '    class function TrimLeft(const Text: WideString): WideString; external;'+#13#10+
      '    class function TrimRight(const Text: WideString): WideString; external;'+#13#10+

      '    class function UpperCase(const Text: WideString): WideString; external;'+#13#10+
      '    class function LowerCase(const Text: WideString): WideString; external;'+#13#10+
      '  end;'+#13#10+
      #13#10+
      '  StringEncoding = class(TExternalObject)' + #13#10 + 
      '  public' + #13#10 +
      '    /// Converts a utf-8 encoded string to a system string' + #13#10 + 
      '    class function ToString(const s: UTF8String): string; external; overload;' + #13#10 + 
      '    /// Converts a utf-16 encoded string to a system string' + #13#10 +
      '    class function ToString(const s: WideString): string; external; overload;' + #13#10 + 
      '    /// Converts an ansi encoded string to a system string' + #13#10 + 
      '    class function ToString(const s: AnsiString): string; external; overload;' + #13#10 + 
      #13#10 + 
      '    /// Converts a system string to a utf-8 string' + #13#10 + 
      '    class function ToUTF8(const s: string): UTF8String; external; overload;' + #13#10 + 
      '    /// Converts a utf-16 string to a utf-8 string' + #13#10 + 
      '    class function ToUTF8(const s: WideString): UTF8String; external; overload;' + #13#10 + 
      '    /// Converts an ansi encoded string to a utf-8 string' + #13#10 + 
      '    class function ToUTF8(const s: AnsiString): UTF8String; external; overload;' + #13#10 + 
      #13#10 + 
      '    /// Converts a system string to a utf-16 string' + #13#10 + 
      '    class function ToUnicode(const s: string): WideString; external; overload;' + #13#10 + 
      '    /// Converts a utf-8 string to a utf-16 string' + #13#10 + 
      '    class function ToUnicode(const s: UTF8string): WideString; external; overload;' + #13#10 + 
      '    /// Converts an ansi encoded string to a utf-8 string' + #13#10 + 
      '    class function ToUnicode(const s: AnsiString): WideString; external; overload;' + #13#10 + 
      #13#10 + 
      '    /// Converts a a system string to an ansi string' + #13#10 + 
      '    class function ToAnsi(const s: string): AnsiString; external; overload;' + #13#10 + 
      '    /// Converts a utf-8 string to an ansi string' + #13#10 + 
      '    class function ToAnsi(const s: UTF8String): AnsiString; external; overload;' + #13#10 + 
      '    /// Converts a utf-16 string to an ansi string' + #13#10 + 
      '    class function ToAnsi(const s: WideString): AnsiString; external; overload;' + #13#10 + 
      #13#10 + 
      '    /// Interpretate the input string as a system string' + #13#10 + 
      '    class function AsString(const s: UTF8String): string; external; overload;' + #13#10 + 
      '    /// Interpretate the input string as a system string' + #13#10 + 
      '    class function AsString(const s: WideString): string; external; overload;' + #13#10 + 
      '    /// Interpretate the input string as a system string' + #13#10 + 
      '    class function AsString(const s: AnsiString): string; external; overload;' + #13#10 + 
      #13#10 + 
      '    /// Interpretate the input string as a utf-8 string' + #13#10 + 
      '    class function AsUTF8(const s: string): UTF8String; external; overload;' + #13#10 + 
      '    /// Interpretate the input string as a utf-8 string' + #13#10 + 
      '    class function AsUTF8(const s: WideString): UTF8String; external; overload;' + #13#10 + 
      '    /// Interpretate the input string as a utf-8 string' + #13#10 + 
      '    class function AsUTF8(const s: AnsiString): UTF8String; external; overload;' + #13#10 + 
      #13#10 + 
      '    /// Interpretate the input string as a utf-16 string' + #13#10 + 
      '    class function AsUnicode(const s: string): WideString; external; overload;' + #13#10 + 
      '    /// Interpretate the input string as a utf-16 string' + #13#10 + 
      '    class function AsUnicode(const s: UTF8String): WideString; external; overload;' + #13#10 + 
      '    /// Interpretate the input string as a utf-16 string' + #13#10 + 
      '    class function AsUnicode(const s: AnsiString): WideString; external; overload;' + #13#10 + 
      #13#10 +
      '    /// Interpretate the input string as an ansi string' + #13#10 +
      '    class function AsAnsi(const s: string): AnsiString; external; overload;' + #13#10 + 
      '    /// Interpretate the input string as an ansi string' + #13#10 + 
      '    class function AsAnsi(const s: UTF8String): AnsiString; external; overload;' + #13#10 + 
      '    /// Interpretate the input string as an ansi string' + #13#10 + 
      '    class function AsAnsi(const s: WideString): AnsiString; external; overload;' + #13#10 +
      '  end;' + #13#10 +
      #13#10 +
      'implementation'+#13#10+
      #13#10+
      'end.';

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

type
  Strings = class
  public
    class function FromASCIIIndex(index: word): string;
    class function ToASCIIIndex(const s: string): word;
    class function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
    class function CompareStr(const S1, S2: string): Integer;
    class function CompareText(const S1, S2: string): Integer;
    class function Copy(const s: string; Index, Count: Integer): string;
    {$IFDEF SEII_USE_SOUND_EX}
    class function DecodeSoundExWord(AValue: Word): string;
    {$ENDIF}
    class procedure Delete(var S: string; Index, Count:Integer);
    class function Duplicate(const AText: string; ACount: Integer): string;
    class procedure Insert(Source: string; var S: string; Index: Integer);
    class function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
    class function LastDelimiter(const Delimiters, S: string): Integer;
    class function LeftBStr(const AText: string; const AByteCount: Integer): string;
    class function LeftStr(const AText: string; const ACount: Integer): string;
    class function Length(const s: string): Integer;
    class function LowerCase(const S: string): string;
    class function MidBStr(const AText: string; const AByteStart, AByteCount: Integer): string;
    class function MidStr(const AText: string; const AStart, ACount: Integer): string;
    class function Pos(Substr: string; S: string): Integer;
    class function PosEx0(const SubStr, S: string; Offset: Cardinal): Integer;
    class function PosEx1(const SubStr, S: string): Integer;
    class function QuotedStr(const S: string): string;
    class function ReverseString(const AText: string): string;

    class procedure SetLength0(var s: string; NewLength: integer; ClearContent: boolean);
    class procedure SetLength1(var S: string; NewLength: Integer); 
    class function SameStr(const s1, s2: string): boolean;
    class function SameText(const S1, S2: string): Boolean;
    class function RightStr(const AText: string; const ACount: Integer): string;
    class function RightBStr(const AText: string; const AByteCount: Integer): string;

    {$IFDEF SEII_USE_SOUND_EX}
    class function SoundEx0(const AText: string; ALength: TSoundExLength): string;
    class function SoundEx1(const AText: string): string;
    class function SoundExCompare0(const AText, AOther: string; ALength: TSoundExLength): Integer;    
    class function SoundExCompare1(const AText, AOther: string): Integer;
    class function SoundExProc(const AText, AOther: string): Boolean;
    class function SoundExSimilar0(const AText, AOther: string; ALength: TSoundExLength): Boolean;  
    class function SoundExSimilar1(const AText, AOther: string): Boolean;
    class function SoundExWord(const AText: string): Word;
    {$ENDIF}

    class function StringReplace(const s: string; OldPattern, NewPattern: string; Flags: TReplaceFlags): string;
    class function StuffString(const AText: string; AStart, ALength: Cardinal; const ASubText: string): string;
    class function Trim(const Text: string): string;
    class function TrimLeft(const Text: string): string;
    class function TrimRight(const Text: string): string;
    class function UpperCase(const Text: string): string;
    class function WrapText(const Text: string; MaxCol: Integer): string;
  end;

  WideStrings = class
  public
    class function Copy(const s: WideString; Index, Count: Integer): WideString;
    class procedure Delete(var S: WideString; Index, Count: Integer); 
    class procedure Insert(Source: WideString; var S: WideString; Index: Integer); 
    class function Pos(Substr: WideString; S: WideString): Integer;
    class function SameStr(const s1, s2: WideString): boolean;
    class function SameText(const s1, s2: WideString): boolean;
    class function Length(const s: WideString): integer;

    class function Trim(const Text: WideString): WideString; 
    class function TrimLeft(const Text: WideString): WideString; 
    class function TrimRight(const Text: WideString): WideString; 

    class function UpperCase(const Text: WideString): WideString; 
    class function LowerCase(const Text: WideString): WideString;
  end;


function StringEncoding_ToString(Self: TObject; const s: UTF8String): string;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.UTF8ToString(@p1, @p2);
end;

function StringEncoding_ToString1(Self: TObject; const s: WideString): string;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.WideToString(@p1, @p2);
end;   

function StringEncoding_ToString2(__Self: pointer; const s: AnsiString): String;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.AnsiStringToString(@p1, @p2);
end;

function StringEncoding_ToUTF8(Self: TObject; const s: string): UTF8String;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.StringToUTF8(@p1, @p2);
end;

function StringEncoding_ToUTF81(Self: TObject; const s: WideString): UTF8String;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.WideToUTF8(@p1, @p2);
end;

function StringEncoding_ToUTF82(__Self: pointer; const s: AnsiString): UTF8String;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.AnsiStringToUTF8(@p1, @p2);
end;

function StringEncoding_ToUnicode(Self: TObject; const s: string): WideString;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.StringToWide(@p1, @p2);
end;

function StringEncoding_ToUnicode1(Self: TObject; const s: UTF8String): WideString;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.UTF8ToWide(@p1, @p2);
end;     

function StringEncoding_ToUnicode2(__Self: pointer; const s: AnsiString): WideString;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.AnsiStringToWide(@p1, @p2);
end;

function StringEncoding_ToAnsi(__Self: pointer; const s: String): AnsiString;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.StringToAnsiString(@p1, @p2);
end;

function StringEncoding_ToAnsi1(__Self: pointer; const s: UTF8String): AnsiString;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.UTF8ToAnsiString(@p1, @p2);
end;

function StringEncoding_ToAnsi2(__Self: pointer; const s: WideString): AnsiString;
var p1, p2: Pointer;
begin
  p1 := @s;
  p2 := @result;
  TSE2StringHelper.WideToAnsiString(@p1, @p2);
end;

function StringEncoding_AsString(Self: TObject; const s: string): string;
begin
  result := s;
end;

function StringEncoding_AsString1(Self: TObject; const s: string): string;
begin
  result := s;
end;       

function StringEncoding_AsString2(__Self: pointer; const s: string): string;
begin
  result := s;
end;

function StringEncoding_AsUTF8(Self: TObject; const s: UTF8String): UTF8String;
begin
  result := s;
end;

function StringEncoding_AsUTF81(Self: TObject; const s: UTF8String): UTF8String;
begin
  result := s;
end;      

function StringEncoding_AsUTF82(__Self: pointer; const s: UTF8String): UTF8String;
begin
  result := s;
end;

function StringEncoding_AsUnicode(Self: TObject; const s: WideString): WideString;
begin
  result := s;
end;

function StringEncoding_AsUnicode1(Self: TObject; const s: WideString): WideString;
begin
  result := s;
end;

function StringEncoding_AsUnicode2(__Self: pointer; const s: WideString): WideString;
begin
  result := s;
end;

function StringEncoding_AsAnsi(__Self: pointer; const s: AnsiString): AnsiString;
begin
  result := s;
end;

function StringEncoding_AsAnsi1(__Self: pointer; const s: AnsiString): AnsiString;
begin
  result := s;
end;

function StringEncoding_AsAnsi2(__Self: pointer; const s: AnsiString): AnsiString;
begin
  result := s;
end;

function Strings_DefaultEncoding(__Self: pointer): byte;
begin
  case SizeOf(char) of
  1 : result := 0; // ansi
  2 : result := 2; // utf-16
  4 : result := 3; // utf-32
  else result := 1; // utf-8
  end;
end;

{ Ansi Strings }

function AnsiStrings_ToASCIIIndex(__Self: pointer; const s: AnsiString): Word;
begin                           
  if s <> '' then
     result := Ord(s[1])
  else
     result := 0;
end;

function AnsiStrings_FromASCIIIndex(__Self: pointer; index: Word): AnsiString;
begin
  result := AnsiChar(index);
end;

function AnsiStrings_AdjustLineBreaks(__Self: pointer; const S: AnsiString; Style: TTextLineBreakStyle): AnsiString;
begin
  result := AnsiString(SysUtils.AdjustLineBreaks(string(s), Style))
end;

function AnsiStrings_CompareStr(__Self: pointer; const S1, S2: AnsiString): Integer;
begin
  result := SysUtils.CompareStr(string(s1), string(s2));
end;

function AnsiStrings_CompareText(__Self: pointer; const S1, S2: AnsiString): Integer;
begin
  result := SysUtils.CompareText(string(S1), string(S2));
end;

function AnsiStrings_Copy(__Self: pointer; const s: AnsiString; Index, Count: Integer): AnsiString;
begin
  result := System.Copy(s, Index, Count);
end;

procedure AnsiStrings_Delete(__Self: pointer; var S: AnsiString; Index, Count: Integer);
begin
  System.Delete(S, Index, Count);
end;

function AnsiStrings_Duplicate(__Self: pointer; const AText: AnsiString; ACount: Integer): AnsiString;
begin
  result := AnsiString(StrUtils.DupeString(string(AText), ACount));
end;

procedure AnsiStrings_Insert(__Self: pointer; SubStr: AnsiString; var S: AnsiString; Index: Integer);
begin
  System.Insert(SubStr, S, Index);
end;

function AnsiStrings_IsDelimiter(__Self: pointer; const Delimiters, S: AnsiString; Index: Integer): Boolean;
begin
  result := SysUtils.IsDelimiter(string(Delimiters), string(S), Index);
end;

function AnsiStrings_LastDelimiter(__Self: pointer; const Delimiters, S: AnsiString): Integer;
begin
  result := SysUtils.LastDelimiter(string(Delimiters), string(S));
end;

function AnsiStrings_LeftBStr(__Self: pointer; const AText: AnsiString; const AByteCount: Integer): AnsiString;
begin
  result := StrUtils.LeftBStr(AText, AByteCount);
end;

function AnsiStrings_LeftStr(__Self: pointer; const AText: AnsiString; const ACount: Integer): AnsiString;
begin
  result := StrUtils.LeftStr(AText, ACount);
end;

function AnsiStrings_Length(__Self: pointer; const s: AnsiString): Integer;
begin
  result := System.Length(s);
end;

function AnsiStrings_LowerCase(__Self: pointer; const S: AnsiString): AnsiString;
begin
  result := AnsiString(SysUtils.LowerCase(string(S)));
end;

function AnsiStrings_MidBStr(__Self: pointer; const AText: AnsiString; const AByteStart, AByteCount: Integer): AnsiString;
begin
  result := StrUtils.MidBStr(AText, AByteStart, AByteCount);
end;

function AnsiStrings_MidStr(__Self: pointer; const AText: AnsiString; const AStart, ACount: Integer): AnsiString;
begin
  result := StrUtils.MidStr(AText, AStart, ACount);
end;

function AnsiStrings_Pos(__Self: pointer; Substr, S: AnsiString): Integer;
begin
  result := System.Pos(Substr, S);
end;

{$IFNDEF DELPHI7UP}
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;
{$ENDIF}

function AnsiStrings_PosEx(__Self: pointer; SubStr, S: AnsiString; Offset: Cardinal): Integer;
begin
  {$IFDEF DELPHI7UP}
  result := StrUtils.PosEx(string(SubStr), string(s), Offset);
  {$ELSE}
  result := uSE2IncStrings.PosEx(SubStr, s, Offset);
  {$ENDIF}
end;

function AnsiStrings_PosEx1(__Self: pointer; const SubStr, S: AnsiString): Integer;
begin
  {$IFDEF DELPHI7UP}
  result := System.Pos(SubStr, S);
  {$ELSE}
  result := uSE2IncStrings.PosEx(SubStr, s);
  {$ENDIF}
end;

function AnsiStrings_QuotedStr(__Self: pointer; const S: AnsiString): AnsiString;
begin
  result := AnsiString(SysUtils.QuotedStr(string(S)));
end;

function AnsiStrings_ReverseString(__Self: pointer; const AText: AnsiString): AnsiString;
begin
  result := AnsiString(StrUtils.ReverseString(string(AText)));
end;

procedure AnsiStrings_SetLength(__Self: pointer; var s: AnsiString; NewLength: Integer; ClearContent: Boolean);
begin
  System.SetLength(s, NewLength);
  if ClearContent then
     System.FillChar(s[1], NewLength * SizeOf(AnsiChar), 0);
end;

procedure AnsiStrings_SetLength1(__Self: pointer; var S: AnsiString; NewLength: Integer);
begin
  System.SetLength(s, NewLength);
end;

function AnsiStrings_SameStr(__Self: pointer; const s1, s2: AnsiString): Boolean;
begin
  result := SysUtils.CompareStr(string(s1), string(s2)) = 0;
end;

function AnsiStrings_SameText(__Self: pointer; const S1, S2: AnsiString): Boolean;
begin
  result := SysUtils.CompareText(string(S1), string(S2)) = 0;
end;

function AnsiStrings_RightStr(__Self: pointer; const AText: AnsiString; const ACount: Integer): AnsiString;
begin
  result := StrUtils.RightStr(AText, ACount);
end;

function AnsiStrings_RightBStr(__Self: pointer; const AText: AnsiString; const AByteCount: Integer): AnsiString;
begin
  result := StrUtils.RightBStr(AText, AByteCount);
end;

function AnsiStrings_Replace(__Self: pointer; const s: AnsiString; OldPattern, NewPattern: AnsiString; Flags: TReplaceFlags): AnsiString;
begin
  result := AnsiString(SysUtils.StringReplace(string(s), string(OldPattern), string(NewPattern), Flags));
end;

function AnsiStrings_StuffString(__Self: pointer; const AText: AnsiString; AStart, ALength: Cardinal; const ASubText: AnsiString): AnsiString;
begin
  result := AnsiString(StrUtils.StuffString(string(AText), AStart, ALength, string(ASubText)));
end;

function AnsiStrings_Trim(__Self: pointer; const Text: AnsiString): AnsiString;
begin
  result := AnsiString(SysUtils.Trim(string(Text)));
end;

function AnsiStrings_TrimLeft(__Self: pointer; const Text: AnsiString): AnsiString;
begin
  result := AnsiString(SysUtils.TrimLeft(string(Text)));
end;

function AnsiStrings_TrimRight(__Self: pointer; const Text: AnsiString): AnsiString;
begin
  result := AnsiString(SysUtils.TrimRight(string(Text)));
end;

function AnsiStrings_UpperCase(__Self: pointer; const Text: AnsiString): AnsiString;
begin
  result := Ansistring(SysUtils.UpperCase(string(Text)));
end;

function AnsiStrings_WrapText(__Self: pointer; const Text: AnsiString; MaxCol: Integer): AnsiString;
begin
  result := Ansistring(SysUtils.WrapText(string(Text), MaxCol));
end;

{ Unit Source}

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
    Target.Method['Strings.DefaultEncoding[0]', C_UnitName] := @Strings_DefaultEncoding;

    Target.Method['Strings.ToASCIIIndex[0]', C_UnitName] := @Strings.ToASCIIIndex;
    Target.Method['Strings.FromASCIIIndex[0]', C_UnitName] := @Strings.FromASCIIIndex;
    Target.Method['Strings.AdjustLineBreaks[0]', C_UnitName] := @Strings.AdjustLineBreaks;
    Target.Method['Strings.CompareStr[0]', C_UnitName] := @Strings.CompareStr;
    Target.Method['Strings.CompareText[0]', C_UnitName] := @Strings.CompareText;
    Target.Method['Strings.Copy[0]', C_UnitName] := @Strings.Copy;
    {$IFDEF SEII_USE_SOUND_EX}
    Target.Method['Strings.DecodeSoundExWord[0]', C_UnitName] := @Strings.DecodeSoundExWord;
    {$ENDIF}
    Target.Method['Strings.Delete[0]', C_UnitName] := @Strings.Delete;
    Target.Method['Strings.Duplicate[0]', C_UnitName] := @Strings.Duplicate;
    Target.Method['Strings.Insert[0]', C_UnitName] := @Strings.Insert;
    Target.Method['Strings.IsDelimiter[0]', C_UnitName] := @Strings.IsDelimiter;
    Target.Method['Strings.LastDelimiter[0]', C_UnitName] := @Strings.LastDelimiter;
    Target.Method['Strings.LeftBStr[0]', C_UnitName] := @Strings.LeftBStr;
    Target.Method['Strings.LeftStr[0]', C_UnitName] := @Strings.LeftStr;
    Target.Method['Strings.Length[0]', C_UnitName] := @Strings.Length;
    Target.Method['Strings.LowerCase[0]', C_UnitName] := @Strings.LowerCase;
    Target.Method['Strings.MidBStr[0]', C_UnitName] := @Strings.MidBStr;
    Target.Method['Strings.MidStr[0]', C_UnitName] := @Strings.MidStr;
    Target.Method['Strings.Pos[0]', C_UnitName] := @Strings.Pos;
    Target.Method['Strings.PosEx[0]', C_UnitName] := @Strings.PosEx0;
    Target.Method['Strings.PosEx[1]', C_UnitName] := @Strings.PosEx1;
    Target.Method['Strings.QuotedStr[0]', C_UnitName] := @Strings.QuotedStr;
    Target.Method['Strings.ReverseString[0]', C_UnitName] := @Strings.ReverseString;

    Target.Method['Strings.SetLength[0]', C_UnitName] := @Strings.SetLength0;
    Target.Method['Strings.SetLength[1]', C_UnitName] := @Strings.SetLength1;
    Target.Method['Strings.SameStr[0]', C_UnitName] := @Strings.SameStr;
    Target.Method['Strings.SameText[0]', C_UnitName] := @Strings.SameText;
    Target.Method['Strings.RightStr[0]', C_UnitName] := @Strings.RightStr;
    Target.Method['Strings.RightBStr[0]', C_UnitName] := @Strings.RightBStr;

    {$IFDEF SEII_USE_SOUND_EX}
    Target.Method['Strings.SoundEx[0]', C_UnitName] := @Strings.SoundEx0;
    Target.Method['Strings.SoundEx[1]', C_UnitName] := @Strings.SoundEx1;
    Target.Method['Strings.SoundExCompare[0]', C_UnitName] := @Strings.SoundExCompare0;
    Target.Method['Strings.SoundExCompare[1]', C_UnitName] := @Strings.SoundExCompare1;
    Target.Method['Strings.SoundExProc[0]', C_UnitName] := @Strings.SoundExProc;
    Target.Method['Strings.SoundExSimilar[0]', C_UnitName] := @Strings.SoundExSimilar0;
    Target.Method['Strings.SoundExSimilar[1]', C_UnitName] := @Strings.SoundExSimilar1;
    Target.Method['Strings.SoundExWord[0]', C_UnitName] := @Strings.SoundExWord;
    {$ENDIF}

    Target.Method['Strings.Replace[0]', C_UnitName] := @Strings.StringReplace;
    Target.Method['Strings.StuffString[0]', C_UnitName] := @Strings.StuffString;
    Target.Method['Strings.Trim[0]', C_UnitName] := @Strings.Trim;
    Target.Method['Strings.TrimLeft[0]', C_UnitName] := @Strings.TrimLeft;
    Target.Method['Strings.TrimRight[0]', C_UnitName] := @Strings.TrimRight;
    Target.Method['Strings.UpperCase[0]', C_UnitName] := @Strings.UpperCase;
    Target.Method['Strings.WrapText[0]', C_UnitName] := @Strings.WrapText;



    Target.Method['WideStrings.Copy[0]', C_UnitName] := @WideStrings.Copy;
    Target.Method['WideStrings.Delete[0]', C_UnitName] := @WideStrings.Delete;
    Target.Method['WideStrings.Insert[0]', C_UnitName] := @WideStrings.Insert;
    Target.Method['WideStrings.Pos[0]', C_UnitName] := @WideStrings.Pos;
    Target.Method['WideStrings.SameStr[0]', C_UnitName] := @WideStrings.SameStr;
    Target.Method['WideStrings.SameText[0]', C_UnitName] := @WideStrings.SameText;
    Target.Method['WideStrings.Length[0]', C_UnitName] := @WideStrings.Length;

    Target.Method['WideStrings.Trim[0]', C_UnitName] := @WideStrings.Trim;
    Target.Method['WideStrings.TrimLeft[0]', C_UnitName] := @WideStrings.TrimLeft;
    Target.Method['WideStrings.TrimRight[0]', C_UnitName] := @WideStrings.TrimRight;

    Target.Method['WideStrings.UpperCase[0]', C_UnitName] := @WideStrings.UpperCase;
    Target.Method['WideStrings.LowerCase[0]', C_UnitName] := @WideStrings.LowerCase;

    Target.Method['AnsiStrings.ToASCIIIndex[0]', C_UnitName] := @AnsiStrings_ToASCIIIndex;
    Target.Method['AnsiStrings.FromASCIIIndex[0]', C_UnitName] := @AnsiStrings_FromASCIIIndex;
    Target.Method['AnsiStrings.AdjustLineBreaks[0]', C_UnitName] := @AnsiStrings_AdjustLineBreaks;
    Target.Method['AnsiStrings.CompareStr[0]', C_UnitName] := @AnsiStrings_CompareStr;
    Target.Method['AnsiStrings.CompareText[0]', C_UnitName] := @AnsiStrings_CompareText;
    Target.Method['AnsiStrings.Copy[0]', C_UnitName] := @AnsiStrings_Copy;
    Target.Method['AnsiStrings.Delete[0]', C_UnitName] := @AnsiStrings_Delete;
    Target.Method['AnsiStrings.Duplicate[0]', C_UnitName] := @AnsiStrings_Duplicate;
    Target.Method['AnsiStrings.Insert[0]', C_UnitName] := @AnsiStrings_Insert;
    Target.Method['AnsiStrings.IsDelimiter[0]', C_UnitName] := @AnsiStrings_IsDelimiter;
    Target.Method['AnsiStrings.LastDelimiter[0]', C_UnitName] := @AnsiStrings_LastDelimiter;
    Target.Method['AnsiStrings.LeftBStr[0]', C_UnitName] := @AnsiStrings_LeftBStr;
    Target.Method['AnsiStrings.LeftStr[0]', C_UnitName] := @AnsiStrings_LeftStr;
    Target.Method['AnsiStrings.Length[0]', C_UnitName] := @AnsiStrings_Length;
    Target.Method['AnsiStrings.LowerCase[0]', C_UnitName] := @AnsiStrings_LowerCase;
    Target.Method['AnsiStrings.MidBStr[0]', C_UnitName] := @AnsiStrings_MidBStr;
    Target.Method['AnsiStrings.MidStr[0]', C_UnitName] := @AnsiStrings_MidStr;
    Target.Method['AnsiStrings.Pos[0]', C_UnitName] := @AnsiStrings_Pos;
    Target.Method['AnsiStrings.PosEx[0]', C_UnitName] := @AnsiStrings_PosEx;
    Target.Method['AnsiStrings.PosEx[1]', C_UnitName] := @AnsiStrings_PosEx1;
    Target.Method['AnsiStrings.QuotedStr[0]', C_UnitName] := @AnsiStrings_QuotedStr;
    Target.Method['AnsiStrings.ReverseString[0]', C_UnitName] := @AnsiStrings_ReverseString;
    Target.Method['AnsiStrings.SetLength[0]', C_UnitName] := @AnsiStrings_SetLength;
    Target.Method['AnsiStrings.SetLength[1]', C_UnitName] := @AnsiStrings_SetLength1;
    Target.Method['AnsiStrings.SameStr[0]', C_UnitName] := @AnsiStrings_SameStr;
    Target.Method['AnsiStrings.SameText[0]', C_UnitName] := @AnsiStrings_SameText;
    Target.Method['AnsiStrings.RightStr[0]', C_UnitName] := @AnsiStrings_RightStr;
    Target.Method['AnsiStrings.RightBStr[0]', C_UnitName] := @AnsiStrings_RightBStr;
    Target.Method['AnsiStrings.Replace[0]', C_UnitName] := @AnsiStrings_Replace;
    Target.Method['AnsiStrings.StuffString[0]', C_UnitName] := @AnsiStrings_StuffString;
    Target.Method['AnsiStrings.Trim[0]', C_UnitName] := @AnsiStrings_Trim;
    Target.Method['AnsiStrings.TrimLeft[0]', C_UnitName] := @AnsiStrings_TrimLeft;
    Target.Method['AnsiStrings.TrimRight[0]', C_UnitName] := @AnsiStrings_TrimRight;
    Target.Method['AnsiStrings.UpperCase[0]', C_UnitName] := @AnsiStrings_UpperCase;
    Target.Method['AnsiStrings.WrapText[0]', C_UnitName] := @AnsiStrings_WrapText;
    
    Target.Method['StringEncoding.ToString[0]', C_UnitName] := @StringEncoding_ToString;
    Target.Method['StringEncoding.ToString[1]', C_UnitName] := @StringEncoding_ToString1;
    Target.Method['StringEncoding.ToString[2]', C_UnitName] := @StringEncoding_ToString2;
    Target.Method['StringEncoding.ToUTF8[0]', C_UnitName] := @StringEncoding_ToUTF8;
    Target.Method['StringEncoding.ToUTF8[1]', C_UnitName] := @StringEncoding_ToUTF81;
    Target.Method['StringEncoding.ToUTF8[2]', C_UnitName] := @StringEncoding_ToUTF82;
    Target.Method['StringEncoding.ToUnicode[0]', C_UnitName] := @StringEncoding_ToUnicode;
    Target.Method['StringEncoding.ToUnicode[1]', C_UnitName] := @StringEncoding_ToUnicode1;
    Target.Method['StringEncoding.ToUnicode[2]', C_UnitName] := @StringEncoding_ToUnicode2;
    Target.Method['StringEncoding.ToAnsi[0]', C_UnitName] := @StringEncoding_ToAnsi;
    Target.Method['StringEncoding.ToAnsi[1]', C_UnitName] := @StringEncoding_ToAnsi1;
    Target.Method['StringEncoding.ToAnsi[2]', C_UnitName] := @StringEncoding_ToAnsi2;
    Target.Method['StringEncoding.AsString[0]', C_UnitName] := @StringEncoding_AsString;
    Target.Method['StringEncoding.AsString[1]', C_UnitName] := @StringEncoding_AsString1;
    Target.Method['StringEncoding.AsString[2]', C_UnitName] := @StringEncoding_AsString2;
    Target.Method['StringEncoding.AsUTF8[0]', C_UnitName] := @StringEncoding_AsUTF8;
    Target.Method['StringEncoding.AsUTF8[1]', C_UnitName] := @StringEncoding_AsUTF81;
    Target.Method['StringEncoding.AsUTF8[2]', C_UnitName] := @StringEncoding_AsUTF82;
    Target.Method['StringEncoding.AsUnicode[0]', C_UnitName] := @StringEncoding_AsUnicode;
    Target.Method['StringEncoding.AsUnicode[1]', C_UnitName] := @StringEncoding_AsUnicode1;
    Target.Method['StringEncoding.AsUnicode[2]', C_UnitName] := @StringEncoding_AsUnicode2;
    Target.Method['StringEncoding.AsAnsi[0]', C_UnitName] := @StringEncoding_AsAnsi;
    Target.Method['StringEncoding.AsAnsi[1]', C_UnitName] := @StringEncoding_AsAnsi1;
    Target.Method['StringEncoding.AsAnsi[2]', C_UnitName] := @StringEncoding_AsAnsi2;
  end;
end;

procedure RegisterUnit;
var p : TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.Priority          := 3;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

{ Strings }

class function Strings.AdjustLineBreaks(const S: string;
  Style: TTextLineBreakStyle): string;
begin
  result := SysUtils.AdjustLineBreaks(s, Style)
end;

class function Strings.CompareStr(const S1, S2: string): Integer;
begin
  result := SysUtils.CompareStr(s1, s2)
end;

class function Strings.CompareText(const S1, S2: string): Integer;
begin
  result := SysUtils.CompareText(s1, s2)
end;

class function Strings.Copy(const s: string; Index,
  Count: Integer): string;
begin
  result := System.Copy(s, Index, Count);
end;

{$IFDEF SEII_USE_SOUND_EX}
class function Strings.DecodeSoundExWord(AValue: Word): string;
begin
  result := StrUtils.DecodeSoundexWord(AValue)
end;
{$ENDIF}

class procedure Strings.Delete(var S: string; Index, Count: Integer);
begin
  System.Delete(s, Index, Count);
end;

class function Strings.Duplicate(const AText: string;
  ACount: Integer): string;
begin
  result := StrUtils.DupeString(AText, ACount)
end;

class function Strings.FromASCIIIndex(index: word): string;
begin
  result := Chr(index);
end;

class procedure Strings.Insert(Source: string; var S: string;
  Index: Integer);
begin
  System.Insert(Source, s, Index);
end;

class function Strings.IsDelimiter(const Delimiters, S: string;
  Index: Integer): Boolean;
begin
  result := SysUtils.IsDelimiter(Delimiters, s, Index)
end;

class function Strings.LastDelimiter(const Delimiters, S: string): Integer;
begin
  result := SysUtils.LastDelimiter(Delimiters, s);
end;

class function Strings.LeftBStr(const AText: string;
  const AByteCount: Integer): string;
begin
  result := System.Copy(AText, 1, AByteCount);
end;

class function Strings.LeftStr(const AText: string;
  const ACount: Integer): string;
begin
  result := StrUtils.LeftStr(AText, ACount)
end;

class function Strings.Length(const s: string): Integer;
begin
  result := System.Length(s);
end;

class function Strings.LowerCase(const S: string): string;
begin
  result := SysUtils.LowerCase(s);
end;

class function Strings.MidBStr(const AText: string; const AByteStart,
  AByteCount: Integer): string;
begin
  result := System.Copy(AText, AByteStart, AByteCount);
end;

class function Strings.MidStr(const AText: string; const AStart,
  ACount: Integer): string;
begin
  result := System.Copy(AText, AStart, ACount)
end;

class function Strings.Pos(Substr, S: string): Integer;
begin
  result := System.Pos(SubStr, s);
end;

class function Strings.PosEx0(const SubStr, S: string;
  Offset: Cardinal): Integer;
begin
  {$IFDEF DELPHI7UP}
  result := StrUtils.PosEx(SubStr, s, Offset);
  {$ELSE}
  result := uSE2IncStrings.PosEx(SubStr, s, Offset);
  {$ENDIF}
end;

class function Strings.PosEx1(const SubStr, S: string): Integer;
begin
  {$IFDEF DELPHI7UP}
  result := StrUtils.PosEx(SubStr, s)
  {$ELSE}
  result := uSE2IncStrings.PosEx(SubStr, s);
  {$ENDIF}
end;

class function Strings.QuotedStr(const S: string): string;
begin
  result := SysUtils.QuotedStr(s);
end;

class function Strings.ReverseString(const AText: string): string;
begin
  result := StrUtils.ReverseString(AText);
end;

class function Strings.RightBStr(const AText: string;
  const AByteCount: Integer): string;
begin
  Result := System.Copy(AText, System.Length(AText) + 1 - AByteCount, AByteCount);
end;

class function Strings.RightStr(const AText: string;
  const ACount: Integer): string;               
begin
  result := StrUtils.RightStr(AText, ACount)
end;

class function Strings.SameStr(const s1, s2: string): boolean;
begin
  result := SysUtils.CompareStr(s1, s2) = 0;
end;

class function Strings.SameText(const S1, S2: string): Boolean;
begin
  result := SysUtils.SameText(S1, s2);
end;

class procedure Strings.SetLength0(var s: string; NewLength: integer; ClearContent: boolean);
begin
  System.SetLength(s, NewLength);
  if ClearContent then
     System.FillChar(s[1], NewLength * SizeOf(char), 0);
end;

class procedure Strings.SetLength1(var S: string; NewLength: Integer);
begin
  System.SetLength(s, NewLength);
end;

{$IFDEF SEII_USE_SOUND_EX}
class function Strings.SoundEx0(const AText: string;
  ALength: TSoundExLength): string;
begin
  result := StrUtils.Soundex(AText, ALength)
end;
{$ENDIF}
                          
{$IFDEF SEII_USE_SOUND_EX}
class function Strings.SoundEx1(const AText: string): string;
begin
  result := StrUtils.Soundex(AText)
end;        
{$ENDIF}
                               
{$IFDEF SEII_USE_SOUND_EX}
class function Strings.SoundExCompare0(const AText, AOther: string;
  ALength: TSoundExLength): Integer;
begin
  result := StrUtils.SoundexCompare(AText, AOther, ALength)
end;     
{$ENDIF}
                                   
{$IFDEF SEII_USE_SOUND_EX}
class function Strings.SoundExCompare1(const AText, AOther: string): Integer;
begin
  result := StrUtils.SoundexCompare(AText, AOther)
end;           
{$ENDIF}
                           
{$IFDEF SEII_USE_SOUND_EX}
class function Strings.SoundExProc(const AText, AOther: string): Boolean;
begin
  result := StrUtils.SoundexProc(AText, AOther)
end;       
{$ENDIF}
                               
{$IFDEF SEII_USE_SOUND_EX}
class function Strings.SoundExSimilar0(const AText, AOther: string;
  ALength: TSoundExLength): Boolean;
begin
  result := StrUtils.SoundexSimilar(AText, AOther, ALength)
end;       
{$ENDIF}
                           
{$IFDEF SEII_USE_SOUND_EX}
class function Strings.SoundExSimilar1(const AText, AOther: string): Boolean;
begin
  result := StrUtils.SoundexSimilar(AText, AOther)
end;     
{$ENDIF}
                          
{$IFDEF SEII_USE_SOUND_EX}
class function Strings.SoundExWord(const AText: string): Word;
begin
  result := StrUtils.SoundexWord(AText)
end;          
{$ENDIF}

class function Strings.StringReplace(const s: string; OldPattern,
  NewPattern: string; Flags: TReplaceFlags): string;
begin
  result := SysUtils.StringReplace(s, OldPattern, NewPattern, Flags)
end;

class function Strings.StuffString(const AText: string; AStart,
  ALength: Cardinal; const ASubText: string): string;
begin
  result := StrUtils.StuffString(AText, AStart, ALength, ASubText)
end;

class function Strings.ToASCIIIndex(const s: string): word;
begin
  if s <> '' then
     result := Ord(s[1])
  else
     result := 0;
end;

class function Strings.Trim(const Text: string): string;
begin
  result := SysUtils.Trim(Text);
end;

class function Strings.TrimLeft(const Text: string): string;
begin
  result := SysUtils.TrimLeft(Text);
end;

class function Strings.TrimRight(const Text: string): string;
begin
  result := SysUtils.TrimRight(Text);
end;

class function Strings.UpperCase(const Text: string): string;
begin
  result := SysUtils.UpperCase(Text);
end;

class function Strings.WrapText(const Text: string;
  MaxCol: Integer): string;
begin
  result := SysUtils.WrapText(Text, MaxCol);
end;

{ WideStrings }

class function WideStrings.Copy(const s: WideString; Index,
  Count: Integer): WideString;
begin
  result := System.Copy(s, Index, Count);
end;

class procedure WideStrings.Delete(var S: WideString; Index,
  Count: Integer);
begin
  System.Delete(s, Index, Count);
end;

class procedure WideStrings.Insert(Source: WideString; var S: WideString;
  Index: Integer);
begin
  System.Insert(Source, s, Index);
end;

class function WideStrings.Length(const s: WideString): integer;
begin
  result := System.Length(s);
end;

class function WideStrings.LowerCase(const Text: WideString): WideString;
begin
  result := SysUtils.WideLowerCase(Text);
end;

class function WideStrings.Pos(Substr, S: WideString): Integer;
begin
  result := System.Pos(Substr, s);
end;

class function WideStrings.SameStr(const s1, s2: WideString): boolean;
begin
  result := SysUtils.WideSameStr(s1, s2);
end;

class function WideStrings.SameText(const s1, s2: WideString): boolean;
begin
  result := SysUtils.WideSameText(s1, s2);
end;

class function WideStrings.Trim(const Text: WideString): WideString;
begin
  result := SysUtils.Trim(Text);
end;

class function WideStrings.TrimLeft(const Text: WideString): WideString;
begin
  result := SysUtils.TrimLeft(Text);
end;

class function WideStrings.TrimRight(const Text: WideString): WideString;
begin
  result := SysUtils.TrimRight(Text);
end;

class function WideStrings.UpperCase(const Text: WideString): WideString;
begin
  result := SysUtils.WideUpperCase(Text);
end;

initialization
  RegisterUnit;

end.
