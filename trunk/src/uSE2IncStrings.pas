unit uSE2IncStrings;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, StrUtils, uSE2Types, uSE2BaseTypes, uSE2RunAccess, uSE2UnitManager;

implementation

uses
  uSE2RunType;

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
      '  Strings = class(TExternalObject)'+#13#10+
      '  public'+#13#10+
      '    class function ToASCIIIndex(const s: string): word; external;'+#13#10+
      '    class function FromASCIIIndex(index: word): string; external;'+#13#10+
      '    class function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string; external;'+#13#10+
      '    class function CompareStr(const S1, S2: string): Integer; external;'+#13#10+
      '    class function CompareText(const S1, S2: string): Integer; external;'+#13#10+
      '    class function Copy(const s: string; Index, Count: Integer): string; external;'+#13#10+
      {$IFDEF SEII_USE_SOUND_EX}
      '    class function DecodeSoundExWord(AValue: Word): string; external;'+#13#10+
      {$ENDIF}
      '    class procedure Delete(var S: string; Index, Count:Integer); external;'+#13#10+
      '    class function Duplicate(const AText: string; ACount: Integer): string; external;'+#13#10+
      '    class procedure Insert(SubStr: string; var S: string; Index: Integer); external;'+#13#10+
      '    class function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean; external;'+#13#10+
      '    class function LastDelimiter(const Delimiters, S: string): Integer; external;'+#13#10+
      '    class function LeftBStr(const AText: string; const AByteCount: Integer): string; external;'+#13#10+
      '    class function LeftStr(const AText: string; const ACount: Integer): string; external;'+#13#10+
      '    class function Length(const s: string): Integer; external;'+#13#10+
      '    class function LowerCase(const S: string): string; external;'+#13#10+
      '    class function MidBStr(const AText: string; const AByteStart, AByteCount: Integer): string; external;'+#13#10+
      '    class function MidStr(const AText: string; const AStart, ACount: Integer): string; external;'+#13#10+
      '    class function Pos(Substr: string; S: string): Integer; external;'+#13#10+
      '    class function PosEx(const SubStr, S: string; Offset: Cardinal): Integer; overload; external;'+#13#10+
      '    class function PosEx(const SubStr, S: string): Integer; overload; external;'+#13#10+
      '    class function QuotedStr(const S: string): string; external;'+#13#10+
      '    class function ReverseString(const AText: string): string; external;'+#13#10+

      '    class procedure SetLength(var S: string; NewLength: Integer); external;'+#13#10+

      '    class function SameStr(const s1, s2: string): boolean; external;'+#13#10+
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

      '    class function Replace(const s: string; OldPattern, NewPattern: string; Flags: TReplaceFlags): string; external;'+#13#10+
      '    class function StuffString(const AText: string; AStart, ALength: Cardinal; const ASubText: string): string; external;'+#13#10+
      '    class function Trim(const Text: string): string; external;'+#13#10+
      '    class function TrimLeft(const Text: string): string; external;'+#13#10+
      '    class function TrimRight(const Text: string): string; external;'+#13#10+
      '    class function UpperCase(const Text: string): string; external;'+#13#10+
      '    class function WrapText(const Text: string; MaxCol: Integer): string; external;'+#13#10+
      '  end;'+#13#10+
      #13#10+
      '  WideStrings = class(TExternalObject)'+#13#10+
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
      '    class function ToString(const s: UTF8String): string; external; overload;' + #13#10 +
      '    class function ToString(const s: WideString): string; external; overload;' + #13#10 +
      '    class function ToUTF8(const s: string): UTF8String; external; overload;' + #13#10 +
      '    class function ToUTF8(const s: WideString): UTF8String; external; overload;' + #13#10 +
      '    class function ToUnicode(const s: string): WideString; external; overload;' + #13#10 +
      '    class function ToUnicode(const s: UTF8string): WideString; external; overload;' + #13#10 +
      #13#10 +
      '    class function AsString(const s: UTF8String): string; external; overload;' + #13#10 +
      '    class function AsString(const s: WideString): string; external; overload;' + #13#10 +
      '    class function AsUTF8(const s: string): UTF8String; external; overload;' + #13#10 +
      '    class function AsUTF8(const s: WideString): UTF8String; external; overload;' + #13#10 +
      '    class function AsUnicode(const s: string): WideString; external; overload;' + #13#10 +
      '    class function AsUnicode(const s: UTF8String): WideString; external; overload;' + #13#10 +
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

    class procedure SetLength(var S: string; NewLength: Integer);      
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

function StringEncoding_AsString(Self: TObject; const s: Pointer): Pointer;
begin
  result := s;
end;

function StringEncoding_AsString1(Self: TObject; const s: Pointer): Pointer;
begin
  result := s;
end;

function StringEncoding_AsUTF8(Self: TObject; const s: Pointer): Pointer;
begin
  result := s;
end;

function StringEncoding_AsUTF81(Self: TObject; const s: Pointer): Pointer;
begin
  result := s;
end;

function StringEncoding_AsUnicode(Self: TObject; const s: Pointer): Pointer;
begin
  result := s;
end;

function StringEncoding_AsUnicode1(Self: TObject; const s: Pointer): Pointer;
begin
  result := s;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
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

    Target.Method['Strings.SetLength[0]', C_UnitName] := @Strings.SetLength; 
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

    
    Target.Method['StringEncoding.ToString[0]', C_UnitName] := @StringEncoding_ToString;
    Target.Method['StringEncoding.ToString[1]', C_UnitName] := @StringEncoding_ToString1;
    Target.Method['StringEncoding.ToUTF8[0]', C_UnitName] := @StringEncoding_ToUTF8;
    Target.Method['StringEncoding.ToUTF8[1]', C_UnitName] := @StringEncoding_ToUTF81;
    Target.Method['StringEncoding.ToUnicode[0]', C_UnitName] := @StringEncoding_ToUnicode;
    Target.Method['StringEncoding.ToUnicode[1]', C_UnitName] := @StringEncoding_ToUnicode1;
    Target.Method['StringEncoding.AsString[0]', C_UnitName] := @StringEncoding_AsString;
    Target.Method['StringEncoding.AsString[1]', C_UnitName] := @StringEncoding_AsString1;
    Target.Method['StringEncoding.AsUTF8[0]', C_UnitName] := @StringEncoding_AsUTF8;
    Target.Method['StringEncoding.AsUTF8[1]', C_UnitName] := @StringEncoding_AsUTF81;
    Target.Method['StringEncoding.AsUnicode[0]', C_UnitName] := @StringEncoding_AsUnicode;
    Target.Method['StringEncoding.AsUnicode[1]', C_UnitName] := @StringEncoding_AsUnicode1;
  end;
end;

procedure RegisterUnit;
var p : TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.Priority          := 10;
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

class procedure Strings.SetLength(var S: string; NewLength: Integer);
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
