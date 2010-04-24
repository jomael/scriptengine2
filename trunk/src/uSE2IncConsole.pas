unit uSE2IncConsole;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, Graphics, uSE2BaseTypes, uSE2RunAccess, uSE2UnitManager;

type
  TSE2ConsoleMethodType = record
    Clear      : procedure;
    ClearLn    : procedure;
    CursorTo   : procedure(x, y: integer);
    ScrollTo   : procedure(x, y: integer);
    Write      : procedure(s: string);
    Read       : function: string;
    ReadKey    : function: string;
    KeyPressed : function: boolean;
    GetLine    : function(line: integer): string;
    SetLine    : procedure(line: integer; s: string);

    GetBgColor : function: TColor;
    SetBgColor : procedure(value: TColor);
    GetFgColor : function: TColor;
    SetFgColor : procedure(value: TColor);
  end;

var
  TSE2Console : TSE2ConsoleMethodType;

implementation

const
  C_UnitName   = 'System';
  C_UnitSource =
     'unit System;'+#13#10+
     #13#10+
     'interface'+#13#10+
     #13#10+
     'type'+#13#10+
     '  TColor = cardinal;'+#13#10+
     #13#10+
     'type'+#13#10+
     '  Console = partial class(TExternalObject)'+#13#10+
     '  protected'+#13#10+
     '    class function  GetForegroundColor: TColor; external;'+#13#10+
     '    class function  GetBackgroundColor: TColor; external;'+#13#10+

     '    class procedure SetForegroundColor(value: TColor); external;'+#13#10+
     '    class procedure SetBackgroundColor(value: TColor); external;'+#13#10+

     '    class function  GetLine(line: integer): string; external;'+#13#10+
     '    class procedure SetLine(line: integer; s: string); external;'+#13#10+
     '  public'+#13#10+
     '    class procedure Clear; external;'+#13#10+
     '    class procedure ClearLine; external;'+#13#10+

     '    class property  ForegroundColor : TColor  read GetForegroundColor  write SetForegroundColor;'+#13#10+ 
     '    class property  BackgroundColor : TColor  read GetBackgroundColor  write SetBackgroundColor;'+#13#10+
     '    class property  Lines[index: integer]: string read GetLine write SetLine;'+#13#10+

     '    class procedure CursorTo(x, y: integer); external;'+#13#10+
     '    class procedure ScrollTo(x, y: integer); external;'+#13#10+

     '    class procedure WriteLine(i: byte); external; overload;'+#13#10+
     '    class procedure WriteLine(i: shortint); external; overload;'+#13#10+
     '    class procedure WriteLine(i: word); external; overload;'+#13#10+
     '    class procedure WriteLine(i: smallint); external; overload;'+#13#10+
     '    class procedure WriteLine(i: cardinal); external; overload;'+#13#10+
     '    class procedure WriteLine(i: integer); external; overload;'+#13#10+
     '    class procedure WriteLine(i: int64); external; overload;'+#13#10+
     '    class procedure WriteLine(i: single); external; overload;'+#13#10+
     '    class procedure WriteLine(i: double); external; overload;'+#13#10+
     '    class procedure WriteLine(b: boolean); external; overload;'+#13#10+
     '    class procedure WriteLine(p: pointer); external; overload;'+#13#10+
     '    class procedure WriteLine(const s: string); external; overload;'+#13#10+
     '    class procedure WriteLine(const s: UTF8String); external; overload;'+#13#10+
     '    class procedure WriteLine(const s: WideString); external; overload;'+#13#10+
     '    class procedure WriteLine(const s: PChar); external; overload;'+#13#10+
     '    class procedure WriteLine; external; overload;'+#13#10+

     '    class procedure Write(i: byte); external; overload;'+#13#10+
     '    class procedure Write(i: shortint); external; overload;'+#13#10+
     '    class procedure Write(i: word); external; overload;'+#13#10+
     '    class procedure Write(i: smallint); external; overload;'+#13#10+
     '    class procedure Write(i: cardinal); external; overload;'+#13#10+
     '    class procedure Write(i: integer); external; overload;'+#13#10+
     '    class procedure Write(i: int64); external; overload;'+#13#10+
     '    class procedure Write(i: single); external; overload;'+#13#10+
     '    class procedure Write(i: double); external; overload;'+#13#10+
     '    class procedure Write(b: boolean); external; overload;'+#13#10+  
     '    class procedure Write(p: pointer); external; overload;'+#13#10+
     '    class procedure Write(const s: string); external; overload;'+#13#10+
     '    class procedure Write(const s: UTF8String); external; overload;'+#13#10+
     '    class procedure Write(const s: WideString); external; overload;'+#13#10+
     '    class procedure Write(const s: PChar); external; overload;'+#13#10+

     '    class function  KeyPressed: boolean; external;'+#13#10+
     '    class function  ReadLine: string; external;'+#13#10+
     '    class function  ReadInt: int64; external;'+#13#10+
     '    class function  ReadFloat: double; external;'+#13#10+
     '    class function  ReadKey: string; external;'+#13#10+
     'end;'+#13#10+
     #13#10+
     {'const'+#13#10+
     '  clBlack      : TColor = $000000;'+#13#10+
     '  clMaroon     : TColor = $000080;'+#13#10+
     '  clGreen      : TColor = $008000;'+#13#10+
     '  clOlive      : TColor = $008080;'+#13#10+
     '  clNavy       : TColor = $800000;'+#13#10+
     '  clPurple     : TColor = $800080;'+#13#10+
     '  clTeal       : TColor = $808000;'+#13#10+
     '  clGray       : TColor = $808080;'+#13#10+
     '  clSilver     : TColor = $C0C0C0;'+#13#10+
     '  clRed        : TColor = $0000FF;'+#13#10+
     '  clLime       : TColor = $00FF00;'+#13#10+
     '  clYellow     : TColor = $00FFFF;'+#13#10+
     '  clBlue       : TColor = $FF0000;'+#13#10+
     '  clFuchsia    : TColor = $FF00FF;'+#13#10+
     '  clAqua       : TColor = $FFFF00;'+#13#10+
     '  clLtGray     : TColor = $C0C0C0;'+#13#10+
     '  clDkGray     : TColor = $808080;'+#13#10+
     '  clWhite      : TColor = $FFFFFF;'+#13#10+
     '  clMoneyGreen : TColor = $C0DCC0;'+#13#10+
     '  clSkyBlue    : TColor = $F0CAA6;'+#13#10+
     '  clCream      : TColor = $F0FBFF;'+#13#10+
     '  clMedGray    : TColor = $A4A0A0;'+#13#10+ }
     #13#10+
     'type'+#13#10+
     '  Colors = class(TExternalObject)'+#13#10+
     '  public'+#13#10+
     '    const Black      : TColor = $000000;' + #13#10 +
     '    const Maroon     : TColor = $000080;' + #13#10 +
     '    const Green      : TColor = $008000;' + #13#10 +
     '    const Olive      : TColor = $008080;' + #13#10 +
     '    const Navy       : TColor = $800000;' + #13#10 +
     '    const Purple     : TColor = $800080;' + #13#10 +
     '    const Teal       : TColor = $808000;' + #13#10 +
     '    const Gray       : TColor = $808080;' + #13#10 +
     '    const Silver     : TColor = $C0C0C0;' + #13#10 +
     '    const Red        : TColor = $0000FF;' + #13#10 +
     '    const Lime       : TColor = $00FF00;' + #13#10 +
     '    const Yellow     : TColor = $00FFFF;' + #13#10 +
     '    const Blue       : TColor = $FF0000;' + #13#10 +
     '    const Fuchsia    : TColor = $FF00FF;' + #13#10 +
     '    const Aqua       : TColor = $FFFF00;' + #13#10 +
     '    const LightGray  : TColor = $C0C0C0;' + #13#10 +
     '    const DarkGray   : TColor = $808080;' + #13#10 +
     '    const White      : TColor = $FFFFFF;' + #13#10 +
     '    const MoneyGreen : TColor = $C0DCC0;' + #13#10 +
     '    const SkyBlue    : TColor = $F0CAA6;' + #13#10 +
     '    const Cream      : TColor = $F0FBFF;' + #13#10 +
     '    const MediumGray : TColor = $A4A0A0;' + #13#10 +
     #13#10 +
     '    class function RGB(red, green, blue: byte): TColor; external;'+#13#10+
     '    class function RedValue(aColor: TColor): byte; external;'+#13#10+
     '    class function GreenValue(aColor: TColor): byte; external;'+#13#10+
     '    class function BlueValue(aColor: TColor): byte; external;'+#13#10+
     '  end;'+#13#10+
     #13#10+
     'implementation'+#13#10+
     #13#10+
     'end.';

type
  TConsole = class
  public
    class procedure Clear;
    class procedure ClearLine;

    class procedure CursorTo(x, y: integer);
    class procedure ScrollTo(x, y: integer); 

    class procedure WriteLineU8(i: byte);
    class procedure WriteLineS8(i: shortint);
    class procedure WriteLineU16(i: word);
    class procedure WriteLineS16(i: smallint);
    class procedure WriteLineU32(i: cardinal);
    class procedure WriteLineS32(i: integer);
    class procedure WriteLineS64(i: int64);
    class procedure WriteLineF(i: single);
    class procedure WriteLineD(i: double);
    class procedure WriteLineB(b: boolean);
    class procedure WriteLineP(p: pointer);
    class procedure WriteLineS(const s: string);
    class procedure WriteLineU(const s: UTF8String);
    class procedure WriteLineW(const s: WideString);
    class procedure WriteLineC(const s: PChar);
    class procedure WriteLine;

    class procedure WriteU8(i: byte);
    class procedure WriteS8(i: shortint);
    class procedure WriteU16(i: word);
    class procedure WriteS16(i: smallint);
    class procedure WriteU32(i: cardinal);
    class procedure WriteS32(i: integer);
    class procedure WriteS64(i: int64);
    class procedure WriteF(i: single);
    class procedure WriteD(i: double);
    class procedure WriteB(b: boolean);
    class procedure WriteP(p: pointer);
    class procedure WriteS(const s: string);
    class procedure WriteU(const s: UTF8String); 
    class procedure WriteW(const s: WideString); 
    class procedure WriteC(const s: PChar);

    class function  GetForegroundColor: TColor;
    class function  GetBackgroundColor: TColor;

    class function  GetLine(i: integer): string;
    class procedure SetLine(i: integer; s: string);

    class procedure SetForegroundColor(value: TColor);
    class procedure SetBackgroundColor(value: TColor); 

    class function  KeyPressed: boolean;
    class function  ReadLine: string;   
    class function  ReadInt: int64;   
    class function  ReadFloat: double;   
    class function  ReadKey: string;
  end;

  Colors = class
  public
    class function RGB(red, green, blue: byte): TColor;

    class function RedValue(aColor: TColor): byte;
    class function GreenValue(aColor: TColor): byte;
    class function BlueValue(aColor: TColor): byte;
  end;


procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
    Target.Method['Console.GetForegroundColor[0]', C_UnitName] := @TConsole.GetForegroundColor;
    Target.Method['Console.GetBackgroundColor[0]', C_UnitName] := @TConsole.GetBackgroundColor; 
    Target.Method['Console.SetForegroundColor[0]', C_UnitName] := @TConsole.SetForegroundColor;
    Target.Method['Console.SetBackgroundColor[0]', C_UnitName] := @TConsole.SetBackgroundColor;
    Target.Method['Console.GetLine[0]', C_UnitName] := @TConsole.GetLine;
    Target.Method['Console.SetLine[0]', C_UnitName] := @TConsole.SetLine;

    Target.Method['Console.Clear[0]', C_UnitName] := @TConsole.Clear;
    Target.Method['Console.ClearLine[0]', C_UnitName] := @TConsole.ClearLine;

    Target.Method['Console.CursorTo[0]', C_UnitName] := @TConsole.CursorTo;
    Target.Method['Console.ScrollTo[0]', C_UnitName] := @TConsole.ScrollTo;

    Target.Method['Console.WriteLine[0]', C_UnitName] := @TConsole.WriteLineU8;
    Target.Method['Console.WriteLine[1]', C_UnitName] := @TConsole.WriteLineS8;
    Target.Method['Console.WriteLine[2]', C_UnitName] := @TConsole.WriteLineU16;
    Target.Method['Console.WriteLine[3]', C_UnitName] := @TConsole.WriteLineS16;
    Target.Method['Console.WriteLine[4]', C_UnitName] := @TConsole.WriteLineU32;
    Target.Method['Console.WriteLine[5]', C_UnitName] := @TConsole.WriteLineS32;
    Target.Method['Console.WriteLine[6]', C_UnitName] := @TConsole.WriteLineS64;
    Target.Method['Console.WriteLine[7]', C_UnitName] := @TConsole.WriteLineF;
    Target.Method['Console.WriteLine[8]', C_UnitName] := @TConsole.WriteLineD;
    Target.Method['Console.WriteLine[9]', C_UnitName] := @TConsole.WriteLineB;  
    Target.Method['Console.WriteLine[10]', C_UnitName] := @TConsole.WriteLineP;
    Target.Method['Console.WriteLine[11]', C_UnitName] := @TConsole.WriteLineS;
    Target.Method['Console.WriteLine[12]', C_UnitName] := @TConsole.WriteLineU;
    Target.Method['Console.WriteLine[13]', C_UnitName] := @TConsole.WriteLineW;
    Target.Method['Console.WriteLine[14]', C_UnitName] := @TConsole.WriteLineC;
    Target.Method['Console.WriteLine[15]', C_UnitName] := @TConsole.WriteLine;

    Target.Method['Console.Write[0]', C_UnitName] := @TConsole.WriteU8;
    Target.Method['Console.Write[1]', C_UnitName] := @TConsole.WriteS8;
    Target.Method['Console.Write[2]', C_UnitName] := @TConsole.WriteU16;
    Target.Method['Console.Write[3]', C_UnitName] := @TConsole.WriteS16;
    Target.Method['Console.Write[4]', C_UnitName] := @TConsole.WriteU32;
    Target.Method['Console.Write[5]', C_UnitName] := @TConsole.WriteS32;
    Target.Method['Console.Write[6]', C_UnitName] := @TConsole.WriteS64;
    Target.Method['Console.Write[7]', C_UnitName] := @TConsole.WriteF;
    Target.Method['Console.Write[8]', C_UnitName] := @TConsole.WriteD;
    Target.Method['Console.Write[9]', C_UnitName] := @TConsole.WriteB;  
    Target.Method['Console.Write[10]', C_UnitName] := @TConsole.WriteP;
    Target.Method['Console.Write[11]', C_UnitName] := @TConsole.WriteS;
    Target.Method['Console.Write[12]', C_UnitName] := @TConsole.WriteU;
    Target.Method['Console.Write[13]', C_UnitName] := @TConsole.WriteW;
    Target.Method['Console.Write[14]', C_UnitName] := @TConsole.WriteC;

    Target.Method['Console.KeyPressed[0]', C_UnitName] := @TConsole.KeyPressed;
    Target.Method['Console.ReadLine[0]', C_UnitName] := @TConsole.ReadLine;
    Target.Method['Console.ReadInt[0]', C_UnitName] := @TConsole.ReadInt;
    Target.Method['Console.ReadFloat[0]', C_UnitName] := @TConsole.ReadFloat;
    Target.Method['Console.ReadKey[0]', C_UnitName] := @TConsole.ReadKey;

    Target.Method['Colors.RGB[0]', C_UnitName] := @Colors.RGB;

    Target.Method['Colors.RedValue[0]', C_UnitName] := @Colors.RedValue;
    Target.Method['Colors.GreenValue[0]', C_UnitName] := @Colors.GreenValue;
    Target.Method['Colors.BlueValue[0]', C_UnitName] := @Colors.BlueValue;
  end;
end;

procedure RegisterUnit;
var p : TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;          
  p.Priority          := 2;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

{ Colors }

class function Colors.BlueValue(aColor: TColor): byte;
begin
  result := byte(aColor shr 16)
end;

class function Colors.GreenValue(aColor: TColor): byte;
begin
  result := byte(aColor shr 8);
end;

class function Colors.RedValue(aColor: TColor): byte;
begin
  result := byte(aColor and $FF);
end;

class function Colors.RGB(red, green, blue: byte): TColor;
begin
  result := (red or (green shl 8) or (blue shl 16));
end;

{ TConsole }

class procedure TConsole.Clear;
begin
  if @TSE2Console.Clear <> nil then
     TSE2Console.Clear;
end;

class function TConsole.ReadFloat: double;
begin
  result := StrToFloatDef(TConsole.ReadLine, 0);
end;

class function TConsole.ReadInt: int64;
begin
  result := StrToInt64Def(TConsole.ReadLine, 0);
end;

class function TConsole.ReadKey: string;
begin
  if @TSE2Console.ReadKey <> nil then
     result := TSE2Console.ReadKey;
end;

class function TConsole.ReadLine: string;
begin
  if @TSE2Console.Read <> nil then
     result := TSE2Console.Read;
end;

class procedure TConsole.WriteS8(i: shortint);
begin
  TConsole.WriteS(IntToStr(i));
end;

class procedure TConsole.WriteU16(i: word);
begin
  TConsole.WriteS(IntToStr(i));
end;

class procedure TConsole.WriteU8(i: byte);
begin
  TConsole.WriteS(IntToStr(i));
end;

class procedure TConsole.WriteD(i: double);
begin
  TConsole.WriteS(FloatToStr(i));
end;

class procedure TConsole.WriteF(i: single);
begin
  TConsole.WriteS(FloatToStr(i));
end;

class procedure TConsole.WriteU32(i: cardinal);
begin
  TConsole.WriteS(IntToStr(i));
end;

class procedure TConsole.WriteS16(i: smallint);
begin
  TConsole.WriteS(IntToStr(i));
end;

class procedure TConsole.WriteS64(i: int64);
begin
  TConsole.WriteS(IntToStr(i));
end;

class procedure TConsole.WriteS32(i: integer);
begin
  TConsole.WriteS(IntToStr(i));
end;

class procedure TConsole.WriteLineS16(i: smallint);
begin
  TConsole.WriteLineS(IntToStr(i));
end;

class procedure TConsole.WriteLineU32(i: cardinal);
begin
  TConsole.WriteLineS(IntToStr(i));
end;

class procedure TConsole.WriteLineU16(i: word);
begin
  TConsole.WriteLineS(IntToStr(i));
end;

class procedure TConsole.WriteLineU8(i: byte);
begin
  TConsole.WriteLineS(IntToStr(i));
end;

class procedure TConsole.WriteLineS8(i: shortint);
begin
  TConsole.WriteLineS(IntToStr(i));
end;

class procedure TConsole.WriteLineS64(i: int64);
begin
  TConsole.WriteLineS(IntToStr(i));
end;

class procedure TConsole.WriteLineS32(i: integer);
begin
  TConsole.WriteLineS(IntToStr(i));
end;

class procedure TConsole.WriteLineD(i: double);
begin
  TConsole.WriteLineS(FloatToStr(i));
end;

class procedure TConsole.WriteLineF(i: single);
begin
  TConsole.WriteLineS(FloatToStr(i));
end;   

class procedure TConsole.WriteLineB(b: boolean);
begin
  TConsole.WriteLineS(BoolToStr(b, True));
end;

class procedure TConsole.WriteB(b: boolean);
begin
  TConsole.WriteS(BoolToStr(b, True));
end;

class procedure TConsole.WriteC(const s: PChar);
begin
  TConsole.WriteS(s);
end;

class procedure TConsole.WriteW(const s: WideString);
begin
  TConsole.WriteS(Utf8ToAnsi(UTF8Encode(s)));
end;

class procedure TConsole.WriteU(const s: UTF8String);
begin
  TConsole.WriteS(Utf8ToAnsi(s));
end;

class procedure TConsole.WriteS(const s: string);
begin
  if @TSE2Console.Write <> nil then
     TSE2Console.Write(s);
  // Output
end;

class procedure TConsole.WriteLineS(const s: string);
begin
  TConsole.WriteS(s + #13#10);
end;    

class procedure TConsole.WriteLineU(const s: UTF8String);
begin
  TConsole.WriteLineS(Utf8ToAnsi(s));
end;

class procedure TConsole.WriteLineC(const s: PChar);
begin
  TConsole.WriteLineS(s);
end;

class procedure TConsole.WriteLineW(const s: WideString);
begin
  TConsole.WriteLineS(Utf8ToAnsi(UTF8Encode(s)));
end;         

class procedure TConsole.WriteLineP(p: pointer);
begin
  if p = nil then
     TConsole.WriteLineS('nil')
  else
     TConsole.WriteLineS('$' + IntToHex(cardinal(p), 8));
end;

class procedure TConsole.WriteP(p: pointer);
begin       
  if p = nil then
     TConsole.WriteS('nil')
  else
     TConsole.WriteS('$' + IntToHex(cardinal(p), 8));
end;

class procedure TConsole.WriteLine;
begin
  TConsole.WriteLineS('');
end;

class function TConsole.KeyPressed: boolean;
begin
  if @TSE2Console.KeyPressed <> nil then
     result := TSE2Console.KeyPressed
  else
     result := False;
end;

class procedure TConsole.ClearLine;
begin
  if @TSE2Console.ClearLn <> nil then
     TSE2Console.ClearLn;
end;

class procedure TConsole.CursorTo(x, y: integer);
begin
  if @TSE2Console.CursorTo <> nil then
     TSE2Console.CursorTo(x, y);
end;

class procedure TConsole.ScrollTo(x, y: integer);
begin              
  if @TSE2Console.ScrollTo <> nil then
     TSE2Console.ScrollTo(x, y);
end;

class function TConsole.GetBackgroundColor: TColor;
begin
  if @TSE2Console.GetBgColor <> nil then
     result := TSE2Console.GetBgColor
  else
     result := clBlack;
end;

class function TConsole.GetForegroundColor: TColor;
begin
  if @TSE2Console.GetFgColor <> nil then
     result := TSE2Console.GetFgColor
  else
     result := clWhite;
end;

class procedure TConsole.SetBackgroundColor(value: TColor);
begin
  if @TSE2Console.SetBgColor <> nil then
     TSE2Console.SetBgColor(value);
end;

class procedure TConsole.SetForegroundColor(value: TColor);
begin                    
  if @TSE2Console.SetFgColor <> nil then
     TSE2Console.SetFgColor(value);
end;

class function TConsole.GetLine(i: integer): string;
begin
  if @TSE2Console.GetLine <> nil then
     result := TSE2Console.GetLine(i)
  else
     result := '';
end;

class procedure TConsole.SetLine(i: integer; s: string);
begin
  if @TSE2Console.SetLine <> nil then
     TSE2Console.SetLine(i, s);
end;

initialization
  RegisterUnit;
  TSE2Console.Clear := nil;
  TSE2Console.Write := nil;

end.
