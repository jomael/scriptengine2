unit uSE2IncConvert;        

{$INCLUDE ScriptEngine.inc}

interface

uses
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;

implementation

uses
  SysUtils, uSE2OpCode;

const
  C_UnitName   = 'System';
  C_UnitSource =
        'unit System;' + #13#10 +
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  TFloatFormat = (ffGeneral, ffExponent, ffFixed, ffNumber, ffCurrency);' + #13#10 + 
        #13#10 +
        '  /// Provides methods to convert different types'+#13#10+
        '  Convert = sealed partial class(TExternalObject)' + #13#10 + 
        '  public' + #13#10 + 
        '    class function ToInt8(value: Shortint): shortint; overload;' + #13#10 + 
        '    class function ToInt8(value: Byte): shortint; overload;' + #13#10 + 
        '    class function ToInt8(value: Smallint): shortint; overload;' + #13#10 + 
        '    class function ToInt8(value: Word): shortint; overload;' + #13#10 + 
        '    class function ToInt8(value: Integer): shortint; overload;' + #13#10 + 
        '    class function ToInt8(value: Cardinal): shortint; overload;' + #13#10 + 
        '    class function ToInt8(value: int64): shortint; overload;' + #13#10 + 
        '    class function ToInt8(value: UInt64): shortint; overload;' + #13#10 + 
        '    class function ToInt8(value: string): shortint; overload; external;' + #13#10 + 
        '    class function ToInt8(value: string; Default: shortint): shortint; overload; external;' + #13#10 + 
        '    class function ToInt8(value: boolean): shortint; overload;' + #13#10 + 
        '    class function ToInt8(value: single): shortint; overload;' + #13#10 +
        '    class function ToInt8(value: double): shortint; overload;' + #13#10 + 
        #13#10 + 
        '    class function ToInt16(value: Shortint): smallint; overload;' + #13#10 + 
        '    class function ToInt16(value: Byte): smallint; overload;' + #13#10 + 
        '    class function ToInt16(value: Smallint): smallint; overload;' + #13#10 + 
        '    class function ToInt16(value: Word): smallint; overload;' + #13#10 + 
        '    class function ToInt16(value: Integer): smallint; overload;' + #13#10 + 
        '    class function ToInt16(value: Cardinal): smallint; overload;' + #13#10 + 
        '    class function ToInt16(value: int64): smallint; overload;' + #13#10 + 
        '    class function ToInt16(value: UInt64): smallint; overload;' + #13#10 + 
        '    class function ToInt16(value: string): smallint; overload; external;' + #13#10 + 
        '    class function ToInt16(value: string; Default: smallint): smallint; overload; external;' + #13#10 + 
        '    class function ToInt16(value: boolean): smallint; overload;' + #13#10 + 
        '    class function ToInt16(value: single): smallint; overload;' + #13#10 + 
        '    class function ToInt16(value: double): smallint; overload;' + #13#10 + 
        #13#10 + 
        '    class function ToInt32(value: Shortint): integer; overload;' + #13#10 + 
        '    class function ToInt32(value: Byte): integer; overload;' + #13#10 + 
        '    class function ToInt32(value: Smallint): integer; overload;' + #13#10 + 
        '    class function ToInt32(value: Word): integer; overload;' + #13#10 + 
        '    class function ToInt32(value: Integer): integer; overload;' + #13#10 + 
        '    class function ToInt32(value: Cardinal): integer; overload;' + #13#10 +
        '    class function ToInt32(value: int64): integer; overload;' + #13#10 + 
        '    class function ToInt32(value: UInt64): integer; overload;' + #13#10 + 
        '    class function ToInt32(value: string): integer; overload; external;' + #13#10 + 
        '    class function ToInt32(value: string; Default: integer): integer; overload; external;' + #13#10 + 
        '    class function ToInt32(value: boolean): integer; overload;' + #13#10 + 
        '    class function ToInt32(value: single): integer; overload;' + #13#10 + 
        '    class function ToInt32(value: double): integer; overload;' + #13#10 + 
        #13#10 + 
        '    class function ToInt64(value: Shortint): int64; overload;' + #13#10 + 
        '    class function ToInt64(value: Byte): int64; overload;' + #13#10 + 
        '    class function ToInt64(value: Smallint): int64; overload;' + #13#10 + 
        '    class function ToInt64(value: Word): int64; overload;' + #13#10 + 
        '    class function ToInt64(value: Integer): int64; overload;' + #13#10 + 
        '    class function ToInt64(value: Cardinal): int64; overload;' + #13#10 + 
        '    class function ToInt64(value: int64): int64; overload;' + #13#10 + 
        '    class function ToInt64(value: UInt64): int64; overload;' + #13#10 + 
        '    class function ToInt64(value: string): int64; overload; external;' + #13#10 + 
        '    class function ToInt64(value: string; Default: int64): int64; overload; external;' + #13#10 + 
        '    class function ToInt64(value: boolean): int64; overload;' + #13#10 + 
        '    class function ToInt64(value: single): int64; overload;' + #13#10 + 
        '    class function ToInt64(value: double): int64; overload;' + #13#10 + 
        #13#10 +
        '    class function ToUInt8(value: Shortint): Byte; overload;' + #13#10 + 
        '    class function ToUInt8(value: Byte): Byte; overload;' + #13#10 + 
        '    class function ToUInt8(value: Smallint): Byte; overload;' + #13#10 + 
        '    class function ToUInt8(value: Word): Byte; overload;' + #13#10 + 
        '    class function ToUInt8(value: Integer): Byte; overload;' + #13#10 + 
        '    class function ToUInt8(value: Cardinal): Byte; overload;' + #13#10 + 
        '    class function ToUInt8(value: int64): Byte; overload;' + #13#10 + 
        '    class function ToUInt8(value: UInt64): Byte; overload;' + #13#10 + 
        '    class function ToUInt8(value: string): Byte; overload; external;' + #13#10 + 
        '    class function ToUInt8(value: string; Default: Byte): Byte; overload; external;' + #13#10 + 
        '    class function ToUInt8(value: boolean): Byte; overload;' + #13#10 + 
        '    class function ToUInt8(value: single): Byte; overload;' + #13#10 + 
        '    class function ToUInt8(value: double): Byte; overload;' + #13#10 + 
        #13#10 + 
        '    class function ToUInt16(value: Shortint): Word; overload;' + #13#10 + 
        '    class function ToUInt16(value: Byte): Word; overload;' + #13#10 + 
        '    class function ToUInt16(value: Smallint): Word; overload;' + #13#10 + 
        '    class function ToUInt16(value: Word): Word; overload;' + #13#10 + 
        '    class function ToUInt16(value: Integer): Word; overload;' + #13#10 + 
        '    class function ToUInt16(value: Cardinal): Word; overload;' + #13#10 + 
        '    class function ToUInt16(value: int64): Word; overload;' + #13#10 + 
        '    class function ToUInt16(value: UInt64): Word; overload;' + #13#10 +
        '    class function ToUInt16(value: string): Word; overload; external;' + #13#10 + 
        '    class function ToUInt16(value: string; Default: Word): Word; overload; external;' + #13#10 + 
        '    class function ToUInt16(value: boolean): Word; overload;' + #13#10 + 
        '    class function ToUInt16(value: single): Word; overload;' + #13#10 + 
        '    class function ToUInt16(value: double): Word; overload;' + #13#10 + 
        #13#10 + 
        '    class function ToUInt32(value: Shortint): cardinal; overload;' + #13#10 + 
        '    class function ToUInt32(value: Byte): cardinal; overload;' + #13#10 + 
        '    class function ToUInt32(value: Smallint): cardinal; overload;' + #13#10 + 
        '    class function ToUInt32(value: Word): cardinal; overload;' + #13#10 + 
        '    class function ToUInt32(value: Integer): cardinal; overload;' + #13#10 + 
        '    class function ToUInt32(value: Cardinal): cardinal; overload;' + #13#10 + 
        '    class function ToUInt32(value: int64): cardinal; overload;' + #13#10 + 
        '    class function ToUInt32(value: UInt64): cardinal; overload;' + #13#10 + 
        '    class function ToUInt32(value: string): cardinal; overload; external;' + #13#10 + 
        '    class function ToUInt32(value: string; Default: cardinal): cardinal; overload; external;' + #13#10 + 
        '    class function ToUInt32(value: boolean): cardinal; overload;' + #13#10 + 
        '    class function ToUInt32(value: single): cardinal; overload;' + #13#10 + 
        '    class function ToUInt32(value: double): cardinal; overload;' + #13#10 + 
        #13#10 + 
        '    class function ToUInt64(value: Shortint): Uint64; overload;' + #13#10 + 
        '    class function ToUInt64(value: Byte): Uint64; overload;' + #13#10 +
        '    class function ToUInt64(value: Smallint): Uint64; overload;' + #13#10 + 
        '    class function ToUInt64(value: Word): Uint64; overload;' + #13#10 + 
        '    class function ToUInt64(value: Integer): Uint64; overload;' + #13#10 + 
        '    class function ToUInt64(value: Cardinal): Uint64; overload;' + #13#10 + 
        '    class function ToUInt64(value: int64): Uint64; overload;' + #13#10 + 
        '    class function ToUInt64(value: UInt64): UInt64; overload;' + #13#10 + 
        '    class function ToUInt64(value: string): Uint64; overload; external;' + #13#10 + 
        '    class function ToUInt64(value: string; Default: Uint64): Uint64; overload; external;' + #13#10 + 
        '    class function ToUInt64(value: boolean): Uint64; overload;' + #13#10 + 
        '    class function ToUInt64(value: single): Uint64; overload;' + #13#10 + 
        '    class function ToUInt64(value: double): Uint64; overload;' + #13#10 + 
        #13#10 + 
        '    class function ToSingle(value: Shortint): Single; overload;' + #13#10 + 
        '    class function ToSingle(value: Byte): Single; overload;' + #13#10 + 
        '    class function ToSingle(value: Smallint): Single; overload;' + #13#10 + 
        '    class function ToSingle(value: Word): Single; overload;' + #13#10 + 
        '    class function ToSingle(value: Integer): Single; overload;' + #13#10 + 
        '    class function ToSingle(value: Cardinal): Single; overload;' + #13#10 + 
        '    class function ToSingle(value: int64): Single; overload;' + #13#10 + 
        '    class function ToSingle(value: UInt64): Single; overload;' + #13#10 + 
        '    class function ToSingle(value: string): Single; overload; external;' + #13#10 + 
        '    class function ToSingle(value: string; Default: Single): Single; overload; external;' + #13#10 +
        '    class function ToSingle(value: boolean): Single; overload;' + #13#10 + 
        '    class function ToSingle(value: single): Single; overload;' + #13#10 + 
        '    class function ToSingle(value: double): Single; overload;' + #13#10 + 
        #13#10 + 
        '    class function ToDouble(value: Shortint): Double; overload;' + #13#10 + 
        '    class function ToDouble(value: Byte): Double; overload;' + #13#10 + 
        '    class function ToDouble(value: Smallint): Double; overload;' + #13#10 + 
        '    class function ToDouble(value: Word): Double; overload;' + #13#10 + 
        '    class function ToDouble(value: Integer): Double; overload;' + #13#10 + 
        '    class function ToDouble(value: Cardinal): Double; overload;' + #13#10 + 
        '    class function ToDouble(value: int64): Double; overload;' + #13#10 + 
        '    class function ToDouble(value: UInt64): Double; overload;' + #13#10 + 
        '    class function ToDouble(value: string): Double; overload; external;' + #13#10 + 
        '    class function ToDouble(value: string; Default: Double): Double; overload; external;' + #13#10 + 
        '    class function ToDouble(value: boolean): Double; overload;' + #13#10 + 
        '    class function ToDouble(value: single): Double; overload;' + #13#10 + 
        '    class function ToDouble(value: double): Double; overload;' + #13#10 + 
        #13#10 + 
        '    class function ToBoolean(value: Shortint): Boolean; overload;' + #13#10 + 
        '    class function ToBoolean(value: Byte): Boolean; overload;' + #13#10 + 
        '    class function ToBoolean(value: Smallint): Boolean; overload;' + #13#10 + 
        '    class function ToBoolean(value: Word): Boolean; overload;' + #13#10 +
        '    class function ToBoolean(value: Integer): Boolean; overload;' + #13#10 + 
        '    class function ToBoolean(value: Cardinal): Boolean; overload;' + #13#10 + 
        '    class function ToBoolean(value: int64): Boolean; overload;' + #13#10 + 
        '    class function ToBoolean(value: UInt64): Boolean; overload;' + #13#10 + 
        '    class function ToBoolean(value: string): Boolean; overload; external;' + #13#10 + 
        '    class function ToBoolean(value: string; Default: Boolean): Boolean; overload; external;' + #13#10 + 
        '    class function ToBoolean(value: boolean): Boolean; overload;' + #13#10 + 
        '    class function ToBoolean(value: single): Boolean; overload;' + #13#10 + 
        '    class function ToBoolean(value: double): Boolean; overload;' + #13#10 + 
        #13#10 + 
        '    class function ToString(value: Shortint): String; overload; external;' + #13#10 + 
        '    class function ToString(value: ShortInt; Digits: byte): String; overload;' + #13#10 + 
        '    class function ToString(value: ShortInt; Digits: byte; Prefix: string): String; overload;' + #13#10 + 
        '    class function ToString(value: Byte): String; overload; external;' + #13#10 + 
        '    class function ToString(value: Byte; Digits: byte): String; overload;' + #13#10 + 
        '    class function ToString(value: Byte; Digits: byte; Prefix: string): String; overload;' + #13#10 + 
        '    class function ToString(value: Smallint): String; overload; external;' + #13#10 + 
        '    class function ToString(value: SmallInt; Digits: byte): String; overload;' + #13#10 + 
        '    class function ToString(value: SmallInt; Digits: byte; Prefix: string): String; overload;' + #13#10 + 
        '    class function ToString(value: Word): String; overload; external;' + #13#10 + 
        '    class function ToString(value: Word; Digits: byte): String; overload;' + #13#10 + 
        '    class function ToString(value: Word; Digits: byte; Prefix: string): String; overload;' + #13#10 +
        '    class function ToString(value: Integer): String; overload; external;' + #13#10 + 
        '    class function ToString(value: Integer; Digits: byte): String; overload;' + #13#10 + 
        '    class function ToString(value: Integer; Digits: byte; Prefix: string): String; overload;' + #13#10 + 
        '    class function ToString(value: Cardinal): String; overload; external;' + #13#10 + 
        '    class function ToString(value: Cardinal; Digits: byte): String; overload;' + #13#10 + 
        '    class function ToString(value: Cardinal; Digits: byte; Prefix: string): String; overload;' + #13#10 + 
        '    class function ToString(value: int64): String; overload;external;' + #13#10 + 
        '    class function ToString(value: int64; Digits: byte): String; overload;' + #13#10 + 
        '    class function ToString(value: int64; Digits: byte; Prefix: string): String; overload;' + #13#10 + 
        '    class function ToString(value: UInt64): String; overload; external;' + #13#10 + 
        '    class function ToString(value: UInt64; Digits: byte): String; overload;' + #13#10 + 
        '    class function ToString(value: UInt64; Digits: byte; Prefix: string): String; overload;' + #13#10 + 
        '    class function ToString(value: boolean): String; overload; external;' + #13#10 + 
        '    class function ToString(value: boolean; UseBooleanStrings: boolean): String; overload; external;' + #13#10 + 
        '    class function ToString(value: single): String; overload; external;' + #13#10 + 
        '    class function ToString(value: single; Digits: byte): String; overload; external;' + #13#10 + 
        '    class function ToString(value: single; Format: TFloatFormat; Digits: byte): String; overload; external;' + #13#10 +
        '    class function ToString(value: single; Format: TFloatFormat; Precision, Digits: byte): String; overload; external;' + #13#10 +
        '    class function ToString(value: double): String; overload; external;' + #13#10 + 
        '    class function ToString(value: double; Digits: byte): String; overload; external;' + #13#10 +
        '    class function ToString(value: double; Format: TFloatFormat; Digits: byte): String; overload; external;' + #13#10 +
        '    class function ToString(value: double; Format: TFloatFormat; Precision, Digits: byte): String; overload; external;' + #13#10 +
        #13#10 + 
        '    class function ToHex(value: Shortint): String; overload;' + #13#10 + 
        '    class function ToHex(value: Shortint; Digits: integer): String; overload; external;' + #13#10 + 
        '    class function ToHex(value: Byte): String; overload;' + #13#10 + 
        '    class function ToHex(value: Byte; Digits: integer): String; overload; external;' + #13#10 + 
        '    class function ToHex(value: Smallint): String; overload;' + #13#10 + 
        '    class function ToHex(value: Smallint; Digits: integer): String; overload; external;' + #13#10 + 
        '    class function ToHex(value: Word): String; overload;' + #13#10 + 
        '    class function ToHex(value: Word; Digits: integer): String; overload; external;' + #13#10 + 
        '    class function ToHex(value: Integer): String; overload;' + #13#10 + 
        '    class function ToHex(value: Integer; Digits: integer): String; overload; external;' + #13#10 + 
        '    class function ToHex(value: Cardinal): String; overload;' + #13#10 + 
        '    class function ToHex(value: Cardinal; Digits: integer): String; overload; external;' + #13#10 + 
        '    class function ToHex(value: Int64): String; overload;' + #13#10 + 
        '    class function ToHex(value: Int64; Digits: integer): String; overload; external;' + #13#10 + 
        '    class function ToHex(value: UInt64): String; overload;' + #13#10 + 
        '    class function ToHex(value: UInt64; Digits: integer): String; overload; external;' + #13#10 + 
        #13#10 + 
        '    class function TryToInt8(value: string; out res: shortint): boolean; external;' + #13#10 + 
        '    class function TryToInt16(value: string; out res: smallint): boolean; external;' + #13#10 + 
        '    class function TryToInt32(value: string; out res: integer): boolean; external;' + #13#10 + 
        '    class function TryToInt64(value: string; out res: int64): boolean; external;' + #13#10 +
        '    class function TryToUInt8(value: string; out res: byte): boolean; external;' + #13#10 + 
        '    class function TryToUInt16(value: string; out res: word): boolean; external;' + #13#10 + 
        '    class function TryToUInt32(value: string; out res: cardinal): boolean; external;' + #13#10 + 
        '    class function TryToUInt64(value: string; out res: UInt64): boolean; external;' + #13#10 + 
        '    class function TryToSingle(value: string; out res: single): boolean; external;' + #13#10 + 
        '    class function TryToDouble(value: string; out res: double): boolean; external;' + #13#10 + 
        '    class function TryToBoolean(value: string; out res: boolean): boolean; external;' + #13#10 + 
        #13#10 + 
        '    class function IsInt8(value: string): boolean; external;' + #13#10 + 
        '    class function IsInt16(value: string): boolean; external;' + #13#10 + 
        '    class function IsInt32(value: string): boolean; external;' + #13#10 + 
        '    class function IsInt64(value: string): boolean; external;' + #13#10 + 
        '    class function IsUInt8(value: string): boolean; external;' + #13#10 + 
        '    class function IsUInt16(value: string): boolean; external;' + #13#10 + 
        '    class function IsUInt32(value: string): boolean; external;' + #13#10 + 
        '    class function IsUInt64(value: string): boolean; external;' + #13#10 + 
        '    class function IsSingle(value: string): boolean; external;' + #13#10 + 
        '    class function IsDouble(value: string): boolean; external;' + #13#10 + 
        '    class function IsBoolean(value: string): boolean; external;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 +
        #13#10 + 
        '{ ToInt8 }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt8(value: Shortint): shortint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt8(value: Byte): shortint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt8(value: Smallint): shortint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt8(value: Word): shortint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 +
        #13#10 + 
        'class function Convert.ToInt8(value: Integer): shortint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt8(value: Cardinal): shortint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt8(value: int64): shortint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt8(value: Uint64): shortint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt8(value: boolean): shortint;' + #13#10 +
        'begin' + #13#10 + 
        '  if value then' + #13#10 + 
        '     result := 1' + #13#10 + 
        '  else' + #13#10 + 
        '     result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt8(value: single): shortint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt8(value: double): shortint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToInt16 }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt16(value: Shortint): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 +
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt16(value: Byte): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt16(value: Smallint): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt16(value: Word): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt16(value: Integer): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 +
        'class function Convert.ToInt16(value: Cardinal): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt16(value: int64): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt16(value: Uint64): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt16(value: boolean): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  if value then' + #13#10 + 
        '     result := 1' + #13#10 + 
        '  else' + #13#10 + 
        '     result := 0;' + #13#10 + 
        'end;' + #13#10 +
        #13#10 + 
        'class function Convert.ToInt16(value: single): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt16(value: double): smallint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToInt32 }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt32(value: Shortint): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt32(value: Byte): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 + 
        'end;' + #13#10 +
        #13#10 + 
        'class function Convert.ToInt32(value: Smallint): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt32(value: Word): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt32(value: Integer): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt32(value: Cardinal): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt32(value: int64): integer;' + #13#10 +
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt32(value: Uint64): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt32(value: boolean): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  if value then' + #13#10 + 
        '     result := 1' + #13#10 + 
        '  else' + #13#10 + 
        '     result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt32(value: single): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 +
        'class function Convert.ToInt32(value: double): integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToInt64 }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt64(value: Shortint): int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt64(value: Byte): int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt64(value: Smallint): int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 +
        'class function Convert.ToInt64(value: Word): int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt64(value: Integer): int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt64(value: Cardinal): int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt64(value: int64): int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt64(value: Uint64): int64;' + #13#10 + 
        'begin' + #13#10 +
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt64(value: boolean): int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  if value then' + #13#10 + 
        '     result := 1' + #13#10 + 
        '  else' + #13#10 + 
        '     result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt64(value: single): int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToInt64(value: double): int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToUInt8 }' + #13#10 +
        #13#10 + 
        'class function Convert.ToUInt8(value: Shortint): byte;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt8(value: Byte): byte;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt8(value: Smallint): byte;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt8(value: Word): byte;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt8(value: Integer): byte;' + #13#10 +
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt8(value: Cardinal): byte;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt8(value: int64): byte;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt8(value: UInt64): byte;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt8(value: boolean): byte;' + #13#10 + 
        'begin' + #13#10 + 
        '  if value then' + #13#10 +
        '     result := 1' + #13#10 + 
        '  else' + #13#10 + 
        '     result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt8(value: single): byte;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt8(value: double): byte;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToUInt16 }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt16(value: Shortint): word;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 +
        'class function Convert.ToUInt16(value: Byte): word;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt16(value: Smallint): word;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt16(value: Word): word;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt16(value: Integer): word;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt16(value: Cardinal): word;' + #13#10 + 
        'begin' + #13#10 +
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt16(value: int64): word;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt16(value: UInt64): word;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt16(value: boolean): word;' + #13#10 + 
        'begin' + #13#10 + 
        '  if value then' + #13#10 + 
        '     result := 1' + #13#10 + 
        '  else' + #13#10 + 
        '     result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt16(value: single): word;' + #13#10 +
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt16(value: double): word;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToUInt32 }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: Shortint): cardinal;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: Byte): cardinal;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: Smallint): cardinal;' + #13#10 +
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: Word): cardinal;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: Integer): cardinal;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: Cardinal): cardinal;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: int64): cardinal;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 +
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: UInt64): cardinal;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: boolean): cardinal;' + #13#10 + 
        'begin' + #13#10 + 
        '  if value then' + #13#10 + 
        '     result := 1' + #13#10 + 
        '  else' + #13#10 + 
        '     result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: single): cardinal;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt32(value: double): cardinal;' + #13#10 + 
        'begin' + #13#10 +
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToUInt64 }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt64(value: Shortint): UInt64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt64(value: Byte): UInt64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt64(value: Smallint): UInt64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt64(value: Word): UInt64;' + #13#10 + 
        'begin' + #13#10 +
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt64(value: Integer): UInt64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt64(value: Cardinal): UInt64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt64(value: int64): UInt64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt64(value: UInt64): UInt64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 +
        #13#10 + 
        'class function Convert.ToUInt64(value: boolean): UInt64;' + #13#10 + 
        'begin' + #13#10 + 
        '  if value then' + #13#10 + 
        '     result := 1' + #13#10 + 
        '  else' + #13#10 + 
        '     result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt64(value: single): UInt64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToUInt64(value: double): UInt64;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Round(value);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToSingle }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: Shortint): Single;' + #13#10 +
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: Byte): Single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: Smallint): Single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: Word): Single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: Integer): Single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 +
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: Cardinal): Single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: int64): Single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: UInt64): Single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: boolean): Single;' + #13#10 + 
        'begin' + #13#10 + 
        '  if value then' + #13#10 + 
        '     result := 1' + #13#10 + 
        '  else' + #13#10 +
        '     result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: single): Single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToSingle(value: double): Single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToDouble }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToDouble(value: Shortint): Double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToDouble(value: Byte): Double;' + #13#10 + 
        'begin' + #13#10 +
        '  result := value' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToDouble(value: Smallint): Double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToDouble(value: Word): Double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToDouble(value: Integer): Double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToDouble(value: Cardinal): Double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 +
        #13#10 + 
        'class function Convert.ToDouble(value: int64): Double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToDouble(value: UInt64): Double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToDouble(value: boolean): Double;' + #13#10 + 
        'begin' + #13#10 + 
        '  if value then' + #13#10 + 
        '     result := 1' + #13#10 + 
        '  else' + #13#10 + 
        '     result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToDouble(value: single): Double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 +
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToDouble(value: double): Double;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToBoolean }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToBoolean(value: Shortint): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value <> 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToBoolean(value: Byte): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value <> 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToBoolean(value: Smallint): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value <> 0;' + #13#10 +
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToBoolean(value: Word): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value <> 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToBoolean(value: Integer): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value <> 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToBoolean(value: Cardinal): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value <> 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToBoolean(value: int64): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value <> 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 +
        'class function Convert.ToBoolean(value: UInt64): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value <> 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToBoolean(value: boolean): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToBoolean(value: single): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value <> 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToBoolean(value: double): Boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := value <> 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToString }' + #13#10 + 
        #13#10 +
        'class function Convert.ToString(value: ShortInt; Digits: byte): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := ''0'' + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: ShortInt; Digits: byte; Prefix: string): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := Prefix + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: Byte; Digits: byte): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := ''0'' + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: Byte; Digits: byte; Prefix: string): String;' + #13#10 +
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := Prefix + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: SmallInt; Digits: byte): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := ''0'' + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: SmallInt; Digits: byte; Prefix: string): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := Prefix + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: Word; Digits: byte): String;' + #13#10 + 
        'begin' + #13#10 +
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := ''0'' + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: Word; Digits: byte; Prefix: string): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := Prefix + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: Integer; Digits: byte): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := ''0'' + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: integer; Digits: byte; Prefix: string): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 +
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := Prefix + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: Cardinal; Digits: byte): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := ''0'' + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: Cardinal; Digits: byte; Prefix: string): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := Prefix + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: int64; Digits: byte): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 +
        '    result := ''0'' + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: int64; Digits: byte; Prefix: string): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := Prefix + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: UInt64; Digits: byte): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := ''0'' + result;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToString(value: UInt64; Digits: byte; Prefix: string): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToString(value);' + #13#10 + 
        '  while Strings.Length(result) < Digits do' + #13#10 + 
        '    result := Prefix + result;' + #13#10 +
        'end;' + #13#10 + 
        #13#10 + 
        '{ ToHex }' + #13#10 + 
        #13#10 + 
        'class function Convert.ToHex(value: Shortint): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToHex(value, 1);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToHex(value: Byte): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToHex(value, 1);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToHex(value: Smallint): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToHex(value, 1);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToHex(value: Word): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToHex(value, 1);' + #13#10 +
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToHex(value: Integer): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToHex(value, 1);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToHex(value: Cardinal): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToHex(value, 1);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToHex(value: int64): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToHex(value, 1);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function Convert.ToHex(value: UInt64): String;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Convert.ToHex(value, 1);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 +
        'end.';

function Convert_ToInt88(__Self: pointer; value: String): Shortint;
begin
  result := StrToInt(value);
end;

function Convert_ToInt89(__Self: pointer; value: String; Default: Shortint): Shortint;
begin
  result := StrToIntDef(value, Default);
end;

function Convert_ToInt168(__Self: pointer; value: String): Smallint;
begin
  result := StrToInt(value);
end;

function Convert_ToInt169(__Self: pointer; value: String; Default: Smallint): Smallint;
begin
  result := StrToIntDef(value, Default);
end;

function Convert_ToInt328(__Self: pointer; value: String): Integer;
begin
  result := StrToInt(value);
end;

function Convert_ToInt329(__Self: pointer; value: String; Default: Integer): Integer;
begin
  result := StrToIntDef(value, Default);
end;

function Convert_ToInt648(__Self: pointer; value: String): Int64;
begin
  result := StrToInt64(value);
end;

function Convert_ToInt649(__Self: pointer; value: String; Default: Int64): Int64;
begin
  result := StrToInt64Def(value, Default);
end;

function Convert_ToUInt88(__Self: pointer; value: String): Byte;
begin
  result := StrToInt(value);
end;

function Convert_ToUInt89(__Self: pointer; value: String; Default: Byte): Byte;
begin
  result := StrToIntDef(value, Default);
end;

function Convert_ToUInt168(__Self: pointer; value: String): Word;
begin
  result := StrToInt(value);
end;

function Convert_ToUInt169(__Self: pointer; value: String; Default: Word): Word;
begin
  result := StrToIntDef(value, Default);
end;

function Convert_ToUInt328(__Self: pointer; value: String): Cardinal;
begin
  result := StrToInt(value);
end;

function Convert_ToUInt329(__Self: pointer; value: String; Default: Cardinal): Cardinal;
begin
  result := StrToIntDef(value, Default);
end;

function Convert_ToUInt648(__Self: pointer; value: String): TbtU64;
begin
  result := StrToInt64(value);
end;

function Convert_ToUInt649(__Self: pointer; value: String; Default: TbtU64): TbtU64;
begin
  result := StrToInt64Def(value, Default);
end;

function Convert_ToSingle8(__Self: pointer; value: String): Single;
var i: single;
    c: integer;
begin
  Val(value, i, c);
  if c = 0 then
     result := i
  else
     result := StrToFloat(value);
end;

function Convert_ToSingle9(__Self: pointer; value: String; Default: Single): Single;
var i: double;
    c: integer;
begin
  Val(value, i, c);
  if c = 0 then
     result := i
  else
     result := StrToFloatDef(value, Default);
end;

function Convert_ToDouble8(__Self: pointer; value: String): Double;
var i: double;
    c: integer;
begin
  Val(value, i, c);
  if c = 0 then
     result := i
  else
     result := StrToFloat(value);
end;

function Convert_ToDouble9(__Self: pointer; value: String; Default: Double): Double;
var i: double;
    c: integer;
begin
  Val(value, i, c);
  if c = 0 then
     result := i
  else
     result := StrToFloatDef(value, Default);
end;

function Convert_ToBoolean8(__Self: pointer; value: String): Boolean;
begin
  result := StrToBool(value);
end;

function Convert_ToBoolean9(__Self: pointer; value: String; Default: Boolean): Boolean;
begin
  result := StrToBoolDef(value, Default);
end;

function Convert_ToString(__Self: pointer; value: Shortint): String;
begin
  result := IntToStr(value);
end;

function Convert_ToString3(__Self: pointer; value: Byte): String;
begin
  result := IntToStr(value);
end;

function Convert_ToString6(__Self: pointer; value: Smallint): String;
begin
  result := IntToStr(value);
end;

function Convert_ToString9(__Self: pointer; value: Word): String;
begin
  result := IntToStr(value);
end;

function Convert_ToString12(__Self: pointer; value: Integer): String;
begin
  result := IntToStr(value);
end;

function Convert_ToString15(__Self: pointer; value: Cardinal): String;
begin
  result := IntToStr(value);
end;

function Convert_ToString18(__Self: pointer; value: Int64): String;
begin
  result := IntToStr(value);
end;

function Convert_ToString21(__Self: pointer; value: TbtU64): String;
begin
  result := Format('%u', [value]);
end;

function Convert_ToString24(__Self: pointer; value: Boolean): String;
begin
  result := BoolToStr(value);
end;

function Convert_ToString25(__Self: pointer; value, UseBooleanStrings: Boolean): String;
begin
  result := BoolToStr(value, UseBooleanStrings);
end;

function Convert_ToString26(__Self: pointer; value: Single): String;
begin
  result := FloatToStr(value);
end;

function Convert_ToString27(__Self: pointer; value: Single; Digits: byte): String;
begin
  result := FloatToStrF(value, ffFixed, 8, Digits);
end;

function Convert_ToString28(__Self: pointer; value: Single; Format: TFloatFormat; Digits: byte): String;
begin
  result := FloatToStrF(value, Format, 8, Digits);
end;

function Convert_ToString29(__Self: pointer; value: Single; Format: TFloatFormat; Precision, Digits: byte): String;
begin
  result := FloatToStrF(value, Format, Precision, Digits);
end;

function Convert_ToString30(__Self: pointer; value: Double): String;
begin
  result := FloatToStr(value);
end;

function Convert_ToString31(__Self: pointer; value: Double; Digits: byte): String;
begin
  result := FloatToStrF(value, ffFixed, 16, Digits);
end;

function Convert_ToString32(__Self: pointer; value: Double; Format: TFloatFormat; Digits: byte): String;
begin
  result := FloatToStrF(value, Format, 16, Digits);
end;

function Convert_ToString33(__Self: pointer; value: Double; Format: TFloatFormat; Precision, Digits: byte): String;
begin
  result := FloatToStrF(value, Format, Precision, Digits);
end;

function Convert_ToHex1(__Self: pointer; value: Shortint; Digits: Integer): String;
begin
  result := IntToHex(value, Digits);
end;

function Convert_ToHex3(__Self: pointer; value: Byte; Digits: Integer): String;
begin
  result := IntToHex(value, Digits);
end;

function Convert_ToHex5(__Self: pointer; value: Smallint; Digits: Integer): String;
begin
  result := IntToHex(value, Digits);
end;

function Convert_ToHex7(__Self: pointer; value: Word; Digits: Integer): String;
begin
  result := IntToHex(value, Digits);
end;

function Convert_ToHex9(__Self: pointer; value, Digits: Integer): String;
begin
  result := IntToHex(value, Digits);
end;

function Convert_ToHex11(__Self: pointer; value: Cardinal; Digits: Integer): String;
begin
  result := IntToHex(value, Digits);
end;

function Convert_ToHex13(__Self: pointer; value: Int64; Digits: Integer): String;
begin
  result := IntToHex(value, Digits);
end;

function Convert_ToHex15(__Self: pointer; value: TbtU64; Digits: Integer): String;
begin
  result := IntToHex(Int64(value), Digits);
end;

function Convert_TryToInt8(__Self: pointer; value: String; var res: Shortint): Boolean;
var d: integer;
begin
  result := TryStrToInt(value, d);
  res    := d;
end;

function Convert_TryToInt16(__Self: pointer; value: String; var res: Smallint): Boolean;
var d: integer;
begin
  result := TryStrToInt(value, d);
  res    := d;
end;

function Convert_TryToInt32(__Self: pointer; value: String; var res: Integer): Boolean;
var d: integer;
begin
  result := TryStrToInt(value, d);
  res    := d;
end;

function Convert_TryToInt64(__Self: pointer; value: String; var res: Int64): Boolean;
var d: int64;
begin
  result := TryStrToInt64(value, d);
  res    := d;
end;

function Convert_TryToUInt8(__Self: pointer; value: String; var res: Byte): Boolean;
var d: integer;
begin
  result := TryStrToInt(value, d);
  res    := d;
end;

function Convert_TryToUInt16(__Self: pointer; value: String; var res: Word): Boolean;
var d: integer;
begin
  result := TryStrToInt(value, d);
  res    := d;
end;

function Convert_TryToUInt32(__Self: pointer; value: String; var res: Cardinal): Boolean;
var d: integer;
begin
  result := TryStrToInt(value, d);
  res    := d;
end;

function Convert_TryToUInt64(__Self: pointer; value: String; var res: TbtU64): Boolean;
var d: Int64;
begin
  result := TryStrToInt64(value, d);
  res    := d;
end;

function Convert_TryToSingle(__Self: pointer; value: String; var res: Single): Boolean;
begin
  result := TryStrToFloat(value, res);
end;

function Convert_TryToDouble(__Self: pointer; value: String; var res: Double): Boolean;
begin
  result := TryStrToFloat(value, res);
end;

function Convert_TryToBoolean(__Self: pointer; value: String; var res: Boolean): Boolean;
begin
  result := TryStrToBool(value, res);
end;

{$HINTS OFF}

function Convert_IsInt8(__Self: pointer; value: String): Boolean;
var i: Shortint;
    c: integer;
begin
  Val(value, i, c);
  result := c = 0;
end;

function Convert_IsInt16(__Self: pointer; value: String): Boolean;
var i: Smallint;
    c: integer;
begin
  Val(value, i, c);
  result := c = 0;
end;

function Convert_IsInt32(__Self: pointer; value: String): Boolean;
var i: Integer;
    c: integer;
begin
  Val(value, i, c);
  result := c = 0;
end;

function Convert_IsInt64(__Self: pointer; value: String): Boolean;
var i: Int64;
    c: integer;
begin
  Val(value, i, c);
  result := c = 0;
end;

function Convert_IsUInt8(__Self: pointer; value: String): Boolean;
var i: Byte;
    c: integer;
begin
  Val(value, i, c);
  result := c = 0;
end;

function Convert_IsUInt16(__Self: pointer; value: String): Boolean;
var i: Word;
    c: integer;
begin
  Val(value, i, c);
  result := c = 0;
end;

function Convert_IsUInt32(__Self: pointer; value: String): Boolean;
var i: Cardinal;
    c: integer;
begin
  Val(value, i, c);
  result := c = 0;
end;

function Convert_IsUInt64(__Self: pointer; value: String): Boolean;
var i: TbtU64;
    c: integer;
begin
  Val(value, i, c);
  result := c = 0;
end;

function Convert_IsSingle(__Self: pointer; value: String): Boolean;
var i: single;
    c: integer;
begin
  Val(value, i, c);
  result := c = 0;
  if not result then
  begin
    result := SysUtils.TryStrToFloat(value, i);
  end;
end;

function Convert_IsDouble(__Self: pointer; value: String): Boolean;
var i: double;
    c: integer;
begin
  Val(value, i, c);
  result := c = 0;
  if not result then
  begin
    result := SysUtils.TryStrToFloat(value, i);
  end;
end;

function Convert_IsBoolean(__Self: pointer; value: String): Boolean;
var b: boolean;
begin
  result := TryStrToBool(value, b);
end;

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
    Target.Method['Convert.ToInt8[8]', C_UnitName] := @Convert_ToInt88;
    Target.Method['Convert.ToInt8[9]', C_UnitName] := @Convert_ToInt89;
    Target.Method['Convert.ToInt16[8]', C_UnitName] := @Convert_ToInt168;
    Target.Method['Convert.ToInt16[9]', C_UnitName] := @Convert_ToInt169;
    Target.Method['Convert.ToInt32[8]', C_UnitName] := @Convert_ToInt328;
    Target.Method['Convert.ToInt32[9]', C_UnitName] := @Convert_ToInt329;
    Target.Method['Convert.ToInt64[8]', C_UnitName] := @Convert_ToInt648;
    Target.Method['Convert.ToInt64[9]', C_UnitName] := @Convert_ToInt649;
    Target.Method['Convert.ToUInt8[8]', C_UnitName] := @Convert_ToUInt88;
    Target.Method['Convert.ToUInt8[9]', C_UnitName] := @Convert_ToUInt89;
    Target.Method['Convert.ToUInt16[8]', C_UnitName] := @Convert_ToUInt168;
    Target.Method['Convert.ToUInt16[9]', C_UnitName] := @Convert_ToUInt169;
    Target.Method['Convert.ToUInt32[8]', C_UnitName] := @Convert_ToUInt328;
    Target.Method['Convert.ToUInt32[9]', C_UnitName] := @Convert_ToUInt329;
    Target.Method['Convert.ToUInt64[8]', C_UnitName] := @Convert_ToUInt648;
    Target.Method['Convert.ToUInt64[9]', C_UnitName] := @Convert_ToUInt649;
    Target.Method['Convert.ToSingle[8]', C_UnitName] := @Convert_ToSingle8;
    Target.Method['Convert.ToSingle[9]', C_UnitName] := @Convert_ToSingle9;
    Target.Method['Convert.ToDouble[8]', C_UnitName] := @Convert_ToDouble8;
    Target.Method['Convert.ToDouble[9]', C_UnitName] := @Convert_ToDouble9;
    Target.Method['Convert.ToBoolean[8]', C_UnitName] := @Convert_ToBoolean8;
    Target.Method['Convert.ToBoolean[9]', C_UnitName] := @Convert_ToBoolean9;
    Target.Method['Convert.ToString[0]', C_UnitName] := @Convert_ToString;
    Target.Method['Convert.ToString[3]', C_UnitName] := @Convert_ToString3;
    Target.Method['Convert.ToString[6]', C_UnitName] := @Convert_ToString6;
    Target.Method['Convert.ToString[9]', C_UnitName] := @Convert_ToString9;
    Target.Method['Convert.ToString[12]', C_UnitName] := @Convert_ToString12;
    Target.Method['Convert.ToString[15]', C_UnitName] := @Convert_ToString15;
    Target.Method['Convert.ToString[18]', C_UnitName] := @Convert_ToString18;
    Target.Method['Convert.ToString[21]', C_UnitName] := @Convert_ToString21;
    Target.Method['Convert.ToString[24]', C_UnitName] := @Convert_ToString24;
    Target.Method['Convert.ToString[25]', C_UnitName] := @Convert_ToString25;
    Target.Method['Convert.ToString[26]', C_UnitName] := @Convert_ToString26;
    Target.Method['Convert.ToString[27]', C_UnitName] := @Convert_ToString27;
    Target.Method['Convert.ToString[28]', C_UnitName] := @Convert_ToString28;
    Target.Method['Convert.ToString[29]', C_UnitName] := @Convert_ToString29;
    Target.Method['Convert.ToString[30]', C_UnitName] := @Convert_ToString30;
    Target.Method['Convert.ToString[31]', C_UnitName] := @Convert_ToString31;
    Target.Method['Convert.ToString[32]', C_UnitName] := @Convert_ToString32;
    Target.Method['Convert.ToString[33]', C_UnitName] := @Convert_ToString33;
    Target.Method['Convert.ToHex[1]', C_UnitName] := @Convert_ToHex1;
    Target.Method['Convert.ToHex[3]', C_UnitName] := @Convert_ToHex3;
    Target.Method['Convert.ToHex[5]', C_UnitName] := @Convert_ToHex5;
    Target.Method['Convert.ToHex[7]', C_UnitName] := @Convert_ToHex7;
    Target.Method['Convert.ToHex[9]', C_UnitName] := @Convert_ToHex9;
    Target.Method['Convert.ToHex[11]', C_UnitName] := @Convert_ToHex11;
    Target.Method['Convert.ToHex[13]', C_UnitName] := @Convert_ToHex13;
    Target.Method['Convert.ToHex[15]', C_UnitName] := @Convert_ToHex15;
    Target.Method['Convert.TryToInt8[0]', C_UnitName] := @Convert_TryToInt8;
    Target.Method['Convert.TryToInt16[0]', C_UnitName] := @Convert_TryToInt16;
    Target.Method['Convert.TryToInt32[0]', C_UnitName] := @Convert_TryToInt32;
    Target.Method['Convert.TryToInt64[0]', C_UnitName] := @Convert_TryToInt64;
    Target.Method['Convert.TryToUInt8[0]', C_UnitName] := @Convert_TryToUInt8;
    Target.Method['Convert.TryToUInt16[0]', C_UnitName] := @Convert_TryToUInt16;
    Target.Method['Convert.TryToUInt32[0]', C_UnitName] := @Convert_TryToUInt32;
    Target.Method['Convert.TryToUInt64[0]', C_UnitName] := @Convert_TryToUInt64;
    Target.Method['Convert.TryToSingle[0]', C_UnitName] := @Convert_TryToSingle;
    Target.Method['Convert.TryToDouble[0]', C_UnitName] := @Convert_TryToDouble;
    Target.Method['Convert.TryToBoolean[0]', C_UnitName] := @Convert_TryToBoolean;
    Target.Method['Convert.IsInt8[0]', C_UnitName] := @Convert_IsInt8;
    Target.Method['Convert.IsInt16[0]', C_UnitName] := @Convert_IsInt16;
    Target.Method['Convert.IsInt32[0]', C_UnitName] := @Convert_IsInt32;
    Target.Method['Convert.IsInt64[0]', C_UnitName] := @Convert_IsInt64;
    Target.Method['Convert.IsUInt8[0]', C_UnitName] := @Convert_IsUInt8;
    Target.Method['Convert.IsUInt16[0]', C_UnitName] := @Convert_IsUInt16;
    Target.Method['Convert.IsUInt32[0]', C_UnitName] := @Convert_IsUInt32;
    Target.Method['Convert.IsUInt64[0]', C_UnitName] := @Convert_IsUInt64;
    Target.Method['Convert.IsSingle[0]', C_UnitName] := @Convert_IsSingle;
    Target.Method['Convert.IsDouble[0]', C_UnitName] := @Convert_IsDouble;
    Target.Method['Convert.IsBoolean[0]', C_UnitName] := @Convert_IsBoolean;
  end
end;

procedure RegisterUnit();
var p: TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;                   
  p.Priority          := 11;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

initialization
  RegisterUnit();

end.
