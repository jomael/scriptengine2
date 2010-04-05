unit uSE2Consts;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils;

type
  TSE2TokenType  = (sesNone, sesUnknown, sesIdentifier,
                    // Aritmetic operators
                    sesPlus, sesMinus, sesStar, sesSlash, sesDiv, sesMod,
                    // Comparison
                    sesEqual, sesSmaller, sesSmallerEqual, sesBiggerEqual, sesBigger, sesUnEqual,
                    sesIs,
                    // Assignment
                    sesBecomes, sesDoublePoint, sesDot, sesSemiColon, sesColon,
                    // Operators
                    sesAnd, sesOr, sesXor, sesNot, sesShr, sesShl, sesExit, sesBreak, sesContinue,
                    sesAt,
                    // Unit Positions
                    sesProgram, sesUnit, sesInterface, sesImplementation, sesInitialization, sesFinalization,
                    sesType, sesConst, sesVar, sesOut, sesSet, sesRecord, sesArray,
                    // Expressions
                    sesUses, sesBegin, sesEnd, sesIf, sesThen, sesElse, sesFor, sesTo, sesDownTo, sesDo, sesRepeat, sesUntil, sesWhile, sesCase, sesOf,
                    // Expression Helpers
                    sesOpenRound, sesCloseRound, sesOpenBracket, sesCloseBracket,
                    // Special expressions
                    sesTry, sesFinally, sesExcept, sesOn, sesDeprecated,
                    // Class Definitions
                    sesClass, sesPrivate, sesProtected, sesPublic, sesProperty, sesVirtual, sesAbstract, sesOverride, sesOverload,
                    sesInherited, sesReintroduce, sesPartial, sesHelper,
                    // Method definitions
                    sesProcedure, sesFunction, sesConstructor, sesDestructor, sesForward, sesObject,
                    // External Methods
                    sesExternal, sesCdecl, sesPascal, sesRegister, sesSafecall, sesStdCall, sesExport,
                    // Direct Value Types
                    sesString, sesInteger, sesFloat, sesNil,

                    // Special Tokens
                    sesImplicitCast, sesExplicitCast,

                    // Limit
                    sesLastToken
                    );

  TSE2TokenTypes = set of TSE2TokenType;

  TSE2CallType   = (callRegister, callStdcall, callCdecl, callPascal, callSafecall);

type
  // Custom Exceptions
  ESE2InvalidDataStream   = class(Exception);
  ESE2ParserError         = class(EAbort);
  ESE2InternalParserError = class(ESE2ParserError);
  ESE2NullReferenceError  = class(Exception);
  ESE2RunTimeError        = class(Exception);
  ESE2RunTimeCallError    = class(Exception);
  ESE2CallParameterError  = class(ESE2RunTimeCallError);
  ESE2PackageError        = class(Exception);
  ESE2PackageIncompatible = class(ESE2PackageError);

const
  TSE2TokenString : array[TSE2TokenType] of string = (
                    '', '', '',
                    // Aritmetic operators
                    '+', '-', '*', '/', 'div', 'mod',
                    // Comparison
                    '=', '<', '<=', '>=', '>', '<>',
                    'is',
                    // Assignment
                    ':=', ':', '.', ';', ',',
                    // Operators
                    'and', 'or', 'xor', 'not', 'shr', 'shl', 'exit', 'break', 'continue',
                    '@',
                    // Unit Positions
                    'program', 'unit', 'interface', 'implementation', 'initialization', 'finalization', 'type',
                    'const', 'var', 'out', 'set', 'record', 'array',
                    // Expressions
                    'uses', 'begin', 'end', 'if', 'then', 'else', 'for', 'to', 'downto', 'do', 'repeat', 'until', 'while', 'case', 'of',
                    // Expression Helpers
                    '(', ')', '[', ']',
                    // Special expressions
                    'try', 'finally', 'except', 'on', 'deprecated',
                    // Class Definitions
                    'class', 'private', 'protected', 'public', 'property', 'virtual', 'abstract', 'override', 'overload',
                    'inherited', 'reintroduce', 'partial', 'helper',
                    // Method definitions
                    'procedure', 'function', 'constructor', 'destructor', 'forward', 'object',
                    // External Methods
                    'external', 'cdecl', 'pascal', 'register', 'safecall', 'stdcall', 'export',
                    // Direct Value Types
                    '', '', '', 'nil',

                    // Special Tokens
                    '', '',

                    // Limit
                    '');

const
  C_SE2SystemUnitName           = 'System';
  C_SE2TObjectName              = 'TObject';
  C_SE2TExternalObjectName      = 'TExternalObject';
  C_SE2Boolean                  = 'boolean';
  C_SE2Int64                    = 'int64';   
  C_SE2String                   = 'string';
  C_SE2Double                   = 'double';   
  C_SE2Pointer                  = 'pointer';
  C_SE2PEHeaderStr              : AnsiString = 'SEII_PE';

  C_SE2MainMethod               = '!MAIN';
  C_SE2InitializationMethod     = '!INITIALIZATION';
  C_SE2FinalizationMethod       = '!FINALIZATION';

type
  TSE2StringEncoding = (encodingAnsi, encodingUnicode);

const
  CSE2SaveStringEncoding = encodingUnicode;

type
  TSE2StreamHelper = class(TObject)
  public
    class procedure ReadString(Stream: TStream; out s: string); overload;
    class function  ReadString(Stream: TStream): string; overload;
    class procedure WriteString(Stream: TStream; const s: string); overload;

    class procedure ReadString(Stream: TStream; Encoding: TSE2StringEncoding; out s: string); overload;
    class function  ReadString(Stream: TStream; Encoding: TSE2StringEncoding): string; overload;
    class procedure WriteString(Stream: TStream; Encoding: TSE2StringEncoding; const s: string); overload;

    class procedure ReadAnsiString(Stream: TStream; out s: AnsiString); overload;
    class function  ReadAnsiString(Stream: TStream): AnsiString; overload;
    class procedure WriteAnsiString(Stream: TStream; const s: AnsiString);
  end;

  { YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT AND/OR THE FOLLOWING CLASS }
  TSE2ScriptEngineInfo = class(TObject)
  public
    class function  Name      : string;
    class function  Author    : string;
    class function  Version   : string;
    class function  BuildDate : TDateTime;
    class function  Copyright : string;
  end;

  { YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT AND/OR THE FOLLOWING CLASS }
  TSE2ScriptEngineAdditionalInfo = class(TObject)
  public
    class function  Authors   : string;
    class function  Revision  : string;
    class function  BuildDate : TDateTime;
    class function  Copyright : string;
    class function  ChangeLog : string;
  end;

  TSE2ScriptEngineVersion = packed record
    Major, Minor, Patch, Build : word;
  end;

const
  CSE2Version : TSE2ScriptEngineVersion = (Major: 0; Minor: 4; Patch: 8; Build: 0);
  
implementation

{ TSE2StreamHelper }

class procedure TSE2StreamHelper.ReadString(Stream: TStream;
  out s: string);
var len: cardinal;
begin
  s := '';
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(len, SizeOf(len)) < SizeOf(len) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  if len > 0 then
  begin
    SetLength(s, len);
    Stream.Read(s[1], len * SizeOf(char));
  end;
end;

class procedure TSE2StreamHelper.ReadAnsiString(Stream: TStream;
  out s: AnsiString);
var len: cardinal;
begin
  s := '';
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(len, SizeOf(len)) < SizeOf(len) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  if len > 0 then
  begin
    SetLength(s, len);
    Stream.Read(s[1], len);
  end;
end;

class function TSE2StreamHelper.ReadAnsiString(
  Stream: TStream): AnsiString;
begin
  ReadAnsiString(Stream, result);
end;

class procedure TSE2StreamHelper.ReadString(Stream: TStream;
  Encoding: TSE2StringEncoding; out s: string);
var strAnsi : AnsiString;
{$IFDEF Unicode}
    strUni  : string;
{$ELSE}
    strUni  : WideString;
{$ENDIF}
    len     : cardinal;
begin
  s := '';
  if Stream.Read(len, SizeOf(len)) < SizeOf(len) then
     exit;

  if len > 0 then
  begin
    case Encoding of
    encodingAnsi :
        begin
          SetLength(strAnsi, len);
          Stream.Read(strAnsi[1], len);
          s := string(strAnsi);
        end;
    encodingUnicode :
        begin
          SetLength(strUni, len);
          Stream.Read(strUni[1], len * SizeOf({$IFDEF Unicode} Char {$ELSE} WideChar {$ENDIF}));
          s := string(strUni);
        end;
    else
        raise ESE2InvalidDataStream.Create('Unsupported encoding selected');
    end;
  end;
end;

class function TSE2StreamHelper.ReadString(Stream: TStream;
  Encoding: TSE2StringEncoding): string;
begin
  TSE2StreamHelper.ReadString(Stream, Encoding, result);
end;

class function TSE2StreamHelper.ReadString(Stream: TStream): string;
begin
  ReadString(Stream, result);
end;

class procedure TSE2StreamHelper.WriteAnsiString(Stream: TStream;
  const s: AnsiString);
var len: cardinal;
begin
  len := length(s);
  Stream.Write(len, SizeOf(len));
  if len > 0 then
     Stream.Write(s[1], len);
end;

class procedure TSE2StreamHelper.WriteString(Stream: TStream;
  Encoding: TSE2StringEncoding; const s: string);
var strAnsi : AnsiString;
{$IFDEF Unicode}
    strUni  : string;
{$ELSE}
    strUni  : WideString;
{$ENDIF}
    len     : cardinal;
begin
  len := length(s);    
  Stream.Write(len, SizeOf(len));
  if len > 0 then
  begin
    case Encoding of
    encodingAnsi :
        begin
          strAnsi := AnsiString(s);
          Stream.Write(strAnsi[1], len);
        end;
    encodingUnicode :
        begin
          strUni  := s;
          Stream.Write(strUni[1], len * SizeOf( {$IFDEF Unicode} Char {$ELSE} WideChar {$ENDIF} ));
        end;
    else
        raise ESE2InvalidDataStream.Create('Unsupported encoding selected');
    end;
  end;
end;

class procedure TSE2StreamHelper.WriteString(Stream: TStream;
  const s: string);
var len: cardinal;
begin
  len := length(s);
  Stream.Write(len, SizeOf(len));
  if len > 0 then
     Stream.Write(s[1], len * SizeOf(char));
end;

{ TSE2ScriptEngineInfo }

{ YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT AND/OR THE FOLLOWING FUNCTION }
class function TSE2ScriptEngineInfo.Author: string;
begin
  result := 'David Huettig';
end;

{ YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT AND/OR THE FOLLOWING FUNCTION }
class function TSE2ScriptEngineInfo.BuildDate: TDateTime;
begin
  result := EncodeDate(2010, 04, 05);
end;

{ YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT AND/OR THE FOLLOWING FUNCTION }
class function TSE2ScriptEngineInfo.Copyright: string;
begin
  result := 'Copyright (c) 2009-2010 David Huettig';
end;

{ YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT AND/OR THE FOLLOWING FUNCTION }
class function TSE2ScriptEngineInfo.Name: string;
begin
  result := 'ScriptEngineII';
end;

{ YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT AND/OR THE FOLLOWING FUNCTION }
class function TSE2ScriptEngineInfo.Version: string;
begin
  result := Format('%d.%d.%d.%d', [CSE2Version.Major, CSE2Version.Minor, CSE2Version.Patch, CSE2Version.Build]);
end;

{ TSE2ScriptEngineAdditionalInfo }

{ YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT

  If you have modified(*) any file of the script engine, you
  can add your name to the following result string. To seperate
  several names, please use a comma

  e.g. Hans Mustermann, Petra Musterfrau, ..., YOUR NAME

  (*) modification means:
       - remove an existing bug
       - add a new feature
}
class function TSE2ScriptEngineAdditionalInfo.Authors: string;
begin
  result := '';
end;

{ YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT

  If you have modified(*) any file of the script engine, please
  change the date below to the date of your last modification

  e.g. last modified april the 21st 2003 ->
       result := EncodeDate(2003, 4, 21);

  (*) modification means:
       - remove an existing bug
       - add a new feature
}
class function TSE2ScriptEngineAdditionalInfo.BuildDate: TDateTime;
begin
  result := TSE2ScriptEngineInfo.BuildDate;
  //result := EncodeDate(year, month, day);
end;

{ YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT

  If you have modified(*) any file of the script engine, please
  add your changes to the change log.

  e.g. you have found a bug and removed it -> add the following
       line to the function and fill out the data between the brackets
       result := result + #13#10 + '[date YY/MM/DD] [your name (optional)] [file name (optional)]: [description of the changes you have made]';


  (*) modification means:
       - remove an existing bug
       - add a new feature
}
class function TSE2ScriptEngineAdditionalInfo.ChangeLog: string;
begin
  // Please use the following format:
  // [date] [name] [file name]: description
  //   date: the current date in the format "YY/MM/DD"
  //   name: your name or your sign (optional)
  //   file name: the file you modified (optional)
  //   description: a short but detailed description about what you did
  result := '';
end;

{ YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT

  If you have modified(*) any file of the script engine, you can add
  a copyright string if you want. Do not remove other copyright notes

  (*) modification means:
       - remove an existing bug
       - add a new feature
}
class function TSE2ScriptEngineAdditionalInfo.Copyright: string;
begin
  result := '';
end;
      

{ YOU ARE NOT ALLOWED TO MODIFY AND/OR TO REMOVE THIS COMMENT

  If you have modified(*) any file of the script engine, please
  increase the revision number by one ( + 1 )

  (*) modification means:
       - remove an existing bug
       - add a new feature
}
class function TSE2ScriptEngineAdditionalInfo.Revision: string;
begin
  result := '1';
end;

end.
