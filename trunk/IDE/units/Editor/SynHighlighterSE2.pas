{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPas.pas, released 2000-04-17.
The Original Code is based on the mwPasSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterPas.pas,v 1.27.2.10 2009/02/23 15:43:50 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Pascal/Delphi syntax highlighter for SynEdit)
@author(Martin Waldenburg)
@created(1998, converted to SynEdit 2000-04-07)
@lastmod(2004-03-19)
The SynHighlighterPas unit provides SynEdit with a Object Pascal syntax highlighter.
Two extra properties included (DelphiVersion, PackageSource):
  DelphiVersion - Allows you to enable/disable the highlighting of various
                  language enhancements added in the different Delphi versions.
  PackageSource - Allows you to enable/disable the highlighting of package keywords
}

{$IFDEF Unicode}

{$IFNDEF QSYNHIGHLIGHTERPAS}
unit SynHighlighterSE2;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynUnicode,
{$ELSE}
  Windows,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkFloat, tkHex, tkDirec, tkChar);

  TRangeState = (rsANil, rsAnsi, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsExports, rsDirective, rsDirectiveAsm, rsUnKnown);

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;


  TDelphiVersion = (dvDelphi1, dvDelphi2, dvDelphi3, dvDelphi4, dvDelphi5,
    dvDelphi6, dvDelphi7, dvDelphi8, dvDelphi2005);

const
  LastDelphiVersion = dvDelphi2005;
  BDSVersionPrefix = 'BDS';

type
  TSynSE2Syn = class(TSynCustomHighlighter)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    fIdentFuncTable: array[0..388] of TIdentFuncTableFunc;
    fTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fCharAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fFloatAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fDelphiVersion: TDelphiVersion;
    fPackageSource: Boolean;
    function AltFunc(Index: Integer): TtkTokenKind;
    function KeyWordFunc(Index: Integer): TtkTokenKind;
    function FuncAsm(Index: Integer): TtkTokenKind;
    function FuncAutomated(Index: Integer): TtkTokenKind;
    function FuncCdecl(Index: Integer): TtkTokenKind;
    function FuncContains(Index: Integer): TtkTokenKind;
    function FuncDeprecated(Index: Integer): TtkTokenKind;
    function FuncDispid(Index: Integer): TtkTokenKind;
    function FuncDispinterface(Index: Integer): TtkTokenKind;
    function FuncEnd(Index: Integer): TtkTokenKind;
    function FuncExports(Index: Integer): TtkTokenKind;
    function FuncFinal(Index: Integer): TtkTokenKind;
    function FuncFinalization(Index: Integer): TtkTokenKind;
    function FuncHelper(Index: Integer): TtkTokenKind;
    function FuncImplements(Index: Integer): TtkTokenKind;
    function FuncIndex(Index: Integer): TtkTokenKind;
    function FuncName(Index: Integer): TtkTokenKind;
    function FuncNodefault(Index: Integer): TtkTokenKind;
    function FuncOperator(Index: Integer): TtkTokenKind;
    function FuncOverload(Index: Integer): TtkTokenKind;
    function FuncPackage(Index: Integer): TtkTokenKind;
    function FuncPlatform(Index: Integer): TtkTokenKind;
    function FuncProperty(Index: Integer): TtkTokenKind;
    function FuncRead(Index: Integer): TtkTokenKind;
    function FuncReadonly(Index: Integer): TtkTokenKind;
    function FuncReintroduce(Index: Integer): TtkTokenKind;
    function FuncRequires(Index: Integer): TtkTokenKind;
    function FuncResourcestring(Index: Integer): TtkTokenKind;
    function FuncSafecall(Index: Integer): TtkTokenKind;
    function FuncSealed(Index: Integer): TtkTokenKind;
    function FuncStdcall(Index: Integer): TtkTokenKind;
    function FuncStored(Index: Integer): TtkTokenKind;
    function FuncStringresource(Index: Integer): TtkTokenKind;
    function FuncThreadvar(Index: Integer): TtkTokenKind;
    function FuncWrite(Index: Integer): TtkTokenKind;
    function FuncWriteonly(Index: Integer): TtkTokenKind;
    function HashKey(Str: PWideChar): Cardinal;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure InitIdent;








































    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonOrGreaterProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure RoundOpenProc;
    procedure SemicolonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure SetDelphiVersion(const Value: TDelphiVersion);
    procedure SetPackageSource(const Value: Boolean);
  protected
    function GetSampleSource: UnicodeString; override;

    function IsFilterStored: Boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;

    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: Integer; override;

    procedure Next; override;
    procedure ResetRange; override;

    procedure SetRange(Value: Pointer); override;
    function UseUserSettings(VersionIndex: Integer): Boolean; override;
    procedure EnumUserSettings(DelphiVersions: TStrings); override;

  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property FloatAttri: TSynHighlighterAttributes read fFloatAttri
      write fFloatAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri
      write fHexAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property CharAttri: TSynHighlighterAttributes read fCharAttri
      write fCharAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property DelphiVersion: TDelphiVersion read fDelphiVersion write SetDelphiVersion
      default LastDelphiVersion;
    property PackageSource: Boolean read fPackageSource write SetPackageSource default True;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  // if the language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..110] of UnicodeString = (
    'absolute', 'abstract', 'and', 'array', 'as', 'asm', 'assembler',
    'automated', 'begin', 'case', 'cdecl', 'class', 'const', 'constructor',
    'contains', 'default', 'deprecated', 'destructor', 'dispid',
    'dispinterface', 'div', 'do', 'downto', 'dynamic', 'else', 'end', 'except',
    'export', 'exports', 'external', 'far', 'file', 'final', 'finalization',
    'finally', 'for', 'forward', 'function', 'goto', 'helper', 'if',
    'implementation', 'implements', 'in', 'index', 'inherited',
    'initialization', 'inline', 'interface', 'is', 'label', 'library',
    'message', 'mod', 'name', 'near', 'nil', 'nodefault', 'not', 'object', 'of',
    'on', 'operator', 'or', 'out', 'overload', 'override', 'package', 'packed',
    'pascal', 'platform', 'private', 'procedure', 'program', 'property',
    'protected', 'public', 'published', 'raise', 'read', 'readonly', 'record',
    'register', 'reintroduce', 'repeat', 'requires', 'resourcestring',
    'safecall', 'sealed', 'set', 'shl', 'shr', 'stdcall', 'stored', 'string',
    'stringresource', 'then', 'threadvar', 'to', 'try', 'type', 'unit', 'until',
    'uses', 'var', 'virtual', 'while', 'with', 'write', 'writeonly', 'xor'
  );

  KeyIndices: array[0..388] of Integer = (
    -1, -1, -1, 105, -1, 51, -1, 108, -1, -1, -1, -1, -1, 75, -1, -1, 46, -1,
    -1, 103, -1, -1, -1, -1, 55, -1, -1, -1, -1, 76, -1, -1, 96, 14, -1, 31, 3,
    102, -1, -1, -1, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, 78, -1, -1, 25, -1,
    -1, 56, 65, 95, -1, -1, -1, 34, -1, 85, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 22, -1, -1, -1, -1, -1, -1, 80, -1, -1, -1, -1, 50, -1, -1, 109, 98, -1,
    86, -1, 13, -1, -1, -1, 107, -1, -1, 60, -1, 0, 64, -1, -1, -1, -1, 8, 10,
    -1, -1, -1, 67, -1, -1, -1, 74, -1, 17, -1, 73, 69, -1, 68, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 16, -1, -1, 23, 39, -1, 35, 30, -1, -1, -1, 70, -1, 37,
    -1, -1, 89, 71, 84, 72, -1, 29, 40, -1, -1, -1, 32, -1, -1, -1, 94, -1, -1,
    87, -1, -1, -1, -1, -1, -1, 77, -1, -1, -1, -1, -1, -1, 11, 57, 41, 6, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 24, -1, -1, -1, -1, 97, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 44, 12, -1, -1, 101, -1, 58, -1, -1, -1, 99, -1, -1,
    -1, -1, 53, 20, -1, -1, -1, 36, -1, -1, 63, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 45, -1, -1, -1, -1, 27, -1, -1, -1, -1, -1, 59,
    -1, 110, -1, 15, -1, 52, -1, -1, -1, -1, 5, 48, -1, -1, -1, 81, -1, 28, -1,
    -1, -1, 2, -1, 1, -1, 106, -1, -1, -1, -1, 90, -1, 83, -1, -1, -1, -1, -1,
    79, -1, -1, 33, 62, -1, -1, -1, -1, -1, -1, 4, -1, -1, -1, -1, -1, -1, 88,
    61, 54, -1, 42, -1, -1, -1, 66, -1, -1, -1, 92, 100, -1, -1, -1, -1, -1, 18,
    -1, -1, 26, 47, 38, -1, -1, 93, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    9, -1, 91, -1, -1, -1, -1, -1, -1, 49, -1, 21, -1, -1, -1, -1, -1, -1, 43,
    -1, 82, -1, 19, 104, -1, -1, -1, -1, -1
  );

{$Q-}
function TSynSE2Syn.HashKey(Str: PWideChar): Cardinal;












































































begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 812 + Ord(Str^) * 76;
    inc(Str);





  end;
  Result := Result mod 389;
  fStringLen := Str - fToIdent;






















end;
{$Q+}

function TSynSE2Syn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])

  else
    Result := tkIdentifier;
end;

procedure TSynSE2Syn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[275] := FuncAsm;
  fIdentFuncTable[41] := FuncAutomated;
  fIdentFuncTable[112] := FuncCdecl;
  fIdentFuncTable[33] := FuncContains;
  fIdentFuncTable[137] := FuncDeprecated;
  fIdentFuncTable[340] := FuncDispid;
  fIdentFuncTable[382] := FuncDispinterface;
  fIdentFuncTable[54] := FuncEnd;
  fIdentFuncTable[282] := FuncExports;
  fIdentFuncTable[163] := FuncFinal;
  fIdentFuncTable[306] := FuncFinalization;
  fIdentFuncTable[141] := FuncHelper;
  fIdentFuncTable[325] := FuncImplements;
  fIdentFuncTable[214] := FuncIndex;
  fIdentFuncTable[323] := FuncName;
  fIdentFuncTable[185] := FuncNodefault;
  fIdentFuncTable[307] := FuncOperator;
  fIdentFuncTable[58] := FuncOverload;
  fIdentFuncTable[116] := FuncPackage;
  fIdentFuncTable[148] := FuncPlatform;
  fIdentFuncTable[120] := FuncProperty;
  fIdentFuncTable[303] := FuncRead;
  fIdentFuncTable[83] := FuncReadonly;
  fIdentFuncTable[297] := FuncReintroduce;
  fIdentFuncTable[65] := FuncRequires;
  fIdentFuncTable[94] := FuncResourcestring;
  fIdentFuncTable[170] := FuncSafecall;
  fIdentFuncTable[321] := FuncSealed;
  fIdentFuncTable[333] := FuncStdcall;
  fIdentFuncTable[348] := FuncStored;
  fIdentFuncTable[59] := FuncStringresource;
  fIdentFuncTable[204] := FuncThreadvar;
  fIdentFuncTable[7] := FuncWrite;
  fIdentFuncTable[91] := FuncWriteonly;

  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if @fIdentFuncTable[i] = nil then
      fIdentFuncTable[i] := KeyWordFunc;



































































































































































































end;

function TSynSE2Syn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier











end;

function TSynSE2Syn.KeyWordFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then


    Result := tkKey


  else
    Result := tkIdentifier
end;

function TSynSE2Syn.FuncAsm(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then






  begin
    Result := tkKey;
    fRange := rsAsm;
    fAsmStart := True;
  end
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncAutomated(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then




    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncCdecl(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and IsCurrentToken(KeyWords[Index]) then


    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncContains(Index: Integer): TtkTokenKind;
begin
  if PackageSource and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncDeprecated(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi6) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncDispid(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncDispinterface(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncEnd(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
  begin
    Result := tkKey;
    fRange := rsUnknown;
  end









  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncExports(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then








  begin
    Result := tkKey;
    fRange := rsExports;
  end
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncFinal(Index: Integer): TtkTokenKind;
begin
 if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncFinalization(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey


  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncHelper(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then


    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncImplements(Index: Integer): TtkTokenKind;
begin
  if (fRange = rsProperty) and (DelphiVersion >= dvDelphi4) and IsCurrentToken(KeyWords[Index]) then


    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncIndex(Index: Integer): TtkTokenKind;
begin
  if (fRange in [rsProperty, rsExports]) and IsCurrentToken(KeyWords[Index]) then


    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncName(Index: Integer): TtkTokenKind;
begin
  if (fRange = rsExports) and IsCurrentToken(KeyWords[Index]) then


    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncNodefault(Index: Integer): TtkTokenKind;
begin
  if (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncOperator(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then


    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncOverload(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi4) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncPackage(Index: Integer): TtkTokenKind;
begin
  if PackageSource and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncPlatform(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi6) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey


  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncProperty(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then






  begin
    Result := tkKey;
    fRange := rsProperty;
  end
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncRead(Index: Integer): TtkTokenKind;
begin
  if (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncReadonly(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncReintroduce(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi4) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncRequires(Index: Integer): TtkTokenKind;
begin
  if PackageSource and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncResourcestring(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then











    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncSafecall(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncSealed(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncStdcall(Index: Integer): TtkTokenKind;


begin
  if (DelphiVersion >= dvDelphi2) and IsCurrentToken(KeyWords[Index]) then









    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncStored(Index: Integer): TtkTokenKind;

begin
  if (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncStringresource(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncThreadvar(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and IsCurrentToken(KeyWords[Index]) then


    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncWrite(Index: Integer): TtkTokenKind;
begin
  if (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.FuncWriteonly(Index: Integer): TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and (fRange = rsProperty) and IsCurrentToken(KeyWords[Index]) then
    Result := tkKey


  else
    Result := tkIdentifier;
end;

constructor TSynSE2Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  fDelphiVersion := LastDelphiVersion;
  fPackageSource := True;

  fAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler, SYNS_FriendlyAttrAssembler);
  AddAttribute(fAsmAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  fDirecAttri.Style:= [fsItalic];
  AddAttribute(fDirecAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(fNumberAttri);
  fFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  AddAttribute(fFloatAttri);
  fHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  AddAttribute(fHexAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(fStringAttri);
  fCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  AddAttribute(fCharAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterPascal;
end;

procedure TSynSE2Syn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '@' then inc(Run);
end;

procedure TSynSE2Syn.AsciiCharProc;

  function IsAsciiChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '$', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  fTokenID := tkChar;
  Inc(Run);
  while IsAsciiChar do
    Inc(Run);
end;

procedure TSynSE2Syn.BorProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else






















    begin
      if fRange in [rsDirective, rsDirectiveAsm] then
        fTokenID := tkDirec
      else
        fTokenID := tkComment;
      repeat
        if fLine[Run] = '}' then

        begin

          Inc(Run);
          if fRange in [rsBorAsm, rsDirectiveAsm] then
            fRange := rsAsm
          else
            fRange := rsUnKnown;
          break;
        end;




        Inc(Run);
      until IsLineEnd(Run);
    end;
  end;
end;

procedure TSynSE2Syn.BraceOpenProc;
begin
  if (fLine[Run + 1] = '$') then
  begin
    if fRange = rsAsm then
      fRange := rsDirectiveAsm
    else
      fRange := rsDirective;
  end

  else



  begin
    if fRange = rsAsm then
      fRange := rsBorAsm

    else
      fRange := rsBor;
  end;
  BorProc;
end;

procedure TSynSE2Syn.ColonOrGreaterProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);


end;

procedure TSynSE2Syn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end;

procedure TSynSE2Syn.IdentProc;
begin
  fTokenID := IdentKind(fLine + Run);
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    Inc(Run);

end;

procedure TSynSE2Syn.IntegerProc;

  function IsIntegerChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', 'A'..'F', 'a'..'f':
        Result := True;
      else
        Result := False;
    end;
  end;


begin

  inc(Run);
  fTokenID := tkHex;
  while IsIntegerChar do
    Inc(Run);
end;

procedure TSynSE2Syn.LFProc;

begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSE2Syn.LowerProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '=') or (fLine[Run] = '>') then
    Inc(Run);
end;

procedure TSynSE2Syn.NullProc;
begin
  fTokenID := tkNull;

  inc(Run);
end;

procedure TSynSE2Syn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
      '0'..'9', '.', 'e', 'E', '-', '+':
        Result := True;
      else
        Result := False;
    end;
  end;













begin
  Inc(Run);
  fTokenID := tkNumber;
  while IsNumberChar do
  begin
    case fLine[Run] of
      '.':
        if fLine[Run + 1] = '.' then
          Break
        else
          fTokenID := tkFloat;
      'e', 'E': fTokenID := tkFloat;
      '-', '+':
        begin
          if fTokenID <> tkFloat then // arithmetic
            Break;
          if (FLine[Run - 1] <> 'e') and (FLine[Run - 1] <> 'E') then
            Break; //float, but it ends here
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynSE2Syn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '.') or (fLine[Run - 1] = ')') then
    Inc(Run);
end;

procedure TSynSE2Syn.AnsiProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then begin
        Inc(Run, 2);
        if fRange = rsAnsiAsm then
          fRange := rsAsm
        else
          fRange := rsUnKnown;
        break;
      end;
      Inc(Run);
    until IsLineEnd(Run);
  end;
end;

procedure TSynSE2Syn.RoundOpenProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        Inc(Run);
        if fRange = rsAsm then
          fRange := rsAnsiAsm
        else
          fRange := rsAnsi;
        fTokenID := tkComment;
        if not IsLineEnd(Run) then
          AnsiProc;
      end;
    '.':
      begin
        inc(Run);
        fTokenID := tkSymbol;
      end;
  else
    fTokenID := tkSymbol;
  end;
end;

procedure TSynSE2Syn.SemicolonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fRange in [rsProperty, rsExports] then
    fRange := rsUnknown;
end;

procedure TSynSE2Syn.SlashProc;
begin
  Inc(Run);
  if (fLine[Run] = '/') and (fDelphiVersion > dvDelphi1) then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until IsLineEnd(Run);
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynSE2Syn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynSE2Syn.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not IsLineEnd(Run) do
  begin
    if fLine[Run] = #39 then begin
      Inc(Run);
      if fLine[Run] <> #39 then
        break;
    end;
    Inc(Run);
  end;
end;

procedure TSynSE2Syn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSE2Syn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSE2Syn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm, rsDirective, rsDirectiveAsm:
      BorProc;
    else
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        '#': AsciiCharProc;
        '$': IntegerProc;
        #39: StringProc;
        '0'..'9': NumberProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        '{': BraceOpenProc;
        '}', '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
          begin
            case fLine[Run] of
              '(': RoundOpenProc;
              '.': PointProc;
              ';': SemicolonProc;
              '/': SlashProc;
              ':', '>': ColonOrGreaterProc;
              '<': LowerProc;
              '@': AddressOpProc;
              else
                 SymbolProc;
            end;






          end;
        else
          UnknownProc;
      end;
  end;
  inherited;
end;

function TSynSE2Syn.GetDefaultAttribute(Index: Integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSE2Syn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynSE2Syn.GetTokenID: TtkTokenKind;
begin
  if not fAsmStart and (fRange = rsAsm)
    and not (fTokenId in [tkNull, tkComment, tkDirec, tkSpace])
  then
    Result := tkAsm
  else
    Result := fTokenId;
end;

function TSynSE2Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkDirec: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkFloat: Result := fFloatAttri;
    tkHex: Result := fHexAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkChar: Result := fCharAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSE2Syn.GetTokenKind: Integer;

begin
  Result := Ord(GetTokenID);

end;

function TSynSE2Syn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynSE2Syn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynSE2Syn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynSE2Syn.EnumUserSettings(DelphiVersions: TStrings);

  procedure LoadKeyVersions(const Key, Prefix: string);
  var
    Versions: TStringList;
    i: Integer;
  begin
    with TBetterRegistry.Create do
    begin
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKeyReadOnly(Key) then
        begin
          try
            Versions := TStringList.Create;
            try
              GetKeyNames(Versions);
              for i := 0 to Versions.Count - 1 do
                DelphiVersions.Add(Prefix + Versions[i]);
            finally
              FreeAndNil(Versions);
            end;
          finally
            CloseKey;
          end;

        end;
      finally
        Free;
      end;
    end;
  end;


begin
  { returns the user settings that exist in the registry }
{$IFNDEF SYN_CLX}
  // See UseUserSettings below where these strings are used
  LoadKeyVersions('\SOFTWARE\Borland\Delphi', '');
  LoadKeyVersions('\SOFTWARE\Borland\BDS', BDSVersionPrefix);
  LoadKeyVersions('\SOFTWARE\CodeGear\BDS', BDSVersionPrefix);
{$ENDIF}
end;

function TSynSE2Syn.UseUserSettings(VersionIndex: Integer): Boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   True : settings were read and used
//   False: problem reading settings or invalid version specified - old settings
//          were preserved

{$IFNDEF SYN_CLX}
  function ReadDelphiSettings(settingIndex: Integer): Boolean;

    function ReadDelphiSetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
    var
      Version: Currency;
      VersionStr: string;








      function ReadDelphi2Or3(settingTag: string; attri: TSynHighlighterAttributes; name: string): Boolean;
      var
        i: Integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                '\Software\Borland\Delphi\'+settingTag+'\Highlight',name,True);
      end; { ReadDelphi2Or3 }

      function ReadDelphi4OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Borland\Delphi\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi4OrMore }

      function ReadDelphi8To2007(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;


      begin


        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Borland\BDS\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi8OrMore }

      function ReadDelphi2009OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): Boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\CodeGear\BDS\'+settingTag+'\Editor\Highlight',key,False);
      end; { ReadDelphi2009OrMore }

    begin { ReadDelphiSetting }
      try
        if Pos('BDS', settingTag) = 1 then // BDS product
        begin
          VersionStr := Copy(settingTag, Length(BDSVersionPrefix) + 1, 999);
          Version := 0;
          if not TryStrToCurr(StringReplace(VersionStr, '.', DecimalSeparator, []), Version) then
          begin
            Result := False;
            Exit;
          end;
          if Version >= 6 then
            Result := ReadDelphi2009OrMore(VersionStr, attri, key)
          else
            Result := ReadDelphi8To2007(VersionStr, attri, key);
        end
        else begin // Borland Delphi 7 or earlier
          if (settingTag[1] = '2') or (settingTag[1] = '3')
            then Result := ReadDelphi2Or3(settingTag, attri, key)
            else Result := ReadDelphi4OrMore(settingTag, attri, key);
        end;
      except Result := False; end;
    end; { ReadDelphiSetting }

  var
    tmpAsmAttri, tmpCommentAttri, tmpIdentAttri, tmpKeyAttri, tmpNumberAttri,
    tmpSpaceAttri, tmpStringAttri, tmpSymbolAttri: TSynHighlighterAttributes;
    iVersions: TStringList;
    iVersionTag: string;
  begin { ReadDelphiSettings }
    {$IFDEF SYN_COMPILER_7_UP}
    {$IFNDEF SYN_COMPILER_9_UP}
    Result := False; // Silence the compiler warning
    {$ENDIF}
    {$ENDIF}
    iVersions := TStringList.Create;
    try
      EnumUserSettings(iVersions);
      if (settingIndex < 0) or (settingIndex >= iVersions.Count) then
      begin
        Result := False;
        Exit;
      end;
      iVersionTag := iVersions[settingIndex];
    finally
      iVersions.Free;
    end;
    tmpAsmAttri     := TSynHighlighterAttributes.Create('', '');
    tmpCommentAttri := TSynHighlighterAttributes.Create('', '');
    tmpIdentAttri   := TSynHighlighterAttributes.Create('', '');
    tmpKeyAttri     := TSynHighlighterAttributes.Create('', '');
    tmpNumberAttri  := TSynHighlighterAttributes.Create('', '');
    tmpSpaceAttri   := TSynHighlighterAttributes.Create('', '');
    tmpStringAttri  := TSynHighlighterAttributes.Create('', '');
    tmpSymbolAttri  := TSynHighlighterAttributes.Create('', '');

    Result := ReadDelphiSetting(iVersionTag, tmpAsmAttri,'Assembler') and
      ReadDelphiSetting(iVersionTag, tmpCommentAttri,'Comment') and
      ReadDelphiSetting(iVersionTag, tmpIdentAttri,'Identifier') and
      ReadDelphiSetting(iVersionTag, tmpKeyAttri,'Reserved word') and
      ReadDelphiSetting(iVersionTag, tmpNumberAttri,'Number') and
      ReadDelphiSetting(iVersionTag, tmpSpaceAttri,'Whitespace') and
      ReadDelphiSetting(iVersionTag, tmpStringAttri,'String') and
      ReadDelphiSetting(iVersionTag, tmpSymbolAttri,'Symbol');

    if Result then
    begin
      fAsmAttri.AssignColorAndStyle(tmpAsmAttri);
      fCharAttri.AssignColorAndStyle(tmpStringAttri); { Delphi lacks Char attribute }
      fCommentAttri.AssignColorAndStyle(tmpCommentAttri);
      fDirecAttri.AssignColorAndStyle(tmpCommentAttri); { Delphi lacks Directive attribute }
      fFloatAttri.AssignColorAndStyle(tmpNumberAttri); { Delphi lacks Float attribute }
      fHexAttri.AssignColorAndStyle(tmpNumberAttri); { Delphi lacks Hex attribute }
      fIdentifierAttri.AssignColorAndStyle(tmpIdentAttri);
      fKeyAttri.AssignColorAndStyle(tmpKeyAttri);
      fNumberAttri.AssignColorAndStyle(tmpNumberAttri);
      fSpaceAttri.AssignColorAndStyle(tmpSpaceAttri);
      fStringAttri.AssignColorAndStyle(tmpStringAttri);
      fSymbolAttri.AssignColorAndStyle(tmpSymbolAttri);
    end;
    tmpAsmAttri.Free;
    tmpCommentAttri.Free;
    tmpIdentAttri.Free;
    tmpKeyAttri.Free;
    tmpNumberAttri.Free;
    tmpSpaceAttri.Free;
    tmpStringAttri.Free;
    tmpSymbolAttri.Free;
  end;
{$ENDIF}

begin
{$IFNDEF SYN_CLX}
  Result := ReadDelphiSettings(VersionIndex);
{$ELSE}
  Result := False;
{$ENDIF}





end;

function TSynSE2Syn.GetSampleSource: UnicodeString;
begin
  Result := '{ Syntax highlighting }'#13#10 +
             'procedure TForm1.Button1Click(Sender: TObject);'#13#10 +
             'var'#13#10 +
             '  Number, I, X: Integer;'#13#10 +
             'begin'#13#10 +
             '  Number := 123456;'#13#10 +
             '  Caption := ''The Number is'' + #32 + IntToStr(Number);'#13#10 +
             '  for I := 0 to Number do'#13#10 +
             '  begin'#13#10 +
             '    Inc(X);'#13#10 +
             '    Dec(X);'#13#10 +
             '    X := X + 1.0;'#13#10 +
             '    X := X - $5E;'#13#10 +
             '  end;'#13#10 +
             '  {$R+}'#13#10 +
             '  asm'#13#10 +
             '    mov AX, 1234H'#13#10 +
             '    mov Number, AX'#13#10 +
             '  end;'#13#10 +
             '  {$R-}'#13#10 +
             'end;';
end;


class function TSynSE2Syn.GetLanguageName: string;
begin
  Result := SYNS_LangPascal;
end;

class function TSynSE2Syn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynSE2Syn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPascal;
end;

procedure TSynSE2Syn.SetDelphiVersion(const Value: TDelphiVersion);
begin
  if fDelphiVersion <> Value then
  begin
    fDelphiVersion := Value;
    if (fDelphiVersion < dvDelphi3) and fPackageSource then
      fPackageSource := False;
    DefHighlightChange(Self);
  end;
end;

procedure TSynSE2Syn.SetPackageSource(const Value: Boolean);

begin
  if fPackageSource <> Value then
  begin
    fPackageSource := Value;
    if fPackageSource and (fDelphiVersion < dvDelphi3) then
      fDelphiVersion := dvDelphi3;
    DefHighlightChange(Self);
  end;
end;

class function TSynSE2Syn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangPascal;
end;

initialization

{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSE2Syn);
{$ENDIF}
end.

{$ELSE}

{------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPas.pas, released 2000-04-17.
The Original Code is based on the mwPasSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Martin Waldenburg.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterPas.pas,v 1.30 2005/01/28 16:53:24 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Pascal/Delphi syntax highlighter for SynEdit)
@author(Martin Waldenburg)
@created(1998, converted to SynEdit 2000-04-07)
@lastmod(2001-11-21)
The SynHighlighterPas unit provides SynEdit with a Object Pascal syntax highlighter.
Two extra properties included (DelphiVersion, PackageSource):
  DelphiVersion - Allows you to enable/disable the highlighting of various
                  language enhancements added in the different Delphi versions.
  PackageSource - Allows you to enable/disable the highlighting of package keywords
}

{$IFNDEF QSYNHIGHLIGHTERPAS}
unit SynHighlighterSE2;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Windows,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkAsm, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkSpace, tkString, tkSymbol, tkUnknown, tkFloat, tkHex, tkDirec, tkChar);

  TRangeState = (rsANil, rsAnsi, rsString, rsAnsiAsm, rsAsm, rsBor, rsBorAsm, rsProperty,
    rsExports, rsDirective, rsDirectiveAsm, rsUnKnown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TDelphiVersion = (dvDelphi1, dvDelphi2, dvDelphi3, dvDelphi4, dvDelphi5,
    dvDelphi6, dvDelphi7, dvDelphi8, dvDelphi2005);

const
  LastDelphiVersion = dvDelphi2005;

type
  TSynSE2Syn = class(TSynCustomHighlighter)
  private
    fAsmStart: Boolean;
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fIdentFuncTable: array[0..191] of TIdentFuncTableFunc;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fStringAttri: TSynHighlighterAttributes;
    fCharAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fFloatAttri: TSynHighlighterAttributes;
    fHexAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fAsmAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fDirecAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fDelphiVersion: TDelphiVersion;
    fPackageSource: Boolean;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func20: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func95: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func102: TtkTokenKind;
    function Func103: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func117: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func129: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func166: TtkTokenKind;
    function Func168: TtkTokenKind;
    function Func191: TtkTokenKind;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceOpenProc;
    procedure ColonOrGreaterProc;
    procedure CRProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PointProc;
    procedure RoundOpenProc;
    procedure SemicolonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure SetDelphiVersion(const Value: TDelphiVersion);
    procedure SetPackageSource(const Value: Boolean);
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: boolean; override;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; override;
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(NewValue: string; LineNumber:Integer); override;
    procedure SetRange(Value: Pointer); override;
    function UseUserSettings(VersionIndex: integer): boolean; override;
    procedure EnumUserSettings(DelphiVersions: TStrings); override;
    property IdentChars;
  published
    property AsmAttri: TSynHighlighterAttributes read fAsmAttri write fAsmAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DirectiveAttri: TSynHighlighterAttributes read fDirecAttri
      write fDirecAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property FloatAttri: TSynHighlighterAttributes read fFloatAttri
      write fFloatAttri;
    property HexAttri: TSynHighlighterAttributes read fHexAttri
      write fHexAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property CharAttri: TSynHighlighterAttributes read fCharAttri
      write fCharAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property DelphiVersion: TDelphiVersion read fDelphiVersion write SetDelphiVersion
      default LastDelphiVersion;
    property PackageSource: Boolean read fPackageSource write SetPackageSource default True;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    J := UpCase(I);
    Case I of
      'a'..'z', 'A'..'Z', '_': mHashTable[I] := Ord(J) - 64;
    else mHashTable[Char(I)] := 0;
    end;
  end;
end;

procedure TSynSE2Syn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[20] := Func20;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[27] := Func27;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[46] := Func46;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[61] := Func61;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[75] := Func75;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[95] := Func95;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[102] := Func102;
  fIdentFuncTable[103] := Func103;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[112] := Func112;
  fIdentFuncTable[117] := Func117;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[129] := Func129;
  fIdentFuncTable[132] := Func132;
  fIdentFuncTable[133] := Func133;
  fIdentFuncTable[136] := Func136;
  fIdentFuncTable[141] := Func141;
  fIdentFuncTable[143] := Func143;
  fIdentFuncTable[166] := Func166;
  fIdentFuncTable[168] := Func168;
  fIdentFuncTable[191] := Func191;
end;

function TSynSE2Syn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  if ToHash^ in ['_', '0'..'9'] then inc(ToHash);
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TSynSE2Syn.KeyComp(const aKey: string): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end; { KeyComp }

function TSynSE2Syn.Func15: TtkTokenKind;
begin
  if KeyComp('If') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSE2Syn.Func19: TtkTokenKind;
begin
  if KeyComp('Do') then Result := tkKey else
    if KeyComp('And') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSE2Syn.Func20: TtkTokenKind;
begin
  if KeyComp('As') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSE2Syn.Func21: TtkTokenKind;
begin
  if KeyComp('Of') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSE2Syn.Func23: TtkTokenKind;
begin
  if KeyComp('End') then
  begin
    Result := tkKey;
    fRange := rsUnknown;
  end
  else if KeyComp('In') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func25: TtkTokenKind;
begin
  if KeyComp('Far') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func27: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and KeyComp('Cdecl') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func28: TtkTokenKind;
begin
  if KeyComp('Is') then
    Result := tkKey
  else if (fRange = rsProperty) and KeyComp('Read') then
    Result := tkKey
  else if KeyComp('Case') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func29: TtkTokenKind;
begin
  if KeyComp('on') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func32: TtkTokenKind;
begin
  if KeyComp('Label') then
    Result := tkKey
  else if KeyComp('Mod') then
    Result := tkKey
  else if KeyComp('File') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func33: TtkTokenKind;
begin
  if KeyComp('Or') then
    Result := tkKey
  else if KeyComp('Asm') then
  begin
    Result := tkKey;
    fRange := rsAsm;
    fAsmStart := True;
  end
  else if (fRange = rsExports) and KeyComp('name') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func35: TtkTokenKind;
begin
  if KeyComp('Nil') then
    Result := tkKey
  else if KeyComp('To') then
    Result := tkKey
  else if KeyComp('Div') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func37: TtkTokenKind;
begin
  if KeyComp('Begin') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func38: TtkTokenKind;
begin
  if KeyComp('Near') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func39: TtkTokenKind;
begin
  if KeyComp('For') then
    Result := tkKey
  else if KeyComp('Shl') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func40: TtkTokenKind;
begin
  if KeyComp('Packed') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func41: TtkTokenKind;
begin
  if KeyComp('Else') then
    Result := tkKey
  else if KeyComp('Var') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func42: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and KeyComp('Final') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func44: TtkTokenKind;
begin
  if KeyComp('Set') then
    Result := tkKey
  else if PackageSource and KeyComp('package') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func45: TtkTokenKind;
begin
  if KeyComp('Shr') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func46: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and KeyComp('Sealed') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func47: TtkTokenKind;
begin
  if KeyComp('Then') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func49: TtkTokenKind;
begin
  if KeyComp('Not') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func52: TtkTokenKind;
begin
  if KeyComp('Pascal') then
    Result := tkKey
  else if KeyComp('Raise') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func54: TtkTokenKind;
begin
  if KeyComp('Class') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func55: TtkTokenKind;
begin
  if KeyComp('Object') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func56: TtkTokenKind;
begin
  if (fRange in [rsProperty, rsExports]) and KeyComp('Index') then
    Result := tkKey
  else if KeyComp('Out') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;


function TSynSE2Syn.Func57: TtkTokenKind;
begin
  if KeyComp('Goto') then
    Result := tkKey
  else if KeyComp('While') then
    Result := tkKey
  else if KeyComp('Xor') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func59: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and KeyComp('Safecall') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func60: TtkTokenKind;
begin
  if KeyComp('With') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func61: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and KeyComp('Dispid') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func63: TtkTokenKind;
begin
  if KeyComp('Public') then
    Result := tkKey
  else if KeyComp('Record') then
    Result := tkKey
  else if KeyComp('Array') then
    Result := tkKey
  else if KeyComp('Try') then
    Result := tkKey
  else if KeyComp('Inline') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func64: TtkTokenKind;
begin
  if KeyComp('Unit') then
    Result := tkKey
  else if KeyComp('Uses') then
    Result := tkKey
  else if (DelphiVersion >= dvDelphi8) and KeyComp('Helper') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func65: TtkTokenKind;
begin
  if KeyComp('Repeat') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func66: TtkTokenKind;
begin
  if KeyComp('Type') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func69: TtkTokenKind;
begin
  if KeyComp('Default') then
    Result := tkKey
  else if KeyComp('Dynamic') then
    Result := tkKey
  else if KeyComp('Message') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func71: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and KeyComp('Stdcall') then
    Result := tkKey
  else if KeyComp('Const') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func73: TtkTokenKind;
begin
  if KeyComp('Except') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func75: TtkTokenKind;
begin
  if (fRange = rsProperty) and KeyComp('Write') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func76: TtkTokenKind;
begin
  if KeyComp('Until') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func77: TtkTokenKind;
begin
  if KeyComp('Partial') then
     result := tkKey
  else
     result := tkIdentifier
end;

function TSynSE2Syn.Func79: TtkTokenKind;
begin
  if KeyComp('Finally') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func81: TtkTokenKind;
begin
  if (fRange = rsProperty) and KeyComp('Stored') then
    Result := tkKey
  else if KeyComp('Interface') then
    Result := tkKey
  else if (DelphiVersion >= dvDelphi6) and KeyComp('deprecated') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func84: TtkTokenKind;
begin
  if KeyComp('Abstract') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func85: TtkTokenKind;
begin
  if KeyComp('Forward') then
    Result := tkKey
  else if KeyComp('Library') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func87: TtkTokenKind;
begin
  if KeyComp('String') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func88: TtkTokenKind;
begin
  if KeyComp('Program') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func91: TtkTokenKind;
begin
  if KeyComp('Downto') then
    Result := tkKey
  else if KeyComp('Private') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func92: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi4) and KeyComp('overload') then
    Result := tkKey
  else if KeyComp('Inherited') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func94: TtkTokenKind;
begin
  if KeyComp('Assembler') then
    Result := tkKey
  else if (DelphiVersion >= dvDelphi3) and (fRange = rsProperty) and KeyComp('Readonly') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func95: TtkTokenKind;
begin
  if KeyComp('Absolute') then
    Result := tkKey
  else if PackageSource and KeyComp('contains') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func96: TtkTokenKind;
begin
  if KeyComp('Published') then
    Result := tkKey
  else if KeyComp('Override') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func97: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and KeyComp('Threadvar') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func98: TtkTokenKind;
begin
  if KeyComp('Export') then
    Result := tkKey
  else if (fRange = rsProperty) and KeyComp('Nodefault') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func99: TtkTokenKind;
begin
  if KeyComp('External') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func100: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and KeyComp('Automated') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func101: TtkTokenKind;
begin
  if KeyComp('Register') then
    Result := tkKey
  else if (DelphiVersion >= dvDelphi6) and KeyComp('platform') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func102: TtkTokenKind;
begin
  if KeyComp('Function') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func103: TtkTokenKind;
begin
  if KeyComp('Virtual') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func105: TtkTokenKind;
begin
  if KeyComp('Procedure') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func106: TtkTokenKind;
begin
  if KeyComp('Protected') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func108: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi8) and KeyComp('Operator') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func112: TtkTokenKind;
begin
  if PackageSource and KeyComp('requires') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func117: TtkTokenKind;
begin
  if KeyComp('Exports') then
  begin
    Result := tkKey;
    fRange := rsExports;
  end
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func126: TtkTokenKind;
begin
  if (fRange = rsProperty) and (DelphiVersion >= dvDelphi4) and KeyComp('Implements') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func129: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and KeyComp('Dispinterface') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func132: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi4) and KeyComp('Reintroduce') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func133: TtkTokenKind;
begin
  if KeyComp('Property') then
  begin
    Result := tkKey;
    fRange := rsProperty;
  end
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func136: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi2) and KeyComp('Finalization') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;


function TSynSE2Syn.Func141: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and (fRange = rsProperty) and KeyComp('Writeonly') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func143: TtkTokenKind;
begin
  if KeyComp('Destructor') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func166: TtkTokenKind;
begin
  if KeyComp('Constructor') then
    Result := tkKey
  else if KeyComp('Implementation') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func168: TtkTokenKind;
begin
  if KeyComp('Initialization') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.Func191: TtkTokenKind;
begin
  if (DelphiVersion >= dvDelphi3) and KeyComp('Resourcestring') then
    Result := tkKey
  else if (DelphiVersion >= dvDelphi3) and KeyComp('Stringresource') then
    Result := tkKey
  else
    Result := tkIdentifier;
end;

function TSynSE2Syn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier
end;

function TSynSE2Syn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 192 then Result := fIdentFuncTable[HashKey] else
    Result := tkIdentifier;
end;

procedure TSynSE2Syn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      '#': fProcTable[I] := AsciiCharProc;
      '$': fProcTable[I] := IntegerProc;
      #39: fProcTable[I] := StringProc;
      '0'..'9': fProcTable[I] := NumberProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := IdentProc;
      '{': fProcTable[I] := BraceOpenProc;
      '}', '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case I of
            '(': fProcTable[I] := RoundOpenProc;
            '.': fProcTable[I] := PointProc;
            ';': fProcTable[I] := SemicolonProc;
            '/': fProcTable[I] := SlashProc;
            ':', '>': fProcTable[I] := ColonOrGreaterProc;
            '<': fProcTable[I] := LowerProc;
            '@': fProcTable[I] := AddressOpProc;
          else
            fProcTable[I] := SymbolProc;
          end;
        end;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynSE2Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDelphiVersion := LastDelphiVersion;
  fPackageSource := True;

  fAsmAttri := TSynHighlighterAttributes.Create(SYNS_AttrAssembler);
  AddAttribute(fAsmAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style:= [fsItalic];
  AddAttribute(fCommentAttri);
  fDirecAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor);
  fDirecAttri.Style:= [fsItalic];
  AddAttribute(fDirecAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style:= [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
  fFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat);
  AddAttribute(fFloatAttri);
  fHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal);
  AddAttribute(fHexAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  AddAttribute(fStringAttri);
  fCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter);
  AddAttribute(fCharAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  MakeMethodTables;
  fRange := rsUnknown;
  fAsmStart := False;
  fDefaultFilter := SYNS_FilterPascal;
end; { Create }

procedure TSynSE2Syn.SetLine(NewValue: string; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

procedure TSynSE2Syn.AddressOpProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '@' then inc(Run);
end;

procedure TSynSE2Syn.AsciiCharProc;
begin
  fTokenID := tkChar;
  Inc(Run);
  while FLine[Run] in ['0'..'9', '$', 'A'..'F', 'a'..'f'] do
    Inc(Run);
end;

procedure TSynSE2Syn.BorProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      if fRange in [rsDirective, rsDirectiveAsm] then
        fTokenID := tkDirec
      else
        fTokenID := tkComment;
      repeat
        if fLine[Run] = '}' then
        begin
          Inc(Run);
          if fRange in [rsBorAsm, rsDirectiveAsm] then
            fRange := rsAsm
          else
            fRange := rsUnKnown;
          break;
        end;
        Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynSE2Syn.BraceOpenProc;
begin
  if (fLine[Run + 1] = '$') then
  begin
    if fRange = rsAsm then
      fRange := rsDirectiveAsm
    else
      fRange := rsDirective;
  end
  else
  begin
    if fRange = rsAsm then
      fRange := rsBorAsm
    else
      fRange := rsBor;
  end;
  BorProc;
end;

procedure TSynSE2Syn.ColonOrGreaterProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynSE2Syn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    Inc(Run);
end; { CRProc }


procedure TSynSE2Syn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end; { IdentProc }


procedure TSynSE2Syn.IntegerProc;
begin
  inc(Run);
  fTokenID := tkHex;
  while FLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f'] do
    Inc(Run);
end; { IntegerProc }


procedure TSynSE2Syn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end; { LFProc }


procedure TSynSE2Syn.LowerProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['=', '>'] then
    Inc(Run);
end; { LowerProc }


procedure TSynSE2Syn.NullProc;
begin
  fTokenID := tkNull;
end; { NullProc }

procedure TSynSE2Syn.NumberProc;
begin
  Inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E', '-', '+'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then
          Break
        else
          fTokenID := tkFloat;
      'e', 'E': fTokenID := tkFloat;
      '-', '+':
        begin
          if fTokenID <> tkFloat then // arithmetic
            Break;
          if not (FLine[Run - 1] in ['e', 'E']) then
            Break; //float, but it ends here
        end;
    end;
    Inc(Run);
  end;
end; { NumberProc }

procedure TSynSE2Syn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in ['.', ')'] then
    Inc(Run);
end; { PointProc }

procedure TSynSE2Syn.AnsiProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then begin
        Inc(Run, 2);
        if fRange = rsAnsiAsm then
          fRange := rsAsm
        else
          fRange := rsUnKnown;
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynSE2Syn.RoundOpenProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        Inc(Run);
        if fRange = rsAsm then
          fRange := rsAnsiAsm
        else
          fRange := rsAnsi;
        fTokenID := tkComment;
        if not (fLine[Run] in [#0, #10, #13]) then
          AnsiProc;
      end;
    '.':
      begin
        inc(Run);
        fTokenID := tkSymbol;
      end;
  else
    fTokenID := tkSymbol;
  end;
end;

procedure TSynSE2Syn.SemicolonProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
  if fRange in [rsProperty, rsExports] then
    fRange := rsUnknown;
end;

procedure TSynSE2Syn.SlashProc;
begin
  Inc(Run);
  if (fLine[Run] = '/') and (fDelphiVersion > dvDelphi1) then
  begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end
  else
    fTokenID := tkSymbol;
end;

procedure TSynSE2Syn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;
  
procedure TSynSE2Syn.StringProc;
var
  iCloseChar: char;
  oldRange  : TRangeState;
begin
  oldRange := fRange;
  fRange   := rsString;
  if (FLine[Run] in [#0, #10, #13]) and (fTokenPos = Run) then
  begin
    fProcTable[ FLine[Run] ];
    Exit;
  end;
  fTokenID := tkString;
  iCloseChar := #39;
  while not( FLine[Run] in [#0, #10, #13] ) do
  begin
    if (FLine[Run] = iCloseChar) and (oldRange = rsString) then
      break;

    oldRange := rsString;
    iCloseChar := #39;
    Inc(Run);
  end;
  if (FLine[Run] = iCloseChar) then
    fRange := rsUnKnown;
  if FLine[Run] <> #0 then inc(Run);
end;  

procedure TSynSE2Syn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSE2Syn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSE2Syn.Next;
begin
  fAsmStart := False;
  fTokenPos := Run;
  case fRange of
    rsString: StringProc;
    rsAnsi, rsAnsiAsm:
      AnsiProc;
    rsBor, rsBorAsm, rsDirective, rsDirectiveAsm:
      BorProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynSE2Syn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSE2Syn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynSE2Syn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynSE2Syn.GetTokenID: TtkTokenKind;
begin
  if not fAsmStart and (fRange = rsAsm)
    and not (fTokenId in [tkNull, tkComment, tkDirec, tkSpace])
  then
    Result := tkAsm
  else
    Result := fTokenId;
end;

function TSynSE2Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkAsm: Result := fAsmAttri;
    tkComment: Result := fCommentAttri;
    tkDirec: Result := fDirecAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkFloat: Result := fFloatAttri;
    tkHex: Result := fHexAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkChar: Result := fCharAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSE2Syn.GetTokenKind: integer;
begin
  Result := Ord(GetTokenID);
end;

function TSynSE2Syn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynSE2Syn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynSE2Syn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynSE2Syn.ResetRange;
begin
  fRange:= rsUnknown;
end;

procedure TSynSE2Syn.EnumUserSettings(DelphiVersions: TStrings);
begin
  { returns the user settings that exist in the registry }
{$IFNDEF SYN_CLX}
  with TBetterRegistry.Create do
  begin
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly('\SOFTWARE\Borland\Delphi') then
      begin
        try
          GetKeyNames(DelphiVersions);
        finally
          CloseKey;
        end;
      end;
    finally
      Free;
    end;
  end;
{$ENDIF}
end;

function TSynSE2Syn.UseUserSettings(VersionIndex: integer): boolean;
// Possible parameter values:
//   index into TStrings returned by EnumUserSettings
// Possible return values:
//   true : settings were read and used
//   false: problem reading settings or invalid version specified - old settings
//          were preserved

{$IFNDEF SYN_CLX}
  function ReadDelphiSettings(settingIndex: integer): boolean;

    function ReadDelphiSetting(settingTag: string; attri: TSynHighlighterAttributes; key: string): boolean;

      function ReadDelphi2Or3(settingTag: string; attri: TSynHighlighterAttributes; name: string): boolean;
      var
        i: integer;
      begin
        for i := 1 to Length(name) do
          if name[i] = ' ' then name[i] := '_';
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
                '\Software\Borland\Delphi\'+settingTag+'\Highlight',name,true);
      end; { ReadDelphi2Or3 }

      function ReadDelphi4OrMore(settingTag: string; attri: TSynHighlighterAttributes; key: string): boolean;
      begin
        Result := attri.LoadFromBorlandRegistry(HKEY_CURRENT_USER,
               '\Software\Borland\Delphi\'+settingTag+'\Editor\Highlight',key,false);
      end; { ReadDelphi4OrMore }

    begin { ReadDelphiSetting }
      try
        if (settingTag[1] = '2') or (settingTag[1] = '3')
          then Result := ReadDelphi2Or3(settingTag,attri,key)
          else Result := ReadDelphi4OrMore(settingTag,attri,key);
      except Result := false; end;
    end; { ReadDelphiSetting }

  var
    tmpAsmAttri       : TSynHighlighterAttributes;
    tmpCommentAttri   : TSynHighlighterAttributes;
    tmpIdentAttri     : TSynHighlighterAttributes;
    tmpKeyAttri       : TSynHighlighterAttributes;
    tmpNumberAttri    : TSynHighlighterAttributes;
    tmpSpaceAttri     : TSynHighlighterAttributes;
    tmpStringAttri    : TSynHighlighterAttributes;
    tmpSymbolAttri    : TSynHighlighterAttributes;
    iVersions         : TStringList;
    iVersionTag       : string;
  begin { ReadDelphiSettings }
    {$IFDEF SYN_DELPHI_7_UP}
    //Result := False; // Silence the compiler warning 
    {$ENDIF}
    iVersions := TStringList.Create;
    try
      EnumUserSettings( iVersions );
      if (settingIndex < 0) or (settingIndex >= iVersions.Count) then
      begin
        Result := False;
        Exit;
      end;
      iVersionTag := iVersions[ settingIndex ];
    finally
      iVersions.Free;
    end;
    tmpAsmAttri     := TSynHighlighterAttributes.Create('');
    tmpCommentAttri := TSynHighlighterAttributes.Create('');
    tmpIdentAttri   := TSynHighlighterAttributes.Create('');
    tmpKeyAttri     := TSynHighlighterAttributes.Create('');
    tmpNumberAttri  := TSynHighlighterAttributes.Create('');
    tmpSpaceAttri   := TSynHighlighterAttributes.Create('');
    tmpStringAttri  := TSynHighlighterAttributes.Create('');
    tmpSymbolAttri  := TSynHighlighterAttributes.Create('');

    Result := ReadDelphiSetting( iVersionTag, tmpAsmAttri,'Assembler') and
      ReadDelphiSetting( iVersionTag, tmpCommentAttri,'Comment') and
      ReadDelphiSetting( iVersionTag, tmpIdentAttri,'Identifier') and
      ReadDelphiSetting( iVersionTag, tmpKeyAttri,'Reserved word') and
      ReadDelphiSetting( iVersionTag, tmpNumberAttri,'Number') and
      ReadDelphiSetting( iVersionTag, tmpSpaceAttri,'Whitespace') and
      ReadDelphiSetting( iVersionTag, tmpStringAttri,'String') and
      ReadDelphiSetting( iVersionTag, tmpSymbolAttri,'Symbol');
      
    if Result then
    begin
      fAsmAttri.AssignColorAndStyle( tmpAsmAttri );
      fCharAttri.AssignColorAndStyle( tmpStringAttri ); { Delphi lacks Char attribute }
      fCommentAttri.AssignColorAndStyle( tmpCommentAttri );
      fDirecAttri.AssignColorAndStyle( tmpCommentAttri ); { Delphi lacks Directive attribute }
      fFloatAttri.AssignColorAndStyle( tmpNumberAttri ); { Delphi lacks Float attribute }
      fHexAttri.AssignColorAndStyle( tmpNumberAttri ); { Delphi lacks Hex attribute }
      fIdentifierAttri.AssignColorAndStyle( tmpIdentAttri );
      fKeyAttri.AssignColorAndStyle( tmpKeyAttri );
      fNumberAttri.AssignColorAndStyle( tmpNumberAttri );
      fSpaceAttri.AssignColorAndStyle( tmpSpaceAttri );
      fStringAttri.AssignColorAndStyle( tmpStringAttri );
      fSymbolAttri.AssignColorAndStyle( tmpSymbolAttri );
    end;
    tmpAsmAttri.Free;
    tmpCommentAttri.Free;
    tmpIdentAttri.Free;
    tmpKeyAttri.Free;
    tmpNumberAttri.Free;
    tmpSpaceAttri.Free;
    tmpStringAttri.Free;
    tmpSymbolAttri.Free;
  end; { ReadDelphiSettings }
{$ENDIF}

begin
{$IFNDEF SYN_CLX}
  Result := ReadDelphiSettings( VersionIndex );
{$ELSE}
  Result := False;
{$ENDIF}
end; { TSynSE2Syn.UseUserSettings }

function TSynSE2Syn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynSE2Syn.GetSampleSource: string;
begin
  Result := '{ Syntax highlighting }'#13#10 +
             'procedure TForm1.Button1Click(Sender: TObject);'#13#10 +
             'var'#13#10 +
             '  Number, I, X: Integer;'#13#10 +
             'begin'#13#10 +
             '  Number := 123456;'#13#10 +
             '  Caption := ''The Number is'' + #32 + IntToStr(Number);'#13#10 +
             '  for I := 0 to Number do'#13#10 +
             '  begin'#13#10 +
             '    Inc(X);'#13#10 +
             '    Dec(X);'#13#10 +
             '    X := X + 1.0;'#13#10 +
             '    X := X - $5E;'#13#10 +
             '  end;'#13#10 +
             '  {$R+}'#13#10 +
             '  asm'#13#10 +
             '    mov AX, 1234H'#13#10 +
             '    mov Number, AX'#13#10 +
             '  end;'#13#10 +
             '  {$R-}'#13#10 +
             'end;';
end; { GetSampleSource }


class function TSynSE2Syn.GetLanguageName: string;
begin
  Result := SYNS_LangPascal;
end;

class function TSynSE2Syn.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := inherited GetCapabilities + [hcUserSettings];
end;

function TSynSE2Syn.IsFilterStored: boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterPascal;
end;

procedure TSynSE2Syn.SetDelphiVersion(const Value: TDelphiVersion);
begin
  if fDelphiVersion <> Value then
  begin
    fDelphiVersion := Value;
    if (fDelphiVersion < dvDelphi3) and fPackageSource then
      fPackageSource := False;
    DefHighlightChange( Self );
  end;
end;


procedure TSynSE2Syn.SetPackageSource(const Value: Boolean);
begin
  if fPackageSource <> Value then
  begin
    fPackageSource := Value;
    if fPackageSource and (fDelphiVersion < dvDelphi3) then
      fDelphiVersion := dvDelphi3;
    DefHighlightChange( Self );
  end;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSE2Syn);
{$ENDIF}
end.


{$ENDIF}


