unit uSE2RunType;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2OpCode, uSE2BaseTypes, uSE2Consts, uSE2MemoryManager, uSE2PEData;

type
  PSE2VarData = ^TSE2VarData;
  TSE2VarData =  record
    AType       : TSE2TypeIdent;
    RefCounter  : word;
    RefContent  : boolean; // Content is owned by another vardata
    case Byte of
      btU8             : (tu8         : ^TbtU8);
      btS8             : (ts8         : ^TbtS8);
      btU16            : (tu16        : ^TbtU16);
      btS16            : (ts16        : ^TbtS16);
      btU32            : (tu32        : ^TbtU32);
      btS32            : (ts32        : ^TbtS32);
      btReturnAddress,
      btS64            : (ts64        : ^TbtS64);
      btU64            : (tu64        : ^TbtU64);
      btSingle         : (tSingle     : ^TbtSingle);
      btDouble         : (tDouble     : ^TbtDouble);
      btString,
      btWideString,
      btPChar,
      btUTF8String,
      btAnsiString,
      btPAnsiChar,
      btPWideChar     : (tString     : Pointer);   // tString -> PString -> 'abc'
      btPointer,
      btRecord,
      btArray,
      btProcPtr,
      btObject         : (tPointer    : Pointer);
  end;

  TSE2VarEvent = procedure(Data: PSE2VarData) of object;

  TSE2VarHelper = class(TObject)
  private
    MM              : TSE2MemoryManager;
    FOnRecordDelete : TSE2VarEvent;

  public
    constructor Create(MemMngr: TSE2MemoryManager; OnRecordDelete: TSE2VarEvent);

    function  CreateVarData(aType: TSE2TypeIdent): PSE2VarData;
    procedure CreateVarContent(Data: PSE2VarData); overload;
    procedure CreateVarContent(Data: PSE2VarData; MM: TSE2MemoryManager); overload;
    procedure ClearVarContent(Data: PSE2VarData);
    procedure FreeVarContent(Data: PSE2VarData); overload;
    procedure FreeVarContent(Data: PSE2VarData; MM: TSE2MemoryManager); overload;
    procedure FreeVarData(Data: PSE2VarData);
    procedure SetVarData(Source, Dest: PSE2VarData); overload;
    procedure SetVarData(Source, Dest: PSE2VarData; MM: TSE2MemoryManager); overload;
    function  VarHasValue(Data: PSE2VarData; Value: Pointer): boolean;
    procedure ConvertContent(Data: PSE2VarData; newType: TSE2TypeIdent);

    procedure WriteContent(Data: PSE2VarData; Target: Pointer; size: integer = 0);

    procedure SetIntContent(Data: PSE2VarData; value: int64);
    procedure SetStringContent(Data: PSE2VarData; value: string);
    procedure SetContentAsSingle(Data: PSE2VarData; value: single);
    procedure SetContentAsDouble(Data: PSE2VarData; value: double);
  end;

  TSE2StringHelper = class(TObject)
  private
    class function  InputAsWide(Input: Pointer): WideString;
    class function  InputAsPWide(Input: Pointer): WideString;
  public
    { String }
    class procedure StringToUTF8(Input, Output: Pointer);
    class procedure StringToWide(Input, Output: Pointer);
    class procedure StringToChar(Input, Output: Pointer);
    class procedure StringToAnsiString(Input, Ouptut: Pointer);
    class procedure StringToPAnsiChar(Input, Output: Pointer);
    class procedure StringToPWideChar(Input, Output: Pointer);
    { UTF-8}
    class procedure UTF8ToString(Input, Output: Pointer);
    class procedure UTF8ToWide(Input, Output: Pointer);
    class procedure UTF8ToChar(Input, Output: Pointer);
    class procedure UTF8ToAnsiString(Input, Ouptut: Pointer);
    class procedure UTF8ToPAnsiChar(Input, Output: Pointer);
    class procedure UTF8ToPWideChar(Input, Output: Pointer);
    { WideString }
    class procedure WideToString(Input, Output: Pointer);
    class procedure WideToUTF8(Input, Output: Pointer);
    class procedure WideToChar(Input, Output: Pointer);
    class procedure WideToAnsiString(Input, Ouptut: Pointer);
    class procedure WideToPAnsiChar(Input, Output: Pointer);
    class procedure WideToPWideChar(Input, Output: Pointer);
    { PChar }
    class procedure CharToString(Input, Output: Pointer);
    class procedure CharToUTF8(Input, Output: Pointer);
    class procedure CharToWide(Input, Output: Pointer);
    class procedure CharToAnsiString(Input, Ouptut: Pointer);
    class procedure CharToPAnsiChar(Input, Output: Pointer);
    class procedure CharToPWideChar(Input, Output: Pointer);
    { AnsiString }
    class procedure AnsiStringToString(Input, Ouptut: Pointer);
    class procedure AnsiStringToUTF8(Input, Output: Pointer);
    class procedure AnsiStringToWide(Input, Output: Pointer);
    class procedure AnsiStringToChar(Input, Output: Pointer);
    class procedure AnsiStringToPAnsiChar(Input, Output: Pointer);
    class procedure AnsiStringToPWideChar(Input, Output: Pointer);
    { PAnsiChar }
    class procedure PAnsiCharToString(Input, Ouptut: Pointer);
    class procedure PAnsiCharToUTF8(Input, Output: Pointer);
    class procedure PAnsiCharToWide(Input, Output: Pointer);
    class procedure PAnsiCharToChar(Input, Output: Pointer);
    class procedure PAnsiCharToAnsiString(Input, Output: Pointer);
    class procedure PAnsiCharToPWideChar(Input, Output: Pointer);
    { PWideChar }
    class procedure PWideCharToString(Input, Ouptut: Pointer);
    class procedure PWideCharToUTF8(Input, Output: Pointer);
    class procedure PWideCharToWide(Input, Output: Pointer);
    class procedure PWideCharToChar(Input, Output: Pointer);
    class procedure PWideCharToAnsiString(Input, Output: Pointer);
    class procedure PWideCharToPAnsiChar(Input, Output: Pointer);
  end;

  TSE2VarPool = class(TObject)
  private
    FVarHelp  : TSE2VarHelper;
    {$IFDEF SEII_STACK_USE_CACHE}
    FList     : TList;
    FCounter  : integer;
    {$ENDIF}
    FMinCount : integer;
    FMaxCount : integer;
  public
    constructor Create(VarHelp: TSE2VarHelper);
    destructor Destroy; override;

    procedure ManagePool;
    procedure Clear;

    function  Pop(AType: TSE2TypeIdent): PSE2VarData;
    procedure Push(const Data: PSE2VarData);

    property MinCount: integer      read FMinCount      write FMinCount;
    property MaxCount: integer      read FMaxCount      write FMaxCount;
  end;

  TSE2Stack = class(TList)
  private
    FVarHelp : TSE2VarHelper;
    FPool    : TSE2VarPool;
    FSize    : integer;
    FTop     : PSE2VarData;

    FMaxSize : integer;
    FIncSize : integer;
  protected
    function  GetItem(index: integer): PSE2VarData;
    procedure SetItem(index: integer; value: PSE2VarData);
    function  GetTop: PSE2VarData;

    procedure ManageStack;
  public
    constructor Create(VarHelp: TSE2VarHelper); reintroduce;
    destructor Destroy; override;

    function  PushNew(AType: TSE2TypeIdent): PSE2VarData;
    procedure Push(Data: PSE2VarData);

    procedure Clear; override;
    procedure Pop; overload;
    //procedure Pop(index: integer); overload;

    //procedure PopNoDel; overload;
    //procedure PopNoDel(index: integer); overload;

    property  Top: PSE2VarData      read FTop;

    property MaxSize : integer     read FMaxSize     write FMaxSize;
    property IncSize : integer     read FIncSize     write FIncSize;

    property Size    : integer     read FSize;              
    property Pool    : TSE2VarPool read FPool;

    property Items[index: integer]: PSE2VarData read GetItem  write SetItem; default;
  end;

  PSE2RegisteredException = ^TSE2RegisteredException;
  TSE2RegisteredException = record
    AClass    : TClass;
    Meta      : TSE2MetaEntry;
  end;

  TSE2RegisteredExceptions = class(TObject)
  private
    FList : TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Sort;

    procedure Add(Ex: TClass; Meta: TSE2MetaEntry);
    function  GetException(Ex: TClass; var isUnknown: boolean): TSE2MetaEntry;
  end;

  TSE2ClassGC = class(TObject)
  private
    FList : TList;
  protected
    function GetCount: integer;
    function GetItem(index: integer): Pointer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(ptr: pointer);
    procedure Delete(ptr: pointer); overload;
    procedure Delete(index: integer); overload;

    function  IndexOf(ptr: Pointer): integer;

    property Items[index: integer]: Pointer read GetItem; default;
    property Count                : integer read GetCount;
  end;

  PSE2RecordGCEntry = ^TSE2RecordGCEntry;
  TSE2RecordGCEntry = record
    StackSize : integer;
    Ptr       : Pointer;
  end;

  TSE2RecordGC = class(TObject)
  private
    FList : TList;
  protected
    function GetCount: integer;
    function GetItem(index: integer): PSE2RecordGCEntry;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(ptr: pointer; StackSize: integer);
    procedure Delete(index: integer); overload;
    procedure Delete(ptr: pointer); overload;

    function  IndexOf(ptr: Pointer; StackSize: integer): integer; overload;
    function  IndexOf(ptr: Pointer): integer; overload;

    property Items[index: integer]: PSE2RecordGCEntry read GetItem; default;
    property Count                : integer           read GetCount;
  end;

  TSE2StackItem = class(TObject)
  private
    FUnitName     : string;
    FName         : string;
    FHasResult    : boolean;
    FResultValue  : string;

    FParamTrace   : string;
  public
    property AUnitName   : string  read FUnitName     write FUnitName;
    property Name        : string  read FName         write FName;
    property HasResult   : boolean read FHasResult    write FHasResult;
    property ResultValue : string  read FResultValue  write FResultValue;
    property ParamTrace  : string  read FParamTrace   write FParamTrace;
  end;

  TSE2StackTrace = class(TObject)
  private
    FItems : TList;
  protected
    function GetItem(index: integer): TSE2StackItem;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function  Add: TSE2StackItem;

    property  Items[index: integer]: TSE2StackItem read GetItem; default;
    property  Count                : integer       read GetCount;

  end;

implementation

uses SysUtils, uSE2SystemUnit;

{ TSE2VarHelper }

var
  TSE2MemorySize : array[TSE2TypeIdent] of byte;

procedure FillMemorySizes;
var i: TSE2TypeIdent;
    v: byte;
begin
  for i:=0 to 254 do
  begin
    v := 0;
    case i of
    btU8             : v := SizeOf(TbtU8);
    btS8             : v := SizeOf(TbtS8);
    btU16            : v := SizeOf(TbtU16);
    btS16            : v := SizeOf(TbtS16);
    btU32            : v := SizeOf(TbtU32);
    btS32            : v := SizeOf(TbtS32);
    btReturnAddress,
    btS64            : v := SizeOf(TbtS64);
    btU64            : v := SizeOf(TbtU64);
    btSingle         : v := SizeOf(TbtSingle);
    btDouble         : v := SizeOf(TbtDouble);
    btString,
    btWideString,
    btPChar,
    btUTF8String,
    btAnsiString,
    btPAnsiChar,
    btPWideChar     : v := SizeOf(Pointer);
    btPointer,
    btRecord,
    btArray,
    btObject         : v := SizeOf(Pointer);
    btProcPtr        : v := SizeOf(Pointer) * 2;
    end;
    TSE2MemorySize[i] := v;
  end;
end;

procedure TSE2VarHelper.ConvertContent(Data: PSE2VarData;
  newType: TSE2TypeIdent);
var newData  : Pointer;
    FreeData : boolean;
    pS       : PbtString;
    pU       : PbtUTF8String;
    pW       : PbtWideString;
    pC       : PbtPChar;
    pAS      : PbtAnsiString;
    pAC      : PbtPAnsiChar;
    pWC      : PbtPWideChar;
begin
  if Data.RefContent then
     exit;

  if Data.AType = newType then
     exit;

  FreeData := False;
  newData  := MM.GetMem(TSE2MemorySize[newType]);
  case Data.AType of
  btU8 :
      begin
        case newType of
        btS8           : FreeData := True;
        btU16          : TbtU16(newData^)         := TbtU8(Data.tPointer^);
        btS16          : TbtS16(newData^)         := TbtU8(Data.tPointer^);
        btS32          : TbtS32(newData^)         := TbtU8(Data.tPointer^);
        btU32          : TbtU32(newData^)         := TbtU8(Data.tPointer^);
        btS64          : TbtS64(newData^)         := TbtU8(Data.tPointer^);
        btU64          : TbtU64(newData^)         := TbtU8(Data.tPointer^);
        btSingle       : TbtSingle(newData^)      := TbtU8(Data.tPointer^);
        btDouble       : TbtDouble(newData^)      := TbtU8(Data.tPointer^);
        btPointer,
        btObject       : TbtPointer(newData^)     := TbtU8(Data.tPointer^);
        end;
      end;
  btS8 :
      begin
        case newType of
        btU8           : FreeData := True;
        btU16          : TbtU16(newData^)         := TbtS8(Data.tPointer^);
        btS16          : TbtS16(newData^)         := TbtS8(Data.tPointer^);
        btS32          : TbtS32(newData^)         := TbtS8(Data.tPointer^);
        btU32          : TbtU32(newData^)         := TbtS8(Data.tPointer^);
        btS64          : TbtS64(newData^)         := TbtS8(Data.tPointer^);
        btU64          : TbtU64(newData^)         := TbtS8(Data.tPointer^);
        btSingle       : TbtSingle(newData^)      := TbtS8(Data.tPointer^);
        btDouble       : TbtDouble(newData^)      := TbtS8(Data.tPointer^);
        btPointer,
        btObject       : TbtPointer(newData^)     := TbtS8(Data.tPointer^);
        end;
      end;
  btU16 :
      begin
        case newType of
        btU8           : TbtU8(newData^)          := TbtU16(Data.tPointer^);
        btS8           : TbtS8(newData^)          := TbtU16(Data.tPointer^);
        btS16          : FreeData := True;
        btS32          : TbtS32(newData^)         := TbtU16(Data.tPointer^);
        btU32          : TbtU32(newData^)         := TbtU16(Data.tPointer^);
        btS64          : TbtS64(newData^)         := TbtU16(Data.tPointer^);
        btU64          : TbtU64(newData^)         := TbtU16(Data.tPointer^);
        btSingle       : TbtSingle(newData^)      := TbtU16(Data.tPointer^);
        btDouble       : TbtDouble(newData^)      := TbtU16(Data.tPointer^);
        btPointer,
        btObject       : TbtPointer(newData^)     := TbtU16(Data.tPointer^);
        end;
      end;
  btS16 :
      begin
        case newType of
        btU8           : TbtU8(newData^)          := TbtS16(Data.tPointer^);
        btS8           : TbtS8(newData^)          := TbtS16(Data.tPointer^);
        btU16          : FreeData := True;
        btS32          : TbtS32(newData^)         := TbtS16(Data.tPointer^);
        btU32          : TbtU32(newData^)         := TbtS16(Data.tPointer^);
        btS64          : TbtS64(newData^)         := TbtS16(Data.tPointer^);
        btU64          : TbtU64(newData^)         := TbtU16(Data.tPointer^);
        btSingle       : TbtSingle(newData^)      := TbtS16(Data.tPointer^);
        btDouble       : TbtDouble(newData^)      := TbtS16(Data.tPointer^);
        btPointer,
        btObject       : TbtPointer(newData^)     := TbtS16(Data.tPointer^);
        end;
      end;
  btU32 :
      begin
        case newType of
        btU8           : TbtU8(newData^)          := TbtU32(Data.tPointer^);
        btS8           : TbtS8(newData^)          := TbtU32(Data.tPointer^);
        btU16          : TbtU16(newData^)         := TbtU32(Data.tPointer^);
        btS16          : TbtS16(newData^)         := TbtU32(Data.tPointer^);
        btS32          : FreeData := True;
        btS64          : TbtS64(newData^)         := TbtU32(Data.tPointer^);  
        btU64          : TbtU64(newData^)         := TbtU32(Data.tPointer^);
        btSingle       : TbtSingle(newData^)      := TbtU32(Data.tPointer^);
        btDouble       : TbtDouble(newData^)      := TbtU32(Data.tPointer^);
        btPointer,
        btObject       : TbtPointer(newData^)     := TbtU32(Data.tPointer^);
        end;
      end;
  btS32 :
      begin
        case newType of
        btU8           : TbtU8(newData^)          := TbtS32(Data.tPointer^);
        btS8           : TbtS8(newData^)          := TbtS32(Data.tPointer^);
        btU16          : TbtU16(newData^)         := TbtS32(Data.tPointer^);
        btS16          : TbtS16(newData^)         := TbtS32(Data.tPointer^);
        btU32          : FreeData := True;
        btS64          : TbtS64(newData^)         := TbtS32(Data.tPointer^);  
        btU64          : TbtU64(newData^)         := TbtS32(Data.tPointer^);
        btSingle       : TbtSingle(newData^)      := TbtS32(Data.tPointer^);
        btDouble       : TbtDouble(newData^)      := TbtS32(Data.tPointer^);
        btPointer,
        btObject       : TbtPointer(newData^)     := TbtS32(Data.tPointer^);
        end;
      end;
  btS64 :
      begin
        case newType of
        btU8           : TbtU8(newData^)          := TbtS64(Data.tPointer^);
        btS8           : TbtS8(newData^)          := TbtS64(Data.tPointer^);
        btU16          : TbtU16(newData^)         := TbtS64(Data.tPointer^);
        btS16          : TbtS16(newData^)         := TbtS64(Data.tPointer^);
        btU32          : TbtU32(newData^)         := TbtS64(Data.tPointer^);
        btS32          : TbtS32(newData^)         := TbtS64(Data.tPointer^);
        btU64          : TbtU64(newData^)         := TbtS64(Data.tPointer^);
        btSingle       : TbtSingle(newData^)      := TbtS64(Data.tPointer^);
        btDouble       : TbtDouble(newData^)      := TbtS64(Data.tPointer^);
        btPointer,
        btObject       : TbtPointer(newData^)     := TbtS64(Data.tPointer^);
        end;
      end;  
  btU64 :
      begin
        case newType of
        btU8           : TbtU8(newData^)          := TbtU64(Data.tPointer^);
        btS8           : TbtS8(newData^)          := TbtU64(Data.tPointer^);
        btU16          : TbtU16(newData^)         := TbtU64(Data.tPointer^);
        btS16          : TbtS16(newData^)         := TbtU64(Data.tPointer^);
        btU32          : TbtU32(newData^)         := TbtU64(Data.tPointer^);
        btS32          : TbtS32(newData^)         := TbtU64(Data.tPointer^);
        btS64          : TbtS64(newData^)         := TbtU64(Data.tPointer^);
        btSingle       : TbtSingle(newData^)      := TbtU64(Data.tPointer^);
        btDouble       : TbtDouble(newData^)      := TbtU64(Data.tPointer^);
        btPointer,
        btObject       : TbtPointer(newData^)     := TbtU64(Data.tPointer^);
        end;
      end;
  btSingle :
      begin
        case newType of
        btDouble       : TbtDouble(newData^)      := TbtSingle(Data.tPointer^);
        end;
      end;
  btDouble :
      begin
        case newType of
        btSingle       : TbtSingle(newData^)      := TbtDouble(Data.tPointer^);
        end;
      end;
  btPointer, btObject :
      begin
        case newType of
        btU8           : TbtU8(newData^)          := cardinal(Data.tPointer^);
        btS8           : TbtS8(newData^)          := cardinal(Data.tPointer^);
        btU16          : TbtU16(newData^)         := cardinal(Data.tPointer^);
        btS16          : TbtS16(newData^)         := cardinal(Data.tPointer^);
        btU32          : TbtU32(newData^)         := cardinal(Data.tPointer^);
        btS32          : TbtS32(newData^)         := cardinal(Data.tPointer^);
        btS64          : TbtS64(newData^)         := cardinal(Data.tPointer^);  
        btU64          : TbtU64(newData^)         := cardinal(Data.tPointer^);
        btSingle       : TbtSingle(newData^)      := cardinal(Data.tPointer^);
        btDouble       : TbtDouble(newData^)      := cardinal(Data.tPointer^);
        btPointer,
        btObject       : FreeData := True;
        btProcPtr      : PPointer(newData)^       := Pointer(Data.tPointer^);
        end;
      end;
  btProcPtr :
      begin
        case newType of
        btPointer,
        btObject      : PPointer(newData)^        := Pointer(Data.tPointer^);
        end;
      end;
  btString :
      begin
        case newType of
        btUTF8String   :
            begin
              New(pU);  pU^ := ''; PPointer(newData)^ := pU;
              TSE2StringHelper.StringToUTF8(Data.tString, newData);
            end;
        btWideString   :
            begin
              New(pW);  pW^ := ''; PPointer(newData)^ := pW;
              TSE2StringHelper.StringToWide(Data.tString, newData);
            end;
        btPChar        :
            begin
              New(pC);  pC^ := ''; PPointer(newData)^ := pC;
              TSE2StringHelper.StringToChar(Data.tString, newData);
            end;
        btAnsiString :
            begin
              New(pAS); pAS^ := ''; PPointer(newData)^ := pAS;
              TSE2StringHelper.StringToAnsiString(Data.tString, newData);
            end;
        btPAnsiChar :
            begin
              New(pAC); pAC^ := ''; PPointer(newData)^ := pAC;
              TSE2StringHelper.StringToPAnsiChar(Data.tString, newData);
            end;
        btPWideChar :
            begin
              New(pWC); pWC^ := ''; PPointer(newData)^ := pWC;
              TSE2StringHelper.StringToPWideChar(Data.tString, newData);
            end;
        end;
      end;
  btUTF8String :
      begin
        case newType of
        btString       :
            begin
              New(pS); pS^ := ''; PPointer(newData)^ := pS;
              TSE2StringHelper.UTF8ToString(Data.tString, newData);
            end;
        btWideString   :
            begin
              New(pW); pW^ := ''; PPointer(newData)^ := pW;
              TSE2StringHelper.UTF8ToWide(Data.tString, newData);
            end;
        btPChar        :
            begin
              New(pC); pC^ := ''; PPointer(newData)^ := pC;
              TSE2StringHelper.UTF8ToChar(Data.tString, newData);
            end;
        btAnsiString :
            begin
              New(pAS); pAS^ := ''; PPointer(newData)^ := pAS;
              TSE2StringHelper.UTF8ToAnsiString(Data.tString, newData);
            end;
        btPAnsiChar :
            begin
              New(pAC); pAC^ := ''; PPointer(newData)^ := pAC;
              TSE2StringHelper.UTF8ToPAnsiChar(Data.tString, newData);
            end;
        btPWideChar :
            begin
              New(pWC); pWC^ := ''; PPointer(newData)^ := pWC;
              TSE2StringHelper.UTF8ToPWideChar(Data.tString, newData);
            end;
        end;
      end;
  btWideString :
      begin
        case newType of
        btString       :
            begin
              New(pS); pS^ := ''; PPointer(newData)^ := pS;
              TSE2StringHelper.WideToString(Data.tString, newData);
            end;
        btUTF8String   :
            begin
              New(pU); pU^ := ''; PPointer(newData)^ := pU;
              TSE2StringHelper.WideToUTF8(Data.tString, newData);
            end;
        btPChar        :
            begin
              New(pC); pC^ := ''; PPointer(newData)^ := pC;
              TSE2StringHelper.WideToChar(Data.tString, newData);
            end;
        btAnsiString :
            begin
              New(pAS); pAS^ := ''; PPointer(newData)^ := pAS;
              TSE2StringHelper.WideToAnsiString(Data.tString, newData);
            end;
        btPAnsiChar :
            begin
              New(pAC); pAC^ := ''; PPointer(newData)^ := pAC;
              TSE2StringHelper.WideToPAnsiChar(Data.tString, newData);
            end;
        btPWideChar :
            begin
              New(pWC); pWC^ := ''; PPointer(newData)^ := pWC;
              TSE2StringHelper.WideToPWideChar(Data.tString, newData);
            end;
        end;
      end;
  btPChar :
      begin
        case newType of
        btString       :
            begin
              New(pS); pS^ := ''; PPointer(newData)^ := pS;
              TSE2StringHelper.CharToString(Data.tString, newData);
            end;
        btUTF8String   :
            begin
              New(pU); pU^ := ''; PPointer(newData)^ := pU;
              TSE2StringHelper.CharToUTF8(Data.tString, newData);
            end;
        btWideString   :
            begin
              New(pW); pW^ := ''; PPointer(newData)^ := pW;
              TSE2StringHelper.CharToWide(Data.tString, newData);
            end;
        btAnsiString :
            begin
              New(pAS); pAS^ := ''; PPointer(newData)^ := pAS;
              TSE2StringHelper.CharToAnsiString(Data.tString, newData);
            end;
        btPAnsiChar :
            begin
              New(pAC); pAC^ := ''; PPointer(newData)^ := pAC;
              TSE2StringHelper.CharToPAnsiChar(Data.tString, newData);
            end;
        btPWideChar :
            begin
              New(pWC); pWC^ := ''; PPointer(newData)^ := pWC;
              TSE2StringHelper.CharToPWideChar(Data.tString, newData);
            end;
        end;
      end;

  btAnsiString :
      begin
        case newType of
        btString       : 
            begin
              New(pS); pS^ := ''; PPointer(newData)^ := pS;
              TSE2StringHelper.AnsiStringToString(Data.tString, newData);
            end;
        btUTF8String   :
            begin
              New(pU); pU^ := ''; PPointer(newData)^ := pU;
              TSE2StringHelper.AnsiStringToUTF8(Data.tString, newData);
            end;
        btWideString   :
            begin
              New(pW); pW^ := ''; PPointer(newData)^ := pW;
              TSE2StringHelper.AnsiStringToWide(Data.tString, newData);
            end;
        btPChar        :
            begin
              New(pC); pC^ := ''; PPointer(newData)^ := pC;
              TSE2StringHelper.AnsiStringToChar(Data.tString, newData);
            end;
        btPAnsiChar :
            begin
              New(pAC); pAC^ := ''; PPointer(newData)^ := pAC;
              TSE2StringHelper.AnsiStringToPAnsiChar(Data.tString, newData);
            end;
        btPWideChar :
            begin
              New(pWC); pWC^ := ''; PPointer(newData)^ := pWC;
              TSE2StringHelper.AnsiStringToPWideChar(Data.tString, newData);
            end;
        end;
      end;

  btPAnsiChar :
      begin
        case newType of
        btString       : 
            begin
              New(pS); pS^ := ''; PPointer(newData)^ := pS;
              TSE2StringHelper.PAnsiCharToString(Data.tString, newData);
            end;
        btUTF8String   :
            begin
              New(pU); pU^ := ''; PPointer(newData)^ := pU;
              TSE2StringHelper.PAnsiCharToUTF8(Data.tString, newData);
            end;
        btWideString   : 
            begin
              New(pW); pW^ := ''; PPointer(newData)^ := pW;
              TSE2StringHelper.PAnsiCharToWide(Data.tString, newData);
            end;    
        btPChar        :
            begin
              New(pC); pC^ := ''; PPointer(newData)^ := pC;
              TSE2StringHelper.PAnsiCharToChar(Data.tString, newData);
            end;
        btAnsiString :
            begin
              New(pAS); pAS^ := ''; PPointer(newData)^ := pAS;
              TSE2StringHelper.PAnsiCharToAnsiString(Data.tString, newData);
            end;
        btPWideChar :
            begin
              New(pWC); pWC^ := ''; PPointer(newData)^ := pWC;
              TSE2StringHelper.PAnsiCharToPWideChar(Data.tString, newData);
            end;
        end;
      end;

  btPWideChar :
      begin
        case newType of
        btString       : 
            begin
              New(pS); pS^ := ''; PPointer(newData)^ := pS;
              TSE2StringHelper.PWideCharToString(Data.tString, newData);
            end;
        btUTF8String   :
            begin
              New(pU); pU^ := ''; PPointer(newData)^ := pU;
              TSE2StringHelper.PWideCharToUTF8(Data.tString, newData);
            end;
        btWideString   :
            begin
              New(pW); pW^ := ''; PPointer(newData)^ := pW;
              TSE2StringHelper.PWideCharToWide(Data.tString, newData);
            end;
        btPChar        :
            begin
              New(pC); pC^ := ''; PPointer(newData)^ := pC;
              TSE2StringHelper.PWideCharToChar(Data.tString, newData);
            end;
        btAnsiString :
            begin
              New(pAS); pAS^ := ''; PPointer(newData)^ := pAS;
              TSE2StringHelper.PWideCharToAnsiString(Data.tString, newData);
            end;
        btPAnsiChar :
            begin
              New(pAC); pAC^ := ''; PPointer(newData)^ := pAC;
              TSE2StringHelper.PWideCharToPAnsiChar(Data.tString, newData);
            end;
        end;
      end;


  end;

  if FreeData then
  begin
    Data.AType := newType;
    MM.FreeMem(newData);
  end else
  begin
    FreeVarContent(Data);
    Data.AType    := newType;
    Data.tPointer := newData;
  end;
end;

procedure TSE2VarHelper.CreateVarContent(Data: PSE2VarData);
var pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pAS: PbtAnsiString;
    pAC: PbtPAnsiChar;
    pWC: PbtPWideChar;
begin
  if Data.RefContent then
  begin
    Data.tPointer := nil;
    exit;
  end;

  if Data <> nil then
     Data.tPointer := MM.GetMem(TSE2MemorySize[Data.AType]);

  case Data.AType of
  btString        :
      begin
        New(pS);
        pS^ := '';
        Pointer(Data.tString^) := pS;
      end;
  btWideString    :
      begin
        New(pW);
        pW^ := '';
        Pointer(Data.tString^) := pW;
      end;
  btUTF8String    :
      begin
        New(pU);
        pU^ := '';
        Pointer(Data.tString^) := pU;
      end;
  btPChar         :
      begin
        New(pC);
        pC^ := '';
        Pointer(Data.tString^) := pC;
      end;
  btAnsiString :
      begin
        New(pAS);
        pAS^ := '';
        Pointer(Data.tString^) := pAS;
      end;
  btPAnsiChar :
      begin
        New(pAC);
        pAC^ := '';
        Pointer(Data.tString^) := pAC;
      end;
  btPWideChar :
      begin
        New(pWC);
        pWC^ := '';
        Pointer(Data.tString^) := pWC;
      end;
  end;
end;

procedure TSE2VarHelper.CreateVarContent(Data: PSE2VarData;
  MM: TSE2MemoryManager);
var pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pAS: PbtAnsiString;
    pAC: PbtPAnsiChar;
    pWC: PbtPWideChar;
begin
  if Data.RefContent then
  begin
    Data.tPointer := nil;
    exit;
  end;

  if Data <> nil then
     Data.tPointer := MM.GetMem(TSE2MemorySize[Data.AType]);

  case Data.AType of
  btString        :
      begin
        New(pS);
        pS^ := '';
        Pointer(Data.tString^) := pS;
      end;
  btWideString    :
      begin
        New(pW);
        pW^ := '';
        Pointer(Data.tString^) := pW;
      end;
  btUTF8String    :
      begin
        New(pU);
        pU^ := '';
        Pointer(Data.tString^) := pU;
      end;
  btPChar         :
      begin
        New(pC);
        pC^ := '';
        Pointer(Data.tString^) := pC;
      end;
  btAnsiString :
      begin
        New(pAS);
        pAS^ := '';
        Pointer(Data.tString^) := pAS;
      end;
  btPAnsiChar :
      begin
        New(pAC);
        pAC^ := '';
        Pointer(Data.tString^) := pAC;
      end;
  btPWideChar :
      begin
        New(pWC);
        pWC^ := '';
        Pointer(Data.tString^) := pWC;
      end;
  end;
end;

procedure TSE2VarHelper.ClearVarContent(Data: PSE2VarData);
begin
  if not Data.RefContent then
    if Data <> nil then
      case Data.AType of
      btString      : PbtString(Data.tString^)^     := '';
      btWideString  : PbtWideString(Data.tString^)^ := '';
      btUTF8String  : PbtUTF8String(Data.tString^)^ := '';
      btPChar       : PbtPChar(Data.tString^)^      := '';
      btAnsiString  : PbtAnsiString(Data.tString^)^ := '';
      btPAnsiChar   : PbtPAnsiChar(Data.tString^)^  := '';
      btPWideChar   : PbtPWideChar(Data.tString^)^  := '';
      else
          FillChar(Data.tPointer^, TSE2MemorySize[Data.AType], 0);
      end;
end;

function TSE2VarHelper.CreateVarData(aType: TSE2TypeIdent): PSE2VarData;
begin
  New(result);
  FillChar(result^, SizeOf(TSE2VarData), 0);
  result^.AType      := aType;
  if aType <> $FF then
     CreateVarContent(result);
end;

procedure TSE2VarHelper.FreeVarContent(Data: PSE2VarData);
begin
  if Data <> nil then
  begin
    if not Data.RefContent then
      if Data.tPointer <> nil then
      begin
        case Data.AType of
        btString      :
            begin
              PbtString(Data.tString^)^     := '';
              Dispose(PbtString(Data.tString^));
            end;
        btWideString  :
            begin
              PbtWideString(Data.tString^)^ := '';
              Dispose(PbtWideString(Data.tString^));
            end;
        btUTF8String  :
            begin
              PbtUTF8String(Data.tString^)^ := '';
              Dispose(PbtUTF8String(Data.tString^));
            end;
        btPChar       :
            begin
              PbtPChar(Data.tString^)^      := '';
              Dispose(PbtPChar(Data.tString^));
            end;
        btAnsiString       :
            begin
              PbtAnsiString(Data.tString^)^      := '';
              Dispose(PbtAnsiString(Data.tString^));
            end;
        btPAnsiChar       :
            begin
              PbtPAnsiChar(Data.tString^)^      := '';
              Dispose(PbtPAnsiChar(Data.tString^));
            end;
        btPWideChar       :
            begin
              PbtPWideChar(Data.tString^)^      := '';
              Dispose(PbtPWideChar(Data.tString^));
            end;
        btRecord      :
            begin
              //if @FOnRecordDelete <> nil then
              //FOnRecordDelete(Data);
            end;
        btArray       : ;
        btObject      : ;
        end;

        MM.FreeMem(Data.tPointer);
      end;
    Data.tPointer := nil;
    Data.AType    := $FF;
  end;
end;

procedure TSE2VarHelper.FreeVarContent(Data: PSE2VarData;
  MM: TSE2MemoryManager);
begin
  if Data <> nil then
  begin
    if not Data.RefContent then
      if Data.tPointer <> nil then
      begin
        case Data.AType of
        btString      :
            begin
              PbtString(Data.tString^)^     := '';
              Dispose(PbtString(Data.tString^));
            end;
        btWideString  :
            begin
              PbtWideString(Data.tString^)^ := '';
              Dispose(PbtWideString(Data.tString^));
            end;
        btUTF8String  :
            begin
              PbtUTF8String(Data.tString^)^ := '';
              Dispose(PbtUTF8String(Data.tString^));
            end;
        btPChar       :
            begin
              PbtPChar(Data.tString^)^      := '';
              Dispose(PbtPChar(Data.tString^));
            end;
        btAnsiString       :
            begin
              PbtAnsiString(Data.tString^)^      := '';
              Dispose(PbtAnsiString(Data.tString^));
            end;
        btPAnsiChar       :
            begin
              PbtPAnsiChar(Data.tString^)^      := '';
              Dispose(PbtPAnsiChar(Data.tString^));
            end;
        btPWideChar       :
            begin
              PbtPWideChar(Data.tString^)^      := '';
              Dispose(PbtPWideChar(Data.tString^));
            end;
        btRecord      :
            begin
              //if @FOnRecordDelete <> nil then
              //FOnRecordDelete(Data);
            end;
        btArray       : ;
        btObject      : ;
        end;

        MM.FreeMem(Data.tPointer);
      end;
    Data.tPointer := nil;
    Data.AType    := $FF;
  end;
end;

procedure TSE2VarHelper.FreeVarData(Data: PSE2VarData);
begin
  if Data <> nil then
  begin
    FreeVarContent(Data);
    Dispose(Data);
  end;
end;

procedure TSE2VarHelper.SetContentAsDouble(Data: PSE2VarData;
  value: double);
begin
  if Data^.AType <> btDouble then
  begin
    FreeVarContent(Data);
    Data^.AType := btDouble;
    CreateVarContent(Data);
  end;
  Data^.tDouble^ := value;
end;

procedure TSE2VarHelper.SetContentAsSingle(Data: PSE2VarData;
  value: single);
begin
  if Data^.AType <> btSingle then
  begin
    FreeVarContent(Data);
    Data^.AType := btSingle;
    CreateVarContent(Data);
  end;
  Data^.tSingle^ := value;
end;

procedure TSE2VarHelper.SetVarData(Source, Dest: PSE2VarData);
begin
  if (Source = nil) or (Dest = nil) then
     exit;

  if Dest^.RefContent then
  begin
    case Source^.AType of
    btString      : PbtString(Dest.tString^)^     := PbtString(Source.tString^)^;
    btWideString  : PbtWideString(Dest.tString^)^ := PbtWideString(Source.tString^)^;
    btUTF8String  : PbtUTF8String(Dest.tString^)^ := PbtUTF8String(Source.tString^)^;
    btPChar       : PbtPChar(Dest.tString^)^      := PbtPChar(Source.tString^)^;
    btAnsiString  : PbtAnsiString(Dest.tString^)^ := PbtAnsiString(Source.tString^)^;  
    btPAnsiChar   : PbtPAnsiChar(Dest.tString^)^  := PbtPAnsiChar(Source.tString^)^; 
    btPWideChar   : PbtPWideChar(Dest.tString^)^  := PbtPWideChar(Source.tString^)^;
    else Move(Source^.tPointer^, Dest^.tPointer^, TSE2MemorySize[Dest^.AType]);
    end;
  end else
  begin

    if not ((Dest.AType = btProcPtr) and (Source.AType in [btProcPtr, btPointer])) then
    begin
      if Dest.AType <> Source.AType then
      begin
        FreeVarContent(Dest);
        Dest.AType := Source.AType;
        CreateVarContent(Dest);
      end;
    end;

    case Source^.AType of
    btString      : PbtString(Dest.tString^)^     := PbtString(Source.tString^)^;
    btWideString  : PbtWideString(Dest.tString^)^ := PbtWideString(Source.tString^)^;
    btUTF8String  : PbtUTF8String(Dest.tString^)^ := PbtUTF8String(Source.tString^)^;
    btPChar       : PbtPChar(Dest.tString^)^      := PbtPChar(Source.tString^)^;
    btAnsiString  : PbtAnsiString(Dest.tString^)^ := PbtAnsiString(Source.tString^)^;
    btPAnsiChar   : PbtPAnsiChar(Dest.tString^)^  := PbtPAnsiChar(Source.tString^)^;
    btPWideChar   : PbtPWideChar(Dest.tString^)^  := PbtPWideChar(Source.tString^)^;
    (*btRecord      :
        begin
          uSE2SystemUnit.DestroyScriptRecord(PPointer(Dest^.tPointer)^);
          PPointer(Dest^.tPointer)^ := PPointer(Source^.tPointer)^;
        end; *)
    else Move(Source^.tPointer^, Dest^.tPointer^, TSE2MemorySize[Source.AType]);
    end;
  end;
end;

procedure TSE2VarHelper.SetVarData(Source, Dest: PSE2VarData;
  MM: TSE2MemoryManager);
begin
  if (Source = nil) or (Dest = nil) then
     exit;

  if Dest^.RefContent then
  begin
    case Source^.AType of
    btString      : PbtString(Dest.tString^)^     := PbtString(Source.tString^)^;
    btWideString  : PbtWideString(Dest.tString^)^ := PbtWideString(Source.tString^)^;
    btUTF8String  : PbtUTF8String(Dest.tString^)^ := PbtUTF8String(Source.tString^)^;
    btPChar       : PbtPChar(Dest.tString^)^      := PbtPChar(Source.tString^)^;  
    btAnsiString  : PbtAnsiString(Dest.tString^)^ := PbtAnsiString(Source.tString^)^;
    btPAnsiChar   : PbtPAnsiChar(Dest.tString^)^  := PbtPAnsiChar(Source.tString^)^;
    btPWideChar   : PbtPWideChar(Dest.tString^)^  := PbtPWideChar(Source.tString^)^;
    else Move(Source^.tPointer^, Dest^.tPointer^, TSE2MemorySize[Dest^.AType]);
    end;
  end else
  begin

    if not ((Dest.AType = btProcPtr) and (Source.AType in [btProcPtr, btPointer])) then
    begin
      if Dest.AType <> Source.AType then
      begin
        FreeVarContent(Dest, MM);
        Dest.AType := Source.AType;
        CreateVarContent(Dest, MM);
      end;
    end;

    case Source^.AType of
    btString      : PbtString(Dest.tString^)^     := PbtString(Source.tString^)^;
    btWideString  : PbtWideString(Dest.tString^)^ := PbtWideString(Source.tString^)^;
    btUTF8String  : PbtUTF8String(Dest.tString^)^ := PbtUTF8String(Source.tString^)^;
    btPChar       : PbtPChar(Dest.tString^)^      := PbtPChar(Source.tString^)^;  
    btAnsiString  : PbtAnsiString(Dest.tString^)^ := PbtAnsiString(Source.tString^)^;
    btPAnsiChar   : PbtPAnsiChar(Dest.tString^)^  := PbtPAnsiChar(Source.tString^)^;
    btPWideChar   : PbtPWideChar(Dest.tString^)^  := PbtPWideChar(Source.tString^)^;
    (*btRecord      :
        begin
          uSE2SystemUnit.DestroyScriptRecord(PPointer(Dest^.tPointer)^);
          PPointer(Dest^.tPointer)^ := PPointer(Source^.tPointer)^;
        end; *)
    else Move(Source^.tPointer^, Dest^.tPointer^, TSE2MemorySize[Source.AType]);
    end;
  end;
end;

function TSE2VarHelper.VarHasValue(Data: PSE2VarData; Value: Pointer): boolean;
begin
  result := False;

  case Data.AType of
  btU8       : result := Data^.tu8^        = PByte(Value)^;
  btS8       : result := Data^.ts8^        = PShortint(Value)^;
  btU16      : result := Data^.tu16^       = PWord(Value)^;
  btS16      : result := Data^.ts16^       = PSmallint(Value)^;
  btU32      : result := Data^.tu32^       = PCardinal(Value)^;
  btS32      : result := Data^.ts32^       = PInteger(Value)^;
  btReturnAddress,
  btS64      : result := Data^.ts64^       = PInt64(Value)^;    
  btU64      : result := Data^.ts64^       = PUInt64(Value)^;
  btSingle   : result := Data^.tSingle^    = PSingle(Value)^;
  btDouble   : result := Data^.tDouble^    = PDouble(Value)^;

  btString,
  btUTF8String,
  btWideString,
  btPChar,
  btAnsiString,
  btPAnsiChar,
  btPWideChar  : result := Data^.tString   = PPointer(Value)^;
  btPointer,
  btRecord,
  btObject    : result := Data^.tPointer   = PPointer(Value)^;
  end;
end;

procedure TSE2VarHelper.SetIntContent(Data: PSE2VarData;
  value: int64);
begin
  case Data.AType of
  btU8              : Data^.tu8^   := value;
  btS8              : Data^.ts8^   := value;
  btU16             : Data^.tu16^  := value;
  btS16             : Data^.ts16^  := value;
  btU32             : Data^.tu32^  := value;
  btS32             : Data^.ts32^  := value;
  btS64             : Data^.ts64^  := value;  
  btU64             : Data^.ts64^  := value;
  end;
end;

procedure TSE2VarHelper.SetStringContent(Data: PSE2VarData;
  value: string);
begin

  case Data.AType of
  btString          : PbtString(Data.tString^)^     := value;
  btWideString      : PbtWideString(Data.tString^)^ := value;
  btUTF8String      :
      {$IFDEF DELPHI2009UP}
        TSE2StringHelper.StringToUTF8(@value, Data.tString);
      {$ELSE}
        PbtUTF8String(Data.tString^)^ := value;
      {$ENDIF}
  btPChar           : PbtPChar(Data.tString^)^      := PChar(value);
  btAnsiString      : PbtAnsiString(Data.tString^)^ := AnsiString(value);
  btPAnsiChar       : PbtPAnsiChar(Data.tString^)^  := PAnsiChar(AnsiString(value));
  btPWideChar       : PbtPWideChar(Data.tString^)^  := PWideChar(WideString(value));
  end;
end;

constructor TSE2VarHelper.Create(MemMngr: TSE2MemoryManager; OnRecordDelete: TSE2VarEvent);
begin
  inherited Create;
  MM := MemMngr;
  FOnRecordDelete := OnRecordDelete;
end;

procedure TSE2VarHelper.WriteContent(Data: PSE2VarData; Target: Pointer;
  size: integer);
begin
  case Data.AType of
  btU8, btS8, btU16, btS16,
  btU32, btS32, btS64, btU64,
  btSingle, btDouble,
  btString, btWideString, btUTF8String, btPChar, btPAnsiChar, btPWideChar, btAnsiString,
  btPointer, btObject :
      Move(Data.tPointer^, Target^, TSE2MemorySize[Data.AType]);
  else
      Move(PPointer(Data.tPointer^)^, Target^, size);
  end;
end;

{ TSE2VarPool }

procedure TSE2VarPool.Clear;
{$IFDEF SEII_STACK_USE_CACHE}
var i: integer;
begin
  for i:=FCounter-1 downto 0 do
    FVarHelp.FreeVarData(FList.List[i]);
  FCounter := 0;
  FList.Clear;
{$ELSE}
begin
{$ENDIF}
end;

constructor TSE2VarPool.Create(VarHelp: TSE2VarHelper);
begin
  inherited Create;
  FVarHelp := VarHelp;
  {$IFDEF SEII_STACK_USE_CACHE}
  FList    := TList.Create;
  {$ENDIF}

  FMinCount := 5;
  FMaxCount := 255;

  {$IFDEF SEII_STACK_USE_CACHE}
  FCounter  := 0;
  FList.Count := FMaxCount + 1;

  ManagePool;
  {$ENDIF}
end;

destructor TSE2VarPool.Destroy;
begin
  {$IFDEF SEII_STACK_USE_CACHE}
  Clear;
  FList.Free;
  {$ENDIF}
  inherited;
end;

procedure TSE2VarPool.ManagePool;
{$IFDEF SEII_STACK_USE_CACHE}
var Difference : integer;
    i          : integer;
begin
  for i:=FCounter to FMaxCount - 1 do
  begin
    FList.List[i] := FVarHelp.CreateVarData($FF);
  end;
  FCounter := FMaxCount;
  exit;
  Difference := (FMaxCount - FMinCount) div 3;
  if FCounter < (FMaxCount - Difference * 2) then
  begin
    for i:=0 to Difference do
    begin
      FList.List[FCounter] := FVarHelp.CreateVarData($FF);
      FCounter := FCounter + 1;
    end;
  end;
{$ELSE}
begin
{$ENDIF}
end;

function TSE2VarPool.Pop(AType: TSE2TypeIdent): PSE2VarData;
begin
  {$IFDEF SEII_STACK_USE_CACHE}
  if FCounter = 0 then
  begin
  {$ENDIF}
    result := FVarHelp.CreateVarData(AType);
    result.RefCounter := 1;
  {$IFDEF SEII_STACK_USE_CACHE}
  end else
  begin
  {$ENDIF}

  {$IFDEF SEII_STACK_USE_CACHE}
    dec(FCounter);
    result := FList.List[FCounter];

    result.AType      := AType;
    result.RefCounter := 1;
    FVarHelp.CreateVarContent(result);
  {$ENDIF}

  {$IFDEF SEII_STACK_USE_CACHE}
  end;
  {$ENDIF}
end;

procedure TSE2VarPool.Push(const Data: PSE2VarData);
begin
  {$IFDEF SEII_STACK_USE_CACHE}
  if (FCounter < FMaxCount) then
  begin
    FVarHelp.FreeVarContent(Data);
    Data.RefContent := False;
    FList.List[FCounter] := (Data);
    inc(FCounter);
  end else
  {$ENDIF}
    FVarHelp.FreeVarData(Data);
end;

{ TSE2Stack }

procedure TSE2Stack.Clear;
var i: integer;
begin
  for i:=FSize-1 downto 0 do
    Pop;
  inherited;
  //FList.Clear;
  FSize := 0;
end;

const
  BaseStackSize = $AFFF;

constructor TSE2Stack.Create(VarHelp: TSE2VarHelper);
begin
  inherited Create;
  FVarHelp := VarHelp;
  FPool := TSE2VarPool.Create(FVarHelp);

  FMaxSize := $FFFFF;
  FIncSize := $3E8;

  Self.Count := BaseStackSize;

  ManageStack;
end;

destructor TSE2Stack.Destroy;
begin
  Clear;
  //FList.Free;
  FPool.Free;
  inherited;
end;

function TSE2Stack.GetItem(index: integer): PSE2VarData;
begin
  if (index < 0) or (index >= FSize) then
     result := nil
  else
     result := List[index];
end;

function TSE2Stack.GetTop: PSE2VarData;
begin
  if FSize = 0 then
     result := nil
  else
     result := inherited Items[FSize - 1];
end;

procedure TSE2Stack.ManageStack;
var newSize : integer;
begin
  if Self.Count < FSize + 1 then
  begin
    newSize := FSize + FIncSize;
    if newSize > FMaxSize then
       raise ESE2StackOverflow.Create('Stack Overflow');

    Self.Count := newSize;
  end else
  if (Self.Count > FSize + 1 + (FIncSize * 5)) then
  begin
    newSize := FSize + 1 + FIncSize * 2;
    if newSize < BaseStackSize then
       newSize := BaseStackSize;

    if Self.Count <> newSize then
       Self.Count := newSize;
  end;
end;

procedure TSE2Stack.Pop;
var p: PSE2VarData;
begin
  if Count > 0 then
  begin
    p := List[FSize - 1]; // inherited Items[FSize - 1];
    dec(p^.RefCounter);

    if p^.RefCounter = 0 then
       FPool.Push(p);

    dec(FSize);
    if FSize > 0 then
       FTop  := List[FSize - 1]
    else
       FTop  := nil;
  end;
  //ManageStack;
end;

procedure TSE2Stack.Push(Data: PSE2VarData);
begin
  if Self.Count <= FSize + 1 then
     ManageStack;
  FSize := FSize + 1;

  List[FSize - 1] := Data;
  inc(Data^.RefCounter);
  FTop := Data;
end;

function TSE2Stack.PushNew(AType: TSE2TypeIdent): PSE2VarData;
begin
  if Self.Count <= FSize + 1 then
     ManageStack;
  inc(FSize);

  result           := FPool.Pop(AType);
  List[FSize - 1]  := result;
  FTop := result;
end;

procedure TSE2Stack.SetItem(index: integer; value: PSE2VarData);
begin
  if (index >= 0) and (index < FSize) then
  begin
     inherited Items[index] := value;
     value^.RefCounter := value^.RefCounter + 1;
     if index = FSize - 1 then
        FTop := value;
  end;
end;

{ TSE2StringHelper }

class procedure TSE2StringHelper.AnsiStringToChar(Input, Output: Pointer);
begin
  PbtPChar(Output^)^ := PChar(string(PbtAnsiString(Input^)^));
end;

class procedure TSE2StringHelper.AnsiStringToPAnsiChar(Input,
  Output: Pointer);
begin
  PbtPAnsiChar(Output^)^ := PAnsiChar(PbtAnsiString(Input^)^);
end;

class procedure TSE2StringHelper.AnsiStringToPWideChar(Input,
  Output: Pointer);
begin
  PbtPWideChar(Output^)^ :=
    PWideChar(
      {$IFDEF DELPHI2009UP}
         UTF8ToWideString( AnsiToUtf8( string(PbtAnsiString(Input^)^)))
      {$ELSE}
         UTF8Decode(AnsiToUtf8(PbtAnsiString(Input^)^))
      {$ENDIF}
    );
end;

class procedure TSE2StringHelper.AnsiStringToString(Input,
  Ouptut: Pointer);
begin
  PbtString(Ouptut^)^ := string(PbtAnsiString(Input^)^);
end;

class procedure TSE2StringHelper.AnsiStringToUTF8(Input, Output: Pointer);
begin
  PbtUTF8String(Output^)^ := AnsiToUtf8(string(PbtAnsiString(Input^)^));
end;

class procedure TSE2StringHelper.AnsiStringToWide(Input, Output: Pointer);
begin
  PbtWideString(Output^)^ :=
    {$IFDEF DELPHI2009UP}
       UTF8ToWideString( AnsiToUtf8( string(PbtAnsiString(Input^)^)));
    {$ELSE}
       UTF8Decode(AnsiToUtf8(PbtAnsiString(Input^)^));
    {$ENDIF}
end;

class procedure TSE2StringHelper.CharToAnsiString(Input, Ouptut: Pointer);
begin
  PbtAnsiString(Ouptut^)^ := AnsiString(PbtPChar(Input^)^);
end;

class procedure TSE2StringHelper.CharToPAnsiChar(Input, Output: Pointer);
begin
  PbtPAnsiChar(Output^)^ := PAnsiChar(PbtPChar(Input^)^);
end;

class procedure TSE2StringHelper.CharToPWideChar(Input, Output: Pointer);
begin
  PbtPWideChar(Output^)^ :=
    PWideChar(
      {$IFDEF DELPHI2009UP}
         PbtChar(Input^)^
      {$ELSE}
         UTF8Decode(AnsiToUtf8(string(PbtPChar(Input^)^)))
      {$ENDIF}
    );
end;

class procedure TSE2StringHelper.CharToString(Input, Output: Pointer);
begin
  PbtString(Output^)^ := string(PbtPChar(Input^)^);
end;

class procedure TSE2StringHelper.CharToUTF8(Input, Output: Pointer);
begin
  {$IFDEF DELPHI2009UP}
  PbtUTF8String(Output^)^ := UTF8Encode(string(PbtPChar(Input^)^));
  {$ELSE}
  PbtUTF8String(Output^)^ := AnsiToUtf8(string(PbtPChar(Input^)^));
  {$ENDIF}
end;

class procedure TSE2StringHelper.CharToWide(Input, Output: Pointer);
begin
  PbtWideString(Output^)^ :=
    {$IFDEF DELPHI2009UP}
       string(PbtPChar(Input^)^);
    {$ELSE}
       UTF8Decode(AnsiToUtf8(string(PbtPChar(Input^)^)));
    {$ENDIF}
end;

class procedure TSE2StringHelper.PAnsiCharToAnsiString(Input,
  Output: Pointer);
begin
  PbtAnsiString(Output^)^ := AnsiString(PbtPAnsiChar(Input^)^);
end;

class procedure TSE2StringHelper.PAnsiCharToChar(Input, Output: Pointer);
begin
  {$IFDEF DELPHI2009UP}
  PbtPChar(Output^)^ := PChar(string(PbtPAnsiChar(Input^)^));
  {$ELSE}
  PbtPChar(Output^)^ := PChar(PbtPAnsiChar(Input^)^);
  {$ENDIF}
end;

class procedure TSE2StringHelper.PAnsiCharToPWideChar(Input,
  Output: Pointer);
begin
  PbtPWideChar(Output^)^ :=
    PWideChar(
      {$IFDEF DELPHI2009UP}
         UTF8ToWideString( AnsiToUtf8( string(PbtAnsiString(Input^)^)))
      {$ELSE}
         UTF8Decode(AnsiToUtf8(PbtAnsiString(Input^)^))
      {$ENDIF}
    );
end;

class procedure TSE2StringHelper.PAnsiCharToString(Input, Ouptut: Pointer);
begin
  PbtString(Ouptut^)^ := string(PbtPAnsiChar(Input^)^);
end;

class procedure TSE2StringHelper.PAnsiCharToUTF8(Input, Output: Pointer);
begin
  PbtUTF8String(Output^)^ := AnsiToUtf8(string(PbtPAnsiChar(Input^)^));
end;

class procedure TSE2StringHelper.PAnsiCharToWide(Input, Output: Pointer);
begin
  PbtWideString(Output^)^ :=
    {$IFDEF DELPHI2009UP}
       UTF8ToWideString( AnsiToUtf8( string(PbtPAnsiChar(Input^)^) ));
    {$ELSE}
       UTF8Decode(AnsiToUtf8(AnsiString(PbtPAnsiChar(Input^)^)));
    {$ENDIF}
end;

class procedure TSE2StringHelper.PWideCharToAnsiString(Input,
  Output: Pointer);
begin
  PbtAnsiString(Output^)^ := AnsiString(Utf8ToAnsi(UTF8Encode(PbtPWideChar(Input^)^)));
end;

class procedure TSE2StringHelper.PWideCharToChar(Input, Output: Pointer);
begin
  {$IFDEF DELPHI2009UP}
  PbtPChar(Output^)^ := PChar(PbtPWideChar(Input^)^);
  {$ELSE}
  PbtPChar(Output^)^ := PChar(Utf8ToAnsi(UTF8Encode(PbtPWideChar(Input^)^)));
  {$ENDIF}
end;

class procedure TSE2StringHelper.PWideCharToPAnsiChar(Input,
  Output: Pointer);
begin
  PbtPAnsiChar(Output^)^ := PAnsiChar(AnsiString(Utf8ToAnsi(UTF8Encode(PbtPWideChar(Input^)^))));
end;

class procedure TSE2StringHelper.PWideCharToString(Input, Ouptut: Pointer);
begin
  {$IFDEF DELPHI2009UP}
  PbtString(Ouptut^)^ := string(PbtPWideChar(Input^)^);
  {$ELSE}
  PbtString(Ouptut^)^ := Utf8ToAnsi(UTF8Encode(PbtPWideChar(Input^)^));
  {$ENDIF}
end;

class procedure TSE2StringHelper.PWideCharToUTF8(Input, Output: Pointer);
begin
  PbtUTF8String(Output^)^ := UTF8Encode(PbtPWideChar(Input^)^);
end;

class function TSE2StringHelper.InputAsPWide(Input: Pointer): WideString;
begin
  result := PbtPWideChar(Input^)^;
end;

class procedure TSE2StringHelper.PWideCharToWide(Input, Output: Pointer);
begin
  PbtWideString(Output^)^ :=  InputAsPWide(Input);
end;

class procedure TSE2StringHelper.StringToAnsiString(Input,
  Ouptut: Pointer);
begin
  PbtAnsiString(Ouptut^)^ := AnsiString(PbtString(Input^)^);
end;

class procedure TSE2StringHelper.StringToChar(Input, Output: Pointer);
begin
  PbtPChar(Output^)^ := PChar(PbtString(Input^)^);
end;

class procedure TSE2StringHelper.StringToPAnsiChar(Input, Output: Pointer);
begin
  PbtPAnsiChar(Output^)^ := PAnsiChar(AnsiString(PbtString(Input^)^));
end;

class procedure TSE2StringHelper.StringToPWideChar(Input, Output: Pointer);
begin
  PbtPWideChar(Output^)^ :=
    PWideChar(
      {$IFDEF DELPHI2009UP}
         PChar( PbtString(Input^)^)
      {$ELSE}
         UTF8Decode(AnsiToUtf8(PbtString(Input^)^))
      {$ENDIF}
    );
end;

class procedure TSE2StringHelper.StringToUTF8(Input, Output: Pointer);
begin
  {$IFDEF DELPHI2009UP}
  PbtUTF8String(Output^)^ := UTF8Encode(PbtString(Input^)^);
  {$ELSE}
  PbtUTF8String(Output^)^ := AnsiToUtf8(PbtString(Input^)^);
  {$ENDIF}
end;

class procedure TSE2StringHelper.StringToWide(Input, Output: Pointer);
begin
  PbtWideString(Output^)^ :=
    {$IFDEF DELPHI2009UP}
       PChar( PbtString(Input^)^)
    {$ELSE}
       UTF8Decode(AnsiToUtf8(PbtString(Input^)^))
    {$ENDIF}
end;

class procedure TSE2StringHelper.UTF8ToAnsiString(Input, Ouptut: Pointer);
begin
  PbtAnsiString(Ouptut^)^ := AnsiString(Utf8ToAnsi(PbtUTF8String(Input^)^));
end;

class procedure TSE2StringHelper.UTF8ToChar(Input, Output: Pointer);
begin
  {$IFDEF DELPHI2009UP}
  PbtPChar(Output^)^ := PChar(System.UTF8ToString( PbtUTF8String(Input^)^) );
  {$ELSE}
  PbtPChar(Output^)^ := PChar(Utf8ToAnsi(PbtUTF8String(Input^)^));
  {$ENDIF}
end;

class procedure TSE2StringHelper.UTF8ToPAnsiChar(Input, Output: Pointer);
begin
  PbtPAnsiChar(Output^)^ := PAnsiChar(AnsiString(Utf8ToAnsi(PbtUTF8String(Input^)^)));
end;

class procedure TSE2StringHelper.UTF8ToPWideChar(Input, Output: Pointer);
begin
  {$IFDEF DELPHI2009UP}
  PbtPWideChar(Output^)^ := PWideChar(System.UTF8ToString(PbtUTF8String(Input^)^));
  {$ELSE}
  PbtPWideChar(Output^)^ := PWideChar(UTF8Decode(PbtUTF8String(Input^)^));
  {$ENDIF}
end;

class procedure TSE2StringHelper.UTF8ToString(Input, Output: Pointer);
begin
  {$IFDEF DELPHI2009UP}
  PbtString(Output^)^ := System.UTF8ToString(PbtUTF8String(Input^)^);
  {$ELSE}
  PbtString(Output^)^ := Utf8ToAnsi(PbtUTF8String(Input^)^);
  {$ENDIF}
end;

class procedure TSE2StringHelper.UTF8ToWide(Input, Output: Pointer);
begin
  PbtWideString(Output^)^ :=
    {$IFDEF DELPHI2009UP}
       UTF8ToWideString( PbtUTF8String(Input^)^ );
    {$ELSE}
       UTF8Decode(PbtUTF8String(Input^)^);
    {$ENDIF}
end;

class function TSE2StringHelper.InputAsWide(Input: Pointer): WideString;
var s: UTF8String;
begin
  s := UTF8Encode(PbtWideString(Input^)^);
  result :=
    {$IFDEF DELPHI2009UP}
       UTF8ToWideString( s );
    {$ELSE}
       UTF8Decode( s );
    {$ENDIF}
end;

class procedure TSE2StringHelper.WideToAnsiString(Input, Ouptut: Pointer);
begin
  PbtAnsiString(Ouptut^)^ := AnsiString(Utf8ToAnsi(UTF8Encode(PbtWideString(Input^)^)));
end;

class procedure TSE2StringHelper.WideToChar(Input, Output: Pointer);
begin
  {$IFDEF DELPHI2009UP}
  PbtPChar(Output^)^ := PChar(PbtWideString(Input^)^);
  {$ELSE}
  PbtPChar(Output^)^ := PChar(Utf8ToAnsi(UTF8Encode(PbtWideString(Input^)^)));
  {$ENDIF}
end;

class procedure TSE2StringHelper.WideToPAnsiChar(Input, Output: Pointer);
begin
  PbtPAnsiChar(Output^)^ := PAnsiChar(AnsiString(Utf8ToAnsi(UTF8Encode(PbtWideString(Input^)^))));
end;

class procedure TSE2StringHelper.WideToPWideChar(Input, Output: Pointer);
var s: WideString;
begin
  s := InputAsWide(Input);
  PbtPWideChar(Output^)^ := PWideChar(s);
end;

class procedure TSE2StringHelper.WideToString(Input, Output: Pointer);
begin
  {$IFDEF DELPHI2009UP}
  PbtString(Output^)^ := string(PbtWideString(Input^)^);
  {$ELSE}
  PbtString(Output^)^ := Utf8ToAnsi(UTF8Encode(PbtWideString(Input^)^));
  {$ENDIF}
end;

class procedure TSE2StringHelper.WideToUTF8(Input, Output: Pointer);
begin
  PbtUTF8String(Output^)^ := UTF8Encode(PbtWideString(Input^)^);
end;

{ TSE2ClassGC }

procedure TSE2ClassGC.Add(ptr: pointer);
begin
  if ptr <> nil then
    if IndexOf(ptr) < 0 then
       FList.Add(ptr);
end;

procedure TSE2ClassGC.Clear;
begin
  FList.Clear;
end;

constructor TSE2ClassGC.Create;
begin
  inherited;
  FList := TList.Create;
end;

procedure TSE2ClassGC.Delete(ptr: pointer);
var i: integer;
begin
  i := IndexOf(ptr);
  if i > -1 then
     FList.Delete(i);
end;

procedure TSE2ClassGC.Delete(index: integer);
begin
  FList.Delete(index);
end;

destructor TSE2ClassGC.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2ClassGC.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2ClassGC.GetItem(index: integer): Pointer;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := Flist.List[index];
end;

function TSE2ClassGC.IndexOf(ptr: Pointer): integer;
begin
  for result := FList.Count-1 downto 0 do
    if FList.List[result] = ptr then
      exit;
  result := -1;
end;

{ TSE2RecordGC }

procedure TSE2RecordGC.Add(ptr: pointer; StackSize: integer);
var p: PSE2RecordGCEntry;
begin
  New(p);
  p^.StackSize := StackSize;
  p^.Ptr       := ptr;
  FList.Add(p);
end;

procedure TSE2RecordGC.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Dispose(PSE2RecordGCEntry(FList.List[i]));
  FList.Clear;
end;

constructor TSE2RecordGC.Create;
begin
  inherited;
  FList := TList.Create;
end;

procedure TSE2RecordGC.Delete(index: integer);
var p: PSE2RecordGCEntry;
begin
  p := FList.List[index];
  Flist.Delete(index);
  Dispose(p);
end;

procedure TSE2RecordGC.Delete(ptr: pointer);
var i: integer;
begin
  i := IndexOf(ptr);
  if i > -1 then
     Delete(i);
end;

destructor TSE2RecordGC.Destroy;
begin
  Clear;
  Flist.Free;
  inherited;
end;

function TSE2RecordGC.GetCount: integer;
begin
  result := Flist.Count;
end;

function TSE2RecordGC.GetItem(index: integer): PSE2RecordGCEntry;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := Flist.List[index];
end;

function TSE2RecordGC.IndexOf(ptr: Pointer; StackSize: integer): integer;
begin
  for result := FList.Count-1 downto 0 do
  begin
    if PSE2RecordGCEntry(FList.List[result])^.Ptr = ptr then
      if PSE2RecordGCEntry(FList.List[result])^.StackSize = StackSize then
         exit;
  end;
  result := -1;
end;

function TSE2RecordGC.IndexOf(ptr: Pointer): integer;
begin
  for result := FList.Count-1 downto 0 do
  begin
    if PSE2RecordGCEntry(FList.List[result])^.Ptr = ptr then
       exit;
  end;
  result := -1;
end;

{ TSE2StackTrace }

function TSE2StackTrace.Add: TSE2StackItem;
begin
  result := TSE2StackItem.Create;
  FItems.Add(result);
end;

procedure TSE2StackTrace.Clear;
var i: integer;
begin
  for i:=FItems.Count-1 downto 0 do
    TSE2StackItem(FItems[i]).Free;
  FItems.Clear;
end;

constructor TSE2StackTrace.Create;
begin
  inherited;
  FItems := TList.Create;
end;

destructor TSE2StackTrace.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TSE2StackTrace.GetCount: integer;
begin
  result := FItems.Count;
end;

function TSE2StackTrace.GetItem(index: integer): TSE2StackItem;
begin
  if (index < 0) or (index >= FItems.Count) then
     result := nil
  else
     result := FItems[index];

end;

{ TSE2RegisteredExceptions }

procedure TSE2RegisteredExceptions.Clear;
var i: integer;
    p: PSE2RegisteredException;
begin
  for i:=FList.Count-1 downto 0 do
  begin
    p := FList.List[i];
    Dispose(p);
  end;
  FList.Clear;
end;

constructor TSE2RegisteredExceptions.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TSE2RegisteredExceptions.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2RegisteredExceptions.GetException(Ex: TClass; var isUnknown: boolean): TSE2MetaEntry;
var i: integer;
    p: PSE2RegisteredException;
begin
  isUnknown := False;
  for i := FList.Count-1 downto 0 do
  begin
    p := FList.List[i];
    if p.AClass = Ex then
    begin
      result := p^.Meta;
      exit;
    end;
  end;

  for i := FList.Count-1 downto 0 do
  begin
    p := FList.List[i];
    if Ex.InheritsFrom(p.AClass) then
    begin
      result := p^.Meta;
      exit;
    end;
  end;

  for i := 0 to FList.Count-1 do
  begin
    p := FList.List[i];
    if SameText(p^.Meta.AUnitName, C_SE2SystemUnitName) then
      if SameText(p^.Meta.Name, C_SE2ExceptUnknown) then
      begin        
        isUnknown := True;
        result := p^.Meta;
        exit;
      end;
  end;

  result := nil;
end;

procedure TSE2RegisteredExceptions.Add(Ex: TClass;
  Meta: TSE2MetaEntry);
var p: PSE2RegisteredException;
begin
  if Meta = nil then
     exit;
     
  New(p);
  p^.AClass := Ex;
  p^.Meta   := Meta;
  FList.Add(p);
end;

function ExceptionsSortCompare(Item1, Item2: PSE2RegisteredException): Integer;
begin
  if Item1^.Meta.Index < Item2^.Meta.Index then
     result := -1
  else
  if Item1^.Meta.Index > Item2^.Meta.Index then
     result := 1
  else
     result := 0;
end;

procedure TSE2RegisteredExceptions.Sort;
begin
  FList.Sort(@ExceptionsSortCompare);
end;

initialization
  FillMemorySizes;

end.
