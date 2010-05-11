unit uSE2OpCode;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2BaseTypes, uSE2Consts;

type
  TSE2ParamMode = (pmIn, pmInOut, pmResult);

  TSE2TypeIdent   = byte;

  TbtU8           = byte;
  TbtS8           = ShortInt;
  TbtU16          = word;
  TbtS16          = SmallInt;
  TbtU32          = Cardinal;
  TbtS32          = Integer;
  TbtS64          = int64;
  TbtSingle       = Single;
  TbtDouble       = double;
  TbtString       = String;
  TBtChar         = Char;
  TbtPointer      = Cardinal;
  TbtUTF8String   = UTF8String;
  TbtPChar        = PChar;
  TbtWideString   = WideString;

  PbtU8           = ^TbtU8;
  PbtS8           = ^TbtS8;
  PbtU16          = ^TbtU16;
  PbtS16          = ^TbtS16;
  PbtU32          = ^TbtU32;
  PbtS32          = ^TbtS32;
  PbtS64          = ^TbtS64;
  PbtSingle       = ^TbtSingle;
  PbtDouble       = ^TbtDouble;
  PbtPointer      = ^TbtPointer;
  PbtString       = ^TbtString;      
  PbtChar         = ^TBtChar;
  PbtUTF8String   = ^TBtUTF8String;
  PbtPChar        = ^TbtPChar;
  PbtWideString   = ^TbtWideString;

const                          
  btU8                     = 1;
  btBoolean                = btU8;
  btS8                     = 2;
  btU16                    = 3;
  btS16                    = 4;
  btU32                    = 5;
  btS32                    = 6;
  btS64                    = 7;
  btSingle                 = 8;
  btDouble                 = 9;
  btString                 = 10;
  btUTF8String             = 11;
  btWideString             = 12;
  btPChar                  = 13;
  btPointer                = 14;
  btObject                 = 15;


  btReturnAddress          = 40;
  btExtended               = 41;
  btRecord                 = 42;
  btArray                  = 43;
  btVariant                = 44;
  btChar                   = 45;
  btProcPtr                = 46;
  btStaticArray            = 47;
  btSet                    = 48;
  btCurrency               = 49;
  btInterface              = 50;
  btNotificationVariant    = 51;
  btVariable               = 52;
  btType                   = 53;
  btEnum                   = 54;
  btExtClass               = 55;
  (*
  btReturnAddress          = 0;
  btU8                     = 1;
  btBoolean                = btU8;
  btS8                     = 2;
  btU16                    = 3;
  btS16                    = 4;
  btU32                    = 5;
  btS32                    = 6;
  btS64                    = 17;
  btSingle                 = 7;
  btDouble                 = 8;
  btExtended               = 9;
  btString                 = 10;
  btRecord                 = 11;
  btArray                  = 12;
  btPointer                = 13;
  btPChar                  = 14;
  btVariant                = 16;
  btChar                   = 19;
  btProcPtr                = 21;
  btStaticArray            = 22;
  btSet                    = 23;
  btCurrency               = 24;
  btObject                 = 25;
  btInterface              = 26;
  btNotificationVariant    = 27;
  btVariable               = 28;
  btType                   = 29;
  btEnum                   = 30;
  btExtClass               = 31;
  btUTF8String             = 32;
  btWideString             = 34;  *)

  pcIsVarMask              = $80;


type
  TSE2OpCode = (// Default
                soNOOP,

                // Stack Methods
                soSTACK_INC, soSTACK_INC_COUNT,
                soSTACK_DEC, soSTACK_DEC_NODEL, soSTACK_DEC_COUNT,

                // Program execution flow
                soFLOW_GOTO, soFLOW_JIZ, soFLOW_JNZ, soFLOW_CALL, soFLOW_CALLEX, soFLOW_CALLDYN, soFLOW_CALLPTR,
                soFLOW_RET, soFLOW_PUSHRET,

                // Aritmetic operation
                soOP_OPERATION, soOP_COMPARE,
                soOP_FASTCOMPARE, soOP_FASTOPERATION,

                // Data Movement
                soDAT_COPY_TO, soDAT_COPY_FROM,
                soDAT_MOVE_TO, soDAT_MOVE_FROM,
                soDAT_CONVERT, soDAT_CHANGETYPE,

                // Data Pointer Movement
                soDAT_PTR_LOAD, soDAT_PTR_SAVE,
                soDAT_PTR_CREATE, soDAT_PTR_FREE,        

                // Data Assign
                soDAT_SetInt, soDAT_SetFloat, soDAT_SetPtr, soDAT_LOADRES, soDAT_CLEAR,

                // Special Data
                soSPEC_INCP, soSPEC_CREATE, soSPEC_DESTROY, soSPEC_UNREF, soSPEC_DECP,
                soSPEC_GetRef, soSPEC_GetProcPtr,

                // Record Data
                soREC_MAKE, soREC_FREE, soREC_COPY_TO, soREC_MARK_DEL, soREC_DEL_RECS,

                // integer increment
                soINT_INCSTATIC, soINT_INCSTACK,

                // integer decrement
                soINT_DECSTATIC, soINT_DECSTACK,

                // Safe-Blocks
                soSAFE_TRYFIN, soSAFE_TRYEX, soSAFE_BLOCK, soSAFE_TRYEND, soSAFE_SJUMP,
                soSAFE_INTER, soSAFE_STACK, soSAFE_PEX,

                // special runtime data
                soDEBUG_META, soFINIT_STACK,

                // Meta
                soMETA_PUSH, soMETA_SHARE, soMETA_CNAME, soMETA_CAST
                );

type
  PSE2OpDefault = ^TSE2OpDefault;
  TSE2OpDefault = packed record
    OpCode     : TSE2OpCode;
    Data       : array[0..7] of byte; // 8 Byte
  end;

  PSE2OpNOOP = ^TSE2OpNOOP;
  TSE2OpNOOP = packed record
    OpCode     : TSE2OpCode;
  end;

  PSE2OpSTACK_INC = ^TSE2OpSTACK_INC;
  TSE2OpSTACK_INC = packed record
    OpCode     : TSE2OpCode;
    AType      : TSE2TypeIdent;
  end;

  PSE2OpSTACK_INC_COUNT = ^TSE2OpSTACK_INC_COUNT;
  TSE2OpSTACK_INC_COUNT = packed record
    OpCode     : TSE2OpCode;
    AType      : TSE2TypeIdent;
    Count      : integer;
  end;

  PSE2OpSTACK_DEC = ^TSE2OpSTACK_DEC;
  TSE2OpSTACK_DEC = packed record
    OpCode     : TSE2OpCode;
  end;

  PSE2OpSTACK_DEC_NODEL = ^TSE2OpSTACK_DEC_NODEL;
  TSE2OpSTACK_DEC_NODEL = packed record
    OpCode     : TSE2OpCode;
  end;

  PSE2OpSTACK_DEC_COUNT = ^TSE2OpSTACK_DEC_COUNT;
  TSE2OpSTACK_DEC_COUNT = packed record
    OpCode     : integer;
    Count      : integer;
  end;

  PSE2OpFLOW_GOTO = ^TSE2OpFLOW_GOTO;
  TSE2OpFLOW_GOTO = packed record
    OpCode     : TSE2OpCode;
    Position   : cardinal;
  end;

  PSE2OpFLOW_JIZ = ^TSE2OpFLOW_JIZ;
  TSE2OpFLOW_JIZ = packed record
    OpCode     : TSE2OpCode;
    Position   : cardinal;
  end;

  PSE2OpFLOW_JNZ = ^TSE2OpFLOW_JNZ;
  TSE2OpFLOW_JNZ = packed record
    OpCode     : TSE2OpCode;
    Position   : cardinal;
  end;

  PSE2OpFLOW_CALL = ^TSE2OpFLOW_CALL;
  TSE2OpFLOW_CALL = packed record
    OpCode     : TSE2OpCode;
    Position   : cardinal;
  end;

  PSE2OpFLOW_CALLEX = ^TSE2OpFLOW_CALLEX;
  TSE2OpFLOW_CALLEX = packed record
    OpCode     : TSE2OpCode;
    Position   : {$IFDEF SEII_FPC} PtrUInt {$ELSE} cardinal {$ENDIF};
    MetaIndex  : integer;
  end;

  PSE2OpFLOW_CALLDYN = ^TSE2OpFLOW_CALLDYN;
  TSE2OpFLOW_CALLDYN = packed record
    OpCode     : TSE2OpCode;
    Offset     : cardinal;
  end;

  PSE2OpFLOW_CALLPTR = ^TSE2OpFLOW_CALLPTR;
  TSE2OpFLOW_CALLPTR = packed record
    OpCode     : TSE2OpCode;
  end;

  PSE2OpFLOW_RET = ^TSE2OpFLOW_RET;
  TSE2OpFLOW_RET = packed record
    OpCode     : TSE2OpCode;
  end;

  PSE2OpFLOW_PUSHRET = ^TSE2OpFLOW_PUSHRET;
  TSE2OpFLOW_PUSHRET = packed record
    OpCode     : TSE2OpCode;
    Position   : cardinal;
    DebugData  : cardinal;
  end;

  PSE2OpOP_OPERATION = ^TSE2OpOP_OPERATION;
  TSE2OpOP_OPERATION = packed record
    OpCode     : TSE2OpCode;
    OpType     : byte;
  end;

  PSE2OpOP_FASTOPERATION = ^TSE2OpOP_FASTOPERATION;
  TSE2OpOP_FASTOPERATION = packed record
    OpCode     : TSE2OpCode;
    OpType     : byte;
    Src1,
    Src2       : Smallint;
  end;

  PSE2OpOP_COMPARE = ^TSE2OpOP_COMPARE;
  TSE2OpOP_COMPARE = packed record
    OpCode     : TSE2OpCode;
    CompType   : byte;
  end;

  PSE2OpOP_FASTCOMPARE = ^TSE2OpOP_FASTCOMPARE;
  TSE2OpOP_FASTCOMPARE = packed record
    OpCode     : TSE2OpCode;
    CompType   : byte;
    Src1,
    Src2       : Smallint;
  end;

  PSE2OpDAT_COPY_TO = ^TSE2OpDAT_COPY_TO;
  TSE2OpDAT_COPY_TO = packed record
    OpCode     : TSE2OpCode;
    Target     : integer;
    Static     : boolean;
  end;

  PSE2OpDAT_COPY_FROM = ^TSE2OpDAT_COPY_FROM;
  TSE2OpDAT_COPY_FROM = packed record
    OpCode     : TSE2OpCode;
    Source     : integer;
    Static     : boolean;
  end;

  PSE2OpDAT_MOVE_TO = ^TSE2OpDAT_MOVE_TO;
  TSE2OpDAT_MOVE_TO = packed record
    OpCode     : TSE2OpCode;
    Target     : integer;
    Static     : boolean;
  end;

  PSE2OpDAT_MOVE_FROM = ^TSE2OpDAT_MOVE_FROM;
  TSE2OpDAT_MOVE_FROM = packed record
    OpCode     : TSE2OpCode;
    Source     : integer;
    Static     : boolean;
  end;

  PSE2OpDAT_CONVERT = ^TSE2OpDAT_CONVERT;
  TSE2OpDAT_CONVERT = packed record
    OpCode     : TSE2OpCode;
    NewType    : TSE2TypeIdent;
    Index      : integer;
  end;

  PSE2OpDAT_CHANGETYPE = ^TSE2OpDAT_CHANGETYPE;
  TSE2OpDAT_CHANGETYPE = packed record
    OpCode     : TSE2OpCode;
    NewType    : TSE2TypeIdent;
  end;

  PSE2OpDAT_SetInt = ^TSE2OpDAT_SetInt;
  TSE2OpDAT_SetInt = packed record
    OpCode     : TSE2OpCode;
    Value      : int64;
  end;

  PSE2OpDAT_SetPtr = ^TSE2OpDAT_SetPtr;
  TSE2OpDAT_SetPtr = packed record
    OpCode     : TSE2OpCode;
    Value      : Pointer;
  end;

  PSE2OpDAT_SetFloat = ^TSE2OpDAT_SetFloat;
  TSE2OpDAT_SetFloat = packed record
    OpCode     : TSE2OpCode;
    Value      : double;
  end;

  PSE2OpDAT_LOADRES = ^TSE2OpDAT_LOADRES;
  TSE2OpDAT_LOADRES = packed record
    OpCode     : TSE2OpCode;
    Index      : integer;
  end;

  PSE2OpDAT_CLEAR = ^TSE2OpDAT_CLEAR;
  TSE2OpDAT_CLEAR = packed record
    OpCode     : TSE2OpCode;
  end;

  PSE2OpDAT_PTR_LOAD = ^TSE2OpDAT_PTR_LOAD;
  TSE2OpDAT_PTR_LOAD = packed record
    OpCode     : TSE2OpCode;
    Position   : integer;
    Static     : boolean;
  end;

  PSE2OpDAT_PTR_SAVE = ^TSE2OpDAT_PTR_SAVE;
  TSE2OpDAT_PTR_SAVE = packed record
    OpCode     : TSE2OpCode;
    Position   : integer;
    Static     : boolean;
  end;

  PSE2OpDAT_PTR_CREATE = ^TSE2OpDAT_PTR_CREATE;
  TSE2OpDAT_PTR_CREATE = packed record
    OpCode     : TSE2OpCode;
    NewSize    : integer;
  end;

  PSE2OpDAT_PTR_FREE = ^TSE2OpDAT_PTR_FREE;
  TSE2OpDAT_PTR_FREE = packed record
    OpCode     : TSE2OpCode;
  end;

  PSE2OpSPEC_INCP = ^TSE2OpSPEC_INCP;
  TSE2OpSPEC_INCP = packed record
    OpCode     : TSE2OpCode;
    Offset     : integer;
    newType    : TSE2TypeIdent;
    NoRef      : boolean;
  end;

  PSE2OpSPEC_DECP = ^TSE2OpSPEC_DECP;
  TSE2OpSPEC_DECP = packed record
    OpCode     : TSE2OpCode;
    Target     : integer;
  end;

  PSE2OpSPEC_GetRef = ^TSE2OpSPEC_GetRef;
  TSE2OpSPEC_GetRef = packed record
    OpCode     : TSE2OpCode;
    Offset     : integer;
    Static     : boolean;
    UsePush    : boolean;
  end;

  PSE2OpSPEC_GetProcPtr = ^TSE2OpSPEC_GetProcPtr;
  TSE2OpSPEC_GetProcPtr = packed record
    OpCode     : TSE2OpCode;
    MetaIndex  : integer;
    HasSelf    : boolean;
  end;

  PSE2OpSPEC_CREATE = ^TSE2OpSPEC_CREATE;
  TSE2OpSPEC_CREATE = packed record
    OpCode     : TSE2OpCode;
    Variables  : integer;
    MetaIndex  : integer;
  end;

  PSE2OpSPEC_DESTROY = ^TSE2OpSPEC_DESTROY;
  TSE2OpSPEC_DESTROY = packed record
    OpCode     : TSE2OpCode;
  end;

  PSE2OpREC_MAKE = ^TSE2OpREC_MAKE;
  TSE2OpREC_MAKE = packed record
    OpCode     : TSE2OpCode;
    Variables  : integer;
    MetaIndex  : integer;
  end;

  PSE2OpREC_FREE = ^TSE2OpREC_FREE;
  TSE2OpREC_FREE = packed record
    OpCode     : TSE2OpCode;
    Offset     : integer;
  end;

  PSE2OpREC_DEL_RECS = ^TSE2OpREC_DEL_RECS;
  TSE2OpREC_DEL_RECS = packed record
    OpCode     : TSE2OpCode;
    MaxRecords : integer;
  end;

  PSE2OpREC_COPY_TO = ^TSE2OpREC_COPY_TO;
  TSE2OpREC_COPY_TO = packed record
    OpCode     : TSE2OpCode;
    Target     : integer;
    MetaIndex  : integer;
  end;

  PSE2OpSPEC_UNREF = ^TSE2OpSPEC_UNREF;
  TSE2OpSPEC_UNREF = packed record
    OpCode     : TSE2OpCode;
  end;

  PSE2OpINT_INCSTATIC = ^TSE2OpINT_INCSTATIC;
  TSE2OpINT_INCSTATIC = packed record
    OpCode     : TSE2OpCode;
    Offset     : integer;
    Value      : cardinal;
  end;

  PSE2OpINT_INCSTACK = ^TSE2OpINT_INCSTACK;
  TSE2OpINT_INCSTACK = packed record
    OpCode     : TSE2OpCode;
    Offset     : integer;
    Value      : cardinal;
  end;

  PSE2OpINT_DECSTATIC = ^TSE2OpINT_DECSTATIC;
  TSE2OpINT_DECSTATIC = packed record
    OpCode     : TSE2OpCode;
    Offset     : integer;
    Value      : cardinal;
  end;

  PSE2OpINT_DECSTACK = ^TSE2OpINT_DECSTACK;
  TSE2OpINT_DECSTACK = packed record
    OpCode     : TSE2OpCode;
    Offset     : integer;
    Value      : cardinal;
  end;

  PSE2OpSAFE_TRYFIN = ^TSE2OpSAFE_TRYFIN;
  TSE2OpSAFE_TRYFIN = packed record
    OpCode     : TSE2OpCode;
    SavePos    : cardinal;
    LeavePos   : cardinal;
  end;

  PSE2OpSAFE_TRYEX = ^TSE2OpSAFE_TRYEX;
  TSE2OpSAFE_TRYEX = packed record
    OpCode     : TSE2OpCode;
    SavePos    : cardinal;
    LeavePos   : cardinal;
  end;

  PSE2OpSAFE_BLOCK = ^TSE2OpSAFE_BLOCK;
  TSE2OpSAFE_BLOCK = packed record
    OpCode     : TSE2OpCode;
    SkipPoint  : cardinal;
  end;

  PSE2OpSAFE_TRYEND = ^TSE2OpSAFE_TRYEND;
  TSE2OpSAFE_TRYEND = packed record
    OpCode     : TSE2OpCode;
  end;

  PSE2OpSAFE_SJUMP = ^TSE2OpSAFE_SJUMP;
  TSE2OpSAFE_SJUMP = packed record
    OpCode     : TSE2OpCode;
    Target     : cardinal;
    ExitTo     : cardinal;
  end;

  PSE2OpDEBUG_META = ^TSE2OpDEBUG_META;
  TSE2OpDEBUG_META = packed record
    OpCode     : TSE2OpCode;
    MetaIndex  : cardinal;
  end;

  PSE2OpFINIT_STACK = ^TSE2OpFINIT_STACK;
  TSE2OpFINIT_STACK = packed record
    OpCode     : TSE2OpCode;
    StackSize  : integer;
  end;

  PSE2OpMETA_PUSH = ^TSE2OpMETA_PUSH;
  TSE2OpMETA_PUSH = packed record
    OpCode     : TSE2OpCode;
    MetaIndex  : integer;
  end;

  PSE2OpMETA_SHARE = ^TSE2OpMETA_SHARE;
  TSE2OpMETA_SHARE = packed record
    OpCode     : TSE2OpCode;
    MetaIndex  : integer;
  end;

  PSE2OpMETA_CAST = ^TSE2OpMETA_CAST;
  TSE2OpMETA_CAST = packed record
    OpCode     : TSE2OpCode;
    MetaIndex  : integer;
  end;

  TSE2OpCodeList = class(TSE2Object)
  private
    FList : TList;
  protected
    function  GetItem(Index: integer): PSE2OpDefault;
    function  GetCount: integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(Data: PSE2OpDefault);
    function Delete(index: integer): boolean;

    property Items[index: integer]: PSE2OpDefault read GetItem; default;
    property Count                : integer       read GetCount;
  end;

  TSE2LinkOpCode = class(TSE2Object)
  private
    FOpCode      : PSE2OpDefault;
    FCodeIndex   : string;
    FCodeHash    : integer;
    FPositionSet : boolean;
  protected
    procedure SetCodeIndex(value: string);
  public
    constructor Create(OpCode: Pointer; CodeIndex: string); reintroduce;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure SetPosition(index: integer);
    procedure AddOffset(Offset: integer; MinJumpPos: integer);
    function  GetJumpPos: integer;

    property OpCode      : PSE2OpDefault    read FOpCode      write FOpCode;
    property CodeIndex   : string           read FCodeIndex   write SetCodeIndex;
    property CodeHash    : integer          read FCodeHash;
    property PositionSet : boolean          read FPositionSet write FPositionSet;
  end;

  TSE2LinkOpCodeList = class(TSE2Object)
  private
    FList       : TList;
  protected
    function  GetItem(index: integer): TSE2LinkOpCode;
    procedure SetItem(index: integer; value: TSE2LinkOpCode);
    function  GetCount: integer;
    function  GetLast: TSE2LinkOpCode;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure Clear;

    procedure Add(Entry: TSE2LinkOpCode);
    procedure Insert(Index: integer; Entry: TSE2LinkOpCode);
    procedure SetPosition(Name: string; Index: integer);
    function  Delete(index: integer): boolean;

    property Last                 : TSE2LinkOpCode  read GetLast;
    property Items[index: integer]: TSE2LinkOpCode  read GetItem   write SetItem; default;
    property Count                : integer         read GetCount; 
  end;

  TSE2OpCodeGen = class(TSE2Object)
  private
    class function DefaultOP(OpCode: TSE2OpCode): Pointer;
  public
    class function NOOP: PSE2OpDefault;
    class function STACK_INC(AType: TSE2TypeIdent): PSE2OpDefault;
    class function STACK_DEC: PSE2OpDefault;
    class function STACK_DEC_NODEL: PSE2OpDefault;
    class function FLOW_GOTO(Position: cardinal): PSE2OpDefault;
    class function FLOW_JIZ(Postion: cardinal): PSE2OpDefault;
    class function FLOW_JNZ(Position: cardinal): PSE2OpDefault;
    class function FLOW_CALL(Position: cardinal): PSE2OpDefault;
    class function FLOW_CALLEX(Position: cardinal; MetaIndex: integer): PSE2OpDefault;
    class function FLOW_CALLDYN(Offset: cardinal): PSE2OpDefault;
    class function FLOW_CALLPTR: PSE2OpDefault;
    class function FLOW_RET: PSE2OpDefault;
    class function FLOW_PUSHRET(Target, DebugIndex: integer): PSE2OpDefault;
    class function OP_OPERATION(OpType: byte): PSE2OpDefault;
    class function OP_COMPARE(CompType: byte): PSE2OpDefault;
    class function DAT_COPY_TO(Target: integer; Static: boolean): PSE2OpDefault;
    class function DAT_COPY_FROM(Source: integer; Static: boolean): PSE2OpDefault;
    class function DAT_MOVE_TO(Target: integer; Static: boolean): PSE2OpDefault;
    class function DAT_MOVE_FROM(Source: integer; Static: boolean): PSE2OpDefault;
    class function DAT_SetInt(value: int64): PSE2OpDefault;
    class function DAT_SetFloat(value: double): PSE2OpDefault;
    class function DAT_SetPtr(value: pointer): PSE2OpDefault;
    class function DAT_LOADRES(index: integer): PSE2OpDefault;
    class function DAT_CLEAR: PSE2OpDefault;
    class function DAT_CONVERT(NewType: TSE2TypeIdent; Index: integer): PSE2OpDefault;
    class function DAT_CHANGETYPE(NewType: TSE2TypeIdent): PSE2OpDefault;
    class function INT_INCSTATIC(Position, value: integer): PSE2OpDefault;
    class function INT_INCSTACK(Position, value: integer): PSE2OpDefault;  
    class function INT_DECSTATIC(Position, value: integer): PSE2OpDefault;
    class function INT_DECSTACK(Position, value: integer): PSE2OpDefault;

    class function SPEC_INCP(Offset: integer; newType: TSE2TypeIdent; NoRef: boolean): PSE2OpDefault;
    class function SPEC_CREATE(Variables: integer; MetaIndex: integer): PSE2OpDefault;
    class function SPEC_DESTROY: PSE2OpDefault;                                     
    class function SPEC_UNREF: PSE2OpDefault;
    class function SPEC_DECP(Target: integer): PSE2OpDefault;
    class function SPEC_GetRef(Offset: integer; Static, UsePush: boolean): PSE2OpDefault;
    class function SPEC_GetProcPtr(MetaIndex: integer; HasSelf: boolean): PSE2OpDefault;

    class function REC_MAKE(Variables: integer; MetaIndex: integer): PSE2OpDefault;
    class function REC_FREE(Offset: integer): PSE2OpDefault;
    class function REC_COPY_TO(Target: integer; Meta: integer): PSE2OpDefault;
    class function REC_MARK_DEL: PSE2OpDefault;
    class function REC_DEL_RECS(MaxRecords: integer): PSE2OpDefault;

    class function SAFE_TRYFIN(SavePos, LeavePos: integer): PSE2OpDefault;
    class function SAFE_TRYEX(SavePos, LeavePos: integer): PSE2OpDefault;
    class function SAFE_BLOCK(SkipPoint: cardinal): PSE2OpDefault;
    class function SAFE_TRYEND: PSE2OpDefault;
    class function SAFE_SJUMP(Target, ExitTo: cardinal): PSE2OpDefault;
    class function SAFE_INTER: PSE2OpDefault;
    class function SAFE_STACK: PSE2OpDefault;
    class function SAFE_PEX: PSE2OpDefault;

    class function DEBUG_META(index: cardinal): PSE2OpDefault;
    class function FINIT_STACK(stackSize: integer): PSE2OpDefault;

    class function META_CNAME: PSE2OpDefault;
    class function META_PUSH(index: integer): PSE2OpDefault;
    class function META_SHARE(index: integer): PSE2OpDefault;
    class function META_CAST(index: integer): PSE2OpDefault;
  end;

  TSE2ParamHelper = class(TSE2Object)
    class function MakeParamData(ParamType: TSE2TypeIdent; IsVarParameter: boolean): byte;
    class function IsVarParam(ParamData: byte): boolean;
    class function GetParamType(ParamData: byte): byte;
  end;

implementation

uses SysUtils;

{ TSE2OpCodeGen }

class function TSE2OpCodeGen.DAT_CHANGETYPE(
  NewType: TSE2TypeIdent): PSE2OpDefault;
begin
  result := DefaultOP(soDAT_CHANGETYPE);
  PSE2OpDAT_CHANGETYPE(result).NewType := NewType;
end;

class function TSE2OpCodeGen.DAT_CONVERT(
  NewType: TSE2TypeIdent; Index: integer): PSE2OpDefault;
begin
  result := DefaultOP(soDAT_CONVERT);
  PSE2OpDAT_CONVERT(result)^.NewType := NewType;
  PSE2OpDAT_CONVERT(result)^.Index   := Index;
end;

class function TSE2OpCodeGen.DAT_COPY_FROM(Source: integer;
  Static: boolean): PSE2OpDefault;
begin
  result := DefaultOP(soDAT_COPY_FROM);
  PSE2OpDAT_COPY_FROM(result)^.Source := Source;
  PSE2OpDAT_COPY_FROM(result)^.Static := Static;

  if not Static then
     Assert(Source < 1);
end;

class function TSE2OpCodeGen.DAT_COPY_TO(Target: integer;
  Static: boolean): PSE2OpDefault;
begin
  result := DefaultOP(soDAT_COPY_TO);
  PSE2OpDAT_COPY_TO(result)^.Target := Target;
  PSE2OpDAT_COPY_TO(result)^.Static := Static;
end;

class function TSE2OpCodeGen.DAT_LOADRES(
  index: integer): PSE2OpDefault;
begin
  result := DefaultOP(soDAT_LOADRES);
  PSE2OpDAT_LOADRES(result)^.Index := index;
end;

class function TSE2OpCodeGen.DAT_MOVE_FROM(Source: integer;
  Static: boolean): PSE2OpDefault;
begin
  result := DefaultOP(soDAT_MOVE_FROM);
  PSE2OpDAT_MOVE_FROM(result)^.Source := Source;
  PSE2OpDAT_MOVE_FROM(result)^.Static := Static;
end;

class function TSE2OpCodeGen.DAT_MOVE_TO(Target: integer;
  Static: boolean): PSE2OpDefault;
begin
  result := DefaultOP(soDAT_MOVE_TO);
  PSE2OpDAT_MOVE_TO(result)^.Target := Target;
  PSE2OpDAT_MOVE_TO(result)^.Static := Static;
end;

class function TSE2OpCodeGen.DAT_SetFloat(
  value: double): PSE2OpDefault;
begin
  result := DefaultOP(soDAT_SetFloat);
  PSE2OpDAT_SetFloat(result)^.Value := value;
end;

class function TSE2OpCodeGen.DAT_SetInt(value: int64): PSE2OpDefault;
begin
  result := DefaultOP(soDAT_SetInt);
  PSE2OpDAT_SetInt(result)^.Value := value;
end;

class function TSE2OpCodeGen.DAT_SetPtr(value: pointer): PSE2OpDefault;
begin
  result := DefaultOP(soDAT_SetPtr);
  PSE2OpDAT_SetPtr(result)^.Value := value;
end;

class function TSE2OpCodeGen.DefaultOP(OpCode: TSE2OpCode): Pointer;
var p: PSE2OpDefault;
begin
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  GetMem(p, SizeOf(TSE2OpDefault));
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  FillChar(p^, SizeOf(TSE2OpDefault), 0);
  p^.OpCode := OpCode;
  result := p;
end;

class function TSE2OpCodeGen.FLOW_CALL(
  Position: cardinal): PSE2OpDefault;
begin
  result := DefaultOP(soFLOW_CALL);
  PSE2OpFLOW_CALL(result)^.Position := Position;
end;

class function TSE2OpCodeGen.FLOW_CALLEX(Position: cardinal;
  MetaIndex: integer): PSE2OpDefault;
begin
  result := DefaultOP(soFLOW_CALLEX);
  PSE2OpFLOW_CALLEX(result)^.Position   := Position;
  PSE2OpFLOW_CALLEX(result)^.MetaIndex  := MetaIndex;
end;

class function TSE2OpCodeGen.FLOW_CALLDYN(Offset: Cardinal): PSE2OpDefault;
begin
  result := DefaultOP(soFLOW_CALLDYN);
  PSE2OpFLOW_CALLDYN(result)^.Offset   := Offset;
end;

class function TSE2OpCodeGen.FLOW_GOTO(
  Position: cardinal): PSE2OpDefault;
begin
  result := DefaultOP(soFLOW_GOTO);
  PSE2OpFLOW_GOTO(result)^.Position := Position;
end;

class function TSE2OpCodeGen.FLOW_JIZ(Postion: cardinal): PSE2OpDefault;
begin
  result := DefaultOP(soFLOW_JIZ);
  PSE2OpFLOW_JIZ(result)^.Position := Postion;
end;

class function TSE2OpCodeGen.FLOW_JNZ(Position: cardinal): PSE2OpDefault;
begin
  result := DefaultOP(soFLOW_JNZ);
  PSE2OpFLOW_JNZ(result)^.Position := Position;
end;

class function TSE2OpCodeGen.FLOW_RET: PSE2OpDefault;
begin
  result := DefaultOP(soFLOW_RET);
end;      

class function TSE2OpCodeGen.FLOW_PUSHRET(Target, DebugIndex: integer): PSE2OpDefault;
begin
  result := DefaultOP(soFLOW_PUSHRET);
  PSE2OpFLOW_PUSHRET(result)^.Position  := Target;
  PSE2OpFLOW_PUSHRET(result)^.DebugData := DebugIndex;
end;

class function TSE2OpCodeGen.NOOP: PSE2OpDefault;
begin
  result := DefaultOP(soNOOP);
end;

class function TSE2OpCodeGen.OP_COMPARE(CompType: byte): PSE2OpDefault;
begin
  result := DefaultOP(soOP_COMPARE);
  PSE2OpOP_COMPARE(result)^.CompType := CompType;
end;

class function TSE2OpCodeGen.OP_OPERATION(
  OpType: byte): PSE2OpDefault;
begin
  result := DefaultOP(soOP_OPERATION);
  PSE2OpOP_OPERATION(result)^.OpType := OpType;
end;

class function TSE2OpCodeGen.SAFE_BLOCK(
  SkipPoint: cardinal): PSE2OpDefault;
begin
  result := DefaultOP(soSAFE_BLOCK);
  PSE2OpSAFE_BLOCK(result)^.SkipPoint := SkipPoint;
end;

class function TSE2OpCodeGen.SAFE_SJUMP(Target,
  ExitTo: cardinal): PSE2OpDefault;
begin
  result := DefaultOP(soSAFE_SJUMP);
  PSE2OpSAFE_SJUMP(result)^.Target := Target;
  PSE2OpSAFE_SJUMP(result)^.ExitTo := ExitTo;
end;

class function TSE2OpCodeGen.SAFE_TRYFIN(SavePos, LeavePos: integer): PSE2OpDefault;
begin
  result := DefaultOP(soSAFE_TRYFIN);
  PSE2OpSAFE_TRYFIN(result)^.SavePos  := SavePos;
  PSE2OpSAFE_TRYFIN(result)^.LeavePos := LeavePos;
end;

class function TSE2OpCodeGen.SAFE_TRYEND: PSE2OpDefault;
begin
  result := DefaultOP(soSAFE_TRYEND);
end;

class function TSE2OpCodeGen.SPEC_INCP(Offset: integer; newType: TSE2TypeIdent; NoRef: boolean): PSE2OpDefault;
begin
  result := DefaultOP(soSPEC_INCP);
  PSE2OpSPEC_INCP(result)^.Offset := Offset;
  PSE2OpSPEC_INCP(result)^.newType := newType;    
  PSE2OpSPEC_INCP(result)^.NoRef := NoRef;
end;

class function TSE2OpCodeGen.STACK_DEC: PSE2OpDefault;
begin
  result := DefaultOP(soSTACK_DEC);
end;

class function TSE2OpCodeGen.STACK_DEC_NODEL: PSE2OpDefault;
begin
  result := DefaultOP(soSTACK_DEC_NODEL);
end;

class function TSE2OpCodeGen.STACK_INC(AType: TSE2TypeIdent): PSE2OpDefault;
begin
  result := DefaultOP(soSTACK_INC);
  PSE2OpSTACK_INC(result)^.AType := AType;
end;

class function TSE2OpCodeGen.SAFE_TRYEX(SavePos,
  LeavePos: integer): PSE2OpDefault;
begin
  result := DefaultOP(soSAFE_TRYEX);
  PSE2OpSAFE_TRYEX(result)^.SavePos  := SavePos;
  PSE2OpSAFE_TRYEX(result)^.LeavePos := LeavePos;
end;

class function TSE2OpCodeGen.DAT_CLEAR: PSE2OpDefault;
begin
  result := DefaultOP(soDAT_CLEAR);
end;

class function TSE2OpCodeGen.INT_DECSTACK(Position,
  value: integer): PSE2OpDefault;
begin
  result := DefaultOP(soINT_DECSTACK);
  PSE2OpINT_DECSTACK(Result).Offset := Position;
  PSE2OpINT_DECSTACK(result).Value  := value;
end;

class function TSE2OpCodeGen.INT_DECSTATIC(Position,
  value: integer): PSE2OpDefault;
begin
  result := DefaultOP(soINT_DECSTATIC);
  PSE2OpINT_DECSTATIC(Result).Offset := Position;
  PSE2OpINT_DECSTATIC(result).Value  := value;
end;

class function TSE2OpCodeGen.INT_INCSTACK(Position,
  value: integer): PSE2OpDefault;
begin
  result := DefaultOP(soINT_INCSTACK);
  PSE2OpINT_INCSTACK(Result).Offset := Position;
  PSE2OpINT_INCSTACK(result).Value  := value;
end;

class function TSE2OpCodeGen.INT_INCSTATIC(Position,
  value: integer): PSE2OpDefault;
begin                     
  result := DefaultOP(soINT_INCSTATIC);
  PSE2OpINT_INCSTATIC(Result).Offset := Position;
  PSE2OpINT_INCSTATIC(result).Value  := value;
end;

class function TSE2OpCodeGen.SPEC_CREATE(Variables,
  MetaIndex: integer): PSE2OpDefault;
begin
  result := DefaultOP(soSPEC_CREATE);
  PSE2OpSPEC_CREATE(result).Variables := Variables;
  PSE2OpSPEC_CREATE(result).MetaIndex := MetaIndex;
end;

class function TSE2OpCodeGen.SPEC_DESTROY: PSE2OpDefault;
begin
  result := DefaultOP(soSPEC_DESTROY);
end;

class function TSE2OpCodeGen.SPEC_UNREF: PSE2OpDefault;
begin                             
  result := DefaultOP(soSPEC_UNREF);
end;

class function TSE2OpCodeGen.SPEC_DECP(Target: integer): PSE2OpDefault;
begin
  result := DefaultOP(soSPEC_DECP);
  PSE2OpSPEC_DECP(result).Target := Target;
end;

class function TSE2OpCodeGen.SPEC_GetRef(Offset: integer; Static, UsePush: boolean): PSE2OpDefault;
begin
  result := DefaultOP(soSPEC_GetRef);
  PSE2OpSPEC_GetRef(result)^.Offset  := Offset;
  PSE2OpSPEC_GetRef(result)^.Static  := Static;
  PSE2OpSPEC_GetRef(result)^.UsePush := UsePush;
end;

class function TSE2OpCodeGen.REC_FREE(Offset: integer): PSE2OpDefault;
begin   
  result := DefaultOP(soREC_FREE);
  PSE2OpREC_FREE(result)^.Offset := Offset;
end;

class function TSE2OpCodeGen.REC_MAKE(Variables,
  MetaIndex: integer): PSE2OpDefault;
begin              
  result := DefaultOP(soREC_MAKE);
  PSE2OpREC_MAKE(result)^.Variables := Variables;
  PSE2OpREC_MAKE(result)^.MetaIndex := MetaIndex;
end;

class function TSE2OpCodeGen.REC_COPY_TO(Target: integer;
  Meta: integer): PSE2OpDefault;
begin      
  result := DefaultOP(soREC_COPY_TO);
  PSE2OpREC_COPY_TO(result)^.Target := Target;
  PSE2OpREC_COPY_TO(result)^.MetaIndex   := Meta;
end;

class function TSE2OpCodeGen.REC_MARK_DEL: PSE2OpDefault;
begin
  result := DefaultOP(soREC_MARK_DEL);
end;

class function TSE2OpCodeGen.FLOW_CALLPTR: PSE2OpDefault;
begin
  result := DefaultOP(soFLOW_CALLPTR);
end;

class function TSE2OpCodeGen.SPEC_GetProcPtr(MetaIndex: integer;
  HasSelf: boolean): PSE2OpDefault;
begin
  result := DefaultOP(soSPEC_GetProcPtr);
  PSE2OpSPEC_GetProcPtr(result)^.MetaIndex := MetaIndex;
  PSE2OpSPEC_GetProcPtr(result)^.HasSelf   := HasSelf;
end;

class function TSE2OpCodeGen.DEBUG_META(index: cardinal): PSE2OpDefault;
begin
  result := DefaultOP(soDEBUG_META);
  PSE2OpDEBUG_META(result)^.MetaIndex := index;
end;

class function TSE2OpCodeGen.FINIT_STACK(
  stackSize: integer): PSE2OpDefault;
begin
  result := DefaultOP(soFINIT_STACK);
  PSE2OpFINIT_STACK(result)^.StackSize := stackSize;
end;

class function TSE2OpCodeGen.REC_DEL_RECS(MaxRecords: integer): PSE2OpDefault;
begin
  result := DefaultOP(soREC_DEL_RECS);
  PSE2OpREC_DEL_RECS(result)^.MaxRecords := MaxRecords;
end;

class function TSE2OpCodeGen.META_PUSH(index: integer): PSE2OpDefault;
begin
  result := DefaultOP(soMETA_PUSH);
  PSE2OpMETA_PUSH(result)^.MetaIndex := index;
end;

class function TSE2OpCodeGen.META_SHARE(index: integer): PSE2OpDefault;
begin
  result := DefaultOP(soMETA_SHARE);
  PSE2OpMETA_SHARE(result)^.MetaIndex := index;
end;

class function TSE2OpCodeGen.META_CNAME: PSE2OpDefault;
begin
  result := DefaultOP(soMETA_CNAME);
end;       

class function TSE2OpCodeGen.META_CAST(index: integer): PSE2OpDefault;
begin
  result := DefaultOP(soMETA_CAST);
  PSE2OpMETA_CAST(result)^.MetaIndex := index;
end;

class function TSE2OpCodeGen.SAFE_INTER: PSE2OpDefault;
begin
  result := DefaultOP(soSAFE_INTER);
end;

class function TSE2OpCodeGen.SAFE_STACK: PSE2OpDefault;
begin
  result := DefaultOP(soSAFE_STACK)
end;

class function TSE2OpCodeGen.SAFE_PEX: PSE2OpDefault;
begin
  result := DefaultOP(soSAFE_PEX);
end;

{ TSE2OpCodeList }

procedure TSE2OpCodeList.Add(Data: PSE2OpDefault);
begin
  FList.Add(Data);
end;

procedure TSE2OpCodeList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Delete(i);
end;

constructor TSE2OpCodeList.Create;
begin
  inherited;
  FList := TList.Create;
end;

function TSE2OpCodeList.Delete(index: integer): boolean;
var p: PSE2OpDefault;
begin
  if (index < 0) or (index >= FList.Count) then
     result := False
  else
  begin
    p := FList[index];
    FList.Delete(index);
    Dispose(p);
    result := True;
  end;
end;

destructor TSE2OpCodeList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2OpCodeList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2OpCodeList.GetItem(Index: integer): PSE2OpDefault;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := FList[index];

end;

{ TSE2LinkOpCode }

constructor TSE2LinkOpCode.Create(OpCode: Pointer;
  CodeIndex: string);
begin
  inherited Create;
  FOpCode    := OpCode;
  SetCodeIndex(CodeIndex);
end;

destructor TSE2LinkOpCode.Destroy;
begin
  if FOpCode <> nil then
     FreeMem(FOpCode);
     //Dispose(FOpCode);
  inherited;
end;

procedure TSE2LinkOpCode.SetCodeIndex(value: string);
begin
  FCodeIndex := value;
  FCodeHash  := MakeHash(value);
end;   

{$Warnings off}
function TSE2LinkOpCode.GetJumpPos: integer;

  function Max(i1, i2: integer): integer;
  begin
    result := i1;
    if i2 > result then
       result := i2;
  end;

begin
  result := 0;
  if FOpCode = nil then
     exit;

  case FOpCode.OpCode of
  soFLOW_GOTO       : result := PSE2OpFLOW_GOTO(FOpCode).Position;
  soFLOW_JIZ        : result := PSE2OpFLOW_JIZ(FOpCode).Position;
  soFLOW_JNZ        : result := PSE2OpFLOW_JNZ(FOpCode).Position;
  soFLOW_CALL       : result := PSE2OpFLOW_CALL(FOpCode).Position;
  soFLOW_PUSHRET    : result := PSE2OpFLOW_PUSHRET(FOpCode).Position;

  soSAFE_SJUMP      : result := Max(PSE2OpSAFE_SJUMP(FOpCode).Target, PSE2OpSAFE_SJUMP(FOpCode).ExitTo);
  soSAFE_TRYEX      : result := Max(PSE2OpSAFE_TRYEX(FOpCode).SavePos, PSE2OpSAFE_TRYEX(FOpCode).LeavePos);
  soSAFE_TRYFIN     : result := Max(PSE2OpSAFE_TRYFIN(FOpCode).SavePos, PSE2OpSAFE_TRYFIN(FOpCode).LeavePos);
  soSAFE_BLOCK      : result := PSE2OpSAFE_BLOCK(FOpCode)^.SkipPoint;
  end;
end;

{$IFDEF SEII_FPC} {$HINTS OFF} {$ENDIF}
procedure TSE2LinkOpCode.AddOffset(Offset: integer; MinJumpPos: integer);
begin
  if FOpCode = nil then
     exit;

  case FOpCode.OpCode of
  soFLOW_GOTO       : PSE2OpFLOW_GOTO(FOpCode).Position   := PSE2OpFLOW_GOTO(FOpCode).Position + Offset;
  soFLOW_JIZ        : PSE2OpFLOW_JIZ(FOpCode).Position    := PSE2OpFLOW_JIZ(FOpCode).Position + Offset;
  soFLOW_JNZ        : PSE2OpFLOW_JNZ(FOpCode).Position    := PSE2OpFLOW_JNZ(FOpCode).Position + Offset;
  soFLOW_CALL       : PSE2OpFLOW_CALL(FOpCode).Position   := PSE2OpFLOW_CALL(FOpCode).Position + Offset;
  soFLOW_PUSHRET    : PSE2OpFLOW_PUSHRET(FOpCode).Position:= PSE2OpFLOW_CALL(FOpCode).Position + Offset;

  soSAFE_SJUMP      :
      begin
        if PSE2OpSAFE_SJUMP(FOpCode)^.Target > MinJumpPos then
           PSE2OpSAFE_SJUMP(FOpCode)^.Target := PSE2OpSAFE_SJUMP(FOpCode)^.Target  + Offset;

        if PSE2OpSAFE_SJUMP(FOpCode)^.ExitTo > MinJumpPos then
           PSE2OpSAFE_SJUMP(FOpCode)^.ExitTo := PSE2OpSAFE_SJUMP(FOpCode)^.ExitTo  + Offset;
      end;
  soSAFE_TRYEX      :
      begin
        if PSE2OpSAFE_TRYEX(FOpCode)^.SavePos > MinJumpPos then
           PSE2OpSAFE_TRYEX(FOpCode)^.SavePos  := PSE2OpSAFE_TRYEX(FOpCode)^.SavePos  + Offset;
        if PSE2OpSAFE_TRYEX(FOpCode)^.LeavePos > MinJumpPos then
           PSE2OpSAFE_TRYEX(FOpCode)^.LeavePos := PSE2OpSAFE_TRYEX(FOpCode)^.LeavePos + Offset;
      end;
  soSAFE_TRYFIN     :
      begin
        if PSE2OpSAFE_TRYFIN(FOpCode)^.SavePos > MinJumpPos then
           PSE2OpSAFE_TRYFIN(FOpCode)^.SavePos  := PSE2OpSAFE_TRYFIN(FOpCode)^.SavePos  + Offset;  
        if PSE2OpSAFE_TRYFIN(FOpCode)^.LeavePos > MinJumpPos then
           PSE2OpSAFE_TRYFIN(FOpCode)^.LeavePos := PSE2OpSAFE_TRYFIN(FOpCode)^.LeavePos + Offset;
      end;
  soSAFE_BLOCK     :
      begin
        PSE2OpSAFE_BLOCK(FOpCode)^.SkipPoint  := PSE2OpSAFE_BLOCK(FOpCode)^.SkipPoint + Offset;
      end;
  end;
end;        
{$IFDEF SEII_FPC} {$HINTS ON} {$ENDIF}
{$Warnings on}

procedure TSE2LinkOpCode.SetPosition(index: integer);
//var tmp: integer;
begin
  if FOpCode = nil then
     exit;

  case FOpCode.OpCode of
  soFLOW_GOTO       : PSE2OpFLOW_GOTO(FOpCode).Position   := index;
  soFLOW_JIZ        : PSE2OpFLOW_JIZ(FOpCode).Position    := index;
  soFLOW_JNZ        : PSE2OpFLOW_JNZ(FOpCode).Position    := index;
  soFLOW_CALL       : PSE2OpFLOW_CALL(FOpCode).Position   := index;
  soDAT_COPY_TO     : if PSE2OpDAT_COPY_TO(FOpCode).Static then
                         PSE2OpDAT_COPY_TO(FOpCode).Target   := index;
  soDAT_COPY_FROM   : if PSE2OpDAT_COPY_FROM(FOpCode).Static then
                         PSE2OpDAT_COPY_FROM(FOpCode).Source := index;
  soDAT_MOVE_TO     : if PSE2OpDAT_MOVE_TO(FOpCode).Static then
                         PSE2OpDAT_MOVE_TO(FOpCode).Target   := index;
  soDAT_MOVE_FROM   : if PSE2OpDAT_MOVE_FROM(FOpCode).Static then
                         PSE2OpDAT_MOVE_FROM(FOpCode).Source := index;
  soDAT_LOADRES     : PSE2OpDAT_LOADRES(FOpCode).Index := index;
  soSPEC_GetRef     : if PSE2OpSPEC_GetRef(FOpCode).Static then
                         PSE2OpSPEC_GetRef(FOpCode).Offset := index;
  soREC_COPY_TO     : if PSE2OpREC_COPY_TO(FOpCode).Target >= 0 then
                         PSE2OpREC_COPY_TO(FOpCode).Target   := index;
  soINT_INCSTATIC   : PSE2OpINT_INCSTATIC(FOpCode).Offset := index;
  soINT_DECSTATIC   : PSE2OpINT_DECSTATIC(FOpCode).Offset := index;
  soOP_FASTCOMPARE  :
      begin
        if PSE2OpOP_FASTCOMPARE(FOpCode).Src1 >= 0 then
           PSE2OpOP_FASTCOMPARE(FOpCode).Src1 := index
        else
        if PSE2OpOP_FASTCOMPARE(FOpCode).Src2 >= 0 then
           PSE2OpOP_FASTCOMPARE(FOpCode).Src2 := index;
      end;
  soOP_FASTOPERATION :
      begin
        if PSE2OpOP_FASTOPERATION(FOpCode).Src1 >= 0 then
           PSE2OpOP_FASTOPERATION(FOpCode).Src1 := index;
        if PSE2OpOP_FASTOPERATION(FOpCode).Src2 >= 0 then
           PSE2OpOP_FASTOPERATION(FOpCode).Src2 := index;
      end;
  end;
  FPositionSet := True;
end;

procedure TSE2LinkOpCode.LoadFromStream(Stream: TStream);
var version  : byte;
    OpCode   : PSE2OpDefault;
begin
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  case version of
  1 :
      begin
        TSE2StreamHelper.ReadString(Stream, FCodeIndex);
        Stream.Read(FCodeHash, SizeOf(FCodeHash));
        Stream.Read(FPositionSet, SizeOf(FPositionSet));

        {$IFDEF SEII_FPC}
          {$HINTS OFF}
        {$ENDIF}
        GetMem(OpCode, SizeOf(TSE2OpDefault));
        {$IFDEF SEII_FPC}
          {$HINTS ON}
        {$ENDIF}
        Stream.Read(OpCode^, SizeOf(TSE2OpDefault));
        FOpCode := OpCode;
      end;
  else raise ESE2InvalidDataStream.Create('Invalid Stream Version');
  end;
end;

procedure TSE2LinkOpCode.SaveToStream(Stream: TStream);
var version  : byte;
begin
  version := 1;
  Stream.Write(version, SizeOf(version));

  TSE2StreamHelper.WriteString(Stream, FCodeIndex);
  Stream.Write(FCodeHash, SizeOf(FCodeHash));
  Stream.Write(FPositionSet, SizeOf(FPositionSet));

  Stream.Write(FOpCode^, SizeOf(TSE2OpDefault));
end;

{ TSE2LinkOpCodeList }

procedure TSE2LinkOpCodeList.Add(Entry: TSE2LinkOpCode);
begin
  FList.Add(Entry);
end;

procedure TSE2LinkOpCodeList.Insert(Index: integer; Entry: TSE2LinkOpCode);
begin
  FList.Insert(Index, Entry);
end;

procedure TSE2LinkOpCodeList.SetItem(index: integer;
  value: TSE2LinkOpCode);
begin
  FList[index] := value;
end;

procedure TSE2LinkOpCodeList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Delete(i);
end;

constructor TSE2LinkOpCodeList.Create;
begin
  inherited;
  FList := TList.Create;
end;

function TSE2LinkOpCodeList.Delete(index: integer): boolean;
var p: TSE2LinkOpCode;
begin
  if (index < 0) or (index >= FList.Count) then
     result := False
  else
  begin
    p := FList[index];
    FList.Delete(index);
    p.Free;
    result := True;
  end;
end;

destructor TSE2LinkOpCodeList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2LinkOpCodeList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2LinkOpCodeList.GetItem(index: integer): TSE2LinkOpCode;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := FList[index];
end;

procedure TSE2LinkOpCodeList.SetPosition(Name: string; Index: integer);
var i: integer;
    p: TSE2LinkOpCode;
begin
  for i:=FList.Count-1 downto 0 do
  begin
    p := FList[i];
    if StringIdentical(p.CodeIndex, Name) then
       p.SetPosition(Index);
  end;
end;

procedure TSE2LinkOpCodeList.LoadFromStream(Stream: TStream);
var version : byte;
    i, count: integer;
    p       : TSE2LinkOpCode;
begin
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(version, SizeOf(version)) < SizeOF(version) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  case version of
  1 :
      begin
        {$IFDEF SEII_FPC}
          {$HINTS OFF}
        {$ENDIF}
        Stream.Read(count, SizeOf(count));
        {$IFDEF SEII_FPC}
          {$HINTS ON}
        {$ENDIF}
        for i:=0 to count-1 do
        begin
          p := TSE2LinkOpCode.Create(nil, '');
          FList.Add(p);
          p.LoadFromStream(Stream);
        end;
      end;
  else raise ESE2InvalidDataStream.Create('Invalid Stream Version');
  end;
end;

procedure TSE2LinkOpCodeList.SaveToStream(Stream: TStream);
var version : byte;
    i, count: integer;
    p       : TSE2LinkOpCode;
begin
  version := 1;
  STream.Write(version, SizeOf(version));

  count := SElf.Count;
  Stream.Write(count, SizeOf(count));
  for i:=0 to count-1 do
  begin
    p := FList[i];
    p.SaveToStream(Stream);
  end;                    
end;

function TSE2LinkOpCodeList.GetLast: TSE2LinkOpCode;
begin
  result := Items[Count-1];
end;

{ TSE2ParamHelper }

class function TSE2ParamHelper.GetParamType(ParamData: byte): byte;
begin            
  result := ParamData and (not pcIsVarMask);
end;

class function TSE2ParamHelper.IsVarParam(ParamData: byte): boolean;
begin
  result := (ParamData and pcIsVarMask) = pcIsVarMask;
end;

class function TSE2ParamHelper.MakeParamData(ParamType: TSE2TypeIdent;
  IsVarParameter: boolean): byte;
begin
  result := ParamType;
  if IsVarParameter then
     result := result or pcIsVarMask;
end;

end.
