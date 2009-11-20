unit uSE2RunOperation;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2RunType, uSE2OpCode, uSE2MemoryManager;

type
  TSE2VarOperation = class(TObject)
  private
    FVarHelp : TSE2VarHelper;
  public
    constructor Create(VarHelper: TSE2VarHelper);

    procedure Negation(Data: PSE2VarData);
    procedure Addition(Param1, Param2: PSE2VarData);
    procedure Substract(Param1, Param2: PSE2VarData);
    procedure Multiply(Param1, Param2: PSE2VarData);
    procedure Divide(Param1, Param2: PSE2VarData);
    procedure BitAnd(Param1, Param2: PSE2VarData);
    procedure BitOr(Param1, Param2: PSE2VarData);
    procedure BitXor(Param1, Param2: PSE2VarData);
    procedure BitShr(Param1, Param2: PSE2VarData);
    procedure BitShl(Param1, Param2: PSE2VarData);
    procedure DivideMod(Param1, Param2: PSE2VarData);
    procedure BitNot(Data: PSE2VarData);
    procedure BooleanNot(Data: PSE2VarData);
  end;

  TSE2VarCompare = class(TObject)
  private
    FVarHelp : TSE2VarHelper;
  public
    constructor Create(VarHelper: TSE2VarHelper);

    function  Equal(Param1, Param2: PSE2VarData): boolean;
    function  Smaller(Param1, Param2: PSE2VarData): boolean;
    function  SmallerEqual(Param1, Param2: PSE2VarData): boolean;
    function  Bigger(Param1, Param2: PSE2VarData): boolean;
    function  BiggerEqual(Param1, Param2: PSE2VarData): boolean;
    function  UnEqual(Param1, Param2: PSE2VarData): boolean;
  end;

implementation

{ TSE2VarOperation }

{$Warnings off}

procedure TSE2VarOperation.Addition(Param1, Param2: PSE2VarData);
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu8^       := Param1^.tu8^ + Param2^.tu8^;
        btS8        : Param1^.tu8^       := Param1^.tu8^ + Param2^.ts8^;
        btU16       : Param1^.tu8^       := Param1^.tu8^ + Param2^.tu16^;
        btS16       : Param1^.tu8^       := Param1^.tu8^ + Param2^.ts16^;
        btU32       : Param1^.tu8^       := Param1^.tu8^ + Param2^.tu32^;
        btS32       : Param1^.tu8^       := Param1^.tu8^ + Param2^.ts32^;
        btS64       : Param1^.tu8^       := Param1^.tu8^ + Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu8^ + Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu8^ + Param2^.tDouble^);
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts8^       := Param1^.ts8^ + Param2^.tu8^;
        btS8        : Param1^.ts8^       := Param1^.ts8^ + Param2^.ts8^;
        btU16       : Param1^.ts8^       := Param1^.ts8^ + Param2^.tu16^;
        btS16       : Param1^.ts8^       := Param1^.ts8^ + Param2^.ts16^;
        btU32       : Param1^.ts8^       := Param1^.ts8^ + Param2^.tu32^;
        btS32       : Param1^.ts8^       := Param1^.ts8^ + Param2^.ts32^;
        btS64       : Param1^.ts8^       := Param1^.ts8^ + Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts8^ + Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts8^ + Param2^.tDouble^);
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu16^       := Param1^.tu16^ + Param2^.tu8^;
        btS8        : Param1^.tu16^       := Param1^.tu16^ + Param2^.ts8^;
        btU16       : Param1^.tu16^       := Param1^.tu16^ + Param2^.tu16^;
        btS16       : Param1^.tu16^       := Param1^.tu16^ + Param2^.ts16^;
        btU32       : Param1^.tu16^       := Param1^.tu16^ + Param2^.tu32^;
        btS32       : Param1^.tu16^       := Param1^.tu16^ + Param2^.ts32^;
        btS64       : Param1^.tu16^       := Param1^.tu16^ + Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu16^ + Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu16^ + Param2^.tDouble^);
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts16^       := Param1^.ts16^ + Param2^.tu8^;
        btS8        : Param1^.ts16^       := Param1^.ts16^ + Param2^.ts8^;
        btU16       : Param1^.ts16^       := Param1^.ts16^ + Param2^.tu16^;
        btS16       : Param1^.ts16^       := Param1^.ts16^ + Param2^.ts16^;
        btU32       : Param1^.ts16^       := Param1^.ts16^ + Param2^.tu32^;
        btS32       : Param1^.ts16^       := Param1^.ts16^ + Param2^.ts32^;
        btS64       : Param1^.ts16^       := Param1^.ts16^ + Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts16^ + Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts16^ + Param2^.tDouble^);
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu32^       := Param1^.tu32^ + Param2^.tu8^;
        btS8        : Param1^.tu32^       := Param1^.tu32^ + Param2^.ts8^;
        btU16       : Param1^.tu32^       := Param1^.tu32^ + Param2^.tu16^;
        btS16       : Param1^.tu32^       := Param1^.tu32^ + Param2^.ts16^;
        btU32       : Param1^.tu32^       := Param1^.tu32^ + Param2^.tu32^;
        btS32       : Param1^.tu32^       := Param1^.tu32^ + Param2^.ts32^;
        btS64       : Param1^.tu32^       := Param1^.tu32^ + Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu32^ + Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu32^ + Param2^.tDouble^);
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts32^       := Param1^.ts32^ + Param2^.tu8^;
        btS8        : Param1^.ts32^       := Param1^.ts32^ + Param2^.ts8^;
        btU16       : Param1^.ts32^       := Param1^.ts32^ + Param2^.tu16^;
        btS16       : Param1^.ts32^       := Param1^.ts32^ + Param2^.ts16^;
        btU32       : Param1^.ts32^       := Param1^.ts32^ + Param2^.tu32^;
        btS32       : Param1^.ts32^       := Param1^.ts32^ + Param2^.ts32^;
        btS64       : Param1^.ts32^       := Param1^.ts32^ + Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts32^ + Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts32^ + Param2^.tDouble^);
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts64^       := Param1^.ts64^ + Param2^.tu8^;
        btS8        : Param1^.ts64^       := Param1^.ts64^ + Param2^.ts8^;
        btU16       : Param1^.ts64^       := Param1^.ts64^ + Param2^.tu16^;
        btS16       : Param1^.ts64^       := Param1^.ts64^ + Param2^.ts16^;
        btU32       : Param1^.ts64^       := Param1^.ts64^ + Param2^.tu32^;
        btS32       : Param1^.ts64^       := Param1^.ts64^ + Param2^.ts32^;
        btS64       : Param1^.ts64^       := Param1^.ts64^ + Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts64^ + Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts64^ + Param2^.tDouble^);
        end;
      end;
  btSingle :
      begin
        case Param2.AType of
        btU8        : Param1^.tSingle^       := Param1^.tSingle^ + Param2^.tu8^;
        btS8        : Param1^.tSingle^       := Param1^.tSingle^ + Param2^.ts8^;
        btU16       : Param1^.tSingle^       := Param1^.tSingle^ + Param2^.tu16^;
        btS16       : Param1^.tSingle^       := Param1^.tSingle^ + Param2^.ts16^;
        btU32       : Param1^.tSingle^       := Param1^.tSingle^ + Param2^.tu32^;
        btS32       : Param1^.tSingle^       := Param1^.tSingle^ + Param2^.ts32^;
        btS64       : Param1^.tSingle^       := Param1^.tSingle^ + Param2^.ts64^;
        btSingle    : Param1^.tSingle^       := Param1^.tSingle^ + Param2^.tSingle^;
        btDouble    : Param1^.tSingle^       := Param1^.tSingle^ + Param2^.tDouble^;
        end;
      end;
  btDouble :
      begin
        case Param2.AType of
        btU8        : Param1^.tDouble^       := Param1^.tDouble^ + Param2^.tu8^;
        btS8        : Param1^.tDouble^       := Param1^.tDouble^ + Param2^.ts8^;
        btU16       : Param1^.tDouble^       := Param1^.tDouble^ + Param2^.tu16^;
        btS16       : Param1^.tDouble^       := Param1^.tDouble^ + Param2^.ts16^;
        btU32       : Param1^.tDouble^       := Param1^.tDouble^ + Param2^.tu32^;
        btS32       : Param1^.tDouble^       := Param1^.tDouble^ + Param2^.ts32^;
        btS64       : Param1^.tDouble^       := Param1^.tDouble^ + Param2^.ts64^;
        btSingle    : Param1^.tDouble^       := Param1^.tDouble^ + Param2^.tSingle^;
        btDouble    : Param1^.tDouble^       := Param1^.tDouble^ + Param2^.tDouble^;
        end;
      end;
  btString :
      begin
        case Param2.AType of
        btString       : PbtString(Param1^.tString^)^ := PbtString(Param1^.tString^)^ + PbtString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btString);
              PbtString(Param1^.tString^)^ := PbtString(Param1^.tString^)^ + PbtString(Param2^.tString^)^;
            end;
        end;
      end;
  btUTF8String :
      begin
        case Param2.AType of
        btUTF8String   : PbtUTF8String(Param1^.tString^)^ := PbtUTF8String(Param1^.tString^)^ + PbtUTF8String(Param2^.tString^)^;
        btString,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btUTF8String);
              PbtUTF8String(Param1^.tString^)^ := PbtUTF8String(Param1^.tString^)^ + PbtUTF8String(Param2^.tString^)^;
            end;
        end;
      end;
  btWideString :
      begin
        case Param2.AType of
        btWideString   : PbtWideString(Param1^.tString^)^ := PbtWideString(Param1^.tString^)^ + PbtWideString(Param2^.tString^)^;
        btString,
        btUTF8String,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btWideString);
              PbtWideString(Param1^.tString^)^ := PbtWideString(Param1^.tString^)^ + PbtWideString(Param2^.tString^)^;
            end;
        end;
      end;
  btPChar :
      begin
        if not (Param2.AType in [btPChar, btString, btUTF8String, btWideString]) then
           exit;
        FVarHelp.ConvertContent(Param1, btString);
        case Param2.AType of
        btString       : PbtString(Param1^.tString^)^ := PbtString(Param1^.tString^)^ + PbtString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btString);
              PbtString(Param1^.tString^)^ := PbtString(Param1^.tString^)^ + PbtString(Param2^.tString^)^;
            end;
        end;
        FVarHelp.ConvertContent(Param1, btPChar);
      end;
  end;
end;

procedure TSE2VarOperation.BitAnd(Param1, Param2: PSE2VarData);
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu8^       := Param1^.tu8^ and Param2^.tu8^;
        btS8        : Param1^.tu8^       := Param1^.tu8^ and Param2^.ts8^;
        btU16       : Param1^.tu8^       := Param1^.tu8^ and Param2^.tu16^;
        btS16       : Param1^.tu8^       := Param1^.tu8^ and Param2^.ts16^;
        btU32       : Param1^.tu8^       := Param1^.tu8^ and Param2^.tu32^;
        btS32       : Param1^.tu8^       := Param1^.tu8^ and Param2^.ts32^;
        btS64       : Param1^.tu8^       := Param1^.tu8^ and Param2^.ts64^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts8^       := Param1^.ts8^ and Param2^.tu8^;
        btS8        : Param1^.ts8^       := Param1^.ts8^ and Param2^.ts8^;
        btU16       : Param1^.ts8^       := Param1^.ts8^ and Param2^.tu16^;
        btS16       : Param1^.ts8^       := Param1^.ts8^ and Param2^.ts16^;
        btU32       : Param1^.ts8^       := Param1^.ts8^ and Param2^.tu32^;
        btS32       : Param1^.ts8^       := Param1^.ts8^ and Param2^.ts32^;
        btS64       : Param1^.ts8^       := Param1^.ts8^ and Param2^.ts64^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu16^       := Param1^.tu16^ and Param2^.tu8^;
        btS8        : Param1^.tu16^       := Param1^.tu16^ and Param2^.ts8^;
        btU16       : Param1^.tu16^       := Param1^.tu16^ and Param2^.tu16^;
        btS16       : Param1^.tu16^       := Param1^.tu16^ and Param2^.ts16^;
        btU32       : Param1^.tu16^       := Param1^.tu16^ and Param2^.tu32^;
        btS32       : Param1^.tu16^       := Param1^.tu16^ and Param2^.ts32^;
        btS64       : Param1^.tu16^       := Param1^.tu16^ and Param2^.ts64^;
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts16^       := Param1^.ts16^ and Param2^.tu8^;
        btS8        : Param1^.ts16^       := Param1^.ts16^ and Param2^.ts8^;
        btU16       : Param1^.ts16^       := Param1^.ts16^ and Param2^.tu16^;
        btS16       : Param1^.ts16^       := Param1^.ts16^ and Param2^.ts16^;
        btU32       : Param1^.ts16^       := Param1^.ts16^ and Param2^.tu32^;
        btS32       : Param1^.ts16^       := Param1^.ts16^ and Param2^.ts32^;
        btS64       : Param1^.ts16^       := Param1^.ts16^ and Param2^.ts64^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu32^       := Param1^.tu32^ and Param2^.tu8^;
        btS8        : Param1^.tu32^       := Param1^.tu32^ and Param2^.ts8^;
        btU16       : Param1^.tu32^       := Param1^.tu32^ and Param2^.tu16^;
        btS16       : Param1^.tu32^       := Param1^.tu32^ and Param2^.ts16^;
        btU32       : Param1^.tu32^       := Param1^.tu32^ and Param2^.tu32^;
        btS32       : Param1^.tu32^       := Param1^.tu32^ and Param2^.ts32^;
        btS64       : Param1^.tu32^       := Param1^.tu32^ and Param2^.ts64^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts32^       := Param1^.ts32^ and Param2^.tu8^;
        btS8        : Param1^.ts32^       := Param1^.ts32^ and Param2^.ts8^;
        btU16       : Param1^.ts32^       := Param1^.ts32^ and Param2^.tu16^;
        btS16       : Param1^.ts32^       := Param1^.ts32^ and Param2^.ts16^;
        btU32       : Param1^.ts32^       := Param1^.ts32^ and Param2^.tu32^;
        btS32       : Param1^.ts32^       := Param1^.ts32^ and Param2^.ts32^;
        btS64       : Param1^.ts32^       := Param1^.ts32^ and Param2^.ts64^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts64^       := Param1^.ts64^ and Param2^.tu8^;
        btS8        : Param1^.ts64^       := Param1^.ts64^ and Param2^.ts8^;
        btU16       : Param1^.ts64^       := Param1^.ts64^ and Param2^.tu16^;
        btS16       : Param1^.ts64^       := Param1^.ts64^ and Param2^.ts16^;
        btU32       : Param1^.ts64^       := Param1^.ts64^ and Param2^.tu32^;
        btS32       : Param1^.ts64^       := Param1^.ts64^ and Param2^.ts32^;
        btS64       : Param1^.ts64^       := Param1^.ts64^ and Param2^.ts64^;
        end;
      end;
  end;
end;

procedure TSE2VarOperation.BitNot(Data: PSE2VarData);
begin
  case Data^.AType of
  btU8          : Data^.tu8^      := not Data^.tu8^;
  btS8          : Data^.ts8^      := not Data^.ts8^;
  btU16         : Data^.tu16^     := not Data^.tu16^;
  btS16         : Data^.ts16^     := not Data^.ts16^;
  btU32         : Data^.tu32^     := not Data^.tu32^;
  btS32         : Data^.ts32^     := not Data^.ts32^;
  btS64         : Data^.ts64^     := not Data^.ts64^;
  end;
end;

procedure TSE2VarOperation.BitOr(Param1, Param2: PSE2VarData);
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu8^       := Param1^.tu8^ or Param2^.tu8^;
        btS8        : Param1^.tu8^       := Param1^.tu8^ or Param2^.ts8^;
        btU16       : Param1^.tu8^       := Param1^.tu8^ or Param2^.tu16^;
        btS16       : Param1^.tu8^       := Param1^.tu8^ or Param2^.ts16^;
        btU32       : Param1^.tu8^       := Param1^.tu8^ or Param2^.tu32^;
        btS32       : Param1^.tu8^       := Param1^.tu8^ or Param2^.ts32^;
        btS64       : Param1^.tu8^       := Param1^.tu8^ or Param2^.ts64^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts8^       := Param1^.ts8^ or Param2^.tu8^;
        btS8        : Param1^.ts8^       := Param1^.ts8^ or Param2^.ts8^;
        btU16       : Param1^.ts8^       := Param1^.ts8^ or Param2^.tu16^;
        btS16       : Param1^.ts8^       := Param1^.ts8^ or Param2^.ts16^;
        btU32       : Param1^.ts8^       := Param1^.ts8^ or Param2^.tu32^;
        btS32       : Param1^.ts8^       := Param1^.ts8^ or Param2^.ts32^;
        btS64       : Param1^.ts8^       := Param1^.ts8^ or Param2^.ts64^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu16^       := Param1^.tu16^ or Param2^.tu8^;
        btS8        : Param1^.tu16^       := Param1^.tu16^ or Param2^.ts8^;
        btU16       : Param1^.tu16^       := Param1^.tu16^ or Param2^.tu16^;
        btS16       : Param1^.tu16^       := Param1^.tu16^ or Param2^.ts16^;
        btU32       : Param1^.tu16^       := Param1^.tu16^ or Param2^.tu32^;
        btS32       : Param1^.tu16^       := Param1^.tu16^ or Param2^.ts32^;
        btS64       : Param1^.tu16^       := Param1^.tu16^ or Param2^.ts64^;
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts16^       := Param1^.ts16^ or Param2^.tu8^;
        btS8        : Param1^.ts16^       := Param1^.ts16^ or Param2^.ts8^;
        btU16       : Param1^.ts16^       := Param1^.ts16^ or Param2^.tu16^;
        btS16       : Param1^.ts16^       := Param1^.ts16^ or Param2^.ts16^;
        btU32       : Param1^.ts16^       := Param1^.ts16^ or Param2^.tu32^;
        btS32       : Param1^.ts16^       := Param1^.ts16^ or Param2^.ts32^;
        btS64       : Param1^.ts16^       := Param1^.ts16^ or Param2^.ts64^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu32^       := Param1^.tu32^ or Param2^.tu8^;
        btS8        : Param1^.tu32^       := Param1^.tu32^ or Param2^.ts8^;
        btU16       : Param1^.tu32^       := Param1^.tu32^ or Param2^.tu16^;
        btS16       : Param1^.tu32^       := Param1^.tu32^ or Param2^.ts16^;
        btU32       : Param1^.tu32^       := Param1^.tu32^ or Param2^.tu32^;
        btS32       : Param1^.tu32^       := Param1^.tu32^ or Param2^.ts32^;
        btS64       : Param1^.tu32^       := Param1^.tu32^ or Param2^.ts64^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts32^       := Param1^.ts32^ or Param2^.tu8^;
        btS8        : Param1^.ts32^       := Param1^.ts32^ or Param2^.ts8^;
        btU16       : Param1^.ts32^       := Param1^.ts32^ or Param2^.tu16^;
        btS16       : Param1^.ts32^       := Param1^.ts32^ or Param2^.ts16^;
        btU32       : Param1^.ts32^       := Param1^.ts32^ or Param2^.tu32^;
        btS32       : Param1^.ts32^       := Param1^.ts32^ or Param2^.ts32^;
        btS64       : Param1^.ts32^       := Param1^.ts32^ or Param2^.ts64^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts64^       := Param1^.ts64^ or Param2^.tu8^;
        btS8        : Param1^.ts64^       := Param1^.ts64^ or Param2^.ts8^;
        btU16       : Param1^.ts64^       := Param1^.ts64^ or Param2^.tu16^;
        btS16       : Param1^.ts64^       := Param1^.ts64^ or Param2^.ts16^;
        btU32       : Param1^.ts64^       := Param1^.ts64^ or Param2^.tu32^;
        btS32       : Param1^.ts64^       := Param1^.ts64^ or Param2^.ts32^;
        btS64       : Param1^.ts64^       := Param1^.ts64^ or Param2^.ts64^;
        end;
      end;
  end;
end;

procedure TSE2VarOperation.BitShl(Param1, Param2: PSE2VarData);
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu8^       := Param1^.tu8^ shl Param2^.tu8^;
        btS8        : Param1^.tu8^       := Param1^.tu8^ shl Param2^.ts8^;
        btU16       : Param1^.tu8^       := Param1^.tu8^ shl Param2^.tu16^;
        btS16       : Param1^.tu8^       := Param1^.tu8^ shl Param2^.ts16^;
        btU32       : Param1^.tu8^       := Param1^.tu8^ shl Param2^.tu32^;
        btS32       : Param1^.tu8^       := Param1^.tu8^ shl Param2^.ts32^;
        btS64       : Param1^.tu8^       := Param1^.tu8^ shl Param2^.ts64^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts8^       := Param1^.ts8^ shl Param2^.tu8^;
        btS8        : Param1^.ts8^       := Param1^.ts8^ shl Param2^.ts8^;
        btU16       : Param1^.ts8^       := Param1^.ts8^ shl Param2^.tu16^;
        btS16       : Param1^.ts8^       := Param1^.ts8^ shl Param2^.ts16^;
        btU32       : Param1^.ts8^       := Param1^.ts8^ shl Param2^.tu32^;
        btS32       : Param1^.ts8^       := Param1^.ts8^ shl Param2^.ts32^;
        btS64       : Param1^.ts8^       := Param1^.ts8^ shl Param2^.ts64^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu16^       := Param1^.tu16^ shl Param2^.tu8^;
        btS8        : Param1^.tu16^       := Param1^.tu16^ shl Param2^.ts8^;
        btU16       : Param1^.tu16^       := Param1^.tu16^ shl Param2^.tu16^;
        btS16       : Param1^.tu16^       := Param1^.tu16^ shl Param2^.ts16^;
        btU32       : Param1^.tu16^       := Param1^.tu16^ shl Param2^.tu32^;
        btS32       : Param1^.tu16^       := Param1^.tu16^ shl Param2^.ts32^;
        btS64       : Param1^.tu16^       := Param1^.tu16^ shl Param2^.ts64^;
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts16^       := Param1^.ts16^ shl Param2^.tu8^;
        btS8        : Param1^.ts16^       := Param1^.ts16^ shl Param2^.ts8^;
        btU16       : Param1^.ts16^       := Param1^.ts16^ shl Param2^.tu16^;
        btS16       : Param1^.ts16^       := Param1^.ts16^ shl Param2^.ts16^;
        btU32       : Param1^.ts16^       := Param1^.ts16^ shl Param2^.tu32^;
        btS32       : Param1^.ts16^       := Param1^.ts16^ shl Param2^.ts32^;
        btS64       : Param1^.ts16^       := Param1^.ts16^ shl Param2^.ts64^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu32^       := Param1^.tu32^ shl Param2^.tu8^;
        btS8        : Param1^.tu32^       := Param1^.tu32^ shl Param2^.ts8^;
        btU16       : Param1^.tu32^       := Param1^.tu32^ shl Param2^.tu16^;
        btS16       : Param1^.tu32^       := Param1^.tu32^ shl Param2^.ts16^;
        btU32       : Param1^.tu32^       := Param1^.tu32^ shl Param2^.tu32^;
        btS32       : Param1^.tu32^       := Param1^.tu32^ shl Param2^.ts32^;
        btS64       : Param1^.tu32^       := Param1^.tu32^ shl Param2^.ts64^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts32^       := Param1^.ts32^ shl Param2^.tu8^;
        btS8        : Param1^.ts32^       := Param1^.ts32^ shl Param2^.ts8^;
        btU16       : Param1^.ts32^       := Param1^.ts32^ shl Param2^.tu16^;
        btS16       : Param1^.ts32^       := Param1^.ts32^ shl Param2^.ts16^;
        btU32       : Param1^.ts32^       := Param1^.ts32^ shl Param2^.tu32^;
        btS32       : Param1^.ts32^       := Param1^.ts32^ shl Param2^.ts32^;
        btS64       : Param1^.ts32^       := Param1^.ts32^ shl Param2^.ts64^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts64^       := Param1^.ts64^ shl Param2^.tu8^;
        btS8        : Param1^.ts64^       := Param1^.ts64^ shl Param2^.ts8^;
        btU16       : Param1^.ts64^       := Param1^.ts64^ shl Param2^.tu16^;
        btS16       : Param1^.ts64^       := Param1^.ts64^ shl Param2^.ts16^;
        btU32       : Param1^.ts64^       := Param1^.ts64^ shl Param2^.tu32^;
        btS32       : Param1^.ts64^       := Param1^.ts64^ shl Param2^.ts32^;
        btS64       : Param1^.ts64^       := Param1^.ts64^ shl Param2^.ts64^;
        end;
      end;
  end;
end;

procedure TSE2VarOperation.BitShr(Param1, Param2: PSE2VarData);
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu8^       := Param1^.tu8^ shr Param2^.tu8^;
        btS8        : Param1^.tu8^       := Param1^.tu8^ shr Param2^.ts8^;
        btU16       : Param1^.tu8^       := Param1^.tu8^ shr Param2^.tu16^;
        btS16       : Param1^.tu8^       := Param1^.tu8^ shr Param2^.ts16^;
        btU32       : Param1^.tu8^       := Param1^.tu8^ shr Param2^.tu32^;
        btS32       : Param1^.tu8^       := Param1^.tu8^ shr Param2^.ts32^;
        btS64       : Param1^.tu8^       := Param1^.tu8^ shr Param2^.ts64^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts8^       := Param1^.ts8^ shr Param2^.tu8^;
        btS8        : Param1^.ts8^       := Param1^.ts8^ shr Param2^.ts8^;
        btU16       : Param1^.ts8^       := Param1^.ts8^ shr Param2^.tu16^;
        btS16       : Param1^.ts8^       := Param1^.ts8^ shr Param2^.ts16^;
        btU32       : Param1^.ts8^       := Param1^.ts8^ shr Param2^.tu32^;
        btS32       : Param1^.ts8^       := Param1^.ts8^ shr Param2^.ts32^;
        btS64       : Param1^.ts8^       := Param1^.ts8^ shr Param2^.ts64^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu16^       := Param1^.tu16^ shr Param2^.tu8^;
        btS8        : Param1^.tu16^       := Param1^.tu16^ shr Param2^.ts8^;
        btU16       : Param1^.tu16^       := Param1^.tu16^ shr Param2^.tu16^;
        btS16       : Param1^.tu16^       := Param1^.tu16^ shr Param2^.ts16^;
        btU32       : Param1^.tu16^       := Param1^.tu16^ shr Param2^.tu32^;
        btS32       : Param1^.tu16^       := Param1^.tu16^ shr Param2^.ts32^;
        btS64       : Param1^.tu16^       := Param1^.tu16^ shr Param2^.ts64^;
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts16^       := Param1^.ts16^ shr Param2^.tu8^;
        btS8        : Param1^.ts16^       := Param1^.ts16^ shr Param2^.ts8^;
        btU16       : Param1^.ts16^       := Param1^.ts16^ shr Param2^.tu16^;
        btS16       : Param1^.ts16^       := Param1^.ts16^ shr Param2^.ts16^;
        btU32       : Param1^.ts16^       := Param1^.ts16^ shr Param2^.tu32^;
        btS32       : Param1^.ts16^       := Param1^.ts16^ shr Param2^.ts32^;
        btS64       : Param1^.ts16^       := Param1^.ts16^ shr Param2^.ts64^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu32^       := Param1^.tu32^ shr Param2^.tu8^;
        btS8        : Param1^.tu32^       := Param1^.tu32^ shr Param2^.ts8^;
        btU16       : Param1^.tu32^       := Param1^.tu32^ shr Param2^.tu16^;
        btS16       : Param1^.tu32^       := Param1^.tu32^ shr Param2^.ts16^;
        btU32       : Param1^.tu32^       := Param1^.tu32^ shr Param2^.tu32^;
        btS32       : Param1^.tu32^       := Param1^.tu32^ shr Param2^.ts32^;
        btS64       : Param1^.tu32^       := Param1^.tu32^ shr Param2^.ts64^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts32^       := Param1^.ts32^ shr Param2^.tu8^;
        btS8        : Param1^.ts32^       := Param1^.ts32^ shr Param2^.ts8^;
        btU16       : Param1^.ts32^       := Param1^.ts32^ shr Param2^.tu16^;
        btS16       : Param1^.ts32^       := Param1^.ts32^ shr Param2^.ts16^;
        btU32       : Param1^.ts32^       := Param1^.ts32^ shr Param2^.tu32^;
        btS32       : Param1^.ts32^       := Param1^.ts32^ shr Param2^.ts32^;
        btS64       : Param1^.ts32^       := Param1^.ts32^ shr Param2^.ts64^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts64^       := Param1^.ts64^ shr Param2^.tu8^;
        btS8        : Param1^.ts64^       := Param1^.ts64^ shr Param2^.ts8^;
        btU16       : Param1^.ts64^       := Param1^.ts64^ shr Param2^.tu16^;
        btS16       : Param1^.ts64^       := Param1^.ts64^ shr Param2^.ts16^;
        btU32       : Param1^.ts64^       := Param1^.ts64^ shr Param2^.tu32^;
        btS32       : Param1^.ts64^       := Param1^.ts64^ shr Param2^.ts32^;
        btS64       : Param1^.ts64^       := Param1^.ts64^ shr Param2^.ts64^;
        end;
      end;
  end;
end;

procedure TSE2VarOperation.BitXor(Param1, Param2: PSE2VarData);
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu8^       := Param1^.tu8^ xor Param2^.tu8^;
        btS8        : Param1^.tu8^       := Param1^.tu8^ xor Param2^.ts8^;
        btU16       : Param1^.tu8^       := Param1^.tu8^ xor Param2^.tu16^;
        btS16       : Param1^.tu8^       := Param1^.tu8^ xor Param2^.ts16^;
        btU32       : Param1^.tu8^       := Param1^.tu8^ xor Param2^.tu32^;
        btS32       : Param1^.tu8^       := Param1^.tu8^ xor Param2^.ts32^;
        btS64       : Param1^.tu8^       := Param1^.tu8^ xor Param2^.ts64^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts8^       := Param1^.ts8^ xor Param2^.tu8^;
        btS8        : Param1^.ts8^       := Param1^.ts8^ xor Param2^.ts8^;
        btU16       : Param1^.ts8^       := Param1^.ts8^ xor Param2^.tu16^;
        btS16       : Param1^.ts8^       := Param1^.ts8^ xor Param2^.ts16^;
        btU32       : Param1^.ts8^       := Param1^.ts8^ xor Param2^.tu32^;
        btS32       : Param1^.ts8^       := Param1^.ts8^ xor Param2^.ts32^;
        btS64       : Param1^.ts8^       := Param1^.ts8^ xor Param2^.ts64^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu16^       := Param1^.tu16^ xor Param2^.tu8^;
        btS8        : Param1^.tu16^       := Param1^.tu16^ xor Param2^.ts8^;
        btU16       : Param1^.tu16^       := Param1^.tu16^ xor Param2^.tu16^;
        btS16       : Param1^.tu16^       := Param1^.tu16^ xor Param2^.ts16^;
        btU32       : Param1^.tu16^       := Param1^.tu16^ xor Param2^.tu32^;
        btS32       : Param1^.tu16^       := Param1^.tu16^ xor Param2^.ts32^;
        btS64       : Param1^.tu16^       := Param1^.tu16^ xor Param2^.ts64^;
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts16^       := Param1^.ts16^ xor Param2^.tu8^;
        btS8        : Param1^.ts16^       := Param1^.ts16^ xor Param2^.ts8^;
        btU16       : Param1^.ts16^       := Param1^.ts16^ xor Param2^.tu16^;
        btS16       : Param1^.ts16^       := Param1^.ts16^ xor Param2^.ts16^;
        btU32       : Param1^.ts16^       := Param1^.ts16^ xor Param2^.tu32^;
        btS32       : Param1^.ts16^       := Param1^.ts16^ xor Param2^.ts32^;
        btS64       : Param1^.ts16^       := Param1^.ts16^ xor Param2^.ts64^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu32^       := Param1^.tu32^ xor Param2^.tu8^;
        btS8        : Param1^.tu32^       := Param1^.tu32^ xor Param2^.ts8^;
        btU16       : Param1^.tu32^       := Param1^.tu32^ xor Param2^.tu16^;
        btS16       : Param1^.tu32^       := Param1^.tu32^ xor Param2^.ts16^;
        btU32       : Param1^.tu32^       := Param1^.tu32^ xor Param2^.tu32^;
        btS32       : Param1^.tu32^       := Param1^.tu32^ xor Param2^.ts32^;
        btS64       : Param1^.tu32^       := Param1^.tu32^ xor Param2^.ts64^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts32^       := Param1^.ts32^ xor Param2^.tu8^;
        btS8        : Param1^.ts32^       := Param1^.ts32^ xor Param2^.ts8^;
        btU16       : Param1^.ts32^       := Param1^.ts32^ xor Param2^.tu16^;
        btS16       : Param1^.ts32^       := Param1^.ts32^ xor Param2^.ts16^;
        btU32       : Param1^.ts32^       := Param1^.ts32^ xor Param2^.tu32^;
        btS32       : Param1^.ts32^       := Param1^.ts32^ xor Param2^.ts32^;
        btS64       : Param1^.ts32^       := Param1^.ts32^ xor Param2^.ts64^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts64^       := Param1^.ts64^ xor Param2^.tu8^;
        btS8        : Param1^.ts64^       := Param1^.ts64^ xor Param2^.ts8^;
        btU16       : Param1^.ts64^       := Param1^.ts64^ xor Param2^.tu16^;
        btS16       : Param1^.ts64^       := Param1^.ts64^ xor Param2^.ts16^;
        btU32       : Param1^.ts64^       := Param1^.ts64^ xor Param2^.tu32^;
        btS32       : Param1^.ts64^       := Param1^.ts64^ xor Param2^.ts32^;
        btS64       : Param1^.ts64^       := Param1^.ts64^ xor Param2^.ts64^;
        end;
      end;
  end;
end;

procedure TSE2VarOperation.BooleanNot(Data: PSE2VarData);
begin
  case Data^.AType of
  btBoolean     :
      begin
        if Data^.tu8^ <> 0 then
           Data^.tu8^ := 0
        else
           Data^.tu8^ := 1;
      end;
  btS8          : Data^.ts8^      := TbtS8(not boolean(Data^.ts8^));
  btU16         : Data^.tu16^     := TbtU16(not boolean(Data^.tu16^));
  btS16         : Data^.ts16^     := TbtS16(not boolean(Data^.ts16^));
  btU32         : Data^.tu32^     := TbtU32(not boolean(Data^.tu32^));
  btS32         : Data^.ts32^     := TbtS32(not boolean(Data^.ts32^));
  btS64         : Data^.ts64^     := TbtS64(not boolean(Data^.ts64^));
  end;
end;

constructor TSE2VarOperation.Create(VarHelper: TSE2VarHelper);
begin
  inherited Create;
  FVarHelp := VarHelper;
end;

procedure TSE2VarOperation.Divide(Param1, Param2: PSE2VarData);
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu8^       := Param1^.tu8^ div Param2^.tu8^;
        btS8        : Param1^.tu8^       := Param1^.tu8^ div Param2^.ts8^;
        btU16       : Param1^.tu8^       := Param1^.tu8^ div Param2^.tu16^;
        btS16       : Param1^.tu8^       := Param1^.tu8^ div Param2^.ts16^;
        btU32       : Param1^.tu8^       := Param1^.tu8^ div Param2^.tu32^;
        btS32       : Param1^.tu8^       := Param1^.tu8^ div Param2^.ts32^;
        btS64       : Param1^.tu8^       := Param1^.tu8^ div Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu8^ / Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu8^ / Param2^.tDouble^);
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts8^       := Param1^.ts8^ div Param2^.tu8^;
        btS8        : Param1^.ts8^       := Param1^.ts8^ div Param2^.ts8^;
        btU16       : Param1^.ts8^       := Param1^.ts8^ div Param2^.tu16^;
        btS16       : Param1^.ts8^       := Param1^.ts8^ div Param2^.ts16^;
        btU32       : Param1^.ts8^       := Param1^.ts8^ div Param2^.tu32^;
        btS32       : Param1^.ts8^       := Param1^.ts8^ div Param2^.ts32^;
        btS64       : Param1^.ts8^       := Param1^.ts8^ div Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts8^ / Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts8^ / Param2^.tDouble^);
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu16^       := Param1^.tu16^ div Param2^.tu8^;
        btS8        : Param1^.tu16^       := Param1^.tu16^ div Param2^.ts8^;
        btU16       : Param1^.tu16^       := Param1^.tu16^ div Param2^.tu16^;
        btS16       : Param1^.tu16^       := Param1^.tu16^ div Param2^.ts16^;
        btU32       : Param1^.tu16^       := Param1^.tu16^ div Param2^.tu32^;
        btS32       : Param1^.tu16^       := Param1^.tu16^ div Param2^.ts32^;
        btS64       : Param1^.tu16^       := Param1^.tu16^ div Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu16^ / Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu16^ / Param2^.tDouble^);
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts16^       := Param1^.ts16^ div Param2^.tu8^;
        btS8        : Param1^.ts16^       := Param1^.ts16^ div Param2^.ts8^;
        btU16       : Param1^.ts16^       := Param1^.ts16^ div Param2^.tu16^;
        btS16       : Param1^.ts16^       := Param1^.ts16^ div Param2^.ts16^;
        btU32       : Param1^.ts16^       := Param1^.ts16^ div Param2^.tu32^;
        btS32       : Param1^.ts16^       := Param1^.ts16^ div Param2^.ts32^;
        btS64       : Param1^.ts16^       := Param1^.ts16^ div Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts16^ / Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts16^ / Param2^.tDouble^);
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu32^       := Param1^.tu32^ div Param2^.tu8^;
        btS8        : Param1^.tu32^       := Param1^.tu32^ div Param2^.ts8^;
        btU16       : Param1^.tu32^       := Param1^.tu32^ div Param2^.tu16^;
        btS16       : Param1^.tu32^       := Param1^.tu32^ div Param2^.ts16^;
        btU32       : Param1^.tu32^       := Param1^.tu32^ div Param2^.tu32^;
        btS32       : Param1^.tu32^       := Param1^.tu32^ div Param2^.ts32^;
        btS64       : Param1^.tu32^       := Param1^.tu32^ div Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu32^ / Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu32^ / Param2^.tDouble^);
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts32^       := Param1^.ts32^ div Param2^.tu8^;
        btS8        : Param1^.ts32^       := Param1^.ts32^ div Param2^.ts8^;
        btU16       : Param1^.ts32^       := Param1^.ts32^ div Param2^.tu16^;
        btS16       : Param1^.ts32^       := Param1^.ts32^ div Param2^.ts16^;
        btU32       : Param1^.ts32^       := Param1^.ts32^ div Param2^.tu32^;
        btS32       : Param1^.ts32^       := Param1^.ts32^ div Param2^.ts32^;
        btS64       : Param1^.ts32^       := Param1^.ts32^ div Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts32^ / Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts32^ / Param2^.tDouble^);
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts64^       := Param1^.ts64^ div Param2^.tu8^;
        btS8        : Param1^.ts64^       := Param1^.ts64^ div Param2^.ts8^;
        btU16       : Param1^.ts64^       := Param1^.ts64^ div Param2^.tu16^;
        btS16       : Param1^.ts64^       := Param1^.ts64^ div Param2^.ts16^;
        btU32       : Param1^.ts64^       := Param1^.ts64^ div Param2^.tu32^;
        btS32       : Param1^.ts64^       := Param1^.ts64^ div Param2^.ts32^;
        btS64       : Param1^.ts64^       := Param1^.ts64^ div Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts64^ / Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts64^ / Param2^.tDouble^);
        end;
      end;
  btSingle :
      begin
        case Param2.AType of
        btU8        : Param1^.tSingle^       := Param1^.tSingle^ / Param2^.tu8^;
        btS8        : Param1^.tSingle^       := Param1^.tSingle^ / Param2^.ts8^;
        btU16       : Param1^.tSingle^       := Param1^.tSingle^ / Param2^.tu16^;
        btS16       : Param1^.tSingle^       := Param1^.tSingle^ / Param2^.ts16^;
        btU32       : Param1^.tSingle^       := Param1^.tSingle^ / Param2^.tu32^;
        btS32       : Param1^.tSingle^       := Param1^.tSingle^ / Param2^.ts32^;
        btS64       : Param1^.tSingle^       := Param1^.tSingle^ / Param2^.ts64^;
        btSingle    : Param1^.tSingle^       := Param1^.tSingle^ / Param2^.tSingle^;
        btDouble    : Param1^.tSingle^       := Param1^.tSingle^ / Param2^.tDouble^;
        end;
      end;
  btDouble :
      begin
        case Param2.AType of
        btU8        : Param1^.tDouble^       := Param1^.tDouble^ / Param2^.tu8^;
        btS8        : Param1^.tDouble^       := Param1^.tDouble^ / Param2^.ts8^;
        btU16       : Param1^.tDouble^       := Param1^.tDouble^ / Param2^.tu16^;
        btS16       : Param1^.tDouble^       := Param1^.tDouble^ / Param2^.ts16^;
        btU32       : Param1^.tDouble^       := Param1^.tDouble^ / Param2^.tu32^;
        btS32       : Param1^.tDouble^       := Param1^.tDouble^ / Param2^.ts32^;
        btS64       : Param1^.tDouble^       := Param1^.tDouble^ / Param2^.ts64^;
        btSingle    : Param1^.tDouble^       := Param1^.tDouble^ / Param2^.tSingle^;
        btDouble    : Param1^.tDouble^       := Param1^.tDouble^ / Param2^.tDouble^;
        end;
      end;
  end;
end;

procedure TSE2VarOperation.DivideMod(Param1, Param2: PSE2VarData);
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu8^       := Param1^.tu8^ mod Param2^.tu8^;
        btS8        : Param1^.tu8^       := Param1^.tu8^ mod Param2^.ts8^;
        btU16       : Param1^.tu8^       := Param1^.tu8^ mod Param2^.tu16^;
        btS16       : Param1^.tu8^       := Param1^.tu8^ mod Param2^.ts16^;
        btU32       : Param1^.tu8^       := Param1^.tu8^ mod Param2^.tu32^;
        btS32       : Param1^.tu8^       := Param1^.tu8^ mod Param2^.ts32^;
        btS64       : Param1^.tu8^       := Param1^.tu8^ mod Param2^.ts64^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts8^       := Param1^.ts8^ mod Param2^.tu8^;
        btS8        : Param1^.ts8^       := Param1^.ts8^ mod Param2^.ts8^;
        btU16       : Param1^.ts8^       := Param1^.ts8^ mod Param2^.tu16^;
        btS16       : Param1^.ts8^       := Param1^.ts8^ mod Param2^.ts16^;
        btU32       : Param1^.ts8^       := Param1^.ts8^ mod Param2^.tu32^;
        btS32       : Param1^.ts8^       := Param1^.ts8^ mod Param2^.ts32^;
        btS64       : Param1^.ts8^       := Param1^.ts8^ mod Param2^.ts64^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu16^       := Param1^.tu16^ mod Param2^.tu8^;
        btS8        : Param1^.tu16^       := Param1^.tu16^ mod Param2^.ts8^;
        btU16       : Param1^.tu16^       := Param1^.tu16^ mod Param2^.tu16^;
        btS16       : Param1^.tu16^       := Param1^.tu16^ mod Param2^.ts16^;
        btU32       : Param1^.tu16^       := Param1^.tu16^ mod Param2^.tu32^;
        btS32       : Param1^.tu16^       := Param1^.tu16^ mod Param2^.ts32^;
        btS64       : Param1^.tu16^       := Param1^.tu16^ mod Param2^.ts64^;
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts16^       := Param1^.ts16^ mod Param2^.tu8^;
        btS8        : Param1^.ts16^       := Param1^.ts16^ mod Param2^.ts8^;
        btU16       : Param1^.ts16^       := Param1^.ts16^ mod Param2^.tu16^;
        btS16       : Param1^.ts16^       := Param1^.ts16^ mod Param2^.ts16^;
        btU32       : Param1^.ts16^       := Param1^.ts16^ mod Param2^.tu32^;
        btS32       : Param1^.ts16^       := Param1^.ts16^ mod Param2^.ts32^;
        btS64       : Param1^.ts16^       := Param1^.ts16^ mod Param2^.ts64^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu32^       := Param1^.tu32^ mod Param2^.tu8^;
        btS8        : Param1^.tu32^       := Param1^.tu32^ mod Param2^.ts8^;
        btU16       : Param1^.tu32^       := Param1^.tu32^ mod Param2^.tu16^;
        btS16       : Param1^.tu32^       := Param1^.tu32^ mod Param2^.ts16^;
        btU32       : Param1^.tu32^       := Param1^.tu32^ mod Param2^.tu32^;
        btS32       : Param1^.tu32^       := Param1^.tu32^ mod Param2^.ts32^;
        btS64       : Param1^.tu32^       := Param1^.tu32^ mod Param2^.ts64^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts32^       := Param1^.ts32^ mod Param2^.tu8^;
        btS8        : Param1^.ts32^       := Param1^.ts32^ mod Param2^.ts8^;
        btU16       : Param1^.ts32^       := Param1^.ts32^ mod Param2^.tu16^;
        btS16       : Param1^.ts32^       := Param1^.ts32^ mod Param2^.ts16^;
        btU32       : Param1^.ts32^       := Param1^.ts32^ mod Param2^.tu32^;
        btS32       : Param1^.ts32^       := Param1^.ts32^ mod Param2^.ts32^;
        btS64       : Param1^.ts32^       := Param1^.ts32^ mod Param2^.ts64^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts64^       := Param1^.ts64^ mod Param2^.tu8^;
        btS8        : Param1^.ts64^       := Param1^.ts64^ mod Param2^.ts8^;
        btU16       : Param1^.ts64^       := Param1^.ts64^ mod Param2^.tu16^;
        btS16       : Param1^.ts64^       := Param1^.ts64^ mod Param2^.ts16^;
        btU32       : Param1^.ts64^       := Param1^.ts64^ mod Param2^.tu32^;
        btS32       : Param1^.ts64^       := Param1^.ts64^ mod Param2^.ts32^;
        btS64       : Param1^.ts64^       := Param1^.ts64^ mod Param2^.ts64^;
        end;
      end;
  end;
end;

procedure TSE2VarOperation.Multiply(Param1, Param2: PSE2VarData);
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu8^       := Param1^.tu8^ * Param2^.tu8^;
        btS8        : Param1^.tu8^       := Param1^.tu8^ * Param2^.ts8^;
        btU16       : Param1^.tu8^       := Param1^.tu8^ * Param2^.tu16^;
        btS16       : Param1^.tu8^       := Param1^.tu8^ * Param2^.ts16^;
        btU32       : Param1^.tu8^       := Param1^.tu8^ * Param2^.tu32^;
        btS32       : Param1^.tu8^       := Param1^.tu8^ * Param2^.ts32^;
        btS64       : Param1^.tu8^       := Param1^.tu8^ * Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu8^ * Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu8^ * Param2^.tDouble^);
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts8^       := Param1^.ts8^ * Param2^.tu8^;
        btS8        : Param1^.ts8^       := Param1^.ts8^ * Param2^.ts8^;
        btU16       : Param1^.ts8^       := Param1^.ts8^ * Param2^.tu16^;
        btS16       : Param1^.ts8^       := Param1^.ts8^ * Param2^.ts16^;
        btU32       : Param1^.ts8^       := Param1^.ts8^ * Param2^.tu32^;
        btS32       : Param1^.ts8^       := Param1^.ts8^ * Param2^.ts32^;
        btS64       : Param1^.ts8^       := Param1^.ts8^ * Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts8^ * Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts8^ * Param2^.tDouble^);
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu16^       := Param1^.tu16^ * Param2^.tu8^;
        btS8        : Param1^.tu16^       := Param1^.tu16^ * Param2^.ts8^;
        btU16       : Param1^.tu16^       := Param1^.tu16^ * Param2^.tu16^;
        btS16       : Param1^.tu16^       := Param1^.tu16^ * Param2^.ts16^;
        btU32       : Param1^.tu16^       := Param1^.tu16^ * Param2^.tu32^;
        btS32       : Param1^.tu16^       := Param1^.tu16^ * Param2^.ts32^;
        btS64       : Param1^.tu16^       := Param1^.tu16^ * Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu16^ * Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu16^ * Param2^.tDouble^);
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts16^       := Param1^.ts16^ * Param2^.tu8^;
        btS8        : Param1^.ts16^       := Param1^.ts16^ * Param2^.ts8^;
        btU16       : Param1^.ts16^       := Param1^.ts16^ * Param2^.tu16^;
        btS16       : Param1^.ts16^       := Param1^.ts16^ * Param2^.ts16^;
        btU32       : Param1^.ts16^       := Param1^.ts16^ * Param2^.tu32^;
        btS32       : Param1^.ts16^       := Param1^.ts16^ * Param2^.ts32^;
        btS64       : Param1^.ts16^       := Param1^.ts16^ * Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts16^ * Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts16^ * Param2^.tDouble^);
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu32^       := Param1^.tu32^ * Param2^.tu8^;
        btS8        : Param1^.tu32^       := Param1^.tu32^ * Param2^.ts8^;
        btU16       : Param1^.tu32^       := Param1^.tu32^ * Param2^.tu16^;
        btS16       : Param1^.tu32^       := Param1^.tu32^ * Param2^.ts16^;
        btU32       : Param1^.tu32^       := Param1^.tu32^ * Param2^.tu32^;
        btS32       : Param1^.tu32^       := Param1^.tu32^ * Param2^.ts32^;
        btS64       : Param1^.tu32^       := Param1^.tu32^ * Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu32^ * Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu32^ * Param2^.tDouble^);
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts32^       := Param1^.ts32^ * Param2^.tu8^;
        btS8        : Param1^.ts32^       := Param1^.ts32^ * Param2^.ts8^;
        btU16       : Param1^.ts32^       := Param1^.ts32^ * Param2^.tu16^;
        btS16       : Param1^.ts32^       := Param1^.ts32^ * Param2^.ts16^;
        btU32       : Param1^.ts32^       := Param1^.ts32^ * Param2^.tu32^;
        btS32       : Param1^.ts32^       := Param1^.ts32^ * Param2^.ts32^;
        btS64       : Param1^.ts32^       := Param1^.ts32^ * Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts32^ - Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts32^ - Param2^.tDouble^);
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts64^       := Param1^.ts64^ * Param2^.tu8^;
        btS8        : Param1^.ts64^       := Param1^.ts64^ * Param2^.ts8^;
        btU16       : Param1^.ts64^       := Param1^.ts64^ * Param2^.tu16^;
        btS16       : Param1^.ts64^       := Param1^.ts64^ * Param2^.ts16^;
        btU32       : Param1^.ts64^       := Param1^.ts64^ * Param2^.tu32^;
        btS32       : Param1^.ts64^       := Param1^.ts64^ * Param2^.ts32^;
        btS64       : Param1^.ts64^       := Param1^.ts64^ * Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts64^ * Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts64^ * Param2^.tDouble^);
        end;
      end;
  btSingle :
      begin
        case Param2.AType of
        btU8        : Param1^.tSingle^       := Param1^.tSingle^ * Param2^.tu8^;
        btS8        : Param1^.tSingle^       := Param1^.tSingle^ * Param2^.ts8^;
        btU16       : Param1^.tSingle^       := Param1^.tSingle^ * Param2^.tu16^;
        btS16       : Param1^.tSingle^       := Param1^.tSingle^ * Param2^.ts16^;
        btU32       : Param1^.tSingle^       := Param1^.tSingle^ * Param2^.tu32^;
        btS32       : Param1^.tSingle^       := Param1^.tSingle^ * Param2^.ts32^;
        btS64       : Param1^.tSingle^       := Param1^.tSingle^ * Param2^.ts64^;
        btSingle    : Param1^.tSingle^       := Param1^.tSingle^ * Param2^.tSingle^;
        btDouble    : Param1^.tSingle^       := Param1^.tSingle^ * Param2^.tDouble^;
        end;
      end;
  btDouble :
      begin
        case Param2.AType of
        btU8        : Param1^.tDouble^       := Param1^.tDouble^ * Param2^.tu8^;
        btS8        : Param1^.tDouble^       := Param1^.tDouble^ * Param2^.ts8^;
        btU16       : Param1^.tDouble^       := Param1^.tDouble^ * Param2^.tu16^;
        btS16       : Param1^.tDouble^       := Param1^.tDouble^ * Param2^.ts16^;
        btU32       : Param1^.tDouble^       := Param1^.tDouble^ * Param2^.tu32^;
        btS32       : Param1^.tDouble^       := Param1^.tDouble^ * Param2^.ts32^;
        btS64       : Param1^.tDouble^       := Param1^.tDouble^ * Param2^.ts64^;
        btSingle    : Param1^.tDouble^       := Param1^.tDouble^ * Param2^.tSingle^;
        btDouble    : Param1^.tDouble^       := Param1^.tDouble^ * Param2^.tDouble^;
        end;
      end;
  end;
end;

procedure TSE2VarOperation.Negation(Data: PSE2VarData);
begin
  case Data^.AType of
  btU8          : Data^.tu8^      := -Data^.tu8^;
  btS8          : Data^.ts8^      := -Data^.ts8^;
  btU16         : Data^.tu16^     := -Data^.tu16^;
  btS16         : Data^.ts16^     := -Data^.ts16^;
  btU32         : Data^.tu32^     := -Data^.tu32^;
  btS32         : Data^.ts32^     := -Data^.ts32^;
  btS64         : Data^.ts64^     := -Data^.ts64^;
  btSingle      : Data^.tSingle^  := -Data^.tSingle^;
  btDouble      : Data^.tDouble^  := -Data^.tDouble^;
  end;
end;

procedure TSE2VarOperation.Substract(Param1, Param2: PSE2VarData);
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu8^       := Param1^.tu8^ - Param2^.tu8^;
        btS8        : Param1^.tu8^       := Param1^.tu8^ - Param2^.ts8^;
        btU16       : Param1^.tu8^       := Param1^.tu8^ - Param2^.tu16^;
        btS16       : Param1^.tu8^       := Param1^.tu8^ - Param2^.ts16^;
        btU32       : Param1^.tu8^       := Param1^.tu8^ - Param2^.tu32^;
        btS32       : Param1^.tu8^       := Param1^.tu8^ - Param2^.ts32^;
        btS64       : Param1^.tu8^       := Param1^.tu8^ - Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu8^ - Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu8^ - Param2^.tDouble^);
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts8^       := Param1^.ts8^ - Param2^.tu8^;
        btS8        : Param1^.ts8^       := Param1^.ts8^ - Param2^.ts8^;
        btU16       : Param1^.ts8^       := Param1^.ts8^ - Param2^.tu16^;
        btS16       : Param1^.ts8^       := Param1^.ts8^ - Param2^.ts16^;
        btU32       : Param1^.ts8^       := Param1^.ts8^ - Param2^.tu32^;
        btS32       : Param1^.ts8^       := Param1^.ts8^ - Param2^.ts32^;
        btS64       : Param1^.ts8^       := Param1^.ts8^ - Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts8^ - Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts8^ - Param2^.tDouble^);
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu16^       := Param1^.tu16^ - Param2^.tu8^;
        btS8        : Param1^.tu16^       := Param1^.tu16^ - Param2^.ts8^;
        btU16       : Param1^.tu16^       := Param1^.tu16^ - Param2^.tu16^;
        btS16       : Param1^.tu16^       := Param1^.tu16^ - Param2^.ts16^;
        btU32       : Param1^.tu16^       := Param1^.tu16^ - Param2^.tu32^;
        btS32       : Param1^.tu16^       := Param1^.tu16^ - Param2^.ts32^;
        btS64       : Param1^.tu16^       := Param1^.tu16^ - Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu16^ - Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu16^ - Param2^.tDouble^);
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts16^       := Param1^.ts16^ - Param2^.tu8^;
        btS8        : Param1^.ts16^       := Param1^.ts16^ - Param2^.ts8^;
        btU16       : Param1^.ts16^       := Param1^.ts16^ - Param2^.tu16^;
        btS16       : Param1^.ts16^       := Param1^.ts16^ - Param2^.ts16^;
        btU32       : Param1^.ts16^       := Param1^.ts16^ - Param2^.tu32^;
        btS32       : Param1^.ts16^       := Param1^.ts16^ - Param2^.ts32^;
        btS64       : Param1^.ts16^       := Param1^.ts16^ - Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts16^ - Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts16^ - Param2^.tDouble^);
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : Param1^.tu32^       := Param1^.tu32^ - Param2^.tu8^;
        btS8        : Param1^.tu32^       := Param1^.tu32^ - Param2^.ts8^;
        btU16       : Param1^.tu32^       := Param1^.tu32^ - Param2^.tu16^;
        btS16       : Param1^.tu32^       := Param1^.tu32^ - Param2^.ts16^;
        btU32       : Param1^.tu32^       := Param1^.tu32^ - Param2^.tu32^;
        btS32       : Param1^.tu32^       := Param1^.tu32^ - Param2^.ts32^;
        btS64       : Param1^.tu32^       := Param1^.tu32^ - Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.tu32^ - Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.tu32^ - Param2^.tDouble^);
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts32^       := Param1^.ts32^ - Param2^.tu8^;
        btS8        : Param1^.ts32^       := Param1^.ts32^ - Param2^.ts8^;
        btU16       : Param1^.ts32^       := Param1^.ts32^ - Param2^.tu16^;
        btS16       : Param1^.ts32^       := Param1^.ts32^ - Param2^.ts16^;
        btU32       : Param1^.ts32^       := Param1^.ts32^ - Param2^.tu32^;
        btS32       : Param1^.ts32^       := Param1^.ts32^ - Param2^.ts32^;
        btS64       : Param1^.ts32^       := Param1^.ts32^ - Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts32^ - Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts32^ - Param2^.tDouble^);
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : Param1^.ts64^       := Param1^.ts64^ - Param2^.tu8^;
        btS8        : Param1^.ts64^       := Param1^.ts64^ - Param2^.ts8^;
        btU16       : Param1^.ts64^       := Param1^.ts64^ - Param2^.tu16^;
        btS16       : Param1^.ts64^       := Param1^.ts64^ - Param2^.ts16^;
        btU32       : Param1^.ts64^       := Param1^.ts64^ - Param2^.tu32^;
        btS32       : Param1^.ts64^       := Param1^.ts64^ - Param2^.ts32^;
        btS64       : Param1^.ts64^       := Param1^.ts64^ - Param2^.ts64^;
        btSingle    : FVarHelp.SetContentAsSingle(Param1, Param1^.ts64^ - Param2^.tSingle^);
        btDouble    : FVarHelp.SetContentAsDouble(Param1, Param1^.ts64^ - Param2^.tDouble^);
        end;
      end;
  btSingle :
      begin
        case Param2.AType of
        btU8        : Param1^.tSingle^       := Param1^.tSingle^ - Param2^.tu8^;
        btS8        : Param1^.tSingle^       := Param1^.tSingle^ - Param2^.ts8^;
        btU16       : Param1^.tSingle^       := Param1^.tSingle^ - Param2^.tu16^;
        btS16       : Param1^.tSingle^       := Param1^.tSingle^ - Param2^.ts16^;
        btU32       : Param1^.tSingle^       := Param1^.tSingle^ - Param2^.tu32^;
        btS32       : Param1^.tSingle^       := Param1^.tSingle^ - Param2^.ts32^;
        btS64       : Param1^.tSingle^       := Param1^.tSingle^ - Param2^.ts64^;
        btSingle    : Param1^.tSingle^       := Param1^.tSingle^ - Param2^.tSingle^;
        btDouble    : Param1^.tSingle^       := Param1^.tSingle^ - Param2^.tDouble^;
        end;
      end;
  btDouble :
      begin
        case Param2.AType of
        btU8        : Param1^.tDouble^       := Param1^.tDouble^ - Param2^.tu8^;
        btS8        : Param1^.tDouble^       := Param1^.tDouble^ - Param2^.ts8^;
        btU16       : Param1^.tDouble^       := Param1^.tDouble^ - Param2^.tu16^;
        btS16       : Param1^.tDouble^       := Param1^.tDouble^ - Param2^.ts16^;
        btU32       : Param1^.tDouble^       := Param1^.tDouble^ - Param2^.tu32^;
        btS32       : Param1^.tDouble^       := Param1^.tDouble^ - Param2^.ts32^;
        btS64       : Param1^.tDouble^       := Param1^.tDouble^ - Param2^.ts64^;
        btSingle    : Param1^.tDouble^       := Param1^.tDouble^ - Param2^.tSingle^;
        btDouble    : Param1^.tDouble^       := Param1^.tDouble^ - Param2^.tDouble^;
        end;
      end;
  end;
end;

{ TSE2VarCompare }

function TSE2VarCompare.Bigger(Param1, Param2: PSE2VarData): boolean;
begin
  result := False;
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu8^ > Param2^.tu8^;
        btS8        : result := Param1^.tu8^ > Param2^.ts8^;
        btU16       : result := Param1^.tu8^ > Param2^.tu16^;
        btS16       : result := Param1^.tu8^ > Param2^.ts16^;
        btU32       : result := Param1^.tu8^ > Param2^.tu32^;
        btS32       : result := Param1^.tu8^ > Param2^.ts32^;
        btS64       : result := Param1^.tu8^ > Param2^.ts64^;
        btSingle    : result := Param1^.tu8^ > Param2^.tSingle^;
        btDouble    : result := Param1^.tu8^ > Param2^.tDouble^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts8^ > Param2^.tu8^;
        btS8        : result := Param1^.ts8^ > Param2^.ts8^;
        btU16       : result := Param1^.ts8^ > Param2^.tu16^;
        btS16       : result := Param1^.ts8^ > Param2^.ts16^;
        btU32       : result := Param1^.ts8^ > Param2^.tu32^;
        btS32       : result := Param1^.ts8^ > Param2^.ts32^;
        btS64       : result := Param1^.ts8^ > Param2^.ts64^;
        btSingle    : result := Param1^.ts8^ > Param2^.tSingle^;
        btDouble    : result := Param1^.ts8^ > Param2^.tDouble^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu16^ > Param2^.tu8^;
        btS8        : result := Param1^.tu16^ > Param2^.ts8^;
        btU16       : result := Param1^.tu16^ > Param2^.tu16^;
        btS16       : result := Param1^.tu16^ > Param2^.ts16^;
        btU32       : result := Param1^.tu16^ > Param2^.tu32^;
        btS32       : result := Param1^.tu16^ > Param2^.ts32^;
        btS64       : result := Param1^.tu16^ > Param2^.ts64^;
        btSingle    : result := Param1^.tu16^ > Param2^.tSingle^;
        btDouble    : result := Param1^.tu16^ > Param2^.tDouble^;
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts16^ > Param2^.tu8^;
        btS8        : result := Param1^.ts16^ > Param2^.ts8^;
        btU16       : result := Param1^.ts16^ > Param2^.tu16^;
        btS16       : result := Param1^.ts16^ > Param2^.ts16^;
        btU32       : result := Param1^.ts16^ > Param2^.tu32^;
        btS32       : result := Param1^.ts16^ > Param2^.ts32^;
        btS64       : result := Param1^.ts16^ > Param2^.ts64^;
        btSingle    : result := Param1^.ts16^ > Param2^.tSingle^;
        btDouble    : result := Param1^.ts16^ > Param2^.tDouble^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu32^ > Param2^.tu8^;
        btS8        : result := Param1^.tu32^ > Param2^.ts8^;
        btU16       : result := Param1^.tu32^ > Param2^.tu16^;
        btS16       : result := Param1^.tu32^ > Param2^.ts16^;
        btU32       : result := Param1^.tu32^ > Param2^.tu32^;
        btS32       : result := Param1^.tu32^ > Param2^.ts32^;
        btS64       : result := Param1^.tu32^ > Param2^.ts64^;
        btSingle    : result := Param1^.tu32^ > Param2^.tSingle^;
        btDouble    : result := Param1^.tu32^ > Param2^.tDouble^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts32^ > Param2^.tu8^;
        btS8        : result := Param1^.ts32^ > Param2^.ts8^;
        btU16       : result := Param1^.ts32^ > Param2^.tu16^;
        btS16       : result := Param1^.ts32^ > Param2^.ts16^;
        btU32       : result := Param1^.ts32^ > Param2^.tu32^;
        btS32       : result := Param1^.ts32^ > Param2^.ts32^;
        btS64       : result := Param1^.ts32^ > Param2^.ts64^;
        btSingle    : result := Param1^.ts32^ > Param2^.tSingle^;
        btDouble    : result := Param1^.ts32^ > Param2^.tDouble^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts64^ > Param2^.tu8^;
        btS8        : result := Param1^.ts64^ > Param2^.ts8^;
        btU16       : result := Param1^.ts64^ > Param2^.tu16^;
        btS16       : result := Param1^.ts64^ > Param2^.ts16^;
        btU32       : result := Param1^.ts64^ > Param2^.tu32^;
        btS32       : result := Param1^.ts64^ > Param2^.ts32^;
        btS64       : result := Param1^.ts64^ > Param2^.ts64^;
        btSingle    : result := Param1^.ts64^ > Param2^.tSingle^;
        btDouble    : result := Param1^.ts64^ > Param2^.tDouble^;
        end;
      end;
  btSingle :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tSingle^ > Param2^.tu8^;
        btS8        : result := Param1^.tSingle^ > Param2^.ts8^;
        btU16       : result := Param1^.tSingle^ > Param2^.tu16^;
        btS16       : result := Param1^.tSingle^ > Param2^.ts16^;
        btU32       : result := Param1^.tSingle^ > Param2^.tu32^;
        btS32       : result := Param1^.tSingle^ > Param2^.ts32^;
        btS64       : result := Param1^.tSingle^ > Param2^.ts64^;
        btSingle    : result := Param1^.tSingle^ > Param2^.tSingle^;
        btDouble    : result := Param1^.tSingle^ > Param2^.tDouble^;
        end;
      end;
  btDouble :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tDouble^ > Param2^.tu8^;
        btS8        : result := Param1^.tDouble^ > Param2^.ts8^;
        btU16       : result := Param1^.tDouble^ > Param2^.tu16^;
        btS16       : result := Param1^.tDouble^ > Param2^.ts16^;
        btU32       : result := Param1^.tDouble^ > Param2^.tu32^;
        btS32       : result := Param1^.tDouble^ > Param2^.ts32^;
        btS64       : result := Param1^.tDouble^ > Param2^.ts64^;
        btSingle    : result := Param1^.tDouble^ > Param2^.tSingle^;
        btDouble    : result := Param1^.tDouble^ > Param2^.tDouble^;
        end;      
      end;
  btString :
      begin
        case Param2.AType of
        btString       : result := PbtString(Param1^.tString^)^ > PbtString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btString);
              result := PbtString(Param1^.tString^)^ > PbtString(Param2^.tString^)^;
            end;
        end;
      end;
  btUTF8String :
      begin
        case Param2.AType of
        btString       : result := PbtUTF8String(Param1^.tString^)^ > PbtUTF8String(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btUTF8String);
              result := PbtUTF8String(Param1^.tString^)^ > PbtUTF8String(Param2^.tString^)^;
            end;
        end;
      end;
  btWideString :
      begin
        case Param2.AType of
        btString       : result := PbtWideString(Param1^.tString^)^ > PbtWideString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btWideString);
              result := PbtWideString(Param1^.tString^)^ > PbtWideString(Param2^.tString^)^;
            end;
        end;
      end;
  btPChar :
      begin
        case Param2.AType of
        btString       : result := PbtPChar(Param1^.tString^)^ > PbtPChar(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btPChar);
              result := PbtPChar(Param1^.tString^)^ > PbtPChar(Param2^.tString^)^;
            end;
        end;
      end;
  end;
end;

function TSE2VarCompare.BiggerEqual(Param1,
  Param2: PSE2VarData): boolean;
begin
  result := False;
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu8^ >= Param2^.tu8^;
        btS8        : result := Param1^.tu8^ >= Param2^.ts8^;
        btU16       : result := Param1^.tu8^ >= Param2^.tu16^;
        btS16       : result := Param1^.tu8^ >= Param2^.ts16^;
        btU32       : result := Param1^.tu8^ >= Param2^.tu32^;
        btS32       : result := Param1^.tu8^ >= Param2^.ts32^;
        btS64       : result := Param1^.tu8^ >= Param2^.ts64^;
        btSingle    : result := Param1^.tu8^ >= Param2^.tSingle^;
        btDouble    : result := Param1^.tu8^ >= Param2^.tDouble^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts8^ >= Param2^.tu8^;
        btS8        : result := Param1^.ts8^ >= Param2^.ts8^;
        btU16       : result := Param1^.ts8^ >= Param2^.tu16^;
        btS16       : result := Param1^.ts8^ >= Param2^.ts16^;
        btU32       : result := Param1^.ts8^ >= Param2^.tu32^;
        btS32       : result := Param1^.ts8^ >= Param2^.ts32^;
        btS64       : result := Param1^.ts8^ >= Param2^.ts64^;
        btSingle    : result := Param1^.ts8^ >= Param2^.tSingle^;
        btDouble    : result := Param1^.ts8^ >= Param2^.tDouble^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu16^ >= Param2^.tu8^;
        btS8        : result := Param1^.tu16^ >= Param2^.ts8^;
        btU16       : result := Param1^.tu16^ >= Param2^.tu16^;
        btS16       : result := Param1^.tu16^ >= Param2^.ts16^;
        btU32       : result := Param1^.tu16^ >= Param2^.tu32^;
        btS32       : result := Param1^.tu16^ >= Param2^.ts32^;
        btS64       : result := Param1^.tu16^ >= Param2^.ts64^;
        btSingle    : result := Param1^.tu16^ >= Param2^.tSingle^;
        btDouble    : result := Param1^.tu16^ >= Param2^.tDouble^;
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts16^ >= Param2^.tu8^;
        btS8        : result := Param1^.ts16^ >= Param2^.ts8^;
        btU16       : result := Param1^.ts16^ >= Param2^.tu16^;
        btS16       : result := Param1^.ts16^ >= Param2^.ts16^;
        btU32       : result := Param1^.ts16^ >= Param2^.tu32^;
        btS32       : result := Param1^.ts16^ >= Param2^.ts32^;
        btS64       : result := Param1^.ts16^ >= Param2^.ts64^;
        btSingle    : result := Param1^.ts16^ >= Param2^.tSingle^;
        btDouble    : result := Param1^.ts16^ >= Param2^.tDouble^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu32^ >= Param2^.tu8^;
        btS8        : result := Param1^.tu32^ >= Param2^.ts8^;
        btU16       : result := Param1^.tu32^ >= Param2^.tu16^;
        btS16       : result := Param1^.tu32^ >= Param2^.ts16^;
        btU32       : result := Param1^.tu32^ >= Param2^.tu32^;
        btS32       : result := Param1^.tu32^ >= Param2^.ts32^;
        btS64       : result := Param1^.tu32^ >= Param2^.ts64^;
        btSingle    : result := Param1^.tu32^ >= Param2^.tSingle^;
        btDouble    : result := Param1^.tu32^ >= Param2^.tDouble^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts32^ >= Param2^.tu8^;
        btS8        : result := Param1^.ts32^ >= Param2^.ts8^;
        btU16       : result := Param1^.ts32^ >= Param2^.tu16^;
        btS16       : result := Param1^.ts32^ >= Param2^.ts16^;
        btU32       : result := Param1^.ts32^ >= Param2^.tu32^;
        btS32       : result := Param1^.ts32^ >= Param2^.ts32^;
        btS64       : result := Param1^.ts32^ >= Param2^.ts64^;
        btSingle    : result := Param1^.ts32^ >= Param2^.tSingle^;
        btDouble    : result := Param1^.ts32^ >= Param2^.tDouble^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts64^ >= Param2^.tu8^;
        btS8        : result := Param1^.ts64^ >= Param2^.ts8^;
        btU16       : result := Param1^.ts64^ >= Param2^.tu16^;
        btS16       : result := Param1^.ts64^ >= Param2^.ts16^;
        btU32       : result := Param1^.ts64^ >= Param2^.tu32^;
        btS32       : result := Param1^.ts64^ >= Param2^.ts32^;
        btS64       : result := Param1^.ts64^ >= Param2^.ts64^;
        btSingle    : result := Param1^.ts64^ >= Param2^.tSingle^;
        btDouble    : result := Param1^.ts64^ >= Param2^.tDouble^;
        end;
      end;
  btSingle :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tSingle^ >= Param2^.tu8^;
        btS8        : result := Param1^.tSingle^ >= Param2^.ts8^;
        btU16       : result := Param1^.tSingle^ >= Param2^.tu16^;
        btS16       : result := Param1^.tSingle^ >= Param2^.ts16^;
        btU32       : result := Param1^.tSingle^ >= Param2^.tu32^;
        btS32       : result := Param1^.tSingle^ >= Param2^.ts32^;
        btS64       : result := Param1^.tSingle^ >= Param2^.ts64^;
        btSingle    : result := Param1^.tSingle^ >= Param2^.tSingle^;
        btDouble    : result := Param1^.tSingle^ >= Param2^.tDouble^;
        end;
      end;
  btDouble :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tDouble^ >= Param2^.tu8^;
        btS8        : result := Param1^.tDouble^ >= Param2^.ts8^;
        btU16       : result := Param1^.tDouble^ >= Param2^.tu16^;
        btS16       : result := Param1^.tDouble^ >= Param2^.ts16^;
        btU32       : result := Param1^.tDouble^ >= Param2^.tu32^;
        btS32       : result := Param1^.tDouble^ >= Param2^.ts32^;
        btS64       : result := Param1^.tDouble^ >= Param2^.ts64^;
        btSingle    : result := Param1^.tDouble^ >= Param2^.tSingle^;
        btDouble    : result := Param1^.tDouble^ >= Param2^.tDouble^;
        end;      
      end;
  btString :
      begin
        case Param2.AType of
        btString       : result := PbtString(Param1^.tString^)^ >= PbtString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btString);
              result := PbtString(Param1^.tString^)^ >= PbtString(Param2^.tString^)^;
            end;
        end;
      end;
  btUTF8String :
      begin
        case Param2.AType of
        btString       : result := PbtUTF8String(Param1^.tString^)^ >= PbtUTF8String(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btUTF8String);
              result := PbtUTF8String(Param1^.tString^)^ >= PbtUTF8String(Param2^.tString^)^;
            end;
        end;
      end;
  btWideString :
      begin
        case Param2.AType of
        btString       : result := PbtWideString(Param1^.tString^)^ >= PbtWideString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btWideString);
              result := PbtWideString(Param1^.tString^)^ >= PbtWideString(Param2^.tString^)^;
            end;
        end;
      end;
  btPChar :
      begin
        case Param2.AType of
        btString       : result := PbtPChar(Param1^.tString^)^ >= PbtPChar(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btPChar);
              result := PbtPChar(Param1^.tString^)^ >= PbtPChar(Param2^.tString^)^;
            end;
        end;
      end;
  end;
end;

constructor TSE2VarCompare.Create(VarHelper: TSE2VarHelper);
begin
  inherited Create;
  FVarHelp := VarHelper;
end;

function TSE2VarCompare.Equal(Param1, Param2: PSE2VarData): boolean;
begin
  result := False;
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu8^ = Param2^.tu8^;
        btS8        : result := Param1^.tu8^ = Param2^.ts8^;
        btU16       : result := Param1^.tu8^ = Param2^.tu16^;
        btS16       : result := Param1^.tu8^ = Param2^.ts16^;
        btU32       : result := Param1^.tu8^ = Param2^.tu32^;
        btS32       : result := Param1^.tu8^ = Param2^.ts32^;
        btS64       : result := Param1^.tu8^ = Param2^.ts64^;
        btSingle    : result := Param1^.tu8^ = Param2^.tSingle^;
        btDouble    : result := Param1^.tu8^ = Param2^.tDouble^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts8^ = Param2^.tu8^;
        btS8        : result := Param1^.ts8^ = Param2^.ts8^;
        btU16       : result := Param1^.ts8^ = Param2^.tu16^;
        btS16       : result := Param1^.ts8^ = Param2^.ts16^;
        btU32       : result := Param1^.ts8^ = Param2^.tu32^;
        btS32       : result := Param1^.ts8^ = Param2^.ts32^;
        btS64       : result := Param1^.ts8^ = Param2^.ts64^;
        btSingle    : result := Param1^.ts8^ = Param2^.tSingle^;
        btDouble    : result := Param1^.ts8^ = Param2^.tDouble^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu16^ = Param2^.tu8^;
        btS8        : result := Param1^.tu16^ = Param2^.ts8^;
        btU16       : result := Param1^.tu16^ = Param2^.tu16^;
        btS16       : result := Param1^.tu16^ = Param2^.ts16^;
        btU32       : result := Param1^.tu16^ = Param2^.tu32^;
        btS32       : result := Param1^.tu16^ = Param2^.ts32^;
        btS64       : result := Param1^.tu16^ = Param2^.ts64^;
        btSingle    : result := Param1^.tu16^ = Param2^.tSingle^;
        btDouble    : result := Param1^.tu16^ = Param2^.tDouble^;
        end;        
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts16^ = Param2^.tu8^;
        btS8        : result := Param1^.ts16^ = Param2^.ts8^;
        btU16       : result := Param1^.ts16^ = Param2^.tu16^;
        btS16       : result := Param1^.ts16^ = Param2^.ts16^;
        btU32       : result := Param1^.ts16^ = Param2^.tu32^;
        btS32       : result := Param1^.ts16^ = Param2^.ts32^;
        btS64       : result := Param1^.ts16^ = Param2^.ts64^;
        btSingle    : result := Param1^.ts16^ = Param2^.tSingle^;
        btDouble    : result := Param1^.ts16^ = Param2^.tDouble^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu32^ = Param2^.tu8^;
        btS8        : result := Param1^.tu32^ = Param2^.ts8^;
        btU16       : result := Param1^.tu32^ = Param2^.tu16^;
        btS16       : result := Param1^.tu32^ = Param2^.ts16^;
        btU32       : result := Param1^.tu32^ = Param2^.tu32^;
        btS32       : result := Param1^.tu32^ = Param2^.ts32^;
        btS64       : result := Param1^.tu32^ = Param2^.ts64^;
        btSingle    : result := Param1^.tu32^ = Param2^.tSingle^;
        btDouble    : result := Param1^.tu32^ = Param2^.tDouble^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts32^ = Param2^.tu8^;
        btS8        : result := Param1^.ts32^ = Param2^.ts8^;
        btU16       : result := Param1^.ts32^ = Param2^.tu16^;
        btS16       : result := Param1^.ts32^ = Param2^.ts16^;
        btU32       : result := Param1^.ts32^ = Param2^.tu32^;
        btS32       : result := Param1^.ts32^ = Param2^.ts32^;
        btS64       : result := Param1^.ts32^ = Param2^.ts64^;
        btSingle    : result := Param1^.ts32^ = Param2^.tSingle^;
        btDouble    : result := Param1^.ts32^ = Param2^.tDouble^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts64^ = Param2^.tu8^;
        btS8        : result := Param1^.ts64^ = Param2^.ts8^;
        btU16       : result := Param1^.ts64^ = Param2^.tu16^;
        btS16       : result := Param1^.ts64^ = Param2^.ts16^;
        btU32       : result := Param1^.ts64^ = Param2^.tu32^;
        btS32       : result := Param1^.ts64^ = Param2^.ts32^;
        btS64       : result := Param1^.ts64^ = Param2^.ts64^;
        btSingle    : result := Param1^.ts64^ = Param2^.tSingle^;
        btDouble    : result := Param1^.ts64^ = Param2^.tDouble^;
        end;
      end;
  btSingle :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tSingle^ = Param2^.tu8^;
        btS8        : result := Param1^.tSingle^ = Param2^.ts8^;
        btU16       : result := Param1^.tSingle^ = Param2^.tu16^;
        btS16       : result := Param1^.tSingle^ = Param2^.ts16^;
        btU32       : result := Param1^.tSingle^ = Param2^.tu32^;
        btS32       : result := Param1^.tSingle^ = Param2^.ts32^;
        btS64       : result := Param1^.tSingle^ = Param2^.ts64^;
        btSingle    : result := Param1^.tSingle^ = Param2^.tSingle^;
        btDouble    : result := Param1^.tSingle^ = Param2^.tDouble^;
        end;
      end;
  btDouble :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tDouble^ = Param2^.tu8^;
        btS8        : result := Param1^.tDouble^ = Param2^.ts8^;
        btU16       : result := Param1^.tDouble^ = Param2^.tu16^;
        btS16       : result := Param1^.tDouble^ = Param2^.ts16^;
        btU32       : result := Param1^.tDouble^ = Param2^.tu32^;
        btS32       : result := Param1^.tDouble^ = Param2^.ts32^;
        btS64       : result := Param1^.tDouble^ = Param2^.ts64^;
        btSingle    : result := Param1^.tDouble^ = Param2^.tSingle^;
        btDouble    : result := Param1^.tDouble^ = Param2^.tDouble^;
        end;      
      end;
  btPointer, btObject, btProcPtr :
      begin
        case Param2.AType of
        btPointer,
        btProcPtr,
        btObject        : result := Pointer(Param1^.tPointer^) = Pointer(Param2^.tPointer^);
        end;
      end;  
  btString :
      begin
        case Param2.AType of
        btString       : result := PbtString(Param1^.tString^)^ = PbtString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btString);
              result := PbtString(Param1^.tString^)^ = PbtString(Param2^.tString^)^;
            end;
        end;
      end;
  btUTF8String :
      begin
        case Param2.AType of
        btString       : result := PbtUTF8String(Param1^.tString^)^ = PbtUTF8String(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btUTF8String);
              result := PbtUTF8String(Param1^.tString^)^ = PbtUTF8String(Param2^.tString^)^;
            end;
        end;
      end;
  btWideString :
      begin
        case Param2.AType of
        btString       : result := PbtWideString(Param1^.tString^)^ = PbtWideString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btWideString);
              result := PbtWideString(Param1^.tString^)^ = PbtWideString(Param2^.tString^)^;
            end;
        end;     
      end;
  btPChar :
      begin
        case Param2.AType of
        btString       : result := PbtPChar(Param1^.tString^)^ = PbtPChar(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btPChar);
              result := PbtPChar(Param1^.tString^)^ = PbtPChar(Param2^.tString^)^;
            end;
        end;
      end;
  end;
end;

function TSE2VarCompare.Smaller(Param1,
  Param2: PSE2VarData): boolean;
begin
  result := False;
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu8^ < Param2^.tu8^;
        btS8        : result := Param1^.tu8^ < Param2^.ts8^;
        btU16       : result := Param1^.tu8^ < Param2^.tu16^;
        btS16       : result := Param1^.tu8^ < Param2^.ts16^;
        btU32       : result := Param1^.tu8^ < Param2^.tu32^;
        btS32       : result := Param1^.tu8^ < Param2^.ts32^;
        btS64       : result := Param1^.tu8^ < Param2^.ts64^;
        btSingle    : result := Param1^.tu8^ < Param2^.tSingle^;
        btDouble    : result := Param1^.tu8^ < Param2^.tDouble^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts8^ < Param2^.tu8^;
        btS8        : result := Param1^.ts8^ < Param2^.ts8^;
        btU16       : result := Param1^.ts8^ < Param2^.tu16^;
        btS16       : result := Param1^.ts8^ < Param2^.ts16^;
        btU32       : result := Param1^.ts8^ < Param2^.tu32^;
        btS32       : result := Param1^.ts8^ < Param2^.ts32^;
        btS64       : result := Param1^.ts8^ < Param2^.ts64^;
        btSingle    : result := Param1^.ts8^ < Param2^.tSingle^;
        btDouble    : result := Param1^.ts8^ < Param2^.tDouble^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu16^ < Param2^.tu8^;
        btS8        : result := Param1^.tu16^ < Param2^.ts8^;
        btU16       : result := Param1^.tu16^ < Param2^.tu16^;
        btS16       : result := Param1^.tu16^ < Param2^.ts16^;
        btU32       : result := Param1^.tu16^ < Param2^.tu32^;
        btS32       : result := Param1^.tu16^ < Param2^.ts32^;
        btS64       : result := Param1^.tu16^ < Param2^.ts64^;
        btSingle    : result := Param1^.tu16^ < Param2^.tSingle^;
        btDouble    : result := Param1^.tu16^ < Param2^.tDouble^;
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts16^ < Param2^.tu8^;
        btS8        : result := Param1^.ts16^ < Param2^.ts8^;
        btU16       : result := Param1^.ts16^ < Param2^.tu16^;
        btS16       : result := Param1^.ts16^ < Param2^.ts16^;
        btU32       : result := Param1^.ts16^ < Param2^.tu32^;
        btS32       : result := Param1^.ts16^ < Param2^.ts32^;
        btS64       : result := Param1^.ts16^ < Param2^.ts64^;
        btSingle    : result := Param1^.ts16^ < Param2^.tSingle^;
        btDouble    : result := Param1^.ts16^ < Param2^.tDouble^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu32^ < Param2^.tu8^;
        btS8        : result := Param1^.tu32^ < Param2^.ts8^;
        btU16       : result := Param1^.tu32^ < Param2^.tu16^;
        btS16       : result := Param1^.tu32^ < Param2^.ts16^;
        btU32       : result := Param1^.tu32^ < Param2^.tu32^;
        btS32       : result := Param1^.tu32^ < Param2^.ts32^;
        btS64       : result := Param1^.tu32^ < Param2^.ts64^;
        btSingle    : result := Param1^.tu32^ < Param2^.tSingle^;
        btDouble    : result := Param1^.tu32^ < Param2^.tDouble^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts32^ < Param2^.tu8^;
        btS8        : result := Param1^.ts32^ < Param2^.ts8^;
        btU16       : result := Param1^.ts32^ < Param2^.tu16^;
        btS16       : result := Param1^.ts32^ < Param2^.ts16^;
        btU32       : result := Param1^.ts32^ < Param2^.tu32^;
        btS32       : result := Param1^.ts32^ < Param2^.ts32^;
        btS64       : result := Param1^.ts32^ < Param2^.ts64^;
        btSingle    : result := Param1^.ts32^ < Param2^.tSingle^;
        btDouble    : result := Param1^.ts32^ < Param2^.tDouble^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts64^ < Param2^.tu8^;
        btS8        : result := Param1^.ts64^ < Param2^.ts8^;
        btU16       : result := Param1^.ts64^ < Param2^.tu16^;
        btS16       : result := Param1^.ts64^ < Param2^.ts16^;
        btU32       : result := Param1^.ts64^ < Param2^.tu32^;
        btS32       : result := Param1^.ts64^ < Param2^.ts32^;
        btS64       : result := Param1^.ts64^ < Param2^.ts64^;
        btSingle    : result := Param1^.ts64^ < Param2^.tSingle^;
        btDouble    : result := Param1^.ts64^ < Param2^.tDouble^;
        end;
      end;
  btSingle :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tSingle^ < Param2^.tu8^;
        btS8        : result := Param1^.tSingle^ < Param2^.ts8^;
        btU16       : result := Param1^.tSingle^ < Param2^.tu16^;
        btS16       : result := Param1^.tSingle^ < Param2^.ts16^;
        btU32       : result := Param1^.tSingle^ < Param2^.tu32^;
        btS32       : result := Param1^.tSingle^ < Param2^.ts32^;
        btS64       : result := Param1^.tSingle^ < Param2^.ts64^;
        btSingle    : result := Param1^.tSingle^ < Param2^.tSingle^;
        btDouble    : result := Param1^.tSingle^ < Param2^.tDouble^;
        end;
      end;
  btDouble :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tDouble^ < Param2^.tu8^;
        btS8        : result := Param1^.tDouble^ < Param2^.ts8^;
        btU16       : result := Param1^.tDouble^ < Param2^.tu16^;
        btS16       : result := Param1^.tDouble^ < Param2^.ts16^;
        btU32       : result := Param1^.tDouble^ < Param2^.tu32^;
        btS32       : result := Param1^.tDouble^ < Param2^.ts32^;
        btS64       : result := Param1^.tDouble^ < Param2^.ts64^;
        btSingle    : result := Param1^.tDouble^ < Param2^.tSingle^;
        btDouble    : result := Param1^.tDouble^ < Param2^.tDouble^;
        end;      
      end;
  btString :
      begin
        case Param2.AType of
        btString       : result := PbtString(Param1^.tString^)^ < PbtString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btString);
              result := PbtString(Param1^.tString^)^ < PbtString(Param2^.tString^)^;
            end;
        end;
      end;
  btUTF8String :
      begin
        case Param2.AType of
        btString       : result := PbtUTF8String(Param1^.tString^)^ < PbtUTF8String(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btUTF8String);
              result := PbtUTF8String(Param1^.tString^)^ < PbtUTF8String(Param2^.tString^)^;
            end;
        end;
      end;
  btWideString :
      begin
        case Param2.AType of
        btString       : result := PbtWideString(Param1^.tString^)^ < PbtWideString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btWideString);
              result := PbtWideString(Param1^.tString^)^ < PbtWideString(Param2^.tString^)^;
            end;
        end;
      end;
  btPChar :
      begin
        case Param2.AType of
        btString       : result := PbtPChar(Param1^.tString^)^ < PbtPChar(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btPChar);
              result := PbtPChar(Param1^.tString^)^ < PbtPChar(Param2^.tString^)^;
            end;
        end;
      end;
  end;
end;

function TSE2VarCompare.SmallerEqual(Param1,
  Param2: PSE2VarData): boolean;
begin
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu8^ <= Param2^.tu8^;
        btS8        : result := Param1^.tu8^ <= Param2^.ts8^;
        btU16       : result := Param1^.tu8^ <= Param2^.tu16^;
        btS16       : result := Param1^.tu8^ <= Param2^.ts16^;
        btU32       : result := Param1^.tu8^ <= Param2^.tu32^;
        btS32       : result := Param1^.tu8^ <= Param2^.ts32^;
        btS64       : result := Param1^.tu8^ <= Param2^.ts64^;
        btSingle    : result := Param1^.tu8^ <= Param2^.tSingle^;
        btDouble    : result := Param1^.tu8^ <= Param2^.tDouble^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts8^ <= Param2^.tu8^;
        btS8        : result := Param1^.ts8^ <= Param2^.ts8^;
        btU16       : result := Param1^.ts8^ <= Param2^.tu16^;
        btS16       : result := Param1^.ts8^ <= Param2^.ts16^;
        btU32       : result := Param1^.ts8^ <= Param2^.tu32^;
        btS32       : result := Param1^.ts8^ <= Param2^.ts32^;
        btS64       : result := Param1^.ts8^ <= Param2^.ts64^;
        btSingle    : result := Param1^.ts8^ <= Param2^.tSingle^;
        btDouble    : result := Param1^.ts8^ <= Param2^.tDouble^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu16^ <= Param2^.tu8^;
        btS8        : result := Param1^.tu16^ <= Param2^.ts8^;
        btU16       : result := Param1^.tu16^ <= Param2^.tu16^;
        btS16       : result := Param1^.tu16^ <= Param2^.ts16^;
        btU32       : result := Param1^.tu16^ <= Param2^.tu32^;
        btS32       : result := Param1^.tu16^ <= Param2^.ts32^;
        btS64       : result := Param1^.tu16^ <= Param2^.ts64^;
        btSingle    : result := Param1^.tu16^ <= Param2^.tSingle^;
        btDouble    : result := Param1^.tu16^ <= Param2^.tDouble^;
        end;
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts16^ <= Param2^.tu8^;
        btS8        : result := Param1^.ts16^ <= Param2^.ts8^;
        btU16       : result := Param1^.ts16^ <= Param2^.tu16^;
        btS16       : result := Param1^.ts16^ <= Param2^.ts16^;
        btU32       : result := Param1^.ts16^ <= Param2^.tu32^;
        btS32       : result := Param1^.ts16^ <= Param2^.ts32^;
        btS64       : result := Param1^.ts16^ <= Param2^.ts64^;
        btSingle    : result := Param1^.ts16^ <= Param2^.tSingle^;
        btDouble    : result := Param1^.ts16^ <= Param2^.tDouble^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu32^ <= Param2^.tu8^;
        btS8        : result := Param1^.tu32^ <= Param2^.ts8^;
        btU16       : result := Param1^.tu32^ <= Param2^.tu16^;
        btS16       : result := Param1^.tu32^ <= Param2^.ts16^;
        btU32       : result := Param1^.tu32^ <= Param2^.tu32^;
        btS32       : result := Param1^.tu32^ <= Param2^.ts32^;
        btS64       : result := Param1^.tu32^ <= Param2^.ts64^;
        btSingle    : result := Param1^.tu32^ <= Param2^.tSingle^;
        btDouble    : result := Param1^.tu32^ <= Param2^.tDouble^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts32^ <= Param2^.tu8^;
        btS8        : result := Param1^.ts32^ <= Param2^.ts8^;
        btU16       : result := Param1^.ts32^ <= Param2^.tu16^;
        btS16       : result := Param1^.ts32^ <= Param2^.ts16^;
        btU32       : result := Param1^.ts32^ <= Param2^.tu32^;
        btS32       : result := Param1^.ts32^ <= Param2^.ts32^;
        btS64       : result := Param1^.ts32^ <= Param2^.ts64^;
        btSingle    : result := Param1^.ts32^ <= Param2^.tSingle^;
        btDouble    : result := Param1^.ts32^ <= Param2^.tDouble^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts64^ <= Param2^.tu8^;
        btS8        : result := Param1^.ts64^ <= Param2^.ts8^;
        btU16       : result := Param1^.ts64^ <= Param2^.tu16^;
        btS16       : result := Param1^.ts64^ <= Param2^.ts16^;
        btU32       : result := Param1^.ts64^ <= Param2^.tu32^;
        btS32       : result := Param1^.ts64^ <= Param2^.ts32^;
        btS64       : result := Param1^.ts64^ <= Param2^.ts64^;
        btSingle    : result := Param1^.ts64^ <= Param2^.tSingle^;
        btDouble    : result := Param1^.ts64^ <= Param2^.tDouble^;
        end;
      end;
  btSingle :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tSingle^ <= Param2^.tu8^;
        btS8        : result := Param1^.tSingle^ <= Param2^.ts8^;
        btU16       : result := Param1^.tSingle^ <= Param2^.tu16^;
        btS16       : result := Param1^.tSingle^ <= Param2^.ts16^;
        btU32       : result := Param1^.tSingle^ <= Param2^.tu32^;
        btS32       : result := Param1^.tSingle^ <= Param2^.ts32^;
        btS64       : result := Param1^.tSingle^ <= Param2^.ts64^;
        btSingle    : result := Param1^.tSingle^ <= Param2^.tSingle^;
        btDouble    : result := Param1^.tSingle^ <= Param2^.tDouble^;
        end;
      end;
  btDouble :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tDouble^ <= Param2^.tu8^;
        btS8        : result := Param1^.tDouble^ <= Param2^.ts8^;
        btU16       : result := Param1^.tDouble^ <= Param2^.tu16^;
        btS16       : result := Param1^.tDouble^ <= Param2^.ts16^;
        btU32       : result := Param1^.tDouble^ <= Param2^.tu32^;
        btS32       : result := Param1^.tDouble^ <= Param2^.ts32^;
        btS64       : result := Param1^.tDouble^ <= Param2^.ts64^;
        btSingle    : result := Param1^.tDouble^ <= Param2^.tSingle^;
        btDouble    : result := Param1^.tDouble^ <= Param2^.tDouble^;
        end;      
      end;
  btString :
      begin
        case Param2.AType of
        btString       : result := PbtString(Param1^.tString^)^ <= PbtString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btString);
              result := PbtString(Param1^.tString^)^ <= PbtString(Param2^.tString^)^;
            end;
        end;
      end;
  btUTF8String :
      begin
        case Param2.AType of
        btString       : result := PbtUTF8String(Param1^.tString^)^ <= PbtUTF8String(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btUTF8String);
              result := PbtUTF8String(Param1^.tString^)^ <= PbtUTF8String(Param2^.tString^)^;
            end;
        end;
      end;
  btWideString :
      begin
        case Param2.AType of
        btString       : result := PbtWideString(Param1^.tString^)^ <= PbtWideString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btWideString);
              result := PbtWideString(Param1^.tString^)^ <= PbtWideString(Param2^.tString^)^;
            end;
        end;
      end;
  btPChar :
      begin
        case Param2.AType of
        btString       : result := PbtPChar(Param1^.tString^)^ <= PbtPChar(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btPChar);
              result := PbtPChar(Param1^.tString^)^ <= PbtPChar(Param2^.tString^)^;
            end;
        end;
      end;
  else result := False;
  end;
end;

function TSE2VarCompare.UnEqual(Param1,
  Param2: PSE2VarData): boolean;
begin
  result := False;
  case Param1.AType of
  btU8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu8^ <> Param2^.tu8^;
        btS8        : result := Param1^.tu8^ <> Param2^.ts8^;
        btU16       : result := Param1^.tu8^ <> Param2^.tu16^;
        btS16       : result := Param1^.tu8^ <> Param2^.ts16^;
        btU32       : result := Param1^.tu8^ <> Param2^.tu32^;
        btS32       : result := Param1^.tu8^ <> Param2^.ts32^;
        btS64       : result := Param1^.tu8^ <> Param2^.ts64^;
        btSingle    : result := Param1^.tu8^ <> Param2^.tSingle^;
        btDouble    : result := Param1^.tu8^ <> Param2^.tDouble^;
        end;
      end;
  btS8 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts8^ <> Param2^.tu8^;
        btS8        : result := Param1^.ts8^ <> Param2^.ts8^;
        btU16       : result := Param1^.ts8^ <> Param2^.tu16^;
        btS16       : result := Param1^.ts8^ <> Param2^.ts16^;
        btU32       : result := Param1^.ts8^ <> Param2^.tu32^;
        btS32       : result := Param1^.ts8^ <> Param2^.ts32^;
        btS64       : result := Param1^.ts8^ <> Param2^.ts64^;
        btSingle    : result := Param1^.ts8^ <> Param2^.tSingle^;
        btDouble    : result := Param1^.ts8^ <> Param2^.tDouble^;
        end;
      end;
  btU16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu16^ <> Param2^.tu8^;
        btS8        : result := Param1^.tu16^ <> Param2^.ts8^;
        btU16       : result := Param1^.tu16^ <> Param2^.tu16^;
        btS16       : result := Param1^.tu16^ <> Param2^.ts16^;
        btU32       : result := Param1^.tu16^ <> Param2^.tu32^;
        btS32       : result := Param1^.tu16^ <> Param2^.ts32^;
        btS64       : result := Param1^.tu16^ <> Param2^.ts64^;
        btSingle    : result := Param1^.tu16^ <> Param2^.tSingle^;
        btDouble    : result := Param1^.tu16^ <> Param2^.tDouble^;
        end;        
      end;
  btS16 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts16^ <> Param2^.tu8^;
        btS8        : result := Param1^.ts16^ <> Param2^.ts8^;
        btU16       : result := Param1^.ts16^ <> Param2^.tu16^;
        btS16       : result := Param1^.ts16^ <> Param2^.ts16^;
        btU32       : result := Param1^.ts16^ <> Param2^.tu32^;
        btS32       : result := Param1^.ts16^ <> Param2^.ts32^;
        btS64       : result := Param1^.ts16^ <> Param2^.ts64^;
        btSingle    : result := Param1^.ts16^ <> Param2^.tSingle^;
        btDouble    : result := Param1^.ts16^ <> Param2^.tDouble^;
        end;
      end;
  btU32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tu32^ <> Param2^.tu8^;
        btS8        : result := Param1^.tu32^ <> Param2^.ts8^;
        btU16       : result := Param1^.tu32^ <> Param2^.tu16^;
        btS16       : result := Param1^.tu32^ <> Param2^.ts16^;
        btU32       : result := Param1^.tu32^ <> Param2^.tu32^;
        btS32       : result := Param1^.tu32^ <> Param2^.ts32^;
        btS64       : result := Param1^.tu32^ <> Param2^.ts64^;
        btSingle    : result := Param1^.tu32^ <> Param2^.tSingle^;
        btDouble    : result := Param1^.tu32^ <> Param2^.tDouble^;
        end;
      end;
  btS32 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts32^ <> Param2^.tu8^;
        btS8        : result := Param1^.ts32^ <> Param2^.ts8^;
        btU16       : result := Param1^.ts32^ <> Param2^.tu16^;
        btS16       : result := Param1^.ts32^ <> Param2^.ts16^;
        btU32       : result := Param1^.ts32^ <> Param2^.tu32^;
        btS32       : result := Param1^.ts32^ <> Param2^.ts32^;
        btS64       : result := Param1^.ts32^ <> Param2^.ts64^;
        btSingle    : result := Param1^.ts32^ <> Param2^.tSingle^;
        btDouble    : result := Param1^.ts32^ <> Param2^.tDouble^;
        end;
      end;
  btS64 :
      begin
        case Param2.AType of
        btU8        : result := Param1^.ts64^ <> Param2^.tu8^;
        btS8        : result := Param1^.ts64^ <> Param2^.ts8^;
        btU16       : result := Param1^.ts64^ <> Param2^.tu16^;
        btS16       : result := Param1^.ts64^ <> Param2^.ts16^;
        btU32       : result := Param1^.ts64^ <> Param2^.tu32^;
        btS32       : result := Param1^.ts64^ <> Param2^.ts32^;
        btS64       : result := Param1^.ts64^ <> Param2^.ts64^;
        btSingle    : result := Param1^.ts64^ <> Param2^.tSingle^;
        btDouble    : result := Param1^.ts64^ <> Param2^.tDouble^;
        end;
      end;
  btSingle :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tSingle^ <> Param2^.tu8^;
        btS8        : result := Param1^.tSingle^ <> Param2^.ts8^;
        btU16       : result := Param1^.tSingle^ <> Param2^.tu16^;
        btS16       : result := Param1^.tSingle^ <> Param2^.ts16^;
        btU32       : result := Param1^.tSingle^ <> Param2^.tu32^;
        btS32       : result := Param1^.tSingle^ <> Param2^.ts32^;
        btS64       : result := Param1^.tSingle^ <> Param2^.ts64^;
        btSingle    : result := Param1^.tSingle^ <> Param2^.tSingle^;
        btDouble    : result := Param1^.tSingle^ <> Param2^.tDouble^;
        end;
      end;
  btDouble :
      begin
        case Param2.AType of
        btU8        : result := Param1^.tDouble^ <> Param2^.tu8^;
        btS8        : result := Param1^.tDouble^ <> Param2^.ts8^;
        btU16       : result := Param1^.tDouble^ <> Param2^.tu16^;
        btS16       : result := Param1^.tDouble^ <> Param2^.ts16^;
        btU32       : result := Param1^.tDouble^ <> Param2^.tu32^;
        btS32       : result := Param1^.tDouble^ <> Param2^.ts32^;
        btS64       : result := Param1^.tDouble^ <> Param2^.ts64^;
        btSingle    : result := Param1^.tDouble^ <> Param2^.tSingle^;
        btDouble    : result := Param1^.tDouble^ <> Param2^.tDouble^;
        end;      
      end;
  btPointer, btObject, btProcPtr :
      begin
        case Param2.AType of
        btPointer,
        btProcPtr,
        btObject        : result := Pointer(Param1^.tPointer^) <> Pointer(Param2^.tPointer^);
        end;
      end;
  btString :
      begin
        case Param2.AType of
        btString       : result := PbtString(Param1^.tString^)^ <> PbtString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btString);
              result := PbtString(Param1^.tString^)^ <> PbtString(Param2^.tString^)^;
            end;
        end;
      end;
  btUTF8String :
      begin
        case Param2.AType of
        btString       : result := PbtUTF8String(Param1^.tString^)^ <> PbtUTF8String(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btUTF8String);
              result := PbtUTF8String(Param1^.tString^)^ <> PbtUTF8String(Param2^.tString^)^;
            end;
        end;
      end;
  btWideString :
      begin
        case Param2.AType of
        btString       : result := PbtWideString(Param1^.tString^)^ <> PbtWideString(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btWideString);
              result := PbtWideString(Param1^.tString^)^ <> PbtWideString(Param2^.tString^)^;
            end;
        end;
      end;
  btPChar :
      begin
        case Param2.AType of
        btString       : result := PbtPChar(Param1^.tString^)^ <> PbtPChar(Param2^.tString^)^;
        btUTF8String,
        btWideString,
        btPChar        :
            begin
              FVarHelp.ConvertContent(Param2, btPChar);
              result := PbtPChar(Param1^.tString^)^ <> PbtPChar(Param2^.tString^)^;
            end;
        end;
      end;
  end;
end;

end.
