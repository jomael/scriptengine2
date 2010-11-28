unit uSE2Optimizer;  

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2OpCode, uSE2Types;

type
  TSE2OpCodeArrayList = array[0..10] of TSE2LinkOpCode;
  PSE2OpCodeArrayList = ^TSE2OpCodeArrayList;

  TSE2BaseOptimizer = class
  private
    FMethod       : TSE2Method;
  protected
    procedure AddCodeOffset(Offset, CurrentIndex: integer);
  public
    OpCodes : PSE2OpCodeArrayList;
  public
    constructor Create; overload;
    constructor Create(Method: TSE2Method); overload;
    destructor Destroy; override;

    function  IsStartup(OpCode: TSE2LinkOpCode): boolean; virtual; abstract;
    function  Optimize(Index: integer): boolean; virtual; abstract;
    property  Method       : TSE2Method read FMethod write FMethod;
  end;

  TSE2OptimizeRemoveUselessPops = class(TSE2BaseOptimizer)
  public
    function Optimize(Index: Integer): Boolean; override;
    function IsStartup(OpCode: TSE2LinkOpCode): Boolean; override;
  end;

  TSE2OptimizeInsertFastIncrement = class(TSE2BaseOptimizer)
  public
    function Optimize(Index: Integer): Boolean; override;
    function IsStartup(OpCode: TSE2LinkOpCode): Boolean; override;
  end;

  TSE2OptimizeRemoveConstIntConverts = class(TSE2BaseOptimizer)
  public
    function Optimize(Index: Integer): Boolean; override;
    function IsStartup(OpCode: TSE2LinkOpCode): Boolean; override;
  end;

  TSE2OptimizeInsertFastCompare = class(TSE2BaseOptimizer)
  public
    function Optimize(Index: Integer): Boolean; override;
    function IsStartup(OpCode: TSE2LinkOpCode): Boolean; override;
  end;

  TSE2OptimizeInsertFastOperation = class(TSE2BaseOptimizer)
  public
    function Optimize(Index: Integer): Boolean; override;
    function IsStartup(OpCode: TSE2LinkOpCode): Boolean; override;
  end;

  TSE2OptimizeInsertPushConstants = class(TSE2BaseOptimizer)
  public
    function Optimize(Index: Integer): Boolean; override;
    function IsStartup(OpCode: TSE2LinkOpCode): Boolean; override;
  end;

  TSE2OptimizeUseJumpIfZero = class(TSE2BaseOptimizer)
  public
    function Optimize(Index: Integer): Boolean; override;
    function IsStartup(OpCode: TSE2LinkOpCode): Boolean; override;
  end;

  TSE2OptimizeCombinePops = class(TSE2BaseOptimizer)
  public
    function Optimize(Index: Integer): Boolean; override;
    function IsStartup(OpCode: TSE2LinkOpCode): Boolean; override;
  end;

  TSE2OptimizeRemoveNOOPs = class(TSE2BaseOptimizer)
  public
    function Optimize(Index: Integer): Boolean; override;
    function IsStartup(OpCode: TSE2LinkOpCode): Boolean; override;
  end;

  TSE2Optimizer = class
  private
    FOptimizer : array[0..8] of TSE2BaseOptimizer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Optimize(Method: TSE2Method);
  end;

implementation

{ TSE2BaseOptimizer }

procedure TSE2BaseOptimizer.AddCodeOffset(Offset, CurrentIndex: integer);
var i: integer;
begin
  for i:=0 to Method.OpCodes.Count-1 do
  begin
    if Method.OpCodes[i].GetJumpPos > CurrentIndex then
       Method.OpCodes[i].AddOffset(Offset, CurrentIndex);
  end;
end;

constructor TSE2BaseOptimizer.Create(Method: TSE2Method);
begin
  inherited Create;
  FMethod       := Method;
end;

constructor TSE2BaseOptimizer.Create;
begin
  inherited Create;
end;

destructor TSE2BaseOptimizer.Destroy;
begin
  inherited;
end;

{ TSE2OptimizeRemoveUselessPops }

function TSE2OptimizeRemoveUselessPops.IsStartup(
  OpCode: TSE2LinkOpCode): Boolean;
begin
  result := OpCode.OpCode.OpCode = soDAT_COPY_FROM;
end;

function TSE2OptimizeRemoveUselessPops.Optimize(Index: Integer): Boolean;
var n, c: integer;
begin
  result := False;
  if OpCodes^[0].OpCode.OpCode = soDAT_COPY_FROM then
  begin
    if OpCodes^[1] <> nil then
    begin
      if OpCodes^[1].OpCode.OpCode = soSTACK_DEC then
      begin
        // Delete this and the next opcode
        Method.OpCodes.Delete(Index);
        Method.OpCodes.Delete(Index);

        // Decrease every position
        AddCodeOffset(-2, Index);
        result := True;
      end else
      if OpCodes^[1].OpCode.OpCode = soSPEC_INCP then
      begin
        c := 2;
        for n:=2 to 9 do
          if OpCodes^[n] <> nil then
          begin
            if OpCodes^[n].OpCode.OpCode = soSPEC_INCP then
               c := c + 1
            else
               break;
          end else
            break;

        if OpCodes^[c] <> nil then
          if OpCodes^[c].OpCode.OpCode = soSTACK_DEC then
          begin
            Method.OpCodes.Delete(Index);
            for n:=2 to c do
              Method.OpCodes.Delete(Index);
            Method.OpCodes.Delete(Index);


            AddCodeOffset(-c - 1, Index);
            result := True;
          end;
      end;
    end;
  end;
end;

{ TSE2OptimizeInsertFastIncrement }

function TSE2OptimizeInsertFastIncrement.IsStartup(
  OpCode: TSE2LinkOpCode): Boolean;
begin
  result := OpCode.OpCode.OpCode = soDAT_COPY_FROM;
end;

function TSE2OptimizeInsertFastIncrement.Optimize(Index: Integer): Boolean;
var bIsStatic   : boolean;
    bHasConvert : boolean;
begin
  result := False;
  if OpCodes^[0].OpCode.OpCode = soDAT_COPY_FROM then
  begin
    // Copy Variable = 0; Add New Variable = 1; Set Variable Value = 2; Add-Operation = 3; Copy Variable Back = 4;
    if (OpCodes^[1] <> nil) and (OpCodes^[2] <> nil) and (OpCodes^[3] <> nil) and (OpCodes^[4] <> nil) then
      if OpCodes^[1].OpCode.OpCode = soSTACK_INC then
        if (OpCodes^[2].OpCode.OpCode = soDAT_SetInt) and
           ((PSE2OpDAT_SetInt(OpCodes^[2].OpCode).Value and $FFFFFFFF00000000) = 0) then
        begin
          bHasConvert := False;
          if OpCodes^[3].OpCode.OpCode = soDAT_CONVERT then
          begin
            if PSE2OpDAT_CONVERT(OpCodes^[3].OpCode).NewType in [btSingle, btDouble] then
               exit;

            if OpCodes^[5] = nil then
               exit;

            OpCodes^[3] := OpCodes^[4];
            OpCodes^[4] := OpCodes^[5];
            bHasConvert := True;
          end;

          if OpCodes^[3].OpCode.OpCode = soOP_OPERATION then
            if PSE2OpOP_OPERATION(OpCodes^[3].OpCode).OpType in [2, 3] then
              if OpCodes^[4].OpCode.OpCode = soDAT_COPY_TO then
                if (PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Static = PSE2OpDAT_COPY_TO(OpCodes^[4].OpCode).Static) and
                   (
                     (PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Static and (PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Source = PSE2OpDAT_COPY_TO(OpCodes^[4].OpCode).Target)) or 
                     ((not PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Static) and (PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Source = PSE2OpDAT_COPY_TO(OpCodes^[4].OpCode).Target + 1))
                   ) then
                begin
                  bIsStatic := PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Static;
                  case PSE2OpOP_OPERATION(OpCodes^[3].OpCode).OpType of
                  2 : begin
                        if bIsStatic then
                        begin
                          OpCodes^[0].OpCode.OpCode := soINT_INCSTATIC;
                          OpCodes^[0].CodeIndex     := OpCodes^[4].CodeIndex;
                          //PSE2OpINT_INCSTATIC(OpCodes^[0].OpCode).Offset := PSE2OpDAT_COPY_TO(OpCodes^[4].OpCode).Target;
                        end else
                        begin
                          OpCodes^[0].OpCode.OpCode := soINT_INCSTACK;
                          PSE2OpINT_INCSTATIC(OpCodes^[0].OpCode).Offset := PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Source;
                        end;
                      end;
                  3 : begin
                        if bIsStatic then
                        begin
                          OpCodes^[0].OpCode.OpCode := soINT_DECSTATIC;  
                          OpCodes^[0].CodeIndex     := OpCodes^[4].CodeIndex;
                          //PSE2OpINT_INCSTATIC(OpCodes^[0].OpCode).Offset := PSE2OpDAT_COPY_TO(OpCodes^[4].OpCode).Target;
                        end else
                        begin
                          OpCodes^[0].OpCode.OpCode := soINT_DECSTACK;      
                          PSE2OpINT_INCSTATIC(OpCodes^[0].OpCode).Offset := PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Source;
                        end;
                      end;
                  end;


                  PSE2OpINT_INCSTATIC(OpCodes^[0].OpCode).Value  := PSE2OpDAT_SetInt(OpCodes^[2].OpCode).Value;

                  // Delete the obsolte opcodes
                  Method.OpCodes.Delete(index + 1);  // OpCodes^[1]
                  Method.OpCodes.Delete(index + 1);  // OpCodes^[2]
                  Method.OpCodes.Delete(index + 1);  // OpCodes^[3]
                  Method.OpCodes.Delete(index + 1);  // OpCodes^[4]
                  if bHasConvert then
                     Method.OpCodes.Delete(index + 1);

                  // Decrease every position
                  if bHasConvert then
                    AddCodeOffset(-5, index)
                  else
                    AddCodeOffset(-4, index);

                  //i := i - 1;
                  result := True;
                  exit;
                end;
        end;
  end;
end;

{ TSE2OptimizeRemoveConstIntConverts }

function TSE2OptimizeRemoveConstIntConverts.IsStartup(
  OpCode: TSE2LinkOpCode): Boolean;
begin
  result := OpCode.OpCode.OpCode = soSTACK_INC;
end;

function TSE2OptimizeRemoveConstIntConverts.Optimize(
  Index: Integer): Boolean;
begin
  result := False;
  if OpCodes^[0].OpCode.OpCode <> soSTACK_INC then
     exit;

  if PSE2OpSTACK_INC(OpCodes^[0].OpCode).AType = btS64 then
  begin
    if OpCodes^[1].OpCode.OpCode <> soDAT_SetInt then
       exit;

    if OpCodes^[2].OpCode.OpCode <> soDAT_CONVERT then
       exit;

    if PSE2OpDAT_CONVERT(OpCodes^[2].OpCode).NewType in [btU8, btS8, btU16, btS16, btU32, btS32] then
    begin
      PSE2OpSTACK_INC(OpCodes^[0].OpCode).AType := PSE2OpDAT_CONVERT(OpCodes^[2].OpCode).NewType;
      // Delete the convert opcode
      Method.OpCodes.Delete(Index + 2);

      // Decrease every position
      AddCodeOffset(-1, index);
      result := True;
    end else
    if PSE2OpDAT_CONVERT(OpCodes^[2].OpCode).NewType in [btSingle, btDouble] then
    begin
      PSE2OpSTACK_INC(OpCodes^[0].OpCode).AType    := PSE2OpDAT_CONVERT(OpCodes^[2].OpCode).NewType;
      PSE2OpDAT_SetFloat(OpCodes^[1].OpCode).Value := PSE2OpDAT_SetInt(OpCodes^[1].OpCode).Value;
      OpCodes^[1].OpCode.OpCode := soDAT_SetFloat;


      // Delete the convert opcode
      Method.OpCodes.Delete(Index + 2);

      // Decrease every position
      AddCodeOffset(-1, index);
      result := True;
    end;
  end else
  if PSE2OpSTACK_INC(OpCodes^[0].OpCode).AType = btDouble then
  begin
    if OpCodes^[1].OpCode.OpCode <> soDAT_SetFloat then
       exit;

    if OpCodes^[2].OpCode.OpCode <> soDAT_CONVERT then
       exit;

    if PSE2OpDAT_CONVERT(OpCodes^[2].OpCode).NewType in [btSingle, btDouble] then
    begin
      PSE2OpSTACK_INC(OpCodes^[0].OpCode).AType := PSE2OpDAT_CONVERT(OpCodes^[2].OpCode).NewType;
      // Delete the convert opcode
      Method.OpCodes.Delete(index + 2);

      // Decrease every position
      AddCodeOffset(-1, index);
      result := True;
    end
  end;
end;

{ TSE2OptimizeInsertFastCompare }

function TSE2OptimizeInsertFastCompare.IsStartup(
  OpCode: TSE2LinkOpCode): Boolean;
begin
  result := OpCode.OpCode.OpCode = soDAT_COPY_FROM;
end;

function TSE2OptimizeInsertFastCompare.Optimize(Index: Integer): Boolean;
var src1, src2: integer;
begin
  result := False;
  if OpCodes^[0].OpCode.OpCode <> soDAT_COPY_FROM then
     exit;
  if OpCodes^[1].OpCode.OpCode <> soDAT_COPY_FROM then
     exit;
  if OpCodes^[2].OpCode.OpCode <> soOP_COMPARE then
     exit;
  if PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Static and PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Static then
      exit;

  if not PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Static and (PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Source = 0) then
     exit;
  if not PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Static and (PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Source = 0) then
     exit;

  if PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Static then
     OpCodes^[0].CodeIndex := OpCodes^[1].CodeIndex;

  if PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Static then
     src1 := 0
  else
  begin
    src1 := PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Source;
    {
    src1 := abs(PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Source);
    if PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Source < 0 then
       src1 := src1 or $8000000;
    }
  end;

  if PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Static then
     src2 := 0
  else
  begin
    src2 := PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Source;
    {
    src2 := abs(PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Source);
    if PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Source < 0 then
       src2 := src2 or $8000000;
    }
  end;

  OpCodes^[0].OpCode.OpCode := soOP_FASTCOMPARE;
  PSE2OpOP_FASTCOMPARE(OpCodes^[0].OpCode).CompType := PSE2OpOP_COMPARE(OpCodes^[2].OpCode).CompType;
  PSE2OpOP_FASTCOMPARE(OpCodes^[0].OpCode).Src1     := src2;
  PSE2OpOP_FASTCOMPARE(OpCodes^[0].OpCode).Src2     := src1;
  {
  PSE2OpOP_FASTCOMPARE(OpCodes^[0].OpCode).iSrc1 := src2 shl 4;
  PSE2OpOP_FASTCOMPARE(OpCodes^[0].OpCode).iSrc2 := ((src2 and $F0) shl 24) or (src1 and $FFFFFFF);  //src1;
  }

  // Delete the convert opcode
  Method.OpCodes.Delete(index + 1);
  Method.OpCodes.Delete(index + 1);

  // Decrease every position
  AddCodeOffset(-2, index);
  result := True;
end;

{ TSE2OptimizeInsertFastOperation }

function TSE2OptimizeInsertFastOperation.IsStartup(
  OpCode: TSE2LinkOpCode): Boolean;
begin
  result := OpCode.OpCode.OpCode = soDAT_COPY_FROM;
end;

function TSE2OptimizeInsertFastOperation.Optimize(Index: Integer): Boolean;
var src1, src2: integer;
begin
  result := False;
  if OpCodes^[0].OpCode.OpCode <> soDAT_COPY_FROM then
     exit;
  if OpCodes^[1].OpCode.OpCode <> soDAT_COPY_FROM then
     exit;
  if OpCodes^[2].OpCode.OpCode <> soOP_OPERATION then
     exit;
  if PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Static and PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Static then
      exit;

  if not PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Static and (PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Source = 0) then
     exit;
  if not PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Static and (PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Source = 0) then
     exit;

  if PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Static then
     OpCodes^[0].CodeIndex := OpCodes^[1].CodeIndex;

  if PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Static then
     src1 := 0
  else
  begin
    src1 := PSE2OpDAT_COPY_FROM(OpCodes^[0].OpCode).Source;
  end;

  if PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Static then
     src2 := 0
  else
  begin
    src2 := PSE2OpDAT_COPY_FROM(OpCodes^[1].OpCode).Source;
  end;

  OpCodes^[0].OpCode.OpCode := soOP_FASTOPERATION;
  PSE2OpOP_FASTOPERATION(OpCodes^[0].OpCode).OpType   := PSE2OpOP_OPERATION(OpCodes^[2].OpCode).OpType;
  PSE2OpOP_FASTOPERATION(OpCodes^[0].OpCode).Src1     := src2;
  PSE2OpOP_FASTOPERATION(OpCodes^[0].OpCode).Src2     := src1;

  // Delete the convert opcode
  Method.OpCodes.Delete(index + 1);
  Method.OpCodes.Delete(index + 1);

  // Decrease every position
  AddCodeOffset(-2, index);
  result := True;
end;

{ TSE2OptimizeInsertPushConstants }

function TSE2OptimizeInsertPushConstants.IsStartup(
  OpCode: TSE2LinkOpCode): Boolean;
begin
  result := OpCode.OpCode.OpCode = soSTACK_INC;
end;

function TSE2OptimizeInsertPushConstants.Optimize(Index: Integer): Boolean;
begin
  result := False;
  if (OpCodes^[0] = nil) or (OpCodes^[1] = nil) then
     exit;

  if OpCodes^[0].OpCode.OpCode <> soSTACK_INC then
     exit;

  case OpCodes^[1].OpCode.OpCode of
  soDAT_SetInt   :
    begin
      if PSE2OpSTACK_INC(OpCodes^[0].OpCode).AType = btS64 then
      begin
        OpCodes^[0].OpCode.OpCode := soDAT_PUSHInt64;
        PSE2OpDAT_PUSHInt64(OpCodes^[0].OpCode).Value := PSE2OpDAT_SetInt(OpCodes^[1].OpCode).Value;
      end else
      if PSE2OpSTACK_INC(OpCodes^[0].OpCode).AType = btU64 then
      begin
        OpCodes^[0].OpCode.OpCode := soDAT_PUSHUInt64;
        PSE2OpDAT_PUSHUInt64(OpCodes^[0].OpCode).Value := PSE2OpDAT_SetInt(OpCodes^[1].OpCode).Value;
      end else
      begin
        OpCodes^[0].OpCode.OpCode := soDAT_PUSHInt32;
        PSE2OpDAT_PUSHInt32(OpCodes^[0].OpCode).Value := PSE2OpDAT_SetInt(OpCodes^[1].OpCode).Value;
      end;
    end;
  soDAT_SetFloat :
    begin
      if PSE2OpSTACK_INC(OpCodes^[0].OpCode).AType = btSingle then
      begin
        OpCodes^[0].OpCode.OpCode := soDAT_PUSHFloat4;
        PSE2OpDAT_PUSHFloat4(OpCodes^[0].OpCode).Value := PSE2OpDAT_SetFloat(OpCodes^[1].OpCode).Value;
      end else
      begin
        OpCodes^[0].OpCode.OpCode := soDAT_PUSHFloat8;
        PSE2OpDAT_PUSHFloat8(OpCodes^[0].OpCode).Value := PSE2OpDAT_SetFloat(OpCodes^[1].OpCode).Value;
      end;
    end;
  soDAT_SetPtr   :
    begin
      if PSE2OpSTACK_INC(OpCodes^[0].OpCode).AType = btPointer then
      begin
        OpCodes^[0].OpCode.OpCode := soDAT_PUSHPtr;
        PSE2OpDAT_PUSHPtr(OpCodes^[0].OpCode).Value := PSE2OpDAT_SetPtr(OpCodes^[1].OpCode).Value;
      end else
        exit;
    end;
  soDAT_LOADRES :
    begin
      if PSE2OpSTACK_INC(OpCodes^[0].OpCode).AType in [btString, btPChar, btAnsiString, btPAnsiChar, btWideString, btPWideChar, btUTF8String] then
      begin
        OpCodes^[0].OpCode.OpCode := soDAT_PUSHRES;
        OpCodes^[0].CodeIndex := OpCodes^[1].CodeIndex;
        PSE2OpDAT_PUSHRES(OpCodes^[0].OpCode).Index := PSE2OpDAT_LoadRes(OpCodes^[1].OpCode).Index;
      end else
        exit;
    end;
  else exit;
  end;

  Method.OpCodes.Delete(index + 1);
  AddCodeOffset(-1, index);
  result := True;
end;

{ TSE2OptimizeUseJumpIfZero }

function TSE2OptimizeUseJumpIfZero.IsStartup(
  OpCode: TSE2LinkOpCode): Boolean;
begin
  result := OpCode.OpCode.OpCode in
      [soDAT_PUSHInt32, soDAT_PUSHInt64, soDAT_PUSHUInt64, soDAT_PUSHFloat4,
       soDAT_PUSHFloat8, soDAT_PUSHPtr];
end;

function TSE2OptimizeUseJumpIfZero.Optimize(Index: Integer): Boolean;
begin
  result := False;
  if (OpCodes^[0] = nil) or (OpCodes^[1] = nil) or (OpCodes^[2] = nil) then
     exit;

  if not ( OpCodes^[0].OpCode.OpCode in
      [soDAT_PUSHInt32, soDAT_PUSHInt64, soDAT_PUSHUInt64, soDAT_PUSHFloat4,
       soDAT_PUSHFloat8, soDAT_PUSHPtr]) then
     exit;

  if OpCodes^[1].OpCode.OpCode <> soOP_COMPARE then
     exit;

  if PSE2OpOP_COMPARE(OpCodes^[1].OpCode).CompType <> 1 then
     exit;
       
  if OpCodes^[2].OpCode.OpCode <> soFLOW_JIZ then
     exit;

  case OpCodes^[0].OpCode.OpCode of
  soDAT_PUSHInt32  : if PSE2OpDAT_PUSHInt32(OpCodes^[0].OpCode).Value <> 0 then exit;
  soDAT_PUSHInt64  : if PSE2OpDAT_PUSHInt64(OpCodes^[0].OpCode).Value <> 0 then exit;
  soDAT_PUSHUInt64 : if PSE2OpDAT_PUSHUInt64(OpCodes^[0].OpCode).Value <> 0 then exit;
  soDAT_PUSHFLOAT4 : if PSE2OpDAT_PUSHFloat4(OpCodes^[0].OpCode).Value <> 0 then exit;
  soDAT_PUSHFLOAT8 : if PSE2OpDAT_PUSHFloat8(OpCodes^[0].OpCode).Value <> 0 then exit;
  soDAT_PUSHPtr    : if PSE2OpDAT_PUSHPtr(OpCodes^[0].OpCode).Value <> nil then exit;
  end;

  PSE2OpFLOW_JNZ(OpCodes^[0].OpCode).OpCode := soFLOW_JNZ;
  PSE2OpFLOW_JNZ(OpCodes^[0].OpCode).Position := PSE2OpFLOW_JIZ(OpCodes^[2].OpCode).Position;
  OpCodes^[0].CodeIndex := OpCodes^[2].CodeIndex;

  Method.OpCodes.Delete(index + 1);
  Method.OpCodes.Delete(index + 1);
  AddCodeOffset(-2, index);
  result := True;
end;

{ TSE2OptimizeCombinePops }

function TSE2OptimizeCombinePops.IsStartup(
  OpCode: TSE2LinkOpCode): Boolean;
begin
  result := OpCode.OpCode.OpCode = soSTACK_INC;
end;

function TSE2OptimizeCombinePops.Optimize(Index: Integer): Boolean;
var i, popCount : integer;

  function AnybodyJumpingToPosition(TargetPos: integer): boolean;
  var i: integer;
  begin
    result := True;
    for i:=0 to Method.OpCodes.Count-1 do
      if Method.OpCodes[i].GetJumpPos = TargetPos then
         exit;
    result := False;
  end;

begin
  result := False;
  if OpCodes^[0] = nil then
     exit
  else
  if OpCodes^[0].OpCode^.OpCode <> soSTACK_DEC then
     exit;

  popCount := 0;
  for i:=0 to 10 do
  begin
      if AnybodyJumpingToPosition(i + index) then
      begin
        break;
      end;

    if OpCodes^[i] = nil then
       break
    else
    if OpCodes^[i].OpCode.OpCode = soSTACK_DEC then
       popCount := popCount + 1
    else
       break;
  end;

  if popCount < 2 then
     exit;

  OpCodes^[0].OpCode.OpCode := soSTACK_DEC_COUNT;
  PSE2OpSTACK_DEC_COUNT(OpCodes^[0].OpCode).Count := popCount;

  for i:=2 to popCount do
    Method.OpCodes.Delete(index + 1);

  AddCodeOffset(-(popCount - 1), index);
  result := True;
end;

{ TSE2OptimizeRemoveNOOPs }

function TSE2OptimizeRemoveNOOPs.IsStartup(
  OpCode: TSE2LinkOpCode): Boolean;
begin
  result := OpCode.OpCode.OpCode = soNOOP;
end;

function TSE2OptimizeRemoveNOOPs.Optimize(Index: Integer): Boolean;
begin
  result := False;
  if OpCodes^[0].OpCode.OpCode = soNOOP then
  begin
    // Delete this opcode
    Method.OpCodes.Delete(index);

    // Decrease every position
    AddCodeOffset(-1, index);
    result := True;
  end;
end;

{ TSE2Optimizer }

constructor TSE2Optimizer.Create;
begin
  inherited;

  FOptimizer[0] := TSE2OptimizeRemoveUselessPops.Create();
  FOptimizer[1] := TSE2OptimizeInsertFastIncrement.Create();
  FOptimizer[2] := TSE2OptimizeInsertFastCompare.Create();
  FOptimizer[3] := TSE2OptimizeInsertFastOperation.Create();
  FOptimizer[4] := TSE2OptimizeRemoveConstIntConverts.Create();
  FOptimizer[5] := TSE2OptimizeInsertPushConstants.Create();
  FOptimizer[6] := TSE2OptimizeUseJumpIfZero.Create();
  FOptimizer[7] := TSE2OptimizeCombinePops.Create();
  FOptimizer[8] := TSE2OptimizeRemoveNOOPs.Create();
end;

destructor TSE2Optimizer.Destroy;
var Mode: integer;
begin
  for Mode := High(FOptimizer) downto Low(FOptimizer) do
    FOptimizer[Mode].Free;
  inherited;
end;

procedure TSE2Optimizer.Optimize(Method: TSE2Method);
var i, j      : integer;
    OpCodes   : TSE2OpCodeArrayList;
    Mode      : integer;
begin
  for Mode := Low(FOptimizer) to High(FOptimizer) do
  begin
    FOptimizer[Mode].OpCodes := @OpCodes;
    FOptimizer[Mode].Method  := Method;
  end;

  i := 0;
  repeat
    for j:=0 to 10 do
      OpCodes[j] := Method.OpCodes[i + j];

    for Mode := Low(FOptimizer) to High(FOptimizer) do
      if FOptimizer[Mode].IsStartup(OpCodes[0]) then
        if FOptimizer[Mode].Optimize(i) then
        begin
          i := i - 1;
          break;
        end;

    inc(i);
  until i >= Method.OpCodes.Count;
end;

end.
