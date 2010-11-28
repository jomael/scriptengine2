unit uSE2ExecutionContext;

{$INCLUDE ScriptEngine.inc}

interface

{$IFDEF SEII_FPC}
  {$HINTS OFF}
  {$WARNINGS OFF}
{$ENDIF}

uses
  Classes, SysUtils, uSE2RunType, uSE2Consts, uSE2BaseTypes, uSE2OpCode, uSE2PEData, uSE2RunCall,
  uSE2RunOperation, uSE2RunAccess, uSE2SafeBlockMngr, uSE2DebugData, uSE2MemoryManager, uSE2PerfMonitor,
  uSE2NativeCallList, uSE2RunTimeClasses;

type
  TSE2RunTimeError = procedure(Sender: TObject; Exp: ExceptClass; const Msg: string; CodePos: integer; const CallStack: string) of object;

  TSE2ExecutionData = class
  private
    FAppCode      : TSE2PE;
    FCodeAccess   : TSE2RunAccess;
    FMemoryMngr   : TSE2MemoryManager;
    FStaticMemory : TSE2Stack;
    FStaticHelper : TSE2VarHelper;    
    FNativeList   : TSE2NativeCallList;
    FPackedData   : TSE2RunTimeClasses;
    FRunTime      : Pointer;
    FExtMethPtrs  : boolean;
    FPInitialized : PBoolean;
  public
    property ExtMethPtrs  : boolean            read FExtMethPtrs  write FExtMethPtrs;
    property AppCode      : TSE2PE             read FAppCode      write FAppCode;
    property CodeAccess   : TSE2RunAccess      read FCodeAccess   write FCodeAccess;
    property MemoryMngr   : TSE2MemoryManager  read FMemoryMngr   write FMemoryMngr;
    property StaticMemory : TSE2Stack          read FStaticMemory write FStaticMemory;
    property StaticHelper : TSE2VarHelper      read FStaticHelper write FStaticHelper;
    property NativeList   : TSE2NativeCallList read FNativeList   write FNativeList;
    property PackedData   : TSE2RunTimeClasses read FPackedData   write FPackedData;
    property RunTime      : Pointer            read FRunTime      write FRunTime;
    property PInitialized : PBoolean           read FPInitialized write FPInitialized;
  end;

  TSE2ExecutionContext = class(TSE2Object)
  private
    { Global Memory Data }
    FExecutionData     : TSE2ExecutionData;
    FOpCodes           : TSE2OpCodes;

    { Dynamic Data }
    FStack             : TSE2Stack;
    FMemoryManager     : TSE2MemoryManager;
    FStackHelper       : TSE2VarHelper;
    FDataOperation     : TSE2VarOperation;
    FDataComparison    : TSE2VarCompare;

    { Packed Data Management }
    FPackedData        : TSE2RunTimeClasses;

    { Garbage Collector }
    FClassGC           : TSE2ClassGC;
    FRecordGC          : TSE2RecordGC;

    { Runtime Data }
    FCodePos           : integer;
    FDoAbort           : boolean;

    { Exceptions }           
    FSafeBlocks        : TSE2SafeBlockMngr;
    FHasException      : boolean;   
    FOnError           : TSE2RunTimeError;

    { Calling }
    FMethodCall        : TSE2MethodCall;
    FOnBeforeOperation : TNotifyEvent;
  protected
    procedure BuildScriptExceptions(TryBlock: TSE2TryBlock; ex: Exception);
    procedure DoErrorEvent(Exp: ExceptClass; const Msg: string; ErrorPos: integer; const CallStack: string);

    function  MethodDescToStr(Entry: TSE2MetaEntry): string;
    function  MetaEntryToStr(Entry: TSE2MetaEntry; StackIndex: integer): string;
    procedure MetaEntryToItem(Entry: TSE2MetaEntry; StackIndex: integer; Target: TSE2StackItem);

    procedure RemoveUnusedRecords(numRecords: integer);

    function  ExceptionCallStack: string;
    procedure Process;

    procedure SetExecutionData(value: TSE2ExecutionData);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Run(CodePos: integer);
    function  Call(Method: Pointer; const Params: array of const): variant;
    procedure Abort;
    function  GetCallStack(limit: integer = -1): string; overload;
    procedure GetCallStack(Target: TSE2StackTrace); overload;

    function  ScriptAsMethod(const Method, ClassInstance: Pointer): TMethod;
    function  MethodAsScript(Data: Pointer; MethodPos, ClassPtr: Pointer): boolean;
                         
    procedure ProcessGC;
    procedure ProcessRecordGC;

    procedure Initialize;

    property ClassGCList       : TSE2ClassGC       read FClassGC;
    property PackedData        : TSE2RunTimeClasses read FPackedData;
    property Stack             : TSE2Stack         read FStack;
    property ExecutionData     : TSE2ExecutionData read FExecutionData write SetExecutionData;
    property DoAbort           : boolean           read FDoAbort       write FDoAbort;
    property CodePos           : integer           read FCodePos       write FCodePos;

    property OnError           : TSE2RunTimeError  read FOnError              write FOnError;
    property OnBeforeOperation : TNotifyEvent      read FOnBeforeOperation    write FOnBeforeOperation;
  end;

implementation

uses uSE2RunTime, uSE2UnitManager, uSE2SystemUnit, uSE2NativeScriptCall;

{ TSE2RunTime }

constructor TSE2ExecutionContext.Create;
begin
  inherited;
  FMemoryManager  := TSE2MemoryManager.Create;
  FPackedData     := TSE2RunTimeClasses.Create(FMemoryManager);
  FStackHelper    := TSE2VarHelper.Create(FMemoryManager, nil);
  FDataOperation  := TSE2VarOperation.Create(FStackHelper);
  FDataComparison := TSE2VarCompare.Create(FStackHelper);
  FClassGC        := TSE2ClassGC.Create;
  FRecordGC       := TSE2RecordGC.Create;

  FStack          := TSE2Stack.Create(FStackHelper);
  FSafeBlocks     := TSE2SafeBlockMngr.Create;

  FMethodCall     := TSE2MethodCall.Create(Self);
end;

destructor TSE2ExecutionContext.Destroy;
begin
  FSafeBlocks.Free;
  FStack.Free;
  FClassGC.Free;
  FRecordGC.Free;
  FMethodCall.Free;

  FStackHelper.Free;
  FDataOperation.Free;
  FDataComparison.Free;
  FMemoryManager.Free;
  FPackedData.Free;

  inherited;
end;

procedure TSE2ExecutionContext.ProcessRecordGC;
var i: integer;
begin
  for i:=FRecordGC.Count-1 downto 0 do
    FPackedData.DestroyScriptRecord(PSE2RecordGCEntry(FRecordGC[i])^.Ptr);
  FRecordGC.Clear;
end;

procedure TSE2ExecutionContext.ProcessGC;
var i: integer;
begin
  for i:=FClassGC.Count-1 downto 0 do
    FPackedData.DestroyScriptClassObject(FClassGC[i]);
  FClassGC.Clear;
end;

procedure TSE2ExecutionContext.Run(CodePos: integer);
var oldPos: integer;
begin
  oldPos   := FCodePos;

  FCodePos := CodePos;
  FDoAbort := False;
  FOpCodes := FExecutionData.AppCode.OpCodes;
  Process;

  FCodePos := oldPos;
end;

procedure TSE2ExecutionContext.RemoveUnusedRecords(numRecords: integer);
var i: integer;
    p: PSE2RecordGCEntry;
    m: integer;
begin
  m := FRecordGC.Count-1-numRecords;
  if m < 0 then
     m := 0;
  for i:=FRecordGC.Count-1 downto m do
  begin
    p := FRecordGC.Items[i];
    if p^.StackSize >= FStack.Size then
    begin
      FPackedData.DestroyScriptRecord(p^.Ptr);
      FRecordGC.Delete(i);
    end;
  end;
end;

procedure TSE2ExecutionContext.Process;

type
  PPosArray = ^TPosArray;
  TPosArray = array[0..1] of integer;
  PPtrArray = ^TPtrArray;
  TPtrArray = array[0..1] of pointer;

var OpCode        : PSE2OpDefault;
    CompareInt    : integer;
    Pos           : TPosArray;
    Ptr           : TPtrArray;
    r1, r2, r3    : PSE2VarData;
    //VarDat        : array[0..3] of PSE2VarData;
    Meta          : TSE2MetaEntry;
    TryBlock      : TSE2TryBlock;
    TempTryBlock  : TSE2TryBlock;
begin
  FHasException := False;
  while (FCodePos > 0) and (not FDoAbort) do
  begin
    try
  OpCode := FOpCodes.List[FCodePos];
  if OpCode = nil then
     exit;

  if Assigned(FOnBeforeOperation) then
     FOnBeforeOperation(TObject(FExecutionData.RunTime));

  {$IFDEF PERF_MONITOR}
  FPerfMonitor.Start;
  {$ENDIF}

  case OpCode.OpCode of
  soSTACK_INC  :
      begin
        FStack.PushNew(PSE2OpSTACK_INC(OpCode).AType);
      end;
  soMEM_MAKE :
      begin
        FExecutionData.StaticMemory.PushNew(PSE2OpMEM_MAKE(OpCode).AType);
      end;
  soSTACK_INC_COUNT :
      begin
        for CompareInt := PSE2OpSTACK_INC_COUNT(OpCode).Count-1 downto 0 do
          FStack.PushNew(PSE2OpSTACK_INC_COUNT(OpCode).AType);
      end;
  soSTACK_DEC :
      begin
        FStack.Pop;
      end;
  soSTACK_DEC_COUNT :
      begin
        for CompareInt := PSE2OpSTACK_DEC_COUNT(OpCode).Count-1 downto 0 do
          FStack.Pop;
      end;
  soSTACK_DEC_NODEL :
      begin
        // Stack does reference counting automatically
        FStack.Pop;        
      end;
  soFLOW_GOTO :
      begin
        FCodePos := PSE2OpFLOW_GOTO(OpCode).Position - 1;
      end;
  soFLOW_JIZ :
      begin
        CompareInt := 0;
        r1 := FStack.Top;

        case r1.AType of
        btU8, btS8, btU16, btS16, btU32, btS32 :
            if FStackHelper.VarHasValue(r1, @CompareInt) then
               FCodePos := PSE2OpFLOW_JIZ(OpCode).Position - 1;
        btS64, btU64 :
            if r1^.ts64^ = 0 then
               FCodePos := PSE2OpFLOW_JIZ(OpCode).Position - 1;
        btObject, btPointer, btProcPtr :
            if PPointer(r1^.tPointer)^ = nil then
               FCodePos := PSE2OpFLOW_JIZ(OpCode).Position - 1;
        btSingle :
            if r1^.tSingle^ = 0 then
               FCodePos := PSE2OpFLOW_JIZ(OpCode).Position - 1;
        btDouble :
            if r1^.tDouble^ = 0 then
               FCodePos := PSE2OpFLOW_JIZ(OpCode).Position - 1;
        end;
        FStack.Pop;
      end;
  soFLOW_JNZ :
      begin
        CompareInt := 0;
        r1 := FStack.Top;

        case r1.AType of
        btU8, btS8, btU16, btS16, btU32, btS32 :
            if not FStackHelper.VarHasValue(r1, @CompareInt) then
               FCodePos := PSE2OpFLOW_JNZ(OpCode).Position - 1;
        btS64, btU64 :
            if r1^.ts64^ <> 0 then
               FCodePos := PSE2OpFLOW_JNZ(OpCode).Position - 1;
        btObject, btPointer, btProcPtr :
            if PPointer(r1^.tPointer)^ <> nil then
               FCodePos := PSE2OpFLOW_JNZ(OpCode).Position - 1;
        btSingle :
            if r1^.tSingle^ <> 0 then
               FCodePos := PSE2OpFLOW_JNZ(OpCode).Position - 1;
        btDouble :
            if r1^.tDouble^ <> 0 then
               FCodePos := PSE2OpFLOW_JNZ(OpCode).Position - 1;
        end;
        FStack.Pop;
      end;
  soFLOW_CALL :
      begin
        FCodePos := PSE2OpFLOW_CALL(OpCode).Position - 1;
      end;
  soDAT_LOADEX  :
      begin
        if PSE2OpFLOW_CALLEX(FOpCodes.List[FCodePos + 1]).Position = 0 then
        begin
          PSE2OpFLOW_CALLEX(FOpCodes.List[FCodePos + 1]).Position :=
            cardinal(TSE2RunTime( FExecutionData.RunTime ).DoLoadMethod(
              FExecutionData.AppCode.MetaData[PSE2OpFLOW_CALLEX(FOpCodes.List[FCodePos + 1]).MetaIndex]
            ));
          //TSE2RunTime( FExecutionData.RunTime )
        end;
      end;
  soFLOW_CALLEX :
      begin
        FMethodCall.Clear;
        FMethodCall.Run(Pointer(PSE2OpFLOW_CALLEX(OpCode).Position), FExecutionData.AppCode.MetaData[PSE2OpFLOW_CALLEX(OpCode).MetaIndex]);
      end;
  soFLOW_CALLPEX :
      begin
        FMethodCall.Clear;
        r1 := FStack.Top;
        r2 := FStack.List[FStack.Size - 2];
        if r1^.AType <> btS32 then
           raise ESE2RunTimeError.Create('Internal error: stack not valid');
        if r2^.AType <> btProcPtr then
           raise ESE2RunTimeError.Create('Internal error: stack not valid');

        r2 := Pointer(r2^.tPointer^);
        CompareInt := r1.tS32^;

        if CompareInt < 0 then 
           raise ESE2RunTimeError.Create('Internal error: external method description not assigned');

        Stack.Pop;
        Stack.Pop;

        if Pointer(r2) = nil then
           raise ESE2UnassignedMethod.Create('Method pointer not assigned');

        FMethodCall.Run( Pointer(r2), FExecutionData.AppCode.MetaData[CompareInt] );
      end;
  soFLOW_CALLDYN :
      begin
        r1 { VarDat[0] } := FStack.Top;//Items[FStack.Size - 2];

        if not (r1 { VarDat[0] }.AType in [btObject, btPointer]) then
           raise ESE2RunTimeError.Create('Internal error: dynamic method call outside of a class');

        if Pointer(r1 { VarDat[0] }.tPointer^) = nil then
           raise ESE2NullReferenceError.Create('Called class is not assigned');

        CompareInt := FPackedData.GetClassMethods(PPointer(r1 { VarDat[0] }^.tPointer)^).Items[PSE2OpFLOW_CALLDYN(OpCode)^.Offset];

        if CompareInt < 1 then
           raise ESE2RunTimeError.Create('Abstract method was not implemented');

        FCodePos := CompareInt - 1;
        FStack.Pop;
      end;
  soFLOW_CALLPTR :
      begin
        r1 { VarDat[0] } := FStack.Top;
        if not (r1 { VarDat[0] }.AType in [btPointer, btProcPtr]) then
           raise ESE2RunTimeError.Create('Internal error: stack corrupt');

        if Pointer(r1 { VarDat[0] }.tPointer^) = nil then
           raise ESE2NullReferenceError.Create('Method call not assigned');

        Meta := PPointer(r1 { VarDat[0] }.tPointer)^;
        FCodePos := Meta.CodePos - 1;


        r2 := FStack.Items[FStack.Size - 2];
        if r2.AType = btReturnAddress then
        begin
          Pos    := PPosArray(r2.ts64)^;
          Pos[1] := Meta.Index + 1;
          r2.ts64^ := PInt64(@(Pos[0]))^;
        end;


        if Meta.HasSelf then
        begin
          r3 { VarDat[2] } := FStack.Items[FStack.Size - Meta.ParamCount - 2];

          r2 { VarDat[1] } := FStackHelper.CreateVarData(btObject);
          PPointer(r2 { VarDat[1] }^.tPointer)^ := PPointer(Integer(r3 { VarDat[2] }^.tPointer) + SizeOf(Pointer))^;

          FStack.ITems[FStack.Size - Meta.ParamCount - 2] := r2 { VarDat[1] };
          FStack.Pool.Push(r3 { VarDat[2] });

          if Meta.DynIndex > -1 then
          begin
            FCodePos := FPackedData.GetClassMethods(PPointer(r2 { VarDat[1] }^.tPointer)^).Items[Meta.DynIndex];
            if FCodePos < 1 then
               raise ESE2RunTimeError.Create('Abstract method was not implemented');
            FCodePos := FCodePos - 1;
          end;
        end;
        FStack.Pop;
      end;
  soFLOW_RET :
      begin
        if FStack.Top.AType <> btReturnAddress then
           raise ESE2RunTimeError.Create('['+IntToStr(FCodePos)+'] Internal error: return stack value not valid');

        FCodePos := ((FStack.Top^.ts64^) and $FFFFFFFF) - 1;
        FStack.Pop;

        //if FRecordGC.Count > 0 then
        //   ProcessRecordGC;
      end;
  soFINIT_STACK :
      begin
        CompareInt := PSE2OpFINIT_STACK(OpCode).StackSize;
        while FStack.Size > CompareInt do
          FStack.Pop;
      end;
  soDEBUG_META :
      begin
        r1 := FStack.Top;
        if r1.AType = btReturnAddress then
        begin
          Pos := PPosArray(r1.ts64)^;
          Pos[1] := PSE2OpDEBUG_META(OpCode).MetaIndex;
          r1.ts64^ := PInt64(@(Pos[0]))^;
        end;
      end;
  soMETA_CNAME   :
      begin
        r1 := FStack.Top;
        r2 := FStack[FStack.Size - 3];

        if not (r2.AType in [btObject, btPointer]) then
           raise ESE2RunTimeError.Create('Stack item is not a class');

        if PPointer(r2.tPointer)^ = nil then
           raise ESE2NullReferenceError.Create('Class is not assigned');


        Meta := FPackedData.GetClassMeta(PPointer(r2.tPointer)^);
        PString(r1.tString^)^ := Meta.AUnitName + '.' + Meta.Name;
      end;
  soMETA_PUSH    :
      begin
        FStack.PushNew(btS32).ts32^ := PSE2OpMETA_PUSH(OpCode).MetaIndex;
      end;
  soMETA_SHARE   :
      begin
        r1 := FStack.Top;

        if (not (r1.AType in [btPointer, btObject])) then
           raise ESE2RunTimeError.Create('['+IntToStr(FCodePos)+']: is-comparison not possible');

        if PPointer(r1.tPointer)^ = nil then
           CompareInt := 0
        else
        begin
          CompareInt := Ord(FExecutionData.CodeAccess.IsChildOfClass(
                             FExecutionData.AppCode.MetaData.Items[PSE2OpMETA_SHARE(OpCode).MetaIndex],
                             FPackedData.GetClassMeta(PPointer(r1.tPointer)^),
                             True)
                           );
        end;

        FStack.Pop;
        FStack.PushNew(btBoolean).tu8^ := CompareInt;
      end;
  soMETA_CAST :
      begin
        r1 := FStack.Top;

        if (not (r1.AType in [btPointer, btObject])) then
           raise ESE2RunTimeError.Create('['+IntToStr(FCodePos)+']: as-cast not possible');

        if PPointer(r1.tPointer)^ <> nil then
        begin
          if not (FExecutionData.CodeAccess.IsChildOfClass(
                             FExecutionData.AppCode.MetaData.Items[PSE2OpMETA_SHARE(OpCode).MetaIndex],
                             FPackedData.GetClassMeta(PPointer(r1.tPointer)^),
                             True)
                           ) then
             raise EInvalidCast.Create('Class not compatible');
        end;
      end;
  soFLOW_PUSHRET :
      begin
        Pos[1] := PSE2OpFLOW_PUSHRET(OpCode).DebugData;
        Pos[0] := PSE2OpFLOW_PUSHRET(OpCode).Position;

        FStack.PushNew(btReturnAddress)^.ts64^ := PInt64(@(Pos[0]))^;
      end;
  soOP_OPERATION :
      begin
        case PSE2OpOP_OPERATION(OpCode).OpType of
        1, 20,
        21      :
            begin
              r1 { VarDat[0] } := FStack.Top;

              if r1 { VarDat[0] }^.RefContent then
              begin
                r3 { VarDat[2] } := FStackHelper.CreateVarData(r1 { VarDat[0] }^.AType);
                FStackHelper.SetVarData(r1 { VarDat[0] }, r3 { VarDat[2] });
                FStackHelper.FreeVarData(r1 { VarDat[0] });

                FStack[FStack.Size - 1] := r3 { VarDat[2] };
              end;

              case PSE2OpOP_OPERATION(OpCode).OpType of
              1  : FDataOperation.Negation(FStack.Top); // negation
              20 : FDataOperation.BitNot(FStack.Top);
              21 : FDataOperation.BooleanNot(FStack.Top);
              end;
            end;
        2..11   :
            begin
              r2 { VarDat[1] } := FStack.Top;
              r1 { VarDat[0] } := FStack.List[FStack.Size - 2]; //r1 { VarDat[0] } := Stack[Stack.Size - 2];

              if r1 { VarDat[0] }^.RefContent then
              begin
                r3 { VarDat[2] } := FStackHelper.CreateVarData(r1 { VarDat[0] }^.AType);
                FStackHelper.SetVarData(r1 { VarDat[0] }, r3 { VarDat[2] });
                FStackHelper.FreeVarData(r1 { VarDat[0] });
                r1 { VarDat[0] } := r3 { VarDat[2] };

                FStack[FStack.Size - 2] := r3 { VarDat[2] };
              end;
              
              case PSE2OpOP_OPERATION(OpCode).OpType of
              2  : FDataOperation.Addition(r1 { VarDat[0] }, r2 { VarDat[1] });
              3  : FDataOperation.Substract(r1 { VarDat[0] }, r2 { VarDat[1] });
              4  : FDataOperation.Multiply(r1 { VarDat[0] }, r2 { VarDat[1] });
              5  : FDataOperation.Divide(r1 { VarDat[0] }, r2 { VarDat[1] });
              6  : FDataOperation.BitAnd(r1 { VarDat[0] }, r2 { VarDat[1] });
              7  : FDataOperation.BitOr(r1 { VarDat[0] }, r2 { VarDat[1] });
              8  : FDataOperation.BitXor(r1 { VarDat[0] }, r2 { VarDat[1] });
              9  : FDataOperation.DivideMod(r1 { VarDat[0] }, r2 { VarDat[1] });
              10 : FDataOperation.BitShr(r1 { VarDat[0] }, r2 { VarDat[1] });
              11 : FDataOperation.BitShl(r1 { VarDat[0] }, r2 { VarDat[1] });
              end;


              // Aritmetic
              FStack.Pop;
            end;
        end;
      end;
  soOP_FASTOPERATION :
      begin
        case PSE2OpOP_FASTOPERATION(OpCode).OpType of
        1, 20,
        21      :
            begin
              { 1st Parameter }
              CompareInt := PSE2OpOP_FASTOPERATION(OpCode).Src1;
              if CompareInt >= 0 then // Static
              begin
                r1 := FExecutionData.StaticMemory.List[CompareInt];
                r2 := FStack.PushNew(r1.AType);
              end else
              begin
                r1 := FStack.List[FStack.Size-1 + CompareInt];
                r2 := FStack.PushNew(r1.AType);
              end;
              FStackHelper.SetVarData(r1, r2);

              case PSE2OpOP_OPERATION(OpCode).OpType of
              1  : FDataOperation.Negation(FStack.Top); // negation
              20 : FDataOperation.BitNot(FStack.Top);
              21 : FDataOperation.BooleanNot(FStack.Top);
              end;
            end;
        2..11   :
            begin
              { 1st Parameter }
              CompareInt := PSE2OpOP_FASTOPERATION(OpCode).Src1;
              if CompareInt >= 0 then  // Static
                r2 := FExecutionData.StaticMemory[CompareInt]
              else
                r2 := FStack[FStack.Size + CompareInt];

              { 2nd Parameter }
              CompareInt := PSE2OpOP_FASTOPERATION(OpCode).Src2;
              if CompareInt >= 0 then // Static
                r3 := FExecutionData.StaticMemory[CompareInt]
              else
                r3 := FStack[FStack.Size - 1 + CompareInt];

              r1 := FStack.PushNew(r3.AType); 
              FStackHelper.SetVarData(r3, r1);
                        

              case PSE2OpOP_OPERATION(OpCode).OpType of
              2  : FDataOperation.Addition(r1 { VarDat[0] }, r2 { VarDat[1] });
              3  : FDataOperation.Substract(r1 { VarDat[0] }, r2 { VarDat[1] });
              4  : FDataOperation.Multiply(r1 { VarDat[0] }, r2 { VarDat[1] });
              5  : FDataOperation.Divide(r1 { VarDat[0] }, r2 { VarDat[1] });
              6  : FDataOperation.BitAnd(r1 { VarDat[0] }, r2 { VarDat[1] });
              7  : FDataOperation.BitOr(r1 { VarDat[0] }, r2 { VarDat[1] });
              8  : FDataOperation.BitXor(r1 { VarDat[0] }, r2 { VarDat[1] });
              9  : FDataOperation.DivideMod(r1 { VarDat[0] }, r2 { VarDat[1] });
                   //mTable_mod[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              10 : FDataOperation.BitShr(r1 { VarDat[0] }, r2 { VarDat[1] });
              11 : FDataOperation.BitShl(r1 { VarDat[0] }, r2 { VarDat[1] });
              end;
            end;
        end;
      end;
  soOP_COMPARE :
      begin
        //QueryPerformanceFrequency(c);
        //QueryPerformanceCounter(t1);
        r2 { VarDat[1] } := FStack.Top;
        r1 { VarDat[0] } := FStack.List[FStack.Size - 2]; //r1 { VarDat[0] } := FStack[Stack.Size - 2];

        case PSE2OpOP_COMPARE(OpCode).CompType of
        1 : CompareInt := Ord(FDataComparison.Equal(r1 { VarDat[0] }, r2 { VarDat[1] }));
        2 : CompareInt := Ord(FDataComparison.Smaller(r1 { VarDat[0] }, r2 { VarDat[1] }));
        3 : CompareInt := Ord(not FDataComparison.SmallerEqual(r1 { VarDat[0] }, r2 { VarDat[1] }));
        4 : CompareInt := Ord(not FDataComparison.Smaller(r1 { VarDat[0] }, r2 { VarDat[1] }));
        5 : CompareInt := Ord(FDataComparison.SmallerEqual(r1 { VarDat[0] }, r2 { VarDat[1] }));
        6 : CompareInt := Ord(not FDataComparison.Equal(r1 { VarDat[0] }, r2 { VarDat[1] }));
        end;


        FStack.Pop;
        FStack.Pop;
        FStack.PushNew(btBoolean).tu8^ := CompareInt;
        //QueryPerformanceCounter(t2);
        //FRunTime := FRunTime + ( t2 - t1) / c;
      end;
  soOP_FASTCOMPARE :
      begin
        { 1st Parameter }
        CompareInt := PSE2OpOP_FASTCOMPARE(OpCode).Src1;
        if CompareInt >= 0 then  // Static
          r2 := FExecutionData.StaticMemory[CompareInt]
        else
          r2 := FStack[FStack.Size + CompareInt];

        { 2nd Parameter }
        CompareInt := PSE2OpOP_FASTCOMPARE(OpCode).Src2;
        if CompareInt >= 0 then // Static
          r1 := FExecutionData.StaticMemory[CompareInt]
        else
          r1 := FStack[FStack.Size - 1 + CompareInt];

        case PSE2OpOP_COMPARE(OpCode).CompType of
        1 : CompareInt := Ord(FDataComparison.Equal(r1 { VarDat[0] }, r2 { VarDat[1] }));
        2 : CompareInt := Ord(FDataComparison.Smaller(r1 { VarDat[0] }, r2 { VarDat[1] }));
        3 : CompareInt := Ord(not FDataComparison.SmallerEqual(r1 { VarDat[0] }, r2 { VarDat[1] }));
        4 : CompareInt := Ord(not FDataComparison.Smaller(r1 { VarDat[0] }, r2 { VarDat[1] }));
        5 : CompareInt := Ord(FDataComparison.SmallerEqual(r1 { VarDat[0] }, r2 { VarDat[1] }));
        6 : CompareInt := Ord(not FDataComparison.Equal(r1 { VarDat[0] }, r2 { VarDat[1] }));
        end;

        FStack.PushNew(btBoolean).tu8^ := CompareInt;
      end;
  soDAT_COPY_TO :
      begin
        CompareInt := PSE2OpDAT_COPY_TO(OpCode).Target;

        if PSE2OpDAT_COPY_TO(OpCode).Static then
           r1 { VarDat[0] } := FExecutionData.StaticMemory.List[CompareInt]
        else
           r1 { VarDat[0] } := FStack.List[FStack.Size-1 + CompareInt];

        if r1 { VarDat[0] }^.RefContent then
        begin
          r2 { VarDat[1] } := FStackHelper.CreateVarData(r1 { VarDat[0] }^.AType);
          FStackHelper.SetVarData(r1 { VarDat[0] }, r2 { VarDat[1] });

          FStackHelper.FreeVarData(r1 { VarDat[0] });
          r1 { VarDat[0] } := r2 { VarDat[1] };
          if PSE2OpDAT_COPY_TO(OpCode).Static then
             FExecutionData.StaticMemory[CompareInt] := r2 { VarDat[1] }
          else
             FStack[FStack.Size-1 + CompareInt] := r2 { VarDat[1] };
        end;

        if PSE2OpDAT_COPY_TO(OpCode).Static then
          FStackHelper.SetVarData(FStack.Top, r1, FExecutionData.MemoryMngr)
        else
          FStackHelper.SetVarData(FStack.Top, r1 { VarDat[0] });
        FStack.Pop;
      end;
  soDAT_COPY_FROM :
      begin
        CompareInt := PSE2OpDAT_COPY_FROM(OpCode).Source;

        if PSE2OpDAT_COPY_FROM(OpCode).Static then
        begin
          r1 { VarDat[0] } := FExecutionData.StaticMemory.List[CompareInt];
          r2 { VarDat[1] } := FStack.PushNew(r1 { VarDat[0] }.AType);
        end else
        begin
          r1 { VarDat[0] } := FStack.List[FStack.Size-1 + CompareInt];
          r2 { VarDat[1] } := FStack.PushNew(r1 { VarDat[0] }.AType);
        end;
        FStackHelper.SetVarData(r1 { VarDat[0] }, r2 { VarDat[1] });
        //r2 { VarDat[1] }^.RefContent := r1 { VarDat[0] }^.RefContent;
      end;
  soDAT_MOVE_TO :
      begin
        CompareInt := PSE2OpDAT_MOVE_TO(OpCode).Target;
        if PSE2OpDAT_MOVE_TO(OpCode).Static then
        begin
          FExecutionData.StaticMemory[CompareInt] := FStack.Top;
          FStack.Pop;
        end else
        begin
          FStack[FStack.Size-1 + CompareInt] := FStack.Top;
          FStack.Pop;
        end;
      end;
  soDAT_MOVE_FROM :
      begin
        CompareInt := PSE2OpDAT_MOVE_FROM(OpCode).Source;
        if PSE2OpDAT_MOVE_FROM(OpCode).Static then
        begin
          FStack.Push(FExecutionData.StaticMemory[CompareInt]);
        end else
        begin
          FStack.Push(FStack[FStack.Size-1 + CompareInt]);
        end;
      end;
  soDAT_CONVERT :
      begin
        r1 { VarDat[0] } := FStack[FStack.Size - 1 + PSE2OpDAT_CONVERT(OpCode).Index];

        if r1 { VarDat[0] }^.RefContent then
        begin
          r3 { VarDat[2] } := FStackHelper.CreateVarData(r1 { VarDat[0] }^.AType);
          FStackHelper.SetVarData(r1 { VarDat[0] }, r3 { VarDat[2] });
          FStackHelper.FreeVarData(r1 { VarDat[0] });
          r1 { VarDat[0] } := r3 { VarDat[2] };

          FStack[FStack.Size - 1 + PSE2OpDAT_CONVERT(OpCode).Index] := r3 { VarDat[2] };
        end;

        if r1.AType <> PSE2OpDAT_CONVERT(OpCode).NewType then
           FStackHelper.ConvertContent(r1 { VarDat[0] }, PSE2OpDAT_CONVERT(OpCode).NewType);
      end;
  soDAT_CHANGETYPE :
      begin                   
        // Only for a break point
        //FStack.MaxSize := FStack.MaxSize;
      end;
  soDAT_SetInt :
      begin
        if not (FStack.Top.AType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64, btU64]) then
        begin
          FStackHelper.FreeVarContent(FStack.Top);
          FStack.Top.AType := btS64;
          FStackHelper.CreateVarContent(FStack.Top);
        end;
        FStackHelper.SetIntContent(FStack.Top, PSE2OpDAT_SetInt(OpCode).Value);
      end;
  soDAT_PUSHInt32 :
      begin
        r1 := FStack.PushNew(btS32);
        r1^.ts32^ := PSE2OpDAT_PUSHInt32(OpCode).Value;
      end;  
  soDAT_PUSHInt64 :
      begin
        r1 := FStack.PushNew(btS64);
        r1^.ts64^ := PSE2OpDAT_PUSHInt64(OpCode).Value;
      end;
  soDAT_PUSHUInt64 :
      begin
        r1 := FStack.PushNew(btU64);
        r1^.ts64^ := PSE2OpDAT_PUSHUInt64(OpCode).Value;
      end;
  soDAT_PUSHFloat4 :
      begin
        r1 := FStack.PushNew(btSingle);
        r1^.tSingle^ := PSE2OpDAT_PUSHFloat4(OpCode).Value;
      end;
  soDAT_PUSHFloat8 :
      begin
        r1 := FStack.PushNew(btDouble);
        r1^.tDouble^ := PSE2OpDAT_PUSHFloat8(OpCode).Value;
      end;         
  soDAT_PUSHPtr :
      begin
        r1 := FStack.PushNew(btPointer);
        Pointer(r1^.tPointer^) := PSE2OpDAT_PUSHPtr(OpCode).Value;
      end;
  soDAT_PUSHRES :
      begin           
        r1 := FStack.PushNew(btString);
        FStackHelper.SetStringContent(r1, FExecutionData.AppCode.Strings[PSE2OpDAT_PUSHRES(OpCode).Index]);
      end;
  soSAVE_INSTANCE :
      begin
        PPointer(FExecutionData.StaticMemory.Top.tPointer)^ := FExecutionData.RunTime;
      end;
  soDAT_SetFloat :
      begin
        r1 { VarDat[0] } := FStack.Top;
        if not (r1 { VarDat[0] }.AType in [btSingle, btDouble]) then
        begin
          FStackHelper.FreeVarContent(FStack.Top);
          FStack.Top.AType := btDouble;
          FStackHelper.CreateVarContent(FStack.Top);
          r1 { VarDat[0] } := FStack.Top;
        end;
        case r1 { VarDat[0] }.AType of
        btSingle : r1 { VarDat[0] }.tSingle^ := PSE2OpDAT_SetFloat(OpCode).Value;
        btDouble : r1 { VarDat[0] }.tDouble^ := PSE2OpDAT_SetFloat(OpCode).Value;
        end;
      end;
  soDAT_SetPtr :
      begin
        if not (FStack.Top.AType in [btPointer, btObject]) then
        begin
          FStackHelper.FreeVarContent(FStack.Top);
          FStack.Top.AType := btPointer;
          FStackHelper.CreateVarContent(FStack.Top);
        end;
        Pointer(FStack.Top.tPointer^) := PSE2OpDAT_SetPtr(OpCode).Value;
      end;
  soDAT_LOADRES :
      begin
        if not (FStack.Top.AType in [btString, btUTF8String, btWideString, btPChar, btAnsiString, btPAnsiChar, btPWideChar]) then
        begin
          FStackHelper.FreeVarContent(FStack.Top);
          FStack.Top.AType := btString;
          FStackHelper.CreateVarContent(FStack.Top);
        end;
        FStackHelper.SetStringContent(FStack.Top, FExecutionData.AppCode.Strings[PSE2OpDAT_LOADRES(OpCode).Index]);
      end;
  soDAT_CLEAR :
      begin
        if PSE2OpDAT_CLEAR(OpCode).Static then
          FExecutionData.StaticHelper.ClearVarContent(FExecutionData.StaticMemory.Top)
        else
          FStackHelper.ClearVarContent(FStack.Top);
      end;
  soSPEC_INCP :
      begin                     
        if PSE2OpSPEC_INCP(OpCode).NoRef then
        begin
          r1 := Pointer(FStack.Top.tPointer^);
          r1 := Pointer(PtrInt(r1) + PSE2OpSPEC_INCP(OpCode).Offset);
          FStack.Pop;

          r2 := FStack.PushNew(PSE2OpSPEC_INCP(OpCode).newType);
          PPointer(r2.tPointer)^ := Pointer(r1);
        end else
        begin
          r1 := Pointer(FStack.Top.tPointer^);
          r1 := Pointer(PtrInt(r1) + PSE2OpSPEC_INCP(OpCode).Offset);
          FStack.Pop;

          r2 := FStack.PushNew($FF);
          r2.AType := PSE2OpSPEC_INCP(OpCode).newType;
          r2.RefContent := True;

          Pointer(r2.tPointer) := Pointer(r1);
        end;
      end;
  soSPEC_DECP   :
      begin
        CompareInt := PSE2OpSPEC_DECP(OpCode).Target;
        r1 { VarDat[0] } := FStack[FStack.Size-1 + CompareInt];
        FStackHelper.SetVarData(FStack.Top, r1 { VarDat[0] });
        FStack.Pop;
      end;
  soSPEC_UNREF  :
      begin
        r1 { VarDat[0] } := FStack.Top;
        if r1 { VarDat[0] }^.RefContent then
        begin
          r2 { VarDat[1] } := FStackHelper.CreateVarData(r1 { VarDat[0] }^.AType);
          FStackHelper.SetVarData(r1 { VarDat[0] }, r2 { VarDat[1] });
          FStack.Pop;
          FStack.Push(r2 { VarDat[1] });
        end;
      end;
  soSPEC_GetRef :
      begin
        CompareInt := PSE2OpSPEC_GetRef(OpCode).Offset;

        if PSE2OpSPEC_GetRef(OpCode).Static then
        begin
          r1 { VarDat[0] } := FExecutionData.StaticMemory[CompareInt].tPointer;
        end else
        begin
          r1 { VarDat[0] } := FStack[FStack.Size-1 + CompareInt].tPointer;
        end;

        if not PSE2OpSPEC_GetRef(OpCode).UsePush then   
           FStack.Pop;

        r2 { VarDat[1] } := FStack.PushNew(btPointer);
        PPointer(r2 { VarDat[1] }.tPointer)^ := Pointer(r1 { VarDat[0] });
      end;
  soSPEC_GetProcPtr :
      begin
        Meta := FExecutionData.AppCode.MetaData[PSE2OpSPEC_GetProcPtr(OpCode).MetaIndex];
        if Meta = nil then
           raise ESE2RunTimeError.Create('Method pointer could not be get');

        if Meta.IsExternal and (Meta.MetaType = mtMethod) then
        begin
          if FExecutionData.FExtMethPtrs then
             Ptr[0] := Pointer(PSE2OpFLOW_CALLEX(FOpCodes.List[Meta.CodePos + 1]).Position)
          else
             Ptr[0] := nil;
        end
        else
           Ptr[0] := Meta;
        Ptr[1] := nil;

        if Meta.HasSelf then
        begin
          Ptr[1] := PPointer(FStack.Top.tPointer)^;
          FStack.Pop;
        end;

        r1 := FStack.PushNew(btProcPtr);
        PPtrArray(r1.tPointer)^ := Ptr;

                             (*
        r1 { VarDat[0] } := FStack.PushNew(btProcPtr);
        PPointer(r1 { VarDat[0] }.tPointer)^ := Meta;
        if Meta.HasSelf then
        begin
          r2 { VarDat[1] } := FStack.Items[FStack.Size - 2];
          PPointer(Integer(r1 { VarDat[0] }^.tPointer) + SizeOf(Pointer))^ := PPointer(r2 { VarDat[1] }.tPointer)^;
          //r3 { VarDat[2] } := r2 { VarDat[1] };
          FStack.Items[FStack.Size - 2] := r1 { VarDat[0] };
          FStack.Pop;
          FStack.Pool.Push(r2 { VarDat[1] });
        end;     *)
      end;
  soSPEC_CREATE :
      begin
        // Self - Pointer
        r1 { VarDat[0] } := FStack[FStack.Size - PSE2OpSPEC_CREATE(OpCode).Variables - 2];
        r2 { VarDat[1] } := FStack[FStack.Size - PSE2OpSPEC_CREATE(OpCode).Variables - 1];

        if PSE2OpSPEC_CREATE(OpCode).MetaIndex = -1 then
           raise ESE2RunTimeError.Create('Runtime error: Meta index not assigned');

        if (not (r1 { VarDat[0] }.AType in [btObject])) or
           (not (r2 { VarDat[1] }.AType in [btObject])) then
           raise ESE2RunTimeError.Create('Runtime error: stack corrupt');

        Pointer(r1 { VarDat[0] }.tPointer^) := FPackedData.CreateScriptClassObject(FExecutionData.AppCode.MetaData[PSE2OpSPEC_CREATE(OpCode).MetaIndex], FExecutionData.AppCode);
        Pointer(r2 { VarDat[1] }.tPointer^) := Pointer(r1 { VarDat[0] }.tPointer^);
        FClassGC.Add(Pointer(r1 { VarDat[0] }.tPointer^));
      end;
  soSPEC_DESTROY :
      begin
        r1 { VarDat[0] } := FStack.Top;

        FClassGC.Delete(Pointer(r1 { VarDat[0] }.tPointer^));
        FExecutionData.NativeList.ClearForClass(Pointer(r1 { VarDat[0] }.tPointer^));
        FPackedData.DestroyScriptClassObject(Pointer(r1 { VarDat[0] }.tPointer^));
      end;
  soREC_MAKE :
      begin
        // Self - Pointer
        r1 { VarDat[0] } := FStack[FStack.Size - PSE2OpREC_MAKE(OpCode).Variables - 1];

        if PSE2OpREC_MAKE(OpCode).MetaIndex = -1 then
           raise ESE2RunTimeError.Create('Runtime error: Meta index not assigned');

        if (not (r1 { VarDat[0] }.AType in [btRecord])) then
           raise ESE2RunTimeError.Create('Runtime error: stack corrupt');

        Pointer(r1 { VarDat[0] }.tPointer^) := FPackedData.CreateScriptRecord(FExecutionData.AppCode.MetaData[PSE2OpREC_MAKE(OpCode).MetaIndex], FExecutionData.AppCode);
      end;
  soMEM_REC_MAKE :
      begin
        // Self - Pointer
        r1 { VarDat[0] } := FExecutionData.StaticMemory[FExecutionData.StaticMemory.Size - PSE2OpMEM_REC_MAKE(OpCode).Variables - 1];

        if PSE2OpMEM_REC_MAKE(OpCode).MetaIndex = -1 then
           raise ESE2RunTimeError.Create('Runtime error: Meta index not assigned');

        if (not (r1 { VarDat[0] }.AType in [btRecord])) then
           raise ESE2RunTimeError.Create('Runtime error: stack corrupt');

        Pointer(r1 { VarDat[0] }.tPointer^) := FExecutionData.PackedData.CreateScriptRecord(FExecutionData.AppCode.MetaData[PSE2OpMEM_REC_MAKE(OpCode).MetaIndex], FExecutionData.AppCode);
      end;
  soREC_FREE :
      begin
        r1 { VarDat[0] } := FStack.Items[FStack.Size - 1 + PSE2OpREC_FREE(OpCode).Offset];

        FRecordGC.Delete(Pointer(r1 { VarDat[0] }.tPointer^));
        FPackedData.DestroyScriptRecord(Pointer(r1 { VarDat[0] }.tPointer^));
      end;
  soMEM_REC_FREE :
      begin  
        r1 { VarDat[0] } := FExecutionData.StaticMemory.Items[FExecutionData.StaticMemory.Size - 1 + PSE2OpMEM_REC_FREE(OpCode).Offset];

        FExecutionData.PackedData.DestroyScriptRecord(Pointer(r1 { VarDat[0] }.tPointer^));
      end;
  soREC_COPY_TO :
      begin
        CompareInt := PSE2OpREC_COPY_TO(OpCode).Target;

        if CompareInt >= 0 then
           r1 := FExecutionData.StaticMemory[CompareInt]
        else
           r1 := FStack[FStack.Size-1 + CompareInt];

        if (r1.AType <> btRecord) or
           (FStack.Top.AType <> btRecord) then
           raise ESE2RunTimeError.Create('Record copy not possible');

        FPackedData.CopyScriptRecord(PPointer(FStack.Top.tPointer)^, PPointer(r1.tPointer)^, FExecutionData.AppCode.MetaData[PSE2OpREC_COPY_TO(OpCode).MetaIndex]);
      end;
  soREC_MARK_DEL  :
      begin
        r1 { VarDat[0] } := FStack.Top;
        if r1 { VarDat[0] }.AType = btRecord then
           FRecordGC.Add(PPointer(r1 { VarDat[0] }.tPointer)^, FStack.Size - 1);
      end;
  soREC_DEL_RECS  :
      begin
        RemoveUnusedRecords(PSE2OpREC_DEL_RECS(OpCode).MaxRecords);
      end;
  soREC_EQUAL :
      begin
        r2 { VarDat[1] } := FStack.Top;
        r1 { VarDat[0] } := FStack.List[FStack.Size - 2]; //r1 { VarDat[0] } := FStack[Stack.Size - 2];

        CompareInt := Ord(FPackedData.ScriptRecordEqual(PPointer(r2^.tPointer)^, PPointer(r1^.tPointer)^,
                          FExecutionData.AppCode.MetaData[PSE2OpREC_EQUAL(OpCode).MetaIndex]));


        FStack.Pop;
        FStack.Pop;
        FStack.PushNew(btBoolean).tu8^ := CompareInt;
      end;
  soREC_UNEQUAL :
      begin      
        r2 { VarDat[1] } := FStack.Top;
        r1 { VarDat[0] } := FStack.List[FStack.Size - 2]; //r1 { VarDat[0] } := FStack[Stack.Size - 2];

        CompareInt := Ord(not FPackedData.ScriptRecordEqual(PPointer(r2^.tPointer)^, PPointer(r1^.tPointer)^,
                          FExecutionData.AppCode.MetaData[PSE2OpREC_EQUAL(OpCode).MetaIndex]));


        FStack.Pop;
        FStack.Pop;
        FStack.PushNew(btBoolean).tu8^ := CompareInt;
      end;
  soINT_INCSTATIC :
      begin
        r1 { VarDat[0] } := FExecutionData.StaticMemory[PSE2OpINT_INCSTATIC(OpCode).Offset];
        r2 { VarDat[1] } := FStack.Pool.Pop(btU32);
        r2 { VarDat[1] }^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FDataOperation.Addition(r1 { VarDat[0] }, r2 { VarDat[1] });
        FStackHelper.FreeVarData(r2 { VarDat[1] });
      end;
  soINT_DECSTATIC :
      begin
        r1 { VarDat[0] } := FExecutionData.StaticMemory[PSE2OpINT_INCSTATIC(OpCode).Offset];
        r2 { VarDat[1] } := FStack.Pool.Pop(btU32);
        r2 { VarDat[1] }^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FDataOperation.Substract(r1 { VarDat[0] }, r2 { VarDat[1] });
        FStackHelper.FreeVarData(r2 { VarDat[1] });
      end;
  soINT_INCSTACK :
      begin
        r1 { VarDat[0] } := FStack[FStack.Size - 1 + PSE2OpINT_INCSTATIC(OpCode).Offset];
        r2 { VarDat[1] } := FStack.Pool.Pop(btU32);
        r2 { VarDat[1] }^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FDataOperation.Addition(r1 { VarDat[0] }, r2 { VarDat[1] });
        FStackHelper.FreeVarData(r2 { VarDat[1] });
      end;
  soINT_DECSTACK :
      begin
        r1 { VarDat[0] } := FStack[FStack.Size - 1 + PSE2OpINT_INCSTATIC(OpCode).Offset];
        r2 { VarDat[1] } := FStack.Pool.Pop(btU32);
        r2 { VarDat[1] }^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FDataOperation.Substract(r1 { VarDat[0] }, r2 { VarDat[1] });
        FStackHelper.FreeVarData(r2 { VarDat[1] });
      end;
  soSAFE_PEX :
      begin
        if FSafeBlocks.TopItem = nil then
           raise ESE2RunTimeError.Create('Could not get exception class');

        PPointer(FStack.PushNew(btObject).tPointer)^ := FSafeBlocks.TopItem.ScriptExcept;
      end;
  soSAFE_STACK  :
      begin
        r1 := FStack.Top;
        PString(r1.tString^)^ := ExceptionCallStack;
      end;
  soSAFE_INTER  :
      begin
        r1 := FStack.Top;
        ESE2ScriptException(r2) := ESE2ScriptException.Create('');
        ESE2ScriptException(r2).ScriptException := PPointer(r1^.tPointer)^;
        FStack.Pop;
        raise ESE2ScriptException(r2);
      end;
  soSAFE_TRYFIN :
      begin
        FSafeBlocks.Add(blFinally, FStack.Size, PSE2OpSAFE_TRYFIN(OpCode).SavePos, PSE2OpSAFE_TRYFIN(OpCode).LeavePos);
      end;
  soSAFE_TRYEX :
      begin
        FSafeBlocks.Add(blExcept, FStack.Size, PSE2OpSAFE_TRYEX(OpCode).SavePos, PSE2OpSAFE_TRYEX(OpCode).LeavePos);
      end;
  soSAFE_BLOCK :
      begin
        TryBlock := FSafeBlocks.TopItem;
        if PSE2OpSAFE_BLOCK(OpCode).StackCheck then
           while FStack.Size > TryBlock.NewStackSize do
           begin
             FStack.Pop;
           end;

        TryBlock.IsAquired := True;
        if TryBlock.BlockType = blExcept then
        begin
          if not FHasException then
             FCodePos := TryBlock.BlockLeave - 1;
        end;
      end;
  soSAFE_TRYEND :
      begin
        TryBlock := FSafeBlocks.TopItem;
        if FHasException then
        begin
          if TryBlock.BlockType = blFinally then
          begin
            TempTryBlock := TSE2TryBlock.Create;
            try
              TempTryBlock.ScriptExcept:= TryBlock.ScriptExcept;
              TempTryBlock.ErrorExcept := TryBlock.ErrorExcept;
              TempTryBlock.ErrorMsg    := TryBlock.ErrorMsg;
              TempTryBlock.ErrorStack  := TryBlock.ErrorStack;
              TempTryBlock.ErrorPos    := TryBlock.ErrorPos;

              FSafeBlocks.Pop;
              TryBlock := FSafeBlocks.TopItem;

              if TryBlock = nil then
              begin
                try
                  DoErrorEvent(TempTryBlock.ErrorExcept, TempTryBlock.ErrorMsg, TempTryBlock.ErrorPos, TempTryBlock.ErrorStack);
                except
                end;
                FCodePos := -1;
                continue;
              end else
              begin
                FCodePos := TryBlock.BlockStart - 1;
                TryBlock.ScriptExcept:= TempTryBlock.ScriptExcept;
                TryBlock.ErrorExcept := TempTryBlock.ErrorExcept;
                TryBlock.ErrorMsg    := TempTryBlock.ErrorMsg;
                TryBlock.ErrorStack  := TempTryBlock.ErrorStack;
                TryBlock.ErrorPos    := TempTryBlock.ErrorPos;
              end;
            finally
              TempTryBlock.Free;
            end;
          end else
          begin
            FHasException := False;
            FSafeBlocks.Pop;
          end;
        end else
        begin
          if TryBlock.ExitTo > -1 then
             FCodePos := TryBlock.ExitTo - 1;
          FSafeBlocks.Pop;
        end;
      end;
  soSAFE_SJUMP :
      begin
        FCodePos                   := PSE2OpSAFE_SJUMP(OpCode).Target - 1;
        FSafeBlocks.TopItem.ExitTo := PSE2OpSAFE_SJUMP(OpCode).ExitTo;
      end;                
  soNOOP : ;
  end;

  FCodePos := FCodePos + 1;

    except
      on E:Exception do
      begin
        TryBlock := FSafeBlocks.TopItem;

        // Get the 1st unaquired try block
        while TryBlock <> nil do
        begin
          if TryBlock.IsAquired then
          begin
            FSafeBlocks.Pop;
            TryBlock := FSafeBlocks.TopItem;
          end else
            break;
        end;

        if TryBlock = nil then
        begin
          DoErrorEvent(ExceptClass(e.ClassType), e.Message, FCodePos, GetCallStack(200));
          // no Exception Manager
          FCodePos := -1;
        end else
        begin
          FHasException := True;
          FCodePos      := TryBlock.BlockStart;

          TryBlock.ErrorStack  := ExceptionCallStack;
          try
            BuildScriptExceptions(TryBlock, E);
          except
          end;

          TryBlock.ErrorExcept := ExceptClass(E.ClassType);
          TryBlock.ErrorMsg    := E.Message;
          TryBlock.ErrorPos    := FCodePos;
        end;
      end;
    end;

  {$IFDEF PERF_MONITOR}
  FPerfMonitor.Stop(OpCode.OpCode);
  {$ENDIF}

  end;
end;

function TSE2ExecutionContext.ExceptionCallStack: string;
var i     : integer;
    sl    : TStringList;
    index : integer;
    p     : PSE2VarData;
begin
  result := '';

  sl := TStringList.Create;
  try
    for i:=FStack.Size-1 downto 0 do
    begin
      p := FStack.List[i];
      if p^.AType = btReturnAddress then
      begin
        index := (p^.tS64^ shr 32);
        if index > 0 then
        begin
          if sl.Count > 0 then
             sl.Add('at ' + MetaEntryToStr(FExecutionData.AppCode.MetaData[index-1], i))
          else
             sl.Add('at ' + MetaEntryToStr(FExecutionData.AppCode.MetaData[index-1], i) + ' ['+IntToStr(FCodePos)+']')
        end;
      end;
      if sl.Count > 300 then
      begin
        sl.Add('...');
        break;
      end;
    end;
    result := sl.Text;
  finally
    sl.Free;
  end;
end;

function TSE2ExecutionContext.GetCallStack(limit: integer = -1): string;
var i     : integer;
    sl    : TStringList;
    index : integer;
    p     : PSE2VarData;
begin
  result := '';

  sl := TStringList.Create;
  try
    for i:=FStack.Size-1 downto 0 do
    begin
      p := FStack.List[i];
      if p^.AType = btReturnAddress then
      begin
        index := (p^.tS64^ shr 32);
        if index > 0 then
        begin
          if sl.Count > 0 then
             sl.Add(MetaEntryToStr(FExecutionData.AppCode.MetaData[index-1], i))
          else
             sl.Add(MetaEntryToStr(FExecutionData.AppCode.MetaData[index-1], i) + ' ['+IntToStr(FCodePos)+']');
        end;
      end;

      if limit > -1 then
        if sl.Count > limit then
        begin
          sl.Add('...');
          break;
        end;
    end;
    result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TSE2ExecutionContext.GetCallStack(Target: TSE2StackTrace);
var i     : integer;
    Item  : TSE2StackItem;
    index : integer;
begin
  for i:=FStack.Size-1 downto 0 do
  begin
    if FStack[i]^.AType = btReturnAddress then
    begin
      index := (FStack[i]^.tS64^ shr 32);
      if index > 0 then
      begin
        Item := Target.Add;
        MetaEntryToItem(FExecutionData.AppCode.MetaData[index - 1], i, Item);
      end;
    end;
  end;
end;

function TSE2ExecutionContext.MethodDescToStr(Entry: TSE2MetaEntry): string;
var i         : integer;
    ParamDecl : byte;
begin
  if Entry = nil then
  begin
    result := 'DEBUG ERROR: Meta Data is null';
    exit;
  end;

  result := Entry.AUnitName + '.' + Entry.Name;
  if Entry.ParamCount > 0 then
  begin
    result := result + '(';
    for i:=0 to Entry.ParamCount-1 do
    begin
      ParamDecl := Ord(Entry.ParamDecl[i + 1]);
      if TSE2ParamHelper.IsVarParam(ParamDecl) then
         result := result + 'var '; 
      result := result + TSE2DebugHelper.VarTypeToStr(TSE2ParamHelper.GetParamType(ParamDecl));
      if i < Entry.ParamCount-1 then
         result := result + ', ';
    end;
    result := result + ')';
  end;

  if Entry.HasResult then
  begin
    result := result + ': ';
    result := result + TSE2DebugHelper.VarTypeToStr(TSE2ParamHelper.GetParamType(Entry.ResultType));
  end;
  result := result + ';';
end;

function TSE2ExecutionContext.MetaEntryToStr(Entry: TSE2MetaEntry;
  StackIndex: integer): string;
var i: integer;
    p: PSE2VarData;
begin
  if Entry = nil then
  begin
    result := 'DEBUG ERROR: Meta Data is null';
    exit;
  end;

  result := Entry.AUnitName + '.' + Entry.Name;
  if Entry.ParamCount > 0 then
  begin
    result := result + '(';
    for i:=0 to Entry.ParamCount-1 do
    begin
      p := FStack[StackIndex - Entry.ParamCount + i];
      if p = nil then
        result := result + '???'
      else
      if p.tPointer = nil then
        result := result + 'nil'
      else
        result := result + TSE2DebugHelper.VarContentToStr(p, nil, 5);
      if i < Entry.ParamCount-1 then
         result := result + ', ';
    end;
    result := result + ')';
  end;

  if Entry.HasResult then
  begin
    result := result + ': ';
    result := result + TSE2DebugHelper.VarContentToStr(FStack[StackIndex - 1 - Entry.ParamCount], nil, 5)
  end;
  result := result + ';';
end;

procedure TSE2ExecutionContext.MetaEntryToItem(Entry: TSE2MetaEntry;
  StackIndex: integer; Target: TSE2StackItem);
var i: integer;
    p: PSE2VarData;
begin
  if Entry = nil then
     exit;

  Target.HasResult := Entry.HasResult;
  if Target.HasResult then
  begin
    Target.ResultValue := TSE2DebugHelper.VarContentToStr(FStack[StackIndex - 1 - Entry.ParamCount], nil, 5);
  end;

  Target.AUnitName := Entry.AUnitName;
  Target.Name      := Entry.Name;
  Target.ParamTrace:= '';

  if Entry.ParamCount > 0 then
  begin
    Target.ParamTrace:= '(';
    for i:=0 to Entry.ParamCount-1 do
    begin
      p := FStack[StackIndex - Entry.ParamCount + i];
      if p = nil then
        Target.ParamTrace := Target.ParamTrace + '???'
      else
      if p.tPointer = nil then
        Target.ParamTrace := Target.ParamTrace + 'nil'
      else
        Target.ParamTrace := Target.ParamTrace + TSE2DebugHelper.VarContentToStr(p, nil, 5);
      if i < Entry.ParamCount-1 then
         Target.ParamTrace := Target.ParamTrace + ', ';
    end;
    Target.ParamTrace := Target.ParamTrace + ')';
  end;
end;

procedure TSE2ExecutionContext.DoErrorEvent(Exp: ExceptClass; const Msg: string; ErrorPos: integer;
  const CallStack: string);
begin
  if not (Exp = EAbort) then
    if Assigned(FOnError) then
       FOnError(Self, Exp, Msg, ErrorPos, CallStack);
end;

procedure TSE2ExecutionContext.Abort;
begin
  FDoAbort := True;
end;

function TSE2ExecutionContext.Call(Method: Pointer;
  const Params: array of const): variant;
var MetaEntry  : TSE2MetaEntry;
    LastEntry  : PSE2VarData;
    SelfPtr    : PSE2VarData;

  procedure SetVariableContent(aParamType: byte; DataType: byte; Data: Pointer; ParamIndex: integer = -1);
  const 
    SParamNotCompatible = 'Parameter not compatible to script method parameter';

  var newEntry    : PSE2VarData;
      fSingle     : single;
      fDouble     : double;
      index       : integer;
      RecMeta     : TSE2MetaEntry;
  begin
    case DataType of
    vtInteger :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btU8      : newEntry.tu8^      := PInteger(Data)^;
          btS8      : newEntry.ts8^      := PInteger(Data)^;
          btU16     : newEntry.tu16^     := PInteger(Data)^;
          btS16     : newEntry.ts16^     := PInteger(Data)^;
          btU32     : newEntry.tu32^     := PInteger(Data)^;
          btS32     : newEntry.ts32^     := PInteger(Data)^;
          btS64     : newEntry.ts64^     := PInteger(Data)^;
          btU64     : newEntry.ts64^     := PInteger(Data)^;
          btSingle  : newEntry.tSingle^  := PInteger(Data)^;
          btDouble  : newEntry.tDouble^  := PInteger(Data)^;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtInt64 :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btU8      : newEntry.tu8^      := PInt64(Data)^;
          btS8      : newEntry.ts8^      := PInt64(Data)^;
          btU16     : newEntry.tu16^     := PInt64(Data)^;
          btS16     : newEntry.ts16^     := PInt64(Data)^;
          btU32     : newEntry.tu32^     := PInt64(Data)^;
          btS32     : newEntry.ts32^     := PInt64(Data)^;
          btS64     : newEntry.ts64^     := PInt64(Data)^;
          btU64     : newEntry.ts64^     := PInt64(Data)^;
          btSingle  : newEntry.tSingle^  := PInt64(Data)^;
          btDouble  : newEntry.tDouble^  := PInt64(Data)^;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtExtended :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btSingle  :
              begin
                fSingle := PExtended(Data)^;
                newEntry.tSingle^  := fSingle;
              end;
          btDouble :
              begin
                fDouble := PExtended(Data)^;
                newEntry.tDouble^  := fDouble;
              end;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtCurrency :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btSingle  :
              begin
                fSingle := PCurrency(Data)^;
                newEntry.tSingle^  := fSingle;
              end;
          btDouble :
              begin
                fDouble := PCurrency(Data)^;
                newEntry.tDouble^  := fDouble;
              end;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtBoolean :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btBoolean : newEntry.tu8^ := Ord(PBoolean(Data)^);
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtString :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          (* OLD CODE: v0.5.3.3
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^)); *)

          
          btString     : PbtString(newEntry.tString^)^     := TbtString(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := TbtWideString(PPointer(Data)^);
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := TbtUTF8STring(PPointer(Data)^);
          btPChar      : PbtPChar(newEntry.tString^)^      := TbtPChar(PPointer(Data)^);
          btAnsiString : PbtAnsiString(newEntry.tString^)^ := TbtAnsiString(PPointer(Data)^);
          btPAnsiChar  : PbtPAnsiChar(newEntry.tString^)^  := TbtPAnsiChar(PPointer(Data)^);
          btPWideChar  : PbtPWideChar(newEntry.tString^)^  := TbtPWideChar(PPointer(Data)^);

          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtAnsiString :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          (* OLD CODE: v0.5.3.3
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));  *)
          
          btString     : PbtString(newEntry.tString^)^     := TbtString(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := TbtWideString(PPointer(Data)^);
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := TbtUTF8STring(PPointer(Data)^);
          btPChar      : PbtPChar(newEntry.tString^)^      := TbtPChar(PPointer(Data)^);
          btAnsiString : PbtAnsiString(newEntry.tString^)^ := TbtAnsiString(PPointer(Data)^);
          btPAnsiChar  : PbtPAnsiChar(newEntry.tString^)^  := TbtPAnsiChar(PPointer(Data)^);
          btPWideChar  : PbtPWideChar(newEntry.tString^)^  := TbtPWideChar(PPointer(Data)^);
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtPChar:
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          (* OLD CODE: v0.5.3.3
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));   *)

          
          btString     : PbtString(newEntry.tString^)^     := TbtString(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := TbtWideString(PPointer(Data)^);
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := TbtUTF8STring(PPointer(Data)^);
          btPChar      : PbtPChar(newEntry.tString^)^      := TbtPChar(PPointer(Data)^);
          btAnsiString : PbtAnsiString(newEntry.tString^)^ := TbtAnsiString(PPointer(Data)^);
          btPAnsiChar  : PbtPAnsiChar(newEntry.tString^)^  := TbtPAnsiChar(PPointer(Data)^);
          btPWideChar  : PbtPWideChar(newEntry.tString^)^  := TbtPWideChar(PPointer(Data)^);

          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtChar :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          (* OLD CODE: v0.5.3.3
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));       *)

          
          btString     : PbtString(newEntry.tString^)^     := TbtString(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := TbtWideString(PPointer(Data)^);
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := TbtUTF8STring(PPointer(Data)^);
          btPChar      : PbtPChar(newEntry.tString^)^      := TbtPChar(PPointer(Data)^);
          btAnsiString : PbtAnsiString(newEntry.tString^)^ := TbtAnsiString(PPointer(Data)^);
          btPAnsiChar  : PbtPAnsiChar(newEntry.tString^)^  := TbtPAnsiChar(PPointer(Data)^);
          btPWideChar  : PbtPWideChar(newEntry.tString^)^  := TbtPWideChar(PPointer(Data)^);

          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtWideString :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          (* OLD CODE: v0.5.3.3
          btString     : PbtString(newEntry.tString^)^     := Utf8ToAnsi(UTF8Encode(WideString(PPointer(Data)^)));
          btWideString : PbtWideString(newEntry.tString^)^ := WideString(PPointer(Data)^);
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := UTF8Encode(WideString(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(Utf8ToAnsi(UTF8Encode(WideString(PPointer(Data)^))));
          *)

          btString     :
              begin
                PbtString(newEntry.tString^)^ :=
                  {$IFDEF DELPHI2009UP}
                    string(WideString(PPointer(Data)^));
                  {$ELSE}
                    Utf8ToAnsi(UTF8Encode(WideString(PPointer(Data)^)));
                  {$ENDIF}
              end;
          btWideString : PbtWideString(newEntry.tString^)^ := WideString(PPointer(Data)^);
          btUTF8String :
              begin
                PbtUTF8String(newEntry.tString^)^ :=
                  {$IFDEF DELPHI2009UP}
                    UTF8Encode(WideString(PPointer(Data)^));
                  {$ELSE}
                    UTF8Encode(WideString(PPointer(Data)^));
                  {$ENDIF}
              end;
          btPChar      :
              begin
                PbtPChar(newEntry.tString^)^      :=
                  {$IFDEF DELPHI2009UP}
                    PChar(WideString(PPointer(Data)^));
                  {$ELSE}
                    PChar(Utf8ToAnsi(UTF8Encode(WideString(PPointer(Data)^))));
                  {$ENDIF}
              end;
          btAnsiString : PbtAnsiString(newEntry.tString^)^ := TbtAnsiString(WideString(PPointer(Data)^));
          btPAnsiChar  : PbtPAnsiChar(newEntry.tString^)^  := TbtPAnsiChar(AnsiString(WideString(PPointer(Data)^)));
          btPWideChar  : PbtPWideChar(newEntry.tString^)^  := TbtPWideChar(WideString(PPointer(Data)^));

          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtPointer :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btPointer : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          btObject  : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          btRecord  :
             begin
               index := MetaEntry.RTTI.FindSize(btRecord, ParamIndex);
               if index < 0 then
                  raise ESE2CallParameterError.Create(SParamNotCompatible);

               RecMeta := FExecutionData.AppCode.MetaData[index];
               if RecMeta = nil then
                  raise ESE2CallParameterError.Create(SParamNotCompatible);

               Pointer(newEntry.tPointer^) := FPackedData.CreateScriptRecord(RecMeta, FExecutionData.AppCode);
               FPackedData.DelphiToScriptRecord(PPointer(Data)^, Pointer(newEntry.tPointer^), RecMeta);
             end;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtClass :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btPointer : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          btObject  : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtObject :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btPointer : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          btObject  : Pointer(newEntry.tPointer^) := PPointer(Data)^;
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    else raise ESE2CallParameterError.Create('Unsupported parameter');
    end;
    LastEntry := newEntry;
  end;

  procedure PushParamsToStack;
  const
    SParamNotCompatible = 'Parameter not compatible to script method parameter';
    SParamNotVarParam   = 'var parameters must have pointer to values';

  var i           : integer;
      bIsVarParam : boolean;
      aParamType  : byte;
      Parameter   : TVarRec;
      RecMeta     : TSE2MetaEntry;
  begin
    // result
    if MetaEntry.HasResult then
       FStack.PushNew(MetaEntry.ResultType);

    if MetaEntry.ResultType = btRecord then
    begin
      i := MetaEntry.RTTI.FindSize(btRecord, -1);
      if i < 0 then
         raise ESE2CallParameterError.Create(SParamNotCompatible);

      RecMeta := FExecutionData.AppCode.MetaData[i];
      if RecMeta = nil then
         raise ESE2CallParameterError.Create(SParamNotCompatible);

      Pointer(FStack.Top.tPointer^) := FPackedData.CreateScriptRecord(RecMeta, FExecutionData.AppCode);
    end;

    // parameters
    for i:=Low(Params) to High(Params) do
    begin
      Parameter   := Params[i];
      bIsVarParam := TSE2ParamHelper.IsVarParam(Ord(MetaEntry.ParamDecl[i+1]));
      aParamType  := TSE2ParamHelper.GetParamType(Ord(MetaEntry.ParamDecl[i+1]));

      if bIsVarParam then
        if Parameter.VType <> vtPointer then
          raise ESE2CallParameterError.Create(SParamNotVarParam);

      if not bIsVarParam then
      begin
        case Parameter.VType of
        vtInteger      : SetVariableContent(aParamType, vtInteger, @Parameter.VInteger);
        vtInt64        : SetVariableContent(aParamType, vtInt64, Parameter.VInt64);
        vtExtended     : SetVariableContent(aParamType, vtExtended, Parameter.VExtended);
        vtCurrency     : SetVariableContent(aParamType, vtCurrency, Parameter.VCurrency);
        vtBoolean      : SetVariableContent(aParamType, vtBoolean, @Parameter.VBoolean);
        vtString       : SetVariableContent(aParamType, vtString, Parameter.VString);
        vtAnsiString   : SetVariableContent(aParamType, vtAnsiString, @Parameter.VAnsiString);
        vtPChar        : SetVariableContent(aParamType, vtPChar, @Parameter.VPChar);
        vtChar         : SetVariableContent(aParamType, vtChar, @Parameter.VChar);
        vtWideString   : SetVariableContent(aParamType, vtWideString, @Parameter.VWideString);
        vtPointer      : SetVariableContent(aParamType, vtPointer, @Parameter.VPointer, i);
        vtClass        : SetVariableContent(aParamType, vtClass, @Parameter.VClass);
        vtObject       : SetVariableContent(aParamType, vtObject, @Parameter.VObject);
        else raise ESE2CallParameterError.Create('Unsupported parameter');
        end;
      end else
      begin
        case aParamType of
        btU8, btS8, btU16, btS16, btU32, btS32 :
            SetVariableContent(aParamType, vtInteger, Parameter.VPointer);
        btS64, btU64 :
            SetVariableContent(aParamType, vtInt64, Parameter.VPointer);
        btSingle, btDouble :
            SetVariableContent(aParamType, btExtended, Parameter.VPointer);
        btString, btUTF8String, btAnsiString :
            SetVariableContent(aParamType, vtAnsiString, Parameter.VPointer);
        btWideString :
            SetVariableContent(aParamType, vtWideString, Parameter.VPointer);
        btPChar, btPAnsiChar, btPWideChar :
            SetVariableContent(aParamType, vtAnsiString, Parameter.VPointer);
        btPointer, btRecord, btArray, btObject :
            SetVariableContent(aParamType, vtPointer, Parameter.VPointer, i);
        end;
      end;

      if MetaEntry.HasSelf then
        if i = 0 then
          SelfPtr := LastEntry;
    end;

    // return address
    FStack.PushNew(btReturnAddress)^.ts64^ := int64((Int64(MetaEntry.Index) shl 32) and int64($00000000));
  end;

  procedure PopParamsFromStack;
  var i           : integer;
      bIsVarParam : boolean;
      Parameter   : TVarRec;
      Data        : PSE2VarData;
      tMeta       : TSE2MetaEntry;
  begin
    // return address
       // already pop-ed by OP_FLOW_RET

    // parameters
    for i:=High(Params) downto Low(Params) do
    begin
      bIsVarParam := TSE2ParamHelper.IsVarParam(Ord(MetaEntry.ParamDecl[i+1]));
      Data := FStack.Top;
      if bIsVarParam then
      begin
        Parameter   := Params[i];
        case Data^.AType of
        btU8           : PbtU8(Parameter.VPointer)^     := Data^.tU8^;
        btS8           : PbtS8(Parameter.VPointer)^     := Data^.tS8^;
        btU16          : PbtU16(Parameter.VPointer)^    := Data^.tU16^;
        btS16          : PbtS16(Parameter.VPointer)^    := Data^.tS16^;
        btU32          : PbtU32(Parameter.VPointer)^    := Data^.tU32^;
        btS32          : PbtS32(Parameter.VPointer)^    := Data^.tS32^;
        btS64          : PbtS64(Parameter.VPointer)^    := Data^.tS64^;
        btU64          : PbtS64(Parameter.VPointer)^    := Data^.tS64^;
        btSingle       : PbtSingle(Parameter.VPointer)^ := Data^.tSingle^;
        btDouble       : PbtDouble(Parameter.VPointer)^ := Data^.tDouble^;
        btString       : PbtString(Parameter.VPointer)^ := PbtString(Data^.tString^)^;
        btUTF8String   : PbtUTF8String(Parameter.VPointer)^ := PbtUTF8String(Data^.tString^)^;
        btWideString   : PbtWideString(Parameter.VPointer)^ := PbtWideString(Data^.tString^)^;
        btPChar        : PbtPChar(Parameter.VPointer)^      := PbtPChar(Data^.tString^)^;
        btAnsiString   : PbtAnsiString(Parameter.VPointer)^ := PbtAnsiString(Data^.tString^)^; 
        btPAnsiChar    : PbtPAnsiChar(Parameter.VPointer)^      := PbtPAnsiChar(Data^.tString^)^;
        btPWideChar    : PbtPWideChar(Parameter.VPointer)^      := PbtPWideChar(Data^.tString^)^;
        btPointer,
        btArray        : PPointer(Parameter.VPointer)^    := Pointer(Data^.tPointer^);
        btRecord       :
            begin
              tMeta := FExecutionData.AppCode.MetaData[MetaEntry.RTTI.FindSize(btRecord, i)];
                FPackedData.ScriptToDelphiRecord(PPointer(Data^.tPointer)^, Parameter.VPointer,
                                     tMeta );


              //Self.FRunClasses.ScriptToDelphiRecord(PPointer(Data^.tPointer)^, Parameter.VPointer);
            end;
        end;
      end;

      if Data^.AType = btRecord then
        if Pointer(Data^.tPointer^) <> nil then
        begin
          FPackedData.DestroyScriptRecord(Pointer(Data^.tPointer^));
        end;


      FStack.Pop;
    end;

    // result
    if MetaEntry.HasResult then
    begin
      case FStack.Top^.AType of
      btU8         : result := FStack.Top^.tU8^;
      btS8         : result := FStack.Top^.tS8^;
      btU16        : result := FStack.Top^.tU16^;
      btS16        : result := FStack.Top^.tS16^;
      btU32        : result := FStack.Top^.tU32^;
      btS32        : result := FStack.Top^.tS32^;
      btS64        : result := FStack.Top^.tS64^;
      btU64        : result := FStack.Top^.tS64^;
      btSingle     : result := FStack.Top^.tSingle^;
      btDouble     : result := FStack.Top^.tDouble^;
      btString     : result := PbtString(FStack.Top^.tString^)^;
      btUTF8String : result := PbtUTF8String(FStack.Top^.tString^)^;
      btWideString : result := PbtWideString(FStack.Top^.tString^)^;
      btPChar      : result := string(PbtPChar(FStack.Top^.tString^)^);
      btAnsiString : result := PbtAnsiString(FStack.Top^.tString^)^;  
      btPAnsiChar  : result := AnsiString(PbtPAnsiChar(FStack.Top^.tString^)^);
      btPWideChar  : result := WideString(PbtPWideChar(FStack.Top^.tString^)^);
      btArray,
      btPointer,
      btObject     : result := cardinal(FStack.Top^.tPointer^);
      btRecord     :
          begin
            {index := MetaEntry.RTTI.FindSize(btRecord, ParamIndex);
               if index < 0 then
                  raise ESE2CallParameterError.Create(SParamNotCompatible);

               RecMeta := FAppCode.MetaData[index];
               if RecMeta = nil then
                  raise ESE2CallParameterError.Create(SParamNotCompatible);

               Pointer(newEntry.tPointer^) := FRunClasses.CreateScriptRecord(RecMeta, FAppCode);
               FRunClasses.DelphiToScriptRecord(PPointer(Data)^, Pointer(newEntry.tPointer^));
            }

            tMeta := FExecutionData.AppCode.MetaData[MetaEntry.RTTI.FindSize(btRecord, -1)];
              result := cardinal(FPackedData.ScriptToDelphiRecord(Pointer(FStack.Top^.tPointer^),
                                    tMeta));
              FPackedData.DestroyScriptRecord(Pointer(FStack.Top^.tPointer^));
            {result := cardinal(FRunClasses.ScriptToDelphiRecord(Pointer(FStack.Top^.tPointer^)));
            FRunClasses.DestroyScriptRecord(Pointer(FStack.Top^.tPointer^));}
          end;
      end;
      FStack.Pop;
    end;
  end;

var OldCodePos   : cardinal;
    OldStackSize : integer;
    Methods      : TSE2DynMethodList;
    Index        : integer;
begin
  SelfPtr := nil;
  if Method = nil then
     raise ESE2NullReferenceError.Create('Method can not be nil');

  MetaEntry := Method;

  if length(Params) <> MetaEntry.ParamCount then
     raise ESE2CallParameterError.Create('Not enough parameters');

  OldStackSize := MaxInt;
  OldCodePos := FCodePos;
  try
    PushParamsToStack;
    // -1 because of the return value, which is automatically
    // removed after Self.Process
    if MetaEntry.HasResult then
      OldStackSize := FStack.Size
    else
      OldStackSize := FStack.Size - 1;
    FDoAbort   := False;

    if (MetaEntry.DynIndex > -1) and (SelfPtr <> nil) then
    begin
      Methods  := FPackedData.GetClassMethods(PPointer(SelfPtr.tPointer)^);
      Index    := Methods[MetaEntry.DynIndex];
      if Index > 0 then
      begin
        FCodePos := index;
        FOpCodes := ExecutionData.AppCode.OpCodes;
        Process;
      end;
    end else
    begin
      FCodePos   := MetaEntry.CodePos;
      FOpCodes   := ExecutionData.AppCode.OpCodes;
      Process;
    end;
  finally
    while FStack.Size > OldStackSize do
      FStack.Pop;
    
    PopParamsFromStack;
    FCodePos := OldCodePos;
  end;
end;

function TSE2ExecutionContext.ScriptAsMethod(const Method,
  ClassInstance: Pointer): TMethod;
var CodeData : Pointer;
begin
  if FExecutionData.NativeList = nil then
     exit;

  if Method = nil then
     raise ESE2NullReferenceError.Create('Method pointer can not be nil');


  CodeData := FExecutionData.NativeList.GenEntry(Self, Method, ClassInstance);
  result.Data := CodeData;
  result.Code := @SE2MethodScriptCallHandler;
end;

function TSE2ExecutionContext.MethodAsScript(Data, MethodPos,
  ClassPtr: Pointer): boolean;
begin
  result := FExecutionData.NativeList.FindEntry(Data, MethodPos, ClassPtr);
end;

procedure TSE2ExecutionContext.BuildScriptExceptions(TryBlock: TSE2TryBlock;
  ex: Exception);
var p1, p2       : Pointer;
    Meta         : TSE2MetaEntry;
    isUnknown    : boolean;
begin
  if (ex is ESE2ScriptException) then
  begin
    TryBlock.ScriptExcept := ESE2ScriptException(ex).ScriptException;
    exit;
  end;

  Meta := FExecutionData.CodeAccess.Exceptions.GetException(ex.ClassType, isUnknown);
  if Meta <> nil then
  begin
    TryBlock.ScriptExcept := FPackedData.CreateScriptClassObject(Meta, FExecutionData.AppCode );
    FClassGC.Add(TryBlock.ScriptExcept);

    // Message
    p1 := Pointer(cardinal(TryBlock.ScriptExcept) + 0);
    // CallStack
    p2 := Pointer(cardinal(TryBlock.ScriptExcept) + 4);

    try
      PString(p1^)^ := ex.Message;
    except
      PString(p1^)^ := '';
    end;
    PString(p2^)^ := TryBlock.ErrorStack;

    if isUnknown then
    begin
      p1 := Pointer(cardinal(TryBlock.ScriptExcept) + 8);
      PString(p1^)^ := ex.ClassName;
    end;
  end;
end;

procedure TSE2ExecutionContext.Initialize;
begin
  FSafeBlocks.Clear;
  FClassGC.Clear;
  FRecordGC.Clear;
end;

procedure TSE2ExecutionContext.SetExecutionData(value: TSE2ExecutionData);
begin
  Self.FExecutionData := value;
end;

end.

