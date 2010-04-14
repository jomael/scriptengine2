unit uSE2RunTime;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2RunType, uSE2Consts, uSE2BaseTypes, uSE2OpCode, uSE2PEData, uSE2RunCall,
  uSE2RunOperation, uSE2RunAccess, uSE2SafeBlockMngr, uSE2DebugData, uSE2MemoryManager, uSE2PerfMonitor,
  uSE2NativeCallList, uSE2RunTimeClasses;

type
  TSE2RunTimeError = procedure(Sender: TObject; Exp: ExceptClass; const Msg: string; CodePos: integer; const CallStack: string) of object;

  {.$DEFINE PERF_MONITOR}

  {$IFDEF SEII_FPC}
    {$UNDEF PERF_MONITOR}
  {$ENDIF}

  TSE2RunTime = class(TSE2Object)
  private
    FAppCode           : TSE2PE;
    FStack             : TSE2Stack;
    FCodeAccess        : TSE2RunAccess;
    FNativeList        : TSE2NativeCallList;
    FMethodCall        : TSE2MethodCall;

    {$IFDEF PERF_MONITOR}
    FPerfMonitor       : TSE2PerfMonitor;
    {$ENDIF}
    FMemoryManager     : TSE2MemoryManager;
    FRunClasses        : TSE2RunTimeClasses;
    FVarHelper         : TSE2VarHelper;
    FVarOperation      : TSE2VarOperation;
    FVarCompare        : TSE2VarCompare;
    FClassGC           : TSE2ClassGC;
    FRecordGC          : TSE2RecordGC;

    FCodePos           : integer;
    FInitialized       : boolean;
    FDoAbort           : boolean;

    { Exception Handling }
    FSafeBlocks        : TSE2SafeBlockMngr;
    FHasException      : boolean;

    FOnError           : TSE2RunTimeError;
    {$IFNDEF SEII_NO_RUN_EVENT}
    FOnBeforeOperation : TNotifyEvent;
    {$ENDIF}
  protected
    procedure SetAppCode(const value: TSE2PE);

    procedure DoErrorEvent(Exp: ExceptClass; const Msg: string; ErrorPos: integer; const CallStack: string);

    function  MetaEntryToStr(Entry: TSE2MetaEntry; StackIndex: integer): string;
    procedure MetaEntryToItem(Entry: TSE2MetaEntry; StackIndex: integer; Target: TSE2StackItem);

    procedure ProcessGC;
    procedure ProcessRecordGC;
    procedure RecordDeleteEvent(Data: PSE2VarData);
  public
    constructor Create; override;
    destructor Destroy; override;

                              
    procedure Process; // do not call directly
    procedure Initialize;
    procedure Run;
    function  Call(Method: Pointer; Params: array of const): variant;
    procedure Finalize;

    procedure Abort;
    function  GetCallStack: string; overload;
    procedure GetCallStack(Target: TSE2StackTrace); overload;

    function  CreateClass(const Meta: TSE2MetaEntry): Pointer;
    procedure FreeClass(AClass: Pointer);
    function  ScriptAsMethod(Method, ClassInstance: Pointer): TMethod;
    function  MethodAsScript(Data: Pointer; MethodPos, ClassPtr: Pointer): boolean;



    // Only for Debugging the component
      property Stack   : TSE2Stack read FStack;
      property AppCode : TSE2PE    read FAppCode   write SetAppCode;
      property CodePos : integer   read FCodePos   write FCodePos;
      {$IFDEF PERF_MONITOR}
      property PerfMonitor : TSE2PerfMonitor read FPerfMonitor;
      {$ENDIF}
    // Debuging Component End

    property RunTimeClasses    : TSE2RunTimeClasses read FRunClasses;
    property RecordClassList   : TSE2RecordGC     read FRecordGC;
    property ScriptClassList   : TSE2ClassGC      read FClassGC;
    property CodeAccess        : TSE2RunAccess    read FCodeAccess;
    property Initialized       : boolean          read FInitialized;

    // Events
    property OnError           : TSE2RunTimeError read FOnError              write FOnError;
    {$IFNDEF SEII_NO_RUN_EVENT}
    property OnBeforeOperation : TNotifyEvent     read FOnBeforeOperation    write FOnBeforeOperation;
    {$ENDIF}
  end;

implementation

uses  uSE2UnitManager, uSE2SystemUnit, uSE2NativeScriptCall;

{ TSE2RunTime }

constructor TSE2RunTime.Create;
begin
  inherited;
  {$IFDEF PERF_MONITOR}
  FPerfMonitor   := TSE2PerfMonitor.Create;
  {$ENDIF}
  FMemoryManager := TSE2MemoryManager.Create;

  FRunClasses    := TSE2RunTimeClasses.Create(FMemoryManager);  
  FVarHelper     := TSE2VarHelper.Create(FMemoryManager, RecordDeleteEvent);
  FVarOperation  := TSE2VarOperation.Create(FVarHelper);
  FVarCompare    := TSE2VarCompare.Create(FVarHelper);
  FClassGC       := TSE2ClassGC.Create;
  FRecordGC      := TSE2RecordGC.Create;

  FStack         := TSE2Stack.Create(FVarHelper);
  FSafeBlocks    := TSE2SafeBlockMngr.Create;

  FMethodCall    := TSE2MethodCall.Create(Self);
end;

destructor TSE2RunTime.Destroy;
begin
  FreeAndNil(FCodeAccess);
  FreeAndNil(FNativeList);

  FAppCode.Free;
  FSafeBlocks.Free;
  FStack.Free;
  FClassGC.Free;
  FRecordGC.Free;
  FMethodCall.Free;

  FVarHelper.Free;
  FVarOperation.Free;
  FVarCompare.Free;
  FMemoryManager.Free;
  FRunClasses.Free;
  {$IFDEF PERF_MONITOR}
  FPerfMonitor.Free;
  {$ENDIF}
  inherited;
end;

procedure TSE2RunTime.ProcessRecordGC; 
var i: integer;
begin
  for i:=FRecordGC.Count-1 downto 0 do
    FRunClasses.DestroyScriptRecord(PSE2RecordGCEntry(FRecordGC[i])^.Ptr);
  FRecordGC.Clear;
end;

procedure TSE2RunTime.ProcessGC;
var i: integer;
begin
  for i:=FClassGC.Count-1 downto 0 do
    FreeClass(FClassGC[i]);
  FClassGC.Clear;
end;

procedure TSE2RunTime.Finalize;
begin
  if FAppCode = nil then
     exit;
     
  if not FInitialized then
     exit;

  FDoAbort := False;
  FCodePos := FAppCode.FinalizationPoint;
  Process;

  ProcessGC;
  ProcessRecordGC;

  FInitialized := False;
end;

procedure TSE2RunTime.Initialize;
begin
  if FAppCode = nil then
     exit;

  if FInitialized then
     exit;

  FSafeBlocks.Clear;                        
  FInitialized := True;
  FClassGC.Clear;
  FRecordGC.Clear;
  
  FCodePos := FAppCode.InitializationPoint;
  FDoAbort := False;
  Process;
end;

procedure TSE2RunTime.Run;
begin
  if not FInitialized then
     exit;

  {$IFDEF PERF_MONITOR}
  FPerfMonitor.Clear;
  {$ENDIF}

  FCodePos := FAppCode.MainMethodPoint;
  FDoAbort := False;
  Process;
end;

procedure TSE2RunTime.SetAppCode(const value: TSE2PE);
begin
  if FInitialized then
     exit;

  FreeAndNil(FCodeAccess);
  FreeAndNil(FNativeList);
  FAppCode := value;
  if value <> nil then
  begin
    FCodeAccess := TSE2RunAccess.Create(value);
    if not value.PointerReady then
    begin
      TSE2UnitManager.RegisterMethods(FCodeAccess);
      value.PointerReady := True;
    end;
    FNativeList := TSE2NativeCallList.Create;
  end;
end;

procedure TSE2RunTime.RecordDeleteEvent(Data: PSE2VarData);
var p: Pointer;
    i: integer;
begin
  p := Pointer(Data.tPointer^);
  i := FRecordGC.IndexOf(p, FStack.Size - 1);
  if i > -1 then
  begin
    FRecordGC.Delete(i);
    FRunClasses.DestroyScriptRecord(p);
  end;
end;

procedure TSE2RunTime.Process;

type
  PPosArray = ^TPosArray;
  TPosArray = array[0..1] of integer;

var OpCode        : PSE2OpDefault;
    CompareInt    : integer;
    Pos           : TPosArray;
    r1, r2, r3, r4: PSE2VarData;
    //VarDat        : array[0..3] of PSE2VarData;
    Meta          : TSE2MetaEntry;
    TryBlock      : TSE2TryBlock;
    TempTryBlock  : TSE2TryBlock;
begin
  FHasException := False;
  while (FCodePos > 0) and (not FDoAbort) do
  begin
    try
  OpCode := FAppCode.OpCodes.Data[FCodePos];
  if OpCode = nil then
     exit;

  {$IFNDEF SEII_NO_RUN_EVENT}
  if Assigned(FOnBeforeOperation) then
     FOnBeforeOperation(Self);
  {$ENDIF}

  {$IFDEF PERF_MONITOR}
  FPerfMonitor.Start;
  {$ENDIF}

  case OpCode.OpCode of
  soSTACK_INC  :
      begin
        FStack.PushNew(PSE2OpSTACK_INC(OpCode).AType);
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
        if FVarHelper.VarHasValue(FStack.Top, @CompareInt) then
           FCodePos := PSE2OpFLOW_JIZ(OpCode).Position - 1;
        FStack.Pop;           
      end;
  soFLOW_JNZ :
      begin
        CompareInt := 0;
        if not FVarHelper.VarHasValue(FStack.Top, @CompareInt) then
           FCodePos := PSE2OpFLOW_JNZ(OpCode).Position - 1;
        FStack.Pop;
      end;
  soFLOW_CALL :
      begin
        FCodePos := PSE2OpFLOW_CALL(OpCode).Position - 1;
      end;
  soFLOW_CALLEX :
      begin
        FMethodCall.Clear;
        FMethodCall.Run(Pointer(PSE2OpFLOW_CALLEX(OpCode).Position), FAppCode.MetaData[PSE2OpFLOW_CALLEX(OpCode).MetaIndex]);
      end;
  soFLOW_CALLDYN :
      begin
        r1 { VarDat[0] } := FStack.Top;//Items[FStack.Size - 2];

        if not (r1 { VarDat[0] }.AType in [btObject, btPointer]) then
           raise ESE2RunTimeError.Create('Internal error: dynamic method call outside of a class');

        if Pointer(r1 { VarDat[0] }.tPointer^) = nil then
           raise ESE2NullReferenceError.Create('Called class is not assigned');

        CompareInt := FRunClasses.GetClassMethods(PPointer(r1 { VarDat[0] }^.tPointer)^).Items[PSE2OpFLOW_CALLDYN(OpCode)^.Offset];

        if CompareInt < 1 then
           raise ESE2RunTimeError.Create('Abstract method was not implemented');

        FCodePos := CompareInt - 1;
        Stack.Pop;
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
        if Meta.HasSelf then
        begin
          r3 { VarDat[2] } := FStack.Items[FStack.Size - Meta.ParamCount - 2];

          r2 { VarDat[1] } := FVarHelper.CreateVarData(btProcPtr);
          FVarHelper.CreateVarContent(r2 { VarDat[1] });
          PPointer(r2 { VarDat[1] }^.tPointer)^ := PPointer(Integer(r3 { VarDat[2] }^.tPointer) + SizeOf(Pointer))^;

          FStack.Items[FStack.Size - Meta.ParamCount - 2] := r2 { VarDat[1] };
          FStack.Pool.Push(r3 { VarDat[2] });

          if Meta.DynIndex > -1 then
          begin
            FCodePos := FRunClasses.GetClassMethods(PPointer(r2 { VarDat[1] }^.tPointer)^).Items[Meta.DynIndex];
            if FCodePos < 1 then
               raise ESE2RunTimeError.Create('Abstract method was not implemented');
            FCodePos := FCodePos - 1;
          end;
        end;
        Stack.Pop;
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
        while Stack.Size > CompareInt do
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
  soFLOW_PUSHRET :
      begin
        Pos[1] := PSE2OpFLOW_PUSHRET(OpCode).DebugData;
        Pos[0] := PSE2OpFLOW_PUSHRET(OpCode).Position;

        FStack.PushNew(btReturnAddress)^.ts64^ := PInt64(@(Pos[0]))^;

        (*FStack.PushNew(btReturnAddress).ts64^ :=
          (Int64(PSE2OpFLOW_PUSHRET(OpCode).DebugData shl 32)) or
          ((Int64(PSE2OpFLOW_PUSHRET(OpCode).Position) and $FFFFFFFF);*)
      end;
  soOP_OPERATION :
      begin
        case PSE2OpOP_OPERATION(OpCode).OpType of
        1, 20,
        21      :
            begin
              r1 { VarDat[0] } := Stack.Top;

              if r1 { VarDat[0] }^.RefContent then
              begin
                r3 { VarDat[2] } := FVarHelper.CreateVarData(r1 { VarDat[0] }^.AType);
                FVarHelper.SetVarData(r1 { VarDat[0] }, r3 { VarDat[2] });
                FVarHelper.FreeVarData(r1 { VarDat[0] });

                FStack[FStack.Size - 1] := r3 { VarDat[2] };
              end;

              case PSE2OpOP_OPERATION(OpCode).OpType of
              1  : FVarOperation.Negation(FStack.Top); // negation
              20 : FVarOperation.BitNot(FStack.Top);
              21 : FVarOperation.BooleanNot(FStack.Top);
              end;
            end;
        2..11   :
            begin
              r2 { VarDat[1] } := Stack.Top;
              r1 { VarDat[0] } := FStack.Data[FStack.Size - 2]; //r1 { VarDat[0] } := Stack[Stack.Size - 2];

              if r1 { VarDat[0] }^.RefContent then
              begin
                r3 { VarDat[2] } := FVarHelper.CreateVarData(r1 { VarDat[0] }^.AType);
                FVarHelper.SetVarData(r1 { VarDat[0] }, r3 { VarDat[2] });
                FVarHelper.FreeVarData(r1 { VarDat[0] });
                r1 { VarDat[0] } := r3 { VarDat[2] };

                Stack[Stack.Size - 2] := r3 { VarDat[2] };
              end;

              case PSE2OpOP_OPERATION(OpCode).OpType of
              2  : uSE2RunOperation.mTable_add[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper);
              3  : uSE2RunOperation.mTable_sub[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper);
              4  : uSE2RunOperation.mTable_mul[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper);
              5  : uSE2RunOperation.mTable_div[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper);
              6  : uSE2RunOperation.mTable_and[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              7  : uSE2RunOperation.mTable_or [r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              8  : uSE2RunOperation.mTable_xor[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              9  : uSE2RunOperation.mTable_mod[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              10 : uSE2RunOperation.mTable_shr[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              11 : uSE2RunOperation.mTable_shl[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              end;

              (*
              case PSE2OpOP_OPERATION(OpCode).OpType of
              2  : FVarOperation.Addition(r1 { VarDat[0] }, r2 { VarDat[1] });
              3  : FVarOperation.Substract(r1 { VarDat[0] }, r2 { VarDat[1] });
              4  : FVarOperation.Multiply(r1 { VarDat[0] }, r2 { VarDat[1] });
              5  : FVarOperation.Divide(r1 { VarDat[0] }, r2 { VarDat[1] });
              6  : FVarOperation.BitAnd(r1 { VarDat[0] }, r2 { VarDat[1] });
              7  : FVarOperation.BitOr(r1 { VarDat[0] }, r2 { VarDat[1] });
              8  : FVarOperation.BitXor(r1 { VarDat[0] }, r2 { VarDat[1] });
              9  : FVarOperation.DivideMod(r1 { VarDat[0] }, r2 { VarDat[1] });
                   //mTable_mod[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              10 : FVarOperation.BitShr(r1 { VarDat[0] }, r2 { VarDat[1] });
              11 : FVarOperation.BitShl(r1 { VarDat[0] }, r2 { VarDat[1] });
              end;
              *)

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
                r1 := FStack.Data[CompareInt];
                r2 := FStack.PushNew(r1.AType);
              end else
              begin
                r1 := FStack.Data[FStack.Size-1 + CompareInt];
                r2 := FStack.PushNew(r1.AType);
              end;
              FVarHelper.SetVarData(r1, r2);

              case PSE2OpOP_OPERATION(OpCode).OpType of
              1  : FVarOperation.Negation(FStack.Top); // negation
              20 : FVarOperation.BitNot(FStack.Top);
              21 : FVarOperation.BooleanNot(FStack.Top);
              end;
            end;
        2..11   :
            begin
              { 1st Parameter }
              CompareInt := PSE2OpOP_FASTOPERATION(OpCode).Src1;
              if CompareInt >= 0 then  // Static
                r2 := FStack[CompareInt]
              else
                r2 := FStack[FStack.Size + CompareInt];

              { 2nd Parameter }
              CompareInt := PSE2OpOP_FASTOPERATION(OpCode).Src2;
              if CompareInt >= 0 then // Static
                r3 := FStack[CompareInt]
              else
                r3 := FStack[FStack.Size - 1 + CompareInt];

              r1 := FStack.PushNew(r3.AType); 
              FVarHelper.SetVarData(r3, r1);

              case PSE2OpOP_OPERATION(OpCode).OpType of
              2  : uSE2RunOperation.mTable_add[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper);
              3  : uSE2RunOperation.mTable_sub[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper);
              4  : uSE2RunOperation.mTable_mul[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper);
              5  : uSE2RunOperation.mTable_div[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper);
              6  : uSE2RunOperation.mTable_and[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              7  : uSE2RunOperation.mTable_or [r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              8  : uSE2RunOperation.mTable_xor[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              9  : uSE2RunOperation.mTable_mod[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              10 : uSE2RunOperation.mTable_shr[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              11 : uSE2RunOperation.mTable_shl[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] });
              end;
            end;
        end;
      end;
  soOP_COMPARE :
      begin
        //QueryPerformanceFrequency(c);
        //QueryPerformanceCounter(t1);
        r2 { VarDat[1] } := FStack.Top;
        r1 { VarDat[0] } := FStack.Data[FStack.Size - 2]; //r1 { VarDat[0] } := FStack[Stack.Size - 2];

        if (r1.AType in [btU8..btObject]) and (r2.AType in [btU8..btObject]) then
        begin
          case PSE2OpOP_COMPARE(OpCode).CompType of
          1 : CompareInt := Ord(uSE2RunOperation.mTable_Equal[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          2 : CompareInt := Ord(uSE2RunOperation.mTable_Smaller[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          3 : CompareInt := Ord(uSE2RunOperation.mTable_Bigger[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          4 : CompareInt := Ord(uSE2RunOperation.mTable_BiggerEqual[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          5 : CompareInt := Ord(uSE2RunOperation.mTable_SmallerEqual[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          6 : CompareInt := Ord(uSE2RunOperation.mTable_UnEqual[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          end;
        end else
        begin
          case PSE2OpOP_COMPARE(OpCode).CompType of
          1 : CompareInt := Ord(FVarCompare.Equal(r1 { VarDat[0] }, r2 { VarDat[1] }));
          2 : CompareInt := Ord(FVarCompare.Smaller(r1 { VarDat[0] }, r2 { VarDat[1] }));
          3 : CompareInt := Ord(FVarCompare.Bigger(r1 { VarDat[0] }, r2 { VarDat[1] }));
          4 : CompareInt := Ord(FVarCompare.BiggerEqual(r1 { VarDat[0] }, r2 { VarDat[1] }));
          5 : CompareInt := Ord(FVarCompare.SmallerEqual(r1 { VarDat[0] }, r2 { VarDat[1] }));
          6 : CompareInt := Ord(FVarCompare.UnEqual(r1 { VarDat[0] }, r2 { VarDat[1] }));
          end;
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
          r2 := FStack[CompareInt]
        else
          r2 := FStack[FStack.Size + CompareInt];

        { 2nd Parameter }
        CompareInt := PSE2OpOP_FASTCOMPARE(OpCode).Src2;
        if CompareInt >= 0 then // Static
          r1 := FStack[CompareInt]
        else
          r1 := FStack[FStack.Size - 1 + CompareInt];

        if (r1.AType in [btU8..btObject]) and (r2.AType in [btU8..btObject]) then
        begin
          case PSE2OpOP_FASTCOMPARE(OpCode).CompType of
          1 : CompareInt := Ord(uSE2RunOperation.mTable_Equal[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          2 : CompareInt := Ord(uSE2RunOperation.mTable_Smaller[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          3 : CompareInt := Ord(uSE2RunOperation.mTable_Bigger[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          4 : CompareInt := Ord(uSE2RunOperation.mTable_BiggerEqual[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          5 : CompareInt := Ord(uSE2RunOperation.mTable_SmallerEqual[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          6 : CompareInt := Ord(uSE2RunOperation.mTable_UnEqual[r1 { VarDat[0] }.AType, r2 { VarDat[1] }.AType](r1 { VarDat[0] }, r2 { VarDat[1] }, FVarHelper));
          end;
        end else
        begin     
          case PSE2OpOP_COMPARE(OpCode).CompType of
          1 : CompareInt := Ord(FVarCompare.Equal(r1 { VarDat[0] }, r2 { VarDat[1] }));
          2 : CompareInt := Ord(FVarCompare.Smaller(r1 { VarDat[0] }, r2 { VarDat[1] }));
          3 : CompareInt := Ord(FVarCompare.Bigger(r1 { VarDat[0] }, r2 { VarDat[1] }));
          4 : CompareInt := Ord(FVarCompare.BiggerEqual(r1 { VarDat[0] }, r2 { VarDat[1] }));
          5 : CompareInt := Ord(FVarCompare.SmallerEqual(r1 { VarDat[0] }, r2 { VarDat[1] }));
          6 : CompareInt := Ord(FVarCompare.UnEqual(r1 { VarDat[0] }, r2 { VarDat[1] }));
          end;
        end;

        FStack.PushNew(btBoolean).tu8^ := CompareInt;
      end;
  soDAT_COPY_TO :
      begin
        CompareInt := PSE2OpDAT_COPY_TO(OpCode).Target;

        if PSE2OpDAT_COPY_TO(OpCode).Static then
           r1 { VarDat[0] } := FStack.Data[CompareInt]
        else
           r1 { VarDat[0] } := FStack.Data[FStack.Size-1 + CompareInt];

        if r1 { VarDat[0] }^.RefContent then
        begin
          r2 { VarDat[1] } := FVarHelper.CreateVarData(r1 { VarDat[0] }^.AType);
          FVarHelper.SetVarData(r1 { VarDat[0] }, r2 { VarDat[1] });

          FVarHelper.FreeVarData(r1 { VarDat[0] });
          r1 { VarDat[0] } := r2 { VarDat[1] };
          if PSE2OpDAT_COPY_TO(OpCode).Static then
             FStack[CompareInt] := r2 { VarDat[1] }
          else
             FStack[FStack.Size-1 + CompareInt] := r2 { VarDat[1] };
        end;

        FVarHelper.SetVarData(FStack.Top, r1 { VarDat[0] });
        FStack.Pop;
      end;
  soDAT_COPY_FROM :
      begin
        CompareInt := PSE2OpDAT_COPY_FROM(OpCode).Source;

        if PSE2OpDAT_COPY_FROM(OpCode).Static then
        begin
          r1 { VarDat[0] } := FStack.Data[CompareInt];
          r2 { VarDat[1] } := FStack.PushNew(r1 { VarDat[0] }.AType);
        end else
        begin
          r1 { VarDat[0] } := FStack.Data[FStack.Size-1 + CompareInt];
          r2 { VarDat[1] } := FStack.PushNew(r1 { VarDat[0] }.AType);
        end;
        FVarHelper.SetVarData(r1 { VarDat[0] }, r2 { VarDat[1] });
        //r2 { VarDat[1] }^.RefContent := r1 { VarDat[0] }^.RefContent;
      end;
  soDAT_MOVE_TO :
      begin
        CompareInt := PSE2OpDAT_MOVE_TO(OpCode).Target;
        if PSE2OpDAT_MOVE_TO(OpCode).Static then
        begin
          FStack[CompareInt] := FStack.Top;
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
          FStack.Push(FStack[CompareInt]);
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
          r3 { VarDat[2] } := FVarHelper.CreateVarData(r1 { VarDat[0] }^.AType);
          FVarHelper.SetVarData(r1 { VarDat[0] }, r3 { VarDat[2] });
          FVarHelper.FreeVarData(r1 { VarDat[0] });
          r1 { VarDat[0] } := r3 { VarDat[2] };

          FStack[FStack.Size - 1 + PSE2OpDAT_CONVERT(OpCode).Index] := r3 { VarDat[2] };
        end;

        FVarHelper.ConvertContent(r1 { VarDat[0] }, PSE2OpDAT_CONVERT(OpCode).NewType);
      end;
  soDAT_CHANGETYPE :
      begin                   
        // Only for a break point
        //FStack.MaxSize := FStack.MaxSize;
      end;
  soDAT_SetInt :
      begin
        if not (FStack.Top.AType in [btU8, btS8, btU16, btS16, btU32, btS32, btS64]) then
        begin
          FVarHelper.FreeVarContent(FStack.Top);
          FStack.Top.AType := btS64;
          FVarHelper.CreateVarContent(FStack.Top);
        end;
        FVarHelper.SetIntContent(FStack.Top, PSE2OpDAT_SetInt(OpCode).Value);
      end;
  soDAT_SetFloat :
      begin
        r1 { VarDat[0] } := FStack.Top;
        if not (r1 { VarDat[0] }.AType in [btSingle, btDouble]) then
        begin
          FVarHelper.FreeVarContent(FStack.Top);
          FStack.Top.AType := btDouble;
          FVarHelper.CreateVarContent(FStack.Top);
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
          FVarHelper.FreeVarContent(FStack.Top);
          FStack.Top.AType := btPointer;
          FVarHelper.CreateVarContent(FStack.Top);
        end;
        Pointer(FStack.Top.tPointer^) := PSE2OpDAT_SetPtr(OpCode).Value;
      end;
  soDAT_LOADRES :
      begin
        if not (FStack.Top.AType in [btString, btUTF8String, btWideString, btPChar]) then
        begin
          FVarHelper.FreeVarContent(FStack.Top);
          FStack.Top.AType := btString;
          FVarHelper.CreateVarContent(FStack.Top);
        end;
        FVarHelper.SetStringContent(FStack.Top, FAppCode.Strings[PSE2OpDAT_LOADRES(OpCode).Index]);
      end;
  soDAT_CLEAR :
      begin
        FVarHelper.ClearVarContent(Stack.Top);
      end;         (*
  soDAT_PTR_LOAD :
      begin
        CompareInt := PSE2OpDAT_PTR_LOAD(OpCode).Position;

        r3 { VarDat[2] } := FStack.Top;
        if not (FStack.Top.AType in [btU8, btS8, btU16, btS16, btU32, btS32]) then
           raise ESE2RunTimeError.Create('Internal error: array offset is not ordinal');

        if PSE2OpDAT_PTR_LOAD(OpCode).Static then
          r1 { VarDat[0] } := FStack[CompareInt]
        else
          r1 { VarDat[0] } := FStack[FStack.Size-1 + CompareInt - 1];

        case r3 { VarDat[2] }.AType of
        btU8  : CompareInt := r2 { VarDat[1] }^.tu8^;
        btS8  : CompareInt := r2 { VarDat[1] }^.ts8^;
        btU16 : CompareInt := r2 { VarDat[1] }^.tu16^;
        btS16 : CompareInt := r2 { VarDat[1] }^.ts16^;
        btU32 : CompareInt := r2 { VarDat[1] }^.tu32^;
        btS32 : CompareInt := r2 { VarDat[1] }^.ts32^;
        end;
        FStack.Pop;
        r3 { VarDat[2] } := nil;

        r2 { VarDat[1] } := FStack.PushNew(btPointer);
        FVarHelper.FreeVarContent(r2 { VarDat[1] });
        r2 { VarDat[1] }.RefContent := True;
        r2 { VarDat[1] }.tPointer   := Pointer(integer(r1 { VarDat[0] }.tPointer^) + CompareInt);
      end;
  soDAT_PTR_SAVE :
      begin
        CompareInt := PSE2OpDAT_PTR_LOAD(OpCode).Position;

        r3 { VarDat[2] } := FStack.Top;
        if not (FStack.Top.AType in [btU8, btS8, btU16, btS16, btU32, btS32]) then
           raise ESE2RunTimeError.Create('Internal error: array offset is not ordinal');

        if PSE2OpDAT_PTR_LOAD(OpCode).Static then
          r1 { VarDat[0] } := FStack[CompareInt]
        else
          r1 { VarDat[0] } := FStack[FStack.Size-1 + CompareInt - 1];

        case r3 { VarDat[2] }.AType of
        btU8  : CompareInt := r2 { VarDat[1] }^.tu8^;
        btS8  : CompareInt := r2 { VarDat[1] }^.ts8^;
        btU16 : CompareInt := r2 { VarDat[1] }^.tu16^;
        btS16 : CompareInt := r2 { VarDat[1] }^.ts16^;
        btU32 : CompareInt := r2 { VarDat[1] }^.tu32^;
        btS32 : CompareInt := r2 { VarDat[1] }^.ts32^;
        end;
        FStack.Pop;
        r3 { VarDat[2] } := nil;

        FVarHelper.WriteContent(FStack.Top, Pointer(integer(r1 { VarDat[0] }.tPointer^) + CompareInt));
        FStack.Pop;
      end;
  soDAT_PTR_CREATE :
      begin
        if not (FStack.Top.AType in [btObject, btArray, btRecord]) then
           raise ESE2RunTimeError.Create('Internal error: data type not compatible');

        FStack.Top.tPointer := FMemoryManager.GetMem( PSE2OpDAT_PTR_CREATE(OpCode).NewSize);
      end;
  soDAT_PTR_FREE :
      begin
        FStack.Pop;
        if not (FStack.Top.AType in [btObject, btArray, btRecord]) then
           raise ESE2RunTimeError.Create('Internal error: data type not compatible');

        FMemoryManager.FreeMem(FStack.Top.tPointer);
        FStack.Top.tPointer := nil;
      end;     *)
  soSPEC_INCP :
      begin
        r1 { VarDat[0] } := Pointer(FStack.Top.tPointer^);
        r1 { VarDat[0] } := Pointer(integer(r1 { VarDat[0] }) + PSE2OpSPEC_INCP(OpCode).Offset);
        Stack.Pop;

        r2 { VarDat[1] } := FStack.PushNew($FF);
        //r2 { VarDat[1] } := FVarHelper.CreateVarData($FF);
        //Stack.Push(r2 { VarDat[1] });
        r2 { VarDat[1] }.AType := PSE2OpSPEC_INCP(OpCode).newType;
        r2 { VarDat[1] }.RefContent := True;
        Pointer(r2 { VarDat[1] }.tPointer)   := Pointer(r1 { VarDat[0] });
      end;
  soSPEC_DECP   :
      begin
        CompareInt := PSE2OpSPEC_DECP(OpCode).Target;
        r1 { VarDat[0] } := FStack[FStack.Size-1 + CompareInt];
        FVarHelper.SetVarData(FStack.Top, r1 { VarDat[0] });
        FStack.Pop;
      end;
  soSPEC_UNREF  :
      begin
        r1 { VarDat[0] } := Stack.Top;
        if r1 { VarDat[0] }^.RefContent then
        begin
          r2 { VarDat[1] } := FVarHelper.CreateVarData(r1 { VarDat[0] }^.AType);
          FVarHelper.SetVarData(r1 { VarDat[0] }, r2 { VarDat[1] });
          FStack.Pop;
          FStack.Push(r2 { VarDat[1] });
        end;
      end;
  soSPEC_GetRef :
      begin
        CompareInt := PSE2OpSPEC_GetRef(OpCode).Offset;

        if PSE2OpSPEC_GetRef(OpCode).Static then
        begin
          r1 { VarDat[0] } := FStack[CompareInt].tPointer;
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
        Meta := FAppCode.MetaData[PSE2OpSPEC_GetProcPtr(OpCode).MetaIndex];
        if Meta = nil then
           raise ESE2RunTimeError.Create('Method pointer could not be get');



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
        end;
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

        Pointer(r1 { VarDat[0] }.tPointer^) := CreateClass(FAppCode.MetaData[PSE2OpSPEC_CREATE(OpCode).MetaIndex]);
        Pointer(r2 { VarDat[1] }.tPointer^) := Pointer(r1 { VarDat[0] }.tPointer^);
        FClassGC.Add(Pointer(r1 { VarDat[0] }.tPointer^));
      end;
  soSPEC_DESTROY :
      begin
        r1 { VarDat[0] } := FStack.Top;

        FClassGC.Delete(Pointer(r1 { VarDat[0] }.tPointer^));
        FNativeList.ClearForClass(Pointer(r1 { VarDat[0] }.tPointer^));
        FreeClass(Pointer(r1 { VarDat[0] }.tPointer^));
      end;
  soREC_MAKE :
      begin
        // Self - Pointer
        r1 { VarDat[0] } := FStack[FStack.Size - PSE2OpREC_MAKE(OpCode).Variables - 1];

        if PSE2OpREC_MAKE(OpCode).MetaIndex = -1 then
           raise ESE2RunTimeError.Create('Runtime error: Meta index not assigned');

        if (not (r1 { VarDat[0] }.AType in [btRecord])) then
           raise ESE2RunTimeError.Create('Runtime error: stack corrupt');

        Pointer(r1 { VarDat[0] }.tPointer^) := FRunClasses.CreateScriptRecord(FAppCode.MetaData[PSE2OpREC_MAKE(OpCode).MetaIndex], FAppCode);
      end;
  soREC_FREE :
      begin
        r1 { VarDat[0] } := FStack.Items[FStack.Size - 1 + PSE2OpREC_FREE(OpCode).Offset];

        FRecordGC.Delete(Pointer(r1 { VarDat[0] }.tPointer^));
        FRunClasses.DestroyScriptRecord(Pointer(r1 { VarDat[0] }.tPointer^));
      end;
  soREC_COPY_TO :
      begin
        CompareInt := PSE2OpREC_COPY_TO(OpCode).Target;

        if PSE2OpREC_COPY_TO(OpCode).Static then
           r1 { VarDat[0] } := FStack[CompareInt]
        else
           r1 { VarDat[0] } := FStack[FStack.Size-1 + CompareInt];

        if (r1 { VarDat[0] }.AType <> btRecord) or
           (FStack.Top.AType <> btRecord) then
           raise ESE2RunTimeError.Create('Record copy not possible');

        FRunClasses.CopyScriptRecord(PPointer(FStack.Top.tPointer)^, PPointer(r1 { VarDat[0] }.tPointer)^);
      end;
  soREC_MARK_DEL  :
      begin
        r1 { VarDat[0] } := FStack.Top;
        if r1 { VarDat[0] }.AType = btRecord then
           FRecordGC.Add(PPointer(r1 { VarDat[0] }.tPointer)^, Stack.Size - 1);
      end;
  soINT_INCSTATIC :
      begin
        r1 { VarDat[0] } := FStack[PSE2OpINT_INCSTATIC(OpCode).Offset];
        r2 { VarDat[1] } := FStack.Pool.Pop(btU32);
        r2 { VarDat[1] }^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Addition(r1 { VarDat[0] }, r2 { VarDat[1] });
        FVarHelper.FreeVarData(r2 { VarDat[1] });
      end;
  soINT_DECSTATIC :
      begin
        r1 { VarDat[0] } := FStack[PSE2OpINT_INCSTATIC(OpCode).Offset];
        r2 { VarDat[1] } := FStack.Pool.Pop(btU32);
        r2 { VarDat[1] }^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Substract(r1 { VarDat[0] }, r2 { VarDat[1] });   
        FVarHelper.FreeVarData(r2 { VarDat[1] });
      end;
  soINT_INCSTACK :
      begin
        r1 { VarDat[0] } := FStack[FStack.Size - 1 + PSE2OpINT_INCSTATIC(OpCode).Offset];
        r2 { VarDat[1] } := FStack.Pool.Pop(btU32);
        r2 { VarDat[1] }^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Addition(r1 { VarDat[0] }, r2 { VarDat[1] });   
        FVarHelper.FreeVarData(r2 { VarDat[1] });
      end;
  soINT_DECSTACK :
      begin
        r1 { VarDat[0] } := FStack[FStack.Size - 1 + PSE2OpINT_INCSTATIC(OpCode).Offset];
        r2 { VarDat[1] } := FStack.Pool.Pop(btU32);
        r2 { VarDat[1] }^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Substract(r1 { VarDat[0] }, r2 { VarDat[1] }); 
        FVarHelper.FreeVarData(r2 { VarDat[1] });
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

  {$IFDEF PERF_MONITOR}
  FPerfMonitor.Stop(OpCode.OpCode);
  {$ENDIF}

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
          DoErrorEvent(ExceptClass(e.ClassType), e.Message, FCodePos, GetCallStack);
          // no Exception Manager
          FCodePos := -1;
        end else
        begin
          FHasException := True;
          FCodePos      := TryBlock.BlockStart;
          TryBlock.ErrorExcept := ExceptClass(E.ClassType);
          TryBlock.ErrorMsg    := E.Message;
          TryBlock.ErrorPos    := FCodePos;
          TryBlock.ErrorStack  := GetCallStack;
        end;
      end;  
    end;
  end;
end;

function TSE2RunTime.GetCallStack: string;
var i     : integer;
    sl    : TStringList;
    index : integer;
begin
  result := '';

  sl := TStringList.Create;
  try
    for i:=FStack.Size-1 downto 0 do
    begin         
      if FStack[i]^.AType = btReturnAddress then
      begin
        index := (FStack[i]^.tS64^ shr 32);
        if index > 0 then
           sl.Add(MetaEntryToStr(FAppCode.MetaData[index-1], i));
      end;
    end;
    result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TSE2RunTime.GetCallStack(Target: TSE2StackTrace);
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
        MetaEntryToItem(FAppCode.MetaData[index - 1], i, Item);
      end;
    end;
  end;
end;

function TSE2RunTime.MetaEntryToStr(Entry: TSE2MetaEntry;
  StackIndex: integer): string;
var i: integer;
    p: PSE2VarData;
begin
  if Entry = nil then
  begin
    result := 'DEBUG ERROR: Meta Data is null';
    exit;
  end;

  if Entry.HasResult then
     result := 'function '
  else
     result := 'procedure ';

  result := result + Entry.AUnitName + '.' + Entry.Name;
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

procedure TSE2RunTime.MetaEntryToItem(Entry: TSE2MetaEntry;
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

procedure TSE2RunTime.DoErrorEvent(Exp: ExceptClass; const Msg: string; ErrorPos: integer;
  const CallStack: string);
begin
  if not (Exp = EAbort) then
    if Assigned(FOnError) then
       FOnError(Self, Exp, Msg, ErrorPos, CallStack);
end;

procedure TSE2RunTime.Abort;
begin
  FDoAbort := True;
end;

function TSE2RunTime.Call(Method: Pointer;
  Params: array of const): variant;
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
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtAnsiString :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtPChar:
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtChar :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btString     : PbtString(newEntry.tString^)^     := string(PPointer(Data)^);
          btWideString : PbtWideString(newEntry.tString^)^ := {$IFDEF DELPHI2009UP}UTF8ToWideString{$ELSE}UTF8Decode{$ENDIF}(AnsiToUtf8(string(PPointer(Data)^)));
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := AnsiToUtf8(string(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(string(PPointer(Data)^));
          else raise ESE2CallParameterError.Create(SParamNotCompatible);
          end;
        end;
    vtWideString :
        begin
          newEntry := FStack.PushNew(aParamType);
          case aParamType of
          btString     : PbtString(newEntry.tString^)^     := Utf8ToAnsi(UTF8Encode(WideString(PPointer(Data)^)));
          btWideString : PbtWideString(newEntry.tString^)^ := WideString(PPointer(Data)^);
          btUTF8String : PbtUTF8String(newEntry.tString^)^ := UTF8Encode(WideString(PPointer(Data)^));
          btPChar      : PbtPChar(newEntry.tString^)^      := PChar(Utf8ToAnsi(UTF8Encode(WideString(PPointer(Data)^))));
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

               RecMeta := FAppCode.MetaData[index];
               if RecMeta = nil then
                  raise ESE2CallParameterError.Create(SParamNotCompatible);

               Pointer(newEntry.tPointer^) := FRunClasses.CreateScriptRecord(RecMeta, FAppCode);
               FRunClasses.DelphiToScriptRecord(PPointer(Data)^, Pointer(newEntry.tPointer^));
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

      RecMeta := FAppCode.MetaData[i];
      if RecMeta = nil then
         raise ESE2CallParameterError.Create(SParamNotCompatible);

      Pointer(FStack.Top.tPointer^) := FRunClasses.CreateScriptRecord(RecMeta, FAppCode);
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
        btS64 :
            SetVariableContent(aParamType, vtInt64, Parameter.VPointer);
        btSingle, btDouble :
            SetVariableContent(aParamType, btExtended, Parameter.VPointer);
        btString :
            SetVariableContent(aParamType, vtAnsiString, Parameter.VPointer);
        btUTF8String :
            SetVariableContent(aParamType, vtAnsiString, Parameter.VPointer);
        btWideString :
            SetVariableContent(aParamType, vtWideString, Parameter.VPointer);
        btPChar :
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
    FStack.PushNew(btReturnAddress)^.ts64^ := int64($FFFFFFFF00000000);
  end;

  procedure PopParamsFromStack;
  var i           : integer;
      bIsVarParam : boolean;
      Parameter   : TVarRec;
      Data        : PSE2VarData;
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
        btSingle       : PbtSingle(Parameter.VPointer)^ := Data^.tSingle^;
        btDouble       : PbtDouble(Parameter.VPointer)^ := Data^.tDouble^;
        btString       : PbtString(Parameter.VPointer)^ := PbtString(Data^.tString^)^;
        btUTF8String   : PbtUTF8String(Parameter.VPointer)^ := PbtUTF8String(Data^.tString^)^;
        btWideString   : PbtWideString(Parameter.VPointer)^ := PbtWideString(Data^.tString^)^;
        btPChar        : PbtPChar(Parameter.VPointer)^      := PbtPChar(Data^.tString^)^;
        btPointer,
        btArray        : PPointer(Parameter.VPointer)^    := Pointer(Data^.tPointer^);
        btRecord       :
            begin
              Self.FRunClasses.ScriptToDelphiRecord(PPointer(Data^.tPointer)^, Parameter.VPointer);
            end;
        end;
      end;

      if Data^.AType = btRecord then
        if Pointer(Data^.tPointer^) <> nil then
          FRunClasses.DestroyScriptRecord(Pointer(Data^.tPointer^));


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
      btSingle     : result := FStack.Top^.tSingle^;
      btDouble     : result := FStack.Top^.tDouble^;
      btString     : result := PbtString(FStack.Top^.tString^)^;
      btUTF8String : result := PbtUTF8String(FStack.Top^.tString^)^;
      btWideString : result := PbtWideString(FStack.Top^.tString^)^;
      btPChar      : result := string(PbtPChar(FStack.Top^.tString^)^);
      btArray,
      btPointer,
      btObject     : result := cardinal(FStack.Top^.tPointer^);
      btRecord     :
          begin
            result := cardinal(FRunClasses.ScriptToDelphiRecord(Pointer(FStack.Top^.tPointer^)));
            FRunClasses.DestroyScriptRecord(Pointer(FStack.Top^.tPointer^));
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

  if not FInitialized then
     Initialize;

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
      Methods  := FRunClasses.GetClassMethods(PPointer(SelfPtr.tPointer)^);
      Index    := Methods[MetaEntry.DynIndex];
      if Index > 0 then
      begin
        FCodePos := index;
        Process;
      end;
    end else
    begin
      FCodePos   := MetaEntry.CodePos;
      Process;
    end;
  finally
    while FStack.Size > OldStackSize do
      Stack.Pop;
    
    PopParamsFromStack;
    FCodePos := OldCodePos;
  end;
end;

function TSE2RunTime.CreateClass(const Meta: TSE2MetaEntry): Pointer;
begin
  result := FRunClasses.CreateScriptClassObject(Meta, FAppCode);
end;

procedure TSE2RunTime.FreeClass(AClass: Pointer);
begin
  FRunClasses.DestroyScriptClassObject(AClass);
end;

function TSE2RunTime.ScriptAsMethod(Method,
  ClassInstance: Pointer): TMethod;
var CodeData : Pointer;
begin
  if FNativeList = nil then
     exit;

  if Method = nil then
     raise ESE2NullReferenceError.Create('Method pointer can not be nil');


  CodeData := FNativeList.GenEntry(Self, Method, ClassInstance);
  result.Data := CodeData;
  result.Code := @SE2MethodScriptCallHandler;
end;

function TSE2RunTime.MethodAsScript(Data, MethodPos,
  ClassPtr: Pointer): boolean;
begin
  result := FNativeList.FindEntry(Data, MethodPos, ClassPtr);
end;

end.
