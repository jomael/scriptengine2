unit uSE2RunTime;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, SysUtils, uSE2RunType, uSE2Consts, uSE2BaseTypes, uSE2OpCode, uSE2PEData, uSE2RunCall,
  uSE2RunOperation, uSE2RunAccess, uSE2SafeBlockMngr, uSE2DebugData, uSE2MemoryManager, uSE2PerfMonitor,
  uSE2NativeCallList;

type
  TSE2RunTimeError = procedure(Sender: TObject; Exp: ExceptClass; const Msg: string; CodePos: integer; const CallStack: string) of object;

  {.$DEFINE PERF_MONITOR}
  {$DEFINE Run_Inline}

  {$IFDEF FPC}
    {$UNDEF PERF_MONITOR}
  {$ENDIF}

  TSE2RunTime = class(TSE2Object)
  private
    FAppCode           : TSE2PE;
    FStack             : TSE2Stack;
    FCodeAccess        : TSE2RunAccess;
    FNativeList        : TSE2NativeCallList;

    {$IFDEF PERF_MONITOR}
    FPerfMonitor       : TSE2PerfMonitor;
    {$ENDIF}
    FMemoryManager     : TSE2MemoryManager;
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

    {$IFNDEF Run_Inline}
    procedure ProcessOperation;
    {$ENDIF}
    procedure DoErrorEvent(Exp: ExceptClass; const Msg: string; ErrorPos: integer; const CallStack: string);

    function  MetaEntryToStr(Entry: TSE2MetaEntry; StackIndex: integer): string;

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
    function  GetCallStack: string;

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
  FVarHelper     := TSE2VarHelper.Create(FMemoryManager, RecordDeleteEvent);
  FVarOperation  := TSE2VarOperation.Create(FVarHelper);
  FVarCompare    := TSE2VarCompare.Create(FVarHelper);
  FClassGC       := TSE2ClassGC.Create;
  FRecordGC      := TSE2RecordGC.Create;

  FStack         := TSE2Stack.Create(FVarHelper);
  FSafeBlocks    := TSE2SafeBlockMngr.Create;
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
                  
  FVarHelper.Free;
  FVarOperation.Free;
  FVarCompare.Free;
  FMemoryManager.Free;
  {$IFDEF PERF_MONITOR}
  FPerfMonitor.Free;
  {$ENDIF}
  inherited;
end;

procedure TSE2RunTime.ProcessRecordGC; 
var i: integer;
begin
  for i:=FRecordGC.Count-1 downto 0 do
    DestroyScriptRecord(PSE2RecordGCEntry(FRecordGC[i])^.Ptr);
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
    DestroyScriptRecord(p);
  end;
end;

procedure TSE2RunTime.Process;

{$IFDEF Run_Inline}
var OpCode        : PSE2OpDefault;
    CompareInt    : integer;
    Pos           : array[0..1] of integer;
    VarDat        : array[0..3] of PSE2VarData;
    Meta          : TSE2MetaEntry;
    TryBlock      : TSE2TryBlock;
    TempTryBlock  : TSE2TryBlock;
{$ELSE}
var TryBlock : TSE2TryBlock;
{$ENDIF}

begin
  FHasException := False;
  while (FCodePos > 0) and (not FDoAbort) do
  begin
    try
{$IFNDEF Run_Inline}
      ProcessOperation;
{$ELSE}

  OpCode := FAppCode.OpCodes[FCodePos];
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
  soSTACK_DEC :
      begin
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
        TSE2CallExternal.CallMethod(FStack, Pointer(PSE2OpFLOW_CALLEX(OpCode).Position),
                                    FAppCode.MetaData[PSE2OpFLOW_CALLEX(OpCode).MetaIndex], Self);
      end;
  soFLOW_CALLDYN :
      begin
        VarDat[0] := FStack.Top;//Items[FStack.Size - 2];

        if VarDat[0].AType <> btObject then
           raise ESE2RunTimeError.Create('Internal error: dynamic method call outside of a class');

        if Pointer(VarDat[0].tPointer^) = nil then
           raise ESE2NullReferenceError.Create('Called class is not assigned');

        CompareInt := GetClassMethods(PPointer(VarDat[0]^.tPointer)^).Items[PSE2OpFLOW_CALLDYN(OpCode)^.Offset];

        if CompareInt < 1 then
           raise ESE2RunTimeError.Create('Abstract method was not implemented');

        FCodePos := CompareInt - 1;
        Stack.Pop;
      end;
  soFLOW_CALLPTR :
      begin
        VarDat[0] := FStack.Top;
        if not (VarDat[0].AType in [btPointer, btProcPtr]) then
           raise ESE2RunTimeError.Create('Internal error: stack corrupt');

        if Pointer(VarDat[0].tPointer^) = nil then
           raise ESE2NullReferenceError.Create('Method call not assigned');

        Meta := PPointer(VarDat[0].tPointer)^;
        FCodePos := Meta.CodePos - 1;
        if Meta.HasSelf then
        begin
          VarDat[2] := FStack.Items[FStack.Size - Meta.ParamCount - 2];

          VarDat[1] := FVarHelper.CreateVarData(btProcPtr);
          FVarHelper.CreateVarContent(VarDat[1]);
          PPointer(VarDat[1]^.tPointer)^ := PPointer(Integer(VarDat[2]^.tPointer) + SizeOf(Pointer))^;

          FStack.Items[FStack.Size - Meta.ParamCount - 2] := VarDat[1];
          FStack.Pool.Push(VarDat[2]);

          if Meta.DynIndex > -1 then
          begin
            FCodePos := GetClassMethods(PPointer(VarDat[1]^.tPointer)^).Items[Meta.DynIndex];
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
              VarDat[0] := Stack.Top;

              if VarDat[0]^.RefContent then
              begin
                VarDat[2] := FVarHelper.CreateVarData(varDat[0]^.AType);
                FVarHelper.SetVarData(VarDat[0], VarDat[2]);
                FVarHelper.FreeVarData(VarDat[0]);
                VarDat[0] := VarDat[2];

                FStack[FStack.Size - 1] := VarDat[2];
              end;

              case PSE2OpOP_OPERATION(OpCode).OpType of
              1  : FVarOperation.Negation(FStack.Top); // negation
              20 : FVarOperation.BitNot(FStack.Top);
              21 : FVarOperation.BooleanNot(FStack.Top);
              end;
            end;
        2..11   :
            begin
              VarDat[1] := Stack.Top;
              VarDat[0] := Stack[Stack.Size - 2];

              if VarDat[0]^.RefContent then
              begin
                VarDat[2] := FVarHelper.CreateVarData(varDat[0]^.AType);
                FVarHelper.SetVarData(VarDat[0], VarDat[2]);
                FVarHelper.FreeVarData(VarDat[0]);
                VarDat[0] := VarDat[2];

                Stack[Stack.Size - 2] := VarDat[2];
              end;

              case PSE2OpOP_OPERATION(OpCode).OpType of
              2  : FVarOperation.Addition(VarDat[0], VarDat[1]);
              3  : FVarOperation.Substract(VarDat[0], VarDat[1]);
              4  : FVarOperation.Multiply(VarDat[0], VarDat[1]);
              5  : FVarOperation.Divide(VarDat[0], VarDat[1]);
              6  : FVarOperation.BitAnd(VarDat[0], VarDat[1]);
              7  : FVarOperation.BitOr(VarDat[0], VarDat[1]);
              8  : FVarOperation.BitXor(VarDat[0], VarDat[1]);
              9  : FVarOperation.DivideMod(VarDat[0], VarDat[1]);
              10 : FVarOperation.BitShr(VarDat[0], VarDat[1]);
              11 : FVarOperation.BitShl(VarDat[0], VarDat[1]);
              end;
              // Aritmetic
              FStack.Pop;
            end;
        end;
      end;
  soOP_COMPARE :
      begin
        //QueryPerformanceFrequency(c);
        //QueryPerformanceCounter(t1);
        VarDat[1] := FStack.Top;
        VarDat[0] := FStack[Stack.Size - 2];
        case PSE2OpOP_COMPARE(OpCode).CompType of
        1 : CompareInt := Ord(FVarCompare.Equal(VarDat[0], VarDat[1]));
        2 : CompareInt := Ord(FVarCompare.Smaller(VarDat[0], VarDat[1]));
        3 : CompareInt := Ord(FVarCompare.Bigger(VarDat[0], VarDat[1])); 
        4 : CompareInt := Ord(FVarCompare.BiggerEqual(VarDat[0], VarDat[1]));
        5 : CompareInt := Ord(FVarCompare.SmallerEqual(VarDat[0], VarDat[1]));
        6 : CompareInt := Ord(FVarCompare.UnEqual(VarDat[0], VarDat[1]));
        end;
        FStack.Pop;
        FStack.Pop;
        FStack.PushNew(btBoolean).tu8^ := CompareInt;
        //QueryPerformanceCounter(t2);
        //FRunTime := FRunTime + ( t2 - t1) / c;
      end;
  soDAT_COPY_TO :
      begin
        CompareInt := PSE2OpDAT_COPY_TO(OpCode).Target;

        if PSE2OpDAT_COPY_TO(OpCode).Static then
           VarDat[0] := FStack[CompareInt]
        else
           VarDat[0] := FStack[FStack.Size-1 + CompareInt];

        if VarDat[0]^.RefContent then
        begin
          VarDat[1] := FVarHelper.CreateVarData(varDat[0]^.AType);
          FVarHelper.SetVarData(VarDat[0], VarDat[1]);
                                              
          FVarHelper.FreeVarData(VarDat[0]);
          VarDat[0] := VarDat[1];
          if PSE2OpDAT_COPY_TO(OpCode).Static then
             FStack[CompareInt] := VarDat[1]
          else
             FStack[FStack.Size-1 + CompareInt] := VarDat[1];
        end;

        FVarHelper.SetVarData(FStack.Top, VarDat[0]);
        FStack.Pop;
      end;
  soDAT_COPY_FROM :
      begin
        CompareInt := PSE2OpDAT_COPY_FROM(OpCode).Source;

        if PSE2OpDAT_COPY_FROM(OpCode).Static then
        begin
          VarDat[0] := FStack[CompareInt];
          VarDat[1] := FStack.PushNew(VarDat[0].AType);
        end else
        begin
          VarDat[0] := FStack[FStack.Size-1 + CompareInt];
          VarDat[1] := FStack.PushNew(VarDat[0].AType);
        end;
        FVarHelper.SetVarData(VarDat[0], VarDat[1]);
        //VarDat[1]^.RefContent := VarDat[0]^.RefContent;
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
        VarDat[0] := FStack[FStack.Size - 1 + PSE2OpDAT_CONVERT(OpCode).Index];

        if VarDat[0]^.RefContent then
        begin
          VarDat[2] := FVarHelper.CreateVarData(varDat[0]^.AType);
          FVarHelper.SetVarData(VarDat[0], VarDat[2]);
          FVarHelper.FreeVarData(VarDat[0]);
          VarDat[0] := VarDat[2];

          FStack[FStack.Size - 1 + PSE2OpDAT_CONVERT(OpCode).Index] := VarDat[2];
        end;

        FVarHelper.ConvertContent(VarDat[0], PSE2OpDAT_CONVERT(OpCode).NewType);
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
        if FStack.Top.AType <> btDouble then
        begin
          FVarHelper.FreeVarContent(FStack.Top);
          FStack.Top.AType := btDouble;
          FVarHelper.CreateVarContent(FStack.Top);
        end;
        FStack.Top.tDouble^ := PSE2OpDAT_SetFloat(OpCode).Value;
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
      end;         {
  soDAT_PTR_LOAD :
      begin
        CompareInt := PSE2OpDAT_PTR_LOAD(OpCode).Position;

        VarDat[2] := FStack.Top;
        if not (FStack.Top.AType in [btU8, btS8, btU16, btS16, btU32, btS32]) then
           raise ESE2RunTimeError.Create('Internal error: array offset is not ordinal');

        if PSE2OpDAT_PTR_LOAD(OpCode).Static then
          VarDat[0] := FStack[CompareInt]
        else
          VarDat[0] := FStack[FStack.Size-1 + CompareInt - 1];

        case VarDat[2].AType of
        btU8  : CompareInt := VarDat[1]^.tu8^;
        btS8  : CompareInt := VarDat[1]^.ts8^;
        btU16 : CompareInt := VarDat[1]^.tu16^;
        btS16 : CompareInt := VarDat[1]^.ts16^;
        btU32 : CompareInt := VarDat[1]^.tu32^;
        btS32 : CompareInt := VarDat[1]^.ts32^;
        end;
        FStack.Pop;
        VarDat[2] := nil;

        VarDat[1] := FStack.PushNew(btPointer);
        FVarHelper.FreeVarContent(VarDat[1]);
        VarDat[1].RefContent := True;
        VarDat[1].tPointer   := Pointer(integer(VarDat[0].tPointer^) + CompareInt);
      end;
  soDAT_PTR_SAVE :
      begin
        CompareInt := PSE2OpDAT_PTR_LOAD(OpCode).Position;

        VarDat[2] := FStack.Top;
        if not (FStack.Top.AType in [btU8, btS8, btU16, btS16, btU32, btS32]) then
           raise ESE2RunTimeError.Create('Internal error: array offset is not ordinal');

        if PSE2OpDAT_PTR_LOAD(OpCode).Static then
          VarDat[0] := FStack[CompareInt]
        else
          VarDat[0] := FStack[FStack.Size-1 + CompareInt - 1];

        case VarDat[2].AType of
        btU8  : CompareInt := VarDat[1]^.tu8^;
        btS8  : CompareInt := VarDat[1]^.ts8^;
        btU16 : CompareInt := VarDat[1]^.tu16^;
        btS16 : CompareInt := VarDat[1]^.ts16^;
        btU32 : CompareInt := VarDat[1]^.tu32^;
        btS32 : CompareInt := VarDat[1]^.ts32^;
        end;
        FStack.Pop;
        VarDat[2] := nil;

        FVarHelper.WriteContent(FStack.Top, Pointer(integer(VarDat[0].tPointer^) + CompareInt));
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
      end;     }
  soSPEC_INCP :
      begin
        VarDat[0] := Pointer(FStack.Top.tPointer^);
        VarDat[0] := Pointer(integer(VarDat[0]) + PSE2OpSPEC_INCP(OpCode).Offset);
        Stack.Pop;

        VarDat[1] := FStack.PushNew($FF);
        //VarDat[1] := FVarHelper.CreateVarData($FF);
        //Stack.Push(VarDat[1]);
        VarDat[1].AType := PSE2OpSPEC_INCP(OpCode).newType;
        VarDat[1].RefContent := True;
        Pointer(VarDat[1].tPointer)   := Pointer(VarDat[0]);
      end;
  soSPEC_DECP   :
      begin
        CompareInt := PSE2OpSPEC_DECP(OpCode).Target;
        VarDat[0] := FStack[FStack.Size-1 + CompareInt];
        FVarHelper.SetVarData(FStack.Top, VarDat[0]);
        FStack.Pop;
      end;
  soSPEC_UNREF  :
      begin
        VarDat[0] := Stack.Top;
        if VarDat[0]^.RefContent then
        begin
          VarDat[1] := FVarHelper.CreateVarData(varDat[0]^.AType);
          FVarHelper.SetVarData(VarDat[0], VarDat[1]);
          FStack.Pop;
          FStack.Push(VarDat[1]);
        end;
      end;
  soSPEC_GetRef :
      begin
        CompareInt := PSE2OpSPEC_GetRef(OpCode).Offset;

        if PSE2OpSPEC_GetRef(OpCode).Static then
        begin
          VarDat[0] := FStack[CompareInt].tPointer;
        end else
        begin
          VarDat[0] := FStack[FStack.Size-1 + CompareInt].tPointer;
        end;

        if not PSE2OpSPEC_GetRef(OpCode).UsePush then   
           FStack.Pop;

        VarDat[1] := FStack.PushNew(btPointer);
        PPointer(VarDat[1].tPointer)^ := Pointer(VarDat[0]);
      end;
  soSPEC_GetProcPtr :
      begin
        Meta := FAppCode.MetaData[PSE2OpSPEC_GetProcPtr(OpCode).MetaIndex];
        if Meta = nil then
           raise ESE2RunTimeError.Create('Method pointer could not be get');



        VarDat[0] := FStack.PushNew(btProcPtr);
        PPointer(VarDat[0].tPointer)^ := Meta;
        if Meta.HasSelf then
        begin
          VarDat[1] := FStack.Items[FStack.Size - 2];
          PPointer(Integer(VarDat[0]^.tPointer) + SizeOf(Pointer))^ := PPointer(VarDat[1].tPointer)^;
          VarDat[2] := VarDat[1];
          FStack.Items[FStack.Size - 2] := VarDat[0];
          FStack.Pop;
          FStack.Pool.Push(VarDat[1]);
        end;
      end;
  soSPEC_CREATE :
      begin
        // Self - Pointer
        VarDat[0] := FStack[FStack.Size - PSE2OpSPEC_CREATE(OpCode).Variables - 2];
        VarDat[1] := FStack[FStack.Size - PSE2OpSPEC_CREATE(OpCode).Variables - 1];

        if PSE2OpSPEC_CREATE(OpCode).MetaIndex = -1 then
           raise ESE2RunTimeError.Create('Runtime error: Meta index not assigned');

        if (not (VarDat[0].AType in [btObject])) or
           (not (VarDat[1].AType in [btObject])) then
           raise ESE2RunTimeError.Create('Runtime error: stack corrupt');

        Pointer(VarDat[0].tPointer^) := CreateClass(FAppCode.MetaData[PSE2OpSPEC_CREATE(OpCode).MetaIndex]);
        Pointer(VarDat[1].tPointer^) := Pointer(VarDat[0].tPointer^);
        FClassGC.Add(Pointer(VarDat[0].tPointer^));
      end;
  soSPEC_DESTROY :
      begin
        VarDat[0] := FStack.Top;

        FClassGC.Delete(Pointer(VarDat[0].tPointer^));
        FNativeList.ClearForClass(Pointer(VarDat[0].tPointer^));
        FreeClass(Pointer(VarDat[0].tPointer^));
      end;
  soREC_MAKE :
      begin
        // Self - Pointer
        VarDat[0] := FStack[FStack.Size - PSE2OpREC_MAKE(OpCode).Variables - 1];

        if PSE2OpREC_MAKE(OpCode).MetaIndex = -1 then
           raise ESE2RunTimeError.Create('Runtime error: Meta index not assigned');

        if (not (VarDat[0].AType in [btRecord])) then
           raise ESE2RunTimeError.Create('Runtime error: stack corrupt');

        Pointer(VarDat[0].tPointer^) := uSE2SystemUnit.CreateScriptRecord(FAppCode.MetaData[PSE2OpREC_MAKE(OpCode).MetaIndex], FAppCode);
      end;
  soREC_FREE :
      begin
        VarDat[0] := FStack.Items[FStack.Size - 1 + PSE2OpREC_FREE(OpCode).Offset];

        FRecordGC.Delete(Pointer(VarDat[0].tPointer^));
        uSE2SystemUnit.DestroyScriptRecord(Pointer(VarDat[0].tPointer^));
      end;
  soREC_COPY_TO :
      begin
        CompareInt := PSE2OpREC_COPY_TO(OpCode).Target;

        if PSE2OpREC_COPY_TO(OpCode).Static then
           VarDat[0] := FStack[CompareInt]
        else
           VarDat[0] := FStack[FStack.Size-1 + CompareInt];

        if (VarDat[0].AType <> btRecord) or
           (FStack.Top.AType <> btRecord) then
           raise ESE2RunTimeError.Create('Record copy not possible');

        uSE2SystemUnit.CopyScriptRecord(PPointer(FStack.Top.tPointer)^, PPointer(VarDat[0].tPointer)^);
      end;
  soREC_MARK_DEL  :
      begin
        VarDat[0] := FStack.Top;
        if VarDat[0].AType = btRecord then
           FRecordGC.Add(PPointer(VarDat[0].tPointer)^, Stack.Size - 1);
      end;
  soINT_INCSTATIC :
      begin
        VarDat[0] := FStack[PSE2OpINT_INCSTATIC(OpCode).Offset];
        VarDat[1] := FStack.Pool.Pop(btU32);
        VarDat[1]^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Addition(VarDat[0], VarDat[1]);
        FVarHelper.FreeVarData(VarDat[1]);
      end;
  soINT_DECSTATIC :
      begin
        VarDat[0] := FStack[PSE2OpINT_INCSTATIC(OpCode).Offset];
        VarDat[1] := FStack.Pool.Pop(btU32);
        VarDat[1]^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Substract(VarDat[0], VarDat[1]);   
        FVarHelper.FreeVarData(VarDat[1]);
      end;
  soINT_INCSTACK :
      begin
        VarDat[0] := FStack[FStack.Size - 1 + PSE2OpINT_INCSTATIC(OpCode).Offset];
        VarDat[1] := FStack.Pool.Pop(btU32);
        VarDat[1]^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Addition(VarDat[0], VarDat[1]);   
        FVarHelper.FreeVarData(VarDat[1]);
      end;
  soINT_DECSTACK :
      begin
        VarDat[0] := FStack[FStack.Size - 1 + PSE2OpINT_INCSTATIC(OpCode).Offset];
        VarDat[1] := FStack.Pool.Pop(btU32);
        VarDat[1]^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Substract(VarDat[0], VarDat[1]); 
        FVarHelper.FreeVarData(VarDat[1]);
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


{$ENDIF}
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

{$IFNDEF Run_Inline}
procedure TSE2RunTime.ProcessOperation;
var OpCode        : PSE2OpDefault;
    CompareInt    : integer;
    Pos           : array[0..1] of integer;
    VarDat        : array[0..1] of PSE2VarData;
    TryBlock      : TSE2TryBlock;
    TempTryBlock  : TSE2TryBlock;
begin
  OpCode := FAppCode.OpCodes[FCodePos];
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
  soSTACK_DEC :
      begin
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
        TSE2CallExternal.CallMethod(FStack, Pointer(PSE2OpFLOW_CALLEX(OpCode).Position),
                                    FAppCode.MetaData[PSE2OpFLOW_CALLEX(OpCode).MetaIndex]);
      end;
  soFLOW_RET :
      begin
        if FStack.Top.AType <> btReturnAddress then
           raise ESE2RunTimeError.Create('['+IntToStr(FCodePos)+'] Internal error: return stack value not valid');

        FCodePos := ((FStack.Top^.ts64^) and $FFFFFFFF) - 1;
        FStack.Pop;
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
        1       : FVarOperation.Negation(FStack.Top); // negation
        20      : FVarOperation.BitNot(FStack.Top);
        21      : FVarOperation.BooleanNot(FStack.Top);
        2..11   :
            begin
              VarDat[1] := Stack.Top;
              VarDat[0] := Stack[Stack.Size - 2];
              case PSE2OpOP_OPERATION(OpCode).OpType of
              2  : FVarOperation.Addition(VarDat[0], VarDat[1]);
              3  : FVarOperation.Substract(VarDat[0], VarDat[1]);
              4  : FVarOperation.Multiply(VarDat[0], VarDat[1]);
              5  : FVarOperation.Divide(VarDat[0], VarDat[1]);
              6  : FVarOperation.BitAnd(VarDat[0], VarDat[1]);
              7  : FVarOperation.BitOr(VarDat[0], VarDat[1]);
              8  : FVarOperation.BitXor(VarDat[0], VarDat[1]);
              9  : FVarOperation.DivideMod(VarDat[0], VarDat[1]);
              10 : FVarOperation.BitShr(VarDat[0], VarDat[1]);
              11 : FVarOperation.BitShl(VarDat[0], VarDat[1]);
              end;
              // Aritmetic
              FStack.Pop;
            end;
        end;
      end;
  soOP_COMPARE :
      begin
        //QueryPerformanceFrequency(c);
        //QueryPerformanceCounter(t1);
        VarDat[1] := FStack.Top;
        VarDat[0] := FStack[Stack.Size - 2];
        case PSE2OpOP_COMPARE(OpCode).CompType of
        1 : CompareInt := Ord(FVarCompare.Equal(VarDat[0], VarDat[1]));
        2 : CompareInt := Ord(FVarCompare.Smaller(VarDat[0], VarDat[1]));
        3 : CompareInt := Ord(FVarCompare.Bigger(VarDat[0], VarDat[1])); 
        4 : CompareInt := Ord(FVarCompare.BiggerEqual(VarDat[0], VarDat[1]));
        5 : CompareInt := Ord(FVarCompare.SmallerEqual(VarDat[0], VarDat[1]));
        6 : CompareInt := Ord(FVarCompare.UnEqual(VarDat[0], VarDat[1]));
        end;
        FStack.Pop;
        FStack.Pop;
        FStack.PushNew(btBoolean).tu8^ := CompareInt;
        //QueryPerformanceCounter(t2);
        //FRunTime := FRunTime + ( t2 - t1) / c;
      end;
  soDAT_COPY_TO :
      begin
        CompareInt := PSE2OpDAT_COPY_TO(OpCode).Target;
        if PSE2OpDAT_COPY_TO(OpCode).Static then
           FVarHelper.SetVarData(FStack.Top, FStack[CompareInt])
        else
           FVarHelper.SetVarData(FStack.Top, FStack[FStack.Size-1 + CompareInt]);
        FStack.Pop;
      end;
  soDAT_COPY_FROM :
      begin
        CompareInt := PSE2OpDAT_COPY_FROM(OpCode).Source;

        if PSE2OpDAT_COPY_FROM(OpCode).Static then
        begin
          VarDat[0] := FStack[CompareInt];
          VarDat[1] := FStack.PushNew(VarDat[0].AType);
        end else
        begin
          VarDat[0] := FStack[FStack.Size-1 + CompareInt];
          VarDat[1] := FStack.PushNew(VarDat[0].AType);
        end;
        FVarHelper.SetVarData(VarDat[0], VarDat[1]);       
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
        FVarHelper.ConvertContent(Stack.Top, PSE2OpDAT_CONVERT(OpCode).NewType);
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
        if FStack.Top.AType <> btDouble then
        begin
          FVarHelper.FreeVarContent(FStack.Top);
          FStack.Top.AType := btDouble;
          FVarHelper.CreateVarContent(FStack.Top);
        end;
        FStack.Top.tDouble^ := PSE2OpDAT_SetFloat(OpCode).Value;
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
      end;
  soSPEC_INCP :
      begin

      end;
  soSPEC_DECP :
      begin

      end;
  soINT_INCSTATIC :
      begin
        VarDat[0] := FStack[PSE2OpINT_INCSTATIC(OpCode).Offset];
        VarDat[1] := FStack.Pool.Pop(btU32);
        VarDat[1]^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Addition(VarDat[0], VarDat[1]);
        FVarHelper.FreeVarData(VarDat[1]);
      end;
  soINT_DECSTATIC :
      begin
        VarDat[0] := FStack[PSE2OpINT_INCSTATIC(OpCode).Offset];
        VarDat[1] := FStack.Pool.Pop(btU32);
        VarDat[1]^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Substract(VarDat[0], VarDat[1]);   
        FVarHelper.FreeVarData(VarDat[1]);
      end;
  soINT_INCSTACK :
      begin
        VarDat[0] := FStack[FStack.Size - 1 - PSE2OpINT_INCSTATIC(OpCode).Offset];
        VarDat[1] := FStack.Pool.Pop(btU32);
        VarDat[1]^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Addition(VarDat[0], VarDat[1]);   
        FVarHelper.FreeVarData(VarDat[1]);
      end;
  soINT_DECSTACK :
      begin
        VarDat[0] := FStack[FStack.Size - 1 - PSE2OpINT_INCSTATIC(OpCode).Offset];
        VarDat[1] := FStack.Pool.Pop(btU32);
        VarDat[1]^.tu32^ := PSE2OpINT_INCSTATIC(OpCode).Value;

        FVarOperation.Substract(VarDat[0], VarDat[1]); 
        FVarHelper.FreeVarData(VarDat[1]);
      end;
  soSAFE_TRYFIN :
      begin
        FSafeBlocks.Add(blFinally, FStack.Size, PSE2OpSAFE_TRYFIN(OpCode).SavePos, PSE2OpSAFE_TRYFIN(OpCode).LeavePos);
      end;
  soSAFE_TRYEX :
      begin
        FSafeBlocks.Add(blExcept, FStack.Size, PSE2OpSAFE_TRYFIN(OpCode).SavePos, PSE2OpSAFE_TRYFIN(OpCode).LeavePos);
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
                DoErrorEvent(TempTryBlock.ErrorExcept, TempTryBlock.ErrorMsg, TempTryBlock.ErrorPos, TempTryBlock.ErrorStack);
                FCodePos := -1;
                exit;
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
end;
{$ENDIF}

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

  procedure SetVariableContent(aParamType: byte; DataType: byte; Data: Pointer);
  const 
    SParamNotCompatible = 'Parameter not compatible to script method parameter';

  var newEntry    : PSE2VarData;
      fSingle     : single;
      fDouble     : double;
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
  begin
    // result
    if MetaEntry.HasResult then
       FStack.PushNew(MetaEntry.ResultType);

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
        vtPointer      : SetVariableContent(aParamType, vtPointer, @Parameter.VPointer);
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
            SetVariableContent(aParamType, vtPointer, Parameter.VPointer); 
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
      if bIsVarParam then
      begin
        Parameter   := Params[i];
        Data := FStack.Top;
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
        btArray,
        btRecord       : PPointer(Parameter.VPointer)^    := Pointer(Data^.tPointer^);
        end;
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
      btSingle     : result := FStack.Top^.tSingle^;
      btDouble     : result := FStack.Top^.tDouble^;
      btString     : result := PbtString(FStack.Top^.tString^)^;
      btUTF8String : result := PbtUTF8String(FStack.Top^.tString^)^;
      btWideString : result := PbtWideString(FStack.Top^.tString^)^;
      btPChar      : result := string(PbtPChar(FStack.Top^.tString^)^);
      btRecord,
      btArray,
      btPointer,
      btObject     : result := cardinal(FStack.Top^.tPointer^);
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
      Methods  := GetClassMethods(PPointer(SelfPtr.tPointer)^);
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
  result := uSE2SystemUnit.CreateScriptClassObject(Meta, FAppCode);
end;

procedure TSE2RunTime.FreeClass(AClass: Pointer);
begin
  uSE2SystemUnit.DestroyScriptClassObject(AClass);
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
