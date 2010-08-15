unit uSE2RunTime;

{$INCLUDE ScriptEngine.inc}

interface

{$IFDEF SEII_FPC}
  {$HINTS OFF}
  {$WARNINGS OFF}
{$ENDIF}

uses
  Classes, SysUtils, uSE2RunType, uSE2Consts, uSE2BaseTypes, uSE2OpCode, uSE2PEData, uSE2RunCall,
  uSE2RunOperation, uSE2RunAccess, uSE2SafeBlockMngr, uSE2DebugData, uSE2MemoryManager, uSE2PerfMonitor,
  uSE2NativeCallList, uSE2RunTimeClasses, uSE2ExecutionContext, uSE2ThreadMonitor;

type
  {$IFDEF SEII_FPC}
    {$UNDEF PERF_MONITOR}
  {$ENDIF}

  TScriptDelayEvent = procedure(var AbortWait: boolean) of object;

  TSE2RunTime = class(TSE2Object)
  private
    { FExecution }
    FExecutionData     : TSE2ExecutionData;
    FContext           : TSE2ExecutionContext;
    FInitialized       : boolean;

    {$IFNDEF SEII_NO_SCRIPT_THREAD}
    FThreadContext     : TThreadList;
    FThreadMonitor     : TSE2ThreadMonitor;
    {$ENDIF}

    FOnError           : TSE2RunTimeError;
  protected
    procedure SetAppCode(const value: TSE2PE);
    function  GetAppCode: TSE2PE;
    function  GetCodeAccess: TSE2RunAccess;
    function  GetClassGCList: TSE2ClassGC;
    function  GetCodePos: integer;
    function  GetStack: TSE2Stack;
                             
    {$IFNDEF SEII_NO_SCRIPT_THREAD}
    procedure ClearThreadContext;
    {$ENDIF}

    function  GetOnBeforeOperation: TNotifyEvent;
    procedure SetOnBeforeOperation(value: TNotifyEvent);
  public
    constructor Create; override;
    destructor Destroy; override;

    // DO NOT CALL DIRECTLY
    procedure DoErrorEvent(Sender: TObject; Exp: ExceptClass; const Msg: string; ErrorPos: integer; const CallStack: string);

    // DO NOT CALL     
    {$IFNDEF SEII_NO_SCRIPT_THREAD}
    procedure AddThreadContext(AContext: Pointer);
    procedure RemoveThreadContext(AContext: Pointer);
    {$ENDIF}

    procedure Initialize;
    procedure Run;
    function  Call(Method: Pointer; const Params: array of const): variant;
    procedure Finalize;

    procedure Abort;
    function  GetCallStack(limit: integer = -1): string; overload;
    procedure GetCallStack(Target: TSE2StackTrace); overload;

    function  CreateClass(const Meta: TSE2MetaEntry): Pointer;
    procedure FreeClass(AClass: Pointer);
    function  ScriptAsMethod(const Method, ClassInstance: Pointer): TMethod;
    function  MethodAsScript(Data: Pointer; MethodPos, ClassPtr: Pointer): boolean;


    {$IFNDEF SEII_NO_SCRIPT_THREAD}
    property ThreadMonitor     : TSE2ThreadMonitor read FThreadMonitor;
    {$ENDIF}
    property ExecutionData     : TSE2ExecutionData read FExecutionData;
    property AppCode           : TSE2PE           read GetAppCode              write SetAppCode;
    property CodeAccess        : TSE2RunAccess    read GetCodeAccess;
    property Initialized       : boolean          read FInitialized;
    property ClassList         : TSE2ClassGC      read GetClassGCList;
    property CodePos           : integer          read GetCodePos;
    property Stack             : TSE2Stack        read GetStack;

    // Events
    property OnError           : TSE2RunTimeError read FOnError              write FOnError;
    property OnBeforeOperation : TNotifyEvent     read GetOnBeforeOperation  write SetOnBeforeOperation;
  end;

var
  OnScriptDelay : TScriptDelayEvent = nil;

implementation

uses  uSE2UnitManager, uSE2SystemUnit, uSE2NativeScriptCall;

{ TSE2RunTime }

constructor TSE2RunTime.Create;
begin
  inherited;

  FExecutionData := TSE2ExecutionData.Create;
  FExecutionData.MemoryMngr   := TSE2MemoryManager.Create;
  FExecutionData.StaticHelper := TSE2VarHelper.Create(FExecutionData.MemoryMngr, nil);
  FExecutionData.StaticMemory := TSE2Stack.Create(FExecutionData.StaticHelper);
  FExecutionData.PackedData   := TSE2RunTimeClasses.Create(FExecutionData.MemoryMngr);
  FExecutionData.RunTime      := Self;
  FExecutionData.PInitialized := @FInitialized;

  FContext := TSE2ExecutionContext.Create;
  FContext.ExecutionData := FExecutionData;
  FContext.OnError       := DoErrorEvent;
                                
  {$IFNDEF SEII_NO_SCRIPT_THREAD}
  FThreadContext := TThreadList.Create;
  FThreadMonitor := TSE2ThreadMonitor.Create;
  {$ENDIF}
end;

destructor TSE2RunTime.Destroy;
begin
  FContext.Free;

  FExecutionData.CodeAccess.Free;
  FExecutionData.CodeAccess := nil;
  FExecutionData.NativeList.Free;
  FExecutionData.NativeList := nil;

  FExecutionData.AppCode.Free;
  FExecutionData.AppCode := nil;
  FExecutionData.PackedData.Free;
  FExecutionData.StaticMemory.Free;
  FExecutionData.StaticHelper.Free;
  FExecutionData.MemoryMngr.Free;
  FExecutionData.Free;

  {$IFNDEF SEII_NO_SCRIPT_THREAD}
  ClearThreadContext;
  FThreadContext.Free;
  FThreadMonitor.Free;
  {$ENDIF}

  inherited;
end;

procedure TSE2RunTime.Finalize;
begin
  if FExecutionData.AppCode = nil then
     exit;
     
  if not FInitialized then
     exit;

  FContext.Run(FExecutionData.AppCode.FinalizationPoint);
  FContext.ProcessGC;
  FContext.ProcessRecordGC;

  {$IFNDEF SEII_NO_SCRIPT_THREAD}
  ClearThreadContext;
  FThreadMonitor.Clear;
  {$ENDIF}
  
  FInitialized := False;
end;

procedure TSE2RunTime.Initialize;
begin
  if FExecutionData.AppCode = nil then
     exit;

  if FInitialized then
     exit;

  FInitialized := True;
  
  {$IFNDEF SEII_NO_SCRIPT_THREAD}
  FThreadMonitor.Clear;
  {$ENDIF}

  FContext.Initialize;
  FContext.Run(FExecutionData.AppCode.InitializationPoint);
end;

procedure TSE2RunTime.Run;
begin
  if not FInitialized then
     exit;

  FContext.Run(FExecutionData.AppCode.MainMethodPoint);
end;

procedure TSE2RunTime.SetAppCode(const value: TSE2PE);
begin
  if FInitialized then
     exit;

  FExecutionData.CodeAccess.Free;
  FExecutionData.CodeAccess := nil;

  FExecutionData.NativeList.Free;
  FExecutionData.NativeList := nil;

  FExecutionData.AppCode := value;
  if value <> nil then
  begin
    FExecutionData.CodeAccess := TSE2RunAccess.Create(value);
    if not value.PointerReady then
    begin
      TSE2UnitManager.RegisterMethods(FExecutionData.CodeAccess);
      TSE2UnitManager.RegisterExceptions(FExecutionData.CodeAccess);

      value.PointerReady := True;
    end;
    FExecutionData.NativeList := TSE2NativeCallList.Create;
  end;
end;

function TSE2RunTime.GetCallStack(limit: integer = -1): string;
begin
  result := FContext.GetCallStack(limit);
end;

procedure TSE2RunTime.GetCallStack(Target: TSE2StackTrace);
begin
  FContext.GetCallStack(Target);
end;

procedure TSE2RunTime.DoErrorEvent(Sender: TObject; Exp: ExceptClass; const Msg: string; ErrorPos: integer;
  const CallStack: string);
begin
  if not (Exp = EAbort) then
    if Assigned(FOnError) then
       FOnError(Self, Exp, Msg, ErrorPos, CallStack);
end;

procedure TSE2RunTime.Abort;
begin
  FContext.Abort;
end;

function TSE2RunTime.Call(Method: Pointer;
  const Params: array of const): variant;
begin
  result := FContext.Call(Method, Params);
end;

function TSE2RunTime.CreateClass(const Meta: TSE2MetaEntry): Pointer;
begin
  result := FExecutionData.PackedData.CreateScriptClassObject(Meta, FExecutionData.AppCode);
end;

procedure TSE2RunTime.FreeClass(AClass: Pointer);
begin
  FExecutionData.PackedData.DestroyScriptClassObject(AClass);
end;

function TSE2RunTime.ScriptAsMethod(const Method,
  ClassInstance: Pointer): TMethod;
begin
  result := FContext.ScriptAsMethod(Method, ClassInstance);
end;

function TSE2RunTime.MethodAsScript(Data, MethodPos,
  ClassPtr: Pointer): boolean;
begin
  result := FContext.MethodAsScript(Data, MethodPos, ClassPtr);
end;

function TSE2RunTime.GetAppCode: TSE2PE;
begin
  result := FExecutionData.AppCode;
end;

function TSE2RunTime.GetCodeAccess: TSE2RunAccess;
begin
  result := FExecutionData.CodeAccess;
end;

function TSE2RunTime.GetClassGCList: TSE2ClassGC;
begin
  result := FContext.ClassGCList;
end;

function TSE2RunTime.GetCodePos: integer;
begin
  result := FContext.CodePos;
end;

function TSE2RunTime.GetStack: TSE2Stack;
begin
  result := FContext.Stack;
end;

function TSE2RunTime.GetOnBeforeOperation: TNotifyEvent;
begin
  result := FContext.OnBeforeOperation;
end;

procedure TSE2RunTime.SetOnBeforeOperation(value: TNotifyEvent);
begin
  FContext.OnBeforeOperation := value;
end;
                            
{$IFNDEF SEII_NO_SCRIPT_THREAD}
procedure TSE2RunTime.AddThreadContext(AContext: Pointer);
begin
  if AContext <> nil then
     FThreadContext.Add(AContext);
end;
{$ENDIF}
        
{$IFNDEF SEII_NO_SCRIPT_THREAD}
procedure TSE2RunTime.RemoveThreadContext(AContext: Pointer);
begin
  if AContext <> nil then
     FThreadContext.Remove(AContext);
end;
{$ENDIF}
                               
{$IFNDEF SEII_NO_SCRIPT_THREAD}
procedure TSE2RunTime.ClearThreadContext;
var i    : integer;
    list : TList;
    p    : Pointer;
    n    : TObject;
begin
  list := FThreadContext.LockList;
  try
    for i:=0 to list.Count-1 do
    begin
      p := list[i];
      n := PPointer(p)^;
      PPointer(p)^ := nil;
      n.Free;
    end;
    
    list.Clear;
{$Hints off}
  finally
    // Hint raises, that inline could not be expanded because uses
    // Windows is missing - but we do not want to change this
    FThreadContext.UnlockList;
  end;
end;
{$Hints on}
{$ENDIF}

end.
