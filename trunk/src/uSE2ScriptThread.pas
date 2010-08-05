unit uSE2ScriptThread;

{$INCLUDE ScriptEngine.inc}

interface

{$IFNDEF SEII_NO_SCRIPT_THREAD}

uses
  Classes, SysUtils, uSE2ExecutionContext, SyncObjs;

type
  ESE2ThreadException = class(Exception);
  ESE2ThreadTerminate = class(ESE2ThreadException);

  TThreadState = (tsWaiting, tsExecuting, tsSuspended, tsFinished);

  TSE2ScriptThread = class(TThread)
  private
    FStateSection      : SyncObjs.TCriticalSection;
    FState             : TThreadState;
    FExecutePos        : Pointer;
    FInstance          : Pointer;
    FScriptContext     : Pointer;
    FContext           : TSE2ExecutionContext;
    FOnScriptTerminate : TNotifyEvent;
    FDoTerminate       : boolean;
  protected
    procedure BeforeExecutingEvent(Sender: TObject);
    procedure TerminatedEvent(Sender: TObject);

    function  GetAffinity: cardinal;
    procedure SetAffinity(value: cardinal);
    procedure SetState(value: TThreadState);
    function  GetState: TThreadState;
    procedure Execute; override;

    procedure FreeScriptContext;
  public
    constructor Create(ARunTime, AContext: Pointer); virtual;
    destructor Destroy; override;

    procedure Terminate; reintroduce;

    property Affinity    : cardinal     read GetAffinity        write SetAffinity;
    property Context     : Pointer      read FScriptContext;
    property State       : TThreadState read GetState;
    property OnDone      : TNotifyEvent read FOnScriptTerminate write FOnScriptTerminate;
  end;

{$ENDIF}

implementation

{$IFNDEF SEII_NO_SCRIPT_THREAD}

uses
  uSE2RunTime, uSE2OpCode, uSE2Consts
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  ;

{ TScriptThread }

procedure TSE2ScriptThread.BeforeExecutingEvent(Sender: TObject);
begin
  if FDoTerminate then
  begin
    FDoTerminate := False;
    raise ESE2ThreadTerminate.Create('Terminal signal raised');
  end;
end;

constructor TSE2ScriptThread.Create(ARunTime, AContext: Pointer);
var setThreadPtr: Pointer;
begin
  if ARunTime = nil then
     raise ESE2NullReferenceError.Create('Script runtime not assigned');
  if AContext = nil then
     raise ESE2NullReferenceError.Create('Script execution context can not be nil');

  inherited Create(True);
  FreeOnTerminate := False;
  FInstance       := ARunTime;
  FScriptContext  := AContext;

  FContext        := TSE2ExecutionContext.Create;
  if FInstance <> nil then
     TSE2RunTime(FInstance).AddThreadContext(@FContext);

  FContext.ExecutionData := TSE2RunTime(ARunTime).ExecutionData;
  FContext.OnBeforeOperation := BeforeExecutingEvent;

  inherited OnTerminate := TerminatedEvent;

  FState := tsWaiting;

  setThreadPtr := FContext.ExecutionData.CodeAccess.FindMethod('TExecutionContext.__SetThread', C_SE2ThreadingUnit, [pmIn, pmIn], [btObject, btObject]);
  if setThreadPtr <> nil then
     FContext.Call(setThreadPtr, [AContext, Self]);

  FExecutePos := FContext.ExecutionData.CodeAccess.FindMethod('TExecutionContext.Execute', C_SE2ThreadingUnit, [pmIn], [btObject]);
  if FExecutePos = nil then
     raise ESE2ThreadException.Create('Execute-Method not found');


  FStateSection := SyncObjs.TCriticalSection.Create;
end;

destructor TSE2ScriptThread.Destroy;
begin
  FreeScriptContext;
  FStateSection.Free;
  inherited;
end;

procedure TSE2ScriptThread.Execute;
begin
  try
    FContext.Call(FExecutePos, [FScriptContext]);
  finally                       
    SetState(tsFinished);
  end;
end;

procedure TSE2ScriptThread.FreeScriptContext;
begin                                
  if FContext <> nil then
  begin
    if FInstance <> nil then
       TSE2RunTime(FInstance).RemoveThreadContext(@FContext);
    FreeAndNil(FContext);
  end;
end;
          
{$Warnings off}
function TSE2ScriptThread.GetAffinity: cardinal;
{$IFDEF MSWINDOWS}
var tmp: cardinal;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  GetProcessAffinityMask(Self.Handle, result, tmp);
  {$ELSE}
  result := $FFFFFFFF;
  {$ENDIF}
end;
{$Warnings on}

function TSE2ScriptThread.GetState: TThreadState;
begin
  result := tsWaiting;
  try
    FStateSection.Enter;
    try
      result := FState;
    finally
      FStateSection.Leave;
    end;
  except
  end;
end;

procedure TSE2ScriptThread.SetAffinity(value: cardinal);
begin
  {$IFDEF MSWINDOWS}
  SetThreadAffinityMask(Self.Handle, value);
  {$ENDIF}
end;

procedure TSE2ScriptThread.SetState(value: TThreadState);
begin
  try
    FStateSection.Enter;
    try
      FState := value;
    finally
      FStateSection.Leave;
    end;
  except
  end;
end;

procedure TSE2ScriptThread.Terminate;
begin
  FDoTerminate := True;
end;

procedure TSE2ScriptThread.TerminatedEvent(Sender: TObject);
begin
  if Assigned(Self.OnDone) then
  try
     Self.OnDone(TObject(FScriptContext));
  except
  end;
  FreeScriptContext;
end;

{$ENDIF}

end.
