unit uSE2IncSystemThreading;      

{$INCLUDE ScriptEngine.inc}

interface

uses
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;

implementation

{$IFNDEF SEII_NO_SCRIPT_THREAD}

uses
  Classes, SysUtils, uSE2ScriptThread;

const
  C_UnitName   = C_SE2ThreadingUnit;
  C_UnitSource = 
        'unit ' + C_SE2ThreadingUnit + ';' + #13#10 +
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  TExecutionContext = class;' + #13#10 + 
        #13#10 + 
        '  TThreadState = byte;' + #13#10 +
        '  TThreadPriority = byte;' + #13#10 +
        #13#10 + 
        '  ThreadState = record' + #13#10 + 
        '  public' + #13#10 + 
        '    const Waiting   : TThreadState = 0;' + #13#10 + 
        '    const Executing : TThreadState = 1;' + #13#10 + 
        '    const Suspended : TThreadState = 2;' + #13#10 + 
        '    const Finished  : TThreadState = 3;' + #13#10 +
        '  end;' + #13#10 +
        #13#10 +
        '  ThreadPriority = record' + #13#10 +
        '  public' + #13#10 +
        '    const Idle          : TThreadPriority = 0;' + #13#10 +
        '    const Lowest        : TThreadPriority = 1;' + #13#10 +
        '    const Lower         : TThreadPriority = 2;' + #13#10 +
        '    const Normal        : TThreadPriority = 3;' + #13#10 +
        '    const Higher        : TThreadPriority = 4;' + #13#10 +
        '    const Highest       : TThreadPriority = 5;' + #13#10 +
        '  end;' + #13#10+
        #13#10 + 
        '  EThreadException = class(EExternalException)' + #13#10 +
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 +
        #13#10 +
        '  EThreadTerminate = class(EThreadException)' + #13#10 +
        '  public' + #13#10 +
        '    constructor Create(const Message: string); override;' + #13#10 +
        '  end;'+#13#10+
        #13#10 +
        '  TThreadAffinity = cardinal;'+#13#10+
        #13#10 + 
        '  TThread = class(TExternalObject)' + #13#10 + 
        '  private' + #13#10 +
        '    function  GetPriority: TThreadPriority; external;'+#13#10+
        '    procedure SetPriority(value: TThreadPriority); external;' +#13#10 +
        '    function  GetThreadState: TThreadState; external;' + #13#10 + 
        '    function  GetContext: TExecutionContext; external;' + #13#10 +
        '    function  GetOnDone: TNotifyEvent; external;' + #13#10 + 
        '    procedure SetOnDone(value: TNotifyEvent); external;' + #13#10 +
        '    function  GetAffinity: TThreadAffinity; external;'+#13#10+
        '    procedure SetAffinity(value: TThreadAffinity); external;'+#13#10+
        '    constructor CreateInstance(Instance: Pointer; Context: TExecutionContext; AutoFree: boolean); external;' + #13#10 +
        '  public' + #13#10 +
        '    class function NewThread(Context: TExecutionContext): TThread; overload;' + #13#10 +
        '    class function NewThread(Context: TExecutionContext; AutoFree: boolean): TThread; overload;' + #13#10 +
        #13#10 + 
        '    procedure Start; external;' + #13#10 + 
        '    procedure Suspend; external;' + #13#10 + 
        '    procedure Resume; external;' + #13#10 + 
        '    procedure Terminate; external;' + #13#10 + 
        '    procedure WaitFor; external;' + #13#10 + 
        #13#10 +
        '    property  Priority: TThreadPriority   read GetPriority write SetPriority;'+#13#10+
        '    property  Affinity: TThreadAffinity   read GetAffinity write SetAffinity;'+#13#10+
        '    property  State   : TThreadState      read GetThreadState;' + #13#10 + 
        '    property  Context : TExecutionContext read GetContext;' + #13#10 + 
        '    property  OnDone  : TNotifyEvent      read GetOnDone write SetOnDone;' + #13#10 + 
        #13#10 + 
        '    class procedure Sleep(time: integer); external; overload;' + #13#10 + 
        '    class procedure Sleep(time: TTimeSpan); overload;' + #13#10 + 
        '  end;' + #13#10 +
        #13#10 +
        '  ThreadAffinity = record' + #13#10 +
        '  public' + #13#10 + 
        '    const AllCPUs   : TThreadAffinity = $FFFFFFFF;' + #13#10 +
        '    const CPU0      : TThreadAffinity = 1;' + #13#10 +
        '    const CPU1      : TThreadAffinity = 2;' + #13#10 +
        '    const CPU2      : TThreadAffinity = 4;' + #13#10 +
        '    const CPU3      : TThreadAffinity = 8;' + #13#10 +
        '    const CPU4      : TThreadAffinity = 16;' + #13#10 +
        '    const CPU5      : TThreadAffinity = 32;' + #13#10 +
        '    const CPU6      : TThreadAffinity = 64;' + #13#10 +
        '    const CPU7      : TThreadAffinity = 128;' + #13#10 +
        '    const CPU8      : TThreadAffinity = 256;' + #13#10 +
        '    const CPU9      : TThreadAffinity = 512;' + #13#10 +
        '    const CPU10     : TThreadAffinity = 1024;' + #13#10 +
        '    const CPU11     : TThreadAffinity = 2048;' + #13#10 +
        '    const CPU12     : TThreadAffinity = 4096;' + #13#10 +
        '    const CPU13     : TThreadAffinity = 8192;' + #13#10 +
        '    const CPU14     : TThreadAffinity = 16384;' + #13#10 +
        '    const CPU15     : TThreadAffinity = 32768;' + #13#10 +
        '  end;' + #13#10 +
        #13#10 + 
        '  TExecutionContext = class(TObject)' + #13#10 + 
        '  private' + #13#10 + 
        '    FThread : TThread;' + #13#10 + 
        '    procedure __SetThread(value: TThread); export;' + #13#10 +
        '  protected' + #13#10 + 
        '    procedure Execute; virtual;' + #13#10 + 
        '  public' + #13#10 +
        '    function RunInThread: TThread; overload;' + #13#10 +
        '    function RunInThread(AutoStart: boolean): TThread; overload;' + #13#10 +
        '    function RunInThread(AutoStart, AutoFreeThread: boolean): TThread; overload;' + #13#10 +
        #13#10 +
        '    property Thread : TThread read FThread;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 +
        'implementation' + #13#10 + 
        #13#10 +
        'constructor EThreadException.Create(const Message: string);' + #13#10 +
        'begin' + #13#10 +
        '  inherited;' + #13#10 +
        'end;' + #13#10 +  
        'constructor EThreadTerminate.Create(const Message: string);' + #13#10 +
        'begin' + #13#10 +
        '  inherited;' + #13#10 +
        'end;' + #13#10 +
        #13#10 +
        'class function TThread.NewThread(Context: TExecutionContext): TThread;' + #13#10 +
        'begin' + #13#10 +
        '  result := TThread.CreateInstance(ScriptRunTime.Instance, Context, False);' + #13#10 +
        'end;' + #13#10 +
        #13#10 +
        'class function TThread.NewThread(Context: TExecutionContext; AutoFree: boolean): TThread;' + #13#10 +
        'begin' + #13#10 +
        '  result := TThread.CreateInstance(ScriptRunTime.Instance, Context, AutoFree);' + #13#10 +
        'end;' + #13#10 +
        #13#10 + 
        'class procedure TThread.Sleep(time: TTimeSpan);' + #13#10 + 
        'begin' + #13#10 + 
        '  TThread.Sleep(Math.Trunc(time.TotalMilliseconds));' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'procedure TExecutionContext.__SetThread(value: TThread);' + #13#10 +
        'begin' + #13#10 + 
        '  Self.FThread := value;' + #13#10 + 
        'end;' + #13#10 +
        #13#10 + 
        'procedure TExecutionContext.Execute;' + #13#10 +
        'begin' + #13#10 + 
        'end;' + #13#10 +
        #13#10 +
        'function TExecutionContext.RunInThread: TThread;' + #13#10 +
        'begin' + #13#10 + 
        '  result := Self.RunInThread(True);' + #13#10 + 
        'end;' + #13#10 +
        #13#10 +
        'function TExecutionContext.RunInThread(AutoStart: boolean): TThread;' + #13#10 +
        'begin' + #13#10 +
        '  result := TThread.NewThread(Self);' + #13#10 +
        '  if AutoStart then' + #13#10 +
        '     result.Start;' + #13#10 +
        'end;' + #13#10 +   
        #13#10 +
        'function TExecutionContext.RunInThread(AutoStart, AutoFreeThread: boolean): TThread;' + #13#10 +
        'begin' + #13#10 +
        '  result := TThread.NewThread(Self, AutoFreeThread);' + #13#10 +
        '  if AutoStart then' + #13#10 +
        '     result.Start;' + #13#10 +
        'end;' + #13#10 +
        #13#10 + 
        'end.';

function TThread_GetPriority(Self: TSE2ScriptThread): TThreadPriority;
begin
  result := Self.Priority;
end;

procedure TThread_SetPriority(Self: TSE2ScriptThread; value: TThreadPriority);
begin
  Self.Priority := value;
end;

function TThread_GetThreadState(Self: TSE2ScriptThread): TThreadState;
begin
  result := Self.State;
  if Self.Suspended and (result <> tsWaiting) then
     result := tsSuspended;
end;

function TThread_GetContext(Self: TSE2ScriptThread): Pointer;
begin
  result := Self.Context;
end;

function TThread_GetOnDone(Self: TSE2ScriptThread): TNotifyEvent;
begin
  result := Self.OnDone;
end;

procedure TThread_SetOnDone(Self: TSE2ScriptThread; value: TNotifyEvent);
begin
  Self.OnDone := value;
end;

function  TThread_GetAffinity(Self: TSE2ScriptThread): cardinal;
begin
  result := Self.Affinity;
end;

procedure TThread_SetAffinity(Self: TSE2ScriptThread; value: cardinal);
begin
  Self.Affinity := value;
end;

function TThread_CreateInstance(Self: TSE2ScriptThread; Instance: pointer; Context: Pointer; AutoFree: boolean): TThread;
begin
  result := TSE2ScriptThread.Create(Instance, Context);
  result.FreeOnTerminate := AutoFree;
end;

procedure TThread_Start(Self: TSE2ScriptThread);
begin
  if Self.State = tsWaiting then
  {$Warnings off}
     Self.Resume;
  {$Warnings on}
end;

procedure TThread_Suspend(Self: TThread);
begin
  {$Warnings off}
  Self.Suspend;
  {$Warnings on}
end;

procedure TThread_Resume(Self: TThread);
begin
  {$Warnings off}
  Self.Resume;
  {$Warnings on}
end;

procedure TThread_Terminate(Self: TSE2ScriptThread);
begin
  Self.Terminate;
end;

procedure TThread_WaitFor(Self: TThread);
begin
  Self.WaitFor;
end;

procedure TThread_Sleep(Self: TThread; time: integer);
begin
  SysUtils.Sleep(time);
end;

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
    Target.Method['TThread.GetPriority[0]', C_UnitName] := @TThread_GetPriority;
    Target.Method['TThread.SetPriority[0]', C_UnitName] := @TThread_SetPriority;
    Target.Method['TThread.GetThreadState[0]', C_UnitName] := @TThread_GetThreadState;
    Target.Method['TThread.GetContext[0]', C_UnitName] := @TThread_GetContext;
    Target.Method['TThread.GetOnDone[0]', C_UnitName] := @TThread_GetOnDone;
    Target.Method['TThread.SetOnDone[0]', C_UnitName] := @TThread_SetOnDone;
    Target.Method['TThread.GetAffinity[0]', C_UnitName] := @TThread_GetAffinity;
    Target.Method['TThread.SetAffinity[0]', C_UnitName] := @TThread_SetAffinity;
    Target.Method['TThread.CreateInstance[0]', C_UnitName] := @TThread_CreateInstance;
    Target.Method['TThread.Start[0]', C_UnitName] := @TThread_Start;
    Target.Method['TThread.Suspend[0]', C_UnitName] := @TThread_Suspend;
    Target.Method['TThread.Resume[0]', C_UnitName] := @TThread_Resume;
    Target.Method['TThread.Terminate[0]', C_UnitName] := @TThread_Terminate;
    Target.Method['TThread.WaitFor[0]', C_UnitName] := @TThread_WaitFor;
    Target.Method['TThread.Sleep[0]', C_UnitName] := @TThread_Sleep;
  end
end;

procedure Unit_RegisterException(const Target: TSE2RunAccess);
begin
  Target.Exceptions.Add(ESE2ThreadException, Target.FindClass('EThreadException', C_UnitName));   
  Target.Exceptions.Add(ESE2ThreadTerminate, Target.FindClass('EThreadTerminate', C_UnitName));
end;

procedure RegisterUnit();
var p: TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.DoRegExceptions   := Unit_RegisterException;
  p.UnitName          := C_UnitName;
  p.Priority          := 0;
  TSE2UnitManager.RegisterUnit(p);
end;

initialization
  RegisterUnit();

{$ENDIF}

end.
