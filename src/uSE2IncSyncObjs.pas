unit uSE2IncSyncObjs;    

{$INCLUDE ScriptEngine.inc}

// To use the package mode, uncomment the following define
{.$DEFINE SE2_IMPORT_AS_PACKAGE}

// If you only want to register the method pointers to
// the script engine (e.g. for a release without
// the posibility to recompile scripts), uncomment the
// following define
{.$DEFINE SE2_ONLY_REGISTER_METHODS}

interface

{$IFNDEF SEII_NO_SCRIPT_THREAD}

uses
{$IFDEF SE2_IMPORT_AS_PACKAGE}
  uSE2PackageAPI, uSE2Consts;
{$ELSE}
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;
{$ENDIF}

{$IFDEF SE2_ONLY_REGISTER_METHODS}
const
  C_UnitName   = C_SE2ThreadingUnit;
{$ELSE}
const
  C_UnitName   = C_SE2ThreadingUnit;
  C_UnitSource = 
        'unit '+C_SE2ThreadingUnit+';' + #13#10 +
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  TSynchronizeObject = class(TExternalObject)' + #13#10 + 
        '  public' + #13#10 + 
        '    procedure Acquire; external;' + #13#10 + 
        '    procedure Release; external;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  TWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError);' + #13#10 + 
        #13#10 + 
        '  TEvent = class(TSynchronizeObject)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Name: string; ManualReset, InitialSate: boolean); overload; external;' + #13#10 + 
        '    constructor Create(const Name: string); overload; external;' + #13#10 + 
        #13#10 + 
        '    function  WaitFor(Timeout: cardinal): TWaitResult; overload; external;' + #13#10 + 
        '    function  WaitFor(TimeSpan: TTimeSpan): TWaitResult; overload;' + #13#10 + 
        #13#10 + 
        '    procedure SetEvent; external;' + #13#10 + 
        '    procedure ResetEvent; external;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  TCriticalSection = class(TSynchronizeObject)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create; external;' + #13#10 + 
        #13#10 + 
        '    procedure Enter; external;' + #13#10 + 
        '    procedure Leave; external;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  TMutex = class(TSynchronizeObject)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create; external;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 +
        '  Monitor = class(TExternalObject)' + #13#10 +
        '  private'+#13#10+
        '    class procedure PEnter(AInstance, Data: Pointer); external;'+#13#10+
        '    class procedure PLeave(AInstance, Data: Pointer); external;'+#13#10+
        '  public' + #13#10 + 
        '    class procedure Enter(Data: Pointer);' + #13#10 +
        '    class procedure Leave(Data: Pointer);' + #13#10 +
        '  end;' + #13#10 +
        #13#10 +
        'implementation' + #13#10 + 
        #13#10 + 
        'function TEvent.WaitFor(TimeSpan: TTimeSpan): TWaitResult;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.WaitFor(Math.Round(TimeSpan.TotalMilliseconds));' + #13#10 + 
        'end;' + #13#10 +
        #13#10 +
        'class procedure Monitor.Enter(Data: Pointer);'+#13#10+
        'begin Monitor.PEnter(ScriptRunTime.Instance, Data); end;'+#13#10+  
        #13#10 +
        'class procedure Monitor.Leave(Data: Pointer);'+#13#10+
        'begin Monitor.PLeave(ScriptRunTime.Instance, Data); end;'+#13#10+
        #13#10 + 
        'end.';

{$ENDIF}

{$IFDEF SE2_IMPORT_AS_PACKAGE}
procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
{$ENDIF}

{$ENDIF}

implementation

{$IFNDEF SEII_NO_SCRIPT_THREAD}

uses
  SyncObjs, uSE2ThreadMonitor, uSE2RunTime,
  {$IFDEF SEII_FPC}
  SysUtils;
  {$ELSE}
  SysUtils, Windows;
  {$ENDIF}

type
  TMutex = class(TSynchroObject)
  private
    FCritSect    : TCriticalSection;
    FIntCritSect : TCriticalSection;
    FEnterCount  : Cardinal;
    FHandle      : Cardinal;
  public
    constructor Create;
    destructor Destroy;override;

    procedure Acquire;override;
    procedure Release;override;
  end;

constructor TMutex.Create;
begin
  inherited;
  FCritSect := TCriticalSection.Create;
  FIntCritSect := TCriticalSection.Create;
  
  FEnterCount := 0;
  FHandle := 0;
end;

destructor TMutex.Destroy;
begin
  FCritSect.Free;
  FIntCritSect.Free;
  inherited;
end;

procedure TMutex.Acquire;
var
  h: Cardinal;
begin
  FIntCritSect.Enter;
  try
    h := GetCurrentThreadId;
  finally
    FIntCritSect.Leave;
  end;

  if h <> FHandle then
  begin
    FCritSect.Enter;

    FIntCritSect.Enter;
    try
      FHandle := h;
      FEnterCount := 0;
    finally
      FIntCritSect.Leave;
    end;
  end
  else
    FEnterCount := FEnterCount + 1;

end;

procedure TMutex.Release;
begin
  if (FEnterCount = 0) then
  begin
    FCritSect.Leave;
    FIntCritSect.Enter;
    try
      FHandle := 0;
    finally
      FIntCritSect.Leave;
    end;
  end else
    FEnterCount := FEnterCount - 1;
end;

procedure TSynchronizeObject_Acquire(Self: TSynchroObject);
begin
  Self.Acquire;
end;

procedure TSynchronizeObject_Release(Self: TSynchroObject);
begin
  Self.Release;
end;

function TEvent_Create(Self: TEvent; const Name: string; ManualReset, InitialSate: boolean): TEvent;
begin
  result := TEvent.Create(nil, ManualReset, InitialSate, Name);
end;

function TEvent_Create1(Self: TEvent; const Name: string): TEvent;
begin
  result := TEvent.Create(nil, False, False, Name);
end;

function TEvent_WaitFor(Self: TEvent; Timeout: cardinal): TWaitResult;
begin
  result := Self.WaitFor(Timeout);
end;

procedure TEvent_SetEvent(Self: TEvent);
begin
  Self.SetEvent;
end;

procedure TEvent_ResetEvent(Self: TEvent);
begin
  Self.ResetEvent;
end;

function TCriticalSection_Create(Self: TCriticalSection): TCriticalSection;
begin
  result := TCriticalSection.Create;
end;

procedure TCriticalSection_Enter(Self: TCriticalSection);
begin
  Self.Enter;
end;

procedure TCriticalSection_Leave(Self: TCriticalSection);
begin
  Self.Leave;
end;

function TMutex_Create(Self: TMutex): TMutex;
begin
  result := TMutex.Create;
end;

procedure Monitor_Enter(Self: Pointer; AInstance, Data: pointer);
begin
  TSE2RunTime(AInstance).ThreadMonitor.Enter(Data);
end;

procedure Monitor_Leave(Self: Pointer; AInstance, Data: pointer);
begin
  TSE2RunTime(AInstance).ThreadMonitor.Leave(Data);
end;

{$IFNDEF SE2_IMPORT_AS_PACKAGE}
procedure Unit_GetSource(var Target: string);
begin
  {$IFNDEF SE2_ONLY_REGISTER_METHODS}
  Target := C_UnitSource;
  {$ELSE}
  Target := '';
  {$ENDIF}
end;
{$ENDIF}

{$IFNDEF SE2_IMPORT_AS_PACKAGE}
procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
    Target.Method['TSynchronizeObject.Acquire[0]', C_UnitName] := @TSynchronizeObject_Acquire;
    Target.Method['TSynchronizeObject.Release[0]', C_UnitName] := @TSynchronizeObject_Release;
    Target.Method['TEvent.Create[0]', C_UnitName] := @TEvent_Create;
    Target.Method['TEvent.Create[1]', C_UnitName] := @TEvent_Create1;
    Target.Method['TEvent.WaitFor[0]', C_UnitName] := @TEvent_WaitFor;
    Target.Method['TEvent.SetEvent[0]', C_UnitName] := @TEvent_SetEvent;
    Target.Method['TEvent.ResetEvent[0]', C_UnitName] := @TEvent_ResetEvent;
    Target.Method['TCriticalSection.Create[0]', C_UnitName] := @TCriticalSection_Create;
    Target.Method['TCriticalSection.Enter[0]', C_UnitName] := @TCriticalSection_Enter;
    Target.Method['TCriticalSection.Leave[0]', C_UnitName] := @TCriticalSection_Leave;
    Target.Method['TMutex.Create[0]', C_UnitName] := @TMutex_Create;
    Target.Method['Monitor.PEnter[0]', C_UnitName] := @Monitor_Enter;
    Target.Method['Monitor.PLeave[0]', C_UnitName] := @Monitor_Leave;
  end
end;
{$ELSE}
procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TSynchronizeObject_Acquire, 'TSynchronizeObject.Acquire[0]');
  CallBack(Module, Data, @TSynchronizeObject_Release, 'TSynchronizeObject.Release[0]');
  CallBack(Module, Data, @TEvent_Create, 'TEvent.Create[0]');
  CallBack(Module, Data, @TEvent_Create1, 'TEvent.Create[1]');
  CallBack(Module, Data, @TEvent_WaitFor, 'TEvent.WaitFor[0]');
  CallBack(Module, Data, @TEvent_SetEvent, 'TEvent.SetEvent[0]');
  CallBack(Module, Data, @TEvent_ResetEvent, 'TEvent.ResetEvent[0]');
  CallBack(Module, Data, @TCriticalSection_Create, 'TCriticalSection.Create[0]');
  CallBack(Module, Data, @TCriticalSection_Enter, 'TCriticalSection.Enter[0]');
  CallBack(Module, Data, @TCriticalSection_Leave, 'TCriticalSection.Leave[0]');
  CallBack(Module, Data, @TMutex_Create, 'TMutex.Create[0]');       
  CallBack(Module, Data, @Monitor_Enter, 'Monitor.PEnter[0]');
  CallBack(Module, Data, @Monitor_Leave, 'Monitor.PLeave[0]');
end;
{$ENDIF}


{$IFNDEF SE2_IMPORT_AS_PACKAGE}
procedure RegisterUnit();
var p: TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;
{$ENDIF}

{$IFNDEF SE2_IMPORT_AS_PACKAGE}
initialization
  RegisterUnit();
{$ENDIF}

{$ENDIF}

end.
