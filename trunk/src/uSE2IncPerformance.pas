unit uSE2IncPerformance;

{$INCLUDE ScriptEngine.inc}

interface

uses
  {$IFDEF SEII_INCLUDE_PERF_TIMER}
  Windows,
  {$ENDIF}
  Classes, SysUtils, uSE2Types, uSE2BaseTypes, uSE2RunAccess, uSE2UnitManager;

implementation

const
  C_UnitName   = 'System';
  C_UnitSource =
     'unit System;'+#13#10+
     #13#10+
     'interface'+#13#10+
     #13#10+

     {$IFDEF SEII_INCLUDE_PERF_TIMER}

     'type'+#13#10+
     '  TTimeCounter = class(TExternalObject)'+#13#10+
     '  private'+#13#10+
     '    function GetRunning: boolean; external;'+#13#10+
     '  public'+#13#10+
     '    constructor Create; external;'+#13#10+
     #13#10+
     '    procedure Start; external;'+#13#10+
     '    function  Tick: double; external;'+#13#10+
     '    function  Stop: double; external;'+#13#10+

     '    property  Running: boolean read GetRunning;'+#13#10+
     '  end;'+#13#10+

     {$ENDIF}

     #13#10+
     'implementation'+#13#10+
     #13#10+
     'end.';
  
procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

{$IFDEF SEII_INCLUDE_PERF_TIMER}
type
  TTimeCounter = class
  private
    FRunning : boolean;
    FFreq    : int64;
    FStart   : int64;
    FTick    : int64;
  public
    constructor Create;

    procedure Start;
    function  Stop: double;
    function  Tick: double;

    function  Running: boolean;
  end;
{$ENDIF}

{$IFDEF SEII_INCLUDE_PERF_TIMER}
function TTimeCounter_Create(Temp: Pointer): TTimeCounter;
begin
  result := TTimeCounter.Create;
end;
{$ENDIF}

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  {$IFDEF SEII_INCLUDE_PERF_TIMER}
  if Target.HasUnit(C_UnitName) then
  begin                                                                      
     Target.Method['TTimeCounter.Create[0]', C_UnitName] := @TTimeCounter_Create;
     Target.Method['TTimeCounter.Start[0]', C_UnitName] := @TTimeCounter.Start;
     Target.Method['TTimeCounter.Stop[0]', c_UnitName] := @TTimeCounter.Stop;
     Target.Method['TTimeCounter.Tick[0]', C_UnitName] := @TTimeCounter.Tick;
     Target.Method['TTimeCounter.GetRunning[0]', C_UnitName] := @TTimeCounter.Running;
  end;
  {$ENDIF}
end;

procedure RegisterUnit;
{$IFDEF SEII_INCLUDE_PERF_TIMER}
var p : TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
{$ELSE}
begin
{$ENDIF}
end;

{$IFDEF SEII_INCLUDE_PERF_TIMER}

{ TTimeCounter }

constructor TTimeCounter.Create;
begin
  inherited;
  QueryPerformanceFrequency(FFreq);
end;

function TTimeCounter.Running: boolean;
begin
  result := FRunning;
end;

procedure TTimeCounter.Start;
begin
  QueryPerformanceCounter(FStart);
  FRunning := True;
  FTick := FStart;
end;

function TTimeCounter.Stop: double;
var t1: int64;
begin
  if not FRunning then
     result := 0
  else
  begin
    QueryPerformanceCounter(t1);
    FRunning := False;

    result := (t1 - FStart) / FFreq;
  end;
end;

function TTimeCounter.Tick: double;
var t1: int64;
begin
  if not FRunning then
     result := 0
  else
  begin
    QueryPerformanceCounter(t1);    
    result := (t1 - FTick) / FFreq;
    FTick := t1;
  end;
end;

{$ENDIF}

initialization
  RegisterUnit;

end.
