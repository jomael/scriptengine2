unit uSE2IncDiagnosticsDebug;

{$INCLUDE ScriptEngine.inc}

interface

uses
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;

implementation

uses
{$IFNDEF SEII_FPC}
  Windows
{$ELSE}
  SysUtils
{$ENDIF}
  ;

const
  C_UnitName   = 'System.Diagnostics';
  C_UnitSource = 
        'unit System.Diagnostics;' + #13#10 + 
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  EAssertionFailed = class(EException)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const Message: string); override;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  Debug = sealed partial class(TObject)' + #13#10 + 
        '  public' + #13#10 + 
        '    class procedure Assert(value: boolean); overload;' + #13#10 + 
        '    /// Checks if "value" is true. If value is false, EAssertionFailed-Exception is generated' + #13#10 + 
        '    class procedure Assert(value: boolean; Message: string); overload;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  /// Stopwatch to measure time' + #13#10 + 
        '  TStopwatch = record' + #13#10 + 
        '  private' + #13#10 + 
        '    FRunning   : boolean;' + #13#10 + 
        '    FStartTime : int64;' + #13#10 + 
        '  private' + #13#10 + 
        '    class var FFrequency : int64;' + #13#10 + 
        '    class procedure InitiateStopwatch;' + #13#10 + 
        '    class function GetCurrentTicks: int64; external;' + #13#10 + 
        '    class function GetFrequency: int64; external;' + #13#10 + 
        '  protected' + #13#10 + 
        '    function GetElapsed: TTimeSpan;' + #13#10 + 
        '    function GetElapsedTicks: int64;' + #13#10 + 
        '    function GetElapsedMilliseconds: int64;' + #13#10 + 
        '  public' + #13#10 + 
        '    /// Make a new stop watch and start it' + #13#10 + 
        '    class function StartNew: TStopwatch;' + #13#10 + 
        #13#10 + 
        '    /// Start the stop watch' + #13#10 + 
        '    procedure Start;' + #13#10 + 
        '    /// Stop the stop watch' + #13#10 + 
        '    procedure Stop;' + #13#10 + 
        #13#10 + 
        '    /// Time in ticks, the stopwatch has started' + #13#10 + 
        '    property StartTicks          : int64     read FStartTime;' + #13#10 + 
        '    /// Elapsed milliseconds since ".Start" has been called' + #13#10 + 
        '    property ElapsedMilliseconds : int64     read GetElapsedMilliseconds;' + #13#10 + 
        '    /// Elapsed ticks since ".Start" has been called' + #13#10 + 
        '    property ElapsedTicks        : int64     read GetElapsedTicks;' + #13#10 + 
        '    /// Elapsed time since ".Start" has been called' + #13#10 + 
        '    property Elapsed             : TTimeSpan read GetElapsed;' + #13#10 + 
        '    /// Stopwatch is running' + #13#10 + 
        '    property Running             : boolean   read FRunning;' + #13#10 + 
        #13#10 + 
        '    /// Frequency to convert "ticks" to "seconds" (seconds = "Ticks" / Frequency' + #13#10 + 
        '    class property Frequency     : int64     read FFrequency;' + #13#10 + 
        '  end;' + #13#10 +
        #13#10 +
        'implementation' + #13#10 + 
        #13#10 + 
        'constructor EAssertionFailed.Create(const Message: string);' + #13#10 + 
        'begin' + #13#10 + 
        '  inherited;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class procedure Debug.Assert(value: boolean);' + #13#10 + 
        'begin' + #13#10 + 
        '  if not value then' + #13#10 + 
        '     raise EAssertionFailed.Create(''Assertion failed'');' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class procedure Debug.Assert(value: boolean; Message: string);' + #13#10 + 
        'begin' + #13#10 + 
        '  if not value then' + #13#10 + 
        '     raise EAssertionFailed.Create(Message);' + #13#10 + 
        'end;' + #13#10 +
        #13#10 +
        '{ TStopwatch }' + #13#10 + 
        #13#10 + 
        'class procedure TStopwatch.InitiateStopwatch;' + #13#10 + 
        'begin' + #13#10 + 
        '  TStopwatch.FFrequency := TStopwatch.GetFrequency;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TStopwatch.GetElapsed: TTimeSpan;' + #13#10 + 
        'begin' + #13#10 + 
        '  if Self.FRunning then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if TStopwatch.Frequency = 0 then' + #13#10 + 
        '       TStopwatch.InitiateStopwatch;' + #13#10 + 
        #13#10 + 
        '    result := TTimeSpan.FromSeconds( (TStopwatch.GetCurrentTicks - Self.FStartTime ) / TStopwatch.FFrequency );' + #13#10 + 
        '  end;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TStopwatch.GetElapsedTicks: int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  if Self.FRunning then' + #13#10 + 
        '  begin' + #13#10 + 
        '    result := TStopwatch.GetCurrentTicks - Self.FStartTime;' + #13#10 + 
        '  end else' + #13#10 + 
        '    result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TStopwatch.GetElapsedMilliseconds: int64;' + #13#10 + 
        'begin' + #13#10 + 
        '  if Self.FRunning then' + #13#10 + 
        '  begin' + #13#10 + 
        '    if TStopwatch.FFrequency = 0 then' + #13#10 + 
        '       TStopwatch.InitiateStopwatch;' + #13#10 + 
        #13#10 + 
        '    result := Math.Round( (TStopwatch.GetCurrentTicks - Self.FStartTime) / TStopwatch.FFrequency * 1000.0);' + #13#10 + 
        '  end else' + #13#10 + 
        '    result := 0;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TStopwatch.StartNew: TStopwatch;' + #13#10 + 
        'begin' + #13#10 +
        '  result.Start;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'procedure TStopwatch.Start;' + #13#10 + 
        'begin' + #13#10 + 
        '  Self.FRunning := True;' + #13#10 +
        '  if TStopwatch.FFrequency = 0 then' + #13#10 +
        '     TStopwatch.InitiateStopwatch;' + #13#10 +
        '  Self.FStartTime := TStopwatch.GetCurrentTicks;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'procedure TStopwatch.Stop;' + #13#10 + 
        'begin' + #13#10 + 
        '  Self.FRunning := False;' + #13#10 + 
        'end;' + #13#10 +
        #13#10 + 
        'end.';

var
  TStopwatch_Frequency : int64 = 0;
  TStopwatch_UseQuery  : boolean;

procedure TStopwatch_InitiateFrequency;
begin
  {$IFNDEF SEII_FPC}
  if not QueryPerformanceFrequency(TStopwatch_Frequency) then
  begin
    TStopwatch_UseQuery  := False;
    TStopwatch_Frequency := 1000;
  end else
    TStopwatch_UseQuery := True;
  {$ELSE}
  TStopwatch_UseQuery  := False;
  TStopwatch_Frequency := 1000;
  {$ENDIF}
end;

function TStopwatch_GetCurrentTicks(__Self: pointer): int64;
begin
  if TStopwatch_Frequency = 0 then
     TStopwatch_InitiateFrequency;

  {$IFNDEF SEII_FPC}
  if TStopwatch_UseQuery then
     QueryPerformanceCounter(Result)
  else
     result := GetTickCount;
  {$ELSE}
  result := GetTickCount;
  {$ENDIF}
end;

function TStopwatch_GetFrequency(__Self: pointer): int64;
begin
  if TStopwatch_Frequency = 0 then
     TStopwatch_InitiateFrequency;
  result := TStopwatch_Frequency;
end;

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin         
    Target.Method['TStopwatch.GetCurrentTicks[0]', C_UnitName] := @TStopwatch_GetCurrentTicks;
    Target.Method['TStopwatch.GetFrequency[0]', C_UnitName] := @TStopwatch_GetFrequency;
  end
end;

procedure RegisterUnit();
var p: TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

initialization
  RegisterUnit();

end.
