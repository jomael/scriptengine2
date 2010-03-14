unit uSE2PerfMonitor;

{$INCLUDE ScriptEngine.inc}

interface

{$IFNDEF SEII_FPC}
uses
  Windows, Classes, uSE2BaseTypes, uSE2OpCode;

type
  PSE2PerfMonitorEntry = ^TSE2PerfMonitorEntry;
  TSE2PerfMonitorEntry = record
    OpCode : TSE2OpCode;
    Time   : double;
    Count  : integer;
  end;

  TSE2PerfMonitor = class(TObject)
  private
    FList : TList;
    FFreq : int64;
    FT1   : int64;
  protected
    function  GetOpCodeTime(OpCode: TSE2OpCode): double;
    function  GetOpCodeCount(OpCode: TSE2OpCode): integer;
    function  IndexOf(OpCode: TSE2OpCode): integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Start;
    procedure Stop(OpCode: TSE2OpCode);

    property  Times[OpCode: TSE2OpCode]: double read GetOpCodeTime; default;
    property  Count[OpCode: TSE2OpCode]: integer read GetOpCodeCount;

  end;
  
{$ENDIF}

implementation

{$IFNDEF SEII_FPC}

{ TSE2PerfMonitor }

procedure TSE2PerfMonitor.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Dispose(PSE2PerfMonitorEntry(FList[i]));
  FList.Clear;
end;

constructor TSE2PerfMonitor.Create;
begin
  inherited;
  FList := TList.Create;
  QueryPerformanceFrequency(FFreq);
end;

destructor TSE2PerfMonitor.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2PerfMonitor.GetOpCodeCount(OpCode: TSE2OpCode): integer;
var i: integer;
begin
  i := IndexOf(OpCode);
  if i < 0 then
     result := 0
  else
     result := PSE2PerfMonitorEntry(FList[i])^.Count;
end;

function TSE2PerfMonitor.GetOpCodeTime(OpCode: TSE2OpCode): double;
var i: integer;
begin
  i := IndexOf(OpCode);
  if i < 0 then
     result := 0
  else
     result := PSE2PerfMonitorEntry(FList[i])^.Time;
end;

function TSE2PerfMonitor.IndexOf(OpCode: TSE2OpCode): integer;
begin
  for result:=FList.Count-1 downto 0 do
    if PSE2PerfMonitorEntry(FList[result])^.OpCode = OpCode then
      exit;
  result := -1;
end;

procedure TSE2PerfMonitor.Start;
begin
  QueryPerformanceCounter(FT1);
end;

procedure TSE2PerfMonitor.Stop(OpCode: TSE2OpCode);
var t2: int64;
    p : PSE2PerfMonitorEntry;
    i : integer;
begin
  QueryPerformanceCounter(t2);
  i := IndexOf(OpCode);
  if i < 0 then
  begin
    New(p);
    p^.OpCode := OpCode;
    p^.Time   := 0;
    p^.Count  := 0;
    FList.Add(p);
  end else
    p := FList[i];

  p^.Time := p^.Time + (t2 - FT1) / FFreq;
  p^.Count := p^.Count + 1;
end;

{$ENDIF}

end.
