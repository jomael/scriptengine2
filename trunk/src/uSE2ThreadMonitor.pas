unit uSE2ThreadMonitor;

{$INCLUDE ScriptEngine.inc}

interface

{$IFNDEF SEII_NO_SCRIPT_THREAD}

uses
  {$IFDEF MSWINDOWS}
    {$IFNDEF SEII_FPC}
    Windows,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, SyncObjs;

{$IFDEF SEII_FPC}
type
  TMREWSync = TMultiReadExclusiveWriteSynchronizer;
{$ENDIF}

type
  PSE2MonitorItem = ^TSE2MonitorItem;
  TSE2MonitorItem = packed record
    LockedData  : Pointer;
    LockQueries : integer;
    LockSection : TCriticalSection;
  end;

  TSE2ThreadMonitor = class
  private
    FList     : TList;
    FSection  : TMREWSync;
  protected
    function  IndexOf(AData: Pointer): integer;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    
    procedure Enter(AData: Pointer);
    procedure Leave(AData: Pointer);
  end;

{$ENDIF}

implementation

{$IFNDEF SEII_NO_SCRIPT_THREAD}

{ TThreadMonitor }

procedure TSE2ThreadMonitor.Clear;
var i: integer;
    p: PSE2MonitorItem;
begin
  FSection.BeginWrite;
  try
    for i:=FList.Count-1 downto 0 do
    begin
      p := FList[i];
      p^.LockSection.Free;
      Dispose(p);
    end;
    FList.Clear;
  finally
    FSection.EndWrite;
  end;
end;

constructor TSE2ThreadMonitor.Create;
begin
  inherited;
  FList    := TList.Create;
  FSection := TMREWSync.Create;
end;

destructor TSE2ThreadMonitor.Destroy;
begin
  Clear;
  FList.Free;
  FSection.Free;
  inherited;
end;

procedure TSE2ThreadMonitor.Enter(AData: Pointer);
var index: integer;
    Item : PSE2MonitorItem;
begin
  FSection.BeginRead;
  try
    index := IndexOf(AData);
    if index < 0 then
    begin
      FSection.BeginWrite;
      try
        index := IndexOf(AData);
        if index >= 0 then
           Item := FList[index]
        else
        begin
          New(Item);
          Item^.LockedData  := AData;
          Item^.LockQueries := 0;
          Item^.LockSection := TCriticalSection.Create;
          FList.Add(Item);
        end;
      finally
        FSection.EndWrite;
      end;
    end else
      Item := FList[index];
      
    InterlockedIncrement(Item^.LockQueries);
  finally
    FSection.EndRead;
  end;

  Item^.LockSection.Enter;
end;

function TSE2ThreadMonitor.IndexOf(AData: Pointer): integer;
begin
  for result := FList.Count-1 downto 0 do
     if PSE2MonitorItem(FList[result])^.LockedData = AData then
       exit;
  result := -1;
end;

procedure TSE2ThreadMonitor.Leave(AData: Pointer);
var index: integer;
    Item : PSE2MonitorItem;
begin
  Item := nil;
  FSection.BeginRead;
  try
    index := IndexOf(AData);
    if index >= 0 then
    begin
      Item := FList[index];
      InterlockedDecrement(Item^.LockQueries);
    end;
  finally
    FSection.EndRead;
  end;

  if Item <> nil then
     Item.LockSection.Leave;
end;

{$ENDIF}

end.
