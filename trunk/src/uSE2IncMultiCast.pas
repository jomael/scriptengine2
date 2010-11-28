unit uSE2IncMultiCast;

{$INCLUDE ScriptEngine.inc}

// To use the package mode, uncomment the following define
{.$DEFINE SE2_IMPORT_AS_PACKAGE}

// If you only want to register the method pointers to
// the script engine (e.g. for a release without
// the posibility to recompile scripts), uncomment the
// following define
{.$DEFINE SE2_ONLY_REGISTER_METHODS}

interface

uses
{$IFDEF SE2_IMPORT_AS_PACKAGE}
  uSE2PackageAPI;
{$ELSE}
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;
{$ENDIF}

{$IFDEF SE2_ONLY_REGISTER_METHODS}
const
  C_UnitName   = 'System';
{$ELSE}
const
  C_UnitName   = 'System';
  C_UnitSource = 
        'unit System;' + #13#10 +
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  TEventArgs = class(TObject)' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  TBroadcastEvent = procedure(Sender: TObject; EventArgs: TEventArgs) of object;' + #13#10 + 
        #13#10 + 
        '  /// Allows to fire a single event to several subscribed event handlers' + #13#10 + 
        '  TMulticastEvent = class(TExternalObject)' + #13#10 + 
        '  private' + #13#10 + 
        '    function GetCount: integer; external;' + #13#10 + 
        '  public' + #13#10 + 
        '    /// Create a new instance of the multi cast event notifier' + #13#10 + 
        '    constructor Create; external;' + #13#10 + 
        #13#10 + 
        '    /// Remove every subscriber from the broadcast list' + #13#10 + 
        '    procedure Clear; external;' + #13#10 + 
        '    /// Add a new subscriber to the broadcast list' + #13#10 + 
        '    procedure AddListener(Handler: TBroadcastEvent); external;' + #13#10 + 
        '    /// Remove a subscriber from the broadcast list' + #13#10 + 
        '    procedure RemoveListener(Handler: TBroadcastEvent); external;' + #13#10 + 
        #13#10 + 
        '    procedure Notify(Sender: TObject; EventArgs: TEventArgs); overload; external;' + #13#10 + 
        '    /// Notify the subscribers' + #13#10 + 
        '    procedure Notify(Sender: TObject; EventArgs: TEventArgs; index: integer); overload; external;' + #13#10 + 
        #13#10 + 
        '    /// Number of subscribers' + #13#10 + 
        '    property  Count : integer read GetCount;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

{$ENDIF}

{$IFDEF SE2_IMPORT_AS_PACKAGE}
procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
{$ENDIF}

implementation

uses
  Classes;

type
  TEventArgs = class

  end;

  TBroadcastEvent = procedure(Sender: TObject; EventArgs: TEventArgs) of object;

  TMulticastEvent = class
  private
    FList : TList;
  protected
    function IndexOf(const Method: TMethod): integer;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddListener(Handler: TBroadcastEvent);
    procedure RemoveListener(Handler: TBroadcastEvent);
    procedure Notify(Sender: TObject; EventArgs: TEventArgs); overload;
    procedure Notify(Sender: TObject; EventArgs: TEventArgs; index: integer); overload;

    property  Count : integer read GetCount;
  end;

{ TEventMultiCast }

type
  PMethod = ^TMethod;

procedure TMulticastEvent.AddListener(Handler: TBroadcastEvent);
var p: PMethod;
begin
  if not Assigned(Handler) then
     exit;

  if IndexOf(TMethod(Handler)) >= 0 then
     exit;

  New(p);
  p^.Code := TMethod(Handler).Code;
  p^.Data := TMethod(Handler).Data;

  FList.Add(p);
end;

procedure TMulticastEvent.Clear;
var i: integer;
begin
  if FList.Count > 0 then
  begin
    for i:=FList.Count-1 downto 0 do
      Dispose(PMethod(FList.List[i]));
    FList.Clear;
  end;
end;

constructor TMulticastEvent.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TMulticastEvent.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TMulticastEvent.GetCount: integer;
begin
  result := FList.Count;
end;

function TMulticastEvent.IndexOf(const Method: TMethod): integer;
begin
  for result:=FList.Count-1 downto 0 do
    if (Method.Code = PMethod(FList.List[result])^.Code) and
       (Method.Data = PMethod(FList.List[result])^.Data) then
       exit;

  result := -1;
end;

procedure TMulticastEvent.Notify(Sender: TObject;
  EventArgs: TEventArgs);
var i: integer;
begin
  for i:=0 to FList.Count-1 do
    Notify(Self, EventArgs, i);
end;

procedure TMulticastEvent.Notify(Sender: TObject;
  EventArgs: TEventArgs; index: integer);
begin
  if (index >= 0) and (index < FList.Count) then
  begin
    TBroadcastEvent(FList.List[index]^)(Sender, EventArgs);
  end;
end;

procedure TMulticastEvent.RemoveListener(Handler: TBroadcastEvent);
var index: integer;
begin
  if Assigned(Handler) then
  begin
    index := IndexOf(TMethod(Handler));
    if index >= 0 then
    begin
      Dispose(PMethod(FList.List[index]));
      FList.Delete(index);
    end;
  end;
end;

{ Import Methods }

function TMulticastEvent_GetCount(Self: TMulticastEvent): integer;
begin
  result := Self.Count;
end;

function TMulticastEvent_Create(Self: TMulticastEvent): TMulticastEvent;
begin
  result := TMulticastEvent.Create;
end;

procedure TMulticastEvent_Clear(Self: TMulticastEvent);
begin
  Self.Clear;
end;

procedure TMulticastEvent_AddListener(Self: TMulticastEvent; Handler: TBroadcastEvent);
begin
  Self.AddListener(Handler);
end;

procedure TMulticastEvent_RemoveListener(Self: TMulticastEvent; Handler: TBroadcastEvent);
begin
  Self.RemoveListener(Handler);
end;

procedure TMulticastEvent_Notify(Self: TMulticastEvent; Sender: TObject; EventArgs: TEventArgs);
begin
  Self.Notify(Sender, EventArgs);
end;

procedure TMulticastEvent_Notify1(Self: TMulticastEvent; Sender: TObject; EventArgs: TEventArgs; index: integer);
begin
  Self.Notify(Sender, EventArgs, index);
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
    Target.Method['TMulticastEvent.GetCount[0]', C_UnitName] := @TMulticastEvent_GetCount;
    Target.Method['TMulticastEvent.Create[0]', C_UnitName] := @TMulticastEvent_Create;
    Target.Method['TMulticastEvent.Clear[0]', C_UnitName] := @TMulticastEvent_Clear;
    Target.Method['TMulticastEvent.AddListener[0]', C_UnitName] := @TMulticastEvent_AddListener;
    Target.Method['TMulticastEvent.RemoveListener[0]', C_UnitName] := @TMulticastEvent_RemoveListener;
    Target.Method['TMulticastEvent.Notify[0]', C_UnitName] := @TMulticastEvent_Notify;
    Target.Method['TMulticastEvent.Notify[1]', C_UnitName] := @TMulticastEvent_Notify1;
  end
end;
{$ELSE}
procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TMulticastEvent_GetCount, 'TMulticastEvent.GetCount[0]');
  CallBack(Module, Data, @TMulticastEvent_Create, 'TMulticastEvent.Create[0]');
  CallBack(Module, Data, @TMulticastEvent_Clear, 'TMulticastEvent.Clear[0]');
  CallBack(Module, Data, @TMulticastEvent_AddListener, 'TMulticastEvent.AddListener[0]');
  CallBack(Module, Data, @TMulticastEvent_RemoveListener, 'TMulticastEvent.RemoveListener[0]');
  CallBack(Module, Data, @TMulticastEvent_Notify, 'TMulticastEvent.Notify[0]');
  CallBack(Module, Data, @TMulticastEvent_Notify1, 'TMulticastEvent.Notify[1]');
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

end.
