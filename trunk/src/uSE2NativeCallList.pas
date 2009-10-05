unit uSE2NativeCallList;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2BaseTypes;

type
  PSE2NativeCallEntry = ^TSE2NativeCallEntry;
  TSE2NativeCallEntry = packed record
    RunTime    : Pointer;
    MethodInfo : Pointer;
    ClassData  : Pointer;
  end;

  TSE2NativeCallList = class(TObject)
  private
    FList : TSE2List;
  protected
    function GetCount: integer;
    function GetItem(index: integer): PSE2NativeCallEntry;

    function IndexOf(MethodInfo, ClassData: Pointer): integer; 
  public
    constructor Create;
    destructor Destroy; override;
                     
    procedure Clear;

    function  GenEntry(RunTime, MethodInfo, ClassData: Pointer): PSE2NativeCallEntry;
    procedure Add(Entry: PSE2NativeCallEntry);
    procedure ClearForClass(ClassData: Pointer);

    property Items[index: integer]: PSE2NativeCallEntry read GetItem; default;
    property Count                : integer             read GetCount;
  end;

implementation

{ TSE2NativeCallList }

procedure TSE2NativeCallList.Add(Entry: PSE2NativeCallEntry);
begin
  FList.Add(Entry);
end;

procedure TSE2NativeCallList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
  begin
    Dispose(PSE2NativeCallEntry(FList[i]));
  end;
  FList.Clear;
end;

procedure TSE2NativeCallList.ClearForClass(ClassData: Pointer);
var i: integer;
    p: PSE2NativeCallEntry;
begin
  if ClassData = nil then
     exit;

  for i:=FList.Count-1 downto 0 do
  begin
    p := FList[i];
    if p^.ClassData = ClassData then
    begin
      Dispose(p);
      FList.Delete(i);
    end;
  end;
end;

constructor TSE2NativeCallList.Create;
begin
  inherited;
  FList := TSE2List.Create;
end;

destructor TSE2NativeCallList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2NativeCallList.GenEntry(RunTime, MethodInfo,
  ClassData: Pointer): PSE2NativeCallEntry;
var i: integer;
begin
  i := IndexOf(MethodInfo, ClassData);
  if i > -1 then
  begin
    result := FList[i];
    exit;
  end;
  
  New(result);
  result^.RunTime    := RunTime;
  result^.MethodInfo := MethodInfo;
  result^.ClassData  := ClassData;

  FList.Add(result);
end;

function TSE2NativeCallList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2NativeCallList.GetItem(index: integer): PSE2NativeCallEntry;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := Flist[index];
end;

function TSE2NativeCallList.IndexOf(MethodInfo,
  ClassData: Pointer): integer;
var p: PSE2NativeCallEntry;
begin
  for result := FList.Count-1 downto 0 do
  begin
    p := Flist[result];
    if (p^.MethodInfo = MethodInfo) and (p^.ClassData = ClassData) then
       exit;
  end;
  result := -1;
end;

end.
