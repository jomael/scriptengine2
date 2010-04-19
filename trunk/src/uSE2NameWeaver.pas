unit uSE2NameWeaver;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2BaseTypes;

type
  PSE2NameWeaverEntry = ^TSE2NameWeaverEntry;
  TSE2NameWeaverEntry = record
    Name     : string;
    NameHash : integer;
    Target   : Pointer;
  end;

  TSE2NameWeaver = class(TSE2Object)
  private
    FList : TList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(const Name: string; Target: Pointer);
    procedure SetValues(const Name: string; Value: Pointer);
  end;

implementation

uses SysUtils;

{ TSE2NameWeaver }

procedure TSE2NameWeaver.Add(const Name: string; Target: Pointer);
var p: PSE2NameWeaverEntry;
begin
  if Name = '' then
     exit;

     
  New(p);
  p^.Name     := Name;
  p^.NameHash := MakeHash(Name);
  p^.Target   := Target;
  FList.Add(p);
end;

procedure TSE2NameWeaver.Clear;
var i: integer;
    p: PSE2NameWeaverEntry;
begin
  for i:=FList.Count-1 downto 0 do
  begin
    p := FList[i];
    p^.Name   := '';
    p^.Target := nil;
    Dispose(p);
  end;
  FList.Clear;
end;

constructor TSE2NameWeaver.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TSE2NameWeaver.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TSE2NameWeaver.SetValues(const Name: string; Value: Pointer);
var NameHash : integer;
    p        : PSE2NameWeaverEntry;
    i        : integer;
begin
  NameHash := MakeHash(Name);
  for i:=FList.Count-1 downto 0 do
  begin
    p := FList[i];

    if p^.NameHash = NameHash then
      if StringIdentical(p^.Name, Name) then
         Pointer(p^.Target^) := Value;
  end;
end;

end.
