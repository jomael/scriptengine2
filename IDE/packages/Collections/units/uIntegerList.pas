unit uIntegerList;

{$IFDEF FPC}
{$MODE Delphi}{$H+}
{$ENDIF}

interface

uses
  Classes;

type
  TIntegerList = class(TList)
  protected
    function GetItem(index: integer): integer;
    procedure SetItem(index, value: integer);
  public
    procedure Add(value: integer);
    function  First: integer;
    function  Last: integer;
    function  IndexOf(Item: integer): integer;
    procedure Insert(index, value: integer);
    function  Remove(value: integer): integer;
    procedure Sort(Ascending: boolean);

    property Items[index: integer]: integer read GetItem write SetItem; default;
  end;

  TInt64List = class(TList)
  protected
    function GetItem(index: integer): int64;
    procedure SetItem(index: integer; value: int64);
  public
    procedure Clear; override;
    procedure Delete(index: integer);

    procedure Add(value: int64);
    function  First: int64;
    function  Last: int64;
    function  IndexOf(Item: int64): integer;
    procedure Insert(index: integer; value: int64);
    function  Remove(value: int64): integer;
    procedure Sort(Ascending: boolean);

    property Items[index: integer]: int64 read GetItem write SetItem; default;
  end;

implementation

{ TIntegerList }

procedure TIntegerList.Add(value: integer);
begin
  inherited Add(Pointer(value));
end;

function TIntegerList.First: integer;
begin
  result := integer(inherited First);
end;

function TIntegerList.GetItem(index: integer): integer;
begin
  result := integer(inherited Items[index]);
end;

function TIntegerList.IndexOf(Item: integer): integer;
begin
  result := inherited IndexOf(Pointer(Item));
end;

procedure TIntegerList.Insert(index, value: integer);
begin
  inherited Insert(index, Pointer(value));
end;

function TIntegerList.Last: integer;
begin
  result := integer(inherited Last);
end;

function TIntegerList.Remove(value: integer): integer;
begin
  result := inherited Remove(Pointer(value));
end;

procedure TIntegerList.SetItem(index, value: integer);
begin
  inherited Items[index] := Pointer(value);
end;

function IntegerCompareDescending(i1, i2: integer): integer;
begin
  if i1 < i2 then
     result := -1
  else
  if i1 > i2 then
     result := 1
  else
     result := 0;
end;

function IntegerCompareAscending(i1, i2: integer): integer;
begin
  result := -IntegerCompareDescending(i1, i2);
end;

procedure TIntegerList.Sort(Ascending: boolean);
begin
  if Ascending then
     inherited Sort(@IntegerCompareAscending)
  else                   
     inherited Sort(@IntegerCompareDescending);
end;

{ TInt64List }

procedure TInt64List.Add(value: int64);
var p: PInt64;
begin
  New(p);
  p^ := value;
  inherited Add(p);
end;

procedure TInt64List.Clear;
var i: integer;
    p: PInt64;
begin
  for i:=Count-1 downto 0 do
  begin
    p := PInt64( inherited Items[i] );
    if p <> nil then
       Dispose(p);
  end;
  inherited;
end;

procedure TInt64List.Delete(index: integer);
var p: PInt64;
begin
  if (index >= 0) and (index < Count) then
  begin
    p := PInt64(inherited Items[index]);
    if p <> nil then
     Dispose(p);
  end;

  inherited Delete(index);
end;

function TInt64List.First: int64;
var p: PInt64;
begin
  p := PInt64(inherited First);
  if p <> nil then
    result := p^
  else
    result := 0;
end;

function TInt64List.GetItem(index: integer): int64;
var p: PInt64;
begin
  p := PInt64(inherited Items[index]);
  if p <> nil then
     result := p^
  else
     result := 0;
end;

function TInt64List.IndexOf(Item: int64): integer;
begin
  for result := Count-1 downto 0 do
    if Items[result] = Item then
      exit;
  result := -1;
end;

procedure TInt64List.Insert(index: integer; value: int64);
var p: PInt64;
begin
  New(p);
  p^ := value;
  inherited Insert(index, p);
end;

function TInt64List.Last: int64;
var p: PInt64;
begin
  p := PInt64(inherited Last);
  if p <> nil then
    result := p^
  else
    result := 0;
end;

function TInt64List.Remove(value: int64): integer;
begin
  result := IndexOf(value);
  if result >= 0 then
     Delete(result);
end;

procedure TInt64List.SetItem(index: integer; value: int64);
var p: PInt64;
begin
  p := inherited Items[index];
  if p <> nil then
     p^ := value
  else
  begin
    New(p);
    p^ := value;
    inherited Items[index] := p;
  end;
end;

function Int64CompareDescending(i1, i2: PInt64): integer;
var v1, v2: int64;
begin
  if i1 = nil then
    v1 := 0
  else
    v1 := i1^;
    
  if i2 = nil then
    v2 := 0
  else
    v2 := i2^;

  if v1 < v2 then
     result := -1
  else
  if v1 > v2 then
     result := 1
  else
     result := 0;
end;

function Int64CompareAscending(i1, i2: PInt64): integer;
begin
  result := -Int64CompareDescending(i1, i2);
end;

procedure TInt64List.Sort(Ascending: boolean);
begin
  if Ascending then
     inherited Sort(@Int64CompareAscending)
  else
     inherited Sort(@Int64CompareDescending);
end;

end.
