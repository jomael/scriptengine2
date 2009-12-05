unit uFloatList;

interface

uses
  Classes;

type
  TFloatList = class(TList)
  protected
    function GetItem(index: integer): double;
    procedure SetItem(index: integer; value: double);
  public
    procedure Clear; override;
    procedure Delete(index: integer);

    procedure Add(value: double);
    function  First: double;
    function  Last: double;
    function  IndexOf(Item: double): integer;
    procedure Insert(index: integer; value: double);
    function  Remove(value: double): integer;
    procedure Sort(Ascending: boolean);

    property Items[index: integer]: double read GetItem write SetItem; default;
  end;

implementation

{ TFloatList }

procedure TFloatList.Add(value: double);
var p: PDouble;
begin
  New(p);
  p^ := value;
  inherited Add(p);
end;

procedure TFloatList.Clear;
var i: integer;
    p: PDouble;
begin
  for i:=Count-1 downto 0 do
  begin
    p := PDouble( inherited Items[i] );
    if p <> nil then
       Dispose(p);
  end;
  inherited;
end;

procedure TFloatList.Delete(index: integer);
var p: PDouble;
begin
  if (index >= 0) and (index < Count) then
  begin
    p := PDouble(inherited Items[index]);
    if p <> nil then
      Dispose(p);
  end;

  inherited Delete(index);
end;

function TFloatList.First: double;
var p: PDouble;
begin
  p := PDouble(inherited First);
  if p <> nil then
    result := p^
  else
    result := 0.0;
end;

function TFloatList.GetItem(index: integer): double;
var p: PDouble;
begin
  p := PDouble(inherited Items[index]);
  if p <> nil then
     result := p^
  else
     result := 0.0;
end;

function TFloatList.IndexOf(Item: double): integer;
begin
  for result := Count-1 downto 0 do
    if Items[result] = Item then
      exit;
  result := -1;
end;

procedure TFloatList.Insert(index: integer; value: double);
var p: PDouble;
begin
  New(p);
  p^ := value;
  inherited Insert(index, p);
end;

function TFloatList.Last: double;
var p: PDouble;
begin
  p := PDouble(inherited Last);
  if p <> nil then
     result := p^
  else
     result := 0.0;
end;

function TFloatList.Remove(value: double): integer;
begin
  result := IndexOf(value);
  if result >= 0 then
     Delete(result);
end;

procedure TFloatList.SetItem(index: integer; value: double);
var p: PDouble;
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

function FloatCompareDescending(i1, i2: PDouble): integer;
var v1, v2: double;
begin
  if i1 = nil then
     v1 := 0.0
  else
     v1 := i1^;

  if i2 = nil then
     v2 := 0.0
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

function FloatCompareAscending(i1, i2: PDouble): integer;
begin
  result := -FloatCompareDescending(i1, i2);
end;

procedure TFloatList.Sort(Ascending: boolean);
begin
  if Ascending then
     inherited Sort(@FloatCompareAscending)
  else
     inherited Sort(@FloatCompareDescending);
end;

end.
