unit uSE2BaseTypes;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2Consts;

type
  TSE2Object = class({$IFDEF SEII_INCLUDE_RTTI}TPersistant{$ELSE}TObject{$ENDIF})
  private
    FTag     : integer;
    FData    : TObject;
    FOwnsObj : boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Tag     : integer   read FTag       write FTag;
    property Data    : TObject   read FData      write FData;
    property OwnsObj : boolean   read FOwnsObj   write FOwnsObj;
  end;

  TSE2Converter = class(TSE2Object)
  public
    class function IsInteger(s: string): boolean;
    class function IsFloat(s: string): boolean;

    class function StrToInt(s: string): int64;
    class function StrToFloat(s: string): double;
    
    class function IntToStr(i: integer): string;
    class function FloatToStr(f: double): string;
  end;

  TSE2Token = class(TSE2Object)
  private
    FValue   : string;
    FType    : TSE2TokenType;
  protected
    function  GetAsInt: int64;
    function  GetAsFloat: double;
    procedure SetAsInt(const value: int64);
    procedure SetAsFloat(const value: double);

    function  GetIsInteger: boolean;
    function  GetIsFloat: boolean;
    function  GetValue: string;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset;

    property IsInteger : boolean        read GetIsInteger;
    property IsFloat   : boolean        read GetIsFloat;

    property Value     : string         read GetValue    write FValue;
    property AType     : TSE2TokenType  read FType       write FType;
    property AsInteger : int64          read GetAsInt    write SetAsInt;
    property AsFloat   : double         read GetAsFloat  write SetAsFloat;
  end;

const
  MaxListSize       = MaxInt div 16;
  FCapacityInc      = 32;
  FMaxCheckCount    = (FCapacityInc div 4) * 64;

type
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  TSE2List = class(TSE2Object)
  private
    FData        : PPointerList;
    FCapacity    : integer;
    FCount       : integer;
    FCheckCount  : integer;
  protected
    function  GetItem(index: integer): Pointer;
    procedure SetItem(index: integer; p: Pointer);
    procedure SetCount(value: integer);
    procedure SetCapacity(value: integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    function  Add(p: Pointer): integer;
    function  IndexOf(p: Pointer): integer;

    procedure Recreate;
    procedure AddBlock(List: PPointerList; Count: integer);
    procedure Remove(p: pointer);
    procedure Delete(index: integer);
    procedure DeleteLast;
    procedure Clear; virtual;

    property  Capacity             : integer      read FCapacity  write SetCapacity;
    property  Items[index: integer]: Pointer      read GetItem    write SetItem; default;
    property  Count                : integer      read FCount     write SetCount;
    property  List                 : PPointerList read FData;
  end;

  TSE2HashedStringList = class
  private
    FList          : TList;
    FCaseSensitive : boolean;
  protected
    function  GetIndexAt(index: integer): string;
    procedure SetIndexAt(index: integer; const value: string);
    function  GetObjectAt(const index: string): Pointer;
    procedure SetObjectAt(const index: string; value: Pointer);
    function  GetObjectAtIndex(index: integer): Pointer;
    procedure SetObjectAtIndex(index: integer; value: Pointer);
    function  GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(const value: string); overload;
    procedure Add(const value: string; AObject: Pointer); overload;
    procedure Insert(index: integer; const value: string); overload;
    procedure Insert(index: integer; const value: string; AObject: Pointer); overload;
    procedure Exchange(index1, index2: integer);
    function  Delete(index: integer): boolean; overload;
    function  Delete(const index: string): boolean; overload;
    function  IndexOf(const value: string): integer; overload;
    function  IndexOf(const AObject: Pointer): integer; overload;

    property CaseSensitive              : boolean read FCaseSensitive write FCaseSensitive;
    property ItemAtIndex[index: integer]: Pointer read GetObjectAtIndex write SetObjectAtIndex;
    property Items[const index: string] : Pointer read GetObjectAt write SetObjectAt;
    property IndexAt[index: integer]    : string  read GetIndexAt write SetIndexAt;
    property Count                      : integer read GetCount;
  end;

function MakeHash(const s: string; MakeLower: boolean = True): integer;
function StringIdentical(const s1, s2: string): boolean;

implementation

uses SysUtils, Math;

type
  PSE2HashedStringItem = ^TSE2HashedStringItem;
  TSE2HashedStringItem = record
    Value   : string;
    Hash    : integer;
    AObject : Pointer;
  end;

function StringIdentical(const s1, s2: string): boolean;
begin
  result := CompareText(s1, s2) = 0;
end;

function MakeHash(const s: string; MakeLower: boolean = True): integer;
var i: integer;
begin
  if MakeLower then
  begin
    result := MakeHash(AnsiLowerCase(s), False);
    exit;
  end;
  result := 0;
  for i:=1 to length(s) do
    result := ((result shl 7) or (result shr 25)) + Ord(s[i]);
end;

{ TSE2Object }

constructor TSE2Object.Create;
begin
  inherited;
  FTag     := 0;
  FData    := nil;
  FOwnsObj := True;
end;

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

destructor TSE2Object.Destroy;
begin
  if FOwnsObj then
    if (FData <> nil) then
    try
      FreeAndNil(FData);
    except
    end;
  inherited;
end;

{ TSE2Converter }

class function TSE2Converter.FloatToStr(f: double): string;
begin
  result := FloatToStrF(f, ffGeneral, 16, 8);
end;

class function TSE2Converter.IntToStr(i: integer): string;
begin
  result := SysUtils.IntToStr(i);
end;

{$HINTS OFF}
{$IFDEF SEII_FPC}
  {$NOTES OFF}
{$ENDIF}
class function TSE2Converter.IsFloat(s: string): boolean;
var c: integer;
    i: extended;
begin
  Val(s, i, c);
  result := c = 0;
end;

// goto http://www.delphipraxis.net/133049-uint64-typ-problem-bei-vorgabewerten.html

class function TSE2Converter.IsInteger(s: string): boolean;
var i: int64;
    c: integer;
begin
  Val(s, i, c);
  result := c = 0;
end;
{$IFDEF SEII_FPC}
  {$NOTES ON}
{$ENDIF}
{$HINTS ON}

class function TSE2Converter.StrToFloat(s: string): double;
var c: integer;
    i: extended;
begin
  Val(s, i, c);
  if c = 0 then
     result := i
  else
  begin
    if not TryStrToFloat(s, i) then
       result := NaN
    else
       result := i;
  end;
end;

class function TSE2Converter.StrToInt(s: string): int64;
var i: int64;
    c: integer;
begin
  Val(s, i, c);
  if c = 0 then
     result := i
  else
     result := -1;
end;


{ TSE2Token }

constructor TSE2Token.Create;
begin
  inherited;
end;

destructor TSE2Token.Destroy;
begin
  inherited;
end;

function TSE2Token.GetAsFloat: double;
begin
  result := TSE2Converter.StrToFloat(FValue);
end;

function TSE2Token.GetAsInt: int64;
begin
  result := TSE2Converter.StrToInt(FValue);
end;

function TSE2Token.GetIsFloat: boolean;
begin
  result := TSE2Converter.IsFloat(FValue);
end;

function TSE2Token.GetIsInteger: boolean;
begin
  result := TSE2Converter.IsInteger(FValue);
end;

function TSE2Token.GetValue: string;
begin
  result := FValue;
end;

procedure TSE2Token.Reset;
begin
  FValue := '';
  FType  := sesNone;
end;

procedure TSE2Token.SetAsFloat(const value: double);
begin
  FValue := TSE2Converter.FloatToStr(value);
end;

procedure TSE2Token.SetAsInt(const value: int64);
begin
  FValue := TSE2Converter.IntToStr(value);
end;

function MM(i1,i2: Integer): Integer;
begin
  if ((i1 div i2) * i2) < i1 then
    result := (i1 div i2 + 1) * i2
  else
    result := (i1 div i2) * i2;
end;

{ TSE2List }

procedure TSE2List.SetCapacity(value: integer);
var NewData      : PPointerList;
    NewCapacity  : Cardinal;
begin
  if value = FCapacity then
     exit;

  if value <= FCount then
     exit;

  NewCapacity := value;
  {$IFDEF SEII_FPC} {$HINTS OFF} {$ENDIF}
  GetMem(NewData, NewCapacity * SizeOf(Pointer));
  {$IFDEF SEII_FPC} {$HINTS ON} {$ENDIF}

  Move(FData^, NewData^, FCount * SizeOf(Pointer));

  FreeMem(FData, FCapacity * SizeOf(Pointer));
  FData     := NewData;
  FCapacity := NewCapacity;
end;

function TSE2List.Add(p: Pointer): integer;
begin
  if FCount >= FCapacity then
  begin
    Inc(FCapacity, FCapacityInc);// := FCount + 1;
    ReAllocMem(FData, FCapacity shl 2);
  end;
  FData[FCount] := P; // Instead of SetItem
  Result := FCount;
  Inc(FCount);
  Inc(FCheckCount);
  if FCheckCount > FMaxCheckCount then
     Recreate;
end;

procedure TSE2List.AddBlock(List: PPointerList; Count: integer);
var l: integer;
begin
  if Longint(FCount) + Count > Longint(FCapacity) then
  begin
    Inc(FCapacity, mm(Count, FCapacityInc));
    ReAllocMem(FData, FCapacity shl 2);
  end;
  for L := 0 to Count -1 do
  begin
    FData^[FCount] := List^[L];
    Inc(FCount);
  end;
  Inc(FCheckCount);
  if FCheckCount > FMaxCheckCount then Recreate;
end;

procedure TSE2List.Clear;
begin
  FCount := 0;
  Recreate;
end;

constructor TSE2List.Create;
begin
  inherited;
  FCount        := 0;
  FCapacity     := 16;
  FCheckCount   := 0;
  GetMem(FData, 64);
end;

procedure TSE2List.Delete(index: integer);
begin
  if FCount = 0 then Exit;
  if index < FCount then
  begin
    Move(FData[index + 1], FData[index], (FCount - index) * 4);
    Dec(FCount);
    Inc(FCheckCount);
    if FCheckCount > FMaxCheckCount then Recreate;
  end;
end;

procedure TSE2List.DeleteLast;
begin
  if FCount = 0 then Exit;
  Dec(FCount);
    Inc(FCheckCount);
    if FCheckCount > FMaxCheckCount then Recreate;
end;

destructor TSE2List.Destroy;
begin
  FreeMem(FData, FCapacity * 4);
  inherited;
end;

function TSE2List.GetItem(index: integer): Pointer;
begin
  if index < FCount then
     result := FData[index]
  else
     result := nil;
end;

function TSE2List.IndexOf(p: Pointer): integer;
var
  i: Integer;
begin
  for i := FCount -1 downto 0 do
  begin
    if FData[i] = p then
    begin
      result := i;
      exit;
    end;
  end;
  result := -1;

end;

procedure TSE2List.Recreate;
var NewData      : PPointerList;
    NewCapacity  : Cardinal;
begin
  FCheckCount := 0;
  NewCapacity := mm(FCount, FCapacityInc);
  if NewCapacity < 64 then
     NewCapacity := 64;
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  GetMem(NewData, NewCapacity * SizeOf(Pointer));
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  Move(FData^, NewData^, FCount * SizeOf(Pointer));

  FreeMem(FData, FCapacity * 4);
  FData     := NewData;
  FCapacity := NewCapacity;
end;

procedure TSE2List.Remove(p: pointer);
var
  I: integer;
begin
  if FCount = 0 then Exit;
  I := 0;
  while I < FCount do
  begin
    if FData[I] = P then
    begin
      Delete(I);
      Exit;
    end;
    Inc(I);
  end;
end;

procedure TSE2List.SetCount(value: integer);
var i: integer;
begin
  for i:=Count to value-1 do
    Add(nil);
end;

procedure TSE2List.SetItem(index: integer; p: Pointer);
begin
  if (FCount = 0) or (index >= FCount) then
    Exit;
  FData[index] := P;
end;

{ TSE2HashedStringList }

procedure TSE2HashedStringList.Add(const value: string);
begin
  Add(value, nil);
end;

procedure TSE2HashedStringList.Add(const value: string; AObject: Pointer);
var p: PSE2HashedStringItem;
begin
  New(p);
  p^.Value := value;
  p^.Hash  := MakeHash(value);
  p^.AObject := AObject;
  FList.Add(p);
end;

procedure TSE2HashedStringList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Delete(i);
end;

constructor TSE2HashedStringList.Create;
begin
  inherited;
  FList := TList.Create;
  FCaseSensitive := False;
end;

function TSE2HashedStringList.Delete(index: integer): boolean;
var p: PSE2HashedStringItem;
begin
  if (index < 0) or (index >= FList.Count) then
     result := False
  else
  begin
    p := FList.List[index];
    FList.Delete(index);
    result := True;

    p^.Value := '';
    Dispose(p);
  end;
end;

function TSE2HashedStringList.Delete(const index: string): boolean;
begin
  result := Delete(IndexOf(index));
end;

destructor TSE2HashedStringList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TSE2HashedStringList.Exchange(index1, index2: integer);
begin
  FList.Exchange(index1, index2);
end;

function TSE2HashedStringList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2HashedStringList.IndexOf(const value: string): integer;
var p: PSE2HashedStringItem;
    h: integer;
begin
  h := MakeHash(value);
  for result:=0 to FList.Count-1 do
  begin
    p := FList.List[result];
    if p^.Hash = h then
    begin
      if FCaseSensitive then
      begin
        if not AnsiSameStr(value, p^.Value) then
           continue;
      end else
      begin
        if not AnsiSameText(value, p^.Value) then
           continue;
      end;
      exit;
    end;
  end;
  result := -1;
end;

procedure TSE2HashedStringList.Insert(index: integer; const value: string);
begin
  Insert(index, value, nil);
end;

function TSE2HashedStringList.GetIndexAt(index: integer): string;
begin
  if (index < 0) or (index >= FList.Count) then
     result := ''
  else
     result := PSE2HashedStringItem(FList.List[index])^.Value;
end;

function TSE2HashedStringList.GetObjectAt(const index: string): Pointer;
begin
  result := GetObjectAtIndex(IndexOf(index));
end;

function TSE2HashedStringList.GetObjectAtIndex(index: integer): Pointer;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := PSE2HashedStringItem(FList.List[index])^.AObject;
end;

function TSE2HashedStringList.IndexOf(const AObject: Pointer): integer;
begin
  for result:=0 to FList.Count-1 do
    if PSE2HashedStringItem(FList.List[result])^.AObject = AObject then
       exit;
  result := -1;
end;

procedure TSE2HashedStringList.Insert(index: integer; const value: string;
  AObject: Pointer);
var p: PSE2HashedStringItem;
begin
  New(p);
  p^.Value := value;
  p^.Hash  := MakeHash(value);
  p^.AObject := AObject;
  FList.Insert(index, p);
end;

procedure TSE2HashedStringList.SetIndexAt(index: integer;
  const value: string);        
var p: PSE2HashedStringItem;
begin
  if (index >= 0) and (index < FList.Count) then
  begin
    p := FList.List[index];
    p^.Value := value;
    p^.Hash  := MakeHash(value);
  end;
end;

procedure TSE2HashedStringList.SetObjectAt(const index: string;
  value: Pointer);
begin
  SetObjectAtIndex(IndexOf(index), value);
end;

procedure TSE2HashedStringList.SetObjectAtIndex(index: integer;
  value: Pointer);
begin
  if (index >= 0) and (index < FList.Count) then
     PSE2HashedStringItem(FList.List[index])^.AObject := value;
end;

end.


