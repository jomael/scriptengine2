unit uIntegerListImport;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  uIntegerList,
  uSE2PackageAPI;

const
  C_UnitName   = 'Collections';
  C_UnitSource = 
        'unit Collections;' + #13#10 +
        #13#10 + 
        'interface' + #13#10 +
        #13#10 + 
        'type' + #13#10 + 
        '  TIntegerList = class(TList)' + #13#10 + 
        '  protected' + #13#10 + 
        '    function GetItem(index: integer): integer; external;' + #13#10 + 
        '    procedure SetItem(index, value: integer); external;' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create; external;' + #13#10 + 
        #13#10 + 
        '    procedure Add(value: integer); external;' + #13#10 + 
        '    function  First: integer; external;' + #13#10 + 
        '    function  Last: integer; external;' + #13#10 + 
        '    function  IndexOf(Item: integer): integer; external;' + #13#10 + 
        '    procedure Insert(index, value: integer); external;' + #13#10 + 
        '    function  Remove(value: integer): integer; external;' + #13#10 + 
        '    procedure Sort(Ascending: boolean); external;' + #13#10 + 
        #13#10 + 
        '    property Items[index: integer]: integer read GetItem write SetItem;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  TInt64List = class(TList)' + #13#10 + 
        '  protected' + #13#10 + 
        '    function GetItem(index: integer): int64; external;' + #13#10 + 
        '    procedure SetItem(index: integer; value: int64); external;' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create; external;' + #13#10 + 
        #13#10 + 
        '    procedure Clear; external;' + #13#10 + 
        '    procedure Delete(index: integer); external;' + #13#10 + 
        #13#10 + 
        '    procedure Add(value: int64); external;' + #13#10 + 
        '    function  First: int64; external;' + #13#10 + 
        '    function  Last: int64; external;' + #13#10 + 
        '    function  IndexOf(Item: int64): integer; external;' + #13#10 + 
        '    procedure Insert(index: integer; value: int64); external;' + #13#10 + 
        '    function  Remove(value: int64): integer; external;' + #13#10 + 
        '    procedure Sort(Ascending: boolean); external;' + #13#10 + 
        #13#10 + 
        '    property Items[index: integer]: int64 read GetItem write SetItem;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

function TIntegerList_GetItem(Self: TIntegerList; index: integer): integer;
begin
  result := Self.Items[index];
end;

procedure TIntegerList_SetItem(Self: TIntegerList; index, value: integer);
begin
  Self.Items[index] := value;
end;

function TIntegerList_Create(Self: TIntegerList): TIntegerList;
begin
  result := TIntegerList.Create;
end;

procedure TIntegerList_Add(Self: TIntegerList; value: integer);
begin
  Self.Add(value);
end;

function TIntegerList_First(Self: TIntegerList): integer;
begin
  result := Self.First;
end;

function TIntegerList_Last(Self: TIntegerList): integer;
begin
  result := Self.Last;
end;

function TIntegerList_IndexOf(Self: TIntegerList; Item: integer): integer;
begin
  result := Self.IndexOf(Item);
end;

procedure TIntegerList_Insert(Self: TIntegerList; index, value: integer);
begin
  Self.Insert(index, value);
end;

function TIntegerList_Remove(Self: TIntegerList; value: integer): integer;
begin
  result := Self.Remove(value);
end;

procedure TIntegerList_Sort(Self: TIntegerList; Ascending: boolean);
begin
  Self.Sort(Ascending);
end;

function TInt64List_GetItem(Self: TInt64List; index: integer): int64;
begin
  result := Self.Items[index];
end;

procedure TInt64List_SetItem(Self: TInt64List; index: integer; value: int64);
begin
  Self.Items[index] := value;
end;

function TInt64List_Create(Self: TInt64List): TInt64List;
begin
  result := TInt64List.Create;
end;

procedure TInt64List_Clear(Self: TInt64List);
begin
  Self.Clear;
end;

procedure TInt64List_Delete(Self: TInt64List; index: integer);
begin
  Self.Delete(index);
end;

procedure TInt64List_Add(Self: TInt64List; value: int64);
begin
  Self.Add(value);
end;

function TInt64List_First(Self: TInt64List): int64;
begin
  result := Self.First;
end;

function TInt64List_Last(Self: TInt64List): int64;
begin
  result := Self.Last;
end;

function TInt64List_IndexOf(Self: TInt64List; Item: int64): integer;
begin
  result := Self.IndexOf(Item);
end;

procedure TInt64List_Insert(Self: TInt64List; index: integer; value: int64);
begin
  Self.Insert(index, value);
end;

function TInt64List_Remove(Self: TInt64List; value: int64): integer;
begin
  result := Self.Remove(value);
end;

procedure TInt64List_Sort(Self: TInt64List; Ascending: boolean);
begin
  Self.Sort(Ascending);
end;

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TIntegerList_GetItem, 'TIntegerList.GetItem[0]');
  CallBack(Module, Data, @TIntegerList_SetItem, 'TIntegerList.SetItem[0]');
  CallBack(Module, Data, @TIntegerList_Create, 'TIntegerList.Create[0]');
  CallBack(Module, Data, @TIntegerList_Add, 'TIntegerList.Add[0]');
  CallBack(Module, Data, @TIntegerList_First, 'TIntegerList.First[0]');
  CallBack(Module, Data, @TIntegerList_Last, 'TIntegerList.Last[0]');
  CallBack(Module, Data, @TIntegerList_IndexOf, 'TIntegerList.IndexOf[0]');
  CallBack(Module, Data, @TIntegerList_Insert, 'TIntegerList.Insert[0]');
  CallBack(Module, Data, @TIntegerList_Remove, 'TIntegerList.Remove[0]');
  CallBack(Module, Data, @TIntegerList_Sort, 'TIntegerList.Sort[0]');
  CallBack(Module, Data, @TInt64List_GetItem, 'TInt64List.GetItem[0]');
  CallBack(Module, Data, @TInt64List_SetItem, 'TInt64List.SetItem[0]');
  CallBack(Module, Data, @TInt64List_Create, 'TInt64List.Create[0]');
  CallBack(Module, Data, @TInt64List_Clear, 'TInt64List.Clear[0]');
  CallBack(Module, Data, @TInt64List_Delete, 'TInt64List.Delete[0]');
  CallBack(Module, Data, @TInt64List_Add, 'TInt64List.Add[0]');
  CallBack(Module, Data, @TInt64List_First, 'TInt64List.First[0]');
  CallBack(Module, Data, @TInt64List_Last, 'TInt64List.Last[0]');
  CallBack(Module, Data, @TInt64List_IndexOf, 'TInt64List.IndexOf[0]');
  CallBack(Module, Data, @TInt64List_Insert, 'TInt64List.Insert[0]');
  CallBack(Module, Data, @TInt64List_Remove, 'TInt64List.Remove[0]');
  CallBack(Module, Data, @TInt64List_Sort, 'TInt64List.Sort[0]');
end;

end.
