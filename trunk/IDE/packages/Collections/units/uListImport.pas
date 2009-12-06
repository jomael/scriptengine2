unit uListImport;

{$IFDEF FPC}
{$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  Classes,
  uSE2PackageAPI;

const
  C_UnitName   = 'Collections';
  C_UnitSource = 
        'unit Collections;' + #13#10 + 
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  TListNotification = (lnAdded, lnExtracted, lnDeleted);' + #13#10 + 
        '  TListAssignOp = (laCopy, laAnd, laOr, laXor, laSrcUnique, laDestUnique);' + #13#10 + 
        '  TDuplicates = (dupIgnore, dupAccept, dupError);' + #13#10 + 
        #13#10 + 
        '  TList = class(TExternalObject)' + #13#10 + 
        '  private' + #13#10 + 
        '    function  GetCapacity : integer; external;' + #13#10 + 
        '    procedure SetCapacity(value: integer);  external;' + #13#10 + 
        '    function  GetCount: integer; external;' + #13#10 + 
        '    procedure SetCount(value: integer); external;' + #13#10 + 
        '    function  GetItem(index: integer): Pointer; external;' + #13#10 + 
        '    procedure SetItem(index: integer; value: Pointer); external;' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create; external;' + #13#10 + 
        '    function Add(Item: Pointer): Integer;  external;' + #13#10 + 
        '    procedure Clear;   external;' + #13#10 + 
        '    procedure Delete(Index: Integer); external;' + #13#10 + 
        '    class procedure Error(const Msg: string; Data: Integer); external;' + #13#10 + 
        '    procedure Exchange(Index1, Index2: Integer); external;' + #13#10 + 
        '    function Expand: TList; external;' + #13#10 + 
        '    function Extract(Item: Pointer): Pointer; external;' + #13#10 + 
        '    function First: Pointer; external;' + #13#10 + 
        '    function IndexOf(Item: Pointer): Integer; external;' + #13#10 + 
        '    procedure Insert(Index: Integer; Item: Pointer); external;' + #13#10 + 
        '    function Last: Pointer; external;' + #13#10 + 
        '    procedure Move(CurIndex, NewIndex: Integer); external;' + #13#10 + 
        '    function Remove(Item: Pointer): Integer; external;' + #13#10 + 
        '    procedure Pack; external;' + #13#10 + 
        '    procedure Assign(ListA: TList; AOperator: TListAssignOp; ListB: TList); overload; external;' + #13#10 + 
        '    procedure Assign(ListA: TList; AOperator: TListAssignOp); overload; external;' + #13#10 + 
        '    procedure Assign(ListA: TList); overload; external;' + #13#10 + 
        #13#10 + 
        '    property Capacity: Integer read GetCapacity write SetCapacity;' + #13#10 + 
        '    property Count   : Integer read GetCount write SetCount;' + #13#10 + 
        '    property Items[Index: Integer]: Pointer read GetItem write SetItem;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

function TList_GetCapacity(Self: TList): integer;
begin
  result := Self.Capacity;
end;

procedure TList_SetCapacity(Self: TList; value: integer);
begin
  Self.Capacity := value;
end;

function TList_GetCount(Self: TList): integer;
begin
  result := Self.Count;
end;

procedure TList_SetCount(Self: TList; value: integer);
begin
  Self.Count := value;
end;

function TList_GetItem(Self: TList; index: integer): pointer;
begin
  result := Self.Items[index];
end;

procedure TList_SetItem(Self: TList; index: integer; value: pointer);
begin
  Self.Items[index] := value;
end;

function TList_Create(Self: TList): TList;
begin
  result := TList.Create;
end;

function TList_Add(Self: TList; Item: pointer): integer;
begin
  result := Self.Add(Item);
end;

procedure TList_Clear(Self: TList);
begin
  Self.Clear;
end;

procedure TList_Delete(Self: TList; Index: integer);
begin
  Self.Delete(Index);
end;

procedure TList_Error(Self: TList; const Msg: string; Data: integer);
begin
  TList.Error(Msg, Data);
end;

procedure TList_Exchange(Self: TList; Index1, Index2: integer);
begin
  Self.Exchange(Index1, Index2);
end;

function TList_Expand(Self: TList): TList;
begin
  result := Self.Expand;
end;

function TList_Extract(Self: TList; Item: pointer): pointer;
begin
  result := Self.Extract(Item);
end;

function TList_First(Self: TList): pointer;
begin
  result := Self.First;
end;

function TList_IndexOf(Self: TList; Item: pointer): integer;
begin
  result := Self.IndexOf(Item);
end;

procedure TList_Insert(Self: TList; Index: integer; Item: pointer);
begin
  Self.Insert(Index, Item);
end;

function TList_Last(Self: TList): pointer;
begin
  result := Self.Last;
end;

procedure TList_Move(Self: TList; CurIndex, NewIndex: integer);
begin
  Self.Move(CurIndex, NewIndex);
end;

function TList_Remove(Self: TList; Item: pointer): integer;
begin
  result := Self.Remove(Item);
end;

procedure TList_Pack(Self: TList);
begin
  Self.Pack;
end;

procedure TList_Assign(Self, ListA: TList; AOperator: TListAssignOp; ListB: TList);
begin
  Self.Assign(ListA, AOperator, ListB);
end;

procedure TList_Assign1(Self, ListA: TList; AOperator: TListAssignOp);
begin
  Self.Assign(ListA, AOperator);
end;

procedure TList_Assign2(Self, ListA: TList);
begin
  Self.Assign(ListA);
end;

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TList_GetCapacity, 'TList.GetCapacity[0]');
  CallBack(Module, Data, @TList_SetCapacity, 'TList.SetCapacity[0]');
  CallBack(Module, Data, @TList_GetCount, 'TList.GetCount[0]');
  CallBack(Module, Data, @TList_SetCount, 'TList.SetCount[0]');
  CallBack(Module, Data, @TList_GetItem, 'TList.GetItem[0]');
  CallBack(Module, Data, @TList_SetItem, 'TList.SetItem[0]');
  CallBack(Module, Data, @TList_Create, 'TList.Create[0]');
  CallBack(Module, Data, @TList_Add, 'TList.Add[0]');
  CallBack(Module, Data, @TList_Clear, 'TList.Clear[0]');
  CallBack(Module, Data, @TList_Delete, 'TList.Delete[0]');
  CallBack(Module, Data, @TList_Error, 'TList.Error[0]');
  CallBack(Module, Data, @TList_Exchange, 'TList.Exchange[0]');
  CallBack(Module, Data, @TList_Expand, 'TList.Expand[0]');
  CallBack(Module, Data, @TList_Extract, 'TList.Extract[0]');
  CallBack(Module, Data, @TList_First, 'TList.First[0]');
  CallBack(Module, Data, @TList_IndexOf, 'TList.IndexOf[0]');
  CallBack(Module, Data, @TList_Insert, 'TList.Insert[0]');
  CallBack(Module, Data, @TList_Last, 'TList.Last[0]');
  CallBack(Module, Data, @TList_Move, 'TList.Move[0]');
  CallBack(Module, Data, @TList_Remove, 'TList.Remove[0]');
  CallBack(Module, Data, @TList_Pack, 'TList.Pack[0]');
  CallBack(Module, Data, @TList_Assign, 'TList.Assign[0]');
  CallBack(Module, Data, @TList_Assign1, 'TList.Assign[1]');
  CallBack(Module, Data, @TList_Assign2, 'TList.Assign[2]');
end;

end.
