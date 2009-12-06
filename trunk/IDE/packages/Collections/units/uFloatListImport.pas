unit uFloatListImport;

{$IFDEF FPC}
{$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  uFloatList,
  uSE2PackageAPI;

const
  C_UnitName   = 'Collections';
  C_UnitSource = 
        'unit Collections;' + #13#10 +
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  TFloatList = class(TList)' + #13#10 + 
        '  protected' + #13#10 + 
        '    function GetItem(index: integer): double; external;' + #13#10 + 
        '    procedure SetItem(index: integer; value: double); external;' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create; external;' + #13#10 + 
        '    procedure Clear; external;' + #13#10 + 
        '    procedure Delete(index: integer); external;' + #13#10 + 
        #13#10 + 
        '    procedure Add(value: double); external;' + #13#10 + 
        '    function  First: double;external;' + #13#10 + 
        '    function  Last: double; external;' + #13#10 + 
        '    function  IndexOf(Item: double): integer; external;' + #13#10 + 
        '    procedure Insert(index: integer; value: double); external;' + #13#10 + 
        '    function  Remove(value: double): integer; external;' + #13#10 + 
        '    procedure Sort(Ascending: boolean); external;' + #13#10 + 
        #13#10 + 
        '    property Items[index: integer]: double read GetItem write SetItem;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

function TFloatList_GetItem(Self: TFloatList; index: integer): double;
begin
  result := Self.Items[index];
end;

procedure TFloatList_SetItem(Self: TFloatList; index: integer; value: double);
begin
  Self.Items[index] := value;
end;

function TFloatList_Create(Self: TFloatList): TFloatList;
begin
  result := TFloatList.Create;
end;

procedure TFloatList_Clear(Self: TFloatList);
begin
  Self.Clear;
end;

procedure TFloatList_Delete(Self: TFloatList; index: integer);
begin
  Self.Delete(index);
end;

procedure TFloatList_Add(Self: TFloatList; value: double);
begin
  Self.Add(value);
end;

function TFloatList_First(Self: TFloatList): double;
begin
  result := Self.First;
end;

function TFloatList_Last(Self: TFloatList): double;
begin
  result := Self.Last;
end;

function TFloatList_IndexOf(Self: TFloatList; Item: double): integer;
begin
  result := Self.IndexOf(Item);
end;

procedure TFloatList_Insert(Self: TFloatList; index: integer; value: double);
begin
  Self.Insert(index, value);
end;

function TFloatList_Remove(Self: TFloatList; value: double): integer;
begin
  result := Self.Remove(value);
end;

procedure TFloatList_Sort(Self: TFloatList; Ascending: boolean);
begin
  Self.Sort(Ascending);
end;

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TFloatList_GetItem, 'TFloatList.GetItem[0]');
  CallBack(Module, Data, @TFloatList_SetItem, 'TFloatList.SetItem[0]');
  CallBack(Module, Data, @TFloatList_Create, 'TFloatList.Create[0]');
  CallBack(Module, Data, @TFloatList_Clear, 'TFloatList.Clear[0]');
  CallBack(Module, Data, @TFloatList_Delete, 'TFloatList.Delete[0]');
  CallBack(Module, Data, @TFloatList_Add, 'TFloatList.Add[0]');
  CallBack(Module, Data, @TFloatList_First, 'TFloatList.First[0]');
  CallBack(Module, Data, @TFloatList_Last, 'TFloatList.Last[0]');
  CallBack(Module, Data, @TFloatList_IndexOf, 'TFloatList.IndexOf[0]');
  CallBack(Module, Data, @TFloatList_Insert, 'TFloatList.Insert[0]');
  CallBack(Module, Data, @TFloatList_Remove, 'TFloatList.Remove[0]');
  CallBack(Module, Data, @TFloatList_Sort, 'TFloatList.Sort[0]');
end;

end.
