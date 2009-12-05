unit uThreadListImport;

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
        '  { TThreadList class }' + #13#10 + 
        #13#10 + 
        '  TThreadList = class(TExternalObject)' + #13#10 + 
        '  protected' + #13#10 + 
        '    function  GetDuplicates : TDuplicates; external;' + #13#10 + 
        '    procedure SetDuplicates(value: TDuplicates); external;' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create; external;' + #13#10 + 
        '    procedure Add(Item: Pointer); external;' + #13#10 + 
        '    procedure Clear; external;' + #13#10 + 
        '    function  LockList: TList; external;' + #13#10 + 
        '    procedure Remove(Item: Pointer); external;' + #13#10 + 
        '    procedure UnlockList; external;' + #13#10 + 
        #13#10 + 
        '    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

function TThreadList_GetDuplicates(Self: TThreadList): TDuplicates;
begin
  result := Self.Duplicates;
end;

procedure TThreadList_SetDuplicates(Self: TThreadList; value: TDuplicates);
begin
  Self.Duplicates := value;
end;

function TThreadList_Create(Self: TThreadList): TThreadList;
begin
  result := TThreadList.Create;
end;

procedure TThreadList_Add(Self: TThreadList; Item: pointer);
begin
  Self.Add(Item);
end;

procedure TThreadList_Clear(Self: TThreadList);
begin
  Self.Clear;
end;

function TThreadList_LockList(Self: TThreadList): TList;
begin
  result := Self.LockList;
end;

procedure TThreadList_Remove(Self: TThreadList; Item: pointer);
begin
  Self.Remove(Item);
end;

procedure TThreadList_UnlockList(Self: TThreadList);
begin
  Self.UnlockList;
end;

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TThreadList_GetDuplicates, 'TThreadList.GetDuplicates[0]');
  CallBack(Module, Data, @TThreadList_SetDuplicates, 'TThreadList.SetDuplicates[0]');
  CallBack(Module, Data, @TThreadList_Create, 'TThreadList.Create[0]');
  CallBack(Module, Data, @TThreadList_Add, 'TThreadList.Add[0]');
  CallBack(Module, Data, @TThreadList_Clear, 'TThreadList.Clear[0]');
  CallBack(Module, Data, @TThreadList_LockList, 'TThreadList.LockList[0]');
  CallBack(Module, Data, @TThreadList_Remove, 'TThreadList.Remove[0]');
  CallBack(Module, Data, @TThreadList_UnlockList, 'TThreadList.UnlockList[0]');
end;

end.
