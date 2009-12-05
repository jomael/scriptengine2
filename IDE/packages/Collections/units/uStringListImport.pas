unit uStringListImport;

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
        '  { TStringList class }' + #13#10 + 
        '  TStringList = class(TStrings)' + #13#10 + 
        '  protected' + #13#10 + 
        '    function GetDuplicates : TDuplicates; external;' + #13#10 + 
        '    procedure SetDuplicates(value: TDuplicates); external;' + #13#10 + 
        '    function GetSorted: boolean; external;' + #13#10 + 
        '    procedure SetSorted(value: boolean); external;' + #13#10 + 
        '    function GetCaseSensitive: boolean; external;' + #13#10 + 
        '    procedure SetCaseSensitive(value: boolean); external;' + #13#10 +
        '    function GetOnChange: TNotifyEvent; external;' + #13#10 +
        '    procedure SetOnChange(value: TNotifyEvent); external;' + #13#10 +   
        '    function GetOnChanging: TNotifyEvent; external;' + #13#10 +
        '    procedure SetOnChanging(value: TNotifyEvent); external;' + #13#10 +
        '  public' + #13#10 + 
        '    constructor Create; external;' + #13#10 + 
        '    function Add(const S: string): Integer; external;' + #13#10 + 
        '    function AddObject(const S: string; AObject: TObject): Integer; external;' + #13#10 + 
        '    procedure Clear; external;' + #13#10 + 
        '    procedure Delete(Index: Integer); external;' + #13#10 + 
        '    procedure Exchange(Index1, Index2: Integer); external;' + #13#10 + 
        '    function Find(const S: string; var Index: Integer): Boolean; external;' + #13#10 + 
        '    function IndexOf(const S: string): Integer; external;' + #13#10 + 
        '    procedure Insert(Index: Integer; const S: string); external;' + #13#10 + 
        '    procedure InsertObject(Index: Integer; const S: string;' + #13#10 + 
        '      AObject: TObject); external;' + #13#10 + 
        '    procedure Sort;  external;' + #13#10 + 
        #13#10 + 
        '    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;' + #13#10 + 
        '    property Sorted: Boolean read GetSorted write SetSorted;' + #13#10 +
        '    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;' + #13#10 +
        '    property OnChange   : TNotifyEvent read GetOnChange   write SetOnChange;' + #13#10 +
        '    property OnChanging : TNotifyEvent read GetOnChanging write SetOnChanging;' + #13#10 +
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

function TStringList_GetOnChanging(Self: TStringList): TNotifyEvent;
begin
  result := Self.OnChanging;
end;

procedure TStringList_SetOnChanging(Self: TStringList; value: TNotifyEvent);
begin
  Self.OnChanging := TNotifyEvent(value);
end;

function TStringList_GetOnChange(Self: TStringList): TNotifyEvent;
begin
  result := Self.OnChange;
end;

procedure TStringList_SetOnChange(Self: TStringList; value: TNotifyEvent);
begin
  Self.OnChange := TNotifyEvent(value);
end;

function TStringList_GetDuplicates(Self: TStringList): TDuplicates;
begin
  result := Self.Duplicates;
end;

procedure TStringList_SetDuplicates(Self: TStringList; value: TDuplicates);
begin
  Self.Duplicates := value;
end;

function TStringList_GetSorted(Self: TStringList): boolean;
begin
  result := Self.Sorted;
end;

procedure TStringList_SetSorted(Self: TStringList; value: boolean);
begin
  Self.Sorted := value;
end;

function TStringList_GetCaseSensitive(Self: TStringList): boolean;
begin
  result := Self.CaseSensitive;
end;

procedure TStringList_SetCaseSensitive(Self: TStringList; value: boolean);
begin
  Self.CaseSensitive := value;
end;

function TStringList_Create(Self: TStringList): TStringList;
begin
  result := TStringList.Create;
end;

function TStringList_Add(Self: TStringList; const S: string): integer;
begin
  result := Self.Add(S);
end;

function TStringList_AddObject(Self: TStringList; const S: string; AObject: TObject): integer;
begin
  result := Self.AddObject(S, AObject);
end;

procedure TStringList_Clear(Self: TStringList);
begin
  Self.Clear;
end;

procedure TStringList_Delete(Self: TStringList; Index: integer);
begin
  Self.Delete(Index);
end;

procedure TStringList_Exchange(Self: TStringList; Index1, Index2: integer);
begin
  Self.Exchange(Index1, Index2);
end;

function TStringList_Find(Self: TStringList; const S: string; var Index: integer): boolean;
begin
  result := Self.Find(S, Index);
end;

function TStringList_IndexOf(Self: TStringList; const S: string): integer;
begin
  result := Self.IndexOf(S);
end;

procedure TStringList_Insert(Self: TStringList; Index: integer; const S: string);
begin
  Self.Insert(Index, S);
end;

procedure TStringList_InsertObject(Self: TStringList; Index: integer; const S: string; AObject: TObject);
begin
  Self.InsertObject(Index, S, AObject);
end;

procedure TStringList_Sort(Self: TStringList);
begin
  Self.Sort;
end;

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TStringList_GetDuplicates, 'TStringList.GetDuplicates[0]');
  CallBack(Module, Data, @TStringList_SetDuplicates, 'TStringList.SetDuplicates[0]');
  CallBack(Module, Data, @TStringList_GetSorted, 'TStringList.GetSorted[0]');
  CallBack(Module, Data, @TStringList_SetSorted, 'TStringList.SetSorted[0]');
  CallBack(Module, Data, @TStringList_GetCaseSensitive, 'TStringList.GetCaseSensitive[0]');
  CallBack(Module, Data, @TStringList_SetCaseSensitive, 'TStringList.SetCaseSensitive[0]');
  CallBack(Module, Data, @TStringList_Create, 'TStringList.Create[0]');
  CallBack(Module, Data, @TStringList_Add, 'TStringList.Add[0]');
  CallBack(Module, Data, @TStringList_AddObject, 'TStringList.AddObject[0]');
  CallBack(Module, Data, @TStringList_Clear, 'TStringList.Clear[0]');
  CallBack(Module, Data, @TStringList_Delete, 'TStringList.Delete[0]');
  CallBack(Module, Data, @TStringList_Exchange, 'TStringList.Exchange[0]');
  CallBack(Module, Data, @TStringList_Find, 'TStringList.Find[0]');
  CallBack(Module, Data, @TStringList_IndexOf, 'TStringList.IndexOf[0]');
  CallBack(Module, Data, @TStringList_Insert, 'TStringList.Insert[0]');
  CallBack(Module, Data, @TStringList_InsertObject, 'TStringList.InsertObject[0]');
  CallBack(Module, Data, @TStringList_Sort, 'TStringList.Sort[0]');
  CallBack(Module, Data, @TStringList_GetOnChange, 'TStringList.GetOnChange[0]');
  CallBack(Module, Data, @TStringList_SetOnChange, 'TStringList.SetOnChange[0]');  
  CallBack(Module, Data, @TStringList_GetOnChanging, 'TStringList.GetOnChanging[0]');
  CallBack(Module, Data, @TStringList_SetOnChanging, 'TStringList.SetOnChanging[0]');
end;

end.
