unit uStringsImport;

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
        'uses' + #13#10 + 
        '  Streams;' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  TStrings = class(TPersistent)' + #13#10 + 
        '  private' + #13#10 + 
        '    function  GetCapacity: integer; external;' + #13#10 + 
        '    procedure SetCapacity(value: integer); external;' + #13#10 + 
        '    function  GetCommaText: string;  external;' + #13#10 + 
        '    procedure SetCommaText(value: string); external;' + #13#10 + 
        '    function  GetCount: integer; external;' + #13#10 + 
        '    function  GetDelimiter: string; external;' + #13#10 + 
        '    procedure SetDelimiter(value: string); external;' + #13#10 + 
        '    function  GetDelimitedText: string; external;' + #13#10 + 
        '    procedure SetDelimitedText(value: string); external;' + #13#10 + 
        '    function  GetName(index: integer): string; external;' + #13#10 + 
        '    function  GetObject(index: integer): TObject; external;' + #13#10 + 
        '    procedure PutObject(index: integer; value: TObject); external;' + #13#10 + 
        '    function  GetQuoteChar: string;  external;' + #13#10 + 
        '    procedure SetQuoteChar(value: string); external;' + #13#10 + 
        '    function  GetValue(index: string): string; external;' + #13#10 + 
        '    procedure SetValue(index: string; value: string); external;' + #13#10 + 
        '    function  GetValueFromIndex(index: integer): string; external;' + #13#10 + 
        '    procedure SetValueFromIndex(index: integer; value: string); external;' + #13#10 + 
        '    function  GetNameValueSeparator: string; external;' + #13#10 + 
        '    procedure SetNameValueSeparator(value: string); external;' + #13#10 + 
        '    function  GetString(index: integer): string; external;' + #13#10 + 
        '    procedure PutString(index: integer; value: string); external;' + #13#10 + 
        '    function  GetTextStr: string; external;' + #13#10 + 
        '    procedure SetTextStr(value: string); external;' + #13#10 + 
        '  public' + #13#10 + 
        '    function Add(const S: string): Integer;  external;' + #13#10 + 
        '    function AddObject(const S: string; AObject: TObject): Integer;  external;' + #13#10 + 
        '    procedure Append(const S: string); external;' + #13#10 + 
        '    procedure AddStrings(Strings: TStrings); external;' + #13#10 + 
        '    procedure Assign(Source: TPersistent); external;' + #13#10 + 
        '    procedure BeginUpdate; external;' + #13#10 + 
        '    procedure Clear; external;' + #13#10 + 
        '    procedure Delete(Index: Integer); external;' + #13#10 + 
        '    procedure EndUpdate; external;' + #13#10 + 
        '    function Equals(Strings: TStrings): Boolean;  external;' + #13#10 + 
        '    procedure Exchange(Index1, Index2: Integer);  external;' + #13#10 + 
        '    function GetText: PChar;  external;' + #13#10 + 
        '    function IndexOf(const S: string): Integer;  external;' + #13#10 + 
        '    function IndexOfName(const Name: string): Integer;  external;' + #13#10 + 
        '    function IndexOfObject(AObject: TObject): Integer;  external;' + #13#10 + 
        '    procedure Insert(Index: Integer; const S: string); external;' + #13#10 + 
        '    procedure InsertObject(Index: Integer; const S: string; AObject: TObject);   external;' + #13#10 + 
        '    procedure LoadFromFile(const FileName: string);   external;' + #13#10 + 
        '    procedure LoadFromStream(Stream: TStream);  external;' + #13#10 + 
        '    procedure Move(CurIndex, NewIndex: Integer);  external;' + #13#10 + 
        '    procedure SaveToFile(const FileName: string);  external;' + #13#10 + 
        '    procedure SaveToStream(Stream: TStream);  external;' + #13#10 + 
        '    procedure SetText(Text: PChar);  external;' + #13#10 + 
        #13#10 + 
        '    property Capacity: Integer read GetCapacity write SetCapacity;' + #13#10 + 
        '    property CommaText: string read GetCommaText write SetCommaText;' + #13#10 + 
        '    property Count: Integer read GetCount;' + #13#10 + 
        '    property Delimiter: string read GetDelimiter write SetDelimiter;' + #13#10 + 
        '    property DelimitedText: string read GetDelimitedText write SetDelimitedText;' + #13#10 + 
        '    property Names[Index: Integer]: string read GetName;' + #13#10 + 
        '    property Objects[Index: Integer]: TObject read GetObject write PutObject;' + #13#10 + 
        '    property QuoteChar: string read GetQuoteChar write SetQuoteChar;' + #13#10 + 
        '    property Values[Name: string]: string read GetValue write SetValue;' + #13#10 + 
        '    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;' + #13#10 + 
        '    property NameValueSeparator: string read GetNameValueSeparator write SetNameValueSeparator;' + #13#10 + 
        '    property Strings[Index: Integer]: string read GetString write PutString;' + #13#10 + 
        '    property Text: string read GetTextStr write SetTextStr;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

function TStrings_GetCapacity(Self: TStrings): integer;
begin
  result := Self.Capacity;
end;

procedure TStrings_SetCapacity(Self: TStrings; value: integer);
begin
  Self.Capacity := value;
end;

function TStrings_GetCommaText(Self: TStrings): string;
begin
  result := Self.CommaText;
end;

procedure TStrings_SetCommaText(Self: TStrings; value: string);
begin
  Self.CommaText := value;
end;

function TStrings_GetCount(Self: TStrings): integer;
begin
  result := Self.Count;
end;

function TStrings_GetDelimiter(Self: TStrings): string;
begin
  result := Self.Delimiter;
end;

procedure TStrings_SetDelimiter(Self: TStrings; value: string);
begin
  if value <> '' then
     Self.Delimiter := value[1];
end;

function TStrings_GetDelimitedText(Self: TStrings): string;
begin
  result := Self.DelimitedText;
end;

procedure TStrings_SetDelimitedText(Self: TStrings; value: string);
begin
  Self.DelimitedText := value;
end;

function TStrings_GetName(Self: TStrings; index: integer): string;
begin
  result := Self.Names[index];
end;

function TStrings_GetObject(Self: TStrings; index: integer): TObject;
begin
  result := Self.Objects[index];
end;

procedure TStrings_PutObject(Self: TStrings; index: integer; value: TObject);
begin
  Self.Objects[index] := value;
end;

function TStrings_GetQuoteChar(Self: TStrings): string;
begin
  result := Self.QuoteChar;
end;

procedure TStrings_SetQuoteChar(Self: TStrings; value: string);
begin
  if value <> '' then
     Self.QuoteChar := value[1];
end;

function TStrings_GetValue(Self: TStrings; index: string): string;
begin
  result := Self.Values[index];
end;

procedure TStrings_SetValue(Self: TStrings; index, value: string);
begin
  Self.Values[index] := value;
end;

function TStrings_GetValueFromIndex(Self: TStrings; index: integer): string;
begin
  result := Self.ValueFromIndex[index];
end;

procedure TStrings_SetValueFromIndex(Self: TStrings; index: integer; value: string);
begin
  Self.ValueFromIndex[index] := value;
end;

function TStrings_GetNameValueSeparator(Self: TStrings): string;
begin
  result := Self.NameValueSeparator;
end;

procedure TStrings_SetNameValueSeparator(Self: TStrings; value: string);
begin
  if value <> '' then
     Self.NameValueSeparator := value[1];
end;

function TStrings_GetString(Self: TStrings; index: integer): string;
begin
  result := Self.Strings[index];
end;

procedure TStrings_PutString(Self: TStrings; index: integer; value: string);
begin
  Self.Strings[index] := value;
end;

function TStrings_GetTextStr(Self: TStrings): string;
begin
  result := Self.Text;
end;

procedure TStrings_SetTextStr(Self: TStrings; value: string);
begin
  Self.Text := value;
end;

function TStrings_Add(Self: TStrings; const S: string): integer;
begin
  result := Self.Add(S);
end;

function TStrings_AddObject(Self: TStrings; const S: string; AObject: TObject): integer;
begin
  result := Self.AddObject(S, AObject);
end;

procedure TStrings_Append(Self: TStrings; const S: string);
begin
  Self.Append(S);
end;

procedure TStrings_AddStrings(Self, Strings: TStrings);
begin
  Self.AddStrings(Strings);
end;

procedure TStrings_Assign(Self: TStrings; Source: TPersistent);
begin
  Self.Assign(Source);
end;

procedure TStrings_BeginUpdate(Self: TStrings);
begin
  Self.BeginUpdate;
end;

procedure TStrings_Clear(Self: TStrings);
begin
  Self.Clear;
end;

procedure TStrings_Delete(Self: TStrings; Index: integer);
begin
  Self.Delete(Index);
end;

procedure TStrings_EndUpdate(Self: TStrings);
begin
  Self.EndUpdate;
end;

function TStrings_Equals(Self, Strings: TStrings): boolean;
begin
  result := Self.Equals(Strings);
end;

procedure TStrings_Exchange(Self: TStrings; Index1, Index2: integer);
begin
  Self.Exchange(Index1, Index2);
end;

function TStrings_GetText(Self: TStrings): PChar;
begin
  result := Self.GetText;
end;

function TStrings_IndexOf(Self: TStrings; const S: string): integer;
begin
  result := Self.IndexOf(S);
end;

function TStrings_IndexOfName(Self: TStrings; const Name: string): integer;
begin
  result := Self.IndexOfName(Name);
end;

function TStrings_IndexOfObject(Self: TStrings; AObject: TObject): integer;
begin
  result := Self.IndexOfObject(AObject);
end;

procedure TStrings_Insert(Self: TStrings; Index: integer; const S: string);
begin
  Self.Insert(Index, S);
end;

procedure TStrings_InsertObject(Self: TStrings; Index: integer; const S: string; AObject: TObject);
begin
  Self.InsertObject(Index, S, AObject);
end;

procedure TStrings_LoadFromFile(Self: TStrings; const FileName: string);
begin
  Self.LoadFromFile(FileName);
end;

procedure TStrings_LoadFromStream(Self: TStrings; Stream: TStream);
begin
  Self.LoadFromStream(Stream);
end;

procedure TStrings_Move(Self: TStrings; CurIndex, NewIndex: integer);
begin
  Self.Move(CurIndex, NewIndex);
end;

procedure TStrings_SaveToFile(Self: TStrings; const FileName: string);
begin
  Self.SaveToFile(FileName);
end;

procedure TStrings_SaveToStream(Self: TStrings; Stream: TStream);
begin
  Self.SaveToStream(Stream);
end;

procedure TStrings_SetText(Self: TStrings; Text: PChar);
begin
  Self.SetText(Text);
end;

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TStrings_GetCapacity, 'TStrings.GetCapacity[0]');
  CallBack(Module, Data, @TStrings_SetCapacity, 'TStrings.SetCapacity[0]');
  CallBack(Module, Data, @TStrings_GetCommaText, 'TStrings.GetCommaText[0]');
  CallBack(Module, Data, @TStrings_SetCommaText, 'TStrings.SetCommaText[0]');
  CallBack(Module, Data, @TStrings_GetCount, 'TStrings.GetCount[0]');
  CallBack(Module, Data, @TStrings_GetDelimiter, 'TStrings.GetDelimiter[0]');
  CallBack(Module, Data, @TStrings_SetDelimiter, 'TStrings.SetDelimiter[0]');
  CallBack(Module, Data, @TStrings_GetDelimitedText, 'TStrings.GetDelimitedText[0]');
  CallBack(Module, Data, @TStrings_SetDelimitedText, 'TStrings.SetDelimitedText[0]');
  CallBack(Module, Data, @TStrings_GetName, 'TStrings.GetName[0]');
  CallBack(Module, Data, @TStrings_GetObject, 'TStrings.GetObject[0]');
  CallBack(Module, Data, @TStrings_PutObject, 'TStrings.PutObject[0]');
  CallBack(Module, Data, @TStrings_GetQuoteChar, 'TStrings.GetQuoteChar[0]');
  CallBack(Module, Data, @TStrings_SetQuoteChar, 'TStrings.SetQuoteChar[0]');
  CallBack(Module, Data, @TStrings_GetValue, 'TStrings.GetValue[0]');
  CallBack(Module, Data, @TStrings_SetValue, 'TStrings.SetValue[0]');
  CallBack(Module, Data, @TStrings_GetValueFromIndex, 'TStrings.GetValueFromIndex[0]');
  CallBack(Module, Data, @TStrings_SetValueFromIndex, 'TStrings.SetValueFromIndex[0]');
  CallBack(Module, Data, @TStrings_GetNameValueSeparator, 'TStrings.GetNameValueSeparator[0]');
  CallBack(Module, Data, @TStrings_SetNameValueSeparator, 'TStrings.SetNameValueSeparator[0]');
  CallBack(Module, Data, @TStrings_GetString, 'TStrings.GetString[0]');
  CallBack(Module, Data, @TStrings_PutString, 'TStrings.PutString[0]');
  CallBack(Module, Data, @TStrings_GetTextStr, 'TStrings.GetTextStr[0]');
  CallBack(Module, Data, @TStrings_SetTextStr, 'TStrings.SetTextStr[0]');
  CallBack(Module, Data, @TStrings_Add, 'TStrings.Add[0]');
  CallBack(Module, Data, @TStrings_AddObject, 'TStrings.AddObject[0]');
  CallBack(Module, Data, @TStrings_Append, 'TStrings.Append[0]');
  CallBack(Module, Data, @TStrings_AddStrings, 'TStrings.AddStrings[0]');
  CallBack(Module, Data, @TStrings_Assign, 'TStrings.Assign[0]');
  CallBack(Module, Data, @TStrings_BeginUpdate, 'TStrings.BeginUpdate[0]');
  CallBack(Module, Data, @TStrings_Clear, 'TStrings.Clear[0]');
  CallBack(Module, Data, @TStrings_Delete, 'TStrings.Delete[0]');
  CallBack(Module, Data, @TStrings_EndUpdate, 'TStrings.EndUpdate[0]');
  CallBack(Module, Data, @TStrings_Equals, 'TStrings.Equals[0]');
  CallBack(Module, Data, @TStrings_Exchange, 'TStrings.Exchange[0]');
  CallBack(Module, Data, @TStrings_GetText, 'TStrings.GetText[0]');
  CallBack(Module, Data, @TStrings_IndexOf, 'TStrings.IndexOf[0]');
  CallBack(Module, Data, @TStrings_IndexOfName, 'TStrings.IndexOfName[0]');
  CallBack(Module, Data, @TStrings_IndexOfObject, 'TStrings.IndexOfObject[0]');
  CallBack(Module, Data, @TStrings_Insert, 'TStrings.Insert[0]');
  CallBack(Module, Data, @TStrings_InsertObject, 'TStrings.InsertObject[0]');
  CallBack(Module, Data, @TStrings_LoadFromFile, 'TStrings.LoadFromFile[0]');
  CallBack(Module, Data, @TStrings_LoadFromStream, 'TStrings.LoadFromStream[0]');
  CallBack(Module, Data, @TStrings_Move, 'TStrings.Move[0]');
  CallBack(Module, Data, @TStrings_SaveToFile, 'TStrings.SaveToFile[0]');
  CallBack(Module, Data, @TStrings_SaveToStream, 'TStrings.SaveToStream[0]');
  CallBack(Module, Data, @TStrings_SetText, 'TStrings.SetText[0]');
end;

end.
