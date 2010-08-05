unit uSE2Reader;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2Consts, uSE2BaseTypes;

type
  TSE2Reader = class(TSE2Object)
  protected
    FPosition       : integer;
    FLine           : integer;
    FLineIncreased  : boolean;
    FSymbol         : char;
  protected
    function  GetNextChar(): char; virtual; abstract;
    procedure SetPosition(const value: integer); virtual; abstract;
    procedure SetLine(const value: integer); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;  
    procedure AfterConstruction; override;

    procedure ResetReader; virtual;
    function  NextChar(ResetPosition: boolean = False): char; 

    property Symbol        : char      read FSymbol         write FSymbol;
    property LineIncreased : boolean   read FLineIncreased  write FLineIncreased;
    property Position      : integer   read FPosition       write SetPosition;
    property Line          : integer   read FLine           write SetLine;
  end;

  TSE2ReaderList = class(TObject)
  private
    FList        : TList;
    FOwnsObjects : boolean;
  protected
    function  GetCount: integer;
    function  GetItem(index: integer): TSE2Reader;
    function  GetFirst: TSE2Reader;
    function  GetLast: TSE2Reader;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function  Add(Reader: TSE2Reader): boolean; overload;
    procedure Add(const Source: string); overload;
    procedure Add(const Stream: TStream); overload;

    function Delete(index: integer): boolean; overload;
    function Delete(Reader: TSE2Reader): boolean; overload;
    function IndexOf(Reader: TSE2Reader): integer;

    property Items[index: integer]: TSE2Reader read GetItem; default;
    property Count                : integer    read GetCount;
    property First                : TSE2Reader read GetFirst;
    property Last                 : TSE2Reader read GetLast;
    property OwnsObjects          : boolean    read FOwnsObjects  write FOwnsObjects;
  end;

  TSE2StringReader = class(TSE2Reader)
  protected
    FScriptSource      : string;
    FScriptLength      : integer;
    procedure SetSource(const value: string);    
    function  GetNextChar(): char; override;
    procedure SetPosition(const value: integer); override;
  public
    constructor Create; overload; override;
    constructor Create(const Source: string; aData: TObject = nil); reintroduce; overload;
    destructor Destroy; override;

    property ScriptSource: string   read FScriptSource    write SetSource;
    property ScriptLength: integer  read FScriptLength;
  end;

  TSE2StreamReader = class(TSE2Reader)
  protected
    FStream            : TStream;
    procedure SetStream(const Stream: TStream);
    function  GetNextChar(): char; override;
    procedure SetPosition(const value: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Stream: TStream   read FStream     write SetStream;
  end;
  

implementation

uses SysUtils;

{ TSE2BaseReader }

procedure TSE2Reader.AfterConstruction;
begin
  inherited;
  ResetReader;
end;

constructor TSE2Reader.Create;
begin
  inherited;
end;

destructor TSE2Reader.Destroy;
begin
  inherited;
end;

function TSE2Reader.NextChar(ResetPosition: boolean): char;
var BufferPos  : integer;
    BufferChar : char;
begin
  FLineIncreased := False;
  BufferPos      := FPosition;
  BufferChar     := FSymbol;

  result         := GetNextChar;

  if ResetPosition then
  begin
    Position := BufferPos;
    if FLineIncreased then
       FLine := FLine - 1;
    FLineIncreased := False;
    FSymbol := BufferChar;
  end else
    FSymbol := result;
end;

procedure TSE2Reader.ResetReader;
begin
  FPosition := 1;
  FLine     := 1;
end;

procedure TSE2Reader.SetLine(const value: integer);
begin
  FLine := value;
end;

{ TSE2StringReader }

constructor TSE2StringReader.Create;
begin
  inherited;
end;

constructor TSE2StringReader.Create(const Source: string; aData: TObject = nil);
begin
  inherited Create;
  Self.ScriptSource := Source;
  Self.Data         := aData;
  Self.OwnsObj      := False;
end;

destructor TSE2StringReader.Destroy;
begin
  FScriptSource := '';
  inherited;
end;

function TSE2StringReader.GetNextChar: char;
begin
  FLineIncreased := False;
  result         := #0;
  if Position > FScriptLength then
     exit;

  result  := FScriptSource[FPosition];
  inc(FPosition);

  if Result = #13 then
  begin
    FLine          := FLine + 1;
    FLineIncreased := True;
  end;
end;

procedure TSE2StringReader.SetPosition(const value: integer);
begin
  {$IFNDEF SEII_FPC}
  inherited;
  {$ENDIF}
  FPosition := value;
end;

procedure TSE2StringReader.SetSource(const value: string);
begin
  ResetReader;
  FScriptSource := value;
  FScriptLength := length(value);
end;

{ TSE2StreamReader }

constructor TSE2StreamReader.Create;
begin
  inherited;
  FStream := nil;
end;

destructor TSE2StreamReader.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

function TSE2StreamReader.GetNextChar: char;
begin             
  FLineIncreased := False;
  result         := #0;

  if FStream.Size < Position then
     exit;

  if FStream.Position <> FPosition then
     FStream.Position := FPosition;

  FStream.Read(result, SizeOf(result));
  inc(FPosition);

  if Result = #13 then
  begin
    FLine          := FLine + 1;
    FLineIncreased := True;
  end;          
end;

procedure TSE2StreamReader.SetPosition(const value: Integer);
begin
  inherited;
  FStream.Position := value;
end;

procedure TSE2StreamReader.SetStream(const Stream: TStream);
begin
  FreeAndNil(FStream);
  FStream := Stream;
end;

{ TSE2ReaderList }

function TSE2ReaderList.Add(Reader: TSE2Reader): boolean;
begin
  result := IndexOf(Reader) < 0;
  if result then
     FList.Add(Reader);
end;

procedure TSE2ReaderList.Add(const Source: string);
begin
  FList.Add(TSE2StringReader.Create(Source));
end;

procedure TSE2ReaderList.Add(const Stream: TStream);
var Reader: TSE2StreamReader;
begin
  Reader := TSE2StreamReader.Create;
  Reader.Stream := Stream;
  FList.Add(Reader);
end;

procedure TSE2ReaderList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Delete(i);
end;

constructor TSE2ReaderList.Create;
begin
  inherited;
  FList := TList.Create;
  FOwnsObjects := True;
end;

function TSE2ReaderList.Delete(index: integer): boolean;
var reader: TSE2Reader;
begin
  if (index < 0) or (index >= FList.Count) then
     result := False
  else
  begin
    reader := FList[index];
    FList.Delete(index);
    if FOwnsObjects then
       reader.Free;
    result := True;
  end;
end;

function TSE2ReaderList.Delete(Reader: TSE2Reader): boolean;
begin
  result := Delete(IndexOf(Reader));
end;

destructor TSE2ReaderList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2ReaderList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2ReaderList.GetFirst: TSE2Reader;
begin
  if (FList.Count > 0) then
     result := FList.List[0]
  else
     result := nil;
end;

function TSE2ReaderList.GetItem(index: integer): TSE2Reader;
begin
  if (index < 0) or (index > FList.Count) then
     result := nil
  else
     result := FList.List[index];
end;

function TSE2ReaderList.GetLast: TSE2Reader;
begin
  if (FList.Count > 0) then
     result := FList.List[FList.Count-1]
  else
     result := nil;
end;

function TSE2ReaderList.IndexOf(Reader: TSE2Reader): integer;
begin
  result := FList.IndexOf(Reader);
end;

end.
