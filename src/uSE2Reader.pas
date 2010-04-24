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

procedure TSE2StreamReader.SetStream(const Stream: TStream);
begin
  FreeAndNil(FStream);
  FStream := Stream;
end;

end.
