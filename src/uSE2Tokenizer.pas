unit uSE2Tokenizer;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2Consts, uSE2Reader, uSE2BaseTypes;

type       
  TSE2Tokenizer = class(TSE2Object)
  private
    FReader     : TSE2Reader;
    FToken      : TSE2Token;
    FTokenCache : array[0..255] of Char;
    FCachePos   : integer;
    FInlineDoc  : string;
  protected
    procedure SetReader(const value: TSE2Reader);
    procedure StepSpaces;

    procedure CacheToToken;
    procedure AppendCache;
    function  CompilerConditions: boolean;
    function  GetReservedToken(const value: string): TSE2TokenType;
  public
    constructor Create(const Reader: TSE2Reader); reintroduce;
    destructor Destroy; override;

    function NextToken: boolean;

    property InlineDoc : string       read FInlineDoc write FInlineDoc;
    property Token     : TSE2Token    read FToken;
    property Reader    : TSE2Reader   read FReader    write SetReader;
  end;

implementation

uses SysUtils;

{ TSE2Tokenizer }

type
  TSE2CharSet = {$IFDEF DELPHI2009UP}TSysCharSet{$ELSE}set of char{$ENDIF};

function CharInSet(C: char; const CharSet: TSE2CharSet): Boolean;
begin
  {$IFDEF DELPHI2009UP}
  Result := SysUtils.CharInSet(c, CharSet);
  {$ELSE}
  Result := c in CharSet;
  {$ENDIF}
end;

procedure TSE2Tokenizer.CacheToToken;
begin
  Token.Value := Copy(string(FTokenCache), 1, FCachePos);
end;

procedure TSE2Tokenizer.AppendCache;
begin
  Token.Value := Token.Value + Copy(string(FTokenCache), 1, FCachePos);
end;

function TSE2Tokenizer.CompilerConditions: boolean;
begin
  while not CharInSet(FReader.Symbol, [#0, '}']) do
    FReader.NextChar();

  result := FReader.Symbol <> #0;
end;

constructor TSE2Tokenizer.Create(const Reader: TSE2Reader);
begin
  inherited Create;
  Assert(Reader <> nil, 'The data reader can not be nil');
  FReader := Reader;

  FToken  := TSE2Token.Create;
end;

destructor TSE2Tokenizer.Destroy;
begin
  FreeAndNil(FToken);
  FreeAndNil(FReader);
  inherited;
end;

var
  TokenHash : array[TSE2TokenType] of integer;
  HashBuilt : boolean = False;

procedure BuildTokenHash;
var t: TSE2TokenType;
begin
  for t := sesNone to sesLastToken do
    TokenHash[t] := MakeHash(TSE2TokenString[t]);


  HashBuilt := True;
end;

function TSE2Tokenizer.GetReservedToken(
  const value: string): TSE2TokenType;
var iHash: integer;
begin
  if not HashBuilt then
     BuildTokenHash;

  iHash := MakeHash(value);
  for result := sesNone to sesLastToken do
    if TokenHash[result] = iHash then
      if TSE2TokenString[result] <> '' then
        if StringIdentical(TSE2TokenString[result], Value) then
          exit;

  result := sesUnknown;
end;

function TSE2Tokenizer.NextToken: boolean;
var TokenType    : TSE2TokenType;
    tempChar     : char;
    tempStr      : string;
    isInlineDoc  : boolean;
label StartPos;
begin
  isInlineDoc := False;
  StartPos :

  result := False;
  FToken.Reset;

  StepSpaces;

  case FReader.Symbol of
  'a'..'z', 'A'..'Z', '_' :
      begin
        Token.Value := '';
        FCachePos := 0;
        while True do
        begin
          FTokenCache[FCachePos] := FReader.Symbol;
          FCachePos := FCachePos + 1;
          //Token.Value := Token.Value + FReader.Symbol;
          if not CharInSet(FReader.NextChar(True), ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
             break;
          FReader.NextChar;

          if FCachePos > High(FTokenCache) then
          begin
            AppendCache;
            FCachePos := 0;
          end;
        end;

        if FCachePos > 0 then
           AppendCache;
        result := True;
        TokenType := GetReservedToken(Token.Value);
        if TokenType = sesUnknown then
           Token.AType := sesIdentifier
        else
           Token.AType := TokenType;
        exit;
      end;
  { One - Char - Symbols }
  '+', '-', '*', '/', '=', '<', '>', '(', ')',
  ',', '.', ';', '[', ']', '@' :
      begin
        if CharInSet(FReader.Symbol, ['(']) then
        begin
          isInlineDoc := False;
          TempChar := FReader.NextChar(True);
          if CharInSet(TempChar, ['*']) then
          begin
            FReader.NextChar();
            while True do
            begin
              repeat
                TempChar := FReader.NextChar()
              until CharInSet(TempChar, ['*', #0]);
              if TempChar = #0 then
              begin
                result := False;
                exit;
              end;
              TempChar := FReader.NextChar();
              if TempChar = ')' then
                 goto StartPos;
            end;
          end;
        end;
        if CharInSet(FReader.Symbol, ['/']) then
        begin
          TempChar := FReader.NextChar(True);
          if CharInSet(TempChar, ['/']) then
          begin
            FReader.NextChar();
            if FReader.NextChar(True) = '/' then
            begin
              if not isInlineDoc then
                 FInlineDoc := '';
              isInlineDoc := True;
              FReader.NextChar();
            end;


            // Line comment
            while not FReader.LineIncreased do
            begin
              if FReader.NextChar() = #0 then
                exit;
              if isInlineDoc then
                if not FReader.LineIncreased then
                   FInlineDoc := FInlineDoc + FReader.Symbol;
            end;

            if isInlineDoc then
               FInlineDoc := FInlineDoc + #13#10;

            goto StartPos;
          end;
        end;
        if FReader.Symbol = '.' then
        begin
          tempChar := FReader.NextChar(True);
          if tempChar = '.' then
          begin
            FReader.NextChar();
            Token.Value := '..';
            Token.AType := sesDotDot;
            result := True;
            exit;
          end;
        end;
        if CharInSet(FReader.Symbol, ['>', '<']) then
        begin
          tempStr  := FReader.Symbol;
          TempChar := FReader.NextChar(True);
          if TempChar = '=' then
          begin
            FReader.NextChar();
            TokenType := GetReservedToken(tempStr + tempChar);
            if TokenType <> sesUnknown then
                begin
                  Token.Value   := FReader.Symbol + TempChar;
                  Token.AType   := TokenType;
                  result        := True;
                  exit;
                end;
          end else
          if (FReader.Symbol = '<') and (TempChar = '>') then
          begin
            FReader.NextChar();
            TokenType := sesUnEqual;
            //TokenType := GetReservedToken('<>');
                begin
                  Token.Value   := '<>';
                  Token.AType   := TokenType;
                  result        := True;
                  exit;
                end;
          end;
        end;
        TokenType := GetReservedToken(FReader.Symbol);
        if TokenType <> sesUnknown then
            begin
              Token.Value   := FReader.Symbol;
              Token.AType   := TokenType;
              result        := True;
              exit;
            end;
      end;
  { String contents }
  '#' :
      begin
        Token.Value  := '';
        Token.AType  := sesNone;
        result       := True;
        repeat
          if not CharInSet(FReader.NextChar(), ['0'..'9']) then
             exit;

          Token.AType := sesString;
          tempStr     := FReader.Symbol;
          while CharInSet(FReader.NextChar(True), ['0'..'9']) do
          begin
            FReader.NextChar();
            tempStr := tempStr + FReader.Symbol;
          end;
          Token.Value := Token.Value + Chr(StrToInt(tempStr));

          if CharInSet(FReader.NextChar(True), ['#']) then
          begin
            FReader.NextChar(False);
          end;
        until not CharInSet(FReader.Symbol, ['#']);
      end;
  #39 :
      begin
        Token.Value := '';
        FCachePos   := 0;
        repeat
          FReader.NextChar();
          if FReader.Symbol = #0 then
             break;

          if (FReader.Symbol <> #39) then
          begin
             // Token.Value := Token.Value + FReader.Symbol
             FTokenCache[FCachePos] := FReader.Symbol;
             FCachePos := FCachePos + 1;
             if FCachePos > High(FTokenCache) then
             begin
               AppendCache;
               FCachePos := 0;
             end;
          end else
          begin
            TempChar := FReader.NextChar(True);
            if TempChar = #39 then
            begin
              FTokenCache[FCachePos] := #39;
              FReader.NextChar();
              FReader.Symbol := #1;

              FCachePos := FCachePos + 1;
              if FCachePos > High(FTokenCache) then
              begin
                AppendCache;
                FCachePos := 0;
              end;
            end;
          end;
        until (FReader.Symbol = #39) or (FReader.Symbol = #0);
        if FCachePos > 0 then
           AppendCache;
        Token.AType   := sesString;
        result        := True;
        exit;
      end;
  { Make constant }
  ':' :
      begin
        TempChar := FReader.NextChar(True);
        if TempChar = '=' then
        begin
          FReader.NextChar();
          //TokenType := GetReservedToken(':=');
          TokenType := sesBecomes;
          //if TokenType <> sesUnknown then
              begin
                Token.Value   := ':=';
                Token.AType   := TokenType;
                result := True;
                exit;
              end;
        end else
        begin
          //TokenType := GetReservedToken(':');
          TokenType := sesDoublePoint;
          //if TokenType <> sesUnknown then
              begin
                Token.Value   := ':';
                Token.AType   := TokenType;
                result := True;
                exit;
              end;
          Token.AType   := sesUnknown;
          result        := False;
          exit;
        end;
      end;
  { Numbers }
  '0'..'9', '$' :
      begin
        FCachePos := 0;
        if FReader.Symbol = '$' then
        begin
          while True do
          begin
            FTokenCache[FCachePos] := FReader.Symbol;
            FCachePos := FCachePos + 1;
            //Token.Value  := Token.Value  + FReader.Symbol;
            if not CharInSet(FReader.NextChar(True), ['0'..'9', 'a'..'f', 'A'..'F']) then
               break;
            FReader.NextChar();
          end;
          result := True;
          CacheToToken;
          if TSE2Converter.IsInteger(Token.Value) then
             Token.AType := sesInteger
          else
             Token.AType := sesUnknown;
          exit;
        end else
        begin
          Token.AType   := sesInteger;
          repeat
            //Token.Value  := Token.Value  + FReader.Symbol;
            FTokenCache[FCachePos] := FReader.Symbol;
            FCachePos := FCachePos + 1;
            FReader.NextChar();
            if (FReader.Symbol = '.') or (FReader.Symbol = 'E') or (FReader.Symbol = 'e') then
            begin
              if (FReader.Symbol = '.') and (not CharInSet(FReader.NextChar(True), ['0'..'9'])) then
              begin
                CacheToToken;
                if not TSE2Converter.IsInteger(Token.Value) then
                begin
                   Token.AType := sesUnknown;
                   exit;
                end else
                   break;
              end else
              begin
                Token.AType := sesFloat;
                if CharInSet(FReader.Symbol, ['e', 'E']) then
                begin
                  TempChar := FReader.NextChar(True);
                  if not CharInSet(TempChar, ['0'..'9', '-']) then
                  begin
                    result        := True;
                    Token.AType   := sesUnknown;
                    exit;
                  end;
                  if CharInSet(TempChar, ['-']) then
                  begin
                    FTokenCache[FCachePos] := 'E';
                    FTokenCache[FCachePos + 1] := '-';
                    FCachePos := FCachePos + 2;
                    FReader.NextChar();
                    FReader.NextChar();
                  end;
                end;
              end;
            end;
          until not CharInSet(FReader.Symbol, ['0'..'9', '.', 'e', 'E']);
          FReader.Position := FReader.Position - 1;
          result := True;
          CacheToToken;
          if Token.AType = sesInteger then
          begin
            if not TSE2Converter.IsInteger(Token.Value) then
               Token.AType := sesUnknown;
          end else
          begin
            if not TSE2Converter.IsFloat(Token.Value) then
               Token.AType := sesUnknown;
          end;
          exit;
        end;
      end;
  { Comments }
  '{' :
      begin
        isInlineDoc  := False;
        if FReader.NextChar(True) = '$' then
        begin
          if CompilerConditions() then
             goto StartPos
          else
          begin
            Token.AType := sesNone;
            result      := False;
            exit;
          end;
        end else
        begin
          while not CharInSet(FReader.Symbol, ['}', #0]) do
          begin
            FReader.NextChar();
            if FReader.Symbol = #0 then
               exit;
          end;
          goto StartPos;
        end;
      end;
  else
      begin
        if FReader.Symbol >= #32 then
        begin
          Token.AType := sesUnknown;
          Token.Value := FReader.Symbol;
          result      := True;
        end;
      end;
  end;

end;

procedure TSE2Tokenizer.SetReader(const value: TSE2Reader);
begin
  FreeAndNil(FReader);
  FReader := value;
end;

procedure TSE2Tokenizer.StepSpaces;
begin
  while CharInSet(FReader.NextChar(), [#1..#32]) do
end;

end.
