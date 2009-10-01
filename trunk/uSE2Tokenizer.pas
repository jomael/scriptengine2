unit uSE2Tokenizer;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2Consts, uSE2Reader, uSE2BaseTypes;

type       
  TSE2Tokenizer = class(TSE2Object)
  private
    FReader : TSE2Reader;
    FToken  : TSE2Token;
  protected
    procedure SetReader(const value: TSE2Reader);
    procedure StepSpaces;

    function  CompilerConditions: boolean;
  public
    constructor Create(const Reader: TSE2Reader); reintroduce;
    destructor Destroy; override;

    function NextToken: boolean;

    property Token    : TSE2Token    read FToken;
    property Reader   : TSE2Reader   read FReader   write SetReader;     
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

function TSE2Tokenizer.CompilerConditions: boolean;
begin
  while not CharInSet(FReader.NextChar(False), [#0, '}']) do
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

function TSE2Tokenizer.NextToken: boolean;
var TokenType : TSE2TokenType;
    tempChar  : char;
    tempStr   : string;
label StartPos;
begin
  StartPos :

  result := False;
  FToken.Reset;

  StepSpaces;

  case FReader.Symbol of
  'a'..'z', 'A'..'Z', '_' :
      begin
        while True do
        begin
          Token.Value := Token.Value + FReader.Symbol;
          if not CharInSet(FReader.NextChar(True), ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
             break;
          FReader.NextChar;
        end;

        for TokenType := sesNone to sesLastToken do
          if TSE2TokenString[TokenType] <> '' then
            if StringIdentical(TSE2TokenString[TokenType], Token.Value) then
            begin
              Token.AType := TokenType;
              result      := True;
              exit;
            end;

        Token.AType   := sesIdentifier;
        result        := True;
        exit;
      end;
  { One - Char - Symbols }
  '+', '-', '*', '/', '=', '<', '>', '(', ')',
  ',', '.', ';', '[', ']', '@' :
      begin
        if CharInSet(FReader.Symbol, ['(']) then
        begin
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
            // Line comment
            while not FReader.LineIncreased do
              if FReader.NextChar() = #0 then
                exit;

            goto StartPos;
          end;
        end;
        if CharInSet(FReader.Symbol, ['>', '<']) then
        begin
          tempStr  := FReader.Symbol;
          TempChar := FReader.NextChar(True);
          if TempChar = '=' then
          begin
            FReader.NextChar();
            for TokenType := sesNone to sesLastToken do
              if TSE2TokenString[TokenType] <> '' then
                if TSE2TokenString[TokenType] = tempStr + TempChar then
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
            for TokenType := sesNone to sesLastToken do
              if TSE2TokenString[TokenType] <> '' then
                if TSE2TokenString[TokenType] = '<>' then
                begin
                  Token.Value   := FReader.Symbol + TempChar;
                  Token.AType   := TokenType;
                  result        := True;
                  exit;
                end;
          end;
        end;
        for TokenType := sesNone to sesLastToken do
          if TSE2TokenString[TokenType] <> '' then
            if TSE2TokenString[TokenType] = FReader.Symbol then
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
        repeat
          FReader.NextChar();
          if FReader.Symbol = #0 then
             break;

          if (FReader.Symbol <> #39) then
             Token.Value := Token.Value + FReader.Symbol
          else
          begin
            TempChar := FReader.NextChar(True);
            if TempChar = #39 then
            begin
              Token.Value := Token.Value + #39;
              FReader.NextChar();
              FReader.Symbol := #1;
            end;
          end;
        until (FReader.Symbol = #39) or (FReader.Symbol = #0);
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
          for TokenType := sesNone to sesLastToken do
            if TSE2TokenString[TokenType] <> '' then
              if TSE2TokenString[TokenType] = ':=' then
              begin
                Token.Value   := ':=';
                Token.AType   := TokenType;
                result := True;
                exit;
              end;
        end else
        begin
          for TokenType := sesNone to sesLastToken do
            if TSE2TokenString[TokenType] <> '' then
              if TSE2TokenString[TokenType] = ':' then
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
        if FReader.Symbol = '$' then
        begin
          while True do
          begin
            Token.Value  := Token.Value  + FReader.Symbol;
            if not CharInSet(FReader.NextChar(True), ['0'..'9', 'a'..'f', 'A'..'F']) then
               break;
            FReader.NextChar();
          end;
          result := True;
          if TSE2Converter.IsInteger(Token.Value) then
             Token.AType := sesInteger
          else
             Token.AType := sesUnknown;
          exit;
        end else
        begin
          Token.AType   := sesInteger;
          repeat
            Token.Value  := Token.Value  + FReader.Symbol;
            FReader.NextChar();
            if (FReader.Symbol = '.') or (FReader.Symbol = 'E') or (FReader.Symbol = 'e') then
            begin
              if (FReader.Symbol = '.') and (not CharInSet(FReader.NextChar(True), ['0'..'9'])) then
              begin
                if not TSE2Converter.IsInteger(Token.Value) then
                   Token.AType := sesUnknown;
                exit;
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
                    Token.Value  := Token.Value + 'E-';
                    FReader.NextChar();
                    FReader.NextChar();
                  end;
                end;
              end;
            end;
          until not CharInSet(FReader.Symbol, ['0'..'9', '.', 'e', 'E']);
          FReader.Position := FReader.Position - 1;
          result := True;
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
