unit uSE2Compiler;

{$INCLUDE ScriptEngine.inc}

interface

uses
  Classes, uSE2BaseTypes, uSE2Types, uSE2Consts, uSE2Tokenizer, uSE2Reader,
  uSE2Parser, uSE2Errors, uSE2SystemUnit, uSE2Linker, uSE2PEData,
  uSE2UnitManager, uSE2UnitCacheMngr, uSE2UnitCache;

type
  TSE2GetFileReader   = procedure(Sender: TObject; const Name: string; const Readers: TList) of object;

  TSE2Compiler = class(TSE2Object)
  private
    FUnitList        : TSE2BaseTypeList;
    FUnitCache       : TSE2UnitCacheMngr;
    FUseUnitCache    : boolean;
    FCompiledUnits   : TStringList;
    FLines           : integer;

    // Events
    FGetFileReader   : TSE2GetFileReader;
    FCompilerError   : TSE2ErrorEvent;
  protected
    // Parser Events
    procedure ParserBeforeParse(Sender: TObject; const UnitName: string; var UnitData: TSE2Unit);
    procedure ParserUnitRequest(Sender: TObject; const UnitName: string; var UnitData: TSE2Unit);
    procedure ParserError(Sender: TObject; ErrorType: TSE2ErrorType; ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: integer; UserData: TObject);


    // Event Raise
    procedure GetUnitReader(const UnitName: string; var Readers: TList);

    function  CreateParser(Tokenizer: TSE2Tokenizer): TSE2Parser;
    function  DoCompile(const Tokenizer: TSE2Tokenizer; CallBack: TSE2CompileCallBack = nil; CompilePos: integer = -1; ExpectedName: string = ''): TSE2Unit;
    function  MakeSystemUnit: boolean;
    function  CompileUnits(const UnitName: string; var count: integer): boolean;

    procedure SetUseUnitCache(value: boolean);
  public
    constructor Create; override;
    destructor Destroy; override;

    function  Compile(const Tokenizer: TSE2Tokenizer): TSE2PE; overload;
    function  Compile(const Reader: TSE2Reader): TSE2PE; overload;
    function  Compile(const ScriptSource: string): TSE2PE; overload;
    procedure CodeComplete(const Source: string; CallBack: TSE2CompileCallBack; Position: integer); overload;
    procedure CodeComplete(const Reader: TSE2ReaderList; CallBack: TSE2CompileCallBack; Position: integer); overload;

    property UnitList         : TSE2BaseTypeList   read FUnitList;
    property UseUnitCache     : boolean            read FUseUnitCache      write SetUseUnitCache;
    property UnitCache        : TSE2UnitCacheMngr  read FUnitCache         write FUnitCache;
    property CompiledLines    : integer            read FLines;

    // Events
    property OnGetFile        : TSE2GetFileReader  read FGetFileReader     write FGetFileReader;
    property OnCompilerError  : TSE2ErrorEvent     read FCompilerError     write FCompilerError;
  end;

implementation

{ TSE2Compiler }

function TSE2Compiler.Compile(const Tokenizer: TSE2Tokenizer): TSE2PE;
var Linker: TSE2Linker;
begin
  result := nil;
  FCompiledUnits.Clear;
  FLines := 0;

  if FUnitCache <> nil then
     FUnitCache.CheckForChanges;

  if DoCompile(Tokenizer) <> nil then
  begin
    Linker := TSE2Linker.Create(FUnitList);
    try
      result := Linker.LinkProgram;
    finally
      Linker.Free;
    end;
  end;
end;

function TSE2Compiler.Compile(const ScriptSource: string): TSE2PE;
begin
  result := Compile(TSE2StringReader.Create(ScriptSource));
end;

constructor TSE2Compiler.Create;
begin
  inherited;
  FUnitList     := TSE2BaseTypeList.Create;
  FUseUnitCache := True;
  FCompiledUnits := TStringList.Create;
  FCompiledUnits.CaseSensitive := False;
  FCompiledUnits.Sorted := True;
end;

function TSE2Compiler.CreateParser(Tokenizer: TSE2Tokenizer): TSE2Parser;
begin
  result := TSE2Parser.Create(Tokenizer);
  result.OnUnitRequest := ParserUnitRequest;
  result.OnError       := ParserError;
  result.OnBeforeParse := ParserBeforeParse;
end;

destructor TSE2Compiler.Destroy;
begin
  FCompiledUnits.Free;
  FUnitList.Free;
  inherited;
end;

function TSE2Compiler.CompileUnits(const UnitName: string; var count: integer): boolean;
var i, j        : integer;
    Token       : TSE2Tokenizer;
    src         : string;
    FUnit       : TSE2Unit;
    CanUseCache : boolean;
    Cache       : TSE2UnitCache;
    Readers     : TList;
begin
  // 1st Check if any unit can not be restored from the unit cache

  if FCompiledUnits.IndexOf(UnitName) > -1 then
  begin
     ParserError(nil, petError, UnitName, 'Unit cross reference to unit "'+UnitName+'"', 0, 0, nil);
     result := False;
     exit;
  end
  else
    FCompiledUnits.Add(UnitName);

  GetUnitReader(UnitName, Readers);
  try
    if FUseUnitCache and (Readers.Count = 0) and (FUnitCache <> nil) then
    begin
      Cache       := FUnitCache.GetCache(UnitName);
      CanUseCache := Cache <> nil;
      if CanUseCache then
      begin
        for i:=0 to TSE2UnitManager.Instance.Count-1 do
          for j:=0 to TSE2UnitManager.Instance.Units[i].Modules-1 do
          begin
            if StringIdentical(TSE2UnitManager.Instance.Units[i].UnitNames[j], UnitName) then
            begin
              CanUseCache := CanUseCache and
                             (
                              (TSE2UnitManager.Instance.Units[i].CanCacheSource(j)) and
                              (TSE2UnitManager.Instance.Units[i].LastChangeTime(j) < Cache.CacheTime)
                             );
              if not CanUseCache then
                 break;
            end;
          end;

      end;

      if CanUseCache then
      begin
        FUnit := FUnitCache.GetUnit(Cache, FUnitList);
        if FUnit <> nil then
        begin
          FUnitList.Add(FUnit);
          result := True;
          Count  := 1;
          exit;
        end;
      end;
    end;

    if FUseUnitCache then
      if FUnitCache <> nil then
        FUnitCache.ClearCache(UnitName);

    result := True;
    count  := 0;
    FUnit  := nil;
    for i:=0 to TSE2UnitManager.Instance.Count-1 do
      for j:=0 to TSE2UnitManager.Instance.Units[i].Modules-1 do
      begin
        if StringIdentical(TSE2UnitManager.Instance.Units[i].UnitNames[j], UnitName) then
          if not TSE2UnitManager.Instance.Units[i].IsExtender[j] then
          begin
            TSE2UnitManager.Instance.Units[i].GetUnitSource(j, src);
            Token := TSE2Tokenizer.Create(TSE2StringReader.Create(src));
            FUnit := DoCompile(Token, nil, -1, UnitName);
            result := result and (FUnit <> nil);
            count := count + 1;
            if not result then
               exit;
          end;
      end;

    for i:=0 to TSE2UnitManager.Instance.Count-1 do
      for j:=0 to TSE2UnitManager.Instance.Units[i].Modules-1 do
      begin
        if StringIdentical(TSE2UnitManager.Instance.Units[i].UnitNames[j], UnitName) then
          if TSE2UnitManager.Instance.Units[i].IsExtender[j] then
          begin
            TSE2UnitManager.Instance.Units[i].GetUnitSource(j, src);
            Token := TSE2Tokenizer.Create(TSE2StringReader.Create(src));
            FUnit := DoCompile(Token, nil, -1, UnitName);
            result := result and (FUnit <> nil);
            count := count + 1;
            if not result then
               exit;
          end;
      end;

    if (Readers.Count > 0) and (result) then
    begin
      for i:=0 to Readers.Count-1 do
      begin
        Token := TSE2Tokenizer.Create(TSE2Reader(Readers[0]));
        Readers.Delete(0);

        FUnit  := DoCompile(Token, nil, -1, UnitName);
        result := result and (FUnit <> nil);
        count  := count + 1;
        if not result then
           break;
      end;
    end;

    if result then
      if FUseUnitCache then
        if FUnitCache <> nil then
         FUnitCache.Add(FUnit);
    result := result and (count > 0);
  finally
    for i:=0 to Readers.Count-1 do
      TSE2Reader(Readers[i]).Free;
    Readers.Free;
  end;
end;

function TSE2Compiler.MakeSystemUnit: boolean;
var SystemUnit : TSE2Unit;
    c          : integer;
begin
  FUnitList.Clear;
  if FUseUnitCache and (FUnitCache <> nil) then
    if FUnitCache.HasUnit(C_SE2SystemUnitName) then
    begin
      SystemUnit := FUnitCache.GetUnit(C_SE2SystemUnitName, FUnitList);
      if SystemUnit <> nil then
      begin
        FUnitList.Add(SystemUnit);
        result := True;
        exit;
      end;
    end;

  SystemUnit := TSE2Unit.Create;
  TSE2SystemUnit.FillSystemUnit(SystemUnit);
  FUnitList.Add(SystemUnit);

  if not CompileUnits(C_SE2SystemUnitName, c) then
  begin
    result := False;
    ParserError(Self, petError, '', 'CRITICAL ERROR: System unit could not be compiled', 0, 0, nil);
  end else
    result := True;
end;

function TSE2Compiler.DoCompile(const Tokenizer: TSE2Tokenizer; CallBack: TSE2CompileCallBack = nil; CompilePos: integer = -1; ExpectedName: string = ''): TSE2Unit;
var FParser    : TSE2Parser;
begin
  result  := nil;
  if FUnitList.Count = 0 then
  begin
    if not MakeSystemUnit then
    begin
      Tokenizer.Free;
      exit;
    end;
  end;

  FParser := CreateParser(Tokenizer);
  try
    if (Assigned(CallBack) and (CompilePos > -1)) then
       FParser.RegisterCallBack(CompilePos, CallBack);

    FParser.ExpectedName := ExpectedName;
    if not FParser.Compile then
       exit;

    FLines := FLines + Tokenizer.Reader.Line;
    result := FParser.AUnit;
    if FParser.OwnsUnit then
       FUnitList.Add(FParser.AUnit);
    FParser.OwnsUnit := False;
  finally
    FParser.Free;
  end;
end;

procedure TSE2Compiler.GetUnitReader(const UnitName: string; var Readers: TList);
begin
  Readers := TList.Create;

  if not Assigned(FGetFileReader) then
     exit;

  FGetFileReader(Self, UnitName, Readers);
end;

procedure TSE2Compiler.ParserBeforeParse(Sender: TObject;
  const UnitName: string; var UnitData: TSE2Unit);
var Filter: TSE2BaseTypeFilter;
begin
  Filter := TSE2BaseTypeFilter.Create([TSE2Unit]);
  try
    UnitData := TSE2Unit(FUnitList.FindItem(UnitName, '', nil, nil, Filter, CAllVisibilities));
  finally
    Filter.Free;
  end;
end;

procedure TSE2Compiler.ParserError(Sender: TObject;
  ErrorType: TSE2ErrorType; ErrorUnit, ErrorText: string; ErrorPos,
  ErrorLine: integer; UserData: TObject);
begin
  if Assigned(FCompilerError) then
  begin
    FCompilerError(Self, ErrorType, ErrorUnit, ErrorText, ErrorPos, ErrorLine, UserData);
  end;
end;

procedure TSE2Compiler.ParserUnitRequest(Sender: TObject;
  const UnitName: string; var UnitData: TSE2Unit);
var Entry  : TSE2BaseType;
    count  : integer;
begin
  UnitData := nil;

  Entry := FUnitList.FindItem(UnitName);
  if Entry = nil then
  begin
    if not CompileUnits(UnitName, count) then
    begin
      if count = 0 then
         ParserError(nil, petError, '', 'Could not find the unit "'+UnitName+'"', 0, 0, nil)
      else
         ParserError(nil, petError, UnitName, 'Could not compile the unit "'+UnitName+'"', 0, 0, nil);
      exit;
    end;       
    Entry := FUnitList.FindItem(UnitName);
  end;

  UnitData := TSE2Unit(Entry);
end;

procedure TSE2Compiler.CodeComplete(const Reader: TSE2ReaderList;
  CallBack: TSE2CompileCallBack; Position: integer);
var i: integer;
begin
  if Reader.Count = 0 then
     exit;

  Reader.OwnsObjects := False;
  if FUnitCache <> nil then
     FUnitCache.CheckForChanges;

  for i:=0 to Reader.Count-2 do
     DoCompile(TSE2Tokenizer.Create(Reader[i]));

  DoCompile(TSE2Tokenizer.Create(Reader.Last), CallBack, Position);
  Reader.Clear;
end;

procedure TSE2Compiler.CodeComplete(const Source: string;
  CallBack: TSE2CompileCallBack; Position: integer);
begin            
  if FUnitCache <> nil then
     FUnitCache.CheckForChanges;

  DoCompile(TSE2Tokenizer.Create(TSE2StringReader.Create(Source)), CallBack, Position);
end;

procedure TSE2Compiler.SetUseUnitCache(value: boolean);
begin
  if value <> FUseUnitCache then
  begin
    FUseUnitCache := value;
    if not FUseUnitCache then
      if FUnitCache <> nil then
        FUnitCache.Clear;
  end;
end;

function TSE2Compiler.Compile(const Reader: TSE2Reader): TSE2PE;
begin
  result := Compile(TSE2Tokenizer.Create(Reader));
end;

end.
