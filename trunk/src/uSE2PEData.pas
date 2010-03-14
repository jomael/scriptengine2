unit uSE2PEData;
                             
{$INCLUDE ScriptEngine.inc}  

interface

uses
  Classes, uSE2BaseTypes, uSE2OpCode, uSE2Consts;

type
  TSE2OpCodes = class(TSE2List)
  private
    //FList      : TSE2List;
    FPosition  : integer;                     
  protected
    function GetOpCode: PSE2OpDefault;
    function GetItem(index: integer): PSE2OpDefault;

    procedure Remove(index: integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure Clear; override;
    procedure Add(OpCode: PSE2OpDefault);
    function  Delete(index: integer): boolean;
    procedure Reset;
    function  Next: boolean;

    property  Items[index: integer]: PSE2OpDefault read GetItem;  default;
    property  OpCode               : PSE2OpDefault read GetOpCode;

    property  Position             : integer       read FPosition       write FPosition;
  end;

  TSE2StringList = class(TSE2Object)
  private
    FList : TSE2List;
  protected
    function  GetCount: integer;
    function  GetItem(index: integer): string;
    procedure Remove(index: integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure Clear;
    procedure Add(const s: string);
    function  Delete(index: integer): boolean;

    property  Items[index: integer]: string  read GetItem; default;
    property  Count                : integer read GetCount;
  end;

  TSE2MetaType = (mtMethod, mtClass, mtRecord, mtArray);

  PSE2RTTIEntry = ^TSE2RTTIEntry;
  TSE2RTTIEntry = record
    Offset : integer;
    Size   : integer;
    AType  : TSE2TypeIdent;
  end;

  TSE2RTTIList = class(TSE2Object)
  private
    FList  : TList;
  protected
    function GetItem(index: integer): PSE2RTTIEntry;
    function GetCount: integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Add(AType: TSE2TypeIdent; Offset, Size: integer);
    procedure Clear;

    function  FindSize(AType: TSE2TypeIdent; Offset: integer): integer;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property Items[index: integer]: PSE2RTTIEntry read GetItem; default;
    property Count                : integer       read GetCount;
  end;

  TSE2DynMethodList = class(TObject)
  private
    FList : TList;
  protected
    function  GetCount: integer;
    procedure SetCount(value: integer);
    function  GetItem(index: integer): integer;
    procedure SetItem(index, value: integer);
  public
    constructor Create; 
    destructor Destroy; override;

    procedure Clear;
    procedure Add(position: integer);
    procedure Assign(const Source: TSE2DynMethodList); reintroduce;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property  Items[index: integer]: integer read GetItem  write SetItem; default;
    property  Count                : integer read GetCount write SetCount;
  end;

  TSE2MetaEntry = class(TSE2Object)
  private
    FName       : string;
    FUnitName   : string;

    FSourcePos  : integer;
    FSourceLine : integer;
    FCodePos    : integer;
    FCallType   : TSE2CallType;

    FHasSelf    : boolean;
    FParamCount : integer;
    FResultType : byte;
    FIsExternal : boolean;
    FParamDecl  : AnsiString;
    FDynIndex   : integer;

    FMetaType   : TSE2MetaType;
    FRTTI       : TSE2RTTIList;
    FDynMethods : TSE2DynMethodList;
  protected
    function GetHasResult: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property DynMethods : TSE2DynMethodList read FDynMethods;
    property RTTI       : TSE2RTTIList read FRTTI;
    property MetaType   : TSE2MetaType read FMetaType   write FMetaType;
    property Name       : string     read FName         write FName;
    property AUnitName  : string     read FUnitName     write FUnitName;

    property DynIndex   : integer    read FDynIndex     write FDynIndex;
    property SourcePos  : integer    read FSourcePos    write FSourcePos;
    property SourceLine : integer    read FSourceLine   write FSourceLine;
    property CodePos    : integer    read FCodePos      write FCodePos;
    property HasSelf    : boolean    read FHasSelf      write FHasSelf;

    property ParamCount : integer    read FParamCount   write FParamCount;
    property HasResult  : boolean    read GetHasResult;
    property ResultType : byte       read FResultType   write FResultType;
    property IsExternal : boolean    read FIsExternal   write FIsExternal;
    property CallType   : TSE2CallType read FCallType   write FCallType;
    property ParamDecl  : AnsiString read FParamDecl    write FParamDecl;
  end;

  TSE2MetaList = class(TSE2Object)
  private
    FList : TSE2List;
  protected
    function  GetItem(index: integer): TSE2MetaEntry;
    function  GetCount: integer;
    procedure Remove(index: integer);
    function  IndexOfCodePos(CodePos: integer): integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(Item: TSE2MetaEntry);
    function  Delete(index: integer): boolean;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    function  GetByCodePos(CodePos: integer): TSE2MetaEntry;

    property  Items[index: integer]: TSE2MetaEntry  read GetItem; default;
    property  Count                : integer        read GetCount;
  end;

  TSE2PE = class(TSE2Object)
  private
    FOpCodes      : TSE2OpCodes;
    FStrings      : TSE2StringList;
    FMetaData     : TSE2MetaList;
    FPointerReady : boolean;

    FInitializationPoint : cardinal;
    FMainMethodPoint     : cardinal;
    FFinalizationPoint   : cardinal;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    function FinalStreamSize: int64;

    property OpCodes    : TSE2OpCodes    read FOpCodes;
    property Strings    : TSE2StringList read FStrings;
    property MetaData   : TSE2MetaList   read FMetaData;

    property PointerReady        : boolean        read FPointerReady            write FPointerReady;
    property InitializationPoint : cardinal       read FInitializationPoint     write FInitializationPoint;
    property MainMethodPoint     : cardinal       read FMainMethodPoint         write FMainMethodPoint;
    property FinalizationPoint   : cardinal       read FFinalizationPoint       write FFinalizationPoint;
  end;

implementation

uses SysUtils;

const
  TSE2OpCodes_StreamVersion    = 1;
  TSE2StringList_StreamVersion = 2;
  TSE2MetaEntry_StreamVersion  = 3;
  TSE2MetaList_StreamVersion   = 1;
  TSE2PE_StreamVersion         = 1;

{ TSE2OpCodes }

procedure TSE2OpCodes.Add(OpCode: PSE2OpDefault);
begin
  inherited Add(OpCode);
end;

procedure TSE2OpCodes.Clear;
var i: integer;
begin
  for i:=Self.Count-1 downto 0 do
    Remove(i);
  inherited;
  Reset;
end;

constructor TSE2OpCodes.Create;
begin
  inherited;
end;

function TSE2OpCodes.Delete(index: integer): boolean;
begin
  if (index < 0) or (index >= Self.Count) then
     result := False
  else
  begin
    Remove(index);
    inherited Delete(index);
    result := True;
  end;
end;

destructor TSE2OpCodes.Destroy;
begin
  Clear;
  inherited;
end;

function TSE2OpCodes.GetItem(index: integer): PSE2OpDefault;
begin
  if (index < 0) or (index >= Self.Count) then
     result := nil
  else
     result := inherited Items[index];
end;

function TSE2OpCodes.GetOpCode: PSE2OpDefault;
begin
  if (FPosition < 0) or (FPosition >= Self.Count) then
     result := nil
  else
     result := inherited Items[FPosition];
end;

function TSE2OpCodes.Next: boolean;
begin
  FPosition := FPosition + 1;
  result := FPosition < Self.Count;
end;

procedure TSE2OpCodes.Remove(index: integer);
begin
  FreeMem(PSE2OpDefault(inherited Items[index]));
end;

procedure TSE2OpCodes.Reset;
begin
  FPosition := 0;
end;

procedure TSE2OpCodes.LoadFromStream(Stream: TStream);
var version  : byte;
    OpCode   : PSE2OpDefault;
    i, count : integer;
begin
  Clear;
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  case version of
  1 :
      begin
        {$IFDEF SEII_FPC}
          {$HINTS OFF}
        {$ENDIF}
        Stream.Read(count, SizeOf(integer));
        {$IFDEF SEII_FPC}
          {$HINTS ON}
        {$ENDIF}
        Self.Count := count;
        for i:=0 to count-1 do
        begin
          {$IFDEF SEII_FPC}
            {$HINTS OFF}
          {$ENDIF}
          GetMem(OpCode, SizeOf(TSE2OpDefault));
          Stream.Read(OpCode^, SizeOf(TSE2OpDefault));
          {$IFDEF SEII_FPC}
            {$HINTS OFF}
          {$ENDIF}
          inherited Items[i] := OpCode;
        end;
      end;
  else
      begin
        raise ESE2InvalidDataStream.Create('Unsupported stream version');
      end;
  end;
end;

procedure TSE2OpCodes.SaveToStream(Stream: TStream);
var version  : byte;
    OpCode   : PSE2OpDefault;
    i, count : integer;
begin
  version := TSE2OpCodes_StreamVersion;
  Stream.Write(version, SizeOf(version));

  count := Self.Count;
  Stream.Write(count, SizeOf(integer));

  for i:=0 to count-1 do
  begin
    OpCode := inherited Items[i];
    Stream.Write(OpCode^, SizeOf(TSE2OpDefault));
  end;
end;

{ TSE2StringList }

procedure TSE2StringList.Add(const s: string);
var p: PString;
begin
  New(p);
  p^ := s;
  FList.Add(p);
end;

procedure TSE2StringList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Remove(i);
  FList.Clear;
end;

constructor TSE2StringList.Create;
begin
  inherited;
  FList := TSE2List.Create;
end;

function TSE2StringList.Delete(index: integer): boolean;
begin
  if (index < 0) or (index >= FList.Count) then
     result := False
  else
  begin
    Remove(index);
    FList.Delete(index);
    result := True;
  end;
end;

destructor TSE2StringList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2StringList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2StringList.GetItem(index: integer): string;
begin
  if (index < 0) or (index >= FList.Count) then
     result := ''
  else
     result := PString(FList[index])^;
end;        

procedure TSE2StringList.Remove(index: integer);
var p: PString;
begin
  p  := FList[index];
  p^ := '';
  Dispose(p);
end;

procedure TSE2StringList.LoadFromStream(Stream: TStream);
var version : byte;
    i, count: integer;
    p       : PString;
    encoding: TSE2StringEncoding;
begin
  Clear;
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  case version of
  1, 2 :
      begin
        if version > 1 then
        begin
          Stream.Read(encoding, SizeOf(encoding));
        end;

        {$IFDEF SEII_FPC}
          {$HINTS OFF}
        {$ENDIF}
        Stream.Read(count, SizeOf(integer));
        {$IFDEF SEII_FPC}
          {$HINTS ON}
        {$ENDIF}
        FList.Count := count;

        for i:=0 to count-1 do
        begin
          New(p);
          if version > 1 then
            p^ := TSE2StreamHelper.ReadString(Stream, encoding)
          else
            p^ := TSE2StreamHelper.ReadString(Stream);
          FList[i] := p;
        end;
      end;
  else
      begin
        raise ESE2InvalidDataStream.Create('Unsupported stream version');
      end;
  end;
end;

procedure TSE2StringList.SaveToStream(Stream: TStream);
var version : byte;                       
    i, count: integer;
    p       : PString;
    encoding: TSE2StringEncoding;
begin
  version := TSE2StringList_StreamVersion;
  Stream.Write(version, SizeOf(version));

  encoding := CSE2SaveStringEncoding;
  Stream.Write(encoding, SizeOf(encoding));

  count := FList.Count;
  Stream.Write(count, SizeOf(integer));

  for i:=0 to count-1 do
  begin
    p  := FList[i];
    TSE2StreamHelper.WriteString(Stream, encoding, p^);
  end;
end;

{ TSE2MetaEntry }

constructor TSE2MetaEntry.Create;
begin
  inherited;
  FRTTI := TSE2RTTIList.Create;
  FDynMethods := TSE2DynMethodList.Create;
end;

destructor TSE2MetaEntry.Destroy;
begin
  FDynMethods.Free;
  FRTTI.Free;
  inherited;
end;

function TSE2MetaEntry.GetHasResult: boolean;
begin
  result := FResultType <> 0;
end;

procedure TSE2MetaEntry.LoadFromStream(Stream: TStream);
var version: byte;
    encoding: TSE2StringEncoding;
begin
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  case version of
  1, 2, 3 :
      begin
        if version > 2 then
           Stream.Read(encoding, SizeOf(encoding));

        Stream.Read(FMetaType, SizeOf(TSE2MetaType));

        if version > 2 then
        begin
          TSE2StreamHelper.ReadString(Stream, encoding, FName);
          TSE2StreamHelper.ReadString(Stream, encoding, FUnitName);
        end else
        begin
          TSE2StreamHelper.ReadString(Stream, FName);
          TSE2StreamHelper.ReadString(Stream, FUnitName);
        end;

        Stream.Read(FCodePos    , SizeOf(integer));
        Stream.Read(FSourcePos  , SizeOf(integer));
        Stream.Read(FSourceLine , SizeOf(integer));
        Stream.Read(FHasSelf    , SizeOf(boolean));
        Stream.Read(FDynIndex   , SizeOf(integer));

        Stream.Read(FCallType   , SizeOf(FCallType));


        Stream.Read(FParamCount , SizeOf(integer));
        Stream.Read(FResultType , SizeOf(byte));
        Stream.Read(FIsExternal , SizeOf(boolean));

        TSE2StreamHelper.ReadAnsiString(Stream, FParamDecl);

        if version > 1 then
           FRTTI.LoadFromStream(Stream);

        if FMetaType = mtClass then
        begin
          if version < 2 then   
             FRTTI.LoadFromStream(Stream);
          FDynMethods.LoadFromStream(Stream);
        end;
      end;
  else
      begin
        raise ESE2InvalidDataStream.Create('Unsupported stream version');
      end;
  end;
end;

procedure TSE2MetaEntry.SaveToStream(Stream: TStream);
var version  : byte;
    encoding : TSE2StringEncoding;
begin
  version := TSE2MetaEntry_StreamVersion;
  Stream.Write(version, SizeOf(version));
  
  encoding := CSE2SaveStringEncoding;
  Stream.Write(encoding, SizeOf(encoding));
                                         
  Stream.Write(FMetaType, SizeOf(TSE2MetaType));

  TSE2StreamHelper.WriteString(Stream, encoding, FName);
  TSE2StreamHelper.WriteString(Stream, encoding, FUnitName);
                                                 
  Stream.Write(FCodePos    , SizeOf(integer));
  Stream.Write(FSourcePos  , SizeOf(integer));
  Stream.Write(FSourceLine , SizeOf(integer));
  Stream.Write(FHasSelf    , SizeOf(boolean));
  Stream.Write(FDynIndex   , SizeOf(integer));

  Stream.Write(FCallType   , SizeOf(FCallType));

  Stream.Write(FParamCount , SizeOf(integer));
  Stream.Write(FResultType , SizeOf(byte));
  Stream.Write(FIsExternal , SizeOf(boolean));

  TSE2StreamHelper.WriteAnsiString(Stream, FParamDecl);

  FRTTI.SaveToStream(Stream);

  if FMetaType = mtClass then
  begin
     FDynMethods.SaveToStream(Stream);
  end;
end;

{ TSE2MetaList }

procedure TSE2MetaList.Add(Item: TSE2MetaEntry);
begin
  FList.Add(Item);
end;

procedure TSE2MetaList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Remove(i);
  FList.Clear;
end;

constructor TSE2MetaList.Create;
begin
  inherited;
  FList := TSE2List.Create;
end;

function TSE2MetaList.Delete(index: integer): boolean;
begin
  if (index < 0) or (index >= FList.Count) then
     result := False
  else
  begin
    Remove(index);
    FList.Delete(index);
    result := True;
  end;
end;

destructor TSE2MetaList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2MetaList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2MetaList.GetItem(index: integer): TSE2MetaEntry;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := FList[index];
end;        

procedure TSE2MetaList.Remove(index: integer);
begin
  TSE2MetaEntry(FList[index]).Free;
end;

procedure TSE2MetaList.LoadFromStream(Stream: TStream);
var version  : byte;
    i, count : integer;
    p        : TSE2MetaEntry;
begin
  Clear;
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  case version of
  1 :
      begin
        {$IFDEF SEII_FPC}
          {$HINTS OFF}
        {$ENDIF}
        Stream.Read(count, SizeOf(count));
        {$IFDEF SEII_FPC}
          {$HINTS ON}
        {$ENDIF}
        FList.Count := count;

        for i:=0 to FList.Count-1 do
        begin
          p := TSE2MetaEntry.Create;
          p.LoadFromStream(Stream);
          FList[i] := p;
        end;
      end;
  else
      begin
        raise ESE2InvalidDataStream.Create('Unsupported stream version');
      end;
  end;
end;

procedure TSE2MetaList.SaveToStream(Stream: TStream);
var version  : byte;        
    i, count : integer;
    p        : TSE2MetaEntry;
begin
  version := TSE2MetaList_StreamVersion;
  Stream.Write(version, SizeOf(version));

  count := FList.Count;
  Stream.Write(count, SizeOf(count));

  for i:=0 to FList.Count-1 do
  begin
    p := FList[i];
    p.SaveToStream(Stream);
  end;

end;

function TSE2MetaList.GetByCodePos(CodePos: integer): TSE2MetaEntry;
var i: integer;
begin
  i := IndexOfCodePos(CodePos);
  if i = -1 then
     result := nil
  else
     result := FList[i];
end;

function TSE2MetaList.IndexOfCodePos(CodePos: integer): integer;
begin
  for result:=FList.Count-1 downto 0 do
    if Items[result].CodePos = CodePos then
       exit;
  result := -1;
end;

{ TSE2PE }

constructor TSE2PE.Create;
begin
  inherited;
  FOpCodes  := TSE2OpCodes.Create;
  FStrings  := TSE2StringList.Create;
  FMetaData := TSE2MetaList.Create;
  FPointerReady := False;
end;

destructor TSE2PE.Destroy;
begin
  FOpCodes.Free;
  FStrings.Free;
  FMetaData.Free;
  inherited;
end;

function TSE2PE.FinalStreamSize: int64;
var ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    SaveToStream(ms);
    result := ms.Size;
  finally
    ms.Free;
  end;
end;

function CheckPEHeader(Stream: TStream): boolean;
var s: AnsiString;
begin
  result := False;
  SetLength(s, length(C_SE2PEHeaderStr));
  if Stream.Read(s[1], length(C_SE2PEHeaderStr)) <> length(C_SE2PEHeaderStr) then
     exit;

  result := AnsiSameStr(string(s), string(C_SE2PEHeaderStr));
end;

procedure WritePEHeader(Stream: TStream);
var s: AnsiString;
begin
  s := C_SE2PEHeaderStr;
  Stream.Write(s[1], length(s));
end;

procedure TSE2PE.LoadFromStream(Stream: TStream);
var version: byte;
begin
  FPointerReady := False;
  if not CheckPEHeader(Stream) then  
     raise ESE2InvalidDataStream.Create('Unsupported data stream');

  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  case version of
  1 :
      begin
        Stream.Read(FInitializationPoint, SizeOf(FInitializationPoint));
        Stream.Read(FMainMethodPoint, SizeOf(FMainMethodPoint));
        Stream.Read(FFinalizationPoint, SizeOf(FFinalizationPoint));

        FOpCodes.LoadFromStream(Stream);
        FStrings.LoadFromStream(Stream);
        FMetaData.LoadFromStream(Stream);
      end;
  else
      begin
        raise ESE2InvalidDataStream.Create('Unsupported stream version');
      end;
  end;
end;

procedure TSE2PE.SaveToStream(Stream: TStream);
var version: byte;
begin
  WritePEHeader(Stream);
  version := TSE2PE_StreamVersion;
  Stream.Write(version, SizeOf(version));

  Stream.Write(FInitializationPoint, SizeOf(FInitializationPoint));
  Stream.Write(FMainMethodPoint, SizeOf(FMainMethodPoint));
  Stream.Write(FFinalizationPoint, SizeOf(FFinalizationPoint));

  FOpCodes.SaveToStream(Stream);
  FStrings.SaveToStream(Stream);
  FMetaData.SaveToStream(Stream);    
end;

{ TSE2RTTIList }

procedure TSE2RTTIList.Add(AType: TSE2TypeIdent; Offset, Size: integer);
var p: PSE2RTTIEntry;
begin
  New(p);
  p^.Offset := Offset;
  p^.Size   := Size;
  p^.AType  := AType;
  FList.Add(p);
end;

procedure TSE2RTTIList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Dispose(PSE2RTTIEntry(FList[i]));
  Flist.Clear;
end;

constructor TSE2RTTIList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TSE2RTTIList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2RTTIList.FindSize(AType: TSE2TypeIdent;
  Offset: integer): integer;
var p: PSE2RTTIEntry;
begin
  for result := FList.Count-1 downto 0 do
  begin
    p := FList[result];
    if p^.AType = AType then
      if p^.Offset = Offset then
        exit;
  end;
  result := -1;
end;

function TSE2RTTIList.GetCount: integer;
begin
  result := FList.Count;
end;

function TSE2RTTIList.GetItem(index: integer): PSE2RTTIEntry;
begin
  if (index < 0) or (index >= FList.Count) then
     result := nil
  else
     result := Flist[index];
end;

procedure TSE2RTTIList.LoadFromStream(Stream: TStream);
var version  : byte;
    i, count : integer;
    p        : PSE2RTTIEntry;
begin
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  Clear;
  case version of
  1 :
      begin
        {$IFDEF SEII_FPC}
          {$HINTS OFF}
        {$ENDIF}
        Stream.Read(count, SizeOf(count));
        {$IFDEF SEII_FPC}
          {$HINTS ON}
        {$ENDIF}
        FList.Count := count;

        for i:=0 to count-1 do
        begin
          New(p);
          Stream.Read(p^.Offset, SizeOf(p^.Offset));
          Stream.Read(p^.Size, SizeOf(p^.Size));
          Stream.Read(p^.AType, SizeOf(p^.AType));
          Flist[i] := p;
        end;
      end;
  else
      begin
        raise ESE2InvalidDataStream.Create('Unsupported stream version');
      end;
  end;
end;

procedure TSE2RTTIList.SaveToStream(Stream: TStream);
var version  : byte;
    i, count : integer;
    p        : PSE2RTTIEntry;
begin
  version := 1;
  Stream.Write(version, SizeOf(version));

  count := Flist.Count;
  Stream.Write(count, SizeOf(count));

  for i:=0 to count-1 do
  begin
    p := FList[i];       
    Stream.Write(p^.Offset, SizeOf(p^.Offset));
    Stream.Write(p^.Size, SizeOf(p^.Size));
    Stream.Write(p^.AType, SizeOf(p^.AType));
  end;
end;

{ TSE2DynMethodList }

procedure TSE2DynMethodList.Add(position: integer);
var p: PInteger;
begin
  New(p);
  p^ := position;
  FList.Add(p);
end;

procedure TSE2DynMethodList.Assign(const Source: TSE2DynMethodList);
var i: integer;
    p: PInteger;
begin
  Clear;
  FList.Count := Source.Count;
  for i:=0 to Source.Count-1 do
  begin
    New(p);
    p^ := Source[i];
    FList[i] := p;
  end;
end;

procedure TSE2DynMethodList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Dispose(PInteger(Flist[i]));
  FList.Clear;
end;

constructor TSE2DynMethodList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TSE2DynMethodList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSE2DynMethodList.GetCount: integer;
begin
  result := Flist.Count;
end;

function TSE2DynMethodList.GetItem(index: integer): integer;
begin
  //Assert(index >= 0, Self.ClassName);
  if (index < 0) or (index >= FList.Count) then
     result := -1
  else
     result := Pinteger(FList[index])^;
end;

procedure TSE2DynMethodList.LoadFromStream(Stream: TStream);
var version  : byte;
    i, count : integer;
    n        : integer;
    p        : PInteger;
begin
  {$IFDEF SEII_FPC}
    {$HINTS OFF}
  {$ENDIF}
  if Stream.Read(version, SizeOf(version)) < SizeOf(version) then
     exit;
  {$IFDEF SEII_FPC}
    {$HINTS ON}
  {$ENDIF}

  Clear;
  case version of
  1 :
      begin
        {$IFDEF SEII_FPC}
          {$HINTS OFF}
        {$ENDIF}
        Stream.Read(count, SizeOf(count));
        {$IFDEF SEII_FPC}
          {$HINTS ON}
        {$ENDIF}
        FList.Count := count;

        for i:=0 to count-1 do
        begin
          {$IFDEF SEII_FPC}
            {$HINTS OFF}
          {$ENDIF}
          Stream.Read(n, SizeOf(n));
          {$IFDEF SEII_FPC}
            {$HINTS ON}
          {$ENDIF}
          New(p);
          p^ := n;
          Flist[i] := p;
        end;
      end;
  else
      begin
        raise ESE2InvalidDataStream.Create('Unsupported stream version');
      end;
  end;
end;

procedure TSE2DynMethodList.SaveToStream(Stream: TStream);
var version  : byte;
    i, count : integer;
    n        : integer;
begin
  version := 1;
  Stream.Write(version, SizeOf(version));

  count := Flist.Count;
  Stream.Write(count, SizeOf(count));

  for i:=0 to count-1 do
  begin
    n := Pinteger(FList[i])^;
    Stream.Write(n, SizeOf(n));
  end;

end;

procedure TSE2DynMethodList.SetCount(value: integer);
var i: integer;
    p: PInteger;
begin
  for i:=FList.Count to value-1 do
  begin
    New(p);
    p^ := 0;
    FList.Add(p)
  end;
end;

procedure TSE2DynMethodList.SetItem(index, value: integer);
begin
  if index <= FList.Count then
     SetCount(index + 1);
  PInteger(FList[index])^ := value;
end;

end.
