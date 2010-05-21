unit uSE2RunTimeClasses;

{$INCLUDE ScriptEngine.inc}

interface

{$IFDEF SEII_FPC}
  {$HINTS OFF}
  {$WARNINGS OFF}
{$ENDIF}

uses
  Classes, uSE2MemoryManager, uSE2PEData, uSE2Consts;

type
  TSE2RunTimeClasses = class(TObject)
  private
    FMM  : TSE2MemoryManager;
    FPtr : Pointer;
  public
    constructor Create(MM: TSE2MemoryManager);
    destructor Destroy; override;

    { Classes }
    function  CreateScriptClassObject(const Meta: TSE2MetaEntry; PE: TSE2PE): Pointer;
    procedure DestroyScriptClassObject(AClass: Pointer);

    { Records }
    function  CreateScriptRecord(const Meta: TSE2MetaEntry; PE: TSE2PE): Pointer;
    procedure DestroyScriptRecord(ARecord: Pointer);
    function  DuplicateScriptRecord(ARecord: Pointer): Pointer;
    function  ScriptRecordEqual(ARec1, ARec2: Pointer; Meta: TSE2MetaEntry): boolean;

    //procedure CopyScriptRecord(ASource, ADest: Pointer); overload;
    procedure CopyScriptRecord(ASource, ADest: Pointer; Meta: TSE2MetaEntry); overload;

    //procedure DelphiToScriptRecord(ASource, ADest: Pointer); overload;
    procedure DelphiToScriptRecord(ASource, ADest: Pointer; Meta: TSE2MetaEntry); overload;
    //procedure ScriptToDelphiRecord(ASource: Pointer; ADest: Pointer); overload;
    //function  ScriptToDelphiRecord(ASource: Pointer): Pointer; overload;
    function  ScriptToDelphiRecord(ASource: Pointer; Meta: TSE2MetaEntry): Pointer; overload;
    procedure ScriptToDelphiRecord(ASource: Pointer; ADest: Pointer; Meta: TSE2MetaEntry); overload;

    { Helper }
    function  GetClassMethods(AClass: Pointer): TSE2DynMethodList;
    function  GetClassMeta(AClass: Pointer): TSE2MetaEntry;


    property MM : TSE2MemoryManager read FMM write FMM;
  end;

const
  vmtScriptInstanceSize = -4;
  vmtScriptMetaEntry    = -8;
  vmtScriptPointerList  = -12;
  vmtScriptMethodList   = -16;

  vmtScriptMetaTotalSize = 16;

implementation

uses
  uSE2RunAccess, uSE2OpCode, SysUtils;

type
  PSE2ClassPointerEntry = ^TSE2ClassPointerEntry;
  TSE2ClassPointerEntry = record
    Offset : integer;
    Size   : integer;
    aType  : TSE2TypeIdent;
  end;

  TSE2ClassPointerList = class(TObject)
  private
    Flist : TList;
    FRT   : TSE2RunTimeClasses;
  public
    constructor Create(RT: TSE2RunTimeClasses);
    destructor Destroy; override;

    procedure Assign(Source: TSE2ClassPointerList);                                                               
    procedure Clear;
    procedure Add(aType: TSE2TypeIdent; Offset, Size: integer);

    procedure CopyData(AClass, ADataSource: Pointer); overload;
    class procedure CopyData(ASource, ADest: Pointer; Meta: TSE2MetaEntry); overload;
    
    class procedure DelphiToScriptRecDat(ASource, ADest: Pointer; Meta: TSE2MetaEntry);
    class procedure ScriptToDelphiRecData(ASource, ADest: Pointer; Meta: TSE2MetaEntry);

    class procedure CreateData(ADest: Pointer; Meta: TSE2MetaEntry); overload;
    procedure CreateData(AClass: Pointer; PE: TSE2PE); overload;
    procedure CreateData(AClass, ADataSource: Pointer); overload;

    procedure FreeData(AClass: Pointer); overload;
    class procedure FreeData(AData: Pointer; Meta: TSE2MetaEntry); overload;
  end;

  PSE2PointerEntry = ^TSE2PointerEntry;
  TSE2PointerEntry = record
    Offset   : integer;
    Value    : Pointer;
  end;

  TSE2StrPtrStyle = (spsSE2, spsToDelphi, spsToSE2);

  TSE2PointerList = class(TObject)
  private
    FList : TList;
    FRT   : TSE2RunTimeClasses;
  public
    constructor Create(RT: TSE2RunTimeClasses);
    destructor Destroy; override;

    procedure Clear;
    procedure PushPointers(Data: Pointer; StoreList: TSE2ClassPointerList; CompStrPtr: TSE2StrPtrStyle = spsSE2); overload;
    procedure PushPointers(Data: Pointer; Meta: TSE2MetaEntry; CompStrPtr: TSE2StrPtrStyle = spsSE2); overload;
    procedure PopPointers(Data: Pointer);
  end;

{ TSE2ClassPointerList }

procedure TSE2ClassPointerList.Add(aType: TSE2TypeIdent; Offset,
  Size: integer);
var p: PSE2ClassPointerEntry;
begin
  New(p);
  p^.Offset := Offset;
  p^.Size   := Size;
  p^.aType  := aType;
  Flist.Add(p);
end;       

procedure TSE2ClassPointerList.Assign(Source: TSE2ClassPointerList);
var i    : integer;
    p, s : PSE2ClassPointerEntry;
begin
  Clear;
  Flist.Count := Source.Flist.Count;
  for i:=0 to Source.Flist.Count-1 do
  begin
    s := Source.Flist[i];
    New(p);
    p^.Offset := s^.Offset;
    p^.Size   := s^.Size;
    p^.aType  := s^.aType;
    FList[i]  := s;
  end;
end;

procedure TSE2ClassPointerList.Clear;
var i: integer;
begin
  for i:=Flist.Count-1 downto 0 do
    Dispose(PSE2ClassPointerEntry(FList[i]));
  Flist.Clear;
end;

constructor TSE2ClassPointerList.Create(RT: TSE2RunTimeClasses);
begin
  inherited Create;
  FRT   := RT;
  Flist := TList.Create;
end;

procedure TSE2ClassPointerList.CopyData(AClass, ADataSource: Pointer);
var i  : integer;
    p  : PSE2ClassPointerEntry;
begin
  for i:=0 to Flist.Count-1 do
  begin
    p := Flist[i];
    case p.aType of
    btString     :
        begin
          PbtString(PPointer(PtrInt(AClass) + p.Offset)^)^ := PbtString(PPointer(PtrInt(ADataSource) + p.Offset)^)^;
        end;
    btUTF8String :
        begin
          PbtUTF8String(PPointer(PtrInt(AClass) + p.Offset)^)^ := PbtUTF8String(PPointer(PtrInt(ADataSource) + p.Offset)^)^;
        end;
    btWideString :
        begin
          PbtWideString(PPointer(PtrInt(AClass) + p.Offset)^)^ := PbtWideString(PPointer(PtrInt(ADataSource) + p.Offset)^)^;
        end;
    btPChar      :
        begin
          PbtPChar(PPointer(PtrInt(AClass) + p.Offset)^)^ := PbtPChar(PPointer(PtrInt(ADataSource) + p.Offset)^)^;
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          //FRT.CopyScriptRecord(PPointer(PtrInt(ADataSource) + p.Offset)^, PPointer(PtrInt(AClass) + p.Offset)^);
        end;
    end;
  end;
end;

class procedure TSE2ClassPointerList.CopyData(ASource, ADest: Pointer;
  Meta: TSE2MetaEntry);
var i  : integer;
    p  : PSE2RTTIEntry;
begin
  for i:=0 to Meta.RTTI.Count-1 do
  begin
    p := Meta.RTTI[i];
    case p.aType of
    btString     :
        begin
          PbtString(PPointer(PtrInt(ADest) + p.Offset)^)^ := PbtString(PPointer(PtrInt(ASource) + p.Offset)^)^;
        end;
    btUTF8String :
        begin
          PbtUTF8String(PPointer(PtrInt(ADest) + p.Offset)^)^ := PbtUTF8String(PPointer(PtrInt(ASource) + p.Offset)^)^;
        end;
    btWideString :
        begin
          PbtWideString(PPointer(PtrInt(ADest) + p.Offset)^)^ := PbtWideString(PPointer(PtrInt(ASource) + p.Offset)^)^;
        end;
    btPChar      :
        begin
          PbtPChar(PPointer(PtrInt(ADest) + p.Offset)^)^ := PbtPChar(PPointer(PtrInt(ASource) + p.Offset)^)^;
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          //FRT.CopyScriptRecord(PPointer(PtrInt(ASource) + p.Offset)^, PPointer(PtrInt(ADest) + p.Offset)^);
        end;
    end;
  end;
end;

procedure TSE2ClassPointerList.CreateData(AClass, ADataSource: Pointer);
var i  : integer;
    p  : PSE2ClassPointerEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pP : Pointer;
begin
  for i:=0 to Flist.Count-1 do
  begin
    p := Flist[i];
    case p.aType of
    btString     :
        begin
          New(pS);
          pS^ := PbtString(PPointer(PtrInt(ADataSource) + p.Offset)^)^;
          PPointer(PtrInt(AClass) + p.Offset)^ := pS;
        end;
    btUTF8String :
        begin
          New(pU);
          pU^ := PbtUTF8String(PPointer(PtrInt(ADataSource) + p.Offset)^)^;
          PPointer(PtrInt(AClass) + p.Offset)^ := pU;
        end;
    btWideString :
        begin
          New(pW);
          pW^ := PbtWideString(PPointer(PtrInt(ADataSource) + p.Offset)^)^;
          PPointer(integer(AClass) + p.Offset)^ := pW;
        end;
    btPChar      :
        begin
          New(pC);
          pC^ := PbtPChar(PPointer(PtrInt(ADataSource) + p.Offset)^)^;
          PPointer(PtrInt(AClass) + p.Offset)^ := pC;
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          pP := FRT.DuplicateScriptRecord(PPointer(PtrInt(ADataSource) + p.Offset)^);
          PPointer(PtrInt(AClass) + p.Offset)^ := pP;
        end;
    end;
  end;
end;

procedure TSE2ClassPointerList.CreateData(AClass: Pointer; PE: TSE2PE);
var i  : integer;
    p  : PSE2ClassPointerEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pP : Pointer;
begin
  for i:=0 to Flist.Count-1 do
  begin
    p := Flist[i];
    case p.aType of
    btString     :
        begin
          New(pS);
          pS^ := '';
          PPointer(PtrInt(AClass) + p.Offset)^ := pS;
        end;
    btUTF8String :
        begin
          New(pU);
          pU^ := '';
          PPointer(PtrInt(AClass) + p.Offset)^ := pU;
        end;
    btWideString :
        begin
          New(pW);
          pW^ := '';
          PPointer(PtrInt(AClass) + p.Offset)^ := pW;
        end;
    btPChar      :
        begin
          New(pC);
          pC^ := '';
          PPointer(PtrInt(AClass) + p.Offset)^ := pC;
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          pP := FRT.CreateScriptRecord( PE.MetaData[p.Size], PE );
          PPointer(PtrInt(AClass) + p.Offset)^ := pP;
        end;
    end;
  end;
end;

class procedure TSE2ClassPointerList.CreateData(ADest: Pointer;
  Meta: TSE2MetaEntry);
var i  : integer;
    p  : PSE2RTTIEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    //pP : Pointer;
begin
  for i:=0 to Meta.RTTI.Count-1 do
  begin
    p := Meta.RTTI[i];
    case p.aType of
    btString     :
        begin
          New(pS);
          pS^ := '';
          PPointer(PtrInt(ADest) + p.Offset)^ := pS;
        end;
    btUTF8String :
        begin
          New(pU);
          pU^ := '';
          PPointer(PtrInt(ADest) + p.Offset)^ := pU;
        end;
    btWideString :
        begin
          New(pW);
          pW^ := '';
          PPointer(PtrInt(ADest) + p.Offset)^ := pW;
        end;
    btPChar      :
        begin
          New(pC);
          pC^ := '';
          PPointer(PtrInt(ADest) + p.Offset)^ := pC;
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
        end;
    end;
  end;
end;

procedure TSE2ClassPointerList.FreeData(AClass: Pointer);
var i  : integer;
    p  : PSE2ClassPointerEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pP : Pointer;
begin
  if Self = nil then
     exit;
  for i:=0 to Flist.Count-1 do
  begin
    p := Flist[i];
    case p.aType of
    btString     :
        begin
          pS := PPointer(PtrInt(AClass) + p.Offset)^;
          pS^ := '';
          Dispose(pS);
        end;
    btUTF8String :
        begin
          pU := PPointer(PtrInt(AClass) + p.Offset)^;
          pU^ := '';
          Dispose(pU);
        end;
    btWideString :
        begin
          pW := PPointer(PtrInt(AClass) + p.Offset)^;
          pW^ := '';
          Dispose(pW);
        end;
    btPChar      :
        begin
          pC := PPointer(PtrInt(AClass) + p.Offset)^;
          pC^ := '';
          Dispose(pC);
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          pP := PPointer(Pointer(PtrInt(AClass) + p^.Offset))^;
          FRT.DestroyScriptRecord(pP);
        end;
    end;
  end;
end;

class procedure TSE2ClassPointerList.FreeData(AData: Pointer;
  Meta: TSE2MetaEntry);
var i  : integer;
    p  : PSE2RTTIEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
begin
  for i:=0 to Meta.RTTI.Count-1 do
  begin
    p := Meta.RTTI[i];
    case p.aType of
    btString     :
        begin
          pS := PPointer(PtrInt(AData) + p.Offset)^;
          pS^ := '';
          Dispose(pS);
        end;
    btUTF8String :
        begin
          pU := PPointer(PtrInt(AData) + p.Offset)^;
          pU^ := '';
          Dispose(pU);
        end;
    btWideString :
        begin
          pW := PPointer(PtrInt(AData) + p.Offset)^;
          pW^ := '';
          Dispose(pW);
        end;
    btPChar      :
        begin
          pC := PPointer(PtrInt(AData) + p.Offset)^;
          pC^ := '';
          Dispose(pC);
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin

        end;
    end;
  end;
end;

destructor TSE2ClassPointerList.Destroy;
begin
  Clear;
  Flist.Free;
  inherited;
end;

class procedure TSE2ClassPointerList.DelphiToScriptRecDat(ASource, ADest: Pointer; Meta: TSE2MetaEntry);
var i  : integer;
    p  : PSE2RTTIEntry;
begin
  for i:=0 to Meta.RTTI.Count-1 do
  begin
    p := Meta.RTTI[i];
    case p.aType of
    btString,
    btUTF8String,
    btWideString,
    btPChar :
        begin
          PPointer(PPointer(PtrInt(ADest) + p.Offset)^)^ := PPointer(PtrInt(ASource) + p.Offset)^;
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          //FRT.CopyScriptRecord(PPointer(PtrInt(ASource) + p.Offset)^, PPointer(PtrInt(ADest) + p.Offset)^);
        end;
    end;
  end;
end;

class procedure TSE2ClassPointerList.ScriptToDelphiRecData(ASource,
  ADest: Pointer; Meta: TSE2MetaEntry);
var i  : integer;
    p  : PSE2RTTIEntry;
begin
  for i:=0 to Meta.RTTI.Count-1 do
  begin
    p := Meta.RTTI[i];
    case p.aType of
    btString,
    btUTF8String,
    btWideString,
    btPChar :
        begin
          PPointer(PtrInt(ADest) + p.Offset)^    := PPointer(PPointer(PtrInt(ASource) + p.Offset)^)^;
        end;
    btArray      :
        begin

        end;
    btRecord     :
        begin
          //FRT.CopyScriptRecord(PPointer(PtrInt(ASource) + p.Offset)^, PPointer(PtrInt(ADest) + p.Offset)^);
        end;
    end;
  end;
end;

{ TSE2PointerList }

procedure TSE2PointerList.Clear;
var i: integer;
begin
  for i:=FList.Count-1 downto 0 do
    Dispose(PSE2PointerEntry(Flist[i]));
  Flist.Clear;
end;

constructor TSE2PointerList.Create(RT: TSE2RunTimeClasses);
begin
  inherited Create;
  FRT   := RT;
  Flist := TList.Create;
end;

destructor TSE2PointerList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TSE2PointerList.PopPointers(Data: Pointer);
var i     : integer;
    Entry : PSE2PointerEntry;
begin
  for i:=0 to Flist.Count-1 do
  begin
    Entry := Flist[i];
    //if Entry <> nil then
       PPointer(PtrInt(Data) + Entry^.Offset)^ := Entry^.Value;
  end;
end;

procedure TSE2PointerList.PushPointers(Data: Pointer;
  StoreList: TSE2ClassPointerList; CompStrPtr: TSE2StrPtrStyle = spsSE2);
var i  : integer;
    p  : PSE2ClassPointerEntry;
    Entry : PSE2PointerEntry;
begin
  Clear;
  FList.Count := StoreList.Flist.Count;
  for i:=0 to StoreList.Flist.Count-1 do
  begin
    p := StoreList.Flist[i];
    case p.aType of
    btString,
    btUTF8String,
    btWideString,
    btPChar  :
        begin
          New(Entry);
          Entry^.Offset := p.Offset;
          case CompStrPtr of
          spsSE2      : Entry^.Value := PPointer(PtrInt(Data) + p.Offset)^;
          spsToDelphi : Entry^.Value := PPointer(PPointer(PtrInt(Data) + p.Offset)^)^;
          spsToSE2    :
             begin
               Entry^.Value := Pointer(PtrInt(Data) + p.Offset);
               //Entry^.Value := @Entry^.Value;
             end;
          end;
          FList[i] := Entry;
        end;
    btRecord :
        begin
          New(Entry);
          Entry^.Offset := p.Offset;
          Entry^.Value  := PPointer(PtrInt(Data) + p.Offset)^;
          FList[i] := Entry;
        end;
    end;
  end;
end;

procedure TSE2PointerList.PushPointers(Data: Pointer; Meta: TSE2MetaEntry;
  CompStrPtr: TSE2StrPtrStyle);
var i  : integer;
    p  : PSE2RTTIEntry;
    Entry : PSE2PointerEntry;
begin
  Clear;
  FList.Count := Meta.RTTI.Count;;
  for i:=0 to Meta.RTTI.Count-1 do
  begin
    p := Meta.RTTI[i];
    case p.aType of
    btString,
    btUTF8String,
    btWideString,
    btPChar  :
        begin
          New(Entry);
          Entry^.Offset := p.Offset;
          case CompStrPtr of
          spsSE2      : Entry^.Value := PPointer(PtrInt(Data) + p.Offset)^;
          spsToDelphi : Entry^.Value := PPointer(PPointer(PtrInt(Data) + p.Offset)^)^;
          spsToSE2    :
             begin
               Entry^.Value := Pointer(PtrInt(Data) + p.Offset);
               //Entry^.Value := @Entry^.Value;
             end;
          end;
          FList[i] := Entry;
        end;
    btRecord :
        begin
          New(Entry);
          Entry^.Offset := p.Offset;
          Entry^.Value  := PPointer(PtrInt(Data) + p.Offset)^;
          FList[i] := Entry;
        end;
    end;
  end;
end;

{ TSE2RunTimeClasses }
 {
procedure TSE2RunTimeClasses.CopyScriptRecord(ASource, ADest: Pointer);
var PtrSave : TSE2PointerList;
    runInfo : TSE2ClassPointerList;
    iSize   : integer;
begin
  if (ASource <> nil) and (ADest <> nil) then
  begin
    PtrSave := TSE2PointerList.Create(Self);

    runInfo := PPointer(PtrInt(ASource) + vmtScriptPointerList)^;
    PtrSave.PushPointers(ADest, runInfo);

    iSize := PInteger(PtrInt(ASource) + vmtScriptInstanceSize)^;
    Move(ASource^, ADest^, iSize);

    PtrSave.PopPointers(ADest);
    runInfo.CopyData(ADest, ASource);

    PtrSave.Free;
  end;
end;               }

procedure TSE2RunTimeClasses.CopyScriptRecord(ASource, ADest: Pointer;
  Meta: TSE2MetaEntry);
begin
  if (ASource <> nil) and (ADest <> nil) then
  begin
    TSE2PointerList(FPtr).PushPointers(ADest, Meta);
    Move(ASource^, ADest^, Meta.ParamCount);
    TSE2PointerList(FPtr).PopPointers(ADest);
    TSE2ClassPointerList.CopyData(ASource, ADest, Meta);
    TSE2PointerList(FPtr).Clear;
  end;
end;

constructor TSE2RunTimeClasses.Create(MM: TSE2MemoryManager);
begin
  inherited Create;
  FMM  := MM;
  FPtr := TSE2PointerList.Create(Self);
end;

destructor TSE2RunTimeClasses.Destroy;
begin
  TObject(FPtr).Free;
  inherited;
end;

function TSE2RunTimeClasses.CreateScriptClassObject(
  const Meta: TSE2MetaEntry; PE: TSE2PE): Pointer;
var runInfo : TSE2ClassPointerList;
    runCalls: TSE2DynMethodList;
    i       : integer;
    p       : PSE2RTTIEntry;
begin
  result := nil;
  if Meta <> nil then
    if Meta.MetaType = mtClass then
    begin
      FMM.GetMem(result, Meta.ParamCount + vmtScriptMetaTotalSize);
      FillChar(result^, Meta.ParamCount + vmtScriptMetaTotalSize, 0);
      result := Pointer(cardinal(result) + vmtScriptMetaTotalSize);

      PInteger(PtrInt(result) + vmtScriptInstanceSize)^ := Meta.ParamCount;
      PPointer(PtrInt(result) + vmtScriptMetaEntry)^    := Meta;

      runInfo := TSE2ClassPointerList.Create(Self);
      PPointer(PtrInt(result) + vmtScriptPointerList)^  := runInfo;

      runCalls := TSE2DynMethodList.Create;
      PPointer(PtrInt(result) + vmtScriptMethodList)^   := runCalls;

      runCalls.Assign(Meta.DynMethods);

      for i:=0 to Meta.RTTI.Count-1 do
      begin
        p := Meta.RTTI[i];
        runInfo.Add(p^.AType, p^.Offset, p^.Size);
      end;
      runInfo.CreateData(result, PE);
    end;
end;

function TSE2RunTimeClasses.CreateScriptRecord(const Meta: TSE2MetaEntry;
  PE: TSE2PE): Pointer;
begin
  result := nil;          
  if Meta <> nil then
    if Meta.MetaType = mtRecord then
    begin
      FMM.GetMem(result, Meta.ParamCount + SizeOf(Pointer));
      FillChar(result^, Meta.ParamCount, 0);

      PPointer(result)^ := Meta;
      result := Pointer(PtrInt(result) + SizeOf(Pointer));

      TSE2ClassPointerList.CreateData(result, Meta);
    end;
end;

{var runInfo : TSE2ClassPointerList;
    runCalls: TSE2DynMethodList;
    i       : integer;
    p       : PSE2RTTIEntry;
begin
  result := nil;
  if Meta <> nil then
    if Meta.MetaType = mtRecord then
    begin
      FMM.GetMem(result, Meta.ParamCount + vmtScriptMetaTotalSize);
      FillChar(result^, Meta.ParamCount + vmtScriptMetaTotalSize, 0);
      result := Pointer(cardinal(result) + vmtScriptMetaTotalSize);

      PInteger(PtrInt(result) + vmtScriptInstanceSize)^ := Meta.ParamCount;
      PPointer(PtrInt(result) + vmtScriptMetaEntry)^    := Meta;

      runInfo := TSE2ClassPointerList.Create(Self);
      PPointer(PtrInt(result) + vmtScriptPointerList)^  := runInfo;

      runCalls := TSE2DynMethodList.Create;             
      PPointer(PtrInt(result) + vmtScriptMethodList)^   := runCalls;

      runCalls.Assign(Meta.DynMethods);

      for i:=0 to Meta.RTTI.Count-1 do
      begin
        p := Meta.RTTI[i];
        runInfo.Add(p^.AType, p^.Offset, p^.Size);
      end;
      runInfo.CreateData(result, PE);
    end;
end;      }

procedure TSE2RunTimeClasses.DestroyScriptClassObject(AClass: Pointer);
var iSize   : integer;
    runInfo : TSE2ClassPointerList;
    runCalls: TSE2DynMethodList;

begin
  if AClass <> nil then
  begin
    iSize  := PInteger(PtrInt(AClass) + vmtScriptInstanceSize)^;
    runInfo := PPointer(PtrInt(AClass) + vmtScriptPointerList)^;
    runInfo.FreeData(AClass);
    runInfo.Free;
    runCalls := PPointer(PtrInt(AClass) + vmtScriptMethodList)^;
    runCalls.Free;

    AClass := Pointer(PtrInt(AClass) - vmtScriptMetaTotalSize);
    FMM.FreeMem(AClass, iSize + vmtScriptMetaTotalSize);
  end;
end;

procedure TSE2RunTimeClasses.DestroyScriptRecord(ARecord: Pointer);
var Meta: TSE2MetaEntry;
begin
  if ARecord <> nil then
  begin
    Meta    := PPointer(PtrInt(ARecord) - SizeOf(Pointer))^;
    TSE2ClassPointerList.FreeData(ARecord, Meta);

    ARecord := Pointer(PtrInt(ARecord) - SizeOf(Pointer));
    FMM.FreeMem(ARecord, Meta.ParamCount + SizeOf(Pointer));
  end;

end;

{
{var iSize   : integer;
    runInfo : TSE2ClassPointerList;
    runCalls: TSE2DynMethodList;
begin
  if ARecord <> nil then
  begin
    iSize  := PInteger(PtrInt(ARecord) + vmtScriptInstanceSize)^;
    runInfo := PPointer(PtrInt(ARecord) + vmtScriptPointerList)^;
    runInfo.FreeData(ARecord);
    runInfo.Free;
    runCalls := PPointer(PtrInt(ARecord) + vmtScriptMethodList)^;
    runCalls.Free;

    ARecord := Pointer(PtrInt(ARecord) - vmtScriptMetaTotalSize);
    FMM.FreeMem(ARecord, iSize + vmtScriptMetaTotalSize);
  end;
end;   }

function TSE2RunTimeClasses.DuplicateScriptRecord(
  ARecord: Pointer): Pointer;
var iSize   : integer;
    pReal   : Pointer;
    runInfo : TSE2ClassPointerList;
    runCalls: TSE2DynMethodList;

begin
  result := nil;
  if ARecord <> nil then
  begin
    pReal  := Pointer(PtrInt(ARecord) - vmtScriptMetaTotalSize);
    iSize  := PInteger(PtrInt(ARecord) + vmtScriptInstanceSize)^;
    FMM.GetMem(result, iSize + vmtScriptMetaTotalSize);
    Move(pReal^, result^, iSize + vmtScriptMetaTotalSize);
    result := Pointer(cardinal(result) + vmtScriptMetaTotalSize);

    runInfo := TSE2ClassPointerList.Create(Self);
    runInfo.Assign(TSE2ClassPointerList(PPointer(PtrInt(result) + vmtScriptPointerList)^));
    PPointer(integer(result) + vmtScriptPointerList)^  := runInfo;

    runCalls := TSE2DynMethodList.Create;
    runCalls.Assign(TSE2DynMethodList(PPointer(PtrInt(result) + vmtScriptMethodList)^));
    PPointer(PtrInt(result) + vmtScriptMethodList)^   := runCalls;

    runInfo.CreateData(result, ARecord);
  end;
end;

function TSE2RunTimeClasses.GetClassMeta(AClass: Pointer): TSE2MetaEntry;
begin
  result := PPointer(PtrInt(AClass) + vmtScriptMetaEntry)^;
end;

function TSE2RunTimeClasses.GetClassMethods(
  AClass: Pointer): TSE2DynMethodList;
begin                          
  result := PPointer(PtrInt(AClass) + vmtScriptMethodList)^;
end;
                 {
procedure TSE2RunTimeClasses.DelphiToScriptRecord(ASource, ADest: Pointer);
var PtrSave : TSE2PointerList;
    runInfo : TSE2ClassPointerList;
    iSize   : integer;
begin
  if (ASource <> nil) and (ADest <> nil) then
  begin
    PtrSave := TSE2PointerList.Create(Self);
    try

      runInfo := PPointer(PtrInt(ADest) + vmtScriptPointerList)^;
      PtrSave.PushPointers(ADest, runInfo);

      iSize := PInteger(PtrInt(ADest) + vmtScriptInstanceSize)^;
      Move(ASource^, ADest^, iSize);

      PtrSave.PopPointers(ADest);
      runInfo.DelphiToScriptRecDat(ASource, ADest);

    finally
      PtrSave.Free;
    end;
  end;
end;      }
              {
function TSE2RunTimeClasses.ScriptToDelphiRecord(
  ASource: Pointer): Pointer;
var iSize   : integer;
begin
  result := nil;
  if ASource <> nil then
  begin
    iSize := PInteger(PtrInt(ASource) + vmtScriptInstanceSize)^;
    GetMem(result, iSize);
    FillChar(result^, iSize, 0);
    ScriptToDelphiRecord(ASource, result);
  end;
end;        }
                  {
procedure TSE2RunTimeClasses.ScriptToDelphiRecord(ASource, ADest: Pointer);
var runInfo : TSE2ClassPointerList;
    iSize   : integer;
    PtrSave : TSE2PointerList;
begin
  if (ASource <> nil) and (ADest <> nil) then
  begin
    PtrSave := TSE2PointerList.Create(Self);

    runInfo := PPointer(PtrInt(ASource) + vmtScriptPointerList)^;
    PtrSave.PushPointers(ADest, runInfo);

    iSize := PInteger(PtrInt(ASource) + vmtScriptInstanceSize)^;
    Move(ASource^, ADest^, iSize);

    PtrSave.PopPointers(ADest);
    runInfo.ScriptToDelphiRecData(ASource, ADest);

    PtrSave.Free;
  end;
end;            }

function TSE2RunTimeClasses.ScriptToDelphiRecord(ASource: Pointer;
  Meta: TSE2MetaEntry): Pointer;
begin
  result := nil;
  if ASource <> nil then
  begin
    FMM.GetMem(result, Meta.ParamCount);
    FillChar(result^, Meta.ParamCount, 0);
    ScriptToDelphiRecord(ASource, result, Meta);
  end;
end;

procedure TSE2RunTimeClasses.ScriptToDelphiRecord(ASource, ADest: Pointer;
  Meta: TSE2MetaEntry);
begin
  if (ASource <> nil) and (ADest <> nil) then
  begin
    TSE2PointerList(FPtr).PushPointers(ADest, Meta);
    Move(ASource^, ADest^, Meta.ParamCount);
    TSE2PointerList(FPtr).PopPointers(ADest);
    if Meta.RTTI.Count > 0 then
       TSE2ClassPointerList.ScriptToDelphiRecData(ASource, ADest, Meta);
    TSE2PointerList(FPtr).Clear;
  end;
end;

procedure TSE2RunTimeClasses.DelphiToScriptRecord(ASource, ADest: Pointer;
  Meta: TSE2MetaEntry);
begin
  if (ASource <> nil) and (ADest <> nil) then
  begin
    TSE2PointerList(FPtr).PushPointers(ADest, Meta);
    Move(ASource^, ADest^, Meta.ParamCount);
    TSE2PointerList(FPtr).PopPointers(ADest);
    if Meta.RTTI.Count > 0 then
       TSE2ClassPointerList.DelphiToScriptRecDat(ASource, ADest, Meta);
    TSE2PointerList(FPtr).Clear;
  end;
end;

function TSE2RunTimeClasses.ScriptRecordEqual(ARec1, ARec2: Pointer;
  Meta: TSE2MetaEntry): boolean;
begin
  result := CompareMem(ARec1, ARec2, Meta.ParamCount);
end;

end.
