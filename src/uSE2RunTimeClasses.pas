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
    function  ScriptRecordEqual(ARec1, ARec2: Pointer; Meta: TSE2MetaEntry): boolean;

    { Arrays }
    function  CreateScriptArray(const Meta: TSE2MetaEntry; PE: TSE2PE): Pointer;
    procedure DestroyScriptArray(AArray: Pointer);
    function  CreateScriptArrayLength(ElemCount: integer; const Meta: TSE2MetaEntry; PE: TSE2PE): Pointer;
    function  GetScriptArrayLength(AArray: Pointer): integer;

    { Other }
    procedure CopyScriptRecord(ASource, ADest: Pointer; Meta: TSE2MetaEntry);
    procedure CopyScriptArray(ASource, ADest: Pointer; len: integer; Meta: TSE2MetaEntry);

    procedure DelphiToScriptRecord(ASource, ADest: Pointer; Meta: TSE2MetaEntry); overload;
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

    procedure Clear;
    procedure Add(aType: TSE2TypeIdent; Offset, Size: integer);

    class procedure CopyData(ASource, ADest: Pointer; Meta: TSE2MetaEntry); overload;
    
    class procedure DelphiToScriptRecDat(ASource, ADest: Pointer; Meta: TSE2MetaEntry);
    class procedure ScriptToDelphiRecData(ASource, ADest: Pointer; Meta: TSE2MetaEntry);

    procedure CreateData(ADest: Pointer; Meta: TSE2MetaEntry; PE: TSE2PE); overload;
    procedure CreateData(AClass: Pointer; PE: TSE2PE); overload;
    //procedure CreateData(AClass, ADataSource: Pointer); overload;

    procedure FreeData(AClass: Pointer); overload;
    procedure FreeData(AData: Pointer; Meta: TSE2MetaEntry); overload;
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
    procedure PushPointers(Data: Pointer; Meta: TSE2MetaEntry; Clear: boolean; CompStrPtr: TSE2StrPtrStyle = spsSE2);
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
    btAnsiString     :
        begin
          PbtAnsiString(PPointer(PtrInt(ADest) + p.Offset)^)^ := PbtAnsiString(PPointer(PtrInt(ASource) + p.Offset)^)^;
        end;
    btPAnsiChar      :
        begin
          PbtPAnsiChar(PPointer(PtrInt(ADest) + p.Offset)^)^ := PbtPAnsiChar(PPointer(PtrInt(ASource) + p.Offset)^)^;
        end;
    btPWideChar      :
        begin
          PbtPWideChar(PPointer(PtrInt(ADest) + p.Offset)^)^ := PbtPWideChar(PPointer(PtrInt(ASource) + p.Offset)^)^;
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

procedure TSE2ClassPointerList.CreateData(AClass: Pointer; PE: TSE2PE);
var i  : integer;
    p  : PSE2ClassPointerEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pAS: PbtAnsiString;
    pAC: PbtPAnsiChar;
    pWC: PbtPWideChar;
    pP : Pointer;
    Meta : TSE2MetaEntry;
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
    btAnsiString     :
        begin
          New(pAS);
          pAS^ := '';
          PPointer(PtrInt(AClass) + p.Offset)^ := pAS;
        end;   
    btPAnsiChar      :
        begin
          New(pAC);
          pAC^ := '';
          PPointer(PtrInt(AClass) + p.Offset)^ := pAC;
        end;          
    btPWideChar      :
        begin
          New(pWC);
          pWC^ := '';
          PPointer(PtrInt(AClass) + p.Offset)^ := pWC;
        end;
    btArray      :
        begin
          Meta := PE.MetaData[p.Size];
          if Meta.ParamCount >= 0 then
            pP := FRT.CreateScriptArrayLength( Meta.ParamCount div Meta.DynIndex, Meta, PE )
          else
            pP := FRT.CreateScriptArray(Meta, PE);
          PPointer(PtrInt(AClass) + p.Offset)^ := pP;
        end;
    btRecord     :
        begin
          pP := FRT.CreateScriptRecord( PE.MetaData[p.Size], PE );
          PPointer(PtrInt(AClass) + p.Offset)^ := pP;
        end;
    end;
  end;
end;

procedure TSE2ClassPointerList.CreateData(ADest: Pointer;
  Meta: TSE2MetaEntry; PE: TSE2PE);
var i  : integer;
    p  : PSE2RTTIEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pAS: PbtAnsiString;
    pAC: PbtPAnsiChar;
    pWC: PbtPWideChar;
    pP : Pointer;

    m  : TSE2MetaEntry;
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
    btAnsiString     :
        begin
          New(pAS);
          pAS^ := '';
          PPointer(PtrInt(ADest) + p.Offset)^ := pAS;
        end; 
    btPAnsiChar      :
        begin
          New(pAC);
          pAC^ := '';
          PPointer(PtrInt(ADest) + p.Offset)^ := pAC;
        end;    
    btPWideChar      :
        begin
          New(pWC);
          pWC^ := '';
          PPointer(PtrInt(ADest) + p.Offset)^ := pWC;
        end;
    btArray      :
        begin
          m := PE.MetaData[p.Size];
          if m.ParamCount >= 0 then
            pP := FRT.CreateScriptArrayLength( m.ParamCount div m.DynIndex, Meta, PE )
          else
            pP := FRT.CreateScriptArray(m, PE);
          PPointer(PtrInt(ADest) + p.Offset)^ := pP;
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
    pAS: PbtAnsiString;
    pAC: PbtPAnsiChar;
    pWC: PbtPWideChar;
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
    btAnsiString     :
        begin
          pAS := PPointer(PtrInt(AClass) + p.Offset)^;
          pAS^ := '';
          Dispose(pAS);
        end; 
    btPAnsiChar      :
        begin
          pAC := PPointer(PtrInt(AClass) + p.Offset)^;
          pAC^ := '';
          Dispose(pAC);
        end;    
    btPWideChar      :
        begin
          pWC := PPointer(PtrInt(AClass) + p.Offset)^;
          pWC^ := '';
          Dispose(pWC);
        end;
    btArray      :
        begin
          pP := PPointer(PtrInt(AClass) + p^.Offset)^;
          FRT.DestroyScriptArray(pP);
        end;
    btRecord     :
        begin
          pP := PPointer(Pointer(PtrInt(AClass) + p^.Offset))^;
          FRT.DestroyScriptRecord(pP);
        end;
    end;
  end;
end;

procedure TSE2ClassPointerList.FreeData(AData: Pointer;
  Meta: TSE2MetaEntry);
var i  : integer;
    p  : PSE2RTTIEntry;

    pS : PbtString;
    pW : PbtWideString;
    pU : PbtUTF8String;
    pC : PbtPChar;
    pAS: PbtAnsiString;
    pAC: PbtPAnsiChar;
    pWC: PbtPWideChar;
    pP : Pointer;
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
    btAnsiString     :
        begin
          pAS := PPointer(PtrInt(AData) + p.Offset)^;
          pAS^ := '';
          Dispose(pAS);
        end;
    btPAnsiChar      :
        begin
          pAC := PPointer(PtrInt(AData) + p.Offset)^;
          pAC^ := '';
          Dispose(pAC);
        end;
    btPWideChar      :
        begin
          pWC := PPointer(PtrInt(AData) + p.Offset)^;
          pWC^ := '';
          Dispose(pWC);
        end;
    btArray      :
        begin
          pP := PPointer(PtrInt(AData) + p^.Offset)^;
          FRT.DestroyScriptArray(pP);
        end;
    btRecord     :
        begin       
          pP := PPointer(PtrInt(AData) + p^.Offset)^;
          FRT.DestroyScriptRecord(pP);
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
    btAnsiString,
    btPAnsiChar,
    btPWideChar,
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
    btAnsiString,
    btPWideChar,
    btPAnsiChar,
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

procedure TSE2PointerList.PushPointers(Data: Pointer; Meta: TSE2MetaEntry;
  Clear: boolean; CompStrPtr: TSE2StrPtrStyle);
var i  : integer;
    p  : PSE2RTTIEntry;
    Entry : PSE2PointerEntry;
begin
  if Clear then
     Self.Clear;
  FList.Count := Meta.RTTI.Count;;
  for i:=0 to Meta.RTTI.Count-1 do
  begin
    p := Meta.RTTI[i];
    case p.aType of
    btString,
    btUTF8String,
    btWideString,
    btPAnsiChar,
    btPWideChar,
    btAnsiString,
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
    btArray  :
        begin
          New(Entry);
          Entry^.Offset := p.Offset;
          Entry^.Value  := PPointer(PtrInt(Data) + p.Offset)^;
          FList[i]:= Entry;
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
    TSE2PointerList(FPtr).PushPointers(ADest, Meta, True);
    Move(ASource^, ADest^, Meta.ParamCount);
    TSE2PointerList(FPtr).PopPointers(ADest);
    TSE2ClassPointerList.CopyData(ASource, ADest, Meta);
    TSE2PointerList(FPtr).Clear;
  end;
end;

procedure TSE2RunTimeClasses.CopyScriptArray(ASource, ADest: Pointer; len: integer;
  Meta: TSE2MetaEntry);
var i: integer;
begin
  if (ASource <> nil) and (ADest <> nil) then
  begin
    if Meta.RTTI.Count > 0 then
    begin
      TSE2PointerList(FPtr).Clear;
      for i:=0 to len-1 do
        TSE2PointerList(FPtr).PushPointers(Pointer(PtrInt(ADest) + i * Meta.DynIndex), Meta, False);
    end;
    Move(ASource^, ADest^, Meta.DynIndex * len);
    if Meta.RTTI.Count > 0 then
    begin                                 
      for i:=0 to len-1 do
      begin
        TSE2PointerList(FPtr).PopPointers(Pointer(PtrInt(ADest) + i * Meta.DynIndex));
        TSE2ClassPointerList.CopyData(Pointer(PtrInt(ASource) + i * Meta.DynIndex), Pointer(PtrInt(ADest) + i * Meta.DynIndex), Meta);
      end;
      TSE2PointerList(FPtr).Clear;
    end;
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
      FillChar(result^, Meta.ParamCount + SizeOf(Pointer), 0);

      PPointer(result)^ := Meta;
      result := Pointer(PtrInt(result) + SizeOf(Pointer));

      TSE2ClassPointerList(FPtr).CreateData(result, Meta, PE);
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
    TSE2ClassPointerList(FPtr).FreeData(ARecord, Meta);

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

function TSE2RunTimeClasses.GetClassMeta(AClass: Pointer): TSE2MetaEntry;
begin
  result := PPointer(PtrInt(AClass) + vmtScriptMetaEntry)^;
end;

function TSE2RunTimeClasses.GetClassMethods(
  AClass: Pointer): TSE2DynMethodList;
begin                          
  result := PPointer(PtrInt(AClass) + vmtScriptMethodList)^;
end;

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
    TSE2PointerList(FPtr).PushPointers(ADest, Meta, True);
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
    TSE2PointerList(FPtr).PushPointers(ADest, Meta, True);
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

function TSE2RunTimeClasses.CreateScriptArray(const Meta: TSE2MetaEntry;
  PE: TSE2PE): Pointer;
begin
  result := nil;
  if Meta <> nil then
    if Meta.MetaType = mtArray then
    begin
      FMM.GetMem(result, SizeOf(integer) + SizeOf(Pointer));
      FillChar(result^, SizeOf(integer) + SizeOf(Pointer), 0);

      PPointer(result)^ := Meta;
      result := Pointer(PtrInt(result) + SizeOf(Pointer) + SizeOf(integer));

      //TSE2ClassPointerList.CreateData(result, Meta);
    end;
end;

function TSE2RunTimeClasses.CreateScriptArrayLength(ElemCount: integer;
   const Meta: TSE2MetaEntry; PE: TSE2PE): Pointer;
var i: integer;
begin
  result := nil;
  if Meta <> nil then
    if Meta.MetaType = mtArray then
    begin
      FMM.GetMem(result, SizeOf(integer) + SizeOf(Pointer) + Meta.DynIndex * ElemCount);
      FillChar(result^, SizeOf(integer) + SizeOf(Pointer) + Meta.DynIndex * ElemCount, 0);

      PPointer(result)^ := Meta;
      result := Pointer(PtrInt(result) + SizeOf(Pointer));
      PInteger(result)^ := ElemCount;
      result := Pointer(PtrInt(result) + SizeOf(integer));

      if Meta.RTTI.Count > 0 then
      begin
        for i:=0 to ElemCount-1 do
          TSE2ClassPointerList(FPtr).CreateData(Pointer(PtrInt(result) + i * Meta.DynIndex), Meta, PE);
      end;
    end;
end;

procedure TSE2RunTimeClasses.DestroyScriptArray(AArray: Pointer);
var Meta: TSE2MetaEntry;
    i   : integer;
begin
  if AArray <> nil then
  begin
    Meta    := PPointer(PtrInt(AArray) - SizeOf(Pointer) - SizeOf(integer))^;

    if (PInteger(PtrInt(AArray) - SizeOf(integer))^ > 0) and (Meta.RTTI.Count > 0) then
    begin
      for i:=0 to PInteger(PtrInt(AArray) - SizeOf(integer))^ - 1 do
        TSE2ClassPointerList(FPtr).FreeData(Pointer(PtrInt(AArray) + i * Meta.DynIndex), Meta);
    end;

    AArray := Pointer(PtrInt(AArray) - SizeOf(Pointer) - SizeOf(integer));
    FMM.FreeMem(AArray);
  end;
end;

function TSE2RunTimeClasses.GetScriptArrayLength(AArray: Pointer): integer;
begin
  result := PInteger(PtrInt(AArray) - SizeOf(integer))^;
end;

end.
