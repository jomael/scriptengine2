unit uSE2ScriptLibrary;

{$INCLUDE ScriptEngine.inc}

interface

{$IFDEF SEII_FPC}
  {$HINTS OFF}
  {$WARNINGS OFF}
{$ENDIF}

uses
  Classes, SysUtils, uSE2Consts, uSE2BaseTypes, uSE2UnitManager, uSE2RunAccess;

type
  TSE2ScriptLibrary = class
  private
    FStream      : TStream;
    FGuid        : TGUID;
    FMinVersion  : TSE2ScriptEngineVersion;
    FUnitNames   : TStringList;
    FUnitSources : TList;
  protected
    function GetSource(index: integer): string;
  public
    constructor Create(Stream: TStream; AutoLoad: boolean); reintroduce;
    destructor Destroy; override;

    procedure  Clear;

    procedure  AddUnit(const UnitName, UnitSource: string);
    procedure  Save;
    procedure  Load;

    property   Guid        : TGUID       read FGuid;
    property   UnitNames   : TStringList read FUnitNames;
    property   Source[index: integer]: string read GetSource;
  end;

  TSE2ScriptLibraryUnit = class(TSE2ManagedUnit)
  private
    FLoadTime        : TDateTime;
    FPackage         : TSE2ScriptLibrary;
  protected
    function  GetNumModules: Integer; override;
    function  GetNameHash(index: Integer): Integer; override;

    function  GetModuleName(index: Integer): String; override;
    procedure SetModuleName(index: Integer; value: String); override; 
    function  GetIsExtender(index: Integer): Boolean; override;

    function  GetGuid: TGUID;
    function  GetMinVersion: TSE2ScriptEngineVersion;
  public
    constructor Create(Package: TSE2ScriptLibrary); reintroduce; overload;
    constructor Create(Stream: TStream; AutoLoad: boolean); reintroduce; overload;
    destructor  Destroy; override;

    function  CanCacheSource(index: integer): Boolean; override;
    function  LastChangeTime(index: integer): TDateTime; override;

    procedure GetUnitSource(index: integer; var Target: String); override;
    procedure RegisterMethods(index: integer; const Target: TSE2RunAccess); override;
    procedure RegisterVariables(index: integer; const Target: TSE2RunAccess); override;
    procedure RegisterExceptions(index: Integer; const Target: TSE2RunAccess); override;


    property  GUID       : TGUID                   read GetGuid;
    property  LoadTime   : TDateTime               read FLoadTime;
    property  MinVersion : TSE2ScriptEngineVersion read GetMinVersion;
    property  Package    : TSE2ScriptLibrary       read FPackage;
  end;

implementation

type
  TSE2PackageStreamId = array[0..4] of AnsiChar;

const
  PackageStreamId : TSE2PackageStreamId = 'SE2PS';

{ TSE2ScriptLibrary }

procedure TSE2ScriptLibrary.AddUnit(const UnitName, UnitSource: string);
var p: PString;
begin
  FUnitNames.Add(UnitName);

  New(p);
  p^ := UnitSource;
  FUnitSources.Add(p);
end;

procedure TSE2ScriptLibrary.Clear;
var i: integer;
begin
  for i:=FUnitSources.Count-1 downto 0 do
  begin
    PString(FUnitSources[i])^ := '';
    Dispose(PString(FUnitSources[i]));
  end;
  FUnitSources.Clear;
  FUnitNames.Clear;
end;

constructor TSE2ScriptLibrary.Create(Stream: TStream; AutoLoad: boolean);
begin
  inherited Create;
  FStream := Stream;

  FUnitNames := TStringList.Create;
  FUnitSources := TList.Create;

  if AutoLoad then
     Load;
end;

destructor TSE2ScriptLibrary.Destroy;
begin
  Clear;

  FUnitNames.Free;
  FUnitSources.Free;
  FStream.Free;
  inherited;
end;

function TSE2ScriptLibrary.GetSource(index: integer): string;
begin
  if (index < 0) or (index >= FUnitSources.Count) then
     result := ''
  else
     result := PString(FUnitSources[index])^;
end;

procedure TSE2ScriptLibrary.Load;
var vers     : byte;
    id       : TSE2PackageStreamId;
    encoding : TSE2StringEncoding;
    i        : integer;
    s        : PString;
begin
  Clear;

  if FStream.Read(id, SizeOf(id)) < SizeOf(id) then
     raise ESE2InvalidDataStream.Create('Package Stream Id not readable');
  if id <> PackageStreamId then
     raise ESE2InvalidDataStream.Create('Package Stream Id not found');

  if FStream.Read(vers, SizeOf(vers)) < SizeOf(vers) then
     raise ESE2InvalidDataStream.Create('Invalid script package stream');

  case vers of
  1 :
      begin
        FStream.Read(FGuid, SizeOf(FGuid));
        FStream.Read(FMinVersion, SizeOf(FMinVersion));

        if not SE2VersionIsInRange(FMinVersion, CSE2Version) then
           raise ESE2PackageIncompatible.Create('The script package was created with a newer version');

        if FStream.Read(encoding, SizeOf(encoding)) < SizeOf(encoding) then
           raise ESE2InvalidDataStream.Create('Failed to read encoding');

        // Unit Names
        FUnitNames.Text    := TSE2StreamHelper.ReadString(FStream, encoding);
        // Unit Source

        for i:=0 to FUnitNames.Count-1 do
        begin
          New(s);
          s^ := TSE2StreamHelper.ReadString(FStream, encoding);
          FUnitSources.Add(s);
        end;
      end;
  else
      raise ESE2InvalidDataStream.Create('Unsupported package stream version found');
  end;
end;

procedure TSE2ScriptLibrary.Save;
var vers     : byte;
    id       : TSE2PackageStreamId;
    encoding : TSE2StringEncoding;
    i        : integer;
begin
  id := PackageStreamId;
  FStream.Write(id, SizeOf(id));

  vers := 1;
  FStream.Write(vers, SizeOf(vers));

  CreateGUID(FGuid);
  FMinVersion := CSE2Version;

  FStream.Write(FGuid, SizeOf(FGuid));
  FStream.Write(FMinVersion, SizeOf(FMinVersion));

  encoding := encodingUnicode;
  FStream.Write(encoding, SizeOf(encoding));

  // Unit Names
  TSE2StreamHelper.WriteString(FStream, encoding, FUnitNames.Text);
                   
  // Unit Source
  for i:=0 to FUnitNames.Count-1 do
    TSE2StreamHelper.WriteString(FStream, encoding, PString(FUnitSources[i])^);
end;

{ TSE2ScriptLibraryUnit }

function TSE2ScriptLibraryUnit.CanCacheSource(index: integer): Boolean;
begin
  result := True;
end;

constructor TSE2ScriptLibraryUnit.Create(Package: TSE2ScriptLibrary);
begin
  inherited Create();
  FPackage  := Package;
  FLoadTime := Now;
end;

constructor TSE2ScriptLibraryUnit.Create(Stream: TStream;
  AutoLoad: boolean);
begin
  inherited Create;
  FPackage  := TSE2ScriptLibrary.Create(Stream, AutoLoad);
  FLoadTime := Now;
end;

destructor TSE2ScriptLibraryUnit.Destroy;
begin
  FPackage.Free;
  inherited;
end;

function TSE2ScriptLibraryUnit.GetGuid: TGUID;
begin
  result := FPackage.Guid;
end;

function TSE2ScriptLibraryUnit.GetIsExtender(index: Integer): Boolean;
begin
  result := False;
end;

function TSE2ScriptLibraryUnit.GetMinVersion: TSE2ScriptEngineVersion;
begin
  result := FPackage.FMinVersion;
end;

function TSE2ScriptLibraryUnit.GetModuleName(index: Integer): String;
begin
  result := FPackage.UnitNames[index];
end;

function TSE2ScriptLibraryUnit.GetNameHash(index: Integer): Integer;
begin
  result := MakeHash(GetModuleName(index));
end;

function TSE2ScriptLibraryUnit.GetNumModules: Integer;
begin
  result := FPackage.UnitNames.Count;
end;

procedure TSE2ScriptLibraryUnit.GetUnitSource(index: integer;
  var Target: String);
begin
  Target := FPackage.Source[index];
end;

function TSE2ScriptLibraryUnit.LastChangeTime(index: integer): TDateTime;
begin
  result := FLoadTime;
end;

procedure TSE2ScriptLibraryUnit.RegisterExceptions(index: Integer;
  const Target: TSE2RunAccess);
begin
  inherited;
end;

procedure TSE2ScriptLibraryUnit.RegisterMethods(index: integer;
  const Target: TSE2RunAccess);
begin
  inherited;
end;

procedure TSE2ScriptLibraryUnit.RegisterVariables(index: integer;
  const Target: TSE2RunAccess);
begin
  inherited;
end;

procedure TSE2ScriptLibraryUnit.SetModuleName(index: Integer;
  value: String);
begin

end;

end.
