library Streams;

{$MODE OBJFPC}{$H+}

uses
  SysUtils,
  Classes,
  uSE2PackageAPI,
  uBasicStreams in 'units\uBasicStreams.pas',
  uMemoryStream in 'units\uMemoryStream.pas',
  uResourceStream in 'units\uResourceStream.pas', SEII;

{$R *.res}

// Package Version
procedure PackageMinimumVersion(var Version: TSE2Version); stdcall;
begin
  Version.Major := 0;
  Version.Minor := 3;
  Version.Patch := 6;
  Version.Build := 0;
end;

procedure PackageGetGUID(var GUID: TGUID); stdcall;
begin
  GUID := StringToGUID('{B7972690-6FD1-4206-A972-725417E1564F}');
end;

// Package Initialization
function PackageInitialize: integer; stdcall;
begin
  result := 0;
end;

var
  oldMM : TMemoryManager;
  newMM : TMemoryManager;
  useMM : boolean;

// Package Finalization
procedure PackageFinalize; stdcall;
begin
  if useMM then
  begin
    SetMemoryManager(oldMM);
    useMM := False;
  end;
end;

procedure PackageSetMM(const MemoryManager: TMemoryManager); stdcall;
begin
  if not useMM then
  begin
    GetMemoryManager(oldMM);
    newMM := MemoryManager;
    SetMemoryManager(newMM);
    useMM := True;
  end;
end;

function PackageInitModule(Module: TPackageModule): integer; stdcall;
begin
  result := 0;
end;

procedure PackageFinalizeModule(Module: TPackageModule); stdcall;
begin

end;

type
  TDataSendType = (dataName, dataSource);

function PackageNumModules: integer; stdcall;
begin
  result := 3;
end;

procedure PackageRegisterModule(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister); stdcall;
begin
  case Module of
  0 : CBasicStreamsRegister(Module, Data, CallBack);
  1 : uMemoryStream.RegisterMethods(Module, Data, CallBack);
  2 : uResourceStream.RegisterMethods(Module, Data, CallBack);
  end;
end;

function GetModuleData(Module: TPackageModule; pName: PAnsiChar; BuffSize: integer; SendType: TDataSendType): integer;
var s: string;
begin
  result := -1;
  if not (SendType in [dataName, dataSource]) then
     exit;
  case Module of
  0     :
      begin
        case SendType of
        dataName   : s := CBasicStreamsName;
        dataSource : s := CBasicStreamsSource;
        end;
      end;
  1     :
      begin
        case SendType of
        dataName   : s := uMemoryStream.C_UnitName;
        dataSource : s := uMemoryStream.C_UnitSource;
        end;
      end;
  2     :
      begin
        case SendType of
        dataName   : s := uResourceStream.C_UnitName;
        dataSource : s := uResourceStream.C_UnitSource;
        end;
      end;
  else exit;
  end;

  if pName = nil then
     result := length(s)
  else
  begin
    if BuffSize < length(s) then
       result := -1
    else
    begin
      // Copy the content to the buffer
      Move(s[1], pName^, length(s));
      result := length(pName);
    end;
  end;
end;

procedure PackageModuleSize(Module: TPackageModule; var NameLen, SourceLen: integer); stdcall;
begin
  NameLen   := GetModuleData(Module, nil, 0, dataName);
  SourceLen := GetModuleData(Module, nil, 0, dataSource);
end;

function PackageGetModuleName(Module: TPackageModule; pName: PAnsiChar; BuffSize: integer): integer; stdcall;
begin
  result := GetModuleData(Module, pName, BuffSize, dataName);
end;

function PackageGetModuleSource(Module: TPackageModule; pSource: PAnsiChar; BuffSize: integer): integer; stdcall;
begin
  result := GetModuleData(Module, pSource, BuffSize, dataSource);
end;


exports
  PackageMinimumVersion name CSE2PackageMinVersion,
  PackageGetGUID name CSE2PackageGUID,
  PackageInitialize name CSE2PackageInitialize,
  PackageFinalize name CSE2PackageFinalize,
  PackageSetMM name CSE2PackageSetMM,
  PackageNumModules name CSE2PackageNumModules,
  PackageInitModule name CSE2PackageInitModule,
  PackageFinalizeModule name CSE2PackageFinalizeModule,
  PackageModuleSize name CSE2PackageModuleSize,
  PackageGetModuleName name CSE2PackageGetModuleName,
  PackageGetModuleSource name CSE2PackageGetModuleSource,
  PackageRegisterModule name CSE2PackageRegisterModule;


end.
 
