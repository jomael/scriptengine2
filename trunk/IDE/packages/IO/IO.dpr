library IO;

{$R *.res}

uses
  SysUtils,
  Classes,
  uSE2PackageAPI in '..\..\units\Script Engine\Package\uSE2PackageAPI.pas',
  uBasicModule in 'units\uBasicModule.pas',
  uFileStreamModule in 'units\uFileStreamModule.pas',
  uFileAccess in 'units\uFileAccess.pas';

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
  GUID := StringToGUID('{4AAA0561-5188-4871-839D-4DD50D8373BD}');
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

function  PackageIsExtender(Module: TPackageModule): boolean; stdcall;
begin
  case Module of
  1   : result := True;
  else  result := False;
  end;
end;

procedure PackageRegisterModule(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister); stdcall;
begin
  case Module of
  0 : CBasicModuleRegister(Module, Data, CallBack);
  1 : CFileStreamModuleRegister(Module, Data, CallBack);
  2 : uFileAccess.RegisterMethods(Module, Data, CallBack);
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
        dataName   : s := CBasicModuleName;
        dataSource : s := CBasicModuleSource;
        end;
      end;
  1    :
      begin
        case SendType of
        dataName   : s := CFileStreamModuleName;
        dataSource : s := CFileStreamModuleSource;
        end;
      end;
  2    :
      begin
        case SendType of
        dataName   : s := uFileAccess.C_UnitName;
        dataSource : s := uFileAccess.C_UnitSource;
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
  PackageIsExtender name CSE2PackageIsExtender,
  PackageGetModuleName name CSE2PackageGetModuleName,
  PackageGetModuleSource name CSE2PackageGetModuleSource,
  PackageRegisterModule name CSE2PackageRegisterModule;


end.
