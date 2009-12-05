library Collections;


uses
  SysUtils,
  Classes,
  uSE2PackageAPI in '..\..\units\Script Engine\Package\uSE2PackageAPI.pas',
  uListImport in 'units\uListImport.pas',
  uThreadListImport in 'units\uThreadListImport.pas',
  uStringsImport in 'units\uStringsImport.pas',
  uStringListImport in 'units\uStringListImport.pas',
  uIntegerListImport in 'units\uIntegerListImport.pas',
  uIntegerList in 'units\uIntegerList.pas',
  uFloatListImport in 'units\uFloatListImport.pas',
  uFloatList in 'units\uFloatList.pas';

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
  GUID := StringToGUID('{AAEC4CC9-525D-491C-9741-F70D695B15BE}');
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
  result := 6;
end;

procedure PackageRegisterModule(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister); stdcall;
begin
  case Module of
  0 : uListImport.RegisterMethods(Module, Data, CallBack);
  1 : uThreadListImport.RegisterMethods(Module, Data, CallBack);
  2 : uStringsImport.RegisterMethods(Module, Data, CallBack);
  3 : uStringListImport.RegisterMethods(Module, Data, CallBack);
  4 : uIntegerListImport.RegisterMethods(Module, Data, CallBack);
  5 : uFloatListImport.RegisterMethods(Module, Data, CallBack);
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
      case SendType of
      dataName   : s := uListImport.C_UnitName;
      dataSource : s := uListImport.C_UnitSource;
      end;
  1     :
      case SendType of
      dataName   : s := uThreadListImport.C_UnitName;
      dataSource : s := uThreadListImport.C_UnitSource;
      end;
  2     :
      case SendType of
      dataName   : s := uStringsImport.C_UnitName;
      dataSource : s := uStringsImport.C_UnitSource;
      end;
  3     :
      case SendType of
      dataName   : s := uStringListImport.C_UnitName;
      dataSource : s := uStringListImport.C_UnitSource;
      end;
  4     :
      case SendType of
      dataName   : s := uIntegerListImport.C_UnitName;
      dataSource : s := uIntegerListImport.C_UnitSource;
      end;
  5     :
      case SendType of
      dataName   : s := uFloatListImport.C_UnitName;
      dataSource : s := uFloatListImport.C_UnitSource;
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
