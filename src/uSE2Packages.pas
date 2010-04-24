unit uSE2Packages;

{$INCLUDE ScriptEngine.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, uSE2Consts, uSE2BaseTypes, uSE2UnitManager, uSE2RunAccess, uSE2PackageAPI;

type
  TSE2PackageUnit = class(TSE2ManagedUnit)
  private
    FLoadTime        : TDateTime;
    FDLLHandle       : THandle;
    FFileName        : TFileName;
    FOwnsHandle      : boolean;
    FPackageLoaded   : boolean;
    FCurrentUnitName : string;
    FMinVersion      : TSE2Version;
    FGUID            : TGUID;

    FGetIsExtender   : TSE2PackageIsExtender;
    FGetNumModules   : TSE2PackageNumModules;
    FGetModuleSize   : TSE2PackageModuleSize;
    FGetModuleName   : TSE2PackageGetModuleName;
    FGetModuleSource : TSE2PackageGetModuleSource;
    FRegisterMethods : TSE2PackageRegisterModule;
    FRegisterExcept  : TSE2PackageRegExceptions;
  protected
    function  PackageGetNumModules: integer;
    procedure PackageGetModuleName(index: integer; var Target: AnsiString);
    procedure PackageGetModuleSource(index: integer; var Target: AnsiString);
    procedure PackageRegisterMethods(index: integer; const Target: TSE2RunAccess);
    function  PackageIsExtender(index: integer): boolean;
  protected
    function  InitializePackage: boolean;
    procedure FinalizePackage;



    function  GetNumModules: Integer; override;
    function  GetNameHash(index: Integer): Integer; override;

    function  GetModuleName(index: Integer): String; override;
    procedure SetModuleName(index: Integer; value: String); override; 
    function  GetIsExtender(index: Integer): Boolean; override;

  public
    constructor Create(DLLHandle: THandle); reintroduce; overload;
    constructor Create(PackageName: TFileName); reintroduce; overload;
    destructor  Destroy; override;

    function  CanCacheSource(index: integer): Boolean; override;
    function  LastChangeTime(index: integer): TDateTime; override;

    procedure GetUnitSource(index: integer; var Target: String); override;
    procedure RegisterMethods(index: integer; const Target: TSE2RunAccess); override;
    procedure RegisterVariables(index: integer; const Target: TSE2RunAccess); override;
    procedure RegisterExceptions(index: Integer; const Target: TSE2RunAccess); override;


    property  GUID       : TGUID       read FGUID;
    property  LoadTime   : TDateTime   read FLoadTime;
    property  MinVersion : TSE2Version read FMinVersion;
    property  DLLHandle  : THandle     read FDLLHandle;
    property  FileName   : TFileName   read FFileName;
    property  OwnsHandle : boolean     read FOwnsHandle     write FOwnsHandle;
  end;

implementation

uses
  uSE2DLLMemoryManager;

{ TSE2PackageUnit }

constructor TSE2PackageUnit.Create(PackageName: TFileName);
begin
  inherited Create;

  FFileName  := PackageName;
  FDLLHandle := SE2SafeLoadLibrary(PackageName); // SafeLoadLibrary(PackageName);

  if FDLLHandle = 0 then
     raise ESE2PackageError.CreateFmt('Failed to load package "%s". %s', [PackageName, {$IFDEF MSWINDOWS}SysErrorMessage(GetLastError){$ELSE}''{$ENDIF}] );

  FOwnsHandle := True;
  if not InitializePackage then
     raise ESE2PackageError.CreateFmt('Package is not valid: "%s"', [PackageName]);
end;

constructor TSE2PackageUnit.Create(DLLHandle: THandle);
begin
  inherited Create;

  FDLLHandle := DLLHandle;
  if FDLLHandle = 0 then
     raise ESE2PackageError.Create('Package handle can not be zero');

  FOwnsHandle := False;
  if not InitializePackage then
     raise ESE2PackageError.Create('Package module is not valid');
end;

destructor TSE2PackageUnit.Destroy;
begin
  if FPackageLoaded then
     FinalizePackage;

  if FDLLHandle <> 0 then
    if FOwnsHandle then
    begin
      try
        {$IFDEF MSWINDOWS}
          FreeLibrary(FDLLHandle);
        {$ENDIF}
        {$IFDEF LINUX}
          dlclose(Pointer(FDLLHandle));
        {$ENDIF}
      except
      end;
    end;
  inherited;
end;      

function TSE2PackageUnit.CanCacheSource(index: integer): Boolean;
begin
  result := True;
end;

procedure TSE2PackageUnit.GetUnitSource(index: integer; var Target: String);
var s: AnsiString;
begin
  PackageGetModuleSource(index, s);
  Target := string(s);
end;

function TSE2PackageUnit.LastChangeTime(index: integer): TDateTime;
begin
  result := FLoadTime;
end;

procedure TSE2PackageUnit.RegisterMethods(index: integer; const Target: TSE2RunAccess);
begin
  PackageRegisterMethods(index, Target);
end;

procedure TSE2PackageUnit.RegisterVariables(index: integer; const Target: TSE2RunAccess);
begin
  //
end;

procedure TSE2PackageUnit.FinalizePackage;
var PackFinit       : TSE2PackageFinalize;
    PackModuleFinit : TSE2PackageFinalizeModule;
    i               : integer;
begin
  if not FPackageLoaded then
     exit;

  PackModuleFinit := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageFinalizeModule));
  if Assigned(PackModuleFinit) then
  begin
    for i:=PackageGetNumModules-1 downto 0 do
      PackModuleFinit(i);
  end;

  PackFinit := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageFinalize));
  if Assigned(PackFinit) then
     PackFinit();

  FPackageLoaded := False;
end;

function TSE2PackageUnit.InitializePackage: boolean;
var MM             : TMemoryManager;
    {$IFDEF DELPHI2005UP}
    MMEx           : TMemoryManagerEx;
    {$ENDIF}

    PackVers       : TSE2PackageMinVersion;
    PackGUID       : TSE2PackageGetGUID;
    PackInit       : TSE2PackageInitialize;
    PackFinit      : TSE2PackageFinalize;
    PackMM         : TSE2PackageSetMM;
    {$IFDEF DELPHI2005UP}
    PackMMEx       : TSE2PackageSetMMEx;
    {$ENDIF}
    ModuleInit     : TSE2PackageInitModule;

    i              : integer;
    CurrentVersion : TSE2ScriptEngineVersion;

  function VersionOK: boolean;
  begin
    result := False;
    if CurrentVersion.Major < FMinVersion.Major then
       exit
    else
    if CurrentVersion.Major > FMinVersion.Major then
    begin
      result := True;
      exit;
    end;

    if CurrentVersion.Minor < FMinVersion.Minor then
       exit
    else
    if CurrentVersion.Minor > FMinVersion.Minor then
    begin
      result := True;
      exit;
    end;

    if CurrentVersion.Patch < FMinVersion.Patch then
       exit
    else
    if CurrentVersion.Patch > FMinVersion.Patch then
    begin
      result := True;
      exit;
    end;

    if CurrentVersion.Build >= FMinVersion.Build then
       result := True;
  end;

begin
  result := True;
  if FPackageLoaded then
     exit;

  result := False;
  FLoadTime := Now;    
                      
  PackVers         := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageMinVersion));
  if not Assigned(PackVers) then
     exit;
     
  FillChar(FMinVersion, SizeOf(FMinVersion), 0);
  CurrentVersion := CSE2Version;
  PackVers(FMinVersion);

  if not VersionOK then
     raise ESE2PackageIncompatible.CreateFmt('Package need version %d.%d.%d.%d or above',
                   [FMinVersion.Major, FMinVersion.Minor, FMinVersion.Patch, FMinVersion.Build]);

  PackGUID         := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageGUID));
  if Assigned(PackGUID) then
     PackGUID(FGUID)
  else
     exit;

  PackInit         := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageInitialize));
  if not Assigned(PackInit) then
     exit;

  PackFinit        := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageFinalize));
  if not Assigned(PackFinit) then
     exit;

  PackMM           := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageSetMM));
  {$IFDEF DELPHI2005UP}
  PackMMEx         := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageSetMMEx));
  {$ENDIF}
  FGetNumModules   := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageNumModules));
  if not Assigned(FGetNumModules) then
     exit;

  ModuleInit       := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageInitModule));
  FGetModuleSize   := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageModuleSize));
  if not Assigned(FGetModuleSize) then
     exit;

  FGetModuleName   := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageGetModuleName));
  if not Assigned(FGetModuleName) then
     exit;

  FGetModuleSource := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageGetModuleSource));
  if not Assigned(FGetModuleSource) then
     exit;

  FRegisterMethods := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageRegisterModule));
  if not Assigned(FRegisterMethods) then
     exit;

  FRegisterExcept := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageRegExceptions));

  FGetIsExtender   := GetProcAddress(FDLLHandle, PAnsiChar(CSE2PackageIsExtender));  

  if PackInit() <> 0 then
     exit;

  {$IFDEF DELPHI2005UP}
  if Assigned(PackMMEx) then
  begin
    GetMemoryManager(MMEx);
    PackMMEx(MMEx);
  end else
  {$ENDIF}
  if Assigned(PackMM) then
  begin
    GetMemoryManager(MM);
    PackMM(MM);
  end;

  if Assigned(ModuleInit) then
  begin
    for i:=0 to PackageGetNumModules-1 do
      if ModuleInit(i) <> 0 then
         exit;
  end;

  FPackageLoaded := True;
  result := True;
end;

function TSE2PackageUnit.GetModuleName(index: Integer): String;
var s: AnsiString;
begin
  PackageGetModuleName(index, s);
  result := string(s);
end;

function TSE2PackageUnit.GetNameHash(index: Integer): Integer;
begin
  result := MakeHash(GetModuleName(index));
end;

function TSE2PackageUnit.GetNumModules: Integer;
begin
  result := PackageGetNumModules;
end;

procedure TSE2PackageUnit.SetModuleName(index: Integer; value: String);
begin
  // nothing here
end;

// Package Wrapper

procedure TSE2PackageUnit.PackageGetModuleName(index: integer;
  var Target: AnsiString);
var Buffer            : PAnsiChar;
    nameLen, SourceLen: integer;
    newLen            : integer;
begin
  Target := '';

  if not FPackageLoaded then
     exit;

  if (not Assigned(FGetModuleSize)) or (not Assigned(FGetModuleName)) then
     exit;

  nameLen   := 0;
  SourceLen := 0;
  FGetModuleSize(index, nameLen, SourceLen);

  if nameLen = 0 then
     exit;

  GetMem(Buffer, nameLen + 1);
  try
    FillChar(Buffer^, nameLen + 1, 0);
    newLen := FGetModuleName(index, Buffer, nameLen);
    if (newLen < 1) or (newLen > nameLen) then
       exit;

    SetLength(Target, newLen);
    Move(Buffer^, Target[1], newLen);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TSE2PackageUnit.PackageGetModuleSource(index: integer;
  var Target: AnsiString);
var Buffer            : PAnsiChar;
    nameLen, SourceLen: integer;
    newLen            : integer;
begin
  Target := '';

  if not FPackageLoaded then
     exit;

  if (not Assigned(FGetModuleSize)) or (not Assigned(FGetModuleSource)) then
     exit;

  nameLen   := 0;
  SourceLen := 0;
  FGetModuleSize(index, nameLen, SourceLen);

  if SourceLen = 0 then
     exit;

  GetMem(Buffer, SourceLen + 1);
  try
    FillChar(Buffer^, SourceLen + 1, 0);
    newLen := FGetModuleSource(index, Buffer, SourceLen);
    if (newLen < 1) or (newLen > SourceLen) then
       exit;

    SetLength(Target, newLen);
    Move(Buffer^, Target[1], newLen);
  finally
    FreeMem(Buffer);
  end;
end;

function TSE2PackageUnit.PackageGetNumModules: integer;
begin
  if Assigned(FGetNumModules) then
     result := FGetNumModules
  else
     result := 0;
end;

procedure PackageRegisterCallback(Module: TPackageModule; Data: TSE2RunAccess; MethodPos: Pointer; MethodName: PAnsiChar); stdcall;
begin
  if Data <> nil then
    if Data.TmpObj <> nil then
    begin
      Data.Method[string(MethodName), TSE2PackageUnit(Data.TmpObj).FCurrentUnitName] := MethodPos;
    end;
end;

procedure TSE2PackageUnit.PackageRegisterMethods(index: integer; const Target: TSE2RunAccess);
var sUnitName : AnsiString;
begin
  if not FPackageLoaded then
     exit;

  Target.TmpObj := Self;
  PackageGetModuleName(index, sUnitName);
  FCurrentUnitName := string(sUnitName);

  if Target.HasUnit(FCurrentUnitName) then
    if Assigned(FRegisterMethods) then
       FRegisterMethods(index, Target, @PackageRegisterCallback);
end;

procedure PackageRegisterExceptCallback(Module: TPackageModule; Data: TSE2RunAccess; AClass: Pointer; ATypeName: PAnsiChar); stdcall;
begin
  if Data <> nil then
    if Data.TmpObj <> nil then
    begin
      Data.Exceptions.Add(AClass, Data.FindClass(string(ATypeName), TSE2PackageUnit(Data.TmpObj).FCurrentUnitName));
    end;
end;

procedure TSE2PackageUnit.RegisterExceptions(index: Integer;
  const Target: TSE2RunAccess);
var sUnitName : AnsiString;
begin
  if not FPackageLoaded then
     exit;

  Target.TmpObj := Self;
  PackageGetModuleName(index, sUnitName);
  FCurrentUnitName := string(sUnitName);

  if Target.HasUnit(FCurrentUnitName) then
    if Assigned(FRegisterExcept) then
       FRegisterExcept(index, Target, @PackageRegisterExceptCallback);
end;

function TSE2PackageUnit.GetIsExtender(index: Integer): Boolean;
begin
  result := PackageIsExtender(index)
end;

function TSE2PackageUnit.PackageIsExtender(index: integer): boolean;
begin
  if @FGetIsExtender <> nil then
     result := FGetIsExtender(index)
  else
     result := False;
end;

end.
