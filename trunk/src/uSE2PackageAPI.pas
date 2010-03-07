unit uSE2PackageAPI;

{$INCLUDE ScriptEngine.inc}

interface

const
  CSE2PackageInitialize       = 'SE2Init';
  CSE2PackageFinalize         = 'SE2Finalize';
  CSE2PackageMinVersion       = 'SE2NeededVersion';
  CSE2PackageGUID             = 'SE2PackageGUID';

  CSE2PackageSetMM            = 'SE2SetMM';
  CSE2PackageSetMMEx          = 'SE2SetMMEx';
  CSE2PackageNumModules       = 'SE2NumModules';

  CSE2PackageInitModule       = 'SE2InitModule';
  CSE2PackageFinalizeModule   = 'SE2FinalizeModule';

  CSE2PackageIsExtender       = 'SE2IsExtender';
  CSE2PackageModuleSize       = 'SE2GetModuleSize';
  CSE2PackageGetModuleName    = 'SE2ModuleName';
  CSE2PackageGetModuleSource  = 'SE2ModuleSource';
  CSE2PackageRegisterModule   = 'SE2RegisterModule';



type
  TPackageModule = integer;
  TSE2Version = packed record
    Major, Minor, Patch, Build: word;
  end;

  TSE2PackageFunctionRegister = procedure(Module: TPackageModule; Data: Pointer; MethodPos: Pointer; MethodName: PAnsiChar); stdcall;

  
  // Package Information
  TSE2PackageMinVersion       = procedure(var Version: TSE2Version); stdcall;
  TSE2PackageGetGUID          = procedure(var GUID: TGUID); stdcall;

  // Inititialization, Finalization
  // Initialization = return 0 if everything is ok
  //                  return none-zero for error-code
  TSE2PackageInitialize       = function: integer; stdcall;
  TSE2PackageFinalize         = procedure; stdcall;


  // Memory Manager Setup
  {$IFDEF DELPHI2005UP}
    {$Warnings off}
  {$ENDIF}
  TSE2PackageSetMM            = procedure(const MemoryManager: TMemoryManager); stdcall;
  {$IFDEF DELPHI2005UP}
    {$Warnings on}
  {$ENDIF}
  {$IFDEF DELPHI2005UP}
  TSE2PackageSetMMEx          = procedure(const MemoryManager: TMemoryManagerEx); stdcall;
  {$ENDIF}

  // Modules in the package
  TSE2PackageNumModules       = function: integer; stdcall;

  // Initialize/finalize a module
  // Initialization = return 0 if everything is ok
  //                  return none-zero for error-code
  TSE2PackageInitModule       = function(Module: TPackageModule): integer; stdcall;
  TSE2PackageFinalizeModule   = procedure(Module: TPackageModule); stdcall;
  // Finalize the module
  TSE2PackageModuleSize       = procedure(Module: TPackageModule; var NameLen, SourceLen: integer); stdcall;
  
  // Get Module Dependency
  TSE2PackageIsExtender       = function(Module: TPackageModule): boolean; stdcall;

  // Get Module Data
  TSE2PackageGetModuleName    = function(Module: TPackageModule; pName: PAnsiChar; BuffSize: integer): integer; stdcall;
  TSE2PackageGetModuleSource  = function(Module: TPackageModule; pSource: PAnsiChar; BuffSize: integer): integer; stdcall;
  TSE2PackageRegisterModule   = procedure(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister); stdcall;

implementation

end.
