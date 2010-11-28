unit uSE2IncSystemInteropWindows;   

{$INCLUDE ScriptEngine.inc}

interface

uses
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;

implementation

const
  C_UnitName   = 'System.Interop.Windows';
  C_UnitSource = 
        'unit System.Interop.Windows;' + #13#10 + 
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  TDynamicLinkLibrary = class(TObject)' + #13#10 + 
        '  private' + #13#10 + 
        '    FHandle : THandle;' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(const LibraryName: WideString);' + #13#10 + 
        '    destructor Destroy; override;' + #13#10 + 
        #13#10 + 
        '    function FindMethod(const MethodName: AnsiString): Pointer; overload;' + #13#10 + 
        '    function FindMethod(const MethodName: AnsiString; ThrowIfNil: boolean): Pointer; overload;' + #13#10 + 
        '    function FindMethod(index: UInt16): Pointer; overload;' + #13#10 + 
        '    function FindMethod(index: UInt16; ThrowIfNil: boolean): Pointer; overload;' + #13#10 + 
        #13#10 + 
        '    property Handle   : THandle    read FHandle;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  LoadLibraryWin32 = class' + #13#10 + 
        '  public' + #13#10 + 
        '    const DllName = ''kernel32.dll'';' + #13#10 + 
        #13#10 + 
        '    class function LoadLibrary(const LibraryName: WideString): THandle; external DllName name ''LoadLibraryW''; stdcall;' + #13#10 + 
        '    class function GetProcAddress(const hLib: THandle; const Name: AnsiString): Pointer; overload; external DllName name ''GetProcAddress''; stdcall;' + #13#10 + 
        '    class function GetProcAddress(const hLib: THandle; const Index: UInt32): Pointer; overload; external DllName name ''GetProcAddress''; stdcall;' + #13#10 + 
        '    class function FreeLibrary(const hLib: THandle): boolean; external DllName name ''FreeLibrary''; stdcall;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'constructor TDynamicLinkLibrary.Create(const LibraryName: WideString);' + #13#10 + 
        'begin' + #13#10 + 
        '  inherited Create;' + #13#10 + 
        '  Self.FHandle := LoadLibraryWin32.LoadLibrary(LibraryName);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'destructor TDynamicLinkLibrary.Destroy;' + #13#10 + 
        'begin' + #13#10 + 
        '  if FHandle <> 0 then' + #13#10 + 
        '     LoadLibraryWin32.FreeLibrary(FHandle);' + #13#10 + 
        '  inherited;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TDynamicLinkLibrary.FindMethod(const MethodName: AnsiString): Pointer;' + #13#10 + 
        'begin' + #13#10 + 
        '  if FHandle <> 0 then' + #13#10 + 
        '     result := LoadLibraryWin32.GetProcAddress(FHandle, MethodName)' + #13#10 + 
        '  else' + #13#10 + 
        '     result := nil;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TDynamicLinkLibrary.FindMethod(const MethodName: AnsiString; ThrowIfNil: boolean): Pointer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := LoadLibraryWin32.GetProcAddress(FHandle, MethodName);' + #13#10 + 
        '  if (result = nil) and ThrowIfNil then' + #13#10 + 
        '     raise EMethodNotAssigned.Create(''The method '' + MethodName + '' could not be found in library 0x'' + Convert.ToHex(FHandle));' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TDynamicLinkLibrary.FindMethod(index: UInt16): Pointer;' + #13#10 + 
        'begin' + #13#10 + 
        '  if FHandle <> 0 then' + #13#10 + 
        '     result := LoadLibraryWin32.GetProcAddress(FHandle, index)' + #13#10 + 
        '  else' + #13#10 + 
        '     result := nil;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TDynamicLinkLibrary.FindMethod(index: UInt16; ThrowIfNil: boolean): Pointer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := LoadLibraryWin32.GetProcAddress(FHandle, index);' + #13#10 + 
        '  if (result = nil) and ThrowIfNil then' + #13#10 + 
        '     raise EMethodNotAssigned.Create(''The method could not be found at index '' + index.ToString + '' in library 0x'' + Convert.ToHex(FHandle));' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'end.';

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
  end
end;

procedure RegisterUnit();
var p: TSE2MethodUnit;
begin
  p := TSE2MethodUnit.Create;
  p.DoRegisterMethods := Unit_RegisterMethods;
  p.DoGetUnitSource   := Unit_GetSource;
  p.UnitName          := C_UnitName;
  TSE2UnitManager.RegisterUnit(p);
end;

initialization
  RegisterUnit();

end.
