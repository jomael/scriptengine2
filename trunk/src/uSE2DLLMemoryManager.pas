unit uSE2DLLMemoryManager;

{$Include ScriptEngine.inc}

interface

uses
  {$IFDEF SEII_FPC}
    {$IFDEF MSWINDOWS}
    Windows, SysUtils
    {$ELSE}
    SysUtils, Types
    {$ENDIF}
  {$ELSE}
    {$IFDEF MSWINDOWS}
    Windows
    {$ENDIF}
    {$IFDEF LINUX}
    SysUtils,
    Types
    {$ENDIF}
  {$ENDIF}
  ;

{$IFNDEF MSWINDOWS}
const
  SEM_NOOPENFILEERRORBOX = $8000;
{$ENDIF}

{$IFDEF Unicode}
function SE2LoadLibrary(lpLibFileName: PWideChar): HMODULE;
{$ELSE}
function SE2LoadLibrary(lpLibFileName: PAnsiChar): HMODULE;
{$ENDIF}

function SE2SafeLoadLibrary(const FileName: string; ErrorMode: cardinal = {$IFDEF SEII_FPC} 0 {$ELSE} {$IFDEF MSWINDOWS} SEM_NOOPENFILEERRORBOX {$ELSE} 0 {$ENDIF} {$ENDIF}): HMODULE;

implementation

type
  TMMMngr = {$IF declared(TMemoryManagerEx)} TMemoryManagerEx {$ELSE} TMemoryManager {$IFEND} ;

var
  OldMemoryManager : TMMMngr;
  NewMemoryManager : TMMMngr;
  MemoryManagerSet : boolean = False;


type
  TMemoryMappedName = array[0..38] of AnsiChar;

const
  CMemoryMappedName : TMemoryMappedName =
      '4C2C79D9-7B52-4B65-BA62-             '#0;

{$IFDEF MSWINDOWS}
var
  MappedHandle     : THandle;
  ViewHandle       : Pointer;

function GenerateMemFileName: TMemoryMappedName;
var processId : cardinal;
    i         : integer;
begin
  result    := CMemoryMappedName;
  processId := GetCurrentProcessId;

  for i:= 1 to 13 do
  begin
    result[24 + i] := AnsiChar((processId and $0F) + Ord('0'));
    processId := processId shr 4;
  end;
end;

procedure CreateMemoryMappedFile;
var mm        : TMMMngr;
    mmName    : TMemoryMappedName;
begin
  mmName := GenerateMemFileName;

  MappedHandle := CreateFileMappingA(INVALID_HANDLE_VALUE, nil, FILE_MAP_READ, 0,
                    SizeOf(TMMMngr) * 2, @mmName);
  ViewHandle   := MapViewOfFile(MappedHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, 0);
  if (MappedHandle <> 0) and (ViewHandle <> nil) then
  begin
    {$HINTS OFF}
    GetMemoryManager(mm);
    {$HINTS ON}
    ZeroMemory(ViewHandle, SizeOf(mm) * 2);
    CopyMemory(ViewHandle, @mm, SizeOf(mm));
  end;
end;

procedure FreeMemoryMappedFile;
begin
  if ViewHandle <> nil then
     UnmapViewOfFile(ViewHandle);

  if MappedHandle <> 0 then
     CloseHandle(MappedHandle);

  ViewHandle   := nil;
  MappedHandle := 0;
end;
{$ENDIF}


{$IFDEF Unicode}
function SE2LoadLibrary(lpLibFileName: PWideChar): HMODULE;
{$ELSE}
function SE2LoadLibrary(lpLibFileName: PAnsiChar): HMODULE;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  CreateMemoryMappedFile;
  try
  {$ENDIF}

    {$IFDEF MSWINDOWS}
      {$IFDEF Unicode}
      result := Windows.LoadLibraryW(lpLibFileName);
      {$ELSE}
      result := Windows.LoadLibraryA(lpLibFileName);
      {$ENDIF}
    {$ENDIF}
    {$IFDEF LINUX} 
      {$IFDEF Unicode}
      result := HMODULE(dlopen(lpLibFileName, RTLD_LAZY));
      {$ELSE}
      result := HMODULE(dlopen(lpLibFileName, RTLD_LAZY));
      {$ENDIF}
    {$ENDIF}

  {$IFDEF MSWINDOWS}
  finally
    FreeMemoryMappedFile;
  end;
  {$ENDIF}
end;

function SE2SafeLoadLibrary(const FileName: string; ErrorMode: UINT): HMODULE;
{$IFNDEF SEII_FPC}
var         
  {$IFDEF MSWINDOWS}
   OldMode        : UINT;
  {$ENDIF}
  FPUControlWord : Word;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  CreateMemoryMappedFile;
  try
  {$ENDIF}
    {$IFDEF SEII_FPC}
       result := SafeLoadLibrary(FileName, ErrorMode);
    {$ELSE}
      {$IFDEF MSWINDOWS}
      OldMode := SetErrorMode(ErrorMode);
      try
        asm
          FNSTCW  FPUControlWord
        end;
        try
          Result := SE2LoadLibrary({$IFDEF Unicode}PWideChar{$ELSE}PChar{$ENDIF}(Filename));
        finally
          asm
            FNCLEX
            FLDCW FPUControlWord
          end;
        end;
      finally
        SetErrorMode(OldMode);
      end;
      {$ENDIF}
      {$IFDEF LINUX}
      asm
        FNSTCW  FPUControlWord
      end;
      try
        Result := SE2LoadLibrary({$IFDEF Unicode}PWideChar{$ELSE}PChar{$ENDIF}(Filename));
      finally
        asm
          FNCLEX
          FLDCW FPUControlWord
        end;
      end;
      {$ENDIF}
    {$ENDIF}

  {$IFDEF MSWINDOWS}
  finally
    FreeMemoryMappedFile;
  end;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure LoadMemoryManagerFromStream;
var mmName    : TMemoryMappedName;
    mm        : TMMMngr;
    Map       : THandle;
    View      : Pointer;
begin
  if MemoryManagerSet then
     exit;

  mmName := GenerateMemFileName;

  Map  := OpenFileMappingA(FILE_MAP_READ, False, @mmName);
  if Map <> 0 then
  begin                             
    View := MapViewOfFile(Map, FILE_MAP_READ, 0, 0, 0);
    if View <> nil then
    begin
      CopyMemory(@mm, View, SizeOf(mm));
      GetMemoryManager(OldMemoryManager);
      NewMemoryManager := mm;
      SetMemoryManager(NewMemoryManager);
      MemoryManagerSet := True;
      
      UnmapViewOfFile(View);
    end;
    
    CloseHandle(Map);
  end;
end;
{$ENDIF}

initialization
  {$IFDEF MSWINDOWS}
  LoadMemoryManagerFromStream;
  {$ENDIF}

finalization
  if MemoryManagerSet then
     SetMemoryManager(OldMemoryManager);


end.
