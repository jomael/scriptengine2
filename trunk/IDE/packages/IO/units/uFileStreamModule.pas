unit uFileStreamModule;

{$IFDEF FPC}
{$MODE OBJFPC}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, uSE2PackageAPI;

const
  CFileStreamModuleName   = 'Streams';
  CFileStreamModuleSource =
        'unit Streams;'+#13#10+
        #13#10+
        'interface'+#13#10+
        #13#10+
        'const'+#13#10+
        '  fmCreate         = $FFFF;'+#13#10+
        '  fmOpenRead       = $0000;'+#13#10+
        '  fmOpenWrite      = $0001;'+#13#10+
        '  fmOpenReadWrite  = $0002;'+#13#10+
        #13#10+
        '  fmShareExclusive = $0010;'+#13#10+
        '  fmShareDenyWrite = $0020;'+#13#10+
        '  fmShareDenyRead  = $0030;'+#13#10+
        '  fmShareDenyNone  = $0040;'+#13#10+
        #13#10+
        'type'+#13#10+
        '  THandleStream = class(TStream)'+#13#10+
        '  private'+#13#10+
        '    function GetHandle: THandle; external;'+#13#10+
        '  public'+#13#10+
        '    constructor Create(AHandle: THandle); external;'+#13#10+
        #13#10+
        '    property Handle: THandle read GetHandle;'+#13#10+
        '  end;'+#13#10+
        #13#10+
        '  TFileStream = class(THandleStream)'+#13#10+
        '  public'+#13#10+
        '    constructor Create(const FileName: string; Mode: Word); overload; external;'+#13#10+
        '    constructor Create(const FileName: string; Mode: Word; Rights: Cardinal); overload; external;'+#13#10+
        '    constructor Create(AHandle: THandle); overload; external;'+#13#10+
        '  end;'+#13#10+
        #13#10+
        'implementation'+#13#10+
        #13#10+ 
        'end.';


procedure CFileStreamModuleRegister(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

function THandleStream_GetHandle(Self: THandleStream): integer;
begin
  result := Self.Handle;
end;

function THandleStream_Create(Self: THandleStream; AHandle: integer): THandleStream;
begin
  result := THandleStream.Create(AHandle);
end;

function TFileStream_Create0(Self: TFileStream; const FileName: string; Mode: Word): TFileStream;
begin
  result := TFileStream.Create(FileName, Mode);
end;

function TFileStream_Create1(Self: TFileStream; const FileName: string; Mode: Word; Rights: cardinal): TFileStream;
begin
  result := TFileStream.Create(FileName, Mode, Rights);
end;     

function TFileStream_Create2(Self: TFileStream; AHandle: integer): TFileStream;
begin
  // units\uFileStreamModule.pas(74,39) Error: Wrong number of parameters specified for call to "Create"
  {$IFNDEF FPC}
  result := TFileStream.Create(AHandle);
  {$ENDIF}
end;

procedure CFileStreamModuleRegister(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @THandleStream_GetHandle, 'THandleStream.GetHandle[0]');
  CallBack(Module, Data, @THandleStream_Create, 'THandleStream.Create[0]');
  CallBack(Module, Data, @TFileStream_Create0, 'TFileStream.Create[0]');
  CallBack(Module, Data, @TFileStream_Create1, 'TFileStream.Create[1]');   
  CallBack(Module, Data, @TFileStream_Create2, 'TFileStream.Create[2]');
end;

end.
