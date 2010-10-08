unit uFileStreamModule;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, uSE2PackageAPI;

const
  CFileStreamModuleName   = 'IO.Streams';
  CFileStreamModuleSource =
        'unit IO.Streams;'+#13#10+
        #13#10+
        'interface'+#13#10+
        #13#10+
        'type'+#13#10+
        '  FileMode = class'+#13#10+
        '  public'+#13#10+
        '    const CreateFile = $FFFF;'+#13#10+
        '    const Read       = $0000;'+#13#10+
        '    const Write      = $0001;'+#13#10+
        '    const ReadWrite  = $0002;'+#13#10+
        '  end;'+#13#10+
        #13#10+
        '  FileShare = class'+#13#10+
        '  public'+#13#10+
        '    const Exclusive = $0010;'+#13#10+
        '    const DenyWrite = $0020;'+#13#10+
        '    const DenyRead  = $0030;'+#13#10+
        '    const DenyNone  = $0040;'+#13#10+
        '  end;'+#13#10+
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
  result := TFileStream.Create(AHandle);
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
