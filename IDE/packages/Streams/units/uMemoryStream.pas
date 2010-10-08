unit uMemoryStream;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  uSE2PackageAPI;

const
  C_UnitName   = 'IO.Streams';
  C_UnitSource = 
        'unit IO.Streams;' + #13#10 + 
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  { TCustomMemoryStream abstract class }' + #13#10 + 
        #13#10 + 
        '  TCustomMemoryStream = class(TStream)' + #13#10 + 
        '  protected' + #13#10 + 
        '    function GetMemory: Pointer; external;' + #13#10 + 
        '  public' + #13#10 + 
        '    procedure SaveToStream(Stream: TStream); external;' + #13#10 + 
        '    procedure SaveToFile(const FileName: string); external;' + #13#10 + 
        '    property Memory: Pointer read GetMemory;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '{ TMemoryStream }' + #13#10 + 
        #13#10 + 
        '  TMemoryStream = class(TCustomMemoryStream)' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create; external;' + #13#10 + 
        '    procedure Clear; external;' + #13#10 + 
        '    procedure LoadFromStream(Stream: TStream); external;' + #13#10 + 
        '    procedure LoadFromFile(const FileName: string); external;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

function TCustomMemoryStream_GetMemory(Self: TCustomMemoryStream): pointer;
begin
  result := Self.Memory;
end;

procedure TCustomMemoryStream_SaveToStream(Self: TCustomMemoryStream; Stream: TStream);
begin
  Self.SaveToStream(Stream);
end;

procedure TCustomMemoryStream_SaveToFile(Self: TCustomMemoryStream; const FileName: string);
begin
  Self.SaveToFile(FileName);
end;

function TMemoryStream_Create(Self: TMemoryStream): TMemoryStream;
begin
  result := TMemoryStream.Create;
end;

procedure TMemoryStream_Clear(Self: TMemoryStream);
begin
  Self.Clear;
end;

procedure TMemoryStream_LoadFromStream(Self: TMemoryStream; Stream: TStream);
begin
  Self.LoadFromStream(Stream);
end;

procedure TMemoryStream_LoadFromFile(Self: TMemoryStream; const FileName: string);
begin
  Self.LoadFromFile(FileName);
end;

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TCustomMemoryStream_GetMemory, 'TCustomMemoryStream.GetMemory[0]');
  CallBack(Module, Data, @TCustomMemoryStream_SaveToStream, 'TCustomMemoryStream.SaveToStream[0]');
  CallBack(Module, Data, @TCustomMemoryStream_SaveToFile, 'TCustomMemoryStream.SaveToFile[0]');
  CallBack(Module, Data, @TMemoryStream_Create, 'TMemoryStream.Create[0]');
  CallBack(Module, Data, @TMemoryStream_Clear, 'TMemoryStream.Clear[0]');
  CallBack(Module, Data, @TMemoryStream_LoadFromStream, 'TMemoryStream.LoadFromStream[0]');
  CallBack(Module, Data, @TMemoryStream_LoadFromFile, 'TMemoryStream.LoadFromFile[0]');
end;

end.
