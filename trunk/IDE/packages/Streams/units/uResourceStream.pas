unit uResourceStream;

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
        '{ TResourceStream }' + #13#10 + 
        #13#10 + 
        '  TResourceStream = class(TCustomMemoryStream)' + #13#10 + 
        '  private' + #13#10 + 
        '  public' + #13#10 + 
        '    constructor Create(Instance: THandle; const ResName: string; ResType: PChar); external;' + #13#10 + 
        '    constructor CreateFromID(Instance: THandle; ResID: Integer; ResType: PChar); external;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        'end.';

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);

implementation

function TResourceStream_Create(Self: TResourceStream; Instance: THandle; const ResName: string; ResType: PChar): TResourceStream;
begin
  result := TResourceStream.Create(Instance, ResName, ResType);
end;

function TResourceStream_CreateFromID(Self: TResourceStream; Instance: THandle; ResID: integer; ResType: PChar): TResourceStream;
begin
  result := TResourceStream.CreateFromID(Instance, ResID, ResType);
end;

procedure RegisterMethods(Module: TPackageModule; Data: Pointer; CallBack: TSE2PackageFunctionRegister);
begin
  CallBack(Module, Data, @TResourceStream_Create, 'TResourceStream.Create[0]');
  CallBack(Module, Data, @TResourceStream_CreateFromID, 'TResourceStream.CreateFromID[0]');
end;

end.
