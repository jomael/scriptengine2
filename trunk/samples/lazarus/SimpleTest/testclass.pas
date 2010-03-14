unit TestClass;

{$mode delphi}{$H+}

interface

uses
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;

implementation

const
  C_UnitName   = 'Unit1';
  C_UnitSource =
        'unit Unit1;' + #13#10 +
        #13#10 +
        'interface' + #13#10 +
        #13#10 +
        'type' + #13#10 +
        '  TMyTest = class(TExternalObject)' + #13#10 +
        '  public' + #13#10 +
        '    constructor Create; external;' + #13#10 +
        #13#10 +
        '    function  GetValue: string; external;' + #13#10 +
        '    procedure SetValue(value: string); external;' + #13#10 +
        '  end;' + #13#10 +
        #13#10 +
        'implementation' + #13#10 +
        #13#10 +
        'end.';

type

  { TMyTest }

  TMyTest = class
  private
    FValue : string;
  public
    function  GetValue: string;
    procedure SetValue(value: string);
  end;

{ TMyTest }

function TMyTest.GetValue: string;
begin
  result := FValue;
end;

procedure TMyTest.SetValue(value: string);
begin
  FValue := value;
end;

function TMyTest_Create(Self: TMyTest): TMyTest;
begin
  result := TMyTest.Create;
end;

function TMyTest_GetValue(Self: TMyTest): string;
begin
  result := Self.GetValue;
end;

procedure TMyTest_SetValue(Self: TMyTest; value: string);
begin
  Self.SetValue(value);
end;

procedure Unit_GetSource(var Target: string);
begin
  Target := C_UnitSource;
end;

procedure Unit_RegisterMethods(const Target: TSE2RunAccess);
begin
  if Target.HasUnit(C_UnitName) then
  begin
    Target.Method['TMyTest.Create[0]', C_UnitName] := @TMyTest_Create;
    Target.Method['TMyTest.GetValue[0]', C_UnitName] := @TMyTest_GetValue;
    Target.Method['TMyTest.SetValue[0]', C_UnitName] := @TMyTest_SetValue;
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
