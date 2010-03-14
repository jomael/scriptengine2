unit uSE2IncKeyCodes;

{$INCLUDE ScriptEngine.inc}

interface

uses
  uSE2RunAccess, uSE2UnitManager, uSE2Consts;

implementation

const
  C_UnitName   = 'System';
  C_UnitSource = 
        'unit System;' + #13#10 + 
        #13#10 + 
        'interface' + #13#10 + 
        #13#10 + 
        'type' + #13#10 + 
        '  KeyCodes = partial class' + #13#10 + 
        '  public' + #13#10 + 
        '    const LeftMouse = 1;' + #13#10 + 
        '    const RightMouse = 2;' + #13#10 + 
        '    const Cancel = 3;' + #13#10 + 
        '    const MiddleMouse = 4;' + #13#10 + 
        '    const Back = 8;' + #13#10 + 
        '    const Tab = 9;' + #13#10 + 
        '    const Clear = 12;' + #13#10 + 
        '    const Return = 13;' + #13#10 + 
        '    const Shift = $10;' + #13#10 + 
        '    const Control = 17;' + #13#10 + 
        '    const Menu = 18;' + #13#10 + 
        '    const Pause = 19;' + #13#10 + 
        '    const Captial = 20;' + #13#10 + 
        '    const Accept = 30;' + #13#10 + 
        '    const Escape = 27;' + #13#10 + 
        '    const Space = $20;' + #13#10 + 
        '    const Prior = 33;' + #13#10 + 
        '    const Next = 34;' + #13#10 + 
        '    const EndKey = 35;' + #13#10 + 
        '    const Home = 36;' + #13#10 + 
        '    const Left = 37;' + #13#10 + 
        '    const Up = 38;' + #13#10 + 
        '    const Right = 39;' + #13#10 + 
        '    const Down = 40;' + #13#10 + 
        '    const Print = 42;' + #13#10 + 
        '    const Execute = 43;' + #13#10 + 
        '    const Snapshot = 44;' + #13#10 + 
        '    const Insert = 45;' + #13#10 + 
        '    const Delete = 46;' + #13#10 + 
        '    const Help = 47;' + #13#10 + 
        #13#10 + 
        '    const Numpad0 = 96;' + #13#10 + 
        '    const Numpad1 = 97;' + #13#10 + 
        '    const Numpad2 = 98;' + #13#10 + 
        '    const Numpad3 = 99;' + #13#10 + 
        '    const Numpad4 = 100;' + #13#10 + 
        '    const Numpad5 = 101;' + #13#10 + 
        '    const Numpad6 = 102;' + #13#10 + 
        '    const Numpad7 = 103;' + #13#10 + 
        '    const Numpad8 = 104;' + #13#10 + 
        '    const Numpad9 = 105;' + #13#10 + 
        '    const Multiply = 106;' + #13#10 + 
        '    const Add = 107;' + #13#10 + 
        '    const Separator = 108;' + #13#10 + 
        '    const Substract = 109;' + #13#10 + 
        '    const Decimal = 110;' + #13#10 + 
        '    const Divide = 111;' + #13#10 + 
        '    const F1 = 112;' + #13#10 + 
        '    const F2 = 113;' + #13#10 + 
        '    const F3 = 114;' + #13#10 + 
        '    const F4 = 115;' + #13#10 + 
        '    const F5 = 116;' + #13#10 + 
        '    const F6 = 117;' + #13#10 + 
        '    const F7 = 118;' + #13#10 + 
        '    const F8 = 119;' + #13#10 + 
        '    const F9 = 120;' + #13#10 + 
        '    const F10 = 121;' + #13#10 + 
        '    const F11 = 122;' + #13#10 + 
        '    const F12 = 123;' + #13#10 + 
        '    const F13 = 124;' + #13#10 + 
        '    const F14 = 125;' + #13#10 + 
        '    const F15 = 126;' + #13#10 + 
        '    const F16 = 127;' + #13#10 + 
        '    const F17 = 128;' + #13#10 + 
        '    const F18 = 129;' + #13#10 + 
        '    const F19 = 130;' + #13#10 + 
        '    const F20 = 131;' + #13#10 + 
        '    const F21 = 132;' + #13#10 + 
        '    const F22 = 133;' + #13#10 + 
        '    const F23 = 134;' + #13#10 + 
        '    const F24 = 135;' + #13#10 + 
        '    const Numlock = 144;' + #13#10 + 
        '    const Scroll = 145;' + #13#10 + 
        '    const LeftShift = 160;' + #13#10 + 
        '    const RightShift = 161;' + #13#10 + 
        '    const LeftControl = 162;' + #13#10 + 
        '    const RightControl = 163;' + #13#10 + 
        '    const LeftMenu = 164;' + #13#10 + 
        '    const RightMenu = 165;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
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
