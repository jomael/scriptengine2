unit uSE2IncTypes;

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
        '  TPoint = record' + #13#10 + 
        '  public' + #13#10 + 
        '    X, Y : integer;' + #13#10 + 
        '  public' + #13#10 + 
        '    class function Point(x, y: integer): TPoint;' + #13#10 + 
        #13#10 + 
        '    function Add(x, y: integer): TPoint; overload;' + #13#10 + 
        '    function Add(t: TPoint): TPoint; overload;' + #13#10 + 
        #13#10 + 
        '    function Sub(x, y: integer): TPoint; overload;' + #13#10 + 
        '    function Sub(t: TPoint): TPoint; overload;' + #13#10 + 
        '    function Length: single;' + #13#10 + 
        #13#10 + 
        '    function DistanceTo(t: TPoint): single;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        '  TRect = record' + #13#10 + 
        '  private' + #13#10 + 
        '    function  GetTopLeft: TPoint;' + #13#10 + 
        '    procedure SetTopLeft(value: TPoint);' + #13#10 + 
        '    function  GetBottomRight: TPoint;' + #13#10 + 
        '    procedure SetBottomRight(value: TPoint);' + #13#10 + 
        #13#10 + 
        '    function  GetWidth: integer;' + #13#10 + 
        '    procedure SetWidth(value: integer);' + #13#10 + 
        '    function  GetHeight: integer;' + #13#10 + 
        '    procedure SetHeight(value: integer);' + #13#10 + 
        '  public' + #13#10 + 
        '    Left, Top,' + #13#10 + 
        '    Right, Bottom : integer;' + #13#10 + 
        '  public' + #13#10 + 
        '    class function Rect(Left, Top, Right, Bottom: integer): TRect; overload;' + #13#10 + 
        '    class function Rect(LeftTop, RightBottom: TPoint): TRect; overload;' + #13#10 + 
        #13#10 + 
        '    function IsPointWithin(const Point: TPoint): boolean;' + #13#10 + 
        '    function IsRectWithin(const Rect: TRect): boolean;' + #13#10 + 
        #13#10 + 
        '    property TopLeft     : TPoint  read GetTopLeft     write SetTopLeft;' + #13#10 + 
        '    property BottomRight : TPoint  read GetBottomRight write SetBottomRight;' + #13#10 + 
        '    property Width       : integer read GetWidth       write SetWidth;' + #13#10 + 
        '    property Height      : integer read GetHeight      write SetHeight;' + #13#10 + 
        '  end;' + #13#10 + 
        #13#10 + 
        'implementation' + #13#10 + 
        #13#10 + 
        '{ TPoint }' + #13#10 + 
        #13#10 + 
        'class function TPoint.Point(x, y: integer): TPoint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.X := x;' + #13#10 + 
        '  result.Y := y;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TPoint.Add(x, y: integer): TPoint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.X := Self.X + x;' + #13#10 + 
        '  result.Y := Self.Y + y;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TPoint.Add(t: TPoint): TPoint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.X := Self.X + t.X;' + #13#10 + 
        '  result.Y := Self.Y + t.Y;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TPoint.Sub(x, y: integer): TPoint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.X := Self.X - x;' + #13#10 + 
        '  result.Y := Self.Y - y;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TPoint.Sub(t: TPoint): TPoint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.X := Self.X - t.X;' + #13#10 + 
        '  result.Y := Self.Y - t.Y;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TPoint.Length: single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Math.Sqrt(Self.X * Self.X + Self.Y * Self.Y);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TPoint.DistanceTo(t: TPoint): single;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.Sub(t).Length;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        '{ TRect }' + #13#10 + 
        #13#10 + 
        'function  TRect.GetTopLeft: TPoint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.X := Self.Left;' + #13#10 + 
        '  result.Y := Self.Top;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'procedure TRect.SetTopLeft(value: TPoint);' + #13#10 + 
        'begin' + #13#10 + 
        '  Self.Left := value.X;' + #13#10 + 
        '  Self.Top  := value.Y;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function  TRect.GetBottomRight: TPoint;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.X := Self.Right;' + #13#10 + 
        '  result.Y := Self.Bottom;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'procedure TRect.SetBottomRight(value: TPoint);' + #13#10 + 
        'begin' + #13#10 + 
        '  Self.Right  := value.X;' + #13#10 + 
        '  Self.Bottom := value.Y;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function  TRect.GetWidth: integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.Right - Self.Left;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'procedure TRect.SetWidth(value: integer);' + #13#10 + 
        'begin' + #13#10 + 
        '  Self.Right := Self.Left + value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function  TRect.GetHeight: integer;' + #13#10 + 
        'begin' + #13#10 + 
        '  result := Self.Bottom - Self.Top;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'procedure TRect.SetHeight(value: integer);' + #13#10 + 
        'begin' + #13#10 + 
        '  Self.Bottom := Self.Top + value;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TRect.Rect(Left, Top, Right, Bottom: integer): TRect;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.Left   := Left;' + #13#10 + 
        '  result.Top    := Top;' + #13#10 + 
        '  result.Right  := Right;' + #13#10 + 
        '  result.Bottom := Bottom;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'class function TRect.Rect(LeftTop, RightBottom: TPoint): TRect;' + #13#10 + 
        'begin' + #13#10 + 
        '  result.TopLeft     := LeftTop;' + #13#10 + 
        '  result.BottomRight := RightBottom;' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TRect.IsPointWithin(const Point: TPoint): boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result :=' + #13#10 + 
        '    (Self.Left <= Point.X) and (Self.Right >= Point.X) and' + #13#10 + 
        '    (Self.Top  <= Point.Y) and (Self.Bottom >= Point.Y);' + #13#10 + 
        'end;' + #13#10 + 
        #13#10 + 
        'function TRect.IsRectWithin(const Rect: TRect): boolean;' + #13#10 + 
        'begin' + #13#10 + 
        '  result :=' + #13#10 + 
        '    Self.IsPointWithin(Rect.TopLeft) and' + #13#10 + 
        '    Self.IsPointWithin(Rect.BottomRight);' + #13#10 + 
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
