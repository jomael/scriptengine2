unit ConsoleForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  Console, StdCtrls, ExtCtrls;

type
  TConsoleWindow = class(TForm)
    panelTop: TPanel;
    editInput: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure editInputKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure editInputKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
    FTerminal : TColorConsole;

    FDoAbort   : boolean;
    FInput     : string;
    FInputDone : boolean;
    FSingleKey : boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;

    procedure MakeEditVisible;
    procedure HideEdit;

    function  EditRead(FullLine: boolean): string;
  public
    { Public-Deklarationen } 
    property Terminal : TColorConsole  read FTerminal;
  end;

var
  CONSOLE_UseEditInput : boolean = False;

procedure CloseConsole;

implementation

{$R *.dfm}

uses
  uSE2IncConsole, StrUtils;

var
  ConsoleWindow : TConsoleWindow = nil;

function  Console: TConsoleWindow;
begin
  if ConsoleWindow = nil then
  begin
     ConsoleWindow := TConsoleWindow.Create(nil);
     ConsoleWindow.Visible := True;
  end;
  result := ConsoleWindow;
end;

procedure CloseConsole;
begin
  if ConsoleWindow <> nil then
     ConsoleWindow.Terminal.DoAbort := True;
  FreeAndNil(ConsoleWindow);
end;

procedure Console_DoClear;
begin
  Console.Terminal.ClrScr;
end;

function  Console_DoKeyPressed: boolean;
begin
  result := Console.Terminal.KeyPressed;
end;

procedure Console_DoWrite(s: string);
begin
  Console.Terminal.WriteString(s);
end;

function Console_DoReadLn: string;
var len : integer;
begin
  if CONSOLE_UseEditInput then
     result := Console.EditRead(True)
  else
  begin
    SetLength(result, 8196);
    len := Console.Terminal.ReadBuf(PChar(result), 8196);
    result := Copy(result, 1, len - 2);
  end;
end;

function Console_DoReadKey: string;
begin
  if CONSOLE_UseEditInput then
     result := Console.EditRead(False)
  else
     result := Console.Terminal.ReadKey;
end;

procedure Console_DoClearLine;
begin
  Console.Terminal.ClrEol;
end;

procedure Console_DoCursorTo(x, y: integer);
begin
  Console.Terminal.CursorTo(x, y);
end;   

procedure Console_DoScrollTo(x, y: integer);
begin
  Console.Terminal.ScrollTo(x, y);
end;

procedure Console_DoSetBgColor(value: TColor);
begin
  Console.Terminal.Font.BkColor := value;
end;

procedure Console_DoSetFgColor(value: TColor);
begin
  Console.Terminal.Font.Color := value;
end;

function  Console_DoGetBgColor: TColor;
begin
  result := Console.Terminal.Font.BkColor;
end;

function  Console_DoGetFgColor: TColor;
begin
  result := Console.Terminal.Font.Color;
end;

function  Console_DoGetLine(line: integer): string;
begin
  result := IntToStr(line);
end;

procedure Console_DoSetLine(line: integer; s: string);
begin
  Console.Terminal.CursorTo(0, line);

  if length(s) < Console.Terminal.Cols then
     s := s + DupeString(' ', Console.Terminal.Cols - length(s));
     
  Console.Terminal.WriteString(s);
end;

procedure RegisterConsole;
begin
  uSE2IncConsole.TSE2Console.Clear      := Console_DoClear;
  uSE2IncConsole.TSE2Console.Write      := Console_DoWrite;
  uSE2IncConsole.TSE2Console.Read       := Console_DoReadLn;
  uSE2IncConsole.TSE2Console.ReadKey    := Console_DoReadKey;
  uSE2IncConsole.TSE2Console.KeyPressed := Console_DoKeyPressed;
  uSE2IncConsole.TSE2Console.ClearLn    := Console_DoClearLine;
  uSE2IncConsole.TSE2Console.CursorTo   := Console_DoCursorTo;
  uSE2IncConsole.TSE2Console.ScrollTo   := Console_DoScrollTo;
  uSE2IncConsole.TSE2Console.GetBgColor := Console_DoGetBgColor;
  uSE2IncConsole.TSE2Console.GetFgColor := Console_DoGetFgColor;
  uSE2IncConsole.TSE2Console.SetBgColor := Console_DoSetBgColor;
  uSE2IncConsole.TSE2Console.SetFgColor := Console_DoSetFgColor;
  uSE2IncConsole.TSE2Console.GetLine    := Console_DoGetLine;
  uSE2IncConsole.TSE2Console.SetLine    := Console_DoSetLine;
end;

procedure TConsoleWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := 0;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TConsoleWindow.FormCreate(Sender: TObject);
begin
  FTerminal := TColorConsole.Create(Self);
  FTerminal.Parent       := Self;
  FTerminal.Align        := alClient;
  FTerminal.Color        := clBlack; 
  FTerminal.Options      := [coAutoTracking, coCheckBreak, coLazyWrite];
  FTerminal.Font.BkColor := clBlack;
  FTerminal.Font.Color   := clWhite;
  FTerminal.Font.Size    := 10;
  FTerminal.Font.Name    := 'Courier New';
  FTerminal.Font.Style   := [];
  FTerminal.LineBreak    := CRLF;
  FTerminal.Rows         := 150;
  FTerminal.Cols         := 80;
  FTerminal.ClrScr;

  panelTop.Top := ClientHeight + 1;
  FInputDone := True;
end;

procedure TConsoleWindow.FormDestroy(Sender: TObject);
begin
  FDoAbort := True;
  FTerminal.Free;
  ConsoleWindow := nil;
end;

procedure TConsoleWindow.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;
  FDoAbort := True;
  Self.FTerminal.DoAbort := True;
end;

procedure TConsoleWindow.HideEdit;
begin   
  while panelTop.Top <= ClientHeight do
  begin
    panelTop.Top := panelTop.Top + 1;
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

procedure TConsoleWindow.MakeEditVisible;
begin
  while panelTop.Top > ClientHeight - panelTop.Height do
  begin
    panelTop.Top := panelTop.Top - 1;
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

function TConsoleWindow.EditRead(FullLine: boolean): string;
begin
  panelTop.BringToFront;
  editInput.Text := '';
  MakeEditVisible;

  FInputDone := False;
  FSingleKey := not FullLine;

  editInput.SetFocus;
  while not FInputDone do
  begin
    Application.HandleMessage;
    Sleep(1);
    if FDoAbort then
       Abort;
  end;

  result := FInput;

  HideEdit;
end;

procedure TConsoleWindow.editInputKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin        
  if FSingleKey then
  begin
    FInput := Chr(Key);
    FInputDone := True;
    Key := 0;
  end;
end;

procedure TConsoleWindow.editInputKeyPress(Sender: TObject; var Key: Char);
begin
  if FInputDone then
  begin
    Key := #0;
    exit;
  end;

  if Key = #13 then
  begin
    Key := #0;
    FInput := editInput.Text;
    FInputDone := True;

    Console_DoWrite(FInput + #13#10);
  end;
end;

initialization
  RegisterConsole;

finalization
  FreeAndNil(ConsoleWindow);

end.
