unit ConsoleForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  Console, StdCtrls, ExtCtrls;

const
  WM_MSG_SYNC = WM_USER + 3;

type
  TWMSyncMsg = packed record
    Msg     : Cardinal;
    Cmd     : cardinal;
    Data    : TMemoryStream;
    Result  : cardinal;
  end;

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

    procedure WMMessageSync(var Message: TWMSyncMsg); message WM_MSG_SYNC;
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
  ConsoleWindow     : TConsoleWindow = nil;
  MainAppThreadId   : cardinal;

const
  WM_CMD_BASE             = 0;
  WM_CMD_CLEAR            = WM_CMD_BASE   + 1;
  WM_CMD_WRITE            = WM_CMD_BASE   + 2;
  WM_CMD_READLN           = WM_CMD_BASE   + 3;
  WM_CMD_READKEY          = WM_CMD_BASE   + 4;
  WM_CMD_KEYPRESSED       = WM_CMD_BASE   + 5;
  WM_CMD_CLEARLN          = WM_CMD_BASE   + 6;
  WM_CMD_CURSORTO         = WM_CMD_BASE   + 7;
  WM_CMD_SCROLLTO         = WM_CMD_BASE   + 8;
  WM_CMD_GETBGCOLOR       = WM_CMD_BASE   + 9;
  WM_CMD_GETFGCOLOR       = WM_CMD_BASE   + 10;
  WM_CMD_SETBGCOLOR       = WM_CMD_BASE   + 11;
  WM_CMD_SETFGCOLOR       = WM_CMD_BASE   + 12;
  WM_CMD_GETLINE          = WM_CMD_BASE   + 13;
  WM_CMD_SETLINE          = WM_CMD_BASE   + 14;




function  InvokeRequired: boolean;
begin
  result := GetCurrentThreadId <> MainAppThreadId;
end;

procedure BeginInvoke(var Msg: TWMSyncMsg; const Cmd: cardinal);
begin
  Msg.Msg  := WM_MSG_SYNC;
  Msg.Cmd  := Cmd;
  Msg.Data := TMemoryStream.Create;
end;

procedure EndInvoke(var Msg: TWMSyncMsg);
begin
  Msg.Data.Free;
end;

procedure DoInvoke(var Msg: TWMSyncMsg);
var p: PMessage;
begin
  p := @Msg;
  if ConsoleWindow <> nil then
  begin
    case Msg.Cmd of
    WM_CMD_CLEAR,
    WM_CMD_WRITE,
    WM_CMD_CLEARLN,
    WM_CMD_CURSORTO,
    WM_CMD_SCROLLTO,
    WM_CMD_SETBGCOLOR,
    WM_CMD_SETFGCOLOR,
    WM_CMD_SETLINE :
         begin
           PostMessage(ConsoleWindow.Handle, p^.Msg, p^.WParam, p^.LParam);
           Msg.Data := nil; // freed by command handler
         end;
    else begin
           SendMessage(ConsoleWindow.Handle, p^.Msg, p^.WParam, p^.LParam);
         end;
    end;
  end else
  begin
    EndInvoke(Msg);
    raise Exception.Create('The console windows must be allocated before accessing from a sperate thread');
  end;
end;

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
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_CLEAR);
    DoInvoke(msg);
    EndInvoke(msg);
  end else
    Console.Terminal.ClrScr;
end;

function  Console_DoKeyPressed: boolean;
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_KEYPRESSED);
    DoInvoke(msg);
    msg.Data.Read(result, SizeOf(boolean));
    EndInvoke(msg);
  end else
    result := Console.Terminal.KeyPressed;
end;

procedure Console_DoWrite(s: string);
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_WRITE);
    msg.Data.Write(s[1], length(s) * SizeOf(char));
    DoInvoke(msg);
    EndInvoke(msg);
  end else
    Console.Terminal.WriteString(AnsiString(s));
end;

function Console_DoReadLn: string;
var len : integer;
    msg : TWMSyncMsg;
{$IFDEF Unicode}
    s   : AnsiString;
{$ENDIF}
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_READLN);
    DoInvoke(msg);
    SetLength(result, msg.Data.Size div SizeOf(char));
    msg.Data.Read(result[1], msg.Data.Size);
    EndInvoke(msg);
  end else
  begin
    if CONSOLE_UseEditInput then
       result := Console.EditRead(True)
    else
    begin
      {$IFDEF Unicode}
      SetLength(s, 8196);
      len := Console.Terminal.ReadBuf(PAnsiChar(s), 8196);
      result := Copy(string(s), 1, len - 2);
      {$ELSE}
      SetLength(result, 8196);
      len := Console.Terminal.ReadBuf(PAnsiChar(Result), 8196);
      result := Copy(result, 1, len - 2);
      {$ENDIF}
    end;
  end;
end;

function Console_DoReadKey: string;
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_READKEY);
    DoInvoke(msg);
    SetLength(result, msg.Data.Size div SizeOf(char));
    msg.Data.Read(result[1], msg.Data.Size);
    EndInvoke(msg);
  end else
  begin
    if CONSOLE_UseEditInput then
       result := Console.EditRead(False)
    else
       result := string(Console.Terminal.ReadKey);
  end;
end;

procedure Console_DoClearLine;
var Msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(Msg, WM_CMD_CLEARLN);
    DoInvoke(Msg);
    EndInvoke(Msg);
  end else
    Console.Terminal.ClrEol;
end;

procedure Console_DoCursorTo(x, y: integer);
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_CURSORTO);
    msg.Data.Write(x, SizeOf(integer));
    msg.Data.Write(y, SizeOf(integer));
    DoInvoke(msg);
    EndInvoke(msg);
  end else
    Console.Terminal.CursorTo(x, y);
end;   

procedure Console_DoScrollTo(x, y: integer);
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_SCROLLTO);
    msg.Data.Write(x, SizeOf(integer));
    msg.Data.Write(y, SizeOf(integer));
    DoInvoke(msg);
    EndInvoke(msg);
  end else
    Console.Terminal.ScrollTo(x, y);
end;

procedure Console_DoSetBgColor(value: TColor);
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_SETBGCOLOR);
    msg.Data.Write(value, SizeOf(integer));
    DoInvoke(msg);
    EndInvoke(msg);
  end else
    Console.Terminal.Font.BkColor := value;
end;

procedure Console_DoSetFgColor(value: TColor);
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_SETFGCOLOR);
    msg.Data.Write(value, SizeOf(integer));
    DoInvoke(msg);
    EndInvoke(msg);
  end else
    Console.Terminal.Font.Color := value;
end;

function  Console_DoGetBgColor: TColor;
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_GETBGCOLOR);
    DoInvoke(msg);
    msg.Data.Read(result, SizeOf(integer));
    EndInvoke(msg);
  end else
    result := Console.Terminal.Font.BkColor;
end;

function  Console_DoGetFgColor: TColor;
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_GETFGCOLOR);
    DoInvoke(msg);
    msg.Data.Read(result, SizeOf(integer));
    EndInvoke(msg);
  end else
    result := Console.Terminal.Font.Color;
end;

function  Console_DoGetLine(line: integer): string;
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_GETLINE);
    msg.Data.Write(line, SizeOf(integer));
    DoInvoke(msg);
    SetLength(result, msg.Data.Size div SizeOf(Char));
    msg.Data.Read(result[1], msg.Data.Size);
    EndInvoke(msg);
  end else
    result := IntToStr(line);
end;

procedure Console_DoSetLine(line: integer; s: string);
var msg: TWMSyncMsg;
begin
  if InvokeRequired then
  begin
    BeginInvoke(msg, WM_CMD_SETLINE);
    msg.Data.Write(line, SizeOf(integer));

    line := length(s);
    msg.Data.Write(line, SizeOf(integer));
    msg.Data.Write(s[1], line * SizeOf(char));
    DoInvoke(msg);
    EndInvoke(msg);
  end else
  begin
    Console.Terminal.CursorTo(0, line);

    if length(s) < Console.Terminal.Cols then
       s := s + DupeString(' ', Console.Terminal.Cols - length(s));

    Console.Terminal.WriteString(AnsiString(s));
  end;
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

procedure TConsoleWindow.WMMessageSync(var Message: TWMSyncMsg);
var s: string;
    b: boolean;
    i1, i2: integer;
begin
  inherited;
  if Message.Msg = WM_MSG_SYNC then
  begin
    if Message.Data <> nil then
       Message.Data.Position := 0;

    case Message.Cmd of
    WM_CMD_CLEAR         :
        begin
          Console_DoClear;
          FreeAndNil(Message.Data);
        end;
    WM_CMD_WRITE         :
        begin
          SetLength(s, Message.Data.Size div SizeOf(char));
          Message.Data.Read(s[1], Message.Data.Size);
          Console_DoWrite(s);    
          FreeAndNil(Message.Data);
        end;
    WM_CMD_READLN        :
        begin
          s := Console_DoReadLn;
          Message.Data.Write(s[1], length(s) * SizeOf(char));
        end;
    WM_CMD_READKEY       :
        begin
          s := Console_DoReadKey;    
          Message.Data.Write(s[1], length(s) * SizeOf(char));
        end;
    WM_CMD_KEYPRESSED    :
        begin
          b := Console_DoKeyPressed;
          Message.Data.Write(b, SizeOf(boolean));
        end;
    WM_CMD_CLEARLN       :
        begin
          Console_DoClearLine;  
          FreeAndNil(Message.Data);
        end;
    WM_CMD_CURSORTO      :
        begin
          Message.Data.Read(i1, SizeOf(integer));
          Message.Data.Read(i2, SizeOf(integer));
          Console_DoCursorTo(i1, i2);   
          FreeAndNil(Message.Data);
        end;
    WM_CMD_SCROLLTO      :
        begin       
          Message.Data.Read(i1, SizeOf(integer));
          Message.Data.Read(i2, SizeOf(integer));
          Console_DoScrollTo(i1, i2);     
          FreeAndNil(Message.Data);
        end;
    WM_CMD_GETBGCOLOR    :
        begin
          i1 := Console_DoGetBgColor;
          Message.Data.Write(i1, SizeOf(integer));
        end;
    WM_CMD_GETFGCOLOR    :
        begin
          i1 := Console_DoGetFgColor;   
          Message.Data.Write(i1, SizeOf(integer));
        end;
    WM_CMD_SETBGCOLOR    :
        begin
          Message.Data.Read(i1, SizeOf(integer));
          Console_DoSetBgColor(i1);          
          FreeAndNil(Message.Data);
        end;
    WM_CMD_SETFGCOLOR    :
        begin
          Message.Data.Read(i1, SizeOf(integer));
          Console_DoSetFgColor(i1);    
          FreeAndNil(Message.Data);
        end;
    WM_CMD_GETLINE       :
        begin
          Message.Data.Read(i1, SizeOf(integer));
          s := Console_DoGetLine(i1);
          Message.Data.Clear;
          Message.Data.Write(s[1], length(s) * SizeOf(char));
        end;
    WM_CMD_SETLINE       :
        begin
          Message.Data.Read(i1, SizeOf(integer));
          Message.Data.Read(i2, SizeOf(integer));
          SetLength(s, i2);
          Message.Data.Read(s[1], i2 * SizeOf(char));
          Console_DoSetLine(i1, s);  
          FreeAndNil(Message.Data);
        end;
    end;

    if Message.Data <> nil then
       Message.Data.Position := 0;
  end;
end;

initialization
  MainAppThreadId := GetCurrentThreadId;
  RegisterConsole;

finalization
  FreeAndNil(ConsoleWindow);

end.
