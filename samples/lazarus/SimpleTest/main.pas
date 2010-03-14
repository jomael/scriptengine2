unit main;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,

  uSE2Compiler, // für den Compiler
  uSE2UnitCacheMngr, // für den Unit-Cache-Manager
  uSE2Errors, // für TSE2ErrorType
  uSE2Reader, // wird zum Lesen der Daten benutzt
  uSE2PEData, // die ByteCode-Daten
  uSE2RunTime, // für die RunTime
  uSE2IncConsole, ExtCtrls, StdCtrls, Menus, LResources;

type

  { TForm1 }

  TMyEvent = procedure(Sender: TObject) of object register;

  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Close1: TMenuItem;
    Compile1: TMenuItem;
    Compile2: TMenuItem;
    CompileandRun1: TMenuItem;
    Memo1: TMemo;
    Memo2: TMemo;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Compile2Click(Sender: TObject);
    procedure CompileandRun1Click(Sender: TObject);
  private
    { Private-Deklarationen }
    FUnitCache : TSE2UnitCacheMngr;

    procedure CompilerError(Sender: TObject; ErrorType: TSE2ErrorType;
      ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: integer;
      UserData: TObject);
    procedure CompilerNeedUnit(Sender: TObject; const Name: string;
      const Readers: TList);

    function DoCompile: TSE2PE;
    procedure RunTimeError(Sender: TObject; Exp: ExceptClass;
      const Msg: string; CodePos: integer; const CallStack: string);
  public
    { Public-Deklarationen }
    TestEventVar : TNotifyEvent;
    procedure TestEvent(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation


procedure ScriptConsoleWrite(const s: string);
begin
  Form1.Memo2.Lines.Add(Trim(s));
end;

var mhmhm : TNotifyEvent;

procedure AssignTestEvent(obj: TForm1; event: TNotifyEvent); register;
var p: pointer;
begin
  p := obj;
  mhmhm := event;
end;

procedure TForm1.TestEvent(Sender: TObject);
begin
  //
end;

procedure TForm1.FormCreate(Sender: TObject);
var p: TNotifyEvent;
begin
  TSE2Console.Write := @ScriptConsoleWrite; 
  FUnitCache := TSE2UnitCacheMngr.Create;

  AssignTestEvent(Self, TestEvent);
  //TestEventVar(nil);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FUnitCache.Free;
end;

procedure TForm1.CompilerError(Sender: TObject; ErrorType: TSE2ErrorType;
  ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: integer; UserData: TObject);
var s: string;
begin
  case ErrorType of
  petHint : s := 'Hint';
  petWarning : s := 'Warning';
  petError : s := 'Error';
  end;

  Memo2.Lines.Add(s + Format(': [%s] [Line %d]: %s', [ErrorUnit, ErrorLine, ErrorText] ));
end;

procedure TForm1.CompilerNeedUnit(Sender: TObject; const Name: string;
  const Readers: TList);
begin
  //
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
  Close;
end;

function TForm1.DoCompile: TSE2PE;
var Compiler : TSE2Compiler;
begin
  Memo2.Clear;
  Compiler := TSE2Compiler.Create;
  try
    Compiler.UnitCache       := FUnitCache;
    Compiler.OnCompilerError := CompilerError;
    Compiler.OnGetFile       := CompilerNeedUnit;
    result := Compiler.Compile(Memo1.Text);
  finally
    // den Compiler freigeben
    Compiler.Free;
  end;
end;

procedure TForm1.Compile2Click(Sender: TObject);
var PE: TSE2PE;
begin
  PE := DoCompile;
  if PE <> nil then
     PE.Free;
end;

procedure TForm1.CompileandRun1Click(Sender: TObject);
var RunTime: TSE2RunTime;
    PE     : TSE2PE;
begin
  PE := DoCompile;
  if PE <> nil then
  begin
    RunTime := TSE2RunTime.Create;
    try
      RunTime.AppCode := PE;
      RunTime.OnError := RunTimeError;

      RunTime.Initialize;
      RunTime.Run;
      RunTime.Finalize;
    finally
      RunTime.Free;
    end;
  end;
end;

procedure TForm1.RunTimeError(Sender: TObject; Exp: ExceptClass;
  const Msg: string; CodePos: integer; const CallStack: string);
begin
  // Den Script-RunTime fehler anzeigen
  // Exp ist nicht die Exception selbst, sondern nur die Klasse
  // der Exception, da diese vorher freigegeben wird.
  Memo2.Lines.Add('RunTime error: '+#13#10+
              Exp.ClassName + ': ' + Msg + #13#10#13#10 +
              CallStack);
end;


initialization
  {$i main.lrs}

end.
 
