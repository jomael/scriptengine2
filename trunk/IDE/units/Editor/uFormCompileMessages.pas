unit uFormCompileMessages;

interface

uses
  Windows, Classes, SysUtils, Controls, StdCtrls, ExtCtrls, ComCtrls,
  JvComponent, JvExControls, JvGradient, JvTabBar,

  uSE2Compiler, uSE2RunTime, uSE2Errors;

type
  TCompileErrorEvemt = procedure(Sender: TObject; ErrorPos, ErrorLine: integer; EditorData: TObject) of object;

  TMessageViewType = (mvNone, mvMessages, mvSearch, mvRunMessage, mvCustom);

  TCompileMessage = class(TPanel)
  private
    FFirstError     : boolean;
    FMessages       : TListView;
    FOnCompileError : TCompileErrorEvemt;
  protected
    procedure ParserError(Sender: TObject; ErrorType: TSE2ErrorType; ErrorUnit, ErrorText: string; ErrorPos,
                          ErrorLine: integer; UserData: TObject);
    procedure ListDblClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure AddCompileMsg(MsgType: TSE2ErrorType; ErrorUnit, ErrorText: string; ErrorPos, ErrorLine: integer; Data: TObject);

    property Messages        : TListView           read FMessages;
    property OnCompileError  : TCompileErrorEvemt  read FOnCompileError    write FOnCompileError;
  end;

  TSearchResult = class(TPanel)
  private
    FItems: TListView;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const UnitName: string; SearchLine, Row: integer; const LineText: string; Data: TObject);

    property Items: TListView read FItems;
  end;

  TRunTimeError = class(TPanel)
  private
    FMessages : TMemo;
  protected
    procedure RunTimeError(Sender: TObject; Exp: ExceptClass; const Msg: string; CodePos: integer; const CallStack: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure DoAddError(Exp: ExceptClass; const Msg: string; CodePos: integer; const CallStack: string);

    property  Messages : TMemo  read FMessages;
  end;

  TEditorMessages = class(TPanel)
  private
    FTabLists         : TJvTabBar;
    FViewType         : TMessageViewType;
    FOnVisibleChanged : TNotifyEvent;
  protected
    procedure CodeTabClosing(Sender: TObject; Item: TJvTabBarItem; var AllowClose: boolean);
    procedure CodeTabSelected(Sender: TObject; Item: TJvTabBarItem);

    procedure SetViewType(aType: TMessageViewType);

    function  GetMessage: TCompileMessage;
    function  GetSearch: TSearchResult;
    function  GetRunTime: TRunTimeError;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateMessages;

    procedure RegisterCompiler(Comp: TSE2Compiler);
    procedure RegisterRunTime(Run: TSE2RunTime);

    procedure AddPanel(const Panel: TCustomPanel; Caption: string);
    procedure SetActiveIndex(i: integer);

    property  Messages : TCompileMessage   read GetMessage;
    property  Search   : TSearchResult     read GetSearch;
    property  RunTime  : TRunTimeError     read GetRunTime;
    property  ViewType : TMessageViewType  read FViewType   write SetViewType;

    property  OnVisibleChanged : TNotifyEvent read FOnVisibleChanged write FOnVisibleChanged;
  end;


implementation

{ TCompileMessage }

procedure TCompileMessage.AddCompileMsg(MsgType: TSE2ErrorType; ErrorUnit,
  ErrorText: string; ErrorPos, ErrorLine: integer; Data: TObject);
var Item: TListItem;
begin
  if FFirstError and (MsgType = petError) then
     Item := FMessages.Items.Insert(0)
  else
     Item := FMessages.Items.Add;
  case MsgType of
  petHint    : Item.Caption := 'Hint';
  petWarning : Item.Caption := 'Warning';
  petError   : Item.Caption := 'Error';
  end;

  Item.SubItems.Add(ErrorUnit);
  Item.SubItems.Add(ErrorText);
  Item.SubItems.Add(IntToStr(ErrorLine));
  Item.SubItems.Add(IntToStr(ErrorPos));

  Item.Data := Data;
  if MsgType = petError then
  begin
    if FFirstError then
    begin
      TEditorMessages(Self.Parent).ViewType := mvMessages;
      if Assigned(FOnCompileError) then
         FOnCompileError(Self, ErrorPos, ErrorLine, Data);
    end;
    if ErrorUnit <> '' then
       FFirstError := False;
  end;
end;

procedure TCompileMessage.Clear;
begin
  FFirstError := True;
  FMessages.Clear;
end;

constructor TCompileMessage.Create(AOwner: TComponent);

  procedure AddColumn(Caption: string; Width: integer; Align: TAlignment);
  var Column : TListColumn;
  begin
    Column := FMessages.Columns.Add;
    Column.Caption   := Caption;
    Column.Width     := Width;
    Column.Alignment := Align;
  end;

begin
  inherited;

  FMessages := TListView.Create(Self);
  FMessages.Parent      := Self;
  FMessages.Align       := alClient;
  FMessages.ColumnClick := False;
  FMessages.ReadOnly    := True;
  FMessages.RowSelect   := True;
  FMessages.ViewStyle   := vsReport;
  FMessages.OnDblClick  := ListDblClick;

  AddColumn('Type', 50, taLeftJustify);
  AddColumn('Unit', 80, taLeftJustify);
  AddColumn('Message', 350, taLeftJustify);
  AddColumn('Line', 40, taRightJustify); 
  BevelOuter := bvNone;
end;

destructor TCompileMessage.Destroy;
begin
  FMessages.Free;
  inherited;
end;

procedure TCompileMessage.ListDblClick(Sender: TObject);
begin
  if FMessages.Selected <> nil then
    if FMessages.Selected.Data <> nil then
      if Assigned(FOnCompileError) then
      begin
        FOnCompileError(Self, StrToInt(FMessages.Selected.SubItems[3]), StrToInt(FMessages.Selected.SubItems[2]), TObject(FMessages.Selected.Data));
      end;
end;

procedure TCompileMessage.ParserError(Sender: TObject;
  ErrorType: TSE2ErrorType; ErrorUnit, ErrorText: string; ErrorPos,
  ErrorLine: integer; UserData: TObject);
begin
  if ErrorType = petError then
     TEditorMessages(Self.Parent).ViewType := mvMessages;
  AddCompileMsg(ErrorType, ErrorUnit, ErrorText, ErrorPos, ErrorLine, UserData);
end;

{ TSearchResult }

procedure TSearchResult.Add(const UnitName: string; SearchLine,
  Row: integer; const LineText: string; Data: TObject);
var Item: TListItem;
begin
  Item := FItems.Items.Add;

  Item.Caption := UnitName;
  Item.SubItems.Add(Format('[%d %d]', [SearchLine, Row]));
  Item.SubItems.Add(LineText);
  Item.Data := Data;
end;

procedure TSearchResult.Clear;
begin
  FItems.Clear;
end;

constructor TSearchResult.Create(AOwner: TComponent);

  procedure AddColumn(Caption: string; Width: integer; Align: TAlignment);
  var Column : TListColumn;
  begin
    Column := FItems.Columns.Add;
    Column.Caption   := Caption;
    Column.Width     := Width;
    Column.Alignment := Align;
  end;

begin
  inherited;

  FItems := TListView.Create(Self);
  FItems.Parent      := Self;
  FItems.Align       := alClient;
  FItems.ColumnClick := False;
  FItems.ReadOnly    := True;
  FItems.RowSelect   := True;
  FItems.ViewStyle   := vsReport;
  //FItems.OnDblClick  := ListDblClick;

  AddColumn('Unit Name', 80, taLeftJustify);
  AddColumn('Position', 50, taLeftJustify);
  AddColumn('Line', 500, taLeftJustify);
  BevelOuter := bvNone;
end;

destructor TSearchResult.Destroy;
begin
  FItems.Free;
  inherited;
end;

{ TRunTimeError }

procedure TRunTimeError.Clear;
begin
  FMessages.Clear;
end;

constructor TRunTimeError.Create(AOwner: TComponent);
begin
  inherited;

  FMessages := TMemo.Create(Self);
  FMessages.Parent      := Self;
  FMessages.Align       := alClient;
  FMessages.ReadOnly    := True;
  FMessages.WordWrap    := False;
  FMessages.ScrollBars  := ssBoth;
  BevelOuter := bvNone;
end;

destructor TRunTimeError.Destroy;
begin
  FMessages.Free;
  inherited;
end;

procedure TRunTimeError.DoAddError(Exp: ExceptClass; const Msg: string; CodePos: integer;
  const CallStack: string);
begin
  FMessages.Lines.Add('================================================================');
  FMessages.Lines.Add('RunTime - Exception: ' + Exp.ClassName);
  FMessages.Lines.Add(Msg);
  FMessages.Lines.Add('');
  FMessages.Lines.Add('Call stack:');
  FMessages.Lines.Add(CallStack);
  FMessages.Lines.Add('[MAIN ENTRY POINT]');
  FMessages.Lines.Add('================================================================');
  FMessages.Lines.Add('');
end;

procedure TRunTimeError.RunTimeError(Sender: TObject; Exp: ExceptClass; const Msg: string;
  CodePos: integer; const CallStack: string);
begin
  DoAddError(Exp, Msg, CodePos, CallStack);
  if Parent <> nil then
     TEditorMessages(Parent).ViewType := mvRunMessage;
end;

{ TEditorMessages }

procedure TEditorMessages.CodeTabClosing(Sender: TObject;
  Item: TJvTabBarItem; var AllowClose: boolean);
begin
  AllowClose := False;
  ViewType   := mvNone;
end;

procedure TEditorMessages.CodeTabSelected(Sender: TObject;
  Item: TJvTabBarItem);
var i: integer;
begin
  if FTabLists.Tabs.Count < 3 then
     exit;

  for i:=0 to FTabLists.Tabs.Count-1 do
    TPanel(FTabLists.Tabs[i].Data).Visible := Item = FTabLists.Tabs[i];

  case Item.Index of
  0 : FViewType := mvMessages;
  1 : FViewType := mvSearch;
  2 : FViewType := mvRunMessage;
  end;
end;

constructor TEditorMessages.Create(AOwner: TComponent);
begin
  inherited;   
  BevelOuter := bvNone;
end;

procedure TEditorMessages.CreateMessages;
var //Tab   : TJvTabBarItem;
    FComp : TCompileMessage;
    FSrch : TSearchResult;
    FRun  : TRunTimeError;
begin
  Visible   := False;

  FTabLists := TJvTabBar.Create(Self);
  FTabLists.Parent := Self;
  FTabLists.Align  := alTop;
  FTabLists.OnTabSelected := CodeTabSelected;
  FTabLists.OnTabClosing  := CodeTabClosing;

  FViewType := mvNone;

  FComp := TCompileMessage.Create(Self);
  AddPanel(FComp, 'Messages');
  FComp.Visible := True;
  
  FSrch := TSearchResult.Create(Self);
  AddPanel(FSrch, 'Search');

  FRun := TRunTimeError.Create(Self);
  AddPanel(FRun, 'Debug Messages');

                 (*
  FComp.Parent := Self;
  FComp.Align  := alClient;
  Tab := FTabLists.AddTab('Messages');
  Tab.Data := FComp;    *)
                        (*
  FSrch := TSearchResult.Create(Self);
  FSrch.Parent  := Self;
  FSrch.Align   := alClient;
  FSrch.Visible := False;
  Tab := FTabLists.AddTab('Search');
  Tab.Data := FSrch;

  FRun := TRunTimeError.Create(Self);
  FRun.Parent   := Self;
  FRun.Align    := alClient;
  FRun.Visible  := False;
  Tab := FTabLists.AddTab('Debug Messages');
  Tab.Data := FRun;  *)
end;

procedure TEditorMessages.AddPanel(const Panel: TCustomPanel;
  Caption: string);
begin
  Panel.Parent := Self;
  Panel.Align := alClient;
  Panel.Visible := False;
  FTabLists.AddTab(Caption).Data := Panel;
end;

destructor TEditorMessages.Destroy;
var i: integer;
begin
  FTabLists.OnTabSelected := nil;
  for i:=FTabLists.Tabs.Count-1 downto 0 do
    FTabLists.Tabs[i].Free;
  FTabLists.Free;
  inherited;
end;

function TEditorMessages.GetMessage: TCompileMessage;
begin
  result := TCompileMessage(FTabLists.Tabs[0].Data);
end;

function TEditorMessages.GetRunTime: TRunTimeError;
begin
  result := TRunTimeError(FTabLists.Tabs[2].Data);
end;

function TEditorMessages.GetSearch: TSearchResult;
begin
  result := TSearchResult(FTabLists.Tabs[1].Data);
end;

procedure TEditorMessages.RegisterCompiler(Comp: TSE2Compiler);
begin
  Comp.OnCompilerError := TCompileMessage(FTabLists.Tabs[0].Data).ParserError;
end;

procedure TEditorMessages.RegisterRunTime(Run: TSE2RunTime);
begin
  Run.OnError := TRunTimeError(FTabLists.Tabs[2].Data).RunTimeError;
end;

procedure TEditorMessages.SetActiveIndex(i: integer);
begin
  if (i >= 0) and (i < FTabLists.Tabs.Count) then
     FTabLists.Tabs[i].Selected := True;
end;

procedure TEditorMessages.SetViewType(aType: TMessageViewType);
begin
  FViewType := aType;
  case aType of
  mvNone     :
      begin
        Visible := False;
        if Assigned(FOnVisibleChanged) then
           FOnVisibleChanged(Self)
      end;
  mvMessages :
      begin
        FTabLists.Tabs[0].Selected := True;
        Visible := True;   
        if Assigned(FOnVisibleChanged) then
           FOnVisibleChanged(Self);
      end;
  mvSearch :
      begin
        FTabLists.Tabs[1].Selected := True;
        Visible := True;        
        if Assigned(FOnVisibleChanged) then
           FOnVisibleChanged(Self);
      end;
  mvRunMessage :
      begin
        FTabLists.Tabs[2].Selected := True;
        Visible := True;    
        if Assigned(FOnVisibleChanged) then
           FOnVisibleChanged(Self);
      end;
  else
  begin
    Visible := True;
    if Assigned(FOnVisibleChanged) then
       FOnVisibleChanged(Self);
  end;
  end;
end;

end.
