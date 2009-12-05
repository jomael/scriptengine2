unit uFormCodeEditor;

interface

uses
  Windows, Classes, Types, SysUtils, Controls, Dialogs, Graphics, StdCtrls, ExtCtrls,
  Menus,

  uFormCompileMessages,

  JvComponent, JvExControls, JvGradient, JvTabBar,

  SynEdit, SynCompletionProposal, SynEditRegexSearch, SynEditMiscClasses,
  SynEditSearch, SynHighlighterSE2,
  
  uSE2Errors, uSE2PEData, uSE2RunTime, uSE2DebugData, uSE2CodeCompletion,
  uSE2Compiler, uSE2RunType, uSE2UnitCacheMngr;


type
  TEditorGetCustomUnits = uSE2CodeCompletion.TSE2GetCustomUnits;

  TCodeEditor = class(TObject)
  private
    FEditor        : TSynEdit;
    FErrorLine     : integer;

    FCodeComplete  : TSynCompletionProposal;
    FParamComplete : TSynCompletionProposal;
    FHighlighter   : TSynSE2Syn;

    FOnGetUnit     : TSE2GetFileReader;
    FOnGetUnitMngr : TSE2GetUnitMngr;
    FOnGetCustomUnits : TEditorGetCustomUnits;
  protected
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure EditorLineColor(Sender: TObject; Line: integer; var Special: boolean; var FG, BG: TColor);

    procedure CodeCompleteInsert(Sender: TObject; const DisplayText, InsertText: string);
    procedure ParamCompleteInsert(Sender: TObject; const DisplayText, InsertText: string);

    procedure CompleteGetUnitMngr(Sender: TObject; var UnitMngr: TSE2UnitCacheMngr);

    procedure CodeCompleteExecute(Kind: TSynCompletionType;
      Sender: TObject; var CurrentInput: String; var x, y: Integer;
      var CanExecute: Boolean);
    procedure ParamCompleteExecute(Kind: TSynCompletionType;
      Sender: TObject; var CurrentInput: String; var x, y: Integer;
      var CanExecute: Boolean);

    function  GetSource: string;
    procedure SetSource(const value: string);

    procedure SetErrorLine(value: integer);
  public
    constructor Create(AParent: TWinControl); reintroduce;
    destructor Destroy; override;
    class procedure SetupColors(FHighlighter: TSynSE2Syn; FEditor: TSynEdit);
                                    
    property  CodeComplete   : TSynCompletionProposal  read FCodeComplete;
    property  ParamComplete  : TSynCompletionProposal  read FParamComplete;
    property  Highlighter    : TSynSE2Syn              read FHighlighter;
    property  Editor         : TSynEdit                read FEditor;
    property  ErrorLine      : integer                 read FErrorLine       write SetErrorLine;
    property  Source         : string                  read GetSource        write SetSource;
  end;

  TCodeEditorPanel = class(TPanel)
  private
    FEditor      : TCodeEditor;
    FFileName    : string;
    FTabEntry    : TJvTabBarItem;
    FData        : TObject;

    FOnGetUnit     : TSE2GetFileReader;
    FOnGetUnitMngr : TSE2GetUnitMngr;
    FOnGetCustomUnits : TEditorGetCustomUnits;
  protected
    function  GetCaption: string;
    procedure SetCaption(const value: string);

    function  GetModified: boolean;
    procedure SetModified(value: boolean);

    procedure CompleteGetUnitMngr(Sender: TObject; var UnitMngr: TSE2UnitCacheMngr);

    procedure EditorChanged(Sender: TObject);
    procedure EditorGetUnit(Sender: TObject; const Name: string; const Readers: TList);
    procedure EditorGetCustomUnits(Sender: TObject; const Target: TStrings);
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    property  Modified   : boolean        read GetModified   write SetModified;
    property  Editor     : TCodeEditor    read FEditor;
    property  TabEntry   : TJvTabBarItem  read FTabEntry     write FTabEntry;
    property  FileName   : string         read FFileName     write FFileName;
    property  Caption    : string         read GetCaption    write SetCaption;
    property  Data       : TObject        read FData         write FData;
  end;

  TRequireSaveEvent = procedure(Sender: TObject; CodeTab: TCodeEditorPanel; var CanClose: boolean) of object;
  TRequireUnitEvent = procedure(Sender: TObject; Data: TObject) of object;

  TScriptEditor = class(TPanel)
  private
    FCodeTabs       : TJvTabBar;  
    FOnGetUnit      : TSE2GetFileReader;
    FOnGetUnitMngr  : TSE2GetUnitMngr;
    FOnGetCustomUnits : TEditorGetCustomUnits;
    FOnRequireSave  : TRequireSaveEvent;
    FOnRequireUnit  : TRequireUnitEvent;
  protected
    procedure CodeTabClosing(Sender: TObject; Item: TJvTabBarItem; var AllowClose: boolean);
    procedure CodeTabSelected(Sender: TObject; Item: TJvTabBarItem);
    procedure CodeTabClosed(Sender: TObject; Item: TJvTabBarItem);
                                                                 
    procedure CompleteGetUnitMngr(Sender: TObject; var UnitMngr: TSE2UnitCacheMngr);
    procedure EditorGetUnit(Sender: TObject; const Name: string; const Readers: TList);
    procedure EditorGetCustomUnits(Sender: TObject; const Target: TStrings);

    function  GetSelected: TCodeEditorPanel;
    procedure SetSelected(value: TCodeEditorPanel);

    function  GetItem(index: integer): TCodeEditorPanel;
    function  GetCount: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    function  Add(Caption: string): TCodeEditorPanel;
    function  Open(const Caption: string): TCodeEditorPanel;
    procedure Close(aData: TObject);

    function  GetByData(aData: TObject): TCodeEditorPanel;
    function  IndexOf(Caption: string): integer;

    property  Selected             : TCodeEditorPanel     read GetSelected   write SetSelected;
    property  Items[index: integer]: TCodeEditorPanel     read GetItem; default;
    property  Count                : integer              read GetCount;

    property  OnGetUnit            : TSE2GetFileReader    read FOnGetUnit      write FOnGetUnit;
    property  OnGetUnitMngr        : TSE2GetUnitMngr      read FOnGetUnitMngr  write FOnGetUnitMngr;
    property  OnGetCustomUnits     : TEditorGetCustomUnits read FOnGetCustomUnits write FOnGetCustomUnits;
    property  OnRequireSave        : TRequireSaveEvent    read FOnRequireSave  write FOnRequireSave;
    property  OnRequireUnit        : TRequireUnitEvent    read FOnRequireUnit  write FOnRequireUnit;
  end;

  TScriptEditorMngr = class(TPanel)
  private
    FEditor   : TScriptEditor;
    FSplitter : TSplitter;
    FMessages : TEditorMessages;
  protected
    procedure MessagesVisibleChanged(Sender: TObject);
    procedure CompileErrorEvent(Sender: TObject; ErrorPos, ErrorLine: integer; EditorData: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateEditor;

    property  Editor   : TScriptEditor   read FEditor;
    property  Messages : TEditorMessages read FMessages;
  end;

implementation

{ TCodeEditor }

procedure TCodeEditor.CodeCompleteExecute(Kind: TSynCompletionType;
  Sender: TObject; var CurrentInput: String; var x, y: Integer;
  var CanExecute: Boolean);
begin
  FCodeComplete.ClearList;

  with TSE2SynCodeCompletion.Create(CodeCompleteInsert) do
  try
    OnGetCustomUnits   := FOnGetCustomUnits;
    OnGetUnitMngr      := CompleteGetUnitMngr;
    Compiler.OnGetFile := FOnGetUnit;
    CanExecute         := GetCodeCompletion(Source, FEditor.SelStart);
    FCodeComplete.AddItem('', '');
  finally
    Free;
  end;
end;

procedure TCodeEditor.CodeCompleteInsert(Sender: TObject;
  const DisplayText, InsertText: string);
begin
  FCodeComplete.AddItem(DisplayText, InsertText);
end;

procedure TCodeEditor.CompleteGetUnitMngr(Sender: TObject;
  var UnitMngr: TSE2UnitCacheMngr);
begin
  if Assigned(FOnGetUnitMngr) then
     FOnGetUnitMngr(Self, UnitMngr);
end;

constructor TCodeEditor.Create(AParent: TWinControl);
begin
  inherited Create;
  FEditor := TSynEdit.Create(AParent);
  FEditor.Parent := AParent;
  FEditor.Align  := alClient;

  FHighlighter   := TSynSE2Syn.Create(AParent);

  FCodeComplete  := TSynCompletionProposal.Create(AParent);
  FCodeComplete.OnExecute      := CodeCompleteExecute;
  FCodeComplete.Options        := [scoLimitToMatchedText,scoTitleIsCentered,scoUseInsertList,scoUsePrettyText,scoUseBuiltInTimer,scoEndCharCompletion,scoCompleteWithTab,scoCompleteWithEnter];
  FCodeComplete.TimerInterval  := 50;
  FCodeComplete.Columns.Add.BiggestWord := 'CONSTRUCTOR';
  FCodeComplete.Editor         := FEditor;
  FCodeComplete.EndOfTokenChr  := '()[]. 1234567890+-*/';
  FCodeComplete.TriggerChars   := '.';
  FCodeComplete.Title          := 'ScriptEngine II';

  FParamComplete := TSynCompletionProposal.Create(AParent);
  FParamComplete.DefaultType   := ctParams;
  FParamComplete.Editor        := FEditor;
  FParamComplete.EndOfTokenChr := '()[].';
  FParamComplete.Options       := [scoLimitToMatchedText,scoUsePrettyText,scoUseBuiltInTimer];
  FParamComplete.TimerInterval := 50;
  FParamComplete.ShortCut      := ShortCut(VK_SPACE, [ssCtrl, ssShift]);
  FParamComplete.TriggerChars  := '([';
  FParamComplete.OnExecute     := ParamCompleteExecute;

  FEditor.Highlighter         := FHighlighter;
  FEditor.Gutter.Font.Name    := 'MS Sans Serif';
  FEditor.Gutter.Gradient     := True;
  FEditor.Gutter.LeftOffset   := 10;
  FEditor.Gutter.ShowLineNumbers := True;
  FEditor.Options             := [eoAutoIndent,eoDragDropEditing,eoGroupUndo,eoScrollHintFollows,eoScrollPastEof,eoScrollPastEol,eoShowScrollHint,eoSmartTabDelete,eoTabsToSpaces];
  FEditor.ScrollHintFormat    := shfTopToBottom;
  FEditor.TabWidth            := 2;
  FEditor.WantTabs            := True;
  FEditor.OnMouseDown         := EditorMouseDown;
  FEditor.OnSpecialLineColors := EditorLineColor;
  FErrorLine                  := -1;

  SetupColors(FHighlighter, FEditor);
end;

destructor TCodeEditor.Destroy;
begin
  FHighlighter.Free;
  FCodeComplete.Free;
  FParamComplete.Free;
  FEditor.Free;
  inherited;
end;

procedure TCodeEditor.EditorLineColor(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
begin
  if Line = FErrorLine then
  begin
    Special := True;
    BG      := clMaroon;
    FG      := clWhite;
  end else
    Special := False;
end;

procedure TCodeEditor.EditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FErrorLine := -1;
  FEditor.Refresh;
end;

function TCodeEditor.GetSource: string;
begin
  result := FEditor.Text;
end;

procedure TCodeEditor.ParamCompleteExecute(Kind: TSynCompletionType;
  Sender: TObject; var CurrentInput: String; var x, y: Integer;
  var CanExecute: Boolean);
var index: integer;
begin
  FParamComplete.ClearList;

  with TSE2SynParamCompletion.Create(ParamCompleteInsert) do
  try
    OnGetUnitMngr      := CompleteGetUnitMngr;
    Compiler.OnGetFile := FOnGetUnit;
    CanExecute := GetParamCompletion(Source, FEditor.SelStart, index);
    if TWinControl(FParamComplete.Form).Visible then
       CanExecute := True;
    FParamComplete.Form.CurrentIndex := index;
  finally
    Free;
  end;
end;

procedure TCodeEditor.ParamCompleteInsert(Sender: TObject;
  const DisplayText, InsertText: string);
begin
  FParamComplete.ItemList.Add(DisplayText);
end;

procedure TCodeEditor.SetErrorLine(value: integer);
begin
  if value <> FErrorLine then
  begin
    FErrorLine := value;
    FEditor.Refresh;
  end;
end;

procedure TCodeEditor.SetSource(const value: string);
begin
  SetErrorLine(-1);
  FEditor.Text := value;
end;

class procedure TCodeEditor.SetupColors(FHighlighter: TSynSE2Syn; FEditor: TSynEdit);
begin
  with FHighlighter do
  begin
    CommentAttri.Style       := [fsItalic];
    CommentAttri.Foreground  := clBlue;
    DirectiveAttri.Style     := [fsItalic];
    DirectiveAttri.Foreground:= clMaroon;
    FloatAttri.Foreground    := clFuchsia;
    FloatAttri.Style         := [fsBold];
    HexAttri.Foreground      := clFuchsia;
    HexAttri.Style           := [fsBold];
    KeyAttri.Foreground      := clGreen;
    KeyAttri.Style           := [fsBold];
    NumberAttri.Foreground   := clFuchsia;
    NumberAttri.Style        := [fsBold];
    StringAttri.Foreground   := clNavy;
    StringAttri.Style        := [];
    SymbolAttri.Foreground   := clRed;
    SymbolAttri.Style        := [];
  end;
  with FEditor do
  begin
    Color                     := clWhite;
    Font.Color                := clBlack;
    Gutter.BorderStyle        := gbsNone;
    Gutter.GradientStartColor := clSilver;
    Gutter.GradientEndColor   := RGB(245, 245, 245);
  end
end;

{ TCodeEditorPanel }

procedure TCodeEditorPanel.CompleteGetUnitMngr(Sender: TObject;
  var UnitMngr: TSE2UnitCacheMngr);
begin
  if Assigned(FOnGetUnitMngr) then
     FOnGetUnitMngr(Self, UnitMngr);
end;

constructor TCodeEditorPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditor := TCodeEditor.Create(Self);
  FEditor.Editor.OnChange := EditorChanged;
  FEditor.FOnGetUnit      := EditorGetUnit;
  FEditor.FOnGetCustomUnits := EditorGetCustomUnits;
  FEditor.FOnGetUnitMngr  := CompleteGetUnitMngr;
  BevelOuter := bvNone;
end;

destructor TCodeEditorPanel.Destroy;
begin
  FEditor.Free;
  inherited;
end;

procedure TCodeEditorPanel.EditorChanged(Sender: TObject);
begin
  Modified := True;
  FEditor.ErrorLine := -1;
end;

procedure TCodeEditorPanel.EditorGetCustomUnits(Sender: TObject;
  const Target: TStrings);
begin
  if Assigned(FOnGetCustomUnits) then
     FOnGetCustomUnits(Sender, Target);
end;

procedure TCodeEditorPanel.EditorGetUnit(Sender: TObject;
  const Name: string; const Readers: TList);
begin
  if Assigned(FOnGetUnit) then
     FOnGetUnit(Sender, Name, Readers);
end;

function TCodeEditorPanel.GetCaption: string;
begin
  result := FTabEntry.Caption;
end;

function TCodeEditorPanel.GetModified: boolean;
begin
  result := FTabEntry.Modified;
end;

procedure TCodeEditorPanel.SetCaption(const value: string);
begin
  FTabEntry.Caption := value;
end;

procedure TCodeEditorPanel.SetModified(value: boolean);
begin
  FTabEntry.Modified := value;
end;

{ TScriptEditor }

function TScriptEditor.Add(Caption: string): TCodeEditorPanel;
begin
  if Caption = '' then
     Caption := 'unnamed';

  result := TCodeEditorPanel.Create(Self);
  result.Parent    := Self;
  result.Align     := alClient;
  result.Visible   := False;
  result.TabEntry  := FCodeTabs.AddTab(Caption);
  result.TabEntry.Data := result;
  result.FOnGetUnit    := EditorGetUnit;
  result.FOnGetUnitMngr := CompleteGetUnitMngr;
  result.FOnGetCustomUnits := EditorGetCustomUnits;
end;

procedure TScriptEditor.Clear;
var i: integer;
begin
  for i:=FCodeTabs.Tabs.Count-1 downto 0 do
    FCodeTabs.Tabs[i].Free;
end;

procedure TScriptEditor.Close(aData: TObject);
var i: integer;
begin
  for i:=Self.Count-1 downto 0 do
    if Items[i].Data = aData then
    begin
      FCodeTabs.Tabs.Delete(i);
    end;
end;

procedure TScriptEditor.CodeTabClosed(Sender: TObject;
  Item: TJvTabBarItem);
var CanClose : boolean;
begin
  if TCodeEditorPanel(Item.Data).Modified then
  begin
    CanClose := True;
    if Assigned(FOnRequireSave) then
       FOnRequireSave(Self, TCodeEditorPanel(Item.Data), CanClose);

    if not CanClose then
       exit;
  end;
  FreeAndNil(Item);
end;

procedure TScriptEditor.CodeTabClosing(Sender: TObject;
  Item: TJvTabBarItem; var AllowClose: boolean);
begin
  AllowClose := True;
end;

procedure TScriptEditor.CodeTabSelected(Sender: TObject;
  Item: TJvTabBarItem);
var i: integer;
begin
  for i:=FCodeTabs.Tabs.Count-1 downto 0 do
    if Assigned(FCodeTabs.Tabs[i].Data) then
      TCodeEditorPanel(FCodeTabs.Tabs[i].Data).Visible := FCodeTabs.Tabs[i] = Item;
end;

procedure TScriptEditor.CompleteGetUnitMngr(Sender: TObject;
  var UnitMngr: TSE2UnitCacheMngr);
begin
  if Assigned(FOnGetUnitMngr) then
     FOnGetUnitMngr(Sender, UnitMngr);
end;

constructor TScriptEditor.Create(AOwner: TComponent);
begin
  inherited;

  FCodeTabs := TJvTabBar.Create(Self);
  FCodeTabs.Parent  := Self;
  FCodeTabs.Align   := alTop;
  FCodeTabs.AutoFreeClosed := False;
  FCodeTabs.OnTabClosing   := CodeTabClosing;
  FCodeTabs.OnTabClosed    := CodeTabClosed;
  FCodeTabs.OnTabSelected  := CodeTabSelected;
  
  BevelOuter := bvNone;
end;

destructor TScriptEditor.Destroy;
begin
  Clear;
  FCodeTabs.Free;
  inherited;
end;

procedure TScriptEditor.EditorGetCustomUnits(Sender: TObject;
  const Target: TStrings);
begin
  if Assigned(FOnGetCustomUnits) then
     FOnGetCustomUnits(Sender, Target);
end;

procedure TScriptEditor.EditorGetUnit(Sender: TObject; const Name: string;
  const Readers: TList);
begin
  if Assigned(FOnGetUnit) then
     FOnGetUnit(Sender, Name, Readers);
end;

function TScriptEditor.GetByData(aData: TObject): TCodeEditorPanel;
var i: integer;
begin
  for i:=0 to Self.Count-1 do
    if Items[i].Data = aData then
    begin
      result := Items[i];
      exit;
    end;
  result := nil;
end;

function TScriptEditor.GetCount: integer;
begin
  result := FCodeTabs.Tabs.Count;
end;

function TScriptEditor.GetItem(index: integer): TCodeEditorPanel;
begin
  if (index < 0) or (index >= Count) then
     result := nil
  else
     result := TCodeEditorPanel(FCodeTabs.Tabs[index].Data);
end;

function TScriptEditor.GetSelected: TCodeEditorPanel;
begin
  if FCodeTabs.SelectedTab = nil then
     result := nil
  else
     result := TCodeEditorPanel(FCodeTabs.SelectedTab.Data);
end;

function TScriptEditor.IndexOf(Caption: string): integer;
begin
  Caption := AnsiUpperCase(Caption);
  for result:=FCodeTabs.Tabs.Count-1 downto 0 do
    if AnsiUpperCase(TCodeEditorPanel(FCodeTabs.Tabs[result].Data).Caption) = Caption then
      exit;
  result := -1;
end;

function TScriptEditor.Open(const Caption: string): TCodeEditorPanel;
begin
  result   := Add(Caption);
  Selected := result;
  result.Visible := True;
end;

procedure TScriptEditor.SetSelected(value: TCodeEditorPanel);
begin
  if value <> nil then
     if value.FTabEntry <> nil then
     begin
       value.FTabEntry.Selected := True;
     end;
end;

{ TScriptEditorMngr }

procedure TScriptEditorMngr.CompileErrorEvent(Sender: TObject; ErrorPos,
  ErrorLine: integer; EditorData: TObject);
var Editor : TCodeEditorPanel;
begin
  Editor := FEditor.GetByData(EditorData);
  if Editor = nil then
  begin
    if Assigned(FEditor.OnRequireUnit) then
    begin
      FEditor.OnRequireUnit(FEditor, EditorData);
      Editor := FEditor.GetByData(EditorData);
    end;
  end;
  if Editor <> nil then
  begin
    FEditor.Selected := Editor;
    Editor.Editor.Editor.SelStart := ErrorPos - 1;
    Editor.Editor.ErrorLine       := ErrorLine;
    try
      Editor.Editor.Editor.SetFocus;
    except
    end;
  end;
end;

constructor TScriptEditorMngr.Create(AOwner: TComponent);
begin
  inherited;
  BevelOuter := bvNone;
end;

procedure TScriptEditorMngr.CreateEditor;
begin
  FEditor := TScriptEditor.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align  := alClient;

  FMessages := TEditorMessages.Create(Self);
  FMessages.Parent   := Self;
  FMessages.Align    := alBottom;
  FMessages.Height   := 140;
  FMessages.CreateMessages;
  FMessages.ViewType := mvMessages;
  FMessages.Messages.OnCompileError := CompileErrorEvent;
  FMessages.OnVisibleChanged := MessagesVisibleChanged;


  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Align  := alBottom;
  FSplitter.Height := 7;
  FSplitter.Beveled := False;
end;

destructor TScriptEditorMngr.Destroy;
begin
  FSplitter.Free;
  FEditor.Free;
  FMessages.Free;
  inherited;
end;

procedure TScriptEditorMngr.MessagesVisibleChanged(Sender: TObject);
begin
  if FMessages.Visible then
  begin
    FSplitter.Visible := True;
    FSplitter.Top     := 0;
  end else
    FSplitter.Visible := False;

end;

end.
