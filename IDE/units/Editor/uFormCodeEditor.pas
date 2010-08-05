unit uFormCodeEditor;

interface

uses
  Windows, Classes, Types, SysUtils, Controls, Dialogs, Graphics, StdCtrls, ExtCtrls,
  Menus, Forms,

  uFormCompileMessages,

  JvComponent, JvExControls, JvGradient, JvTabBar,

  SynEdit, SynCompletionProposal, SynEditRegexSearch, SynEditMiscClasses,
  SynEditSearch, SynHighlighterSE2, SynEditTypes,
  
  uSE2Errors, uSE2PEData, uSE2RunTime, uSE2DebugData, uSE2CodeCompletion,
  uSE2Compiler, uSE2RunType, uSE2UnitCacheMngr, uSE2Reader;


type
  TEditorGetCustomUnits = uSE2CodeCompletion.TSE2GetCustomUnits;

  TEditorSourceEvent = procedure(Sender: TObject; const Readers: TSE2ReaderList) of object;

  TCodeEditor = class(TObject)
  private
    FUnderlines    : TList;

    FEditor        : TSynEdit;
    FErrorLine     : integer;

    FCodeComplete  : TSynCompletionProposal;
    FParamComplete : TSynCompletionProposal;
    FItemInfo      : TSynCompletionProposal;
    FHighlighter   : TSynSE2Syn;

    FOnGetUnit     : TSE2GetFileReader;
    FOnGetUnitMngr : TSE2GetUnitMngr;
    FOnGetCustomUnits : TEditorGetCustomUnits;

    FOnCompletionDependency : TEditorSourceEvent;
  protected
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure EditorLineColor(Sender: TObject; Line: integer; var Special: boolean; var FG, BG: TColor);
    procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas; TransientType: TTransientType);

    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; x, y: integer);
    procedure EditorContextHelp(Sender: TObject; word: string);
    procedure CodeCompleteInsert(Sender: TObject; const DisplayText, InsertText: string);
    procedure ParamCompleteInsert(Sender: TObject; const DisplayText, InsertText: string);  
    procedure ItemInfoInsert(Sender: TObject; const DisplayTest, InsertTest: string);

    procedure CompleteGetUnitMngr(Sender: TObject; var UnitMngr: TSE2UnitCacheMngr);

    procedure CodeCompleteExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: String; var x, y: Integer;
      var CanExecute: Boolean);
    procedure ParamCompleteExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: String; var x, y: Integer;
      var CanExecute: Boolean);  
    procedure ItemInfoExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: String; var x, y: Integer;
      var CanExecute: Boolean);

    function  GetSource: string;
    procedure SetSource(const value: string);

    procedure SetErrorLine(value: integer);
    procedure ClearUnderlines;
    procedure AddUnderline(Pos: TPoint; Color: TColor);
    procedure DrawUnderLines(Canvas: TCanvas);
  public
    constructor Create(AParent: TWinControl); reintroduce;
    destructor Destroy; override;
    class procedure SetupColors(FHighlighter: TSynSE2Syn; FEditor: TSynEdit);
    procedure RemoveErrorCodes;

    property  CodeComplete   : TSynCompletionProposal  read FCodeComplete;
    property  ParamComplete  : TSynCompletionProposal  read FParamComplete;
    property  ItemInfo       : TSynCompletionProposal  read FItemInfo;
    property  Highlighter    : TSynSE2Syn              read FHighlighter;
    property  Editor         : TSynEdit                read FEditor;
    property  ErrorLine      : integer                 read FErrorLine       write SetErrorLine;
    property  Source         : string                  read GetSource        write SetSource;

    property  OnCompletionDependency : TEditorSourceEvent read FOnCompletionDependency write FOnCompletionDependency;
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
    FOnChanged     : TNotifyEvent;
    FOnDependency   : TEditorSourceEvent;
  protected
    function  GetCaption: string;
    procedure SetCaption(const value: string);

    function  GetModified: boolean;
    procedure SetModified(value: boolean);

    procedure CodeEditorDependency(Sender: TObject; const Readers: TSE2ReaderList);
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

    property  OnChanged  : TNotifyEvent   read FOnChanged    write FOnChanged;
    property  OnDependency : TEditorSourceEvent   read FOnDependency   write FOnDependency;
  end;

  TRequireSaveEvent = procedure(Sender: TObject; CodeTab: TCodeEditorPanel; var CanClose: boolean) of object;
  TRequireUnitEvent = procedure(Sender: TObject; Data: TObject) of object;
  TCodeEditorEvent  = procedure(Sender: TObject; const Editor: TCodeEditorPanel) of object;

  TScriptEditor = class(TPanel)
  private
    FCodeTabs       : TJvTabBar;  
    FOnGetUnit      : TSE2GetFileReader;
    FOnGetUnitMngr  : TSE2GetUnitMngr;
    FOnGetCustomUnits : TEditorGetCustomUnits;
    FOnRequireSave  : TRequireSaveEvent;
    FOnRequireUnit  : TRequireUnitEvent;
    FOnUnitChanged  : TCodeEditorEvent;
    FOnDependency   : TEditorSourceEvent;
  protected
    procedure CodeTabClosing(Sender: TObject; Item: TJvTabBarItem; var AllowClose: boolean);
    procedure CodeTabSelected(Sender: TObject; Item: TJvTabBarItem);
    procedure CodeTabClosed(Sender: TObject; Item: TJvTabBarItem);

    procedure CodeEditorDependency(Sender: TObject; const Readers: TSE2ReaderList);
    procedure CompleteGetUnitMngr(Sender: TObject; var UnitMngr: TSE2UnitCacheMngr);
    procedure EditorGetUnit(Sender: TObject; const Name: string; const Readers: TList);
    procedure EditorGetCustomUnits(Sender: TObject; const Target: TStrings);

    function  GetSelected: TCodeEditorPanel;
    procedure SetSelected(value: TCodeEditorPanel);

    function  GetItem(index: integer): TCodeEditorPanel;
    function  GetCount: integer;

    procedure CodeEditorChanged(Sender: TObject);
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
    property  OnUnitChanged        : TCodeEditorEvent     read FOnUnitChanged  write FOnUnitChanged;
    property  OnDependency         : TEditorSourceEvent   read FOnDependency   write FOnDependency;
  end;

  TScriptEditorMngr = class(TPanel)
  private
    FEditor   : TScriptEditor;
    FSplitter : TSplitter;
    FMessages : TEditorMessages;
  protected
    procedure MessagesVisibleChanged(Sender: TObject);
    procedure CompileErrorEvent(Sender: TObject; ErrorPos, ErrorLine: integer; EditorData: TObject; FirstError: boolean; ErrorType: TErrorType);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateEditor;

    property  Editor   : TScriptEditor   read FEditor;
    property  Messages : TEditorMessages read FMessages;
  end;

implementation

type
  PUnderlineData = ^TUnderlineData;
  TUnderlineData = record
    x, y  : integer;
    Color : TColor;
  end;

{ TCodeEditor }

procedure TCodeEditor.CodeCompleteExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: String; var x, y: Integer;
  var CanExecute: Boolean);
var Readers: TSE2ReaderList;
begin
  FCodeComplete.ClearList;

  with TSE2SynCodeCompletion.Create(CodeCompleteInsert) do
  try
    OnGetCustomUnits   := FOnGetCustomUnits;
    OnGetUnitMngr      := CompleteGetUnitMngr;
    Compiler.OnGetFile := FOnGetUnit;

    Readers := TSE2ReaderList.Create;
    try
      if Assigned(FOnCompletionDependency) then
         FOnCompletionDependency(Self, Readers);

      Readers.Add(TSE2StringReader.Create(Source));
      CanExecute         := GetCodeCompletion(Readers, FEditor.SelStart);
    finally
      Readers.Free;
    end;
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

procedure TCodeEditor.ItemInfoExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: String; var x, y: Integer;
  var CanExecute: Boolean);
var Readers: TSE2ReaderList;
begin
  FItemInfo.ClearList;

  with TSE2SynInlineDocumentation.Create(ItemInfoInsert) do
  try
    OnGetUnitMngr      := CompleteGetUnitMngr;
    Compiler.OnGetFile := FOnGetUnit;

    Readers := TSE2ReaderList.Create;
    try
      if Assigned(FOnCompletionDependency) then
         FOnCompletionDependency(Self, Readers);

      Readers.Add(TSE2StringReader.Create(Source));
      CanExecute         := GetInlineDocumentation(Readers, FEditor.SelStart, FEditor.WordAtCursor);
    finally
      Readers.Free;
    end;
  finally
    Free;
  end;
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
  FCodeComplete.EndOfTokenChr  := '()[]. +-*/:;,';
  FCodeComplete.TriggerChars   := '.';
  FCodeComplete.Title          := 'ScriptEngine II';

  FParamComplete := TSynCompletionProposal.Create(AParent);
  FParamComplete.DefaultType   := ctParams;
  FParamComplete.Editor        := FEditor;
  FParamComplete.EndOfTokenChr := '()[].:;,';
  FParamComplete.Options       := [scoLimitToMatchedText,scoUsePrettyText,scoUseBuiltInTimer];
  FParamComplete.TimerInterval := 50;
  FParamComplete.ShortCut      := ShortCut(VK_SPACE, [ssCtrl, ssShift]);
  FParamComplete.TriggerChars  := '([';
  FParamComplete.OnExecute     := ParamCompleteExecute;

  FItemInfo := TSynCompletionProposal.Create(AParent);
  FItemInfo.DefaultType        := ctHint;
  FItemInfo.Editor             := Editor;
  FItemInfo.EndOfTokenChr      := '';
  FItemInfo.Options            := [scoLimitToMatchedText,scoUsePrettyText];
  FItemInfo.TimerInterval      := 50;
  FItemInfo.ShortCut           := ShortCut(VK_SPACE, [ssCtrl, ssAlt]);
  FItemInfo.TriggerChars       := '';
  FItemInfo.OnExecute          := ItemInfoExecute;
  FItemInfo.ClBackground       := $00A8F4FF;
  FItemInfo.Font.Name          := 'Tahoma';

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
  FEditor.OnContextHelp       := EditorContextHelp;
  FEditor.OnMouseMove         := EditorMouseMove;
  FEditor.OnPaintTransient    := EditorPaintTransient;
  FErrorLine                  := -1;

  SetupColors(FHighlighter, FEditor);
  FUnderlines := TList.Create;
end;

destructor TCodeEditor.Destroy;
begin
  ClearUnderlines;
  FUnderlines.Free;
  FHighlighter.Free;
  FCodeComplete.Free;
  FParamComplete.Free;
  FItemInfo.Free;
  FEditor.Free;
  inherited;
end;      

procedure TCodeEditor.EditorMouseMove(Sender: TObject; Shift: TShiftState;
  x, y: integer);
begin
  FEditor.Hint := FEditor.WordAtMouse;
  Application.Hint := FEditor.WordAtMouse;
end;

procedure TCodeEditor.EditorContextHelp(Sender: TObject; word: string);
begin
  Application.HintPause := 0;
  Application.HintShortPause := 10;
  Application.HintHidePause := 10000;
  if word <> '' then
  begin
     FEditor.Hint := word + ' hello';
     FEditor.ShowHint := True;
  end;
    // word := 'Hello';
  //
end;

procedure TCodeEditor.EditorLineColor(Sender: TObject; Line: integer;
  var Special: boolean; var FG, BG: TColor);
begin
  if Line = FErrorLine then
  begin
    Special := False;    {
    BG      := clMaroon;
    FG      := clWhite;   }
  end else
    Special := False;
end;

procedure TCodeEditor.EditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  //SetErrorLine(-1);
end;

function TCodeEditor.GetSource: string;
begin
  result := FEditor.Text;
end;

procedure TCodeEditor.ParamCompleteExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: String; var x, y: Integer;
  var CanExecute: Boolean);
var index: integer;
    readers: TSE2ReaderList;
begin
  if (FParamComplete.ItemList.Count = 0) or (not TWinControl(FParamComplete.Form).Visible) or
     (CurrentInput = '') or (Pos('(', CurrentInput) > 0) or (Pos(')', CurrentInput) > 0) then
  begin
    FParamComplete.ClearList;

    with TSE2SynParamCompletion.Create(ParamCompleteInsert) do
    try
      OnGetUnitMngr      := CompleteGetUnitMngr;
      Compiler.OnGetFile := FOnGetUnit;

      readers := TSE2ReaderList.Create;
      try
        if Assigned(FOnCompletionDependency) then
           FOnCompletionDependency(Self, readers);

        readers.Add(TSE2StringReader.Create(Source));

        CanExecute := GetParamCompletion(readers, FEditor.SelStart, index);
      finally
        readers.Free;
      end;
      if TWinControl(FParamComplete.Form).Visible then
         CanExecute := True;
      FParamComplete.Form.CurrentIndex := index;
    finally
      Free;
    end;
  end;
end;

procedure TCodeEditor.ParamCompleteInsert(Sender: TObject;
  const DisplayText, InsertText: string);
begin
  FParamComplete.ItemList.Add(DisplayText);
end;

procedure TCodeEditor.ItemInfoInsert(Sender: TObject;
  const DisplayTest, InsertTest: string);
begin
  FItemInfo.ItemList.Add(DisplayTest);
end;

procedure TCodeEditor.SetErrorLine(value: integer);
begin
  if value <> FErrorLine then
  begin
    if value < 0 then
       ClearUnderlines;
       
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

procedure TCodeEditor.AddUnderline(Pos: TPoint; Color: TColor);
var p: PUnderlineData;
begin
  New(p);
  p^.x := Pos.x;
  p^.y := Pos.y;
  p^.Color := Color;
  FUnderlines.Add(p);
  FEditor.Refresh;
end;

procedure TCodeEditor.ClearUnderlines;
var i: integer;
begin
  for i:=FUnderlines.Count-1 downto 0 do
    Dispose(PUnderlineData(FUnderlines[i]));
  FUnderlines.Clear;
end;

procedure TCodeEditor.DrawUnderLines(Canvas: TCanvas);

  procedure DrawErrorLine(Line, StartPos: integer; Color: TColor);
  var fromPos, ToPos, Y: integer;
      t: TDisplayCoord;
      p: TPoint;
      buffer : TBufferCoord;
  begin
    if (Line < 1) or (Line > FEditor.Lines.Count) then
       exit;

    buffer := FEditor.PrevWordPosEx(BufferCoord(StartPos, Line));
    if buffer.Line = Line then
       StartPos := buffer.Char
    else
    if StartPos > 1 then
       StartPos := StartPos - 1;


    t.Column := StartPos;
    t.Row    := Line;
    p := FEditor.RowColumnToPixels(t);
    fromPos  := p.X;

    t.Column := length(TrimRight(FEditor.Lines[Line - 1])) + 1;
    p := FEditor.RowColumnToPixels(t);
    ToPos    := p.X;

    Y := p.Y + FEditor.LineHeight - 3;

    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := Color;
    Canvas.MoveTo(FromPos, y);
    Canvas.LineTo(ToPos, y);

    Canvas.Pen.Style := psDot;
    Canvas.Pen.Width := 1;

    Canvas.MoveTo(FromPos, y - 1);
    Canvas.LineTo(ToPos, y - 1);

    Canvas.MoveTo(FromPos - 3, y + 1);
    Canvas.LineTo(ToPos - 3, y + 1);
    
    Canvas.Pen.Style := psSolid;
  end;

var i: integer;
    p: PUnderlineData;
begin
  for i:=0 to FUnderlines.Count-1 do
  begin
    p := FUnderlines[i];
    DrawErrorLine(p^.Y, p^.X, p^.Color);
  end;
end;

procedure TCodeEditor.EditorPaintTransient(Sender: TObject;
  Canvas: TCanvas; TransientType: TTransientType);
begin
  if TransientType = ttAfter then
     DrawUnderLines(Canvas);
end;

procedure TCodeEditor.RemoveErrorCodes;
begin
  if FUnderlines.Count > 0 then
  begin
    ClearUnderlines;
    FEditor.Refresh;
  end;
end;

{ TCodeEditorPanel }

procedure TCodeEditorPanel.CodeEditorDependency(Sender: TObject;
  const Readers: TSE2ReaderList);
begin
  if Assigned(FOnDependency) then
     FOnDependency(Self, Readers);
end;

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
  FEditor.OnCompletionDependency := CodeEditorDependency;
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
  FEditor.RemoveErrorCodes;

  if Assigned(FOnChanged) then
     FOnChanged(Self);
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
  result.OnChanged         := CodeEditorChanged;
  result.OnDependency := CodeEditorDependency;
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

procedure TScriptEditor.CodeEditorChanged(Sender: TObject);
begin
  if Assigned(FOnUnitChanged) then
     FOnUnitChanged(Self, TCodeEditorPanel(Sender));
end;

procedure TScriptEditor.CodeEditorDependency(Sender: TObject;
  const Readers: TSE2ReaderList);
begin
  if Assigned(FOnDependency) then
     FOnDependency(Sender, Readers);
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
  ErrorLine: integer; EditorData: TObject; FirstError: boolean; ErrorType: TErrorType);
var Editor : TCodeEditorPanel;
    Coord  : TBufferCoord;
    Color  : TColor;
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
    if FirstError and (ErrorType in [etError, etNone]) then
    begin
      FEditor.Selected := Editor;
      Editor.Editor.Editor.SelStart := ErrorPos - 1;
      Editor.Editor.ErrorLine       := ErrorLine;
      try
        Editor.Editor.Editor.SetFocus;
      except
      end;
    end;
    case ErrorType of
    etHint : Color := clGreen;
    etWarning : Color := clNavy;
    etError : Color := clRed;
    else exit;
    end;

    Coord := Editor.Editor.Editor.CharIndexToRowCol(ErrorPos);
    Editor.Editor.AddUnderline(Point(Coord.Char, Coord.Line), Color);
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
